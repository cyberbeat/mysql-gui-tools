//
//  MySQLStartupPref.m
//  MySQLStartup
//
//  Created by Alfredo Kojima on 1/4/05.
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//

#define MYSQL_PREFPANE
#import "MySQLStartupPref.h"
#include "../mahelper.h"
#include "../mahelper_priv.h"
#undef NSLocalizedString
#define NSLocalizedString(a,b) a

#include <sys/stat.h>
#include <pwd.h>

@implementation MySQLStartupPref

- (void) mainViewDidLoad
{
  authRef= 0;
  curState= Stopped;
  firstTime= YES;
	originalWarningText= [[warningText stringValue] retain];
	
  [self isRunning];
  [self isAutoStarting];
  [NSTimer scheduledTimerWithTimeInterval: 2.0
                                   target:self
                                 selector:@selector(isRunning) 
                                 userInfo:nil 
                                  repeats:YES];

  [NSTimer scheduledTimerWithTimeInterval: 2.0 
                                   target:self
                                 selector:@selector(isAutoStarting) 
                                 userInfo:nil 
                                  repeats:YES];
}

- (void)dealloc
{
	[originalWarningText release];
  if (authRef)
    AuthorizationFree(authRef,kAuthorizationFlagDefaults);
  [super dealloc];
}


- (BOOL)isAutoStarting
{  
  BOOL autoStart;
  
  if (autoStart= mautoStartState())
    [autoStartCheck setState:NSOnState];
  else
    [autoStartCheck setState:NSOffState];
  
  return autoStart;
}


static char *get_mysql_datadir()
{
	// try to guess where is the mysql data dir by 1st looking at /etc/my.cnf and then
	// just assume the default
	
	FILE *f= fopen(MYSQL_CONFIG_FILE, "r");
	if (f)
	{
		char line[1024];
		
		while ((fgets(line, sizeof(line), f)))
		{
			if (strncmp(line, "datadir ", strlen("datadir "))==0 || strncmp(line, "datadir=", strlen("datadir="))==0)
			{
				char *ptr= strchr(line, '=');
				if (ptr)
				{
					++ptr;
					while (isspace(*ptr)) ++ptr;
					while (isspace(*(ptr+strlen(ptr)-1))) ptr[strlen(ptr)-1]= 0;
					return strdup(ptr);
				}
			}
		}
		fclose(f);
	}
	
	return strdup(MYSQL_DEFAULT_DATADIR);
}


- (BOOL)checkDataDirOwner
{
	char *datadir= get_mysql_datadir();
	BOOL badowner= NO;
	
	NSLog(@"stat %s", datadir);
	if (datadir)
	{
		struct stat stbuf;
		
		if (stat(datadir, &stbuf) == 0)
		{
			// check uid is mysql or _mysql
			struct passwd *pwent= getpwuid(stbuf.st_uid);
			if (pwent)
			{
				if (strcmp(pwent->pw_name, "mysql")!=0 && strcmp(pwent->pw_name, "_mysql")!=0)
				{
					NSLog(@"directory %s is owned by %s instead of mysql, which could be a problem. Please fix the owner of the mysql datadir", datadir, pwent->pw_name);
					badowner= YES;
				}
			}
		}
		else
			NSLog(@"could not stat %s (%s)", datadir, strerror(errno));
	}
	
	if (badowner)
	{
		[warningText setStringValue:
		 [NSString stringWithFormat:@"Warning: The %s directory is not owned by the 'mysql' or '_mysql' user.",datadir]];
		[warningText setHidden:NO];
	}
	
	if (datadir)
		free(datadir);
	
	return !badowner;
}


- (ServerState)isRunning
{
  ServerState state= Stopped;
  NSTask *task = [[NSTask alloc] init];  
	
#if 0 // apparently, we can't run suid programs in Tiger anymore...
  NSData *inData = nil;
  NSArray *array = nil;
  NSString *string = nil;
  int i, count;
  
  [task setLaunchPath:@"/bin/ps"];
  [task setArguments:[NSArray arrayWithObjects:@"ax",@"-ocommand",nil]];
#else
  [task setLaunchPath:[NSString stringWithUTF8String:MYSQLADMIN_COMMAND]];
  [task setArguments:[NSArray arrayWithObject:@"ping"]];
#endif
  [task setStandardOutput:[NSPipe pipe]];
  [task setStandardError:[task standardOutput]];
	@try {
		[task launch];
		[task waitUntilExit];
	}
	@catch (NSException *exc) {
		state= NotFound;
	}

	if (state != NotFound)
	{
#if 0
		inData = [[[task standardOutput] fileHandleForReading] readDataToEndOfFile];
		string = [[[NSString alloc] initWithData:inData encoding:NSASCIIStringEncoding] autorelease];
		array = [string componentsSeparatedByString:@"\n"];
		[task release];
		count= [array count];
		for (i= 0; i < count; i++)
		{
			NSString *line= [array objectAtIndex:i];
			NSRange range;
			
			range= [line rangeOfString:@"mysqld"];
			if (range.location != NSNotFound)
			{
				state= YES;
				break;
			}
		}
#else
		state= [task terminationStatus] == 0 ? Running : Stopped;
#endif
	}
		
  if (state != curState || firstTime)
  {
    NSBundle *bundle= [NSBundle bundleForClass:[self class]];
    firstTime= NO;
    curState= state;
    if (state == Running)
    {
      [button setTitle:NSLocalizedString(@"Stop MySQL Server", nil)];
      [descrText setStringValue:@"The MySQL Database Server is started and ready for client connections.\n"
        "To shut the Server down, use the \"Stop MySQL Server\" button."];
			[warningText setStringValue: originalWarningText];
      [warningText setHidden:NO];
      [[stateText cell] setTextColor: [NSColor greenColor]];
      [stateText setStringValue:@"running"];
      [stateImage setImage:
        [[[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"instance_started"
                                                                 ofType:@"png"]] autorelease]];
    }
		else if (state == Stopped) 
		{
      [button setTitle:NSLocalizedString(@"Start MySQL Server", nil)];
      [descrText setStringValue:@"The MySQL Database Server is currently stopped.\n"
        "To start it, use the \"Start MySQL Server\" button."];
			if ([self checkDataDirOwner])
				[warningText setHidden:YES];
      [[stateText cell] setTextColor: [NSColor redColor]];
      [stateText setStringValue:@"stopped"];
      [stateImage setImage:
        [[[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"instance_stopped"
                                                                  ofType:@"png"]] autorelease]];
    }
		else
		{
			[button setTitle:NSLocalizedString(@"Start MySQL Server", nil)];
			[button setEnabled:NO];
			[descrText setStringValue:
			 [NSString stringWithFormat:@"The MySQL Database Server installation was not found.\n"
					"Please make sure the %s file is present.", MYSQLADMIN_COMMAND]];
      [warningText setHidden:YES];
      [[stateText cell] setTextColor: [NSColor redColor]];
      [stateText setStringValue:@"not running"];
      [stateImage setImage:
			 [[[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"instance_stopped"
																																 ofType:@"png"]] autorelease]];
		}
		[warningText setNeedsDisplay];
    [stateImage setNeedsDisplay];
  }
  
  return state;
}



- (int)performCommand:(MAHelperCommand)command
{
  //AuthorizationRef authRef= NULL;
  NSString *helperPath;
  AuthorizationItem right = { "com.mysql.administrator.helper", 0, NULL, 0 };
  AuthorizationRights rightSet = { 1, &right };
  OSStatus status;

  helperPath= [[NSBundle bundleForClass:[self class]] pathForResource:@"mahelper"
                                                               ofType:@""];
  if (!authRef)
  {
    if (AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, 
                            &authRef)!=errAuthorizationSuccess)
    {
      NSLog(@"Could not create authorization reference object.");
      return MAHelperCommandInternalError;
    }
    
    status = AuthorizationCopyRights(authRef, &rightSet, kAuthorizationEmptyEnvironment, 
                                     kAuthorizationFlagDefaults | kAuthorizationFlagPreAuthorize
                                     | kAuthorizationFlagInteractionAllowed | kAuthorizationFlagExtendRights,
                                     NULL);
  } else
    status= errAuthorizationSuccess;
  if (status == errAuthorizationSuccess)
  {
    int hstatus;
    
		NSLog(@"about to perform command");
    hstatus = mhelperPerformCommand(authRef, [helperPath UTF8String], command);
		NSLog(@"status= %i", hstatus);
    return hstatus;
  }
  else
  {
    AuthorizationFree(authRef, kAuthorizationFlagDestroyRights);
    authRef= 0;
    if (status != errAuthorizationCanceled)
    {
      // pre-auth failed
      NSLog(@"Pre-auth failed");
      return MAHelperCommandAuthFailed;
    }
    else
      return MAHelperCommandCancelled;
  }
}


static NSString *errorMessage(int rc)
{
  switch (rc)
  {
    case MAHelperCommandInternalError:
      return @"Internal error.";
    case MAHelperCommandSuccess:
      return @"Success";
    case MAHelperCommandExecFailed:
      return @"Could not execute command.";
    case MAHelperCommandChildError:
      return @"Error in command.";
    case MAHelperCommandAuthFailed:
      return @"Authentication error.";
    case MAHelperCommandOperationFailed:
      return @"Operation failed.";
    case MAHelperCommandCancelled:
      return @"Cancelled.";
    case MAHelperCommandHelperNotFound:
      return @"Helper binary not found. Please reinstall the preference pane.";
    default:
      return [NSString stringWithFormat:@"%i", rc];//@"";
  }
}


- (BOOL)shutdownServer
{
  MAHelperCommand command;
  
  command.authorizedCommandId= MAHelperShutdownMySQL;
  
  return [self performCommand:command]==MAHelperCommandSuccess;
}

- (IBAction)toggleServer:(id)sender
{
  MAHelperCommand command;
  BOOL start= ![self isRunning];
  int rc;

  command.authorizedCommandId= start ? MAHelperStartMySQL : MAHelperStopMySQL;
  
  if ((rc= [self performCommand:command])!=MAHelperCommandSuccess)
  {
    if (rc != MAHelperCommandCancelled)
    {
      if (start)
        NSRunAlertPanel(NSLocalizedString(@"Error",nil), 
                        [NSString stringWithFormat:NSLocalizedString(@"Could not startup MySQL Server.\nReason: %@",nil), errorMessage(rc)], 
                        nil, nil, nil);
      else
	  {
		if (![self shutdownServer])
		{
		  NSRunAlertPanel(NSLocalizedString(@"Error",nil), 
						  [NSString stringWithFormat:NSLocalizedString(@"Could not stop MySQL Server.\nReason: %@",nil), errorMessage(rc)], 
						  nil, nil, nil);
		}
	  }
    }
  }
}


- (IBAction)changeAutoStart:(id)sender
{
  MAHelperCommand command;
  BOOL flag= ![self isAutoStarting];
  int rc;
    
  command.authorizedCommandId= MAHelperToggleAutoStart;
  command.enable= flag;
  if ((rc = [self performCommand:command]) != MAHelperCommandSuccess)
  {
    if (rc != MAHelperCommandCancelled)
      NSRunAlertPanel(NSLocalizedString(@"Error",nil),
                      [NSString stringWithFormat:NSLocalizedString(@"Could not change automatic startup configuration.\nReason: %@",nil), errorMessage(rc)], 
                      nil, nil, nil);
  }
  [self isAutoStarting];
}


@end
