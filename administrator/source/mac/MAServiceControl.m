#import "MAServiceControl.h"

#include <sys/wait.h>

#import "MAdministrator.h"
#import <MySQLToolsCommon/MConnectionInfo.h>
#include "mahelper.h"

@interface MAServiceControl(Private)
- (void)updateStatus;
- (BOOL)performCommand:(MAHelperCommand)command
             withRight:(const char*)rightName;
- (void)startStopServer:(id)flag;
- (void)enableServerAutoStart:(BOOL)flag;
@end

@implementation MAServiceControl(Private)
- (void)updateStatus
{
  NSImage *image;
  NSString *status;
  NSString *blabel;
  BOOL changeable= YES;
  BOOL autoStart= NO;
  
  if (![_owner isLocal])
  {
    image= [NSImage imageNamed: @"service_status_running.png"];
    status= @"Remote Server is Running";
    blabel= @"Stop Server";
    changeable= NO;
  }
  else
  {
    // check if autostart is enabled
    autoStart= mautoStartState();
    
    [startOnBootCheck setState:autoStart?NSOnState:NSOffState];
    
    switch ([_owner serverStatus])
    {
      case 0:
        image= [NSImage imageNamed: @"service_status_stopped.png"];
        status= @"Server is Stopped";
        blabel= @"Start Server";
        break;
      case 1:
        image= [NSImage imageNamed: @"service_status_running.png"];
        status= @"Server is Running";
        blabel= @"Stop Server";
        break;
      default:
        image= [NSImage imageNamed: @"service_status_unknown.png"];
        status= @"Cannot determine server status";
        blabel= @"Stop Server";
        changeable= NO;
        break;
    }
  }
  [statusImage setImage: image];
  [statusLabel setStringValue: status];
  [toggleButton setTitle:blabel];
  [toggleButton setEnabled:changeable];
  [startOnBootCheck setEnabled:changeable];
}

static void helperout_callback(const char *text, void *data)
{
  [[[((MAServiceControl*)data)->messageText textStorage] mutableString] appendFormat:@"%s", text];
}

- (BOOL)performCommand:(MAHelperCommand)command
             withRight:(const char*)rightName
{
  NSString *helperPath;
  AuthorizationItem right = { rightName, 0, NULL, 0 };
  AuthorizationRights rightSet = { 1, &right };
  OSStatus status;
  
  helperPath= [[NSBundle bundleForClass:[self class]] pathForResource:@"mahelper"
                                                               ofType:@""];
  if (!_authRef)
  {
    if (AuthorizationCreate(NULL, kAuthorizationEmptyEnvironment, kAuthorizationFlagDefaults, 
                            &_authRef)!=errAuthorizationSuccess)
    {
      [[[messageText textStorage] mutableString] appendString:@"Error setting-up authorization for the helper.\n"];
      NSLog(@"Could not create authorization reference object.");
      return NO;
    }
  }
  
  status = AuthorizationCopyRights(_authRef, &rightSet, kAuthorizationEmptyEnvironment, 
                                   kAuthorizationFlagDefaults | kAuthorizationFlagPreAuthorize
                                   | kAuthorizationFlagInteractionAllowed | kAuthorizationFlagExtendRights,
                                   NULL);
  
  if (status == errAuthorizationSuccess)
  {
    int hstatus;
    
    hstatus = mhelperPerformCommand2(_authRef, [helperPath fileSystemRepresentation], command,
                                     helperout_callback, self);        
  }
  else
  {
    AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
    _authRef= 0;
    if (status != errAuthorizationCanceled)
    {
      // pre-auth failed
      [[[messageText textStorage] mutableString] appendString:@"Error during authorization step for the helper.\n"];
    }
    else
      return YES; // YES means it was cancelled
  }
  return NO;
}


- (void)startStopServer:(id)startNum
{
  MAHelperCommand command;
  BOOL start= [startNum boolValue];
  BOOL cancelled;
  
  command.authorizedCommandId= start ? MAHelperStartMySQL : MAHelperStopMySQL;
  
  cancelled= [self performCommand:command
                        withRight:"com.mysql.administrator.server"];
  
  if (cancelled)
  {
    [[[messageText textStorage] mutableString] appendString:@"Cancelled.\n"];
    [_owner serverStatus];
    return;
  }
  
  if (start)
  {
    if ([_owner serverStatus]==0)
      [[[messageText textStorage] mutableString] appendString:@"Failed to start server.\n"];
    else
    {
      [[[messageText textStorage] mutableString] appendString:@"Started server.\n"];
      
      [[[messageText textStorage] mutableString] appendString:@"Reconnecting...\n"];
      
      if ([[_owner serverInfo] reconnect:[_owner mysql]])
        [[[messageText textStorage] mutableString] appendString:@"Reconnected to server.\n"];
      else
        [[[messageText textStorage] mutableString] appendString:@"Reconnection failed!\n"];
    }
  }
  else
  {
    if ([_owner serverStatus]!=0)
    {
      [[[messageText textStorage] mutableString] appendString:@"Failed to stop server, trying shutdown...\n"];
      
#if ((MYSQL_VERSION_ID >= 40103) && (MYSQL_VERSION_ID < 50000)) || (MYSQL_VERSION_ID >= 50001)
      mysql_shutdown([_owner mysql], SHUTDOWN_DEFAULT);
#else
      mysql_shutdown([_owner mysql]);
#endif
      // sleep a bit waiting the server to finish shutdown
      sleep(2);
      
      if ([_owner serverStatus]!=0)
        [[[messageText textStorage] mutableString] appendString:@"Failed to stop server.\n"];
      else
        [[[messageText textStorage] mutableString] appendString:@"Stopped server.\n"];
    }
    else
      [[[messageText textStorage] mutableString] appendString:@"Stopped server.\n"];
  }
  [self updateStatus];
}


- (void)enableServerAutoStart:(BOOL)flag
{
  MAHelperCommand command;
  
  command.authorizedCommandId= MAHelperToggleAutoStart;
  command.enable= flag;
  if (![self performCommand:command
                  withRight:"com.mysql.administrator.chconfig"])
  {
    NSRunAlertPanel(@"Error", @"Could not change automatic startup configuration.", 
                    nil, nil, nil);
  }
}

@end

@implementation MAServiceControl

- (IBAction)toggleServer:(id)sender
{
  BOOL start= [_owner serverStatus]==0;
  
  if (start)
    [[[messageText textStorage] mutableString] appendString:@"Starting server...\n"];
  else
    [[[messageText textStorage] mutableString] appendString:@"Stopping server...\n"];
  
  [toggleButton setEnabled:NO];
  [NSApplication detachDrawingThread:@selector(startStopServer:)
                            toTarget:self
                          withObject:[NSNumber numberWithBool: start]];
}

- (IBAction)toggleServerStartup:(id)sender
{
  [self enableServerAutoStart:[sender state]==NSOnState];
  [self updateStatus];
}


- (void)didShow
{
  [self updateStatus];
}

- (void)awakeFromNib
{
  [self updateStatus];
  /*
   if ([self getStartupItem]==NULL)
   {
     [messageText setString:@"The MySQL Startup Item was not found in /Librart/StartupItems\n"
       "You must select the \"Install Startup Item\" option during installation to enable "
       "server control from this tool."];
     [toggleButton setEnabled:NO];
     [startOnBootCheck setEnabled:NO];
   }
   */
}

+ (NSImage*)icon
{
  return [NSImage imageNamed: @"OSX-Icons_35.png"];
}

+ (NSString*)label
{
  return @"Service";
}

+ (NSString*)toolTip
{
  return @"Control MySQL Server Startup and Service.";
}

+ (BOOL)needsConnection
{
  return NO;
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"ServiceControl" panelOwner: owner];
  
  _defaultFrame= [[self topView] frame];
  
  return self;
}

- (void)dealloc
{
  if (_authRef)
    AuthorizationFree(_authRef, kAuthorizationFlagDestroyRights);
  [super dealloc];
}

@end
