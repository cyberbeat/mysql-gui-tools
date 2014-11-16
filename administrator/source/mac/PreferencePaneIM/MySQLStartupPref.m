//
//  MySQLStartupPref.m
//  MySQLStartup
//
//  Created by Alfredo Kojima on 1/4/05.
//  Copyright (c) 2005, 2006 MySQL AB. All rights reserved.
//


/*
 Discussion about authorization in PrefPane:
 
com.mysql.prefPane.server.startStop (needs root)
  - starting and stopping MySQL (or IM)
 
com.mysql.prefPane.server.changeSettings (needs root)
  - allows one to toggle auto-start setting in /etc/hostconfig
  - change whether to use IM in mysql.server script
  - change /etc/mysqlmanager.passwd
   
com.mysql.prefPane.server.configure (dont need root)
 - file operations on /etc/my.cnf

all others require admin rights
 
 */

#define MYSQL_PREFPANE
#import "MySQLStartupPref.h"
#include "myx_instanceconf_public_interface.h"
#include "../mahelper_priv.h"
#include <mysqld_error.h>
#include <errmsg.h>
#include <Security/Security.h>

#undef NSLocalizedString
#define NSLocalizedString(a,b) a

@implementation MySQLStartupPref

static NSString *errorMessage(int rc);

- (void)dealloc
{
  if (_mysqlPath)
    free(_mysqlPath);
  [_instanceList release];
  [super dealloc];
}


- (void)checkConnection
{
  int er;
  if (_imConnection)
  {
    er= mysql_errno(_imConnection);
    if (er == CR_SERVER_GONE_ERROR || er == CR_SERVER_LOST)
    {
      NSRunAlertPanel(@"Error", @"Connection to MySQL server lost.", @"OK", nil, nil);
      mysql_close(_imConnection);
      _imConnection = 0;
      
      [imConnectButton setHidden:NO];
    }
  }
}

- (void)mainViewDidLoad
{
  NSBundle *bundle= [NSBundle bundleForClass:[self class]];
  AuthorizationItem rightItems[2]= {
  {"com.mysql.prefPane.server.changeSettings", 0, NULL, 0},
  {"com.mysql.prefPane.server.startStop", 0, NULL, 0},
  };
  AuthorizationRights rights= {2, rightItems};

  [authView setAuthorizationRights:&rights];
  [authView setAutoupdate:YES];
  [authView setNeedsDisplay:YES];
  [authView updateStatus:nil];
  
  _runningIcon= 
    [[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"instance_started"
                                                              ofType:@"png"]];
  _stoppedIcon= 
    [[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"instance_stopped"
                                                             ofType:@"png"]];
  _runningMiniIcon= 
    [[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"service_status_running"
                                                             ofType:@"png"]];
  [_runningMiniIcon setScalesWhenResized:YES];
  [_runningMiniIcon setSize:NSMakeSize(32,32)];
  
  _stoppedMiniIcon= 
    [[NSImage alloc] initWithContentsOfFile:[bundle pathForResource:@"service_status_stopped"
                                                             ofType:@"png"]];
  [_stoppedMiniIcon setScalesWhenResized:YES];
  [_stoppedMiniIcon setSize:NSMakeSize(32,32)];

  _instanceList= [[NSMutableArray alloc] init];

  // if mysql not found in standard location, cant do anything
  // just display a message
  _mysqlPath= mim_locate_mysql_installation();

  if (!_mysqlPath)
  {
    [tabview setTabViewType:NSNoTabsNoBorder];

    [tabview selectTabViewItemWithIdentifier:@"badInstall"];
    NSLog(@"MySQL install not found");
    _uiMode= UI_BadInstallation;
  }
  else
  {
    NSLog(@"MySQL found at %s", _mysqlPath);

    [tabview removeTabViewItem:[tabview tabViewItemAtIndex:[tabview indexOfTabViewItemWithIdentifier:@"badInstall"]]];

    _instanceTab= [[tabview tabViewItemAtIndex:[tabview indexOfTabViewItemWithIdentifier:@"instance"]] retain];
    _instancesTab= [[tabview tabViewItemAtIndex:[tabview indexOfTabViewItemWithIdentifier:@"instances"]] retain];
    _imTab= [[tabview tabViewItemAtIndex:[tabview indexOfTabViewItemWithIdentifier:@"im"]] retain];    
    _imDisabledTab= [[tabview tabViewItemAtIndex:[tabview indexOfTabViewItemWithIdentifier:@"imDisabled"]] retain];
    
    [tabview removeTabViewItem:_instanceTab];
    [tabview removeTabViewItem:_instancesTab];
    [tabview removeTabViewItem:_imDisabledTab];
  }
  
  [self tableViewSelectionDidChange:nil];
}


- (void)didSelect
{
  [self checkStatus];
  
  _timer= [NSTimer scheduledTimerWithTimeInterval: 5.0 
                                           target:self
                                         selector:@selector(checkStatus) 
                                         userInfo:nil 
                                          repeats:YES];
}


- (void)didUnselect
{
  [_timer invalidate];
}


- (void)hideTab:(id)tab
{
  if ([tabview indexOfTabViewItem:tab] != NSNotFound)
    [tabview removeTabViewItem:tab];
}

- (void)showTab:(id)tab atIndex:(int)index
{
  if ([tabview indexOfTabViewItem:tab] == NSNotFound)
    [tabview insertTabViewItem:tab atIndex:index];
}


static BOOL password_retrieve(char **username, char **password)
{
  UInt32 password_length= 0;
  void *password_data= NULL;
  const char *name= "MySQLIM@127.0.0.1";

  SecKeychainFindGenericPassword(NULL,
                                 strlen(name),
                                 name,
                                 strlen("PreferencePane"),
                                 "PreferencePane",
                                 &password_length,
                                 &password_data,
                                 NULL);
  if (password_data)
  {
    char *ptr;
    *username= malloc(password_length+1);
    *password= malloc(password_length+1); 
    strncpy(*username, password_data, password_length);
    (*username)[password_length]= 0;
    SecKeychainItemFreeContent(NULL, password_data);

    ptr= strchr(*username, '\n');
    if (ptr)
    {
      strcpy(*password, ptr+1);
      *ptr= 0;
      return 1;
    }
    else
    {
      free(*username);
      free(*password);
      *username= NULL;
      *password= NULL;
      return 0;
    }
  }
  return 0;
}


static char *password_store(const char *username, const char *password)
{
  const char *name= "MySQLIM@127.0.0.1";
  char buffer[strlen(username)+strlen(password)+2];
  SecKeychainItemRef ref;
  
  NSLog(@"STORE TO %s", name);
  
  sprintf(buffer, "%s\n%s", username, password);
  
  // store in the keychain
  if (SecKeychainFindGenericPassword(NULL,
                                     strlen(name),
                                     name,
                                     strlen("PreferencePane"),
                                     "PreferencePane",
                                     NULL,
                                     NULL,
                                     &ref) == noErr)
  {
    SecKeychainItemDelete(ref);
    CFRelease(ref);
  }
  
  SecKeychainAddGenericPassword(NULL,
                                strlen(name),
                                name, 
                                strlen("PreferencePane"),
                                "PreferencePane",
                                strlen(buffer),
                                buffer,
                                NULL);
  return NULL;
}



- (int)performCommand:(MAHelperCommand)command
{
  NSString *helperPath;

  helperPath= [[NSBundle bundleForClass:[self class]] pathForResource:@"helper"
                                                               ofType:@""];

  if ([authView authorizationState] == SFAuthorizationViewUnlockedState)
  {
    int hstatus;
    
    hstatus = mhelperPerformCommand([[authView authorization] authorizationRef],
                                    [helperPath UTF8String], command);
    if (hstatus != MAHelperCommandSuccess)
      NSLog(@"helper returned %i for command %i", hstatus, command.authorizedCommandId);
    return hstatus;
  }
  return MAHelperCommandCancelled;
}





- (IBAction)newAccountClose:(id)sender
{
  if ([sender tag] == 12) // cancel
    [NSApp endSheet:imNewAccountWindow returnCode:NO];
  else
  {
    NSString *p1, *p2;
    NSView *view= [imNewAccountWindow contentView];
    NSTextField *error= [view viewWithTag:10];
    BOOL reseting= [[view viewWithTag:11] state] == NSOnState;

    p1= [[view viewWithTag:2] stringValue];
    p2= [[view viewWithTag:4] stringValue];

    if ([p1 length] == 0 && reseting)
    {
      [error setStringValue:@"Please provide a password for the account."];
      [error setHidden:NO];
    }
    else if (![p1 isEqualToString:p2] && reseting)
    {
      [error setStringValue:@"Password confirmation does not match."];
      [error setHidden:NO];
    }
    else if ([[[[imNewAccountWindow contentView] viewWithTag:1] stringValue] length] == 0)
    {
      [error setStringValue:@"Please provide a username for the account."];
      [error setHidden:NO];
    }
    else
      [NSApp endSheet:imNewAccountWindow returnCode:YES];
  }
}


- (void)requestIMAccountDone:(NSWindow *)sheet
                  returnCode:(int)returnCode
                 contextInfo:(void *)contextInfo
{
  *(int*)contextInfo= returnCode;
  [sheet orderOut:self];
  [NSApp stopModal];
}


- (IBAction)toggleCreateAccount:(id)sender
{
  NSView *view= [imNewAccountWindow contentView];

  if ([sender state] == NSOffState)
  {
    [[view viewWithTag:3] setTextColor:[NSColor grayColor]];
    [[view viewWithTag:4] setEnabled:NO];
  }
  else
  {
    [[view viewWithTag:3] setTextColor:[NSColor blackColor]];
    [[view viewWithTag:4] setEnabled:YES];
  }
}


- (IBAction)connectIM:(id)sender
{
  _dontTryIMConnect= NO;
  [self checkStatus];
}


- (void)configureNewInstance
{
  NSString *appBundle= [[NSBundle bundleForClass:[self class]] pathForResource:@"MySQL Setup Assistant"
                                                                        ofType:@"app"];
  NSString *executable= [[NSBundle bundleWithPath:appBundle] executablePath];
  
  NSLog(@"laynch %@ for new ins",
        [[NSBundle bundleForClass:[self class]] pathForResource:@"MySQL Setup Assistant"
                                                         ofType:@"app"]);
  [NSTask launchedTaskWithLaunchPath:executable
                                 arguments:[NSArray arrayWithObject:@"--create"]];
  
}

- (void)reconfigureInstance:(NSString*)name
{
  NSString *appBundle= [[NSBundle bundleForClass:[self class]] pathForResource:@"MySQL Setup Assistant"
                                                                        ofType:@"app"];
  NSString *executable= [[NSBundle bundleWithPath:appBundle] executablePath];
  
  [NSTask launchedTaskWithLaunchPath:executable
                           arguments:[NSArray arrayWithObject:[@"--reconfigure=" stringByAppendingString:name]]];

}

- (void)dropInstance:(NSString*)name
{
  myx_drop_instance_in_im(_imConnection, [name UTF8String]);
}


- (IBAction)performAction:(id)sender
{
  NSString *instance;

  if ([[sender selectedItem] tag] < 10)
    instance= [[_instanceList lastObject] objectForKey:@"name"];
  else
    instance= [[_instanceList objectAtIndex:[instanceList selectedRow]] objectForKey:@"name"];

  switch ([[sender selectedItem] tag])
  {
    case 1:
    case 11:
      [self configureNewInstance];
      break;
    case 2:
    case 12:
      [self reconfigureInstance:instance];
      break;
    case 3:
    case 13:
      if (NSRunAlertPanel(@"Drop Instance", 
                          [NSString stringWithFormat:@"The instance '%@' will be stopped and removed.",instance],
                          @"Cancel",@"Drop",nil) != NSAlertDefaultReturn)
      {
        [self dropInstance:instance];
      }      
      break;
  }
}


- (BOOL)requestIMUsername:(char**)username
                 password:(char**)password
                showError:(NSString*)error
                    reset:(BOOL)flag
{
  int returnCode= 0;
  NSView *view;
  
  if (!imNewAccountWindow)
    [NSBundle loadNibNamed:@"IMLoginInfo" owner:self];

  view= [imNewAccountWindow contentView];

  if (error)
  {
    [[view viewWithTag:10] setStringValue:error];
    [[view viewWithTag:10] setHidden:NO];
  }
  else
    [[view viewWithTag:10] setHidden:YES];
  
  [[view viewWithTag:1] setStringValue:@"dba"];
  [[view viewWithTag:2] setStringValue:@""];
  if (flag)
  {
    [[view viewWithTag:3] setTextColor:[NSColor blackColor]];
    [[view viewWithTag:4] setEnabled:YES];
    //[[view viewWithTag:11] setHidden:YES];
    [[view viewWithTag:11] setState:NSOnState];
  }
  else
  {
    [[view viewWithTag:3] setTextColor:[NSColor grayColor]];
    [[view viewWithTag:4] setEnabled:NO];
    [[view viewWithTag:11] setState:NSOffState];
  }
  [[view viewWithTag:4] setStringValue:@""];
  
  [NSApp beginSheet:imNewAccountWindow
     modalForWindow:[[self mainView] window]
      modalDelegate:self
     didEndSelector:@selector(requestIMAccountDone:returnCode:contextInfo:)
        contextInfo:&returnCode];
  [NSApp runModalForWindow:imNewAccountWindow];
  
  if (returnCode == 1)
  {
    *username= strdup([[[view viewWithTag:1] stringValue] UTF8String]);
    *password= strdup([[[view viewWithTag:2] stringValue] UTF8String]);
    return YES;
  }
  return NO;
}


- (void)showInstanceInfo
{
  NSDictionary *inst= (NSDictionary*)[_instanceList lastObject];
  NSDictionary *options= (NSDictionary*)[inst valueForKey:@"options"];
  // for when there's only 1 instance
  if (inst && [[inst valueForKey:@"status"] boolValue])
  {
    [singleButton setTitle:NSLocalizedString(@"Stop Instance", nil)];
    [singleStatusText setTextColor:[NSColor greenColor]];
    [singleStatusText setStringValue:NSLocalizedString(@"Online",nil)];
    [singleWarningText setStringValue:NSLocalizedString(@"Stopping the MySQL Server will disconnect any users currently connected to it.", nil)];
    [singleIcon setImage:_runningIcon];
  }
  else
  {
    [singleButton setTitle:NSLocalizedString(@"Start Instance", nil)];
    [singleStatusText setTextColor:[NSColor redColor]];
    [singleStatusText setStringValue:NSLocalizedString(@"Offline",nil)];
    [singleWarningText setStringValue:NSLocalizedString(@"", nil)];
    [singleIcon setImage:_stoppedIcon];
  }
  [singleVersionText setStringValue:[inst valueForKey:@"version"]?:@""];
  [singlePortText setStringValue:[options valueForKey:@"port"]?:@"3306"];
  [singleSocketPathText setStringValue:[options valueForKey:@"socket"]?:@""];
  [singleDataPathText setStringValue:[options valueForKey:@"datadir"]?:@""];
  [singleBasePathText setStringValue:[options valueForKey:@"mysqld-path"]?:@""];
}


- (void)checkInstances
{
  BOOL found_password;
  MIM_INSTANCES *instances;
  NSString *error= nil;
  BOOL password_stored= NO;
  NSLog(@"checking instances");
  if (!_imConnection)
  {
    NSLog(@"no im connection");
    if (_dontTryIMConnect)
      return;
    
    if (!_imUsername)
    {
      NSLog(@"no user");
      // lookup keychain for IM user/password
      found_password= password_retrieve(&_imUsername, &_imPassword);
      if (found_password)
        password_stored= YES;
    }
    else
      found_password= YES;
    NSLog(@"found pwd %i", found_password);
    do 
    {
      int error_code;
      char *error_msg= NULL;
      
      if (!found_password)
      {
        if (_imUsername) free(_imUsername); _imUsername= NULL;
        if (_imPassword) free(_imPassword); _imPassword= NULL;
        
        // if no password in keychain, request user for username/password (or create new acct)
        if (!_dontTryIMConnect && ![self requestIMUsername:&_imUsername password:&_imPassword showError:error reset:NO])
        {
          _dontTryIMConnect= YES;
          break;
        }
        else
        {
          password_stored= NO;
        }
      }
      
      // try to connect
      _imConnection= mim_connect(DEFAULT_INSTANCE_MANAGER_PORT, _imUsername, _imPassword,
                         &error_code, &error_msg);
      if (!_imConnection)
      {
        NSLog(@"error connecting with user %s/%s", _imUsername, _imPassword);
        if (error_code == ER_ACCESS_DENIED_ERROR || error_code == ER_DBACCESS_DENIED_ERROR)
        {
          error= NSLocalizedString(@"Access denied to Instance Manager",nil);
          found_password= NO;
        }
        else
        {
          if (NSRunAlertPanel(NSLocalizedString(@"Error Connecting to Instance Manager",nil),
                              [NSString stringWithFormat:NSLocalizedString(@"Could not connect to the IM: %s", nil), error_msg],
                              NSLocalizedString(@"Retry",nil),
                              NSLocalizedString(@"Cancel",nil), 
                              nil) == NSAlertAlternateReturn)
          { // cancel
            if (error_msg) free(error_msg);
            _dontTryIMConnect= YES;
            break;
          }
          else
          {
            // retry
          }
        }
      }
      else
        NSLog(@"connect OK");
      if (error_msg) free(error_msg);
      // if fails, go back to ask for username/password
    } while (!_imConnection);
    
    if (_dontTryIMConnect)
    {    
      [self hideTab:_instanceTab];
      [self hideTab:_instancesTab];
      
      _uiMode= UI_IMCantConnect;
      if (_imConnection)
        mysql_close(_imConnection);
      _imConnection= 0;
      return;
    }
  
    if (!found_password && !password_stored)
    {
      // if succeeds, save user/password in keychain and go on
      password_store(_imUsername, _imPassword);
    }
  }

  NSLog(@"listing instances");
  // get list of instances
  instances= mim_list_instances(_imConnection);
  if (!instances)
  {
    [self checkConnection];
    // XXX show error
    NSLog(@"error fetching list of instances (%s)", mysql_error(_imConnection));
  }
  else
  {
    unsigned int i;

    [_instanceList removeAllObjects];
    for (i= 0; i < instances->instances_num; i++)
    {
      NSMutableDictionary *options= [NSMutableDictionary dictionary];
      unsigned int o;
      for (o= 0; o < instances->instances[i].options->variables_num; o++)
      {
        [options setValue:[NSString stringWithUTF8String:instances->instances[i].options->values[o]?:""]
                   forKey:[NSString stringWithUTF8String:instances->instances[i].options->names[o]]];
      }

      [_instanceList addObject:[NSDictionary dictionaryWithObjectsAndKeys:
        [NSString stringWithUTF8String:instances->instances[i].version], @"version",
        [NSString stringWithUTF8String:instances->instances[i].full_version], @"full_version",
        [NSNumber numberWithBool:instances->instances[i].online], @"status",
        [NSString stringWithUTF8String:instances->instances[i].name], @"name",
        [NSNumber numberWithBool:!instances->instances[i].nonguarded], @"autostart",
        options, @"options",
        nil]];
    }
    
    mim_free_instances(instances);
    
    if ([_instanceList count] == 1)
    {
      [self hideTab:_instancesTab];
      [self showTab:_instanceTab atIndex:0];
      
      [self showInstanceInfo];
      
      _uiMode= UI_SingleInstance;
    }
    else
    {
      [self hideTab:_instanceTab];
      [self showTab:_instancesTab atIndex:0];

      [instanceList reloadData];
      [self tableViewSelectionDidChange:nil];
      
      _uiMode= UI_MultiInstance;
    }
  }
}


- (void)checkStatus
{
  // we can be in one of the following situations:
  // 0 - MySQL not found
  // 1 - IM not found
  // 2 - IM not active
  // 3 - IM active but with no password (ie, cant be used)
  // 4 - IM active with password, but with only 1 instance configured
  // 5 - IM active with password, with more than 1 instance configured
  
  // 1, 2 and 3 are treated the same and the server is simple started/stopped normally
  // through /usr/local/mysql/support-files/mysql.server start/stop
  // if 2, allow enable IM
  // if 3, recommend to set the password
  
  // for 4, show simplified, single instance UI
  // for 5, show full UI
  if (_checking) return;
  _checking= YES;

  if (!mim_instance_manager_is_installed(_mysqlPath))
  {
    if (_uiMode != UI_IMNotFound)
    {
      _uiMode= UI_IMNotFound;
      
      [tabview setTabViewType:NSNoTabsNoBorder];
      [self showTab:_imDisabledTab atIndex:0];
      [tabview selectTabViewItemWithIdentifier:@"imDisabled"];
      
      [imlessEnableButton setEnabled:NO];
      [imlessNotFoundText setHidden:YES];
    }
  }
  else if (!mim_instance_manager_is_active())
  {
    // show "pls activate IM" interface
    _uiMode= UI_IMNotActive;
    
    [tabview setTabViewType:NSNoTabsNoBorder];
    [self showTab:_imDisabledTab atIndex:0];
    [tabview selectTabViewItemWithIdentifier:@"imDisabled"];
    
    [imlessEnableButton setEnabled:YES];
    [imlessNotFoundText setHidden:NO];
  }
  else      
  {
    [self hideTab:_imDisabledTab];

    if (mim_instance_manager_has_password())
    {    
      [imPasswordButton setTitle:NSLocalizedString(@"Change IM Password...",nil)];
      [tabview setTabViewType:NSTopTabsBezelBorder];

      if (mim_get_auto_starts())
        [imCheck setState:NSOnState];
      else
        [imCheck setState:NSOffState];
      
      if (mim_instance_manager_is_running(DEFAULT_INSTANCE_MANAGER_PORT))
      {
        [imButton setTitle:NSLocalizedString(@"Stop MySQL IM", nil)];
        [imWarningText setStringValue:NSLocalizedString(@"Stopping the IM will also stop all MySQL server instances.", nil)];
        [imStateText setTextColor:[NSColor greenColor]];
        [imStateText setStringValue:NSLocalizedString(@"Online", nil)];
        [imStateImage setImage:_runningIcon];
        
        if ([authView authorizationState] == SFAuthorizationViewUnlockedState)
          [singleButton setEnabled:YES];
        else
          [singleButton setEnabled:NO];
        [self checkInstances];
      }
      else
      {
        if (_uiMode != UI_IMNotRunning)
        {
          // we don't know how many instances there are, so just show the single instance tab
          [self hideTab:_instancesTab];
          [self showTab:_instanceTab atIndex:0]; 
          
          [imButton setTitle:NSLocalizedString(@"Start MySQL IM", nil)];
          [imWarningText setStringValue:NSLocalizedString(@"The MySQL Instance Manager and Database Server are currently stopped.\nClick on 'Start MySQL IM' to start them.", nil)];
          [imStateText setTextColor:[NSColor redColor]];
          [imStateText setStringValue:NSLocalizedString(@"Offline", nil)];
          [imStateImage setImage:_stoppedIcon];
          
          [self showInstanceInfo];
          [singleButton setEnabled:NO];

          _uiMode = UI_IMNotRunning;
        }
      }
      
      if (_uiMode == UI_IMCantConnect)
      {
        [imConnectButton setEnabled:YES];
        [imConnectButton setHidden:NO];
        [imPasswordText setStringValue:NSLocalizedString(@"Connect to the Instance Manager by clicking on 'Connect to IM...' to enable more functionality.", nil)];
      }
      else
      {
        [imConnectButton setHidden:YES];
        [imPasswordText setStringValue:@""];        
      }
    }
    else
    {
      NSLog(@"NO PASSWORD");
      // no password set, just show simplified UI with set password button
      [self hideTab:_instanceTab];
      [self hideTab:_instancesTab];

      _uiMode= UI_IMPasswordless;
      
      [imConnectButton setHidden:YES];
      [imPasswordButton setTitle:NSLocalizedString(@"Set IM Password...", nil)];
      [imPasswordText setStringValue:NSLocalizedString(@"The Instance Manager is currently not accepting remote connections. If you want to enable"
                                                     "remote (and local) management of MySQL Instances in this computer, you must set a password for it.", nil)];
      
      if (mim_get_auto_starts())
        [imCheck setState:NSOnState];
      else
        [imCheck setState:NSOffState];
      
      if (mim_instance_manager_is_running(DEFAULT_INSTANCE_MANAGER_PORT))
      {
        [imButton setTitle:NSLocalizedString(@"Stop MySQL IM", nil)];
        [imWarningText setStringValue:NSLocalizedString(@"Stopping the IM will also stop all MySQL server instances.", nil)];
        [imStateText setTextColor:[NSColor greenColor]];
        [imStateText setStringValue:NSLocalizedString(@"Online", nil)];
        [imStateImage setImage:_runningIcon];
      }
      else
      {
        [imButton setTitle:NSLocalizedString(@"Start MySQL IM", nil)];
        [imWarningText setStringValue:NSLocalizedString(@"The MySQL Instance Manager and Database Server are currently stopped.\nClick on 'Start MySQL IM' to start them.", nil)];
        [imStateText setTextColor:[NSColor redColor]];
        [imStateText setStringValue:NSLocalizedString(@"Offline", nil)];
        [imStateImage setImage:_stoppedIcon];
      }
    }
  }
  _checking= NO;
}



- (BOOL)setIMPassword:(char*)password user:(char*)username
{
  MAHelperCommand cmd;
  int rc;
  
  memset(&cmd, 0, sizeof(cmd));
  cmd.authorizedCommandId= MAHelperSetIMPassword;
  strncpy(cmd.username, username, sizeof(cmd.username)-1);
  cmd.username[sizeof(cmd.username)-1]= 0;
  strncpy(cmd.password, password, sizeof(cmd.password)-1);
  cmd.password[sizeof(cmd.password)-1]= 0;
  
  rc= [self performCommand:cmd] == MAHelperCommandSuccess;

  return rc;
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


- (IBAction)toggleServerx:(id)sender
{
  MAHelperCommand command;
  BOOL start= !mim_instance_manager_is_running(DEFAULT_INSTANCE_MANAGER_PORT);
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


- (IBAction)changeIMAutoStart:(id)sender
{
  MAHelperCommand command;
  int rc;
  
  command.authorizedCommandId= MAHelperToggleAutoStart;
  command.enable= !mim_get_auto_starts();
  if ((rc = [self performCommand:command]) != MAHelperCommandSuccess)
  {
    if (rc != MAHelperCommandCancelled)
      NSRunAlertPanel(NSLocalizedString(@"Error",nil),
                      [NSString stringWithFormat:NSLocalizedString(@"Could not change automatic startup configuration.\nReason: %@",nil), errorMessage(rc)], 
                      nil, nil, nil);
  }
}


- (IBAction)setIMPassword:(id)sender
{
  char *username= NULL;
  char *password= NULL;
  
  if ([self requestIMUsername:&username
                     password:&password
                    showError:nil
                        reset:YES])
  {
    if (_imUsername) free(_imUsername);
    _imUsername= username;
    if (_imPassword) free(_imPassword);
    _imPassword= password;
    
    if (![self setIMPassword:password user:username])
      NSRunAlertPanel(NSLocalizedString(@"Error",nil),
                      NSLocalizedString(@"Could not change IM password.",nil), 
                      nil, nil, nil);
    else
    {
      [imPasswordText setStringValue:@"Instance Manager password changed.\nThe new password will only take effect after the IM is restarted."];
      //don't save the new password as it's unlikely the IM will get restarted too soon
      //password_store(username, password);
    }
  }
}



- (IBAction)setServerPassword:(id)sender
{
}


- (IBAction)showHelp:(id)sender
{
}


- (IBAction)toggleIM:(id)sender
{
  MAHelperCommand cmd;
  int rc;
  BOOL start= _uiMode == UI_IMNotRunning;
  
  if ([authView authorizationState] == SFAuthorizationViewLockedState)
  {
    [authView authorize:self];
  }
  if ([authView authorizationState] != SFAuthorizationViewUnlockedState)
    return;
  
  if (start)
    [imWarningText setStringValue:NSLocalizedString(@"Starting Instance Manager...", nil)];
  else
    [imWarningText setStringValue:NSLocalizedString(@"Stopping Instance Manager...", nil)];
  
  [imButton setEnabled:NO];
  
  memset(&cmd, 0, sizeof(cmd));
  cmd.authorizedCommandId= start ? MAHelperStartMySQL : MAHelperStopMySQL;
  rc= [self performCommand:cmd];

  if (rc != MAHelperCommandSuccess)
  {
    if (rc != MAHelperCommandCancelled)
      NSRunAlertPanel(NSLocalizedString(@"Error",nil),
                      [NSString stringWithFormat:NSLocalizedString(@"Could not change automatic startup configuration.\nReason: %@",nil), errorMessage(rc)], 
                      nil, nil, nil);
    if (start)
      [imWarningText setStringValue:NSLocalizedString(@"Could not start IM", nil)];
    else
      [imWarningText setStringValue:NSLocalizedString(@"Could not stop IM", nil)];
  }
  
  [imButton setEnabled:YES];
}


- (IBAction)toggleServer:(id)sender
{
  NSString *instance= nil;
  
  if ([authView authorizationState] == SFAuthorizationViewLockedState)
    [authView authorize:nil];

  if ([authView authorizationState] != SFAuthorizationViewUnlockedState)
    return;
  
  if ([sender tag] == 200)
  {
    int row= [instanceList selectedRow];
    if (row >= 0)
    {
      NSDictionary *inst= [_instanceList objectAtIndex:row];
      
      instance= [inst valueForKey:@"name"];
      if ([[inst valueForKey:@"status"] boolValue])
      {
        mim_stop_instance(_imConnection, [instance UTF8String]);
      }
      else
      {
        mim_start_instance(_imConnection, [instance UTF8String]);
      }
      [self checkConnection];
      
      [self checkInstances];
    }
  }
  else if ([sender tag] == 100)
  {
    NSDictionary *inst= [_instanceList lastObject];
    
    instance= [inst valueForKey:@"name"];
    if ([[inst valueForKey:@"status"] boolValue])
    {
      mim_stop_instance(_imConnection, [instance UTF8String]);
    }
    else
    {
      mim_start_instance(_imConnection, [instance UTF8String]);
    }
    
    [self checkInstances];
  }
  else if ([sender tag] == 400)
  {
    
  }
}



- (int)numberOfRowsInTableView:(NSTableView*)aTableView
{
  return [_instanceList count];
}


- (id)tableView:(NSTableView*)aTableView
objectValueForTableColumn:(NSTableColumn*)aTableColumn
row:(int)rowIndex
{
  NSDictionary *inst= (NSDictionary*)[_instanceList objectAtIndex:rowIndex];

  if ([[aTableColumn identifier] isEqualToString:@"name"])
  {
    return [inst valueForKey:@"name"];
  }
  else if ([[aTableColumn identifier] isEqualToString:@"autostart"])
  {
    return [inst valueForKey:@"autostart"];
  }
  else if ([[aTableColumn identifier] isEqualToString:@"version"])
  {
    return [inst valueForKey:@"version"];
  }    
  else
  {
    return [NSString stringWithFormat:@"Port: %@\nSocket: %@", 
      [[inst valueForKey:@"options"] valueForKey:@"port"]?:@"", 
      [[inst valueForKey:@"options"] valueForKey:@"socket"]?:@""];
  }
}


- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  NSDictionary *inst;
  NSDictionary *options;
  
  if ([instanceList selectedRow] >= 0)
  {
    inst= [_instanceList objectAtIndex:[instanceList selectedRow]];
    if ([authView authorizationState] == SFAuthorizationViewUnlockedState)
    {
      [serverButton setEnabled:YES];
      [serverActionButton setEnabled:YES];
    }
    if ([[inst valueForKey:@"status"] boolValue])
      [serverButton setTitle:NSLocalizedString(@"Stop Instance", nil)];
    else
      [serverButton setTitle:NSLocalizedString(@"Start Instance", nil)];
  }
  else
  {
    inst= nil;
    [serverButton setEnabled:NO];
    [serverActionButton setEnabled:NO];
  }
  options= [inst valueForKey:@"options"];
  [serverVersionText setStringValue:[inst valueForKey:@"version"]?:@""];
  [serverDataPathText setStringValue:[options valueForKey:@"datadir"]?:@""];
  [serverBasePathText setStringValue:[options valueForKey:@"mysqld-path"]?:@""];
}


- (void)authorizationViewDidAuthorize:(SFAuthorizationView *)view
{
  [imButton setEnabled:YES];
  [imCheck setEnabled:YES];
  [imPasswordButton setEnabled:YES];
  [imlessButton setEnabled:YES];
  [imlessEnableButton setEnabled:YES];
  [serverButton setEnabled:YES];
  [singleButton setEnabled:YES];
  [serverActionButton setEnabled:YES];
  [singleActionButton setEnabled:YES];
}


- (void)authorizationViewDidDeauthorize:(SFAuthorizationView *)view
{
  [imButton setEnabled:NO];
  [imCheck setEnabled:NO];
  [imPasswordButton setEnabled:NO];
  [imlessButton setEnabled:NO];
  [imlessEnableButton setEnabled:NO];
  [serverButton setEnabled:NO];
  [singleButton setEnabled:NO];    
  [serverActionButton setEnabled:NO];
  [singleActionButton setEnabled:NO];
}


- (void)tableView:(NSTableView *)aTableView willDisplayCell:(id)aCell forTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualToString:@"name"])
  {  
    NSDictionary *inst= (NSDictionary*)[_instanceList objectAtIndex:rowIndex];

    if ([[inst valueForKey:@"status"] boolValue])
      [aCell setImage:_runningMiniIcon];
    else
      [aCell setImage:_stoppedMiniIcon];
    [aCell setBackgroundColor:[[NSColor controlAlternatingRowBackgroundColors] objectAtIndex:rowIndex%[[NSColor controlAlternatingRowBackgroundColors] count]]];
  }
}



@end
