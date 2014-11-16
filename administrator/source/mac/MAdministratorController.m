#import "MAdministratorController.h"

#import "MAdministrator.h"

#import "MAHomePanel.h"
#import "MAServerInformation.h"
#import "MAUserAdministration.h"
#import "MABackup.h"
#import "MARestore.h"
#import "MAServerLogs.h"
#import "MAServerConnections.h"
#import "MACatalogs.h"
#import "MAHealth.h"
#import "MAServiceControl.h"
#import "MAStartupVariables.h"

#import <MySQLToolsCommon/MConnectionPanel.h>
#import <MySQLToolsCommon/MPreferences.h>
#import <MySQLToolsCommon/MPreferenceEditor.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>

#include "config.h"

@interface MAdministratorController(Private)

- (void)applicationWillFinishLaunching:(NSNotification *)aNotification;
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification;
- (void)connectionPanel:(MConnectionPanel*)panel finished:(MYSQL*)mysql info:(MConnectionInfo*)info;
- (void)connectionPanel:(MConnectionPanel*)panel aborted:(BOOL)error;

- (void)recentMenuActivate:(id)sender;
- (void)updateRecentMenu;
@end


@implementation MAdministratorController(Private)

- (void)applicationWillFinishLaunching:(NSNotification *)aNotification
{
  // don't need to instantiate, it's already done from the nib
  //[[MAdministratorController alloc] init];
  
  // register all modules/panels
//  MARegisterPanel([MAHomePanel class]);
  MARegisterPanel([MAServerInformation class]);
  MARegisterPanel([MAServiceControl class]);
  MARegisterPanel([MAStartupVariables class]);
  MARegisterPanel([MAUserAdministration class]);
  MARegisterPanel([MAServerConnections class]);
  MARegisterPanel([MAHealth class]);
  MARegisterPanel([MAServerLogs class]);
  MARegisterPanel([MABackup class]);
  MARegisterPanel([MARestore class]);
  MARegisterPanel([MACatalogs class]);
}


- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  [self newDocument:nil];
}


- (void)connectionPanel:(MConnectionPanel*)panel finished:(MYSQL*)mysql info:(MConnectionInfo*)info
{
  MAdministrator *admin;

  if (!mysql || !info)
  {
    admin= [[MAdministrator alloc] initWithoutConnection];
  }
  else
    admin= [[MAdministrator alloc] initWithConnection:mysql info:info];
  if (admin)
  {
    [self addInstance: admin];
    [admin show];
    [admin release];
  }
  else
    NSLog(@"Main window initialization failed!");

  [panel autorelease];
}

- (void)connectionPanel:(MConnectionPanel*)panel aborted:(BOOL)error
{
  [panel release];
}


- (void)recentMenuActivate:(id)sender
{
  [self openInstanceWithConnectionName:[sender representedObject]];
}


- (void)updateRecentMenu
{
  NSMenu *menu= [recentMenu submenu];
  unsigned int i;
  NSArray *array= [[NSUserDefaults standardUserDefaults] arrayForKey:@"RecentConnections"];
  
  while ([menu numberOfItems] > 2)
    [menu removeItemAtIndex:0];
   
  for (i= 0; i < [array count]; i++)
  {
    id item= [menu addItemWithTitle:[array objectAtIndex:i]
                             action:@selector(recentMenuActivate:)
                      keyEquivalent:@""];
    [item setRepresentedObject:[array objectAtIndex:i]];
  }
}

@end


//=======================================================================

@implementation MAdministratorController


static MAdministratorController *theSharedInstance= nil;


+ (MAdministratorController*)sharedDocumentController
{
  if (!theSharedInstance)
    theSharedInstance= [[MAdministratorController alloc] init];
  
  return theSharedInstance;
}


- (id)init
{
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  NSMutableDictionary *appDefaults = [NSMutableDictionary dictionaryWithObjectsAndKeys: 
    [NSArray array], @"RecentConnections",
    [NSNumber numberWithBool:NO], @"DisplayGlobalPrivileges",
    [NSNumber numberWithBool:NO], @"DisplayTablePrivileges",
    [NSNumber numberWithInt:0], @"ConnectionListRefreshDelay",
    [MPreferences checkDirectory:@"Administrator/Profiles"], @"BackupProfileDirectory",
    nil];

  [appDefaults addEntriesFromDictionary:[MSyntaxColoring defaultColorDictionary]];

  [defaults registerDefaults:appDefaults];

  [NSApp setDelegate:self];
  
  self= [super init];
  if (!self)
    return nil;

  _instances= [[NSMutableArray alloc] init];

  theSharedInstance= self;
  
  return self;
}

- (void)awakeFromNib
{
  [self updateRecentMenu];
}


- (void)dealloc
{
  [_prefPanel release];
  [_instances release];
  [super dealloc];
}


- (void)newDocument: (id)sender
{
  MConnectionPanel *panel;

  panel= [[MConnectionPanel alloc] initWithConnectionsFile: 
    [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];
  [panel setDelegate:self];
  [panel show];
  [panel setAllowSkip];
  [panel setHeaderImage: [[[NSImage alloc] initWithContentsOfFile: 
    [[NSBundle mainBundle] pathForResource:@"connect_header_admin"
                                    ofType:@"png"]] autorelease]];
}


- (void)openInstanceWithConnectionName: (NSString*)connectionName
{
  MConnectionPanel *panel;
  
  panel= [[MConnectionPanel alloc] initWithConnectionsFile: 
    [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];
  [panel setDelegate:self];
  [panel setAllowSkip];
  [panel showAndConnectTo:connectionName];
  [panel setHeaderImage: [[[NSImage alloc] initWithContentsOfFile: 
    [[NSBundle mainBundle] pathForResource:@"connect_header_admin"
                                    ofType:@"png"]] autorelease]];
}


- (void)addInstance: (MAdministrator*)instance
{
  [_instances addObject: instance];
  if ([instance serverInfo])
    [self noteNewRecentConnection:[instance serverInfo]];
}


- (void)removeInstance: (MAdministrator*)instance
{
  [_instances removeObject: instance];
}


- (void)noteNewRecentConnection: (MConnectionInfo*)info
{
  NSString *name;
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  NSMutableArray *array= [NSMutableArray arrayWithArray:[defaults objectForKey:@"RecentConnections"]];
  
  name= [info description];
  
  [array removeObject:name];
  
  while ([array count] > 9)
  {
    [array removeLastObject];
  }
  
  [array insertObject:name atIndex:0];
  [defaults setObject:array forKey:@"RecentConnections"];
  [self updateRecentMenu];
}


- (IBAction)showAbout:(id)sender
{
  [versionLabel setStringValue:[NSString stringWithFormat:@"Version %@", 
    [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleVersion"]]];

  [aboutPanel makeKeyAndOrderFront:sender];
}


- (IBAction)showPreferences:(id)sender
{
  if (!_prefPanel)
  {
    _prefPanel= [[MPreferenceEditor alloc] initForConnectionsFile:
      [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];

    [_prefPanel registerPageNib:@"Preferences" withLabel:@"Administrator"];
  }
  [_prefPanel show];
}


- (IBAction)clearRecentMenu:(id)sender
{
  [[NSUserDefaults standardUserDefaults] setObject:[NSArray array] forKey:@"RecentConnections"];
  [self updateRecentMenu];
}

- (IBAction)enterpriseHome:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://enterprise.mysql.com/"]];
}

- (IBAction)enterpriseSoftware:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://enterprise.mysql.com/software/enterprise.php"]];
}

- (IBAction)enterpriseKB:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://kb.mysql.com/index.php"]];
}

- (IBAction)enterpriseUpdate:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://enterprise.mysql.com/updates/index.php"]];
}

- (IBAction)enterpriseMonitoring:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://enterprise.mysql.com/monitoring/"]];
}

- (IBAction)enterpriseSupport:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL: [NSURL URLWithString: @"https://support.mysql.com/main.php"]];
}

@end
