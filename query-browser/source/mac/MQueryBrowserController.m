#import "MQueryBrowserController.h"
#import "MQueryBrowserDocument.h"

#import "MQueryBrowser.h"
#import "MQHistory.h"
#import "MQBookmark.h"
#import "MQPreferences.h"

#import <MySQLToolsCommon/MConnectionPanel.h>
#import <MySQLToolsCommon/MPreferences.h>
#import <MySQLToolsCommon/MPreferenceEditor.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>

@interface MQueryBrowserController(Private)

- (void)connectionPanel:(MConnectionPanel*)panel finished:(MYSQL*)mysql info:(MConnectionInfo*)info;
- (void)connectionPanel:(MConnectionPanel*)panel aborted:(BOOL)error;

- (void)recentMenuActivate:(id)sender;
- (void)updateRecentMenu;
@end


@implementation MQueryBrowserController(Private)

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  [self newConnection:nil];
}


- (void)connectionPanel:(MConnectionPanel*)panel finished:(MYSQL*)mysql info:(MConnectionInfo*)info
{
  MQueryBrowserDocument *qb;
  qb= [[MQueryBrowserDocument alloc] initWithConnection:mysql info:info];

  if (qb)
  {
    [self addDocument:qb];
    [qb makeWindowControllers];
//    [self noteNewRecentConnection:info];
    [qb release];
  }
  else
    NSLog(@"Main window initialization failed!");
  [panel autorelease];
}

- (void)connectionPanel:(MConnectionPanel*)panel aborted:(BOOL)error
{
  [panel release];
}

#if 0
- (void)recentMenuActivate:(id)sender
{
  [self openInstanceWithConnectionName:[sender representedObject]];
}


- (void)updateRecentMenu
{
/*
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
 */
}
#endif
@end


//=======================================================================

@implementation MQueryBrowserController

- (void)setupDefaults
{
  NSMutableDictionary *appDefaults= [NSMutableDictionary dictionaryWithCapacity:8];
  [appDefaults setObject:[NSNumber numberWithInt:0] forKey:@"QueryHistoryLimit"];
  [appDefaults setObject:[NSArchiver archivedDataWithRootObject:[NSColor whiteColor]] forKey:@"EditorBackgroundColor"];
  [appDefaults setObject:[NSArchiver archivedDataWithRootObject:[NSColor blackColor]] forKey:@"EditorForegroundColor"];
  [appDefaults setObject:[NSArchiver archivedDataWithRootObject:[NSFont fontWithName:@"Monaco" size:12.0]] forKey:@"EditorFont"];
  [appDefaults setObject:[NSArray array] forKey:@"RecentConnections"];
  [appDefaults setObject:[MPreferences checkDirectory:@"QueryBrowser"] forKey: @"QBDirectory"];
  [appDefaults setObject:[NSNumber numberWithInt:100] forKey:@"PartialTableListLimit"];
  [appDefaults setObject:[NSNumber numberWithInt: 18] forKey:@"ResultsetRowHeight"];
  
  [appDefaults addEntriesFromDictionary:[MSyntaxColoring defaultColorDictionary]];

  [[NSUserDefaults standardUserDefaults] registerDefaults:appDefaults];
}

- (id)init
{
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
    
  [self setupDefaults];
  
  self= [super init];
  if (!self)
    return nil;
  
  _history= [[MQHistory alloc] init];
  [_history loadFromFile: [NSString stringWithFormat:@"%@/history.xml",
	[defaults valueForKey:@"QBDirectory"]]];
  
  _bookmarks= [[MQBookmarkList alloc] init];
  [_bookmarks loadFromFile:[NSString stringWithFormat:@"%@/bookmarks.xml",
    [defaults valueForKey:@"QBDirectory"]]];

  return self;
}


- (void)dealloc
{
  [_history release];
  [_prefPanel release];
  [super dealloc];
}


- (IBAction)newConnection: (id)sender
{
  MConnectionPanel *panel;

  panel= [[MConnectionPanel alloc] initWithConnectionsFile: 
    [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];
  [panel setEditsSchema];
  [panel setDelegate:self];
  [panel show];
  [panel setHeaderImage: [[[NSImage alloc] initWithContentsOfFile: 
    [[NSBundle mainBundle] pathForResource:@"connect_header_qb"
                                    ofType:@"png"]] autorelease]];
}


- (void)openInstanceWithConnectionName: (NSString*)connectionName
{
  MConnectionPanel *panel;

  panel= [[MConnectionPanel alloc] initWithConnectionsFile: 
    [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];
  [panel setEditsSchema];
  [panel setDelegate:self];
  [panel showAndConnectTo:connectionName];
  [panel setHeaderImage: [[[NSImage alloc] initWithContentsOfFile: 
    [[NSBundle mainBundle] pathForResource:@"connect_header_qb"
                                    ofType:@"png"]] autorelease]];
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
    MQPreferences *prefs;
    
    _prefPanel= [[MPreferenceEditor alloc] initForConnectionsFile:
      [[MPreferences preferences] pathForFile:@"mysqlx_user_connections.xml"]];

    prefs= [_prefPanel registerMultiPageNib:@"Preferences" withLabels:[NSArray arrayWithObjects:@"Query Browser",@"SQL Editor",nil]];
    [prefs setHistory:_history];
  }
  [_prefPanel show];
}


- (IBAction)clearRecentMenu:(id)sender
{
  [[NSUserDefaults standardUserDefaults] setObject:[NSArray array] forKey:@"RecentConnections"];
  [self updateRecentMenu];
}

- (MQHistory*)history
{
  return _history;
}

- (void)rememberQuery:(NSString*)query
			  catalog:(NSString*)catalog
			   schema:(NSString*)schema
{
  [_history rememberQuery:query catalog:catalog schema:schema];
  _historyNeedsSave= YES;
}

- (MQBookmarkList*)bookmarks
{
  return _bookmarks;
}


- (void)applicationWillTerminate:(NSNotification *)aNotification
{
  if (_historyNeedsSave)
  {
    _historyNeedsSave= NO;
    [_history storeToFile:[NSString stringWithFormat:@"%@/history.xml",
      [[NSUserDefaults standardUserDefaults] valueForKey:@"QBDirectory"]]];
  }

  [_bookmarks storeToFile:[NSString stringWithFormat:@"%@/bookmarks.xml",
    [[NSUserDefaults standardUserDefaults] valueForKey:@"QBDirectory"]]];
}


- (int)numberOfItemsInMenu:(NSMenu *)menu
{
  return [menu numberOfItems];
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


- (BOOL)menu:(NSMenu *)menu
  updateItem:(NSMenuItem *)item
	 atIndex:(int)index
shouldCancel:(BOOL)shouldCancel
{
  NSWindow *window= [NSApp keyWindow];
  if (window)
  {
    MQueryBrowser *document= [self documentForWindow:window];
    
    if (document)
      return [document menu:menu
                 updateItem:item
                    atIndex:index
               shouldCancel:NO];
  }
  if (windowsMenu != menu)
    [item setEnabled:NO];
  return YES;
}


@end
