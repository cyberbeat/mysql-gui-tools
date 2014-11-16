
#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"

@class MQueryBrowser;
@class MQHistory;
@class MQBookmarkList;
@class MConnectionInfo;

// this does the role of a NSDocumentController
@interface MQueryBrowserController : NSDocumentController
{
  IBOutlet id aboutPanel;
  IBOutlet id versionLabel;
  IBOutlet id windowsMenu;
  
  MQHistory *_history;
  BOOL _historyNeedsSave;
  MQBookmarkList *_bookmarks;
  
  id _prefPanel;
}

- (IBAction)showAbout:(id)sender;
- (IBAction)showPreferences:(id)sender;
- (IBAction)clearRecentMenu:(id)sender;
- (IBAction)newConnection:(id)sender;

- (void)openInstanceWithConnectionName: (NSString*)connectionName;

- (void)noteNewRecentConnection: (MConnectionInfo*)info;

- (IBAction)enterpriseHome:(id)sender;
- (IBAction)enterpriseSoftware:(id)sender;
- (IBAction)enterpriseKB:(id)sender;
- (IBAction)enterpriseUpdate:(id)sender;
- (IBAction)enterpriseMonitoring:(id)sender;
- (IBAction)enterpriseSupport:(id)sender;

- (MQHistory*)history;
- (MQBookmarkList*)bookmarks;
- (void)rememberQuery:(NSString*)query
			  catalog:(NSString*)catalog
			   schema:(NSString*)schema;

@end
