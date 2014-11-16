/* MAdministratorController */

#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"

@class MAdministrator;
@class MConnectionInfo;

// this does the role of a NSDocumentController
@interface MAdministratorController : NSObject
{
  IBOutlet id aboutPanel;
  IBOutlet id versionLabel;
  IBOutlet id recentMenu;
  
  id _prefPanel;
  NSMutableArray *_instances;
}

- (IBAction)showAbout:(id)sender;
- (IBAction)showPreferences:(id)sender;
- (IBAction)clearRecentMenu:(id)sender;

- (IBAction)enterpriseHome:(id)sender;
- (IBAction)enterpriseSoftware:(id)sender;
- (IBAction)enterpriseKB:(id)sender;
- (IBAction)enterpriseUpdate:(id)sender;
- (IBAction)enterpriseMonitoring:(id)sender;
- (IBAction)enterpriseSupport:(id)sender;

+ (MAdministratorController*)sharedDocumentController;

- (id)init;

- (void)newDocument: (id)sender;

- (void)addInstance: (MAdministrator*)instance;

- (void)removeInstance: (MAdministrator*)instance;

- (void)openInstanceWithConnectionName: (NSString*)connectionName;

- (void)noteNewRecentConnection: (MConnectionInfo*)info;

@end
