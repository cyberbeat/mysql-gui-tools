/* MAServerConnections */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"
#include "myx_admin_public_interface.h"


@class ProcessDataSource;
@class UserDataSource;

@interface MAServerConnections : MAPanel
{
    IBOutlet NSTableView *connectionsTable;
    IBOutlet NSTableView *fullTable;
    IBOutlet NSButton *killButton;
    IBOutlet NSButton *killUserButton;
    IBOutlet NSTableView *userTable;
    IBOutlet NSTabView *tabView;

    NSMutableDictionary *_users;
    MYX_PROCESS_LIST *_plist;
    
    ProcessDataSource *_connectionsDS;
    ProcessDataSource *_fullDS;
    UserDataSource *_userDS;
    
    NSTimer *_timer;
}
- (IBAction)killConnection:(id)sender;
- (IBAction)killUser:(id)sender;
- (IBAction)refresh:(id)sender;
@end
