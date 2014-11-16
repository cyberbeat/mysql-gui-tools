/* MAUserAdministration */

#import <Cocoa/Cocoa.h>

#import "MAPanel.h"
#include "myx_admin_public_interface.h"

@class MOutlineView;
@class MFilteredSchemaDataSource;
@class MUserListDataSource;
@class PrivilegeDataSource;
@class MUserItem;

@interface MAUserAdministration : MAPanel
{
    IBOutlet NSTextField *confirmText;
    IBOutlet NSTextView *contactText;
    IBOutlet NSTextField *descriptionText;
    IBOutlet NSTextField *emailText;
    IBOutlet NSTextField *fullNameText;
    IBOutlet NSTableView *globalAssignedTable;
    IBOutlet NSTableView *globalAvailableTable;
    IBOutlet NSImageView *iconView;
    IBOutlet NSTextField *maxConnectionsText;
    IBOutlet NSTextField *maxQuestionText;
    IBOutlet NSTextField *maxUpdatesText;
    IBOutlet NSTextField *passwordText;
    IBOutlet NSTableView *schemaAssignedTable;
    IBOutlet NSTableView *schemaAvailableTable;
    IBOutlet NSOutlineView *schemaOutline;
    IBOutlet NSTextField *schemaSearch;
//    IBOutlet NSView *sideBox;
    IBOutlet NSTableView *tableAssignedTable;
    IBOutlet NSTableView *tableAvailableTable;
    IBOutlet NSOutlineView *tableOutline;
    IBOutlet NSTextField *tableSearch;
    IBOutlet NSTextField *usernameText;
    IBOutlet MOutlineView *userOutline;
    IBOutlet NSSearchField *userSearch;
    IBOutlet NSTextField *userLabel1;
    IBOutlet NSTextField *userLabel2;
    IBOutlet NSTextField *userLabel3;
    IBOutlet NSTextField *userLabel4;
    IBOutlet NSTabView *tabView;

    IBOutlet NSPanel *addHostPanel;
    IBOutlet NSMatrix *addHostMatrix;
    IBOutlet NSTextField *addHostText;


    MUserListDataSource *_userDS;
    PrivilegeDataSource *_globalPrivDS;
    PrivilegeDataSource *_schemaPrivDS;
    PrivilegeDataSource *_tablePrivDS;
    PrivilegeDataSource *_columnPrivDS;
    PrivilegeDataSource *_routinePrivDS;
	
    MFilteredSchemaDataSource *_schemaDS;
    MFilteredSchemaDataSource *_objectDS;
    
    id globalPageView;
    BOOL globalVisible;
    id tablePageView;
    BOOL tableVisible;

    NSString *_oldHost;
    MUserItem *_oldUser;
    int _oldRow;
    
    // privilege templates
    MYX_USER_OBJECT_PRIVILEGES *_globalPrivs;
    MYX_USER_OBJECT_PRIVILEGES *_schemaPrivs;
    MYX_USER_OBJECT_PRIVILEGES *_tablePrivs;    
    MYX_USER_OBJECT_PRIVILEGES *_columnPrivs;
    MYX_USER_OBJECT_PRIVILEGES *_routinePrivs;
    
    NSImage *_privilegeIcon;
	
    int _selectedUserRow;
}
- (IBAction)addUser:(id)sender;
- (IBAction)removeUser:(id)sender;
- (IBAction)addHost:(id)sender;
- (IBAction)removeHost:(id)sender;
- (IBAction)chooseIcon:(id)sender;
- (IBAction)discardChanges:(id)sender;
- (IBAction)globalAddPrivilege:(id)sender;
- (IBAction)globalRemovePrivilege:(id)sender;
- (IBAction)saveChanges:(id)sender;
- (IBAction)schemaAddPrivilege:(id)sender;
- (IBAction)schemaRemovePrivilege:(id)sender;
- (IBAction)tableAddPrivilege:(id)sender;
- (IBAction)tableRemovePrivilege:(id)sender;

- (IBAction)addHostAdd:(id)sender;
- (IBAction)addHostCancel:(id)sender;

- (IBAction)filterTableList:(id)sender;
- (IBAction)filterSchemaList:(id)sender;
- (IBAction)filterUserList:(id)sender;

- (IBAction)refreshUserList:(id)sender;

- (void)showUser:(MUserItem*)user host:(NSString*)host;
- (void)showUser:(MUserItem*)user host:(NSString*)host keepAccountInfo:(BOOL)flag;

- (void)removeUserAlertDidEnd:(NSAlert *)alert returnCode:(int)returnCode
                  contextInfo:(void *)contextInfo;

@end
