/* MAHealth */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"
#include "myx_admin_public_interface.h"

@interface MAHealth : MAPanel
{
    IBOutlet NSOutlineView *serverIndex;
    IBOutlet NSTableView *serverTable;
    IBOutlet NSOutlineView *statusIndex;
    IBOutlet NSTableView *statusTable;
    IBOutlet NSTabView *tabView;
    IBOutlet NSMenu *pageMenu;
    IBOutlet NSMenu *editMenu;
    
    BOOL _cancelThread;
    
    NSImage *_editableIcon;
    NSImage *_nonEditableIcon;

    MYX_VARIABLES_LISTING *_serverVariableListing;
    MYX_VARIABLES_LISTING *_statusVariableListing;
    
    //MYX_TRANS *_trans;

    NSMutableArray *_pages;
    BOOL _saveGraphs;

    MYX_VARIABLES *_statusVariables;
    MYX_VARIABLES *_serverVariables;
}

- (IBAction)refreshServerVariables:(id)sender;
- (IBAction)refreshStatusVariables:(id)sender;

- (IBAction)revertToFactory:(id)sender;

- (IBAction)editOK:(id)sender;

- (IBAction)newGraph:(id)sender;
- (IBAction)editGraph:(id)sender;
- (IBAction)removeGraph:(id)sender;

- (IBAction)newGroup:(id)sender;
- (IBAction)renameGroup:(id)sender;
- (IBAction)removeGroup:(id)sender;

- (IBAction)newPage:(id)sender;
- (IBAction)renamePage:(id)sender;
- (IBAction)removePage:(id)sender;

@end
