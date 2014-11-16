/* MARestore */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"
#include "myx_admin_public_interface.h"

@class MARestoreDataSource;

@interface MARestore : MAPanel
{
    IBOutlet NSButton *ignoreErrorCheck;
    IBOutlet NSTextField *informationText;
    IBOutlet NSButton *createCheck;
    IBOutlet NSComboBox *restoreTargetCombo;
    IBOutlet NSProgressIndicator *parseProgress;
    IBOutlet NSOutlineView *selectionOutline;
    IBOutlet NSButton *restoreButton;
    
    IBOutlet NSView *accessoryView;
    IBOutlet NSPopUpButton *charsetPopUp;
    
    IBOutlet NSPanel *progressPanel;
    IBOutlet NSProgressIndicator *restoreProgress;
    IBOutlet NSTextField *progressText;
    IBOutlet NSTextView *warningText;
	  
    BOOL _abortRestore;

    NSImage *_tableIcon;
    NSImage *_schemaIcon;
    NSImage *_viewIcon;

    MARestoreDataSource *_selectionDS;
	
    NSString *_path;
    NSString *_charset;
    MYX_BACKUP_CONTENT *_content;
}
- (IBAction)startRestore:(id)sender;
- (IBAction)chooseFile:(id)sender;

- (IBAction)abortRestore:(id)sender;
//- (IBAction)detectCharset:(id)sender;
@end
