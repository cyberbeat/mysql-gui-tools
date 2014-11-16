/* MABackup */

#import <Cocoa/Cocoa.h>
#import "MAPanel.h"
#import <MySQLToolsCommon/MTableView.h>

#include "myx_admin_public_interface.h"

@class MABackupDataSource;

@interface MABackup : MAPanel
{
    IBOutlet NSPopUpButton *connectionPopUp;
    IBOutlet NSButton *contentAddButton;
    IBOutlet NSOutlineView *contentOutline;
    IBOutlet NSButton *contentRemoveButton;
    IBOutlet NSTextField *dailyTimeText;
    IBOutlet NSButton *fullBackupButton;
    IBOutlet NSTextField *monthlyDaysText;
    IBOutlet NSTextField *monthlyTimeText;
    IBOutlet NSTextField *nameText;
    IBOutlet NSTextField *prefixText;
    IBOutlet MTableView *profileTable;
    IBOutlet NSButton *scheduleButton;
    IBOutlet NSPopUpButton *schedulePopUp;
    IBOutlet NSTabView *scheduleTab;
    IBOutlet NSOutlineView *schemaOutline;
    IBOutlet NSSearchField *schemaSearch;
    IBOutlet NSButton *startButton;
    IBOutlet NSTextField *statusText;
    IBOutlet NSTextField *targetText;
    IBOutlet NSPopUpButton *typePopUp;
    IBOutlet NSButton *unscheduleButton;
    IBOutlet NSTextField *weeklyTimeText;
    IBOutlet NSTextField *scheduleDailyTime;
    IBOutlet NSMatrix *scheduleWeekdays;
    IBOutlet NSTextField *scheduleWeeklyTime;

    IBOutlet NSMatrix *modeMatrix;
    IBOutlet NSMatrix *optionMatrix;
    
    IBOutlet NSPanel *progressPanel;
    IBOutlet NSProgressIndicator *backupProgress;
    IBOutlet NSTextField *progressText;
    IBOutlet NSTextField *tableProgressText;
    
    IBOutlet NSTabView *tabView;
    
    NSImage *_tableIcon;
    NSImage *_schemaIcon;
    NSImage *_viewIcon;
    NSImage *_procIcon;
    NSImage *_funcIcon;

    BOOL _backupAborted;
    MYX_BACKUP_PROFILE *_profile;
    MYX_USER_CONNECTIONS *_pconns;

    MABackupDataSource *_selectionDS;
    NSMutableArray *_profiles;
}

- (BOOL) exchangeCronEntry: entry;

- (IBAction)addContent:(id)sender;
- (IBAction)addProfile:(id)sender;
- (IBAction)chooseTargetDirectory:(id)sender;
- (IBAction)importProfile:(id)sender;
- (IBAction)removeContent:(id)sender;
- (IBAction)removeProfile:(id)sender;
- (IBAction)scheduleBackup:(id)sender;
- (IBAction)startBackup:(id)sender;
- (IBAction)unscheduleBackup:(id)sender;
- (IBAction)abortBackup:(id)sender;
- (IBAction)showConnectionEditor:(id)sender;
@end
