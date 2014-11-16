/* MATableMaintenanceController */

#import <Cocoa/Cocoa.h>

@class MMySQLDispatcher;

@interface MATableMaintenanceController : NSWindowController
{
    IBOutlet NSMatrix *checkMatrix;
    IBOutlet NSButton *continueButton;
    IBOutlet NSMatrix *operationMatrix;
    IBOutlet NSButton *optimizeLocalCheck;
    IBOutlet NSButton *repairLocalCheck;
    IBOutlet NSMatrix *repairMatrix;
    IBOutlet NSTextField *resultDescription;
    IBOutlet NSTextField *resultLabel;
    IBOutlet NSTextView *resultText;
    IBOutlet NSTextView *selectionText;
    IBOutlet NSTabView *tabView;
    IBOutlet NSTextField *resultLabel2;
    IBOutlet NSProgressIndicator *progress;
    
    NSArray *_tables;
    MMySQLDispatcher *_dispatcher;
}

- (void)runWithTables:(NSArray*)tables dispatcher:(MMySQLDispatcher*)dispatcher
            operation:(int)op;

- (IBAction)cancel:(id)sender;
- (IBAction)goNext:(id)sender;
@end
