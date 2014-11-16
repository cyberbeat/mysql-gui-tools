/* MQSPEditor */

#import <Cocoa/Cocoa.h>

#include <myx_public_interface.h>

@class MSourceTextController;

@interface MQSPEditor : NSObject
{
    IBOutlet NSPopUpButton *returnType;
    IBOutlet NSTextField *spName;
    IBOutlet NSTableView *spParameters;
    IBOutlet NSProgressIndicator *spProgress;
    IBOutlet NSTextField *spResult;
    IBOutlet NSTextView *spText;
    IBOutlet NSPopUpButton *spType;
    IBOutlet NSView *spView;
}
- (IBAction)testSP:(id)sender;
- (void)editSP:(MYX_DBM_STORED_PROCEDURE_DATA*)spdata;
@end
