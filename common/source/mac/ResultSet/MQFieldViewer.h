/* MQFieldViewer */

#import <Cocoa/Cocoa.h>

@interface MQFieldViewer : NSWindowController
{
    IBOutlet NSPopUpButton *formatPop;
    IBOutlet NSTextField *infoText;
    IBOutlet NSTextField *columnText;
    IBOutlet NSView *contentView;
    IBOutlet id delegate;
    
    NSData *_data;
    NSDictionary *_info;
  
    id _currentViewer;
    NSMutableArray *_viewers;
    BOOL _editing;
}
- (IBAction)cancel:(id)sender;
- (IBAction)changeView:(id)sender;
- (IBAction)load:(id)sender;
- (IBAction)ok:(id)sender;
- (IBAction)save:(id)sender;

- (void)setDelegate:(id)delegate;

- (void)setInfo:(NSDictionary*)info;
- (NSDictionary*)info;
- (void)showData:(NSData*)data editable:(BOOL)editable;
- (void)showTextData:(NSData*)data editable:(BOOL)editable;
@end
