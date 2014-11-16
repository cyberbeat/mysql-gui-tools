/* TestApplication */

#import <Cocoa/Cocoa.h>

@class MConnectionPanel;

@interface TestApplication : NSApplication
{
  IBOutlet NSWindow *testWindow;
  
  MConnectionPanel *_cpanel;
}
- (IBAction)showConnectionPanel:(id)sender;
- (IBAction)showBoxTest:(id)sender;
- (IBAction)showButtonTest:(id)sender;
@end
