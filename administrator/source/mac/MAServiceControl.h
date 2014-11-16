/* MAServiceControl */

#import <Cocoa/Cocoa.h>
#include <Security/Authorization.h>
#import "MAPanel.h"

@interface MAServiceControl : MAPanel
{
    IBOutlet NSTextView *messageText;
    IBOutlet NSTextField *statusLabel;
    IBOutlet NSButton *toggleButton;
    IBOutlet NSImageView *statusImage;
    IBOutlet NSProgressIndicator *progress;
    IBOutlet NSButton *startOnBootCheck;
	
	AuthorizationRef _authRef;
}
- (IBAction)toggleServer:(id)sender;
- (IBAction)toggleServerStartup:(id)sender;
@end
