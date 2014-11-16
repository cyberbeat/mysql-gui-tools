/* MAServerInformation */

#import "MAPanel.h"

@interface MAServerInformation : MAPanel {
    IBOutlet NSTextField *clientInfoLabel;
    IBOutlet NSTextField *instanceInfoCaption;
    IBOutlet NSTextField *instanceInfoLabel;
    IBOutlet NSTextField *serverInfoLabel;
    IBOutlet NSImageView *statusImage;
    IBOutlet NSTextField *statusLabel;
}


- (void)didShow;

- (void)updateStatus;

@end
