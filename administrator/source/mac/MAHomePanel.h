/* MAHomePanel */

#import <Cocoa/Cocoa.h>

#import "MAPanel.h"

@interface MAHomePanel : MAPanel
{
}

+ (NSImage*)icon;
+ (NSString*)label;
+ (NSString*)toolTip;

- (void)build;

@end
