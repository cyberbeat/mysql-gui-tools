/* MQAddBookmarkPanel */

#import <Cocoa/Cocoa.h>

@class MQBookmarkList;

@interface MQAddBookmarkPanel : NSWindowController
{
    IBOutlet NSPopUpButton *folder;
    IBOutlet NSTextField *name;
}
- (IBAction)add:(id)sender;
- (IBAction)cancel:(id)sender;

- (void)showSheetForWindow:(NSWindow*)window
                 bookmarks:(MQBookmarkList*)bookmarks
                   catalog:(NSString*)catalog
                    schema:(NSString*)schema
                     query:(NSString*)query;
@end
