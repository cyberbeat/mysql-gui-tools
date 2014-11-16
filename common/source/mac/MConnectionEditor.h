/* MConnectionEditor */

#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"

@interface MConnectionEditor : NSObject
{
@public
  IBOutlet NSView *topView;
@protected  
  IBOutlet NSTextField *hostname;
  IBOutlet NSOutlineView *list;
  IBOutlet NSTextField *name;
  IBOutlet NSTextView *notes;
  IBOutlet NSTextField *password;
  IBOutlet NSTextField *port;
  IBOutlet NSTextField *schema;
  IBOutlet NSTextField *socket;
  IBOutlet NSTabView *tabview;
  IBOutlet NSTextField *username;
  IBOutlet NSButton *compress;

  
  NSImage *connIcon, *groupIcon;
  
  NSString *filename;

  NSMutableArray *indices;
  
  MYX_USER_CONNECTIONS *connections;
}

- (id)initForFile:(NSString*)file;

- (IBAction)addConnection:(id)sender;
- (IBAction)removeConnection:(id)sender;
- (IBAction)toggleOption:(id)sender;


- (void)load:(id)sender;
- (void)save:(id)sender;

@end
