#import "MQAddBookmarkPanel.h"
#import "MQBookmark.h"

@implementation MQAddBookmarkPanel

- (id)init
{
  self= [super initWithWindowNibName:@"BookmarkAdd" owner:self];
  [self loadWindow];
  return self;
}

- (IBAction)add:(id)sender
{
  [NSApp endSheet:[self window] returnCode:1];
}

- (IBAction)cancel:(id)sender
{
  [NSApp endSheet:[self window] returnCode:0];
}

- (void)sheetDidEnd:(NSWindow *)sheet
         returnCode:(int)returnCode
        contextInfo:(void *)contextInfo
{
  [[self window] orderOut:nil];
  [NSApp stopModalWithCode:returnCode];
}

- (void)showSheetForWindow:(NSWindow*)window
                 bookmarks:(MQBookmarkList*)bookmarks
                   catalog:(NSString*)catalog
                    schema:(NSString*)schema
                     query:(NSString*)query
{
  unsigned int i, c;
  NSArray *items= [bookmarks getFolderList];
  [folder removeAllItems];
  
  c= [items count];
  for (i= 0; i < c; i++)
  {
    id item= [items objectAtIndex:i];
    id mitem;

    [folder addItemWithTitle:[item caption]];
    mitem= [folder lastItem];
    [mitem setRepresentedObject:item];
  }

  [NSApp beginSheet:[self window]
     modalForWindow:window
      modalDelegate:self
     didEndSelector:@selector(sheetDidEnd:returnCode:contextInfo:)
        contextInfo:NULL];

  if ([NSApp runModalForWindow:[self window]] == 1)
  {
    [bookmarks addBookmark:[name stringValue]
                  forQuery:query
                forCatalog:catalog
                    schema:schema
                  inFolder:[[folder selectedItem] representedObject]];
  }
}

@end
