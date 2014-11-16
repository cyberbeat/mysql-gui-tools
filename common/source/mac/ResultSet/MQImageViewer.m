//
//  MQImageViewer.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQImageViewer.h"
#import <MySQLToolsCommon/mxUtils.h>

@interface MQDraggableImageView : NSImageView
{
}
@end

@implementation MQDraggableImageView
- (void)mouseDown:(NSEvent *)theEvent
{
  NSSize dragOffset = NSMakeSize(0.0, 0.0);
  NSPasteboard *pboard;
  NSRect rect= [self frame];
  NSSize imageSize= [[self image] size];
  NSRect imageRect;
  NSPoint pt;

  pt= NSMakePoint(NSMidX(rect),NSMidY(rect));
  imageRect.size= imageSize;
  pt.x-= imageSize.width/2;
  pt.y-= imageSize.height/2;
  imageRect.origin= pt;
  if (NSPointInRect([self convertPoint:[theEvent locationInWindow] fromView:nil],imageRect))
  {
    pboard = [NSPasteboard pasteboardWithName:NSDragPboard];
    [pboard declareTypes:[NSArray arrayWithObject:NSTIFFPboardType] owner:self];
    [pboard setData:[[self image] TIFFRepresentation] forType:NSTIFFPboardType];
    [self dragImage:[self image] at:pt offset:dragOffset 
              event:theEvent pasteboard:pboard source:self slideBack:YES];
  }
}
@end


@implementation MQImageViewer

+ (BOOL)canDisplayData:(NSData*)data
{
  if ([NSImageRep imageRepClassForData:data])
    return YES;
  return NO;
}


- (id)initWithData:(NSData*)data
{
  self= [super init];
  if (self)
  {
    NSRect frame;
    _image= [[NSImage alloc] initWithData:data];
    frame= MXRectWithSize([_image size]);
    _imageView= [[MQDraggableImageView alloc] initWithFrame:frame];
    frame.size.width+= 2.0;
    frame.size.height+= 2.0;
    frame.size= [NSScrollView frameSizeForContentSize:frame.size
                                hasHorizontalScroller:YES
                                  hasVerticalScroller:YES
                                           borderType:NSGrooveBorder];
    _scroll= [[NSScrollView alloc] initWithFrame:frame];
    [_scroll setDocumentView:_imageView];
    [_scroll setHasVerticalScroller:YES];
    [_scroll setHasHorizontalScroller:YES];
    [_scroll setBorderType:NSGrooveBorder];
    [_scroll setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [_imageView setImage:_image];
    [_imageView setImageAlignment:NSImageAlignCenter];
    [_imageView setImageFrameStyle:NSImageFrameNone];
    [_imageView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [_imageView registerForDraggedTypes:
      [NSArray arrayWithObject:NSTIFFPboardType]];
  }
  return self;
}

- (void)setData:(NSData*)data
{
  NSImage *image= [[NSImage alloc] initWithData:data];
  NSRect frame= MXRectWithSize([image size]);
  
  [_imageView setFrame:frame];
  [_imageView setImage:image];
  [image release];
}

- (NSString*)label
{
  return @"Image";
}


- (NSView*)view
{
  return _scroll;
}


- (void)setEditable:(BOOL)flag
{
#ifdef MAC_OS_X_VERSION_10_4
  [_imageView setAllowsCutCopyPaste:flag];
#endif
}


- (void)dealloc
{
  [_image release];
  [_imageView release];
  [_scroll release];
  [super dealloc];
}

@end
