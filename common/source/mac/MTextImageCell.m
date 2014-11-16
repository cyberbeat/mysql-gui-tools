//
//  MTextImageCell.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Wed Jul 07 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MTextImageCell.h"

#define IMAGE_OFFSET 2

@implementation MTextImageCell

- (void)dealloc 
{
  [_image release];
  [super dealloc];
}

- copyWithZone:(NSZone*)zone 
{
  MTextImageCell *copy = (MTextImageCell*)[super copyWithZone:zone];
  copy->_image = [_image retain];
  return copy;
}

- (void)setImage:(NSImage*)image 
{
  if (image != _image) 
  {
    [_image release];
    _image = [image retain];
  }
}

- (NSImage*)image 
{
  return _image;
}


- (void)editWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject event:(NSEvent *)theEvent 
{
  NSRect textFrame, imageFrame;
  NSDivideRect(aRect, &imageFrame, &textFrame, IMAGE_OFFSET + [_image size].width, NSMinXEdge);
  [super editWithFrame: textFrame inView: controlView editor:textObj delegate:anObject event: theEvent];
}

- (void)selectWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject start:(int)selStart length:(int)selLength 
{
  NSRect textFrame, imageFrame;
  NSDivideRect(aRect, &imageFrame, &textFrame, IMAGE_OFFSET + [_image size].width, NSMinXEdge);
  [super selectWithFrame: textFrame inView: controlView editor:textObj delegate:anObject start:selStart length:selLength];
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView *)controlView 
{
  if (_image != nil) 
  {
    NSSize imageSize;
    NSRect imageFrame;
    
    imageSize = [_image size];
    NSDivideRect(cellFrame, &imageFrame, &cellFrame, IMAGE_OFFSET + imageSize.width, NSMinXEdge);
    if ([self drawsBackground]) 
    {
      [[self backgroundColor] set];
      NSRectFill(imageFrame);
    }
    imageFrame.origin.x += IMAGE_OFFSET;
    if ([controlView isFlipped])
      imageFrame.origin.y += ceil((cellFrame.size.height + imageSize.height) / 2);
    else
      imageFrame.origin.y += ceil((cellFrame.size.height - imageSize.height) / 2);
    [_image compositeToPoint:imageFrame.origin operation:NSCompositeSourceOver];
  }
  [super drawWithFrame:cellFrame inView:controlView];
}

- (NSSize)cellSize 
{
  NSSize cellSize= [super cellSize];
  cellSize.width += (_image ? [_image size].width : 0) + IMAGE_OFFSET;
  return cellSize;
}

@end
