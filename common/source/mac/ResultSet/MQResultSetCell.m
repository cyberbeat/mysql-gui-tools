//
//  MQResultSetCell.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/13/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQResultSetCell.h"
#import <MySQLToolsCommon/mxUtils.h>

@implementation MQResultSetCell

- (id)init
{
  self= [super init];
  if (self)
  {
    _blobIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]],@"blob_icon.png") retain];
    _nullIcon= [MXGetImageFromBundle([NSBundle bundleForClass:[self class]],@"field_overlay_null.png") retain];
  }
  return self;
}


- (id)copyWithZone:(NSZone*)zone 
{
  MQResultSetCell *copy = (MQResultSetCell*)[super copyWithZone:zone];
  copy->_blobIcon = [_blobIcon retain];
  copy->_nullIcon = [_nullIcon retain];
  return copy;
}


- (void)dealloc
{
  [_blobIcon release];
  [_nullIcon release];
  [super dealloc];
}


- (void)editWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject event:(NSEvent *)theEvent
{
  if (!_blob)
  {
    aRect.size.height-= 3.0;
    aRect.size.width-= 3.0;
    [super editWithFrame:aRect inView:controlView editor:textObj delegate:anObject event:theEvent];
  }
}

- (void)selectWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject start:(int)selStart length:(int)selLength 
{
  [self setBackgroundColor:[NSColor whiteColor]];
  [self setTextColor:[NSColor blackColor]];
  aRect.size.height-= 3.0;
  aRect.size.width-= 3.0;
  [super selectWithFrame:aRect inView:controlView editor:textObj delegate:anObject start:selStart length:selLength];
}

- (void)setPlaceholder:(BOOL)flag
{
  _placeholder= flag;
}

- (void)setIsBlob:(BOOL)flag
{
  _blob= flag;
}

- (void)setIsNull:(BOOL)flag
{
  _null= flag;
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView *)controlView 
{
  NSPoint point= cellFrame.origin;
  point.x+= 4;

  [super drawWithFrame:cellFrame inView:controlView];

  if (!_placeholder)
  {
    if (_blob)
    {
      if ([self objectValue])
      {
        point.y+= [_blobIcon size].height + (NSHeight(cellFrame)-[_blobIcon size].height)/2;
        [_blobIcon compositeToPoint:point operation:NSCompositeSourceOver];
      }
    }
    if (_null)
    {
      point.y+= [_nullIcon size].height + (NSHeight(cellFrame)-[_nullIcon size].height)/2;
      [_nullIcon compositeToPoint:point operation:NSCompositeSourceOver];
    }
  }
}

@end
