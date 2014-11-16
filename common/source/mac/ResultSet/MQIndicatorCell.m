//
//  MQIndicatorCell.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/9/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQIndicatorCell.h"

@implementation MQIndicatorCell

- (id)init
{
  self= [super init];
  if (self)
  {
    _attribs= [[NSDictionary dictionaryWithObjectsAndKeys:[NSFont systemFontOfSize:11], NSFontAttributeName, 
      nil] retain];
    [_attribs retain];
    
    _arrow= [[NSString stringWithUTF8String:"\xe2\x96\xb6"] retain];
  }
  return self;
}


- (void)dealloc
{
  [_arrow release];
  [_attribs release];
  [super dealloc];
}


- (id)copyWithZone:(NSZone*)zone 
{
  MQIndicatorCell *copy = (MQIndicatorCell*)[super copyWithZone:zone];
  copy->_attribs = [_attribs retain];
  copy->_arrow= [_arrow retain];
  return copy;
}


- (void)editWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject event:(NSEvent *)theEvent
{
}

- (void)selectWithFrame:(NSRect)aRect inView:(NSView *)controlView editor:(NSText *)textObj delegate:(id)anObject start:(int)selStart length:(int)selLength 
{
}

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView *)controlView 
{
  if (_placeholder)
    [[NSColor lightGrayColor] set];
  else
    [[NSColor grayColor] set];
  NSRectFill(cellFrame);
  
  if (_selected)
    [_arrow drawAtPoint:NSMakePoint(cellFrame.origin.x+cellFrame.size.width-14,cellFrame.origin.y+1)
         withAttributes:_attribs];
}

- (void)setSelected:(BOOL)flag
{
  _selected= flag;
}

- (void)setPlaceholder:(BOOL)flag
{
  _placeholder= flag;
}

@end
