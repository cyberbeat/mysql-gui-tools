//
//  MQDropActionView.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/9.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQDropActionView.h"
#import "MQActionBar.h"
#import <MySQLToolsCommon/MSchemaDataSource.h>

@implementation MQDropActionView


- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    [self registerForDraggedTypes:[NSArray arrayWithObject:MSchemaItemPboardType]];
  }
  return self;
}

- (void)drawRect:(NSRect)rect 
{
  NSPoint pos;
  
  [[NSColor whiteColor] set];
  NSRectFill(rect);
  
  rect.origin.x+= 1;
  rect.size.width-= 2;
  
  if (_highlight)
  {
    rect= NSInsetRect(rect, 1, 1);
    [[NSColor alternateSelectedControlColor] set];
    [NSBezierPath setDefaultLineWidth:2.0];
    [NSBezierPath strokeRect:rect];
  }
  else
  {
    [[NSColor blackColor] set];
    [NSBezierPath setDefaultLineWidth:1.0];
    [NSBezierPath strokeRect:rect];
  }
  
  pos.x= (NSWidth(rect) - [_font widthOfString:_label]) / 2;
  pos.y= [_font descender] + (NSHeight(rect) - ([_font ascender] + [_font descender])) / 2;
  
  [_label drawAtPoint:pos
       withAttributes:[NSDictionary dictionaryWithObject:_font
                                                 forKey:NSFontAttributeName]];
}

- (void)setText:(NSString*)text
{
  if (_label != text)
  {
    [_label release];
    _label= [text retain];
  }
}

- (void)setFont:(NSFont*)font
{
  _font= [font retain];
}

- (void)setTarget:(id)target
{
  _target= target;
}

- (void)setDropAction:(SEL)action
{
  _action= action;
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{ 
  id source= [[sender draggingSource] dataSource];
  if ([source isKindOfClass:[MSchemaDataSource class]])
  {
    MSchemaItem *item= [source draggedItem];
    
    if (item && [item type] == MTableItemType)
    {
      _highlight= YES;
      [(id)[self superview] show];
      [self setNeedsDisplay:YES];
      //[_target performSelector:_action withObject:self withObject:item];
      return NSDragOperationCopy;
    }
  }
  return NSDragOperationNone;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
  _highlight= NO;
  [(id)[self superview] delayedHide];
  [self setNeedsDisplay:YES];
//  [_target performSelector:_action
//                withObject:self
//                withObject:nil];
}

- (void)draggingEnd:(id <NSDraggingInfo>)sender
{
  _highlight= NO;
  [(id)[self superview] delayedHide];
  [self setNeedsDisplay:YES];
//  [_target performSelector:_action
//                withObject:self
//                withObject:nil]; 
}

- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
  id source= [[sender draggingSource] dataSource];
  
  _highlight= NO;
  [self setNeedsDisplay:YES];
  
  if ([source isKindOfClass:[MSchemaDataSource class]])
  {
    MSchemaItem *item= [source draggedItem];
    
    if (item && [item type] == MTableItemType)
    {      
      [(id)[self superview] hide];      
      [_target performSelector:_action withObject:self withObject:item];
      return YES;
    }
  }
  return NO;
}


@end
