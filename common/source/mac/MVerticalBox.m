//
//  MVerticalBox.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 05/9/12.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MVerticalBox.h"


@implementation MVerticalBox


- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    _xOffset= 16.0;
    _yOffset= 10.0;
    _spacing= 0;
  }
  return self;
}


- (void)setSpacing:(float)spacing
{
  _spacing= spacing;
}


- (void)setExpandsHorizontally:(BOOL)flag
{
  _expandHorizontally= flag;
}


- (BOOL)isFlipped
{
  return YES;
}

- (void)dealloc 
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}


- (void)setOffset:(NSSize)offset
{
  _xOffset= offset.width;
  _yOffset= offset.height;
}

- (void)_subviewResized:(NSNotification*)notif
{
  NSArray *subviews= [self subviews];
  int i;
  float y= _yOffset;
  NSSize size;
  
  for (i= 0; i < [subviews count]; i++)
  {
    [[subviews objectAtIndex:i] setFrameOrigin:NSMakePoint(_xOffset, y)];
    y+= NSHeight([[subviews objectAtIndex:i] frame]) + _spacing;
  }

  size.width= NSWidth([self frame]);
  
  if (_expandHorizontally)
    size.width= NSWidth([[self superview] frame]);
  size.height= y;

  [self setFrameSize:size];
  [self setNeedsDisplay:YES];
}



- (void)resizeSubviewsWithOldSize:(NSSize)oldBoundsSize
{
  NSArray *subviews= [self subviews];
  int i;
  float y= _yOffset;
  float width= NSWidth([self frame]);
  
  for (i= 0; i < [subviews count]; i++)
  {
    NSRect frame;
    frame= [[subviews objectAtIndex:i] frame];
    
    frame.origin.y= y;
    frame.origin.x= _xOffset;
    frame.size.width= width - 2 * _xOffset;
    
    [[subviews objectAtIndex:i] setFrame:frame];
    
    y+= NSHeight(frame) + _spacing;
  }
  [self setNeedsDisplay:YES];  
}


- (void)willRemoveSubview:(NSView*)view
{
  NSArray *subviews= [self subviews];
  int i;
  float y= _yOffset;
  float width= NSWidth([self frame]);

  for (i= 0; i < [subviews count]; i++)
  {
    NSRect frame;
    
    if ([subviews objectAtIndex:i] == view)
      continue;
    
    frame= [[subviews objectAtIndex:i] frame];
    frame.origin.y= y;
    frame.origin.x= _xOffset;
    frame.size.width= width - 2 * _xOffset;
    
    [[subviews objectAtIndex:i] setFrame:frame];
    
    y+= NSHeight(frame) + _spacing;
  }
  [self setNeedsDisplay:YES];  
}


- (void)didAddSubview:(NSView*)view
{
  NSRect rect= [view frame];
  rect.size.width= NSWidth([self frame]);
  [view setFrame:rect];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(_subviewResized:)
                                               name:NSViewFrameDidChangeNotification
                                             object:view];
  
  [self resizeSubviewsWithOldSize:NSMakeSize(0,0)];
}

@end
