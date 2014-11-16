//
//  MAccessoryScrollView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 9/3/04.
//  Copyright 2004 MySQL AB. All rights reserved.
//

#import "MAccessoryScrollView.h"


@implementation MAccessoryScrollView


- (void)dealloc
{
  [_vAccessoryViews release];
  [_hAccessoryViews release];
  [super dealloc];
}

- (void)addVerticalAccessoryView:(NSView*)view
{
  if (!_vAccessoryViews)
    _vAccessoryViews= [[NSMutableArray alloc] init];
  
  [_vAccessoryViews addObject:view];
  [self tile];
}

- (void)addHorizontalAccessoryView:(NSView*)view
{
  if (!_hAccessoryViews)
    _hAccessoryViews= [[NSMutableArray alloc] init];
  
  [_hAccessoryViews addObject:view];
  [self tile];
}

- (void)tile
{
  int i;
  NSRect rect;
  NSScroller *hscroller= [self horizontalScroller];
  NSScroller *vscroller= [self verticalScroller];
  float x, y;

  // 1st do the default arrangement 
  [super tile];
  
  // now arrange our accessory views
  
  rect= [vscroller frame];
  y= rect.origin.y;
  for (i= 0; i < [_vAccessoryViews count]; i++)
  {
    NSView *view= [_vAccessoryViews objectAtIndex:i];
    float h= NSHeight([view frame]);
    [view setFrame:NSMakeRect(NSMinX(rect), y, NSWidth(rect), h)];
    [view setNeedsDisplay:YES];
    y+= h;
  }
  rect.size.height-= (y - rect.origin.y);
  rect.origin.y= y;
  [vscroller setFrame:rect];
  [vscroller setNeedsDisplay];
  
  
  rect= [hscroller frame];
  x= rect.origin.x;
  for (i= 0; i < [_hAccessoryViews count]; i++)
  {
    NSView *view= [_hAccessoryViews objectAtIndex:i];
    float w= NSWidth([view frame]);
    [view setFrame:NSMakeRect(x, NSMinY(rect), w, NSHeight(rect))];
    [view setNeedsDisplay:YES];
    x+= w;
  }
  rect.size.width-= (x - rect.origin.x);
  rect.origin.x= x;
  [hscroller setFrame:rect];
  [hscroller setNeedsDisplay];
}

- (void)removeAccessoryViews
{
  [_vAccessoryViews makeObjectsPerformSelector:@selector(removeFromSuperview)];
  [_vAccessoryViews removeAllObjects];
  [_hAccessoryViews makeObjectsPerformSelector:@selector(removeFromSuperview)];
  [_hAccessoryViews removeAllObjects];
}

@end
