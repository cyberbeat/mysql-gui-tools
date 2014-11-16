//
//  MAMenuContextView.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 2/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MAMenuContextView.h"


@implementation MAMenuContextView


- (NSMenu *)menuForEvent:(NSEvent *)theEvent
{
  int i;
  NSMenu *menu= [self menu];

  [self retain];
  
  for (i= 0; i < [menu numberOfItems]; i++)
  {
    if (![[menu itemAtIndex:i] isSeparatorItem])
      [[menu itemAtIndex:i] setRepresentedObject:self];
  }
  
  [self release];

  return menu;
}

- (id)attachedObject
{
  return _attachedObject;
}


- (void)setAttachedObject:(id)obj
{
  _attachedObject= obj;
}

- (void)drawRect:(NSRect)rect
{
  [super drawRect:rect];
  if (_highlighted)
  {
    [[[NSColor whiteColor] colorWithAlphaComponent:0.5] set];
    NSRectFill(rect);
  }
}


- (void)setHighlighted:(BOOL)flag
{
  _highlighted= flag;
  [self setNeedsDisplay:YES];
}

@end
