//
//  NSView_extras.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Tue Aug 17 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "NSView_extras.h"
#import <AppKit/NSTabView.h>

@implementation NSView(NSView_MExtras)

- (void)setEnabledRecursive:(BOOL)flag
{
  [self setEnabledRecursive:flag exceptTagsInRange:NSMakeRange(0, 0)];
}


- (void)setEnabledRecursive:(BOOL)flag exceptTagsInRange:(NSRange)range
{
  NSArray *children= [self subviews];
  unsigned int i, c= [children count];
  
  if (range.length && NSLocationInRange([self tag], range))
    return;

  for (i= 0; i < c; i++)
  {
    id sv= [children objectAtIndex:i];
    [sv setEnabledRecursive:flag exceptTagsInRange:range];
  }
  if ([self isKindOfClass:[NSTabView class]])
  {
    NSTabView *tview= (NSTabView*)self;
    c= [tview numberOfTabViewItems];
    for (i= 0; i < c; i++)
    {
      id page= [tview tabViewItemAtIndex:i];
      [[page view] setEnabledRecursive:flag exceptTagsInRange:range];
    }
  }
  else if ([self respondsToSelector:@selector(setEnabled:)])
    [(id)self setEnabled:flag];
  else if ([self respondsToSelector:@selector(setEditable:)])
    [(id)self setEditable:flag];
}


- (void)setChildrenEnabled:(BOOL)flag exceptTagsInRange:(NSRange)range
{
  NSArray *children= [self subviews];
  unsigned int i, c= [children count];
  
  if (range.length && NSLocationInRange([self tag], range))
    return;
  
  for (i= 0; i < c; i++)
  {
    id sv= [children objectAtIndex:i];
    [sv setEnabledRecursive:flag exceptTagsInRange:range];
  }
  if ([self isKindOfClass:[NSTabView class]])
  {
    NSTabView *tview= (NSTabView*)self;
    c= [tview numberOfTabViewItems];
    for (i= 0; i < c; i++)
    {
      id page= [tview tabViewItemAtIndex:i];
      [[page view] setEnabledRecursive:flag exceptTagsInRange:range];
    }
  }
}

@end
