//
//  MCanvasScrollView.mm
//  GenericCanvas
//
//  Created by Alfredo Kojima on 05/6/17.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MCanvasScrollView.h"
#import "MGenericCanvasView.h"

@implementation MCanvasScrollView

- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    _hScroller= [[NSScroller alloc] initWithFrame:NSMakeRect(0, 0, NSWidth(frame)-[NSScroller scrollerWidth], [NSScroller scrollerWidth])];
    _vScroller= [[NSScroller alloc] initWithFrame:NSMakeRect(NSMaxX(frame) - [NSScroller scrollerWidth], [NSScroller scrollerWidth],
                                                             [NSScroller scrollerWidth], NSHeight(frame))];
    [self addSubview:_hScroller];
    [self addSubview:_vScroller];
    
    [_hScroller setAction:@selector(scrolled:)];
    [_hScroller setTarget:self];
    [_vScroller setAction:@selector(scrolled:)];
    [_vScroller setTarget:self];
    
    [_hScroller setEnabled:YES];
    [_vScroller setEnabled:YES];
  }
  return self;
}


- (void)dealloc
{
  [_hScroller release];
  [_vScroller release];
  [super dealloc];
}


- (void)tile
{
  NSRect frame= [self frame];
  NSRect contentFrame;

  contentFrame= NSMakeRect(0, [NSScroller scrollerWidth], 
                           NSWidth(frame) - [NSScroller scrollerWidth],
                           NSHeight(frame) - [NSScroller scrollerWidth]); 
  [_contentView setFrame:contentFrame];
  if (_hAccessoryView)
  {
    NSRect accRect= [_hAccessoryView frame];
    [_hAccessoryView setFrame:NSMakeRect(0, 0, NSWidth(accRect), [NSScroller scrollerWidth])];
    [_hScroller setFrame:NSMakeRect(NSWidth(accRect), 0, NSWidth(frame) - NSWidth(accRect) - [NSScroller scrollerWidth], [NSScroller scrollerWidth])];
  }
  else
    [_hScroller setFrame:NSMakeRect(0, 0, NSWidth(frame) - [NSScroller scrollerWidth], [NSScroller scrollerWidth])];
  [_vScroller setFrame:NSMakeRect(NSMaxX(frame) - [NSScroller scrollerWidth], [NSScroller scrollerWidth],
                                  [NSScroller scrollerWidth], NSHeight(frame) - [NSScroller scrollerWidth])];
}


- (void)scrolled:(id)sender
{
  float line;
  float page;
  NSRect rect= [_contentView actualVisibleRect];
  NSSize actualSize= [_contentView actualSize];
  
  if (sender == _hScroller)
  {
    line = (NSWidth(rect)/20) / actualSize.width;
    page = NSWidth(rect) / actualSize.width;
  }
  else
  {
    line = (NSHeight(rect)/20) / actualSize.height;
    page = NSHeight(rect) / actualSize.height;    
  }
  switch ([sender hitPart])
  {
    case NSScrollerDecrementPage:
      [sender setFloatValue:MAX([sender floatValue] - page, 0.0)];
      break;
    case NSScrollerIncrementPage:
      [sender setFloatValue:MIN([sender floatValue] + page, 1.0)];
      break;      
    case NSScrollerDecrementLine:
      [sender setFloatValue:MAX([sender floatValue] - line, 0.0)];
      break;
    case NSScrollerIncrementLine:
      [sender setFloatValue:MIN([sender floatValue] + line, 1.0)];
      break;
  }
  [_contentView performSelector:@selector(updateFromScrollers)];
}


- (void)reflectContentRect
{
  NSSize size= [_contentView baseSize];
  NSRect visible= [_contentView actualVisibleRect];
  
  if (NSWidth(visible) >= size.width)
    [_hScroller setFloatValue:0.0
               knobProportion:1.0];
  else
    [_hScroller setFloatValue:visible.origin.x/(size.width-NSWidth(visible))
               knobProportion:NSWidth(visible)/size.width];
  if (NSHeight(visible) >= size.height)
    [_vScroller setFloatValue:0.0
               knobProportion:1.0];
  else
    [_vScroller setFloatValue:visible.origin.y/(size.height-NSHeight(visible))
               knobProportion:NSHeight(visible)/size.height];
}


- (void)setContentCanvas:(MGenericCanvasView*)canvas
{
  _contentView= canvas;

  [self addSubview:canvas];
  [self tile];
}

- (NSSize)contentSize
{
  NSSize size= [self frame].size;
  
  size.width-= [NSScroller scrollerWidth];
  size.height-= [NSScroller scrollerWidth];
  
  return size;
}

- (void)resizeSubviewsWithOldSize:(NSSize)oldBoundsSize
{
  [self tile];
}


- (void)setHAccessory:(NSView*)view
{
  [self addSubview:view];
  _hAccessoryView= view;
  [self tile];
}


- (NSScroller*)verticalScroller
{
  return _vScroller;
}

- (NSScroller*)horizontalScroller
{
  return _hScroller;
}

@end
