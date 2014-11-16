//
//  MTabView.m
//
//  Created by Alfredo Kojima on 2/16/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MTabView.h"
#include "mxUtils.h"

#define MARGIN 0
#define MAX_TAB_WIDTH 150
#define MIN_TAB_WIDTH 50
#define TAB_HEIGHT 21
#define TAB_MARGIN 20

@interface MTabViewItem(Private)
- (MTabViewItemState)closeState;
- (void)setCloseState:(MTabViewItemState)state;
@end


@implementation MTabViewItem(Private)

- (MTabViewItemState)closeState
{
  return _closeState;
}

- (void)setCloseState:(MTabViewItemState)state
{
  _closeState= state;
}

@end


@implementation MTabViewItem

- (id)initWithIdentifier:(NSString*)identifier
{
  self= [super init];
  if (self)
  {
    _identifier= [identifier retain];
  }
  return self;
}

- (void)dealloc
{
  [_icon release];
  [view release];
  [_label release];
  [_identifier release];
  
  [super dealloc];
}

- (void)setIcon:(NSImage*)icon
{
  if (_icon != icon)
  {
    [_icon release];
    _icon= [icon retain];
  }
}

- (NSImage*)icon
{
  return _icon;
}


- (NSString*)label
{
  return _label;
}

- (void)setLabel:(NSString*)label
{
  if (label != _label)
  {
    [_label release];
    _label= [label retain];
  }
}

- (NSString*)labelTextWithWidth:(float)width
{
  NSFont *font= [NSFont boldSystemFontOfSize:[NSFont smallSystemFontSize]];
  
  if ([font widthOfString:_label] < width)
    return _label;
  else
  {//XXX
    return _label;
  }
}

- (MTabViewItemState)tabState
{
  return _state;
}

- (void)setTabState:(MTabViewItemState)state
{
  _state= state;
}

- (NSView*)view
{
  return view;
}

- (void)setView:(NSView*)aView
{
  if (view != aView)
  {
    [view release];
    view= [aView retain];
  }
}

@end


@implementation MTabView

- (id)initWithFrame:(NSRect)frame 
{
  self = [super initWithFrame:frame];
  if (self) 
	{
	  _pages= [[NSMutableArray alloc] init];
	  
	  _backColor[0]= [[NSColor colorWithDeviceRed:0.8 green:0.8 blue:0.8 alpha:0.85] retain];
	  _backColor[1]= [[NSColor colorWithDeviceRed:1.0 green:1.0 blue:1.0 alpha:0.90] retain];
	  _backColor[2]= [_backColor[1] retain];
		  
	  _lborderColor= [[NSColor colorWithDeviceRed:193.0/255 green:194.0/255 blue:194.0/255 alpha:1.0] retain];
	  _borderColor= [[NSColor colorWithDeviceRed:81.0/255 green:81.0/255 blue:81.0/255 alpha:0.8] retain];
    
	  _tabAttrs= [[NSMutableDictionary dictionaryWithObjectsAndKeys:
      [NSColor blackColor], NSForegroundColorAttributeName,
      [NSFont boldSystemFontOfSize:[NSFont smallSystemFontSize]], NSFontAttributeName,
      nil] retain];
	  
	  _closeImage[0]= [MXGetImageFromBundle([NSBundle bundleForClass:[MTabView class]],@"tab_close_normal.png") retain];
	  _closeImage[1]= [MXGetImageFromBundle([NSBundle bundleForClass:[MTabView class]],@"tab_close_over.png") retain];
    _closeImage[2]= [MXGetImageFromBundle([NSBundle bundleForClass:[MTabView class]],@"tab_close_pressed.png") retain];
	  _currentPage= -1;
	  _tabWidth= 100.0;
    _maxTabWidth= MAX_TAB_WIDTH;
    
    _hasBorder= YES;
	  
	  [self recalcTabSizes];
  }
  return self;
}

- (void)dealloc
{
  int i;
  
  if (_trackingRects)
    free(_trackingRects);
  for (i= 0; i < 3; i++)
  {
    [_closeImage[i] release];
    [_backColor[i] release];
  }
  [_tabAttrs release];
  [_lborderColor release];
  [_borderColor release];
  [_pages release];
  [super dealloc];
}

- (BOOL)mouseDownCanMoveWindow
{
  return NO;
}

- (void)mouseDown:(NSEvent *)theEvent
{
  NSPoint pt= [self convertPoint:[theEvent locationInWindow] fromView:nil];
  NSRect tabRect= NSMakeRect(TAB_MARGIN, _tabY, _tabWidth, TAB_HEIGHT);
  NSRect closeRect= NSMakeRect(TAB_MARGIN+_tabWidth-16, _tabY+1.0, 12.0, 13.0);
  int i;

  for (i= 0; i < [_pages count]; i++)
  {
    if ([self mouse:pt inRect:closeRect])
    {
      BOOL done= NO;
      MTabViewItem *item= [_pages objectAtIndex:i];
      
      [item setCloseState:ISPushed];
      [self display];
      
      while (!done)
      {
        BOOL isInside;
        
        theEvent = [[self window] nextEventMatchingMask: NSLeftMouseUpMask |
          NSLeftMouseDraggedMask];
        
        pt= [self convertPoint:[theEvent locationInWindow] fromView:nil];
        isInside= [self mouse:pt inRect:closeRect];
        
        switch ([theEvent type])
        {
          case NSLeftMouseDragged:
            if (isInside)
              [item setCloseState:ISPushed];
            else
              [item setCloseState:ISBack];
            break;
          case NSLeftMouseUp:
            if (isInside)
            {
              [delegate performSelector:@selector(tabView:closeTab:) withObject:self withObject:item];
              return;
            }
            [item setCloseState:ISBack];
            done= YES;
            break;
          default:
            break;
        }
        [self display];
      }
      break;
    }
    else if ([self mouse:pt inRect:tabRect])
    {
      [self selectPage:i];
      break;
    }
    closeRect.origin.x += _tabWidth;
    tabRect.origin.x += _tabWidth;
  }
}

- (NSRect)calcContentFrame
{
  NSRect frame= [self frame];
  
  frame.origin.x= MARGIN;
  if (_tabsOnBottom)
    frame.origin.y= TAB_HEIGHT+MARGIN;
  else
    frame.origin.y= MARGIN;
  frame.size.width-= 2*MARGIN;
  frame.size.height-= TAB_HEIGHT+MARGIN;
  
  return frame;
}


- (void)setCommonContent:(NSView*)content
{
  [self addSubview:_contentView];
  [_contentView setFrame:[self calcContentFrame]];
}


- (void)setHasBorder:(BOOL)flag
{
  _hasBorder= flag;
  [self setNeedsDisplay:YES];
}


- (void)selectPage:(int)page
{
  MTabViewItem *pageItem;
  
  if (_currentPage >= 0)
  {
    if (!_contentView)
      [[[_pages objectAtIndex:_currentPage] view] removeFromSuperview];
    [[_pages objectAtIndex:_currentPage] setTabState:ISBack];
  }
  _currentPage= page;
  
  pageItem= [_pages objectAtIndex:page];
  
  [pageItem setTabState:ISFront];
  if (!_contentView)
  {
    [self addSubview:[pageItem view]];
  //  [[pageItem view] setNextResponder:pageItem];
  
    [[pageItem view] setFrame:[self calcContentFrame]];
  }
  [self setNeedsDisplay:YES];
  
  [delegate performSelector:@selector(tabViewChangedPage:) withObject:self];
}


- (NSSize)contentSize
{
  NSSize size= [self frame].size;
  
  size.width-= 2*MARGIN;
  size.height-= TAB_HEIGHT+MARGIN;
  
  return size;
}


- (MTabViewItem*)selectedPage
{
  return [_pages objectAtIndex: _currentPage];
}

- (unsigned int)numberOfPages
{
  return [_pages count];
}

- (void)selectNextPage
{
  if (_currentPage < [_pages count]-1)
    [self selectPage:_currentPage+1];
}

- (void)selectPreviousPage
{
  if (_currentPage > 0)
    [self selectPage:_currentPage-1];
}

- (void)mouseEntered:(NSEvent *)theEvent
{
  int tab= (int)[theEvent userData];
  
  if (tab&1)
  {
    [[_pages objectAtIndex:tab/2] setCloseState:ISHover];
    [self setNeedsDisplay:YES];
  }
  else if (tab/2 != _currentPage)
  {
    [[_pages objectAtIndex:tab/2] setTabState:ISHover];
    [self setNeedsDisplay:YES];
  }
}

- (void)mouseExited:(NSEvent *)theEvent
{
  int tab= (int)[theEvent userData];  
  
  if (tab&1)
  {
    [[_pages objectAtIndex:tab/2] setCloseState:ISBack];
    [self setNeedsDisplay:YES];
  }
  else if (tab/2 != _currentPage)
  {
    [[_pages objectAtIndex:tab/2] setTabState:ISBack];
    [self setNeedsDisplay:YES];
  }
}

- (void)recalcTabSizes
{
  NSRect rect= [self frame];
  int i;
  
  if (_tabsOnBottom)
    _tabY= 0.0;
  else
    _tabY= rect.size.height - TAB_HEIGHT + 2.0;
  
  for (i= 0; i < _visibleTabCount*2; i++)
    [self removeTrackingRect:_trackingRects[i]];
  
  _tabWidth= ceil((rect.size.width - 2*TAB_MARGIN) / [_pages count]);
  if (_tabWidth > _maxTabWidth)
  {
    _tabWidth= _maxTabWidth;
    _visibleTabCount= [_pages count];
  }
  else if (_tabWidth < MIN_TAB_WIDTH)
  {
    _tabWidth= MIN_TAB_WIDTH;
    _visibleTabCount= (int)((rect.size.width - 2*TAB_MARGIN) / _tabWidth);
  }
  else
    _visibleTabCount= [_pages count];
  
  _trackingRects= (NSTrackingRectTag*)realloc(_trackingRects, sizeof(NSTrackingRectTag)*_visibleTabCount*2);
  
  for (i= 0; i < _visibleTabCount; i++)
  {
    // tracking rect for the tab
    _trackingRects[i*2]= [self addTrackingRect:NSMakeRect(TAB_MARGIN+i*_tabWidth, _tabY, _tabWidth, TAB_HEIGHT)
                                         owner:self
                                      userData:(void*)(i*2)
                                  assumeInside:NO];
    
    // tracking rect for the close button
    _trackingRects[i*2+1]= [self addTrackingRect:NSMakeRect(TAB_MARGIN+(i+1)*_tabWidth-16, _tabY+1.0, 12, 13)
                                           owner:self
                                        userData:(void*)(i*2+1)
                                    assumeInside:NO];
  }
  [self setNeedsDisplay:YES];  
}

- (int)addTabViewItem:(MTabViewItem*)item
{
  [_pages addObject:item];
  
  [self recalcTabSizes];
  [self setNeedsDisplay:YES];
  
  if (_currentPage < 0)
    [self selectPage:0];
  
  return [_pages count]-1;
}

- (void)removeTabViewItem:(MTabViewItem*)item
{
  if (_currentPage == [_pages indexOfObject:item])
    [self selectPage:_currentPage > 0 ? _currentPage-1 : 0];
  
  [_pages removeObject:item];
  if (_currentPage > [_pages count]-1)
    _currentPage--;
  
  [self recalcTabSizes];
  
  [self setNeedsDisplay:YES];  
}

- (void)resizeSubviewsWithOldSize:(NSSize)oldBoundsSize
{
  id page= [_pages objectAtIndex:_currentPage];
  if (_contentView)
    [_contentView setFrame:[self calcContentFrame]];
  else
  {
    [[page view] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
    [[page view] setFrame:[self calcContentFrame]];
  }
  [self recalcTabSizes];
}

- (void)resizeWithOldSuperviewSize:(NSSize)oldBoundsSize
{
  NSRect rect= [self frame];
  
  rect.size.height= TAB_HEIGHT;
  
  [super resizeWithOldSuperviewSize:oldBoundsSize];
  [self recalcTabSizes];
}

- (void)drawTab:(int)index
{
  NSRect rect;
  NSPoint tleft, tright, bleft, bright;
  MTabViewItem *item= [_pages objectAtIndex:index];
  
  rect= NSMakeRect(TAB_MARGIN + index*_tabWidth, _tabY+0.5, _tabWidth, TAB_HEIGHT);
  
  tleft= NSMakePoint(rect.origin.x+0.5, rect.origin.y+TAB_HEIGHT);
  tright= NSMakePoint(rect.origin.x+_tabWidth+0.5, rect.origin.y+TAB_HEIGHT);
  
  bleft= NSMakePoint(rect.origin.x+0.5, rect.origin.y-3.0);
  bright= NSMakePoint(rect.origin.x+_tabWidth+0.5, rect.origin.y-3.0);
  
  if ([item tabState] == ISFront)
  {
    NSRect r= rect;
    if (_tabsOnBottom)
    {
      r.origin.y+= 3.0;
      bleft.y+= 2.0;
      bright.y+= 2.0;
    }
    else
    {
      r.origin.y-= 3.0;
      r.size.height-= 2.0;
      tleft.y-= 5.0;
      tright.y-= 5.0;
    }
    //NSDrawWindowBackground(r);
    [[NSColor whiteColor] set];
    NSRectFill(r);
    
    [[NSColor darkGrayColor] set];
    
    // left
    [NSBezierPath strokeLineFromPoint:tleft toPoint:bleft];
    
    // right
    [NSBezierPath strokeLineFromPoint:tright toPoint:bright];
    
    // bottom line
    if (_tabsOnBottom)
    {
      bleft.x+= 2.0;
      bright.x-= 2.0;
      
      [NSBezierPath strokeLineFromPoint:bleft
                                toPoint:bright];	
      
      // corners
      [NSBezierPath strokeLineFromPoint:bright
                                toPoint:NSMakePoint(bright.x+2.0,r.origin.y+1.0)];
      
      [NSBezierPath strokeLineFromPoint:bleft
                                toPoint:NSMakePoint(bleft.x-2.0,r.origin.y+1.0)];	
    }
    else
    {
      tleft.x+= 2.0;
      tright.x-= 2.0;
      
      [NSBezierPath strokeLineFromPoint:tleft
                                toPoint:tright];
      
      // corners
      [NSBezierPath strokeLineFromPoint:tright
                                toPoint:NSMakePoint(tright.x+2.0,tright.y-1.0)];
      
      [NSBezierPath strokeLineFromPoint:tleft
                                toPoint:NSMakePoint(tleft.x-2.0,tleft.y-1.0)];
    }
  }
  else
  {
    if ([item tabState] == ISHover)
      NSRectFillListWithColorsUsingOperation(&rect,_backColor+1,1,NSCompositePlusDarker);
    else if ([item tabState] == ISPushed)
      NSRectFillListWithColorsUsingOperation(&rect,_backColor+2,1,NSCompositePlusDarker);
    
    if (index != _currentPage + 1)
    {
      // left
      [NSBezierPath strokeLineFromPoint:tleft toPoint:bleft];
    }
    if (index != _currentPage - 1)
    {
      // right
      [NSBezierPath strokeLineFromPoint:tright toPoint:bright];	
    }
  }
  
  NSImage *img= nil;
  float textY= _tabY;
  if (_tabsOnBottom)
    textY+= 2.0;
  else
    textY+= 0.0;
  
  img= [item icon];
  [img compositeToPoint:NSMakePoint(rect.origin.x+6, textY+1.0)
               fromRect:MXRectWithSize([img size])
              operation:NSCompositeSourceOver];
  
  switch ([item closeState])
  {
    case ISBack: img= _closeImage[0]; break;
    case ISFront: img= _closeImage[0]; break;
    case ISHover: img= _closeImage[1]; break;
    case ISPushed: img= _closeImage[2]; break;
  }
  [img compositeToPoint:NSMakePoint(rect.origin.x+rect.size.width-16.0, textY+1.0)
               fromRect:MXRectWithSize([img size])
              operation:NSCompositeSourceOver];
  
  [_tabAttrs setObject:[NSColor colorWithDeviceWhite:0.8 alpha:1.0] forKey:NSForegroundColorAttributeName];
  [[[_pages objectAtIndex:index] label] drawAtPoint:NSMakePoint(rect.origin.x+25.0, textY)
                                     withAttributes:_tabAttrs];
  
  [_tabAttrs setObject:[[NSColor blackColor] colorWithAlphaComponent:0.9] forKey:NSForegroundColorAttributeName];
  [[[_pages objectAtIndex:index] label] drawAtPoint:NSMakePoint(rect.origin.x+25.0, textY+1.0)
                                     withAttributes:_tabAttrs];  
}


- (void)drawRect:(NSRect)rect 
{  
  NSRect r= [self bounds];
  float i;
  int count= [_pages count];
  float y1, y2, y3;

  rect= r;
  r.origin.y= rect.size.height-TAB_HEIGHT;
  r.size.height= TAB_HEIGHT-1.0;
  
  // ------- y1    ------- y3
  // |-----| y2    |     |
  // |     |       |     |
  // |     |       |-----| y2
  // ------- y3    ------- y1
  if (_tabsOnBottom)
  {
    y1= 0.5;
    y2= TAB_HEIGHT-0.5;
    y3= rect.size.height-1.5;
  }
  else
  {
    y1= rect.size.height-0.5;
    y2= rect.size.height-TAB_HEIGHT-0.5;
    y3= 0.5;
  }
  
  NSRectFillListWithColorsUsingOperation(&r,_backColor,1,NSCompositeSourceOver);
  
  if (_hasBorder)
  {
    [NSBezierPath setDefaultLineWidth:0.0];
    [_borderColor set];  
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y3)
                              toPoint:NSMakePoint(r.size.width-0.5,y3)];
    
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y2)
                              toPoint:NSMakePoint(r.size.width-0.5,y2)];
    
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y1)
                              toPoint:NSMakePoint(r.size.width-0.5,y1)];
    
    [_lborderColor set];
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y3+1.0)
                              toPoint:NSMakePoint(r.size.width-0.5,y3+1.0)];
    
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y2+1.0)
                              toPoint:NSMakePoint(r.size.width-0.5,y2+1.0)];
    
    [NSBezierPath strokeLineFromPoint:NSMakePoint(r.origin.x+0.5,y1+1.0)
                              toPoint:NSMakePoint(r.size.width-0.5,y1+1.0)];
    
  }
  if (count==0)
    return;
  
  if (count > _visibleTabCount)
    count= _visibleTabCount;
  
  [_borderColor set];
  for (i= 0; i < count; i += 1)
  {	
    if (i != _currentPage)
      [self drawTab:i];
  }  
  [self drawTab:_currentPage];
}

- (void)setDelegate:(id)deleg
{
  delegate= deleg;
}

- (void)setTabsOnBottom:(BOOL)flag
{
  _tabsOnBottom= flag;
}

- (void)setMaxTabSize:(float)size
{
  _maxTabWidth= size;
}

- (MTabViewItem*)pageAtIndex:(int)page
{
  return [_pages objectAtIndex:page];
}
@end
