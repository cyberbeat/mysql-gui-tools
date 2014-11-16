//
//  MQTableView.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/29/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQTableView.h"
#import "MQResultSetCell.h"

@implementation MQTableView

- (void) awakeFromNib
{
  int h= [[NSUserDefaults standardUserDefaults] integerForKey:  @"ResultsetRowHeight"];
  if(h != [self rowHeight])
    [self setRowHeight: h];
}

- (void)drawRect:(NSRect)aRect
{
  [super drawRect:aRect];
  if (_highlight)
  {
    [[NSColor alternateSelectedControlColor] set];
    [NSBezierPath setDefaultLineWidth:4];
    [NSBezierPath strokeRect:aRect];
  }  
}

#if 0
- (void)drawGridInClipRect:(NSRect)aRect
{
  NSRange columns= [self columnsInRect:clipRect];
  NSRange rows= [self rowsInRect:clipRect];
  
  [[self gridColor] set];
  
  //XXX
}
#endif

- (void)mouseDown:(NSEvent*)event
{
  NSPoint point= [self convertPoint:[event locationInWindow]
                           fromView:nil];
  theColumn= [self columnAtPoint:point];
  [self setNeedsDisplay];
  [super mouseDown:event];
  
  [[self delegate] performSelector:@selector(tableViewWasClicked:) withObject:self];
}

- (void)keyDown:(NSEvent *)theEvent
{
  if ([theEvent type] == NSKeyDown)
  {
    unichar key= [[theEvent characters] characterAtIndex:0];
    
    if (key == NSLeftArrowFunctionKey)
    {
      if (theColumn > 1) // we don't want to select the indicator
        theColumn--;
      [self setNeedsDisplay];
      [self scrollColumnToVisible:theColumn];
    }
    else if (key == NSRightArrowFunctionKey)
    {
      if (theColumn < [self numberOfColumns]-1)
        theColumn++;
      [self setNeedsDisplay];
      [self scrollColumnToVisible:theColumn];
    }
  }
  [super keyDown:theEvent];
}

- (void)editColumn:(int)columnIndex row:(int)rowIndex withEvent:(NSEvent *)theEvent select:(BOOL)flag
{
  theColumn= [self editedColumn];
  [self setNeedsDisplay];
  [super editColumn:columnIndex row:rowIndex withEvent:theEvent select:flag]; 
}


// highlighting is done in the cell itself because the highlight is drawn
// before the cell, so the cell's background color will be drawn over the 
// selection highlight

- (id)_highlightColorForCell:(NSCell *)cell
{
  return nil;
}

- (int)lastClickedColumn
{
  return theColumn;
}

- (void)selectCellAtRow:(unsigned int)row
                 column:(unsigned int)column
{
  theColumn= column;
  [self selectRow:row byExtendingSelection:NO];
}


- (void)setDragHandlers:(NSDictionary*)handlers
                 target:(id)target
{
  if (_handlers != handlers)
  {
    _handlerTarget= target;
    [_handlers release];
    _handlers= [handlers retain];
  }
}


- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
//  _highlight= YES;
//  [self setNeedsDisplay];
  return NSDragOperationCopy;
}

- (NSDragOperation)draggingUpdated:(id <NSDraggingInfo>)sender
{
  return NSDragOperationCopy;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
//  _highlight= NO;
//  [self setNeedsDisplay];
}

- (void)concludeDragOperation:(id <NSDraggingInfo>)sender
{
  _highlight= NO;
  [self setNeedsDisplay];  
}

- (BOOL)prepareForDragOperation:(id <NSDraggingInfo>)sender
{
  return YES;
}


- (BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
  NSPasteboard *pboard= [sender draggingPasteboard];
  NSArray *types= [pboard types];
  unsigned int i, c= [types count];

  for (i= 0; i < c; i++)
  {
    id type= [types objectAtIndex:i];
    NSString *handler= [_handlers objectForKey:type];
    if (handler)
    {
      if ([_handlerTarget performSelector:NSSelectorFromString(handler)
                               withObject:self
                               withObject:sender])
        return YES;
      break;
    }
  }
  return NO;
}

@end
