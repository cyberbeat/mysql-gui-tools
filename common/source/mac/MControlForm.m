//
//  MControlForm.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 06/4/30.
//  Copyright 2006 MySQL AB. All rights reserved.
//

#import "MControlForm.h"

#define HORIZ_SPACING 4

@implementation MFormCell

- (NSMutableDictionary*)_attributesDict
{
  static NSMutableDictionary *attributes= 0;

  if (!attributes)
  {
    NSMutableParagraphStyle *paragraphStyle;
    paragraphStyle= [[NSParagraphStyle defaultParagraphStyle] mutableCopy];
    //if (_cell.wraps)
    //  [paragraphStyle setLineBreakMode: NSLineBreakByWordWrapping];
    //else
    [paragraphStyle setLineBreakMode: NSLineBreakByClipping];
    [paragraphStyle setAlignment: NSRightTextAlignment];
    
    attributes= [[NSMutableDictionary alloc] init];
    [attributes setObject:paragraphStyle forKey:NSParagraphStyleAttributeName];
    [attributes setObject:[NSFont systemFontOfSize:[NSFont systemFontSize]] forKey:NSFontAttributeName];
  }
  else
  {
    NSMutableParagraphStyle *paragraphStyle;  
    paragraphStyle= [attributes objectForKey: NSParagraphStyleAttributeName];
    [paragraphStyle setLineBreakMode: NSLineBreakByClipping];
    [paragraphStyle setAlignment: NSRightTextAlignment];
  }
  return attributes;
}


- (NSMutableDictionary*)_descriptionAttributesDict
{
  NSMutableDictionary *attr= [self _attributesDict];
  NSMutableParagraphStyle *paragraphStyle;  
  paragraphStyle= [attr objectForKey: NSParagraphStyleAttributeName];
  [paragraphStyle setLineBreakMode: NSLineBreakByWordWrapping];
  [paragraphStyle setAlignment: NSLeftTextAlignment];
  return attr;
}


- (NSSize)sizeForString:(NSString*)aString withFont:(NSFont*)aFont width:(float)aWidth
{
  if (![aString hasSuffix:@"\n"])
    aString = [NSString stringWithFormat:@"%@\n", aString];
  
  NSTextStorage *textStorage = [[[NSTextStorage alloc] initWithString:aString] autorelease];
  NSTextContainer *textContainer = [[[NSTextContainer alloc] initWithContainerSize:NSMakeSize(aWidth,1e7)] autorelease];
  NSLayoutManager *layoutManager = [[[NSLayoutManager alloc] init] autorelease];
  [layoutManager addTextContainer:textContainer];
  [textStorage addLayoutManager:layoutManager];
  [layoutManager setTypesetterBehavior:NSTypesetterBehavior_10_2_WithCompatibility];
  
  [textStorage addAttribute:NSFontAttributeName value:aFont range:NSMakeRange(0, [textStorage length])];
  [textContainer setLineFragmentPadding:0.0];

  [layoutManager glyphRangeForTextContainer:textContainer];	
  
  return [layoutManager usedRectForTextContainer:textContainer].size;
}


- (float)calcTitleWidth
{
  return [[self font] widthOfString:[self stringValue]];
}


- (void)setTitleWidth:(float)width
{
  _titleWidth= width;
}


- (float)titleWidth
{
  if (_titleWidth == 0)
    _titleWidth= [self calcTitleWidth];
  return _titleWidth;
}


- (NSSize)cellSizeForWidth:(float)aWidth
{
  NSSize size= [super cellSize];
  NSSize contentSize= [_contentCell cellSize];
  size.height= MAX(size.height, contentSize.height);
  size.width+= contentSize.width;
  if (_descriptionString)
  {
    NSSize descrSize;
    
    descrSize= [self sizeForString:_descriptionString 
                          withFont:_descriptionFont?:[NSFont systemFontOfSize:[NSFont systemFontSizeForControlSize:NSMiniControlSize]] 
                             width:aWidth];
    
    size.width+= descrSize.width;
    size.height= MAX(size.height, descrSize.height);
  
    _descriptionSize= descrSize;
  }
  return size;
}


- (NSSize)cellSize
{
  NSSize size= [super cellSize];
  NSSize contentSize= [_contentCell cellSize];
  size.height= MAX(size.height, contentSize.height);
  size.width+= contentSize.width;
  if (_descriptionString)
  {
    size.width+= _descriptionSize.width;
    size.height= MAX(size.height, _descriptionSize.height);
  }
  return size;
}


- (BOOL)trackMouse:(NSEvent *)theEvent
            inRect:(NSRect)cellFrame
            ofView:(NSView *)controlView
      untilMouseUp:(BOOL)untilMouseUp
{
  NSPoint location= [controlView convertPoint:[theEvent locationInWindow] fromView:nil];
  float x= [self titleWidth];
  
  if (location.x >= x && location.x < x + [_contentCell cellSize].width)
  {
    cellFrame.origin.x= x;
    cellFrame.size= [_contentCell cellSize];
    return [_contentCell trackMouse:theEvent inRect:cellFrame ofView:controlView untilMouseUp:untilMouseUp];
  }
  return NO;
}


- (void)setTitle:(NSString*)title
{
  [super setStringValue:title];
}

- (NSString*)title
{
  return [super stringValue];
}


- (void)setDescriptionFont:(NSFont*)font
{
  if (_descriptionFont != font)
  {
    [_descriptionFont release];
    _descriptionFont= [font retain];
  }
}


- (NSFont*)descriptionFont
{
  return _descriptionFont;
}


- (void)setDescription:(NSString*)description
{
  if (_descriptionString != description)
  {
    [_descriptionString release];
    _descriptionString= [description retain];
  }
}


- (NSString*)descrption
{
  return _descriptionString;
}


- (void)setContentCell:(NSCell*)cell
{
  if (_contentCell != cell)
  {
    [_contentCell release];
    _contentCell= [cell retain];
  }
}


- (NSCell*)contentCell
{
  return _contentCell;
}


- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView *)controlView
{
  NSMutableDictionary *attrs= [self _attributesDict];
  NSRect rect;
  NSRect contentRect;
  float leftoverWidth;
  NSString *title;

  if ([self state] || [self isHighlighted]) 
  {
    //[[NSColor  whiteColor] set];
    //NSRectFill(cellFrame);
  }
  rect= cellFrame;

  title= [self stringValue];
  if (title && ![title isEqualTo:@""])
  {
    // draw title
    rect.size.width= [self titleWidth];
    [attrs setObject:[self font] forKey:NSFontAttributeName];
    [[attrs objectForKey:NSParagraphStyleAttributeName] setLineBreakMode:NSLineBreakByClipping];
    [title drawInRect:rect withAttributes:attrs];
    
    rect.origin.x+= HORIZ_SPACING + NSWidth(rect);
    leftoverWidth= NSWidth(cellFrame) - (NSWidth(rect) + HORIZ_SPACING);
  }
  else
    leftoverWidth= NSWidth(cellFrame);
  
  // draw the action cell
  if (_descriptionString)
  {
    rect.size.width= leftoverWidth / 2;
    if (_minActionWidth > 0 && NSWidth(rect) < _minActionWidth)
      rect.size.width= _minActionWidth;
    leftoverWidth-= NSWidth(rect);
  }
  else
    rect.size.width= leftoverWidth;
  
  contentRect.size= [_contentCell cellSize];
  contentRect.size.width= rect.size.width;
  contentRect.origin.x= rect.origin.x;
  contentRect.origin.y= rect.origin.y= rect.origin.y - (NSHeight(rect) - NSHeight(contentRect)) / 2;
  [_contentCell drawWithFrame:contentRect inView:controlView];
  
  // description
  if (_descriptionString)
  {
    rect.origin.y= rect.origin.y - (NSHeight(rect) - _descriptionSize.height) / 2;
    rect.origin.x+= HORIZ_SPACING + NSWidth(rect);
    rect.size.width= leftoverWidth - HORIZ_SPACING;

    attrs= [self _descriptionAttributesDict];
    [attrs setObject:_descriptionFont?:[NSFont systemFontOfSize:[NSFont systemFontSizeForControlSize:NSMiniControlSize]]
              forKey:NSFontAttributeName];
    [_descriptionString drawInRect:rect withAttributes:attrs];
  }
}


- (void)editWithFrame:(NSRect)aRect
               inView:(NSView *)controlView
               editor:(NSText *)textObj
             delegate:(id)anObject
                event:(NSEvent *)theEvent
{
  NSLog(@"edit %@", NSStringFromRect(aRect));
}

@end




@implementation MControlForm


- (id)initWithFrame:(NSRect)rect
{
  self= [super initWithFrame:rect];
  if (self)
  {
    _rows= [[NSMutableArray alloc] init];
    _spacing= 4.0;
  }
  return self;
}


- (void)dealloc
{
  [_rows release];
  [super dealloc];
}


- (BOOL)isFlipped
{
  return YES;
}


- (BOOL)acceptsFirstMouse: (NSEvent *)aEvent
{
  return YES;
}


- (BOOL)acceptsFirstResponder
{
  return YES;
}

- (BOOL)becomeFirstResponder
{
  NSText *text = [[self window] fieldEditor: YES
                            forObject: self];
  NSLog(@"1st resp"); 
  if ([text superview] != nil)
    if ([text resignFirstResponder] == NO)
      return NO;

  [[_rows lastObject] selectWithFrame: [[_rows lastObject] frame]
                          inView: self
                          editor: text
                        delegate: self
                           start: 0
                          length: 1];
  return YES;
}


- (int)getRowForPoint:(NSPoint)location
{
  unsigned int i, c= [_rows count];
  float y;
  
  y= 0;
  for (i= 0; i < c; i++)
  {
    MFormCell *cell= [_rows objectAtIndex:i];
    NSSize size= [cell cellSize];

    if (location.y <= y + size.height)
      return i;
    y+= size.height + _spacing;
  }
  return -1;
}


- (void)mouseDown:(NSEvent*)theEvent
{
  NSPoint location= [theEvent locationInWindow];
  NSRect cellFrame= [self frame];
  int row= -1;
  unsigned int i, c= [_rows count];
  float y= 0.0;
  
  location= [self convertPoint:location fromView:nil];  

  for (i= 0; i < c; i++)
  {
    MFormCell *cell= [_rows objectAtIndex:i];
    NSSize size= [cell cellSize];

    if (location.y <= y + size.height)
    {
      cellFrame.origin.y+= y;
      cellFrame.size= size;
      row= i;
      break;
    }
    y+= size.height + _spacing;
  }
  if (row < 0)
    return;

  if (row >= 0)
  {
    MFormCell *cell= [_rows objectAtIndex:row];
    
    if ([cell isEnabled])
    {
      NSLog(@"%@ %i %@",cell, row, NSStringFromRect(cellFrame));
      [cell trackMouse:theEvent inRect:cellFrame ofView:self untilMouseUp:YES];
    }
  }
}


- (void)relayout
{
  unsigned int i, c= [_rows count];
  float width, maxWidth= 0;
  
  for (i= 0; i < c; i++)
  {
    MFormCell *cell= [_rows objectAtIndex:i];
    width= [cell calcTitleWidth];
    maxWidth= MAX(width, maxWidth);
  }
  for (i= 0; i < c; i++)
  {
    MFormCell *cell= [_rows objectAtIndex:i];
    [cell setTitleWidth:maxWidth];
  }    
}


- (void)drawRect:(NSRect)aRect
{
  unsigned int i, c= [_rows count];
  NSRect frame= [self frame];
  NSRect cellFrame;
  
  [self relayout];
  
  NSDrawWindowBackground(aRect);
  
  [self lockFocus];
  
  cellFrame.origin= NSMakePoint(0,0);
  cellFrame= frame;
  for (i= 0; i < c; i++)
  {
    MFormCell *cell= [_rows objectAtIndex:i];

    cellFrame.size.height= [cell cellSizeForWidth:NSWidth(frame)].height;
    [cell drawWithFrame:cellFrame inView:self];
    cellFrame.origin.y+= NSHeight(cellFrame) + _spacing;
  }
  
  [self unlockFocus];
}

- (void)addFormRow:(MFormCell*)cell
{
  [_rows addObject:cell];
}


- (void)insertFormRow:(MFormCell*)cell atIndex:(int)index
{
  //[_rows insertObject:cell atIndex:index];
}


- (MFormCell*)addTextFieldWithTitle:(NSString*)title
                        description:(NSString*)description
{
  NSTextFieldCell *cell= [[NSTextFieldCell alloc] initTextCell:@""];
  MFormCell *formCell= [[MFormCell alloc] init];
  
//  [cell setBezeled:YES];
///  [cell setBezelStyle:NSTextFieldSquareBezel];
//  [cell setDrawsBackground:YES];
  [cell setEditable:YES];
//  [cell setEnabled:YES];
  [formCell setTitle:title];
  [formCell setContentCell:[cell autorelease]];
  [formCell setDescription:description];
  [self addFormRow:formCell];
  
  return [formCell autorelease];
}


- (MFormCell*)addCheckBoxWithTitle:(NSString*)title
                       description:(NSString*)description
{
  return nil;
}


- (MFormCell*)addPopUpWithTitle:(NSString*)title
                          items:(NSArray*)items
                    description:(NSString*)description
{
  NSPopUpButtonCell *cell= [[NSPopUpButtonCell alloc] initTextCell:@"" pullsDown:NO];
  MFormCell *formCell= [[MFormCell alloc] init];

  [cell addItemsWithTitles:items];
  [formCell setTitle:title];
  [formCell setContentCell:[cell autorelease]];
  [formCell setDescription:description];
  [self addFormRow:formCell];

  return [formCell autorelease];  
}


- (MFormCell*)addFilePickerWithTitle:(NSString*)title
                           directory:(BOOL)flag
                         description:(NSString*)description
{
  return nil;
}

@end
