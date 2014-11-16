//
//  MSourceTextView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSourceTextView.h"
#import "MSyntaxColoring.h"
#import "MSourceLayoutManager.h"
#import "mxUtils.h"

@implementation MSourceTextView


- (id)initWithFrame:(NSRect)rect
{
  self= [super initWithFrame:rect];
  if (self)
  {    
    [[self textContainer] replaceLayoutManager:[[[MSourceLayoutManager alloc] init] autorelease]];
    
    _bpGutter= [MXGetImageFromBundle([NSBundle bundleForClass:[MSourceTextView class]],@"gutter_breakpoint.png") retain];
    _pcGutter= [MXGetImageFromBundle([NSBundle bundleForClass:[MSourceTextView class]],@"gutter_current_pos.png") retain];
    _stGutter= [MXGetImageFromBundle([NSBundle bundleForClass:[MSourceTextView class]],@"gutter_query_start.png") retain];

    _breakpointsEnabled= YES;
    
    [self setDelegate:self];
  }
  return self;
}


- (void)dealloc
{
  [_bpGutter release];
  [_pcGutter release];
  [_stGutter release];  
  [_syntaxColor release];
  [super dealloc];
}

- (void)setGutterView:(MTextGutterView*)gutter
{
  _gutterView= gutter;
  [gutter setDelegate:self];
  
  [_gutterView setLineHeight:[[self layoutManager] defaultLineHeightForFont:[self font]]];
}


- (void)setFont:(NSFont*)font
{
  [super setFont:font];
  [_gutterView setLineHeight:[[self layoutManager] defaultLineHeightForFont:font]];
}


- (NSArray *)completionsForPartialWordRange:(NSRange)charRange 
                        indexOfSelectedItem:(int *)index
{
  return [_syntaxColor completionListForWord:[[self string] substringWithRange:charRange]];
}


- (void)setSyntaxColorer:(MSyntaxColoring*)colorer
{
  if (_syntaxColor != colorer)
  {
    [_syntaxColor release];
    _syntaxColor= [colorer retain];
  }
  [colorer setDelegate:self];
  [(MSourceLayoutManager*)[self layoutManager] setSyntaxColorer:colorer];
}

- (MSyntaxColoring*)syntaxColorer
{
  return _syntaxColor;
}


- (void)gutterView:(MTextGutterView*)gutter drawLine:(unsigned int)line rect:(NSRect)rect
{
  if (_breakpointsEnabled && [_syntaxColor marker:@"breakpoint" atLine:line])
    [_bpGutter compositeToPoint:rect.origin operation:NSCompositeSourceOver];
  else if ([_syntaxColor marker:@"statement" atLine:line])
    [_stGutter compositeToPoint:rect.origin operation:NSCompositeSourceOver];
  if ([_syntaxColor marker:@"pc" atLine:line])
    [_pcGutter compositeToPoint:NSMakePoint(NSMaxX(rect)-[_pcGutter size].width, NSMinY(rect))
                      operation:NSCompositeSourceOver];
}

- (void)gutterView:(MTextGutterView*)gutter clickedLine:(unsigned int)line
{  
  if (_breakpointsEnabled && [_syntaxColor marker:@"breakpoint" atLine:line])
    [_syntaxColor removeMarker:@"breakpoint" atLine:line];
  else if ([_syntaxColor marker:@"statement" atLine:line])
    [_syntaxColor addMarker:@"breakpoint" atLine:line];
  [_gutterView setNeedsDisplay:YES];
}

- (void)syntaxColoringDidFinish:(MSyntaxColoring*)colorer
{
  [_gutterView setNumberOfLines:[colorer numberOfLines]];
}

- (void)enableBreakpoints:(BOOL)flag
{
  _breakpointsEnabled= flag;
}

@end
