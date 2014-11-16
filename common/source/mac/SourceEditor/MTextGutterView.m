//
//  MTextGutterView.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MTextGutterView.h"

#import "mxUtils.h"

@implementation MTextGutterView

- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    _attributes= [[NSMutableDictionary alloc] init];

    [self setAutoresizingMask:NSViewHeightSizable|NSViewWidthSizable];

    _lineHeight= 20.0;

    [_attributes setObject:[NSFont systemFontOfSize:9] forKey:@"Font"];
    [_attributes setObject:[NSColor colorWithDeviceWhite:0.93 alpha:1.0] forKey:@"BackgroundColor"];
    [_attributes setObject:[NSColor colorWithDeviceWhite:0.34 alpha:1.0] forKey:@"TextColor"];
    [_attributes setObject:[NSNumber numberWithInt:_lineHeight] forKey:@"LineHeight"];

    _textAttr= [[NSMutableDictionary alloc] init];
    [_textAttr setObject:[_attributes objectForKey:@"TextColor"] forKey:NSForegroundColorAttributeName];
    [_textAttr setObject:[_attributes objectForKey:@"Font"] forKey:NSFontAttributeName];
  }
  return self;
}

- (void)dealloc
{
  [_attributes release];
  [_textAttr release];
  [super dealloc];
}

- (void)setDelegate:(id)deleg
{
  _delegate= deleg;
}

- (void)drawRect:(NSRect)rect
{
  unsigned int i;
  char numbuf[32];
  int firstLine, lastLine;
  NSRect myRect= [self bounds];
  NSFont *font= [_attributes objectForKey:@"Font"];
  float y;
  float yOffset= [font descender];
  BOOL callDelegate= [_delegate respondsToSelector:@selector(gutterView:drawLine:rect:)];

  [[_attributes objectForKey:@"BackgroundColor"] set];
  NSRectFill(rect);

  firstLine= NSMinY(rect) / _lineHeight;
  lastLine= MAX(MIN(NSMaxY(rect) / _lineHeight, _numberOfLines-1), 0);

  for (i= firstLine; i <= lastLine; i++)
  {
    NSString *s;
    y= floor(i * _lineHeight - yOffset);
    sprintf(numbuf, "%i", i+1);
    s= [[NSString alloc] initWithBytesNoCopy:numbuf length:strlen(numbuf) encoding:NSUTF8StringEncoding freeWhenDone:NO];
    [s drawAtPoint:NSMakePoint(floor(NSWidth(myRect)-[font widthOfString:s])-2, y) withAttributes:_textAttr];
    [s release];
    
    if (callDelegate)
      [_delegate gutterView:self 
                   drawLine:i
                       rect:NSMakeRect(1, (i+1)*_lineHeight, NSWidth(myRect)-2, _lineHeight)];
  }
}


- (BOOL)mouseDown:(NSEvent*)event
{
  float y= [self convertPoint:[event locationInWindow]
                     fromView:nil].y;
  unsigned int line= y / _lineHeight;
  
  if (line < _numberOfLines)
    [_delegate gutterView:self clickedLine:line];
  return YES;
}

- (BOOL)isFlipped
{
  // textview is flipped, so we flip too
  return YES;
}

- (void)setLineHeight:(float)height
{
  _lineHeight= height;
}

- (void)setNumberOfLines:(int)count
{
  if (_numberOfLines != count)
  {
    _numberOfLines= count;
    [self setNeedsDisplay:YES];
  }
}


@end
