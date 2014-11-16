//
//  MSourceLayoutManager.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/24/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSourceLayoutManager.h"
#import "MSyntaxColoring.h"

@implementation MSourceLayoutManager

- (void)dealloc
{
  [_syntaxColor release];
  [super dealloc];
}

- (void)setSyntaxColorer:(MSyntaxColoring*)colorer
{
  if (_syntaxColor != colorer)
  {
    [_syntaxColor release];
    _syntaxColor= [colorer retain];
  }
}

- (MSyntaxColoring*)syntaxColorer
{
  return _syntaxColor;
}


- (void)drawBackgroundForGlyphRange:(NSRange)glyphRange atPoint:(NSPoint)containerOrigin
{
  unsigned int i;
  NSRange charRange;
  NSString *text= [[self textStorage] string];
  NSTextContainer *container= [self textContainerForGlyphAtIndex:glyphRange.location
                                                  effectiveRange:NULL];
    
  [super drawBackgroundForGlyphRange:glyphRange atPoint:containerOrigin];
  
  charRange= [self characterRangeForGlyphRange:glyphRange
                              actualGlyphRange:NULL];

  for (i= charRange.location; i < NSMaxRange(charRange); )
  {
    NSRange lineRange= [text lineRangeForRange:NSMakeRange(i, 0)];
    unsigned int line= [_syntaxColor lineIndexForCharacter:lineRange.location];
    NSColor *lineColor= [_syntaxColor colorMarkerAtLine:line];
    if (lineColor)
    {
      NSRectArray rects;
      unsigned int rectCount;
      rects= [self rectArrayForCharacterRange:lineRange
                 withinSelectedCharacterRange:NSMakeRange(NSNotFound, 0)
                              inTextContainer:container
                                    rectCount:&rectCount];
      if (rects)
      {
        unsigned int j;
        for (j= 0; j < rectCount; j++)
        {
          rects[j].origin.x= 0;
          rects[j].size.width= MAX(1000, rects[j].size.width);
          [[lineColor colorWithAlphaComponent:0.3] set];
          NSRectFillUsingOperation(rects[j], NSCompositeSourceOver);
          [lineColor set];
          [NSBezierPath strokeLineFromPoint:NSMakePoint(0, NSMinY(rects[j])+0.5)
                                    toPoint:NSMakePoint(NSWidth(rects[j]), NSMinY(rects[j])+0.5)];
          [NSBezierPath strokeLineFromPoint:NSMakePoint(0, NSMaxY(rects[j])-0.5)
                                    toPoint:NSMakePoint(NSWidth(rects[j]), NSMaxY(rects[j])-0.5)];
        }
      }
    }
    i= NSMaxRange(lineRange);
  }
}

@end