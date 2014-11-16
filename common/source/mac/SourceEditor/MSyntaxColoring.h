//
//  MSyntaxColoring.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/20/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MSyntaxColoring : NSObject
{
  struct MarkerInfo *_markers; // there is a max of 32 marker types!
  int _markerCount;

  struct LineInfo *_lines;
  unsigned int _linesSize;
  
@protected
  unsigned int _lineCount;
  
  id _delegate;

  NSTextView *_textView;
  NSMutableDictionary *_colors;
}

+ (NSMutableDictionary*)defaultColorDictionary;

- (id)initForTextView:(NSTextView*)textView;

- (void)reparseLines;
- (unsigned int)numberOfLines;
- (NSRange)lineRangeForCharacterRange:(NSRange)range;
- (unsigned int)lineIndexForCharacter:(unsigned int)charIndex;
- (NSRange)characterRangeOfLine:(unsigned int)line;

- (void)recolorRange:(NSRange)charRange;

- (void)setDelegate:(id)delegate;

- (void)addMarkerName:(NSString*)name
            withColor:(NSColor*)color;
- (void)addMarkerName:(NSString*)name;
- (NSColor*)colorForMarker:(NSString*)name;

- (void)addMarker:(NSString*)name
           atLine:(unsigned int)line;
- (void)removeMarker:(NSString*)name
              atLine:(unsigned int)line;
- (void)removeMarkerFromAllLines:(NSString*)name;
- (void)removeMarkersAtLine:(unsigned int)line;
- (BOOL)marker:(NSString*)name
        atLine:(unsigned int)line;
- (NSArray*)markersAtLine:(unsigned int)line;
- (NSColor*)colorMarkerAtLine:(unsigned int)line;
- (NSArray*)completionListForWord:(NSString*)word;

@end
