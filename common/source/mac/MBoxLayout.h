//
//  MBoxLayout.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Fri Jul 23 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


typedef struct {
  BOOL expand;
  NSSize normalSize;
  float minSize;
} MBoxLayoutAttributes;

@interface MBoxLayout : NSView
{
  BOOL _vertical;
  float _topBorder, _bottomBorder, _leftBorder, _rightBorder;
  float _spacing;
  
  int _subviewCount;
  MBoxLayoutAttributes *_flags;
}

- (void)setLeftBorder:(float)leftWidth
            topBorder:(float)topWidth
          rightBorder:(float)rightWidth
         bottomBorder:(float)bottomWidth;
- (void)setAllBorders:(float)width;
- (void)setSpacing:(float)spacing;
- (void)setVertical:(BOOL)vertical;

- (void)pack:(NSView*)subview
      expand:(BOOL)expand;

- (void)pack:(NSView*)subview
      expand:(BOOL)expand
     minSize:(float)mins;

- (void)updateLayout;
- (void)sizeToFit;

@end
