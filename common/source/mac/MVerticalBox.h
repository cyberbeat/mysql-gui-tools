//
//  MVerticalBox.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 05/9/12.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MVerticalBox : NSView
{
  float _spacing;
  float _xOffset;
  float _yOffset;
  BOOL _expandHorizontally;
}

- (void)setOffset:(NSSize)offset;
- (void)setSpacing:(float)spacing;
- (void)setExpandsHorizontally:(BOOL)flag;

@end
