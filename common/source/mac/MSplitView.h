//
//  MSplitView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MSplitView : NSSplitView
{
  NSColor *_inactiveColor;
  NSColor *_dividerColor;
  BOOL _active;
  float _thickness;
}

- (void)resizeSubview:(id)view toSize:(float)size;
- (void)setActive:(BOOL)active;
- (void)setInactiveColor:(NSColor*)color;
- (void)setDividerColor:(NSColor*)color;
- (void)setDividerThickness:(float)value;

@end
