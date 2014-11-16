//
//  MTextGutterView.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 5/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MTextGutterView : NSView 
{
  NSMutableDictionary *_attributes;
  NSMutableDictionary *_textAttr;
  
  id _delegate;
  
  int _numberOfLines;
  float _lineHeight;
}

- (void)setLineHeight:(float)height;
- (void)setNumberOfLines:(int)count;

- (void)setDelegate:(id)deleg;

@end
