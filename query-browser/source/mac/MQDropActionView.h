//
//  MQDropActionView.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/9.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQDropActionView : NSView 
{
  NSFont *_font;
  NSString *_label;
  
  id _target;
  SEL _action;
  
  BOOL _highlight;
}

- (void)setFont:(NSFont*)font;
- (void)setText:(NSString*)text;
- (void)setTarget:(id)target;
- (void)setDropAction:(SEL)action;

@end
