//
//  MQActionBar.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/9.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include <myx_public_interface.h>

@class MQDropActionView;

@interface MQActionBar : NSView 
{
  NSMutableArray *_actions;

  NSString *_originalQuery;
  MYSQL *_mysql;
  NSTextView *_textView;
  NSTimer *_timer;
}


- (void)setMySQL:(MYSQL*)mysql;
- (void)setTextView:(NSTextView*)textView;
- (void)popupAt:(NSPoint)pos;
- (void)hide;
- (void)delayedHide;
- (void)show;

@end
