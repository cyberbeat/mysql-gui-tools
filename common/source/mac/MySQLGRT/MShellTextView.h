//
//  MShellTextView.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRT.h>
#import "myx_grt_public_interface.h"

@interface MShellTextView : NSTextView
{
  MGRT *_grt;
  
  NSMutableArray *_history;
  int _historyIndex;
  NSString *_currentString;
  
  NSRange _promptRange;
  BOOL _promptShown;
}

- (void)setGRTEnvironment:(MGRT*)grt;
- (void)makeReady;
- (void)appendText:(NSString*)text;
@end
