//
//  MAFileTextField.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Mon Aug 02 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <AppKit/AppKit.h>


@interface MAFileTextField : NSView 
{
  NSTextField *_textf;
  NSButton *_button;
  BOOL _directory;
}

- (void)setPickDirectory:(BOOL)flag;
- (void)setPath:(NSString*)path;
- (NSString*)path;

@end
