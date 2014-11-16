//
//  MButtonImageCell.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Wed Jul 07 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MButtonImageCell : NSButtonCell {
  @private
  NSImage *_image;
}

- (void)setIconImage:(NSImage*)image;
- (NSImage*)iconImage;

- (void)drawWithFrame:(NSRect)cellFrame inView:(NSView *)view;
- (NSSize)cellSize;

@end