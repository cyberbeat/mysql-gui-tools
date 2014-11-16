//
//  MQImageViewer.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQImageViewer : NSObject {
  NSScrollView *_scroll;
  NSImage *_image;
  NSImageView *_imageView;
}

+ (BOOL)canDisplayData:(NSData*)data;

- (id)initWithData:(NSData*)data;
- (void)setData:(NSData*)data;
- (NSString*)label;
- (NSView*)view;
- (void)setEditable:(BOOL)flag;

@end
