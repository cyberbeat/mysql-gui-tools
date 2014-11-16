//
//  MQTextViewer.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MQTextViewer : NSObject 
{
  NSScrollView *_sview;
  NSTextView *_text;
  BOOL _edited;
}

- (id)initWithData:(NSData*)data;
- (void)setData:(NSData*)data;
- (NSData*)editedData;
- (NSString*)label;
- (NSView*)view;
- (void)setEditable:(BOOL)flag;
@end
