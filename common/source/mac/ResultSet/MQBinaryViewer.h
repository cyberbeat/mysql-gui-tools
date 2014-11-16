//
//  MQBinaryViewer.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/28/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MQBVDataSource;

@interface MQBinaryViewer : NSObject {
  NSScrollView *_sview;
  NSTableView *_table;
  
  MQBVDataSource *_dataSource;
}

- (id)initWithData:(NSData*)data;
- (NSString*)label;
- (NSView*)view;
- (void)setEditable:(BOOL)flag;
- (void)setData:(NSData*)data;
@end
