//
//  MQSyntaxHelp.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface MQSyntaxHelp : NSObject 
{
  NSMutableArray *_index;
  NSImage *_groupIcon;
  NSImage *_icon;
}

- (void)loadFile:(NSString*)file;
- (void)setGroupIcon:(NSImage*)groupIcon
            itemIcon:(NSImage*)icon;
- (const char*)topicIdForItem:(id)item;
@end
