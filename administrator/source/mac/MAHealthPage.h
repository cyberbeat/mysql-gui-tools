//
//  MAHealthPage.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on 2/7/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@class MAHealthGroup;
@class MAHealthGraph;

@interface MAHealthPage : NSTabViewItem
{
  NSDictionary *_props;
  
  NSMutableArray *_groups;
}

- (id)initWithProperties:(NSDictionary*)props;
- (void)addGroup:(MAHealthGroup*)group;
- (void)removeGroup:(MAHealthGroup*)group;
- (void)rearrange;
- (void)setMenu:(NSMenu*)menu;
- (void)setProperties:(NSDictionary*)props;
- (NSDictionary*)properties;

- (NSArray*)groups;
@end



@interface MAHealthGroup : NSBox
{
  MAHealthPage *_page;
  
  NSDictionary *_props;
  NSMutableArray *_graphs;
}

- (id)initWithProperties:(NSDictionary*)props;
- (void)addGraph:(MAHealthGraph*)graph;
- (void)removeGraph:(MAHealthGraph*)graph;
- (void)setMenu:(NSMenu*)menu;
- (void)setProperties:(NSDictionary*)props;
- (NSSize)contentSize;
- (NSDictionary*)properties;

- (void)setPage:(MAHealthPage*)page;
- (MAHealthPage*)page;

- (NSArray*)graphs;
@end
