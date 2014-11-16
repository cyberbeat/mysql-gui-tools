//
//  MGRTValueTreeDataSource.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

// DataSource for displaying a GRT subtree structure, including
// information such as data types.

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRT.h>

@interface MGRTValueNodeData : NSObject
{
  MYX_GRT_VALUE *_value;
  NSString *_name;
  NSImage *_icon;
}
- (id)initWithValue:(MYX_GRT_VALUE*)value;
- (int)count:(BOOL)containerOnly;
- (void)setName:(NSString*)name;
- (NSString*)description;
- (MYX_GRT_VALUE*)value;
- (void)setIcon:(NSImage*)icon;
- (NSImage*)icon;
@end;


@interface MGRTValueTreeDataSource : NSObject 
{
  NSString *_iconPath;
  GHashTable *_nodeTable;
  
  NSImage *_dictIcon;
  NSImage *_listIcon;
  NSImage *_simpleIcon;
  
  MGRT *_grt;
  
  NSString *_rootPath;

  BOOL _displayObjectValues;
}

- (id)initWithMGRT:(MGRT*)grt;
- (void)setMGRT:(MGRT*)grt;
- (void)reset;
- (void)setRoot:(NSString*)path;

@end
