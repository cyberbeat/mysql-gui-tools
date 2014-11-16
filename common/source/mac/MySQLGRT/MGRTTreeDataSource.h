//
//  MGRTTreeDataSource.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <MySQLGRT/MGRT.h>

extern NSString *MGRTTreeNodePboardType;

@class MGRTNodeData;

@interface MGRTTreeDataSource : NSObject 
{
  GHashTable *_nodeTable;
    
  MGRT *_grt;
 
  NSImage *_listIcon;
  
  MYX_GRT_VALUE *_rootValue;
  MGRTNodeData *_rootNode;

  NSDictionary *_displayInfo;
  NSDictionary *_iconInfo;
}

- (void)setMGRT:(MGRT*)grt;
- (void)reset;
- (void)setRootValue:(MYX_GRT_VALUE*)root;
- (void)setDisplayInfo:(NSDictionary*)displayInfo;
- (void)setIconInfo:(NSDictionary*)displayInfo;
- (MYX_GRT_VALUE*)objectAtNode:(id)node;
@end


