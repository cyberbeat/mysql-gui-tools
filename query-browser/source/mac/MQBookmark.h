//
//  MQBookmark.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/25/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"

extern NSString *MQBookmarkPboardType;

@interface MQBookmark : NSObject
{
  @public
  NSString *caption;
  NSString *catalog;
  NSString *schema;
  NSString *sql;
  MYX_Q_TYPE queryType;
  unsigned int accessCount;
  
  time_t ctime;
  time_t atime;
  time_t mtime;
}

- (id)initWithBookmark:(MYX_BOOKMARK*)bookmark;
- (NSString*)caption;
- (void)setCaption:(NSString*)caption;
- (NSString*)query:(BOOL)touch;
@end


@interface MQBookmarkGroup : NSObject
{
  @public
  NSMutableArray *items;
  NSString *caption;
}
- (id)initWithGroup:(MYX_BOOKMARK_GROUP*)group;
- (void)addBookmarkItem:(id)item atIndex:(int)index;
- (NSString*)caption;
- (void)setCaption:(NSString*)caption;
@end


@interface MQBookmarkList : NSObject 
{
  MQBookmarkGroup *_root;
  NSArray *_draggedItems;
  
  NSString *_file;
}

- (BOOL)loadFromFile:(NSString*)file;
- (void)storeToFile:(NSString*)file;
- (void)save;

- (MQBookmarkGroup*)addGroupInItem:(id)item;
- (void)addBookmark:(NSString*)name
           forQuery:(NSString*)query
         forCatalog:(NSString*)catalog
             schema:(NSString*)schema
           inFolder:(id)folder;
- (void)removeBookmark:(id)item;
- (NSArray*)getFolderList;

@end
