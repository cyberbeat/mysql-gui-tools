//
//  MQBookmark.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/25/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQBookmark.h"
#include <MySQLToolsCommon/myxUtil.h>
#import <MySQLToolsCommon/NSArray_extras.h>
#include <time.h>

NSString *MQBookmarkPboardType= @"MQBookmarkPboardType";
NSString *MQBookmarkItemPboardType= @"MQBookmarkItemPboardType";

@implementation MQBookmark
- (id)initWithBookmark:(MYX_BOOKMARK*)bookmark
{
  self= [super init];
  if (self)
  {
    struct tm t;
    
	caption= [NSStr(bookmark->caption) retain];
	catalog= [NSStr(bookmark->catalog) retain];
	schema= [NSStr(bookmark->schema) retain];
	sql= [NSStr(bookmark->sql) retain];
	queryType= bookmark->query_type;
	accessCount= bookmark->access_count;

    if (bookmark->date_created)
    {
      strptime(bookmark->date_created, "%F %T", &t);
      ctime= mktime(&t);
    }
    if (bookmark->date_last_access)
    {
      strptime(bookmark->date_last_access, "%F %T", &t);
      atime= mktime(&t);
    }
    if (bookmark->date_modified)
    {
      strptime(bookmark->date_modified, "%F %T", &t);
      mtime= mktime(&t);
    }
  }
  return self;
}

- (void)dealloc
{
  [caption release];
  [catalog release];
  [schema release];
  [sql release];
  [super dealloc];
}

- (NSString*)caption
{
  return caption;
}

- (void)setCaption:(NSString*)capt
{
  if (capt != caption)
  {
    [caption release];
    caption= [capt retain];
    mtime= time(NULL);
  }
}

- (NSString*)query:(BOOL)touch
{
  if (touch)
    atime= time(NULL);
  accessCount++;
  return sql;
}

- (NSString*)description
{
  atime= time(NULL);
  accessCount++;
  return sql;
}
@end


@implementation MQBookmarkGroup
- (id)init
{
  self= [super init];
  if (self)
	items= [[NSMutableArray alloc] init];
  return self;
}
	
- (id)initWithGroup:(MYX_BOOKMARK_GROUP*)group
{
  self= [super init];
  if (self)
  {
    unsigned int i;
	items= [[NSMutableArray alloc] init];
    caption= [NSStr(group->caption) retain];
    
    for (i= 0; i < group->bookmark_groups_num; i++)
    {
      MQBookmarkGroup *bmg= [[MQBookmarkGroup alloc] initWithGroup:group->bookmark_groups+i];
      [items addObject:bmg];
      [bmg release];
    }

    for (i= 0; i < group->bookmarks_num; i++)
    {
      id bmk= [[MQBookmark alloc] initWithBookmark:group->bookmarks+i];
      [items addObject:bmk];
      [bmk release];
    }    
  }
  return self;
}

- (NSString*)caption
{
  return caption;
}

- (void)setCaption:(NSString*)capt
{
  if (caption != capt)
  {
    [caption release];
    caption= [capt retain];
  }
}

- (void)addBookmarkItem:(id)item atIndex:(int)index
{
  if (index < 0)
    [items addObject:item];
  else
    [items insertObject:item atIndex:index];
}

- (void)dealloc
{
  [items release];
  [caption release];
  [super dealloc];
}
@end

//-----------------------------------------------------------------------------

@implementation MQBookmarkList
- (id)init
{
  self= [super init];
  if (self)
  {
    _root= [[MQBookmarkGroup alloc] init];
    [_root setCaption:@"Bookmarks"];
  }
  return self;
}

- (BOOL)loadFromFile:(NSString*)file
{
  MYX_LIB_ERROR error;
  MYX_BOOKMARKS *bookmarks;
  bookmarks= myx_bookmarks_load([file fileSystemRepresentation], &error);

  if (bookmarks)
  {
    [_root release];
    _root= [[MQBookmarkGroup alloc] initWithGroup:bookmarks->bookmark_groups];
    myx_bookmarks_free(bookmarks);
    _file= [file retain];
    return YES;
  }
  else 
    NSLog(@"Error loading bookmarks from %@: %i", file, error);
  return NO;
}

- (void)save
{
  [self storeToFile:_file];
}


static void convertBookmark(MYX_BOOKMARK *mark, MQBookmark *bm)
{
  char buffer[100];
  mark->caption= g_strdup([bm->caption UTF8String]);
  mark->pos= 0;
  
  mark->catalog= g_strdup([bm->catalog UTF8String]);
  mark->schema= g_strdup([bm->schema UTF8String]);
  
  mark->sql= g_strdup([bm->sql UTF8String]);
  
  mark->access_count= bm->accessCount;
  
  strftime(buffer, sizeof(buffer), "%F %T", gmtime(&bm->ctime));
  mark->date_created= g_strdup(buffer);
  strftime(buffer, sizeof(buffer), "%F %T", gmtime(&bm->mtime));
  mark->date_modified= g_strdup(buffer);
  strftime(buffer, sizeof(buffer), "%F %T", gmtime(&bm->atime));
  mark->date_last_access= g_strdup(buffer);
}

static void convertBookmarkGroup(MYX_BOOKMARK_GROUP *group, MQBookmarkGroup *bm)
{
  unsigned int i, c= [bm->items count];
  unsigned int g, b;
  
  group->caption= g_strdup([[bm caption] UTF8String]);
  group->pos= 0;
  group->bookmarks_num= 0;
  group->bookmark_groups_num= 0;
  for (i= 0; i < c; i++)
  {
    if ([[bm->items objectAtIndex:i] isKindOfClass:[MQBookmarkGroup class]])
      group->bookmark_groups_num++;
    else
      group->bookmarks_num++;
  }
  group->bookmarks= g_new0(MYX_BOOKMARK, group->bookmarks_num);
  group->bookmark_groups= g_new0(MYX_BOOKMARK_GROUP, group->bookmark_groups_num);
  b= 0;
  g= 0;
  for (i= 0; i < c; i++)
  {
    id item= [bm->items objectAtIndex:i];
    if ([item isKindOfClass:[MQBookmarkGroup class]])
      convertBookmarkGroup(group->bookmark_groups+g++, item);
    else
      convertBookmark(group->bookmarks+b++, item);
  }
}


- (void)storeToFile:(NSString*)file
{
  MYX_BOOKMARKS *bookmarks;
  MYX_LIB_ERROR error;

  bookmarks= g_new0(MYX_BOOKMARKS, 1);
  
  bookmarks->bookmark_groups_num= 1;
  bookmarks->bookmark_groups= g_new0(MYX_BOOKMARK_GROUP, 1);
  convertBookmarkGroup(bookmarks->bookmark_groups, _root);
  
  error= myx_bookmarks_store([file fileSystemRepresentation],bookmarks);
  myx_bookmarks_free(bookmarks);
  
  if (error != MYX_NO_ERROR)
    NSLog(@"Error saving bookmarks to file '%@'", file);
}


- (void)dealloc
{
  [_file release];
  [_root release];
  [super dealloc];
}


static id findParent(id item, MQBookmarkGroup *group)
{
  unsigned int i, c= [group->items count];
  id parent;
  
  if (item == group)
    return nil;
  
  for (i= 0; i < c; i++)
  {
    id obj= [group->items objectAtIndex:i];
    if (item == obj)
      return group;
    else if ([obj isKindOfClass: [MQBookmarkGroup class]])
    {
      parent= findParent(item, obj);
      if (parent)
        return parent;
    }
  }
  return nil;
}


- (MQBookmarkGroup*)addGroupInItem:(id)item
{
  MQBookmarkGroup *group= [[MQBookmarkGroup alloc] init];
  [group setCaption:@"new group"];
  
  if (!item)
    item= _root;
  else if ([item isKindOfClass:[MQBookmark class]])
    item= findParent(item, _root);
  [item addBookmarkItem:group atIndex:-1];
  [group autorelease];
  
  return group;
}


- (void)addBookmark:(NSString*)name
           forQuery:(NSString*)query
         forCatalog:(NSString*)catalog
             schema:(NSString*)schema
           inFolder:(id)folder
{
  MQBookmark *bmk= [[MQBookmark alloc] init];
  
  bmk->caption= [name retain];
  bmk->catalog= [catalog retain];
  bmk->schema= [schema retain];
  bmk->sql= [query retain];
  bmk->accessCount= 0;
  
  bmk->ctime= time(NULL);
  bmk->atime= 0;
  bmk->mtime= 0;
  [(MQBookmarkGroup*)folder addBookmarkItem:bmk atIndex:-1];
  [bmk release];
}

static void addFolders(NSMutableArray *array,
                       MQBookmarkGroup *group)
{
  unsigned int i, c= [group->items count];
  
  [array addObject:group];
  
  for (i= 0; i < c; i++)
  {
    id item= [group->items objectAtIndex:i];
    
    if ([item isKindOfClass:[MQBookmarkGroup class]])
      addFolders(array, item);
  }
}

- (NSArray*)getFolderList
{
  NSMutableArray *array= [NSMutableArray arrayWithCapacity:8];
  
  addFolders(array, _root);
  
  return array;
}


- (void)removeBookmark:(id)item
{
  MQBookmarkGroup *parent= findParent(item, _root);

  if (!parent)
    parent= _root;
  
  [parent->items removeObject:item];
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
  if (item == nil)
	return [_root->items objectAtIndex:index];
  else
	return [((MQBookmarkGroup*)item)->items objectAtIndex:index];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  if (item == nil)
	return YES;
  else if ([item isKindOfClass: [MQBookmarkGroup class]])
	return YES;
  else
	return NO;
}

- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (item == nil)
	return [_root->items count];
  else if ([item isKindOfClass: [MQBookmarkGroup class]])
	return [((MQBookmarkGroup*)item)->items count];
  else
	return 0;
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  return [item caption];
}

- (void)outlineView:(NSOutlineView *)outlineView setObjectValue:(id)value
   forTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  [item setCaption:value];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView
         writeItems:(NSArray*)items toPasteboard:(NSPasteboard*)pboard 
{
  BOOL isItem= NO;
  
  if ([items count] == 1 && [[items objectAtIndex:0] isKindOfClass:[MQBookmark class]])
    isItem= YES;
  
  if (isItem)
    [pboard declareTypes:[NSArray arrayWithObjects:MQBookmarkItemPboardType,MQBookmarkPboardType,NSStringPboardType,nil]
                   owner:nil];
  else
    [pboard declareTypes:[NSArray arrayWithObjects:MQBookmarkItemPboardType,NSStringPboardType,nil]
                   owner:nil];
  
  // data doesn't matter, this dragged type shouldn't matter to anyone else
  _draggedItems= items;
  [pboard setData:[NSData data] forType:MQBookmarkItemPboardType];

  if (isItem)
    [pboard setString:[[items lastObject] query:NO] forType:MQBookmarkPboardType];
  
  [pboard setString:[[items arrayWithResultsOfSelector:@selector(description)]
                             componentsJoinedByString:@"\n"] forType:NSStringPboardType];
  return YES;
}


static BOOL isDescendant(MQBookmarkGroup *sgroup, MQBookmarkGroup *tgroup)
{
  unsigned int i, c= [tgroup->items count];
  
  if (sgroup == tgroup)
    return YES;
  
  for (i= 0; i < c; i++)
  {
    id item= [tgroup->items objectAtIndex:i];
    if ([item isKindOfClass:[MQBookmarkGroup class]])
    {
      if (isDescendant(sgroup, item))
        return YES;
    }
  }
  return NO;
}


- (unsigned int)outlineView:(NSOutlineView*)outlineView
               validateDrop:(id <NSDraggingInfo>)info
               proposedItem:(id)item
         proposedChildIndex:(int)childIndex 
{
  // cant drop on top of another bookmark
  if (childIndex == NSOutlineViewDropOnItemIndex && ![item isKindOfClass:[MQBookmarkGroup class]])
    return NSDragOperationNone;
  else if (outlineView == [info draggingSource] && [item isKindOfClass:[MQBookmarkGroup class]])
  {
    NSArray *nodes= ((MQBookmarkList*)[[info draggingSource] dataSource])->_draggedItems;
    unsigned int i, c= [nodes count];
    // check if we're dragging to inside ourself
    for (i= 0; i < c; i++)
      if ([[nodes objectAtIndex:i] isKindOfClass:[MQBookmarkGroup class]] && 
          isDescendant(item, [nodes objectAtIndex:i]))
        return NSDragOperationNone;
  } 

  return NSDragOperationGeneric;
}

- (BOOL)outlineView:(NSOutlineView*)outlineView
         acceptDrop:(id <NSDraggingInfo>)info
               item:(id)targetItem
         childIndex:(int)childIndex 
{
  NSPasteboard *pboard= [info draggingPasteboard];

  if ([pboard availableTypeFromArray:[NSArray arrayWithObject:MQBookmarkItemPboardType]])
  {
    NSArray *nodes= ((MQBookmarkList*)[[info draggingSource] dataSource])->_draggedItems;

    if (childIndex == NSOutlineViewDropOnItemIndex)
    {
      unsigned int i, c= [nodes count];
      for (i= 0; i < c; i++)
      {
        id item= [nodes objectAtIndex:i];
        MQBookmarkGroup *parent= findParent(item, _root);
        [item retain];
        [parent->items removeObject:item];
        [targetItem addBookmarkItem:item atIndex:-1];
        [item release];
      }
    }
    else
    {
      unsigned int i, c= [nodes count];
      for (i= 0; i < c; i++)
      {
        id item= [nodes objectAtIndex:i];
        int oi;
        MQBookmarkGroup *parent= findParent(item, _root);
        [item retain];
        
        oi= [parent->items indexOfObject:item];
        if (!targetItem)
          targetItem= _root;
        
        if (targetItem != parent)
        {
          [parent->items removeObject:item];
          [targetItem addBookmarkItem:item atIndex:childIndex];
        }
        else
        {
          [parent->items removeObjectAtIndex:oi];
          if (oi <= childIndex)
            childIndex--;
          [parent addBookmarkItem:item atIndex:childIndex];
        }

        [item release];
      }
    }
    [outlineView reloadData];
  }
  else
  {
    MQBookmark *bmk= [[MQBookmark alloc] init];
    int index;
	bmk->caption= @"new bookmark";
	bmk->catalog= nil; //XXX put the current catalog and schema
	bmk->schema= nil;
	bmk->sql= [[pboard stringForType:NSStringPboardType] retain];
	bmk->queryType= 0;
	bmk->accessCount= 0;

    if (!targetItem)
      targetItem= _root;

    // string
    if (childIndex == NSOutlineViewDropOnItemIndex)
    {
      index= 0;
      [targetItem addBookmarkItem:bmk atIndex:-1];
    }
    else
    {
      index= childIndex;
      [targetItem addBookmarkItem:bmk atIndex:childIndex];
    }
    [outlineView reloadData];
    [outlineView selectRow:index byExtendingSelection:NO];
    [outlineView editColumn:0 row:index withEvent:nil select:YES];
  }
  return YES;
}

@end
