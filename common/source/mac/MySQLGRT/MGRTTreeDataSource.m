//
//  MGRTTreeDataSource.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTTreeDataSource.h"
#include <myx_grt_public_interface.h>
#import <MySQLToolsCommon/myxutil.h>

NSString *MGRTTreeNodePboardType= @"MGRTTreeNodePboardType";

@interface MGRTNodeData : NSObject
{
  MYX_GRT_VALUE *_value;
  NSString *_name;
  NSImage *_icon;
  NSArray *_dictKeysToDisplay;
}
- (id)initWithValue:(MYX_GRT_VALUE*)value;
- (void)setDictKeysToDisplay:(NSArray*)keys;
- (int)count;
- (void)setTitle:(NSString*)name;
- (NSString*)description;
- (MYX_GRT_VALUE*)value;
- (MYX_GRT_VALUE*)childAtIndex:(int)index;
- (NSString*)childNameAtIndex:(int)index;
- (const char*)grtId;
- (BOOL)isExpandable;
- (void)setIcon:(NSImage*)icon;
- (NSImage*)icon;
- (BOOL)isList;
- (NSString*)dictStructName;
@end;


@implementation MGRTNodeData
- (id)initWithValue:(MYX_GRT_VALUE*)value
{
  self= [super init];
  if (self)
    _value= myx_grt_value_retain(value);
  return self;
}

- (void)dealloc
{
  [_icon release];
  [_name release];
  [_dictKeysToDisplay release];
  myx_grt_value_release(_value);
  [super dealloc];
}

- (void)setTitle:(NSString*)name
{
  [_name release];
  _name= [name retain];
}

- (void)setDictKeysToDisplay:(NSArray*)keys
{
  if (_dictKeysToDisplay != keys)
  {
    [_dictKeysToDisplay release];
    _dictKeysToDisplay= [keys retain];
  }
}


- (BOOL)isList
{
  if (myx_grt_value_get_type(_value) == MYX_LIST_VALUE)
    return YES;
  return NO;
}

- (int)count
{
  switch (myx_grt_value_get_type(_value))
  {
    case MYX_LIST_VALUE:
      return myx_grt_list_item_count(_value);
    case MYX_DICT_VALUE:
    {
      int i;
      int count= 0;
      for (i= 0; i < [_dictKeysToDisplay count]; i++)
      {
        NSString *key= [_dictKeysToDisplay objectAtIndex:i];
        MYX_GRT_VALUE *value;

        value= myx_grt_dict_item_get_value(_value, [key UTF8String]);
        if (value)
          count++;
      }
      return count;
    }
  }
  return 0;
}


- (NSString*)childNameAtIndex:(int)index
{
  return [_dictKeysToDisplay objectAtIndex:index];
}


- (MYX_GRT_VALUE*)childAtIndex:(int)index
{
  if (myx_grt_value_get_type(_value) == MYX_LIST_VALUE)
    return myx_grt_list_item_get(_value, index);
  else
  {
    int i;
    int count= 0;
    for (i= 0; i < [_dictKeysToDisplay count]; i++)
    {
      NSString *key= [_dictKeysToDisplay objectAtIndex:i];
      MYX_GRT_VALUE *value;
      
      value= myx_grt_dict_item_get_value(_value, [key UTF8String]);
      if (value)
      {
        if (count == index)
          return value;
        count++;
      }
    }
  }
  return NULL;
}


- (const char*)grtId
{
  return myx_grt_dict_id_item_as_string(_value);
}


- (MYX_GRT_VALUE*)value
{
  return _value;
}


- (NSString*)dictStructName
{
  if (myx_grt_value_get_type(_value) == MYX_DICT_VALUE)
    return NSStr(myx_grt_dict_struct_get_name(_value));
  else
    return nil;
}

- (NSString*)description
{
  if (myx_grt_value_get_type(_value) == MYX_DICT_VALUE)
    return NSStr(myx_grt_dict_name_item_as_string(_value));
  else
    return _name;
}


- (void)setIcon:(NSImage*)icon
{
  if (_icon != icon)
  {
    [_icon release];
    _icon= [icon retain];
  }
}

- (NSImage*)icon
{
  return _icon;
}

- (BOOL)isExpandable
{
  if (myx_grt_value_get_type(_value) == MYX_LIST_VALUE)
    return YES;
  else if (myx_grt_value_get_type(_value) == MYX_DICT_VALUE)
    return _dictKeysToDisplay ? YES : NO;
  else
    return NO;
}
@end





@implementation MGRTTreeDataSource


static void releaseObject(gpointer object)
{
  [(id)object release];
}


- (id)init
{
  self= [super init];
  if (self)
  {
    _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                      NULL, releaseObject);
    
    _listIcon= [[NSImage alloc] initWithContentsOfFile:[[NSBundle bundleForClass:[self class]] pathForImageResource:@"grt_value_list"]];
  }
  return self;
}


- (void)setDisplayInfo:(NSDictionary*)displayInfo
{
  if (_displayInfo != displayInfo)
  {
    [_displayInfo release];
    _displayInfo= [displayInfo retain];
  }
}


- (void)setIconInfo:(NSDictionary*)iconInfo
{
  if (_iconInfo != iconInfo)
  {
    [_iconInfo release];
    _iconInfo= [iconInfo retain];
  }  
}


- (void)setMGRT:(MGRT*)grt
{
  [_grt release];
  _grt= [grt retain];
}



- (void)dealloc
{
  g_hash_table_destroy(_nodeTable);
  [_grt release];
  [_listIcon release];
  [_iconInfo release];
  [_displayInfo release];
  [super dealloc];
}


- (void)reset
{
  g_hash_table_destroy(_nodeTable);
  _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                    NULL, releaseObject);  
}


- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  int c;
  if (!item)
    c= [_rootNode count];
  else
    c= [item count];
  return c;
}


- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  return [item description];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  return item==nil || [item isExpandable];
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)parentItem
{
  if (!parentItem)
    parentItem= _rootNode;
  
  MYX_GRT_VALUE *child= [parentItem childAtIndex:index];
  id item= g_hash_table_lookup(_nodeTable, child);
  
  if (!item)
  {
    item= [[MGRTNodeData alloc] initWithValue:child];
    if ([item isList])
      [item setTitle:[parentItem childNameAtIndex:index]];

    [item setDictKeysToDisplay:[_displayInfo objectForKey:[item dictStructName]]];
    g_hash_table_insert(_nodeTable, child, item);
  }
  return item;
}

- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
  if ([[tableColumn identifier] isEqualToString:@"description"])
  {
    if ([item isList])
      [cell setImage:_listIcon];
    else
      [cell setImage:[_iconInfo objectForKey:[item dictStructName]]];
  }
}


- (BOOL)outlineView:(NSOutlineView *)outlineView writeItems:(NSArray *)items toPasteboard:(NSPasteboard *)pboard
{
  NSMutableArray *data= [NSMutableArray arrayWithCapacity:[items count]];
  int i;
  for (i= 0; i < [items count]; i++)
  {
    id item= [items objectAtIndex:i];
    if (![item isList])
      [data addObject:NSStr([item grtId])];
  }
  
  [pboard declareTypes:[NSArray arrayWithObject:MGRTTreeNodePboardType] owner:nil];
  [pboard setPropertyList:data forType:MGRTTreeNodePboardType];
  
  return YES;
}

- (void)setRootValue:(MYX_GRT_VALUE*)root
{
  _rootValue= root;
  
  g_hash_table_destroy(_nodeTable);
  _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                    NULL, releaseObject);

  _rootNode= [[MGRTNodeData alloc] initWithValue:root];
//  [_rootNode setDictKeysToDisplay:[_displayInfo objectForKey:[_rootNode dictStructName]]];
  g_hash_table_insert(_nodeTable, root, _rootNode);
  
}


- (MYX_GRT_VALUE*)objectAtNode:(id)node
{
  return [(MGRTNodeData*)node value];
}

@end
