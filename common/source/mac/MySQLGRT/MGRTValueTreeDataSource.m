//
//  MGRTValueTreeDataSource.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTValueTreeDataSource.h"
#include <myx_grt_public_interface.h>

@implementation MGRTValueNodeData
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
  myx_grt_value_release(_value);
  [super dealloc];
}

- (void)setName:(NSString*)name
{
  [_name release];
  _name= [name retain];
}

- (int)count:(BOOL)containerOnly
{
  switch (myx_grt_value_get_type(_value))
  {
    case MYX_DICT_VALUE:
      if (containerOnly)
        return myx_grt_dict_item_count_complex(_value);
      else
        return myx_grt_dict_item_count(_value);
    case MYX_LIST_VALUE:
      return myx_grt_list_item_count(_value);
    default:
      return 0;
  }
}


- (MYX_GRT_VALUE*)value
{
  return _value;
}


- (NSString*)grtRefCount
{
  return [NSString stringWithFormat:@"%i", _myx_grt_get_refcount(_value)];
}


- (NSString*)description
{
  const char *sname= NULL;
  
  switch (myx_grt_value_get_type(_value))
  {
    case MYX_LIST_VALUE:
      sname= myx_grt_list_content_get_struct_name(_value);
      if (sname)
        return [NSString stringWithFormat:@"%@   list [dict: %s]", _name, sname];
      else
      {
        sname= myx_get_value_type_as_string(myx_grt_list_content_get_type(_value));
        return [NSString stringWithFormat:@"%@   list [%s]", _name, sname];
      }
        break;
    case MYX_DICT_VALUE:
      sname= myx_grt_dict_content_get_struct_name(_value);
      if (sname)
        return [NSString stringWithFormat:@"%@   dict: %s", _name, sname];
      else
        return [NSString stringWithFormat:@"%@   dict", _name];  
      break;
    case MYX_STRING_VALUE:
      return [NSString stringWithFormat:@"%s", myx_grt_value_as_string(_value)];
    case MYX_INT_VALUE:
      return [NSString stringWithFormat:@"%i", myx_grt_value_as_int(_value)];
    case MYX_REAL_VALUE:
      return [NSString stringWithFormat:@"%f", myx_grt_value_as_real(_value)];
    default:
      break;
  }
  
  return @"bug!?";
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
@end





@implementation MGRTValueTreeDataSource


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
    _displayObjectValues= NO;
    _iconPath= [[[NSBundle bundleForClass:[self class]] resourcePath] retain];
    
    _dictIcon= [[NSImage alloc] initWithContentsOfFile:[NSString stringWithFormat:@"%@/grt_value_dict.png",_iconPath]];
    _listIcon= [[NSImage alloc] initWithContentsOfFile:[NSString stringWithFormat:@"%@/grt_value_list.png",_iconPath]];
    _simpleIcon= [[NSImage alloc] initWithContentsOfFile:[NSString stringWithFormat:@"%@/grt_value_simple.png",_iconPath]];    
  }
  return self;
}


- (id)initWithMGRT:(MGRT*)grt
{
  self= [self init];
  
  _grt= [grt retain];
  
  return self;
}



- (void)setMGRT:(MGRT*)grt
{
  [_grt release];
  _grt= [grt retain];
}



- (void)dealloc
{
  g_hash_table_destroy(_nodeTable);
  [_simpleIcon release];
  [_dictIcon release];
  [_listIcon release];
  [_iconPath release];  
  [_grt release];
  [super dealloc];
}


- (void)reset
{
  g_hash_table_destroy(_nodeTable);
  _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                    NULL, releaseObject);  
}

- (void)setupIcon:(MGRTValueNodeData*)node
{
  char *path;
  switch (myx_grt_value_get_type([node value]))
  {
    case MYX_DICT_VALUE:
    {
      MYX_GRT_STRUCT *vstr= myx_grt_dict_struct_get([_grt grt], [node value]);
      
      if (vstr)
        path= myx_grt_struct_get_icon_path([_grt grt], vstr, MYX_IT_SMALL);
      else
        path= NULL;
      if (path)
      {
        NSImage *icon= [[NSImage alloc] initWithContentsOfFile:[NSString stringWithFormat:@"%@/%s",_iconPath,path]];
        if (!icon)
          icon= [[NSImage alloc] initWithContentsOfFile:[NSString stringWithFormat:@"%@/GrtObject.16x16.png",_iconPath]];
        [node setIcon:icon];
        [icon release];
        g_free(path);
      }
      else
        [node setIcon:_dictIcon];
      break;
    }
    case MYX_LIST_VALUE:
      [node setIcon:_listIcon];
      break;
    default:
      [node setIcon:_simpleIcon];
      break;
  }
}



- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (!item)
    return 1;
    
  return [item count:!_displayObjectValues];
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if ([[tableColumn identifier] isEqualTo:@"refcount"])
    return [item grtRefCount];
  else
    return [item description];
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  return item==nil || [item count:!_displayObjectValues]>0;
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)parentItem
{
  static int root;
  MGRTValueNodeData *parentNode= (MGRTValueNodeData*)parentItem;  
  MGRTValueNodeData *node;
  
  if (!parentNode)
  {
    node= g_hash_table_lookup(_nodeTable, &root);
    if (!node)
    {
      if (!_rootPath)
      {
        node= [[MGRTValueNodeData alloc] initWithValue:myx_grt_get_root([_grt grt])];
        [node setName:@"root"];
      }
      else
      {
        MYX_GRT_VALUE *rootVal= myx_grt_get_root([_grt grt]);
        MYX_GRT_VALUE *item;
        
        item= myx_grt_dict_item_get_by_path([_grt grt], rootVal, [_rootPath UTF8String]);
        if (!item)
        {
          node= [[MGRTValueNodeData alloc] initWithValue:myx_grt_get_root([_grt grt])];
          [node setName:@"root"];
        }
        else
        {
          node= [[MGRTValueNodeData alloc] initWithValue:item];
          [node setName:[NSString stringWithUTF8String:myx_grt_dict_name_item_as_string(item)]];
        }
      }
      [self setupIcon:node];
      g_hash_table_insert(_nodeTable, &root, node);
    }
  }
  else
  {
    MYX_GRT_VALUE *parentValue= [parentNode value];
    MYX_GRT_VALUE *value= NULL;
    const char *key;
    
    switch (myx_grt_value_get_type(parentValue))
    {
      case MYX_DICT_VALUE:
      {
        if (_displayObjectValues)
          myx_grt_dict_item_by_index(parentValue, index, &key, &value);
        else
        {
          key= myx_grt_dict_item_key_by_index_complex(parentValue, index);
          value= myx_grt_dict_item_value_by_index_complex(parentValue, index);
        }
        node= g_hash_table_lookup(_nodeTable, value);
        if (!node)
        {
          node= [[MGRTValueNodeData alloc] initWithValue:value];
          [node setName:[NSString stringWithUTF8String:key]];
          [self setupIcon:node];
          g_hash_table_insert(_nodeTable, value, node);
        }
        break;
      }
      case MYX_LIST_VALUE:
      {
        value= myx_grt_list_item_get(parentValue, index);
        node= g_hash_table_lookup(_nodeTable, value);
        if (!node)
        {
          // if this is a ref _id, then get the referenced value
          if (myx_grt_value_get_type(value) == MYX_STRING_VALUE
              && myx_grt_list_content_get_struct_name(parentValue))
          {
            MYX_GRT_VALUE *rvalue= myx_grt_reference_cache_lookup([_grt grt], myx_grt_value_as_string(value));
            
            node= [[MGRTValueNodeData alloc] initWithValue:rvalue];
          }
          else
            node= [[MGRTValueNodeData alloc] initWithValue:value];
          
          if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
          {
            const char *name= myx_grt_dict_name_item_as_string(value);
            if (name)
              [node setName:[NSString stringWithUTF8String:name]];
          }
          [self setupIcon:node];
          g_hash_table_insert(_nodeTable, value, node);
        }
        break;
      }
      default:
        return nil;
    }
  }
  return node;
}

- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
  if ([[tableColumn identifier] isEqualToString:@"description"])
    [cell setImage:[item icon]];
}

- (void)setRoot:(NSString*)path
{
  if (_rootPath != path)
  {
    [_rootPath release];
    _rootPath= [path retain];
  }
}

@end
