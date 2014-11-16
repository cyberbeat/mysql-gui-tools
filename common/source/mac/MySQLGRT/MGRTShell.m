//
//  MGRTShell.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRTShell.h"
#import "MShellTextView.h"
#import "MGRTValueTreeDataSource.h"
#include "myx_grt_private.h"
#import <MySQLToolsCommon/MPtrNode.h>
#import <MySQLToolsCommon/mxUtils.h>


@implementation MGRTShell

static void releaseObject(gpointer data)
{
  [(id)data release];
}

- (id)initWithMGRT:(MGRT*)grt
{
  self= [super init];
  if (self)
  {
    NSBundle *bundle= [NSBundle bundleForClass:[self class]];
    
    _grt= [grt retain];
    _valueDS= [[MGRTValueTreeDataSource alloc] initWithMGRT:grt];
    [NSBundle loadNibNamed:@"GRTShell" owner:self];
    _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                        NULL, releaseObject);
    
    _packageIcon= [MXGetImageFromBundle(bundle, @"grt_value_dict.png") retain];
    _structIcon= [MXGetImageFromBundle(bundle, @"grt_value_struct.png") retain];
    _valueIcon= [MXGetImageFromBundle(bundle, @"grt_value_simple.png") retain];
    
    _moduleIcon= [MXGetImageFromBundle(bundle, @"grt_module.png") retain];
    _funcIcon= [MXGetImageFromBundle(bundle, @"grt_function.png") retain];    
  }
  return self;
}


- (void)dealloc
{
  [_packageIcon release];
  [_structIcon release];
  [_valueIcon release];
  [_moduleIcon release];
  [_funcIcon release];
  
  g_hash_table_destroy(_nodeTable);
  [_valueDS release];
  [_toolbar release];
  [_grt release];
  [super dealloc];
}


- (void)awakeFromNib
{
  [textView setGRTEnvironment:_grt];
  [_grt setConsole:textView];
  _toolbar= [[NSToolbar alloc] initWithIdentifier:@"tbar"];
  [_toolbar setSizeMode:NSToolbarSizeModeSmall];
  [_toolbar setDelegate:self];
  [window setToolbar:_toolbar];
  
  [valueTree setDataSource:_valueDS];
  [valueTree reloadData];
  
  [moduleInfo setStringValue:@""];
}

- (void)show:(id)sender
{
  [window makeKeyAndOrderFront:nil];
}

- (void)hide:(id)sender
{
  [window orderOut:nil];
}


- (IBAction)reloadTree:(id)sender
{
  id items= MXGetExpandedOutlineItems(valueTree, -1, [[valueTree tableColumns] objectAtIndex:0]);
  
  [_valueDS reset];
  [valueTree reloadData];
  
  g_hash_table_destroy(_nodeTable);
  _nodeTable= g_hash_table_new_full(g_direct_hash, g_direct_equal,
                                      NULL, releaseObject);  
  [structTree reloadData];
  [moduleTree reloadData];
  
  MXExpandOutlineItems(valueTree, items, [[valueTree tableColumns] objectAtIndex:0]);
}


- (IBAction)reloadItem:(id)sender
{
  int row= [valueTree selectedRow];
  id item;
  if (row < 0)
    return;
  item= [valueTree itemAtRow:row];
  g_hash_table_remove(_nodeTable, [item value]);
  [valueTree reloadItem:item reloadChildren:YES];
}


- (IBAction)reloadDetails:(id)sender
{
  [valueList reloadData];
}


- (IBAction)changeRoot:(id)sender
{
  NSString *root= [rootCombo stringValue];
  
  if ([root isEqualToString:@"GRT root"])
  {
    return;
  }
  [self reloadTree:nil];
}


- (NSArray *)toolbarAllowedItemIdentifiers: (NSToolbar *)toolbar 
{
  return [NSArray arrayWithObjects: @"RefreshAllItem", @"RefreshItem", nil];
}

- (NSArray *)toolbarDefaultItemIdentifiers: (NSToolbar *)toolbar 
{
  return [self toolbarAllowedItemIdentifiers:toolbar];
}


- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar
     itemForItemIdentifier:(NSString *)itemIdentifier
 willBeInsertedIntoToolbar:(BOOL)flag
{
  NSToolbarItem *toolbarItem = [[[NSToolbarItem alloc] initWithItemIdentifier:itemIdentifier] autorelease];

  if ([itemIdentifier isEqualToString: @"RefreshAllItem"]) 
  {
    [toolbarItem setImage:MXGetImageFromBundle([NSBundle bundleForClass:[self class]],@"refresh.png")];

    [toolbarItem setLabel:@"Reload"];
    [toolbarItem setPaletteLabel:@"Reload"];
    [toolbarItem setTarget:self];
    [toolbarItem setAction:@selector(reloadTree:)];
  }
  else if ([itemIdentifier isEqualToString: @"RefreshItem"]) 
  {
    [toolbarItem setImage:MXGetImageFromBundle([NSBundle bundleForClass:[self class]],@"refresh.png")];
    
    [toolbarItem setLabel:@"Reload Selected"];
    [toolbarItem setPaletteLabel:@"Reload Selected"];
    [toolbarItem setTarget:self];
    [toolbarItem setAction:@selector(reloadItem:)];
  }  
  else
    toolbarItem= nil;
  return toolbarItem;
}


- (void)setDetailValue:(MYX_GRT_VALUE*)value
{
  _detailValue= value;
  
  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
  {
    const char *dname= myx_grt_dict_struct_get_name(value);
    const char *name= myx_grt_dict_name_item_as_string(value);
    
    if (name)
      [valueText setStringValue:[NSString stringWithFormat:@"%s (%s)", name, dname]];
    else
      [valueText setStringValue:@"DICT"];
  }
  else if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
  {
    const char *dname= myx_grt_list_content_get_struct_name(value);
    const char *name= myx_get_value_type_as_string(myx_grt_list_content_get_type(value));
    if (dname)
      [valueText setStringValue:[NSString stringWithFormat:@"[%s: %s]", name, dname]];
    else
      [valueText setStringValue:[NSString stringWithFormat:@"[%s]", name]];
  }
  else
    [valueText setStringValue:@""];
  
  [valueList reloadData];
}


- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (outlineView == structTree)
  {
    if (!item)
      return myx_grt_package_count([_grt grt]);
    else if ([item tag] == 0)
      return myx_grt_package_struct_count([_grt grt], [item pointer]);
    else if ([item tag] == 1)
      return myx_grt_struct_get_member_count([item pointer]);
  }
  else if (outlineView == moduleTree)
  {
    if (!item)
      return myx_grt_module_get_count([_grt grt]);
    else if ([item tag]==0)
      return myx_grt_module_function_get_count([item pointer]);
  }  
  return 0;
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if (outlineView == structTree)
  {
    if ([item isKindOfClass:[MPtrNode class]])
      return [item caption];
    else
      return item;
  }
  else if (outlineView == moduleTree)
  {
    return [item caption];
  }  
  return nil;
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  if (outlineView == structTree)
  {
    if (!item)
      return YES;
    if ([item isKindOfClass:[MPtrNode class]])
    {
      if ([item tag] <= 1)
        return YES;
    }
  }
  else if (outlineView == moduleTree)
  {
    if (!item || [item tag]==0)
      return YES;
  }  
  return NO;
}

- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)parentItem
{
  if (outlineView == structTree)
  {
    if (!parentItem)
    {
      char *package= myx_grt_package_by_index([_grt grt], index);
      MPtrNode *node= g_hash_table_lookup(_nodeTable, package);
      if (!node)
      {
        node= [[MPtrNode alloc] init];
        [node setCaption:[NSString stringWithUTF8String:(package && *package)?package:"<base>"]];
        [node setPointer:package];
        [node setTag:0];
        g_hash_table_insert(_nodeTable, package, node);
      }
      return node;
    }
    else if ([parentItem tag] == 0)
    {
      MYX_GRT_STRUCT *gstruct= myx_grt_package_struct_by_index([_grt grt], [parentItem pointer], index);
      MPtrNode *node= g_hash_table_lookup(_nodeTable, gstruct);
      if (!node)
      {
        const char *caption;
        int inherited;
        
        caption= myx_grt_struct_get_caption([_grt grt], gstruct, &inherited);
        
        node= [[MPtrNode alloc] init];
        if (inherited && caption && *caption)
          [node setCaption:[NSString stringWithFormat:@"%s  <<%s>>",myx_grt_struct_get_name(gstruct),caption]];
        else
          [node setCaption:[NSString stringWithFormat:@"%s  (%s)",myx_grt_struct_get_name(gstruct),caption]];
        [node setPointer:gstruct];
        [node setTag:1];
        g_hash_table_insert(_nodeTable, gstruct, node);
      }
      return node;
    }
    else if ([parentItem tag] == 1)
    {
      MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index([parentItem pointer], index);
      MPtrNode *node= g_hash_table_lookup(_nodeTable, member);
      if (!node)
      {
        node= [[MPtrNode alloc] init];
        [node setCaption:[NSString stringWithFormat:@"%s: %s;", member->name, myx_get_value_type_as_string(myx_grt_struct_member_get_type(member))]];
        [node setPointer:member];
        [node setTag:2];
        g_hash_table_insert(_nodeTable, member, node);
      }
      return node;
    }
  }
  else if (outlineView == moduleTree)
  {
    if (!parentItem)
    {
      MYX_GRT_MODULE *module= myx_grt_module_get_by_index([_grt grt], index);
      MPtrNode *node= g_hash_table_lookup(_nodeTable, module);
      if (!node)
      {
        node= [[MPtrNode alloc] init];
        [node setCaption:[NSString stringWithUTF8String:module->name]];
        [node setPointer:module];
        [node setTag:0];
        g_hash_table_insert(_nodeTable, module, node);
      }
      return node;
    }
    else
    {
      MYX_GRT_FUNCTION *func= myx_grt_module_function_get_by_index([parentItem pointer], index);
      MPtrNode *node= g_hash_table_lookup(_nodeTable, func);
      if (!node)
      {
        node= [[MPtrNode alloc] init];
        [node setCaption:[NSString stringWithUTF8String:func->name]];
        [node setPointer:func];
        [node setTag:1];
        g_hash_table_insert(_nodeTable, func, node);
      }
      return node;
    }
  }  
  return nil;
}

- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
  if (outlineView == valueTree)
  {
    if ([[tableColumn identifier] isEqualToString:@"description"])
      [cell setImage:[item icon]];
  }
  else if (outlineView == structTree)
  {
    switch ([item tag])
    {
      case 0:
        [cell setImage:_packageIcon];
        break;
      case 1:
        [cell setImage:_structIcon];
        break;
      case 2:
        [cell setImage:_valueIcon];
        break;
    }
  }
  else if (outlineView == moduleTree)
  {
    if ([item tag] == 0)
      [cell setImage:_moduleIcon];
    else
      [cell setImage:_funcIcon];
  }
}


- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  if ([notification object] == valueTree)
  {
    int row= [valueTree selectedRow];
    if (row < 0)
      [self setDetailValue:NULL];
    else
      [self setDetailValue:[(MGRTValueNodeData*)[valueTree itemAtRow:row] value]];
  }
  else if ([notification object] == moduleTree)
  {
    int row= [moduleTree selectedRow];
    if (row < 0)
    {
      [moduleInfo setStringValue:@""];
      return;
    }
    MPtrNode *node= [moduleTree itemAtRow:row];
    MYX_GRT_MODULE *module;
    char *type;
    
    if ([node tag] == 0)
      module= [node pointer];
    else
    {
      MYX_GRT_FUNCTION *function= [node pointer];
      module= function->module;
    }
    
    switch (module->loader->loader_type)
    {
      case MYX_BUILTIN_MODULE_TYPE: type= "C Module"; break;
      case MYX_JAVA_MODULE_TYPE: type= "Java Module"; break;
      case MYX_LUA_MODULE_TYPE: type= "Lua Module"; break;
      case MYX_PYTHON_MODULE_TYPE: type= "Python Module"; break;
      default: type= "-"; break;
    }
    
    [moduleInfo setStringValue:[NSString stringWithFormat:@"Module: %s\nPath: %s\nExtends: %s\nType: %s",
                              module->name, module->path?:"", module->extends?:"", type]];
  }
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (_detailValue)
  {  
    if (myx_grt_value_get_type(_detailValue) == MYX_DICT_VALUE)
      return myx_grt_dict_item_count(_detailValue);
    else if (myx_grt_value_get_type(_detailValue) == MYX_LIST_VALUE)
      return myx_grt_list_item_count(_detailValue);
  }
  return 0;
}
    
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex
{
  if (_detailValue)
  {
    if (myx_grt_value_get_type(_detailValue) == MYX_DICT_VALUE)
    {
      if ([[aTableColumn identifier] isEqualToString:@"param"])
        return [NSString stringWithUTF8String:myx_grt_dict_item_key_by_index(_detailValue, rowIndex)];
      else
        return [NSString stringWithUTF8String:myx_grt_value_formated_as_string(myx_grt_dict_item_value_by_index(_detailValue, rowIndex))?:""];
    }
    else if (myx_grt_value_get_type(_detailValue) == MYX_LIST_VALUE)
    {
      if ([[aTableColumn identifier] isEqualToString:@"param"])
        return [NSString stringWithFormat:@"[%i]",rowIndex+1];
      else
        return [NSString stringWithUTF8String:myx_grt_value_formated_as_string(myx_grt_list_item_get(_detailValue, rowIndex))];
    }
  }
  return 0;
}


@end
