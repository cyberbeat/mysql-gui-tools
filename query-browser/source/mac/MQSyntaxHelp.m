//
//  MQSyntaxHelp.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQSyntaxHelp.h"
#include <myx_public_interface.h>
#include <myx_qb_public_interface.h>

#import <MySQLToolsCommon/MPtrNode.h>

@implementation MQSyntaxHelp

- (void) fillArray: (NSMutableArray *)arr forGroup: (MYX_SQL_FUNCTIONGROUP *)parent_group
{
  unsigned f;

  [arr addObject:[NSString stringWithUTF8String: parent_group->caption]];

  for (f= 0; f < parent_group->functions_num; f++)
  {
    MYX_SQL_FUNCTION *func= parent_group->functions+f;
    MPtrNode *node= [[MPtrNode alloc] init];
    [node setCaption: [NSString stringWithUTF8String: func->caption]];
    [node setPointer: g_strdup(func->id) destroyer: (MPtrFree)g_free];
    [arr addObject: node];
    [node release];
  }

  for(f= 0; f < parent_group->subgroups_num; f++)
  {
    struct MYX_SQL_FUNCTIONGROUP *subgroup= parent_group->subgroups+f;
    NSMutableArray *subarray= [[NSMutableArray alloc] init];
    MPtrNode *node= [[MPtrNode alloc] init];
    [arr addObject:node];
    [node release];
    [self fillArray: subarray forGroup: subgroup];
    [node setSubnodes: subarray];
  }
}

- (void)loadFile:(NSString*)file
{
  NSString *path= [[NSBundle mainBundle] pathForResource:file
                                                  ofType:@"xml"];
  MYX_LIB_ERROR error;
  MYX_SQL_FUNCTIONINDEX *index;
  unsigned int g;

  
  // _index
  //    [ [group_caption func_node(name,id) func_node(name,id) ...],
  //      ...]

  index= myx_load_sql_function_list([path fileSystemRepresentation], &error);
  if (index)
  {
    _index= [[NSMutableArray alloc] initWithCapacity:index->groups_num];
    for (g= 0; g < index->groups_num; g++)
    {
      MYX_SQL_FUNCTIONGROUP *group= index->groups+g;
      NSMutableArray *groupArray= [NSMutableArray arrayWithCapacity:group->functions_num+1];
      
      //[groupArray addObject:[NSString stringWithUTF8String:group->caption]];
      [self fillArray: groupArray forGroup: group];

/*      
      for (f= 0; f < group->functions_num; f++)
      {
        MYX_SQL_FUNCTION *func= group->functions+f;
        MPtrNode *node= [[MPtrNode alloc] init];
        [node setCaption:[NSString stringWithUTF8String:func->caption]];
        [node setPointer:g_strdup(func->id) destroyer:(MPtrFree)g_free];
        [groupArray addObject:node];
        [node release];
      }

      if (f= 0; f < group->subgroups_num; f++)
      {
        MYX_SQL_FUNCTIONGROUP *subgroup= group->subgroups[f];
      }
*/      
      [_index addObject:groupArray];
    }
    myx_free_sql_function_list(index);
  }
}

- (void)setGroupIcon:(NSImage*)groupIcon
            itemIcon:(NSImage*)icon
{
  _groupIcon= [groupIcon retain];
  _icon= [icon retain];
}

- (void)dealloc
{
  [_groupIcon release];
  [_icon release];
  [_index release];
  [super dealloc];
}

- (const char*)topicIdForItem:(id)item
{
  if (![item isKindOfClass:[NSArray class]])
    return [item pointer];
  else
    return NULL;
}

- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if ([item isKindOfClass:[NSArray class]])
  {
    return [item objectAtIndex:0];
  }
  else if([item isKindOfClass:[MPtrNode class]])
  {
    if([(MPtrNode *)item subnodeCount] > 0)
      return [[(MPtrNode *)item subnodes] objectAtIndex:0];
    else
      return [item caption];
  }
  else 
  {
    return @"";
  }
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
  if (!item)
    return [_index objectAtIndex:index];
  else if ([item isKindOfClass:[NSArray class]])
    return [item objectAtIndex:index+1];
   else if(([item isKindOfClass:[MPtrNode class]]) && ([(MPtrNode *)item subnodeCount] > 0))
    return [[(MPtrNode *)item subnodes] objectAtIndex: index+1];
  else
    return nil;
}


- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  if (!item)
    return YES;
  else if ([item isKindOfClass:[NSArray class]])
    return YES;
  else if(([item isKindOfClass:[MPtrNode class]]) && ([(MPtrNode *)item subnodeCount] > 0))
    return YES;
  else
    return NO;
}


- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (!item)
    return [_index count];
  else if ([item isKindOfClass:[NSArray class]])
    return [item count]-1;
  else if([item isKindOfClass:[MPtrNode class]])
    return [(MPtrNode *)item subnodeCount]-1;
  else
    return 0;
}


- (void)outlineView:(NSOutlineView *)outlineView willDisplayCell:(id)cell forTableColumn:(NSTableColumn *)tableColumn item:(id)item
{
  if ([item isKindOfClass:[NSArray class]])
    [cell setImage:_groupIcon];
  else if(([item isKindOfClass:[MPtrNode class]]) && ([(MPtrNode *)item subnodeCount] > 0))
    [cell setImage:_groupIcon];  
  else
    [cell setImage:_icon];
}

@end
