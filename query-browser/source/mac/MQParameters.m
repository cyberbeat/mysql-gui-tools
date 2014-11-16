//
//  MQParameters.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQParameters.h"
#import <MySQLToolsCommon/MQResultSetView.h>
#import <MySQLToolsCommon/MQTableView.h>

@implementation MQParameters

- (void)loadGlobals
{
  NSString *path= [NSString stringWithFormat:@"%@/global_params.xml", [[NSUserDefaults standardUserDefaults] stringForKey:@"QBDirectory"]];
  NSData *plistData;
  NSString *error;
  NSPropertyListFormat format;
  id plist;
  plistData = [NSData dataWithContentsOfFile:path];
  if (plistData)
  {
    plist = [NSPropertyListSerialization propertyListFromData:plistData
                                             mutabilityOption:NSPropertyListMutableContainers
                                                       format:&format
                                             errorDescription:&error];
    if (!plist)
    {
      NSLog(error);
      [error release];
    }
    _globalParams= [plist retain];
  }
  else
    _globalParams= nil;
}


- (void)saveGlobals
{
  NSString *path= [NSString stringWithFormat:@"%@/global_params.xml", [[NSUserDefaults standardUserDefaults] stringForKey:@"QBDirectory"]];
  NSData *xmlData;
  NSString *error;
  
  xmlData = [NSPropertyListSerialization dataFromPropertyList:_globalParams
                                                       format:NSPropertyListXMLFormat_v1_0
                                             errorDescription:&error];
  if (xmlData)
  {
    [xmlData writeToFile:path atomically:YES];
  }
  else
  {
    NSLog(error);
    [error release];
  }
}


- (id)init
{
  self= [super init];
  if (self)
  {
    _normalParams= [[NSMutableArray alloc] init];
    _localParams= [[NSMutableArray alloc] init];

    [self loadGlobals];
    if (!_globalParams)
      _globalParams= [[NSMutableArray alloc] init];
  }
  return self;
}


- (void)dealloc
{
  [self saveGlobals];
  [_normalParams release];
  [_globalParams release];
  [_localParams release];
  [super dealloc];
}


- (id)localParameters
{
  return _localParams;
}


- (void)setLocalParameters:(id)params
{
  if (_localParams != params)
  {
    [_localParams release];
    if (params)
      _localParams= [params retain];
    else
      _localParams= [[NSMutableArray alloc] init];
  }
}


- (void)setResultset:(MYX_RESULTSET*)rs row:(int)rowIndex
{
  unsigned int i;
  
  [_normalParams removeAllObjects];
  if (rs)
  {
    if (rowIndex < 0)
      _row= NULL;
    else
      _row= rs->rows+rowIndex;
    for (i= 0; i < rs->columns_num_to_display; i++)
    {
      [_normalParams addObject:[NSString stringWithUTF8String:rs->columns[i].name]];
      if (_row && _row->fields[i].value)
        [_normalParams addObject:[NSString stringWithUTF8String:_row->fields[i].value]];
      else
        [_normalParams addObject:@""];
      [_normalParams addObject:[NSNumber numberWithLongLong:(1LL<<32)|i]];
    }
  }
  else
    _row= NULL;
}


- (BOOL)itemCanBeDeleted:(id)item
{
  if ([item isKindOfClass:[NSString class]])
    return NO;
  else if (([item longLongValue] >> 32) > 1)
    return YES;
  else
    return NO;
}


- (BOOL)itemCanBeAdded:(id)item
{
  if ([item isEqualTo:@"local"] || [item isEqualTo:@"global"])
    return YES;
  else if ([item isEqualTo:@"normal"])
    return NO;
  else if (([item longLongValue] >> 32) > 1)
    return YES;
  else
    return NO;
}


- (BOOL)itemCanBeEdited:(id)item
{
  if ([item isKindOfClass:[NSString class]])
    return NO;
  else if (([item longLongValue] >> 32) > 1)
    return YES;
  else
    return NO;
}


- (void)addParameter:(id)item
{
  if ([item isKindOfClass:[NSString class]])
  {
    if ([item isEqualTo:@"local"])
    {
      [_localParams addObject:@"new_param"];
      [_localParams addObject:@""];
      [_localParams addObject:[NSNumber numberWithLongLong:(2LL<<32)|[_localParams count]/3]];
    }
    else if ([item isEqualTo:@"global"])
    {
      [_globalParams addObject:@"new_param"];
      [_globalParams addObject:@""];
      [_globalParams addObject:[NSNumber numberWithLongLong:(3LL<<32)|[_globalParams count]/3]];
    }
  }
  else if (([item longLongValue] >> 32) != 1)
  {
    if ([item longLongValue] >> 32 == 2)
    {
      [_localParams addObject:@"new_param"];
      [_localParams addObject:@""];
      [_localParams addObject:[NSNumber numberWithLongLong:(2LL<<32)|[_localParams count]/3]];
    }
    else if ([item longLongValue] >> 32 == 3)
    {
      [_globalParams addObject:@"new_param"];
      [_globalParams addObject:@""];
      [_globalParams addObject:[NSNumber numberWithLongLong:(3LL<<32)|[_globalParams count]/3]];
    }
  }
}


- (void)deleteParameter:(id)item
{
  if ([item longLongValue] >> 32 == 2)
  {
    int index= [item longLongValue]&0xffffffffL;
    [_localParams removeObjectAtIndex:index*3+2];
    [_localParams removeObjectAtIndex:index*3+1];
    [_localParams removeObjectAtIndex:index*3];
  }
  else if ([item longLongValue] >> 32 == 3)
  {
    int index= [item longLongValue]&0xffffffffL;
    [_globalParams removeObjectAtIndex:index*3+2];
    [_globalParams removeObjectAtIndex:index*3+1];
    [_globalParams removeObjectAtIndex:index*3];
  }  
}


- (MYX_STRINGLIST*)parametersFromMaster:(MQResultSetView*)master
{
  MYX_STRINGLIST *sl= g_new0(MYX_STRINGLIST, 1);
  unsigned int idx, i, c;
  MYX_RESULTSET *rs= [master resultset];
  int rowIndex= [[master tableView] selectedRow];
  
  if (rs && rowIndex >= 0)
    sl->strings_num= ([_globalParams count]+[_localParams count])/3+rs->columns_num_to_display;
  else
    sl->strings_num= ([_globalParams count]+[_localParams count])/3;
  sl->strings= g_new0(char*, sl->strings_num);
  idx= 0;
  c= [_globalParams count]/3;
  for (i= 0; i < c; i++)
  {
    sl->strings[idx++]= g_strdup_printf("%s=%s",
                                        [[_globalParams objectAtIndex:i*3] UTF8String],
                                        [[_globalParams objectAtIndex:i*3+1] UTF8String]);
  }

  c= [_localParams count]/3;
  for (i= 0; i < c; i++)
  {
    sl->strings[idx++]= g_strdup_printf("%s=%s",
                                        [[_localParams objectAtIndex:i*3] UTF8String],
                                        [[_localParams objectAtIndex:i*3+1] UTF8String]);
  }

  if (rs && rowIndex >= 0)
  {
    MYX_RS_ROW *row= rs->rows+rowIndex;
    for (i= 0; i < rs->columns_num_to_display; i++)
    {
      sl->strings[idx++]= g_strdup_printf("%s=%s",
                                          rs->columns[i].name,
                                          row->fields[i].value?:"NULL");
    }
  }
  
  return sl;
}


- (void)outlineView:(NSOutlineView *)outlineView setObjectValue:(id)object forTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if ([item isKindOfClass:[NSNumber class]])
  {
    int type= [item longLongValue]>>32;
    int index= [item longLongValue]&0xffffffffLL;
    NSMutableArray *array= type == 2 ? _localParams : _globalParams;
    
    if ([[tableColumn identifier] isEqualTo:@"name"])
      [array replaceObjectAtIndex:index*3
                       withObject:object];
    else
      [array replaceObjectAtIndex:index*3+1
                       withObject:object];
  }
}


- (id)outlineView:(NSOutlineView *)outlineView objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
  if ([item isKindOfClass:[NSString class]])
  {
    if ([[tableColumn identifier] isEqualTo:@"name"])
    {
      if ([item isEqualTo:@"normal"])
        return NSLocalizedString(@"From Master", @"Parameter Sidebar");
      else if ([item isEqualTo:@"local"])
        return NSLocalizedString(@"Local", @"Parameter Sidebar");
      else
        return NSLocalizedString(@"Global", @"Parameter Sidebar");
    }
  }
  else
  {
    int type= [item longLongValue]>>32;
    int index= [item longLongValue]&0xffffffffLL;

    if ([[tableColumn identifier] isEqualTo:@"name"])
    {
      switch (type)
      {
        case 1:
          return [_normalParams objectAtIndex:index*3];
        case 2:
          return [_localParams objectAtIndex:index*3];
        case 3:
          return [_globalParams objectAtIndex:index*3];
      }
    }
    else
    {
      switch (type)
      {
        case 1:
          return [_normalParams objectAtIndex:index*3+1];
        case 2:
          return [_localParams objectAtIndex:index*3+1];
        case 3:
          return [_globalParams objectAtIndex:index*3+1];
      }
    }
  }
  return nil;
}


- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
  if (!item)
  {
    switch (index)
    {
      case 0:
        return @"normal";
      case 1:
        return @"local";
      case 2:
        return @"global";
    }
    return @"???";
  }
  else
  {
    if ([item isEqualTo:@"normal"])
      return [_normalParams objectAtIndex:index*3+2];
    else if ([item isEqualTo:@"local"])
      return [_localParams objectAtIndex:index*3+2];
    else
      return [_globalParams objectAtIndex:index*3+2];
  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
  if (!item)
    return YES;
  else if ([item isKindOfClass:[NSString class]])
    return YES;
  else
    return NO;
}

- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
  if (!item)
    return 3;
  else
  {
    if ([item isEqual:@"normal"])
      return [_normalParams count]/3;
    else if ([item isEqual:@"local"])
      return [_localParams count]/3;
    else if ([item isEqual:@"global"])
      return [_globalParams count]/3;
    else
      return 0;
  }
}

@end
