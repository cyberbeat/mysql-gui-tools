//
//  PrivilegeDataSource.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 08 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "PrivilegeDataSource.h"

static BOOL has_priv(MYX_USER_OBJECT_PRIVILEGES *privs, const char *priv,
                     PrivilegeType type)
{
  unsigned int i;
  for (i= 0; i < privs->user_privileges_num; i++)
  {
    char *end, *begin;
    bool table_column_priv= NO;
    
    begin= (char*)privs->user_privileges[i];
    
    if (type == PTable)
    {
      if(g_str_has_prefix(begin, "Table_priv_"))
      {
        begin+= sizeof("Table_priv_")-1;
      }
      else if(g_str_has_prefix(priv, "Column "))
      {
        begin+= sizeof("Column_priv_")-1;
        table_column_priv= YES;
      }
      end= strchr(begin, '=');
    }
    else if (type == PColumn)
    {
      begin+= sizeof("Column_priv_")-1;
      end= strchr(begin, '=');
    }
    else if (type == PRoutine)
    {
      begin+= sizeof("Proc_priv_")-1;
      end= strchr(begin, '=');
    }
    else
      end= strstr(begin, "_priv=");
    if (end)
    {
      if (strncasecmp(begin, priv, end-begin)==0 && (int)strlen(priv) == end-begin)
      {
        end= strchr(end, '=');
        return *(end+1)=='Y';
      }
      else if (table_column_priv && strncasecmp(begin, priv+sizeof("Column ")-1, end-begin)==0 && (int)strlen(priv+sizeof("Column ")-1) == end-begin)
      {
        end= strchr(end, '=');
        return *(end+1)=='Y';
      }
    }
  }
  return NO;
}

static void set_priv(MYX_USER_OBJECT_PRIVILEGES *privs, const char *priv,
                     BOOL flag, PrivilegeType type)
{
  unsigned int i;
  for (i= 0; i < privs->user_privileges_num; i++)
  {
    char *end, *begin;
    bool table_column_priv= NO;
    
    begin= (char*)privs->user_privileges[i];
    
    if (type == PTable)
    {
      if(g_str_has_prefix(begin, "Table_priv_"))
      {
        begin+= sizeof("Table_priv_")-1;
      }
      else
      {
        begin+= sizeof("Column_priv_")-1;
        table_column_priv= YES;
      }
      end= strchr(begin, '=');
    }
    else if (type == PColumn)
    {
      begin+= sizeof("Column_priv_")-1;
      end= strchr(begin, '=');
    }
    else if (type == PRoutine)
    {
      begin+= sizeof("Routine_priv_")-1;
      end= strchr(begin, '=');
    }
    else
      end= strstr(begin, "_priv=");
    
    if (end)
    {
      if (strncasecmp(begin, priv, end-begin)==0 && (int)strlen(priv) == end-begin)
      {
        char *eq= strchr(end, '=');
        *(eq+1)= flag ? 'Y' : 'N';
      }
      else if (table_column_priv && (strncasecmp(begin, priv+sizeof("Column ")-1, end-begin)==0 && (int)strlen(priv+sizeof("Column ")-1) == end-begin))
      {
        char *eq= strchr(end, '=');
        *(eq+1)= flag ? 'Y' : 'N';
      }
    }
  }
}

/*
static BOOL compare_privs(MYX_USER_OBJECT_PRIVILEGES *a, MYX_USER_OBJECT_PRIVILEGES *b)
{
  unsigned int i;
  g_return_val_if_fail(strcmp(a->host, b->host)==0, NO);
  g_return_val_if_fail(strcmp(a->object_name, b->object_name)==0, NO);
  g_return_val_if_fail(a->user_privileges_num==b->user_privileges_num, NO);
  for (i= 0; i < a->user_privileges_num; i++)
  {
    if (strcmp(a->user_privileges[i], b->user_privileges[i])==0)
      return NO;
  }
  return YES;
}
*/




@implementation PrivilegeDataSource

- (void)dealloc
{
  [_allPrivileges release];
  [_assignedPrivileges release];
  [_sourceTable release];
  [super dealloc];
}

- (id)initWithPrivileges: (NSArray*)privileges
             sourceTable: (NSTableView*)table
                    type: (PrivilegeType)type
{
  self= [super init];
  if (self)
  {
    _assignedPrivileges= [[NSMutableArray alloc] init];
    _allPrivileges= [privileges retain];
    _sourceTable= [table retain];
    _privileges= NULL;
    _type= type;
  }
  return self;
}

- (void)setPrivileges: (MYX_USER_OBJECT_PRIVILEGES*)objPrivileges
{
  int i, c= [_allPrivileges count];
  _privileges= objPrivileges;
  [_assignedPrivileges removeAllObjects];
  if (_privileges)
  {
    for (i= 0; i < c; i++)
    {
      NSString *p= [_allPrivileges objectAtIndex:i];
      if (has_priv(_privileges, [p UTF8String], _type))
        [_assignedPrivileges addObject: p];
    }
  }
}

- (void)revoke: (NSString*)priv
{
  NSAssert(_privileges, @"Privilege datasource is not initialized with user's privilege structure");
  set_priv(_privileges, [priv UTF8String], NO, _type);
  [_assignedPrivileges removeObjectIdenticalTo: priv];
  if ([_assignedPrivileges indexOfObjectIdenticalTo: priv]!=NSNotFound)
    NSLog(@"COULDNT REVOKE PRIVILEGE");
}

- (void)grant: (NSString*)priv
{
  NSAssert(_privileges, @"Privilege datasource is not initialized with user's privilege structure");
  NSAssert([_assignedPrivileges indexOfObjectIdenticalTo:priv]==NSNotFound,@"Granting duplicated privlege");
  set_priv(_privileges, [priv UTF8String], YES, _type);
  [_assignedPrivileges addObject: priv];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (_sourceTable == aTableView)
    return [_allPrivileges count] - [_assignedPrivileges count];
  else
    return [_assignedPrivileges count];
}


- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  if (_sourceTable == aTableView)
  {
    int c= [_allPrivileges count];
    int j;
    int d= rowIndex;
    for (j= 0; j < c; j++)
    {
      NSString *s= [_allPrivileges objectAtIndex:j];
      if ([_assignedPrivileges indexOfObjectIdenticalTo:s]==NSNotFound)
      {
        if (d == 0)
        {
          if (!aTableColumn || [[aTableColumn identifier] isEqualToString:@"privilege"])
            return s;
          else
            return [NSString stringWithFormat:@"Grants the %@ privilege to the user.",s];
        }
        --d;
      }
    }
  }
  else if (_privileges)
  {
    return [_assignedPrivileges objectAtIndex: rowIndex];
  }
  return @"?";
}

@end
