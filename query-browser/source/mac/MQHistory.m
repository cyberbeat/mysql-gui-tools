//
//  MQHistory.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#include <time.h>
#import <Cocoa/Cocoa.h>
#import "MQHistory.h"
#include <MySQLToolsCommon/myxUtil.h>

NSString *MQHistoryDidChangeNotification= @"MQHistoryDidChangeNotification";

@interface MQHistoryGroup : NSObject
{
@public
  NSMutableArray *array;
  NSString *name;
}
@end


@implementation MQHistoryGroup
- (id)init
{
  self= [super init];
  if (self)
  {
	array= [[NSMutableArray alloc] init];
  }
  return self;
}

- (void)dealloc
{
  [name release];
  [array release];
  [super dealloc];
}
@end


@implementation MQHistoryItem

- (id)initWithItem:(MYX_HISTORY_ENTRY*)entry
{
  self= [super init];
  if (self)
  {
	char *p;
	struct tm tstamp;
	
	_query= [NSStr(entry->sql) retain];
	_catalog= [NSStr(entry->catalog) retain];
	_schema= [NSStr(entry->schema) retain];
	_queryType= entry->query_type;

	p= strchr(entry->date_last_access, 'T');
	if (p) *p= ' ';
	
	strptime(entry->date_last_access, "%F %T", &tstamp);
	
	_lastAccess= mktime(&tstamp);
  }
  return self;
}

- (NSString*)query
{
  return _query;
}
@end



@implementation MQHistory

- (id)init
{
  self= [super init];
  if (self)
  {
	_itemsByInterval= [[NSMutableDictionary alloc] init];
  }
  return self;
}


- (BOOL)loadFromFile:(NSString*)file;
{
  MYX_HISTORY *history;
  MYX_HISTORY_TREE *tree;
  MYX_LIB_ERROR error;
  static NSString *names[]= {
	@"Today",
	@"Monday",
	@"Tuesday",
	@"Wednesday", 
	@"Thursday",
	@"Friday",
	@"Saturday",
	@"Sunday",
	@"Yesterday", 
	@"Last Week",
	@"Before Last Week"
  };
  static int order[]= {
    MYX_HIT_TODAY, 
    MYX_HIT_YESTERDAY,
    MYX_HIT_MONDAY,
    MYX_HIT_TUESDAY,
    MYX_HIT_WEDNESDAY,
    MYX_HIT_THURSDAY,
    MYX_HIT_FRIDAY,
    MYX_HIT_SATURDAY,
    MYX_HIT_SUNDAY,
    MYX_HIT_LAST_WEEK,
    MYX_HIT_BEFORE_LAST_WEEK
  };
  unsigned int o= 0;
  
  
  history= myx_history_load([file fileSystemRepresentation], &error);
  if (!history && error != MYX_ERROR_CANT_OPEN_FILE)
  {
	NSRunAlertPanel(nil, @"Could not load history data from file %@:\n%@",
					@"OK", nil, nil,
					file, MXGetErrorString(error));
	return NO;
  }
  
  // Always add Today group
  {
	MQHistoryGroup *group= [[[MQHistoryGroup alloc] init] autorelease];
	group->name= [names[MYX_HIT_TODAY] retain];
	[_itemsByInterval setObject:group
						 forKey:[NSNumber numberWithInt:MYX_HIT_TODAY]];
    _periodForIndex[o++]= MYX_HIT_TODAY;
  }  

  if (history)
  {
	unsigned int i, c, s, e;

	tree= myx_history_get_tree(history);

	for (i= 0; i < tree->history_intervals_num; i++)
	{
	  MQHistoryGroup *group;

      if (tree->history_intervals[i].interval_type > MYX_HIT_BEFORE_LAST_WEEK)
		tree->history_intervals[i].interval_type= MYX_HIT_BEFORE_LAST_WEEK;
      
      if (tree->history_intervals[i].interval_type == MYX_HIT_TODAY)
        group= [_itemsByInterval objectForKey:[NSNumber numberWithInt:MYX_HIT_TODAY]];
      else
      {
        group= [[[MQHistoryGroup alloc] init] autorelease];
        group->name= [names[tree->history_intervals[i].interval_type] retain];
        [_itemsByInterval setObject:group
                             forKey:[NSNumber numberWithInt:tree->history_intervals[i].interval_type]];
        _periodForIndex[o++]= tree->history_intervals[i].interval_type;
      }

	  for (c= 0; c < tree->history_intervals[i].catalogs_num; c++)
	  {
		for (s= 0; s < tree->history_intervals[i].catalogs[c].schemata_num; s++)
		{
		  for (e= 0; e < tree->history_intervals[i].catalogs[c].schemata[s].entries_num; e++)
		  {
			MQHistoryItem *item= [[MQHistoryItem alloc] initWithItem:tree->history_intervals[i].catalogs[c].schemata[s].entries[e]];
	
			[group->array addObject:item];
			[item release];
		  }
		}
	  }
	}
    myx_history_free_tree(tree);
	
	myx_history_free(history);
    
    // bubblesort the orders
    for (i= 0; i < o; i++)
    {
      int j;
      for (j= i; j < o; j++)
      {
        if (order[_periodForIndex[i]] > order[_periodForIndex[j]])
        {
          int tmp= _periodForIndex[j];
          _periodForIndex[j]= _periodForIndex[i];
          _periodForIndex[i]= tmp;
        }
      }
    }
  }  
  return YES;
}


static int compareEntry(const MYX_HISTORY_ENTRY *e1, const MYX_HISTORY_ENTRY *e2)
{
  return strcmp(e1->date_last_access, e2->date_last_access);
}


- (void)storeToFile:(NSString*)file
{
  MYX_HISTORY *history;
  NSEnumerator *enu;
  MQHistoryGroup *items;
  unsigned int j;
  int limit= [[NSUserDefaults standardUserDefaults] integerForKey:@"QueryHistoryLimit"];
  
  history= g_new0(MYX_HISTORY, 1);
  enu= [_itemsByInterval objectEnumerator];
  while ((items= [enu nextObject]))
    history->entries_num+= [items->array count];
  history->entries= g_new0(MYX_HISTORY_ENTRY, history->entries_num);
  
  j= 0;
  enu= [_itemsByInterval objectEnumerator];
  while ((items= [enu nextObject]))
  {
    unsigned int i, c= [items->array count];
    for (i= 0; i < c; i++)
    {
      MQHistoryItem *item= [items->array objectAtIndex:i];
      MYX_HISTORY_ENTRY *entry= history->entries+j++;
      char buffer[100];

      entry->sql= g_strdup([item->_query UTF8String]);
      entry->catalog= g_strdup([item->_catalog UTF8String]);
      entry->schema= g_strdup([item->_schema UTF8String]);

      entry->query_type= item->_queryType;

      strftime(buffer, sizeof(buffer), "%FT%T", gmtime(&item->_lastAccess));
      entry->date_last_access= g_strdup(buffer);
    }
  }
  
  qsort(history->entries, history->entries_num, sizeof(MYX_HISTORY_ENTRY), 
        (int(*)(const void*,const void*))compareEntry);
  
  j= history->entries_num;
  if (limit > 0)
    history->entries_num= limit;
  myx_history_store([file fileSystemRepresentation], history);
  history->entries_num= j;
  
  myx_history_free(history);
}


- (void)rememberQuery:(NSString*)query
			  catalog:(NSString*)catalog
			   schema:(NSString*)schema
{
  NSEnumerator *enumer= [_itemsByInterval objectEnumerator];
  MQHistoryGroup *group;
  MQHistoryItem *item= nil;
  BOOL found= NO;
  
  while (!found && (group= [enumer nextObject]))
  {
	unsigned int i, c;
	c= [group->array count];
	for (i= 0; i < c; i++)
	{
	  item= [group->array objectAtIndex:i];
	  
	  if ([[item query] isEqualToString:query])
	  {
		item->_lastAccess= time(NULL);
		[[item retain] autorelease];
		[group->array removeObject:item];
		found= YES; // break outer loop
		break;
	  }
	}
  }
  
  if (!found)
  {
	item= [[[MQHistoryItem alloc] init] autorelease];
	item->_query= [query retain];
	item->_catalog= [catalog retain];
	item->_schema= [schema retain];
	
	item->_lastAccess= time(NULL);
  }
  
  group= [_itemsByInterval objectForKey:[NSNumber numberWithInt:MYX_HIT_TODAY]];
  if (group)
    [group->array insertObject:item atIndex:0];
    
  [[NSNotificationCenter defaultCenter] postNotificationName:MQHistoryDidChangeNotification 
													  object:self];
}


- (void)reset
{ 
  [_itemsByInterval removeAllObjects];  

  MQHistoryGroup *group= [[[MQHistoryGroup alloc] init] autorelease];
  group->name= @"Today";
  [_itemsByInterval setObject:group
                       forKey:[NSNumber numberWithInt:MYX_HIT_TODAY]];
  _periodForIndex[0]= MYX_HIT_TODAY;
    
  [[NSNotificationCenter defaultCenter] postNotificationName:MQHistoryDidChangeNotification 
													  object:self];  
}


- (void)removeItem:(MQHistoryItem*)item
{
  NSEnumerator *en= [_itemsByInterval objectEnumerator];
  MQHistoryGroup *group;
  [item retain];
  while ((group= [en nextObject]))
  {
    [group->array removeObject:item];
  }
  [item release];
}


- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (!item)
	return [_itemsByInterval objectForKey:[NSNumber numberWithInt:_periodForIndex[index]]];
  else
	return [((MQHistoryGroup*)item)->array objectAtIndex:index];
}


- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if (!item || [item isKindOfClass:[MQHistoryGroup class]])
	return YES;
  else
	return NO;
}


- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{
  int count; 
  if (!item)
	count= [_itemsByInterval count];
  else
	count= [((MQHistoryGroup*)item)->array count];
  return count;
}


- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  if ([item isKindOfClass:[MQHistoryItem class]])
	return [[item query] substringToIndex:MIN(100, [[item query] length])];
  else
	return ((MQHistoryGroup*)item)->name;
}


- (BOOL)outlineView:(NSOutlineView *)outlineView
         writeItems:(NSArray*)items toPasteboard:(NSPasteboard*)pboard 
{
  if (![[items objectAtIndex:0] isKindOfClass:[MQHistoryItem class]])
    return NO;
  [pboard declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
  
  [pboard setString:[[items objectAtIndex:0] query] forType:NSStringPboardType];
  
  return YES;
}

@end
