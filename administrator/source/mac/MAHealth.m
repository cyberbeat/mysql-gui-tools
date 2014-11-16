#import "MAHealth.h"
#import "MAHealthGraph.h"
#import <MySQLToolsCommon/MTextImageCell.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#import <MySQLToolsCommon/MPreferences.h>
#import <MySQLToolsCommon/MConnectionInfo.h>
#import <MySQLToolsCommon/mxUtils.h>
#import <MySQLToolsCommon/MDialogs.h>
#include <sys/time.h>
#include <sys/select.h>

#import "MAGraphEditor.h"
#import "MAMenuContextView.h"
#import "MAHealthPage.h"

#include "myx_public_interface.h"
#include "myx_admin_public_interface.h"


static NSDictionary *dictFromPageData(MYX_HEALTH_PAGE *page)
{
  NSMutableArray *groups= [NSMutableArray arrayWithCapacity:page->groups_num];
  unsigned int i;

  for (i= 0; i < page->groups_num; i++)
  {
    MYX_HEALTH_GROUP *group= page->groups+i;
    NSMutableArray *graphs= [NSMutableArray arrayWithCapacity:group->graphs_num];
    unsigned int j;
    
    for (j= 0; j < group->graphs_num; j++)
    {
      MYX_HEALTH_GRAPH *graph= group->graphs+j;
      id values[12];
      id keys[12];
      int c= 0;

      if (graph->graph_caption)
      {
        values[c]= [NSString stringWithUTF8String:graph->graph_caption];
        keys[c]= @"title";
        c++;
      }
      values[c]= [NSNumber numberWithBool:graph->display_graph_caption];
      keys[c]= @"showTitle";
      c++;
      values[c]= [NSNumber numberWithInt:graph->graphtype];
      keys[c]= @"graphType";
      c++;
      values[c]= [NSNumber numberWithInt:graph->value_unit];
      keys[c]= @"graphUnit";
      c++;
      values[c]= [NSNumber numberWithDouble:graph->min];
      keys[c]= @"minValue";
      c++;
      values[c]= [NSNumber numberWithDouble:graph->max];
      keys[c]= @"maxValue";
      c++;
      values[c]= [NSNumber numberWithBool:graph->autoextend_max];
      keys[c]= @"autoExtend";
      c++;
      if (graph->value_formula)
      {
        values[c]= [NSString stringWithCString:graph->value_formula];
        keys[c]= @"valueFormula";
        c++;
      }
      if (graph->max_formula)
      {
        values[c]= [NSString stringWithCString:graph->max_formula];
        keys[c]= @"maxFormula";
        c++;
      }
      if (graph->value_caption)
      {
        values[c]= [NSString stringWithCString:graph->value_caption];
        keys[c]= @"valueCaption";
        c++;
      }
      if (graph->max_caption)
      {
        values[c]= [NSString stringWithCString:graph->max_caption];
        keys[c]= @"maxCaption";
        c++;
      }
      values[c]= [NSNumber numberWithInt:graph->refreshtime];
      keys[c]= @"refreshTime";
      c++;
      [graphs addObject:[NSDictionary dictionaryWithObjects:values forKeys:keys count:c]];
    }

    [groups addObject:[NSDictionary dictionaryWithObjectsAndKeys:
   group->caption?[NSString stringWithUTF8String:group->caption]:nil, @"title",
      graphs, @"graphs",
      nil]];
  }

  return [NSDictionary dictionaryWithObjectsAndKeys:
    page->caption?[NSString stringWithUTF8String:page->caption]:nil, @"title",
    page->description?[NSString stringWithUTF8String:page->description]:nil, @"description",
    groups, @"groups",
    nil];
}


@interface Item : NSObject
{
  @public
  void *ptr;
  int type;
  NSMutableArray *children;
}
+ (Item*)itemWithPtr:(void*)ptr type:(int)type;
@end
@implementation Item

+ (Item*)itemWithPtr:(void*)aptr type:(int)atype
{
  Item *item= [[[Item alloc] init] autorelease];
  item->ptr= aptr;
  item->type= atype;
  return item;
}

- (void)dealloc
{
  [children release];
  [super dealloc];
}
@end

@interface ListingDataSource : NSObject
{
  //MYX_TRANS *_trans;
  MYX_VARIABLES_LISTING *_listing;
  
  NSMutableArray *_items;
}
- (id)initWithListing:(MYX_VARIABLES_LISTING*)listing;// translation:(MYX_TRANS*)trans;

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item;

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item;

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item;

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item;

@end

@implementation ListingDataSource

- (id)initWithListing:(MYX_VARIABLES_LISTING*)listing;// translation:(MYX_TRANS*)trans
{
  self= [super init];
  if (self)
  {
    unsigned int i, j;
    _listing= listing;
    //_trans= trans;
    
    _items= [[NSMutableArray alloc] initWithCapacity:_listing->groups_num];
    for (i= 0; i < _listing->groups_num; i++)
    {
      Item *item= [Item itemWithPtr:_listing->groups+i type:'G'];
      [_items addObject:item];
      
      item->children= [[NSMutableArray alloc] init];
      for (j= 0; j < _listing->groups[i].subgroups_num; j++)
      {
        Item *sitem= [Item itemWithPtr:_listing->groups[i].subgroups+j type:'S'];
        sitem->children= [[NSMutableArray alloc] init];
        [item->children addObject:sitem];
      }
    }
  }
  return self;
}

- (void)dealloc
{
  [_items release];
  [super dealloc];
}

- (id)outlineView:(NSOutlineView *)outlineView 
            child:(int)index 
           ofItem:(id)item
{
  if (item == nil)
    return [_items objectAtIndex:index];
  else
  {
    Item *it= (Item*)item;
    return [it->children objectAtIndex:index];
  }
}

- (BOOL)outlineView:(NSOutlineView *)outlineView 
   isItemExpandable:(id)item
{
  if (item == nil)
    return YES;
  else if (((Item*)item)->type=='G')
    return YES;
  else
    return NO;
}

- (int)outlineView:(NSOutlineView *)outlineView 
numberOfChildrenOfItem:(id)item
{
  if (item == nil)
    return [_items count];
  else if (((Item*)item)->type=='G')
    return [((Item*)item)->children count];
  else
    return 0;
}

- (id)outlineView:(NSOutlineView *)outlineView 
objectValueForTableColumn:(NSTableColumn *)tableColumn 
           byItem:(id)item
{
  Item *it= (Item*)item;

  switch (it->type)
  {
    case 'G':
      return [NSString stringWithUTF8String:((MYX_VARIABLES_GROUP*)it->ptr)->name];
    case 'S':
      return [NSString stringWithUTF8String:((MYX_VARIABLES_SUBGROUP*)it->ptr)->name];
    default:
      return @"?";
  }
}

@end


@interface VariableDataSource : NSObject
{
  NSMutableArray *_items;
  MYX_VARIABLES *_values;
  //MYX_TRANS *_trans;
  
  MAHealth *_owner;
}
//- (id)initWithTranslations:(MYX_TRANS*)trans owner:(MAHealth*)owner;
- (id)initWithOwner:(MAHealth*)owner;
- (void)setFromSubgroup:(MYX_VARIABLES_SUBGROUP*)sg;
- (void)setFromGroup:(MYX_VARIABLES_GROUP*)g;
- (NSArray*)items;
- (MYX_VARIABLES*)values;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex;
- (void)tableView:(NSTableView *)aTableView
   setObjectValue:(id)anObject
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex;
@end

//==============================================================================

@interface MAHealth(Private)
- (MAHealthPage*)buildPage:(NSDictionary*)pageProps;
- (MAHealthGroup*)buildGroup:(NSDictionary*)props inPage:(MAHealthPage*)page;
- (MAHealthGraph*)buildGraph:(NSDictionary*)props;
- (void)updateThread:(id)arg;
- (void)update:(id)arg;
- (void)outlineViewSelectionDidChange:(NSNotification *)notification;
- (BOOL)tableView:(NSTableView *)aTableView
 shouldEditTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex;
- (void)tableView:(NSTableView *)aTableView 
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn 
              row:(int)rowIndex;
- (void)performOnAllGraphs:(SEL)selector withArgument:(NSValue*)argument;
- (void)setValue:(NSString*)value forVariable:(const char*)var;
- (void)valueSaved:(id)argument result:(void*)result;
- (BOOL)saveGraphSettings;
- (void)reloadGraphs;
@end

//==============================================================================

@implementation VariableDataSource
//- (id)initWithTranslations:(MYX_TRANS*)trans owner:(MAHealth*)owner
- (id)initWithOwner:(MAHealth*)owner
{
  self= [super init];
  if (self)
  {
    //_trans= trans;
    _owner= owner;
  }
  return self;
}

- (void)dealloc
{
  [_items release];
  [super dealloc];
}

- (void)setFromSubgroup:(MYX_VARIABLES_SUBGROUP*)sg
{
  unsigned int j;
  [_items release];
  _items= [[NSMutableArray alloc] initWithCapacity:sg ? sg->variables_num : 0];
  if (sg)
  {
    for (j= 0; j < sg->variables_num; j++)
    {
      Item *vitem= [Item itemWithPtr:sg->variables+j type:'V'];
      [_items addObject:vitem];
    }
  }
}

- (void)setFromGroup:(MYX_VARIABLES_GROUP*)g
{
  unsigned int j;
  [_items release];
  _items= [[NSMutableArray alloc] initWithCapacity:g ? g->variables_num : 0];
  if (g)
  {
    for (j= 0; j < g->variables_num; j++)
    {
      Item *vitem= [Item itemWithPtr:g->variables+j type:'V'];
      [_items addObject:vitem];
    }
  }
}

- (NSArray*)items
{
  return _items;
}

- (MYX_VARIABLES*)values
{
  return _values;
}

- (void)setValues:(MYX_VARIABLES*)values
{
  _values= values;
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return [_items count];
}

- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  Item *it= (Item*)[_items objectAtIndex:rowIndex];
  MYX_VARIABLE_ELEMENT *var= (MYX_VARIABLE_ELEMENT*)it->ptr;
  
  if ([[aTableColumn identifier] isEqualToString:@"variable"])
  {
    return [NSString stringWithUTF8String:var->mysql_id];
  }
  else if ([[aTableColumn identifier] isEqualToString:@"value"])
  {
    unsigned int i;
    if (_values)
    {
      for (i= 0; i < _values->variables_num; i++)
      {
        if (strcmp(_values->variables[i].name, var->mysql_id)==0)
          return [NSString stringWithUTF8String:_values->variables[i].value?:""];
      }
    }
    return @"";
  }
  else
  {    
    const char *v= var->desc_id;
    //v= myx_t(_trans, "MySQLVariables", v, v);
    return [NSString stringWithUTF8String:v];
  }
}

- (void)tableView:(NSTableView *)aTableView
   setObjectValue:(id)anObject
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  Item *it= (Item*)[_items objectAtIndex:rowIndex];
  MYX_VARIABLE_ELEMENT *var= (MYX_VARIABLE_ELEMENT*)it->ptr;
  unsigned int i;

  // need to set this here too, even if it'll be reloaded
  // later, because otherwise when you finish editing with Return,
  // it will display the old value and somehow we'll get another
  // message telling our value changed to the old value.
  if (_values)
  {
    for (i= 0; i < _values->variables_num; i++)
    {
      if (strcmp(_values->variables[i].name, var->mysql_id)==0)
      {
        g_free(_values->variables[i].value);
        _values->variables[i].value= g_strdup([anObject UTF8String]);
        break;
      }
    }
  }
  
  [_owner setValue:anObject forVariable:var->mysql_id];
}
@end

//==============================================================================


@implementation MAHealth(Private)

- (void)updateThread:(id)arg
{
  MYX_VARIABLES *vars;
  MYX_USER_CONNECTION *conn= (MYX_USER_CONNECTION*)[arg pointerValue];
  MYSQL *mysql;
  struct timeval tv1, tv2;
  
  mysql= myx_mysql_init();

  if (myx_connect_to_instance(conn,mysql) < 0)
  {
    NSLog(@"Could not connect to MySQL instance");
    myx_free_user_connection_content(conn);
    g_free(conn);
    myx_mysql_close(mysql);
    mysql= NULL;
    return;
  }
  
  [self retain];
    
  while (!_cancelThread)
  {
    struct timeval elapsed, wait;
    id value;
    
    gettimeofday(&tv1, NULL);

    vars= myx_get_status_variables(mysql);
    if (!vars)
    {
      NSLog(@"error fetching status variables");
      break;
    }

    value= [[NSValue alloc] initWithBytes:&vars objCType:@encode(void *)];
    [self performSelectorOnMainThread:@selector(update:) 
                           withObject:value
                        waitUntilDone:YES];
    [value release];
    gettimeofday(&tv2, NULL);
    
    // wait to complete 1s
    elapsed.tv_usec= (tv2.tv_usec - tv1.tv_usec) % 1000000;
    elapsed.tv_sec= (tv2.tv_sec - tv1.tv_sec) + (tv2.tv_usec - tv1.tv_usec)/1000000;
    if (elapsed.tv_sec < 1)
    {
      wait.tv_usec= 1000000 - elapsed.tv_usec;
      wait.tv_sec= 0;
      select(0, NULL, NULL, NULL, &wait);
    }
  }
  myx_mysql_close(mysql);

  myx_free_user_connection_content(conn);
  g_free(conn);

  [self release];
}


- (IBAction)moreInfo:(id)sender
{
  [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"http://www.mysql.com/network"]];
}

- (void)update:(id)arg
{ 
  [self performOnAllGraphs:@selector(updateValues:) withArgument:arg];
}


- (MAHealthGraph*)buildGraph:(NSDictionary*)props
{
  switch ([[props objectForKey:@"graphType"] intValue])
  {
    case MYX_LINE_GRAPH:
    {
      MALineHealthGraph *hg;
      hg= [[MALineHealthGraph alloc] initWithProperties:props];
      [hg setMeterImage:[NSImage imageNamed:@"health_level_fg.png"]];
      [hg setMenu:editMenu];
      [hg autorelease];
      return hg;
    }

    case MYX_BAR_GRAPH:
	{
	  MABarHealthGraph *hg;
      hg= [[MABarHealthGraph alloc] initWithProperties:props];
      [hg setMenu:editMenu];
      [hg autorelease];
      return hg;
	}
  }
  return nil;
}

- (MAHealthGroup*)buildGroup:(NSDictionary*)props inPage:(MAHealthPage*)page
{
  NSArray *groupGraphs= [props objectForKey:@"graphs"];
  MAHealthGroup *group;
  int j;
  
  group= [[[MAHealthGroup alloc] initWithProperties:[NSMutableDictionary dictionaryWithObjectsAndKeys:[props objectForKey:@"title"],
    @"title", nil]] autorelease];
  [group setMenu:editMenu];
  [group setFrame:NSMakeRect(0,0,[[page view] frame].size.width-17.0*2, [group frame].size.height)];

  for (j= (int)[groupGraphs count]-1; j >= 0; j--)
  {
    MAHealthGraph *graph= [self buildGraph:[groupGraphs objectAtIndex:j]];

    if (graph)
	{
      [group addGraph:graph];
	  
	  // this is a hack to force the graph to resize itself and adjust its
	  // size correctly. should fix this eventually...
	  [graph setProperties:[graph properties]];
	}
  }

  return group;
}

- (MAHealthPage*)buildPage:(NSDictionary*)pageProps
{
  MAHealthPage *page= [[[MAHealthPage alloc] initWithProperties:
    [NSDictionary dictionaryWithObjectsAndKeys:
      [pageProps objectForKey:@"title"], @"title", 
      [pageProps objectForKey:@"description"], @"description", nil]] autorelease];
  NSArray *pageGroups= [pageProps objectForKey:@"groups"];
  unsigned int i;
  
  [tabView insertTabViewItem:page atIndex:[tabView numberOfTabViewItems]-2];

  [page setMenu:pageMenu];

  for (i= 0; i < [pageGroups count]; i++)
  {
    [page addGroup: [self buildGroup:[pageGroups objectAtIndex:i] inPage:page]];
  }
  
  [page rearrange];
  
  return page;
}

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  id sender= [notification object];
  Item *item= (Item*)[sender itemAtRow:[sender selectedRow]];
  
  if (sender == statusIndex)
  {
    if (item)
    {
      if (item->type == 'S')
        [[statusTable dataSource] setFromSubgroup:item->ptr];
      else
        [[statusTable dataSource] setFromGroup:item->ptr];
    }
    else
      [[statusTable dataSource] setFromGroup:NULL];
    [statusTable reloadData];
  }
  else
  {
    if (item)
    {
      if (item->type == 'S')
        [[serverTable dataSource] setFromSubgroup:item->ptr];
      else
        [[serverTable dataSource] setFromGroup:item->ptr];    
    }
    else
      [[serverTable dataSource] setFromGroup:NULL];
    [serverTable reloadData];
  }
}


- (BOOL)tableView:(NSTableView *)aTableView 
shouldEditTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  if ([(VariableDataSource*)[aTableView dataSource] values])
  {
    Item *it= (Item*)[[[aTableView dataSource] items] objectAtIndex:rowIndex];
    MYX_VARIABLE_ELEMENT *var= (MYX_VARIABLE_ELEMENT*)it->ptr;
    return var->editable ? YES : NO;
  }
  return NO;
}


- (void)tableView:(NSTableView *)aTableView 
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn 
              row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualToString:@"variable"])
  {
    if ([self tableView:aTableView shouldEditTableColumn:aTableColumn row:rowIndex])
      [aCell setImage:_editableIcon];
    else
      [aCell setImage:_nonEditableIcon];
  }
}

- (void)variablesArrived:(id)arg result:(void*)result
{
  if (!result && ![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
  {
    [self refreshServerVariables:nil];
    [self refreshStatusVariables:nil];
    return;
  }
  
  if ([arg isEqualToString:@"status"])
  {
    if (_statusVariables)
      myx_free_variables(_statusVariables);
    _statusVariables= result;
    [[statusTable dataSource] setValues:_statusVariables];
    [statusTable reloadData];
  }
  else
  {
    if (_serverVariables)
      myx_free_variables(_serverVariables);
    _serverVariables= result;
    [[serverTable dataSource] setValues:_serverVariables];
    [serverTable reloadData];
    
    // update values in graphs
    [self performOnAllGraphs:@selector(updateServerVariables:)
                withArgument:[NSValue valueWithPointer:_serverVariables]];
  }
}

- (void)valueSaved:(id)argument result:(void*)result
{
  if (![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
    ;
    //[self setValue:
    
  g_free((char*)argument);
  [self refreshServerVariables:self];
}

- (void)setValue:(NSString*)value 
     forVariable:(const char*)var
{
  char *tmp= g_strdup([value UTF8String]);
  
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*))myx_set_variable
                              argument:(char*)var
                              argument:tmp
                      finishedSelector:@selector(valueSaved:result:)
                              argument:(id)tmp
                                target:self];
}


- (void)performOnAllGraphs:(SEL)selector withArgument:(NSValue*)argument
{
  unsigned int p, r, g;
  unsigned int pc= [_pages count];
  unsigned int rc, gc;
  void *arg= [argument pointerValue];

  for (p= 0; p < pc; p++)
  {
    NSArray *groups= [[_pages objectAtIndex:p] groups];
    rc= [groups count];
    for (r= 0; r < rc; r++)
    {
      NSArray *graphs= [[groups objectAtIndex:r] graphs];
      gc= [graphs count];
      for (g= 0; g < gc; g++)
        [[graphs objectAtIndex:g] performSelector:selector withObject:arg];
    }
  }
}

- (BOOL)saveGraphSettings
{
  unsigned int p, r, g;
  unsigned int pc= [_pages count];
  unsigned int rc, gc;
  BOOL ok;
  NSString *path= [[MPreferences preferences] pathForFile:@"Administrator/mysqladmin_custom_health.xml"];
  MYX_HEALTH_PAGES *pages= g_new0(MYX_HEALTH_PAGES, 1);
  
  pages->pages_num= pc;
  pages->pages= g_new0(MYX_HEALTH_PAGE, pc);

  for (p= 0; p < pc; p++)
  {
    id page= [_pages objectAtIndex:p];
    NSArray *groups= [page groups];
    rc= [groups count];
    
    pages->pages[p].caption= g_strdup([[[page properties] objectForKey:@"title"] UTF8String]);
    pages->pages[p].description= g_strdup([[[page properties] objectForKey:@"description"] UTF8String]);
    pages->pages[p].pos= p;
    pages->pages[p].groups_num= rc;
    pages->pages[p].groups= g_new0(MYX_HEALTH_GROUP, rc);
    for (r= 0; r < rc; r++)
    {
      id group= [groups objectAtIndex:r];
      NSArray *graphs= [group graphs];
      gc= [graphs count];
      
      pages->pages[p].groups[r].caption= g_strdup([[[group properties] objectForKey:@"title"] UTF8String]);
      pages->pages[p].groups[r].pos= r;
      pages->pages[p].groups[r].graphs_num= gc;
      pages->pages[p].groups[r].graphs= g_new0(MYX_HEALTH_GRAPH, gc);
      for (g= 0; g < gc; g++)
      {
        id graph= [[graphs objectAtIndex:g] properties];
        MYX_HEALTH_GRAPH *gptr= pages->pages[p].groups[r].graphs+g;
        
        gptr->graph_caption= g_strdup([[graph objectForKey:@"title"] UTF8String]);
        gptr->display_graph_caption= [[graph objectForKey:@"showTitle"] boolValue];
        gptr->graphtype= [[graph objectForKey:@"graphType"] intValue];
        gptr->value_unit= [[graph objectForKey:@"graphUnit"] intValue];
        gptr->min= [[graph objectForKey:@"minValue"] doubleValue];
        gptr->max= [[graph objectForKey:@"maxValue"] doubleValue];
        gptr->autoextend_max= [[graph objectForKey:@"autoExtend"] boolValue];
        gptr->value_formula= g_strdup([[graph objectForKey:@"valueFormula"] UTF8String]);
        gptr->max_formula= g_strdup([[graph objectForKey:@"maxFormula"] UTF8String]);
        gptr->value_caption= g_strdup([[graph objectForKey:@"valueCaption"] UTF8String]);
        gptr->max_caption= g_strdup([[graph objectForKey:@"maxCaption"] UTF8String]);
        gptr->refreshtime= [[graph objectForKey:@"refreshTime"] intValue];
        gptr->pos= g;
      }
    }
  }
  
  ok= NO;
  if (pages && myx_save_health_pages(pages, [path fileSystemRepresentation])==0)
    ok= YES;
  
  if (pages)
    myx_free_health_pages(pages);
  return ok;
}


- (void)reloadGraphs
{
  NSString *path;
  MYX_ADMIN_LIB_ERROR err;
  MYX_HEALTH_PAGES *pageData;
  
  // remove everything old
  while ([_pages count] > 0)
  {
	id page= [_pages objectAtIndex:0];
	[tabView removeTabViewItem:page];
	[_pages removeObject:page];
  }
  
  // try customized
  path= [[MPreferences preferences] pathForFile:@"Administrator/mysqladmin_custom_health.xml"];
  pageData= myx_read_in_health_pages([path UTF8String], &err);
  if (!pageData)
  {
	// try factory
	path= [[NSBundle mainBundle] pathForResource:@"mysqladmin_health"
										  ofType:@"xml"];
	pageData= myx_read_in_health_pages([path UTF8String], &err);
  }
  if (!pageData)
	NSRunAlertPanel(@"Error", @"Could not read graph definitions.",
					@"OK", nil, nil);
  else
  {
	unsigned int i;
	for (i= 0; i < pageData->pages_num; i++)
	  [_pages addObject:[self buildPage: dictFromPageData(pageData->pages+i)]];
	[tabView selectTabViewItem:[tabView tabViewItemAtIndex:0]];
	myx_free_health_pages(pageData);
  }
  
  if (_serverVariables)
    [self performOnAllGraphs:@selector(updateServerVariables:)
				withArgument:[NSValue valueWithPointer:_serverVariables]];
}


@end


@implementation MAHealth

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_45.png"];
}

+ (NSString*)label
{
  return @"Health";
}

+ (NSString*)toolTip
{
  return @"Server Health and Statistics.";
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"Health" panelOwner: owner];
  if (self)
  {
    MYX_LIB_ERROR xerr;
    MYX_ADMIN_LIB_ERROR err;
    NSString *path;
    id ds;
    
    _defaultFrame= [[self topView] frame];
        
    _editableIcon= [[NSImage imageNamed:@"variable_editable_mac.png"] retain];
//    _nonEditableIcon= [[NSImage imageNamed:@"variable_mac.png"] retain];
	_nonEditableIcon= [[NSImage alloc] initWithSize:NSMakeSize(16.0,16.0)];
            
    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_server_variables
                          finishedSelector:@selector(variablesArrived:result:)
                            argument:@"server"
                                  target:self];
    [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_status_variables
                          finishedSelector:@selector(variablesArrived:result:)
                            argument:@"status"
                                  target:self];
   
    _pages= [[NSMutableArray alloc] init];
	
	[self reloadGraphs];
    
    path= [[NSBundle mainBundle] pathForResource:@"mysqlx_translations_administrator"
                                          ofType:@"xml"];
    {
      NSString *path0= [[NSBundle mainBundle] pathForResource:@"mysqlx_translations_general"
                                                       ofType:@"xml"];
      //_trans= myx_init_trans([path0 UTF8String], [path UTF8String], "en", &xerr);
    }
    
    path= [[NSBundle mainBundle] pathForResource:@"mysqladmin_status_variables"
                                          ofType:@"xml"];
    _statusVariableListing= myx_get_variables_listing((char*)[path UTF8String], &err);

    path= [[NSBundle mainBundle] pathForResource:@"mysqladmin_system_variables"
                                          ofType:@"xml"];
    _serverVariableListing= myx_get_variables_listing((char*)[path UTF8String], &err);
    
    ds= [[ListingDataSource alloc] initWithListing:_statusVariableListing];
                                       //translation:_trans];
    [statusIndex setDataSource:ds];
    MXExpandOutline(statusIndex,YES);
    
    ds= [[ListingDataSource alloc] initWithListing:_serverVariableListing];
                                       //translation:_trans];
    [serverIndex setDataSource:ds];
    MXExpandOutline(serverIndex,YES);
    
    [[[serverTable tableColumnWithIdentifier:@"description"] dataCell] setFont:
      [NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];

    [[[statusTable tableColumnWithIdentifier:@"description"] dataCell] setFont:
      [NSFont systemFontOfSize:[NSFont smallSystemFontSize]]];

    //ds= [[VariableDataSource alloc] initWithTranslations:_trans owner:self];
    ds= [[VariableDataSource alloc] initWithOwner:self];
    [ds setValues:_statusVariables];
    [statusTable setDataSource:ds];
    
    //ds= [[VariableDataSource alloc] initWithTranslations:_trans owner:self];
    ds= [[VariableDataSource alloc] initWithOwner:self];
    [ds setValues:_serverVariables];
    [serverTable setDataSource:ds];
    
    [self update:nil];
    
    _cancelThread= NO;
    
    if ([_owner serverInfo])
      [NSThread detachNewThreadSelector:@selector(updateThread:)
                               toTarget:self
                             withObject:[NSValue valueWithPointer:[[_owner serverInfo] createUserConnection]]];
  }
  return self;
}


- (void)revertAlertDidEnd:(NSAlert *)alert returnCode:(int)returnCode
                  contextInfo:(void *)contextInfo
{
  [[alert window] orderOut:self];
  
  if (returnCode == NSAlertDefaultReturn)
  {
    _saveGraphs= NO;
    [[NSFileManager defaultManager] removeFileAtPath:[[MPreferences preferences] pathForFile:@"Administrator/mysqladmin_custom_health.xml"]
                                             handler:nil];
    [self reloadGraphs];
  }
}


- (IBAction)revertToFactory:(id)sender
{
  NSAlert *alert = [NSAlert alertWithMessageText:@"Reset Customizations?"
                                   defaultButton:@"Reset" alternateButton:@"Cancel" otherButton:nil        
                       informativeTextWithFormat:@"This will reset all graphs to their default factory settings."];

  [alert beginSheetModalForWindow:[topBox window] modalDelegate:self
                   didEndSelector:@selector(revertAlertDidEnd:returnCode:contextInfo:) contextInfo:NULL];
}


- (IBAction)refreshServerVariables:(id)sender
{
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_server_variables
                      finishedSelector:@selector(variablesArrived:result:)
                              argument:@"server"
                                target:self];
}

- (IBAction)refreshStatusVariables:(id)sender
{
  [[_owner dispatcher] performCallback:(void*(*)(MYSQL*))myx_get_status_variables
                      finishedSelector:@selector(variablesArrived:result:)
                              argument:@"status"
                                target:self];
}


- (void)dealloc
{
  if (_statusVariables)
    myx_free_variables(_statusVariables);
  if (_serverVariables)
    myx_free_variables(_serverVariables);      
  myx_free_variables_listing(_statusVariableListing);
  myx_free_variables_listing(_serverVariableListing);
  //myx_free_trans(_trans);
  [_pages release];
  [_editableIcon release];
  [_nonEditableIcon release];
  [super dealloc];
}

- (BOOL)willClose
{
  _cancelThread= YES;
  if (_saveGraphs)
  {
    [self saveGraphSettings];
  }  
  return YES;
}


- (BOOL)validateMenuItem:(id <NSMenuItem>)anItem
{
  // First, the menuForEvent: method for each object with a menu attached
  // will call setRepresentedObject on the menu item with the object itself.
  // At this stage, we will change the representedObject of each item to 
  // the object that matters for the command (group for New Graph, 
  // graph for Edit Graph etc).
  id object= [anItem representedObject];
  id robj= nil;
  int type= 0;

  if ([object isKindOfClass:[MAHealthGroup class]])
  {
    type= 'G';
    robj= object;
  }
  else if ([object isKindOfClass:[MAHealthPage class]])
  {
    type= 'P';
    robj= object;
  }
  else if ([[object attachedObject] isKindOfClass:[MAHealthGraph class]])
  {
    robj= [object attachedObject];
    type= 'H';
  }
  else if ([[object attachedObject] isKindOfClass:[MAHealthGroup class]])
  {
    type= 'G';
    robj= [object attachedObject];
  }
  else if ([[object attachedObject] isKindOfClass:[MAHealthPage class]])
  {
    type= 'P';
    robj= [object attachedObject];
  }
  else if ([[object superview] isKindOfClass:[MAHealthGraph class]])
  {
    type= 'H';
    robj= [object superview];
  }
  else
    NSLog(@"Unknown %@", [object className]);
    
  if ([anItem menu] == editMenu)
  {
    id theGroup= nil, thePage= nil, theGraph= nil;
    
    switch (type)
    {
      case 'H':
        theGraph= robj;
        theGroup= [(MAMenuContextView*)[robj superview] attachedObject];
        thePage= [theGroup page];
        break;
      case 'G':
        theGraph= nil;
        theGroup= robj;
        thePage= [robj page];
        break;
      case 'P':
        theGraph= nil;
        theGroup= nil;
        thePage= robj;
        break;
    }
    
    switch ([editMenu indexOfItem:anItem])
    {
      case 0:
        [anItem setRepresentedObject:theGroup];
        return YES;
      case 1:
      case 2:
        [anItem setRepresentedObject:theGraph];
        if (type == 'H')
          return YES;
        else
          return NO;

      case 4:
        [anItem setRepresentedObject:thePage];
        return YES;
      case 5:
      case 6:
        [anItem setRepresentedObject:theGroup];
        if (type == 'G' || type == 'H')
          return YES;
        else
          return NO;
      
      case 8:
      case 9:
      case 10:
        [anItem setRepresentedObject:thePage];
        return YES;
        
      case 12:
        [anItem setRepresentedObject:nil];
        return YES;
    }
  }
  else
  {
    if ([tabView indexOfTabViewItem:[tabView selectedTabViewItem]] >=
        [tabView numberOfTabViewItems]-2)
    {
      return NO;
    }
  
    [anItem setRepresentedObject:robj];

    switch ([[anItem menu] indexOfItem:anItem])
    {
      case 0:
        return YES;
      case 1:
      case 2:
        if (type == 'G')
          return YES;
        else
          return NO;
        
      case 4:
        return YES;
      case 5:
      case 6:
        if (type == 'P')
          return YES;
        else
          return NO;
        
      case 8:
        [anItem setRepresentedObject:nil];
        return YES;  
    }
  }
  return NO;
}

- (IBAction)editOK:(id)sender
{
  [NSApp stopModalWithCode:NSOKButton];
}

- (void)windowWillClose:(NSNotification*)notif
{
  [NSApp stopModalWithCode:NSCancelButton];
}


- (void)graphEditFinished:(id)sender
{
  NSDictionary *props= [sender graph];
  NSDictionary *graph= [sender editedGraphObject];
  MAHealthGroup *group= [graph objectForKey:@"group"];

  if (![graph objectForKey:@"graph"])
  {
	id g= [self buildGraph:props];
    [group addGraph:g];
	[g updateServerVariables:_serverVariables];
  }
  else
  {
    [[graph objectForKey:@"graph"] setProperties:props];
  }

  [[group page] rearrange];
  _saveGraphs= YES;
}

- (IBAction)newGraph:(id)sender
{
  MAGraphEditor *editor= [[MAGraphEditor alloc] init];

  [editor setEditedGraphObject:[NSDictionary dictionaryWithObjectsAndKeys:[sender representedObject],@"group", nil]];

  [editor setTarget:self];
  [editor setApplyAction:@selector(graphEditFinished:)];  
}

- (IBAction)editGraph:(id)sender
{
  MAGraphEditor *editor= [[MAGraphEditor editGraph:[[sender representedObject] properties]] retain];

  [editor setEditedGraphObject:[NSDictionary dictionaryWithObjectsAndKeys:[sender representedObject],@"graph",
    [(MAMenuContextView*)[[sender representedObject] superview] attachedObject], @"group", nil]];

  [editor setTarget:self];
  [editor setApplyAction:@selector(graphEditFinished:)];
}

- (IBAction)removeGraph:(id)sender
{
  MAHealthGroup *group;
  MAHealthGraph *graph= [sender representedObject];

  group= [(MAMenuContextView*)[graph superview] attachedObject];
  [group removeGraph:graph];  
  [[group page] rearrange];
  _saveGraphs= YES;
}


- (IBAction)newGroup:(id)sender
{
  MAHealthPage *page= [sender representedObject];
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"New Group"
														   labels:[NSArray arrayWithObject:@"Group Title:"]];
  NSArray *array;
  
  if ((array= [sheet runModal:[_owner window]]))
  {
    NSDictionary *group= [NSDictionary dictionaryWithObjectsAndKeys:
      [array objectAtIndex:0], @"title", nil];
    [page addGroup: [self buildGroup:group inPage:page]];
    [page rearrange];
    _saveGraphs= YES;
  }
}

- (IBAction)renameGroup:(id)sender
{
  MAHealthGroup *group= [sender representedObject];
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Rename Group"
														   labels:[NSArray arrayWithObject:@"Group Title:"]];
  NSArray *array;
  
  [sheet setDefaultValues:[NSArray arrayWithObject:[[group properties] objectForKey:@"title"]]];
  
  if ((array= [sheet runModal:[_owner window]]))
  {
    NSMutableDictionary *props= [NSMutableDictionary dictionaryWithDictionary:[group properties]];
    NSString *title;
    
    title= [array objectAtIndex:0];
    
    [props setObject:title forKey:@"title"];
    
    [group setProperties:props];  
    _saveGraphs= YES;
  }
}

- (IBAction)removeGroup:(id)sender
{
  MAHealthGroup *group= [sender representedObject];

  [[group page] removeGroup:group];
  _saveGraphs= YES;
}


- (IBAction)newPage:(id)sender
{
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"New Page"
														   labels:[NSArray arrayWithObject:@"Page Title:"]];
  NSArray *array;
  
  if ((array= [sheet runModal:[_owner window]]))
  {
    NSMutableDictionary *props= [NSDictionary dictionaryWithObjectsAndKeys:
      [array objectAtIndex:0], @"title",
      @"", @"decription",
      nil];

    MAHealthPage *page= [[MAHealthPage alloc] initWithProperties:props];
    
    [tabView insertTabViewItem:page atIndex:[tabView numberOfTabViewItems]-2];
    
    [page setMenu:pageMenu];
    [_pages addObject:page];
    [page release];
    
    _saveGraphs= YES;
  }
}

- (IBAction)renamePage:(id)sender
{
  MAHealthPage *page= [sender representedObject];
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Rename Page"
														   labels:[NSArray arrayWithObject:@"Page Title:"]];
  NSArray *array;

  [sheet setDefaultValues:[NSArray arrayWithObject:[[page properties] objectForKey:@"title"]]];

  if ((array= [sheet runModal:[_owner window]]))
  {
    NSMutableDictionary *props= [NSMutableDictionary dictionaryWithDictionary:[page properties]];
    NSString *title;
    
    title= [array objectAtIndex:0];
    
    [props setObject:title forKey:@"title"];
    
    [page setProperties:props];
    
    _saveGraphs= YES;
  }
}

- (IBAction)removePage:(id)sender
{
  MAHealthPage *page= [sender representedObject];
  
  [tabView removeTabViewItem:page];
  [_pages removeObject:page];
  
  _saveGraphs= YES;
}

@end
