#import "MACatalogs.h"
#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/NSString_extras.h>
#import <MySQLToolsCommon/MTreeDataSource.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#import <MySQLToolsCommon/MTableEditor.h>
#import <MySQLToolsCommon/MDialogs.h>
#import <MySQLToolsCommon/myxutil.h>
#import <MySQLToolsCommon/mxUtils.h>
#import <MySQLToolsCommon/MSQLEditor.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>
#import "MATableMaintenanceController.h"


@interface MACatalogIndexItem : MTreeItem
{
  @public
  MYX_TABLE_INDEX *index;
  MYX_TABLE_INDEX_COLUMN *column;
}
- (id)initRoot:(MYX_SCHEMA_ENTITY_STATUS*)item;
@end

@implementation MACatalogIndexItem
- (id)initRoot:(MYX_SCHEMA_ENTITY_STATUS*)item
{
  self= [super initWithTag:'C' repr:nil];
  if (self)
  {
    unsigned int i, j, k;
    for (i= 0; i < item->schema_entities_num; i++)
    {
      if (item->schema_entities[i].entity_type == MYX_ENTITY_TABLE)
      {
        MYX_TABLE_STATUS *table= (MYX_TABLE_STATUS*)item->schema_entities[i].entity;
        MACatalogIndexItem *tit=nil, *cit;
        for (j= 0; j < table->indexes_num; j++)
        {
          MYX_TABLE_INDEX *idx= table->indexes+j;
          if (strcmp((char*)idx->table_name, (char*)table->table_name)==0)
          {
            tit= [[MACatalogIndexItem alloc] initWithTag:'I' repr:[NSString stringWithUTF8String:table->table_name]];
            tit->index= idx;
            [self addChild:tit];
            [tit release];
            
            for (k= 0; k < idx->index_columns_num; k++)
            {
              cit= [[MACatalogIndexItem alloc] initWithTag:'C' repr:[NSString stringWithUTF8String:idx->key_name]];
              cit->column= idx->index_columns+k;
              [tit addChild:cit];
              [cit release];
            }
          }
        }
      }
    }
  }
  return self;
}

- (id)valueForIdentifier:(NSString*)ident
{
  if ([self tag] == 'I')
  {
    if ([ident isEqualToString:@"column"])
    {
      return [NSString stringWithUTF8String:index->table_name];
    }
    else if ([ident isEqualToString:@"name"])
    {
      return [NSString stringWithUTF8String:index->key_name];
    }
    else if ([ident isEqualToString:@"type"])
    {
      return [NSString stringWithUTF8String:index->index_type];
    }
    else if ([ident isEqualToString:@"unique"])
    {
      return index->unique?@"UNIQUE":@"-";
    }
    else if ([ident isEqualToString:@"null"])
    {
      return index->not_null?@"NOT NULL":@"NULL";
    }
  }
  else
  {
    if ([ident isEqualToString:@"column"])
    {
      return [NSString stringWithUTF8String:column->column_name];
    }
    else if ([ident isEqualToString:@"seq"])
    {
      return [NSString stringWithUTF8String:column->seq_in_index];
    }
    else if ([ident isEqualToString:@"collation"])
    {
      return *column->collation=='A'?@"Ascending":@"";
    }
  }
  return @"";
}

@end


@interface MACatalogs(Private)
- (void)objectStatusArrived:(id)arg result:(void*)res;

- (MYX_SCHEMA*)selectedSchema;

- (void*)objectOfType:(MYX_ENTITY_TYPE)type atIndex:(unsigned int)index;
@end


@implementation MACatalogs(Private)

- (void)objectStatusArrived:(id)arg result:(void*)res
{
  long long index_length, data_length, row_count;
  unsigned int i;
  
  if (_objectStatus)
    myx_free_schema_entity_status(_objectStatus);
  _objectStatus= (MYX_SCHEMA_ENTITY_STATUS*)res;
  if (_objectStatus)
    [[indexOutline dataSource] setRoot:[[[MACatalogIndexItem alloc] initRoot:_objectStatus] autorelease]];
  else
    [[indexOutline dataSource] setRoot:nil];
  
  g_free(_tableIndices);
  _tableIndices= NULL;
  g_free(_viewIndices);
  _viewIndices= NULL;
  g_free(_funcIndices);
  _funcIndices= NULL;
  g_free(_procIndices);
  _procIndices= NULL;
  
  // calculate summaries
  index_length= 0;
  data_length= 0;
  row_count= 0;
  _tableCount= 0;
  _viewCount= 0;
  _procCount= 0;
  _funcCount= 0;
  if (_objectStatus)
  {
    for (i= 0; i < _objectStatus->schema_entities_num; i++)
    {
      MYX_TABLE_STATUS *status;
      
      switch (_objectStatus->schema_entities[i].entity_type)
      {
        case MYX_ENTITY_TABLE:
          _tableCount++;
          break;
        case MYX_ENTITY_VIEW:
          _viewCount++;
          break;
        case MYX_ENTITY_PROC:
          _procCount++;
          break;
        case MYX_ENTITY_FUNC:
          _funcCount++;
          break;
      }
      
      if (_objectStatus->schema_entities[i].entity_type != MYX_ENTITY_TABLE)
        continue;
      status= (MYX_TABLE_STATUS*)_objectStatus->schema_entities[i].entity;
      data_length+= strtoll((char*)status->data_length?:"0", NULL, 0);
      index_length+= strtoll((char*)status->index_length?:"0", NULL, 0);
      row_count+= strtoll((char*)status->rows?:"0", NULL, 0);
    }
    
    _tableIndices= g_new(int, _tableCount);
    _viewIndices= g_new(int, _viewCount);
    _funcIndices= g_new(int, _funcCount);
    _procIndices= g_new(int, _procCount);
    
    _tableCount= _viewCount= _procCount= _funcCount= 0;
    for (i= 0; i < _objectStatus->schema_entities_num; i++)
    {
      switch (_objectStatus->schema_entities[i].entity_type)
      {
        case MYX_ENTITY_TABLE:
          _tableIndices[_tableCount]= i;
          _tableCount++;
          break;
        case MYX_ENTITY_VIEW:
          _viewIndices[_viewCount]= i;
          _viewCount++;
          break;
        case MYX_ENTITY_PROC:
          _procIndices[_procCount]= i;
          _procCount++;
          break;
        case MYX_ENTITY_FUNC:
          _funcIndices[_funcCount]= i;
          _funcCount++;
          break;
      }
    }
  }
  [[tableStatsMatrix cellWithTag:111] setStringValue:[NSString stringWithFormat:@"No. of Tables: %i", _tableCount]];
  [[tableStatsMatrix cellWithTag:112] setStringValue:[NSString stringWithFormat:@"No. of Rows: %lli", row_count]];
  [[tableStatsMatrix cellWithTag:113] setStringValue:[NSString stringWithFormat:@"Data Length: %@B", [NSString stringWithMultNumber:data_length]]];
  [[tableStatsMatrix cellWithTag:114] setStringValue:[NSString stringWithFormat:@"Index Length: %@B", [NSString stringWithMultNumber:index_length]]];
  
  [tablesTable reloadData];
  [viewTable reloadData];
  [spTable reloadData];
  [funcTable reloadData];
  [indexOutline reloadData];
  MXExpandOutline(indexOutline, YES);
}


- (void*)objectOfType:(MYX_ENTITY_TYPE)type atIndex:(unsigned int)index
{
  switch (type)
  {
    case MYX_ENTITY_TABLE:
      return _objectStatus->schema_entities[_tableIndices[index]].entity;
    case MYX_ENTITY_VIEW:
      return _objectStatus->schema_entities[_viewIndices[index]].entity;
    case MYX_ENTITY_FUNC:
      return _objectStatus->schema_entities[_funcIndices[index]].entity;
    case MYX_ENTITY_PROC:
      return _objectStatus->schema_entities[_procIndices[index]].entity;
  }
  return NULL;
}

- (MYX_SCHEMA*)selectedSchema
{
  int row= [schemataOutline selectedRow];
  MSchemaItem *item= row >= 0 ? [schemataOutline itemAtRow:row] : nil;
  if (!item || item->type != MSchemaItemType) 
    return nil;
  return [item schema];
}

@end


@implementation MACatalogs

- (IBAction)toggleTableDetails:(id)sender
{
  NSRect trect= (sender != nil ? [[topBox window] frame] : [topBox frame]);
  float bheight= [tableDetailsTab frame].size.height;
  
  // change autoresizing, so that the layout stays the way we want as we resize the window
  [tableStatsMatrix setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin];
  [tableDetailToggleButton setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin];
  [[topBox viewWithTag:14] setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin];
  [tableScrollView setAutoresizingMask:NSViewWidthSizable|NSViewMinYMargin];
  [[tableDetailsTab superview] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
  
  if ([tableDetailToggleButton state] == NSOnState)
  {    
    if (sender == nil)
    {
      trect.size.height+= bheight;
      [topBox setFrame:trect];
    }
    else
    {      
      trect= [tableScrollView frame];
      trect.size.height -= bheight;
      trect.origin.y += bheight;
      [tableScrollView setFrame:trect];
      
      trect= [tableStatsMatrix frame];
      trect.origin.y += bheight;
      [tableStatsMatrix setFrame:trect];
      
      trect= [[topBox viewWithTag:14] frame];
      trect.origin.y += bheight;
      [[topBox viewWithTag:14] setFrame:trect];
      
      trect= [[tableDetailsTab superview] frame];
      trect.size.height += bheight;
      [[tableDetailsTab superview] setFrame:trect];
      
      trect= [tableDetailToggleButton frame];
      trect.origin.y += bheight;
      [tableDetailToggleButton setFrame:trect];
      
      [topBox setNeedsDisplay:YES];
    }
  }
  else
  {
    if (sender == nil)
    {
      trect.size.height-= bheight;
      [topBox setFrame:trect];
    }
    else
    {
      trect= [tableScrollView frame];
      trect.size.height += bheight;
      trect.origin.y -= bheight;
      [tableScrollView setFrame:trect];
      
      trect= [tableStatsMatrix frame];
      trect.origin.y -= bheight;
      [tableStatsMatrix setFrame:trect];
      
      trect= [[topBox viewWithTag:14] frame];
      trect.origin.y -= bheight;
      [[topBox viewWithTag:14] setFrame:trect];
      
      trect= [[tableDetailsTab superview] frame];
      trect.size.height -= bheight;
      [[tableDetailsTab superview] setFrame:trect];
      
      trect= [tableDetailToggleButton frame];
      trect.origin.y -= bheight;
      [tableDetailToggleButton setFrame:trect];
      
      [topBox setNeedsDisplay:YES];
    }
  }  
  
  [[tableDetailsTab superview] setAutoresizingMask:NSViewWidthSizable];
  [tableDetailToggleButton setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin|NSViewMaxXMargin];
  [[topBox viewWithTag:14] setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin|NSViewMaxXMargin];
  [tableScrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];
  [tableStatsMatrix setAutoresizingMask:NSViewWidthSizable|NSViewMaxYMargin];
}


- (void)openTableMaintenance:(int)operation
{
  MATableMaintenanceController *mnt;
  NSMutableArray *tables= [NSMutableArray arrayWithCapacity:1];
  NSIndexSet *rows= [tablesTable selectedRowIndexes];
  unsigned int i;
  MYX_SCHEMA *schema= [self selectedSchema];
  if (!schema)
    return;
  
  for (i= [rows firstIndex]; i <= [rows lastIndex]; i= [rows indexGreaterThanIndex:i])
  {
    MYX_TABLE_STATUS *table= [self objectOfType:MYX_ENTITY_TABLE atIndex:i];
    
    [tables addObject:[NSString stringWithFormat:@"%s.%s",schema->schema_name,
      table->table_name]];
  }
  
  if ([tables count] == 0)
    return;
  
  mnt= [[MATableMaintenanceController alloc] initWithWindowNibName:@"TableMaintenance"];
  [mnt loadWindow];
  [mnt runWithTables:tables dispatcher:[_owner dispatcher] operation:operation];
  
  [mnt close];
  
  [mnt release];
}

- (IBAction)checkTable:(id)sender
{
  [self openTableMaintenance:'C'];
}

- (IBAction)repairTable:(id)sender
{
  [self openTableMaintenance:'R'];
}

- (IBAction)optimizeTable:(id)sender
{
  [self openTableMaintenance:'O'];
}


- (void)tableChangedObserver:(NSNotification*)notif
{
  [self refreshContents:nil];
}

- (IBAction)createTable:(id)sender
{
  MYX_SCHEMA *schema= [self selectedSchema];
  MTableEditor *editor;
  
  if (!schema)
    return;
  
  editor= [[MTableEditor alloc] init];
  
  [editor setConnection:[_owner mysql]];
  [editor setCatalogs:[_owner catalogList]];
  [editor showNewTableForCatalog:NSStr(schema->catalog_name)
                          schema:NSStr(schema->schema_name)];
  
  [editor setReleaseOnClose:YES];  
}


- (void)editorClose:(NSNotification*)notif
{
  id window= [notif object];
  unsigned int i, c= [_editors count];
  for (i= 0; i < c; i++)
  {
    id obj= [_editors objectAtIndex:i];
    if ([obj window] == window)
    {
      [_editors removeObject:obj];
      break;
    }
  }
}

- (IBAction)editTable:(id)sender
{
  MYX_SCHEMA *schema= [self selectedSchema];
  int row= [tablesTable selectedRow];
  unsigned int i;
  
  if (!schema)
    return;
    
  if (row >= 0)
  {
    MYX_TABLE_STATUS *ts= [self objectOfType:MYX_ENTITY_TABLE atIndex:row];
    BOOL found= NO;
    
    for (i=0; i < [_editors count]; i++)
    {
      MTableEditor *e= [_editors objectAtIndex:i];
      
      if (strcmp([[e catalog] UTF8String], schema->catalog_name)==0
          && strcmp([[e schema] UTF8String], schema->schema_name)==0
          && strcmp([[e table] UTF8String], ts->table_name)==0)
      {
        [[e window] makeKeyAndOrderFront:nil];
        found= YES;
        break;
      }
    }

    if (!found)
    {
      MTableEditor *editor= [[MTableEditor alloc] init];
      
      [editor setConnection:[_owner mysql]];
      [editor setCatalogs:[_owner catalogList]];
      [editor showTable:NSStr((char*)ts->table_name) catalog:NSStr(schema->catalog_name)
                 schema:NSStr(schema->schema_name)];
      
      [editor setReleaseOnClose:YES];
      
      [_editors addObject:editor];
    }
  }
}


static void *dropTables(MYSQL *mysql, void *schema, void *tables_arg)
{
  unsigned int i;
  char **tables= (char**)tables_arg;
  long long int affected_rows;
  
  for (i= 0; tables[i]!=NULL; i++)
  {
    MYX_LIB_ERROR error;
    char *query= g_strdup_printf("DROP TABLE `%s`.`%s`",
                                 (char*)schema, tables[i]);
    
    myx_query_execute_direct(mysql, query, &error, &affected_rows);
    g_free(query);
    
    if (error != MYX_NO_ERROR)
    {
      return [[NSString alloc] initWithFormat: @"Error dropping table %s: %s", 
        tables[i], mysql_error(mysql)];
    }
  }
  return nil;
}


- (void)dropTablesAlertDidEnd:(NSAlert *)sheet 
                   returnCode:(int)returnCode
                  contextInfo:(void *)contextInfo
{
  MYX_SCHEMA *schema= [self selectedSchema];
  NSString *error;
  
  [[sheet window] orderOut:self];
  
  if (!schema || returnCode != NSAlertDefaultReturn)
  {
    g_free(contextInfo);
    return;
  }
  
  error= [[_owner dispatcher] performCallback: dropTables
                                     argument: schema->schema_name
                                     argument: (id)contextInfo 
                                waitForWindow: [topBox window]
                                      message: @"Dropping tables..."];
  if (error)
  {
    NSRunAlertPanel(@"Error", error, nil, nil, nil);
  }
  
  g_free(contextInfo);
  
  [self refreshContents:nil];
}


- (IBAction)dropTable:(id)sender
{
  NSIndexSet *indexes= [tablesTable selectedRowIndexes];
  unsigned int i, j;
  char **tables;
  
  if ([indexes count] == 0)
    return;
  
  tables= g_new(char*, [indexes count]+1);
  
  j= 0;
  for (i= [indexes firstIndex]; i != NSNotFound; i= [indexes indexGreaterThanIndex: i])
  {
    MYX_TABLE_STATUS *status= [self objectOfType:MYX_ENTITY_TABLE atIndex:i];
    tables[j++]= status->table_name;
  }
  tables[j]= NULL;
  
  {  
    char *table_names= g_strjoinv(", ", tables);
    NSAlert *alert = [NSAlert alertWithMessageText:@"Drop Selected Tables"
                                     defaultButton:@"Drop Tables" 
                                   alternateButton:@"Cancel" 
                                       otherButton:nil
                         informativeTextWithFormat:@"The following tables will be dropped and all data contained in them will be permanently lost!\n\n %s",
      table_names];
    g_free(table_names);
    
    [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                     didEndSelector:@selector(dropTablesAlertDidEnd:returnCode:contextInfo:) 
                        contextInfo:tables];
  }
}


- (IBAction)createView:(id)sender
{
  MSQLEditor *editor;
  editor= [[[MSQLEditor alloc] initWithMySQL:[_owner mysql]] autorelease];
  
  [editor setScript:[NSString stringWithFormat:@"CREATE VIEW `%s`.`new_view` AS\nSELECT 1",[self selectedSchema]->schema_name]];
  
  [[editor window] setTitle:@"Create View"];
  
again:
  
  if ([NSApp runModalForWindow:[editor window]] == NSOKButton)
  {
    MYX_LIB_ERROR error;
    
    error= [[_owner dispatcher] performQuery: [editor script]
                               waitForWindow: [topBox window]
                                     message: @"Executing Create View..."];
    if (error == MYX_SQL_ERROR)
      MXRunAlertPanelWithMySQLError(@"Error executing Create View.",@"",[_owner mysql]);
    else if (error != MYX_NO_ERROR)
      MXRunAlertPanelWithError(@"Error", @"Error executing Create View.", error);
    
    if (error != MYX_NO_ERROR) goto again;
    
    [self refreshContents:nil];
  }
}


- (IBAction)editView:(id)sender
{
  MSQLEditor *editor;
  int row= [viewTable selectedRow];
  MYX_DBM_VIEW_DATA *view;
  MYX_VIEW_STATUS *viewS;
  
  if (row < 0) return;
  
  myx_push_schema([_owner mysql], [self selectedSchema]->schema_name);
  
  editor= [[[MSQLEditor alloc] initWithMySQL:[_owner mysql]] autorelease];
  
  viewS= [self objectOfType:MYX_ENTITY_VIEW atIndex:row];
retry:  
  view= [[_owner dispatcher] performCallback: (void*(*)(MYSQL*,void*,void*,void*))myx_dbm_get_view_data
                                    argument: [self selectedSchema]->catalog_name    
                                    argument: [self selectedSchema]->schema_name
                                    argument: viewS->view_name
                               waitForWindow: [topBox window]
                                     message: @"Getting view definition..."];    
  if (!view && ![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
    goto retry;
  
  if (!view)
  {
    if (mysql_error([_owner mysql]) != 0)
    {
      MXRunAlertPanelWithMySQLError(@"Error",
                                    [NSString stringWithFormat:@"Could not retrieve definition of view %s",viewS->view_name],
                                    [_owner mysql]);
    }
    myx_pop_schema([_owner mysql]);
    return;
  }
  [editor setScript:NSStr(view->definition)];
  
  [[editor window] setTitle:@"Edit View"];

again:  
  if ([NSApp runModalForWindow:[editor window]] == NSOKButton)
  {
    MYX_LIB_ERROR error;

    error= [[_owner dispatcher] performQuery: [NSString stringWithFormat:@"DROP VIEW `%s`.`%s`",[self selectedSchema]->schema_name,viewS->view_name]
                               waitForWindow: [topBox window]
                                     message: @"Dropping View..."];
    
    if (error == MYX_SQL_ERROR)
      MXRunAlertPanelWithMySQLError(@"Error", @"Error replacing View.",[_owner mysql]);
    else if (error != MYX_NO_ERROR)
      MXRunAlertPanelWithError(@"Error", @"Error replacing View.", error);    
    else
    {
      error= [[_owner dispatcher] performQuery: [editor script]
                                 waitForWindow: [topBox window]
                                       message: @"Executing Create View..."];
      if (error == MYX_SQL_ERROR)
        MXRunAlertPanelWithMySQLError(@"Error executing Create View.",@"",[_owner mysql]);
      else if (error != MYX_NO_ERROR)
        MXRunAlertPanelWithError(@"Error", @"Error executing Create View.", error);
    }
    if (error != MYX_NO_ERROR) goto again;
    
    [self refreshContents:nil];
  }
  myx_pop_schema([_owner mysql]);
}


- (void)dropViewAlertDidEnd:(NSAlert *)sheet 
                 returnCode:(int)returnCode
                contextInfo:(void *)contextInfo
{
  MYX_SCHEMA *schema= [self selectedSchema];
  const char *view= (char*)contextInfo;
  
  MYX_LIB_ERROR error;
  
  [[sheet window] orderOut:self];
  
  if (!schema || returnCode != NSAlertDefaultReturn)
  {
    g_free(contextInfo);
    return;
  }
  
retry:
  error= (MYX_LIB_ERROR)[[_owner dispatcher] performQuery: [NSString stringWithFormat:@"DROP VIEW `%s`.`%s`", [self selectedSchema]->schema_name, view]
                                            waitForWindow: [topBox window]
                                                  message: @"Dropping view..."];
  if (error != MYX_NO_ERROR)
  {
    if (![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
      goto retry;
    
    NSRunAlertPanel(@"Error",
                    [NSString stringWithFormat: @"Error dropping view %s: %s", 
                      view, mysql_error([_owner mysql])], @"OK", nil, nil);
  }
  
  g_free(contextInfo);
  
  [self refreshContents:nil];
}


- (IBAction)dropView:(id)sender
{
  MYX_VIEW_STATUS *view;
    
  view= [self objectOfType:MYX_ENTITY_VIEW atIndex:[viewTable selectedRow]];
  
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop View?"
                                   defaultButton:@"Drop View"
                                 alternateButton:@"Cancel" 
                                     otherButton:nil
                       informativeTextWithFormat:@"The '%s' view will be dropped permanently.",
    view->view_name];
  
  [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                   didEndSelector:@selector(dropViewAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:g_strdup(view->view_name)];
}


- (IBAction)createProcedure:(id)sender
{
  NSString *templ;
  MSQLEditor *editor;
  NSString *what;

  if ([sender tag] < 50)
  {
    templ= [NSString stringWithFormat:@"CREATE PROCEDURE `%s`.`new_procedure` ()\n"\
      "BEGIN\n\n"\
      "END\n", [self selectedSchema]->schema_name];
    what= @"Create Stored Procedure";
  }
  else
  {
    templ= [NSString stringWithFormat:@"CREATE FUNCTION `%s`.`new_function` ()\nRETURNS INT\n"\
      "BEGIN\n\n"\
      "    RETURN 0;\n"\
      "END\n", [self selectedSchema]->schema_name];    
    what= @"Create Function";
  }
  
  editor= [[[MSQLEditor alloc] initWithMySQL:[_owner mysql]] autorelease];

  [editor setScript:templ];
  
  [[editor window] setTitle:what];
  
again:
  
  if ([NSApp runModalForWindow:[editor window]] == NSOKButton)
  {
    MYX_LIB_ERROR error;
    
    error= [[_owner dispatcher] performQuery: [editor script]
                               waitForWindow: [topBox window]
                                     message: [NSString stringWithFormat:@"Executing %@...",what]];
    if (error == MYX_SQL_ERROR)
    {
      if (![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
        goto again;
      
      MXRunAlertPanelWithMySQLError([NSString stringWithFormat:@"Error executing %@.",what], @"", [_owner mysql]);
    }
    else if (error != MYX_NO_ERROR)
      MXRunAlertPanelWithError(@"Error", [NSString stringWithFormat:@"Error executing %@.",what], error);

    if (error != MYX_NO_ERROR) goto again;
    
    [self refreshContents:nil];
  }  
}


- (IBAction)editProcedure:(id)sender
{
  MYX_SCHEMA_STORED_PROCEDURE *routine;
  MYX_DBM_STORED_PROCEDURE_DATA *spdata;
  MSQLEditor *editor;
  NSString *sptype;
  
  if ([sender tag] < 50)
  {
    routine= [self objectOfType:MYX_ENTITY_PROC atIndex:[spTable selectedRow]];
    sptype= @"Procedure";
  }
  else
  {
    routine= [self objectOfType:MYX_ENTITY_FUNC atIndex:[funcTable selectedRow]];
    sptype= @"Function";
  }
 
  editor= [[[MSQLEditor alloc] initWithMySQL:[_owner mysql]] autorelease];
    
retry:
  spdata= [[_owner dispatcher] performCallback: (void*(*)(MYSQL*,void*,void*,void*,void*))myx_dbm_get_sp_data
                                       argument: [self selectedSchema]->catalog_name
                                       argument: [self selectedSchema]->schema_name
                                       argument: routine->name
                                       argument: (void*)routine->sp_type
                                  waitForWindow: [topBox window]
                                        message: @"Getting routine definition..."];    
  if (!spdata)
  {
    if (![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
      goto retry;
    
    if (mysql_error([_owner mysql]) != 0)
    {
      MXRunAlertPanelWithMySQLError(@"Error",
                                    [NSString stringWithFormat:@"Could not retrieve definition of routine %s",routine->name],
                                    [_owner mysql]);
    }
    [editor close];
    [editor release];
    return;
  }
  [editor setScript:NSStr(spdata->definition)];
  
  if ([sender tag] < 50)
    [[editor window] setTitle:@"Edit Stored Procedure"];
  else
    [[editor window] setTitle:@"Edit Function"];

again:
  
  if ([NSApp runModalForWindow:[editor window]] == NSOKButton)
  {
    MYX_LIB_ERROR error;
    
    myx_use_schema([_owner mysql], [self selectedSchema]->schema_name);
    
    error= [[_owner dispatcher] performQuery: [NSString stringWithFormat:@"DROP %@ IF EXISTS `%s`.`%s`",sptype,[self selectedSchema]->schema_name,spdata->name]
                               waitForWindow: [topBox window]
                                     message: @"Dropping Old Routine..."];
    
    if (error == MYX_SQL_ERROR)
      MXRunAlertPanelWithMySQLError(@"Error", [NSString stringWithFormat:@"Error replacing %@.",sptype],[_owner mysql]);
    else if (error != MYX_NO_ERROR)
      MXRunAlertPanelWithError(@"Error", [NSString stringWithFormat:@"Error replacing %@.",sptype], error);    
    else
    {
      error= [[_owner dispatcher] performQuery: [editor script]
                                 waitForWindow: [topBox window]
                                       message: [NSString stringWithFormat:@"Executing Create %@...",sptype]];
      if (error == MYX_SQL_ERROR)
        MXRunAlertPanelWithMySQLError([NSString stringWithFormat:@"Error executing Create %@.",sptype], @"", [_owner mysql]);
      else if (error != MYX_NO_ERROR)
        MXRunAlertPanelWithError(@"Error", [NSString stringWithFormat:@"Error executing Create %@.", sptype], error);
    }
    if (error != MYX_NO_ERROR) goto again;
    
    [self refreshContents:nil];
  }
}


- (void)dropRoutineAlertDidEnd:(NSAlert *)sheet 
                    returnCode:(int)returnCode
                   contextInfo:(void *)contextInfo
{
  MYX_SCHEMA *schema= [self selectedSchema];
  MYX_SCHEMA_STORED_PROCEDURE *routine= (MYX_SCHEMA_STORED_PROCEDURE*)contextInfo;
  NSString *query;
  
  MYX_LIB_ERROR error;
  
  [[sheet window] orderOut:self];
  
  if (!schema || returnCode != NSAlertDefaultReturn)
  {
    g_free(contextInfo);
    return;
  }
  
  query= [NSString stringWithFormat:@"DROP %@ `%s`.`%s`", routine->sp_type == MSPT_FUNCTION ? @"FUNCTION" : @"PROCEDURE",
    [self selectedSchema]->schema_name, routine->name];
  
  error= (MYX_LIB_ERROR)[[_owner dispatcher] performQuery: query
                                            waitForWindow: [topBox window]
                                                  message: @"Dropping routine..."];
  if (error != MYX_NO_ERROR)
  {
    NSRunAlertPanel(@"Error",
                    [NSString stringWithFormat: @"Error dropping routine %s: %s", 
                      routine->name, mysql_error([_owner mysql])], @"OK", nil, nil);
  }
  
  [self refreshContents:nil];
}


- (IBAction)dropProcedure:(id)sender
{
  MYX_SCHEMA_STORED_PROCEDURE *routine;
  NSAlert *alert;
  
  if ([sender tag] < 50)
  {
    routine= [self objectOfType:MYX_ENTITY_PROC atIndex:[spTable selectedRow]];
    
    alert = [NSAlert alertWithMessageText:@"Drop Stored Procedure?"
                            defaultButton:@"Drop SP"
                          alternateButton:@"Cancel" 
                              otherButton:nil
                informativeTextWithFormat:@"The '%s' stored procedure will be dropped permanently.",
      routine->name];
  }
  else
  {
    routine= [self objectOfType:MYX_ENTITY_FUNC atIndex:[funcTable selectedRow]];
  
    alert = [NSAlert alertWithMessageText:@"Drop Function?"
                            defaultButton:@"Drop Function"
                          alternateButton:@"Cancel" 
                              otherButton:nil
                informativeTextWithFormat:@"The '%s' function will be dropped permanently.",
      routine->name];
  }
  [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                   didEndSelector:@selector(dropRoutineAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:routine];
}


- (IBAction)refreshSchemas:(id)sender
{
  [_owner refreshSchemata];
  [schemataOutline reloadData];
}

- (IBAction)refreshContents:(id)sender
{
  MYX_SCHEMA *schema= [self selectedSchema];
  if (schema)
  {
    void *res;
retry:
    res=[[_owner dispatcher] performCallback:(void*(*)(MYSQL*,void*,void*))myx_get_schema_entity_status
                                    argument:schema->catalog_name
                                    argument:schema->schema_name
                               waitForWindow:[_owner window]
                                     message:[NSString stringWithFormat:@"Fetching objects from '%s'...",schema->schema_name]];
    if (!res && ![[_owner dispatcher] checkConnection] && [[_owner dispatcher] reconnect])
      goto retry;
    
    [self objectStatusArrived:nil result:res];
    
    [tabView setHidden:NO];
    [emptyText setHidden:YES];
  }
  else
  {
    [tabView setHidden:YES];
    [emptyText setHidden:NO];

    [self objectStatusArrived:nil result:NULL];
  }
}

- (IBAction)searchCatalogs:(id)sender
{
  [_schemaDS performSearch:[sender stringValue]];
  [schemataOutline reloadData];
}

+ (NSImage*)icon
{
  return [NSImage imageNamed:@"OSX-Icons_51.png"];
}

+ (NSString*)label
{
  return @"Catalogs";
}

+ (NSString*)toolTip
{
  return @"Catalogs in the connected MySQL server.";
}

- (id)initWithOwner: (id<MAdministratorProtocol>)owner
{
  self= [super initWithNibFile: @"Catalogs" panelOwner: owner];
  if (self)
  {
    if (![[NSUserDefaults standardUserDefaults] boolForKey:@"CatalogDetailsShown"])
      [self toggleTableDetails:nil];
    _defaultFrame= [[self topView] frame];

    _editors= [[NSMutableArray alloc] init];
    
    _tableIcon= [[NSImage imageNamed:@"16x16_Table.png"] retain];
    _indexIcon= [[NSImage imageNamed:@"16x16_Index.png"] retain];
    _columnIcon= [[NSImage imageNamed:@"16x16_Field.png"] retain];
    _viewIcon= [[NSImage imageNamed:@"16x16_View.png"] retain];
    _procIcon= [[NSImage imageNamed:@"16x16_StoredProc.png"] retain];
    _funcIcon= [[NSImage imageNamed:@"16x16_StoredProc.png"] retain];
    
    [tablesTable setDoubleAction:@selector(editTable:)];
    [tablesTable setTarget:self];
    [indexOutline setDataSource:[[MTreeDataSource alloc] init]];
    
    [tablesTable sizeLastColumnToFit];
    [indexOutline sizeLastColumnToFit];    
    [schemataOutline sizeLastColumnToFit];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(tableChangedObserver:)
                                                 name:MTableEditorTableDidChange
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(editorClose:)
                                                 name:NSWindowWillCloseNotification
                                               object:nil];    
  }
  return self;
}

- (void)dealloc
{
  [_editors makeObjectsPerformSelector:@selector(close:) withObject:nil];
  [_editors release];

  [_tableIcon release];
  if (_objectStatus)
    myx_free_schema_entity_status(_objectStatus);
  g_free(_tableIndices);
  g_free(_viewIndices);
  g_free(_funcIndices);
  g_free(_procIndices);
  [_tableIcon release];
  [_indexIcon release];
  [_columnIcon release];
  [_viewIcon release];
  [_procIcon release];
  [_funcIcon release];
  [tableDetailsTab release];
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}


- (void)updateButtonSensitivity
{
  int row= [schemataOutline selectedRow];
  if (row>=0 && [schemataOutline levelForRow:row]>0)
  {
    [actionButton setEnabled:YES];
    [delSchemaButton setEnabled:YES];
    [delSchemaButton setImage:[NSImage imageNamed:@"del_normal.png"]];
    
    [[topBox viewWithTag:10] setEnabled:YES];
    
    [[topBox viewWithTag:30] setEnabled:YES];
    [[topBox viewWithTag:33] setEnabled:YES];
    
    [[topBox viewWithTag:40] setEnabled:YES];
    [[topBox viewWithTag:43] setEnabled:YES];
    
    [[topBox viewWithTag:50] setEnabled:YES];
    [[topBox viewWithTag:53] setEnabled:YES];
  }
  else
  {
    [actionButton setEnabled:NO];
    [delSchemaButton setEnabled:NO];
    [delSchemaButton setImage:[NSImage imageNamed:@"del_disabled.png"]];
    
    [[topBox viewWithTag:10] setEnabled:NO];
    
    [[topBox viewWithTag:30] setEnabled:NO];
    [[topBox viewWithTag:33] setEnabled:NO];
    
    [[topBox viewWithTag:40] setEnabled:NO];
    [[topBox viewWithTag:43] setEnabled:NO];
    
    [[topBox viewWithTag:50] setEnabled:NO];
    [[topBox viewWithTag:53] setEnabled:NO];      
  }
}


- (void)awakeFromNib
{
  [tableDetailsTab retain];
  
  [[addSchemaButton cell] setImageDimsWhenDisabled:NO];
  [addSchemaButton setImage:[NSImage imageNamed:@"add_normal.png"]];
  [addSchemaButton setAlternateImage:[NSImage imageNamed:@"add_pressed.png"]];
  
  [[delSchemaButton cell] setImageDimsWhenDisabled:NO];
  [delSchemaButton setImage:[NSImage imageNamed:@"del_disabled.png"]];
  [delSchemaButton setAlternateImage:[NSImage imageNamed:@"del_pressed.png"]];
  [delSchemaButton setEnabled:NO];
  
  [tablesTable setDoubleAction:@selector(editTable:)];
  [viewTable setDoubleAction:@selector(editView:)];
  [viewTable setTarget:self];
  [spTable setDoubleAction:@selector(editProcedure:)];
  [spTable setTarget:self];
  [funcTable setDoubleAction:@selector(editProcedure:)];
  [funcTable setTarget:self];
  
  [[topBox viewWithTag:200] setToolTip:@"Create a new schema (database)."];
  [[topBox viewWithTag:201] setToolTip:@"Drop the selected schema and all its tables and other assets."];
  
  [self updateButtonSensitivity];
  
  [self refreshContents:nil];
}


- (void)didShow
{
  if (![schemataOutline dataSource])
  {
    id ds= [_owner sharedSchemaDS];
    _schemaDS= [[MFilteredSchemaDataSource alloc] initWithDataSource:ds];
    int i, count= [_schemaDS outlineView:schemataOutline numberOfChildrenOfItem:nil];
    
    [schemataOutline setDataSource:_schemaDS];
    
    for (i= 0; i < count; i++)
      [schemataOutline expandItem:[_schemaDS outlineView:schemataOutline child:i ofItem:nil]
                   expandChildren:YES];
  }
}


- (IBAction)addSchema:(id)sender
{
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Create Schema"
                                                           labels:[NSArray arrayWithObject:@"New Schema Name:"]];
  NSArray *name;
	
  if ((name= [sheet runModal:[_owner window]]))
  {
    char *query;
    MYX_LIB_ERROR error;
    
    query= g_strdup_printf("create database `%s`", [[name objectAtIndex:0] UTF8String]);
    
    [[_owner dispatcher] performCallback: (void*(*)(MYSQL*,void*,void*,void*))myx_query_execute_direct
                                argument: query
                                argument: &error
                                argument: nil
                           waitForWindow: [_owner window]
                                 message: @"Creating schema..."];

    g_free(query);
    
    if (error!=MYX_NO_ERROR)
    {
      MXRunAlertPanelWithError(@"Error",@"Error creating schema.",error);
    }
    [self refreshSchemas:nil];
  }
}


- (void)dropSchemaAlertDidEnd:(NSAlert*)sheet
                   returnCode:(int)rc
                  contextInfo:(void*)context
{
  char *query;
  MYX_LIB_ERROR error;
  
  [[sheet window] orderOut:self];
  
  if (rc != NSAlertDefaultReturn)
    return;
  
  query= g_strdup_printf("drop database `%s`",
                         [(MSchemaItem*)context schema]->schema_name);
  
  [[_owner dispatcher] performCallback: (void*(*)(MYSQL*,void*,void*,void*))myx_query_execute_direct
                              argument: query
                              argument: &error
                              argument: nil
                         waitForWindow: [topBox window]
                               message: @"Dropping schema..."];
  
  g_free(query);

  if (error!=MYX_NO_ERROR)
  {
    MXRunAlertPanelWithError(@"Error",@"Error dropping schema.",error);
  }
  
  [self refreshSchemas:nil];
}


- (IBAction)delSchema:(id)sender
{
  MSchemaItem *item= [schemataOutline itemAtRow:[schemataOutline selectedRow]];
  
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop Selected Schema"
                                   defaultButton:@"Drop Schema" 
                                 alternateButton:@"Cancel" 
                                     otherButton:nil
                       informativeTextWithFormat:@"The schema '%@' will be dropped and all data contained in it will be permanently lost!",
    [item repr]];
  
  [alert beginSheetModalForWindow:[topBox window] modalDelegate:self 
                   didEndSelector:@selector(dropSchemaAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:item];
}


//============================= Delegate ================================

- (void)tabView:(NSTabView *)tabView didSelectTabViewItem:(NSTabViewItem *)tabViewItem
{
  [self updateButtonSensitivity];
}


- (BOOL)validateMenuItem:(id <NSMenuItem>)menuItem
{
  switch ([[menuItem menu] indexOfItem:menuItem])
  {
    case 1:
    case 2:
    case 3:
    case 6:
        return ([tablesTable numberOfSelectedRows] == 1) ? YES : NO;    
    case 8:
        return ([tablesTable selectedRow] >= 0) ? YES : NO;
    default:
      return YES;
  }
}


- (void)tableViewSelectionDidChange:(NSNotification *)aNotification
{
  NSTableView *table= [aNotification object];
  if (table == tablesTable)
  {
    int row= [tablesTable selectedRow];
    MYX_TABLE_STATUS *ts;
    MYX_TABLE_STATUS zero;
    
    if (row < 0)
    {
      memset(&zero, 0, sizeof(zero));
      ts= &zero;      
    }
    else
    {
      ts= [self objectOfType:MYX_ENTITY_TABLE atIndex:row];
    }
    
    [tableStatusText setStringValue:[NSString stringWithFormat:@"%s\n%s\n%s\n%s\n%s",
                                        (char*)ts->table_type?:"",
                                        (char*)ts->row_format?:"",
                                    (char*)ts->auto_increment?:"",
                                    (char*)ts->create_options?:"",
                                           (char*)ts->comment?:""]];
    
    [tableStatsDataText setStringValue:[NSString stringWithFormat:@"%s\n%s\n%s\n%s\n%s\n%s",
                                                 (char*)ts->rows?:"",
                                       (char*)ts->avg_row_length?:"",
                                          (char*)ts->data_length?:"",
                                      (char*)ts->max_data_length?:"",
                                         (char*)ts->index_length?:"",
                                            (char*)ts->data_free?:""]];
    
    [tableStatsTimeText setStringValue:[NSString stringWithFormat:@"%s\n%s\n%s",
                                          (char*)ts->create_time?:"",
                                          (char*)ts->update_time?:"",
                                           (char*)ts->check_time?:""]];
    
    
    NSMenu *menu= [[topBox viewWithTag:19] menu];
    
    if ([tablesTable selectedRow] < 0)
    {
      [[menu itemWithTag:1] setEnabled:NO];
      [[menu itemWithTag:2] setEnabled:NO];
      [[menu itemWithTag:3] setEnabled:NO];
      
      [[menu itemWithTag:5] setEnabled:NO];
      
      [[menu itemWithTag:6] setEnabled:NO];
    }
    else
    {
      [[menu itemWithTag:1] setEnabled:YES];
      [[menu itemWithTag:2] setEnabled:YES];
      [[menu itemWithTag:3] setEnabled:YES];
      
      [[menu itemWithTag:5] setEnabled:YES];
      
      [[menu itemWithTag:6] setEnabled:YES]; 
    }    
  }
  else if (table == viewTable)
  {
    if ([viewTable selectedRow] < 0)
    {
      [[topBox viewWithTag:31] setEnabled:NO];
      [[topBox viewWithTag:32] setEnabled:NO];
    }
    else
    {
      [[topBox viewWithTag:31] setEnabled:YES];
      [[topBox viewWithTag:32] setEnabled:YES];
    }
  }
  else if (table == spTable)
  {
    if ([spTable selectedRow] < 0)
    {
      [[topBox viewWithTag:41] setEnabled:NO];
      [[topBox viewWithTag:42] setEnabled:NO];
    }
    else
    {
      [[topBox viewWithTag:41] setEnabled:YES];
      [[topBox viewWithTag:42] setEnabled:YES];
    }    
  }
  else if (table == funcTable)
  {
    if ([funcTable selectedRow] < 0)
    {
      [[topBox viewWithTag:51] setEnabled:NO];
      [[topBox viewWithTag:52] setEnabled:NO];
    }
    else
    {
      [[topBox viewWithTag:51] setEnabled:YES];
      [[topBox viewWithTag:52] setEnabled:YES];
    }    
  }  
}


- (void)tableView:(NSTableView *)aTableView
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualToString:@"name"])
  {
    if (aTableView == tablesTable)
    {
      [aCell setImage:_tableIcon];
    }
    else if (aTableView == viewTable)
    {
      [aCell setImage:_viewIcon];
    }
    else if (aTableView == spTable)
    {
      [aCell setImage:_procIcon];
    }
    else if (aTableView == funcTable)
    {
      [aCell setImage:_funcIcon];
    }    
  }
}

- (void)outlineView:(NSOutlineView *)outlineView 
    willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if (outlineView == schemataOutline)
  {
    if ([item respondsToSelector:@selector(icon)])
      [cell setImage:[item icon]];
    else
      [cell setImage:nil];
  }
  else if (outlineView == indexOutline && [[tableColumn identifier] isEqualToString:@"column"])
  {
    if ([outlineView levelForItem:item]==0)
      [cell setImage:_indexIcon];
    else
      [cell setImage:_columnIcon];
  }
}

- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  NSOutlineView *oview= [notification object];
  
  if (oview == schemataOutline)
  {
    [self updateButtonSensitivity];
    
    [self refreshContents:nil];
  }
}


//============================= DataSource ================================

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  if (aTableView == tablesTable)
    return _tableCount;
  else if (aTableView == viewTable)
    return _viewCount;
  else if (aTableView == spTable)
    return _procCount;
  else if (aTableView == funcTable)
    return _funcCount;
  else
    return 0;
}

- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex
{
  NSString *ident= [aTableColumn identifier];
  if (aTableView == tablesTable)
  {
    MYX_TABLE_STATUS *table= [self objectOfType:MYX_ENTITY_TABLE atIndex:rowIndex];
    if ([ident isEqualToString:@"name"])
    {
      return NSStr(table->table_name);
    }
    else if ([ident isEqualToString:@"type"])
    {
      return NSStr((char*)table->table_type);
    }
    else if ([ident isEqualToString:@"format"])
    {
      return NSStr((char*)table->row_format);
    }
    else if ([ident isEqualToString:@"rows"])
    {
      return NSStr((char*)table->rows);
    }
    else if ([ident isEqualToString:@"data"])
    {
      return [NSString stringWithMultNumber:strtoll((char*)table->data_length?:"0",NULL,0)];
    }
    else if ([ident isEqualToString:@"index"])
    {
      return [NSString stringWithMultNumber:strtoll((char*)table->index_length?:"0",NULL,0)];
    }
    else if ([ident isEqualToString:@"update"])
    {
      return NSStr((char*)table->update_time);
    }
  }
  else if (aTableView == viewTable)
  {
    MYX_VIEW_STATUS *view= [self objectOfType:MYX_ENTITY_VIEW atIndex:rowIndex];
    if ([ident isEqualToString:@"name"])
    {
      return NSStr(view->view_name);
    }
    else if ([ident isEqualToString:@"comment"])
    {
      return NSStr(view->comment);
    }
    else if ([ident isEqualToString:@"updatable"])
    {
      return @"-";
    }
    return nil;
  }
  else if (aTableView == spTable || aTableView == funcTable)
  {
    MYX_SCHEMA_STORED_PROCEDURE *proc;
    if (aTableView == spTable)
      proc= [self objectOfType:MYX_ENTITY_PROC atIndex:rowIndex];
    else
      proc= [self objectOfType:MYX_ENTITY_FUNC atIndex:rowIndex];

    if ([ident isEqualToString:@"name"])
    {
      return NSStr(proc->name);
    }
    else if ([ident isEqualToString:@"definer"])
    {
      return NSStr(proc->definer);
    }
    else if ([ident isEqualToString:@"ctime"])
    {
      return NSStr(proc->created);
    }
    else if ([ident isEqualToString:@"utime"])
    {
      return NSStr(proc->modified);
    }
    else if ([ident isEqualToString:@"type"])
    {
      return NSStr(proc->return_datatype);
    }
    else if ([ident isEqualToString:@"comment"])
    {
      return NSStr(proc->comment);
    }
    return nil;
  }
  return nil;
}

@end
