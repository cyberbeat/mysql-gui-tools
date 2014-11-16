//
//  MSchemaEditHelper.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on 3/23/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MSchemaEditHelper.h"
#import "MMySQLDispatcher.h"
#import "MSchemaDataSource.h"
#import "MDialogs.h"

#import "MTableEditor.h"
#include "myxutil.h"
#import "mxUtils.h"

@implementation MSchemaEditHelper


- (id)init
{
  self= [super init];
  if (self)
  {
    _editors= [[NSMutableArray alloc] init];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(tableEditorChange:)
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
  
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  [super dealloc];
}

- (void)tableEditorChange:(NSNotification*)notif
{
  [delegate performSelector:@selector(schemaHelperChangedSchemata:) withObject:self];
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


- (void)setConnection:(MYSQL*)mysql
{
  _mysql= mysql;
}

- (void)setDelegate:(id)deleg
{
  delegate= deleg;
}

- (IBAction)createSchema:(id)sender
{
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Create Schema"
														   labels:[NSArray arrayWithObject:@"New Schema Name:"]];
  NSArray *name;
  
  if ((name= [sheet runModal:parentWindow]))
  {
    char *query;
    MYX_LIB_ERROR error;
    long long int affected_rows;
    
    query= g_strdup_printf("create database `%s`", [[name objectAtIndex:0] UTF8String]);
    myx_query_execute_direct(_mysql,query,&error, &affected_rows);
    g_free(query);
      
    [delegate performSelector:@selector(schemaHelperChangedSchemata:) withObject:self];
  }
}

- (void)editSchema:(MSchemaItem*)item
{
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Edit Schema"
														   labels:[NSArray arrayWithObject:@"Schema Name:"]];
  NSArray *name;

  if ((name= [sheet runModal:parentWindow]))
  {
    char *query;
    MYX_LIB_ERROR error;
    long long int affected_rows;

    query= g_strdup_printf("create database `%s`", [[name objectAtIndex:0] UTF8String]);
    myx_query_execute_direct(_mysql, query, &error, &affected_rows);
    g_free(query);

    [delegate performSelector:@selector(schemaHelperChangedSchemata:) withObject:self];
  }  
}

- (IBAction)createTable:(id)sender
{
  MSchemaItem *item= [outline itemAtRow:[outline selectedRow]];
  MYX_SCHEMA *schema= [item schema];
  MTableEditor *editor;
  
  if (schema)
  {
    editor= [[MTableEditor alloc] init];
    
    [editor setConnection:_mysql];
    [editor setCatalogs:(MYX_CATALOGS*)[[outline dataSource] catalogs]];
    [editor showNewTableForCatalog:NSStr(schema->catalog_name)
                            schema:NSStr(schema->schema_name)];
        
    [_editors addObject:editor];
    [editor setReleaseOnClose:YES];
  }
}

- (void)editTable:(MSchemaItem*)item
{
  MYX_SCHEMA_TABLE *table= [item table];
  MYX_SCHEMA *schema= [item schema];
  unsigned int i;
  BOOL found= NO;
  
  for (i=0; i < [_editors count]; i++)
  {
    MTableEditor *e= [_editors objectAtIndex:i];
    
    if (strcmp([[e catalog] UTF8String], schema->catalog_name)==0
        && strcmp([[e schema] UTF8String], schema->schema_name)==0
        && strcmp([[e table] UTF8String], table->table_name)==0)
    {
      [[e window] makeKeyAndOrderFront:nil];
      found= YES;
      break;
    }
  }

  if (!found)
  {
    MTableEditor *editor= [[MTableEditor alloc] init];
    
    [editor setConnection:_mysql];
    [editor setCatalogs:(MYX_CATALOGS*)[[outline dataSource] catalogs]];
    [editor showTable:NSStr((char*)table->table_name) catalog:NSStr(schema->catalog_name)
               schema:NSStr(schema->schema_name)];
    
    [_editors addObject:editor];
    [editor setReleaseOnClose:YES];
    
    table= [item table];
  }
}


- (void)editView:(MSchemaItem*)item
{
  MYX_SCHEMA_TABLE *view= [item table];
  MYX_SCHEMA *schema= [item schema];

  MYX_DBM_VIEW_DATA *view_data= myx_dbm_get_view_data(_mysql, schema->catalog_name,
                                                      schema->schema_name,
                                                      view->table_name,
                                                      MYX_DEFAULT_QUOTE_CHAR);

  if (view_data)
    [delegate schemaHelper:self editScript:
      [NSString stringWithFormat:@"DROP VIEW IF EXISTS `%s`;\n%@;\n",
        view->table_name,
        [NSString stringWithUTF8String:view_data->definition]]];
  else
    MXRunAlertPanelWithMySQLError(@"Error", @"Could not retrieve View definition.",
                                  _mysql);
}


- (void)editSP:(MSchemaItem*)item
{
  MYX_SCHEMA_STORED_PROCEDURE *sp= [item sp];
  MYX_SCHEMA *schema= [item schema];

  MYX_DBM_STORED_PROCEDURE_DATA *sp_data= myx_dbm_get_sp_data(_mysql,schema->catalog_name,
                                                              schema->schema_name,
                                                              sp->name, sp->sp_type,
                                                              MYX_DEFAULT_QUOTE_CHAR, 0);
  if (sp_data)
    [delegate schemaHelper:self editScript:
      [NSString stringWithFormat:@"DROP PROCEDURE IF EXISTS `%s`;\nDELIMITER $$\n%@\n$$",
        sp->name,
        [NSString stringWithUTF8String:sp_data->definition]]];
  else
  {
    MXRunAlertPanelWithMySQLError(@"Error", @"Could not retrieve Stored Procedure definition.",
                                  _mysql);
  }
}


- (void)dropObjectAlertDidEnd:(NSAlert *)sheet 
                   returnCode:(int)returnCode
                  contextInfo:(void *)contextInfo
{
  MSchemaItem *item= (MSchemaItem*)contextInfo;
  char *query= NULL;
  MYX_LIB_ERROR error;
  long long int affected_rows;
  
  [[sheet window] orderOut:self];
  
  if (returnCode != NSAlertAlternateReturn)
    return;

  switch ([item type])
  {
    case MTableItemType:
      query= g_strdup_printf("drop table `%s`.`%s`", [item schema]->schema_name, [item table]->table_name);
      break;
    case MViewItemType:
      query= g_strdup_printf("drop view `%s`.`%s`", [item schema]->schema_name, [item table]->table_name);
      break;
    case MSPItemType:
      query= g_strdup_printf("drop procedure `%s`.`%s`", [item schema]->schema_name, [item sp]->name);
      break;
    default:
      break;
  }
  myx_query_execute_direct(_mysql,query,&error, &affected_rows);
  g_free(query);

  [delegate performSelector:@selector(schemaHelperChangedSchemata:) withObject:self];
}



- (void)dropTable:(MSchemaItem*)item
{
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop Table?"
                                   defaultButton:@"Cancel" 
                                 alternateButton:@"Drop Table"
                                     otherButton:nil
                       informativeTextWithFormat:@"The table '%s' will be dropped and all data contained in it will be permanently erased.",
    [item table]->table_name];

  [alert beginSheetModalForWindow:parentWindow modalDelegate:self 
                   didEndSelector:@selector(dropObjectAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:item];
}


- (void)dropSP:(MSchemaItem*)item
{
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop Stored Procedure?"
                                   defaultButton:@"Cancel" 
                                 alternateButton:@"Drop SP"
                                     otherButton:nil
                       informativeTextWithFormat:@"The Stored Procedure '%s' will be dropped.",
    [item sp]->name];
  
  [alert beginSheetModalForWindow:parentWindow modalDelegate:self 
                   didEndSelector:@selector(dropObjectAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:item];
}


- (void)dropView:(MSchemaItem*)item
{
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop View?"
                                   defaultButton:@"Cancel" 
                                 alternateButton:@"Drop View"
                                     otherButton:nil
                       informativeTextWithFormat:@"The view '%s' will be dropped.",
    [item table]->table_name];
  
  [alert beginSheetModalForWindow:parentWindow modalDelegate:self 
                   didEndSelector:@selector(dropObjectAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:item];
}


- (void)dropSchemaAlertDidEnd:(NSAlert *)sheet 
                   returnCode:(int)returnCode
                  contextInfo:(void *)contextInfo
{
  MSchemaItem *item= (MSchemaItem*)contextInfo;
  char *query;
  MYX_LIB_ERROR error;
  long long int affected_rows;
  
  [[sheet window] orderOut:self];
  
  if (returnCode != NSAlertAlternateReturn)
    return;
  
  if (strcmp([item schema]->schema_name, "mysql")==0)
  {
    NSRunAlertPanel(@"Error",@"MySQL schema cannot be dropped.",
                    @"OK",nil,nil);
    return;
  }

  myx_query_execute_direct(_mysql,
    "/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;",
    &error, &affected_rows);  
  query= g_strdup_printf("drop database `%s`", [item schema]->schema_name);
  myx_query_execute_direct(_mysql,query,&error,&affected_rows);
  g_free(query);
  myx_query_execute_direct(_mysql,"/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;",&error,&affected_rows);
  
  [delegate performSelector:@selector(schemaHelperChangedSchemata:) withObject:self];
}


- (void)dropSchema:(MSchemaItem*)item
{
  NSAlert *alert = [NSAlert alertWithMessageText:@"Drop Schema?"
                                   defaultButton:@"Cancel" 
                                 alternateButton:@"Drop Schema"
                                     otherButton:nil
                       informativeTextWithFormat:@"The schema (database) '%s' will be dropped and all its tables will be permanently erased.",
    [item schema]->schema_name];

  [alert beginSheetModalForWindow:parentWindow modalDelegate:self 
                   didEndSelector:@selector(dropSchemaAlertDidEnd:returnCode:contextInfo:) 
                      contextInfo:item];
}


- (IBAction)createSP:(id)sender
{
  int row= [outline selectedRow];
  MSchemaItem *item= [outline itemAtRow:row];
  MYX_SCHEMA *schema= [item schema];
  MStringRequestSheet *sheet;
  NSArray *name;
  NSString *template;
  
  if (!schema)
    return;
  
  sheet= [[MStringRequestSheet alloc] initWithTitle:@"Give a name for the new Stored Procedure or Function"
                                             labels:[NSArray arrayWithObject:@"Routine Name:"]
                                            buttons:[NSArray arrayWithObjects:@"Create Procedure",@"Create Function",@"Cancel",nil]];
  
  name= [sheet runModal:parentWindow];
  if (name && [sheet clickedButton]!=2)
  {
    const char *type;
    const char *rtype;
    
    if ([sheet clickedButton]==0)
    {
      type= "PROCEDURE";
      rtype= "";
    }
    else
    {
      type= "FUNCTION";
      rtype= " RETURNS INT";
    }
    
    template= [NSString stringWithFormat:
      @"DELIMITER $$\n\n"
      "DROP %s IF EXISTS `%s`.`%@`$$\n"
      "CREATE %s `%s`.`%@` ()%s\n"
      "BEGIN\n\n"
      "END$$\n\n"
      "DELIMITER ;\n",
      type, schema->schema_name, [name objectAtIndex:0],
      type, schema->schema_name, [name objectAtIndex:0], rtype];
    
    [delegate schemaHelper:self editScript:template];    
  }
  [sheet release];
}

- (IBAction)createView:(id)sender
{
  NSString *template;
  int row= [outline selectedRow];
  MSchemaItem *item= [outline itemAtRow:row];
  MYX_SCHEMA *schema= [item schema];
  const char *table_name= "";
  NSArray *name;
  MStringRequestSheet *sheet;
  
  sheet= [[MStringRequestSheet alloc] initWithTitle:@"Give a name for the new View"
                                             labels:[NSArray arrayWithObject:@"View Name:"]
                                            buttons:[NSArray arrayWithObjects:@"Create View",@"Cancel",nil]];
  
  name= [sheet runModal:parentWindow];
  if ([sheet clickedButton]==0)
  {
    if ([item type] == MTableItemType)
      table_name= [item table]->table_name;
  
    template= [NSString stringWithFormat:
      @"CREATE VIEW `%s`.`%@` AS\n    SELECT * FROM `%s`",
      schema->schema_name, [name objectAtIndex:0],
      table_name];
    
    [delegate schemaHelper:self editScript:template];
  }
  [sheet release];
}


- (IBAction)editSelection:(id)sender
{
  int row= [outline selectedRow];
  
  if (row >= 0)
  {
    MSchemaItem *item= [outline itemAtRow:row];
    
    switch (item->type)
    {
      case MSchemaItemType:
        break;
      case MTableItemType:
        [self editTable:item];
        break;
      case MSPItemType:
        [self editSP:item];
        break;
      case MViewItemType:
        [self editView:item];
        break;
      default:
        break;
    }
  }
}

- (IBAction)dropSelection:(id)sender
{
  int row= [outline selectedRow];
  
  if (row >= 0)
  {
    MSchemaItem *item= [outline itemAtRow:row];
    
    switch (item->type)
    {
      case MSchemaItemType:
        [self dropSchema:item];
        break;
      case MTableItemType:
        [self dropTable:item];
        break;
      case MSPItemType:
        [self dropSP:item];
        break;
      case MViewItemType:
        [self dropView:item];
        break;        
      default:
        break;
    }
  }    
}


- (NSString*)getCreateCommandForItem:(MSchemaItem*)item
{
  NSString *s;
  char *sql;
  
  switch (item->type)
  {
    case MSchemaItemType:
      sql= myx_dbm_get_create_sql(_mysql,
                                  [item schema]->catalog_name,
                                  [item schema]->schema_name,
                                  [item schema]->schema_name,
                                  MYX_DBM_OT_SCHEMA,
                                  1, MYX_DEFAULT_QUOTE_CHAR, 0);
      break;
    case MTableItemType:
      sql= myx_dbm_get_create_sql(_mysql,
                                  [item schema]->catalog_name,
                                  [item schema]->schema_name,
                                  [item table]->table_name,
                                  MYX_DBM_OT_TABLE,
                                  1, MYX_DEFAULT_QUOTE_CHAR, 0);
      break;
    case MSPItemType:
      sql= myx_dbm_get_create_sql(_mysql,
                                  [item schema]->catalog_name,
                                  [item schema]->schema_name,
                                  [item sp]->name,
                                  [item sp]->sp_type==MSPT_PROCEDURE?MYX_DBM_OT_PROCEDURE:MYX_DBM_OT_FUNCTION,
                                  1, MYX_DEFAULT_QUOTE_CHAR, 0);
      break;
    case MViewItemType:
      sql= myx_dbm_get_create_sql(_mysql,
                                  [item schema]->catalog_name,
                                  [item schema]->schema_name,
                                  [item table]->table_name,
                                  MYX_DBM_OT_VIEW,
                                  1, MYX_DEFAULT_QUOTE_CHAR, 0);
      break;        
    default:
      sql= NULL;
      break;
  }  
  if (sql)
  {
    s= [NSString stringWithUTF8String:sql];
    g_free(sql);
  }
  else
    s= nil;
  return s;
}


- (IBAction)copySelectionSQL:(id)sender
{
  int row= [outline selectedRow];
  
  if (row >= 0)
  {
    MSchemaItem *item= [outline itemAtRow:row];
    NSString *s= [self getCreateCommandForItem:item];
    NSPasteboard *pasteboard= [NSPasteboard generalPasteboard];
    [pasteboard declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                       owner:nil];
    [pasteboard setString:s forType:NSStringPboardType];
  }  
}


- (BOOL)editStoredProceduresInCatalog:(NSString*)catalog schema:(NSString*)schema 
{
  MYX_SCHEMA_STORED_PROCEDURES *sps;
  BOOL ok= NO;
  
  sps= myx_get_schema_sps(_mysql, [catalog UTF8String], [schema UTF8String]);
  if (sps)
  {
    char *script;
    
    script= myx_dbm_make_script_from_sps(_mysql, 
                                         [catalog UTF8String], [schema UTF8String],
                                         sps, MYX_DEFAULT_QUOTE_CHAR);
    if (script)
    {
      [delegate schemaHelper:self editScript:[NSString stringWithUTF8String:script]];
      g_free(script);
      ok= YES;
    }
    myx_free_schema_sps(sps);
  }
  return ok;
}

@end
