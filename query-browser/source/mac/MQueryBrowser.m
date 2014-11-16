//
//  MQueryBrowser.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on Sat Mar 5 2005.
//  Copyright (c) 2005 MySQL AB. All rights reserved.
//

#import "MQueryBrowserController.h"
#import "MQueryBrowserDocument.h"

#import "MQueryBrowser.h"
#import "MQHistory.h"
#import "MQAddBookmarkPanel.h"
#import "MQBookmark.h"
#import "MQParameters.h"
#import "MQSyntaxHelp.h"

#import <MySQLToolsCommon/MQResultSetView.h>
#import <MySQLToolsCommon/MAccessoryScrollView.h>
#import <MySQLToolsCommon/MDialogs.h>
#import "MQScriptController.h"
#import "MQQueryController.h"
#import "MQResultSetTab.h"
#import "MQScriptTab.h"
#import "MQHelpTab.h"

#import <MySQLToolsCommon/MSchemaEditHelper.h>
#include <MySQLToolsCommon/mxUtils.h>

#include "myx_public_interface.h"

//#include "myx_qb_public_interface.h"

@interface MQueryBrowser(Private)
- (void)fetchSchemasFor:(MSchemaDataSource*)ds;
- (void)tablesArrived:(NSArray*)args result:(MYX_SCHEMA_TABLES*)tables;
- (void)requestTablesForSchema:(MSchemaDataSource*)sender item:(MSchemaItem*)item;
- (void)selectSchema:(NSString*)schema;
- (void)updateHistory:(NSNotification*)notif;
@end

@implementation MQueryBrowser(Private)

- (void)fetchSchemasFor:(MSchemaDataSource*)ds
{
  MYX_CATALOGS *cats;
  MMySQLDispatcher *dispatcher= [[self document] dispatcher];
  
retry:
  cats= [dispatcher performCallback:(void*(*)(MYSQL*))myx_get_catalogs
                      waitForWindow:[self window]
                            message:@"Retrieving catalog list..."];
  if (!cats && ![dispatcher checkConnection] && [dispatcher reconnect])
    goto retry;

  if (!cats)
  {
    NSLog(@"ERROR: Could not retrieve catalog list");
    return;
  }

  [ds updateWithCatalogs:cats];
  [catalogList reloadData];
  
  if (_catalogs)
    myx_free_catalogs(_catalogs);
  _catalogs= cats;
}


- (void)requestTablesForSchema:(MSchemaDataSource*)sender item:(MSchemaItem*)item
{
  MYX_SCHEMA *schema= [item schema];
  MMySQLDispatcher *dispatcher= [[self document] dispatcher];
 
  [_columnDS resetSchemaChildren:item];
  
  [dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_get_schema_tables
                     argument:schema->catalog_name
                     argument:schema->schema_name
             finishedSelector:@selector(tablesArrived:result:)
                     argument:[[NSArray arrayWithObjects:sender,item,nil] retain]
                       target:self];
  
  if ([[self document] connectedMySQLIsAtLeastMajor:5 minor:0])
  {
    [dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_get_schema_sps
                       argument:schema->catalog_name
                       argument:schema->schema_name
               finishedSelector:@selector(spsArrived:result:)
                       argument:[[NSArray arrayWithObjects:sender,item,nil] retain]
                         target:self];
  }
}


- (void)tablesArrived:(NSArray*)args result:(MYX_SCHEMA_TABLES*)tables
{
  MMySQLDispatcher *dispatcher= [[self document] dispatcher];
  MSchemaDataSource *ds= [args objectAtIndex:0];
  MSchemaItem *sitem= [args objectAtIndex:1];
  MYX_SCHEMA *schema= [sitem schema];
  [args autorelease];
  
  if (!tables && ![dispatcher checkConnection] && [dispatcher reconnect])
  {
    [self requestTablesForSchema:ds item:sitem];
    return;
  }

  if (tables)
  {
    if (schema->schema_tables)
      myx_free_schema_tables(schema->schema_tables);
    schema->schema_tables= tables;
  }
  if (!tables)
  {
    NSLog(@"could not retrieve tables list");
    return;
  }
  
  [ds updateSchema:sitem 
		withTables:tables];
  
  [catalogList reloadData];
}

- (void)spsArrived:(NSArray*)args result:(MYX_SCHEMA_STORED_PROCEDURES*)sps
{
  MSchemaDataSource *ds= [args objectAtIndex:0];
  MSchemaItem *sitem= [args objectAtIndex:1];
  MYX_SCHEMA *schema= [sitem schema];
  
  [args autorelease];
  
  if (sps)
  {
    if (schema->schema_sps)
      myx_free_schema_sps(schema->schema_sps);
    schema->schema_sps= sps;
  }
  if (!sps)
  {
    NSLog(@"could not retrieve sp list");
    return;
  }
  
  [ds updateSchema:sitem 
           withSPs:sps];
  
  [catalogList reloadData];
}


- (void)selectSchema:(NSString*)schema
{
  unsigned int i;
  
  if (![[[self document] defaultSchema] isEqualToString:schema])
    [[self document] setDefaultSchema:schema];
  
  for (i= 0; i < [tabView numberOfPages]; i++)
  {
    if ([[tabView pageAtIndex:i] respondsToSelector:@selector(setDefaultSchema:)])
      [[tabView pageAtIndex:i] performSelector:@selector(setDefaultSchema:)
                                    withObject:schema];
  }
  [queryCtl setDefaultSchema:schema];
  
  [catalogList reloadData];
  
  if (schema)
    MXExpandOutlineItems(catalogList,
                         [NSDictionary dictionaryWithObject:[NSDictionary dictionaryWithObject:[NSNull null]
                                                                                        forKey:schema]
                                                     forKey:@"def"],
                         nil);
}

- (void)updateHistory:(NSNotification*)notif
{
  [historyList reloadData];
}  
@end

//------------------------------------------------------------------------------

@implementation MQueryBrowser


- (id)init
{ 
  self = [super initWithWindowNibName:@"QueryBrowser" owner:self];
  if (!self)
    return nil;
  return self;
}


- (void)awakeFromNib
{  
  [self setShouldCascadeWindows:NO];
  
  [tabView setMaxTabSize:100];
  
  _historyGroupIcon= [[NSImage imageNamed:@"history_interval_14x14.png"] retain];
  _historyItemIcon= [[NSImage imageNamed:@"query_14x14.png"] retain];

  _bookmarkGroupIcon= [[NSImage imageNamed:@"folder_closed_14x14.png"] retain];
  _bookmarkItemIcon= [[NSImage imageNamed:@"bookmark_14x14.png"] retain];

  _boldFont= [[NSFont boldSystemFontOfSize:[NSFont smallSystemFontSize]] retain];
  _normalFont= [[NSFont systemFontOfSize:[NSFont smallSystemFontSize]] retain];

  [bookmarkList registerForDraggedTypes:[NSArray arrayWithObjects:MQBookmarkPboardType, NSStringPboardType, nil]];

  [[NSNotificationCenter defaultCenter] addObserver:self
										   selector:@selector(updateHistory:)
											   name:MQHistoryDidChangeNotification
											 object:nil];
  
  _parameters= [[MQParameters alloc] init];
  [parameterList setDataSource:_parameters];
  [parameterList reloadData];
  MXExpandOutline(parameterList, YES);
  {
    MAccessoryScrollView *sv= (MAccessoryScrollView *)[parameterList enclosingScrollView];
    NSButton *button;
    button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,12,12)] autorelease];
    [button setButtonType: NSMomentaryChangeButton];
    [button setBordered:NO];
    [button setEnabled:NO];
    [button setTag:1];
    [[button cell] setBezelStyle:NSShadowlessSquareBezelStyle];
    [[button cell] setImagePosition:NSImageOnly];
    [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[MAccessoryScrollView class]], @"mini_add_12.png")];
    [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[MAccessoryScrollView class]], @"mini_add_pressed_12.png")];    
    [button setTarget:self];
    [button setAction:@selector(addParameter:)];
    [sv addSubview:button];
    [sv addHorizontalAccessoryView:button];
    
    button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,12,12)] autorelease];
    [button setButtonType: NSMomentaryChangeButton];
    [button setBordered:NO];
    [button setEnabled:NO];
    [button setTag:2];
    [[button cell] setBezelStyle:NSShadowlessSquareBezelStyle];
    [[button cell] setImagePosition:NSImageOnly];
    [button setImage: MXGetImageFromBundle([NSBundle bundleForClass:[MAccessoryScrollView class]], @"mini_del_12.png")];
    [button setAlternateImage: MXGetImageFromBundle([NSBundle bundleForClass:[MAccessoryScrollView class]], @"mini_del_pressed_12.png")];
    [button setTarget:self];
    [button setAction:@selector(deleteParameter:)];
    [sv addSubview:button];
    [sv addHorizontalAccessoryView:button];
  }
  [tabView setDelegate:self];
  [tabView setHasBorder:YES];

  NSImage *folderIcon= [NSImage imageNamed:@"folder_closed_14x14.png"];
  [[syntaxList dataSource] loadFile:@"mysqlqb_statements"];
  [[syntaxList dataSource] setGroupIcon:folderIcon itemIcon:[NSImage imageNamed:@"syntax_14x14.png"]];
  [syntaxList setTarget:self];
  [syntaxList setDoubleAction:@selector(showKeywordHelp:)];
  [syntaxList reloadData];
  [[functionList dataSource] loadFile:@"mysqlqb_functions"];
  [[functionList dataSource] setGroupIcon:folderIcon itemIcon:[NSImage imageNamed:@"function_14x14.png"]];
  [functionList setDoubleAction:@selector(showKeywordHelp:)];
  [functionList reloadData];
  
  _groupIcon= [folderIcon retain];
  _paramIcon= [[NSImage imageNamed:@"param_14x14.png"] retain];
  
  _toolbarHeight= 58;
  
  _columnDS= [[MSchemaDataSource alloc] initWithRoot:MSchemaItemType 
                                                leaf:MColumnItemType
                                               icons:[NSDictionary dictionaryWithObjectsAndKeys:
                                                 [NSImage imageNamed:@"16x16_Catalog.png"], @"catalog",
                                                 [NSImage imageNamed:@"myx_schema_16x16.png"], @"schema",
                                                 [NSImage imageNamed:@"myx_schema_table_16x16.png"], @"table",
                                                 [NSImage imageNamed:@"myx_schema_view_16x16.png"], @"view",
                                                 [NSImage imageNamed:@"myx_schema_sp_16x16.png"], @"sp",
                                                 [NSImage imageNamed:@"16x16_Field.png"], @"column",
                                                 [NSImage imageNamed:@"16x16_KeyColumn.png"], @"key",
                                                 nil]];
  [_columnDS setTableFetcher:self selector:@selector(requestTablesForSchema:item:)];
  [self refreshSchemata:nil];

  [schemaHelper setConnection:[[self document] mysql]];

  [catalogList setDataSource:_columnDS];
  [catalogList reloadData];
  MXExpandOutline(catalogList, NO);
  [catalogList setDoubleAction:@selector(schemataDoubleClicked:)];
  
  [historyList setDataSource:[[MQueryBrowserController sharedDocumentController] history]];
  MXExpandOutline(historyList, NO);
  [historyList reloadData];
  [historyList setDoubleAction:@selector(executeHistory:)];
  
  [bookmarkList setDataSource:[[MQueryBrowserController sharedDocumentController] bookmarks]];
  MXExpandOutline(bookmarkList, NO);
  [bookmarkList reloadData];
  
  
  
  [self selectSchema: [[self document] defaultSchema]];
  [self addQueryTab: self];
  
  [drawer open];
  
  [[self window] setFrameAutosaveName:@"mainWindow"];
}


- (void)dealloc
{
  [_columnDS release];
  [_parameters release];
  [_groupIcon release];
  [_paramIcon release];

  if (_catalogs)
    myx_free_catalogs(_catalogs);
  
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  
  [super dealloc];
}


- (void)selectQueryArea:(id)sender
{
  if ([[self currentTab] isKindOfClass:[MQResultSetTab class]])
    [queryCtl selectQueryArea:sender];
}


- (BOOL)setupQueryWithSchemaItem:(MSchemaItem*)item
{
  switch (item->type)
  {
    case MSchemaItemType:
      [self setDefaultSchema:nil];
      break;
    case MViewItemType:
    case MTableItemType:
      if ([[tabView selectedPage] isKindOfClass:[MQResultSetTab class]])
        return [queryCtl setupQueryWithItem:item];
      break;
    case MSPItemType:
      if ([[tabView selectedPage] isKindOfClass:[MQResultSetTab class]])
        return [queryCtl setupQueryWithItem:item];
      break;
    case MCatalogItemType:
    case MColumnItemType:
      break;
  }
  return NO;
}


- (IBAction)showHelp:(id)sender
{
  unsigned int i, c= [tabView numberOfPages];
  MQHelpTab *page= nil;
  
  for (i= 0; i < c; i++)
  {
    if ([[tabView pageAtIndex:i] isKindOfClass:[MQHelpTab class]])
    {
      page= (MQHelpTab*)[tabView pageAtIndex:i];
      [tabView selectPage:i];
      break;
    }
  }
  if (!page)
  {
    page= [[[MQHelpTab alloc] initWithIdentifier:@"Help"] autorelease];
    [tabView addTabViewItem:page];
    [tabView selectPage:[tabView numberOfPages]-1];
  }

  switch ([sender tag])
  {
    case 2: // QuickStart help
      [page showQuickStartHelp];
      break;
  }
}


- (IBAction)showKeywordHelp:(id)sender
{
  unsigned int i, c= [tabView numberOfPages];
  MQHelpTab *page= nil;
  const char *topicId;

  topicId= [[sender dataSource] topicIdForItem:[sender itemAtRow:[sender selectedRow]]];
  if (!topicId)
    return;
  
  for (i= 0; i < c; i++)
  {
    if ([[tabView pageAtIndex:i] isKindOfClass:[MQHelpTab class]])
    {
      page= (MQHelpTab*)[tabView pageAtIndex:i];
      [tabView selectPage:i];
      break;
    }
  }
  if (!page)
  {
    page= [[[MQHelpTab alloc] initWithIdentifier:@"Help"] autorelease];
    [tabView addTabViewItem:page];
    [tabView selectPage:[tabView numberOfPages]-1];
  }
  

  if (sender == syntaxList)
  {
    [page loadFile:@"mysqlqb_statements" topic:[NSString stringWithUTF8String:topicId]];
  }
  else
  {
    [page loadFile:@"mysqlqb_functions" topic:[NSString stringWithUTF8String:topicId]];
  }
}


- (IBAction)addQueryTab:(id)sender
{
  MQResultSetTab *page= [queryCtl createPage];
  if (page)
  {
    [page setDefaultSchema: [[self document] defaultSchema]];
    [tabView addTabViewItem:page];
    [tabView selectPage:[tabView numberOfPages]-1];
    
    [queryCtl addNewResultSet:NO toPage:page];
  }
}


- (IBAction)addScriptTab:(id)sender
{
  MQScriptTab *page= [scriptCtl createPage];
  if (page)
  {
    [page setDefaultSchema: [[self document] defaultSchema]];
    [tabView addTabViewItem:page];
    [tabView selectPage:[tabView numberOfPages]-1];
  }
}


- (void)exportResultset:(id)sender
{
  [queryCtl exportResultSet:sender];
}


- (void)openDocument:(id)sender
{
  [scriptCtl openScript:sender];
}

- (NSString *) queryString
{
  return [queryCtl currentQuery];
}

- (void)executeQuery:(id)sender
{
  [queryCtl executeQuery:sender];
}


- (void)stopQuery:(id)sender
{
  [queryCtl stopQuery:sender];
}


- (void)explainQuery:(id)sender
{
  [queryCtl explainQuery:sender];
}


- (void)compareResultsets:(id)sender
{
  [queryCtl compareQuery:sender];
}



- (void)showTableRows:(id)sender
{
  int row= [catalogList selectedRow];
  
  if (row < 0) return;
  
  if ([sender tag] == 10) 
  {
    [queryCtl setupSelectQueryWithItem:[catalogList itemAtRow:row] 
                              rowLimit:[[NSUserDefaults standardUserDefaults] integerForKey:@"PartialTableListLimit"]];
  }
  else
  {
    [queryCtl setupSelectQueryWithItem:[catalogList itemAtRow:row] 
                              rowLimit:0];
  }
  [self executeQuery:nil];
}


- (void)doSchemaAction:(id)sender
{
  switch ([sender tag])
  {
    case 20: // create view with query
      if ([[self currentTab] isKindOfClass:[MQResultSetTab class]])
      {
        MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Create view from the current query."
                                                                 labels:[NSArray arrayWithObject:@"View Name:"]];
        NSArray *results= [sheet runModal:[self window]];
        
        if (results)
        {
          [scriptCtl setupEditorWithScript:[NSString stringWithFormat:@"CREATE VIEW `%@` AS\n    %@;\n", [results objectAtIndex:0], [queryCtl currentQuery]]];
        }
      }
      break;
    case 21: // edit all SPs
      [schemaHelper editStoredProceduresInCatalog:nil schema:[[self document] defaultSchema]];
      break;
  }
}


- (void)closeTab:(MTabViewItem*)tabItem
{
  // remove tabsheet
  [tabView removeTabViewItem:tabItem];
}


- (IBAction)toggleDrawer:(id)sender
{
  if ([drawer state] == NSDrawerOpenState)
    [sender setTitle:@"Show Side Drawer"];
  else
    [sender setTitle:@"Hide Side Drawer"];
  [drawer toggle:sender];
}

- (IBAction)schemataDoubleClicked:(id)sender
{
  int row= [catalogList selectedRow];
  MSchemaItem *item= [catalogList itemAtRow:row];
  
  if (item->type == MSchemaItemType)
  {
    [self setDefaultSchema:nil];
  }
  else if ([self setupQueryWithSchemaItem:item])
  {
    [queryCtl selectQueryArea:nil];
    
    if ([[tabView selectedPage] isKindOfClass:[MQResultSetTab class]])
      [queryCtl executeQuery:nil];
  }
}


- (IBAction)refreshSchemata:(id)sender
{
  NSDictionary *expandedItems= MXGetExpandedOutlineItems(catalogList, 2, nil);
  [_columnDS resetTree];
  [catalogList reloadData];
  [self fetchSchemasFor:_columnDS];
  MXExpandOutlineItems(catalogList, expandedItems, nil);
}

- (IBAction)setDefaultSchema:(id)sender
{  
  NSString *schema;
  
  schema= [NSString stringWithUTF8String:[[catalogList itemAtRow:[catalogList selectedRow]] schema]->schema_name];
  [self selectSchema:schema];
}

- (IBAction)copyHistory:(id)sender
{
  int row= [historyList selectedRow];
  id item= [historyList itemAtRow:row];
  
  if ([item isKindOfClass:[MQHistoryItem class]])
  {
    NSPasteboard *pasteboard= [NSPasteboard generalPasteboard];
    [pasteboard declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                       owner:nil];
    [pasteboard setString:[item query] forType:NSStringPboardType];
  }  
}


- (IBAction)executeHistory:(id)sender
{
  int row= [historyList selectedRow];
  id item= [historyList itemAtRow:row];
  
  [queryCtl performQuery:[item query]];
}


- (IBAction)deleteHistory:(id)sender
{
  int row= [historyList selectedRow];
  id item= [historyList itemAtRow:row];

  [[[MQueryBrowserController sharedDocumentController] history] removeItem:item];
  [historyList reloadData];
}



- (IBAction)copyBookmark:(id)sender
{
  int row= [bookmarkList selectedRow];
  id item= [bookmarkList itemAtRow:row];
  
  if (![item isKindOfClass:[MQBookmarkGroup class]])
  {
    NSPasteboard *pasteboard= [NSPasteboard generalPasteboard];
    [pasteboard declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                       owner:nil];
    [pasteboard setString:[item query:YES] forType:NSStringPboardType];
  }
}

- (IBAction)executeBookmark:(id)sender
{
  int row= [bookmarkList selectedRow];
  id item= [bookmarkList itemAtRow:row];

  [queryCtl performQuery:[item query:YES]];
}

- (IBAction)newBookmarkGroup:(id)sender
{
  int row= [bookmarkList selectedRow];
  id item= [bookmarkList itemAtRow:row];
  int nrow;
  id nitem;

  nitem= [[[MQueryBrowserController sharedDocumentController] bookmarks]
    addGroupInItem:item];
  [bookmarkList reloadData];
  
  nrow= [bookmarkList rowForItem:nitem];
  
  [bookmarkList selectRow:nrow byExtendingSelection:NO];
  [bookmarkList editColumn:0 row:nrow withEvent:nil select:YES];
  [[bookmarkList dataSource] save];
}

- (IBAction)deleteBookmarkItem:(id)sender
{
  int row= [bookmarkList selectedRow];
  id item= [bookmarkList itemAtRow:row];

  [[bookmarkList dataSource] removeBookmark:item];
  [bookmarkList reloadData];
  [[bookmarkList dataSource] save];
}

- (IBAction)renameBookmarkItem:(id)sender
{
  int row= [bookmarkList selectedRow];
  [bookmarkList editColumn:0 row:row withEvent:nil select:YES];
}


- (IBAction)bookmarkQuery:(id)sender
{
  NSString *query;
  MQAddBookmarkPanel *panel;
  
  query= [queryCtl currentQuery];
  if (!query)
  {
    NSBeep();
    return;
  }
  
  panel= [[[MQAddBookmarkPanel alloc] init] autorelease];

  [panel showSheetForWindow:[self window]
                  bookmarks:[[MQueryBrowserController sharedDocumentController] bookmarks]
                    catalog:@"def"
                     schema:[[self document] defaultSchema]
                      query:[NSString stringWithString:query]];
  [bookmarkList reloadData];
  [[bookmarkList dataSource] save];
}


- (IBAction)addParameter:(id)sender
{
  int row= [parameterList selectedRow];
  if (row >= 0)
  {
    [_parameters addParameter:[parameterList itemAtRow:row]];
    [parameterList reloadData];
  }
}


- (IBAction)deleteParameter:(id)sender
{
  int row= [parameterList selectedRow];
  if (row >= 0)
  {
    [_parameters deleteParameter:[parameterList itemAtRow:row]];
    [parameterList reloadData];
  }  
}


- (BOOL)keyDown:(NSEvent*)event
{
  if (([event modifierFlags] & (NSCommandKeyMask|NSShiftKeyMask))==(NSCommandKeyMask|NSShiftKeyMask))
  {
    switch ([[event characters] characterAtIndex:0])
    {
      case NSLeftArrowFunctionKey:
        [tabView selectPreviousPage];
        break;
      case NSRightArrowFunctionKey:
        [tabView selectNextPage];
        break;
    }
  }
  return NO;
}

- (void)schemaHelperChangedSchemata:(MSchemaEditHelper*)helper
{
  [self refreshSchemata:nil];
}


- (void)schemaHelper:(MSchemaEditHelper*)helper editScript:(NSString*)data
{
  [scriptCtl setupEditorWithScript:data];
}


- (NSString *)windowTitleForDocumentDisplayName:(NSString *)displayName
{
  if (displayName)
    return [NSString stringWithFormat:@"%@ @ %@", 
      displayName, [[self document] connectedInstance]];
  else
    return [[self document] connectedInstance];
}


- (void)windowWillClose:(NSNotification *)aNotification
{
  [_parameters saveGlobals];
  
//  [[MQueryBrowserController sharedDocumentController] removeInstance:self];
}


- (MSchemaEditHelper*)schemaHelper
{
  return schemaHelper;
}

- (MQParameters*)parameters
{
  return _parameters;
}

- (NSOutlineView*)parameterList
{
  return parameterList;
}

//==============================================================================

- (int)numberOfItemsInMenu:(NSMenu *)menu
{
  return [menu numberOfItems];
}


- (BOOL)menu:(NSMenu *)menu
  updateItem:(NSMenuItem *)item
	 atIndex:(int)index
shouldCancel:(BOOL)shouldCancel
{
  if (menu == catalogMenu)
  {
    int selRow= [catalogList selectedRow];
    int level= selRow < 0 ? -1 : [catalogList levelForRow:selRow];
    BOOL flag= NO;
    
    switch (index)
    {
      case 0: // Set Def. Schema
        if (level > 0)
          flag= YES;
        else
          flag= NO;
        break;
      case 2: // Create Schema
        if (level >= 0)
          flag= YES;
        else
          flag= NO;
        break;
      case 3: // Create Table
        if (level >= 1)
          flag= YES;
        else
          flag= NO;
        break;
      case 4: // Create SP
        if (level >= 1 && [[self document] connectedMySQLIsAtLeastMajor:5 minor:0])
          flag= YES;
        else
          flag= NO;
        break;
      case 5: // Create View
        if (level >= 1 && [[self document] connectedMySQLIsAtLeastMajor:5 minor:0])
          flag= YES;
        else
          flag= NO;
        break;
      case 6: // Edit
      {
        MSchemaItem *sitem= [catalogList itemAtRow:selRow];
        switch (level)
        {
          case 0:
            [item setTitle:@"Edit Object"];
            flag= NO;
            break;		  
          case 1:
            [item setTitle:@"Edit Schema"];
            flag= NO;//XXX NOT SUPPORTED YET
            break;		  
          case 2:
            if ([sitem type] == MTableItemType)
            {
              [item setTitle:@"Edit Table"];
              flag= YES;
            }
            else if ([sitem type] == MViewItemType)
            {
              [item setTitle:@"Edit View"];
              flag= [[self document] connectedMySQLIsAtLeastMajor:5 minor:0]?YES:0;
            }
            else
            {
              [item setTitle:@"Edit Stored Procedure"];
              flag= [[self document] connectedMySQLIsAtLeastMajor:5 minor:0]?YES:0;
            }
            break;
          default:
            [item setTitle:@"Edit Object"];
            flag= NO;
            break;
        } 
        break;
      }
      case 7: // Drop
        switch (level)
        {
          case 0:
            [item setTitle:@"Drop Object"];
            flag= NO;
            break;
          case 1:
            [item setTitle:@"Drop Schema"];
            flag= YES;
            break;
          case 2:
          {
            MSchemaItem *sitem= [catalogList itemAtRow:selRow];
            if ([sitem type] == MTableItemType)
            {
              [item setTitle:@"Drop Table"];
              flag= YES;
            }
            else if ([sitem type] == MViewItemType)
            {
              [item setTitle:@"Drop View"];
              flag= [[self document] connectedMySQLIsAtLeastMajor:5 minor:0]?YES:0;
            }
            else
            {
              [item setTitle:@"Drop Stored Procedure"];
              flag= [[self document] connectedMySQLIsAtLeastMajor:5 minor:0]?YES:0;
            }
            break;
          }
          default:
            [item setTitle:@"Drop Object"];
            flag= NO;
            break;
        }
        break;
      case 9: // Copy to Clipboard
      {
        MSchemaItem *sitem= [catalogList itemAtRow:selRow];
        switch ([sitem type])
        {
          case MTableItemType:
          case MViewItemType:
          case MSchemaItemType:
          case MSPItemType:
            flag= YES;
            break;
          default:
            flag= NO;
            break;
        }
        break;
      }
      case 11: // Refresh
        flag= YES;
        break;
    }
    
    [item setEnabled:flag];
  }
  else if (menu == bookmarkMenu)
  {
    int sel= [bookmarkList selectedRow];
    id bitem= sel < 0 ? nil : [bookmarkList itemAtRow:sel];
    BOOL isGroup= [bitem isKindOfClass:[MQBookmarkGroup class]];
    BOOL flag= NO;

    switch (index)
    {
      case 0:
        flag= !isGroup;
        break;
      case 1:
        flag= !isGroup;
        break;
      case 3:
        flag= !isGroup;
        break;
      case 4:
        flag= !isGroup;
        break;
      case 6:
        flag= YES;
        break;
      case 7:
        flag= isGroup;
        break;
      case 8:
        flag= isGroup;
        break;
    }
    [item setEnabled:flag];
  }
  else if (menu == historyMenu)
  {
    int sel= [historyList selectedRow];
    id hitem= sel < 0 ? nil : [historyList itemAtRow:sel];
    [item setEnabled:[hitem isKindOfClass:[MQHistoryItem class]]];
  }
  else if ([[[[[NSApp mainMenu] itemWithTag:100] submenu] itemWithTag:100] submenu] == menu) // Export Resultset submenu
  {
    if ([[self currentTab] isKindOfClass:[MQResultSetTab class]]
        && [[[self currentTab] activeResultSet] resultset])
      [item setEnabled:YES];
    else
      [item setEnabled:NO];
  }  
  else
  {    
    switch ([[[NSApp mainMenu] itemAtIndex:[[NSApp mainMenu] indexOfItemWithSubmenu:menu]] tag])
    {
      case 104: // Schema menu
      {
        int row= [catalogList selectedRow];
        MSchemaItem *sitem;
        BOOL flag= NO;
        
        switch (index)
        {
          case 0: // Show Table Rows
          case 1:
            if (row >= 0)
            {
              sitem= [catalogList itemAtRow:row];
              if ([sitem type] == MTableItemType || [sitem type] == MViewItemType)
                flag= YES;
            }
            break;
          case 3: // Create view with query
            if ([[self currentTab] isKindOfClass:[MQResultSetTab class]])
              flag= YES;
            break;
          case 4: // Edit all SPs
            if ([[self document] defaultSchema] || (row >= 0 && [(MSchemaItem*)[catalogList itemAtRow:row] type] != MCatalogItemType))
              flag= YES;
            break;
        }
        [item setEnabled:flag];
        return YES;
      }
      case 105: // Query menu
        return [queryCtl stateOfMenuItem:item atIndex:index inMenu:menu];

      case 106: // Script menu
        return [scriptCtl stateOfMenuItem:item atIndex:index inMenu:menu];
    }
  }
  return YES;
}


- (void)tabViewChangedPage:(MTabView*)sender
{
  id page= [sender selectedPage];
  
  if ([page isKindOfClass:[MQResultSetTab class]])
  {
    [queryCtl willShowPage:page];
    [scriptCtl didHidePage];
  }
  else if ([page isKindOfClass:[MQScriptTab class]])
  {
    [scriptCtl willShowPage:page];
    [queryCtl didHidePage];
  }
}


- (IBAction)switchToolbarType:(id)sender
{
  BOOL small;

  if ([sender tag] != 100)
  {
    [sender setTag:100];
    [sender setTitle:@"Normal Query Area"];
    small= YES;
  }
  else
  {
    [sender setTag:101];
    [sender setTitle:@"Maximize Query Area"];
    small= NO;
  }
  [self setSmallToolbar:small];
}


- (void)setToolbarHeight:(float)height
{  
  if (!_smallToolbar)
  {
    NSRect rect= [tabView frame];
    
    rect.size.height= (rect.size.height + height) - _toolbarHeight;    
    [tabView setFrame:rect];
    
    [queryCtl resizeToolbar:height];
    [scriptCtl resizeToolbar:height];
  }
  _toolbarHeight= height;
}

- (void) saveScript: (id)sender
{
  [scriptCtl saveScript: sender];
}

- (void) saveQueryOrScript
{
  NSString *current_query= [queryCtl currentQuery];
  if(current_query != nil)
    [queryCtl saveQuery: self];
  else
    [scriptCtl saveScript: self];
}

- (void)setSmallToolbar:(BOOL)flag
{
  if (_smallToolbar != flag)
  {
    NSRect rect= [tabView frame];

    _smallToolbar= flag;
    
    if (flag)
      rect.size.height= (rect.size.height + _toolbarHeight) - 38;
    else
      rect.size.height= (rect.size.height + 38) - _toolbarHeight;
 
    [tabView setFrame:rect];

    [queryCtl switchToolbar:flag];
    [scriptCtl switchToolbar:flag];
    
    [[[self window] contentView] setNeedsDisplay:YES];
  }
}


- (BOOL)smallToolbar
{
  return _smallToolbar;
}


- (id)currentTab
{
  return [tabView selectedPage];
}


- (void)closeTabAlertDidEnd:(NSAlert *)alert returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
  if (returnCode == NSAlertDefaultReturn)
    [self close];
}


- (void)modalAlertDidEnd:(NSAlert*)alert returnCode:(int)returnCode contextInfo:(void*)info
{
  [NSApp stopModalWithCode:returnCode];
}

- (void)tabView:(MTabView*)aTabView closeTab:(MTabViewItem*)tab
{
  if ([tab isKindOfClass:[MQScriptTab class]])
  {
    if ([(MQScriptTab*)tab isDocumentEdited])
    {
      NSAlert *alert= [NSAlert alertWithMessageText:@"Save script before closing?"
                                      defaultButton:@"Save"
                                    alternateButton:@"Don't Save"
                                        otherButton:@"Cancel"
                          informativeTextWithFormat:@"This script has changes that were not yet saved."];
      [alert beginSheetModalForWindow:[self window]
                        modalDelegate:self
                       didEndSelector:@selector(modalAlertDidEnd:returnCode:contextInfo:) 
                          contextInfo:tab];
      switch ([NSApp runModalForWindow:[self window]])
      {
        case NSAlertDefaultReturn:
          [(MQScriptTab*)tab saveScript:nil];
          break;
        case NSAlertAlternateReturn:
          break;
        case NSAlertOtherReturn:
          return;
      }
    }
  }
  else if ([tab isKindOfClass:[MQResultSetTab class]])
  {
    if ([(MQResultSetTab*)tab hasUnsavedChanges])
    {
      NSAlert *alert= [NSAlert alertWithMessageText:@"Discard resultset changes?"
                                      defaultButton:@"Cancel"
                                    alternateButton:@"Discard and Close"
                                        otherButton:nil
                          informativeTextWithFormat:@"There are resultset changes that were not yet saved and will be discarded if the tabsheet is closed."];
      [alert beginSheetModalForWindow:[self window]
                        modalDelegate:self
                       didEndSelector:@selector(modalAlertDidEnd:returnCode:contextInfo:) 
                          contextInfo:tab];
      switch ([NSApp runModalForWindow:[self window]])
      {
        case NSAlertDefaultReturn:
          return;
        case NSAlertAlternateReturn:
          break;
        case NSAlertOtherReturn:
          break;
      }
    }
  }
  
  
  if ([aTabView numberOfPages] == 1)
  {
    [[NSAlert alertWithMessageText:@"Close Tabsheet"
                    defaultButton:@"Close"
                  alternateButton:@"Cancel"
                      otherButton:nil
        informativeTextWithFormat:@"Closing the last tabsheet in the window will close the window as well."]
      beginSheetModalForWindow:[self window]
                 modalDelegate:self
                didEndSelector:@selector(closeTabAlertDidEnd:returnCode:contextInfo:) 
                   contextInfo:tab];
  }
  else
    [self closeTab:tab];
}


- (void)performCloseTab:(id)sender
{
  [self tabView:tabView closeTab:[self currentTab]];
}


- (void)outlineView:(NSOutlineView *)outlineView 
    willDisplayCell:(id)cell 
     forTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if (outlineView == catalogList)
  {
	NSString *schema= [[self document] defaultSchema];
	
	if ([item respondsToSelector:@selector(icon)])
	  [cell setImage:[item icon]];
	else
	  [cell setImage:nil];
    
	if ([catalogList levelForItem:item] == 1)
	{
	  if ([[item repr] isEqualToString:schema])
		[cell setFont:_boldFont];
	  else
		[cell setFont:_normalFont];
	}
	else
	  [cell setFont:_normalFont];
  }
  else if (outlineView == historyList)
  {
	if ([historyList levelForItem:item] == 0)
	  [cell setImage:_historyGroupIcon];
	else
	  [cell setImage:_historyItemIcon];
  }
  else if (outlineView == bookmarkList)
  {
    if ([item isKindOfClass:[MQBookmarkGroup class]])
      [cell setImage:_bookmarkGroupIcon];
    else
      [cell setImage:_bookmarkItemIcon];
  }
  else if (outlineView == parameterList)
  {
    if ([[tableColumn identifier] isEqualToString:@"name"])
    {
      if ([item isKindOfClass:[NSString class]])
        [cell setImage:_groupIcon];
      else
        [cell setImage:_paramIcon];
    }
  }
}


- (BOOL)outlineView:(NSOutlineView *)outlineView
 shouldEditTableColumn:(NSTableColumn *)tableColumn 
               item:(id)item
{
  if (outlineView == parameterList)
  {
    int row= [parameterList selectedRow];
    
    if (row >= 0)
    {
      id item= [parameterList itemAtRow:row];
      if ([_parameters itemCanBeEdited:item])
      {
        return YES;
      }
    }
    return NO;
  }
  return YES;
}


- (void)outlineViewSelectionDidChange:(NSNotification *)notification
{
  if ([notification object] == parameterList)
  {
    int row= [parameterList selectedRow];
    BOOL add= NO;
    BOOL del= NO;
    if (row >= 0)
    {
      id item= [parameterList itemAtRow:row];
      if ([_parameters itemCanBeAdded:item])
        add= YES;
      if ([_parameters itemCanBeDeleted:item])
        del= YES;
    }
    [[[parameterList enclosingScrollView] viewWithTag:1] setEnabled:add];
    [[[parameterList enclosingScrollView] viewWithTag:2] setEnabled:del];
  }
}


@end
