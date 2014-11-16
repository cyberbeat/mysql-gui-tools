//
//  MQQueryController.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/31/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <MySQLToolsCommon/MMenuButton.h>
#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/MQTableView.h>
#import <MySQLToolsCommon/MQResultSetView.h>
#import <MySQLToolsCommon/MSourceTextEditor.h>
#import <MySQLToolsCommon/MSourceTextView.h>
#import <MySQLToolsCommon/MSQLSyntaxColoring.h>

#import "MQueryBrowserController.h"
#import "MQueryBrowserDocument.h"
#import "MQResultSetTab.h"
#import "MQQueryController.h"
#import "MQueryBrowser.h"
#import "MQBookmark.h"
#import "MQActionBar.h"
#import "MQParameters.h"


#include <myx_public_interface.h>

@interface MQActionBarTextView : MSourceTextView
{
@public
  MQActionBar *actionBar;
}
@end

@implementation MQActionBarTextView
- (id)initWithFrame:(NSRect)frame
{
  self= [super initWithFrame:frame];
  if (self)
  {
    actionBar= [[MQActionBar alloc] init];
    [self registerForDraggedTypes:[NSArray arrayWithObject:MSchemaItemPboardType]];
  }
  return self;
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
  NSPoint pt= [self frame].origin;
  pt.y+= NSHeight([self frame]);
  if (![actionBar superview])
  {
    NSView *view= [[self window] contentView];
    
    [actionBar setHidden:YES];
    [view addSubview:actionBar positioned:NSWindowAbove relativeTo:[[view subviews] objectAtIndex:0]];
  }
  
  if ([[[sender draggingSource] dataSource] isKindOfClass:[MSchemaDataSource class]])
    [actionBar popupAt:[[self superview] convertPoint:pt toView:nil]];

  return NSDragOperationNone;
}

- (void)draggingExited:(id <NSDraggingInfo>)sender
{
  [actionBar delayedHide];
}

- (void)draggingEnd:(id <NSDraggingInfo>)sender
{
  [actionBar delayedHide];
}

/*
- (NSArray *)completionsForPartialWordRange:(NSRange)charRange 
                        indexOfSelectedItem:(int *)index
{
  return [_syntaxColor completionListForWord:[[self string] substringWithRange:charRange]];
}
*/

@end


@implementation MQQueryAreaEditor

- (MSourceTextView*)createTextViewWithFrame:(NSRect)frame
{
  return [[MQActionBarTextView alloc] initWithFrame:frame];
}

@end


@implementation MQQueryController

- (void)awakeFromNib
{
  NSRect wrect= [[owner window] contentRectForFrameRect:[[owner window] frame]];
  NSRect trect;
  NSUserDefaults *defaults= [NSUserDefaults standardUserDefaults];
  
  trect= [toolbar frame];
  [toolbar setFrameOrigin:NSMakePoint(-1,NSHeight(wrect)-NSHeight(trect))];
  
  trect= [bigToolbar frame];
  [bigToolbar setFrameOrigin:NSMakePoint(-1,NSHeight(wrect)-NSHeight(trect))];
  
  queryArea= [queryEditor textView];

  // no horizontal scroller
  [queryArea setHorizontallyResizable:NO];
  [[queryArea textContainer] setWidthTracksTextView:YES];
    
  
  [((MQActionBarTextView*)queryArea)->actionBar setTextView:queryArea];

  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(textDidChange:)
                                               name:NSTextDidChangeNotification
                                             object:queryArea];

  [[[queryArea enclosingScrollView] verticalScroller] setControlSize:NSSmallControlSize];
  [[[queryArea enclosingScrollView] horizontalScroller] setControlSize:NSSmallControlSize];
  [[queryArea enclosingScrollView] setAutohidesScrollers:YES];
  
  [queryArea registerForDraggedTypes:[NSArray arrayWithObject:MSchemaItemPboardType]];
  
  [queryArea setFont:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorFont"]]];
  [queryArea setBackgroundColor:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorBackgroundColor"]]];
  [queryArea setTextColor:[NSUnarchiver unarchiveObjectWithData:[defaults objectForKey:@"EditorForegroundColor"]]];

  _idleImage= [[NSImage imageNamed:@"sakila.png"] retain];
  _busyImage= [[NSImage imageNamed:@"dolphin_anim.gif"] retain];
  
  _queryViews= [[NSMutableArray alloc] init];  
  
  _localHistory= [[NSMutableArray alloc] init];   
}


- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  if (_syntaxInfo)
	myx_free_syn(_syntaxInfo);
  
  [_queryViews release];
  [_idleImage release];
  [_busyImage release];
  [_localHistory release];
  
  [super dealloc];
}


- (MQResultSetTab*)createPage
{
  MQResultSetTab *page= [[MQResultSetTab alloc] initWithIdentifier:@"aa"];
  
  [page setIcon:[NSImage imageNamed:@"tabsheet_icon_resultset.png"]];
  [page setLabel:@"Query"];
  
  return [page autorelease];
}


- (IBAction)compareQuery:(id)sender
{
  if ([[owner currentTab] isKindOfClass:[MQResultSetTab class]])
    [[owner currentTab] compareResultsets:sender]; 
}


- (IBAction)executeQuery:(id)sender
{
  NSString *query= [NSString stringWithString:[queryArea string]];
  MQResultSetTab *page;
  MQResultSetView *rsview;
  int i;
  
  switch ([sender tag])
  {
    case 2:
      [owner addQueryTab:nil]; 
      page= [owner currentTab];
      rsview= [page activeResultSet];
      [queryArea setString:query];
      break;
    case 3:
      [self splitV:nil];
      page= [owner currentTab];
      rsview= [page activeResultSet];
      [rsview setQuery:query];
      [queryArea setString:query];
      break;
    case 1:
    default:
      page= [owner currentTab];
      rsview= [page activeResultSet];
      [rsview setQuery:query];
      break;
  }
  [rsview performQuery:query withParameters:[[owner parameters] parametersFromMaster:[rsview masterView]]];
  
  for (i= 0; i < _localHistoryPos; i++)
    [_localHistory removeObjectAtIndex:0];
  _localHistoryPos= 0;
  
  if ([_localHistory indexOfObject:query] == NSNotFound)
    [_localHistory insertObject:query atIndex:0];
  
  [self updateHistoryMenus];
}

- (IBAction)explainQuery:(id)sender
{
  if ([[owner currentTab] isKindOfClass:[MQResultSetTab class]])
  {
    MQResultSetTab *rstab= [owner currentTab];
    NSString *query= [[self currentResultSetView] query];

    if (query && [query length]>0)
      [[self addNewResultSet:[rstab vertical]
                      toPage:rstab] performQuery:[NSString stringWithFormat:@"EXPLAIN %@", query]];
  }
}


- (void)exportResultSet:(id)sender
{
  MQResultSetTab *page= [owner currentTab];

  if ([page isKindOfClass:[MQResultSetTab class]])
  {
    const char *format= NULL;
    switch ([sender tag])
    {
      case 1: // CSV
        format= "CSV";
        break;
      case 2: // HTML
        format= "HTML";
        break;
      case 3: // XML
        format= "XML";
        break;
      case 4: // Excel
        format= "Excel";
        break;
      case 5: // PropList
        format= "PLIST";
        break;
    }
    if (format)
      [page exportResultSet:format];
  }
}


- (void)openLocalHistory:(id)sender
{
  id item;
  
  if ([sender isKindOfClass:[NSString class]])
    item= sender;
  else
    item= [sender representedObject];
  [self performQuery:item];
  _localHistoryPos= [_localHistory indexOfObject:item];
}


- (void)updateHistoryMenus
{
  int i, c;
  BOOL ok;
  
  while ([backMenu numberOfItems])
    [backMenu removeItemAtIndex:0];
  while ([nextMenu numberOfItems])
    [nextMenu removeItemAtIndex:0];
  
  c= [_localHistory count];
  if (_localHistoryPos >= c && c > 0)
    _localHistoryPos= c-1;
  
  ok= NO;
  for (i= _localHistoryPos+1; i < c; i++)
  {
    NSMenuItem *item= [backMenu addItemWithTitle:[_localHistory objectAtIndex:i]
                                          action:@selector(openLocalHistory:)
                                   keyEquivalent:@""];
    [item setRepresentedObject:[_localHistory objectAtIndex:i]];
    [item setTarget:self];
    [item setAction:@selector(openLocalHistory:)];
    ok= YES;
  }
  [backButton setEnabled:ok];
  [[toolbar viewWithTag:10] setEnabled:ok];
  
  ok= NO;
  for (i= _localHistoryPos-1; i >= 0; i--)
  {
    NSMenuItem *item= [nextMenu addItemWithTitle:[_localHistory objectAtIndex:i]
                                          action:@selector(openLocalHistory:)
                                   keyEquivalent:@""];
    [item setRepresentedObject:[_localHistory objectAtIndex:i]];
    [item setTarget:self];
    [item setAction:@selector(openLocalHistory:)];
    ok= YES;
  }
  [nextButton setEnabled:ok];
  [[toolbar viewWithTag:11] setEnabled:ok];
}

- (IBAction)goBack:(id)sender
{
  id item= [_localHistory objectAtIndex:++_localHistoryPos];
  [self openLocalHistory:item];
  [self updateHistoryMenus];
}

- (IBAction)goNext:(id)sender
{
  id item= [_localHistory objectAtIndex:--_localHistoryPos];
  [self openLocalHistory:item];
  [self updateHistoryMenus];
}


- (void)updateTransactionState
{
  BOOL flag= [[[owner currentTab] activeResultSet] transactionOpen];
    
  [[toolbar viewWithTag:14] setEnabled:!flag];
  [[toolbar viewWithTag:15] setEnabled:flag];
  [[toolbar viewWithTag:16] setEnabled:flag];
}


- (IBAction)rollbackTransaction:(id)sender
{
  [[[owner currentTab] activeResultSet] rollbackTransaction];
}

- (IBAction)startTransaction:(id)sender
{
  [[[owner currentTab] activeResultSet] startTransaction];
}

- (IBAction)commitTransaction:(id)sender
{
  [[[owner currentTab] activeResultSet] commitTransaction];
}

- (IBAction)stopQuery:(id)sender
{
  [[owner document] killConnection:mysql_thread_id([[self currentResultSetView] mysql])];
  [[self currentResultSetView] stopQuery];
}

- (void)selectQueryArea:(id)sender
{
  [[owner window] makeFirstResponder:queryArea];
  if (sender)
    [queryArea selectAll:sender];
}

- (void)setDefaultSchema:(NSString*)schema
{
  MYSQL *mysql= [[owner document] mysql];
  
  _syntaxInfo= myx_refresh_dbinfo(mysql, _syntaxInfo);
  
  MSQLSyntaxColoring *colorer= [[MSQLSyntaxColoring alloc] initForTextView:queryArea
                                                                syntaxInfo:_syntaxInfo];
  [queryArea setSyntaxColorer: colorer];
  [colorer release];
  
  [((MQActionBarTextView*)queryArea)->actionBar setMySQL:mysql];
}



- (MQResultSetView*)addNewResultSet:(BOOL)vertical
                             toPage:(MQResultSetTab*)page
{
  MQResultSetView *rsview;
  
  rsview= [[[MQResultSetView alloc] initWithConnectionTo:[[owner document] serverInfo]] autorelease];
  [[rsview tableView] registerForDraggedTypes:
    [NSArray arrayWithObjects:MQBookmarkPboardType,MSchemaItemPboardType,nil]];
  
  [[rsview tableView] setDragHandlers:[NSDictionary dictionaryWithObjectsAndKeys:
    NSStringFromSelector(@selector(handleDropToRSTable:dragInfo:)), MQBookmarkPboardType, 
    NSStringFromSelector(@selector(handleDropToRSTable:dragInfo:)), MSchemaItemPboardType,
    nil]
                               target:self];
  
  [rsview setDelegate:self];
  [rsview setDefaultSchema:[[owner document] defaultSchema]];
  
  [page setVertical:vertical];
  [page addResultSet:rsview];
  
  [_queryViews addObject:rsview];
  [page setActiveResultSet:rsview];
  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(masterChangedSelection:)
                                               name:NSTableViewSelectionDidChangeNotification
                                             object:[rsview tableView]];
  return rsview;
}


- (void)performQuery:(NSString*)query
{
  MQResultSetView *rsview= [(MQResultSetTab*)[owner currentTab] activeResultSet];
  [queryArea setString:query];
  [rsview performQuery:query withParameters:[[owner parameters] parametersFromMaster:[rsview masterView]]];
}

- (MQResultSetView*)currentResultSetView
{
  id rstab= [owner currentTab];
  
  if ([rstab isKindOfClass:[MQResultSetTab class]])
    return [rstab activeResultSet];
  else
    return nil;
}

- (NSString*)currentQuery
{
  MQResultSetView *rsview= [self currentResultSetView];
  
  if (rsview)
    return [rsview query];
  else
    return nil;
}

- (void) saveQuery: (id) sender
{
  [[self currentResultSetView] saveQuery];
}

- (BOOL)setupQueryWithItem:(MSchemaItem*)item
{
  char *query;
  MYX_SCHEMA *schema= [item schema];
  MYX_Q_TABLE_ADD_ERROR error;
  int cursor= 0;
  MQResultSetView *rsview= [self currentResultSetView];
  
  if (!rsview)
    return NO;
  
  if ([item type] == MTableItemType || [item type] == MViewItemType)
  {
    MYX_SCHEMA_TABLE *table= [item table];
  
    query= myx_query_add_table_to_sql([rsview mysql],
                                      [[rsview defaultSchema] UTF8String],
                                      schema->catalog_name, schema->schema_name,
                                      table->table_name, "",
                                      MYX_QTAT_SELECT, &cursor, &error);
  }
  else
  {
    MYX_SCHEMA_STORED_PROCEDURE *sp= [item sp];
    char **paraml= NULL;
    char *params= NULL;
    unsigned int i;
    
    if (sp->params_num > 0)
    {
      paraml= g_new0(char*,sp->params_num);
      for (i= 0; i < sp->params_num; i++)
      {
        paraml[i]= g_strdup_printf("<%s:%s>", sp->params[i].name, sp->params[i].datatype); 
      }
  
      params= g_strjoinv(", ",paraml);
      query= g_strdup_printf("CALL %s(%s)", sp->name, params);
    }
    else
      query= g_strdup_printf("CALL %s()", sp->name);
    cursor= strchr(query, '(')+1-query;
    g_free(params);
    g_strfreev(paraml);
  }
  
  if (query)
  {
    NSString *s= [NSString stringWithUTF8String:query];
    if ([[queryArea string] isEqualTo:s])
      return YES;
    else
    {
      [queryArea setString:s];
      [queryArea setSelectedRange:NSMakeRange(cursor, 1)];
    }
    g_free(query);
  }
  return NO;
}


- (void)setupSelectQueryWithItem:(MSchemaItem*)item 
                        rowLimit:(unsigned int)limit
{
  [self setupQueryWithItem:item];
  
  if (limit > 0)
    [queryArea insertText:[NSString stringWithFormat:@"\nLIMIT 0,%i", limit]];
}


- (MQResultSetView*)resultSetViewWithTable:(MQTableView*)table
{
  unsigned int i, c= [_queryViews count];
  for (i= 0; i < c; i++)
  {
    MQResultSetView *rsv= [_queryViews objectAtIndex:i];
    if ([rsv tableView] == table)
      return rsv;
  }
  return nil;
}



//-----------------------------------------------------------------------------
- (void)textDidChange:(NSNotification *)aNotification
{
  if (!_switching)
  {
    NSString *query= [NSString stringWithString:[queryArea string]];
    int lines;
    float theight;
    [[self currentResultSetView] setQuery:query];
   
    lines= [[[queryEditor textView] syntaxColorer] numberOfLines];
 
    theight= lines * [[queryArea layoutManager] defaultLineHeightForFont:[queryArea font]];
    if (theight < 58)
      theight= 58;
    else if (lines > 10)
      theight= 10 * [[queryArea layoutManager] defaultLineHeightForFont:[queryArea font]];
   
//    NSLog(@"resize to %f / %i", theight, lines);
//    [self resizeToolbar:theight];

    //[[[NSApp orderedDocuments] objectAtIndex: 0] updateChangeCount: NSChangeDone];
  }
}

- (void)resultSetViewTransactionStarted:(MQResultSetView*)rsview
{
  [self updateTransactionState];
  if ([[owner currentTab] isKindOfClass:[MQResultSetTab class]])
  {
    MQResultSetTab *rstab= [owner currentTab];
    [rstab setStatusText:@"Transaction Started" icon:nil];
  }
}


- (void)resultSetViewTransactionEnded:(MQResultSetView*)rsview
                             commited:(BOOL)commited
{
  if ([[owner currentTab] isKindOfClass:[MQResultSetTab class]])
  {
    MQResultSetTab *rstab= [owner currentTab];
    if (commited)
      [rstab setStatusText:@"Transaction Commited" icon:nil];
    else
      [rstab setStatusText:@"Transaction Rolled Back" icon:nil];
  }  
  [self updateTransactionState];
}


- (void)resultSetViewWantsFocus:(MQResultSetView*)rsview
{
  [[(MQResultSetTab*)[owner currentTab] activeResultSet] setLocalParameters:[[owner parameters] localParameters]];
  
  [(MQResultSetTab*)[owner currentTab] setActiveResultSet:rsview];
}

- (void)resultSetViewDidGetFocus:(MQResultSetView*)rsview
{
  NSString *query= [NSString stringWithString:[rsview query]];
  MQResultSetView *master= [rsview masterView];
  
  _switching= YES;
  [queryArea setString:query];
  _switching= NO;
//XXX  [owner setTitleForDocumentName:[rsview defaultSchema]];

  [[owner parameters] setLocalParameters:[rsview localParameters]];
  if (master)
  {
    [[owner parameters] setResultset:[master resultset]
                                 row:[[master tableView] selectedRow]];
    [[owner parameterList] reloadData];
  }
  else
  {
    [[owner parameters] setResultset:NULL
                                 row:-1];
    [[owner parameterList] reloadData];    
  }
  [self updateTransactionState];
}

- (void)resultSetView:(MQResultSetView*)rsview willStartQuery:(NSString*)query
{
  [animbox setImage:_busyImage];

  [[toolbar viewWithTag:10] setEnabled:NO];
  [backButton setEnabled:NO];  
  [[toolbar viewWithTag:11] setEnabled:NO];
  [nextButton setEnabled:NO];
  
  [[toolbar viewWithTag:12] setEnabled:NO];
  [executeButton setEnabled:NO];
  [[toolbar viewWithTag:13] setEnabled:YES];  
  [stopButton setEnabled:YES];
  
  [[toolbar viewWithTag:14] setEnabled:NO];
  [[toolbar viewWithTag:15] setEnabled:NO];
  [[toolbar viewWithTag:16] setEnabled:NO];
  [[toolbar viewWithTag:17] setEnabled:NO];
  [[toolbar viewWithTag:18] setEnabled:NO];
  
  [queryArea setEditable:NO];
}

- (void)resultSetViewFinishedQuery:(MQResultSetView*)rsview error:(BOOL)flag
{
  [animbox setImage:_idleImage];
  [backButton setEnabled:[backMenu numberOfItems] > 0];
  [[toolbar viewWithTag:10] setEnabled:YES];
  [nextButton setEnabled:[nextMenu numberOfItems] > 0];
  [[toolbar viewWithTag:11] setEnabled:YES];  
  [executeButton setEnabled:YES];
  [[toolbar viewWithTag:12] setEnabled:YES];
  [stopButton setEnabled:NO];
  [[toolbar viewWithTag:13] setEnabled:NO];

  [[toolbar viewWithTag:17] setEnabled:YES];
  [[toolbar viewWithTag:18] setEnabled:YES];

  [self updateTransactionState];
  [queryArea setEditable:YES];
  
  if (!flag)
  {
    [[MQueryBrowserController sharedDocumentController]
	rememberQuery:[NSString stringWithString:[rsview query]]
        catalog:@"def"
         schema:[rsview defaultSchema]];
  }
}


- (IBAction)splitV:(id)sender
{
  [[self addNewResultSet:YES toPage:(MQResultSetTab*)[owner currentTab]] setMasterView:sender];
  [[owner parameterList] reloadData];
}


- (IBAction)splitH:(id)sender
{
  [[self addNewResultSet:NO toPage:(MQResultSetTab*)[owner currentTab]] setMasterView:sender];
  [[owner parameterList] reloadData];
}


- (IBAction)unsplit:(id)sender
{
  MQResultSetView *rsview= sender;
  
  [(MQResultSetTab*)[owner currentTab] closeResultSet:rsview];
  
  [_queryViews removeObject:rsview];
}


- (void)switchToolbar:(BOOL)small
{
  if ([[owner currentTab] isKindOfClass:[MQResultSetTab class]])
  {
    NSView *qa= queryEditor;
    
    [bigToolbar setHidden:small?YES:NO];
    [toolbar setHidden:small?NO:YES];
    
    if (small)
    {
      [[owner currentTab] setEmbeddedQueryArea:YES];
      
      _queryAreaFrame= [qa frame];
      [qa retain];
      [qa removeFromSuperview];
      [[owner currentTab] setTopView:qa];
      [qa release];
    }
    else
    {
      [[owner currentTab] setEmbeddedQueryArea:NO];
      [qa retain];
      [qa removeFromSuperview];
      [bigToolbar addSubview:qa];
      [qa setFrame:_queryAreaFrame];
      [qa release];
    }
  }
}


- (void)resizeToolbar:(float)height
{
  NSRect frame= [bigToolbar frame];
  
  frame.origin.y-= height-frame.size.height;
  frame.size.height= height;
  [bigToolbar setFrame:frame];
}


- (void)willShowPage:(id)page
{
  MQResultSetView *rsview= [page activeResultSet];

  [[queryEditor retain] autorelease];

  if (_lastSelectedPage != page && _lastSelectedPage)
  {
    if ([owner smallToolbar])
      [_lastSelectedPage setTopView:nil];
  }

  if ([rsview query])
  {
    [queryArea setString:[rsview query]];
  }

  if ([rsview isBusy])
  {
    [animbox setImage:_busyImage];
    [backButton setEnabled:NO];
    [[toolbar viewWithTag:10] setEnabled:NO];
    [nextButton setEnabled:NO];
    [[toolbar viewWithTag:11] setEnabled:NO];
    [executeButton setEnabled:NO];
    [[toolbar viewWithTag:12] setEnabled:NO];
    [stopButton setEnabled:YES];
    [[toolbar viewWithTag:13] setEnabled:YES];
    [queryArea setEditable:NO];
  }
  else
  {
    [animbox setImage:_idleImage];
    [backButton setEnabled:[backMenu numberOfItems]>0];
    [[toolbar viewWithTag:10] setEnabled:[backMenu numberOfItems]>0];
    [nextButton setEnabled:[nextMenu numberOfItems]>0];
    [[toolbar viewWithTag:11] setEnabled:[nextMenu numberOfItems]>0];
    [executeButton setEnabled:YES];
    [[toolbar viewWithTag:12] setEnabled:YES];
    [stopButton setEnabled:NO];
    [[toolbar viewWithTag:13] setEnabled:NO];
    [queryArea setEditable:YES];
  }

  if ([owner smallToolbar])
  {
    [toolbar setHidden:NO];
    [bigToolbar setHidden:YES];
    [page setEmbeddedQueryArea:YES];
    [page setTopView:queryEditor];
  }
  else
  {
    [toolbar setHidden:YES];
    [bigToolbar setHidden:NO];
    [page setEmbeddedQueryArea:NO];
  }
  [owner setDocumentEdited:NO];
//XXX  [owner setTitleForDocumentName:[rsview defaultSchema]];
  [self updateTransactionState];

  _lastSelectedPage= page;
}

- (void)didHidePage
{
  [toolbar setHidden:YES];
  [bigToolbar setHidden:YES];
}

- (BOOL)handleDropToRSTable:(MQTableView*)table
                   dragInfo:(id)info
{
  NSPasteboard *pboard= [info draggingPasteboard];
  NSArray *array= [pboard types];
  
  if ([array indexOfObject:MQBookmarkPboardType]!=NSNotFound)
  {
    NSString *query= [pboard stringForType:MQBookmarkPboardType];
    [(MQResultSetTab*)[owner currentTab] setActiveResultSet:[self resultSetViewWithTable:table]];
    [self performQuery:query];
    return YES;
  }
  else if ([array indexOfObject:MSchemaItemPboardType]!=NSNotFound)
  {
    MSchemaItem *item;
    
    [(MQResultSetTab*)[owner currentTab] setActiveResultSet:[self resultSetViewWithTable:table]];
    item= [[[info draggingSource] dataSource] draggedItem];
    if ([item type] == MTableItemType || [item type] == MViewItemType)
    {
      [self setupSelectQueryWithItem:item 
                            rowLimit:[[NSUserDefaults standardUserDefaults] integerForKey:@"PartialTableListLimit"]];
      [self executeQuery:nil];
      return YES;
    }
  }
  return NO;
}


static BOOL hasVariables(MYX_RESULTSET *rs, const char *query)
{
  unsigned int i;
  const char *ptr= query;
  
  while ((ptr= strchr(ptr, ':')))
  {
    ++ptr;
    for (i= 0; i < rs->columns_num_to_display; i++)
    {
      if (strncmp(rs->columns[i].name, ptr, strlen(rs->columns[i].name))==0)
        return YES;
    }
  }
  return NO;
}


- (void)masterChangedSelection:(NSNotification*)notif
{
  MQResultSetView *master= [self resultSetViewWithTable:[notif object]];
  NSArray *details= [[owner currentTab] resultSetsWithMaster:master];
  if (details)
  {
    unsigned int i, c= [details count];

    for (i= 0; i < c; i++)
    {
      MQResultSetView *rsview= [details objectAtIndex:i];
      if ([rsview resultset] && [rsview query] && hasVariables([master resultset], [[rsview query] UTF8String]))
      {
        [rsview performQuery:[rsview query]
              withParameters:[[owner parameters] parametersFromMaster:master]];
      }
    }
  }
}


- (BOOL)stateOfMenuItem:(NSMenuItem*)item atIndex:(int)index inMenu:(NSMenu*)menu
{
  if ([[[NSApp mainMenu] itemAtIndex:[[NSApp mainMenu] indexOfItemWithSubmenu:menu]] tag]== 105)
  {
    MQResultSetTab *page= [owner currentTab];
    BOOL flag= NO;

    if (![page isKindOfClass:[MQResultSetTab class]])
    {
      [item setEnabled:NO];
      return YES;
    }

    switch (index)
    {
      case 0: // Bookmark
        if ([[[page activeResultSet] query] length] > 0)
          flag= YES;
        break;
      case 2: // Execute
        if (![[page activeResultSet] isBusy])
          flag= YES;
        break;
      case 3: // Stop
        if ([[page activeResultSet] isBusy])
          flag= YES;
        break;
      case 5: // Explain Query
        if (![[page activeResultSet] isBusy] && [[page activeResultSet] resultset])
          flag= YES;
        break;
      case 6: // Compare Resultsets
        if (![[page activeResultSet] isBusy])
          flag= YES;
        break;
    }
    [item setEnabled:flag];
  }
  return YES;
}

@end
