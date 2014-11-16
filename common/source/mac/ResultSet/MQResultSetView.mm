#import "MQResultSetView.h"
#import "MQResultSetDataSource.h"
#import "MQFieldViewer.h"
#import "MQTableView.h"
#import "MQIndicatorCell.h"
#import "MQResultSetCell.h"

#include <errmsg.h>
#include <myx_public_interface.h>

#import <MySQLToolsCommon/MConnectionInfo.h>
#import <MySQLToolsCommon/mxUtils.h>
#import <MySQLToolsCommon/NSString_extras.h>

#import "MAccessoryScrollView.h"
#import "MSplitView.h"


static int editBarTags[]= {
  49, 50, 51, 52, 53, 54, 55
};

typedef struct {
  int error;
  MYX_QUERY_ERROR_LEVEL level;
  NSString *message;
  unsigned int row;
  unsigned int column;
} MessageItem;

static NSLock *resultset_lock= nil;

static void resultset_realloc_before (void *user_data)
{
  [resultset_lock lock];
}

static void resultset_realloc_after (void *user_data)
{
  [resultset_lock unlock];
}

@interface MessageDS : NSObject
{
  @public
  MessageItem *items;
  int itemCount;
  NSImage *images[3];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex;
- (void)addError:(int)error
          ofType:(MYX_QUERY_ERROR_LEVEL)level
         message:(NSString*)message
           atRow:(unsigned int)row
          column:(unsigned int)column;
- (void)reset;

@end

@implementation MessageDS

- (id)init
{
  self= [super init];
  if (self)
  {
    items= NULL;
    itemCount= 0;
    images[0]= MXGetCommonImage(MXMiniNoticeImage);
    images[1]= MXGetCommonImage(MXMiniWarningImage);
    images[2]= MXGetCommonImage(MXMiniErrorImage);
  }
  return self;
}


- (void)dealloc
{
  int i;  
  for (i= 0; i < itemCount; i++)
    [items[i].message release];
  g_free(items);
  items= 0;
  [super dealloc];
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return itemCount;
}


- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
            row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualTo:@"error"])
    return [NSNumber numberWithInt:items[rowIndex].error];
  else
    return items[rowIndex].message;
}


- (void)addError:(int)error
          ofType:(MYX_QUERY_ERROR_LEVEL)level
         message:(NSString*)message
           atRow:(unsigned int)row
          column:(unsigned int)column
{
  itemCount++;
  items= (MessageItem*)g_realloc(items, sizeof(MessageItem)*itemCount);
  items[itemCount-1].error= error;
  items[itemCount-1].level= level;
  items[itemCount-1].message= [message retain];
  items[itemCount-1].row= row;
  items[itemCount-1].column= column;
}


- (void)tableView:(NSTableView *)aTableView
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualTo:@"error"])
  {
    switch (items[rowIndex].level)
    {
      case MYX_QEL_NOTE:
        [aCell setImage:images[0]];
        break;
      case MYX_QEL_WARNING:
        [aCell setImage:images[1]];
        break;
      case MYX_QEL_ERROR:
        [aCell setImage:images[2]];
        break;
    }
  }
}

- (void)reset
{
  int i;
  for (i= 0; i < itemCount; i++)
    [items[i].message release];
  g_free(items);
  itemCount= 0;
  items= NULL;
}

@end


//=============================================================================


@implementation MQResultSetView

- (void)updateButtonBar
{
  if (_dataSource)
  {
    if ([_dataSource editing])
    {
      [[view viewWithTag:50] setEnabled:NO];
      [[view viewWithTag:51] setEnabled:YES];
      [[view viewWithTag:52] setEnabled:YES];
    }
    else
    {
      if ([_dataSource isEditable])
        [[view viewWithTag:50] setEnabled:YES];
      else
        [[view viewWithTag:50] setEnabled:NO];
      [[view viewWithTag:51] setEnabled:NO];
      [[view viewWithTag:52] setEnabled:NO];
    }
    [[view viewWithTag:53] setEnabled:YES];
    [[view viewWithTag:54] setEnabled:YES];
    [[view viewWithTag:55] setEnabled:NO]; // SEARCH not supported yet
  }
  else
  {
    [[view viewWithTag:50] setEnabled:NO];
    [[view viewWithTag:51] setEnabled:NO];
    [[view viewWithTag:52] setEnabled:NO];
    
    [[view viewWithTag:53] setEnabled:NO];
    [[view viewWithTag:54] setEnabled:NO];
    [[view viewWithTag:55] setEnabled:NO];	
  }
}

- (void)setMessagesShown:(BOOL)flag
{
  if (flag)
    [splitView resizeSubview:[[messageTable superview] superview] toSize:70];
  else
    [splitView resizeSubview:[[messageTable superview] superview] toSize:0];
}

- (id)initWithConnectionTo:(MConnectionInfo*)info
{
  self= [super init];
  if (self)
  {
    int i;
    MYX_USER_CONNECTION *conn;
    
    _mysqlInfo= info;
    
    if (![NSBundle loadNibNamed:@"ResultSet" owner:self])
    {
      NSLog(@"Could not load nib ResultSet.nib");
      [self release];
      return nil;
    }
    
    _mysql= myx_mysql_init();
    if (!_mysql)
    {
      NSLog(@"could not create MySQL connection");
      [self release];
      return nil;
    }
    
    conn= [info createUserConnection];
    if (myx_connect_to_instance(conn, _mysql) < 0)
    {
      MXRunAlertPanelWithMySQLError(@"Error",@"Could not connect to MySQL instance.",_mysql);
      myx_free_user_connection_content(conn);
      g_free(conn);
      [self release];
      myx_mysql_close(_mysql);
      return nil;
    }
    myx_free_user_connection_content(conn);
    g_free(conn);
    
    // limit memory usage by resultsets to 50% of physical mem
    myx_mysql_limit_resultset_size(_mysql, (get_physical_memory_size()/(1024*1024LL))/2);
    
    _messageDS= [[MessageDS alloc] init];
    [messageTable setDataSource:_messageDS];
    [messageTable setDelegate:_messageDS];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(selectedError:) 
                                                 name:NSTableViewSelectionDidChangeNotification
                                               object:messageTable];
    
    [self updateButtonBar];
    
    _statusColors[MESUnchanged] = [[NSColor colorWithDeviceRed:0.92 green:0.92 blue:1.0 alpha:1.0] retain];
    _statusColors[MESChanged] = [[NSColor colorWithDeviceRed:0.8 green:0.8 blue:1.0 alpha:1.0] retain];
    _statusColors[MESPlaceHolder] = [[NSColor colorWithDeviceRed:0.95 green:1.0 blue:0.95 alpha:1.0] retain];
    _statusColors[MESAdded] = [[NSColor colorWithDeviceRed:0.8 green:1.0 blue:0.8 alpha:1.0] retain];
    _statusColors[MESDeleted] = [[NSColor colorWithDeviceRed:1.0 green:0.8 blue:0.8 alpha:1.0] retain];
    
    
    _diffColors[MCSMatch] = nil;
    _diffColors[MCSDiffers] = [[NSColor colorWithDeviceRed:0xaa/255.0
                                                     green:0xbb/255.0
                                                      blue:0xcc/255.0
                                                     alpha:1.0] retain];
    _diffColors[MCSOnlyInThis] = [[NSColor colorWithDeviceRed:0xaa/255.0
                                                        green:0xcc/255.0
                                                         blue:0xaa/255.0
                                                        alpha:1.0] retain]; 
    _diffColors[MCSOnlyInOther] = [[NSColor colorWithDeviceRed:0xcc/255.0
                                                         green:0xaa/255.0
                                                          blue:0xaa/255.0
                                                         alpha:1.0] retain];
    
    
    _viewers= [[NSMutableArray alloc] init];
    
    [splitView setInactiveColor:[table gridColor]];
    [splitView setDividerThickness:2.0];
    
    [self setMessagesShown:NO];
    [self changeSplitDirection:MQDNone];
    
    [table setIntercellSpacing:NSMakeSize(0,0)];
    
    for (i= 0; i < 7; i++)
      _editBarSizes[i]= [[view viewWithTag:editBarTags[i]] frame].size;
    
    //#ifdef MAC_OS_X_VERSION_10_4
    //    [table setColumnAutoresizingStyle:NSTableViewNoColumnAutoresizing];
    //#else
    //    [table setAutoresizesAllColumnsToFit:NO];
    //#endif
  }
  return self;
}


- (void)dealloc
{
  int i;
  for (i= 0; i < 5; i++)
    [_statusColors[i] release];
  for (i= 0; i < 4; i++)
    [_diffColors[i] release];
  [_query release];
  if (_mysql)
    myx_mysql_close(_mysql);
  [_dataSource release];
  
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  
  [_localParams release];
  [_viewers makeObjectsPerformSelector:@selector(close)];
  [_viewers release];
  
  [_messageDS release];
  
  [super dealloc];
}


- (void)changeSplitDirection:(MQRSSplitDirection)newDir
{
  NSButton *button;
  float size= [NSScroller scrollerWidth];
  
  [sview removeAccessoryViews];
  
  if (newDir == MQDNone || newDir == MQDHorizontal)
  {
    button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
    [button setButtonType: NSMomentaryChangeButton];
    [button setBordered:NO];
    [[button cell] setImagePosition:NSImageOnly];
    [button setImage: [NSImage imageNamed: @"sbar_splitv.png"]];
    
    [button setTarget:self];
    [button setAction:@selector(splitH:)];
    
    [sview addSubview:button];
    [sview addVerticalAccessoryView:button];
  }
  
  if (newDir == MQDNone || newDir == MQDVertical)
  {
    button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
    [button setButtonType: NSMomentaryChangeButton];
    [button setBordered:NO];
    [[button cell] setImagePosition:NSImageOnly];
    [button setImage: [NSImage imageNamed: @"sbar_splith.png"]];
    
    [button setTarget:self];
    [button setAction:@selector(splitV:)];
    
    [sview addSubview:button];
    [sview addHorizontalAccessoryView:button];
  }
  
  if (_closeable)
  {
    button= [[[NSButton alloc] initWithFrame:NSMakeRect(0,0,size,size)] autorelease];
    [button setButtonType: NSMomentaryChangeButton];
    [button setBordered:NO];
    [[button cell] setImagePosition:NSImageOnly];
    if (newDir == MQDHorizontal)
      [button setImage: [NSImage imageNamed: @"sbar_unsplitv.png"]];
    else
      [button setImage: [NSImage imageNamed: @"sbar_unsplith.png"]];
    
    [button setTarget:self];
    [button setAction:@selector(unsplit:)];
    
    [sview addSubview:button];
    if (newDir == MQDHorizontal)
      [sview addVerticalAccessoryView:button];
    else
      [sview addHorizontalAccessoryView:button];
  }
  
  if (newDir == MQDNone || newDir == MQDHorizontal)
    [self setCompact:NO];
  else
    [self setCompact:YES];
  
  _direction= newDir;
}

- (void)setCloseable:(BOOL)flag
{
  _closeable= flag;
  [self changeSplitDirection:_direction];
}

- (BOOL)closeable
{
  return _closeable;
}

- (void)splitV:(id)sender
{
  [_delegate performSelector:@selector(splitV:) withObject:self];
}

- (void)splitH:(id)sender
{
  [_delegate performSelector:@selector(splitH:) withObject:self];
}

- (void)unsplit:(id)sender
{
  [_delegate performSelector:@selector(unsplit:) withObject:self];
}

- (IBAction)addRow:(id)sender
{
  [_dataSource addRow];
  [table reloadData];
}

- (IBAction)deleteRow:(id)sender
{
  [_dataSource deleteRow:[table selectedRow]];
  [table reloadData];
}

- (IBAction)copyField:(id)sender
{
  unsigned int row= [table selectedRow];
  int column= [table lastClickedColumn]-1;
  MYX_RESULTSET *rs= [_dataSource resultset];
  NSPasteboard *pboard;
  
  if (column < 0)
    return;
  
  pboard= [NSPasteboard generalPasteboard];
  
  if (rs->columns[column].column_type == MYX_RSCT_BLOB)
  {
    NSData *data= [NSData dataWithBytes:rs->rows[row].fields[column].value
                                 length:rs->rows[row].fields[column].value_length];
    
    if ([NSImageRep imageRepClassForData:data])
    {
      NSImage *image= [[NSImage alloc] initWithData:data];
      
      [pboard declareTypes:[NSArray arrayWithObject:NSTIFFPboardType] owner:nil];
      
      [pboard setData:[image TIFFRepresentationUsingCompression:NSTIFFCompressionLZW
                                                         factor:1.0]
              forType:NSTIFFPboardType];
      [image release];
    }
  }
  else
  {
    [pboard declareTypes:[NSArray arrayWithObject:NSStringPboardType] owner:nil];
    
    [pboard setString:[[[NSString alloc] initWithCStringNoCopy:rs->rows[row].fields[column].value
                                                        length:rs->rows[row].fields[column].value_length
                                                  freeWhenDone:NO] autorelease]
              forType:NSStringPboardType];
  }
}

- (IBAction)saveField:(id)sender
{
  NSSavePanel *panel= [NSSavePanel savePanel];
  unsigned int row= [table selectedRow];
  int column= [table lastClickedColumn]-1;
  MYX_RESULTSET *rs= [_dataSource resultset];  
  NSData *data= [NSData dataWithBytes:rs->rows[row].fields[column].value
                               length:rs->rows[row].fields[column].value_length];
  
  [panel setTitle:@"Save Value to File"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    [data writeToFile:[panel filename] atomically:NO];
  }  
}

- (IBAction)loadField:(id)sender
{
  NSOpenPanel *panel= [NSOpenPanel openPanel];
  
  [panel setTitle:@"Load Value from File"];
  if ([panel runModal] == NSFileHandlingPanelOKButton)
  {
    id data;
    unsigned int row= [table selectedRow];
    int column= [table lastClickedColumn];
    MYX_RESULTSET *rs= [_dataSource resultset];  
    
    if (rs->columns[column-1].column_type == MYX_RSCT_BLOB)
      data= [NSData dataWithContentsOfFile:[panel filename]];
    else
      data= [NSString stringWithContentsOfFile:[panel filename]];
    
    [_dataSource tableView:table
            setObjectValue:data
            forTableColumn:[table tableColumnWithIdentifier:[NSNumber numberWithInt:column-1]]
                       row:row];
    
    [table setNeedsDisplay];
    
    [statusLabel setStringValue:@"Loaded file into field."];
  }
}

- (IBAction)clearField:(id)sender
{
  unsigned int row= [table selectedRow];
  int column= [table lastClickedColumn];
  
  [_dataSource tableView:table
          setObjectValue:nil
          forTableColumn:[table tableColumnWithIdentifier:[NSNumber numberWithInt:column-1]]
                     row:row];
  [table setNeedsDisplay];
}

- (void)editSelectedField:(BOOL)editable
{
  MQFieldViewer *viewer= [[MQFieldViewer alloc] init];
  unsigned int row= [table selectedRow];
  int column= [table lastClickedColumn]-1;
  MYX_RESULTSET *rs= [_dataSource resultset];
  NSData *data;
  
  if (column < 0)
    return;

  data= [NSData dataWithBytes:rs->rows[row].fields[column].value
                       length:rs->rows[row].fields[column].value_length];
  
  if (rs->columns[column].column_type == MYX_RSCT_BLOB)
    [viewer showData:data editable:editable];
  else    
    [viewer showTextData:data editable:editable];
  [viewer setInfo:[NSDictionary dictionaryWithObjectsAndKeys:
    [NSString stringWithUTF8String:rs->columns[column].name], @"columnName",
    [NSNumber numberWithInt:column], @"column",
    [NSNumber numberWithUnsignedInt:row], @"row",
    nil]];
  [viewer setDelegate:self];
  [viewer showWindow:nil];
  
  [_viewers addObject:viewer];
  [viewer release];  
}

- (IBAction)editField:(id)sender
{
  [self editSelectedField:YES];
}

- (IBAction)viewField:(id)sender
{
  [self editSelectedField:NO];
}

- (IBAction)editRS:(id)sender
{
  [_dataSource setEditing:YES];
  [self updateButtonBar];
  [statusLabel setStringValue:@"Editing enabled."];
  [table reloadData];
}

- (IBAction)commitRS:(id)sender
{
  [_dataSource setEditing:NO];
  [self updateButtonBar];
  
  [statusLabel setStringValue:@"Saving changes..."];
  
  [_dataSource preCommit];
  
  [self performApply];
}

- (IBAction)discardRS:(id)sender
{
  [_dataSource setEditing:NO];
  [self updateButtonBar];
  [_dataSource discardChanges];
  [table reloadData];
  [statusLabel setStringValue:@"Discarded changes."];
}

- (IBAction)goFirst:(id)sender
{
  if ([_dataSource numberOfRowsInTableView:table] > 0)
  {
    [table selectRow:0 byExtendingSelection:NO];
    [table scrollRowToVisible:0];
  }
}

- (IBAction)goLast:(id)sender
{
  if ([_dataSource numberOfRowsInTableView:table] > 0)
  {
    [table selectRow:[_dataSource numberOfRowsInTableView:table]-1
byExtendingSelection:NO];
    [table scrollRowToVisible:[_dataSource numberOfRowsInTableView:table]-1];
  }
}


- (IBAction)copyMessage:(id)sender
{
  int row= [messageTable selectedRow];
  if (row >= 0)
  {
    NSString *msg= [_messageDS tableView:messageTable
               objectValueForTableColumn:nil
                                     row:row];
    
    NSPasteboard *pasteboard= [NSPasteboard generalPasteboard];
    [pasteboard declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                       owner:nil];
    [pasteboard setString:msg forType:NSStringPboardType];    
  }
}


- (IBAction)clearMessages:(id)sender
{
  [_messageDS reset];
  [messageTable reloadData];
  [self setMessagesShown:NO];
}


- (MYSQL*)mysql
{
  return _mysql;
}

- (NSString*)defaultCatalog
{
  return @"def";
}

- (void)setDefaultSchema:(NSString*)schema
{
  if (_defaultSchema != schema)
  {
    [_defaultSchema release];
    _defaultSchema= [schema retain];
  }
  if (schema)
    myx_use_schema(_mysql, [schema UTF8String]);
}

- (NSString*)defaultSchema
{
  return _defaultSchema;
}

- (id)view
{
  return view;
}

- (MQTableView*)tableView
{
  return table;
}

- (void)setActive:(BOOL)flag
{
  _active= flag;
  if (flag)
    [table setBackgroundColor:[NSColor whiteColor]];
  else
    [table setBackgroundColor:[table gridColor]];
  [splitView setActive:flag];
  [splitView setNeedsDisplay:YES];
  
  if (flag)
    [_delegate performSelector:@selector(resultSetViewDidGetFocus:) withObject:self];
}


- (BOOL)active
{
  return _active;
}


- (void)setDelegate:(id)delegate
{
  _delegate= delegate;
}


- (void)notifyFinished:(NSDictionary*)info
{
  MYX_RESULTSET *rs= (MYX_RESULTSET*)[[info objectForKey:@"resultset"] pointerValue];
  MYX_LIB_ERROR error= (MYX_LIB_ERROR)[[info objectForKey:@"error"] intValue];
  
  [table reloadData];
  
  if (error == MYX_STOP_EXECUTION)
    [statusLabel setStringValue:[NSString stringWithFormat:@"Interrupted. %i rows fetched.", 
      rs->rows_num]];
  else if (error == MYX_MEMORY_LIMIT_EXCEEDED)
  {
    [statusLabel setStringValue:[NSString stringWithFormat:@"Memory usage limit exceeded. %i rows fetched", 
      rs->rows_num]];
    NSRunAlertPanel(@"Memory Limit Exceeded",
                    [NSString stringWithFormat:@"The query resultset has exceeded the memory size limit of %@B and was interrupted. It is advisable to use the LIMIT keyword in SELECT queries on large tables.",
                      [NSString stringWithMultNumber:myx_mysql_get_resultset_size_limit(_mysql)*1024*1024LL]],
                    @"OK", nil, nil);
  }
  else
    [statusLabel setStringValue:[NSString stringWithFormat:@"%i rows fetched.", 
      rs->rows_num]];
  
  [_delegate performSelector:@selector(resultSetViewFinishedQuery:error:)
                  withObject:self
                  withObject:(id)NO];
  
  if (myx_check_whether_commits_transaction(_mysql, [[info objectForKey:@"query"] UTF8String]))
  {
    _transactionOpen= NO;
    [_delegate performSelector:@selector(resultSetViewTransactionEnded:commited:)
                    withObject:self
                    withObject:(id)NO];
  }
  
  [self updateButtonBar];  
}


- (void)displayResults:(NSDictionary*)info
{
  MYX_RESULTSET *rs= (MYX_RESULTSET*)[[info valueForKey:@"resultset"] pointerValue];
  MYX_LIB_ERROR error= (MYX_LIB_ERROR)[[info valueForKey:@"error"] intValue];
  unsigned int i;
  NSArray *columns= [table tableColumns];
  NSTableColumn *column;
  NSFont *font;
  
  _busy= NO;
  
  if (!rs)
  {
    MYX_MYSQL_ERROR_MSGS *errors;
    errors= myx_mysql_error_msgs_fetch(_mysql);	
    
    if (error == MYX_NO_ERROR && !errors)
    {
      [statusLabel setStringValue:[NSString stringWithFormat:@"Empty resultset."]];
      
      [table setDataSource:nil];
      [_dataSource release];
      _dataSource= nil;
    }
    else
    {
      unsigned int i;
      NSTableColumn *column= [[messageTable tableColumns] objectAtIndex:1];
      
      [statusLabel setStringValue:[NSString stringWithFormat:@"Error executing query."]];
      if (errors)
      {
        for (i= 0; i < errors->errors_num; i++)
        {
          NSString *message= [NSString stringWithUTF8String:errors->errors[i].text];
          [_messageDS addError:errors->errors[i].error
                        ofType:errors->errors[i].level
                       message:message
                         atRow:(unsigned int)-1
                        column:(unsigned int)-1];
          MXResizeTableColumnToFitValue(messageTable, column, message);
        }
        myx_mysql_error_msgs_free(errors);
      }
      else
      {
        NSString *message= MXGetErrorString(error);
        [_messageDS addError:-1 
                      ofType:MYX_QEL_ERROR
                     message:message
                       atRow:(unsigned int)-1
                      column:(unsigned int)-1];
        MXResizeTableColumnToFitValue(messageTable, column, message);
      }
      [messageTable reloadData];
      [self setMessagesShown:YES];      
    }
    [_delegate performSelector:@selector(resultSetViewFinishedQuery:error:) withObject:self withObject:(id)YES];
    return;
  }
  MQResultSetDataSource *oldDaataSource= _dataSource;
  
  _dataSource= [[MQResultSetDataSource alloc] initWithResultSet:rs lock: resultset_lock];
  [table setDataSource: _dataSource];
  [table reloadData];
  
  [oldDaataSource release];
  
  while ([columns count] > 0)
    [table removeTableColumn:[columns objectAtIndex:0]];
  
  column=  [[NSTableColumn alloc] initWithIdentifier:@"indicator"];
  [column setDataCell:[[[MQIndicatorCell alloc] init] autorelease]];
  [column setWidth:30];
  [column setMinWidth:30];
  [column setMaxWidth:30];
  [column setEditable:NO];
  [[column headerCell] setTitle:@""];
  [table addTableColumn:column];
  
  font= [NSFont systemFontOfSize:11];
  
  for (i= 0; i < rs->columns_num; i++)
  {
    column= [[NSTableColumn alloc] initWithIdentifier:[NSNumber numberWithInt:i]];
    NSString *title= [NSString stringWithUTF8String:rs->columns[i].name];
    float width= 0.0;
    float twidth;
    unsigned int j;
    
    [column setDataCell:[[[MQResultSetCell alloc] init] autorelease]];
    [[column dataCell] setEditable:YES];
    
    switch (rs->columns[i].column_type)
    {
      case MYX_RSCT_DECIMAL:
      case MYX_RSCT_INTEGER:
        width= [font widthOfString:@"0000000000"];
        break;
      case MYX_RSCT_FLOAT:
        width= [font widthOfString:@"0000000000"];
        break;
      case MYX_RSCT_STRING:
        {
          int jidx= 0;
          int len= 0;
          
          [resultset_lock lock];
          for (j= 0; j < rs->rows_num; j++)
          {
            int l= strlen(rs->rows[j].fields[i].value ?: "");
            if(len < l)
            {
              len= l;
              jidx= j;
            }
          }
          if (rs->rows_num > 0)
            width= [font widthOfString:[NSString stringWithUTF8String:rs->rows[jidx].fields[i].value?:""]];
          else
            width= [font widthOfString: @""];
          [resultset_lock unlock];
        }

        /*for (j= 0; j < rs->rows_num; j++)
        {
          float w= [font widthOfString:[NSString stringWithUTF8String:rs->rows[j].fields[i].value?:""]];
          if (width < w)
            width= w;
        }*/
          if (width == 0.0)
          {
            width= [font widthOfString:@"XYZW"] * rs->columns[i].type_size / 4;
          }
          else
            width+= 4.0;
        if (width < 40.0)
          width= 40.0;
          break;
      case MYX_RSCT_DATE:
        width= [font widthOfString:@"00/00/0000"];
        break;
      case MYX_RSCT_TIME:
        width= [font widthOfString:@"00:00:00.0000"];
        break;
      case MYX_RSCT_DATETIME:
        width= [font widthOfString:@"00/00/0000  00:00:00.0000"];
        break;
      case MYX_RSCT_BLOB:
        [[column dataCell] setIsBlob:YES];
        width= 50;
        break;
      case MYX_RSCT_TEXT:
        width= 250;
        break;
      case MYX_RSCT_ENUM:
        width= 100;
        break;
      case MYX_RSCT_SET:
        width= 100;
        break;
    }
    
    [[column dataCell] setDrawsBackground:YES];
    [[column dataCell] setFont: font];
    
    twidth= [[[column headerCell] font] widthOfString:title];
    width= MIN(MAX(width, twidth), 250);
    [[column headerCell] setWraps:NO];
    
    if (rs->columns[i].table_column && rs->columns[i].table_column->is_pk)
      [[column headerCell] setStringValue:[NSString stringWithFormat:@"%@%@",
        [NSString stringWithUTF8String:"\xe2\x80\xbb"], title]];
    else
      [[column headerCell] setStringValue:title];
    
    [table addTableColumn:column];
    
    if (rs->rows_num > 0)
      [column setWidth:width];
  }
  
  [table setDataSource:_dataSource];
  
  if (![info objectForKey:@"current"]) // if we're not being called from the progress callback
    [self notifyFinished:info];
  
  _resultDisplayed= YES;
}


- (void)queryProgress:(NSDictionary*)info
{
  [statusLabel setStringValue:[NSString stringWithFormat:@"%@ rows fetched",
    [info objectForKey:@"current"]]];
  
  if ([[info objectForKey:@"previous"] intValue] == 0)
    [self displayResults:info];
  else
    [table noteNumberOfRowsChanged];
}

static int fetchProgress(unsigned long current_row_count, 
                         unsigned long previous_row_count, 
                         MYX_RESULTSET *result_set, void *user_data)
{
  NSDictionary *dict;
  
  dict= [NSDictionary dictionaryWithObjectsAndKeys:
    [NSNumber numberWithLong:current_row_count], @"current",
    [NSNumber numberWithLong:previous_row_count], @"previous",
    [NSValue valueWithPointer:result_set], @"resultset",
    nil];
  [(id)user_data performSelectorOnMainThread:@selector(queryProgress:)
                                  withObject:dict
                               waitUntilDone:NO];
  return ((MQResultSetView*)user_data)->_stopQuery; 
}

- (void)queryFinished:(NSDictionary*)info
{
  // we're being called from a performSelectorOnMainThread, so
  // set up to display the results later, when we're already running normally
  
  // if the resultset has not been displayed yet (by the progress callback),
  // display it, else just call the final stuff
  if (!_resultDisplayed)
    [[NSRunLoop currentRunLoop] performSelector:@selector(displayResults:)
                                         target:self
                                       argument:info
                                          order:100
                                          modes:[NSArray arrayWithObject:NSDefaultRunLoopMode]];
  else
    [[NSRunLoop currentRunLoop] performSelector:@selector(notifyFinished:)
                                         target:self
                                       argument:info
                                          order:100
                                          modes:[NSArray arrayWithObject:NSDefaultRunLoopMode]];
}

- (void)stopQuery
{
  _stopQuery= YES;
}

- (void)doQuery:(NSDictionary*)args
{
  MYSQL *mysql= (MYSQL*)[[args objectForKey:@"mysql"] pointerValue];
  NSString *query= [args objectForKey:@"query"];
  MYX_STRINGLIST *params= (MYX_STRINGLIST*)[[args objectForKey:@"params"] pointerValue];
  MYX_LIB_ERROR error_code;
  MYX_RESULTSET *result;
  NSDictionary *info;
  long long int affected_rows;
  
  mysql_thread_init();
  
  if(resultset_lock == nil)
    resultset_lock= [[NSLock alloc] init];
  
  result= myx_query_execute(mysql, [query UTF8String],
                            TRUE,
                            params, &error_code, self,
                            fetchProgress,
                            resultset_realloc_before,
                            resultset_realloc_after,
                            &affected_rows);  
  if (params)
    myx_free_stringlist(params);

  info= [NSDictionary dictionaryWithObjectsAndKeys:
    [NSValue valueWithPointer:result], @"resultset",
    [NSNumber numberWithInt:error_code], @"error", 
    query, @"query", nil];
  
  [self performSelectorOnMainThread:@selector(queryFinished:)
                         withObject:info
                      waitUntilDone:YES];

  mysql_thread_end();
}


- (void)performQuery:(NSString*)query
{
  [_messageDS reset];
  [messageTable reloadData];
  [self setMessagesShown:NO];
  
  [self performQuery:query withParameters:NULL];
}


- (void)performQuery:(NSString*)query withParameters:(MYX_STRINGLIST*)parameters
{
  if (mysql_ping(_mysql) != 0)
  {
    if (mysql_errno(_mysql) == CR_SERVER_GONE_ERROR || mysql_errno(_mysql) == CR_SERVER_LOST)
    {
      if ([_mysqlInfo reconnect:_mysql])
      {
        if (!_stopQuery)
        {
          if (NSRunAlertPanel(@"Warning", @"Connection to MySQL server was lost but successfully restablished.",
                              @"Reissue Query", @"Cancel", nil) != NSAlertDefaultReturn)
            return;
        }
      }
    }
  }
  
  [self setQuery:query];
  
  [statusLabel setStringValue:@"Performing query..."];
  
  [_delegate performSelector:@selector(resultSetView:willStartQuery:)
                  withObject:self
                  withObject:query];
  
  _resultDisplayed= NO;
  _stopQuery= NO;
  [NSApplication detachDrawingThread:@selector(doQuery:)
                            toTarget:self
                          withObject:[NSDictionary dictionaryWithObjectsAndKeys:
                            [NSValue valueWithPointer:_mysql], @"mysql",
                            [NSValue valueWithPointer:parameters], @"params",
                            query, @"query",
                            nil]];
  _busy= YES;
}


- (void)applyFinished:(id)errorsVal
{
  MYX_RS_ACTION_ERRORS *errors= (MYX_RS_ACTION_ERRORS*)[errorsVal pointerValue];
  
  if (errors)
  {
    unsigned int i;
    int errcount= 0;
    [_messageDS reset];
    for (i= 0; i < errors->errors_num; i++)
    {
      if (errors->errors[i].level == MYX_QEL_ERROR)
        errcount++;
      [_messageDS addError:errors->errors[i].error
                    ofType:errors->errors[i].level
                   message:[NSString stringWithUTF8String:errors->errors[i].error_text]
                     atRow:errors->errors[i].action->row
                    column:[_dataSource resultset]->columns - errors->errors[i].action->column];
    }
    myx_query_free_action_errors(errors);
    
    [_dataSource postCommit:YES];
    [messageTable noteNumberOfRowsChanged];
    [self setMessagesShown:YES];
    
    if (errcount > 0)
      [statusLabel setStringValue:@"Error while applying actions."];
    else
      [statusLabel setStringValue:@"Warnings while applying actions."];
    
    [_dataSource setEditing:YES];
    [self updateButtonBar];
  }
  else
  {
    [_dataSource postCommit:NO];
    [_messageDS reset];
    [messageTable reloadData];
    [statusLabel setStringValue:@"Applied actions."];    
  }
  [table reloadData];
  
  _busy= NO;
}


- (void)doApply:(NSDictionary*)args
{
  MYX_RESULTSET *rset= (MYX_RESULTSET*)[[args objectForKey:@"resultset"] pointerValue];
  MYX_RS_ACTION_ERRORS *errors;
  
  if (rset->actions)
  {
    mysql_thread_init();
    
    errors= myx_query_apply_actions(rset);
    
    [self performSelectorOnMainThread:@selector(applyFinished:)
                           withObject:[NSValue valueWithPointer:errors]
                        waitUntilDone:YES];
    
    mysql_thread_end();
  }
}


- (void)setCompact:(BOOL)flag
{
  if (flag != _compact)
  {
    NSRect rect;
    int i;
    id part;
    float x= 5.0;
    float width= NSWidth([view frame]);
    
    x= width;
    for (i= 6; i >= 1; i--)
    {
      part= [view viewWithTag:editBarTags[i]];
      rect= [part frame];
      if (flag)
        rect.size.width= 25;
      else
        rect.size.width= _editBarSizes[i].width;
      x-= rect.size.width;
      
      if (i == 3)
        x-= 2;
      rect.origin.x= x;
      [part setFrame:rect];
    }
    part= [view viewWithTag:editBarTags[0]];
    rect= [part frame];
    rect.size.width= width-5.0-x;
    [part setFrame:rect];
    _compact= flag;
  }
}


- (void)performApply
{
  if ([_dataSource resultset]->actions)
  {
    [statusLabel setStringValue:@"Applying actions..."];
    
    //  [_delegate resultSetView:self willStartQuery:query];
    
    [NSApplication detachDrawingThread:@selector(doApply:)
                              toTarget:self
                            withObject:[NSDictionary dictionaryWithObjectsAndKeys:
                              [NSValue valueWithPointer:_mysql], @"mysql",
                              [NSValue valueWithPointer:[_dataSource resultset]], @"resultset",
                              nil]];
    _busy= YES;
  }
}



- (BOOL)transactionOpen
{
  return _transactionOpen;
}


- (void)startTransaction
{
  if (myx_mysql_query(_mysql, "START TRANSACTION") == 0)
  {
    _transactionOpen= YES;
    [_delegate performSelector:@selector(resultSetViewTransactionStarted:)
                    withObject:self];
  }
  else
    MXRunAlertPanelWithMySQLError(@"Error", @"Error starting transaction.", _mysql);
}


- (void)commitTransaction
{
  if (myx_mysql_query(_mysql, "COMMIT") == 0)
  {
    _transactionOpen= NO;
    [_delegate performSelector:@selector(resultSetViewTransactionEnded:commited:)
                    withObject:self
                    withObject:(id)YES];
  }
  else
    MXRunAlertPanelWithMySQLError(@"Error", @"Error commiting transaction.", _mysql);
}


- (void)rollbackTransaction
{
  if (myx_mysql_query(_mysql, "ROLLBACK")== 0)
  {
    _transactionOpen= NO;
    [_delegate performSelector:@selector(resultSetViewTransactionEnded:commited:)
                    withObject:self
                    withObject:(id)NO];
  }
  else
    MXRunAlertPanelWithMySQLError(@"Error", @"Error rolling back transaction.", _mysql);
}


- (MYX_RESULTSET*)resultset
{
  return [_dataSource resultset];
}


- (void)setMasterView:(MQResultSetView*)rsview
{
  _masterView= rsview;
}


- (MQResultSetView*)masterView
{
  return _masterView;
}


- (void)setLocalParameters:(id)params
{
  if (_localParams != params)
  {
    [_localParams release];
    _localParams= [params retain];
  }
}


- (id)localParameters
{
  return _localParams;
}


- (BOOL)isBusy
{
  return _busy;
}

- (void)setQuery:(NSString*)query
{
  if (_query != query)
  {
    [_query release];
    if ([query isMemberOfClass:[NSMutableString class]])
      _query= [[NSString stringWithString:query] retain];
    else
      _query= [query retain];
  }
}

- (NSString*)query
{
  return _query ? : @"";
}


- (BOOL)tableView:(NSTableView *)aTableView 
shouldEditTableColumn:(NSTableColumn *)aTableColumn 
              row:(int)rowIndex
{
  int column= [[aTableColumn identifier] intValue];
  if ([_dataSource editing] && [_dataSource statusOfRow:rowIndex
                                                 column:column] != MESDeleted
      && [_dataSource resultset]->columns[column].column_type!=MYX_RSCT_BLOB)
    return YES;
  else
    return NO;
}

- (int)numberOfItemsInMenu:(NSMenu *)menu
{
  return [menu numberOfItems];
}

- (BOOL)menu:(NSMenu *)menu
  updateItem:(NSMenuItem *)item
     atIndex:(int)index
shouldCancel:(BOOL)shouldCancel
{
  BOOL flag= NO;
  BOOL hasSelection= [table selectedRow]>=0;
  
  switch (index)
  {
    case 0: //add
      flag= [_dataSource editing];
      break;
    case 1: //delete
      flag= [_dataSource editing] && hasSelection;
      break;
    case 3: //load
      flag= [_dataSource editing] && hasSelection;
      break;
    case 4: //save
      flag= hasSelection;
      break;
    case 5: //copy
      flag= hasSelection;
      break;
    case 6: //clear
      flag= [_dataSource editing] && hasSelection;
      break;	  
    case 8: //view
      flag= hasSelection;
      break;
    case 9: //edit
      flag= [_dataSource editing] && hasSelection;
      break;
  }
  [item setEnabled:flag];
  [menu itemChanged:item];
  return YES;
}


- (void)tableViewWasClicked:(NSTableView *)aTableView
{
  if (!_active)
    [_delegate performSelector:@selector(resultSetViewWantsFocus:) withObject:self];
}


- (void)tableView:(NSTableView *)aTableView 
  willDisplayCell:(id)aCell
   forTableColumn:(NSTableColumn *)aTableColumn
              row:(int)rowIndex
{
  if ([[aTableColumn identifier] isEqualTo:@"indicator"])
  {
    if ([aTableView selectedRow] == rowIndex)
      [aCell setSelected:YES];
    else
      [aCell setSelected:NO];
    
    if ([_dataSource statusOfRow:rowIndex column:0] == MESPlaceHolder)
      [aCell setPlaceholder:YES];
    else
      [aCell setPlaceholder:NO];
  }
  else if ([_dataSource editing])
  {
    int column= [[aTableColumn identifier] intValue];
    MYXRSCompareStatus diff;
    
    [aCell setPlaceholder:NO];
    [aCell setIsNull:[_dataSource myxResultSet]->isNull(rowIndex, column)];
    
    if (rowIndex == [aTableView selectedRow])
    {
      if (column == [(MQTableView*)aTableView lastClickedColumn]-1)
        [aCell setBackgroundColor:_active ? [NSColor alternateSelectedControlColor] : [NSColor grayColor]];
      else
        [aCell setBackgroundColor:[NSColor lightGrayColor]];
      [aCell setTextColor:[NSColor alternateSelectedControlTextColor]];
      return;
    }
    
    [aCell setTextColor:[NSColor blackColor]];
    
    if (rowIndex >= (int)[_dataSource resultset]->rows_num ||
        (diff=[_dataSource compareStatusOfRow:rowIndex column:column])==MCSMatch)
    {    
      switch ([_dataSource statusOfRow:rowIndex column:column])
      {
        case MESUnchanged:
          if (rowIndex & 1)
            [aCell setBackgroundColor:_statusColors[MESUnchanged]];
          else
            [aCell setBackgroundColor:[NSColor whiteColor]];
          break;
        case MESChanged:
          [aCell setBackgroundColor:_statusColors[MESChanged]];
          break;
        case MESPlaceHolder:
          [aCell setBackgroundColor:_statusColors[MESPlaceHolder]];
          [aCell setPlaceholder:YES];
          break;
        case MESAdded:
          [aCell setBackgroundColor:_statusColors[MESAdded]];
          break;
        case MESDeleted:
          [aCell setBackgroundColor:_statusColors[MESDeleted]];
          break;
      }
    }
    else
      [aCell setBackgroundColor:_diffColors[diff]];
  }
  else
  {
    int column= [[aTableColumn identifier] intValue];
    MYXRSCompareStatus diff;

    [aCell setPlaceholder:NO];
    [aCell setIsNull:[_dataSource myxResultSet]->isNull(rowIndex, column)];
    if ((diff=[_dataSource compareStatusOfRow:rowIndex column:column])==MCSMatch)
    {
      if (rowIndex == [aTableView selectedRow])
      {
        if (column == [(MQTableView*)aTableView lastClickedColumn]-1)
          [aCell setBackgroundColor:_active ? [NSColor alternateSelectedControlColor] : [NSColor grayColor]];
        else
          [aCell setBackgroundColor:[NSColor lightGrayColor]];
        [aCell setTextColor:[NSColor alternateSelectedControlTextColor]];
      }
      else
      {
        [aCell setTextColor:[NSColor blackColor]];
        if (rowIndex & 1)
          [aCell setBackgroundColor:_statusColors[MESUnchanged]];
        else
          [aCell setBackgroundColor:[NSColor whiteColor]];
      }
    }
    else
      [aCell setBackgroundColor:_diffColors[diff]];
  }
}


- (void)splitView:(NSSplitView *)sender resizeSubviewsWithOldSize:(NSSize)oldSize
{
  NSArray *children= [sender subviews];
  NSView *top= [children objectAtIndex:0];
  NSView *bottom= [children lastObject];
  NSRect rect;
  
  rect= [sender frame];
  rect.size.height-= [sender dividerThickness] + NSHeight([bottom frame]);  
  [top setFrame:rect];
  
  rect.origin.y= rect.size.height + [sender dividerThickness];
  rect.size.height= NSHeight([bottom frame]);
  [bottom setFrame:rect];
}

- (float)splitView:(NSSplitView *)sender
 constrainMinCoordinate:(float)proposedMin
       ofSubviewAt:(int)offset
{
  return NSHeight([sender frame])-80;
}


- (void)fieldViewerDidClose:(MQFieldViewer*)viewer
{
  [_viewers removeObject:viewer];
}


- (void)fieldViewer:(MQFieldViewer*)viewer saveData:(NSData*)data
{
  NSDictionary *info= [viewer info];
  
  [_dataSource tableView:table
          setObjectValue:data
          forTableColumn:[table tableColumnWithIdentifier:[info objectForKey:@"column"]]
                     row:[[info objectForKey:@"row"] unsignedIntValue]];
  
  [table setNeedsDisplay];
}


- (void)selectedError:(NSNotification*)notif
{
  int row= [messageTable selectedRow];
  MessageItem *item= ((MessageDS*)_messageDS)->items + row;
  
  if (row >= 0 && item->row != (unsigned int)-1)
  {
    [table selectCellAtRow:item->row
                    column:item->column];
  }
}

- (BOOL)hasChanges
{
  return [_dataSource hasChanges];
}

- (NSString *) querySaveFilename
{
  return _query_save_filename;
}

- (void) setQuerySaveFilename: (NSString *)filename
{
  _query_save_filename= filename;
}

- (void) saveQuery
{
  NSString *path= _query_save_filename;

  if (!path)
  {
    NSSavePanel *panel= [NSSavePanel savePanel];
    
    [panel setTitle:@"Save SQL Query"];
    if ([panel runModal] == NSFileHandlingPanelOKButton)
    {
      path= [panel filename];
    }
    else
      path= nil;
  }

  if (path)
  {
    FILE *file= fopen([path fileSystemRepresentation], "w");
    if (file)
    {
      const char *utfData= [_query UTF8String];
      fwrite(utfData, 1, strlen(utfData), file);
      fclose(file);
      
      if (path != _query_save_filename)
      {
        [_query_save_filename release];
        _query_save_filename= [path retain];
      }
    }
    else
    {
      NSRunAlertPanel(@"Error",[NSString stringWithFormat:@"Could not write to file: %s",strerror(errno)],
                      @"OK", nil, nil);
    }
  }
}

@end

