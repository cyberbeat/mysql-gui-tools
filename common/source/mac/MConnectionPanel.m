#import "MConnectionPanel.h"
#import "MDialogs.h"
#import "MPreferences.h"
#include "myxutil.h"
#import "MPreferenceEditor.h"

#include <mysqld_error.h>

@interface MConnectionPanel(Private)
- (void)setEnabled: (BOOL)flag;
- (void)controlTextDidChange:(NSNotification *)aNotification;

- (void)showConnectionError: (MYSQL*)mysql
                       info: (MYX_USER_CONNECTION*)info;


- (void)connectionFinished: (id)result;

- (void)checkConnectOK;
- (MYX_USER_CONNECTION*)getDisplayedConnection;
- (void)displayConnection:(MYX_USER_CONNECTION*)info;
- (void)fillConnectionPopUp:(BOOL)selectLast;
- (void)startConnection: (MYX_USER_CONNECTION*)connectionInfo;

- (void)showConnectionEditor:(id)sender;
- (void)saveConnection:(id)sender;

- (void)finish:(id)arg;
@end


@implementation MConnectionPanel(Private)

- (void)finish:(id)arg
{
  int rc;
  MConnectionInfo *info= nil;

  if (_mysql && myx_mysql_errno(_mysql)==0)
  {
    MYX_USER_CONNECTION *conn= [self getDisplayedConnection];
    
    info= [MConnectionInfo connectionInfoFromUserConnection:conn];
    rc= 1;
    
    // save
    if (conn && !conn->connection_name)
    {
      unsigned int i;
      // look for connection with same contents
      for (i= 0; i < _connections->user_connections_num; i++)
      {
        if (!_connections->user_connections[i].connection_name)
        {
          if (myx_same_user_connections(conn, _connections->user_connections+i))
          {
            _connections->last_connection= i;
            break;
          }
        }
      }
      if (i == _connections->user_connections_num)
        _connections->last_connection= myx_add_user_connection(&_connections, conn);
    }
    else
    {
      _connections->last_connection= myx_add_user_connection(&_connections, conn);
    }
    //XXX check return value
    myx_store_user_connections(_connections,[MPreferences preferences]->passwordStorageType,
                               [_connectionsFile fileSystemRepresentation]);
  }
  else if (_mysql)
  {
    NSLog(@"connected with errno %i", myx_mysql_errno(_mysql));
    myx_mysql_close(_mysql);
    _mysql= NULL;
    rc= -1;
  }
  else
    rc= 0;

  [[self window] close];

  if (rc > 0)
    [_delegate connectionPanel:self finished:_mysql info:info];
  else
    [_delegate connectionPanel:self aborted:(rc<0?YES:NO)];
}


- (void)setEnabled: (BOOL)flag
{
  [connectButton setEnabled: flag];
  [detailsButton setEnabled: flag];
  
  [hostname setEnabled: flag];
  [password setEnabled: flag];
  [username setEnabled: flag];
  [port setEnabled: flag];
  [portSpin setEnabled: flag];
}


- (void)flagsChanged:(NSEvent *)theEvent
{
  if (_allowSkip)
  {
    if ([theEvent modifierFlags] & NSCommandKeyMask)
    {
      _skip= YES;
      [connectButton setTitle:NSLocalizedString(@"Skip",nil)];
    }
    else
    {
      _skip= NO;
      [connectButton setTitle:NSLocalizedString(@"Connect",nil)];
    }
  }
  [super flagsChanged:theEvent];
}


- (void)connectionFinished: (id)result
{  
  int err;

  _connecting= NO;

  [progressIndicator stopAnimation: self];
  [self setEnabled: YES];
  
  if (_mysql)
    myx_mysql_close(_mysql);
  _mysql= [[result valueForKey:@"mysql"] pointerValue];
  
  if (!_mysql)
  {
    NSRunAlertPanel(@"Error",
                    @"Could not initialize MySQL connection.",
                    @"OK", nil, nil);
  }
  else if ((err= myx_mysql_errno(_mysql))!=0)
  {
    if (myx_get_mysql_major_version(_mysql) < 4 && myx_get_mysql_major_version(_mysql) > 1)
      NSRunAlertPanel(@"Error",
                      @"Sorry. MySQL server version 4.0 or newer is required, but you are connecting an older, unsupported version.",
                      @"OK", nil, nil);
    else
      [self showConnectionError: _mysql
                           info: [[result valueForKey:@"info"] pointerValue]];
    if (_mysql)
      myx_mysql_close(_mysql);
    _mysql= NULL;
    
    switch (err)
    {
      case ER_ACCESS_DENIED_ERROR:
        [[self window] makeFirstResponder:password];
        break;
    }
  }
  else
  {
    [[NSRunLoop currentRunLoop] performSelector:@selector(finish:)
                                         target:self
                                       argument:nil
                                          order:1000
                                          modes:[NSArray arrayWithObject:NSDefaultRunLoopMode]];
  }
}


- (void)connectThread: (id)info
{
  MYSQL *mysql;
  BOOL ok= YES;
  MYX_USER_CONNECTION *conn;
  char *saveSchema= NULL;
  
  mysql_thread_init();
  
  mysql= [[info objectForKey:@"mysql"] pointerValue];
  conn= [[info objectForKey:@"info"] pointerValue];
  
  if (!_pickSchema)
  {
    saveSchema= conn->schema;
    conn->schema= NULL;
  }
  
  if (myx_connect_to_instance(conn, mysql) < 0)
    ok= NO;

  if (!_pickSchema)
    conn->schema= saveSchema;
  
  [self performSelectorOnMainThread:@selector(connectionFinished:)
                          withObject:info
                       waitUntilDone:YES];
  
  mysql_thread_end();
}


- (void)startConnection: (MYX_USER_CONNECTION*)connectionInfo
{
  if (!_connecting)
  {
    [NSThread detachNewThreadSelector: @selector(connectThread:)
                             toTarget: self
                           withObject: [NSDictionary dictionaryWithObjectsAndKeys:
                             [NSValue valueWithPointer:connectionInfo], @"info",
                             [NSValue valueWithPointer:myx_mysql_init()], @"mysql",
                             nil]];
  }
}


- (MYX_USER_CONNECTION*)getDisplayedConnection
{
  id item= [connectionPopUp selectedItem];

  if (item && [item representedObject])
  {
    MYX_USER_CONNECTION *conn= [[item representedObject] pointerValue];

    if (conn)
    {
      g_free(conn->password);
      conn->password= g_strdup([[password stringValue] UTF8String]);
    }
    return conn;
  }
  else
  { 
    unsigned int i;
    
    g_free(_new_connection.username);
    _new_connection.username= g_strdup([[username stringValue] UTF8String]);
    g_free(_new_connection.password);
    _new_connection.password= g_strdup([[password stringValue] UTF8String]);
    g_free(_new_connection.hostname);
    _new_connection.hostname= g_strdup([[hostname stringValue] UTF8String]);
    _new_connection.port= [port intValue];

    _new_connection.schema= g_strdup([[schema stringValue] UTF8String]);
	
    _new_connection.storage_type= MYX_HISTORY_USER_CONNECTION;

    for (i= 0; i < _new_connection.advanced_options_num; i++)
      g_free(_new_connection.advanced_options[i]);
    
    if (_new_connection.advanced_options)
      g_free(_new_connection.advanced_options);
    
    _new_connection.advanced_options= g_new0(char*, 3);
    
    i= 0;
    if ([[socketText stringValue] length] > 0)
      _new_connection.advanced_options[i++]= g_strdup_printf("SOCKET_PATH=%s", 
                                                             [[[socketText stringValue] stringByExpandingTildeInPath] UTF8String]);
    if ([compress state] == NSOnState)
      _new_connection.advanced_options[i++]= g_strdup("COMPRESS=Y");
    
    switch ([ansiQuotes state])
    {
      case NSOnState:
        _new_connection.advanced_options[i++]= g_strdup("ANSI_QUOTES=Y");
        break;
      case NSOffState:
        _new_connection.advanced_options[i++]= g_strdup("ANSI_QUOTES=N");
        break;
      case NSMixedState:
        break;
    }
    
    _new_connection.advanced_options_num= i;
    
    return &_new_connection;
  }
}

- (void)displayConnection:(MYX_USER_CONNECTION*)info
{
  if (info)
  {
    [username setStringValue: [NSString stringWithUTF8String: info->username?:""]];
    [password setStringValue: [NSString stringWithUTF8String: info->password?:""]];
    [hostname setStringValue: [NSString stringWithUTF8String: info->hostname?:""]];
    [port setIntValue: info->port];
    [schema setStringValue: [NSString stringWithUTF8String: info->schema?:""]];
    
    [socketText setStringValue: 
      [NSString stringWithUTF8String: 
        myx_get_option_value((const char**)info->advanced_options,
                             info->advanced_options_num,
                             "SOCKET_PATH") ? : ""]];
	
    [compress setState: 
      myx_get_option_value((const char**)info->advanced_options,
                           info->advanced_options_num,
                           "COMPRESS") ? NSOnState : NSOffState];
    
    const char *op= myx_get_option_value((const char**)info->advanced_options,
                                   info->advanced_options_num,
                                   "ANSI_QUOTES");
    [ansiQuotes setState: op ? (*op == 'Y' ? NSOnState : NSOffState) : NSMixedState];
  }
}

- (void)fillConnectionPopUp:(BOOL)selectLast
{
  unsigned int i;
  id selectItem= nil;
  id item;
  
  [connectionPopUp removeAllItems];
  [connectionPopUp addItemWithTitle:@""];
  
  for (i= 0; i < _connections->user_connections_num; i++)
  {
    MYX_USER_CONNECTION *conn= _connections->user_connections+i;
    if (conn->connection_name && conn->storage_type == MYX_FAVORITE_USER_CONNECTION)
    {
      NSString *name= [NSString stringWithUTF8String: conn->connection_name];
      // dont add duplicated items
      if ([connectionPopUp itemWithTitle:name]!=nil)
      {
        if (i== _connections->last_connection)
          selectItem= [connectionPopUp itemWithTitle:name];
        continue;
      }
      
      [connectionPopUp addItemWithTitle: name];
      
      [[connectionPopUp lastItem] setRepresentedObject: [NSValue valueWithPointer: conn]];
      if (i == _connections->last_connection)
      {
        selectItem= [connectionPopUp lastItem];
      }
    } 
    else
    {
      if (i == _connections->last_connection)
      {
        [connectionPopUp insertItemWithTitle:@"(last connection)"
                                     atIndex:1];
        [(selectItem= [connectionPopUp itemAtIndex: 1]) setRepresentedObject:
          [NSValue valueWithPointer: conn]];
      }
    }
  }
  
  [[connectionPopUp menu] addItem: [NSMenuItem separatorItem]];

  [connectionPopUp addItemWithTitle: @"Save Connection..."];
  item= [connectionPopUp lastItem];
  [item setTarget: self];
  [item setAction: @selector(saveConnection:)];

  [connectionPopUp addItemWithTitle: @"Open Connection Editor..."];
  item = [connectionPopUp lastItem];
  [item setAction: @selector(showConnectionEditor:)];
  [item setTarget: self];

  if (selectItem && selectLast)
  {
    [connectionPopUp selectItem:selectItem];
    [self connectionChanged:self];
  }
}


- (void)controlTextDidChange:(NSNotification *)aNotification
{
  NSString *old= [[aNotification object] stringValue];
  [self checkConnectOK];
  [connectionPopUp selectItemAtIndex:0];

  [[aNotification object] setStringValue:old];
}

- (void)checkConnectOK
{
  if ([[hostname stringValue] length] > 0)
    [connectButton setEnabled: YES];
  else
    [connectButton setEnabled: NO];
}

- (void)showConnectionError: (MYSQL*)mysql
                       info: (MYX_USER_CONNECTION*)info
{
  NSRect f= [errorPanel frame];
  f.size= [errorPanel minSize];
  
  [pingTextContainer setHidden: YES];
  
  [errorText setStringValue: 
    [NSString stringWithFormat: @"Could not connect to MySQL instance at %s.\nError: %s (code %i)",
      info->hostname, myx_mysql_error(mysql), myx_mysql_errno(mysql)]];

  [errorPanel setFrame: f
               display: YES];
  
  [NSApp runModalForWindow: errorPanel];
}


- (void)showConnectionEditor:(id)sender
{
  MPreferenceEditor *editor= [[MPreferenceEditor alloc] initForConnectionsFile:_connectionsFile];
  MYX_USER_CONNECTIONS *conns;
  MYX_LIB_ERROR error;
  
  [editor runConnectionEditor];
  
  conns= myx_load_user_connections([_connectionsFile cString], &error);
  if (conns)
  {
    myx_free_user_connections(_connections);
    _connections= conns;
  }

  [self fillConnectionPopUp:YES];
  [connectionPopUp selectItemAtIndex:_lastSelectedConnection];

  [editor release];
}


- (void)saveConnection:(id)sender
{
  MStringRequestSheet *sheet= [MStringRequestSheet sheetWithTitle:@"Save this connection for later use."
														   labels:[NSArray arrayWithObject:@"Connection Name:"]];
  NSArray *name;

  [connectionPopUp selectItemAtIndex:0];
  
  name= [sheet runModal:[self window]];
  
  if (name && [(NSString*)[name objectAtIndex:0] length]>0)
  {
    unsigned int i;
    const char *sname= [[name objectAtIndex:0] UTF8String];
    for (i= 0; i < _connections->user_connections_num; i++)
    {
      if (strcmp2(sname, _connections->user_connections[i].connection_name)==0)
        break;
    }
    if (_connections->user_connections_num != i)
    {
      NSRunAlertPanel(@"Error", @"The given name is already used by another connection.",
                      nil, nil, nil);
    }
    else
    {
      MYX_USER_CONNECTION *conn= [self getDisplayedConnection];
      
      if (conn->connection_name)
        g_free(conn->connection_name);
      conn->connection_name= g_strdup([[name objectAtIndex:0] UTF8String]);
      conn->storage_type= MYX_FAVORITE_USER_CONNECTION;

      myx_add_user_connection(&_connections, conn);
      
      myx_store_user_connections(_connections, [MPreferences preferences]->passwordStorageType,
                                 [_connectionsFile fileSystemRepresentation]);
      
      [self fillConnectionPopUp:NO];

      [connectionPopUp selectItemWithTitle:[NSString stringWithUTF8String:conn->connection_name]];
    }
  }  
}

@end

//==========================================================================

@implementation MConnectionPanel

- (id)initWithConnectionsFile:(NSString*)file
{
  MYX_LIB_ERROR error;

  [MConnectionInfo initialize];
  
  self= [super initWithWindowNibName: @"ConnectPanel"];

  _connectionsFile= [file retain];
  _connections= myx_load_user_connections([file cString], &error);
  if (!_connections)
  {
    if (error != MYX_ERROR_CANT_OPEN_FILE)
      NSLog(@"Could not load user connections file");
    _connections= g_malloc0(sizeof(MYX_USER_CONNECTIONS));
  }

  _connecting= NO;
  _mysql= NULL;
  memset(&_new_connection, 0, sizeof(MYX_USER_CONNECTION));

  return self;
}

- (void)awakeFromNib
{  
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(errorPanelWillClose:)
                                               name:NSWindowWillCloseNotification
                                             object:errorPanel];
}

- (void)setAllowSkip
{
  _allowSkip= YES;
}


- (void)setEditsSchema
{
  NSRect frame= [[self window] frame];
  NSRect eframe= [extraView frame];

  [schema setHidden:NO];
  [[[[self window] contentView] viewWithTag:201] setHidden:NO];
  
  frame.origin.y-= 30;
  frame.size.height+= 30;
  
  [[self window] setFrame:frame display:NO];
  [extraView setFrame:eframe];
  [[self window] setMinSize:frame.size];
  
  [password setNextKeyView:schema];
  
  _pickSchema= YES;
}

- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver: self];
  
  [_connectionsFile release];
  if (_connections)
    myx_free_user_connections(_connections);
  myx_free_user_connection_content(&_new_connection);
  
  
  if (pingPipe) [pingPipe release];
  if (pingTask) 
  {
    [pingTask terminate]; 
    [pingTask release];
  }
  if (pingPipeHandle) [pingPipeHandle release];
  [super dealloc];
}

- (IBAction)advancedOptionChanged:(id)sender
{
  [connectionPopUp selectItemAtIndex:0];  
}


- (IBAction)cancel:(id)sender
{
  if (!_connecting)
  {
    [self finish:nil];
  }
}


- (IBAction)connect:(id)sender
{
  MYX_USER_CONNECTION *conn;
  
  if (_skip)
  {
    [[self window] close];
    [_delegate connectionPanel:self finished:NULL info:NULL];
  }
  else
  {
    [hostname validateEditing];
    [username validateEditing];
    [password validateEditing];
    [schema validateEditing];
    [port validateEditing];
    
    [progressIndicator startAnimation: self];
    
    [self setEnabled: NO];
    
    conn= [self getDisplayedConnection];
    
    if (conn)
      [self startConnection: conn];
  }
}


- (IBAction)connectionChanged:(id)sender
{
  id pitem= [connectionPopUp selectedItem];
  id item= [pitem representedObject];
  MYX_USER_CONNECTION *conn= [item pointerValue];
  
  if (conn)
    [self displayConnection:conn];
  [self checkConnectOK];
  
  _lastSelectedConnection= [connectionPopUp indexOfItem:pitem];
}

- (IBAction)toggleDetails:(id)sender
{
  NSRect frame= [[self window] frame];
  NSSize newSize;
  BOOL expanded;

  if ([detailsButton state])
  {
    expanded= YES;
    newSize= [[self window] minSize];
    newSize.height+= 108-22;
  }
  else
  {
    expanded= NO;
    newSize= [[self window] minSize];
  }

  frame.origin.y += (frame.size.height - newSize.height);
  frame.size= newSize;

  if (expanded)
  {
    [[self window] setFrame:frame display:YES animate:YES];
    [extraView setFrame:NSMakeRect(20, 43, 388, 108)];
  }
  else
  {
    [[self window] setFrame:frame display:YES animate:YES];
    [extraView setFrame:NSMakeRect(20, 43, 388, 22)];
  }

  [[[self window] contentView] setNeedsDisplay:YES];
}

- (void)windowDidLoad
{
  // fill list of saved connections
  [self fillConnectionPopUp:YES];

  [self checkConnectOK];
}

- (void)show
{ 
  [self showWindow: self];  
}


- (void)showAndConnectTo:(NSString*)connection
{
  unsigned int i;
  
  for (i= 0; i < _connections->user_connections_num; i++)
  {
    if (_connections->user_connections[i].connection_name &&
        strcmp([connection UTF8String], _connections->user_connections[i].connection_name)==0)
    {
      [connectionPopUp selectItem:[connectionPopUp itemWithTitle:connection]];
      break;
    }
    if (!_connections->user_connections[i].connection_name &&
        [connection isEqualTo:[NSString stringWithFormat:@"%s@%s",
          _connections->user_connections[i].username,
          _connections->user_connections[i].hostname]])
    {
      [connectionPopUp selectItemAtIndex:0];
      [self displayConnection:_connections->user_connections+i];
      break;
    }
  }
  
  [self show];
  
  if (i < _connections->user_connections_num)
  {
    [self connect:nil];
  }
}

- (void)setDelegate:(id)deleg
{
  _delegate= deleg;
}

- (void)setHeaderImage:(NSImage*)image
{
  [topImage setImage: image];
}

// --------------------------

- (void)handlePingData:(NSNotification*)notif
{
  NSDictionary *userInfo= [notif userInfo];
  NSString *str= [[NSString alloc] initWithData:[userInfo objectForKey:@"NSFileHandleNotificationDataItem"]
                                       encoding: NSUTF8StringEncoding];

  [[pingText textStorage] appendAttributedString:[[[NSAttributedString alloc] initWithString: str] autorelease]];
  
  [pingText scrollPageDown:nil];
  [str release];
  
  [pingPipeHandle readInBackgroundAndNotifyForModes:[NSArray arrayWithObject:NSModalPanelRunLoopMode]];
}


- (IBAction)pingHost:(id)sender
{
  if (!pingTask)
  {
    NSRect f= [errorPanel frame];
    f.origin.y+= (f.size.height - [errorPanel maxSize].height);
    f.size= [errorPanel maxSize];
    
    [pingTextContainer setHidden: NO];
  
    [errorPanel setFrame: f
                 display: YES
                 animate: YES];
    
    pingPipe= [[NSPipe alloc] init];
    pingPipeHandle= [[pingPipe fileHandleForReading] retain];
    
    pingTask= [[NSTask alloc] init];
    [pingTask setLaunchPath:@"/sbin/ping"];
    [pingTask setArguments:[NSArray arrayWithObjects:[hostname stringValue], nil]];
    [pingTask setStandardOutput:pingPipe];
    [pingTask setStandardError:pingPipe];

    [pingTask launch];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handlePingData:)
                                                 name:NSFileHandleReadCompletionNotification
                                               object:pingPipeHandle];
    
    [pingPipeHandle readInBackgroundAndNotifyForModes:[NSArray arrayWithObject: NSModalPanelRunLoopMode]];
  }
}


- (IBAction)closeErrorPanel:(id)sender
{
  [pingTask terminate];
  [pingTask release];
  pingTask= nil;
  [pingPipe release];
  pingPipe= nil;
  [pingPipeHandle release];
  pingPipeHandle= nil;

  [[NSNotificationCenter defaultCenter] removeObserver:self
                                                  name:NSFileHandleReadCompletionNotification
                                                object:pingPipeHandle];

  [NSApp stopModal];
  if (sender != nil) // ie, not being called from the notif. observer
    [errorPanel close];
}


- (void)errorPanelWillClose:(NSNotification*)notif
{
  [self closeErrorPanel:nil];
}


@end
