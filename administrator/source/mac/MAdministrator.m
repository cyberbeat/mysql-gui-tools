//
//  MAdministrator.m
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jun 24 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MAdministratorController.h"
#import "MAdministrator.h"
#import "MAPanel.h"

#import <MySQLToolsCommon/MConnectionInfo.h>

#include "myx_admin_public_interface.h"

#define DEFAULT_PANEL_WIDTH 800


static NSMutableArray *Panels= nil;
static NSMutableDictionary *PanelsById= nil;

NSString *MASchemaDataSourceChanged=@"MASchemaDataSourceChanged";

BOOL MARegisterPanel(id panelClass)
{
  if (!Panels)
    Panels= [[NSMutableArray alloc] init];
  if (!PanelsById)
    PanelsById= [[NSMutableDictionary alloc] init];
  
  if (![panelClass isSubclassOfClass: [MAPanel class]])
  {
    NSLog(@"ERROR: Not registering invalid panel (%@)", [panelClass className]);
    return NO;
  }
  
  //NSLog(@"registering panel %@", [panelClass className]);
  [Panels addObject: panelClass];
  
  [PanelsById setObject: panelClass forKey: [panelClass className]];
  
  return YES;
}


NSArray *MAGetPanelList()
{
  return Panels;
}



@interface MAdministrator(Private)
- (void)fetchSchemas;
- (void)tablesArrived:(NSArray*)args result:(MYX_SCHEMA_TABLES*)tables;
- (void)requestTablesForSchema:(MSchemaDataSource*)sender item:(MSchemaItem*)item;
@end

@implementation MAdministrator(Private)

- (void)fetchSchemas
{
  MYX_CATALOGS *cats;
  
retry:
  cats= [_dispatcher performCallback:(void*(*)(MYSQL*))myx_get_catalogs
                       waitForWindow:[self window]
                             message:@"Retrieving catalog list..."];
  if (!cats && ![_dispatcher checkConnection] && [_dispatcher reconnect])
    goto retry;

  if (!cats)
  {
    char *tmp;
    NSRunAlertPanel(@"Error",
                    [NSString stringWithFormat:@"Could not retrieve catalogs.\n%s", 
                      tmp= myx_mysql_error(_mysql)],
                    @"OK",nil,nil);
    g_free(tmp);
    NSLog(@"ERROR: Could not retrieve catalog list");
    return;
  }
  
  [_schemaDS updateWithCatalogs:cats];
  [_objectDS updateWithCatalogs:cats];
  
  [[NSNotificationCenter defaultCenter]
 postNotificationName:MASchemaDataSourceChanged
               object:self
             userInfo:[NSDictionary dictionaryWithObject:_objectDS
                                                  forKey:@"ds"]];
  [[NSNotificationCenter defaultCenter]
 postNotificationName:MASchemaDataSourceChanged
               object:self
             userInfo:[NSDictionary dictionaryWithObject:_schemaDS
                                                  forKey:@"ds"]];
  
  if (_catalogs)
    myx_free_catalogs(_catalogs);
  _catalogs= cats;
}


- (void)requestTablesForSchema:(MSchemaDataSource*)sender item:(MSchemaItem*)item
{
  MYX_SCHEMA *schema= [item schema];
  [_dispatcher performCallback:(void*(*)(MYSQL*,void*,void*))myx_get_schema_tables
                      argument:schema->catalog_name
                      argument:schema->schema_name
              finishedSelector:@selector(tablesArrived:result:)
                      argument:[[NSArray arrayWithObjects:sender,item,nil] retain]
                        target:self];
}


- (void)tablesArrived:(NSArray*)args result:(MYX_SCHEMA_TABLES*)tables
{
  MSchemaDataSource *ds= [args objectAtIndex:0];
  MSchemaItem *sitem= [args objectAtIndex:1];
  MYX_SCHEMA *schema= [sitem schema];
  [args autorelease];
  
  if (tables)
  {
    if (schema->schema_tables)
      myx_free_schema_tables(schema->schema_tables);
    schema->schema_tables= tables;
  }
  if (!tables && ![_dispatcher checkConnection] && [_dispatcher reconnect])
  {
    [self requestTablesForSchema:ds item:sitem];
    return;
  }
  if (!tables)
  {
    NSLog(@"could not retrieve tables list");
    return;
  }
  [_objectDS resetSchemaChildren:sitem];
  
  [_objectDS updateSchema:sitem
               withTables:tables];
  
  [[NSNotificationCenter defaultCenter]
 postNotificationName:MASchemaDataSourceChanged
               object:self
             userInfo:[NSDictionary dictionaryWithObject:ds 
                                                  forKey:@"ds"]];
}
@end


@implementation MAdministrator

- (id)init
{  
  NSRect frame;
  
  self = [super initWithWindowNibName:@"Administrator" owner:self];
  if (!self)
    return nil;
  
  [self window];
  if (![self isWindowLoaded])
    NSLog(@"ERROR: Could not load document window");
  
  _panels= [[NSMutableDictionary alloc] init];
  _mysqlLock= [[NSLock alloc] init];
  
  _catalogIcon= [[NSImage imageNamed:@"16x16_Catalog.png"] retain];
  _schemaIcon= [[NSImage imageNamed:@"16x16_Database.png"] retain];
  _tableIcon= [[NSImage imageNamed:@"16x16_Table.png"] retain];
  _columnIcon= [[NSImage imageNamed:@"16x16_Field.png"] retain];
  _keyIcon= [[NSImage imageNamed:@"16x16_KeyColumn.png"] retain];
  
  frame= [[self window] frame];
  if (frame.size.width < DEFAULT_PANEL_WIDTH)
    frame.size.width= DEFAULT_PANEL_WIDTH;
  
  
  _schemaDS= [[MSchemaDataSource alloc] initWithRoot:MCatalogItemType leaf:MSchemaItemType];
  
  _objectDS= [[MSchemaDataSource alloc] initWithRoot:MCatalogItemType leaf:MObjectItemType];
  [_objectDS setTableFetcher:self selector:@selector(requestTablesForSchema:item:)];
  
  
  [[self window] setFrame:frame display:YES animate:NO];
  
  return self;
}


- (id)initWithoutConnection
{
  self = [self init];
  if (!self)
    return nil;
  
  if (self)
  {    
    _connectedInstance= [[NSString stringWithString:@"Not Connected"] retain];
    
    [self setTitleForPanel: nil];
  }
  return self;  
}

- (id)initWithConnection: (MYSQL*)mysql
                    info: (MConnectionInfo*)info
{
  self = [self init];
  if (!self)
    return nil;
  
  if (self)
  {
    int major_version= myx_get_mysql_major_version(mysql);
    
    _mysql= mysql;
    _info= [info retain];
    
    
    if (major_version < 4)
      NSRunAlertPanel(@"Warning", 
                      [NSString stringWithFormat:@"You are connecting to a MySQL server with version %i.%i.x, "
                        "but this application was designed to work with MySQL "
                        "servers 4.x and newer.\n"
                        "Please note that older versions are not supported and "
                        "may not work properly.",
                        major_version, myx_get_mysql_minor_version(mysql)],
                      nil, nil, nil);
    
    
    _dispatcher= [[MMySQLDispatcher alloc] initWithMySQL:mysql
                                                instance:info];
    
    if (mysql->unix_socket)
      _connectedInstance= [[NSString alloc] initWithFormat:@"%@ via socket", [info hostname]];
    else
      _connectedInstance= [[NSString alloc] initWithFormat:@"%@:%i", [info hostname], [info port]];
    
    [self setTitleForPanel: nil];
  }
  return self;
}


- (void)dealloc
{
  [_dispatcher release];
  [_panels release];
  [_connectedInstance release];
  [_mysqlLock release];
  [_catalogIcon release];
  [_schemaIcon release];
  [_tableIcon release];
  [_columnIcon release];
  [_schemaDS release];
  [_objectDS release];
  [_info release];
  
  if (_catalogs)
  {
    myx_free_catalogs(_catalogs);
    _catalogs= 0;
  }
  if (_mysql)
    myx_mysql_close(_mysql);
  
  [super dealloc];
}

- (void)awakeFromNib
{
  NSToolbar *toolbar= [[NSToolbar alloc] initWithIdentifier:@"toolbar"];
  
  [toolbar setDelegate: self];
  [[self window] setToolbar:toolbar];
  
  [toolbar release];
}


- (MMySQLDispatcher*)dispatcher
{
  return _dispatcher;
}


- (void)setCanSwitchPanel:(BOOL)flag
{
  
}

- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar 
     itemForItemIdentifier:(NSString *)itemIdentifier 
 willBeInsertedIntoToolbar:(BOOL)flag
{
  NSToolbarItem *newItem = [[[NSToolbarItem alloc] initWithItemIdentifier:itemIdentifier] autorelease];
  id itemClass = [PanelsById objectForKey:itemIdentifier];
  
  [newItem setLabel:[itemClass label]];
  [newItem setPaletteLabel:[itemClass label]];
  [newItem setImage:[itemClass icon]];
  [newItem setToolTip:[itemClass toolTip]];
  [newItem setTarget:self];
  [newItem setAction:@selector(toolbarItemClicked:)];
  //  [newItem setMenuFormRepresentation:[item menuFormRepresentation]];
  
  return newItem;
}

- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar*)toolbar
{
  NSMutableArray *array= [NSMutableArray array];
  int i;
  int count= [Panels count];
  //  [array addObject: [[Panels objectAtIndex: 0] className]];
  //  [array addObject: NSToolbarSeparatorItemIdentifier];  
  for (i= 0; i < count; i++)
    [array addObject: [[Panels objectAtIndex: i] className]];
  return array;
}

- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar*)toolbar
{
  NSMutableArray *array= [NSMutableArray array];
  int i;
  int count= [Panels count];
  for (i= 0; i < count; i++)
    [array addObject: [[Panels objectAtIndex: i] className]];
  return array;
}

- (NSArray *)toolbarSelectableItemIdentifiers:(NSToolbar *)toolbar
{
  return [self toolbarAllowedItemIdentifiers:toolbar];
}

- (void)toolbarItemClicked:(id)sender
{
  MAPanel *panel;
  NSString *panelName;
  
  if (sender)
    panelName= [sender itemIdentifier];
  else
    panelName= [[[self window] toolbar] selectedItemIdentifier];
  
  panel= [_panels objectForKey: panelName];
  if (!panel)
  {
    if ([[PanelsById objectForKey: panelName] needsConnection] && ![self isConnected])
    {
      // panel is for when connected only and we're not connected
      panel= [[MAPanel alloc] initWithMessage:@"Not Connected" owner:self];
    }
    else
    {
      // initialize and cache panel if it wasn't yet
      panel= [[[PanelsById objectForKey: panelName] alloc] initWithOwner: self];
      if(panel)
      {
        [_panels setObject: panel forKey: panelName];
        [panel release];
      }
    }
  }
  
  if(panel)
  {
    [self setTitleForPanel: [NSString stringWithFormat:@"Loading %@...", 
      [[PanelsById objectForKey: panelName] label]]];
    [[self window] display];

    [self switchToPanel: panel];
  }
  else
  {
    [[[self window] toolbar] setSelectedItemIdentifier: [_currentPanel className]];  
  }
}

- (void)setTitleForPanel: (NSString*)name
{
  if (name)
    [[self window] setTitle: [NSString stringWithFormat:@"%@ %s %@", 
      name, "Ñ", _connectedInstance]];
  else
    [[self window] setTitle: _connectedInstance];
}

- (void)show
{
  NSToolbar *toolbar;
  if (_info)
  {
    MYX_USER_CONNECTION *conn= [_info createUserConnection];
  
    MYX_MACHINE_INFO *mi= myx_get_server_info(conn, _mysql);
    mi= myx_get_server_info(conn, _mysql);
  
    myx_free_user_connection_content(conn);
    g_free(conn);
  }
  toolbar= [[self window] toolbar]; 
  
  [toolbar setSelectedItemIdentifier: [[Panels objectAtIndex:0] className]];
  [self toolbarItemClicked: nil];
  
  [[self window] setFrameAutosaveName: [NSString stringWithFormat:@"administrator.%@",_connectedInstance]];
  
  [[self window] makeKeyAndOrderFront:nil];
}

- (void)close
{
  if (!_closing)
    [[self window] close];
  
  [[MAdministratorController sharedDocumentController] removeInstance:self];
}


- (void)windowWillClose: (NSNotification*)sender
{
  NSEnumerator *enumerator = [_panels keyEnumerator];
  id key;
  
  while ((key= [enumerator nextObject])) 
  {
    [[_panels objectForKey:key] willClose];
  }  
  
  _closing= YES;
  [self close];
}


- (void)switchToPanel: (MAPanel*)panel
{
  NSRect oldFrame;
  NSRect newFrame;
  NSWindow *window= [self window];
  
  if (panel == _currentPanel)
    return;
  
  if (_currentPanel)
  {
    if (![_currentPanel willHide])
      return;
  }
  if (![panel willShow])
    return;
  
  oldFrame= [window frame];
  
  newFrame= [window frameRectForContentRect: [[panel topView] frame]];
  newFrame.size.width= oldFrame.size.width;
  
  newFrame.origin= oldFrame.origin;
  if (newFrame.size.height > oldFrame.size.height)
    newFrame.origin.y+= (oldFrame.size.height - newFrame.size.height);
  
  if (newFrame.size.height < oldFrame.size.height)
    newFrame.size.height= oldFrame.size.height;
  
  if (_currentPanel)
    [[_currentPanel topView] retain];
  [window setContentView:nil];
  
  [window setFrame:newFrame display:YES animate:NO];
  [window setMinSize: [panel defaultFrame].size];
  
  [window setContentView:[panel topView]];
  [[panel topView] setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable|NSViewMinYMargin];
  
  [self setTitleForPanel: [[PanelsById objectForKey:[[[self window] toolbar] selectedItemIdentifier]] label]];
  
  {
    NSView *sideBox= [panel sideView];
    [drawer setContentView: sideBox];
    if (sideBox)
    {
      [sideBox setFrameSize: [drawer contentSize]];
      [drawer open];
    }
    else
    {
      [drawer close];
    }
  }
  
  if (_currentPanel)
    [_currentPanel didHide];
  [panel didShow];
  
  _currentPanel= panel;
}


- (BOOL)isConnected
{
  if (_mysql)
    return YES;
  return NO;
}

- (BOOL)isLocal
{
  if (_mysql)
    return myx_is_localhost([[_info hostname] UTF8String])!=0;
  else
    return YES;
}

- (int)serverStatus
{
  gchar *output= NULL;
  gint exst;
  
  if (![self isLocal])
  {
    if ([self isConnected])
      return 1;
    
    return -1;
  }  
  if (g_spawn_command_line_sync("/bin/ps ax -ocommand", &output, NULL, &exst, NULL)
      && exst == 0)
  {
    char *ptr= strtok(output, "\n");
    while (ptr != NULL)
    {
      char *a;
      if ((a=strstr(ptr, "mysqld")))
      {
        g_free(output);
        return 1;
      }
      ptr= strtok(NULL, "\n");
    }
  }
  if (output)
    g_free(output);
  return 0;
}

- (void)unlockMySQL
{
  [_mysqlLock unlock];
}

- (MYSQL*)mysql
{
  return _mysql;
}

- (MYSQL*)mysqlLock
{
  [_mysqlLock lock];
  return _mysql;
}

- (MConnectionInfo*)serverInfo
{
  return _info;
}

- (MSchemaDataSource*)sharedSchemaDS
{
  if (!_schemaFetched)
  {
    _schemaFetched= YES;
    [self fetchSchemas];
  }
  return _schemaDS;
}

- (MSchemaDataSource*)sharedObjectDS
{
  if (!_schemaFetched)
  {    
    _schemaFetched= YES;
    [self fetchSchemas];
  }
  return _objectDS;
}

- (void)refreshSchemata
{
  [self fetchSchemas];
}

- (MYX_CATALOGS*)catalogList
{
  return _catalogs;
}

- (NSWindow*)window
{
  return [super window];
}

@end
