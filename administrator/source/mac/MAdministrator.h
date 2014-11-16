//
//  MAdministrator.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jun 24 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//


#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MSchemaDataSource.h>
#import <MySQLToolsCommon/MMySQLDispatcher.h>
#include <myx_public_interface.h>

#import "MAdministratorProtocol.h"

@class MAPanel;
@class MSchemaDataSource;
@class MConnectionInfo;


extern NSString *MASchemaDataSourceChanged;

// this does the role of a NSDocument
@interface MAdministrator : NSWindowController < MAdministratorProtocol > 
{
  IBOutlet NSDrawer *drawer;
  NSMutableDictionary *_panels; // className -> panel instances

  MAPanel *_dummyPanel;
  
  MAPanel *_currentPanel;

  NSString *_connectedInstance;
  
  NSImage *_catalogIcon;
  NSImage *_schemaIcon;
  NSImage *_tableIcon;
  NSImage *_columnIcon;
  NSImage *_keyIcon;

  NSLock *_mysqlLock; // remove
  MConnectionInfo *_info;
  MYSQL *_mysql; // remove
  
  MMySQLDispatcher *_dispatcher;

  BOOL _closing;
  
  MYX_CATALOGS *_catalogs;
  
  BOOL _schemaFetched;
  MSchemaDataSource *_schemaDS;
  MSchemaDataSource *_objectDS;
}

// gains ownership of mysql and info
- (id)initWithConnection: (MYSQL*)mysql
                    info: (MConnectionInfo*)info;
- (id)initWithoutConnection;

- (void)toolbarItemClicked:(id)sender;

- (NSToolbarItem *)toolbar:(NSToolbar *)toolbar 
     itemForItemIdentifier:(NSString *)itemIdentifier 
 willBeInsertedIntoToolbar:(BOOL)flag;
- (NSArray *)toolbarDefaultItemIdentifiers:(NSToolbar*)toolbar;
- (NSArray *)toolbarAllowedItemIdentifiers:(NSToolbar*)toolbar;
- (NSArray *)toolbarSelectableItemIdentifiers:(NSToolbar *)toolbar;

- (void)setTitleForPanel: (NSString*)name;

- (void)show;
- (void)close;

- (void)switchToPanel: (MAPanel*)panel;

- (BOOL)isConnected;
- (BOOL)isLocal;
- (int)serverStatus;

- (void)unlockMySQL;
- (MYSQL*)mysql;
- (MYSQL*)mysqlLock;
- (MConnectionInfo*)serverInfo;

- (MMySQLDispatcher*)dispatcher;

- (MSchemaDataSource*)sharedSchemaDS;
- (MSchemaDataSource*)sharedObjectDS;

- (MYX_CATALOGS*)catalogList;

@end


extern BOOL MARegisterPanel(id panelClass);
extern NSArray *MAGetPanelList();
