//
//  MQueryBrowserDocument.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/22.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#import <MySQLToolsCommon/MMySQLDispatcher.h>
#include <myx_public_interface.h>
#include <myx_qb_public_interface.h>

@class MConnectionInfo;

@interface MQueryBrowserDocument : NSDocument 
{
  NSLock *_mysqlLock; // remove
  MConnectionInfo *_info;
  MYSQL *_mysql;
  
  NSString *_connectedInstance;
  
  MMySQLDispatcher *_dispatcher;
}

// gains ownership of mysql and info
- (id)initWithConnection: (MYSQL*)mysql
                    info: (MConnectionInfo*)info;


- (MMySQLDispatcher*)dispatcher;

- (void)setDefaultSchema:(NSString*)schema;
- (NSString*)defaultSchema;

- (void)unlockMySQL;
- (MYSQL*)mysql;
- (MYSQL*)mysqlLock;
- (MConnectionInfo*)serverInfo;
- (void)killConnection:(unsigned long)pid;
- (NSString*)connectedInstance;

- (BOOL)connectedMySQLIsAtLeastMajor:(int)major minor:(int)minor;
@end
