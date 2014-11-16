//
//  MAdministratorProtocol.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Wed Jul 28 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#include "myx_admin_public_interface.h"

@class MMySQLDispatcher;
@class MSchemaDataSource;
@class MConnectionInfo;

@protocol MAdministratorProtocol

- (void)setCanSwitchPanel:(BOOL)flag;

- (MMySQLDispatcher*)dispatcher;

- (MYSQL*)mysql;
- (MConnectionInfo*)serverInfo;

- (int)serverStatus;
- (BOOL)isLocal;

- (NSWindow*)window;

- (void)refreshSchemata;

- (MSchemaDataSource*)sharedSchemaDS;
- (MSchemaDataSource*)sharedObjectDS;

- (MYX_CATALOGS*)catalogList;

@end
