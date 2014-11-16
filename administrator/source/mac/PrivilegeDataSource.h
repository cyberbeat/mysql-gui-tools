//
//  PrivilegeDataSource.h
//  MySQL Administrator
//
//  Created by Alfredo Kojima on Thu Jul 08 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>

#include "myx_admin_public_interface.h"


typedef enum {
  PGlobal,
  PSchema,
  PTable,
  PColumn,
  PRoutine
} PrivilegeType;

@interface PrivilegeDataSource : NSObject
{
  NSArray *_allPrivileges;
  NSMutableArray *_assignedPrivileges;
  MYX_USER_OBJECT_PRIVILEGES *_privileges;
  
  PrivilegeType _type;
  
  NSTableView *_sourceTable;
}

- (void)dealloc;
- (id)initWithPrivileges: (NSArray*)privileges
             sourceTable: (NSTableView*)table
                    type: (PrivilegeType)type;

- (void)setPrivileges: (MYX_USER_OBJECT_PRIVILEGES*)objPrivileges;
- (void)revoke: (NSString*)priv;
- (void)grant: (NSString*)priv;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn 
            row:(int)rowIndex;

@end