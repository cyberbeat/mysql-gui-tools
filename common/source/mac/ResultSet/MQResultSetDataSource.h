//
//  MQResultSetDataSource.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include <myx_public_interface.h>

#include "MYXResultSet.h"

@interface MQResultSetDataSource : NSObject 
{
  MYXResultSet *_resultSet;
  NSZone *_zone;
  NSLock *_lock;
  
  BOOL _editEnabled;
}

- (id)initWithResultSet:(MYX_RESULTSET*)rs lock: (NSLock *)lock;

- (MYX_RESULTSET*)resultset;
- (MYXResultSet*)myxResultSet;

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView
 objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (id)tableView:(NSTableView *)aTableView 
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (void)tableView:(NSTableView *)aTableView 
   setObjectValue:(id)anObject 
   forTableColumn:(NSTableColumn *)aTableColumn 
			  row:(int)rowIndex;

- (void)addRow;
- (void)deleteRow:(int)rowIndex;

- (MYXRSEditStatus)statusOfRow:(int)row
                        column:(int)column;

- (MYXRSCompareStatus)compareStatusOfRow:(int)row
                                  column:(int)column;

- (BOOL)isEditable;
- (BOOL)editing;
- (void)setEditing:(BOOL)flag;
- (BOOL)hasChanges;

- (void)preCommit;
- (void)postCommit:(BOOL)failed;
- (void)discardChanges;
@end
