//
//  MQResultSetDataSource.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 3/6/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQResultSetDataSource.h"
#include "MYXResultSet.h"

@implementation MQResultSetDataSource

static MYXResultSetCallbacks callbacks= {
  NULL,
  NULL,
  NULL
};

- (id)initWithResultSet:(MYX_RESULTSET*)rs lock: (NSLock *)lock
{
  self= [super init];
  if (self)
  {
	_resultSet= new MYXResultSet(rs, callbacks);
	_zone= NSCreateZone(256*1024, 64*1024, NO);
  _lock= lock;
  }
  return self;
}

- (MYX_RESULTSET*)resultset
{
  return _resultSet->get_resultset();
}

- (MYXResultSet*)myxResultSet
{
  return _resultSet;
}

- (void)dealloc
{
  NSRecycleZone(_zone);
  delete _resultSet;
  [super dealloc];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return _resultSet->get_row_count();
}

- (id)tableView:(NSTableView *)aTableView
 objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex
{
  int column= [[aTableColumn identifier] intValue];
  char *value;
  size_t length;
  
  if(_lock)
    [_lock lock];
  bool b= _resultSet->get(rowIndex, column, value, length);
  if(_lock)
    [_lock unlock];

  if (b)
  {
    if (_resultSet->get_column_type(column) == MYX_RSCT_BLOB)
      return @"";
    else
      return [[NSString allocWithZone:_zone] initWithBytesNoCopy:value
                                                          length:length
                                                        encoding:NSUTF8StringEncoding
                                                    freeWhenDone:NO];
  }
  else
    return nil;
} 


- (void)tableView:(NSTableView *)aTableView 
   setObjectValue:(id)anObject 
   forTableColumn:(NSTableColumn *)aTableColumn 
			  row:(int)rowIndex
{
  int column= [[aTableColumn identifier] intValue];
  //id ovalue= [self tableView:aTableView
  // objectValueForTableColumn:aTableColumn
	//					 row:rowIndex];
  unsigned int nr;

// this optimization had problmes when changing
// string value from NULL to empty string
// if you really need it make sure NULL-to-empty works as well
//  if (ovalue && [ovalue isEqualTo:anObject])
//    return;

  nr= _resultSet->get_row_count();
  if ([anObject isKindOfClass:[NSString class]])
  {
    const char *data= (const char*)[anObject UTF8String];
    _resultSet->set(rowIndex, column, data, strlen(data));
  }
  else if(anObject)
  {
    _resultSet->set(rowIndex, column, (const char*)[anObject bytes], [anObject length]);
  }
  else
  {
    _resultSet->set(rowIndex, column, NULL, sizeof(const char *));
  }

  if (_resultSet->get_row_count() != nr)
    [aTableView noteNumberOfRowsChanged];
}


- (void)addRow
{
  _resultSet->add_row();
}


- (void)deleteRow:(int)rowIndex
{
  _resultSet->delete_row(rowIndex);
}


- (BOOL)isEditable
{
  return _resultSet->get_resultset()->editable;
}

- (BOOL)editing
{
  return _editEnabled;
}

- (BOOL)hasChanges
{
  return _resultSet->has_changes();
}

- (void)setEditing:(BOOL)flag
{
  _resultSet->set_placeholder_enabled(flag);
  _editEnabled= flag;
}


- (MYXRSEditStatus)statusOfRow:(int)row
				   column:(int)column
{
  return _resultSet->get_edit_status(row, column);
}


- (MYXRSCompareStatus)compareStatusOfRow:(int)row
                                  column:(int)column
{
  if(_lock)
    [_lock lock];
  MYXRSCompareStatus status= _resultSet->get_compare_status(row, column);
  if(_lock)
    [_lock unlock];
  return status;
}


- (void)preCommit
{
  _resultSet->pre_commit();
}

- (void)postCommit:(BOOL)failed
{
  _resultSet->post_commit(failed);
}

- (void)discardChanges
{
  _resultSet->discard_changes();
}

@end
