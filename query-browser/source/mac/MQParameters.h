//
//  MQParameters.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 05/6/11.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <myx_public_interface.h>

@class MQResultSetView;

@interface MQParameters : NSObject 
{
  NSMutableArray *_normalParams;
  NSMutableArray *_globalParams;
  NSMutableArray *_localParams;

  MYX_RS_ROW *_row;
}

- (void)saveGlobals;
  
- (void)setResultset:(MYX_RESULTSET*)rs row:(int)rowIndex;
- (id)localParameters;
- (void)setLocalParameters:(id)params;

- (BOOL)itemCanBeDeleted:(id)item;
- (BOOL)itemCanBeAdded:(id)item;
- (BOOL)itemCanBeEdited:(id)item;

- (void)addParameter:(id)parent;
- (void)deleteParameter:(id)item;

- (MYX_STRINGLIST*)parametersFromMaster:(MQResultSetView*)rsview;
@end
