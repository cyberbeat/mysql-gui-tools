//
//  MQHistory.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 4/19/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>

#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"


extern NSString *MQHistoryDidChangeNotification;

@interface MQHistoryItem : NSObject 
{
@public
  NSString *_query;
  NSString *_catalog;
  NSString *_schema;

  MYX_Q_TYPE _queryType;
  
  time_t _lastAccess;
}

- (id)initWithItem:(MYX_HISTORY_ENTRY*)entry;

- (NSString*)query;

@end


@interface MQHistory : NSObject
{
  NSMutableDictionary *_itemsByInterval;
  int _periodForIndex[32];
}

- (BOOL)loadFromFile:(NSString*)file;
- (void)storeToFile:(NSString*)file;
- (void)removeItem:(MQHistoryItem*)item;

- (void)rememberQuery:(NSString*)query
			  catalog:(NSString*)catalog
			   schema:(NSString*)schema;


- (void)reset;

@end
