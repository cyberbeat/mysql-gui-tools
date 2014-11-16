//
//  MMySQLDispatcher.h
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Thu Jul 15 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import <Foundation/Foundation.h>
#include <pthread.h>

#include <myx_public_interface.h>

@class MConnectionInfo;

@interface MMySQLDispatcher : NSObject {
  pthread_t _tid;

  pthread_mutex_t _mysql_mx;
  MYSQL *_mysql;
  MConnectionInfo *_instance;

  pthread_mutex_t _request_mx;
  pthread_cond_t _request_ready;
  NSMutableArray *_request_queue;
  
  BOOL _disconnected;
}

- (id)initWithMySQL: (MYSQL*)mysql
           instance: (MConnectionInfo*)instance;

- (NSString*)lastMySQLError;

- (void)performSelector:(SEL)selector
               argument:(id)argument
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target;

- (void)performCallback:(void*(*)(MYSQL*))cback
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target;

- (void)performCallback:(void*(*)(MYSQL*,void*))cback
               argument:(void*)arg1
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target;

- (void)performCallback:(void*(*)(MYSQL*,void*,void*))cback
               argument:(void*)arg1
               argument:(void*)arg2
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target;

- (void)performCallback:(void*(*)(MYSQL*,void*,void*,void*))cback
               argument:(void*)arg1
               argument:(void*)arg2
               argument:(void*)arg3
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target;

- (void*)performCallback:(void*(*)(MYSQL*))cback
           waitForWindow:(id)window
                 message:(NSString*)message;

- (void*)performCallback:(void*(*)(MYSQL*,void*))cback
                argument:(void*)arg1
           waitForWindow:(id)window
                 message:(NSString*)message;

- (void*)performCallback:(void*(*)(MYSQL*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
           waitForWindow:(id)window
                 message:(NSString*)message;

- (void*)performCallback:(void*(*)(MYSQL*,void*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
                argument:(void*)arg3
           waitForWindow:(id)window
                 message:(NSString*)message;

- (void*)performCallback:(void*(*)(MYSQL*,void*,void*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
                argument:(void*)arg3
                argument:(void*)arg4
           waitForWindow:(id)window
                 message:(NSString*)message;


- (BOOL)checkConnection;
- (BOOL)reconnect;

- (MYX_LIB_ERROR)performQuery:(NSString*)query
                waitForWindow:(id)window
                      message:(NSString*)message;

@end
