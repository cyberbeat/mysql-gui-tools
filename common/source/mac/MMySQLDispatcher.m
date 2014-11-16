//
//  MMySQLDispatcher.m
//  MySQLGUICommon
//
//  Created by Alfredo Kojima on Thu Jul 15 2004.
//  Copyright (c) 2004 MySQL AB. All rights reserved.
//

#import "MMySQLDispatcher.h"
#import "MConnectionInfo.h"
#import "MProgressPanel.h"
#include "errmsg.h"

@interface Request : NSObject
{
  @public
  id result;
  SEL finished;
  id target;
  id finArgument;
}
- (void)execute:(MYSQL*)mysql;
- (void)finish;
@end

@implementation Request
- (void)execute:(MYSQL*)mysql
{
}

- (void)finish
{
  [target performSelector:finished withObject:finArgument withObject:result];
}
@end


@interface SelectorRequest : Request
{
  @public
  id arg;
  SEL selector;
}
@end

@implementation SelectorRequest
- (void)execute:(MYSQL*)mysql
{
  [target performSelector:selector withObject:(id)mysql withObject:arg];
}
@end


@interface CallbackRequest : Request
{
  @public
  void *(*callback0)(MYSQL*);
  void *(*callback1)(MYSQL*,void*);
  void *(*callback2)(MYSQL*,void*,void*);
  void *(*callback3)(MYSQL*,void*,void*,void*);
  void *(*callback4)(MYSQL*,void*,void*,void*,void*);
  void *arg1;
  void *arg2;
  void *arg3;
  void *arg4;
}
@end

@implementation CallbackRequest
- (void)execute:(MYSQL*)mysql
{
  if (callback0)
    result= (*callback0)(mysql);
  else if (callback1)
    result= (*callback1)(mysql, arg1);
  else if (callback2)
    result= (*callback2)(mysql, arg1, arg2);
  else if (callback3)
    result= (*callback3)(mysql, arg1, arg2, arg3);
  else if (callback4)
    result= (*callback4)(mysql, arg1, arg2, arg3, arg4);
}
@end


@interface WaitCallbackRequest : CallbackRequest
{
  @public
  BOOL dialogVisible;
  BOOL didFinish;
}
@end

@implementation WaitCallbackRequest
- (void)execute:(MYSQL*)mysql
{
  [super execute:mysql];
  didFinish= YES;
}

- (void)finish
{
  didFinish= YES;
  if (dialogVisible)
  {
    [NSApp abortModal];
  }
}
@end



@interface MMySQLDispatcher(Private)
- (Request*)getNextRequest;
- (void)executeRequest:(Request*)request;

- (void)enqueue:(Request*)req;
- (void)handleFinished:(Request*)req;
- (BOOL)waitForRequestToFinish:(WaitCallbackRequest*)req window:(NSWindow*)window message:(NSString*)msg;
@end


@implementation MMySQLDispatcher(Private)
- (Request*)getNextRequest
{
  Request *req;
  
  // wait for a request
  pthread_mutex_lock(&_request_mx);
  while ([_request_queue count]==0)
    pthread_cond_wait(&_request_ready, &_request_mx);
  
  // get next request
  req= [_request_queue objectAtIndex:0];
  [_request_queue removeObjectAtIndex:0];
  
  pthread_mutex_unlock(&_request_mx);
  
  return req;
}


- (void)executeRequest:(Request*)request
{
  // execute request
  pthread_mutex_lock(&_mysql_mx);
  
  [request execute:_mysql];
  pthread_mutex_unlock(&_mysql_mx);
  
  // notify result ready
  [self performSelectorOnMainThread:@selector(handleFinished:)
                         withObject:request
                      waitUntilDone:YES];
}

- (void)enqueue: (Request*)req
{
  pthread_mutex_lock(&_request_mx);
  [_request_queue addObject:req];
  pthread_mutex_unlock(&_request_mx);
  
  pthread_cond_signal(&_request_ready);
}


- (void)handleFinished:(Request*)msg
{
  [self checkConnection];
  
  [msg finish];
  [msg release];
}


- (void)cancelRequest:(id)sender
{
  MYSQL *mysql= myx_mysql_init();
  MYX_USER_CONNECTION *conn= [_instance createUserConnection];
  
  if (myx_connect_to_instance(conn, mysql) == 0)
  {
    MYX_LIB_ERROR error;
    long long int affected_rows;
    char *sql= g_strdup_printf("kill %li", mysql_thread_id(_mysql));
    myx_query_execute_direct(mysql, sql, &error, &affected_rows);
    g_free(sql);
  }
  
  myx_free_user_connection_content(conn);
  g_free(conn);
  
  myx_mysql_close(mysql);
}


- (BOOL)waitForRequestToFinish:(WaitCallbackRequest*)req window:(NSWindow*)window message:(NSString*)msg
{  
  // if still didn't finish, then show a busy sheet
  if (!req->didFinish)
  {    
    MProgressPanel *panel= [[MProgressPanel alloc] initAndLoad];
    
    [panel setStopAction:@selector(cancelRequest:)];
    [panel setTarget:self];
    
    req->dialogVisible= YES;
    
    if (window)
      [panel centerOverWindow:window];
    [panel showWithTitle:msg];
    [NSApp runModalForWindow:[panel window]];
    
    [panel close];
    [panel release];
  }
  return YES;
}

@end

//===================================================

@implementation MMySQLDispatcher

static void *processRequests(void *arg)
{
  MMySQLDispatcher *disp= (MMySQLDispatcher*)arg;
  
  mysql_thread_init();
  
  for (;;)
  {
    Request *req;
    
    req= [disp getNextRequest];
    
    if (!req) // this signals a DIE!
      break;
    
    [disp executeRequest:req];
  }
  
  mysql_thread_end();
  
  return NULL;
}


- (id)initWithMySQL: (MYSQL*)mysql
           instance: (MConnectionInfo*)instance
{
  self= [super init];
  if (self)
  {
    _mysql= mysql;
    _instance= [instance retain];
    
    pthread_mutex_init(&_mysql_mx, NULL);
    pthread_mutex_init(&_request_mx, NULL);
    pthread_cond_init(&_request_ready, NULL);
    
    _request_queue= [[NSMutableArray alloc] init];
    
    pthread_create(&_tid, NULL, processRequests, self);
  }
  return self;
}


- (NSString*)lastMySQLError
{
  return [NSString stringWithUTF8String:mysql_error(_mysql)];
}

- (void)dealloc
{  
  pthread_mutex_destroy(&_mysql_mx);
  pthread_mutex_destroy(&_request_mx);
  pthread_cond_destroy(&_request_ready);
  
  [_instance release];
  [_request_queue release];
  
  [super dealloc];
}


- (void)performSelector:(SEL)selector
               argument:(id)argument
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target
{
  SelectorRequest *req= [[SelectorRequest alloc] init];
  
  req->arg= argument;
  req->selector= selector;
  req->finished= finished;
  req->finArgument= finArgument;
  req->target= target;
  
  [self enqueue:req];
}


- (void)performCallback:(void*(*)(MYSQL*))cback
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target
{
  CallbackRequest *req= [[CallbackRequest alloc] init];
  
  req->callback0= cback;
  req->finished= finished;
  req->target= target;
  req->finArgument= finArgument;
  [self enqueue:req];
}


- (void)performCallback:(void*(*)(MYSQL*,void*))cback
               argument:(void*)arg1
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target
{
  CallbackRequest *req= [[CallbackRequest alloc] init];
  
  req->callback1= cback;
  req->finished= finished;
  req->target= target;
  req->finArgument= finArgument;
  req->arg1= arg1;
  
  [self enqueue:req];
}


- (void)performCallback:(void*(*)(MYSQL*,void*,void*))cback
               argument:(void*)arg1
               argument:(void*)arg2
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target
{
  CallbackRequest *req= [[CallbackRequest alloc] init];
  
  req->callback2= cback;
  req->finished= finished;
  req->target= target;
  req->finArgument= finArgument;
  req->arg1= arg1;
  req->arg2= arg2;
  
  [self enqueue:req];
}


- (void)performCallback:(void*(*)(MYSQL*,void*,void*,void*))cback
               argument:(void*)arg1
               argument:(void*)arg2
               argument:(void*)arg3
       finishedSelector:(SEL)finished
               argument:(id)finArgument
                 target:(id)target
{
  CallbackRequest *req= [[CallbackRequest alloc] init];
  
  req->callback3= cback;
  req->finished= finished;
  req->target= target;
  req->finArgument= finArgument;
  req->arg1= arg1;
  req->arg2= arg2;
  req->arg3= arg3;
  
  [self enqueue:req];
}

- (void*)performCallback:(void*(*)(MYSQL*))cback
           waitForWindow:(id)window
                 message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  
  req->callback0= cback;
  
  [req retain];
  [self enqueue:req];
  
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  
  return req->result;
}

- (void*)performCallback:(void*(*)(MYSQL*,void*))cback
                argument:(void*)arg1
           waitForWindow:(id)window
                 message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  
  req->callback1= cback;
  req->arg1= arg1;
  
  [req retain];
  [self enqueue:req];
  
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  
  return req->result;
}

- (void*)performCallback:(void*(*)(MYSQL*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
           waitForWindow:(id)window
                 message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  
  req->callback2= cback;
  req->arg1= arg1;
  req->arg2= arg2;
  
  [req retain];
  [self enqueue:req];
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  return req->result;
}

- (void*)performCallback:(void*(*)(MYSQL*,void*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
                argument:(void*)arg3
           waitForWindow:(id)window
                 message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  
  req->callback3= cback;
  req->arg1= arg1;
  req->arg2= arg2;
  req->arg3= arg3;
  
  [req retain];
  [self enqueue:req];
  
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  
  return req->result;
}


- (void*)performCallback:(void*(*)(MYSQL*,void*,void*,void*,void*))cback
                argument:(void*)arg1
                argument:(void*)arg2
                argument:(void*)arg3
                argument:(void*)arg4
           waitForWindow:(id)window
                 message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  
  req->callback4= cback;
  req->arg1= arg1;
  req->arg2= arg2;
  req->arg3= arg3;
  req->arg4= arg4;
  
  [req retain];
  [self enqueue:req];
  
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  
  return req->result;
}


- (MYX_LIB_ERROR)performQuery:(NSString*)query
                waitForWindow:(id)window
                      message:(NSString*)message
{
  WaitCallbackRequest *req= [[WaitCallbackRequest alloc] init];
  MYX_LIB_ERROR error;
  
  req->callback2= (void*(*)(MYSQL*,void*,void*))myx_query_execute_direct;
  req->arg1= (void*)[query UTF8String];
  req->arg2= &error;
  
  [req retain];
  [self enqueue:req];
  
  [self waitForRequestToFinish:req window:window message:message];
  [req autorelease];
  
  return error;
}



- (BOOL)checkConnection
{
  int err= mysql_errno(_mysql);
  
  if (err == CR_SERVER_GONE_ERROR || err == CR_SERVER_LOST || _disconnected)
  {
    NSLog(@"lost connection to MySQL server");
    _disconnected= YES;
    return NO;
  }
  return YES;
}


- (BOOL)reconnect
{
  if (![_instance reconnect:_mysql])
  {
    return NO;
  }
  else
  {
    _disconnected= NO;
    NSRunAlertPanel(@"Information", @"Connection to MySQL server was lost but successfully restablished.", nil, nil, nil);
  }
  return YES;
}

@end
