//
//  MQScriptExecutionHelper.h
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/30/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"

@class MSourceTextEditor;

typedef enum MQScriptState {
  ScriptIdle,
  ScriptExecuting,
  ScriptWaiting,
  ScriptBreakpoint,
  ScriptError,
  ScriptFinished,
  ScriptStopped
} MQScriptState;


typedef enum WorkerState {
  WorkerStarting,
  WorkerWaiting,
  WorkerExecuting,
  WorkerAllFinished,
  WorkerExiting
} WorkerState;


@interface MQScriptExecutionHelper : NSObject 
{
  NSLock *_sqlTextLock;
  MYX_SQL_TEXT *_sqlText;
  MSourceTextEditor *_text;
  
  MYSQL *_mysql;
  unsigned int _currentStatement;
  int _stopAtStatement;

  id _errorHandler;
  SEL _errorHandlerSelector;
  
  id _delegate;
  
  MQScriptState _state;
  
  NSConditionLock *_workerLock;
  WorkerState _workerState;
  BOOL _workerEnd;
  
  BOOL _reparseScheduled;

  BOOL _stepping;
  BOOL _stepOK;
}

- (id)initWithText:(MSourceTextEditor*)text;
- (void)reparseNearLines:(NSRange)lineRange;

- (void)setConnection:(MYSQL*)mysql;

- (void)setDelegate:(id)deleg;
- (void)setErrorHandler:(id)handler
               selector:(SEL)selector;

- (IBAction)executeScript:(id)sender;
- (IBAction)stepScript:(id)sender;
- (IBAction)stopScript:(id)sender;
- (IBAction)continueScript:(id)sender;
- (IBAction)pauseScript:(id)sender;

- (MQScriptState)state;
@end
