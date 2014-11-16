//
//  MQScriptExecutionHelper.m
//  MySQL QueryBrowser
//
//  Created by Alfredo Kojima on 5/30/05.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MQScriptExecutionHelper.h"
#import <MySQLToolsCommon/MSourceTextEditor.h>
#import <MySQLToolsCommon/MSourceTextView.h>
#import <MySQLToolsCommon/MTextGutterView.h>
#import <MySQLToolsCommon/MSyntaxColoring.h>

#define WORKER_GO 1
#define WORKER_STOP 0

@implementation MQScriptExecutionHelper

- (id)initWithText:(MSourceTextEditor*)text
{
  self= [super init];
  if (self)
  {
    _text= text;
    _sqlText= myx_init_mysql_text();
    _sqlTextLock= [[NSLock alloc] init];
    
    _workerLock= [[NSConditionLock alloc] initWithCondition:WORKER_STOP];
  }
  return self;
}

- (void)dealloc
{
  [_workerLock release];
  [_sqlTextLock release];
  myx_free_sql_text(_sqlText);
  [super dealloc];
}

- (void)setConnection:(MYSQL*)mysql
{
  _mysql= mysql;
}

- (unsigned int)lineForStatement:(unsigned int)statement
{
  if (_sqlText && statement < _sqlText->stmts_num)
    return _sqlText->stmts[statement].stmt_begin_line;
  return (unsigned int)-1;
}

- (unsigned int)statementAtLine:(unsigned int)line
{
  unsigned int i;
  for (i=0; i < _sqlText->stmts_num; i++)
  {
    if (_sqlText->stmts[i].stmt_begin_line <= (int)line &&
        (int)line <= _sqlText->stmts[i].stmt_end_line)
      return i;
  }
  return (unsigned int)-1;
}

- (void)setPCPosition:(unsigned int)index
{
  MSourceTextView *textView= [_text textView];
  MSyntaxColoring *colorer= [textView syntaxColorer];
  
  [colorer removeMarker:@"pc" atLine:[self lineForStatement:_currentStatement]];
  
  if (index != (unsigned int)-1)
  {
    unsigned int line;
    NSPoint point;
    
    _currentStatement= index;
    line= [self lineForStatement:_currentStatement];
    [colorer addMarker:@"pc" atLine:line];
    
    point.x= 0;
    point.y= line * [[textView layoutManager] defaultLineHeightForFont:[textView font]] - NSHeight([(NSClipView*)[textView superview] documentVisibleRect])/2;
    point.y= MAX(0, point.y);
    [[textView superview] scrollPoint:point];
  }
  [_text setNeedsDisplay:YES];
}

- (void)reparseNearLines:(NSRange)lineRange
{
  if (!_reparseScheduled)
  {
    [self performSelector:@selector(doReparse:)
               withObject:nil
               afterDelay:0.3];
    _reparseScheduled= YES;
  }
#if 0
  int i;
  MSyntaxColoring *colorer= [[_text textView] syntaxColorer];
  int stmtBefore, stmtAfter;
    
  // find the last statement before the one edited
  stmtBefore= 0;
  for (i= 0; i < _sqlText->stmts_num; i++)
  {
    if (lineRange.location <= _sqlText->stmts[i].stmt_end_line)
    {
      stmtBefore= i > 0 ? i-1 : 0;
      break;
    }
  }
  
  // find the 1st statement after the one edited
  stmtAfter= _sqlText->stmts_num;
  for (i= _sqlText->stmts_num-1; i >= stmtBefore; --i)
  {
    if (NSMaxRange(lineRange) >= _sqlText->stmts[i].stmt_begin_line)
    {
      stmtAfter= i < stmtAfter ? i+1 : stmtAfter;
      break;
    }
  }
  
  NSLog(@"reanalyze from stmt %i to %i",  stmtBefore, stmtAfter);
#endif
}

 
- (void)doReparse:(id)obj
{
  unsigned int i;
  MSyntaxColoring *colorer= [[_text textView] syntaxColorer];
  _reparseScheduled= NO;

  [_sqlTextLock lock];
  myx_analyze_text(_sqlText, (char*)[[[[_text textView] textStorage] string] UTF8String]);
  [_sqlTextLock unlock];

  [colorer removeMarkerFromAllLines:@"statement"];
  for (i= 0; i < _sqlText->stmts_num; i++)
    [colorer addMarker:@"statement" atLine:_sqlText->stmts[i].stmt_begin_line];
  [[_text gutterView] setNeedsDisplay:YES];
}


- (void)workerLoop:(id)arg
{
  NSString *text= [[[_text textView] textStorage] string];

  mysql_thread_init();
  
  _workerState= WorkerWaiting;
  for (;;)
  {
    NSMutableDictionary *info= nil;
    MYX_MYSQL_ERROR_MSGS *msgs;
    
    // wait for main thread to give us the go ahead
    [_workerLock lockWhenCondition:WORKER_GO];
    _workerState= WorkerExecuting;
    
    [_workerLock unlockWithCondition:WORKER_STOP];
  
    if (_workerEnd)
      break;
    
    if (_stepping)
      [self performSelectorOnMainThread:@selector(notifyWorkerBusy:)
                             withObject:nil
                          waitUntilDone:NO];
  
    [_sqlTextLock lock];
    NSRange range= NSMakeRange(_sqlText->stmts[_currentStatement].stmt_begin_char, 
                               _sqlText->stmts[_currentStatement].stmt_end_char-_sqlText->stmts[_currentStatement].stmt_begin_char);
    char *query= g_strndup([text UTF8String]+range.location, range.length);
    [_sqlTextLock unlock];

    if (!g_str_has_prefix(query, "DELIMITER"))
    {
      if (myx_mysql_query(_mysql, query) != 0)
      {      
        info= [[NSMutableDictionary alloc] initWithObjectsAndKeys:
          [NSNumber numberWithInt:myx_mysql_errno(_mysql)], @"errno",
          [NSString stringWithUTF8String:mysql_error(_mysql)], @"error",
          [NSNumber numberWithInt:_sqlText->stmts[_currentStatement].stmt_begin_line], @"line",
          nil];
      }
      msgs= myx_mysql_error_msgs_fetch(_mysql);
      if (msgs)
      {
        if (!info)
          info= [[NSMutableDictionary alloc] initWithObjectsAndKeys:
            [NSNumber numberWithInt:_sqlText->stmts[_currentStatement].stmt_begin_line], @"line",
            nil];
        [info setObject:[NSValue valueWithPointer:msgs] forKey:@"msgs"];
      }
    }
    g_free(query);

    _workerState= WorkerWaiting;
    // tell the main thread that we're ready
    [self performSelectorOnMainThread:@selector(notifyWorkerDone:)
                           withObject:info
                        waitUntilDone:NO];
    
    if (info)
    {
      [info release];
      info= nil;
    }
  }
  _workerState= WorkerExiting;
  [self performSelectorOnMainThread:@selector(notifyWorkerDone:)
                         withObject:nil
                      waitUntilDone:NO];  
  
  mysql_thread_end();
}


- (void)changeState:(MQScriptState)newState
{
  _state= newState;
  [_delegate scriptChangedState:self state:_state];
}

- (void)notifyWorkerBusy:(NSDictionary*)userData
{
  [self changeState:ScriptExecuting];
}

- (void)reset
{
  [self setPCPosition:-1];
  [[[_text textView] syntaxColorer] removeMarkerFromAllLines:@"error"];
  _currentStatement= 0;
  _stepping= NO;
  [_errorHandler performSelector:_errorHandlerSelector
                      withObject:self
                      withObject:nil];  
}

- (void)notifyWorkerDone:(NSDictionary*)userData
{
  if (userData)
    [_errorHandler performSelector:_errorHandlerSelector
                        withObject:self
                        withObject:userData];
  
  if ([userData objectForKey:@"error"])
  {
    _stepping= YES;
    [self setPCPosition:_currentStatement];
    [[[_text textView] syntaxColorer] addMarker:@"error"
                                         atLine:[self lineForStatement:_currentStatement]];
    [self changeState:ScriptError];
    return;
  }
  
  // normal execution
  if (!_stepping && _workerState == WorkerWaiting)
  {
#define HAS_NEXT_STATEMENT(cur, limit, last)\
    ((cur) < (last) && ((limit)<0 || ((cur) < (limit))))
    if (HAS_NEXT_STATEMENT(_currentStatement, (unsigned int)_stopAtStatement, _sqlText->stmts_num-1))
    {
      if ([[[_text textView] syntaxColorer] marker:@"breakpoint"
                                            atLine:[self lineForStatement:_currentStatement+1]])
      {
        _stepping= YES;
        [self setPCPosition:_currentStatement+1];
        [self changeState:ScriptBreakpoint];
        return;
      }
      else
        _currentStatement++;
    }
    else
    {
      _workerEnd= YES;
      [self setPCPosition:-1];
      [self changeState:ScriptFinished];
    }
    [_workerLock unlockWithCondition:WORKER_GO];
  }
  else
  {
    if (_workerState == WorkerExiting)
    {
      [self setPCPosition:-1];
      [self changeState:_currentStatement == _sqlText->stmts_num-1 ? ScriptFinished : ScriptStopped];
    }
    else if (_currentStatement < _sqlText->stmts_num && _workerState == WorkerWaiting)
    { // stepping
      [self setPCPosition:_currentStatement+1];
      [self changeState:ScriptWaiting];
    }
    else
    {
      _workerEnd= YES;
      [self changeState:ScriptFinished];
    }
  }
}


- (void)setDelegate:(id)deleg
{
  _delegate= deleg;
}


- (void)setErrorHandler:(id)handler
               selector:(SEL)selector
{
  _errorHandler= handler;
  _errorHandlerSelector= selector;
}


- (IBAction)executeScript:(id)sender
{
  MSourceTextView *sourceText= [_text textView];
  unsigned int start;
  
  switch ([sender tag])
  {
    case 24:
    case 0: // Execute
    case 1:
      _stopAtStatement= -1;
      start= 0;
      _stepping= NO;
      break;
    case 2: // Execute Selected
    {
      NSRange lineRange= [[sourceText syntaxColorer] lineRangeForCharacterRange:[sourceText selectedRange]];
      
      start= [self statementAtLine:lineRange.location];
      _stopAtStatement= [self statementAtLine:NSMaxRange(lineRange)];
      _stepping= NO;
      break;
    }
    case 3: // Execute Stepping
      _stopAtStatement= -1;
      start= 0;
      _stepping= YES;
      break;
  }

  [[sourceText syntaxColorer] removeMarkerFromAllLines:@"pc"];

  if (!_sqlText || start >= _sqlText->stmts_num)
    return;

  if (_stepping)
    [self setPCPosition:start];
  else
  {
    _currentStatement= start;
    [self changeState:ScriptExecuting];
  }

  _workerEnd= NO;
  _workerState= WorkerStarting;
  
  [NSApplication detachDrawingThread:@selector(workerLoop:)
                            toTarget:self
                          withObject:nil];

  if ([[[_text textView] syntaxColorer] marker:@"breakpoint"
                                        atLine:[self lineForStatement:_currentStatement]]
      || _stepping)
  {
    _stepping= YES;
    [self setPCPosition:_currentStatement];
    [self changeState:_stepping ? ScriptWaiting : ScriptBreakpoint];
  }
  else
    [_workerLock unlockWithCondition:WORKER_GO];
}


- (IBAction)stepScript:(id)sender
{
  if (_currentStatement < _sqlText->stmts_num)
  {
    [_workerLock unlockWithCondition:WORKER_GO];
  }
}

- (IBAction)stopScript:(id)sender
{
  [self reset];
  _workerEnd= YES;
  [_workerLock unlockWithCondition:WORKER_GO];
}

- (IBAction)pauseScript:(id)sender
{
  _stepping= YES;
}

- (IBAction)continueScript:(id)sender
{
  [self setPCPosition:-1];
  _stepping= NO;
  [self changeState:ScriptExecuting];
  [_workerLock unlockWithCondition:WORKER_GO];
}

- (MQScriptState)state
{
  return _state;
}

@end
