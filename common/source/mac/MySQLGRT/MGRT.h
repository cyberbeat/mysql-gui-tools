//
//  MGRT.h
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#include <MySQLGRT/myx_grt_public_interface.h>


@class MShellTextView;
@class MGRTModuleLoader;

@interface NSGRTValue : NSObject
{
  MYX_GRT_VALUE *_value;
}
+ (NSGRTValue*)grtValueWithValue:(MYX_GRT_VALUE*)value;
- (id)initWithGRTValue:(MYX_GRT_VALUE*)value;
- (MYX_GRT_VALUE*)grtValue;
@end


@interface MGRT : NSObject 
{
  MYX_GRT *_grt;
  
  MGRTModuleLoader *_objcLoader;
  
  NSMutableArray *_requestQueue;
  NSLock *_requestQueueLock;
  MPSemaphoreID _requestReady;

  MShellTextView *_console;

  id _outputHandler;
  SEL _outputSelector;
  id _inputHandler;
  SEL _inputSelector;
  
  id _messageHandler;
  SEL _messageSelector;

  MYX_GRT_ERROR _lastError;
  NSString *_lastErrorMessage;
}

- (MYX_GRT*)grt;
- (void)initializeGRT:(NSString*)resourcePath;
- (void)setConsole:(MShellTextView*)textView;
- (void)outText:(NSString*)text;
- (NSString*)askInput:(NSString*)caption asPassword:(BOOL)password;
- (void)outMessage:(NSString*)text type:(NSString*)type;
- (void)performShellCommand:(NSString*)command;
- (NSString*)shellPrompt;

- (MGRTModuleLoader*)objCLoader;

- (NSGRTValue*)globalGRTValue:(const char*)path;
- (NSGRTValue*)globalGRTRefValue:(const char*)path;

- (MYX_GRT_VALUE*)globalValue:(const char*)path;
- (MYX_GRT_VALUE*)globalRefValue:(const char*)path;
- (void)setGlobalValue:(MYX_GRT_VALUE*)value forPath:(const char*)path;

- (BOOL)saveSubtree:(const char*)path
             toFile:(NSString*)file;
- (BOOL)loadSubtree:(const char*)path
           fromFile:(NSString*)file;
- (MYX_GRT_VALUE*)globalAppDict;
- (NSString*)stringAppOption:(NSString*)option;
- (int)intAppOption:(NSString*)option;

- (BOOL)scanStructsInPath:(NSString*)path;
- (BOOL)scanModulesInPath:(NSString*)path;
- (BOOL)resultIsError:(MYX_GRT_VALUE*)value;

- (BOOL)performModule:(NSString*)module
            procedure:(NSString*)procedure
            arguments:(NSArray*)args;

- (MYX_GRT_VALUE*)performModule:(NSString*)module
                       function:(NSString*)function
                      arguments:(NSArray*)args;

- (BOOL)performAsyncModule:(NSString*)module
                  function:(NSString*)function
                 arguments:(NSArray*)args
          finishedSelector:(SEL)sel
                    target:(id)target
                  userData:(id)data;

- (int)performModule:(NSString*)module
         intFunction:(NSString*)function
           arguments:(NSArray*)args;

- (NSString*)performModule:(NSString*)module
            stringFunction:(NSString*)function
                 arguments:(NSArray*)args;

- (void)setInputHandler:(id)object
               selector:(SEL)selector;
- (void)resetInputHandler;

- (void)setOutputHandler:(id)object
                selector:(SEL)selector;
- (void)resetOutputHandler;

- (void)setMessageHandler:(id)object
                 selector:(SEL)selector;
- (void)resetMessageHandler;

- (void)reportError:(MYX_GRT_ERROR)error;

- (MYX_GRT_ERROR)lastError;
- (NSString*)lastErrorDescription;

- (BOOL)resultIsError:(MYX_GRT_VALUE*)result;
- (void)reportErrorResult:(MYX_GRT_VALUE*)result;

+ (NSString*)errorText:(MYX_GRT_ERROR)error;

@end
