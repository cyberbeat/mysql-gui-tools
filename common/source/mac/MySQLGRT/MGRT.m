//
//  MGRT.m
//  MySQL GRT
//
//  Created by Alfredo Kojima on 05/7/13.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MGRT.h"
#import "MShellTextView.h"
#import "MGRTModuleLoader.h"
#import <MySQLToolsCommon/myxutil.h>

#include "myx_grt_public_interface.h"
#include "myx_grt_builtin_module_public_interface.h"
#include "myx_grt_private.h"
#include "myx_grt_python.h"
#include "myx_grt_lua.h"
//#include "myx_grt_java.h"


#define FMT(s, ...) [NSString stringWithFormat:s,##__VA_ARGS__]

static NSString *makeErrorMessageFromResult(MYX_GRT_VALUE *result);

//#define SINGLE_THREAD


static MYX_GRT_VALUE *createArglist(NSArray *args)
{
  MYX_GRT_VALUE *arg, *argl;
  int i;
  
  argl= myx_grt_list_new(MYX_ANY_VALUE, NULL);
  
  for (i= 0; i < [args count]; i++)
  {
    id obj= [args objectAtIndex:i];
    if ([obj isKindOfClass:[NSString class]])
      arg= myx_grt_value_from_string([obj UTF8String]);
    else if ([obj isKindOfClass:[NSNumber class]])
    {
      if (strcmp([obj objCType], "f")==0)
        arg= myx_grt_value_from_real([obj floatValue]);
      else if (strcmp([obj objCType], "d")==0)
        arg= myx_grt_value_from_real([obj doubleValue]);
      else
        arg= myx_grt_value_from_int([obj intValue]);
    }
    else if ([obj isKindOfClass:[NSValue class]])
      arg= myx_grt_value_retain([obj pointerValue]);
    else if ([obj isKindOfClass:[NSGRTValue class]])
      arg= myx_grt_value_retain([obj grtValue]);
    else if ([obj isKindOfClass:[NSArray class]])
      arg= createArglist(obj);
    else
      continue;
    myx_grt_list_item_add(argl, arg);
    myx_grt_value_release(arg);
  }
  return argl;
}

@interface MGRTRequest : NSObject
{
  char *module;
  char *function;
  MYX_GRT_VALUE *args;
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *result;
}
- (void)performWithGRT:(MYX_GRT*)grt;
- (MYX_GRT_VALUE*)result;
- (MYX_GRT_ERROR)error;
@end

@implementation MGRTRequest
- (id)initWithModule:(const char*)mod
            function:(const char*)func
           arguments:(NSArray*)argl
{
  self= [super init];
  if (self)
  {
    module= g_strdup(mod);
    function= g_strdup(func);
    args= createArglist(argl);
  }
  return self;
}

- (void)dealloc
{
  g_free(module);
  g_free(function);
  myx_grt_value_release(args);
  myx_grt_value_release(result);
  [super dealloc];
}


- (MYX_GRT_VALUE*)result
{
  return result;
}


- (MYX_GRT_ERROR)error
{
  return error;
}


- (void)finish
{
}


- (void)performWithGRT:(MYX_GRT*)grt
{
  result= myx_grt_function_get_and_call(grt,
                                        module,
                                        function,
                                        NO,
                                        args,
                                        &error);
  [self finish];
}
@end


@interface MGRTBlockingRequest : MGRTRequest
{
  BOOL ready;
}

- (void)wait:(BOOL)runloop;
@end


@implementation MGRTBlockingRequest
- (id)initWithModule:(const char*)mod
            function:(const char*)func
           arguments:(NSArray*)argl
{
  self= [super initWithModule:mod function:func arguments:argl];
  if (self)
  {
    ready= NO;
  }
  return self;
}


- (void)finish
{
  ready= YES;
}


- (void)wait:(BOOL)runloop
{
  //NSLog(@"waiting %s.%s", module, function);
  while (!ready)
  {
    NSDate* next = [NSDate dateWithTimeIntervalSinceNow:0.1];
    
    if (runloop)
      [[NSRunLoop currentRunLoop] runUntilDate:next];
  }
}

@end



@interface MGRTAsyncRequest : MGRTRequest
{
  id target;
  id userData;
  SEL action;
  MGRT *grt;
}
- (void)setGRT:(MGRT*)grt;
- (void)setTarget:(id)obj;
- (void)setAction:(SEL)selec;
- (void)setUserData:(id)data;
@end

@implementation MGRTAsyncRequest
- (void)dealloc
{
  [userData release];
  [super dealloc];
}

- (void)setTarget:(id)obj
{
  target= obj;
}

- (void)setAction:(SEL)selec
{
  action= selec;
}

- (void)setUserData:(id)data
{
  userData= [data retain];
}

- (void)setGRT:(MGRT*)grt_
{
  grt= grt_;
}

- (void)finish
{
  NSDictionary *data;
  
  if (error != MYX_GRT_NO_ERROR || [grt resultIsError:result])
  {
    if (result)
      data= [NSDictionary dictionaryWithObjectsAndKeys:
        [NSGRTValue grtValueWithValue:myx_grt_dict_item_get_value(result,"error")], @"error",
        [NSNumber numberWithInt:error], @"errorCode",
        makeErrorMessageFromResult(result), @"message",
        userData, @"userData", // has to be the last item, as userData can be nil
        nil];
    else
      data= [NSDictionary dictionaryWithObjectsAndKeys:
        [NSNumber numberWithInt:error], @"errorCode",
        @"function returned NULL result", @"message",
        userData, @"userData", // has to be the last item, as userData can be nil
        nil];    
  }
  else
  {
    id tmp;
    
    if (result)
      tmp= [NSGRTValue grtValueWithValue:myx_grt_dict_item_get_value(result,"value")];
    else
      tmp= [NSNull null];
    
    data= [NSDictionary dictionaryWithObjectsAndKeys:
      tmp, @"result",
      userData, @"userData", // has to be the last item, as userData can be nil
      nil];
  }
  [target performSelectorOnMainThread:action
                           withObject:data
                        waitUntilDone:YES];
}
@end



@implementation NSGRTValue
+ (NSGRTValue*)grtValueWithValue:(MYX_GRT_VALUE*)value
{
  return [[[NSGRTValue alloc] initWithGRTValue:value] autorelease];
}

- (id)initWithGRTValue:(MYX_GRT_VALUE*)value
{
  self= [super init];
  if (self)
  {
    _value= myx_grt_value_retain(value);
  }
  return self;
}

- (MYX_GRT_VALUE*)grtValue
{
  return _value;
}

- (void)dealloc
{
  myx_grt_value_release(_value);
  [super dealloc];
}
@end




@implementation MGRT

+ (NSString*)errorText:(MYX_GRT_ERROR)error
{
  switch (error)
  {
    case MYX_GRT_NO_ERROR: return @"Success";
    case MYX_GRT_INTERNAL_ERROR: return @"Internal error";
    case MYX_GRT_BAD_PATH: return @"Invalid path";
    case MYX_GRT_CANT_OPEN_FILE: return @"Cannot open file";
    case MYX_GRT_BAD_FUNCTION: return @"Invalid function";
    case MYX_GRT_DUPLICATE_ENTRY: return @"Duplicate entry";
    case MYX_GRT_BAD_VALUE: return @"Bad value";
    case MYX_GRT_BAD_DATA: return @"Bad data";

    case MYX_GRT_VALIDATION_ERROR: return @"Validation error";
    case MYX_GRT_FUNCTION_CALL_ERROR: return @"Function call error";
    case MYX_GRT_MODULE_INIT_ERROR: return @"Module init error";
    case MYX_GRT_BAD_MODULE: return @"Bad module";
    case MYX_GRT_UNKNOWN_MODULE_TYPE: return @"Unknown module type";
    
    case MYX_GRT_JAVA_NOT_FOUND: return @"Java Runtime not found";
    case MYX_GRT_JAVA_REGISTRY_CORRUPTED: return @"Java registry corrupted";
    case MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED: return @"JRE cannot be loaded";
  }
  return nil;
}


static void process_grt_output(const char *text, void *user_data)
{
  MGRT *grt= (MGRT*)user_data;
  if (text && *text)
    [grt outText:[NSString stringWithUTF8String:text]];
}


static void process_grt_message(MYX_GRT_MSGS *msgs, void *user_data)
{
  MGRT *grt= (MGRT*)user_data;
  unsigned int i;
  
  if (msgs)
  {
    for (i= 0; i < msgs->msgs_num; i++)
    {
      NSString *type= nil;
      switch (msgs->msgs[i].msg_type)
      {
        case 2: type= @"ERROR"; break;
        case 1: type= @"WARNING"; break;
        case 0: type= @"MESSAGE"; break;
        case -1: type= @"PROGRESS"; break;
      }
      if (!type)
        continue;
      if (msgs->msgs[i].msg_detail)
      {
        unsigned int j;
        NSString *msg= NSStr(msgs->msgs[i].msg);
        for (j= 0; j < msgs->msgs[i].msg_detail->strings_num; j++)
          msg= [msg stringByAppendingFormat:@"    %s\n", msgs->msgs[i].msg_detail->strings[j]];

        [grt outMessage:msg type:type];
      }
      else
        [grt outMessage:NSStr(msgs->msgs[i].msg) type:type];
    }
  }
}


static int process_grt_input(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data)
{
  MGRT *grt= (MGRT*)user_data;
  NSString *result;
  
  result= [grt askInput:NSStr(caption) asPassword:options&MYX_GR_IO_PASSWORD];
  
  *text= [result UTF8String];
  
  return result != nil ? 0 : -1;
}


static int copy_to_clipboard(const char *text, void *data)
{
  [[NSPasteboard generalPasteboard] declareTypes:[NSArray arrayWithObject:NSStringPboardType]
                                           owner:nil];

  [[NSPasteboard generalPasteboard] setString:[NSString stringWithUTF8String:text]
                                      forType:NSStringPboardType];
  return 0;
}


- (id)init 
{
  static int globalInit= 0;
  
  if (!globalInit)
  {
    globalInit= 1;
    myx_grt_init_threads();
    myx_grt_module_base_set_copy_to_clipboard_callback(copy_to_clipboard, NULL);
  }
  
  self = [super init];
  if (self != nil) 
  {
    _grt= myx_grt_initialize(0);
    _requestQueue= [[NSMutableArray alloc] init];
    _requestQueueLock= [[NSLock alloc] init];
    MPCreateSemaphore(1000000, 0, &_requestReady);
  }
  return self;
}



- (void)initializeGRTThread:(NSString*)resourcePath
{
  MYX_GRT_ERROR error;
  MYX_GRT_MODULE_LOADER *loader;
  int i;
 
  myx_grt_set_output_callback(_grt, self, process_grt_output);
  myx_grt_set_message_callback(_grt, self, process_grt_message);
  myx_grt_set_input_callback(_grt, self, process_grt_input);
  
  myx_grt_setup_shell(_grt, MYX_GRT_SHELL_LUA);
  
  myx_grt_shell_print_welcome(_grt);
  
  [self scanStructsInPath:FMT(@"%@/xml",resourcePath)];

  // loaders
  myx_register_builtin_grt_module_base(_grt);
  myx_register_builtin_grt_module_reverse_engineer_mysql(_grt);
  myx_register_builtin_grt_module_transformation_mysql(_grt);
  
#ifdef ENABLE_JAVA_MODULES
  // java modules
  [self outText:@"Loading Java modules..."];
  loader= myx_java_init_loader(_grt, NULL, &error, NULL, 
                               [[NSString stringWithFormat:@"%@/", resourcePath] fileSystemRepresentation]);
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      [self outText:@"Error registering Java module loader\n"];
    else
    {
      i= myx_grt_scan_for_modules(_grt, [[NSString stringWithFormat:@"%@/java/com/mysql/grt/modules",resourcePath] fileSystemRepresentation], &error);
      if (error != MYX_GRT_NO_ERROR)
        [self outText:FMT(@"Error scanning Java modules (%i)\n", error)];
      else
        [self outText:FMT(@"Registered %i Java modules\n", i)];
    }
  }
  else
    [self outText:FMT(@"Error initializing Java module loader (%i)\n",error)];
#endif

#ifdef ENABLE_PHP_MODULES
  // php modules
  [self outText:@"Loading PHP modules..."];
  loader= myx_php_init_loader(_grt, &error);
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      [self outText:@"Error registering PHP module loader\n"];
    else
    {
      i= myx_grt_scan_for_modules(_grt, [[NSString stringWithFormat:@"%@/php/modules",resourcePath] fileSystemRepresentation], &error);
      if (error != MYX_GRT_NO_ERROR)
        [self outText:@"Error scanning PHP modules (%i)\n", error];
      else
        [self outText:@"Registered %i PHP modules\n", i];
    }
  }
  else
    [self outText:@"Error initializing PHP module loader (%i)\n",error];
#endif
  
  // lua modules
  [self outText:@"Loading Lua modules..."];
  loader= myx_lua_init_loader(_grt, &error, [FMT(@"%@/lua",resourcePath) fileSystemRepresentation]);
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      [self outText:@"Error registering Lua module loader\n"];
    else
    {
      i= myx_grt_scan_for_modules(_grt, [FMT(@"%@/lua",resourcePath) fileSystemRepresentation], &error);
      if (error != MYX_GRT_NO_ERROR)
        [self outText:FMT(@"Error scanning Lua modules (%i)\n", error)];
      else
        [self outText:FMT(@"Registered %i Lua modules\n", i)];
    }
  }
  else
    [self outText:FMT(@"Error initializing Lua module loader (%i)\n",error)];

  // objc module
  [self outText:@"Initializing Objective C module loader\n"];
  _objcLoader= [[MGRTModuleLoader alloc] initWithGRT:_grt];
  loader= [_objcLoader initLoader];
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      [self outText:@"Error registering ObjC module loader\n"];
  }
  else
    [self outText:FMT(@"Error initializing ObjC module loader (%i)\n",error)];
  
  
  myx_grt_shell_init(_grt);
}


- (MYX_GRT_VALUE*)globalAppDict
{
  MYX_GRT_VALUE *result;
  
  result= [self globalValue:"/app"];
  if (!result)
  {
    result= myx_grt_dict_new(_grt, "base.ApplicationData");
    [self setGlobalValue:result forPath:"/app"];
  }
  return result;
}


- (NSString*)stringAppOption:(NSString*)option
{
  MYX_GRT_VALUE *value= [self globalValue:[[NSString stringWithFormat:@"/app/options/%@",option] UTF8String]];
  
  if (value && myx_grt_value_get_type(value) == MYX_STRING_VALUE)
    return [NSString stringWithUTF8String:myx_grt_value_as_string(value)];
  return nil;
}


- (int)intAppOption:(NSString*)option
{
  MYX_GRT_VALUE *value= [self globalValue:[[NSString stringWithFormat:@"/app/options/%@",option] UTF8String]];
  
  if (value && myx_grt_value_get_type(value) == MYX_INT_VALUE)
    return [NSString stringWithUTF8String:myx_grt_value_as_int(value)];
  return 0;
}


- (BOOL)saveSubtree:(const char*)path
             toFile:(NSString*)file
{
  MYX_GRT_VALUE *subtree= [self globalValue:path];
  if (!subtree || myx_grt_store_to_file(_grt,
                                        subtree,
                                        [file fileSystemRepresentation]) == MYX_GRT_NO_ERROR)
    return YES;
  return NO;
}


- (BOOL)loadSubtree:(const char*)path
           fromFile:(NSString*)file
{
  MYX_GRT_VALUE *dict;
  
  dict= myx_grt_retrieve_from_file(_grt, [file fileSystemRepresentation]);
  
  if (dict)
  {
    [self setGlobalValue:dict forPath:path];
    myx_grt_value_release(dict);
    return YES;
  }
  return NO;
}


- (void)grtThread:(NSArray*)args
{
  NSString *resourcePath= [args objectAtIndex:0];
  MPSemaphoreID ready= [[args objectAtIndex:1] pointerValue];
  NSAutoreleasePool *pool;
  
  pool= [[NSAutoreleasePool alloc] init];
  [self initializeGRTThread:resourcePath];
  [pool release];

  // signal the main thread that initialization is done
  MPSignalSemaphore(ready);
  
  for (;;)
  {
    MGRTRequest *req;
    
    MPWaitOnSemaphore(_requestReady, kDurationForever);
  
    pool= [[NSAutoreleasePool alloc] init];
    
    [_requestQueueLock lock];
    req= [[_requestQueue objectAtIndex:0] retain];
    [_requestQueue removeObjectAtIndex:0];
    [_requestQueueLock unlock];

    [req performWithGRT:_grt];
    
    [req release];

    [pool release];
  }
}


- (void)initializeGRT:(NSString*)resourcePath
{
  MPSemaphoreID workerReady;
  
  MPCreateSemaphore(1, 0, &workerReady);
  
#ifdef SINGLE_THREAD
  [self initializeGRTThread:resourcePath];
#else
  [NSThread detachNewThreadSelector:@selector(grtThread:)
                           toTarget:self
                         withObject:[NSArray arrayWithObjects:
                           resourcePath, [NSValue valueWithPointer:workerReady], nil]];
  // wait for the thread to initialize the GRT
  MPWaitOnSemaphore(workerReady, kDurationForever);
  MPDeleteSemaphore(workerReady);
#endif
  
  [_console makeReady];
}


- (void)dealloc
{
  MPDeleteSemaphore(_requestReady);
  [_objcLoader release];
  if (_grt)
    myx_grt_finalize(_grt);
  [super dealloc];
}


- (MYX_GRT*)grt
{
  return _grt;
}


- (void)setConsole:(MShellTextView*)textView
{
  _console= textView;
}


- (void)outText:(NSString*)text
{
  //NSLog(@"GRT> %@", text);
  if (_outputHandler)
    [_outputHandler performSelector:_outputSelector withObject:text];
  else
    [_console appendText:text];
}


- (NSString*)askInput:(NSString*)caption asPassword:(BOOL)password
{
  NSMutableDictionary *options= [NSMutableDictionary dictionaryWithObjectsAndKeys:
    caption, @"caption",
    [NSNumber numberWithBool:password], @"password", nil];

  if (_inputHandler)
  {
    [_inputHandler performSelectorOnMainThread:_inputSelector
                                    withObject:options 
                                 waitUntilDone:YES
                                         modes:[NSArray arrayWithObjects:NSDefaultRunLoopMode,NSModalPanelRunLoopMode,nil]];
    return [options objectForKey:@"result"];
  }
  else
    NSLog(@"input handler not set");
  return nil;
}


- (void)outMessage:(NSString*)text type:(NSString*)type
{
  NSLog(@"GRT:%@> %@", type, text);
  if (_messageHandler)
    [_messageHandler performSelector:_messageSelector withObject:text];
  else
    [_console appendText:[NSString stringWithFormat:@"%@: %@", type, text]];
}


- (void)performShellCommand:(NSString*)command
{
  MYX_GRT_SHELL_COMMAND error;

  error= myx_grt_shell_execute(_grt, [command UTF8String]);
  //if (error != MYX_GRT_SHELL_
  
  myx_grt_messages_stack_flush(_grt, 0);
}

- (NSString*)shellPrompt
{
  return [NSString stringWithUTF8String:myx_grt_shell_get_prompt(_grt)];
}


- (BOOL)scanModulesInPath:(NSString*)path
{
  MYX_GRT_ERROR error;
  int i;
  
  i= myx_grt_scan_for_modules(_grt, [path fileSystemRepresentation], &error);
  if (error != MYX_GRT_NO_ERROR)
  {
    [self outText:FMT(@"Error scanning for modules in %@ (%i)\n", path, error)];
    return NO;
  }
  else
  {
    [self outText:FMT(@"Registered %i modules\n", i)];
    return YES;
  }
}

- (BOOL)scanStructsInPath:(NSString*)path
{
  int i;
  MYX_GRT_ERROR error;
  
  i= myx_grt_scan_for_structs(_grt, [path fileSystemRepresentation],
                              &error);
  if (error != MYX_GRT_NO_ERROR)
  {
    [self outText:FMT(@"Error loading struct definition files from %@ (%i)\n",path,error)];
    return NO;
  }
  else
  {
    if (i == 1)
      [self outText:@"Registered one struct definition\n"];
    else
      [self outText:[NSString stringWithFormat:@"Registered %i struct definitions\n",i]];
    return YES;
  }
}

- (void)queueRequest:(MGRTRequest*)request
{
  [_requestQueueLock lock];
  [_requestQueue addObject:request];
  [_requestQueueLock unlock];
  
  MPSignalSemaphore(_requestReady);
}


static NSString *makeErrorMessageFromResult(MYX_GRT_VALUE *result)
{
  if (result)
  {
    const char *error= myx_grt_dict_item_get_as_string(result, "error");
    const char *detail= myx_grt_dict_item_get_as_string(result, "detail");
    if (detail)
      return [[NSString stringWithFormat:@"%s (%s)", error, detail] retain];
    else
      return [NSStr(error) retain];
  }
  return nil;
}


- (BOOL)performModule:(NSString*)module
            procedure:(NSString*)procedure
            arguments:(NSArray*)args
{
  MGRTBlockingRequest *request;
  MYX_GRT_VALUE *result;
  MYX_GRT_ERROR error;

  _lastError= MYX_GRT_NO_ERROR;
  [_lastErrorMessage release];
  _lastErrorMessage= nil;
  
  request= [[MGRTBlockingRequest alloc] initWithModule:[module UTF8String]
                                              function:[procedure UTF8String]
                                             arguments:args];

#ifdef SINGLE_THREAD
  [request performWithGRT:_grt];
#else
  [self queueRequest:request];

  [request wait:NO];
#endif
  result= [request result];
  error= [request error];
  
  if (result) myx_grt_value_retain(result);
  
  [request release];
  
  myx_grt_messages_stack_flush(_grt, 0);

  if ((result == NULL || ![self resultIsError:result]) && error == MYX_GRT_NO_ERROR)
  {
    if (result) myx_grt_value_release(result);
    return YES;
  }
  else
  {
    _lastError= error;
    _lastErrorMessage= makeErrorMessageFromResult(result);
    
    if (result)
      [self outText:[NSString stringWithFormat:@"%@ calling %@.%@: %s", 
         error == MYX_GRT_NO_ERROR ? @"Error" : [MGRT errorText:error],
        module, procedure,
        myx_grt_dict_item_get_as_string(result, "error")?:""]];
    else
      [self outText:[NSString stringWithFormat:@"%@ calling %@.%@", 
         error == MYX_GRT_NO_ERROR ? @"Error" : [MGRT errorText:error],
        module, procedure]];
    if (result)
      myx_grt_value_release(result);
    return NO;
  }
}


- (MYX_GRT_VALUE*)performModule:(NSString*)module
                       function:(NSString*)function
                      arguments:(NSArray*)args
{
  MYX_GRT_VALUE *result;
  MYX_GRT_ERROR error;
  MGRTBlockingRequest *request;

  _lastError= MYX_GRT_NO_ERROR;
  [_lastErrorMessage release];
  _lastErrorMessage= nil;
  
  //NSLog(@"calling module function %@:%@", module, function);
  
  request= [[MGRTBlockingRequest alloc] initWithModule:[module UTF8String]
                                              function:[function UTF8String]
                                             arguments:args];

#ifdef SINGLE_THREAD
  [request performWithGRT:_grt];
#else
  [self queueRequest:request];
  
  [request wait:YES];
#endif
  result= [request result];
  error= [request error];
  
  if (result) myx_grt_value_retain(result);
  
  [request release];
  
  myx_grt_messages_stack_flush(_grt, 0);
  
  if (![self resultIsError:result] && error == MYX_GRT_NO_ERROR)
  {
    if (result)
    {
      MYX_GRT_VALUE *tmp= myx_grt_dict_item_get_value(result, "value");
      myx_grt_value_retain(tmp);
      myx_grt_value_release(result);
      return tmp;
    }
    return NULL;
  }
  else
  {
    _lastError= error;
    _lastErrorMessage= makeErrorMessageFromResult(result);

    if (result)
      [self outText:[NSString stringWithFormat:@"%@ calling %@.%@: %s", 
         error == MYX_GRT_NO_ERROR ? @"Error" : [MGRT errorText:error],
        module, function,
        myx_grt_dict_item_get_as_string(result, "error")]];
    else
      [self outText:[NSString stringWithFormat:@"%@ calling %@.%@", 
         error == MYX_GRT_NO_ERROR ? @"Error" : [MGRT errorText:error],
        module, function]];
    if (result)
      myx_grt_value_release(result);
    return NULL;
  }
}


- (BOOL)performAsyncModule:(NSString*)module
                  function:(NSString*)function
                 arguments:(NSArray*)args
          finishedSelector:(SEL)sel
                    target:(id)target
                  userData:(id)data
{
  MGRTAsyncRequest *request;
  
  _lastError= MYX_GRT_NO_ERROR;
  [_lastErrorMessage release];
  _lastErrorMessage= nil;
  
  //NSLog(@"calling async module function %@:%@", module, function);
  
  request= [[MGRTAsyncRequest alloc] initWithModule:[module UTF8String]
                                           function:[function UTF8String]
                                          arguments:args];
  [request setAction:sel];
  [request setTarget:target];
  [request setUserData:data];
  
  [self queueRequest:request];
  [request release];

  return YES;
}


- (int)performModule:(NSString*)module
         intFunction:(NSString*)function
           arguments:(NSArray*)args
{
  MYX_GRT_VALUE *result;
  
  result= [self performModule:module function:function arguments:args];
  
  if (result)
  {
    int tmp= myx_grt_value_as_int(result);
    myx_grt_value_release(result);
    return tmp;
  }
  return -1;
}


- (NSString*)performModule:(NSString*)module
            stringFunction:(NSString*)function
                 arguments:(NSArray*)args
{
  MYX_GRT_VALUE *result;
  
  result= [self performModule:module function:function arguments:args];
  
  if (result)
  {
    MYX_GRT_VALUE *result_as_string= myx_grt_value_as_string(result);
    if(result_as_string)
    {
      NSString *tmp= [NSString stringWithUTF8String: result_as_string];
      myx_grt_value_release(result);
      return tmp;
    }
  }
  return nil;  
}


- (MYX_GRT_ERROR)lastError
{
  return _lastError;
}


- (NSString*)lastErrorDescription
{
  return _lastErrorMessage;
}


- (BOOL)resultIsError:(MYX_GRT_VALUE*)result
{
  if (result && myx_grt_dict_item_get_value(result, "error"))
    return YES;
  return NO;
}


- (void)reportErrorResult:(MYX_GRT_VALUE*)result
{
  MYX_GRT_VALUE *value;
  value= myx_grt_dict_item_get_value(result, "error");
  myx_grt_value_print(_grt, value);
}


- (void)reportError:(MYX_GRT_ERROR)error
{
  [self outText:[NSString stringWithFormat:@"%@", 
    [MGRT errorText:error]]];
}



- (NSGRTValue*)globalGRTValue:(const char*)path
{
  return [NSGRTValue grtValueWithValue:[self globalValue:path]];
}


- (NSGRTValue*)globalGRTRefValue:(const char*)path
{
  return [NSGRTValue grtValueWithValue:[self globalRefValue:path]];
}


- (MYX_GRT_VALUE*)globalValue:(const char*)path
{
  return myx_grt_dict_item_get_by_path(_grt, myx_grt_get_root(_grt), path);
}


- (MYX_GRT_VALUE*)globalRefValue:(const char*)path
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_by_path(_grt, myx_grt_get_root(_grt), path);;
  if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
    return myx_grt_reference_cache_lookup(_grt, myx_grt_value_as_string(value));
  else
    return value;
}


- (void)setGlobalValue:(MYX_GRT_VALUE*)value forPath:(const char*)path
{
  char prefix[strlen(path)+1];
  char *ptr;
  MYX_GRT_VALUE *dict;
  
  if (strcmp(path, "/")==0)
  {
    myx_grt_set_root(_grt, value);
    return;
  }
  
  strcpy(prefix, path);
  ptr= strrchr(prefix, '/');
  if (ptr == prefix)
    dict= myx_grt_dict_item_get_by_path(_grt, myx_grt_get_root(_grt), "/");
  else
  {
    if (ptr) *ptr= 0;
    dict= myx_grt_dict_item_get_by_path(_grt, myx_grt_get_root(_grt), prefix);
  }
  if (dict && dict->type == MYX_STRING_VALUE)
    dict= myx_grt_reference_cache_lookup(_grt, myx_grt_value_as_string(dict));
  if (dict)
    myx_grt_dict_item_set_value(dict, ptr+1, value);
}

- (MGRTModuleLoader*)objCLoader
{
  return _objcLoader;
}


- (void)setOutputHandler:(id)object
                selector:(SEL)selector
{
  _outputHandler= object;
  _outputSelector= selector;  
}


- (void)setInputHandler:(id)object
               selector:(SEL)selector
{
  _inputHandler= object;
  _inputSelector= selector;  
}


- (void)resetInputHandler
{
  _inputHandler= nil;
  _inputSelector= nil;
}


- (void)resetOutputHandler
{
  _outputHandler= nil;
  _outputSelector= nil;
}

- (void)setMessageHandler:(id)object
                 selector:(SEL)selector
{
  _messageHandler= object;
  _messageSelector= selector;
}


- (void)resetMessageHandler
{
  _messageHandler= nil;
  _messageSelector= nil;
}

@end
