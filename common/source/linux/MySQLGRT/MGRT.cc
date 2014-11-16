/* Copyright (C) 2005 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */


#include "MGRT.h"
#include "myx_grt_builtin_module_public_interface.h"
#include "myx_grt_private.h"
#include <gtkmm/main.h>
#include "MGShellView.h"
/**
 * @file  MGRT.cc
 * @brief
 */

static Glib::ustring make_error_message_from_result(const MGRTValue &result)
{
  if (result.isValid())
  {
    const char *error= result["error"].isValid() ? result["error"].asString() : 0;
    const char *detail= result["detail"].isValid() ? result["detail"].asString() : 0;
    if (detail)
      return ufmt("%s (%s)", error, detail);
    else
      return error;
  }
  return "";
}


void MGRT::Request::set_output_handler(const OutputHandlerSlot &handler)
{
  _outputHandler= handler;
}

    
void MGRT::Request::set_message_handler(const MessageHandlerSlot &handler)
{
  _messageHandler= handler;
}


void MGRT::Request::set_input_handler(const InputHandlerSlot &handler)
{
 _inputHandler= handler;
}



class BlockingRequest : public MGRT::Request {
    Glib::Cond _finished;
    Glib::Mutex _finished_mx;
    bool _ready;
  public:
    BlockingRequest(const Glib::ustring &module,
                    const Glib::ustring &function,
                    const MGRTValue &arguments)
    {
      _module= module;
      _function= function;
      _arguments= arguments;
      _ready= false;
    }

    virtual bool is_async()
    {
      return false;
    }
    
    virtual void finish(MGRT *grt)
    {
      _finished_mx.lock();
      _ready= true;
      _finished.signal();
      _finished_mx.unlock();
    }
    
    virtual bool wait()
    {
      _finished_mx.lock();
      while (!_ready)
        _finished.wait(_finished_mx);
      _finished_mx.unlock();
      
      return true;
    }

    virtual void execute(MGRT *grt)
    {
      _result= MGRTValue(myx_grt_function_get_and_call(grt->grt(),
                                                       _module.c_str(),
                                                       _function.c_str(),
                                                       0,
                                                       _arguments.grtValue(),
                                                       &_error));
    }
};



MGRT::AsyncRequest::AsyncRequest(const Glib::ustring &module,
                                 const Glib::ustring &function,
                                 const MGRTValue &arguments,
                                 void *user_data,
                                 sigc::slot<void,const MGRTValue&,bool,void*> finished,
                                 bool null_ok)
  : _data(user_data), _finished(finished)
{
  _module= module;
  _function= function;
  _arguments= arguments;
  _null_ok= null_ok;
}


bool MGRT::AsyncRequest::is_async()
{
  return true;
}


void MGRT::AsyncRequest::finish(MGRT *grt)
{
  bool error= false;
  
  if ((!_result.isValid() || !grt->result_is_error(_result)) && _error == MYX_GRT_NO_ERROR)
  {
    if (!_result.isValid() && !_null_ok)
    {
      grt->out_text(ufmt("Error: function %s.%s returned unexpected NULL result",
                          _module.c_str(), _function.c_str()));
      error= true;
    }
  }
  else
  {    
    if (_result.isValid())
      grt->out_text(ufmt("%s calling %s.%s: %s",
                          error == MYX_GRT_NO_ERROR ? "Error" : grt->error_text(_error).c_str(),
                          _module.c_str(), _function.c_str(),
                          myx_grt_dict_item_get_as_string(_result, "error")?:""));
    else
      grt->out_text(ufmt("%s calling %s.%s",
                          error == MYX_GRT_NO_ERROR ? "Error" : grt->error_text(_error).c_str(),
                          _module.c_str(), _function.c_str()));
    error= true;
  }

  if (_finished)
    _finished(error || !_result.isValid() ? _result : _result["value"], error, _data);
}


void MGRT::AsyncRequest::execute(MGRT *grt)
{
  _result= MGRTValue(myx_grt_function_get_and_call(grt->grt(),
                                                   _module.c_str(),
                                                   _function.c_str(),
                                                   0,
                                                   _arguments.grtValue(),
                                                   &_error));
}


bool MGRT::AsyncRequest::wait()
{
  return false;
}



MGRT::MGRT()
{
  _grt= myx_grt_initialize(0);
  
  _finishedRequestSignal.connect(sigc::mem_fun(*this,&MGRT::handle_finished_async_requests));
}
  
  
void MGRT::initialize_grt_thread(const Glib::ustring &resourcePath, const Glib::ustring &binResourcePath)
{
  _thread_ready.lock();
  
  Glib::Thread::create(sigc::bind<std::string,std::string>(sigc::mem_fun(*this, &MGRT::request_thread), resourcePath, binResourcePath),
                       false);

  // wait until thread ready
  _thread_ready.lock();
  
  // cleanup
  _thread_ready.unlock();
}


void MGRT::dump(const MGRTValue &value)
{
  myx_grt_value_print(_grt, value.grtValue());
}


void MGRT::init_thread(const std::string &resourcePath, const std::string &binResourcePath)
{
  MYX_GRT_MODULE_LOADER *loader;
  MYX_GRT_ERROR error;
    
  myx_grt_set_input_callback(_grt, this, process_grt_input);
  myx_grt_set_output_callback(_grt, this, process_grt_output);
  myx_grt_set_message_callback(_grt, this, process_grt_message);
  myx_grt_set_status_query_callback(_grt, this, process_grt_status_query);

  if (myx_grt_setup_shell(_grt, MYX_GRT_SHELL_LUA) != MYX_GRT_NO_ERROR)
  {
    out_text("could not initialize shell");
    _thread_ready.unlock();
    return;
  }
  
  myx_grt_shell_print_welcome(_grt);
  
  scan_structs_in_path(resourcePath+"/grt");
  
    // loaders
  myx_register_builtin_grt_module_base(_grt);
  myx_register_builtin_grt_module_reverse_engineer_mysql(_grt);
  myx_register_builtin_grt_module_transformation_mysql(_grt);


#ifdef ENABLE_JAVA_MODULES
  // java modules
  out_text("Loading Java modules...");
  loader= myx_java_init_loader(_grt, 
                               binResourcePath.c_str(), &error, NULL,
                               resourcePath.c_str());
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      out_text("Error registering Java module loader\n");
    else
    {
      int i= myx_grt_scan_for_modules(_grt, std::string(resourcePath+"/java").c_str(), &error);
      if (error != MYX_GRT_NO_ERROR)
        out_text(ufmt("Error scanning Java modules (%i)\n", error));
      else
        out_text(ufmt("Registered %i Java modules\n", i));
    }
  }
  else
    out_text(ufmt("Could not initialize Java module loader (%i)\n",error));
  flush_messages();
#endif

#ifdef ENABLE_PHP_MODULES
  // php modules
  out_text("Loading PHP modules...");
  loader= myx_php_init_loader(_grt, &error);
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      out_text("Error registering PHP module loader\n");
    else
    {
      i= myx_grt_scan_for_modules(_grt, 
                                  std::string(resourcePath+"/php/modules").c_str());
      if (error != MYX_GRT_NO_ERROR)
        out_text(ufmt("Error scanning PHP modules (%i)\n", error));
      else
        out_text("Registered %i PHP modules\n", i);
    }
  }
  else
    out_text("Error initializing PHP module loader (%i)\n",error);
  flush_messages();
#endif

  // lua modules
  out_text("Loading Lua modules...");
  loader= myx_lua_init_loader(_grt, &error, std::string(resourcePath+"/lua").c_str());
  if (loader)
  {
    if (myx_grt_register_module_loader(_grt, loader) < 0)
      out_text("Error registering Lua module loader\n");
    else
    {
      int i= myx_grt_scan_for_modules(_grt, std::string(resourcePath+"/lua").c_str(), &error);
      if (error != MYX_GRT_NO_ERROR)
        out_text(ufmt("Error scanning Lua modules (%i)\n", error));
      else
        out_text(ufmt("Registered %i Lua modules\n", i));
    }
  }
  else
    out_text(ufmt("Error initializing Lua module loader (%i)\n",error));
  flush_messages();

  // builtin c++
  out_text("Initializing C++ loader...");
  setup_loader();
  flush_messages();
  
  myx_grt_shell_init(_grt);
  
  // this will "signal" the main thread that init is done. 
  // must be the last thing done in this function
  _thread_ready.unlock();
}



void MGRT::flush_messages()
{
  myx_grt_messages_stack_flush(_grt, 0);
}



MGRT::~MGRT()
{
  queue_request(0);

  myx_grt_finalize(_grt);
}
    

void MGRT::set_console(MGShellView *shell)
{
  _console= shell;
  
  //_console->make_ready();
}



int MGRT::process_grt_input(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data)
{
  MGRT *me= (MGRT*)user_data;
  Glib::ustring out;
  
  if (!me->_inputHandler(caption, options & MYX_GR_IO_PASSWORD, &out))
    return -1;
  
  *text= g_strdup(out.c_str());
    
  return 0;
}


void MGRT::process_grt_output(const char *text, void *data)
{
  MGRT *me= (MGRT*)data;

  me->out_text(text);
}


void MGRT::process_grt_message(MYX_GRT_MSGS *msgs, void *data)
{
  MGRT *me= (MGRT*)data;
  
  if (msgs)
  {
    Glib::ustring type;
    for (unsigned int i= 0; i < msgs->msgs_num; i++)
    {
      switch (msgs->msgs[i].msg_type)
      {
      case 0: type= "ERROR"; break;
      case 1: type= "WARNING"; break;
      default: type= "MESSAGE"; break;
      }
      if (msgs->msgs[i].msg_detail)
      {
        Glib::ustring msg= msgs->msgs[i].msg;
        for (unsigned int j= 0; j < msgs->msgs[i].msg_detail->strings_num; j++)
          msg+= ufmt("    %s\n", msgs->msgs[i].msg_detail->strings[j]);
        
        me->out_message(msg, type);
      }
      else
        me->out_message(msgs->msgs[i].msg, type);
    }
  }
}


int MGRT::process_grt_status_query(void *data)
{
  MGRT *me= (MGRT*)data;

  if (me->_statusQueryHandler)
    return me->_statusQueryHandler();
  return 0;
}


void MGRT::out_text(const Glib::ustring &text)
{
  if (!_outputHandler.empty())
    _outputHandler(text);
  else if (_console)
    _console->out_text(text);
  if (getenv("DEBUG"))
    g_message("%s", text.c_str());
}


void MGRT::out_message(const Glib::ustring &text,
                      const Glib::ustring &type)
{
  if (getenv("DEBUG"))
    g_message("%s: %s", type.c_str(), text.c_str());

  if (!_messageHandler.empty())
    _messageHandler(text, type);
  else if (_console)
    _console->out_text(type+": "+text);
}


void MGRT::perform_shell_command(const Glib::ustring &command)
{
  MYX_GRT_SHELL_COMMAND error;
  
  error= myx_grt_shell_execute(_grt, command.c_str());
  
  myx_grt_messages_stack_flush(_grt, 0);
}


Glib::ustring MGRT::shell_prompt()
{
  return myx_grt_shell_get_prompt(_grt);
}



bool MGRT::save_subtree(const char *subtree_path, const std::string &path)
{
  MGRTValue subtree(global_value(subtree_path));
  
  if (!subtree.isValid())
    return true;
  
  if (myx_grt_store_to_file(_grt,
                            subtree.grtValue(),
                            path.c_str()) == MYX_GRT_NO_ERROR)
    return true;
  return false;
}


bool MGRT::load_subtree(const char *subtree_path, const std::string &path)
{
  MYX_GRT_VALUE *dict= 0;

  if (access(path.c_str(), F_OK)==0)
    dict= myx_grt_retrieve_from_file(_grt, path.c_str());

  if (dict)
  {
    set_global_value(subtree_path, dict);
    myx_grt_value_release(dict);
    return true;
  }
  return false;
}


MGRTValue MGRT::global_app_dict()
{
  MGRTValue result(global_value("/app"));

  if (!result.isValid())
  {
    result= myx_grt_dict_new(_grt, "base.ApplicationData");
    set_global_value("/app", result.grtValue());
  }
  return result;
}


void MGRT::load_common_options(const std::string &path)
{
  if (!load_subtree("/app/commonOptions", path))
  {
    MGRTValue options(MGRTValue::createTypedDict());
    
    

    set_global_value("/app/commonOptions", options);
  }
}


bool MGRT::scan_structs_in_path(const std::string &path)
{
  int i;
  MYX_GRT_ERROR error;

  i= myx_grt_scan_for_structs(_grt, path.c_str(), &error);
  if (error != MYX_GRT_NO_ERROR)
  {
    out_text(ufmt("Error loading struct definition files from %s (%i)\n",path.c_str(),error));
    return false;
  }
  else
  {
    if (i == 1)
      out_text("Registered one struct definition\n");
    else
      out_text(ufmt("Registered %i struct definitions\n",i));
    return true;
  }
}


bool MGRT::scan_modules_in_path(const std::string &path)
{
  MYX_GRT_ERROR error;
  int i;

  i= myx_grt_scan_for_modules(_grt, path.c_str(), &error);
  if (error != MYX_GRT_NO_ERROR)
  {
    out_text(ufmt("Error scanning for modules in %s (%i)\n", path.c_str(), error));
    return false;
  }
  else
  {
    out_text(ufmt("Registered %i modules\n", i));
    return true;
  }
}



void MGRT::request_thread(const std::string &resourcePath, const std::string &binResourcePath)
{
  init_thread(resourcePath, binResourcePath);
  
  for (;;)
  {
    Request *req;
    OutputHandlerSlot outputSlot;
    InputHandlerSlot inputSlot;
    MessageHandlerSlot messageSlot;
    
    _requestQueueLock.lock();
    while (_requestQueue.empty())
      _requestReady.wait(_requestQueueLock);

    req= *_requestQueue.begin();
    _requestQueue.erase(_requestQueue.begin());
    _requestQueueLock.unlock();
    
    if (!req)
      break;

    if (req->_inputHandler)
      inputSlot= set_input_handler(req->_inputHandler);
    if (req->_outputHandler)
      outputSlot= set_output_handler(req->_outputHandler);
    if (req->_messageHandler)
      messageSlot= set_message_handler(req->_messageHandler);

    req->execute(this);

    if (outputSlot)
      set_output_handler(outputSlot);
    if (inputSlot)
      set_input_handler(inputSlot);
    if (messageSlot)
      set_message_handler(messageSlot);

    if (req->is_async())
    {
      _finishedRequestLock.lock();
      _finishedRequestQueue.push_back(req);
      _finishedRequestLock.unlock();
      _finishedRequestSignal.emit();
    }
    else
      req->finish(this);
  }
}


void MGRT::queue_request(Request *req)
{
  _requestQueueLock.lock();
  _requestQueue.push_front(req);
  _requestReady.signal();
  _requestQueueLock.unlock();
}



void MGRT::handle_finished_async_requests()
{
  Request *req;
  
  _finishedRequestLock.lock();
  req= *_finishedRequestQueue.begin();
  _finishedRequestQueue.erase(_finishedRequestQueue.begin());
  _finishedRequestLock.unlock();

  req->finish(this);

  delete req;
}


MGRT::AsyncRequest *MGRT::prepare_async_call(const std::string &module,
                                             const std::string &function,
                                             const MGRTValue &arguments,
                                             void *data,
                                             sigc::slot<void,const MGRTValue&,bool,void*> finished,
                                             bool null_ok)
{
  AsyncRequest *request;

  _lastError= MYX_GRT_NO_ERROR;
  _lastErrorMessage= "";

  request= new AsyncRequest(module, function, arguments, data, finished, null_ok);
  
  return request;
}



bool MGRT::call_async_function(const std::string &module,
                               const std::string &function,
                               const MGRTValue &arguments,
                               void *data,
                               sigc::slot<void,const MGRTValue&,bool,void*> finished,
                               bool null_ok)
{
  AsyncRequest *request;

  _lastError= MYX_GRT_NO_ERROR;
  _lastErrorMessage= "";

  request= new AsyncRequest(module, function, arguments, data, finished, null_ok);
  
  queue_request(request);

  return true;
}



bool MGRT::call_procedure(const std::string &module,
                         const std::string &procedure,
                         const MGRTValue &arguments)
{
  MGRTValue result(call_function(module, procedure, arguments));
  
  return result.isValid();
}


MGRTValue MGRT::call_function(const std::string &module,
                              const std::string &function,
                              const MGRTValue &arguments)
{
  BlockingRequest *request;
  
  _lastError= MYX_GRT_NO_ERROR;
  _lastErrorMessage= "";
  
  request= new BlockingRequest(module, function, arguments);
  
  queue_request(request);

  request->wait();
  
  MGRTValue result(request->result());
  MYX_GRT_ERROR error= request->error();
  
  delete request;
  
  myx_grt_messages_stack_flush(_grt, 0);
  
  if ((!result.isValid() || !result_is_error(result)) && error == MYX_GRT_NO_ERROR)
  {
    if (result.isValid())
      return result["value"];
    else
      return MGRTValue();
  }
  else
  {
    _lastError= error;
    _lastErrorMessage= make_error_message_from_result(result);
    
    if (result.isValid())
      out_text(ufmt("%s calling %s.%s: %s",
                    error == MYX_GRT_NO_ERROR ? "Error" : error_text(error).c_str(),
                    module.c_str(), function.c_str(),
                    myx_grt_dict_item_get_as_string(result, "error")?:""));
    else
      out_text(ufmt("%s calling %s.%s",
                    error == MYX_GRT_NO_ERROR ? "Error" : error_text(error).c_str(),
                    module.c_str(), function.c_str()));
    return MGRTValue();
  }
}

    
int MGRT::call_int_function(const std::string &module,
                          const std::string &function,
                          const MGRTValue &arguments)
{
  MGRTValue result(call_function(module, function, arguments));
  
  if (result.isValid())
    return result.asInt();
  return 0;
}


Glib::ustring MGRT::call_string_function(const std::string &module,
                                       const std::string &function,
                                       const MGRTValue &arguments)
{
  MGRTValue result(call_function(module, function, arguments));
  
  if (result.isValid())
    return result.asString();
  return "";
}


MGRT::OutputHandlerSlot MGRT::set_output_handler(const OutputHandlerSlot &handler)
{
  _handlerMutex.lock();
  OutputHandlerSlot old= _outputHandler;
  _outputHandler= handler;
  _handlerMutex.unlock();
  return old;
}


MGRT::InputHandlerSlot MGRT::set_input_handler(const InputHandlerSlot &handler)
{
  _handlerMutex.lock();
  InputHandlerSlot old= _inputHandler;
  _inputHandler= handler;
  _handlerMutex.unlock();
  return old;
}


void MGRT::reset_output_handler()
{
  _handlerMutex.lock();
  _outputHandler.disconnect();
  _handlerMutex.unlock();
}


void MGRT::set_status_query_handler(sigc::slot<int> handler)
{
  _statusQueryHandler= handler;
}


MGRT::MessageHandlerSlot MGRT::set_message_handler(const MessageHandlerSlot &handler)
{
  _handlerMutex.lock();
  MessageHandlerSlot old= _messageHandler;
  _messageHandler= handler;
  _handlerMutex.unlock();
  return old;
}


void MGRT::reset_message_handler()
{
  _handlerMutex.lock();
  _messageHandler.disconnect();
  _handlerMutex.unlock();
}



void MGRT::report_error(MYX_GRT_ERROR error)
{
  out_text(error_text(error));
}


MYX_GRT_ERROR MGRT::last_error()
{
  return _lastError;
}


Glib::ustring MGRT::last_error_description()
{
  return _lastErrorMessage;
}



bool MGRT::result_is_error(const MGRTValue &value)
{
  if (value.isValid() && value["error"].isValid())
    return true;
  return false;
}


MGRTValue MGRT::result_value(const MGRTValue &value)
{
  return value["value"];
}


void MGRT::report_error_result(const MGRTValue &result)
{
  if (result.isValid() && result["error"].isValid())
    myx_grt_value_print(_grt, result["error"].grtValue());
  else
    myx_grt_value_print(_grt, MGRTValue("no error").grtValue());
}



Glib::ustring MGRT::error_text(MYX_GRT_ERROR error)
{
  switch (error)
  {
  case MYX_GRT_NO_ERROR: return "Success";
  case MYX_GRT_INTERNAL_ERROR: return "Internal error";
  case MYX_GRT_BAD_PATH: return "Invalid path";
  case MYX_GRT_CANT_OPEN_FILE: return "Cannot open file";
  case MYX_GRT_BAD_FUNCTION: return "Invalid function";
  case MYX_GRT_DUPLICATE_ENTRY: return "Duplicate entry";
  case MYX_GRT_BAD_VALUE: return "Bad value";
  case MYX_GRT_BAD_DATA: return "Bad data";
    
  case MYX_GRT_VALIDATION_ERROR: return "Validation error";
  case MYX_GRT_FUNCTION_CALL_ERROR: return "Function call error";
  case MYX_GRT_MODULE_INIT_ERROR: return "Module init error";
  case MYX_GRT_BAD_MODULE: return "Bad module";
  case MYX_GRT_UNKNOWN_MODULE_TYPE: return "Unknown module type";
    
  case MYX_GRT_SHELL_UNSUPPORTED: return "Unsupported shell language";
  case MYX_GRT_SHELL_ALREADY_LOADED: return "Internal error: shell already loaded";
  case MYX_GRT_SHELL_INIT_ERROR: return "Error initializing GRT shell";

  case MYX_GRT_JAVA_NOT_FOUND: return "Java Runtime not found";
  case MYX_GRT_JAVA_REGISTRY_CORRUPTED: return "Java registry corrupted";
  case MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED: return "JRE cannot be loaded";
  }
  return "";
}



MGRTValue MGRT::global_value(const char *path)
{
  return MGRTValue::fromGlobal(_grt, path);
}


MGRTValue MGRT::global_ref_value(const char *path)
{
  return MGRTValue::refObject(_grt, MGRTValue::fromGlobal(_grt, path).asString());
}


void MGRT::set_global_value(const char *path, const MGRTValue &value)
{
  myx_grt_dict_item_set_by_path(myx_grt_get_root(_grt), path, value.grtValue());
}


void MGRT::unset_global_value(const char *path)
{
  char *end= strrchr(path, '/');
  std::string subpath(path, end-path);
  MYX_GRT_VALUE *dict;
  
  dict= myx_grt_dict_item_get_by_path(_grt, myx_grt_get_root(_grt), subpath.c_str());
  myx_grt_dict_item_del(dict, end+1);
}


//----------------------------------------------------------------------

class MGRTModuleFunctionWrapper {    
  public:
    sigc::slot<MGRTValue,MGRTValue> function;

    static MYX_GRT_ERROR call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);
};


MYX_GRT_ERROR MGRTModuleFunctionWrapper::call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MGRTModuleFunctionWrapper *wrapper= (MGRTModuleFunctionWrapper*)function->priv;
  MGRTValue ret;

  ret= wrapper->function(MGRTValue(value));
  if (ret.isValid())
    *retval= ret.grtValue();
  else
    *retval= NULL;

  return MYX_GRT_NO_ERROR;
}

void MGRT::setup_loader()
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  
  loader->grt= _grt;
  loader->loader_type= MYX_CPP_MODULE_TYPE;
  loader->priv= NULL;
  loader->init_module= NULL;
  loader->call_function= MGRTModuleFunctionWrapper::call_function;
  loader->extensions_num= 0;
  loader->extensions= NULL;

  myx_grt_register_module_loader(_grt, loader);
}


void MGRT::register_builtin_module(const Glib::ustring &module_name,
                                   std::map<Glib::ustring,sigc::slot<MGRTValue,MGRTValue> > &functions)
{
  MYX_GRT_MODULE *module;
  MYX_GRT_ERROR error;
  int count= 0;
  
  GRT_ENTER(_grt);
  
  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);
  
  module->loader= myx_grt_get_loader_of_type(_grt, MYX_CPP_MODULE_TYPE);
  module->priv= NULL;
  module->name= g_strdup(module_name.c_str());
  module->path= NULL;
  module->functions_num= 0;
  module->functions= NULL;
  for (std::map<Glib::ustring,sigc::slot<MGRTValue,MGRTValue> >::const_iterator iter= functions.begin();
       iter != functions.end(); ++iter)
  {
    MGRTModuleFunctionWrapper *wrapper= new MGRTModuleFunctionWrapper();
    
    wrapper->function= iter->second;
    
    count= count+1;
    module->functions= (MYX_GRT_FUNCTION*)g_realloc(module->functions, sizeof(MYX_GRT_FUNCTION)*count);
    memset(module->functions+count-1, 0, sizeof(MYX_GRT_FUNCTION));
    module->functions[count-1].module= module;
    module->functions[count-1].name= g_strdup(iter->first.c_str());
    module->functions[count-1].priv= (MYX_GRT_FUNCTION_PRIVATE*)wrapper;
  }
  module->functions_num= count;
  module->extends= NULL;
    
  error= myx_grt_add_module(_grt, module);
  if (error == MYX_GRT_NO_ERROR)
    out_text("Registration ok");
  else
    out_text(ufmt("C++ loader registration failed (%i)", error));
    
  GRT_LEAVE(_grt);
}
