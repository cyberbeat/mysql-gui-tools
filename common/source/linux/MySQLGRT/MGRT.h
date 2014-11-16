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

#ifndef _MGRT_H_
#define _MGRT_H_

#include "myx_grt_public_interface.h"
#include <glibmm.h>
#include <sigc++/sigc++.h>
#include <list>
#include "MGRTValue.h"
#include <myg_utils.h>


class MGShellView;


class MGRT {
  public:
    typedef sigc::slot<void,Glib::ustring> OutputHandlerSlot;
    typedef sigc::slot<void,Glib::ustring,Glib::ustring> MessageHandlerSlot;
    typedef sigc::slot<bool,Glib::ustring,bool,Glib::ustring*> InputHandlerSlot;

    class Request {
        friend class MGRT;

      protected:
        Glib::ustring _module, _function;
        MGRTValue _arguments;

        MGRTValue _result;
        MYX_GRT_ERROR _error;
        
        OutputHandlerSlot _outputHandler;
        MessageHandlerSlot _messageHandler;
        InputHandlerSlot _inputHandler;

      public:
        void set_output_handler(const OutputHandlerSlot &handler);
        void set_message_handler(const MessageHandlerSlot &handler);
        void set_input_handler(const InputHandlerSlot &handler);

        virtual ~Request() {};
        virtual void execute(MGRT *grt)= 0;
        virtual void finish(MGRT *grt)= 0;
        virtual bool is_async()= 0;
        virtual bool wait()= 0;
        virtual MGRTValue result() { return _result; };
        virtual MYX_GRT_ERROR error() { return _error; };
    };
    
    class AsyncRequest : public Request {
        void *_data;
        sigc::slot<void,const MGRTValue&,bool,void*> _finished;
        bool _null_ok;

      public:
        AsyncRequest(const Glib::ustring &module,
                     const Glib::ustring &function,
                     const MGRTValue &arguments,
                     void *user_data,
                     sigc::slot<void,const MGRTValue&,bool,void*> finished,
                     bool null_ok);

        virtual void finish(MGRT *grt);
        
        virtual bool is_async();
        
        virtual bool wait();

        virtual void execute(MGRT *grt);
    };

  protected:    
    MYX_GRT *_grt;

    std::list<Request*> _requestQueue;
    Glib::Mutex _requestQueueLock;
    Glib::Cond _requestReady;

    std::list<Request*> _finishedRequestQueue;
    Glib::Mutex _finishedRequestLock;
    Glib::Dispatcher _finishedRequestSignal;
    
    MGShellView *_console;

    Glib::Mutex _handlerMutex;
    OutputHandlerSlot _outputHandler;
    MessageHandlerSlot _messageHandler;
    InputHandlerSlot _inputHandler;
    sigc::slot<int> _statusQueryHandler;

    MYX_GRT_ERROR _lastError;
    Glib::ustring _lastErrorMessage;
    
    Glib::ustring _appDictPath;
    
    Glib::Mutex _thread_ready;

    static int process_grt_input(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data);
    static void process_grt_output(const char *text, void *data);
    static void process_grt_message(MYX_GRT_MSGS *msgs, void *data);
    static int process_grt_status_query(void *data);
    
    void init_thread(const std::string &resourcePath, const std::string &binResourcePath);
    void request_thread(const std::string &resourcePath, const std::string &binResourcePath);
    
    void handle_finished_async_requests();

    void setup_loader();
  public:
    MGRT();
    virtual ~MGRT();

    void initialize_grt_thread(const Glib::ustring &resourcePath, const Glib::ustring &binResourcePath);
    
    inline MYX_GRT *grt() const { return _grt; };

    void flush_messages();
   
    void dump(const MGRTValue &value);
    
    static void dummy(const MGRTValue&, bool, void*) {};
    
    void set_console(MGShellView *shell);
    void out_text(const Glib::ustring &text);
    void out_message(const Glib::ustring &text,
                    const Glib::ustring &type);
    void perform_shell_command(const Glib::ustring &command);
    Glib::ustring shell_prompt();

    bool save_subtree(const char *subtree_path, const std::string &path);
    bool load_subtree(const char *subtree_path, const std::string &path);

    MGRTValue global_app_dict();
    void load_common_options(const std::string &path);

    bool scan_structs_in_path(const std::string &path);
    bool scan_modules_in_path(const std::string &path);

    void queue_request(Request *req);

    bool call_procedure(const std::string &module,
                       const std::string &procedure,
                       const MGRTValue &arguments);
    MGRTValue call_function(const std::string &module,
                            const std::string &function,
                            const MGRTValue &arguments);
    
    int call_int_function(const std::string &module,
                        const std::string &function,
                        const MGRTValue &arguments);

    Glib::ustring call_string_function(const std::string &module,
                                     const std::string &function,
                                     const MGRTValue &arguments);

    
    AsyncRequest *prepare_async_call(const std::string &module,
                                     const std::string &function,
                                     const MGRTValue &arguments,
                                     void *data,
                                     sigc::slot<void,const MGRTValue&,bool,void*> finished,
                                     bool null_ok);
    
    bool call_async_function(const std::string &module,
                             const std::string &function,
                             const MGRTValue &arguments,
                             void *data= 0,
                             sigc::slot<void,const MGRTValue&,bool,void*> finished= sigc::ptr_fun(MGRT::dummy),
                             bool null_ok= false);


    InputHandlerSlot set_input_handler(const InputHandlerSlot &handler);

    OutputHandlerSlot set_output_handler(const OutputHandlerSlot &handler);
    void reset_output_handler();
    
    void set_status_query_handler(sigc::slot<int> handler);

    MessageHandlerSlot set_message_handler(const MessageHandlerSlot &handler);
    void reset_message_handler();
    
    void report_error(MYX_GRT_ERROR error);

    MYX_GRT_ERROR last_error();
    Glib::ustring last_error_description();

    bool result_is_error(const MGRTValue &value);
    MGRTValue result_value(const MGRTValue &value);
    void report_error_result(const MGRTValue &value);

    Glib::ustring error_text(MYX_GRT_ERROR error);

    MGRTValue global_value(const char *path);
    MGRTValue global_ref_value(const char *path);
    void set_global_value(const char *path, const MGRTValue &value);
    void unset_global_value(const char *path);

    MGRTValue global_value(const Glib::ustring &path) { return global_value(path.c_str()); };
    MGRTValue global_ref_value(const Glib::ustring &path) { return global_ref_value(path.c_str()); };
    void set_global_value(const Glib::ustring &path, const MGRTValue &value) { set_global_value(path.c_str(), value); };
    

    void register_builtin_module(const Glib::ustring &module_name,
                                 std::map<Glib::ustring,sigc::slot<MGRTValue,MGRTValue> > &functions);
};

#endif /* _MGRT_H_ */

