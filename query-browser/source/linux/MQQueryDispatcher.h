/* Copyright (C) 2004 MySQL AB

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

#ifndef _MQQUERYDISPATCHER_H_
#define _MQQUERYDISPATCHER_H_

#include "myx_public_interface.h"
#include "myx_qb_public_interface.h"
#include "MDispatcher.h"
#include "MQPreferences.h"

#include <gtkmm/widget.h>

#include "MYXInterface.h"

#include <queue>

class MGMySQLConsole;

extern MQPreferences prefs;

class MQQueryDispatcher : public Glib::ObjectBase {
  public:
    typedef sigc::slot2<void,MYX_RESULTSET*,MYX_MYSQL_ERROR_MSGS*> FinishedCallback;
    typedef sigc::slot3<void,MYX_RESULTSET*,MYX_MYSQL_ERROR_MSGS*,MYX_LIB_ERROR> ErrorCallback;
    typedef sigc::slot3<bool,MYX_RESULTSET*,unsigned long,unsigned long> ProgressCallback;
    typedef sigc::slot1<void,MYSQL*> FetchMoreCallback;

    typedef sigc::signal0<void> SchemaChangedSignal;
    typedef sigc::signal0<void> TransactionStartedSignal;
    typedef sigc::signal1<void,bool> TransactionEndedSignal;
    typedef sigc::signal1<void,Glib::ustring> TransactionCommandSignal;

    typedef sigc::slot1<void,MYX_RS_ACTION_ERRORS*> SaveFinishedCallback;

    class Message {
      public:
      virtual void finish()= 0;
      virtual ~Message() {};
    };
    
    class Request : public Message {
      protected:
        volatile bool complete;
      public:
        bool keep_open;
        bool free_mysql;
        MYSQL *tmp_mysql;

        bool got_error;
        MYX_MYSQL_ERROR_MSGS *warnings;

        Request();
        virtual ~Request();
        virtual void execute(MYSQL *mysql)= 0;

        bool is_complete() const { return complete; }
    };
    
    class QueryRequest : public Request {
      public:
        QueryRequest() : enforceEditable(prefs.auto_add_pk_check), result(0) {};

        MQQueryDispatcher *dispatcher;
      
        bool enforceEditable;
        
        Glib::ustring query;
        MYX_STRINGLIST *params;

        MYX_RESULTSET *result;

        MYX_LIB_ERROR error;

        bool cancelled;

        FinishedCallback finish_cb;
        ProgressCallback progress_cb;
        ErrorCallback error_cb;
        FetchMoreCallback fetch_more_cb;

        virtual void execute(MYSQL *mysql);
        virtual void finish();
    };
    
    class ProgressReport : public Message {
      public:
      QueryRequest *request;

      MYX_RESULTSET *result;
      unsigned long prev_count;
      unsigned long count;

      ProgressCallback progress_cb;

      virtual void finish();
    };


    class SaveRequest : public Request {
        MYX_RS_ACTION_ERRORS *errors;
      public:
        SaveRequest() : errors(0) {};
        
        MYX_RESULTSET *rset;

        SaveFinishedCallback finished_cb;

        virtual void execute(MYSQL *mysql);
        virtual void finish();
    };
  protected:
    Gtk::Widget *_owner;
    
    Glib::Dispatcher _thread_dispatcher;
    void normal_thread(Request *req);
    void mysql_query_thread();

    void handle_query_result();

    static void pre_query_hook(MYSQL *mysql, void *cdata, const char *query, unsigned int length);
    void handle_query_from_queue();
    
    // if a transaction has started, all queries are serialized
    // through _mysql_queue
    bool _transaction_started;
    Glib::Cond _mysql_queue_ready;
    Glib::Mutex _mysql_queue_mutex;
    std::queue<Request*> _mysql_queue;
    Glib::Mutex _mysql_queue_result_mutex;
    std::queue<Message*> _mysql_queue_result;
    
    Glib::Dispatcher _hook_queue_ready;
    std::queue<std::pair<MYSQL*,Glib::ustring> > _hook_queue;
    Glib::Mutex _hook_queue_mutex;
    
    Glib::Mutex _mysql_mutex;
    MYSQL *_mysql;

    Glib::ustring _current_catalog;
    MYX::UserConnection _conn;

    SchemaChangedSignal _schema_changed_signal;
    TransactionStartedSignal _transaction_started_signal;
    TransactionEndedSignal _transaction_ended_signal;
    TransactionCommandSignal _transaction_command_signal;

    static int progress_row_fetch(unsigned long cur_count,
                                  unsigned long prev_count,
                                  MYX_RESULTSET *result_set,
                                  void *udata);
    
    static void before_realloc(void *udata);
    static void after_realloc(void *udata);
    
  public:
    MQQueryDispatcher(MYSQL *mysql, const MYX::UserConnection &conn);

    bool check_version(int major, int minor, int pl);
    
    void set_owner_widget(Gtk::Widget *owner) { _owner = owner; };
    
    MYX::UserConnection get_user_connection() const { return _conn; };
    
    Glib::ustring get_current_catalog() const { return _current_catalog; };
    Glib::ustring get_current_schema() const { return _conn.schema; };

    void start_query(const Glib::ustring &query,
                     MYX_STRINGLIST *params,
                     const FinishedCallback &finished,
                     const ProgressCallback &progress,
                     const ErrorCallback &error,
                     const FetchMoreCallback &fetch_more,
                     MYSQL *mysql);
    
    bool execute(const Glib::ustring &command, bool report_errors= true);
    
    void start_command();

    MYX_EXPLAIN_RESULT *explain_query(const Glib::ustring &query,
                                      MYX_STRINGLIST *params);

    void save_edits(MYX_RESULTSET *resultset, const SaveFinishedCallback &finished);

    bool can_start_transaction();
    bool is_transaction_started();
    bool start_transaction();
    bool commit_transaction();
    bool rollback_transaction();
    bool check_implicit_transaction_commit(const Glib::ustring &query);

//    bool kill_thread();
    
    MYSQL *get_mysql() { return _mysql; };
    bool select_schema(const Glib::ustring &catalog, const Glib::ustring &schema);

    SchemaChangedSignal signal_schema_changed() { return _schema_changed_signal; };
    TransactionStartedSignal signal_transaction_started() { return _transaction_started_signal; };
    TransactionEndedSignal signal_transaction_ended() { return _transaction_ended_signal; };
    TransactionCommandSignal signal_transaction_command() { return _transaction_command_signal; };
    
    MYX_CATALOGS* get_catalogs();
    MYX_SCHEMA_TABLES *get_tables(const Glib::ustring &catalog, const Glib::ustring &schema);
    MYX_SCHEMA_STORED_PROCEDURES *get_sps(const Glib::ustring &catalog, const Glib::ustring &schema);
    MYX_DBM_STORED_PROCEDURE_DATA *get_sp_data(const Glib::ustring &catalog,
                                               const Glib::ustring &schema,
                                               const Glib::ustring &sp,
                                               bool func);
    MYX_DBM_VIEW_DATA *get_view_data(const Glib::ustring &catalog,
                                     const Glib::ustring &schema,
                                     const Glib::ustring &view);
    
    Glib::ustring get_all_sps(const Glib::ustring &catalog, const Glib::ustring &schema);
};

#endif /* _MQQUERYDISPATCHER_H_ */
