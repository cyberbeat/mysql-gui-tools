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


#include "MQQueryDispatcher.h"
#include "myg_gtkutils.h"

#include "myqb.h"

#include "MGMySQLConsole.h"

#include <sigc++/class_slot.h>

#include <gtkmm/main.h>

MQQueryDispatcher::Request::Request()
  : keep_open(false), tmp_mysql(0), warnings(0), complete(false)
{
}


MQQueryDispatcher::Request::~Request()
{
}


void MQQueryDispatcher::QueryRequest::finish()
{
  if (got_error)
  {
    error_cb(result,warnings,error);
    if (tmp_mysql)
      myx_mysql_close(tmp_mysql);
  }
  else
  {
    finish_cb(result,warnings);
    if (!result)
    {
//XXX verificar isso (obsoleto depois do callback?)
      dispatcher->check_implicit_transaction_commit(query);
      if (tmp_mysql)
        myx_mysql_close(tmp_mysql);
    }
    else
    {
      // signal that we're ready to check for more resultsets 
      fetch_more_cb(tmp_mysql);
    }
  }
}


void MQQueryDispatcher::QueryRequest::execute(MYSQL *mysql)
{
  long long int affected_rows;
  myx_mysql_limit_resultset_size(mysql, (get_physical_memory_size()/(1024LL*1024LL))/2);

  result= myx_query_execute(mysql, tmp_mysql?NULL:query.c_str(), enforceEditable, params, &error,
                            this, MQQueryDispatcher::progress_row_fetch,
                            MQQueryDispatcher::before_realloc,
                            MQQueryDispatcher::after_realloc,
                            &affected_rows);

  warnings= myx_mysql_error_msgs_fetch(mysql);
  if (error != MYX_NO_ERROR && error != MYX_STOP_EXECUTION)
    got_error= true;
  else
    got_error= false;
}



void MQQueryDispatcher::ProgressReport::finish()
{
  request->cancelled= !progress_cb(result, count, prev_count);
}



void MQQueryDispatcher::SaveRequest::execute(MYSQL *mysql)
{
  errors= NULL;

  rset->mysql= mysql;

  errors= myx_query_apply_actions(rset);
  got_error= errors!=NULL;
}


void MQQueryDispatcher::SaveRequest::finish()
{
  finished_cb(errors);
}



class TableFetchRequest : public MQQueryDispatcher::Request {
    Glib::ustring catalog;
    Glib::ustring schema;

  public:
    TableFetchRequest(const Glib::ustring &catalog,
                      const Glib::ustring &schema,
                      MYX_SCHEMA_TABLES **tbls)
      : catalog(catalog), schema(schema), tables(tbls) {};
    MYX_SCHEMA_TABLES **tables;

    virtual void execute(MYSQL *mysql)
    {
      complete= false;
      *tables= myx_get_schema_tables(mysql, catalog.c_str(), schema.c_str());
      complete= true;
    }

    virtual void finish()
    {
      Gtk::Main::instance()->quit();
    }
};


class SPFetchRequest : public MQQueryDispatcher::Request {
    Glib::ustring catalog;
    Glib::ustring schema;

  public:
    SPFetchRequest(const Glib::ustring &catalog,
                   const Glib::ustring &schema,
                   MYX_SCHEMA_STORED_PROCEDURES **sps)
      : catalog(catalog), schema(schema), procedures(sps) {};
    MYX_SCHEMA_STORED_PROCEDURES **procedures;

    virtual void execute(MYSQL *mysql)
    {
      complete= false;
      *procedures= myx_get_schema_sps(mysql, catalog.c_str(), schema.c_str());
      complete= true;
    }

    virtual void finish()
    {
      Gtk::Main::instance()->quit();
    }
};

//----------------------------------------------------------------------


MQQueryDispatcher::MQQueryDispatcher(MYSQL *mysql, const MYX::UserConnection &conn)
  : _owner(0), _transaction_started(false), _mysql(mysql), _conn(conn)
{
  _thread_dispatcher.connect(sigc::mem_fun(*this,&MQQueryDispatcher::handle_query_result));
  _hook_queue_ready.connect(sigc::mem_fun(*this,&MQQueryDispatcher::handle_query_from_queue));

  myx_mysql_set_query_hooks(mysql, pre_query_hook, NULL, this);
  
  Glib::Thread::create(sigc::mem_fun(*this, &MQQueryDispatcher::mysql_query_thread), false);
}


bool MQQueryDispatcher::check_version(int major, int minor, int pl)
{
  return mysql_full_version_is_later_or_equal_than(_mysql, major, minor, pl);
}


void MQQueryDispatcher::save_edits(MYX_RESULTSET *resultset, const SaveFinishedCallback &finished)
{
  SaveRequest *req= new SaveRequest;

  req->rset= resultset;

  req->finished_cb= finished;

  if (getenv("DEBUG_QB"))
  {
    req->execute(_mysql);
    req->finish();
    return;
  }
  
  if (_transaction_started)
  {
    _mysql_queue_mutex.lock();
    _mysql_queue.push(req);
    _mysql_queue_mutex.unlock();

    _mysql_queue_ready.signal();
  }
  else
  {
    Glib::Thread::create(sigc::bind<Request*>(sigc::mem_fun(*this, &MQQueryDispatcher::normal_thread), req), false);
  }
}

void MQQueryDispatcher::start_query(const Glib::ustring &query,
                                    MYX_STRINGLIST *params,
                                    const FinishedCallback &finished,
                                    const ProgressCallback &progress,
                                    const ErrorCallback &error,
                                    const FetchMoreCallback &fetch_more,
                                    MYSQL *continue_from)
{
  QueryRequest *req= new QueryRequest;

  if (!continue_from)
    req->keep_open= true;
  else
  {
    req->keep_open= true;
    req->tmp_mysql= continue_from;
  }
  req->dispatcher= this;
  req->query= query;
  req->error= MYX_NO_ERROR;
  req->result= 0;
  req->cancelled= false;
  req->got_error= false;
  req->params= params;
  req->finish_cb= finished;
  req->progress_cb= progress;
  req->error_cb= error;
  req->fetch_more_cb= fetch_more;
  
  if (_transaction_started)
  {
    // this will use the connection in _mysql 

    _mysql_queue_mutex.lock();
    _mysql_queue.push(req);
    _mysql_queue_mutex.unlock();

    _mysql_queue_ready.signal();
  }
  else
  {
    // this will create a new connection

    Glib::Thread::create(sigc::bind<Request*>(sigc::mem_fun(*this, &MQQueryDispatcher::normal_thread), req), false);
  }
}


bool MQQueryDispatcher::execute(const Glib::ustring &command, bool report_errors)
{
  long long int affected_rows;
  MYX_LIB_ERROR error;

  _mysql_mutex.lock();

  myx_query_execute_direct(_mysql, command.c_str(), &error, &affected_rows);

  if (error == MYX_NO_ERROR)
  {
    _mysql_mutex.unlock();
    return true;
  }
  else
  {
    if (report_errors)
      myg_show_mysql_error(*static_cast<Gtk::Window*>(_owner),
                           ufmt(_("Error while executing query: %s"),
                                command.c_str()), _mysql);

    _mysql_mutex.unlock();

    return false;
  }
}


void MQQueryDispatcher::before_realloc(void *udata)
{
}


void MQQueryDispatcher::after_realloc(void *udata)
{
}


int MQQueryDispatcher::progress_row_fetch(unsigned long cur_count,
                                          unsigned long prev_count,
                                          MYX_RESULTSET *result_set,
                                          void *udata)
{
  QueryRequest *req= static_cast<QueryRequest*>(udata);
  ProgressReport *rep;
  MQQueryDispatcher *disp= req->dispatcher;

  if (req->cancelled)
  {
    g_message("cancelled");
    return -1;
  }

  rep= new ProgressReport;
  rep->request= req;
  rep->prev_count= prev_count;
  rep->count= cur_count;
  rep->result= result_set;
  rep->progress_cb= req->progress_cb;

  // put the request in the result queue
  disp->_mysql_queue_result_mutex.lock();
  disp->_mysql_queue_result.push(rep);
  disp->_mysql_queue_result_mutex.unlock();
  // notify result ready
  disp->_thread_dispatcher.emit();

  return 0;
}


void MQQueryDispatcher::normal_thread(Request *req)
{
  mysql_thread_init();

  MYSQL *mysql= req->tmp_mysql ? req->tmp_mysql : myx_mysql_init();

  MYX_USER_CONNECTION conn;
  _conn.fill(&conn);

  if (req->tmp_mysql || myx_connect_to_instance(&conn, mysql) >= 0)
  {
    req->execute(mysql);
  }
  else
  {
    req->warnings= myx_mysql_error_msgs_fetch(mysql);

    req->got_error= true;
  }

  if (!req->keep_open && !req->tmp_mysql)
    myx_mysql_close(mysql);
  else if (!req->tmp_mysql) // keep the used connection so that we can use
    req->tmp_mysql= mysql;  // mysql_next_result()

  _mysql_queue_result_mutex.lock();
  _mysql_queue_result.push(req);
  _mysql_queue_result_mutex.unlock();

  _thread_dispatcher.emit();

  mysql_thread_end();
}


void MQQueryDispatcher::mysql_query_thread()
{
  mysql_thread_init();
  for (;;) 
  {
    Request *req;

    // wait for a request
    _mysql_queue_mutex.lock();
    while (_mysql_queue.empty())
      _mysql_queue_ready.wait(_mysql_queue_mutex);

    // get next request
    req= _mysql_queue.front();
    _mysql_queue.pop();
    _mysql_queue_mutex.unlock();

    // execute request
    _mysql_mutex.lock();

    req->execute(_mysql);

    _mysql_mutex.unlock();

    // put the request in the result queue
    _mysql_queue_result_mutex.lock();
    _mysql_queue_result.push(req);
    _mysql_queue_result_mutex.unlock();
    // notify result ready
    _thread_dispatcher.emit();
  }
  mysql_thread_end();
}


void MQQueryDispatcher::pre_query_hook(MYSQL *mysql, void *cdata, const char *query, unsigned int length)
{
  MQQueryDispatcher *disp= (MQQueryDispatcher*)cdata;

  disp->_hook_queue_mutex.lock();
  // we have to convert in 2 steps, because length is in bytes and ustring takes char. count
  disp->_hook_queue.push(std::pair<MYSQL*,Glib::ustring>(mysql, Glib::ustring(std::string(query, length))));
  disp->_hook_queue_mutex.unlock();

  disp->_hook_queue_ready.emit();
}


void MQQueryDispatcher::handle_query_from_queue()
{
  std::pair<MYSQL*,Glib::ustring> query;
  
  _hook_queue_mutex.lock();
  
  query= _hook_queue.front();
  _hook_queue.pop();

  _hook_queue_mutex.unlock();

  if (_transaction_started && query.first == _mysql)
    _transaction_command_signal.emit(query.second);
}



void MQQueryDispatcher::handle_query_result()
{
  Message *msg= 0;

  _mysql_queue_result_mutex.lock();
  msg= _mysql_queue_result.front();
  _mysql_queue_result.pop();
  _mysql_queue_result_mutex.unlock();

  msg->finish();

  delete msg;
}


bool MQQueryDispatcher::can_start_transaction()
{
  bool ok;
  _mysql_queue_mutex.lock();
  ok= _mysql_queue.empty();
  _mysql_queue_mutex.unlock();
  return ok;
}


bool MQQueryDispatcher::is_transaction_started()
{
  return _transaction_started;
}


bool MQQueryDispatcher::start_transaction()
{
  if (!can_start_transaction())
  {
    g_error("starting transaction in invalid state");
    return false;
  }

  if (execute("START TRANSACTION"))
  {
    _transaction_started= true;
    _transaction_started_signal.emit();
    return true;
  }
  return false;
}


bool MQQueryDispatcher::commit_transaction()
{
  if (execute("COMMIT"))
  {
    _transaction_started= false;
    _transaction_ended_signal.emit(true);
    return true;
  }
  return false;
}


bool MQQueryDispatcher::rollback_transaction()
{
  if (execute("ROLLBACK"))
  {
    _transaction_started= false;
    _transaction_ended_signal.emit(false);
    return true;
  }
  return false;
}


bool MQQueryDispatcher::check_implicit_transaction_commit(const Glib::ustring &query)
{
  if (myx_check_whether_commits_transaction(_mysql, query.c_str()))
  {
    _transaction_started= false;
    _transaction_ended_signal.emit(true);
  }
  return false;
}


MYX_CATALOGS *MQQueryDispatcher::get_catalogs()
{
  MYX_CATALOGS *catalogs;
  
  _mysql_mutex.lock();
  catalogs= myx_get_catalogs(_mysql);
  _mysql_mutex.unlock();

  return catalogs;
}


MYX_SCHEMA_TABLES *MQQueryDispatcher::get_tables(const Glib::ustring &catalog,
                                                 const Glib::ustring &schema)
{
  MYX_SCHEMA_TABLES *tables= 0;
  TableFetchRequest *req= new TableFetchRequest(catalog, schema, &tables);

  _mysql_queue_mutex.lock();
  _mysql_queue.push(req);
  _mysql_queue_mutex.unlock();

  _mysql_queue_ready.signal();

  Gtk::Main::instance()->run();

  //while(!req->is_complete())
  //  ;

  return tables;
}


MYX_SCHEMA_STORED_PROCEDURES *MQQueryDispatcher::get_sps(const Glib::ustring &catalog,
                                                         const Glib::ustring &schema)
{
  MYX_SCHEMA_STORED_PROCEDURES *sps= 0;
  SPFetchRequest *req= new SPFetchRequest(catalog, schema, &sps);

  _mysql_queue_mutex.lock();
  _mysql_queue.push(req);
  _mysql_queue_mutex.unlock();

  _mysql_queue_ready.signal();

  Gtk::Main::instance()->run();

  while(!req->is_complete())
    ;

  return sps;
}


MYX_DBM_STORED_PROCEDURE_DATA *MQQueryDispatcher::get_sp_data(const Glib::ustring &catalog,
                                                              const Glib::ustring &schema,
                                                              const Glib::ustring &sp,
                                                              bool func)
{
  MYX_DBM_STORED_PROCEDURE_DATA *spdata;
  
  _mysql_mutex.lock();
  spdata= myx_dbm_get_sp_data(_mysql, catalog.c_str(), schema.c_str(), sp.c_str(), 
                              func?MSPT_FUNCTION:MSPT_PROCEDURE,
                              MYX_DEFAULT_QUOTE_CHAR, 0);
  _mysql_mutex.unlock();
  
  return spdata;
}


MYX_DBM_VIEW_DATA *MQQueryDispatcher::get_view_data(const Glib::ustring &catalog,
                                                    const Glib::ustring &schema,
                                                    const Glib::ustring &view)
{
  MYX_DBM_VIEW_DATA *vdata;
  
  _mysql_mutex.lock();
  vdata= myx_dbm_get_view_data(_mysql, catalog.c_str(), schema.c_str(), view.c_str(),
                               MYX_DEFAULT_QUOTE_CHAR);
  _mysql_mutex.unlock();
  
  return vdata;
}


MYX_EXPLAIN_RESULT *MQQueryDispatcher::explain_query(const Glib::ustring &query,
                                                     MYX_STRINGLIST *params)
{
  MYX_EXPLAIN_RESULT *result;
  
  _mysql_mutex.lock();
  result= myx_query_explain(_mysql, query.c_str());
  _mysql_mutex.unlock();

  return result;
}


bool MQQueryDispatcher::select_schema(const Glib::ustring &catalog, 
                                      const Glib::ustring &schema)
{
  bool ok= true;
  
  _mysql_mutex.lock();
  if (mysql_select_db(_mysql, schema.c_str()) != 0)
    ok= false;
  else
  {
    _conn.schema= schema;
    _current_catalog= catalog;
  }
  _mysql_mutex.unlock();
  
  if (ok)
    _schema_changed_signal.emit();
  
  return ok;
}


Glib::ustring MQQueryDispatcher::get_all_sps(const Glib::ustring &catalog, const Glib::ustring &schema)
{
  char *script;
  Glib::ustring tmp;
  MYX_SCHEMA_STORED_PROCEDURES *sps;
  
  sps= get_sps(catalog, schema);
  if (!sps)
    return "";

  _mysql_mutex.lock();  
  script= myx_dbm_make_script_from_sps(_mysql, catalog.c_str(), schema.c_str(),
                                       sps, MYX_DEFAULT_QUOTE_CHAR);
  _mysql_mutex.unlock();
  
  myx_free_schema_sps(sps);
  
  if (!script)
    return "";
  
  tmp= script;
  g_free(script);
  
  return tmp;
}
