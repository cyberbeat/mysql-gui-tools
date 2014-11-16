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



#ifndef _MINSTANCEINFO_H_
#define _MINSTANCEINFO_H_

#include <glibmm.h>

#include <pthread.h>
#include <mysql.h>
#include "MDispatcher.h"

#include <list>

#include "MGExecSU.h"
#include "MYXInterface.h"
//#include <wmyx_public_interface.h>
#include <myx_public_interface.h>


class MAdministrator;


class MInstanceInfo : public Glib::ObjectBase {
 public:
    typedef void *(*DataFetcher4)(MYSQL *,void*,void*,void*);
    typedef void *(*DataFetcher3)(MYSQL *,void*,void*);
    typedef void *(*DataFetcher2)(MYSQL *,void*);
    typedef void *(*DataFetcher)(MYSQL *);
    typedef bool (*AsyncCallback)(void *,void*);

    enum ServerState {
      SUnknown,
      SStopped,
      SRunning
    };
    
    
    struct FilesystemInfo {
      std::string fs;
      std::string path;
      std::string type;
      long long size;
      long long free;
      bool ro;
    };
    
    enum ServerPath {
      PSlowLog,
      PErrorLog,
      PLogs
    };


    class SavedInfo : public MGOptions {
      protected:
        virtual void process_options(MYX_APPLICATION_OPTIONS *options, bool shared);
        virtual MYX_APPLICATION_OPTIONS *prepare_options(bool shared);
        
      public:
        SavedInfo(const MYX::UserConnection &user_conn);
        
        std::string error_log_path;
        std::string general_log_path;
        std::string slow_log_path;
        
        std::string mycnf_path;
    };

 private:
    MDispatcher _dispatcher;

    sigc::signal<void> _disconnect_signal;
    sigc::signal<void> _connect_signal;

    int _mysql_tid;
    
    pthread_mutex_t _mysql_mx;
    MYSQL *_mysql;
    bool _lost_connection_msg_shown;
    MYX::UserConnection _user_conn;
    std::map<std::string,MYSQL*> _extra_mysql;

    MAdministrator *_main_win;
    SavedInfo *_local_info;

    bool _socket_connection;
   
    bigint _log_file_pos;

    std::string _var_datadir;
    std::string _var_errorlog;
    
    bool process_service_output(const Glib::ustring &text, const Glib::RefPtr<Gtk::TextBuffer> &buffer);
    void finished_service(MGExecSU::Status status, const Glib::RefPtr<Gtk::TextBuffer> &buffer, MGExecSU *exs, bool starting);

    void cancel_start_stop(MGExecSU *exs);
    
    static void *fetch_server_info(MYSQL *mysql, void *data);
    
 public:
    MInstanceInfo(MAdministrator *main_win);
    ~MInstanceInfo();
    void disconnect();
    
    bool reconnect();
    
    bool check_connection();
    
    void set_connection(MYSQL *mysql, const MYX::UserConnection &user_conn);
    void refresh_server_info();

    bool perform_async_data_fetch3(const char *name, 
                                   DataFetcher3 fetcher, void *arg1, void *arg2,
                                   AsyncCallback callback, void *data,
                                   long delay);
    bool perform_async_data_fetch2(const char *name,
                                   DataFetcher2 fetcher, void *arg,
                                   AsyncCallback callback, void *data,
                                   long delay);
    bool perform_async_data_fetch(const char *name,
                                  DataFetcher fetcher,
                                  AsyncCallback callback, void *data,
                                  long delay);

    void cancel_async_data_fetch(const char *name);

    void *perform_data_fetch4(DataFetcher4 fetcher, void *arg1, void *arg2, void *arg3,
                              const Glib::ustring &msg=Glib::ustring());
    void *perform_data_fetch3(DataFetcher3 fetcher, void *arg1, void *arg2,
                              const Glib::ustring &msg=Glib::ustring());
    void *perform_data_fetch2(DataFetcher2 fetcher, void *arg,
                              const Glib::ustring &msg=Glib::ustring());
    void *perform_data_fetch(DataFetcher fetcher,
                             const Glib::ustring &msg=Glib::ustring());

    bool perform_query(const Glib::ustring &query, const Glib::ustring &msg=Glib::ustring());
    
    void cancel_data_fetch();
    
    int get_mysql_connection_id() { return _mysql_tid; };

    //--------------------------------------------------

    SavedInfo *get_saved_info() { return _local_info; }; // not for general use


    MYX_MACHINE_INFO *get_client_info();
    MYX_MACHINE_INFO *get_server_info();

    bool is_local();
    bool is_connected();
    bool is_socket_connection() { return _socket_connection; };

    void start_stop_service(bool stop, const Glib::RefPtr<Gtk::TextBuffer> &text);

    void mark_log_file();
    Glib::ustring fetch_logs_from_mark();
   
    void lock_mysql(bool flag);
    MYSQL *get_mysql() const { return _mysql; };
    MYSQL *clone_mysql();
    MYX::UserConnection get_connection_data() const { return _user_conn; };
    
    bool get_fs_info(std::list<FilesystemInfo> &fslist);
    
    std::string get_server_path(ServerPath type);
    
    ServerState get_server_state();

    sigc::signal<void> signal_disconnect() { return _disconnect_signal; };
    sigc::signal<void> signal_connect() { return _connect_signal; };
};


#endif /* _MINSTANCEINFO_H_ */


