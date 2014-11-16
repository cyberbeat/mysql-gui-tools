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


#include "myadmin.h"

/* for FreeBSD */
#ifdef HAVE_SYS_MOUNT_H
#undef MIN
#undef MAX
#include <sys/param.h>
#include <sys/mount.h>
#endif

/* for Solaris */
#ifdef sun
# include <sys/types.h>
# include <sys/statvfs.h>
#elif defined(hpux) || defined(__hpux)
# include <sys/vfs.h>
#else
/* for Linux */
# ifdef HAVE_SYS_STATFS_H
#  include <sys/statfs.h>
#  include <sys/vfs.h>
# endif
#endif

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/utsname.h>

#include <errmsg.h>

#include "MInstanceInfo.h"

#include "MAdministrator.h"

#include "MGExecSU.h"

#include "myx_util_functions.h"
#include "myx_public_interface.h"
#include "myx_admin_public_interface.h"

#include "myg_gtkutils.h"
#include "myg_utils.h"

struct FSType {
  long id;
  const char *name;
} fstypes[]= {
{ 0xadf5, "adfs" },
{ 0xADFF, "affs" },
{ 0x42465331, "bffs" },
{ 0x1BADFACE, "bfs" },
{ 0xFF534D42, "CIFS" },
{ 0x73757245, "CODA" },
{ 0x012FF7B7, "coh" },
{ 0x28cd3d45, "cramfs" },
{ 0x1373, "" }, //devfs
{ 0x00414A53, "efs" },
{ 0x137D, "ext" },
{ 0xEF51, "ext2" },
{ 0xEF53, "ext2" },
{ 0xEF53, "ext3" },
{ 0x4244, "hfs" },
{ 0xF995E849, "hpfs" },
{ 0x958458f6, "hugetlbfs" },
{ 0x9660, "isofs" },
{ 0x72b6, "jffs2" },
{ 0x3153464a, "JFS" },
{ 0x137F, "minix" },
{ 0x138F, "minix" },
{ 0x2468, "minix2" },
{ 0x2478, "minix2" },
{ 0x4d44, "VFAT" },
{ 0x564c, "ncp" },
{ 0x6969, "NFS" },
{ 0x5346544e, "NTFS" },
{ 0x9fa1, "OpenPROM" },
{ 0x9fa0, "" }, //proc
{ 0x002f, "QNX4" },
{ 0x52654973, "ReiserFS" },
{ 0x7275, "ROMFS" },
{ 0x517B, "SMB" },
{ 0x012FF7B6, "SysV2" },
{ 0x012FF7B5, "SysV4" },
{ 0x01021994, "tmpfs" },
{ 0x15013346, "UDF" },
{ 0x00011954, "UFS" },
{ 0x9fa2, "" },//usbdev
{ 0xa501FCF5, "VXFS" },
{ 0x012FF7B4, "xenix" },
{ 0x58465342, "XFS" },
{ 0x012FD16D, "XIAFS" }
};


#define START_STOP_TIMEOUT 15

MInstanceInfo::SavedInfo::SavedInfo(const MYX::UserConnection &user_conn)
{
  struct utsname buf;
  std::string hostname= "localhost";
        
  if (uname(&buf) == 0)
  {
    hostname= buf.nodename;
  }

  std::string fname= "instance_info_"+hostname+":"+tostr(user_conn.port)+".xml";

  load("administrator/"+fname);
}


void MInstanceInfo::SavedInfo::process_options(MYX_APPLICATION_OPTIONS *options, bool shared)
{
  const char *value;

  if (shared)
    return;
  
  for (unsigned int i= 0; i < options->option_groups_num; i++)
  {
    MYX_OPTION_GROUP *group= options->option_groups+i;

    if (g_strcasecmp(group->name,"log_files")==0)
    {
      if ((value= find_value(group, "error_log")))
        error_log_path= value;
      if ((value= find_value(group, "general_log")))
        general_log_path= value;
      if ((value= find_value(group, "slow_log")))
        slow_log_path= value;
    }
    else if (g_strcasecmp(group->name,"general")==0)
    {
      if ((value= find_value(group, "mycnf_path")))
        mycnf_path= value;
    }
  }
}



MYX_APPLICATION_OPTIONS *MInstanceInfo::SavedInfo::prepare_options(bool shared)
{
  if (!shared)
  {
    MYX_APPLICATION_OPTIONS *options= (MYX_APPLICATION_OPTIONS*)g_malloc0(sizeof(MYX_APPLICATION_OPTIONS));
    std::list<ValuePair> l;

    l.clear();
    l.push_back(ValuePair("error_log",error_log_path));
    l.push_back(ValuePair("general_log",general_log_path));
    l.push_back(ValuePair("slow_log",slow_log_path));

    add_group(options, "log_files", l);
  
    l.clear();
    l.push_back(ValuePair("mycnf_path",mycnf_path));
    add_group(options, "general", l);
    
    return options;
  }
  return 0;
}




//----------------------------------------------------------------------

MInstanceInfo::MInstanceInfo(MAdministrator *main_win)
  : _mysql(0), _lost_connection_msg_shown(false), _main_win(main_win), 
    _local_info(0), _socket_connection(false)
{
  pthread_mutex_init(&_mysql_mx, NULL);

  _dispatcher.signal_work_start_stop().connect(sigc::mem_fun(*main_win, &MAdministrator::set_busy_progress));
}


MInstanceInfo::~MInstanceInfo()
{
  pthread_mutex_destroy(&_mysql_mx);
  delete _local_info;
}


void MInstanceInfo::disconnect()
{
  _disconnect_signal.emit();

  myx_mysql_close(_mysql);
  _mysql= NULL;

  delete _local_info;
  _local_info= 0;
}


bool MInstanceInfo::reconnect()
{
  MYSQL *mysql;
  MYX_USER_CONNECTION conn;
  
  _user_conn.fill(&conn);
  
  mysql= myx_mysql_init();
  if (myx_connect_to_instance(&conn, mysql) < 0)
    return false;

  if (_mysql)
    myx_mysql_close(_mysql);
  _mysql= mysql;

  return true;
}


void MInstanceInfo::set_connection(MYSQL *mysql, 
                                   const MYX::UserConnection &user_conn)
{
  _mysql= mysql;
  _user_conn.assign(user_conn);
  _lost_connection_msg_shown= false;

  // check server version
  if (is_connected())
  {    
    MYX_MACHINE_INFO *server= get_server_info();
    
    if (server->version)
    {
      if (strstr(server->version, "via socket"))
        _socket_connection= true;
      else
        _socket_connection= false;
   
    
      int major_version= myx_get_mysql_major_version(mysql);

      if (major_version < 4)
      {
        Gtk::MessageDialog
          dlg(ufmt(_("You are connecting to a MySQL server with version %i.%i.x, "
                     "but this application was designed to work with MySQL "
                     "servers 4.x and newer.\n"
                     "Please note that older versions are not supported and "
                     "may not work properly."), 
                   major_version, myx_get_mysql_minor_version(mysql)),
              false,
              Gtk::MESSAGE_WARNING,
              Gtk::BUTTONS_OK,
              true);
        dlg.run();
      }
      
      _mysql_tid= (long)perform_data_fetch((DataFetcher)myx_get_thread_id);
    }
    myx_free_pc_info(server);

    if (is_local())
    {
      _local_info= new SavedInfo(user_conn);
    }
  }

  // reset data cached from the server
  _var_datadir="";
  _var_errorlog="";
  
  _connect_signal.emit();
}


void MInstanceInfo::lock_mysql(bool flag)
{
  if (flag)
  {
    pthread_mutex_lock(&_mysql_mx);
  }
  else
  {
    pthread_mutex_unlock(&_mysql_mx);
  }
}


MYSQL *MInstanceInfo::clone_mysql()
{
  MYSQL *mysql= myx_mysql_init();
  
  MYX_USER_CONNECTION conn;
  _user_conn.fill(&conn);
  
  if (myx_connect_to_instance(&conn, mysql) < 0)
  {
    myx_mysql_close(mysql);
    mysql= 0;
  }
  
  return mysql;
}


bool MInstanceInfo::check_connection()
{
  bool gone= false;

  if (_lost_connection_msg_shown || !_mysql)
    return false;

  switch (myx_mysql_errno(_mysql))
  {
  case CR_SERVER_GONE_ERROR:
  case CR_CONNECTION_ERROR:
  case CR_SERVER_LOST:
  case CR_CONN_HOST_ERROR:
    gone= true;
    break;
  }

  if (!gone)
  {
    if (0&&mysql_ping(_mysql) != 0)
    {
      gone= true;
    }
  }

  if (gone)
  {
    Gtk::MessageDialog dlg(_("<b>The Connection to the MySQL server was lost.</b>\nA server shut down or timeout are possible causes. You may try reconnecting."), true,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_NONE, true);

//    _lost_connection_msg_shown= true;
    
    dlg.add_button(_("Quit"), 1);
    dlg.add_button(_("_Ignore"), 0);
    dlg.add_button(_("_Reconnect"), 2);
    
    switch (dlg.run())
    {
    case 0:
      break;
      
    case 1:
      _main_win->quit();
      break;
      
    case 2:
      MYX_USER_CONNECTION conn;
      _user_conn.fill(&conn);
      
      if (!_mysql)
        _mysql= myx_mysql_init();
      
      if (myx_connect_to_instance(&conn, _mysql) < 0)
      {
        myg_show_mysql_error(_("Could not reconnect to MySQL server."), _mysql);
        return false;
      }
      return true;
    }
    return false;
  }
  return true;
}


void MInstanceInfo::refresh_server_info()
{
  if (is_connected())
    perform_data_fetch2(fetch_server_info, this);
}


struct func_data4 {
  MYSQL *mysql;
  MInstanceInfo::DataFetcher4 fetcher;
  void *arg1;
  void *arg2;
  void *arg3;
};

static void *fetch_data4(void *data)
{
  func_data4 *fdata= (func_data4*)data;

  return (*fdata->fetcher)(fdata->mysql, fdata->arg1, fdata->arg2, fdata->arg3);
}


void *MInstanceInfo::perform_data_fetch4(DataFetcher4 fetcher,
                                          void *arg1, void *arg2, void *arg3,
                                          const Glib::ustring &msg)
{
  func_data4 data;

  data.mysql= _mysql;
  data.fetcher= fetcher;
  data.arg1= arg1;
  data.arg2= arg2;
  data.arg3= arg3;

  if (check_connection())
  {
    _main_win->set_busy(msg.empty()?_("Retrieving data from MySQL..."):msg);

    lock_mysql(true);
    void *result= _dispatcher.dispatch(fetch_data4, &data);
    lock_mysql(false);

    _main_win->set_busy();

    if (_dispatcher.was_cancelled())
      _main_win->set_status("");//XXX_("Data retrieval canceled!"));
  
    return result;
  }
  return NULL;
}



struct func_data3 {
  MYSQL *mysql;
  MInstanceInfo::DataFetcher3 fetcher;
  void *arg1;
  void *arg2;
};

static void *fetch_data3(void *data)
{
  func_data3 *fdata= (func_data3*)data;

  return (*fdata->fetcher)(fdata->mysql, fdata->arg1, fdata->arg2);
}


void *MInstanceInfo::perform_data_fetch3(DataFetcher3 fetcher,
                                          void *arg1, void *arg2,
                                          const Glib::ustring &msg)
{
  func_data3 data;

  data.mysql= _mysql;
  data.fetcher= fetcher;
  data.arg1= arg1;
  data.arg2= arg2;

  if (check_connection())
  {
    _main_win->set_busy(msg.empty()?_("Retrieving data from MySQL..."):msg);

    lock_mysql(true);
    void *result= _dispatcher.dispatch(fetch_data3, &data);
    lock_mysql(false);

    _main_win->set_busy();

    if (_dispatcher.was_cancelled())
      _main_win->set_status("");//XXX_("Data retrieval canceled!"));
  
    return result;
  }
  return NULL;
}



bool MInstanceInfo::perform_async_data_fetch3(const char *name,
                                               DataFetcher3 fetcher,
                                               void *arg1, void *arg2,
                                               AsyncCallback callback, void *udata,
                                               long delay)
{
  MYSQL *mysql= clone_mysql();
  _extra_mysql[name]= mysql;

  if (mysql)
  {
    func_data3 *data= (func_data3*)g_malloc(sizeof(func_data3));
    data->mysql= mysql;
    data->fetcher= fetcher;
    data->arg1= arg1;
    data->arg2= arg2;

    _dispatcher.dispatch_async(name, fetch_data3, data, g_free,
                               callback, udata,
                               delay);
    return true;
  }
  else
    return false;
}



struct func_data2 {
  MYSQL *mysql;
  MInstanceInfo::DataFetcher2 fetcher;
  void *arg;
};

static void *fetch_data2(void *data)
{
  func_data2 *fdata= (func_data2*)data;

  return (*fdata->fetcher)(fdata->mysql, fdata->arg);
}

void *MInstanceInfo::perform_data_fetch2(DataFetcher2 fetcher,
                                          void *arg,
                                          const Glib::ustring &msg)
{
  func_data2 data;

  data.mysql= _mysql;
  data.fetcher= fetcher;
  data.arg= arg;

  if (check_connection())
  {
    _main_win->set_busy(msg.empty()?_("Retrieving data from MySQL..."):msg);
    
    lock_mysql(true);
    void *result= _dispatcher.dispatch(fetch_data2, &data);
    lock_mysql(false);
  
    _main_win->set_busy();

    if (_dispatcher.was_cancelled())
      _main_win->set_status("");//XXX_("Data retrieval canceled!"));

    return result;
  }
  return NULL;
}


bool MInstanceInfo::perform_async_data_fetch2(const char *name,
                                               DataFetcher2 fetcher,
                                               void *arg,
                                               AsyncCallback callback, void *udata,
                                               long delay)
{
  MYSQL *mysql= clone_mysql();
  if (mysql)
  {
    func_data2 *data= (func_data2*)g_malloc(sizeof(func_data2));
    _extra_mysql[name]= mysql;
    
    data->mysql= mysql;
    data->fetcher= fetcher;
    data->arg= arg;
    
    _dispatcher.dispatch_async(name, fetch_data2, data, free, callback, udata, delay);
    return true;
  }
  return false;
}


void *MInstanceInfo::perform_data_fetch(DataFetcher fetcher,
                                         const Glib::ustring &msg)
{
  if (check_connection())
  {
    _main_win->set_busy(msg.empty()?_("Retrieving data from MySQL..."):msg);

    lock_mysql(true);
    void *result= _dispatcher.dispatch((void*(*)(void*))fetcher, _mysql);
    lock_mysql(false);

    _main_win->set_busy();

    if (_dispatcher.was_cancelled())
      _main_win->set_status("");//XXX_("Data retrieval canceled!"));

    return result;
  }
  return NULL;
}


bool MInstanceInfo::perform_async_data_fetch(const char *name,
                                              DataFetcher fetcher,
                                              AsyncCallback callback, void *udata,
                                              long delay)
{
  MYSQL *mysql= clone_mysql();
  if (mysql)
  {
    _extra_mysql[name]= mysql;
    
    _dispatcher.dispatch_async(name, (void*(*)(void*))fetcher, mysql, NULL,
                               callback, udata, delay);
    return true;
  }
  return false;
}

bool MInstanceInfo::perform_query(const Glib::ustring &query, const Glib::ustring &msg)
{
  MYX_LIB_ERROR error;
  bigint affected_rows;

  if (check_connection())
  {
    perform_data_fetch4((DataFetcher4)myx_query_execute_direct, (void*)query.c_str(), 
                        &error, &affected_rows);

    if (error != MYX_NO_ERROR)
    {
      myg_show_xlib_error(ufmt(_("Could not execute query '%s'."), query.c_str()), error);
      return false;
    }
    return true;
  }
  return false;
}


bool MInstanceInfo::process_service_output(const Glib::ustring &text, const Glib::RefPtr<Gtk::TextBuffer> &buffer)
{
  Gtk::TextIter iter= buffer->end();
  buffer->insert(iter, text);
  return true;
}


void MInstanceInfo::finished_service(MGExecSU::Status status, const Glib::RefPtr<Gtk::TextBuffer> &buffer, MGExecSU *exs,
                                     bool starting)
{
  Gtk::TextIter iter= buffer->end();

  buffer->insert(iter, exs->read_to_end()+"\n");
  iter= buffer->end();

  switch (status)
  {
  case MGExecSU::RSUCCESS:
    if (starting)
      buffer->insert(iter, _("Successfully started\n"));
    else
      buffer->insert(iter, _("Successfully stopped\n"));
    break;
  case MGExecSU::RBADPASSWORD:
    buffer->insert(iter, _("Authentication failure\n"));
    break;
  case MGExecSU::RTIMEOUT:
    buffer->insert(iter, _("Timedout waiting for MySQL init script\n"));
    break;
  case MGExecSU::RERROR:
    buffer->insert(iter, _("Error calling MySQL init script\n"));
    break;
  default:
    break;
  }
  _main_win->set_busy();
  _main_win->pop_stop_button_handler();
  Gtk::Main::instance()->quit();
}


void MInstanceInfo::cancel_start_stop(MGExecSU *exs)
{
  exs->cancel();
}


void MInstanceInfo::start_stop_service(bool stop, 
                                       const Glib::RefPtr<Gtk::TextBuffer> &text)
{
  MGExecSU *execsu;
  Glib::ustring command;

  if (!Glib::file_test(prefs.start_script_path, Glib::FILE_TEST_EXISTS))
  {
    myg_show_error(_("<b>Could not locate MySQL startup script.</b>\n\nPlease go to 'Preferences' and specify its location."));
    return;
  }
  else
  {
    command= ufmt("%s %s", prefs.start_script_path.c_str(),
                  stop ? "stop" : "start");;
  }

  execsu= new MGExecSU(ufmt(_("Administrative privileges are required to %s the MySQL server.\n"
                              "Please enter the root password to proceed."), stop ? _("stop") : _("start")),
                       command);
  
  execsu->signal_output().connect(sigc::bind<Glib::RefPtr<Gtk::TextBuffer> >(sigc::mem_fun(*this, &MInstanceInfo::process_service_output), text));
  execsu->signal_finished().connect(sigc::bind<Glib::RefPtr<Gtk::TextBuffer>, MGExecSU*, bool >(sigc::mem_fun(*this, &MInstanceInfo::finished_service), text, execsu, !stop));
  execsu->set_timeout(START_STOP_TIMEOUT);
  
  _main_win->set_busy(stop?_("Stopping service..."):_("Starting service..."));

  _main_win->push_stop_button_handler(sigc::bind<MGExecSU*>(sigc::mem_fun(*this,&MInstanceInfo::cancel_start_stop), execsu));

  if (!execsu->run())
  {
    Gtk::TextIter iter= text->end();
    text->insert(iter, _("Could not execute MySQL init script\n"));
    _main_win->set_busy();
    _main_win->pop_stop_button_handler();
    return;
  }

  Gtk::Main::instance()->run();
}


void MInstanceInfo::cancel_async_data_fetch(const char *name)
{
  _dispatcher.cancel_async(name);

  if (_extra_mysql.find(name)!=_extra_mysql.end())
  {
    myx_mysql_close(_extra_mysql[name]);
  }
}


void MInstanceInfo::cancel_data_fetch()
{
  _dispatcher.cancel();
}


bool MInstanceInfo::is_local()
{
  if (_mysql)
    return myx_is_localhost(_user_conn.hostname.c_str())!=0;
  else
    return true;
}


bool MInstanceInfo::is_connected()
{
  return _mysql != 0;
}


void *MInstanceInfo::fetch_server_info(MYSQL *mysql, void *data)
{
  char *value;
  MInstanceInfo *info= static_cast<MInstanceInfo*>(data);
  
  value= myx_get_server_variable(mysql, "datadir");
  info->_var_datadir= value;
  if (value)
  {
    g_free(value);
  }

  value= myx_get_server_variable(mysql, "log_error");
  info->_var_errorlog= value;
  if (value)
  {
    g_free(value);
  }

  return NULL;
}


MInstanceInfo::ServerState MInstanceInfo::get_server_state()
{
  const char *ps_command= "ps -e";
  //XXX replace with instance manager thing


  
  if (!is_local())
  {
    if (is_connected())
      return SRunning;

    return SUnknown;
  }
  
  gchar *out= NULL;
  gint exst;
  if (g_spawn_command_line_sync(ps_command, &out, NULL, &exst, NULL)
      && exst == 0)
  {
    char *ptr= strtok(out, "\n");
    while (ptr != NULL)
    {
      if (strstr(ptr, "mysqld"))
      {
	g_free(out);
        return SRunning;
      }
      ptr= strtok(NULL, "\n");
    }
  }
  if (out) g_free(out);

  // the pid file approach was not working because the paths
  // can vary a lot and are unpredictable and also because the user
  // administrator is running as might not have permissions to enter
  // the directory it is located.
#if 0
  if (Glib::file_test(DEFAULT_MYSQLD_PID_FILE, Glib::FILE_TEST_EXISTS))
  {
    return SRunning;
  }
#ifdef MYSQLD_PID_FILE_TRY_GUESS
  char *paths[]= {
    "/var/lock/subsys/mysql",
    "/var/run/mysqld/mysqld.pid",
      "/var/run/mysql/mysqld.pid"
      "/var/lib/mysqld/mysqld.pid",
      "/var/lib/mysql/mysqld.pid",
  };
  for (unsigned int i= 0; i < sizeof(paths)/sizeof(char*); i++)
    if (Glib::file_test(paths[i], Glib::FILE_TEST_EXISTS))
      return SRunning;
#endif

  // default path set by the mysql init script
  std::string path= _var_datadir.empty()?"/var/lib/mysql/":_var_datadir;
  
  struct utsname buf;
  std::string hostname= "localhost";
        
  if (uname(&buf) == 0)
  {
    hostname= buf.nodename;
  }
  path+= hostname+".pid";
  
  if (Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    return SRunning;
#endif
  return SStopped;
}
    


#ifdef sun
#define FSTAB_FILE "/etc/vfstab"

static bool do_get_info(MInstanceInfo::FilesystemInfo &info, const std::string &path)
{
  struct statvfs fs;
  
  if (statvfs(path.c_str(), &fs) < 0)
  {
    g_error("could not stat filesystem '%s': %s", path.c_str(),
            strerror(errno));
    return false;
  }

  info.size= (long long)fs.f_bsize * fs.f_blocks;
  info.free= (long long)fs.f_bsize * fs.f_bavail;

  info.type= fs.f_basetype;

  return true;
}


#else // !sun

#if defined(hpux) || defined(__hpux)
# define FSTAB_FILE "/etc/mnttab"
#else
# define FSTAB_FILE "/etc/mtab"
#endif

static bool do_get_info(MInstanceInfo::FilesystemInfo &info, const std::string &path)
{
  struct statfs fs;
  
  if (statfs(path.c_str(), &fs) < 0)
  {
    g_error("could not stat filesystem '%s': %s", path.c_str(),
            strerror(errno));
    return false;
  }

  info.size= (long long)fs.f_bsize * fs.f_blocks;
  info.free= (long long)fs.f_bsize * fs.f_bavail;

  info.type= "unknown";
  for (unsigned int i= 0; i < sizeof(fstypes)/sizeof(FSType); i++)
  {
    if (fstypes[i].id == fs.f_type)
    {
      info.type= fstypes[i].name;
      break;
    }
  }
  return true;
}

#endif


bool MInstanceInfo::get_fs_info(std::list<MInstanceInfo::FilesystemInfo> &fslist)
{
  char buffer[1024];
  FILE *f= fopen(FSTAB_FILE, "r");
  if (!f)
  {
    g_error("could not open filesystem table '%s': %s", FSTAB_FILE, strerror(errno));
    return false;
  }

  while (fgets(buffer, sizeof(buffer), f))
  {
    FilesystemInfo info;

#ifdef sun
    info.fs= strtok(buffer, " \t");
    strtok(NULL, " \t");
    info.path= strtok(NULL, " \t");
#else
    info.fs= strtok(buffer, " \t");
    info.path= strtok(NULL, " \t");
#endif

    // not a real fs
    if (info.fs[0]!='/' && info.fs.find(':')==std::string::npos)
      continue;
    
    if (!do_get_info(info, info.path))
      continue;
    
    fslist.push_back(info);
  }

  fclose(f);

  return true;
}





std::string MInstanceInfo::get_server_path(ServerPath type)
{
  char *path;
  const char *mycnf= myx_get_my_cnf_path();
  MYX_ADMIN_LIB_ERROR err;
  std::string tmp;

  switch (type)
  {
  case PErrorLog:
    if (_local_info && !_local_info->error_log_path.empty())
      return _local_info->error_log_path;

    {
      std::string datadir= _var_datadir.empty()?"/var/lib/mysql/":_var_datadir;
      
      if (!_var_errorlog.empty())
      {
        if (_var_errorlog[0]=='/')
          tmp= _var_errorlog;
        else
          tmp= Glib::build_filename(datadir, "mysql.err");

        if (!Glib::file_test(tmp, Glib::FILE_TEST_EXISTS))
          tmp="";
      }

      // try in my.cnf
      if (tmp.empty() && mycnf)
      {
        path= myx_get_cnf_value(mycnf, "mysqld", "err-log", &err);
        if (path)
        {
          tmp= path;
          free(path);
        }
      }

      // try /var/lib/mysql/hostname.err
      if (tmp.empty())
      {
        struct utsname buf;
        
        if (uname(&buf) == 0)
        {
          tmp= ufmt("/var/lib/mysql/%s.err",buf.nodename);
          if (!Glib::file_test(tmp, Glib::FILE_TEST_EXISTS))
            tmp= "";
        }
      }

      if (tmp.empty())
        tmp= DEFAULT_ERR_LOG_FILE;

      if (_local_info)
        _local_info->error_log_path= tmp;

      return tmp;
    }
    break;

  case PLogs:
    if (_local_info && !_local_info->general_log_path.empty())
      return _local_info->general_log_path;
    {
      if (mycnf)
      {
        path= myx_get_cnf_value(mycnf, "mysqld", "log", &err);
        if (path)
        {
          tmp= path;
          free(path);
        }
      }
      // try /var/lib/mysql/hostname.log
      if (tmp.empty())
      {
        struct utsname buf;
        
        if (uname(&buf) == 0)
        {
          tmp= ufmt("/var/lib/mysql/%s.log",buf.nodename);
          if (!Glib::file_test(tmp.c_str(), Glib::FILE_TEST_EXISTS))
            tmp= "";
        }
      }

      if (tmp.empty())
        tmp= DEFAULT_LOG_FILE;
      
      if (_local_info)
        _local_info->general_log_path= tmp;
      
      return tmp;
    }
  case PSlowLog:
    if (_local_info && !_local_info->slow_log_path.empty())
      return _local_info->slow_log_path;
    
    {
      path= myx_get_cnf_value(mycnf, "mysqld", "log-slow-queries", &err);
      if (path)
      {
        tmp= path;
        free(path);
      }

      if (tmp.empty())
        tmp= DEFAULT_SLOW_LOG_FILE;

      if (_local_info)
        _local_info->slow_log_path= tmp;

      return tmp;
    }
  }
  return "";
}


MYX_MACHINE_INFO *MInstanceInfo::get_client_info()
{
  return myx_get_client_info(get_mysql());
}


MYX_MACHINE_INFO *MInstanceInfo::get_server_info()
{
  MYX_USER_CONNECTION conn;
  _user_conn.fill(&conn);
  return myx_get_server_info(&conn, get_mysql());
}


void MInstanceInfo::mark_log_file()
{
  std::string path= get_server_path(PErrorLog);
  
  if (!path.empty())
    _log_file_pos= get_file_size(path.c_str());
  else
    _log_file_pos= 0;
}

  
Glib::ustring MInstanceInfo::fetch_logs_from_mark()
{
  std::string path= get_server_path(PErrorLog);
  
  if (!path.empty())
  {
    bigint new_size= get_file_size(path.c_str());

    if (new_size > _log_file_pos)
    {
      bigint diff= new_size - _log_file_pos;
      ssize_t len;
      char *buffer= (char*)g_malloc(diff+1);
      int fd= open(path.c_str(), O_RDONLY);

      lseek64(fd, _log_file_pos, SEEK_SET);
      if ((len= read(fd, buffer, diff)) < 0)
      {
        close(fd);
        g_free(buffer);
        return "";
      }
      buffer[len]= 0;
      close(fd);
      Glib::ustring tmp= buffer;
      g_free(buffer);
      return tmp;
    }
  }
  return "";
}

