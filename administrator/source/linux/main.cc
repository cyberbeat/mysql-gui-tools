/* Copyright (C) 2003, 2004, 2005, 2006, 2007 MySQL AB

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

#include "MAdministrator.h"

#include "MABackupPanel.h"
#include "MYXInterface.h"
#include "MAPreferencesEditor.h"

#include <string>
#include <MGConnectDialog.h>
#include "myg_gtkutils.h"
#include "myg_utils.h"

#include <errno.h>

#include <mysql.h>



// global
MAPreferences prefs;


// cmd line options
static bool args_do_not_connect= false;
static MYX::UserConnection args_user_conn;
static bool args_user_conn_set= false;

static char *argv0= NULL;
static char *startup_dir= NULL;

static std::string args_connection;
static bool args_backup= false;

static void save_startup_dir();

static char *get_arg(char **argv, int &index, int option_length=1)
{
  char *arg= argv[index];
  char *ptr= strchr(arg,'=');
  if (ptr)
  {
    if (*(ptr+1)) /* --foo=bla */
      return ptr+1;
    else
      return NULL; /* --foo= */
  }
  else
  {
    if (strncmp(arg,"--",2)==0) /* --foo bla */
      return argv[++index];
    else
    {
      if (strlen(arg)==(unsigned)option_length+1) /* -f bla */
        return argv[++index];
      else
        return argv[index]+1+option_length; /* -fbla */
    }
  }
}


bool parse_cmd_line_args(int argc, char **argv)
{
  char *value;
  int i;

#define CHECK_OPTION(s,v) (Glib::str_has_prefix(s,v"+") || strcmp(s,v)==0)
  
  for (i= 1; i < argc; i++)
  {
    if (strncmp(argv[i], "-u", 2)==0 || CHECK_OPTION(argv[i],"--user"))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      args_user_conn.username= value;
      args_user_conn_set= true;
    }
    else if (strncmp(argv[i], "-p", 2)==0 || CHECK_OPTION(argv[i],"--password"))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      args_user_conn.password= value;
      args_user_conn_set= true;
    }
    else if (strncmp(argv[i], "-h", 2)==0 || CHECK_OPTION(argv[i],"--host"))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      args_user_conn.hostname= value;
      args_user_conn_set= true;
    }
    else if (strncmp(argv[i], "-P", 2)==0 || CHECK_OPTION(argv[i],"--port"))
    {
      int port;
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      if (sscanf(value, "%i", &port)!=1 || port < 1 || port > (1<<16))
      {
        g_print(_("Bad port number value '%s' for option\n"), value);
        return false;
      }
      args_user_conn.port= port;
      args_user_conn_set= true;
    }
    else if (strncmp(argv[i], "-S", 2)==0 || CHECK_OPTION(argv[i],"--socket"))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      args_user_conn.advanced_options.push_back(ufmt("SOCKET_PATH=%s", value));
    }
    else if (strncmp(argv[i], "-c", 2)==0 || CHECK_OPTION(argv[i],"--connection"))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      args_connection= value;
    }
    else if (Glib::str_has_prefix(argv[i], "--mycnf="))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      myx_set_my_cnf_path(value);
    }
    else if (strcmp(argv[i], "--dont-connect")==0)
    {
      args_do_not_connect= true;
    }
    else if (strncmp(argv[i], "-bp", 3)==0)
    {
      value= get_arg(argv, i, 2);
      if (!value)
        goto missing_arg;
      args_backup= true;
    }
    else if (strncmp(argv[i], "-bt", 3)==0
             || strncmp(argv[i], "-bx", 3)==0)
    {
      value= get_arg(argv, i, 2);
      if (!value)
        goto missing_arg;
      // dont do anything, they'll be handled by the backup code
    }
    else if (strcmp(argv[i], "--help")==0)
    {
      g_print("%s Ver %s\n", MY_NAME, VERSION);
      g_print("%s\n", COPYRIGHT_NOTE);
      g_print(_("Usage: %s [OPTIONS]\n"), argv0);
      g_print(_("  --help                Display this help and exit.\n"));
      g_print(_("  -u,--user=name        User for login if not current user.\n"));
      g_print(_("  -p,--password=s       Password to use when connecting to server.\n"));
      g_print(_("  -h,--host=name        Connect to host.\n"));
      g_print(_("  -P,--port=#           Port number to use for connection.\n"));
      g_print(_("  -S,--socket=name      Socket file to use for connection.\n"));
      g_print(_("  -c,--connection=name  Specify the name of a connection to use.\n"));
      g_print(_("  -bp name 		 Execute backup with named profile.\n"));
      g_print(_("  -bt target 		 Target path for backup.\n"));
      g_print(_("  -bx prefix 		 Prefix for backup file.\n"));
      g_print(_("  -c,--connection=name  Specify the name of a connection to use.\n"));
      g_print(_("  --dont-connect        Start without connecting to a MySQL server.\n"));
      g_print(_("  --mycnf=path          Path for my.cnf file.\n"));
      exit(0);
    }
    else if (strncmp(argv[i], "--g-", 4)==0)
    {
    }
    else
    {
      g_print(_("%s: unknown option %s\n"), g_get_prgname(), argv[i]);
      return false;
    }
  }
  
  
  if (args_user_conn_set)
  {
    // set some defaults
    if (args_user_conn.hostname.empty())
      args_user_conn.hostname= "localhost";
    if (!args_user_conn.port)
      args_user_conn.port= 3306;
  }
  
  
  return true;

missing_arg:
  g_print(_("%s: missing value for option %s\n"), g_get_prgname(), argv[i]);
  return false;
}


static void setup_preferences_editor()
{
  MGPreferencesEditor::setup(&prefs);
  MGPreferencesEditor *ed= MGPreferencesEditor::instance();
  MAPreferenceGroup *group= new MAPreferenceGroup();
  ed->addGroup(group, _("Administrator"), PIXCACHE->load("MySQLIcon_Admin_48x48.png"));
  
  ed->init();
}

static bool get_connection_named(const Glib::ustring &name,
                                 MYX::UserConnection &connection)
{
  MYX_LIB_ERROR merror;
  MYX_USER_CONNECTIONS *connection_list;

  // try to connect using passed named connection
  connection_list= myx_load_user_connections(prefs.build_path_to(prefs.connections_filename).c_str(),
                                             &merror);
  if (!connection_list)
  {
    g_print(_("%s: Could not load connection list file: %s\n"),
            g_get_prgname(),
            myg_message_for_xlib_error(merror).c_str());
    return false;
  }

  bool ok= false;
  for (unsigned int i= 0; i < connection_list->user_connections_num; i++)
  {
    if (strcmp2(connection_list->user_connections[i].connection_name, 
                name.c_str())==0)
    {
      connection= MYX::UserConnection(connection_list->user_connections+i);
      ok= true;
      break;
    }
  }
  
  myx_free_user_connections(connection_list);
  
  return ok;
}


void perform_backup(int argc, char **argv)
{
  std::string profile;
  std::string target;
  std::string prefix;
  char *value;
  
  MYX_USER_CONNECTION user_conn;
  MYSQL *mysql;
  bool ok= true;
  int i;
  
  for (i= 1; i < argc; i++)
  {
    if (strncmp(argv[i], "-bp", 3)==0)
    {
      value= get_arg(argv, i, 2);
      if (!value)
        goto missing_arg;
      profile= value;
    }
    else if (strncmp(argv[i], "-bt", 3)==0)
    {
      value= get_arg(argv, i, 2);
      if (!value)
        goto missing_arg;
      target= value;
    }
    else if (strncmp(argv[i], "-bx", 3)==0)
    {
      value= get_arg(argv, i, 2);
      if (!value)
        goto missing_arg;
      prefix= value;
    }
  }

  mysql= myx_mysql_init();
  
  // try to connect using cmd line arguments
  if (args_user_conn_set)
  {
    args_user_conn.fill(&user_conn);
    if (myx_connect_to_instance(&user_conn, mysql) < 0)
      ok= false;
  }
  else
    ok= false;

  if (!ok && !args_connection.empty())
  {
    MYX::UserConnection conn;
    
    if (get_connection_named(args_connection, conn))
    {
      MYX_USER_CONNECTION user_conn;

      conn.fill(&user_conn);
      if (myx_connect_to_instance(&user_conn, mysql) < 0)
        ok= false;
      else
        ok= true;
    }
    else
    {
      g_printerr(_("%s: invalid connection name %s"),
                 g_get_prgname(), args_connection.c_str());
      exit(1);
    }
  }

  if (!ok)
  {
    g_printerr(_("%s: could not connect to MySQL:\n%s (%i)\n"),
            g_get_prgname(),
            mysql_error(mysql),
            mysql_errno(mysql));
    exit(1);
  }

  // perform backup
  if (!MABackupPanel::execute_backup(mysql, profile, target, prefix))
  {
    exit(1);
  }
  return;
  
missing_arg:
  g_print(_("%s: missing value for option %s\n"), g_get_prgname(), argv[i]);
  exit(1);
}

#ifdef ENABLE_GT
extern "C" { 
extern void init_snoop_monkey(); 
}
#endif

#include "myx_admin_public_interface.h"
int main(int argc, char **argv)
{
  save_startup_dir();

  Glib::ustring open_panel= getenv("DEFAULT_PANEL")?:"";

  if (!getenv(PATH_ENV))
  {
    g_print(_("You must start this program through \"mysql-administrator\".\n"));
    exit(0);
  }

  argv0= g_strdup(argv[0]);

  myx_init_library(std::string(get_prefix()+DATADIRNAME).c_str());
#ifdef ENABLE_NLS
  bindtextdomain(GETTEXT_PACKAGE, std::string(get_prefix()+DATADIRNAME+"/locale").c_str());
  bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
  textdomain(GETTEXT_PACKAGE);
#endif

  // hack for binary release
  // since the statically linked client lib checks for my.cnf in /etc, 
  // it wouldnt work in distros with it in /etc/mysql/
#ifdef MY_CNF_PATH_TRY_GUESS
  if (Glib::file_test(DEFAULT_MY_CNF_PATH, Glib::FILE_TEST_EXISTS))
    myx_set_my_cnf_path(DEFAULT_MY_CNF_PATH);
  else if (Glib::file_test("/etc/my.cnf", Glib::FILE_TEST_EXISTS))
    myx_set_my_cnf_path("/etc/my.cnf");
  else if (Glib::file_test("/etc/mysql/my.cnf", Glib::FILE_TEST_EXISTS))
    myx_set_my_cnf_path("/etc/mysql/my.cnf");
#else
  if (Glib::file_test(DEFAULT_MY_CNF_PATH, Glib::FILE_TEST_EXISTS))
    myx_set_my_cnf_path(DEFAULT_MY_CNF_PATH);
#endif

  if (!parse_cmd_line_args(argc, argv))
    return 1;

  // load preferences
  prefs.load("mysqla_options.xml");

  // check app specific rc dir
  prefs.check_directory(prefs.build_path_to("administrator"), true);

  // set datadir for myx library
  myg_set_datadir(get_prefix()+DATADIRNAME+"/mysql-gui/"+COMMONDIRNAME);


  // if backup cmd line options were passed, process them
  if (args_backup)
  {
    g_set_prgname(strrchr(argv[0],'/')?strrchr(argv[0],'/')+1:argv[0]);
    perform_backup(argc, argv);
    return 0;
  }

  // init gtk
  Gtk::Main app(argc, argv, true);

#ifdef ENABLE_GT
  {
   init_snoop_monkey();
  }
#endif
  // add search paths for images
  {
    std::string prefix= get_prefix()+DATADIRNAME+"/mysql-gui";
    PIXCACHE->add_search_path(prefix+"/administrator");
    PIXCACHE->add_search_path(prefix+"/"+COMMONDIRNAME);
    PIXCACHE->add_search_path(prefix);
  }
  
  setup_preferences_editor();

  {
    MAdministrator madmin;
    MYSQL *mysql= NULL;
    MYX::UserConnection user_connection;

    MInstanceInfo instance(&madmin);
    
    if (!madmin.init(&instance))
    {
      g_error(_("Error initializing GUI"));
      return 1;
    }

    if (!args_do_not_connect)
    {
      MGConnectDialog connect_dlg(&prefs);

      if (args_user_conn_set || !args_connection.empty())
      {
        MYX::UserConnection user_conn;
        bool ok= true;

        if (args_user_conn_set)
          user_conn.assign(args_user_conn);
        else
        {
          if (!get_connection_named(args_connection, user_conn))
            ok= false;
        }

        if (connect_dlg.run_with_defaults(MY_NAME, ok ? &user_conn : 0, 
                                          MGConnectDialog::MYG_FLAG_ALLOW_SKIP))
        {
          mysql= connect_dlg.get_connection();
          user_connection= connect_dlg.get_user_connection();
        }
        else
        {
          // if we tried to connect using cmd line defaults, exit
          return 1;
        }
      }
      else
      {
        if (connect_dlg.run(MY_NAME, MGConnectDialog::MYG_FLAG_ALLOW_SKIP,
                            "connect_header_admin.png"))
        {
          mysql= connect_dlg.get_connection();
          user_connection= connect_dlg.get_user_connection();
        }
        else
          return 0;
      }
    }

    instance.set_connection(mysql, user_connection);

    madmin.show();

    if (!open_panel.empty())
        madmin.switch_panel(open_panel);

    app.run();
  }

  return 0;
}


std::string get_prefix()
{
  if (getenv(PATH_ENV))
    return std::string(getenv(PATH_ENV))+"/";
  else
    return PREFIX"/";
}


std::string get_glade_file(const std::string &file)
{
  return get_prefix() + DATADIRNAME + "/mysql-gui/administrator/"+file;
}


std::string get_app_file(const std::string &file)
{
  return get_prefix() + DATADIRNAME + "/mysql-gui/administrator/"+file;
}


void start_new_instance()
{
  g_spawn_command_line_async(argv0, NULL);
}

std::string get_argv0()
{
  return argv0;
}

static void save_startup_dir()
{
  size_t size = 100;

  while (1)
  {
    startup_dir = (char *) malloc (size);
    if (getcwd (startup_dir, size) == startup_dir)
    {
      break;
    }
    free (startup_dir);
    size *= 2;
  }
}

std::string get_startup_dir()
{
  return startup_dir;
}
