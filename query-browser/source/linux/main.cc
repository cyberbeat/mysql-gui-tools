/* Copyright (C) 2003, 2004 MySQL AB

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

#include "myqb.h"

#include "MQMainWindow.h"
#include "myg_gtkutils.h"

#include "MQPreferencesEditor.h"

#include "html.h"

#include <string>
#include <MGConnectDialog.h>

#include <errno.h>

#include <mysql.h>



// global
MQPreferences prefs;

static char *argv0= NULL;

// cmd line options
static bool args_do_not_connect= false;
static MYX::UserConnection args_user_conn;
static bool args_user_conn_set= false;
static std::string *arg_file = NULL;
static std::string *arg_query = NULL;
static std::string *arg_select = NULL;

static char *get_arg(char **argv, int index)
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
      return argv[index+1];
    else
    {
      if (strlen(arg)==2) /* -f bla */
        return argv[index+1];
      else
        return argv[index]+2; /* -fbla */
    }
  }
}


bool parse_cmd_line_args(int argc, char **argv)
{
  char *value;
  int i;

#define CHECK_OPTION(s,v) (Glib::str_has_prefix(s,v"=") || strcmp(s,v)==0)
  
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
    else if (Glib::str_has_prefix(argv[i], "--mycnf="))
    {
      value= get_arg(argv, i);
      if (!value)
        goto missing_arg;
      myx_set_my_cnf_path(value);
    }
    else if (strncmp(argv[i], "-f", 2)==0 || CHECK_OPTION(argv[i],"--file"))
    {
      value = get_arg(argv, i);
      if (!value)
        goto missing_arg;
      arg_file = new std::string(value);
    }
    else if (strncmp(argv[i], "-q", 2)==0 || CHECK_OPTION(argv[i],"--query"))
    {
      value = get_arg(argv, i);
      if (!value)
        goto missing_arg;
      arg_query = new std::string(value);
    }
    else if (strncmp(argv[i], "-t", 2)==0 || CHECK_OPTION(argv[i],"--select-table"))
    {
      value = get_arg(argv, i);
      if (!value)
        goto missing_arg;
      arg_select = new std::string(value);
    }
    else if (strcmp(argv[i], "--dont-connect")==0)
    {
      args_do_not_connect= true;
    }
    else if (CHECK_OPTION(argv[i],"--mqb-dir"))
    {
      setenv("MQB_DIR", get_arg(argv, i), 1);
    }
    else if (strcmp(argv[i], "--help")==0 || strcmp(argv[i], "-h")==0)
    {
      Glib::ustring name= g_get_prgname();
      if (Glib::str_has_suffix(name, "-bin"))
        name= name.substr(0, name.size()-4);
      g_print("MySQL Query Browser Ver %s\n", VERSION);
      g_print("Copyright (c) 2004, 2005, 2006 MySQL AB\n");
      g_print(_("Usage: %s [OPTIONS]\n"), name.c_str());
      g_print(_("  -h,--help                Display this help and exit.\n"));
      g_print(_("  -u,--user=name           User for login if not current user.\n"));
      g_print(_("  -p,--password=s          Password to use when connecting to server.\n"));
      g_print(_("  -h,--host=name           Connect to host.\n"));
      g_print(_("  -P,--port=#              Port number to use for connection.\n"));
      g_print(_("  -S,--socket=name         Socket file to use for connection.\n"));
      g_print(_("  --dont-connect           Start without connecting to a MySQL server.\n"));
      g_print(_("  --mycnf=path             Path for my.cnf file.\n"));
      g_print(_("  -f,--file=path             SQL script file to be processed on startup\n"));
      g_print(_("  -q,--query=string          SQL query to be processed on startup\n"));
      g_print(_("  -t,--select-table=string   SQL SELECT query to be processed on startup\n"));
      exit(0);
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
  MQPreferenceGroup *group= new MQPreferenceGroup();
  ed->addGroup(group, _("Query Browser"), PIXCACHE->load("MySQLIcon_QueryBrowser_48x48.png"));
  
  ed->init();
}


int main(int argc, char **argv)
{
  Glib::thread_init();

  argv0= g_strdup(argv[0]);
  
  // force read of our custom style file
  Gtk::RC::add_default_file("query-browser.rc");
  Gtk::RC::add_default_file(get_app_file("query-browser.rc"));

  Gtk::Main app(argc, argv, true);

#ifndef WITH_GTKHTML2
  // gnome is used by gtkhtml and needs to be initialized at some point (like here)
  gnome_program_init(PACKAGE, VERSION, LIBGNOME_MODULE,
                     argc, argv,
                     NULL,
                     GNOME_PARAM_NONE);
#endif
#ifdef ENABLE_NLS
  bindtextdomain(GETTEXT_PACKAGE, std::string(get_prefix()+DATADIRNAME"/locale").c_str());
  bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
  textdomain(GETTEXT_PACKAGE);

  myx_init_library(std::string(get_prefix()+DATADIRNAME).c_str());
#endif

  if (!parse_cmd_line_args(argc, argv))
    return 1;

  if (!getenv(PATH_ENV))
  {
    g_print(_("You must start this program using mysql-query-browser.\n"));
    exit(0);
  }

  // add search paths for images
  {
    std::string prefix= get_prefix()+DATADIRNAME"/mysql-gui";
    PIXCACHE->add_search_path(prefix+"/query-browser");
    PIXCACHE->add_search_path(prefix+"/"+COMMONDIRNAME);
    PIXCACHE->add_search_path(prefix);
  }

  // set datadir for myx library
  myg_set_datadir(get_prefix()+DATADIRNAME+"/mysql-gui/"+COMMONDIRNAME);
  
  // load preferences
  prefs.load("mysqlqb_options.xml");

  // check app specific rc dir
  prefs.check_directory(prefs.build_path_to("query-browser"), true);

  setup_preferences_editor();
  
  html_init();
  
  {
    MQMainWindow *mainw= MQMainWindow::create();
    MYSQL *mysql= NULL;
    MYX::UserConnection user_connection;

    if (!mainw)
    {
      g_error(_("Error initializing GUI"));
      return 1;
    }

    if (!args_do_not_connect)
    {
      MGConnectDialog connect_dlg(&prefs);

      if (args_user_conn_set)
      {
        if (connect_dlg.run_with_defaults("MySQL Query Browser",
                                          &args_user_conn, 
                                          MGConnectDialog::MYG_FLAG_PICK_SCHEMA,
                                          "connect_header_qb.png"))
        {
          mysql= connect_dlg.get_connection();
          user_connection= connect_dlg.get_user_connection();
        }
        else // if we tried to connect using cmd line defaults, exit
          return 0;
      }
      else
      {
        if (connect_dlg.run("MySQL Query Browser",
                            MGConnectDialog::MYG_FLAG_PICK_SCHEMA,
			    "connect_header_qb.png"))
        {
          mysql= connect_dlg.get_connection();
          user_connection= connect_dlg.get_user_connection();
        }
        else
          return 0;
      }
    }

    if (!mainw->init(mysql, user_connection))
    {
      Gtk::MessageDialog dlg(_("Error initializing GUI"), 
                              false,
                              Gtk::MESSAGE_ERROR,
                              Gtk::BUTTONS_OK);
      dlg.run();
      return 1;
    }

    mainw->show();

		if(NULL != arg_query)
		{
			mainw->execute_query_raw(arg_query->c_str());
		}
		
		if(NULL != arg_file)
		{
			mainw->execute_script_file_new_window(arg_file->c_str());
		}

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


std::string get_app_file(const std::string &file)
{
  return get_prefix() + DATADIRNAME + "/mysql-gui/query-browser/"+file;
}

void start_new_instance()
{
  g_spawn_command_line_async(argv0, NULL);
}
