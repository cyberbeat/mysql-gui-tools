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

#include "mygpriv.h"
#include "MGConnectDialog.h"
#include "myg_gtk_utils.h"
#include "myg_gtkutils.h"
#include "myg_utils.h"
#include <stdarg.h>

#include "MGPreferences.h"
#include "MGPreferencesEditor.h"


#define PROGRESS_INDICATOR_FILE "progress_indicator.gif"

#define GLADE_FILE "connect_dialog.glade"


static const char *disable_while_connecting[]=
{
  "connection_list",
      "hostname_entry",
      "password_entry",
      "username_entry",
      "port_spin",
      "schema_entry",
      "advanced_frame",
      "details_button",
      "clear_button",
      "connect_button",
      NULL
};


static MGAdvancedOption option_list[]=
{
  {"COMPRESS", "option1_check", 'B'},
  {"USE_SSL", "option2_check", 'B'},
  {"SOCKET_PATH", "option1_entry", 'S'},
  {"ANSI_QUOTES", "ansiquotes", 'B'},
  {NULL, NULL, 0}
};


MGConnectDialog::MGConnectDialog(MGPreferences *prefs)
  : _prefs(prefs), _xml(0), _mysql(0), _connections(0),
    _conn_thread(0), _ping_view(0), _ping_output(0), _advanced_shown(false),
    _ignore_changes(0), _skip_connect(false)
{
  _ignore_connection_change= false;
}


MGConnectDialog::~MGConnectDialog()
{  
  delete _xml;

  delete _connections;
}


bool MGConnectDialog::on_key_pressed(GdkEventKey *key)
{
  if (key->keyval == GDK_Control_L || key->keyval == GDK_Control_R)
  {
    _skip_connect= true;
    _xml->get_label("connect_button_label")->set_text(_("Skip"));
    update_connect_sensitivity();
  }
  return false;
}
 

bool MGConnectDialog::on_key_released(GdkEventKey *key)
{
  if (key->keyval == GDK_Control_L || key->keyval == GDK_Control_R)
  {
    _skip_connect= false;
    _xml->get_label("connect_button_label")->set_text(_("Connect"));
    update_connect_sensitivity();
  }
  return false;
}


bool MGConnectDialog::init(int flags, const std::string &header_image_file)
{
  Glib::RefPtr<Gdk::PixbufAnimation> anim;
//  GError *error= NULL;
  Gtk::Widget *wid;
  Gtk::Image *prog;

  try {
    _xml= new MGGladeXML(myg_datadir+"/"GLADE_FILE, "", "mysql-gui-common");
  } catch (...) {
    g_warning("could not load glade xml file '%s'", 
              std::string(myg_datadir+"/"GLADE_FILE).c_str());
    return false;
  }

  if (!header_image_file.empty())
    ((Gtk::Image*)_xml->get_widget("header_image"))->set(PIXCACHE->load(header_image_file,false));

  if (flags & MYG_FLAG_ALLOW_SKIP)
  {
    ((Gtk::Window*)_xml->get_widget("connect_dialog"))->signal_key_press_event().connect(sigc::mem_fun(*this,&MGConnectDialog::on_key_pressed));
    ((Gtk::Window*)_xml->get_widget("connect_dialog"))->signal_key_release_event().connect(sigc::mem_fun(*this,&MGConnectDialog::on_key_released));
  }

  ((Gtk::Window*)_xml->get_widget("connect_dialog"))->signal_delete_event().connect(sigc::mem_fun(*this,&MGConnectDialog::on_window_close));

  _pick_schema= (flags&MYG_FLAG_PICK_SCHEMA)!=0;

  /* enable history in text entries */
  wid= _xml->get_widget("username_entry");
  myg_entry_history_attach(wid->gobj());

  wid= _xml->get_widget("hostname_entry");
  myg_entry_history_attach(wid->gobj());

  wid= _xml->get_widget("schema_entry");
  myg_entry_history_attach(wid->gobj());

  wid= _xml->get_widget("option1_entry");
  myg_entry_history_attach(wid->gobj());

  /* connect signals */
  {
    Gtk::Button *btn;

    btn= (Gtk::Button*)_xml->get_widget("connect_button");
    btn->signal_clicked().connect(sigc::mem_fun(*this, &MGConnectDialog::on_connect_button_clicked));

    btn= (Gtk::Button*)_xml->get_widget("clear_button");
    btn->signal_clicked().connect(sigc::mem_fun(*this, &MGConnectDialog::on_clear_button_clicked));

    btn= (Gtk::Button*)_xml->get_widget("cancel_button");
    btn->signal_clicked().connect(sigc::mem_fun(*this, &MGConnectDialog::on_cancel_button_clicked));

    btn= (Gtk::Button*)_xml->get_widget("details_button");
    btn->signal_clicked().connect(sigc::mem_fun(*this, &MGConnectDialog::on_details_button_clicked));
  }
  {
    Gtk::Entry *entry;

    entry= (Gtk::Entry*)_xml->get_widget("username_entry");
    entry->signal_changed().connect(sigc::mem_fun(*this, &MGConnectDialog::on_check_entry));

    entry= (Gtk::Entry*)_xml->get_widget("password_entry");
//    entry->signal_changed().connect(sigc::mem_fun(*this, &MGConnectDialog::on_check_entry));

    entry= (Gtk::Entry*)_xml->get_widget("hostname_entry");
    entry->signal_changed().connect(sigc::mem_fun(*this, &MGConnectDialog::on_check_entry));

    entry= (Gtk::Entry*)_xml->get_widget("port_spin");
    entry->signal_changed().connect(sigc::mem_fun(*this, &MGConnectDialog::on_check_entry));

    entry= (Gtk::Entry*)_xml->get_widget("schema_entry");
    entry->signal_changed().connect(sigc::mem_fun(*this, &MGConnectDialog::on_check_entry));

    std::list<std::string> dummy;
    myg_show_advanced_options(_xml, option_list, dummy);
  }
  {
    Gtk::OptionMenu *menu;

    menu= (Gtk::OptionMenu*)_xml->get_widget("connection_list");
    menu->signal_changed().connect(sigc::mem_fun(*this,
                                              &MGConnectDialog::on_connection_list_changed));
  }

  /* setup progress animation thingy (libglade seems to be buggy, it loads
   the image as a static image) */
  prog= (Gtk::Image*)_xml->get_widget("progress_image");

  anim= PIXCACHE->load_anim(PROGRESS_INDICATOR_FILE);
  if (!anim)
  {
//    if (error)
//    {
//      printf("%s\n",error->message);
//    }
//    g_error_free(error);
    g_message("could not load animation %s", 
              PROGRESS_INDICATOR_FILE);
  }
  else
  {
    prog->set(anim);
  }

  if (_pick_schema)
  {
    _xml->get_widget("schema_label")->show();
    _xml->get_widget("schema_entry")->show();
  }
  else
  {
    _xml->get_widget("schema_label")->hide();
    _xml->get_widget("schema_entry")->hide();
  }
  
  return true;
}

bool MGConnectDialog::run(const Glib::ustring &title, int flags,
                          const std::string &header_image_file)
{
  return run_with_defaults(title, NULL, flags, header_image_file);
}
  

bool MGConnectDialog::run_with_defaults(const Glib::ustring &title,
                                        MYX::UserConnection *default_conn,
                                        int flags, const std::string &header_image_file)
{
  Gtk::Window *win;
  
  if (!_xml)
  {
    if (!init(flags, header_image_file))
    {
      Gtk::MessageDialog *dlg;

      dlg= new Gtk::MessageDialog(_("Can't show connection dialog.\n"
                                    "Software installation might be incorrect or corrupted."),
                                  false,
                                  Gtk::MESSAGE_ERROR,
                                  Gtk::BUTTONS_OK,
                                  true);
      dlg->run();
      delete dlg;

      return false;
    }
  }
  
  _cancel_clicked= false;
  

  if (_connections)
  {
    delete _connections;
    _connections= 0;
  }
  load_connection_list(_prefs->build_path_to(_prefs->connections_filename));

  update_connect_sensitivity();

  _mysql= NULL;

  win= (Gtk::Window*)_xml->get_widget("connect_dialog");

  win->set_title(title);
  win->show();

  _app_title= title;

  if (default_conn)
  {
    _cur_conn= *default_conn;
    
    /* try to connect with default values */
    display_connection_values(_cur_conn);

    start_connection();
  }

  // run modally
  Gtk::Main::run();
  win->hide();

  if (_mysql) /* successful connection */
  {
    int history= ((Gtk::OptionMenu*)_xml->get_widget("connection_list"))->get_history();

    // add current connection
    if (history == 0)
    {
      _cur_conn.storage_type= MYX_HISTORY_USER_CONNECTION;
      _connections->connections.push_back(_cur_conn);
    }

    if (!default_conn)
    {
      if (history == 0)
      {
        _connections->last_connection= -1;
        for (unsigned int i= 0; i < _connections->connections.size(); i++)
        {
          if (_connections->connections[i] == _cur_conn)
          {
            _connections->last_connection= i;
            break;
          }
        }
      }
      else
      {
        Gtk::MenuItem *item= (Gtk::MenuItem*)((Gtk::Menu*)_xml->get_widget("connection_menu"))->get_active();
        _connections->last_connection= *(int*)item->get_data("user_data");
      }
      /* save connection list */
      myx_store_user_connections(_connections->get_obj(),
                                 _prefs->password_storage_type,
                                 _prefs->build_path_to(_prefs->connections_filename).c_str());
    }
    return true;
  }

  return _skip_connect && !_cancel_clicked ? true : false;
}



static bool is_str_blank(const Glib::ustring &s)
{
  for (unsigned i= 0; i < s.size(); i++)
  {
    if (!isspace(s[i]))
        return false;
  }
  return true;
}


void MGConnectDialog::load_connection_list(const std::string &clpath)
{
  Gtk::Entry *username_e, *hostname_e, *schema_e;

  username_e= (Gtk::Entry*)_xml->get_widget("username_entry");
  hostname_e= (Gtk::Entry*)_xml->get_widget("hostname_entry");
  schema_e= (Gtk::Entry*)_xml->get_widget("schema_entry");

  /* load connection list data */
  if (!clpath.empty()) {

    MYX_USER_CONNECTIONS *connection_list;
    MYX_LIB_ERROR merror;
 
    connection_list= myx_load_user_connections(clpath.c_str(), &merror);
  
    if (connection_list)
    {
      if (_connections)
        delete _connections;
      _connections= new MYX::UserConnectionList(connection_list);
      myx_free_user_connections(connection_list);
    }
    else
    {
      if (!_connections)
      {
        g_warning("could not load connection list (%i)",
                  merror);
        _connections= new MYX::UserConnectionList;
      }
    }
  }

  {
    Gtk::Menu *menu;
    Gtk::MenuItem *item;
    int last_connection_menu_index= -1;
    int menu_index= 0;

    menu= (Gtk::Menu*)_xml->get_widget("connection_menu");
    menu->items().clear();

    item= new Gtk::MenuItem("");
    item->show();
    menu->append(*Gtk::manage(item));
    menu_index++;

    unsigned int index= 0;
    for (std::vector<MYX::UserConnection>::const_iterator iter = _connections->connections.begin();
         iter != _connections->connections.end(); ++iter, index++)
    {
      if (iter->storage_type == MYX_FAVORITE_USER_CONNECTION
          || _connections->last_connection == (int)index)
      {
        if (iter->storage_type == MYX_FAVORITE_USER_CONNECTION)
          item= new Gtk::MenuItem(!iter->connection_name.empty()?iter->connection_name:"-");
        else
          item= new Gtk::MenuItem(_("(last connection)"));

        {
          int *data= g_new(int, 1);
          *data= (index);
          item->set_data("user_data", data, g_free);
        }

        if (iter->storage_type == MYX_FAVORITE_USER_CONNECTION)
        {
          menu->append(*Gtk::manage(item));
          if (_connections->last_connection == (int)index)
            last_connection_menu_index= menu_index;
        }
        else
        {
          menu->insert(*Gtk::manage(item), 1);
          if (_connections->last_connection == (int)index)
            last_connection_menu_index= 1;
        }
        menu_index++;
      }
      if (!iter->username.empty())
          myg_entry_history_add(GTK_WIDGET(username_e->gobj()), iter->username.c_str());
      if (!iter->hostname.empty())
          myg_entry_history_add(GTK_WIDGET(hostname_e->gobj()), iter->hostname.c_str());
      if (!iter->schema.empty())
          myg_entry_history_add(GTK_WIDGET(schema_e->gobj()), iter->schema.c_str());

      //if (!iter->schema.empty())
      //    myg_entry_history_add(GTK_WIDGET(schema_e->gobj()), iter->schema.c_str());
    }

    /* add extras */
    menu->append(*Gtk::manage(myg_make_separator_item()));
    item= Gtk::manage(new Gtk::MenuItem(_("Save This Connection...")));
    item->signal_activate().connect(sigc::mem_fun(*this,&MGConnectDialog::save_connection));
    menu->append(*item);
    item= Gtk::manage(new Gtk::MenuItem(_("Open Connection Editor")));
    item->signal_activate().connect(sigc::mem_fun(*this,&MGConnectDialog::open_connection_editor));
    menu->append(*item);
    menu->show_all();

    /* set the last connection */
    if (_connections->last_connection >= 0)
    {
      if (last_connection_menu_index > 0)
      {
        Gtk::OptionMenu *menu= (Gtk::OptionMenu*)_xml->get_widget("connection_list");
        menu->set_history(last_connection_menu_index);
      }

      if (_connections->last_connection < (int)_connections->connections.size())
        _cur_conn.assign(_connections->connections[_connections->last_connection]);
    }
  }
}


void MGConnectDialog::update_connect_sensitivity()
{
  bool can_connect= true;

  /* check if everything is filled and we can connect */
  // anonymous username is ok
  //if (is_str_blank(((Gtk::Entry*)_xml->get_widget("username_entry"))->get_text()))
  //  can_connect= false;

  if (!_skip_connect)
  {
    if (is_str_blank(((Gtk::Entry*)_xml->get_widget("hostname_entry"))->get_text()))
      can_connect= false;

    /*
    if (_pick_schema)
    {
      if (is_str_blank(((Gtk::Entry*)_xml->get_widget("schema_entry"))->get_text()))
        can_connect= false;
    }
     */
  }
  /* enable/disable button */
  _xml->get_widget("connect_button")->set_sensitive(can_connect);
}


void *MGConnectDialog::connection_thread(void *data)
{
  MGConnectDialog *me= (MGConnectDialog*)data;
  MYX_USER_CONNECTION conn;

  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
  /* check if we got a cancelation before setting it async */
  pthread_testcancel();

  mysql_thread_init();

  me->_cur_conn.fill(&conn);
  
  me->_conn_result= -1;
  me->_conn_result= myx_connect_to_instance(&conn, me->_mysql);

  mysql_thread_end();

  me->_connecting= false;

  return NULL;
}


bool MGConnectDialog::process_ping_output(Glib::IOCondition cond)
{
  char buffer[256];

  if (cond == Glib::IO_IN && fgets(buffer, sizeof(buffer), _ping_output))
  {
    append_text_to_ping_view(buffer);
  }
  else
  {
    /* error or end of data, simulate click on stop button */
    _ping_dialog->response(1);
  }
  
  return true;
}


Gtk::TextView *MGConnectDialog::add_text_to_ping_dialog()
{
  Gtk::TextView *text;
  Gtk::ScrolledWindow *scroll= new Gtk::ScrolledWindow();
  Gtk::Frame *frame;

  scroll->set_shadow_type(Gtk::SHADOW_IN);
  scroll->set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC);

  frame= new Gtk::Frame();
  frame->set_border_width(10);
  frame->set_shadow_type(Gtk::SHADOW_NONE);
  frame->add(*Gtk::manage(scroll));
  scroll->show();
  
  text= new Gtk::TextView;
  text->set_editable(false);
  text->set_cursor_visible(false);

  scroll->add(*Gtk::manage(text));
  text->show();
  
  frame->set_size_request(-1, 100);

  _ping_dialog->get_vbox()->pack_start(*Gtk::manage(frame), true, true, 0);
  frame->show();
  
  return text;
}


void MGConnectDialog::append_text_to_ping_view(const Glib::ustring &msg)
{
  Gtk::TextIter iter;
  Glib::RefPtr<Gtk::TextBuffer> text_buf= _ping_view->get_buffer();

  iter= text_buf->end();

  text_buf->insert(iter, msg);

  iter= text_buf->end();

  _ping_view->scroll_to_iter(iter, 0.0);
}


void MGConnectDialog::save_connection()
{
  Glib::ustring name;

  _ignore_connection_change= true;

  // select back the original item
  ((Gtk::OptionMenu*)_xml->get_widget("connection_list"))->set_history(_current_selected_item);

  _ignore_connection_change= false;

  if (myg_ask_string(*(Gtk::Window*)_xml->get_widget("connect_dialog"),
                     _("Save Connection"),
                     _("Type in a name for the connection to be saved."),
                     name) && !name.empty())
  {
    MYX::UserConnection con;
    bool ok= true;
    bool replace= false;
    
    if (_connections->find_connection(name, con))
    {
      Gtk::MessageDialog dlg(_("<b>Replace Connection</b>\n"
                               "There is already a connection with the same name.\nReplace?"),
                             true,
                             Gtk::MESSAGE_WARNING,
                             Gtk::BUTTONS_CANCEL, true);
      dlg.add_button(_("Replace"), 42);
      ok= false;
      if (dlg.run() == 42)
      {
        replace= true;
        ok= true;
      }
    }
        
    if (ok)
    {
      fetch_connection_values(con);
      con.connection_name= name;
      con.connection_type= MYX_MYSQL_CONN;
      con.storage_type= MYX_FAVORITE_USER_CONNECTION;
      
      if (!replace)
      {
        _connections->connections.push_back(con);
        _connections->last_connection= _connections->connections.size()-1;
      }
      else
        _connections->update_connection(name, con);

      myx_store_user_connections(_connections->get_obj(),
                                 _prefs->password_storage_type,
                                 _prefs->build_path_to(_prefs->connections_filename).c_str());

      load_connection_list(_prefs->build_path_to(_prefs->connections_filename));
    }
  }
}


void MGConnectDialog::preferences_changed()
{
  load_connection_list(_prefs->build_path_to(_prefs->connections_filename));
}

void MGConnectDialog::preferences_closed()
{
  Gtk::Main::instance()->quit();
}


bool MGConnectDialog::on_window_close(GdkEventAny *ev)
{
  bool cancel= _cancel_clicked;
  
  on_cancel_button_clicked();
  
  _cancel_clicked= cancel;
  
  return true;
}


void MGConnectDialog::open_connection_editor()
{
  // select back the original item
  ((Gtk::OptionMenu*)_xml->get_widget("connection_list"))->set_history(_current_selected_item);

  sigc::connection c1= MGPreferencesEditor::instance()->signal_closed().connect(sigc::mem_fun(*this,
                                                                                           &MGConnectDialog::preferences_closed));
  sigc::connection c2= MGPreferencesEditor::instance()->signal_changed().connect(sigc::mem_fun(*this,
                                                                                            &MGConnectDialog::preferences_changed));
  MGPreferencesEditor::instance()->show(true);
  MGPreferencesEditor::instance()->set_modal(true);
  Gtk::Main::instance()->run();
  MGPreferencesEditor::instance()->set_modal(false);
  c1.disconnect();
  c2.disconnect();
}


void MGConnectDialog::show_connection_failed()
{
  Gtk::Button *btn;
  pid_t ping_pid= 0;
  sigc::connection input_handler;
  int rc;
  bool pinging= false;
  int myerror= 0;
  Glib::ustring title= _app_title + " " + _("Error");
  char *ping_args[]= 
  {
    "/bin/ping",
        NULL,
        NULL
  };
  char *tmp;

  _ping_dialog= new Gtk::MessageDialog(*(Gtk::Window*)_xml->get_widget("connect_dialog"),
                                       ufmt(_("Could not connect to host '%s'.\n"
                                              "MySQL Error Nr. %i\n"
                                              "%s\n\n"
                                              "Click the 'Ping' button to see if there is a networking problem."),
                                            _cur_conn.hostname.c_str(),
                                            (myerror=myx_mysql_errno(_mysql)),
                                            (tmp=myx_mysql_error(_mysql))),
                                       false,
                                       Gtk::MESSAGE_ERROR,
                                       Gtk::BUTTONS_OK,
                                       true);
  g_free(tmp);

  btn= _ping_dialog->add_button(_("_Ping Host"), 1);

  while ((rc= _ping_dialog->run()))
  {
    if (pinging || rc == Gtk::RESPONSE_OK)
    {
      pinging= false;
      btn->set_label(_("_Ping Host"));
      
      if (_ping_output)
      {
        input_handler.disconnect();
        myx_pclose(_ping_output, ping_pid);
        _ping_output= NULL;
        ping_pid= 0;
      }

      if (rc == Gtk::RESPONSE_OK)
          break;
    }
    else
    {
      if (!_ping_view)
      {
        _ping_view= add_text_to_ping_dialog();
      }
      else
      {
        _ping_view->get_buffer()->set_text("");
      }
      
      if (_ping_output)
      {
        myx_pclose(_ping_output, ping_pid);
      }
      
      ping_args[1]= (char*)_cur_conn.hostname.c_str();
      
      _ping_output= myx_popen(ping_args, &ping_pid);
      if (!_ping_output)
      {
        myg_text_append(GTK_WIDGET(_ping_view->gobj()), 
                        _("Could not execute ping\n"));
      }
      else
      {
        input_handler= Glib::signal_io().connect(sigc::mem_fun(*this, &MGConnectDialog::process_ping_output),
                                                 fileno(_ping_output),
                                                 Glib::IO_IN|Glib::IO_HUP);
        pinging= true;
        btn->set_label(_("Stop _Ping"));
      }
    }
  }

  if (myerror == 1045) // Access denied
    _xml->get_widget("password_entry")->grab_focus();
  else if (myerror == 2005) // Unknown host
    _xml->get_widget("hostname_entry")->grab_focus();
  else if (myerror == 1049 && _xml->get_widget("schema_entry")->is_visible()) // Unknown database
    _xml->get_widget("schema_entry")->grab_focus();

  delete _ping_dialog;

  _ping_dialog= 0;
  _ping_view= 0;
}


bool MGConnectDialog::monitor_connection_thread()
{
  if (!_connecting)
  {
    void *dummy;

    /* finished connecting */
    pthread_join(_conn_thread, &dummy);
    _conn_thread= 0;

    /* reenable interface elements */
    for (const char **p= disable_while_connecting; *p; p++) {
      _xml->get_widget(*p)->set_sensitive(true);
    }

    _xml->get_widget("progress_image")->hide();

    if (_conn_result == 0)
    {
      /* success! */
      Gtk::Main::quit();
    }
    else
    {
      show_connection_failed();
      
      myx_mysql_close(_mysql);
      _mysql= NULL;
    }
    return false;
  }

  return true;
}


void MGConnectDialog::start_connection()
{
  fetch_connection_values(_cur_conn);

  /* disable interface elements while connecting... */
  for (const char **p= disable_while_connecting; *p; p++) 
  {
    _xml->get_widget(*p)->set_sensitive(false);
  }

  _xml->get_widget("progress_image")->show();

  if (!_conn_thread)
  {
    _connecting= true;

    _mysql= myx_mysql_init();
    /* start connection thread */
    pthread_create(&_conn_thread, NULL, connection_thread, this);

    Glib::signal_idle().connect(sigc::mem_fun(*this, &MGConnectDialog::monitor_connection_thread));
  }
}


void MGConnectDialog::stop_connection()
{
  /* stop connection thread */
  if (_conn_thread)
  {
    void *ret;
    pthread_cancel(_conn_thread);
    pthread_join(_conn_thread, &ret);
    _conn_thread= 0;
  }

  if (_mysql)
  {
    g_message("destroying mysql connection");
    myx_mysql_close(_mysql);
    g_message("done");
    _mysql= NULL;
  }
  /* reenable interface elements */
  for (const char **p= disable_while_connecting; *p; p++)
  {
    _xml->get_widget(*p)->set_sensitive(true);
  }
  _xml->get_widget("progress_image")->hide();
}



void MGConnectDialog::reset_connection_values()
{
  Gtk::OptionMenu *menu;
  Gtk::Entry *entry;

  menu= (Gtk::OptionMenu*)_xml->get_widget("connection_list");
  menu->set_history(0);

  entry= (Gtk::Entry*)_xml->get_widget("username_entry");
  entry->set_text("");

  entry= (Gtk::Entry*)_xml->get_widget("password_entry");
  entry->set_text("");

  entry= (Gtk::Entry*)_xml->get_widget("hostname_entry");
  entry->set_text("");

  entry= (Gtk::Entry*)_xml->get_widget("schema_entry");
  entry->set_text("");

  ((Gtk::SpinButton*)_xml->get_widget("port_spin"))->set_value(MYSQL_PORT);
}


void MGConnectDialog::display_connection_values(const MYX::UserConnection &conn)
{
  Gtk::Entry *entry;
  Gtk::SpinButton *spin;

  _ignore_changes++;
  
  entry= (Gtk::Entry*)_xml->get_widget("username_entry");
  entry->set_text(conn.username);

  entry= (Gtk::Entry*)_xml->get_widget("password_entry");
  entry->set_text(conn.password);
  if (conn.password.empty())
  {
    entry->grab_focus();
  }

  entry= (Gtk::Entry*)_xml->get_widget("hostname_entry");
  entry->set_text(conn.hostname);
  
  spin= (Gtk::SpinButton*)_xml->get_widget("port_spin");

  spin->set_value(conn.port);
  
  if (_pick_schema)
  {
    entry= (Gtk::Entry*)_xml->get_widget("schema_entry");
    entry->set_text(conn.schema);
  }

  myg_show_advanced_options(_xml, option_list, conn.advanced_options);

  _ignore_changes--;
}


void MGConnectDialog::fetch_connection_values(MYX::UserConnection &conn)
{
  Gtk::Entry *entry;
  Gtk::SpinButton *spin;
  Glib::ustring username, password, hostname, schema;
  int port;

  entry= (Gtk::Entry*)_xml->get_widget("username_entry");
  username= entry->get_text();

  entry= (Gtk::Entry*)_xml->get_widget("password_entry");
  password= entry->get_text();

  entry= (Gtk::Entry*)_xml->get_widget("hostname_entry");
  hostname= entry->get_text();

  spin= (Gtk::SpinButton*)_xml->get_widget("port_spin");
  port= spin->get_value_as_int();

  entry= (Gtk::Entry*)_xml->get_widget("schema_entry");
  schema= entry->get_text();

  myg_fetch_advanced_options(_xml, option_list, conn.advanced_options);
  
  conn.username= username;
  conn.password= password;
  conn.hostname= hostname;
  conn.port= port;
  conn.schema= schema;
}


/* signal handlers */

void MGConnectDialog::on_connect_button_clicked()
{
  if (_skip_connect)
  {
    myg_image_button_set(*_xml->get_button("connect_button"),
                         Gtk::Stock::OK, _("Connect"));
    Gtk::Main::instance()->quit();
  }
  else
    start_connection();
}


void MGConnectDialog::on_clear_button_clicked()
{
  reset_connection_values();
}


void MGConnectDialog::on_cancel_button_clicked()
{
  _cancel_clicked= true;
  if (_conn_thread)
    stop_connection();
  else
    Gtk::Main::instance()->quit();
}


void MGConnectDialog::on_check_entry()
{
  if (!_ignore_changes)
    ((Gtk::OptionMenu*)_xml->get_widget("connection_list"))->set_history(0);
 
  update_connect_sensitivity();
}


void MGConnectDialog::on_details_button_clicked()
{
  Gtk::Frame *frame;
  Gtk::Button *btn;

  frame= (Gtk::Frame*)_xml->get_widget("advanced_frame");
  btn= (Gtk::Button*)_xml->get_widget("details_button");
  
  if (_advanced_shown)
  {
    _advanced_shown= 0;
    frame->hide();
    
    btn->set_label(_("_Details >>"));
  }
  else
  {
    _advanced_shown= 1;
    frame->show();

    btn->set_label(_("_Details <<"));
  }
}


void MGConnectDialog::on_connection_list_changed()
{
  if (_ignore_connection_change)
    return;

  Gtk::MenuItem *item= (Gtk::MenuItem*)((Gtk::Menu*)_xml->get_widget("connection_menu"))->get_active();

  int *index= (int*)item->get_data("user_data");

  _current_selected_item= ((Gtk::OptionMenu*)_xml->get_widget("connection_list"))->get_history();
  
  if (index)
  {
    display_connection_values(_connections->connections[*index]);
    _cur_conn.assign(_connections->connections[*index]);
  }
  else
    _cur_conn.assign(MYX::UserConnection());
}


