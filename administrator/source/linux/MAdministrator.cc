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


#include "myadmin.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "MGGladeXML.h"

#include "MAdministrator.h"
#include "MAPanel.h"
#include "MDataInterface.h"

#include "myg_gtk_utils.h"
#include "myg_gtkutils.h"

#include "MGConnectDialog.h"
#include "MGPreferencesEditor.h"
#include "MGAboutPanel.h"

#include "MAServerInformationPanel.h"
#include "MAServiceControlPanel.h"
#include "MAStartupParametersPanel.h"
#include "MAServerConnectionsPanel.h"
#include "MAUserAdministrationPanel.h"
#include "MAHealthPanel.h"
#include "MAServerLogsPanel.h"
#include "MABackupPanel.h"
#include "MARestorePanel.h"
#include "MAReplicationStatusPanel.h"
#include "MACatalogsPanel.h"
#include "MADummyPanel.h"


#define COPYRIGHT_STRING "(c) Copyright 2004-2007 by MySQL AB. All rights reserved."

#define CREDITS_STRING "Michael G. Zinner: Graphic Design, Windows GUI, Library.\nAlfredo K. Kojima: Linux GUI, Library.\n"\
                       "Mike Lischke: Windows development.    Victor Vagin: Library, QA\n"\
                       "Ulrich Bayer: Library, WIX.    Brian Aker: Main concept.\n"\
                       "Mike Hillyer: Documentation."


static struct sidebar_item {
  char *icon;
  char *icon_gray;
  Glib::ustring label;
  char *menu_item;
  MAPanel*(*instantiate)(MAdministrator*,MDataInterface*);
} sidebar_item_list[]=
{
  { "server_info.png", "server_info_gray.png", 
      N_("Server Information"), "server_information1",
      create_server_information_panel },
  { "service_config.png", "service_config_gray.png",
      N_("Service Control"), "service_control1",
      create_service_control_panel },
  { "startup_variables.png", "startup_variables_gray.png",
      N_("Startup Parameters"), "startup_parameters1",
      create_startup_parameters_panel },
  { "user_admin.png", "user_admin_gray.png",
      N_("User Administration"), "user_administration1",
      create_user_administration_panel },
  { "server_connections.png", "server_connections_gray.png",
      N_("Server Connections"), "server_connections1",
      create_server_connections_panel },
  { "health.png", "health_gray.png",
      N_("Health"), "health1",
      create_health_panel },
  { "server_logs.png", "server_logs_gray.png",
      N_("Server Logs"), "server_logs1",
      create_server_logs_panel },
  { "backup.png", "backup_gray.png",
      N_("Backup"), "backup1",
      create_backup_panel },
  { "restore.png", "restore_gray.png",
      N_("Restore Backup"), "restore1",
      create_restore_panel },
  { "replication.png", "replication_gray.png",
      N_("Replication Status"), "replication_status1",
      create_replication_status_panel },
  { "catalogs.png", "catalogs_gray.png",
      N_("Catalogs"), "catalogs1",
      create_catalogs_panel },
};


void MAdministrator::populate_sidebar()
{  
  unsigned int i;
  _side_store= Gtk::TreeStore::create(_sidebar_columns);
  bool is_local, is_connected;
  Gdk::Color normal= _tree->get_style()->get_fg(Gtk::STATE_NORMAL);
  Gdk::Color gray= _tree->get_style()->get_fg(Gtk::STATE_INSENSITIVE);

  is_connected= _instance->is_connected();
  is_local= _instance->is_local();

  for (i= 0; i < sizeof(sidebar_item_list)/sizeof(struct sidebar_item); i++)
  {
    Gtk::TreeModel::iterator iter= _side_store->append();
    Gtk::TreeModel::Row row= *iter;
    MAPanel *panel= 0;
    bool is_active= true;

    panel= (*sidebar_item_list[i].instantiate)(this, _dataif);

    if (panel->is_local_only() && !is_local)
      is_active= false;
    if (panel->needs_connection() && !is_connected)
      is_active= false;

    row[_sidebar_columns._panel]= panel;

    // set menu callback
    Gtk::MenuItem *item= (Gtk::MenuItem*)get_widget(sidebar_item_list[i].menu_item);
    item->signal_activate().connect(sigc::bind<int>(sigc::mem_fun(*this,&MAdministrator::switch_panel_num),i));

    // set icon
    try
    {
      row[_sidebar_columns._icon]= PIXCACHE->load(is_active
                                               ?sidebar_item_list[i].icon
                                               :sidebar_item_list[i].icon_gray);
    } 
    catch (...) 
    {
      g_message("cant load icon %s\n", sidebar_item_list[i].icon);
    }

    row[_sidebar_columns._text]= sidebar_item_list[i].label;

    row[_sidebar_columns._color]= is_active ? normal : gray;
  }

  _tree->set_model(_side_store);
}



void MAdministrator::setup_sidebar()
{
  Glib::RefPtr<Gtk::TreeSelection> select;

  Gtk::TreeView::Column *column= new Gtk::TreeView::Column("");
  column->pack_start(_sidebar_columns._icon, false);
  column->pack_start(_sidebar_columns._text);
  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  column->add_attribute(((Gtk::CellRendererText*)rends[1])->property_foreground_gdk(),
                       _sidebar_columns._color);
  _tree->append_column(*column);

  populate_sidebar();

  select= _tree->get_selection();
  select->signal_changed().connect(sigc::mem_fun(*this, &MAdministrator::tree_selection_changed_cb));
  select->set_mode(Gtk::SELECTION_BROWSE);
  select->select(Gtk::TreePath("0"));
}


Gtk::Widget *MAdministrator::get_widget(const Glib::ustring &name)
{
  Gtk::Widget *w= _xml->get_widget(name);

  if (!w)
  {
    g_critical("request for non-existent widget %s in glade XML", name.c_str());
    g_assert_not_reached();
  }

  return w;
}


MAdministrator::MAdministrator()
  : _xml(0), _prefs_editor(0), _tmp_status(0), _instance(0), 
    _dataif(0), _changing_panel(false)
{
  _current_panel= NULL;
}


MAdministrator::~MAdministrator()
{
  delete _dataif;
  delete _xml;
}



bool MAdministrator::init(MInstanceInfo *inst)
{
  _instance= inst;
  _dataif= new MDataInterface(this, inst);
  
  _xml= new MGGladeXML(get_app_file(GLADE_MAINWINDOW_FILE), "", "mysql-administrator");
  if (!_xml)
  {
    Gtk::MessageDialog dlg(ufmt(_("The user interface file (%s) could not be loaded.\n"
                                  "%s was probably not installed correctly."),
                                get_app_file(GLADE_MAINWINDOW_FILE).c_str(),
                                PACKAGE),
                           false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
    return false;
  }

  _win= (Gtk::Window*)get_widget("main_window");
  _tree= (Gtk::TreeView*)get_widget("sidebar_tree");

  // edit menu
  _xml->get_menu_item("menuitem2")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::update_edit_menu));
  _xml->get_menu_item("cut1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::cut_cb));
  _xml->get_menu_item("copy1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::copy_cb));
  _xml->get_menu_item("paste1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::paste_cb));
  
  _win->signal_delete_event().connect(sigc::mem_fun(*this,&MAdministrator::close_cb));
  _xml->get_menu_item("new_connection1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::new_connection_cb));
  _xml->get_menu_item("close_connection1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::close_connection_cb));
  _xml->get_menu_item("reconnect1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::reconnect_cb));
  _xml->get_menu_item("quit1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::menu_quit_cb));

//  _xml->get_menu_item("sidebar1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::toggle_sidebar_cb));
  _xml->get_menu_item("contents1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::open_help));
  _xml->get_menu_item("about1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::menu_about_cb));

  _xml->get_menu_item("manage_connections1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MAdministrator::menu_preferences_cb),true));
  _xml->get_menu_item("preferences1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MAdministrator::menu_preferences_cb),false));
  _xml->get_menu_item("mysql_text_console1")->signal_activate().connect(sigc::mem_fun(*this,&MAdministrator::start_console));

  _xml->get_menu_item("home1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url),
                                                                                    "https://enterprise.mysql.com"));
  _xml->get_menu_item("software1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url),
                                                                                        "https://enterprise.mysql.com/software/enterprise.php"));
  _xml->get_menu_item("knowledge_base1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url), 
                                                                                              "https://kb.mysql.com/index.php"));
  _xml->get_menu_item("update_service1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url), 
                                                                                              "https://enterprise.mysql.com/updates/index.php"));
  _xml->get_menu_item("monitoring_service1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url), 
                                                                                                  "https://enterprise.mysql.com/monitoring/"));
  _xml->get_menu_item("technical_support1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MAdministrator::open_url), 
                                                                                                 "https://support.mysql.com/main.php"));

  _progressbar= (Gtk::ProgressBar*)get_widget("status_progress");

  _dummy_panel= new MADummyPanel(this, _dataif);
  
  setup_sidebar();

  ((Gtk::Button*)_xml->get_widget("status_button"))->signal_clicked().connect(sigc::mem_fun(*this, &MAdministrator::cancel_button_clicked_cb));
/*
  if (!_transl.open(get_app_file("mysqlx_translations_general.xml").c_str(),
                    get_app_file("mysqlx_translations_administrator.xml").c_str(),
                    "en"))
  {
    g_warning(_("Could not open translation files"));
  }
*/
  _instance->signal_disconnect().connect(sigc::mem_fun(*this,&MAdministrator::disconnect_db_handler));
  _instance->signal_connect().connect(sigc::mem_fun(*this,&MAdministrator::connect_db_handler));

  // set window title
  if (_instance->is_connected())
  {
    MYX::UserConnection conn= _instance->get_connection_data();
    if (_instance->is_socket_connection())
      _win->set_title(ufmt(MY_NAME" %s@%s via socket",
                           conn.username.c_str(), conn.hostname.c_str()));
    else
      _win->set_title(ufmt(MY_NAME" %s@%s:%i",
                           conn.username.c_str(), conn.hostname.c_str(), conn.port));
  }
  else
  {
    _win->set_title(ufmt(MY_NAME" %s", _("(not connected)")));
  }

  // setup icons
  std::list<Glib::RefPtr<Gdk::Pixbuf> > icons;
  icons.push_back(PIXCACHE->load("MySQLIcon_Admin_16x16.png"));
  icons.push_back(PIXCACHE->load("MySQLIcon_Admin_32x32.png")),
  icons.push_back(PIXCACHE->load("MySQLIcon_Admin_48x48.png"));
  _win->set_icon_list(icons);

  return true;
}


MInstanceInfo *MAdministrator::get_instance() const
{
  g_assert(_instance != 0);

  return _instance; 
}


void MAdministrator::show()
{
//  _win->set_size_request(1024, 768);
  _win->show();

  // calculate the min size of the sidebar to fit all rows
  int w= 0, h= 0;
  
  Gdk::Rectangle rect;
  for (Gtk::TreeIter iter= _side_store->children().begin();
       iter != _side_store->children().end(); ++iter)
  {
    _tree->get_background_area(Gtk::TreePath(iter),
                               *_tree->get_column(0), rect);
    w= max(w, rect.get_width());
    h += rect.get_height();
  }

  static_cast<Gtk::Paned*>(get_widget("main_paned"))->set_position(w);
  _tree->set_size_request(w, h);

//  _win->maximize();
}


void MAdministrator::disconnect_db_handler()
{
  update_sidebar(true);
  
  if (_current_panel && _current_panel->needs_connection())
  {
    ((MADummyPanel*)_dummy_panel)->set_connection_text();
    switch_panel(_dummy_panel);
  }
}



void MAdministrator::connect_db_handler()
{
  MYX::UserConnection conn= _instance->get_connection_data();
  
  if (_instance->is_socket_connection())
    _win->set_title(ufmt(MY_NAME" %s@%s via socket",
                         conn.username.c_str(), conn.hostname.c_str()));
  else
    _win->set_title(ufmt(MY_NAME" %s@%s:%i",
                         conn.username.c_str(), conn.hostname.c_str(), conn.port));

  update_sidebar();
  
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    
    switch_panel(row[_sidebar_columns._panel]);
  }
  
  _instance->refresh_server_info();
}


void MAdministrator::update_sidebar(bool disconnecting)
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  Gdk::Color normal= _tree->get_style()->get_fg(Gtk::STATE_NORMAL);
  Gdk::Color gray= _tree->get_style()->get_fg(Gtk::STATE_INSENSITIVE);

  int i= 0;
  for (iter= _side_store->children().begin();
       iter != _side_store->children().end();
       ++iter, i++)
  {
    row= *iter;
    MAPanel *panel= row[_sidebar_columns._panel];
    
    if ((panel->needs_connection() && (!_instance->is_connected() || disconnecting))
        || (panel->is_local_only() && !_instance->is_local()))
    {
      row[_sidebar_columns._icon]= PIXCACHE->load(sidebar_item_list[i].icon_gray);
      row[_sidebar_columns._color]= gray;
    }
    else
    {
      row[_sidebar_columns._icon]= PIXCACHE->load(sidebar_item_list[i].icon);
      row[_sidebar_columns._color]= normal;
    }
  }
}


void MAdministrator::quit()
{
  Gtk::TreeIter iter;
  bool ok= true;

  for (iter= _side_store->children().begin();
       iter != _side_store->children().end();
       ++iter)
  {
    Gtk::TreeModel::Row row= *iter;
    MAPanel *panel= row[_sidebar_columns._panel];
    
    if (panel)
    {
      if (!panel->before_quit())
      {
        ok= false;
        break;
      }
    }
  }
  
  if (ok)
  {
    prefs.save();

    Gtk::Main::instance()->quit();
  }
}


Gtk::Container *MAdministrator::parent_frame()
{
  return (Gtk::Container*)get_widget("parent_frame");
}


Gtk::Window *MAdministrator::window()
{
  return _win;
}


void MAdministrator::switch_panel(const Glib::ustring &name)
{
  Gtk::TreeModel::Children children= _side_store->children();

  for (Gtk::TreeIter iter= children.begin(); iter != children.end(); ++iter)
  {
    Gtk::TreeModel::Row row= *iter;
    if (row[_sidebar_columns._text] == name)
    {
      _tree->get_selection()->select(iter);
      break;
    }
  }
}


void MAdministrator::switch_panel_num(int index)
{
  if (_changing_panel)
      return;

  Gtk::TreeModel::Children children= _side_store->children();
  
  for (Gtk::TreeIter iter= children.begin(); iter != children.end(); ++iter)
  {
    Gtk::TreeModel::Row row= *iter;
    if (index--==0)
    {
      if (!_tree->get_selection()->get_selected() || _tree->get_selection()->get_selected()!=iter)
        _tree->get_selection()->select(iter);
      break;
    }
  }
}


void MAdministrator::switch_panel(MAPanel *panel)
{
  bool changed= false;

  if (panel->is_local_only() && !_instance->is_local())
  {
    panel= _dummy_panel;
    ((MADummyPanel*)_dummy_panel)->set_local_text();
  }
  if (panel->needs_connection() && !_instance->is_connected())
  {
    panel= _dummy_panel;
    ((MADummyPanel*)_dummy_panel)->set_connection_text();
  }
  
  if (panel == _current_panel)
    return;

  if (panel->init())
  {
    if (_current_panel)
    {
      if (_current_panel->before_hide() && panel->before_show())
      {
        _current_panel->hide();
        panel->show();

        _current_panel= panel;
        changed= true;
      }
    }
    else
    {
      if (panel->before_show())
      {
        panel->show();
        _current_panel= panel;
        
        changed= true;
      }
    }
  }
  
  // select the corresponding menu item
  Gtk::TreeModel::Children ch= _side_store->children();
  int i= 0;
  for (Gtk::TreeIter it= ch.begin(); it != ch.end(); ++it, ++i)
  {
    if (it == _tree->get_selection()->get_selected())
      break;
  }
  _changing_panel= true;
  if (!((Gtk::RadioMenuItem*)get_widget(sidebar_item_list[i].menu_item))->get_active())
      ((Gtk::RadioMenuItem*)get_widget(sidebar_item_list[i].menu_item))->activate();
  _changing_panel= false;

  if (!changed)
  {
    //XXX 
    // undo tree selection change
  }
}


void MAdministrator::add_side_panel(Gtk::Widget *panel)
{
  Gtk::Box *box= (Gtk::Box*)get_widget("sidebar_box");
  Gtk::Widget *cnt= _tree->get_parent();

  // readd the tree non-expanding
  cnt->reference();
  box->remove(*cnt);
  box->pack_start(*cnt, false, true);

  box->pack_start(*panel, true, true, 0);
  panel->show();
}


void MAdministrator::remove_side_panel(Gtk::Widget *panel)
{
  Gtk::Box *box= (Gtk::Box*)get_widget("sidebar_box");
  Gtk::Widget *cnt= _tree->get_parent();

  // readd the tree expanding
  cnt->reference();
  box->remove(*cnt);
  box->pack_start(*cnt, true, true);

  panel->reference();
  panel->hide();
  box->remove(*panel);
}


Gtk::MenuItem *MAdministrator::add_section_menu(Gtk::Menu *menu, const Glib::ustring &label)
{
  Gtk::MenuItem *item;

  item= new Gtk::MenuItem(label, true);
  item->set_submenu(*menu);

  ((Gtk::MenuBar*)get_widget("main_menu"))->insert(*item, 3);

  return item;
}



void MAdministrator::push_status(const Glib::ustring &text)
{
  Gtk::Statusbar *s= (Gtk::Statusbar*)get_widget("status_bar");

  if (_tmp_status)
  {
    s->remove_message(_tmp_status);
    _tmp_status= 0;
  }
  s->push(text);
}


void MAdministrator::pop_status()
{
  Gtk::Statusbar *s= (Gtk::Statusbar*)get_widget("status_bar");

  s->pop();
}


void MAdministrator::set_status(const Glib::ustring &text)
{
  Gtk::Statusbar *s= (Gtk::Statusbar*)get_widget("status_bar");

  _tmp_status= s->push(text);
}


void MAdministrator::push_stop_button_handler(const sigc::slot0<void> &slot)
{
  _stop_button_handlers.push_back(slot);
}

void MAdministrator::pop_stop_button_handler()
{
  _stop_button_handlers.pop_back();
}


void MAdministrator::set_busy(const Glib::ustring &message)
{
  Gtk::ProgressBar *pbar;
  Gtk::Button *btn;
  Gtk::Paned *paned;
  Gtk::MenuBar *menu;

  pbar= (Gtk::ProgressBar*)get_widget("status_progress");
  btn= (Gtk::Button*)get_widget("status_button");
  paned= (Gtk::Paned*)get_widget("main_paned");
  menu= (Gtk::MenuBar*)get_widget("main_menu");

  if (!message.empty())
  {
    pbar->show();
    btn->show();
    menu->set_sensitive(false);
    paned->set_sensitive(false);

    pbar->set_text(_("Please wait..."));

    push_status(message);
    
    set_busy_progress(true);
  }
  else
  {
    set_busy_progress(false);

    pbar->set_text("");
    pbar->set_fraction(0.0);
    pop_status();

    pbar->hide();
    btn->hide();
    menu->set_sensitive(true);
    paned->set_sensitive(true);
  }
}


void MAdministrator::set_busy_progress(bool start)
{
  if (start)
  {
    if (!_pulse_conn)
      _pulse_conn= Glib::signal_timeout().connect(sigc::mem_fun(*this, &MAdministrator::pulse_progress),
                                                  10000);
  }
  else
  {
    if (_pulse_conn)
      _pulse_conn.disconnect();
  }
}


bool MAdministrator::pulse_progress()
{
  _progressbar->pulse();
  return true;
}


/**********************************************************************/

void MAdministrator::new_connection_cb()
{
  extern void start_new_instance();
  
  start_new_instance();
  /*
  MGConnectDialog connect_dlg(&prefs);

  if (connect_dlg.run("MySQL Administrator", 0))
  {
    if (_instance->is_connected())
    {
      _instance->disconnect();
      _win->set_title(ufmt(MY_NAME" %s", _("(not connected)")));
    }

    _instance->set_connection(connect_dlg.get_connection(), 
                              connect_dlg.get_user_connection());
  }
   */
}


void MAdministrator::close_connection_cb()
{
  if (_instance->is_connected())
  {
    _instance->disconnect();
    _win->set_title(ufmt(MY_NAME" %s", _("(not connected)")));
  }
}


void MAdministrator::menu_about_cb()
{
  MGAboutPanel *about= new MGAboutPanel(get_app_file("about.png"),
                                        MY_NAME,
                                        VERSION,
                                        COPYRIGHT_STRING,
                                        CREDITS_STRING);
//  about->set_credits();
  about->set_transient_for(*window());
  about->set_delete_on_close();
  about->show();
}


void MAdministrator::menu_preferences_cb(bool conn_only)
{
  if (!_prefs_editor)
  {
    _prefs_editor= MGPreferencesEditor::instance();
    _prefs_editor->set_transient_for(*_win);
    _prefs_editor->signal_changed().connect(sigc::mem_fun(*this,&MAdministrator::prefs_changed));
  }

  _prefs_editor->show(conn_only);
}


void MAdministrator::prefs_changed()
{
  _signal_prefs_changed.emit();
}


void MAdministrator::menu_quit_cb()
{
  quit();
}


bool MAdministrator::close_cb(GdkEventAny *ev)
{
  quit();
  return true;
}


void MAdministrator::tree_selection_changed_cb()
{
  Gtk::TreeModel::iterator iter;

  iter= _tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeModel::Row row= *iter;

    switch_panel(row[_sidebar_columns._panel]);
  }
}


void MAdministrator::cancel_button_clicked_cb()
{
  if (_stop_button_handlers.empty())
    _instance->cancel_data_fetch();
  else
    _stop_button_handlers.back()();
}


void MAdministrator::open_help()
{
  std::string path;

#ifdef DOCDIR
  path= DOCDIR"/mysql-administrator/index.html";
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+"/doc/administrator.html"; // /opt/mysql-administrator/doc
#else
  path= get_prefix()+"/doc/administrator.html"; // /opt/mysql-administrator/doc
#endif
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+DATADIRNAME+"/doc/mysql-administrator/administrator.html"; // /usr/share/doc
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+DATADIRNAME+"/mysql-gui/doc/administrator/administrator.html"; // /usr/share/mysql-gui/doc
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= "/usr/share/doc/packages/mysql-administrator/administrator.html"; // location in SUSE^M

  g_spawn_command_line_async(ufmt("desktop-launch %s",path.c_str()).c_str(), NULL);
}

/*
void MAdministrator::toggle_sidebar_cb()
{
  bool show= static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("sidebar1"))->get_active();

  if (show)
    _xml->get_widget("scrolledwindow1")->show();
  else
    _xml->get_widget("scrolledwindow1")->hide();
}
 */


void MAdministrator::update_edit_menu()
{
  Gtk::Widget *widget= _win->get_focus();
  bool cut= false;
  bool copy= false;
  bool paste= false;
  
  if (!widget) return;
  
  if (GTK_IS_LABEL(widget->gobj()))
  {
    Gtk::Label *label= (Gtk::Label*)widget;
    int s, e;

    if (label->get_selection_bounds(s,e) > 0)
    {
      copy= true;
    }
  }
  else if (GTK_IS_EDITABLE(widget->gobj()))
  {
    Gtk::Editable *editable= (Gtk::Editable*)widget;
    int s, e;
    
    if (editable->get_selection_bounds(s,e) > 0)
    {
      cut= true;
      copy= true;
    }
    if (Gtk::Clipboard::get()->wait_is_text_available() && editable->get_editable())
      paste= true;
  }
  else if (GTK_IS_TEXT_VIEW(widget->gobj()))
  {
    Gtk::TextView *tview= (Gtk::TextView*)widget;
    Gtk::TextIter s, e;
    
    if (tview->get_buffer()->get_selection_bounds(s, e))
    {
      cut= true;
      copy= true;
    }
    if (Gtk::Clipboard::get()->wait_is_text_available() && tview->get_editable())
      paste= true;
  }
  _xml->get_menu_item("cut1")->set_sensitive(cut);
  _xml->get_menu_item("copy1")->set_sensitive(copy);
  _xml->get_menu_item("paste1")->set_sensitive(paste);
}


void MAdministrator::cut_cb()
{
  Gtk::Widget *widget= _win->get_focus();
  if (!widget) return;
  if (GTK_IS_EDITABLE(widget->gobj()))
  {
    Gtk::Editable *editable= (Gtk::Editable*)widget;

    editable->cut_clipboard();
  }
  else if (GTK_IS_TEXT_VIEW(widget->gobj()))
  {
    Gtk::TextView *tview= (Gtk::TextView*)widget;

    tview->get_buffer()->cut_clipboard(Gtk::Clipboard::get());
  }
}


void MAdministrator::copy_cb()
{
  Gtk::Widget *widget= _win->get_focus();
  if (!widget) return;
  if (GTK_IS_LABEL(widget->gobj()))
  {
    Gtk::Label *label= (Gtk::Label*)widget;
    Glib::ustring text= label->get_label();
    int s, e;
    
    if (label->get_selection_bounds(s,e))
    {
      Gtk::Clipboard::get()->set_text(text.substr(s, e));
    }

  }
  else if (GTK_IS_EDITABLE(widget->gobj()))
  {
    Gtk::Editable *editable= (Gtk::Editable*)widget;

    editable->copy_clipboard();
  }
  else if (GTK_IS_TEXT_VIEW(widget->gobj()))
  {
    Gtk::TextView *tview= (Gtk::TextView*)widget;

    tview->get_buffer()->copy_clipboard(Gtk::Clipboard::get());
  }
}


void MAdministrator::paste_cb()
{
  Gtk::Widget *widget= _win->get_focus();
  if (!widget) return;
  if (GTK_IS_EDITABLE(widget->gobj()))
  {
    Gtk::Editable *editable= (Gtk::Editable*)widget;

    editable->paste_clipboard();
  }
  else if (GTK_IS_TEXT_VIEW(widget->gobj()))
  {
    Gtk::TextView *tview= (Gtk::TextView*)widget;

    tview->get_buffer()->paste_clipboard(Gtk::Clipboard::get());
  }
}



void MAdministrator::start_console()
{
  MYX::UserConnection conn= _instance->get_connection_data();

  if (Glib::find_program_in_path("mysql").empty())
  {
    Gtk::MessageDialog dlg(_("<b>MySQL Client Missing</b>\n\n"
                             "The mysql command line client is missing from the search path."
                             "Please make sure it is installed and accessible."),
                           true,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK);
    dlg.run();
  }
  else
  {
    if (!Glib::find_program_in_path("gnome-terminal").empty())
      Glib::spawn_command_line_async(ufmt("gnome-terminal --disable-factory --title='mysql' -e \"mysql -u%s -h%s -P%i %s\"",
                                          conn.username.c_str(), conn.hostname.c_str(), conn.port,
                                          conn.password.empty()?"":std::string("-p"+Glib::shell_quote(conn.password)).c_str()));
    else
      Glib::spawn_command_line_async(ufmt("xterm  -T mysql -e \"mysql -u%s -h%s -P%i %s\"",
                                          conn.username.c_str(), conn.hostname.c_str(), conn.port,
                                          conn.password.empty()?"":std::string("-p"+Glib::shell_quote(conn.password)).c_str()));
  }
}


void MAdministrator::reconnect_cb()
{
  if (!_instance->reconnect())
  {
    set_status(_("Reconnection failed."));
  }
  else
    set_status(_("Reconnected."));
}




void MAdministrator::open_merlin_page()
{
//  system("gnome-www-browser http://www.mysql.com/network/&");
  system("firefox http://www.mysql.com/network/&");
}


void MAdministrator::open_url(const Glib::ustring &url)
{
//  system("gnome-www-browser http://www.mysql.com/network/&");
  system(Glib::ustring("firefox "+url+"&").c_str());
}


