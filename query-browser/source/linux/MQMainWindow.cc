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

#include "myqb.h"

#include "MQWorkArea.h"
#include "MQMainWindow.h"
#include "MGGladeXML.h"

#include <gtk/gtkwindow.h>

#include "MGPreferencesEditor.h"

#include "MGAboutPanel.h"

#include "MQQueryDispatcher.h"

#include "myg_gtkutils.h"

#define COPYRIGHT_STRING "(c) Copyright 2004-2007 by MySQL AB. All rights reserved."

#define CREDITS_STRING "Michael G. Zinner: Graphic Design, Windows GUI, Library.\nAlfredo K. Kojima: Linux GUI, Library.\n"\
                       "Mike Lischke: Windows development.    Victor Vagin: Library, QA\n"\
                       "Ulrich Bayer: Library, WIX.    Brian Aker: Main concept.\n"\
                       "Mike Hillyer: Documentation."


MQMainWindow::MQMainWindow(GtkWindow *win)
  : MQMainWindowInterface(win),
    _bookmarks(new MGGladeXML(get_app_file("query_browser.glade"),
                              "add_bookmark_dialog")),
    _has_status(false)
{
  _bookmarks.signal_changed().connect(sigc::mem_fun(*this,&MQMainWindow::bookmarks_changed));
  
  _state= new MQSavedState();
}


MQMainWindow *MQMainWindow::create()
{
  MGGladeXML *xml= new MGGladeXML(get_app_file("workarea.glade"),
                                  "main_window");
  
  MQMainWindow *mainw= 0;
  xml->get_widget_derived("main_window", mainw);

  mainw->_xml= xml;
  
  return mainw;
}


bool MQMainWindow::init(MYSQL *mysql, const MYX::UserConnection &conn)
{
  _workarea= MQWorkArea::create(_xml, this);

  _workarea->setup();

  _dispatcher= new MQQueryDispatcher(mysql, conn);
  _dispatcher->set_owner_widget(this);

  _workarea->set_dispatcher(_dispatcher);

  signal_delete_event().connect(sigc::mem_fun(*this,&MQMainWindow::close_clicked));

  _workarea->add_result_view("");
  if (_state->first_time)
  {
    _workarea->show_quickstart();
    _workarea->add_script_view("Script");
    _state->save();
  }

  bookmarks_changed();

  if (mysql->unix_socket)
    set_title(ufmt("MySQL Query Browser - %s@%s via socket",
                   conn.username.c_str(), conn.hostname.c_str()));
  else
    set_title(ufmt("MySQL Query Browser - %s@%s:%i",
                   conn.username.c_str(), conn.hostname.c_str(), conn.port));

  std::list<Glib::RefPtr<Gdk::Pixbuf> > icons;
  icons.push_back(PIXCACHE->load("MySQLIcon_QueryBrowser_16x16.png"));
  icons.push_back(PIXCACHE->load("MySQLIcon_QueryBrowser_32x32.png")),
  icons.push_back(PIXCACHE->load("MySQLIcon_QueryBrowser_48x48.png"));
  set_icon_list(icons);

  
  // bind menu items
  _xml->get_menu_item("new_connection2")->signal_activate().connect(sigc::mem_fun(*this,&MQMainWindow::new_connection));
  _xml->get_menu_item("manage_connections1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQMainWindow::show_preferences),true));
  _xml->get_menu_item("preferences1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQMainWindow::show_preferences),false));

  _xml->get_menu_item("about1")->signal_activate().connect(sigc::mem_fun(*this,&MQMainWindow::about));

  _xml->get_menu_item("mysql_text_console1")->signal_activate().connect(sigc::mem_fun(*this,&MQMainWindow::start_console));

  
  _xml->get_menu_item("home1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url),
                                                                                    "https://enterprise.mysql.com"));
  _xml->get_menu_item("software1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url),
                                                                                        "https://enterprise.mysql.com/software/enterprise.php"));
  _xml->get_menu_item("knowledge_base1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url), 
                                                                                              "https://kb.mysql.com/index.php"));
  _xml->get_menu_item("update_service1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url), 
                                                                                              "https://enterprise.mysql.com/updates/index.php"));
  _xml->get_menu_item("monitoring_service1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url), 
                                                                                                  "https://enterprise.mysql.com/monitoring/"));
  _xml->get_menu_item("technical_support1")->signal_activate().connect(sigc::bind<Glib::ustring>(sigc::mem_fun(*this,&MQMainWindow::open_url), 
                                                                                                 "https://support.mysql.com/main.php"));
  return true;
}



void MQMainWindow::start_console()
{
  MYX::UserConnection conn= _dispatcher->get_user_connection();

/* terminal start up but immediately exits later, don't know why. -akk
  if (!Glib::find_program_in_path("gnome-terminal").empty())
    Glib::spawn_command_line_async(ufmt("gnome-terminal -e \"mysql -u%s -h%s -P%i %s\"",
                                        conn.username.c_str(), conn.hostname.c_str(), conn.port,
                                        conn.password.empty()?"":std::string("-p"+conn.password).c_str()));
  else
 */
    Glib::spawn_command_line_async(ufmt("xterm -e \"mysql -u%s -h%s -P%i %s\"",
                                        conn.username.c_str(), conn.hostname.c_str(), conn.port,
                                        conn.password.empty()?"":std::string("-p"+conn.password).c_str()));
}


void MQMainWindow::show()
{
  MQMainWindowInterface::show();
  _workarea->show();
}

void MQMainWindow::quit()
{
  prefs.save();
  
  _bookmarks.save();
  
  Gtk::Main::instance()->quit();
}


void MQMainWindow::about()
{
  MGAboutPanel *about= new MGAboutPanel(get_app_file("about.png"),
                                        "MySQL Query Browser",
                                        VERSION,
                                        COPYRIGHT_STRING,
                                        CREDITS_STRING);
  about->set_transient_for(*this);
  about->set_delete_on_close();
  about->show();
}


bool MQMainWindow::close_clicked(GdkEventAny *ev)
{
  Gtk::Main::instance()->quit();
  return true;
}


void MQMainWindow::set_status(const Glib::ustring &text)
{
  Gtk::Statusbar *sb= static_cast<Gtk::Statusbar*>(_xml->get_widget("statusbar1"));
  
  if (_has_status)
    sb->pop();
  _has_status= false;

  if (!text.empty())
  {
    sb->push(text);
    _has_status= true;
  }
}


void MQMainWindow::set_cursor(int line, int column)
{
  if (line < 0 || column < 0)
    _xml->get_label("cursor_label")->set_label("");
  else
    _xml->get_label("cursor_label")->set_label(ufmt("%i:%i", line+1, column+1));
}


void MQMainWindow::bookmark_query(const Glib::ustring &catalog,
                                  const Glib::ustring &schema,
                                  const Glib::ustring &query)
{
  _bookmarks.interactive_add(catalog, schema, query);
}

void MQMainWindow::execute_query_raw(const Glib::ustring &query)
{
  _workarea->execute_query_raw(query);
}

void MQMainWindow::execute_script_file_new_window(const Glib::ustring &filename)
{
  //MQScriptEditorTab *add_script_view(const Glib::ustring &title);	
  MQScriptEditorTab *tab = _workarea->add_script_view(filename);
  if(tab) 
  {
    tab->load_file(filename);
    tab->execute_script();
  }
}

void MQMainWindow::bookmarks_changed()
{
  /*
  _bookmarks.update_bookmark_menu(_workarea->get_bookmark_menu(), 3,
                                  sigc::mem_fun(*_workarea,&MQWorkArea::open_bookmark));
   */
}

void MQMainWindow::show_preferences(bool conn_only)
{
  MGPreferencesEditor *editor= MGPreferencesEditor::instance();
//  _prefs_editor->signal_changed().connect(sigc::mem_fun(*this,&MAdministrator::prefs_changed));

  editor->set_transient_for(*this);
  
  editor->show(conn_only);
}


void MQMainWindow::new_connection()
{
  extern void start_new_instance();
  
  start_new_instance();
}



void MQMainWindow::open_url(const Glib::ustring &url)
{
//  system("gnome-www-browser http://www.mysql.com/network/&");
  system(Glib::ustring("firefox "+url+"&").c_str());
}
