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


#ifndef _MADMINISTRATOR_H_
#define _MADMINISTRATOR_H_


#include <gtkmm.h>
#include <map>
#include <string>
#include <mysql.h>

#include <MGGladeXML.h>

//#include "MTranslation.h"
#include "MInstanceInfo.h"

class MDataInterface;

class MGPreferencesEditor;
class MAPanel;


class MAdministrator : public Glib::ObjectBase {
 private:
    class SidebarColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        SidebarColumns() { 
          add(_icon); add(_text); add(_color); add(_panel);
        };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<MAPanel*> _panel;
        Gtk::TreeModelColumn<Gdk::Color> _color;
    };
    SidebarColumns _sidebar_columns;

    MGGladeXML *_xml;

    Gtk::Window *_win;
    Gtk::TreeView *_tree;

    Glib::RefPtr<Gtk::TreeStore> _side_store;
    
    MAPanel *_dummy_panel;
    
    MAPanel *_current_panel;
    MGPreferencesEditor *_prefs_editor;

    Gtk::ProgressBar *_progressbar;

    sigc::signal0<void> _signal_prefs_changed;
    
    sigc::connection _pulse_conn;
    bool pulse_progress();
    
    void setup_sidebar();
    void populate_sidebar();
    void update_sidebar(bool disconnecting=false);

    void switch_panel_num(int index);
    void switch_panel(MAPanel *panel);

 private:
    void tree_selection_changed_cb();
    void cancel_button_clicked_cb();

    bool close_cb(GdkEventAny *ev);
    
    void new_connection_cb();
    void close_connection_cb();
    void reconnect_cb();
    
    void update_edit_menu();
    void cut_cb();
    void copy_cb();
    void paste_cb();

    void menu_quit_cb();
    void menu_preferences_cb(bool conn_only);
    void menu_about_cb();

    void start_console();

    void disconnect_db_handler();
    void connect_db_handler();
    
    void prefs_changed();

    void open_help();

 private:
    guint _tmp_status;

    MInstanceInfo *_instance;
    MDataInterface *_dataif;
    //MTranslation _transl;
    
    bool _changing_panel;
    
    std::list<sigc::slot0<void> > _stop_button_handlers;
 public:
    MAdministrator();
    ~MAdministrator();

    MInstanceInfo *get_instance() const;
    //MTranslation *get_translation() { return &_transl; };

    sigc::signal0<void> signal_prefs_changed() { return _signal_prefs_changed; };
    
    bool init(MInstanceInfo *inst);

    void show();
    void quit();
    void switch_panel(const Glib::ustring &name);

    Gtk::Container *parent_frame();
    Gtk::Window *window();

    void add_side_panel(Gtk::Widget *panel);
    void remove_side_panel(Gtk::Widget *panel);
    Gtk::Widget *get_widget(const Glib::ustring &name);
    
    Gtk::MenuItem *add_section_menu(Gtk::Menu *menu, const Glib::ustring &label);
    
    void set_status(const Glib::ustring &text);
    void push_status(const Glib::ustring &text);
    void pop_status();

    void set_busy_progress(bool start);

    void set_busy(const Glib::ustring &message=Glib::ustring());
    void push_stop_button_handler(const sigc::slot0<void> &slot);
    void pop_stop_button_handler();
   
    void open_merlin_page();
    void open_url(const Glib::ustring &url);
};

#endif /* _MADMINISTRATOR_H_ */
