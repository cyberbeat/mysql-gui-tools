/* Copyright (C) 2005 MySQL AB

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

#ifndef _MGRTREVENGFILTERPANE_H_
#define _MGRTREVENGFILTERPANE_H_


#include <gtkmm/frame.h>
#include <gtkmm/checkbutton.h>
#include <gtkmm/box.h>
#include <gtkmm/table.h>
#include <gtkmm/treeview.h>
#include <gtkmm/liststore.h>
#include <gtkmm/scrolledwindow.h>
#include <gtkmm/label.h>
#include <gtkmm/image.h>
#include <MySQLGRT/MGRT.h>

class MGRTRevEngFilterPane : public Gtk::Frame {
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(text); add(grt); add(owner);
        };
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<MGRTValue> grt;
        Gtk::TreeModelColumn<MGRTValue> owner;
    } _columns;
    
    MGRT *_grt;

    Gtk::Table _table;
    
    Gtk::CheckButton _check;
    Gtk::Label _label1;
    Gtk::Label _label2;
    Gtk::Label _label3;
    
    Gtk::TreeView _source_tree;
    Gtk::TreeView _ignore_tree;
    Gtk::VBox _button_box;
    Gtk::Table _detail_table;
    Gtk::Button _detail_button;
    
    Glib::ustring _struct_name;
    
    Glib::RefPtr<Gtk::ListStore> _source_store;
    Glib::RefPtr<Gtk::ListStore> _ignore_store;

    void show_details();
    void move_object(const char *dir);
    void toggle_migrate();
  public:
    MGRTRevEngFilterPane(MGRT *grt, const Glib::ustring &struct_name);
    
    void refresh();
    void set_selected(bool flag);
};

#endif /* _MGRTREVENGFILTERPANE_H_ */
