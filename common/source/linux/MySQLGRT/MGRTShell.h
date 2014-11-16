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

#ifndef _MGRTSHELL_H_
#define _MGRTSHELL_H_

#include <gtkmm/paned.h>
#include <gtkmm/textview.h>
#include <gtkmm/notebook.h>
#include <gtkmm/combobox.h>
#include <gtkmm/entry.h>
#include <gtkmm/treeview.h>
#include <gtkmm/treestore.h>
#include <gtkmm/liststore.h>
#include <gtkmm/label.h>
#include <gtkmm/box.h>
#include <gtkmm/menubar.h>
#include <gtkmm/window.h>
#include <gtkmm/scrolledwindow.h>

#include <MySQLGRT/MGRT.h>
#include "MGShellView.h"

class MGRTShell : public MGShellInterface, public Gtk::Window {
    class TreeColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        TreeColumns() {
          add(icon); add(text); add(path); add(detail); add(data);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<Glib::ustring> path;
        Gtk::TreeModelColumn<int> detail;
        Gtk::TreeModelColumn<void*> data;
    } _columns;

    class DetailColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        DetailColumns() {
          add(text); add(value);
        };
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<Glib::ustring> value;
    } _dcolumns;

    Gtk::MenuBar _menu;
    Gtk::VBox _top_box;
    
    Gtk::HPaned _paned;
    Gtk::ScrolledWindow _text_scroll;
    MGShellView _text;
    Gtk::Notebook _sidenote;

    Gtk::VPaned _value_paned;
    Glib::RefPtr<Gtk::TreeStore> _value_store;
    Gtk::VBox _value_box;
    Gtk::ComboBox _value_combo;
    Gtk::ScrolledWindow _value_scroll;
    Gtk::TreeView _value_tree;
    Gtk::Entry _value_entry;
    Gtk::ScrolledWindow _detail_scroll;
    Gtk::TreeView _detail_tree;
    Glib::RefPtr<Gtk::ListStore> _detail_store;

    Gtk::ScrolledWindow _struct_scroll;
    Glib::RefPtr<Gtk::TreeStore> _struct_store;
    Gtk::TreeView _struct_tree;
    
    Gtk::ScrolledWindow _module_scroll;
    Glib::RefPtr<Gtk::TreeStore> _module_store;
    Gtk::VBox _module_box;
    Gtk::TreeView _module_tree;
    Gtk::Label _module_label;
    
    MGRT *_grt;
        
    Glib::ustring _root_path;

    Glib::RefPtr<Gdk::Pixbuf> _simple_icon;
    Glib::RefPtr<Gdk::Pixbuf> _dict_icon;
    Glib::RefPtr<Gdk::Pixbuf> _struct_icon;
    Glib::RefPtr<Gdk::Pixbuf> _list_icon;
    Glib::RefPtr<Gdk::Pixbuf> _folder_icon;
    Glib::RefPtr<Gdk::Pixbuf> _module_icon;
    Glib::RefPtr<Gdk::Pixbuf> _function_icon;

    Gtk::CheckMenuItem *_container_only_item;
    
    void setup_menu();
    
    void set_icon(Gtk::TreeRow row, MGRTValue value);
    Glib::ustring get_caption(MGRTValue value);
    void add_value_item(Gtk::TreeIter iter, MGRTValue value);
    
    void add_list_to_store(MGRTValue list, Gtk::TreeRow &parent, Glib::RefPtr<Gtk::TreeStore> store);
    void add_dict_to_store(MGRTValue dict, Gtk::TreeRow &parent, Glib::RefPtr<Gtk::TreeStore> store);
    

    void value_activated(const Gtk::TreeModel::Path &path,Gtk::TreeViewColumn *column);
    void value_selected();
    void module_selected();

    void toggle_view_option();
    
    void save_tree();

  public:
    MGRTShell(MGRT *grt);

    MGShellView *shell_view();

    Glib::ustring get_prompt();
    void perform_command(const Glib::ustring &command);
    
    void refresh();
};

#endif /* _MGRTSHELL_H_ */
