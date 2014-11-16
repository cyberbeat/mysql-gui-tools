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

#ifndef _MGRTVALUETREE_H_
#define _MGRTVALUETREE_H_

#include <gtkmm/treeview.h>
#include <gtkmm/treestore.h>

#include <MySQLGRT/MGRT.h>

class MGRTValueTree : public Gtk::TreeView {
    class TreeColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        TreeColumns() {
          add(icon); add(text); add(detail); add(data);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<int> detail;
        Gtk::TreeModelColumn<MGRTValue> data;
    } _columns;

    Glib::RefPtr<Gtk::TreeStore> _store;

    MGRT *_grt;
    
    Glib::ustring _root_path;

    Glib::RefPtr<Gdk::Pixbuf> _simple_icon;
    Glib::RefPtr<Gdk::Pixbuf> _dict_icon;
    Glib::RefPtr<Gdk::Pixbuf> _list_icon;
    
    std::map<std::string, Glib::RefPtr<Gdk::Pixbuf> > _icon_info;
    sigc::slot<bool,MGRTValue>  _display_filter;
    
    void set_icon(Gtk::TreeRow row, MGRTValue value);
    void add_value_item(Gtk::TreeIter iter, MGRTValue value);
    
    void add_list_to_store(MGRTValue list, Gtk::TreeRow *parent, std::list<MYX_GRT_VALUE*> *list);
    void add_dict_to_store(MGRTValue dict, Gtk::TreeRow &parent, std::list<MYX_GRT_VALUE*> *list);

    void remember_path(Gtk::TreeView *tree, const Gtk::TreeModel::Path &path, std::list<MYX_GRT_VALUE*> &list);
  public:
    MGRTValueTree(MGRT *grt, const Glib::ustring &root_path, const Glib::ustring &title);
    MGRTValueTree(GtkTreeView *tree);

    void set_icon_info(const std::map<std::string, Glib::RefPtr<Gdk::Pixbuf> > &icon_info);

    void set_display_filter(sigc::slot<bool,MGRTValue> filter);
    
    void set_grt(MGRT *grt);
    void set_root_path(const Glib::ustring &root_path);
    void set_title(const Glib::ustring &title);
    
    void refresh();
    
    MGRTValue value_at_row(const Gtk::TreePath &path);
};

#endif /* _MGRTSHELL_H_ */
