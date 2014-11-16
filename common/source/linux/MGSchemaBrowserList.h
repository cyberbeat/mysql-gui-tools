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


#ifndef _MGSCHEMABROWSERLIST_H_
#define _MGSCHEMABROWSERLIST_H_

#include "MGBrowserList.h"
#include "myx_library.h"


class MGSchemaBrowserList : public MGBrowserList {
  public:
    enum ViewType {
  	Normal,
	SchemaOnly,
  	TableOnly,
        FullSchemata
    };

    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(_icon); add(_text); add(_catalog);
          add(_type); add(_color); add(_data); add(_populated);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<Glib::ustring> _catalog;
        Gtk::TreeModelColumn<char> _type;
        Gtk::TreeModelColumn<Gdk::Color> _color;
        Gtk::TreeModelColumn<MYX_SCHEMA*> _data;
        Gtk::TreeModelColumn<bool> _populated;
    };

    Columns _columns;

    typedef bool (*MarkRowDelegate)(gpointer, const Glib::ustring&,
                                    const Glib::ustring&,
                                    const Glib::ustring&);
    
    
    typedef sigc::slot2<bool,MGSchemaBrowserList*,const Gtk::TreeIter&> PopulateSlot;

  protected:
    void cursor_changed();

    virtual void refresh_list(const Glib::ustring &filter);

    ViewType _view_type;
    bool _show_columns;
    
    MYX_CATALOGS *_catalogs;

    Glib::RefPtr<Gdk::Pixbuf> _schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sys_schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _table_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sp_icon;
    Glib::RefPtr<Gdk::Pixbuf> _index_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;

    gpointer _mark_delegate_data;
    MarkRowDelegate _mark_delegate;

    PopulateSlot _populate_slot;

    bool get_last_child_of_node(const Gtk::TreeIter &node,
                                Gtk::TreeIter &child_ret);

    void refresh_column_markings(const Gtk::TreeIter &node,
                                 const Glib::ustring &schema,
                                 const Glib::ustring &table);
    void refresh_table_markings(const Gtk::TreeIter &node,
                                const Glib::ustring &schema);
                                
    void append_schema(MYX_SCHEMA *schema);
    
    bool contains_schema(MYX_SCHEMA *schema);
  public:
    void refresh_schema_list(const Gtk::TreeIter &schema_node,
                             MYX_SCHEMA_TABLES *tables,
                             MYX_SCHEMA_STORED_PROCEDURES *sps);

    void refresh_markings();

    MGSchemaBrowserList(const Glib::ustring &caption, ViewType type);
    void set_icons(const Glib::RefPtr<Gdk::Pixbuf> &schema_icon,
                   const Glib::RefPtr<Gdk::Pixbuf> &sys_schema_icon,
                   const Glib::RefPtr<Gdk::Pixbuf> &sp_icon,
                   const Glib::RefPtr<Gdk::Pixbuf> &table_icon,
                   const Glib::RefPtr<Gdk::Pixbuf> &column_icon);
    void set_index_icon(const Glib::RefPtr<Gdk::Pixbuf> &index_icon);

    void append_child(const Gtk::TreeIter &node,
                      const Glib::RefPtr<Gdk::Pixbuf> &icon,
                      const Glib::ustring &text);

    void set_catalogs(MYX_CATALOGS *catalogs);

    void set_populate_func(const PopulateSlot &slot);

    void set_mark_delegate(MarkRowDelegate deleg, gpointer data);

    void populate_node(const Gtk::TreeIter &iter);
    
    char get_row_object(const Gtk::TreeIter &iter,
                        Glib::ustring &catalog, Glib::ustring &schema, 
                        Glib::ustring &table, Glib::ustring &column,
                        Glib::ustring &child);

    bool get_selected_schema(Gtk::TreeIter &node,
                             Glib::ustring &catalog, Glib::ustring &schema,
                             Glib::ustring &child);

    void mark(const Gtk::TreeIter &node, bool flag=true);
};


#endif /* _MGSCHEMABROWSERLIST_H_ */
