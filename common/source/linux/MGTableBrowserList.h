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

#ifndef _MGTABLEBROWSERLIST_H_
#define _MGTABLEBROWSERLIST_H_


#include "MGBrowserList.h"
#include "myx_library.h"
#include "MGPtrWrap.h"


Glib::ustring MGSchemaObjectNameFromTable(const Glib::ustring &catalog,
                                        const Glib::ustring &schema,
                                        const Glib::ustring &table);


bool MGTableFromSchemaObjectName(const Glib::ustring &objectName,
                                 Glib::ustring &catalog,
                                 Glib::ustring &schema,
                                 Glib::ustring &table);


class MGTableBrowserList : public MGBrowserList {
  public:
    enum RowType {
      Schema,
      Table,
      Column,
      SP,
      Parameter
    };
 
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(icon); add(text); add(type); add(data); add(weight);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<RowType> type;
        Gtk::TreeModelColumn<void*> data;
        Gtk::TreeModelColumn<int> weight;
    } _columns;

    typedef sigc::signal0<void> RowActivateSignal;
    typedef sigc::slot3<bool,const Glib::ustring&,const Glib::ustring&,MYX_SCHEMA_TABLES *&> FetchSchemaTablesSlot;
    typedef sigc::slot3<bool,const Glib::ustring&,const Glib::ustring&,MYX_SCHEMA_STORED_PROCEDURES *&> FetchSchemaSPsSlot;

  protected:
    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > _catalogs;
    MYX_SCHEMA_TABLES *_tables;

    Glib::RefPtr<Gdk::Pixbuf> _schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _table_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sp_icon;
    Glib::RefPtr<Gdk::Pixbuf> _view_icon;
    Glib::RefPtr<Gdk::Pixbuf> _key_icon;

    RowType _leaf_type;

    bool _show_tables;
    bool _show_sps;

    RowActivateSignal _signal_row_activate;
    FetchSchemaTablesSlot _fetch_schema_tables;
    FetchSchemaSPsSlot _fetch_schema_sps;

    // the following data and set_selection_info() function
    // helps keep schema tree state between schema reloads
    Glib::ustring _selected_schema_name;
    Glib::ustring _selected_schema_entity_name;
    RowType _selected_row_type;
    bool _schema_is_expanded;

    void set_selection_info(const char *schema_name,
      const char *entity_name, RowType rowtype, bool schema_exp);

    int get_search_type();

    void row_activated(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *col);
    void row_selected();

    void update_menu();

    void drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, Gtk::SelectionData& selection_data, guint info, guint time);

    virtual void refresh_list(const Glib::ustring &filter);

    bool refresh_table_list(const Gtk::TreeIter &piter,
                            MYX_SCHEMA_TABLES *tables,
                            const Glib::ustring &filter);
    bool refresh_sp_list(const Gtk::TreeIter &piter,
                         MYX_SCHEMA_STORED_PROCEDURES *sps,
                         const Glib::ustring &filter);

    void on_row_expanded(const Gtk::TreeModel::iterator&, const Gtk::TreeModel::Path&);
    void on_row_collapsed(const Gtk::TreeModel::iterator&, const Gtk::TreeModel::Path&);

  public:
    MGTableBrowserList(const Glib::ustring &caption,
                       RowType leaf_type=Table);
    
    void set_show_sps(bool flag);

    void set_catalogs(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalogs);
    
    void set_fetch_schema_tables_func(const FetchSchemaTablesSlot &slot);
    void set_fetch_schema_sps_func(const FetchSchemaSPsSlot &slot);

    RowActivateSignal signal_row_activate() const { return _signal_row_activate; };

    Gtk::TreeIter find_table(const Glib::ustring &catalog,
                             const Glib::ustring &schema,
                             const Glib::ustring &table="");

    void set_node_bold(const Gtk::TreeIter &iter, bool flag);

//    std::list<Gtk::TreeIter> get_selected_nodes();

    RowType get_type(const Gtk::TreeIter &iter);
    Glib::ustring get_catalog(const Gtk::TreeIter &iter);
    Glib::ustring get_schema(const Gtk::TreeIter &iter);
    Glib::ustring get_table(const Gtk::TreeIter &iter);
    Glib::ustring get_table_column(const Gtk::TreeIter &iter);
    Glib::ustring get_procedure(const Gtk::TreeIter &iter, bool &is_function);
    MYX_SCHEMA *get_schema_object(const Gtk::TreeIter &iter);

    bool is_view(const Gtk::TreeIter &iter);

    MYX_SCHEMA_TABLE *get_table(const Gtk::TreeIter &iter,
                                Glib::ustring &name);
    Glib::ustring get_column(const Gtk::TreeIter &iter);

    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > get_catalogs() { return _catalogs; };
};


#endif /* _MGTABLEBROWSERLIST_H_ */
