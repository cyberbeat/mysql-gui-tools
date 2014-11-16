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

#ifndef _MGTABLEEDITOR_H_
#define _MGTABLEEDITOR_H_

#include <gtkmm/window.h>
#include <gtkmm/liststore.h>
#include <gtkmm/menu.h>
#include <gtkmm/comboboxentry.h>

#include "myx_public_interface.h"
#include "MGPtrWrap.h"

namespace Gtk {
  class Combo;
  class TreeView;
};

class MGGladeXML;

class MGTableEditor : public Glib::ObjectBase {
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(name); add(object);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<void*> object;
    } _columns;

    // index list
    class ILColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ILColumns() {
          add(name); add(kind); add(type); add(columns); add(object);
        };
        struct IndexColumn {
          Glib::ustring name;
          std::string length;
          std::string collation; // A, D
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<MYX_DBM_INDEX_KIND> kind;
        Gtk::TreeModelColumn<MYX_DBM_INDEX_TYPE> type;
        Gtk::TreeModelColumn<std::list<IndexColumn> > columns;
        Gtk::TreeModelColumn<void*> object;
    } _il_columns;

    // index column list
    class ICLColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ICLColumns() {
          add(length); add(name); add(collation);
        };
        Gtk::TreeModelColumn<std::string> length;
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<std::string> collation; //A, D
    } _icl_columns;

    // foreign key list
    class FKColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FKColumns() {
          add(name); add(table); add(on_delete); add(on_update); add(columns);
          add(object);
        };
        struct FKMapping {
          Glib::ustring source;
          Glib::ustring target;
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> table;
        Gtk::TreeModelColumn<MYX_DBM_FK_ACTION> on_delete;
        Gtk::TreeModelColumn<MYX_DBM_FK_ACTION> on_update;
        Gtk::TreeModelColumn<std::list<FKMapping> > columns;
        Gtk::TreeModelColumn<void*> object;
    } _fk_columns;

    // foreign key mapping list
    class FKMColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FKMColumns() {
          add(source); add(target);
        };
        Gtk::TreeModelColumn<Glib::ustring> source;
        Gtk::TreeModelColumn<Glib::ustring> target;
    } _fkm_columns;

    class FlagColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FlagColumns() { add(value); add(name); };
        Gtk::TreeModelColumn<bool> value;
        Gtk::TreeModelColumn<Glib::ustring> name;
    } _flag_columns;

    class TColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        TColumns() {
          add(icon); add(name); add(type); add(pk); add(nnull); add(autoinc);
          add(flags); add(defval); add(defnull); add(comment); add(comment_full); 
          add(charset); add(collation); add(object); 
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> type;
        Gtk::TreeModelColumn<bool> pk;
        Gtk::TreeModelColumn<bool> nnull;
        Gtk::TreeModelColumn<bool> autoinc;
        Gtk::TreeModelColumn<Glib::ustring> flags;
        Gtk::TreeModelColumn<Glib::ustring> defval;
        Gtk::TreeModelColumn<bool> defnull;
        Gtk::TreeModelColumn<Glib::ustring> comment;
        Gtk::TreeModelColumn<Glib::ustring> comment_full;
        Gtk::TreeModelColumn<Glib::ustring> charset;
        Gtk::TreeModelColumn<Glib::ustring> collation;
        Gtk::TreeModelColumn<MYX_DBM_COLUMN_DATA*> object;
    } _tcolumns;

    MGGladeXML *_xml;
    MGGladeXML *_confirm_xml;
    bool _windowed;
    
    bool _creating_table;

    MYX_DBM_DATATYPES *_data_types;
    MYX_DBM_CHARSETS *_charsets;
    Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > _catalog_data;
  
    MYX_ENGINES *_engines;
    
    MYSQL *_mysql;
    Glib::ustring _catalog;
    Glib::ustring _schema;
    Glib::ustring _table;
    MYX_DBM_TABLE_DATA *_data;

    Gtk::TreeView *_column_tree;
    
    bool _anti_recursion;
    bool _showing_column_info;

    Gtk::Menu _popup;

    MYX_LIB_ERROR _script_execute_status;

    Glib::RefPtr<Gtk::ListStore> _column_store;
    
    Glib::RefPtr<Gtk::ListStore> _flag_store;
    
    Glib::RefPtr<Gtk::ListStore> _fk_store;
    Glib::RefPtr<Gtk::ListStore> _fk_column_store;

    Glib::RefPtr<Gtk::ListStore> _index_store;
    Glib::RefPtr<Gtk::ListStore> _index_column_store;    
        
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;
    Glib::RefPtr<Gdk::Pixbuf> _pk_icon;

    Glib::RefPtr<Gdk::Pixbuf> _blob_icon;
    Glib::RefPtr<Gdk::Pixbuf> _date_icon;
    Glib::RefPtr<Gdk::Pixbuf> _numeric_icon;
    Glib::RefPtr<Gdk::Pixbuf> _spatial_icon;
    Glib::RefPtr<Gdk::Pixbuf> _string_icon;
    Glib::RefPtr<Gdk::Pixbuf> _userdef_icon;

    Glib::ustring description_for_charset(const Glib::ustring &s);
    Glib::ustring charset_for_description(const Glib::ustring &s);

    void set_combo_item(Gtk::ComboBox *combo, const Glib::ustring &item);
    Glib::ustring get_combo_item(Gtk::ComboBox *combo, const Glib::ustring &defval);
    
    bool check_datatype(const Glib::ustring &s);
    
    void fill_collation_combo(Gtk::ComboBox *combo, const Glib::ustring &charset);
        
    MYX_ENGINE *get_selected_engine();

    void fill_aux_data();
    void update_for_selected_engine();

    void show_table_info(MYX_DBM_TABLE_DATA *data);
    void show_table_options(MYX_DBM_TABLE_DATA *data);

    void update_pk_index();
    
    void show_dbm(MYX_DBM_TABLE_DATA *data);

    bool validate();
   
    static int execute_sql_statement(const char *sql, void *userdata);
    MYX_LIB_ERROR execute_sql_script(const Glib::ustring &script);
    
    bool _confirm_cancelled;
    void confirm_save_script();
    void confirm_cancel();
    void confirm_execute();
    void close_confirm_dialog(GdkEventAny *ev);
    
    void revert_changes();
    void commit_changes();
    bool confirm_apply(Glib::ustring &query);
    
    void charset_changed();

    // column stuff
    void column_edited(const Glib::ustring &path, const Glib::ustring &new_text, int column);
    void column_toggled(const Glib::ustring &path, int column);
    
    void column_option_changed(const char *option);
    void column_option_toggled(const Glib::ustring &path);

    void column_selected();

    void show_column_list(MYX_DBM_TABLE_DATA *data);
    void show_column_info(MYX_DBM_TABLE_DATA *data, const Gtk::TreeIter &iter);

    Gtk::TreeIter column_add(const Gtk::TreeIter &before=Gtk::TreeIter());
    
    int _column_tree_last_key;
    bool column_tree_key_released(GdkEventKey *event);

    void column_tree_clicked(GdkEventButton *event);
    Glib::RefPtr<Gdk::Pixbuf> get_icon_for_column(MYX_DBM_DATATYPE *type,
                                                  bool pk);
    
    void column_append();
    void column_insert();
    void column_delete();
    
    // index stuff
    void column_drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, Gtk::SelectionData &selection_data, guint info, guint time);
    void index_drop_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData &selection_data, guint info, guint time);
    void index_remove_column();

    void index_column_edited(const Glib::ustring &path, const Glib::ustring &new_text);

    void index_option_changed(const char *option);

    void index_selected();
    void add_index();
    void remove_index();
    void save_index_changes();

    void show_index_list(MYX_DBM_TABLE_DATA *data);
    void show_index_info(MYX_DBM_TABLE_DATA *tdata, const Gtk::TreeIter &iter);

    // fk stuff
    void fk_drop_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData &selection_data, guint info, guint time);
    void fk_option_changed(const char *option);
    void fk_column_edited(const Glib::ustring &path, const Glib::ustring &new_text);
    void fk_add_column();
    void fk_remove_column();

    void fk_selected();
    void add_fk();
    void remove_fk();
    void save_fk_changes();

    void show_fk_list(MYX_DBM_TABLE_DATA *data);
    void show_fk_info(MYX_DBM_TABLE_DATA *tdata, const Gtk::TreeIter &iter);

    bool delete_window_event(GdkEventAny *event);

  public:
    MGTableEditor(bool windowed= true);
    ~MGTableEditor();

    void show_table(MYSQL *mysql, const Glib::ustring &catalog, const Glib::ustring &schema, const Glib::ustring &table,
                    const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalog_data);
    
    void new_table(MYSQL *mysql, const Glib::ustring &catalog, const Glib::ustring &schema,
                   const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalog_data);
    
    Glib::SignalProxy0<void> signal_close();
    Gtk::Widget *get_widget();

    Glib::ustring get_catalog() const { return _catalog; };
    Glib::ustring get_schema() const { return _schema; };
    Glib::ustring get_table() const { return _table; };
};

#endif /* _MGTABLEEDITOR_H_ */
