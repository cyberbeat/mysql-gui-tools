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

#ifndef _MGRTTABLEEDITOR_H_
#define _MGRTTABLEEDITOR_H_

#include <MySQLGRT/MGRTObjectEditor.h>
#include <MySQLGRT/MGRTTable.h>
#include <gtkmm/liststore.h>
#include <gtkmm/checkbutton.h>

class MGGladeXML;

class MGRTTableEditor : public MGRTObjectEditor {
    class ColumnColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ColumnColumns() {
          add(name); add(icon); add(pk); add(type); add(nnull); 
          add(autoinc); add(flags); add(defval);
          add(defnull); add(comment); add(grt);
          add(placeholder);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<bool> pk;
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> type;
        Gtk::TreeModelColumn<Glib::ustring> flags;
        Gtk::TreeModelColumn<bool> nnull;
        Gtk::TreeModelColumn<bool> autoinc;
        Gtk::TreeModelColumn<Glib::ustring> defval;
        Gtk::TreeModelColumn<bool> defnull;
        Gtk::TreeModelColumn<Glib::ustring> comment;
        Gtk::TreeModelColumn<MGRTValue> grt;
        Gtk::TreeModelColumn<bool> placeholder;
    } _c_columns;

    class IndexColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        IndexColumns() {
          add(name); add(type); add(comment); add(grt); add(placeholder);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> type;
        Gtk::TreeModelColumn<Glib::ustring> comment;
        Gtk::TreeModelColumn<MGRTValue> grt;
        Gtk::TreeModelColumn<bool> placeholder;
    } _i_columns;

    class IndexColumnColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        IndexColumnColumns() {
          add(column); add(order); add(length); add(function); add(comment); add(grt);
          add(placeholder);
        }
        Gtk::TreeModelColumn<Glib::ustring> column;
        Gtk::TreeModelColumn<Glib::ustring> order;
        Gtk::TreeModelColumn<int> length;
        Gtk::TreeModelColumn<Glib::ustring> function;
        Gtk::TreeModelColumn<Glib::ustring> comment;
        Gtk::TreeModelColumn<MGRTValue> grt;
        Gtk::TreeModelColumn<bool> placeholder;
    } _ic_columns;

    class FKColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FKColumns() {
          add(name); add(ondelete); add(onupdate); add(reftable); add(comment);
          add(grt); add(placeholder);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<Glib::ustring> ondelete;
        Gtk::TreeModelColumn<Glib::ustring> onupdate;
        Gtk::TreeModelColumn<Glib::ustring> reftable;
        Gtk::TreeModelColumn<Glib::ustring> comment;
        Gtk::TreeModelColumn<MGRTValue> grt;
        Gtk::TreeModelColumn<bool> placeholder;
    } _fk_columns;

    class FKColumnColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FKColumnColumns() {
          add(column); add(refcolumn); add(index); add(placeholder);
        };
        Gtk::TreeModelColumn<Glib::ustring> column;
        Gtk::TreeModelColumn<Glib::ustring> refcolumn;
        Gtk::TreeModelColumn<int> index;
        Gtk::TreeModelColumn<bool> placeholder;
    } _fkc_columns;
    
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() {
          add(name);
          add(grt);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<MGRTValue> grt;
    } _columns;
    
    MGGladeXML *_xml;

    MGRTTable *_table_data;
    
    bool _displaying;
    
    Gdk::Color _placeholder_color;

    Glib::RefPtr<Gtk::ListStore> _column_list;
    Glib::RefPtr<Gtk::ListStore> _index_list;
    Glib::RefPtr<Gtk::ListStore> _index_column_list;
    Glib::RefPtr<Gtk::ListStore> _fk_list;
    Glib::RefPtr<Gtk::ListStore> _fk_column_list;

    Glib::RefPtr<Gtk::ListStore> _collation;
    Glib::RefPtr<Gtk::ListStore> _column_collation;

    Glib::RefPtr<Gtk::ListStore> _engine_list;
    Glib::RefPtr<Gtk::ListStore> _index_type_list;
    Glib::RefPtr<Gtk::ListStore> _index_order_list;
    Glib::RefPtr<Gtk::ListStore> _aux_column_names_list;
    Glib::RefPtr<Gtk::ListStore> _fk_ondelete_list;
    Glib::RefPtr<Gtk::ListStore> _fk_onupdate_list;
    Glib::RefPtr<Gtk::ListStore> _fk_reftable_list;
    Glib::RefPtr<Gtk::ListStore> _fk_refcolumn_list;

    Glib::RefPtr<Gdk::Pixbuf> _pk_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;
    Glib::RefPtr<Gdk::Pixbuf> _blob_icon;
    Glib::RefPtr<Gdk::Pixbuf> _date_icon;
    Glib::RefPtr<Gdk::Pixbuf> _numeric_icon;
    Glib::RefPtr<Gdk::Pixbuf> _spatial_icon;
    Glib::RefPtr<Gdk::Pixbuf> _string_icon;
    Glib::RefPtr<Gdk::Pixbuf> _userdef_icon;
    Glib::RefPtr<Gdk::Pixbuf> _null_icon;

    void refresh_reftable_list();
    void refresh_refcolumn_list();

    bool commit_changes();
    
    void select_last_item(Gtk::TreeView *tree);
    
    void refresh_column_list();
    void column_selected();
    void column_value_edited(const Glib::ustring &,const Glib::ustring &,int);
    void column_value_toggled(const Glib::ustring &,int);
    void delete_column();
    void show_column(MGRTValue column);
    void column_flag_changed(Gtk::CheckButton *check, const char *flag);
    void column_entry_changed(Gtk::Widget *widget, const char *flag);
    void set_column_icon(Gtk::TreeIter iter);

    void index_value_edited(const Glib::ustring &,const Glib::ustring &,int);
    void index_column_value_edited(const Glib::ustring &,const Glib::ustring &,int);
    void delete_index(bool column);
    void refresh_index_list();
    void refresh_index_column_list();

    void fk_value_edited(const Glib::ustring &,const Glib::ustring &,int);
    void fk_column_value_edited(const Glib::ustring &,const Glib::ustring &,int);
    void delete_fk(bool column);
    void refresh_fk_list();
    void refresh_fk_column_list();

    bool tree_key_up(GdkEventKey *event, Gtk::TreeView *tree);
    
    void toggle_advanced();
    
    virtual MGRTValue edited_object();
    virtual bool commit();
    virtual void revert();
    virtual void show_object();

    virtual void setup();
    
  public:
    MGRTTableEditor(GtkWindow *window);
    static MGRTTableEditor *create(MGRT *grt, MGRTValue catalog);

    void edit_object(MGRTValue object);
    void create_new();
};

#endif /* _MGRTTABLEEDITOR_H_ */
