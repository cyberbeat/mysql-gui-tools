/* Copyright (C) 2003 MySQL AB

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


#ifndef _MACATALOGSPANEL_H_
#define _MACATALOGSPANEL_H_

#include "MAPanel.h"
#include <MGTableBrowserList.h>

#include <myx_admin_public_interface.h>

class MDataInterface;

class MGTableEditor;

class MACatalogsPanel : public MAPanel {
    enum MaintenanceCommand {
      Select,
      Check,
      Optimize,
      Repair
    };
    class TableColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        TableColumns() {
          add(_icon); add(_name); add(_type);
          add(_format); add(_rows); add(_length); add(_ilength); 
          add(_time); add(_status); add(_table);
          add(_data_percent); add(_index_percent);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _format;
        Gtk::TreeModelColumn<Glib::ustring> _rows;
        
        Gtk::TreeModelColumn<Glib::ustring> _length;
        Gtk::TreeModelColumn<int> _data_percent;
        Gtk::TreeModelColumn<Glib::ustring> _ilength;
        Gtk::TreeModelColumn<int> _index_percent;
        Gtk::TreeModelColumn<Glib::ustring> _time;
        Gtk::TreeModelColumn<MYX_TABLE_STATUS*> _status;
        Gtk::TreeModelColumn<MYX_SCHEMA_TABLE*> _table;
    } _table_columns;

    class ColumnColumns : public Gtk::TreeModel::ColumnRecord {
      public://XXX
        ColumnColumns() {
          add(_icon); add(_name); add(_key); add(_type);
          add(_null); add(_default); add(_extra);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _key;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _default;
        Gtk::TreeModelColumn<Glib::ustring> _null;
        Gtk::TreeModelColumn<Glib::ustring> _extra;
    } _column_columns;

    class IndexColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        IndexColumns() {
          add(_icon); add(_name); add(_key); add(_type); 
          add(_unique); add(_null); add(_seq); add(_coll); add(_bold);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _key;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _unique;
        Gtk::TreeModelColumn<Glib::ustring> _null;
        Gtk::TreeModelColumn<Glib::ustring> _seq;
        Gtk::TreeModelColumn<Glib::ustring> _coll;
        Gtk::TreeModelColumn<int> _bold;
    } _index_columns;

    class ViewColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ViewColumns() {
          add(_icon); add(_name); add(_comment); add(_updatable);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _comment;
        Gtk::TreeModelColumn<Glib::ustring> _updatable;
    } _view_columns;

    class UserColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        UserColumns() {
          add(_icon); add(_name); add(_fullname); add(_privileges);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _fullname;
        Gtk::TreeModelColumn<Glib::ustring> _privileges;
    } _user_columns;

    class DocColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        DocColumns() {
          add(_icon); add(_name); add(_type); add(_version); add(_user);
          add(_checked_out); add(_description);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _version;
        Gtk::TreeModelColumn<Glib::ustring> _user;
        Gtk::TreeModelColumn<Glib::ustring> _checked_out;
        Gtk::TreeModelColumn<Glib::ustring> _description;
    } _doc_columns;

    class EventColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        EventColumns() {
          add(_icon); add(_name); add(_type); add(_ctime); add(_utime);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _ctime;
        Gtk::TreeModelColumn<Glib::ustring> _utime;
    } _event_columns;

    class ProcedureColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ProcedureColumns() {
          add(_icon); add(_name); add(_definer); add(_created); 
          add(_modified); add(_return_datatype); add(_comment);
        };
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _definer;
        Gtk::TreeModelColumn<Glib::ustring> _created;
        Gtk::TreeModelColumn<Glib::ustring> _modified;
        Gtk::TreeModelColumn<Glib::ustring> _return_datatype;
        Gtk::TreeModelColumn<Glib::ustring> _comment;
    } _procedure_columns;
    
    class MaintTableColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        MaintTableColumns() { add(_table); };
        Gtk::TreeModelColumn<Glib::ustring> _table;
    } _maint_table_columns;


    MGGladeXML *_maint_dlg_xml;

    MGTableBrowserList *_schema_browser;
    Gtk::Menu _schemata_menu;

    MYX_SCHEMA_TABLES *_tables;
    //MYX_SCHEMA_TABLE_STATUS *_table_status;
    MYX_SCHEMA_ENTITY_STATUS *_entity_status;
    
    std::vector<MGTableEditor*> _open_editors;
    
    bool _details_shown;
    
    Gtk::Menu _table_menu;
    
    Glib::RefPtr<Gdk::Pixbuf> _schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sys_schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _table_icon;
    Glib::RefPtr<Gdk::Pixbuf> _proc_icon;
    Glib::RefPtr<Gdk::Pixbuf> _view_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;
    Glib::RefPtr<Gdk::Pixbuf> _key_column_icon;

    Glib::RefPtr<Gdk::Pixbuf> _index_icon;
    Glib::RefPtr<Gdk::Pixbuf> _doc_icon;


    Glib::RefPtr<Gtk::ListStore> _table_store;
    Glib::RefPtr<Gtk::TreeStore> _column_store;
    Glib::RefPtr<Gtk::TreeStore> _tindex_store;
    Glib::RefPtr<Gtk::ListStore> _view_store;
    Glib::RefPtr<Gtk::ListStore> _event_store;
    Glib::RefPtr<Gtk::TreeStore> _index_store;
    Glib::RefPtr<Gtk::ListStore> _user_store;
    Glib::RefPtr<Gtk::TreeStore> _doc_store;
    Glib::RefPtr<Gtk::ListStore> _procedure_store;
    
    Glib::RefPtr<Gtk::ListStore> _maint_table_store;

    int _sum_total_tables;
    int _sum_rows;
    long long _sum_data;
    long long _sum_index;
    
    void setup_trees();
    
    void display_schema(const Glib::ustring &catalog, const Glib::ustring &schema, bool refresh);

    void refresh_schemata();
    void schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata);

    void create_schema();
    void drop_schema();

    void refresh_table_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_event_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_view_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_index_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_procedure_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_doc_info(const Glib::ustring &catalog, const Glib::ustring &schema);
    void refresh_user_info(const Glib::ustring &catalog, const Glib::ustring &schema);

    void update_button_sensitivity();
    void selected_table();

    void maint_next_page();
    bool maint_pulse_progressbar();
    void maint_cancel();

    std::list<Glib::ustring> get_selected_tables();

    void table_dbl_clicked(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *column);
    void table_button_press(GdkEventButton *ev);
    
    void toggle_details();
    void create_table();
    void show_create_table();
    void edit_table();
    void edit_table_data();
    void drop_table();
    
    
    void close_create_table_window(GdkEventAny *ev);
    void save_create_table(const char *script);

    void table_editor_closed(MGTableEditor *editor);
    void table_edit_mode(bool flag);
    
    void table_maintenance(MaintenanceCommand command= Select);
    
    void reload_table_list();
    
    void schema_selected(MGBrowserList *sender,
                         const Gtk::TreeIter &node);

    void create_procedure();
    void create_function();
    void create_procedure_impl(MYX_SCHEMA_STORED_PROCEDURE_TYPE t);
    void edit_procedure();
    void drop_procedure();

    void create_view();
    void edit_view();
    void drop_view();

    char *run_script_editor(const char *title, const char *sql);
    char *input_string_dialog(const char *title, const char *prompt);

  void update_table_menu_sensitivity();

  public:
    MACatalogsPanel(MAdministrator *app, MDataInterface *info);
    ~MACatalogsPanel();

    virtual void show();
    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_catalogs_panel(MAdministrator *app, MDataInterface *info);

#endif /* _MACATALOGSPANEL_H_ */
