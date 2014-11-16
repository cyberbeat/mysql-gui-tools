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


#ifndef _MAUSERADMINISTRATIONPANEL_H_
#define _MAUSERADMINISTRATIONPANEL_H_

#include "MAPanel.h"
#include "MGSchemaBrowserList.h"

//#include "wmyx_public_interface.h"
#include "myx_admin_library.h"

#include <map>

class MGBrowserList;
class MGUserBrowserList;
class MAUserAdministrationPanel;

// helper class to select schemata names for given user
// (see bug #12323)

class MGUserSchemaBrowserList : public MGSchemaBrowserList
{
private:
  MAUserAdministrationPanel& _panel;

protected:    
  virtual void refresh_list(const Glib::ustring &filter);

public:
  MGUserSchemaBrowserList(MAUserAdministrationPanel& panel, 
    const Glib::ustring &caption, ViewType type)
  : MGSchemaBrowserList(caption, type), _panel(panel)
  {}
};

class MAUserAdministrationPanel : public MAPanel {
  private:
    class AssignedColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        AssignedColumns() { add(_icon); add(_text); add(_name); add(_color); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _color;
    };
    AssignedColumns _assigned_columns;
    
    class AvailableColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        AvailableColumns() { add(_icon); add(_text); add(_name); add(_help); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _help;
    };
    AvailableColumns _available_columns;

    MInstanceInfo *_instance;
    
    MGGladeXML *_dialog_xml;
    MGGladeXML *_host_dialog_xml;

    Glib::RefPtr<Gtk::SizeGroup> _size_group;
    
    Gtk::Menu *_menu;
    Gtk::MenuItem *_menu_item;
    
    Gtk::MenuItem *_user_menu_items[16];

    Gtk::Menu *_schemas_popup_menu;
    
    Glib::RefPtr<Gdk::Pixbuf> _default_user_icon;
    Glib::RefPtr<Gdk::Pixbuf> _default_user_mini_icon;
    Glib::RefPtr<Gdk::Pixbuf> _right_icon;

    MYX_USER_NAMES *_user_names;

    MYX_USER *_current_user;
//    Glib::ustring _current_user_name;
    Glib::ustring _current_host;
    Glib::RefPtr<Gdk::Pixbuf> _current_user_icon;
    unsigned char *_current_icon_data;
    bool _user_is_new;
    
    int _current_icon_data_length;
    std::map<Glib::ustring,MYX_USER_OBJECT_PRIVILEGES*> _object_privilege;

    MYX_USER_OBJECT_PRIVILEGES *_global_privileges_template;
    MYX_USER_OBJECT_PRIVILEGES *_schema_privileges_template;
    MYX_USER_OBJECT_PRIVILEGES *_table_privileges_template;
    MYX_USER_OBJECT_PRIVILEGES *_column_privileges_template;
    MYX_USER_OBJECT_PRIVILEGES *_proc_privileges_template;
    
    MGUserBrowserList *_user_browser;
    MGUserSchemaBrowserList *_schema_browser;
    MGSchemaBrowserList *_table_browser;
    
    Glib::RefPtr<Gtk::ListStore> _global_assigned_privs;
    Glib::RefPtr<Gtk::ListStore> _global_available_privs;

    Glib::RefPtr<Gtk::ListStore> _schema_assigned_privs;
    Glib::RefPtr<Gtk::ListStore> _schema_available_privs;

    Glib::RefPtr<Gtk::ListStore> _table_assigned_privs;
    Glib::RefPtr<Gtk::ListStore> _table_available_privs;

    bool _first_show;
    bool _dirty;

    bool _apply_changes_on_user_switch;
    bool _discard_changes_on_user_switch;

    bool _ignore_entry_changes;
    bool _ignore_limit_changes;
    
    void setup_list_views();
    void connect_signals();

    bool fetch_privilege_templates();
    
    MYX_USER_OBJECT_PRIVILEGES *clone_privilege_struct(MYX_USER_OBJECT_PRIVILEGES*);
    MYX_USER_OBJECT_PRIVILEGES *fetch_privileges_for_object(MYX_USER *user,
                                                            const Glib::ustring &host,
                                                            const Glib::ustring &object,
                                                            bool cache_only=false, 
                                                            bool is_stored_proc=false);
    
    bool add_host_to_user(MYX_USER *user, const Glib::ustring &host);
    
    void fill_privilege_list(Glib::RefPtr<Gtk::ListStore> &lstore, 
                             Glib::RefPtr<Gtk::ListStore> &rstore,
                             char **privs, unsigned int nprivs,
                             char **global_privs, unsigned int global_nprivs,
                             const char *priv_prefix, const char *priv_suffix);
  
    void refresh_user_list();
  
    bool populate_user_host(MGUserBrowserList *sender,
                            const Gtk::TreeIter &node,
                            std::list<Glib::ustring> &hosts);

    bool populate_schema_tables(MGSchemaBrowserList *sender,
                                const Gtk::TreeIter &node);

    bool grant_revoke_privilege(MYX_USER_OBJECT_PRIVILEGES *privs,
                                Gtk::TreeView *llist,
                                Gtk::TreeView *rlist,
                                bool revoke);

    void update_sensitivity();

    void update_global_privilege_sensitivity();
    void update_schema_privilege_sensitivity();
    void update_table_privilege_sensitivity();

    MYX_USER_OBJECT_PRIVILEGES *get_current_global_privileges();
    MYX_USER_OBJECT_PRIVILEGES *get_current_schema_privileges(Gtk::TreeIter &node_ret);
    MYX_USER_OBJECT_PRIVILEGES *get_current_table_privileges(Gtk::TreeIter &node_ret);

    void remove_user_from_name_list(const Glib::ustring &name);

    void create_new_user();
    void clone_user();
    void remove_user();
    void show_user(const Glib::ustring &username, const Glib::ustring &host);
    void unshow_user();
    void show_global_privileges(MYX_USER *user, const Glib::ustring &host);
    void show_schema_privileges(MYX_USER *user, const Glib::ustring &catalog, 
                                const Glib::ustring &schema, 
                                const Glib::ustring &host);
    void show_table_privileges(MYX_USER *user,
                               const Glib::ustring &catalog, 
                               const Glib::ustring &schema,
                               const Glib::ustring &table,
                               const Glib::ustring &column,
                               const Glib::ustring &host);
    void show_proc_privileges(MYX_USER *user,
                               const Glib::ustring &catalog, 
                               const Glib::ustring &schema,
                               const Glib::ustring &table,
                               const Glib::ustring &host);
    void show_limits(MYX_USER *user, const Glib::ustring &host);

    void set_shown_user(MYX_USER *user, const Glib::ustring &host=Glib::ustring());
    void apply_shown_user();

    void add_host();
    void remove_host();
    
    void set_user_icon_file(const Glib::ustring &filename);
    
    void set_dirty();

    bool validate();
    
    bool confirm_user_change();

    bool apply_changes();
    void discard_changes();

    void discard_state_data();

    void prefs_changed();

  private:
    int assigned_list_comparer(const Gtk::TreeIter &a, const Gtk::TreeIter &b);
    int available_list_comparer(const Gtk::TreeIter &a, const Gtk::TreeIter &b);

    void set_privilege_values(const Gtk::TreeIter &iter,
                              MYX_USER_OBJECT_PRIVILEGES *privs,
                              bool revoke);

    void user_selected(MGBrowserList *sender,
                       const Gtk::TreeIter &node);

    void schema_selected(MGBrowserList *sender, const Gtk::TreeIter &node);
    void table_selected(MGBrowserList *sender, const Gtk::TreeIter &node);

    bool user_will_change_delegate(MGBrowserList *sender,
                                   const Gtk::TreeIter &node);

    static bool schema_mark_delegate(void *data, 
                                     const Glib::ustring &schema,
                                     const Glib::ustring &table,
                                     const Glib::ustring &column);
    static bool table_mark_delegate(void *data, 
                                    const Glib::ustring &schema,
                                    const Glib::ustring &table, 
                                    const Glib::ustring &column);


    void entry_changed();

    void global_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                        bool assigned);
    void global_revoke_grant_button_clicked(bool revoke);

    void schema_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                        bool assigned);
    void schema_revoke_grant_button_clicked(bool revoke);

    void table_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                       bool assigned);
    void table_revoke_grant_button_clicked(bool revoke);

    void icon_change_button_clicked();

    void limit_changed(const char *w);
    
    void apply_button_clicked();
    void discard_button_clicked();
    void new_user_button_clicked();
    
    void add_user_activate();
    void clone_user_activate();
    void remove_user_activate();
    void add_host_activate();
    void remove_host_activate();
    
    void disconnected_cb();
    
    void reload_catalogs();
  public:
    MAUserAdministrationPanel(MAdministrator *app, MDataInterface *data);
    virtual ~MAUserAdministrationPanel();

    virtual void show();
    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();
   
    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
    
    MYX_USER * get_current_user();
    Glib::ustring get_current_host();
};

extern MAPanel *create_user_administration_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MAUSERADMINISTRATIONPANEL_H_ */
