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


#ifndef _MABACKUPPANEL_H_
#define _MABACKUPPANEL_H_

#include "MAPanel.h"

class MGBrowserList;
class MGFileBrowserList;
class MGTableBrowserList;

#include "MGCellRenderer.h"
#include "MCrontab.h"
#include "MGPtrWrap.h"
#include "myx_admin_library.h"


class MDataInterface;


class MABackupPanel : public MAPanel {
    class SelectionColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        enum What {
          CSchema,
          CTable,
          CView,
          CRoutine
        };

        SelectionColumns() {
          add(_icon); add(_state); add(_object); 
          add(_type); add(_rows); add(_length); add(_lengthn); add(_time);
          add(_what);
        };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<MGCellRendererTristate::State> _state;
        Gtk::TreeModelColumn<Glib::ustring> _object;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<long> _rows;
        Gtk::TreeModelColumn<Glib::ustring> _length;
        Gtk::TreeModelColumn<unsigned long> _lengthn;
        Gtk::TreeModelColumn<Glib::ustring> _time;

        Gtk::TreeModelColumn<What> _what;
    } _sel_columns;

    Glib::RefPtr<Gdk::Pixbuf> _schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sys_schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _table_icon;
    Glib::RefPtr<Gdk::Pixbuf> _view_icon;
    Glib::RefPtr<Gdk::Pixbuf> _proc_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;

    MGTableBrowserList *_schema_browser;

    MGGladeXML *_progress_xml;
    Gtk::Label *_progress_label;
    Gtk::ProgressBar *_table_progress;
    Gtk::ProgressBar *_row_progress;

    MGFileBrowserList *_file_browser;

    Gtk::TreeView *_selection_tree;
    Glib::RefPtr<Gtk::TreeStore> _selection_store;


    Glib::ustring _profile_name; // original profile name
    
    bool _dirty;

    void schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata);
    
    void setup_selection_list();

    void add_schema_to_list(const Glib::ustring &catalog,
                            const Glib::ustring &schema,
                            bool checked);

    void update_sensitivity(bool flag);
    void update_button_sensitivity();
    void update_backup_all_state();
    void update_buttonbar_sensitivity();

    void add_object();
    void remove_object();
    void selected_object();
    
    void select_target_path();
    void backup_all_check_toggled();

    void copy_profile(const std::string &source_name,
                      const std::string &dest_name);

    void import_profile();
    void create_profile();
    void delete_profile();
    void clone_profile();
    void show_profile(MYX_BACKUP_PROFILE *profile);
    void save_profile();
    MYX_BACKUP_PROFILE *gather_profile();
    MYX_BACKUP_CONTENT *gather_selected_tables();

    bool update_backup_progress(void *data);
    static int backup_progress_cb(const char *table,
                                  int num_tables_total,
                                  int num_tables,
                                  int num_rows_total,
                                  int num_rows,
                                  void *user_data);
    static void *backup_thread(void *data);
    void perform_backup();
    
    void stop_backup();

    void schedule_type_changed();
    
    void name_changed();
    void set_dirty();
    void row_toggled(const Glib::ustring &path);
    
    bool gather_schedule(MCrontab::Entry &schedule);
    void show_schedule(const MCrontab::Entry &schedule);
    
    void show_schedule();
    void install_schedule();
    void uninstall_schedule(bool refresh_screen=true);
    
  private:
    void schema_selected(MGBrowserList *sender,
                         const Gtk::TreeIter &node);

    void profile_selected(MGBrowserList *sender,
                          const Gtk::TreeIter &node);
  public:
    MABackupPanel(MAdministrator *app, MDataInterface *data);
    ~MABackupPanel();

    static Glib::ustring get_backup_error_text(MYSQL *mysql, MYX_BACKUP_ERROR err);

    static bool execute_backup(MYSQL *mysql,
                               const std::string &profile,
                               const std::string &path,
                               const std::string &prefix);

    virtual void show();
    virtual bool before_show();
    virtual bool before_hide();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_backup_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MABACKUPPANEL_H_ */
