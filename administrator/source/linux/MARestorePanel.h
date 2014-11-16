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


#ifndef _MARESTOREPANEL_H_
#define _MARESTOREPANEL_H_

#include "MAPanel.h"

#include "MGCellRenderer.h"

#include "myx_admin_library.h"

class MDataInterface;
class MGBrowserList;
class MGFileBrowserList;

class MARestorePanel : public MAPanel {
    class TargetColumns : public Gtk::TreeModel::ColumnRecord {
      public:
       TargetColumns() { add(name); add(created); };
       Gtk::TreeModelColumn<Glib::ustring> name;
       Gtk::TreeModelColumn<bool> created;
    } _target_columns;

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
          add(_type); add(_rows);
          add(_what);
        };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<MGCellRendererTristate::State> _state;
        Gtk::TreeModelColumn<Glib::ustring> _object;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<long> _rows;

        Gtk::TreeModelColumn<What> _what;
    } _sel_columns;

    MGFileBrowserList *_browser;

    Glib::RefPtr<Gdk::Pixbuf> _schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _sys_schema_icon;
    Glib::RefPtr<Gdk::Pixbuf> _table_icon;
    Glib::RefPtr<Gdk::Pixbuf> _view_icon;
    Glib::RefPtr<Gdk::Pixbuf> _proc_icon;
    Glib::RefPtr<Gdk::Pixbuf> _column_icon;

    Gtk::TreeView *_selection_tree;
    Glib::RefPtr<Gtk::TreeStore> _selection_store;
    
    int _old_schema_index;
    Glib::RefPtr<Gtk::ListStore> _schema_store;

    MGGladeXML *_progress_xml;
    
    Gtk::ProgressBar *_progress;
    Glib::RefPtr<Gtk::TextBuffer> _warning_buffer;
    
    
    MYX_BACKUP_CONTENT *_cur_content;
    Glib::ustring _cur_content_charset;

    void setup_selection_list();

    void schemata_reloaded(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &schemata);
    void target_changed();
   
    void file_selected(MGBrowserList *sender, const Gtk::TreeIter &node);

    void show_content_info(const std::string &file, const std::string &charset, MYX_BACKUP_CONTENT *content);

    bool detect_charset(Glib::ustring &chset);

    bool update_status(void *data);

    static void *load_thread(void *data);
    static void *restore_thread(void *data);

    void row_toggled(const Glib::ustring &path);
    
    void abort_operation();
    void close_progress();
    
    static int progress_cb(bigint bytes_read, bigint bytes_total, void *data);

    static void restore_warning_cb(const char *msg, void *data);
    
    MYX_BACKUP_CONTENT *gather_selected_tables();
    
    MYX_BACKUP_CONTENT *perform_load(const std::string &file,
                                     const std::string &charset,
                                     MYX_BACKUP_ERROR &error);
    void perform_restore();
    
    bool check_tables_to_be_dropped(MYX_BACKUP_CONTENT *content);

  public:
    MARestorePanel(MAdministrator *app, MDataInterface *data);
    ~MARestorePanel();

    virtual bool before_show();
    virtual bool before_hide();
    virtual bool before_quit();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };
};

extern MAPanel *create_restore_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MARESTOREPANEL_H_ */
