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


#ifndef _MDYNAMICINTERFACE_H_
#define _MDYNAMICINTERFACE_H_

#include <map>
#include <gtkmm.h>
#include <myx_admin_public_interface.h>

#include <MGGladeXML.h>
#include "MInstanceInfo.h"


class MDynamicInterface;
class MGImageCheckButton;

class MInnoDBFilePathEditor : public Gtk::Table {
    class Columns : public Gtk::TreeModel::ColumnRecord {
      public:
        Columns() { add(_file); add(_size); };

        Gtk::TreeModelColumn<Glib::ustring> _file;
        Gtk::TreeModelColumn<Glib::ustring> _size;
    } _columns;
    
    class FSColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        FSColumns() { add(_fs); add(_path); add(_type); add(_size);
          add(_space); add(_info);
        };
        Gtk::TreeModelColumn<Glib::ustring> _fs;
        Gtk::TreeModelColumn<Glib::ustring> _path;
        Gtk::TreeModelColumn<Glib::ustring> _type;
        Gtk::TreeModelColumn<Glib::ustring> _size;
        Gtk::TreeModelColumn<Glib::ustring> _space;
        Gtk::TreeModelColumn<MInstanceInfo::FilesystemInfo> _info;
    } _fs_columns;

    MDynamicInterface *_owner;
    MInstanceInfo *_instance;

    Gtk::ScrolledWindow _swin;
    Gtk::TreeView _tree;
    Gtk::Button _add_button;
    Gtk::Button _remove_button;
    Gtk::CheckButton _extend_check;
    Glib::RefPtr<Gtk::ListStore> _list;

    Glib::RefPtr<Gdk::GC> _gc;
    Gdk::Color _free_color;
    Gdk::Color _used_color;
    Gdk::Color _data_color;
    Gdk::Color _data_new_color;
    
    bool expose_graph(GdkEventExpose *ev);

    void update_fs_list();
    void selected_fs();
    void draw_fs_graph(long long total, long long used,
                       long long used_data,
                       long long new_data);

    long long parse_size(const Glib::ustring &size);
    long long get_total_table_space(const std::string &fspath);
    bool check_if_path_in_fs(const std::string &fspath, const std::string &file);
    std::string get_data_directory();

    int fscompare(const Gtk::TreeIter &a, const Gtk::TreeIter &b);
    
    void update_sensitivity();
    void add_file();
    void remove_file();

    void add_value(const Glib::ustring &file,
                   const Glib::ustring &size);
    void set_auto_extends(bool flag);
    
  private:
    MGGladeXML *_dlg_xml;

    Glib::RefPtr<Gtk::ListStore> _fs_list;

    void update_dlg_sensitivity();

  public:
    MInnoDBFilePathEditor(MDynamicInterface *owner, MInstanceInfo *info);
    ~MInnoDBFilePathEditor();

    Glib::ustring get_text();
    void set_text(const Glib::ustring &value);
};


class MFilePathEditor : public Gtk::HBox {
    Gtk::Entry _entry;
    Gtk::Button _button;
    bool _dir_only;
    
    void clicked();
  public:
    MFilePathEditor(MDynamicInterface *owner, bool dir_only);
    
    Glib::ustring get_text();
    void set_text(const Glib::ustring &value);
};


class MDynamicInterface : public Glib::ObjectBase {
    class ImageCheck;

    struct WObject {
      MYX_GUI_WIDGET_TYPE type;
      Gtk::Widget *w;
      Gtk::Widget *aux;
      Gtk::Label *label;
      Gtk::Label *dlabel;
      MGImageCheckButton *enable;
      MYX_GUI_WIDGET *ptr;
      bool startup;
    };

#if 0
    struct WSensControl {
      MYX_GUI_WIDGET_TYPE type;
      Gtk::Widget *source;
      std::string value;
      Gtk::Widget *target;
    };
#endif

    Gtk::Notebook *_top_notebook;

    MInstanceInfo *_instance;
    
    MYX_GUI_DESCRIPTION *_descr;
#if 0    
    std::list<WSensControl*> _sens_controls;
#endif
    Glib::RefPtr<Gdk::Pixbuf> _page_icon;

    Glib::RefPtr<Gdk::Pixbuf> _edit_on;
    Glib::RefPtr<Gdk::Pixbuf> _edit_off;

    std::map<std::string,WObject> _widgets;

    std::string _xml_file;
    std::string _version;
    std::string _cnf_file;
    std::string _cnf_section;

    Gtk::Tooltips _tips;

    Gtk::Widget *create_widget(MYX_GUI_WIDGET *w);

    Gtk::Widget *process_group(MYX_GUI_GROUP *page);
    bool process_page(MYX_GUI_PAGE *page);
#if 0
    void add_enabled_by_handler(MYX_GUI_ENABLED_BY_CONTROL *info,
                                Gtk::Widget *target);

    void update_sensitivity(WSensControl *data);
#endif
    void revert_page_values(MYX_GUI_PAGE *page);

    bool fetch_values();
    bool fetch_page_values(MYX_GUI_PAGE *page);

    void option_enable_toggled(const char *id);

  public:
    MDynamicInterface(Gtk::Notebook *top_note, 
                      const Glib::RefPtr<Gdk::Pixbuf> &icon,
                      MInstanceInfo *instance);
    ~MDynamicInterface();
    MYX_ADMIN_LIB_ERROR open(const std::string &file, const std::string &version,
                             const std::string &cnf_file, 
                             const std::string &section);
    int save();
    void revert_values();

    Glib::ustring get_text_value(const std::string &id);
    Glib::ustring get_spin_value(const std::string &id);
    Glib::ustring get_check_value(const std::string &id);
    Glib::ustring get_dropdown_value(const std::string &id);

    Glib::ustring get_option_value(const std::string &id);

    void set_text_value(const std::string &id, const std::string &value);
    void set_spin_value(const std::string &id, const std::string &value);
    void set_check_value(const std::string &id, const std::string &value);
    void set_dropdown_value(const std::string &id, const std::string &value);

    void set_option_value(const std::string &id, const std::string &value);
};

#endif /* _MDYNAMICINTERFACE_H_ */
