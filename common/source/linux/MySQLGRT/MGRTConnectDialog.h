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


#ifndef _MGRTCONNECTDIALOG_H_
#define _MGRTCONNECTDIALOG_H_


#include <MySQLGRT/MGRT.h>
#include <gtkmm/container.h>
#include <gtkmm/treeview.h>
#include <gtkmm/label.h>
#include <gtkmm/sizegroup.h>
#include <gtkmm/frame.h>
#include <gtkmm/button.h>
#include <gtkmm/window.h>
#include <gtkmm/combobox.h>
#include <gtkmm/tooltips.h>
#include <gtkmm/liststore.h>
#include <gtkmm/box.h>
#include <map>

class MGRTConnectDialog {
    class ComboColumns : public Gtk::TreeModel::ColumnRecord 
    {
      public:
        ComboColumns() 
        {
          add(name);
          add(data);
        };
        Gtk::TreeModelColumn<Glib::ustring> name;
        Gtk::TreeModelColumn<MGRTValue> data;
    } _combo_columns;

    class ParameterMapping;
    
    MGRT *_grt;
    
    Gtk::Window *_window;
    
    Gtk::Frame *_rdbms_frame;
    Gtk::Frame *_option_frame;
    Gtk::Frame *_advanced_frame;
    
    Gtk::ComboBox *_rdbms_combo;
    Glib::RefPtr<Gtk::ListStore> _rdbms_list;
    Gtk::ComboBox *_driver_combo;
    Glib::RefPtr<Gtk::ListStore> _driver_list;

    Gtk::HBox *_connection_box;
    Gtk::Label *_connection_label;
    Gtk::ComboBox *_connection_combo;
    Glib::RefPtr<Gtk::ListStore> _connection_store;
    Gtk::Button *_add_button;
    Gtk::Button *_remove_button;
    
    Gtk::Button *_details_button;
    Gtk::Button *_cancel_button;
    Gtk::Button *_clear_button;
    Gtk::Button *_connect_button;

    std::map<Glib::ustring,ParameterMapping*> _parameters;
    
    
    Gtk::Widget *_first_control;
    
    Gtk::Tooltips _tooltip;
    
    Glib::ustring _conn_info_path;
    Glib::ustring _conn_target_path;
    
    bool _pick_rdbms;
    bool _pick_schema;
    bool _jdbc_only;
    bool _setting_conn_values;

    sigc::signal<void,bool> _signal_ready_changed;
    
    Glib::RefPtr<Gtk::SizeGroup> _param_sg;
    Glib::RefPtr<Gtk::SizeGroup> _advanced_sg;

    MGRTValue get_default_driver();
    
    int index_of_list_item(Glib::RefPtr<Gtk::ListStore> list, const MGRTValue &value);
    
    void add_connection();
    void remove_connection();
    
    void driver_param_value_changed();
    int add_drivers_to_combo(MGRTValue *rdbms);
    void rdbms_selected();
    void driver_selected();
    void build_driver_controls(Gtk::Frame *frame,
                               MGRTValue *driver,
                               bool advanced);
    void fill_stored_connections_for_driver(MGRTValue *driver);
    void connection_changed();
    void set_connection(MGRTValue conn= MGRTValue());

  public:
    MGRTConnectDialog(MGRT *grt, const Glib::ustring &path, const Glib::ustring &target,
                      bool windowed);
    ~MGRTConnectDialog();

    void setup();

    void set_selects_rdbms(bool flag);
    void set_edits_schema(bool flag);
    
    void refresh_rdbms_info();
    
    MGRTValue write_connection_to_target();
    
    Gtk::Container *params_panel() { return _option_frame; };
    Gtk::Container *rdbms_panel() { return _rdbms_frame; };
    Gtk::Container *advanced_panel() { return _advanced_frame; };
    
    sigc::signal<void,bool> signal_ready_changed() { return _signal_ready_changed; };
};

#endif /* _MGRTCONNECTDIALOG_H_ */
