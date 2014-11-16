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


#ifndef _MQRESULTSETVIEW_H_
#define _MQRESULTSETVIEW_H_

#include "myqb.h"
#include <gtkmm/box.h>
#include <gtkmm/treeview.h>
#include <gtkmm/treemodel.h>
#include <gtkmm/eventbox.h>
#include <gtkmm/separator.h>
#include <gtkmm/button.h>
#include <gtkmm/liststore.h>
#include <gtkmm/treestore.h>
#include <gtkmm/scrolledwindow.h>
#include <gtkmm/menu.h>

#include "MGResultSetModel.h"
#include "MGCellRendererBlob.h"

class MQQueryDispatcher;
class MQHistory;

class MQGlobalQueryParameters;
class MQQueryParameters;

class MGBlobEditor;

class MQResultSetView : public Gtk::EventBox, public MGBlobDelegate {
  public:
    typedef sigc::signal2<void,MQResultSetView*,bool> QueryStartedSignal;
    typedef sigc::signal1<void,MQResultSetView*> QueryFinishedSignal;
    typedef sigc::signal2<void,MQResultSetView*,MYSQL*> QueryMoreDataSignal;
    
    typedef sigc::signal1<void,MQResultSetView*> ActivateSignal;
    
    typedef sigc::signal1<void,MQResultSetView*> RowChangedSignal;

  protected:
    Gtk::VBox _box;

    class ModelColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        std::vector<Gtk::TreeModelColumn<Glib::ustring> *> columns;
        std::vector<Gtk::TreeModelColumn<Glib::ustring> *> bgcolors;
        std::vector<Gtk::TreeModelColumn<gpointer> *> blobs;
        Gtk::TreeModelColumn<bool> editable;
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > indicator;

        // columns are like:
        // ( text|blob color )* editable indicator

        void add_text_column()
        {
          Gtk::TreeModelColumn<Glib::ustring> *col= new Gtk::TreeModelColumn<Glib::ustring>();
          columns.push_back(col);
          add(*col);
          Gtk::TreeModelColumn<Glib::ustring> *color= new Gtk::TreeModelColumn<Glib::ustring>();
          bgcolors.push_back(color);
          add(*color);
        }

        void add_blob_column()
        {
          Gtk::TreeModelColumn<gpointer> *col= new Gtk::TreeModelColumn<gpointer>();
          blobs.push_back(col);
          add(*col);
          Gtk::TreeModelColumn<Glib::ustring> *color= new Gtk::TreeModelColumn<Glib::ustring>();
          bgcolors.push_back(color);
          add(*color);
        }
        
        void add_extra_columns()
        {
          // add new columns after this line and update MGResultSetModel
          add(editable);
          add(indicator);
        }

        ~ModelColumns()
        {
          for (int i= 0; i < (int)columns.size(); i++)
            delete columns[i];
          for (int i= 0; i < (int)bgcolors.size(); i++)
            delete bgcolors[i];
          for (int i= 0; i < (int)blobs.size(); i++)
            delete blobs[i];
        }
    };
    
    class MessageColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        MessageColumns()
        {
          add(icon);
          add(errornum);
          add(message);
          add(row);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<int> errornum;
        Gtk::TreeModelColumn<int> row;
        Gtk::TreeModelColumn<Glib::ustring> message;
    } _msg_columns;
    
    class ExplainColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        ExplainColumns()
        {
          add(select_type);
          add(table);
          add(type);
          add(possible_keys);
          add(key);
          add(key_len);
          add(ref);
          add(rows);
          add(extra);
        }
        Gtk::TreeModelColumn<Glib::ustring> select_type;
        Gtk::TreeModelColumn<Glib::ustring> table;
        Gtk::TreeModelColumn<Glib::ustring> type;
        Gtk::TreeModelColumn<Glib::ustring> possible_keys;
        Gtk::TreeModelColumn<Glib::ustring> key;
        Gtk::TreeModelColumn<Glib::ustring> key_len;
        Gtk::TreeModelColumn<Glib::ustring> ref;
        Gtk::TreeModelColumn<Glib::ustring> rows;
        Gtk::TreeModelColumn<Glib::ustring> extra;
    } _exp_columns;

    class PopupTreeView : public Gtk::TreeView {
        virtual bool on_button_press_event(GdkEventButton *ev);
        Gtk::Menu *_popup;
        
        sigc::signal0<void> _signal_will_popup;
      public:
        PopupTreeView(Gtk::Menu *popup) : Gtk::TreeView(), _popup(popup) {};
        
        sigc::signal0<void> signal_will_popup() { return _signal_will_popup; };
    };
    
    enum FieldAction {
      FieldLoad,
      FieldSave,
      FieldCopy,
      FieldClear,
      FieldEdit,
      FieldView
    };

    ModelColumns *_columns;

    Gtk::ScrolledWindow _scroll;
    PopupTreeView *_tree;

    Gtk::EventBox _ebox;
    Gtk::HBox _hbox;
    Gtk::Label _status_label;
    Gtk::VSeparator _sep1;
    Gtk::Button _edit_btn;
    Gtk::VSeparator _sep1a;
    Gtk::Button _apply_btn;
    Gtk::VSeparator _sep1b;
    Gtk::VSeparator _sep2;
    Gtk::Button _first_btn;
    Gtk::VSeparator _sep2a;
    Gtk::Button _last_btn;
    Gtk::VSeparator _sep2b;
    Gtk::Button _search_btn;

    Gtk::ScrolledWindow _msgscroll;
    Gtk::TreeView _msgtree;
    Glib::RefPtr<Gtk::ListStore> _msgstore;

    Gtk::Window *_explain_window;
    
    Glib::RefPtr<MGResultSetModel> _model;

    Gtk::Menu _edit_menu;
    
    MYX_RESULTSET *_result;
    MQQueryParameters *_params;

    bool _current_query_cancelled;
    Glib::ustring _current_query;
    struct timeval _current_query_start;

    MQQueryDispatcher *_dispatcher;

    MQHistory *_history_list;

    std::list<std::string> _history;
    int _history_index;

    QueryStartedSignal _query_started_signal;
    QueryFinishedSignal _query_finished_signal;
    QueryMoreDataSignal _query_more_data_signal;
    ActivateSignal _activate_signal;
    RowChangedSignal _row_changed_signal;
    sigc::signal0<void> _editable_change_signal;

    bool _editing;
    bool _is_busy;
    bool _compact_mode;
    
    bool _active;
    
    bool get_selected_field(Gtk::TreePath &path, int &column);

    void editor_save(MGBlobEditor *editor);
    virtual void blob_edit(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column);
    virtual void blob_save(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column);
    virtual void blob_load(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column);
    virtual void blob_clear(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column);
    virtual void blob_view(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column);
    
    void blob_data_edited(const Glib::ustring& path, gpointer data, gsize size, int column);
    void string_data_edited(const Glib::ustring& path, const Glib::ustring& new_text, int column);

    void display_result(MYX_RESULTSET *result);
    void display_errors(MYX_MYSQL_ERROR_MSGS *errors);

    void query_finished(MYX_RESULTSET *result,MYX_MYSQL_ERROR_MSGS*warnings);
    bool query_progress(MYX_RESULTSET *result, unsigned long count,
                        unsigned long prev_count);
    void query_error(MYX_RESULTSET *result, MYX_MYSQL_ERROR_MSGS*errors, MYX_LIB_ERROR merror);
    void query_fetch_more(MYSQL *mysql);
    
    void save_finished(MYX_RS_ACTION_ERRORS *errors);

    void update_menu();
    
    void edit_start();
    void edit_apply();

    void go_first();
    void go_last();

    std::vector<bool> get_search_column_list(const Glib::ustring &column_list);

    void selected_row();
    void selected_error();

    void add_history_entry(const Glib::ustring &query);

    void update_edit_button_sensitivity();

    void set_sensitive(bool flag);
    
    bool show_menu(GdkEventButton *ev);
    
    bool editbar_clicked(GdkEventButton *ev);
    bool focused(GdkEventFocus *ev);
    
    void append_error(int num, const Glib::ustring &msg,
                      Glib::RefPtr<Gdk::Pixbuf> icon= Glib::RefPtr<Gdk::Pixbuf>(),
                      int row=-1);

    void save_cell_change(MGBlobEditor *editor,
                          unsigned int column);
    
    void add_row_mi();
    void delete_row_mi();
    void undo_row_mi();
    
    bool tree_key_released(GdkEventKey *event);

    void column_action_mi(FieldAction action);
  public:
    MQResultSetView(MQQueryDispatcher *disp, MQHistory *history,
                    MQGlobalQueryParameters *params);
    ~MQResultSetView();

    void set_show_editbar(bool flag);
    
    void redisplay();
    
    void execute_query(const Glib::ustring &query,
                       bool refresh= false);
    void fetch_more(MYSQL *mysql);
    void cancel_query();
    
    bool explain_query();
    bool explain_shown() { return _explain_window != 0; };
    void hide_explain();
    bool hide_explain_d(GdkEventAny *ev);
    
    Glib::ustring get_shown_query() { return _result ? _current_query : ""; };
    
    Glib::ustring go_history_next();
    Glib::ustring go_history_back();
    int get_history_index();

    bool is_busy() { return _is_busy; };

    bool get_active();
    void set_active(bool flag);
    
    void set_compact_editbar(bool flag);
    
    void set_search_func(const sigc::slot1<void,MQResultSetView*> &slot);

    MYX_RESULTSET *get_resultset() { return _result; };
    MYX_RS_ROW *get_selected_row();
    
    Glib::RefPtr<MGResultSetModel> get_model() { return _model; };
    
    void copy_row_to_clipboard();
    void copy_row_names_to_clipboard();
    
    bool find_next(const Glib::ustring &str, const Glib::ustring &columns);
    bool find_previous(const Glib::ustring &str, const Glib::ustring &columns);
    
    bool find_next_diff();
    bool find_previous_diff();

    bool is_editable();
    
    Gtk::TreePath get_selected_row_path();
    void set_selected_row_path(const Gtk::TreePath &path);

    std::list<std::string> get_history() const { return _history; };

    QueryStartedSignal signal_query_started() { return _query_started_signal; };
    QueryFinishedSignal signal_query_finished() { return _query_finished_signal; };
    QueryMoreDataSignal signal_more_data() { return _query_more_data_signal; };
    
    ActivateSignal signal_activate() { return _activate_signal; };
    RowChangedSignal signal_row_changed() { return _row_changed_signal; };
    
    sigc::signal0<void> signal_editable_changed() { return _editable_change_signal; };

    Gtk::ScrolledWindow *get_scrollwin() { return &_scroll; };
    
    MQQueryParameters *get_parameters() { return _params; };

    Gtk::Menu *get_menu() { return &_edit_menu; };
    
    
    bool dispose_if_no_result;
};


#endif /* _MQRESULTSETVIEW_H_ */
