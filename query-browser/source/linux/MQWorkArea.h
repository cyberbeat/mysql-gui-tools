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

#ifndef _MQWORKAREA_H_
#define _MQWORKAREA_H_

#include "myqb.h"

#include <gtkmm/notebook.h>
#include <gtkmm/menu.h>
#include <gtkmm/tooltips.h>
#include <gtkmm/image.h>
#include <gtkmm/treemodel.h>
#include <gtkmm/treestore.h>
#include <gtkmm/treeview.h>

#include "MQBaseModule.h"

#include "MGSQLTextBuffer.h"
#include "myx_public_interface.h"

#include "MYXInterface.h"

#include "MQBookmarks.h"

#include "MQResultTab.h"
#include "MQScriptEditorTab.h"
#include "MGCompletionList.h"


class MGBrowserList;
class MGTableBrowserList;
class MGSchemaBrowserHelper;
class MQFunctionBrowser;
class MQHistoryBrowser;
class MGGladeXML;
class MQHistory;

class MQResultSetView;

class MQScriptEditorTab;

class MQGlobalQueryParameters;
class MQQueryParameters;

class MQWorkArea;


class MQWorkArea : public MQBaseModule {
    friend class MQMySQLConsole;

    MGGladeXML *_xml;
    MGGladeXML *_search_xml;
    MGGladeXML *_table_add_xml;
    MGGladeXML *_column_add_xml;
    
    MGTableBrowserList *_table_browser;
    MQHistoryBrowser *_history_browser;
    MQFunctionBrowser *_function_browser;
    MQFunctionBrowser *_statement_browser;
    
    MGSchemaBrowserHelper *_table_helper;

    Gtk::Menu _table_browser_menu;
    Glib::ustring _last_selected_schema;

    MQHistory *_history;
    MYX_SQL_FUNCTIONINDEX *_function_index;
    MYX_SQL_FUNCTIONINDEX *_statement_index;

    MQHelpTab *_help_tab;

    Glib::RefPtr<MGSQLTextBuffer> _query_buffer;
    int _query_line_height;

    Glib::RefPtr<Gtk::TextTagTable> _tag_table;

    MQGlobalQueryParameters *_global_params;

    Gtk::Widget *_query_toolbar;
    Gtk::Widget *_script_toolbar;

    Gtk::Menu _execute_menu;
    Gtk::Menu _execute_script_menu;
    
    Gtk::Menu _back_history_menu;
    Gtk::Menu _next_history_menu;
        
    Gtk::Menu _popup_menu;

    Gtk::Menu _recent_files_menu;

    Gtk::Tooltips _tips;

    class TransColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        TransColumns()
        {
          add(icon);
          add(text);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
    } _tcolumns;

    Gtk::TreeView *_trans_tree;
    Glib::RefPtr<Gtk::ListStore> _trans_log;
    
    std::map<std::string,Gdk::Cursor> _cursors;
    
    Glib::RefPtr<Gdk::PixbufAnimation> _busy_anim;
    Glib::RefPtr<Gdk::Pixbuf> _idle_image;

    guint _last_drop_time;
    
    bool _inside_query_text;
    bool _inside_menu;
    
    MGCompletionList *_compl;

    // parameter list
    class ParamColumns : public Gtk::TreeModel::ColumnRecord
    {
      public:
        ParamColumns()
        {
          add(icon);
          add(text);
          add(value);
          add(editable);
          add(type);
        }
        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > icon;
        Gtk::TreeModelColumn<Glib::ustring> text;
        Gtk::TreeModelColumn<Glib::ustring> value;
        Gtk::TreeModelColumn<bool> editable;
        Gtk::TreeModelColumn<int> type;
    } _pcolumns;
    
    Glib::RefPtr<Gtk::TreeStore> _param_store;
    Gtk::Menu _param_menu;
    
    bool _first_time_show;
        
    int _resultset_count;

    bool hide_if_needed();
    
    // general
    void setup_button_images();
    void bind_main_menu_items();

    void default_schema_changed(bool refresh_only=true);

    void save_query();
    void load_query_mi();
    void load_query(const std::string &file);

    void update_edit_menu();
    void edit_copy_mi();
    void edit_cut_mi();
    void edit_paste_mi();
    void edit_find_mi();
    void edit_copy_row_mi();
    void edit_copy_row_names_mi();

    void quit_mi();

    void toggle_view_type(const char *button);

    virtual void add_tab(MQBaseTab *tab);
    virtual Gtk::Menu *get_tab_menu() { return &_popup_menu; };

    Glib::ustring interactive_modify_sql(const Glib::ustring &current_query,
                                         Gdk::ModifierType modifier_state, int &cursor_pos,
                                         Gtk::TextView *view= 0, bool below= false);
    void save_file();
    void save_file_as();

    void reload_mi();

    void recent_file_open(const char *file);
    void refresh_recent_menu();
    
    void edit_all_stored_procedures();
    void edit_selection();
    void edit_stored_procedure();
    void edit_view();
    void create_stored_procedure();
    void create_view();
    void create_view_with_query();

    // toolbar/query area
    void update_resultset_menu(MQResultSetView *rsview, Gtk::Menu *menu);
    
    void init_tag_table();

    void object_add_menu_drag_leave(const Glib::RefPtr<Gdk::DragContext>& context, guint time);
    bool object_add_menu_drag_motion(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, guint time);

    void table_add_dropped(MYX_Q_TABLE_ADD_TYPE type);
    void show_table_add_menu(Gtk::Widget *view, bool below);

    void column_add_dropped(MYX_Q_CLAUSE_TYPE type);
    void show_column_add_menu(Gtk::Widget *view, bool below);
  
    void query_drag_leave(const Glib::RefPtr<Gdk::DragContext>& context, guint time);
    bool query_drag_motion(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, guint time);

    void search_resultset();

    void export_resultset(const char *fmt);

    //bool get_schema_from_selection_data(GtkSelectionData* selection_data,
    bool get_schema_from_selection_data(const Gtk::SelectionData& selection_data,
                                        Glib::ustring &catalog,
                                        Glib::ustring &schema,
                                        Glib::ustring &query);
                              
    
    //void query_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, GtkSelectionData* selection_data, guint info, guint time);
    void query_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData& selection_data, guint info, guint time);

    bool query_get_completion_list(const Gtk::TextIter&,std::list<Glib::ustring>&);
    
    bool query_text_key_press(GdkEventKey *ev);
    void text_changed();
    void execute_clicked(bool refresh= false);
    void get_exec_pos(int &x, int &y, bool &push_in);
    void execute_menu_clicked();
    void stop_clicked();

    void execute_in_new_tab();
    void execute_in_split_tab();

    void trans_start_clicked();
    void trans_commit_clicked();
    void trans_rollback_clicked();
    
    void trans_started();
    void trans_ended(bool commited);
    void trans_command(Glib::ustring query);

    void compare_results();
    void explain_result();

    void rset_changed(MQResultSetView *sender);

    void rset_row_changed(MQResultSetView *sender, MQResultTab *tab);
    
    void tab_changed(GtkNotebookPage* page, guint page_num);

    void set_query_bar_sensitivity(bool flag);
    void update_toolbar_sensitivity();

    void get_back_pos(int &x, int &y, bool &push_in);
    void get_next_pos(int &x, int &y, bool &push_in);
    void back_button_clicked();
    void next_button_clicked();
    void goback_clicked();
    void gonext_clicked();

    void do_tab_action(MQBaseTab *sender,/*MQBaseTab::TabActionType*/int action);
    void do_tab_action2(MQBaseTab::TabActionType action,MQResultSetView *rset);

    MQResultTab *get_tab_for_rsview(MQResultSetView *rsview);

    void query_started(MQResultSetView *sender, bool saving);
    void query_finished(MQResultSetView *sender);
    void query_more_data(MQResultSetView *sender, MYSQL *mysql);

    void set_busy_animation(bool flag);
    void set_script_busy_animation(bool flag);

    // script editor
    bool script_editor_key_press(GdkEventKey *ev);
    void get_execs_pos(int &x, int &y, bool &push_in);
    void update_cursor_position();
    void execute_script_selection();
    void execute_script_stepping();
    void execute_script();
    void execute_script_menu_clicked();
    void continue_script();
    void step_over();
    void toggle_breakpoint();
    void clear_breakpoints();
    void run_until_return();
    void stop_script(bool pause_only);
    void reset_script();
    void load_script_mi();
    void load_script_into();
    void load_script(const std::string &file);
    void save_script(bool save_as);
  
    void script_edited(bool flag);

    void script_editor_script_finished(MQScriptEditorTab *sender);
    void script_editor_state_changed(MQScriptEditorTab *sender,MQScriptEditorTab::State state);

    // sidebar
    void toggle_sidebar_mi();

    void update_browser_menu();
    void update_parameter_list(MQQueryParameters *params);
    void popup_param_menu(GdkEventButton *event);
    void param_add_mi();
    void param_delete_mi();
    void param_edited(const Glib::ustring& path,
                      const Glib::ustring& new_text);
    
    void update_history_menu(MQResultSetView *view);

    virtual void catalogs_refreshed();
    void select_schema_from_browser();
    void copy_sql();
        
    MYX_Q_CLAUSE_TYPE get_current_clause_type();
    void schemata_dbl_clicked();
    void schemata_selected(MGBrowserList *list,const Gtk::TreeIter &iter);
    
    // result set area
    
    void start_search(MQResultSetView *sender);

    void find_cancel();
    void find_next(MQResultSetView *target);
    void find_previous(MQResultSetView *target);

  public:
    MQWorkArea(GtkVBox *vbox);
    virtual ~MQWorkArea();
    static MQWorkArea *create(MGGladeXML *xml, MQMainWindowInterface *mainw);

    virtual Gtk::Widget *get_widget();
    
    virtual void set_dispatcher(MQQueryDispatcher *dispatcher);

    void open_bookmark(MQBookmarks::BookmarkItem *bookmark);
    void open_history(std::string id);
    void show_function_help(const std::string &id);
    void show_syntax_help(const std::string &id);
    void show_quickstart();
    void show_help();

    void execute_query_raw(const Glib::ustring &query);

    void execute_query(const Glib::ustring &catalog, const Glib::ustring &schema,
                       const Glib::ustring &query, MQResultSetView *rset=0,
                       bool refresh=false);
    
    void save_bookmark();
    void toggle_sidebar(bool show);

    virtual void setup();
    virtual void show();
    virtual void hide();

    void bookmark_current_query();

    MQResultSetView *add_result_view(const Glib::ustring &title,
                                 MQResultTab *in_tab=0,
                                 bool vertical=false);

    MQScriptEditorTab *add_script_view(const Glib::ustring &title);

    Glib::ustring shorten_query(const Glib::ustring &query);
};

#endif /* _MQWORKAREA_H_ */
