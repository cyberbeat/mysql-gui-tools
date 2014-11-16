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


#include "MQWorkArea.h"
#include "MGGladeXML.h"

#include "MGTableBrowserList.h"
#include "MQHistoryBrowser.h"
#include "MQBookmarkBrowser.h"
#include "MQFunctionBrowser.h"
#include "MGSchemaBrowserHelper.h"

#include "MQHistory.h"

#include "MGSQLTextBuffer.h"

#include "myg_gtkutils.h"

#include "MQQueryDispatcher.h"
#include "MQResultTab.h"
#include "MQScriptEditorTab.h"
#include "MQResultSetView.h"
#include "MGImage.h"

#include "MQQueryParameters.h"

#include <gtkmm/menubar.h>


static void parse_drop_query_data(char *data, 
                                  char *&catalog, char *&schema, char *&query)
{
  char *ptr;
  
  catalog= NULL;
  schema= NULL;
  query= NULL;

  ptr= strtok(data, "\n");
  if (strncmp(ptr, "C:", 2)==0)
    catalog= ptr+2;

  ptr= strtok(NULL, "\n");
  if (strncmp(ptr, "S:", 2)==0)
    schema= ptr+2;
  
  ptr= strtok(NULL, "");
  if (strncmp(ptr, "Q:", 2)==0)
    query= ptr+2;

  if (!catalog || !*catalog)
    catalog= "def";
  if (schema && !*schema)
    schema= NULL;
  if (query && !*query)
    query= NULL;
}


class DropDataReceivedProxyHack : public Glib::ObjectBase {
    // This is a hack needed because we need to know the target widget where
    // the query was dropped (when drag/dropping queries from the bookmark
    // or history lists). sigc::bind<> on the normal signal handler
    // did not work, probably because the signal handler already has too
    // many arguments.
  public:
    MQResultSetView *rset;
    MQWorkArea *warea;
    //void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, GtkSelectionData* selection_data, guint info, guint time)
    void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData& selection_data, guint info, guint time)
    {
      std::string type;
#if 0
      std::vector<std::string> targets= context->get_targets();
      if (!targets.empty())
        g_message("==> %s", targets[0].c_str());
#else
      //GdkAtom target= rset->drag_dest_find_target(context, rset->drag_dest_get_target_list());
      //type= gdk_atom_name(target);
      type= rset->drag_dest_find_target(context, rset->drag_dest_get_target_list());
#endif
      if (!rset->is_busy() && (selection_data.get_length() >= 0) && (selection_data.get_format() == 8))
      {
        char *data= g_strndup((const char*)selection_data.get_data(), selection_data.get_length());
        
        if (type == "x-mysqlgui-schema-object")
        {
          Glib::ustring catalog, schema, table;

          if (MGTableFromSchemaObjectName(data, catalog, schema, table))
          {
            Glib::ustring query;
            
            query= "SELECT * FROM `"+schema+"`.`"+table+"`";
            if (prefs.default_limit_value > 0)
              query+=" LIMIT 0,"+tostr(prefs.default_limit_value);
          
            warea->execute_query(catalog, schema, query, false);
          }
        }
        else
        {
          char *catalog, *schema, *query;
          
          parse_drop_query_data(data, catalog, schema, query);
          
          if (catalog && schema && query)
            warea->execute_query(catalog, schema, query, rset);
        }
        g_free(data);
      }
    }

    static void destroy(gpointer bla)
    {
      DropDataReceivedProxyHack *o= (DropDataReceivedProxyHack*)bla;
      delete o;
    }
};


// same hack for table_add menu
class TableAddDropProxyHack : public Glib::ObjectBase {
  public:
    typedef sigc::signal1<void,MYX_Q_TABLE_ADD_TYPE> TableAddSignal;

    MYX_Q_TABLE_ADD_TYPE type;

  private:
    TableAddSignal _signal;

  public:
    TableAddDropProxyHack(MYX_Q_TABLE_ADD_TYPE t) : type(t) {};

    TableAddSignal signal_activate() { return _signal; };

    //void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, GtkSelectionData* selection_data, guint info, guint time)
    void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData& selection_data, guint info, guint time)
    {
      if ((selection_data.get_length() >= 0) && (selection_data.get_format() == 8))
      {
        _signal.emit(type);
      }
    }

    static void destroy(gpointer bla)
    {
      TableAddDropProxyHack *o= (TableAddDropProxyHack*)bla;
      delete o;
    }
};

// same hack for column_add menu
class ColumnAddDropProxyHack : public Glib::ObjectBase {
  public:
    typedef sigc::signal1<void,MYX_Q_CLAUSE_TYPE> ColumnAddSignal;

    MYX_Q_CLAUSE_TYPE type;

  private:
    ColumnAddSignal _signal;

  public:
    ColumnAddDropProxyHack(MYX_Q_CLAUSE_TYPE t) : type(t) {};

    ColumnAddSignal signal_activate() { return _signal; };

    //void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, GtkSelectionData* selection_data, guint info, guint time)
    void drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData& selection_data, guint info, guint time)
    {
      if ((selection_data.get_length() >= 0) && (selection_data.get_format() == 8))
      {
        _signal.emit(type);
      }
      context->drag_finish(true, false, time);
    }

    static void destroy(gpointer bla)
    {
      ColumnAddDropProxyHack *o= (ColumnAddDropProxyHack*)bla;
      delete o;
    }
};

//----------------------------------------------------------------------
static const char *query_menu_exceptions[]= {
  "start_transaction",
    "commit_transaction",
    "rollback_transaction",
    "execute1",
    "stop1",
    NULL
};


static void set_menu_items_sensitive(Gtk::Menu *menu, bool flag,
                                     const char **exceptions)
{
  for (int i= menu->items().size()-1; i>=0; --i)
  {
    const char *s= menu->items()[i].get_name().c_str();
    bool skip= false;
    if (exceptions)
    {
      for (int j= 0; exceptions[j]; j++)
        if (strcmp(exceptions[j], s)==0)
        {
          skip= true;
          break;
        }
    }
    if (!skip)
      menu->items()[i].set_sensitive(flag);
  }
}


MQWorkArea::MQWorkArea(GtkVBox *vbox)
  : MQBaseModule(vbox), _xml(0), _search_xml(0), _table_add_xml(0), _history_browser(0),
   _help_tab(0), 
   _trans_tree(0),
   _last_drop_time(0), _compl(0), _first_time_show(true), _resultset_count(1)
{
  MYX_LIB_ERROR err;

  init_tag_table();

  _inside_menu= false;
  _inside_query_text= false;

  _history= MQHistory::instance();

  _function_index= myx_load_sql_function_list(get_app_file("mysqlqb_functions.xml").c_str(), &err);
  if (!_function_index)
  {
    myg_show_xlib_error(*static_cast<Gtk::Window*>(get_toplevel()),
                        ufmt(_("Could not load function help file.\n%s"),
                             get_app_file("mysqlqb_functions.xml").c_str()),
                        err);
  }

  _statement_index= myx_load_sql_function_list(get_app_file("mysqlqb_statements.xml").c_str(), &err);
  if (!_statement_index)
  {
    myg_show_xlib_error(*static_cast<Gtk::Window*>(get_toplevel()),
                        ufmt(_("Could not load statement help file.\n%s"),
                             get_app_file("mysqlqb_statements.xml").c_str()),
                        err);
  }

  _global_params= new MQGlobalQueryParameters();

  myg_menu_add(_popup_menu,_("Add New _Resultset Tab"),
               sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::ANewQuery),
               "add_rs");
  myg_menu_add(_popup_menu, _("Add New _Script Tab"),
               sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::ANewScript),
               "add_script");
  
  myg_menu_add(_popup_menu);
  
  myg_menu_add(_popup_menu, _("Remove _Other Tabs"),
               sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::ACloseOther),
               "remove_other");
  myg_menu_add(_popup_menu, _("_Remove Tab"),
               sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::AClose),
               "remove");
}


MQWorkArea::~MQWorkArea()
{
  delete _history;
  delete _global_params;
  delete _compl;
  delete _table_helper;
}


void MQWorkArea::set_dispatcher(MQQueryDispatcher *dispatcher)
{
  MQBaseModule::set_dispatcher(dispatcher);
  
  dispatcher->signal_schema_changed().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::default_schema_changed),false));
  dispatcher->signal_transaction_started().connect(sigc::mem_fun(*this,&MQWorkArea::trans_started));
  dispatcher->signal_transaction_ended().connect(sigc::mem_fun(*this,&MQWorkArea::trans_ended));
  dispatcher->signal_transaction_command().connect(sigc::mem_fun(*this,&MQWorkArea::trans_command));

  _query_buffer= MGSQLTextBuffer::create(_tag_table, dispatcher->get_mysql());
  _query_buffer->signal_changed().connect(sigc::mem_fun(*this,&MQWorkArea::text_changed));
  _xml->get_text("query_text")->set_buffer(_query_buffer);

  _compl= new MGCompletionList(_xml->get_text("query_text"));
  _compl->signal_request_suggestions().connect(sigc::mem_fun(*this,&MQWorkArea::query_get_completion_list));
  
  Pango::FontDescription font;
  font.set_size(10*Pango::SCALE);
  _compl->set_font(font);

  if (_catalogs)
    _table_browser->set_catalogs(_catalogs);

  _table_helper->set_mysql(_dispatcher->get_mysql());
  
  if (!dispatcher->get_current_schema().empty())
  {
    Gtk::TreeIter node;

    if (_table_browser->find_node(dispatcher->get_current_schema(),
                                  _table_browser->_columns.text,
                                  node))
    {
      _last_selected_schema= dispatcher->get_current_schema();
      _table_browser->set_selection(node);
      _table_browser->set_node_bold(node, true);
      _table_browser->expand_node(node);
      
      _mainw->get_bookmarks()->get_browser()->refresh_activatable(dispatcher->get_current_catalog(),
                                                                  dispatcher->get_current_schema());
    }
    
    _mainw->set_status(ufmt(_("Default schema set to '%s'"),
                            dispatcher->get_current_schema().c_str()));
  }
  else
    _mainw->set_status(ufmt(_("Default schema not set. You must select one in the File menu before executing any query.")));

  update_toolbar_sensitivity();
}


void MQWorkArea::bind_main_menu_items()
{  
  _xml->get_menu_item("edit_menuitem")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::update_edit_menu));
  _xml->get_menu_item("copy1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_copy_mi));
  _xml->get_menu_item("cut1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_cut_mi));
  _xml->get_menu_item("paste1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_paste_mi));
  _xml->get_menu_item("find1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_find_mi));
  _xml->get_menu_item("copy_row_values1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_copy_row_mi));
  _xml->get_menu_item("copy_column_names1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_copy_row_names_mi));

  _xml->get_menu_item("select_schema1")->signal_activate().connect(sigc::mem_fun(*this,&MQBaseModule::select_schema));

  _xml->get_menu_item("save_query1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::save_file_as));
  _xml->get_menu_item("save1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::save_file));

  _xml->get_menu_item("load_query1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::load_query_mi));
  _xml->get_menu_item("load_script1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::load_script_mi));

  _xml->get_menu_item("close_tab1")->signal_activate().connect(sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action), 0, MQBaseTab::AClose));

  {
    Gtk::MenuItem *pitem= _xml->get_menu_item("export_resultset1");
    Gtk::MenuItem *item;
    Gtk::Menu *menu= Gtk::manage(new Gtk::Menu());
    
    item= Gtk::manage(new Gtk::MenuItem("as CSV..."));
    menu->append(*item);
    item->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::export_resultset),"CSV"));

    item= Gtk::manage(new Gtk::MenuItem("as HTML..."));
    menu->append(*item);
    item->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::export_resultset),"HTML"));

    item= Gtk::manage(new Gtk::MenuItem("as XML..."));
    menu->append(*item);
    item->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::export_resultset),"XML"));

    item= Gtk::manage(new Gtk::MenuItem("as Excel Worksheet..."));
    menu->append(*item);
    item->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::export_resultset),"Excel"));

    pitem->set_submenu(*menu);
    menu->show_all();
  }

  _xml->get_menu_item("new_query_tab1")->signal_activate().connect(sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::ANewQuery));
  _xml->get_menu_item("new_script_tab1")->signal_activate().connect(sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),0,MQBaseTab::ANewScript));
  _xml->get_menu_item("quit2")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::quit_mi));

  
  _xml->get_menu_item("start_transaction")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::trans_start_clicked));
  _xml->get_menu_item("commit_transaction")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::trans_commit_clicked));
  _xml->get_menu_item("rollback_transaction")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::trans_rollback_clicked));

  _xml->get_menu_item("execute1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::execute_clicked),false));
  _xml->get_menu_item("stop1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::stop_clicked));

  _xml->get_menu_item("explain_query1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::explain_result));
  _xml->get_menu_item("compare_results1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::compare_results));

  _xml->get_menu_item("normal_view1")->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::toggle_view_type),"normal_view1"));
  _xml->get_menu_item("expand_query_area1")->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::toggle_view_type),"expand_query_area1"));
  _xml->get_menu_item("resultset_only1")->signal_activate().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MQWorkArea::toggle_view_type),"resultset_only1"));
  _xml->get_menu_item("sidebar1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::toggle_sidebar_mi));

  _xml->get_menu_item("add_to_bookmarks1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::bookmark_current_query));
  
  _xml->get_menu_item("quickstart")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::show_quickstart));

  _xml->get_menu_item("help1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::show_help));

  
  _xml->get_menu_item("run1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  _xml->get_menu_item("run_selection1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script_selection));
  _xml->get_menu_item("toggle_breakpoint1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::toggle_breakpoint));
  _xml->get_menu_item("clear_breakpoints1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::clear_breakpoints));
  _xml->get_menu_item("continue1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::continue_script));
  _xml->get_menu_item("pause1")->signal_activate().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::stop_script),true));
  _xml->get_menu_item("step_over1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::step_over));
  _xml->get_menu_item("reset1")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::reset_script));

  _xml->get_menu_item("create_sp")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::create_stored_procedure));
  _xml->get_menu_item("edit_all_sps")->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::edit_all_stored_procedures));
}


void MQWorkArea::update_edit_menu()
{
  Gtk::Widget *focused= _mainw->get_focus();
  bool can_insert= false;
  bool has_selection= false;

  if (focused && GTK_IS_TEXT_VIEW(focused->gobj()))
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer= ((Gtk::TextView*)focused)->get_buffer();
    Gtk::TextIter start, end;

    buffer->get_selection_bounds(start, end);

    has_selection= (start.get_offset() < end.get_offset());
    can_insert= buffer->get_iter_at_mark(buffer->get_insert()).can_insert();

    _xml->get_menu_item("cut1")->set_sensitive(has_selection);
    _xml->get_menu_item("copy1")->set_sensitive(has_selection);
    _xml->get_menu_item("paste1")->set_sensitive(can_insert);
    _xml->get_menu_item("find1")->set_sensitive(false);
    _xml->get_menu_item("copy_row_values1")->set_sensitive(false);
    _xml->get_menu_item("copy_column_names1")->set_sensitive(false);
  }
  else if (focused && GTK_IS_EDITABLE(focused->gobj()))
  {
    int start, end;
    Gtk::Editable *editable= (Gtk::Editable*)focused;

    editable->get_selection_bounds(start, end);

    has_selection= (start < end);
    can_insert= editable->get_editable();

    _xml->get_menu_item("cut1")->set_sensitive(has_selection);
    _xml->get_menu_item("copy1")->set_sensitive(has_selection);
    _xml->get_menu_item("paste1")->set_sensitive(can_insert);
    _xml->get_menu_item("find1")->set_sensitive(false);
    _xml->get_menu_item("copy_row_values1")->set_sensitive(false);
    _xml->get_menu_item("copy_column_names1")->set_sensitive(false);
  }
  else
  {
    bool can_search= false;
    
    int p= _note->get_current_page();
    if (get_tab(p)->get_type() == TResultSet)
    {
      MQResultTab *tab= static_cast<MQResultTab*>(get_tab(p));
      MQResultSetView *rset= tab->get_active();
      
      if (!rset->is_busy() && rset->get_resultset())
        can_search= true;
    }

    _xml->get_menu_item("cut1")->set_sensitive(false);
    _xml->get_menu_item("copy1")->set_sensitive(false);
    _xml->get_menu_item("paste1")->set_sensitive(false);
    _xml->get_menu_item("find1")->set_sensitive(can_search);
    
    _xml->get_menu_item("copy_row_values1")->set_sensitive(can_search);
    _xml->get_menu_item("copy_column_names1")->set_sensitive(can_search);
  }
}


void MQWorkArea::edit_copy_mi()
{
  Gtk::Widget *focused= _mainw->get_focus();

  if (focused && GTK_IS_TEXT_VIEW(focused->gobj()))
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer= ((Gtk::TextView*)focused)->get_buffer();

    buffer->copy_clipboard(Gtk::Clipboard::get());
  }
  else if (focused && GTK_IS_EDITABLE(focused->gobj()))
  {
    ((Gtk::Editable*)focused)->copy_clipboard();
  }
  else
  {
    g_message("unhandled copy");
  }
}


void MQWorkArea::edit_cut_mi()
{
  Gtk::Widget *focused= _mainw->get_focus();

  if (focused && GTK_IS_TEXT_VIEW(focused->gobj()))
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer= ((Gtk::TextView*)focused)->get_buffer();

    buffer->cut_clipboard(Gtk::Clipboard::get());
  }
  else if (focused && GTK_IS_EDITABLE(focused->gobj()))
  {
    ((Gtk::Editable*)focused)->cut_clipboard();
  }
  else
  {
    g_message("unhandled cut");
  }
}

void MQWorkArea::edit_copy_row_mi()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    rset->copy_row_to_clipboard();
  }
}

void MQWorkArea::edit_copy_row_names_mi()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    rset->copy_row_names_to_clipboard();
  }
}

void MQWorkArea::edit_paste_mi()
{
  Gtk::Widget *focused= _mainw->get_focus();

  if (focused && GTK_IS_TEXT_VIEW(focused->gobj()))
  {
    Glib::RefPtr<Gtk::TextBuffer> buffer= ((Gtk::TextView*)focused)->get_buffer();

    buffer->paste_clipboard(Gtk::Clipboard::get());
  }
  else if (focused && GTK_IS_EDITABLE(focused->gobj()))
  {
    ((Gtk::Editable*)focused)->paste_clipboard();
  }
  else
  {
    g_message("unhandled paste");
  }
}


void MQWorkArea::edit_find_mi()
{
  search_resultset();
}



void MQWorkArea::default_schema_changed(bool refresh_only)
{
  Gtk::TreeIter iter;
  Glib::ustring catalog= _dispatcher->get_current_catalog();
  Glib::ustring schema= _dispatcher->get_current_schema();

  if (!refresh_only)
    _mainw->set_status(ufmt(_("Changed default schema to '%s'"),
                            schema.c_str()));

  if (!_query_buffer->init_highlighting(_dispatcher->get_mysql()))
    g_message("error resetting syntax highlighter");

  iter= _table_browser->find_table(catalog, _last_selected_schema);
  if (iter)
    _table_browser->set_node_bold(iter, false);
  
  _last_selected_schema= schema;
  iter= _table_browser->find_table(catalog, schema);
  if (iter)
  {
    _table_browser->set_selection(iter);
    _table_browser->expand_node(iter);
    _table_browser->set_node_bold(iter, true);
  }
}


void MQWorkArea::save_file_as()
{
  switch (get_tab()->get_type())
  {
  case TResultSet:
    save_query();
    break;
  case TScriptEditor:
    save_script(true);
    break;
  default:
    break;
  }
}


void MQWorkArea::save_file()
{
  switch (get_tab()->get_type())
  {
  case TResultSet:
    save_query();
    break;
  case TScriptEditor:
    save_script(false);
    break;
  default:
    break;
  }
}


void MQWorkArea::save_query()
{
  Glib::ustring data;
  Gtk::FileSelection dlg(_("Save Query"));
  std::string fname;
    
  if (dlg.run()!=Gtk::RESPONSE_OK)
    return;
  
  fname= dlg.get_filename();

  _mainw->_state->add_recent_file("query://"+fname);
  refresh_recent_menu();

  _mainw->set_status(ufmt(_("Saving %s..."), fname.c_str()));
  data= _query_buffer->get_text();

  Glib::RefPtr<Glib::IOChannel> file;
  
  file= Glib::IOChannel::create_from_file(fname.c_str(), "w+");
  file->set_encoding();
  
  if (file->write(data) != Glib::IO_STATUS_NORMAL)
  {
    myg_show_sys_error(ufmt(_("Could not save to file '%s'."), fname.c_str()), errno);
  }
  file->close();

  _mainw->set_status(_("Query saved."));
}


void MQWorkArea::load_query_mi()
{
  Gtk::FileSelection dlg(_("Open Query"));

  if (dlg.run()==Gtk::RESPONSE_OK)
  {
    load_query(dlg.get_filename());
  }
}


void MQWorkArea::load_query(const std::string &file)
{
  Glib::ustring data;

  try
  {
    data= Glib::file_get_contents(file);
  }
  catch (Glib::FileError &err)
  {
    myg_show_error(ufmt(_("Could not open file '%s'"), err.what().c_str()));
    return;
  }

  if (get_tab()->get_type() != TResultSet)
  {
    MQResultSetView *rsview= add_result_view("");
    rsview->set_active(true);
  }
    
  _query_buffer->set_text(data);

  _mainw->set_status(ufmt(_("Query loaded from file '%s'."), file.c_str()));
  
  _mainw->_state->add_recent_file("query://"+file);
  refresh_recent_menu();
}


void MQWorkArea::reload_mi()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    if (rset->get_resultset())
    {
      Glib::ustring query= rset->get_shown_query();
      _query_buffer->set_text(query);

      rset->execute_query(query, true);
    }
  }
}


void MQWorkArea::quit_mi()
{
  _mainw->_state->global_params= _global_params->get_as_list();
  _mainw->_state->save();

  _mainw->quit();
}


void MQWorkArea::toggle_view_type(const char *button)
{
  int new_type;

  if (button && !static_cast<Gtk::CheckMenuItem*>(_xml->get_widget(button))->get_active())
    return;

  if (static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("expand_query_area1"))->get_active())
    new_type= 'Q';
  else if (static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("resultset_only1"))->get_active())
    new_type= 'R';
  else
    new_type= 'N';


  if (new_type == 'Q' && prefs.view_type == 'Q')
  {
    static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("normal_view1"))->set_active(true);

    return;
  }

  if (new_type == prefs.view_type)
    return;
  
  // Undo Old Type
  switch (prefs.view_type)
  {    
  case 'Q':
    {
      switch (get_tab()->get_type())
      {
      case TResultSet:
        _query_toolbar->hide();
        _query_toolbar->set_sensitive(false);
        break;
      case TScriptEditor:
        _script_toolbar->hide();
        _script_toolbar->set_sensitive(false);
        break;
      case THelp:
        break;
      }
    }
    break;

  case 'R':
    _xml->get_widget("side_vpane")->show();
    break;

  default:
  case 'N':
    switch (get_tab()->get_type())
    {
    case TResultSet:
      _query_toolbar->hide();
      _query_toolbar->set_sensitive(false);
      break;
    case TScriptEditor:
      _script_toolbar->hide();
      _script_toolbar->set_sensitive(false);
      break;
    case THelp:
      break;
    }
    break;
  }

  prefs.view_type= new_type;

  // New Type
  switch (prefs.view_type)
  {
  case 'Q':
    {
      _query_toolbar= _xml->get_widget("small_query_toolbar");
      _script_toolbar= _xml->get_widget("small_script_toolbar");

      switch (get_tab()->get_type())
      {
      case TResultSet:
        {
          Gtk::Widget *qarea= _xml->get_widget("query_area_scroll");
          _query_toolbar->show();
          _query_toolbar->set_sensitive(true);
          static_cast<MQResultTab*>(get_tab())->set_top_widget(qarea);
          // these two lines fix bug #12366
          set_query_bar_sensitivity(false);
          set_query_bar_sensitivity(true);
        }
        break;
      case TScriptEditor:
        _script_toolbar->show();
        _script_toolbar->set_sensitive(true);
        break;
      case THelp:
        break;
      }
    }
    break;

  case 'R':
    {
      _xml->get_widget("side_vpane")->hide();

      _script_toolbar->hide();
      _query_toolbar->hide();
    }
    break;

  case 'N':
    _query_toolbar= _xml->get_widget("query_toolbar");
    _script_toolbar= _xml->get_widget("script_toolbar");
    switch (get_tab()->get_type())
    {
    case TResultSet:
      _xml->get_widget("query_area_scroll")->reparent(*_xml->get_container("query_area_vp"));

      _query_toolbar->set_sensitive(true);
      _query_toolbar->show();
      break;
    case TScriptEditor:
      _script_toolbar->set_sensitive(true);
      _script_toolbar->show();
      break;
    case THelp:
      break;
    }
    break;
  }
}


void MQWorkArea::toggle_sidebar_mi()
{
  bool show= static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("sidebar1"))->get_active();
  
  toggle_sidebar(show);
}


void MQWorkArea::toggle_sidebar(bool show)
{
  Gtk::Paned *pane= _xml->get_paned("hpaned1");

  prefs.show_sidebar= show;

  if (show)
    pane->set_position(_mainw->get_width()-200);
  else
    pane->set_position(_mainw->get_width());
}


void MQWorkArea::setup_button_images()
{
  MGImage *image= 0;
  static struct {
    Gtk::StateType state;
    char *prefix;
  } suffixes[]={
    {Gtk::STATE_NORMAL, ""},
    {Gtk::STATE_ACTIVE, "_down"},
    {Gtk::STATE_PRELIGHT, "_highlight"},
    {Gtk::STATE_SELECTED, NULL},
    {Gtk::STATE_INSENSITIVE, "_disabled"}
  };
  static struct {
    char *widget;
    char *image;
  } buttons[]= {
    {"goback", "query_goback"},
    {"gonext", "query_next"},
    {"execute", "query_execute"},
    {"stop", "query_stop"},
    {"tstart", "trans_start"},
    {"tcommit", "trans_commit"},
    {"trollback", "trans_rollback"},
    {"lock", "lock_enable"},
    {"unlock", "lock_disable"},
    {"explain", "special_explain"},
    {"compare", "special_compare"},
    {NULL, NULL}
  };

  for (unsigned int j= 0; buttons[j].widget; j++)
  {
    image= 0;
    _xml->get_widget_derived(std::string(buttons[j].widget)+"_image", image);
    for (int i= 0; i < 5; i++)
    {
      if (suffixes[i].prefix)
        image->set_image(suffixes[i].state,
                         PIXCACHE->load(std::string(buttons[j].image)+std::string(suffixes[i].prefix)+".png"));
    }
  }
}


void MQWorkArea::setup()
{
  bind_main_menu_items();
  
//  setup_button_images();
  
  _idle_image= PIXCACHE->load("sakila.png");
  _busy_anim= PIXCACHE->load_anim("dolphin_anim.gif");

  set_busy_animation(false);
  set_script_busy_animation(false);

  _note= (Gtk::Notebook*)_xml->get_widget("result_note");
  _note->signal_switch_page().connect(sigc::mem_fun(*this,&MQWorkArea::tab_changed));
  _note->set_show_tabs(false);
  _note->set_show_border(false);

  // set up sidebar
  Gtk::Notebook *note= static_cast<Gtk::Notebook*>(_xml->get_widget("sidetop_notebook"));

  // table list
  _table_browser= new MGTableBrowserList("", MGTableBrowserList::Column);

  _table_browser->set_show_sps(true);

  _table_browser->set_border_width(6);
//  _table_browser->set_allow_multiple_selection(true);
  _table_browser->show();
  note->append_page(*_table_browser, _("Schemata"));

  _table_browser->set_fetch_schema_tables_func(sigc::mem_fun(*this,&MQBaseModule::schemata_fetch_tables));
  _table_browser->set_fetch_schema_sps_func(sigc::mem_fun(*this,&MQBaseModule::schemata_fetch_sps));
  _table_browser->signal_row_activate().connect(sigc::mem_fun(*this,&MQWorkArea::schemata_dbl_clicked));
  _table_browser->signal_selected().connect(sigc::mem_fun(*this,&MQWorkArea::schemata_selected));

  _table_helper= new MGSchemaBrowserHelper(_table_browser);

  _table_helper->signal_changed().connect(sigc::mem_fun(*this,&MQBaseModule::refresh_catalogs));

  _table_browser_menu.signal_show().connect(sigc::mem_fun(*this,&MQWorkArea::update_browser_menu));
  
  // make schema browser menu
  myg_menu_add(_table_browser_menu, _("Set as Default Schema"),
               sigc::mem_fun(*this,&MQWorkArea::select_schema_from_browser),
               "set_default");

  myg_menu_add(_table_browser_menu);

  myg_menu_add(_table_browser_menu, _("Copy SQL to Clipboard"),
               sigc::mem_fun(*this,&MQWorkArea::copy_sql),
               "copy_sql");

  myg_menu_add(_table_browser_menu, _("_Edit..."),
               sigc::mem_fun(*this,&MQWorkArea::edit_selection),
               "edit");
  myg_menu_add(_table_browser_menu, _("_Drop..."),
               sigc::mem_fun(*_table_helper,&MGSchemaBrowserHelper::drop_object),
               "drop");

  myg_menu_add(_table_browser_menu);

  myg_menu_add(_table_browser_menu, _("Create _Schema..."),
               sigc::mem_fun(*_table_helper,&MGSchemaBrowserHelper::create_schema),
               "create_schema");
  myg_menu_add(_table_browser_menu, _("Create _Table..."),
               sigc::mem_fun(*_table_helper,&MGSchemaBrowserHelper::create_table),
               "create_table");
  myg_menu_add(_table_browser_menu, _("Create _View..."),
               sigc::mem_fun(*this,&MQWorkArea::create_view),
               "create_view");
  myg_menu_add(_table_browser_menu, _("Create Stored _Routine..."),
               sigc::mem_fun(*this,&MQWorkArea::create_stored_procedure),
               "create_sp");

  myg_menu_add(_table_browser_menu);

  myg_menu_add(_table_browser_menu, Gtk::Stock::REFRESH, _("Refresh Schemata"),
               sigc::mem_fun(*this,&MQBaseModule::refresh_catalogs),
               "refresh");

  _table_browser->set_popup_menu(&_table_browser_menu);

  // bookmarks
  MQBookmarks *bmks=  _mainw->get_bookmarks();
  bmks->get_browser()->set_border_width(6);
  bmks->get_browser()->show();
  bmks->get_browser()->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::open_bookmark));
  note->append_page(*bmks->get_browser(), _("Bookmarks"));

  // history
  _history_browser= new MQHistoryBrowser(_history);
  _history_browser->set_border_width(6);
  _history_browser->show();
  _history_browser->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::open_history));
  note->append_page(*_history_browser, _("History"));

  _history->signal_changed().connect(sigc::mem_fun(*_history_browser,&MQHistoryBrowser::refresh));

  _history_browser->refresh();

  note= static_cast<Gtk::Notebook*>(_xml->get_widget("sidebottom_notebook"));

  // statement list
  _statement_browser= new MQFunctionBrowser(_statement_index, "syntax_14x14.png");
  _statement_browser->set_border_width(6);
  _statement_browser->show();
  _statement_browser->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::show_syntax_help));
  _statement_browser->refresh();
  note->insert_page(*_statement_browser, _("Syntax"), 0);

  // function list
  _function_browser= new MQFunctionBrowser(_function_index, "function_14x14.png");
  _function_browser->set_border_width(6);
  _function_browser->show();
  _function_browser->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::show_function_help));
  _function_browser->refresh();
  note->insert_page(*_function_browser, _("Functions"), 1);

  note->set_current_page(0);

  // parameter list
  {
    _param_store= Gtk::TreeStore::create(_pcolumns);
    Gtk::TreeView *tree= _xml->get_tree("param_tree");
    Gtk::TreeViewColumn *column= new Gtk::TreeView::Column("");
    column->pack_start(_pcolumns.icon, false);
    column->pack_start(_pcolumns.text);
    tree->append_column(*Gtk::manage(column));
    tree->append_column_editable("", _pcolumns.value);
    column= tree->get_column(1);
    column->add_attribute(((Gtk::CellRendererText*)column->get_first_cell_renderer())->property_editable(), _pcolumns.editable);
    ((Gtk::CellRendererText*)column->get_first_cell_renderer())->signal_edited().connect(sigc::mem_fun(*this,&MQWorkArea::param_edited));

    tree->set_model(_param_store);

    tree->signal_button_press_event().connect_notify(sigc::mem_fun(*this,&MQWorkArea::popup_param_menu));

    myg_menu_add(_param_menu, _("Add Parameter"),
                 sigc::mem_fun(*this,&MQWorkArea::param_add_mi),
                 "add");
    myg_menu_add(_param_menu, _("Delete Parameter"),
                 sigc::mem_fun(*this,&MQWorkArea::param_delete_mi),
                 "delete");
  }

  // transaction list
  {
    _trans_tree= _xml->get_tree("trx_tree");
    
    _trans_log= Gtk::ListStore::create(_tcolumns);
    _trans_tree->set_model(_trans_log);

    _trans_tree->set_headers_visible(false);
    _trans_tree->append_column("", _tcolumns.icon);
    _trans_tree->append_column("", _tcolumns.text);

    Gtk::TreeIter iter= _trans_log->append();
    Gtk::TreeRow row= *iter;
    row[_tcolumns.text]= _("No Active Transaction.");
  }

  // query execute button menu
  myg_menu_add(_execute_menu, _("Execute"),
               sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::execute_clicked),false),
               "execute");
  myg_menu_add(_execute_menu, _("New Tab and Execute"),
               sigc::mem_fun(*this,&MQWorkArea::execute_in_new_tab),
               "new_tab");
  myg_menu_add(_execute_menu, _("Split Tab and Execute"),
               sigc::mem_fun(*this,&MQWorkArea::execute_in_split_tab),
               "split");

  // script execute button menu
  myg_menu_add(_execute_script_menu, _("Execute"),
               sigc::mem_fun(*this,&MQWorkArea::execute_script),
               "execute");
  myg_menu_add(_execute_script_menu, _("Execute Selected"),
               sigc::mem_fun(*this,&MQWorkArea::execute_script_selection),
               "execute_selected");
  myg_menu_add(_execute_script_menu, _("Execute Stepping"),
               sigc::mem_fun(*this,&MQWorkArea::execute_script_stepping),
               "execute_stepping");

  
  // query toolbar
  _xml->get_button("tstart_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::trans_start_clicked));
  _xml->get_button("tcommit_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::trans_commit_clicked));
  _xml->get_button("trollback_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::trans_rollback_clicked));

  _xml->get_button("explain_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::explain_result));
  _xml->get_button("compare_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::compare_results));
  
  _xml->get_button("back_btn")->signal_pressed().connect(sigc::mem_fun(*this,&MQWorkArea::back_button_clicked));
  _xml->get_button("goback_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::goback_clicked));
  _xml->get_button("back_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::goback_clicked));
 
  _xml->get_button("next_btn")->signal_pressed().connect(sigc::mem_fun(*this,&MQWorkArea::next_button_clicked));
  _xml->get_button("gonext_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::gonext_clicked));
  _xml->get_button("next_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::gonext_clicked));

  _xml->get_button("execute_btn")->signal_pressed().connect(sigc::mem_fun(*this,&MQWorkArea::execute_menu_clicked));
  _xml->get_button("doexecute_btn")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::execute_clicked),false));
  _xml->get_button("execute_sbtn")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::execute_clicked),false));

  _xml->get_button("stop_btn")->set_sensitive(false);
  _xml->get_button("stop_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::stop_clicked));
  _xml->get_button("stop_sbtn")->set_sensitive(false);
  _xml->get_button("stop_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::stop_clicked));

  // script toolbar
  /*XXX
  _xml->get_button("undo_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  _xml->get_button("redo_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  _xml->get_button("undo_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  _xml->get_button("redo_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
   */
  _xml->get_button("undo_btn")->set_sensitive(false);
  _xml->get_button("redo_btn")->set_sensitive(false);
  _xml->get_button("undo_sbtn")->set_sensitive(false);
  _xml->get_button("redo_sbtn")->set_sensitive(false);

  _xml->get_button("open_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::load_script_into));
  _xml->get_button("open_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::load_script_into));
  _xml->get_button("save_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::save_file));
  _xml->get_button("save_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::save_file));

  _xml->get_button("sdoexecute_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  _xml->get_button("sexecute_btn")->signal_pressed().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script_menu_clicked));
  _xml->get_button("sexecute_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::execute_script));
  
  _xml->get_button("sstop_btn")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::stop_script),false));
  _xml->get_button("sstop_sbtn")->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this,&MQWorkArea::stop_script),false));

  _xml->get_button("continue_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::continue_script));
  _xml->get_button("continue_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::continue_script));

  _xml->get_button("step_btn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::step_over));
  _xml->get_button("step_sbtn")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::step_over));

  
  
  _xml->get_menu_item("open_recent1")->set_submenu(_recent_files_menu);
  refresh_recent_menu();
  
  // toolbars
  if (prefs.view_type == 'Q')
  {
    _query_toolbar= _xml->get_widget("small_query_toolbar");
    _script_toolbar= _xml->get_widget("small_script_toolbar");
  }
  else
  {
    _query_toolbar= _xml->get_widget("query_toolbar");
    _script_toolbar= _xml->get_widget("script_toolbar");
  }

  _xml->get_widget("query_toolbar")->hide();
  _xml->get_widget("script_toolbar")->hide();
  _xml->get_widget("small_query_toolbar")->hide();
  _xml->get_widget("small_script_toolbar")->hide();


  // load the saved global params
  _global_params->set_from_list(_mainw->_state->global_params);
  
  // calc the height of a line in the query text area
  {
    int dummy;
    _xml->get_text("query_text")->create_pango_layout("AIjl123")->get_pixel_size(dummy, _query_line_height);
  }
  // enable dnd
  std::list<Gtk::TargetEntry> targets;
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", Gtk::TargetFlags(0), 0));
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-schema-object", Gtk::TargetFlags(0), 0));

  _xml->get_widget("query_text")->signal_drag_motion().connect(sigc::mem_fun(*this,&MQWorkArea::query_drag_motion), false);
  _xml->get_widget("query_text")->signal_drag_leave().connect(sigc::mem_fun(*this,&MQWorkArea::query_drag_leave), false);
  
  _xml->get_text("query_text")->drag_dest_set(targets);
  _xml->get_text("query_text")->signal_drag_data_received().connect(sigc::mem_fun(*this, &MQWorkArea::query_drag_data_received));

  _xml->get_text("query_text")->signal_key_press_event().connect(sigc::mem_fun(*this,&MQWorkArea::query_text_key_press),false);

  _xml->get_text("query_text")->grab_focus();

  // setup drag and drop on clause menu
  _table_add_xml= new MGGladeXML(get_app_file("workarea.glade"), "clause_menu");
  _column_add_xml= new MGGladeXML(get_app_file("workarea.glade"), "column_menu");

  targets.clear();
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-schema-object", Gtk::TargetFlags(0), 0));

  {
    static struct {
      char *name;
      MYX_Q_TABLE_ADD_TYPE type;
    } items[]= {
      {"select_l", MYX_QTAT_SELECT},
      {"add_l", MYX_QTAT_SELECT_ADD}, 
      {"join_l", MYX_QTAT_SELECT_JOIN},
      {"ljoin_l", MYX_QTAT_SELECT_LEFT_OUTER_JOIN},
      {"update_l", MYX_QTAT_UPDATE},
      {"insert_l", MYX_QTAT_INSERT},
      {"delete_l", MYX_QTAT_DELETE}
    };

    _table_add_xml->get_widget("clause_menu")->modify_bg(Gtk::STATE_NORMAL, get_style()->get_white());

    for (unsigned int i= 0; i < sizeof(items)/sizeof(*items); i++)
    {
      Gtk::Widget *w= _table_add_xml->get_widget(items[i].name);
      TableAddDropProxyHack *proxy= new TableAddDropProxyHack(items[i].type);
      w->drag_dest_set(targets);
      w->set_data("bla", proxy, TableAddDropProxyHack::destroy);
      w->signal_drag_data_received().connect(sigc::mem_fun(*proxy,&TableAddDropProxyHack::drag_data_received));
      
      proxy->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::table_add_dropped));
      
      w->signal_drag_motion().connect(sigc::mem_fun(*this,&MQWorkArea::object_add_menu_drag_motion), false);
      w->signal_drag_leave().connect(sigc::mem_fun(*this,&MQWorkArea::object_add_menu_drag_leave), false);
    }
  }

  {
    static struct {
      char *name;
      MYX_Q_CLAUSE_TYPE type;
    } items[]= {
      {"select_c", MYX_QCT_SELECT_CLAUSE},
      {"from_c", MYX_QCT_FROM_CLAUSE}, 
      {"where_c", MYX_QCT_WHERE_CLAUSE},
      {"group_c", MYX_QCT_GROUP_CLAUSE},
      {"having_c", MYX_QCT_HAVING_CLAUSE},
      {"order_c", MYX_QCT_ORDER_CLAUSE},
      {"limit_c", MYX_QCT_LIMIT_CLAUSE},
      {"set_c", MYX_QCT_SET_CLAUSE},
      {"into_c", MYX_QCT_INTO_CLAUSE},
      {"update_c", MYX_QCT_UPDATE_CLAUSE},
      {"delete_c", MYX_QCT_DELETE_CLAUSE},
      {"using_c", MYX_QCT_USING_CLAUSE}
    };

    _column_add_xml->get_widget("column_menu")->modify_bg(Gtk::STATE_NORMAL, get_style()->get_white());

    for (unsigned int i= 0; i < sizeof(items)/sizeof(*items); i++)
    {
      Gtk::Widget *w= _column_add_xml->get_widget(items[i].name);
      ColumnAddDropProxyHack *proxy= new ColumnAddDropProxyHack(items[i].type);
      w->drag_dest_set(targets);
      w->set_data("bla", proxy, ColumnAddDropProxyHack::destroy);
      w->signal_drag_data_received().connect(sigc::mem_fun(*proxy,&ColumnAddDropProxyHack::drag_data_received));
      
      proxy->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::column_add_dropped));
      
      w->signal_drag_motion().connect(sigc::mem_fun(*this,&MQWorkArea::object_add_menu_drag_motion), false);
      w->signal_drag_leave().connect(sigc::mem_fun(*this,&MQWorkArea::object_add_menu_drag_leave), false);
    }
  }
  
  update_edit_menu();
}


void MQWorkArea::set_busy_animation(bool flag)
{
  if (flag)
    static_cast<Gtk::Image*>(_xml->get_widget("image"))->set(_busy_anim);
  else
    static_cast<Gtk::Image*>(_xml->get_widget("image"))->set(_idle_image);
}


void MQWorkArea::set_script_busy_animation(bool flag)
{
  if (flag)
    static_cast<Gtk::Image*>(_xml->get_widget("simage"))->set(_busy_anim);
  else
    static_cast<Gtk::Image*>(_xml->get_widget("simage"))->set(_idle_image);
}


MYX_Q_CLAUSE_TYPE MQWorkArea::get_current_clause_type()
{
  static const struct { char *kw; MYX_Q_CLAUSE_TYPE cl; } keywords[]= {
    {"from", MYX_QCT_FROM_CLAUSE},
    {"group", MYX_QCT_GROUP_CLAUSE},
    {"having", MYX_QCT_HAVING_CLAUSE},
    {"order", MYX_QCT_ORDER_CLAUSE},
    {"select", MYX_QCT_SELECT_CLAUSE},
    {"set", MYX_QCT_SET_CLAUSE},
    {"where", MYX_QCT_WHERE_CLAUSE},
    {NULL, MYX_QCT_NO_CLAUSE},
  };
  
  for (unsigned int i= 0; keywords[i].kw; i++)
  {
    if (_xml->get_toggle(std::string(keywords[i].kw)+"_button")->get_active())
      return keywords[i].cl;
  }
  return MYX_QCT_NO_CLAUSE;
}


MQWorkArea *MQWorkArea::create(MGGladeXML *xml, MQMainWindowInterface *mainw)
{
  MQWorkArea *me= 0;
  xml->get_widget_derived("work_area", me);

  me->_xml= xml;
  me->_mainw= mainw;
   
  return me;
}

void MQWorkArea::add_tab(MQBaseTab *tab)
{
  MQBaseModule::add_tab(tab);

  tab->set_tab_action_handler(sigc::mem_fun(*this,&MQWorkArea::do_tab_action));
}


void MQWorkArea::set_query_bar_sensitivity(bool flag)
{
  _xml->get_widget("goback_btn")->set_sensitive(flag);
  _xml->get_widget("gonext_btn")->set_sensitive(flag);
  _xml->get_widget("query_text")->set_sensitive(flag);
  _xml->get_widget("execute_btn")->set_sensitive(flag);
  _xml->get_widget("doexecute_btn")->set_sensitive(flag);
  _xml->get_widget("stop_btn")->set_sensitive(!flag);
  
  _xml->get_widget("execute1")->set_sensitive(flag);
  _xml->get_widget("stop1")->set_sensitive(!flag);
  
  _xml->get_widget("execute_sbtn")->set_sensitive(flag);
  _xml->get_widget("stop_sbtn")->set_sensitive(!flag);
}


void MQWorkArea::update_toolbar_sensitivity()
{
  if (_dispatcher->is_transaction_started())
  {
    _xml->get_widget("tstart_btn")->set_sensitive(false);
    _xml->get_widget("tcommit_btn")->set_sensitive(true);
    _xml->get_widget("trollback_btn")->set_sensitive(true);
    
    _xml->get_widget("start_transaction")->set_sensitive(false);
    _xml->get_widget("commit_transaction")->set_sensitive(true);
    _xml->get_widget("rollback_transaction")->set_sensitive(true);
  }
  else
  {
    _xml->get_widget("tstart_btn")->set_sensitive(_dispatcher->can_start_transaction());
    _xml->get_widget("tcommit_btn")->set_sensitive(false);
    _xml->get_widget("trollback_btn")->set_sensitive(false);
    
    _xml->get_widget("start_transaction")->set_sensitive(_dispatcher->can_start_transaction());
    _xml->get_widget("commit_transaction")->set_sensitive(false);
    _xml->get_widget("rollback_transaction")->set_sensitive(false);
  }
}


void MQWorkArea::init_tag_table()
{
  Glib::RefPtr<Gtk::TextTag> tag;
  
  _tag_table= Gtk::TextTagTable::create();
  
  tag= Gtk::TextTag::create("normal");
  tag->property_foreground()= "#000000";
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("table");
  tag->property_foreground()= "#aa0000";
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("column");
  tag->property_foreground()= "#999900";
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("comment");
  tag->property_foreground()= "#aaaaaa";
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("string");
  tag->property_foreground()= "#338833";
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("symbol");
  tag->property_foreground()= "#4455ff";
  tag->property_weight()= Pango::WEIGHT_BOLD;
  _tag_table->add(tag);
  
  tag= Gtk::TextTag::create("function");
  tag->property_foreground()= "#6666aa";
  tag->property_weight()= Pango::WEIGHT_BOLD;
  _tag_table->add(tag);
}


void MQWorkArea::tab_changed(GtkNotebookPage* page, guint page_num)
{
  switch (get_tab()->get_type())
  {
  case TResultSet:
    {
      MQResultTab *tab= static_cast<MQResultTab*>(get_tab());

      _query_toolbar->set_sensitive(true);
      _query_toolbar->show();
      _script_toolbar->set_sensitive(false);
      _script_toolbar->hide();

      Gtk::Widget *qarea= _xml->get_widget("query_area_scroll");
      if (prefs.view_type == 'Q')
        tab->set_top_widget(qarea);
      else
        qarea->reparent(*_xml->get_container("query_area_vp"));

      _query_buffer->set_text(tab->get_query_text(tab->get_active()));
      _query_buffer->do_hiliting();

      update_history_menu(tab->get_active());
      update_parameter_list(tab->get_active()->get_parameters());

      update_toolbar_sensitivity();
  
      set_query_bar_sensitivity(!tab->get_active()->is_busy());
      
      set_script_busy_animation(false);
      if (tab->is_busy())
        set_busy_animation(true);
      else
        set_busy_animation(false);
      
      set_menu_items_sensitive(_xml->get_menu_item("query_menuitem")->get_submenu(), true, query_menu_exceptions);
      set_menu_items_sensitive(_xml->get_menu_item("script_menuitem")->get_submenu(), false, NULL);
      _xml->get_widget("create_sp")->set_sensitive(true);
      _xml->get_widget("edit_all_sps")->set_sensitive(true);
      
      _mainw->set_cursor();

      _xml->get_widget("query_text")->grab_focus();
    }
    break;
  case TScriptEditor:
    {
      MQScriptEditorTab *tab= (MQScriptEditorTab*)get_tab();
      
      _query_toolbar->set_sensitive(false);
      _query_toolbar->hide();
      _script_toolbar->set_sensitive(true);
      _script_toolbar->show();

      _query_buffer->set_text("");

      update_parameter_list(0);

      set_busy_animation(false);
      if (tab->is_busy())
        set_script_busy_animation(true);
      else
        set_script_busy_animation(false);

      script_editor_state_changed(tab, tab->get_state());
      
      set_menu_items_sensitive(_xml->get_menu_item("query_menuitem")->get_submenu(), false, NULL);
      set_menu_items_sensitive(_xml->get_menu_item("script_menuitem")->get_submenu(), true, NULL);
      
      script_edited(tab->get_editor()->get_dirty());
      
      tab->get_editor()->get_wrapper()->grab_focus();
    }
    break;
  case THelp:
    {
      _query_toolbar->set_sensitive(false);
      _query_toolbar->show();
      _script_toolbar->hide();
      
      update_parameter_list(0);
      
      set_busy_animation(false);
      
      set_menu_items_sensitive(_xml->get_menu_item("query_menuitem")->get_submenu(), false, NULL);
      set_menu_items_sensitive(_xml->get_menu_item("script_menuitem")->get_submenu(), false, NULL);

      _mainw->set_cursor();
    }
    break;
    
  default:
    g_assert(0);
  }
  

  if (!_dispatcher->check_version(5,0,1))
  {
    _xml->get_menu_item("create_sp")->set_sensitive(false);
    _xml->get_menu_item("edit_all_sps")->set_sensitive(false);
  }
}


Glib::ustring MQWorkArea::shorten_query(const Glib::ustring &query)
{
  Glib::ustring tmp= strreplace(strreplace(query, "\n", " "), "  ", " ");
  
  if (tmp.size()>32)
    return tmp.substr(0, 32)+"...";
  else
    return tmp;
}


void MQWorkArea::do_tab_action(MQBaseTab *wtab, /*MQBaseTab::TabActionType*/int action)
{
  MQResultTab *tab;

  if (!wtab)
    wtab= get_tab();
  
  tab= wtab->get_type()==TResultSet ? dynamic_cast<MQResultTab*>(wtab) : 0;
  
  switch (action)
  {
  case MQBaseTab::ANewQuery:
    add_result_view("");
    break;
  case MQBaseTab::ANewScript:
    add_script_view(_("New Script"));
    break;
  case MQBaseTab::ACloseOther:
    close_other_tabs(wtab);
    if (wtab != _help_tab)
      _help_tab= 0;
    break;
  case MQBaseTab::AClose:
    if (close_tab(wtab))
    {
      if (wtab == _help_tab)
        _help_tab= 0;
    }
    break;
  case MQBaseTab::ASplit:
    // split tab
    if (tab)
    {
      MQResultSetView *current= tab->get_active();
      MQResultSetView *newrs= add_result_view("", tab, false);
      // restore query text for the other half
      tab->set_query_text(current, current->get_shown_query());
      newrs->set_active(true);
    }
    break;
  case MQBaseTab::ASplitV:
    // split tab
    if (tab)
    {
      MQResultSetView *current= tab->get_active();
      MQResultSetView *newrs= add_result_view("", tab, true);
      // restore query text for the other half
      tab->set_query_text(current, current->get_shown_query());
      newrs->set_active(true);
    }
    break;
  case MQBaseTab::AUnsplit:
    g_assert(0); // handled in do_tab_action2
    break;
  }
}


void MQWorkArea::do_tab_action2(MQBaseTab::TabActionType action,
                                MQResultSetView *rset)
{
  MQBaseTab *wtab= get_tab();
  MQResultTab *tab= wtab->get_type()==TResultSet ? dynamic_cast<MQResultTab*>(wtab) : 0;

  g_assert(action == MQBaseTab::AUnsplit);
  
  if (tab && tab->get_item_count()>1)
  {
    if (rset->is_busy())
      rset->cancel_query();

    tab->remove_resultset(rset);

    // focus remaining resultset, so that the right query appears in the
    // text box
    tab->get(0)->set_active(true);
    tab->get(0)->set_compact_editbar(false);
  }
}


Gtk::Widget *MQWorkArea::get_widget()
{
  return _xml->get_widget("work_area");
}


void MQWorkArea::show()
{
  get_widget()->show();

  if (_first_time_show)
  {
    _first_time_show= false;
    _xml->get_paned("side_vpane")->set_position(280);

    switch (prefs.view_type)
    {
    case 'N':
      prefs.view_type= 0;
      static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("normal_view1"))->set_active(true);
      break;
    case 'R':
      prefs.view_type= 0;
      static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("resultset_only1"))->set_active(true);
      break;
    case 'Q':
      prefs.view_type= 0;
      static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("expand_query_area1"))->set_active(true);
      break;
    }
//    toggle_view_type();

    static_cast<Gtk::CheckMenuItem*>(_xml->get_widget("sidebar1"))->set_active(prefs.show_sidebar);
    toggle_sidebar_mi();
  }
}


void MQWorkArea::hide()
{
  get_widget()->hide();
}

void MQWorkArea::recent_file_open(const char *file)
{
  char *tmp= g_strdup(file);
  char *type;
  char *path;
  
  type= strtok(tmp, "://");
  path= strtok(NULL, "")+2;

  if (strcmp(type, "script")==0)
  {
    load_script(path);
  }
  else if (strcmp(type, "query")==0)
  {
    load_query(path);
  }
  
  g_free(tmp);
}


void MQWorkArea::refresh_recent_menu()
{
  _recent_files_menu.items().clear();
  
  for (std::list<std::string>::const_iterator iter= _mainw->_state->recent_files.begin();
       iter != _mainw->_state->recent_files.end(); ++iter)
  {
    Glib::ustring name;
    Gtk::MenuItem *item;

    if (iter->find('/')==std::string::npos)
      name= *iter;
    else
      name= iter->substr(iter->rfind('/')+1);
    
    if (strncmp(iter->c_str(),"script:",strlen("script:"))==0)
      name+=" (script)";
    else if (strncmp(iter->c_str(),"query:",strlen("query:"))==0)
      name+=" (query)";
    item= Gtk::manage(new Gtk::MenuItem(name));
    item->signal_activate().connect(sigc::bind<const char *>(sigc::mem_fun(*this,&MQWorkArea::recent_file_open), iter->c_str()));
    _recent_files_menu.append(*item);
    item->show();
  }
}


//======================================================================
// Sidebar
//======================================================================


void MQWorkArea::update_browser_menu()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  bool is_mysql501= _dispatcher->check_version(5,0,1);
  bool table_selected= false;
  bool view_selected= false;
  bool sp_selected= false;
  bool schema_selected= false;
  bool row_selected= false;

  // which entity selected?
  if(iter)
  {
    MGTableBrowserList::RowType row_type= _table_browser->get_type(iter);
    switch(row_type)
    {
    case MGTableBrowserList::Table:
      table_selected= !(view_selected= _table_browser->is_view(iter));
      break;
    
    case MGTableBrowserList::SP:
      sp_selected= true;
      break;
    
    case MGTableBrowserList::Schema:
      schema_selected= true;
      break;
    
    default:
      row_selected= true;
    }
  }

  myg_menu_set_sensitive(_table_browser_menu, "set_default", schema_selected);
  myg_menu_set_sensitive(_table_browser_menu, "create_sp", is_mysql501);
  myg_menu_set_sensitive(_table_browser_menu, "create_view", is_mysql501 && table_selected);
  myg_menu_set_sensitive(_table_browser_menu, "drop", schema_selected || view_selected || table_selected || sp_selected);
  myg_menu_set_sensitive(_table_browser_menu, "edit", view_selected || table_selected || sp_selected);
  myg_menu_set_sensitive(_table_browser_menu, "copy_sql", view_selected || table_selected || sp_selected);

  /*

  // old implementation

  bool flag= false;

  if (!iter)
    flag= false;
  else
    flag= true;
  
  myg_menu_set_sensitive(_table_browser_menu, "set_default", flag);
  myg_menu_set_sensitive(_table_browser_menu, "create_table", flag);
  myg_menu_set_sensitive(_table_browser_menu, "create_sp", flag);

  flag= false;
  
  if (iter && _table_browser->get_type(iter)==MGTableBrowserList::Table
      && !_table_browser->is_view(iter))
    flag= true;

  if (_dispatcher->check_version(5,0,1))
    myg_menu_set_sensitive(_table_browser_menu, "create_view", flag);
  else
  {
    myg_menu_set_sensitive(_table_browser_menu, "create_view", false);
    myg_menu_set_sensitive(_table_browser_menu, "create_sp", false);
  }

  flag= false;
  if (iter && (_table_browser->get_type(iter)==MGTableBrowserList::Table
               || _table_browser->get_type(iter)==MGTableBrowserList::SP))
    flag= true;
  myg_menu_set_sensitive(_table_browser_menu, "edit", flag);
  myg_menu_set_sensitive(_table_browser_menu, "copy_sql", flag);

  flag= false;
  if (iter && (_table_browser->get_type(iter)==MGTableBrowserList::Table
               || _table_browser->get_type(iter)==MGTableBrowserList::SP
               || _table_browser->get_type(iter)==MGTableBrowserList::Schema))
    flag= true;
  myg_menu_set_sensitive(_table_browser_menu, "drop", flag);
  */
}


void MQWorkArea::edit_selection()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (iter) 
  {
    switch (_table_browser->get_type(iter))
    {
    case MGTableBrowserList::SP:
      edit_stored_procedure();
      break;
    case MGTableBrowserList::Table:
      {
        MYX_SCHEMA_TABLE *table;
        Glib::ustring name;
        
        table= _table_browser->get_table(iter, name);
        if (table)
        {
          if (table->table_type == MSTT_VIEW)
          {
            edit_view();
            break;
          }
        }
      }
    default:
      _table_helper->edit_object();
      break;
    }
  }
}


void MQWorkArea::edit_view()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (iter)
  {
    Glib::ustring name;
    MYX_DBM_VIEW_DATA *vdata;
    
    name= _table_browser->get_table(iter);
    
    vdata= _dispatcher->get_view_data(_table_browser->get_catalog(iter),
                                      _table_browser->get_schema(iter),
                                      name);
    if (vdata)
    {
      MQScriptEditorTab *tab= add_script_view(name);
      if (tab)
      {
        tab->set_text(vdata->definition);
      }
      g_free(vdata);
    }
  }
}


void MQWorkArea::edit_stored_procedure()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (iter)
  {
    Glib::ustring sp;
    bool is_function;
    MYX_DBM_STORED_PROCEDURE_DATA *spdata;

    sp= _table_browser->get_procedure(iter, is_function);
    spdata= _dispatcher->get_sp_data(_table_browser->get_catalog(iter),
                                     _table_browser->get_schema(iter),
                                     sp, is_function);
    if (spdata)
    {
      MQScriptEditorTab *tab= add_script_view(sp);
      if (tab)
      {
        Glib::ustring tmpl;
        int quote= myx_get_mysql_quote_char(_dispatcher->get_mysql(), 1);

        tmpl= ufmt("DELIMITER $$\n\n"
                   "DROP %s IF EXISTS %c%s%c.%c%s%c$$\n"
                   "%s$$\n\n"
                   "DELIMITER ;\n",
                   is_function ? "FUNCTION":"PROCEDURE",
                   quote, _table_browser->get_schema(iter).c_str(), quote, 
                   quote, sp.c_str(), quote,
                   spdata->definition);
        tab->set_text(tmpl);
      }

      myx_dbm_free_sp_data(spdata);
    }
    else
      _mainw->set_status(ufmt(_("Can't retrieve definition for '%s'"),sp.c_str()));
  }
}


void MQWorkArea::create_view_with_query()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    if (rset->get_resultset())
    {
      Glib::ustring query= rset->get_shown_query();

      if (query.empty())
      {
        _mainw->set_status(_("No query to create view from."));
        return;
      }

      Gtk::Dialog dlg(_("Create View"), *_mainw, true, true);
      Gtk::Label label(_("Give a name for the View to be created."));
      Gtk::Entry entry;
      Gtk::Alignment align(0.5, 0.5, 0.5, 0.0);
      Glib::ustring templ, name;
      
      align.add(entry);
      
      dlg.get_vbox()->pack_start(label, true, true, 12);
      dlg.get_vbox()->pack_start(align, false, false, 6);
      
      dlg.get_vbox()->show_all();
      
      dlg.add_button(Gtk::Stock::CANCEL, 0);
      dlg.add_button(Gtk::Stock::OK, 1);

      if (dlg.run() == 0)
        return;

      name= entry.get_text();
      if (name.empty())
        return;
      
      templ= ufmt("CREATE VIEW `%s`.`%s` AS\n"
                  "  %s;", _dispatcher->get_current_schema().c_str(), name.c_str(),
                  query.c_str());
      
      if (_dispatcher->execute(templ))
        _mainw->set_status(_("View Created"));
    }
  }
}


void MQWorkArea::create_view()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (!iter || _table_browser->get_type(iter)!=MGTableBrowserList::Table)
    return;

  Gtk::Dialog dlg(_("Create View"), *_mainw, true, true);
  Gtk::Label label(_("Give a name for the View to be created."));
  Gtk::Entry entry;
  Gtk::Alignment align(0.5, 0.5, 0.5, 0.0);
  Glib::ustring templ, name;
  Glib::ustring schema= _table_browser->get_schema(iter);
  Glib::ustring table= _table_browser->get_table(iter);

  align.add(entry);

  dlg.get_vbox()->pack_start(label, true, true, 12);
  dlg.get_vbox()->pack_start(align, false, false, 6);

  dlg.get_vbox()->show_all();

  dlg.add_button(Gtk::Stock::CANCEL, 0);
  dlg.add_button(Gtk::Stock::OK, 1);

  if (dlg.run() == 0)
    return;

  name= entry.get_text();
  if (name.empty())
    return;

  templ= ufmt("CREATE VIEW `%s`.`%s` AS\n"
              "  SELECT * FROM %s;", schema.c_str(), name.c_str(),
              table.c_str());

  // create a new script editor tab and put the template there
  MQScriptEditorTab *tab= add_script_view(name);
  if (tab)
    tab->set_text(templ);
}


void MQWorkArea::edit_all_stored_procedures()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (!iter)
    return;

  Glib::ustring code= _dispatcher->get_all_sps(_dispatcher->get_current_catalog(),
                                               _dispatcher->get_current_schema());
  if (!code.empty())
  {
    MQScriptEditorTab *tab= add_script_view("Stored Procedures");
    if (tab)
      tab->set_text(code);
  }
}


void MQWorkArea::create_stored_procedure()
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  if (!iter)
    return;

  Gtk::Dialog dlg(_("Create Stored Routine"), *_mainw, true, true);
  Gtk::Label label(_("Give a name for the Stored Procedure or Function to be created."));
  Gtk::Entry entry;
  Gtk::Alignment align(0.5, 0.5, 0.5, 0.0);
  Glib::ustring templ, what, name, retval;
  Glib::ustring schema= _table_browser->get_schema(iter);
  
  align.add(entry);
  
  dlg.get_vbox()->pack_start(label, true, true, 12);
  dlg.get_vbox()->pack_start(align, false, false, 6);
  
  dlg.get_vbox()->show_all();
  
  dlg.add_button(Gtk::Stock::CANCEL, 0);
  dlg.add_button(_("Create Function"), 1);
  dlg.add_button(_("Create Procedure"), 2);

  switch (dlg.run())
  {
  case 0: return;
  case 1:
    what= "FUNCTION";
    retval= " RETURNS INT";
    break;
  case 2:
    what= "PROCEDURE";
    retval= "";
    break;
  }
  
  name= entry.get_text();
  if (name.empty())
    return;
  
  templ= ufmt("DELIMITER $$\n\n"
              "DROP %s IF EXISTS `%s`.`%s`$$\n"
              "CREATE %s `%s`.`%s` ()%s\n"
              "BEGIN\n\n"
              "END$$\n\n"
              "DELIMITER ;\n",
              what.c_str(), schema.c_str(), name.c_str(), 
              what.c_str(), schema.c_str(), name.c_str(), retval.c_str());

  // create a new script editor tab and put the template there
  MQScriptEditorTab *tab= add_script_view(name);
  if (tab)
    tab->set_text(templ);
}


void MQWorkArea::update_history_menu(MQResultSetView *view)
{  
  _back_history_menu.items().erase(_back_history_menu.items().begin(),
                                   _back_history_menu.items().end());
  _next_history_menu.items().erase(_next_history_menu.items().begin(),
                                   _next_history_menu.items().end());

  int index= view->get_history_index();

  // make a list of history entries
  int i= 0;
  std::list<std::string> history= view->get_history();
    
  for (std::list<std::string>::const_iterator iter= history.begin();
       iter != history.end(); ++iter, ++i)
  {
    MYX_HISTORY_ENTRY *hist= _history->find_entry(*iter);

    if (!hist)
    {
      continue;
    }
    
    Gtk::MenuItem *item= Gtk::manage(new Gtk::MenuItem(shorten_query(hist->sql)));

    _tips.set_tip(*item, hist->sql);

    item->signal_activate().connect(sigc::bind<std::string>(sigc::mem_fun(*this,&MQWorkArea::open_history),*iter));

    if (index > i)
    {
      _next_history_menu.prepend(*item);
    }
    else if (index < i)
    {
      _back_history_menu.append(*item);
    }
  }
  _back_history_menu.show_all();
  _next_history_menu.show_all();
  
  _xml->get_widget("back_box")->set_sensitive(!_back_history_menu.items().empty());
  _xml->get_widget("back_sbtn")->set_sensitive(!_back_history_menu.items().empty());

  _xml->get_widget("next_box")->set_sensitive(!_next_history_menu.items().empty());
  _xml->get_widget("next_sbtn")->set_sensitive(!_next_history_menu.items().empty());
}


void MQWorkArea::show_function_help(const std::string &id)
{
  if (!_help_tab)
  {
    _help_tab= Gtk::manage(new MQHelpTab);
    add_tab(_help_tab);
  }

  _help_tab->set_title(_("Function Help"));
  _help_tab->show_help(id, get_app_file("mysqlqb_functions.html"));
  _note->set_current_page(_note->page_num(*_help_tab));
}


void MQWorkArea::show_syntax_help(const std::string &id)
{
  if (!_help_tab)
  {
    _help_tab= Gtk::manage(new MQHelpTab);
    add_tab(_help_tab);
  }

  _help_tab->set_title(_("SQL Help"));
  _help_tab->show_help(id, get_app_file("mysqlqb_statements.html"));
  _note->set_current_page(_note->page_num(*_help_tab));
}


void MQWorkArea::show_quickstart()
{
  if (!_help_tab)
  {
    _help_tab= Gtk::manage(new MQHelpTab);
    add_tab(_help_tab);
  }

  _help_tab->set_title(_("Quickstart Guide"));
  _help_tab->show_help("", get_app_file("mysqlqb_quickstart.html"));
  _note->set_current_page(_note->page_num(*_help_tab));
}


void MQWorkArea::show_help()
{
  std::string path;
  
#ifdef DOCDIR
  path= DOCDIR"/mysql-query-browser/index.html";
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+"/doc/query-browser.html";
#else
    path= get_prefix()+"/doc/query-browser.html"; // /opt/mysql-query-browser/doc
#endif
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+DATADIRNAME+"/doc/mysql-query-browser/query-browser.html"; // /usr/share/doc
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_prefix()+DATADIRNAME+"/mysql-gui/doc/query-browser/query-browser.html"; // /usr/share/mysql-gui/doc
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    path= get_app_file("query-browser.html");
  if (!Glib::file_test(path, Glib::FILE_TEST_EXISTS))
    /* location of the manual in SUSE */
    path= "/usr/share/doc/packages/mysql-query-browser/query-browser.html";


  if (!_help_tab)
  {
    _help_tab= Gtk::manage(new MQHelpTab);
    add_tab(_help_tab);
  }

  _help_tab->set_title(_("Help"));
  _help_tab->show_help("", path);
  _note->set_current_page(_note->page_num(*_help_tab));
}



void MQWorkArea::popup_param_menu(GdkEventButton *event)
{
  if (event->button == 3)
    _param_menu.popup(event->button, event->time);
}


void MQWorkArea::param_add_mi()
{
  Gtk::TreeIter iter= _xml->get_tree("param_tree")->get_selection()->get_selected();
  if (!iter)
    return;
  
  Gtk::TreeRow row= *iter;
  
  if (row[_pcolumns.editable])
    iter= row.parent();
  row= *iter;
  
  int type= row[_pcolumns.type];
  Glib::ustring name, value;
  
  if (get_tab()->get_type() != TResultSet)
    return;
  
  {
    Gtk::Dialog dlg(type == 0?_("New Global Parameter") : _("New Local Parameter"), *_mainw, true, true);
    Gtk::Label label1(_("Parameter Name:"), 1.0, 0.5);
    Gtk::Label label2(_("Value:"), 1.0, 0.5);
    Gtk::Entry entry1, entry2;
    Gtk::Table table;

    table.set_row_spacings(12);
    table.set_col_spacings(8);
    
    table.set_border_width(12);
    table.attach(label1, 0, 1, 0, 1, Gtk::FILL, Gtk::FILL);
    table.attach(label2, 0, 1, 1, 2, Gtk::FILL, Gtk::FILL);
    table.attach(entry1, 1, 2, 0, 1);
    table.attach(entry2, 1, 2, 1, 2);

    dlg.get_vbox()->pack_start(table, false, false, 6);

    dlg.get_vbox()->show_all();
      
    dlg.add_button(Gtk::Stock::CANCEL, 0);
    dlg.add_button(Gtk::Stock::OK, 1);

again:
    
    if (dlg.run() == 0)
      return;
    
    name= entry1.get_text();
    value= entry2.get_text();
    
    if (name.empty())
      return;
    
    
    for (unsigned int i= 0; i < name.size(); i++)
      if (!isalnum(name[i]) && name[i]!='_')
      {
        myg_show_warning(*_mainw, 
                         _("Parameter name may only contain alphabetic and numeric characters."));
        goto again;
      }
  }
  
  MQResultSetView *rsview= static_cast<MQResultSetView*>(static_cast<MQResultTab*>(get_tab())->get_active());
  
  Gtk::TreeIter niter= _param_store->append(row.children());
  row= *niter;

  row[_pcolumns.icon]= PIXCACHE->load("param_14x14.png");
  row[_pcolumns.type]= type;
  row[_pcolumns.editable]= true;
  row[_pcolumns.text]= name;
  row[_pcolumns.value]= value;

  if (type == 0)
    rsview->get_parameters()->set_global(name, value);
  else
    rsview->get_parameters()->set_local(name, value);

  _xml->get_tree("param_tree")->expand_all();
}


void MQWorkArea::param_delete_mi()
{
  Gtk::TreeIter iter= _xml->get_tree("param_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;

  if (get_tab()->get_type() != TResultSet || !iter)
    return;

  MQResultSetView *rsview= static_cast<MQResultSetView*>(static_cast<MQResultTab*>(get_tab())->get_active());

  if (row[_pcolumns.editable])
  {
    int type= row[_pcolumns.type];
    
    if (type == 0)
      rsview->get_parameters()->delete_global(row[_pcolumns.text]);
    else
      rsview->get_parameters()->delete_local(row[_pcolumns.text]);
    _param_store->erase(iter);
  }
  
  _xml->get_tree("param_tree")->columns_autosize();
}


void MQWorkArea::update_parameter_list(MQQueryParameters *params)
{
  _param_store->clear();
  
  if (params)
  {
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    Gtk::TreeIter piter;
    Gtk::TreeRow prow;

    piter= _param_store->append();
    prow= *piter;
    prow[_pcolumns.icon]= PIXCACHE->load("folder_open_14x14.png");
    prow[_pcolumns.text]= _("Local");
    prow[_pcolumns.editable]= false;
    prow[_pcolumns.type]= 1;
    std::list<MQParameter> list= params->get_local();
    for (std::list<MQParameter>::const_iterator it= list.begin(); 
         it != list.end(); ++it)
    {
      iter= _param_store->append(prow.children());
      row= *iter;

      row[_pcolumns.icon]= PIXCACHE->load("param_14x14.png");
      row[_pcolumns.text]= it->first;
      row[_pcolumns.value]= it->second;
      row[_pcolumns.editable]= true;
      row[_pcolumns.type]= 1;
    }

    piter= _param_store->append();
    prow= *piter;
    prow[_pcolumns.icon]= PIXCACHE->load("folder_open_14x14.png");
    prow[_pcolumns.text]= _("Global");
    prow[_pcolumns.editable]= false;
    prow[_pcolumns.type]= 0;
    list= params->get_global();
    for (std::list<MQParameter>::const_iterator it= list.begin(); 
         it != list.end(); ++it)
    {
      iter= _param_store->append(prow.children());
      row= *iter;

      row[_pcolumns.icon]= PIXCACHE->load("param_14x14.png");
      row[_pcolumns.text]= it->first;
      row[_pcolumns.value]= it->second;
      row[_pcolumns.editable]= true;
      row[_pcolumns.type]= 0;
    }
    
    piter= _param_store->append();
    prow= *piter;
    prow[_pcolumns.icon]= PIXCACHE->load("folder_open_14x14.png");
    prow[_pcolumns.text]= _("Dynamic");
    prow[_pcolumns.editable]= false;
    prow[_pcolumns.type]= 0;

    list= params->get_dynamic();
    for (std::list<MQParameter>::const_iterator it= list.begin();
         it != list.end(); ++it)
    {
      iter= _param_store->append(prow.children());
      row= *iter;

      row[_pcolumns.icon]= PIXCACHE->load("param_14x14.png");
      row[_pcolumns.text]= it->first;
      row[_pcolumns.value]= it->second;
      row[_pcolumns.editable]= false;
      row[_pcolumns.type]= 0;
    }

    _xml->get_tree("param_tree")->expand_all();
  }
}


void MQWorkArea::param_edited(const Glib::ustring& path,
                              const Glib::ustring& new_text)
{
  Gtk::TreeIter iter= _param_store->get_iter(Gtk::TreePath(path));
  Gtk::TreeRow row= *iter;
  MQResultSetView *rsview= static_cast<MQResultSetView*>(static_cast<MQResultTab*>(get_tab())->get_active());

  int type= row[_pcolumns.type];

  if (type == 0)
    rsview->get_parameters()->set_global(row[_pcolumns.text], new_text);
  else
    rsview->get_parameters()->set_local(row[_pcolumns.text], new_text);
}


void MQWorkArea::catalogs_refreshed()
{
  Gtk::TreeIter item;
  Glib::ustring catalog= _dispatcher->get_current_catalog();
  Glib::ustring schema= _dispatcher->get_current_schema();

  _table_browser->set_catalogs(_catalogs);

  item= _table_browser->find_table(catalog, schema);

  if (item)
    default_schema_changed();
}


void MQWorkArea::select_schema_from_browser()
{
  Gtk::TreeIter item= _table_browser->get_selected();
  if (item)
  {
    Glib::ustring catalog= _table_browser->get_catalog(item);
    Glib::ustring schema= _table_browser->get_schema(item);

    _mainw->get_bookmarks()->get_browser()->refresh_activatable(catalog, schema);
    
    _dispatcher->select_schema(catalog, schema);
  }
}

void MQWorkArea::copy_sql()
{
  Gtk::TreeIter item= _table_browser->get_selected();
  if (item)
  {
    bool is_func= false;
    Glib::ustring catalog= _table_browser->get_catalog(item);
    Glib::ustring schema= _table_browser->get_schema(item);
    Glib::ustring entity;
    Glib::ustring sql;
    
    switch(_table_browser->get_type(item)) {
    case MGTableBrowserList::SP:
      entity= _table_browser->get_procedure(item, is_func);
      if(is_func)
      {
        sql= myx_dbm_get_create_sql(_dispatcher->get_mysql(), 
                                    catalog.c_str(), schema.c_str(), entity.c_str(), 
                                    MYX_DBM_OT_FUNCTION, 1,
                                    MYX_DEFAULT_QUOTE_CHAR, 0);
      }
      else
      {
        sql= myx_dbm_get_create_sql(_dispatcher->get_mysql(), 
                                    catalog.c_str(), schema.c_str(), entity.c_str(),
                                    MYX_DBM_OT_PROCEDURE, 1,
                                    MYX_DEFAULT_QUOTE_CHAR, 0);
      }
      break;
    case MGTableBrowserList::Table:
      entity= _table_browser->get_table(item);
      sql= myx_dbm_get_create_sql(_dispatcher->get_mysql(),
        catalog.c_str(), schema.c_str(), entity.c_str(), MYX_DBM_OT_TABLE, 1,
                                  MYX_DEFAULT_QUOTE_CHAR, 0);
      break;
    }
    
    Gtk::Clipboard::get()->set_text(sql);
  }
}


Glib::ustring MQWorkArea::interactive_modify_sql(const Glib::ustring &current_query,
                                                 Gdk::ModifierType modifier_state, int &cursor_pos,
                                                 Gtk::TextView *view, bool below)
{
  Glib::ustring query= current_query;
  MYX_Q_TABLE_ADD_TYPE add_type;
  MYX_Q_TABLE_ADD_ERROR add_error;
  Glib::ustring result;
  Gtk::TreeIter iter;

  if ((modifier_state & Gdk::CONTROL_MASK) == Gdk::CONTROL_MASK)
    add_type= MYX_QTAT_SELECT_ADD;
  else if ((modifier_state & Gdk::SHIFT_MASK) == Gdk::CONTROL_MASK)
    add_type= MYX_QTAT_SELECT_JOIN;
  else
    add_type= MYX_QTAT_SELECT;

  iter= _table_browser->get_selected();
  if (iter)
  {
    switch (_table_browser->get_type(iter))
    {
    case MGTableBrowserList::Schema:
      result="USE "+_table_browser->get_schema(iter);
      break;
    case MGTableBrowserList::Table:
      query= myx_query_add_table_to_sql(_dispatcher->get_mysql(),
                                        _dispatcher->get_current_schema().c_str(),
                                        _table_browser->get_catalog(iter).c_str(),
                                        _table_browser->get_schema(iter).c_str(),
                                        _table_browser->get_table(iter).c_str(),
                                        query.c_str(),
                                        add_type,
                                        &cursor_pos,
                                        &add_error);
      switch (add_error)
      {
      case MYX_QC_TABLES_WITHOUT_ALIAS:
        _mainw->set_status(_("Can't merge table to the query because it's missing an alias."));
        break;
      case MYX_QC_TABLES_CAN_NOT_BE_JOINED:
        _mainw->set_status(_("The tables cannot be joined."));
        break;
      default:
        if (add_type == MYX_QTAT_SELECT && prefs.default_limit_value > 0)
          query+=" LIMIT 0,"+tostr(prefs.default_limit_value);
        break;
      }
      result= query;
      break;
    case MGTableBrowserList::Column:
      result= myx_query_add_column_to_sql(_dispatcher->get_mysql(),
                                  //_dispatcher->get_current_schema().c_str(),
                                  _dispatcher->get_mysql()->db,
                                  _table_browser->get_catalog(iter).c_str(),
                                  _table_browser->get_schema(iter).c_str(),
                                  _table_browser->get_table(iter).c_str(),
                                  _table_browser->get_table_column(iter).c_str(),
                                  query.c_str(),
                                  MYX_QCT_SELECT_CLAUSE,
                                  &cursor_pos);
      
      break;
    case MGTableBrowserList::SP:
      {
        bool is_func;
        Glib::ustring sp, schema;
        sp= _table_browser->get_procedure(iter, is_func);
        schema= _table_browser->get_schema(iter);
        if (is_func)
          result=ufmt("SELECT `%s`.`%s`()", schema.c_str(), sp.c_str());
        else
          result=ufmt("CALL `%s`.`%s`()", schema.c_str(), sp.c_str());
        cursor_pos= result.size()-1;
      }
      break;
    default:
      result= query;
      break;
    }
  }
  else
    result= query;
  return result;
}


void MQWorkArea::schemata_dbl_clicked()
{
  Glib::ustring query;
  int cursor_pos;
  int x,y;
  Gtk::TextIter iter;
  Gdk::ModifierType mod;
  Gdk::Display::get_default()->get_pointer(x,y,mod);

  Gtk::TreeIter it= _table_browser->get_selected();
  if (it && _table_browser->get_type(it) == MGTableBrowserList::Schema)
  {
    select_schema_from_browser();
  }
  else
  {
    Glib::ustring nquery;
    
    query= _query_buffer->get_text();
    
    nquery= interactive_modify_sql(query, mod, cursor_pos);
    
    _query_buffer->set_text(nquery);
    if (cursor_pos >= 0)
    {
      iter= _query_buffer->get_iter_at_offset(cursor_pos);
      _query_buffer->place_cursor(iter);
    }
    _xml->get_text("query_text")->grab_focus();
    
    //XXX add check so that it will execute only if there was no error in
    //interactive_modify_sql
    if (nquery == query && !query.empty())
      execute_clicked();
  }
}


void MQWorkArea::schemata_selected(MGBrowserList *list,const Gtk::TreeIter &item)
{
}


//======================================================================
// Query Browser
//======================================================================


void MQWorkArea::search_resultset()
{
  int p= _note->get_current_page();
  if (get_tab(p)->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab(p));
    MQResultSetView *rset= tab->get_active();
    start_search(rset);
  }
}


void MQWorkArea::export_resultset(const char *fmt)
{
  MYX_TABLE_EXPORTER *exporter= myx_get_table_exporter(fmt);
  MYX_TABLE_EXPORTER_INFO *info;
  MYX_RESULTSET *rset= 0;
  Gtk::FileSelection fsel;
  Glib::ustring filename;
  MQBaseTab *wtab= get_tab();
  MQResultTab *tab= wtab->get_type()==TResultSet ? dynamic_cast<MQResultTab*>(wtab) : 0;
  Glib::ustring detail_query;

  if (!exporter)
  {
    myg_show_error(*_mainw, ufmt(_("Could not locate export plugin for %s."),fmt));
    return;
  }
  
  
  if (!tab || !tab->get_active() || tab->get_active()->is_busy()
      || !tab->get_active()->get_resultset())
    return;


  if (strcmp(fmt, "HTML")==0 || strcmp(fmt, "XML")==0)
  {
    if (tab->get_item_count() > 1
        && tab->get(0)->get_resultset() && tab->get(1)->get_resultset())
    {
      rset= tab->get(0)->get_resultset();
      detail_query= tab->get(1)->get_shown_query();
    }
  }

  if (!rset)
    rset= tab->get_active()->get_resultset();

  info= (*exporter->init)();
  
  fsel.set_title(exporter->description);
  fsel.set_filename(ufmt("resultset.%s",exporter->file_extension));
  
  if (strcmp(fmt, "CSV")==0)
  {
    //XXX put the option to pick the separator char
  }
  
  if (fsel.run() != Gtk::RESPONSE_OK)
  {
    (*exporter->free)(info);
    return;
  }
  
  filename= fsel.get_filename();

  if (myx_export_resultset(_dispatcher->get_mysql(), info,
                           filename.c_str(),
                           "$QUERY$",
                           rset, detail_query.empty()?NULL:detail_query.c_str())< 0)
  {
    myg_show_error(*_mainw, ufmt(_("Error exporting resultset to file '%s'."), filename.c_str()));
    _mainw->set_status(_("Could not export resultset."));
  }
  else
    _mainw->set_status(ufmt(_("Resultset exported to file '%s'."), filename.c_str()));

  (*exporter->free)(info);
}


void MQWorkArea::update_resultset_menu(MQResultSetView *rsview, Gtk::Menu *menu)
{
  MQResultTab *tab;
  
  if (_note->get_n_pages()>1)
    myg_menu_set_sensitive(*menu, "remove_tab", true);
  else
    myg_menu_set_sensitive(*menu, "remove_tab", false);

  tab= get_tab_for_rsview(rsview); 
  
  if (tab && tab->get_item_count() > 1)
  {
    myg_menu_set_sensitive(*menu, "remove_rs", true);
    
    if (!tab->is_vertical())
    {
      myg_menu_set_sensitive(*menu, "splith", true);
      myg_menu_set_sensitive(*menu, "splitv", false);
    }
    else
    {
      myg_menu_set_sensitive(*menu, "splith", false);
      myg_menu_set_sensitive(*menu, "splitv", true);
    }
  }
  else
  {
    myg_menu_set_sensitive(*menu, "remove_rs", false);
    myg_menu_set_sensitive(*menu, "splith", true);
    myg_menu_set_sensitive(*menu, "splitv", true);
  }
}


MQResultSetView *MQWorkArea::add_result_view(const Glib::ustring &titl,
                                             MQResultTab *in_tab,
                                             bool vertical)
{
  bool new_tab= false;
  Glib::ustring title= titl;

  if (title.empty())
  {
    title= ufmt(_("Resultset %i"), _resultset_count);
    // user created tabs have in_tab==0, kinda hackish but who cares..
    if (!in_tab) _resultset_count++;
  }
  if (!in_tab)
  {
    in_tab= Gtk::manage(new MQResultTab);
    new_tab= true;
  }

  MQResultSetView *rset= Gtk::manage(new MQResultSetView(_dispatcher, _history, _global_params));
  
  {
    Gtk::Menu *menu= rset->get_menu();

    menu->signal_show().connect(sigc::bind<MQResultSetView*,Gtk::Menu*>(sigc::mem_fun(*this,&MQWorkArea::update_resultset_menu),rset,menu));
    
    myg_menu_add(*menu);

    myg_menu_add(*menu, "TabSheetPopup_AddRS.png", _("Add New _Resultset Tab"),
                 sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),in_tab,MQBaseTab::ANewQuery),
                 "add_rs");
    myg_menu_add(*menu, "TabSheetPopup_AddScript.png", _("Add New _Script Tab"),
                 sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),in_tab,MQBaseTab::ANewScript),
                 "add_script");
    
    myg_menu_add(*menu);
    
    myg_menu_add(*menu, "TabSheetPopup_SplitVertical.png", _("Split Tab _Vertically"),
                 sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),in_tab,MQBaseTab::ASplitV),
                 "splitv");
    myg_menu_add(*menu, "TabSheetPopup_SplitHorizont.png", _("Split Tab _Horizontally"),
                 sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),in_tab,MQBaseTab::ASplit),
                 "splith");

    myg_menu_add(*menu);

    myg_menu_add(*menu, "TabSheetPopup_DelSplit.png", _("Remove Resultset"),
                 sigc::bind<MQBaseTab::TabActionType,MQResultSetView*>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action2),MQBaseTab::AUnsplit,rset),
                 "remove_rs");
    myg_menu_add(*menu, "TabSheetPopup_DelTab.png", _("Remove Tab"),
                 sigc::bind<MQBaseTab*,int>(sigc::mem_fun(*this,&MQWorkArea::do_tab_action),in_tab,MQBaseTab::AClose),
                 "remove_tab");
  }
  
  rset->set_search_func(sigc::mem_fun(*this,&MQWorkArea::start_search));

  rset->set_active(true);
  in_tab->add_resultset(rset, vertical);

  rset->signal_activate().connect(sigc::mem_fun(*this,&MQWorkArea::rset_changed));
  rset->signal_query_started().connect(sigc::mem_fun(*this,&MQWorkArea::query_started));
  rset->signal_query_finished().connect(sigc::mem_fun(*this,&MQWorkArea::query_finished));
  rset->signal_more_data().connect(sigc::mem_fun(*this,&MQWorkArea::query_more_data));
  rset->signal_row_changed().connect(sigc::bind<MQResultTab*>(sigc::mem_fun(*this,&MQWorkArea::rset_row_changed),in_tab));


  std::list<Gtk::TargetEntry> targets;
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-query", Gtk::TARGET_SAME_WIDGET, 0));
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-schema-object", Gtk::TARGET_SAME_WIDGET, 0));
  rset->drag_dest_set(targets);
  
  {
    // ugly hack, see top of this file if you want to know why this exists
    DropDataReceivedProxyHack *proxy= new DropDataReceivedProxyHack();
    proxy->rset= rset;
    proxy->warea= this;
    // make the hack object get freed when the widget is destroyed
    rset->set_data("bla", proxy, DropDataReceivedProxyHack::destroy);
    rset->signal_drag_data_received().connect(sigc::mem_fun(*proxy,&DropDataReceivedProxyHack::drag_data_received));

    if (new_tab)
    {
      in_tab->set_label(rset, title);
      add_tab(in_tab);
    }
  }
  return rset;
}


void MQWorkArea::text_changed()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    
    tab->set_query_text(tab->get_active(), _query_buffer->get_text());
  }
  
  if (prefs.view_type != 'Q')
  {
    int line_count= _query_buffer->end().get_line();
    int wanted_height;
    int cur_w, cur_h;
  
    wanted_height= _query_line_height*(line_count+1);
    if (wanted_height > 150)
      wanted_height= 150;
    
    _xml->get_widget("query_area_vp")->get_size_request(cur_w, cur_h);
    if (cur_h != wanted_height)
    {
      // increase (or decrease) the height of the textbox if necessary
      _xml->get_widget("query_area_vp")->set_size_request(-1, wanted_height);
    }
  }
}


void MQWorkArea::get_exec_pos(int &x, int &y, bool &push_in)
{
  _xml->get_widget("doexecute_btn")->get_window()->get_origin(x,y);
  _xml->get_widget("doexecute_btn")->translate_coordinates(*get_toplevel(),
                                                           x,y, x,y);
  y+= _xml->get_widget("doexecute_btn")->get_height();
}


void MQWorkArea::execute_menu_clicked()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());

    tab->set_query_text(tab->get_active(), _query_buffer->get_text());

    _execute_menu.popup(sigc::mem_fun(*this,&MQWorkArea::get_exec_pos),0, 0);
  }
}


void MQWorkArea::get_execs_pos(int &x, int &y, bool &push_in)
{
  _xml->get_widget("sdoexecute_btn")->get_window()->get_origin(x,y);
  _xml->get_widget("sdoexecute_btn")->translate_coordinates(*get_toplevel(),
                                                            x,y, x,y);
  y+= _xml->get_widget("sdoexecute_btn")->get_height();
}

void MQWorkArea::execute_script_menu_clicked()
{
  if (get_tab()->get_type() == TScriptEditor)
  {
    _execute_script_menu.popup(sigc::mem_fun(*this,&MQWorkArea::get_execs_pos),0, 0);
  }
}


bool MQWorkArea::query_get_completion_list(const Gtk::TextIter &iter, std::list<Glib::ustring> &list)
{
  list= _query_buffer->get_suggestions(iter);

  return !list.empty();
}


bool MQWorkArea::query_text_key_press(GdkEventKey *ev)
{
  if (ev->keyval == GDK_Return && (ev->state & GDK_CONTROL_MASK))
  {
    execute_clicked();
    return true;
  }
  else if (ev->keyval == GDK_u && (ev->state & GDK_CONTROL_MASK))
//  else if (ev->keyval == GDK_Escape)
  {
    _query_buffer->set_text("");
    return true;
  }
  else if ((ev->state & GDK_CONTROL_MASK) && 
           (ev->keyval == GDK_Page_Up || ev->keyval == GDK_Page_Down))
  {
    if (ev->keyval == GDK_Page_Down)
      _note->next_page();
    else
      _note->prev_page();
  }
  return false;
}


void MQWorkArea::execute_clicked(bool refresh)
{
  Glib::ustring query= _query_buffer->get_text();
	
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    if (!rset->is_busy())
    {
      if (refresh)
      {
        query= rset->get_shown_query();
        _query_buffer->set_text(query);
      }
      rset->execute_query(query, refresh);
      // in case a transaction was implicitly closed
      update_toolbar_sensitivity();

      update_history_menu(rset);
    }
  }
}


void MQWorkArea::execute_in_new_tab()
{
  Glib::ustring query= _query_buffer->get_text();

  // create new tab
  add_result_view("");

  // set up tab
  _query_buffer->set_text(query);

  execute_clicked();
}


void MQWorkArea::execute_in_split_tab()
{
  Glib::ustring query= _query_buffer->get_text();

  // split tab
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *old_rs= tab->get_active();
    Glib::ustring old_query= old_rs ? old_rs->get_shown_query() : "";
    int nidx;

    MQResultSetView *rset= add_result_view("", tab, false);

    nidx= tab->get_index(rset);

    if (old_rs)
    {
      // restore query text for the other half
      tab->set_query_text(old_rs, old_query);
    }

    // set up tab
    rset->set_active(true);

    _query_buffer->set_text(query);

    int ni= nidx-1;
    if (ni < 0)
      ni= nidx+1;

    // set up parameters
    if (tab->get(ni)->get_selected_row())
    {
      rset->get_parameters()->process_resultset(_dispatcher->get_mysql(),
                                                tab->get(ni)->get_resultset(),
                                                tab->get(ni)->get_selected_row());
    }
    execute_clicked();
  }
}



void MQWorkArea::stop_clicked()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());

    tab->get_active()->cancel_query();
  }
}


void MQWorkArea::explain_result()
{  
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());

    tab->get_active()->explain_query();
  }
}


void MQWorkArea::compare_results()
{
  MQBaseTab *tab= get_tab();
  
  if (tab->get_type() == TResultSet)
  {
    MQResultTab *rtab= static_cast<MQResultTab*>(tab);

    if (rtab->get_item_count() == 2 && rtab->is_vertical())
    {
      MQResultSetView *rsv1= rtab->get(0);
      MQResultSetView *rsv2= rtab->get(1);
      MYX_RESULTSET *rs1, *rs2;
      
      rs1= rsv1->get_resultset();
      rs2= rsv2->get_resultset();
      
      if (rs1 && rs2)
      {
        if (!myx_query_compare_possible(rs1, rs2))
        {
          myg_show_info(*static_cast<Gtk::Window*>(get_toplevel()), 
                        _("Resultsets must contain the same columns and have primary keys to allow comparison."));
        }
        else
        {
          int res;
          
          _mainw->set_status(_("Comparing resultsets..."));
          res= myx_query_compare_results(rs1, rs2);
          _mainw->set_status(_("Comparison finished."));
          
          if (res < 0)
          {
            myg_show_error(*static_cast<Gtk::Window*>(get_toplevel()), 
                          _("An error occurred while comparing the resultsets"));
          }
          else
          {
            rtab->show_compare_toolbar();
            rsv1->redisplay();
            rsv2->redisplay();
            rsv1->set_compact_editbar(true);
            rsv2->set_compact_editbar(true);
            rtab->set_sync_scrolling(true);
            rtab->queue_draw();
          }
        }
      }
      else
      {
        myg_show_info(*static_cast<Gtk::Window*>(get_toplevel()), 
                       _("You must have two resultsets in the same tab in order to compare them."));
      }
    }
    else if (rtab->get_item_count() == 2 && !rtab->is_vertical())
    {
      myg_show_info(*static_cast<Gtk::Window*>(get_toplevel()),
                    _("You must have two resultsets laid-out vertically to compare them."));
    }
    else if (rtab->get_item_count() > 2)
    {
      myg_show_info(*static_cast<Gtk::Window*>(get_toplevel()),
                    _("You can compare at most two query resultsets."));
    }
    else
    {
      myg_show_info(*static_cast<Gtk::Window*>(get_toplevel()),
                    _("You must have two query results in the same tab in order to compare them.\n"
                      "Please split the tab vertically first."));
    }
  }
}


void MQWorkArea::trans_start_clicked()
{
  _dispatcher->start_transaction();
}


void MQWorkArea::trans_commit_clicked()
{
  _dispatcher->commit_transaction();
}


void MQWorkArea::trans_rollback_clicked()
{
  _dispatcher->rollback_transaction();
}


void MQWorkArea::trans_started()
{
  update_toolbar_sensitivity();
  _mainw->set_status(_("Transaction started."));
  
  Gtk::Notebook *note= (Gtk::Notebook*)_xml->get_widget("sidebottom_notebook");
  
  note->set_current_page(note->get_n_pages()-1);

  Gtk::TreeIter iter= _trans_log->append();
  Gtk::TreeRow row= *iter;
  row[_tcolumns.icon]= PIXCACHE->load("trans_started.png");
  row[_tcolumns.text]= _("Transaction Started.");
}


void MQWorkArea::trans_ended(bool commited)
{
  update_toolbar_sensitivity();

  if (commited)
    _mainw->set_status(_("Transaction commited."));
  else
    _mainw->set_status(_("Transaction rolled back."));

  Gtk::TreeIter iter= _trans_log->append();
  Gtk::TreeRow row= *iter;

  if (commited)
  {
    row[_tcolumns.icon]= PIXCACHE->load("trans_commited.png");
    row[_tcolumns.text]= _("Transaction commited.");
  }
  else
  {
    row[_tcolumns.icon]= PIXCACHE->load("trans_rolledback.png");
    row[_tcolumns.text]= _("Transaction rolled back.");
  }
}


void MQWorkArea::trans_command(Glib::ustring query)
{
  Gtk::TreeIter iter= _trans_log->append();
  Gtk::TreeRow row= *iter;
  row[_tcolumns.text]= query;
}


MQResultTab *MQWorkArea::get_tab_for_rsview(MQResultSetView *rsview)
{
  for (int i= 0; i < _note->get_n_pages(); i++)
  {
    MQBaseTab *tab= get_tab(i);
    
    if (tab->get_type() == TResultSet && ((MQResultTab*)tab)->contains_rsview(rsview))
    {
      return (MQResultTab*)tab;
    }
  }
  return 0;
}


void MQWorkArea::query_started(MQResultSetView *sender, bool saving)
{  
  update_toolbar_sensitivity();

  _xml->get_button("execute_btn")->set_sensitive(false);
  _xml->get_button("doexecute_btn")->set_sensitive(false);
  _xml->get_button("stop_btn")->set_sensitive(true);

  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());

    if (tab->contains_rsview(sender))
    {
      set_query_bar_sensitivity(false);
      set_busy_animation(true);
    }
  }

  get_tab_for_rsview(sender)->set_busy(true);

  if (saving)
    _mainw->set_status(_("Saving changes..."));
  else
    _mainw->set_status(_("Executing query..."));
}



static bool remove_resultset(MQResultTab *tab, MQResultSetView *rsview)
{
  tab->remove_resultset(rsview);
  
  return false;
}

void MQWorkArea::query_finished(MQResultSetView *sender)
{
  MQResultTab *tab;

  update_toolbar_sensitivity();

  // for every query at least 2 resultsets will be created,
  // more if it returns more than 1 resultset. The extra resultset
  // is destroyed after it's determined that there was no more resultsets
  // to fetch
  if (sender->get_resultset())
    update_history_menu(sender);

  tab= get_tab_for_rsview(sender);
  tab->set_busy(false);

  _xml->get_button("execute_btn")->set_sensitive(true);
  _xml->get_button("doexecute_btn")->set_sensitive(true);
  _xml->get_button("stop_btn")->set_sensitive(false);

  if (tab == static_cast<MQResultTab*>(get_tab()))
  {
    set_query_bar_sensitivity(true);
    set_busy_animation(false);
  }

  //if (sender->get_resultset())
    _mainw->set_status(_("Query finished."));

  // make the unused resultsetview be disposed as soon as it can
  if (!sender->get_resultset() && sender->dispose_if_no_result)
  {
    //Glib::signal_idle().connect(sigc::bind(sigc::mem_fun(*tab, &MQResultTab::remove_resultset), sender));
    Glib::signal_idle().connect(sigc::bind_return(sigc::bind(sigc::mem_fun(*tab, &MQResultTab::remove_resultset), sender), false));
  }
  else
    sender->dispose_if_no_result= false;

  _xml->get_text("query_text")->grab_focus();
}


void MQWorkArea::query_more_data(MQResultSetView *sender, MYSQL *mysql)
{
  MQResultTab *tab= get_tab_for_rsview(sender);
  MQResultSetView *rsview;

  // create a new resultset view, but leave it hidden until data arrives to it
  rsview= add_result_view("", tab, tab->is_vertical());
  
  // make the resultset be automatically destroyed if there's no resultset
  rsview->dispose_if_no_result= true;
  rsview->get_parent()->get_parent()->hide();
  
  rsview->fetch_more(mysql);
}



void MQWorkArea::rset_changed(MQResultSetView *sender)
{
  if (get_tab() && get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
      
    _query_buffer->set_text(tab->get_query_text(sender));
    
    update_parameter_list(sender->get_parameters());
  }
}


void MQWorkArea::rset_row_changed(MQResultSetView *sender, MQResultTab *tab)
{
  if (tab->get_item_count()>1)
  {
    MQResultSetView *target= 0;
    int index;
    
    index= tab->get_index(sender);
    if (index < 0 || index == tab->get_item_count()-1
        || !sender->get_selected_row())
      return;
    
    target= tab->get(index+1);

    // update dynamic parameters
    target->get_parameters()->process_resultset(_dispatcher->get_mysql(),
                                                sender->get_resultset(),
                                                sender->get_selected_row());

    // Re-execute the detail query for master-detail queries
    // 
    if (!target->get_resultset() || (target->get_resultset()->query && target->get_resultset()->query->params_num > 0))
    {
      Glib::ustring shown_query= tab->get_query_text(target);
      
      if (!shown_query.empty())
      {
        target->execute_query(shown_query, true);
      }
    }
  }
}


void MQWorkArea::find_cancel()
{
  Gtk::Main::instance()->quit();
}


void MQWorkArea::find_next(MQResultSetView *target)
{
  Glib::ustring value, columns;

  value= _search_xml->get_entry("value_entry")->get_text();
  columns= _search_xml->get_entry("columns_entry")->get_text();
  
  if (value.empty())
    return;

  _mainw->set_status(_("Searching..."));
  if (!target->find_next(value, columns))
    _mainw->set_status(_("No more matches."));
  else
    _mainw->set_status("");
}


void MQWorkArea::find_previous(MQResultSetView *target)
{
  Glib::ustring value, columns;

  value= _search_xml->get_entry("value_entry")->get_text();
  columns= _search_xml->get_entry("columns_entry")->get_text();

  if (value.empty())
    return;

  _mainw->set_status(_("Searching..."));
  if (!target->find_previous(value, columns))
    _mainw->set_status(_("No more matches."));
  else
    _mainw->set_status("");
}


void MQWorkArea::start_search(MQResultSetView *sender)
{
  if (!_search_xml)
  {
    _search_xml= new MGGladeXML(get_app_file("query_browser.glade"),
                                "find_dialog");
    static_cast<Gtk::Window*>(_search_xml->get_widget("find_dialog"))->set_transient_for(*static_cast<Gtk::Window*>(get_toplevel()));
    _search_xml->get_button("cancel_button")->signal_clicked().connect(sigc::mem_fun(*this,&MQWorkArea::find_cancel));
    _search_xml->get_button("next_button")->signal_clicked().connect(sigc::bind<MQResultSetView*>(sigc::mem_fun(*this,&MQWorkArea::find_next),sender));
    _search_xml->get_button("previous_button")->signal_clicked().connect(sigc::bind<MQResultSetView*>(sigc::mem_fun(*this,&MQWorkArea::find_previous),sender));
  }
  
  if (_search_xml->get_widget("find_dialog")->is_visible())
  {
    // if we're being called recursively, quit the previous loop
    Gtk::Main::instance()->quit();
  }

  _search_xml->get_widget("find_dialog")->show();
  Gtk::Main::instance()->run();
  _search_xml->get_widget("find_dialog")->hide();
}


void MQWorkArea::get_back_pos(int &x, int &y, bool &push_in)
{
  _xml->get_widget("goback_btn")->get_window()->get_origin(x,y);
  _xml->get_widget("goback_btn")->translate_coordinates(*get_toplevel(),
                                                        x,y, x,y);
  y+= _xml->get_widget("goback_btn")->get_height();
}


void MQWorkArea::get_next_pos(int &x, int &y, bool &push_in)
{
  _xml->get_widget("gonext_btn")->get_window()->get_origin(x,y);
  _xml->get_widget("gonext_btn")->translate_coordinates(*get_toplevel(),
                                                        x,y, x,y);
  y+= _xml->get_widget("gonext_btn")->get_height();
}


void MQWorkArea::back_button_clicked()
{    
  _back_history_menu.popup(sigc::mem_fun(*this,&MQWorkArea::get_back_pos),0, 0);
}


void MQWorkArea::next_button_clicked()
{
  _next_history_menu.popup(sigc::mem_fun(*this,&MQWorkArea::get_next_pos),0, 0);
}


void MQWorkArea::goback_clicked()
{
  MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
  MQResultSetView *rset= tab->get_active();
  Glib::ustring query;
  
  query= rset->go_history_back();

  if (!query.empty())
  {
    //rset->execute_query(query, true);
    _query_buffer->set_text(query);
    update_history_menu(rset);
  }
}


void MQWorkArea::gonext_clicked()
{
  MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
  MQResultSetView *rset= tab->get_active();
  Glib::ustring query;
  
  query= rset->go_history_next();

  if (!query.empty())
  {
    //rset->execute_query(query, true); 
    _query_buffer->set_text(query);
    update_history_menu(rset);
  }
}


void MQWorkArea::bookmark_current_query()
{
  if (get_tab()->get_type() == TResultSet)
  {
    MQResultTab *tab= static_cast<MQResultTab*>(get_tab());
    MQResultSetView *rset= tab->get_active();

    if (!rset->get_shown_query().empty())
      _mainw->bookmark_query(_dispatcher->get_current_catalog(),
                             _dispatcher->get_current_schema(),
                             rset->get_shown_query());
  }
}

void MQWorkArea::execute_query_raw(const Glib::ustring &query)
{
  _query_buffer->set_text(query);
  execute_clicked(false);
}

void MQWorkArea::execute_query(const Glib::ustring &catalog, const Glib::ustring &schema,
                               const Glib::ustring &query, MQResultSetView *rset,
                               bool refresh)
{
  MQResultTab *tab= 0;

  if (!rset)
  {
    if (get_tab()->get_type() == TResultSet)
    {
      tab= static_cast<MQResultTab*>(get_tab());
      rset= tab->get_active();
    }
  }

  if (rset && !rset->is_busy())
  {
    Gtk::TreeIter iter= _table_browser->find_table(catalog, schema);

    rset->set_active(true);
    if (iter)
      _table_browser->set_selection(iter);
    _query_buffer->set_text(query);
    text_changed();
    execute_clicked(refresh);
  }
}


void MQWorkArea::open_bookmark(MQBookmarks::BookmarkItem *bookmark)
{
  execute_query(bookmark->catalog, bookmark->schema, bookmark->query, false);
}


void MQWorkArea::open_history(std::string id)
{
  MYX_HISTORY_ENTRY *hist= _history->find_entry(id);

  if (hist)
    execute_query(hist->catalog?:"", hist->schema?:"", hist->sql, 0, false);
  else
    g_message("history entry %s not found", id.c_str());
}


//bool MQWorkArea::get_schema_from_selection_data(const GtkSelectionData& selection_data,
bool MQWorkArea::get_schema_from_selection_data(const Gtk::SelectionData& selection_data,
                                                Glib::ustring &ucatalog,
                                                Glib::ustring &uschema,
                                                Glib::ustring &uquery)
{
  //if ((selection_data->length >= 0) && (selection_data->format == 8))
  if ((selection_data.get_length() >= 0) && (selection_data.get_format() == 8))
  {
    //char *data= g_strndup((const char*)selection_data->data, selection_data->length);
    char *data= g_strndup((const char*)selection_data.get_data(), selection_data.get_length());
    char *catalog, *schema, *query;

    parse_drop_query_data(data, catalog, schema, query);

    if (catalog && schema && query)
    {
      ucatalog= catalog;
      uschema= schema;
      uquery= query;
      g_free(data);

      return true;
    }
    g_free(data);
  }
  return false;
}


void MQWorkArea::table_add_dropped(MYX_Q_TABLE_ADD_TYPE type)
{
  Glib::ustring query;
  Glib::ustring result;
  Gtk::TreeIter iter;
  int cursor_pos;
  
  query= _query_buffer->get_text();

  iter= _table_browser->get_selected();
  if (iter)
  {
    MYX_Q_TABLE_ADD_ERROR error;
    
    query= myx_query_add_table_to_sql(_dispatcher->get_mysql(),
                                      _dispatcher->get_current_schema().c_str(),
                                      _table_browser->get_catalog(iter).c_str(),
                                      _table_browser->get_schema(iter).c_str(),
                                      _table_browser->get_table(iter).c_str(),
                                      query.c_str(),
                                      type,
                                      &cursor_pos,
                                      &error);
    
    switch (error)
    {
    case MYX_QC_TABLES_WITHOUT_ALIAS:
      _mainw->set_status(_("Can't merge table to the query because it's missing an alias."));
      break;
    case MYX_QC_TABLES_CAN_NOT_BE_JOINED:
      _mainw->set_status(_("The tables cannot be joined."));
      break;
    default:
      break;
    }
    _query_buffer->set_text(query);
    if (cursor_pos >= 0)
    {
      Gtk::TextIter iter= _query_buffer->get_iter_at_offset(cursor_pos);
      _query_buffer->place_cursor(iter);
    }
    _xml->get_text("query_text")->grab_focus();
  }
}


void MQWorkArea::column_add_dropped(MYX_Q_CLAUSE_TYPE type)
{
  Glib::ustring query;
  Glib::ustring result;
  Gtk::TreeIter iter;
  int cursor_pos;
  
  query= _query_buffer->get_text();

  iter= _table_browser->get_selected();
  if (iter)
  {    
    query= myx_query_add_column_to_sql(_dispatcher->get_mysql(),
                                       // _dispatcher->get_current_schema().c_str(),
                                       _dispatcher->get_mysql()->db,
                                       _table_browser->get_catalog(iter).c_str(),
                                       _table_browser->get_schema(iter).c_str(),
                                       _table_browser->get_table(iter).c_str(),
                                       _table_browser->get_column(iter).c_str(),
                                       query.c_str(), type, &cursor_pos);
    _query_buffer->set_text(query);
    if (cursor_pos >= 0)
    {
      Gtk::TextIter iter= _query_buffer->get_iter_at_offset(cursor_pos);
      _query_buffer->place_cursor(iter);
    }
    _xml->get_text("query_text")->grab_focus();
  }
}


void MQWorkArea::object_add_menu_drag_leave(const Glib::RefPtr<Gdk::DragContext>& context, guint time)
{
  _inside_menu= false;
  Glib::signal_timeout().connect(sigc::mem_fun(*this,&MQWorkArea::hide_if_needed), 100);
}


bool MQWorkArea::object_add_menu_drag_motion(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, guint time)
{
  _inside_menu= true;
  return false;
}


void MQWorkArea::show_table_add_menu(Gtk::Widget *view, bool below)
{
  int x,y;
  int min_width;

  if (_table_add_xml->get_widget("clause_menu")->is_mapped())
    return;

  view->get_window()->get_origin(x,y);
//  view->translate_coordinates(*get_toplevel(), x,y, x,y);
  if (!below)
    y+= view->get_height()-2;

  _table_add_xml->get_widget("clause_menu")->show();
  min_width= _table_add_xml->get_widget("clause_menu")->get_width();
  _table_add_xml->get_widget("clause_menu")->set_size_request(std::max(min_width,view->get_width()), 25);
  _table_add_xml->get_widget("clause_menu")->get_window()->move(x,y);
}


static void show_only(MGGladeXML *xml, const char **all, const char **show)
{
  for (; *all; all++)
  {
    const char **tmp;
    for (tmp= show; *tmp; tmp++)
    {
      if (strcmp(*tmp, *all)==0)
        break;
    }
    if (!*tmp)
      xml->get_widget(*all)->hide();
  }
  for (; *show; show++)
  {
    xml->get_widget(*show)->show();
  }
}


void MQWorkArea::show_column_add_menu(Gtk::Widget *view, bool below)
{
  int x,y;
  int min_width;
  static const char *all_items[]= {
    "from_c",
      "from_s",
      "where_c",
      "where_s",
      "group_c",
      "group_s",
      "having_c",
      "having_s",
      "order_c",
      "order_s",
      "limit_c",
      "limit_s",
      "set_c",
      "set_s",
      "into_c",
      "into_s",
      "update_c",
      "update_s",
      "delete_c",
      "delete_s",
      "using_c",
      NULL
  };
  static const char *select_items[]= {
      "select_c",
      "select_s",
      "from_c",
      "from_s",
      "where_c",
      "where_s",
      "group_c",
      "group_s",
      "having_c",
      "having_s",
      "order_c",
      "order_s",
      "using_c",
      NULL
  };
  static const char *update_items[]= {
    "update_c",
      "update_s",
      "from_c",
      "from_s",
      "where_c",
      "where_s",
      "set_c",
      "set_s",
      NULL
  };
  static const char *insert_items[]= {
    "into_c",
      "into_s",
      "from_c",
      "from_s",
      "where_c",
      "where_s",
      NULL
  };
  static const char *delete_items[]= {
    "delete_c",
      "delete_s",
      "from_c",
      "from_s",
      "where_c",
      "where_s",
      NULL
  };

  if (_column_add_xml->get_widget("column_menu")->is_mapped())
    return;

  
  switch (myx_query_type(_query_buffer->get_text().c_str()))
  {
  case MYX_QT_SELECT:
  case MYX_QT_SELECT_INTO_OUTFILE:
    show_only(_column_add_xml, all_items, select_items);
    break;
  case MYX_QT_UPDATE:
    show_only(_column_add_xml, all_items, update_items);
    break;
  case MYX_QT_INSERT:
    show_only(_column_add_xml, all_items, insert_items);
    break;
  case MYX_QT_DELETE:
    show_only(_column_add_xml, all_items, delete_items);
    break;
  default:
    return;
  }

  view->get_window()->get_origin(x,y);
//  view->translate_coordinates(*get_toplevel(), x,y, x,y);
  if (!below)
    y+= view->get_height()-2;

  _column_add_xml->get_widget("column_menu")->show();
  min_width= _column_add_xml->get_widget("column_menu")->get_width();
  _column_add_xml->get_widget("column_menu")->set_size_request(std::max(min_width,view->get_width()), 25);
  _column_add_xml->get_widget("column_menu")->get_window()->move(x,y);
}


bool MQWorkArea::hide_if_needed()
{
  if (!_inside_query_text && !_inside_menu)
  {
    _table_add_xml->get_widget("clause_menu")->hide();
    _column_add_xml->get_widget("column_menu")->hide();
  }
  return false;
}

void MQWorkArea::query_drag_leave(const Glib::RefPtr<Gdk::DragContext>& context, guint time)
{
  _inside_query_text= false;
  Glib::signal_timeout().connect(sigc::mem_fun(*this,&MQWorkArea::hide_if_needed), 100);
}


bool MQWorkArea::query_drag_motion(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, guint time)
{
  Gtk::TreeIter iter= _table_browser->get_selected();
  _inside_query_text= true;
  if (iter)
  {
    if (_table_browser->get_type(iter) == MGTableBrowserList::Table)
      show_table_add_menu(_xml->get_widget("query_text"), false);
    else if (_table_browser->get_type(iter) == MGTableBrowserList::Column)
      show_column_add_menu(_xml->get_widget("query_text"), false);
  }
  return true;
}


//void MQWorkArea::query_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, GtkSelectionData* selection_data, guint info, guint time)
void MQWorkArea::query_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData& selection_data, guint info, guint time)
{
  // this is to avoid a duplicated drop event we're getting
  if (time <= _last_drop_time)
    return;
  _last_drop_time= time;

  //if (selection_data->target==Gdk::AtomStringTraits::to_c_type("x-mysqlgui-query"))
  if (const_cast<GtkSelectionData*>(selection_data.gobj())->target == Gdk::AtomStringTraits::to_c_type("x-mysqlgui-query"))
  {
    if (get_tab()->get_type() == TResultSet)
    {
      Glib::ustring catalog, schema, query;
      
      if (get_schema_from_selection_data(selection_data, catalog, schema, query))
      {
        execute_query(catalog, schema, query);
      }
    }
    context->drag_finish(false, false, time);
  }
  //else if (selection_data->target==Gdk::AtomStringTraits::to_c_type("x-mysqlgui-schema-object"))
  else if (const_cast<GtkSelectionData*>(selection_data.gobj())->target == Gdk::AtomStringTraits::to_c_type("x-mysqlgui-schema-object"))
  {
    Glib::ustring query;
    int cursor_pos;
    int x,y;
    Gtk::TextIter iter;
    Gdk::ModifierType mod;
    Gdk::Display::get_default()->get_pointer(x,y,mod);

    query= _query_buffer->get_text();
    query= interactive_modify_sql(query, mod, cursor_pos,
                                  _xml->get_text("query_text"));
    
    _query_buffer->set_text(query);
    if (cursor_pos >= 0)
    {
      iter= _query_buffer->get_iter_at_offset(cursor_pos);
      _query_buffer->place_cursor(iter);
    }
    _xml->get_text("query_text")->grab_focus();

    context->drag_finish(false, false, time);
  }
}


//======================================================================
// Script Editor
//======================================================================


bool MQWorkArea::script_editor_key_press(GdkEventKey *ev)
{
  if ((ev->state & GDK_CONTROL_MASK) && 
      (ev->keyval == GDK_Page_Up || ev->keyval == GDK_Page_Down))
  {
    if (ev->keyval == GDK_Page_Down)
      _note->next_page();
    else
      _note->prev_page();
    return true;
  }
  return false;
}


MQScriptEditorTab *MQWorkArea::add_script_view(const Glib::ustring &title)
{
  MQScriptEditorTab *tab= Gtk::manage(new MQScriptEditorTab(_dispatcher));
  MQSQLCodeEditor *editor= tab->get_editor();

  tab->signal_script_finished().connect(sigc::mem_fun(*this,&MQWorkArea::script_editor_script_finished));
  tab->signal_state_changed().connect(sigc::mem_fun(*this,&MQWorkArea::script_editor_state_changed));
  editor->signal_cursor_moved().connect(sigc::mem_fun(*this,&MQWorkArea::update_cursor_position));
  editor->signal_changed().connect(sigc::mem_fun(*this,&MQWorkArea::script_edited));

  script_edited(false);

  tab->set_title(title);

  add_tab(tab);
  
  editor->get_wrapper()->signal_key_press_event().connect(sigc::mem_fun(*this,&MQWorkArea::script_editor_key_press), false);

  return tab;
}


void MQWorkArea::update_cursor_position()
{
  int line, column;

  if (get_tab() && get_tab()->get_type() == TScriptEditor)
  {
    dynamic_cast<MQScriptEditorTab*>(get_tab())->get_editor()->get_cursor(line, column);

    _mainw->set_cursor(line, column);
  }
}

void MQWorkArea::execute_script_selection()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
  {
    if (tab->has_selection())
      tab->execute_selection();
    else
      myg_show_error(_("You must select the statements you wish to execute."));
  }
}

void MQWorkArea::execute_script_stepping()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
    tab->execute_script(true);
}

void MQWorkArea::execute_script()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
    tab->execute_script();
}


void MQWorkArea::continue_script()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
    tab->continue_script();
}


void MQWorkArea::step_over()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  
  if (tab)
    tab->step_over();
}


void MQWorkArea::toggle_breakpoint()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  
  if (tab)
    tab->toggle_breakpoint();
}


void MQWorkArea::clear_breakpoints()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  
  if (tab)
    tab->clear_breakpoints();
}


void MQWorkArea::run_until_return()
{
}


void MQWorkArea::reset_script()
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
    tab->reset();
}


void MQWorkArea::stop_script(bool pause_only)
{
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  if (tab)
    tab->stop_script(pause_only);
}


void MQWorkArea::load_script_into()
{
  Gtk::FileSelection dlg(_("Open Script"));

  if (dlg.run()==Gtk::RESPONSE_OK)
  {
    MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
    dlg.hide();
    
    if (tab->get_editor()->get_dirty())
    {
      tab= add_script_view("Script");
    }

    tab->load_file(dlg.get_filename());
  }
}


void MQWorkArea::load_script_mi()
{
  Gtk::FileSelection dlg(_("Open Script"));

  if (dlg.run()==Gtk::RESPONSE_OK)
  {
    dlg.hide();
    load_script(dlg.get_filename());
  }
}


void MQWorkArea::load_script(const std::string &file)
{
  MQScriptEditorTab *tab= add_script_view("Script");
  int rc;

  _mainw->set_status(_("Loading script..."));
  rc= tab->load_file(file);
  if (rc > 0)
  {
    _mainw->_state->add_recent_file("script://"+file);
    refresh_recent_menu();
    _mainw->set_status(ufmt(_("Script loaded from file '%s'."), file.c_str()));
  }
  else if (rc < 0)
    _mainw->set_status(_("Error loading script."));
  else
    _mainw->set_status(_("Cancelled."));
}


void MQWorkArea::save_script(bool save_as)
{
  Glib::ustring data;
  MQScriptEditorTab *tab= dynamic_cast<MQScriptEditorTab*>(get_tab());
  MQSQLCodeEditor *editor= tab->get_editor();
  
  if (tab->get_file_name().empty() || save_as)
  {
    Gtk::FileSelection dlg(_("Save Script"));
    
    if (dlg.run()!=Gtk::RESPONSE_OK)
      return;

    tab->set_file_name(dlg.get_filename());
    
    _mainw->_state->add_recent_file(tab->get_file_name());
    refresh_recent_menu();
  }
  
  _mainw->set_status(ufmt(_("Saving %s..."), tab->get_file_name().c_str()));
  data= editor->get_text();
  
  Glib::RefPtr<Glib::IOChannel> file;
  
  file= Glib::IOChannel::create_from_file(tab->get_file_name().c_str(), "w+");
  file->set_encoding();
  
  if (file->write(data) != Glib::IO_STATUS_NORMAL)
  {
    myg_show_sys_error(ufmt(_("Could not save to file '%s'."), tab->get_file_name().c_str()), errno);
  }
  file->close();

  _mainw->set_status(_("Script saved."));
  tab->get_editor()->set_dirty(false);
}


void MQWorkArea::script_edited(bool flag)
{
  _xml->get_button("save_btn")->set_sensitive(flag);
}


void MQWorkArea::script_editor_state_changed(MQScriptEditorTab *sender,
                                             MQScriptEditorTab::State state)
{
  switch (state)
  {
  case MQScriptEditorTab::SRunning:
    _mainw->set_status(_("Executing script..."));
    _xml->get_button("continue_btn")->set_sensitive(false);
    _xml->get_button("continue_sbtn")->set_sensitive(false);
    _xml->get_button("sstop_btn")->set_sensitive(true);
    _xml->get_button("sstop_sbtn")->set_sensitive(true);
    _xml->get_button("sdoexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_sbtn")->set_sensitive(false);
    _xml->get_button("step_btn")->set_sensitive(false);
    _xml->get_button("step_sbtn")->set_sensitive(false);
    set_script_busy_animation(true);
    break;
  case MQScriptEditorTab::SPaused:
  case MQScriptEditorTab::SWaiting:
    if (state == MQScriptEditorTab::SWaiting)
      _mainw->set_status(_("Executed statement."));
    else
      _mainw->set_status(_("Execution interrupted."));
    _xml->get_button("continue_btn")->set_sensitive(true);
    _xml->get_button("continue_sbtn")->set_sensitive(true);
    _xml->get_button("sstop_btn")->set_sensitive(true);
    _xml->get_button("sstop_sbtn")->set_sensitive(true);
    _xml->get_button("sdoexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_sbtn")->set_sensitive(false);
    _xml->get_button("step_btn")->set_sensitive(true);
    _xml->get_button("step_sbtn")->set_sensitive(true);
    set_script_busy_animation(false);
    break;
  case MQScriptEditorTab::SIdle:
    _mainw->set_status("");
    _xml->get_button("continue_btn")->set_sensitive(false);
    _xml->get_button("continue_sbtn")->set_sensitive(false);
    _xml->get_button("sstop_btn")->set_sensitive(false);
    _xml->get_button("sstop_sbtn")->set_sensitive(false);
    _xml->get_button("sdoexecute_btn")->set_sensitive(true);
    _xml->get_button("sexecute_btn")->set_sensitive(true);
    _xml->get_button("sexecute_sbtn")->set_sensitive(true);
    _xml->get_button("step_btn")->set_sensitive(false);
    _xml->get_button("step_sbtn")->set_sensitive(false);
    set_script_busy_animation(false);
    break;
  case MQScriptEditorTab::SBreakpoint:
    _mainw->set_status(_("Breakpoint."));
    _xml->get_button("continue_btn")->set_sensitive(true);
    _xml->get_button("continue_sbtn")->set_sensitive(true);
    _xml->get_button("sstop_btn")->set_sensitive(true);
    _xml->get_button("sstop_sbtn")->set_sensitive(true);
    _xml->get_button("sdoexecute_btn")->set_sensitive(false);
    _xml->get_button("sdoexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_btn")->set_sensitive(false);
    _xml->get_button("step_btn")->set_sensitive(true);
    _xml->get_button("step_sbtn")->set_sensitive(true);
    set_script_busy_animation(false);
    break;
  case MQScriptEditorTab::SError:
    _mainw->set_status(_("Error during script execution"));
    _xml->get_button("continue_btn")->set_sensitive(true);
    _xml->get_button("continue_sbtn")->set_sensitive(true);
    _xml->get_button("sstop_btn")->set_sensitive(true);
    _xml->get_button("sstop_sbtn")->set_sensitive(true);
    _xml->get_button("sexecute_btn")->set_sensitive(false);
    _xml->get_button("sexecute_sbtn")->set_sensitive(false);
    _xml->get_button("step_btn")->set_sensitive(true);
    _xml->get_button("step_sbtn")->set_sensitive(true);
    set_script_busy_animation(false);
    break;
  }
}


void MQWorkArea::script_editor_script_finished(MQScriptEditorTab *sender)
{
  _mainw->set_status(_("Script executed."));
}
