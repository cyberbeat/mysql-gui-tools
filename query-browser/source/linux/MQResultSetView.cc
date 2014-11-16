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


#include <sys/time.h>

#include <gtkmm/fileselection.h>
#include <gtkmm/stock.h>
#include <gtkmm/messagedialog.h>

#include "MQResultSetView.h"
#include "myg_gtkutils.h"

#include "MQQueryDispatcher.h"
#include "myx_public_interface.h"
#include "MGResultSetModel.h"
#include "MQHistory.h"
#include "MGCellRendererBlob.h"
#include "MGCellRendererText.h"
#include "MGBlobEditor.h"
#include "MQIndicatorCellRenderer.h"

#include "MQQueryParameters.h"


bool MQResultSetView::PopupTreeView::on_button_press_event(GdkEventButton *ev)
{
  bool return_value= true;
  
  MQResultSetView *rsv= (MQResultSetView*)get_data("owner");
  if (rsv)
    rsv->set_active(true);

  // make the tree ignore right mouse buttons
  // to avoid annoying behaviour when rows are editable or draggable
  // and we want a popup menu
  if ((ev->type != GDK_BUTTON_PRESS) || (ev->button != 3))
  {
    return_value = TreeView::on_button_press_event(ev);
  }
  else
  {
    Gtk::TreePath path;
    Gtk::TreeViewColumn *column;
    int cx, cy;

    if (get_path_at_pos((gint)ev->x, (gint)ev->y,
                    path, column,
                    cx, cy))
      set_cursor(path, *column, false);
  }

  if (_popup)
  {
    _signal_will_popup.emit();
    
    if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
      _popup->popup(ev->button, ev->time);
  }

  return return_value;
}


bool MQResultSetView::show_menu(GdkEventButton *ev)
{  
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    update_menu();
    _edit_menu.popup(ev->button, ev->time);
  }
  else if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 1))
    set_active(true);
  
  return false;
}


bool MQResultSetView::tree_key_released(GdkEventKey *event)
{
  Gtk::TreeViewColumn *column= 0, *previous= 0;
  Gtk::TreePath path;

  _tree->get_cursor(path, column);

  if (event->keyval == GDK_Tab && column)
  {
    std::list<Gtk::TreeViewColumn*> columns= _tree->get_columns();
    bool found= false;

    if (event->state & GDK_SHIFT_MASK)
    {
      std::list<Gtk::TreeViewColumn*>::const_iterator iter= columns.begin();
      ++iter;

      for (; iter != columns.end(); ++iter)
      {
        if (*iter == column)
        {
          found= true;
          break;
        }
        previous= *iter;
      }
      column= previous;
      
      if (!found)
        path.prev();
      _tree->set_cursor(path, *column, true);
    }
    else
    {
      for (std::list<Gtk::TreeViewColumn*>::const_iterator iter= columns.begin();
           iter != columns.end(); ++iter)
      {
        if (previous == column)
        {
          found= true;
          column= *iter;
          break;
        }
        previous= *iter;
      }

      if (!found)
      {
        path.next();
        _tree->set_cursor(path, *_tree->get_column(1), true);
        _tree->scroll_to_cell(path, *_tree->get_column(1));
      }
      else
      {
        _tree->set_cursor(path, *column, true);
        _tree->scroll_to_cell(path, *column);
      }
    }
    return true;
  }
  return false;
}

MQResultSetView::MQResultSetView(MQQueryDispatcher *disp,
                                 MQHistory *history,
                                 MQGlobalQueryParameters *params)
  : _box(false, 0),
    _columns(0),
    _tree(0),
    _hbox(false, 0),
    _status_label("", 0.0, 0.5),
    _explain_window(0),
    _result(0),
    _dispatcher(disp),
    _history_list(history),
    _history_index(0),
    _is_busy(false),
    _compact_mode(false),
    dispose_if_no_result(false)
{
  add(_box);
  _box.show();
  
  signal_button_press_event().connect(sigc::mem_fun(*this,&MQResultSetView::show_menu));

  _params= new MQQueryParameters(params);
  
  _box.pack_start(_scroll, true, true);
  _scroll.set_shadow_type(Gtk::SHADOW_IN);
  _scroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);

  {
    Gdk::Color color1("white"), color2("#f0f0f0");
  
    get_colormap()->alloc_color(color1);
    get_colormap()->alloc_color(color2);

    _ebox.modify_bg(Gtk::STATE_NORMAL, color1);
    _ebox.modify_bg(Gtk::STATE_INSENSITIVE, color2);
  }
  _status_label.set_padding(4, 0);

  _status_label.set_text("");

  _ebox.set_name("resultset_bar");
  
  _ebox.signal_button_press_event().connect(sigc::mem_fun(*this,&MQResultSetView::editbar_clicked));

  _edit_btn.property_can_focus()= false;
  _apply_btn.property_can_focus()= false;

  _first_btn.property_can_focus()= false;
  _last_btn.property_can_focus()= false;
  _search_btn.property_can_focus()= false;
  
  _edit_btn.signal_clicked().connect(sigc::mem_fun(*this,&MQResultSetView::edit_start));
  _apply_btn.signal_clicked().connect(sigc::mem_fun(*this,&MQResultSetView::edit_apply));
  _first_btn.signal_clicked().connect(sigc::mem_fun(*this,&MQResultSetView::go_first));
  _last_btn.signal_clicked().connect(sigc::mem_fun(*this,&MQResultSetView::go_last));
    
  _box.pack_start(_ebox, false, false);
  _ebox.add(_hbox);
  _hbox.pack_start(_status_label, true, true);
  _hbox.pack_start(_sep1, false, false);
  _hbox.pack_start(_edit_btn, false, false);
  _hbox.pack_start(_sep1a, false, false);
  _hbox.pack_start(_apply_btn, false, false);
  _hbox.pack_start(_sep1b, false, false);
  _hbox.pack_start(_sep2, false, false);
  _hbox.pack_start(_first_btn, false, false);
  _hbox.pack_start(_sep2a, false, false);
  _hbox.pack_start(_last_btn, false, false);
  _hbox.pack_start(_sep2b, false, false);
  _hbox.pack_start(_search_btn, false, false);

  _edit_btn.set_relief(Gtk::RELIEF_NONE);
  myg_make_image_button(_edit_btn, PIXCACHE->load("rs_edit_disabled.png"),
                        _("Start Editing"));
  _apply_btn.set_relief(Gtk::RELIEF_NONE);
  myg_make_image_button(_apply_btn, PIXCACHE->load("rs_apply_disabled.png"),
                        _("Apply Changes"));
  
  _first_btn.set_relief(Gtk::RELIEF_NONE);
  myg_make_image_button(_first_btn, PIXCACHE->load("rs_first.png"),
                        _("First"));
  _last_btn.set_relief(Gtk::RELIEF_NONE);
  myg_make_image_button(_last_btn, PIXCACHE->load("rs_last.png"),
                        _("Last"));
  _search_btn.set_relief(Gtk::RELIEF_NONE);
  myg_make_image_button(_search_btn, PIXCACHE->load("rs_search.png"),
                        _("Search"));
  
  set_sensitive(false);
  
  show_all();

  // result menu
  myg_menu_add(_edit_menu, Gtk::Stock::ADD, _("Add Row"),
               sigc::mem_fun(*this,&MQResultSetView::add_row_mi),
               "add_row");
//  myg_menu_add(_edit_menu, Gtk::Stock::UNDO, _("Undo Last Edition"),
//               sigc::mem_fun(*this,&MQResultSetView::undo_row_mi),
//               "undo");

  myg_menu_add(_edit_menu, Gtk::Stock::REMOVE, _("Delete Row"),
               sigc::mem_fun(*this,&MQResultSetView::delete_row_mi),
               "delete_row");

  myg_menu_add(_edit_menu);

  // column stuff
  myg_menu_add(_edit_menu, Gtk::Stock::OPEN, _("_Load Field Content"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldLoad),
               "load_field");
  myg_menu_add(_edit_menu, Gtk::Stock::SAVE, _("_Save Field Content"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldSave),
               "save_field");
  myg_menu_add(_edit_menu, Gtk::Stock::COPY, _("_Copy Field Content"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldCopy),
               "copy_field");
  myg_menu_add(_edit_menu, Gtk::Stock::CLEAR, _("Set Field to _NULL"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldClear),
               "clear_field");
  myg_menu_add(_edit_menu);
  myg_menu_add(_edit_menu, _("_View Field in Popup Editor"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldView),
               "view_field");
  myg_menu_add(_edit_menu, _("_Edit Field in Popup Editor"),
               sigc::bind<FieldAction>(sigc::mem_fun(*this,&MQResultSetView::column_action_mi), FieldEdit),
               "edit_field");
  
  
  // the error box
  _msgscroll.add(_msgtree);
  _msgtree.show();
  _msgscroll.set_policy(Gtk::POLICY_AUTOMATIC,Gtk::POLICY_AUTOMATIC);
  _msgscroll.set_shadow_type(Gtk::SHADOW_IN);
  _msgstore= Gtk::ListStore::create(_msg_columns);
  _msgtree.append_column("", _msg_columns.icon);
  _msgtree.append_column("errno", _msg_columns.errornum);
  _msgtree.append_column("Message", _msg_columns.message);
  _msgtree.set_headers_visible(false);
  _msgtree.get_column(1)->set_resizable();
  _msgtree.get_column(2)->set_resizable();
  _msgtree.set_model(_msgstore);
  _msgtree.get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQResultSetView::selected_error));

  _box.pack_start(_msgscroll, false, false, 2);
}


MQResultSetView::~MQResultSetView()
{
  delete _columns;
  delete _tree;
  delete _params;
  delete _explain_window;

  if (_result)
    myx_query_free_resultset(_result);
}


void MQResultSetView::set_show_editbar(bool flag)
{
  if (flag)
    _ebox.show();
  else
    _ebox.hide();
}


void MQResultSetView::append_error(int num, const Glib::ustring &msg,
                                   Glib::RefPtr<Gdk::Pixbuf> icon,
                                   int r)
{
  Gtk::TreeIter iter= _msgstore->append();
  Gtk::TreeRow row= *iter;
  Glib::ustring errmsg= msg;

  if (num == 1046)
  {
    errmsg+=_(". Please select one in File->Select Schema");
  }

  row[_msg_columns.icon]= icon;
  row[_msg_columns.message]= errmsg;
  row[_msg_columns.errornum]= num;
  row[_msg_columns.row]= r;

  _msgscroll.set_size_request(100, -1);
  _msgscroll.show();
  _msgtree.set_cursor(_msgstore->get_path(iter));

  if (!prefs.dont_beep)
    Gdk::Display::get_default()->beep();
}

static MYX_Q_TABLE_COLUMN *get_table_column(MYX_RS_COLUMN *rcol)
{
  if (rcol->table)
  {
    for (unsigned int i= 0; i < rcol->table->columns_num; i++)
    {
      if (strcmp(rcol->table->columns[i].column, rcol->name)==0)
        return rcol->table->columns+i;
    }
  }
  return 0;
}

/*
static const char *get_type_name(MYX_RS_COLUMN_TYPE type)
{
  switch (type) 
  {
  case MYX_RSCT_INTEGER: return "integer";
  case MYX_RSCT_FLOAT: return "float";
  case MYX_RSCT_STRING: return "varchar";
  case MYX_RSCT_DATE: return "date";
  case MYX_RSCT_TIME: return "time";
  case MYX_RSCT_DATETIME: return "datetime";
  case MYX_RSCT_BLOB: return "blob";
  case MYX_RSCT_TEXT: return "text";
  case MYX_RSCT_ENUM: return "enum";
  case MYX_RSCT_SET: return "set";
  default: return "";
  }
}
*/

void MQResultSetView::display_result(MYX_RESULTSET *result)
{
  _scroll.remove();
  _model.clear();
  delete _tree; _tree= 0; // when in compare mode
  delete _columns; _columns= 0;

  if (_explain_window)
  {
    delete _explain_window;
    _explain_window= 0;
  }

  if (_result != result)
  {
    if (_result)
      myx_query_free_resultset(_result);
    _result= 0;
  }

  if (result)
  {
    std::vector<Glib::ustring> colors;
    
    _columns= new ModelColumns();
    for (unsigned int i= 0; i < result->columns_num; i++)
    {
      if (result->columns[i].column_type == MYX_RSCT_BLOB
          || result->columns[i].column_type == MYX_RSCT_TEXT)
        _columns->add_blob_column();
      else
        _columns->add_text_column();
    }
    _columns->add_extra_columns();

    _tree= new PopupTreeView(&_edit_menu);
    
    _tree->signal_key_release_event().connect(sigc::mem_fun(*this,&MQResultSetView::tree_key_released));
    _tree->set_data("owner", this);

    _model= MGResultSetModel::create(*_columns, result);

    colors.push_back("#cc5555"); // deleted
    colors.push_back("#55bb55"); // added
    colors.push_back("#cccc55"); // modified
    colors.push_back("#eeffee"); // placeholder for new row
    _model->set_state_colors(colors);

    colors.clear();
    colors.push_back("#ccaaaa"); // only in other
    colors.push_back("#aaccaa"); // only in this
    colors.push_back("#aabbcc"); // different
    _model->set_diff_colors(colors);


    _result= result;

    _scroll.add(*_tree);

    _tree->set_model(_model);
    _tree->show();

    _tree->set_rules_hint();

    // add indicator column
    Gtk::TreeViewColumn *col;
    MQIndicatorCellRenderer *irend= Gtk::manage(new MQIndicatorCellRenderer);
    irend->property_active()= _active;
    col= new Gtk::TreeView::Column("", *irend);
    _tree->append_column(*Gtk::manage(col));
    
    col->set_min_width(20);
    col->set_fixed_width(20);

    
    unsigned int text_idx= 0;
    unsigned int blob_idx= 0;
    
    for (unsigned int i= 0; i < result->columns_num_to_display; i++)
    {
//      Glib::ustring column_info;
      
      if (result->columns[i].column_type == MYX_RSCT_BLOB
          || result->columns[i].column_type == MYX_RSCT_TEXT)
      {
        CellRendererBlob *rend= Gtk::manage(new CellRendererBlob(result->columns[i].column_type == MYX_RSCT_BLOB));

        rend->set_overlay_icons(PIXCACHE->load("field_overlay_clear.png"),
                                PIXCACHE->load("field_overlay_edit.png"),
                                PIXCACHE->load("field_overlay_load.png"),
                                PIXCACHE->load("field_overlay_save.png"),
                                PIXCACHE->load("field_overlay_view.png"),
                                PIXCACHE->load("field_overlay_null.png"));
        rend->set_blob_icon(PIXCACHE->load("blob_icon.png"));
        rend->set_max_text_width(prefs.max_blob_length);
        rend->property_column()= i;
        rend->set_delegate(this);

        _tree->append_column("", *rend);
        col= _tree->get_column(i+1);

        col->add_attribute(rend->property_field(), *_columns->blobs[blob_idx++]);
        col->add_attribute(rend->property_background(), *_columns->bgcolors[i]);
        // set the cell to be editable when the model's editable flag is set
        col->add_attribute(rend->property_editable(), _columns->editable);
      }
      else
      {
        MGCellRendererText *rend= Gtk::manage(new MGCellRendererText(PIXCACHE->load("field_overlay_null.png")));

        // make the row height fixed, to speed up things when there are
        // many rows
        rend->set_fixed_height_from_font(1);

        _tree->append_column("", *rend);
        col= _tree->get_column(i+1);

        rend->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MQResultSetView::string_data_edited), (i)*2));

        col->add_attribute(rend->property_text(), *_columns->columns[text_idx++]);
        col->add_attribute(rend->property_background(), *_columns->bgcolors[i]);
        // set the cell to be editable when the model's editable flag is set
        col->add_attribute(rend->property_editable(), _columns->editable);
      }

      col->set_resizable();

      MYX_Q_TABLE_COLUMN *tcol= get_table_column(result->columns+i);

      if (tcol && tcol->is_pk)
      {
        Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(false, 0));
        hbox->pack_start(*Gtk::manage(new Gtk::Image(PIXCACHE->load("16x16_KeyColumn.png"))));
        hbox->pack_start(*Gtk::manage(new Gtk::Label(result->columns[i].name)));
        hbox->show_all();
        col->set_widget(*hbox);
      }
      else
      {
        col->set_widget(*Gtk::manage(new Gtk::Label(result->columns[i].name)));
      }

      /*
      column_info= ufmt("Column: %s\nTable: %s\nType: %s(%i)",
                        result->columns[i].name,
                        result->columns[i].table->name,
                        get_type_name(result->columns[i].column_type),
                        result->columns[i].type_size);
      
      _tip.set_tip(*col->get_widget(), column_info);
       */
      col->get_widget()->show();
    }
    _tree->get_selection()->set_mode(Gtk::SELECTION_BROWSE);
    _tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MQResultSetView::selected_row));
    go_first();
    selected_row();

    _tree->signal_will_popup().connect(sigc::mem_fun(*this,&MQResultSetView::update_menu));
    
    _tree->signal_focus_in_event().connect(sigc::mem_fun(*this,&MQResultSetView::focused));
  }
}


void MQResultSetView::redisplay()
{
  MQResultSetView::display_result(_result);
}


void MQResultSetView::display_errors(MYX_MYSQL_ERROR_MSGS *errors)
{
  if (!errors)
    return;

  for (unsigned int i= 0; i < errors->errors_num; i++)
  {
    switch (errors->errors[i].level)
    {
    case MYX_QEL_ERROR:
      append_error(errors->errors[i].error,
                   errors->errors[i].text, 
                   PIXCACHE->load("rs_error.png"));
      break;
    case MYX_QEL_WARNING:
      append_error(errors->errors[i].error,
                   errors->errors[i].text, 
                   PIXCACHE->load("rs_warning.png"));
      break;
    case MYX_QEL_NOTE:
      append_error(errors->errors[i].error,
                   errors->errors[i].text, 
                   PIXCACHE->load("rs_notice.png"));
      break;
    }
  }
}


void MQResultSetView::cancel_query()
{
  _current_query_cancelled= true;
}


void MQResultSetView::execute_query(const Glib::ustring &query,
                                    bool reexecute)
{
  char *arg;

  if (!reexecute)
    add_history_entry(query);

  if ((arg= myx_parse_sqlmemo_command_use(query.c_str())))
  {
    _status_label.set_text(_("Changing Default Schema..."));
    if (_dispatcher->select_schema(_dispatcher->get_current_catalog(), arg))
      _status_label.set_text(_("Default Schema Changed."));
    else
    {
      _status_label.set_text(_("Error changing default schema."));
      myg_show_mysql_error(ufmt(_("Error changing default schema to '%s': "),
                                 arg), _dispatcher->get_mysql());
    }
    g_free(arg);

    return;
  }
  else if (myx_parse_sqlmemo_command_transaction_start(query.c_str()))
  {
    _dispatcher->start_transaction();
    return;
  }
  else if (myx_parse_sqlmemo_command_transaction_commit(query.c_str()))
  {
    _dispatcher->commit_transaction();
    return;
  }
  else if (myx_parse_sqlmemo_command_transaction_rollback(query.c_str()))
  {
    _dispatcher->rollback_transaction();
    return;
  }    

  _is_busy= true;
  _current_query= query;
  _current_query_cancelled= false;
  gettimeofday(&_current_query_start, NULL);

  _status_label.set_text(_("Executing query..."));

  set_sensitive(false);

  _query_started_signal.emit(this, false);

  _dispatcher->start_query(query, _params->get_all(),
                           sigc::mem_fun(*this,&MQResultSetView::query_finished),
                           sigc::mem_fun(*this,&MQResultSetView::query_progress),
                           sigc::mem_fun(*this,&MQResultSetView::query_error),
                           sigc::mem_fun(*this,&MQResultSetView::query_fetch_more),
                           NULL);
}


void MQResultSetView::fetch_more(MYSQL *mysql)
{
  _is_busy= true;
  _current_query= "";
  _current_query_cancelled= false;
  gettimeofday(&_current_query_start, NULL);

  _status_label.set_text(_("Executing query..."));

  set_sensitive(false);

  _query_started_signal.emit(this, false);

  _dispatcher->start_query("", NULL,
                           sigc::mem_fun(*this,&MQResultSetView::query_finished),
                           sigc::mem_fun(*this,&MQResultSetView::query_progress),
                           sigc::mem_fun(*this,&MQResultSetView::query_error),
                           sigc::mem_fun(*this,&MQResultSetView::query_fetch_more),
                           mysql);
}


bool MQResultSetView::explain_query()
{
  if (_current_query.empty())
    return false;

  _is_busy= true;

  set_sensitive(false);

  _query_started_signal.emit(this, false);

  MYX_EXPLAIN_RESULT *result;

  result= _dispatcher->explain_query(_current_query, _params->get_all());
  
  _query_finished_signal.emit(this);

  _is_busy= false;
  
  set_sensitive(true);

  if (result)
  {
    Gtk::ScrolledWindow *expscroll;
    Gtk::TreeView *exptree;
    Glib::RefPtr<Gtk::TreeStore> expstore;
    Gtk::VBox *vbox;
    Gtk::Button *button= 0;
    Gtk::Label *label;

    delete _explain_window;
    
    _explain_window= new Gtk::Window();
    _explain_window->set_title(_("Explain Query"));
    _explain_window->set_transient_for(*(Gtk::Window*)get_toplevel());

    vbox= Gtk::manage(new Gtk::VBox(false, 8));
    vbox->set_border_width(12);

    label= Gtk::manage(new Gtk::Label(_current_query));
    label->set_line_wrap(true);
    vbox->pack_start(*label, false, false);

    _explain_window->add(*vbox);

    expscroll= Gtk::manage(new Gtk::ScrolledWindow);
    expscroll->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
    expscroll->set_shadow_type(Gtk::SHADOW_IN);
    
    exptree= Gtk::manage(new Gtk::TreeView());
    expscroll->add(*exptree);
    
    exptree->append_column(_("Select Type"), _exp_columns.select_type);
    exptree->append_column(_("Table"), _exp_columns.table);
    exptree->append_column(_("Join Type"), _exp_columns.type);
    exptree->append_column(_("Possible Keys"), _exp_columns.possible_keys);
    exptree->append_column(_("Key"), _exp_columns.key);
    exptree->append_column(_("Key Length"), _exp_columns.key_len);
    exptree->append_column(_("ref"), _exp_columns.ref);
    exptree->append_column(_("Rows to Examine"), _exp_columns.rows);
    exptree->append_column(_("Other Info"), _exp_columns.extra);

    exptree->show();
    
    expstore= Gtk::TreeStore::create(_exp_columns);
    exptree->set_model(expstore);
    
    for (unsigned int i=0; i < result->rows_num; i++)
    {
      MYX_EXPLAIN_ROW *expl= result->rows+i;
      Gtk::TreeIter iter= expstore->append();
      Gtk::TreeRow row= *iter;

      row[_exp_columns.select_type]= expl->select_type?:"";
      row[_exp_columns.table]= expl->table?:"";
      row[_exp_columns.type]= expl->join_type?:"";
      row[_exp_columns.key]= expl->key?:"";
      row[_exp_columns.key_len]= expl->key_len?:"";
      row[_exp_columns.ref]= expl->ref?:"";
      row[_exp_columns.rows]= expl->rows?:"";
      row[_exp_columns.extra]= expl->extra?:"";
      for (unsigned int k=0; k < expl->possible_keys_num; k++)
      {
        Gtk::TreeIter kiter= expstore->append(row.children());
        Gtk::TreeRow krow= *kiter;

        krow[_exp_columns.possible_keys]= expl->possible_keys[k]?:"";
      }
    }
    
    exptree->expand_all();
    vbox->pack_start(*expscroll, true, true);
    
    button= Gtk::manage(new Gtk::Button(Gtk::Stock::CLOSE));
    button->signal_clicked().connect(sigc::mem_fun(*this,&MQResultSetView::hide_explain));
    button->signal_delete_event().connect(sigc::mem_fun(*this,&MQResultSetView::hide_explain_d));
    {
      Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(false, 0));
      hbox->pack_end(*button, false, false);
      vbox->pack_start(*hbox, false, false);
    }

    _explain_window->show_all();
    
    _explain_window->set_default_size(600, 240);
    _explain_window->resize(600, 240);
    
    _explain_window->show();

    myx_free_explain_result(result);
  }
  
  return true;
}


bool MQResultSetView::hide_explain_d(GdkEventAny *e)
{
  hide_explain();
  return true;
}


void MQResultSetView::hide_explain()
{
  delete _explain_window;
  _explain_window= 0;
}


Glib::ustring MQResultSetView::go_history_next()
{
  if (_history_index >= 0)
    _history_index--;

  int i= _history_index;
  std::list<std::string>::const_iterator iter;
  for (iter= _history.begin(); iter != _history.end(), i>=0; ++iter, i--)
    ;
  if (iter!=_history.end()) --iter;

  if (iter!=_history.end())
    return _history_list->find_entry(*iter)->sql?:"";
  else
    return "";
}


Glib::ustring MQResultSetView::go_history_back()
{
  _history_index++;

  int i= _history_index;
  std::list<std::string>::const_iterator iter;
  for (iter= _history.begin(); iter != _history.end(), i>0; ++iter, i--)
    ;
  if (iter!=_history.end())
    return _history_list->find_entry(*iter)->sql?:"";
  else
    return "";
}


int MQResultSetView::get_history_index()
{
  return _history_index;
}


void MQResultSetView::update_menu()
{
  bool is_field;
  Gtk::TreePath path;
  int column;

  if (_tree)
    is_field= get_selected_field(path, column);
  
  if (_model && _model->get_editable())
  {
    Gtk::TreeIter iter= _tree->get_selection()->get_selected();
    bool flag;

    myg_menu_set_sensitive(_edit_menu, "add_row", true);
    // enable Delete button only if there's something selected
    if (iter)
      myg_menu_set_sensitive(_edit_menu, "delete_row", true);
    else
      myg_menu_set_sensitive(_edit_menu, "delete_row", false);

    /*
    if (!_model->row_undoable(iter))
      myg_menu_set_sensitive(_edit_menu, "undo", false);
    else
      myg_menu_set_sensitive(_edit_menu, "undo", true);
     */

    if (iter && is_field)
      flag= true;
    else
      flag= false;

    myg_menu_set_sensitive(_edit_menu, "load_field", flag);
    myg_menu_set_sensitive(_edit_menu, "save_field", flag);
    myg_menu_set_sensitive(_edit_menu, "copy_field", flag);
    myg_menu_set_sensitive(_edit_menu, "clear_field", flag);
    myg_menu_set_sensitive(_edit_menu, "clear_field", flag);
    myg_menu_set_sensitive(_edit_menu, "edit_field", flag);
  }
  else
  {
    myg_menu_set_sensitive(_edit_menu, "add_row", false);
    myg_menu_set_sensitive(_edit_menu, "delete_row", false);      
    myg_menu_set_sensitive(_edit_menu, "load_field", false);
    
    bool flag;
    if (_model && _tree->get_selection()->get_selected() && is_field)
      flag= true;
    else
      flag= false;

    myg_menu_set_sensitive(_edit_menu, "save_field", flag);
    myg_menu_set_sensitive(_edit_menu, "copy_field", flag);
    myg_menu_set_sensitive(_edit_menu, "view_field", flag);
    myg_menu_set_sensitive(_edit_menu, "clear_field", false);
    myg_menu_set_sensitive(_edit_menu, "edit_field", false);
  }
}


bool MQResultSetView::is_editable()
{
  if (_model && _model->get_editable())
    return true;
  else
    return false;
}


void MQResultSetView::selected_row()
{
  _row_changed_signal.emit(this);
}


void MQResultSetView::selected_error()
{
  Gtk::TreeIter iter= _msgtree.get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    int rowi= row[_msg_columns.row];
    if (_tree && rowi >= 0)
    {
      Gtk::TreePath path;
      path.push_back(rowi);
      _tree->get_selection()->select(_model->get_iter(path));
    }
  }
}


void MQResultSetView::query_finished(MYX_RESULTSET *result, MYX_MYSQL_ERROR_MSGS *warnings)
{
  char *delay;
  struct timeval tv;
  unsigned int s;
  int ms;

  _msgscroll.hide();

  _is_busy= false;

  set_sensitive(true);
  
  gettimeofday(&tv, NULL);
  
  s= (tv.tv_sec-_current_query_start.tv_sec) + (tv.tv_usec-_current_query_start.tv_usec)/1000000;
  ms= (tv.tv_usec-_current_query_start.tv_usec)%1000000;
  if (ms < 0)
  {
    s--;
    ms= 1-ms;
  }
  delay= g_strdup_printf("%i:%02i.%04i", s/60, (s%60), ms/100);

  if (result)
    _status_label.set_text(ufmt(_("%i rows fetched in %s"), result->rows_num,
                                delay));
  else
    _status_label.set_text(ufmt(_("Query executed in %s"), delay));
  g_free(delay);

  if (warnings != 0)
  {
    for (unsigned int i= 0; i < warnings->errors_num; i++)
    {
      switch (warnings->errors[i].level)
      {
      case MYX_QEL_ERROR:
        append_error(warnings->errors[i].error,
                     warnings->errors[i].text, 
                     PIXCACHE->load("rs_error.png"));
        break;
      case MYX_QEL_WARNING:
        append_error(warnings->errors[i].error,
                     warnings->errors[i].text, 
                     PIXCACHE->load("rs_warning.png"));
        break;
      case MYX_QEL_NOTE:
        append_error(warnings->errors[i].error,
                     warnings->errors[i].text, 
                     PIXCACHE->load("rs_notice.png"));
        break;
      }
    }
    myx_mysql_error_msgs_free(warnings);
  }
  
  display_result(result);

  _query_finished_signal.emit(this);

  update_edit_button_sensitivity();
  
  if (dispose_if_no_result && !result)
    delete this;
}


bool MQResultSetView::query_progress(MYX_RESULTSET *result, 
                                     unsigned long count,
                                     unsigned long prev_count)
{
  if (result)
  {
    if (dispose_if_no_result)
      get_parent()->get_parent()->show();

    _status_label.set_text(ufmt(_("%i rows fetched"), count));
  }
  return !_current_query_cancelled;
}


void MQResultSetView::query_error(MYX_RESULTSET *result, MYX_MYSQL_ERROR_MSGS *errors, MYX_LIB_ERROR merror)
{
  if (!errors)
  {
    if (merror == MYX_MEMORY_LIMIT_EXCEEDED)
    {
      if (result)
        _status_label.set_text(ufmt(_("Memory limit exceeded. %i rows fetched."),
                                    result->rows_num));
      else
        _status_label.set_text(_("Memory limit exceeded."));

      Gtk::MessageDialog dlg(_("<b>The query generated a very large resultset</b>\n"
                               "Resultset retrieval was interrupted to preserve this machine's stability.\n"
                               "Please use the LIMIT keyword to limit the number of rows retrieved."),
                             true, Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK, true);
      dlg.run();
    }
    else
    {
      if (result)
        _status_label.set_text(ufmt(_("Query cancelled. %i rows fetched."),
                                    result->rows_num));
      else
        _status_label.set_text(_("Query cancelled."));
    }
  }
  else
  {
    _status_label.set_text(_("Error executing query"));
  }
  _is_busy= false;
  set_sensitive(true);

  display_result(result);

  _query_finished_signal.emit(this);

  display_errors(errors);

  if (errors != 0)
    myx_mysql_error_msgs_free(errors);

  update_edit_button_sensitivity();
}


void MQResultSetView::query_fetch_more(MYSQL *mysql)
{
  // we may have more resultsets to be fetched.
  // signal our parent about it, so that it can create a new rsview
  // and fetch it from there

  _query_more_data_signal.emit(this, mysql);
}


void MQResultSetView::save_finished(MYX_RS_ACTION_ERRORS *errors)
{
  bool has_failed_actions= false;
  if (_result->actions)
    for (unsigned int i= 0; i < _result->actions->actions_num; i++)
    {
      if (_result->actions->actions[i].status== MYX_RSAS_FAILED)
      {
        has_failed_actions= true;
        break;
      }
    }
  
  if (errors || has_failed_actions)
  {
    _model->post_commit(true);

    _msgstore->clear();
    if (errors)
    {
      for (unsigned int i= 0; i < errors->errors_num; i++)
      {
        switch (errors->errors[i].level)
        {
        case MYX_QEL_ERROR:
          append_error(errors->errors[i].error,
                       _("Error saving changed query. ")+Glib::ustring(errors->errors[i].error_text),
                       PIXCACHE->load("rs_error.png"), errors->errors[i].action->row);
          break;
        case MYX_QEL_WARNING:
          append_error(errors->errors[i].error,
                       Glib::ustring(errors->errors[i].error_text),
                       PIXCACHE->load("rs_warning.png"), errors->errors[i].action->row);
          break;
        case MYX_QEL_NOTE:
          append_error(errors->errors[i].error,
                       Glib::ustring(errors->errors[i].error_text),
                       PIXCACHE->load("rs_notice.png"), errors->errors[i].action->row);
          break;
        }
      }
      Gtk::TreePath path;
      path.push_back(_msgstore->children().size()-1);
      _msgtree.get_selection()->select(_msgstore->get_iter(path));

      myx_query_free_action_errors(errors);
    }
    // make it editable back
    _model->set_editable(true);
    update_edit_button_sensitivity();

    _query_finished_signal.emit(this);
    
    if (!prefs.dont_beep)
      Gdk::Display::get_default()->beep();
  }
  else
  {
    _model->post_commit(false);
    _model->set_editable(false);

    // refresh
    _tree->queue_draw();
    _query_finished_signal.emit(this);
    
    _msgscroll.hide();
  }

  _tree->set_model(Gtk::ListStore::create(_msg_columns));
  _tree->set_model(_model);
}


void MQResultSetView::set_sensitive(bool flag)
{
  _scroll.set_sensitive(flag);
  
  _hbox.set_sensitive(flag);
}


void MQResultSetView::update_edit_button_sensitivity()
{
  if (_model && _model->get_editable()) // editing
  {
    myg_image_button_set(_edit_btn, PIXCACHE->load("rs_discard.png"),
                         _compact_mode ? "":_("Discard Changes"));
//    _edit_btn.set_sensitive(false);

    myg_image_button_set(_apply_btn, PIXCACHE->load("rs_apply.png"),
                         _compact_mode ? "":_("Apply Changes"));
    _apply_btn.set_sensitive(true);
  }
  else
  {
    if (_result && _result->editable) // editable
    {
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_edit.png"),
                           _compact_mode?"":_("Start Editing"));
      _edit_btn.set_sensitive(true);
    }
    else                                  // not editable
    {
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_edit_disabled.png"),
                           _compact_mode?"":_("Start Editing"));
      _edit_btn.set_sensitive(false);
    }

    myg_image_button_set(_apply_btn, PIXCACHE->load("rs_apply_disabled.png"),
                         _compact_mode?"":_("Apply Changes"));
    _apply_btn.set_sensitive(false);
  }
}


void MQResultSetView::go_first()
{
  if (_model->children().size()>0)
  {
    Gtk::TreeIter iter= *_model->children().begin();
    _tree->scroll_to_row(_model->get_path(iter));
    _tree->get_selection()->select(*_model->children().begin());
  }
}


void MQResultSetView::go_last()
{
  if (_model->children().size()>0)
  {
    Gtk::TreePath path;
    
    path.push_back(_model->children().size()-1);
    _tree->scroll_to_row(path);
    _tree->get_selection()->select(_model->get_iter(path));
  }
}


void MQResultSetView::edit_start()
{
  if (!is_editable())
  {
    if (_model && !_model->get_editable())
    {
      _model->set_editable(true);
      update_edit_button_sensitivity();
      
      _editable_change_signal.emit();
    }
  }
  else
  {
    _model->set_editable(false);
    _model->discard_changes();
    update_edit_button_sensitivity();

    _tree->set_model(Gtk::ListStore::create(_msg_columns));
    _tree->set_model(_model);
    
    _tree->queue_draw();
    
    _editable_change_signal.emit();
  }
}

void MQResultSetView::edit_apply()
{
  _model->set_editable(false);
  _model->pre_commit();
  update_edit_button_sensitivity();
  _tree->set_model(Gtk::ListStore::create(_msg_columns));
  _tree->set_model(_model);

  if (_result->actions && _result->actions->actions_num > 0)
  {
    _query_started_signal.emit(this, true);
    _dispatcher->save_edits(_result,
                            sigc::mem_fun(*this,&MQResultSetView::save_finished));
  }
  else
  {
    _query_started_signal.emit(this, true);
    save_finished(NULL);
  }
  _editable_change_signal.emit();
}


void MQResultSetView::add_history_entry(const Glib::ustring &query)
{
  std::string dupe_id;

  if (query.empty())
    return;

  dupe_id= _history_list->check_dupe_and_mark(query);

  if (_history_index > 0)
  {
    std::list<std::string>::iterator iter= _history.begin();
    int i= _history_index;
    while (--i >= 0) ++iter;
    _history.erase(_history.begin(), iter);
  }
  _history_index= 0;

  if (dupe_id.empty())
  {
    std::string index= _history_list->add_entry(query,
                                                _dispatcher->get_current_catalog(),
                                                _dispatcher->get_current_schema());
    _history.push_front(index);
  }
  else
    _history.push_front(dupe_id);
}


bool MQResultSetView::get_active()
{
  return _active;
}


void MQResultSetView::set_active(bool flag)
{
  _active= flag;
  
  if (_tree)
  {
    std::vector<Gtk::CellRenderer*> rends= _tree->get_column(0)->get_cell_renderers();
    static_cast<MQIndicatorCellRenderer*>(rends[0])->property_active()= flag;

    _scroll.queue_draw();
  }

  _ebox.set_sensitive(flag);

  if (flag)
    _activate_signal.emit(this);
}


MYX_RS_ROW *MQResultSetView::get_selected_row()
{
  if (_result)
  {
    Gtk::TreeIter iter= _tree->get_selection()->get_selected();
    if (iter)
      return _model->get_iter_row(iter);
  }
  return 0;
}


bool MQResultSetView::editbar_clicked(GdkEventButton *ev)
{
  set_active(true);
  return true;
}


void MQResultSetView::save_cell_change(MGBlobEditor *editor,
                                       unsigned int column)
{
  char *cpath= (char*)((Glib::Object*)editor)->get_data("path");
  Gtk::TreePath path(cpath);
  gpointer data;
  gsize size;

  editor->get_data(data, size);
  _model->set_value(_model->get_iter(path), column*2, data, size);
  _tree->queue_draw();
  g_free(data);
}


bool MQResultSetView::get_selected_field(Gtk::TreePath &path, int &column)
{
  Gtk::TreeViewColumn *col= 0;

  if (!_tree->get_selection()->get_selected())
    return false;
  
  _tree->get_cursor(path, col);

  if (!col || path.empty())
    return false;

  column= -1;
  
  for (unsigned int i= 0; ; i++)
  {
    Gtk::TreeViewColumn *tmp= _tree->get_column(i);
    if (!tmp)
      break;
    if (col == tmp)
    {
      column= i-1;
      break;
    }
  }
  if (column < 0)
    return false;

  return true;
}


void MQResultSetView::column_action_mi(FieldAction action)
{
  Gtk::TreePath path;
  int column_index= 0;
  bool is_blob= false;
  Gtk::TreeViewColumn *column;

  if (!get_selected_field(path, column_index))
    return;
  
  column= _tree->get_column(column_index+1);

  if (_result->columns[column_index].column_type == MYX_RSCT_BLOB
      || _result->columns[column_index].column_type == MYX_RSCT_TEXT)
    is_blob= true;

  switch (action)
  {
  case FieldLoad:
    {
      Gtk::FileSelection fsel(_("Load Field Data"));

      if (fsel.run() == Gtk::RESPONSE_OK)
      {
        Glib::RefPtr<Glib::IOChannel> channel;
        try 
        {
          channel= Glib::IOChannel::create_from_file(fsel.get_filename(),"r");
          channel->set_encoding();
        } 
        catch (Glib::Error &exc)
        {
          myg_show_error(ufmt(_("Could not open file '%s'"), fsel.get_filename().c_str())+"\n"+exc.what());
          return;
        }
        
        try
        {
          gsize size= get_file_size(fsel.get_filename().c_str());
          gchar *buffer= g_new0(gchar, size);
          
          channel->read(buffer, size, size);
          
          _model->set_value(_model->get_iter(path), column_index*2, buffer, size);
          
          g_free(buffer);
        }
        catch (Glib::Error &exc)
        {
          myg_show_error(ufmt(_("Could not read data from file '%s'"), fsel.get_filename().c_str())+"\n"+exc.what());
        }
        channel->close();
      }
    }
    break;
  case FieldSave:
    {
      Gtk::FileSelection fsel(_("Save Field Data"));
      gsize written;
      gpointer fdata;
      size_t fsize;
      
      if (fsel.run() == Gtk::RESPONSE_OK)
      {
        _model->get_value(_model->get_iter(path), column_index*2, fdata, fsize);
        
        try
        {
          Glib::RefPtr<Glib::IOChannel> channel= Glib::IOChannel::create_from_file(fsel.get_filename(),"w+");
          channel->set_encoding();
          channel->write((char*)fdata, fsize, written);
          channel->close();
        } 
        catch (Glib::FileError &exc)
        {
          myg_show_error(ufmt(_("Could not save data to file '%s'.\n"), fsel.get_filename().c_str())+exc.what());
        }
      }
    }
    break;
  case FieldCopy:
    if (!is_blob)
    {
      gpointer fdata;
      size_t fsize;
      _model->get_value(_model->get_iter(path), column_index*2, fdata, fsize);

      if (fdata)
      {
        Gtk::Clipboard::get()->set_text((char*)fdata);
      }
    }
    break;
  case FieldClear:
    _model->set_value(_model->get_iter(path), column_index*2, NULL, 0);
    break;
  case FieldEdit:
    if (is_blob)
    {
      ((CellRendererBlob*)column->get_first_cell_renderer())->edit_clicked(get_parent(),
                                                                           path.to_string());
    }
    else
    {
      gpointer fdata;
      size_t fsize;
      char *cpath;
      MGBlobEditor *editor= new MGBlobEditor((MGBlobEditor::ViewType)(MGBlobEditor::VText|MGBlobEditor::VBinary),true);

      _model->get_value(_model->get_iter(path), column_index*2, fdata, fsize);
      editor->set_delete_on_close();
      editor->set_transient_for(*(Gtk::Window*)get_toplevel());
      editor->set_data(fdata, fsize);
      editor->show();
      
      cpath= g_strdup(path.to_string().c_str());
      ((Glib::Object*)editor)->set_data("path", cpath, (Glib::Object::DestroyNotify)g_free);
      
      editor->signal_save().connect(sigc::bind<MGBlobEditor*,unsigned int>(sigc::mem_fun(*this,&MQResultSetView::save_cell_change),
                                                                                       (MGBlobEditor*)editor,(unsigned int)column_index));
    }
    break;
  case FieldView:
    if (is_blob)
    {
      ((CellRendererBlob*)column->get_first_cell_renderer())->view_clicked(get_parent(),
                                                                           path.to_string());
    }
    else
    {
      gpointer fdata;
      size_t fsize;
      MGBlobEditor *editor= new MGBlobEditor((MGBlobEditor::ViewType)(MGBlobEditor::VText|MGBlobEditor::VBinary));

      _model->get_value(_model->get_iter(path), column_index*2, fdata, fsize);
      editor->set_delete_on_close();
      editor->set_transient_for(*(Gtk::Window*)get_toplevel());
      editor->set_data(fdata, fsize);
      editor->show();
    }
    break;
  }
}


void MQResultSetView::add_row_mi()
{
  _model->append();
  go_last();
}


void MQResultSetView::delete_row_mi()
{
  Gtk::TreeModel::iterator iter= _tree->get_selection()->get_selected();

  if (iter)
  {
    _model->erase(iter);
  }
}


void MQResultSetView::copy_row_names_to_clipboard()
{
  Gtk::TreeModel::iterator iter= _tree->get_selection()->get_selected();

  if (iter)
  {
    MYX_RESULTSET *rs= _model->get_resultset();

    if (rs)
    {
      Glib::ustring str;

      for (unsigned int i= 0; i < rs->columns_num_to_display; i++)
      {
        Glib::ustring value(rs->columns[i].name);

        value= "'"+value+"'";

        if (i > 0)
          str+= ", "+value;
        else
          str+= value;
      }
      Gtk::Clipboard::get()->set_text(str);
    }
  }
}

void MQResultSetView::copy_row_to_clipboard()
{
  Gtk::TreeModel::iterator iter= _tree->get_selection()->get_selected();

  if (iter)
  {
    MYX_RS_ROW *row= _model->get_iter_row(iter);
    MYX_RESULTSET *rs= _model->get_resultset();

    if (row && rs)
    {
      Glib::ustring str;

      for (unsigned int i= 0; i < rs->columns_num_to_display; i++)
      {
        Glib::ustring value(row->fields[i].value?:"", 0, row->fields[i].value_length);

        if (row->fields[i].value && MYX_RSCT_NEEDS_QUOTE(rs->columns[i].column_type))
          value= "'"+value+"'";

        if (i > 0)
          str+= ", "+value;
        else
          str+= value;
      }
      Gtk::Clipboard::get()->set_text(str);
    }
  }
}

void MQResultSetView::undo_row_mi()
{
  Gtk::TreeModel::iterator iter= _tree->get_selection()->get_selected();

  if (iter)
  {
//    _model->undo(iter);
  }
}


bool MQResultSetView::focused(GdkEventFocus *ev)
{
  set_active(true);
  return true;
}


std::vector<bool> MQResultSetView::get_search_column_list(const Glib::ustring &column_list)
{
  std::vector<bool> search_in(_result->columns_num_to_display);
  
  for (unsigned int i= 0; i < _result->columns_num_to_display; i++)
    search_in[i]= column_list.empty();

  {
    Glib::ustring col;
    Glib::ustring::size_type e;
    Glib::ustring columns= column_list;

    while (!columns.empty()) 
    {
      e= columns.find(',');
      if (e != Glib::ustring::npos)
      {
        col= columns.substr(0, e);
        columns= columns.substr(e+1);
      }
      else
      {
        col= columns;
        columns.clear();
      }
      for (unsigned int i= 0; i < _result->columns_num_to_display; i++)
      {
        if (col.compare(_result->columns[i].name)==0)
        {
          search_in[i]= true;
          break;
        }
      }
    } 
  }
  return search_in;
}


bool MQResultSetView::find_next(const Glib::ustring &str, const Glib::ustring &column_list)
{
  if (_model->children().size()==0)
    return false;
  
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (!iter)
    iter= *_model->children().begin();

  Gtk::TreePath path= _model->get_path(iter);
  int first= path[0];

  std::vector<bool> search_in= get_search_column_list(column_list);

  for (unsigned int r= first+1; r < _model->children().size()-1; r++)
  {
    for (unsigned int c= 0; c < _result->columns_num_to_display; c++)
    {
      if (search_in[c] && _result->rows[r].fields[c].value
	  && _result->columns[c].column_type != MYX_RSCT_BLOB)
      {
        if (g_strstr_len((_result->rows+r)->fields[c].value,
                         (_result->rows+r)->fields[c].value_length,
                         str.c_str()))
        {
          Gtk::TreePath path;
          path.push_back(r);

          _tree->scroll_to_row(path);
          _tree->get_selection()->select(_model->get_iter(path));

          return true;
        }
      }
    }
  }
  return false;
}


bool MQResultSetView::find_previous(const Glib::ustring &str, const Glib::ustring &column_list)
{
  if (_model->children().size()==0)
    return false;

  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  int first;
  if (iter)
  {
    Gtk::TreePath path= _model->get_path(iter);
    first= path[0];
  }
  else
    first= _model->children().size()-1;

  std::vector<bool> search_in= get_search_column_list(column_list);

  for (int r= first-1; r>=0; r--)
  {
    for (unsigned int c= 0; c < _result->columns_num_to_display; c++)
    {
      if (search_in[c] && _result->rows[r].fields[c].value
	  && _result->columns[c].column_type != MYX_RSCT_BLOB)
      {
        if (g_strstr_len((_result->rows+r)->fields[c].value,
                         (_result->rows+r)->fields[c].value_length,
                         str.c_str()))
        {
          Gtk::TreePath path;
          path.push_back(r);

          _tree->scroll_to_row(path);
          _tree->get_selection()->select(_model->get_iter(path));

          return true;
        }
      }
    }
  }
  return false;
}


bool MQResultSetView::find_next_diff()
{
  if (_model->children().size()==0)
    return false;

  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if (!iter)
    iter= *_model->children().begin();

  Gtk::TreePath path= _model->get_path(iter);
  int first= path[0];

  for (unsigned int r= first+1; r < _model->children().size()-1; r++)
  {
    if (_result->rows[r].diff!=0)
    {
      Gtk::TreePath path;
      path.push_back(r);

      _tree->scroll_to_row(path);
      _tree->get_selection()->select(_model->get_iter(path));
      return true;
    }
  }

  return false;
}


bool MQResultSetView::find_previous_diff()
{
  if (_model->children().size()==0)
    return false;

  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  int first;
  if (iter)
  {
    Gtk::TreePath path= _model->get_path(iter);
    first= path[0];
  }
  else
    first= _model->children().size()-1;

  for (int r= first-1; r>=0; r--)
  {
    if (_result->rows[r].diff!=0)
    {
      Gtk::TreePath path;
      path.push_back(r);

      _tree->scroll_to_row(path);
      _tree->get_selection()->select(_model->get_iter(path));
      return true;
    }
  }

  return false;
}


Gtk::TreePath MQResultSetView::get_selected_row_path()
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  
  if (iter)
    return Gtk::TreePath(iter);
  else
    return Gtk::TreePath();
}


void MQResultSetView::set_selected_row_path(const Gtk::TreePath &path)
{
  if (!path.empty())
    _tree->get_selection()->select(path);
}




void MQResultSetView::set_search_func(const sigc::slot1<void,MQResultSetView*> &slot)
{
  _search_btn.signal_clicked().connect(sigc::bind<MQResultSetView*>(slot,this));
}


void MQResultSetView::set_compact_editbar(bool flag)
{
  if (flag)
  {
    if (is_editable())
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_discard.png"), "");
    else
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_edit.png"), "");
    myg_image_button_set(_apply_btn, PIXCACHE->load("rs_apply.png"), "");
  
    myg_image_button_set(_first_btn, PIXCACHE->load("rs_first.png"), "");
    myg_image_button_set(_last_btn, PIXCACHE->load("rs_last.png"), "");
    myg_image_button_set(_search_btn, PIXCACHE->load("rs_search.png"), "");
  }
  else
  {
    if (is_editable())
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_discard.png"),
                           _("Discard Changed"));
    else
      myg_image_button_set(_edit_btn, PIXCACHE->load("rs_edit.png"),
                           _("Start Editing"));
    myg_image_button_set(_apply_btn, PIXCACHE->load("rs_apply.png"),
                         _("Apply Changes"));
  
    myg_image_button_set(_first_btn, PIXCACHE->load("rs_first.png"),
                       _("First"));
    myg_image_button_set(_last_btn, PIXCACHE->load("rs_last.png"),
                         _("Last"));
    myg_image_button_set(_search_btn, PIXCACHE->load("rs_search.png"),
                         _("Search"));
  }
  _compact_mode= flag;
  
  update_edit_button_sensitivity();
}



void MQResultSetView::editor_save(MGBlobEditor *editor)
{
  int column;
  gpointer buffer;
  gsize size;
  Glib::ustring path;

  editor->get_data(buffer, size);

  path= (char*)((Glib::Object*)editor)->get_data("path");
  column= (int)(long)((Glib::Object*)editor)->get_data("column");

  Gtk::TreeIter iter= _model->get_iter(Gtk::TreePath(path));
  _model->set_value(iter, column*2, buffer, size);
  _tree->queue_draw();
  
  g_free(buffer);
}


static MGBlobEditor::ViewType get_flags_for_data(char *fdata, gsize fsize,
                                                 bool binary)
{
  MGBlobEditor::ViewType flags;
  
  if (fsize > 0 && myx_guess_image_format(fdata, fsize)!=MYX_IMG_UNKNOWN)
    flags= MGBlobEditor::ViewType(MGBlobEditor::VImage|MGBlobEditor::VBinary);
  else if (binary)
    flags= MGBlobEditor::VBinary;
  else
    flags= MGBlobEditor::ViewType(MGBlobEditor::VText|MGBlobEditor::VBinary);

  return flags;
}

void MQResultSetView::blob_edit(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)
{
  gpointer fdata; 
  size_t fsize; 
  Gtk::TreeIter iter= _model->get_iter(Gtk::TreePath(path));

  if (_model->get_value(iter, column*2, fdata, fsize))
  {
    MGBlobEditor *editor= new MGBlobEditor(get_flags_for_data((char*)fdata, fsize, cell->is_binary()), true);
    editor->set_data(fdata, fsize);
    editor->resize(400, 300);
    editor->set_delete_on_close();

    ((Glib::Object*)editor)->set_data("path", g_strdup(path.c_str()), g_free);
    ((Glib::Object*)editor)->set_data("column", (char *)(long)column, NULL);
    
    editor->set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
    editor->set_transient_for(*(Gtk::Window*)widget->get_toplevel());
    editor->property_destroy_with_parent()=true;
    editor->signal_save().connect(sigc::bind<MGBlobEditor*>(sigc::mem_fun(*this,&MQResultSetView::editor_save),editor));
    
    editor->show();
  }
}


void MQResultSetView::blob_save(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)
{
  gpointer fdata; 
  size_t fsize;
  Gtk::TreeIter iter= _model->get_iter(Gtk::TreePath(path));

  if (_model->get_value(iter, column*2, fdata, fsize))
  {
    Gtk::FileSelection fsel(_("Save Field Data"));
    
    if (fsel.run() == Gtk::RESPONSE_OK)
    {
      gsize written;
      
      try 
      {
        Glib::RefPtr<Glib::IOChannel> channel= Glib::IOChannel::create_from_file(fsel.get_filename(),"w+");
        channel->set_encoding();
        channel->write((char*)fdata, fsize, written);
        channel->close();
      } 
      catch (Glib::FileError &exc)
      {
        myg_show_error(ufmt(_("Could not save data to file '%s'.\n"), fsel.get_filename().c_str())+exc.what());
      }
    }
  }
}


void MQResultSetView::blob_load(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)
{
  Gtk::FileSelection fsel(_("Load Field Data"));

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    Glib::RefPtr<Glib::IOChannel> channel;
    try 
    {
      channel= Glib::IOChannel::create_from_file(fsel.get_filename(),"r");
      channel->set_encoding();
    } 
    catch (Glib::Error &exc)
    {
      myg_show_error(ufmt(_("Could not open file '%s'"), fsel.get_filename().c_str())+"\n"+exc.what());
      return;
    }

    try
    {
      gsize size= get_file_size(fsel.get_filename().c_str());
      gchar *buffer= g_new0(gchar, size);

      channel->read(buffer, size, size);

      blob_data_edited(path, buffer, size, column*2);
      
      g_free(buffer);
    }
    catch (Glib::Error &exc)
    {
      myg_show_error(ufmt(_("Could not read data from file '%s'"), fsel.get_filename().c_str())+"\n"+exc.what());
    }
    channel->close();
  }
}


void MQResultSetView::blob_clear(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)
{
  blob_data_edited(path, NULL, 0, column*2);
}


void MQResultSetView::blob_view(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)
{
  gpointer fdata; 
  size_t fsize; 
  Gtk::TreeIter iter= _model->get_iter(Gtk::TreePath(path));

  if (_model->get_value(iter, column*2, fdata, fsize))
  {
    MGBlobEditor *editor= new MGBlobEditor(get_flags_for_data((char*)fdata, fsize, cell->is_binary()));
    editor->set_data(fdata, fsize);
    editor->resize(400, 300);
    editor->set_delete_on_close();
    
    editor->set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
    editor->set_transient_for(*(Gtk::Window*)widget->get_toplevel());
    editor->property_destroy_with_parent()=true;

    editor->show();
  }
}


void MQResultSetView::blob_data_edited(const Glib::ustring& path, gpointer data, gsize size, int column)
{
  _model->set_value(_model->get_iter(path), column, data, size);
}


void MQResultSetView::string_data_edited(const Glib::ustring& path, const Glib::ustring& new_text, int column)
{
  _model->set_value(_model->get_iter(path), column, (const gpointer)new_text.data(), new_text.bytes());
}
