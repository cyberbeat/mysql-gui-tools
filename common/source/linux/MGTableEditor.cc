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


#include "myx_sql_parser_public_interface.h"

#include "MGTableEditor.h"

#include "MGGladeXML.h"

#include "mygpriv.h"
#include "myg_utils.h"
#include "myg_gtkutils.h"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include <gtkmm/stock.h>


#define DBG(s) s


static void split_datatype(const Glib::ustring &datatype,
                    Glib::ustring &type, Glib::ustring &param)
{
  Glib::ustring::size_type p= datatype.find('(');
  
  if (p!=Glib::ustring::npos)
  {
    type= datatype.substr(0, p);
    param= datatype.substr(p);
  }
  else
  {
    type= datatype;
    param= "";
  }
}


static Glib::ustring format_data_type(MYX_DBM_COLUMN_DATA *cdata)
{
  if (!cdata)
    return "";
  if (cdata->datatype_params)
    return Glib::ustring(cdata->datatype_name)+Glib::ustring(cdata->datatype_params);
  else
    return Glib::ustring(cdata->datatype_name);
}


static MYX_DBM_DATATYPE *find_datatype(MYX_DBM_DATATYPES *data_types, Glib::ustring type)
{
  Glib::ustring::size_type end= type.find('(');
  if (end == Glib::ustring::npos)
    end= type.size();
  for (unsigned int dt= 0; dt < data_types->datatypes_num; dt++)
  {
    if (strncasecmp(data_types->datatypes[dt].name, type.c_str(), end)==0)
    {
      return data_types->datatypes+dt;
    }
  }
  return NULL;
}

static Glib::ustring shorten_comment(const Glib::ustring &comment)
{
  Glib::ustring::size_type p= comment.find('\n');
  
  if (p == Glib::ustring::npos)
    return comment;
  else
    return comment.substr(0, p)+" ...";
}


#define DEFAULT_CHARSET ""

MGTableEditor::MGTableEditor(bool windowed)
  : _confirm_xml(0), _windowed(windowed),
    _data_types(0), _charsets(0), _engines(0), 
    _mysql(0), _data(0), _anti_recursion(false),
    _showing_column_info(false)
{
  if (windowed)
    _xml= new MGGladeXML(myg_datadir+"/table_editor.glade", "table_editor");
  else
    _xml= new MGGladeXML(myg_datadir+"/table_editor.glade", "top_frame");

  _column_tree= _xml->get_tree("column_tree");
  
  if (windowed)
    _xml->get_widget("table_editor")->signal_delete_event().connect(sigc::mem_fun(*this,&MGTableEditor::delete_window_event));
  
  Gtk::TreeView *tree;
  Gtk::TreeView::Column *column;
  
  tree= _xml->get_tree("flags_tree");
  _flag_store= Gtk::ListStore::create(_flag_columns);
  tree->append_column_editable("", _flag_columns.value);
  tree->append_column("", _flag_columns.name);
  tree->set_model(_flag_store);
  ((Gtk::CellRendererToggle*)tree->get_column(0)->get_first_cell_renderer())->signal_toggled().connect(sigc::mem_fun(*this,&MGTableEditor::column_option_toggled));
  ((Gtk::CellRendererToggle*)tree->get_column(0)->get_first_cell_renderer())->property_activatable()= true;
  ((Gtk::CellRendererToggle*)tree->get_column(0)->get_first_cell_renderer())->set_active(true);

  tree= _xml->get_tree("fk_tree");
  _fk_store= Gtk::ListStore::create(_fk_columns);
  tree->append_column("", _columns.name);
  tree->set_model(_fk_store);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::fk_selected));

  tree= _xml->get_tree("fk_column_tree");
  _fk_column_store= Gtk::ListStore::create(_fkm_columns);
  tree->append_column_editable(_("Column"), _fkm_columns.source);
  tree->append_column_editable(_("Foreign Column"), _fkm_columns.target);
  tree->get_column(0)->set_resizable();
  tree->get_column(1)->set_resizable();
  ((Gtk::CellRendererText*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::mem_fun(*this,&MGTableEditor::fk_column_edited));
  tree->set_model(_fk_column_store);

  tree= _xml->get_tree("index_tree");
  _index_store= Gtk::ListStore::create(_il_columns);
  tree->append_column/*_editable*/("", _il_columns.name);
  tree->set_model(_index_store);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::index_selected));

  tree= _xml->get_tree("index_columns_tree");
  _index_column_store= Gtk::ListStore::create(_icl_columns);
  tree->append_column(_("Column"), _icl_columns.name);
  tree->append_column_editable(_("Length"), _icl_columns.length);
  tree->set_model(_index_column_store);
  tree->get_column(0)->set_resizable();
  ((Gtk::CellRendererText*)tree->get_column(0)->get_first_cell_renderer())->property_editable()= false;
  tree->get_column(1)->set_resizable();
  ((Gtk::CellRendererText*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::mem_fun(*this,&MGTableEditor::index_column_edited));

//  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::index_column_selected));


  // set up column tree
  tree= _column_tree;
  _column_store= Gtk::ListStore::create(_tcolumns);
  column= new Gtk::TreeView::Column(_("Column Name"));
  column->pack_start(_tcolumns.icon, false);
  column->pack_start(_tcolumns.name);
  tree->append_column(*Gtk::manage(column));
  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  
  tree->signal_button_press_event().connect_notify(sigc::mem_fun(*this,&MGTableEditor::column_tree_clicked));
  tree->signal_key_release_event().connect(sigc::mem_fun(*this,&MGTableEditor::column_tree_key_released));

  ((Gtk::CellRendererText*)rends[1])->property_editable()= true;
  ((Gtk::CellRendererText*)rends[1])->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_edited),0));
  tree->append_column_editable(_("Data Type"), _tcolumns.type);
  tree->append_column_editable("", _tcolumns.nnull);
  tree->append_column_editable("", _tcolumns.autoinc);
  tree->append_column(_("Flags"), _tcolumns.flags);
  tree->append_column_editable(_("Default Value"), _tcolumns.defval);
  tree->get_column(5)->add_attribute(static_cast<Gtk::CellRendererText*>(tree->get_column(5)->get_first_cell_renderer())->property_style_set(),
                                     _tcolumns.defnull);
  static_cast<Gtk::CellRendererText*>(tree->get_column(5)->get_first_cell_renderer())->property_style()= Pango::STYLE_ITALIC;
  tree->append_column_editable(_("Comments"), _tcolumns.comment);
  Gtk::Widget *w;
  tree->get_column(2)->set_widget(*(w=Gtk::manage(new Gtk::Image(PIXCACHE->load("editor_table_not_null.png")))));
  w->show();
  tree->get_column(3)->set_widget(*(w=Gtk::manage(new Gtk::Image(PIXCACHE->load("editor_table_auto_inc.png")))));
  w->show();
  tree->set_model(_column_store);
  for (int i= 0; i < 6; i++)
  {
    column= tree->get_column(i);
    column->set_resizable();
  }
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::column_selected));
  // handle editions in the column tree
  ((Gtk::CellRendererText*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_edited), 1));
  ((Gtk::CellRendererToggle*)tree->get_column(2)->get_first_cell_renderer())->signal_toggled().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_toggled), 2));
  ((Gtk::CellRendererToggle*)tree->get_column(2)->get_first_cell_renderer())->property_activatable()= true;
  ((Gtk::CellRendererToggle*)tree->get_column(2)->get_first_cell_renderer())->set_active(true);
  ((Gtk::CellRendererToggle*)tree->get_column(3)->get_first_cell_renderer())->signal_toggled().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_toggled), 3));
  ((Gtk::CellRendererToggle*)tree->get_column(3)->get_first_cell_renderer())->property_activatable()= true;
  ((Gtk::CellRendererToggle*)tree->get_column(3)->get_first_cell_renderer())->set_active(true);

  ((Gtk::CellRendererText*)tree->get_column(5)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_edited), 5));
  ((Gtk::CellRendererText*)tree->get_column(6)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGTableEditor::column_edited), 6));


  // handle change events for column options
  _xml->get_entry("column_name_entry")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"name"));
  _xml->get_entry("default_entry")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"default"));
  ((Gtk::Entry*)_xml->get_combo_entry("datatype_combo")->get_child())->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"type"));
  _xml->get_combo("column_charset_combo")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"charset"));
  _xml->get_combo("column_collation_combo")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"collation"));
  _xml->get_toggle("pk_check")->signal_toggled().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"pk"));
  _xml->get_toggle("nnull_check")->signal_toggled().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"nn"));
  _xml->get_toggle("autoinc_check")->signal_toggled().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"autoinc"));
  _xml->get_text("comment_text")->get_buffer()->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"comment"));
  _xml->get_toggle("null_toggle")->signal_toggled().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::column_option_changed),"defnull"));
  
  // set up dragging from column list
  std::vector<Gtk::TargetEntry> types;
  types.push_back(Gtk::TargetEntry("x-mysqlgui-table-column"));
  // we set some modifier masks because the default behavious of click-drag
  // is trigerring the edition of the cell, which is somehow crashing
  _column_tree->drag_source_set(types, Gdk::SHIFT_MASK|Gdk::BUTTON1_MASK);
  _column_tree->signal_drag_data_get().connect(sigc::mem_fun(*this,&MGTableEditor::column_drag_data_get));

  // set up dropping in column list of indices
  _xml->get_tree("index_columns_tree")->drag_dest_set(types);
  _xml->get_tree("index_columns_tree")->signal_drag_data_received().connect(sigc::mem_fun(*this,&MGTableEditor::index_drop_drag_data_received), false);

  _xml->get_entry("index_name_entry")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::index_option_changed),"name"));
  _xml->get_option("index_kind_option")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::index_option_changed),"kind"));
  _xml->get_option("index_type_option")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::index_option_changed),"type"));

  _xml->get_button("index_add_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::add_index));
  _xml->get_button("index_remove_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::remove_index));
  _xml->get_button("index_column_remove_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::index_remove_column));

  // same for fk
  _xml->get_tree("fk_column_tree")->drag_dest_set(types);
  _xml->get_tree("fk_column_tree")->signal_drag_data_received().connect(sigc::mem_fun(*this,&MGTableEditor::fk_drop_drag_data_received), false);

  _xml->get_entry("fk_name_entry")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::fk_option_changed),"name"));
  _xml->get_option("ondelete_option")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::fk_option_changed),"ondelete"));
  _xml->get_option("onupdate_option")->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::fk_option_changed),"onupdate"));
  ((Gtk::Entry*)_xml->get_combo_entry("fk_table_combo")->get_child())->signal_changed().connect(sigc::bind<char*>(sigc::mem_fun(*this,&MGTableEditor::fk_option_changed),"table"));

  _xml->get_button("fk_add_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::add_fk));
  _xml->get_button("fk_remove_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::remove_fk));
  _xml->get_button("add_fk_column_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::fk_add_column));
  _xml->get_button("remove_fk_column_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::fk_remove_column));

  _xml->get_button("ok_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::commit_changes));
  _xml->get_button("discard_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::revert_changes));

  if (windowed)
    _xml->get_button("close_button")->signal_clicked().connect(sigc::mem_fun(*_xml->get_widget("table_editor"),&Gtk::Widget::hide));
  else
    myg_image_button_set(*_xml->get_button("close_button"),
                         Gtk::Stock::GO_BACK, _("Back"));

  
  _xml->get_combo("charset_combo")->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::charset_changed));
  
  
  // menu
  {
    Gtk::MenuItem *item;
    item= Gtk::manage(new Gtk::MenuItem("_Add Column", true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MGTableEditor::column_append));
    _popup.append(*item);
    item= Gtk::manage(new Gtk::MenuItem("_Insert Column", true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MGTableEditor::column_insert));
    _popup.append(*item);
    _popup.append(*myg_make_separator_item());
    item= Gtk::manage(new Gtk::MenuItem("_Delete Column", true));
    item->signal_activate().connect(sigc::mem_fun(*this,&MGTableEditor::column_delete));
    _popup.append(*item);
    _popup.show_all();
  }
  _column_icon= PIXCACHE->load("16x16_Field.png");
  _pk_icon= PIXCACHE->load("16x16_KeyColumn.png");

  _blob_icon= PIXCACHE->load("datatype_blob.png");
  _date_icon= PIXCACHE->load("datatype_datetime.png");
  _numeric_icon= PIXCACHE->load("datatype_numeric.png");
  _spatial_icon= PIXCACHE->load("datatype_spatial.png");
  _string_icon= PIXCACHE->load("datatype_string.png");
  _userdef_icon= PIXCACHE->load("datatype_userdefined.png");
}


MGTableEditor::~MGTableEditor()
{
  myx_free_engines(_engines);
}



void MGTableEditor::show_table_info(MYX_DBM_TABLE_DATA *data)
{
  _xml->get_entry("name_entry")->set_text(data->name?:"");
  
  /*
  ((Gtk::Entry*)_xml->get_combo_entry("database_combo")->get_child())->set_text(data->schema?:"");
  _xml->get_combo_entry("database_combo")->set_sensitive(false);
   */

  _xml->get_entry("comment_entry")->set_text(data->comment?:"");
}


bool MGTableEditor::check_datatype(const Glib::ustring &s)
{
  Glib::ustring type, param;
  
  split_datatype(s, type, param);
  
  for (unsigned int i= 0; i < _data_types->datatypes_num; i++)
  {
    if (strcasecmp(_data_types->datatypes[i].name, type.c_str())==0)
      return true;
  }
  return false;
}


void MGTableEditor::show_table_options(MYX_DBM_TABLE_DATA *data)
{
  // general options
  _xml->get_combo("engine_combo")->set_active(-1);
  if (data->storage_engine)
  {
    for (unsigned i= 0; i < _engines->engines_num; i++)
    {    
      if (strcmp(_engines->engines[i].name, data->storage_engine->name)==0)
      {
        _xml->get_combo("engine_combo")->set_active(i);
        break;
      }
    }
  }
  update_for_selected_engine();

  _xml->get_entry("merge_tables_entry")->set_text(data->merge_union?:"");
  _xml->get_option("insert_method_option")->set_history((int)data->merge_insert);

  set_combo_item(_xml->get_combo("charset_combo"), data->charset?data->charset:DEFAULT_CHARSET);
  set_combo_item(_xml->get_combo("collation_combo"), data->collation?:DEFAULT_CHARSET);

  // advanced options
  _xml->get_entry("next_autoinc_entry")->set_text(data->next_auto_inc?:"");
  _xml->get_entry("password_entry")->set_text(data->password?:"");
  _xml->get_toggle("delay_update_check")->set_active(data->delay_key_write);

  _xml->get_entry("datadir_entry")->set_text(data->table_data_dir?:"");
  _xml->get_entry("indexdir_entry")->set_text(data->table_index_dir?:"");

  _xml->get_option("pack_option")->set_history((int)data->pack_keys);

  _xml->get_option("raid_option")->set_history((int)data->raid_type);
  _xml->get_spin("chunk_num_spin")->set_text(data->raid_chunks?:"");
  _xml->get_spin("chunk_size_spin")->set_text(data->raid_chunk_size?:"");
  
  _xml->get_option("format_option")->set_history((int)data->row_format);
  _xml->get_entry("avg_length_entry")->set_text(data->avg_row_length?:"");
  _xml->get_entry("min_rows_entry")->set_text(data->min_rows?:"");
  _xml->get_entry("max_rows_entry")->set_text(data->max_rows?:"");
  
  _xml->get_toggle("checksum_check")->set_active(data->checksum);
}



void MGTableEditor::show_column_list(MYX_DBM_TABLE_DATA *data)
{
  _column_store->clear();
  for (unsigned int i= 0; i < data->columns_num; i++)
  {
    MYX_DBM_COLUMN_DATA *column= data->columns+i;
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    
    iter= _column_store->append();
    row= *iter;
    
    row[_tcolumns.icon]= get_icon_for_column(column->datatype_pointer,
                                             column->primary_key);
    row[_tcolumns.pk]= column->primary_key;
    row[_tcolumns.name]= column->name;
    row[_tcolumns.type]= format_data_type(column);
    row[_tcolumns.nnull]= column->not_null!=0;
    row[_tcolumns.autoinc]= column->auto_inc!=0;
    Glib::ustring tmp;
    for (unsigned int j= 0; j < column->datatype_flags_num; j++)
    {
      if (j > 0)
        tmp+= ","+Glib::ustring(column->datatype_flags[j]);
      else
        tmp= Glib::ustring(column->datatype_flags[j]);
    }
    row[_tcolumns.flags]= tmp;
    if (column->default_value_is_null)
      row[_tcolumns.defval]= "NULL";
    else
      row[_tcolumns.defval]= column->default_value?:"";
    row[_tcolumns.defnull]= column->default_value_is_null;
    row[_tcolumns.comment]= shorten_comment(column->comment?:"");
    row[_tcolumns.comment_full]= column->comment?:"";
    row[_tcolumns.charset]= column->charset?column->charset:DEFAULT_CHARSET;
    row[_tcolumns.collation]= column->collation?:"";
    row[_tcolumns.object]= column;
  }
  _xml->get_tree("column_tree")->columns_autosize();
}


void MGTableEditor::column_selected()
{
  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();

  show_column_info(_data, iter);
}


Glib::ustring MGTableEditor::get_combo_item(Gtk::ComboBox *combo, const Glib::ustring &defval)
{
  Gtk::TreeIter iter= combo->get_active();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    return (char*)(void*)row[_columns.object];
  }
  return defval;
}


void MGTableEditor::set_combo_item(Gtk::ComboBox *combo, const Glib::ustring &item)
{
  Glib::RefPtr<Gtk::ListStore> store= Glib::RefPtr<Gtk::ListStore>::cast_dynamic(combo->get_model());

  if (store)
  {
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    
    for (iter= store->children().begin(); iter != store->children().end(); ++iter)
    {
      row= *iter;
      if (Glib::ustring((char*)(void*)row[_columns.object]) == item)
      {
        combo->set_active(iter);
        break;
      }
    }
  }
}


void MGTableEditor::show_column_info(MYX_DBM_TABLE_DATA *data, const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row;
  
  _showing_column_info= true;
  
  if (iter)
    row= *iter;
  _xml->get_entry("column_name_entry")->set_text(iter ? (Glib::ustring)row[_tcolumns.name] : "");
  ((Gtk::Entry*)_xml->get_combo_entry("datatype_combo")->get_child())->set_text(iter ? (Glib::ustring)row[_tcolumns.type] : "");
  
  _xml->get_toggle("null_toggle")->set_active(iter ? (bool)row[_tcolumns.defnull] : false);
  _xml->get_toggle("pk_check")->set_active(iter ? (bool)row[_tcolumns.pk] : false);
  _xml->get_toggle("nnull_check")->set_active(iter ? (bool)row[_tcolumns.nnull] : false);
  _xml->get_toggle("autoinc_check")->set_active(iter ? (bool)row[_tcolumns.autoinc] : false);
  _xml->get_entry("default_entry")->set_text(iter ? (Glib::ustring)row[_tcolumns.defval] : "");

  Glib::ustring type, param;
  if (iter)
  {
    type= row[_tcolumns.type];
    split_datatype(type, type, param);
  }

  _flag_store->clear();
  for (unsigned int i= 0; i < _data_types->datatypes_num; i++)
  {
    if (type.compare(_data_types->datatypes[i].name)==0)
    {
      Glib::ustring flags= row[_tcolumns.flags];
      for (unsigned int j= 0; j < _data_types->datatypes[i].flags_num; j++)
      {
        Gtk::TreeIter iter= _flag_store->append();
        Gtk::TreeRow row= *iter;
        bool on= false;
        Glib::ustring::size_type p0= 0, p;
                
        while ((p= flags.find(',', p0)) != Glib::ustring::npos)
        {
          if (flags.substr(p0, p).compare(_data_types->datatypes[i].flags[j])==0)
          {
            on= true;
            break;
          }
          p0= p+1;
        }
        if (flags.substr(p0).compare(_data_types->datatypes[i].flags[j])==0)
        {
          on= true;
        }
        row[_flag_columns.value]= on;
        row[_flag_columns.name]= _data_types->datatypes[i].flags[j];
      }
      break;
    }
  }

  set_combo_item(_xml->get_combo("column_charset_combo"), iter ? (Glib::ustring)row[_tcolumns.charset] : DEFAULT_CHARSET);
  set_combo_item(_xml->get_combo("column_collation_combo"), iter ? (Glib::ustring)row[_tcolumns.collation] : DEFAULT_CHARSET);

  _xml->get_text("comment_text")->get_buffer()->set_text(iter ? (Glib::ustring)row[_tcolumns.comment_full] : "");

  _xml->get_widget("column_page")->set_sensitive((iter));
  
  _showing_column_info= false;
}


void MGTableEditor::show_index_list(MYX_DBM_TABLE_DATA *data)
{
  _index_store->clear();
  
  for (unsigned int i= 0; i < data->indices_num; i++)
  {
    MYX_DBM_INDEX_DATA *index= data->indices+i;
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    
    iter= _index_store->append();
    row= *iter;

    row[_il_columns.name]= index->name;
    row[_il_columns.kind]= index->index_kind;
    row[_il_columns.type]= index->index_type;

    std::list<ILColumns::IndexColumn> columns;
    for (unsigned int j= 0; j < index->columns_num; j++)
    {
      ILColumns::IndexColumn ic;
      
      ic.name= index->columns[j].name;
      if (strcmp2(index->columns[j].len?:"NULL", "NULL")==0)
        ic.length= "";
      else
        ic.length= index->columns[j].len;
      ic.collation= index->columns[j].value_order?:"";
      columns.push_back(ic);
    }
    row[_il_columns.columns]= columns;
    row[_il_columns.object]= index;
  }
}


void MGTableEditor::index_selected()
{
  Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  
  show_index_info(_data, iter);

  Gtk::TreeRow row;
  if (iter)
    row= *iter;
  bool editable= (iter && row[_il_columns.name]!="PRIMARY");

  _xml->get_widget("index_options_frame")->set_sensitive(editable);
}


void MGTableEditor::show_index_info(MYX_DBM_TABLE_DATA *tdata, const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row;
  
  if (iter)
    row= *iter;
  _xml->get_entry("index_name_entry")->set_text(iter?(Glib::ustring)row[_il_columns.name]:"");
  _xml->get_option("index_kind_option")->set_history(iter?(MYX_DBM_INDEX_KIND)row[_il_columns.kind]:0);
  _xml->get_option("index_type_option")->set_history(iter?(MYX_DBM_INDEX_TYPE)row[_il_columns.type]:0);

  _index_column_store->clear();
  std::list<ILColumns::IndexColumn> columns;
  
  if (iter)
    columns= row[_il_columns.columns];
  for (std::list<ILColumns::IndexColumn>::const_iterator it= columns.begin();
       it != columns.end(); ++it)
  {
    Gtk::TreeIter iter= _index_column_store->append();
    Gtk::TreeRow row= *iter;
    row[_icl_columns.name]= it->name;
    row[_icl_columns.length]= it->length;
    row[_icl_columns.collation]= it->collation;
  }
}


Glib::RefPtr<Gdk::Pixbuf> MGTableEditor::get_icon_for_column(MYX_DBM_DATATYPE *type,
                                                             bool pk)
{
  if (pk)
    return _pk_icon;

  if (!type)
    return _column_icon;

  switch (type->group)
  {
  case MYX_DBM_DTG_NUMERIC:
    return _numeric_icon;
  case MYX_DBM_DTG_DATETIME:
    return _date_icon;
  case MYX_DBM_DTG_STRING:
    return _string_icon;
  case MYX_DBM_DTG_BLOB:
    return _blob_icon;
  case MYX_DBM_DTG_SPATIAL:
    return _spatial_icon;
  case MYX_DBM_DTG_USERDEFINED:
    return _userdef_icon;
  }
  return _column_icon;
}

    
void MGTableEditor::show_fk_list(MYX_DBM_TABLE_DATA *data)
{
  _fk_store->clear();

  for (unsigned int i= 0; i < data->fks_num; i++)
  {
    MYX_DBM_FK_DATA *fk= data->fks+i;
    Gtk::TreeIter iter;
    Gtk::TreeRow row;
    
    iter= _fk_store->append();
    row= *iter;
    
    row[_fk_columns.name]= fk->name;
    row[_fk_columns.on_delete]= fk->on_delete;
    row[_fk_columns.on_update]= fk->on_update;
    if (fk->reference_schema_name && *fk->reference_schema_name)
    {
      char *tmp;
      row[_fk_columns.table]= tmp= g_strdup_printf("`%s`.`%s`", fk->reference_schema_name, fk->reference_table_name);
      g_free(tmp);
    }
    else
    {
      char *tmp;
      if (myx_identifier_needs_quotes(fk->reference_table_name))
      {
        row[_fk_columns.table]= tmp= quote_identifier(fk->reference_table_name, '`');
        g_free(tmp);
      }
      else
        row[_fk_columns.table]= fk->reference_table_name;
    }

    std::list<FKColumns::FKMapping> list;
    for (unsigned int j= 0; j < fk->column_mapping_num; j++)
    {
      FKColumns::FKMapping m;
      m.source= fk->column_mapping[j].name;
      m.target= fk->column_mapping[j].value;
      list.push_back(m);
    }
    row[_fk_columns.columns]= list;
    row[_fk_columns.object]= fk;
  }
}


void MGTableEditor::fk_selected()
{
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
    
  show_fk_info(_data, iter);
  
  _xml->get_widget("fk_frame")->set_sensitive((iter));
}


void MGTableEditor::show_fk_info(MYX_DBM_TABLE_DATA *tdata, const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;
  
  if (iter)
    row= *iter;

  _xml->get_entry("fk_name_entry")->set_text(iter ? (Glib::ustring)row[_fk_columns.name]:"");
  _xml->get_option("ondelete_option")->set_history(iter ? (int)row[_fk_columns.on_delete]:0);
  _xml->get_option("onupdate_option")->set_history(iter ? (int)row[_fk_columns.on_update]:0);
  ((Gtk::Entry*)_xml->get_combo_entry("fk_table_combo")->get_child())->set_text(iter ? (Glib::ustring)row[_fk_columns.table]:"");

  std::list<FKColumns::FKMapping> list;
  if (iter)
    list= row[_fk_columns.columns];
  _fk_column_store->clear();
  for (std::list<FKColumns::FKMapping>::const_iterator it= list.begin();
       it != list.end(); ++it)
  {
    Gtk::TreeIter jter= _fk_column_store->append();
    Gtk::TreeRow row2= *jter;
    row2[_fkm_columns.source]= it->source;
    row2[_fkm_columns.target]= it->target;
  }
}

void MGTableEditor::show_dbm(MYX_DBM_TABLE_DATA *data)
{
  show_table_info(data);
  show_table_options(data);
  
  show_index_list(data);
  index_selected();
  
  show_fk_list(data);
  fk_selected();

  show_column_list(data);
  column_selected();
}


void MGTableEditor::update_for_selected_engine()
{
  MYX_ENGINE *type= get_selected_engine();

  if (type)
    _xml->get_label("engine_label")->set_text(type->description);

  int count= _xml->get_option("index_kind_option")->get_menu()->items().size();
  Gtk::Menu *menu= _xml->get_option("index_kind_option")->get_menu();

  if (type && strcasecmp(type->name, "merge")==0)
    _xml->get_widget("merge_options")->set_sensitive(true);
  else
    _xml->get_widget("merge_options")->set_sensitive(false);
  
  // index kind
  if (type && strcasecmp(type->name, "memory")==0)
  {
    for (int i= 1; i < count; i++)
      menu->items()[i].set_sensitive(false);
    
    //BTREE
    menu->items()[1].set_sensitive(true);

    //HASH
    menu->items()[2].set_sensitive(true);
  }
  else if (type && (strcasecmp(type->name, "myisam")==0 || strcasecmp(type->name, "innodb")==0))
  {
    for (int i= 1; i < count; i++)
      menu->items()[i].set_sensitive(false);
    
    //BTREE
    menu->items()[1].set_sensitive(true);
  }
  else
  {
    for (int i= 0; i < count; i++)
      menu->items()[i].set_sensitive(true);
  }
}


void MGTableEditor::fill_aux_data()
{
  MYX_CATALOGS *cats= _catalog_data->ptr();
  MYX_CATALOG *my_catalog= NULL;
  MYX_SCHEMA_TABLES *my_schema_tables= NULL;
  //Glib::RefPtr<Gtk::ListStore> dblist= Gtk::ListStore::create(_columns);
  Glib::RefPtr<Gtk::ListStore> tblist= Gtk::ListStore::create(_columns);
  Glib::RefPtr<Gtk::ListStore> typelist= Gtk::ListStore::create(_columns);
  Glib::RefPtr<Gtk::ListStore> cslist1= Gtk::ListStore::create(_columns);
  Glib::RefPtr<Gtk::ListStore> cslist2= Gtk::ListStore::create(_columns);
  Gtk::TreeIter iter, iter2;
  Gtk::TreeRow row, row2;

  // set database list
  for (unsigned int i= 0; i < cats->catalogs_num; i++)
  {
    if (_catalog.compare(cats->catalogs[i].catalog_name)==0
        /*XXX*/ || (_catalog.empty() && strcmp(cats->catalogs[i].catalog_name,"def")==0))
    {
      my_catalog= _catalog_data->ptr()->catalogs+i;
      
      for (unsigned int j= 0; j < my_catalog->schemata_num; j++)
      {
        //iter= dblist->append();
        //row= *iter;
        
        //row[_columns.name]= my_catalog->schemata[j].schema_name;
        //row[_columns.object]= my_catalog->schemata+j;

        if (_schema.compare(my_catalog->schemata[j].schema_name)==0)
          my_schema_tables= my_catalog->schemata[j].schema_tables;
      }
      break;
    }
  }
/*
  _xml->get_combo_entry("database_combo")->set_model(dblist);
  _xml->get_combo_entry("database_combo")->set_text_column(0);
*/

  // fill table list in referenced table combo
  for (unsigned int i= 0; i < my_schema_tables->schema_tables_num; i++)
  {
    iter= tblist->append();
    row= *iter;
    row[_columns.name]= my_schema_tables->schema_tables[i].table_name;
    row[_columns.object]= my_schema_tables->schema_tables+i;
  }
  _xml->get_combo_entry("fk_table_combo")->set_model(tblist);
  _xml->get_combo_entry("fk_table_combo")->set_text_column(0);

  // set datatype list
  for (unsigned int i= 0; i < _data_types->datatypes_num; i++)
  {
    iter= typelist->append();
    row= *iter;
    row[_columns.name]= _data_types->datatypes[i].name;
    row[_columns.object]= _data_types->datatypes+i;
  }
  _xml->get_combo_entry("datatype_combo")->set_model(typelist);
  _xml->get_combo_entry("datatype_combo")->set_text_column(0);
  
  // set character set lists
  iter= cslist1->append();
  row= *iter;
  row[_columns.name]= DEFAULT_CHARSET;
  row[_columns.object]= (void*)"";
  iter2= cslist2->append();
  row2= *iter2;
  row2[_columns.name]= DEFAULT_CHARSET;
  row2[_columns.object]= (void*)"";
  for (unsigned int i= 0; _charsets && i < _charsets->charsets_num; i++)
  {
    iter= cslist1->append();
    row= *iter;
    iter2= cslist2->append();
    row2= *iter2;

    if (!_charsets->charsets[i].desc || !*_charsets->charsets[i].desc)
    {
      row[_columns.name]= _charsets->charsets[i].name;
      row2[_columns.name]= _charsets->charsets[i].name;
    }
    else
    {
      row[_columns.name]= _charsets->charsets[i].desc;
      row2[_columns.name]= _charsets->charsets[i].desc;
    }
    row[_columns.object]= _charsets->charsets[i].name;
    row2[_columns.object]= _charsets->charsets[i].name;
  }

  _xml->get_combo("column_charset_combo")->set_model(cslist1);
  _xml->get_combo("column_charset_combo")->pack_start(_columns.name);

  _xml->get_combo("charset_combo")->set_model(cslist2);
  _xml->get_combo("charset_combo")->pack_start(_columns.name);
}


void MGTableEditor::fill_collation_combo(Gtk::ComboBox *combo, const Glib::ustring &charset)
{
  MYX_DBM_CHARSET *chs= NULL;

  for (unsigned int i= 0; _charsets && i < _charsets->charsets_num; i++)
  {
    if (charset.compare(_charsets->charsets[i].name)==0)
    {
      chs= _charsets->charsets+i;
      break;
    }
  }

  Glib::RefPtr<Gtk::ListStore> store= Gtk::ListStore::create(_columns);
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  store->clear();
  iter= store->append();
  row= *iter;
  row[_columns.name]= DEFAULT_CHARSET;
  row[_columns.object]= (void*)"";
  Glib::ustring default_collation;
  if (!chs)
  {
    if (charset == DEFAULT_CHARSET)
    {
      combo->set_model(store);
    }
//    else
//      g_message("bad charset!?!?! %s", charset.c_str());
  }
  else
  {
    for (unsigned int i= 0; i < chs->collations_num; i++)
    {
      iter= store->append();
      row= *iter;
      row[_columns.name]= chs->collations[i].name;
      row[_columns.object]= chs->collations[i].name;
      if (chs->collations[i].is_default)
        default_collation= chs->collations[i].name;
    }
  }
  combo->clear();
  combo->set_model(store);
  combo->pack_start(_columns.name);
  if (!default_collation.empty())
    set_combo_item(combo, default_collation);
}


void MGTableEditor::new_table(MYSQL *mysql, const Glib::ustring &catalog, const Glib::ustring &schema,
                              const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalog_data)
{
  show_table(mysql, catalog, schema, "", catalog_data);
  _creating_table= true;
}

void MGTableEditor::show_table(MYSQL *mysql, const Glib::ustring &catalog, 
                               const Glib::ustring &schema,
                               const Glib::ustring &table,
                               const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalog_data)
{
  _creating_table= false;
  if (_mysql != mysql)
  {
    extern std::string get_prefix(); // must be defined in the app
    MYX_LIB_ERROR error;

    _mysql= mysql;

    if (_data_types)
      myx_free_datatype(_data_types);

    std::string path= myg_datadir + "/mysqlx_dbm_datatypes.xml";
    _data_types= myx_datatype_load(path.c_str(), &error);
    if (!_data_types)
      myg_show_xlib_error(ufmt(_("Could not load data files '%s' for table editor."), path.c_str()), error);

    if (_charsets)
      myx_free_charsets(_charsets);

    if (_engines)
      myx_free_engines(_engines);
    _engines= myx_get_engines(mysql);

    {
      Gtk::ComboBox *combo= (Gtk::ComboBox*)_xml->get_combo("engine_combo");
      Glib::RefPtr<Gtk::ListStore> store= Gtk::ListStore::create(_columns);

      combo->set_model(store);
      //combo->pack_start(_columns.name);

      for (unsigned i= 0; i < _engines->engines_num; i++)
      {
        Gtk::TreeRow row= *store->append();
        row[_columns.name]= _engines->engines[i].name;
      }

      combo->signal_changed().connect(sigc::mem_fun(*this,&MGTableEditor::update_for_selected_engine));
    }

    {
      int major= myx_get_mysql_major_version(mysql);
      int minor= myx_get_mysql_minor_version(mysql);
      if (major > 4 || (major == 4 && minor > 0))
        _charsets= myx_dbm_retrieve_charsets(_mysql, &error);
      else
      {
        path= myg_datadir+"/mysqlx_dbm_charsets.xml";
        _charsets= myx_charsets_load(path.c_str(), &error);
      }
    }
    if (!_charsets)
      myg_show_xlib_error(_("Could not retrieve available character set information for table editor."), error);
  }
  
  if (!_data_types || !_charsets)
  {
    _xml->get_button("close_button")->clicked();
    return;
  }
  
  _catalog_data= catalog_data;
  _catalog= catalog;
  _schema= schema;
  _table= table;

  fill_aux_data();

  revert_changes();
  
  // always leave a placeholder row
  //column_add();	// revert_changes now adds the placeholder
}


Glib::ustring MGTableEditor::description_for_charset(const Glib::ustring &s)
{
  for (unsigned int i= 0; i < _charsets->charsets_num; i++)
  {
    if (s.compare(_charsets->charsets[i].name)==0)
    {
      if (*_charsets->charsets[i].desc)
        return _charsets->charsets[i].desc;
      else
        return _charsets->charsets[i].name;
    }
  }
  return DEFAULT_CHARSET;
}


MYX_ENGINE *MGTableEditor::get_selected_engine()
{
  int row= _xml->get_combo("engine_combo")->get_active_row_number();
  if (row >= 0)
      return _engines->engines+row;
  return 0;
}


bool MGTableEditor::delete_window_event(GdkEventAny *event)
{
  _xml->get_button("close_button")->clicked();
  //_xml->get_widget("table_editor")->hide();
  return true;
}


void MGTableEditor::charset_changed()
{
  Glib::ustring charset= get_combo_item(_xml->get_combo("charset_combo"), "");
  
  fill_collation_combo(_xml->get_combo("collation_combo"), charset);
}


void MGTableEditor::column_tree_clicked(GdkEventButton *ev)
{
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    _popup.popup(ev->button, ev->time);
  }
}


void MGTableEditor::column_append()
{
  _column_tree->set_cursor(Gtk::TreePath(column_add()), *_column_tree->get_column(0), true);
}


void MGTableEditor::column_insert()
{
  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();

  if (iter)
    _column_tree->set_cursor(Gtk::TreePath(column_add(iter)), *_column_tree->get_column(0), true);
}


void MGTableEditor::column_delete()
{
  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring name= row[_tcolumns.name];
    bool ok= false;
    if (name.empty())
      ok= true;
    else
    {
      Gtk::MessageDialog dlg(ufmt(_("Please confirm deletion of column '%s'."),
                                  name.c_str()),
                             false,
                             Gtk::MESSAGE_QUESTION, 
                             Gtk::BUTTONS_OK_CANCEL, true);
      if (dlg.run() == Gtk::RESPONSE_OK)
        ok= true;
    }
    if (ok)
      _column_store->erase(iter);
  }
}


bool MGTableEditor::column_tree_key_released(GdkEventKey *event)
{
  _column_tree_last_key= event->keyval;
  if (event->keyval == GDK_Tab || event->keyval == GDK_Delete)
  {
    Gtk::TreeViewColumn *column;
    Gtk::TreePath path;
    bool append_row= false;
    _column_tree->get_cursor(path, column);

    if (event->keyval == GDK_Tab)
    {
      if (column == _column_tree->get_column(0))
        _column_tree->set_cursor(path, *_column_tree->get_column(1), true);
      else
      {
        path.next();      
        if (_column_store->get_iter(path))
          _column_tree->set_cursor(path, *_column_tree->get_column(0), true);
        else
          append_row= true;
      }
    }
    else if (event->keyval == GDK_Return)
    {
      Gtk::TreeIter next;
      Gtk::TreeIter current= _column_store->get_iter(path);
      path.next();
      next= _column_store->get_iter(path);
      if (next)
      {
        Gtk::TreeRow nextr= *next;
        // set to edit next row if the next is a new row
        if (nextr[_tcolumns.name] == "")
          _column_tree->set_cursor(path, *_column_tree->get_column(0), true);
      }
      else
      {
        Gtk::TreeRow crow= *current;

        // if the current row has a name, append a new row
        if (crow[_tcolumns.name] != "")
          append_row= true;
      }
    }
    else if (event->keyval == GDK_Delete)
    {
      if (_column_tree == static_cast<Gtk::Window*>(_column_tree->get_toplevel())->get_focus())
        column_delete();
    }
    return true;
  }
  return false;
}


Gtk::TreeIter MGTableEditor::column_add(const Gtk::TreeIter &before)
{
  Gtk::TreeIter iter;

  if (before)
    iter= _column_store->insert(before);
  else
    iter= _column_store->append();

  Gtk::TreeRow row= *iter;
  
  // this was changed to empty string
  // due to ussue #6134
  // https://support.mysql.com/view.php?id=6134
  // new behavior corresponds to the behavior of windows version of QB
  row[_tcolumns.type]= "";//"INT(11)"; // default type
  row[_tcolumns.icon]= _column_icon;
  row[_tcolumns.pk]= false;
  row[_tcolumns.nnull]= true;
  row[_tcolumns.autoinc]= false;
  row[_tcolumns.charset]= DEFAULT_CHARSET;
  row[_tcolumns.collation]= DEFAULT_CHARSET;

  return iter;
}


void MGTableEditor::column_edited(const Glib::ustring &path, const Glib::ustring &new_text, int column)
{
  Gtk::TreeIter iter= _column_store->get_iter(path);
  Gtk::TreeRow row= *iter;
  bool changed= false;
  Glib::ustring final_text;

  if (_showing_column_info)
    return;

  switch (column)
  {
  case 0: // name
    if (row[_tcolumns.name] != new_text)
    {
      row[_tcolumns.name]= new_text; // we need to manually set the value here
      _xml->get_entry("column_name_entry")->set_text(new_text);
      changed= true;
    }
    break;
  case 1: // type
    //if (row[_tcolumns.type] != new_text)
    {
      bool ok= false;
      if (check_datatype(new_text))
      {
        ((Gtk::Entry*)_xml->get_combo_entry("datatype_combo")->get_child())->set_text(new_text);
        ok= true;
      }
      else
        row[_tcolumns.type]= ((Gtk::Entry*)_xml->get_combo_entry("datatype_combo")->get_child())->get_text();
      {
        Glib::ustring a, b;
        split_datatype((Glib::ustring)row[_tcolumns.type], a, b);

        row[_tcolumns.icon]= get_icon_for_column(find_datatype(_data_types, a),
                                                 row[_tcolumns.pk]);
      }
      if (ok)
        column_option_changed("type");
    }
    break;
  case 5: // default
    if (new_text == "NULL" || str_is_numeric(new_text.c_str()) ||
        strcasecmp(new_text.c_str(), "CURRENT_TIMESTAMP")==0)
      final_text= new_text;
    else
    {
      Glib::ustring old_text= row[_tcolumns.defval];
      // if the default value was empty before, add ''
      if (old_text == "" && new_text[0] != '\'')
        final_text= "'" + new_text +"'";
      else
        // if there was a value before, use the new text as it is
        // so the user might have removed the '' if he likes to
        final_text= new_text;
    }
    _xml->get_entry("default_entry")->set_text(final_text);
    row[_tcolumns.defval]= final_text;
    row[_tcolumns.defnull]= false;
    _xml->get_toggle("null_toggle")->set_active(false);
    break;
  case 6: // comment
    {
      Glib::ustring old_text= _xml->get_text("comment_text")->get_buffer()->get_text();
      Glib::ustring::size_type p= old_text.find('\n');
      if (p == Glib::ustring::npos)
        _xml->get_text("comment_text")->get_buffer()->set_text(new_text);
      else
      {
        _xml->get_text("comment_text")->get_buffer()->set_text(new_text+old_text.substr(p));
      }
      break;
    }
  }

  if ((unsigned int)Gtk::TreePath(path).front() == _column_store->children().size()-1 && changed)
    column_add();
}


void MGTableEditor::column_toggled(const Glib::ustring &path, int column)
{
  Gtk::TreeIter iter= _column_store->get_iter(path);
  Gtk::TreeRow row= *iter;

  if (_anti_recursion)
    return;
  switch (column)
  {
  case 2: // not null
    _anti_recursion= true;
#if (GTKMM_MAJOR_VERSION == 2) && (GTKMM_MINOR_VERSION < 8)
    // with gtkmm 2.6, the flag seems to get automatically toggled
#else
    row[_tcolumns.nnull]= !row[_tcolumns.nnull];
#endif
    if (_xml->get_toggle("nnull_check")->get_active() != row[_tcolumns.nnull])
      _xml->get_toggle("nnull_check")->set_active(row[_tcolumns.nnull]);
    _anti_recursion= false;
    break;
  case 3: // autoinc
    _anti_recursion= true;
#if (GTKMM_MAJOR_VERSION == 2) && (GTKMM_MINOR_VERSION < 8)
#else
    row[_tcolumns.autoinc]= !row[_tcolumns.autoinc];
#endif
    if (_xml->get_toggle("autoinc_check")->get_active() != row[_tcolumns.autoinc])
      _xml->get_toggle("autoinc_check")->set_active(row[_tcolumns.autoinc]);
    _anti_recursion= false;
    break;
  default:
    update_pk_index();
    break;
  }
}


void MGTableEditor::column_option_toggled(const Glib::ustring &path)
{
  Glib::ustring flags;
  Gtk::TreeRow row= *_flag_store->get_iter(path);

#if (GTKMM_MAJOR_VERSION == 2) && (GTKMM_MINOR_VERSION < 8)
    // with gtkmm 2.6, the flag seems to get automatically toggled
#else
  row[_flag_columns.value]= !row[_flag_columns.value];
#endif

  for (Gtk::TreeIter iter= _flag_store->children().begin();
       iter != _flag_store->children().end(); ++iter)
  {
    Gtk::TreeRow row= *iter;
    
    if (row[_flag_columns.value])
    {
      if (flags.empty())
        flags= row[_flag_columns.name];
      else
        flags+= ","+row[_flag_columns.name];
    }
  }

  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();
  row= *iter;

  row[_tcolumns.flags]= flags;
}


void MGTableEditor::column_option_changed(const char *option)
{
  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();

  if (iter && !_anti_recursion && !_showing_column_info)
  {
    Gtk::TreeRow row= *iter;

    if (strcmp(option,"name")==0)
    {
      row[_tcolumns.name]= _xml->get_entry("column_name_entry")->get_text();

      if ((unsigned int)Gtk::TreePath(_column_store->get_path(iter)).front() == _column_store->children().size()-1)
        column_add();

      update_pk_index();
    }
    else if (strcmp(option, "type")==0)
    {
      Glib::ustring type, param, otype;
      
      split_datatype((Glib::ustring)row[_tcolumns.type], otype, param);

      row[_tcolumns.type]= ((Gtk::Entry*)_xml->get_combo_entry("datatype_combo")->get_child())->get_text();

      split_datatype((Glib::ustring)row[_tcolumns.type], type, param);
      if (type != otype)
      {
        _flag_store->clear();
        for (unsigned int i= 0; i < _data_types->datatypes_num; i++)
        {
          if (strcasecmp(type.c_str(), _data_types->datatypes[i].name)==0)
          {
            Glib::ustring flags= row[_tcolumns.flags];
            for (unsigned int j= 0; j < _data_types->datatypes[i].flags_num; j++)
            {
              if (_data_types->datatypes[i].flags[j])
              {
                Gtk::TreeIter iter= _flag_store->append();
                Gtk::TreeRow row= *iter;
              
                row[_flag_columns.value]= false;
                row[_flag_columns.name]= _data_types->datatypes[i].flags[j];
              }
            }

            row[_tcolumns.icon]= get_icon_for_column(_data_types->datatypes+i, row[_tcolumns.pk]);
          }
        }
      }
    }
    else if (strcmp(option, "default")==0)
    {
      Glib::ustring new_text= _xml->get_entry("default_entry")->get_text();

      if (new_text == "NULL" || str_is_numeric(new_text.c_str()) ||
          strcasecmp(new_text.c_str(), "CURRENT_TIMESTAMP")==0)
        ;
      else
      {
        // if the default value was empty before, add ''
        if ((Glib::ustring)row[_tcolumns.defval] == "" && new_text[0] != '\'')
          new_text= "'" + new_text +"'";
        else
          // if there was a value before, use the new text as it is
          // so the user might have removed the '' if he likes to
          ;
      }
      row[_tcolumns.defval]= new_text;
    }
    else if (strcmp(option, "defnull")==0)
    {
      bool flag= _xml->get_toggle("null_toggle")->get_active();
      if (flag)
      {
        row[_tcolumns.defnull]= true;
        row[_tcolumns.defval]= "NULL";
      }
      else
      {
        row[_tcolumns.defnull]= false;
        row[_tcolumns.defval]= "";
        _xml->get_entry("default_entry")->set_text("");
      }
      _xml->get_entry("default_entry")->set_sensitive(!flag);
    }
    else if (strcmp(option, "pk")==0)
    {
      row[_tcolumns.pk]= _xml->get_toggle("pk_check")->get_active();
      row[_tcolumns.icon]= get_icon_for_column(find_datatype(_data_types, row[_tcolumns.type]),
                                               row[_tcolumns.pk]);
      
      update_pk_index();
    }
    else if (strcmp(option, "nn")==0)
      row[_tcolumns.nnull]= _xml->get_toggle("nnull_check")->get_active();
    else if (strcmp(option, "autoinc")==0)
      row[_tcolumns.autoinc]= _xml->get_toggle("autoinc_check")->get_active();
    else if (strcmp(option, "comment")==0)
    {
      Glib::ustring text= _xml->get_text("comment_text")->get_buffer()->get_text();
      row[_tcolumns.comment]= text.substr(0, text.find('\n'));
      row[_tcolumns.comment_full]= text;
    }
    else if (strcmp(option, "charset")==0)
    {      
      row[_tcolumns.charset]= get_combo_item(_xml->get_combo("column_charset_combo"), "");
      Glib::ustring s= row[_tcolumns.charset];

      fill_collation_combo(_xml->get_combo("column_collation_combo"),
                           row[_tcolumns.charset]);
    }
    else if (strcmp(option, "collation")==0)
    {
      row[_tcolumns.collation]= get_combo_item(_xml->get_combo("column_collation_combo"), "");
    }
  }
}


void MGTableEditor::column_drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, Gtk::SelectionData &selection_data, guint info, guint time)
{
  Gtk::TreeIter iter= _column_tree->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    Glib::ustring column= row[_tcolumns.name];

    selection_data.set("x-mysqlgui-table-column", column);
  }
}

    
void MGTableEditor::index_drop_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData &selection_data, guint info, guint time)
{
  //ugh. required to make the treeview drop handler to not be called
  g_signal_stop_emission_by_name(_xml->get_tree("index_columns_tree")->gobj(), "drag_data_received");

  if ((selection_data.get_length() >= 0) && (selection_data.get_data_type() == "x-mysqlgui-table-column"))
  {
    Gtk::TreeIter iter= _index_column_store->append();
    Gtk::TreeRow row= *iter;

    row[_icl_columns.name]= selection_data.get_data_as_string();
    
    save_index_changes();
  }
  context->drag_finish(false, false, time);
}


void MGTableEditor::fk_drop_drag_data_received(const Glib::RefPtr<Gdk::DragContext>& context, int x, int y, const Gtk::SelectionData &selection_data, guint info, guint time)
{
  //ugh. required to make the treeview drop handler to not be called
  g_signal_stop_emission_by_name(_xml->get_tree("fk_column_tree")->gobj(), "drag_data_received");
  
  if ((selection_data.get_length() >= 0) && (selection_data.get_data_type() == "x-mysqlgui-table-column"))
  {
    Gtk::TreeIter iter= _fk_column_store->append();
    Gtk::TreeRow row= *iter;
    
    row[_fkm_columns.source]= selection_data.get_data_as_string();
    
    save_fk_changes();
  }
  context->drag_finish(false, false, time);
}


void MGTableEditor::index_column_edited(const Glib::ustring &path, const Glib::ustring &new_text)
{
  Gtk::TreeRow row= *_index_column_store->get_iter(path);

  row[_icl_columns.length]= new_text;

  save_index_changes();
}


void MGTableEditor::index_remove_column()
{
  Gtk::TreeIter iter= _xml->get_tree("index_columns_tree")->get_selection()->get_selected();
  if (iter)
  {
    _index_column_store->erase(iter);
    save_index_changes();
  }
}


void MGTableEditor::add_index()
{
  Gtk::TreeIter iter= _index_store->append();
  Gtk::TreeRow row= *iter;
  
  row[_il_columns.name]= "new_index";
  
  _xml->get_tree("index_tree")->get_selection()->select(iter);
}


void MGTableEditor::remove_index()
{
  Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  if (iter)
    _index_store->erase(iter);
}


void MGTableEditor::index_option_changed(const char *option)
{
  const char *t;
  Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
  
    if (strcmp(option,"name")==0) 
    {
      row[_il_columns.name]= _xml->get_entry("index_name_entry")->get_text();
    }
    else if (strcmp(option,"kind")==0)
    {
      t= _xml->get_option("index_kind_option")->get_menu()->get_active()->get_name().c_str();
      if(strcmp(t, "index1") == 0)
      {
        row[_il_columns.kind]= MYX_DBM_IK_INDEX;
      }
      else if(strcmp(t, "primary1") == 0) 
      {
        row[_il_columns.kind]= MYX_DBM_IK_PRIMARY;
      }
    }
      
//      else if (strcmp(option,"type")==0)
  }
}


void MGTableEditor::save_index_changes()
{
  Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;
  
  if (iter)
  {
    row= *iter;
    Gtk::TreeIter iter2;
    
    std::list<ILColumns::IndexColumn> columns;
    for (iter2= _index_column_store->children().begin();
         iter2 != _index_column_store->children().end();
         ++iter2)
    {
      Gtk::TreeRow row2= *iter2;
      ILColumns::IndexColumn ic;
      
      ic.name= row2[_icl_columns.name];
      ic.length= row2[_icl_columns.length];
      ic.collation= row2[_icl_columns.collation];
      columns.push_back(ic);
    }
    row[_il_columns.columns]= columns;
  }
}


void MGTableEditor::add_fk()
{  
  MYX_ENGINE *engine= get_selected_engine();
  
  if (engine && strcasecmp(engine->name, "innodb")==0)
  {
    Gtk::TreeIter iter= _fk_store->append();
    Gtk::TreeRow row= *iter;

    row[_fk_columns.name]= "new_fk_constraint";
    row[_fk_columns.on_delete]= MYX_DBM_FA_RESTRICT;
    row[_fk_columns.on_update]= MYX_DBM_FA_RESTRICT;

    _xml->get_tree("fk_tree")->get_selection()->select(iter);
  }
  else
  {
    if (_windowed)
      myg_show_info(*(Gtk::Window*)_xml->get_widget("table_editor"), 
                    _("Foreign Keys can only be defined for tables that use the InnoDB engine.\n"
                      "Make this an InnoDB table by selecting 'InnoDB' in the 'Table Options' tab."));
    else
      myg_show_info(_("Foreign Keys can only be defined for tables that use the InnoDB engine.\n"
                      "Make this an InnoDB table by selecting 'InnoDB' in the 'Table Options' tab."));
  }
}


void MGTableEditor::remove_fk()
{
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  if (iter)
    _fk_store->erase(iter);
}


void MGTableEditor::save_fk_changes()
{
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;
  
  if (iter)
  {
    row= *iter;
    Gtk::TreeIter iter2;
    
    std::list<FKColumns::FKMapping> columns;
    for (iter2= _fk_column_store->children().begin();
         iter2 != _fk_column_store->children().end();
         ++iter2)
    {
      Gtk::TreeRow row2= *iter2;
      FKColumns::FKMapping fkm;
      fkm.source= row2[_fkm_columns.source];
      fkm.target= row2[_fkm_columns.target];
      columns.push_back(fkm);
    }
    row[_fk_columns.columns]= columns;
  }
}


void MGTableEditor::fk_option_changed(const char *option)
{
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;
  
  if (iter)
  {
    row= *iter;

    if (strcmp(option,"name")==0)
      row[_fk_columns.name]= _xml->get_entry("fk_name_entry")->get_text();
    else if (strcmp(option,"ondelete")==0)
      row[_fk_columns.on_delete]= (MYX_DBM_FK_ACTION)_xml->get_option("ondelete_option")->get_history();
    else if (strcmp(option,"onupdate")==0)
      row[_fk_columns.on_update]= (MYX_DBM_FK_ACTION)_xml->get_option("onupdate_option")->get_history();
    else if (strcmp(option,"table")==0)
      row[_fk_columns.table]= ((Gtk::Entry*)_xml->get_combo_entry("fk_table_combo")->get_child())->get_text();
  }
}


void MGTableEditor::fk_column_edited(const Glib::ustring &path, const Glib::ustring &new_text)
{
  Gtk::TreeRow row= *_fk_column_store->get_iter(path);

  row[_fkm_columns.target]= new_text;

  save_fk_changes();
}



void MGTableEditor::fk_add_column()
{
  Gtk::TreeIter iter= _fk_column_store->append();
  Gtk::TreeRow row= *iter;
  
  row[_fk_columns.name]= "column";
}

void MGTableEditor::fk_remove_column()
{
  Gtk::TreeIter iter= _xml->get_tree("fk_column_tree")->get_selection()->get_selected();
  if (iter)
    _fk_column_store->erase(iter);
}


void MGTableEditor::revert_changes()
{
  if (_data)
  {
    myx_dbm_free_table_data(_data);
    _data= NULL;
  }

  MYX_LIB_ERROR error;
  
  if (!_table.empty() && !_creating_table)
  {
    _data= myx_dbm_retrieve_table_data(_mysql, _data_types, _engines,
                                       _catalog.c_str(), _schema.c_str(),
                                       _table.c_str(), &error);
    if (!_data)
    {
      if (error == MYX_SQL_ERROR)
        myg_show_mysql_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                             ufmt(_("Could not retrieve information for table '%s.%s'"),
                                  _schema.c_str(), _table.c_str()), _mysql);
      else
        myg_show_xlib_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                            ufmt(_("Error retrieving information for table '%s.%s'"),
                                 _schema.c_str(), _table.c_str()), error);
    }
    else
    {
      if (_windowed)
        _xml->get_widget("table_editor")->show();

      show_dbm(_data);
    }
  }
  else
  {
    if (_windowed)
      _xml->get_widget("table_editor")->show();

    _data= (MYX_DBM_TABLE_DATA*)g_malloc0(sizeof(MYX_DBM_TABLE_DATA));
    
    _data->schema= g_strdup(_schema.c_str());

    // set the default table option
    for (unsigned int i= 0; i < _engines->engines_num; i++)
    {
      if (_engines->engines[i].isdefault)
      {
        _data->storage_engine= _engines->engines+i;
        break;
      }
    }

    show_dbm(_data);
  }
  column_add();
}


bool MGTableEditor::validate()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  bool flag;

  if (_xml->get_entry("name_entry")->get_text().empty())
  {
    Gtk::MessageDialog dlg(_("<b>Cannot Create Table</b>\n\nPlease specify a name for the table."),
                           true, Gtk::MESSAGE_ERROR);
    dlg.run();

    return false;
  }
  
  // remove blank columns
  do {
    flag= false;
    for (iter= _column_store->children().begin(); iter!= _column_store->children().end();
         ++iter)
    {
      row= *iter;
      if (((Glib::ustring)row[_tcolumns.name]).empty())
      {
        _column_store->erase(iter);
        flag= true;
        break;
      }
      if (((Glib::ustring)row[_tcolumns.type]).empty())
      {
        Gtk::MessageDialog dlg(_("<b>Cannot Create Table</b>\n\nOne or more columns are missing their data types."),
                               true, Gtk::MESSAGE_ERROR);
        dlg.run();
        return false;
      }
    }
  } while (flag);
  
  if (_column_store->children().size() == 0)
  {
    Gtk::MessageDialog dlg(_("<b>Cannot Create Table</b>\n\nThere are no columns defined for the table."),
                           true, Gtk::MESSAGE_ERROR);
    column_add();
    return false;
  }  
  return true;
}


void MGTableEditor::update_pk_index()
{
  bool has_primary_index= false;
  Gtk::TreeIter iter, pkiter;

  // look if there's a PRIMARY index
  for (iter= _index_store->children().begin(); iter != _index_store->children().end(); ++iter)
  {
    Gtk::TreeRow row= *iter;
    
    if (row[_il_columns.name] == "PRIMARY")
    {
      has_primary_index= true;
      pkiter= iter;
      break;
    }
  }
  
  std::list<ILColumns::IndexColumn> index_columns;
  // rebuild PK column list
  for (iter= _column_store->children().begin(); iter != _column_store->children().end(); ++iter)
  {
    Gtk::TreeRow row= *iter;
    
    if (row[_tcolumns.pk])
    {
      ILColumns::IndexColumn ic;

      ic.name= row[_tcolumns.name];
      index_columns.push_back(ic);
    }
  }

  if (index_columns.empty())
  {
    if (has_primary_index)
      _index_store->erase(pkiter);
  }
  else
  {
    if (has_primary_index)
    {
      Gtk::TreeRow row= *pkiter;

      row[_il_columns.columns]= index_columns;
    }
    else
    {
      iter= _index_store->prepend();
      Gtk::TreeRow row= *iter;
      
      // create a PRIMARY index if needed
      row[_il_columns.name]= "PRIMARY";
      row[_il_columns.kind]= MYX_DBM_IK_PRIMARY;
      row[_il_columns.type]= MYX_DBM_IT_DEFAULT;
      row[_il_columns.columns]= index_columns;
    }
  }
  
  index_selected();
}


int MGTableEditor::execute_sql_statement(const char *sql, void *userdata)
{
  MGTableEditor *me= (MGTableEditor*)userdata;
  MYX_LIB_ERROR error;

  long long int affected_rows= 0L;

  if (me->_script_execute_status == MYX_NO_ERROR)
  {
    myx_query_execute_direct(me->_mysql, sql, &error, &affected_rows);

    me->_script_execute_status= error;
  }

  return 0;
}


MYX_LIB_ERROR MGTableEditor::execute_sql_script(const Glib::ustring &script)
{
  _script_execute_status= MYX_NO_ERROR;

  myx_process_sql_statements(script.c_str(), MGTableEditor::execute_sql_statement, this, MYX_SPM_NORMAL_MODE);

  return _script_execute_status;
}


void MGTableEditor::commit_changes()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;

  if (!_data)
    return;
  
  if (!validate())
    return;

#define ASSIGN(var, val) do { if ((val).compare(var?:"")!=0) {if (var) g_free(var); var= g_strdup((val).c_str());} } while (0)
#define ASSIGN_N(var, val) do { if ((val).compare(var?:"")!=0) {if (var) g_free(var); if ((val).empty()) var= NULL; else var= g_strdup((val).c_str());} } while (0)
#define SETNULL(var) do { if (var) g_free(var); var= NULL; } while (0)
  
  // table options
  ASSIGN(_data->name, _xml->get_entry("name_entry")->get_text());
//  ASSIGN(_data->schema, ((Gtk::Entry*)_xml->get_combo_entry("database_combo")->get_child())->get_text());
  ASSIGN(_data->comment, _xml->get_entry("comment_entry")->get_text());
  
  // more table options
  _data->storage_engine= get_selected_engine();

  ASSIGN(_data->merge_union, _xml->get_entry("merge_tables_entry")->get_text());
  
  _data->merge_insert= (MYX_DBM_TABLE_MERGE_INSERT)_xml->get_option("insert_method_option")->get_history();
  
  if (get_combo_item(_xml->get_combo("charset_combo"), DEFAULT_CHARSET) == DEFAULT_CHARSET)
    SETNULL(_data->charset);
  else
  {
    Gtk::TreeIter iter= _xml->get_combo("charset_combo")->get_active();
    Gtk::TreeRow row= *iter;
    Glib::ustring charset= (char*)(void*)row[_columns.object];
    ASSIGN_N(_data->charset, charset);
  }
  if (get_combo_item(_xml->get_combo("collation_combo"), "") == "")
    SETNULL(_data->collation);
  else
    ASSIGN_N(_data->collation, get_combo_item(_xml->get_combo("collation_combo"), ""));

  // advanced table options
  ASSIGN_N(_data->next_auto_inc, _xml->get_entry("next_autoinc_entry")->get_text());
  ASSIGN_N(_data->password, _xml->get_entry("password_entry")->get_text());
  _data->delay_key_write= _xml->get_toggle("delay_update_check")->get_active();

  ASSIGN_N(_data->table_data_dir, _xml->get_entry("datadir_entry")->get_text());
  ASSIGN_N(_data->table_index_dir, _xml->get_entry("indexdir_entry")->get_text());

  _data->pack_keys= (MYX_DBM_TABLE_PACK_KEYS)_xml->get_option("pack_option")->get_history();

  _data->raid_type= (MYX_DBM_TABLE_RAID_TYPE)_xml->get_option("raid_option")->get_history();
  ASSIGN_N(_data->raid_chunks, _xml->get_spin("chunk_num_spin")->get_text());
  ASSIGN_N(_data->raid_chunk_size, _xml->get_spin("chunk_size_spin")->get_text());
  
  _data->row_format= (MYX_DBM_TABLE_ROW_FORMAT)_xml->get_option("format_option")->get_history();
  ASSIGN_N(_data->avg_row_length, _xml->get_entry("avg_length_entry")->get_text());
  ASSIGN_N(_data->min_rows, _xml->get_entry("min_rows_entry")->get_text());
  ASSIGN_N(_data->max_rows, _xml->get_entry("max_rows_entry")->get_text());
  
  _data->checksum= _xml->get_toggle("checksum_check")->get_active();

  // columns
  {
    unsigned int ncolumns_num= _column_store->children().size();
    MYX_DBM_COLUMN_DATA *ncolumns= g_new0(MYX_DBM_COLUMN_DATA, ncolumns_num);
    unsigned int c= 0;
    
    for (iter= _column_store->children().begin();
         iter != _column_store->children().end(); ++iter)
    {
      MYX_DBM_COLUMN_DATA *column;
      MYX_DBM_COLUMN_DATA *original_column;
      Glib::ustring a,b;
      
      row= *iter;
      original_column= (MYX_DBM_COLUMN_DATA*)row[_tcolumns.object];
      column= ncolumns+ c++;

      column->primary_key= row[_tcolumns.pk];
      ASSIGN(column->name, (Glib::ustring)row[_tcolumns.name]);

      column->original_name= original_column?g_strdup(original_column->original_name):NULL;

      split_datatype((Glib::ustring)row[_tcolumns.type], a, b);
      column->datatype_pointer= find_datatype(_data_types, a);

      ASSIGN(column->datatype_name, a);
      ASSIGN_N(column->datatype_params, b);
      column->not_null= row[_tcolumns.nnull];
      column->auto_inc= row[_tcolumns.autoinc];
      column->datatype_flags= g_strsplit(((Glib::ustring)row[_tcolumns.flags]).c_str(), ",", -1);
      column->datatype_flags_num= 0;
      while (column->datatype_flags[column->datatype_flags_num])
        column->datatype_flags_num++;
      column->default_value_is_null= row[_tcolumns.defnull];
      ASSIGN(column->default_value, (Glib::ustring)row[_tcolumns.defval]);
      if (!column->default_value) column->default_value= g_strdup("");
      ASSIGN_N(column->comment, (Glib::ustring)row[_tcolumns.comment_full]);
      a= row[_tcolumns.charset];
      if (a != DEFAULT_CHARSET)
        ASSIGN_N(column->charset, a);
      a= row[_tcolumns.collation];
      if (a != DEFAULT_CHARSET)
        ASSIGN_N(column->collation, a);
      if (column->charset == NULL)
        //column->charset= g_strdup("NULL");
        column->charset= g_strdup("");
      if (column->collation == NULL)
        //column->collation= g_strdup("NULL");
        column->collation= g_strdup("");
      
      row[_tcolumns.object]= column;
    }

    // replace the column list

    // first free the old one
    for (unsigned int i= 0; i < _data->columns_num; i++)
    {
      MYX_DBM_COLUMN_DATA *column= _data->columns+i;

      g_free(column->name);
      g_free(column->original_name);
      
      g_free(column->datatype_name);
      g_free(column->datatype_params);
      
      g_free(column->default_value);
      g_free(column->comment);
      
      for(unsigned int j= 0; j < column->datatype_flags_num; j++)
        g_free(column->datatype_flags[j]);
      g_free(column->datatype_flags);
    }
    g_free(_data->columns);

    _data->columns= ncolumns;
    _data->columns_num= ncolumns_num;
  }

  {
    // indices
    unsigned int nindices_num= _index_store->children().size();
    MYX_DBM_INDEX_DATA *nindices= g_new0(MYX_DBM_INDEX_DATA, nindices_num);
    unsigned int c= 0;

    for (iter= _index_store->children().begin();
         iter != _index_store->children().end(); ++iter)
    {
      MYX_DBM_INDEX_DATA *index;
      MYX_DBM_INDEX_DATA *original_index;

      row= *iter;
      index= nindices + c++;
      original_index= (MYX_DBM_INDEX_DATA*)(void*)row[_il_columns.object];

      ASSIGN(index->name, (Glib::ustring)row[_il_columns.name]);
      index->original_name= original_index?g_strdup(original_index->original_name):NULL;

      index->index_kind= row[_il_columns.kind];
      index->index_type= row[_il_columns.type];
      
      // create new list with index columns
      std::list<ILColumns::IndexColumn> ixcolumns= row[_il_columns.columns];
      index->columns_num= ixcolumns.size();
      index->columns= g_new0(MYX_DBM_INDEX_COLUMN_DATA,index->columns_num);
      unsigned int ci=0;
      for (std::list<ILColumns::IndexColumn>::const_iterator cit= ixcolumns.begin();
           cit != ixcolumns.end(); ++cit, ci++)
      {
        ASSIGN(index->columns[ci].name, cit->name);
        ASSIGN(index->columns[ci].len, cit->length);
        ASSIGN_N(index->columns[ci].value_order, cit->collation);
      }

      row[_il_columns.object]= index;
    }
    
    // replace the index list
    for (unsigned int i= 0; i < _data->indices_num; i++)
    {
      MYX_DBM_INDEX_DATA *index= _data->indices+i;
      g_free(index->name);
      g_free(index->original_name);
      for (unsigned int j= 0; j < index->columns_num; j++)
      {
        g_free(index->columns[j].name);
        g_free(index->columns[j].len);
        g_free(index->columns[j].value_order);
      }
      g_free(index->columns);
    }
    g_free(_data->indices);
    _data->indices= nindices;
    _data->indices_num= nindices_num;
  }

  {
    // fks
    unsigned int nfk_num= _fk_store->children().size();
    MYX_DBM_FK_DATA *nfk= g_new0(MYX_DBM_FK_DATA, nfk_num);
    unsigned int c= 0;

    for (iter= _fk_store->children().begin();
         iter != _fk_store->children().end(); ++iter)
    {
      MYX_DBM_FK_DATA *fk;
      MYX_DBM_FK_DATA *original_fk;

      row= *iter;
      fk= nfk + c++;
      original_fk= (MYX_DBM_FK_DATA*)(void*)row[_fk_columns.object];

      ASSIGN(fk->name, (Glib::ustring)row[_fk_columns.name]);
      fk->original_name= original_fk?g_strdup(original_fk->original_name):NULL;

      fk->on_delete= row[_fk_columns.on_delete];
      fk->on_update= row[_fk_columns.on_update];
      split_schema_table(((Glib::ustring)row[_fk_columns.table]).c_str(),
                         &fk->reference_schema_name,
                         &fk->reference_table_name);

      // create new list with fk columns
      std::list<FKColumns::FKMapping> list= row[_fk_columns.columns];
      fk->column_mapping_num= list.size();
      fk->column_mapping= g_new0(MYX_NAME_VALUE_PAIR,fk->column_mapping_num);
      unsigned int ci=0;
      for (std::list<FKColumns::FKMapping>::const_iterator cit= list.begin();
           cit != list.end(); ++cit, ci++)
      {
        ASSIGN(fk->column_mapping[ci].name, cit->source);
        ASSIGN(fk->column_mapping[ci].value, cit->target);
      }

      row[_fk_columns.object]= fk;
    }

    for (unsigned int i= 0; i < _data->fks_num; i++)
    {
      MYX_DBM_FK_DATA *fk= _data->fks+i;
      
      g_free(fk->name);
      g_free(fk->original_name);
      g_free(fk->reference_table_name);    
      for (unsigned int j= 0; j < fk->column_mapping_num; j++)
      {
        g_free(fk->column_mapping[j].name);
        g_free(fk->column_mapping[j].value);
      }
      g_free(fk->column_mapping);
    }
    g_free(_data->fks);
    _data->fks_num= nfk_num;
    _data->fks= nfk;
  }  

  {
    MYX_LIB_ERROR error;
    char *script= NULL;
    MYX_DBM_SERVER_VERSION server_version;
        
    server_version.major_version= myx_get_mysql_major_version(_mysql);
    server_version.minor_version= myx_get_mysql_minor_version(_mysql);

    MYX_DBM_TABLE_DATA *original= myx_dbm_retrieve_table_data(_mysql, _data_types, _engines,
                                                              _catalog.c_str(), _schema.c_str(),
                                                              _table.c_str(), &error);
    if (!original)
    {
      if (!_creating_table) // modified existing table
      {
        if (error == MYX_SQL_ERROR)
          myg_show_mysql_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                               ufmt(_("Could not retrieve information for table '%s.%s' to perform table diff."),
                                    _schema.c_str(), _table.c_str()), _mysql);
        else
          myg_show_xlib_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                              ufmt(_("Could not retrieve information for table '%s.%s' to perform table diff."),
                                   _schema.c_str(), _table.c_str()), error);
      }
    }
    
    if (original || _creating_table)
    {
      script= myx_dbm_get_table_sql_diff(original, _data, &server_version, &error);
      if (original)
        myx_dbm_free_table_data(original);
      if (!script || !*script)
      {
        if (error != MYX_NO_ERROR)
          myg_show_xlib_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                              _("Could not generate differences between original table and modified table."),
                              error);
        else
          myg_show_info(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                        _("No modifications to be applied."));
      }
    }

    if (script && *script)
    {
      Glib::ustring tmp= script;
      g_free(script);
      
      if (!tmp.empty() && confirm_apply(tmp))
      {
        MYX_LIB_ERROR error;
        Glib::ustring msg;
        
        if (_creating_table)
          msg= _("Error executing SQL commands to create table.");
        else
          msg= _("Error executing SQL commands to update table.");

        error= execute_sql_script(tmp);

        if (error == MYX_SQL_ERROR)
          myg_show_mysql_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                               msg, _mysql);
        else if (error != MYX_NO_ERROR)
          myg_show_xlib_error(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                              msg, error);
        else
        {
          if (_creating_table)
            myg_show_info(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                          _("Table created."));
          else
            myg_show_info(*(Gtk::Window*)_xml->get_widget("top_frame")->get_toplevel(),
                          _("Table modifications applied."));

          // for when the table is renamed
          _table= _xml->get_entry("name_entry")->get_text();

          // reload newly saved data
          revert_changes();
        }
      }
    }
    else
    {
      // add back placeholder column removed in validate()
      column_add();
    }
  }
}


bool MGTableEditor::confirm_apply(Glib::ustring &query)
{
  if (!_confirm_xml)
  {
    _confirm_xml= new MGGladeXML(myg_datadir+"/table_editor.glade", "confirm_dialog");
    
    _confirm_xml->get_button("save_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::confirm_save_script));
    _confirm_xml->get_button("cancel_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::confirm_cancel));
    _confirm_xml->get_button("execute_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGTableEditor::confirm_execute));
  }

  _confirm_xml->get_text("text")->get_buffer()->set_text(query);

  ((Gtk::Window*)_confirm_xml->get_widget("confirm_dialog"))->set_transient_for(*(Gtk::Window*)_xml->get_widget("table_editor"));
  
  _confirm_xml->get_widget("confirm_dialog")->show();
  if (_creating_table)
    ((Gtk::Window*)_confirm_xml->get_widget("confirm_dialog"))->set_title(_("Confirm Table Creation"));
  else
    ((Gtk::Window*)_confirm_xml->get_widget("confirm_dialog"))->set_title(_("Confirm Table Changes"));

  _confirm_cancelled= false;
  _confirm_xml->get_button("execute_button")->grab_default();
  _confirm_xml->get_button("execute_button")->grab_focus();

  Gtk::Main::instance()->run();
  
  _confirm_xml->get_widget("confirm_dialog")->hide();
  
  if (!_confirm_cancelled)
  {
    query= _confirm_xml->get_text("text")->get_buffer()->get_text();
  }

  return !_confirm_cancelled;
}


void MGTableEditor::confirm_save_script()
{
  Gtk::FileSelection fsel(_("Save SQL Script"));
  
  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    std::string fname= fsel.get_filename(); 
    int fd= open(fname.c_str(), O_CREAT|O_WRONLY, 0644);
    Glib::ustring data= _confirm_xml->get_text("text")->get_buffer()->get_text();
    
    if (fd < 0)
    {
      myg_show_sys_error(ufmt(_("Could not create file '%s'."), fname.c_str()), errno);
      return;
    }
    if (write(fd, data.data(), data.bytes()) < 0)
    {
      myg_show_sys_error(ufmt(_("Could not save to file '%s'."), fname.c_str()), errno);
    }
    close(fd);
  }
}


void MGTableEditor::confirm_cancel()
{
  _confirm_cancelled= true;
  Gtk::Main::instance()->quit();
}

void MGTableEditor::confirm_execute()
{
  Gtk::Main::instance()->quit();
}


void MGTableEditor::close_confirm_dialog(GdkEventAny *ev)
{
  _confirm_xml->get_widget("confirm_dialog")->show();
  Gtk::Main::instance()->quit();
}


Glib::SignalProxy0<void> MGTableEditor::signal_close()
{
  return _xml->get_button("close_button")->signal_clicked();
}


Gtk::Widget *MGTableEditor::get_widget()
{
  return _xml->get_widget("top_frame");
}
