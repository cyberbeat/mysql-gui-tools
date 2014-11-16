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


#include "MGTableBrowserList.h"
#include "myx_public_interface.h"
#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "mygpriv.h"


Glib::ustring MGSchemaObjectNameFromTable(const Glib::ustring &catalog,
                                        const Glib::ustring &schema,
                                        const Glib::ustring &table)
{
  return "Table\nC:"+catalog+"\nS:"+schema+"\nT:"+table;
}


Glib::ustring MGSchemaObjectNameFromTableColumn(const Glib::ustring &catalog,
                                                const Glib::ustring &schema,
                                                const Glib::ustring &table,
                                                const Glib::ustring &column)
{
  return "Table\nC:"+catalog+"\nS:"+schema+"\nT:"+table+"\nT:"+column;
}


bool MGTableFromSchemaObjectName(const Glib::ustring &objectName,
                                 Glib::ustring &catalog,
                                 Glib::ustring &schema,
                                 Glib::ustring &table)
{
  char *tmp= g_strdup(objectName.c_str());
  char *tok;
  char *t;
  
  tok= strtok_r(tmp, "\n", &t);
  if (strcmp2(tok, "Table")==0)
  {
    tok= strtok_r(NULL, "\n", &t);
    if (tok && strncmp(tok, "C:", 2)==0)
    {
      catalog= tok+2;
      tok= strtok_r(NULL, "\n", &t);
      if (tok && strncmp(tok, "S:", 2)==0)
      {
        schema= tok+2;
        tok= strtok_r(NULL, "\n", &t);
        if (tok && strncmp(tok, "T:", 2)==0)
        {
          table= tok+2;
          g_free(tmp);
          return true;
        }
      }
    }
  }
  return false;
}

MGTableBrowserList::MGTableBrowserList(const Glib::ustring &caption, RowType type)
    : MGBrowserList(true, caption), _tables(0),
      _table_icon(0), _column_icon(0), _leaf_type(type), 
      _show_tables(true), _show_sps(false)
{
  Gtk::TreeViewColumn *column= new Gtk::TreeView::Column("");
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  column->add_attribute(((Gtk::CellRendererText*)rends[1])->property_weight(), _columns.weight);
  ((Gtk::CellRendererText*)rends[1])->property_weight_set()= true;
  _tree->append_column(*Gtk::manage(column));

  _tree->set_name("table_browser");

  set_store(Gtk::TreeStore::create(_columns));

  std::list<Gtk::TargetEntry> targets;
  targets.push_back(Gtk::TargetEntry("x-mysqlgui-schema-object", Gtk::TARGET_SAME_APP, 0));
  _tree->drag_source_set(targets, Gdk::ModifierType(GDK_BUTTON1_MASK),
                         Gdk::DragAction(GDK_ACTION_COPY));
  _tree->signal_drag_data_get().connect(sigc::mem_fun(*this,&MGTableBrowserList::drag_data_get));

  _tree->signal_row_activated().connect(sigc::mem_fun(*this,&MGTableBrowserList::row_activated));
  _tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGTableBrowserList::row_selected));
  _tree->signal_row_collapsed().connect(sigc::mem_fun(*this, &MGTableBrowserList::on_row_collapsed));
  _tree->signal_row_expanded().connect(sigc::mem_fun(*this, &MGTableBrowserList::on_row_expanded));

  _schema_icon= PIXCACHE->load("myx_schema_16x16.png");
  _table_icon= PIXCACHE->load("myx_schema_table_16x16.png");
  _sp_icon= PIXCACHE->load("myx_schema_sp_16x16.png");
  _view_icon= PIXCACHE->load("myx_schema_view_16x16.png");

  _column_icon= PIXCACHE->load("16x16_Field.png");
  _key_icon= PIXCACHE->load("16x16_KeyColumn.png");

  update_menu();
}

void MGTableBrowserList::set_selection_info(const char *schema_name, 
  const char *entity_name, RowType rowtype, bool schema_exp)
{
  if(schema_name != NULL)
    _selected_schema_name= schema_name;
  if(entity_name != NULL)
    _selected_schema_entity_name= entity_name;
  _selected_row_type= rowtype;
  _schema_is_expanded= schema_exp;
}

void MGTableBrowserList::on_row_expanded(const Gtk::TreeModel::iterator& iter, const Gtk::TreeModel::Path&)
{
  if (iter && get_type(iter) == Schema)
  {
    Gtk::TreeRow row= *iter;
    void *data= row[_columns.data];
    MYX_SCHEMA *schema= (MYX_SCHEMA*)data;

    // gtk allows to expand a tree node w/o focusing it
    // we keep expanded/collapsed state only for selected node
    if(_selected_schema_name.compare(schema->schema_name) == 0)
      set_selection_info(schema->schema_name, "", Schema, true);
  }
}

void MGTableBrowserList::on_row_collapsed(const Gtk::TreeModel::iterator& iter, const Gtk::TreeModel::Path&)
{
  if (iter && get_type(iter) == Schema)
  {
    Gtk::TreeRow row= *iter;
    void *data= row[_columns.data];
    MYX_SCHEMA *schema= (MYX_SCHEMA*)data;

    // gtk allows to expand a tree node w/o focusing it
    // we keep expanded/collapsed state only for selected node
    if(_selected_schema_name.compare(schema->schema_name) == 0)
      set_selection_info(schema->schema_name, "", Schema, false);
  }
}

void MGTableBrowserList::update_menu()
{
  std::vector<Glib::ustring> items;

  items.push_back(_("All"));
  if (_leaf_type >= Schema)
    items.push_back(_("Schemas"));
  if (_leaf_type >= Table)
    items.push_back(_("Tables"));
  if (_show_sps)
    items.push_back(_("Procedures/Functions"));

  set_menu_items(items);
}


int MGTableBrowserList::get_search_type()
{
  if (_selected_menu_item < 0)
    return 'A';

  int i;
  if (_selected_menu_item == 0)
    return 'A';
  i= 0;
  if (_leaf_type >= Schema)
  {
    i++;
    if (_selected_menu_item == i)
      return 'S';
  }
  if (_leaf_type >= Table)
  {
    i++;
    if (_selected_menu_item == i)
      return 'T';
  }
  if (_show_sps)
  {
    i++;
    if (_selected_menu_item == i)
      return 'P';
  }
  return 'A';
}


void MGTableBrowserList::set_show_sps(bool flag)
{
  _show_sps= flag;

  update_menu();
}


void MGTableBrowserList::row_activated(const Gtk::TreeModel::Path &path, Gtk::TreeViewColumn *col)
{
  Gtk::TreeIter iter= _store->get_iter(path);
  Gtk::TreeRow row= *iter;
/*  
  if (row.children().size()>0)
  {
    if (_tree->row_expanded(path))
      _tree->collapse_row(path);
    else
      _tree->expand_row(path, false);
  }
 */
  _signal_row_activate.emit();
}


void MGTableBrowserList::row_selected()
{
  Gtk::TreeIter iter= get_selected();

  if(!iter)
    return;

  if (get_type(iter) == Schema)
  {
    Gtk::TreeRow row= *iter;
    void *data= row[_columns.data];
    MYX_SCHEMA *schema= (MYX_SCHEMA*)data;

    set_selection_info(schema->schema_name, "", Schema, true);

    row[_columns.icon]= PIXCACHE->load("busy.png");
    // flush drawing
    while (Gtk::Main::events_pending())
      Gtk::Main::iteration(false);

    if (_show_tables && schema->schema_tables == NULL)
    {
      Gtk::TreeRow prow= *row.parent();
      MYX_SCHEMA_TABLES *tables= 0;
  
      if (_fetch_schema_tables(schema->catalog_name?:"def", schema->schema_name, tables))
      {
        refresh_table_list(iter, tables, "");
        schema->schema_tables= tables;
      }
    }
    if (_show_sps && schema->schema_sps == NULL)
    {
      Gtk::TreeRow prow= *row.parent();
      MYX_SCHEMA_STORED_PROCEDURES *sps= 0;

      if (_fetch_schema_sps(schema->catalog_name?:"def", schema->schema_name, sps))
      {
        refresh_sp_list(iter, sps, "");
        schema->schema_sps= sps;
      }
    }
    row[_columns.icon]= _schema_icon;
  }
  else if (get_type(iter) == Table)
  {
    Gtk::TreeRow row= *iter;
    void *data= row[_columns.data];
    MYX_SCHEMA_TABLE *table= (MYX_SCHEMA_TABLE *)data;

    set_selection_info(NULL, table->table_name, Table, true);
  }
  else if (get_type(iter) == SP)
  {
    Gtk::TreeRow row= *iter;
    void *data= row[_columns.data];
    MYX_SCHEMA_STORED_PROCEDURE *sp= (MYX_SCHEMA_STORED_PROCEDURE *)data;

    set_selection_info(NULL, sp->name, SP, true);
  }
}

/*
std::list<Gtk::TreeIter> MGTableBrowserList::get_selected_nodes()
{
  std::list<Gtk::TreeIter> sel;
  
  return sel;
}
 */


bool MGTableBrowserList::refresh_table_list(const Gtk::TreeIter &piter,
                                            MYX_SCHEMA_TABLES *tables,
                                            const Glib::ustring &filter)
{
  Gtk::TreeRow prow= *piter;
  bool showed_something= false;

  for (unsigned int i= 0; i < tables->schema_tables_num; i++)
  {
    MYX_SCHEMA_TABLE *table= tables->schema_tables+i;
    Gtk::TreeIter niter= _store->append(prow.children());
    Gtk::TreeRow nrow= *niter;
    bool show_it= false;
    bool show_parent= false;

    if ((get_search_type() == 'A' || get_search_type() == 'T') &&
        myx_match_pattern(table->table_name, filter.c_str(), 0, 0)!=0)
    {
      show_it= true;
      show_parent= true;
    }

    nrow[_columns.icon]= table->table_type == MSTT_VIEW ? _view_icon : _table_icon;
    nrow[_columns.text]= table->table_name;
    nrow[_columns.type]= Table;
    nrow[_columns.data]= table;
    nrow[_columns.weight]= Pango::WEIGHT_NORMAL;

    if (_leaf_type > Table)
    {
      for (unsigned int c= 0; c < table->columns_num; c++)
      {
        MYX_SCHEMA_TABLE_COLUMN *column= table->columns+c;
        Gtk::TreeIter column_iter;

        if (show_parent || (get_search_type() == 'A' && myx_match_pattern(column->column_name, filter.c_str(), 0, 0)!=0))
        {
          show_it= true;

          column_iter= _store->append(nrow.children());
          Gtk::TreeModel::Row column_row= *column_iter;

          column_row[_columns.icon]= column->primary_key ? _key_icon : _column_icon;
          column_row[_columns.text]= column->column_name;
          column_row[_columns.type]= Column;
          column_row[_columns.data]= column;
          column_row[_columns.weight]= Pango::WEIGHT_NORMAL;
        }
      }
    }
    if (!show_it)
      _store->erase(niter);
    else
      showed_something= true;
  }

  //
  //void *data= prow[_columns.data];
  //MYX_SCHEMA *schema= (MYX_SCHEMA*)data;
  //schema->schema_tables= tables;

  return showed_something;
}


bool MGTableBrowserList::refresh_sp_list(const Gtk::TreeIter &piter,
                                         MYX_SCHEMA_STORED_PROCEDURES *sps,
                                         const Glib::ustring &filter)
{
  Gtk::TreeRow prow= *piter;
  bool showed_something= false;

  for (unsigned int i= 0; i < sps->schema_sps_num; i++)
  {
    MYX_SCHEMA_STORED_PROCEDURE *sp= sps->schema_sps+i;
    Gtk::TreeIter niter= _store->append(prow.children());
    Gtk::TreeRow nrow= *niter;
    bool show_it= false;

    if ((get_search_type() == 'A' || get_search_type() == 'P') &&
        myx_match_pattern(sp->name, filter.c_str(), 0, 0)!=0)
      show_it= true;

    nrow[_columns.icon]= _sp_icon;
    if (sp->return_datatype)
      nrow[_columns.text]= ufmt("%s: %s", sp->name, sp->return_datatype);
    else
      nrow[_columns.text]= sp->name;
    nrow[_columns.type]= SP;
    nrow[_columns.data]= sp;
    nrow[_columns.weight]= Pango::WEIGHT_NORMAL;

    if (_leaf_type > Table && show_it)
    {
      for (unsigned int c= 0; c < sp->params_num; c++)
      {
        MYX_SCHEMA_STORED_PROCEDURE_PARAM *param= sp->params+c;
        Gtk::TreeIter column_iter;

        show_it= true;

        column_iter= _store->append(nrow.children());
        Gtk::TreeModel::Row column_row= *column_iter;
        
        column_row[_columns.icon]= _sp_icon;
        column_row[_columns.text]= ufmt("%s: %s", param->name, param->datatype);
        column_row[_columns.type]= Parameter;
        column_row[_columns.weight]= Pango::WEIGHT_NORMAL;
      }
    }
 
    if (!show_it)
      _store->erase(niter);
    else
      showed_something= true;
  }
  return showed_something;
}


void MGTableBrowserList::set_fetch_schema_tables_func(const MGTableBrowserList::FetchSchemaTablesSlot &slot)
{
  _fetch_schema_tables= slot;
}


void MGTableBrowserList::set_fetch_schema_sps_func(const MGTableBrowserList::FetchSchemaSPsSlot &slot)
{
  _fetch_schema_sps= slot;
}


void MGTableBrowserList::refresh_list(const Glib::ustring &filter)
{
  Gtk::TreeIter sch_iter;
  Gtk::TreeRow sch_row;
  MYX_CATALOGS *cats= _catalogs ? _catalogs->ptr() : NULL;

  _store->clear();
  if (!cats)
    return;

  for (unsigned int i= 0; i < cats->catalogs_num; i++)
  {
    MYX_CATALOG *cat= cats->catalogs+i;

    for (unsigned int s= 0; s < cat->schemata_num; s++)
    {
      bool flag= false;
      
      sch_iter= _store->append();
      sch_row= *sch_iter;

      sch_row[_columns.icon]= _schema_icon;
      sch_row[_columns.text]= cat->schemata[s].schema_name;
      sch_row[_columns.data]= cat->schemata+s;
      sch_row[_columns.type]= Schema;
      sch_row[_columns.weight]= Pango::WEIGHT_NORMAL;
      
      if ((get_search_type() == 'A' || get_search_type() == 'S') &&
          myx_match_pattern(cat->schemata[s].schema_name, filter.c_str(), 0, 0)!=0)
        flag= true;

      // not sure if this ever worked as this method is called
      // after reload which basically frees existing schemata and
      // refetches schemata info from server, so we never have
      // a chance to have a non-NULL
      // cat->schemata[s].schema_tables/cat->schemata[s].schema_sps here

      if (cat->schemata[s].schema_tables && _leaf_type > Schema)
      {
        if (refresh_table_list(sch_iter, cat->schemata[s].schema_tables, filter))
          flag= true;
      }
      if (cat->schemata[s].schema_sps && _leaf_type > Schema)
      {
        if (refresh_sp_list(sch_iter, cat->schemata[s].schema_sps, filter))
          flag= true;
      }
      if (!flag)
        _store->erase(sch_iter);
    }
  }

  if (!filter.empty())
    _tree->expand_all();

  if(!_selected_schema_name.empty())
  {
    bool found_schema= false;
    Gtk::TreeIter iter;

    for (iter= _store->children().begin(); 
        iter != _store->children().end();
        iter++)
    {
      if (get_type(iter) == Schema)
      {
        void *data= (*iter)[_columns.data];
        MYX_SCHEMA *schema= (MYX_SCHEMA *)data;
        if(_selected_schema_name == schema->schema_name)
        {
          // select node as otherwise subnodes (tables, etc)
          // will not be loaded
          Glib::ustring schema_name= _selected_schema_name;
          Glib::ustring entity_name= _selected_schema_entity_name;
          RowType entity_type= _selected_row_type;
          _tree->get_selection()->select(iter);
          if(_schema_is_expanded)
            _tree->expand_row(Gtk::TreeModel::Path(iter), false);
          _tree->scroll_to_row(Gtk::TreeModel::Path(_tree->get_selection()->get_selected()));

          // node selection and expansion cause data load from server
          // which could cause a delay during which user can continue
          // his work and select another node, so we restore
          // previous selection only in case it's unchanged
          if((_selected_schema_name == schema_name) && _selected_schema_entity_name.empty())
          {
            set_selection_info(NULL, entity_name.c_str(), entity_type, _schema_is_expanded);
            found_schema= true;
          }
          break;
        }
      }
    }

    if(found_schema && !_selected_schema_entity_name.empty())
    {
      for (Gtk::TreeIter subiter= iter->children().begin(); 
          subiter != iter->children().end();
          subiter++)
      {
        if (get_type(subiter) == _selected_row_type)
        {
          const char *entity_name= NULL;
          void *data= (*subiter)[_columns.data];
          if(_selected_row_type == Table)
            entity_name= ((MYX_SCHEMA_TABLE *)data)->table_name;
          else if(_selected_row_type == SP)
            entity_name= ((MYX_SCHEMA_STORED_PROCEDURE *)data)->name;
          else
            continue;

          if(_selected_schema_entity_name == entity_name)
          {
            _tree->get_selection()->select(subiter);
            _tree->scroll_to_row(Gtk::TreeModel::Path(subiter));
            break;
          }
        }
      } // for
    }
  }
}


void MGTableBrowserList::set_catalogs(const Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > &catalogs)
{
  _catalogs= catalogs;
  refresh();
}


MGTableBrowserList::RowType MGTableBrowserList::get_type(const Gtk::TreeIter &iter)
{
  g_return_val_if_fail(iter, Table);
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    return row[_columns.type];
  }
  return Table;
}


Glib::ustring MGTableBrowserList::get_catalog(const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;
  
  if (row[_columns.type] == Column || row[_columns.type] == Parameter)
      row= *row.parent();
  if (row[_columns.type] == Table || row[_columns.type] == SP)
      row= *row.parent();

  void *data= row[_columns.data];

  return ((MYX_SCHEMA*)data)->catalog_name?:"def";
}


Glib::ustring MGTableBrowserList::get_schema(const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;
  
  if (row[_columns.type] == Column || row[_columns.type] == Parameter)
      row= *row.parent();
  if (row[_columns.type] == Table || row[_columns.type] == SP)
      row= *row.parent();

  return row[_columns.text];
}

MYX_SCHEMA *MGTableBrowserList::get_schema_object(const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;
  
  if (row[_columns.type] == Column || row[_columns.type] == Parameter)
      row= *row.parent();
  if (row[_columns.type] == Table || row[_columns.type] == SP)
      row= *row.parent();

  return (MYX_SCHEMA*)(void*)row[_columns.data];
}

Glib::ustring MGTableBrowserList::get_table(const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;

  if (row[_columns.type] != Column && row[_columns.type] != Table)
    return "";

  if (row[_columns.type] == Column)
      row= *row.parent();

  return row[_columns.text];
}

Glib::ustring MGTableBrowserList::get_table_column(const Gtk::TreeIter &iter)
{
  Gtk::TreeRow row= *iter;
  if (row[_columns.type] != Column)
    return "";

  Gtk::TreeRow parent= *row.parent();

  if(parent[_columns.type] != Table)
    return "";

  return row[_columns.text];
}

MYX_SCHEMA_TABLE *MGTableBrowserList::get_table(const Gtk::TreeIter &iter,
                                                Glib::ustring &name)
{
  Gtk::TreeRow row= *iter;

  if (row[_columns.type] != Column && row[_columns.type] != Table)
  {
    name.clear();
    return 0;
  }

  if (row[_columns.type] == Column)
      row= *row.parent();

  name= row[_columns.text];

  void *data= row[_columns.data];
  return (MYX_SCHEMA_TABLE*)data;
}


bool MGTableBrowserList::is_view(const Gtk::TreeIter &iter)
{
  Glib::ustring dummy;
  MYX_SCHEMA_TABLE *table= get_table(iter, dummy);
  if (table && table->table_type == MSTT_VIEW)
    return true;
  else
    return false;
}


Glib::ustring MGTableBrowserList::get_column(const Gtk::TreeIter &iter)
{  
  Gtk::TreeRow row= *iter;
  
  if (row[_columns.type] != Column)
  {
    return "";
  }

  return row[_columns.text];
}


Glib::ustring MGTableBrowserList::get_procedure(const Gtk::TreeIter &iter, bool &is_function)
{
  Gtk::TreeRow row= *iter;
  
  if (row[_columns.type] != SP)
  {
    return "";
  }

  MYX_SCHEMA_STORED_PROCEDURE *sp= (MYX_SCHEMA_STORED_PROCEDURE*)(void*)row[_columns.data];
  is_function= sp->sp_type == MSPT_FUNCTION;
  
  return sp->name;
}


void MGTableBrowserList::set_node_bold(const Gtk::TreeIter &iter, bool flag)
{
  Gtk::TreeRow row= *iter;
  row[_columns.weight]= flag ? Pango::WEIGHT_BOLD : Pango::WEIGHT_NORMAL;
}


void MGTableBrowserList::drag_data_get(const Glib::RefPtr<Gdk::DragContext>& context, Gtk::SelectionData &selection_data, guint info, guint time)
{
  Gtk::TreeIter iter= _tree->get_selection()->get_selected();

  if (iter)
  {
    Gtk::TreeRow row= *iter;
    
    if (row[_columns.type]== Table)
    {
      MYX_SCHEMA_TABLE *table= (MYX_SCHEMA_TABLE*)(void*)row[_columns.data];

      row= *row.parent();

      MYX_SCHEMA *schema= (MYX_SCHEMA*)(void*)row[_columns.data];

      Glib::ustring object= MGSchemaObjectNameFromTable(schema->catalog_name,
                                                        schema->schema_name,
                                                        table->table_name);

      selection_data.set("x-mysqlgui-schema-object", object);
    }
    else if (row[_columns.type] == Column)
    {
      MYX_SCHEMA_TABLE_COLUMN *column= (MYX_SCHEMA_TABLE_COLUMN*)(void*)row[_columns.data];

      row= *row.parent();

      MYX_SCHEMA_TABLE *table= (MYX_SCHEMA_TABLE*)(void*)row[_columns.data];
      
      row= *row.parent();
      
      MYX_SCHEMA *schema= (MYX_SCHEMA*)(void*)row[_columns.data];

      Glib::ustring object= MGSchemaObjectNameFromTableColumn(schema->catalog_name,
                                                              schema->schema_name,
                                                              table->table_name,
                                                              column->column_name);

      selection_data.set("x-mysqlgui-schema-object", object);
    }
  }
}

Gtk::TreeIter MGTableBrowserList::find_table(const Glib::ustring &catalog,
                                             const Glib::ustring &schema,
                                             const Glib::ustring &table)
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  iter= _store->children().begin();
  while (iter != _store->children().end())
  {
    row= *iter;
    Glib::ustring str= row[_columns.text];
    
    if (str == schema)
    {
      if (table.empty())
        return iter;

      iter= row.children().begin();

      while (iter != row.children().end())
      {
        Gtk::TreeRow row2= *iter;
        str= row2[_columns.text];

        if (table.empty() || str == table)
          return iter;
        ++iter;
      }
      break;
    }
    ++iter;
  }

  return Gtk::TreeIter();
}
