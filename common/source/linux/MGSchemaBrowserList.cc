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


#include "MGSchemaBrowserList.h"
#include "mygpriv.h"
#include "myg_utils.h"

MGSchemaBrowserList::MGSchemaBrowserList(const Glib::ustring &caption,
          MGSchemaBrowserList::ViewType type)
    : MGBrowserList(false, caption), _view_type(type), _catalogs(0),
      _schema_icon(0), _sys_schema_icon(0), _table_icon(0), _column_icon(0),
      _mark_delegate(0)
{
  Gtk::TreeViewColumn *column= new Gtk::TreeView::Column("");
  column->pack_start(_columns._icon, false);
  column->pack_start(_columns._text);
  _tree->append_column(*Gtk::manage(column));
  _tree->signal_cursor_changed().connect(sigc::mem_fun(*this, &MGSchemaBrowserList::cursor_changed));

  std::vector<Gtk::CellRenderer*> rends= column->get_cell_renderers();
  column->add_attribute(static_cast<Gtk::CellRendererText*>(rends[1])->property_foreground_gdk(),
                       _columns._color);
  
  _show_columns= true;
  
  // catalog column
  _tree->append_column("", _columns._catalog);

  set_store(Gtk::TreeStore::create(_columns));
}

void MGSchemaBrowserList::cursor_changed()
{
  if (_view_type == SchemaOnly)
    return;

  Gtk::TreeIter iter= _tree->get_selection()->get_selected();
  if(iter->children().empty())
  {
    populate_node(iter);
  }
}


// merge this into set_icons
void MGSchemaBrowserList::set_index_icon(const Glib::RefPtr<Gdk::Pixbuf> &index_icon)
{
  _index_icon= index_icon;
}


void MGSchemaBrowserList::set_icons(const Glib::RefPtr<Gdk::Pixbuf> &schema_icon,
                                    const Glib::RefPtr<Gdk::Pixbuf> &sys_schema_icon,
                                    const Glib::RefPtr<Gdk::Pixbuf> &table_icon,
                                    const Glib::RefPtr<Gdk::Pixbuf> &column_icon,
                                    const Glib::RefPtr<Gdk::Pixbuf> &sp_icon)
{
  _schema_icon= schema_icon;
  
  _sys_schema_icon= sys_schema_icon;
  
  _table_icon= table_icon;
  
  _column_icon= column_icon;
  
  _sp_icon= sp_icon;
}

void MGSchemaBrowserList::append_schema(MYX_SCHEMA *schema)
{
  Gtk::TreeIter sche_iter= _store->append();
  Gtk::TreeModel::Row sche_row= *sche_iter;
  char *cat= (char*)schema->catalog_name;
  bool sys= false;
  
  if(cat)
  {
    if (strcmp(cat, "def")==0)
    {
      cat= NULL;
    }
    else if (strcmp(cat, "sys")==0)
    {
      sys= true;
    }
  }  
  
  // XXX put if schema is system or not in icon
  sche_row[_columns._icon]= sys ? _sys_schema_icon : _schema_icon;
  sche_row[_columns._text]= schema->schema_name;
  sche_row[_columns._catalog]= cat?"("+Glib::ustring(cat)+")":"";
  sche_row[_columns._type]= 'S';
  sche_row[_columns._data]= schema;
  sche_row[_columns._populated]= false;
  if (_mark_delegate)
  {
    if ((*_mark_delegate)(_mark_delegate_data,
                          Glib::ustring((char*)schema->schema_name),
                          Glib::ustring(), Glib::ustring()))
      mark_node(sche_iter, _columns._color, true, true);
  }


  schema->schema_tables= NULL;
  schema->schema_indices= NULL;
  schema->schema_sps= NULL;

  if (_view_type != SchemaOnly)
  {
    //populate_node(sche_iter);
  }
}

bool MGSchemaBrowserList::contains_schema(MYX_SCHEMA *schema)
{
  Gtk::TreeIter sche_iter= _store->get_iter(Glib::ustring("0"));
  Glib::ustring n, c;
  
  while(sche_iter)
  {
    Gtk::TreeModel::Row sche_row= *sche_iter;
    n= sche_row[_columns._text];
    c= sche_row[_columns._catalog];
    //if((strcmp(n.c_str(), schema->schema_name) == 0)
    //  && (strcmp(c.c_str(), schema->catalog_name) == 0))
    if(strcmp(n.c_str(), schema->schema_name) == 0)
    {
      return true;
    }
    sche_iter++;
  }
  return false;
}

void MGSchemaBrowserList::refresh_list(const Glib::ustring &filter)
{
  _store->clear();

  /*
   * catalog
   *    schemata (database)
   *      [table]
   *       table
   *          column
   */
  
  if (_catalogs)
  {
    for (unsigned int c= 0; c < _catalogs->catalogs_num; c++)
    {
      MYX_CATALOG *catalog= _catalogs->catalogs+c;

      for (unsigned int s= 0; s < catalog->schemata_num; s++)
        append_schema(catalog->schemata+s);
    }
  }
}


void MGSchemaBrowserList::refresh_schema_list(const Gtk::TreeIter &schema_node,
                                              MYX_SCHEMA_TABLES *tables,
                                              MYX_SCHEMA_STORED_PROCEDURES *sps)
{
  Gtk::TreeIter type_iter;
  Gtk::TreeRow type_row;
  Gtk::TreeIter table_iter;
  Gtk::TreeIter sp_iter;
  Glib::ustring schema_name;
  Gtk::TreeModel::Row row= *schema_node;
  Gtk::TreeRow parent_row;

  row[_columns._populated]= true;
  schema_name= row[_columns._text];

  // tables
  if (_view_type == FullSchemata)
  {
    type_iter= _store->append(row.children());
    type_row= *type_iter;

    type_row[_columns._icon]= _table_icon;
    type_row[_columns._text]= _("Tables");
    type_row[_columns._catalog]= "";
    type_row[_columns._type]= 'l';

    parent_row= type_row;
  }
  else
    parent_row= row;
  
  while(!parent_row.children().empty())
  {
    _store->erase(parent_row.children().begin());
  }
  
  for (unsigned int t= 0; t < tables->schema_tables_num; t++)
  {
    MYX_SCHEMA_TABLE *table= tables->schema_tables+t;
    Gtk::TreeIter column_iter;
    Gtk::TreeModel::Row table_row;
    
    table_iter= _store->append(parent_row.children());
    table_row= *table_iter;

    table_row[_columns._icon]= _table_icon;
    table_row[_columns._text]= table->table_name;
    table_row[_columns._catalog]= "";
    table_row[_columns._type]= 'T';

    if (_mark_delegate)
    {
      if ((*_mark_delegate)(_mark_delegate_data,
                            schema_name,
                            Glib::ustring((char*)table->table_name),
                            Glib::ustring()))
        mark_node(table_iter, _columns._color, true, true);
    }

    if (_show_columns)
    {
      for (unsigned int c= 0; c < table->columns_num; c++)
      {
        MYX_SCHEMA_TABLE_COLUMN *column= table->columns+c;
        
        column_iter= _store->append(table_row.children());
        Gtk::TreeModel::Row column_row= *column_iter;
        
        column_row[_columns._icon]= _column_icon;
        column_row[_columns._text]= column->column_name;
        column_row[_columns._catalog]= "";
        column_row[_columns._type]= 'C';
        if (_mark_delegate)
        {
          if ((*_mark_delegate)(_mark_delegate_data,
                                schema_name,
                                Glib::ustring((char*)table->table_name),
                                Glib::ustring((char*)column->column_name)))
            mark_node(column_iter, _columns._color, true, true);
        }
      }
    }
  }

  if (sps)
  {
    for (unsigned int t= 0; t < sps->schema_sps_num; t++)
    {
      MYX_SCHEMA_STORED_PROCEDURE *sp= sps->schema_sps+t;
      Gtk::TreeIter column_iter;
      Gtk::TreeModel::Row sp_row;
      
      sp_iter= _store->append(parent_row.children());
      sp_row= *sp_iter;
      
      sp_row[_columns._icon]= _sp_icon;
      sp_row[_columns._text]= sp->name;
      sp_row[_columns._catalog]= "";
      sp_row[_columns._type]= 'P';
      
      if (_mark_delegate)
      {
        if ((*_mark_delegate)(_mark_delegate_data,
                              schema_name,
                              Glib::ustring((char*)sp->name),
                              Glib::ustring()))
          mark_node(table_iter, _columns._color, true, true);
      }
    }
  }

#if 0
  // index list
  if (_view_type == FullSchemata)
  {
    type_iter= _store->append(row.children());
    type_row= *type_iter;

    type_row[_columns._icon]= _table_icon;
    type_row[_columns._text]= _("Indices");
    type_row[_columns._catalog]= "";
    type_row[_columns._type]= 'l';

    parent_row= type_row;

    for (unsigned int t= 0; t < tables->indices_num; t++)
    {
      MYX_SCHEMA_INDEX *table= tables->schema_indices+t;
      Gtk::TreeIter index_iter;
      Gtk::TreeModel::Row index_row;
      
      index_iter= _store->append(parent_row.children());
      index_row= *index_iter;
      
      index_row[_columns._icon]= _index_icon;
      index_row[_columns._text]= ufmt("%s (%s)", index->key_name, index->table_name);
      index_row[_columns._catalog]= "";
      index_row[_columns._type]= 'I';
    }
  }
#endif
}


char MGSchemaBrowserList::get_row_object(const Gtk::TreeIter &iter,
                                         Glib::ustring &catalog, Glib::ustring &schema,
                                         Glib::ustring &table, Glib::ustring &column,
                                         Glib::ustring &child)
{
  char o_type= 0;
  bool o_type_set= false;
  Gtk::TreeIter parent, node;
  Glib::ustring text, cat;
  gint type= 0;
  
  g_return_val_if_fail(iter, o_type);

  catalog.clear();
  schema.clear();
  table.clear();
  column.clear();
  child.clear();
  
  node= iter;

  do
  {
    Gtk::TreeModel::Row row= *node;
    text= row[_columns._text];
    cat= row[_columns._catalog];
    type= row[_columns._type];
    if(!o_type_set)
    {
      o_type= type;
      o_type_set= true;
    }
    switch (type)
    {/*
      case 'c':
      catalog= text;
      break;*/
    case 'S':
      schema= text;
      catalog= cat;
      type= 'c'; //break loop
      break;
    case 'l': // type label
      parent= row.parent();
      break;
    case 'T':
      table= text;
      parent= row.parent();
      break;
    case 'P': // Stored Routine
      table= text;
      parent= row.parent();
      break;
    case 'C':
      column= text;
      parent= row.parent();
      break;
    case 'H':
      child= text;
      parent= row.parent();
      break;
    }
    node= parent;
  } while (type != 'c' && node);
  return o_type;
}


bool MGSchemaBrowserList::get_selected_schema(Gtk::TreeIter &node,
                                              Glib::ustring &catalog,
                                              Glib::ustring &schema,
                                              Glib::ustring &child)
{
  Glib::ustring table, column;
  
  if (!(node= get_selected()))
    return false;

  get_row_object(node, catalog, schema, table, column, child);
  
  return true;
}


void MGSchemaBrowserList::append_child(const Gtk::TreeIter &node,
                                       const Glib::RefPtr<Gdk::Pixbuf> &icon, 
                                       const Glib::ustring &text)
{
  Gtk::TreeIter iter;

  // append child after last child node (of type H), or as first if there
  // are none yet
  if (!get_last_child_of_node(node, iter))
  {
    iter= _store->append(Gtk::TreeModel::Row(*node).children());
  }
  else
  {
    iter= _store->insert(iter);
  }

  Gtk::TreeModel::Row row= *iter;
  
  row[_columns._icon]= icon;
  row[_columns._text]= text;
  row[_columns._catalog]= "";
  row[_columns._type]= 'H';

  _tree->expand_row(Gtk::TreePath(node), true);
}


bool MGSchemaBrowserList::get_last_child_of_node(const Gtk::TreeIter &node,
                                                 Gtk::TreeIter &child_ret)
{
  Gtk::TreeIter iter;

  iter= Gtk::TreeModel::Row(*node).children().begin();
  if (iter)
  {
    int prev_type= 0, type= 0;

    do {
      Gtk::TreeModel::Row row= *iter;
      
      type= row[_columns._type];

      child_ret= iter;
      prev_type= type;
    } while (type == 'H' && ++iter);
    
    if (prev_type== 'H')
      return true;
    else
      return false;
  }

  return false;
}


void MGSchemaBrowserList::set_catalogs(MYX_CATALOGS *catalogs)
{
  _catalogs= catalogs;
  refresh_list(_entry->get_text());
}


void MGSchemaBrowserList::set_populate_func(const PopulateSlot &slot)
{
  _populate_slot= slot;
}


void MGSchemaBrowserList::set_mark_delegate(MarkRowDelegate deleg, 
                                            gpointer data)
{
  _mark_delegate_data= data;
  _mark_delegate= deleg;
}


void MGSchemaBrowserList::refresh_column_markings(const Gtk::TreeIter &node,
                                                  const Glib::ustring &schema,
                                                  const Glib::ustring &table)
{
  Gtk::TreeIter iter;
  Gtk::TreeModel::Children children= Gtk::TreeModel::Row(*node).children();

  iter= children.begin();
  while (iter != children.end())
  {
    Glib::ustring column;

    column= Gtk::TreeModel::Row(*iter)[_columns._text];
    if (!column.empty())
    {
      if ((*_mark_delegate)(_mark_delegate_data, schema, table, column))
        mark_node(iter, _columns._color, true, true);
      else
        mark_node(iter, _columns._color, false, false);
    }
    ++iter;
  }
}


void MGSchemaBrowserList::refresh_table_markings(const Gtk::TreeIter &node,
                                                 const Glib::ustring &schema)
{
  Gtk::TreeIter iter;
  Glib::ustring blank;
  Gtk::TreeModel::Children children= Gtk::TreeModel::Row(*node).children();

  iter= children.begin();
  while (iter != children.end())
  {
    Glib::ustring table;

    table= Gtk::TreeModel::Row(*iter)[_columns._text];
    if (!table.empty())
    {
      if ((*_mark_delegate)(_mark_delegate_data, schema, table, blank))
        mark_node(iter, _columns._color, true, true);
      else
        mark_node(iter, _columns._color, false, false);

      refresh_column_markings(iter, schema, table);
    }
    ++iter;
  }
}


void MGSchemaBrowserList::refresh_markings()
{
  Gtk::TreeIter iter;
  Glib::ustring blank;

  iter= _store->children().begin();
  while (iter != _store->children().end())
  {
    Glib::ustring schema;

    schema= Gtk::TreeModel::Row(*iter)[_columns._text];
    if (!schema.empty())
    {
      if ((*_mark_delegate)(_mark_delegate_data, schema, blank, blank))
        mark_node(iter, _columns._color, true, true);
      else
        mark_node(iter, _columns._color, false, false);
      
      refresh_table_markings(iter, schema);
    }
    ++iter;
  }
}


void MGSchemaBrowserList::mark(const Gtk::TreeIter &node, bool flag)
{
  mark_node(node, _columns._color, flag, true);
}


void MGSchemaBrowserList::populate_node(const Gtk::TreeIter &iter)
{
  Gtk::TreeModel::Row row= *iter;

  if (row[_columns._type] == 'S')
  {
    MYX_SCHEMA *schema= row[_columns._data];
  
    if (!schema->schema_tables)
    {
      _populate_slot(this, iter);
    }

    if (schema->schema_tables || schema->schema_sps)
    {
      refresh_schema_list(iter, schema->schema_tables, schema->schema_sps);
    }
  }
}
