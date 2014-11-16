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

#define RESULTSET_MODULE

#include <myx_grt_public_interface.h>
#include <myx_sql_resultset_public_interface.h>

#include <MYXMySQLResultSetSource.h>
#include <MYXMySQLResultSetStorage.h>

#include "myx_sql_resultset.h"



static MYXResultSetSource *create_mysql(MYX_GRT_VALUE *args)
{
  const char *hostname;
  const char *username;
  const char *password;
  int port;
  const char *socket_path;
  MYXMySQLResultSetSource *source;
  
  hostname= myx_grt_dict_item_get_as_string(args, "hostname");
  username= myx_grt_dict_item_get_as_string(args, "username");
  password= myx_grt_dict_item_get_as_string(args, "password");
  port= myx_grt_dict_item_get_as_int(args, "port");
  socket_path= myx_grt_dict_item_get_as_string(args, "socket_path");
  
  source= new MYXMySQLResultSetSource();
  if (!source->connect(hostname, port, username, password, socket_path))
  {
    delete source;
    return 0;
  }
  return source;
}


static MYXResultSetStorage *create_mysql_store(MYXResultSetSource *source)
{
  return MYXMySQLResultSetStorage::create((MYXMySQLResultSetSource*)source);
}



static struct {
  char *name;
  MYXResultSetSource* (*create)(MYX_GRT_VALUE*);
  MYXResultSetStorage* (*create_storage)(MYXResultSetSource*);
} db_types[]= {
  {"mysql", create_mysql, create_mysql_store},
  //{"maxdb", MYXMaxDBResultSetSource::create},
  //{"jdbc", MYXJDBCResultSetSource::create},
  {NULL}
};


// --------------------------------------------------------------------------
// module registration function

extern "C" {
MYX_GRT_MODULE* myx_register_builtin_grt_module_result_set(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_result_set, grt);
}


MYX_GRT_MODULE* myx_register_builtin_grt_module_result_set_source(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_result_set_source, grt);
}
};

// --------------------------------------------------------------------------
// data source object

MYX_GRT_VALUE * src_open(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *dbtype, *args, *value;
  int i;
  const char *type;

  if (myx_grt_list_item_count(param) == 2)
  {
    dbtype= myx_grt_list_item_get(param, 0);
    args= myx_grt_list_item_get(param, 1);
  }
  else
    return make_return_value_error("This function takes (dbtype, connect-args) as parameters.", "");

  type= myx_grt_value_as_string(dbtype);
  
  for (i= 0; db_types[i].name; i++)
  {
    if (strcasecmp(db_types[i].name, type)==0)
    {
      MYXResultSetSource *source;

      value= myx_grt_dict_new_obj(grt, "db.query.ResultsetSource", "ResultsetSource", "", "");
      
      source= (*db_types[i].create)(args);
      if (!source)
      {
        return make_return_value_error("Error opening connection to database", "");
      }

      myx_grt_dict_item_set_value_from_string(value, "dbType", db_types[i].name);
      
      myx_grt_value_bridge_data_object_set(value, source);

      return make_return_value(value);
    }
  }

  return make_return_value_error("Unsupported database type", type);
}


MYX_GRT_VALUE * src_close(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  
  if (myx_grt_list_item_count(param) == 1)
  {
    MYXResultSetSource *source;
    
    self= myx_grt_list_item_get(param, 0);
    
    source= (MYXResultSetSource*)myx_grt_value_bridge_data_object_get(self);
    delete source;

    myx_grt_value_bridge_data_object_set(self, NULL);

    return NULL;
  }
  else
  {
    return make_return_value_error("Invalid number of arguments", "");
  }
}


MYX_GRT_VALUE * src_status(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}


static void fill_column_data(MYX_GRT *grt, MYX_GRT_VALUE *self, MYXResultSetStorage *store)
{
  MYX_GRT_VALUE *list= myx_grt_list_new(MYX_DICT_VALUE, "db.query.ResultsetColumn");
  MYX_GRT_VALUE *column;
  std::vector<MYXResultSetSource::ColumnInfo*> columns= store->get_columns();
  
  for (unsigned int i= 0; i < columns.size(); i++)
  {
    column= myx_grt_dict_new_obj(grt, "db.query.ResultsetColumn", columns[i]->name, "", myx_grt_dict_id_item_as_string(self));
    
    myx_grt_dict_item_set_value_from_int(column, "length", columns[i]->length);
    myx_grt_dict_item_set_value_from_int(column, "displayLength", columns[i]->display_length);
    myx_grt_dict_item_set_value_from_int(column, "displayDecimals", columns[i]->display_decimals);
    myx_grt_dict_item_set_value_from_int(column, "isPrimaryKey", columns[i]->is_pk);
    myx_grt_dict_item_set_value_from_string(column, "columnType", columns[i]->type_name);
    
    myx_grt_list_item_add(list, column);
    myx_grt_value_release(column);
  }
  
  myx_grt_dict_item_set_value(self, "columns", list);
  myx_grt_value_release(list);
}



MYX_GRT_VALUE * src_query(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *self, *query, *value;
  int page_size;

  if (myx_grt_list_item_count(param) == 3 || myx_grt_list_item_count(param) == 2)
  {
    MYXResultSetSource *source;
    MYXResultSetStorage *storage;
    const char *dbtype;
    int i;
    
    self= myx_grt_list_item_get(param, 0);
    query= myx_grt_list_item_get(param, 1);
    if (myx_grt_list_item_count(param) == 3)
      page_size= myx_grt_value_as_int(myx_grt_list_item_get(param, 2));
    else
      page_size= -1;

    dbtype= myx_grt_dict_item_get_as_string(self, "dbType");
    for (i= 0; db_types[i].name; i++)
    {
      if (strcmp(db_types[i].name, dbtype)==0)
        break;
    }
    
    if (!db_types[i].name)
    {
      return make_return_value_error("Invalid module type in ResultsetSource object", "");
    }
    
    source= (MYXResultSetSource*)myx_grt_value_bridge_data_object_get(self);

    if (!source->execute(myx_grt_value_as_string(query)))
    {
      return make_return_value_error("Error executing query",
                                     source->get_error_message());
    }
    
    storage= (*db_types[i].create_storage)(source);
    if (!storage)
    {
      return make_return_value_error("Could not create ResultsetSource object for query", "");
    }
    
    if (page_size >= 0)
      storage->set_page_size(page_size);
    else
      page_size= storage->get_page_size();

    value= myx_grt_dict_new_obj(grt, "db.query.Resultset", "Resultset", "", "");

    myx_grt_dict_item_set_value_from_string(value, "connection", myx_grt_dict_item_get_as_string(self, "_id"));
    myx_grt_dict_item_set_value_from_string(value, "sql", myx_grt_value_as_string(query));
    myx_grt_dict_item_set_value_from_int(value, "rowBlockSize", page_size);
    
    fill_column_data(grt, value, storage);

    myx_grt_value_bridge_data_object_set(value, storage);
    
    return make_return_value(value);
  }
  else
    return make_return_value_error("Invalid number of arguments", "");
}



// --------------------------------------------------------------------------
// resultset object


MYX_GRT_VALUE * res_close(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  
  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  delete store;
  myx_grt_value_bridge_data_object_set(self, NULL);

  return NULL;
}


MYX_GRT_VALUE * res_move_next(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (!store->next_row())
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * res_move_prior(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (!store->prior_row())
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}



MYX_GRT_VALUE * res_move_first(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (!store->move_first())
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * res_move_last(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (!store->move_last())
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}



MYX_GRT_VALUE * res_field_get(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;
  int index;
  const char *value;
  size_t length;

  if (myx_grt_list_item_count(param) != 2)
    return make_return_value_error("This function takes (db.query.Resultset, columnIndex) as parameters.", "");

  self= myx_grt_list_item_get(param, 0);
  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);
  index= myx_grt_value_as_int(myx_grt_list_item_get(param, 1));

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if ((index < 0) || (index > store->get_column_count()))
    return make_return_value_error("Index out of range.", "");

  if (store->get(index, value, length))
    return make_return_value(myx_grt_value_from_string(value));

  return make_return_value_error("Could not get field value (invalid row)", "");
}


MYX_GRT_VALUE * res_field_set(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;
  bigint row;
  int index;
  const char *value;

  if (myx_grt_list_item_count(param) != 4)
    return make_return_value_error("This function takes (db.query.Resultset, row, columnIndex, value) as parameters.", "");

  self= myx_grt_list_item_get(param, 0);
  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);
  row= myx_grt_value_as_int(myx_grt_list_item_get(param, 1));
  index= myx_grt_value_as_int(myx_grt_list_item_get(param, 2));
  value= myx_grt_value_as_string(myx_grt_list_item_get(param, 3));

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (row < 0 || row >= store->get_row_count())
    return make_return_value_error("Row index out of range.", "");
  
  if ((index < 0) || (index >= store->get_column_count()))
    return make_return_value_error("Column index out of range.", "");

  if (store->set(row, index, value))
    return make_return_value(myx_grt_value_from_int(1));

  return make_return_value_error("Could not set field value", "");
}


MYX_GRT_VALUE * res_commit(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;
  MYXResultSetCommitResult *result;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  result= store->commit_changes();
  if (!result)
    return make_return_value_error("Error commiting changes. Out of memory?", "");

  if (result->has_errors())
  {
    delete result;
    return make_return_value_error("There were errors commiting changes", "");
  }

  delete result;

  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * res_revert(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  store->discard_changes();
  
  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * res_current_row_count(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  return make_return_value(myx_grt_value_from_int(store->get_row_count()));
}


MYX_GRT_VALUE *res_status(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *self;
  MYXResultSetStorage *store;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  self= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  store= (MYXResultSetStorage*)myx_grt_value_bridge_data_object_get(self);

  if (store)
    return make_return_value(myx_grt_value_from_int(1));
  else
    return make_return_value_error("The query has not been executed.", "");
}
