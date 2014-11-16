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


#include <myx_grt_public_interface.h>
#include <myx_sql_resultset_public_interface.h>

#include "myx_sql_resultset.h"

// --------------------------------------------------------------------------
// module registration function

MYX_GRT_MODULE* myx_register_builtin_grt_module_result_set(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_result_set, grt);
}

// --------------------------------------------------------------------------

static void free_result_set(MYX_GRT_RESULT_SET *result_set);

// --------------------------------------------------------------------------

MYX_GRT_VALUE * res_open(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *query, *value, *error= NULL;
  MYX_GRT_RESULT_SET *result_set;
  int row_block_size= 100;

  if (myx_grt_list_item_count(param) == 1) 
  {
    query= myx_grt_list_item_get(param, 0);
  }
  else if (myx_grt_list_item_count(param) == 2)
  {
    query= myx_grt_list_item_get(param, 0);
    row_block_size= myx_grt_value_as_int(myx_grt_list_item_get(param, 1));
  }
  else
    return make_return_value_error("This function takes (db.query.Query) or (db.query.Query, rowBlockSize) as parameters.", "");

  

  value= myx_grt_dict_new_obj(grt, "db.query.Resultset", "Resultset", "", "");

  // Set connection reference
  myx_grt_dict_item_set_value_from_string(value, "connection", myx_grt_dict_item_get_as_string(query, "connection"));

  // Set query reference
  myx_grt_reference_cache_add(grt, query);
  myx_grt_dict_item_set_value_from_string(value, "query", myx_grt_dict_item_get_as_string(query, "_id"));

  // Set row block size
  myx_grt_dict_item_set_value_from_int(value, "rowBlockSize", row_block_size);

  // Set sql
  myx_grt_dict_item_set_value_from_string(value, "sql", myx_grt_dict_item_get_as_string(query, "_id"));

  // generate result_set structure
  result_set= g_new0(MYX_GRT_RESULT_SET, 1);

  // store MYX_GRT_RESULT_SET * in the bridge_data_object
  myx_grt_value_bridge_data_object_set(value, result_set);

  return make_return_value(value);
}

MYX_GRT_VALUE * res_close(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;
  unsigned int col_count;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);
  col_count= myx_grt_list_item_count(myx_grt_dict_item_get_value(result_set_value, "columns"));

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  myx_grt_value_bridge_data_object_set(result_set_value, NULL);

  if (result_set)
  {
    if (result_set->row_blocks)
    {
      unsigned int i;

      MYX_GRT_RESULT_SET_ROW *row= result_set->row_blocks->first;

      // loop over all rows and free the field value and the fields list
      while (row)
      {
        for (i= 0; i < col_count; i++)
          g_free(row->fields[i].value);

        g_free(row->fields);
        
        row= row->next;
      }

      // loop over all row blocks and free the list of rows
      for (i= 0; i < result_set->row_block_count; i++)
      {
        MYX_GRT_RESULT_SET_BLOCK *row_block= result_set->row_blocks + i;

        g_free(row_block->first);
      }

      // free the blocks
      g_free(result_set->row_blocks);
    }

    // free the result set
    g_free(result_set);
  }

  return NULL;
}

MYX_GRT_VALUE * res_rows_add(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *result_set_value, *rows;
  MYX_GRT_RESULT_SET *result_set;
  MYX_GRT_RESULT_SET_BLOCK *row_block, *prior_row_block;
  MYX_GRT_RESULT_SET_ROW *row, *row_prior;
  unsigned int i, row_count, col_count;
  int row_block_size;

  result_set_value= myx_grt_list_item_get(param, 0);
  rows= myx_grt_list_item_get(param, 1);
  row_block_size= myx_grt_dict_item_get_as_int(result_set_value, "rowBlockSize");

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  if (!result_set->row_blocks)
  {
    result_set->row_block_count= 1;
    result_set->row_blocks= g_new0(MYX_GRT_RESULT_SET_BLOCK, 1);
    row_block= result_set->row_blocks;
    prior_row_block= NULL;
  }
  else
  {
    // realloc block
    result_set->row_block_count++;
    result_set->row_blocks= g_realloc(result_set->row_blocks, sizeof(MYX_GRT_RESULT_SET_BLOCK) * result_set->row_block_count);
    row_block= result_set->row_blocks + result_set->row_block_count - 1;

    row_block->first= NULL;
    row_block->last= NULL;

    prior_row_block= result_set->row_blocks + result_set->row_block_count - 2;
  }

  row_count= myx_grt_list_item_count(rows);
  result_set->current_row_count+= row_count;

  // check if there are more rows to fetch
  if ((row_count < (unsigned int) row_block_size) || (row_count == 0))
  {
    result_set->status= MYX_GRT_RSS_FETCHED;

    myx_grt_dict_item_set_value_from_int(result_set_value, "nextBlockStartRow", -1);
  }
  else
  {
    int next_block_start_row= myx_grt_dict_item_get_as_int(result_set_value, "nextBlockStartRow");

    result_set->status= MYX_GRT_RSS_MORE_ROWS;

    myx_grt_dict_item_set_value_from_int(result_set_value, "nextBlockStartRow", 
       next_block_start_row + row_block_size);
  }

  col_count= myx_grt_list_item_count(myx_grt_dict_item_get_value(result_set_value, "columns"));

  row_block->first= g_new0(MYX_GRT_RESULT_SET_ROW, row_count);

  row_prior= NULL;
  row= row_block->first;

  for (i= 0; i < row_count; i++)
  {
    unsigned int j;
    MYX_GRT_VALUE *row_value= myx_grt_list_item_get(rows, i);

    row->prior= row_prior;
    if (row_prior)
      row_prior->next= row;

    row->fields= g_new0(MYX_GRT_RESULT_SET_FIELD, col_count);
    for (j= 0; j < col_count; j++)
    {
      MYX_GRT_RESULT_SET_FIELD *field= row->fields + j;
      MYX_GRT_VALUE *field_value= myx_grt_list_item_get(row_value, j);

      field->value= g_strdup(myx_grt_list_item_get_as_string(field_value, 0));
      field->value_length= atol(myx_grt_list_item_get_as_string(field_value, 1));
    }

    row_prior= row;
    row++;
  }

  row_block->last= row_prior;

  // connect last row from previous blocks with new row
  if (prior_row_block)
  {
    prior_row_block->last->next= row_block->first;
    row_block->first->prior= prior_row_block->last;
  }

  return NULL;
}

static const char *get_query_module_name(MYX_GRT *grt, MYX_GRT_VALUE *result_set_value)
{
  MYX_GRT_VALUE *query= myx_grt_dict_item_get_reference_value(grt, result_set_value, "query");

  return myx_grt_dict_item_get_as_string(query, "moduleName");
}

MYX_GRT_VALUE * res_move_next(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  // if the result set has just been opened and the blocks have been fetched already
  if (!result_set->current && result_set->row_blocks)
    result_set->current= result_set->row_blocks->first;
  // if the current row is available and there is a next row
  else if (result_set->current && result_set->current->next)
    result_set->current= result_set->current->next;
  // if the current row is available but there is now next row in the buffers but in the result set
  else if (result_set->current && !result_set->current->next && (result_set->status == MYX_GRT_RSS_MORE_ROWS))
  {    
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE *args= myx_grt_list_new(MYX_ANY_VALUE, "");

    myx_grt_list_item_add(args, result_set_value);

    myx_grt_function_get_and_call(grt, get_query_module_name(grt, result_set_value), "queryFetchResultSet", 0,
      args, &error);

    myx_grt_value_release(args);

    if (error)
      return make_return_value_error("This function queryFetchResultSet cannot be called.", "");

    if (result_set->current && result_set->current->next)
      result_set->current= result_set->current->next;
  }
  else
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}

MYX_GRT_VALUE * res_move_prior(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  // if the result set has just been opened and the blocks have been fetched already
  if (!result_set->current && result_set->row_blocks)
  {
    MYX_GRT_RESULT_SET_BLOCK *last_block= result_set->row_blocks + result_set->row_block_count - 1;
    result_set->current= last_block->last;
  }
  // if the current row is available and there is a next row
  else if (result_set->current && result_set->current->prior)
    result_set->current= result_set->current->prior;
  else
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}

MYX_GRT_VALUE * res_move_first(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  // if the result set has just been opened and the blocks have been fetched already
  if (result_set->row_blocks)
    result_set->current= result_set->row_blocks->first;
  else
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}

MYX_GRT_VALUE * res_move_last(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= (MYX_GRT *)data;
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) < 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  // if the result set has just been opened and the blocks have been fetched already
  if (result_set->row_blocks)
  {
    MYX_GRT_RESULT_SET_BLOCK *last_block= result_set->row_blocks + result_set->row_block_count - 1;
    result_set->current= last_block->last;
  }
  else
    return make_return_value(myx_grt_value_from_int(0));

  return make_return_value(myx_grt_value_from_int(1));
}


MYX_GRT_VALUE * res_field_as_string(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;
  int index;

  if (myx_grt_list_item_count(param) != 2)
    return make_return_value_error("This function takes (db.query.Resultset, columnIndex) as parameters.", "");

  result_set_value= myx_grt_list_item_get(param, 0);
  index= myx_grt_value_as_int(myx_grt_list_item_get(param, 1));

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  if ((index < 0) || ((unsigned int) index > myx_grt_list_item_count(myx_grt_dict_item_get_value(result_set_value, "columns")) - 1))
    return make_return_value_error("Index out of range.", "");

  if (!result_set->current)
    if (!res_move_next(param, data))
      return make_return_value_error("Cannot get first row.", "");

  return make_return_value(myx_grt_value_from_string(result_set->current->fields[index].value));
}

MYX_GRT_VALUE * res_current_row_count(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  if (result_set)
    return make_return_value(myx_grt_value_from_int(result_set->current_row_count));
  else
    return make_return_value_error("The query has not been executed.", "");
}

MYX_GRT_VALUE *res_status(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT_VALUE *result_set_value;
  MYX_GRT_RESULT_SET *result_set;

  if (myx_grt_list_item_count(param) != 1)
    return make_return_value_error("This function takes a db.query.Resultset as parameter.", "");

  result_set_value= myx_grt_list_item_get(param, 0);

  // retrieve MYX_GRT_RESULT_SET * from the bridge_data_object
  result_set= myx_grt_value_bridge_data_object_get(result_set_value);

  if (result_set)
    return make_return_value(myx_grt_value_from_int((int) result_set->status));
  else
    return make_return_value_error("The query has not been executed.", "");
}