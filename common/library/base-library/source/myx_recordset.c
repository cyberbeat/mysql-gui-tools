/* Copyright (C) 2003,2004 MyQL AB

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

#include <myx_public_interface.h>
#include <myx_library.h>
#include <myx_util_functions.h>
#include <myx_recordset.h>
#include <myx_query.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#define ROWS_ALLOCATE_BLOCK_COUNT 5000
#define CALLBACK_AFTER_BYTES 256 * 1024

/* compatibility with <= 4.0.x */
#if MYSQL_VERSION_ID < 40100
#define MYSQL_TYPE_DECIMAL     FIELD_TYPE_DECIMAL
#define MYSQL_TYPE_TINY        FIELD_TYPE_TINY
#define MYSQL_TYPE_SHORT       FIELD_TYPE_SHORT
#define MYSQL_TYPE_LONG        FIELD_TYPE_LONG
#define MYSQL_TYPE_FLOAT       FIELD_TYPE_FLOAT
#define MYSQL_TYPE_DOUBLE      FIELD_TYPE_DOUBLE
#define MYSQL_TYPE_NULL        FIELD_TYPE_NULL
#define MYSQL_TYPE_TIMESTAMP   FIELD_TYPE_TIMESTAMP
#define MYSQL_TYPE_LONGLONG    FIELD_TYPE_LONGLONG
#define MYSQL_TYPE_INT24       FIELD_TYPE_INT24
#define MYSQL_TYPE_DATE        FIELD_TYPE_DATE
#define MYSQL_TYPE_TIME        FIELD_TYPE_TIME
#define MYSQL_TYPE_DATETIME    FIELD_TYPE_DATETIME
#define MYSQL_TYPE_YEAR        FIELD_TYPE_YEAR
#define MYSQL_TYPE_NEWDATE     FIELD_TYPE_NEWDATE
#define MYSQL_TYPE_ENUM        FIELD_TYPE_ENUM
#define MYSQL_TYPE_SET         FIELD_TYPE_SET
#define MYSQL_TYPE_TINY_BLOB   FIELD_TYPE_TINY_BLOB
#define MYSQL_TYPE_MEDIUM_BLOB FIELD_TYPE_MEDIUM_BLOB
#define MYSQL_TYPE_LONG_BLOB   FIELD_TYPE_LONG_BLOB
#define MYSQL_TYPE_BLOB        FIELD_TYPE_BLOB
#define MYSQL_TYPE_VAR_STRING  FIELD_TYPE_VAR_STRING
#define MYSQL_TYPE_STRING      FIELD_TYPE_STRING
#define MYSQL_TYPE_CHAR        FIELD_TYPE_TINY
#define MYSQL_TYPE_INTERVAL    FIELD_TYPE_ENUM
#define MYSQL_TYPE_GEOMETRY    FIELD_TYPE_GEOMETRY
#endif


extern unsigned int myx_mysql_get_resultset_size_limit(MYSQL *mysql);


static int int_compare(const char*, const char*);
static int bit_compare(const char*, const char*);
static int dummy_compare(const char*, const char*);


static void prepare_value(MYSQL *mysql, const char *value, unsigned int new_value_length, 
                          MYX_RS_COLUMN_TYPE coltype, char *buffer);

/*
 * a list of comparison functions for each type (MYX_RS_COLUMN_TYPE)
 */
int (*SQL_VALUE_COMPARER[])(const char*, const char*)= {
  int_compare,    // MYX_RSCT_INTEGER= 0,
  strcmp,         // MYX_RSCT_FLOAT,
  g_utf8_collate, // MYX_RSCT_STRING,
  strcmp,         // MYX_RSCT_DATE,
  strcmp,         // MYX_RSCT_TIME,
  strcmp,         // MYX_RSCT_DATETIME,
  dummy_compare,  // MYX_RSCT_BLOB,
  g_utf8_collate, // MYX_RSCT_TEXT,
  g_utf8_collate, // MYX_RSCT_ENUM,
  g_utf8_collate, // MYX_RSCT_SET
  NULL,           // MYX_RSCT_DECIMAL,
  bit_compare,    // MYX_RSCT_BIT,
  dummy_compare,  // MYX_RSCT_TIMESTAMP,
  dummy_compare,  // MYX_RSCT_YEAR,
  dummy_compare,  // MYX_RSCT_NEWDATE,
  dummy_compare   // MYX_RSCT_NEWDECIMAL

};


/*
  Takes a SQL statement, analyzes and executes it and for selects returns a result set

  SYNOPSIS
    myx_query_execute()
    mysql                Pointer to MYSQL struct
  	sql                  pointer to SQL statement. NULL if the next resultset should be fetched
  	parameters           list of parameters to substitude in the form name=value
  	error_code           errorcode the function returns
    user_data            user defined data that is used in the callback function
  	callback_function    function that is called when a select is executed after CALLBACK_AFTER_BYTES are transfered from the server
  	affected_rows        The number of rows affected by the first result set in the current query (or the current one if
                         a multi-resultset is currently being retrieved.

  NOTES
    The callback function will be called after CALLBACK_AFTER_BYTES transfered by the server.
	  This only applys to selects.

  TODO
    Make commands except SELECTS work

  RETURN
    MYX_RESULTSET *              when a select is executed successfully
	  NULL                         for all other commands or when an error has occured
	  MYX_LIB_ERROR *error_code    MYX_NO_ERROR on success, other on error
*/

MYX_RESULTSET * myx_query_execute(MYSQL *mysql, const char *sql, int enforce_editable,
  MYX_STRINGLIST *parameters, MYX_LIB_ERROR *error_code, void *user_data,
  int (*progress_row_fetch) (unsigned long current_row_count, unsigned long previous_row_count, MYX_RESULTSET *result_set, void *user_data),
  void (*resultset_realloc_before) (void *user_data),
  void (*resultset_realloc_after) (void *user_data),
  bigint* affected_rows)
{
  MYX_RESULTSET *resultset= g_malloc0(sizeof(MYX_RESULTSET));

  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  int r;
  unsigned long *field_lengths;
  unsigned int i, k;
  unsigned long row_count, rows_allocated;
  unsigned long bytes_loaded= 0, previous_callback_row_count= 0;

  MYX_RS_COLUMN *current_column;
  MYX_RS_ROW *current_row;
  MYX_RS_FIELD *current_field;

  MYX_TIMER_VALUE timer;

  bigint memory_limit= myx_mysql_get_resultset_size_limit(mysql) * 1024LL * 1024LL;

  if (!resultset)
  {
    *error_code= MYX_OUT_OF_MEMORY;
    return NULL;
  }
  *error_code= MYX_NO_ERROR;

  resultset->mysql= mysql;

  // Make sure we read the server charset at least once (it is cached) otherwise
  // we might end up with trying to get it (via a query) while we are reading the result set
  // for the current query, which ends up in an error.
  myx_get_server_charset_name(mysql);

  if (sql)
  {
    resultset->query= query_analyze(mysql, sql);
    if (!resultset->query)
    {
      *error_code= MYX_OUT_OF_MEMORY;
      g_free(resultset);
      return NULL;
    }

    resultset->query->original_sql= g_strdup(sql);

    //Set parameters
    query_set_params(resultset->query, parameters);
    
    if ((resultset->query->query_type != MYX_QT_SELECT) &&
        (resultset->query->query_type != MYX_QT_SHOW) &&
        (resultset->query->query_type != MYX_QT_DESCRIBE) &&
        (resultset->query->query_type != MYX_QT_EXPLAIN) &&
        (resultset->query->query_type != MYX_QT_CALL) &&
        (resultset->query->query_type != MYX_QT_UNION) &&
        (resultset->query->query_type != MYX_QT_CHECK) &&
        (resultset->query->query_type != MYX_QT_ANALYZE) &&
        (resultset->query->query_type != MYX_QT_REPAIR) &&
        (resultset->query->query_type != MYX_QT_OPTIMIZE))
    {
      g_free(resultset);

      myx_query_execute_direct(mysql, sql, error_code, affected_rows);

      return NULL;
    }

    if (resultset->query->query_type == MYX_QT_SELECT)
    {
      resultset->editable= query_is_editable(resultset->query, enforce_editable);
    }
  }
  else
  {
    resultset->editable= 0;
  }

  // Start query timer
  timer_start(&timer);

  if (sql)
    r= myx_mysql_query(mysql, resultset->query->sql);
#if MYSQL_VERSION_ID >= 50000
  else
    r= mysql_next_result(mysql);
#endif
  // Stop query timer
  resultset->query_time= timer_stop(&timer);

  if (r==0)
  {
    // Start fetch timer
    timer_start(&timer);

    res= mysql_use_result(mysql);
    if (res != NULL)
    {
      //Allocate columns
      resultset->columns_num= mysql_num_fields(res);
      if (sql)
        resultset->columns_num_to_display= resultset->columns_num-resultset->query->pk_columns_added_num;
      else
        resultset->columns_num_to_display= resultset->columns_num;
      resultset->columns= g_malloc0(sizeof(MYX_RS_COLUMN) * resultset->columns_num);
      
      if (!resultset->columns)
      {
        *error_code= MYX_OUT_OF_MEMORY;
        mysql_free_result(res);
        return NULL;
      }
      
      fields = mysql_fetch_fields(res);

      for (i= 0;i<resultset->columns_num;i++)
      {
        current_column= resultset->columns + i;

        //Set column infos
        current_column->name= myx_convert_dbstr_utf8(mysql, fields[i].name, -1);
        if (!current_column->name)
        {
          *error_code= MYX_OUT_OF_MEMORY;
          mysql_free_result(res);
          myx_query_free_resultset(resultset);
          return NULL;
        }
        switch(fields[i].type)
        {
          // To be completed
          case MYSQL_TYPE_TINY:
          case MYSQL_TYPE_SHORT: 
          case MYSQL_TYPE_LONG:
          case MYSQL_TYPE_LONGLONG:
          case MYSQL_TYPE_INT24:
            current_column->column_type= MYX_RSCT_INTEGER;
            break;
          case MYSQL_TYPE_FLOAT:
          case MYSQL_TYPE_DOUBLE:
            current_column->column_type= MYX_RSCT_FLOAT;
            break;
          case MYSQL_TYPE_DECIMAL:
            current_column->column_type= MYX_RSCT_DECIMAL;
            break;
          case MYSQL_TYPE_BLOB:
          case MYSQL_TYPE_TINY_BLOB:
          case MYSQL_TYPE_MEDIUM_BLOB:
          case MYSQL_TYPE_LONG_BLOB:
            // TEXT BINARY comes as BLOB with a charset number != 63. The value 63 stands for a real BLOB.
            if (fields[i].flags & BINARY_FLAG && (fields[i].charsetnr == 63))
              current_column->column_type= MYX_RSCT_BLOB;
            else
              current_column->column_type= MYX_RSCT_TEXT;
            break;
          case MYSQL_TYPE_DATE:
            current_column->column_type= MYX_RSCT_DATE;
            break;
          case MYSQL_TYPE_TIME:
            current_column->column_type= MYX_RSCT_TIME;
            break;
          case MYSQL_TYPE_DATETIME:
            current_column->column_type= MYX_RSCT_DATETIME;
            break;
          case MYSQL_TYPE_ENUM:
            current_column->column_type= MYX_RSCT_ENUM;
            break;
          case MYSQL_TYPE_SET:
            current_column->column_type= MYX_RSCT_SET;
            break;
          case MYSQL_TYPE_BIT:
            current_column->column_type= MYX_RSCT_BIT;
            break;
          case MYSQL_TYPE_TIMESTAMP:
            current_column->column_type= MYX_RSCT_TIMESTAMP;
            break;
          case MYSQL_TYPE_YEAR:
            current_column->column_type= MYX_RSCT_YEAR;
            break;
          case MYSQL_TYPE_NEWDATE:
            current_column->column_type= MYX_RSCT_NEWDATE;
            break;
          case MYSQL_TYPE_NEWDECIMAL:
            current_column->column_type= MYX_RSCT_NEWDECIMAL;
            break;
          default:
            current_column->column_type= MYX_RSCT_STRING;
            break;
        }
        current_column->type_size= fields[i].length; 
      }

      //Add corresponding tables to columns
      add_table_info_to_rs_columns(resultset);

      //Allocate first block of rows
      resultset->rows= g_malloc0(sizeof(MYX_RS_ROW)*ROWS_ALLOCATE_BLOCK_COUNT);
      rows_allocated= ROWS_ALLOCATE_BLOCK_COUNT;

      if (!resultset->rows)
      {
        *error_code= MYX_OUT_OF_MEMORY;
        mysql_free_result(res);
        myx_query_free_resultset(resultset);
        return NULL;
      }
      
      row_count= 0;

      do
      {
        row= mysql_fetch_row(res);
        if (row == NULL)
          break;

        if (row_count>=rows_allocated)
        {
          void *new_buffer;
          
          if(resultset_realloc_before)
            (* resultset_realloc_before)(user_data);

          new_buffer= g_realloc(resultset->rows, sizeof(MYX_RS_ROW) * (rows_allocated+ROWS_ALLOCATE_BLOCK_COUNT));
          rows_allocated+= ROWS_ALLOCATE_BLOCK_COUNT;

          if (!new_buffer)
          {
            // out of mem, return with what we've got so far
            *error_code= MYX_OUT_OF_MEMORY;
            resultset->rows_num= row_count;
            mysql_free_result(res);
            return resultset;
          }
          else
            resultset->rows= new_buffer;

          if(resultset_realloc_after)
            (* resultset_realloc_after)(user_data);
        }

        //Get field lengths
        field_lengths= mysql_fetch_lengths(res);

        current_row= resultset->rows + row_count;

        current_row->diff= 0;
        
        //Alloc current row
        current_row->fields= g_malloc0(sizeof(MYX_RS_FIELD) * resultset->columns_num);
        if (!current_row->fields)
        {
          // out of mem, return with what we've got so far
          *error_code= MYX_OUT_OF_MEMORY;
          resultset->rows_num= row_count;
          mysql_free_result(res);
          return resultset;
        }

        for (k= 0; k < resultset->columns_num; k++)
        {
          current_column= resultset->columns + k;
          current_field= current_row->fields + k;

          current_field->value_length= field_lengths[k];
          
          resultset->memory_used+= current_field->value_length;
          
          if (row[k] != NULL)
          {
            switch (current_column->column_type)
            {
              case MYX_RSCT_BIT:
                {
                  // Bit columns come with pure binary values. Convert them to a binary string (e.g. b'100101').
                  ubigint temp= 0;
                  unsigned char* run= row[k];
                  while (*run != '\0')
                    temp= (temp << 8) + (ubigint) *run++;
                  current_field->value= myx_int_to_bit(temp);
                  
                  break;
                };
              case MYX_RSCT_STRING:
              case MYX_RSCT_TEXT:
                {
                  current_field->value= myx_convert_dbstr_utf8(mysql, row[k], current_field->value_length);
                  break;
                };
              default:
                {
                  current_field->value= g_memdup(row[k], current_field->value_length + 1);
                  current_field->value[current_field->value_length]= 0;
                };
            };

            bytes_loaded+= current_field->value_length;
          }
          else
          {
            if ((current_column->column_type == MYX_RSCT_STRING
                 || current_column->column_type == MYX_RSCT_TEXT)
                && current_field->value_length == 0 && row[k]!=NULL)
              current_field->value= g_strdup("");
            else
              continue; // NULL blob, don't check whether malloc failed
          }
          
          // checks whether the value copy failed 
          // (if the value is NULL then either the field is NULL or the
          // malloc failed)
          if (current_field->value==NULL && row[k]!=NULL)
          {
            int ii= k;
            while (ii >= 0)
              g_free(current_row->fields[ii--].value);
            g_free(current_row->fields);
            // out of mem, return with what we've got so far
            *error_code= MYX_OUT_OF_MEMORY;
            resultset->rows_num= row_count;
            mysql_free_result(res);
            
            return resultset;
          }
        }
        row_count++;

        // check if we've reached the memory usage limit
        if (memory_limit > 0 && resultset->memory_used > memory_limit)
        {
          *error_code= MYX_MEMORY_LIMIT_EXCEEDED;
          resultset->fetch_time= timer_stop(&timer);
          resultset->rows_num= row_count;
          mysql_free_result(res);
          
          return resultset;
        }

        // Call the callback-function after a fixed number of bytes have been transfered.
        if (bytes_loaded>=CALLBACK_AFTER_BYTES)
        {
          resultset->rows_num= row_count;

          if (progress_row_fetch && (*progress_row_fetch)(row_count, previous_callback_row_count, resultset, user_data))
          {
            // Don't free resultset, because the user might want to work with what is fetched so far.
            *error_code= MYX_STOP_EXECUTION;
            resultset->rows_num= row_count;
            resultset->fetch_time= timer_stop(&timer);

            mysql_free_result(res);

            return resultset;
          }
          else
          {
            previous_callback_row_count= row_count;
            bytes_loaded= 0;
          }

          //Simulate slow connection
          //Sleep(1000);
        }
      }
      while (1);

      resultset->rows_num= row_count; //(unsigned int)mysql_num_rows(res);
      resultset->rows= g_realloc(resultset->rows, sizeof(MYX_RS_ROW)*resultset->rows_num);

      resultset->memory_used+= sizeof(MYX_RS_ROW)*resultset->rows_num;

      //Call the callback-function one last time to let the program fetch the last records
      if (*error_code != MYX_STOP_EXECUTION && progress_row_fetch)
        (*progress_row_fetch)(resultset->rows_num, previous_callback_row_count, resultset, user_data);

      mysql_free_result(res);
      resultset->fetch_time= timer_stop(&timer);
    }
    else
    {
      *error_code= MYX_SQL_ERROR;
      myx_query_free_resultset(resultset);
      timer_stop(&timer);
      return NULL;
    }
  }
  else
  {
    *error_code= MYX_SQL_ERROR;
    myx_query_free_resultset(resultset);
    return NULL;
  }

#if MYSQL_VERSION_ID >= 50000
  resultset->has_more= mysql_more_results(mysql);
#else
  resultset->has_more= 0;
#endif
  return resultset;
}



static int do_rs_action_update(MYX_RESULTSET *resultset, MYX_RS_ACTION *action)
{
  unsigned int c;
  
  if (action->row >= resultset->rows_num)
    return -1;
  
  for (c= 0; c < resultset->columns_num; c++)
  {
    if (resultset->columns+c == action->column)
      break;
  }
  if (c == resultset->columns_num)
    return -1;

  g_free(resultset->rows[action->row].fields[c].value);
  if (action->new_value)
    resultset->rows[action->row].fields[c].value= g_memdup(action->new_value, action->new_value_length+1); // the +1 is to make sure we catch the \0 for strings
  else
    resultset->rows[action->row].fields[c].value= NULL;
  resultset->rows[action->row].fields[c].value_length= action->new_value_length;

  return 0;
}


static int do_rs_action_add(MYX_RESULTSET *resultset, MYX_RS_ACTION *action)
{
  unsigned int c;
  for (c= 0; c < resultset->columns_num; c++)
  {
    if (resultset->columns+c == action->column)
      break;
  }
  if (c == resultset->columns_num)
    return -1;

  if (action->row >= resultset->rows_num)
  {
    resultset->rows= g_realloc(resultset->rows,
                               sizeof(MYX_RS_ROW)*(action->row+1));
    memset(resultset->rows+resultset->rows_num, 0,
           (action->row+1-resultset->rows_num)*sizeof(MYX_RS_ROW));
    resultset->rows_num= action->row+1;
  }

  if (!resultset->rows[action->row].fields)
    resultset->rows[action->row].fields= g_new0(MYX_RS_FIELD, resultset->columns_num);

  if (action->new_value)
    resultset->rows[action->row].fields[c].value= g_memdup(action->new_value, action->new_value_length+1);
  resultset->rows[action->row].fields[c].value_length= action->new_value_length;

  return 0;
}

// a and b are NOT values but pointers to the values!!!
static int row_compare(const void *a, const void *b)
{
  int aa = *(const int *)(a);
  int bb = *(const int *)(b);
  return bb - aa;
/*
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  if (a < b)
    return 1;
  else if (a > b)
    return -1;
  else
    return 0;
#else
  if ((unsigned int)a < (unsigned int)b)
    return 1;
  else if ((unsigned int)a > (unsigned int)b)
    return -1;
  else
    return 0;
#endif
*/
}


int myx_query_update_resultset(MYX_RESULTSET *resultset)
{
  unsigned int a, i;
  unsigned int *rows_to_delete;
  unsigned int rows_to_delete_count= 0;

  unsigned int j;
  MYX_RS_ACTIONS *new_actions;
    
  if (!resultset->actions)
    return 0;

  // Update the resultset according to the actions that were applied to it
  // and remove the successful actions from the action list.
  
  rows_to_delete= g_new0(unsigned int, resultset->actions->actions_num);

  for (a= 0; a < resultset->actions->actions_num; a++)
  {
    MYX_RS_ACTION *action= resultset->actions->actions+a;

    if ((action->status == MYX_RSAS_FAILED) || (action->status == MYX_RSAS_DISCARDED))
      continue;

    switch (action->action)
    {
    case MYX_RSA_UPDATE:
      if (do_rs_action_update(resultset, action) < 0)
      {
        g_free(rows_to_delete);
        return -1;
      }
      break;
    case MYX_RSA_ADD:
      if (do_rs_action_add(resultset, action) < 0)
      {
        g_free(rows_to_delete);
        return -1;
      }
      break;
    case MYX_RSA_DELETE:
      // just make a list of rows to be deleted later
      for (i= 0; i < rows_to_delete_count; i++)
        if (rows_to_delete[i] == action->row)
          break;
      if (i == rows_to_delete_count)
      {
        rows_to_delete[rows_to_delete_count]= action->row;
        rows_to_delete_count++;
      }
      break;
    }
  }
  
  // Process the failed actions in the action list of the resultset
  new_actions= g_new0(MYX_RS_ACTIONS, 1);
  
  // Check if any of the actions failed
  for (i= 0; i < resultset->actions->actions_num; i++)
  {
    MYX_RS_ACTION *action= resultset->actions->actions+i;
    
    if (action->status == MYX_RSAS_FAILED)
    {
      int failed_action_fixed= 0;
      
      //Check if the action has been replaced by a fixing one
      for (j= resultset->actions->actions_num; j > i ; j--)
      {
        if ((resultset->actions->actions[j].status == MYX_RSAS_APPLIED) &&
            (resultset->actions->actions[j].column == action->column) &&
            (resultset->actions->actions[j].row == action->row))
        {
          failed_action_fixed= 1;
          break;
        }
      }

      //if the action has not been fixed, keep it in the new action list
      if (!failed_action_fixed)
      {
        // Check if there are other failed actions for the same row/column, if so
        // only leave the last one
        for (j= 0; j < new_actions->actions_num; j++)
        {
          if (new_actions->actions[j].column == action->column &&
              new_actions->actions[j].row == action->row)
          {
            break;
          }
        }
        
        if (j == new_actions->actions_num) // not found
        {
          new_actions->actions= g_realloc(new_actions->actions, 
                                          sizeof(MYX_RS_ACTION)*(new_actions->actions_num+1));
          new_actions->actions_num++;
        }
        else
          g_free(new_actions->actions[j].new_value);
        memcpy(new_actions->actions + j, action, sizeof(MYX_RS_ACTION));
        
        new_actions->actions[j].new_value= g_memdup(action->new_value, 
                                                    action->new_value_length+1);
      }
    }
  }

  // free the old action list
  for (i=0; i < resultset->actions->actions_num; i++)
  {
    g_free(resultset->actions->actions[i].new_value);
  }
  g_free(resultset->actions->actions);
  g_free(resultset->actions);
  
  // see if any failed action remain
  if (new_actions->actions_num == 0)
  {
    resultset->actions= NULL;
    g_free(new_actions);
  }
  else
    resultset->actions= new_actions;

  
  // Sort the list of deleted rows backwards and remove the deleted rows
  // from the resultset.
  qsort(rows_to_delete, rows_to_delete_count, sizeof(unsigned int),
        row_compare);

  for (i= 0; i < rows_to_delete_count; i++)
  {
    unsigned int row= rows_to_delete[i];
    unsigned int c;
    unsigned int j;

    if (row >= resultset->rows_num)
      continue;

    if (resultset->actions)
    {
      // Move actions up if the are below a deleted row
      // and set row to -1 for actions that are on the deleted row
      for (j=0; i<resultset->actions->actions_num; i++)
      {
        MYX_RS_ACTION *action= resultset->actions->actions+j;
        if (action->row > row)
          action->row--;
        else if (action->row == row)
          action->row= -1;
      }
    }

    if (resultset->rows[row].fields)
      for (c= 0; c < resultset->columns_num; c++)
        g_free(resultset->rows[row].fields[c].value);
    g_free(resultset->rows[row].fields);

    if (row < resultset->rows_num-1)
    {
      memmove(&resultset->rows[row],
              &resultset->rows[row+1],
              (resultset->rows_num-row-1)*sizeof(MYX_RS_ROW));
    }
    resultset->rows_num--;
  }
  g_free(rows_to_delete);
  

  return 0;
}


int myx_query_pack_resultset(MYX_RESULTSET *resultset)
{
  unsigned int row;
  for (row= 0; row < resultset->rows_num; row++)
  {
    if (resultset->rows[row].fields == NULL)
    {
      if (row < resultset->rows_num-1)
      {
        memmove(&resultset->rows[row],
                &resultset->rows[row+1],
                (resultset->rows_num-row-1)*sizeof(MYX_RS_ROW));
      }
      resultset->rows_num--;
    }
  }
  return 0;
}


int myx_query_free_resultset(MYX_RESULTSET *resultset)
{
  MYX_RS_COLUMN *current_column;
  MYX_RS_ROW *current_row;
  MYX_RS_FIELD *current_field;

  unsigned int i, j;

  if (resultset->columns)
  {
    for(i=0;i<resultset->columns_num;i++)
    {
      current_column= resultset->columns+i;
      g_free(current_column->name);
    }
    g_free(resultset->columns);
  }
  for(i=0;i<resultset->rows_num;i++)
  {
    current_row= resultset->rows+i;

    if (current_row->fields)
    {
      for(j=0; j<resultset->columns_num; j++)
      {
        current_field= current_row->fields+j;
        g_free(current_field->value);
      }
    }
    g_free(current_row->fields);
  }
  g_free(resultset->rows);

  if(resultset->actions)
  {
    for (i=0; i < resultset->actions->actions_num; i++)
    {
      g_free(resultset->actions->actions[i].new_value);
    }
    g_free(resultset->actions->actions);
    g_free(resultset->actions);
  }

  query_free(resultset->query);

  g_free(resultset);

  return 0;
} 

MYX_RS_ACTION * myx_query_create_action(MYX_RS_ACTION_TYPE action_type, unsigned int row, MYX_RS_COLUMN *column, unsigned int new_value_length, const char *new_value)
{
  MYX_RS_ACTION* action= g_malloc0(sizeof(MYX_RS_ACTION));

  action->action= action_type;
  action->status= MYX_RSAS_NEW;
  action->row= row;
  action->column= column;
  if (new_value)
  {
    action->new_value= g_malloc(new_value_length+1);
    memcpy(action->new_value, new_value, new_value_length);
    action->new_value[new_value_length]= 0;
    action->new_value_length= new_value_length;
  }

  return action;
}

int myx_query_free_action(MYX_RS_ACTION *action)
{
  g_free(action);
  
  return 0;
}


int myx_query_add_action(MYX_RESULTSET *resultset, MYX_RS_ACTION *action)
{
  MYX_RS_ACTIONS *actions;
  
  if (!resultset->actions)
    resultset->actions= g_malloc0(sizeof(MYX_RS_ACTIONS));
  
  actions= (MYX_RS_ACTIONS *)resultset->actions;

  actions->actions_num++;
  actions->actions= g_realloc(actions->actions,
                              sizeof(MYX_RS_ACTION)*actions->actions_num);

  actions->actions[actions->actions_num-1]= *action;

  return actions->actions_num-1;
}


int myx_query_delete_action(MYX_RESULTSET *resultset, unsigned int action)
{
  if (!resultset->actions)
    return -1;

  if (action < resultset->actions->actions_num)
  {    
    if (resultset->actions->actions_num > action+1)
      memmove(&resultset->actions->actions[action],
              &resultset->actions->actions[action+1],
              sizeof(MYX_RS_ACTION) * (resultset->actions->actions_num-action-1));
    resultset->actions->actions_num--;
    return 0;
  }

  return -1;
}



static void prepare_action_value(MYSQL *mysql, MYX_RS_ACTION *action, char *buffer)
{
  prepare_value(mysql, action->new_value, action->new_value_length,
                action->column->column_type, buffer);
}


static void prepare_value(MYSQL *mysql, const char *value, unsigned int value_length, 
                          MYX_RS_COLUMN_TYPE coltype, char *buffer)
{
  if (value == NULL)
  {
    strcpy(buffer, "NULL");
  }
  else
  {
    /* locate field name */
    switch (coltype)
    {
      case MYX_RSCT_INTEGER:
      case MYX_RSCT_FLOAT:
      case MYX_RSCT_DECIMAL:
      case MYX_RSCT_NEWDECIMAL:
      case MYX_RSCT_BIT:
      // we need to quote numbers as well. see bug #14841

      //memcpy(buffer, value, value_length);
      //buffer[value_length]= 0;
      //break;

    case MYX_RSCT_ENUM:
    case MYX_RSCT_SET:

      case MYX_RSCT_TIME:
      case MYX_RSCT_DATE:
      case MYX_RSCT_DATETIME:
      case MYX_RSCT_TIMESTAMP:
      case MYX_RSCT_YEAR:
      case MYX_RSCT_NEWDATE:
        g_snprintf(buffer, value_length*2 + 2 + 1, "'%s'", value);
        break;

      case MYX_RSCT_STRING:
      case MYX_RSCT_TEXT:
      case MYX_RSCT_BLOB:
        {
          int l;
          strcpy(buffer, "'");
          l= mysql_real_escape_string(mysql, buffer+1, value, value_length);
          strcpy(buffer+1+l, "'");
        }
        break;
    }
  }
}


static int is_pk_column(MYX_RS_COLUMN *column)
{
  return !column->table_column ? 0 : column->table_column->is_pk;
}


static int is_auto_inc_column(MYX_RS_COLUMN *column)
{
  return !column->table_column ? 0 : column->table_column->is_autoincrement;
}


static char *prepare_pks(MYX_RESULTSET *rs, MYX_RS_ACTION *action)
{
  char **values= NULL;
  MYX_RS_ROW *row= rs->rows + action->row;
  MYX_QUERY *query= rs->query;
  MYX_Q_TABLE *table;
  unsigned int i;
  char *cond;
  
  if (!action->column)
  {
    /* if there's no column in the action (MYX_RS_DELETE?). then take
     table from 1st column in query. this function should only be called
     for resultsets with single tables, so that's enough */
    table= rs->columns[0].table;
  }
  else
    table= action->column->table;

  g_assert(rs->columns_num == query->columns_num);

  //if this is not an added row, take value from RS
  if(action->row < rs->rows_num)
  {
    // iterate over list of columns
    for (i= 0; i < query->columns_num; i++)
    {
      unsigned int j;
      int is_pk= 0;

      // check if this column is a pk
      for (j= 0; j < table->pk_columns_num; j++)
      {
        if (query->columns[i].table == table &&
            strcmp(query->columns[i].column, table->pk_columns[j].column)==0)
        {
          is_pk= 1;
          break;
        }
      }
      if (is_pk)
      {
        char *tmp= g_malloc(row->fields[i].value_length*2+strlen(rs->columns[i].name)+8);

        sprintf(tmp, "`%s`=", rs->columns[i].name);
      
        prepare_value(rs->mysql, row->fields[i].value, row->fields[i].value_length,
                      rs->columns[i].column_type, tmp+strlen(tmp));

        strlist_g_append(&values, tmp);
      }
    }
  }
  //if this is a new added row, return an empty string
  else
  {
    return g_strdup("");
  }

  if (!values)
    g_error("NO PKS FOUND!");

  // join the condition
  cond= g_strjoinv(" AND ", values);
  g_strfreev(values);
  
  return cond;
}


static void append_error(MYX_RS_ACTION_ERRORS **errors, MYSQL *mysql, 
                         MYX_RS_ACTION *action)
{
  MYX_RS_ACTION_ERROR *error;
  
  if (!*errors)
    *errors= g_malloc0(sizeof(MYX_RS_ACTION_ERRORS));

  
  (*errors)->errors_num++;
  (*errors)->errors= g_realloc((*errors)->errors,
                               sizeof(MYX_RS_ACTION_ERROR)*(*errors)->errors_num);
  
  error= (*errors)->errors + (*errors)->errors_num-1;
  
  error->level= MYX_QEL_ERROR;
  error->error= myx_mysql_errno(mysql);
  error->error_text= myx_mysql_error(mysql); // myx_mysql_error() already strdup()s
  error->action= action;
}

#if MYSQL_VERSION_ID >= 50000
static void append_warnings(MYX_RS_ACTION_ERRORS **errors, MYSQL *mysql, 
                            MYX_RS_ACTION *action)
{
  MYX_RS_ACTION_ERROR *error;
  MYSQL_RES *res;
  MYSQL_ROW row;
  int c;

  c= mysql_warning_count(mysql);
  res= NULL;
  if (c > 0 && !myx_mysql_query(mysql, "SHOW WARNINGS"))
    res= mysql_use_result(mysql);
  if (res != NULL)
  {
    if (!*errors)
      *errors= g_malloc0(sizeof(MYX_RS_ACTION_ERRORS));

    (*errors)->errors= g_realloc((*errors)->errors,
                                 sizeof(MYX_RS_ACTION_ERROR)*((*errors)->errors_num+c));

    do
    {
      row= mysql_fetch_row(res);
      if (row == NULL)
        break;

      error= (*errors)->errors + (*errors)->errors_num++;

      if (strcasecmp(row[0], "Note")==0)
        error->level= MYX_QEL_NOTE;
      else
        error->level= MYX_QEL_WARNING;
      error->error= atoi(row[1]);
      error->error_text= g_strdup(row[2]);
      error->action= action;
    }
    while (1);

    mysql_free_result(res);
  }
}
#else // mysql < 5.0
// nothing
# define append_warnings(a,b,c) do {} while (0)
#endif // mysql < 5.0

#if 0
static void append_charset_error(MYX_RS_ACTION_ERRORS **errors,
                                 const char *charset,
                                 MYX_RS_ACTION *action)
{
  MYX_RS_ACTION_ERROR *error;
  
  if (!*errors)
    *errors= g_malloc0(sizeof(MYX_RS_ACTION_ERRORS));

  (*errors)->errors_num++;
  (*errors)->errors= g_realloc((*errors)->errors,
                               sizeof(MYX_RS_ACTION_ERROR)*(*errors)->errors_num);
  
  error= (*errors)->errors + (*errors)->errors_num-1;
  
  error->error= MYX_UTF8_OUT_OF_RANGE;
  error->error_text= g_strdup_printf("String contains characters invalid in the target character set (%s).",
                                     charset);
  error->action= action;
}


static int check_conversion(GIConv gic, const char *string)
{
  char buffer[256];
  gchar *out, *in= (gchar*)string;
  gsize inb= (gsize)strlen(string), outb;
  size_t sz;
  
  while (inb > 0)
  {
    out= (gchar*)buffer;
    outb= sizeof(buffer);
    sz= g_iconv(gic, &in, &inb, &out, &outb);
    if (sz == (size_t)-1 || sz > 0)
      return 0;
  }
  return 1;
}
#endif


static void change_same_row_actions_of_type(MYX_RESULTSET *resultset, int i, int add,
                                            MYX_RS_ACTION_STATUS new_status, int exclude_blobs)
{
  MYX_RS_ACTION *action= resultset->actions->actions + i;
  unsigned int j;
  for (j= i; j < resultset->actions->actions_num; j++)
  {
    MYX_RS_ACTION *action2= resultset->actions->actions+j;

    if (action2->row == action->row
        && action2->status == MYX_RSAS_NEW
        && ((add && (action2->action == MYX_RSA_ADD
                     || action2->action == MYX_RSA_UPDATE))
            ||
            (!add && (action2->action == MYX_RSA_UPDATE)))
        && action2->column->table == action->column->table)
    {
      if (exclude_blobs && action2->column && action2->column->column_type == MYX_RSCT_BLOB)
        ;
      else
        action2->status = new_status;
    }
  }
}



static int add_same_row_actions_of_type(MYX_RESULTSET *resultset, int i, int add,
                                        char ***ret_keys, char ***ret_values,
                                        char *buffer, int exclude_blobs)
{
  MYX_RS_ACTION *action= resultset->actions->actions + i;
  char **columns_arr= NULL;
  char **values_arr= NULL;
  char *t;
  unsigned int j;
  int count= 1;

  // Prepare insert
  strlist_g_append(&columns_arr, quote_identifier(action->column->name, '`'));

  if (action->column->column_type == MYX_RSCT_BLOB && exclude_blobs)
  {
    strlist_g_append(&values_arr, g_strdup("'<placeholder>'"));
  }
  else
  {
    prepare_action_value(resultset->mysql, action, buffer);
    strlist_g_append(&values_arr, g_strdup(buffer));
  }

  //Get all values for the insert
  for (j= i+1; j < resultset->actions->actions_num; j++)
  {
    MYX_RS_ACTION *action2= resultset->actions->actions+j;

    if (action2->row == action->row && action2->status == MYX_RSAS_NEW
        && ((add && (action2->action == MYX_RSA_ADD
                     || action2->action == MYX_RSA_UPDATE))
            ||
            (!add && (action2->action == MYX_RSA_UPDATE)))
        && action2->column->table == action->column->table)
    {
      int q;
      // look up the column in the list
      for (q= 0; columns_arr[q]; q++) 
      {
        t= quote_identifier(action2->column->name, '`');
        if (strcmp(t, columns_arr[q])==0)
        { // value is already in the list, replace it with the newer one
          g_free(t);
          g_free(values_arr[q]);
          prepare_action_value(resultset->mysql, action2, buffer);
          values_arr[q]= g_strdup(buffer);
          break;
        }
        g_free(t);
      }
      if (!columns_arr[q])
      {
        strlist_g_append(&columns_arr, quote_identifier(action2->column->name, '`'));
        
        if (action2->column->column_type == MYX_RSCT_BLOB && exclude_blobs)
        {
          strlist_g_append(&values_arr, g_strdup("'<placeholder>'"));
        }
        else
        {
          prepare_action_value(resultset->mysql, action2, buffer);
          strlist_g_append(&values_arr, g_strdup(buffer));
          count++;
        }
      }
    }
  }

  *ret_keys= columns_arr;
  *ret_values= values_arr;
  
  return count;
}


#if 0
static MYX_RS_ACTION_ERRORS *check_action_character_sets(MYX_RESULTSET *resultset)
{
  GIConv *gic= g_new0(GIConv, resultset->query->columns_num);
  char **charsets= g_new0(char*, resultset->query->columns_num);

  // create a iconv context for each column, so we can check
  // whether data can be converted to the target charset without losses
  for (i= 0; i < resultset->columns_num; i++)
  {
    MYX_Q_TABLE_COLUMN *tcol= NULL;
    MYX_Q_TABLE *table= resultset->columns[i].table;
    unsigned int j;

    if(!table)
    {
      append_error(&errors, resultset->mysql, resultset->actions->actions+i);

      g_free(buffer);
      return errors;
    }

    // locate the MYX_Q_TABLE_COLUMN for this column
    for (j= 0; j < table->columns_num; j++)
    {
      if (strcmp(table->columns[j].column, resultset->query->columns[i].column)==0)
      {
        tcol= table->columns+j;
        break;
      }
    }
    //XXX dont check if BLOB
    if (tcol && tcol->charset)
    {
      if (strcasecmp(tcol->charset, "utf-8")==0 || strcasecmp(tcol->charset, "utf8")==0)
        gic[i]= NULL;
      else
      {
        gic[i]= g_iconv_open(tcol->charset, "utf-8");
        charsets[i]= tcol->charset;
      }
    }
    else if (table->charset)
    {
      gic[i]= g_iconv_open(table->charset, "utf-8");
      charsets[i]= table->charset;
    }
  }

  // check whether all data to be sent can be converted to the final
  // charset without problems
  for (i= 0; i < resultset->actions->actions_num; i++)
  {
    unsigned int colnum;
    for (colnum= 0; colnum < resultset->columns_num; colnum++)
    {
      if (resultset->actions->actions[i].column == resultset->columns+colnum)
        break;
    }
    g_return_val_if_fail(colnum < resultset->columns_num, NULL);

    if (gic[colnum] && resultset->actions->actions[i].new_value &&
        !check_conversion(gic[colnum], resultset->actions->actions[i].new_value))
    {
      append_charset_error(&errors, charsets[i], resultset->actions->actions+i);
    }
  }
  for (i= 0; i < resultset->columns_num; i++)
  {
    if (gic[i])
      g_iconv_close(gic[i]);
  }
  g_free(gic);
  g_free(charsets);
  
  return errors;
}
#endif


static int do_update_value(MYX_RESULTSET *resultset, const char *pks, 
                           MYX_RS_ACTION *action,
                           MYX_RS_ACTION_ERRORS **errors, char *buffer)
{
  char *query;
  int mysql_is_50= mysql_version_is_later_or_equal_than(resultset->mysql,5,0);

  prepare_action_value(resultset->mysql, action, buffer);

  if(action->column->table->schema)
    query= g_strdup_printf("UPDATE `%s`.`%s` SET `%s`=%s WHERE %s;",
                           action->column->table->schema,
                           action->column->table->name,
                           action->column->name, buffer, pks);
  else
    query= g_strdup_printf("UPDATE `%s` SET `%s`=%s WHERE %s;",
                           action->column->table->name,
                           action->column->name, buffer, pks);
        
  // for blobs, we need to set the connection charset to binary
  // otherwise, the server will interpret the binary data as utf8
  // and mangle everything
  // set the connection to BINARY and send BLOB fields
  if ((mysql_version_is_later_or_equal_than(resultset->mysql,4,1)) && (action->column->column_type == MYX_RSCT_BLOB))
  {
    if (myx_mysql_query(resultset->mysql, "SET NAMES binary") != 0)
      g_warning("Can't set character set to binary while saving resultset changes");
  }

  // execute query
  if (myx_mysql_query(resultset->mysql, query)!=0)
  {
    action->status= MYX_RSAS_FAILED;
    append_error(errors, resultset->mysql, action);
    
    if (mysql_is_50)
      append_warnings(errors, resultset->mysql, action);
  }
  else
  {
    action->status= MYX_RSAS_APPLIED;
    
    if (mysql_is_50)
      append_warnings(errors, resultset->mysql, action);    
  }
  g_free(query);
        
  // restore the encoding to utf8
  if ((mysql_version_is_later_or_equal_than(resultset->mysql,4,1)) && (action->column->column_type == MYX_RSCT_BLOB))
  {
    if (myx_mysql_query(resultset->mysql, "SET CHARACTER SET utf8") != 0)
      g_warning("Can't set character set back to utf8 while saving resultset changes");
  }
  
  return 0;
}


MYX_RS_ACTION_ERRORS *myx_query_apply_actions(MYX_RESULTSET *resultset)
{
  MYX_RS_ACTION_ERRORS *errors= NULL;
  unsigned int i;
  unsigned int buffer_size;
  char *buffer;
  char already_deleted;
  int mysql_is_50= mysql_version_is_later_or_equal_than(resultset->mysql,5,0);

  buffer_size= 100;
  // go through all values to make a big enough buffer
  for (i= 0; i < resultset->actions->actions_num; i++)
  {
    MYX_RS_ACTION *action= resultset->actions->actions+i;
    if (action->new_value_length*2+1+2 > buffer_size)
    {
      buffer_size= action->new_value_length*2+1+2;
    }
  }
  buffer= g_malloc(buffer_size);
  
  // process actions
  i= 0;
  while (i < resultset->actions->actions_num)
  {
    MYX_RS_ACTION *action= resultset->actions->actions+i;

    if (action->status != MYX_RSAS_NEW)
    {
      i++;
      continue;
    }
    switch (action->action)
    {
    case MYX_RSA_ADD:
      // go over all ADDs for the same row and commit at once
      {
        char **keys_arr, **values_arr;
        char *keys, *values;
        char *query;
        char *pks;
        MYX_RS_ACTION_STATUS result;
        unsigned int j;

        //Check if this new row has not been deleted as well (shouldn't happen?)
        already_deleted= 0;
        for (j= 0; j < resultset->actions->actions_num; j++)
        {
          MYX_RS_ACTION *action2= resultset->actions->actions+j;

          if(j == i)
            continue;

          if (action2->row == action->row
              && action2->action == MYX_RSA_DELETE
              && action2->status == MYX_RSAS_NEW
              && action2->column->table == action->column->table)
          {
            action->status= MYX_RSAS_DISCARDED;
            action2->status= MYX_RSAS_DISCARDED;
            already_deleted= 1;
            break;
          }
        }
        if(already_deleted)
          continue;
        
        //add_same_row_actions_of_type(resultset, i, 1, &keys_arr, &values_arr, buffer, 1);
        add_same_row_actions_of_type(resultset, i, 1, &keys_arr, &values_arr, buffer, 0);
        
        keys= g_strjoinv(",", keys_arr);
        values= g_strjoinv(",", values_arr);
        
        g_strfreev(keys_arr);
        g_strfreev(values_arr);

        if(action->column->table->schema)
          query= g_strdup_printf("INSERT INTO `%s`.`%s` (%s) VALUES (%s);",
                                action->column->table->schema,
                                action->column->table->name,
                                keys, values);
        else
          query= g_strdup_printf("INSERT INTO `%s` (%s) VALUES (%s);",
                                action->column->table->name,
                                keys, values);
        g_free(keys);
        g_free(values);

        // execute query
        if (myx_mysql_query(resultset->mysql, query)!=0)
        {
          result= MYX_RSAS_FAILED;
          append_error(&errors, resultset->mysql, action);
          if (mysql_is_50)
            append_warnings(&errors, resultset->mysql, action);
        }
        else
        {
          unsigned int c;
          MYX_RS_COLUMN *auto_inc_column= NULL;
          
          if (mysql_is_50)
            append_warnings(&errors, resultset->mysql, action);
          
          /* check if the table has an auto-inc column that wasn't mentioned
           in the INSERT. in that case, we will get the value assigned to that
           column and add an action to the resultset with that value to signal
           the GUI about it. */
          for (c= 0; c < resultset->columns_num; c++)
          {
            MYX_RS_COLUMN *col= resultset->columns+c;

            if (is_auto_inc_column(col))
            {
              int flag= 0;

              // check if the column was mentioned in an action
              for (j= 0; j < resultset->actions->actions_num; j++)
              {
                MYX_RS_ACTION *action2= resultset->actions->actions+j;

                if (action2->row == action->row &&
                    action2->status == MYX_RSAS_NEW &&
                    (action2->action == MYX_RSA_ADD || action2->action == MYX_RSA_UPDATE) &&
                    (strcmp(action2->column->name, col->name)==0))
                {
                  flag= 1;
                  break;
                }
              }
              if (!flag)
              {
                auto_inc_column= col;
                break;
              }
            }
          }
          if (auto_inc_column)
          {
            MYX_RS_ACTION *naction;
            char tmp[50];

            g_snprintf(tmp, sizeof(tmp), "%lli", mysql_insert_id(resultset->mysql));
            naction= myx_query_create_action(MYX_RSA_ADD,
                                             action->row, auto_inc_column,
                                             (unsigned int) strlen(tmp), tmp);
            naction->status = MYX_RSAS_APPLIED;
            myx_query_add_action(resultset, naction);
            g_free(naction);
          }
          result= MYX_RSAS_APPLIED;
        };

        // go over all the involved records and update their status
        change_same_row_actions_of_type(resultset, i, 1, result, 0);

        g_free(query);
      }
      break;
    case MYX_RSA_UPDATE:
      // commit one update at a time
      // but PK changes have to be commited all at once and after
      // all other changes to the same row have been done
      if (!is_pk_column(action->column))
      {
        /* setup primary keys */
        char *pks= prepare_pks(resultset, action);

        do_update_value(resultset, pks, action, &errors, buffer);
        
        g_free(pks);
      }
      break;
    case MYX_RSA_DELETE:
      // just delete it
      {
        char *query;
        char *pks;

        pks= prepare_pks(resultset, action);

        //do not delete if pks is an empty string,
        //that can occure if a new added row was deleted
        if(pks && *pks)
        {
          MYX_Q_TABLE *table= resultset->columns[0].table;
          
          if(table->schema)
            query= g_strdup_printf("DELETE FROM `%s`.`%s` WHERE %s;",
                                  table->schema,
                                  table->name,
                                  pks);
          else
            query= g_strdup_printf("DELETE FROM `%s` WHERE %s;",
                                  table->name,
                                  pks);

          // execute query
          if (myx_mysql_query(resultset->mysql, query)!=0)
          {
            action->status= MYX_RSAS_FAILED;
            append_error(&errors, resultset->mysql, action);
            
            if (mysql_is_50)
              append_warnings(&errors, resultset->mysql, action);
          }
          else
          {
            action->status= MYX_RSAS_APPLIED;
            if (mysql_is_50)
              append_warnings(&errors, resultset->mysql, action);
          }
          g_free(query);
        }
        else
        {
          action->status= MYX_RSAS_APPLIED;
        }
        g_free(pks);
      }
      break;
    }

    i++;
  }

  // Now save changes to PK columns.
  i= 0; 
  while (i < resultset->actions->actions_num)
  {
    MYX_RS_ACTION *action= resultset->actions->actions+i;

    if (action->action == MYX_RSA_UPDATE &&
        action->status == MYX_RSAS_NEW &&
        is_pk_column(action->column))
    {      
      char *query;
      char *pks;
      char **keys_arr= NULL, **values_arr= NULL;
      char *str;
      int j;

      /* setup primary keys */
      pks= prepare_pks(resultset, action);


      add_same_row_actions_of_type(resultset, i, 0, &keys_arr, &values_arr, buffer, 1);

      str= NULL;
      for (j= 0; keys_arr[j]; j++)
      {
        char *tmp= g_strdup_printf("%s%s=%s", j>0?", ":"", keys_arr[j], values_arr[j]);

        str= str_g_append_and_free(str, tmp);
      }
      g_strfreev(keys_arr);
      g_strfreev(values_arr);

      if(action->column->table->schema)
        query= g_strdup_printf("UPDATE `%s`.`%s` SET %s WHERE %s;",
                               action->column->table->schema,
                               action->column->table->name,
                               str, pks);
      else
        query= g_strdup_printf("UPDATE `%s` SET %s WHERE %s;",
                               action->column->table->name,
                               str, pks);
      g_free(pks);
      g_free(str);

      // execute query
      if (myx_mysql_query(resultset->mysql, query)!=0)
      {
        action->status= MYX_RSAS_FAILED;
        append_error(&errors, resultset->mysql, action);
        
        if (mysql_is_50)
          append_warnings(&errors, resultset->mysql, action);
      }
      else
      {
        action->status= MYX_RSAS_APPLIED;
        
        if (mysql_is_50)
          append_warnings(&errors, resultset->mysql, action);
      }
      
      // go over all the involved records and update their status
      change_same_row_actions_of_type(resultset, i, 0, action->action, 1);

      g_free(query);
    }
    
    i++;
  }

  g_free(buffer);
  
  return errors;
}


int myx_query_discard_actions(MYX_RESULTSET *resultset)
{
  unsigned int i;
  MYX_RS_ACTION *action;

  if(resultset->actions)
  {
    for(i=0;i<resultset->actions->actions_num;i++)
    {
      action= resultset->actions->actions+i;

      if( action->status == MYX_RSAS_NEW )
      {
        g_free(action->new_value);
        action->new_value= NULL;
        action->new_value_length= 0;

        action->status= MYX_RSAS_DISCARDED;
      }
    }

    /*g_free(resultset->actions->actions);
    g_free(resultset->actions);

    resultset->actions= NULL;*/
  }

  return 0;
}


int myx_query_free_action_errors(MYX_RS_ACTION_ERRORS *errors)
{
  unsigned int i;

  for (i= 0; i < errors->errors_num; i++)
    g_free(errors->errors[i].error_text);
  g_free(errors->errors);
  g_free(errors);
  
  return 0;
}


MYX_RESULTSET * add_table_info_to_rs_columns(MYX_RESULTSET *resultset)
{
  unsigned int i;
  MYX_RS_COLUMN *rs_col;
  MYX_Q_COLUMN *q_col;

  if (!resultset->query)
    return resultset;

  for(i=0;i<MIN(resultset->columns_num, resultset->query->columns_num);i++)
  {
    rs_col= resultset->columns+i;
    q_col= resultset->query->columns+i;

    rs_col->table= q_col->table;
    rs_col->table_column= q_col->table_column;
  }

  return resultset;
}


#if 0 //XXX
void prepare_action_list()
{
  // if there's a delete, remove all previous inserts and updates for that row

  // if there are more than 1 update for the same column, remove previous

  // !!! if there's an insert, remove all the inserts for the same column
}
#endif


static int rscmp(const void *a, const void *b, gpointer udata)
{
  MYX_RESULTSET *rs= (MYX_RESULTSET*)udata;
  MYX_RS_ROW *ra= rs->rows + *((unsigned int*)a);
  MYX_RS_ROW *rb= rs->rows + *((unsigned int*)b);
  unsigned int c;
  
  // compare primary keys in order of appearance
  for (c= 0; c < rs->columns_num; c++)
  {
    MYX_Q_TABLE *table= rs->columns[c].table;
    int d;

    if(table)
    {
      if (table->columns[c].is_pk)
      {
        if (!ra->fields[c].value && !rb->fields[c].value)
          d= 0;
        else if (!ra->fields[c].value && rb->fields[c].value)
          d= -1;
        else if (ra->fields[c].value && !rb->fields[c].value)
          d= 1;
        else
          d= (*SQL_VALUE_COMPARER[rs->columns[c].column_type])(ra->fields[c].value, rb->fields[c].value);
        if (d != 0)
          return d;
      }
    }
  }
  return 0;
}


static int rscmp2(unsigned int *a, unsigned int *b,
                  MYX_RESULTSET *rsa, MYX_RESULTSET *rsb)
{
  MYX_RS_ROW *ra= rsa->rows + *a;
  MYX_RS_ROW *rb= rsb->rows + *b;
  unsigned int c;
  
  // compare primary keys in order of appearance
  for (c= 0; c < rsa->columns_num; c++)
  {
    MYX_Q_TABLE *table= rsa->columns[c].table;
    int d;

    if(table)
    {
      if (table->columns[c].is_pk)
      {      
        if (!ra->fields[c].value && !rb->fields[c].value)
          d= 0;
        else if (!ra->fields[c].value && rb->fields[c].value)
          d= -1;
        else if (ra->fields[c].value && !rb->fields[c].value)
          d= 1;
        else
          d= (*SQL_VALUE_COMPARER[rsa->columns[c].column_type])(ra->fields[c].value, rb->fields[c].value);

        if (d != 0)
          return d;
      }
    }
  }
  return 0;
}


int myx_query_compare_possible(MYX_RESULTSET *ra, MYX_RESULTSET *rb)
{
  unsigned int i;
  
  // fix of #12165
  if(!ra || !rb)
  {
    return 0;
  }
  
  if (ra->columns_num != rb->columns_num)
    return 0;
  
#define STRCMP(a,b) (!(a) && !(b) ? 0 : strcmp2(a,b))
  
  // compare column names and tables
  for (i= 0; i < ra->columns_num; i++)
  {
    if (!ra->columns[i].table || !rb->columns[i].table)
    {
      if (/*ra->columns[i].table == rb->columns[i].table
          &&*/ STRCMP(ra->columns[i].name, rb->columns[i].name)==0)
        ; // ok
      else
        return 0;
    }
    else if (STRCMP(ra->columns[i].table->catalog,
                    rb->columns[i].table->catalog)!=0
           /*  || STRCMP(ra->columns[i].table->schema,
                       rb->columns[i].table->schema)!=0
             || STRCMP(ra->columns[i].table->name,
                       rb->columns[i].table->name)!=0 */
             || STRCMP(ra->columns[i].name, rb->columns[i].name)!=0)
      return 0;
  }

  return 1;
}



int myx_query_compare_results(MYX_RESULTSET *ra, MYX_RESULTSET *rb)
{
  unsigned int *indexa;
  unsigned int *indexb;
  MYX_RS_ROW *new_rowsa;
  MYX_RS_ROW *new_rowsb;
  unsigned int ni;
  unsigned int i;
  unsigned int ia, ib;
  unsigned int last_indexa, last_indexb;
  unsigned int total;
  
  g_return_val_if_fail(ra->columns_num == rb->columns_num, -1);
  
  // fill index structs (only non-empty rows)
  indexa= g_malloc(sizeof(unsigned int)*ra->rows_num);
  indexb= g_malloc(sizeof(unsigned int)*rb->rows_num);

  last_indexa= 0;
  for (i= 0; i < ra->rows_num; i++)
  {
    if (ra->rows[i].fields)
      indexa[last_indexa++]= i;
  }
  last_indexb= 0;
  for (i= 0; i < rb->rows_num; i++)
  {
    if (rb->rows[i].fields)
      indexb[last_indexb++]= i;
  }
  // sort index structs

  g_qsort_with_data(indexa, last_indexa, sizeof(unsigned int), rscmp, ra);
  g_qsort_with_data(indexb, last_indexb, sizeof(unsigned int), rscmp, rb);
  
  ia= 0;
  ib= 0;
  ni= 0;

  total= MAX(ra->rows_num, rb->rows_num);
  new_rowsa= g_malloc(sizeof(MYX_RS_ROW)*total);
  new_rowsb= g_malloc(sizeof(MYX_RS_ROW)*total);
  
  // go through resultsets, marking new, missing and different rows
  while (ia < last_indexa || ib < last_indexb)
  {
    int d;
    
    if (ia == last_indexa)
      d= 1;
    else if (ib == last_indexb)
      d= -1;
    else
      d= rscmp2(indexa+ia, indexb+ib, ra, rb);
    
    if (ni >= total)
    {
      total += 256;
      new_rowsa= g_realloc(new_rowsa, sizeof(MYX_RS_ROW)*total);
      new_rowsb= g_realloc(new_rowsb, sizeof(MYX_RS_ROW)*total);
    }

    if (d < 0)
    {
      if (ia < last_indexa)
      {
        // this row only exists in A
        new_rowsa[ni]= ra->rows[indexa[ia]];
        memset(&new_rowsb[ni], 0, sizeof(MYX_RS_ROW));
        new_rowsa[ni].diff= MYX_RD_THIS_ONLY;
        new_rowsb[ni].diff= MYX_RD_OTHER_ONLY;
        ni++;
        ia++;
      }
    }
    else if (d > 0)
    {
      if (ib < last_indexb)
      {
        // this row only exists in B
        new_rowsb[ni]= rb->rows[indexb[ib]];
        memset(&new_rowsa[ni], 0, sizeof(MYX_RS_ROW));
        new_rowsa[ni].diff= MYX_RD_OTHER_ONLY;
        new_rowsb[ni].diff= MYX_RD_THIS_ONLY;
        ni++;
        ib++;
      }
    }
    else
    {
      bigint diff_mask = 0;
      int eq= 1;
      // Both rows exist, compare the contents, but set the column mask only for the first 60 columns.
      // 60 instead 64 is because of the lower nibble, which is used for the MYX_RD_DIFFERS flag.
      for (i= 0; i < ra->columns_num; i++)
      {
        if (ra->rows[indexa[ia]].fields[i].value_length != rb->rows[indexb[ib]].fields[i].value_length)
        {
          eq = 0;
          if (i < 60)
            diff_mask |= (1 << i);
        }
        else 
          if (memcmp(ra->rows[indexa[ia]].fields[i].value, rb->rows[indexb[ib]].fields[i].value, ra->rows[indexa[ia]].fields[i].value_length) != 0)
          {
            eq = 0;
            if (i < 60)
              diff_mask |= (1 << i);
          };
      };
      
      new_rowsa[ni]= ra->rows[indexa[ia]];
      new_rowsb[ni]= rb->rows[indexb[ib]];

      if (!eq)
      {
        // If the diff mask is 0 at this point then we had at least one difference in a column with index > 59.
        // Mark all columns < 60 as differing in this case.
        if (diff_mask == 0)
          diff_mask = -1;
        new_rowsa[ni].diff = (diff_mask << 4) | MYX_RD_DIFFERS;
        new_rowsb[ni].diff = (diff_mask << 4) | MYX_RD_DIFFERS;
      }
      else
      {        
        new_rowsa[ni].diff = MYX_RD_MATCHES;
        new_rowsb[ni].diff = MYX_RD_MATCHES;
      }
      ni++;
      ia++;
      ib++;
    }
  }

  // free tmp data
  g_free(indexa);
  g_free(indexb);

  // replace rows structs from resultsets
  g_free(ra->rows);
  ra->rows= new_rowsa;
  ra->rows_num= ni;

  g_free(rb->rows);
  rb->rows= new_rowsb;
  rb->rows_num= ni;

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * int_compare- compare 2 strings numerically
 *
 * DESCRIPTION
 * 
 *  We compare 2 strings numerically by checking the string lengths
 * The shortest one is the smallest. If both have the same length,
 * then compare with plain strcmp. We skip all zeros at left, after
 * checking sign (and comparing the signs, if necessary).
 * 
 * RETURN VALUE
 * 
 * -1 if a < b
 * +1 if a > b
 * 0 if a == b
 *
 * NOTES
 * 
 * Only integers allowed!
 */
static int int_compare(const char *a, const char *b)
{
  int asign= 1, bsign= 1;
  int alen, blen;

  // check signs
  if (*a == '-')
  {
    asign=-1;
    a++;
  }
  if (*b == '-')
  {
    bsign=-1;
    b++;
  }
  if (asign < bsign) // -x < +x
    return -1;
  if (asign > bsign) // +x > -x
    return 1;
  
  while (*a=='0') a++;
  while (*b=='0') b++;

  // check case where either is 0
  if (!*a && !*b) // both 0
    return 0;
  if (!*a) // a is 0
    return -bsign; // if b < 0, then a > b so return 1, which is -bsign
  if (!*b) // b is 0
    return asign; // if a < 0, then a < b  so return -1, which is asign
  
  alen= (int)strlen(a);
  blen= (int)strlen(b);
  
  if (alen < blen) // a < b
    return -1;
  else if (alen > blen) // a > b
    return 1;
  else
    return strcmp(a, b);
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Compares two bit values given as string.
 *
 * @param a The left bit value.
 * @param b The right bit value.
 *
 * @return -1 if a < b, 0 if both are the same, 1 if b is > a.
 */
static int bit_compare(const char *a, const char *b)
{
  unsigned int left= myx_bit_to_int(a);
  unsigned int right= myx_bit_to_int(b);

  return left - right;
}

//----------------------------------------------------------------------------------------------------------------------

static int dummy_compare(const char*a, const char*b)
{
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static char *replace_variables(char *str, MYX_RESULTSET *resultset)
{
  time_t t;
  char *tmp;

  if(resultset->query && resultset->query->sql)
  {
    tmp= str_g_replace(g_strdup(resultset->query->original_sql), "\r\n", " ");
    tmp= str_g_replace(tmp, "\n", " ");
  }
  else
  {
    tmp= g_strdup("");
  }

  time(&t);
  str= str_g_replace(str, "$DATE$", ctime(&t));
  str= str_g_replace(str, "$QUERY$", tmp);
  g_free(tmp);

  return str;
}

//----------------------------------------------------------------------------------------------------------------------

// Resultset Export
static int export_detail(MYX_TABLE_EXPORTER_INFO *info,
                         MYX_RESULTSET *resultset)
{
  unsigned int i, c;
  char **columns;
  
  (*info->te->table_header)(info);
  
  columns= g_new0(char*,resultset->columns_num);

  for (i= 0; i < resultset->rows_num; i++)
  {
    for (c= 0; c < resultset->columns_num; c++)
      columns[c]= resultset->rows[i].fields[c].value;

    (*info->te->columns)(info, (const char**)columns);
  }
  
  g_free(columns);

  (*info->te->table_footer)(info);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static MYX_STRINGLIST *make_param_list(MYSQL *mysql, MYX_RESULTSET *resultset, unsigned int row)
{
  MYX_STRINGLIST *sl= g_new0(MYX_STRINGLIST, 1);
  unsigned int i, size;

  sl->strings= g_new0(char*,resultset->columns_num);
  
  size= 0;
  for (i= 0; i < resultset->columns_num; i++)
    if (size < resultset->rows[row].fields[i].value_length)
      size= resultset->rows[row].fields[i].value_length;

  sl->strings_num= resultset->columns_num;
  for (i= 0; i < resultset->columns_num; i++)
    sl->strings[i]= g_strdup_printf("%s=%s", resultset->columns[i].name, resultset->rows[row].fields[i].value);
  return sl;
}

//----------------------------------------------------------------------------------------------------------------------

static MYX_RESULTSET *perform_detail_query(MYSQL *mysql, MYX_RESULTSET *resultset, unsigned int r,
                                           const char *query)
{
  MYX_RESULTSET *rs;
  MYX_LIB_ERROR error;
  bigint affected_rows;
  MYX_STRINGLIST *parameters= make_param_list(mysql, resultset, r);

  rs= myx_query_execute(mysql, query, 0, parameters, &error, NULL, NULL, NULL, NULL, &affected_rows);

  myx_free_stringlist(parameters);

  return rs;
}


//----------------------------------------------------------------------------------------------------------------------

static MYX_TE_COLUMN *setup_columns(MYX_RESULTSET *resultset)
{
  MYX_TE_COLUMN *columns;
  unsigned int c;

  columns= g_new0(MYX_TE_COLUMN, resultset->columns_num);
  for (c= 0; c < resultset->columns_num; c++)
  {
    columns[c].name= resultset->columns[c].name;
    switch (resultset->columns[c].column_type)
    {
    case MYX_RSCT_DECIMAL:
    case MYX_RSCT_NEWDECIMAL:
    case MYX_RSCT_INTEGER:
    case MYX_RSCT_YEAR:
    case MYX_RSCT_BIT:
      columns[c].ctype= MYX_TE_COL_INTEGER;
      break;
    case MYX_RSCT_FLOAT:
      columns[c].ctype= MYX_TE_COL_FLOAT;
      break;
    case MYX_RSCT_STRING:
    case MYX_RSCT_DATE:
    case MYX_RSCT_NEWDATE:
    case MYX_RSCT_TIME:
    case MYX_RSCT_TIMESTAMP:
    case MYX_RSCT_DATETIME:
    case MYX_RSCT_BLOB:
    case MYX_RSCT_TEXT:
    case MYX_RSCT_ENUM:
    case MYX_RSCT_SET:
      columns[c].ctype= MYX_TE_COL_STRING;
      break;
    }
  }

  return columns;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_export_resultset(MYSQL *mysql, MYX_TABLE_EXPORTER_INFO *info,
                         const char *filename,
                         const char *header_format,
                         MYX_RESULTSET *resultset,
                         const char *detail_query)
{
  unsigned int r;
  unsigned int i, c;
  char *header;
  const char **fields;
  int table_setup_done= 0, detail_setup_done= 0;
  MYX_TE_COLUMN *master_columns;
  MYX_TE_COLUMN *detail_columns= NULL;
  
  // we only support master-detail export for HTML and XML
  if (strcmp(info->te->name, "HTML")!=0 && strcmp(info->te->name, "XML")!=0)
    detail_query= NULL;

  if ((detail_query) && (detail_query[0]==0))
    detail_query= NULL;
  

  for (i= 0; i < info->option_values_num; i++)
    info->option_values[i]= replace_variables(info->option_values[i],
                                              resultset);


  if ((*info->te->setup)(info, filename) < 0)
    return -1;
  
  header= replace_variables(g_strdup(header_format), resultset);
  (*info->te->begin)(info, header);
  g_free(header);

  master_columns= setup_columns(resultset);
  
  // setup master/main table
  // if there is a detail query, then it will be initialized when the 1st
  // row is output
  (*info->te->table_setup)(info, resultset->columns_num_to_display, master_columns);

  fields= g_new(const char *, resultset->columns_num_to_display);
  for (r= 0; r < resultset->rows_num; r++)
  {
    unsigned int j;
    MYX_RS_ACTION *action;
    int skip= 0;

    // prepare columns
    for (c= 0; c < resultset->columns_num_to_display; c++)
      fields[c]= resultset->rows[r].fields[c].value;

    if (resultset->actions)
    {
      for (j= 0; j < resultset->actions->actions_num; j++)
      {
        if (resultset->actions->actions[j].row == r)
        {
          action= resultset->actions->actions+j;
          if (action->action == MYX_RSA_DELETE)
          {
            skip= 1;
            break;
          }
          // apply changes to this row
          if (action->action == MYX_RSA_UPDATE)
          {
            for (c= 0; c < resultset->columns_num_to_display; c++)
            {
              if (resultset->columns+c == action->column)
              {
                fields[c]= action->new_value;
                break;
              }
            }
          }
        }
      }
    }

    if (!skip)     // check if this row was deleted
    {
      MYX_RESULTSET *detail_resultset= NULL;

      if (detail_query && (detail_setup_done || !table_setup_done))
      {
        detail_resultset= perform_detail_query(mysql, resultset, r, detail_query);

        if (!detail_resultset)
          g_warning(("Error performing query during export of resultset with detail."));
      }

      if (!table_setup_done)
      {
        if (detail_resultset)
        {
          detail_columns= setup_columns(detail_resultset);
        
          (*info->te->table_setup)(info, detail_resultset->columns_num_to_display, detail_columns);
          detail_setup_done= 1;
        }

        (*info->te->table_header)(info);
        
        table_setup_done= 1;
      }
      // output row columns
      (*info->te->columns)(info, fields);

      // output detail for the row
      if (detail_setup_done && detail_resultset)
      {
        export_detail(info, detail_resultset);

        myx_query_free_resultset(detail_resultset);
      }
    }
  }

  if (resultset->actions)
  {
    int has_more_rows= 0;
    r= resultset->rows_num;
    // output remaining rows that were newly added
    do
    {
      int nothing_in_this_row= 1;
      unsigned int j;

      for (j= 0; j < resultset->columns_num_to_display; j++)
        fields[j]= NULL;

      for (j= 0; j < resultset->actions->actions_num; j++)
      {
        MYX_RS_ACTION *action= resultset->actions->actions+j;
        
        if (action->action == MYX_RSA_ADD && action->row == r)
        {
          for (c= 0; c < resultset->columns_num_to_display; c++)
          {
            if (resultset->columns+c == action->column)
            {
              fields[c]= action->new_value;
              nothing_in_this_row= 0;
              break;
            }
          }
        }
        else if (action->action == MYX_RSA_ADD)
        {
          if (action->row > r)
            has_more_rows= 1;
        }
      }
      if (!nothing_in_this_row)
      {
        // output columns
        (*info->te->columns)(info, fields);
      }
      r++;
    } while (has_more_rows);
  }

  (*info->te->table_footer)(info);

  g_free((char **)fields);
  g_free(master_columns);
  g_free(detail_columns);

  (*info->te->end)(info);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given string to an int, provided it is in one of the supported binary (bit) notations, which are:
 * - simple integer number
 * - b'010101'
 * - 0x12A94
 * Note: binary values cannot be negative so a negative result indicates an error.
 *
 * @param value The value as string that must be converted to an integer.
 *
 * @return The converted value or -1 if it could not be converted.
 */
ubigint myx_bit_to_int(const char* value)
{
  bigint result= -1;

  if (value != NULL && *value != '\0')
  {
    switch (*value)
    {
      case 'b':
      case 'B':
        {
          // Binary notation
          value++;
          if (*value == '\'')
          {
            value++;
            result= 0;
            while (*value == '0' || *value == '1')
            {
              result *= 2;
              result += *value++ - '0';
            };

            // The string must end with the same quote char and must not have any trailing entries.
            if (*value != '\'')
              result= -1;
            else
            {
              value++;
              if (*value != '\0')
                result= -1;
            };
          };
          break;
        };
      case '0':
        {
          // Hex value, must be followed by 'x' otherwise it's an integer.
          if (value[1] == 'x')
          {
            value += 2;
            result= 0;
            while ((*value >= '0' && *value <= '9') || (*value >= 'a' && *value <= 'f') || (*value >= 'A' && *value <= 'F'))
            {
              int temp= *value++ - '0';
              if (temp > 9)
              {
                // Either A-F or a-f
                temp -= 7;
                if (temp > 15)
                  temp -= 32; // a-f
              };

              result = (result << 4) + temp;
            };

            // No trailing characters allowed. String must end here.
            if (*value != '\0')
              result= -1;
              
            break;
          };

          // else fall through to integer handling.
        };
      default:
        result= atoi(value);
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given integer into a bit sequence usable for bit columns.
 *
 * @param value The integer value to convert.
 *
 * @result A string in the form b'10101010' representing the given value. The caller is responsible to free the
 *         returned string using g_free.
 */
char* myx_int_to_bit(ubigint value)
{
  char* result= NULL;
  char* conversion= baseconv(value, 2);

  if (conversion != NULL)
  {
    result= g_strdup_printf("b'%s'", conversion);
    g_free(conversion);
  };  

  return result;
}

//----------------------------------------------------------------------------------------------------------------------


