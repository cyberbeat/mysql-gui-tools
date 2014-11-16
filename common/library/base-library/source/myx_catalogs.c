/* Copyright (C) 2003 MySQL AB

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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <glib.h>

#include "myx_library.h"

/*
 * public functions definitions
 */

/*
 * private functions definitions
 */
MYX_SCHEMA_VIEW_STATUS * myx_get_schema_view_status(MYSQL *mysql, 
                                                    const char *catalog_name, 
                                                    const char *schema_name);

///////////////////////////////////////////////////////////////////////////////
/** @brief load structure of catalogues
    @param mysql connection handler
    @return loaded catalogues

    function doesn't load 
    MYX_SCHEMA::schema_tables and MYX_SCHEMA::schema_indices
*//////////////////////////////////////////////////////////////////////////////
MYX_CATALOGS * myx_get_catalogs(MYSQL *mysql)
{
  MYX_CATALOGS * catalogs= g_malloc(sizeof(MYX_CATALOGS));
  MYX_CATALOG * catalog;
  MYSQL_RES *res;
  MYSQL_ROW row;
  MYX_SCHEMA *schema;

  res= mysql_list_dbs(mysql, (char*) NULL);
  if (res == NULL)
  {
    g_free(catalogs);
    return NULL;
  }

  catalogs->catalogs_num= 1;
  catalog= catalogs->catalogs=
                        g_malloc(sizeof(MYX_CATALOG)*catalogs->catalogs_num);
  catalog->catalog_name= "def";

  catalog->schemata_num= (unsigned int)mysql_num_rows(res);
  schema= catalog->schemata=
               g_malloc(sizeof(MYX_SCHEMA)*catalogs->catalogs->schemata_num);

  do
  {
    row= mysql_fetch_row(res);
    if (row == NULL)
    break;

    schema->schema_tables= NULL;
    schema->schema_indices= NULL;
    schema->schema_sps= NULL;
    schema->schema_name= myx_convert_dbstr_utf8(mysql, row[0], -1);
    schema->escaped_schema_name= escape_string_for_search(schema->schema_name);
    schema->catalog_name= catalog->catalog_name;
    schema++;
    }
  while (1);

  mysql_free_result(res);
  return catalogs;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief set current defualt database for the connection and store old one

    @param mysql mysql      connection handler
    @param schema_name      name of database to set current
    @param old_schema_name  buffer to store name of old current database

    @return If successful, use_schema_store_old_one returns 1.
            Otherwise, it returns 0.
*//////////////////////////////////////////////////////////////////////////////
int use_schema_store_old_one(MYSQL * mysql, 
                             const char * schema_name, char ** old_schema_name)
{
  if (!mysql->db)
  {
    *old_schema_name= NULL;
  }
  else
  {
    *old_schema_name= g_malloc((gulong)strlen(mysql->db)+1);
    strcpy(*old_schema_name, mysql->db);
  }

  if (!myx_use_schema(mysql, schema_name))
    return 1;

  g_free(*old_schema_name);
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief restore an old current schema name (if it was stored) and 
           free it's buffer
    @param mysql            mysql connection handler
    @param old_schema_name  name of old current database
*//////////////////////////////////////////////////////////////////////////////
void restore_old_schema(MYSQL * mysql, char * old_schema_name)
{
  if (old_schema_name)
  {
    mysql_select_db(mysql, old_schema_name);
    g_free(old_schema_name);
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief load tables for schema (database)

    @param mysql        mysql connection handler
    @param catalog_name name of catalog to load
    @param schema_name  name of schema to load

    @return loaded tables
*//////////////////////////////////////////////////////////////////////////////
MYX_SCHEMA_TABLES * myx_get_schema_tables(MYSQL * mysql,
                                          const char * catalog_name,
                                          const char * schema_name)
{
  MYX_SCHEMA_TABLES *schema_tables= g_malloc(sizeof(MYX_SCHEMA_TABLES));
  MYSQL_RES *tables_res, *columns_res;
  MYSQL_ROW tbl_row, clm_row;
  char *sqlcmd;
  char *old_db;

  schema_tables->schema_tables_num= 0;
  schema_tables->schema_tables= NULL;

  if (!use_schema_store_old_one(mysql, schema_name, &old_db))
  {
    g_free(schema_tables);
    return NULL;
  }

  tables_res= NULL;

  // Try first if we are working on a new server.
  if (mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 3))
  {
	if (myx_mysql_query(mysql, "SHOW FULL TABLES") == 0)
	  tables_res= mysql_store_result(mysql);
  };
  
  // if there is no table list yet try the old way.
  if (tables_res == NULL)
	tables_res= mysql_list_tables(mysql, (char*) NULL);

  if (tables_res == NULL)
  {
    g_free(schema_tables);
    restore_old_schema(mysql, old_db);
    return NULL;
  }
  else
  {
    MYX_SCHEMA_TABLE * table;
    schema_tables->schema_tables_num= (unsigned int)mysql_num_rows(tables_res);
    table= schema_tables->schema_tables=
                g_malloc0(sizeof(MYX_SCHEMA)*schema_tables->schema_tables_num);
  do
  {
    tbl_row= mysql_fetch_row(tables_res);
    if (tbl_row == NULL)
    break;

    table->table_name= myx_convert_dbstr_utf8(mysql, tbl_row[0], -1);
      if (mysql_num_fields(tables_res)>1)
        table->table_type= (strcmp2(tbl_row[1], "VIEW")==0) ? MSTT_VIEW : MSTT_BASE_TABLE;
      else
        table->table_type= MSTT_BASE_TABLE;

    columns_res= NULL;
    sqlcmd= g_strdup_printf("SHOW COLUMNS FROM `%s`", table->table_name);
    if (myx_mysql_query(mysql, sqlcmd) == 0)
      columns_res= mysql_store_result(mysql);
    if (columns_res == NULL)
    {
      MYX_SCHEMA_TABLE_COLUMN * column;

      table->columns_num= 1;
      column= table->columns= g_malloc0(sizeof(MYX_SCHEMA_TABLE_COLUMN)*1);

      column->column_name= g_strdup("Could not fetch columns");
      column->column_type= g_strdup("");
      column->not_null= 0;
      column->primary_key= 0;
      column->default_value= g_strdup("");
      column->extra= g_strdup("");
    }
    else
    {
      MYX_SCHEMA_TABLE_COLUMN * column;
      table->columns_num= (unsigned int)mysql_num_rows(columns_res);
      column= table->columns= g_malloc0(sizeof(MYX_SCHEMA_TABLE_COLUMN) * table->columns_num);

      do
      {
        clm_row= mysql_fetch_row(columns_res);
        if (clm_row == NULL)
          break;

        column->column_name=   myx_convert_dbstr_utf8(mysql, clm_row[0], -1);
        column->column_type=   g_strdup(clm_row[1]);
        column->not_null=      strcmp2(clm_row[2],"YES") ? 1: 0;
        column->primary_key=   strcmp2(clm_row[3], "PRI") ? 0 : 1;
        column->default_value= myx_convert_dbstr_utf8(mysql, clm_row[4], -1);
        column->extra=         g_strdup(clm_row[5]);
        column++;
      }
      while (1);

      mysql_free_result(columns_res);
    }
    g_free(sqlcmd);
    table++;
	}
	while (1);

	mysql_free_result(tables_res);
  }

  restore_old_schema(mysql, old_db);
  return schema_tables;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief create new MYX_TABLE_INDEX_COLUMN[] and copy all indeces to it

    @param src_columns  indeces to copy
    @param count        number of indexes to copy

    @return copyed indeces

    Null-string is copyed to empty string ("")
*//////////////////////////////////////////////////////////////////////////////
MYX_TABLE_INDEX_COLUMN *copy_index_columns(MYX_TABLE_INDEX_COLUMN *src_columns,
                                           unsigned int count)
{
  MYX_TABLE_INDEX_COLUMN * columns=
                                g_malloc(sizeof(MYX_TABLE_INDEX_COLUMN)*count);
  MYX_TABLE_INDEX_COLUMN * column= columns;
  MYX_TABLE_INDEX_COLUMN * columns_end= columns + count;
  memcpy(columns,src_columns,sizeof(MYX_TABLE_INDEX_COLUMN)*count);
  for (; column!=columns_end; column++)
  {
    if (!column->column_name) column->column_name= g_strdup("");
    if (!column->seq_in_index) column->seq_in_index= g_strdup("");
    if (!column->collation) column->collation= g_strdup("");
  }
  return columns;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief find all necessary fields by names in array of MYSQL_FIELD

    @param column_name      array of searched column names
    @param column_name_end  end of array of searched column names
    @param res_fields       array of fields gotten by mysql_fetch_fields
    @param res_fields_end   end of array of fields gotten by mysql_fetch_fields
    @param i_field          array of indexes of fields with required name
*//////////////////////////////////////////////////////////////////////////////
void build_field_subst(const char ** column_name, 
                       const char ** column_name_end, MYSQL_FIELD * res_fields,
                       MYSQL_FIELD * res_fields_end, int * i_field)
{
  for (; column_name != column_name_end; column_name++, i_field++)
  {
    int i;
    MYSQL_FIELD * res_field= res_fields;
    for (i= 0; res_field != res_fields_end; res_field++, i++)
    {
      if (strcmp(res_field->name, *column_name)==0)
      {
        *i_field= i;
        break;
      };
    }
    if (res_field == res_fields_end)
      *i_field= -1;
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief names of columns for result of query 
                                               "SHOW INDEX FROM `<table_name>`"
*//////////////////////////////////////////////////////////////////////////////
static const char * index_columns_names[]=
{
  "Non_unique",
  "Key_name",
  "Null",
  "Index_type",
  "Column_name",
  "Seq_in_index",
  "Collation"
};
static const char ** columns_names_end=
               index_columns_names + sizeof(index_columns_names)/sizeof(char*);

///////////////////////////////////////////////////////////////////////////////
/** @brief load indices from the result of 
                            mysql_query(mysql,"SHOW INDEX FROM `<table_name>`")

    @param mysql      mysql connection handler
    @param table_name name of table to get indeces of
    @param res        result of 
                            mysql_query(mysql,"SHOW INDEX FROM `<table_name>`")
    @param index_num  pointer to the returned number of the gotten indices

    @return array of gotten indices allocated via g_malloc0
*//////////////////////////////////////////////////////////////////////////////
MYX_TABLE_INDEX *get_indices_from_res(MYSQL *mysql, char *table_name, 
                                      MYSQL_RES * res, unsigned int *index_num)
{
  unsigned int num_allocated_indices= 5;
  MYX_TABLE_INDEX *indices=
                      g_malloc0(sizeof(MYX_TABLE_INDEX)*num_allocated_indices);
  MYX_TABLE_INDEX *index= NULL;
  unsigned int num_filled_indices= 0;
  MYSQL_ROW row;
  MYSQL_FIELD *fields= mysql_fetch_fields(res);
  unsigned int num_fields= mysql_num_fields(res);
  char *current_index_name= NULL;
  MYX_TABLE_INDEX_COLUMN tmp_columns[50];
  MYX_TABLE_INDEX_COLUMN * cur_column= tmp_columns;
  int fi[sizeof(index_columns_names)/sizeof(char*)];

  build_field_subst(index_columns_names,columns_names_end,
                    fields,fields+num_fields,fi);

#define XSTRDUP(s) g_strdup(s?s:"")
  do
  {
	row= mysql_fetch_row(res);
	if (row == NULL)
	  break;

	if (strcmp2(current_index_name, row[fi[1]])!=0) // new index start
    {
      if (current_index_name)
      {
        index->index_columns_num= (unsigned int)(cur_column - tmp_columns);
        index->index_columns=
                      copy_index_columns(tmp_columns,index->index_columns_num);
        cur_column= tmp_columns;
      }

      //Check if still enough mem allocated
      if (num_filled_indices >= num_allocated_indices)
      {
        num_allocated_indices+= 5;
        indices= g_realloc(indices,
                           sizeof(MYX_TABLE_INDEX)*num_allocated_indices);
      }
      index= indices + num_filled_indices;
      index->index_columns= NULL;

      index->table_name= g_strdup(table_name);
      index->unique= fi[0]==-1 ? 0 : (atoi(row[fi[0]]) + 1) % 2;
      index->key_name=
                     fi[1]==-1 ? 0 : myx_convert_dbstr_utf8(mysql, row[fi[1]], -1);
      index->not_null= fi[2]==-1 ? 0 :!(strcmp2(row[fi[2]],"YES")==0);
      index->index_type= fi[3]==-1 ? 0 : XSTRDUP(row[fi[3]]);
      num_filled_indices++;
      current_index_name= index->key_name;
    }
    cur_column->column_name=
                     fi[4]==-1 ? 0 : myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);
    cur_column->seq_in_index= fi[5]==-1 ? 0 : XSTRDUP(row[fi[5]]);
    cur_column->collation= fi[6]==-1 ? 0 : XSTRDUP(row[fi[6]]);
    cur_column++;
  }
  while (1);

  mysql_free_result(res);
#undef XSTRDUP

  if (num_filled_indices > 0)
  {
    index->index_columns_num= (unsigned int)(cur_column - tmp_columns);
    index->index_columns=
                      copy_index_columns(tmp_columns,index->index_columns_num);
  }
  *index_num= num_filled_indices;

  return indices;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief load indices for schema (database)

    @param mysql        mysql connection handler
    @param catalog_name name of catalog to load
    @param schema_name  name of schema to load

    @return loaded indices
*//////////////////////////////////////////////////////////////////////////////
MYX_SCHEMA_INDICES * myx_get_schema_indices(MYSQL *mysql,
                                            const char *catalog_name,
                                            const char *schema_name)
{
  MYX_SCHEMA_INDICES *schema_indices= g_malloc0(sizeof(MYX_SCHEMA_INDICES));
  MYX_TABLE_INDEX *table_indices;
  MYSQL_RES *res_tbl, *res_ind;
  MYSQL_ROW row;
  char *table_name;
  char *old_db;
  char *sqlcmd;
  unsigned int num_allocated= 5, table_indices_num;

  if (!use_schema_store_old_one(mysql, schema_name,&old_db))
  {
    myx_free_schema_indices(schema_indices);
    return NULL;
  }

  schema_indices->indices= g_malloc0(sizeof(MYX_TABLE_INDEX)*num_allocated);

  res_tbl= mysql_list_tables(mysql, (char*) NULL);
  if (res_tbl == NULL)
  {
    myx_free_schema_indices(schema_indices);
    restore_old_schema(mysql, old_db);
    return NULL;
  }
  else
  {
  do
  {
    row= mysql_fetch_row(res_tbl);
    if (row == NULL)
    break;

    res_ind= NULL;
    table_name= myx_convert_dbstr_utf8(mysql, row[0], -1);
    sqlcmd= g_strdup_printf("SHOW INDEX FROM `%s`", table_name);
    if (myx_mysql_query(mysql, sqlcmd) == 0)
      res_ind= mysql_store_result(mysql);
    if (res_ind == NULL)
    {
        myx_free_schema_indices(schema_indices);
        g_free(table_name);
        g_free(sqlcmd);
        restore_old_schema(mysql, old_db);
        return NULL;
      }
      else
      {
        table_indices= get_indices_from_res(mysql, table_name, res_ind,
                                            &table_indices_num);
        if (schema_indices->indices_num+table_indices_num>=num_allocated)
        {
          num_allocated+= table_indices_num+5;
          schema_indices->indices= 
              g_realloc(schema_indices->indices,
                        sizeof(MYX_TABLE_INDEX)*num_allocated);
        }
        memcpy(schema_indices->indices+schema_indices->indices_num,
               table_indices, sizeof(MYX_TABLE_INDEX)*table_indices_num);
        g_free(table_indices);
        schema_indices->indices_num+= table_indices_num;
      }
      g_free(table_name);
      g_free(sqlcmd);
	}
	while (1);

	mysql_free_result(res_tbl);
  }

  restore_old_schema(mysql, old_db);
  return schema_indices;
}

static const char * table_status_names[]=
{
  "Name",            // 0
  "Type",            // 1
  "Row_format",      // 2
  "Rows",            // 3
  "Avg_row_length",  // 4
  "Data_length",     // 5
  "Max_data_length", // 6
  "Index_length",    // 7
  "Data_free",       // 8
  "Auto_increment",  // 9
  "Create_time",     // 10
  "Update_time",     // 11
  "Check_time",      // 12
  "Create_options",  // 13
  "Comment",         // 14
  "Engine",          // 15
};
static const char ** table_status_names_end=
                 table_status_names + sizeof(table_status_names)/sizeof(char*);

static const char * describe_table_names[]=
{
  "Field",     // 0
  "Type",      // 1
  "Null",      // 2
  "Key",       // 3
  "Default",   // 4
  "Extra",     // 5
};
static const char ** describe_table_names_end=
             describe_table_names + sizeof(describe_table_names)/sizeof(char*);

/*
static void copy_table_status(MYX_TABLE_STATUS *dst, const MYX_TABLE_STATUS *src)
{
  dst->table_name= g_strdup(src->table_name);
  dst->table_type= g_strdup(src->table_type);
  dst->row_format= g_strdup(src->row_format);
  dst->rows= g_strdup(src->rows);
  dst->avg_row_length= g_strdup(src->avg_row_length);
  dst->data_length= g_strdup(src->data_length);
  dst->max_data_length= g_strdup(src->max_data_length);
  dst->index_length= g_strdup(src->index_length);
  dst->data_free= g_strdup(src->data_free);
  dst->auto_increment= g_strdup(src->auto_increment);
  dst->create_time= g_strdup(src->create_time);
  dst->update_time= g_strdup(src->update_time);
  dst->check_time= g_strdup(src->check_time);
  dst->create_options= g_strdup(src->create_options);
  dst->comment= g_strdup(src->comment);

  dst->indexes_num= src->indexes_num;
  dst->columns_num= src->columns_num;
  
  MYX_TABLE_INDEX *indexes;

  
  MYX_SCHEMA_TABLE_COLUMN *columns;


}
*/

///////////////////////////////////////////////////////////////////////////////
/** @brief load status info for schema entities (currently they are tables, SPs, views)

    @param mysql        mysql connection handler
    @param catalog_name name of catalog to load
    @param schema_name  name of schema to load

    @return loaded entity status info
*//////////////////////////////////////////////////////////////////////////////

MYX_SCHEMA_ENTITY_STATUS * myx_get_schema_entity_status(MYSQL *mysql,
                                                       const char *catalog_name,
                                                       const char *schema_name)
{
  unsigned i, j;
  MYX_SCHEMA_TABLE_STATUS *tables;
  MYX_SCHEMA_VIEW_STATUS *views;
  MYX_SCHEMA_STORED_PROCEDURES *sps;
  MYX_SCHEMA_ENTITY_STATUS *entities= g_malloc0(sizeof(MYX_SCHEMA_ENTITY_STATUS));

  tables= myx_get_schema_table_status(mysql, catalog_name, schema_name);
  if (!tables)
    return NULL;
  views= myx_get_schema_view_status(mysql, catalog_name, schema_name);
  if (!views)
  {
    myx_free_schema_table_status(tables);
    return NULL;
  }
  sps= myx_get_schema_sps(mysql, catalog_name, schema_name);
  if (!sps)
  {
    myx_free_schema_table_status(tables);
    myx_free_schema_view_status(views);
    return NULL;
  }
  
  entities->schema_entities_num= tables->schema_tables_num + views->schema_views_num + sps->schema_sps_num;

  entities->schema_entities= g_malloc0(sizeof(MYX_ENTITY_STATUS)*entities->schema_entities_num);

  for(j= 0, i= 0; j < tables->schema_tables_num; j++, i++)
  {
    entities->schema_entities[i].entity_type= MYX_ENTITY_TABLE;
    entities->schema_entities[i].entity= tables->schema_tables + j;
  }

  for(j= 0; j < views->schema_views_num; j++, i++)
  {
    entities->schema_entities[i].entity_type= MYX_ENTITY_VIEW;
    entities->schema_entities[i].entity= views->schema_views + j;
  }

  for(j= 0; j < sps->schema_sps_num; j++, i++)
  {
    if(sps->schema_sps[j].sp_type == MSPT_PROCEDURE)
    {
      entities->schema_entities[i].entity_type= MYX_ENTITY_PROC;
    }
    else
    {
      entities->schema_entities[i].entity_type= MYX_ENTITY_FUNC;
    }
    entities->schema_entities[i].entity= sps->schema_sps + j;
  }

  g_free(sps);
  g_free(tables);
  g_free(views);

  return entities;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @brief load view status info for schema
 *
 * @param mysql        mysql connection handler
 * @param catalog_name name of catalog to load
 * @param schema_name  name of schema to load
 *
 * @return loaded table statuses
 *
 * @note covered by unit tests
 */
MYX_SCHEMA_VIEW_STATUS * myx_get_schema_view_status(MYSQL *mysql, const char *catalog_name, const char *schema_name)
{
  MYX_SCHEMA_VIEW_STATUS *views= g_malloc0(sizeof(MYX_SCHEMA_VIEW_STATUS));
  MYX_VIEW_STATUS *view;

  MYSQL_RES *res_tbl;
  MYSQL_ROW row_tbl;
  MYSQL_FIELD *fields_tbl;
  unsigned int num_fields_tbl;
  unsigned int view_count= 0;
  char *old_db;

  if (!use_schema_store_old_one(mysql, schema_name,&old_db))
  {
    g_free(views);
    return NULL;
  };

  res_tbl= NULL;
  if (myx_mysql_query(mysql, "SHOW TABLE STATUS") == 0)
    res_tbl= mysql_store_result(mysql);
  if (res_tbl == NULL)
  {
    myx_free_schema_view_status(views);
    restore_old_schema(mysql, old_db);
    return NULL;
  }
  else
  {
    int fi[16];
    views->schema_views_num= (unsigned int)mysql_num_rows(res_tbl);
    view= views->schema_views= g_malloc0(sizeof(MYX_VIEW_STATUS)*views->schema_views_num);

    // Depending on the version of the server there might be different columns.
    num_fields_tbl= mysql_num_fields(res_tbl);
    fields_tbl= mysql_fetch_fields(res_tbl);
    build_field_subst(table_status_names,table_status_names_end, fields_tbl,fields_tbl+num_fields_tbl,fi);

    do
    {
      row_tbl= mysql_fetch_row(res_tbl);
      if (row_tbl == NULL)
        break;

#define XSTRDUP(s) g_strdup(s?s:"")
      // check whether it is a view
      if (((fi[1] != -1) && (row_tbl[fi[1]] != NULL)) || ((fi[15] != -1) && (row_tbl[fi[15]] != NULL)))
        continue;

      // Views are marked with the VIEW keyword or an error beginning with the VIEW keyword in the comment field,
      // otherwise it might be a defect table.
      if ((fi[14] == -1) || (row_tbl[fi[14]] == NULL) ||
        (strcasecmp(row_tbl[fi[14]], "view") != 0 && strncasecmp(row_tbl[fi[14]], "view ", 5) != 0))
        continue;

      view_count++;
      view->view_name= fi[0]==-1  ? 0 : myx_convert_dbstr_utf8(mysql, row_tbl[fi[0]], -1);
      view->comment=   fi[14]==-1 ? 0 : XSTRDUP(row_tbl[fi[14]]);

      // If there is more than "VIEW" in the comment then we very likely have an error in the view. Mark this for UI.
      if (strlen(row_tbl[fi[14]]) > 4)
        view->invalid= 1;

#undef XSTRDUP
      view++;
    }
    while (1);

    mysql_free_result(res_tbl);

    views->schema_views_num= view_count;
    if (views->schema_views_num==0)
    {
      g_free(views->schema_views);
      views->schema_views= NULL;
    }
    else
    {
      // Re-allocate with real size.
      views->schema_views= g_realloc(views->schema_views, sizeof(MYX_VIEW_STATUS)*views->schema_views_num);
    };
  };

  restore_old_schema(mysql, old_db);
  return views;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Load table statuses for schema (database).
 *
 * @param mysql        mysql connection handler
 * @param catalog_name name of catalog to load
 * @param schema_name  name of schema to load
 *
 * @return loaded table statuses
 *
 * @note covered by unit tests.
*/
MYX_SCHEMA_TABLE_STATUS * myx_get_schema_table_status(MYSQL *mysql, const char *catalog_name, const char *schema_name)
{
  MYX_SCHEMA_TABLE_STATUS *tables= g_malloc0(sizeof(MYX_SCHEMA_TABLE_STATUS));
  MYX_TABLE_STATUS *table;
  MYX_SCHEMA_TABLE_COLUMN *column;

  MYSQL_RES *res_tbl, *res;
  MYSQL_ROW row_tbl, row;
  MYSQL_FIELD *fields_tbl, *fields;
  unsigned int num_fields_tbl, num_fields;
  unsigned int table_count= 0;
  char *sqlcmd;
  char *old_db;

  if (!use_schema_store_old_one(mysql, schema_name,&old_db))
  {
    g_free(tables);
    return NULL;
  }

  res_tbl= NULL;
  if (myx_mysql_query(mysql, "SHOW TABLE STATUS") == 0)
    res_tbl= mysql_store_result(mysql);
  if (res_tbl == NULL)
  {
    myx_free_schema_table_status(tables);
    restore_old_schema(mysql, old_db);
    return NULL;
  }
  else
  {
    int fi[16];
    tables->schema_tables_num= (unsigned int)mysql_num_rows(res_tbl);
    table= tables->schema_tables= g_malloc0(sizeof(MYX_TABLE_STATUS) * tables->schema_tables_num);

    // Depending on the version of the server there might be different columns
    num_fields_tbl= mysql_num_fields(res_tbl);
    fields_tbl= mysql_fetch_fields(res_tbl);
    build_field_subst(table_status_names,table_status_names_end, fields_tbl,fields_tbl+num_fields_tbl,fi);

    do
    {
      char* engine;
      int skip_details; // Set when an error for a table occured.
      
      row_tbl= mysql_fetch_row(res_tbl);
      if (row_tbl == NULL)
        break;

#define XSTRDUP(s) g_strdup(s?s:"")
      // Check table_type/Engine first.
      engine= (fi[1] == -1) ? (fi[15]==-1 ? 0 : row_tbl[fi[15]]) : row_tbl[fi[1]];

      // Engine == NULL means either this is a view or an error occured.
      skip_details= 0;
      if (engine != NULL)
        table->table_type= XSTRDUP(engine);
      else
        // Check if there is a comment field. If not then something is wrong.
        if (fi[14] == -1)
        {
          table->table_type= g_strdup("Error while retrieving table info. Repair the table to fix this problem.");
          skip_details= 1;
        }
        else
          if (strcasecmp(row_tbl[fi[14]], "view") == 0 || strncasecmp(row_tbl[fi[14]], "view ", 5) == 0)
            continue;
          else
          {
            // If there is a comment then it contains error info.
            table->table_type= table->table_type= g_strdup(row_tbl[fi[14]]);
            skip_details= 1;
          };

      table_count++;
      table->table_name= fi[0]==-1 ? NULL : myx_convert_dbstr_utf8(mysql, row_tbl[fi[0]], -1);
      table->row_format=       fi[2]==-1  ? NULL : XSTRDUP(row_tbl[fi[2]]);
      table->rows=             fi[3]==-1  ? NULL : XSTRDUP(row_tbl[fi[3]]);
      table->avg_row_length=   fi[4]==-1  ? NULL : XSTRDUP(row_tbl[fi[4]]);
      table->data_length=      fi[5]==-1  ? NULL : XSTRDUP(row_tbl[fi[5]]);
      table->max_data_length=  fi[6]==-1  ? NULL : XSTRDUP(row_tbl[fi[6]]);
      table->index_length=     fi[7]==-1  ? NULL : XSTRDUP(row_tbl[fi[7]]);
      table->data_free=        fi[8]==-1  ? NULL : XSTRDUP(row_tbl[fi[8]]);
      table->auto_increment=   fi[9]==-1  ? NULL : XSTRDUP(row_tbl[fi[9]]);
      table->create_time=      fi[10]==-1 ? NULL : XSTRDUP(row_tbl[fi[10]]);
      table->update_time=      fi[11]==-1 ? NULL : XSTRDUP(row_tbl[fi[11]]);
      table->check_time=       fi[12]==-1 ? NULL : XSTRDUP(row_tbl[fi[12]]);
      table->create_options=   fi[13]==-1 ? NULL : XSTRDUP(row_tbl[fi[13]]);
      table->comment=          fi[14]==-1 ? NULL : XSTRDUP(row_tbl[fi[14]]);
      table->invalid= 0;

      if (skip_details)
        table->invalid= 1;
      else
      {
        if ((table->table_type) && (table->table_type[0]))
        {
          sqlcmd= g_strdup_printf("SHOW INDEX FROM `%s`", table->table_name);
          res= NULL;

          if (myx_mysql_query(mysql, sqlcmd) == 0)
            res= mysql_store_result(mysql);
          if (res != NULL)
            table->indexes= get_indices_from_res(mysql, table->table_name, res, &table->indexes_num);
          else
          {
            g_free(sqlcmd);
            myx_free_schema_table_status(tables);
            restore_old_schema(mysql, old_db);
            mysql_free_result(res_tbl);
            return NULL;
          }
          g_free(sqlcmd);
        };

        table->columns_num= 0;
        table->columns= NULL;

        res= NULL;
        sqlcmd= g_strdup_printf("DESCRIBE `%s`", table->table_name);
        if (myx_mysql_query(mysql, sqlcmd) == 0)
          res= mysql_store_result(mysql);
        if (res == NULL)
        {
          g_free(sqlcmd);
          myx_free_schema_table_status(tables);
          mysql_free_result(res_tbl);
          restore_old_schema(mysql, old_db);
          return NULL;
        }
        else
        {
          int fi[6];

          // Depending on the version of the server there might be different columns.
          num_fields= mysql_num_fields(res);
          fields= mysql_fetch_fields(res);

          build_field_subst(describe_table_names,describe_table_names_end,
                            fields,fields+num_fields,fi);

          table->columns_num= (unsigned int)mysql_num_rows(res);
          column= table->columns=
            g_malloc0(sizeof(MYX_SCHEMA_TABLE_COLUMN)*table->columns_num);

          do
          {
            row= mysql_fetch_row(res);
            if (row == NULL)
            break;

            column->column_name=   fi[0] == -1 ? NULL : myx_convert_dbstr_utf8(mysql, row[fi[0]], -1);
            column->column_type=   fi[1] == -1 ? NULL : XSTRDUP(row[fi[1]]);
            column->not_null=      fi[2] == -1 ? NULL : !(strcmp2(row[fi[2]],"YES")==0);
            column->primary_key=   fi[3] == -1 ? NULL : strcmp2(row[fi[3]],"PRI")==0;
            column->default_value= fi[4] == -1 ? NULL : myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);
            column->extra=         fi[5] == -1 ? NULL : myx_convert_dbstr_utf8(mysql, row[fi[5]], -1);
            column++;
          }
          while (1);

          mysql_free_result(res);
        }
        g_free(sqlcmd);
      };  
#undef XSTRDUP
      table++;
    }
    while (1);

    mysql_free_result(res_tbl);

    tables->schema_tables_num= table_count;
    if (tables->schema_tables_num == 0)
    {
      g_free(tables->schema_tables);
      tables->schema_tables= NULL;
    }
    else
    {
      // Re-allocate with real size.
      tables->schema_tables= g_realloc(tables->schema_tables, sizeof(MYX_TABLE_STATUS) * tables->schema_tables_num);
    };
  };

  restore_old_schema(mysql, old_db);
  return tables;
}

//----------------------------------------------------------------------------------------------------------------------

/** @brief extracts only the header of a SP code

    @param sp_code      code of SP

    @return new allocated header or NULL
*/
static char *filter_sp_header(char *sp_code)
{
  unsigned int i= 0, len= (unsigned int)strlen(sp_code);
  unsigned int b_count= 0;
  unsigned int header_start= 0;
  char *sp_header= NULL;

  if (!sp_code)
    return 0;

  while (i<len)
  {
    if (sp_code[i]=='(')
    {
      if(b_count==0)
        header_start= i;
      b_count++;
    }
    else if (sp_code[i]==')')
    {
      b_count--;

      if(b_count==0)
        break;
    }

    i++;
  }

  if (header_start>0) 
    sp_header= g_strndup(sp_code+header_start, i-header_start+1);

  g_free(sp_code);

  return sp_header;
}


static const char * sps_names[]=
{
  "Db",              // 0
  "Name",            // 1
  "Type",            // 2
  "Definer",         // 3
  "Modified",        // 4
  "Created",         // 5
  "Security_type",   // 6
  "Comment",         // 7
};
static const char ** sps_names_end=
                 sps_names + sizeof(sps_names)/sizeof(char*);

static const char * sp_show_create[]=
{
  "Procedure",       // 0 (col 1)
  "Function",        // 1 (col 1)
  "sql_mode",        // 2 (col 2)
  "Create Procedure",// 3 (col 3)
  "Create Function", // 4 (col 3)
};
static const char ** sp_show_create_end=
                 sp_show_create + sizeof(sp_show_create)/sizeof(char*);

///////////////////////////////////////////////////////////////////////////////
/** @brief load stored procedures for schema (database)

    @param mysql        mysql connection handler
    @param catalog_name name of catalog to load
    @param schema_name  name of schema to load

    @return loaded stored procedures

    @note covered by unit tests
*//////////////////////////////////////////////////////////////////////////////
MYX_SCHEMA_STORED_PROCEDURES * myx_get_schema_sps(MYSQL *mysql, const char *catalog_name, const char *schema_name)
{
  MYX_SCHEMA_STORED_PROCEDURES *sps= g_malloc0(sizeof(MYX_SCHEMA_STORED_PROCEDURES));
  MYX_SCHEMA_STORED_PROCEDURE *sp;
  MYSQL_RES *res_sps, *res;
  MYSQL_ROW row_sps, row;
  MYSQL_FIELD *fields_sps, *fields;
  unsigned int num_fields_sps, num_fields;
  unsigned int sp_num= 0;
  static const char *queries[]={
    "SHOW PROCEDURE STATUS",
    "SHOW FUNCTION STATUS"
  };
  int q;
  char *old_db;
  char *sqlcmd, *sp_code;

  pcre *pcre_exp, *pcre_exp2;
  const char *error_str;
  int erroffset, offset;

  //use the following regex to get all params of a SP
  //(IN|OUT|INOUT)?\s?([\w\d]+)\s+([\w\d\(\)\,]+)\s*(\)|\,)
  // 1: param type (IN, OUT, INOUT), IN is not existing
  // 2: param name
  // 3: param datatype
  // 4: param datatype name
  // 5: param datatype options in ()
  // 6: separator , or )

#define SP_PARAMS "(IN\\s+|OUT\\s+|INOUT\\s+)?([\\w\\d]+)\\s+([\\w\\d]+)\\s*((\\([\\s\\w\\d,]+\\))?)\\s*(\\)|\\,)"

  //use the following regex to get the returntype of a function
  //RETURNS\s+([\w\d\(\)\,\s]+)\s+(LANGUAGE|NOT|DETERMINISTIC|SQL|COMMENT|RETURN|BEGIN)
  // 1: datatype
  // 2: keyword after datatype (not important)

//#define SP_RETURN_TYPE "RETURNS\\s+([\\w\\d\\(\\)\\,\\s]+?)\\s+(LANGUAGE|NOT|DETERMINISTIC|SQL|COMMENT|RETURN|BEGIN)"

// this is accroding to the documentation for 5.0.9 server
#define SP_RETURN_TYPE "RETURNS\\s+(" \
  "(TINY|BIG|SMALL|MEDIUM)INT(\\s*\\([0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"   \
  "INT(\\s*\\([0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"       \
  "INTEGER(\\s*\\([0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"   \
  "DATE|"         \
  "TIME|"         \
  "TIMESTAMP|"    \
  "DATETIME|"     \
  "TINYBLOB|"     \
  "BLOB|"         \
  "MEDIUMBLOB|"   \
  "LONGBLOB|"     \
  "VARCHAR(\\s*\\([0-9]+\\))?(\\s+BINARY)?|"  \
  "TINYTEXT(\\s+BINARY)?|"                                      \
  "TEXT(\\s+BINARY)?|"                                          \
  "MEDIUMTEXT(\\s+BINARY)?|"                                    \
  "LONGTEXT(\\s+BINARY)?|"                                      \
  "ENUM(\\s*\\([^)]*\\))?|"                                     \
  "DOUBLE(\\s*\\([0-9]+\\,[0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"     \
  "CHAR(\\s*\\([0-9]+\\))?(\\s+BINARY)?(\\s+ASCII)?(\\s+UNICODE)?|" \
  "SET(\\s*\\([^)]*\\))?|"                                      \
  "FLOAT(\\s*\\([0-9]+\\,[0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"      \
  "DECIMAL(\\s*\\([0-9]+\\,[0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"    \
  "NUMERIC(\\s*\\([0-9]+\\,[0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"    \
  "REAL(\\s*\\([0-9]+\\,[0-9]+\\))?(\\s+UNSIGNED)?(\\s+ZEROFILL)?|"       \
  "GEOMETRY|"     \
  "POINT|"        \
  "LINESTRING|"   \
  "POLYGON|"      \
  "MULTIPOINT|"   \
  "MULTILINESTRING|"    \
  "MULTIPOLYGON|" \
  "GEOMETRYCOLLECTION"  \
  ")"

  pcre_exp= pcre_compile(SP_PARAMS, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (!pcre_exp)
  {
    g_free(sps);
    return NULL;
  }

  pcre_exp2= pcre_compile(SP_RETURN_TYPE, PCRE_CASELESS, &error_str, &erroffset, NULL);
  if (!pcre_exp)
  {
    g_free(sps);
    pcre_free(pcre_exp);
    return NULL;
  }

  if (!use_schema_store_old_one(mysql, schema_name, &old_db))
  {
    g_free(sps);
    pcre_free(pcre_exp);
    pcre_free(pcre_exp2);
    return NULL;
  }

  for (q = 0; q < 2; q++)
  {
    res_sps= NULL;
    if (myx_mysql_query(mysql, queries[q]) == 0)
      res_sps= mysql_store_result(mysql);
    if (res_sps == NULL)
    {
      restore_old_schema(mysql, old_db);
      pcre_free(pcre_exp);
      pcre_free(pcre_exp2);
      
      // version < 5.0
      sps->schema_sps_num= 0;
      sps->schema_sps= NULL;
      return sps;
    }
    else
    {
      int fi[8];
      unsigned int row_count= (unsigned int) mysql_num_rows(res_sps);
      if (row_count > 0)
      {
        sps->schema_sps_num+= row_count;
        sps->schema_sps= g_realloc(sps->schema_sps, sizeof(MYX_SCHEMA_STORED_PROCEDURE) * sps->schema_sps_num);

        // Depending on the version of the server there might be different columns.
        num_fields_sps= mysql_num_fields(res_sps);
        fields_sps= mysql_fetch_fields(res_sps);

        build_field_subst(sps_names, sps_names_end, fields_sps, fields_sps+num_fields_sps, fi);

        do
        {
          row_sps= mysql_fetch_row(res_sps);
          if (row_sps == NULL)
            break;

  #define XSTRDUP(s) g_strdup(s?s:"")
  
          // Ignore SPs from wrong DB.
          if (fi[0]==-1 ? 1 : (strcmp2(row_sps[fi[0]], schema_name)!=0))
            continue;

          sp= sps->schema_sps+sp_num;

          sp->name=        fi[1]==-1 ? 0 : myx_convert_dbstr_utf8(mysql, row_sps[fi[1]], -1);
          sp->sp_type=     fi[2]==-1 ? 0 : (
                                            (strcmp2(row_sps[fi[2]],"PROCEDURE")==0) ? MSPT_PROCEDURE : MSPT_FUNCTION);
          sp->definer=     fi[3]==-1  ? 0 : XSTRDUP(row_sps[fi[3]]);
          sp->modified=    fi[4]==-1  ? 0 : XSTRDUP(row_sps[fi[4]]);
          sp->created=     fi[5]==-1  ? 0 : XSTRDUP(row_sps[fi[5]]);
          sp->sp_security= fi[6]==-1 ? 0 : (
                                            (strcmp2(row_sps[fi[6]],"DEFINER")==0) ? MSPS_DEFINER : MSPS_INVOKER);
          sp->comment=     fi[7]==-1  ? 0 : XSTRDUP(row_sps[fi[7]]);
          sp->return_datatype= NULL;
          sp->params_num= 0;
          sp->params= NULL;

          //Get SP parameter
          if (sp->sp_type==MSPT_PROCEDURE)
            sqlcmd= g_strdup_printf("SHOW CREATE PROCEDURE `%s`.`%s`", schema_name, sp->name);
          else
            sqlcmd= g_strdup_printf("SHOW CREATE FUNCTION `%s`.`%s`", schema_name, sp->name);
          res= NULL;
          if (myx_mysql_query(mysql, sqlcmd) == 0)
            res= mysql_store_result(mysql);
          if (res == NULL)
          {
            g_free(sqlcmd);
            mysql_free_result(res_sps);
            pcre_free(pcre_exp);
            pcre_free(pcre_exp2);
            g_free(sps);
            restore_old_schema(mysql,old_db);
            return NULL;
          }
          else
          {
            int fi[5];

            //Depending on the version of the server there might
            //   be different columns
            num_fields= mysql_num_fields(res);
            fields= mysql_fetch_fields(res);
            build_field_subst(sp_show_create,sp_show_create_end, fields,fields+num_fields,fi);

            row= mysql_fetch_row(res);
            if (row != NULL)
            {
              if (fi[3]!=-1)
                sp_code= myx_convert_dbstr_utf8(mysql, row[fi[3]], -1);
              else
                sp_code= fi[4]==-1 ? 0 : myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);

              if (sp_code)
              {
                const char *param_type, *param_name, *param_datatype;
                unsigned int sp_num_allocated= 5;
                MYX_SCHEMA_STORED_PROCEDURE_PARAM *param;
                int matched[60], rc;

                // Get function return type.
                if (sp->sp_type == MSPT_FUNCTION)
                {
                  const char *return_type = NULL;
                  int matched[60], rc;

                  if((rc= pcre_exec(pcre_exp2, NULL, sp_code, (int)strlen(sp_code), 0, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
                  {
                    pcre_get_substring(sp_code, matched, rc, 1, &return_type);
                    sp->return_datatype= g_strdup(return_type);
                    pcre_free_substring(return_type);
                  }
                }

                if(sp_code[0])
                {
                  //Look only at params
                  sp_code= filter_sp_header(sp_code);

                  sp->params= g_malloc(sizeof(MYX_SCHEMA_STORED_PROCEDURE_PARAM)*sp_num_allocated);
                  sp->params_num= 0;              
                
                  offset= 0;

                  while((rc= pcre_exec(pcre_exp, NULL, sp_code, (int)strlen(sp_code), offset, 0, matched, sizeof(matched)/sizeof(*matched)))>=0)
                  {
                    if (sp->params_num>=sp_num_allocated)
                    {
                      sp_num_allocated+= 5;
                      sp->params= g_realloc(sp->params, sizeof(MYX_SCHEMA_STORED_PROCEDURE_PARAM)*sp_num_allocated);
                    }

                    param= sp->params + sp->params_num;

                    pcre_get_substring(sp_code, matched, rc, 1, &param_type);
                    pcre_get_substring(sp_code, matched, rc, 2, &param_name);
                    pcre_get_substring(sp_code, matched, rc, 3, &param_datatype);

                    param->name= g_strdup(param_name);
                    param->datatype= g_strdup(param_datatype);
                    if (strcmp2(param_type, "")==0)
                      param->param_type= MSPPT_IN;
                    else
                      if (g_ascii_strncasecmp(param_type, "OUT", 3)==0)
                        param->param_type= MSPPT_OUT;
                      else
                        if (g_ascii_strncasecmp(param_type, "INOUT", 5)==0)
                          param->param_type= MSPPT_INOUT;
                        else
                          param->param_type= MSPPT_IN;

                    pcre_free_substring(param_type);
                    pcre_free_substring(param_name);
                    pcre_free_substring(param_datatype);

                    //Move offset
                    offset= matched[1];

                    sp->params_num++;
                  }

                  if (sp->params_num!=sp_num_allocated)
                    sp->params= g_realloc(sp->params, sizeof(MYX_SCHEMA_STORED_PROCEDURE_PARAM)*sp->params_num);
                }
                else
                {
                  sp->params= NULL;
                  sp->params_num= 0;
                }

              }
            }
            mysql_free_result(res);
          }
          g_free(sqlcmd);

          sp_num++;

  #undef XSTRDUP
        }
        while (1);

      };
      mysql_free_result(res_sps);
    };
  };

  // It can happen that we end up with less entries than anticipated, so adjust the array a last time.
  if (sps->schema_sps_num != sp_num)
  {
    sps->schema_sps= g_realloc(sps->schema_sps, sizeof(MYX_SCHEMA_STORED_PROCEDURE) * sp_num);
    sps->schema_sps_num= sp_num;
  };

  pcre_free(pcre_exp2);
  pcre_free(pcre_exp);

  restore_old_schema(mysql, old_db);
  return sps;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_CATALOGS struct
    @param catalogs MYX_CATALOGS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_catalogs(MYX_CATALOGS *catalogs)
{
  MYX_CATALOG * catalog= catalogs->catalogs;
  MYX_CATALOG * catalogs_end= catalog + catalogs->catalogs_num;

  for (; catalog!=catalogs_end; catalog++)
  {
    MYX_SCHEMA * schemata= catalog->schemata;
    MYX_SCHEMA * schematas_end= schemata + catalog->schemata_num;
    for (; schemata!=schematas_end; schemata++)
    {
      myx_free_schema_tables(schemata->schema_tables);
      myx_free_schema_indices(schemata->schema_indices);
      myx_free_schema_sps(schemata->schema_sps);
      g_free(schemata->schema_name);
      g_free(schemata->escaped_schema_name);
    }
    g_free(catalog->schemata);
  }

  g_free(catalogs->catalogs);
  g_free(catalogs);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_TABLE_COLUMN struct
    @param schema_column MYX_SCHEMA_TABLE_COLUMN struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_column(MYX_SCHEMA_TABLE_COLUMN *schema_column)
{
  g_free(schema_column->column_name);
  g_free(schema_column->column_type);
  g_free(schema_column->default_value);
  g_free(schema_column->extra);
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_TABLE_INDEX_COLUMN struct
    @param schema_column MYX_TABLE_INDEX_COLUMN struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int free_index_column(MYX_TABLE_INDEX_COLUMN *schema_column)
{
  g_free(schema_column->column_name);
  g_free(schema_column->seq_in_index);
  g_free(schema_column->collation);
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_TABLE_INDEX struct
    @param schema_index MYX_TABLE_INDEX struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_index(MYX_TABLE_INDEX *schema_index)
{
  MYX_TABLE_INDEX_COLUMN * column= schema_index->index_columns;
  MYX_TABLE_INDEX_COLUMN * columns_end= column+schema_index->index_columns_num;

  g_free(schema_index->key_name);
  g_free(schema_index->index_type);
  g_free(schema_index->table_name);

  for (; column!=columns_end; column++)
    free_index_column(column);

  g_free(schema_index->index_columns);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_TABLES struct
    @param schema_tables MYX_SCHEMA_TABLES struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_tables(MYX_SCHEMA_TABLES *schema_tables)
{
  if (schema_tables)
  {
    MYX_SCHEMA_TABLE *table= schema_tables->schema_tables;
    MYX_SCHEMA_TABLE *tables_end= table + schema_tables->schema_tables_num;
    for (;table!=tables_end;table++)
    {
      MYX_SCHEMA_TABLE_COLUMN * column= table->columns;
      MYX_SCHEMA_TABLE_COLUMN * columns_end= column + table->columns_num;
      for (; column!=columns_end; column++)
        myx_free_schema_column(column);
      g_free(table->columns);
      g_free(table->table_name);
    }
    g_free(schema_tables->schema_tables);
    g_free(schema_tables);
  }

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_VIEW_STATUS struct
    @param schema_table MYX_VIEW_STATUS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_view_status_struct(MYX_VIEW_STATUS *schema_view)
{
  g_free(schema_view->comment);
  g_free(schema_view->view_name);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_TABLE_STATUS struct
    @param schema_table MYX_TABLE_STATUS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_table_status_struct(MYX_TABLE_STATUS *schema_table)
{
  MYX_TABLE_INDEX * index= schema_table->indexes;
  MYX_TABLE_INDEX * indexes_end= index + schema_table->indexes_num;
  MYX_SCHEMA_TABLE_COLUMN * column= schema_table->columns;
  MYX_SCHEMA_TABLE_COLUMN * columns_end= column + schema_table->columns_num;

  g_free(schema_table->auto_increment);
  g_free(schema_table->avg_row_length);
  g_free(schema_table->check_time);
  g_free(schema_table->comment);
  g_free(schema_table->create_options);
  g_free(schema_table->create_time);
  g_free(schema_table->data_free);
  g_free(schema_table->data_length);
  g_free(schema_table->index_length);
  g_free(schema_table->max_data_length);
  g_free(schema_table->row_format);
  g_free(schema_table->rows);
  g_free(schema_table->table_name);
  g_free(schema_table->table_type);
  g_free(schema_table->update_time);

  for (; index!=indexes_end; index++)
    myx_free_schema_index(index);
  g_free(schema_table->indexes);

  for (; column!=columns_end; column++)
    myx_free_schema_column(column);
  g_free(schema_table->columns);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_STORED_PROCEDURE struct
    @param schema_sps MYX_SCHEMA_STORED_PROCEDURE struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_sps_struct(MYX_SCHEMA_STORED_PROCEDURE *sp)
{
    MYX_SCHEMA_STORED_PROCEDURE_PARAM *param= sp->params;
    MYX_SCHEMA_STORED_PROCEDURE_PARAM *params_end= param + sp->params_num;

    if (sp->name) 
      g_free(sp->name);
    if (sp->definer) 
      g_free(sp->definer);
    if (sp->created) 
      g_free(sp->created);
    if (sp->modified) 
      g_free(sp->modified);
    if (sp->comment) 
      g_free(sp->comment);
    if (sp->return_datatype) 
      g_free(sp->return_datatype);

    for (; param!=params_end; param++)
    {
      g_free(param->name);
      g_free(param->datatype);
    }

    g_free(sp->params);
    return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_INDICES struct
    @param schema_indices MYX_SCHEMA_INDICES struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_indices(MYX_SCHEMA_INDICES *schema_indices)
{
  if(schema_indices) 
  {
    MYX_TABLE_INDEX * index= schema_indices->indices;
    MYX_TABLE_INDEX * indexes_end= index + schema_indices->indices_num;

    for (; index!=indexes_end; index++)
      myx_free_schema_index(index);
    g_free(schema_indices->indices);
    g_free(schema_indices);
  }

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_TABLE_STATUS struct
    @param schema_entities MYX_SCHEMA_TABLE_STATUS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_entity_status(MYX_SCHEMA_ENTITY_STATUS *e)
{
  unsigned i;

  if (e != NULL)
  {
    for(i= 0; i < e->schema_entities_num; i++)
    {
      if (e->schema_entities[i].entity == NULL) continue;
      switch(e->schema_entities[i].entity_type)
      {
      case MYX_ENTITY_TABLE:
        myx_free_schema_table_status_struct((MYX_TABLE_STATUS *)e->schema_entities[i].entity);
        break;
      case MYX_ENTITY_VIEW:
        myx_free_schema_view_status_struct((MYX_VIEW_STATUS *)e->schema_entities[i].entity);
        break;
      case MYX_ENTITY_PROC:
      case MYX_ENTITY_FUNC:
        myx_free_schema_sps_struct((MYX_SCHEMA_STORED_PROCEDURE *)e->schema_entities[i].entity);
        break;
      }
    }
    g_free(e->schema_entities);
    g_free(e);
  };

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_TABLE_STATUS struct
    @param schema_tables MYX_SCHEMA_TABLE_STATUS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_table_status(MYX_SCHEMA_TABLE_STATUS *schema_tables)
{
  if (schema_tables)
  {
    MYX_TABLE_STATUS * table= schema_tables->schema_tables;
    MYX_TABLE_STATUS * tables_end= table + schema_tables->schema_tables_num;
    for (; table!=tables_end; table++)
      myx_free_schema_table_status_struct(table);
    g_free(schema_tables->schema_tables);
    g_free(schema_tables);
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_VIEW_STATUS struct
    @param schema_tables MYX_SCHEMA_VIEW_STATUS struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_view_status(MYX_SCHEMA_VIEW_STATUS *schema_views)
{
  if (schema_views)
  {
    MYX_VIEW_STATUS * view= schema_views->schema_views;
    MYX_VIEW_STATUS * views_end= view + schema_views->schema_views_num;
    for (; view!=views_end; view++)
      myx_free_schema_view_status_struct(view);
    g_free(schema_views->schema_views);
    g_free(schema_views);
  }
  return 0;
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the MYX_SCHEMA_STORED_PROCEDURE struct
    @param schema_sps MYX_SCHEMA_STORED_PROCEDURE struct to free
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
int myx_free_schema_sps(MYX_SCHEMA_STORED_PROCEDURES *schema_sps)
{
  if (schema_sps)
  {
    MYX_SCHEMA_STORED_PROCEDURE *sp= schema_sps->schema_sps;
    MYX_SCHEMA_STORED_PROCEDURE *sps_end= sp + schema_sps->schema_sps_num;
    for (; sp!=sps_end; sp++)
    {
      myx_free_schema_sps_struct(sp);
    }

    g_free(schema_sps->schema_sps);
    g_free(schema_sps);
  }
  return 0;
}
