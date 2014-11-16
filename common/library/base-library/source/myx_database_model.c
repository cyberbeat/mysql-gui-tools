   // Copyright (C) 2003 - 2008 MySQL AB, 2008 Sun Microsystems, Inc.

#include "myx_public_interface.h"
#include "myx_util.h"
#include "myx_library.h"

#define STR_EMPTY(s)  (!(s) || !*(s))

#define STR_OR_NULL(s) (STR_EMPTY(s) ? "NULL" : s)

#define COLUMN_LIST_PCRE "(\\s*" IDENTIFIER_IGNORE_PCRE "\\s*(?:,\\s*" IDENTIFIER_IGNORE_PCRE "\\s*)*)"
#define MATCH_PCRE "(?:\\s+match\\s+full|match\\s+partial|match\\s+simple)?"

/*
 * functions
 */

char *myx_dbm_get_view_name_from_query(const char *create_view_query)
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  int o_vector[256];
  int rc;
  char *rex;

  rex= "CREATE.*VIEW\\s+(" QUALIFIED_IDENTIFIER_PCRE ")\\s+AS.*";
  pcre_exp= pcre_compile(rex, PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &erroffset, NULL);

  if((rc= pcre_exec(pcre_exp, NULL, create_view_query, (int)strlen(create_view_query),
                       0, 0, o_vector, sizeof(o_vector)/sizeof(int))) > 0)
  {
    const char *value;
    char *retval;
    pcre_get_substring(create_view_query, o_vector, rc, 1, &value);
    retval= g_strdup(value);
    pcre_free_substring(value);
    pcre_free(pcre_exp);
    return retval;
  }

  return NULL;
}

static char *myx_dbm_force_create_or_replace(const char *str)
{
  pcre *pcre_exp;
  const char *error_str;
  int erroffset;
  int o_vector[256];
  int rc;
  char *rex;

  pcre_exp= pcre_compile("CREATE\\s+OR\\s+REPLACE\\s+.*",
                         PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &erroffset, NULL);

  if((pcre_exec(pcre_exp, NULL, str, (int)strlen(str),
                       0, 0, o_vector, sizeof(o_vector)/sizeof(int))) > 0)
  {
    pcre_free(pcre_exp);
    return g_strdup(str); // str already contains CoR
  }

  pcre_free(pcre_exp);
  pcre_exp= pcre_compile("CREATE\\s+(.*)",
                         PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &erroffset, NULL);

  if((rc= pcre_exec(pcre_exp, NULL, str, (int)strlen(str),
                       0, 0, o_vector, sizeof(o_vector)/sizeof(int))) > 0)
  {
    const char *value;
    char *retval;
    pcre_get_substring(str, o_vector, rc, 1, &value);
    retval= g_strdup_printf("CREATE OR REPLACE %s", value);
    pcre_free_substring(value);
    pcre_free(pcre_exp);
    return retval;
  }
  else
  {
    pcre_free(pcre_exp);
    return g_strdup(str); // str doesnt contain CREATE
  }


}

MYX_DBM_DATATYPE * myx_dbm_get_datatype(MYX_DBM_DATATYPES *datatypes, char **dtype)
{
  unsigned int i;
  MYX_DBM_DATATYPE *datatype;
  MYX_NAME_VALUE_PAIR *subst;
  char *tmp2;

  //Check datatype substitutes
  for(i= 0; i<datatypes->substitutes_num; i++)
  {
    int len;
    subst= datatypes->substitutes+i;
    
    len= (int)strlen(subst->name);
    if (g_ascii_strncasecmp(*dtype, subst->name, len) == 0
        && ((*dtype)[len]=='(' || (*dtype)[len]==' ' || (*dtype)[len]==0))
    {
      tmp2= *dtype;
      *dtype= g_strdup_printf("%s%s", subst->value, *dtype+len);
      g_free(tmp2);
      break;
    }
  }

  //Find datatype
  for(i= 0; i<datatypes->datatypes_num; i++)
  {
    int len;

    datatype= datatypes->datatypes+i;

    len= (int)strlen(datatype->name);
    if (g_ascii_strncasecmp(*dtype, datatype->name, len) == 0
        && ((*dtype)[len]=='(' || (*dtype)[len]==' ' || (*dtype)[len]==0))
      return datatype;
  }

  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Search the engines structure for the an entry with the given name and returns it.
 *
 * @param engines A list of available engines in the database.
 * @param engine The name of a storage engine to search for.
 *
 * @return A pointer to an engine structure in the engines list or NULL if the engine could not be found.
 */
MYX_ENGINE *find_storage_engine(MYX_ENGINES *engines, const char *engine)
{
  unsigned int i;
  MYX_ENGINE* result= NULL;
  MYX_ENGINE* run= engines->engines;

  for (i = 0; i < engines->engines_num; i++, run++)
  {
    if (strcasecmp(engine, run->name) == 0)
    {
      result= run;
      break;
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_dbm_get_datatype_name(char *dtype)
{
  char *ptr1, *ptr2;
  
  ptr1= strchr(dtype, '(');
  ptr2= strchr(dtype, ' ');
  if (!ptr1 && !ptr2)
    return g_strdup(dtype);
  else
  {
    if ((ptr1 < ptr2 && ptr1) || !ptr2)
      return g_strndup(dtype, (gsize)(ptr1-dtype));
    else
      return g_strndup(dtype, (gsize)(ptr2-dtype));
  }
}


char * myx_dbm_get_datatype_params(char *datatype_name, char *dtype)
{
  if(!datatype_name)
    return NULL;

  if(dtype[strlen(datatype_name)] != '(')
    return NULL;
  else
  {
    char *copy= g_strdup(dtype+strlen(datatype_name));
    char *ptr;
    
    ptr= strrchr(copy, ')');
    if (ptr)
    {
      ptr++;
      *ptr= 0;
    }
    return copy;
  }
}
 
char ** get_datatype_flags(MYX_DBM_DATATYPE *datatype, char *dtype, unsigned int *num)
{
  unsigned int i, len= (unsigned int)strlen(dtype);
  char *tmp= mask_out_string(g_ascii_strup(dtype, len), '(', ')', 'x');
  char *tmp2;
  char **flags= NULL;

  *num= 0;

  if(datatype)
  {
    for(i= 0; i < datatype->flags_num; i++)
    {
      tmp2= g_strstr_len(tmp, len, datatype->flags[i]);
      if(tmp2)
      {
        if(!flags)
          flags= g_malloc(sizeof(char *));
        else
          flags= g_realloc(flags, sizeof(char *)*(*num+1));

        flags[*num]= g_strdup(datatype->flags[i]);

        *num= *num+1;
      }
    }
  }

  g_free(tmp);

  return flags;
}


static void parse_foreign_keys(MYSQL *mysql, const char *table_def, MYX_DBM_TABLE_DATA *table)
{
  pcre *compiled_constraint;
  const char *error_str;
  int erroffset;
  int constraint_result_vector[60];
  int rc;
  int offset;
  char *rex;
  char *action_details;
  static struct {
    char *kw;
    MYX_DBM_FK_ACTION action;
  } fk_actions[]= {
    {"CASCADE", MYX_DBM_FA_CASCADE},
    {"SET NULL", MYX_DBM_FA_SET_NULL},
    {"NO ACTION", MYX_DBM_FA_NO_ACTION},
    {"RESTRICT", MYX_DBM_FA_RESTRICT},
    {NULL, MYX_DBM_FA_NO_ACTION}
  };

  table->fks_num= 0;
  table->fks= NULL;

  rex= ".*?constraint\\s+(" IDENTIFIER_PCRE ")\\s+foreign\\s+key\\s+\\(" COLUMN_LIST_PCRE "\\)"
    "\\s+REFERENCES\\s+" QUALIFIED_IDENTIFIER_PCRE "\\s+\\(" COLUMN_LIST_PCRE "\\)" MATCH_PCRE
    "((?:\\s+on\\s+\\w+\\s+(?:no\\s+action|set\\s+null|restrict|cascade))*)";
  action_details= "(?:\\s+on\\s+(delete|update)\\s+(set\\s+null|no\\s+action|restrict|cascade))";

  compiled_constraint= pcre_compile(rex, PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &error_str, &erroffset, NULL);
  
  if (compiled_constraint)
  {
    offset= 0;
    while ((rc= pcre_exec(compiled_constraint, NULL, table_def, (int)strlen(table_def), offset, 0, constraint_result_vector, 60)) > 0)
    {
      const char* value;
      const char* option;
      MYX_DBM_FK_DATA fk;
      char **src;
      char **dst;
      int i;

      // name
      pcre_get_substring(table_def, constraint_result_vector, rc, 1, &value);
      fk.name= unquote_identifier(myx_convert_dbstr_utf8(mysql, value, -1));
      fk.original_name= g_strdup(fk.name);
      pcre_free_substring(value);

      // source columns
      pcre_get_substring(table_def, constraint_result_vector, rc, 2, &value);
      src= g_strsplit(value, ", ", 0);
      pcre_free_substring(value);

      // Referenced schema (optional)
      pcre_get_substring(table_def, constraint_result_vector, rc, 3, &value);
      fk.reference_schema_name= unquote_identifier(myx_convert_dbstr_utf8(mysql, value, -1));
      pcre_free_substring(value);

      // Referenced table
      pcre_get_substring(table_def, constraint_result_vector, rc, 4, &value);
      fk.reference_table_name= unquote_identifier(myx_convert_dbstr_utf8(mysql, value, -1));
      pcre_free_substring(value);

      // target columns
      pcre_get_substring(table_def, constraint_result_vector, rc, 5, &value);
      dst= g_strsplit(value, ", ", 0);
      pcre_free_substring(value);

      for (i= 0; src[i] && dst[i]; i++);
      g_assert(!src[i] && !dst[i]);

      fk.column_mapping_num= i;
      fk.column_mapping= g_malloc(sizeof(MYX_DBM_FK_DATA)*i);
      for (i= 0; src[i]; i++)
      {
        fk.column_mapping[i].name= g_strdup(unquote_identifier(src[i]));
        fk.column_mapping[i].value= g_strdup(unquote_identifier(dst[i]));
      }
      g_strfreev(src);
      g_strfreev(dst);

      fk.on_update= MYX_DBM_FA_RESTRICT;
      fk.on_delete= MYX_DBM_FA_RESTRICT;

      // Actions, note: there can be more than one.
      if (rc > 6)
      {
        const signed char *actions;
        pcre *compiled_actions;
        int action_result_vector[60];
        const signed char *action_pcre_error;
        int action_error_offset;
        int action_result;
        int action_offset= 0;

        pcre_get_substring(table_def, constraint_result_vector, rc, 6, &actions);

        compiled_actions= pcre_compile(action_details, PCRE_CASELESS|PCRE_UTF8|PCRE_DOTALL, &action_pcre_error,
          &action_error_offset, NULL);
        while ((action_result= pcre_exec(compiled_actions, NULL, actions, (int)strlen(actions), action_offset, 0,
          action_result_vector, 60)) > 0)
        {
          pcre_get_substring(actions, action_result_vector, action_result, 1, &value);
          pcre_get_substring(actions, action_result_vector, action_result, 2, &option);

          if (strcasecmp(value, "UPDATE") == 0)
          {
            for (i= 0; fk_actions[i].kw; i++)
            {
              if (strncasecmp(option, fk_actions[i].kw, strlen(fk_actions[i].kw)) == 0)
              {
                fk.on_update= fk_actions[i].action;
                break;
              }
            }
          };

          if (strcasecmp(value, "DELETE") == 0)
          {
            for (i= 0; fk_actions[i].kw; i++)
            {
              if (strncasecmp(option, fk_actions[i].kw, strlen(fk_actions[i].kw))==0)
              {
                fk.on_delete= fk_actions[i].action;
                break;
              }
            }
          }

          pcre_free_substring(value);
          pcre_free_substring(option);

          // Move starting point the end of the last match.
          action_offset= action_result_vector[2 * action_result - 1];
        };
        pcre_free_substring(actions);
        pcre_free(compiled_actions);
      };

      table->fks_num++;
      table->fks= g_realloc(table->fks, sizeof(MYX_DBM_FK_DATA) * table->fks_num);
      memcpy(table->fks+(table->fks_num - 1), &fk, sizeof(fk));

      // Move starting point the end of the last match.
      offset= constraint_result_vector[2 * rc - 1];
    }
		pcre_free(compiled_constraint);
	}
}

MYX_DBM_TABLE_DATA * myx_dbm_retrieve_table_data(MYSQL *mysql, MYX_DBM_DATATYPES *datatypes, MYX_ENGINES *engines,
  const char *catalog, const char *schema, const char *table, MYX_LIB_ERROR *error_code)
{
  MYX_DBM_TABLE_DATA *table_data= g_malloc0(sizeof(MYX_DBM_TABLE_DATA));
  MYX_DBM_COLUMN_DATA *column;
  char *query;
  unsigned int i, j;
  char *tmp;
  char *tbl_options= NULL;
  unsigned int len;
  char *lower_case_table;

  int has_pk= 0;

  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;

  *error_code= MYX_NO_ERROR;

  //--------------------
  // Get CREATE TABLE statement
  query= g_strdup_printf("SHOW CREATE TABLE `%s`.`%s`", schema, table);

  if (!(myx_mysql_query(mysql, query)))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        table_data->catalog= g_strdup(catalog);
        table_data->schema= g_strdup(schema);
        table_data->name= myx_convert_dbstr_utf8(mysql, row[0], -1);
        table_data->original_name= g_strdup(table_data->name);

        table_data->create_table_stmt= myx_convert_dbstr_utf8(mysql, row[1], -1);
      }
      else
      {
        *error_code= MYX_SQL_ERROR;
        myx_dbm_free_table_data(table_data);
        g_free(query);
        return NULL;
      }

      mysql_free_result(res);
    }
    else
    {
      *error_code= MYX_OBJECT_NOT_FOUND;
      myx_dbm_free_table_data(table_data);
      g_free(query);
      return NULL;
    }
  }
  else
  {
    *error_code= MYX_OBJECT_NOT_FOUND;
    myx_dbm_free_table_data(table_data);
    g_free(query);
    return NULL;
  }
  g_free(query);

  // Get table options.
  mysql_query(mysql, "/*!40101 SET @old_collation_connection=@@collation_connection, @@collation_connection=utf8_general_ci */");

  if (mysql_is_case_sensitive(mysql) == 0)
  {
    lower_case_table= g_utf8_strdown(table, -1);
    query= g_strdup_printf("SHOW TABLE STATUS FROM `%s` LIKE '%s'", schema, lower_case_table);
    g_free(lower_case_table);
  }
  else
    query= g_strdup_printf("SHOW TABLE STATUS FROM `%s` LIKE '%s'", schema, table);

  if (!(myx_mysql_query(mysql, query)))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      // Depending on the version of the server there might be different columns.
      num_fields = mysql_num_fields(res);
      fields = mysql_fetch_fields(res);

      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        for (i= 0; i < num_fields; i++)
        {
          if ((strcmp(fields[i].name, "Type") == 0) || (strcmp(fields[i].name, "Engine") == 0))
            table_data->storage_engine= find_storage_engine(engines, row[i]);
          else
            if (strcasecmp(fields[i].name, "Auto_increment") == 0)
              table_data->next_auto_inc= g_strdup(row[i]);
            else
              if (strcasecmp(fields[i].name, "Collation") == 0)
              {
                table_data->collation= g_strdup(row[i]);
                table_data->charset= g_strdup(table_data->collation);

                for(j= 0; j < 256; j++)
                {
                  if (table_data->charset[j] == '_')
                  {
                    table_data->charset[j]= 0;
                    break;
                  };
                };
              }
              else
                if (strcasecmp(fields[i].name, "Comment") == 0)
                  table_data->comment= myx_convert_dbstr_utf8(mysql, row[i], -1);
                else
                  if (strcasecmp(fields[i].name, "Create_options") == 0)
                    tbl_options= g_strdup(row[i]);
                  else
                    if (strcasecmp(fields[i].name, "Row_format") == 0)
                    {
                      if (strcasecmp(row[i], "DYNAMIC") == 0)
                        table_data->row_format= MYX_DBM_TRF_DYNAMIC;
                      else
                        if (strcasecmp(row[i], "FIXED") == 0)
                          table_data->row_format= MYX_DBM_TRF_FIXED;
                        else
                          if (strcasecmp(row[i], "COMPRESSED") == 0)
                            table_data->row_format= MYX_DBM_TRF_COMPRESSED;
                          else
                            if (strcasecmp(row[i], "COMPACT") == 0)
                              table_data->row_format= MYX_DBM_TRF_COMPACT;
                            else
                              if(strcasecmp(row[i], "REDUNDANT") == 0)
                                table_data->row_format= MYX_DBM_TRF_REDUNDANT;
                              else
                                table_data->row_format= MYX_DBM_TRF_DEFAULT;
                    };
        }
      }
      else
      {
        *error_code= MYX_SQL_ERROR;
        myx_dbm_free_table_data(table_data);
        g_free(query);
        return NULL;
      }

      mysql_free_result(res);
    }
    else
    {
      *error_code= MYX_SQL_ERROR;
      myx_dbm_free_table_data(table_data);
      g_free(query);
      return NULL;
    }
  }
  else
  {
    *error_code= MYX_SQL_ERROR;
    myx_dbm_free_table_data(table_data);
    g_free(query);
    return NULL;
  }
  g_free(query);

  mysql_query(mysql, "/*!40101 SET @@collation_connection=@old_collation_connection */");

  //--------------------
  //Process table options
  if(tbl_options)
  {
    len= (unsigned int)strlen(tbl_options);

    //Delay key write
    tmp= get_value_from_text(tbl_options, len, "DELAY_KEY_WRITE\\s*=\\s*(0|1)");
    if(strcmp2(tmp, "1") == 0)
      table_data->delay_key_write= 1;
    g_free(tmp);

    //Pack keys
    tmp= get_value_from_text(tbl_options, len, "PACK_KEYS\\s*=\\s*(\\w+)");
    if(strcmp2(tmp, "0") == 0)
      table_data->pack_keys= MYX_DBM_TPK_NONE;
    else if(strcmp2(tmp, "1") == 0)
      table_data->pack_keys= MYX_DBM_TPK_ALL;
    else
      table_data->pack_keys= MYX_DBM_TPK_DEFAULT;
    g_free(tmp);

    table_data->avg_row_length= get_value_from_text(tbl_options, len, "AVG_ROW_LENGTH\\s*=\\s*(\\w+)");
    table_data->min_rows= get_value_from_text(tbl_options, len, "MIN_ROWS\\s*=\\s*(\\w+)");
    table_data->max_rows= get_value_from_text(tbl_options, len, "MAX_ROWS\\s*=\\s*(\\w+)");
    tmp= get_value_from_text(tbl_options, len, "CHECKSUM=\\s*(0|1)");
    if(strcmp2(tmp, "1") == 0)
      table_data->checksum= 1;
    else
      table_data->checksum= 0;
    g_free(tmp);

    g_free(tbl_options);
  }
  else
  {
    table_data->row_format= MYX_DBM_TRF_DEFAULT;
    table_data->pack_keys= MYX_DBM_TPK_DEFAULT;
  }

  //--------------------
  //Process CREATE TABLE statement
  if(table_data->create_table_stmt)
  {
    //Mask out strings
    tbl_options= mask_out_string(g_strdup(table_data->create_table_stmt), '\'', '\'', 'x');
    len= (unsigned int)strlen(tbl_options);

    //Merge UNION
    table_data->merge_union= get_value_from_text(tbl_options, len, "UNION\\s*=\\s*\\((.+)\\)");

    //Merge insert method
    tmp= get_value_from_text(tbl_options, len, "INSERT_METHOD\\s*=\\s*(\\w+)");
    if(strcmp2(tmp, "FIRST") == 0)
      table_data->merge_insert= MYX_DBM_TMI_FIRST;
    else if(strcmp2(tmp, "LAST") == 0)
      table_data->merge_insert= MYX_DBM_TMI_LAST;
    else
      table_data->merge_insert= MYX_DBM_TMI_NO;
    g_free(tmp);

    //Data and Index directory
    table_data->table_data_dir= get_value_from_text(tbl_options, len, "DATA DIRECTORY\\s*=\\s*'(.+)'");
    table_data->table_index_dir= get_value_from_text(tbl_options, len, "INDEX DIRECTORY\\s*=\\s*'(.+)'");
    
    //RAID type
    tmp= get_value_from_text(tbl_options, len, "RAID_TYPE\\s*=\\s*(\\w+)");
    if((strcmp2(tmp, "1") == 0) || (strcmp2(tmp, "STRIPED") == 0) || (strcmp2(tmp, "RAID0") == 0))
      table_data->raid_type= MYX_DBM_TRT_STRIPED;
    else
      table_data->raid_type= MYX_DBM_TRT_NONE;
    g_free(tmp);

    table_data->raid_chunks= get_value_from_text(tbl_options, len, "RAID_CHUNKS\\s*=\\s*(\\w+)");
    table_data->raid_chunk_size= get_value_from_text(tbl_options, len, "RAID_CHUNKSIZE\\s*=\\s*(\\w+)");

    // connection for federated
    table_data->federated_connection= get_value_from_text(tbl_options, len, "CONNECTION\\s*=\\s*'(.*)'");

    //Foreign key constraints
    parse_foreign_keys(mysql, tbl_options, table_data);
    
    g_free(tbl_options);
  }
  else
  {
    table_data->merge_insert= MYX_DBM_TMI_NO;
    table_data->raid_type= MYX_DBM_TRT_NONE;
  }

  //-----------------
  // Get table indices.
  // We also need to determine if we have a primary key in general, as this is required
  // to make sure a column marked as PK ("PRI") is really a PK column and not something special.
  // Read also: http://dev.mysql.com/doc/refman/5.0/en/show-columns.html
  res= NULL;
  query= g_strdup_printf("SHOW INDEX FROM `%s`.`%s`", schema, table);
  if (myx_mysql_query(mysql, query) == 0)
    res= mysql_store_result(mysql);
  if (res != NULL)
  {
    MYX_DBM_INDEX_DATA *table_index= NULL;
    int index_name_pos= 2;
    char *current_index_name;
    unsigned int k;
    int cur_index;

    table_data->indices_num= (unsigned int)mysql_num_rows(res);
    table_data->indices= g_malloc0(sizeof(MYX_TABLE_INDEX)*table_data->indices_num);

    // Depending on the version of the server there might be different columns.
    num_fields = mysql_num_fields(res);
    fields = mysql_fetch_fields(res);

    for(k= 0; k<num_fields; k++)
    {
      if((strcmp(fields[k].name, "Key_name")==0))
      {
        index_name_pos= k;
        break;
      }
    }

    current_index_name= NULL;
    cur_index= 0;

    do
    {
      row= mysql_fetch_row(res);
      if (row == NULL)
      break;

      if (strcmp2(current_index_name, row[index_name_pos])!=0)
      {
        table_index= table_data->indices + cur_index;

        table_index->name= myx_convert_dbstr_utf8(mysql, row[index_name_pos], -1);
        table_index->original_name= g_strdup(table_index->name);

        if(strcmp2(table_index->name, "PRIMARY") == 0)
        {
          table_index->index_kind= MYX_DBM_IK_PRIMARY;
          has_pk= 1;
        }

        for(k=0;k<num_fields;k++)
        {
          if (strcmp2(fields[k].name, "Index_type") == 0)
          {
            if(strcmp2(row[k], "BTREE") == 0)
            {
               table_index->index_type= MYX_DBM_IT_BTREE;
            }
            else if(strcmp2(row[k], "RTREE") == 0)
            {
               table_index->index_type= MYX_DBM_IT_RTREE;
            }
            else if(strcmp2(row[k], "HASH") == 0)
            {
               table_index->index_type= MYX_DBM_IT_HASH;
            }
            else if(strcmp2(row[k], "FULLTEXT") == 0)
            {
               table_index->index_type= MYX_DBM_IT_DEFAULT;
               table_index->index_kind= MYX_DBM_IK_FULLTEXT;
            }
            else if(strcmp2(row[k], "SPATIAL") == 0)
            {
               table_index->index_type= MYX_DBM_IT_DEFAULT;
               table_index->index_kind= MYX_DBM_IK_SPATIAL;
            }
          }
          else if (strcmp2(fields[k].name, "Non_unique") == 0)
          {
            if((strcmp2(row[k], "0") == 0) && (table_index->index_kind != MYX_DBM_IK_PRIMARY))
            {
              table_index->index_kind= MYX_DBM_IK_UNIQUE;
            }
          }
        }

        table_index->columns_num= 0;
        table_index->columns= NULL;
        
        cur_index++;
        current_index_name= table_index->name;
      }

      table_index->columns= g_realloc(table_index->columns, 
                                      sizeof(MYX_DBM_INDEX_COLUMN_DATA)*(table_index->columns_num+1));

      table_index->columns[table_index->columns_num].name= NULL;
      table_index->columns[table_index->columns_num].len= NULL;
      table_index->columns[table_index->columns_num].value_order= NULL;
      for(k=0;k<num_fields;k++)
      {
        if (strcmp(fields[k].name, "Column_name")==0)
        {
          table_index->columns[table_index->columns_num].name= myx_convert_dbstr_utf8(mysql, row[k], -1);
        }
        else if (strcmp(fields[k].name, "Sub_part")==0)
        {
          table_index->columns[table_index->columns_num].len= g_strdup(row[k]);
        }
        else if (strcmp(fields[k].name, "Collation")==0)
        {
          table_index->columns[table_index->columns_num].value_order= g_strdup(row[k]);
        }
      }
      table_index->columns_num++;
    }
    while (1);

    table_data->indices_num= cur_index;
    table_data->indices= g_realloc(table_data->indices, sizeof(MYX_TABLE_INDEX)*table_data->indices_num);

    mysql_free_result(res);

  }

  //--------------------
  //Get table columns

  query= g_strdup_printf("SHOW FULL COLUMNS FROM `%s`.`%s`", schema, table);

  if (!myx_mysql_query(mysql, query))
  {
    res= mysql_store_result(mysql);
    if (res != NULL)
    {
      //Depending on the version of the server there might be different columns
      num_fields = mysql_num_fields(res);
      fields = mysql_fetch_fields(res);

      table_data->columns_num= (unsigned int)mysql_num_rows(res);
      table_data->columns= g_malloc0(sizeof(MYX_DBM_COLUMN_DATA)*table_data->columns_num);

      for(column= table_data->columns; (row= mysql_fetch_row(res))!=NULL; column++)
      {
        column->default_value_is_null= 1;

        for (i= 0; i < num_fields; i++)
        {
          if (strcmp(fields[i].name, "Field") == 0)
          {
            column->name= myx_convert_dbstr_utf8(mysql, row[i], -1);
            column->original_name= g_strdup(column->name);
          }
          else if (strcmp(fields[i].name, "Type") == 0)
          {
            tmp= myx_convert_dbstr_utf8(mysql, row[i], -1);

            // Try to find datatype
            column->datatype_pointer= myx_dbm_get_datatype(datatypes, &tmp);
            // If not found just get name.
            if (!column->datatype_pointer)
              column->datatype_name= myx_dbm_get_datatype_name(tmp);
            else
              column->datatype_name= g_strdup(column->datatype_pointer->name);
            column->datatype_params= myx_dbm_get_datatype_params(column->datatype_name, tmp);
            column->datatype_flags= get_datatype_flags(column->datatype_pointer, tmp, &column->datatype_flags_num);

            g_free(tmp);
          }
          else if (strcmp(fields[i].name, "Null") == 0)
          {
            if(strcmp2(row[i], "YES") == 0)
              column->not_null= 0;
            else
              column->not_null= 1;
          }
          else if (strcmp(fields[i].name, "Key")==0)
          {
            if (strcmp2(row[i], "PRI") == 0)
              // There is a speciality when a column is marked as PRI.
              // See: http://dev.mysql.com/doc/refman/5.0/en/show-columns.html
              column->primary_key= has_pk;
            else
              column->primary_key= 0;
          }
          else if (strcmp(fields[i].name, "Default") == 0)
          {
            column->default_value_is_null= row[i] ? 0 : 1;

            column->default_value= myx_convert_dbstr_utf8(mysql, row[i], -1);

            if (column->default_value &&
              (strcmp2(column->default_value, "CURRENT_TIMESTAMP") != 0) &&
              !str_is_numeric(column->default_value))
            {
              char *quoted_default= g_strdup_printf("'%s'", column->default_value);

              g_free(column->default_value);
              column->default_value= quoted_default;
            }
          }
          else if (strcmp(fields[i].name, "Extra")==0)
          {
            if(strcmp2(row[i], "auto_increment") == 0)
              column->auto_inc= 1;
            else
              column->auto_inc= 0;
          }
          else if (strcmp(fields[i].name, "Comment") == 0)
          {
            column->comment= myx_convert_dbstr_utf8(mysql, row[i], -1);
          }
          else if (strcmp(fields[i].name, "Collation") == 0)
          {
            column->collation= myx_convert_dbstr_utf8(mysql, row[i], -1);
            column->charset= g_strdup(column->collation);

            if (column->charset)
            {
              for(j= 0; j<256 && column->charset[j]; j++)
              {
                if(column->charset[j] == '_')
                {
                  column->charset[j]= 0;
                  break;
                }
              }
            }
          }
        }
      }

      mysql_free_result(res);
    }
    else
    {
      *error_code= MYX_SQL_ERROR;
      myx_dbm_free_table_data(table_data);
      g_free(query);
      return NULL;
    }
  }
  else
  {
    *error_code= MYX_SQL_ERROR;
    myx_dbm_free_table_data(table_data);
    g_free(query);
    return NULL;
  }
  g_free(query);

  return table_data;
}

void myx_dbm_free_table_data(MYX_DBM_TABLE_DATA *table_data)
{
  unsigned int i, j;
  MYX_DBM_COLUMN_DATA *column;

  if(!table_data)
    return;

  g_free(table_data->catalog);
  g_free(table_data->schema);
  g_free(table_data->name);
  g_free(table_data->original_name);

  g_free(table_data->create_table_stmt);

  g_free(table_data->next_auto_inc);
  g_free(table_data->charset);
  g_free(table_data->collation);

  g_free(table_data->comment);

  g_free(table_data->merge_union);

  g_free(table_data->table_data_dir);
  g_free(table_data->table_index_dir);

  g_free(table_data->raid_chunks);
  g_free(table_data->raid_chunk_size);

  g_free(table_data->avg_row_length);
  g_free(table_data->min_rows);
  g_free(table_data->max_rows);

  column= table_data->columns;
  for(i= 0; i < table_data->columns_num; i++)
  {
    g_free(column->name);
    g_free(column->original_name);

    g_free(column->datatype_name);
    g_free(column->datatype_params);

    g_free(column->default_value);
    g_free(column->comment);

    for(j= 0; j < column->datatype_flags_num; j++)
    {
      g_free(column->datatype_flags[j]);
    }
    g_free(column->datatype_flags);

    column++;
  }
  g_free(table_data->columns);
  
  for (i= 0; i < table_data->indices_num; i++)
  {
    g_free(table_data->indices[i].name);
    g_free(table_data->indices[i].original_name);

    for (j= 0; j < table_data->indices[i].columns_num; j++)
    {
      g_free(table_data->indices[i].columns[j].name);
      g_free(table_data->indices[i].columns[j].len);
      g_free(table_data->indices[i].columns[j].value_order);
    }
    g_free(table_data->indices[i].columns);
  }
  g_free(table_data->indices);
  
  for (i= 0; i < table_data->fks_num; i++)
  {
    g_free(table_data->fks[i].name);
    g_free(table_data->fks[i].original_name);
    g_free(table_data->fks[i].reference_table_name);    
    for (j= 0; j < table_data->fks[i].column_mapping_num; j++)
    {
      g_free(table_data->fks[i].column_mapping[j].name);
      g_free(table_data->fks[i].column_mapping[j].value);
    }
    g_free(table_data->fks[i].column_mapping);
  }
  g_free(table_data->fks);

  g_free(table_data->federated_connection);

  g_free(table_data);
}

MYX_DBM_CHARSETS * myx_dbm_retrieve_charsets(MYSQL *mysql, MYX_LIB_ERROR *error_code)
{
  MYX_DBM_CHARSETS *charsets= g_malloc0(sizeof(MYX_DBM_CHARSETS));
  MYX_DBM_CHARSET *charset;
  MYX_DBM_COLLATION *collation;

  char *query;
  unsigned int i, j;

  MYSQL_RES *res;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;


  *error_code= MYX_NO_ERROR;

  //--------------------
  // Get CREATE TABLE statement
  query= g_strdup("SHOW CHARACTER SET");

  if (!(myx_mysql_query(mysql, query)))
  {
	res= mysql_store_result(mysql);
	if (res != NULL)
    {
      //Depending on the version of the server there might be different columns
      num_fields = mysql_num_fields(res);
      fields = mysql_fetch_fields(res);

      charsets->charsets_num= (unsigned int)mysql_num_rows(res);
      charsets->charsets= g_malloc0(sizeof(MYX_DBM_CHARSET)*charsets->charsets_num);

      for(charset= charsets->charsets; (row= mysql_fetch_row(res))!=NULL; charset++)
      {
        for (i= 0; i < num_fields; i++)
        {
          if (strcmp(fields[i].name, "Charset") == 0)
          {
            charset->name= myx_convert_dbstr_utf8(mysql, row[i], -1);
          }
          else if (strcmp(fields[i].name, "Description") == 0)
          {
            charset->desc= myx_convert_dbstr_utf8(mysql, row[i], -1);
          }
          else if (strcmp(fields[i].name, "Default collation") == 0)
          {
            charset->default_collation= myx_convert_dbstr_utf8(mysql, row[i], -1);
          }
          else if (strcmp(fields[i].name, "Default collation") == 0)
          {
            charset->max_len= atoi(row[i]);
          }
        }
      }

      mysql_free_result(res);
    }
    else
    {
      *error_code= MYX_SQL_ERROR;
      myx_free_charsets(charsets);
      g_free(query);
      return NULL;
    }
  }
  else
  {
    *error_code= MYX_SQL_ERROR;
    myx_free_charsets(charsets);
    g_free(query);
    return NULL;
  }
  g_free(query);

  for(j= 0; j < charsets->charsets_num; j++)
  {
    charset= charsets->charsets + j;

    query= g_strdup_printf("SHOW COLLATION LIKE '%s%%'", charset->name);

    if (!(myx_mysql_query(mysql, query)))
    {
      res= mysql_store_result(mysql);
      if (res != NULL)
      {
        //Depending on the version of the server there might be different columns
        num_fields = mysql_num_fields(res);
        fields = mysql_fetch_fields(res);

        charset->collations_num= (unsigned int)mysql_num_rows(res);
        charset->collations= g_malloc0(sizeof(MYX_DBM_COLLATION)*charset->collations_num);

        for(collation= charset->collations; (row= mysql_fetch_row(res))!=NULL; collation++)
        {
          for (i= 0; i < num_fields; i++)
          {
            if (strcmp(fields[i].name, "Collation") == 0)
            {
              collation->name= myx_convert_dbstr_utf8(mysql, row[i], -1);
            }
            else if (strcmp(fields[i].name, "Id") == 0)
            {
              collation->id= atoi(row[i]);
            }
            else if (strcmp(fields[i].name, "Default") == 0)
            {
              if(strcmp2("Yes", row[i]) == 0)
                collation->is_default= 1;
            }
            else if (strcmp(fields[i].name, "Compiled") == 0)
            {
              if(strcmp2("Yes", row[i]) == 0)
                collation->is_compiled= 1;
            }
            else if (strcmp(fields[i].name, "Sortlen") == 0)
            {
              collation->sort_len= atoi(row[i]);
            }
          }
        }

        mysql_free_result(res);
      }
      else
      {
        *error_code= MYX_SQL_ERROR;
        myx_free_charsets(charsets);
        g_free(query);
        return NULL;
      }
    }
    else
    {
      *error_code= MYX_SQL_ERROR;
      myx_free_charsets(charsets);
      g_free(query);
      return NULL;
    }

    g_free(query);
  }

  return charsets;
}

char * dbm_get_sql_column_create_code(MYX_DBM_COLUMN_DATA *column, MYX_DBM_SERVER_VERSION *version, int pk_clause)
{
  char *col_def, *tmp;

  if(column->datatype_flags)
  {
    tmp= str_trim(g_strjoinv(" ", column->datatype_flags));
    col_def= g_strdup_printf("`%s` %s%s %s", column->name, 
                             column->datatype_name, column->datatype_params!=NULL?column->datatype_params:"", tmp);
    g_free(tmp);
  }
  else
  {
    col_def= g_strdup_printf("`%s` %s%s", column->name, 
                             column->datatype_name, column->datatype_params!=NULL?column->datatype_params:"");
  }
  
  if ((((version->major_version==4) && (version->minor_version>=1)) || (version->major_version>=4)) && 
    !STR_EMPTY(column->charset) && (strcmp2(column->charset, "NULL")!=0))
  {
    col_def= str_g_append(col_def, " CHARACTER SET ");
    col_def= str_g_append(col_def, column->charset);

    if (!STR_EMPTY(column->collation) && (strcmp2(column->charset, "NULL")!=0))
    {
      col_def= str_g_append(col_def, " COLLATE ");
      col_def= str_g_append(col_def, column->collation);
    }
  }

  if(column->not_null)
    col_def= str_g_append(col_def, " NOT NULL");

  if(!column->default_value_is_null && column->default_value && column->default_value[0])
  {
    tmp = col_def;
    if((column->datatype_pointer) && (column->datatype_pointer->group == MYX_DBM_DTG_NUMERIC))
    {
      if (strcmp(column->default_value, "") != 0)
        col_def= g_strdup_printf("%s DEFAULT %s", col_def, column->default_value);
      else
        col_def= g_strdup_printf("%s DEFAULT 0", col_def);
    }
    else if((column->datatype_pointer) && (column->datatype_pointer->group == MYX_DBM_DTG_DATETIME))
    {
      // starting with 5.x MySQL server doesn't accept '' as default datetime value
      if (strcmp(column->default_value, "") != 0)
      {
        col_def= g_strdup_printf("%s DEFAULT %s", col_def, column->default_value);
      }
      else
      {
        col_def= g_strdup_printf("%s DEFAULT 0", col_def);
      }
    }
    else
      col_def= g_strdup_printf("%s DEFAULT %s", col_def, column->default_value);
      
    g_free(tmp);
  }
  else
    // Add default NULL clause only if the column can actually accept a NULL value.
    if (column->default_value_is_null && !column->not_null)
      col_def= str_g_append(col_def, " DEFAULT NULL");

  if (column->auto_inc)
    col_def= str_g_append(col_def, " AUTO_INCREMENT");

  if(!STR_EMPTY(column->comment))
  {
    tmp= str_g_replace(g_strdup(column->comment), "'", "\\'");
    col_def= str_g_append(col_def, " COMMENT '");
    col_def= str_g_append(col_def, tmp);
    col_def= str_g_append(col_def, "'");
    g_free(tmp);
  }

  return col_def;
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_dbm_get_sql_column_create_code(MYX_DBM_COLUMN_DATA *column, MYX_DBM_SERVER_VERSION *version)
{
  return dbm_get_sql_column_create_code(column, version, 1);
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_dbm_get_sql_column_create_code_no_pk(MYX_DBM_COLUMN_DATA *column, MYX_DBM_SERVER_VERSION *version)
{
  return dbm_get_sql_column_create_code(column, version, 0);
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_dbm_get_sql_index_create_code(MYX_DBM_INDEX_DATA *index, MYX_DBM_SERVER_VERSION *version)
{
  char *index_definition;
  char *columns_list, *index_type;
  unsigned int j;

  //Index kind
  switch (index->index_kind)
  {
    case MYX_DBM_IK_PRIMARY:
      index_definition= g_strdup("PRIMARY KEY ");
      break;
    case MYX_DBM_IK_INDEX:
      index_definition= g_strdup("INDEX ");
      break;
    case MYX_DBM_IK_UNIQUE:
      index_definition= g_strdup("UNIQUE INDEX ");
      break;
    case MYX_DBM_IK_FULLTEXT:
      index_definition= g_strdup("FULLTEXT INDEX ");
      break;
    case MYX_DBM_IK_SPATIAL:
      index_definition= g_strdup("SPATIAL INDEX ");
      break;
    default:
      index_definition= NULL;
  };

  // Index type.
  if ((((version->major_version == 4) && (version->minor_version >= 1)) ||
    (version->major_version > 4)) && (index->index_kind != MYX_DBM_IK_FULLTEXT))
  {
    switch (index->index_type)
    {
      case MYX_DBM_IT_BTREE:
        index_type= g_strdup(" USING BTREE");
        break;
      case MYX_DBM_IT_HASH:
        index_type= g_strdup(" USING HASH");
        break;
      case MYX_DBM_IT_RTREE:
        index_type= g_strdup(" USING RTREE");
        break;
      default:
        index_type= NULL;
    };
  }
  else
    index_type= NULL;

  if (index_definition != NULL)
  {
    if (index->index_kind != MYX_DBM_IK_PRIMARY)
       index_definition= str_g_append_and_free(index_definition, g_strdup_printf("`%s`", index->name));

    if (index_type)
      index_definition= str_g_append_and_free(index_definition, index_type);
  }
  else
    index_definition= NULL;

  if (index_definition != NULL)
  {
    // Collect columns in the index.
    index_definition= str_g_append(index_definition, "(");
    if(index->columns)
    {
      columns_list= g_strdup("");
      for(j= 0; j < index->columns_num; j++)
      {
        columns_list= str_g_append_and_free(columns_list, g_strdup_printf("`%s`", index->columns[j].name));

        // Index column length.
        if (index->columns[j].len && *index->columns[j].len)
          columns_list= str_g_append_and_free(columns_list, g_strdup_printf("(%s)", index->columns[j].len));

        if (j < index->columns_num-1)
          columns_list= str_g_append(columns_list, ", ");
      }
      index_definition= str_g_append(index_definition, columns_list);
      g_free(columns_list);
    }
    index_definition= str_g_append(index_definition, ")");
  }
  else
    index_definition= g_strdup("");

  return index_definition;
}

//----------------------------------------------------------------------------------------------------------------------

char * myx_dbm_get_sql_fk_create_code(MYX_DBM_FK_DATA *fk, MYX_DBM_SERVER_VERSION *version)
{
  char *ind_def;
  char *tmp, *tmp2;
  unsigned int j;
  int column_was_added= 0;

  if(fk->column_mapping)
  {
    tmp= g_strdup("");
    tmp2= g_strdup("");
    for(j= 0; j < fk->column_mapping_num; j++)
    {
      if(!fk->column_mapping[j].name || !fk->column_mapping[j].value ||
        (strlen(fk->column_mapping[j].name) == 0) || (strlen(fk->column_mapping[j].value) == 0))
      {
        column_was_added= 0;
        continue;
      }

//      if((column_was_added == 1) && (j < fk->column_mapping_num-1))
//      {
//        tmp= str_g_append(tmp, ", ");
//        tmp2= str_g_append(tmp2, ", ");
//      }

//      column_was_added= 1;

      if(column_was_added)
      {
        tmp= str_g_append_and_free(tmp, g_strdup_printf(", `%s`", fk->column_mapping[j].name));
        tmp2= str_g_append_and_free(tmp2, g_strdup_printf(", `%s`", fk->column_mapping[j].value));
      }
      else
      {
        tmp= str_g_append_and_free(tmp, g_strdup_printf("`%s`", fk->column_mapping[j].name));
        tmp2= str_g_append_and_free(tmp2, g_strdup_printf("`%s`", fk->column_mapping[j].value));
        column_was_added= 1;
      }
    }
    if (!STR_EMPTY(fk->reference_schema_name))
      ind_def= g_strdup_printf("CONSTRAINT `%s` FOREIGN KEY `%s` (%s)" _br "    REFERENCES `%s`.`%s` (%s)", fk->name, fk->name,
                               tmp, fk->reference_schema_name, fk->reference_table_name, tmp2);
    else
      ind_def= g_strdup_printf("CONSTRAINT `%s` FOREIGN KEY `%s` (%s)" _br "    REFERENCES `%s` (%s)", fk->name, fk->name,
                               tmp, fk->reference_table_name, tmp2);
    g_free(tmp);
    g_free(tmp2);

    //On delete
    if(fk->on_delete == MYX_DBM_FA_CASCADE)
      ind_def= str_g_append(ind_def, _br "    ON DELETE CASCADE");
    else if(fk->on_delete == MYX_DBM_FA_SET_NULL)
      ind_def= str_g_append(ind_def, _br "    ON DELETE SET NULL");
    else if(fk->on_delete == MYX_DBM_FA_RESTRICT)
      ind_def= str_g_append(ind_def, _br "    ON DELETE RESTRICT");
    else
      ind_def= str_g_append(ind_def, _br "    ON DELETE NO ACTION");

    //On update
    if(fk->on_update == MYX_DBM_FA_CASCADE)
      ind_def= str_g_append(ind_def, _br "    ON UPDATE CASCADE");
    else if(fk->on_update == MYX_DBM_FA_SET_NULL)
      ind_def= str_g_append(ind_def, _br "    ON UPDATE SET NULL");
    else if(fk->on_update == MYX_DBM_FA_RESTRICT)
      ind_def= str_g_append(ind_def, _br "    ON UPDATE RESTRICT");
    else
      ind_def= str_g_append(ind_def, _br "    ON UPDATE NO ACTION");
  }
  else
    ind_def= g_strdup("");

  return ind_def;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if the given storage engine is a merge engine.
 *
 * @param engine The engine to check.
 *
 * @return 1 if the engine is a merge engine otherwise 0.
 */
static int is_merge_engine(MYX_ENGINE *engine)
{
  return (engine != NULL) && ((strcasecmp(engine->name, "merge") == 0 || strcasecmp(engine->name, "mrg_myisam") == 0));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * @note: covered by unit tests
 */
char * myx_dbm_get_sql_option_create_code(MYX_DBM_TABLE_DATA *table, MYX_DBM_SERVER_VERSION *version)
{
  char *sql= NULL;
  char *tmp;

  // Storage engine.
  if (table->storage_engine != NULL)
  {
    if ((version->major_version >= 5) || ((version->major_version == 4) && (version->minor_version > 0)))
      sql= str_g_append_and_free(sql, g_strdup_printf("ENGINE = %s" _br, table->storage_engine->name));
    else
      sql= str_g_append_and_free(sql, g_strdup_printf("TYPE = %s" _br, table->storage_engine->name));
  };

  //auto_inc
  if(!STR_EMPTY(table->next_auto_inc))
      sql= str_g_append_and_free(sql, g_strdup_printf("AUTO_INCREMENT = %s" _br, table->next_auto_inc));

  //password
  if(!STR_EMPTY(table->password))
      sql= str_g_append_and_free(sql, g_strdup_printf("PASSWORD = '%s'" _br, table->password));

  //delay_key_write
  if(table->delay_key_write == 1)
    sql= str_g_append(sql, "DELAY_KEY_WRITE = 1" _br);

  //Charset
  if(!STR_EMPTY(table->charset))
    {
      sql= str_g_append_and_free(sql, g_strdup_printf("CHARACTER SET %s", table->charset));

      if(!STR_EMPTY(table->collation) && ((version->major_version >= 5) || ((version->major_version >= 4) && (version->minor_version >= 1))))
          sql= str_g_append_and_free(sql, g_strdup_printf(" COLLATE %s", table->collation));

      sql= str_g_append(sql, _br);
    }

  //comment
  if(!STR_EMPTY(table->comment))
    {
      tmp= str_g_replace(g_strdup(table->comment), "'", "\\'");
      sql= str_g_append_and_free(sql, g_strdup_printf("COMMENT = '%s'" _br, tmp));
      g_free(tmp);
    }

  //merge_union
  if (!STR_EMPTY(table->merge_union) && is_merge_engine(table->storage_engine))
      sql= str_g_append_and_free(sql, g_strdup_printf("UNION = %s" _br, table->merge_union));

  //merge_union
  if (is_merge_engine(table->storage_engine))
  {
    if(table->merge_insert == MYX_DBM_TMI_NO)
      sql= str_g_append(sql, "INSERT_METHOD = NO" _br);
    else if(table->merge_insert == MYX_DBM_TMI_FIRST)
      sql= str_g_append(sql, "INSERT_METHOD = FIRST" _br);
    else if(table->merge_insert == MYX_DBM_TMI_LAST)
      sql= str_g_append(sql, "INSERT_METHOD = LAST" _br);
  }

  
  //table_data_dir
  if(!STR_EMPTY(table->table_data_dir))
    {
      tmp= str_g_replace(g_strdup(table->table_data_dir), "'", "\\'");
      sql= str_g_append_and_free(sql, g_strdup_printf("DATA DIRECTORY = '%s'" _br, tmp));
      g_free(tmp);
    }

  //table_data_dir
  if(!STR_EMPTY(table->table_index_dir))
    {
      tmp= str_g_replace(g_strdup(table->table_index_dir), "'", "\\'");
      sql= str_g_append_and_free(sql, g_strdup_printf("INDEX DIRECTORY = '%s'" _br, tmp));
      g_free(tmp);
    }

  //pack_keys
  if(table->pack_keys == MYX_DBM_TPK_NONE)
    sql= str_g_append(sql, "PACK_KEYS = 0" _br);
  else if(table->pack_keys == MYX_DBM_TPK_ALL)
    sql= str_g_append(sql, "PACK_KEYS = 1" _br);

  //raid_type
  if(table->raid_type == MYX_DBM_TRT_STRIPED)
  {
    sql= str_g_append(sql, "RAID_TYPE = STRIPED" _br);

    //raid_chunks
    if(!STR_EMPTY(table->raid_chunks))
        sql= str_g_append_and_free(sql, g_strdup_printf("  RAID_CHUNKS = %s" _br, table->raid_chunks));

    //raid_chunk_size
    if(!STR_EMPTY(table->raid_chunk_size))
        sql= str_g_append_and_free(sql, g_strdup_printf("  RAID_CHUNKSIZE = %s" _br, table->raid_chunk_size));
  }

  //checksum
  if(table->checksum == 1)
    sql= str_g_append(sql, "CHECKSUM = 1" _br);

  if(table->row_format == MYX_DBM_TRF_DYNAMIC)
    sql= str_g_append(sql, "ROW_FORMAT = DYNAMIC" _br);
  else if(table->row_format == MYX_DBM_TRF_FIXED)
    sql= str_g_append(sql, "ROW_FORMAT = FIXED" _br);
  else if(table->row_format == MYX_DBM_TRF_COMPRESSED)
    sql= str_g_append(sql, "ROW_FORMAT = COMPRESSED" _br);
  else if(table->row_format == MYX_DBM_TRF_COMPACT)
    sql= str_g_append(sql, "ROW_FORMAT = COMPACT" _br);
  else if(table->row_format == MYX_DBM_TRF_REDUNDANT)
    sql= str_g_append(sql, "ROW_FORMAT = REDUNDANT" _br);

  //avg_row_length
  if(!STR_EMPTY(table->avg_row_length))
      sql= str_g_append_and_free(sql, g_strdup_printf("AVG_ROW_LENGTH = %s" _br, table->avg_row_length));

  //min_rows
  if(!STR_EMPTY(table->min_rows))
      sql= str_g_append_and_free(sql, g_strdup_printf("MIN_ROWS = %s" _br, table->min_rows));

  //min_rows
  if(!STR_EMPTY(table->max_rows))
      sql= str_g_append_and_free(sql, g_strdup_printf("MAX_ROWS = %s" _br, table->max_rows));

  //connection for federated
  if(!STR_EMPTY(table->federated_connection))
      sql= str_g_append_and_free(sql, g_strdup_printf("CONNECTION = %s" _br, table->federated_connection));

  // Remove last line break.
  if (sql != NULL)
    sql[strlen(sql)-strlen(_br)]= 0;

  return sql;
}

char * myx_dbm_get_sql_table_create_code(MYX_DBM_TABLE_DATA *table, MYX_DBM_SERVER_VERSION *version)
{
  char *sql;
  unsigned int i;
  char *tmp;

  sql= g_strdup_printf("CREATE TABLE `%s`.`%s` (", table->schema, table->name);
  
  //build columns
  for(i= 0; i < table->columns_num; i++)
  {
    if (i > 0)
      sql= str_g_append(sql, ",");

    sql= str_g_append_and_free(str_g_append(sql, _br "  "), 
      myx_dbm_get_sql_column_create_code(table->columns+i, version));
  }

  //build indices
  for(i= 0; i < table->indices_num; i++)
  {
    //if(strcmp2(table->indices[i].name, "PRIMARY") != 0)
      sql= str_g_append_and_free(str_g_append(sql, "," _br "  "), 
        myx_dbm_get_sql_index_create_code(table->indices+i, version));
  }

  //build FKs
  for(i= 0; i < table->fks_num; i++)
  {
    sql= str_g_append_and_free(str_g_append(sql, "," _br "  "), 
      myx_dbm_get_sql_fk_create_code(table->fks+i, version));    
  }

  sql= str_g_append(sql, _br ")");

  //build options
  tmp= myx_dbm_get_sql_option_create_code(table, version);
  if(!STR_EMPTY(tmp))
  {
    sql= str_g_append(sql, _br);
    sql= str_g_append_and_free(sql, tmp);
  }


  //terminate with ;
  sql= str_g_append(sql, ";" _br);

  return sql;
}

/**
 * @note: covered by unit tests.
 */

MYX_PUBLIC_FUNC char * myx_dbm_get_table_sql_diff(MYX_DBM_TABLE_DATA *existing_table, MYX_DBM_TABLE_DATA *altered_table,
                                                  MYX_DBM_SERVER_VERSION *version, MYX_LIB_ERROR *error_code)
{
  unsigned int i, j;
  char *sql= NULL, *alter_spec= NULL, *alter_options= NULL;
  char *tmp, *tmp2, *tmp3;
  char found, name_changed, column_changed, index_changed, fk_changed, pk_changed;
  MYX_DBM_COLUMN_DATA *column, *ex_column;
  MYX_DBM_INDEX_DATA *index, *ex_index, *ex_pk;
  MYX_DBM_FK_DATA *fk, *ex_fk;
  unsigned int collation_added= 0;
  char *pre_sql= NULL;

#define NAMECMP(a,b) (a&&b?strcmp(a,b):(a?-1:1))

  *error_code= MYX_NO_ERROR;

  //if the table does not exists yet, write CREATE TABLE statment
  if(existing_table==NULL)
  {
    sql= myx_dbm_get_sql_table_create_code(altered_table, version);
  }
  else
  {
    //Check table name change
    if(NAMECMP(existing_table->name, altered_table->name) != 0)
    {
      alter_spec= g_strdup_printf("," _br " RENAME TO `%s`.`%s`", altered_table->schema, altered_table->name);
    }

    //Check column changes and drops
    pk_changed= 0;
    for(i= 0; i < existing_table->columns_num; i++)
    {
      name_changed= 0;
      column= NULL;
      ex_column= NULL;

      //find altered column in existing table
      for(j= 0; j < altered_table->columns_num; j++)
      {
        if((NAMECMP(altered_table->columns[j].name, existing_table->columns[i].name) == 0) ||
          (NAMECMP(altered_table->columns[j].original_name, existing_table->columns[i].name) == 0))
        {
          if(NAMECMP(altered_table->columns[j].name, existing_table->columns[i].name) != 0)
            name_changed= 1;

          column= altered_table->columns+j;
          ex_column= existing_table->columns+i;
          break;
        }
      };

      //if column is not found anymore, drop it
      if(!column)
      {
        tmp= g_strdup_printf("," _br " DROP COLUMN `%s`", existing_table->columns[i].name);
        alter_spec= str_g_append(alter_spec, tmp);
        g_free(tmp);
      }
      // if column is found, check all settings
      else
      {
        column_changed= 0;

        //Check primary key
        if(column->primary_key != ex_column->primary_key)
          pk_changed= 1;

        //Check NOT NULL
        if(column->not_null != ex_column->not_null)
          column_changed= 1;

        //Check AUTO_INCREMENT
        if(column->auto_inc != ex_column->auto_inc)
          column_changed= 1;

        //Check comment
        if(strcmp3(column->comment, ex_column->comment) != 0)
          column_changed= 1;

        if(((strcmp3(column->charset ? column->charset : "", ex_column->charset ? column->charset : "") != 0) ||
          (strcmp3(column->collation ? column->collation : "", ex_column->collation ? ex_column->collation : "") != 0)) &&
          !((column->charset == NULL) && (strcmp3(altered_table->charset, ex_column->charset) == 0) &&
          (strcmp3(altered_table->collation ? altered_table->collation : "", ex_column->collation ? ex_column->collation : "") == 0)))
          column_changed= 1;

        //Check DefaultValue
        //if there is no default value
        if (column->default_value_is_null != ex_column->default_value_is_null)
        {
          column_changed= 1;
        }
        else if (STR_EMPTY(column->default_value) || column->default_value_is_null)
        {
          //and the default value is different than 0, "", 0-datetime and 0-date
          if((ex_column->default_value || column->default_value_is_null) &&
            (strcmp2(ex_column->default_value, "0") != 0) &&
            (strcmp2(ex_column->default_value, "") != 0) &&
            (strcmp2(ex_column->default_value, "0000-00-00 00:00:00") != 0) &&
            (strcmp2(ex_column->default_value, "0000-00-00") != 0) &&
             !ex_column->default_value_is_null)
            column_changed= 1;
        }
        else
        {
          if(strcmp2(column->default_value, ex_column->default_value) != 0)
          {
            //ignore BOOLEAN / TINYINT: 0=False, 1=True
            if(!((strcasecmp(column->datatype_name, "tinyint") == 0) &&
              (((strcasecmp(column->datatype_name, "true") == 0) && (strcmp2(ex_column->default_value, "1") == 0)) ||
              ((strcasecmp(column->datatype_name, "false") == 0) && (strcmp2(ex_column->default_value, "0") == 0)))))
            {
              column_changed= 1;
            }
          }
        }

        //Check Datatype
        if(column->datatype_pointer != ex_column->datatype_pointer)
        {
          if ((column->datatype_pointer == NULL) || (ex_column->datatype_pointer == NULL))
          {
            column_changed= 1;
          }
          else
          {
            //Check datatype synonym_group
            if (!((column->datatype_pointer->synonym_group == ex_column->datatype_pointer->synonym_group) &&
              (column->datatype_pointer->synonym_group > 0)))
            {
              // Check if there is an user defined datatype
              // Ignore the MySQL silent column changes Varchar(<4) -> char(<4)
              if(((strcasecmp(column->datatype_name, "VARCHAR") == 0) &&
                (strcasecmp(ex_column->datatype_name, "CHAR") == 0)) ||
                ((strcasecmp(column->datatype_name, "CHAR") == 0) &&
                (strcasecmp(ex_column->datatype_name, "VARCHAR") == 0)))
              {
                tmp= g_strdup(column->datatype_params+1);
                tmp3= g_strstr_len(tmp, 64, ")");
                tmp3[0]= 0;

                tmp2= g_strdup(ex_column->datatype_params+1);
                tmp3= g_strstr_len(tmp2, 64, ")");
                tmp3[0]= 0;

                if((atoi(tmp)>4) || (atoi(tmp2)>4))
                  column_changed= 1;

                g_free(tmp2);
                g_free(tmp);
              }
              //Ignore tinyint(1)=BOOL
              else if((strcasecmp(column->datatype_name, "BOOL") == 0) &&
                (strcasecmp(ex_column->datatype_name, "TINYINT") == 0) &&
                (strcasecmp(column->datatype_params, "(1)") == 0))
              {
                //This is ok.
              }
              else
                column_changed= 1;
            }
          }
        }

        //Check Parameters
        if (!column_changed)
        {
          unsigned char p1_assigned= (column->datatype_params != NULL) && (*column->datatype_params != '\0');
          unsigned char p2_assigned= (ex_column->datatype_params != NULL) && (*ex_column->datatype_params != '\0');

          if (p1_assigned || p2_assigned)
            if (p1_assigned && p2_assigned)
            {
              if (strcasecmp(column->datatype_params, ex_column->datatype_params) != 0)
              {
                if ((strcasecmp(ex_column->datatype_name, "INT") == 0) && ((strcasecmp(ex_column->datatype_params, "(10)") == 0) ||
                    (strcasecmp(ex_column->datatype_params, "(11)") == 0)) && ((strcasecmp(column->datatype_name, "INTEGER") == 0) ||
                    (strcasecmp(column->datatype_name, "INT") == 0)))
                {
                  // Tolerate int without params.
                  //Check unsigned ?
                }
                else
                  if ((strcasecmp(ex_column->datatype_name, "BIGINT") == 0) &&
                    (strcasecmp(ex_column->datatype_params, "(20)") == 0) &&
                    (strcasecmp(column->datatype_name, "BIGINT") == 0))
                  {
                    //tolerate bigint(20) = bigint
                  }
                  else
                    if ((strcasecmp(ex_column->datatype_name, "TINYINT") == 0) &&
                      (strcasecmp(ex_column->datatype_params, "(1)") == 0) &&
                      (strcasecmp(column->datatype_name, "BOOL") == 0))
                    {
                      //tolerate tinyint(1)=BOOL
                    }
                    else
                      column_changed= 1;
              };
            }
            else
              column_changed= 1; // A data type param was added or removed.
        };

        //Check flags
        if(!column_changed)
        {
          //The number of flags has to be the same
          if(column->datatype_flags_num != ex_column->datatype_flags_num)
            column_changed= 1;
          else
          {
            //check each flag
            for(j= 0; j<column->datatype_flags_num; j++)
              if(strcasecmp(column->datatype_flags[j], ex_column->datatype_flags[j]) != 0)
              {
                column_changed= 1;
                break;
              }
          }
        }

        if(name_changed || column_changed)
        {
          tmp2= myx_dbm_get_sql_column_create_code_no_pk(column, version);

          if(name_changed) 
          {
            tmp= g_strdup_printf("," _br " CHANGE COLUMN `%s` %s", column->original_name, tmp2);
          }
          else
          {
            tmp= g_strdup_printf("," _br " MODIFY COLUMN %s", tmp2);
          }

          alter_spec= str_g_append(alter_spec, tmp);

          g_free(tmp);
          g_free(tmp2);
        }
      }
    }

    if(altered_table->columns_num>0)
    {
      char* previous_column_name;

      //Add new columns
      for(i= 0; i < altered_table->columns_num; i++)
      {
        column= altered_table->columns+i;
        ex_column= NULL;

        for(j= 0; j < existing_table->columns_num; j++)
        {
          // Compare original name of the column, in case it was renamed *and*
          // the new name, in case it had been dropped and recreated with the same name.
          if ((NAMECMP(altered_table->columns[i].original_name, existing_table->columns[j].name) == 0) ||
            (NAMECMP(altered_table->columns[i].name, existing_table->columns[j].name) == 0))
          {
            ex_column= existing_table->columns+j;
            break;
          }
        }

        if(!ex_column)
        {
          tmp2= myx_dbm_get_sql_column_create_code(column, version);

          if(existing_table->columns_num == 0 || previous_column_name == NULL)
            tmp= g_strdup_printf("," _br " ADD COLUMN %s FIRST", tmp2);
          else
            tmp= g_strdup_printf("," _br " ADD COLUMN %s AFTER `%s`", tmp2, previous_column_name);

          alter_spec= str_g_append(alter_spec, tmp);
          g_free(tmp);
          g_free(tmp2);
        }
        previous_column_name= column->name;
      }
    }

    // Check primary key columns.
    // First find primary key in existing table.
    ex_pk= NULL;
      for (j= 0; j < existing_table->indices_num; j++)
    {
      ex_index= existing_table->indices + j;
      if (NAMECMP(ex_index->name, "PRIMARY") == 0)
      {
        ex_pk= ex_index;
        break;
      };
    };

    if (altered_table->indices_num > 0)
    {
      int new_pk= 0;
      for (i= 0; i < altered_table->indices_num; i++)
      {
        index= altered_table->indices + i;
        if (NAMECMP(index->name, "PRIMARY") == 0)
        {
          // We need to know if there is a new primary key in case the old one must be dropped.
          new_pk= 1;
          if (ex_pk != NULL)
          {
            // Number of participating columns changed?
            if (index->columns_num != ex_pk->columns_num)
              pk_changed= 1;
            else
              // Index type changed?
              if (index->index_type != ex_index->index_type)
                pk_changed= 1;
              else
              {
                // Check all columns in the index.
                for (j= 0; j < index->columns_num; j++)
                {
                  if ((NAMECMP(index->columns[j].name, ex_index->columns[j].name) != 0) ||
                    (strcmp3(index->columns[j].len, ex_index->columns[j].len) != 0))
                  {
                    pk_changed= 1;
                    break;
                  };
                };
              };
          }
          else
            // Set to changed if there wasn't a PK before but now is.
            pk_changed= 1;

          break;
        }
      };

      // If there is no new primary key but we had an old one then set changed flag so the old one gets dropped.
      if (!new_pk)
          pk_changed= 1;
      }
    else
    {
      // Altered table has no indices at all.
      if (ex_pk != NULL)
      {
        // Existing table had a PK - need to add DROP PRIMARY KEY (below).
        pk_changed= 1;

        // Remove column attributes.
        for (i= 0; i < altered_table->columns_num; i++)
        altered_table->columns[i].primary_key= 0;
      }
    }

    if (pk_changed)
    {
      // Drop primary key if the existing table had one.
      if (ex_pk)
        alter_spec= str_g_append(alter_spec, "," _br " DROP PRIMARY KEY");

      // Use index instead of PK flag (bugfix for #7136).
      tmp3= NULL;
      for(i= 0; i < altered_table->indices_num; i++)
      {
        if (NAMECMP(altered_table->indices[i].name, "PRIMARY") == 0)
        {
          tmp= myx_dbm_get_sql_index_create_code(index, version);
          alter_spec= str_g_append_and_free(alter_spec, g_strdup_printf("," _br " ADD %s", tmp));
          break;
        };
      };
    };

    // Other indices.
    // Check index changes and drops.
    for(i= 0; i<existing_table->indices_num; i++)
    {
      ex_index= existing_table->indices+i;
      index= NULL;
      index_changed= 0;

      for(j= 0; j<altered_table->indices_num; j++)
        if(NAMECMP(ex_index->name, altered_table->indices[j].name) == 0)
        {
          index= altered_table->indices+j;
          break;
        }

      if(!index)
      {
        if (NAMECMP(ex_index->name, "PRIMARY") != 0)
          alter_spec= str_g_append_and_free(alter_spec,
            g_strdup_printf(_br ", DROP INDEX `%s`", ex_index->name));
      }
      else
      {
        //Primary key has been checked already
        if(NAMECMP(ex_index->name, "PRIMARY") == 0)
          continue;

        //Index kind and type
        if((index->index_kind != ex_index->index_kind) ||
          (index->index_type != ex_index->index_type))
          index_changed= 1;

        //Number of columns
        // index->columns_num == 0 means that all columns of the index were deleted
        // and we should not try to recreate the index
        if((!index_changed)&&(index->columns_num != 0)&&(index->columns_num != ex_index->columns_num))
          index_changed= 1;

        //Check all columns
        if(!index_changed)
        {
          for(j= 0; j<index->columns_num; j++)
          {
            if((NAMECMP(index->columns[j].name, ex_index->columns[j].name) != 0) ||
              (strcmp3(index->columns[j].len, ex_index->columns[j].len) != 0))
            {
              index_changed= 1;
              break;
            }
          }
        }

        //If changed, drop and add again
        if(index_changed)
        {
          alter_spec= str_g_append_and_free(alter_spec, 
            g_strdup_printf("," _br " DROP INDEX `%s`", ex_index->name));

          tmp= myx_dbm_get_sql_index_create_code(index, version);
          alter_spec= str_g_append_and_free(alter_spec,
            g_strdup_printf("," _br " ADD %s", tmp));
          g_free(tmp);
        }
      }
    }

    //Check new indices
    for(i= 0; i<altered_table->indices_num; i++)
    {
      index= altered_table->indices+i;
      ex_index= NULL;

      for(j= 0; j<existing_table->indices_num; j++)
        if(NAMECMP(index->name, existing_table->indices[j].name) == 0)
        {
          ex_index= existing_table->indices+i;
          break;
        }

      //Add index if it does not already exist
      if(!ex_index)
      {
        if (NAMECMP(index->name, "PRIMARY") != 0)
        {
          tmp= myx_dbm_get_sql_index_create_code(index, version);
          alter_spec= str_g_append_and_free(alter_spec, 
            g_strdup_printf("," _br " ADD %s", tmp));
          g_free(tmp);
        }
      }
    }
    
    
    // Foreign Keys

    //Check fk changes and drops
    for(i= 0; i<existing_table->fks_num; i++)
    {
      ex_fk= existing_table->fks+i;
      fk= NULL;
      fk_changed= 0;

      for(j= 0; j<altered_table->fks_num; j++)
        if(NAMECMP(ex_fk->name, altered_table->fks[j].name) == 0)
        {
          fk= altered_table->fks+j;
          break;
        }

      if(!fk)
      {
        alter_spec= str_g_append_and_free(alter_spec,
          g_strdup_printf("," _br " DROP FOREIGN KEY `%s`", ex_fk->name));
      }
      else
      {
        // table name
        if (strcmp3(ex_fk->reference_schema_name, fk->reference_schema_name)!=0 ||
            strcmp2(ex_fk->reference_table_name, fk->reference_table_name)!=0)
          fk_changed= 1;
        
        // actions
        if (ex_fk->on_delete != fk->on_delete || 
            ex_fk->on_update != fk->on_update)
          fk_changed= 1;
        
        if (ex_fk->column_mapping_num != fk->column_mapping_num)
          fk_changed= 1;
        else if (!fk_changed)
        {
          for (j= 0; j < fk->column_mapping_num; j++)
          {
            if (strcmp2(ex_fk->column_mapping[j].name,
                        fk->column_mapping[j].name)!=0 ||
                strcmp2(ex_fk->column_mapping[j].value,
                        fk->column_mapping[j].value)!=0)
            {
              fk_changed= 1;
              break;
            }
          }
        }

        //If changed, drop and add again
        if(fk_changed)
        {
          pre_sql= str_g_append_and_free(pre_sql,
            g_strdup_printf("ALTER TABLE `%s`.`%s`" _br
            " DROP FOREIGN KEY `%s`;" _br _br,
            existing_table->schema, existing_table->name, ex_fk->name));

          tmp= myx_dbm_get_sql_fk_create_code(fk, version);
          alter_spec= str_g_append_and_free(alter_spec, 
            g_strdup_printf("," _br " ADD %s", tmp));
          g_free(tmp);
        }
      }
    }

    //Check new fks
    for(i= 0; i<altered_table->fks_num; i++)
    {
      fk= altered_table->fks+i;
      ex_fk= NULL;

      for(j= 0; j<existing_table->fks_num; j++)
        if(strcmp2(fk->name, existing_table->fks[j].name) == 0)
        {
          ex_fk= existing_table->fks+i;
          break;
        }

      //Add fk if it does not already exist
      if(!ex_fk)
      {
        tmp= myx_dbm_get_sql_fk_create_code(fk, version);
        alter_spec= str_g_append_and_free(alter_spec, 
          g_strdup_printf("," _br " ADD %s", tmp));
        g_free(tmp);
      }
    }

    // Check table option changes.
    // Storage engine. Check the actual engine strings, because the engine pointer might differ, although it is
    // the same engine, when called from Delphi (the passed-in pointer is allocated dynamically).
    if (altered_table->storage_engine != NULL)
    {
      if ((existing_table->storage_engine == NULL) ||
        (strcasecmp(existing_table->storage_engine->name, altered_table->storage_engine->name) != 0))
      {
        if ((version->major_version >= 5) || ((version->major_version == 4) && (version->minor_version > 0)))
          alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "ENGINE = %s",
            altered_table->storage_engine->name));
        else
          alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "TYPE = %s",
            altered_table->storage_engine->name));
      };
    };

    //auto_inc
    if((strcmp3(existing_table->next_auto_inc, altered_table->next_auto_inc) != 0) &&
      !STR_EMPTY(altered_table->next_auto_inc))
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "AUTO_INCREMENT = %s",
                                                      STR_OR_NULL(altered_table->next_auto_inc)));

    //password
    if (strcmp3(existing_table->password, altered_table->password) != 0)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "PASSWORD = '%s'",
                                                      STR_OR_NULL(altered_table->password)));

    //delay_key_write
    if (existing_table->delay_key_write != altered_table->delay_key_write)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "DELAY_KEY_WRITE = %i",
                                                      altered_table->delay_key_write));

    //charset
    if((strcmp3(existing_table->charset, altered_table->charset)!=0) &&
      !STR_EMPTY(altered_table->charset))
    {
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "CHARACTER SET %s",
                                                      altered_table->charset));

      collation_added= 1;
    }

    //collation
    if ((strcmp3(existing_table->collation, altered_table->collation) != 0) &&
      !STR_EMPTY(altered_table->collation) &&
      ((version->major_version >= 5) || ((version->major_version >= 4) && (version->minor_version >= 1))))
    {
      // if the charset has not been added yet, add it now
      if ((collation_added == 0) && !STR_EMPTY(altered_table->charset))
      {
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "CHARACTER SET %s",
                                                    altered_table->charset));

        collation_added= 1;
      }
      else if ((collation_added == 0) && STR_EMPTY(altered_table->charset))
      {
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "CHARACTER SET Default"));

        collation_added= 1;
      }


      if (collation_added == 1)
      {
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(" COLLATE %s", 
                                                      altered_table->collation));
      }
    }

    //comment
    if(strcmp3(existing_table->comment, altered_table->comment)!=0)
    {
      if(altered_table->comment)
        tmp= str_g_replace(g_strdup(altered_table->comment), "'", "\\'");
      else
        tmp= g_strdup("");
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "COMMENT = '%s'", tmp));
      g_free(tmp);
    }

    //merge_union
    if (is_merge_engine(altered_table->storage_engine))
    {
      if(strcmp3(altered_table->merge_union, existing_table->merge_union) != 0)
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "UNION = %s", altered_table->merge_union));

      //merge_insert
      if (altered_table->merge_insert != existing_table->merge_insert)
      {        
        switch (altered_table->merge_insert)
        {
        case MYX_DBM_TMI_NO: alter_options= str_g_append(alter_options, _br "INSERT_METHOD = NO"); break;
        case MYX_DBM_TMI_FIRST: alter_options= str_g_append(alter_options, _br "INSERT_METHOD = FIRST"); break;
        case MYX_DBM_TMI_LAST: alter_options= str_g_append(alter_options, _br "INSERT_METHOD = LAST"); break;
        }
      }
    }

    //table_data_dir
    if(strcmp3(altered_table->table_data_dir, existing_table->table_data_dir)!=0)
    {
      tmp= str_g_replace(g_strdup(altered_table->table_data_dir), "'", "\\'");
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "DATA DIRECTORY = '%s'", tmp));
      g_free(tmp);
    }
    
    //table_index_dir
    if(strcmp3(altered_table->table_index_dir, existing_table->table_index_dir)!=0)
    {
      tmp= str_g_replace(g_strdup(altered_table->table_index_dir), "'", "\\'");
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "INDEX DIRECTORY = '%s'", tmp));
      g_free(tmp);
    }

    //pack_keys
    if(altered_table->pack_keys != existing_table->pack_keys)
    {
      if (altered_table->pack_keys == MYX_DBM_TPK_NONE)
        alter_options= str_g_append(alter_options, _br "PACK_KEYS = 0");
      else if(altered_table->pack_keys == MYX_DBM_TPK_ALL)
        alter_options= str_g_append(alter_options, _br "PACK_KEYS = 1");
    }

    //raid_type
    if(altered_table->raid_type != existing_table->raid_type)
    {
      if (altered_table->raid_type == MYX_DBM_TRT_STRIPED)
      {
        alter_options= str_g_append(alter_options, _br "RAID_TYPE = STRIPED");

        //raid_chunks
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "  RAID_CHUNKS = %s", altered_table->raid_chunks));
      
        //raid_chunk_size
        alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "  RAID_CHUNKSIZE = %s", altered_table->raid_chunk_size));
      }
      //else
        //XXX warn that raid can't be disabled?
    }

    //checksum
    if(altered_table->checksum != existing_table->checksum)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "CHECKSUM = %i", altered_table->checksum));

    if((altered_table->row_format != existing_table->row_format) &&
      altered_table->row_format != MYX_DBM_TRF_DEFAULT)
    {
      switch (altered_table->row_format)
      {
      case MYX_DBM_TRF_DYNAMIC:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = DYNAMIC");
        break;
      case MYX_DBM_TRF_FIXED:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = FIXED");
        break;
      case MYX_DBM_TRF_COMPRESSED:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = COMPRESSED");
        break;
      case MYX_DBM_TRF_REDUNDANT:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = REDUNDANT");
        break;
      case MYX_DBM_TRF_COMPACT:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = COMPACT");
        break;        
      case MYX_DBM_TRF_DEFAULT:
        alter_options= str_g_append(alter_options, _br "ROW_FORMAT = DEFAULT");
        break;
      }
    }

    //avg_row_length
    if(strcmp3(altered_table->avg_row_length, existing_table->avg_row_length)!=0)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "AVG_ROW_LENGTH = %s", altered_table->avg_row_length));
    
    //min_rows
    if(strcmp3(altered_table->min_rows, existing_table->min_rows)!=0)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "MIN_ROWS = %s", altered_table->min_rows));
    
    //min_rows
    if(strcmp3(altered_table->max_rows, existing_table->max_rows)!=0)
      alter_options= str_g_append_and_free(alter_options, g_strdup_printf(_br "MAX_ROWS = %s", altered_table->max_rows));


    //Append alter_spec and alter_options to sql

    if((alter_spec) || (alter_options))
    {
      tmp= NULL;

      if(alter_spec)
        tmp= str_g_append(tmp, alter_spec);

      if(alter_options)
        tmp= str_g_append_and_free(tmp, g_strdup_printf(_br ", %s", alter_options+strlen(_br)));

      sql= g_strdup_printf("ALTER TABLE `%s`.`%s` %s;" _br, existing_table->schema, existing_table->name, tmp+strlen(_br)+2);
      g_free(tmp);

      if(alter_spec)
        g_free(alter_spec);
      if(alter_options)
        g_free(alter_options);

      if (pre_sql)
        sql= str_g_append_and_free(pre_sql, sql);
    }
    else
      sql= g_strdup("");
  }

  return sql;
}

MYX_DBM_SERVER_VERSION * myx_dbm_retrieve_server_version(MYSQL *mysql)
{
  MYX_DBM_SERVER_VERSION *version= g_malloc(sizeof(MYX_DBM_SERVER_VERSION));

  version->major_version= myx_get_mysql_major_version(mysql);
  version->minor_version= myx_get_mysql_minor_version(mysql);

  return version;
}

void myx_dbm_free_server_version(MYX_DBM_SERVER_VERSION *version)
{
  g_free(version);
}

//----------------------------------------------------------------------------------------------------------------------

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


MYX_DBM_STORED_PROCEDURE_DATA *myx_dbm_get_sp_data(MYSQL *mysql, const char *catalog_name, const char *schema_name,
  const char *sp_name, MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type, char quote_char, int add_sql_mode)
{
  MYX_DBM_STORED_PROCEDURE_DATA *spdata= NULL;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;

  const char *q;
  char *query;

  if (sp_type == MSPT_PROCEDURE)
    q= "SHOW CREATE PROCEDURE `%s`.`%s`";
  else
    q= "SHOW CREATE FUNCTION `%s`.`%s`";

  query= g_strdup_printf(q, schema_name, sp_name);

  if (!(myx_mysql_query(mysql, query)))
  {
    MYSQL_RES *res= mysql_store_result(mysql);
    if (res != NULL)
    {
      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        int fi[5];
        char* definition;

        // Depending on the version of the server there might be different columns.
        num_fields= mysql_num_fields(res);
        fields= mysql_fetch_fields(res);

        build_field_subst(sp_show_create, sp_show_create_end, fields,fields + num_fields, fi);

        spdata= g_new0(MYX_DBM_STORED_PROCEDURE_DATA, 1);
        spdata->catalog= g_strdup(catalog_name);
        spdata->schema= g_strdup(schema_name);
        spdata->sp_type= sp_type;

        spdata->name= myx_convert_dbstr_utf8(mysql, row[0], -1);

        // SPs and SFs are returned with the quoting style which was used to create them.
        // This makes it necessary to temporarily set the sql mode to the same state
        // as it was when the function/procedure was created.
        if (fi[3] != -1)
          definition= myx_convert_dbstr_utf8(mysql, row[fi[3]], -1);
        else
          definition= (fi[4] == -1) ? 0 : myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);

        if (add_sql_mode == 1)
        {
          // Finally put the pieces together.
          // Note: we assume a delimiter $$ here as this is what is currently hardcoded everywhere.
          spdata->definition= g_strdup_printf("/*!50003 SET @TEMP_SQL_MODE=@@SQL_MODE, SQL_MODE='%s' */ $$\n%s "
            "$$\n/*!50003 SET SESSION SQL_MODE=@TEMP_SQL_MODE */ ", row[fi[2]], definition);
          g_free(definition);
        }
        else
          spdata->definition= definition;
      }
    }
    mysql_free_result(res);
  }
  g_free(query);

  return spdata;
}

//----------------------------------------------------------------------------------------------------------------------

// return 0 when OK
MYX_LIB_ERROR myx_dbm_drop_sp(MYSQL *mysql, const char *catalog_name, const char *schema_name, const char *sp_name, MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type)
{
  const char *q;
  char *query;
  bigint affected_rows;

  MYX_LIB_ERROR error;

  if (sp_type == MSPT_PROCEDURE)
    q= "DROP PROCEDURE `%s`.`%s`";
  else
    q= "DROP FUNCTION `%s`.`%s`";

  query= g_strdup_printf(q, schema_name, sp_name);

  myx_query_execute_direct(mysql, query, &error, &affected_rows);
  g_free(query);
  
  return error;
}

// return 0 when OK
MYX_LIB_ERROR myx_dbm_drop_view(MYSQL *mysql, const char *catalog_name, const char *schema_name, const char *view_name)
{
  char *query;
  MYX_LIB_ERROR error;
  bigint affected_rows;

  query= g_strdup_printf("DROP VIEW IF EXISTS `%s`.`%s`", schema_name, view_name);

  myx_query_execute_direct(mysql, query, &error, &affected_rows);
  g_free(query);
  
  return error;
}


void myx_dbm_free_sp_data(MYX_DBM_STORED_PROCEDURE_DATA *sp_data)
{
  g_free(sp_data->catalog);
  g_free(sp_data->schema);
  g_free(sp_data->name);
  g_free(sp_data->definition);
  g_free(sp_data);
}


MYX_LIB_ERROR myx_dbm_create_view(MYSQL *mysql, const char *catalog, const char *schema,
                                  const char *view_name, const char *select_sql)
{
  char *query= g_strdup_printf("CREATE VIEW `%s`.%s AS %s", schema, view_name,
                               select_sql);

  MYX_LIB_ERROR error;
  bigint affected_rows;

  myx_query_execute_direct(mysql, query, &error, &affected_rows);
  g_free(query);
  
  return error;
}



static const char * view_show_create[]=
{
  "Table",       // 0 (col 1)
  "Create Table"// 1 (col 2)
};
static const char ** view_show_create_end=
                 view_show_create + sizeof(view_show_create)/sizeof(char*);

static const char * view_show_create_new[]=
{
  "View",       // 0 (col 1)
  "Create View"// 1 (col 2)
};
static const char ** view_show_create_new_end=
                 view_show_create_new + sizeof(view_show_create_new)/sizeof(char*);


MYX_DBM_VIEW_DATA *myx_dbm_get_view_data(MYSQL *mysql, const char *catalog, const char *schema, const char *view,
  char quote_char)
{
  MYX_DBM_VIEW_DATA *vdata= NULL;
  char *query;
  char *tmp;

  query= g_strdup_printf("SHOW CREATE VIEW `%s`.`%s`", schema, view);

  if (!(myx_mysql_query(mysql, query)))
  {
    MYSQL_RES *res= mysql_store_result(mysql);
    MYSQL_ROW row;
    if (res != NULL)
    {
      row= mysql_fetch_row(res);
      if (row != NULL)
      {
        int fi[5];
        int num_fields;
        MYSQL_FIELD *fields;

        //Depending on the version of the server there might 
        //   be different columns
        num_fields= mysql_num_fields(res);
        fields= mysql_fetch_fields(res);

        if (mysql_full_version_is_later_or_equal_than(mysql, 5, 0, 3))
          build_field_subst(view_show_create_new,view_show_create_new_end,
                            fields,fields+num_fields,fi);
        else
          build_field_subst(view_show_create,view_show_create_end,
                            fields,fields+num_fields,fi);

        vdata= g_new0(MYX_DBM_VIEW_DATA, 1);
        vdata->catalog= g_strdup(catalog);
        vdata->schema= g_strdup(schema);
        vdata->name= g_strdup(view);
        vdata->definition=
          myx_dbm_force_create_or_replace(tmp= myx_convert_dbstr_utf8(mysql, row[fi[1]], -1));
        g_free(tmp);
      }
    }
    mysql_free_result(res);
  }
  g_free(query);

  return vdata;
}


void myx_dbm_free_view_data(MYX_DBM_VIEW_DATA *vdata)
{
  g_free(vdata->catalog);
  g_free(vdata->schema);
  g_free(vdata->name);
  g_free(vdata->definition);
  g_free(vdata);
}



typedef struct {
  char *name;
  MYX_SCHEMA_STORED_PROCEDURE_TYPE sptype;
} SP_INFO;

static int compare(const void *a, const void *b)
{
  return strcmp(((SP_INFO*)a)->name, ((SP_INFO*)b)->name);
}


char *myx_dbm_make_script_from_sps(MYSQL *mysql, const char *catalog_name, const char *schema_name,
  MYX_SCHEMA_STORED_PROCEDURES *splist, char quote_char)
{ 
  MYX_DBM_STORED_PROCEDURE_DATA **spdata= g_new0(MYX_DBM_STORED_PROCEDURE_DATA*, splist->schema_sps_num);
  unsigned int i;
  unsigned int spcount= 0;
  char *script;
  SP_INFO *sps= g_new0(SP_INFO,splist->schema_sps_num);

  for (i= 0; i < splist->schema_sps_num; i++)
  {
    sps[i].name= splist->schema_sps[i].name;
    sps[i].sptype= splist->schema_sps[i].sp_type;
  }

  // sort by name
  qsort(sps, splist->schema_sps_num, sizeof(SP_INFO), compare);

  for (i= 0; i < splist->schema_sps_num; i++)
  {
    // for each SP, get its definition
    spdata[spcount]= myx_dbm_get_sp_data(mysql, catalog_name, schema_name, sps[i].name, sps[i].sptype, quote_char, 1);
    if (!spdata[spcount]) 
    {
      g_warning("Cannot retrieve SP definition for %s.%s",
                schema_name, sps[i].name);
      continue;
    }

    spcount++;
  }
  g_free(sps);


  script= g_strdup("DELIMITER $$\n\n"
    "-- -----------------------------------------------------------------------------\n\n");
  // create a big script from the sorted list of SPs
  for (i= 0; i < spcount; i++)
  {
    const char *sptype;
    
    sptype= spdata[i]->sp_type==MSPT_FUNCTION?"FUNCTION":"PROCEDURE";
    
    script= str_g_append_and_free(script,
                                  g_strdup_printf("DROP %s IF EXISTS `%s`.`%s` $$\n", sptype, schema_name,
                                                  spdata[i]->name));
    script= str_g_append_and_free(script, g_strdup_printf("%s $$\n\n", spdata[i]->definition));
    script= str_g_append(script, "-- -----------------------------------------------------------------------------\n\n");
  }

  script= str_g_append(script, "DELIMITER ;");

  // free everything
  for (i= 0; i < spcount; i++)
  {
    myx_dbm_free_sp_data(spdata[i]);
  }
  g_free(spdata);

  return script;
}

//----------------------------------------------------------------------------------------------------------------------

// create table t1 ...
// will become
// create table `schema`.`t1`
static char *add_schema_name_to_stmt(const char *unquoted_schema_name, 
                                     const char *name, // entity name
                                     const char *sql)  // sql stmt
{
  char *stmt_start;
  char *retval= NULL;
  char quote_char= '`';

  if (sql != NULL)
  {
    char *name_start= strstr(sql, name);

    if(!name_start)
    {
      return g_strdup(sql);
    }
    if((name_start[-1] == '`') || (name_start[-1] == '"')) // quoted name
    {
      name_start -= 1;
    quote_char= name_start[0];
    }
    if(name_start[-1] == '.') // already have schema name added?
    {
      return g_strdup(sql);
    }
    stmt_start= g_malloc((gulong) (name_start - sql) + 1);
    strncpy(stmt_start, sql, name_start - sql);
    stmt_start[name_start - sql]= '\0';
    retval= g_strdup_printf("%s %c%s%c.%s", stmt_start, quote_char, unquoted_schema_name, quote_char, name_start);
    g_free(stmt_start);
  };
  
  return retval;
}

//----------------------------------------------------------------------------------------------------------------------

static const char * show_create_table[]=
{
  "Database",        // 0 (col 0)
  "Table",           // 1 (col 0)
  "View",            // 2 (col 0)
  "Create Database", // 3 (col 1)
  "Create Table",    // 4 (col 1)
  "Create View",     // 5 (col 1)
};
static const char ** show_create_table_end= show_create_table + sizeof(show_create_table)/sizeof(char*);

/**
 * @note covered by unit tests
 */
char * myx_dbm_get_create_sql(MYSQL *mysql, const char *catalog_name, const char *schema_name, const char *name,
  MYX_DBM_OBJECT_TYPE object_type, int fully_qualified, char quote_char, int add_sql_mode)
{
  char *query;
  MYSQL_ROW row;
  MYSQL_FIELD *fields;
  unsigned int num_fields;
  char *sql= NULL;
  char *sql2= NULL;

  switch (object_type)
  {
    case MYX_DBM_OT_SCHEMA:
      if (!mysql_version_is_later_or_equal_than(mysql, 4, 1))
        return g_strdup_printf("CREATE DATABASE `%s`", schema_name);
      // fall through  
    case MYX_DBM_OT_TABLE:
    case MYX_DBM_OT_VIEW:
      if (object_type == MYX_DBM_OT_SCHEMA)
        query= g_strdup_printf("SHOW CREATE DATABASE `%s`", schema_name);
      else if (object_type == MYX_DBM_OT_TABLE)
        query= g_strdup_printf("SHOW CREATE TABLE `%s`.`%s`", schema_name, name);
      else if (object_type == MYX_DBM_OT_VIEW)
        query= g_strdup_printf("SHOW CREATE VIEW `%s`.`%s`", schema_name, name);

      if (myx_mysql_query(mysql, query) == 0)
      {
        MYSQL_RES *res= mysql_store_result(mysql);
        if (res != NULL)
        {
          row= mysql_fetch_row(res);
          if (row != NULL)
          {
            int fi[6];
            // Depending on the version of the server there might be different columns.
            num_fields= mysql_num_fields(res);
            fields= mysql_fetch_fields(res);

            build_field_subst(show_create_table, show_create_table_end,
                              fields, fields+num_fields, fi);

            if (fi[3]!=-1)
              sql= myx_convert_dbstr_utf8(mysql, row[fi[3]], -1);
            else if (fi[4]!=-1)
              sql= myx_convert_dbstr_utf8(mysql, row[fi[4]], -1);
            else if (fi[5]!=-1)
              sql= myx_convert_dbstr_utf8(mysql, row[fi[5]], -1);
          }
        }
        mysql_free_result(res);
      }
      g_free(query);
      break;
    case MYX_DBM_OT_PROCEDURE:
    case MYX_DBM_OT_FUNCTION:
    {
      MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type;
      MYX_DBM_STORED_PROCEDURE_DATA *sp_data;

      if (object_type == MYX_DBM_OT_PROCEDURE)
        sp_type= MSPT_PROCEDURE;
      else
        sp_type= MSPT_FUNCTION;

      sp_data= myx_dbm_get_sp_data(mysql, catalog_name, schema_name, name, sp_type, quote_char, add_sql_mode);
      if (sp_data != NULL)
      {
        sql= g_strdup(sp_data->definition);
        myx_dbm_free_sp_data(sp_data);
      };
      break;
    }
    case MYX_DBM_OT_TRIGGER:
    {
      MYX_DBM_TRIGGER_DATA* trigger_data= myx_dbm_get_trigger_data(mysql, catalog_name, schema_name, name, quote_char);
      if (trigger_data != NULL)
      {
        sql= g_strdup(trigger_data->definition);
        myx_dbm_free_trigger_data(trigger_data);
      };
      break;
    }
  };

  if (fully_qualified)
  {
    sql2= add_schema_name_to_stmt(schema_name, name, sql);
    g_free(sql);
  }
  else
    sql2= sql;

  return sql2;
}

//----------------------------------------------------------------------------------------------------------------------

const char* trigger_string = "CREATE TRIGGER %s %s %s ON %s FOR EACH ROW %s";
const char* trigger_string_definer = "CREATE DEFINER = %s TRIGGER %s %s %s ON %s FOR EACH ROW %s";

/**
 * Creates a trigger data structure, which contains info about a trigger.
 *
 * @param mysql A pointer to an active mysql server connection.
 * @param catalog The catalog, which contains the schema, which contains the trigger.
 * @param schema The owning schema of the trigger.
 * @param trigger The name of the trigger.
 * @param quote_char The quote character to use in the created SQL.
 *
 * @return A newly allocated trigger data structure filled with details about the trigger or NULL, if the server is
 *         not version 5.0 or later or the trigger could not be found.
 * @note covered by unit tests.
 */
MYX_DBM_TRIGGER_DATA* myx_dbm_get_trigger_data(MYSQL *mysql, const char *catalog, const char *schema,
  const char *trigger, char quote_char)
{
  MYX_DBM_TRIGGER_DATA* result= NULL;

  if (mysql_version_is_later_or_equal_than(mysql, 5, 0))
  {
    char* escaped_schema= escape_string(schema);
    char* escaped_trigger= escape_string(trigger);
    char* query= g_strdup_printf("select * from information_schema.TRIGGERS where (TRIGGER_SCHEMA = '%s') and"
      " (TRIGGER_NAME = '%s')", escaped_schema, escaped_trigger);
    if (myx_mysql_query(mysql, query) == 0)
    {
      MYSQL_RES *res= mysql_store_result(mysql);
      if (res != NULL)
      {
        MYSQL_ROW row= mysql_fetch_row(res);
        if (row != NULL)
        {
          char* quoted_trigger;
          char* quoted_table;

          result= g_new0(MYX_DBM_TRIGGER_DATA, 1);
          if (row[0] != NULL)
            result->catalog= g_strdup(row[0]);
          else
            result->catalog= g_strdup(catalog);
          result->schema= g_strdup(row[1]);
          result->name= g_strdup(row[2]);

          quoted_trigger= quote_identifier(trigger, quote_char);
          quoted_table= quote_identifier(row[6], quote_char);

          // Column 18 (zero based) contains the definer if any.
          if (row[18] != NULL)
          {
            // Definers are stored as name@address (e.g. root@%).
            // Each part has to be quoted.
            char* definer;
            char** parts= g_strsplit(row[18], "@", 2);

            definer= g_strdup_printf("%s@%s", quote_identifier(parts[0], quote_char), quote_identifier(parts[1],
              quote_char));
            g_strfreev(parts);

            result->definition= g_strdup_printf(trigger_string_definer,
              definer,        // definer
              quoted_trigger, // trigger name
              row[11],        // timing
              row[3],         // action
              quoted_table,   // table
              row[9]          // statement
            );
            g_free(definer);
          }
          else
          {
            result->definition= g_strdup_printf(trigger_string,
              quoted_trigger, // trigger name
              row[11],        // timing
              row[3],         // action
              quoted_table,   // table
              row[9]          // statement
            );
          };
          g_free(quoted_trigger);
          g_free(quoted_table);
        };
      };
    };
    g_free(escaped_schema);
    g_free(escaped_trigger);
    g_free(query);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Frees previously allocated trigger data.
 *
 * @param trigger_data The data to be freed. This point is no longer valid once the function returns.
 * @note covered by unit tests.
 */
void myx_dbm_free_trigger_data(MYX_DBM_TRIGGER_DATA *trigger_data)
{
  if (trigger_data != NULL)
  {
    g_free(trigger_data->catalog);
    g_free(trigger_data->schema);
    g_free(trigger_data->name);
    g_free(trigger_data->definition);
    g_free(trigger_data);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns a list of engines built into the server, along with additional info, depending on the server version.
 *
 * @param mysql The connection to the server to query.
 *
 * @return A list of engines which are supported by the current server. If there is a problem (e.g. old server)
 *         a list of default engines is returned.
 */
MYX_ENGINES* myx_get_engines(MYSQL *mysql)
{
  MYX_ENGINES* result= NULL;

  // Beginning with 5.1 we could use the ENGINES table in I_S but that only gives us additional info over
  // SHOW ENGINES which we don't need yet (and before 5.1 we have to use SHOW ENGINES anyway).
  // For 4.0 and earlier we cannot query the engines so we just return a default list with MyISAM and InnoDB in it.
  char* query= "show engines";
  if (mysql_query(mysql, query) == 0)
  {
    MYSQL_RES *res= mysql_store_result(mysql);
    if (res != NULL)
    {
      MYSQL_ROW row;
      int i= 0;

      result= g_malloc0(sizeof(MYX_ENGINES));
      result->engines_num= mysql_num_rows(res);

      result->engines= g_malloc0(result->engines_num * sizeof(MYX_ENGINE));
      while ((row= mysql_fetch_row(res)) != NULL)
      {
        result->engines[i].name= g_strdup(row[0]);
        result->engines[i].description= g_strdup(row[2]);

        if (strcasecmp(row[1], "DEFAULT") == 0)
          result->engines[i].isdefault= 1;
        if ((strcasecmp(row[1], "YES") == 0) || result->engines[i].isdefault)
          result->engines[i].enabled= 1;
        ++i;
      };
    };
  };

  if (result == NULL)
  {
    result= g_malloc0(sizeof(MYX_ENGINES));
    result->engines_num= 2;

    result->engines= g_malloc0(result->engines_num * sizeof(MYX_ENGINE));
    result->engines[0].name= g_strdup("MyISAM");
    result->engines[0].description= g_strdup("Default engine as of MySQL 3.23 with great performance");
    result->engines[0].isdefault= 1;
    result->engines[0].enabled= 1;
    
    result->engines[1].name= g_strdup("InnoDB");
    result->engines[1].description= g_strdup("Supports transactions, row-level locking, and foreign keys");
    result->engines[1].isdefault= 0;
    result->engines[1].enabled= 1;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Free a single MYX_ENGINE structure.
 *
 * @param engine The structure to free
 */

void myx_free_engine(MYX_ENGINE* engine)
{
  g_free(engine->name);
  g_free(engine->description);
  g_free(engine);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Frees previously by myx_get_engines allocated memory.
 *
 * @param engines The engines structure to clean up.
 */
void myx_free_engines(MYX_ENGINES* engines)
{
  unsigned int i;
  
  if (engines != NULL)
  {
    for (i = 0; i < engines->engines_num; i++)
    {
      g_free(engines->engines[i].name);
      g_free(engines->engines[i].description);
    };
    g_free(engines->engines);
    g_free(engines);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Copies a single MYX_ENGINE structure. Use myx_free_engine to free it later
 *
 * @param engine The structure to copy.
 * @return The copied engine structure.
 */

MYX_ENGINE* myx_copy_engine(MYX_ENGINE* engine)
{
  MYX_ENGINE *engine_copy= NULL;
  
  if(engine == NULL)
    return NULL;

  engine_copy= (MYX_ENGINE *)g_malloc0(sizeof(MYX_ENGINE));
  engine_copy->name= strcpy(g_malloc(strlen(engine->name)+1), engine->name);
  engine_copy->description= strcpy(g_malloc(strlen(engine->description)+1), engine->description);
  engine_copy->isdefault= engine->isdefault;
  engine_copy->enabled= engine->enabled;
  
  return engine_copy;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Return pointer to the default storage engine.
 * (replacement for myx_dbm_get_default_storage_engine when using MYX_ENGINES)
 *
 * @param engine The structure to search
 *
 * @return A pointer to an engine entry if there is a default one, otherwise NULL.
 */

MYX_ENGINE* myx_find_default_engine(MYX_ENGINES* engines)
{
  unsigned int i;
  
  if (engines != NULL)
  {
    for (i = 0; i < engines->engines_num; i++)
    {
      if(engines->engines[i].isdefault)
        return engines->engines + i;
    }
  }
  
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
