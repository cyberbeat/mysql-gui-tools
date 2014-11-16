/* Copyright (C) 2003, 2004, 2005 MySQL AB

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

#define BACKUP_CONTENT_BUFFER_LEN 256 //used by get_backup_content
#define DEFAULT_SCHEMA_NAME "DEFAULT_SCHEMA"
#define DEFAULT_CATALOG_NAME "DEFAULT_CATALOG"
#define ADMIN_DUMP_VERSION "1.4"
#define USER_NAME_PCRE "(`[^`]+`|\"[^\"]+\"|\\S+)@(`[^`]+`|\"[^\"]+\"|\\S+)"
#define USER_NAME_PCRE_IGNORE "(?:`[^`]+`|\"[^\"]+\"|\\S+)@(?:`[^`]+`|\"[^\"]+\"|\\w+)"
#define IF_EXISTS "(?:(?:\\/\\*\\![0-9]+\\s+if\\s+exists\\s*\\*\\/\\s+)|(?:if\\s+exists\\s+))?"
#define IF_NOT_EXISTS "(?:(?:\\/\\*\\![0-9]+\\s+if\\s+not\\s+exists\\s*\\*\\/\\s+)|(?:if\\s+not\\s+exists\\s+))?"
#define PCRE_DEFAULT PCRE_ANCHORED | PCRE_CASELESS | PCRE_DOTALL | PCRE_UTF8

#define INITIAL_BUFFER_LENGTH 1024 * 1024 /* initial size of 1M */

// Size of buffer for dump's select query
#define QUERY_LENGTH 1536

#include "myx_admin_library.h"
#include "myx_util_functions.h"
#include "myx_public_interface.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "myx_library.h"
#include <errno.h>
#include <ctype.h>
#include <glib/gprintf.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <time.h>
#include <string.h>
#if !defined(__WIN__) && !defined(_WIN32) && !defined(_WIN64)
#define __USE_LARGEFILE64
#include <fcntl.h>
#include <sys/stat.h>
#endif

//#define MYX_DEBUG_MEMORY
//#include "myx_debug_memory.h"

// TODO: add gettext support
#define _(s) s

/* taken from Viktor Vagins patch */
#define SAFE_IO(func) \
{func; if (errno == ENOSPC){*error=MYX_BACKUP_OUTPUTDEVICE_FULL;return -1;}}

// Translation of library errors to backup errors.
const MYX_BACKUP_ERROR error_mapping[] =
{
  MYX_BACKUP_NO_ERROR,            // MYX_NO_ERROR
  MYX_BACKUP_CANT_OPEN_FILE,      // MYX_ERROR_CANT_OPEN_FILE
  MYX_BACKUP_SERVER_ERROR,        // MYX_ERROR_CANT_CONNECT_TO_INSTANCE
  MYX_BACKUP_XML_PARSE_ERROR,     // MYX_XML_PARSE_ERROR
  MYX_BACKUP_XML_PARSE_ERROR,     // MYX_XML_NO_VALID_DOCUMENT 
  MYX_BACKUP_XML_PARSE_ERROR,     // MYX_XML_EMPTY_DOCUMENT 
  MYX_BACKUP_SQL_ERROR,           // MYX_SQL_ERROR
  MYX_BACKUP_STOPPED,             // MYX_STOP_EXECUTION
  MYX_BACKUP_MALLOC_FAILED,       // MYX_ALLOC_CHANGE_ERROR
  MYX_BACKUP_UNKNOWN,             // MYX_OBJECT_NOT_FOUND
  MYX_BACKUP_CANT_READ_FROM_FILE, // MYX_CANT_READ_FROM_FILE
  MYX_BACKUP_CHARSET_CONVERSION,  // MYX_CHARSET_CONVERSION_ERROR
  MYX_BACKUP_WRONG_CHARSET,       // MYX_CHARSET_WRONG_CHARSET_SPECIFIED
  MYX_BACKUP_MALLOC_FAILED        // MYX_MEMORY_LIMIT_EXCEEDED
};

/*
 * Public functions
 */

/* Profile functions */
                                                           
/*
 *----------------------------------------------------------------------
 *
 *
 * SYNOPSIS
 *  profile_directory must include a trailing path delimiter
 * DESCRIPTION
 *
 * RETURN VALUE
 *
 * NOTES
 *----------------------------------------------------------------------
 */
MYX_ADMIN_LIB_ERROR myx_save_profile(const char *profile_name,
                                     const char *profile_directory,
                                     MYX_BACKUP_PROFILE *backup_profile)
{
  xmlDocPtr doc;
  xmlNodePtr root_node, entities_node, entity_node;
  MYX_ADMIN_LIB_ERROR res;
  char *filename;

  if (!profile_name || !profile_directory || !backup_profile)
    return -1;

  doc= xmlNewDoc((xmlChar*)"1.0");

  root_node= doc->children= xmlNewDocRawNode(doc,NULL,(xmlChar*)"backup_profile",NULL);

  xmlNewTextChild(root_node,NULL,(xmlChar*)"version", (xmlChar*)"1.2");
  
  xmlNewTextChild(root_node,NULL,(xmlChar*)"profile_name", (xmlChar*)backup_profile->profile_name);
  xmlNewTextChild(root_node,NULL,(xmlChar*)"last_used", (xmlChar*)backup_profile->last_used);
  NewTextChild_int_content(root_node,NULL, (xmlChar*)"options", backup_profile->options);
  NewTextChild_int_content(root_node,NULL, 
                           (xmlChar*)"backup_type", backup_profile->backup_type);

  entities_node= xmlNewTextChild(root_node, NULL, (xmlChar*)"entities", NULL);

  if (backup_profile->backup_content)
  {
    MYX_BACKUP_CONTENT *bc = backup_profile->backup_content;
    MYX_BACKUP_TABLE *table= bc->tables;
    MYX_BACKUP_TABLE *tables_end= table + bc->tables_num;
    for (; table < tables_end; table++)
    {
      entity_node= xmlNewTextChild(entities_node, NULL, (xmlChar*)"entity", NULL);
      xmlNewTextChild(entity_node, NULL, (xmlChar*)"name", (xmlChar*)table->table);
      xmlNewTextChild(entity_node, NULL, (xmlChar*)"schema", (xmlChar*)table->schema);
      xmlNewTextChild(entity_node, NULL, (xmlChar*)"catalog", (xmlChar*)table->catalog);
      if(table->flags & MYX_BTF_IS_VIEW)
      {
        xmlNewTextChild(entity_node, NULL, (xmlChar*)"entity_type", (xmlChar*)"view");
      }
      else if(table->flags & MYX_BTF_IS_PROCEDURE)
      {
        xmlNewTextChild(entity_node, NULL, (xmlChar*)"entity_type", (xmlChar*)"proc");
      }
      else if(table->flags & MYX_BTF_IS_FUNCTION)
      {
        xmlNewTextChild(entity_node, NULL, (xmlChar*)"entity_type", (xmlChar*)"func");
      }
      else
      {
        xmlNewTextChild(entity_node, NULL, (xmlChar*)"entity_type", (xmlChar*)"table");
      }
    }
  }

  filename= g_build_path("/", profile_directory, profile_name, NULL); // Don't forget the NULL parameter to finish the parameter list!
  res= myx_xmlSaveFile(filename, doc);
  g_free(filename);
  xmlFreeDoc(doc);
  return (int)res == -1 ? -1 : 0;
}

int myx_free_profile(MYX_BACKUP_PROFILE *profile)
{
  if (profile)
  {
    xmlFree(profile->profile_name);
    xmlFree(profile->last_used);
    if (profile->backup_content)
      myx_free_backup_content(profile->backup_content);
    g_free(profile);
  }
  return 0;
}

MYX_BACKUP_PROFILE *myx_load_profile(const char *profile_name,
                                     const char *profile_directory,
                                     MYX_ADMIN_LIB_ERROR *error_code)
{
  char* local_filename;
  MYX_BACKUP_PROFILE *backup_profile= NULL;

  char *filename= g_build_filename(profile_directory, profile_name, NULL);

  local_filename= g_filename_from_utf8(filename, -1, NULL, NULL, NULL);
  if (local_filename == NULL)
  {
    *error_code= MYX_CHARSET_CONVERSION_ERROR;
    return NULL;
  };

  if (!file_exists(local_filename))
  {
    *error_code= MYX_ERROR_CANT_OPEN_FILE;
  }
  else
  {
    xmlDocPtr doc= myx_xmlParseFile(filename);
    if (doc == NULL )
    {
      *error_code= MYX_XML_PARSE_ERROR;
    }
    else
    {
      xmlNodePtr root= xmlDocGetRootElement(doc);
      if (root == NULL)
      {
        *error_code= MYX_XML_EMPTY_DOCUMENT;
      }
      else
      {
        if (xmlStrcmp(root->name, (const xmlChar *) "backup_profile"))
        {
          *error_code= MYX_XML_NO_VALID_DOCUMENT;
        }
        else
        {
          xmlNodePtr version = try_to_get_child(doc, root, "version");
          *error_code= MYX_ADMIN_NO_ERROR;
          if(version)
          {
            char *version_num= NULL;
            try_to_get_string_field(doc, version, "version", &version_num);
            if(strcmp(version_num, "1.1") == 0)
            {
              backup_profile= read_in_backup_profile_1_1(root);
            }
            else if(strcmp(version_num, "1.2") == 0)
            {
              backup_profile= read_in_backup_profile_1_2(root);
            }
            else
            {
              *error_code= MYX_XML_PARSE_ERROR;
              backup_profile= NULL;
            }
            g_free(version_num);
          }
          else
          {
            backup_profile= read_in_backup_profile_1_0(root);
          }
        }
      }
      xmlFreeDoc(doc);
    }
  }
  g_free(filename);

  return backup_profile;
}

MYX_BACKUP_ERROR myx_make_backup_with_profile(MYSQL *mysql,
                                              MYX_BACKUP_PROFILE *profile,
                                              const char *path,
                                              int callback_interval,
                                              int (*progress_report)
                                              (
                                                const char* current_table_name,
                                                int num_tables,
                                                int num_tables_processed,
                                                int num_rows,
                                                int num_rows_processed,
                                                void *user_data
                                              ),
                                              void *user_data)
{
  return myx_make_backup(mysql, path, profile->backup_content,
                         profile->backup_type, profile->options,
                         callback_interval, progress_report, user_data);
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the progress report callback stored in the backup status (if given).
 *
 * @param status The current backup status.
 *
 * @return 0 if all is ok, otherwise the result of the callback.
 */
int do_progress_report(MYX_BS_STATUS *status)
{
  if (status->progress_report)
    return (*status->progress_report)(status->current_table_quoted, status->total_count, status->count,
      status->current_table_rows, status->current_table_rows_processed, status->user_data);
  else
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------

static const char * show_master_status_fields[]=
{
  "File",               // 0
  "Position",           // 1
  "Binlog_Do_DB",       // 2
  "Binlog_Ignore_DB"    // 3
};
static const char ** show_master_status_fields_end= show_master_status_fields +
  sizeof(show_master_status_fields) / sizeof(char*);

/**
 * Initializes the backup by preparing the database.
 *
 * @param status The backup status containing all relevant info.
 * @param error A point to an error variable to contain a specific error code if preparation fails.
 *
 * @return 0 if everything went fine, otherwise -1.
 */
int initialize_backup(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  gchar sql_mode_cmd[200] = "/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, sql_mode='";
  char *sql_mode_cmd_end= sql_mode_cmd+strlen(sql_mode_cmd);
  int mode_entry_count = 0;

  if (status->options & MYX_B_ANSI_QUOTES)
  {
    sql_mode_cmd_end= strmov(sql_mode_cmd_end, "ANSI_QUOTES");
    mode_entry_count++;
  };

  if (status->options & MYX_B_COMPATIBILITY_MODE)
  {
    sql_mode_cmd_end= strmov(sql_mode_cmd_end, (mode_entry_count > 0) ? "," : "");
    sql_mode_cmd_end= strmov(sql_mode_cmd_end, "MYSQL323");
    mode_entry_count++;
  };
  strmov(sql_mode_cmd_end, "' */");

  if (write_sql_file_header(status, error))
    return -1;

  // Backup method
  if (status->options & MYX_B_LOCK_ALL_TABLES)
  {
    if (myx_mysql_query(status->mysql, "FLUSH TABLES WITH READ LOCK"))
    {
      *error= MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK;
      return -1; /* fatal */
    }
  }
  else
    if (status->options & MYX_B_SINGLE_TRANSACTION)
    {
      // make sure the correct ISOLATION LEVEL is set
      if (mysql_full_version_is_later_or_equal_than(status->mysql, 4, 0, 5))
      {
        if (myx_mysql_query(status->mysql, "SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        };
      };

      // start the transaction
      if (mysql_full_version_is_later_or_equal_than(status->mysql, 4, 1, 8))
      {
        if (myx_mysql_query(status->mysql, "START TRANSACTION WITH CONSISTENT SNAPSHOT"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        };
      }
      else
      {
        if (myx_mysql_query(status->mysql, "BEGIN"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        };
      };
    }
    else
      if (status->options & MYX_B_POINT_IN_TIME_BACKUP)
      {
        if (myx_mysql_query(status->mysql, "FLUSH TABLES"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        };

        if (myx_mysql_query(status->mysql, "FLUSH TABLES WITH READ LOCK"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        };

        // make sure the correct ISOLATION LEVEL is set
        if (mysql_full_version_is_later_or_equal_than(status->mysql, 4, 0, 5))
        {
          if (myx_mysql_query(status->mysql, "SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ"))
          {
            *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
            return -1;  /* fatal */
          };
        };

        if (mysql_full_version_is_later_or_equal_than(status->mysql, 4, 1, 8))
        {
          if (myx_mysql_query(status->mysql, "START TRANSACTION WITH CONSISTENT SNAPSHOT"))
          {
            *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
            return -1;  /* fatal */
          };
        }
        else
        {
          if (myx_mysql_query(status->mysql, "BEGIN"))
          {
            *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
            return -1;  /* fatal */
          };
        };

        if (myx_mysql_query(status->mysql, "SHOW MASTER STATUS"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        }
        else
        {
          MYSQL_RES *res;

          res= mysql_store_result(status->mysql);
          if (res != NULL)
          {
            MYSQL_ROW row;
            MYSQL_FIELD *fields;
            int num_fields;
            int fi[5];
            char *binlog_filename= NULL;
            char *binlog_pos= NULL;

            // Depending on the version of the server there might be different columns
            num_fields= mysql_num_fields(res);
            fields= mysql_fetch_fields(res);

            build_field_subst(show_master_status_fields, show_master_status_fields_end, fields, fields + num_fields, fi);

            row= mysql_fetch_row(res);
            if (row != NULL)
            {
              if (fi[0] > -1)
                binlog_filename= myx_convert_dbstr_utf8(status->mysql, row[fi[0]], -1);
              if (fi[1] > -1)
                binlog_pos= g_strdup(row[fi[1]]);
            };

            if (binlog_filename && binlog_pos)
            {
              SAFE_IO(fprintf(status->sql_file,
                "--\n"
                "-- Position to start replication or point-in-time recovery from\n"
                "--\n\n"
                "-- CHANGE MASTER TO MASTER_LOG_FILE='%s', MASTER_LOG_POS=%s;\n\n",
                binlog_filename, binlog_pos));
            };

            mysql_free_result(res);
          };
        };

        if (myx_mysql_query(status->mysql, "UNLOCK TABLES"))
        {
          *error= MYX_BACKUP_CANNOT_START_TRANSACTION;
          return -1;  /* fatal */
        }
      };

  if (myx_mysql_query(status->mysql, sql_mode_cmd))
  {
    *error= MYX_BACKUP_CANNOT_SET_ANSI_QUOTES;
    return -1;
  };

  // Set a definite schema at startup to make fully qualified identifiers work reliably.
  // Ignore errors, as they aren't relevant here.
  myx_mysql_query(status->mysql, "use mysql");

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates dummy table definitions for all views to be backup-ed up. These definitions do not reference other elements
 * and are therefore kind of a "view forward declaration". This hack (which is also used by mysqldump) is necessary
 * because it is not possible to tell the server not to check certain references when creating new objects.
 * Stangely enough, stored routines references in stored routines are not checked so it does not matter there.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to contain a specific error code if something fails.
 *
 * @return 0 if everything went fine, otherwise -1.
 */
int dump_dummy_tables_for_views(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  unsigned int i;
  char* sql;
  char buffer[QUERY_LENGTH];
  MYSQL_RES* result;
  MYSQL_ROW row;

  for (i= 0; i < status->backup_content->tables_num; i++)
  {
    if (status->backup_content->tables[i].flags & MYX_BTF_IS_VIEW)
    {
      status->count++;

      status->current_index= i;
      status->current_table_quoted= quote_identifier(status->backup_content->tables[i].table, status->quote_char);
      status->current_schema_quoted= quote_identifier(status->backup_content->tables[i].schema, status->quote_char);

      if (do_progress_report(status) != 0)
      {
         *error= MYX_BACKUP_STOPPED;
         return -1;
      };

      // Write new schema DDL to file if necessary.
      if (write_schema_ddl(status, error))
        return -1;

      // Write a dummy create table statement for that view. Only view name, column names and column types matter.
      if (status->options & MYX_B_COMMENT)
      {
        if (status->qualified)
          SAFE_IO(fprintf(status->sql_file, "\n--\n-- Temporary table structure for view %s.%s\n--\n",
            status->current_schema_quoted, status->current_table_quoted))
        else
          SAFE_IO(fprintf(status->sql_file, "\n--\n-- Temporary table structure for view %s\n--\n",
            status->current_table_quoted));
      };

      if (status->options & MYX_B_ADD_DROP_TABLE)
      {
        if (status->qualified)
          sql= g_strdup_printf("DROP TABLE IF EXISTS %s.%s;\n", status->current_schema_quoted,
            status->current_table_quoted);
        else
          sql= g_strdup_printf("DROP TABLE IF EXISTS %s;\n", status->current_table_quoted);
        SAFE_IO(fprintf(status->sql_file, sql));
        g_free(sql);

        if (status->qualified)
          sql= g_strdup_printf("DROP VIEW IF EXISTS %s.%s;\n", status->current_schema_quoted,
            status->current_table_quoted);
        else
          sql= g_strdup_printf("DROP VIEW IF EXISTS %s;\n", status->current_table_quoted);
        SAFE_IO(fprintf(status->sql_file, sql));
        g_free(sql);
      };

      // Now get the view's fields and build a create table statement from that.
      sprintf(buffer, "SHOW FIELDS FROM %s.%s", status->current_schema_quoted, status->current_table_quoted);
      if (mysql_query(status->mysql, buffer) == 0)
      {
        result= mysql_store_result(status->mysql);
        if (result != NULL)
        {
          int row_count= mysql_num_rows(result);
          if (row_count > 1)
          {
            if (status->qualified)
              sql= g_strdup_printf("CREATE TABLE %s.%s (\n", status->current_schema_quoted,
                status->current_table_quoted);
            else
              sql= g_strdup_printf("CREATE TABLE %s (\n", status->current_table_quoted);
            SAFE_IO(fprintf(status->sql_file, sql));
            g_free(sql);

            /*
               Get first row, following loop will prepend comma - keeps
               from having to know if the row being printed is last to
               determine if there should be a _trailing_ comma.
            */
            row= mysql_fetch_row(result);
            SAFE_IO(fprintf(status->sql_file, "  %c%s%c %s", status->quote_char, row[0], status->quote_char, row[1]));

            while ((row= mysql_fetch_row(result)) != NULL)
            {
              SAFE_IO(fprintf(status->sql_file, ",\n  %c%s%c %s", status->quote_char, row[0],
                status->quote_char, row[1]));
            };
            SAFE_IO(fprintf(status->sql_file, "\n);\n"));
          };
        };
      };

      status->previous_index= i;

      g_free(status->current_schema_quoted);
      status->current_schema_quoted= NULL;
      g_free(status->current_table_quoted);
      status->current_table_quoted= NULL;
    };
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * If there is pending SQL output (due to accumulated insert statements) then write them out finally.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to receive a specific error code if something fails.
 */
int finalize_extended_insert(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  if (!(status->options & MYX_B_NO_EXTENDED_INSERT) && status->extended_insert)
  {
    char *tmp_str;
    char * pos= status->extended_insert + strlen(status->extended_insert) - 1;
    for ( ; *pos!=','; pos--)
      ;
    *pos= ';';  /*overwrite the comma with ;*/
    pos++;
    *pos= 0;

    tmp_str= myx_convert_dbstr_utf8(status->mysql, status->extended_insert, -1);
    g_free(status->extended_insert);
    status->extended_insert= NULL;

    SAFE_IO(fprintf(status->sql_file, "%s\n", tmp_str));
    g_free(tmp_str);
  };
  
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Dumps all tables in the backup content in the order they appear there.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to receive a specific error code if something fails.
 *
 * @return 0 if everything went fine, otherwise -1 (also if user stopped the backup).
 */
int dump_tables(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  MYSQL_ROW row;
  char buffer[QUERY_LENGTH];
  MYSQL_RES* result;
  char* current_table;
  char* current_schema;
  unsigned int i;

  *error= MYX_BACKUP_NO_ERROR;

  for (i= 0; i < status->backup_content->tables_num; i++)
  {
    status->current_index= i;
    if ((status->backup_content->tables[i].table != NULL) && (status->backup_content->tables[i].flags & MYX_BTF_IS_TABLE))
    {
      current_table= status->backup_content->tables[i].table;
      status->current_table_quoted= quote_identifier(current_table, status->quote_char);

      current_schema= status->backup_content->tables[i].schema;
      status->current_schema_quoted= quote_identifier(current_schema, status->quote_char);

      status->ignore= status->backup_content->tables[i].flags & MYX_BTF_IGNORE_CONTENT;

      status->current_table_rows_processed= 0;

      if (do_progress_report(status) != 0)
      {
         *error= MYX_BACKUP_STOPPED;
         return -1;
      };

      // Ignore means no content dump.
      if (!status->ignore)
      {
        // Find out how many rows this table has.
        sprintf(buffer, "SELECT count(*) FROM %s.%s", status->current_schema_quoted, status->current_table_quoted);
        result= NULL;
        if (myx_mysql_query(status->mysql, buffer) == 0)
          result= mysql_store_result(status->mysql);
        if (result == NULL)
        {
          *error= MYX_BACKUP_SERVER_ERROR;
          return -1;
        };

        row= mysql_fetch_row(result);
        if (row)
        {
          status->current_table_rows= atoi(row[0]);
          mysql_free_result(result);
        }
        else
        {
          mysql_free_result(result);
          *error= MYX_BACKUP_SERVER_ERROR;
          return -1;
        };
      }
      else
        status->current_table_rows= 0;

      // Write new schema DDL to file if necessary.
      if (write_schema_ddl(status, error))
        return -1;

      // Get the create-statement of the new table.
      // If writing table create statement failed then ignore this table.
      if (!(status->options & MYX_B_NO_CREATES))
      {
        status->ignore = write_create_statement_to_file(status, error) || status->ignore;
        *error= MYX_BACKUP_NO_ERROR;
      };

      if (!status->ignore)
      {
        // Table content is to be dumped.
        if (status->options & MYX_B_COMMENT)
        {
          if (status->qualified)
            SAFE_IO(fprintf(status->sql_file, "\n--\n-- Dumping data for table %s.%s\n--\n",
              status->current_schema_quoted, status->current_table_quoted))
          else
            SAFE_IO(fprintf(status->sql_file, "\n--\n-- Dumping data for table %s\n--\n",
              status->current_table_quoted));
        }

        // Get the rows of the new table.
        sprintf(buffer, "SELECT /*!40001 SQL_NO_CACHE */ * FROM %s.%s", status->current_schema_quoted,
          status->current_table_quoted);

        status->mysql_result= NULL;
        if (myx_mysql_query(status->mysql, buffer) == 0)
          status->mysql_result= mysql_use_result(status->mysql);
        if (status->mysql_result == NULL)
        {
          *error= MYX_BACKUP_SERVER_ERROR;
          return -1;
        };

        if ((status->options & MYX_B_DISABLE_KEYS) && !status->ignore)
          SAFE_IO(fprintf(status->sql_file, "\n/*!40000 ALTER TABLE %s DISABLE KEYS */;\n", status->current_table_quoted));

        if (status->options & MYX_B_ADD_LOCKS)
          SAFE_IO(fprintf(status->sql_file,"LOCK TABLES %s WRITE;\n",status->current_table_quoted));

        if (status->options & MYX_B_OPTIMIZED_COMMIT)
        {
          SAFE_IO(fprintf(status->sql_file,"SET AUTOCOMMIT=0;\n"));
          status->uncommitted_size= 0;
        };
      }
      else // if (!status->ignore)
        SAFE_IO(fprintf(status->sql_file, "\n"));

      if (!status->ignore && status->current_table_quoted)
      {
        int callback_counter= status->report_interval;

        while ((row= mysql_fetch_row(status->mysql_result)) != NULL)
        {
          if (write_row_to_file(status, row, error))
            return -1;

          status->current_table_rows_processed++;
          callback_counter--;

          if (callback_counter == 0)
          {
            do_progress_report(status);
            callback_counter= status->report_interval;
          }
        };
      };

      mysql_free_result(status->mysql_result);
      status->mysql_result= NULL;

      // It is possible that a last extended insert must yet be written out.
      finalize_extended_insert(status, error);

      if (!status->ignore)
      {
        if (status->options & MYX_B_OPTIMIZED_COMMIT)
        {
          SAFE_IO(fprintf(status->sql_file,"COMMIT;\n"));
        }
        if (status->options & MYX_B_ADD_LOCKS)
          SAFE_IO(fprintf(status->sql_file,"UNLOCK TABLES;\n"));

        if (status->options &  MYX_B_DISABLE_KEYS)
        {
          SAFE_IO(fprintf(status->sql_file, "/*!40000 ALTER TABLE %s ENABLE KEYS */;\n\n",
            status->current_table_quoted));
        };
      };

      status->count++;

      // If triggers are supported the dump them here.
      if (mysql_full_version_is_later_or_equal_than(status->mysql, 5, 0, 9))
        write_triggers_to_file(status, error, current_table, current_schema); 

      status->previous_index= i;
      g_free(status->current_table_quoted);
      g_free(status->current_schema_quoted);
    };
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Dumps all stored functions in the backup content in the order they appear there.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to receive a specific error code if something fails.
 *
 * @return 0 if everything went fine, otherwise -1 (also if user stopped the backup).
 */
int dump_functions(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  char* current_table;
  char* current_schema;
  unsigned int i;
  
  *error= MYX_BACKUP_NO_ERROR;

  for (i= 0; i < status->backup_content->tables_num; i++)
  {
    status->current_index= i;
    if ((status->backup_content->tables[i].table != NULL) && (status->backup_content->tables[i].flags & MYX_BTF_IS_FUNCTION))
    {
      current_table= status->backup_content->tables[i].table;
      status->current_table_quoted= quote_identifier(current_table, status->quote_char);

      current_schema= status->backup_content->tables[i].schema;
      status->current_schema_quoted= quote_identifier(current_schema, status->quote_char);

      status->current_table_rows_processed= 0;

      if (do_progress_report(status) != 0)
      {
         *error= MYX_BACKUP_STOPPED;
         return -1;
      };

      // Write new schema DDL to file if necessary.
      if (write_schema_ddl(status, error))
        return -1;

      // Get the create-statement of the object.
      if (write_create_statement_to_file(status, error))
        return -1;

      status->count++;
      status->previous_index= i;
      g_free(status->current_table_quoted);
      g_free(status->current_schema_quoted);
    };
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Dumps all stored procedures in the backup content in the order they appear there.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to receive a specific error code if something fails.
 *
 * @return 0 if everything went fine, otherwise -1 (also if user stopped the backup).
 */
int dump_procedures(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  char* current_table;
  char* current_schema;
  unsigned int i;

  *error= MYX_BACKUP_NO_ERROR;

  for (i= 0; i < status->backup_content->tables_num; i++)
  {
    status->current_index= i;
    if ((status->backup_content->tables[i].table != NULL) && (status->backup_content->tables[i].flags & MYX_BTF_IS_PROCEDURE))
    {
      current_table= status->backup_content->tables[i].table;
      status->current_table_quoted= quote_identifier(current_table, status->quote_char);

      current_schema= status->backup_content->tables[i].schema;
      status->current_schema_quoted= quote_identifier(current_schema, status->quote_char);

      status->current_table_rows_processed= 0;

      if (do_progress_report(status) != 0)
      {
         *error= MYX_BACKUP_STOPPED;
         return -1;
      };

      // Write new schema DDL to file if necessary.
      if (write_schema_ddl(status, error))
        return -1;

      // Get the create-statement of the object.
      if (write_create_statement_to_file(status, error))
        return -1;

      status->count++;
      status->previous_index= i;
      g_free(status->current_table_quoted);
      g_free(status->current_schema_quoted);
    };
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Dumps all views in the backup content in the order they appear there.
 *
 * @param status The backup status containing all relevant info.
 * @param error A pointer to an error variable to receive a specific error code if something fails.
 *
 * @return 0 if everything went fine, otherwise -1 (also if user stopped the backup).
 */
int dump_views(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  char* current_table;
  char* current_schema;
  unsigned int i;
  
  *error= MYX_BACKUP_NO_ERROR;

  for (i= 0; i < status->backup_content->tables_num; i++)
  {
    status->current_index= i;
    if ((status->backup_content->tables[i].table != NULL) && (status->backup_content->tables[i].flags & MYX_BTF_IS_VIEW))
    {
      current_table= status->backup_content->tables[i].table;
      status->current_table_quoted= quote_identifier(current_table, status->quote_char);

      current_schema= status->backup_content->tables[i].schema;
      status->current_schema_quoted= quote_identifier(current_schema, status->quote_char);

      status->current_table_rows_processed= 0;

      if (do_progress_report(status) != 0)
      {
         *error= MYX_BACKUP_STOPPED;
         return -1;
      };

      // Write new schema DDL to file if necessary.
      if (write_schema_ddl(status, error))
        return -1;

      // Get the create-statement of the object.
      if (write_create_statement_to_file(status, error))
        return -1;

      status->count++;
      status->previous_index= i;
      g_free(status->current_table_quoted);
      g_free(status->current_schema_quoted);
    };
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Starts a backup of the given content. The content can consist of several tables
 * from different schemas and catalogs. display_backup_progress points to a callback
 * function to display the current progress of the backup.
 *
 * SYNOPSIS
 *   mysql : The mysql-connection that will be used
 *   filename: The name of the file that will hold the backup
 *   content: lists all elements that should be backed up
 *   backup_type: at the moment only MYX_BT_SQL_SCRIPT
 *   options: see the description
 *   progress_report: a function that will be called
 *   user_data: a pointer to user-defined data to be passed to the progress_report
 *   callback_interval: determines how many rows must be processed between two callback triggers)
 * DESCRIPTION
 *  There are lot of options that determine how the actual backup is
 *  done:
 *   MYX_B_NO_CREATES:           no create element statements will be written
 *   MYX_B_NO_EXTENDED_INSERT:   instead of mysql's faster insert syntax that
 *                                 allows several values in one insert the ansi-compatible
 *                                 normal syntax is used
 *   MYX_B_ADD_DROP_TABLE:       before each create-statement a "DROP TABLE IF 
 *                                EXISTS" is written
 *   MYX_B_COMMENT:              write some default comments at the beginning of the file
 *   MYX_B_DONT_WRITE_FULL_PATH  don't preceed all identifiers by their schema id.
 *   MYX_B_LOCK_ALL_TABLES       All tables are locked with FLush read locks
 *                                 before doing anything
 *   MYX_B_SINGLE_TRANSACTION    Let the backup be done in a single
 *                                 transaction (for innodb),
 *                                 Mutually exclusive with MYX_B_LOCK_ALL_TABLES
 *   MYX_B_DISABLE_KEYS          write  ALTER TABLE %s DISABLE KEYS before the insers; and
 *                                ALTER TABLE %s ENABLE KEY after all inserts;
 *                                  makes inserting into myisam-tables a lot faster
 *
 *   MYX_B_COMPLETE_INSERTS       INSERTS are of the form "insert into
 *                                   (col1,col2) values (a,b)"
 *                                instead of "insert into values (a,b)" 
 *   MYX_B_ANSI_QUOTES=512        Identifiers are quoted with " instead of mysql's normal `
 *   MYX_B_ADD_LOCKS              Adds a LOCK TABLES statement before an insert-block and 
 *                                an UNLOCK TABLES afterwards
 ** RETURN VALUE
 *   0 on success, non-zero to indicate an error
 * NOTES
 *   If a file with filename already exists, it will be overwritten.
 *
 *----------------------------------------------------------------------
 */
MYX_BACKUP_ERROR myx_make_backup(MYSQL *mysql, const char *filename,
                                 MYX_BACKUP_CONTENT *content,
                                 MYX_BACKUP_TYPE backup_type, int options,
                                 int callback_interval,
                                 int (*progress_report)
                                    (
                                      const char* curr_tbl_name,
                                      int num_tables,
                                      int num_tables_processed,
                                      int num_rows,
                                      int num_rows_processed,
                                      void *user_data
                                    ),
                                 void *user_data)
{
  if (backup_type == MYX_BT_SQL_SCRIPT)
  {
    MYX_BS_STATUS *bs_status;
    unsigned int i;
    MYX_BACKUP_ERROR error= MYX_BACKUP_NO_ERROR;

    bs_status= myx_new_bs_status(mysql, filename, content, options, &error);
    if (bs_status == NULL)
      return error;

    bs_status->progress_report= progress_report;
    bs_status->user_data= user_data;
    bs_status->report_interval= callback_interval;
    
    if (initialize_backup(bs_status, &error) != 0)
    {
      myx_free_bs_status(bs_status);
      return error;
    };

    // Make sure output is quoted.
    if (myx_mysql_query(bs_status->mysql, "SET SQL_QUOTE_SHOW_CREATE=1"))
      return MYX_BACKUP_SERVER_ERROR;

    // Determine total number of objects to dump
    bs_status->total_count= bs_status->backup_content->tables_num;
    bs_status->count= 0;
    bs_status->current_index= 0;
    bs_status->previous_index= -1;

    // Start off with dummy tables for all views to solve reference problems.
    if (!(bs_status->options & MYX_B_NO_CREATES))
    {
      // Add number of dummy tables to total_count to have a correct progress report.
      for (i= 0; i < bs_status->backup_content->tables_num; i++)
      {
        if (bs_status->backup_content->tables[i].flags & MYX_BTF_IS_VIEW)
          bs_status->total_count++;
      };

      if (dump_dummy_tables_for_views(bs_status, &error) != 0)
      {
        myx_free_bs_status(bs_status);
        return error;
      };
    };

    if (dump_tables(bs_status, &error) != 0)
    {
      myx_free_bs_status(bs_status);
      return error;
    };

    if (!(bs_status->options & MYX_B_NO_CREATES))
    {
      if (mysql_full_version_is_later_or_equal_than(bs_status->mysql, 5, 0, 9))
      {
        if (dump_functions(bs_status, &error) != 0)
        {
          myx_free_bs_status(bs_status);
          return error;
        };

        if (dump_procedures(bs_status, &error) != 0)
        {
          myx_free_bs_status(bs_status);
          return error;
        };
      };

      if (dump_views(bs_status, &error) != 0)
      {
        myx_free_bs_status(bs_status);
        return error;
      };
    };
    
    if (bs_status->options & MYX_B_LOCK_ALL_TABLES)
    {
      if (myx_mysql_query(bs_status->mysql, "UNLOCK TABLES"))
      {
        error=  MYX_BACKUP_SERVER_ERROR;
        return -1;
      }
    }
    else
      if (((bs_status->options & MYX_B_SINGLE_TRANSACTION) || (bs_status->options & MYX_B_POINT_IN_TIME_BACKUP)) &&
        myx_mysql_query(bs_status->mysql, "COMMIT"))
      {
        error= MYX_BACKUP_SERVER_ERROR;
        return -1;
      };

    if (myx_mysql_query(bs_status->mysql, "/*!40101 SET sql_mode=@OLD_SQL_MODE */"))
    {
      error= MYX_BACKUP_SERVER_ERROR;
      return -1;
    };

    write_sql_file_footer(bs_status, &error);

    // These both members either where never used or already freed.
    bs_status->current_table_quoted= NULL;
    bs_status->current_schema_quoted= NULL;
    
    myx_free_bs_status(bs_status);
    return error;
  };
  
  return -1; /* unknown backup type */
}

/*
 *----------------------------------------------------------------------
 * Creates the MYX_BS_STATUS, that saves needed information for
 * the backup.
 *
 * SYNOPSIS
 *   content: specifies which tables are to be backed up
 *   options: 0 or a combination of options;
 *            if 0 the default-options will be used
 * DESCRIPTION
 *
 * RETURN VALUE
 *
 * NOTES
 * covered by unit tests
 *----------------------------------------------------------------------
 */
MYX_BS_STATUS *myx_new_bs_status(MYSQL *mysql, const char *filename,
                                 MYX_BACKUP_CONTENT *content,
                                 MYX_BACKUP_OPTIONS options, 
                                 MYX_BACKUP_ERROR *error)
{
  MYX_BS_STATUS *b;
  FILE *file;
  unsigned int i;
  char *qs;

  *error= MYX_BACKUP_NO_ERROR;
  if ((options & MYX_B_LOCK_ALL_TABLES) && (options & MYX_B_SINGLE_TRANSACTION))
  {
    *error= MYX_BACKUP_ILLEGAL_OPTION;
    return NULL;
  }

  if ((options & MYX_B_ANSI_QUOTES) && 
      !mysql_version_is_later_or_equal_than(mysql,4,1))
  {
    *error= MYX_BACKUP_CANNOT_SET_ANSI_QUOTES;
    return NULL;
  }

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64) || !defined(O_LARGEFILE)
  file= myx_fopen(filename, "wb");
#else
  {
    char * local_filename;

    if ((local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)))
    { // allow creation of files bigger than 2G
      int fd= open(local_filename, O_WRONLY|O_CREAT|O_TRUNC|O_LARGEFILE, S_IRUSR|S_IWUSR);
      if (fd >= 0)
        file= fdopen(fd, "wb");
      g_free(local_filename);
    }
  }
#endif
  if (file == NULL)
  {
    *error=  MYX_BACKUP_CANT_OPEN_FILE;
    return NULL;
  }
  b= g_malloc0(sizeof(MYX_BS_STATUS) );
  b->sql_file= file;

  b->mysql= mysql;
  if(options & MYX_B_COMPLETE_SCHEMATAS)
  {
    if(mysql_version_is_later_or_equal_than(mysql, 5, 0)) 
    {
      b->backup_content= select_all_tables_5(mysql, content);
    }
    else
    {
      b->backup_content= select_all_tables_4(mysql, content);
    }
  }
  else
  {
    b->backup_content= copy_backup_content(content);
  }
  b->options= options;

  if (b->options == 0)
  {
    b->options= MYX_B_ADD_DROP_TABLE | MYX_B_COMMENT | MYX_B_DISABLE_KEYS|MYX_B_ADD_LOCKS;
  }

  b->quote_char= (b->options & MYX_B_ANSI_QUOTES) ? '"' : '`';
  b->qualified= (b->options & MYX_B_DONT_WRITE_FULL_PATH) ? 0 : 1;

  for(i= 0; i < b->backup_content->tables_num; i++)
  {
    qs= quote_identifier(b->backup_content->tables[i].schema, b->quote_char);

    if ((b->backup_content->tables[i].flags != MYX_BTF_IS_TABLE) ||
      check_if_ignore_table(mysql, qs, b->backup_content->tables[i].table))
    {
      // If any of the special types flags is set already then we have a view, SP or SF here, which do not require
      // the backup of content.
      b->backup_content->tables[i].flags |= MYX_BTF_IGNORE_CONTENT;
    }
    g_free(qs);
  }

  if (b->options & MYX_B_SORT_TABLES)
  {
    qsort(b->backup_content->tables, b->backup_content->tables_num, sizeof(MYX_BACKUP_TABLE), compare_backup_tables);
  }

  return b;
}

/*
 *----------------------------------------------------------------------
 * Overwrites the current options for a backup-operation with the
 * options specified in the argument
 *
 * SYNOPSIS
 *
 * DESCRIPTION
 *  Should be called only right after myx_new_bs_status.
 *
 * RETURN VALUE
 *  If the options argument is negative, then the current options
 *  value of status is returned.
 *  The newly set option are returned otherwise.
 * NOTES
 *----------------------------------------------------------------------
 */
int myx_set_bs_options(MYX_BS_STATUS *status, int options)
{
  if (options >= 0)
    status->options= options;
  return status->options;
}

int myx_free_bs_status(MYX_BS_STATUS *b)
{
  if (b)
  {
    if (b->sql_file)
      fclose(b->sql_file);
    g_free(b->current_table_quoted);
    g_free(b->current_schema_quoted);
    g_free(b->extended_insert);
    myx_free_backup_content(b->backup_content);
    g_free(b);
  }
  return 0;
}

/**
 * @brief check if should NOT dump table's contents (e.g. MERGE table or VIEW)
 *
 * @param mysql mysql server connection handle
 * @param quoted_schema_name quoted name of the table to check
 * @param table_name unquoted name of the table to check
 * 
 * @return 1 if the table's content should be ignored.
 */
int check_if_ignore_table(MYSQL *mysql, const char *quoted_schema_name, const char *table_name)
{
  char buff[1024];
  MYSQL_RES *res;
  MYSQL_ROW row;
  int result= 1;

  sprintf(buff, "show table status from %s like \"%s\"", quoted_schema_name, table_name);
  if (mysql_query(mysql, buff))
  {
    return 0;      /* assume table is ok */
  }
  res = mysql_store_result(mysql);
  row= mysql_fetch_row(res);
  if (row != NULL)
  {
    if (!(row[1]))
      result= 1;
    else
    {
      if (strcmp(row[1], "MRG_MyISAM") &&
          strcmp(row[1], "MRG_ISAM")) {
        result= 0;
      }
    }
  };

  mysql_free_result(res);
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns all catalogs/schemas/tables stored in the backup
 *
 * @param default_catalog_name If no catalog is given for a table in the given file, it is assumed to be in the
 *                              default_catalog_name
 * @param default_schema_name If a table in the given file has no schema(=database) then it is assumed that the table
 *                            lies in the default_schema_name
 * @param report_interval Specifies the number of bytes that have to be read before progress report is called (again).
 * @parma progress_report call-back function returns 0 if we should continue our work progress_report may be NULL
 * @param user_data user specified data to be passed back when progress_report is called
 * @param error used to return an error code to the caller
 * @param forced_schema If 1 then the user wants to restore into a schema different from that in the dump.
 * @param ignore_errors if != 0 then even if the dump file was not created by MA its content is attempted to read
 *
 * @return The backup content structure created from the content of the dump file.
 *
 * @note covered by unit tests.
 */
MYX_BACKUP_CONTENT *
myx_get_backup_content(const char *filename, const char *filename_charset, MYX_BACKUP_TYPE backup_type,
                       int report_interval,
                       int (*progress_report) (bigint bytes_read,
                                               bigint bytes_total,
                                               void *user_data),
                      void (*report_warning)(const char *msg, void *user_data),
                      void *user_data, MYX_BACKUP_ERROR *error, int forced_schema, int ignore_errors )
{
  MYX_BACKUP_CONTENT *content;

  if (backup_type == MYX_BT_SQL_SCRIPT)
  {
    MYX_BCS_STATUS *status;
    bigint next_call, file_size;

    *error= 0;

    status= myx_new_bcs_status(filename, filename_charset, report_warning, user_data, error, ignore_errors);
    if (status == NULL)
      return NULL;

    file_size= get_file_size(filename);
    next_call= report_interval;

    while (myx_get_backup_content_from_sql_file_incremental(status, 1, forced_schema, ignore_errors, error) > 0)
    {
      if ((*error != MYX_BACKUP_NO_ERROR) && (report_warning))
      {
        char* message= myx_get_backup_error_string(*error);
        report_warning(message, user_data);
        g_free(message);
      };


      if ((progress_report) && (status->we.bytes_read >= next_call))
      {
        next_call+= report_interval;

        // exception point
        if ((*progress_report)(status->we.bytes_read, file_size, user_data))
        {
          myx_free_bcs_status(status,1);
          return NULL;
        }
      }
    }

    if (*error != MYX_BACKUP_NO_ERROR)
    {
      if (report_warning != NULL)
      {
        char* message= myx_get_backup_error_string(*error);
        report_warning(message, user_data);
        g_free(message);
      };
      
      myx_free_bcs_status(status, 1);

      return NULL;
    }

    if (progress_report && (file_size % report_interval))
      (*progress_report)(status->we.bytes_read, file_size, user_data);

    content= status->backup_content;
    myx_free_bcs_status(status, 0);
    
    return content;
  }

  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_backup_content(MYX_BACKUP_CONTENT *backup)
{
  if (backup)
  {
    MYX_BACKUP_TABLE * table= backup->tables;
    MYX_BACKUP_TABLE * tables_end= table + backup->tables_num;
    for (; table!=tables_end; table++)
      free_backup_table_content(table);
    g_free(backup->tables);
    g_free(backup);
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

const char* dump_magic = "-- MySQL Administrator dump ";

MYX_BCS_STATUS* myx_new_bcs_status(const char *filename, const char *filename_charset, 
  void (*report_warning)(const char *msg, void *user_data),
  void *user_data, MYX_BACKUP_ERROR *error, int force)
{
  MYX_BCS_STATUS * b;
  int skip_BOM= 0;
  MYX_INTL_FILE * intl_file= myx_new_intl_file(filename, filename_charset, (MYX_LIB_ERROR*)error);
  char buffer[100];
  if (!intl_file)
    return NULL;

  // Read the 1st line of the file and check if it's from mysqldump.
  if (myx_intl_fgets(buffer, sizeof(buffer), intl_file->file))
  {
    // Check if there is a BOM in the file.
    // 0xFFFE is a MSB-first BOM
    // 0xFEFF is a LSB-first BOM
    // 0xEF 0xBB 0xBF is a UTF-8 BOM
    unsigned short bom= *(unsigned short*)buffer;
    if (bom == 0xFFFE || bom == 0xFEFF || bom == 0xBBEF)
    {
      if (bom == 0xBBEF)
        skip_BOM= 3;
      else
        skip_BOM= 2;
    };

    if (strncmp(&buffer[skip_BOM], dump_magic, strlen(dump_magic)) != 0)
    {
      if (force == 0)
      {
        *error= MYX_BACKUP_FILE_IS_NOT_MA_DUMP;
        myx_free_intl_file(intl_file);
        return NULL;
      }
      else
        if (report_warning)
          report_warning(_("Warning: The dump file was not created with MySQL Administrator. Results are unpredictable"),
            user_data);
    }
  }
  myx_intl_rewind(intl_file->file);
  myx_intl_skip(intl_file->file, skip_BOM);

  b= g_malloc0( sizeof(MYX_BCS_STATUS) );
  b->intl_file= intl_file;

  b->backup_content= g_malloc0(sizeof(MYX_BACKUP_CONTENT) );
  myx_init_sql_parse_environment(&b->we);
  b->we.bytes_read= skip_BOM;

  prepare_line_is_create_statement(&b->re_create_table, &b->pe_create_table);
  prepare_line_is_create_index_statement(&b->re_create_index, &b->pe_create_index); 
  prepare_line_is_create_view_statement(&b->re_create_view, &b->pe_create_view);
  prepare_line_is_create_proc_statement(&b->re_create_proc, &b->pe_create_proc);
  prepare_line_is_create_func_statement(&b->re_create_func, &b->pe_create_func);
  prepare_line_is_create_trigger_statement(&b->re_create_trigger, &b->pe_create_trigger);
  prepare_line_is_db_use_statement(&b->re_db_use, &b->pe_db_use);

  b->default_schema_name= DEFAULT_SCHEMA_NAME;
  b->default_catalog_name= DEFAULT_CATALOG_NAME;

  return b;
}

//----------------------------------------------------------------------------------------------------------------------

int myx_free_bcs_status(MYX_BCS_STATUS *b, int free_backup_content)
{
  if (b != NULL)
  {
    myx_free_intl_file(b->intl_file);
    myx_done_sql_parse_environment(&b->we);
    g_free(b->cur_db_name);

    free_pcre_data(b->re_create_table, b->pe_create_table);
    free_pcre_data(b->re_create_index, b->pe_create_index);
    free_pcre_data(b->re_create_view, b->pe_create_view);
    free_pcre_data(b->re_create_proc, b->pe_create_proc);
    free_pcre_data(b->re_create_func, b->pe_create_func);
    free_pcre_data(b->re_create_trigger, b->pe_create_trigger);
    free_pcre_data(b->re_db_use, b->pe_db_use);

    if (free_backup_content)
      g_free(b->backup_content);

    g_free(b);
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Gets a certain amount of statements from the dump and analyzes them.
 *
 * @param bs_status The current restore status, containg important internal info + the parsed content.
 * @param granularity Number of sql-statements
 * @param forced_schema If set to 1 then no explicit schema qualifier must be found in the dump. If that happens then
 *                      analyzation is stopped with an error. If set to 0 then schema qualifiers don't matter.
 * @param ignore_errors If set then errors during analyzation are ignored otherwise processing is stopped and an
 *                      error code is returned.
 *
 * @return The number of successfully processed sql-statements, 0 if there are no more sql-statements or
 *  a negative number in case of an error
 *
 * @note covered by unit tests
 */
int myx_get_backup_content_from_sql_file_incremental(MYX_BCS_STATUS *bc_status, int granularity, int forced_schema,
  int ignore_errors, MYX_BACKUP_ERROR *error)
{
  int len, tables_found;
  char buffer[BACKUP_CONTENT_BUFFER_LEN];
  char *bufptr= (char*)buffer;
  int buffer_len= sizeof(buffer);
  int count= 0;
  int flags= 0;
  MYX_BACKUP_TABLE *bt;
  char *identifier;
  int has_prefix;
  MYX_LIB_ERROR library_error = MYX_NO_ERROR;

  *error= MYX_BACKUP_NO_ERROR;
  while (count < granularity)
  {
    len= myx_get_next_sql_statement_file(&bc_status->we, bc_status->intl_file, &bufptr, &buffer_len, 0, 0, 0, 0,
      &library_error);
    if (len == 0 || library_error != MYX_NO_ERROR)
      break;

    identifier= NULL;

    // Create table
    if (check_statement_and_prefix(buffer,len, bc_status->re_create_table, bc_status->pe_create_table, 2, &has_prefix,
      &identifier))
      flags= MYX_BTF_IS_TABLE;
    else
    {
      // Create view
      if (check_statement_and_prefix(buffer,len, bc_status->re_create_view,bc_status->pe_create_view, 4, &has_prefix,
        &identifier))
        flags= MYX_BTF_IS_VIEW;
      else
      {
        // Create procedure
        if (check_statement_and_prefix(buffer,len, bc_status->re_create_proc,bc_status->pe_create_proc, 2, &has_prefix,
          &identifier))
          flags= MYX_BTF_IS_PROCEDURE;
        else
        {
          // Create function
          if (check_statement_and_prefix(buffer,len, bc_status->re_create_func,bc_status->pe_create_func, 2, &has_prefix,
            &identifier))
            flags= MYX_BTF_IS_FUNCTION;
          else
          {
            // Create function
            if (check_statement_and_prefix(buffer,len, bc_status->re_create_trigger,bc_status->pe_create_trigger, 4,
              &has_prefix, &identifier))
              flags= MYX_BTF_IS_TRIGGER;
          };
        };
      };
    };

    if (identifier != NULL)
    {
      const char* schema;
      char* unquoted;

      // Set a flag so that the UI knows we found objects with explicit schema names (might prevent some restore options).
      if (has_prefix && forced_schema)
      {
        *error= MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE;
        break;
      };

      // Check if there is already an object with that name. If so it usually means it was created first temporarily
      // (maybe even with a different object type, e.g. a dummy table for a view) and is now about to be
      // finally created (e.g. the temporary table is dropped and now the real view is created).
      schema= bc_status->cur_db_name ? bc_status->cur_db_name : bc_status->default_schema_name;
      unquoted= unquote_identifier(identifier);
      bt = get_object_index(bc_status->backup_content, unquoted, schema, bc_status->default_catalog_name);
      if (bt == NULL)
      {
        tables_found= ++(bc_status->backup_content->tables_num);

        bc_status->backup_content->tables= g_realloc(bc_status->backup_content->tables, tables_found * sizeof(MYX_BACKUP_TABLE));
        bt= bc_status->backup_content->tables + tables_found-1;

        bt->catalog= g_strdup(bc_status->default_catalog_name);
        bt->schema= g_strdup(schema);
        bt->table= unquoted;
      }
      else
        g_free(identifier);
      bt->flags= flags;
    }
    else
    {
      identifier= check_statement(buffer, len, bc_status->re_db_use, bc_status->pe_db_use, 2, 1);
      if (identifier != NULL) /* USE */
      {
        g_free(bc_status->cur_db_name);
        bc_status->cur_db_name= unquote_identifier(identifier);
      };
    };
    count++;
  }

  if (library_error != MYX_NO_ERROR)
    *error = error_mapping[library_error];
  
  return count;
}

//----------------------------------------------------------------------------------------------------------------------

/* RESTORE-BACKUP Functions */

void store_old_cs_set_utf8(MYSQL * mysql)
{
  myx_mysql_query(mysql,
                  "SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT");
  myx_mysql_query(mysql,
                  "SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS");
  myx_mysql_query(mysql,
                  "SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION");
  myx_mysql_query(mysql,"SET NAMES utf8");
}

void restore_old_cs(MYSQL * mysql)
{
  myx_mysql_query(mysql,
                  "SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT");
  myx_mysql_query(mysql,
                  "SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS");
  myx_mysql_query(mysql,
                  "SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION");
}

/*
 *----------------------------------------------------------------------
 * Restores a backup with the given filename into the database that
 * is reached by the mysql-struct, but only the catalogs/schemas/tables
 * specified by content. The backup can be restored in a different catalog and
 * or schema.
 *
 * SYNOPSIS
 *   target_catalog: the catalog that the table should be restored in;
 *                   if NULL or '' the catalog in content will be used
 *   target_schema:  the schema that the table should be restored in;
 *                   if NULL or '' the schema in content will be used
 *   content:        specifies with tables (identified by tablename,schema-
 *                   name and catalogname) that are defined in filename
 *                   are to be restored
 *   progress_report,
 *   report_interval: See myx_get_backup_content
 *   report_warning: this function will be called to report a not-fatal
 *                   error. If NULL the warning is printed to stderr
 * DESCRIPTION
 *  Possible option values:
 *    -) MYX_RBS_FORCE:                 Continue even in case of an sql-error
 *    -) MYX_RBS_DONT_CREATE_TARGETS:   Don't create the target schema and/or
 *                                       target catalog if it doesn't exist
 * RETURN VALUE
 *  0 on success, errorcode otherwise
 *
 * @note covered by unit tests.
 *----------------------------------------------------------------------
 */
MYX_BACKUP_ERROR myx_restore_backup(MYSQL *mysql, const char *filename, 
                                    const char *filename_charset, 
                                    MYX_BACKUP_CONTENT *content, 
                                    const char *target_catalog, 
                                    const char *target_schema,
                                    MYX_BACKUP_TYPE backup_type, 
                                    int options,
                                    int report_interval,
                                    int (*progress_report)(bigint bytes_read,
                                                           bigint bytes_total,
                                                           void *user_data),
                                    void *ruser_data,
                                    void (*report_warning)(const char *msg,
                                                           void *user_data),
                                    void *wuser_data)
{
  MYX_BACKUP_ERROR error= MYX_BACKUP_NO_ERROR;

  if (backup_type == MYX_BT_SQL_SCRIPT)
  {
    MYX_RBS_STATUS *rs;
    bigint file_size;

    file_size= get_file_size(filename);

    rs= myx_new_rbs_status(mysql, filename, filename_charset, content, target_catalog, target_schema, options, &error,
      report_warning, wuser_data);
    if (rs == NULL)
      return error;

    if (mysql_full_version_is_later_or_equal_than(mysql, 4, 1, 1))
      store_old_cs_set_utf8(mysql);

    while (myx_restore_backup_from_sql_file_incremental(rs, progress_report, file_size, ruser_data, &error) > 0)
      ;

    if(error)
    {
      myx_set_mysql_error(g_strdup(mysql_error(mysql)), mysql_errno(mysql));
    }

    if (mysql_full_version_is_later_or_equal_than(mysql, 4, 1, 1))
      restore_old_cs(mysql);

    if ((file_size % report_interval) && (progress_report))
      (*progress_report)(rs->we.bytes_read, file_size, ruser_data);

    myx_free_rbs_status(rs);

    return error;
  }

  return 1;
}

MYX_RBS_STATUS *myx_new_rbs_status(MYSQL *mysql, const char *filename,
                                   const char *filename_charset,
                                   MYX_BACKUP_CONTENT *content,
                                   const char *target_catalog,
                                   const char *target_schema,
                                   int options, MYX_BACKUP_ERROR *error,
                                   void(*report_warning)(const char *msg,
                                                         void *user_data),
                                   void *user_data )
{
  MYX_RBS_STATUS *rs;
  const char *error_str;
  int erroffset;
  MYX_LIB_ERROR library_error = MYX_NO_ERROR;

  rs= g_malloc0( sizeof(MYX_RBS_STATUS) );

  rs->intl_file= myx_new_intl_file(filename, filename_charset, &library_error);
  if (rs->intl_file == NULL)
  {
    *error = error_mapping[library_error];
    g_free(rs);
    return NULL;
  }

  rs->buffer= g_try_malloc(INITIAL_BUFFER_LENGTH);
  if (rs->buffer == NULL)
  {
    *error= MYX_BACKUP_MALLOC_FAILED;
    g_free(rs);
    return NULL;
  }
  rs->buffer_len= INITIAL_BUFFER_LENGTH;

  myx_init_sql_parse_environment(&rs->we);

  prepare_line_is_create_statement(&rs->re_create_table, NULL);
  prepare_line_is_create_index_statement(&rs->re_create_index, NULL);
  prepare_line_is_create_view_statement(&rs->re_create_view, NULL);
  prepare_line_is_create_proc_statement(&rs->re_create_proc, NULL);
  prepare_line_is_create_func_statement(&rs->re_create_func, NULL);
  prepare_line_is_create_trigger_statement(&rs->re_create_trigger, NULL);
  prepare_line_is_db_use_statement(&rs->re_db_use, NULL);

  rs->mysql= mysql;

  // In Delphi there are no NULL-Strings- there is
  // only a string with a length of 0
  // => treat '' the same as NULL strings
  rs->target_catalog= !target_catalog || !(*target_catalog)
                      ? NULL : target_catalog;
  rs->target_schema= !target_schema || !(*target_schema)
                      ? NULL : target_schema;

  rs->backup_content= content;
  rs->current_schema_in_sqlfile= g_strdup(DEFAULT_SCHEMA_NAME);
  rs->current_catalog_in_sqlfile= g_strdup(DEFAULT_CATALOG_NAME);
  rs->current_schema_in_db= g_strdup(" ");
  rs->options= options;
  rs->report_warning= report_warning;
  rs->report_warning_data= user_data;

  // setup the regular expressions
  // drop table may only contain one table at the moment
  rs->re_drop_table= pcre_compile("drop\\s+table\\s+" IF_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_drop_view= pcre_compile("drop\\s+view\\s+" IF_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_drop_proc= pcre_compile("drop\\s+procedure\\s+" IF_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_drop_func= pcre_compile("drop\\s+function\\s+" IF_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_drop_trigger= pcre_compile("drop\\s+trigger\\s+" IF_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_drop_index= pcre_compile("drop\\s+index\\s+(" IDENTIFIER_PCRE ")", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_drop_database= pcre_compile("drop\\s+(?:schema|database)\\s+" IF_EXISTS "(" IDENTIFIER_PCRE ")", PCRE_DEFAULT,
    &error_str, &erroffset, NULL);
  rs->re_insert= pcre_compile("insert\\s+into\\s+" QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_alter= pcre_compile("(?:\\/\\*\\!\\d+\\s+)?alter\\s+table\\s+" QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT,
    &error_str, &erroffset, NULL);
  rs->re_lock= pcre_compile("lock\\s+tables\\s+" QUALIFIED_IDENTIFIER_PCRE "\\s+write\\s*", PCRE_DEFAULT, &error_str,
    &erroffset, NULL);
  rs->re_unlock= pcre_compile("unlock\\s+tables", PCRE_DEFAULT, &error_str, &erroffset, NULL);

  rs->re_charset_set1= pcre_compile(".*(SET).*CHARACTER_SET_CLIENT.*", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_charset_set2= pcre_compile(".*(SET).*NAMES.*", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_charset_set3= pcre_compile(".*(SET).*CHARACTER_SET_RESULTS.*", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_charset_set4= pcre_compile(".*(SET).*COLLATION_CONNECTION.*", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_charset_set5= pcre_compile(".*(SET).*CHARACTER_SET_CONNECTION.*", PCRE_DEFAULT, &error_str, &erroffset, NULL);

  rs->re_set= pcre_compile("(?:\\/\\*\\!\\d+\\s+)?(SET)", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_create_database= pcre_compile("create\\s+(?:database|schema)\\s+" IF_NOT_EXISTS "(" IDENTIFIER_PCRE ")",
    PCRE_DEFAULT, &error_str, &erroffset, NULL);

  rs->re_delimiter= pcre_compile(".*?delimiter\\s+(\\S+)", PCRE_DEFAULT, &error_str, &erroffset, NULL);
  rs->re_commit= pcre_compile(".*?commit", PCRE_DEFAULT, &error_str, &erroffset, NULL);

  if (!rs->re_drop_table || !rs->re_insert || !rs->re_alter ||
      !rs->re_lock || !rs->re_unlock ||
      !rs->re_charset_set1 || !rs->re_charset_set2 ||
      !rs->re_charset_set3 || !rs->re_charset_set4 || !rs->re_charset_set5 ||
      !rs->re_set || !rs->re_create_database || !rs->re_delimiter)
  {
    myx_free_intl_file(rs->intl_file);
    myx_free_rbs_status(rs);
    *error= MYX_BACKUP_PCRE_ERROR;

    return NULL;
  };
  *error = error_mapping[library_error];

  return rs;
}

void ensure_charset_present(char **pbuf, int *plen, const char *defcharset)
{
  int vector[6];
  const char *error;
  int erroffset;
  char *buf, *c;
  size_t cslen;

  pcre *re= pcre_compile(".*?DEFAULT\\s+CHARSET=[a-zA-Z0-9]+.*", /* the pattern */
                        PCRE_CASELESS|PCRE_MULTILINE,                   /* default options */
                        &error,                                         /* for error message */
                        &erroffset,                                     /* for error offset */
                        NULL);                                          /* use default character tables */
  if(pcre_exec(re, NULL, *pbuf, *plen, 0, 0, vector, 6) > 0)
  {
    // DEFAULT CHARSET is present in the statement
    return;
  }
  cslen= strlen (defcharset);
  buf= g_malloc((gulong)(*plen + cslen + 32)); // 128 fro "DEFAULT CHARSET="
  strcpy(buf, *pbuf);
  for(c= buf + *plen - 1; (*c == ' ') && (c > buf); c--);
  strcpy(c, " DEFAULT CHARSET=");
  c += sizeof(" DEFAULT CHARSET=")-1;
  strcpy(c, defcharset);
  c += cslen;
  strcpy(c, ";");
  g_free(*pbuf);
  *pbuf= buf;
  *plen= (int)strlen(buf);
}

/**
 * @note covert by unit tests
 */
int myx_free_rbs_status(MYX_RBS_STATUS *rs)
{
  if (rs)
  {
    myx_free_intl_file(rs->intl_file);
    myx_done_sql_parse_environment(&rs->we);

    free_pcre_data(rs->re_create_table, NULL);
    free_pcre_data(rs->re_create_index, NULL);
    free_pcre_data(rs->re_create_view, NULL);
    free_pcre_data(rs->re_create_proc, NULL);
    free_pcre_data(rs->re_create_func, NULL);
    free_pcre_data(rs->re_create_trigger, NULL);
    free_pcre_data(rs->re_db_use, NULL);

    g_free(rs->current_schema_in_sqlfile);
    g_free(rs->current_catalog_in_sqlfile);

    pcre_free(rs->re_drop_table);
    pcre_free(rs->re_drop_view);
    pcre_free(rs->re_drop_func);
    pcre_free(rs->re_drop_proc);
    pcre_free(rs->re_drop_trigger);
    pcre_free(rs->re_drop_index);
    pcre_free(rs->re_drop_database);
    pcre_free(rs->re_insert);
    pcre_free(rs->re_alter);
    pcre_free(rs->re_lock);
    pcre_free(rs->re_unlock);
    pcre_free(rs->re_charset_set1);
    pcre_free(rs->re_charset_set2);
    pcre_free(rs->re_charset_set3);
    pcre_free(rs->re_charset_set4);
    pcre_free(rs->re_charset_set5);
    pcre_free(rs->re_set);
    pcre_free(rs->re_create_database);
    pcre_free(rs->re_commit);
    pcre_free(rs->re_delimiter);

    g_free(rs->buffer);
    g_free(rs);
  }
  return 0;
}

/* returns the number of successfully processed sql-statements
 * 0 if there is no more sql-statement
 * a negative number in case of an error
 */
int myx_restore_backup_from_sql_file_incremental(
                  MYX_RBS_STATUS *rs,
                  int (*progress_report)(bigint bytes_read,
                                         bigint bytes_total,
                                         void *user_data),
                  bigint file_size,
                  void *user_data,
                  MYX_BACKUP_ERROR *error)
{
  int len;
  int count, found;
  char *tmp, *create;
  char *warning;
  MYX_LIB_ERROR library_error = MYX_NO_ERROR;

  /* We will only execute statements we know. Otherwise we could do something
   * bad without knowing it.
   */
  for (count= 0;
       count < 1 &&
       (len= myx_get_next_sql_statement_file(&rs->we,rs->intl_file,
                                             &rs->buffer,
                                             &rs->buffer_len,1,
                                             progress_report,
                                             file_size,
                                             user_data,
                                             &library_error)) > 0;
       count++)
  {
    if (library_error != MYX_NO_ERROR)
    {
      *error = error_mapping[library_error];
      return -1;
    };

    assert(rs->buffer);
    create= NULL;

    /* Determine actual command. */
    tmp= check_statement(rs->buffer,len, rs->re_insert, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_table, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_view, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_proc, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_func, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_trigger, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_drop_index, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_alter, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_lock, NULL, 5, 2);
    if (tmp == NULL)
      tmp= create= check_statement(rs->buffer,len, rs->re_create_table, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_create_view, NULL, 5, 4);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_create_proc, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_create_func, NULL, 5, 2);
    if (tmp == NULL)
      tmp= check_statement(rs->buffer,len, rs->re_create_trigger, NULL, 10, 4);

    if (tmp != NULL)
    {
      if (create && mysql_version_is_later_or_equal_than(rs->mysql, 4, 1))
      {
        ensure_charset_present(&rs->buffer, &len, rs->intl_file->charset);
        rs->buffer_len= len;
      }
      tmp= unquote_identifier(tmp);
      found= find_table(rs->backup_content, tmp, rs->current_schema_in_sqlfile, rs->current_catalog_in_sqlfile);
      g_free(tmp);
      if (found && interact_with_server(rs, rs->buffer, len, error))
        return -1;
    }
    else
    {
      tmp= check_statement(rs->buffer,len, rs->re_create_index, NULL, 10, 2);
      if (tmp != NULL)
      {
        g_free(tmp);
        if (interact_with_server(rs, rs->buffer, len, error))
          return -1;
      }
      else
      {
        tmp= check_statement(rs->buffer, len, rs->re_unlock, NULL, 10, 0);
        if (tmp != NULL)
        {
          g_free(tmp);
          if (interact_with_server(rs, rs->buffer, len, error))
            return -1;
        }
        else
        {
          /* SET characterset */
          tmp= check_statement(rs->buffer,len,rs->re_charset_set1,NULL, 10, 1);
          if (tmp == NULL)
            tmp= check_statement(rs->buffer,len,rs->re_charset_set2,NULL, 10, 1);
          if (tmp == NULL)
            tmp= check_statement(rs->buffer,len,rs->re_charset_set3,NULL, 10, 1);
          if (tmp == NULL)
            tmp= check_statement(rs->buffer,len,rs->re_charset_set4,NULL, 10, 1);
          if (tmp == NULL)
            tmp= check_statement(rs->buffer,len,rs->re_charset_set5,NULL, 10, 1);
          if (tmp != NULL)
          {
            g_free(tmp); /* Don't send it to the server! */
          }
          else
          {
            /* SET */
            tmp= check_statement(rs->buffer,len,rs->re_set, NULL, 10, 1);
            if (tmp != NULL)
            {
              g_free(tmp);
              if (comfort_query(rs, rs->buffer, len, error))
                return -1;
            }
            else
            {
              /* USE */
              tmp= check_statement(rs->buffer,len,rs->re_db_use, NULL, 10, 1);
              if (tmp != NULL)
              {
                tmp= unquote_identifier(tmp);
                g_free(rs->current_schema_in_sqlfile);
                rs->current_schema_in_sqlfile= tmp;

                if (!rs->target_schema && /* the user has not supplied a target schema */
                    in_backup_content(tmp, rs->backup_content) &&
                    comfort_query(rs, rs->buffer, len, error))
                {                         /* there is at least one table that */
                  return -1;              /*   should get restored in this schema */
                }
              }
              else
              {
                /* CREATE DATABASE */
                tmp= check_statement(rs->buffer,len, rs->re_create_database, NULL, 3, 1);
                if (tmp != NULL)
                {
                  unquote_identifier(tmp);
                  if (!rs->target_schema && /* the user has not supplied a target schema */
                      in_backup_content(tmp, rs->backup_content) &&
                      comfort_query(rs, rs->buffer, len, error))
                  {                         /* there is at least one table that */
                    g_free(tmp);
                    return -1;              /*   should get restored in this schema */
                  }
                  g_free(tmp);
                }
                else
                {
                  /* DROP DATABASE */
                  tmp= check_statement(rs->buffer,len, rs->re_drop_database, NULL, 10, 1);
                  if (tmp != NULL)
                  {
                    // Just consume the statement. Nothing else to do here.
                    g_free(tmp);
                  }
                  else
                  {
                     /* DELIMITER */
                    tmp= check_statement(rs->buffer,len, rs->re_delimiter, NULL, 10, 1);
                    if (tmp != NULL)
                    {
                      // Delimiter was already determined by the myx_get_next_sql_statement_file call above.
                      g_free(tmp);
                    }
                    else
                    {
                      /* COMMIT */
                      tmp= check_statement(rs->buffer, len, rs->re_commit, NULL, 10, 0);
                      if (tmp != NULL)
                      {
                        // Nothing to do with that. Just recognize it.
                        g_free(tmp);
                      }
                      else
                      {
                        /*Unknown statement*/
                        char buffer[500];
                        sprintf(buffer, "Warning: Do not know how to handle this statement at line %d:\n" ,
                          rs->we.stmt_begin_line);
                        warning= g_strconcat(buffer, rs->buffer,"\nIgnoring this statement. Please file"
                          " a bug-report including the statement if this statement should be recognized.\n",NULL);

                        if (rs->report_warning)
                        {
                          (*rs->report_warning)(warning, rs->report_warning_data);
                        }
                        else
                        {
                          fprintf(stderr,"%s", warning);
                        }
                        g_free(warning);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  return count;
}

/*
 * ******************
 *
 * Private functions
 *
 * ******************
 */

/* returns true if backup_content contains a table in schema schema_name */
int in_backup_content(const char *schema_name,
                             MYX_BACKUP_CONTENT *backup_content)
{
  MYX_BACKUP_TABLE * table= backup_content->tables;
  MYX_BACKUP_TABLE * tables_end= table + backup_content->tables_num;
  for (; table!=tables_end; table++)
  {
    if (        table->schema &&
        !strcmp(table->schema, schema_name))
    {
      return 1;
    }
  }
  return 0;
}

MYX_BACKUP_PROFILE *read_in_backup_profile_1_1(xmlNodePtr backup_node)
{
  MYX_BACKUP_PROFILE *bprofile;
  xmlNodePtr cur;
  xmlDocPtr doc;

  doc= backup_node->doc;
  bprofile= g_malloc0(sizeof(MYX_BACKUP_PROFILE));
  bprofile->backup_content= g_malloc0(sizeof(MYX_BACKUP_CONTENT));

  for (cur= backup_node->children; cur; cur= cur->next)
  {
    try_to_get_string_field(doc,cur, "profile_name",&bprofile->profile_name);
    try_to_get_string_field(doc,cur, "last_used",   &bprofile->last_used);
    try_to_get_int_field   (doc,cur, "options",     &bprofile->options);
    try_to_get_int_field   (doc,cur, "backup_type",
                            (int*)&bprofile->backup_type);

    if ( !xmlStrcmp(cur->name, (xmlChar*)"tables") &&  cur->type == XML_ELEMENT_NODE)
    {
      xmlNodePtr cur2;
      MYX_BACKUP_TABLE * table;
      bprofile->backup_content->tables_num= get_child_count(cur,(xmlChar*)"table");
      table= bprofile->backup_content->tables=
                         g_malloc0(sizeof(MYX_BACKUP_TABLE) *
                                   bprofile->backup_content->tables_num);
      for (cur2= cur->children; cur2; cur2= cur2->next)
      {
        if (!xmlStrcmp(cur2->name, (xmlChar*)"table") )
        {
          read_in_table(cur2, table);
          table++;
        }
      }
    }
  }
  return bprofile;
}

MYX_BACKUP_PROFILE *read_in_backup_profile_1_2(xmlNodePtr backup_node)
{
  MYX_BACKUP_PROFILE *bprofile;
  xmlNodePtr cur;
  xmlDocPtr doc;

  doc= backup_node->doc;
  bprofile= g_malloc0(sizeof(MYX_BACKUP_PROFILE));
  bprofile->backup_content= g_malloc0(sizeof(MYX_BACKUP_CONTENT));

  for (cur= backup_node->children; cur; cur= cur->next)
  {
    try_to_get_string_field(doc,cur, "profile_name",&bprofile->profile_name);
    try_to_get_string_field(doc,cur, "last_used",   &bprofile->last_used);
    try_to_get_int_field   (doc,cur, "options",     &bprofile->options);
    try_to_get_int_field   (doc,cur, "backup_type",
                            (int*)&bprofile->backup_type);

    if ( !xmlStrcmp(cur->name, (xmlChar*)"entities") &&  cur->type == XML_ELEMENT_NODE)
    {
      xmlNodePtr cur2;
      MYX_BACKUP_TABLE * table;
      bprofile->backup_content->tables_num= get_child_count(cur,(xmlChar*)"entity");
      table= bprofile->backup_content->tables=
                         g_malloc0(sizeof(MYX_BACKUP_TABLE) *
                                   bprofile->backup_content->tables_num);
      for (cur2= cur->children; cur2; cur2= cur2->next)
      {
        if (!xmlStrcmp(cur2->name, (xmlChar*)"entity") )
        {
          read_in_entity(cur2, table);
          table++;
        }
      }
    }
  }
  return bprofile;
}

MYX_BACKUP_PROFILE *read_in_backup_profile_1_0(xmlNodePtr backup_node)
{
  MYX_BACKUP_PROFILE *bprofile;
  xmlNodePtr cur;
  xmlDocPtr doc;

  doc= backup_node->doc;
  bprofile= g_malloc0(sizeof(MYX_BACKUP_PROFILE));
  bprofile->backup_content= g_malloc0(sizeof(MYX_BACKUP_CONTENT));

  for (cur= backup_node->children; cur; cur= cur->next)
  {
    try_to_get_string_field(doc,cur, "profile_name",&bprofile->profile_name);
    try_to_get_string_field(doc,cur, "last_used",   &bprofile->last_used);
    try_to_get_int_field   (doc,cur, "options",     &bprofile->options);
    try_to_get_int_field   (doc,cur, "backup_type",
                            (int*)&bprofile->backup_type);

    if ( !xmlStrcmp(cur->name, (xmlChar*)"tables") &&  cur->type == XML_ELEMENT_NODE)
    {
      xmlNodePtr cur2;
      MYX_BACKUP_TABLE * table;
      bprofile->backup_content->tables_num= get_child_count(cur,(xmlChar*)"table");
      table= bprofile->backup_content->tables=
                         g_malloc0(sizeof(MYX_BACKUP_TABLE) *
                                   bprofile->backup_content->tables_num);
      for (cur2= cur->children; cur2; cur2= cur2->next)
      {
        if (!xmlStrcmp(cur2->name, (xmlChar*)"table") )
        {
          read_in_table(cur2, table);
          table++;
        }
      }
    }
  }

  return bprofile;
}


void read_in_table(xmlNodePtr table_node, MYX_BACKUP_TABLE *table)
{
  xmlNodePtr cur;
  xmlDocPtr doc;

  doc= table_node->doc;
  table->table= NULL;
  table->schema= NULL;
  table->catalog= NULL;
  table->flags= MYX_BTF_IS_TABLE;

  for (cur= table_node->children; cur; cur= cur->next)
  {
    try_to_get_string_field(doc,cur, "name",   &table->table);
    try_to_get_string_field(doc,cur, "schema", &table->schema);
    try_to_get_string_field(doc,cur, "catalog",&table->catalog);
  }
}

void read_in_entity(xmlNodePtr entity_node, MYX_BACKUP_TABLE *table)
{
  char *t= NULL;
  xmlNodePtr cur;
  xmlDocPtr doc;

  doc= entity_node->doc;
  table->table= NULL;
  table->schema= NULL;
  table->catalog= NULL;

  for (cur= entity_node->children; cur; cur= cur->next)
  {
    try_to_get_string_field(doc,cur, "name",   &table->table);
    try_to_get_string_field(doc,cur, "schema", &table->schema);
    try_to_get_string_field(doc,cur, "catalog",&table->catalog);
    try_to_get_string_field(doc,cur, "entity_type", &t);
    if(t)
    {
      if(strcmp(t, "view") == 0)
      {
        table->flags= MYX_BTF_IS_VIEW;
      } 
      else if(strcmp(t, "proc") == 0)
      {
        table->flags= MYX_BTF_IS_PROCEDURE;
      }
      else if(strcmp(t, "func") == 0)
      {
        table->flags= MYX_BTF_IS_FUNCTION;
      }
      else
      {
        table->flags= MYX_BTF_IS_TABLE;
      }
      g_free(t);
      t= 0;
    }
  }
}

GPtrArray *select_all_schemata(MYX_BACKUP_CONTENT *bc)
{
  unsigned int i;
  GPtrArray *array= g_ptr_array_new();
  GHashTable *hash= g_hash_table_new(g_str_hash, g_str_equal);
  
  for(i= 0; i < bc->tables_num; i++)
  {
    if(!g_hash_table_lookup(hash, bc->tables[i].schema))
    {
      MYX_BACKUP_TABLE *bt= g_malloc(sizeof(MYX_BACKUP_TABLE));
      bt->catalog= bc->tables[i].catalog;
      bt->schema= bc->tables[i].schema;
      bt->table= NULL;
      g_ptr_array_add(array, bt);
      g_hash_table_insert(hash, bc->tables[i].schema, bc->tables[i].schema);
    }
  }

  g_hash_table_destroy(hash);
  return array;
}

MYX_BACKUP_CONTENT* select_all_tables_4(MYSQL* mysql, 
                                               MYX_BACKUP_CONTENT *bc)
{
  unsigned int i, j;
  MYSQL_RES *rs1= NULL, *rs2= NULL, *rs3= NULL;
  MYSQL_ROW row;
  MYX_BACKUP_CONTENT *newbc= (MYX_BACKUP_CONTENT *)g_malloc(sizeof(MYX_BACKUP_CONTENT));

  GPtrArray *all_tables = g_ptr_array_new();
  GPtrArray *all_schemas = select_all_schemata(bc);

  for (i= 0; i < all_schemas->len; i++)
  {
    MYX_BACKUP_TABLE *bt= (MYX_BACKUP_TABLE *)g_ptr_array_index(all_schemas, i);

    rs1= NULL;
    if (mysql_select_db(mysql, bt->schema) == 0)
    {
      if (myx_mysql_query(mysql, "SHOW TABLES") == 0)
        rs1= mysql_store_result(mysql);
    };

    if (rs1 == NULL)
      return NULL;

    j= 0;
    do
    {
      MYX_BACKUP_TABLE *btnew;

      row= mysql_fetch_row(rs1);
      if (row == NULL)
        break;

      btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));
      btnew->catalog= bt->catalog;
      btnew->schema= bt->schema;
      btnew->table= row[0];
      btnew->flags= MYX_BTF_IS_TABLE;
      g_ptr_array_add(all_tables, btnew);
      ++j;
    }
    while (1);


    if(!j)  // empty schema
    {
      MYX_BACKUP_TABLE *btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));
      btnew->catalog= bt->catalog;
      btnew->schema= bt->schema;
      btnew->table= NULL;
      btnew->flags= 0;
      g_ptr_array_add(all_tables, btnew);
    }
  }

  newbc->tables = g_malloc(sizeof(MYX_BACKUP_TABLE)*all_tables->len);
  newbc->tables_num = all_tables->len;

  for(i= 0; i < all_tables->len; i++)
  {
    MYX_BACKUP_TABLE *bt= (MYX_BACKUP_TABLE *)g_ptr_array_index(all_tables, i);
    newbc->tables[i].catalog= g_strdup(bt->catalog);
    newbc->tables[i].schema= g_strdup(bt->schema);
    newbc->tables[i].table= g_strdup(bt->table);
    newbc->tables[i].flags= bt->flags;
    g_free(bt);
  }

  g_ptr_array_free(all_tables, 0);
  for(i= 0; i < all_schemas->len; i++)
  {
    g_free(g_ptr_array_index(all_schemas, i));
  }
  g_ptr_array_free(all_schemas, 0);

  if(rs1)
  {
    mysql_free_result(rs1);
  }
  if(rs2)
  {
    mysql_free_result(rs2);
  }
  if(rs3)
  {
    mysql_free_result(rs3);
  }
  return newbc;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_BACKUP_CONTENT* select_all_tables_5(MYSQL* mysql, MYX_BACKUP_CONTENT *bc)
{
  unsigned int i, j;
  MYSQL_RES *resultset= NULL;
  MYSQL_ROW row;
  char* query;
  MYX_BACKUP_CONTENT *newbc= (MYX_BACKUP_CONTENT *)g_malloc(sizeof(MYX_BACKUP_CONTENT));

  GPtrArray *all_tables = g_ptr_array_new();
  GPtrArray *all_schemas = select_all_schemata(bc);

  for (i= 0; i < all_schemas->len; i++)
  {
    MYX_BACKUP_TABLE *bt= (MYX_BACKUP_TABLE *)g_ptr_array_index(all_schemas, i);
    int fields;

    resultset= NULL;
    if (mysql_select_db(mysql, bt->schema) == 0)
    {
      // for mysql 5.0+
      if (mysql_query(mysql, "SHOW FULL TABLES") == 0)
        resultset= mysql_store_result(mysql);
      else
        // for < mysql 5.0
        if (mysql_query(mysql, "SHOW TABLES") == 0)
          resultset= mysql_store_result(mysql);

      j= 0;
      if (resultset != NULL)
      {
        fields= mysql_num_fields(resultset);
        do
        {
          MYX_BACKUP_TABLE *btnew;

          row= mysql_fetch_row(resultset);
          if (row == NULL)
            break;

          btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));
          btnew->catalog= g_strdup(bt->catalog);
          btnew->schema= g_strdup(bt->schema);
          btnew->table= g_strdup(row[0]);
          if (fields > 1 && strcmp2(row[1], "VIEW") == 0)
          {
            btnew->flags= MYX_BTF_IS_VIEW;
          }
          else
          {
            btnew->flags= MYX_BTF_IS_TABLE;
          }
          g_ptr_array_add(all_tables, btnew);
          ++j;
        }
        while (1);

        mysql_free_result(resultset);
      };

      resultset= NULL;
      query= g_strconcat("SHOW PROCEDURE STATUS WHERE DB='", bt->schema, "'", NULL);
      if (mysql_query(mysql, query) == 0)
        resultset= mysql_store_result(mysql);
      g_free(query);

      if (resultset != NULL)
      {
        do
        {
          MYX_BACKUP_TABLE *btnew;

          row= mysql_fetch_row(resultset);
          if (row == NULL)
            break;

          btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));

          btnew->catalog= g_strdup(bt->catalog);
          btnew->schema= g_strdup(bt->schema);
          btnew->table= g_strdup(row[1]);
          btnew->flags= MYX_BTF_IS_PROCEDURE;
          g_ptr_array_add(all_tables, btnew);
          ++j;
        }
        while (1);

        mysql_free_result(resultset);
      };

      resultset= NULL;
      query= g_strconcat("SHOW FUNCTION STATUS WHERE DB='", bt->schema, "'", NULL);
      if (mysql_query(mysql, query) == 0)
        resultset= mysql_store_result(mysql);
      g_free(query);

      if (resultset != NULL)
      {
        do
        {
          MYX_BACKUP_TABLE *btnew;

          row= mysql_fetch_row(resultset);
          if (row == NULL)
            break;

          btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));

          btnew->catalog= g_strdup(bt->catalog);
          btnew->schema= g_strdup(bt->schema);
          btnew->table= g_strdup(row[1]);
          btnew->flags= MYX_BTF_IS_FUNCTION;
          g_ptr_array_add(all_tables, btnew);
          ++j;
        }
        while (1);

        mysql_free_result(resultset);
      };

      resultset= NULL;
      query= g_strconcat("select TRIGGER_NAME from information_schema.TRIGGERS where TRIGGER_SCHEMA = '",
        bt->schema, "'", NULL);
      if (mysql_query(mysql, query) == 0)
        resultset= mysql_store_result(mysql);
      g_free(query);

      if (resultset != NULL)
      {
        do
        {
          MYX_BACKUP_TABLE *btnew;

          row= mysql_fetch_row(resultset);
          if (row == NULL)
            break;

          btnew= (MYX_BACKUP_TABLE *)g_malloc(sizeof(MYX_BACKUP_TABLE));

          btnew->catalog= g_strdup(bt->catalog);
          btnew->schema= g_strdup(bt->schema);
          btnew->table= g_strdup(row[0]);
          btnew->flags= MYX_BTF_IS_TRIGGER;
          g_ptr_array_add(all_tables, btnew);
          ++j;
        }
        while (1);

        mysql_free_result(resultset);
      };

      if (j == 0)  // empty schema
      {
        MYX_BACKUP_TABLE *btnew= (MYX_BACKUP_TABLE *) g_malloc(sizeof(MYX_BACKUP_TABLE));
        btnew->catalog= g_strdup(bt->catalog);
        btnew->schema= g_strdup(bt->schema);
        btnew->table= NULL;
        btnew->flags= MYX_BTF_IS_TABLE;
        g_ptr_array_add(all_tables, btnew);
      };
    };
  };

  newbc->tables = g_malloc(sizeof(MYX_BACKUP_TABLE)*all_tables->len);
  newbc->tables_num = all_tables->len;

  for(i= 0; i < all_tables->len; i++)
  {
    MYX_BACKUP_TABLE *bt= (MYX_BACKUP_TABLE *) g_ptr_array_index(all_tables, i);

    // No need to duplicate strings here. This was done already when
    // we fill the btnew structures above. Strings are taken over and freed
    // when the backup content itself is deallocated.
    newbc->tables[i].catalog= bt->catalog;
    newbc->tables[i].schema= bt->schema;
    newbc->tables[i].table= bt->table;
    newbc->tables[i].flags= bt->flags;
    g_free(bt);
  };
  g_ptr_array_free(all_tables, 0);

  for(i= 0; i < all_schemas->len; i++)
  {
    g_free(g_ptr_array_index(all_schemas, i));
  };
  g_ptr_array_free(all_schemas, 0);

  return newbc;
}

//----------------------------------------------------------------------------------------------------------------------

MYX_BACKUP_CONTENT* copy_backup_content(MYX_BACKUP_CONTENT *content)
{
  MYX_BACKUP_TABLE * src_table= content->tables;
  MYX_BACKUP_TABLE * end_src_tables= src_table + content->tables_num;
  MYX_BACKUP_TABLE * table;
  MYX_BACKUP_CONTENT *copy= g_malloc0(sizeof(MYX_BACKUP_CONTENT));
  copy->tables_num= content->tables_num;
  copy->tables= g_malloc0(sizeof(MYX_BACKUP_TABLE) * copy->tables_num);

  for (table= copy->tables; src_table!=end_src_tables; table++, src_table++)
  {
    table->flags= src_table->flags;
    table->catalog= g_strdup(src_table->catalog);
    table->schema= g_strdup(src_table->schema);
    if(src_table->table && src_table->table[0])
    {
      table->table= g_strdup(src_table->table);
    }
    else
    {
      table->table= NULL;
    }
  }

  return copy;
}

//----------------------------------------------------------------------------------------------------------------------

int  compare_backup_tables(const void *a, const void *b)
{
  MYX_BACKUP_TABLE *t1= (MYX_BACKUP_TABLE *)a;
  MYX_BACKUP_TABLE *t2= (MYX_BACKUP_TABLE *)b;

  // Sort order is:
  // 1) catalog
  // 2) schema
  // 3) tables
  // 4) functions
  // 5) procedures
  // 6) views
  // 7) triggers

  int result= 0;
  if ((t1->catalog != NULL) && (t2->catalog != NULL))
    result= strcmp(t1->schema, t2->schema);

  if (result == 0)
  {
    result= strcmp(t1->schema, t2->schema);
    if (result == 0)
    {
      int type1= t1->flags & ~MYX_BTF_IGNORE_CONTENT;
      int type2= t2->flags & ~MYX_BTF_IGNORE_CONTENT;
      if (type1 != type2)
      {
        // Objects are not of the same type.
        result= type1 - type2;
      }
      else
        // Same object type, compare names.
        result= strcmp(t1->table, t2->table);
    }

  };
  
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

int write_schema_selection_to_file(MYX_BS_STATUS *status, const char *schema_quoted, MYX_BACKUP_ERROR *error)
{
  MYSQL_ROW row;
  MYSQL_RES *result;
  char *database_name;
  char *stmt= g_strconcat("SHOW CREATE DATABASE WITH IF NOT EXISTS ", schema_quoted,  NULL);

  if (!(status->options & MYX_B_NO_CREATES))
  {
    // If creation of objects is enable then write out the CREATE DATABASE call if necessary.
    result= NULL;
    if (myx_mysql_query(status->mysql, stmt) == 0)
      result= mysql_store_result(status->mysql);
    if (result == NULL)
    {
      SAFE_IO(fprintf(status->sql_file, "--\n-- Create schema %s\n--\n\n", schema_quoted));
      SAFE_IO(fprintf(status->sql_file, "CREATE DATABASE IF NOT EXISTS %s;\n", schema_quoted));
    }
    else
    {
      row= mysql_fetch_row(result);
      if (!row)
      {
        g_free(stmt);
        *error= MYX_BACKUP_SERVER_ERROR;
        return -1;
      }
      database_name= myx_convert_dbstr_utf8(status->mysql, row[1], -1);
      SAFE_IO(fprintf(status->sql_file, "%s;\n", database_name));
      g_free(database_name);
    }
    g_free(stmt);
  };
  
  SAFE_IO(fprintf(status->sql_file, "USE %s;\n", schema_quoted));

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
  @note covered by unit tests
 */
int write_schema_ddl(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  int result;
  MYX_BACKUP_TABLE *current, *last;
  current= status->backup_content->tables + status->current_index;
  last= status->backup_content->tables + status->previous_index;

  *error= MYX_BACKUP_NO_ERROR;

  if ((status->previous_index == -1) || strcmp(current->schema, last->schema))
  {
    if (myx_identifier_needs_quotes(current->schema))
    {
      // In certain situations we already have a quoted schema name already. Use this then.
      char* quoted_schema;
      if (status->current_schema_quoted != NULL)
        quoted_schema= status->current_schema_quoted;
      else
        quoted_schema= quote_identifier(current->schema, status->quote_char);
      result= write_schema_selection_to_file(status, quoted_schema, error);
      if (status->current_schema_quoted == NULL)
        g_free(quoted_schema);
    }
    else
      result= write_schema_selection_to_file(status, current->schema, error);

    if (result == 0)
    {
      if (!status->qualified)
      {
        // Activate this schema also in the database to avoid fully qualified identifiers when retrieving
        // SQL creation data (if qualifed IDs are switched off).
        char *sql= g_strconcat("use `", current->schema, "`", NULL);
        result= myx_mysql_query(status->mysql, sql);
        g_free(sql);
        if (result != 0)
          return -1;
      };
    }
    else
      return -1;
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
  @note covered by unit tests
 */
int write_sql_file_header(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  gchar Buffer[200] = "/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO";
  gchar *BufferEnd= Buffer + strlen(Buffer);

  *error= MYX_BACKUP_NO_ERROR;

  SAFE_IO(fprintf(status->sql_file,
                  "-- MySQL Administrator dump %s\n--\n", ADMIN_DUMP_VERSION));
  SAFE_IO(fputs("-- ------------------------------------------------------\n",
                status->sql_file));
  SAFE_IO(fprintf(status->sql_file,
                  "-- Server version\t%s\n",
                  mysql_get_server_info(status->mysql)));
  SAFE_IO(fprintf(status->sql_file, "\n\n"));


  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET NAMES %s */;\n\n", "utf8"));
  SAFE_IO(fprintf(status->sql_file, "/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\n"));

  if (status->options & MYX_B_ANSI_QUOTES)
    BufferEnd= strmov(BufferEnd, ",ANSI_QUOTES");
  if (status->options & MYX_B_COMPATIBILITY_MODE)
    BufferEnd= strmov(BufferEnd, ",MYSQL323");
  strmov(BufferEnd, "' */;\n\n\n");
  SAFE_IO(fprintf(status->sql_file, Buffer));

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
  @note covered by unit tests
 */
int write_sql_file_footer(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  *error= MYX_BACKUP_NO_ERROR;
  
  SAFE_IO(fprintf(status->sql_file,"\n\n\n/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;\n"));
  SAFE_IO(fprintf(status->sql_file,"/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;\n"));
  SAFE_IO(fprintf(status->sql_file,"/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;\n"));

  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;\n"));
  SAFE_IO(fprintf(status->sql_file, "/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;\n"));

  fprintf(status->sql_file,"/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;\n");

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

const char* object_name[][2] =
{
  {"SCHEMA IF EXISTS", "schema"},       // MYX_DBM_OT_SCHEMA
  {"TABLE IF EXISTS", "table"},         // MYX_DBM_OT_TABLE
  {"VIEW IF EXISTS", "view"},           // MYX_DBM_OT_VIEW
  {"PROCEDURE IF EXISTS", "procedure"}, // MYX_DBM_OT_PROCEDURE
  {"FUNCTION IF EXISTS", "function"},   // MYX_DBM_OT_FUNCTION
  {"TRIGGER /*!50030 IF EXISTS */", "trigger"}      // MYX_DBM_OT_TRIGGER
};

/*
  @note covered by unit tests.
 */
int write_create_statement_to_file(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error)
{
  MYX_DBM_OBJECT_TYPE type;
  char* create_statement;
  char* raw_sql;
  int flags= status->backup_content->tables[status->current_index].flags;
  char* drop_string;
  unsigned char sql_mode_needed= 0;

  if ((flags & MYX_BTF_IS_PROCEDURE) != 0)
  {
    type= MYX_DBM_OT_PROCEDURE;
    sql_mode_needed= 1;
  }
  else
    if ((flags & MYX_BTF_IS_FUNCTION) != 0)
    {
      type= MYX_DBM_OT_FUNCTION;
      sql_mode_needed= 1;
    }
    else
      if ((flags & MYX_BTF_IS_VIEW) != 0)
        type= MYX_DBM_OT_VIEW;
      else
        if ((flags & MYX_BTF_IS_TRIGGER) != 0)
          type= MYX_DBM_OT_TRIGGER;
        else
          type= MYX_DBM_OT_TABLE;

  raw_sql= myx_dbm_get_create_sql(status->mysql, "def", status->backup_content->tables[status->current_index].schema,
    status->backup_content->tables[status->current_index].table, type, status->qualified, status->quote_char, sql_mode_needed);

  if (raw_sql == NULL)
  {
    *error= MYX_BACKUP_CANT_GET_SQL;
    return -1;
  };

  if (status->options & MYX_B_COMMENT)
  {
    if (status->qualified)
      SAFE_IO(fprintf(status->sql_file, "\n--\n-- Definition of %s %s.%s\n--\n", object_name[type][1],
        status->current_schema_quoted, status->current_table_quoted))
    else
      SAFE_IO(fprintf(status->sql_file, "\n--\n-- Definition of %s %s\n--\n", object_name[type][1],
        status->current_table_quoted));
  };

  if (status->options & MYX_B_ADD_DROP_TABLE)
  {
    // For views we have created temporary dummy tables to solve reference issues. They have to be deleted.
    if (type == MYX_DBM_OT_VIEW)
    {
      if (status->qualified)
        drop_string= g_strdup_printf("\nDROP TABLE IF EXISTS %s.%s;", status->current_schema_quoted,
          status->current_table_quoted);
      else
        drop_string= g_strdup_printf("\nDROP TABLE IF EXISTS %s;", status->current_table_quoted);

      SAFE_IO(fprintf(status->sql_file, drop_string));
      g_free(drop_string);
    };

    if (sql_mode_needed)
    {
      // SQL mode is needed for SPs and SFs. Since we would have to parse the content of the used sql mode
      // when the SP/SF was created if we exactly want to know which quote char was used (which is a lot
      // of effort for such a tiny information) we just make it simple and use a back tick in any case.
      if (status->qualified)
        drop_string= g_strdup_printf("\nDROP %s `%s`.`%s`;\n", object_name[type][0],
          status->backup_content->tables[status->current_index].schema,
          status->backup_content->tables[status->current_index].table);
      else
        drop_string= g_strdup_printf("\nDROP %s `%s`;\n", object_name[type][0],
          status->backup_content->tables[status->current_index].table);
    }
    else
    {
      if (status->qualified)
        drop_string= g_strdup_printf("\nDROP %s %s.%s;\n", object_name[type][0], status-> current_schema_quoted,
          status->current_table_quoted);
      else
        drop_string= g_strdup_printf("\nDROP %s %s;\n", object_name[type][0], status->current_table_quoted);
    };

    SAFE_IO(fprintf(status->sql_file, drop_string));
    g_free(drop_string);
  };

  create_statement= myx_convert_dbstr_utf8(status->mysql, raw_sql, -1);
  if (type == MYX_DBM_OT_VIEW || type == MYX_DBM_OT_TABLE)
    SAFE_IO(fprintf(status->sql_file, "%s;\n", create_statement))
  else
    SAFE_IO(fprintf(status->sql_file,"\nDELIMITER $$\n\n%s $$\n\nDELIMITER ;\n", create_statement));

  g_free(raw_sql);
  g_free(create_statement);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Writes all trigger creation statements for a given table out.
 *
 * @param status The backup status containing all relevant info.
 * @param error A point to an error variable to contain a specific error code if something fails.
 * @param table The table for which triggers must be dumped.
 * @param schema The schema in wich "table" is.
 *
 * @return 0 if everything went fine, otherwise -1.
 */
int write_triggers_to_file(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error, const char* table, const char* schema)
{
  char* create_statement;
  char* raw_sql;
  char* drop_string;
  MYSQL_RES *resultset;
  MYSQL_ROW row;
  char* quoted_trigger;
  char* masked_schema= escape_string(schema);
  char* masked_table= escape_string(table);

  resultset= NULL;
  raw_sql= g_strconcat("select TRIGGER_NAME from information_schema.TRIGGERS where (TRIGGER_SCHEMA = '",
    masked_schema, "') and (EVENT_OBJECT_TABLE = '", masked_table, "')", NULL);
  if (mysql_query(status->mysql, raw_sql) == 0)
    resultset= mysql_store_result(status->mysql);
  g_free(raw_sql);
  g_free(masked_schema);
  g_free(masked_table);

  if (resultset != NULL)
  {
    while ((row= mysql_fetch_row(resultset)) != NULL)
    {
      if (do_progress_report(status) != 0)
      {
       *error= MYX_BACKUP_STOPPED;
       return -1;
      };
      
      raw_sql= myx_dbm_get_create_sql(status->mysql, "def", schema, row[0], MYX_DBM_OT_TRIGGER,
        status->qualified, status->quote_char, 0);

      quoted_trigger= quote_identifier(row[0], status->quote_char);
      if (status->options & MYX_B_COMMENT)
      {
        if (status->qualified)
          SAFE_IO(fprintf(status->sql_file, "\n--\n-- Definition of trigger %s.%s\n--\n", status->current_schema_quoted,
            quoted_trigger))
        else
          SAFE_IO(fprintf(status->sql_file, "\n--\n-- Definition of trigger %s\n--\n", quoted_trigger));
      };

      if (status->options & MYX_B_ADD_DROP_TABLE)
      {
        if (status->qualified)
          drop_string= g_strdup_printf("\nDROP TRIGGER /*!50030 IF EXISTS */ %s.%s;\n", status->current_schema_quoted,
            quoted_trigger);
        else
          drop_string= g_strdup_printf("\nDROP TRIGGER /*!50030 IF EXISTS */ %s;\n", quoted_trigger);

        SAFE_IO(fprintf(status->sql_file, drop_string));
        g_free(drop_string);
      };

      create_statement= myx_convert_dbstr_utf8(status->mysql, raw_sql, -1);
      SAFE_IO(fprintf(status->sql_file,"\nDELIMITER $$\n\n%s $$\n\nDELIMITER ;\n", create_statement));

      status->count++;

      g_free(quoted_trigger);
      g_free(raw_sql);
      g_free(create_statement);
    };

    mysql_free_result(resultset);
  };

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/*
  @note covered by unit tests
 */
int write_row_to_file(MYX_BS_STATUS *status, MYSQL_ROW row, MYX_BACKUP_ERROR *error)
{
  unsigned int i;
  char *insert_beg;
  char* object_name;
  char *value_stmt= g_strdup(" ");

  unsigned int num_fields= mysql_num_fields(status->mysql_result);
  unsigned long * lengths= mysql_fetch_lengths(status->mysql_result);
  MYSQL_FIELD * fields= mysql_fetch_fields(status->mysql_result);

  if ((status->options & MYX_B_DONT_WRITE_FULL_PATH) != 0)
    object_name= g_strdup(status->current_table_quoted);
  else
    object_name= g_strdup_printf("%s.%s", status->current_schema_quoted, status->current_table_quoted);

  if (!(status->options & MYX_B_COMPLETE_INSERTS))
  {
    insert_beg= g_strconcat("INSERT INTO ", object_name," VALUES ",NULL);
  }
  else
  {
    insert_beg= g_strconcat("INSERT INTO ", object_name," (", NULL);
    for(i= 0; i < num_fields; i++)
    {
      char *quoted= quote_identifier(fields[i].name, status->quote_char);
      insert_beg= str_g_append_and_free(insert_beg, g_strconcat(quoted,",",NULL));
      g_free(quoted);
    }
    insert_beg[strlen(insert_beg)-1]= ')';
    insert_beg= str_g_append(insert_beg, " VALUES \n");
  };
  g_free(object_name);

  if (! status->extended_insert)
    status->extended_insert= g_strdup(insert_beg);

  for(i= 0; i < num_fields; i++)
  {
    bigint length= lengths[i];
    MYSQL_FIELD *field= fields+i;
    value_stmt= str_g_append(value_stmt, i == 0 ? "(" : ",");

    if (!row[i]) /* if == NULL */
    {
      value_stmt= str_g_append(value_stmt, "NULL"); // write NULL
    }
    else
    {
      if (!length) /* if == empty string */
      {
        value_stmt= str_g_append(value_stmt, "''"); // write ''
      }
      else
      {
        // Binary blobs (BLOB but not TEXT), binary strings (BINARY, VARBINARY, but not CHAR, VARCHAR) as well
        // as bit fields are dumped as hex values.
        if ((field->type == MYSQL_TYPE_BIT) ||
          ((field->type == MYSQL_TYPE_STRING ||
            field->type == MYSQL_TYPE_VAR_STRING ||
            (field->flags & BLOB_FLAG) != 0) && (field->flags & BINARY_FLAG) != 0)
        )
        {
          // A binary or bit field.
          char* hex_string;

          value_stmt= str_g_append(value_stmt, "0x");
          hex_string= hex_encode(row[i], length);
          value_stmt= str_g_append_and_free(value_stmt, hex_string);
        }
        else
          if (IS_NUM_FIELD(field))
          {
            value_stmt=
              !strcmp(row[i],"nan") ||
              !strcmp(row[i],"inf") || !strcmp(row[i],"-inf")
                ? str_g_append(value_stmt, "NULL")
                : field->type != FIELD_TYPE_DECIMAL
                    ? str_g_append(value_stmt, row[i])
                    : str_g_append_and_free(value_stmt,
                                            g_strconcat("'",row[i],"'",NULL));
          }
          else
          {
            char *tmp_str= g_malloc((unsigned int)length*2+1+2);
            unsigned int len= mysql_real_escape_string(status->mysql,
                                                       tmp_str+1, row[i],
                                                       (unsigned int)length);
            tmp_str[0]= '\'';
            tmp_str[len+1]= '\'';
            tmp_str[len+2]= '\0';
            value_stmt= str_g_append_and_free(value_stmt, tmp_str);
          };
      };
    };
  };

  value_stmt= str_g_append(value_stmt, ")");

  if (status->options & MYX_B_NO_EXTENDED_INSERT)
  {
    char *tmp_str= myx_convert_dbstr_utf8(status->mysql, value_stmt, -1);
    SAFE_IO(fprintf(status->sql_file, "%s %s;\n", insert_beg, tmp_str));
    status->uncommitted_size += strlen(insert_beg) + strlen(tmp_str) + strlen("%s %s;\n");
    g_free(tmp_str);
  }
  else  /*extended inserts are not always written out*/
  {
    size_t len= strlen(status->extended_insert);
    status->extended_insert= str_g_append(status->extended_insert, value_stmt);
    if (len < 64000)
    {
      status->extended_insert= str_g_append(status->extended_insert, ",\n");
    }
    else /* we write it out */
    {
      char *tmp_str;
      status->extended_insert= str_g_append(status->extended_insert, ";");

      tmp_str= myx_convert_dbstr_utf8(status->mysql, status->extended_insert, -1);
      SAFE_IO(fprintf(status->sql_file, "%s\n", tmp_str));
      status->uncommitted_size += strlen(tmp_str) + strlen("%s\n");

      g_free(tmp_str);

      g_free(status->extended_insert);
      status->extended_insert= NULL;
    }
  }

  if (((status->options & MYX_B_OPTIMIZED_COMMIT) != 0) && (status->uncommitted_size >= 1000000))
  {
    SAFE_IO(fprintf(status->sql_file,"COMMIT;\n"));
    status->uncommitted_size= 0;
  };

  g_free(insert_beg);
  g_free(value_stmt);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/* A simple wrapper around mysql_query().
 *
 * Return value:
 *     -1 in case of a fatal error
 *      0 if successful
 */
int comfort_query(MYX_RBS_STATUS *rs, char *buffer, int buffer_len, MYX_BACKUP_ERROR *error)
{
  char *warning;

  if(!rs->we.alt_delimiter && (buffer[buffer_len-1] == ';'))
  {
    buffer_len -= 1;
  }
  else if(rs->we.alt_delimiter)
  {
    if(strncmp(buffer + buffer_len - strlen(rs->we.alt_delimiter), rs->we.alt_delimiter, strlen(rs->we.alt_delimiter)) == 0)
    {
      buffer_len -= (int)strlen(rs->we.alt_delimiter);
    }
  }

  assert(buffer);

  if (myx_mysql_real_query(rs->mysql, buffer, buffer_len) )
  {
    warning= g_strconcat("Error while executing this query:", buffer,
                         "\nThe server has returned this error message:",
                         mysql_error(rs->mysql), NULL);
    if (rs->report_warning)
    {
      (*rs->report_warning)(warning, rs->report_warning_data);
    }
    else
    {
      fprintf(stderr, "%s\n",warning);
    }
    g_free(warning);

    if (! (rs->options &  MYX_RBS_FORCE))
    {
      *error= MYX_BACKUP_SERVER_ERROR;
      return -1;
    }

  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/* sends the query to the database, issueing a use database if necessary before
*/
int interact_with_server(MYX_RBS_STATUS *rs, char *buffer, int buffer_len, MYX_BACKUP_ERROR *error)
{
  if (!rs->target_schema)
  {
    /* The user has not supplied an explicit target schema.
     * We will take the target-information from the sql-file ie.
     * USE and create db statements will not be filtered when they
     * are read from the sql-file but be executed. Thus we can assume
     * here that we are already in the correct schema.
     */
  }
  else if (strcmp(rs->current_schema_in_db, rs->target_schema))
  {
    /* Change the default database to target_schema.
     * If target_schema does not exist yet, we will create it.*/
    char *q_use= g_strconcat("USE `", rs->target_schema, "`", NULL);

    if (!myx_mysql_query(rs->mysql,q_use)) 
    {
      g_free(q_use);
    }
    else /* Error executing USE */
    {
      if ( (mysql_errno(rs->mysql) == 1049) &&           /* Unknown database */
           !(rs->options & MYX_RBS_DONT_CREATE_TARGETS))
      {
        char *q_cre= g_strconcat("CREATE DATABASE `", rs->target_schema, "`", NULL);

        if ( comfort_query(rs, q_cre, (int)strlen(q_cre), error ) ||
             comfort_query(rs, q_use, (int)strlen(q_use), error ))
        {
          /* error is already set by comfort_query */
          g_free(q_use);

          g_free(q_cre);
          return -1;
        }
        g_free(q_cre);
      }
      else
      {
        char *warning;
        g_free(q_use);
        if (! (rs->options & MYX_RBS_FORCE))
        {
          *error= MYX_BACKUP_SERVER_ERROR;
          return -1;
        }
        warning= g_strconcat("Warning: Could not change the db.\n The server "
                             "has returned this error message:",
                             mysql_error(rs->mysql), NULL);
        if (rs->report_warning)
        {
          (*rs->report_warning)(warning, rs->report_warning_data);
        }
        else
        {
          fprintf(stderr,"%s", warning);
        }
        g_free(warning);
      }
    }

    g_free(rs->current_schema_in_db);
    rs->current_schema_in_db= g_strdup(rs->target_schema);
  }

  // go ahead and send the query
  return comfort_query(rs, buffer,buffer_len, error);
}

//----------------------------------------------------------------------------------------------------------------------

void free_backup_table_content(MYX_BACKUP_TABLE *t)
{
  g_free(t->schema);
  g_free(t->catalog);
  g_free(t->table);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * returns > 0 if the given table is part of the backup-content
 *
 * @note covered by unit tests
 */
int find_table(MYX_BACKUP_CONTENT *b, const char *table_name, const char *schema_name, const char *catalog_name)
{
  MYX_BACKUP_TABLE * table= b->tables;
  MYX_BACKUP_TABLE * tables_end= table + b->tables_num;

  for (; table!=tables_end; table++)
  {
    if (!strcmp(table->table, table_name) &&
        !strcmp(table->schema, schema_name)) // Ignore catalogs for now
    {
      return 1;
    }
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Searches the given content for an object of a given name.
 *
 * @param b The content to search through.
 * @param name The object name to search.
 * @param schema The schema in which the object is to be searched.
 * @param catalog Similar for catalog (ignored for now).
 *
 * @return The object reference in the content if found otherwise NULL.
 */
MYX_BACKUP_TABLE* get_object_index(MYX_BACKUP_CONTENT *b, const char *name, const char *schema, const char *catalog)
{
  MYX_BACKUP_TABLE* table= b->tables;
  MYX_BACKUP_TABLE* tables_end= table + b->tables_num;

  for (; table!=tables_end; table++)
  {
    if (!strcmp(table->table, name) && !strcmp(table->schema, schema))
    {
      return table;
    }
  }
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Attempts to match the given line against a compiled PCRE expression.
 *
 * @param line The line to match.
 * @param line_len The length of this line.
 * @param re The compiled regular expression.
 * @param pe Extra data returned from pcre_study.
 * @param expectedSubs The number of subexpressions that are at most to be expected to appear.
 *                     This number is used to allocate the internal processing vector, so rather use a bit larger
 *                     value than a too small one.
 * @param neededSub Index of the subexpression that the caller is interested in.
 *                  Example: "create\s+(temporary\s+)?table\s+", the fully matched text is at index 0, "(temporary\s+)?"
 *                  is at index 1 and "table\s+" is at index 2.
 *
 * @result If a match was found then the sub string that matched (given by subIndex) is returned.
 *         The caller is responsible for freeing the returned string!
 *         If there was no match NULL is returned.
 * @note covered by unit tests
 */
char* check_statement(const char *line, int line_len, pcre *re, pcre_extra *pe, int expectedSubs, int neededSub)
{
  const char *ret_val;
  int rc;
  char *return_value;
  int* o_vector;
  int vectorSize;

  vectorSize = (expectedSubs + 1) * 3;
  o_vector = (int*) malloc(vectorSize * sizeof(int));
  return_value = NULL;
  if ((rc= pcre_exec(re,pe, line, line_len, 0, 0, o_vector, vectorSize)) > 0)
  {
    // We cannot return a higher subexpression than there were recognized.
    if (neededSub < rc)
    {
      pcre_get_substring(line, o_vector, rc, neededSub, &ret_val);
      return_value= g_strdup(ret_val);
      pcre_free_substring((char*)ret_val);
    };
  };
  free(o_vector);

  return return_value;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Attempts to match the given line against a compiled PCRE expression similar to check_statement, but also for a schema
 * prefix. The prefix is assumed to be directly preceeding the needed sub expression (which in turn is the object name
 * about which this statement is).
 *
 * @param line The line to match.
 * @param line_len The length of this line.
 * @param re The compiled regular expression.
 * @param pe Extra data returned from pcre_study.
 * @param neededSub Index of the subexpression that the caller is interested in.
 *                  Example: "create\s+(temporary\s+)?table\s+", the fully matched text is at index 0, "(temporary\s+)?"
 *                  is at index 1 and "table\s+" is at index 2.
 * @param has_prefix [out] Gets 0 or 1 depending on whether a prefix was found or not.
 * @param identifier [out] Gets a string with the found identifier or NULL if no prefix was found. The caller is
 *                         reponsible for freeing the string.
 *
 * return 1 if the RE matched, otherwise 0
 * @note covered by unit tests
 */
int check_statement_and_prefix(const char *line, int line_len, pcre *re, pcre_extra *pe, int neededSub,
  int* has_prefix, char** identifier)
{
  int result= 0;
  int rc;
  int o_vector[30];

  if ((rc= pcre_exec(re, pe, line, line_len, 0, 0, o_vector, 30)) > 0)
  {
    // We cannot return a higher subexpression than there were recognized.
    if (neededSub < rc)
    {
      const char* buffer;

      pcre_get_substring(line, o_vector, rc, neededSub, &buffer);
      *identifier= g_strdup(buffer);
      pcre_free_substring(buffer);

      if (neededSub > 0)
        // o_vector contains indices to all recognized substrings, two array values per substring.
        *has_prefix= o_vector[2 * (neededSub - 1)] > -1;

      result= 1;
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;

  *re = pcre_compile("create\\s+(?:temporary\\s+)?table\\s+" IF_NOT_EXISTS QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT,
    &error, &erroffset, NULL);
  if (*re == NULL)
  {
    fprintf(stderr, "%s\n", error);
    return 1;
  };

  if (pe)
    *pe = pcre_study(*re, 0, &error);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_index_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;
  const char *p= "create\\s+(?:unique|fulltext|spatial\\s+)?index\\s+"QUALIFIED_IDENTIFIER_PCRE;

  *re = pcre_compile(p, PCRE_DEFAULT, &error, &erroffset, NULL);
  if (*re == NULL)
  {
    fprintf(stderr, "%s\n",error);
    return 1;
  }

  if (pe)
    *pe = pcre_study(*re, 0, &error);
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_view_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;
  const char *p= "create\\s+"
    "(?:or\\s+replace\\s+)?"
    "(?:algorithm\\s*=\\s*(?:undefined|merge|temptable)\\s+)?"
    "(?:definer\\s*=\\s*(?:" USER_NAME_PCRE "|current_user(?:\\(\\))?)\\s+)?"
    "(?:sql\\s+security\\s+(?:definer|invoker)\\s+)?"
    "view\\s+" QUALIFIED_IDENTIFIER_PCRE;

  *re = pcre_compile(p, PCRE_DEFAULT, &error, &erroffset, NULL);
  if (*re == NULL) /* use default character tables */
  {
    fprintf(stderr, "%s\n",error);
    return 1;
  }

  if (pe)
  {
    *pe = pcre_study(*re, 0, &error);
  }
  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_proc_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;

  *re = pcre_compile("create\\s+(?:definer\\s*=\\s*(?:" USER_NAME_PCRE_IGNORE "|current_user(?:\\(\\))?)\\s+)?"
    "procedure\\s+" QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error, &erroffset, NULL);

  if (*re == NULL)
  {
    fprintf(stderr, "%s\n",error);
    return 1;
  }

  if (pe)
    *pe = pcre_study(*re, 0, &error);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_func_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;

  *re = pcre_compile("create\\s+(?:definer\\s*=\\s*(?:" USER_NAME_PCRE_IGNORE "|current_user(?:\\(\\))?)\\s+)?"
    "function\\s+" QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error, &erroffset, NULL);
  if (*re == NULL)
  {
    fprintf(stderr, "%s\n",error);
    return 1;
  }

  if (pe)
    *pe = pcre_study(*re, 0, &error);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_create_trigger_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;

  *re = pcre_compile("create\\s+(?:definer\\s*=\\s*(?:" USER_NAME_PCRE "|current_user(?:\\(\\))?)\\s+)?trigger\\s+"
    QUALIFIED_IDENTIFIER_PCRE, PCRE_DEFAULT, &error, &erroffset, NULL);
  if (*re == NULL)
  {
    fprintf(stderr, "%s\n", error);
    return 1;
  };

  if (pe)
    *pe = pcre_study(*re, 0, &error);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
int prepare_line_is_db_use_statement(pcre **re, pcre_extra **pe)
{
  const char *error;
  int erroffset;

  *re = pcre_compile("use\\s+(\\S+)\\s*;", PCRE_DEFAULT, &error, &erroffset, NULL);
  if (*re == NULL)
  {
    fprintf(stderr, "%s\n",error);
    return 1;
  }

  if (pe)
    *pe = pcre_study(*re, 0, &error);

  return 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
  * @note covered by unit tests.
 */
void free_pcre_data(pcre *re, pcre_extra *pe)
{
  if (re) pcre_free(re);
  if (pe) pcre_free(pe);
}

//----------------------------------------------------------------------------------------------------------------------

MYX_BACKUP_CONTENT * myx_get_restore_drop_list(MYSQL *mysql, MYX_BACKUP_CONTENT *content)
{
  MYX_BACKUP_CONTENT *drop_list= g_new0(MYX_BACKUP_CONTENT, 1);
  struct Schemas {
    MYX_SCHEMA_TABLES *tables;
    const char *catalog;
    const char *schema;
  } *schemas;
  unsigned int i, k;
  unsigned int schemas_num= 0;

  schemas= g_new0(struct Schemas, content->tables_num);
  for (i= 0; i < content->tables_num; i++)
  {
    int found= 0;
    for (k= 0; k < schemas_num; k++)
    {
      if (strcmp3(schemas[k].catalog, content->tables[i].catalog)==0
          && strcmp3(schemas[k].schema, content->tables[i].schema)==0)
      {
        found= 1;
        break;
      }
    }
    if (found) continue;

    schemas[schemas_num].catalog= content->tables[i].catalog;
    schemas[schemas_num].schema= content->tables[i].schema;
    schemas[schemas_num].tables= myx_get_schema_tables(mysql,
                                             content->tables[i].catalog,
                                             content->tables[i].schema);
    schemas_num++;
  }

  drop_list->tables= g_malloc0(sizeof(MYX_BACKUP_TABLE)*content->tables_num);
  drop_list->tables_num= 0;
  for (i= 0; i < content->tables_num; i++)
  {
    unsigned int s;
    int found= 0;
    for (s= 0; s < schemas_num; s++)
    {
      if (strcmp(schemas[s].catalog, content->tables[i].catalog)==0
          && strcmp(schemas[s].schema, content->tables[i].schema)==0
          && schemas[s].tables != NULL)
      {
        found= 1;
        break;
      }
    }
    if (found)
    {
      for (k= 0; k < schemas[s].tables->schema_tables_num; k++)
      {
        if (strcmp(schemas[s].tables->schema_tables[k].table_name, content->tables[i].table)==0)
        {
          drop_list->tables[drop_list->tables_num].catalog= g_strdup(content->tables[i].catalog);
          drop_list->tables[drop_list->tables_num].schema= g_strdup(content->tables[i].schema);
          drop_list->tables[drop_list->tables_num].table= g_strdup(content->tables[i].table);
          drop_list->tables_num++;
          break;
        }
      }
    }
  }


  for (k= 0; k < schemas_num; k++)
  {
    if (schemas[k].tables)
      myx_free_schema_tables(schemas[k].tables);
  }
  g_free(schemas);
  
  return drop_list;
}


char * myx_get_backup_error_string(MYX_BACKUP_ERROR error)
{
  switch (error)
  {
  case MYX_BACKUP_NO_ERROR:
    return g_strdup(_("Success."));
  case MYX_BACKUP_SERVER_ERROR:
    return g_strdup(_("MySQL Error."));
  case MYX_BACKUP_CANT_OPEN_FILE:
    return g_strdup(_("Cannot open file."));
  case MYX_BACKUP_ILLEGAL_OPTION:
    return g_strdup(_("Illegal option."));
  case MYX_BACKUP_PCRE_ERROR:
    return g_strdup(_("Internal parsing error (pcre)."));
  case MYX_BACKUP_MALLOC_FAILED:
    return g_strdup(_("Memory allocation failed."));
  case MYX_BACKUP_OUTPUTDEVICE_FULL:
    return g_strdup(_("Disk full."));
  case MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK:
    return g_strdup(_("Cannot flush tables with read lock."));
  case MYX_BACKUP_CANNOT_START_TRANSACTION:
    return g_strdup(_("Cannot start transaction."));
  case MYX_BACKUP_CANNOT_SET_ANSI_QUOTES:
    return g_strdup(_("Cannot set ANSI quotes."));
  case MYX_BACKUP_CANT_READ_FROM_FILE:
    return g_strdup(_("Error reading from backup file."));
  case MYX_BACKUP_XML_PARSE_ERROR:
    return g_strdup(_("Error parsing XML file."));
  case MYX_BACKUP_STOPPED:
    return g_strdup(_("User interruption."));
  case MYX_BACKUP_SQL_ERROR:
    return g_strdup(_("SQL error encountered."));
  case MYX_BACKUP_CHARSET_CONVERSION:
    return g_strdup(_("Error during character set conversion."));
  case MYX_BACKUP_WRONG_CHARSET:
    return g_strdup(_("Invalid character set selected for file."));
  case MYX_BACKUP_FILE_IS_NOT_MA_DUMP:
    return g_strdup(_("The selected file was generated by mysqldump and cannot be restored by this application."));
  case MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE:
    return g_strdup(_("Cannot restore to a different schema because the dump contains explicit schema qualifiers. "
      "This error cannot be ignored."));
  case MYX_BACKUP_CANT_GET_SQL:
    return g_strdup(_("Unable to get CREATE DDL for an object."));
  case MYX_BACKUP_UNKNOWN:
    return g_strdup(_("Unknown object in backup file"));
  case MYX_BACKUP_CANT_WRITE_TO_FILE:
    return g_strdup(_("Error writing to backup file."));
    
  }
  return NULL;
}

