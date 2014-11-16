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

#ifndef myx_admin_library_h
#define myx_admin_library_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include <mysql.h>
#include <mysql_version.h>

#include <myx_public_interface.h>
#include <myx_admin_public_interface.h>

#include <myx_library.h>

#include "myx_simple_sql_parsing.h"
#include "myx_xml_util_functions.h"

/* structure declarations */

/* for getting the content of a backup-file */
typedef struct
{
  MYX_INTL_FILE *intl_file;  
  MYX_BACKUP_CONTENT *backup_content;
  MYX_SQL_PARSE_ENVIRONMENT we;
  pcre *re_create_table;
  pcre *re_create_index;
  pcre *re_create_view;
  pcre *re_create_proc;
  pcre *re_create_func;
  pcre *re_create_trigger;
  pcre *re_db_use;
  pcre_extra *pe_create_table;
  pcre_extra *pe_create_index;
  pcre_extra *pe_create_view;
  pcre_extra *pe_create_proc;
  pcre_extra *pe_create_func;
  pcre_extra *pe_create_trigger;
  pcre_extra *pe_db_use;
  char *cur_db_name;
  const char *default_catalog_name;
  const char *default_schema_name;
} MYX_BCS_STATUS;


/* for restoring backup-files */
typedef struct
{
  MYX_INTL_FILE *intl_file;
  MYX_BACKUP_CONTENT *backup_content;
  MYX_SQL_PARSE_ENVIRONMENT we;
  pcre *re_create_database;
  pcre *re_create_table; 
  pcre *re_create_index;
  pcre *re_create_view;
  pcre *re_create_proc;
  pcre *re_create_func;
  pcre *re_create_trigger;
  pcre *re_db_use;
  pcre *re_drop_table;
  pcre *re_drop_view;
  pcre *re_drop_proc;
  pcre *re_drop_func;
  pcre *re_drop_database;
  pcre *re_drop_trigger;
  pcre *re_drop_index;
  pcre *re_insert;
  pcre *re_alter;
  pcre *re_lock;
  pcre *re_unlock;
  pcre *re_set;
  pcre *re_charset_set1;
  pcre *re_charset_set2;
  pcre *re_charset_set3;
  pcre *re_charset_set4;
  pcre *re_charset_set5;
  pcre *re_delimiter;
  pcre *re_commit;
  const char *target_catalog;
  const char *target_schema;
  char *current_schema_in_sqlfile; /* gets updated when we read 'USE db' 
                                      in the sql-file */
  char *current_catalog_in_sqlfile; /* In the future when we have catalog-
                                       support this is going to be updated
                                       by 'USE CATALOG catalogname' or so*/

  char *current_schema_in_db; /* keeps track of the currently chosen default
                                 database. the variable gets updated when we
                                 issue 'USE db' of course. */
  MYSQL *mysql;
  int options;
  char *buffer;
  int buffer_len;
  void (*report_warning) (const char *msg, void *user_data);
  void *report_warning_data;
} MYX_RBS_STATUS;

/* for backing up to sql-files */
typedef struct
{
  MYSQL_RES *mysql_result;
  FILE *sql_file;
  MYX_BACKUP_CONTENT *backup_content;
  MYSQL *mysql;
  int options;
  unsigned int current_index;          // The index of the element currently being dumped.
  unsigned int previous_index;         // The index of the element before the current one that was dumped
                                       // (not necessarily current_index - 1);
  char *current_table_quoted;
  char *current_schema_quoted;
  int current_table_rows_processed;
  int current_table_rows;
  char *extended_insert;
  char quote_char;
  int ignore;
  int uncommitted_size;                // Number of bytes not yet committed. Used only with optimized commit.
  int count;                           // Number of objects processed (processing might happen out of order so
                                       // current_index is not usable for this number).
  int total_count;                     // Total number of objects to dump, includes dummy tables for views.
  int qualified;                       // 1 if the full name of the element must be written out, otherwise 0.

  // Progress
  int (*progress_report) (const char* curr_tbl_name, int num_tables, int num_tables_processed, int num_rows,
    int num_rows_processed, void *user_data);
  void* user_data;
  int report_interval;                 // Number of records in a table to dump before calling the report callback.
                                       // Only used while dumping the content of a table.
  int remaining_ticks;                 // Counter to determine when to trigger the callback next time.

} MYX_BS_STATUS;

/*
 * Functions
 */

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

MYX_GUI_DESCRIPTION* myx_init_gui_description_with_default_values(MYX_GUI_DESCRIPTION *desc);
MYX_GUI_DESCRIPTION* myx_process_mysql_cnf_file(MYX_GUI_DESCRIPTION *desc, const char *ini_filepath, const char *group, MYX_ADMIN_LIB_ERROR *error_code);

int find_table(MYX_BACKUP_CONTENT *b, const char *table_name, const char *schema_name, const char *catalog_name);
MYX_BACKUP_TABLE* get_object_index(MYX_BACKUP_CONTENT *b, const char *name, const char *schema, const char *catalog);

void free_backup_table_content(MYX_BACKUP_TABLE *t);

int interact_with_server(MYX_RBS_STATUS *rs, char *buffer,
                                int buffer_len, MYX_BACKUP_ERROR *error);
int comfort_query(MYX_RBS_STATUS *rs, char *buffer,
                         int buffer_len, MYX_BACKUP_ERROR *error);
int write_create_statement_to_file(MYX_BS_STATUS *status,
                                          MYX_BACKUP_ERROR *error);
int finalize_extended_insert(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error);
int write_row_to_file(MYX_BS_STATUS *status, MYSQL_ROW row, MYX_BACKUP_ERROR *error);
int write_sql_file_header(MYX_BS_STATUS *status,
                                 MYX_BACKUP_ERROR *error);
int write_sql_file_footer(MYX_BS_STATUS *status,
                                 MYX_BACKUP_ERROR *error);
int  compare_backup_tables(const void *a, const void *b);
int write_schema_ddl(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error);
MYX_BACKUP_CONTENT* copy_backup_content(MYX_BACKUP_CONTENT *content);
MYX_BACKUP_CONTENT* select_all_tables_4(MYSQL* mysql, MYX_BACKUP_CONTENT *content);
MYX_BACKUP_CONTENT* select_all_tables_5(MYSQL* mysql, MYX_BACKUP_CONTENT *content);
int in_backup_content(const char *schema_name,
                             MYX_BACKUP_CONTENT *backup_content);

MYX_BACKUP_PROFILE *read_in_backup_profile_1_0(xmlNodePtr backup_node);
MYX_BACKUP_PROFILE *read_in_backup_profile_1_1(xmlNodePtr backup_node);
MYX_BACKUP_PROFILE *read_in_backup_profile_1_2(xmlNodePtr backup_node);
void read_in_table(xmlNodePtr table_node, MYX_BACKUP_TABLE *table);
void read_in_entity(xmlNodePtr entity_node, MYX_BACKUP_TABLE *table);
char* check_statement(const char *line, int line_len, pcre *re, pcre_extra *pe, int expectedSubs, int neededSub);
int check_statement_and_prefix(const char *line, int line_len, pcre *re, pcre_extra *pe, int neededSub,
  int* has_prefix, char** identifier);
int prepare_line_is_create_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_create_index_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_create_view_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_create_proc_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_create_func_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_create_trigger_statement(pcre **re, pcre_extra **pe);
int prepare_line_is_db_use_statement(pcre **re, pcre_extra **pe);
void free_pcre_data(pcre *re, pcre_extra *pe);

MYX_BCS_STATUS* myx_new_bcs_status(const char *filename, const char *filename_charset,
  void (*report_warning)(const char *msg, void *user_data),
  void *user_data, MYX_BACKUP_ERROR *error, int force);
int myx_free_bcs_status(MYX_BCS_STATUS *b, int free_backup_content);
int myx_get_backup_content_from_sql_file_incremental(MYX_BCS_STATUS *bc_status, int granularity, int forced_schema,
  int ignore_errors, MYX_BACKUP_ERROR *error);

MYX_RBS_STATUS * myx_new_rbs_status(MYSQL *mysql, const char *filename,
                                    const char *filename_charset,
                                    MYX_BACKUP_CONTENT *content,
                                    const char *target_catalog,
                                    const char *target_schema,
                                    int options,
                                    MYX_BACKUP_ERROR *error,
                                    void (*report_warning)
                                      ( const char *msg,void *user_data),
                                    void *user_data);
int myx_free_rbs_status(MYX_RBS_STATUS *rs);
int myx_restore_backup_from_sql_file_incremental(
                        MYX_RBS_STATUS *rs,
                        int (*progress_report)(bigint bytes_read,
                                               bigint bytes_total,
                                               void *user_data),
                        bigint file_size,
                        void *user_data,
                        MYX_BACKUP_ERROR *error);
MYX_BS_STATUS * myx_new_bs_status(MYSQL *mysql, const char *filename,
                                  MYX_BACKUP_CONTENT *content,
                                  MYX_BACKUP_OPTIONS options,
                                  MYX_BACKUP_ERROR *error);
int myx_free_bs_status(MYX_BS_STATUS *b);

int myx_set_bs_options(MYX_BS_STATUS *status, int options); //TODO: remove that function?

int check_if_ignore_table(MYSQL *mysql, const char *quoted_schema_name, const char *table_name);
int write_triggers_to_file(MYX_BS_STATUS *status, MYX_BACKUP_ERROR *error, const char* table, const char* schema);

int get_bs_status_size(void);
int get_lib_error_size(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif

