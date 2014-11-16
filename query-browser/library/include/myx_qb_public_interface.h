/* Copyright (C) 2003,2004 MySQL AB

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

#ifndef myx_qb_public_interface_h
#define myx_qb_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif
#include <mysql.h>
#include <myx_util_functions.h>

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif


/*
 * PUBLIC INTERFACE definition for MYSQLLibInterfaceMapper
 */

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlqb"
#define libmysqlqb_PUBLIC_INTERFACE_VERSION 10001

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_util_public_interface, myx_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\..\common\library\base-library\include\myx_public_interface.h"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\..\common\library\utilities\include\myx_util_public_interface.h"

/*
 * Enums
 */



/*
 * Structs
 */


// --------------------------
// EXPLAIN

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_EXPLAIN_ROW
typedef struct {
  char *id;
  char *select_type;
  char *table;
  char *join_type;
  unsigned int possible_keys_num;
  char **possible_keys;
  char *key;
  char *key_len;
  char *ref;
  char *rows;
  char *extra;
} MYX_EXPLAIN_ROW;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_EXPLAIN_RESULT
typedef struct {
  bigint rows_num;
  MYX_EXPLAIN_ROW *rows;
} MYX_EXPLAIN_RESULT;

// --------------------------
// Stripped code

typedef enum {
  MYX_QSSCL_PHP, MYX_QSSCL_JAVA
} MYX_Q_SQL_STRIPPED_CODE_LANGUAGE;

typedef enum {
  MYX_QSSCM_QB_MENU, MYX_QSSCM_KEYSTROKES, MYX_QSSCM_MESSAGE
} MYX_Q_SQL_STRIPPED_COPY_MODE;

typedef struct
{
  char *query_stripped;

  MYX_Q_SQL_STRIPPED_CODE_LANGUAGE code_lang;
  MYX_Q_SQL_STRIPPED_COPY_MODE copy_mode;

} MYX_Q_SQL_STRIPPED;

// --------------------------
// Bookmarks / History

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BOOKMARK
typedef struct 
{
  char *caption;
  unsigned int pos;

  char *catalog;
  char *schema;

  char *sql;
  MYX_Q_TYPE query_type;

  unsigned int access_count;

  /* date is in ISO 8601: CCYY-MM-DD */
  char *date_created;
  char *date_modified;
  char *date_last_access;
} MYX_BOOKMARK;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BOOKMARK_GROUP
typedef struct myx_bookmark_group
{
  char *caption;
  unsigned int pos;

  unsigned int bookmarks_num;
  MYX_BOOKMARK *bookmarks;

  unsigned int bookmark_groups_num;
  struct myx_bookmark_group *bookmark_groups;
} MYX_BOOKMARK_GROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BOOKMARKS
typedef struct 
{
  unsigned int bookmark_groups_num;
  MYX_BOOKMARK_GROUP *bookmark_groups;
} MYX_BOOKMARKS;


typedef struct 
{
  char *catalog;
  char *schema;

  char *sql;
  MYX_Q_TYPE query_type;

  /* date is in YYYY-MM-DDThh:mm:ssTZD eg 1997-07-16T19:20:30+01:00. Time is in UTC. Thus TZD is always Z for us. */
  char *date_last_access;
  unsigned int marked_deleted;
} MYX_HISTORY_ENTRY;

typedef struct 
{
  unsigned int entries_num;
  MYX_HISTORY_ENTRY *entries;
} MYX_HISTORY;

typedef enum
{
  MYX_HIT_TODAY, MYX_HIT_MONDAY, MYX_HIT_TUESDAY, MYX_HIT_WEDNESDAY, MYX_HIT_THURSDAY, MYX_HIT_FRIDAY, MYX_HIT_SATURDAY,
  MYX_HIT_SUNDAY, MYX_HIT_YESTERDAY, MYX_HIT_LAST_WEEK, MYX_HIT_BEFORE_LAST_WEEK
} MYX_HISTORY_INTERVAL_TYPE;

typedef MYX_HISTORY_ENTRY * MYX_HISTORY_ENTRY_PTR;

typedef struct 
{
  char *caption;

  unsigned int entries_num;
  MYX_HISTORY_ENTRY_PTR *entries;
} MYX_HISTORY_SCHEMA;

typedef struct 
{
  char *caption;

  unsigned int schemata_num;
  MYX_HISTORY_SCHEMA *schemata;
} MYX_HISTORY_CATALOG;

typedef struct 
{
  MYX_HISTORY_INTERVAL_TYPE interval_type;

  unsigned int catalogs_num;
  MYX_HISTORY_CATALOG *catalogs;
} MYX_HISTORY_INTERVAL;

typedef struct 
{
  unsigned int history_intervals_num;
  MYX_HISTORY_INTERVAL *history_intervals;
} MYX_HISTORY_TREE;

/* sql-function list */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SQL_FUNCTION
typedef struct {
  char *caption;
  char *id;
} MYX_SQL_FUNCTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SQL_FUNCTIONGROUP
typedef struct MYX_SQL_FUNCTIONGROUP {
  char *caption;

  unsigned int functions_num;
  MYX_SQL_FUNCTION *functions;

  unsigned int subgroups_num;
  struct MYX_SQL_FUNCTIONGROUP *subgroups;
} MYX_SQL_FUNCTIONGROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SQL_FUNCTIONINDEX
typedef struct {
  unsigned int groups_num;
  MYX_SQL_FUNCTIONGROUP *groups;
} MYX_SQL_FUNCTIONINDEX;


/* script editor functions */
typedef struct {
  
  bigint stmt_begin_char;
  bigint stmt_end_char;
  int stmt_begin_line;
  int stmt_end_line;

} MYX_SQL_STATEMENT;

typedef struct {
  
  unsigned int stmts_size;
  unsigned int stmts_num;
  MYX_SQL_STATEMENT *stmts;

  char *private1;
  int private2;
} MYX_SQL_TEXT;

/* script viewer functions */
typedef struct
{
  bigint file_size;
  unsigned int block_num; // = file_size/block_size
  
  unsigned int lines_num;
  char **lines;
} MYX_SCRIPT_FILE_BLOCK;


/* PDF output of resultsets */
typedef struct {
  char *header;
  char *footer;
  double *column_widths;
  double header_font_size;
  double font_size;
  double paper_width, paper_height;
  double margin_top, margin_bottom;
  double margin_left, margin_right;
  double header_height, footer_height;
} MYX_RS_PRINT_PARAMS;


/*
 * Functions
 */

MYX_PUBLIC_FUNC int myx_get_qb_public_interface_version();


/* query EXPLAIN */
MYX_PUBLIC_FUNC MYX_EXPLAIN_RESULT * myx_query_explain(MYSQL *mysql, const char *query);
MYX_PUBLIC_FUNC int myx_free_explain_result(MYX_EXPLAIN_RESULT *res);

/* query history functions */
MYX_PUBLIC_FUNC MYX_HISTORY * myx_history_new();
MYX_PUBLIC_FUNC MYX_HISTORY * myx_history_load(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC MYX_LIB_ERROR myx_history_store(const char *filename, MYX_HISTORY *history);
MYX_PUBLIC_FUNC void myx_history_free(MYX_HISTORY *history);
MYX_PUBLIC_FUNC MYX_HISTORY_ENTRY * myx_history_add_entry(MYX_HISTORY *history, const char *catalog, const char *schema, 
                                                    const char *query, unsigned int max_entries);
MYX_PUBLIC_FUNC MYX_HISTORY_TREE * myx_history_get_tree(MYX_HISTORY *history);
MYX_PUBLIC_FUNC void myx_history_free_tree(MYX_HISTORY_TREE *history_tree);

/* query bookmarks functions */
MYX_PUBLIC_FUNC MYX_BOOKMARKS * myx_bookmarks_load(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC MYX_LIB_ERROR myx_bookmarks_store(const char *filename, MYX_BOOKMARKS *bookmarks);
MYX_PUBLIC_FUNC void myx_bookmarks_free(MYX_BOOKMARKS *bookmarks);

/* sql-function list */
MYX_PUBLIC_FUNC MYX_SQL_FUNCTIONINDEX * myx_load_sql_function_list(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_sql_function_list(MYX_SQL_FUNCTIONINDEX *f);

/* script editor functions */
MYX_PUBLIC_FUNC MYX_SQL_TEXT * myx_init_mysql_text();
MYX_PUBLIC_FUNC void myx_analyze_text(MYX_SQL_TEXT *s, const char *text);
MYX_PUBLIC_FUNC void myx_free_sql_text(MYX_SQL_TEXT *text);

/* script viewer functions */
MYX_PUBLIC_FUNC MYX_SCRIPT_FILE_BLOCK * myx_parse_script_file(const char *filename, int block_size, int block_num, int *last_block);
MYX_PUBLIC_FUNC void myx_free_script_file_block(MYX_SCRIPT_FILE_BLOCK *script);

MYX_PUBLIC_FUNC MYX_Q_SQL_STRIPPED * myx_strip_embedded_sql(const char *sql, MYX_Q_SQL_STRIPPED_CODE_LANGUAGE code_lang,
                                                            MYX_Q_SQL_STRIPPED_COPY_MODE copy_mode);
MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_get_params_from_stripped_query(MYX_Q_SQL_STRIPPED *stripped);
MYX_PUBLIC_FUNC char * myx_reconstruct_embedded_sql(MYX_Q_SQL_STRIPPED *stripped, const char *sql);
MYX_PUBLIC_FUNC void myx_free_stripped_sql(MYX_Q_SQL_STRIPPED *stripped_sql);

/* PDF output */
//MYX_PUBLIC_FUNC int myx_write_resultset_pdf(MYX_RESULTSET *rset, MYX_RS_PRINT_PARAMS *params);

//MYX_PUBLIC_FUNC int myx_generate_pdf(MYX_RESULTSET *resultset, char *pdf_filename);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
