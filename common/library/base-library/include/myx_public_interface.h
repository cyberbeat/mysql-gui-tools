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

#ifndef myx_public_interface_h
#define myx_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include <mysql.h>
#include <myx_util_public_interface.h>
#include <stdio.h>


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

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlx"
#define libmysqlx_PUBLIC_INTERFACE_VERSION 10004

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_util_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\utilities\include\myx_util_public_interface.h"


#define MYX_DEFAULT_QUOTE_CHAR '`'

/*
 * Enums
 */

typedef enum myx_user_connection_type
{
    MYX_MYSQL_CONN = 0, MYX_ODBC_CONN, MYX_ORACLE_CONN, MYX_MSSQL_CONN, MYX_SQLITE_CONN
} MYX_USER_CONNECTION_TYPE;

typedef enum myx_user_connection_storage_type
{
    MYX_FAVORITE_USER_CONNECTION = 1, MYX_HISTORY_USER_CONNECTION
} MYX_USER_CONNECTION_STORAGE_TYPE;

typedef enum myx_password_storage_type
{
	MYX_PASSWORD_NOT_STORED = 1, MYX_PASSWORD_PLAINTEXT, MYX_PASSWORD_OBSCURED, MYX_PASSWORD_OS_SPECIFIC
} MYX_PASSWORD_STORAGE_TYPE;


typedef enum
{
  MYX_SYN_NORMAL,MYX_SYN_TABLE, MYX_SYN_COLUMN, MYX_SYN_COMMENT, MYX_SYN_STRING, MYX_SYN_SYMBOL, MYX_SYN_FUNCTION
} MYX_SYN_TYPE;


typedef enum
{
  MYX_IMG_UNKNOWN=0, MYX_IMG_JPEG, MYX_IMG_PNG, MYX_IMG_BMP, MYX_IMG_GIF
} MYX_IMAGE_FORMAT;

typedef enum
{
  MYX_DBM_OT_SCHEMA = 0,
  MYX_DBM_OT_TABLE,
  MYX_DBM_OT_VIEW,
  MYX_DBM_OT_PROCEDURE,
  MYX_DBM_OT_FUNCTION,
  MYX_DBM_OT_TRIGGER
} MYX_DBM_OBJECT_TYPE;


// Custom MySQL error codes
typedef enum MYX_MYSQL_ERROR_CODES
{
  MYX_UTF8_OUT_OF_RANGE=50000
} MYX_MYSQL_ERROR_CODES;


typedef enum
{
  MYX_QEL_NOTE= 1, MYX_QEL_WARNING, MYX_QEL_ERROR
} MYX_QUERY_ERROR_LEVEL;

/*
 * Structs
 */

typedef void (*MYX_QUERY_CALLBACK)(MYSQL *mysql, void *cdata, const char *query, unsigned int length);

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_MYSQL_ERROR_MSG
typedef struct {
  MYX_QUERY_ERROR_LEVEL level;
  int error;
  char *text;
} MYX_MYSQL_ERROR_MSG;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_MYSQL_ERROR_MSGS
typedef struct {
  unsigned int errors_num;
  MYX_MYSQL_ERROR_MSG *errors;
} MYX_MYSQL_ERROR_MSGS;

// Definition of user connections

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_CONNECTION
typedef struct {
  char *connection_name;
  char *username;
  char *password;
  char *hostname;
  unsigned int port;
  char *schema;
  unsigned int advanced_options_num;
  char **advanced_options;
  char *storage_path;
  char *notes;
  enum myx_user_connection_type connection_type;
  enum myx_user_connection_storage_type storage_type;
} MYX_USER_CONNECTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_CONNECTIONS
typedef struct {
  unsigned int user_connections_num;
  int last_connection;
  MYX_USER_CONNECTION *user_connections;
} MYX_USER_CONNECTIONS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_MYSQLD_CONNECTION
typedef struct {
  MYX_USER_CONNECTION *connection;
} MYX_MYSQLD_CONNECTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_MYSQLD_CONNECTIONS
typedef struct {
  unsigned int mysqld_connections_num;
  int last_connection;
  MYX_MYSQLD_CONNECTION *mysqld_connections;
} MYX_MYSQLD_CONNECTIONS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_IM_CONNECTION
typedef struct {
  MYX_USER_CONNECTION *connection;
  MYX_MYSQLD_CONNECTIONS *mysqlds;
} MYX_IM_CONNECTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_IM_CONNECTIONS
typedef struct {
  int last_im_connection;

  unsigned int im_connections_num;
  MYX_IM_CONNECTION *im_connections;
} MYX_IM_CONNECTIONS;

// Definition of program options

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_OPTION_GROUP
typedef struct {
  char *name;
  unsigned int name_value_pairs_num;
  MYX_NAME_VALUE_PAIR *name_value_pairs;
} MYX_OPTION_GROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_APPLICATION_OPTIONS
typedef struct {
  unsigned int option_groups_num;
  MYX_OPTION_GROUP *option_groups;
} MYX_APPLICATION_OPTIONS;

// -----------------------------------------------------------------------------------------------------------
// Machine Info and Ping

typedef struct {
  char *version;
  char *network_name;
  char *IP;
  char *OS;
  char *hardware;
} MYX_MACHINE_INFO;

typedef struct {
  int round_trip_time;
  int ttl;
} MYX_PING_RESULT;

// -----------------------------------------------------------------------------------------------------------
// Table Status

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_TABLE_COLUMN
typedef struct {
  char *column_name;
  char *column_type;
  char *default_value;
  char *extra;
  unsigned int primary_key;
  unsigned int not_null;
} MYX_SCHEMA_TABLE_COLUMN;

typedef MYX_SCHEMA_TABLE_COLUMN MYX_SCHEMA_VIEW_COLUMN;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_TABLE_INDEX_COLUMN
typedef struct {
  char *column_name;
  char *seq_in_index;
  char *collation;
} MYX_TABLE_INDEX_COLUMN;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_TABLE_INDEX
typedef struct {
  char *key_name;
  char *table_name;
  char *index_type;
  unsigned int not_null;
  unsigned int unique;

  unsigned int index_columns_num;
  MYX_TABLE_INDEX_COLUMN *index_columns;
} MYX_TABLE_INDEX;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_TABLE_STATUS
typedef struct {
  char *table_name;
  char *table_type;
  char *row_format;
  char *rows;
  char *avg_row_length;
  char *data_length;
  char *max_data_length;
  char *index_length;
  char *data_free;
  char *auto_increment;
  char *create_time;
  char *update_time;
  char *check_time;
  char *create_options;
  char *comment;

  unsigned int indexes_num;
  int invalid;               // Set when something is wrong with the table.
  MYX_TABLE_INDEX *indexes;

  unsigned int columns_num;
  MYX_SCHEMA_TABLE_COLUMN *columns;
} MYX_TABLE_STATUS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_TABLE_STATUS
typedef struct {
  unsigned int schema_tables_num;
  MYX_TABLE_STATUS *schema_tables;
} MYX_SCHEMA_TABLE_STATUS;

// -----------------------------------------------------------------------------------------------------------
// View Status

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VIEW_STATUS
typedef struct
{
  char *view_name;
  char *comment;
  unsigned int flags;
  int invalid;               // Set when something is wrong with the view.
} MYX_VIEW_STATUS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_VIEW_STATUS
typedef struct
{
  unsigned int schema_views_num;
  MYX_VIEW_STATUS *schema_views;
} MYX_SCHEMA_VIEW_STATUS;

// -----------------------------------------------------------------------------------------------------------
// Schema entiry Status

typedef enum
{
  MYX_ENTITY_TABLE = 1,
  MYX_ENTITY_VIEW,
  MYX_ENTITY_PROC,
  MYX_ENTITY_FUNC
} MYX_ENTITY_TYPE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_ENTITY_STATUS
typedef struct
{
  MYX_ENTITY_TYPE entity_type;
  void *entity;
} MYX_ENTITY_STATUS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_ENTITY_STATUS
typedef struct
{
  unsigned int schema_entities_num;
  MYX_ENTITY_STATUS *schema_entities;
} MYX_SCHEMA_ENTITY_STATUS;

// -----------------------------------------------------------------------------------------------------------
// Catalog / Schema Info

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_INDICES
typedef struct {
  unsigned int indices_num;
  MYX_TABLE_INDEX *indices;
} MYX_SCHEMA_INDICES;

typedef enum MYX_SCHEMA_TABLE_TYPE
{
  MSTT_BASE_TABLE, MSTT_VIEW
} MYX_SCHEMA_TABLE_TYPE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_TABLE
typedef struct {
  char *table_name;

  unsigned int columns_num;
  MYX_SCHEMA_TABLE_COLUMN *columns;

  MYX_SCHEMA_TABLE_TYPE table_type;
} MYX_SCHEMA_TABLE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_TABLES
typedef struct {
  unsigned int schema_tables_num;
  MYX_SCHEMA_TABLE *schema_tables;
} MYX_SCHEMA_TABLES;


typedef enum MYX_STORED_PROCEDURE_PARAM_TYPE
{
  MSPPT_IN, MSPPT_OUT, MSPPT_INOUT
} MYX_STORED_PROCEDURE_PARAM_TYPE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_STORED_PROCEDURE_PARAM
typedef struct {
  MYX_STORED_PROCEDURE_PARAM_TYPE param_type;
  char *name;
  char *datatype;
} MYX_SCHEMA_STORED_PROCEDURE_PARAM;


typedef enum MYX_STORED_PROCEDURE_TYPE
{
  MSPT_PROCEDURE, MSPT_FUNCTION
} MYX_SCHEMA_STORED_PROCEDURE_TYPE;

typedef enum MYX_STORED_PROCEDURE_SECURITY
{
  MSPS_DEFINER, MSPS_INVOKER
} MYX_SCHEMA_STORED_PROCEDURE_SECURITY;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_STORED_PROCEDURE
typedef struct {
  char *name;
  MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type;
  MYX_SCHEMA_STORED_PROCEDURE_SECURITY sp_security;
  char *definer;
  char *created;
  char *modified;
  char *comment;
  char *return_datatype;

  unsigned int params_num;
  MYX_SCHEMA_STORED_PROCEDURE_PARAM *params;
} MYX_SCHEMA_STORED_PROCEDURE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA_STORED_PROCEDURES
typedef struct {
  unsigned int schema_sps_num;
  MYX_SCHEMA_STORED_PROCEDURE *schema_sps;
} MYX_SCHEMA_STORED_PROCEDURES;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_SCHEMA
typedef struct {
  char *schema_name;
  char *escaped_schema_name;
  char *catalog_name;

  MYX_SCHEMA_TABLES *schema_tables;
  MYX_SCHEMA_INDICES *schema_indices;
  MYX_SCHEMA_STORED_PROCEDURES *schema_sps;
} MYX_SCHEMA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_CATALOG
typedef struct myx_catalog {
  char *catalog_name;
  unsigned int schemata_num;
  MYX_SCHEMA *schemata;
} MYX_CATALOG;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_CATALOGS
typedef struct {
  unsigned int catalogs_num;
  MYX_CATALOG *catalogs;
} MYX_CATALOGS;

typedef struct myx_syn MYX_SYN;

/* FOR SYNTAX-HIGHLIGHTING */
typedef struct
{
  int word_begin;
  int word_end;
  MYX_SYN_TYPE word_type;
} MYX_SYN_WORD;

typedef struct line_state MYX_LINE_STATE;

typedef struct
{
  unsigned int line_states_size;
  MYX_LINE_STATE *line_states;

  unsigned int words_size;
  unsigned int words_num;
  MYX_SYN_WORD *words;

  MYX_SYN* syn;
} MYX_SQL_HIGHLIGHTING;


/* FOR AUTO-COMPLETION */
typedef struct
{
  char *name;
  MYX_SYN_TYPE s_type;

} MYX_SYN_SUGGESTION;

typedef struct
{
  unsigned int suggestions_num;
  MYX_SYN_SUGGESTION *suggestions;

} MYX_SYN_SUGGESTIONS;


// -----------------------------------------------------------------------------------------------------------
// Query and Recordset

typedef enum
{
  MYX_QT_SELECT,
  MYX_QT_SELECT_INTO_OUTFILE,
  MYX_QT_UPDATE,
  MYX_QT_INSERT,
  MYX_QT_DELETE,
  MYX_QT_SCRIPT,
  MYX_QT_SHOW,
  MYX_QT_SET,
  MYX_QT_DESCRIBE,
  MYX_QT_EXPLAIN,
  MYX_QT_CALL,
  MYX_QT_UNION,
  MYX_QT_CHECK,
  MYX_QT_ANALYZE,
  MYX_QT_REPAIR,
  MYX_QT_OPTIMIZE,
  MYX_QT_EMPTY= 254,
  MYX_QT_UNKNOWN= 255
} MYX_Q_TYPE;

typedef enum
{
  MYX_QCT_NO_CLAUSE= 0,
  MYX_QCT_SELECT_CLAUSE,
  MYX_QCT_FROM_CLAUSE,
  MYX_QCT_WHERE_CLAUSE,
  MYX_QCT_GROUP_CLAUSE,
  MYX_QCT_HAVING_CLAUSE,
  MYX_QCT_ORDER_CLAUSE,
  MYX_QCT_LIMIT_CLAUSE,
  MYX_QCT_SET_CLAUSE,
  MYX_QCT_INTO_CLAUSE,
  MYX_QCT_UPDATE_CLAUSE,
  MYX_QCT_DELETE_CLAUSE,
  MYX_QCT_USING_CLAUSE
} MYX_Q_CLAUSE_TYPE;

typedef enum
{
  MYX_QTAT_SELECT=                  10,
  MYX_QTAT_SELECT_ADD,                  /* ctrl+shift */
  MYX_QTAT_SELECT_JOIN,                 /* ctrl       */
  MYX_QTAT_SELECT_LEFT_OUTER_JOIN,      /* shift      */

  MYX_QTAT_UPDATE=                  20,
  MYX_QTAT_INSERT=                  30,
  MYX_QTAT_DELETE=                  40,
  MYX_QTAT_UNKNOWN=                 255
} MYX_Q_TABLE_ADD_TYPE;

typedef enum
{
  MYX_QC_OK,
  MYX_QC_TABLES_WITHOUT_ALIAS,
  MYX_QC_TABLES_CAN_NOT_BE_JOINED
} MYX_Q_TABLE_ADD_ERROR;

typedef enum
{
  MYX_QTRT_NONE, MYX_QTRT_ONE_TO_ONE, MYX_QTRT_ONE_TO_MANY
} MYX_Q_TABLE_RELATIONSHIP_TYPE;

typedef enum
{
  MYX_RSCT_INTEGER,
  MYX_RSCT_FLOAT,
  MYX_RSCT_STRING,
  MYX_RSCT_DATE,
  MYX_RSCT_TIME,
  MYX_RSCT_DATETIME,
  MYX_RSCT_BLOB,
  MYX_RSCT_TEXT,
  MYX_RSCT_ENUM,
  MYX_RSCT_SET,
  MYX_RSCT_DECIMAL,
  MYX_RSCT_BIT,
  MYX_RSCT_TIMESTAMP,
  MYX_RSCT_YEAR,
  MYX_RSCT_NEWDATE,
  MYX_RSCT_NEWDECIMAL
} MYX_RS_COLUMN_TYPE;

#define MYX_RSCT_NEEDS_QUOTE(t) (t != MYX_RSCT_INTEGER && t != MYX_RSCT_FLOAT)

typedef enum
{
  MYX_RSA_UPDATE, MYX_RSA_ADD, MYX_RSA_DELETE
} MYX_RS_ACTION_TYPE;


typedef enum
{
  MYX_RSAS_NEW, MYX_RSAS_APPLIED, MYX_RSAS_FAILED, MYX_RSAS_DISCARDED
} MYX_RS_ACTION_STATUS;

// --------------------------
// Query

typedef struct
{
  char *column;
  int is_pk;
  int is_autoincrement;
  char *charset;
} MYX_Q_TABLE_COLUMN;

typedef struct MYX_Q_TABLE
{
  char *catalog;
  char *schema;
  char *name;
  char *alias;
  char *fullname;
  char *charset;

  unsigned int columns_num;
  MYX_Q_TABLE_COLUMN *columns;

  unsigned int pk_columns_num;
  struct MYX_Q_COLUMN *pk_columns;

  MYX_Q_TABLE_RELATIONSHIP_TYPE relationship_type;
  struct MYX_Q_TABLE *relationship;
} MYX_Q_TABLE;

typedef struct MYX_Q_COLUMN
{
  char *column_alias;
  char *column;
  int is_pk;
  MYX_Q_TABLE *table;
  MYX_Q_TABLE_COLUMN * table_column;
} MYX_Q_COLUMN;

typedef struct
{
  char *clause;
  MYX_Q_CLAUSE_TYPE clause_type;
  unsigned int start_index;
  unsigned int end_index;
  unsigned char end_with_linebreak;
} MYX_Q_CLAUSE;

typedef struct MYX_QUERY
{
  char *sql;
  char *original_sql;
  MYX_Q_TYPE query_type;

  unsigned int options_num;
  char **options;

  unsigned int tables_num;
  MYX_Q_TABLE *tables;

  unsigned int pk_columns_added_num;

  unsigned int columns_num;
  MYX_Q_COLUMN *columns;

  unsigned int clauses_num;
  MYX_Q_CLAUSE *clauses;

  unsigned int subquerys_num;
  struct MYX_QUERY *subquerys;

  unsigned int params_num;
} MYX_QUERY;

// --------------------------
// Columns

typedef struct
{
  MYX_Q_TABLE * table;
  MYX_Q_TABLE_COLUMN * table_column;

  MYX_RS_COLUMN_TYPE column_type;
  unsigned int type_size;

  char *name;
  unsigned int editable;
} MYX_RS_COLUMN;

// --------------------------
// Rows/Fields

typedef enum {
  MYX_RD_MATCHES=0,
  MYX_RD_THIS_ONLY,
  MYX_RD_OTHER_ONLY,
  MYX_RD_DIFFERS
} MYX_RS_ROW_DIFF;

#define MYX_RD_MASK 0x000000000000000fL

typedef struct
{
  unsigned int value_length;
  char *value;
} MYX_RS_FIELD;

typedef struct
{
  MYX_RS_FIELD *fields;
  bigint diff;
} MYX_RS_ROW;

// --------------------------
// Recordset

typedef struct
{
  MYSQL *mysql;
  MYX_QUERY *query;

  unsigned int editable;
  unsigned int has_more;

  unsigned int columns_num_to_display;

  unsigned int columns_num;
  MYX_RS_COLUMN *columns;

  unsigned int rows_num;
  MYX_RS_ROW *rows;

  struct MYX_RS_ACTIONS *actions;

  bigint memory_used;

  double query_time;
  double fetch_time;
} MYX_RESULTSET;

// --------------------------
// Recordset actions

typedef struct
{
  MYX_RS_ACTION_TYPE action;
  MYX_RS_ACTION_STATUS status;

  unsigned int row;
  MYX_RS_COLUMN *column;

  unsigned int new_value_length;
  char *new_value;
} MYX_RS_ACTION;

typedef struct MYX_RS_ACTIONS
{
  unsigned int actions_num;
  MYX_RS_ACTION *actions;
} MYX_RS_ACTIONS;


typedef struct {
  MYX_QUERY_ERROR_LEVEL level;
  int error;
  char *error_text;
  MYX_RS_ACTION *action;
} MYX_RS_ACTION_ERROR;


typedef struct {
  unsigned int errors_num;
  MYX_RS_ACTION_ERROR *errors;
} MYX_RS_ACTION_ERRORS;

// --------------------------
// Server feature structs

typedef struct {
  unsigned int version;
} MYX_AVAILABLE_FEATURES;


// --------------------------
// Database Model structs

typedef enum {
  MYX_DBM_DTG_NUMERIC = 0, MYX_DBM_DTG_DATETIME, MYX_DBM_DTG_STRING, MYX_DBM_DTG_BLOB, MYX_DBM_DTG_SPATIAL,
  MYX_DBM_DTG_USERDEFINED
} MYX_DBM_DATATYPE_GROUP;

typedef enum {
  MYX_DBM_TMI_NO = 0, MYX_DBM_TMI_FIRST, MYX_DBM_TMI_LAST
} MYX_DBM_TABLE_MERGE_INSERT;

typedef enum {
  MYX_DBM_TPK_DEFAULT = 0, MYX_DBM_TPK_NONE, MYX_DBM_TPK_ALL
} MYX_DBM_TABLE_PACK_KEYS;

typedef enum {
  MYX_DBM_TRT_NONE = 0, MYX_DBM_TRT_STRIPED
} MYX_DBM_TABLE_RAID_TYPE;

typedef enum {
  MYX_DBM_TRF_DEFAULT = 0, MYX_DBM_TRF_DYNAMIC, MYX_DBM_TRF_FIXED, MYX_DBM_TRF_COMPRESSED,
  MYX_DBM_TRF_REDUNDANT, MYX_DBM_TRF_COMPACT
} MYX_DBM_TABLE_ROW_FORMAT;

typedef enum {
  MYX_DBM_IT_DEFAULT = 0, MYX_DBM_IT_BTREE, MYX_DBM_IT_HASH, MYX_DBM_IT_RTREE
} MYX_DBM_INDEX_TYPE;

typedef enum {
  MYX_DBM_IK_INDEX = 0, MYX_DBM_IK_PRIMARY, MYX_DBM_IK_UNIQUE, MYX_DBM_IK_FULLTEXT,
  MYX_DBM_IK_SPATIAL, MYX_DBM_IK_FK
} MYX_DBM_INDEX_KIND;

typedef enum {
  MYX_DBM_FA_NO_ACTION = 0, MYX_DBM_FA_CASCADE, MYX_DBM_FA_SET_NULL, MYX_DBM_FA_RESTRICT
} MYX_DBM_FK_ACTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_SERVER_VERSION
typedef struct MYX_DBM_SERVER_VERSION {
  unsigned int major_version;
  unsigned int minor_version;
} MYX_DBM_SERVER_VERSION;

/// begin group charset

/// generate collation
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_COLLATION
typedef struct {
  char *name;

  struct MYX_DBM_CHARSET *charset; // [ref:parent]

  unsigned int id;
  unsigned char is_default;
  unsigned char is_compiled;
  unsigned int sort_len;
} MYX_DBM_COLLATION;

/// generate charset
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_CHARSET
typedef struct MYX_DBM_CHARSET {
  char *name;
  char *desc;
  char *default_collation;
  unsigned int max_len;

  unsigned int collations_num;
  MYX_DBM_COLLATION *collations;
} MYX_DBM_CHARSET;

/// generate charsets [root]
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_CHARSETS
typedef struct {
  unsigned int charsets_num;
  MYX_DBM_CHARSET *charsets;
} MYX_DBM_CHARSETS;

/// end group charset

/// begin group datatype

/// generate datatype
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_DATATYPE
typedef struct {
  char *name;
  MYX_DBM_DATATYPE_GROUP group;

  unsigned int synonym_group;

  unsigned int params_optional;
  unsigned int has_length_param;
  unsigned int has_decimal_param;
  unsigned int has_value_params;

  unsigned int flags_num;
  char **flags;

} MYX_DBM_DATATYPE;

/// generate datatypes [root]
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_DATATYPES
typedef struct {
  unsigned int datatypes_num;
  MYX_DBM_DATATYPE *datatypes;

  unsigned int substitutes_num;
  MYX_NAME_VALUE_PAIR *substitutes;
} MYX_DBM_DATATYPES;

/// end group datatype

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_COLUMN_DATA
typedef struct {
  char *name;
  char *original_name;

  MYX_DBM_DATATYPE *datatype_pointer;
  char *datatype_name;
  char *datatype_params;
  unsigned int datatype_flags_num;
  char **datatype_flags;

  char *charset;
  char *collation;

  unsigned char primary_key;
  unsigned char not_null;
  unsigned char auto_inc;

  char *default_value;
  int default_value_is_null;

  char *comment;

} MYX_DBM_COLUMN_DATA;


/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_INDEX_COLUMN_DATA
typedef struct {
  char *name;
  char *len;
  char *value_order;
} MYX_DBM_INDEX_COLUMN_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_INDEX_DATA
typedef struct {
  char *name;
  char *original_name;

  MYX_DBM_INDEX_KIND index_kind;
  MYX_DBM_INDEX_TYPE index_type;

  unsigned int columns_num;
  MYX_DBM_INDEX_COLUMN_DATA *columns;
} MYX_DBM_INDEX_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_FK_DATA
typedef struct {
  char *name;
  char *original_name;

  char *reference_schema_name;
  char *reference_table_name;

  unsigned int column_mapping_num;
  MYX_NAME_VALUE_PAIR *column_mapping;

  MYX_DBM_FK_ACTION on_delete;
  MYX_DBM_FK_ACTION on_update;

} MYX_DBM_FK_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_ENGINE
typedef struct {
  char *name;
  char *description;
  unsigned char isdefault;
  unsigned char enabled;
} MYX_ENGINE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_ENGINES
typedef struct {
  unsigned int engines_num;
  MYX_ENGINE *engines;
} MYX_ENGINES;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_TABLE_DATA
typedef struct {
  char *name;
  char *original_name;
  char *schema;
  char *catalog;

  char *create_table_stmt;

  //Table Options
  MYX_ENGINE *storage_engine;     // Pointer into the MYX_ENGINES structure (NULL for invalid engine).
  
  char *next_auto_inc;
  char *password;
  unsigned int delay_key_write;

  char *charset;
  char *collation;

  char *comment;

  char *merge_union;
  MYX_DBM_TABLE_MERGE_INSERT merge_insert;

  //Storage Options
  char *table_data_dir;
  char *table_index_dir;

  MYX_DBM_TABLE_PACK_KEYS pack_keys;

  MYX_DBM_TABLE_RAID_TYPE raid_type;
  char *raid_chunks;
  char *raid_chunk_size;


  unsigned int checksum;
  MYX_DBM_TABLE_ROW_FORMAT row_format;
  char *avg_row_length;
  char *min_rows;
  char *max_rows;

  unsigned int fks_num;
  MYX_DBM_FK_DATA *fks;

  unsigned int columns_num;
  MYX_DBM_COLUMN_DATA *columns;

  unsigned int indices_num;
  MYX_DBM_INDEX_DATA *indices;

  char *federated_connection;
} MYX_DBM_TABLE_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_STORED_PROCEDURE_DATA
typedef struct {
  MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type;
  char *catalog;
  char *schema;
  char *name;
  char *definition;
} MYX_DBM_STORED_PROCEDURE_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_VIEW_DATA
typedef struct {
  char *catalog;
  char *schema;
  char *name;
  char *definition;
} MYX_DBM_VIEW_DATA;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DBM_TRIGGER_DATA
typedef struct {
  char *catalog;
  char *schema;
  char *name;
  char *definition;
} MYX_DBM_TRIGGER_DATA;

// ------------------------------
// Table Data Exporting

typedef enum {
  MYX_TEOP_STRING,
  MYX_TEOP_INTEGER,
  MYX_TEOP_FLOAT,
  MYX_TEOP_COLOR,
  MYX_TEOP_BOOL,
  MYX_TEOP_STRINGLIST
} MYX_TE_OPTION_TYPE;


typedef enum {
  MYX_TE_COL_STRING,
  MYX_TE_COL_INTEGER,
  MYX_TE_COL_FLOAT,
  MYX_TE_COL_BOOL,
  MYX_TE_COL_DATE
} MYX_TE_COLUMN_TYPE;


typedef struct
{
  char *name;
  char *value;
  char *description;
  MYX_TE_OPTION_TYPE optype;
} MYX_TE_OPTION;


typedef struct
{
  char *name;
  MYX_TE_COLUMN_TYPE ctype;
} MYX_TE_COLUMN;

typedef struct
{
  struct MYX_TABLE_EXPORTER *te;
  FILE *file_stream;
  unsigned int current_level;

  unsigned int table_levels;
  unsigned int *columns_num;
  MYX_TE_COLUMN **columns;

  unsigned int option_values_num;
  char **option_values;

  void *priv;
} MYX_TABLE_EXPORTER_INFO;

typedef struct MYX_TABLE_EXPORTER
{
  const char *name;
  const char *description;
  const char *file_description;
  const char *file_extension;
  MYX_TABLE_EXPORTER_INFO *(*init)(void);
  void (*free)(MYX_TABLE_EXPORTER_INFO *info);
  int (*setup)(MYX_TABLE_EXPORTER_INFO *self, const char *filename);
  void (*begin)(MYX_TABLE_EXPORTER_INFO *self, const char *text);

  void (*table_setup)(MYX_TABLE_EXPORTER_INFO *self, unsigned int num_columns, MYX_TE_COLUMN *columns);

  void (*table_header)(MYX_TABLE_EXPORTER_INFO *self);
  void (*columns)(MYX_TABLE_EXPORTER_INFO *self, const char *const*text);
  void (*table_footer)(MYX_TABLE_EXPORTER_INFO *self);

  void (*end)(MYX_TABLE_EXPORTER_INFO *self);

  MYX_TE_OPTION *options;
} MYX_TABLE_EXPORTER;

// ------------------------------------------------------------
// Text Shell

typedef struct {
  MYSQL *mysql;
  int (*output_callback)(const char *text, void *user_data);
  void *output_user_data;
} MYX_TEXT_SHELL;

/*
 * Functions
 */

MYX_PUBLIC_FUNC int myx_get_public_interface_version(void);

MYX_PUBLIC_FUNC void myx_free_lib_str(char *str);
MYX_PUBLIC_FUNC int myx_str_in_stringlist(MYX_STRINGLIST *stringlist, const char *src);
MYX_PUBLIC_FUNC int myx_free_lib_stringlist(MYX_STRINGLIST *stringlist);

//Helper functions
MYX_PUBLIC_FUNC char * get_value_from_text_ex(const char *txt, int txt_length, const char *regexpr, unsigned int substring_nr);

//MySQL connect functions
MYX_PUBLIC_FUNC int myx_mysql_embedded_start(void);
MYX_PUBLIC_FUNC void myx_mysql_embedded_prevent_start(void);
MYX_PUBLIC_FUNC void myx_mysql_embedded_shutdown(void);
MYX_PUBLIC_FUNC MYSQL * myx_mysql_embedded_init(void);

MYX_PUBLIC_FUNC MYSQL * myx_mysql_init(void);

MYX_PUBLIC_FUNC int myx_init_library(const char *datadir);

MYX_PUBLIC_FUNC void myx_mysql_set_query_hooks(MYSQL *mysql,
                                               void (*pre_hook)(MYSQL *mysql, void *cdata, const char *query, unsigned int length),
                                               void (*post_hook)(MYSQL *mysql, void *cdata, const char *query, unsigned int length),
                                               void *client_data);
MYX_PUBLIC_FUNC int myx_set_my_cnf_path(const char *path);
MYX_PUBLIC_FUNC const char *myx_get_my_cnf_path(void);
MYX_PUBLIC_FUNC int myx_connect_to_instance(MYX_USER_CONNECTION *user_connection, MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_mysql_close(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_use_schema(MYSQL *mysql, const char *schema);
MYX_PUBLIC_FUNC int myx_push_schema(MYSQL *mysql, const char *schema);
MYX_PUBLIC_FUNC int myx_pop_schema(MYSQL *mysql);
MYX_PUBLIC_FUNC char * myx_get_default_schema(MYSQL *mysql);
MYX_PUBLIC_FUNC unsigned long myx_get_thread_id(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_ping_server(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_get_mysql_quote_char(MYSQL *mysql, int detect);

MYX_PUBLIC_FUNC char * myx_mysql_esc(MYSQL *mysql, const char *cmd, ...);
MYX_PUBLIC_FUNC int myx_mysql_query(MYSQL *mysql, const char *cmd);
MYX_PUBLIC_FUNC int myx_mysql_query_esc(MYSQL *mysql, const char *cmd, ...);
MYX_PUBLIC_FUNC int myx_mysql_real_query(MYSQL *mysql, const char *cmd, unsigned int len);
MYX_PUBLIC_FUNC bigint myx_mysql_affected_rows(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_kill_query(MYSQL* mysql, MYSQL* target);

MYX_PUBLIC_FUNC const char * myx_get_mysql_charset(MYSQL *mysql);
MYX_PUBLIC_FUNC char * myx_get_mysql_full_version(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_get_mysql_major_version(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_get_mysql_minor_version(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_get_mysql_release(MYSQL *mysql);
MYX_PUBLIC_FUNC int mysql_version_is_later_or_equal_than(MYSQL *mysql,
                                                         int major_version, int minor_version);
MYX_PUBLIC_FUNC int mysql_full_version_is_later_or_equal_than(MYSQL *mysql,
                                                              int major_version, int minor_version,
                                                              int patchlevel);
MYX_PUBLIC_FUNC int mysql_is_case_sensitive(MYSQL *mysql);

MYX_PUBLIC_FUNC char * myx_get_server_variable(MYSQL *mysql, const char *name);
/*MYX_PUBLIC_FUNC const char * myx_get_server_charset_name(MYSQL *mysql);*/

MYX_PUBLIC_FUNC char * myx_convert_dbstr_utf8(MYSQL *mysql, const char* str, int length);
MYX_PUBLIC_FUNC char * myx_convert_utf8_dbstr(MYSQL *mysql, const char* str);

MYX_PUBLIC_FUNC int myx_mysql_errno(MYSQL *mysql);
MYX_PUBLIC_FUNC char * myx_mysql_error(MYSQL *mysql);
MYX_PUBLIC_FUNC void myx_set_mysql_error(char *error, int errn);

MYX_PUBLIC_FUNC MYX_MYSQL_ERROR_MSGS * myx_mysql_error_msgs_fetch(MYSQL *mysql);
MYX_PUBLIC_FUNC void myx_mysql_error_msgs_free(MYX_MYSQL_ERROR_MSGS *errors);

MYX_PUBLIC_FUNC void build_field_subst(const char ** column_name,
                                       const char ** column_name_end,
                                       MYSQL_FIELD * res_fields,
                                       MYSQL_FIELD * res_fields_end,
                                       int * i_field);

MYX_PUBLIC_FUNC int use_schema_store_old_one(MYSQL * mysql,
                                             const char * schema_name,
                                             char ** old_schema_name);
MYX_PUBLIC_FUNC void restore_old_schema(MYSQL * mysql, char * old_schema_name);


//Network functions
MYX_PUBLIC_FUNC unsigned long myx_resolve_network_name(const char *hostname);
MYX_PUBLIC_FUNC int myx_ping_host(unsigned long ip, int ping_timeout, MYX_PING_RESULT *ping_result);
MYX_PUBLIC_FUNC int myx_is_localhost(const char *hostname);
MYX_PUBLIC_FUNC int myx_get_ip_as_string(const char *hostname, char *ip);

//User connection functions
MYX_PUBLIC_FUNC MYX_USER_CONNECTIONS * myx_load_user_connections(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_store_user_connections(MYX_USER_CONNECTIONS *user_connections, MYX_PASSWORD_STORAGE_TYPE pwd_storage_type, const char *filename);
MYX_PUBLIC_FUNC int myx_free_user_connections(MYX_USER_CONNECTIONS *user_connections);
MYX_PUBLIC_FUNC int myx_free_user_connection_content(MYX_USER_CONNECTION *user_connection);
MYX_PUBLIC_FUNC int myx_add_user_connection(MYX_USER_CONNECTIONS **user_connections, MYX_USER_CONNECTION *new_connection);

//IM/MYSQLD connection functions
MYX_PUBLIC_FUNC MYX_IM_CONNECTIONS * myx_load_im_connections(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_store_im_connections(MYX_IM_CONNECTIONS *im_connections, MYX_PASSWORD_STORAGE_TYPE pwd_storage_type, const char *filename);
MYX_PUBLIC_FUNC int myx_free_im_connections(MYX_IM_CONNECTIONS *im_connections);
MYX_PUBLIC_FUNC int myx_add_im_connection(MYX_IM_CONNECTIONS **im_connections, MYX_IM_CONNECTION *new_connection);
MYX_PUBLIC_FUNC int myx_add_mysqld_connection(MYX_IM_CONNECTION **im_connection, MYX_MYSQLD_CONNECTION *new_connection);
MYX_PUBLIC_FUNC MYX_MYSQLD_CONNECTIONS *myx_new_mysqld_connections(int nconnections);
MYX_PUBLIC_FUNC void myx_free_mysqld_connections(MYX_MYSQLD_CONNECTIONS *connections);

//client/server info functions
MYX_PUBLIC_FUNC MYX_MACHINE_INFO * myx_get_server_info(MYX_USER_CONNECTION *user_conn, MYSQL *mysql);
MYX_PUBLIC_FUNC MYX_MACHINE_INFO * myx_get_client_info(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_free_pc_info(MYX_MACHINE_INFO *machine_info);

// server features info
MYX_PUBLIC_FUNC int myx_get_available_server_features(MYSQL *mysql, MYX_AVAILABLE_FEATURES *features);

//Catalog functions
MYX_PUBLIC_FUNC MYX_CATALOGS * myx_get_catalogs(MYSQL *mysql);
MYX_PUBLIC_FUNC MYX_SCHEMA_TABLES * myx_get_schema_tables(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC MYX_SCHEMA_INDICES * myx_get_schema_indices(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC MYX_SCHEMA_TABLE_STATUS * myx_get_schema_table_status(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC MYX_SCHEMA_VIEW_STATUS * myx_get_schema_view_status(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC MYX_SCHEMA_STORED_PROCEDURES * myx_get_schema_sps(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC MYX_SCHEMA_ENTITY_STATUS * myx_get_schema_entity_status(MYSQL *mysql, const char *catalog_name, const char *schema_name);
MYX_PUBLIC_FUNC int myx_free_catalogs(MYX_CATALOGS *catalogs);
MYX_PUBLIC_FUNC int myx_free_schema_tables(MYX_SCHEMA_TABLES *schema_tables);
MYX_PUBLIC_FUNC int myx_free_schema_indices(MYX_SCHEMA_INDICES *schema_indices);
MYX_PUBLIC_FUNC int myx_free_schema_entity_status(MYX_SCHEMA_ENTITY_STATUS *schema_tables);
MYX_PUBLIC_FUNC int myx_free_schema_table_status(MYX_SCHEMA_TABLE_STATUS *schema_tables);
MYX_PUBLIC_FUNC int myx_free_schema_view_status(MYX_SCHEMA_VIEW_STATUS *schema_tables);
MYX_PUBLIC_FUNC int myx_free_schema_sps(MYX_SCHEMA_STORED_PROCEDURES *schema_sps);

//Application options functions
MYX_PUBLIC_FUNC int myx_store_application_options(MYX_APPLICATION_OPTIONS *options, char *filename);
MYX_PUBLIC_FUNC MYX_APPLICATION_OPTIONS* myx_get_application_options(char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_application_options(MYX_APPLICATION_OPTIONS *options);


//Various
//Check if a string matches a given search string
MYX_PUBLIC_FUNC int myx_match_pattern(const char *text, const char *search, int case_insensitive, int strict);

//Check whether a given string can be properly converted to an encoding
MYX_PUBLIC_FUNC int myx_check_utf8_convertible(const char *str, const char *encoding);

/* auto-completion & syntax-highlighting */
MYX_PUBLIC_FUNC MYX_SYN* myx_refresh_dbinfo(MYSQL *mysql, MYX_SYN *_syn);
MYX_PUBLIC_FUNC int myx_free_syn(MYX_SYN *syn);

/* syntax-highlighting */
MYX_PUBLIC_FUNC MYX_SQL_HIGHLIGHTING* myx_init_sql_parsing(MYX_SYN* syn);
MYX_PUBLIC_FUNC int myx_free_sql_highlighting(MYX_SQL_HIGHLIGHTING *h);
MYX_PUBLIC_FUNC int myx_highlight(MYX_SQL_HIGHLIGHTING *sql_highlighting, const char *text, unsigned int line_no);
MYX_PUBLIC_FUNC MYX_SYN_TYPE myx_get_identifier_type(MYX_SYN *syn, const char *word);

/* auto-completion */
MYX_PUBLIC_FUNC MYX_SYN_SUGGESTIONS* myx_lookup_line(MYX_SYN* syn, const char *line);
MYX_PUBLIC_FUNC MYX_SYN_SUGGESTIONS* myx_lookup_word(MYX_SYN* syn,const char *word_beg);
MYX_PUBLIC_FUNC int myx_free_syn_suggestions(MYX_SYN_SUGGESTIONS *s);

// query functions
MYX_PUBLIC_FUNC void myx_mysql_limit_resultset_size(MYSQL *mysql, unsigned int limitMB);
MYX_PUBLIC_FUNC unsigned int myx_mysql_get_resultset_size_limit(MYSQL *mysql);

MYX_PUBLIC_FUNC int myx_identifier_needs_quotes(const char *name);
MYX_PUBLIC_FUNC MYX_RESULTSET * myx_query_execute(MYSQL *mysql, const char *sql, int enforce_editable,
  MYX_STRINGLIST *params, MYX_LIB_ERROR *error_code, void *user_data,
  int (*progress_row_fetch) (unsigned long current_row_count, unsigned long previous_row_count, MYX_RESULTSET *result_set, void *user_data),
  void (*resultset_realloc_before) (void *user_data),
  void (*resultset_realloc_after) (void *user_data),
  bigint* affected_rows);
MYX_PUBLIC_FUNC int myx_query_free_resultset(MYX_RESULTSET *resultset);
MYX_PUBLIC_FUNC void myx_query_execute_direct(MYSQL *mysql, const char *sql, MYX_LIB_ERROR *error_code, bigint* affected_rows);

/* result-set functions */
MYX_PUBLIC_FUNC MYX_RS_ACTION * myx_query_create_action(MYX_RS_ACTION_TYPE action, unsigned int row, MYX_RS_COLUMN *column, unsigned int new_value_length, const char *new_value);
MYX_PUBLIC_FUNC int myx_query_free_action(MYX_RS_ACTION *action);
MYX_PUBLIC_FUNC int myx_query_add_action(MYX_RESULTSET *resultset, MYX_RS_ACTION *action);
MYX_PUBLIC_FUNC int myx_query_delete_action(MYX_RESULTSET *resultset, unsigned int action);
MYX_PUBLIC_FUNC MYX_RS_ACTION_ERRORS * myx_query_apply_actions(MYX_RESULTSET *resultset);
MYX_PUBLIC_FUNC int myx_query_update_resultset(MYX_RESULTSET *resultset);
MYX_PUBLIC_FUNC int myx_query_pack_resultset(MYX_RESULTSET *resultset);
MYX_PUBLIC_FUNC int myx_query_discard_actions(MYX_RESULTSET *resultset);
MYX_PUBLIC_FUNC int myx_query_free_action_errors(MYX_RS_ACTION_ERRORS *errors);

MYX_PUBLIC_FUNC int myx_get_unix_newline_count(const char *str);

MYX_PUBLIC_FUNC MYX_Q_TYPE myx_query_type(const char *sql);
MYX_PUBLIC_FUNC char * myx_query_add_column_to_sql(MYSQL *mysql, const char *default_schema, const char *catalog,
  const char *schema, const char *table, const char *column, const char *sqlcmd, MYX_Q_CLAUSE_TYPE clause_type,
  int *cursor_pos);
MYX_PUBLIC_FUNC char * myx_query_add_table_to_sql(MYSQL *mysql, const char *default_schema, const char *catalog, const char *schema,
                                                  const char *table, const char *sqlcmd, MYX_Q_TABLE_ADD_TYPE add_type,
                                                  int *cursor_pos, MYX_Q_TABLE_ADD_ERROR * error);
// return resultset as single integer value
MYX_PUBLIC_FUNC int myx_get_resultset_as_int(MYSQL *mysql, MYX_LIB_ERROR * error_code);

// query diff'ing
MYX_PUBLIC_FUNC int myx_query_compare_possible(MYX_RESULTSET *ra, MYX_RESULTSET *rb);
MYX_PUBLIC_FUNC int myx_query_compare_results(MYX_RESULTSET *ra, MYX_RESULTSET *rb);


// others
MYX_PUBLIC_FUNC char *myx_get_create_table_script(MYSQL *mysql, const char *catalog,
                                                  const char *schema, const char *table);

MYX_PUBLIC_FUNC int myx_check_whether_commits_transaction(MYSQL *mysql, const char *stmt);

// ---------------------------------------------------------
// MYX_DBM functions

MYX_PUBLIC_FUNC char * myx_dbm_get_create_sql(MYSQL *mysql, const char *catalog_name, const char *schema_name,
                                              const char *name, MYX_DBM_OBJECT_TYPE object_type, int fully_qualified,
                                              char quote_char, int add_sql_mode);


MYX_PUBLIC_FUNC MYX_DBM_SERVER_VERSION * myx_dbm_retrieve_server_version(MYSQL *mysql);
MYX_PUBLIC_FUNC void myx_dbm_free_server_version(MYX_DBM_SERVER_VERSION *version);

// table
MYX_PUBLIC_FUNC MYX_DBM_TABLE_DATA * myx_dbm_retrieve_table_data(MYSQL *mysql, MYX_DBM_DATATYPES *datatypes, MYX_ENGINES *engines,
  const char *catalog, const char *schema, const char *table, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC void myx_dbm_free_table_data(MYX_DBM_TABLE_DATA *table_data);
MYX_PUBLIC_FUNC char * myx_dbm_get_table_sql_diff(MYX_DBM_TABLE_DATA *existing_table, MYX_DBM_TABLE_DATA *altered_table,
  MYX_DBM_SERVER_VERSION *version, MYX_LIB_ERROR *error_code);

//Stored Procedure
MYX_PUBLIC_FUNC MYX_DBM_STORED_PROCEDURE_DATA * myx_dbm_get_sp_data(MYSQL *mysql, const char *catalog_name,
  const char *schema_name, const char *sp_name, MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type, char quote_char, int add_sql_mode);

MYX_PUBLIC_FUNC void myx_dbm_free_sp_data(MYX_DBM_STORED_PROCEDURE_DATA *sp_data);
MYX_PUBLIC_FUNC char * myx_dbm_make_script_from_sps(MYSQL *mysql, const char *catalog_name, const char *schema_name,
                                                    MYX_SCHEMA_STORED_PROCEDURES *splist, char quote_char);

MYX_PUBLIC_FUNC MYX_LIB_ERROR myx_dbm_create_view(MYSQL *mysql, const char *catalog, const char *schema,
                                                  const char *view_name, const char *select_sql);

MYX_PUBLIC_FUNC MYX_LIB_ERROR myx_dbm_drop_sp(MYSQL *mysql,
                                              const char *catalog_name,
                                              const char *schema_name,
                                              const char *sp_name,
                                              MYX_SCHEMA_STORED_PROCEDURE_TYPE sp_type);

// View
MYX_PUBLIC_FUNC MYX_LIB_ERROR myx_dbm_drop_view(MYSQL *mysql,
                                                const char *catalog_name,
                                                const char *schema_name,
                                                const char *view_name);

MYX_PUBLIC_FUNC MYX_DBM_VIEW_DATA * myx_dbm_get_view_data(MYSQL *mysql, const char *catalog, const char *schema,
                                                         const char *view, char quote_char);
MYX_PUBLIC_FUNC void myx_dbm_free_view_data(MYX_DBM_VIEW_DATA *vdata);

MYX_PUBLIC_FUNC char * myx_dbm_get_view_name_from_query(const char *create_view_query);

// Trigger
MYX_PUBLIC_FUNC MYX_DBM_TRIGGER_DATA * myx_dbm_get_trigger_data(MYSQL *mysql, const char *catalog, const char *schema,
                                                         const char *trigger, char quote_char);
MYX_PUBLIC_FUNC void myx_dbm_free_trigger_data(MYX_DBM_TRIGGER_DATA *trigger_data);

// Datatype functions

/* datatype - xmlfile functions */
MYX_PUBLIC_FUNC MYX_DBM_DATATYPES * myx_datatype_load(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_datatype_store(const char *filename, MYX_DBM_DATATYPES *obj);
MYX_PUBLIC_FUNC int myx_free_datatype(MYX_DBM_DATATYPES *obj);
MYX_PUBLIC_FUNC MYX_DBM_DATATYPE * myx_dbm_get_datatype(MYX_DBM_DATATYPES *datatypes, char **dtype);
MYX_PUBLIC_FUNC char * myx_dbm_get_datatype_name(char *dtype);
MYX_PUBLIC_FUNC char * myx_dbm_get_datatype_params(char *datatype_name, char *dtype);

// Charset functions

MYX_PUBLIC_FUNC MYX_DBM_CHARSETS * myx_dbm_retrieve_charsets(MYSQL *mysql, MYX_LIB_ERROR *error_code);

/* charset - xmlfile functions */
MYX_PUBLIC_FUNC int myx_charsets_store(const char *filename, MYX_DBM_CHARSETS *obj);
MYX_PUBLIC_FUNC MYX_DBM_CHARSETS * myx_charsets_load(const char *filename, MYX_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_charsets(MYX_DBM_CHARSETS *obj);

/* Others */
MYX_PUBLIC_FUNC MYX_IMAGE_FORMAT myx_guess_image_format(const void *data, unsigned int length);
MYX_PUBLIC_FUNC char * myx_dbm_get_sql_fk_create_code(MYX_DBM_FK_DATA *fk, MYX_DBM_SERVER_VERSION *version);

// ------------------------------------------------------------
// Table Export

MYX_PUBLIC_FUNC int myx_export_resultset(MYSQL *mysql, MYX_TABLE_EXPORTER_INFO *info,
                                         const char *filename,
                                         const char *header_format,
                                         MYX_RESULTSET *resultset,
                                         const char *detail_query);

MYX_PUBLIC_FUNC MYX_TABLE_EXPORTER * myx_get_table_exporter(const char *format);

MYX_PUBLIC_FUNC MYX_TABLE_EXPORTER_INFO * myx_get_table_exporter_info(const char *format);
MYX_PUBLIC_FUNC int myx_free_table_exporter_info(MYX_TABLE_EXPORTER_INFO *info);

MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_get_table_export_formats(void);

MYX_PUBLIC_FUNC int myx_set_table_exporter_option(MYX_TABLE_EXPORTER_INFO *info,
                                                  const char *name,
                                                  const char *value);

// ------------------------------------------------------------
// Text Shell

MYX_PUBLIC_FUNC MYX_TEXT_SHELL * myx_init_text_shell(MYSQL *mysql);
MYX_PUBLIC_FUNC void myx_finalize_text_shell(MYX_TEXT_SHELL *shell);
MYX_PUBLIC_FUNC void myx_ts_set_output_callback(MYX_TEXT_SHELL *shell, void *user_data,
    int (*process_output_func)(const char *text, void *user_data));
MYX_PUBLIC_FUNC int myx_ts_execute_command(MYX_TEXT_SHELL *shell, const char *command);
MYX_PUBLIC_FUNC void myx_ts_display_help(MYX_TEXT_SHELL *shell, const char *command);

// command parsing
MYX_PUBLIC_FUNC char * myx_parse_sqlmemo_command_use(const char *command);
MYX_PUBLIC_FUNC char * myx_parse_sqlmemo_command_load(const char *command);
MYX_PUBLIC_FUNC char * myx_parse_sqlmemo_command_delimiter(const char *command);
MYX_PUBLIC_FUNC int myx_parse_sqlmemo_command_exit(const char *command);
MYX_PUBLIC_FUNC char * myx_parse_sqlmemo_command_help(const char *command);
MYX_PUBLIC_FUNC int myx_parse_sqlmemo_command_transaction_start(const char *command);
MYX_PUBLIC_FUNC int myx_parse_sqlmemo_command_transaction_commit(const char *command);
MYX_PUBLIC_FUNC int myx_parse_sqlmemo_command_transaction_rollback(const char *command);

// Data conversion
MYX_PUBLIC_FUNC ubigint myx_bit_to_int(const char* value);
MYX_PUBLIC_FUNC char* myx_int_to_bit(ubigint value);

// Engine support
MYX_PUBLIC_FUNC MYX_ENGINES* myx_get_engines(MYSQL *mysql);
MYX_PUBLIC_FUNC void myx_free_engines(MYX_ENGINES* engines);
MYX_PUBLIC_FUNC MYX_ENGINE* myx_find_default_engine(MYX_ENGINES* engines);
MYX_PUBLIC_FUNC MYX_ENGINE* myx_copy_engine(MYX_ENGINE* engine);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
