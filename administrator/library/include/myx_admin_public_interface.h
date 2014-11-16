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

#ifndef myx_admin_public_interface_h
#define myx_admin_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif
#include <stdio.h>
#include <mysql.h>
#include <pcre.h>
#include <myx_util_public_interface.h>

#include <myx_public_interface.h>

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

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqladmin"
#define libmysqladmin_PUBLIC_INTERFACE_VERSION 10004

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_util_public_interface, myx_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\..\common\library\utilities\include\myx_util_public_interface.h"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\..\common\library\general-library\include\myx_public_interface.h"

/*
 * Enums
 */


typedef enum myx_admin_lib_error
{
  MYX_ADMIN_NO_ERROR = 0, MYX_ADMIN_ERROR_CANT_OPEN_FILE,MYX_ADMIN_XML_PARSE_ERROR, 
  MYX_ADMIN_XML_NO_VALID_DOCUMENT, MYX_ADMIN_XML_EMPTY_DOCUMENT, MYX_ADMIN_INI_PARSE_ERROR,
  MYX_ADMIN_GENERAL_ERROR, MYX_ADMIN_SQL_ERROR
} MYX_ADMIN_LIB_ERROR;

typedef enum {
    MYX_CHECKBOX, MYX_SPINEDIT, MYX_TEXTEDIT, MYX_DROPDOWNBOX
} MYX_GUI_WIDGET_TYPE;
  
  
typedef enum myx_platform
{
    MYX_WINDOWS, MYX_LINUX, MYX_MACOS
} MYX_PLATFORM;
 
typedef enum
{
  MYX_BT_SQL_SCRIPT
} MYX_BACKUP_TYPE;

typedef enum myx_rbs_options
{
  MYX_RBS_FORCE=1, // Continue even if we get an sql error. 
  MYX_RBS_DONT_CREATE_TARGETS=2 // Don't create the target schema and/or target catalog if it doesn't exist
} MYX_RBS_OPTIONS;

typedef enum myx_backup_options
{
  MYX_B_NO_CREATES           =    1,
  MYX_B_NO_EXTENDED_INSERT   =    2,
  MYX_B_ADD_DROP_TABLE       =    4,
  MYX_B_COMMENT              =    8,
  MYX_B_DONT_WRITE_FULL_PATH =   16,
  MYX_B_LOCK_ALL_TABLES      =   32,
  MYX_B_SINGLE_TRANSACTION   =   64,
  MYX_B_DISABLE_KEYS         =   128,
  MYX_B_COMPLETE_INSERTS     =   256,
  MYX_B_ANSI_QUOTES          =   512,
  MYX_B_ADD_LOCKS            =  1024,
  MYX_B_COMPLETE_SCHEMATAS   =  2048,
  MYX_B_SORT_TABLES          =  4096,
  MYX_B_COMPATIBILITY_MODE   =  8192,
  MYX_B_POINT_IN_TIME_BACKUP = 16384,
  MYX_B_OPTIMIZED_COMMIT     = 32768
} MYX_BACKUP_OPTIONS;

typedef enum myx_backup_error
{
  MYX_BACKUP_NO_ERROR,
  MYX_BACKUP_UNKNOWN,
  MYX_BACKUP_SERVER_ERROR,
  MYX_BACKUP_CANT_OPEN_FILE,
  MYX_BACKUP_CANT_READ_FROM_FILE,
  MYX_BACKUP_CANT_WRITE_TO_FILE,
  MYX_BACKUP_ILLEGAL_OPTION,
  MYX_BACKUP_PCRE_ERROR,
  MYX_BACKUP_MALLOC_FAILED,
  MYX_BACKUP_OUTPUTDEVICE_FULL,
  MYX_BACKUP_CANNOT_FLUSH_TABLES_WITH_READ_LOCK,
  MYX_BACKUP_CANNOT_START_TRANSACTION,
  MYX_BACKUP_CANNOT_SET_ANSI_QUOTES,
  MYX_BACKUP_XML_PARSE_ERROR,
  MYX_BACKUP_SQL_ERROR,
  MYX_BACKUP_STOPPED,
  MYX_BACKUP_CHARSET_CONVERSION,
  MYX_BACKUP_WRONG_CHARSET,
  MYX_BACKUP_FILE_IS_NOT_MA_DUMP,
  MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE,
  MYX_BACKUP_CANT_GET_SQL
} MYX_BACKUP_ERROR;

typedef enum myx_expression_error
{
  MYX_EXPRESSION_NO_ERROR=0, MYX_EXPRESSION_SYNTAX_ERROR, MYX_EXPRESSION_DIVISION_BY_ZERO, MYX_EXPRESSION_BAD_VARIABLE
} MYX_EXPRESSION_ERROR;

typedef enum
{
  MYX_LINE_GRAPH=1, MYX_BAR_GRAPH
} MYX_HEALTH_GRAPH_TYPE;

typedef enum
{
  MYX_HGVU_PERCENTAGE, MYX_HGVU_COUNT, MYX_HGVU_BYTE, MYX_HGVU_SECONDS
} MYX_HEALTH_GRAPH_VALUE_UNIT;

typedef enum {
  MYX_EVENT_START=1, MYX_EVENT_END, MYX_EVENT_ERROR, MYX_EVENT_INNODB_START, MYX_EVENT_INNODB_SHUTDOWN, 
  MYX_EVENT_FORCED_CLOSE_THREAD, MYX_EVENT_ABORT, MYX_EVENT_INIT, MYX_EVENT_CONNECT, MYX_EVENT_QUIT,
  MYX_EVENT_QUERY, MYX_EVENT_SELECT
} MYX_LOGFILE_EVENT_TYPE;

  
typedef enum
{
  MYX_MESSAGE_STATUS, MYX_MESSAGE_ERROR, MYX_MESSAGE_INFO, MYX_MESSAGE_WARNING
} MYX_COMMAND_MESSAGE_TYPE;


typedef enum
{
  MYX_CHECK_QUICK=1,
    MYX_CHECK_FAST=2, 
    MYX_CHECK_MEDIUM=4,
    MYX_CHECK_EXTENDED=8,
    MYX_CHECK_CHANGED=16,
    MYX_REPAIR_USE_FRM=32,
    MYX_REPAIR_NO_WRITE_TO_BINLOG=64
} MYX_TABLE_CHECK_TYPE;


typedef enum
{
  MYX_RHS_AVAILABLE, MYX_RHS_NOT_AVAILABLE, MYX_RHS_NEW_HOST
} MYX_REPL_HOST_STATUS;

typedef enum
{
  MYX_UOP_GLOBAL,
  MYX_UOP_SCHEMA,
  MYX_UOP_TABLE, 
  MYX_UOP_COLUMN,
  MYX_UOP_ROUTINE
} MYX_USER_OBJECT_PRIVILEGE_TYPE;

/*
 * Structs
 */

/* -----------------------------------------------------------------------------------------------------------
 * User information
 */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_OBJECT_PRIVILEGES
typedef struct {
    char *host;
    char *object_name;

    unsigned int user_privileges_num;
    char **user_privileges;
} MYX_USER_OBJECT_PRIVILEGES;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER
typedef struct {
    char *user_name;
    char *password;
    char *full_name;
    char *description;
    char *email;
    char *contact_information;
    unsigned int icon_length;
    char *icon;

    unsigned int hosts_num;
    char **hosts;

    unsigned int user_object_privileges_num;
    MYX_USER_OBJECT_PRIVILEGES *user_object_privileges;
} MYX_USER;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_NAMES
typedef struct {
    unsigned int user_names_num;
    char **user_names;
} MYX_USER_NAMES;


/* -----------------------------------------------------------------------------------------------------------
 * Processlist info
 */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_PROCESS_INFO
typedef struct {
    unsigned char *id;
    unsigned char *user;
    unsigned char *host;
    unsigned char *db;
    unsigned char *command;
    unsigned char *time;
    unsigned char *state;
    unsigned char *info;
} MYX_PROCESS_INFO;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_PROCESS_LIST
typedef struct {
    unsigned int process_infos_num;
    MYX_PROCESS_INFO *process_infos;
} MYX_PROCESS_LIST;

/* -----------------------------------------------------------------------------------------------------------
 * Server Variables Expression Evaluation
 */

// type can be 'V'ariable, variable 'D'elta, 'I'mmediate, +, -, *, /
typedef struct {
  int ntype; 
  int variable_index;
  double immediate;
} MYX_EXPRESSION_NODE;


typedef struct {
  unsigned int nodes_num;
  MYX_EXPRESSION_NODE *nodes;
} MYX_COMPILED_EXPRESSION;

/* -----------------------------------------------------------------------------------------------------------
 * Status / Server Variables
 */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VARIABLES
typedef struct {
  unsigned int variables_num;
  MYX_NAME_VALUE_PAIR *variables;
} MYX_VARIABLES;

// definitions for the variables_listing (which assigns each variable into a category)

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VARIABLE_ELEMENT
typedef struct {
  char *mysql_id;
  char *desc_id;
  unsigned int editable;
} MYX_VARIABLE_ELEMENT;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VARIABLES_SUBGROUP
typedef struct {
  unsigned int pos;
  char *name;
  char *caption_id;

  unsigned int variables_num;
  MYX_VARIABLE_ELEMENT *variables;
} MYX_VARIABLES_SUBGROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VARIABLES_GROUP
typedef struct {
  unsigned int pos;
  char *name;
  char *caption_id;

  unsigned int variables_num;
  MYX_VARIABLE_ELEMENT *variables;

  unsigned int subgroups_num;
  MYX_VARIABLES_SUBGROUP *subgroups;
} MYX_VARIABLES_GROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_VARIABLES_LISTING
typedef struct {
  unsigned int groups_num;
  MYX_VARIABLES_GROUP *groups;

} MYX_VARIABLES_LISTING;


// -----------------------------------------------------------------------------------------------------------
// Startup parameter

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_DROPDOWNBOX
typedef struct {
    int editable; // Specifies if the user can enter a new value not found in the items list

    unsigned int items_num;
    unsigned char **items;
} MYX_GUI_DROPDOWNBOX;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_TEXTEDIT
typedef struct {
    unsigned char *edit_type; //empty for normal string or ip, file, directory, innodbfilepath or memo
} MYX_GUI_TEXTEDIT;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_SPINEDIT
typedef struct {
    unsigned char *unitcontrolbox; /*NULL if there should be no control-box*/
} MYX_GUI_SPINEDIT;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_CHECKBOX
typedef struct {
  int is_boolean;
  int invert;
} MYX_GUI_CHECKBOX;


/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_WIDGET
typedef struct {
  unsigned char *id;

  unsigned int alt_names_num;
  char **alt_names;

  unsigned char *caption;
  unsigned char *description;
  unsigned char *default_value;
  int position;

  unsigned char *value; /*or checked/unchecked */
  int active;
  int loose_option;
  int multiple; /* this means the option can be given multiple times on the command-line */

  MYX_GUI_WIDGET_TYPE widget_type;

  MYX_GUI_CHECKBOX *checkbox;
  MYX_GUI_SPINEDIT *spinedit;
  MYX_GUI_TEXTEDIT *textedit;
  MYX_GUI_DROPDOWNBOX *dropdownbox;
} MYX_GUI_WIDGET;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_GROUP
typedef struct {
    unsigned char *caption;
    int position;
    unsigned char *id;

    unsigned int widgets_num;
    MYX_GUI_WIDGET *widgets;
} MYX_GUI_GROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_PAGE
typedef struct {
    unsigned char *caption;
    unsigned char *description;
    int position;
    unsigned char *id; /*a unique ID or NULL*/

    unsigned int groups_num;
    MYX_GUI_GROUP *groups;
} MYX_GUI_PAGE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GUI_DESCRIPTION
typedef struct {
    unsigned int pages_num;
    MYX_GUI_PAGE *pages;
} MYX_GUI_DESCRIPTION;

// -----------------------------------------------------------------------------------------------------------
// Backup content

// Attention! These constants are not translated automatically by the C -> Delphi mapper
// so if something changes here it must be manually adjusted in AuxAdminBackupRestore.pas!
// Additionally, the values implicitely determine order of elements in backup if sorted.
#define MYX_BTF_IGNORE_CONTENT  0x80000000  // don't backup table content
#define MYX_BTF_IS_TABLE        0x00000001  // is a table
#define MYX_BTF_IS_FUNCTION     0x00000002  // is a stored function
#define MYX_BTF_IS_PROCEDURE    0x00000004  // is a stored procedure
#define MYX_BTF_IS_VIEW         0x00000008  // is a view
#define MYX_BTF_IS_TRIGGER      0x00000010  // is a trigger

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BACKUP_TABLE
typedef struct
{
  char *catalog;
  char *schema;
  char *table;
  int  flags;     // collection of MYX_BTF_ constants
} MYX_BACKUP_TABLE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BACKUP_CONTENT
typedef struct
{
  unsigned int tables_num;
  MYX_BACKUP_TABLE *tables;
} MYX_BACKUP_CONTENT;  
 
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_BACKUP_PROFILE
typedef struct
{
  char *profile_name;
  char *last_used;
  int options;
  MYX_BACKUP_TYPE backup_type;
  MYX_BACKUP_CONTENT *backup_content;
} MYX_BACKUP_PROFILE;

/* -----------------------------------------------------------------------------------------------------------
 * Health Graphs
 */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_HEALTH_GRAPH
typedef struct {
  char *graph_caption;
  int display_graph_caption;
  MYX_HEALTH_GRAPH_TYPE graphtype;
  MYX_HEALTH_GRAPH_VALUE_UNIT value_unit;
  double min;
  double max;
  int autoextend_max;
  char *value_formula;
  char *max_formula;
  char *value_caption;
  char *value_caption_trans_id;
  char *max_caption;
  char *max_caption_trans_id;
  int refreshtime;
  int pos;
} MYX_HEALTH_GRAPH;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_HEALTH_GROUPS
typedef struct {
  char *caption;
  char *caption_trans_id;
  int pos;

  unsigned int graphs_num; 
  MYX_HEALTH_GRAPH *graphs;
} MYX_HEALTH_GROUP;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_HEALTH_PAGE
typedef struct {
  char *caption;
  char *caption_trans_id;
  char *description;
  char *description_trans_id;
  int pos;

  unsigned int groups_num; 
  MYX_HEALTH_GROUP *groups;
} MYX_HEALTH_PAGE;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_HEALTH_PAGES
typedef struct {
  unsigned int pages_num; 
  MYX_HEALTH_PAGE *pages;
} MYX_HEALTH_PAGES;

/* -----------------------------------------------------------------------------------------------------------
 * Logfiles
 */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_DATETIME
typedef struct {
  unsigned int tm_year;
  unsigned int tm_mon;
  unsigned int tm_mday;

  unsigned int tm_hour;
  unsigned int tm_min;
  unsigned int tm_sec;
} MYX_DATETIME;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_LOGFILE_EVENT
typedef struct {
  MYX_LOGFILE_EVENT_TYPE event_type;
  int line_no;
  MYX_DATETIME *date;
} MYX_LOGFILE_EVENT;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_LOGFILE
typedef struct {
  unsigned int events_num;
  MYX_LOGFILE_EVENT *events;

  bigint file_size;
  unsigned int block_num;
  
  unsigned int lines_num;
  char **lines;
} MYX_LOGFILE;

  
// -----------------------------------------------------------------------------------------------------------

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_TABLE_COMMAND_STATUS
typedef struct myx_table_command_status
{
  char *table;
  MYX_COMMAND_MESSAGE_TYPE message_type;
  char *message;
} MYX_TABLE_COMMAND_STATUS;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_TABLE_COMMAND_STATUSES
typedef struct myx_table_command_statuses
{
  unsigned int status_num;
  MYX_TABLE_COMMAND_STATUS *status;
} MYX_TABLE_COMMAND_STATUSES;


// -----------------------------------------------------------------------------------------------------------
// Replication stuff

/* the xml structs */

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_REPL_HOST
typedef struct
{
  char *name;
} MYX_USER_REPL_HOST;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_USER_REPL_HOSTS
typedef struct
{
  unsigned int hosts_num;
  MYX_USER_REPL_HOST *hosts;
} MYX_USER_REPL_HOSTS;


/* The sql structs */


/* has all columns of "show slave hosts" */
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_REPL_HOST
typedef struct
{
  int server_id;
  char *host;
  int port;
  int rpl_recovery_rank;
  int master_id;
  int is_master;
  char *binlog_file;
  char *binlog_pos;
  MYX_REPL_HOST_STATUS status;
} MYX_REPL_HOST;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_REPL_HOSTS
typedef struct
{
  unsigned int hosts_num;
  MYX_REPL_HOST *hosts;
} MYX_REPL_HOSTS;


/*
 * Functions
 */

MYX_PUBLIC_FUNC int myx_get_admin_public_interface_version();

MYX_PUBLIC_FUNC int myx_free_admin_lib_stringlist(MYX_STRINGLIST *stringlist);

/* user management functions */

MYX_PUBLIC_FUNC MYX_USER_NAMES * myx_get_user_names(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_free_user_names(MYX_USER_NAMES *user_names);
MYX_PUBLIC_FUNC int myx_check_mysql_user_info_table(MYSQL *mysql, int check_only);

MYX_PUBLIC_FUNC MYX_USER * myx_get_user(MYSQL *mysql, const char *user_name);
MYX_PUBLIC_FUNC MYX_USER * myx_get_user_with_privileges(MYSQL *mysql, const char *user_name);
MYX_PUBLIC_FUNC int myx_set_user(MYSQL *mysql, MYX_USER *user, const char *previous_user_name, int new_user);
MYX_PUBLIC_FUNC int myx_del_user(MYSQL *mysql, const char *user_name);
MYX_PUBLIC_FUNC int myx_free_user(MYX_USER *user);
MYX_PUBLIC_FUNC MYX_USER_OBJECT_PRIVILEGES * myx_get_privilege_struct(MYSQL *mysql, const char *object_name, MYX_USER_OBJECT_PRIVILEGE_TYPE privilege_type);
MYX_PUBLIC_FUNC int myx_free_user_priv(MYX_USER_OBJECT_PRIVILEGES *user_priv);

MYX_PUBLIC_FUNC char * myx_resize_vector_block(char * ptr, size_t size_of_block, size_t count_of_blocks);
/* process_list functions */

MYX_PUBLIC_FUNC MYX_PROCESS_LIST * myx_get_process_list(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_free_process_list(MYX_PROCESS_LIST *process_list);
MYX_PUBLIC_FUNC int myx_kill_thread(MYSQL *mysql, unsigned long pid);

/* status/server variables functions */

MYX_PUBLIC_FUNC MYX_VARIABLES* myx_get_server_variables(MYSQL *mysql);
MYX_PUBLIC_FUNC MYX_VARIABLES* myx_get_status_variables(MYSQL *mysql);
MYX_PUBLIC_FUNC int myx_free_variables(MYX_VARIABLES *vars);

MYX_PUBLIC_FUNC MYX_VARIABLES_LISTING* myx_get_variables_listing(const char *filename, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_variables_listing(MYX_VARIABLES_LISTING* variables_listing);

MYX_PUBLIC_FUNC int myx_set_variable(MYSQL *mysql, const char *mysql_id, const char *value);

MYX_PUBLIC_FUNC char* myx_get_running_service_name(int port);
MYX_PUBLIC_FUNC char* myx_get_running_service_config_file(int port);

/* cnf file functions */

MYX_PUBLIC_FUNC MYX_GUI_DESCRIPTION * myx_get_gui_description(const char *filename, const char *version, MYX_PLATFORM platform, MYX_ADMIN_LIB_ERROR *error_code, const char *ini_filepath, const char *ini_file_group);
MYX_PUBLIC_FUNC int myx_free_gui_description(MYX_GUI_DESCRIPTION *gui_desc);

MYX_PUBLIC_FUNC MYX_ADMIN_LIB_ERROR myx_update_mysql_cnf_file(MYX_GUI_DESCRIPTION *desc, const char *ini_filepath, const char *group);
MYX_PUBLIC_FUNC MYX_ADMIN_LIB_ERROR myx_update_mysql_cnf_filef(MYX_GUI_DESCRIPTION *desc, const char *ini_filepath, FILE *new_file, const char *group);
MYX_PUBLIC_FUNC MYX_STRINGLIST* myx_get_all_cnf_sections(const char *filename, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC char* myx_get_cnf_value(const char *filename, const char *section, const char *name, MYX_ADMIN_LIB_ERROR *error_code);

/* Backup related functions */
  
MYX_PUBLIC_FUNC MYX_ADMIN_LIB_ERROR myx_save_profile(const char *profile_name, const char *profile_directory, MYX_BACKUP_PROFILE *backup_profile);
MYX_PUBLIC_FUNC MYX_BACKUP_PROFILE * myx_load_profile(const char *profile_name, const char *profile_directory, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_profile(MYX_BACKUP_PROFILE *profile);
  
MYX_PUBLIC_FUNC MYX_BACKUP_ERROR myx_make_backup_with_profile(MYSQL *mysql,  MYX_BACKUP_PROFILE *profile, const char *path, int callback_interval, 
                                                              int (*progress_report_profile) (const char *current_table_name, int num_tables, int num_tables_processed, int num_rows, int num_rows_processed, 
                                                                                              void *user_data), 
                                                              void *user_data);
MYX_PUBLIC_FUNC MYX_BACKUP_ERROR myx_make_backup(MYSQL *mysql, const char *filename, MYX_BACKUP_CONTENT *content, MYX_BACKUP_TYPE backup_type, int options, int callback_interval, int (*progress_report_make_backup)(const char *current_table_name, int num_tables, int num_tables_processed, int num_rows, int num_rows_processed, void *user_data), void *user_data);
  
MYX_PUBLIC_FUNC MYX_BACKUP_CONTENT* myx_get_backup_content(const char *filename, const char *filename_charset,
  MYX_BACKUP_TYPE backup_type, int report_interval,
  int (*progress_report) (bigint bytes_read, bigint bytes_total, void *user_data),
  void (*report_warning) (const char *msg, void *user_data),
  void *user_data, MYX_BACKUP_ERROR *error, int forced_schema, int ignore_errors);
MYX_PUBLIC_FUNC int myx_free_backup_content(MYX_BACKUP_CONTENT *backup);

MYX_PUBLIC_FUNC MYX_BACKUP_CONTENT * myx_get_restore_drop_list(MYSQL *mysql, MYX_BACKUP_CONTENT *content);

MYX_PUBLIC_FUNC MYX_BACKUP_ERROR 
    myx_restore_backup(MYSQL *mysql, const char *filename, const char *filename_charset, MYX_BACKUP_CONTENT *content, const char *target_catalog, 
                       const char *target_schema, MYX_BACKUP_TYPE backup_type,int options, int report_interval, 
                       int (*progress_report) (bigint bytes_read, bigint bytes_total, void *user_data), 
                       void *ruser_data, void (*report_warning) (const char *msg, void *user_data), void *wuser_data);


MYX_PUBLIC_FUNC char * myx_get_backup_error_string(MYX_BACKUP_ERROR error);

/* Expression evaluation functions */
MYX_PUBLIC_FUNC MYX_COMPILED_EXPRESSION * myx_compile_expression(const char *expr, MYX_EXPRESSION_ERROR *error);
MYX_PUBLIC_FUNC double myx_eval_expression(MYX_COMPILED_EXPRESSION *expr, MYX_VARIABLES *old_vars, MYX_VARIABLES *vars, MYX_EXPRESSION_ERROR *error);
MYX_PUBLIC_FUNC int myx_free_expression(MYX_COMPILED_EXPRESSION *expr);

/* Error logfile functions */  
MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_error_log(const char *filename, int block_size, int block_num, int *last_block);
MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_slow_log(const char *filename, int block_size, int block_num, int *last_block);
MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_general_log(const char *filename, int block_size, int block_num, int *last_block);

MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_error_logf(FILE *fp, int block_size, int block_num, int *last_block);
MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_slow_logf(FILE *fp, int block_size, int block_num, int *last_block);
MYX_PUBLIC_FUNC MYX_LOGFILE* myx_parse_general_logf(FILE *fp, int block_size, int block_num, int *last_block);

MYX_PUBLIC_FUNC int myx_free_logfile(MYX_LOGFILE *logfile);

/* Table command functions */
MYX_PUBLIC_FUNC MYX_TABLE_COMMAND_STATUSES * myx_optimize_table(MYSQL *mysql, const char *objects, int type_mask);
MYX_PUBLIC_FUNC MYX_TABLE_COMMAND_STATUSES * myx_check_table(MYSQL *mysql, const char *objects, int type_mask);
MYX_PUBLIC_FUNC MYX_TABLE_COMMAND_STATUSES * myx_repair_table(MYSQL *mysql, const char *objects, int type_mask);
MYX_PUBLIC_FUNC int myx_free_command_status(MYX_TABLE_COMMAND_STATUSES *status);
 
/* Health xml functions */
MYX_PUBLIC_FUNC MYX_HEALTH_PAGES* myx_read_in_health_pages(const char* filename, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_free_health_pages(MYX_HEALTH_PAGES *pages);
MYX_PUBLIC_FUNC int myx_save_health_pages(MYX_HEALTH_PAGES *pages, const char *filename);

/* replication functions */
MYX_PUBLIC_FUNC MYX_REPL_HOSTS * myx_show_repl_hosts_status(MYSQL *mysql, MYX_USER_REPL_HOSTS *replist, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC MYX_USER_REPL_HOSTS * myx_read_repl_user_hosts(const char *filename, MYX_ADMIN_LIB_ERROR *error_code);
MYX_PUBLIC_FUNC int myx_save_repl_user_hosts(const MYX_USER_REPL_HOSTS *replist, const char *filename);
MYX_PUBLIC_FUNC int myx_free_repl_user_hosts(MYX_USER_REPL_HOSTS *replist);
MYX_PUBLIC_FUNC int myx_free_repl_hosts_status(MYX_REPL_HOSTS *list);

MYX_PUBLIC_FUNC void myx_compact_privs(MYSQL *mysql);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif


