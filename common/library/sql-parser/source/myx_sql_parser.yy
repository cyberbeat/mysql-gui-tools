/* Copyright (C) 2000-2003 MySQL AB

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

/* sql_yacc.yy */
%{

#include <stdio.h>
#include "MyxSQLTreeItem.h"

#define YYSTYPE void*

#ifdef __cplusplus
extern "C" {
#endif

extern int yylex(void **yylval);
void *tree;

#ifdef __cplusplus
}
#endif

extern void yyerror(const char *);

%}

%pure_parser

%token  END_OF_INPUT

%token  ABORT_SYM
%token  ACTION
%token  ADD
%token  ADDDATE_SYM
%token  AFTER_SYM
%token  AGAINST
%token  AGGREGATE_SYM
%token  ALGORITHM_SYM
%token  ALL
%token  ALTER
%token  ANALYZE_SYM
%token  AND_AND_SYM
%token  AND_SYM
%token  ANY_SYM
%token  AS
%token  ASC
%token  ASCII_SYM
%token  ASENSITIVE_SYM
%token  ATAN
%token  AUTO_INC
%token  AVG_ROW_LENGTH
%token  AVG_SYM
%token  BACKUP_SYM
%token  BEFORE_SYM
%token  BEGIN_SYM
%token  BENCHMARK_SYM
%token  BERKELEY_DB_SYM
%token  BIGINT
%token  BINARY
%token  BINLOG_SYM
%token  BIN_NUM
%token  BITAND_SYM
%token  BIT_OR
%token  BIT_SYM
%token  BIT_XOR
%token  BLOB_SYM
%token  BOOLEAN_SYM
%token  BOOL_SYM
%token  BOTH
%token  BTREE_SYM
%token  BY
%token  BYTE_SYM
%token  CACHE_SYM
%token  CALL_SYM
%token  CASCADE
%token  CASCADED
%token  CAST_SYM
%token  CHAIN_SYM
%token  CHANGE
%token  CHANGED
%token  CHARSET
%token  CHAR_SYM
%token  CHECKSUM_SYM
%token  CHECK_SYM
%token  CIPHER_SYM
%token  CLIENT_SYM
%token  CLOSE_SYM
%token  COALESCE
%token  COLLATE_SYM
%token  COLLATION_SYM
%token  COLUMNS
%token  COLUMN_SYM
%token  COMMENT_SYM
%token  COMMITTED_SYM
%token  COMMIT_SYM
%token  COMPACT_SYM
%token  COMPRESSED_SYM
%token  CONCAT
%token  CONCAT_WS
%token  CONCURRENT
%token  CONDITION_SYM
%token  CONNECTION_SYM
%token  CONSISTENT_SYM
%token  CONSTRAINT
%token  CONTAINS_SYM
%token  CONTINUE_SYM
%token  CONVERT_SYM
%token  CONVERT_TZ_SYM
%token  COUNT_SYM
%token  CREATE
%token  CROSS
%token  CUBE_SYM
%token  CURDATE
%token  CURRENT_USER
%token  CURSOR_SYM
%token  CURTIME
%token  DATABASE
%token  DATABASES
%token  DATA_SYM
%token  DATETIME
%token  DATE_ADD_INTERVAL
%token  DATE_SUB_INTERVAL
%token  DATE_SYM
%token  DAY_HOUR_SYM
%token  DAY_MICROSECOND_SYM
%token  DAY_MINUTE_SYM
%token  DAY_SECOND_SYM
%token  DAY_SYM
%token  DEALLOCATE_SYM
%token  DECIMAL_NUM
%token  DECIMAL_SYM
%token  DECLARE_SYM
%token  DECODE_SYM
%token  DEFAULT
%token  DEFINER_SYM
%token  DELAYED_SYM
%token  DELAY_KEY_WRITE_SYM
%token  DELETE_SYM
%token  DESC
%token  DESCRIBE
%token  DES_DECRYPT_SYM
%token  DES_ENCRYPT_SYM
%token  DES_KEY_FILE
%token  DETERMINISTIC_SYM
%token  DIRECTORY_SYM
%token  DISABLE_SYM
%token  DISCARD
%token  DISTINCT
%token  DIV_SYM
%token  DOUBLE_SYM
%token  DO_SYM
%token  DROP
%token  DUAL_SYM
%token  DUMPFILE
%token  DUPLICATE_SYM
%token  DYNAMIC_SYM
%token  EACH_SYM
%token  ELSEIF_SYM
%token  ELT_FUNC
%token  ENABLE_SYM
%token  ENCLOSED
%token  ENCODE_SYM
%token  ENCRYPT
%token  END
%token  ENGINES_SYM
%token  ENGINE_SYM
%token  ENUM
%token  EQ
%token  EQUAL_SYM
%token  ERRORS
%token  ESCAPED
%token  ESCAPE_SYM
%token  EVENTS_SYM
%token  EXECUTE_SYM
%token  EXISTS
%token  EXIT_SYM
%token  EXPANSION_SYM
%token  EXPORT_SET
%token  EXTENDED_SYM
%token  EXTRACT_SYM
%token  FALSE_SYM
%token  FAST_SYM
%token  FETCH_SYM
%token  FIELD_FUNC
%token  FILE_SYM
%token  FIRST_SYM
%token  FIXED_SYM
%token  FLOAT_NUM
%token  FLOAT_SYM
%token  FLUSH_SYM
%token  FORCE_SYM
%token  FOREIGN
%token  FORMAT_SYM
%token  FOR_SYM
%token  FOUND_SYM
%token  FRAC_SECOND_SYM
%token  FROM
%token  FROM_UNIXTIME
%token  FULL
%token  FULLTEXT_SYM
%token  FUNCTION_SYM
%token  FUNC_ARG0
%token  FUNC_ARG1
%token  FUNC_ARG2
%token  FUNC_ARG3
%token  GE
%token  GEOMCOLLFROMTEXT
%token  GEOMETRYCOLLECTION
%token  GEOMETRY_SYM
%token  GEOMFROMTEXT
%token  GEOMFROMWKB
%token  GET_FORMAT
%token  GLOBAL_SYM
%token  GOTO_SYM
%token  GRANT
%token  GRANTS
%token  GREATEST_SYM
%token  GROUP
%token  GROUP_CONCAT_SYM
%token  GROUP_UNIQUE_USERS
%token  GT_SYM
%token  HANDLER_SYM
%token  HASH_SYM
%token  HAVING
%token  HELP_SYM
%token  HEX_NUM
%token  HIGH_PRIORITY
%token  HOSTS_SYM
%token  HOUR_MICROSECOND_SYM
%token  HOUR_MINUTE_SYM
%token  HOUR_SECOND_SYM
%token  HOUR_SYM
%token  IDENT
%token  IDENTIFIED_SYM
%token  IF
%token  IGNORE_SYM
%token  IMPORT
%token  INDEXES
%token  INDEX_SYM
%token  INFILE
%token  INNER_SYM
%token  INNOBASE_SYM
%token  INOUT_SYM
%token  INSENSITIVE_SYM
%token  INSERT
%token  INSERT_METHOD
%token  INTERVAL_SYM
%token  INTO
%token  INT_SYM
%token  INVOKER_SYM
%token  IN_SYM
%token  IS
%token  ISOLATION
%token  ISSUER_SYM
%token  ITERATE_SYM
%token  JOIN_SYM
%token  KEYS
%token  KEY_SYM
%token  KILL_SYM
%token  LABEL_SYM
%token  LANGUAGE_SYM
%token  LAST_INSERT_ID
%token  LAST_SYM
%token  LE
%token  LEADING
%token  LEAST_SYM
%token  LEAVES
%token  LEAVE_SYM
%token  LEFT
%token  LEVEL_SYM
%token  LEX_HOSTNAME
%token  LIKE
%token  LIMIT
%token  LINEFROMTEXT
%token  LINES
%token  LINESTRING
%token  LOAD
%token  LOCAL_SYM
%token  LOCATE
%token  LOCATOR_SYM
%token  LOCKS_SYM
%token  LOCK_SYM
%token  LOGS_SYM
%token  LOG_SYM
%token  LONGBLOB
%token  LONGTEXT
%token  LONG_NUM
%token  LONG_SYM
%token  LOOP_SYM
%token  LOW_PRIORITY
%token  LT
%token  MAKE_SET_SYM
%token  MASTER_CONNECT_RETRY_SYM
%token  MASTER_HOST_SYM
%token  MASTER_LOG_FILE_SYM
%token  MASTER_LOG_POS_SYM
%token  MASTER_PASSWORD_SYM
%token  MASTER_PORT_SYM
%token  MASTER_POS_WAIT
%token  MASTER_SERVER_ID_SYM
%token  MASTER_SSL_CAPATH_SYM
%token  MASTER_SSL_CA_SYM
%token  MASTER_SSL_CERT_SYM
%token  MASTER_SSL_CIPHER_SYM
%token  MASTER_SSL_KEY_SYM
%token  MASTER_SSL_SYM
%token  MASTER_SYM
%token  MASTER_USER_SYM
%token  MATCH
%token  MAX_CONNECTIONS_PER_HOUR
%token  MAX_QUERIES_PER_HOUR
%token  MAX_ROWS
%token  MAX_SYM
%token  MAX_UPDATES_PER_HOUR
%token  MAX_USER_CONNECTIONS_SYM
%token  MEDIUMBLOB
%token  MEDIUMINT
%token  MEDIUMTEXT
%token  MEDIUM_SYM
%token  MERGE_SYM
%token  MICROSECOND_SYM
%token  MIGRATE_SYM
%token  MINUTE_MICROSECOND_SYM
%token  MINUTE_SECOND_SYM
%token  MINUTE_SYM
%token  MIN_ROWS
%token  MIN_SYM
%token  MLINEFROMTEXT
%token  MODE_SYM
%token  MODIFIES_SYM
%token  MODIFY_SYM
%token  MOD_SYM
%token  MONTH_SYM
%token  MPOINTFROMTEXT
%token  MPOLYFROMTEXT
%token  MULTILINESTRING
%token  MULTIPOINT
%token  MULTIPOLYGON
%token  MUTEX_SYM
%token  NAMES_SYM
%token  NAME_SYM
%token  NATIONAL_SYM
%token  NATURAL
%token  NCHAR_STRING
%token  NCHAR_SYM
%token  NDBCLUSTER_SYM
%token  NE
%token  NEW_SYM
%token  NEXT_SYM
%token  NONE_SYM
%token  NOT2_SYM
%token  NOT_SYM
%token  NOW_SYM
%token  NO_SYM
%token  NO_WRITE_TO_BINLOG
%token  NULL_SYM
%token  NUM
%token  NUMERIC_SYM
%token  NVARCHAR_SYM
%token  OFFSET_SYM
%token  OLD_PASSWORD
%token  ON
%token  ONE_SHOT_SYM
%token  ONE_SYM
%token  OPEN_SYM
%token  OPTIMIZE
%token  OPTION
%token  OPTIONALLY
%token  OR2_SYM
%token  ORDER_SYM
%token  OR_OR_SYM
%token  OR_SYM
%token  OUTER
%token  OUTFILE
%token  OUT_SYM
%token  PACK_KEYS_SYM
%token  PARTIAL
%token  PASSWORD
%token  PARAM_MARKER
%token  PHASE_SYM
%token  POINTFROMTEXT
%token  POINT_SYM
%token  POLYFROMTEXT
%token  POLYGON
%token  POSITION_SYM
%token  PRECISION
%token  PREPARE_SYM
%token  PREV_SYM
%token  PRIMARY_SYM
%token  PRIVILEGES
%token  PROCEDURE
%token  PROCESS
%token  PROCESSLIST_SYM
%token  PURGE
%token  QUARTER_SYM
%token  QUERY_SYM
%token  QUICK
%token  RAID_0_SYM
%token  RAID_CHUNKS
%token  RAID_CHUNKSIZE
%token  RAID_STRIPED_SYM
%token  RAID_TYPE
%token  RAND
%token  READS_SYM
%token  READ_SYM
%token  REAL
%token  RECOVER_SYM
%token  REDUNDANT_SYM
%token  REFERENCES
%token  REGEXP
%token  RELAY_LOG_FILE_SYM
%token  RELAY_LOG_POS_SYM
%token  RELAY_THREAD
%token  RELEASE_SYM
%token  RELOAD
%token  RENAME
%token  REPAIR
%token  REPEATABLE_SYM
%token  REPEAT_SYM
%token  REPLACE
%token  REPLICATION
%token  REQUIRE_SYM
%token  RESET_SYM
%token  RESOURCES
%token  RESTORE_SYM
%token  RESTRICT
%token  RESUME_SYM
%token  RETURNS_SYM
%token  RETURN_SYM
%token  REVOKE
%token  RIGHT
%token  ROLLBACK_SYM
%token  ROLLUP_SYM
%token  ROUND
%token  ROUTINE_SYM
%token  ROWS_SYM
%token  ROW_COUNT_SYM
%token  ROW_FORMAT_SYM
%token  ROW_SYM
%token  RTREE_SYM
%token  SAVEPOINT_SYM
%token  SECOND_MICROSECOND_SYM
%token  SECOND_SYM
%token  SECURITY_SYM
%token  SELECT_SYM
%token  SENSITIVE_SYM
%token  SEPARATOR_SYM
%token  SERIALIZABLE_SYM
%token  SERIAL_SYM
%token  SESSION_SYM
%token  SET
%token  SET_VAR
%token  SHARE_SYM
%token  SHOW
%token  SHUTDOWN
%token  SIGNED_SYM
%token  SIMPLE_SYM
%token  SLAVE
%token  SMALLINT
%token  SNAPSHOT_SYM
%token  SOUNDS_SYM
%token  SPATIAL_SYM
%token  SPECIFIC_SYM
%token  SQLEXCEPTION_SYM
%token  SQLSTATE_SYM
%token  SQLWARNING_SYM
%token  SQL_BIG_RESULT
%token  SQL_BUFFER_RESULT
%token  SQL_CACHE_SYM
%token  SQL_CALC_FOUND_ROWS
%token  SQL_NO_CACHE_SYM
%token  SQL_SMALL_RESULT
%token  SQL_SYM
%token  SQL_THREAD
%token  SSL_SYM
%token  STARTING
%token  START_SYM
%token  STATUS_SYM
%token  STD_SYM
%token  STDDEV_SAMP_SYM
%token  STOP_SYM
%token  STORAGE_SYM
%token  STRAIGHT_JOIN
%token  STRING_SYM
%token  SUBDATE_SYM
%token  SUBJECT_SYM
%token  SUBSTRING
%token  SUBSTRING_INDEX
%token  SUM_SYM
%token  SUPER_SYM
%token  SUSPEND_SYM
%token  SYSDATE
%token  TABLES
%token  TABLESPACE
%token  TABLE_SYM
%token  TEMPORARY
%token  TEMPTABLE_SYM
%token  TERMINATED
%token  STRING
%token  TEXT_SYM
%token  TIMESTAMP
%token  TIMESTAMP_ADD
%token  TIMESTAMP_DIFF
%token  TIME_SYM
%token  TINYBLOB
%token  TINYINT
%token  TINYTEXT
%token  TO_SYM
%token  TRAILING
%token  TRANSACTION_SYM
%token  TRIGGER_SYM
%token  TRIGGERS_SYM
%token  TRIM
%token  TRUE_SYM
%token  TRUNCATE_SYM
%token  TYPES_SYM
%token  TYPE_SYM
%token  UDF_RETURNS_SYM
%token  UDF_SONAME_SYM
%token  ULONGLONG_NUM
%token  UNCOMMITTED_SYM
%token  UNDEFINED_SYM
%token  UNDERSCORE_CHARSET
%token  UNDO_SYM
%token  UNICODE_SYM
%token  UNION_SYM
%token  UNIQUE_SYM
%token  UNIQUE_USERS
%token  UNIX_TIMESTAMP
%token  UNKNOWN_SYM
%token  UNLOCK_SYM
%token  UNLOCK_SYM
%token  UNSIGNED
%token  UNTIL_SYM
%token  UNTIL_SYM
%token  UPDATE_SYM
%token  UPDATE_SYM
%token  USAGE
%token  USER
%token  USE_FRM
%token  USE_SYM
%token  USING
%token  UTC_DATE_SYM
%token  UTC_TIMESTAMP_SYM
%token  UTC_TIME_SYM
%token  VAR_SAMP_SYM
%token  VALUES
%token  VALUE_SYM
%token  VARBINARY
%token  VARCHAR
%token  VARIABLES
%token  VARIANCE_SYM
%token  VARYING
%token  VIEW_SYM
%token  WARNINGS
%token  WEEK_SYM
%token  WHEN_SYM
%token  WHERE
%token  WHILE_SYM
%token  WITH
%token  WORK_SYM
%token  WRITE_SYM
%token  X509_SYM
%token  XA_SYM
%token  XOR
%token  YEARWEEK
%token  YEAR_MONTH_SYM
%token  YEAR_SYM
%token  ZEROFILL
%token  UP_SYM
%token  BITAND_SYM
%token  CLB_SYM
%token  CRB_SYM

%token QUOTED
%token LT_SYM
%token DOT_SYM
%token ANDAND_SYM
%token GE_SYM
%token LE_SYM
%token SHR_SYM
%token SHL_SYM
%token OROR_SYM
%token NE_SYM
%token ASSIGN_SYM
%token LP_SYM
%token RP_SYM
%token COMMA_SYM
%token PERCENT_SYM
%token SEMICOL_SYM
%token AT_SYM
%token STRING
%token NOT_EXCL_MARKER
%token PLUS_SYM
%token MINUS_SYM
%token MULL_SYM
%token DIV_DIV_SYM
%token VERT_LINE_SYM
%token COL_SYM

%left   JOIN_SYM INNER_SYM STRAIGHT_JOIN CROSS LEFT RIGHT 

/* A dummy token to force the priority of table_ref production in a join. */
%left TABLE_REF_PRIORITY
%left SET_VAR
%left	OR_OR_SYM OROR_SYM OR_SYM OR2_SYM XOR
%left	AND_SYM AND_AND_SYM ANDAND_SYM
%left	BETWEEN_SYM CASE_SYM WHEN_SYM THEN_SYM ELSE
%left	ASSIGN_SYM EQ EQUAL_SYM GE GE_SYM GT_SYM LE LE_SYM LT LT_SYM NE NE_SYM IS LIKE REGEXP IN_SYM
%left	VERT_LINE_SYM
%left	BITAND_SYM
%left	SHL_SYM SHR_SYM
%left	MINUS_SYM PLUS_SYM
%left	MULL_SYM DIV_DIV_SYM PERCENT_SYM DIV_SYM MOD_SYM
%left UP_SYM
%left	NEG '~'
%right NOT_SYM NOT2_SYM
%right BINARY COLLATE_SYM

%%

query:
	| verb_clause { tree= $$= $1; } ;

verb_clause:
	  statement { $$= $1; }
	| begin { $$= $1; }
	;

/* Verb clauses, except begin */
statement:
	  alter { $$= $1; }
	| analyze { $$= $1; }
	| backup { $$= $1; }
	| call { $$= $1; }
	| change { $$= $1; }
	| check { $$= $1; }
	| checksum { $$= $1; }
	| commit { $$= $1; }
	| create { $$= $1; }
  | deallocate { $$= $1; }
	| delete { $$= $1; }
	| describe { $$= $1; }
	| do { $$= $1; }
	| drop { $$= $1; }
  | execute { $$= $1; }
	| flush { $$= $1; }
	| grant { $$= $1; }
	| handler { $$= $1; }
	| help { $$= $1; }
	| insert { $$= $1; }
	| kill { $$= $1; }
	| load { $$= $1; }
	| lock { $$= $1; }
	| optimize { $$= $1; }
  | keycache { $$= $1; }
	| preload { $$= $1; }
  | prepare { $$= $1; }
	| purge { $$= $1; }
	| release { $$= $1; }
	| rename { $$= $1; }
	| repair { $$= $1; }
	| replace { $$= $1; }
	| reset { $$= $1; }
	| restore { $$= $1; }
	| revoke { $$= $1; }
	| rollback { $$= $1; }
	| savepoint { $$= $1; }
	| select { $$= $1; }
	| set { $$= $1; }
	| show { $$= $1; }
	| slave { $$= $1; }
	| start { $$= $1; }
	| truncate { $$= $1; }
	| unlock { $$= $1; }
	| update { $$= $1; }
	| use { $$= $1; }
	| xa { $$= $1; }
  ;

deallocate:
    deallocate_or_drop PREPARE_SYM ident 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("prepare", "prepare"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("deallocate", "", p);
    }
  ;

deallocate_or_drop:
	  DEALLOCATE_SYM
	  {
	    $$= new_simple_tree_item("deallocate", "deallocate");
	  }
	| DROP
	  {
	    $$= new_simple_tree_item("drop", "drop");
	  }
	;

prepare:
    PREPARE_SYM ident FROM prepare_src 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("from", "from"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("prepare", "", p);
    }
  ;

prepare_src:
    TEXT_STRING_sys 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("prepare_src", "", p);
    }
  | AT_SYM ident_or_text 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("at", "@"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("prepare_src", "", p);
    }
  ;

execute:
    EXECUTE_SYM ident execute_using
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      $$= new_tree_item("execute", "", p);
    }
  ;

execute_using:
    /* nothing */
    {
      $$= NULL;
    }
  | USING execute_var_list
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("using", "using"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("execute_using", "", p);
    }
  ;

execute_var_list:
    execute_var_list COMMA_SYM execute_var_ident
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("execute_var_list", "", p);
    }
  | execute_var_ident
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("execute_var_list", "", p);
    }
  ;

execute_var_ident: 
    AT_SYM ident_or_text 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("at", "@"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("execute_var_ident", "", p);
    }
  ;

/* help */

help:
    HELP_SYM ident_or_text 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      $$= new_tree_item("help", "", p);
    }
  ;

/* change master */

change:
    CHANGE MASTER_SYM TO_SYM master_defs 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master", "master"));
      tree_item_list_add(p, new_simple_tree_item("to", "to"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("change", "", p);
    }
  ;

master_defs:
    master_def
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("master_defs", "", p);
    }
  | master_defs COMMA_SYM master_def
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_defs", "", p);
    }
  ;

master_def:
    MASTER_HOST_SYM EQ TEXT_STRING_sys 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_host", "master_host"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_USER_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_user", "master_user"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_PASSWORD_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_password", "master_password"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_PORT_SYM EQ ulong_num
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_port", "master_port"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_CONNECT_RETRY_SYM EQ ulong_num
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_connect_retry", "master_connect_retry"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_SYM EQ ulong_num
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl", "master_ssl"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_CA_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl_ca", "master_ssl_ca"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_CAPATH_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl_capath", "master_ssl_capath"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_CERT_SYM EQ TEXT_STRING_sys 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl_cert", "master_ssl_cert"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_CIPHER_SYM EQ TEXT_STRING_sys 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl_cipher", "master_ssl_cipher"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | MASTER_SSL_KEY_SYM EQ TEXT_STRING_sys 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_ssl_key", "master_ssl_key"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_def", "", p);
    }
  | master_file_def
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("master_def", "", p);
    }
  ;

master_file_def:
    MASTER_LOG_FILE_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_log_file", "master_log_file"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_file_def", "", p);
    }
  | MASTER_LOG_POS_SYM EQ ulonglong_num
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master_log_pos", "master_log_pos"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_file_def", "", p);
    }
  | RELAY_LOG_FILE_SYM EQ TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("relay_log_file", "relay_log_file"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_file_def", "", p);
    }
  | RELAY_LOG_POS_SYM EQ ulong_num
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("relay_log_pos", "relay_log_pos"));
      tree_item_list_add(p, new_simple_tree_item("eq", "="));
      tree_item_list_add(p, $3);
      $$= new_tree_item("master_file_def", "", p);
    }
  ;

/* create a table */

create:
	  CREATE opt_table_options TABLE_SYM opt_if_not_exists table_ident create2
    {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }      
      tree_item_list_add(p, new_simple_tree_item("table", "table"));
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      tree_item_list_add(p, $5);
      tree_item_list_add_all(p, $6);
      delete_tree_item($6);
      $$= new_tree_item("create", "create", p);
    }
	| CREATE opt_unique_or_fulltext INDEX_SYM ident key_alg ON table_ident LP_SYM key_list RP_SYM
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, new_simple_tree_item("index", "index"));
      tree_item_list_add(p, $4);
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      tree_item_list_add(p, new_simple_tree_item("on", "on"));
      tree_item_list_add(p, $7);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $9);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("create", "create", p);
    }
	| CREATE DATABASE opt_if_not_exists ident opt_create_database_options
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("database", "database"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      $$= new_tree_item("create", "create", p);
    }
	| CREATE udf_func_type FUNCTION_SYM sp_name create_function_tail
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("function", "function"));
      tree_item_list_add(p, $4);
      tree_item_list_add(p, $5);
      $$= new_tree_item("create", "create", p);
    }
	| CREATE PROCEDURE sp_name LP_SYM sp_pdparam_list RP_SYM sp_c_chistics sp_proc_stmt
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $5);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      if($7 != NULL)
      {
        tree_item_list_add(p, $7);
      }
      tree_item_list_add(p, $8);
      $$= new_tree_item("create", "create", p);
    }
	| CREATE or_replace algorithm view_user view_suid VIEW_SYM table_ident opt_view_list AS select_view_init check_option
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      tree_item_list_add(p, new_simple_tree_item("view", "view"));
      tree_item_list_add(p, $7);
      if($8 != NULL)
      {
        tree_item_list_add(p, $8);
      }
      tree_item_list_add(p, new_simple_tree_item("as", "as"));
      tree_item_list_add(p, $10);
      if($11 != NULL)
      {
        tree_item_list_add(p, $11);
      }
      $$= new_tree_item("create", "create", p);
    }
  | CREATE TRIGGER_SYM sp_name trg_action_time trg_event ON table_ident FOR_SYM EACH_SYM ROW_SYM sp_proc_stmt
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("trigger", "trigger"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, $4);
      tree_item_list_add(p, $5);
      tree_item_list_add(p, new_simple_tree_item("on", "on"));
      tree_item_list_add(p, $7);
      tree_item_list_add(p, new_simple_tree_item("for", "for"));
      tree_item_list_add(p, new_simple_tree_item("each", "each"));
      tree_item_list_add(p, new_simple_tree_item("row", "row"));
      tree_item_list_add(p, $11);
      $$= new_tree_item("create", "create", p);
    }
	| CREATE USER clear_privileges grant_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("user", "user"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("create", "create", p);
    }
	;

clear_privileges:
    /* Nothing */
    {
    }
  ;

sp_name:
	  ident DOT_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item(".", "."));
      tree_item_list_add(p, $3);
      $$= new_tree_item("sp_name", "", p);
    }
	| ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_name", "", p);
    }
	;

create_function_tail:
	  RETURNS_SYM udf_type UDF_SONAME_SYM TEXT_STRING_sys
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("returns", "returns"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("soname", "soname"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("create_function_tail", "", p);
    }
	| LP_SYM sp_fdparam_list RP_SYM RETURNS_SYM type sp_c_chistics sp_proc_stmt
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      tree_item_list_add(p, new_simple_tree_item("returns", "returns"));
      tree_item_list_add(p, $5);
      tree_item_list_add(p, $6);
      tree_item_list_add(p, $7);
      $$= new_tree_item("create_function_tail", "", p);
    }
	;

sp_a_chistics:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| sp_a_chistics sp_chistic 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	      delete_tree_item($1);
	    }
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_a_chistics", "", p);
	  }
	;

sp_c_chistics:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| sp_c_chistics sp_c_chistic 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add_all(p, $1);
	      delete_tree_item($1);
	    }
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_c_chistics", "", p);
	  }
	;

/* Characteristics for both create and alter */
sp_chistic:
	  COMMENT_SYM TEXT_STRING_sys 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("comment", "comment"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| LANGUAGE_SYM SQL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("language", "language"));
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| NO_SYM SQL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("no", "no"));
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| CONTAINS_SYM SQL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("contains", "contains"));
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| READS_SYM SQL_SYM DATA_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("reads", "reads"));
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("data", "data"));
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| MODIFIES_SYM SQL_SYM DATA_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("modifies", "modifies"));
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("data", "data"));
	    $$= new_tree_item("sp_chistic", "", p);
	  }
	| sp_suid 
	  {
	  }
	;

/* Create characteristics */
sp_c_chistic:
	  sp_chistic            
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    $$= new_tree_item("sp_c_chistic", "", p);
	  }
	| DETERMINISTIC_SYM     
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("deterministic", "deterministic"));
	    $$= new_tree_item("sp_c_chistic", "", p);
	  }
	| not DETERMINISTIC_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("deterministic", "deterministic"));
	    $$= new_tree_item("sp_c_chistic", "", p);
	  }
	;

sp_suid:
	  SQL_SYM SECURITY_SYM DEFINER_SYM 
	  {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("security", "security"));
	    tree_item_list_add(p, new_simple_tree_item("definer", "definer"));
	    $$= new_tree_item("sp_suid", "", p);
	  }
	| SQL_SYM SECURITY_SYM INVOKER_SYM 
	  {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("security", "security"));
	    tree_item_list_add(p, new_simple_tree_item("invoker", "invoker"));
	    $$= new_tree_item("sp_suid", "", p);
	  }
	;

call:
	  CALL_SYM sp_name LP_SYM sp_cparam_list RP_SYM 
	  {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("call", "", p);
	  }
	;

/* CALL parameters */
sp_cparam_list:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| sp_cparams
	  {
	    $$= $1;
	  }
	;

sp_cparams:
	  sp_cparams COMMA_SYM expr 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("sp_cparams", "", p);
	  }
	| expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_cparams", "", p);
	  }
	;

/* Stored FUNCTION parameter declaration list */
sp_fdparam_list:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| sp_fdparams
	  {
	    $$= $1;
	  }
	;

sp_fdparams:
	  sp_fdparams COMMA_SYM sp_fdparam
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("sp_fdparams", "", p);
	  }
	| sp_fdparam
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_fdparams", "", p);
	  }
	;

sp_fdparam:
	  ident type 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_fdparam", "", p);
	  }
	;

/* Stored PROCEDURE parameter declaration list */
sp_pdparam_list:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| sp_pdparams
	  {
	    $$= $1;
	  }
	;

sp_pdparams:
	  sp_pdparams COMMA_SYM sp_pdparam
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("sp_pdparams", "", p);
	  }
	| sp_pdparam
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_pdparams", "", p);
	  }
	;

sp_pdparam:
	  sp_opt_inout ident type 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("sp_pdparam", "", p);
	  }
	;

sp_opt_inout:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| IN_SYM      
	  {
	    $$= new_simple_tree_item("in", "in");
	  }
	| OUT_SYM     
	  {
	    $$= new_simple_tree_item("out", "out");
	  }
	| INOUT_SYM   
	  {
	    $$= new_simple_tree_item("inout", "inout");
	  }
	;

sp_proc_stmts:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| sp_proc_stmts sp_proc_stmt SEMICOL_SYM
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add_all(p, $1);
	      delete_tree_item($1);
	    }
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("semicol", ";"));
	    $$= new_tree_item("sp_proc_stmts", "", p);
	  }
	;

sp_proc_stmts1:
	  sp_proc_stmt SEMICOL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(";", ";"));
	    $$= new_tree_item("sp_proc_stmts1", "", p);
	  }
	| sp_proc_stmts1 sp_proc_stmt SEMICOL_SYM
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item(";", ";"));
	    $$= new_tree_item("sp_proc_stmts1", "", p);
	  }
	;

sp_decls:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| sp_decls sp_decl SEMICOL_SYM
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add_all(p, $1);
	      delete_tree_item($1);
	    }
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("semicol", ";"));
	    $$= new_tree_item("sp_decls", "", p);
	  }
	;

sp_decl:
    DECLARE_SYM sp_decl_idents type sp_opt_default
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("declare", "declare"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("sp_decl", "", p);
    }
	| DECLARE_SYM ident CONDITION_SYM FOR_SYM sp_cond
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("declare", "declare"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("condition", "condition"));
      tree_item_list_add(p, new_simple_tree_item("for", "for"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("sp_decl", "", p);
    }
	| DECLARE_SYM sp_handler_type HANDLER_SYM FOR_SYM sp_hcond_list sp_proc_stmt
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("declare", "declare"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("handler", "handler"));
      tree_item_list_add(p, new_simple_tree_item("for", "for"));
      tree_item_list_add(p, $5);
      tree_item_list_add(p, $6);
      $$= new_tree_item("sp_decl", "", p);
    }
	| DECLARE_SYM ident CURSOR_SYM FOR_SYM sp_cursor_stmt
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("declare", "declare"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("cursor", "cursor"));
      tree_item_list_add(p, new_simple_tree_item("for", "for"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("sp_decl", "", p);
    }
	;

sp_cursor_stmt:
	  statement
	  {
      $$= $1;
    }
	;

sp_handler_type:
	  EXIT_SYM      
	  {
	    $$= new_simple_tree_item("exit", "exit");
	  }
	| CONTINUE_SYM  
	  {
	    $$= new_simple_tree_item("continue", "continue");
	  }
/*	| UNDO_SYM      {} */
	;

sp_hcond_list:
	  sp_hcond
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_hcond_list", "", p);
    }
	| sp_hcond_list COMMA_SYM sp_hcond
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("sp_hcond_list", "", p);
    }
	;

sp_cond:
	  ulong_num
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_cond", "", p);
    }
	| SQLSTATE_SYM opt_value TEXT_STRING_literal
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("sqlstate", "sqlstate"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, $3);
      $$= new_tree_item("sp_cond", "", p);
    }
	;

opt_value:
	  /* Empty */  
	  {
	    $$= NULL;
	  }
	| VALUE_SYM    
	  {
	    $$= new_simple_tree_item("value", "value");
	  }
	;

sp_hcond:
	  sp_cond
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_hcond", "", p);
    }
	| ident			/* CONDITION name */
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_hcond", "", p);
    }
	| SQLWARNING_SYM	/* SQLSTATEs 01??? */
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("sqlwarning", "sqlwarning"));
      $$= new_tree_item("sp_hcond", "", p);
    }
	| not FOUND_SYM		/* SQLSTATEs 02??? */
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("found", "found"));
      $$= new_tree_item("sp_hcond", "", p);
    }
	| SQLEXCEPTION_SYM	/* All other SQLSTATEs */
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("sqlexception", "sqlexception"));
      $$= new_tree_item("sp_hcond", "", p);
    }
	;

sp_decl_idents:
	  ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_decl_idents", "", p);
    }
	| sp_decl_idents COMMA_SYM ident
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("sp_decl_idents", "", p);
    }
	;

sp_opt_default:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
  | DEFAULT expr 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("default", "default"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("sp_opt_default", "", p);
    }
	;

sp_proc_stmt:
	  statement
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
  | RETURN_SYM expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("return", "return"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| IF sp_if END IF
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("if", "if"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("end", "end"));
	    tree_item_list_add(p, new_simple_tree_item("if", "if"));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| CASE_SYM WHEN_SYM sp_case END CASE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("case", "case"));
	    tree_item_list_add(p, new_simple_tree_item("when", "when"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("end", "end"));
	    tree_item_list_add(p, new_simple_tree_item("case", "case"));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  } 
  | CASE_SYM expr WHEN_SYM sp_case END CASE_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("case", "case"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("when", "when"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("end", "end"));
	    tree_item_list_add(p, new_simple_tree_item("case", "case"));
	    $$= new_tree_item("sp_proc_stmt", "", p);
    }
	| sp_labeled_control
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| sp_unlabeled_control
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| LEAVE_SYM label_ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("leave", "leave"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| ITERATE_SYM label_ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("iterate", "iterate"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| LABEL_SYM IDENT
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("label", "label"));
	    tree_item_list_add(p, new_simple_tree_item("ident", $2));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| GOTO_SYM IDENT
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("goto", "goto"));
	    tree_item_list_add(p, new_simple_tree_item("ident", $2));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| OPEN_SYM ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("open", "open"));
	    tree_item_list_add(p, new_simple_tree_item("ident", $2));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| FETCH_SYM sp_opt_fetch_noise ident INTO sp_fetch_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("fetch", "fetch"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("into", "into"));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	| CLOSE_SYM ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("close", "close"));
	    tree_item_list_add(p, new_simple_tree_item("ident", $2));
	    $$= new_tree_item("sp_proc_stmt", "", p);
	  }
	;

sp_opt_fetch_noise:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| NEXT_SYM FROM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("next", "next"));
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    $$= new_tree_item("sp_opt_fetch_noise", "", p);
	  }
	| FROM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    $$= new_tree_item("sp_opt_fetch_noise", "", p);
	  }
	;

sp_fetch_list:
	  ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("sp_fetch_list", "", p);
    }
	| sp_fetch_list COMMA_SYM ident
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("sp_fetch_list", "", p);
    }
	;

sp_if:
    expr THEN_SYM sp_proc_stmts1 sp_elseifs
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("then", "then"));
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("sp_if", "", p);
    }
	;

sp_elseifs:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| ELSEIF_SYM sp_if
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("elseif", "elseif"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_if", "", p);
	  }
	| ELSE sp_proc_stmts1
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("else", "else"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_if", "", p);
	  }
	;

sp_case:
    expr THEN_SYM sp_proc_stmts1 sp_whens
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("then", "then"));
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("sp_case", "", p);
    }
	;

sp_whens:
	  /* Empty */
	  {
      $$= NULL;
    }
	| ELSE sp_proc_stmts1 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("else", "else"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_whens", "", p);
	  }
	| WHEN_SYM sp_case 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("when", "when"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("sp_whens", "", p);
	  }
	;

sp_labeled_control:
	  label_ident COL_SYM sp_unlabeled_control sp_opt_label
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("column", ":"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("sp_labeled_control", "", p);
	  }
	;

sp_opt_label:
      /* Empty  */  
      {
        $$= NULL;
      }
    | label_ident   
      {
        $$= $1;
      }
	;

sp_unlabeled_control:
	  BEGIN_SYM sp_decls sp_proc_stmts END
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("begin", "begin"));
      tree_item_list_add(p, $2);
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, new_simple_tree_item("end", "end"));
      $$= new_tree_item("sp_unlabeled_control", "", p);
    }
	| LOOP_SYM sp_proc_stmts1 END LOOP_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("loop", "loop"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("end", "end"));
      tree_item_list_add(p, new_simple_tree_item("loop", "loop"));
      $$= new_tree_item("sp_unlabeled_control", "", p);
    }
  | WHILE_SYM expr DO_SYM sp_proc_stmts1 END WHILE_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("while", "while"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("do", "do"));
      tree_item_list_add(p, $4);
      tree_item_list_add(p, new_simple_tree_item("end", "end"));
      tree_item_list_add(p, new_simple_tree_item("while", "while"));
      $$= new_tree_item("sp_unlabeled_control", "", p);
    }
  | REPEAT_SYM sp_proc_stmts1 UNTIL_SYM expr END REPEAT_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("repeat", "repeat"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("until", "until"));
      tree_item_list_add(p, $4);
      tree_item_list_add(p, new_simple_tree_item("end", "end"));
      tree_item_list_add(p, new_simple_tree_item("repeat", "repeat"));
      $$= new_tree_item("sp_unlabeled_control", "", p);
    }
	;

trg_action_time:
    BEFORE_SYM 
    {
      $$= new_simple_tree_item("before", "before");
    }
  | AFTER_SYM 
    {
      $$= new_simple_tree_item("after", "after");
    }
  ;

trg_event:
    INSERT 
    {
      $$= new_simple_tree_item("insert", "insert");
    }
  | UPDATE_SYM
    {
      $$= new_simple_tree_item("update", "update");
    }
  | DELETE_SYM
    {
      $$= new_simple_tree_item("delete", "delete");
    }
  ;

create2:
    LP_SYM create2a 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add_all(p, $2);
      delete_tree_item($2);
      $$= new_tree_item("create2", "", p);
    }
  | opt_create_table_options create3 
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add_all(p, $2);
      delete_tree_item($2);
      $$= new_tree_item("create2", "", p);
    }
  | LIKE table_ident 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("like", "like"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("create2", "", p);
    }
  | LP_SYM LIKE table_ident RP_SYM
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, new_simple_tree_item("like", "like"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("create2", "", p);
    }
  ;

create2a:
    field_list RP_SYM opt_create_table_options create3 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add_all(p, $4);
        delete_tree_item($4);
      }
      $$= new_tree_item("create2a", "", p);
    }
	| create_select RP_SYM union_opt 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      $$= new_tree_item("create2a", "", p);
	  }
  ;

create3:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| opt_duplicate opt_as create_select union_clause 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("create3", "", p);
	  }
	| opt_duplicate opt_as LP_SYM create_select RP_SYM union_opt 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($6 != NULL)
	    {
	      tree_item_list_add(p, $6);
	    }
	    $$= new_tree_item("create3", "", p);
	  }
  ;

create_select:
    SELECT_SYM select_options select_item_list opt_select_from
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("select", "select"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, $3);
      tree_item_list_add(p, $4);
      $$= new_tree_item("create_select", "", p);
    }
  ;

opt_as:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| AS
	  {
	    $$= new_simple_tree_item("as", "as");
	  }
	;

opt_create_database_options:
	  /* empty */			
	  {
	    $$= NULL;
	  }
	| create_database_options	
	  {
	    $$= $1;
	  }
	;

create_database_options:
	  create_database_option					
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("create_database_options", "", p);
	  }
	| create_database_options create_database_option	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("create_database_options", "", p);
	  }
	;

create_database_option:
	  default_collation   
	  {
	    $$= $1;
	  }
	| default_charset   
	  {
	    $$= $1;
	  }
	;

opt_table_options:
	  /* empty */	 
	  {
	    $$= NULL;
	  }
	| table_options  
	  {
	    $$= $1;
	  };

table_options:
	  table_option	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("table_options", "", p);
	  }
	  /* in original 5.x it was 'table_option table_options' but this should probably cause conflicts if more than one option? */
	| table_options table_option
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("table_options", "", p);
	  }
	;

table_option:
	  TEMPORARY	
	  {
	    $$= new_simple_tree_item("temporary", "temporary");
	  }
	;

opt_if_not_exists:
	  /* empty */	 
	  {
	    $$= NULL;
	  }
	| IF not EXISTS	 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("if", "if"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("exists", "exists"));
	    $$= new_tree_item("opt_if_not_exists", "", p);
	  }
	;

opt_create_table_options:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| create_table_options
	  {
	    $$= $1;
	  }
	;

create_table_options_space_separated:
	  create_table_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("create_table_options_space_separated", "", p);
	  }
	| create_table_option create_table_options_space_separated
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("create_table_options_space_separated", "", p);
	  }
	;

create_table_options:
	  create_table_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("create_table_options", "", p);
	  }
	| create_table_option create_table_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("create_table_options", "", p);
	  }
	| create_table_option COMMA_SYM create_table_options
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3);
	    $$= new_tree_item("create_table_options", "", p);
    }
	;

create_table_option:
	  ENGINE_SYM opt_equal storage_engines    
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("engine", "engine"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| TYPE_SYM opt_equal storage_engines
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("type", "type"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| MAX_ROWS opt_equal ulonglong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("max_rows", "max_rows"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| MIN_ROWS opt_equal ulonglong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("min_rows", "min_rows"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| AVG_ROW_LENGTH opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("avg_row_length", "avg_row_length"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| PASSWORD opt_equal TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("password", "password"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| COMMENT_SYM opt_equal TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("comment", "comment"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| AUTO_INC opt_equal ulonglong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("auto_increment", "auto_increment"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
  | PACK_KEYS_SYM opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("pack_keys", "pack_keys"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
  | PACK_KEYS_SYM opt_equal DEFAULT
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("pack_keys", "pack_keys"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    $$= new_tree_item("create_table_option", "", p);
	  }
  
	| CHECKSUM_SYM opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("checksum", "checksum"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| DELAY_KEY_WRITE_SYM opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("delay_key_write", "delay_key_write"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| ROW_FORMAT_SYM opt_equal row_types
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("row_format", "row_format"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| RAID_TYPE opt_equal raid_types
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("raid_type", "raid_type"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| RAID_CHUNKS opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("raid_chunks", "raid_chunks"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| RAID_CHUNKSIZE opt_equal ulong_num
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("raid_chunksize", "raid_chunksize"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| UNION_SYM opt_equal LP_SYM table_list RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("union", "union"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| default_charset
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| default_collation
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| INSERT_METHOD opt_equal merge_insert_types
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("insert_method", "insert_method"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| DATA_SYM DIRECTORY_SYM opt_equal TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("data", "data"));
	    tree_item_list_add(p, new_simple_tree_item("directory", "directory"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| INDEX_SYM DIRECTORY_SYM opt_equal TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("index", "index"));
	    tree_item_list_add(p, new_simple_tree_item("directory", "directory"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("create_table_option", "", p);
	  }
	| CONNECTION_SYM opt_equal TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("connection", "connection"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("create_table_option", "", p);
	  }
  ;

default_charset:
    opt_default charset opt_equal charset_name_or_default
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      $$= new_tree_item("default_charset", "", p);
    }
  ;

default_collation:
    opt_default COLLATE_SYM opt_equal collation_name_or_default
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, new_simple_tree_item("collate", "collate"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      $$= new_tree_item("default_collation", "", p);
    }
  ;

storage_engines:
	  ident_or_text 
	  {
	    $$= $1;
	  }
	;

row_types:
	  DEFAULT 
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	| FIXED_SYM
	  {
	    $$= new_simple_tree_item("fixed", "fixed");
	  }
	| DYNAMIC_SYM
	  {
	    $$= new_simple_tree_item("dynamic", "dynamic");
	  }
	| COMPRESSED_SYM
	  {
	    $$= new_simple_tree_item("compressed", "compressed");
	  }
	| REDUNDANT_SYM
	  {
	    $$= new_simple_tree_item("redundant", "redundant");
	  }
	| COMPACT_SYM
	  {
	    $$= new_simple_tree_item("compact", "compact");
	  }
	;

raid_types:
	  RAID_STRIPED_SYM 
	  {
	    $$= new_simple_tree_item("stripped", "stripped");
	  }
	| RAID_0_SYM	 
	  {
	    $$= new_simple_tree_item("raid0", "raid0");
	  }
	| ulong_num	 
	  {
	    $$= $1;
	  }
	;

merge_insert_types:
    NO_SYM        
    {
      $$= new_simple_tree_item("no", "no");
    }
  | FIRST_SYM
    {
      $$= new_simple_tree_item("first", "first");
    }
  | LAST_SYM
    {
      $$= new_simple_tree_item("last", "last");
    }
  ;

opt_select_from:
	  opt_limit_clause 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("opt_select_from", "", p);
	  }
	| select_from select_lock_type
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_select_from", "", p);
	  }
	;

udf_func_type:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| AGGREGATE_SYM 
	  {
	    $$= new_simple_tree_item("aggregate", "aggregate");
	  }
	;

udf_type:
	  STRING_SYM 
	  {
	    $$= new_simple_tree_item("string", "string");
	  }
	| REAL 
	  {
	    $$= new_simple_tree_item("real", "real");
	  }
  | DECIMAL_SYM 
    {
      $$= new_simple_tree_item("decimal", "decimal");
    }
	| INT_SYM 
	  {
	    $$= new_simple_tree_item("int", "int");
	  }
	;

field_list:
	  field_list_item
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("field_list", "", p);
	  }
	| field_list COMMA_SYM field_list_item
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("field_list", "", p);
	  }
	;

field_list_item:
	  column_def
	  {
	    $$= $1;
	  }
  | key_def
    {
      $$= $1;
    }
  ;

column_def:
	  field_spec opt_check_constraint
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("column_def", "", p);
	  }
	| field_spec references 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("column_def", "", p);
	  }
	;

key_def:
	  key_type opt_ident key_alg LP_SYM key_list RP_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $5);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("key_def", "", p);
    }
	| opt_constraint constraint_key_type opt_ident key_alg LP_SYM key_list RP_SYM
	  {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);  
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $6);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("key_def", "", p);
    }
	| opt_constraint FOREIGN KEY_SYM opt_ident LP_SYM key_list RP_SYM references
	  {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, new_simple_tree_item("foreign", "foreign"));
      tree_item_list_add(p, new_simple_tree_item("key", "key"));
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $6);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      tree_item_list_add(p, $8);
      $$= new_tree_item("key_def", "", p);
    }
	| constraint opt_check_constraint
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("key_def", "", p);
    }
	| opt_constraint check_constraint
	  {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);
      $$= new_tree_item("key_def", "", p);
    }
	;

opt_check_constraint:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| check_constraint
	  {
	    $$= $1;
	  }
	;

check_constraint:
	  CHECK_SYM expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("check", "check"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("check_constraint", "", p);
	  }
	;

opt_constraint:
	  /* empty */		
	  {
	    $$= NULL;
	  }
	| constraint		
	  {
	    $$= $1;
	  }
	;

constraint:
	  CONSTRAINT opt_ident	
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("constraint", "constraint"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("constraint", "", p);	  
	  }
	;

field_spec:
	  field_ident type opt_attribute
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("field_spec", "", p);
    }
  ;

type:
	  int_type opt_len field_options	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| real_type opt_precision field_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| FLOAT_SYM float_options field_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("float", "float"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| BIT_SYM			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bit", "bit"));
	    $$= new_tree_item("type", "", p);
	  }
	| BIT_SYM LP_SYM NUM RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bit", "bit"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("type", "", p);
	  }
	| BOOL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bool", "bool"));
	    $$= new_tree_item("type", "", p);
	  }
	| BOOLEAN_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("boolean", "boolean"));
	    $$= new_tree_item("type", "", p);
	  }
	| char LP_SYM NUM RP_SYM opt_binary	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
	    $$= new_tree_item("type", "", p);
	  }
	| char opt_binary		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
	    $$= new_tree_item("type", "", p);
	  }
	| nchar LP_SYM NUM RP_SYM opt_bin_mod	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| nchar opt_bin_mod	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| BINARY LP_SYM NUM RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("binary", "binary"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("type", "", p);
	  }
	| BINARY 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("binary", "binary"));
	    $$= new_tree_item("type", "", p);
	  }
	| varchar LP_SYM NUM RP_SYM opt_binary 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| nvarchar LP_SYM NUM RP_SYM opt_bin_mod 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| VARBINARY LP_SYM NUM RP_SYM 	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("varbinary", "varbinary"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("type", "", p);
	  }
	| YEAR_SYM opt_len field_options 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("year", "year"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| DATE_SYM			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("date", "date"));
	    $$= new_tree_item("type", "", p);
	  }
	| TIME_SYM			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("time", "time"));
	    $$= new_tree_item("type", "", p);
	  }
	| TIMESTAMP opt_len 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("timestamp", "timestamp"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| DATETIME			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("datetime", "datetime"));
	    $$= new_tree_item("type", "", p);
	  }
	| TINYBLOB
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("tinyblob", "tinyblob"));
	    $$= new_tree_item("type", "", p);
	  }
	| BLOB_SYM opt_len		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("blob", "blob"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| spatial_type 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("type", "", p);
	  }
	| MEDIUMBLOB 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("mediumblob", "mediumblob"));
	    $$= new_tree_item("type", "", p);
	  }
	| LONGBLOB	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("longblob", "longblob"));
	    $$= new_tree_item("type", "", p);
	  }
	| LONG_SYM VARBINARY		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("long", "long"));
	    tree_item_list_add(p, new_simple_tree_item("varbinary", "varbinary"));
	    $$= new_tree_item("type", "", p);
	  }
	| LONG_SYM varchar opt_binary	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("long", "long"));
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| TINYTEXT opt_binary		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("tinytext", "tinytext"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| TEXT_SYM opt_len opt_binary	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("text", "text"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| MEDIUMTEXT opt_binary		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("mediumtext", "mediumtext"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| LONGTEXT opt_binary		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("longtext", "longtext"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| DECIMAL_SYM float_options field_options 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("decimal", "decimal"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| NUMERIC_SYM float_options field_options 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("numeric", "numeric"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| FIXED_SYM float_options field_options 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("fixed", "fixed"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| ENUM LP_SYM string_list RP_SYM opt_binary 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("enum", "enum"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| SET LP_SYM string_list RP_SYM opt_binary 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| LONG_SYM opt_binary	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("long", "long"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("type", "", p);
	  }
	| SERIAL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("serial", "serial"));
	    $$= new_tree_item("type", "", p);
	  }
	;

spatial_type:
	  GEOMETRY_SYM	      
	  {
	    $$= new_simple_tree_item("geometry", "geometry");
	  }
	| GEOMETRYCOLLECTION  
	  {
	    $$= new_simple_tree_item("geometrycollection", "geometrycollection");
	  }
	| POINT_SYM           
	  {
	    $$= new_simple_tree_item("point", "point");
	  }
	| MULTIPOINT          
	  {
	    $$= new_simple_tree_item("multipoint", "multipoint");
	  }
	| LINESTRING          
	  {
	    $$= new_simple_tree_item("linestring", "linestring");
	  }
	| MULTILINESTRING     
	  {
	    $$= new_simple_tree_item("multilinestring", "multilinestring");
	  }
	| POLYGON             
	  {
	    $$= new_simple_tree_item("polygon", "polygon");
	  }
	| MULTIPOLYGON        
	  {
	    $$= new_simple_tree_item("multipolygon", "multipolygon");
	  }
	;

char:
	  CHAR_SYM 
	  {
	    $$= new_simple_tree_item("char", $1);
	  }
	;

nchar:
	  NCHAR_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("nchar", "nchar"));
	    $$= new_tree_item("nchar", "", p);
	  }
	| NATIONAL_SYM CHAR_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("national", "national"));
	    tree_item_list_add(p, new_simple_tree_item("char", "char"));
	    $$= new_tree_item("nchar", "", p);
	  }
	;

varchar:
	  char VARYING 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("varying", "varying"));
	    $$= new_tree_item("varchar", "", p);
	  }
	| VARCHAR 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("varchar", "varchar"));
	    $$= new_tree_item("varchar", "", p);
	  }
	;

nvarchar:
	  NATIONAL_SYM VARCHAR
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("national", "national"));
	    tree_item_list_add(p, new_simple_tree_item("varchar", "varchar"));
	    $$= new_tree_item("nvarchar", "", p);
	  }
	| NVARCHAR_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("nvarchar", "nvarchar"));
	    $$= new_tree_item("nvarchar", "", p);
	  }
	| NCHAR_SYM VARCHAR 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("nchar", "nchar"));
	    tree_item_list_add(p, new_simple_tree_item("varchar", "varchar"));
	    $$= new_tree_item("nvarchar", "", p);
	  }
	| NATIONAL_SYM CHAR_SYM VARYING 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("national", "national"));
	    tree_item_list_add(p, new_simple_tree_item("char", "char"));
	    tree_item_list_add(p, new_simple_tree_item("varying", "varying"));
	    $$= new_tree_item("nvarchar", "", p);
	  }
	| NCHAR_SYM VARYING 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("nchar", "nchar"));
	    tree_item_list_add(p, new_simple_tree_item("varying", "varying"));
	    $$= new_tree_item("nvarchar", "", p);
	  }
	;

int_type:
	  INT_SYM
	  {
	    $$= new_simple_tree_item("int", "int");
	  }
	| TINYINT
	  {
	    $$= new_simple_tree_item("tinyint", "tinyint");
	  }
	| SMALLINT	
	  {
	    $$= new_simple_tree_item("smallint", "smallint");
	  }
	| MEDIUMINT
	  {
	    $$= new_simple_tree_item("mediumint", "mediumint");
	  }
	| BIGINT
	  {
	    $$= new_simple_tree_item("bigint", "bigint");	  
	  }
	;

real_type:
	  REAL		
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("real", "real"));
      $$= new_tree_item("real_type", "", p);
    }
	| DOUBLE_SYM	
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("double", "double"));
      $$= new_tree_item("real_type", "", p);
	  }
	| DOUBLE_SYM PRECISION 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("double", "double"));
      tree_item_list_add(p, new_simple_tree_item("precision", "precision"));
      $$= new_tree_item("real_type", "", p);
	  }
	;


float_options:
    /* empty */		
    {
      $$= NULL;
    }
  | LP_SYM NUM RP_SYM		
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, new_simple_tree_item("num", $2));
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("float_options", "", p);
    }
	| precision		
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      $$= new_tree_item("float_options", "", p);
	  }
	;

precision:
	  LP_SYM NUM COMMA_SYM NUM RP_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, new_simple_tree_item("num", $2));
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, new_simple_tree_item("num", $4));
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("precision", "", p);
    }
  ;

field_options:
	  /* empty */		
	  {
	    $$= NULL;
	  }
	| field_opt_list	
	  {
	    $$= $1;
	  }
	;

field_opt_list:
	  field_opt_list field_option 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("field_opt_list", "", p);
	  }
	| field_option 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("field_opt_list", "", p);
	  }
	;

field_option:
	  SIGNED_SYM	
	  {
	    $$= new_simple_tree_item("signed", "signed");
	  }
	| UNSIGNED	
	  {
	    $$= new_simple_tree_item("unsigned", "unsigned");
	  }
	| ZEROFILL	
	  {
	    $$= new_simple_tree_item("zerofill", "zerofill");
	  }
	;

opt_len:
	  /* empty */	
	  /* use default length */
	  {
	    $$= NULL;
	  } 
	| LP_SYM NUM RP_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $2));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("opt_len", "", p);
	  }
	;

opt_precision:
	/* empty */	
	{
	  $$= NULL;
	}
| precision	
  {
    $$= $1;
  }
;

opt_attribute:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| opt_attribute_list 
	  {
	    $$= $1;
	  }
	;

opt_attribute_list:
	  opt_attribute_list attribute 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_attribute_list", "", p);
	  }
	| attribute
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("opt_attribute_list", "", p);
	  }
	;

attribute:
	  NULL_SYM	  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("null", "null"));
	    $$= new_tree_item("attribute", "", p);
	  }
	| not NULL_SYM	  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("null", "null"));
	    $$= new_tree_item("attribute", "", p);
	  }
	| DEFAULT now_or_signed_literal 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("attribute", "", p);
	  }
	| ON UPDATE_SYM NOW_SYM optional_braces 
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("update", "update"));
	    tree_item_list_add(p, new_simple_tree_item("now", "now"));
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("attribute", "", p);
    }
	| AUTO_INC
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("auto_increment", "auto_increment"));
	    $$= new_tree_item("attribute", "", p);
	  }
	| SERIAL_SYM DEFAULT VALUE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("serial", "serial"));
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    tree_item_list_add(p, new_simple_tree_item("value", "value"));
	    $$= new_tree_item("attribute", "", p);
    }
	| opt_primary KEY_SYM 
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL) {
	      tree_item_list_add_all(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("key", "key"));
	    $$= new_tree_item("attribute", "", p);
    }
	| UNIQUE_SYM	  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("unique", "unique"));
	    $$= new_tree_item("attribute", "", p);
    }
	| UNIQUE_SYM KEY_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("unique", "unique"));
	    tree_item_list_add(p, new_simple_tree_item("key", "key"));
	    $$= new_tree_item("attribute", "", p);
    }
	| COMMENT_SYM TEXT_STRING_sys 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("comment", "comment"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("attribute", "", p);
	  }
	| COLLATE_SYM collation_name
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("collate", "collate"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("attribute", "", p);
    }
	;

now_or_signed_literal:
    NOW_SYM optional_braces 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("now", "now"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("now_or_signed_literal", "", p);
    }
  | signed_literal 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("now_or_signed_literal", "", p);
    }
  ;

charset:
	  CHAR_SYM SET	
	  {
	    void *p = new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("character", "character"));
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    $$= new_tree_item("charset", "charset", p);
	  }
	| CHARSET	
	  {
	    void *p = new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("charset", "charset"));
	    $$= new_tree_item("charset", "charset", p);
	  }
	;

charset_name:
	  ident_or_text
	  {
      $$= $1;
    }
	| BINARY 
	  {
	    $$= new_simple_tree_item("binary", "binary");
	  }
	;

charset_name_or_default:
	  charset_name 
	  {
	    $$= $1;
	  }
	| DEFAULT 
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	;


old_or_new_charset_name:
	  ident_or_text 
	  {
	    $$= $1;
	  }
	| BINARY 
	  {
	    $$= new_simple_tree_item("binary", "binary");
	  }
	;

old_or_new_charset_name_or_default:
	  old_or_new_charset_name 
	  {
	    $$= $1;
	  }
	| DEFAULT    
	  {
	    $$= new_simple_tree_item("default", "default");
	  } 
	;

collation_name:
	  ident_or_text 
	  {
	    $$= $1;
	  }
  ;

opt_collate:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| COLLATE_SYM collation_name_or_default 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("collate", "collate"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_collate", "", p);
	  }
	;

collation_name_or_default:
	  collation_name 
	  {
	    $$= $1;
	  }
	| DEFAULT
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	;

opt_default:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| DEFAULT	
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	;

opt_binary:
	  /* empty */			
	  {
	    $$= NULL;
	  }
	| ASCII_SYM opt_bin_mod		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("ascii", "ascii"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("opt_binary", "", p);
	  }
	| BYTE_SYM			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("byte", "byte"));
	    $$= new_tree_item("opt_binary", "", p);
	  }
	| UNICODE_SYM opt_bin_mod
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("unicode", "unicode"));
      if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }	    
	    $$= new_tree_item("opt_binary", "", p);
    } 
	| charset charset_name opt_bin_mod	
	  {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }	    
	    $$= new_tree_item("opt_binary", "", p);
	  }
  | BINARY opt_bin_charset 
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("binary", "binary"));
      if($2 != NULL)
	    {
	      tree_item_list_add_all(p, $2);
	      delete_tree_item($2);
	    }	    
	    $$= new_tree_item("opt_binary", "", p);
    }
  ;

opt_bin_mod:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| BINARY 
	  {
	    $$= new_simple_tree_item("binary", "binary");
	  }
	;

opt_bin_charset:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| ASCII_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("ascii", "ascii"));
	    $$= new_tree_item("opt_bin_charset", "", p);
	  }
	| UNICODE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("unicode", "unicode"));
	    $$= new_tree_item("opt_bin_charset", "", p);
    }
	| charset charset_name	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_bin_charset", "", p);
	  }
  ;

opt_primary:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| PRIMARY_SYM
	  {
	    $$= new_simple_tree_item("primary", "primary");
	  }
	;

references:
	  REFERENCES table_ident opt_ref_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("references", "references"));
      tree_item_list_add(p, $2);
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      $$= new_tree_item("references", "", p);
    }
  ;

opt_ref_list:
	  /* empty */ opt_on_delete 
	  {
	    $$= $1; 
	  }
	| LP_SYM ref_list RP_SYM opt_on_delete 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("opt_ref_list", "", p);
	  }
	;

ref_list:
	ref_list COMMA_SYM ident	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("ref_list", "", p);
	  }
	| ident			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("ref_list", "", p);
	  }
	;

opt_on_delete:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| opt_on_delete_list 
	  {
	    $$= $1;
	  }
	;

opt_on_delete_list:
	  opt_on_delete_list opt_on_delete_item 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_on_delete_list", "", p);
	  }
	| opt_on_delete_item 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("opt_on_delete_list", "", p);
	  }
	;

opt_on_delete_item:
	  ON DELETE_SYM delete_option   
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("delete", "delete"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("opt_on_delete_item", "", p);
	  }
	| ON UPDATE_SYM delete_option 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("update", "update"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("opt_on_delete_item", "", p);
	  }
	| MATCH FULL	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("match", "match"));
	    tree_item_list_add(p, new_simple_tree_item("full", "full"));
	    $$= new_tree_item("opt_on_delete_item", "", p);
	  }
	| MATCH PARTIAL 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("match", "match"));
	    tree_item_list_add(p, new_simple_tree_item("partial", "partial"));
	    $$= new_tree_item("opt_on_delete_item", "", p);
	  }
	| MATCH SIMPLE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("match", "match"));
	    tree_item_list_add(p, new_simple_tree_item("simple", "simple"));
	    $$= new_tree_item("opt_on_delete_item", "", p);
	  }
	;

delete_option:
	  RESTRICT	 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("restrict", "restrict"));
	    $$= new_tree_item("delete_option", "", p);
	  }
	| CASCADE	 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("cascade", "cascade"));
	    $$= new_tree_item("delete_option", "", p);
	  }
	| SET NULL_SYM   
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    tree_item_list_add(p, new_simple_tree_item("null", "null"));
	    $$= new_tree_item("delete_option", "", p);
	  }
	| NO_SYM ACTION  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("no", "no"));
	    tree_item_list_add(p, new_simple_tree_item("action", "action"));
	    $$= new_tree_item("delete_option", "", p);
	  }
	| SET DEFAULT    
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    $$= new_tree_item("delete_option", "", p);
	  }
	;

key_type:
	  key_or_index			    
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("key_type", "", p);
	  }
	| FULLTEXT_SYM opt_key_or_index	    
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("fulltext", "fulltext"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("key_type", "", p);
	  }
	| SPATIAL_SYM opt_key_or_index
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("spatial", "spatial"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("key_type", "", p);
	  }
	;

constraint_key_type:
	  PRIMARY_SYM KEY_SYM  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("primary", "primary"));
	    tree_item_list_add(p, new_simple_tree_item("key", "key"));
	    $$= new_tree_item("constraint_key_type", "", p);
	  }
	| UNIQUE_SYM opt_key_or_index 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("unique", "unique"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("constraint_key_type", "", p);
	  }
	;

key_or_index:
	  KEY_SYM 
	  {
	    $$= new_simple_tree_item("key", "key");
	  }
	| INDEX_SYM 
	  {
	    $$= new_simple_tree_item("index", "index");
	  }
	;

opt_key_or_index:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| key_or_index
	  {
	    $$= $1;
	  }
	;

keys_or_index:
	  KEYS 
	  {
	    $$= new_simple_tree_item("keys", "keys");
	  }
	| INDEX_SYM 
	  {
	    $$= new_simple_tree_item("index", "index");
	  }
	| INDEXES 
	  {
	    $$= new_simple_tree_item("indexes", "indexes");
	  }
	;

opt_unique_or_fulltext:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| UNIQUE_SYM	
	  {
	    $$= new_simple_tree_item("unique", "unique");
	  }
	| FULLTEXT_SYM	
	  {
	    $$= new_simple_tree_item("fulltext", "fulltext");
	  }
	| SPATIAL_SYM
	  {
	    $$= new_simple_tree_item("spatial", "spatial");
    }
  ;

key_alg:
	  /* empty */		   
	  {
	    $$= NULL;
	  }
	| USING opt_btree_or_rtree 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("using", "using"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("key_alg", "", p);
	  }
	| TYPE_SYM opt_btree_or_rtree  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("type", "type"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("key_alg", "", p);
	  }
	;

opt_btree_or_rtree:
	  BTREE_SYM	
	  {
	    $$= new_simple_tree_item("btree", "btree");
	  }
	| RTREE_SYM
	  {
	    $$= new_simple_tree_item("rtree", "rtree");
    }
	| HASH_SYM	
	  {
	    $$= new_simple_tree_item("hash", "hash");
	  }
	;

key_list:
	  key_list COMMA_SYM key_part order_dir 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("key_list", "", p);
	  }
	| key_part order_dir		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("key_list", "", p);
	  }
	;

key_part:
	  ident			
	  {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("key_part", "", p);
	  }
	| ident LP_SYM NUM RP_SYM	
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("num", $3));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("key_part", "", p);
    }
  ;

opt_ident:
	  /* empty */
	  /* Defaultlength */
    {
      $$= NULL;
    }	
	| field_ident	
	  {
	    $$= $1;
	  }
	;

opt_component:
  /* empty */      {}
  | '.' ident      {};

string_list:
	  text_string			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("string_list", "", p);
	  }
	| string_list COMMA_SYM text_string	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("string_list", "", p);
	  }
	;

/*
** Alter table
*/

alter:
	  ALTER opt_ignore TABLE_SYM table_ident alter_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("table", "table"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("alter", "", p);
	  }
	| ALTER DATABASE ident_or_empty opt_create_database_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("database", "database"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("alter", "", p);
    }
	| ALTER PROCEDURE sp_name sp_a_chistics
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("alter", "", p);
    }
	| ALTER FUNCTION_SYM sp_name sp_a_chistics
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("function", "function"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("alter", "", p);
    }
	| ALTER algorithm view_user view_suid VIEW_SYM table_ident opt_view_list AS select_view_init check_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    tree_item_list_add(p, new_simple_tree_item("view", "view"));
	    tree_item_list_add(p, $6);
	    if($7 != NULL)
	    {
	      tree_item_list_add(p, $7);
	    }
	    tree_item_list_add(p, new_simple_tree_item("as", "as"));
	    tree_item_list_add(p, $9);
	    if($10 != NULL)
	    {
	      tree_item_list_add(p, $10);
	    }
	    $$= new_tree_item("alter", "", p);
	  }
	;

ident_or_empty:
	  /* empty */  
	  {
	    $$= NULL;
	  }
	| ident      
	  {
	    $$= $1;
	  }
	;

alter_list:
	| DISCARD TABLESPACE 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("discard", "discard"));
	    tree_item_list_add(p, new_simple_tree_item("tablespace", "tablespace"));
	    $$= new_tree_item("alter_list", "", p);
	  }
	| IMPORT TABLESPACE 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("import", "import"));
	    tree_item_list_add(p, new_simple_tree_item("tablespace", "tablespace"));
	    $$= new_tree_item("alter_list", "", p);
	  }
  | alter_list_item 
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("alter_list", "", p);
    }
	| alter_list COMMA_SYM alter_list_item 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("alter_list", "", p);
	  }
	;

add_column:
	  ADD opt_column
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("add", "add"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
  ;

alter_list_item:
	  add_column column_def opt_place
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
	  }
	| ADD key_def
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("add", "add"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| add_column LP_SYM field_list RP_SYM
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| CHANGE opt_column field_ident field_spec opt_place
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("change", "change"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
  | MODIFY_SYM opt_column field_ident type opt_attribute opt_place
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("modify", "modify"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| DROP opt_column field_ident opt_restrict
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| DROP FOREIGN KEY_SYM opt_ident
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    tree_item_list_add(p, new_simple_tree_item("foreign", "foreign"));
	    tree_item_list_add(p, new_simple_tree_item("key", "key"));
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| DROP PRIMARY_SYM KEY_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    tree_item_list_add(p, new_simple_tree_item("primary", "primary"));
	    tree_item_list_add(p, new_simple_tree_item("key", "key"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| DROP key_or_index field_ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| DISABLE_SYM KEYS
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("disable", "disable"));
	    tree_item_list_add(p, new_simple_tree_item("keys", "keys"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| ENABLE_SYM KEYS
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("enable", "enable"));
	    tree_item_list_add(p, new_simple_tree_item("keys", "keys"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| ALTER opt_column field_ident SET DEFAULT signed_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("alter", "alter"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    tree_item_list_add(p, $6);
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| ALTER opt_column field_ident DROP DEFAULT
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("alter", "alter"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| RENAME opt_to table_ident
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| CONVERT_SYM TO_SYM charset charset_name_or_default opt_collate
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("convert", "convert"));
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("alter_list_item", "", p);
    }
  | create_table_options_space_separated
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| FORCE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("force", "force"));
	    $$= new_tree_item("alter_list_item", "", p);
    }
	| order_clause
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("alter_list_item", "", p);
    }
  ;

opt_column:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| COLUMN_SYM	
	  {
	    $$= new_simple_tree_item("column", "column");
	  }
	;

opt_ignore:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| IGNORE_SYM	
	  {
	    $$= new_simple_tree_item("ignore", "ignore");
	  }
	;

opt_restrict:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| RESTRICT	
	  {
	    $$= new_simple_tree_item("restrict", "restrict");
	  }
	| CASCADE	
	  {
	    $$= new_simple_tree_item("cascade", "cascade");
	  }
	;

opt_place:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| AFTER_SYM ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("after", "after"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_place", "", p);
	  }
	| FIRST_SYM	  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("first", "first"));
	    $$= new_tree_item("opt_place", "", p);
	  }
	;

opt_to:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| TO_SYM	
	  {
	    $$= new_simple_tree_item("to", "to");
	  }
	| EQ
	  {
	    $$= new_simple_tree_item("eq", "eq");
	  }
	| AS
	  {
	    $$= new_simple_tree_item("as", "as");
	  }
	;

/*
  SLAVE START and SLAVE STOP are deprecated. We keep them for compatibility.
*/

slave:
	  START_SYM SLAVE slave_thread_opts slave_until
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("start", "start"));
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("slave", "", p);
    }
  | STOP_SYM SLAVE slave_thread_opts
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("stop", "stop"));
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("slave", "", p);
    }
	| SLAVE START_SYM slave_thread_opts slave_until
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, new_simple_tree_item("start", "start"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("slave", "", p);
    }
	| SLAVE STOP_SYM slave_thread_opts
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, new_simple_tree_item("stop", "stop"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("slave", "", p);
    }
  ;

start:
	  START_SYM TRANSACTION_SYM start_transaction_opts
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("start", "start"));
	    tree_item_list_add(p, new_simple_tree_item("transaction", "transaction"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("start", "", p);
    }
	;

start_transaction_opts:
    /*empty*/ 
    {
      $$= NULL;
    }
  | WITH CONSISTENT_SYM SNAPSHOT_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("consistent", "consistent"));
	    tree_item_list_add(p, new_simple_tree_item("snapshot", "snapshot"));
	    $$= new_tree_item("start_transaction_opts", "", p);
    }
    ;

slave_thread_opts:
	  slave_thread_opt_list
    {
      $$= $1;
    }
	;

slave_thread_opt_list:
	  slave_thread_opt
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    } 
	    $$= new_tree_item("slave_thread_opt_list", "", p);
	  }
	| slave_thread_opt_list COMMA_SYM slave_thread_opt
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("slave_thread_opt_list", "", p);
	  }
	;

slave_thread_opt:
	  /*empty*/	
	  {
	    $$= NULL;
	  }
	| SQL_THREAD	
	  {
	    $$= new_simple_tree_item("sql_thread", "sql_thread");
	  }
	| RELAY_THREAD 	
	  {
	    $$= new_simple_tree_item("relay_thread", "relay_thread");
	  }
	;

slave_until:
	  /*empty*/	
	  {
	    $$= NULL;
	  }
	| UNTIL_SYM slave_until_opts
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("until", "until"));
    }
	;

slave_until_opts:
    master_file_def
    {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
	    $$= new_tree_item("slave_thread_opt_list", "", p);
    }
  | slave_until_opts COMMA_SYM master_file_def 
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
	    $$= new_tree_item("slave_until_opts", "", p);
    }
  ;

restore:
	  RESTORE_SYM table_or_tables table_list FROM TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("from", "from"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("restore", "", p);
    }
  ;

backup:
	  BACKUP_SYM table_or_tables table_list TO_SYM TEXT_STRING_sys
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("to", "to"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("backup", "", p);
    }
  ;

checksum:
    CHECKSUM_SYM table_or_tables table_list opt_checksum_type
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("checksum", "", p);
    }
	;

opt_checksum_type:
    /* nothing */  
    {
      $$= NULL;
    }
	| QUICK        
	  {
	    $$= new_simple_tree_item("quick", "quick");
	  }
	| EXTENDED_SYM 
	  {
	    $$= new_simple_tree_item("extended", "extended");
	  }
  ;

repair:
	  REPAIR opt_no_write_to_binlog table_or_tables table_list opt_mi_repair_type
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("repair", "", p);
	  }
	;

opt_mi_repair_type:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| mi_repair_types 
	  {
	    $$= $1;
	  }
	;

mi_repair_types:
	  mi_repair_type 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("mi_repair_types", "", p);
	  }
	| mi_repair_type mi_repair_types 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("mi_repair_types", "", p);
	  }
	;

mi_repair_type:
	  QUICK          
	  {
	    $$= new_simple_tree_item("quick", "quick");
	  }
	| EXTENDED_SYM
	  {
	    $$= new_simple_tree_item("extended", "extended");
	  }
  | USE_FRM      
    {
      $$= new_simple_tree_item("use_frm", "use_frm");
    }
  ;

analyze:
	  ANALYZE_SYM opt_no_write_to_binlog table_or_tables table_list opt_mi_check_type
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("analyze", "", p);
	  }
	;

check:
	  CHECK_SYM table_or_tables table_list opt_mi_check_type
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("check", "", p);
	  }
	;

opt_mi_check_type:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| mi_check_types 
	  {
	    $$= $1;
	  }
	;

mi_check_types:
	  mi_check_type 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("mi_check_types", "", p);
	  }
	| mi_check_type mi_check_types 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("mi_check_types", "", p);
	  }
	;

mi_check_type:
	  QUICK      
	  {
	    $$= new_simple_tree_item("quick", "quick");
	  }
	| FAST_SYM 
	  {
	    $$= new_simple_tree_item("fast", "fast");
	  }
	| MEDIUM_SYM 
	  {
	    $$= new_simple_tree_item("medium", "medium");
	  }
	| EXTENDED_SYM 
	  {
	    $$= new_simple_tree_item("extended", "extended");
	  }
	| CHANGED  
	  {
	    $$= new_simple_tree_item("changed", "changed");
	  }
	;

optimize:
	  OPTIMIZE opt_no_write_to_binlog table_or_tables table_list opt_mi_check_type
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, $5);
	  }
	;

opt_no_write_to_binlog:
	  /* empty */        
	  {
	    $$= NULL;
	  }
	| NO_WRITE_TO_BINLOG  
	  {
	    $$= new_simple_tree_item("no_write_to_binlog", "no_write_to_binlog");
	  }
	| LOCAL_SYM  
	  {
	    $$= new_simple_tree_item("local", "local");
	  }
	;

rename:
	  RENAME table_or_tables table_to_table_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("rename", "", p);
	  }
	| RENAME USER clear_privileges rename_list
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("user", "user"));
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("rename", "", p);
    }
	;

rename_list:
    user TO_SYM user
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("rename_list", "", p);
    }
  | rename_list COMMA_SYM user TO_SYM user
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("rename_list", "", p);
    }
  ;

table_to_table_list:
	  table_to_table
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("table_to_table_list", "", p);
	  }
	| table_to_table_list COMMA_SYM table_to_table
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("table_to_table_list", "", p);
	  }
	;

table_to_table:
	  table_ident TO_SYM table_ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("to", "to"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("table_to_table_list", "", p);
    }
  ;

keycache:
    CACHE_SYM INDEX_SYM keycache_list IN_SYM key_cache_name
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("index", "index"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("in", "in"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("keycache", "", p);
    }
  ;

keycache_list:
    assign_to_keycache
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("keycache_list", "", p);
    }
  | keycache_list COMMA_SYM assign_to_keycache
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("keycache_list", "", p);
    }
  ;

assign_to_keycache:
    table_ident cache_keys_spec
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      $$= new_tree_item("assign_to_keycache", "", p);
    }
  ;

key_cache_name:
	  ident	   
	  {
	    $$= $1;
	  }
	| DEFAULT  
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	;

preload:
  	LOAD INDEX_SYM INTO CACHE_SYM preload_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("index", "index"));
      tree_item_list_add(p, new_simple_tree_item("into", "into"));
      tree_item_list_add(p, new_simple_tree_item("cache", "cache"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("preload", "", p);
	  }
	;

preload_list:
	  preload_keys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("preload_list", "", p);
	  }
	| preload_list COMMA_SYM preload_keys
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("preload_list", "", p);
    }
	;

preload_keys:
	  table_ident cache_keys_spec opt_ignore_leaves
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("preload_keys", "", p);
    }
	;

cache_keys_spec:
    cache_key_list_or_empty
    {
      $$= $1;
    }
  ;

cache_key_list_or_empty:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| opt_key_or_index LP_SYM key_usage_list2 RP_SYM
	  {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("cache_key_list_or_empty", "", p);
    }
	;

opt_ignore_leaves:
	/* empty */
	  {
	    $$= NULL;
	  }
	| IGNORE_SYM LEAVES 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("ignore", "ignore"));
      tree_item_list_add(p, new_simple_tree_item("leaves", "leaves"));
      $$= new_tree_item("opt_ignore_leaves", "", p);
	  }
	;

/*
  Select : retrieve data from table
*/

select:
	select_init { $$= $1; }
	;

/* Need select_init2 for subselects. */
select_init:
	  SELECT_SYM select_init2 
	  { 
	    // ignore  statement name
	    void *p= new_tree_item_list_reuse($2); 
	    delete_tree_item($2);
	    $$= new_tree_item("select", "", p); 
	  }
	| '(' select_paren ')' union_opt
	;

select_view_init:
    SELECT_SYM remember_name select_init2 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("select", "select"));
      tree_item_list_add_all(p, $3);
      delete_tree_item($3);
      $$= new_tree_item("select_view_init", "", p);
    }
  | LP_SYM remember_name select_paren RP_SYM union_opt 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add_all(p, $3);
      delete_tree_item($3);      
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      $$= new_tree_item("select_view_init", "", p);
    }
  ;

select_paren:
	  SELECT_SYM select_part2 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("select", "select"));
      tree_item_list_add_all(p, $2);
      delete_tree_item($2);
      $$= new_tree_item("select_paren", "", p);
	  }
	| LP_SYM select_paren RP_SYM
	  {
	    void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("select_paren", "", p);
	  }
	;

select_init2:
	  select_part2 union_clause 
	  { 
	    void *p= new_tree_item_list_reuse($1); 
	    delete_tree_item($1);
      tree_item_list_add(p, $2);
	    $$= new_tree_item("select_init2", "", p); 
	  }
	;

select_part2:
	  select_options select_item_list select_into select_lock_type 
	  { 
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, $2);
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3); 
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("select_part2", "", p); 
	  }
	;

select_into:
	  opt_order_clause opt_limit_clause 
	  {
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $1); 
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("select_into", "", p); 
	  }
  | into
	  {
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $1); 
	    $$= new_tree_item("select_into", "", p); 
	  }
	| select_from 
	  { 
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $1); 
	    $$= new_tree_item("select_into", "", p); 
	  }
	| into select_from
	  {
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $1); 
	    tree_item_list_add(p, $2); 
	    $$= new_tree_item("select_into", "", p); 
	  }
	| select_from into
	  {
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $1); 
	    tree_item_list_add(p, $2); 
	    $$= new_tree_item("select_into", "", p); 
	  }
	; 

select_from:
	  FROM join_table_list where_clause group_clause having_clause opt_order_clause opt_limit_clause procedure_clause 
	  { 
	    void* p= new_tree_item_list(); 
	    tree_item_list_add(p, $2); /* join_table_list */
	    tree_item_list_add(p, $3); /* where_clause */
	    tree_item_list_add(p, $4); /* group_clause */
	    tree_item_list_add(p, $5); /* having_clause */
	    tree_item_list_add(p, $6); /* opt_order_clause */
	    tree_item_list_add(p, $7); /* opt_limit_clause */
	    $$= new_tree_item("select_from", "", p);  
	  }
  | FROM DUAL_SYM where_clause opt_limit_clause
          /* oracle compatibility: oracle always requires FROM clause,
             and DUAL is system table without fields.
             Is "SELECT 1 FROM DUAL" any better than "SELECT 1" ?
          Hmmm :) */
	;

select_options:
	  /* empty*/
	  {
	    $$= NULL;
	  }
	| select_option_list 
	  {
	    $$= $1;
	  }
  ;

select_option_list:
	  select_option_list select_option
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("select_option_list", "", p);
	  }
	| select_option
	  {
	    $$= new_simple_tree_item("select_option_list", "");
	  }
	;

select_option:
	  STRAIGHT_JOIN { $$= new_simple_tree_item("straight_join", "straight_join"); }
	| HIGH_PRIORITY { $$= new_simple_tree_item("high_priority", "high_priority"); }
	| DISTINCT      { $$= new_simple_tree_item("distinct", "distinct"); }
	| SQL_SMALL_RESULT { $$= new_simple_tree_item("sql_small_result", "sql_small_result"); }
	| SQL_BIG_RESULT { $$= new_simple_tree_item("sql_big_result", "sql_big_result"); }
	| SQL_BUFFER_RESULT { $$= new_simple_tree_item("sql_buffer_result", "sql_buffer_result"); }
	| SQL_CALC_FOUND_ROWS { $$= new_simple_tree_item("sql_calc_found_rows", "sql_calc_found_rows"); }
	| SQL_NO_CACHE_SYM { $$= new_simple_tree_item("sql_no_cache", "sql_no_cache"); }
	| SQL_CACHE_SYM { $$= new_simple_tree_item("sql_cache", "sql_cache"); }
	| ALL           { $$= new_simple_tree_item("all", "all"); }
	;

select_lock_type:
	  /* empty */
	  {
	    $$= new_simple_tree_item("select_lock_type", "");
	  }
	| FOR_SYM UPDATE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("for", "for"));
	    tree_item_list_add(p, new_simple_tree_item("update", "update"));
	    $$= new_tree_item("select_lock_type", "", p);
	  }
	| LOCK_SYM IN_SYM SHARE_SYM MODE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lock", "lock"));
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, new_simple_tree_item("share", "share"));
	    tree_item_list_add(p, new_simple_tree_item("mode", "mode"));
	    $$= new_tree_item("select_lock_type", "", p);
	  }
	;

select_item_list:
	  select_item_list COMMA_SYM select_item
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("comma", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("select_item_list", "", p);
	  }
	| select_item 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("select_item_list", "", p);
	  }
	| MULL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("select_item_list", "", p);
	  }
  ;

select_item:
	  remember_name select_item2 remember_end select_alias
	  {
      void *p= $2;
      $$= $2;
    }
  ;

remember_name:
	  {}
	;

remember_end:
	  {}
	;

select_item2:
	  table_wild  /* table.* or schema.table.* */
	  {
	    $$= $1;
	  }
	| expr
	  {
	    $$= $1;
	  }
	;

select_alias:
	  /* empty */		
	  {
	    $$= NULL;
	  }
	| AS ident		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("as", "as"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("select_alias", "", p);
	  }
	| AS TEXT_STRING_sys	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("as", "as"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("select_alias", "", p);
	  }
	| ident			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("select_alias", "", p);
	  }
	| TEXT_STRING_sys	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("select_alias", "", p);
	  }
	;

optional_braces:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| LP_SYM RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("optional_braces", "", p);
	  }
	;

/* all possible expressions */
expr:	
	  bool_term bool_or_expr 
	  { 
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("expr", "", p);
	  }
	;

bool_or_expr:
	  /* empty */
	  {
	    $$= NULL;
	  }
  | bool_or_expr or bool_term 
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bool_or_expr", "", p);
    }
  ;

bool_term:
	  bool_term XOR bool_term 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("xor", $2));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bool_or_expr", "", p);
	  }
	| bool_factor bool_and_expr 
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("bool_term", "", p);
	  }
	;

bool_and_expr:
	  /* empty */
	  {
	    $$= NULL;
	  }
  | bool_and_expr and bool_factor 
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bool_and_expr", "", p);
    }
  ;

bool_factor:
	  NOT_SYM bool_factor	
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("not", $1));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("bool_factor", "", p);
	  }
	| bool_test
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bool_factor", "", p);
	  }
	;

bool_test:
	  bool_pri IS TRUE_SYM	
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, new_simple_tree_item("true", $3));
	    $$= new_tree_item("bool_test", "", p);
	  }
	| bool_pri IS not TRUE_SYM
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("true", $4));
	    $$= new_tree_item("bool_test", "", p);
	  }
	| bool_pri IS FALSE_SYM
    {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, new_simple_tree_item("false", $3));
	    $$= new_tree_item("bool_test", "", p);
    }
	| bool_pri IS not FALSE_SYM
    {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("false", $4));
	    $$= new_tree_item("bool_test", "", p);
    }
	| bool_pri IS UNKNOWN_SYM
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, new_simple_tree_item("unknown", $3));
	    $$= new_tree_item("bool_test", "", p);
	  }
	| bool_pri IS not UNKNOWN_SYM
    {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("unknown", $4));
	    $$= new_tree_item("bool_test", "", p);
    }
	| bool_pri
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bool_test", "", p);
	  } 
	;

bool_pri:
	  bool_pri IS NULL_SYM
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, new_simple_tree_item("null", $3));
	    $$= new_tree_item("bool_pri", "", p);
	  }
	| bool_pri IS not NULL_SYM {}
    {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("is", $2));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("null", $4));
	    $$= new_tree_item("bool_pri", "", p);
    }
	| bool_pri EQUAL_SYM predicate	
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("equal", $2));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bool_pri", "", p);
	  }
	| bool_pri comp_op predicate %prec EQ {}
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("bool_pri", "", p);
	  }
	| bool_pri comp_op all_or_any in_subselect %prec EQ 
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("bool_pri", "", p);
	  }
	| predicate
	  {
  	  void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bool_pri", "", p);
	  } 
	;

predicate:
	  bit_expr IN_SYM LP_SYM expr_list RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr not IN_SYM LP_SYM expr_list RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $5);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("predicate", "", p);
	  }
  | bit_expr IN_SYM in_subselect 
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("predicate", "", p);
    }
	| bit_expr not IN_SYM in_subselect 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr BETWEEN_SYM bit_expr AND_SYM predicate 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("between", "between"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr not BETWEEN_SYM bit_expr AND_SYM predicate 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("between", "between"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, $6);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr SOUNDS_SYM LIKE bit_expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("sounds", "sounds"));
	    tree_item_list_add(p, new_simple_tree_item("like", "like"));	    
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr LIKE simple_expr opt_escape 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("like", "like"));	    
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr not LIKE simple_expr opt_escape 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("like", "like"));	    
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr REGEXP bit_expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("regexprlike", "regexprlike"));	    
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr not REGEXP bit_expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("regexprlike", "regexprlike"));	    
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("predicate", "", p);
	  }
	| bit_expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("predicate", "", p);
	  }
	;

bit_expr:
	  bit_expr VERT_LINE_SYM bit_term	{}
	| bit_term
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bit_expr", "", p);
	  } 
	;

bit_term:
	  bit_term BITAND_SYM bit_factor	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("bitand", "&"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bit_term", "", p);
	  }
	| bit_factor
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bit_term", "", p);
	  } 
	;

bit_factor:
	  bit_factor SHL_SYM value_expr
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("shift_left", "<<"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bit_factor", "", p);
	  }
	| bit_factor SHR_SYM value_expr 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("shift_right", ">>"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("bit_factor", "", p);
	  }
	| value_expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("bit_factor", "", p);
	  } 
	;

value_expr:
	  value_expr PLUS_SYM term	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("plus", "+"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("value_expr", "", p);
	  }
	| value_expr MINUS_SYM term	
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("minus", "-"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("value_expr", "", p);
	  }
	| value_expr PLUS_SYM interval_expr interval
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("plus", "+"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("value_expr", "", p);
	  }
	| value_expr MINUS_SYM interval_expr interval
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("minus", "-"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("value_expr", "", p);
	  }
	| term
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("value_expr", "", p);
	  }
	;

term:
	  term MULL_SYM factor 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("mull", "*"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("term", "", p);
	    
	  }
	| term DIV_DIV_SYM factor
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("div", "/"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("term", "", p);
	  }
	| term PERCENT_SYM factor
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("mod", "%"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("term", "", p);
	  }
	| term DIV_SYM factor
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("intdiv", "div"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("term", "", p);
	  }
	| term MOD_SYM factor
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("mod", "mod"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("term", "", p);
	  }
	| factor
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("term", "", p);
	  } 
	;

factor:
    factor UP_SYM simple_expr	
    {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("xor", "^"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("factor", "", p);
    }
	| simple_expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("factor", "", p);
	  }
	;

or:	
    OR_SYM { $$= new_simple_tree_item("or", $1); } 
  | OR2_SYM { $$= new_simple_tree_item("or", $1); } 
  ;
  
and:	
    AND_SYM { $$= new_simple_tree_item("and", $1); } 
  | AND_AND_SYM { $$= new_simple_tree_item("and", $1); }
  ;
  
not:
    NOT_SYM { $$= new_simple_tree_item("not", $1); }
  | NOT2_SYM { $$= new_simple_tree_item("not", $1); }
  ;

not2:	
    NOT_EXCL_MARKER { $$= new_simple_tree_item("not2", $1); } 
  | NOT2_SYM { $$= new_simple_tree_item("not2", $1); } 
  ;

comp_op:  
    EQ { $$= new_simple_tree_item("comp_op", "eq"); }
	| GE { $$= new_simple_tree_item("comp_op", "ge"); }
	| GT_SYM { $$= new_simple_tree_item("comp_op", "gt"); }
	| LE { $$= new_simple_tree_item("comp_op", "le"); }
	| LT { $$= new_simple_tree_item("comp_op", "lt"); }
	| NE { $$= new_simple_tree_item("comp_op", "ne"); }
	;

all_or_any: 
    ALL { $$= new_simple_tree_item("all", $1); }
  | ANY_SYM { $$= new_simple_tree_item("any", $1); }
  ;

interval_expr:
    INTERVAL_SYM expr 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("interval", $1));
      tree_item_list_add(p, $2);
      $$= new_tree_item("interval_expr", "", p);
    }
  ;

simple_expr:
	  simple_ident
	  {
	    $$= $1;
	  }
 	| simple_expr COLLATE_SYM ident_or_text %prec NEG {}
	| literal
	  {
	    $$= $1;
	  }
	| param_marker
	  {
	    $$= $1;
	  }
	| '@' ident_or_text SET_VAR expr {}
	| '@' ident_or_text {}
	| '@' '@' opt_var_ident_type ident_or_text opt_component {}
	| sum_expr
	  {
	    $$= $1;
	  }
	| simple_expr OR_OR_SYM simple_expr {}
	| '+' simple_expr %prec NEG	{}
	| '-' simple_expr %prec NEG	{}
	| '~' simple_expr %prec NEG	{}
	| not2 simple_expr %prec NEG	{}
	| LP_SYM expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("simple_expr", "", p);
	  }
	| LP_SYM expr COMMA_SYM expr_list RP_SYM 
	  {
	  
	  }
	| ROW_SYM LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| EXISTS exists_subselect {}
	| singlerow_subselect   {}
	| '{' ident expr '}'	{}
  | MATCH ident_list_arg AGAINST LP_SYM bit_expr fulltext_options RP_SYM {}
	| ASCII_SYM LP_SYM expr RP_SYM {}
	| BINARY simple_expr %prec NEG {}
	| CAST_SYM LP_SYM expr AS cast_type RP_SYM {}
	| CASE_SYM opt_expr WHEN_SYM when_list opt_else END {}
	| CONVERT_SYM LP_SYM expr COMMA_SYM cast_type RP_SYM {}
	| CONVERT_SYM LP_SYM expr USING charset_name RP_SYM {}
	| DEFAULT LP_SYM simple_ident RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("default", "default"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("simple_expr", "", p);
	  }
	| VALUES LP_SYM simple_ident RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("values", "values"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("simple_expr", "", p);
	  }
	| FUNC_ARG0 LP_SYM RP_SYM {}
	| FUNC_ARG1 LP_SYM expr RP_SYM {}
	| FUNC_ARG2 LP_SYM expr COMMA_SYM expr RP_SYM {}
	| FUNC_ARG3 LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| ADDDATE_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| ADDDATE_SYM LP_SYM expr COMMA_SYM INTERVAL_SYM expr interval RP_SYM {}
	| REPEAT_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| ATAN	LP_SYM expr RP_SYM {}
	| ATAN	LP_SYM expr COMMA_SYM expr RP_SYM {}
	| CHAR_SYM LP_SYM expr_list RP_SYM {}
	| CHAR_SYM LP_SYM expr_list USING charset_name RP_SYM {}
	| CHARSET LP_SYM expr RP_SYM {}
	| COALESCE LP_SYM expr_list RP_SYM {}
	| COLLATION_SYM LP_SYM expr RP_SYM {}
	| CONCAT LP_SYM expr_list RP_SYM {}
	| CONCAT_WS LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| CONVERT_TZ_SYM LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| CURDATE optional_braces {}
	| CURTIME optional_braces {}
	| CURTIME LP_SYM expr RP_SYM {}
	| CURRENT_USER optional_braces {}
	| DATE_ADD_INTERVAL LP_SYM expr COMMA_SYM interval_expr interval RP_SYM {}
	| DATE_SUB_INTERVAL LP_SYM expr COMMA_SYM interval_expr interval RP_SYM {}
	| DATABASE LP_SYM RP_SYM {}
	| DATE_SYM LP_SYM expr RP_SYM {}
	| DAY_SYM LP_SYM expr RP_SYM {}
	| ELT_FUNC LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| MAKE_SET_SYM LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| ENCRYPT LP_SYM expr RP_SYM {}
	| ENCRYPT LP_SYM expr COMMA_SYM expr RP_SYM   {}
	| DECODE_SYM LP_SYM expr COMMA_SYM TEXT_STRING_literal RP_SYM {}
	| ENCODE_SYM LP_SYM expr COMMA_SYM TEXT_STRING_literal RP_SYM {}
	| DES_DECRYPT_SYM LP_SYM expr RP_SYM {}
	| DES_DECRYPT_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| DES_ENCRYPT_SYM LP_SYM expr RP_SYM {}
	| DES_ENCRYPT_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| EXPORT_SET LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| EXPORT_SET LP_SYM expr COMMA_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| EXPORT_SET LP_SYM expr COMMA_SYM expr COMMA_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| FORMAT_SYM LP_SYM expr COMMA_SYM NUM RP_SYM {}
	| FROM_UNIXTIME LP_SYM expr RP_SYM {}
	| FROM_UNIXTIME LP_SYM expr COMMA_SYM expr RP_SYM {}
	| FIELD_FUNC LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| geometry_function {}
	| GET_FORMAT LP_SYM date_time_type  COMMA_SYM expr RP_SYM {}
	| HOUR_SYM LP_SYM expr RP_SYM {}
	| IF LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| INSERT LP_SYM expr COMMA_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| interval_expr interval '+' expr /* we cannot put interval before - */ {}
	| interval_expr {}
	| LAST_INSERT_ID LP_SYM RP_SYM {}
	| LAST_INSERT_ID LP_SYM expr RP_SYM {}
	| LEFT LP_SYM expr COMMA_SYM expr RP_SYM {}
	| LOCATE LP_SYM expr COMMA_SYM expr RP_SYM {}
	| LOCATE LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| GREATEST_SYM LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| LEAST_SYM LP_SYM expr COMMA_SYM expr_list RP_SYM {}
	| LOG_SYM LP_SYM expr RP_SYM {}
	| LOG_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| MASTER_POS_WAIT LP_SYM expr COMMA_SYM expr RP_SYM {}
	| MASTER_POS_WAIT LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| MICROSECOND_SYM LP_SYM expr RP_SYM {}
	| MINUTE_SYM LP_SYM expr RP_SYM {}
	| MOD_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| MONTH_SYM LP_SYM expr RP_SYM {}
	| NOW_SYM optional_braces {}
	| NOW_SYM LP_SYM expr RP_SYM {}
	| PASSWORD LP_SYM expr RP_SYM {}
	| OLD_PASSWORD LP_SYM expr RP_SYM {}
	| POSITION_SYM LP_SYM bit_expr IN_SYM expr RP_SYM {}
	| QUARTER_SYM LP_SYM expr RP_SYM {}
	| RAND LP_SYM expr RP_SYM {}
	| RAND LP_SYM RP_SYM {}
	| REPLACE LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| RIGHT LP_SYM expr COMMA_SYM expr RP_SYM {}
	| ROUND LP_SYM expr RP_SYM {}
	| ROUND LP_SYM expr COMMA_SYM expr RP_SYM {}
	| ROW_COUNT_SYM LP_SYM RP_SYM {}
	| SUBDATE_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| SUBDATE_SYM LP_SYM expr COMMA_SYM INTERVAL_SYM expr interval RP_SYM {}
	| SECOND_SYM LP_SYM expr RP_SYM {}
	| SUBSTRING LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| SUBSTRING LP_SYM expr COMMA_SYM expr RP_SYM {}
	| SUBSTRING LP_SYM expr FROM expr FOR_SYM expr RP_SYM {}
	| SUBSTRING LP_SYM expr FROM expr RP_SYM {}
	| SUBSTRING_INDEX LP_SYM expr COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| SYSDATE optional_braces {}
	| SYSDATE LP_SYM expr RP_SYM {}
	| TIME_SYM LP_SYM expr RP_SYM {}
	| TIMESTAMP LP_SYM expr RP_SYM {}
	| TIMESTAMP LP_SYM expr COMMA_SYM expr RP_SYM {}
	| TIMESTAMP_ADD LP_SYM interval_time_st COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| TIMESTAMP_DIFF LP_SYM interval_time_st COMMA_SYM expr COMMA_SYM expr RP_SYM {}
	| TRIM LP_SYM expr RP_SYM {}
	| TRIM LP_SYM LEADING expr FROM expr RP_SYM {}
	| TRIM LP_SYM TRAILING expr FROM expr RP_SYM {}
	| TRIM LP_SYM BOTH expr FROM expr RP_SYM {}
	| TRIM LP_SYM LEADING FROM expr RP_SYM {}
	| TRIM LP_SYM TRAILING FROM expr RP_SYM {}
	| TRIM LP_SYM BOTH FROM expr RP_SYM {}
	| TRIM LP_SYM expr FROM expr RP_SYM {}
	| TRUNCATE_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| ident '.' ident LP_SYM udf_expr_list RP_SYM {}
	| IDENT_sys LP_SYM  {} udf_expr_list RP_SYM {}
	| UNIQUE_USERS LP_SYM text_literal COMMA_SYM NUM COMMA_SYM NUM COMMA_SYM expr_list RP_SYM {}
  | UNIX_TIMESTAMP LP_SYM RP_SYM {}
	| UNIX_TIMESTAMP LP_SYM expr RP_SYM {}
	| USER LP_SYM RP_SYM {}
	| UTC_DATE_SYM optional_braces {}
	| UTC_TIME_SYM optional_braces {}
	| UTC_TIMESTAMP_SYM optional_braces {}
	| WEEK_SYM LP_SYM expr RP_SYM {}
	| WEEK_SYM LP_SYM expr COMMA_SYM expr RP_SYM {}
	| YEAR_SYM LP_SYM expr RP_SYM {}
	| YEARWEEK LP_SYM expr RP_SYM {}
	| YEARWEEK LP_SYM expr COMMA_SYM expr RP_SYM {}
	| BENCHMARK_SYM LP_SYM ulong_num COMMA_SYM expr RP_SYM {}
	| EXTRACT_SYM LP_SYM interval FROM expr RP_SYM {}
	;

geometry_function:
	  CONTAINS_SYM '(' expr ',' expr ')' {}
	| GEOMFROMTEXT '(' expr ')' {}
	| GEOMFROMTEXT '(' expr ',' expr ')' {}
	| GEOMFROMWKB '(' expr ')' {}
	| GEOMFROMWKB '(' expr ',' expr ')' {}
	| GEOMETRYCOLLECTION '(' expr_list ')' {}
	| LINESTRING '(' expr_list ')' {}
 	| MULTILINESTRING '(' expr_list ')' {}
 	| MLINEFROMTEXT '(' expr ')' {}
	| MLINEFROMTEXT '(' expr ',' expr ')' {}
	| MPOINTFROMTEXT '(' expr ')' {}
	| MPOINTFROMTEXT '(' expr ',' expr ')' {}
	| MPOLYFROMTEXT '(' expr ')' {}
	| MPOLYFROMTEXT '(' expr ',' expr ')' {}
	| MULTIPOINT '(' expr_list ')' {}
 	| MULTIPOLYGON '(' expr_list ')' {}
	| POINT_SYM '(' expr ',' expr ')' {}
 	| POINTFROMTEXT '(' expr ')' {}
	| POINTFROMTEXT '(' expr ',' expr ')' {}
	| POLYFROMTEXT '(' expr ')' {}
	| POLYFROMTEXT '(' expr ',' expr ')' {}
	| POLYGON '(' expr_list ')' {}
 	| GEOMCOLLFROMTEXT '(' expr ')' {}
	| GEOMCOLLFROMTEXT '(' expr ',' expr ')' {}
 	| LINEFROMTEXT '(' expr ')' {}
	| LINEFROMTEXT '(' expr ',' expr ')' {}
	;

fulltext_options:
        /* nothing */                   {}
        | WITH QUERY_SYM EXPANSION_SYM  {}
        | IN_SYM BOOLEAN_SYM MODE_SYM   {}
        ;

udf_expr_list:
	  /* empty */	 
	  {
	    $$= NULL;
	  }
	| udf_expr_list2 
	  {
	    $$= $1;
	  }
	;

udf_expr_list2:
	  udf_expr_list3
	  {
	    $$= $1;
	  }
	;

udf_expr_list3:
	  udf_expr 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("udf_expr_list3", "", p);
    }
	| udf_expr_list3 COMMA_SYM udf_expr 
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("udf_expr_list3", "", p);
    }
	;

udf_expr:
	  remember_name expr remember_end select_alias 
	  {
	    void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $2);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("udf_expr", "", p);
	  }
	;

sum_expr:
	  AVG_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("avg", "avg"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| AVG_SYM LP_SYM DISTINCT in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("avg", "avg"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("distinct", "distinct"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| BITAND_SYM LP_SYM in_sum_expr RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bitand", "&"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| VERT_LINE_SYM  LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bitor", "|"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| UP_SYM  LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("bitxor", "^"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| COUNT_SYM LP_SYM opt_all MULL_SYM RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("count", "count"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| COUNT_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("count", "count"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| COUNT_SYM LP_SYM DISTINCT expr_list RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("count", "count"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("distinct", "distinct"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| GROUP_UNIQUE_USERS LP_SYM text_literal COMMA_SYM NUM COMMA_SYM NUM COMMA_SYM in_sum_expr RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("group_unique_users", "group_unique_users"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, new_simple_tree_item("num", $5)); // todo: make a subrule to match number only (now $5 == all string till the '\0')
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, new_simple_tree_item("num", $7)); // todo: as for $5
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, new_simple_tree_item("num", $9));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| MIN_SYM LP_SYM in_sum_expr RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("min", "min"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
/*
   According to ANSI SQL, DISTINCT is allowed and has
   no sence inside MIN and MAX grouping functions; so MIN|MAX(DISTINCT ...)
   is processed like an ordinary MIN | MAX()
 */
	| MIN_SYM LP_SYM DISTINCT in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("min", "min"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("distinct", "distinct"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| MAX_SYM LP_SYM in_sum_expr RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("max", "max"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| MAX_SYM LP_SYM DISTINCT in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("max", "max"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("distinct", "distinct"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| STD_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("std", $1)); // (std | stddev | stddev_pop)
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| VARIANCE_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("variance", $1)); // (variance | var_pop)
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| STDDEV_SAMP_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("stddev_samp", "stddev_samp"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| VAR_SAMP_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("var_samp", "var_samp"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| SUM_SYM LP_SYM in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("sum", "sum"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| SUM_SYM LP_SYM DISTINCT in_sum_expr RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("sum", "sum"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, new_simple_tree_item("distinct", "distinct"));	    
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
	| GROUP_CONCAT_SYM LP_SYM opt_distinct expr_list opt_gorder_clause opt_gconcat_separator RP_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("group_concat", "group_concat"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
	    tree_item_list_add(p, $4);
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      if($6 != NULL)
      {
        tree_item_list_add(p, $6);
      }
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("sum_expr", "", p);
	  }
  ;

opt_distinct:
      /* empty */ 
      {
        $$= NULL;
      }
    | DISTINCT   
      {
        $$= new_simple_tree_item("distinct", "distinct");
      }
    ;

opt_gconcat_separator:
      /* empty */
      {
        $$= NULL;
      }
    | SEPARATOR_SYM text_string  
      {
        void *p= new_tree_item_list();
        tree_item_list_add(p, new_simple_tree_item("separator", "separator"));
      }
    ;


opt_gorder_clause:
	  /* empty */
	  {
      $$= NULL;
    }
	| order_clause
    {
      $$= $1;
    }
  ;

in_sum_expr:
	  opt_all expr 
	  {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);
      $$= new_tree_item("in_sum_expr", "", p);
    }
  ;

cast_type:
        BINARY opt_len		{}
        | CHAR_SYM opt_len opt_binary	{}
	| NCHAR_SYM opt_len	{}
        | SIGNED_SYM		{}
        | SIGNED_SYM INT_SYM	{}
        | UNSIGNED		{}
        | UNSIGNED INT_SYM	{}
        | DATE_SYM		{}
        | TIME_SYM		{}
        | DATETIME		{}
        | DECIMAL_SYM float_options {}
	;

expr_list:
	  expr_list2
	  {
	    $$= $1;
	  }
	;

expr_list2:
	expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("expr_list", "", p);
	  }
	| expr_list2 COMMA_SYM expr 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item("comma", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("expr_list", "", p);
	  }
	;

ident_list_arg:
          ident_list          {}
        | '(' ident_list ')'  {};

ident_list:
        {}
        ident_list2
        {};

ident_list2:
        simple_ident {}
        | ident_list2 ',' simple_ident {};

opt_expr:
	/* empty */      {}
	| expr           {};

opt_else:
	/* empty */    {}
	| ELSE expr    {};

when_list:
        {}
	when_list2
	{};

when_list2:
	  expr THEN_SYM expr {}
	| when_list2 WHEN_SYM expr THEN_SYM expr
  ;

/* Warning - may return NULL in case of incomplete SELECT */
table_ref:
    table_factor
    {
      void *p= $$= $1;
      void *t= p;
    }
  | join_table 
    {
      void *p= $$= $1;
      void *t= p;
    }
  ;

join_table_list:
	  derived_table_list		
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    $$= new_tree_item("join_table_list", "", p);
	  }
	;

derived_table_list:
    table_ref 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("derived_table_list", "", p);
    }
  | derived_table_list ',' table_ref 
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, $3);
      $$= new_tree_item("derived_table_list", "", p);
    }
  ;

join_table:
    /*
      Evaluate production 'table_ref' before 'normal_join' so that
      [INNER | CROSS] JOIN is properly nested as other left-associative
      joins.
    */
    table_ref %prec TABLE_REF_PRIORITY normal_join table_ref 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("join_table", "", p);
    }
	| table_ref STRAIGHT_JOIN table_factor {}
	| table_ref normal_join table_ref ON expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("on", ""));
      tree_item_list_add(p, $5);
      $$= new_tree_item("join_table", "", p);
	  }
  | table_ref STRAIGHT_JOIN table_factor ON {} expr {}
	| table_ref normal_join table_ref USING {} '(' using_list ')' {}
	| table_ref LEFT opt_outer JOIN_SYM table_ref ON {} expr {}
	| table_ref LEFT opt_outer JOIN_SYM table_factor {} USING '(' using_list ')' {}
	| table_ref NATURAL LEFT opt_outer JOIN_SYM table_factor {}
	| table_ref RIGHT opt_outer JOIN_SYM table_ref ON {} expr {}
	| table_ref RIGHT opt_outer JOIN_SYM table_factor {} USING '(' using_list ')' {}
	| table_ref NATURAL RIGHT opt_outer JOIN_SYM table_factor {}
	| table_ref NATURAL JOIN_SYM table_factor {}
	;

normal_join:
	  JOIN_SYM 
	  {
	    $$= new_simple_tree_item("normal_join", $1);
	  }
	| INNER_SYM JOIN_SYM 
	  {
	    $$= new_simple_tree_item("normal_join", $1);
	  }
	| CROSS JOIN_SYM 
	  {
	    $$= new_simple_tree_item("normal_join", $1);
	  }
	;

table_factor:
    table_ident opt_table_alias opt_key_definition 
    {
      $$= $1;
    }
	| '{' ident table_ref LEFT OUTER JOIN_SYM table_ref ON {} expr '}' {}
	| select_derived_init get_select_lex select_derived2 {}
	| '(' get_select_lex select_derived union_opt ')' opt_table_alias {}
  ;

/* handle contents of parentheses in join expression */
select_derived:
	  get_select_lex
	  {
}
          derived_table_list
          {
}
 	;

select_derived2:
        {
}
        select_options select_item_list
	{

}
	opt_select_from
        ;

get_select_lex:
	/* Empty */ {}
        ;

select_derived_init:
    SELECT_SYM {}
  ;

opt_outer:
	/* empty */	{}
	| OUTER		{};

opt_key_definition:
	  /* empty */	{}
	| USE_SYM    key_usage_list {}
	| FORCE_SYM key_usage_list {}
	| IGNORE_SYM key_usage_list {}
	;

key_usage_list:
	key_or_index {}
        '(' key_list_or_empty ')'
        {}
	;

key_list_or_empty:
	  /* empty */ 		
	  {
	    $$= NULL;
	  }
	| key_usage_list2	
	  {
	    $$= $1;
	  }
	;

key_usage_list2:
	  key_usage_list2 COMMA_SYM ident 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("key_usage_list2", "", p);
	  }
	| ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("key_usage_list2", "", p);
	  }
	| PRIMARY_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("primary", "primary"));
	    $$= new_tree_item("key_usage_list2", "", p);
	  }
	;

using_list:
	  ident {}
	| using_list ',' ident {}
	;

interval:
	interval_time_st	{}
	| DAY_HOUR_SYM		{}
	| DAY_MICROSECOND_SYM	{}
	| DAY_MINUTE_SYM	{}
	| DAY_SECOND_SYM	{}
	| HOUR_MICROSECOND_SYM	{}
	| HOUR_MINUTE_SYM	{}
	| HOUR_SECOND_SYM	{}
	| MICROSECOND_SYM	{}
	| MINUTE_MICROSECOND_SYM	{}
	| MINUTE_SECOND_SYM	{}
	| SECOND_MICROSECOND_SYM	{}
	| YEAR_MONTH_SYM	{};

interval_time_st:
	DAY_SYM			{}
	| WEEK_SYM		{}
	| HOUR_SYM		{}
	| FRAC_SECOND_SYM	{}
	| MINUTE_SYM		{}
	| MONTH_SYM		{}
	| QUARTER_SYM		{}
	| SECOND_SYM		{}
	| YEAR_SYM		{}
        ;

date_time_type:
    DATE_SYM {}
  | TIME_SYM {}
  | DATETIME {}
  | TIMESTAMP {}
  ;

table_alias:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| AS
	  {
	    $$= new_simple_tree_item("table_alias", "as");
	  }
	| EQ
	  {
	    $$= new_simple_tree_item("table_alias", "eq");
	  }
	;

opt_table_alias:
	/* empty */		
	  {
	    $$= NULL;
	  }
	| table_alias ident
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_table_alias", "", p);
	  }
	;

opt_all:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| ALL
	  {
	    $$= new_simple_tree_item("all", "all");
	  }
	;

where_clause:
	  /* empty */
	  {
	    $$= new_simple_tree_item("where_clause", "");
	  }
	| WHERE expr 
	  {
      void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $2); 
	    $$= new_tree_item("where_clause", "", p); 
	  }
 	;

having_clause:
	  /* empty */
	  {
	    $$= new_simple_tree_item("having_clause", "");
	  }
	| HAVING expr
	  {
      void *p= new_tree_item_list(); 
	    tree_item_list_add(p, $2); 
	    $$= new_tree_item("having_clause", "", p); 
	  }
	;

opt_escape:
	  ESCAPE_SYM simple_expr 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("escape", "escape"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("escape_clause", "", p);
	  }
	| /* empty */
    {
      $$= new_simple_tree_item("escape_clause", "");
    }
  ;

/*
   group by statement in select
*/

group_clause:
	  /* empty */
	  {
	    $$= new_simple_tree_item("group_clause", "");
	  }
	| GROUP BY group_list olap_opt
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("group_clause", "", p);
	  }
	;

group_list:
	  group_list ',' order_ident order_dir
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("group_list_item", "", p);
	  }
	| order_ident order_dir
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("group_list_item", "", p);
	  }
	;

olap_opt:
	  /* empty */ {}
	| WITH CUBE_SYM {}
	| WITH ROLLUP_SYM {}
	;

/*
   Order by statement in select
*/

opt_order_clause:
	  /* empty */
	  {
	    $$= new_simple_tree_item("order_clause", "");
	  }
	| order_clause
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    $$= new_tree_item("order_clause", "", p);
	  }
	;

order_clause:
	  ORDER_SYM BY order_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("order", "order"));
	    tree_item_list_add(p, new_simple_tree_item("by", "by"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("order_clause", "", p);
	  }
	;

order_list:
	order_list COMMA_SYM order_ident order_dir
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("order_list_item", "", p);
	  }
	| order_ident order_dir
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("order_list", "", p);
	  }
	;

order_dir:
	  /* empty */ 
	  {
	    $$= new_simple_tree_item("order_dir", "");
	  }
	| ASC  
	  {
	    $$= new_simple_tree_item("order_dir", "asc");
	  }
	| DESC 
	  {
	    $$= new_simple_tree_item("order_dir", "desc");
	  }
	;


opt_limit_clause_init:
	  /* empty */
	  {
      $$= NULL;
    }
	| limit_clause 
	  {
	    $$= $1;
	  }
	;

opt_limit_clause:
	  /* empty */ 
	  {
	    $$= new_simple_tree_item("limit_clause", "");
	  }
	| limit_clause 
	  {
	    $$= $1;
	  }
	;

limit_clause:
	  LIMIT limit_options 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("limit", "limit"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("limit_clause", "", p);
	  }
	;

limit_options:
	limit_option
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("limit_options", "", p);
    }
	| limit_option COMMA_SYM limit_option
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("comma", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("limit_options", "", p);
    }
	| limit_option OFFSET_SYM limit_option
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("offset", "offset"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("limit_options", "", p);
    }
	;

limit_option:
    param_marker
    {
      $$= new_simple_tree_item("limit_option", $1);
    }
/*
  | ULONGLONG_NUM {}
  | LONG_NUM {}
*/
  | NUM 
    {
      $$= new_simple_tree_item("limit_option", $1);
    }
  ;

delete_limit_clause:
	  /* empty */
	  {
      $$= new_simple_tree_item("delete_limit_clause", "delete_limit_clause");
    }
	| LIMIT limit_option
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("limit", "limit"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("delete_limit_clause", "", p);
    }
  ;

ulong_num:
    NUM           
    {
      $$= $1;
    }
	| HEX_NUM       
	  {
	    $$= $1;
	  }
	| LONG_NUM      
	  {
	    $$= $1;
	  }
	| ULONGLONG_NUM 
	  {
	    $$= $1;
	  }
  | DECIMAL_NUM   
    {
      $$= $1;
    }
	| FLOAT_NUM	
	  {
	    $$= $1;
	  }
	;

ulonglong_num:
	  NUM
	  {
	    printf("num\n");
	    $$= new_simple_tree_item("num", $1);
	  }
	| ULONGLONG_NUM
	  {
	    $$= new_simple_tree_item("num", $1);
	  }
	| LONG_NUM
	  {
	    $$= new_simple_tree_item("num", $1);
	  }
  | DECIMAL_NUM
	  {
	    $$= new_simple_tree_item("num", $1);
	  }
	| FLOAT_NUM
	  {
	    $$= new_simple_tree_item("num", $1);
	  }
	;

procedure_clause:
	/* empty */
	| PROCEDURE ident			/* Procedure name */
	  {
}
	  '(' procedure_list ')';


procedure_list:
	/* empty */ {}
	| procedure_list2 {};

procedure_list2:
	procedure_list2 ',' procedure_item
	| procedure_item;

procedure_item:
	  remember_name expr
	  {
}
          ;


select_var_list_init:
	  select_var_list
	  {
	    $$= $1;
	  }
  ;

select_var_list:
	  select_var_list COMMA_SYM select_var_ident
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("select_var_list", "", p);
	  }
  | select_var_ident 
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("select_var_list", "", p);
    }
  ;

select_var_ident:  
    AT_SYM ident_or_text
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("at", "@"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("select_var_ident", "", p);
    }
  | ident_or_text
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("select_var_ident", "", p);
    }
  ;

into:
    INTO OUTFILE TEXT_STRING_sys opt_field_term opt_line_term
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("into", "into"));
      tree_item_list_add(p, new_simple_tree_item("outfile", "outfile"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, $4);
      tree_item_list_add(p, $5);
      $$= new_tree_item("into", "", p);
    }
	| INTO DUMPFILE TEXT_STRING_sys 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("into", "into"));
      tree_item_list_add(p, new_simple_tree_item("dumpfile", "dumpfile"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("into", "", p);
	  }
	| INTO select_var_list_init 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("into", "into"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("into", "", p);
	  }
  ;

/*
  DO statement
*/

do:	
    DO_SYM expr_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      $$= new_tree_item("do", "", p);
    }
	;

/*
  Drop : delete tables or index or user
*/

drop:
	  DROP opt_temporary table_or_tables if_exists table_list opt_restrict
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      tree_item_list_add(p, $5);
      if($6 != NULL)
      {
        tree_item_list_add(p, $6);
      }
      $$= new_tree_item("drop", "", p);
    }
	| DROP INDEX_SYM ident ON table_ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("index", "index"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("on", "on"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("drop", "", p);
    }
	| DROP DATABASE if_exists ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("database", "database"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      $$= new_tree_item("drop", "", p);
    }
	| DROP FUNCTION_SYM if_exists sp_name
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("function", "function"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      $$= new_tree_item("drop", "", p);
    }
	| DROP PROCEDURE if_exists sp_name
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      $$= new_tree_item("drop", "", p);
    }
	| DROP USER clear_privileges user_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("user", "user"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("drop", "", p);
    }
	| DROP VIEW_SYM if_exists table_list opt_restrict
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("view", "view"));
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      $$= new_tree_item("drop", "", p);
    }
  | DROP TRIGGER_SYM sp_name
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("trigger", "trigger"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("drop", "", p);
    }
	;

table_list:
	  table_name
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("table_list", "", p);
	  }
	| table_list COMMA_SYM table_name
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("table_list", "", p);
	  }
	;

table_name:
	  table_ident
	  {
      $$= $1;
    }
	;

if_exists:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| IF EXISTS 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("if", "if"));
	    tree_item_list_add(p, new_simple_tree_item("exists", "exists"));
	    $$= new_tree_item("if_exists", "", p);
	  }
	;

opt_temporary:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| TEMPORARY 
	  {
	    $$= new_simple_tree_item("temporary", "temporary");
	  }
	;
/*
** Insert : add new data to table
*/

insert:
	  INSERT insert_lock_option opt_ignore insert2 insert_field_spec opt_insert_update
	  {
	    // statement name token is ignored
	    void *p = new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add_all(p, $4);
	    delete_tree_item($4);
	    tree_item_list_add(p, $5);
	    if($6 != NULL)
	    {
	      tree_item_list_add(p, $6);
	    }
	    $$= new_tree_item("insert", "", p);
	  }
	;

replace:
	  REPLACE replace_lock_option insert2 insert_field_spec
	  {
	    void *p = new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("replace", "", p);
	  }
	;

insert_lock_option:
	/* empty */	
	  {
	    $$= NULL;
	  }
	| LOW_PRIORITY	
	  {
	    $$= new_simple_tree_item("low_priority", "low_priority");
	  }
	| DELAYED_SYM	
	  {
	    $$= new_simple_tree_item("delayed", "delayed");
	  }
	| HIGH_PRIORITY 
	  {
	    $$= new_simple_tree_item("high_priority", "high_priority");
	  }
	;

replace_lock_option:
	  opt_low_priority 
	  {
	    $$= $1;
	  }
	| DELAYED_SYM	 
	  {
	    $$= new_simple_tree_item("delayed", "delayed");
	  }
	;

insert2:
	  INTO insert_table 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("into", "into"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("insert2", "insert2", p);
	  }
	| insert_table 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("insert2", "insert2", p);
	  }
	;

insert_table:
	  table_name
	  {
      $$= $1;
    }
  ;

insert_field_spec:
	  insert_values 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("insert_field_spec", "", p);
	  }
	| LP_SYM RP_SYM insert_values 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("insert_field_spec", "", p);
	  }
	| LP_SYM fields RP_SYM insert_values 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("insert_field_spec", "", p);
	  }
	| SET ident_eq_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("set", "set"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("insert_field_spec", "", p);
	  }
	;

fields:
	  fields COMMA_SYM insert_ident 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("fields", "", p);
	  }
	| insert_ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("fields", "", p);
	  }
	;

insert_values:
	  VALUES values_list  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("values", "values"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("insert_values", "", p);
	  }
	| VALUE_SYM values_list  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("value", "value"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("insert_values", "", p);
	  }
	| create_select union_clause 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("insert_values", "", p);
	  }
	| LP_SYM create_select RP_SYM union_opt 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("insert_values", "", p);
	  }
  ;

values_list:
	  values_list COMMA_SYM no_braces
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("values_list", "", p);
	  }
	| no_braces
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("values_list", "", p);
	  }
	;

ident_eq_list:
	  ident_eq_list COMMA_SYM ident_eq_value
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("ident_eq_list", "", p);
	  }
	| ident_eq_value
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("ident_eq_list", "", p);
	  }
	;

ident_eq_value:
	  simple_ident_nospvar equal expr_or_default
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("ident_eq_value", "", p);
    }
  ;

equal:	
    EQ		
    {
      $$= new_simple_tree_item("eq", "=");
    }
	| SET_VAR	
	  {
	    $$= new_simple_tree_item("setvar", ":=");
	  }
	;

opt_equal:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| equal		
	  {
	    $$= $1;
	  }
	;

no_braces:
	  LP_SYM opt_values RP_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      if($2 != NULL) 
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("no_braces", "", p);
    }
  ;

opt_values:
	/* empty */ 
	  {
	    $$= NULL
	  }
	| values
	  {
	    $$= $1;
	  }
	;

values:
	  values COMMA_SYM expr_or_default
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("values", "", p);
    }
	| expr_or_default
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("values", "", p);
    }
	;

expr_or_default:
	  expr	  
	  {
	    $$= $1;
	  }
	| DEFAULT 
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	;

opt_insert_update:
    /* empty */
    {
      $$= NULL;
    }
  | ON DUPLICATE_SYM KEY_SYM UPDATE_SYM insert_update_list
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("on", "on"));
      tree_item_list_add(p, new_simple_tree_item("duplicate", "duplicate"));
      tree_item_list_add(p, new_simple_tree_item("key", "key"));
      tree_item_list_add(p, new_simple_tree_item("update", "update"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("opt_insert_update", "", p);
    }
  ;

/* Update rows in a table */

update:
	  UPDATE_SYM opt_low_priority opt_ignore join_table_list SET update_list where_clause opt_order_clause delete_limit_clause 
	  {
	    void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      tree_item_list_add(p, $4);
      tree_item_list_add(p, new_simple_tree_item("set", "set"));
      tree_item_list_add(p, $6);
      tree_item_list_add(p, $7);
      tree_item_list_add(p, $8);
      tree_item_list_add(p, $9);
	    $$= new_tree_item("update", "", p);
	  }
	;

update_list:
	  update_list COMMA_SYM update_elem
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("update_list", "", p);
	  }
	| update_elem
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
	    $$= new_tree_item("update_list", "", p);
	  }
	;

update_elem:
	  simple_ident_nospvar equal expr_or_default
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("update_elem", "", p);
    }
  ;

insert_update_list:
	  insert_update_list COMMA_SYM insert_update_elem
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("insert_update_list", "", p);
	  }
	| insert_update_elem
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("insert_update_list", "", p);
	  }
	;

insert_update_elem:
	  simple_ident_nospvar equal expr_or_default
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("insert_update_elem", "", p);
    }
  ;

opt_low_priority:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| LOW_PRIORITY	
	  {
	    $$= new_simple_tree_item("low_priority", "low_priority");
	  }
	;

/* Delete rows from a table */

delete:
	  DELETE_SYM opt_delete_options single_multi 
	  {
	    // statement name token is ignored
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3);
	    $$= new_tree_item("delete", "", p);
	  }
	;

single_multi:
 	  FROM table_ident where_clause opt_order_clause delete_limit_clause
 	  {
 	    void *p= new_tree_item_list();
 	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
 	    tree_item_list_add(p, $2);
 	    tree_item_list_add(p, $3);
 	    tree_item_list_add(p, $4);
 	    tree_item_list_add(p, $5);
 	    $$= new_tree_item("single_multi", "", p);
 	  }
	| table_wild_list FROM join_table_list where_clause
	  {
 	    void *p= new_tree_item_list();
 	    tree_item_list_add(p, $1);
 	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
 	    tree_item_list_add(p, $3);
 	    tree_item_list_add(p, $4);
      $$= new_tree_item("single_multi", "", p);
	  }
	| FROM table_wild_list USING join_table_list where_clause
	  {
 	    void *p= new_tree_item_list();
 	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
 	    tree_item_list_add(p, $2);
 	    tree_item_list_add(p, new_simple_tree_item("using", "using"));
 	    tree_item_list_add(p, $4);
 	    tree_item_list_add(p, $5);
      $$= new_tree_item("single_multi", "", p);	  
	  }
	;

table_wild_list:
	  table_wild_one 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("table_wild_list", "", p);
	  }
	| table_wild_list COMMA_SYM table_wild_one 
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    delete_tree_item($3);
	    $$= new_tree_item("table_wild_list", "", p);
	  };

table_wild_one:
	  ident opt_wild opt_table_alias
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      $$= new_tree_item("table_wild_one", "", p);
    }
	| ident DOT_SYM ident opt_wild opt_table_alias
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item(".", "."));
      tree_item_list_add(p, $3);
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      $$= new_tree_item("table_wild_one", "", p);
    }
	;

opt_wild:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| DOT_SYM MULL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("opt_wild", "", p);
	  }
	;


opt_delete_options:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| opt_delete_option opt_delete_options 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add_all(p, $2);
	      delete_tree_item($2);
	    }
	    $$= new_tree_item("opt_delete_options", "", p);
	  }
	;

opt_delete_option:
	  QUICK 
	  {
	    $$= new_simple_tree_item("quick", "quick");
	  }
	| LOW_PRIORITY
	  {
	    $$= new_simple_tree_item("low_priority", "low_priority");
	  }
	| IGNORE_SYM
	  {
	    $$= new_simple_tree_item("ignore", "ignore");
	  }
	;

truncate:
	  TRUNCATE_SYM opt_table_sym table_name
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("truncate", "", p);
    }
	;

opt_table_sym:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| TABLE_SYM
	  {
	    $$= new_simple_tree_item("table", "table");
	  }
	;

/* Show things */

show:	
    SHOW show_param
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add_all(p, $2);
	    delete_tree_item($2);
	    $$= new_tree_item("show", "", p);
	  }
	;

show_param:
    DATABASES wild_and_where
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("databases", "databases"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("show", "", p);
    }
  | opt_full TABLES opt_db wild_and_where
    {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("tables", "tables"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("show", "", p);
    }
  | opt_full TRIGGERS_SYM opt_db wild_and_where
    {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("triggers", "triggers"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("show", "", p);
    }
  | TABLE_SYM STATUS_SYM opt_db wild_and_where
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("table", "table"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));	    
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("show", "", p);
    }
  | OPEN_SYM TABLES opt_db wild_and_where
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("open", "open"));
	    tree_item_list_add(p, new_simple_tree_item("tables", "tables"));	    
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("show", "", p);
    }
	| ENGINE_SYM storage_engines show_engine_param
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("engine", "engine"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("show", "", p);
	  }
	| opt_full COLUMNS from_or_in table_ident opt_db wild_and_where
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("columns", "columns"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    tree_item_list_add(p, $6);
	    $$= new_tree_item("show", "", p);
    }
  | NEW_SYM MASTER_SYM FOR_SYM SLAVE WITH MASTER_LOG_FILE_SYM EQ TEXT_STRING_sys AND_SYM MASTER_LOG_POS_SYM EQ ulonglong_num AND_SYM MASTER_SERVER_ID_SYM EQ ulong_num
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("new", "new"));
	    tree_item_list_add(p, new_simple_tree_item("master", "master"));
	    tree_item_list_add(p, new_simple_tree_item("for", "for"));
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("master_log_file", "master_log_file"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, $8);
	    tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, new_simple_tree_item("master_log_pos", "master_log_pos"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, $12);
      tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, new_simple_tree_item("master_server_id", "master_server_id"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, $16);
	    $$= new_tree_item("show", "", p);
    }
  | master_or_binary LOGS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("logs", "logs"));
	    $$= new_tree_item("show", "", p);
    }
  | SLAVE HOSTS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, new_simple_tree_item("hosts", "hosts"));
	    $$= new_tree_item("show", "", p);
    }
  | BINLOG_SYM EVENTS_SYM binlog_in binlog_from opt_limit_clause_init
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("binlog", "binlog"));
	    tree_item_list_add(p, new_simple_tree_item("events", "events"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    if($5 != NULL)
	    {
	      tree_item_list_add(p, $5);
	    }
	    $$= new_tree_item("show", "", p);
    }
  | keys_or_index from_or_in table_ident opt_db where_clause
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("show", "", p);
    }
	| COLUMN_SYM TYPES_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("column", "column"));
	    tree_item_list_add(p, new_simple_tree_item("types", "types"));
      $$= new_tree_item("show", "", p);
    }
	| TABLE_SYM TYPES_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("table", "table"));
	    tree_item_list_add(p, new_simple_tree_item("types", "types"));
      $$= new_tree_item("show", "", p);
    }
	| opt_storage ENGINES_SYM
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("engines", "engines"));
      $$= new_tree_item("show", "", p);
    }
	| PRIVILEGES
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("privileges", "privileges"));
      $$= new_tree_item("show", "", p);
    } 
  | COUNT_SYM LP_SYM MULL_SYM RP_SYM WARNINGS
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("count", "count"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("show", "", p);
    }
  | COUNT_SYM LP_SYM MULL_SYM RP_SYM ERRORS
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("count", "count"));
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      tree_item_list_add(p, new_simple_tree_item("errors", "errors"));
      $$= new_tree_item("show", "", p);
	  }
  | WARNINGS opt_limit_clause_init
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("warnings", "warnings"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
      $$= new_tree_item("show", "", p);
    }
  | ERRORS opt_limit_clause_init
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("errors", "errors"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
      $$= new_tree_item("show", "", p);
    }
  | opt_var_type STATUS_SYM wild_and_where
    {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
      $$= new_tree_item("show", "", p);
    }	
  | INNOBASE_SYM STATUS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("innobase", "innobase"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
      $$= new_tree_item("show", "", p);
    }
  | MUTEX_SYM STATUS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("mutex", "mutex"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
      $$= new_tree_item("show", "", p);
    }
	| opt_full PROCESSLIST_SYM
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("processlist", "processlist"));
      $$= new_tree_item("show", "", p);
	  }
  | opt_var_type  VARIABLES wild_and_where
	  {
	    void *p= new_tree_item_list();
	    if($1 != NULL)
	    {
	      tree_item_list_add(p, $1);
	    }
	    tree_item_list_add(p, new_simple_tree_item("variables", "variables"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
  | charset wild_and_where
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
      $$= new_tree_item("show", "", p);
    }
  | COLLATION_SYM wild_and_where
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("collation", "collation"));
	    tree_item_list_add(p, $2);
      $$= new_tree_item("show", "", p);
    }
	| BERKELEY_DB_SYM LOGS_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("berkeley_db", "berkeley_db"));
	    tree_item_list_add(p, new_simple_tree_item("logs", "logs"));
      $$= new_tree_item("show", "", p);
	  }
	| LOGS_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("logs", "logs"));
      $$= new_tree_item("show", "", p);
	  }
	| GRANTS
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("grants", "grants"));
      $$= new_tree_item("show", "", p);
    }
	| GRANTS FOR_SYM user
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("grants", "grants"));
	    tree_item_list_add(p, new_simple_tree_item("for", "for"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
	| CREATE DATABASE opt_if_not_exists ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("database", "database"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
      $$= new_tree_item("show", "", p);
    }
  | CREATE TABLE_SYM table_ident
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("table", "table"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
  | CREATE VIEW_SYM table_ident
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("view", "view"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
  | MASTER_SYM STATUS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("master", "master"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
      $$= new_tree_item("show", "", p);
    }
  | SLAVE STATUS_SYM
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
      $$= new_tree_item("show", "", p);
    }
	| CREATE PROCEDURE sp_name
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
	| CREATE FUNCTION_SYM sp_name
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("function", "function"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    } 
	| PROCEDURE STATUS_SYM wild_and_where
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
	| FUNCTION_SYM STATUS_SYM wild_and_where
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
	    tree_item_list_add(p, $3);
      $$= new_tree_item("show", "", p);
    }
  ;

show_engine_param:
	STATUS_SYM
	  {
      $$= new_simple_tree_item("status", "status");
    }
	| LOGS_SYM
	  {
	    $$= new_simple_tree_item("logs", "logs");
    }
  ;

master_or_binary:
	  MASTER_SYM
	  {
	    $$= new_simple_tree_item("master", "master");
	  }
	| BINARY
	  {
	    $$= new_simple_tree_item("binary", "binary");
	  }
	;

opt_storage:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| STORAGE_SYM
	  {
	    $$= new_simple_tree_item("storage", "storage");
	  }
	;

opt_db:
	  /* empty */  
	  {
	    $$= NULL;
	  }
	| from_or_in ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_db", "", p);
	  }
	;

opt_full:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| FULL	    
	  {
	    $$= new_simple_tree_item("full", "full");
	  }
	;

from_or_in:
	  FROM
	  {
	    $$= new_simple_tree_item("from", "from");
	  }
	| IN_SYM
	  {
	    $$= new_simple_tree_item("in", "in");
	  }
	;

binlog_in:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| IN_SYM TEXT_STRING_sys 
	  {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("in", "in"));
	    tree_item_list_add(p, $2);
      $$= new_tree_item("binlog_in", "", p);
	  }
	;

binlog_from:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
  | FROM ulonglong_num 
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    tree_item_list_add(p, $2);
      $$= new_tree_item("binlog_from", "", p);
    }
  ;

wild_and_where:
    /* empty */
  | LIKE TEXT_STRING_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("like", "like"));
	    tree_item_list_add(p, $2);
      $$= new_tree_item("wild_and_where", "", p);
    }
  | WHERE expr
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("where", "where"));
	    tree_item_list_add(p, $2);
      $$= new_tree_item("wild_and_where", "", p);
    }
  ;


/* A Oracle compatible synonym for show */
describe:
	  describe_command table_ident opt_describe_column 
	  {
	   void *p= new_tree_item_list();
	   tree_item_list_add(p, $2);
	   if($3 != NULL)
	   {
	     tree_item_list_add(p, $3);
	   }
	   $$= new_tree_item("describe", "", p);
	  }
	| describe_command opt_extended_describe select
    {
	   void *p= new_tree_item_list();
	   if($2 != NULL)
	   {
	     tree_item_list_add(p, $2);
	   }
	   tree_item_list_add(p, $3);
	   $$= new_tree_item("describe", "", p);
    }
	;

describe_command:
	  DESC
	  {
	    $$= new_simple_tree_item("desc", "desc");
	  }
	| DESCRIBE
	  {
	    $$= new_simple_tree_item("describe", "describe");
	  }
	;

opt_extended_describe:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| EXTENDED_SYM 
	  {
	    $$= new_simple_tree_item("extended", "extended");
	  }
	;

opt_describe_column:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| text_string	
	  {
	    $$= $1;
	  }
	| ident
	  {
	    $$= $1;
	  }
	;


/* flush things */

flush:
	  FLUSH_SYM opt_no_write_to_binlog flush_options
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("flush", "", p);
	  }
	;

flush_options:
	  flush_options COMMA_SYM flush_option
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("flush_options", "", p);
	  }
	| flush_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("flush_options", "", p);
	  }
	;

flush_option:
	  table_or_tables	opt_table_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("flush_option", "", p);
	  }
	| TABLES WITH READ_SYM LOCK_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("tables", "tables"));
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("read", "read"));
	    tree_item_list_add(p, new_simple_tree_item("lock", "lock"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| QUERY_SYM CACHE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("query", "query"));
	    tree_item_list_add(p, new_simple_tree_item("cache", "cache"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| HOSTS_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("hosts", "hosts"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| PRIVILEGES	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("privileges", "privileges"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| LOGS_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("logs", "logs"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| STATUS_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("status", "status"));
	    $$= new_tree_item("flush_option", "", p);
	  }
  | SLAVE         
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    $$= new_tree_item("flush_option", "", p);
	  }
  | MASTER_SYM    
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("master", "master"));
	    $$= new_tree_item("flush_option", "", p);
	  }
	| DES_KEY_FILE	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("des_key_file", "des_key_file"));
	    $$= new_tree_item("flush_option", "", p);
	  }
 	| RESOURCES
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("resources", "resources"));
	    $$= new_tree_item("flush_option", "", p);
	  }
 	;

opt_table_list:
	  /* empty */  
	  {
	    $$= NULL;
	  }
	| table_list 
	  {
	    $$= $1;
	  }
	;

reset:
	  RESET_SYM reset_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("reset", "", p);
	  }
	;

reset_options:
	  reset_options COMMA_SYM reset_option
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("reset", "", p);
	  }
	| reset_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("reset_options", "", p);
	  }
	;

reset_option:
    SLAVE                 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
      $$= new_tree_item("reset_options", "", p);
    }
  | MASTER_SYM          
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("master", "master"));
      $$= new_tree_item("reset_options", "", p);
    }
  | QUERY_SYM CACHE_SYM
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("query", "query"));
      tree_item_list_add(p, new_simple_tree_item("cache", "cache"));
      $$= new_tree_item("reset_options", "", p);
    }
  ;

purge:
	  PURGE purge_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("purge", "", p);
	  }
	;

purge_options:
	  master_or_binary LOGS_SYM purge_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("logs", "logs"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("purge_options", "", p);
	  }
	;

purge_option:
    TO_SYM TEXT_STRING_sys
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("purge_option", "", p);
    }
	| BEFORE_SYM expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("before", "before"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("purge_option", "", p);
    }
	;

/* kill threads */

kill:
	  KILL_SYM kill_option expr
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, $3);
      $$= new_tree_item("kill", "", p);
    }
  ;

kill_option:
	  /* empty */	 
	  {
	    $$= NULL;
	  }
	| CONNECTION_SYM 
	  {
	    $$= new_simple_tree_item("connection", "connection");
	  }
	| QUERY_SYM      
	  {
	    $$= new_simple_tree_item("query", "query");
	  }
	;



/* change database */

use:	
    USE_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      $$= new_tree_item("use", "", p);
    }
  ;

/* import, export of files */

load:     
    LOAD DATA_SYM load_data 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("data", "data"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("load", "", p);
    }
  | LOAD TABLE_SYM table_ident FROM MASTER_SYM
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("table", "table"));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("from", "from"));
      tree_item_list_add(p, new_simple_tree_item("master", "master"));
      $$= new_tree_item("load", "", p);
    }
  ;

load_data:
	  load_data_lock opt_local INFILE TEXT_STRING_sys opt_duplicate INTO TABLE_SYM table_ident opt_field_term opt_line_term 
	  opt_ignore_lines opt_field_or_var_spec opt_load_data_set_spec
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, new_simple_tree_item("infile", "infile"));
      tree_item_list_add(p, $4);
      if($5 != NULL)
      {
        tree_item_list_add(p, $5);
      }
      tree_item_list_add(p, new_simple_tree_item("into", "into"));
      tree_item_list_add(p, new_simple_tree_item("table", "table"));
      tree_item_list_add(p, $8);
      if($9 != NULL)
      {
        tree_item_list_add(p, $9);
      }
      if($10 != NULL)
      {
        tree_item_list_add(p, $10);
      }
      if($11 != NULL)
      {
        tree_item_list_add(p, $11);
      }
      if($12 != NULL)
      {
        tree_item_list_add(p, $12);
      }
      if($13 != NULL)
      {
        tree_item_list_add(p, $13);
      }
      $$= new_tree_item("load_data", "", p);
    }
  | FROM MASTER_SYM
    {
    }
  ;

opt_local:
	/* empty */	
	  {
	    $$= NULL;
	  }
	| LOCAL_SYM	
	  {
	    $$= new_simple_tree_item("local", "local");
	  }
	;

load_data_lock:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| CONCURRENT	
	  {
	    $$= new_simple_tree_item("concurrent", "concurrent");
	  }
	| LOW_PRIORITY	
	  {
	    $$= new_simple_tree_item("low_priority", "low_priority");
	  }
	;


opt_duplicate:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| REPLACE	
	  {
	    $$= new_simple_tree_item("replace", "replace");
	  }
	| IGNORE_SYM	
	  {
	    $$= new_simple_tree_item("ignore", "ignore");
	  };

opt_field_term:
	  /* empty */
	  {
	    $$= new_simple_tree_item("opt_field_term", "");
	  }
	| COLUMNS field_term_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("columns", "columns"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_field_term", "", p);
	  }
	;

field_term_list:
	  field_term_list field_term
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("field_term_list", "", p);
	  }
	| field_term
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("field_term_list", "", p);
	  }
	;

field_term:
	  TERMINATED BY text_string 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("teminated", "teminated"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("field_term", "", p);
    }
	| OPTIONALLY ENCLOSED BY text_string
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("optionally", "optionally"));
      tree_item_list_add(p, new_simple_tree_item("enclosed", "enclosed"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $4);
      $$= new_tree_item("field_term", "", p);
    }
  | ENCLOSED BY text_string
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("enclosed", "enclosed"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("field_term", "", p);
    }
  | ESCAPED BY text_string
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("escaped", "escaped"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("field_term", "", p);
    }
  ;

opt_line_term:
	  /* empty */
	  {
	    $$= new_simple_tree_item("opt_line_term", "");
	  }
	| LINES line_term_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lines", "lines"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("opt_line_term", "", p);
	  }
	;

line_term_list:
	  line_term_list line_term
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("line_term_list", "", p);
	  }
	| line_term
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
	    $$= new_tree_item("line_term_list", "", p);
	  }
	;

line_term:
    TERMINATED BY text_string
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("terminated", "terminated"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("line_term", "", p);
    }
  | STARTING BY text_string
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("starting", "starting"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("line_term", "", p);
    }
  ;

opt_ignore_lines:
	  /* empty */
	  {
	    $$= NULL;
	  }
  | IGNORE_SYM NUM LINES
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("ingore", "ingore"));
      tree_item_list_add(p, new_simple_tree_item("num", $2));
      tree_item_list_add(p, new_simple_tree_item("lines", "lines"));
      $$= new_tree_item("opt_ignore_lines", "", p);
    }
  ;

opt_field_or_var_spec:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| LP_SYM fields_or_vars RP_SYM  
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("opt_field_or_var_spec", "", p);
	  }
	| LP_SYM RP_SYM	          
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("opt_field_or_var_spec", "", p);
	  }
	;

fields_or_vars:
    fields_or_vars COMMA_SYM field_or_var
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("fields_or_vars", "", p);
    }
  | field_or_var
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("fields_or_vars", "", p);
    }
  ;

field_or_var:
    simple_ident_nospvar 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("field_or_var", "", p);
    }
  | AT_SYM ident_or_text
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("at", "@"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("field_or_var", "", p);
    }
  ;

opt_load_data_set_spec:
    /* empty */           
    {
      $$= NULL;
    }
  | SET insert_update_list  
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("set", "set"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("opt_load_data_set_spec", "", p);
    }
  ;


/* Common definitions */

text_literal:
	TEXT_STRING_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("text_literal", "", p);
	  }
	| NCHAR_STRING
    {
      $$= NULL /* todo */
    }
	| UNDERSCORE_CHARSET STRING
	  {
	    $$= NULL /* todo */
	  }
	| text_literal TEXT_STRING_literal
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("text_literal", "", p);
	  }
	;

text_string:
	  TEXT_STRING_literal 
	  {
	    $$= $1;
	  }
	| HEX_NUM 
	  {
	    $$= new_simple_tree_item("hex_num", $1);
	  }
  | BIN_NUM 
    {
      $$= new_simple_tree_item("bin_num", $1);
    }
	;

param_marker:
    PARAM_MARKER 
    {
      $$= $1;
    }
	;

signed_literal:
	  literal 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("signed_literal", "", p);
	  }
	| PLUS_SYM NUM_literal 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("plus", "+"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("signed_literal", "", p);
	  }
	| MINUS_SYM NUM_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("minus", "-"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("signed_literal", "", p);
	  }
	;

literal:
	  text_literal	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("literal", "", p);
	  }
	| NUM_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("literal", "", p);
	  }
	| NULL_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("null", "null"));
	    $$= new_tree_item("literal", "", p);
	  }
	| FALSE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("false", "false"));
	    $$= new_tree_item("literal", "", p);
	  }
	| TRUE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("true", "true"));
	    $$= new_tree_item("literal", "", p);
	  }
	| HEX_NUM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("literal", "", p);
	  }
	| BIN_NUM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("literal", "", p);
	  }
	| UNDERSCORE_CHARSET HEX_NUM 
	  {
	    $$= NULL; /* todo */
	  }
	| UNDERSCORE_CHARSET BIN_NUM 
	  {
	    $$= NULL; /* todo */
	  }
	| DATE_SYM text_literal 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("date", "date"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("literal", "", p);
	  }
	| TIME_SYM text_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("time", "time"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("literal", "", p);
	  }
	| TIMESTAMP text_literal
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("timestamp", "timestamp"));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("literal", "", p);
	  }
	;

NUM_literal:
	  NUM 
	  { 
	    $$= new_simple_tree_item("num", $1); 
	  }
	| LONG_NUM 
	  { 
	    $$= new_simple_tree_item("long_num", $1); 
	  }
	| ULONGLONG_NUM 
	  { 
	    $$= new_simple_tree_item("ulonglong_num", $1); 
	  }
  | DECIMAL_NUM 
    { 
      $$= new_simple_tree_item("decimal_num", $1); 
    }
	| FLOAT_NUM 
	  { 
	    $$= new_simple_tree_item("float_num", $1); 
	  }
	;

/**********************************************************************
** Creating different items.
**********************************************************************/

insert_ident:
	  simple_ident_nospvar 
	  {
	    $$= $1;
	  }
	| table_wild	 
	  {
	    $$= $1;
	  }
	;

table_wild:
	  ident DOT_SYM MULL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("table_wild", "", p);
	  }
	| ident DOT_SYM ident DOT_SYM MULL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item(".", "."));	    
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("table_wild", "", p);
	  }
	;

order_ident:
	  expr 
	  {
	    $$= $1;
	  }
	;

simple_ident:
	  ident 
	  {
	    $$= $1; 
	  }
  | simple_ident_q 
    { 
      $$= $1; 
    }
	;

simple_ident_nospvar:
	  ident
	  {
      $$= $1;
    }
	| simple_ident_q 
	  {
	    $$= $1;
	  }
	;

simple_ident_q:
	  ident DOT_SYM ident 
	  {
	  }
	| DOT_SYM ident DOT_SYM ident 
	  {
	  }
	| ident DOT_SYM ident DOT_SYM ident 
	  {
	  }
  ;


field_ident:
	  ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("field_ident", "field_ident", p);
	  }
	| ident DOT_SYM ident DOT_SYM ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("field_ident", "field_ident", p);
	  }
	| ident DOT_SYM ident 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("field_ident", "field_ident", p);
	  }
	| DOT_SYM ident		
	  /* For Delphi */
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("field_ident", "field_ident", p);
	  }
	;	

table_ident:
	  ident 
	  { 
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("table_ident", "table_ident", p);
	  }
	| ident DOT_SYM ident	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("table_ident", "table_ident", p);
	  }
	| DOT_SYM ident 
	  /* For Delphi */
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("table_ident", "table_ident", p);
	  } 
  ;

table_ident_nodb:
	  ident			
	  {
	    $$= $1;
	  }
  ;

IDENT_sys:
	  IDENT 
	  { 
	    $$= new_simple_tree_item("ident", $1); 
	  }
	| QUOTED 
	  { 
	    $$= new_simple_tree_item("quoted_ident", $1); 
	  }
	;

TEXT_STRING_sys:
	  STRING
	  {
      $$= new_simple_tree_item("TEXT_STRING_sys", $1);
    }
	;

TEXT_STRING_literal:
	STRING
	{
	  $$= new_simple_tree_item("TEXT_STRING_literal", $1);
  }
	;

ident:
	  IDENT_sys	  
	  { 
	    $$= $1; 
	  }
	| keyword     
	  { 
	    $$= new_simple_tree_item("keyword", $1); 
	  }
	;

label_ident:
	  IDENT_sys
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("label_ident", "", p);
	  }
	| keyword_sp
	  {
      $$= NULL; /* todo */
    }
	;

ident_or_text:
    ident
    {
      $$= $1;
    }
	| TEXT_STRING_sys
    {
      $$= $1;
    }
	| LEX_HOSTNAME
    {
      $$= NULL; /* todo */
    }
	;

user:
	  ident_or_text
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("user", "", p);
    }
	| ident_or_text AT_SYM ident_or_text
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("at", "@"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("user", "", p);
    }
	| CURRENT_USER optional_braces
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("current_user", "current_user"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("user", "", p);
    }
  ;

/* Keyword that we allow for identifiers (except SP labels) */
keyword:
	  keyword_sp		{}
	| ASCII_SYM		{}
	| BACKUP_SYM		{}
	| BEGIN_SYM		{}
	| BYTE_SYM		{}
	| CACHE_SYM		{}
	| CHARSET		{}
	| CHECKSUM_SYM		{}
	| CLOSE_SYM		{}
	| COMMENT_SYM		{}
	| COMMIT_SYM		{}
	| CONTAINS_SYM          {}
  | DEALLOCATE_SYM        {}
	| DO_SYM		{}
	| END			{}
	| EXECUTE_SYM		{}
	| FLUSH_SYM		{}
	| HANDLER_SYM		{}
	| HELP_SYM		{}
	| LANGUAGE_SYM          {}
	| NO_SYM		{}
	| OPEN_SYM		{}
  | PREPARE_SYM           {}
	| REPAIR		{}
	| RESET_SYM		{}
	| RESTORE_SYM		{}
	| ROLLBACK_SYM		{}
	| SAVEPOINT_SYM		{}
	| SECURITY_SYM		{}
	| SIGNED_SYM		{}
	| SLAVE			{}
	| START_SYM		{}
	| STOP_SYM		{}
	| TRUNCATE_SYM		{}
	| UNICODE_SYM		{}
  | XA_SYM                {}
	;

/*
 * Keywords that we allow for labels in SPs.
 * Anything that's the beginning of a statement or characteristics
 * must be in keyword above, otherwise we get (harmful) shift/reduce
 * conflicts.
 */
keyword_sp:
	  ACTION			
	| ADDDATE_SYM		
	| AFTER_SYM		
	| AGAINST		
	| AGGREGATE_SYM		
	| ALGORITHM_SYM		
	| ANY_SYM		
	| AUTO_INC		
	| AVG_ROW_LENGTH	
	| AVG_SYM		
	| BERKELEY_DB_SYM	
	| BINLOG_SYM		
	| BIT_SYM		
	| BOOL_SYM		
	| BOOLEAN_SYM		
	| BTREE_SYM		
	| CASCADED              
	| CHAIN_SYM		
	| CHANGED		
	| CIPHER_SYM		
	| CLIENT_SYM		
	| COLLATION_SYM		
  | COLUMNS               
	| COMMITTED_SYM		
	| COMPACT_SYM		
	| COMPRESSED_SYM	
	| CONCURRENT		
	| CONSISTENT_SYM	
	| CUBE_SYM		
	| DATA_SYM		
	| DATETIME		
	| DATE_SYM		
	| DAY_SYM		
	| DEFINER_SYM		
	| DELAY_KEY_WRITE_SYM	
	| DES_KEY_FILE		
	| DIRECTORY_SYM		
	| DISCARD		
	| DUMPFILE		
	| DUPLICATE_SYM		
	| DYNAMIC_SYM		
	| ENUM			
	| ENGINE_SYM		
	| ENGINES_SYM		
	| ERRORS		
	| ESCAPE_SYM		
	| EVENTS_SYM		
  | EXPANSION_SYM         
	| EXTENDED_SYM		
	| FAST_SYM		
	| FOUND_SYM		
	| DISABLE_SYM		
	| ENABLE_SYM		
	| FULL			
	| FILE_SYM		
	| FIRST_SYM		
	| FIXED_SYM		
	| FRAC_SECOND_SYM	
	| GEOMETRY_SYM		
	| GEOMETRYCOLLECTION	
	| GET_FORMAT		
	| GRANTS		
	| GLOBAL_SYM		
	| HASH_SYM		
	| HOSTS_SYM		
	| HOUR_SYM		
	| IDENTIFIED_SYM	
	| INVOKER_SYM		
	| IMPORT		
	| INDEXES		
	| ISOLATION		
	| ISSUER_SYM		
	| INNOBASE_SYM		
	| INSERT_METHOD		
	| RELAY_THREAD		
	| LAST_SYM		
	| LEAVES                
	| LEVEL_SYM		
	| LINESTRING		
	| LOCAL_SYM		
	| LOCKS_SYM		
	| LOGS_SYM		
	| MAX_ROWS		
	| MASTER_SYM		
	| MASTER_HOST_SYM	
	| MASTER_PORT_SYM	
	| MASTER_LOG_FILE_SYM	
	| MASTER_LOG_POS_SYM	
	| MASTER_USER_SYM	
	| MASTER_PASSWORD_SYM	
	| MASTER_SERVER_ID_SYM  
	| MASTER_CONNECT_RETRY_SYM	
	| MASTER_SSL_SYM	
	| MASTER_SSL_CA_SYM	
	| MASTER_SSL_CAPATH_SYM	
	| MASTER_SSL_CERT_SYM	
	| MASTER_SSL_CIPHER_SYM	
	| MASTER_SSL_KEY_SYM	
	| MAX_CONNECTIONS_PER_HOUR	 
	| MAX_QUERIES_PER_HOUR	
	| MAX_UPDATES_PER_HOUR	
	| MAX_USER_CONNECTIONS_SYM 
	| MEDIUM_SYM		
	| MERGE_SYM		
	| MICROSECOND_SYM	
  | MIGRATE_SYM           
	| MINUTE_SYM		
	| MIN_ROWS		
	| MODIFY_SYM		
	| MODE_SYM		
	| MONTH_SYM		
	| MULTILINESTRING	
	| MULTIPOINT		
	| MULTIPOLYGON		
  | MUTEX_SYM             
	| NAME_SYM              
	| NAMES_SYM		
	| NATIONAL_SYM		
	| NCHAR_SYM		
	| NDBCLUSTER_SYM	
	| NEXT_SYM		
	| NEW_SYM		
	| NONE_SYM		
	| NVARCHAR_SYM		
	| OFFSET_SYM		
	| OLD_PASSWORD		
	| ONE_SHOT_SYM		
  | ONE_SYM               
	| PACK_KEYS_SYM		
	| PARTIAL		
	| PASSWORD		
  | PHASE_SYM             
	| POINT_SYM		
	| POLYGON		
	| PREV_SYM		
  | PRIVILEGES            
	| PROCESS		
	| PROCESSLIST_SYM	
	| QUARTER_SYM		
	| QUERY_SYM		
	| QUICK			
	| RAID_0_SYM		
	| RAID_CHUNKS		
	| RAID_CHUNKSIZE	
	| RAID_STRIPED_SYM	
	| RAID_TYPE		
  | RECOVER_SYM           
  | REDUNDANT_SYM         
	| RELAY_LOG_FILE_SYM	
	| RELAY_LOG_POS_SYM	
	| RELOAD		
	| REPEATABLE_SYM	
	| REPLICATION		
	| RESOURCES		
  | RESUME_SYM            
	| RETURNS_SYM           
	| ROLLUP_SYM		
	| ROUTINE_SYM		
	| ROWS_SYM		
	| ROW_FORMAT_SYM	
	| ROW_SYM		
	| RTREE_SYM		
	| SECOND_SYM		
	| SERIAL_SYM		
	| SERIALIZABLE_SYM	
	| SESSION_SYM		
	| SIMPLE_SYM		
	| SHARE_SYM		
	| SHUTDOWN		
	| SNAPSHOT_SYM		
	| SOUNDS_SYM		
	| SQL_CACHE_SYM		
	| SQL_BUFFER_RESULT	
	| SQL_NO_CACHE_SYM	
	| SQL_THREAD		
	| STATUS_SYM		
	| STORAGE_SYM		
	| STRING_SYM		
	| SUBDATE_SYM		
	| SUBJECT_SYM		
	| SUPER_SYM		
  | SUSPEND_SYM           
  | TABLES                
	| TABLESPACE		
	| TEMPORARY		
	| TEMPTABLE_SYM		
	| TEXT_SYM		
	| TRANSACTION_SYM	
	| TRIGGERS_SYM		
	| TIMESTAMP		
	| TIMESTAMP_ADD		
	| TIMESTAMP_DIFF	
	| TIME_SYM		
	| TYPES_SYM		
  | TYPE_SYM              
  | UDF_RETURNS_SYM       
	| FUNCTION_SYM		
	| UNCOMMITTED_SYM	
	| UNDEFINED_SYM		
	| UNKNOWN_SYM		
	| UNTIL_SYM		
	| USER			
	| USE_FRM		
	| VARIABLES		
	| VIEW_SYM		
	| VALUE_SYM		
	| WARNINGS		
	| WEEK_SYM		
	| WORK_SYM		
	| X509_SYM		
	| YEAR_SYM		
	;

/* Option functions */

set:
	  SET opt_option option_value_list
	  {
	    void *p= new_tree_item_list();
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("set", "", p);
	  }
	;

opt_option:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| OPTION 
	  {
	    $$= new_simple_tree_item("option", "option");
	  }
	;

option_value_list:
	  option_type_value
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("option_value_list", "", p);
	  }
	| option_value_list COMMA_SYM option_type_value
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("option_value_list", "", p);
	  
	  }
	;

option_type_value:
	  ext_option_value
    {
      $$= $1;
    }
  ;

option_type:
    option_type2    
    {
      $$= $1; // can be NULL
    }
	| GLOBAL_SYM	
	  {
	    $$= new_simple_tree_item("global", "global");
	  }
	| LOCAL_SYM	
	  {
	    $$= new_simple_tree_item("local", "local");
	  }
	| SESSION_SYM	
	  {
	    $$= new_simple_tree_item("session", "session");
	  }
	;

option_type2:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| ONE_SHOT_SYM	
	  {
	    $$= new_simple_tree_item("one_shot", "one_shot");
	  }
	;

opt_var_type:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| GLOBAL_SYM	
	  {
	    $$= new_simple_tree_item("global", "global");
	  }
	| LOCAL_SYM	
	  {
	    $$= new_simple_tree_item("local", "local"); 
	  }
	| SESSION_SYM	
	  {
	    $$= new_simple_tree_item("session", "session");
	  }
	;

opt_var_ident_type:
	  /* empty */		
	  {
	    $$= NULL;
	  }
	| GLOBAL_SYM DOT_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("global", "global"));
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    $$= new_tree_item("opt_var_ident_type", "", p);
	  }
	| LOCAL_SYM DOT_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("local", "local"));
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    $$= new_tree_item("opt_var_ident_type", "", p);
	  }
	| SESSION_SYM DOT_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("session", "session"));
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    $$= new_tree_item("opt_var_ident_type", "", p);
	  }
	;

ext_option_value:
    sys_option_value
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("ext_option_value", "", p);
    }
  | option_type2 option_value
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);
      $$= new_tree_item("ext_option_value", "", p);
    }
  ;

sys_option_value:
    option_type internal_variable_name equal set_expr_or_default
    {
      void *p= new_tree_item_list();
      if($1 != NULL)
      {
        tree_item_list_add(p, $1);
      }
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      tree_item_list_add(p, $4);
      $$= new_tree_item("ext_option_value", "", p);
    }
  | option_type TRANSACTION_SYM ISOLATION LEVEL_SYM isolation_types
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("transaction", "transaction"));
      tree_item_list_add(p, new_simple_tree_item("isolation", "isolation"));
      tree_item_list_add(p, new_simple_tree_item("level", "level"));
      tree_item_list_add(p, $5);
      $$= new_tree_item("ext_option_value", "", p);
    }
  ;

option_value:
	  AT_SYM ident_or_text equal expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("at", "@"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    $$= new_tree_item("option_value", "", p);
    }
	| AT_SYM AT_SYM opt_var_ident_type internal_variable_name equal set_expr_or_default
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("at", "@"));
	    tree_item_list_add(p, new_simple_tree_item("at", "@"));
      if($3 != NULL)
      {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, $5);
	    tree_item_list_add(p, $6);
	    $$= new_tree_item("option_value", "", p);
    }
	| charset old_or_new_charset_name_or_default
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("option_value", "", p);
    }
  | NAMES_SYM equal expr
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("names", "names"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("option_value", "", p);
    }
	| NAMES_SYM charset_name_or_default opt_collate
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("names", "names"));
	    tree_item_list_add(p, $2);
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    $$= new_tree_item("option_value", "", p);
    }
	| PASSWORD equal text_or_password
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("password", "password"));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("option_value", "", p);
    }
	| PASSWORD FOR_SYM user equal text_or_password
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("password", "password"));
	    tree_item_list_add(p, new_simple_tree_item("for", "for"));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("option_value", "", p);
    }
	;

internal_variable_name:
	  ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("internal_variable_name", "", p);
    }
	| ident DOT_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("internal_variable_name", "", p);
    }
	| DEFAULT DOT_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("default", "default"));
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("internal_variable_name", "", p);
    }
  ;

isolation_types:
	  READ_SYM UNCOMMITTED_SYM	
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("read", "read"));
      tree_item_list_add(p, new_simple_tree_item("uncommited", "uncommited"));
      $$= new_tree_item("isolation_types", "", p);
	  }
	| READ_SYM COMMITTED_SYM	
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("read", "read"));
      tree_item_list_add(p, new_simple_tree_item("commited", "commited"));
      $$= new_tree_item("isolation_types", "", p);
	  }
	| REPEATABLE_SYM READ_SYM	
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("repeatable", "repeatable"));
      tree_item_list_add(p, new_simple_tree_item("read", "read"));
      $$= new_tree_item("isolation_types", "", p);
	  }
	| SERIALIZABLE_SYM		
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("serializable", "serializable"));
      $$= new_tree_item("isolation_types", "", p);
	  }
	;

text_or_password:
	STRING {}
	| PASSWORD '(' STRING ')'
	  {
}
	| OLD_PASSWORD '(' STRING ')'
	  {
}
          ;


set_expr_or_default:
	  expr      
	  {
	    $$= $1;
	  }
	| DEFAULT 
	  {
	    $$= new_simple_tree_item("default", "default");
	  }
	| ON	  
	  {
	    $$= new_simple_tree_item("on", "on");
	  }
	| ALL	  
	  {
	    $$= new_simple_tree_item("all", "all");
	  }
	| BINARY  
	  {
	    $$= new_simple_tree_item("binary", "binary");
	  }
	;


/* Lock function */

lock:
	  LOCK_SYM table_or_tables table_lock_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("lock", "", p);
	  }
	;

table_or_tables:
	  TABLE_SYM
	  {
	    $$= new_simple_tree_item("table", "table");
	  }
	| TABLES
	  {
	    $$= new_simple_tree_item("tables", "tables");
	  }
	;

table_lock_list:
	  table_lock
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("table_lock_list", "", p);
	  }
	| table_lock_list COMMA_SYM table_lock
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("table_lock_list", "", p);
	  }
	;

table_lock:
	  table_ident opt_table_alias lock_option
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("table_lock", "", p);
    }
  ;

lock_option:
	  READ_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("read", "read"));
	    $$= new_tree_item("lock_option", "", p);
	  }
	| WRITE_SYM     
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("write", "write"));
	    $$= new_tree_item("lock_option", "", p);
	  }
	| LOW_PRIORITY WRITE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("low_priority", "low_priority"));
	    tree_item_list_add(p, new_simple_tree_item("write", "write"));
	    $$= new_tree_item("lock_option", "", p);
	  }
	| READ_SYM LOCAL_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("read", "read"));
	    tree_item_list_add(p, new_simple_tree_item("local", "local"));
	    $$= new_tree_item("lock_option", "", p);
	  }
  ;

unlock:
	  UNLOCK_SYM table_or_tables
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    $$= new_tree_item("unlock", "", p);
	  }
  ;


/*
** Handler: direct access to ISAM functions
*/

handler:
	HANDLER_SYM table_ident OPEN_SYM opt_table_alias
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("open", "open"));
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("handler", "", p);
    }
	| HANDLER_SYM table_ident_nodb CLOSE_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("close", "close"));
      $$= new_tree_item("handler", "", p);
    }
	| HANDLER_SYM table_ident_nodb READ_SYM handler_read_or_scan where_clause opt_limit_clause 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      tree_item_list_add(p, new_simple_tree_item("read", "read"));
      tree_item_list_add(p, $4);
      tree_item_list_add(p, $5);
      tree_item_list_add(p, $6);
      $$= new_tree_item("handler", "", p);
	  }
  ;

handler_read_or_scan:
	  handler_scan_function         
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("handler_read_or_scan", "", p);
	  }
  | ident handler_rkey_function 
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, $2);
      $$= new_tree_item("handler_read_or_scan", "", p);
    }
  ;

handler_scan_function:
	  FIRST_SYM  
	  {
	    $$= new_simple_tree_item("first", "first");
	  }
	| NEXT_SYM 
	  {
	    $$= new_simple_tree_item("next", "next");
	  }
  ;

handler_rkey_function:
	  FIRST_SYM  
	  {
	    $$= new_simple_tree_item("first", "first");
	  }
	| NEXT_SYM 
	  {
	    $$= new_simple_tree_item("next", "next");
	  }
	| PREV_SYM 
	  {
	    $$= new_simple_tree_item("prev", "prev");
	  }
	| LAST_SYM 
	  {
	    $$= new_simple_tree_item("last", "last");
	  }
	| handler_rkey_mode LP_SYM values RP_SYM 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $3);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("handler_rkey_function", "", p);
	  }
  ;

handler_rkey_mode:
	  EQ
	  {
	    $$= new_simple_tree_item("eq", "=");
	  }     
	| GE     
	  {
	    $$= new_simple_tree_item("ge", ">=");
	  }     
	| LE     
	  {
	    $$= new_simple_tree_item("le", "<=");
	  }     
	| GT_SYM 
	  {
	    $$= new_simple_tree_item("gt", ">");
	  }     
	| LT     
	  {
	    $$= new_simple_tree_item("lt", "<");
	  }     
  ;

/* GRANT / REVOKE */

revoke:
	  REVOKE clear_privileges revoke_command
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3);
	    $$= new_tree_item("revoke", "", p);
	  }
  ;

revoke_command:
	  grant_privileges ON opt_table grant_ident FROM grant_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    $$= new_tree_item("revoke_command", "", p);
    }
  | grant_privileges ON FUNCTION_SYM grant_ident FROM grant_list
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("function", "function"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    $$= new_tree_item("revoke_command", "", p);
    }
	| grant_privileges ON PROCEDURE grant_ident FROM grant_list
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    $$= new_tree_item("revoke_command", "", p);
    }
	| ALL opt_privileges COMMA_SYM GRANT OPTION FROM grant_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("all", "all"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, new_simple_tree_item("grant", "grant"));
	    tree_item_list_add(p, new_simple_tree_item("option", "option"));
	    tree_item_list_add(p, new_simple_tree_item("from", "from"));
	    tree_item_list_add(p, $7);
	    $$= new_tree_item("revoke_command", "", p);
    }
	;

grant:
	  GRANT clear_privileges grant_command
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("grant", "", p);
	  }
  ;

grant_command:
	  grant_privileges ON opt_table grant_ident TO_SYM grant_list require_clause grant_options
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    if($3 != NULL)
	    {
	      tree_item_list_add(p, $3);
	    }
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $6);
	    tree_item_list_add(p, $7);
	    if($8 != NULL)
	    {
	      tree_item_list_add(p, $8);
	    }
	    $$= new_tree_item("grant_command", "", p);
    }
  | grant_privileges ON FUNCTION_SYM grant_ident TO_SYM grant_list require_clause grant_options
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("function", "function"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $6);
	    tree_item_list_add(p, $7);
	    if($8 != NULL)
	    {
	      tree_item_list_add(p, $8);
	    }
	    $$= new_tree_item("grant_command", "", p);
    }
  | grant_privileges ON PROCEDURE grant_ident TO_SYM grant_list require_clause grant_options
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item("on", "on"));
	    tree_item_list_add(p, new_simple_tree_item("procedure", "procedure"));
	    tree_item_list_add(p, $4);
	    tree_item_list_add(p, new_simple_tree_item("to", "to"));
	    tree_item_list_add(p, $6);
	    tree_item_list_add(p, $7);
	    if($8 != NULL)
	    {
	      tree_item_list_add(p, $8);
	    }
	    $$= new_tree_item("grant_command", "", p);
    }
  ;

opt_table:
	  /* Empty */
	  {
	    $$= NULL;
	  }
	| TABLE_SYM 
	  {
	    $$= new_simple_tree_item("table", "table");
	  }
	;
        
grant_privileges:
	  object_privilege_list 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("grant_privileges", "", p);
	  }
	| ALL opt_privileges
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("all", "all"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("grant_privileges", "", p);
    }
  ;

opt_privileges:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| PRIVILEGES
	  {
	    $$= new_simple_tree_item("privileges", "privileges");
	  }
	;

object_privilege_list:
	  object_privilege
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("object_privilege_list", "", p);
	  }
	| object_privilege_list COMMA_SYM object_privilege
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("object_privilege_list", "", p);
	  }
	;

object_privilege:
	  SELECT_SYM opt_column_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("select", "select"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| INSERT opt_column_list 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("insert", "insert"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| UPDATE_SYM opt_column_list 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("update", "update"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| REFERENCES opt_column_list 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("references", "references"));
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| DELETE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("delete", "delete"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| USAGE		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("usage", "usage"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| INDEX_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("index", "index"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| ALTER
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("alter", "alter"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| CREATE	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| DROP
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("drop", "drop"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| EXECUTE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("execute", "execute"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| RELOAD	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("reload", "reload"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| SHUTDOWN	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("shutdown", "shutdown"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| PROCESS	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("process", "process"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| FILE_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("file", "file"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| GRANT OPTION  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("grant", "grant"));
	    tree_item_list_add(p, new_simple_tree_item("option", "option"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| SHOW DATABASES 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("show", "show"));
	    tree_item_list_add(p, new_simple_tree_item("databases", "databases"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| SUPER_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("super", "super"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| CREATE TEMPORARY TABLES 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("temporary", "temporary"));
	    tree_item_list_add(p, new_simple_tree_item("tables", "tables"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| LOCK_SYM TABLES   
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lock", "lock"));
	    tree_item_list_add(p, new_simple_tree_item("tables", "tables"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| REPLICATION SLAVE  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("replication", "replication"));
	    tree_item_list_add(p, new_simple_tree_item("slave", "slave"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| REPLICATION CLIENT_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("replication", "replication"));
	    tree_item_list_add(p, new_simple_tree_item("client", "client"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| CREATE VIEW_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("view", "view"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| SHOW VIEW_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("show", "show"));
	    tree_item_list_add(p, new_simple_tree_item("view", "view"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| CREATE ROUTINE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("routine", "routine"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| ALTER ROUTINE_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("alter", "alter"));
	    tree_item_list_add(p, new_simple_tree_item("routine", "routine"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	| CREATE USER 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("create", "create"));
	    tree_item_list_add(p, new_simple_tree_item("user", "user"));
	    $$= new_tree_item("object_privilege", "", p);
	  }
	;


opt_and:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| AND_SYM	
	  {
	    $$= new_simple_tree_item("and", "and");
	  }
	;

require_list:
	  require_list_element opt_and require_list
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    tree_item_list_add_all(p, $3);
	    delete_tree_item($3);
	    $$= new_tree_item("require_list", "", p);
	  }
  | require_list_element
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("require_list", "", p);
    }
  ;

require_list_element:
	  SUBJECT_SYM STRING
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("subject", "subject"));
	    tree_item_list_add(p, new_simple_tree_item("string", "string"));
	    $$= new_tree_item("require_list_element", "", p);
    }
	| ISSUER_SYM STRING
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("issuer", "issuer"));
	    tree_item_list_add(p, new_simple_tree_item("string", "string"));
	    $$= new_tree_item("require_list_element", "", p);
    }
	| CIPHER_SYM STRING
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("cipher", "cipher"));
	    tree_item_list_add(p, new_simple_tree_item("string", "string"));
	    $$= new_tree_item("require_list_element", "", p);
    }
	;

grant_ident:
	  MULL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("grant_ident", "", p);
    }
	| ident DOT_SYM MULL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("grant_ident", "", p);
    }
	| MULL_SYM DOT_SYM MULL_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    tree_item_list_add(p, new_simple_tree_item(".", "."));
	    tree_item_list_add(p, new_simple_tree_item("*", "*"));
	    $$= new_tree_item("grant_ident", "", p);
    }
	| table_ident
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("grant_ident", "", p);
    }
  ;

user_list:
	  user  
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("user_list", "", p);
	  }
	| user_list COMMA_SYM user
	  {
	    void *p= new_tree_item_list_reuse($1);
	    delete_tree_item($1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("user_list", "", p);
    }
	;

grant_list:
	  grant_user  
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("grant_list", "", p);
	  }
	| grant_list COMMA_SYM grant_user
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);
      $$= new_tree_item("grant_list", "", p);
    }
	;

grant_user:
	  user IDENTIFIED_SYM BY STRING
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("identified", "identified"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, new_simple_tree_item("string", $4));
      $$= new_tree_item("grant_user", "", p);
    }
	| user IDENTIFIED_SYM BY PASSWORD STRING
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("identified", "identified"));
      tree_item_list_add(p, new_simple_tree_item("by", "by"));
      tree_item_list_add(p, new_simple_tree_item("password", "password"));
      tree_item_list_add(p, new_simple_tree_item("string", $4));
      $$= new_tree_item("grant_user", "", p);
	  }
	| user
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("grant_user", "", p);
	  }
  ;

opt_column_list:
	  /* empty */
	  {
      $$= NULL;
    }
	| LP_SYM column_list RP_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("lp", "("));
      tree_item_list_add(p, $1);
      tree_item_list_add(p, new_simple_tree_item("rp", ")"));
      $$= new_tree_item("opt_column_list", "", p);
	  }
	;

column_list:
	  column_list COMMA_SYM column_list_id
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, new_simple_tree_item(",", ","));
      tree_item_list_add(p, $3);      
      $$= new_tree_item("column_list", "", p);
	  }
	| column_list_id
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("column_list", "", p);
	  }
	;

column_list_id:
	  ident
	  {
      $$= $1;
    }
  ;


require_clause: 
    /* empty */
    {
      $$= NULL;
    }
  | REQUIRE_SYM require_list
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("require", "require"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("require_clause", "", p);
    }
  | REQUIRE_SYM SSL_SYM
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("require", "require"));
      tree_item_list_add(p, new_simple_tree_item("ssl", "ssl"));
      $$= new_tree_item("require_clause", "", p);
    }
  | REQUIRE_SYM X509_SYM
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("require", "require"));
      tree_item_list_add(p, new_simple_tree_item("x509", "x509"));
      $$= new_tree_item("require_clause", "", p);
    }
	| REQUIRE_SYM NONE_SYM
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("require", "require"));
      tree_item_list_add(p, new_simple_tree_item("none", "none"));
      $$= new_tree_item("require_clause", "", p);
    }
  ;

grant_options:
	  /* empty */ 
	  {
      $$= NULL;
	  }
	| WITH grant_option_list
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("with", "with"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_options", "", p);
	  }
	;

grant_option_list:
	  grant_option_list grant_option 
	  {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_option_list", "", p);
	  }
	| grant_option 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("grant_option_list", "", p);
	  }
  ;

grant_option:
	  GRANT OPTION 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("grant", "grant"));
      tree_item_list_add(p, new_simple_tree_item("option", "option"));
      $$= new_tree_item("grant_option_list", "", p);
	  }
  | MAX_QUERIES_PER_HOUR ulong_num
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("max_queries_per_hour", "max_queries_per_hour"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_option_list", "", p);
	  }
  | MAX_UPDATES_PER_HOUR ulong_num
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("max_updates_per_hour", "max_updates_per_hour"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_option_list", "", p);
	  }
  | MAX_CONNECTIONS_PER_HOUR ulong_num
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("max_connections_per_hour", "max_connections_per_hour"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_option_list", "", p);
	  }
  | MAX_USER_CONNECTIONS_SYM ulong_num
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("max_user_connections", "max_user_connections"));
      tree_item_list_add(p, $2);
      $$= new_tree_item("grant_option_list", "", p);
	  }
  ;

begin:
	  BEGIN_SYM opt_work 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("begin", "begin"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("begin", "", p);
	  }
	;

opt_work:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| WORK_SYM  
	  {
	    $$= new_simple_tree_item("work", "work");
	  }
  ;

opt_chain:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| AND_SYM NO_SYM CHAIN_SYM	
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, new_simple_tree_item("no", "no"));
	    tree_item_list_add(p, new_simple_tree_item("chain", "chain"));
	    $$= new_tree_item("opt_chain", "", p);
	  }
	| AND_SYM CHAIN_SYM		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("and", "and"));
	    tree_item_list_add(p, new_simple_tree_item("chain", "chain"));
	    $$= new_tree_item("opt_chain", "", p);
	  }
	;

opt_release:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| RELEASE_SYM 			
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("release", "release"));
	    $$= new_tree_item("opt_release", "", p);
	  }
	| NO_SYM RELEASE_SYM 		
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("no", "no"));
	    tree_item_list_add(p, new_simple_tree_item("release", "release"));
	    $$= new_tree_item("opt_release", "", p);
	  }
	;
	
opt_savepoint:
	  /* empty */	
	  {
	    $$= NULL;
	  }
	| SAVEPOINT_SYM 
	  {
	    $$= new_simple_tree_item("savepoint", "savepoint");
	  }
	;

commit:
	  COMMIT_SYM opt_work opt_chain opt_release
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("commit", "", p);
    }
	;

rollback:
  	ROLLBACK_SYM opt_work opt_chain opt_release
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      if($3 != NULL)
      {
        tree_item_list_add(p, $3);
      }
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      $$= new_tree_item("rollback", "", p);
    }
	| ROLLBACK_SYM opt_work TO_SYM opt_savepoint ident
	  {
      void *p= new_tree_item_list();
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      tree_item_list_add(p, new_simple_tree_item("to", "to"));
      if($4 != NULL)
      {
        tree_item_list_add(p, $4);
      }
      tree_item_list_add(p, $5);
      $$= new_tree_item("rollback", "", p);
    }
	;

savepoint:
	  SAVEPOINT_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $2);
      $$= new_tree_item("savepoint", "", p);
    }
	;

release:
	  RELEASE_SYM SAVEPOINT_SYM ident
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("savepoint", "savepoint"));
      tree_item_list_add(p, $3);
      $$= new_tree_item("release", "", p);
    }
	;
  
/*
   UNIONS : glue selects together
*/


union_clause:
	/* empty */ 
	  {
	    $$= NULL;
	  }
	| union_list
	  {
	    $$= $1;	    
	  }
	;

union_list:
	  UNION_SYM union_option select_init 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("union", "union"));
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("union_list", "", p);
    }
	;

union_opt:
	  /* Empty */ 
	  {
	    $$= NULL;
	  }
	| union_list 
	  {
	    $$= $1;
	  }
	| union_order_or_limit 
	  {
	    $$= $1;
	  }
	;

union_order_or_limit:
	  order_or_limit
    {
      $$= $1;
    }
	;

order_or_limit:
	  order_clause opt_limit_clause_init
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    if($2 != NULL)
	    {
	      tree_item_list_add(p, $2);
	    }
	    $$= new_tree_item("order_or_limit", "", p);
	  }
	| limit_clause
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("order_or_limit", "", p);
	  }
	;

union_option:
	/* empty */ 
	  {
	    $$= NULL;
	  }
	| DISTINCT  
	  {
	    $$= new_simple_tree_item("distinct", "distinct");
	  }
	| ALL
	  {
	    $$= new_simple_tree_item("distinct", "all");
	  }
  ;

singlerow_subselect:
	subselect_start singlerow_subselect_init
	subselect_end
	{

};

singlerow_subselect_init:
	select_init2
	{


};

exists_subselect:
	  subselect_start exists_subselect_init subselect_end {}
	;

exists_subselect_init:
	  select_init2
	  {
    }
  ;

in_subselect:
    subselect_start in_subselect_init subselect_end 
    {
      void *p= new_tree_item_list_reuse($1);
      delete_tree_item($1);
      tree_item_list_add(p, $2);
      tree_item_list_add(p, $3);
      $$= new_tree_item("in_subselect", "", p);
    }
  ;

in_subselect_init:
    select_init2 
    {
	    void *p= new_tree_item_list_reuse($1); 
	    delete_tree_item($1);
	    $$= new_tree_item("select", "", p); 
    }
  ;

subselect_start:
	  LP_SYM SELECT_SYM 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    //tree_item_list_add(p, new_simple_tree_item("select", $2));
	    $$= new_tree_item("subselect_start", "", p);
	  }
	;

subselect_end:
	  RP_SYM 
	  {
	    $$= new_simple_tree_item("rp", $1);
	  }
	;

opt_view_list:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| LP_SYM view_list RP_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("lp", "("));
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, new_simple_tree_item("rp", ")"));
	    $$= new_tree_item("opt_view_list", "", p);
	  }
	;

view_list:
	  ident 
	  {
      void *p= new_tree_item_list();
      tree_item_list_add(p, $1);
      $$= new_tree_item("view_list", "", p);
    }
	| view_list COMMA_SYM ident
	  {
    void *p= new_tree_item_list_reuse($1);
    delete_tree_item($1);
    tree_item_list_add(p, new_simple_tree_item(",", ","));
    tree_item_list_add(p, $3);
    $$= new_tree_item("view_list", "", p);
    }
	;

or_replace:
	  /* empty */ 
	  {
	    $$= NULL;
	  }
	| OR_SYM REPLACE 
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("or", "or"));
	    tree_item_list_add(p, new_simple_tree_item("replace", "replace"));
	    $$= new_tree_item("or_replace", "", p);
	  }
	;

algorithm:
	  /* empty */
	  {
	    $$= NULL;
	  }
	| ALGORITHM_SYM EQ UNDEFINED_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("algorithm", "algorithm"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, new_simple_tree_item("undefined", "undefined"));
	    $$= new_tree_item("algorithm", "", p);
	  }
	| ALGORITHM_SYM EQ MERGE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("algorithm", "algorithm"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, new_simple_tree_item("merge", "merge"));
	    $$= new_tree_item("algorithm", "", p);
	  }
	| ALGORITHM_SYM EQ TEMPTABLE_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("algorithm", "algorithm"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, new_simple_tree_item("temptable", "temptable"));
	    $$= new_tree_item("algorithm", "", p);
	  }
	;

view_user:
    /* empty */
    {
      $$= NULL;
    }
  | CURRENT_USER optional_braces
    {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("current_user", "current_user"));
	    $$= new_tree_item("view_user", "", p);
    }
  | DEFINER_SYM EQ ident_or_text AT_SYM ident_or_text
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("definer", "definer"));
	    tree_item_list_add(p, new_simple_tree_item("eq", "="));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item("at", "@"));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("view_user", "", p);
    }
  ;

view_suid:
    /* empty */
	  {
	    $$= NULL;
	  }
  | SQL_SYM SECURITY_SYM DEFINER_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("security", "security"));
	    tree_item_list_add(p, new_simple_tree_item("definer", "definer"));
	    $$= new_tree_item("view_suid", "", p);
	  }
	| SQL_SYM SECURITY_SYM INVOKER_SYM
	  {
	    void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("sql", "sql"));
	    tree_item_list_add(p, new_simple_tree_item("security", "security"));
	    tree_item_list_add(p, new_simple_tree_item("invoker", "invoker"));
	    $$= new_tree_item("view_suid", "", p);
	  }
	;

check_option:
  /* empty */
    {
      $$= NULL;
    }
  | WITH CHECK_SYM OPTION
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("check", "check"));
	    tree_item_list_add(p, new_simple_tree_item("option", "option"));
	    $$= new_tree_item("check_option", "", p);
    }
  | WITH CASCADED CHECK_SYM OPTION
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("cascaded", "cascaded"));
	    tree_item_list_add(p, new_simple_tree_item("check", "check"));
	    tree_item_list_add(p, new_simple_tree_item("option", "option"));
	    $$= new_tree_item("check_option", "", p);
    }
  | WITH LOCAL_SYM CHECK_SYM OPTION
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("with", "with"));
	    tree_item_list_add(p, new_simple_tree_item("local", "local"));
	    tree_item_list_add(p, new_simple_tree_item("check", "check"));
	    tree_item_list_add(p, new_simple_tree_item("option", "option"));
	    $$= new_tree_item("check_option", "", p);
    }
  ;

xa: 
    XA_SYM begin_or_start xid opt_join_or_resume
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $2);
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("xa", "", p);
    }
  | XA_SYM END xid opt_suspend
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("end", "end"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("xa", "", p);
    }
  | XA_SYM PREPARE_SYM xid
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("prepare", "prepare"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("xa", "", p);
    }
  | XA_SYM COMMIT_SYM xid opt_one_phase
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("commit", "commit"));
	    tree_item_list_add(p, $3);
	    if($4 != NULL)
	    {
	      tree_item_list_add(p, $4);
	    }
	    $$= new_tree_item("xa", "", p);
    }
  | XA_SYM ROLLBACK_SYM xid
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("rollback", "rollback"));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("xa", "", p);
    }
  | XA_SYM RECOVER_SYM
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, new_simple_tree_item("recover", "recover"));
	    $$= new_tree_item("xa", "", p);
    }
  ;

xid: 
    text_string
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    $$= new_tree_item("xid", "", p);
    }
  | text_string COMMA_SYM text_string
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    $$= new_tree_item("xid", "", p);
    }
  | text_string COMMA_SYM text_string COMMA_SYM ulong_num
    {
      void *p= new_tree_item_list();
	    tree_item_list_add(p, $1);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $3);
	    tree_item_list_add(p, new_simple_tree_item(",", ","));
	    tree_item_list_add(p, $5);
	    $$= new_tree_item("xid", "", p);
    }
  ;

begin_or_start:   
    BEGIN_SYM 
    {
      $$= new_simple_tree_item("begin", "begin");
    }
  | START_SYM 
    {
      $$= new_simple_tree_item("start", "start");
    }
  ;

opt_join_or_resume:
    /* nothing */           
    {
      $$= NULL;
    }
  | JOIN_SYM              
    {
      $$= new_simple_tree_item("join", "join");
    }
  | RESUME_SYM            
    {
      $$= new_simple_tree_item("resume", "resume");
    }
  ;

opt_one_phase:
    /* nothing */           
    {
      $$= NULL;
    }
  | ONE_SYM PHASE_SYM     
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("one", "one"));
      tree_item_list_add(p, new_simple_tree_item("phase", "phase"));
      $$= new_tree_item("opt_one_phase", "", p);
    }
  ;

opt_suspend:
    /* nothing */           
    {
      $$= NULL;
    }
  | SUSPEND_SYM opt_migrate
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("suspend", "suspend"));
      if($2 != NULL)
      {
        tree_item_list_add(p, $2);
      }
      $$= new_tree_item("opt_suspend", "", p);
    }
  ;

opt_migrate:
    /* nothing */           
    {
      $$= NULL;
    }
  | FOR_SYM MIGRATE_SYM   
    {
      void *p= new_tree_item_list();
      tree_item_list_add(p, new_simple_tree_item("for", "for"));
      tree_item_list_add(p, new_simple_tree_item("migrate", "migrate"));
      $$= new_tree_item("opt_suspend", "", p);
    }
  ;


