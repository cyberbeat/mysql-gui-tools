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

#ifndef myx_library_h
#define myx_library_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #define __LCC__
  #define _WINDOWS
#endif

#include <string.h>
#include <assert.h>
#include <ctype.h>

#include "myx_public_interface.h"
#include "myx_util_functions.h"
#include "myx_shared_util_functions.h"

#define DBSTR(mysql, str, length) myx_convert_dbstr_utf8(mysql, str, length)

/*
 * Enums
 */


/*
 * Structs
 */

typedef struct {
  MYX_QUERY_CALLBACK pre_query_hook;
  MYX_QUERY_CALLBACK post_query_hook;
  void *client_data;
  unsigned int resultset_memory_limit; // 0 for no limit, > 0 for xMB of limit
  char **schema_stack;
  short minor_version;
  short major_version;
  short patchlevel;
  char quote_char;
  short embedded;
  char *charset;
  int case_sensitive;
} MYX_MYSQL;


/*
 * Functions
 */

//----------------------------------------------------------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

MYX_MYSQL *myx_mysql_get_private(MYSQL *mysql);
void myx_get_mysql_version(MYSQL *mysql);
const char * myx_get_server_charset_name(MYSQL *mysql);
char * try_convert_from_cs_to_utf8(const char * str, const char * charsetname, const int length);

char * myx_dbm_get_sql_option_create_code(MYX_DBM_TABLE_DATA *table, MYX_DBM_SERVER_VERSION *version);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif

