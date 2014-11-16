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

#ifndef myx_grt_mysql_h
#define myx_grt_mysql_h

#include <myx_public_interface.h>
#include <myx_grt_public_interface.h>
#include <myx_grt_builtin_module_public_interface.h>

//#include "myx_grt_mysql_reveng.h"
//#include "myx_grt_mysql_reveng_script.h"
//#include "myx_grt_mysql_transformation.h"

#define DBUG(msg) if (getenv("DEBUG_GRT_MYSQL")) g_message("%s", msg)

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Structs and Enums
 */

/*
 * Functions
 */

// GRT return value functions
MYX_GRT_VALUE *make_return_value_mysql_error(MYSQL *mysql, const char *message, const char *details);
MYX_GRT_VALUE *make_return_value_mysql_error_and_close(MYSQL *mysql, const char *message, const char *sql);

// MySQL functions
void get_connection_info(MYX_GRT_VALUE *value, MYX_USER_CONNECTION *conn);
MYSQL * grt_mysql_connect(MYX_GRT_VALUE *param, MYX_GRT_VALUE **retval);
MYX_GRT_VALUE * grt_mysql_execute(MYSQL *mysql, MYSQL_RES **res, const char *sql, char *error_msg);
MYX_GRT_VALUE * grt_mysql_execute_and_free(MYSQL *mysql, MYSQL_RES **res, char *sql, char *error_msg);

#ifdef __cplusplus
};
#endif

#endif
