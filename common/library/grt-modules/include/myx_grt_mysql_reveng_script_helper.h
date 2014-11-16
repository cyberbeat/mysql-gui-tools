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

#ifndef __MYX_GRT_MYSQL_REVENG_SCRIPT_HELPER_H_
#define __MYX_GRT_MYSQL_REVENG_SCRIPT_HELPER_H_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _HELPER_ARGS
{
  MYX_GRT_VALUE * catalog;
  MYX_GRT *grt;
  MYX_GRT_VALUE * used_schema;
  const char *sql;
} 
HELPER_ARGS;

void convert_parser_dom_to_grt_dom(void *parser_dom_tree, HELPER_ARGS * args);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // __MYX_GRT_MYSQL_REVENG_SCRIPT_HELPER_H_

