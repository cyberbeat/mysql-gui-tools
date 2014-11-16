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

#ifndef myx_sql_parser_public_interface_h
#define myx_sql_parser_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define __LCC__
#endif

#include <stdio.h>

#ifdef __cplusplus

#include <fstream>

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

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlsqlparser"
#define libmysqlsqlparser_PUBLIC_INTERFACE_VERSION 10000

/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\base-library\include\myx_public_interface.h"

/*
 * Typedefs
 */

typedef int (* process_sql_statement_callback)(const char *sql, void *user_data);

/*
 * Defines
 */

/*
 * Enums
 */

typedef enum myx_stmt_parse_mode { 
  MYX_SPM_NORMAL_MODE= 0, 
  MYX_SPM_DELIMS_REQUIRED= 1
} MYX_STMT_PARSE_MODE;

/*
 * Structs and Enums
 */


/*
 * Functions
 */

MYX_PUBLIC_FUNC int myx_process_sql_statements(const char *sql, 
                                               int (* process_sql_statement_callback)(const char *sql, void *user_data), 
                                               void *user_data,
                                               int mode);
MYX_PUBLIC_FUNC int myx_process_sql_statements_from_file(const char *filename, 
                                                int (* process_sql_statement_callback)(const char *sql, void *user_data), 
                                                void *user_data, 
                                                int mode);

MYX_PUBLIC_FUNC void myx_set_parser_source(const char *sql);
MYX_PUBLIC_FUNC void myx_free_parser_source(void);
MYX_PUBLIC_FUNC void myx_parse(void);
MYX_PUBLIC_FUNC void * myx_get_parser_tree(void);

MYX_PUBLIC_FUNC const void * tree_item_get_subitem_by_name(const void * tree,
                                                     const char * name,
                                                     int pos);
MYX_PUBLIC_FUNC const char * tree_item_get_name(const void * tree);
MYX_PUBLIC_FUNC const char * tree_item_get_value(const void * tree);

MYX_PUBLIC_FUNC void tree_item_dump_xml_to_file(const void * tree_item,
                                                const char * filename);

#ifdef __cplusplus
}

MYX_PUBLIC_FUNC void myx_set_parser_input(std::istream *sqlstream);

#endif /* __cplusplus */



#endif
