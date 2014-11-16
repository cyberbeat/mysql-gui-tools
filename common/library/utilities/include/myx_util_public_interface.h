/* Copyright (C) 2005 MySQL AB

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA */

#ifndef myx_util_public_interface_h
#define myx_util_public_interface_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include <myx_util_functions.h>
#include <myx_international_file.h>


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

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlutil"
#define libmysqlutil_PUBLIC_INTERFACE_VERSION 10001


/*
 * Enums
 */

// When this type is changed then also the error mapper in myx_backup.c must be adjusted!
typedef enum myx_lib_error
{
    MYX_NO_ERROR = 0, MYX_ERROR_CANT_OPEN_FILE, MYX_ERROR_CANT_CONNECT_TO_INSTANCE, MYX_XML_PARSE_ERROR,
    MYX_XML_NO_VALID_DOCUMENT, MYX_XML_EMPTY_DOCUMENT, MYX_SQL_ERROR, MYX_STOP_EXECUTION, MYX_ALLOC_CHANGE_ERROR,
    MYX_OBJECT_NOT_FOUND, MYX_CANT_READ_FROM_FILE, MYX_CHARSET_CONVERSION_ERROR,MYX_CHARSET_WRONG_CHARSET_SPECIFIED,
    MYX_MEMORY_LIMIT_EXCEEDED, MYX_OUT_OF_MEMORY
} MYX_LIB_ERROR;

/*
 * Structs
 */

#ifndef MYX_STRINGLIST_DECLARED
/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_STRINGLIST
typedef struct {
    unsigned int strings_num;
    char **strings;
} MYX_STRINGLIST;
#define MYX_STRINGLIST_DECLARED
#endif

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_NAME_VALUE_PAIR
typedef struct {
  char *name;
  char *value;
} MYX_NAME_VALUE_PAIR;

// ------------------------------
// International File

typedef struct myx_intl_file MYX_INTL_FILE;

/*
 * Functions
 */

MYX_PUBLIC_FUNC int myx_get_util_public_interface_version();

/* International file functions */
MYX_PUBLIC_FUNC MYX_INTL_FILE * myx_new_intl_file(const char *filename, const char *charset, MYX_LIB_ERROR *error);
MYX_PUBLIC_FUNC void myx_free_intl_file(MYX_INTL_FILE *intl_file);
MYX_PUBLIC_FUNC bigint myx_read_char_from_intl_file(MYX_INTL_FILE *intl_file, int *bytes_read, MYX_LIB_ERROR *error);

MYX_PUBLIC_FUNC char *myx_intl_fgets(char *string, int n, FILE *stream);
MYX_PUBLIC_FUNC void myx_intl_rewind(FILE *stream);
MYX_PUBLIC_FUNC void myx_intl_skip(FILE *stream, int count);

MYX_PUBLIC_FUNC char * escape_string(const char* source);
MYX_PUBLIC_FUNC char * escape_string_for_search(const char* source);

MYX_PUBLIC_FUNC char* baseconv(ubigint num, int base);

MYX_PUBLIC_FUNC const char *stristr(const char *haystack, const char *needle);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
