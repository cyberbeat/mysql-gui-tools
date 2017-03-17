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

#ifndef myx_util_functions_h
#define myx_util_functions_h

#include "myx_util.h"

#include <iconv.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>

#if !defined(__WIN__) && !defined(_WIN32) && !defined(_WIN64)
  #include <stdio.h>
  #include <sys/types.h>
  #include <unistd.h>
  #include <sys/time.h>
#else
  #include <winsock2.h>
  #include <windows.h>
  #include <direct.h>
#endif

#include "glib.h"
#ifndef __BORLANDC__
  // On Windows we use a DLL for PCRE.
  #define PCRE_STATIC
#endif
#include "pcre.h"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  typedef LARGE_INTEGER MYX_TIMER_VALUE;
  //defined in windows
# define MYX_INT_PTR UINT_PTR
#else
  typedef struct timeval MYX_TIMER_VALUE;
# define MYX_INT_PTR intptr_t
#endif


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #define MYX_PUBLIC_FUNC __declspec(dllexport)
  #define _br "\r\n"
  #define MYX_PATH_SEPARATOR '\\'
  #define MYX_PATH_SEPARATOR_STR "\\"
#else
  #define MYX_PUBLIC_FUNC
  #define _br "\n"
  #define MYX_PATH_SEPARATOR '/'
#define MYX_PATH_SEPARATOR_STR "/"
#endif

#define MYX_ORDPTR(value)  ((void*)(unsigned long)(value))

#ifdef __cplusplus
extern "C" {
#endif

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #ifndef strcasecmp
    #define strcasecmp stricmp
    #define strncasecmp strnicmp
  #endif
  #define bigint __int64
  #define ubigint unsigned __int64
#else
  #define bigint long long
  #define ubigint unsigned long long
#endif

// if my_global.h not included
#ifndef my_sprintf
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  typedef unsigned int uint;   
  typedef unsigned char uint8;
  typedef unsigned char uchar;
  typedef signed char int8;                   
  typedef unsigned short uint16;                 
  typedef short int16;                  
  typedef unsigned long uint32;                 
  typedef long int32;   
  typedef unsigned long ulong;
  typedef __int64 longlong;
  typedef unsigned __int64 ulonglong;
#else
// # define min(a,b) ((a)<(b)?(a):(b))
// # define max(a,b) ((a)>(b)?(a):(b))

  typedef unsigned char uint8;
  typedef unsigned char uchar;
  typedef signed char int8;                   
  typedef unsigned short uint16;
  typedef short int16;                  
  typedef unsigned long uint32;                 
  typedef long int32;
  #ifndef __GLIBC__
    typedef unsigned long ulong;
  #endif
  typedef long long longlong;
  typedef unsigned long long ulonglong;
#endif
#endif

/*
 * Enums
 */

/*
 * Structs
 */

/*
 * Functions
 */
MYX_PUBLIC_FUNC char* hex_decode(const char *hex_str, int *ret_str_len);
MYX_PUBLIC_FUNC char *hex_encode(const char *binary_string, int len);
MYX_PUBLIC_FUNC char *iconv_char_name(const char *mysql_character_set_name);
MYX_PUBLIC_FUNC char *unquote_identifier(char *identifier);
MYX_PUBLIC_FUNC char *quote_identifier(const char *identifier, const char quote_char);
MYX_PUBLIC_FUNC int split_schema_table(const char *ident, char **schema, char **table);
MYX_PUBLIC_FUNC char *mask_out_string(char *str, const char open_trigger, const char close_trigger, const char mask);
MYX_PUBLIC_FUNC char *mask_out_string_re(char *str, const char *open_re, const char open_trigger, const char close_trigger, const char mask);
MYX_PUBLIC_FUNC char *str_g_replace(char *str, const char *search, const char *replace);
MYX_PUBLIC_FUNC char *str_g_subst(const char *str, const char *search, const char *replace);
MYX_PUBLIC_FUNC char *str_g_append(char *base_str, const char *addon);
MYX_PUBLIC_FUNC char *str_g_append_and_free(char *base_str, char *addon);
MYX_PUBLIC_FUNC char *str_g_insert(const char *base_str, const char *addon, unsigned int index);

MYX_PUBLIC_FUNC char *strmov(register char *dst, register const char *src);
MYX_PUBLIC_FUNC int strcmp2(const char *str1, const char *str2);
MYX_PUBLIC_FUNC int strcmp3(const char *str1, const char *str2);
MYX_PUBLIC_FUNC unsigned int strlen2(const char *str);
MYX_PUBLIC_FUNC char *strcpy2(char *dst, const char *src);
MYX_PUBLIC_FUNC char *utf8_str_trim(char *str);
MYX_PUBLIC_FUNC char *str_trim(char *str);
MYX_PUBLIC_FUNC char *str_left(char *dest, char *src, unsigned int len);
MYX_PUBLIC_FUNC char *str_right(char *dest, char *src, unsigned int len);
MYX_PUBLIC_FUNC char *str_align_left(const char *txt, unsigned int width, char linechar);
MYX_PUBLIC_FUNC char *str_align_right(const char *txt, unsigned int width, char linechar);
MYX_PUBLIC_FUNC char *str_align_center(const char *txt, unsigned int width, char linechar);
MYX_PUBLIC_FUNC char *auto_line_break(const char *txt, unsigned int width, char sep);
MYX_PUBLIC_FUNC int sub_str_count(char *search_str, const char *src);
MYX_PUBLIC_FUNC char *name_of_str(char *dst, const char *src);
MYX_PUBLIC_FUNC char *value_of_str(char *dst, const char *src);
MYX_PUBLIC_FUNC int set_value(char **string_list, unsigned int string_list_num, char *name, char *new_value);
MYX_PUBLIC_FUNC int get_str_index(char **string_list, unsigned int string_list_num, const char *search);

MYX_PUBLIC_FUNC int strlist_g_indexof(const char **list, const char *value);
MYX_PUBLIC_FUNC void strlist_g_append(char ***list, char *value);
MYX_PUBLIC_FUNC void strlist_g_append_or_replace(char ***list, char *value);

MYX_PUBLIC_FUNC int g_utf8_casecollate(const char *str1, const char *str2);

MYX_PUBLIC_FUNC int str_beginswith(const char *str, const char *substr);
MYX_PUBLIC_FUNC int str_endswith(const char *str, const char *substr);
MYX_PUBLIC_FUNC char *str_toupper(char *str);
MYX_PUBLIC_FUNC int str_is_numeric(const char *str);

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
MYX_PUBLIC_FUNC int get_value_from_registry(HKEY root_key, const char *sub_key, const char *key, const char *def, char *value);
MYX_PUBLIC_FUNC int set_value_to_registry(HKEY root_key, const char *sub_key, const char *key, const char *value);
#else
FILE *myx_popen(char *const argv[], pid_t *pid_ret);
int myx_pclose(FILE *f, pid_t pid);
int myx_read_timeout(FILE *f, int timeout, char *result, size_t result_len);
#endif

MYX_PUBLIC_FUNC void set_os_specific_password_functions(char*(*store_func)(const char *host,
                                                                           const char *username,
                                                                           const char *password),
                                                        char*(*retrieve_func)(const char *host,
                                                                              const char *username,
                                                                              const char *password_data));


MYX_PUBLIC_FUNC char *get_local_os_name(void);
MYX_PUBLIC_FUNC char *get_local_hardware_info(void);

MYX_PUBLIC_FUNC bigint get_physical_memory_size(void);

MYX_PUBLIC_FUNC int copy_file(const char *orig_file, const char *new_fil);
  
MYX_PUBLIC_FUNC bigint get_file_size(const char *filename); 

MYX_PUBLIC_FUNC char *strcasestr_len(const char *haystack, int haystack_len, const char *needle);

MYX_PUBLIC_FUNC char * get_value_from_text(const char *txt, int txt_length, const char *regexpr);
MYX_PUBLIC_FUNC char * get_value_from_text_ex(const char *txt, int txt_length, const char *regexpr, unsigned int substring_nr);
MYX_PUBLIC_FUNC char * get_value_from_text_ex_opt(const char *txt, 
                                                  int txt_length,
                                                  const char *regexpr,
                                                  unsigned int substring_nr,
                                                  int options_for_exec);

MYX_PUBLIC_FUNC const char *strfindword(const char *str, const char *word);

MYX_PUBLIC_FUNC char *subst_pcre(const char *pattern, const char *repl, 
                                 int flags, int max_matches,
                                 const char *string);

MYX_PUBLIC_FUNC char *subst_pcre_matches(const char *src, int *matches, int matchcount, const char *repl);


MYX_PUBLIC_FUNC void timer_start(MYX_TIMER_VALUE *tval);
MYX_PUBLIC_FUNC double timer_stop(MYX_TIMER_VALUE *tval);

MYX_PUBLIC_FUNC void *vec_insert_resize(void *vec, guint size, guint *vecsize, guint pos, void *data);
MYX_PUBLIC_FUNC void *vec_remove(void *vec, guint size, guint *vecsize, guint pos);

MYX_PUBLIC_FUNC char *escape_xml_entities(const char *str);
MYX_PUBLIC_FUNC char *escape_html_entities(const char *str);
MYX_PUBLIC_FUNC char *unescape_html_entities(const char *str);

MYX_PUBLIC_FUNC int myx_mkdir(const char *filename, int mode, int *error_no);
MYX_PUBLIC_FUNC int myx_chdir(const char *path);

MYX_PUBLIC_FUNC int check_file_exists(const char *filename);

#ifdef __cplusplus
};
#endif

#endif
