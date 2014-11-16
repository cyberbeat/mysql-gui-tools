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

#include <stdio.h>
#include <myx_util_functions.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
 
  int state;

  bigint bytes_read;
  bigint chars_read;
  int line_num;

  bigint stmt_begin_char; // stmt_begin in chars
  bigint stmt_end_char;
  int stmt_begin_line;    // stmt_begin in lines
  int stmt_end_line;

  char *alt_delimiter;
} MYX_SQL_PARSE_ENVIRONMENT;


typedef struct {

  char *string;
  int pos; // index of the beginning of the character that is going to be
              //   returned next
} MYX_STRING_WRAPPER;


MYX_PUBLIC_FUNC 
               void myx_init_sql_parse_environment(MYX_SQL_PARSE_ENVIRONMENT*);
MYX_PUBLIC_FUNC
     void myx_init_sql_parse_environment_fill_zero(MYX_SQL_PARSE_ENVIRONMENT*);
MYX_PUBLIC_FUNC
               void myx_done_sql_parse_environment(MYX_SQL_PARSE_ENVIRONMENT*);

MYX_PUBLIC_FUNC 
        void myx_init_string_wrapper(const char *string, MYX_STRING_WRAPPER *);
MYX_PUBLIC_FUNC void myx_done_string_wrapper(MYX_STRING_WRAPPER *w);

MYX_PUBLIC_FUNC bigint myx_read_char_from_string(void *file,
                                                 int *bytes_read, MYX_LIB_ERROR *error);

MYX_PUBLIC_FUNC
        int myx_get_next_sql_statement_file(MYX_SQL_PARSE_ENVIRONMENT *we,
                                            MYX_INTL_FILE *file, char **buffer,
                                            int *buffer_len, int resize_buffer,
                                            int (*progress_report)(
                                                           bigint bytes_read,
                                                           bigint bytes_total,
                                                           void *user_data),
                                            bigint file_size,
                                            void *user_data,
                                            MYX_LIB_ERROR *error);

MYX_PUBLIC_FUNC int myx_get_next_sql_statement(MYX_SQL_PARSE_ENVIRONMENT * we,
                                               char **buffer, int *buffer_len,
                                               int resize_buffer, MYX_LIB_ERROR *error,
                                               bigint (*read_char)
                                                  (
                                                    void *user_data,
                                                    int *bytes_read,
                                                    MYX_LIB_ERROR *error
                                                  ),
                                                int (*progress_report)(
                                                           bigint bytes_read,
                                                           bigint bytes_total,
                                                           void *user_data),
                                                bigint file_size,
                                                void* user_data,
                                              void *arg);

MYX_PUBLIC_FUNC void kill_comments(char * masked_sql);
MYX_PUBLIC_FUNC void mask_quotas(char * masked_sql);
MYX_PUBLIC_FUNC void mask_quotas_and_brackets(char * masked_sql);

#ifdef __cplusplus
}
#endif
