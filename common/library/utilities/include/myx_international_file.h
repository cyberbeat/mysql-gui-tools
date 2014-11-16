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

#ifndef myx_international_file_h
#define myx_international_file_h

/*
 * Structs
 */

#include <myx_util_functions.h>

///////////////////////////////////////////////////////////////////////////////
/** @brief International File.
*//////////////////////////////////////////////////////////////////////////////
#define BUFFER_LEN 1000000
#define MAX_BYTES_PER_CHARACTER 6
#define DETECT_CHARSET_FROM_SQL_FILE_BUFFER_SIZE 4096
  
struct myx_intl_file
{
  FILE *file;           // the file descriptor for filename
  const char *filename; // the file we are reading from
  const char *charset;  // the encoding of the file
  gboolean charset_is_utf8; // we have this to avoid costly string comparisons
                            // for every call of myx_read_char_from_intl_file
  iconv_t conv_to_utf8, conv_from_utf8;

  // untranslated line
  unsigned char buffer[BUFFER_LEN + MAX_BYTES_PER_CHARACTER]; 
  size_t buffer_len_in_bytes;

  gsize bytes_translated; // the number of bytes that really got translated
                          //   from buffer to utf8_buffer

  gchar *utf8_buffer;    // translated line, may not be null-terminated
  size_t utf8_buffer_len_in_bytes;
  gchar *next_utf8_char; // points to the character that is going to be
                         //   returned on the next call to 
                         //   myx_read_char_from_file
  bigint file_pos; // the number of bytes that have already been
                   // read from this file
};

#endif

