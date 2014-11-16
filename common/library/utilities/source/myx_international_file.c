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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <glib.h>
#include <iconv.h>
#include "myx_xml_util_functions.h"
#include "myx_util.h"

///////////////////////////////////////////////////////////////////////////////
/** @brief Creates an international file
    @param filename path to file to open
    @param charset name of character set for file
    @param error pointer to error code (it is set if the function returns NULL)
    @return NULL or created MYX_INTL_FILE struct
*//////////////////////////////////////////////////////////////////////////////
MYX_INTL_FILE * myx_new_intl_file(const char *filename,
                                  const char *charset,
                                  MYX_LIB_ERROR *error)
{
  FILE *fh= myx_fopen(filename, "rb");
  if (!fh)
  {
    *error= MYX_ERROR_CANT_OPEN_FILE;
    return NULL;
  }
  else
  {
    MYX_INTL_FILE *file= (MYX_INTL_FILE*) g_malloc0(sizeof(MYX_INTL_FILE));

    file->filename= filename;
    file->charset= charset;
    file->charset_is_utf8=  ( !g_utf8_casecollate(file->charset, "utf8") ||
                              !g_utf8_casecollate(file->charset, "UTF-8"));
    if (!file->charset_is_utf8)
    {
      file->conv_to_utf8=   iconv_open("UTF-8",file->charset);
      file->conv_from_utf8= iconv_open(file->charset,"UTF-8");
    }

    /*
      since utf8_buffer_len_in_bytes is 0, this will cause
      my_read_char_from_intl_file to believe that a new line has to be read in
    */
    file->next_utf8_char= file->utf8_buffer;
    file->file= fh;
    return file;
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief a wrapper around fgets to ensure it is called from the same module as
    myx_new_intl_file(). See fgets() c runtime function description for details
*//////////////////////////////////////////////////////////////////////////////
char *myx_intl_fgets(char *string, int n, FILE *stream)
{
  return fgets(string, n, stream);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief a wrapper around fgets to ensure it is called from the same module as
    myx_new_intl_file(). See rewind() c runtime function description for details
*//////////////////////////////////////////////////////////////////////////////
void myx_intl_rewind(FILE *stream)
{
  rewind(stream);
}

/**
 * Skips a certain amount of bytes in the file.
 *
 * @param stream The file to work on.
 * @param count The number of bytes to skip.
*/
MYX_PUBLIC_FUNC void myx_intl_skip(FILE *stream, int count)
{
  fseek(stream, count, SEEK_CUR);
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free memory for the International File
    @param file pointer to struct with International File to free (may be null)
    @return 0 always
*//////////////////////////////////////////////////////////////////////////////
void myx_free_intl_file(MYX_INTL_FILE *file)
{
  if (file)
  {
    g_free(file->utf8_buffer);
    if (file->file)
      fclose(file->file);
    //g_free(file); // hmm
    if (!file->charset_is_utf8)
    {
      iconv_close(file->conv_to_utf8);
      iconv_close(file->conv_from_utf8);
    }
    g_free(file);
  }
}

size_t safe_copy_untranslated_characters(MYX_INTL_FILE *file)
{
  if (file->buffer_len_in_bytes <= file->bytes_translated)
  {
    return 0;
  }
  else // this means that we have a partial character at the end
  {
    unsigned char * dst= file->buffer;
    unsigned char * src= file->buffer + file->bytes_translated;
    unsigned char * end= file->buffer + file->buffer_len_in_bytes;
    for (; src<end; src++, dst++)
      *dst= *src;
    return file->buffer_len_in_bytes - file->bytes_translated;
  }
}

int wrong_invalid_chars_at_the_end(MYX_INTL_FILE *file,
                                   char *invalid_chars)
{
  gssize wrong_size=
          (gssize)(file->buffer_len_in_bytes - file->utf8_buffer_len_in_bytes);
  if (wrong_size < MAX_BYTES_PER_CHARACTER &&
      g_utf8_get_char_validated(invalid_chars,wrong_size)== (gunichar)-2)
  {
    return 0;
  }
  else
  {
    assert(g_utf8_get_char_validated(invalid_chars,wrong_size) ==(gunichar)-1);
    return 1;
  }
}

int translate_utf8_buffer(MYX_INTL_FILE * file, MYX_LIB_ERROR * error)
{
  char *invalid_chars= 0;
  if ( g_utf8_validate((char*)file->buffer,
                        (gssize)file->buffer_len_in_bytes,
                        (const gchar**)&invalid_chars) )
  {
    file->utf8_buffer= (char*) g_memdup(file->buffer,
                                            (gssize)file->buffer_len_in_bytes);
    file->utf8_buffer_len_in_bytes= file->buffer_len_in_bytes;
  }
  else // check if we have non utf8 chars or 
  {    //  if our buffer simply ends with a partial character 
    file->utf8_buffer_len_in_bytes= invalid_chars - (char*)file->buffer;
    file->utf8_buffer= (char*) g_memdup(file->buffer,
                                (guint)file->utf8_buffer_len_in_bytes);

    if (wrong_invalid_chars_at_the_end(file,invalid_chars))
    { // that's not a partial char at the end
      *error= MYX_CHARSET_WRONG_CHARSET_SPECIFIED; 
      return 0;
    }
  } // since the source is in UTF-8
  file->bytes_translated= (gssize)file->utf8_buffer_len_in_bytes;
  return 1;
}

int translate_non_utf8_buffer(MYX_INTL_FILE * file, MYX_LIB_ERROR * error)
{
  GError *err= NULL;
  file->utf8_buffer= g_convert((char*)file->buffer, (gssize)file->buffer_len_in_bytes,
                                   "UTF-8", file->charset,
                                   &file->bytes_translated,
                                   &file->utf8_buffer_len_in_bytes, &err);
  if ( !file->utf8_buffer || 
        (err && err->code != G_CONVERT_ERROR_PARTIAL_INPUT))
  {
    *error= MYX_CHARSET_CONVERSION_ERROR;
    return 0;
  }
  return 1;
}

int safe_read_buffer(MYX_INTL_FILE * file, MYX_LIB_ERROR * error)
{
  if (file->next_utf8_char < file->utf8_buffer + file->utf8_buffer_len_in_bytes)
  {
    return 1;
  }
  else
  { // we need to refill our buffer    
    size_t untranslated= safe_copy_untranslated_characters(file);
    size_t read_len;
    
    // Reset relevant buffers.
    g_free(file->utf8_buffer);
    file->utf8_buffer= NULL;
    file->next_utf8_char = NULL;
    file->utf8_buffer_len_in_bytes = 0;
    
    if ( !(read_len= fread(file->buffer + untranslated,
                           1, BUFFER_LEN, file->file)) )
    {
      if (ferror(file->file))
        *error= MYX_CANT_READ_FROM_FILE;
      return 0;
    }
    file->buffer_len_in_bytes= read_len + untranslated;

    if (!( file->charset_is_utf8 ? translate_utf8_buffer(file,error)
                                 : translate_non_utf8_buffer(file,error)))
    {
      return 0;
    }
    file->next_utf8_char= file->utf8_buffer;
    return 1;
  }
}

///////////////////////////////////////////////////////////////////////////////
/** @brief Reads one character from the file 

    @param file       International File described the read file (with charset)
    @param bytes_read If bytes_read is not NULL it will be set to number 
                      of bytes read
    @param error      error pointer to error code
                      (it is set if the function returns -1)

    @return The index of the character in the Unicode-set
            -1 in case of an error or end-of-file
*//////////////////////////////////////////////////////////////////////////////
bigint myx_read_char_from_intl_file(MYX_INTL_FILE * file,
                                    int * bytes_read, MYX_LIB_ERROR * error)
{
  if (!safe_read_buffer(file,error))
  {
    return -1;
  }
  else
  {
    gunichar utf8_char= g_utf8_get_char(file->next_utf8_char);
    const char * cur_utf8_char= file->next_utf8_char;
    file->next_utf8_char= g_utf8_next_char(file->next_utf8_char);

    *bytes_read= (int) (file->next_utf8_char - cur_utf8_char);

    // Let's calculate real number of bytes read
    if ( !file->charset_is_utf8 )
    {
      char buf[20];
      char *buf_pos= buf;
      size_t buf_left= sizeof(buf);
      size_t chars_left= *bytes_read;
      /*size_t res=*/ iconv(file->conv_from_utf8,
                        &cur_utf8_char,&chars_left,
                        &buf_pos,&buf_left);
      *bytes_read= (int)(sizeof(buf) - buf_left);
    }
    file->file_pos += *bytes_read;  

    return utf8_char;
  }
}
