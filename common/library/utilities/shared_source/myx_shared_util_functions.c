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

#include <myx_shared_util_functions.h>

///////////////////////////////////////////////////////////////////////////////
/** @brief Wrapper around myx_fopen that expects a filename in UTF-8 encoding
    @param filename name of file to open
    @param mode second argument of fopen
    @return If successful, myx_fopen returns opened FILE*.
            Otherwise, it returns NULL.
*//////////////////////////////////////////////////////////////////////////////
FILE* myx_fopen(const char *filename, const char *mode)
{
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  WCHAR wsz_filename[1024+1];
  WCHAR wsz_mode[256];

  //Covert filename from utf8 to WideChar
  MultiByteToWideChar(CP_UTF8, 0, filename,
    (int)strlen(filename)+1, wsz_filename,   
    sizeof(wsz_filename)/sizeof(wsz_filename[0]));

  //Covert mode from utf8 to WideChar
  MultiByteToWideChar(CP_UTF8, 0, mode,
    (int)strlen(mode)+1, wsz_mode,   
    sizeof(wsz_mode)/sizeof(wsz_mode[0]));

  return _wfopen(wsz_filename, wsz_mode);
#else
  FILE *file;
  char * local_filename;

  if (! (local_filename= g_filename_from_utf8(filename,-1,NULL,NULL,NULL)))
    return NULL;

  file= fopen(local_filename, mode);

  g_free(local_filename);

  return file;
#endif
}

///////////////////////////////////////////////////////////////////////////////
/** @brief free a MYX_STRINGLIST structure

    @param stringlist  list of strings to look through
    @return            index of found string if it exists, else -1
*//////////////////////////////////////////////////////////////////////////////
int myx_free_stringlist(MYX_STRINGLIST *stringlist)
{
  unsigned int i;

  if(!stringlist)
    return 0;

  for (i= 0; i < stringlist->strings_num; i++)
  {
    g_free(stringlist->strings[i]);
  }
  g_free(stringlist->strings);
  g_free(stringlist);

  return 0;
}