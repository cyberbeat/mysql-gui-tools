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

#ifndef myx_shared_util_functions_h
#define myx_shared_util_functions_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <glib.h>

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

#ifndef MYX_STRINGLIST_DECLARED
typedef struct {
    unsigned int strings_num;
    char **strings;
} MYX_STRINGLIST;
#define MYX_STRINGLIST_DECLARED
#endif

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  #include <winsock2.h>
  #include <windows.h>
  #include <io.h>
#endif

MYX_PUBLIC_FUNC FILE* myx_fopen(const char *filename, const char *mode);
MYX_PUBLIC_FUNC int myx_free_stringlist(MYX_STRINGLIST *stringlist);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif

