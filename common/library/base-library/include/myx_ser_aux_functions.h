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

#ifndef myx_ser_aux_functions_h
#define myx_ser_aux_functions_h


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

typedef struct _MYX_SFD MYX_SFD;

MYX_PUBLIC_FUNC MYX_SFD *myx_sfd_from_fd(int fd);
MYX_PUBLIC_FUNC int myx_free_sfd(MYX_SFD *sfd);

MYX_PUBLIC_FUNC int ser_string(MYX_SFD *fd, const char *s);
MYX_PUBLIC_FUNC int ser_int(MYX_SFD *fd, int value);

MYX_PUBLIC_FUNC int unser_string(MYX_SFD *fd, char **s);
MYX_PUBLIC_FUNC int unser_int(MYX_SFD *fd, int *value);


#endif
