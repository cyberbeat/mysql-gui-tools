/* Copyright (C) 2003 MySQL AB

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

#ifndef myx_network_h
#define myx_network_h

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <winsock2.h>
#endif

#define __LCC__


/*
 * Enums
 */


/*
 * Structs
 */


/*
 * Functions
 */

#define NETWORK_NAME_BUFFER_LENGTH 64
#define IP_BUFFER_LENGTH sizeof("xxx.xxx.xxx.xxx")

int myx_get_ip_as_string      (const char *hostname, char *ip);
int myx_get_network_name_by_ip(const char *ip, char *hostname);

#endif
