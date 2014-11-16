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

#include <myx_util_public_interface.h>

///////////////////////////////////////////////////////////////////////////////
/** @brief get version of library
    @ingroup Common_functions
    @return int version number

    uses constant libmysqlutil_PUBLIC_INTERFACE_VERSION
*//////////////////////////////////////////////////////////////////////////////
int myx_get_util_public_interface_version()
{
  return libmysqlutil_PUBLIC_INTERFACE_VERSION;
}

