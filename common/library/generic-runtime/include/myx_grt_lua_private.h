/* Copyright (c) 2004, 2005 MySQL AB
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
  
   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
 */

#ifndef _MYX_GRT_LUA_PRIVATE_H_
#define _MYX_GRT_LUA_PRIVATE_H_


#include "myx_grt_lua.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>


typedef struct MYX_GRT_MODULE_LOADER_PRIVATE
{
  lua_State *lua;
} MYX_LUA_LOADER;


int myx_lua_push_grt_value(lua_State *L, MYX_GRT_VALUE *value);
MYX_GRT_VALUE *myx_lua_pop_grt_value(lua_State *L);
int myx_lua_push_wrap_grt_value(lua_State *L, MYX_GRT_VALUE *value);

#endif /* _MYX_GRT_LUA_PRIVATE_H_ */
