/*
 Generic Runtime Library (GRT)
 Copyright (C) 2005 MySQL AB

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
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifdef ENABLE_LUA_MODULES

#include "myx_grt_private.h"

#include "myx_grt_lua_private.h"
#include "lxp/lxplib.h"

static MYX_GRT_ERROR lua_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);
static MYX_GRT_ERROR lua_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule);

static int l_log_error(lua_State *lua);
static int l_log_warning(lua_State *lua);
static int l_log_message(lua_State *lua);


static const luaL_reg lualibs[] = {
        { "base",       luaopen_base },
        { "table",      luaopen_table },
        { "io",         luaopen_io },
        { "string",     luaopen_string },
        { "math",       luaopen_math },
        { "debug",      luaopen_debug },
        { "loadlib",    luaopen_loadlib },
        { "lxp",        luaopen_lxp },
        { NULL,         NULL }
};


static MYX_GRT *get_grt(lua_State *L)
{
  MYX_GRT **ctx;
  lua_getglobal(L, "__GRT");

  ctx= (MYX_GRT**)luaL_checkudata(L, -1, "MYX_GRT");
  if (ctx)
  {
    lua_pop(L, 1);
    return *ctx;
  }
  return NULL;
}


static void module_add_listener(MYX_GRT *grt, char *name, void *param, void *userdata)
{
  lua_State *l= (lua_State*)userdata;

  myx_lua_refresh(grt, l);
} 


MYX_GRT_MODULE_LOADER *myx_lua_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error, const char *lua_module_path)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  MYX_LUA_LOADER *priv= g_new0(MYX_LUA_LOADER, 1);
  static char *file_extensions[]= {
    ".lua"
  };
  char *lua_module_file;
  GDir *dir;
  const char *entry;
  
  GRT_ENTER(grt);

  *error= MYX_GRT_NO_ERROR;

  loader->grt= grt;
  loader->loader_type= MYX_LUA_MODULE_TYPE;
  loader->priv= priv;
  loader->init_module= lua_init_module;
  loader->call_function= lua_call_function;
  loader->extensions_num= 1;
  loader->extensions= file_extensions;

  priv->lua= lua_open();

  // register a global __GRT variable 
  {
    MYX_GRT **userdata;
    lua_pushstring(priv->lua, "__GRT");
    userdata= (MYX_GRT**)lua_newuserdata(priv->lua, sizeof(MYX_GRT*));
    *userdata= grt;
    luaL_newmetatable(priv->lua, "MYX_GRT");
    lua_setmetatable(priv->lua, -2);
    lua_settable(priv->lua, LUA_GLOBALSINDEX);
  }
  // register some libs
  {
    const luaL_reg *lib;

    for (lib = lualibs; lib->func != NULL; lib++) 
    {
      lib->func(priv->lua);
      lua_settop(priv->lua, 0);
    }
  }

  // register listener for module notifications
  myx_grt_add_listener(grt, module_add_listener, GRT_MODULE_ADD_NOTIFICATION, priv->lua);
  
  // register logging functions
  lua_register(priv->lua, "logerror", l_log_error);
  lua_register(priv->lua, "logwarning", l_log_warning);
  lua_register(priv->lua, "logmessage", l_log_message);

  myx_lua_register_functions(grt, priv->lua);
  
  dir= g_dir_open(lua_module_path, 0, NULL);

  if (dir)
  {
    while ((entry= g_dir_read_name(dir)) != NULL)
    {
      if (entry[0] == '_')
      {
        lua_module_file= g_build_filename(lua_module_path, entry, NULL);
        if (check_file_exists(lua_module_file))
        {
          int status;
          // load the _library
          status= luaL_loadfile(priv->lua, lua_module_file);
          if (status != 0)
          {
            if (getenv("GRT_VERBOSE"))
            {
              g_warning("Could not load lua _library: %s", lua_tostring(priv->lua, -1));
              lua_pop(priv->lua, 1);
            }
          }

          // execute the _library, so that function declarations in it get executed
          status= lua_pcall(priv->lua, 0, 0, 0);

          if (status != 0)
          {
            g_warning("error executing lua _library: %s\n", lua_tostring(priv->lua, -1));
            lua_pop(priv->lua, 1);
          }
        }
        g_free(lua_module_file);
      }
    }

    g_dir_close(dir);
  }

  g_assert(lua_gettop(priv->lua) == 0);
  
  GRT_RETURN(grt, loader, MYX_GRT_MODULE_LOADER*);
}


static void lua_push_fallback_table(lua_State *l)
{
  lua_newtable(l);
  lua_pushstring(l, "__index");
  lua_getglobal(l, "_G");
  lua_settable(l, -3);
}


static MYX_GRT_ERROR lua_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule)
{
  MYX_GRT_MODULE *module;
  lua_State *l= loader->priv->lua;
  MYX_GRT_VALUE *module_info;
  const char *module_name, *extends;
  char *lua_function_table_name;
  MYX_GRT_LIST *module_functions;
  int status;
  unsigned int i;

  // create a new table which will be the environment for the
  // loaded module
  lua_pushstring(l, "___tmp");
  lua_newtable(l);
  lua_settable(l, LUA_GLOBALSINDEX);

  // set the global environment as a fallback for the module environment
  lua_getglobal(l, "___tmp");
  lua_push_fallback_table(l);
  lua_setmetatable(l, -2);
  lua_pop(l, 1); // pop __tmp

  // load the module
  status= luaL_loadfile(l, file);
  if (status != 0)
  {
    myx_grt_printf(loader->grt, "Could not load lua module %s: %s"NL, file, lua_tostring(l, -1));

    lua_pop(l, 1);
    return MYX_GRT_BAD_MODULE;
  }

  // fetch the new environment table
  lua_getglobal(l, "___tmp");

  // sets it as the environment for the loaded module
  lua_setfenv(l, -2);

  // execute the module, so that function declarations in it get executed
  status= lua_pcall(l, 0, 0, 0);

  if (status != 0)
  {
    g_warning("error executing lua module %s: %s\n", file, lua_tostring(l, -1));
    lua_pop(l, 1);
    goto error;
  }

  // get module info
  lua_getglobal(l, "___tmp");
  lua_pushstring(l, "getModuleInfo");
  lua_gettable(l, -2);
  status= lua_pcall(l, 0, 1, 0);

  if (status != 0)
  {
    g_warning("error calling getModuleInfo() in lua module %s: %s\n", file,
              lua_tostring(l, -1));
    lua_pop(l, 2);
    goto error;
  }

  module_info= myx_lua_pop_grt_value(l);
  if (!module_info || !myx_grt_value_get_type(module_info)==MYX_DICT_VALUE)
  {
    g_warning("invalid return value calling getModuleInfo() in lua module %s\n", file);
    if (module_info)
      myx_grt_value_release(module_info);
    lua_pop(l, 1);
    goto error;
  }
  
  lua_pop(l, 1); // pop __tmp

  // get module data
  module_name= myx_grt_value_as_string(myx_grt_dict_item_get_value(module_info, "name"));
  lua_function_table_name= g_strdup_printf("__%s_lua", module_name);

  extends= myx_grt_value_as_string(myx_grt_dict_item_get_value(module_info, "extends"));

  module_functions= myx_grt_value_as_list(myx_grt_dict_item_get_value(module_info, "functions"));
  
  if (!module_name || !module_functions)
  {
    g_warning("bad info returned from getModuleInfo() in lua module %s: %s\n", file,
              lua_tostring(l, -1));
    if (module_info)
      myx_grt_value_release(module_info);
    goto error;
  }

  // rename the ___tmp module table to the definitive name
  lua_pushstring(l, lua_function_table_name);
  lua_getglobal(l, "___tmp");
  lua_settable(l, LUA_GLOBALSINDEX);

  // !!! temporary workaround
  lua_pushstring(l, "___tmp");
  lua_pushnil(l);
  lua_settable(l, LUA_GLOBALSINDEX);

  g_free(lua_function_table_name);

  
  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);

  module->loader= loader;
  module->priv= NULL;
  module->name= g_strdup(module_name);
  module->path= g_strdup(file);
  module->functions_num= module_functions->items_num;
  module->functions= g_new0(MYX_GRT_FUNCTION, module->functions_num);
  for (i= 0; i < module_functions->items_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    
    func->module= module;
    myx_grt_parse_function_spec(module_functions->items[i]->value.s, func);
    func->priv= NULL;
  }
  module->extends= g_strdup(extends);
  
  myx_grt_value_release(module_info);

  *retmodule= module;
  
  if (getenv("GRT_VERBOSE"))
    g_message("Initialized module %s", file);

  return MYX_GRT_NO_ERROR;
  
error:

  return MYX_GRT_MODULE_INIT_ERROR;
}


static MYX_GRT_ERROR lua_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_GRT_ERROR error= MYX_GRT_NO_ERROR;
  lua_State *lua= function->module->loader->priv->lua;
  char *lua_function_table_name= g_strdup_printf("__%s_lua", function->module->name);
  int rc;
  int top;
  
  if (getenv("GRT_VERBOSE"))
    g_message("Calling lua function %s.%s", function->module->name, function->name);

  lua_getglobal(lua, lua_function_table_name);
  lua_pushstring(lua, function->name);
  lua_gettable(lua, -2);

  if (value)
  {
    myx_lua_push_wrap_grt_value(lua, value);
    myx_grt_value_retain(value);
  }
  else
    lua_pushnil(lua);

  g_free(lua_function_table_name);

  // inser traceback function before the actual function so that any
  // errors get a trace
  top= lua_gettop(lua);
  lua_pushliteral(lua, "_TRACEBACK");
  lua_rawget(lua, LUA_GLOBALSINDEX);
  lua_insert(lua, top-1);
  rc = lua_pcall(lua, 1, 1, top-1);
  lua_remove(lua, top-1);

  if (rc != 0)
  {
    char *error_str= g_strdup_printf("Error calling lua function %s.%s: %s", 
      function->module->name, function->name, lua_tostring(lua, -1));

    lua_pop(lua, 1);

    *retval= myx_grt_dict_new(NULL, NULL);

    myx_grt_dict_item_set_value_from_string(*retval, "error", error_str);

    g_free(error_str);
    
    error= MYX_GRT_FUNCTION_CALL_ERROR;
  }
  else
  {
    *retval= myx_lua_pop_grt_value(lua);
  }

  lua_pop(lua, 1);

  return error;
}





static int l_log_error(lua_State *lua)
{
  MYX_GRT *grt= get_grt(lua);
  const char *message;
  const char *detail;
  
  detail= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);
  message= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);

  grt->logfunc(grt, 2, message, detail);

  return 0;
}


static int l_log_warning(lua_State *lua)
{
  MYX_GRT *grt= get_grt(lua);
  const char *message;
  const char *detail;
  
  detail= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);
  message= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);

  grt->logfunc(grt, 1, message, detail);

  return 0;
}


static int l_log_message(lua_State *lua)
{
  MYX_GRT *grt= get_grt(lua);
  const char *message;
  const char *detail;
  
  detail= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);
  message= luaL_checkstring(lua, -1);
  lua_pop(lua, 1);

  grt->logfunc(grt, 0, message, detail);

  return 0;
}



#endif
