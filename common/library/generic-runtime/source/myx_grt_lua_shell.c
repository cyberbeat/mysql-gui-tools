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

/**
 * @file  myx_grt_lua_shell.c
 * @brief Lua Shell Frontend
 * 
 * See also: <a href="../grt.html#LuaShell">GRT Lua Shell</a>
 */


#ifdef ENABLE_LUA_MODULES

#include "myx_grt_private.h"
//#include "myx_grt_lua_shell.h"
#include "myx_grt_lua_private.h"

#include <math.h>

#ifdef _WINDOWS
  #include <Windows.h>
#endif



typedef struct MYX_GRT_SHELL_PRIVATE {
  lua_State *lua;
  char *current_line;
} MYX_GRT_SHELL_PRIVATE;


// this context is stored in the Lua environments global variable
// to allow access to GRT stuff from Lua callbacks
typedef struct {
  MYX_GRT *grt;
  char *cwd;
} LUA_CONTEXT;



static int myx_lua_add_grt_module_to_table(lua_State *l, MYX_GRT_MODULE *mod, int tbl);
static int myx_lua_call_grt_method_by_name(MYX_GRT *grt, lua_State *l, const char *name, 
                                           const char *func_name, MYX_GRT_VALUE *argument);
static int myx_lua_call_grt_method(lua_State *l);
static MYX_GRT_VALUE *pop_retain_grt_value(lua_State *l);

static MYX_GRT *myx_lua_get_grt(lua_State *l);
static LUA_CONTEXT *myx_lua_get_ctx(lua_State *l);



static int shell_init(MYX_GRT *grt);
static void shell_print_welcome(MYX_GRT *grt);
static MYX_GRT_SHELL_COMMAND shell_execute(MYX_GRT *grt, const char *linebuf);
static char *shell_get_prompt(MYX_GRT *grt);
static void *shell_get_lua(MYX_GRT *grt);
static int shell_run_file(MYX_GRT *grt, const char *file_name, int interactive);
static MYX_GRT_VALUE * shell_get_global_var(MYX_GRT *grt, const char *var_name);
static int shell_set_global_var(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value);


static const char *mlua_popstring(lua_State *l)
{
  const char *s= luaL_checkstring(l, -1);
  lua_pop(l, 1);
  return s;
}


static const char *mlua_popstring_any(lua_State *l)
{
  if (lua_isstring(l, -1))
    return mlua_popstring(l);
  else
  {
    MYX_GRT_VALUE *tmp= pop_retain_grt_value(l);
    if (tmp && myx_grt_value_get_type(tmp) == MYX_STRING_VALUE)
    {
      const char *s= myx_grt_value_as_string(tmp);
      myx_grt_value_release(tmp);
      return s;
    }
    if (tmp) myx_grt_value_release(tmp);
    luaL_error(l, "string argument expected, but got a non-string GRT value");
    return NULL;
  }
}

/*
static const char *mlua_udatatype(lua_State *l, int index)
{
  const char *tn;
  if (lua_getmetatable(l, index))
  {
    lua_rawget(l, LUA_REGISTRYINDEX); 
    tn= lua_tostring(l, -1);
    return tn;
  }
  return NULL;
}
*/

static void mlua_checkempty(lua_State *l)
{
  if (lua_gettop(l)>0)
    luaL_error(l, "too many arguments to function");
}


static lua_State *get_lua_shell_state(MYX_GRT *grt)
{
  return grt->shell->data->lua;
}


//----------------------------------------------------------------------------------------------------------------------

static int l_list_modules(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_MODULE **modules;
  int count, i;

  count= myx_grt_modules_get_that_extend(grt, NULL, &modules);
  for (i= 0; i < count; i++)
  {
    myx_grt_printf(grt, "%s"NL, modules[i]->name);
  }
  g_free(modules);

  return 0;
}


static int l_show_module(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *name= mlua_popstring(l);
  MYX_GRT_MODULE *module= myx_grt_module_get(grt, name);
  unsigned int i;

  if (!module)
  {
    myx_grt_printf(grt, "Module '%s' has not been initialized."NL, name);
    return 0;
  }

  if (module->extends)
    myx_grt_printf(grt, "Module '%s' (extends '%s')"NL, name, module->extends);
  else
    myx_grt_printf(grt, "Module '%s'"NL, name);
  for (i= 0; i < module->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    char *params= myx_grt_module_function_get_params(func);
    char *return_type= myx_grt_module_function_get_return_type(func);

    myx_grt_printf(grt, " %s:%s:%s"NL, module->functions[i].name, params, return_type);
  }

  return 0;
}

static int l_get_modules(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *base_module= NULL;
  MYX_GRT_MODULE **modules;
  int tbl_stack_pos, i, count;

  if (lua_gettop(l) > 0)
    base_module= mlua_popstring(l);

  lua_newtable(l);
  tbl_stack_pos= lua_gettop(l);

  count= myx_grt_modules_get_that_extend(grt, base_module, &modules);
  for (i= 0; i < count; i++)
  {
    lua_pushnumber(l, i + 1);
    lua_pushstring(l, modules[i]->name);

    lua_settable(l, tbl_stack_pos);
  }
  g_free(modules);

  return 1;
}

static int l_get_module_functions(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *module_name= mlua_popstring(l);
  MYX_GRT_MODULE *module= myx_grt_module_get(grt, module_name);
  int tbl_stack_pos, i, count;

  lua_newtable(l);
  if (module)
  {
    tbl_stack_pos= lua_gettop(l);

    count= myx_grt_module_function_get_count(module);
    for (i= 0; i < count; i++)
    {
      MYX_GRT_FUNCTION *module_function= myx_grt_module_function_get_by_index(module, i);

      lua_pushnumber(l, i + 1);
      lua_pushstring(l, module_function->name);

      lua_settable(l, tbl_stack_pos);
    }
  }

  return 1;
}

static int l_call_function(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *argument= NULL;
  const char *module_name, *function_name;
  MYX_GRT_MESSAGE_CALLBACK process_messages= grt->process_messages;
  int res;
  int param_count= lua_gettop(l);

  if (param_count < 2)
    return luaL_error(l, "Missing parameter");

  if (param_count == 4)
  {
    unsigned int hide_messages;

    hide_messages= luaL_checkint(l, -1);
    lua_pop(l, 1);

    if (hide_messages)
      grt->process_messages= NULL;
  }

  if (param_count > 2)
    argument= pop_retain_grt_value(l);

  function_name= mlua_popstring(l);
  module_name= mlua_popstring(l);

  res= myx_lua_call_grt_method_by_name(grt, l, module_name, function_name, argument);

  if (argument)
    myx_grt_value_release(argument);
  
  grt->process_messages= process_messages;

  return res;
}


static int l_refresh(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);

  return myx_lua_refresh(grt, l);
}


static int l_save_value(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *fn= mlua_popstring(l);
  MYX_GRT_VALUE *value;
  MYX_GRT_ERROR err;

  if (lua_gettop(l) == 0)
    luaL_error(l, "Missing parameter");

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "Invalid parameter");

  err= myx_grt_store_to_file(grt, value, fn);
  
  myx_grt_value_release(value);
  
  if (err != MYX_GRT_NO_ERROR)
    luaL_error(l, "Could not save data to file %s", fn);

  return 0;
}


static int l_load_value(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *fn;
  MYX_GRT_VALUE *value;

  fn= mlua_popstring(l);
  mlua_checkempty(l);
  
  value= myx_grt_retrieve_from_file(grt, fn);
  if (!value)
    luaL_error(l, "Could not load data from file %s", fn);

  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_lookup_id(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *fn;
  MYX_GRT_VALUE *value;
  
  fn= mlua_popstring_any(l);
  mlua_checkempty(l);

  value= myx_grt_reference_cache_lookup(grt, fn);
  if (value)
  {
    myx_grt_value_retain(value);
    myx_lua_push_wrap_grt_value(l, value);
  }
  else
    lua_pushnil(l);
  
  return 1;
}

static int l_lookup_add(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *value;

  if (lua_gettop(l) == 0)
    luaL_error(l, "Missing parameter");

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "Invalid parameter");

  myx_grt_reference_cache_add(grt, value);

  return 0;
}


static int l_grt_value_type(lua_State *l)
{
  MYX_GRT_VALUE *value= pop_retain_grt_value(l);
  mlua_checkempty(l);
  
  if (value)
    lua_pushstring(l, myx_get_value_type_as_string(myx_grt_value_get_type(value)));
  else
    lua_pushnil(l);
  
  myx_grt_value_release(value);
  
  return 1;
}


static int l_grt_value_from_xml(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *xml= mlua_popstring(l);
  MYX_GRT_VALUE *value;

  mlua_checkempty(l);
  
  value= myx_grt_value_from_xml(grt, xml, strlen(xml));
  if (!value)
    luaL_error(l, "Could not create a Grt value from the given XML string");

  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_grt_value_to_xml(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *value;
  char *xml;

  if (lua_gettop(l) == 0)
    luaL_error(l, "Missing parameter");

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "Invalid parameter");

  mlua_checkempty(l);
 
  xml= myx_grt_value_to_xml(grt, value);

  myx_grt_value_release(value);

  lua_pushstring(l, xml);

  g_free(xml);

  return 1;
}

static int l_cd(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);
  MYX_GRT_VALUE *value;
  const char *path= mlua_popstring(l);
  char *new_path;
  
  mlua_checkempty(l);
  
  if (!path)
    luaL_error(l, "Invalid path");

  if (!*path)
    return 0;

  new_path= myx_grt_get_abspath(ctx->cwd, path);

  value= myx_grt_dict_item_get_by_path(ctx->grt, ctx->grt->root, new_path);

  if (!value)
  {
    g_free(new_path);
    luaL_error(l, "Invalid path");
  }
  g_free(ctx->cwd);
  ctx->cwd= new_path;

  // set a variable to hold the current object
  myx_grt_value_retain(value);
  lua_pushstring(l, MYX_SHELL_CURNODE);
  myx_lua_push_wrap_grt_value(l, value);
  lua_settable(l, LUA_GLOBALSINDEX);
  
  return 0;
}


static int l_ls(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);
  const char *path;
  char *fpath;
  MYX_GRT_VALUE *value;
  unsigned int i;

  if (lua_gettop(l) > 0)
    path= mlua_popstring(l);
  else
    path = "";

  if (!ctx->grt->root)
    return 0;
  
  fpath= myx_grt_get_abspath(ctx->cwd, path);
  value= myx_grt_dict_item_get_by_path(ctx->grt, ctx->grt->root, fpath);
  g_free(fpath);
  if (!value)
    luaL_error(l, "Invalid path");

  if (value->type == MYX_DICT_VALUE)
  {
    unsigned int Count = myx_grt_dict_item_count_complex(value);
    for (i= 0; i < Count; i++)
    {
      const char* Key = myx_grt_dict_item_key_by_index_complex(value, i);
      myx_grt_printf(ctx->grt, "%s"NL, Key);
    }
  }
  else if (value->type == MYX_LIST_VALUE && value->value.l->content_type == MYX_DICT_VALUE)
  {
    unsigned int unnamed= 0;
    
    unsigned int Count = myx_grt_list_item_count(value);
    for (i= 0; i < Count; i++)
    {
      MYX_GRT_VALUE *item= myx_grt_list_item_get(value, i);
      const char *name;
      
      name= myx_grt_dict_item_get_as_string(item, "name");
      if (name)
        myx_grt_printf(ctx->grt, "%s"NL, name);
      else
        unnamed++;
    }
    if (unnamed > 0)
      myx_grt_printf(ctx->grt, "Plus %i unnamed objects in the list."NL, unnamed);
  }
  else
    luaL_error(l, "Not in a listable object");

  return 0;
}


static int l_pwd(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);

  mlua_checkempty(l);
  
  lua_pushstring(l, ctx->cwd);

  return 1;
}


static int l_get_child(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);
  MYX_GRT_VALUE *value= NULL;
  MYX_GRT_VALUE *dict= NULL;
  const char *path;

  path= mlua_popstring(l);

  if (lua_gettop(l) > 0)
  {
    dict= pop_retain_grt_value(l);
    if (!dict)
      luaL_error(l, "bad GRT value in argument");
  }

  if (dict)
  {
    if (*path != '/')
      luaL_error(l, "bad path for child object in dict. Must be an absolute path");
    value= myx_grt_dict_item_get_by_path(ctx->grt, dict, path);
    myx_grt_value_release(dict);
  }
  if (value)
  {
    myx_grt_value_retain(value);
    myx_lua_push_wrap_grt_value(l, value);
  }
  else
  {
    lua_pushnil(l);
    //luaL_error(l, "object '%s' not found", path);
  }

  return 1;
}

static MYX_GRT_VALUE *luaL_checkgrtudata(lua_State *l, int index)
{
  MYX_GRT_VALUE **value;
  value= luaL_checkudata(l, index, "MYX_GRT_VALUE");
  if (value)
    return *value;
  value= luaL_checkudata(l, index, "MYX_GRT_LIST");
  if (value)
    return *value;
  value= luaL_checkudata(l, index, "MYX_GRT_DICT");
  if (value)
    return *value;
  return NULL;
}

static int l_get_global(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE *dict= NULL;
  const char *path;

  // if the argument is already a MYX_GRT_VALUE, leave it on the stack and exit
  if (luaL_checkgrtudata(l, -1))
    return 0;

  path= mlua_popstring(l);

  if (lua_gettop(l) > 0)
  {
    dict= pop_retain_grt_value(l);
    if (!dict)
      luaL_error(l, "bad GRT value in argument");
  }

  if (dict)
  {
    if (*path != '/')
    {
      myx_grt_value_release(dict);
      luaL_error(l, "bad path for getobj in dict. Must be an absolute path");
    }
    value= myx_grt_dict_item_get_by_path(ctx->grt, dict, path);
    myx_grt_value_release(dict);
  }
  else
  {
    char *tmp= myx_grt_get_abspath(ctx->cwd, path);
    value= myx_grt_dict_item_get_by_path(ctx->grt, ctx->grt->root, tmp);
    g_free(tmp);
  }
  if (!value)
    luaL_error(l, "object '%s' not found", path);

  myx_grt_value_retain(value);
  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}


static int l_set_global(lua_State *l)
{
  LUA_CONTEXT *ctx= myx_lua_get_ctx(l);
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE *indict= NULL;
  const char *path;

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "missing value parameter");

  path= mlua_popstring(l);

  if (lua_gettop(l) > 0)
    indict= pop_retain_grt_value(l);

  mlua_checkempty(l);

  if (indict)
  {
    if (indict->type != MYX_DICT_VALUE)
    {
      myx_grt_value_release(indict);
      myx_grt_value_release(value);
      luaL_error(l, "invalid argument, expected a dict but got something else");
    }
    if (*path != '/')
    {
      myx_grt_value_release(indict);
      myx_grt_value_release(value);
      luaL_error(l, "bad path for setobj in dict. Must be an absolute path");
    }
    if (myx_grt_dict_item_set_by_path(indict, path, value) < 0)
    {
      myx_grt_value_release(indict);
      myx_grt_value_release(value);
      luaL_error(l, "invalid path '%s'", path);
    }
    myx_grt_value_release(indict);
  }
  else
  {
    if (strcmp2(path, "/") == 0)
    {
      /*MYX_GRT_ERROR error= */myx_grt_set_root(ctx->grt, value);
      myx_grt_value_release(value);
    }
    else
    {
      char *fpath;

      fpath= myx_grt_get_abspath(ctx->cwd, path);
      if (myx_grt_dict_item_set_by_path(ctx->grt->root, fpath, value) < 0)
      {
        g_free(fpath);
        myx_grt_value_release(value);
        luaL_error(l, "invalid path '%s'", path);
      }
      g_free(fpath);
    }
  }

  // push the value as result, so one can make 
  // local rdbmsMgmt= grtV.setGlobal("/rdbmsMgmt", RdbmsManagement:getManagementInfo())
  //myx_lua_push_wrap_grt_value(l, value);

  myx_grt_value_release(value);
  return 1;
}


static int l_grt_value_to_lua(lua_State *l)
{
  MYX_GRT_VALUE *value;
  if ((value= luaL_checkgrtudata(l, -1)))
  {
    myx_grt_value_retain(value);
    lua_pop(l, 1);
    myx_lua_push_grt_value(l, value);
    myx_grt_value_release(value);
  }
  else
  {
    /* tollerate a lua value as well
    const char *type= mlua_udatatype(l, -1);
    luaL_error(l, "toLua expects a MYX_GRT_VALUE as argument (got %s)",
               type ? type : "nil");*/
  }

  return 1;
}

static int l_grt_value_new(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *gstruct_name;
  const char *name;
  const char *_id;
  const char *owner;
  MYX_GRT_VALUE *value;

  if (!lua_isstring(l, -1) || !lua_isstring(l, -2) || 
    !lua_isstring(l, -3) || !lua_isstring(l, -4))
  {
    luaL_error(l, "invalid parameters, use {struct_name, name, _id, owner}");
  }


  owner= mlua_popstring(l);
  _id= mlua_popstring(l);
  name= mlua_popstring(l);
  gstruct_name= mlua_popstring(l);


  value= myx_grt_dict_new_obj(grt, gstruct_name, name, _id, owner);

  //push the Grt value as Lua userdata
  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_grt_value_new_list(lua_State *l)
{
 // MYX_GRT *grt= myx_lua_get_grt(l);
  const char *content_type_name= NULL;
  const char *content_struct_name= NULL;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE_TYPE content_type;

  if (lua_isstring(l, -1) && lua_isstring(l, -2))
  {
    content_struct_name= mlua_popstring(l);
    content_type_name= mlua_popstring(l);
  }
  else if (lua_isstring(l, -1))
    content_type_name= mlua_popstring(l);

  if (content_type_name)
  {
    MYX_GRT_ERROR error;

    content_type= myx_get_value_type_from_string(content_type_name, &error);

    if (error != MYX_GRT_NO_ERROR)
      luaL_error(l, "invalid content_type. Use int, real, string, list or dict");
  }
  else
    content_type= MYX_ANY_VALUE;

  value= myx_grt_list_new(content_type, content_struct_name);

  //push the Grt value as Lua userdata
  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_grt_value_new_dict(lua_State *l)
{
//  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *content_type_name= NULL;
  const char *content_struct_name= NULL;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE_TYPE content_type;

  if (lua_isstring(l, -1) && lua_isstring(l, -2))
  {
    content_struct_name= mlua_popstring(l);
    content_type_name= mlua_popstring(l);
  }
  else if (lua_isstring(l, -1))
    content_type_name= mlua_popstring(l);

  if (content_type_name)
  {
    MYX_GRT_ERROR error;

    content_type= myx_get_value_type_from_string(content_type_name, &error);

    if (error != MYX_GRT_NO_ERROR)
      luaL_error(l, "invalid content_type. Use int, real, string, list or dict");
  }
  else
    content_type= MYX_ANY_VALUE;

  value= myx_grt_dict_new_typed(content_type, content_struct_name);

  //push the Grt value as Lua userdata
  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_grt_value_set_type(lua_State *l)
{
//  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *content_type_name= NULL;
  const char *content_struct_name= NULL;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE_TYPE content_type;

  if (lua_isstring(l, -1) && lua_isstring(l, -2))
  {
    content_struct_name= mlua_popstring(l);
    content_type_name= mlua_popstring(l);
  }
  else if (lua_isstring(l, -1))
    content_type_name= mlua_popstring(l);

  // get value
  if ((value= luaL_checkgrtudata(l, -1)))
    lua_pop(l, 1);
  else
    luaL_error(l, "the first argument has to be a GRT value");

  if (content_type_name)
  {
    MYX_GRT_ERROR error;

    content_type= myx_get_value_type_from_string(content_type_name, &error);

    if (error != MYX_GRT_NO_ERROR)
      luaL_error(l, "invalid content_type. Use int, real, string, list or dict");
  }
  else
    content_type= MYX_ANY_VALUE;

  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
  {
    myx_grt_list_content_set_type(value, content_type);

    if (content_struct_name)
      myx_grt_list_content_set_struct_name(value, content_struct_name);
  }
  else if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
  {
    myx_grt_dict_content_set_type(value, content_type);

    if (content_struct_name)
      myx_grt_dict_content_set_struct_name(value, content_struct_name);
  }

  //push the Grt value as Lua userdata
  myx_lua_push_wrap_grt_value(l, value);

  return 1;
}

static int l_grt_value_get_type(lua_State *l)
{
  MYX_GRT_VALUE *value;

  // get value
  if ((value= luaL_checkgrtudata(l, -1)))
    lua_pop(l, 1);
  else
    luaL_error(l, "the first argument has to be a GRT value");

  if (value)
  {
    if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
      lua_pushstring(l, myx_get_value_type_as_string(myx_grt_list_content_get_type(value)));
    else
      lua_pushstring(l, myx_get_value_type_as_string(myx_grt_dict_content_get_type(value)));
  }
  else
    lua_pushnil(l);
  
  return 1;
}


static int l_print(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  int i, n= lua_gettop(l);
  // invert the stack
  for (i= 0; i < n; i++) lua_insert(l, i);

  // print the stack
  while ((n= lua_gettop(l)) > 0)
  {
    if (luaL_checkgrtudata(l, -1))
    {
      // make sure this is not the lua context
      LUA_CONTEXT *ctx= luaL_checkudata(l, -1, "MYX_GRT");

      if (!ctx)
      {
        MYX_GRT_VALUE *value= pop_retain_grt_value(l);
        myx_grt_value_print(myx_lua_get_grt(l), value);
        myx_grt_value_release(value);
      }
      else
        return 0;
    }
    else
    {
      const char *s;
      lua_getglobal(l, "tostring");
      lua_insert(l, -2);  /* move function to be called before the value */
      lua_call(l, 1, 1);
      s = lua_tostring(l, -1);  /* get result */
      if (s == NULL)
        return luaL_error(l, "`tostring' must return a string to `print'");
      //if (i>1) myx_grt_printf(grt, " ");
      myx_grt_printf(grt, "%s" NL, s);
      lua_pop(l, 1);  /* pop result and function */
    }
  }

  //myx_grt_printf(grt, NL);

  return 0;
}

static int l_input(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *text;
  const char *caption= NULL;
 
  if (lua_isstring(l, -1))
    caption= mlua_popstring(l);

  if (!caption) caption= "";

  if ((*grt->process_input)(caption, MYX_GRT_IO_STANDARD, &text, grt->process_input_data) < 0
      || !text)
    lua_pushnil(l);
  else
  {
    char *input= g_strdup(text);

    if (input[strlen(input)-1] == 10)
      input[strlen(input)-1]= 0;

    lua_pushstring(l, input);

    g_free(input);
  }
  
  return 1;
}

static int l_input_password(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *text;
  const char *caption= NULL;
 
  if (lua_isstring(l, -1))
    caption= mlua_popstring(l);

  if ((*grt->process_input)(caption, MYX_GR_IO_PASSWORD, &text, grt->process_input_data) < 0)
    lua_pushnil(l);
  else
    lua_pushstring(l, text);
  
  return 1;
}



static void lua_tracer(lua_State *l, lua_Debug *ar)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  lua_Debug info;

  lua_getstack(l, 0, &info);
  lua_getinfo(l, "S", &info);

  myx_grt_printf(grt, "Lua: %s:%i", info.source, ar->currentline);
}

static void lua_tracer2(lua_State *l, lua_Debug *ar)
{
  lua_Debug info;
  
  lua_getstack(l, 0, &info);
  lua_getinfo(l, "S", &info);
  
  printf("Lua: %s:%i\n", info.source, ar->currentline);
}


static int l_trace_enable(lua_State *l)
{
  if (luaL_checkint(l, -1) == 1)
  {
    lua_sethook(l, lua_tracer, LUA_MASKLINE, 0);
  }
  else if (luaL_checkint(l, -1) == 2)
  {
    lua_sethook(l, lua_tracer2, LUA_MASKLINE, 0);
  }  
  else
    lua_sethook(l, NULL, LUA_MASKLINE, 0);
  
  lua_pop(l, 1);
  
  return 0;
}


static int l_backtrace(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  lua_Debug info;
  int i;
  
  myx_grt_printf(grt, "Current Lua Stacktrace:");
  for (i= 1; ; i++)
  {
    if (lua_getstack(l, i, &info) == 0)
      break;
    lua_getinfo(l, "Snl", &info);
    
    myx_grt_printf(grt, "#%i  %s %s at %s %s:%i", i,
                  info.namewhat, info.name,
                  info.what, info.source, info.currentline);
  }
  return 0;
}


static int l_grt_value_refcount(lua_State *l)
{  
  MYX_GRT_VALUE *value;

  // get value
  if ((value= luaL_checkgrtudata(l, -1)))
    lua_pushnumber(l, _myx_grt_get_refcount(value));
  else
    luaL_error(l, "the first argument has to be a GRT value");

  return 1;
}



static int l_loadstructs(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *path;
  MYX_GRT_ERROR err;

  path= mlua_popstring(l);

  err= myx_grt_struct_load_and_register(grt, path);
  if (err == MYX_GRT_CANT_OPEN_FILE)
    luaL_error(l, "can't open the structs file");
  else if (err != MYX_GRT_NO_ERROR)
    luaL_error(l, "can't register structs");

  return 0;
}


static int l_validate(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname;
  MYX_GRT_VALUE *value;
  MYX_GRT_STRUCT *gstruct;
  int strict;
  int argc= lua_gettop(l);

  strict= (int)luaL_checknumber(l, -1);
  lua_pop(l, 1);

  if (argc > 2)
    sname= mlua_popstring(l);
  else
    sname= NULL;

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "missing value to validate");

  if (argc == 2)
  {
    if (value->type == MYX_DICT_VALUE)
      sname= myx_grt_dict_struct_get_name(value);
  }

  if (sname)
  {
    gstruct= myx_grt_struct_get(grt, sname);
    if (!gstruct)
    {
      myx_grt_value_release(value);
      luaL_error(l, "bad struct %s", sname);
    }
  }
  else
  {
    if (argc == 2)
    {
      // was asked to validate against the assigned struct. if there's none, then its ok
      myx_grt_value_release(value);
      lua_pushnumber(l, 1);
      return 1;
    }
  }

  if (value->type != MYX_DICT_VALUE)
  {
    myx_grt_value_release(value);
    lua_pushnumber(l, 0);
    return 1;
  }

  if (myx_grt_dict_struct_validate(grt, value, sname, strict)>0)
    lua_pushnumber(l, 1);
  else
    lua_pushnumber(l, 0);

  myx_grt_value_release(value);

  return 1;
}


static int l_get_struct(lua_State *l)
{
  const char *sname;
  MYX_GRT_VALUE *value;

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "missing value");
  
  if (value->type != MYX_DICT_VALUE && value->type != MYX_LIST_VALUE)
  {
    myx_grt_value_release(value);
    luaL_error(l, "value must be of type list or dict");
  }

  sname= myx_grt_dict_struct_get_name(value);
  
  if (!sname)
    lua_pushnil(l);
  else
    lua_pushstring(l, sname);

  myx_grt_value_release(value);

  return 1;
}


static int l_get_struct_members(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname;
  MYX_GRT_STRUCT *gstruct;
  unsigned int i, tbl_stack_pos, count;

  sname= mlua_popstring(l);
  if (!(gstruct= myx_grt_struct_get(grt, sname)))
    luaL_error(l, "unknown struct name '%s'", sname);

  lua_newtable(l);
  tbl_stack_pos= lua_gettop(l);

  count= myx_grt_struct_get_member_count_total(grt, gstruct);
  for (i= 0; i < count; i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index_total(grt, gstruct, i);

    lua_pushnumber(l, i + 1);
    lua_pushstring(l, myx_grt_struct_get_member_name(member));

    lua_settable(l, tbl_stack_pos);
  }

  return 1;
}

static int l_get_struct_member_type(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname, *mname;
  MYX_GRT_STRUCT *gstruct;
  MYX_GRT_STRUCT_MEMBER *member;

  mname= mlua_popstring(l);
  sname= mlua_popstring(l);

  if (!(gstruct= myx_grt_struct_get(grt, sname)))
    luaL_error(l, "unknown struct name '%s'", sname);

  if (!(member= myx_grt_struct_get_member_by_name(grt, gstruct, mname, 1)))
    luaL_error(l, "unknown member name '%s.%s'", sname, mname);


  lua_pushstring(l, myx_get_value_type_as_string(myx_grt_struct_member_get_type(member)));

  return 1;
}

static int l_get_struct_member_content_type(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname, *mname;
  MYX_GRT_STRUCT *gstruct;
  MYX_GRT_STRUCT_MEMBER *member;

  mname= mlua_popstring(l);
  sname= mlua_popstring(l);

  if (!(gstruct= myx_grt_struct_get(grt, sname)))
    luaL_error(l, "unknown struct name '%s'", sname);

  if (!(member= myx_grt_struct_get_member_by_name(grt, gstruct, mname, 1)))
    luaL_error(l, "unknown member name '%s.%s'", sname, mname);


  lua_pushstring(l, myx_get_value_type_as_string(myx_grt_struct_member_get_content_type(member)));

  return 1;
}

static int l_get_struct_member_content_struct(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname, *mname;
  MYX_GRT_STRUCT *gstruct;
  MYX_GRT_STRUCT_MEMBER *member;

  mname= mlua_popstring(l);
  sname= mlua_popstring(l);

  if (!(gstruct= myx_grt_struct_get(grt, sname)))
    luaL_error(l, "unknown struct name '%s'", sname);

  if (!(member= myx_grt_struct_get_member_by_name(grt, gstruct, mname, 1)))
    luaL_error(l, "unknown member name '%s.%s'", sname, mname);

  lua_pushstring(l, myx_grt_struct_member_get_content_struct_name_overridden(member));

  return 1;
}

static int l_struct_inherits_from(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname, *parent_name;

  parent_name= mlua_popstring(l);
  sname= mlua_popstring(l);

  if (myx_grt_struct_inherits_from(grt, sname, parent_name))
    lua_pushboolean(l, 1);
  else
    lua_pushboolean(l, 0);

  return 1;
}


static int l_struct_is_or_inherits_from(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname, *parent_name;
  
  parent_name= mlua_popstring(l);
  sname= mlua_popstring(l);
  
  if (myx_grt_struct_is_or_inherits_from(grt, sname, parent_name))
    lua_pushboolean(l, 1);
  else
    lua_pushboolean(l, 0);
  
  return 1;
}

static int l_get_struct_caption(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname;
  MYX_GRT_STRUCT *gstruct;
  int inherited_caption;

  sname= mlua_popstring(l);
  if (!(gstruct= myx_grt_struct_get(grt, sname)))
    luaL_error(l, "unknown struct name '%s'", sname);

  lua_pushstring(l, myx_grt_struct_get_caption(grt, gstruct, &inherited_caption));

  return 1;
}


static int l_setstruct(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *sname;
  MYX_GRT_VALUE *value;

  sname= mlua_popstring(l);
  if (!myx_grt_struct_get(grt, sname))
    luaL_error(l, "unknown struct name '%s'", sname);

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "missing value");

  if (value->type == MYX_DICT_VALUE)
    myx_grt_dict_struct_set_name(grt, value, sname);
  else if (value->type == MYX_LIST_VALUE)
    myx_grt_list_content_set_struct_name(value, sname);
  else
  {
    myx_grt_value_release(value);
    luaL_error(l, "value must be of type list or dict");
  }
  
  myx_grt_value_release(value);
  
  return 0;
}

static int l_struct_exists(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);

  if (!lua_isstring(l, -1))
    luaL_error(l, "missing struct name");

  if (myx_grt_struct_get(grt, mlua_popstring(l)))
    lua_pushboolean(l, 1);
  else
    lua_pushboolean(l, 0);

  return 1;
}


/*static int l_setcontenttype(lua_State *l)
{
//  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *content_type_name;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE_TYPE content_type;
  MYX_GRT_ERROR error;

  content_type_name= mlua_popstring(l);
  content_type= myx_get_value_type_from_string(content_type_name, &error);
  if (error != MYX_GRT_NO_ERROR)
    luaL_error(l, "the content type has to be \"\", \"string\", \"int\", \"real\", \"list\" or \"dict\".");

  value= pop_retain_grt_value(l);
  if (!value)
    luaL_error(l, "missing value");

  if (value->type == MYX_DICT_VALUE)
    myx_grt_dict_content_set_type(value, content_type);
  else if (value->type == MYX_LIST_VALUE)
    myx_grt_list_content_set_type(value, content_type);
  else
  {
    myx_grt_value_release(value);
    luaL_error(l, "value must be of type list or dict");
  }
  
  myx_grt_value_release(value);
  
  return 0;
}*/

static int l_run(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  const char *path;
  MYX_GRT_ERROR err;

  path= mlua_popstring(l);

  err= shell_run_file(grt, path, 1);
  if (err == MYX_GRT_CANT_OPEN_FILE)
    luaL_error(l, "can't open the script file");
  else if (err != MYX_GRT_NO_ERROR)
    luaL_error(l, "error while executing script");

  return 0;
}

static int l_sleep(lua_State *l)
{
  int ms= luaL_checkint(l, -1);
  lua_pop(l, 1);

#ifdef _WINDOWS
  Sleep(ms);
#else
  usleep(ms*1000);
#endif

  return 0;
}

static int l_exit(lua_State *l)
{
  int st= luaL_checkint(l, -1);
  
  exit(st);
  
  return 0;
}

static int l_regex_val(lua_State *l)
{
  const char *txt;
  const char *regex;
  int substr_nr;
  char *val;
  
  if (lua_isnumber(l, -1))
  {
    substr_nr= luaL_checkint(l, -1);
    lua_pop(l, 1);
  }
  else
    substr_nr= 1;

  if (!lua_isstring(l, -1))
    luaL_error(l, "missing regex");
  regex= mlua_popstring(l);

  if (!lua_isstring(l, -1))
    luaL_error(l, "missing text");
  txt= mlua_popstring(l);

  val= get_value_from_text_ex(txt, (int)strlen(txt), regex, substr_nr);

  if (val)
  {
    lua_pushstring(l, val);
    g_free(val);
  }
  else
    lua_pushstring(l, "");

  return 1;
}

static int l_str_replace(lua_State *l)
{
  const char *text;
  const char *from;
  const char *to;
  char *result;
 
  to= mlua_popstring(l);
  from= mlua_popstring(l);
  text= mlua_popstring(l);
  
  result= str_g_subst(text, from, to);

  lua_pushstring(l, result);
  g_free(result);

  return 1;
}


static int l_agent_connect(lua_State *l)
{
  MYX_GRT_AGENT_SESSION *session;
  const char *host;
  int port;

  port= luaL_checkint(l, -1);
  lua_pop(l, 1);

  host= mlua_popstring(l);
  
  session= myx_grt_remote_connect(host, port, "");
  if (!session)
  {
    luaL_error(l, "can't connect to remote agent.");
    return 0;
  }
  else
  {
    MYX_GRT_AGENT_SESSION **sess= lua_newuserdata(l, sizeof(MYX_GRT_AGENT_SESSION*));
    *sess= session;

    luaL_newmetatable(l, "MYX_AGENT");
    lua_setmetatable(l, -2);

    return 1;
  }
}


static int l_agent_close(lua_State *l)
{
  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    lua_pop(l, 1);
    myx_grt_remote_session_close(sess);
  }
  else
    luaL_error(l, "MYX_AGENT value expected as argument");
  return 0;
}


static int l_agent_invoke(lua_State *l)
{
  if (luaL_checkudata(l, -4, "MYX_AGENT"))
  {
    MYX_GRT_VALUE *value= pop_retain_grt_value(l);
    const char *function= mlua_popstring(l);
    const char *module= mlua_popstring(l);
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);

    MYX_GRT_AGENT_STATUS status;
    
    if (!sess || !module || !function || !value)
    {
      if (value) myx_grt_value_release(value);
      luaL_error(l, "insufficient arguments");
    }
    else
    {
      status= myx_grt_remote_function_invoke(sess, module, function, value);
    
      myx_grt_value_release(value);
      
      lua_pushnumber(l, status);

      return 1;
    }
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");

  return 0;
}


static int l_agent_check(lua_State *l)
{
  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    MYX_GRT_AGENT_STATUS status;
    
    lua_pop(l, 1);
    
    status= myx_grt_remote_function_check(sess);
    
    lua_pushnumber(l, status);

    return 1;
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");
  
  return 0;
}


static int l_agent_finish(lua_State *l)
{
  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    MYX_GRT_AGENT_STATUS status;
    MYX_GRT_VALUE *value;
    MYX_GRT_ERROR error;

    lua_pop(l, 1);

    value= myx_grt_remote_function_finish(sess, &error, &status);

    if (status != MYX_GRTA_FINISHED)
      luaL_error(l, "Error finalizing function (%i).", status);
    else
    {
      if (error == MYX_GRT_NO_ERROR)
      {
        if (value)
          myx_lua_push_wrap_grt_value(l, value);
        else
          lua_pushnil(l);
      }
      else
      {
        int tbl;

        lua_newtable(l);
        tbl= lua_gettop(l);
        
        lua_pushstring(l, "error");
        lua_pushstring(l, myx_grt_error_string(error));

        // needs to be tested
        lua_settable(l, tbl);

        lua_pushstring(l, "errno");
        lua_pushnumber(l, error);

        lua_settable(l, tbl);
      }
      return 1;
    }
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");
  
  return 0;
}


static int l_agent_messages(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);

  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    MYX_GRT_MSGS *msgs;

    lua_pop(l, 1);
    
    msgs= myx_grt_remote_get_messages(sess);
    if (msgs)
    {
      unsigned int i;
      for (i= 0; i < msgs->msgs_num; i++)
      {
        myx_grt_messages_stack_add(grt, msgs->msgs[i].msg_type, 
                                   msgs->msgs[i].msg,
                                   msgs->msgs[i].msg_detail, 
                                   0, 
                                   msgs->msgs[i].progress);
        g_free(msgs->msgs[i].msg);
      }
      g_free(msgs->msgs);
      g_free(msgs);
    }
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");

  return 0;
}


static int l_agent_setglobal(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);

  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    MYX_GRT_AGENT_STATUS status;

    lua_pop(l, 1);
    
    myx_grt_remote_set_tree(sess, myx_grt_get_root(grt), &status);

    if (status != MYX_GRTA_OK)
      luaL_error(l, "Error while sending root tree to agent (%i)", status);
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");

  return 0;
}


static int l_agent_getglobal(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);

  if (luaL_checkudata(l, -1, "MYX_AGENT"))
  {
    MYX_GRT_AGENT_SESSION *sess= *(MYX_GRT_AGENT_SESSION**)lua_touserdata(l, -1);
    MYX_GRT_VALUE *tree;
    MYX_GRT_AGENT_STATUS status;
    lua_pop(l, 1);

    tree= myx_grt_remote_get_tree(sess, &status);

    if (status != MYX_GRTA_OK)
      luaL_error(l, "Error while receiving root tree from agent (%i)", status);
    else if (tree)
      myx_grt_set_root(grt, tree);
  }
  else
    luaL_error(l, "MYX_AGENT expected as 1st argument.");

  return 0;
}

int myx_lua_refresh(MYX_GRT *grt, lua_State *l)
{
  int i;
  int count;
  MYX_GRT_MODULE **modules;
//  int module_name_list_tbl;

//  lua_newtable(l);
//  module_name_list_tbl= lua_gettop(l);

  // Will create tables representing the available modules containing
  // the functions it implements. You can then call the functions with
  // module->function()

  count= myx_grt_modules_get_that_extend(grt, NULL, &modules);
  for (i= 0; i < count; i++)
  {
    lua_pushstring(l, modules[i]->name);
    lua_newtable(l);
    myx_lua_add_grt_module_to_table(l, modules[i], lua_gettop(l));
    lua_settable(l, LUA_GLOBALSINDEX);

//    lua_getglobal(l, modules[i]->name);
//    lua_rawseti(l, module_name_list_tbl, i+1);
  }

  return 0;
}


static LUA_CONTEXT *myx_lua_get_ctx(lua_State *l)
{
  LUA_CONTEXT *ctx;
  lua_getglobal(l, "__GRT");

  ctx= luaL_checkudata(l, -1, "MYX_GRT");
  if (ctx)
  {
    lua_pop(l, 1);
    return ctx;
  }
  return NULL;
}


static int gc_function(lua_State *l)
{
  MYX_GRT_VALUE *value= luaL_checkgrtudata(l, 1);
  if (value)
    myx_grt_value_release(value);
  return 0;
}

static int gc_equals(lua_State *l)
{
  MYX_GRT_VALUE *value1= luaL_checkgrtudata(l, 1);
  MYX_GRT_VALUE *value2= luaL_checkgrtudata(l, 2);
  lua_pop(l, 2);
  return value1 == value2;
}

static int l_grt_value_getn(lua_State *l)
{
  MYX_GRT_VALUE *value= pop_retain_grt_value(l);
  if (!value || ((myx_grt_value_get_type(value) != MYX_LIST_VALUE) && 
    (myx_grt_value_get_type(value) != MYX_DICT_VALUE)))
  {
    if (value)
      myx_grt_value_release(value);
    luaL_error(l, "Invalid parameter: expected list or dict value");
  }
  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
    lua_pushnumber(l, myx_grt_list_item_count(value));
  else
    lua_pushnumber(l, myx_grt_dict_item_count(value));
  myx_grt_value_release(value);
  return 1;
}

static int l_grt_value_insert(lua_State *l)
{
  MYX_GRT_VALUE *value= NULL;
  MYX_GRT_VALUE *rvalue= NULL;
  int index= -1;

  if (lua_gettop(l) == 3)
  {
    rvalue= pop_retain_grt_value(l);
    value= luaL_checkgrtudata(l, -2);
    index= (int) lua_tonumber(l, -1);
    lua_pop(l, 2);
  }
  else if (lua_gettop(l) == 2)
  {
    rvalue= pop_retain_grt_value(l);
    value= luaL_checkgrtudata(l, -1);
    lua_pop(l, 1);
    index= -1;
  }
  if (!value || (myx_grt_value_get_type(value) != MYX_LIST_VALUE))
  {
    if (rvalue)
      myx_grt_value_release(rvalue);
    luaL_error(l, "Invalid parameter: expected GRT list value as 1st parameter");
  }
  if (!rvalue)
    luaL_error(l, "Invalid object to be inserted to list");
  if (index == 0)
  {
    if (rvalue)
      myx_grt_value_release(rvalue);
    luaL_error(l, "List index starts at 1");
  }
  if (index > 0)
    index--;
  if (index >= (int)myx_grt_list_item_count(value))
  {
    if (rvalue)
      myx_grt_value_release(rvalue);
    luaL_error(l, "List index out of bounds");
  }
  if (index < 0 || (unsigned int) index >= myx_grt_list_item_count(value))
    myx_grt_list_item_add(value, rvalue);
  else
    myx_grt_list_item_insert(value, (unsigned int)index, rvalue);
  
  myx_grt_value_release(rvalue);
  
  return 0;
}

static int l_grt_value_remove(lua_State *l)
{  
  MYX_GRT_VALUE *value;
  int index;

  value= luaL_checkgrtudata(l, -2);
  index= luaL_checkint(l, -1);
  lua_pop(l, 2);
  if (!value || (myx_grt_value_get_type(value) != MYX_LIST_VALUE))
    luaL_error(l, "Invalid parameter: expected list value");
  if (index == 0)
    luaL_error(l, "List index starts at 1");
  index--;
  if (index < 0 || (unsigned int) index >= myx_grt_list_item_count(value))
    luaL_error(l, "Invalid list index");
  myx_grt_list_item_del(value, index);
  return 0;
}

static int l_grt_list_clear(lua_State *l)
{  
  MYX_GRT_VALUE *value;

  value= luaL_checkgrtudata(l, -1);
  lua_pop(l, 1);
  if (!value || (myx_grt_value_get_type(value) != MYX_LIST_VALUE))
    luaL_error(l, "Invalid parameter: expected list value");

  myx_grt_list_clear(value);
  return 0;
}

static int l_grt_value_remove_object(lua_State *l)
{  
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE *object;
  int i, c;
  
  value= luaL_checkgrtudata(l, -2);
  object= luaL_checkgrtudata(l, -1);
  lua_pop(l, 2);
  if (!value || (myx_grt_value_get_type(value) != MYX_LIST_VALUE))
    luaL_error(l, "Invalid parameter 1: expected list value");
  if (!object)
    luaL_error(l, "Invalid parameter 2: expected GRT value");
  c= myx_grt_list_item_count(value);
  for (i= c-1; i >= 0; --i)
  {
    MYX_GRT_VALUE *tmp= myx_grt_list_item_get(value, i);
    if (tmp == object)
      myx_grt_list_item_del(value, i);
  }
  return 0;
}

static int l_grt_get_key(lua_State *l)
{  
  MYX_GRT_VALUE *value;
  int index;

  value= luaL_checkgrtudata(l, -2);
  index= (int) lua_tonumber(l, -1);
  lua_pop(l, 2);
  if (!value || (myx_grt_value_get_type(value) != MYX_DICT_VALUE))
    luaL_error(l, "Invalid parameter: expected dict value");
  if (index == 0)
    luaL_error(l, "Dict key index starts at 1");
  index--;
  if (index < 0 || (unsigned int) index >= myx_grt_dict_item_count(value))
    luaL_error(l, "Invalid list index");

  lua_pushstring(l, myx_grt_dict_item_key_by_index(value, index));
  return 1;
}

static int l_grt_get_list_item_by_obj_name(lua_State *l)
{
  MYX_GRT_VALUE *list;
  MYX_GRT_VALUE *value;
  const char *name;

  if (!lua_isstring(l, -1))
    luaL_error(l, "missing name");
  name= mlua_popstring(l);

  list= luaL_checkgrtudata(l, -1);
  if (!list || (myx_grt_value_get_type(list) != MYX_LIST_VALUE))
    luaL_error(l, "Invalid parameter: expected list value");
  else
    lua_pop(l, 1);

  value= myx_grt_list_item_get_by_object_name(list, name);
  if (value)
  {
    myx_grt_value_retain(value);
    myx_lua_push_wrap_grt_value(l, value);
  }
  else
    lua_pushnil(l);

  return 1;
}

static int l_grt_get_list_ref_value_by_obj_name(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *list;
  MYX_GRT_VALUE *value;
  const char *name;

  if (!lua_isstring(l, -1))
    luaL_error(l, "missing name");
  name= mlua_popstring(l);

  list= luaL_checkgrtudata(l, -1);
  if (!list || (myx_grt_value_get_type(list) != MYX_LIST_VALUE))
    luaL_error(l, "Invalid parameter: expected list value");
  else
    lua_pop(l, 1);

  value= myx_grt_list_item_get_reference_value_by_object_name(grt, list, name);
  if (value)
  {
    myx_grt_value_retain(value);
    myx_lua_push_wrap_grt_value(l, value);
  }
  else
    lua_pushnil(l);

  return 1;
}

static int l_grt_value_duplicate(lua_State *l)
{  
  MYX_GRT_VALUE *value, *value_dup;

  value= luaL_checkgrtudata(l, -1);
  lua_pop(l, 1);

  value_dup= myx_grt_value_dup(value);
  myx_lua_push_wrap_grt_value(l, value_dup);

  return 1;
}

static int l_grt_value_diff_make(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *source= NULL, *target= NULL, *diff;

  if (lua_gettop(l) == 2)
  {
    target= pop_retain_grt_value(l);
    if (!target)
      luaL_error(l, "The second argument is not a GRT value.");
    source= pop_retain_grt_value(l);
    if (!source)
    {
      myx_grt_value_release(target);
      luaL_error(l, "The first argument is not a GRT value.");
    }
  }
  else
    luaL_error(l, "This functions needs two GRT values as arguments.");

  diff= myx_grt_value_diff_make(grt, source, target);
  if (diff)
    myx_lua_push_wrap_grt_value(l, diff);
  else
    lua_pushnil(l);
  
  if (target) myx_grt_value_release(target);
  if (source) myx_grt_value_release(source);
  
  return 1;
}

static int l_grt_value_diff_apply(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *value= NULL, *diff= NULL;

  if (lua_gettop(l) == 2)
  {
    diff= pop_retain_grt_value(l);
    if (!diff)
      luaL_error(l, "The second argument is not a GRT value.");
    value= pop_retain_grt_value(l);
    if (!value)
    {
      myx_grt_value_release(diff);
      luaL_error(l, "The first argument is not a GRT value.");
    }
  }
  else
    luaL_error(l, "This functions needs two GRT values as arguments.");

  value= myx_grt_value_diff_apply(grt, value, diff);
  if (value)
    myx_lua_push_wrap_grt_value(l, value);
  else
    lua_pushnil(l);
  
  myx_grt_value_release(diff);
  
  return 1;
}

//static int l_grt_value_change_tree_make(lua_State *l)
//{
//  MYX_GRT *grt= myx_lua_get_grt(l);
//  MYX_GRT_VALUE *source= NULL, *target= NULL, *diff;
//
//  if (lua_gettop(l) == 2)
//  {
//    target= pop_retain_grt_value(l);
//    if (!target)
//      luaL_error(l, "The second argument is not a GRT value.");
//    source= pop_retain_grt_value(l);
//    if (!source)
//    {
//      myx_grt_value_release(target);
//      luaL_error(l, "The first argument is not a GRT value.");
//    }
//  }
//  else
//    luaL_error(l, "This functions needs two GRT values as arguments.");
//
//  diff= myx_grt_value_change_tree_make(grt, source, target);
//  if (diff)
//    myx_lua_push_wrap_grt_value(l, diff);
//  else
//    lua_pushnil(l);
//  
//  if (target) myx_grt_value_release(target);
//  if (source) myx_grt_value_release(source);
//  
//  return 1;
//}

static int list_index_function(lua_State *l)
{
  MYX_GRT_VALUE *value= luaL_checkgrtudata(l, -2);
  MYX_GRT_VALUE *rvalue;
  
  if (lua_isnumber(l, -1))
  {
    int index= (int)lua_tonumber(l, -1);

    index--;
    if (index >= (int)myx_grt_list_item_count(value))
      luaL_error(l, "List index out of bounds");
    if (index < 0)
      luaL_error(l, "List index starts at 1");
    rvalue= myx_grt_list_item_get(value, index);
    myx_grt_value_retain(rvalue);
    myx_lua_push_wrap_grt_value(l, rvalue);
  }
  else if (lua_isstring(l, -1) && myx_grt_list_content_get_type(value) == MYX_DICT_VALUE)
  {
    const char *name= lua_tostring(l, -1);
    
    rvalue= myx_grt_list_item_get_by_object_name(value, name);
    if (rvalue)
    {
      myx_grt_value_retain(rvalue);
      myx_lua_push_wrap_grt_value(l, rvalue);
    }
    else
      lua_pushnil(l);
  }
  else
  {
    const char *member= luaL_checkstring(l, -1);
    if (strcmp(member, "count")==0)
      lua_pushnumber(l, myx_grt_list_item_count(value));
    else
      luaL_error(l, "Invalid method '%s' for list value.", member);
  }
  
  return 1;
}

static int list_newindex_function(lua_State *l)
{
  MYX_GRT_VALUE *dvalue;
  MYX_GRT_VALUE *value;
  lua_Number index;
  value= luaL_checkgrtudata(l, -3);
  index= luaL_checknumber(l, -2);
  dvalue= pop_retain_grt_value(l);
  lua_pop(l, 2);

  index--;
  if (index <= 0 || index == myx_grt_list_item_count(value))
    myx_grt_list_item_add(value, dvalue);
  else
    myx_grt_list_item_set(value, (unsigned int)index, dvalue);
  
  myx_grt_value_release(dvalue);
 
  return 0;
}


static int dict_index_function(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *dict= luaL_checkgrtudata(l, -2);
  MYX_GRT_VALUE *dvalue;
  const char *member= luaL_checkstring(l, -1);
  const char *struct_name;

  struct_name= myx_grt_dict_struct_get_name(dict);
  if (struct_name)
  {
    MYX_GRT_STRUCT *gstruct;
    MYX_GRT_STRUCT_MEMBER *smember;

    gstruct= myx_grt_struct_get(grt, struct_name);
    if (gstruct)
    {
      smember= myx_grt_struct_get_member_by_name(grt, gstruct, member, 1);
      if (smember && myx_grt_struct_member_get_is_ref(smember) && 
        (myx_grt_struct_member_get_type(smember) == MYX_STRING_VALUE) &&
        (myx_grt_value_get_type(myx_grt_dict_item_get_value(dict, member)) == MYX_STRING_VALUE))
      {
        dvalue= myx_grt_dict_item_get_reference_value(grt, dict, member);
        if (dvalue)
        {
          myx_grt_value_retain(dvalue);
          myx_lua_push_wrap_grt_value(l, dvalue);
          return 1;
        }
      }
    }
  }
  if (!(dvalue= myx_grt_dict_item_get_value(dict, member)))
    lua_pushnil(l);
    //luaL_error(l, "Invalid member: dictionary value doesn't contain %s", member);
  else
  {
    myx_grt_value_retain(dvalue);
    myx_lua_push_wrap_grt_value(l, dvalue);
  }
  return 1;
}


static int dict_newindex_function(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *dvalue;
  MYX_GRT_VALUE *dict;
  const char *member;
  const char *struct_name;

  dict= luaL_checkgrtudata(l, -3);
  member= luaL_checkstring(l, -2);
  dvalue= pop_retain_grt_value(l);
  lua_pop(l, 2);

  struct_name= myx_grt_dict_struct_get_name(dict);
  if (struct_name)
  {
    MYX_GRT_STRUCT *gstruct;
    MYX_GRT_STRUCT_MEMBER *smember;

    gstruct= myx_grt_struct_get(grt, struct_name);
    if (gstruct)
    {
      smember= myx_grt_struct_get_member_by_name(grt, gstruct, member, 1);
      if (smember && myx_grt_struct_member_get_is_ref(smember))
      {
        const char *ident;
        if (!dvalue || (myx_grt_value_get_type(dvalue) == MYX_DICT_VALUE &&
                        (ident= myx_grt_dict_item_get_as_string(dvalue, "_id"))))
        {
          if (!dvalue)
            myx_grt_dict_item_set_value_from_string(dict, member, "");
          else
            myx_grt_dict_item_set_value_from_string(dict, member, ident);
          return 0;
        }
        else
          luaL_error(l, "Value assigned to a GRT reference member is not a GRT object (dict with _id member)");
      }
    }
  }

  myx_grt_dict_item_set_value(dict, member, dvalue);
    
  return 0;
}


int myx_lua_push_wrap_grt_value(lua_State *l, MYX_GRT_VALUE *value)
{
  MYX_GRT_VALUE **ud_ptr;
  int idx,meta;

  ud_ptr= lua_newuserdata(l, sizeof(MYX_GRT_VALUE*));
  idx= lua_gettop(l);
  *ud_ptr= value;

  // metatable with GC function and accessors for dicts and lists
  switch (myx_grt_value_get_type(value))
  {
    case MYX_LIST_VALUE: luaL_newmetatable(l, "MYX_GRT_LIST"); break;
    case MYX_DICT_VALUE: luaL_newmetatable(l, "MYX_GRT_DICT"); break;
    default: luaL_newmetatable(l, "MYX_GRT_VALUE"); break;
  }
  meta= lua_gettop(l);
  lua_pushstring(l, "__gc");
  lua_pushcfunction(l, gc_function);
  lua_rawset(l, meta);

  lua_pushstring(l, "__eq");
  lua_pushcfunction(l, gc_equals);
  lua_rawset(l, meta);

  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
  {
    lua_pushstring(l, "__index");
    lua_pushcfunction(l, list_index_function);
    lua_rawset(l, meta);
    lua_pushstring(l, "__newindex");
    lua_pushcfunction(l, list_newindex_function);
    lua_rawset(l, meta);
  }
  else if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
  {
    lua_pushstring(l, "__index");
    lua_pushcfunction(l, dict_index_function);
    lua_rawset(l, meta);
    lua_pushstring(l, "__newindex");
    lua_pushcfunction(l, dict_newindex_function);
    lua_rawset(l, meta);
  }
  lua_setmetatable(l, idx);

  return 1;
}


static int myx_lua_add_grt_module_to_table(lua_State *l, MYX_GRT_MODULE *mod, int tbl)
{
  unsigned int i;

  lua_pushstring(l, "_name_");
  lua_pushstring(l, mod->name);
  lua_settable(l, tbl);

  lua_pushstring(l, "_extends_");
  if (mod->extends)
    lua_pushstring(l, mod->extends);
  else
    lua_pushnil(l);
  lua_settable(l, tbl);

  // add all functions to the table, so that they can be called as table.func()
  for (i= 0; i < mod->functions_num; i++)
  {
    lua_pushstring(l, mod->functions[i].name);
    lua_pushcfunction(l, myx_lua_call_grt_method);
    lua_settable(l, tbl);
  }

  return 1;
}


/** 
 ****************************************************************************
 * @brief push a GRT value into the Lua stack as a Lua value
 *
 *   Pushes a GRT value into stack of the given Lua environment, converting
 * the data to Lua types.
 * 
 * @param L  Lua state
 * @param value  GRT value
 *
 * @return number of objects in the stack
 ****************************************************************************
 */
int myx_lua_push_grt_value(lua_State *l, MYX_GRT_VALUE *value)
{
  unsigned int i;
  
  if (value)
  {
    switch (value->type)
    {
    case MYX_INT_VALUE:
      lua_checkstack(l, lua_gettop(l)+1);
      lua_pushnumber(l, value->value.i);
      break;
    case MYX_STRING_VALUE:
      lua_checkstack(l, lua_gettop(l)+1);
      if (value->value.s)
        lua_pushstring(l, value->value.s);
      else
        lua_pushnil(l);
      break;
    case MYX_REAL_VALUE:
      lua_checkstack(l, lua_gettop(l)+1);
      lua_pushnumber(l, value->value.r);
      break;
    case MYX_LIST_VALUE:
      lua_checkstack(l, lua_gettop(l)+value->value.l->items_num+1);
      lua_newtable(l);
      for (i= 0; i < value->value.l->items_num; i++)
      {
        myx_lua_push_grt_value(l, value->value.l->items[i]);
        lua_rawseti(l, -2, i+1);
      }
      break;
    case MYX_DICT_VALUE:
      lua_checkstack(l, lua_gettop(l)+value->value.d->items_num*2+1);
      lua_newtable(l);
      for (i= 0; i < value->value.d->items_num; i++)
      {
        lua_pushstring(l, value->value.d->items[i].key);
        myx_lua_push_grt_value(l, value->value.d->items[i].value);
        lua_rawset(l, -3);
      }
      break;
      
    case MYX_ANY_VALUE: g_assert(0); break;
    }
  }
  else
    lua_pushnil(l);

  return 1;
}


static MYX_GRT_VALUE *pop_retain_grt_value(lua_State *l)
{
 // MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_VALUE *res= NULL;

  // if this is a GRT value
  if ((res= luaL_checkgrtudata(l, -1)))
  {
    lua_pop(l, 1);
    myx_grt_value_retain(res);
  }

  if (!res)
  {
    res= myx_lua_pop_grt_value(l);
  }
  return res;
}


MYX_GRT_VALUE *myx_lua_pop_grt_value(lua_State *l)
{
  MYX_GRT_VALUE *value= NULL;
  switch (lua_type(l, -1))
  {
  case LUA_TBOOLEAN:
    value= myx_grt_value_from_int(lua_toboolean(l, -1));
    lua_pop(l, 1);
    break;
  case LUA_TNUMBER:
    {
      lua_Number n= lua_tonumber(l, -1);
      if (n - floor(n) == 0)
        value= myx_grt_value_from_int((int)n);
      else
        value= myx_grt_value_from_real(n);
      lua_pop(l, 1);
    }
    break;
  case LUA_TSTRING:
    value= myx_grt_value_from_string(lua_tostring(l, -1));
    lua_pop(l, 1);
    break;
  case LUA_TTABLE:
    {
      MYX_GRT_VALUE_TYPE list_type;
      int tbl= lua_gettop(l);
      int can_be_list= 1;
      int empty= 1;
      unsigned int i;
      unsigned int nexti;
      
//printf("popping table value\n");

      value= myx_grt_dict_new(NULL, NULL);

      nexti= 1;
      // first we create a dict from the table, checking if the indices
      // are numeric and in sequence
      lua_pushnil(l);
      while (lua_next(l, tbl) != 0)
      {
        MYX_GRT_VALUE *item_value;

        // handle value
        //item_value= myx_lua_pop_retain_grt_value(l);
        item_value= pop_retain_grt_value(l);

        // handle key
        lua_pushvalue(l, -1);
        myx_grt_dict_item_set_value(value, lua_tostring(l, -1), item_value);
        lua_pop(l, 1);

        myx_grt_value_release(item_value);

        if (lua_type(l, -1) != LUA_TNUMBER || lua_tonumber(l, -1) != nexti)
          can_be_list= 0;
        
        nexti++;
        empty= 0;
        // don't pop key, as it should be kept for lua_next() to fetch the
        // next item
      }
      lua_pop(l, 1);
      
      if (empty)
      {
        myx_grt_value_release(value);
        value= myx_grt_list_new(MYX_ANY_VALUE, NULL);
        break;
      }

      if (can_be_list && (value->value.d->items_num > 0))
      {
        // now we check if the values are all the same 
        // and remember the type of the list
        for (i= 0; i < value->value.d->items_num; i++)
        {
          if (i > 0 && list_type != value->value.d->items[i].value->type)
          {
            list_type= MYX_ANY_VALUE;
            break;
          }
          list_type= value->value.d->items[i].value->type;
        }
      }

      if (can_be_list && value->value.d->items_num > 0)
      {
        MYX_GRT_DICT *dict= value->value.d;

        value->type= MYX_LIST_VALUE;
        value->value.l= g_new0(MYX_GRT_LIST, 1);
        value->value.l->content_type= list_type;
        value->value.l->items_num= dict->items_num;
        value->value.l->items= g_new0(MYX_GRT_VALUE*, dict->items_num);

        for (i= 0; i < dict->items_num; i++)
        {
          int idx= atoi(dict->items[i].key)-1;

          value->value.l->items[idx]= dict->items[i].value;

          g_free(dict->items[i].key);
        }
        g_free(dict->items);
        g_free(dict);
      }
    }
    break;

  case LUA_TNIL:
    lua_pop(l, 1);
    break;

  case LUA_TUSERDATA:
   value= pop_retain_grt_value(l);
   break;

  case LUA_TFUNCTION:
  case LUA_TLIGHTUSERDATA:
  case LUA_TTHREAD:
    g_warning("Invalid data (type=%s) in a Lua result value",
              lua_typename(l, lua_type(l, -1)));
    lua_pop(l, 1);
    break;
  }

  return value;
}

/*
static int myx_lua_call_value_method(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  MYX_GRT_FUNCTION *func;
  lua_Debug dbg;
  const char *name;
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *argument;
  MYX_GRT_VALUE *retval;

  // pop argument from stack, if its there
  if (lua_gettop(l)==1)
    argument= NULL;
  else if (lua_gettop(l)==2)
    argument= pop_retain_grt_value(l);
  else
    luaL_error(l, "too many arguments");

  // get info about the function that's being called
  lua_getstack(l, 0, &dbg);
  lua_getinfo(l, "n", &dbg);

  // get name of the module from the 1st table in stack (which is "self")
  lua_pushstring(l, "_name_");
  lua_gettable(l, -2);

  name= lua_tostring(l, -1);
  lua_pop(l, 1);

  func= myx_grt_function_get(grt, name, dbg.name, 1);
  g_return_val_if_fail(func != NULL, 0);

  retval= NULL;

  // if there was no argument given or the argument was a single value (and not a list as required)
  // create a new list and add the given argument to that list
  if ((argument == NULL) || (myx_grt_value_get_type(argument) != MYX_LIST_VALUE))
  {
    MYX_GRT_VALUE *argument_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);

    if (argument)
      myx_grt_list_item_add(argument_list, argument);

    retval= myx_grt_function_call(grt, func, argument_list, &error);

    myx_grt_value_release(argument_list);
  }
  else
    retval= myx_grt_function_call(grt, func, argument, &error);

  if (retval)
  {
    MYX_GRT_VALUE *value;
    value= myx_grt_dict_item_get_value(retval, "value");
    if (!value)
    {
      lua_pushstring(l, "grtError");
      myx_lua_push_grt_value(l, retval);
      lua_settable(l, LUA_GLOBALSINDEX);
      
      lua_pushnil(l);
    }
    else
    {
      lua_pushstring(l, "grtError");
      lua_pushnil(l);
      lua_settable(l, LUA_GLOBALSINDEX);

      myx_lua_push_wrap_grt_value(l, value);
    }
  }
  else
  {
   lua_pushstring(l, "grtError");
   lua_pushnil(l);
   lua_settable(l, LUA_GLOBALSINDEX);

   lua_pushnil(l);
  }

  if (argument)
    myx_grt_value_release(argument);

  if (error != MYX_GRT_NO_ERROR)
    return luaL_error(l, "error calling GRT function %s.%s (%i)",
                      name, dbg.name, error);
  else
    return 1;
}
 */

static int myx_lua_call_grt_method_by_name(MYX_GRT *grt, lua_State *l, const char *name, 
                                           const char *func_name, MYX_GRT_VALUE *argument)
{
  MYX_GRT_FUNCTION *func;
  MYX_GRT_VALUE *retval;
  MYX_GRT_ERROR error;
  retval= NULL;

  func= myx_grt_function_get(grt, name, func_name, 1);
  if (!func)
    return luaL_error(l, "the GRT function %s.%s does not exist", name, func_name);

  // if there was no argument given or the argument was a single value (and not a list as required)
  // create a new list and add the given argument to that list
  if ((argument == NULL) || (myx_grt_value_get_type(argument) != MYX_LIST_VALUE))
  {
    MYX_GRT_VALUE *argument_list= myx_grt_list_new(MYX_ANY_VALUE, NULL);

    if (argument)
      myx_grt_list_item_add(argument_list, argument);

    retval= myx_grt_function_call(grt, func, argument_list, &error);

    myx_grt_value_release(argument_list);
  }
  else
    retval= myx_grt_function_call(grt, func, argument, &error);

  if (retval)
  {
    MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(retval, "value");
    MYX_GRT_VALUE *error= myx_grt_dict_item_get_value(retval, "error");

    // if there is an error entry, push the return value to the Lua global grtError
    if (error)
    {
      lua_pushstring(l, "grtError");
      myx_lua_push_wrap_grt_value(l, retval);
      lua_settable(l, LUA_GLOBALSINDEX);

      lua_pushnil(l);
    }
    // if there is no error, set the Lua global grtError to nil and push value
    else
    {      
      lua_pushstring(l, "grtError");
      lua_pushnil(l);
      lua_settable(l, LUA_GLOBALSINDEX);

      if (value)
      {
        myx_grt_value_retain(value);
        myx_lua_push_wrap_grt_value(l, value);
      }
      else
        lua_pushnil(l);

      myx_grt_value_release(retval);
    }
  }
  else
  {
    lua_pushstring(l, "grtError");
    lua_pushnil(l);
    lua_settable(l, LUA_GLOBALSINDEX);

    lua_pushnil(l);
  }

  return 1;
}

static int myx_lua_call_grt_method(lua_State *l)
{
  MYX_GRT *grt= myx_lua_get_grt(l);
  lua_Debug dbg;
  const char *name;
  MYX_GRT_VALUE *argument;
  int l_top= lua_gettop(l);
  int res;

  // pop argument from stack, if its there
  if (l_top == 1)
    argument= NULL;
  else if (l_top == 2)
    argument= pop_retain_grt_value(l);
  else
    luaL_error(l, "too many arguments");

  // get info about the function that's being called
  lua_getstack(l, 0, &dbg);
  lua_getinfo(l, "n", &dbg);

  // get name of the module from the 1st table in stack (which is "self")
  lua_pushstring(l, "_name_");
  lua_gettable(l, -2);

  name= lua_tostring(l, -1);
  lua_pop(l, 1);

  if (name)
    res= myx_lua_call_grt_method_by_name(grt, l, name, dbg.name, argument);
  else
    luaL_error(l, "The module name is not set. Please check if you use modulename:function() name instead of modulename.function().");

  if (argument)
    myx_grt_value_release(argument);

  return res;
}


static void lua_shell_report_error(MYX_GRT *grt, int status)
{
  if (status != 0)
  {
    const char *msg= lua_tostring(get_lua_shell_state(grt), -1);
    myx_grt_printf(grt, "luart: error: %s"NL, msg);
    lua_pop(get_lua_shell_state(grt), 1);
  }
}


static int lua_shell_execute(MYX_GRT *grt, const char *linebuf)
{
  int rc= 0;
  int status= 0;
  MYX_GRT_SHELL_PRIVATE *lshell= grt->shell->data;
  
  if (lshell->current_line)
    lshell->current_line= str_g_append(lshell->current_line, linebuf);
  else
    lshell->current_line= g_strdup(linebuf);

  status= luaL_loadbuffer(lshell->lua, lshell->current_line, strlen(lshell->current_line), "=stdin");
  if (status == LUA_ERRSYNTAX && strstr(lua_tostring(lshell->lua, -1), "near `<eof>'"))
  {
    // line is continued
    lua_pop(lshell->lua, 1);
    return 1;
  }

  if (status == 0)
    status= lua_pcall(lshell->lua, lua_gettop(lshell->lua)-1, 0, 0);
  else
    rc= -1;

  g_free(lshell->current_line);
  lshell->current_line= NULL;

  if (status!=0)
  {
    rc= -1;
    lua_shell_report_error(grt, status);
  }
  while (lua_gettop(lshell->lua) > 0)
  { // print stack contents
    lua_getglobal(lshell->lua, "print");
    lua_insert(lshell->lua, 1);
    if (lua_pcall(lshell->lua, lua_gettop(lshell->lua)-2, 0, 0) != 0)
    {
      myx_grt_printf(grt, "luart: error calling print (%s)"NL,
                     lua_tostring(lshell->lua, -1));
    }
  }

  return rc;
}


static MYX_GRT_SHELL_CONTEXT lua_shell_functions= {
  MYX_GRT_SHELL_LUA,
    NULL,
    shell_init,
    shell_print_welcome,
    shell_get_prompt,
    shell_execute,
    shell_run_file,
    shell_get_lua,
    shell_get_global_var,
    shell_set_global_var
};


/** 
 ****************************************************************************
 * @brief Initialize the Lua Shell frontend for the GRT.
 *
 *   Will initialize a Lua environment in the GRT, which may be used as
 * a Lua frontend/shell interface for the GRT.
 * 
 * @param grt  The GRT environment the shell belongs to.
 * 
 * @return GRT shell context
 ****************************************************************************
 */
MYX_GRT_SHELL_CONTEXT *myx_grt_setup_lua_shell(MYX_GRT *grt)
{
  MYX_GRT_SHELL_CONTEXT *shell;
  MYX_GRT_SHELL_PRIVATE *lshell;

  lshell= g_new0(MYX_GRT_SHELL_PRIVATE, 1);
  lshell->lua= NULL;
  lshell->current_line= NULL;

  shell= g_new0(MYX_GRT_SHELL_CONTEXT, 1);
  memcpy(shell, &lua_shell_functions, sizeof(lua_shell_functions));
  shell->data= lshell;

  return shell;
}


void myx_lua_register_functions(MYX_GRT *grt, lua_State *lua)
{
  LUA_CONTEXT *ctx;
  static luaL_reg grtValue[]= {
    {"toLua",         l_grt_value_to_lua},
    {"setGlobal",     l_set_global},
    {"getGlobal",     l_get_global},
    {"child",         l_get_child},
    {"newObj",        l_grt_value_new},
    {"newList",       l_grt_value_new_list},
    {"newDict",       l_grt_value_new_dict},
    {"setContentType",l_grt_value_set_type},
    {"getContentType",l_grt_value_get_type},
    {"getn",	        l_grt_value_getn},
    {"insert",        l_grt_value_insert},
    {"remove",        l_grt_value_remove},
    {"removeObj",     l_grt_value_remove_object},
    {"getKey",        l_grt_get_key},
    {"getListItemByObjName", l_grt_get_list_item_by_obj_name},
    {"getListRefValueByObjName", l_grt_get_list_ref_value_by_obj_name},
    {"load",          l_load_value},
    {"lookupAdd",     l_lookup_add},
    {"lookupId",      l_lookup_id},
    {"save",          l_save_value},
    {"fromXml",       l_grt_value_from_xml},
    {"toXml",         l_grt_value_to_xml},
    {"typeOf",        l_grt_value_type},
    {"duplicate",     l_grt_value_duplicate},
    {"diffMake",      l_grt_value_diff_make},
    {"diffApply",     l_grt_value_diff_apply},
    //{"changeTreeMake", l_grt_value_change_tree_make},
    {"refcount",      l_grt_value_refcount},
    {"clearList",     l_grt_list_clear},
    {NULL,            NULL}
  };
  static luaL_reg grtStruct[]= {
    {"load",                    l_loadstructs},
    {"exists",                  l_struct_exists},
    {"get",                     l_get_struct},
    {"getCaption",              l_get_struct_caption},
    {"getMembers",              l_get_struct_members},
    {"getMemberType",           l_get_struct_member_type},
    {"getMemberContentType",    l_get_struct_member_content_type},
    {"getMemberContentStruct",  l_get_struct_member_content_struct},
    {"inheritsFrom",            l_struct_inherits_from},
    {"isOrInheritsFrom",        l_struct_is_or_inherits_from},
    {"set",                     l_setstruct},
    {"validate",                l_validate},
    {NULL,                      NULL}
  };
  static luaL_reg grtAgent[]= {
    {"connect",    l_agent_connect},
    {"close",      l_agent_close},
    {"invoke",     l_agent_invoke},
    {"check",      l_agent_check},
    {"finish",     l_agent_finish},
    {"messages",   l_agent_messages},
    {"setGlobal",  l_agent_setglobal},
    {"getGlobal",  l_agent_getglobal},
    {NULL,         NULL}
  };
  static luaL_reg grtModules[]= {
    {"list",          l_list_modules},
    {"show",          l_show_module},
    {"get",           l_get_modules},
    {"getFunctions",  l_get_module_functions},
    {"callFunction",  l_call_function},
    {NULL,         NULL}
  };
  static luaL_reg grtUtil[]= {
    {"regExVal",          l_regex_val},
    {"replace",           l_str_replace},
    {NULL,         NULL}
  };

  // register a global __GRT variable 
  lua_pushstring(lua, "__GRT");
  ctx= lua_newuserdata(lua, sizeof(LUA_CONTEXT));
  ctx->grt= grt;
  ctx->cwd= g_strdup("/");
  luaL_newmetatable(lua, "MYX_GRT");
  lua_setmetatable(lua, -2);
  lua_settable(lua, LUA_GLOBALSINDEX);

  // override the built-in print with our own
  lua_register(lua, "print", l_print);
  // register our exported functions
  lua_register(lua, "input", l_input);
  lua_register(lua, "password", l_input_password);

  lua_register(lua, "trace", l_trace_enable);
  lua_register(lua, "backtrace", l_backtrace);
  
  lua_register(lua, "refresh", l_refresh);

  lua_register(lua, "cd", l_cd);
  lua_register(lua, "ls", l_ls);
  lua_register(lua, "dir", l_ls);
  lua_register(lua, "pwd", l_pwd);

  lua_register(lua, "run", l_run);
  lua_register(lua, "exit", l_exit);
  lua_register(lua, "sleep", l_sleep);

  luaL_openlib(lua, "grtV", grtValue, 0);
  lua_pop(lua, 1);
  luaL_openlib(lua, "grtA", grtAgent, 0);
  lua_pop(lua, 1);
  luaL_openlib(lua, "grtS", grtStruct, 0);
  lua_pop(lua, 1);
  luaL_openlib(lua, "grtM", grtModules, 0);
  lua_pop(lua, 1);
  luaL_openlib(lua, "grtU", grtUtil, 0);
  lua_pop(lua, 1);
}



static int shell_init(MYX_GRT *grt)
{
  lua_State *lua;
  MYX_GRT_SHELL_PRIVATE *lshell= grt->shell->data;
  MYX_GRT_MODULE_LOADER *loader;

  // The Lua module loader must be initialized before the shell
  loader= myx_grt_get_loader_of_type(grt, MYX_LUA_MODULE_TYPE);
  if (!loader)
    return -1;

  lua= loader->priv->lua;

  lshell->lua= lua;

  //already done in the loader initializer
  //myx_lua_register_functions(grt, lua);
  
  myx_lua_refresh(grt, lua);

  myx_grt_value_retain(grt->root);
  lua_pushstring(lua, MYX_SHELL_CURNODE);
  myx_lua_push_wrap_grt_value(lua, grt->root);
  lua_settable(lua, LUA_GLOBALSINDEX);
  
  return 0;
}


static MYX_GRT *myx_lua_get_grt(lua_State *l)
{
  LUA_CONTEXT *ctx;
  lua_getglobal(l, "__GRT");

  ctx= luaL_checkudata(l, -1, "MYX_GRT");
  if (ctx)
  {
    lua_pop(l, 1);
    return ctx->grt;
  }
  return NULL;
}

/** 
 ****************************************************************************
 * @brief Prints welcome text for the GRT shell.
 *
 * Shows a welcome text in the GRT shell.
 * 
 * @param grt  The GRT environment the shell belongs to.
 ****************************************************************************
 */
static void shell_print_welcome(MYX_GRT *grt)
{
  char *version= GRT_VERSION;

  myx_grt_printf(grt, "MySQL Generic Runtime Environment %s"NL, version);

  myx_grt_printf(grt, NL"Type 'help' or '?' for help. Type 'quit' to exit the shell."NL);
  myx_grt_printf(grt, "Lua Shell initialized."NL);
}


/** 
 ****************************************************************************
 * @brief Executes a Lua command in the Lua shell
 *
 * This will execute the given Lua command in the Lua Shell from the 
 * GRT environment. Some pre-processing will be performed in the command
 * to handle special GRT shell commands.
 * 
 * @param grt The GRT environment the shell belongs to.
 * @param linebuf Line containing the command to be executed.
 *
 * @return 
 ****************************************************************************
 */
static MYX_GRT_SHELL_COMMAND shell_execute(MYX_GRT *grt, const char *linebuf)
{
  char *cmd;
  unsigned int cmd_len;
  char *cmd_param= NULL;
  MYX_GRT_SHELL_COMMAND res= MYX_GRT_SHELL_COMMAND_UNKNOWN;
  char *preprocessed_cmd= NULL;
  char *line= g_strdup(linebuf);

  cmd= g_strdup(line);
  cmd= str_trim(cmd);
  cmd_len= (unsigned int)strlen(cmd);

  //Help command
  if (strcmp2(cmd, "help")==0)
  {
    myx_grt_shell_show_help(grt, NULL);

    res= MYX_GRT_SHELL_COMMAND_HELP;
  }
  else if ((cmd_param= get_value_from_text_ex(cmd, cmd_len, "^(help|\\\\h)\\s+([\\w\\/\\.]*)", 2)))
  {
    if(cmd_param[0])
      myx_grt_shell_show_help(grt, cmd_param);
    else
      myx_grt_shell_show_help(grt, NULL);

    res= MYX_GRT_SHELL_COMMAND_HELP;
  }
  else if((cmd_param= get_value_from_text_ex(cmd, cmd_len, "^(\\?|\\-\\?)\\s*([\\w\\/\\.]*)", 2)))
  {
    if(cmd_param[0])
      myx_grt_shell_show_help(grt, cmd_param);
    else
      myx_grt_shell_show_help(grt, NULL);

    res= MYX_GRT_SHELL_COMMAND_HELP;
  } 
  //Quit command
  else if( (strcmp2(cmd, "quit")==0) || (strcmp2(cmd, "quit;")==0) || (strcmp2(cmd, "exit")==0) ||
    (strcmp2(cmd, "exit;")==0) || (strcmp2(cmd, "\\q")==0) || (strcmp2(cmd, "q")==0) || (strcmp2(cmd, "\\e")==0))
  {
    myx_grt_printf(grt, "Exiting...");

    res= MYX_GRT_SHELL_COMMAND_EXIT;
  }
  else if( (strcmp2(cmd, "run")==0) || (str_beginswith(cmd, "\\r")) || (str_beginswith(cmd, "run ")))
  {
    char *file_name= get_value_from_text_ex(cmd, (int)strlen(cmd), "(run|\\\\r)\\s+(.+)", 2);

    if((file_name) && (file_name[0]))
    {
      preprocessed_cmd= g_strdup_printf("run(\"%s\")"NL, file_name);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
    else
    {
      myx_grt_shell_show_command_help(grt, "run");
      res= MYX_GRT_SHELL_COMMAND_HELP;
    }

    if(file_name)
      g_free(file_name);
  }
  // Automatically convert cd.. to cd(".." and
  //   cd objectname to cd("objectname")
  else if ((strcmp2(cmd, "cd")==0) || (strcmp2(cmd, "cd..")==0) || (str_beginswith(cmd, "cd ")))
  {
    char *path= get_value_from_text_ex(cmd, (int)strlen(cmd), "cd\\s*(.+)", 1);

    if((path) && (path[0]))
    {
      preprocessed_cmd= g_strdup_printf("cd(\"%s\")"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
    else
    {
      preprocessed_cmd= g_strdup_printf("print(pwd())"NL);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }

    if(path)
      g_free(path);
  }
  // Automatically convert ls -t to table.foreach(x, print)
  else if (str_beginswith(cmd, "ls -t "))
  {
    char *path= get_value_from_text_ex(cmd, (int)strlen(cmd), "ls\\s+\\-t\\s+(.+)", 1);

    if((path) && (path[0]))
    {
      preprocessed_cmd= g_strdup_printf("table.foreach(%s, print)"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
  }
  // Automatically convert ls -m module to grtM.show()
  else if (str_beginswith(cmd, "ls -m "))
  {
    char *path= get_value_from_text_ex(cmd, (int)strlen(cmd), "ls\\s+\\-m\\s+(.+)", 1);

    if((path) && (path[0]))
    {
      preprocessed_cmd= g_strdup_printf("grtM.show(\"%s\")"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
  }
  // Automatically convert ls -m to GrtM.list()
  else 
    // TODO: Parsing for the poor. What if there is more than a space char between the command and its parameter?
    if ((strcmp2(cmd, "ls -m") == 0) || (strcmp2(cmd, "dir -m") == 0))
    {
      preprocessed_cmd= g_strdup("grtM.list()"NL);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
  // Automatically convert ls to ls()
  else 
    if ((strcmp(cmd, "ls") == 0) || (strcmp(cmd, "dir") == 0) || str_beginswith(cmd, "ls ") || str_beginswith(cmd, "dir "))
  {
    preprocessed_cmd= g_strdup("ls()"NL);
    res= MYX_GRT_SHELL_COMMAND_STATEMENT;
  }
  // Automatically convert show to show(grt2Lua(pwd()))
  else if (strcmp2(cmd, "show") == 0)
  {
    preprocessed_cmd= g_strdup_printf("print("MYX_SHELL_CURNODE")"NL);
    res= MYX_GRT_SHELL_COMMAND_STATEMENT;
  }
  // Automatically convert show objectname to show(getGlobal("objectname"))
  else if (str_beginswith(cmd, "show "))
  {
    char *path= get_value_from_text_ex(cmd, (int)strlen(cmd), "show\\s+(.+)", 1);

    if ((path) && (path[0]))
    {
      preprocessed_cmd= g_strdup_printf("print(grtV.getGlobal(\"%s\"))"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }

    g_free(path);
  }
  g_free(cmd);
  if(cmd_param)
    g_free(cmd_param);

  //If the command is still unknown, it needs to be a Lua command
  if((res == MYX_GRT_SHELL_COMMAND_UNKNOWN) || (res == MYX_GRT_SHELL_COMMAND_STATEMENT))
  {
    if(!preprocessed_cmd)
      res= lua_shell_execute(grt, line);
    else
    {
      res= lua_shell_execute(grt, preprocessed_cmd);
      g_free(preprocessed_cmd);
    }

    g_free(line);
    
    if (res > 0)
      return MYX_GRT_SHELL_COMMAND_UNKNOWN;
    else if (res < 0)
      return MYX_GRT_SHELL_COMMAND_ERROR;
    else 
      return MYX_GRT_SHELL_COMMAND_STATEMENT;
  }

  g_free(line);

  return res;
}

/** 
 ****************************************************************************
 * @brief Returns a prompt suitable for displaying in the shell.
 *
 * This will return a string suitable for displaying in the Lua shell,
 * displaying special information such as current object path, line 
 * edit status etc.
 * 
 * @param grt The GRT environment the shell belongs to.
 *
 * @return A string containing the prompt text. Should be freed with g_free()
 ****************************************************************************
 */
static char *shell_get_prompt(MYX_GRT *grt)
{
  LUA_CONTEXT *ctx;
  
  ctx= myx_lua_get_ctx(get_lua_shell_state(grt));

  if (grt->shell->data->current_line)
    return g_strdup_printf("%s>> ", ctx ? ctx->cwd : "");
  else
    return g_strdup_printf("%s > ", ctx ? ctx->cwd : "");
}


/** 
 ****************************************************************************
 * @brief Returns the Lua object used in the Lua Shell of the GRT.
 *
 * @param grt  The GRT environment the shell belongs to.
 *
 * @return The lua_State object.
 ****************************************************************************
 */
static void *shell_get_lua(MYX_GRT *grt)
{
  g_return_val_if_fail(grt!=NULL, NULL);
  g_return_val_if_fail(grt->shell!=NULL, NULL);
  
  return grt->shell->data->lua;
}


/** 
 ****************************************************************************
 * @brief Loads and executes a Lua script file.
 *
 *   This will load the named file and attempt to execute it in the given
 * GRT environment. The script will have access to all modules that are
 * registered inside that environment.
 * 
 * @param grt The GRT environment the shell belongs to.
 * @param file_name name of the script file.
 *
 * @return 0 if the file is executed successfully
 * @return < 0 if there's an error executing the file
 ****************************************************************************
 */
static int shell_run_file(MYX_GRT *grt, const char *file_name, int interactive)
{
  lua_State *l= get_lua_shell_state(grt);
  int status= luaL_loadfile(l, file_name);
  int rc;

  if (interactive)
    myx_grt_printf(grt, "Opening script file %s ..."NL, file_name);

  if (status)
  {
    myx_grt_printf(grt, "Error in file: %s"NL, lua_tostring(l, -1));
    return -1;
  }

  if (interactive)
    myx_grt_printf(grt, "Executing script file %s ..."NLNL, file_name);

  status= lua_pcall(l, 0, LUA_MULTRET, 0);
  if (status)
  {
    myx_grt_printf(grt, "error executing script: %s"NL, lua_tostring(l, -1));
    lua_pop(l, 1);
    while (lua_gettop(l) > 0)
    {
      myx_grt_printf(grt, "    %s"NL, lua_tostring(l, -1));
      lua_pop(l, 1);
    }
    rc= -2;
  }
  else
    rc= 0;

  if ((rc == 0) && (interactive))
    myx_grt_printf(grt, "\nExecution finished."NL);

  return rc;
}

/** 
 ****************************************************************************
 * @brief Gets the value of a global variable in the lua shell 
 *
 *   This will return the value of a global lua variable as a MYX_GRT_VALUE
 * 
 * @param grt The GRT environment the shell belongs to.
 * @param var_name name of the lua variable
 *
 * @return if the var is found, the MYX_GRT_VALUE is returned otherwise NULL
 ****************************************************************************
 */
static MYX_GRT_VALUE * shell_get_global_var(MYX_GRT *grt, const char *var_name)
{
  MYX_GRT_VALUE *value= NULL;
  lua_State *l= get_lua_shell_state(grt);

  lua_getglobal(l, var_name);
  if (!lua_isnil(l, -1))
     value= pop_retain_grt_value(l);
  else
    lua_pop(l, 1);

  return value;
}

/** 
 ****************************************************************************
 * @brief Sets the value of a global variable in the lua shell 
 *
 *   This will set the value of a global lua variable to a MYX_GRT_VALUE
 * 
 * @param grt The GRT environment the shell belongs to.
 * @param var_name name of the lua variable
 * @param value the MYX_GRT_VALUE the variable should be set to
 *
 ****************************************************************************
 */
static int shell_set_global_var(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value)
{
  myx_grt_value_retain(value);
  myx_lua_push_wrap_grt_value(get_lua_shell_state(grt), value);
  lua_setglobal(get_lua_shell_state(grt), var_name);
  
  return 0;
}

  
int myx_grt_lua_shell_global_var_is_grt_value(MYX_GRT *grt, const char *var_name)
{
  int rc;

  lua_getglobal(get_lua_shell_state(grt), var_name);

  if (luaL_checkgrtudata(get_lua_shell_state(grt), -1))
    rc= 1;
  else
    rc= 0;
  
  lua_pop(get_lua_shell_state(grt), 1);
	
  return rc;
}

#endif
