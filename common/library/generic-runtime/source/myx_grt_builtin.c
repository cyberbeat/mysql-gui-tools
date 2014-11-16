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

#include "myx_grt_private.h"


typedef struct MYX_GRT_FUNCTION_PRIVATE
{
  MYX_GRT_VALUE *(*function)(MYX_GRT_VALUE*,void*);
} MYX_BUILTIN_FUNCTION;


static MYX_GRT_ERROR c_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);


MYX_GRT_MODULE_LOADER *myx_builtin_init_loader(MYX_GRT *grt)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);

  loader->grt= grt;
  loader->loader_type= MYX_BUILTIN_MODULE_TYPE;
  loader->priv= NULL;
  loader->init_module= NULL;
  loader->call_function= c_call_function;
  loader->extensions_num= 0;
  loader->extensions= NULL;

  return loader;
}


MYX_GRT_MODULE* myx_grt_module_register_builtin(MYX_GRT *grt, MYX_GRT_BUILTIN_MODULE *mod, void *function_data)
{
  MYX_GRT_MODULE *module;
  unsigned int i;
  MYX_GRT_ERROR error;

  GRT_ENTER(grt);
  
  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);

  module->loader= myx_grt_get_loader_of_type(grt, MYX_BUILTIN_MODULE_TYPE);
  module->priv= function_data;
  module->name= g_strdup(mod->name);
  module->path= NULL;
  module->functions_num= mod->functions_num;
  module->functions= g_new0(MYX_GRT_FUNCTION, module->functions_num);
  for (i= 0; i < mod->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    MYX_BUILTIN_FUNCTION *bf= g_new0(MYX_BUILTIN_FUNCTION,1);

    bf->function= mod->functions[i].function;

    func->module= module;
    myx_grt_parse_function_spec(mod->functions[i].name, func);
    func->priv= bf;
  }
  module->extends= g_strdup(mod->extends);
  
  if (getenv("GRT_VERBOSE"))
    g_message("Initialized module %s", mod->name);

  error= myx_grt_add_module(grt, module);
  if (error != MYX_GRT_NO_ERROR)
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE *);
  else
    GRT_RETURN(grt, module, MYX_GRT_MODULE *);
}

void myx_grt_module_unregister_builtin(MYX_GRT *grt, MYX_GRT_MODULE *module)
{
  unsigned int I;

  myx_grt_remove_module(grt, module);
  g_free(module->name);
  for (I = 0; I < module->functions_num; ++I)
  {
    MYX_GRT_FUNCTION *func = module->functions + I;
    g_free(func->priv);
  };
  g_free(module->functions);
  g_free(module->extends);
  g_free(module);
}

static MYX_GRT_ERROR c_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_BUILTIN_FUNCTION *bfunc= (MYX_BUILTIN_FUNCTION*)function->priv;
  MYX_GRT_ERROR error= MYX_GRT_NO_ERROR;

  if (getenv("GRT_VERBOSE"))
    g_message("Calling builtin function %s.%s", function->module->name, function->name);

  // check if a parameter list was submitted
  if ((myx_grt_value_get_type(value) == MYX_LIST_VALUE) && (myx_grt_bridge_list_item_count(value) > 0))
  {
    unsigned int i, global_ref= 0;

    // if this is a list, check if there is any string that begins with "global::" in the list
    for (i= 0; i < myx_grt_bridge_list_item_count(value); i++)
    {
      MYX_GRT_VALUE *arg= myx_grt_bridge_list_item_get(value, i, 0);

      // if the arg is a string, check if the string starts "global::"
      if ( (myx_grt_value_get_type(arg) == MYX_STRING_VALUE) &&
          str_beginswith(myx_grt_bridge_value_as_string(arg), "global::") )
      {
        global_ref= 1;
        break;
      }
    }

    if (global_ref == 1)
    {
      MYX_GRT_VALUE *new_args= myx_grt_list_new(MYX_ANY_VALUE, NULL);

      for (i= 0; i < myx_grt_bridge_list_item_count(value); i++)
      {
        MYX_GRT_VALUE *arg= myx_grt_bridge_list_item_get(value, i, 0);

        // if the arg is a string, check if the string starts "global::"
        if ( (myx_grt_value_get_type(arg) == MYX_STRING_VALUE) &&
            str_beginswith(myx_grt_bridge_value_as_string(arg), "global::") )
        {
          // if so, take the "real" grt global value instead of the string
          const char *path= myx_grt_bridge_value_as_string(arg)+8;

          arg= myx_grt_dict_item_get_by_path((MYX_GRT *)(function->module->priv),
            myx_grt_get_root((MYX_GRT *)(function->module->priv)), path);
        }

        myx_grt_list_item_add(new_args, arg);
      }

      *retval= (*bfunc->function)(new_args, function->module->priv);

      myx_grt_value_release(new_args);
    }
    else
      *retval= (*bfunc->function)(value, function->module->priv);
  }
  else
    *retval= (*bfunc->function)(value, function->module->priv);

  return error;
}
