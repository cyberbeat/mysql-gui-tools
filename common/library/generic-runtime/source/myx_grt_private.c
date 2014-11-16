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

//----------------------------------------------------------------------------------------------------------------------

/*
 * @brief return the loader struct for the required type
 *
 * @param grt - the GRT environment
 * @param type - type of module loader to look up
 * @return NULL if error, the loader if it's found.
 */
MYX_GRT_MODULE_LOADER *myx_grt_get_loader_of_type(MYX_GRT *grt, MYX_GRT_MODULE_TYPE type)
{
  unsigned int i;
  for (i= 0; i < grt->loaders_num; i++)
  {
    if (grt->loaders[i]->loader_type == type)
      return grt->loaders[i];
  }
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * @brief myx_grt_add_module -- adds a module to the list of modules of the GRT
 *
 * @param grt grt to add module
 * @param module to add. Must not be freed
 * @return MYX_GRT_NO_ERROR
 */
MYX_GRT_ERROR myx_grt_add_module(MYX_GRT *grt, MYX_GRT_MODULE *module)
{  
  if (grt->options & MYX_GRT_VERBOSE)
    myx_grt_printf(grt, "GRT registering module %s\n", module->name);

  grt->modules= g_realloc(grt->modules, sizeof(MYX_GRT_MODULE*)*(grt->modules_num+1));
  grt->modules[grt->modules_num++]= module;
  
  myx_grt_notify_listeners(grt, GRT_MODULE_ADD_NOTIFICATION, module);

  return MYX_GRT_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Searchs the internal module list and returns the first module with the given name (if there is one).
 *
 * @param grt The runtime to operate on.
 * @param name The name of the module to look for.
 * @return The module reference if one could be found. Otherwise NULL.
 */
MYX_GRT_MODULE* myx_grt_find_module(MYX_GRT *grt, char* name)
{
  MYX_GRT_MODULE* Result = NULL;
  unsigned int I;
  for (I = 0; I < grt->modules_num; ++I)
    if (strcmp(grt->modules[I]->name, name) == 0)
    {
      Result = grt->modules[I];
      break;
    };

  return Result;
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Removes the given module from the list of modules in the given runtime.
 *
 * @param grt grt to add module
 * @param module to add. Must not be freed
 */
void myx_grt_remove_module(MYX_GRT *grt, MYX_GRT_MODULE *module)
{
  unsigned int I;
  for (I = 0; I < grt->modules_num; ++I)
    if (grt->modules[I] == module)
    {
      if (I < grt->modules_num - 1)
        memcpy(grt->modules[I], grt->modules[I + 1], sizeof(MYX_GRT_MODULE*) * (grt->modules_num - I - 1));
      grt->modules = g_realloc(grt->modules, sizeof(MYX_GRT_MODULE*) * (--grt->modules_num));
      break;
    };
}

//----------------------------------------------------------------------------------------------------------------------

void myx_grt_parse_function_spec(const char *spec, MYX_GRT_FUNCTION *func)
{
  char *tmp= g_strdup(spec);

  func->name= g_strdup(strtok(tmp, ":"));
  func->param_struct_name= g_strdup(strtok(NULL, ":"));
  func->return_struct_name= g_strdup(strtok(NULL, ":"));

  g_free(tmp);
}

//----------------------------------------------------------------------------------------------------------------------



void myx_grt_add_listener(MYX_GRT *grt, void (*callback)(MYX_GRT*,char*,void*,void*),
                          char *name, void *userdata)
{
  MYX_GRT_LISTENER *listener;
  
  listener= g_new0(MYX_GRT_LISTENER, 1);
  
  listener->wanted_name= g_strdup(name);
  listener->userdata= userdata;
  listener->callback= callback;
  
  listener->next= grt->listeners;
  grt->listeners= listener;
}



void myx_grt_notify_listeners(MYX_GRT *grt, char *name, void *argument)
{
  MYX_GRT_LISTENER *listener= grt->listeners;
  
  while (listener)
  {
    if (!listener->wanted_name || strcmp(name, listener->wanted_name)==0)
    {
      (*listener->callback)(grt, name, argument, listener->userdata);
    }
    
    listener= listener->next;
  }
}
