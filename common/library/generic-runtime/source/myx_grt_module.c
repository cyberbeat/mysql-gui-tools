/* Copyright (c) 2005 MySQL AB
  
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
 * @file  myx_grt_module.c
 * @brief GRT module related functions.
 * 
 * See also: <a href="../grt.html#Modules">Module</a>
 */


#include "myx_grt_private.h"
#ifdef ENABLE_JAVA_MODULES
#include "myx_grt_java.h"
#endif


/**
 ****************************************************************************
 * @brief Scans a directory for modules and inits them
 * 
 *   Will scan through all files in the directory looking for files with 
 * recognized extensions. Each module will be registered wrt it's functionality.
 *
 * @param grt       the GRT environment
 * @param directory directory where modules are located
 * @param error     pointer to error variable
 * 
 * @return Number of modules that were initialized. -1 on error.
 * @return error will contain the error.
 ****************************************************************************/
int myx_grt_scan_for_modules(MYX_GRT *grt, const char *directory,
                             MYX_GRT_ERROR *error)
{
  GDir *dir;
  const char *entry;
  int count= 0;

  *error= MYX_GRT_NO_ERROR;

  g_return_val_if_fail(grt != NULL, -1);

  GRT_ENTER(grt);
  
  dir= g_dir_open(directory, 0, NULL);
  if (!dir)
  {
    myx_grt_messages_stack_add_message(grt, "Cannot find module directory %s.", NULL, 0, directory);

    *error= MYX_GRT_BAD_PATH;
    GRT_RETURN(grt, -1, int);
  }

  if (grt->options & MYX_GRT_VERBOSE)
    myx_grt_messages_stack_add_message(grt, "Scanning module directory %s.", NULL, 0, directory);

  while ((entry= g_dir_read_name(dir)) != NULL)
  {
    char *path= g_build_filename(directory, entry, NULL);

    if ((entry[0] != '_') && (myx_grt_module_init(grt, path) == MYX_GRT_NO_ERROR))
    {
      count++;
    }

    g_free(path);
  }

  g_dir_close(dir);

  GRT_RETURN(grt, count, int);
}

int myx_grt_scan_for_structs(MYX_GRT *grt, const char *directory, MYX_GRT_ERROR *error)
{
  GDir *dir;
  const char *entry;
  int count= 0;

  *error= MYX_GRT_NO_ERROR;

  g_return_val_if_fail(grt != NULL, -1);

  GRT_ENTER(grt);
  
  dir= g_dir_open(directory, 0, NULL);
  if (!dir)
  {
    *error= MYX_GRT_BAD_PATH;
    GRT_RETURN(grt, -1, int);
  }

  while ((entry= g_dir_read_name(dir)) != NULL)
  {
    if ( (str_beginswith(entry, "structs.")) && (str_endswith(entry, ".xml")) )
    {
      char *path= g_build_filename(directory, entry, NULL);

      if (myx_grt_struct_load_and_register(grt, path) == MYX_GRT_NO_ERROR)
      {
        count++;
      }

      g_free(path);
    }
  }

  g_dir_close(dir);

  GRT_RETURN(grt, count, int);
}


/**
 ****************************************************************************
 * @brief Initializes/registers a module in the file
 * 
 *  This will check the file extension to find out the type of module and
 * will then initialize it through the appropriate loader. The loader will
 * find out the list of protocols the module implements, including which function
 * interfaces in the protocol it has.
 * The created module will then be registered in the GRT.
 *
 * \b NOTE
 *  Modules must implement a function called getModuleInfo(), which 
 *  should return a dictionary/map, containing:
 *     name, a string with the name of the module
 *     implements, a string with the name of the protocol the module implements
 *     functions, a list of strings with the name of functions implemented
 * 
 *  Function names must match the names in the protocol definition.
 *
 * @param grt   the GRT environment
 * @param filename  filename of the module file to be loaded
 * 
 * @return MYX_GRT_NO_ERROR on success
 * @return MYX_GRT_UNKNOWN_MODULE_TYPE if there's no loader that supports it
 * @return MYX_GRT_MODULE_INIT_ERROR if there was an error initializing the module
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_module_init(MYX_GRT *grt, const char *filename)
{
  MYX_GRT_MODULE *module= NULL;
  unsigned int i;
  MYX_GRT_ERROR error;

  GRT_ENTER(grt);
  
  for (i= 0; i < grt->loaders_num; i++)
  {
    unsigned int j;
    for (j= 0; j < grt->loaders[i]->extensions_num; j++)
    {
      if (g_str_has_suffix(filename, grt->loaders[i]->extensions[j]))
      {        
        if (grt->options & MYX_GRT_VERBOSE)
          myx_grt_messages_stack_add_message(grt, "Trying to load module %s.", NULL, 0, filename);

        error= grt->loaders[i]->init_module(grt->loaders[i], filename, &module);
        if (error != MYX_GRT_NO_ERROR)
          GRT_RETURN(grt, error, MYX_GRT_ERROR);
        break;
      }
    }
  }

  if (!module)
    error= MYX_GRT_UNKNOWN_MODULE_TYPE;
  else
    error= myx_grt_add_module(grt, module);
  
  GRT_RETURN(grt, error, MYX_GRT_ERROR);
}

/**
 ****************************************************************************
 * @brief Locates the function descriptor.
 *
 * @param grt           the GRT environment
 * @param module        module name where function is located
 * @param function_name the function name from the module
 * @param search_parent search for the function in the module named in extends
 *       if it's not implemented in the given module.
 *
 * @return The function structure or NULL if it doesn't exist. The value should not
 *   be freed or modified.
 ****************************************************************************/
MYX_GRT_FUNCTION *myx_grt_function_get(MYX_GRT *grt, const char *module, const char *function_name, int search_parent)
{
  unsigned int i;
  MYX_GRT_FUNCTION *function= NULL;
  
  GRT_ENTER(grt);
  
  for (i= 0; i < grt->modules_num; i++)
  {
    unsigned int j;

    if (strcmp2(grt->modules[i]->name, module) == 0)
    {
      /* search the function in this module */
      for (j= 0; j < grt->modules[i]->functions_num; j++)
      {
        if (strcmp2(grt->modules[i]->functions[j].name, function_name) == 0)
        {
          GRT_RETURN(grt, grt->modules[i]->functions+j, MYX_GRT_FUNCTION*);
        }
      }
      /* if not found, then search in parent */
      if (search_parent && grt->modules[i]->extends)
      {
        function= myx_grt_function_get(grt, grt->modules[i]->extends, function_name, search_parent);
        break;
      }
    }
  }

  GRT_RETURN(grt, function, MYX_GRT_FUNCTION*);
}

/**
 ****************************************************************************
 * @brief Calls a GRT function.
 * 
 *  This will execute the given function, passing the specified argument.
 * The argument will have it's struct type validated before the function
 * is called and its return type is also validated.
 *
 * @param grt the GRT environment
 * @param func the function to be executed
 * @param argument a MYX_GRT_VALUE containing the argument to the called function
 * @param retval where the return value of the function will be placed
 *
 * @return MYX_NO_ERROR for successful invocation. retval will contain the return value
 *      from the function.
 * @return MYX_GRT_BAD_FUNCTION the function or module is not known
 * @return MYX_GRT_VALIDATION_ERROR if there was a validation error of 
 *   parameter or return value
 * @return other errors
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_function_call(MYX_GRT *grt, MYX_GRT_FUNCTION *func,
                                    MYX_GRT_VALUE *argument, MYX_GRT_ERROR *error)
{
  MYX_GRT_VALUE *retval;

  *error= MYX_GRT_NO_ERROR;
  
  //Asserts
  if ((!grt) || (!func))
  {
    *error= MYX_GRT_INTERNAL_ERROR;
    return NULL;
  }
  //g_return_val_if_fail(grt!=NULL, MYX_GRT_INTERNAL_ERROR);
  //g_return_val_if_fail(func!=NULL, MYX_GRT_INTERNAL_ERROR);

  GRT_ENTER(grt);
  
  if (func->param_struct_name && argument)
  {
    if (!myx_grt_dict_struct_validate(grt, argument, func->param_struct_name, 0))
    {
      *error= MYX_GRT_VALIDATION_ERROR;
      GRT_RETURN(grt, NULL, MYX_GRT_VALUE*);
    }
  }

  *error= (*func->module->loader->call_function)(func, argument, &retval);
  
  if (retval && func->return_struct_name)
  {
    if (!myx_grt_dict_struct_validate(grt, retval, func->return_struct_name, 0))
    {
      myx_grt_value_release(retval);
      *error= MYX_GRT_VALIDATION_ERROR;
      GRT_RETURN(grt, NULL, MYX_GRT_VALUE*);
    }
  }

  GRT_RETURN(grt, retval, MYX_GRT_VALUE*);
}

/**
 ****************************************************************************
 * @brief Looks up for the function and calls it.
 * 
 *  Executes the named function from the module, with the given argument.
 *
 * @param grt the GRT environment
 * @param module module name to search the function
 * @param function_name the function name to be executed
 * @param search_parent search for the function in the module named in extends
 *       if it's not implemented in the given module.
 * @param argument a MYX_GRT_VALUE containing the argument to the called function
 * @param retval where the return value of the function will be placed
 *
 * @return MYX_NO_ERROR for successful invocation. retval will contain the return value
 *      from the function.
 * @return MYX_GRT_BAD_FUNCTION if the module or function name given are invalid.
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_function_get_and_call(MYX_GRT *grt, const char *module, const char *function_name, int search_parent,
                                         MYX_GRT_VALUE *argument, MYX_GRT_ERROR *error)
{
  MYX_GRT_FUNCTION *func;
  GRT_ENTER(grt);
  func= myx_grt_function_get(grt, module, function_name, search_parent);
  if (!func) 
  {
    *error = MYX_GRT_BAD_FUNCTION;
    GRT_RETURN(grt, NULL, MYX_GRT_VALUE*);
  }

  GRT_RETURN(grt, myx_grt_function_call(grt, func, argument, error), MYX_GRT_VALUE*);
}

/**
 ****************************************************************************
 * @brief Creates a result for a function
 * 
 *  Creates a new MYX_DICT_VALUE that contains the result
 *
 * @param result the result value
 *
 * @return a MYX_DICT_VALUE that can be returned by a function
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_function_create_result(MYX_GRT_VALUE *result)
{
  MYX_GRT_VALUE *res= NULL;
  
  if (result)
  {
    res= myx_grt_dict_new(NULL, "");

    myx_grt_dict_item_set_value(res, "value", result);
    myx_grt_value_release(result);
  }

  return res;
}

/**
 ****************************************************************************
 * @brief Extracts the value from a function result
 * 
 *  Returns the MYX_DICT_VALUE that contains the result and releases the result
 *
 * @param result the result value
 *
 * @return a MYX_DICT_VALUE that is the returned value
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_function_extract_value_from_result(MYX_GRT_VALUE *result)
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(result, "value");

  myx_grt_value_retain(value);
  myx_grt_value_release(result);

  return value;
}

/**
 ****************************************************************************
 * @brief Creates an error result for a function
 * 
 *  Creates a new MYX_DICT_VALUE that contains the error information
 *
 * @param error the error text
 *
 * @param error_details more detailed information about the error
 *
 * @return a MYX_DICT_VALUE that can be returned by a function to indicate
 *   that an error has occured
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_function_create_error_result(const char *error, const char *error_details)
{
  MYX_GRT_VALUE *result= myx_grt_dict_new(NULL, "");
  MYX_GRT_VALUE *val= myx_grt_value_from_string(error);

  myx_grt_dict_item_set_value(result, "error", val);
  myx_grt_value_release(val);

  if (error_details && error_details[0])
  {
    val= myx_grt_value_from_string(error_details);
    myx_grt_dict_item_set_value(result, "detail", val);
    myx_grt_value_release(val);
  }

  return result;
}

/**
 ****************************************************************************
 * @brief Returns the error string of a function call result
 * 
 *  Checks whether a function has returned an error as result.
 *
 * @param result the function result
 *
 * @return NULL if there was no error or a new allocated error string that
 *   has to be freed
 ****************************************************************************/
char * myx_grt_function_check_error(MYX_GRT_VALUE *res, int allow_null_as_result)
{
  MYX_GRT_VALUE *error_val;

  //Check if we have a value
  if ( (res == NULL) && (allow_null_as_result == 0) )
    return g_strdup("The function did not return an object.");
  else if ( (res == NULL) && (allow_null_as_result == 1) )
    return NULL;

  //Check if we have a dict value
  if (myx_grt_value_get_type(res) != MYX_DICT_VALUE)
    return g_strdup_printf("The function returned a %s instead of a dictionary object.", 
      myx_get_value_type_as_string(myx_grt_value_get_type(res)));

  error_val= myx_grt_dict_item_get_value(res, "error");
  if (error_val)
  {
    const char *detail= myx_grt_dict_item_get_as_string(res, "detail");

    if (detail)
      return g_strdup_printf("%s" _br 
        "Details: " _br 
        "%s", 
        myx_grt_value_as_string(error_val),
        detail);
    else
      return g_strdup_printf("%s", 
        myx_grt_value_as_string(error_val));
  }
  else
    return NULL;
}

/**
 ****************************************************************************
 * @brief Returns the module with the given name
 * 
 *  Searches for the module with the given name and returns it.
 *
 * @param grt the GRT environment
 * @param name name of the wanted module
 *
 * @return The module struct or NULL if the module is not found.
 ****************************************************************************/
MYX_GRT_MODULE *myx_grt_module_get(MYX_GRT *grt, const char *name)
{
  unsigned int i;
  
  GRT_ENTER(grt);
  for (i= 0; i < grt->modules_num; i++)
  {
    if (strcmp2(grt->modules[i]->name, name)==0)
      GRT_RETURN(grt, grt->modules[i], MYX_GRT_MODULE*);
  }
  GRT_RETURN(grt, NULL, MYX_GRT_MODULE*);
}

MYX_GRT_MODULE * myx_grt_module_get_by_index(MYX_GRT *grt, unsigned int index)
{
  GRT_ENTER(grt);
  if (index < grt->modules_num)
    GRT_RETURN(grt, grt->modules[index], MYX_GRT_MODULE*);
  else
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE*);
}

MYX_GRT_MODULE_TYPE myx_grt_module_get_type(MYX_GRT_MODULE *module)
{
  //GRT_RETURN(grt, module->loader->loader_type, MYX_GRT_MODULE_TYPE);

  return module->loader->loader_type;
}


/**
 ****************************************************************************
 * @brief Get list of modules that extend a given module
 * 
 *   Returns an array with all registered modules that extend the named
 * module.
 *
 * @param grt    the GRT environment
 * @param module name of the module. Can be NULL, in which case it will return 
 *        all registered modules.
 * @param retmodules pointer to array of modules where the result will be returned
 *
 * @returned The number of modules in the array. The module array must be freed.
 ****************************************************************************/
int myx_grt_modules_get_that_extend(MYX_GRT *grt, const char *module, MYX_GRT_MODULE **retmodules[])
{
  unsigned int i;
  int count= 0;

  g_return_val_if_fail(grt!=NULL, 0);
  g_return_val_if_fail(retmodules != NULL, 0);
  
  GRT_ENTER(grt);
  
  if (grt->modules_num == 0)
    GRT_RETURN(grt, 0, int);

  *retmodules= g_new0(MYX_GRT_MODULE*,grt->modules_num);

  for (i= 0; i < grt->modules_num; i++)
  {
    if (!module || !*module || strcmp2(grt->modules[i]->extends, module)==0)
    {
      count++;
      (*retmodules)[count-1]= grt->modules[i];
    }
  }

  if (count == 0)
  {
    g_free(*retmodules);
    *retmodules= NULL;
  }
  else
    *retmodules= g_realloc(*retmodules, sizeof(MYX_GRT_MODULE*)*count);

  GRT_RETURN(grt, count, int);
}

int myx_grt_module_get_count(MYX_GRT *grt)
{
  GRT_ENTER(grt);
  GRT_RETURN(grt, grt->modules_num, int);
}

MYX_STRINGLIST * myx_grt_module_get_names(MYX_GRT *grt)
{
  MYX_STRINGLIST *modules= g_new0(MYX_STRINGLIST, 1);
  unsigned int i;

  GRT_ENTER(grt);
  
  modules->strings_num= grt->modules_num;
  modules->strings= g_malloc0(sizeof(char *)*modules->strings_num);

  for (i= 0; i<modules->strings_num; i++)
  {
    MYX_GRT_MODULE *module= grt->modules[i];
    modules->strings[i]= g_strdup(module->name);
  }

  GRT_RETURN(grt, modules, MYX_STRINGLIST*);
}

int myx_grt_module_function_get_count(MYX_GRT_MODULE *module)
{
  return module->functions_num;
}

MYX_STRINGLIST * myx_grt_module_function_get_names(MYX_GRT_MODULE *module)
{
  MYX_STRINGLIST *funcs= g_new0(MYX_STRINGLIST, 1);
  unsigned int i;

  funcs->strings_num= module->functions_num;
  funcs->strings= g_malloc0(sizeof(char *)*funcs->strings_num);

  for (i= 0; i<funcs->strings_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    funcs->strings[i]= g_strdup(func->name);
  }

  return funcs;
}

MYX_GRT_FUNCTION * myx_grt_module_function_get(MYX_GRT_MODULE *module, const char *name)
{
  unsigned int i;
  for (i= 0; i < module->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;

    if (strcmp2(func->name, name)==0)
      return func;
  }
  return NULL;
}

MYX_GRT_FUNCTION * myx_grt_module_function_get_by_index(MYX_GRT_MODULE *module, unsigned int index)
{
  if (index < module->functions_num)
    return module->functions+index;
  else
    return NULL;
}

char * myx_grt_module_function_get_params(MYX_GRT_FUNCTION *func)
{
    if (func->param_struct_name)
      return g_strdup(func->param_struct_name);
#ifdef ENABLE_JAVA_MODULES
    else if ((func->module->loader->loader_type==MYX_JAVA_MODULE_TYPE) && 
      (((MYX_JAVA_FUNCTION *)(func->priv))->java_signature))
    {
      char *params= g_strdup(((MYX_JAVA_FUNCTION *)(func->priv))->java_signature);
      params= str_g_replace(params, ";)", ")");
      params= str_g_replace(params, ";", ", ");
      params= str_g_replace(params, "Ljava/lang/String", "string");
      params= str_g_replace(params, "Ljava/lang/Integer", "int");
      params= str_g_replace(params, "Lcom/mysql/grt/GrtList", "list");
      params= str_g_replace(params, "Lcom/mysql/grt/", "");

      return params;
    }
#endif
    else
      return g_strdup("");
}

char * myx_grt_module_function_get_return_type(MYX_GRT_FUNCTION *func)
{
  if (func->return_struct_name)
    return g_strdup(func->return_struct_name);
  else
    return g_strdup("");
}

MYX_GRT_MODULE_LOADER * myx_grt_module_loader_create(MYX_GRT *grt, MYX_GRT_MODULE_TYPE loader_type,
                  MYX_STRINGLIST *extensions, 
                  MYX_GRT_ERROR (*init_module)(MYX_GRT_MODULE_LOADER *loader, const char *file_name, MYX_GRT_MODULE **retmodule),
                  MYX_GRT_ERROR (*call_function)(MYX_GRT_FUNCTION *func, MYX_GRT_VALUE *argument, MYX_GRT_VALUE **retval),
                  void *priv)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  unsigned int i;

  GRT_ENTER(grt);
  
  loader->grt= grt;
  loader->loader_type= loader_type;

  //Copy list of extensions
  if (extensions == NULL) 
  {
    loader->extensions_num= 0;
    loader->extensions= NULL;
  }
  else
  {
    loader->extensions_num= extensions->strings_num;
    loader->extensions= g_malloc(extensions->strings_num+1);
    for (i= 0; i < extensions->strings_num; i++)
      loader->extensions[i]= g_strdup(extensions->strings[i]);
    loader->extensions[extensions->strings_num]= NULL;
  }

  loader->init_module= init_module;
  loader->call_function= call_function;

  loader->priv= priv;

  GRT_RETURN(grt, loader, MYX_GRT_MODULE_LOADER*);
}

void * myx_grt_module_loader_get_private_data(MYX_GRT_MODULE_LOADER *loader)
{
  return loader->priv;
}

void * myx_grt_module_get_private_data(MYX_GRT_MODULE *module)
{
  return module->priv;
}


MYX_GRT_MODULE * myx_grt_module_create(MYX_GRT_MODULE_LOADER *loader, const char *name, 
                                                       const char *path, const char *extends, void *priv)
{
  MYX_GRT_MODULE *mod= g_new0(MYX_GRT_MODULE, 1);

  mod->loader= loader;
  mod->name= g_strdup(name);
  if(path)
    mod->path= g_strdup(path);
  if(extends)
    mod->extends= g_strdup(extends);

  mod->priv= priv;

  return mod;
}

void myx_grt_module_add_function(MYX_GRT_MODULE *module, const char *name, const char *param_struct_name, 
                                                 const char *return_struct_name, void *priv)
{
  MYX_GRT_FUNCTION *func;

  module->functions= g_realloc(module->functions, sizeof(MYX_GRT_FUNCTION) * (module->functions_num + 1));
  func= module->functions + module->functions_num;
  module->functions_num++;
  

  func->module= module;
  func->name= g_strdup(name);
  if ((param_struct_name) && (*param_struct_name))
    func->param_struct_name= g_strdup(param_struct_name);
  else
    func->param_struct_name= NULL;

  if ((return_struct_name) && (*return_struct_name))
    func->return_struct_name= g_strdup(return_struct_name);
  else
    func->return_struct_name= NULL;

  func->priv= priv;
}
