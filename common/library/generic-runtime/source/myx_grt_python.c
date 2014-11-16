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


#ifdef ENABLE_PYTHON_MODULES

#include "myx_grt_python.h"


typedef struct MYX_GRT_MODULE_PRIVATE
{
  PyObject *module;
} MYX_PYMOD_PRIVATE;


static MYX_GRT_ERROR py_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule);
static MYX_GRT_ERROR py_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);


MYX_GRT_MODULE_LOADER *myx_python_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error, const char *python_module_path)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  static char *file_extensions[]= {
    ".py"//, ".pyc", ".pyo"
  };
  GDir *dir;

  Py_Initialize();

  PyEval_InitThreads();
  
  PyEval_ReleaseLock();

  *error= MYX_GRT_NO_ERROR;

  loader->grt= grt;
  loader->loader_type= MYX_PYTHON_MODULE_TYPE;
  loader->init_module= py_init_module;
  loader->call_function= py_call_function;
  loader->extensions_num= 1;
  loader->extensions= file_extensions;

  loader->priv= myx_py_setup_environment(grt);
  if (!loader->priv)
  {
    g_free(loader);
    return NULL;
  }

  myx_py_acquire(loader->priv);
  
  // init builtin modules
  dir= g_dir_open(python_module_path, 0, NULL);

  if (dir)
  {
    char *module_file;
    char *olddir= g_get_current_dir();
    const char *entry;
    chdir(python_module_path);
    while ((entry= g_dir_read_name(dir)) != NULL)
    {
      if (entry[0] == '_' && strncmp(entry, "__init__", 8)!=0)
      {
        module_file= NULL;
        if (g_str_has_suffix(entry, ".py"))
        {
          module_file= g_strdup(entry);
          module_file[strlen(module_file)-3]= 0;
        }
        else if (g_str_has_suffix(entry, ".pyc"))
        {
          module_file= g_strdup(entry);
          module_file[strlen(module_file)-4]= 0;
        }
        else if (g_str_has_suffix(entry, ".pyo"))
        {
          module_file= g_strdup(entry);
          module_file[strlen(module_file)-4]= 0;
        }
        if (module_file)
        {
          PyImport_ImportModule(module_file);
          g_free(module_file);
        }
      }
    }
    chdir(olddir);
    g_free(olddir);
    g_dir_close(dir);
  }

  myx_py_release(loader->priv);

  return loader;
}


static MYX_GRT_ERROR py_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule)
{
  MYX_GRT_MODULE *module= NULL;
  MYX_PYMOD_PRIVATE *priv= NULL;
//  MYX_GRT *grt= loader->grt;
  PyObject *mod, *pyval, *pyfunc= NULL, *info= NULL;
  PyObject *old_path, *sysmod, *path_list;
  unsigned int i;
  char *name;
  char *ptr;

  myx_py_acquire(loader->priv);

  name= g_path_get_basename(file);
  ptr= strrchr(name, '.');
  if (ptr) // strip the extension
    *ptr= 0;

  // temporarily add the file's path to the module lookup path
  sysmod= PyImport_AddModule("sys");

  path_list= PyDict_GetItemString(PyModule_GetDict(sysmod), "path");
  old_path= PyList_GetSlice(path_list, 0, PyList_Size(path_list));
  {
    char *path= g_path_get_dirname(file);
    PyObject *tmp= PyString_FromString(path);
    PyList_Append(path_list, tmp);
    g_free(path);
    Py_DECREF(tmp);
  }
  if ((mod= PyImport_ImportModule(name)) == NULL)
  {
    g_free(name);
    PyDict_SetItemString(PyModule_GetDict(sysmod), "path", old_path);
    Py_DECREF(old_path);
    goto error;
  }
  PyDict_SetItemString(PyModule_GetDict(sysmod), "path", old_path);
  Py_DECREF(old_path);
  g_free(name);

  pyfunc= PyObject_GetAttrString(mod, "getModuleInfo");
  if (!pyfunc || !PyCallable_Check(pyfunc))
  {
    goto error;
  }
  info= PyObject_Call(pyfunc, PyTuple_New(0), NULL);
  if (!info)
  {
    goto error;
  }
  module= g_new0(MYX_GRT_MODULE, 1);
  module->loader= loader;
  module->path= g_strdup(file);
  
  // process name
  pyval= PyDict_GetItemString(info, "name");
  if (!pyval || !PyString_Check(pyval))
  {
    g_warning("moduleInfo for Python module %s contains invalid or no name attribute", file);
    goto error;
  }
  module->name= g_strdup(PyString_AsString(pyval));
  
  // process list of functions
  pyval= PyDict_GetItemString(info, "functions");
  if (!pyval || !PyList_Check(pyval))
  {
    g_warning("moduleInfo for Python module %s contains invalid or no functions attribute", file);
    goto error;
  }
  module->functions_num= PyList_Size(pyval);
  module->functions= g_new0(MYX_GRT_FUNCTION, module->functions_num);
  for (i= 0; i < module->functions_num; i++)
  {
    PyObject *v= PyList_GetItem(pyval, i);
    module->functions[i].module= module;
    myx_grt_parse_function_spec(PyString_AsString(v), &module->functions[i]);
    module->functions[i].priv= NULL;    
  }

  // get extended module name
  pyval= PyDict_GetItemString(info, "extends");
  if (pyval && !PyString_Check(pyval))
  {
    g_warning("moduleInfo for Python module %s contains invalid 'extends' attribute", file);
    goto error;
  }
  else if (!pyval)
    module->extends= NULL;
  else
    module->extends= g_strdup(PyString_AsString(pyval));

  Py_DECREF(pyfunc);
  Py_DECREF(info);

  priv= g_new0(MYX_PYMOD_PRIVATE, 1);
  priv->module= mod;

  // private data
  module->priv= priv;
  
  *retmodule= module;
  
  myx_py_release(loader->priv);

  return MYX_GRT_NO_ERROR;
  
error:
  if (PyErr_Occurred())
  {
    g_message("Error importing module '%s':", file);
    PyErr_Print();
    PyErr_Clear();
  }
  if (module)
  {
    g_free(module->path);
    g_free(module->name);
    for (i= 0; i < module->functions_num; i++)
      g_free(module->functions[i].name);
    g_free(module->functions);
    g_free(module->extends);
    g_free(module);
  }

  Py_XDECREF(mod);
  Py_XDECREF(pyfunc);
  Py_XDECREF(info);

  myx_py_release(loader->priv);
  
  return MYX_GRT_MODULE_INIT_ERROR;
}


static MYX_GRT_ERROR py_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_PYMOD_PRIVATE *priv= function->module->priv;
  PyObject *pyfunc, *pyarg, *pyret;

  myx_py_acquire(function->module->loader->priv);

  pyfunc= PyObject_GetAttrString(priv->module, function->name);
  if (!pyfunc)
  {
    g_warning("Function %s not found in module %s", function->name,
              function->module->name);
    PyEval_ReleaseThread(function->module->loader->priv->state);
    return MYX_GRT_BAD_FUNCTION;
  }

  if (value)
    pyarg= myx_py_value_to_object(value, 0);
  else
    pyarg= NULL;
  
  if (pyarg)
  {
    PyObject *tmp= pyarg;
    pyarg= PyList_AsTuple(pyarg);
    Py_DECREF(tmp);
  }

  pyret = PyObject_CallObject(pyfunc, pyarg);
  if (pyret == NULL) // there was an exception in the function
  {
    *retval= myx_grt_dict_new(NULL, NULL);

    if (PyErr_Occurred())
    {
      PyErr_Print();
      myx_grt_dict_item_set_value_from_string(*retval, "error", "Exception calling Python function");
      //myx_grt_dict_item_set_value_from_string(*retval, "detail", );
      
      PyErr_Clear();
    }
    else
      myx_grt_dict_item_set_value_from_string(*retval, "error", "Error calling Python function");
  }
  else
  {
    MYX_GRT_VALUE *result= myx_py_object_to_value(pyret);

    *retval= myx_grt_dict_new(NULL, NULL);

    myx_grt_dict_item_set_value(*retval, "value", result);
    
    myx_grt_value_release(result);
  }

  Py_DECREF(pyfunc);
  Py_XDECREF(pyarg);
  Py_XDECREF(pyret);
  
  myx_py_release(function->module->loader->priv);

  if (pyret)
    return MYX_GRT_NO_ERROR;
  else
    return MYX_GRT_FUNCTION_CALL_ERROR;
}

#endif
