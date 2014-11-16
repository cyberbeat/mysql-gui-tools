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

#ifdef ENABLE_PYTHON_MODULES

#include "myx_grt_python.h"
#include "myx_grt_private.h"


// python internals
#ifdef __APPLE__
#include <Python/node.h>
#include <Python/grammar.h>
#include <Python/parsetok.h>
#include <Python/errcode.h>
#include <Python/token.h>
#else
#include <node.h>
#include <grammar.h>
#include <parsetok.h>
#include <errcode.h>
#include <token.h>
#endif


static int set_current_node(MYX_GRT *grt, MYX_GRT_VALUE *curnode);

typedef struct MYX_GRT_SHELL_PRIVATE {
  char *cwd;
  char *current_line;
  MYX_GRT_PYCONTEXT *pycon;
} MYX_GRT_SHELL_PRIVATE;


static void shell_print_welcome(MYX_GRT *grt)
{
  char *version= GRT_VERSION;

  myx_grt_printf(grt, "MySQL Generic Runtime Environment %s"NL, version);

  myx_grt_printf(grt, NL"Type 'help' or '?' for help. Type 'quit' to exit the shell."NL);
  myx_grt_printf(grt, "Python Shell initialized."NL);
}



static int shell_init(MYX_GRT *grt)
{
  MYX_GRT_MODULE_LOADER *loader;
  MYX_GRT_SHELL_PRIVATE *priv;
  
  // The Python module loader must be initialized before the shell
  loader= myx_grt_get_loader_of_type(grt, MYX_PYTHON_MODULE_TYPE);
  if (!loader)
    return -1;

  priv= grt->shell->data;
  
  priv->cwd= g_strdup("/");
  priv->pycon= loader->priv;

  myx_py_acquire(priv->pycon);
  set_current_node(grt, grt->root);
  myx_py_release(priv->pycon);
  
  return 0;
}


static char *shell_get_prompt(MYX_GRT *grt)
{
  MYX_GRT_SHELL_PRIVATE *priv= grt->shell->data;

  if (!priv->current_line)
    return g_strdup_printf("%s>>> ", priv->cwd ? priv->cwd : "");
  else
    return g_strdup_printf("%s... ", priv->cwd ? priv->cwd : "");
}


static int py_shell_execute(MYX_GRT *grt, const char *linebuf)
{
  node *n;
  MYX_GRT_SHELL_PRIVATE *pshell= grt->shell->data;
  PyObject *result;
  PyObject *mainmod;
  PyObject *globals;

  if (pshell->current_line)
    pshell->current_line= str_g_append(pshell->current_line, linebuf);
  else
    pshell->current_line= g_strdup(linebuf);

  myx_py_acquire(pshell->pycon);

  n= PyParser_SimpleParseStringFlags(pshell->current_line,
                                     Py_single_input,
                                     0);

  if (n && (*linebuf==' ' || *linebuf=='\t'))
  {
    myx_py_release(pshell->pycon);
    return 1;
  }
  if (!n && PyErr_Occurred() && PyErr_ExceptionMatches(PyExc_SyntaxError))
  {
     PyObject *excep;
     PyObject *value;
     PyObject *message;
     PyObject *trace;
     PyErr_Fetch(&excep, &value, &trace);
     Py_DECREF(excep);
     if (trace)
     {
       Py_DECREF(trace);
     }
     message= PyTuple_GetItem(value, 0);
     if (strstr(PyString_AsString(message), "unexpected EOF") || 
        strncmp(PyString_AsString(message), "EOF", 3)==0)
     {
        Py_DECREF(value);
        PyErr_Clear();
        myx_py_release(pshell->pycon);
        return 1;
     }
     Py_DECREF(value);
  }
  
  if (!n)
  {
    PyErr_Print();    

    g_free(pshell->current_line);
    pshell->current_line= NULL;
    
    PyErr_Clear();
    myx_py_release(pshell->pycon);
    return -1;
  }
  PyNode_Free(n);
  PyErr_Clear();

  // command is supposedly complete, try to execute it
  
  mainmod= PyImport_AddModule("__main__");
  if (!mainmod)
  {
    myx_py_release(pshell->pycon);
    return -1;
  }
  globals= PyModule_GetDict(mainmod);
  
  result= PyRun_String(pshell->current_line, Py_single_input, globals, globals);
  
  g_free(pshell->current_line);
  pshell->current_line= NULL;

  if (!result)
  {
    //XXX use myx_grt_prnt to print the exc
    if (PyErr_Occurred())
      PyErr_Print();

    myx_py_release(pshell->pycon);
    
    return -1;
  }
  else
  {
#if notworking
    // attempt to show the result object
    if (result != Py_None)
    {
      PyObject *tmp= PyObject_Str(result);
      if (!tmp)
      {
        PyErr_Clear();
      }
      else
      {
        myx_grt_printf(grt, "%s\n", PyString_AsString(tmp));
      }
      if (tmp)
        Py_DECREF(tmp);
    }
#endif
    Py_DECREF(result);
  }

  myx_py_release(pshell->pycon);
  
  return 0;
}


static MYX_GRT_SHELL_COMMAND shell_execute(MYX_GRT *grt, const char *linebuf)
{
  char *cmd;
  unsigned int cmd_len;
  char *cmd_param= NULL;
  MYX_GRT_SHELL_COMMAND res= MYX_GRT_SHELL_COMMAND_UNKNOWN;
  char *preprocessed_cmd= NULL;

  cmd= g_strdup(linebuf);
  cmd= str_trim(cmd);
  cmd_len= (unsigned int)strlen(cmd);

  //Quit command
  if( (strcmp2(cmd, "quit")==0) || (strcmp2(cmd, "quit;")==0) || (strcmp2(cmd, "exit")==0) ||
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
      preprocessed_cmd= g_strdup_printf("grt.cd(\"%s\")"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
    else
    {
      preprocessed_cmd= g_strdup_printf("print grt.pwd()"NL);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }

    if(path)
      g_free(path);
  }
  // Automatically convert ls -m module to showModule()
  else if (str_beginswith(cmd, "ls -m "))
  {
    char *path= get_value_from_text_ex(cmd, (int)strlen(cmd), "ls\\s+\\-m\\s+(.+)", 1);

    if((path) && (path[0]))
    {
      preprocessed_cmd= g_strdup_printf("grt.showModule(\"%s\")"NL, path);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
  }
  // Automatically convert ls -m to list()
  else 
    // TODO: Parsing for the poor. What if there is more than a space char between the command and its parameter?
    if ((strcmp2(cmd, "ls -m") == 0) || (strcmp2(cmd, "dir -m") == 0))
    {
      preprocessed_cmd= g_strdup("print '\\n'.join(grt.listModules())"NL);
      res= MYX_GRT_SHELL_COMMAND_STATEMENT;
    }
  // Automatically convert ls to ls()
  else 
    if ((strcmp(cmd, "ls") == 0) || (strcmp(cmd, "dir") == 0) || str_beginswith(cmd, "ls ") || str_beginswith(cmd, "dir "))
  {
    preprocessed_cmd= g_strdup("grt.ls()"NL);
    res= MYX_GRT_SHELL_COMMAND_STATEMENT;
  }
  // Automatically convert show to show(current)
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
      preprocessed_cmd= g_strdup_printf("print(grt.getGlobal(\"%s\"))"NL, path);
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
    if (preprocessed_cmd)
      py_shell_execute(grt, preprocessed_cmd);
    else
      py_shell_execute(grt, linebuf);
    
    if (preprocessed_cmd)
      g_free(preprocessed_cmd);
    
    return MYX_GRT_SHELL_COMMAND_STATEMENT;
  }
  
  if (preprocessed_cmd)
    g_free(preprocessed_cmd);

  return res;
}


static int shell_run_file(MYX_GRT *grt, const char *file_name, int interactive)
{
  int status;
  int rc;
  FILE *f;
  MYX_GRT_SHELL_PRIVATE *pshell= grt->shell->data;

  if (interactive)
    myx_grt_printf(grt, "Opening script file %s ..."NL, file_name);

  f= fopen(file_name, "r");
  if (!f)
  {
    if (interactive)
      myx_grt_printf(grt, "Error opening script file %s", strerror(errno));
    return -1;
  }

  if (interactive)
    myx_grt_printf(grt, "Executing script file %s ..."NLNL, file_name);

  myx_py_acquire(pshell->pycon);

  status= PyRun_SimpleFile(f, file_name);
  
  myx_py_release(pshell->pycon);
  
  fclose(f);
 
  if (status)
  {
    myx_grt_printf(grt, "error executing script"NL);
    rc= -2;
  }
  else
    rc= 0;

  if ((rc == 0) && (interactive))
    myx_grt_printf(grt, "\nExecution finished."NL);

  return rc;
}


static void *shell_get_interp(MYX_GRT *grt)
{
  MYX_GRT_SHELL_PRIVATE *pshell= grt->shell->data;
  
  return pshell->pycon->state;
}


static MYX_GRT_VALUE * shell_get_global_var(MYX_GRT *grt, const char *var_name)
{
  PyObject *globals= PyEval_GetGlobals();
  PyObject *item;

  item= PyDict_GetItemString(globals, var_name);
  
  if (!item)
    return NULL;
  
  return myx_py_object_to_value(item);
}


static int shell_set_global_var(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value)
{
  PyObject *object= (PyObject*)myx_py_grtvalue_create(value);
  PyObject *globals= PyEval_GetGlobals();

  PyDict_SetItemString(globals, var_name, object);
  Py_DECREF(object);
  
  return 0;
}



static MYX_GRT_SHELL_CONTEXT python_shell_functions= {
  MYX_GRT_SHELL_PYTHON,
    NULL,
    shell_init,
    shell_print_welcome,
    shell_get_prompt,
    shell_execute,
    shell_run_file,
    shell_get_interp,
    shell_get_global_var,
    shell_set_global_var
};




MYX_GRT_SHELL_CONTEXT *myx_grt_setup_python_shell(MYX_GRT *grt)
{
  MYX_GRT_SHELL_CONTEXT *shell;
  MYX_GRT_SHELL_PRIVATE *priv;

  shell= g_new0(MYX_GRT_SHELL_CONTEXT, 1);
  memcpy(shell, &python_shell_functions, sizeof(python_shell_functions));
  
  priv= g_new0(MYX_GRT_SHELL_PRIVATE, 1);
  
  shell->data= priv;

  
  return shell;
}



static int set_current_node(MYX_GRT *grt, MYX_GRT_VALUE *curnode)
{
  PyObject *module;
  PyObject *node;

  node= (PyObject*)myx_py_grtvalue_create(curnode);
  if (!node)
    return -1;
  
  module= PyImport_AddModule("__main__");
  if (module)
  {
    PyObject *dict= PyModule_GetDict(module);
    PyDict_SetItemString(dict, "current", node);
  }

  module= PyImport_AddModule("grt");
  if (module)
  {
    PyObject *dict= PyModule_GetDict(module);
    PyDict_SetItemString(dict, "current", node);
  }

  Py_DECREF(node);
  
  return 0;
}


PyObject *myx_py_grt_cd(PyObject *self, PyObject *args)
{
  MYX_GRT *grt= myx_py_get_grt()->grt;
  char *path= NULL;
  MYX_GRT_VALUE *curnode;

  if (!PyArg_ParseTuple(args, "s", &path))
    return NULL;

  path= myx_grt_get_abspath(grt->shell->data->cwd, path);
  if (!path)
  {
    PyErr_SetString(PyExc_ValueError, "invalid path");
    return NULL;
  }
  curnode= myx_grt_dict_item_get_by_path(grt, grt->root, path);

  if (!curnode)
  {
    PyErr_Format(PyExc_ValueError, "invalid path '%s'", path);
    return NULL;
  }
  
  if (myx_grt_value_get_type(curnode) != MYX_DICT_VALUE &&
      myx_grt_value_get_type(curnode) != MYX_LIST_VALUE)
  {
    PyErr_Format(PyExc_ValueError, "can't cd into '%s'", path);
    return NULL;
  }

  if (set_current_node(grt, curnode) < 0)
    return NULL;

  g_free(grt->shell->data->cwd);
  grt->shell->data->cwd= path;

  Py_INCREF(Py_None);
  return Py_None;
}


static void print_value(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  const char *name;
  
  switch (myx_grt_value_get_type(value))
  {
  case MYX_DICT_VALUE:
    name= myx_grt_dict_name_item_as_string(value);
    if (name)
      myx_grt_printf(grt, "... %s [dict]"NL, name);
    else
      myx_grt_printf(grt, "... [dict]"NL);
    break;

  case MYX_LIST_VALUE:
    myx_grt_printf(grt, "... [list]"NL);
    break;
    
  case MYX_INT_VALUE:
    myx_grt_printf(grt, "%i"NL, value->value.i);
    break;
    
  case MYX_REAL_VALUE:
    myx_grt_printf(grt, "%f"NL, value->value.r);
    break;

  case MYX_STRING_VALUE:
    myx_grt_printf(grt, "%s"NL, value->value.s);
    break;
    
  case MYX_ANY_VALUE:
    break;
  }
}


PyObject *myx_py_grt_ls(PyObject *self, PyObject *args)
{
  char *path= NULL;
  MYX_GRT_VALUE *curnode;
  PyObject *content;
  MYX_GRT *grt= myx_py_get_grt()->grt;
  unsigned int i;

  if (!PyArg_ParseTuple(args, "|z", &path))
    return NULL;
  
  if (!path)
  {
    curnode= myx_grt_dict_item_get_by_path(grt, 
                                           grt->root,
                                           grt->shell->data->cwd);
  }
  else
  {
    path= myx_grt_get_abspath(grt->shell->data->cwd, path);
    if (!path)
    {
      PyErr_SetString(PyExc_ValueError, "invalid path");
      return NULL;
    }
    curnode= myx_grt_dict_item_get_by_path(grt,
                                           grt->root,
                                           path);
  }

  if (curnode)
  {
    switch (myx_grt_value_get_type(curnode))
    {
    case MYX_LIST_VALUE:
      for (i= 0; i < curnode->value.l->items_num; i++)
        print_value(grt, curnode->value.l->items[i]);
      break;

    case MYX_DICT_VALUE:
      for (i= 0; i < curnode->value.d->items_num; i++)
      {
        myx_grt_printf(grt, "%s => ", curnode->value.d->items[i].key);
        print_value(grt, curnode->value.d->items[i].value);
      }
      break;

    default:
      content= PyObject_Str((PyObject*)curnode);
      if (content)
      {
        myx_grt_printf(grt, PyString_AsString(content));
        Py_DECREF(content);
      }
      break;
    }
  }
  else
  {
    PyErr_Format(PyExc_KeyError, "invalid tree path '%s'", 
                 path ? path : grt->shell->data->cwd);
  }
  g_free(path);
  Py_INCREF(Py_None);
  return Py_None;
}


PyObject *myx_py_grt_pwd(PyObject *self)
{
  MYX_GRT_SHELL_PRIVATE *priv= myx_py_get_grt()->grt->shell->data;
  
  if (priv->cwd)
    return PyString_FromString(priv->cwd);
  else
    return PyString_FromString("");
}
#endif
