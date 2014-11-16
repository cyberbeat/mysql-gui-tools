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


#ifndef _MYX_GRT_PYTHON_H_
#define _MYX_GRT_PYTHON_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __APPLE__
#include <Python/Python.h>
#else
#undef _DEBUG // for MSVC
#include <Python.h>
#endif
  
#include "myx_grt_private.h"

// used as interpreter context for Python loader and
// is also accessible through __grt from the python interpreter (as a C obj)
typedef struct MYX_GRT_MODULE_LOADER_PRIVATE {
  MYX_GRT *grt;
  PyThreadState *state; // interpreter state
  PyObject *module; // grt module object
  PyObject *error; // base exception object
  PyObject *call_error; // function call exception
} MYX_GRT_PYCONTEXT;

  
typedef struct {
  PyObject_HEAD
  MYX_GRT_VALUE *value;
  MYX_GRT *grt;
} MYX_GRT_VALUE_PYOBJECT;


typedef struct {
  PyObject_HEAD
  MYX_GRT_MODULE *module;
} MYX_GRT_MODULE_PYOBJECT;


MYX_GRT_MODULE_LOADER *myx_python_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error, const char *python_module_path);
MYX_GRT_SHELL_CONTEXT *myx_grt_setup_python_shell(MYX_GRT *grt);

MYX_GRT_PYCONTEXT *myx_py_get_grt();

MYX_GRT_PYCONTEXT *myx_py_setup_environment(MYX_GRT *grt);

MYX_GRT_VALUE_PYOBJECT *myx_py_grtvalue_create(MYX_GRT_VALUE *value);
MYX_GRT_MODULE_PYOBJECT *myx_py_grtmodule_create(MYX_GRT_MODULE *module);

PyObject *myx_py_value_to_object(MYX_GRT_VALUE *value, int deep);
MYX_GRT_VALUE *myx_py_object_to_value(PyObject *object);

void myx_py_acquire(MYX_GRT_PYCONTEXT *ctx);
void myx_py_release(MYX_GRT_PYCONTEXT *ctx);

#ifdef __cplusplus
extern }
#endif

#endif /* _MYX_GRT_PYTHON_H_ */
