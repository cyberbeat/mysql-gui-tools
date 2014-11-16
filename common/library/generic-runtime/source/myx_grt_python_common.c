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
#include "myx_grt_private.h"

static PyTypeObject GRTValueType;
static PyTypeObject GRTModuleType;
static PyTypeObject GRTModuleFunctionType;



//TODO
//documentation


static int GRTTypeSignature= 0;

#define PyGRTValue_Check(v)   ((v)->ob_type == &GRTValueType)
#define PyGRTValue_Value(v)   ((MYX_GRT_VALUE_PYOBJECT*)(v))->value

#define PyGRTModule_Check(v)   ((v)->ob_type == &GRTModuleType)
#define PyGRTModule_Module(v)   ((MYX_GRT_MODULE_PYOBJECT*)(v))->module

void myx_py_acquire(MYX_GRT_PYCONTEXT *ctx)
{
  PyEval_AcquireThread(ctx->state);
}


void myx_py_release(MYX_GRT_PYCONTEXT *ctx)
{
  PyEval_ReleaseThread(ctx->state);
}


MYX_GRT_PYCONTEXT *myx_py_get_grt()
{
  PyObject *modules= PyImport_GetModuleDict();
  PyObject *grtmod;
  PyObject *grtobj;

  grtmod= PyDict_GetItemString(modules, "grt");

  grtobj= PyDict_GetItemString(PyModule_GetDict(grtmod), "__grt");
    
  if (PyCObject_GetDesc(grtobj) != &GRTTypeSignature)
  {
    g_warning("__grt value corrupted!");
    return NULL;
  }
  return (MYX_GRT_PYCONTEXT*)PyCObject_AsVoidPtr(grtobj);
}


PyObject *myx_py_value_to_object(MYX_GRT_VALUE *value, int deep)
{
  PyObject *obj= NULL;
  unsigned int i;

  switch (value->type)
  {
  case MYX_INT_VALUE:
    obj= PyLong_FromLong(value->value.i);
    break;
  case MYX_REAL_VALUE:
    obj= PyFloat_FromDouble(value->value.r);
    break;
  case MYX_STRING_VALUE:
    obj= PyString_FromString(value->value.s);
    break;
  case MYX_LIST_VALUE:
    obj= PyList_New(0);
    for (i= 0; i < value->value.l->items_num; i++)
    {
      if (deep)
      {
        PyObject *ch= myx_py_value_to_object(value->value.l->items[i], deep);
        if (ch)
        {
          PyList_Append(obj, ch);
          Py_DECREF(ch);
        }
      }
      else
      {
        PyObject *ch= (PyObject*)myx_py_grtvalue_create(value->value.l->items[i]);
        PyList_Append(obj, ch);
        Py_DECREF(ch);
      }
    }
    break;
  case MYX_DICT_VALUE:
    obj= PyDict_New();
    for (i= 0; i < value->value.d->items_num; i++)
    {
      if (deep)
      {
        PyObject *ch= myx_py_value_to_object(value->value.d->items[i].value, deep);
        if (ch)
        {
          PyDict_SetItemString(obj, value->value.d->items[i].key, ch);
          Py_DECREF(ch);
        }
      }
      else
      {
        PyObject *ch= (PyObject*)myx_py_grtvalue_create(value->value.d->items[i].value);
        PyDict_SetItemString(obj, value->value.d->items[i].key, ch);
        Py_DECREF(ch);
      }
    }
    break;
   case MYX_ANY_VALUE:
    g_assert(0);
  }

  return obj;
}



MYX_GRT_VALUE *myx_py_object_to_value(PyObject *object)
{
  if (PyGRTValue_Check(object))
  {
    MYX_GRT_VALUE *tmp= PyGRTValue_Value(object);
    myx_grt_value_retain(tmp);
    return tmp;
  }
  else if (PyInt_Check(object))
  {
    return myx_grt_value_from_int(PyInt_AsLong(object));
  }
  else if (PyFloat_Check(object))
  {
    return myx_grt_value_from_real(PyFloat_AsDouble(object));
  }
  else if (PyString_Check(object))
  {
    return myx_grt_value_from_string(PyString_AsString(object));
  }
  else if (PyList_Check(object) || PyTuple_Check(object))
  {
    unsigned int i, count= PySequence_Size(object);
    MYX_GRT_VALUE_TYPE content_type, ctype;
    MYX_GRT_VALUE *value;

    for (i= 0; i < count; i++)
    {
      PyObject *item= PySequence_GetItem(object, i);

      if (PyInt_Check(item))
        ctype= MYX_INT_VALUE;
      else if (PyFloat_Check(item))
        ctype= MYX_REAL_VALUE;
      else if (PyString_Check(item))
        ctype= MYX_STRING_VALUE;
      else if (PyList_Check(item) || PyTuple_Check(item))
        ctype= MYX_LIST_VALUE;
      else if (PyDict_Check(item))
        ctype= MYX_DICT_VALUE;
      else if (PyGRTValue_Check(object))
        ctype= myx_grt_value_get_type(PyGRTValue_Value(object));
      else
      {
        PyObject *repr= PyObject_Repr(item);
        g_warning("received invalid data from python function: %s",
                  PyString_AsString(repr));
        Py_DECREF(repr);
        return NULL;
      }

      if (i == 0)
        content_type= ctype;
      else
        if (ctype != content_type)
        {
          content_type= MYX_ANY_VALUE;
          break;
        }
    }

    value= myx_grt_list_new(content_type, NULL);
    if (!value)
      return NULL;

    for (i= 0; i < count; i++)
    {
      PyObject *item= PySequence_GetItem(object, i);
      MYX_GRT_VALUE *gitem;

      gitem= myx_py_object_to_value(item);
      myx_grt_list_item_add(value, gitem);
      myx_grt_value_release(gitem);
    }
    return value;
  }
  else if (PyDict_Check(object))
  {
    int i;
    MYX_GRT_VALUE *value;
    PyObject *pykey, *pyvalue;

    i= 0;
    value= myx_grt_dict_new(NULL, NULL);
    while (PyDict_Next(object, &i, &pykey, &pyvalue))
    {
      MYX_GRT_VALUE *tmp;

      if (!PyString_Check(pykey))
      {
        PyObject *repr= PyObject_Repr(pykey);
        g_warning("received data from python has non-string key: %s",
                  PyString_AsString(repr));
        Py_DECREF(repr);
        myx_grt_value_release(value);
        return NULL;
      }

      tmp= myx_py_object_to_value(pyvalue);
      if (!tmp)
      {
        myx_grt_value_release(value);
        return NULL;
      }

      myx_grt_bridge_dict_item_set_value(value, PyString_AsString(pykey), tmp, 0);
      myx_grt_value_release(tmp);
    }

    return value;
  }
  else if (object == Py_None)
  {
    return NULL;
  }
  else
  {
    PyObject *repr= PyObject_Repr(object);
    g_warning("received invalid data from python function: %s",
              PyString_AsString(repr));
    Py_DECREF(repr);

    return NULL;
  }
}



// ----------------------------------------------------------------------
// GRT Value Wrapper Object




PyDoc_STRVAR(grtvaluetype_doc,
"GRT value wrapper object.");


MYX_GRT_VALUE_PYOBJECT *myx_py_grtvalue_create(MYX_GRT_VALUE *value)
{
  MYX_GRT_VALUE_PYOBJECT *pgv;

  pgv= PyObject_New(MYX_GRT_VALUE_PYOBJECT, &GRTValueType);
  if (pgv == NULL)
    return NULL;

  pgv->value= myx_grt_value_retain(value);
  pgv->grt= myx_py_get_grt()->grt;

  return pgv;
}



static MYX_GRT_VALUE_PYOBJECT *grtvalue_create_object(PyObject *self, PyObject *args)
{
  char *gstruct, *name= NULL, *id= NULL, *owner= NULL;
  MYX_GRT_PYCONTEXT *ctx= myx_py_get_grt();
  MYX_GRT_VALUE_PYOBJECT *tmp;
  MYX_GRT_VALUE *value;

  if (!PyArg_ParseTuple(args, "s|zzz", &gstruct, &name, &id, &owner))
    return NULL;
  
  value= myx_grt_dict_new_obj(ctx->grt, gstruct, name, id, owner);
  if (!value)
  {
    PyErr_SetString(ctx->error, "could not create GRT object (bad struct?)");
    return NULL;
  }

  tmp= myx_py_grtvalue_create(value);
  myx_grt_value_release(value);
  
  return tmp;
}



static MYX_GRT_VALUE_PYOBJECT *grtvalue_create_list(PyObject *self, PyObject *args)
{
  MYX_GRT_VALUE_PYOBJECT *tmp;
  char *ctype= NULL, *cstruct= NULL;
  MYX_GRT_VALUE_TYPE type;
  MYX_GRT_VALUE *value;
  MYX_GRT_ERROR error;

  if (!PyArg_ParseTuple(args, "|zz", &ctype, &cstruct))
    return NULL;

  if (ctype == NULL)
    type= MYX_ANY_VALUE;
  else
  {
    type= myx_get_value_type_from_string(ctype, &error);
    if (error != MYX_GRT_NO_ERROR)
    {
      PyErr_Format(PyExc_ValueError, "invalid GRT type '%s'", ctype);
      return NULL;
    }
  }

  value= myx_grt_list_new(type, cstruct);
  if (value)
  {
    tmp= myx_py_grtvalue_create(value);
    myx_grt_value_release(value);
  }
  else
  {
    PyErr_SetString(PyExc_MemoryError, "could not create list object");
    return NULL;
  }
  return tmp;
}


static MYX_GRT_VALUE_PYOBJECT *grtvalue_create_dict(PyObject *self, PyObject *args)
{
  MYX_GRT_VALUE_PYOBJECT *tmp;
  char *ctype= NULL, *cstruct= NULL;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE_TYPE type;
  MYX_GRT_ERROR error;

  if (!PyArg_ParseTuple(args, "|zz", &ctype, &cstruct))
    return NULL;

  if (ctype == NULL)
    type= MYX_ANY_VALUE;
  else
  {
    type= myx_get_value_type_from_string(ctype, &error);
    if (error != MYX_GRT_NO_ERROR)
    {
      PyErr_Format(PyExc_ValueError, "invalid GRT type '%s'", ctype);
      return NULL;
    }
  }

  value= myx_grt_dict_new_typed(type, cstruct);
  if (value)
  {
    tmp=  myx_py_grtvalue_create(value);
    myx_grt_value_release(value);
  }
  else
  {
    PyErr_SetString(PyExc_MemoryError, "could not create dict object");
    return NULL;
  }
  return tmp;
}


static MYX_GRT_VALUE_PYOBJECT *grtvalue_create_from_xml(PyObject *self, PyObject *args)
{
  MYX_GRT_VALUE_PYOBJECT *pgv;
  const char *data= NULL;
  int length= 0;
  MYX_GRT_VALUE *value;
  
  if (!PyArg_ParseTuple(args, "s#", &data, &length))
    return NULL;
  
  if (!data)
  {
    PyErr_SetString(PyExc_ValueError, "invalid argument, must be a XML string");
    return NULL;
  }

  value= myx_grt_value_from_xml(myx_py_get_grt()->grt, data, length);
  if (!value)
  {
    PyErr_SetString(PyExc_ValueError, "could not parse XML data");
    return NULL;
  }
  
  pgv= PyObject_New(MYX_GRT_VALUE_PYOBJECT, &GRTValueType);
  if (pgv == NULL)
    return NULL;

  pgv->value= value;
  
  pgv->grt= myx_py_get_grt()->grt;

  return pgv;
}



static MYX_GRT_VALUE_PYOBJECT *grtvalue_create_from_python(PyObject *self, PyObject *args)
{
  MYX_GRT_VALUE_PYOBJECT *pgv;
  MYX_GRT_VALUE *value;
  PyObject *pyvalue;
  
  if (!PyArg_ParseTuple(args, "O", &pyvalue))
    return NULL;

  if (!pyvalue)
  {
    PyErr_SetString(PyExc_ValueError, "invalid argument, must be a Python object (primitives, dict and list)");
    return NULL;
  }

  value= myx_py_object_to_value(pyvalue);
  if (!value)
  {
    PyErr_SetString(PyExc_ValueError, "invalid argument, must be a Python object (primitives, dict and list)");
    return NULL;
  }
  
  pgv= PyObject_New(MYX_GRT_VALUE_PYOBJECT, &GRTValueType);
  if (pgv == NULL)
    return NULL;

  pgv->value= value;
  
  pgv->grt= myx_py_get_grt()->grt;

  return pgv;
}


static void grtvalue_dealloc(MYX_GRT_VALUE_PYOBJECT *self)
{
  if (PyGRTValue_Value(self))
    myx_grt_value_release(PyGRTValue_Value(self));
  PyObject_Del(self);
}


static int grtvalue_m_length(MYX_GRT_VALUE_PYOBJECT *self)
{
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_DICT_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type DICT");
    return -1;
  }

  return myx_grt_bridge_dict_item_count(PyGRTValue_Value(self), 0);
}


static PyObject* grtvalue_m_get(MYX_GRT_VALUE_PYOBJECT *self, PyObject *key)
{
  MYX_GRT_VALUE *value;
  int idx;

  switch (myx_grt_value_get_type(PyGRTValue_Value(self)))
  {
  case MYX_DICT_VALUE:
    if (!PyString_Check(key))
    {
      PyErr_SetString(PyExc_TypeError, "key must be a string");
      return NULL;
    }
    value= myx_grt_bridge_dict_item_get_value(PyGRTValue_Value(self), PyString_AsString(key), 0);
    if (!value)
    {
      PyErr_Format(PyExc_KeyError, "DICT does not contain key '%s'", PyString_AsString(key));
      return NULL;
    }
    break;
  case MYX_LIST_VALUE:
    if (!PyInt_Check(key))
    {
      PyErr_SetString(PyExc_TypeError, "key must be an integer");
      return NULL;
    }
    idx= PyInt_AsLong(key);
    if (idx < 0 || idx >= (int)myx_grt_bridge_list_item_count(PyGRTValue_Value(self)))
    {
      PyErr_Format(PyExc_IndexError, "LIST does not have index '%i'", idx);
      return NULL;
    }
    value= myx_grt_bridge_list_item_get(PyGRTValue_Value(self), idx, 0);
    if (!value)
    {
      PyErr_Format(PyExc_IndexError, "LIST does not have index '%i'", idx);
      return NULL;
    }
    break;
  default:
    PyErr_SetString(PyExc_ValueError, "GRT value not of type DICT or LIST");
    return NULL;
  }

  return (PyObject*)myx_py_grtvalue_create(value);
}


static int grtvalue_m_set(MYX_GRT_VALUE_PYOBJECT *self, PyObject *key, PyObject *value)
{
  MYX_GRT_VALUE *gvalue;
  
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_DICT_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type DICT");
    return -1;
  }
  if (!PyString_Check(key))
  {
    PyErr_SetString(PyExc_TypeError, "key must be a string");
    return -1;
  }
  
  if (!PyGRTValue_Check(value))
  {
    gvalue= myx_py_object_to_value(value);
    if (!gvalue)
    {
      PyErr_SetString(PyExc_TypeError, "value must be a GRT value or a convertible native object");
      return -1;
    }
  }
  else
    gvalue= myx_grt_value_retain(PyGRTValue_Value(value));
  
  myx_grt_bridge_dict_item_set_value(PyGRTValue_Value(self), PyString_AsString(key), gvalue, 0);

  myx_grt_value_release(gvalue);
  
  return 0;
}


static int grtvalue_sq_length(MYX_GRT_VALUE_PYOBJECT *self)
{  
  switch (myx_grt_value_get_type(PyGRTValue_Value(self)))
  {
  case MYX_LIST_VALUE:
    return myx_grt_list_item_count(PyGRTValue_Value(self));
    
  case MYX_DICT_VALUE:
    return myx_grt_dict_item_count(PyGRTValue_Value(self));

  default:
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST or DICT");
    return -1;
  }
}


static PyObject *grtvalue_sq_item(MYX_GRT_VALUE_PYOBJECT *self, int i)
{
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_LIST_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST");
    return NULL;
  }
  if (i >= (int)myx_grt_bridge_list_item_count(PyGRTValue_Value(self)))
  {
    PyErr_SetString(PyExc_IndexError, "list index out of range");
    return NULL;
  }

  return (PyObject*)myx_py_grtvalue_create(myx_grt_bridge_list_item_get(PyGRTValue_Value(self), i, 0));
}


static int grtvalue_sq_ass_item(MYX_GRT_VALUE_PYOBJECT *self, int i, PyObject *value)
{
  MYX_GRT_VALUE *gvalue;
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_LIST_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST");
    return -1;
  }
  if (!PyGRTValue_Check(value))
  {
    gvalue= myx_py_object_to_value(value);
    if (!gvalue)
    {
      PyErr_SetString(PyExc_TypeError, "value must be a GRT value or a convertible native object");
      return -1;
    }
  }
  else
    gvalue= myx_grt_value_retain(PyGRTValue_Value(value));

  if (i >= (int)myx_grt_bridge_list_item_count(PyGRTValue_Value(self)))
  {
    PyErr_SetString(PyExc_IndexError, "list assignment index out of range");
    return -1;
  }

  myx_grt_list_item_set(PyGRTValue_Value(self), i, gvalue);

  return 0;
}


static int grtvalue_sq_contains(MYX_GRT_VALUE_PYOBJECT *self, PyObject *value)
{
  MYX_GRT_VALUE_TYPE content_type;
  MYX_GRT_VALUE *gvalue= PyGRTValue_Value(self);

  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_LIST_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST");
    return -1;
  }
  
  content_type= myx_grt_list_content_get_type(PyGRTValue_Value(self));

  if (PyGRTValue_Check(value))
  {
    if (myx_grt_value_get_type(PyGRTValue_Value(value)) == content_type
        || content_type == MYX_ANY_VALUE)
    {
      unsigned int i;
      for (i= 0; i < gvalue->value.l->items_num && gvalue->value.l->items[i] != PyGRTValue_Value(value); i++);
      
      if (i < gvalue->value.l->items_num)
        return 1;
    }
    return 0;
  }
  else if (PyInt_Check(value))
  {
    if (content_type == MYX_ANY_VALUE || content_type == MYX_INT_VALUE)
    {
      unsigned int i;
      long intval= PyInt_AS_LONG(value);

      for (i= 0; i < gvalue->value.l->items_num; i++)
      {
        if (gvalue->value.l->items[i]->type == MYX_INT_VALUE
            && myx_grt_value_as_int(gvalue->value.l->items[i]) == intval)
          return 1;
      }
      
      if (i < gvalue->value.l->items_num)
        return 1;
    }
    return 0;
  }
  else if (PyString_Check(value))
  {
    if (content_type == MYX_ANY_VALUE || content_type == MYX_STRING_VALUE)
    {
      unsigned int i;
      const char *sval= PyString_AS_STRING(value);

      for (i= 0; i < gvalue->value.l->items_num; i++)
      {
        if (gvalue->value.l->items[i]->type == MYX_STRING_VALUE
            && strcmp(myx_grt_value_as_string(gvalue->value.l->items[i]), sval)==0)
          return 1;
      }

      if (i < gvalue->value.l->items_num)
        return 1;
    }
    return 0;
  }
  else if (PyFloat_Check(value))
  {
    if (content_type == MYX_ANY_VALUE || content_type == MYX_REAL_VALUE)
    {
      unsigned int i;
      double dval= PyFloat_AS_DOUBLE(value);

      for (i= 0; i < gvalue->value.l->items_num; i++)
      {
        if (gvalue->value.l->items[i]->type == MYX_REAL_VALUE
            && myx_grt_value_as_real(gvalue->value.l->items[i]) == dval)
          return 1;
      }
      
      if (i < gvalue->value.l->items_num)
        return 1;
    }
    return 0;
  }
  else // other types are not supported
  {
    PyErr_SetString(PyExc_TypeError, "can only check if primitive or GRT values are in a GRT list");
    return -1;
  }
}



static PyObject *grtvalue_get_type(MYX_GRT_VALUE_PYOBJECT *self)
{
  return PyString_FromString(myx_get_value_type_as_string(myx_grt_value_get_type(PyGRTValue_Value(self))));
}


static PyObject *grtvalue_get_content_type(MYX_GRT_VALUE_PYOBJECT *self)
{
  MYX_GRT_VALUE_TYPE type= myx_grt_value_get_type(PyGRTValue_Value(self));
  
  switch (type)
  {
  case MYX_DICT_VALUE:
    return PyString_FromString(myx_get_value_type_as_string(myx_grt_dict_content_get_type(PyGRTValue_Value(self))));

  case MYX_LIST_VALUE:
    return PyString_FromString(myx_get_value_type_as_string(myx_grt_list_content_get_type(PyGRTValue_Value(self))));

  default:
    PyErr_SetString(PyExc_ValueError, "GRT value not a container type");
    return NULL;
  }
}


static PyObject *pyobject_from_struct(MYX_GRT_STRUCT *gstruct, int full)
{
  PyObject *dict= PyDict_New();
  PyObject *obj;
  PyObject *members;
  unsigned int i;
  
#define SET_ITEM(dict, key, value, add_this) if (value && (add_this)) { PyObject *obj= PyString_FromString(value); PyDict_SetItemString(dict, key, obj); Py_DECREF(obj); } else PyDict_SetItemString(dict, key, Py_None)

  SET_ITEM(dict, "name", gstruct->name, 1);
  SET_ITEM(dict, "parent", gstruct->parent_struct_name, 1);
  SET_ITEM(dict, "caption", gstruct->caption, full);
  SET_ITEM(dict, "description", gstruct->desc, full);

  members= PyList_New(gstruct->members_num);
  for (i= 0; i < gstruct->members_num; i++)
  {
    PyObject *member= PyDict_New();

    SET_ITEM(member, "name", gstruct->members[i].name, 1);
    SET_ITEM(member, "caption", gstruct->members[i].caption, full);
    SET_ITEM(member, "description", gstruct->members[i].desc, full);
    SET_ITEM(member, "type", myx_get_value_type_as_string(gstruct->members[i].value_type), 1);
    SET_ITEM(member, "struct", gstruct->members[i].content_struct_name, 1);
    obj= PyInt_FromLong(gstruct->members[i].is_ref);
    PyDict_SetItemString(member, "isReference", obj);
    Py_DECREF(obj);
    SET_ITEM(member, "overrides", gstruct->members[i].overrides, 1);

    // setitem does not retain member
    PyList_SetItem(members, i, member);
  }
  
  PyDict_SetItemString(dict, "members", members);
  
  return dict;
}


static PyObject *grtvalue_get_struct(MYX_GRT_VALUE_PYOBJECT *self, PyObject *args)
{
  MYX_GRT_STRUCT *gstruct;
  int full= 0;

  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_DICT_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type DICT");
    return NULL;
  }
  if (!PyArg_ParseTuple(args, "|i", &full))
      return NULL;

  gstruct= myx_grt_dict_struct_get(self->grt, PyGRTValue_Value(self));
  if (gstruct)
    return pyobject_from_struct(gstruct, 1);
  else
  {
    Py_INCREF(Py_None);
    return Py_None;
  }
}
  
  
static PyObject *grtvalue_get_content_struct(MYX_GRT_VALUE_PYOBJECT *self, PyObject *args)
{
  const char *strname= NULL;
  MYX_GRT_STRUCT *gstruct;
  MYX_GRT_VALUE_TYPE type= myx_grt_value_get_type(PyGRTValue_Value(self));
  int full= 0;
  int error= 0;

  if (!PyArg_ParseTuple(args, "|i", &full))
    return NULL;

  switch (type)
  {
  case MYX_DICT_VALUE:
    if (myx_grt_dict_content_get_type(PyGRTValue_Value(self)) == MYX_DICT_VALUE &&
        myx_grt_dict_content_get_type(PyGRTValue_Value(self)) == MYX_LIST_VALUE)
      strname= myx_grt_dict_content_get_struct_name(PyGRTValue_Value(self));
    else
    {
      error= 1;
      PyErr_SetString(PyExc_ValueError, "GRT value contents is not a container type");
    }
    break;

  case MYX_LIST_VALUE:
    if (myx_grt_list_content_get_type(PyGRTValue_Value(self)) == MYX_DICT_VALUE &&
        myx_grt_list_content_get_type(PyGRTValue_Value(self)) == MYX_LIST_VALUE)
      strname= myx_grt_list_content_get_struct_name(PyGRTValue_Value(self));
    else
    {
      error= 1;
      PyErr_SetString(PyExc_ValueError, "GRT value contents is not a container type");
    }
    break;

  default:
    PyErr_SetString(PyExc_ValueError, "GRT value not a container type");
    error= 1;
    break;
  }

  if (error)
    return NULL;

  gstruct= myx_grt_dict_struct_get(self->grt, PyGRTValue_Value(self));

  return pyobject_from_struct(gstruct, full);
}


static PyObject *grtvalue_append(MYX_GRT_VALUE_PYOBJECT *self, PyObject *args)
{
  PyObject *object;
  MYX_GRT_VALUE *value;
  
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_LIST_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST");
    return 0;
  }

  if (!PyArg_ParseTuple(args, "O", &object))
    return NULL;
  
  value= myx_py_object_to_value(object);
  if (!value)
  {
    PyErr_SetString(PyExc_ValueError, "argument must be a GRT value or convertible type");
    return NULL;
  }
  myx_grt_list_item_insert(self->value, -1, value);
  myx_grt_value_release(value);
  
  Py_INCREF(self);
  return (PyObject*)self;
}


static PyObject *grtvalue_insert(MYX_GRT_VALUE_PYOBJECT *self, PyObject *args)
{
  PyObject *object;
  MYX_GRT_VALUE *value;
  int idx;
  
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_LIST_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type LIST");
    return 0;
  }

  if (!PyArg_ParseTuple(args, "iO", &idx, &object))
    return NULL;

  value= myx_py_object_to_value(object);
  if (!value)
  {
    PyErr_SetString(PyExc_ValueError, "argument must be a GRT value or convertible type");
    return NULL;
  }
  myx_grt_list_item_insert(self->value, idx, value);
  myx_grt_value_release(value);

  Py_INCREF(self);
  return (PyObject*)self;
}



static PyObject *grtvalue_as_xml(MYX_GRT_VALUE_PYOBJECT *self)
{
  PyObject *obj;
  char *xml= myx_grt_value_to_xml(self->grt, PyGRTValue_Value(self));
  if (!xml)
  {
    PyErr_SetString(PyExc_MemoryError, "Out of memory generating xml");
  }
  obj= PyString_FromString(xml);
  if (!obj)
  {
    PyErr_SetString(PyExc_MemoryError, "Out of memory");
    free(xml);
    return NULL;
  }
  free(xml);
  return obj;
}


static PyObject *grtvalue_as_python(MYX_GRT_VALUE_PYOBJECT *self, PyObject *args)
{
  PyObject *ret;
  int deep= 0;
  
  if (!PyArg_ParseTuple(args, "|i", &deep))
    return NULL;
  
  ret= myx_py_value_to_object(PyGRTValue_Value(self), deep);
  if (!ret)
  {
    PyErr_SetString(PyExc_MemoryError, "Out of memory converting to Python object");
    return NULL;
  }
  
  return ret;
}


static PyObject *grtvalue_keys(MYX_GRT_VALUE_PYOBJECT *self)
{
  unsigned int i;
  PyObject *ret= PyList_New(self->value->value.d->items_num);

  for (i= 0; i < self->value->value.d->items_num; i++)
  {
    PyList_SetItem(ret, i, PyString_FromString(self->value->value.d->items[i].key));
  }

  return ret;
}


PyDoc_STRVAR(grtv_get_type_doc,
"getType() => string"NLNL
"Returns the name of the GRT type of the value (string,int,real,list,dict).");

PyDoc_STRVAR(grtv_get_struct_doc,
"getStruct(full) => dict"NLNL
"Returns a dict containing the GRT struct definition of the value."NL
"If full is 1 it will include documentation available for the struct.");

PyDoc_STRVAR(grtv_get_content_type_doc,
"getContentType() => string"NLNL
"Returns the type of the GRT value contents, if it's a list or dict."NL
"Will raise an exception if it's another of type.");

PyDoc_STRVAR(grtv_get_content_struct_doc,
"getContentStruct(full) => dict"NLNL
"Returns a dict containing the GRT struct definition for the"NL
"GRT value contents. Both the value itself and the content type"NL
"of the value must be list or string, otherwise an exception is raised.");

PyDoc_STRVAR(grtv_to_xml_doc,
"toXML() => string"NLNL
"Returns a XML representation of the GRT value. The string may be"NL
"passed to grt.valueFromXML() to create a GRT value object back from it.");

PyDoc_STRVAR(grtv_to_python_doc,
"toPython(recursively=0) => object"NLNL
"Returns a native Python object that represents the GRT value."NL
"If you specify 1 to it's parameter, container types will be converted"NL
"recursively until the last element."NL
"Although you can recreate a similar object with the"NL
"grt.valueFromPython() function from the results of this,"NL
"the information about GRT structs will be lost."NL
"Use toXML() if you wish to storage GRT values.");

PyDoc_STRVAR(grtv_append_doc,
"append(value)"NLNL
"Appends an item to a list GRT value. The value will be converted"NL
"to a GRT representation if it's not already.");

PyDoc_STRVAR(grtv_insert_doc,
"insert(index, value)"NLNL
"Inserts an item to a list GRT value, at the indicated index."NL
"The value will be converted to a GRT representation if it's"NL
"not already.");


PyDoc_STRVAR(grtv_keys_doc,
"keys() => list"NLNL
"Returns the list of keys present in a dict type GRT value.");


static PyMethodDef grtvalue_methods[]= {
  {"getType",    (PyCFunction)grtvalue_get_type, METH_NOARGS, grtv_get_type_doc},
  {"getStruct",  (PyCFunction)grtvalue_get_struct, METH_VARARGS, grtv_get_struct_doc},
  {"getContentType", (PyCFunction)grtvalue_get_content_type, METH_NOARGS, grtv_get_content_type_doc},
  {"getContentStruct", (PyCFunction)grtvalue_get_content_struct, METH_VARARGS, grtv_get_content_struct_doc},
  {"toXML",      (PyCFunction)grtvalue_as_xml, METH_NOARGS, grtv_to_xml_doc},
  {"toPython",   (PyCFunction)grtvalue_as_python, METH_VARARGS, grtv_to_python_doc},

  {"append",     (PyCFunction)grtvalue_append, METH_VARARGS, grtv_append_doc},
  {"insert",     (PyCFunction)grtvalue_insert, METH_VARARGS, grtv_insert_doc},
  
  {"keys",       (PyCFunction)grtvalue_keys, METH_NOARGS, grtv_keys_doc},

  {NULL, NULL}
};


static PyObject* grtvalue_getattr(MYX_GRT_VALUE_PYOBJECT *self, char *attr)
{
  MYX_GRT_VALUE *value;
  
  if (strcmp(attr, "__methods__")==0)
  {
    PyObject *list= PyList_New(0);
    int i;
    
    for (i= 0; grtvalue_methods[i].ml_name; i++)
    {
      PyList_Append(list, PyString_FromString(grtvalue_methods[i].ml_name));
    }
    return list;
  }

  if (myx_grt_value_get_type(PyGRTValue_Value(self)) == MYX_DICT_VALUE)
    value= myx_grt_bridge_dict_item_get_value(PyGRTValue_Value(self), attr, 0);
  else
    value= NULL;

  if (!value)
  {
    return Py_FindMethod(grtvalue_methods, (PyObject *)self, attr);
  }
  return (PyObject*)myx_py_grtvalue_create(value);
}


static int grtvalue_setattr(MYX_GRT_VALUE_PYOBJECT *self, char *attr, PyObject *value)
{
  MYX_GRT_VALUE *obj;
  
  if (myx_grt_value_get_type(PyGRTValue_Value(self)) != MYX_DICT_VALUE)
  {
    PyErr_SetString(PyExc_ValueError, "GRT value not of type DICT");
    return -1;
  }
  
  obj= myx_py_object_to_value(value);
  if (!obj)
  {
    PyErr_SetString(PyExc_ValueError, "invalid value for attribute. Must be a GRT value or a convertible one");
    return -1;
  }

  myx_grt_bridge_dict_item_set_value(PyGRTValue_Value(self), attr, obj, 0);
  myx_grt_value_release(obj);

  return 0;
}


static PyObject *grtvalue_repr(MYX_GRT_VALUE_PYOBJECT *self)
{
  PyObject *str= PyString_FromFormat("<GRT %s Value>", myx_get_value_type_as_string(myx_grt_value_get_type(self->value)));
  
  return str;
}


static PyObject *grtvalue_str(MYX_GRT_VALUE_PYOBJECT *self)
{
  PyObject *obj= myx_py_value_to_object(self->value, 1);
  PyObject *str;
  
  str= PyObject_Str(obj);
  Py_DECREF(obj);
  
  return str;
}


static int grtvalue_compare(MYX_GRT_VALUE_PYOBJECT *self, MYX_GRT_VALUE_PYOBJECT *other)
{
  // 0 = equal, 1 or -1 = different
  
  if (!PyGRTValue_Check(other))
  {
    return 1;
  }
  else
  {
    if (PyGRTValue_Value(self) == PyGRTValue_Value(other))
      return 0;
    
    if (myx_grt_value_get_type(PyGRTValue_Value(self)) != myx_grt_value_get_type(PyGRTValue_Value(other)))
    {
      return -1;
    }

    
    
  }
  return 1;
}


static PyMappingMethods grtvalue_mapping_methods= 
{
    (inquiry)grtvalue_m_length,
    (binaryfunc)grtvalue_m_get,
    (objobjargproc)grtvalue_m_set
};


static PySequenceMethods grtvalue_sequence_methods= 
{
    (inquiry)grtvalue_sq_length,
    0, //sq_concat,
    0, //sq_repeat,
    (intargfunc)grtvalue_sq_item,
    0, // sq_slice,
    (intobjargproc)grtvalue_sq_ass_item,
    0, // sq_ass_slice,
    (objobjproc)grtvalue_sq_contains,
    0, //sq_inplace_concat
    0, //sq_inplace_repeat
};


static PyTypeObject GRTValueType_copy= {
    PyObject_HEAD_INIT(NULL)
    0,                                 /*ob_size*/
    "grt.value",                       /*tp_name*/
    sizeof(MYX_GRT_VALUE_PYOBJECT),    /*tp_size*/
    0,                                 /*tp_itemsize*/
    /* methods */
    (destructor)grtvalue_dealloc,      /*tp_dealloc*/
    0,                                 /*tp_print*/
    (getattrfunc)grtvalue_getattr,     /*tp_getattr*/
    (setattrfunc)grtvalue_setattr,     /*tp_setattr*/
    (cmpfunc)grtvalue_compare,         /*tp_compare*/
    (reprfunc)grtvalue_repr,           /*tp_repr*/
    0,                                 /*tp_as_number*/
    &grtvalue_sequence_methods,        /*tp_as_sequence*/
    &grtvalue_mapping_methods,         /*tp_as_mapping*/
    0,                                 /*tp_hash*/
    0,                                 /*tp_call*/
    (reprfunc)grtvalue_str,            /*tp_str*/
    0,                                 /*tp_getattro*/
    0,                                 /*tp_setattro*/
    0,                                 /*tp_as_buffer*/
    0,                                 /*flags*/
    grtvaluetype_doc,                  /*tp_doc*/
    0,                                 /* tp_traverse */
    0,                                 /* tp_clear */
    0,                                 /* tp_richcompare */
    0,                                 /* tp_weaklistoffset */
    0,                                 /* tp_iter */
    0,                                 /* tp_iternext */
    grtvalue_methods,                  /* tp_methods */
    0,                                 /* tp_members */
    0,                                 /* tp_getset */         
    0,                                 /* tp_base */
    0,                                 /* tp_dict */
    0,                                 /* tp_descr_get */
    0,                                 /* tp_descr_set */
    0,                                 /* tp_dictoffset */
    0,                                 /* tp_init */
    0,                                 /* tp_alloc */
    0,                                 /* tp_new */
    0,                                 /* tp_free */
};


// ----------------------------------------------------------------------
// Function call helper for Modules

typedef struct {
  PyObject_HEAD
  MYX_GRT_FUNCTION *function;
} MYX_GRT_MODULEFUNC_PYOBJECT;


static MYX_GRT_MODULEFUNC_PYOBJECT *grtmodulefunc_create(MYX_GRT_FUNCTION *function)
{
  MYX_GRT_MODULEFUNC_PYOBJECT *pgv;

  pgv= PyObject_New(MYX_GRT_MODULEFUNC_PYOBJECT, &GRTModuleFunctionType);
  if (pgv == NULL)
    return NULL;

  pgv->function= function;

  return pgv;
}


static void grtmodulefunc_dealloc(MYX_GRT_MODULEFUNC_PYOBJECT *self)
{
  PyObject_Del(self);
}



static PyObject *grt_do_function_call(MYX_GRT_FUNCTION *function, PyObject *args)
{
  MYX_GRT_VALUE *gargs, *retval;
  PyObject *res;
  MYX_GRT_PYCONTEXT *pycon= myx_py_get_grt();
  MYX_GRT_ERROR error;
  
  if (PyTuple_Size(args) == 0)
    gargs= NULL;
  else
  {
    gargs= myx_py_object_to_value(args);
    if (!args)
    {
      PyObject *err;
      
      err= Py_BuildValue("((ss)is)", function->module->name, function->name,
                         0, "could not convert arguments to GRT Value objects");
      PyErr_SetObject(pycon->call_error, err);
      Py_DECREF(err);

      return NULL;
    }
  }

  retval= myx_grt_function_call(function->module->loader->grt, 
                                function,
                                gargs, 
                                &error);
  if (gargs)
    myx_grt_value_release(gargs);
  
  if (retval)
  {
    MYX_GRT_VALUE *gvalue= myx_grt_dict_item_get_value(retval, "value");
    MYX_GRT_VALUE *gerror= myx_grt_dict_item_get_value(retval, "error");

    if (gerror)
    {
      PyObject *err;
      PyObject *tmp;
      
      tmp= myx_py_value_to_object(gerror, 1);
      err= Py_BuildValue("((ss)iO)", function->module->name, function->name,
                         error, tmp);
      Py_DECREF(tmp);
      PyErr_SetObject(pycon->call_error, err);
      Py_DECREF(err);

      res= NULL;
    }
    else
    {
      res= (PyObject*)myx_py_grtvalue_create(gvalue);
    }
    myx_grt_value_release(retval);
  }
  else
  {
    PyObject *err;

    err= Py_BuildValue("((ss)is)", function->module->name, function->name,
                       error, myx_grt_error_string(error));
    PyErr_SetObject(pycon->call_error, err);
    Py_DECREF(err);

    res= NULL;
  }
  
  return res;
}



static PyObject *grtmodulefunc_call(MYX_GRT_MODULEFUNC_PYOBJECT *self, PyObject *args, PyObject *kw)
{
  PyObject *res;

  res= grt_do_function_call(self->function, args);
  
  return res;
}


static PyTypeObject GRTModuleFunctionType_copy= {
    PyObject_HEAD_INIT(NULL)
    0,                        /*ob_size*/
    "grt._modulefunc",             /*tp_name*/
    sizeof(MYX_GRT_MODULEFUNC_PYOBJECT),        /*tp_size*/
    0,                        /*tp_itemsize*/
    /* methods */
    (destructor)grtmodulefunc_dealloc, /*tp_dealloc*/
    0,                        /*tp_print*/
    0,                        /*tp_getattr*/
    0,                        /*tp_setattr*/
    0,                        /*tp_compare*/
    0,                        /*tp_repr*/
    0,                        /*tp_as_number*/
    0,                        /*tp_as_sequence*/
    0,                        /*tp_as_mapping*/
    0,                        /*tp_hash*/
    (ternaryfunc)grtmodulefunc_call, /*tp_call*/
    0,                        /*tp_str*/
    0,                        /*tp_getattro*/
    0,                        /*tp_setattro*/
    0,                        /*tp_as_buffer*/
    0,                        /*flags*/
    0,                        /*tp_doc*/
};


// ----------------------------------------------------------------------
// Module Wrapper

MYX_GRT_MODULE_PYOBJECT *myx_py_grtmodule_create(MYX_GRT_MODULE *module)
{
  MYX_GRT_MODULE_PYOBJECT *pgv;

  pgv= PyObject_New(MYX_GRT_MODULE_PYOBJECT, &GRTModuleType);
  if (pgv == NULL)
    return NULL;

  pgv->module= module;

  return pgv;
}


static PyObject* grtmodule_getattr(MYX_GRT_MODULE_PYOBJECT *self, char *attr)
{
  MYX_GRT_MODULE *module= self->module;
  unsigned int i;

  if (strcmp(attr, "__name__")==0)
    return PyString_FromString(module->name);
  if (strcmp(attr, "__extends__")==0)
  {
    if (module->extends)
      return PyString_FromString(module->extends);
    else
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
  }
  if (strcmp(attr, "__members__")==0)
  {
    PyObject *list= PyList_New(2);
    PyObject *str;

    str= PyString_FromString("__name__");
    PyList_SetItem(list, 0, str);
    str= PyString_FromString("__extends__");
    PyList_SetItem(list, 1, str);
    return list;
  }
  if (strcmp(attr, "__methods__")==0)
  {
    PyObject *list= PyList_New(self->module->functions_num);
    PyObject *meth;

    for (i= 0; i < self->module->functions_num; i++)
    {
      meth= PyString_FromString(self->module->functions[i].name);
      PyList_SetItem(list, i, meth);
    }
    return list;
  }

  for (i= 0; i < self->module->functions_num; i++)
  {
    if (strcmp(self->module->functions[i].name, attr)==0)
    {
      return (PyObject*)grtmodulefunc_create(self->module->functions+i);
    }
  }

  PyErr_Format(PyExc_AttributeError, "GRT Module '%s' has no attribute '%s'", 
               module->name, attr);
  return NULL;
}


static void grtmodule_dealloc(MYX_GRT_MODULE_PYOBJECT *self)
{
  PyObject_Del(self);
}


PyDoc_STRVAR(grtmoduletype_doc,
"Proxy object for GRT modules. Allows calling of functions defined in"NL
"GRT modules/plugins."NL
"Function parameters will be automatically converted to GRT values."NL
"Use the dir() command on each specific module to list its"NL
"exported functions."NL
"You can also get that list through the __methods__ attribute."NLNL
"To make working with objects from the GRT shell easier, there are"NL
"a few convenience functions which allow you to treat the global"NL
"GRT tree as a directory/folder hierarchy."NL
"You can use the cd(), ls() and pwd() functions to navigate and"NL
MYX_SHELL_CURNODE" to refer to the current tree node.");


static PyTypeObject GRTModuleType_copy= {
    PyObject_HEAD_INIT(NULL)
    0,                        /*ob_size*/
    "grt.module",             /*tp_name*/
    sizeof(MYX_GRT_MODULE_PYOBJECT),        /*tp_size*/
    0,                        /*tp_itemsize*/
    /* methods */
    (destructor)grtmodule_dealloc,         /*tp_dealloc*/
    0,                        /*tp_print*/
    (getattrfunc)grtmodule_getattr,         /*tp_getattr*/
    0,                        /*tp_setattr*/
    0,                        /*tp_compare*/
    0,                        /*tp_repr*/
    0,                        /*tp_as_number*/
    0,                        /*tp_as_sequence*/
    0,                        /*tp_as_mapping*/
    0,                        /*tp_hash*/
    0,                        /*tp_call*/
    0,                        /*tp_str*/
    0,                        /*tp_getattro*/
    0,                        /*tp_setattro*/
    0,                        /*tp_as_buffer*/
    0,                        /*flags*/
    grtmoduletype_doc,         /*tp_doc*/
};

// ----------------------------------------------------------------------
// functions


static PyObject *grt_find_struct(PyObject *self, PyObject *args)
{
  const char *name;
  MYX_GRT_STRUCT *gstruct;
  
  if (!PyArg_ParseTuple(args, "s", &name))
    return NULL;
  
  gstruct= myx_grt_struct_get(myx_py_get_grt()->grt, name);
  if (!gstruct)
  {
    PyErr_Format(PyExc_ValueError, "struct '%s' is not defined", name);
    return NULL;
  }
  
  return pyobject_from_struct(gstruct, 1);
}



static PyObject *grt_get_global(PyObject *self, PyObject *args)
{
  const char *path;
  MYX_GRT_VALUE *value;
  
  if (!PyArg_ParseTuple(args, "s", &path))
    return NULL;

  if (*path != '/')
  {
    PyErr_Format(PyExc_ValueError, "invalid path '%s': global path must be absolute", path);
    return NULL;
  }

  value= myx_grt_dict_item_get_by_path(myx_py_get_grt()->grt, 
                                       myx_py_get_grt()->grt->root,
                                       path);
  if (!value)
  {
    PyErr_Format(PyExc_KeyError, "invalid path '%s': object does not exist",
                 path);
    return NULL;
  }

  return (PyObject*)myx_py_grtvalue_create(value);
}


static PyObject *grt_set_global(PyObject *self, PyObject *args)
{
  const char *path;
  MYX_GRT_VALUE *value;
  PyObject *object;

  if (!PyArg_ParseTuple(args, "sO", &path, &object))
    return NULL;

  if (*path != '/')
  {
    PyErr_Format(PyExc_ValueError, "invalid path '%s': global path must be absolute", path);
    return NULL;
  }
  value= myx_py_object_to_value(object);
  if (!value)
  {
    PyErr_SetString(PyExc_ValueError, "argument value must be a GRT value or a convertible value");
    return NULL;
  }

  if (strcmp(path, "/")==0)
    myx_grt_set_root(myx_py_get_grt()->grt, value);
  else
  {
    myx_grt_dict_item_set_by_path(myx_py_get_grt()->grt->root,
                                  path,
                                  value);
  }
  myx_grt_value_release(value);

  Py_INCREF(Py_None);
  return Py_None;
}


static PyObject *grt_get_modules(PyObject *self)
{
  MYX_GRT *grt= myx_py_get_grt()->grt;
  PyObject *list= PyList_New(0);
  unsigned int i;
  
  for (i= 0; i < grt->modules_num; i++)
  {
    PyList_Append(list, PyString_FromString(grt->modules[i]->name));
  }

  return list;
}


static void show_module_info(MYX_GRT_MODULE *module)
{
  MYX_GRT *grt= myx_py_get_grt()->grt;
  unsigned int i;
  
  if (module->extends)
    myx_grt_printf(grt, "Module '%s' (extends '%s')"NL, module->name, module->extends);
  else
    myx_grt_printf(grt, "Module '%s'"NL, module->name);
  for (i= 0; i < module->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    char *params= myx_grt_module_function_get_params(func);
    char *return_type= myx_grt_module_function_get_return_type(func);

    myx_grt_printf(grt, " %s:%s:%s"NL, module->functions[i].name, params, return_type);
  }
}


static PyObject *grt_show_module(PyObject *self, PyObject *args)
{
  PyObject *value;
  
  if (!PyArg_ParseTuple(args, "O", &value))
    return NULL;
  
  if (PyString_Check(value))
  {
    MYX_GRT_MODULE *module= myx_grt_find_module(myx_py_get_grt()->grt, PyString_AsString(value));
    if (!module)
    {
      PyErr_Format(PyExc_ValueError, "invalid module '%s'", PyString_AsString(value));
      return NULL;
    }
    show_module_info(module);
  }
  else if (PyGRTModule_Check(value))
  {
    show_module_info(PyGRTModule_Module(value));
  }
  else
  {
    PyErr_SetString(PyExc_ValueError, "invalid argument, must be a module name or a module");
    return NULL;
  }
  
  Py_INCREF(Py_None);
  return Py_None;
}


static PyObject *grt_call_function(PyObject *self, PyObject *args)
{
  char *moduleName, *functionName;
  PyObject *value;
  MYX_GRT_FUNCTION *function;
  
  if (!PyArg_ParseTuple(args, "ssO", &moduleName, &functionName, &value))
    return NULL;
  
  function= myx_grt_function_get(myx_py_get_grt()->grt, moduleName, functionName, 1);
  if (!function)
  {
    PyErr_Format(PyExc_ValueError, "invalid module function '%s.%s'", moduleName, functionName);
    return NULL;
  }
  
  return grt_do_function_call(function, args);
}


static PyObject *grt_print(PyObject *self, PyObject *args)
{
  PyObject *object;
  PyObject *text;

  if (!PyArg_ParseTuple(args, "O", &object))
    return NULL;

  text= PyObject_Str(object);
  if (!text)
    return NULL;

  myx_grt_printf(myx_py_get_grt()->grt, "%s", PyString_AsString(text));
  Py_DECREF(text);

  Py_INCREF(Py_None);
  return Py_None;
}


static PyObject *grt_add_msg(PyObject *self, PyObject *args)
{
  char *text;
  PyObject *details= NULL;
  MYX_STRINGLIST list;

  if (!PyArg_ParseTuple(args, "s|O!", &text, &PyList_Type, &details))
    return NULL;
  
  if (details)
  {
    int i;
    list.strings_num= PyList_Size(details);
    list.strings= g_new0(char*, list.strings_num);
    for (i= 0; i < PyList_Size(details); i++)
    {
      PyObject *item= PyList_GetItem(details, i);
      if (item && PyString_Check(item))
      {
        PyObject *str= PyObject_Str(item);
        list.strings[i]= PyString_AsString(str);
      }
      else
        list.strings[i]= "<could not convert object>";
    }
    
    myx_grt_messages_stack_add_message(myx_py_get_grt()->grt, "%s", &list, 1, text);
    g_free(list.strings);
  }
  else
    myx_grt_messages_stack_add_message(myx_py_get_grt()->grt, "%s", NULL, 0, text);

  Py_INCREF(Py_None);
  return Py_None;
}


static PyObject *grt_add_error(PyObject *self, PyObject *args)
{
  char *text;
  PyObject *details= NULL;
  MYX_STRINGLIST list;

  if (!PyArg_ParseTuple(args, "s|O!", &text, &PyList_Type, &details))
    return NULL;
  
  if (details)
  {
    int i;
    list.strings_num= PyList_Size(details);
    list.strings= g_new0(char*, list.strings_num);
    for (i= 0; i < PyList_Size(details); i++)
    {
      PyObject *item= PyList_GetItem(details, i);
      if (item && PyString_Check(item))
      {
        PyObject *str= PyObject_Str(item);
        list.strings[i]= PyString_AsString(str);
      }
      else
        list.strings[i]= "<could not convert object>";
    }
    
    myx_grt_messages_stack_add_error(myx_py_get_grt()->grt, "%s", &list, 1, text);
    g_free(list.strings);
  }
  else
    myx_grt_messages_stack_add_error(myx_py_get_grt()->grt, "%s", NULL, 0, text);

  Py_INCREF(Py_None);
  return Py_None;
}


static PyObject *grt_flush(PyObject *self)
{
  myx_grt_messages_stack_flush(myx_py_get_grt()->grt, 0);
  
  Py_INCREF(Py_None);
  return Py_None;
}



PyDoc_STRVAR(value_from_xml_doc,
"valueFromXML(xmlstring) => <GRT Value>"NLNL
"Creates a GRT value object from a XML string."NL
"The XML representation of a GRT value may be obtained with"NL
"the toXML() method of GRT value objects.");

PyDoc_STRVAR(value_from_py_doc,
"valueFromPython(object) => <GRT Value>"NLNL
"Creates a GRT value object from a Python object."NL
"The python object must be a string, int, float, dict, list"NL
"or another GRT object.");

PyDoc_STRVAR(get_struct_doc,
"getStruct(structName) => dict"NLNL
"Returns a dictionary containing the named GRT struct definition.");


PyDoc_STRVAR(value_create_list_doc,
"list(contentType=None, contentStruct=None) => <GRT Value>"NLNL
"Creates a GRT value of type list with optional content type."NL);

PyDoc_STRVAR(value_create_dict_doc,
"dict(contentType=None, contentStruct=None) => <GRT Value>"NLNL
"Creates a GRT value of type dict with optional content type."NL);


PyDoc_STRVAR(value_object_doc,
"newObj(structName, name, id=None, owner=None) => <GRT Value>"NLNL
"Creates a GRT value instance of the specified GRT struct type."NL
"If id or owner are not given, default values will be used.");



PyDoc_STRVAR(set_global_doc,
"setGlobal(path, value)"NLNL
"Sets the value in the global GRT tree to the specified one."NL
"See Also: getGlobal()");


PyDoc_STRVAR(get_global_doc,
"getGlobal(path)"NLNL
"Gets the value in the global GRT tree."NL
"See Also: setGlobal()");

PyDoc_STRVAR(show_module_doc,
"showModule(module or name)"NLNL
"Prints information about the GRT module.");

PyDoc_STRVAR(get_modules_doc,
"listModules() => module list"NLNL
"Returns a list of the currently registered GRT modules.");


PyDoc_STRVAR(call_doc,
"call(moduleName, functionName, argsTuple) => result"NLNL
"Calls a GRT module function by name. You can also call"NL
"module functions with moduleName.functionName(...).");


PyDoc_STRVAR(cd_doc,
"cd(path)"NLNL
"Changes the current working global tree node to the one"NL
"addressed by the path. You can then use "MYX_SHELL_CURNODE""NL
"to access the addressed object"NL
"See Also: ls(), pwd()");


PyDoc_STRVAR(pwd_doc,
"pwd() => string"NLNL
"Shows the path of the current working global tree node"NL
"You can access the node itself through "MYX_SHELL_CURNODE"."NL
"See Also: ls(), cd()");


PyDoc_STRVAR(ls_doc,
"ls(path=None)"NLNL
"Shows formatted contents of the current global tree node or"NL
"the one refered to by path. You can get a raw list of members"NL
"with dir(object)."NL
"See Also: cd(), pwd()");



extern PyObject *myx_py_grt_pwd(PyObject *self);
extern PyObject *myx_py_grt_ls(PyObject *self, PyObject *args);
extern PyObject *myx_py_grt_cd(PyObject *self, PyObject *args);


static PyMethodDef grt_functions[]= {
  {"valueFromXML", (PyCFunction)grtvalue_create_from_xml,  METH_VARARGS, value_from_xml_doc},
  {"valueFromPython", (PyCFunction)grtvalue_create_from_python, METH_VARARGS, value_from_py_doc},
  {"newObj", (PyCFunction)grtvalue_create_object, METH_VARARGS, value_object_doc},
  {"newList", (PyCFunction)grtvalue_create_list, METH_VARARGS, value_create_list_doc},
  {"newDict", (PyCFunction)grtvalue_create_dict, METH_VARARGS, value_create_dict_doc},
  {"getStruct", (PyCFunction)grt_find_struct, METH_VARARGS, get_struct_doc},
  {"getGlobal", (PyCFunction)grt_get_global, METH_VARARGS, get_global_doc},
  {"setGlobal", (PyCFunction)grt_set_global, METH_VARARGS, set_global_doc},
  
  {"listModules",  (PyCFunction)grt_get_modules, METH_NOARGS, get_modules_doc},
  {"showModule", (PyCFunction)grt_show_module, METH_VARARGS, show_module_doc},
  
  {"call", (PyCFunction)grt_call_function, METH_VARARGS, call_doc},

  {"cd", (PyCFunction)myx_py_grt_cd, METH_VARARGS, cd_doc},
  {"ls", (PyCFunction)myx_py_grt_ls, METH_VARARGS, ls_doc},
  {"pwd", (PyCFunction)myx_py_grt_pwd, METH_NOARGS, pwd_doc},

  {"write", (PyCFunction)grt_print, METH_VARARGS, NULL},
  {"addMsg", (PyCFunction)grt_add_msg, METH_VARARGS, NULL},
  {"addError", (PyCFunction)grt_add_error, METH_VARARGS, NULL},
  {"flush", (PyCFunction)grt_flush, METH_NOARGS, NULL},
  
  {NULL, NULL}
};



// ----------------------------------------------------------------------

PyDoc_STRVAR(module_doc,
"GRT Environment module.");


static void module_add_listener(MYX_GRT *grt, char *name, void *param, void *userdata)
{
  unsigned int i;
  MYX_GRT_PYCONTEXT *pycon= (MYX_GRT_PYCONTEXT*)userdata;
  PyObject *globals;
  PyObject *mainmod;
  
  myx_py_acquire(pycon);
  
  mainmod= PyImport_AddModule("__main__");
  globals= PyModule_GetDict(mainmod);
  
  for (i= 0; i < grt->modules_num; i++)
  {
    PyObject *modobj= (PyObject*)myx_py_grtmodule_create(grt->modules[i]);
    
    PyDict_SetItemString(globals, grt->modules[i]->name, modobj);
    Py_DECREF(modobj);
  }
  myx_py_release(pycon);
}


MYX_GRT_PYCONTEXT *myx_py_setup_environment(MYX_GRT *grt)
{
  MYX_GRT_PYCONTEXT *pycon;
  PyObject *grtobj;
  PyObject *grtmod, *moddict;
  PyThreadState *pystate;
  
  PyEval_AcquireLock();
  pystate= Py_NewInterpreter();
  if (!pystate)
  {
    PyEval_ReleaseLock();
    return NULL;
  }
  
  // to workaround a limitation in BDS
  GRTValueType= GRTValueType_copy;
  GRTModuleType= GRTModuleType_copy;
  GRTModuleFunctionType= GRTModuleFunctionType_copy;


  GRTValueType.ob_type= &PyType_Type;
  GRTModuleType.ob_type= &PyType_Type;
  GRTModuleFunctionType.ob_type= &PyType_Type;

  // create a module called grt and import it

  grtmod= Py_InitModule3("grt", grt_functions, module_doc);
  moddict= PyModule_GetDict(grtmod);

  PyDict_SetItemString(moddict, "ValueType", (PyObject*)&GRTValueType);
  PyDict_SetItemString(moddict, "ModuleType", (PyObject*)&GRTModuleType);
  PyDict_SetItemString(moddict, "ModuleFunctionType", (PyObject*)&GRTModuleFunctionType);

  pycon= g_new0(MYX_GRT_PYCONTEXT, 1);

  pycon->grt= grt;
  pycon->module= grtmod;
  pycon->state= pystate;

  pycon->error= PyErr_NewException("grt.error", NULL, NULL);
  if (pycon->error == NULL)
  {
    g_warning("Error creating Python exception object for GRT");
  }
  else
  {
    PyModule_AddObject(grtmod, "error", pycon->error);
    
    pycon->call_error= PyErr_NewException("grt.callerror",
                                          pycon->error, NULL);
    PyModule_AddObject(grtmod, "callerror", pycon->call_error);
  }
  grtobj= PyCObject_FromVoidPtrAndDesc(pycon, &GRTTypeSignature, NULL);
  PyDict_SetItemString(moddict, "__grt", grtobj);

  moddict= PyImport_AddModule("__main__");
  PyDict_SetItemString(PyModule_GetDict(moddict),
                       "grt", PyImport_ImportModule("grt"));

  moddict= PyImport_AddModule("sys");
  PyDict_SetItemString(PyModule_GetDict(moddict), "stdout", grtmod);
  PyDict_SetItemString(PyModule_GetDict(moddict), "stderr", grtmod);
  
  PyEval_ReleaseThread(pystate);

  // register listener for module notifications
  myx_grt_add_listener(grt, module_add_listener, GRT_MODULE_ADD_NOTIFICATION, pycon);

//  module_add_listener(grt, GRT_MODULE_ADD_NOTIFICATION, NULL, pycon);
  
  return pycon;
}


#endif
