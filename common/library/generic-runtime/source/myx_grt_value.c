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

/**
 * @file  myx_grt_value.c
 * @brief GRT value handling 
 * 
 * See also: <a href="../grt.html#GRTValue">GRT Value</a>
 */

typedef const char * (*dict_identification_func)(MYX_GRT_VALUE *dict);

static int myx_grt_generic_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *list, 
                                      MYX_GRT_VALUE * path, MYX_GRT_VALUE *source, 
                                      MYX_GRT_VALUE *target, dict_identification_func dict_id_func);

/*
 ****************************************************************************
 * @brief Frees a list struct
 *
 *  Frees a list value and all of its contents.
 *****************************************************************************/
int myx_grt_free_list(MYX_GRT_LIST *list)
{
  unsigned int i;
  for (i= 0; i < list->items_num; i++)
  {
    myx_grt_value_release(list->items[i]);
  }
  g_free(list->content_struct_name);
  g_free(list->items);
  g_free(list);
  
  return 0;
}

/*
 ****************************************************************************
 * @brief Frees a dictionary struct
 *
 * Frees a dictionary value and all of its contents.
 *****************************************************************************/
int myx_grt_free_dict(MYX_GRT_DICT *dict)
{
  unsigned int i;

  for (i= 0; i < dict->items_num; i++)
  {
    g_free(dict->items[i].key);
    myx_grt_value_release(dict->items[i].value);
  }
  g_free(dict->object_path);
  g_free(dict->struct_name);
  g_free(dict->items);
  g_free(dict);
  
  return 0;
}

/*
 ****************************************************************************
 * @brief Calls a bridge function
 *
 * Calls a bridge function given by module, name and value. The function returns
 * a status. 1 means that the object still exists and 0 means that the object has
 * been deleted.
 * 
 * The bridge function that gets called returns a MYX_GRT_VALUE int value that 
 * holds the bridge function result. 
 * NULL or 0 ... success
 * 1 ........... error
 * 2 ........... the object has already been deleted
 *
 * The following functions have to be implemented by the bridge
 *
 * _initDict(dict)  used to add bride_data to a dict based on it's struct_name. 
 *                  This may trigger the creation of a new real world object (e.g.
 *                  forms.Form
 * 
 *
 * _updateToGrt(dict || list)  used to check if the real world object assoziated with 
 *                  the dict still exists and to update the GRT list/dict to be
 *                  in sync with the real world list/dict
 *
 * _updateFromGrt(dict || list)  used to check if the real world object assoziated with 
 *                  the dict still exists and to update the real world list/dict to
 *                  be in sync with the GRT list/dict
 *
 *
 * _valueToGrt(any) used to update simple values according the the current value of 
 *                  the real world object member value
 *
 * _valueFromGrt(any)  used to update the real world object member value from the 
 *                  GRT value
 *
 *
 * _delValue(any)  used to delete the real world value
 *
 * @param module the bridge module
 * @param func_name the name of the bridge function
 * @param the value managed by the bridge
 *
 * @return the value status, 1 for a valid value, 0 for a deleted value
 *****************************************************************************/
int myx_grt_bridge_func(MYX_GRT_MODULE *module, const char *func_name, MYX_GRT_VALUE *value)
{
  MYX_GRT *grt= module->loader->grt;
  MYX_GRT_FUNCTION *func= NULL;

  // find function by name
  if (value->extended)
    func= myx_grt_module_function_get(myx_grt_value_bridge_module_get(value), func_name);

  if (func)
  {
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE *res= myx_grt_function_call(grt, func, value, &error);

    if (error != MYX_GRT_NO_ERROR)
    {
      char *error_msg= g_strdup_printf("The function %s of the %s bridge failed with an error %d.", 
        func_name, module->name, error);

      myx_grt_messages_stack_add(grt, 1, error_msg, NULL, 0, 0);

      g_free(error_msg);

      if (res)
        myx_grt_value_release(res);

      return 0;
    }

    if (res)
    {
      int res_val= myx_grt_value_as_int(res);

      myx_grt_value_release(res);
        
      // error
      if (res_val == 1)
      {
        char *message= g_strdup_printf("The function %s of the %s bridge returned an error.", func_name, module->name);

        myx_grt_messages_stack_add(grt, 1, message, NULL, 0, 0);
        g_free(message);

        return 0;
      }
      // object has been deleted
      else if (res_val == 2)
      {
        return 0;
      }
    }

    return 1;
  }
  else
  {
    char *error_msg= g_strdup_printf("The bridge does not implement the function %s.", func_name);

    if (value->extended)
      value->extended->bridge_module= NULL;

    myx_grt_messages_stack_add(grt, 1, error_msg, NULL, 0, 0);
    g_free(error_msg);

    return 0;
  }
}



/**
 ****************************************************************************
 * @brief Assigns a MYX_GRT_STRUCT to a dict value
 *
 *  This will assign a MYX_GRT_STRUCT to a list value. 
 *
 * @param dict a dict value
 * @param gstruct the name struct to assign or NULL
 *
 * @return 0 if ok, -1 if the parameters are invalid.
 *****************************************************************************/
int myx_grt_dict_struct_set_name(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name)
{
  return myx_grt_bridge_dict_struct_set_name(grt, dict, struct_name, 1, NULL);
}


/**
 ****************************************************************************
 * @brief Assigns a MYX_GRT_STRUCT to a dict value
 *
 *  This will assign a MYX_GRT_STRUCT to a list value. 
 *
 * @param dict a dict value
 * @param gstruct the name struct to assign or NULL
 *
 * @return 0 if ok, -1 if the parameters are invalid.
 *****************************************************************************/
int myx_grt_dict_struct_set_name_no_cache_register(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name)
{
  return myx_grt_bridge_dict_struct_set_name(grt, dict, struct_name, 0, NULL);
}


/**
 ****************************************************************************
 * @brief Assigns a MYX_GRT_STRUCT to a list value
 *
 *  This will assign a MYX_GRT_STRUCT to a list value. The list should then
 * only accept values that match the given struct.
 * 
 * @param list a list value containing dictionaries
 * @param struct_name the name struct to assign or NULL. 
 *
 * @return 0 if ok, -1 if the parameters are invalid.
 *****************************************************************************/
int myx_grt_list_content_set_struct_name(MYX_GRT_VALUE *list, const char *struct_name)
{
  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);

  if (list->value.l->content_struct_name)
    g_free(list->value.l->content_struct_name);
  list->value.l->content_struct_name= g_strdup(struct_name);

  return 0;
}


/**
 ****************************************************************************
 * @brief Returns the struct name associated with the value
 * 
 *    This will return the name of the MYX_GRT_STRUCT associated with the 
 *  dictionary or list which was previously set with 
 *  \c myx_grt_dict_struct_set_name or \c myx_grt_list_content_set_struct_name.
 *
 * @param dict a dict value
 *
 * @return A reference to the struct name or NULL if there's nothing associated.
 *****************************************************************************/
const char *myx_grt_dict_struct_get_name(MYX_GRT_VALUE *dict)
{
  g_return_val_if_fail(dict != NULL, NULL);
  if (!(dict->type == MYX_DICT_VALUE))
    g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);
  
  return dict->value.d->struct_name;
}

/**
 ****************************************************************************
 * @brief Returns the struct name associated with the value
 * 
 *    This will return the name of the MYX_GRT_STRUCT associated with the 
 *  dictionary or list which was previously set with 
 *  \c myx_grt_dict_struct_set_name or \c myx_grt_list_content_set_struct_name.
 *
 * @param grt the GRT environment
 *
 * @param dict a dict value
 *
 * @return A reference to the struct name or NULL if there's nothing associated.
 *****************************************************************************/
MYX_GRT_STRUCT * myx_grt_dict_struct_get(MYX_GRT *grt, MYX_GRT_VALUE *dict)
{
  const char *name;
  g_return_val_if_fail(dict != NULL, NULL);

  name= myx_grt_dict_struct_get_name(dict);
  return name ? myx_grt_struct_get(grt, name) : NULL;
}

/**
 ****************************************************************************
 * @brief Checks if the given dict's struct inherits from a parent struct
 *
 * @param grt the GRT environment
 *
 * @param dict a dict value
 *
 * @param parent_struct_name name of the parent struct
 *
 * @return 1 is the dict's struct inherits from the parent struct, 0 if not
 *****************************************************************************/
MYX_PUBLIC_FUNC int myx_grt_dict_struct_inherits_from(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *parent_struct_name)
{
  return myx_grt_struct_inherits_from(grt, myx_grt_dict_struct_get_name(dict), parent_struct_name);
}


MYX_PUBLIC_FUNC int myx_grt_dict_struct_is_or_inherits_from(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *parent_struct_name)
{
  const char *my_struct= myx_grt_dict_struct_get_name(dict);
  
  return strcmp(my_struct, parent_struct_name)==0 || myx_grt_struct_inherits_from(grt, my_struct, parent_struct_name);
}

static void print_value(MYX_GRT *grt, MYX_GRT_VALUE *value, int depth)
{
  unsigned int i;
  unsigned int count;
  char spaces[1024];
  char buffer[200];

  g_return_if_fail(depth < (int)sizeof(spaces));

  if (!value)
  {
    MYX_PRINT(grt, "NULL"NL);
    return;
  }

  switch (value->type)
  {
  case MYX_INT_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%i"NL, myx_grt_value_as_int(value));
    MYX_PRINT(grt, buffer);
    break;
  case MYX_REAL_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%f"NL, myx_grt_value_as_real(value));
    MYX_PRINT(grt, buffer);
    break;
  case MYX_STRING_VALUE:
    /*MYX_PRINT(grt, value->value.s);
    MYX_PRINT(grt, NL);*/
    g_snprintf(buffer, sizeof(buffer), "\"%s\""NL, myx_grt_value_as_string(value));
    MYX_PRINT(grt, buffer);
    break;
  case MYX_LIST_VALUE:
    MYX_PRINT(grt, "["NL);
    count= myx_grt_list_item_count(value);
    for (i= 0; i < count; i++)
    {
      MYX_GRT_VALUE* Entry = myx_grt_list_item_get(value, i);
      g_snprintf(spaces, sizeof(spaces), "%*s", depth, "");
      MYX_PRINT(grt, spaces);
      print_value(grt, Entry, depth + 1);
    }
    g_snprintf(spaces, sizeof(spaces), "%*s", depth, "");
    MYX_PRINT(grt, spaces);
    MYX_PRINT(grt, "]"NL);
    break;
  case MYX_DICT_VALUE:
    MYX_PRINT(grt, "{"NL);
    count= myx_grt_dict_item_count(value);
    for (i= 0; i < count; i++)
    {
      g_snprintf(spaces, sizeof(spaces), "%*s", depth+1, "");
      MYX_PRINT(grt, spaces);
      myx_grt_printf(grt, "%s = ", myx_grt_dict_item_key_by_index(value, i));
      print_value(grt, myx_grt_dict_item_value_by_index(value, i), depth+1);
    }
    g_snprintf(spaces, sizeof(spaces), "%*s", depth, "");
    MYX_PRINT(grt, spaces);
    MYX_PRINT(grt, "}"NL);
    break;
  default:
    MYX_PRINT(grt, "Invalid value"NL);
    break;
  }
}

/**
 ****************************************************************************
 * @brief Prints the contents of a value to stdout
 *
 * @param grt the GRT environment
 * @param value the value to be printed
 *
 * @return 0
 *****************************************************************************/
int myx_grt_value_print(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  print_value(grt, value, 0);
  return 0;
}


static char * value_as_lua_code(MYX_GRT_VALUE *value, char *code, int depth)
{
  unsigned int i;
  unsigned int count;
  char spaces[1024];
  char buffer[200];

  if (!(depth < (int)sizeof(spaces)))
    return code;

  if (!value)
  {
    return code;
  }

  switch (value->type)
  {    
  case MYX_INT_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%i", myx_grt_value_as_int(value));

    code= str_g_append(code, buffer);
    break;
  case MYX_REAL_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%f", myx_grt_value_as_real(value));
    
    code= str_g_append(code, buffer);
    break;
  case MYX_STRING_VALUE:
    g_snprintf(buffer, sizeof(buffer), "\"%s\"", myx_grt_value_as_string(value));

    code= str_g_append(code, buffer);
    break;
  case MYX_LIST_VALUE:
    code= str_g_append(code, "{"NL);
    count= myx_grt_list_item_count(value);

    for (i= 0; i < count; i++)
    {
      g_snprintf(spaces, sizeof(spaces), "%*s", depth * 2, "");
      code= str_g_append(code, spaces);

      code= value_as_lua_code(myx_grt_list_item_get(value, i), code, depth+1);

      if (i < count - 1)
        code= str_g_append(code, ", " NL);
      else
        code= str_g_append(code, NL);
    }
    g_snprintf(spaces, sizeof(spaces), "%*s", depth * 2, "");
    code= str_g_append(code, spaces);

    code= str_g_append(code, "}");
    break;
  case MYX_DICT_VALUE:
    count= myx_grt_dict_item_count(value);
    code= str_g_append(code, "{"NL);    

    for (i= 0; i < count; i++)
    {
      g_snprintf(spaces, sizeof(spaces), "%*s", depth * 2, "");
      code= str_g_append(code, spaces);

      g_snprintf(buffer, sizeof(buffer), "%s = ", myx_grt_dict_item_key_by_index(value, i));
      code= str_g_append(code, buffer);

      code= value_as_lua_code(myx_grt_dict_item_value_by_index(value, i), code, depth+1);

      if (i < count - 1)
        code= str_g_append(code, ", " NL);
      else
        code= str_g_append(code, NL);
    }
    if (depth > 1)
    {
      g_snprintf(spaces, sizeof(spaces), "%*s", abs(depth - 1) * 2, "");
      code= str_g_append(code, spaces);
    }

    code= str_g_append(code, "}");
    break;
  default:
    break;
  }

  return code;
}

/**
 ****************************************************************************
 * @brief Returns the value formated as Lua code
 *
 * @param grt the GRT environment
 * @param value the value to be formated
 *
 * @return a string containing Lua code that has to be freed
 *****************************************************************************/
char * myx_grt_value_as_lua_code(MYX_GRT_VALUE *value, int depth)
{
  char *code= NULL;

  return value_as_lua_code(value, code, depth);
}

/**
 ****************************************************************************
 * @brief Decrements the reference count of the value
 * 
 *   Will decrease the reference count of the value by one. When it reaches 0
 *   it will recursively release (and free) the whole contents of the value.
 *
 * @param value the value to be released
 *
 * @return 0
 * 
 * @see myx_grt_value_retain
 *****************************************************************************/
int myx_grt_value_release(MYX_GRT_VALUE *value)
{
  //g_return_val_if_fail(value != NULL, -1);
  if (!value)
    return 0;

  g_return_val_if_fail(value->refcount > 0, -1);

  value->refcount--;
  if (value->refcount == 0)
  {
    // First let listeners know.
    myx_grt_value_listener_call(value, MYX_GVCR_DELETE);

    // Remove listeners
    if (value->extended)
    {
      if (value->extended->listeners)
      {
        unsigned int i;

        for (i= 0; i < value->extended->listeners_num; i++)
          g_free(value->extended->listeners[i]);

        g_free(value->extended->listeners);
      }

      // Finally let the bridge do whatever it needs if the value is managed by a bridge.
      if (myx_grt_value_is_bridged(value))
        myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_delValue", value);

      g_free(value->extended);
    }
    
    switch (value->type)
    {
    case MYX_ANY_VALUE:
      break;
    case MYX_INT_VALUE:
    case MYX_REAL_VALUE:
      break;
    case MYX_STRING_VALUE:
      g_free(value->value.s);
      break;
    case MYX_LIST_VALUE:
      myx_grt_free_list(value->value.l);
      break;
    case MYX_DICT_VALUE:
      myx_grt_free_dict(value->value.d);
      break;
    }

    g_free(value);
  }

  return 0;
}

/**
 ****************************************************************************
 * @brief Returns the current reference count of the value
 * 
 *   Will return the current reference count of the value.
 *
 * @param value the value to be queried
 *
 * @return the currente reference count
 * 
 * @see myx_grt_value_retain
 *****************************************************************************/
int myx_grt_value_get_current_reference_count(MYX_GRT_VALUE *value)
{
  return value->refcount;
}


/** 
 ****************************************************************************
 * @brief Increments the reference count of the value.
 *
 *  Increments the reference count of the value by one. Newly created values
 * start with a reference count of 1, so you normally don't need to retain them.
 * Proxy objects cannot have their reference count increased
 *
 * @param value - GRT value
 *
 * @return the GRT value passed as argument
 *
 * @see myx_grt_value_release
 ****************************************************************************
 */
MYX_GRT_VALUE *myx_grt_value_retain(MYX_GRT_VALUE *value)
{
  //g_return_val_if_fail(value != NULL, value);

  if (value)
    value->refcount++;

  return value;
}


/**
 ****************************************************************************
 * @brief create a dict value from a list of key/type/value tuples.
 *
 *   If value is a list of dict, you must pass a MYX_GRT_LIST or
 *   MYX_GRT_DICT. These must not be freed. But usually you will give this
 *   function a MYX_GRT_VALUE containing a list or dict. In that case 
 *   the value will be just retained, so you can release it later.
 *
 * @param  key the dict key for the value or NULL, followed by vtype and value\n
 *    vtype type of the value may be 0 for a MYX_GRT_VALUE\n
 *    value the value\n
 * Repeat these as many times as needed. End list of values with NULL
 *
 * @return  A GRT value of type dict.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_dict_create(MYX_GRT *grt, const char *struct_name, const char *key, ...)
{
  va_list args;
  MYX_GRT_VALUE *dict;
  MYX_GRT_VALUE *gvalue;
  MYX_GRT_VALUE_TYPE vtype;
  
  g_return_val_if_fail(key != NULL, NULL);

  dict= myx_grt_dict_new(grt, struct_name);

  va_start(args, key);

  while (key != NULL)
  {
    vtype= va_arg(args, MYX_GRT_VALUE_TYPE);
    
    switch (vtype)
    {
    case MYX_INT_VALUE:
      gvalue= myx_grt_value_from_int(va_arg(args, int));
      break;
    case MYX_REAL_VALUE:
      gvalue= myx_grt_value_from_real(va_arg(args, double));
      break;
    case MYX_STRING_VALUE:
      gvalue= myx_grt_value_from_string(va_arg(args, char*));
      break;
    case MYX_LIST_VALUE:
      gvalue= g_new0(MYX_GRT_VALUE, 1);
      gvalue->type= MYX_LIST_VALUE;
      gvalue->value.l= va_arg(args, MYX_GRT_LIST*);
      gvalue->refcount= 1;
      break;
    case MYX_DICT_VALUE:
      gvalue= g_new0(MYX_GRT_VALUE, 1);
      gvalue->type= MYX_DICT_VALUE;
      gvalue->value.d= va_arg(args, MYX_GRT_DICT*);
      gvalue->refcount= 1;
      break;
    default:
      gvalue= va_arg(args, MYX_GRT_VALUE*);
      myx_grt_value_retain(gvalue);
    }

    myx_grt_dict_item_set_value(dict, key, gvalue);
    
    myx_grt_value_release(gvalue);
    
    key= va_arg(args, const char*);
  }
  
  va_end(args);
  
  return dict;
}

MYX_GRT_VALUE_TYPE myx_grt_dict_content_get_type(MYX_GRT_VALUE *dict)
{
  return dict->value.d->content_type;
}

void myx_grt_dict_content_set_type(MYX_GRT_VALUE *dict, MYX_GRT_VALUE_TYPE content_type)
{
  dict->value.d->content_type= content_type;
}

void myx_grt_dict_content_set_struct_name(MYX_GRT_VALUE *dict, const char *struct_name)
{
  dict->value.d->content_struct_name= g_strdup(struct_name);
}

const char * myx_grt_dict_content_get_struct_name(MYX_GRT_VALUE *dict)
{
  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(myx_grt_value_get_type(dict) == MYX_DICT_VALUE, NULL);

  return dict->value.d->content_struct_name;
}

const char * myx_grt_dict_get_object_path(MYX_GRT_VALUE *dict)
{
  g_return_val_if_fail(dict != NULL, NULL);

  return dict->value.d->object_path;
}


/**
 ****************************************************************************
 * @brief Creates a GRT value of MYX_INT_VALUE type
 *
 *   Creates a GRT value of type MYX_INT_VALUE and initialized it with the 
 * passed argument.
 * 
 * @param i the integer value that it will be initialized to.
 *
 * @return A newly created value struct.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_value_from_int(int i)
{
  MYX_GRT_VALUE *value;
  value= g_new0(MYX_GRT_VALUE, 1);
  value->type= MYX_INT_VALUE;
  value->value.i= i;
  value->refcount= 1;
  return value;
}


/**
 ****************************************************************************
 * @brief creates a GRT value of MYX_REAL_VALUE type
 *
 *   Creates a GRT value of type MYX_REAL_VALUE and initialized it with the 
 * passed argument.
 *
 * @param d the value that it will be initialized to.
 *
 * @return A newly created value struct.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_value_from_real(double d)
{
  MYX_GRT_VALUE *value= g_new0(MYX_GRT_VALUE, 1);
  value->type= MYX_REAL_VALUE;
  value->value.r= d;
  value->refcount= 1;
  return value;
}

/**
 ****************************************************************************
 * @brief Creates a GRT value of MYX_STRING_VALUE type
 * 
 *   Creates a GRT value of type MYX_STRING_VALUE and initialized it with the 
 * passed argument. The string will be copied.
 *
 * @param s the value that it will be initialized to.
 * 
 * @return A newly created value struct.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_value_from_string(const char *s)
{
  MYX_GRT_VALUE *value= NULL;
  
  if (s)
  {
    value= g_new0(MYX_GRT_VALUE, 1);
    value->type= MYX_STRING_VALUE;
    value->value.s= g_strdup(s);
    value->refcount= 1;
  }

  return value;
}

/**
 ****************************************************************************
 * @brief Changes a GRT value of MYX_INT_VALUE type
 * 
 * @param value the value to change
 * @param i the new integer.
 *
 * @return the original GRT value with the new integer
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_bridge_value_change_int(MYX_GRT_VALUE *value, int i)
{
  g_return_val_if_fail(value!=NULL, NULL);
  g_return_val_if_fail(myx_grt_value_get_type(value) == MYX_INT_VALUE, NULL);

  value->value.i= i;

  return value;
}

/**
 ****************************************************************************
 * @brief Changes a GRT value of MYX_REAL_VALUE type
 * 
 * @param value the value to change
 * @param d the new number.
 *
 * @return the original GRT value with the new number
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_bridge_value_change_real(MYX_GRT_VALUE *value, double d)
{
  g_return_val_if_fail(value!=NULL, NULL);
  g_return_val_if_fail(myx_grt_value_get_type(value) == MYX_REAL_VALUE, NULL);

  value->value.r= d;

  return value;
}

/**
 ****************************************************************************
 * @brief Changes a GRT value of MYX_STRING_VALUE type
 * 
 * @param value the value to change
 * @param s the new string. The string will be copied.
 *
 * @return the original GRT value with the new string
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_bridge_value_change_string(MYX_GRT_VALUE *value, const char *s)
{
  g_return_val_if_fail(value!=NULL, NULL);
  g_return_val_if_fail(myx_grt_value_get_type(value) == MYX_STRING_VALUE, NULL);

  if (value->value.s)
    g_free(value->value.s);

  value->value.s= g_strdup(s);

  return value;
}

static MYX_GRT_VALUE * myx_grt_value_dup_bridge_settings(MYX_GRT_VALUE *src_value, MYX_GRT_VALUE *dest_value)
{
  if (myx_grt_value_is_bridged(src_value))
  {
    myx_grt_value_extend(dest_value);

    dest_value->extended->bridge_module= src_value->extended->bridge_module;
    dest_value->extended->bridge_data_object= src_value->extended->bridge_data_object;
    dest_value->extended->bridge_data_owner= src_value->extended->bridge_data_owner;
    if (dest_value->extended->bridge_dict_key)
      g_free(dest_value->extended->bridge_dict_key);

    if (src_value->extended->bridge_dict_key)
      dest_value->extended->bridge_dict_key= g_strdup(src_value->extended->bridge_dict_key);
    else
      dest_value->extended->bridge_dict_key= NULL;

    dest_value->extended->bridge_list_index= src_value->extended->bridge_list_index;
  }

  return dest_value;
}

/**
 ****************************************************************************
 * @brief Creates a copy of the original value
 * 
 *   Creates a GRT value of type MYX_STRING_VALUE and initialized it with the 
 * passed argument. The string will be copied.
 *
 * @param s the value that it will be initialized to.
 * 
 * @return A newly created value struct.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_value_dup(MYX_GRT_VALUE *value)
{
  if (value != NULL)
  {
    switch (myx_grt_value_get_type(value))
    {
    case MYX_ANY_VALUE:
      break;
    case MYX_INT_VALUE:
      return myx_grt_value_dup_bridge_settings(value, myx_grt_value_from_int(myx_grt_value_as_int(value)));
    case MYX_REAL_VALUE:
      return myx_grt_value_dup_bridge_settings(value, myx_grt_value_from_real(myx_grt_value_as_real(value)));
    case MYX_STRING_VALUE:
      return myx_grt_value_dup_bridge_settings(value, myx_grt_value_from_string(myx_grt_value_as_string(value)));
    case MYX_LIST_VALUE:
      {
        MYX_GRT_VALUE *list= myx_grt_list_new(myx_grt_list_content_get_type(value),  
          myx_grt_list_content_get_struct_name(value));
        unsigned int i;

        myx_grt_value_dup_bridge_settings(value, list);

        //Copy items
        for (i= 0; i < myx_grt_list_item_count(value); i++)
        {
          MYX_GRT_VALUE *item= myx_grt_value_dup(myx_grt_list_item_get(value, i));
          myx_grt_list_item_add(list, item);

          //release item one time, so it is only refered by the list
          myx_grt_value_release(item);
        }

        return list;
      }
    case MYX_DICT_VALUE:
      {
        MYX_GRT_VALUE *dict;
        unsigned int i;

        //Check if we have a typed dict
        if (myx_grt_dict_content_get_type(value) != MYX_ANY_VALUE)
          dict= myx_grt_dict_new_typed(myx_grt_dict_content_get_type(value),  
            myx_grt_dict_content_get_struct_name(value));
        else
        {
          dict= myx_grt_dict_new(NULL, myx_grt_dict_struct_get_name(value));

          myx_grt_value_dup_bridge_settings(value, dict);
        }

        //Copy items
        for (i= 0; i<myx_grt_dict_item_count(value); i++)
        {
          MYX_GRT_VALUE *item= myx_grt_value_dup(myx_grt_dict_item_value_by_index(value, i));
          myx_grt_dict_item_set_value(dict, 
            myx_grt_dict_item_key_by_index(value, i), item);

          //release item one time, so it is only refered by the dict
          myx_grt_value_release(item);
        }

        return dict;
      }
    }
  }

  return NULL;
}

/**
 ****************************************************************************
 * @brief Creates a new empty GRT value of MYX_GRT_DICT type
 *
 *   Creates a GRT value of type MYX_GRT_DICT, which is a dictionary or
 * mapping of keys strings to values. You can add, query and remove values
 * to it with the other dict functions.
 *
 * To type the dictionary, specify a content_type. If the content_type is dict
 * you can define the struct that these dicts must implement with the 
 * content_struct_name parameter
 * 
 * @return  A newly created dict value struct.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_new_typed(MYX_GRT_VALUE_TYPE content_type, const char *content_struct_name)
{
  MYX_GRT_VALUE *value= g_new0(MYX_GRT_VALUE, 1);
  value->type= MYX_DICT_VALUE;
  value->value.d= g_new0(MYX_GRT_DICT, 1);
  value->refcount= 1;

  value->value.d->content_type= content_type;

  if ((content_struct_name) && (content_struct_name[0]))
    value->value.d->content_struct_name= g_strdup(content_struct_name);

  return value;
}

/**
 ****************************************************************************
 * @brief Creates a new empty GRT value of MYX_GRT_DICT type
 *
 *   Creates a GRT value of type MYX_GRT_DICT, which is a dictionary or
 * mapping of keys strings to values. You can add, query and remove values
 * to it with the other dict functions.
 *
 * If the struct_name is set, this dict is an instance of the given struct
 *
 * @param struct_name the struct name the object is an instance of
 * 
 * @return  A newly created dict value struct.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_new(MYX_GRT *grt, const char *struct_name)
{
  return myx_grt_bridge_dict_new(grt, struct_name, NULL);
}

/**
 ****************************************************************************
 * @brief Adds a new listener to the GRT value
 *
 *   Creates a new MYX_GRT_VALUE_LISTENER and adds it to the list of listeners
 * for this GRT value. If the listener list has not been allocated yet
 * it will be allocated.
 *
 * @param grt pointer to the grt environment
 * @param value the value the listener should be added
 * @param user_data pointer that can be set by the caller to store userdata
 * @param listener_callback pointer to the callback function
 *****************************************************************************/
void myx_grt_value_listener_add(MYX_GRT *grt, MYX_GRT_VALUE *value, void *user_data, 
                                MYX_GRT_VALUE_LISTENER_CALLBACK listener_callback)
{
  unsigned int i;
  gboolean foundListener = FALSE;

  if (myx_grt_value_has_listeners(value))
  {
    for (i= 0; i < value->extended->listeners_num; i++)
    {
      MYX_GRT_VALUE_LISTENER *listener= value->extended->listeners[i];
      if (listener->function == listener_callback)
      {
        foundListener = TRUE;
        break;
      };
    };
  };

  // Add listener only if it has not already been done.
  if (!foundListener)
  {
    MYX_GRT_VALUE_LISTENER *listener= g_new0(MYX_GRT_VALUE_LISTENER, 1);

    listener->listeners_grt = grt;
    listener->function= listener_callback;
    listener->user_data= user_data;

    myx_grt_value_extend(value);

    value->extended->listeners_num++;
    value->extended->listeners= g_realloc(value->extended->listeners, 
      sizeof(MYX_GRT_VALUE_LISTENER *) * value->extended->listeners_num);

    value->extended->listeners[value->extended->listeners_num - 1]= listener;
  };
}

/**
 ****************************************************************************
 * @brief Removes the given listener from the GRT value
 *
 *   Removes MYX_GRT_VALUE_LISTENER from the given value.
 *
 * @param value the value the listener should be removed from
 * @param listener_callback pointer to the callback function
 *****************************************************************************/
void myx_grt_value_listener_remove(MYX_GRT_VALUE *value, void *user_data, MYX_GRT_VALUE_LISTENER_CALLBACK listener_callback_remove)
{
  if (myx_grt_value_has_listeners(value))
  {
    unsigned int i;

    for (i= 0; i < value->extended->listeners_num; i++)
    {
      MYX_GRT_VALUE_LISTENER *listener= value->extended->listeners[i];
      if ((listener->function == listener_callback_remove) && (listener->user_data == user_data))
        break;
    };

    if (i < value->extended->listeners_num)
    {
      // Found the listener.
      g_free(value->extended->listeners[i]);
      --value->extended->listeners_num;
      if (i < value->extended->listeners_num)
        memmove(value->extended->listeners + i, value->extended->listeners+ i  + 1,
                sizeof(MYX_GRT_VALUE_LISTENER*)*(value->extended->listeners_num-i));
    };
  }
}

MYX_PUBLIC_FUNC int myx_grt_value_listener_get(MYX_GRT *grt, MYX_GRT_VALUE *value, void *user_data, 
                                                int (*listener_callback_get)(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *user_data))
{
  if (myx_grt_value_has_listeners(value))
  {
    unsigned int i;

    for (i= 0; i < value->extended->listeners_num; i++)
    {
      MYX_GRT_VALUE_LISTENER *listener= value->extended->listeners[i];
      if ((listener->function == listener_callback_get) && (listener->user_data == user_data))
        break;
    };

    if (i < value->extended->listeners_num)
    {
      // Found the listener.
      return 1;
    };
  }

  return 0;
}


/**
 ****************************************************************************
 * @brief Calls back to all registered listeners
 *
 *   Calls back to all registered listeners of the value with the given reason
 *
 * @param value the value which listener should be called
 * @param reason the reason to call the listeners
 *****************************************************************************/
int myx_grt_value_listener_call(MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason)
{
  int res= 0;

  if (myx_grt_value_has_listeners(value))
  {
    unsigned int i;

    for (i= 0; i < value->extended->listeners_num; i++)
    {
      int func_res;

      MYX_GRT_VALUE_LISTENER *listener= value->extended->listeners[i];

      func_res= listener->function(listener->listeners_grt, value, reason, listener->user_data);

      if ((res == 0) && (func_res != 0))
        res= func_res;
    }
  }

  return res;
}


/**
 ****************************************************************************
 * @brief Initialize an empty GRT dict value with its child values defined 
 * by the struct
 *
 *   All direct child values defined by the given struct are also created.
 *
 * @param grt pointer to the GRT environment
 * @param dict a pointer to the empty dict
 * @param name the name of the object
 * @param _id the _id of the object
 * @param owner the owner _id of the object
 * 
 * @return  A newly created dict value struct.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_init_obj(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *name, const char *_id, 
                                      const char *owner)
{
  MYX_GRT_STRUCT *gstruct= myx_grt_struct_get(grt, myx_grt_dict_struct_get_name(dict));
  unsigned int i;
  unsigned int count= myx_grt_struct_get_member_count_total(grt, gstruct);

  // create child values
  for (i= 0; i < count; i++)
  {
    MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index_total(grt, gstruct, i);
    MYX_GRT_VALUE_TYPE member_type= myx_grt_struct_member_get_type(member);
    const char *member_name= myx_grt_struct_get_member_name(member);
    const char *member_default= myx_grt_struct_get_member_default(member);

    if (member_type == MYX_STRING_VALUE)
    {
      MYX_GRT_VALUE *child_value;
      
      if (strcmp2(member_name, "name") == 0)
        child_value= myx_grt_value_from_string(name);
      else if (strcmp2(member_name, "_id") == 0)
      {
        if (_id && _id[0])
          child_value= myx_grt_value_from_string(_id);
        else
        {
          // if the _id is not set yet
          if (myx_grt_dict_item_get_value(dict, "_id") == NULL)
          {
            char *guid= myx_grt_get_guid();

            child_value= myx_grt_value_from_string(guid);

            g_free(guid);
          }
          // if the _id was already set (e.g. by a bridge, keep existing value)
          else
            continue;
        }
      }
      else if (strcmp2(member_name, "owner") == 0)
      {
        if (owner && owner[0])
          child_value= myx_grt_value_from_string(owner);
        else
          child_value= NULL;
      }
      else if (member_default)
        child_value= myx_grt_value_from_string(member_default);
      else
        child_value= myx_grt_value_from_string("");

      /*if (myx_grt_struct_member_get_is_ref(member))
      {
        const char *content_struct= myx_grt_struct_member_get_content_struct_name(member);

        myx_grt_dict_struct_set_name(child_value, content_struct);
      }*/

      myx_grt_dict_item_set_value(dict, member_name, child_value);
    }
    else if (member_type == MYX_INT_VALUE)
    {
      if (member_default && member_default[0])
        myx_grt_dict_item_set_value_from_int(dict, member_name, atoi(member_default));
      else
        myx_grt_dict_item_set_value_from_int(dict, member_name, 0);
    }
    else if (member_type == MYX_REAL_VALUE)
    {
      if (member_default && member_default[0])
        myx_grt_dict_item_set_value_from_real(dict, member_name, atof(member_default));
      else
        myx_grt_dict_item_set_value_from_real(dict, member_name, 0.0);
    }
    else if (member_type == MYX_LIST_VALUE)
    {
      MYX_GRT_VALUE_TYPE content_type= myx_grt_struct_member_get_content_type(member);
      const char *content_struct= myx_grt_struct_member_get_content_struct_name(member);
      MYX_GRT_VALUE *child_value= myx_grt_list_new(content_type, content_struct);

      myx_grt_dict_item_set_value(dict, member_name, child_value);
    }
    else if (member_type == MYX_DICT_VALUE)
    {
      MYX_GRT_VALUE_TYPE content_type= myx_grt_struct_member_get_content_type(member);
      const char *content_struct= myx_grt_struct_member_get_content_struct_name(member);
      MYX_GRT_VALUE *child_value= NULL;

      if (content_type != MYX_ANY_VALUE)
        child_value= myx_grt_dict_new_typed(content_type, content_struct);

      myx_grt_dict_item_set_value(dict, member_name, child_value);

      /*const char *struct_name= myx_grt_struct_member_get_struct_name(member);
      MYX_GRT_VALUE *child_value;

      if (struct_name)
        child_value= myx_grt_dict_new(struct_name);
      else
      {
        MYX_GRT_VALUE_TYPE content_type= myx_grt_struct_member_get_content_type(member);
        const char *content_struct= myx_grt_struct_member_get_content_struct_name(member);

        child_value= myx_grt_dict_new_typed(content_type, content_struct);
      }*/
    }
  }

  return dict;
}

/**
 ****************************************************************************
 * @brief Creates a new empty GRT value of MYX_GRT_DICT type and initialize
 * its child values defined by the struct
 *
 *   Creates a GRT value of type MYX_GRT_DICT, which is a dictionary or
 * mapping of keys strings to values. All direct child values defined by the 
 * given struct are also created.
 *
 * @param grt pointer to the GRT environment
 * @param struct_name the name of the struct
 * @param name the name of the object
 * @param _id the _id of the object
 * @param owner the owner _id of the object
 * 
 * @return  A newly created dict value struct.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_new_obj(MYX_GRT *grt, const char *struct_name, 
                                     const char *name, const char *_id, const char *owner)
{
  MYX_GRT_VALUE *dict= myx_grt_dict_new(grt, struct_name);

  return myx_grt_dict_init_obj(grt, dict, name, _id, owner);
}


static int bisect(MYX_GRT_DICT *dict, const char *key, int *index)
{
  int i;
  int j, k;
  
  j= 0;
  k= dict->items_num-1;
  while (j <= k)
  {
    int r;
    i= (j+k)/2;
    r= strcmp(key, dict->items[i].key);
    
    if (r == 0)
    {
      *index= i;
      return 1;
    }
    else if (r < 0)
      k= i-1;
    else if (r > 0)
      j= i+1;
  }
  
  if (j > k)
    *index= j;
  else
    *index= k;

  return 0;
}

/**
 ****************************************************************************
 * @brief Sets a value for a key in the dict
 * 
 *   Inserts a key value pair into the dict. The value will have its
 * reference count increased, therefore can be released afterwards.
 * If the key already exists in the dictionary, the old one will be released
 * before it's replaced.
 * 
 * \b NOTE
 *   The values are inserted in already order so that a binary search can
 * be used to lookup a specific value.
 * 
 * @param dict  a GRT value of type MYX_GRT_DICT
 * @param key   the key name where the value should be inserted
 * @param value the value to be inserted. The value will have its reference
 *     count increased.
 *
 * @return  0 on success, -1 on error
 *****************************************************************************/
int myx_grt_dict_item_set_value(MYX_GRT_VALUE *dict, const char *key, MYX_GRT_VALUE *value)
{
  return myx_grt_bridge_dict_item_set_value(dict, key, value, TRUE);
}

/**
 ****************************************************************************
 * @brief Sets a string value for a key in the dict
 * 
 *   Creates a new string value and then calls myx_grt_dict_item_set_value
 * 
 * @param dict  a GRT value of type MYX_GRT_DICT
 * @param key   the key name where the value should be inserted
 * @param s     the string to be inserted.
 *
 * @return  0 on success, -1 on error
 *****************************************************************************/
int myx_grt_dict_item_set_value_from_string(MYX_GRT_VALUE *dict, const char *key, const char *s)
{
  return myx_grt_bridge_dict_item_set_value_from_string(dict, key, s, TRUE);
}

/**
 ****************************************************************************
 * @brief Sets a int value for a key in the dict
 * 
 *   Creates a new int value and then calls myx_grt_dict_item_set_value
 * 
 * @param dict  a GRT value of type MYX_GRT_DICT
 * @param key   the key name where the value should be inserted
 * @param i     the int to be inserted.
 *
 * @return  0 on success, -1 on error
 *****************************************************************************/
int myx_grt_dict_item_set_value_from_int(MYX_GRT_VALUE *dict, const char *key, int i)
{
  return myx_grt_bridge_dict_item_set_value_from_int(dict, key, i, TRUE);
}

/**
 ****************************************************************************
 * @brief Sets a real value for a key in the dict
 * 
 *   Creates a new real value and then calls myx_grt_dict_item_set_value
 * 
 * @param dict  a GRT value of type MYX_GRT_DICT
 * @param key   the key name where the value should be inserted
 * @param d     the double to be inserted.
 *
 * @return  0 on success, -1 on error
 *****************************************************************************/
int myx_grt_dict_item_set_value_from_real(MYX_GRT_VALUE *dict, const char *key, double d)
{
  return myx_grt_bridge_dict_item_set_value_from_real(dict, key, d, TRUE);
}

/**
 ****************************************************************************
 * @brief Looks up for the key in the dict and returns the value associated to it.
 *
 *   This will return the value for the specified key.
 * 
 * @param dict a GRT value of type MYX_GRT_DICT
 * @param key the key name to search
 *
 * @return  NULL if the value does not exist in the dict or the value if it does.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_dict_item_get_value(MYX_GRT_VALUE *dict, const char *key)
{
  return myx_grt_bridge_dict_item_get_value(dict, key, 1);
}

/**
 ****************************************************************************
 * @brief Removes the key and it's value from the dict
 *
 *   Remove the value associated with the key in the dict. The value will be 
 *   released.
 *
 * @param dict a GRT value of type MYX_GRT_DICT
 * @param key the key to remove
 * 
 * @return 0 if the value was removed, -1 if the key was not in the dict.
 *****************************************************************************/
int myx_grt_dict_item_del(MYX_GRT_VALUE *dict, const char *key)
{
  return myx_grt_bridge_dict_item_del(dict, key, 1);
}

/**
 ****************************************************************************
 * @brief Returns the items of elements in the dict value
 *
 *   Returns the number of items in the dict. Use in conjunction with
 *  myx_grt_dict_item_by_index() to traverse full contents of the dictionary.
 *
 * @param dict the dict
 *
 * @return Number of items in the dict.
 *****************************************************************************/
unsigned int myx_grt_dict_item_count(MYX_GRT_VALUE *dict)
{
  return myx_grt_bridge_dict_item_count(dict, 1);
}

/**
 ****************************************************************************
 * @brief Get the item at index in the dict
 *
 *  Retrieves the item at the specified index in the dict. If you are
 * traversing the dictionary, you should not modify its contents during
 * that.
 * 
 * @param dict the dict
 * @param index index in the dict to fetch
 * @param retkey pointer to where the key string will be returned
 * @param retvalue pointer to where the return GRT value will be returned
 *
 * @return -1 if there's some error (bad parameters or invalid index), 0 otherwise.
 *****************************************************************************/
int myx_grt_dict_item_by_index(MYX_GRT_VALUE *dict, unsigned int index, 
                          const char **retkey, MYX_GRT_VALUE **retvalue)
{
  g_return_val_if_fail(dict!=NULL, -1);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, -1);

  if (index >= myx_grt_dict_item_count(dict))
    return -1;

  *retkey= dict->value.d->items[index].key;
  *retvalue= dict->value.d->items[index].value;
  
  return 0;
}

/**
 ****************************************************************************
 * @brief Get the item's key name at index in the dict
 *
 *  Retrieves the item's key name at the specified index in the dict. 
 * 
 * @param dict the dict
 * @param index index in the dict to fetch
 *
 * @return NULL if there's some error (bad parameters or invalid index), the string otherwise.
 *****************************************************************************/
const char * myx_grt_dict_item_key_by_index(MYX_GRT_VALUE *dict, unsigned int index)
{
  return myx_grt_bridge_dict_item_key_by_index(dict, index, 1);
}


/**
 ****************************************************************************
 * @brief Get the item's value at index in the dict
 *
 *  Retrieves the item's value at the specified index in the dict. If you are
 * traversing the dictionary, you should not modify its contents during
 * that.
 * 
 * @param dict the dict
 * @param index index in the dict to fetch
 *
 * @return NULL if there's some error (bad parameters or invalid index), the value otherwise.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_item_value_by_index(MYX_GRT_VALUE *dict, unsigned int index)
{
  return myx_grt_bridge_dict_item_value_by_index(dict, index, 1);
}

/**
 ****************************************************************************
 * @brief Returns the complex (dicts, lists) items of elements in the dict value
 *
 *   Returns the number of complex (dicts, lists) items in the dict. Use in 
 *  conjunction with myx_grt_dict_item_key_by_index_complex() and 
 *  myx_grt_dict_item_value_by_index_complex() to traverse full contents of 
 *  the dictionary.
 *
 * @param dict the dict
 *
 * @return Number of items in the dict.
 *****************************************************************************/
unsigned int myx_grt_dict_item_count_complex(MYX_GRT_VALUE *dict)
{
  unsigned int i;
  unsigned int count= 0;

  g_return_val_if_fail(dict!=NULL, 0);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, 0);

  // if the dict is managed by a bridge, update
  if (myx_grt_value_is_bridged(dict))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(dict), "_updateToGrt", dict);

  for (i= 0; i < dict->value.d->items_num; i++)
  {
    MYX_GRT_DICT_ITEM *item= dict->value.d->items + i;
    MYX_GRT_VALUE_TYPE item_type= myx_grt_value_get_type(item->value);

    if ((item_type == MYX_DICT_VALUE) || (item_type == MYX_LIST_VALUE))
      count++;
  }

  return count;
}

/**
 ****************************************************************************
 * @brief Get the item's key name at index in the dict. Only complex (dicts, lists) 
 * values are counted.
 *
 *   Retrieves the item's key name at the specified index in the dict. Only complex 
 * (dicts, lists) values are counted.
 * 
 * @param dict the dict
 * @param index index in the dict to fetch
 *
 * @return NULL if there's some error (bad parameters or invalid index), the string otherwise.
 *****************************************************************************/
const char * myx_grt_dict_item_key_by_index_complex(MYX_GRT_VALUE *dict, unsigned int index)
{
  unsigned int i;
  unsigned int count= 0;
  const char *key= NULL;

  g_return_val_if_fail(dict!=NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  // if the dict is managed by a bridge, update
  /*if (dict->bridge_module)
    myx_grt_bridge_func(dict->bridge_module, "_updateToGrt", dict);*/

  // loop over all items, only look at dicts and lists
  for (i= 0; i < dict->value.d->items_num; i++)
  {
    MYX_GRT_DICT_ITEM *item= dict->value.d->items + i;
    MYX_GRT_VALUE_TYPE item_type= myx_grt_value_get_type(item->value);

    if ((item_type == MYX_DICT_VALUE) || (item_type == MYX_LIST_VALUE))
    {
      count++;

      // if the number of complex items equals the index, return the key
      if (count == index + 1)
      {
        key= dict->value.d->items[i].key;
        break;
      }
    }
  }

  return key;
}

/**
 ****************************************************************************
 * @brief Get the item's value at index in the dict. Only complex (dicts, lists) 
 * values are counted.
 *
 *  Retrieves the item's value at the specified index in the dict. Only complex 
 * (dicts, lists) values are counted.If you are traversing the dictionary, you should 
 * not modify its contents during that.
 * 
 * @param dict the dict
 * @param index index in the dict to fetch
 *
 * @return NULL if there's some error (bad parameters or invalid index), the value otherwise.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_item_value_by_index_complex(MYX_GRT_VALUE *dict, unsigned int index)
{
  unsigned int i;
  unsigned int count= 0;
  MYX_GRT_VALUE *value= NULL;

  g_return_val_if_fail(dict!=NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  // if the dict is managed by a bridge, update
  /*if (dict->bridge_module)
    myx_grt_bridge_func(dict->bridge_module, "_updateToGrt", dict);*/

  // loop over all items, only look at dicts and lists
  for (i= 0; i < dict->value.d->items_num; i++)
  {
    MYX_GRT_DICT_ITEM *item= dict->value.d->items + i;
    MYX_GRT_VALUE_TYPE item_type= myx_grt_value_get_type(item->value);

    if ((item_type == MYX_DICT_VALUE) || (item_type == MYX_LIST_VALUE))
    {
      count++;

      // if the number of complex items equals the index, return the key
      if (count == index + 1)
      {
        value= dict->value.d->items[i].value;
        break;
      }
    }
  }

  // If the value is managed by a bridge, get its value from there.
  if (myx_grt_value_is_bridged(value))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_valueToGrt", value);

  return value;
}

/**
 ****************************************************************************
 * @brief Convenience function to create a list value from a stringlist.
 *
 *   Will create a MYX_GRT_VALUE of type list containing the strings in sl.
 *   All the strings will be copied.
 *
 * @param  sl the stringlist
 *
 * @return  The newly created value or NULL on error.
 *
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_list_create_from_stringlist(MYX_STRINGLIST *sl)
{
  MYX_GRT_VALUE *list;
  unsigned int i;

  g_return_val_if_fail(sl != NULL, NULL);

  list= myx_grt_list_new(MYX_STRING_VALUE, NULL);
  list->value.l->items_num= sl->strings_num;
  list->value.l->items= g_malloc(sizeof(MYX_GRT_VALUE*)*sl->strings_num);
  for (i= 0; i < sl->strings_num; i++)
    list->value.l->items[i]= myx_grt_value_from_string(sl->strings[i]);

  return list;
}

/**
 ****************************************************************************
 * @brief Creates a new empty list GRT value 
 * 
 *   Creates a GRT value of type list. All elements inserted to the list
 * must be of content_type.
 *
 * @param  content_type the type that the contents of the list will have
 * @param  struct_name if content_type is dict and this is set only dicts of the
 *            given struct can be inserted
 *
 * @return  The new list value or NULL if there's an error.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_list_new(MYX_GRT_VALUE_TYPE content_type, const char *struct_name)
{
  MYX_GRT_VALUE *value;
  //g_return_val_if_fail(content_type!=0, NULL);

  value= g_new0(MYX_GRT_VALUE, 1);
  value->type= MYX_LIST_VALUE;
  value->value.l= g_new0(MYX_GRT_LIST, 1);
  value->value.l->content_type= content_type;
  value->refcount= 1;

  if (struct_name && struct_name[0])
    myx_grt_list_content_set_struct_name(value, struct_name);

  return value;
}

/**
 ****************************************************************************
 * @brief Returns the content type of the list
 * 
 *   Every list can only have values of one type or any type. This function returns  
 * this content type.
 *
 * @param list a GRT value of type list
 *
 * @return  The content type as MYX_GRT_VALUE_TYPE
 *****************************************************************************/
MYX_GRT_VALUE_TYPE myx_grt_list_content_get_type(MYX_GRT_VALUE *list)
{
  return list->value.l->content_type;
}

void myx_grt_list_content_set_type(MYX_GRT_VALUE *list, MYX_GRT_VALUE_TYPE content_type)
{
  list->value.l->content_type= content_type;
}

/**
 ****************************************************************************
 * @brief Returns the content struct name of the list
 * 
 *   The content struct name is only appropriate when this is a list with the 
 * content type DICT
 *
 * @param list a GRT value of type list
 *
 * @return  The content type as MYX_GRT_VALUE_TYPE
 *****************************************************************************/
const char * myx_grt_list_content_get_struct_name(MYX_GRT_VALUE *list)
{
  g_return_val_if_fail(list != NULL, NULL);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, NULL);

  return list->value.l->content_struct_name;
}

/**
 ****************************************************************************
 * @brief Returns the content of the list as MYX_STRINGLIST
 * 
 *
 * @param list a GRT value of type list
 *
 * @return  A new allocated MYX_STRINGLIST
 *****************************************************************************/
MYX_STRINGLIST * myx_grt_list_as_stringlist(MYX_GRT_VALUE *list)
{
  MYX_STRINGLIST *strlist;
  unsigned int i;

  g_return_val_if_fail(list->type == MYX_LIST_VALUE, NULL);
  g_return_val_if_fail(list->value.l->content_type == MYX_STRING_VALUE, NULL);

  strlist= g_malloc0(sizeof(MYX_STRINGLIST));
  strlist->strings_num= list->value.l->items_num;
  strlist->strings= g_malloc(sizeof(char *)*strlist->strings_num);

  for (i= 0; i<strlist->strings_num; i++)
  {
    strlist->strings[i]= g_strdup(myx_grt_list_item_get(list, i)->value.s);
  }

  return strlist;
}

/**
 ****************************************************************************
 * @brief Generates a GRT list with contenttype string for a MYX_STRINGLIST
 * 
 * @param str_list a MYX_STRINGLIST stringlist
 *
 * @return  A new allocated GRT value
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_list_from_stringlist(MYX_STRINGLIST *str_list)
{
  MYX_GRT_VALUE *list= myx_grt_list_new(MYX_STRING_VALUE, "");
  unsigned int i;

  for (i= 0; i<str_list->strings_num; i++)
  {
    myx_grt_list_item_add(list, myx_grt_value_from_string(str_list->strings[i]));
  }

  return list;
}

/**
 ****************************************************************************
 * @brief Inserts an element to the list
 *
 *   Inserts a value to the list. The value to be inserted will not be copied
 * but will be have its reference count increased, therefore can be released.
 *
 * @param list a GRT value of type list
 * @param index the index in the list where the item should be inserted. -1 means append
 * @param value the value to insert.
 * 
 * @return  -1 on error, 0 on success.
 *****************************************************************************/
int myx_grt_list_item_insert(MYX_GRT_VALUE *list, int index, MYX_GRT_VALUE *value)
{
  return myx_grt_bridge_list_item_insert(list, index, value, 1);
}


/**
 ****************************************************************************
 * @brief Adds an element to the list
 *
 *   Adds a value at the end of the list. The value to be inserted will not be copied
 * but will be have its reference count increased, therefore can be released.
 *
 * @param list a GRT value of type list
 * @param value the value to insert.
 * 
 * @return  -1 on error, 0 on success.
 *****************************************************************************/
int myx_grt_list_item_add(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  return myx_grt_list_item_insert(list, -1, value);
}

/**
 ****************************************************************************
 * @brief Adds a string to the list
 *
 *   Adds a string at the end of the list.
 *
 * @param list a GRT value of type list
 * @param s the string to insert.
 * 
 * @return  -1 on error, 0 on success.
 *****************************************************************************/
int myx_grt_list_item_add_as_string(MYX_GRT_VALUE *list, const char *s)
{
  MYX_GRT_VALUE *value_string= myx_grt_value_from_string(s);

  myx_grt_list_item_add(list, value_string);

  return myx_grt_value_release(value_string);
}

/**
 ****************************************************************************
 * @brief Removes an element from the list
 *
 *   Removes the element at the given index from the list. The element will
 * be released.
 *
 * @param list GRT value of type list
 * @param index index in the list of the element to remove
 *
 * @return 0 on success, -1 if the element does not exist.
 *****************************************************************************/
int myx_grt_list_item_del(MYX_GRT_VALUE *list, int index)
{
  return myx_grt_bridge_list_item_del(list, index, 1);
}

/**
 ****************************************************************************
 * @brief Removes a string from the list
 *
 *   Removes the string from the list. The string value will
 * be released.
 *
 * @param list GRT value of type list
 * @param s string to remove
 *
 * @return 0 on success, -1 if the element does not exist.
 *****************************************************************************/
int myx_grt_list_item_del_as_string(MYX_GRT_VALUE *list, const char *s)
{
  int i;
  int item_exists= -1;

  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);

  for (i= myx_grt_list_item_count(list)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *value= myx_grt_list_item_get(list, i);

    if ((myx_grt_value_get_type(value) == MYX_STRING_VALUE) && 
      (strcmp2(s, myx_grt_value_as_string(value)) == 0))
    {
      myx_grt_list_item_del(list, i);

      item_exists= 0;
    }
  }

  return item_exists;
}

/**
 ****************************************************************************
 * @brief Removes a value from the list
 *
 *   Removes the value from the list. The value will
 * be released.
 *
 * @param list GRT value of type list
 * @param value value to remove
 *
 * @return 0 on success, -1 if the value does not exist.
 *****************************************************************************/
int myx_grt_list_item_del_value(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value)
{
  int i;
  int item_exists= -1;

  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);

  for (i= myx_grt_list_item_count(list)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get(list, i);

    if (value == item)
    {
      myx_grt_list_item_del(list, i);

      item_exists= 0;
    }
  }

  return item_exists;
}

/**
 ****************************************************************************
 * @brief Removes a GRT object with the given name from the list
 *
 *   Removes the GRT object from the list. The value will
 * be released.
 *
 * @param list GRT value of type list
 * @param name name of the object to remove
 *
 * @return 0 on success, -1 if the value does not exist.
 *****************************************************************************/
int myx_grt_list_del_by_object_name(MYX_GRT_VALUE *list, const char *name)
{
  int i;
  int item_exists= -1;

  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);

  for (i= myx_grt_list_item_count(list)-1; i>=0; i--)
  {
    MYX_GRT_VALUE *value= myx_grt_list_item_get(list, i);

    if (strcmp2(name, myx_grt_dict_name_item_as_string(value)) == 0)
    {
      myx_grt_list_item_del(list, i);

      item_exists= 0;
    }
  }

  return item_exists;
}

/**
 ****************************************************************************
 * @brief Returns the number of elements in the list
 *
 *   Returns element count in the list.
 *
 * @param list GRT value of type list
 *
 * @return Element count in the list.
 *****************************************************************************/
unsigned int myx_grt_list_item_count(MYX_GRT_VALUE *list)
{
  if (list == NULL)
     return 0;

  g_return_val_if_fail(list->type == MYX_LIST_VALUE, 0);

  // if the list is managed by a bridge
  if (myx_grt_value_is_bridged(list))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(list), "_updateToGrt", list);
  
  return list->value.l->items_num;
}


/**
 ****************************************************************************
 * @brief Returns the value at the given index
 *
 *   Returns the element at the given index. The index is 0 based.
 *
 * @param list GRT value of type list
 * @param index in the list to return
 *
 * @return The item or NULL if the index is invalid.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_list_item_get(MYX_GRT_VALUE *list, unsigned int index)
{
  return myx_grt_bridge_list_item_get(list, index, 1);
}


/**
 ****************************************************************************
 * @brief Returns the referenced value at the given index
 *
 *   Returns the value an element at the given index referes to. The index is 0 based.
 *
 * @param list GRT value of type list
 * @param index in the list to return
 *
 * @return The item or NULL if the index is invalid.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_list_item_get_reference_value(MYX_GRT *grt, MYX_GRT_VALUE *list, unsigned int index)
{
  const char *ref_id= myx_grt_list_item_get_as_string(list, index);

  return myx_grt_reference_cache_lookup(grt, ref_id);
}

/**
 ****************************************************************************
 * @brief Returns the object with the name "name" from a list.
 *
 *   Returns the first value that is a dict and has a entry "name". 
 * The index is 0 based.
 *
 * @param list GRT value of type list
 * @param name the name of the object
 *
 * @return The item or NULL if the name is not found.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_list_item_get_by_object_name(MYX_GRT_VALUE *list, const char *name)
{
  MYX_GRT_VALUE *value= NULL;
  unsigned int i;

  if (!list)
    return NULL;

  for (i= 0; i < myx_grt_list_item_count(list); i++)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get(list, i);

    if (myx_grt_value_get_type(item) == MYX_DICT_VALUE)
    {
      const char *item_name= myx_grt_dict_item_get_as_string(item, "name");

      if (item_name && (strcmp2(item_name, name) == 0))
      {
        value= item;
        break;
      }
    }
  }

  return value;
}

/**
 ****************************************************************************
 * @brief Returns the object with the name "name" from a ref list.
 *
 *   Returns the first ref value that is a dict and has a entry "name". 
 * The index is 0 based.
 *
 * @param grt pointer to the GRT
 * @param list GRT value of type list
 * @param name the name of the object
 *
 * @return The refered value or NULL if the name is not found.
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_list_item_get_reference_value_by_object_name(MYX_GRT *grt, MYX_GRT_VALUE *list, const char *name)
{
  MYX_GRT_VALUE *value= NULL;
  unsigned int i;

  for (i= 0; i < myx_grt_list_item_count(list); i++)
  {
    MYX_GRT_VALUE *item= myx_grt_list_item_get_reference_value(grt, list, i);

    if (item)
    {
      const char *item_name= myx_grt_dict_item_get_as_string(item, "name");

      if (item_name && (strcmp2(item_name, name) == 0))
      {
        value= item;
        break;
      }
    }
  }

  return value;
}

/**
 ****************************************************************************
 * @brief Returns the number at the given index as string
 *
 *   Returns the element at the given index as string.
 *
 * @param list GRT value of type list
 * @param index in the list to return
 *
 * @return The item as string or NULL if the index is invalid.
 *****************************************************************************/
const char * myx_grt_list_item_get_as_string(MYX_GRT_VALUE *list, unsigned int index)
{
  MYX_GRT_VALUE *value= myx_grt_list_item_get(list, index);

  g_return_val_if_fail(value != NULL, NULL);
  g_return_val_if_fail(value->type == MYX_STRING_VALUE, NULL);
  /*if (value->type != MYX_STRING_VALUE)
    return "";*/

  return myx_grt_value_as_string(value);
}

/**
 ****************************************************************************
 * @brief Replaces a value in the list
 *
 *  This will replace the value at the specified index with the new one.
 *  The old value will be released and the new one retained.
 *
 * @param list the list
 * @param index index in the list to replace the value. Should be < list_size
 * @param new_value the new value to put in the list
 *
 * @return 0 if ok, -1 on error (such as invalid arguments or bad index)
 *****************************************************************************/
int myx_grt_list_item_set(MYX_GRT_VALUE *list, unsigned int index, MYX_GRT_VALUE *value)
{  
  MYX_GRT_VALUE* currentValue;
  MYX_GRT_VALUE* newValue = value;

  g_return_val_if_fail(list!=NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);
  g_return_val_if_fail(index < myx_grt_list_item_count(list), -1);
  g_return_val_if_fail(value!=NULL, -1);

  // Replace the value.
  currentValue = list->value.l->items[index];
  if (currentValue != value)
  {
    // If there is a struct assign, make sure we just change the value and do not replace it
    if (myx_grt_value_is_simple_type(currentValue) && myx_grt_value_is_simple_type(value))
    {
      // Change the actual value in the existing value.
      switch (myx_grt_value_get_type(currentValue))
      {
        case MYX_INT_VALUE:
          myx_grt_bridge_value_change_int(currentValue, myx_grt_value_as_int(value));
          break;
        case MYX_REAL_VALUE:
          myx_grt_bridge_value_change_real(currentValue, myx_grt_value_as_real(value));
          break;
        case MYX_STRING_VALUE:
          if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
            myx_grt_bridge_value_change_string(currentValue, myx_grt_value_as_string(value));
          else
          {
            char* newString = myx_grt_value_formated_as_string(value);
            myx_grt_bridge_value_change_string(currentValue, newString);
            g_free(newString);
          };
          break;
        default:
          break;
      };

      newValue = currentValue;
    }
    else
    {
      // TODO: simply replacing the value might cause trouble with additional fields in the struct. Avoid it.
      if (value != NULL)
      {
        list->value.l->items[index]= myx_grt_value_retain(value);

        if (myx_grt_value_is_bridged(list))
        {
          // Duplicate additional fields. Use either the existing values (if set) or that of the list.
          if (myx_grt_value_is_bridged(currentValue))
          {
            myx_grt_value_extend(value);

            value->extended->bridge_module= currentValue->extended->bridge_module;
            value->extended->bridge_data_owner= currentValue->extended->bridge_data_owner;
            if (value->extended->bridge_dict_key)
              g_free(value->extended->bridge_dict_key);
            value->extended->bridge_dict_key= g_strdup(list->extended->bridge_dict_key);
            value->extended->bridge_list_index= index;
          }
          else
          {
            myx_grt_value_extend(value);

            value->extended->bridge_module= list->extended->bridge_module;

            // Note: special handling for data owner here. Read comment in myx_grt_bridge_list_item_insert for
            // more details.
            value->extended->bridge_data_owner= list->extended->bridge_data_owner;
            if (value->extended->bridge_dict_key)
              g_free(value->extended->bridge_dict_key);
            value->extended->bridge_dict_key= g_strdup(list->extended->bridge_dict_key);
            value->extended->bridge_list_index= index;
          };
        };
      }
      else
        list->value.l->items[index]= NULL;

      myx_grt_value_release(currentValue);
    }

    if (newValue && myx_grt_value_is_bridged(newValue) && myx_grt_value_is_bridged(list))
    {
      myx_grt_bridge_func(myx_grt_value_bridge_module_get(newValue), "_valueFromGrt", newValue);
      //??? not sure if this is really needed
      //myx_grt_bridge_func(list->bridge_module, "_updateFromGrt", list);
    }
  };

  myx_grt_value_listener_call(list, MYX_GVCR_LIST_ITEM_CHANGE);

  return 0;
}

/**
 ****************************************************************************
 * @brief Clears the given list by removing all items
 *
 *  All items will be released and their reference count will be reduced by one
 *
 * @param list the list
 *
 * @return 0 if ok, -1 on error
 *****************************************************************************/
int myx_grt_list_clear(MYX_GRT_VALUE *list)
{
  g_return_val_if_fail(list!=NULL, -1);

  for (; myx_grt_list_item_count(list) > 0; )
    myx_grt_list_item_del(list, 0);

  return 0;
}



/**
 ****************************************************************************
 * @brief Converts a value of type list to dict
 *
 *  The passed value withh be converted to a dict. The keys in the dict
 * will be strings containing the index of each item. The original list is
 * modified and becomes a dict.
 *
 * @param value a GRT value of type list that will be converted to dict
 *
 * @return 0 on success, -1 on error.
 *****************************************************************************/
int myx_grt_convert_list_to_dict(MYX_GRT_VALUE *value)
{
  unsigned int i;
  MYX_GRT_LIST *list;
  char key[128];

  g_return_val_if_fail(value != NULL, -1);
  g_return_val_if_fail(value->type == MYX_LIST_VALUE, -1);

  list= value->value.l;
  value->value.d= g_new0(MYX_GRT_DICT, 1);
  for (i= 0; i < list->items_num; i++)
  {
    g_snprintf(key, sizeof(key), "%i", i);
    myx_grt_dict_item_set_value(value, key, list->items[i]);
    myx_grt_value_release(list->items[i]);
  }
  g_free(list);
  
  return 0;
}

/**
 ****************************************************************************
 * @brief Removes the last path part from the given path end returns the result.
 *
 * @param path The path which must be traversed one level up.
 *
 * @return Either NULL (if path is NULL or just "/") or the path without the last part.
 *         The caller is responsible to free the returned string if not NULL.
 *****************************************************************************/
MYX_PUBLIC_FUNC char * myx_get_parent_path(const char *path)
{
  char* delimiter;

  if (path == NULL || strcmp2(path, "/") == 0)
    return NULL;

  delimiter= strrchr(path, '/');
  if (delimiter == NULL)
    return NULL;

  return g_strndup(path, (int) (delimiter - path));
}

/**
 ****************************************************************************
 * @brief Traverses a dict and return the requested value
 *
 *   This will treat the given value as a tree, where each node is named by 
 * the key in its parent.
 *   For dictionaries, the path component is used as the key name. For lists,
 * the given component is used to match the name attribute of each list item
 * or if its a number, it's used as the index in the list.
 *
 * @param dict a dict type value to be traversed which nested dicts
 * @param path a / separated list of key names to the value to be returned.
 *
 * @return A pointer to the value if it's found or NULL otherwise.
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_dict_item_get_by_path(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *path)
{
  MYX_GRT_VALUE *value= dict;
  char *p;
  char *part;

  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(path != NULL, NULL);
  g_return_val_if_fail(*path == '/', NULL);

  if (strcmp2(path, "/") == 0)
    return dict;

  p= g_strdup(path);

  part= strtok(p, "/");

  while (part && *part && value)
  {
    if (value->type == MYX_DICT_VALUE)
      value= myx_grt_dict_item_get_value(value, part);
    else if (value->type == MYX_LIST_VALUE)
    {
      unsigned int i;
      MYX_GRT_LIST *list = value->value.l;
      unsigned int lindex;
      int ok = 0;
      unsigned int Count = myx_grt_list_item_count(value);

      if (sscanf(part, "%i", &lindex) == 1 && lindex >= 0 && lindex < Count)
      {
        value = myx_grt_list_item_get(value, lindex);
        ok = 1;
      };

      if (!ok && list->content_type == MYX_DICT_VALUE)
      {
        int FoundEntry = 0;
        for (i= 0; i < Count; i++)
        {
          MYX_GRT_VALUE *name;
          MYX_GRT_VALUE* ListValue = myx_grt_list_item_get(value, i);
          name= myx_grt_dict_item_get_value(ListValue, "name");
          if (name && strcmp(myx_grt_value_as_string(name), part) == 0)
          {
            value = ListValue;
            FoundEntry = TRUE;
            break;
          }
        }

        if (!FoundEntry)
          value = NULL;
      }
    }
    else if (value->type == MYX_STRING_VALUE)
    {
      value= myx_grt_reference_cache_lookup(grt, myx_grt_value_as_string(value));

      if (value)
        value= myx_grt_dict_item_get_value(value, part);
    }
    else
    {
      value= NULL;
      break;
    }
    
    part= strtok(NULL, "/");
  }

  g_free(p);

  return value;
}

/**
 ****************************************************************************
 * @brief Traverses a dict and changes the value in the path
 *
 *   This will treat the given value as a tree, where each node is named by 
 * the key in its parent.
 *   For dictionaries, the path component is used as the key name. For lists,
 * the given component is used to match the name attribute of each list item.
 *
 * @param dict a dict type value to be traversed which nested dicts
 * @param path a / separated list of key names to the value to be changed.
 * @param new_value the value to assign in the dict
 *
 * @return 0 on success
 * @return -1 if some component of the path doesn't exist or the
 * referenced object is not a dictionary or list.
 *
 *****************************************************************************/
int myx_grt_dict_item_set_by_path(MYX_GRT_VALUE *dict, const char *path, MYX_GRT_VALUE *new_value)
{
  MYX_GRT_VALUE *value= dict, *parent= NULL;
  char *p;
  char *part;
  char *name;
  unsigned int index;
  int index_ok= 0;
  int rc= -1;

  g_return_val_if_fail(dict != NULL, -1);
  g_return_val_if_fail(path != NULL, -1);
  g_return_val_if_fail(*path == '/', -1);
  //g_return_val_if_fail(new_value != NULL, -1);

  p= g_strdup(path);
  name= strrchr(p, '/');
  if (name > p && *(name+1) == 0) /* check if the path ends with a / */
  {
    *name= 0;
    name= strrchr(p, '/')+1;
  }
  else
    name++;
  name= g_strdup(name);

  part= strtok(p, "/");

  while (part && *part && value)
  { 
    index_ok= 0;

    parent= value;

    if (value->type == MYX_DICT_VALUE)
      value= myx_grt_dict_item_get_value(value, part);
    else if (value->type == MYX_LIST_VALUE)
    {
      unsigned int i;
      int ok= 0;
      unsigned int lindex;
      MYX_GRT_LIST *list= value->value.l;

      if (sscanf(part, "%i", &lindex)==1 && lindex >= 0)
      {
        if(lindex < list->items_num)
        {
          index= lindex;
          index_ok= 1;
          value= list->items[lindex];
          ok= 1;
        }
        else if(lindex == list->items_num)  // insert after the last element
        {
          index= lindex;
          value= NULL;
          ok= 1;
        }
      }
      if (!ok && list->content_type == MYX_DICT_VALUE)
      {
        for (i= 0; i < list->items_num; i++)
        {
          MYX_GRT_VALUE *name;
          name= myx_grt_dict_item_get_value(list->items[i], "name");
          if (name && strcmp(myx_grt_value_as_string(name), part)==0)
          {
            index= i;
            index_ok= 1;
            value= list->items[i];
            break;
          }
        }
      }
    }
    else
    {
      value= NULL;
      break;
    }
    
    part= strtok(NULL, "/");
  }

  g_free(p);

  if (parent)
  {    
    if (parent->type == MYX_DICT_VALUE)
    {
      myx_grt_dict_item_set_value(parent, name, new_value);
      rc= 0;
    }
    else if (parent->type == MYX_LIST_VALUE && index_ok)
    {
      myx_grt_list_item_set(parent, index, new_value);
      rc= 0;
    }
    else if(parent->type == MYX_LIST_VALUE && index == myx_grt_list_item_count(parent))
    {
      myx_grt_list_item_add(parent, new_value);
    }
  }

  g_free(name);
  
  return rc;
} 

/**
 ****************************************************************************
 * @brief Returns the name of a dict
 *
 *   This returns the name of a dict. The name of a dict is defined by
 * one of its item with the key "name"
 *
 * @param dict a dict type value
 *
 * @return the name as a string which must not be freed
 * @return NULL if no item with the key "name" exists
 *
 *****************************************************************************/
const char * myx_grt_dict_name_item_as_string(MYX_GRT_VALUE *dict)
{
  MYX_GRT_VALUE *name_item;
  
  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  name_item= myx_grt_dict_item_get_value(dict, "name");

  if (name_item)
    return myx_grt_value_as_string(name_item);
  else
    return NULL;
}


/**
 ****************************************************************************
 * @brief Returns the _id member of a dict
 *
 *   This returns the id of a dict. The id of a dict is defined by
 * one of its item with the key "_id"
 *
 * @param dict a dict type value
 *
 * @return the name as a string which must not be freed
 * @return NULL if no item with the key "name" exists
 *
 *****************************************************************************/
const char * myx_grt_dict_id_item_as_string(MYX_GRT_VALUE *dict)
{
  MYX_GRT_VALUE *id_item;

  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  id_item= myx_grt_dict_item_get_value(dict, "_id");

  if (id_item)
    return myx_grt_value_as_string(id_item);
  else
    return NULL;
}


/**
 ****************************************************************************
 * @brief Sets the _id item of the given dict value to a unique identifier
 *
 *   Sets the item _id of the given dict value to a unique identifier
 *
 * @param dict a dict type value
 *
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_dict_generate_id(MYX_GRT_VALUE *dict)
{
  char *guid;

  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  guid= myx_grt_get_guid();
  myx_grt_dict_item_set_value_from_string(dict, "_id", guid);
  g_free(guid);

  return dict;
}



MYX_GRT_VALUE_TYPE myx_grt_value_get_type(MYX_GRT_VALUE *value)
{
  if (value)
    return value->type;
  else
    return MYX_ANY_VALUE;
}

int myx_grt_value_is_simple_type(MYX_GRT_VALUE *value)
{
  MYX_GRT_VALUE_TYPE value_type= myx_grt_value_get_type(value);

  if ( (value_type == MYX_INT_VALUE) ||
    (value_type == MYX_REAL_VALUE) ||
    (value_type == MYX_STRING_VALUE) )
    return 1;
  else
    return 0;
}

/** 
 ****************************************************************************
 * @brief Allocated memory for the extended information. If the memory has
 *  already been allocated, the function does nothing
 *
 * @param value
 *
 * @return the extended value
 ****************************************************************************/
MYX_GRT_VALUE * myx_grt_value_extend(MYX_GRT_VALUE *value)
{
  if (!value->extended)
    value->extended= g_new0(MYX_GRT_VALUE_EXTENDED, 1);

  return value;
}

/** 
 ****************************************************************************
 * @brief Checks if the given value is a bridged object
 *
 * @param value
 *
 * @return 1 if the value is a dict and a proxy object, 0 otherwise
 ****************************************************************************/
int myx_grt_value_is_bridged(MYX_GRT_VALUE *value)
{
  if ((value) && (value->extended) && (value->extended->bridge_module))
    return 1;
  else
    return 0;
}

/** 
 ****************************************************************************
 * @brief Checks if the given value has listeners allocated
 *
 * @param value
 *
 * @return 1 if the value is a dict and a proxy object, 0 otherwise
 ****************************************************************************/
int myx_grt_value_has_listeners(MYX_GRT_VALUE *value)
{
  if ((value) && (value->extended) && (value->extended->listeners))
    return 1;
  else
    return 0;
}

const char * myx_get_value_type_as_string(MYX_GRT_VALUE_TYPE value_type)
{
  switch (value_type)
  {
    case MYX_ANY_VALUE:
      return "";
    case MYX_INT_VALUE:
      return "int";
    case MYX_REAL_VALUE:
      return "real";
    case MYX_STRING_VALUE:
      return "string";
    case MYX_LIST_VALUE:
      return "list";
    case MYX_DICT_VALUE:
      return "dict";
  }
  return NULL;
}

MYX_GRT_VALUE_TYPE myx_get_value_type_from_string(const char *value_type_name, MYX_GRT_ERROR *error)
{
  *error= MYX_GRT_NO_ERROR;

  //if the value_type_name is NULL or an empty string, return MYX_ANY_VALUE
  if (!value_type_name || !value_type_name[0])
    return MYX_ANY_VALUE;
  else if (strcmp2(value_type_name, "int") == 0)
    return MYX_INT_VALUE;
  else if (strcmp2(value_type_name, "real") == 0)
    return MYX_REAL_VALUE;
  else if (strcmp2(value_type_name, "string") == 0)
    return MYX_STRING_VALUE;
  else if (strcmp2(value_type_name, "list") == 0)
    return MYX_LIST_VALUE;
  else if (strcmp2(value_type_name, "dict") == 0)
    return MYX_DICT_VALUE;

  *error= MYX_GRT_BAD_DATA;
  return MYX_INT_VALUE;
}


/* Get value contents */

int myx_grt_value_as_int(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, 0);
  g_return_val_if_fail(value->type == MYX_INT_VALUE, 0);

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_valueToGrt", value))
      return 0;

  return value->value.i;
}


double myx_grt_value_as_real(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, 0.0);
  g_return_val_if_fail((value->type == MYX_REAL_VALUE)
                       || (value->type == MYX_INT_VALUE), 0.0);

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_valueToGrt", value))
      return 0;

  if (value->type == MYX_REAL_VALUE)
    return value->value.r;
  else
    return value->value.i;
}


const char *myx_grt_value_as_string(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);
  g_return_val_if_fail((value->type == MYX_STRING_VALUE), NULL);

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_valueToGrt", value))
      return "";

  return value->value.s;
}

char * myx_grt_value_formated_as_string(MYX_GRT_VALUE *value)
{
  const char *name;

  if (!value)
    return g_strdup("NULL");

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_valueToGrt", value))
      return "";

  switch (value->type)
  {
    case MYX_ANY_VALUE:
      break;
    case MYX_INT_VALUE:
      return g_strdup_printf("%d", value->value.i);
    case MYX_STRING_VALUE:
      return g_strdup(value->value.s);
    case MYX_REAL_VALUE:
      return g_strdup_printf("%.6g", value->value.r);
    case MYX_LIST_VALUE:
      return g_strdup("LIST");
    case MYX_DICT_VALUE:
      name= myx_grt_dict_name_item_as_string(value);
      if (name)
        return g_strdup(name);
      else
        return g_strdup("DICT");
  }
  return g_strdup("");
}


MYX_GRT_LIST *myx_grt_value_as_list(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);
  g_return_val_if_fail(value->type == MYX_LIST_VALUE, NULL);

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_updateToGrt", value))
      return NULL;

  return value->value.l;
}


MYX_GRT_DICT *myx_grt_value_as_dict(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);
  g_return_val_if_fail(value->type == MYX_DICT_VALUE, NULL);

  // if this value is managed by a bridge, get current value of the item
  if (myx_grt_value_is_bridged(value))
    if (!myx_grt_bridge_func(myx_grt_value_bridge_module_get(value), "_updateToGrt", value))
      return NULL;
  
  return value->value.d;
}


/* Utility stuff to get the contents of a dict directly */

const char *myx_grt_dict_item_get_as_string(MYX_GRT_VALUE *dict, const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(dict, key);
  
  if (!value)
    return NULL;

  return myx_grt_value_as_string(value);
}

char * myx_grt_dict_item_get_formated_as_string(MYX_GRT_VALUE *dict, const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(dict, key);

  if (!value)
    return g_strdup("");
  else
    return myx_grt_value_formated_as_string(value);
}

MYX_GRT_VALUE * myx_grt_dict_item_get_reference_value(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *key)
{
  const char *ref_id= myx_grt_dict_item_get_as_string(dict, key);

  return myx_grt_reference_cache_lookup(grt, ref_id);
}


int myx_grt_dict_item_get_as_int(MYX_GRT_VALUE *dict, const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(dict, key);
  
  if (!value)
    return 0;

  return myx_grt_value_as_int(value);
}


double myx_grt_dict_item_get_as_real(MYX_GRT_VALUE *dict, const char *key)
{
  MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(dict, key);
  
  if (!value)
    return 0.0;

  return myx_grt_value_as_real(value);
}


int _myx_grt_get_refcount(MYX_GRT_VALUE *value)
{
  return value->refcount;
}

/**
 ****************************************************************************
 * @brief Creates an error GRT value that can be returned from a function call
 *
 *   Creates a GRT value of type MYX_GRT_DICT, that contains the value as
 * it's successful return value. The value will be released.
 *
 * @param message the error message
 * @param detail detail information about the error
 * 
 * @return  A newly created dict value struct containing the error information.
 *****************************************************************************/
MYX_GRT_VALUE *make_return_value(MYX_GRT_VALUE *value)
{
  MYX_GRT_VALUE *tmp= myx_grt_dict_create(NULL, NULL,
                                          "value", MYX_ANY_VALUE, value,
                                          NULL);
  // do not release the value because myx_grt_dict_create will not retain the value
  //myx_grt_value_release(value);
  return tmp;
}

/**
 ****************************************************************************
 * @brief Creates an error GRT value that can be returned from a function call
 *
 *   Creates a GRT value of type MYX_GRT_DICT, that contains a error and a detail
 * error message.
 *
 * @param message the error message
 * @param detail detail information about the error
 * 
 * @return  A newly created dict value struct containing the error information.
 *****************************************************************************/
MYX_GRT_VALUE *make_return_value_error(const char *message, const char *detail)
{
  return myx_grt_dict_create(NULL, NULL,
    "error", MYX_STRING_VALUE, message,
    "detail", MYX_STRING_VALUE, detail,
    NULL);
}


void myx_grt_value_bridge_module_set(MYX_GRT_VALUE *value, MYX_GRT_MODULE *module)
{
  if (value)
  {
    myx_grt_value_extend(value);

    value->extended->bridge_module= module;
  }
}

MYX_GRT_MODULE * myx_grt_value_bridge_module_get(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);

  if (value->extended)
    return value->extended->bridge_module;
  else
    return NULL;
}

void myx_grt_value_bridge_data_object_set(MYX_GRT_VALUE *value, void *data)
{
  unsigned int i;

  if (value)
  {
    myx_grt_value_extend(value);

    value->extended->bridge_data_object= data;

    // Recursively go down the subtree and set all data to the same value if
    // the value is not a dict (which has usually own data).
    // TODO: Do recursive bridge_data_set for non-NULL values.
    if (data == NULL)
    {
      // No update needed for value at this point. The bridge is going to reset all values
      // so we assume the member count is correct at this moment.
      // Since only dicts and lists can have child values we only consider those here.
      switch (myx_grt_value_get_type(value))
      {
        case MYX_DICT_VALUE:
          {
            for (i= 0; i < value->value.d->items_num; ++i)
            {
              MYX_GRT_VALUE* childValue= value->value.d->items[i].value;
              if ((childValue != NULL) && (childValue->type != MYX_DICT_VALUE))
                myx_grt_value_bridge_data_owner_set(childValue, data);
            };

            break;
          };
        case MYX_LIST_VALUE:
          {
            for (i= 0; i < value->value.l->items_num; ++i)
            {
              MYX_GRT_VALUE* childValue= value->value.l->items[i];
              if ((childValue != NULL) && (childValue->type != MYX_DICT_VALUE))
                myx_grt_value_bridge_data_owner_set(childValue, data);
            };

            break;
          };
        default:
          break;
      };
    };
  };
}

void * myx_grt_value_bridge_data_object_get(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);

  if (value->extended)
    return value->extended->bridge_data_object;
  else
    return NULL;
}

void myx_grt_value_bridge_data_owner_set(MYX_GRT_VALUE *value, void *data)
{
  if (value)
  {
    myx_grt_value_extend(value);

    value->extended->bridge_data_owner= data;
  }
}

void * myx_grt_value_bridge_data_owner_get(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);

  if (value->extended)
    return value->extended->bridge_data_owner;
  else
    return NULL;
}

void myx_grt_value_bridge_dict_key_set(MYX_GRT_VALUE *value, const char *key)
{
  if (value)
  {
    myx_grt_value_extend(value);

    if (value->extended->bridge_dict_key)
      g_free(value->extended->bridge_dict_key);

    value->extended->bridge_dict_key= g_strdup(key);
  }
}

const char * myx_grt_value_bridge_dict_key_get(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);

  if (value->extended)
    return value->extended->bridge_dict_key;
  else
    return NULL;
}

void myx_grt_value_bridge_list_index_set(MYX_GRT_VALUE *value, int index)
{
  if (value)
  {
    myx_grt_value_extend(value);

    value->extended->bridge_list_index= index;
  }
}

int myx_grt_value_bridge_list_index_get(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, -1);

  if (value->extended)
    return value->extended->bridge_list_index;
  else
    return 0;
}

MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value(MYX_GRT_VALUE *dict, const char *key, MYX_GRT_VALUE *value, int do_bridge_callback)
{
  int i;
  gboolean assignDictBridgeData = FALSE;
  MYX_GRT_VALUE* newValue = value;

  g_return_val_if_fail(dict!=NULL, -1);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, -1);
  g_return_val_if_fail(key!=NULL, -1);
  g_return_val_if_fail(*key, -1);
  
  if (dict->value.d->items_num == 0)
  {
    /* dict is empty */
    dict->value.d->items_num++;
    dict->value.d->items= g_new0(MYX_GRT_DICT_ITEM, dict->value.d->items_num);

    dict->value.d->items[0].key= g_strdup(key);

    if (value)
    {
      dict->value.d->items[0].value= myx_grt_value_retain(value);
      assignDictBridgeData = TRUE;
    }
    else
      dict->value.d->items[0].value= NULL;
  }
  else
  {
    if (!bisect(dict->value.d, key, &i))
    {
      // the key was not in the tree already
      dict->value.d->items_num++;
      dict->value.d->items= g_realloc(dict->value.d->items,sizeof(MYX_GRT_DICT_ITEM)*dict->value.d->items_num);
      
      if (i < (int)dict->value.d->items_num-1)
      {
        memmove(dict->value.d->items + i + 1,
                dict->value.d->items + i,
                sizeof(MYX_GRT_DICT_ITEM)*(dict->value.d->items_num-i-1));
      }
      dict->value.d->items[i].key= g_strdup(key);
      if (value)
      {
        dict->value.d->items[i].value= myx_grt_value_retain(value);
        assignDictBridgeData = TRUE;
      }
      else
        dict->value.d->items[i].value= NULL;
    }
    else
    {
      // Replace the value.
      MYX_GRT_VALUE* currentValue = dict->value.d->items[i].value;
      if (currentValue != value)
      {
        // If there is a struct assign, make sure we just change the value and do not replace it
        if (myx_grt_value_is_simple_type(currentValue) && myx_grt_value_is_simple_type(value) && 
          myx_grt_dict_struct_get_name(dict) != NULL)
        {
          // Change the actual value in the existing value.
          switch (myx_grt_value_get_type(currentValue))
          {
            case MYX_INT_VALUE:
              myx_grt_bridge_value_change_int(currentValue, myx_grt_value_as_int(value));
              break;
            case MYX_REAL_VALUE:
              myx_grt_bridge_value_change_real(currentValue, myx_grt_value_as_real(value));
              break;
            case MYX_STRING_VALUE:
              if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
                myx_grt_bridge_value_change_string(currentValue, myx_grt_value_as_string(value));
              else
              {
                char* newString = myx_grt_value_formated_as_string(value);
                myx_grt_bridge_value_change_string(currentValue, newString);
                g_free(newString);
              };
              break;
            default:
              break;
          };

          newValue = currentValue;
        }
        else
        {
          // TODO: simply replacing the value might cause trouble with additional fields in the struct. Avoid it.
          if (value)
          {
            dict->value.d->items[i].value= myx_grt_value_retain(value);

            if (myx_grt_value_is_bridged(dict))
            {
              // Duplicate additional fields. Use either the existing values (if set) or that of the dict.
              if (myx_grt_value_is_bridged(value))
              {
                if (currentValue)
                {
                  value->extended->bridge_module= currentValue->extended->bridge_module;
                  value->extended->bridge_data_owner= currentValue->extended->bridge_data_owner;
                }
                /*
                else if (value->bridge_module == NULL)
                {
                  value->bridge_module= dict->bridge_module;
                  value->bridge_data_owner= dict->bridge_data_owner;
                }*/
                if (value->extended->bridge_dict_key)
                  g_free(value->extended->bridge_dict_key);
                newValue->extended->bridge_dict_key= g_strdup(key);
              }
              else
                assignDictBridgeData = TRUE;
            };
          }
          else
            dict->value.d->items[i].value= NULL;

          myx_grt_value_release(currentValue);
        }
      }
    }
  }

  // set bridge data if the dict is managed by a bridge
  if (newValue && myx_grt_value_is_bridged(dict))
  {
    if (assignDictBridgeData)
    {
      myx_grt_value_extend(newValue);

      newValue->extended->bridge_module= dict->extended->bridge_module;
      newValue->extended->bridge_data_owner= dict->extended->bridge_data_object;
      if (newValue->extended->bridge_dict_key)
        g_free(newValue->extended->bridge_dict_key);
      newValue->extended->bridge_dict_key= g_strdup(key);
    };

    if (do_bridge_callback && newValue->extended)
    {
      myx_grt_bridge_func(newValue->extended->bridge_module, "_valueFromGrt", newValue);
      //??? not sure if this is really needed
      //myx_grt_bridge_func(dict->bridge_module, "_updateFromGrt", dict);
    }
  }

  myx_grt_value_listener_call(dict, MYX_GVCR_DICT_ITEM_CHANGE);

  return 0;
}

MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_string(MYX_GRT_VALUE *dict, const char *key, const char *s, int do_bridge_callback)
{
  int res;
  MYX_GRT_VALUE *value= myx_grt_value_from_string(s);

  if (!value)
    return 0;

  res= myx_grt_bridge_dict_item_set_value(dict, key, value, do_bridge_callback);

  //lower reference counter by one
  myx_grt_value_release(value);

  return res;
}

MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_int(MYX_GRT_VALUE *dict, const char *key, int i, int do_bridge_callback)
{
  int res;
  MYX_GRT_VALUE *value= myx_grt_value_from_int(i);

  if (!value)
    return 0;

  res= myx_grt_bridge_dict_item_set_value(dict, key, value, do_bridge_callback);

  //lower reference counter by one
  myx_grt_value_release(value);

  return res;
}

MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_real(MYX_GRT_VALUE *dict, const char *key, double d, int do_bridge_callback)
{
  int res;
  MYX_GRT_VALUE *value= myx_grt_value_from_real(d);

  if (!value)
    return 0;

  res= myx_grt_bridge_dict_item_set_value(dict, key, value, do_bridge_callback);

  //lower reference counter by one
  myx_grt_value_release(value);

  return res;
}

MYX_GRT_VALUE * myx_grt_bridge_dict_item_get_value(MYX_GRT_VALUE *dict, const char *key, int do_bridge_callback)
{
  int i;
  MYX_GRT_VALUE * result= NULL;

  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);
  g_return_val_if_fail(key != NULL, NULL);
  g_return_val_if_fail(*key, NULL);

  if (!bisect(dict->value.d, key, &i))
    return NULL;
  else
  {
    result= dict->value.d->items[i].value;

    // If this list is managed by a bridge, update it before accessing the values.
    if (myx_grt_value_is_bridged(dict) && (do_bridge_callback == 1))
    {
      // if the dict is still available
      /*if (!myx_grt_bridge_func(dict->bridge_module, "_updateToGrt", dict))
        return NULL;*/

      // If the value is managed by a bridge, get its value from there.
      if (result && myx_grt_value_is_bridged(result))
        myx_grt_bridge_func(myx_grt_value_bridge_module_get(result), "_valueToGrt", result);
    }

    return result;
  }
}

int myx_grt_bridge_value_as_int(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, 0);
  g_return_val_if_fail(value->type == MYX_INT_VALUE, 0);

  return value->value.i;
}


double myx_grt_bridge_value_as_real(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, 0.0);
  g_return_val_if_fail(value->type == MYX_REAL_VALUE
                       || value->type == MYX_INT_VALUE, 0.0);

  if (value->type == MYX_REAL_VALUE)
    return value->value.r;
  else
    return value->value.i;
}


const char *myx_grt_bridge_value_as_string(MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(value != NULL, NULL);
  g_return_val_if_fail((value->type == MYX_STRING_VALUE), NULL);

  return value->value.s;
}

unsigned int myx_grt_bridge_dict_item_count(MYX_GRT_VALUE *dict, int do_bridge_callback)
{
  g_return_val_if_fail(dict!=NULL, 0);
  if (!(dict->type == MYX_DICT_VALUE))
    g_return_val_if_fail(dict->type == MYX_DICT_VALUE, 0);

  if (myx_grt_value_is_bridged(dict) && (do_bridge_callback == 1))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(dict), "_updateToGrt", dict);

  return dict->value.d->items_num;
}

const char * myx_grt_bridge_dict_item_key_by_index(MYX_GRT_VALUE *dict, unsigned int index, int do_bridge_callback)
{
  g_return_val_if_fail(dict!=NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  // to reduce the number of callbacks, the bridge will not be called
  if (index >= myx_grt_bridge_dict_item_count(dict, 0))
    return NULL;

  return dict->value.d->items[index].key;
}

MYX_GRT_VALUE * myx_grt_bridge_dict_item_value_by_index(MYX_GRT_VALUE *dict, unsigned int index, int do_bridge_callback)
{
  MYX_GRT_VALUE* result;

  g_return_val_if_fail(dict != NULL, NULL);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, NULL);

  // to reduce the number of callbacks, the bridge will not be called
  if (index >= myx_grt_bridge_dict_item_count(dict, 0))
    return NULL;

  result= dict->value.d->items[index].value;

  // If the value is managed by a bridge, get its value from there.
  if ((result != NULL) && myx_grt_value_is_bridged(result) && (do_bridge_callback == 1))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(result), "_valueToGrt", result);

  return result;
}

int myx_grt_bridge_dict_item_del(MYX_GRT_VALUE *dict, const char *key, int do_bridge_callback)
{
  int i;
  g_return_val_if_fail(dict!=NULL, -1);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, -1);
  g_return_val_if_fail(key!=NULL, -1);
  g_return_val_if_fail(*key, -1);

  if (!bisect(dict->value.d, key, &i))
    return -1;
  else
  {
    g_free(dict->value.d->items[i].key);
    myx_grt_value_release(dict->value.d->items[i].value);
    
    memmove(dict->value.d->items + i, dict->value.d->items + i + 1,
            sizeof(MYX_GRT_DICT_ITEM)*(dict->value.d->items_num-i-1));
    dict->value.d->items_num--;
    return 0;
  }

  if (myx_grt_value_is_bridged(dict) && (do_bridge_callback == 1))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(dict), "_updateFromGrt", dict);

  myx_grt_value_listener_call(dict, MYX_GVCR_DICT_ITEM_CHANGE);
}

unsigned int myx_grt_bridge_list_item_count(MYX_GRT_VALUE *list)
{
  if (list == NULL)
     return -1;

  g_return_val_if_fail(list->type == MYX_LIST_VALUE, 0);
  
  return list->value.l->items_num;
}

MYX_GRT_VALUE *myx_grt_bridge_list_item_get(MYX_GRT_VALUE *list, unsigned int index, int do_bridge_callback)
{
  g_return_val_if_fail(list != NULL, NULL);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, NULL);

  // if this list is managed by a bridge, update it before accessing the values
  /*if (do_bridge_callback && list->bridge_module)
    if (!myx_grt_bridge_func(list->bridge_module, "_updateToGrt", list))
      return NULL;*/

  if (do_bridge_callback)
    g_return_val_if_fail(index < myx_grt_list_item_count(list), NULL);    
  else
    g_return_val_if_fail(index < myx_grt_bridge_list_item_count(list), NULL);
    

  // if this list is managed by a bridge, get current value of the item
  if (do_bridge_callback && myx_grt_value_is_bridged(list->value.l->items[index]) && 
    (myx_grt_value_is_simple_type(list->value.l->items[index])))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(list->value.l->items[index]), "_valueToGrt", list->value.l->items[index]);

  return list->value.l->items[index];
}

int myx_grt_bridge_list_item_insert(MYX_GRT_VALUE *list, int index, MYX_GRT_VALUE *value, int do_bridge_callback)
{
  MYX_GRT_VALUE *item= value;

  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);
  g_return_val_if_fail(index <= (int)list->value.l->items_num, -1);
  if (!value)
    return 0;

  // Autoconvert
  if ((list->value.l->content_type == MYX_REAL_VALUE) && (item->type == MYX_INT_VALUE))
    item= myx_grt_value_from_real(myx_grt_value_as_real(value));
  else
  {
    g_return_val_if_fail((list->value.l->content_type == item->type) || (list->value.l->content_type == MYX_ANY_VALUE), -1);

    myx_grt_value_retain(item);
  }
  
  list->value.l->items_num++;
  list->value.l->items= g_realloc(list->value.l->items, 
                                  sizeof(MYX_GRT_VALUE)*list->value.l->items_num);
  if (index < 0)
  {
    index= list->value.l->items_num-1;
    list->value.l->items[index]= item;
  }
  else
  {
    if ((index + 1) < (int) list->value.l->items_num)
      memmove(list->value.l->items + index + 1, list->value.l->items + index,
        sizeof(MYX_GRT_VALUE*)*(list->value.l->items_num-index));
    list->value.l->items[index]= item;
  }

  // set bridge data if the list is managed by a bridge
  if (myx_grt_value_is_bridged(list))
  {
    item->extended->bridge_module= myx_grt_value_bridge_module_get(list);
    // Note! For a new list item, the list's bridge_data_owner is set as the items bridge_data_owner
    // because each list item needs to know which "real world object" is its owner
    // Example: Application.Forms[0]
    //          each form in the forms list should know to which application it belongs, therefore
    //          Application->bridge_data_object= pointer_to_real_world_application_object,
    //          Forms[]->bridge_data_owner == Application->bridge_data_object &&
    //          Forms[1]->bridge_data_owner == Application->bridge_data_object,
    //          Forms[1]->bridge_data_object= pointer_to_real_world_form_object
    item->extended->bridge_data_owner= list->extended->bridge_data_owner;
    if (item->extended->bridge_dict_key)
      g_free(item->extended->bridge_dict_key);
    item->extended->bridge_dict_key= g_strdup(list->extended->bridge_dict_key);
    item->extended->bridge_list_index= index;

    if (do_bridge_callback)
    {
      myx_grt_bridge_func(myx_grt_value_bridge_module_get(item), "_valueFromGrt", item);
      //??? not sure if this is really needed
      //myx_grt_bridge_func(list->bridge_module, "_updateFromGrt", list);
    }
  }

  myx_grt_value_listener_call(list, MYX_GVCR_LIST_CHANGE);

  return 0;
}

/**
 ****************************************************************************
 * @brief Removes an element from the list
 *
 *   Removes the element at the given index from the list. The element will
 * be released.
 *
 * @param list GRT value of type list
 * @param index index in the list of the element to remove
 *
 * @return 0 on success, -1 if the element does not exist.
 *****************************************************************************/
int myx_grt_bridge_list_item_del(MYX_GRT_VALUE *list, int index, int do_bridge_callback)
{
  g_return_val_if_fail(list != NULL, -1);
  g_return_val_if_fail(list->type == MYX_LIST_VALUE, -1);
  g_return_val_if_fail(index >= 0 && index < (int)list->value.l->items_num, -1);

  myx_grt_value_release(list->value.l->items[index]);
  
  memmove(list->value.l->items + index, list->value.l->items + index + 1,
          sizeof(MYX_GRT_VALUE*)*(list->value.l->items_num - index -1 ));

  list->value.l->items_num--;

  // call bridge function
  if (do_bridge_callback && myx_grt_value_is_bridged(list))
    myx_grt_bridge_func(myx_grt_value_bridge_module_get(list), "_updateFromGrt", list);

  myx_grt_value_listener_call(list, MYX_GVCR_LIST_CHANGE);

  return 0;
}

MYX_GRT_VALUE * myx_grt_bridge_dict_new(MYX_GRT *grt, const char *struct_name, void *bridge_data)
{
  MYX_GRT_VALUE *value= g_new0(MYX_GRT_VALUE, 1);
  value->type= MYX_DICT_VALUE;
  value->value.d= g_new0(MYX_GRT_DICT, 1);
  value->refcount= 1;

  // if a struct_name was given, assign it to the dict
  if ((struct_name) && (struct_name[0]))
    myx_grt_bridge_dict_struct_set_name(grt, value, struct_name, 1, bridge_data);

  return value;
}

/**
 ****************************************************************************
 * @brief Assigns a MYX_GRT_STRUCT to a dict value and also set the bridge_data
 *
 *  This will assign a MYX_GRT_STRUCT to a dict value and also set the 
 * bridge_data if given
 *
 * @param dict a dict value
 * @param gstruct the name struct to assign or NULL
 *
 * @return 0 if ok, -1 if the parameters are invalid.
 *****************************************************************************/
int myx_grt_bridge_dict_struct_set_name(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name, 
                                        int register_in_cache, void *bridge_data)
{
  g_return_val_if_fail(dict != NULL, -1);
  g_return_val_if_fail(dict->type == MYX_DICT_VALUE, -1);

  if (dict->value.d->struct_name)
    g_free(dict->value.d->struct_name);
  dict->value.d->struct_name= g_strdup(struct_name);

  if (grt)
  {
    myx_grt_value_extend(dict);

    // check if the struct is managed by a bridge
    dict->extended->bridge_module= myx_grt_struct_get_bridge(grt, myx_grt_struct_get(grt, struct_name));

    // set bridge_data
    dict->extended->bridge_data_object= bridge_data;

    // call bridge function
    if (myx_grt_value_bridge_module_get(dict))
      myx_grt_bridge_func(myx_grt_value_bridge_module_get(dict), "_initDict", dict);


    // cache object
    /*if (register_in_cache)
    {
      const char *_id= myx_grt_dict_item_get_as_string(dict, "_id");

      if (!_id)
      {
        char *new_id= myx_grt_get_guid();

        myx_grt_bridge_dict_item_set_value_from_string(dict, "_id", new_id);
        myx_grt_cache_object(dict, new_id);

        g_free(new_id);
      }
      else
        myx_grt_cache_object(dict, _id);
    }*/
  }
  
  return 0;
}

static MYX_GRT_VALUE * myx_append_path(MYX_GRT_VALUE *path, int index, 
                                       const char *postfix, const char *action)
{
  char *path_new= NULL;
  const char *path_cchar= myx_grt_value_as_string(path);
  MYX_GRT_VALUE *retval= NULL;
  if(index >= 0)
  {
    char *idx= g_strdup_printf("%d", index);
    path_new= g_strconcat(action, path_cchar, "/", idx, NULL);
  }
  else
  {
    const char *slash= (postfix && *postfix) ? "/" : "";
    path_new= g_strconcat(action, path_cchar, slash, postfix, NULL);
  }
  retval= myx_grt_value_from_string(path_new);
  g_free(path_new);
  return retval;
}

/**
 ****************************************************************************
 * @brief Compares types of two dict objects
 *
 * @param grt pointer to the GRT environment
 * @param source the source value
 * @param target the target value
 *
 * @return true if the dicts hold the same type, false if not
 *****************************************************************************/
static int myx_grt_dict_compare_types(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target)
{
  const char *c1, *c2;
  g_return_val_if_fail(myx_grt_value_get_type(source) == MYX_DICT_VALUE, 0);
  g_return_val_if_fail(myx_grt_value_get_type(target) == MYX_DICT_VALUE, 0);
  g_return_val_if_fail(myx_grt_dict_content_get_type(source) == myx_grt_dict_content_get_type(target), 0);
  c1= myx_grt_dict_struct_get_name(source);
  c2= myx_grt_dict_struct_get_name(target);
  if(c1 && c2)
  {
    g_return_val_if_fail(strcmp(c1, c2) == 0, 0);
  }
  else if(c1 || c2)
  {
    return 0;
  }
  c1= myx_grt_dict_content_get_struct_name(source);
  c2= myx_grt_dict_content_get_struct_name(target);
  if(c1 && c2)
  {
    g_return_val_if_fail(strcmp(c1, c2) == 0, 0);
  }
  else if(c1 || c2)
  {
    return 0;
  }
  return 1;
}

/**
 ****************************************************************************
 * @brief Compares types of two list objects
 *
 * @param grt pointer to the GRT environment
 * @param source the source value
 * @param target the target value
 *
 * @return true if the lists hold the same type, false if not
 *****************************************************************************/
static int myx_grt_list_compare_types(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target)
{
  const char *c1, *c2;
  g_return_val_if_fail(myx_grt_value_get_type(source) == MYX_LIST_VALUE, 0);
  g_return_val_if_fail(myx_grt_value_get_type(target) == MYX_LIST_VALUE, 0);
  g_return_val_if_fail(myx_grt_list_content_get_type(source) == myx_grt_list_content_get_type(target), 0);
  c1= myx_grt_list_content_get_struct_name(source);
  c2= myx_grt_list_content_get_struct_name(target);
  if(c1 && c2)
  {
    g_return_val_if_fail(strcmp(c1 ,c2) == 0, 0);
  }
  else if(c1 || c2)
  {
    return 0;
  }
  return 1;
}

/**
 ****************************************************************************
 * @brief Compares types of two objects
 *
 * @param grt pointer to the GRT environment
 * @param source the source value
 * @param target the target value
 *
 * @return true if the values are of the same type, false if not
 *****************************************************************************/
static int myx_grt_value_compare_types(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target)
{
  MYX_GRT_VALUE_TYPE st= myx_grt_value_get_type(source);
  MYX_GRT_VALUE_TYPE tt= myx_grt_value_get_type(target);

  g_return_val_if_fail(st == tt, 0);
  if(!myx_grt_value_is_simple_type(source)) 
  {
    if(st == MYX_DICT_VALUE)
    {
      return myx_grt_dict_compare_types(grt, source, target);
    }
    else if(st == MYX_LIST_VALUE)
    {
      return myx_grt_list_compare_types(grt, source, target);
    }
    else
    {
      return 0;
    }
  }
  return 1;
}

static int myx_grt_simple_value_compare(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target)
{
  int int_s, int_t;
  double double_s, double_t;
  const char *cchar_s, *cchar_t;
  MYX_GRT_VALUE_TYPE st= myx_grt_value_get_type(source);
  MYX_GRT_VALUE_TYPE tt= myx_grt_value_get_type(target);
  g_return_val_if_fail(st == tt, -2);

  switch(st)
  {
  case MYX_INT_VALUE:
    int_s= myx_grt_value_as_int(source);
    int_t= myx_grt_value_as_int(target);
    return int_s < int_t ? -1 : int_s == int_t ? 0 : 1;
  case MYX_REAL_VALUE:
    double_s= myx_grt_value_as_real(source);
    double_t= myx_grt_value_as_real(target);
    return double_s < double_t ? -1 : double_s == double_t ? 0 : 1;
  case MYX_STRING_VALUE:
    cchar_s= myx_grt_value_as_string(source);
    cchar_t= myx_grt_value_as_string(target);
    return strcmp(cchar_s, cchar_t);
  default:
     break;
  }
  return -2;
}

// returns 0 if there were no changes
static int myx_grt_simple_value_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *list, 
                                           MYX_GRT_VALUE * path, MYX_GRT_VALUE *source, 
                                           MYX_GRT_VALUE *target)
{
  const char *cchar_s, *cchar_t;
  double double_s, double_t;
  int int_s, int_t;
  int retval= 0;

  MYX_GRT_VALUE_TYPE st= myx_grt_value_get_type(source);
  MYX_GRT_VALUE_TYPE tt= myx_grt_value_get_type(target);
  g_return_val_if_fail(st == tt, 0);

  switch(st)
  {
  case MYX_INT_VALUE:
    int_s= myx_grt_value_as_int(source);
    int_t= myx_grt_value_as_int(target);
    if(int_s != int_t)
    {
      // change int value
      myx_grt_list_item_add(list, myx_append_path(path, -1, "", "/"));
      myx_grt_list_item_add(list, myx_grt_value_from_int(int_s));
      myx_grt_bridge_value_change_int(target, int_s);
      retval= 1;
    }
    break;
  case MYX_REAL_VALUE:
    double_s= myx_grt_value_as_real(source);
    double_t= myx_grt_value_as_real(target);
    if(double_s != double_t)
    {
      // change double value
      myx_grt_list_item_add(list, myx_append_path(path, -1, "", "/"));
      myx_grt_list_item_add(list, myx_grt_value_from_real(double_s));
      myx_grt_bridge_value_change_real(target, double_s);
      retval= 1;
    }
    break;
  case MYX_STRING_VALUE:
    cchar_s= myx_grt_value_as_string(source);
    cchar_t= myx_grt_value_as_string(target);
    if(strcmp(cchar_s, cchar_t) != 0)
    {
      // change const char* value
      myx_grt_list_item_add(list, myx_append_path(path, -1, "", "/"));
      myx_grt_list_item_add(list, myx_grt_value_from_string(cchar_s));
      myx_grt_bridge_value_change_string(target, cchar_s);
      retval= 1;
    }
    break;
  default:
    break;
  }

  return retval;
}

/*
static const char *get_dict_id(MYX_GRT_VALUE *dict)
{
  return myx_grt_dict_id_item_as_string(dict);
}
*/
static const char *get_dict_name(MYX_GRT_VALUE *dict)
{
  const char *old_name= myx_grt_dict_item_get_as_string(dict, "oldName");
  if(old_name)
    return old_name;
  return myx_grt_dict_item_get_as_string(dict, "name");
}

// returns 0 if there were no changes
static int myx_grt_list_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *list, 
                                   MYX_GRT_VALUE * path, MYX_GRT_VALUE *source, 
                                   MYX_GRT_VALUE *target,
                                   dict_identification_func dict_id_func)
{
  int next, is_simple;
  const char *s_id, *t_id;
  unsigned int s_count= myx_grt_list_item_count(source);
  unsigned int t_count;
  MYX_GRT_VALUE *s_value, *t_value, *sub_path;
  unsigned int i, j;
  int retval= 0;

  // find possible / * +
restart1:
  for(i= 0; i < s_count; i++)
  {
    next= 0;
    s_value= myx_grt_list_item_get(source, i);
    t_count= myx_grt_list_item_count(target);
    if(myx_grt_value_is_simple_type(s_value))
    {
      for(j= 0; j < t_count; j++)
      {
        t_value= myx_grt_list_item_get(target, j);
        if(myx_grt_simple_value_compare(grt, s_value, t_value) == 0)
        {
          if(i != j)
          {
            // this value could be already matched earlier
            // this is detected by checking if source[j] is equal to target[j]
            if((j < i) && (myx_grt_simple_value_compare(grt, myx_grt_list_item_get(source, j), t_value) == 0))
              continue;
            
            // move s_value in diff
            sub_path= myx_append_path(path, i, "", "*");
            myx_grt_list_item_add(list, sub_path);
            myx_grt_value_release(sub_path);
            myx_grt_list_item_add(list, myx_grt_value_from_int(j));

            myx_grt_list_item_del(target, j);
            if(i > myx_grt_list_item_count(target))
              myx_grt_list_item_insert(target, myx_grt_list_item_count(target), s_value);
            else
              myx_grt_list_item_insert(target, i, s_value);

            retval= 1;
          }
          next= 1;
          break;
        }
      }
      if(next == 0) // s_value is not found in target
      {
        // add s_value to diff
        sub_path= myx_append_path(path, i, "", "+");
        myx_grt_list_item_add(list, sub_path);
        myx_grt_value_release(sub_path);
        s_value= myx_grt_value_dup(s_value);
        myx_grt_list_item_add(list, s_value);
        if(i > myx_grt_list_item_count(target))
          myx_grt_list_item_insert(target, myx_grt_list_item_count(target), s_value);
        else
          myx_grt_list_item_insert(target, i, s_value);
        retval= 1;
        goto restart1;
      }
    }
    else  // s_value is a dict or a list
    {
      //s_id= myx_grt_dict_id_item_as_string(s_value);
      //s_id= myx_grt_dict_item_get_as_string(s_value, "name");

      s_id= dict_id_func(s_value);
      for(j= 0; j < t_count; j++)
      {
        t_value= myx_grt_list_item_get(target, j);
        //t_id= myx_grt_dict_id_item_as_string(t_value);
        //t_id= myx_grt_dict_item_get_as_string(t_value, "name");
        t_id= dict_id_func(t_value);
        if(strcmp(s_id, t_id) == 0)
        {
          sub_path= myx_append_path(path, i, "", "");
          retval= myx_grt_generic_diff_make(grt, list, sub_path, s_value, t_value, dict_id_func) || retval;
          if(i != j)
          {
            // move s_value in diff
            sub_path= myx_append_path(path, i, "", "*");
            myx_grt_list_item_add(list, sub_path);
            myx_grt_value_release(sub_path);
            myx_grt_list_item_add(list, myx_grt_value_from_int(j));

            myx_grt_list_item_del(target, j);
            if(i > myx_grt_list_item_count(target))
              myx_grt_list_item_insert(target, myx_grt_list_item_count(target), s_value);
            else
              myx_grt_list_item_insert(target, i, s_value);
            retval= 1;
          }
          next= 1;
          break;
        }
      }
      if(next == 0) // s_value is not found in target
      {
        // add s_value to diff
        sub_path= myx_append_path(path, i, "", "+");
        myx_grt_list_item_add(list, sub_path);
        myx_grt_value_release(sub_path);
        s_value= myx_grt_value_dup(s_value);
        myx_grt_list_item_add(list, s_value);

        if(i > myx_grt_list_item_count(target))
          myx_grt_list_item_insert(target, myx_grt_list_item_count(target), s_value);
        else
          myx_grt_list_item_insert(target, i, s_value);
        retval= 1;
        goto restart1;
      }
    }
  } // for(i)

  s_count= myx_grt_list_item_count(source);
restart2:
  t_count= myx_grt_list_item_count(target);
  for(j= 0; j < t_count; j++)
  {
    next= 0;
    t_value= myx_grt_list_item_get(target, j);
    is_simple= myx_grt_value_is_simple_type(t_value);
    if(!is_simple)
    {
      //t_id= myx_grt_dict_id_item_as_string(t_value);
      t_id= dict_id_func(t_value);
    }
    for(i= 0; i < s_count; i++)
    {
      s_value= myx_grt_list_item_get(source, i);
      if(is_simple)
      {
        next= (myx_grt_simple_value_compare(grt, s_value, t_value) == 0);
      }
      else  // t_value is dict/list
      {
        //s_id= myx_grt_dict_id_item_as_string(s_value);
        s_id= dict_id_func(s_value);
        next= (strcmp(s_id, t_id) == 0);
      }
      if(next)
      {
        break;
      }
    }
    if(next == 0)
    {
      // del t_value from the diff
      sub_path= myx_append_path(path, j, "", "-");
      myx_grt_list_item_add(list, sub_path);
      myx_grt_value_release(sub_path);
      myx_grt_list_item_add(list, myx_grt_value_from_string(""));
      myx_grt_list_item_del(target, j);
      retval= 1;
      goto restart2;
    }
  }

  return retval;
}

// returns 0 if nothing was changed
static int myx_grt_dict_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *list, 
                                   MYX_GRT_VALUE * path, MYX_GRT_VALUE *source, 
                                   MYX_GRT_VALUE *target, dict_identification_func dict_id_func)
{
  const char *s_key, *t_key;
  MYX_GRT_VALUE *s_value, *t_value, *sub_path;
  unsigned int i, j, next;
  unsigned int s_count;
  unsigned int t_count;
  MYX_GRT_VALUE_TYPE st;
  MYX_GRT_VALUE_TYPE tt;
  int retval= 0;  // the change flag

  // find possible / + -
restart1:
  s_count= myx_grt_dict_item_count(source);
  for(i= 0; i < s_count; i++)
  {
    next= 0;
    myx_grt_dict_item_by_index(source, i, &s_key, &s_value);
    t_count= myx_grt_dict_item_count(target);
    for(j= 0; j < t_count; j++)
    {
      myx_grt_dict_item_by_index(target, j, &t_key, &t_value);
      st= myx_grt_value_get_type(s_value);
      tt= myx_grt_value_get_type(t_value);

      if(strcmp(s_key, t_key) == 0)
      {
        if(st == tt) 
        {
          // diff s_value/t_value
          sub_path= myx_append_path(path, -1, s_key, "");
          retval= myx_grt_generic_diff_make(grt, list, sub_path, s_value, t_value, dict_id_func) || retval;
          myx_grt_value_release(sub_path);
        }
        else if(s_value != NULL)  // values have differenent types just exchange
        {
          sub_path= myx_append_path(path, -1, s_key, "/");
          myx_grt_list_item_add(list, sub_path);
          myx_grt_list_item_add(list, s_value);
          myx_grt_value_release(sub_path);
          retval= 1;
        }
        else 
        {
          myx_grt_list_item_add(list, myx_append_path(path, -1, t_key, "-"));
          myx_grt_list_item_add(list, myx_grt_value_from_string(""));
          myx_grt_dict_item_del(target, t_key);
          retval= 1;
        }
        next= 1;
        break;
      }
    }
    if(next == 0)
    {
      myx_grt_list_item_add(list, myx_append_path(path, -1, s_key, "+"));
      s_value= myx_grt_value_dup(s_value);
      myx_grt_list_item_add(list, s_value);
      myx_grt_dict_item_set_value(target, g_strdup(s_key), s_value);
      retval= 1;
      goto restart1;
    }
  }

  // find possible -
  s_count= myx_grt_dict_item_count(source);
restart2:
  t_count= myx_grt_dict_item_count(target);
  for(j= 0; j < t_count; j++)
  {
    next= 0;
    myx_grt_dict_item_by_index(target, j, &t_key, &t_value);
    for(i= 0; i < s_count; i++)
    {
      myx_grt_dict_item_by_index(source, i, &s_key, &s_value);
      if(strcmp(s_key, t_key) == 0)
      {
        next= 1;
        break;
      }
    }
    if(next == 0)
    {
      // delete s_value from diff
      myx_grt_list_item_add(list, myx_append_path(path, -1, t_key, "-"));
      myx_grt_list_item_add(list, myx_grt_value_from_string(""));
      myx_grt_dict_item_del(target, t_key);
      retval= 1;
      goto restart2;
    }
  }

  return retval;
}

// returns 1 if diff found any changes
static int myx_grt_generic_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *list, 
                                      MYX_GRT_VALUE * path, MYX_GRT_VALUE *source, 
                                      MYX_GRT_VALUE *target, dict_identification_func dict_id_func)
{
  MYX_GRT_VALUE_TYPE st= myx_grt_value_get_type(source);
  MYX_GRT_VALUE_TYPE tt= myx_grt_value_get_type(target);
  g_return_val_if_fail(st == tt, 0);

  switch(st)
  {
  case MYX_ANY_VALUE:
    break;
  case MYX_INT_VALUE:
  case MYX_REAL_VALUE:
  case MYX_STRING_VALUE:
    return myx_grt_simple_value_diff_make(grt, list, path, source, target);
  case MYX_LIST_VALUE:
    return myx_grt_list_diff_make(grt, list, path, source, target, dict_id_func);
  case MYX_DICT_VALUE:
    return myx_grt_dict_diff_make(grt, list, path, source, target, dict_id_func);
  }
  return 0;
}

MYX_GRT_VALUE * myx_grt_value_diff_make_with_params(MYX_GRT *grt, MYX_GRT_VALUE *source, 
                                                    MYX_GRT_VALUE *target, dict_identification_func dict_id_func)
{
  MYX_GRT_VALUE * retval= myx_grt_list_new(MYX_ANY_VALUE, NULL);
  MYX_GRT_VALUE * source_dup, *target_dup;
  MYX_GRT_VALUE * path;

  g_return_val_if_fail(retval != NULL, NULL);
  g_return_val_if_fail(myx_grt_value_compare_types(grt, source, target) == 1, NULL);

  path= myx_grt_value_from_string("");
  source_dup= myx_grt_value_dup(source);
  target_dup= myx_grt_value_dup(target);
  myx_grt_generic_diff_make(grt, retval, path, target_dup, source_dup, dict_id_func);

  return retval;
}

/**
 ****************************************************************************
 * @brief Creates a diff between two GRT values
 *
 *  Recursivly walks through the given value and its members and builds the 
 * diff as a GRT list, that is of the form 
 *
 * {path, change, path, change, path, change}.
 *
 * Example:
 * {"/name", MYX_STRING_VALUE, "./engine", MYX_STRING_VALUE, "-/columns/1", NULL, 
 *  "+/columns/2", MYX_DICT_VALUE}
 *
 * the encoding of the path is the following. If it starts with
 * / a value change
 * - a value remove
 * + a value added
 * * a value changed position in list
 *
 * for lists, the index that was removed or added is given
 *
 * @param grt pointer to the GRT environment
 * @param source the source value
 * @param target the target value
 *
 * @return the diff as a GRT value
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_value_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target)
{
  return myx_grt_value_diff_make_with_params(grt, source, target, get_dict_name);
}

/**
 ****************************************************************************
 * @brief Applies a diff to a value
 *
 *  Applies a diff to the given value and also returns the updated value
 *
 * @param grt pointer to the GRT environment
 * @param value the value the diff will be applied to
 * @param diff the diff
 *
 * @return value updated (not duplicated)
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_value_diff_apply(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE *diff)
{
  unsigned int i, diff_count;
  
  diff_count= myx_grt_list_item_count(diff);
  for(i= 0; i < diff_count; i++)
  {
    unsigned move_from_index, move_to_index;
    char *parent_path;
    MYX_GRT_VALUE *parent_obj;
    const char *diff_path= myx_grt_value_as_string(myx_grt_list_item_get(diff, i++));
    MYX_GRT_VALUE *action_obj= myx_grt_list_item_get(diff, i);
    char diff_op= *diff_path++;
    MYX_GRT_VALUE_TYPE type;

    parent_path= myx_get_parent_path(diff_path);
    parent_path= (parent_path && parent_path[0]) ? parent_path : g_strdup("/");
    parent_obj= myx_grt_dict_item_get_by_path(grt, value, parent_path);
    //diff_path= diff_path+strlen(parent_path)-1;
    diff_path= diff_path+strlen(parent_path);
    if(diff_path[0] != '/') 
    {
      diff_path= g_strconcat("/", diff_path, NULL);
    }

    switch(diff_op)
    {
    case '+':
      type= myx_grt_value_get_type(parent_obj);
      if(type == MYX_LIST_VALUE)
      {
        if(diff_path[0] == '/')
        {
          ++diff_path;
        }
        myx_grt_list_item_insert(parent_obj, atoi(diff_path), action_obj);
      }
      else if(type == MYX_DICT_VALUE)
      {
        //myx_grt_dict_item_set_by_path(value, diff_path, action_obj);
        myx_grt_dict_item_set_by_path(parent_obj, diff_path, action_obj);
      }
      break;
    case '/':
      //myx_grt_dict_item_set_by_path(, diff_path, action_obj);
      myx_grt_dict_item_set_by_path(parent_obj, diff_path, action_obj);
      break;
    case '-':
      type= myx_grt_value_get_type(parent_obj);
      if(diff_path[0] == '/')
      {
        ++diff_path;
      }
      if(type == MYX_LIST_VALUE)
      {
        myx_grt_list_item_del(parent_obj, atoi(diff_path));
      }
      else if(type == MYX_DICT_VALUE)
      {
        myx_grt_dict_item_del(parent_obj, diff_path);
      }
      break;
    case '*':
      if(diff_path[0] == '/')
      {
        ++diff_path;
      }
      // assuming parent_obj is list (* applicable only to lists)
      move_to_index= atoi(diff_path);
      move_from_index= myx_grt_value_as_int(action_obj);
      action_obj= myx_grt_list_item_get(parent_obj, move_from_index);
      action_obj= myx_grt_value_retain(action_obj);
      myx_grt_list_item_del(parent_obj, move_from_index);
      if(myx_grt_list_item_count(parent_obj) < move_to_index) 
      {
        myx_grt_list_item_insert(parent_obj, myx_grt_list_item_count(parent_obj), action_obj);
      }
      else
      {
        myx_grt_list_item_insert(parent_obj, move_to_index, action_obj);
      }
      myx_grt_value_release(action_obj);
      break;
    }
    g_free(parent_path);
  }
  return value;
}
