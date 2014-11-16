/* Copyright (C) 2005 MySQL AB

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
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA */

#ifdef ENABLE_PHP_MODULES

// this would remove the warning
//#define NSAPI 1

#ifdef __BORLANDC__
  // Taken from sys/types.h Visual Studio as this typedef is missing in Borland's header.
  #ifndef _OFF_T_DEFINED
    typedef long _off_t;                    /* file offset value */

    #if     !__STDC__
    /* Non-ANSI name for compatibility */
    typedef long off_t;
    #endif

    #define _OFF_T_DEFINED
  #endif
#endif

#include "myx_grt_php.h"
#include "myx_shared_util_functions.h"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <Basetsd.h>
#else
#define LongToPtr(l) ((void*)(l))
#define PtrToLong(p) ((long)(p))
#endif

#define PHPGRTCLASSPREFIX "com_mysql_grt_"

static MYX_GRT_ERROR php_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);
static MYX_GRT_ERROR php_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule);

static int php_set_ini_entry(char *entry, char *value, int stage);
static int php_ub_write(const char *str, unsigned int str_length TSRMLS_DC);
static void php_log_message(char *message);
static void php_sapi_error(int type, const char* fmt, ...);

static char * php_eval_string(const char *fmt, ...);
static long php_eval_long(const char *fmt, ...);
static double php_eval_real(const char *fmt, ...);

// thread "safety" !!!
static void ***tsrm_ls;

/**
 ****************************************************************************
 * @brief Helper function to convert a global Grt object to a Php object
 *
 *   Use by the PHP > C callback functions. The Php object is created in a 
 * global variable $com_mysql_grt_global_object.
 *
 * @param grt
 *         Pointer to the MYX_GRT struct
 * @param objectPath
 *         The global Grt object path of the object as a string
 * @param value
 *         the global Grt object value 
 *****************************************************************************/
static void php_global_object_from_grt_value(MYX_GRT *grt, const char *objectPath, MYX_GRT_VALUE *value)
{
  if (value)
  {
    if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
    {
      php_eval_string("$com_mysql_grt_global_object = %s", myx_grt_value_as_string(value));
    }
    else if (myx_grt_value_get_type(value) == MYX_INT_VALUE)
    {
      php_eval_string("$com_mysql_grt_global_object = %s", myx_grt_value_formated_as_string(value));
    }
    else if (myx_grt_value_get_type(value) == MYX_REAL_VALUE)
    {
      php_eval_string("$com_mysql_grt_global_object = %s", myx_grt_value_formated_as_string(value));
    }
    else if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
    {
      const char *p_class_name;

      if ( (myx_grt_list_content_get_type(value) == MYX_STRING_VALUE) && 
        (!myx_grt_list_content_get_struct_name(value)) )
        p_class_name= "GrtStringList";
      else if (myx_grt_list_content_get_struct_name(value))
      {
        p_class_name= "GrtObjectList";
      }
      else
        p_class_name= "GrtList";

      if (myx_grt_list_content_get_struct_name(value))
        php_eval_string("$com_mysql_grt_global_object = new %s('%s', '%s')", 
          p_class_name, myx_grt_list_content_get_struct_name(value), objectPath);
      else
        php_eval_string("$com_mysql_grt_global_object = new %s('', '%s')", 
          p_class_name, objectPath);
    }
    else if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
    {
      if (myx_grt_dict_struct_get_name(value))
      {
        char *p_class_name= g_strdup(PHPGRTCLASSPREFIX);

        // this is a struct instance
        p_class_name= str_g_append_and_free(p_class_name, 
          str_g_replace(g_strdup(myx_grt_dict_struct_get_name(value)), ".", "_"));

        // Create reflection class
        php_eval_string("$com_mysql_grt_global_object_class = new ReflectionClass('%s');", 
          p_class_name);

        // Create new object instance
        php_eval_string("$com_mysql_grt_global_object = $com_mysql_grt_global_object_class->newInstance('%s');", 
          objectPath);

        g_free(p_class_name);
      }
      else
      {
        const char *p_class_name;

        if ( (myx_grt_dict_content_get_type(value) == MYX_STRING_VALUE) && (!myx_grt_dict_struct_get_name(value)) )
          p_class_name= "GrtStringHashMap";
        else
          p_class_name= "GrtHashMap";

        // Create reflection class
        php_eval_string("$com_mysql_grt_global_object_class = new ReflectionClass('%s');", 
          p_class_name);

        // Create new object instance
        php_eval_string("$com_mysql_grt_global_object = $com_mysql_grt_global_object_class->newInstance('', '%s');", 
          objectPath);
      }
    }
  }
}

/**
 ****************************************************************************
 * @brief Helper function to convert a Php object to a global Grt object
 *
 *   Use by the PHP > C callback functions. The global variable 
 * $com_mysql_grt_global_object is queried and a global Grt value is created
 * based on the values
 *
 * @param grt
 *         Pointer to the MYX_GRT struct
 * @param var_name
 *         Name of the global PHP variable to process
 * @param level
 *         Number of recursive calls to php_object_to_global_grt_value
 *****************************************************************************/
static MYX_GRT_VALUE *php_object_to_global_grt_value(MYX_GRT *grt, const char *var_name, int level)
{
  MYX_GRT_VALUE *res= NULL;
  long is_int;
  long is_real;
  long is_string;
  long instance_of_grtlist;
  long instance_of_grthashmap;
  long instance_of_grtobject;

  //FILE *f= myx_fopen("C:\\log.txt", "a");

  if (php_eval_long("$%s == null", var_name) == 1)
    return NULL;

  // Get type
  is_int= php_eval_long("is_long($%s);", var_name);
  if (!is_int) {
    is_real= php_eval_long("is_float($%s);", var_name);

    if (!is_real)
    {
      is_string= php_eval_long("is_string($%s);", var_name);

      if (!is_string)
      {
        instance_of_grtlist= php_eval_long("isInstanceOrSubclass($%s, 'GrtList');", var_name);

        if (!instance_of_grtlist)
        {
          instance_of_grthashmap= php_eval_long("isInstanceOrSubclass($%s, 'GrtHashMap');", var_name);

          if (!instance_of_grthashmap)
          {
            instance_of_grtobject= php_eval_long("isInstanceOrSubclass($%s, '" PHPGRTCLASSPREFIX "GrtObject');", var_name);
          }
        }
      }
    }
  }

  if (is_int)
  {
    // Integer
    int i= php_eval_long("$%s;", var_name);
    res= myx_grt_value_from_int(i);
  }
  else if (is_real)
  {
    // Real
    res= myx_grt_value_from_real(php_eval_real("$%s;", var_name));
  }
  else if (is_string)
  {
    // String
    char *str= php_eval_string("$%s;", var_name);

    res= myx_grt_value_from_string(str);

    g_free(str);
  }
  else if (instance_of_grtlist)
  {
    // List
    char *content_type_name= php_eval_string("$%s->getContentType();", var_name);
    char *content_struct_name_tmp= str_g_replace(php_eval_string("$%s->getContentStructName();", var_name), "_", ".");
    char *content_struct_name= g_strdup(content_struct_name_tmp + strlen(PHPGRTCLASSPREFIX));
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE_TYPE content_type= myx_get_value_type_from_string(content_type_name, &error);
    jint item_count= php_eval_long("$%s->size();", var_name);
    int i;

    g_free(content_struct_name_tmp);

    /*fprintf(f, "LIST" _br);
    fprintf(f, "content_type_name: %s" _br, content_type_name);
    fprintf(f, "content_struct_name: %s" _br, content_struct_name);
    fprintf(f, "item-count: %d" _br, item_count);*/

    // create list of correct content_type and content_struct_name
    if (error == MYX_GRT_NO_ERROR)
      res= myx_grt_list_new(content_type, content_struct_name);

    g_free(content_type_name);
    g_free(content_struct_name);

    if (error != MYX_GRT_NO_ERROR)
      return NULL;

    // now convert the list's items
    for (i= 0; i < item_count; i++)
    {
      char *tmp_var_name= g_strdup_printf("com_mysql_grt_global_object_%d", ++level);
      MYX_GRT_VALUE *item_value;

      php_eval_string("$%s = $%s->getObject(%d);", tmp_var_name, var_name, i);

      item_value= php_object_to_global_grt_value(grt, tmp_var_name, level);
      myx_grt_list_item_add(res, item_value);
      myx_grt_value_release(item_value);

      g_free(tmp_var_name);
    }
  } 
  else if (instance_of_grthashmap)
  {
    // Hashmap
    char *content_type_name= php_eval_string("$%s->getContentType();", var_name);
    char *content_struct_name_tmp= str_g_replace(php_eval_string("$%s->getContentStructName();", var_name), "_", ".");
    char *content_struct_name= g_strdup(content_struct_name_tmp + strlen(PHPGRTCLASSPREFIX));
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE_TYPE content_type= myx_get_value_type_from_string(content_type_name, &error);
    int i;
    int key_count;

    g_free(content_struct_name_tmp);

    php_eval_string("$%s_keys = $%s->getKeys();", var_name, var_name);
    key_count= php_eval_long("count($%s_keys);", var_name);

    /*fprintf(f, "HASHMAP" _br);
    fprintf(f, "content_type_name: %s" _br, content_type_name);
    fprintf(f, "content_struct_name: %s" _br, content_struct_name);
    fprintf(f, "key-count: %s" _br, (*env)->GetArrayLength(env, keys));*/

    // create hashmap of correct content_type and content_struct_name
    if (error == MYX_GRT_NO_ERROR)
      res= myx_grt_dict_new_typed(content_type, content_struct_name);

    g_free(content_type_name);
    g_free(content_struct_name);

    if (error != MYX_GRT_NO_ERROR)
      return NULL;

    // now convert the hashmap's items
    for (i= 0; i < key_count; i++)
    { 
      char *tmp_var_name= g_strdup_printf("com_mysql_grt_global_object_%d", ++level);
      char *key= php_eval_string("$%s_keys[%d];", var_name, i);
      MYX_GRT_VALUE *map_value;

      php_eval_string("$%s = $%s->getObject('%s');", tmp_var_name, var_name, key);

      map_value= php_object_to_global_grt_value(grt, tmp_var_name, level);
      myx_grt_dict_item_set_value(res, key, map_value);
      myx_grt_value_release(map_value);

      g_free(key);
      g_free(tmp_var_name);
    }
  } 
  else if (instance_of_grtobject)
  {
    // Object
    char *class_name= str_g_replace(php_eval_string("get_class($%s);", var_name), "_", ".");
    char *struct_name= g_strdup(class_name + strlen(PHPGRTCLASSPREFIX));
    MYX_GRT_STRUCT *gstruct;
    unsigned int member_count;
    unsigned int i;

    /*fprintf(f, "DICT" _br);
    fprintf(f, "class_name: %s" _br, class_name);
    fprintf(f, "struct_name: %s" _br, struct_name);*/

    // Create object
    res= myx_grt_dict_new(grt, struct_name);

    gstruct= myx_grt_struct_get(grt, struct_name);

    member_count= myx_grt_struct_get_member_count_total(grt, gstruct);

    //fprintf(f, "member_count: %d" _br, member_count);

    // Set object's members
    for (i= 0; i < member_count; i++)
    {
      MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index_total(grt, gstruct, i);
      char *method_name= g_strdup("get");
      //char *method_sig= g_strdup("()");

      method_name= str_g_append(method_name, myx_grt_struct_get_member_name(member));
      method_name[3]= g_ascii_toupper(method_name[3]);

      //fprintf(f, "method_name: %s" _br, method_name);

      if (myx_grt_struct_member_get_type(member) == MYX_INT_VALUE)
      {
        long p_int= php_eval_long("$%s->%s();", var_name, method_name);
        MYX_GRT_VALUE *member_value= myx_grt_value_from_int(p_int);
        myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_REAL_VALUE)
      {
        double p_real= php_eval_real("$%s->%s();", var_name, method_name);
        MYX_GRT_VALUE *member_value= myx_grt_value_from_real(p_real);
        myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_STRING_VALUE)
      {
        char *p_str= php_eval_string("$%s->%s();", var_name, method_name);
        if (p_str) {
          MYX_GRT_VALUE *member_value= myx_grt_value_from_string(p_str);
          myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
          g_free(p_str);
        }
      }
      else
      {
        if ( (myx_grt_struct_member_get_content_struct_name(member) != NULL)
            && (myx_grt_struct_member_get_content_type(member) == MYX_STRING_VALUE) )
        {
          //deal with reference lists
          MYX_GRT_VALUE *ref_list= myx_grt_list_new(MYX_STRING_VALUE, 
              myx_grt_struct_member_get_content_struct_name(member));
          int item_count;
          int j;

          php_eval_string("$com_mysql_grt_list_member = $%s->%s();", var_name, method_name);
          
          item_count= php_eval_long("$com_mysql_grt_rev_list_member->size();");

          for (j= 0; j < item_count; j++)
          {
            char *ref_id= php_eval_string("$com_mysql_grt_rev_list_member->get(%d);", j);
            
            myx_grt_list_item_add_as_string(ref_list, ref_id);

            g_free(ref_id);
          }

          myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), ref_list);
          myx_grt_value_release(ref_list);
        }
        else
        {
          // if the member var is again an object
          char *tmp_var_name= g_strdup_printf("com_mysql_grt_global_object_%d", ++level);
          MYX_GRT_VALUE *member_value;

          php_eval_string("$%s = $%s->%s();", tmp_var_name, var_name, method_name);

          member_value= php_object_to_global_grt_value(grt, tmp_var_name, level);
          myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
          myx_grt_value_release(member_value);

          g_free(tmp_var_name);
        }
      }

      g_free(method_name);
    }

    g_free(class_name);
  }

  //fclose(f);
  return res;
}


/**
 ****************************************************************************
 * @brief Init function of the GRT callback module
 *
 *   This function is called when the GRT module is initialized
 *****************************************************************************/
PHP_MINIT_FUNCTION(grt)
{
  //Init
  return SUCCESS;
}

/**
 ****************************************************************************
 * @brief Finalize function of the GRT callback module
 *
 *   This function is called when the PHP engine is shut down
 *****************************************************************************/
void php_grt_module_shutdown()
{
}

/**
 ****************************************************************************
 * @brief Grt callback function to execute another Grt function
 *
 *   Calls the given function and returns the result as XML string
 *****************************************************************************/
PHP_FUNCTION(grtCallGrtFunction)
{
  zval **myx_grt_pointer;
  zval **module;
  zval **function_name;
  zval **arguments;
  MYX_GRT *grt;
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *result= NULL;
  const char *c_str_module;
  const char *c_str_function_name;
  const char *c_str_arguments= NULL;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 4 || 
    zend_get_parameters_ex(4, &myx_grt_pointer, &module, &function_name, &arguments) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get parameters as strings
  if (Z_TYPE_PP(module) == IS_STRING)
    c_str_module= Z_STRVAL_PP(module);

  if (Z_TYPE_PP(function_name) == IS_STRING)
    c_str_function_name= Z_STRVAL_PP(function_name);

  if (Z_TYPE_PP(arguments) == IS_STRING)
    c_str_arguments= Z_STRVAL_PP(arguments);
    

  if (c_str_module && c_str_function_name)
  {
    // get pointer to grt
    convert_to_long_ex(myx_grt_pointer);
    grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

    // do the actual function call
    result= myx_grt_function_get_and_call(grt, c_str_module, c_str_function_name, 0, 
      myx_grt_value_from_xml_global_object(grt, c_str_arguments, strlen(c_str_arguments)), 
      &error);

    if (error != MYX_GRT_NO_ERROR)
    {
      // return an error
      php_error_docref(NULL TSRMLS_CC, E_WARNING, "the function returned and error %d", error);
    }

    // return result as XML string
	  Z_STRVAL_P(return_value) = myx_grt_value_to_xml(grt, result);
	  Z_STRLEN_P(return_value) = (int) strlen(Z_STRVAL_P(return_value));
	  Z_TYPE_P(return_value) = IS_STRING;
  }
}

/**
 ****************************************************************************
 * @brief Grt callback function to get a global object by _id
 *
 *   Searches for the given global Grt object with the _id and returns it in
 * a global Php variable
 *****************************************************************************/
PHP_FUNCTION(grtGetGrtGlobalById)
{
  zval **myx_grt_pointer;
  zval **id;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value= NULL;
  const char *c_str_id;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &id) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(id) == IS_STRING)
    c_str_id= Z_STRVAL_PP(id);

  if (c_str_id)
  {
    MYX_GRT_VALUE *value= myx_grt_reference_cache_lookup(grt, c_str_id);

    php_global_object_from_grt_value(grt, myx_grt_dict_get_object_path(value), value);
  }
}

/**
 ****************************************************************************
 * @brief Grt callback function to get a global Grt object as a Php object
 *
 *   Returns the global Grt object defined by the objectPath as a 
 * Php object
 *****************************************************************************/
PHP_FUNCTION(getGrtGlobalAsObject)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value= NULL;
  const char *c_str_object_path;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &objectPath) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  //convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);

  //if the value doesn't exist yet, create it
  if (!value)
  {
    MYX_GRT_VALUE *parent_value;
    char *parent_path= g_strdup(c_str_object_path);
    char *ptr= strrchr(parent_path, '/'); 
    char *member_name= g_strdup(ptr+1);
    *ptr= 0;

    //fetch parent value
    parent_value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), parent_path);

    //if the parent is a dict
    if ((parent_value) && (myx_grt_value_get_type(parent_value) == MYX_DICT_VALUE))
    {
      //get the assigned struct
      MYX_GRT_STRUCT *parent_struct= myx_grt_dict_struct_get(grt, parent_value);

      if (parent_struct)
      {
        //get the struct's member with the correct name
        MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_name(grt, parent_struct, member_name, 1);

        //create a list with the correct content_type and content_struct_name
        if ((member) && (member->value_type == MYX_LIST_VALUE))
        {
          if (member->overrides)
            value= myx_grt_list_new(member->content_type, member->overrides);
          else
            value= myx_grt_list_new(member->content_type, member->content_struct_name);
          myx_grt_dict_item_set_value(parent_value, member_name, value);
        }
      }
    }

    g_free(parent_path);
    g_free(member_name);
  }

  php_global_object_from_grt_value(grt, c_str_object_path, value);
}

PHP_FUNCTION(setGrtGlobalFromObject)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value= NULL;
  const char *c_str_object_path;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &objectPath) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  value= php_object_to_global_grt_value(grt, "com_mysql_grt_global_object", 0);

  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), c_str_object_path, value);
}

PHP_FUNCTION(getGrtGlobalListSize)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value= NULL;
  const char *c_str_object_path;
  int list_size;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &objectPath) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);

  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
    list_size= myx_grt_list_item_count(value);
  else
    list_size= -1;

  RETURN_LONG (list_size);
}

PHP_FUNCTION(getGrtGlobalListItem)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  zval **index;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value= NULL;
  const char *c_str_object_path;
  int c_index= -1;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 3 || 
    zend_get_parameters_ex(3, &myx_grt_pointer, &objectPath, &index) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  if (Z_TYPE_PP(index) == IS_LONG)
    c_index= Z_LVAL_PP(index);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);

  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
  {
    MYX_GRT_VALUE *list_item= myx_grt_list_item_get(value, c_index);
    char *item_path= g_strdup_printf("%s/%d", c_str_object_path, c_index);

    php_global_object_from_grt_value(grt, item_path, list_item);
    g_free(item_path);
  }
}

PHP_FUNCTION(addGrtGlobalListItem)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE *list_item;
  const char *c_str_object_path;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &objectPath) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);
  list_item= php_object_to_global_grt_value(grt, "com_mysql_grt_global_object", 0);

  if ( (value) && (list_item) )
    myx_grt_list_item_add(value, list_item);
}

PHP_FUNCTION(removeGrtGlobalListItem)
{

}

// ------------------------------------------------------------------------------------------------------------------
// Dict
//

PHP_FUNCTION(getGrtGlobalDictItem)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  zval **key;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value;
  const char *c_str_object_path;
  const char *c_str_key;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 3 || 
    zend_get_parameters_ex(3, &myx_grt_pointer, &objectPath, &key) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  if (Z_TYPE_PP(key) == IS_STRING)
    c_str_key= Z_STRVAL_PP(key);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);

  if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
  {
    char *value_path= g_strdup_printf("%s/%s", c_str_object_path, c_str_key);
    MYX_GRT_VALUE *dict_value= myx_grt_dict_item_get_value(value, c_str_key);

    php_global_object_from_grt_value(grt, value_path, dict_value);
  }
}

PHP_FUNCTION(addGrtGlobalDictItem)
{
  zval **myx_grt_pointer;
  zval **objectPath;
  zval **key;
  MYX_GRT *grt;
  MYX_GRT_VALUE *value;
  MYX_GRT_VALUE *dict_item;
  const char *c_str_object_path;
  const char *c_str_key;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 3 || 
    zend_get_parameters_ex(3, &myx_grt_pointer, &objectPath, &key) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(objectPath) == IS_STRING)
    c_str_object_path= Z_STRVAL_PP(objectPath);

  if (Z_TYPE_PP(key) == IS_STRING)
    c_str_key= Z_STRVAL_PP(key);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_object_path);
  dict_item= php_object_to_global_grt_value(grt, "com_mysql_grt_global_object", 0);

  if (value)
    myx_grt_dict_item_set_value(value, c_str_key, dict_item);
}

PHP_FUNCTION(removeGrtGlobalDictItem)
{

}

PHP_FUNCTION(processMessages)
{
  zval **myx_grt_pointer;
  zval **xml;
  MYX_GRT *grt;
  const char *c_str_xml;
  int res= 0;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 2 || 
    zend_get_parameters_ex(2, &myx_grt_pointer, &xml) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  // get parameters as strings
  if (Z_TYPE_PP(xml) == IS_STRING)
    c_str_xml= Z_STRVAL_PP(xml);

  if (grt->process_messages)
  {
    MYX_GRT_VALUE *value= myx_grt_value_from_xml(NULL, c_str_xml, strlen(c_str_xml));

    // convert the Grt values to Grt messages
    MYX_GRT_MSGS *msgs= myx_grt_messages_convert(value);

    // call the callback function to process the messages
    grt->process_messages(msgs, grt->process_messages_data);

    myx_grt_messages_free(msgs);

    myx_grt_value_release(value);
  }

  //RETURN_LONG (res);
}

PHP_FUNCTION(processStatusQuery)
{
  zval **myx_grt_pointer;
  MYX_GRT *grt;
  int res= 0;

  // fetch parameters
  if (ZEND_NUM_ARGS() != 1 || 
    zend_get_parameters_ex(1, &myx_grt_pointer) == FAILURE) {
		WRONG_PARAM_COUNT;
	}

  // get pointer to grt
  convert_to_long_ex(myx_grt_pointer);
  grt= LongToPtr(Z_LVAL_PP(myx_grt_pointer));

  if (grt->process_status_query)
  {
    // call the callback function to process the messages
    res= grt->process_status_query(grt->process_status_query_data);
  }

  RETURN_LONG (res);
}

function_entry php_grt_functions[] = {
  PHP_FE(grtCallGrtFunction, NULL)
  PHP_FE(grtGetGrtGlobalById, NULL)
  PHP_FE(getGrtGlobalAsObject, NULL)
  PHP_FE(setGrtGlobalFromObject, NULL)
  PHP_FE(getGrtGlobalListSize, NULL)
  PHP_FE(getGrtGlobalListItem, NULL)
  PHP_FE(addGrtGlobalListItem, NULL)
  PHP_FE(removeGrtGlobalListItem, NULL)
  PHP_FE(getGrtGlobalDictItem, NULL)
  PHP_FE(addGrtGlobalDictItem, NULL)
  PHP_FE(removeGrtGlobalDictItem, NULL)
  PHP_FE(processMessages, NULL)
  { NULL, NULL, NULL }
};

zend_module_entry php_grt_module_entry = {
  STANDARD_MODULE_HEADER,
  APPLNAME,
  php_grt_functions,
  PHP_MINIT(grt),
  NULL,
  NULL,
  NULL,
  NULL,
  PHP_GRT_VERSION,
  STANDARD_MODULE_PROPERTIES
};


/**
 ****************************************************************************
 * @brief Initializes the PHP loader
 *
 *   Initializes the PHP engine and registers the grt callback module
 *
 * @return  returns 0 if engine was initialized correctly
 *****************************************************************************/
MYX_GRT_MODULE_LOADER *myx_php_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  MYX_PHP_LOADER *priv= g_new0(MYX_PHP_LOADER, 1);
  
  static char *file_extensions[]= {
    ".php"
  };

  static char *argv[2] = {APPLNAME, NULL};

  *error= MYX_GRT_NO_ERROR;

  loader->grt= grt;
  loader->loader_type= MYX_PHP_MODULE_TYPE;
  loader->priv= priv;
  loader->init_module= php_init_module;
  loader->call_function= php_call_function;
  loader->extensions_num= 1;
  loader->extensions= file_extensions;

  // override PHP/ZE output
  php_embed_module.ub_write= php_ub_write;
  php_embed_module.log_message= php_log_message;
  php_embed_module.sapi_error= php_sapi_error;
  php_embed_module.php_ini_path_override = "php/php.ini";

  if (php_embed_init(1, argv PTSRMLS_CC) == FAILURE) 
    goto error;

  // set ini values
  php_set_ini_entry("html_errors", "0", PHP_INI_STAGE_ACTIVATE);
  php_set_ini_entry("display_errors", "1", PHP_INI_STAGE_ACTIVATE);
  php_set_ini_entry("display_startup_errors", "1", PHP_INI_STAGE_ACTIVATE);
  php_set_ini_entry("max_execution_time", "0", PHP_INI_STAGE_ACTIVATE);

  // register the callback functions
  zend_startup_module(&php_grt_module_entry);

  // set the current directory
  php_eval_string("chdir('./php')");

  // include the core PHP classes
  php_eval_string("include 'GrtInit.php'");

  // set MyxGrtPointer
  php_eval_string("Grt::getInstance()->setMyxGrtPointer(%d)", PtrToLong(grt));

  // set callback class
  php_eval_string("Grt::getInstance()->setCallback('GrtCallbackNative', '')");

  return loader;
  
error:
  *error= MYX_GRT_MODULE_INIT_ERROR;
//errorfree:
  g_free(priv);
  g_free(loader);
  return NULL;
}

/**
 ****************************************************************************
 * @brief Finalizes the PHP engine
 *
 *   The php_grt_module_shutdown function is called explicitly because
 * there are known issues with using the MSHUTDOWN function
 *****************************************************************************/
static void finialize_php(void)
{
  php_grt_module_shutdown();

  php_embed_shutdown(TSRMLS_C);
}

/**
 ****************************************************************************
 * @brief Modifies an PHP ini setting
 *
 *   Modifies the given entry
 *
 * @param entry
 *           name of the entry to modify
 * @param value  
 *           new value to which the entry should be set
 * @param stage  
 *           the stage
 * 
 * @return  returns SUCCESS if the string was processed correctly.
 *****************************************************************************/
static int php_set_ini_entry(char *entry, char *value, int stage)
{
  int retval;

  retval = zend_alter_ini_entry(entry, (uint)strlen(entry)+1, value, (uint)strlen(value)+1,
    PHP_INI_USER, stage);

  return retval;
}

/**
 ****************************************************************************
 * @brief Captures PHP output
 *
 *   This function captures PHP output so that it is not written to stdout
 *
 * @param str
 *           string that was written out
 * @param str_length  
 *           length of the string that was written out
 * 
 * @return  returns SUCCESS if the string was processed correctly.
 *****************************************************************************/
static int php_ub_write(const char *str, unsigned int str_length TSRMLS_DC)
{
  printf("%.*s\n", str_length, str);
  return str_length;
}

/**
 ****************************************************************************
 * @brief Captures PHP log message
 *
 *   This function captures PHP log message so that it is not written to stdout
 *
 * @param message
 *           message that was written out
 *****************************************************************************/
static void php_log_message(char *message)
{
  printf("log_message: %s\n", message);
}

/**
 ****************************************************************************
 * @brief Captures PHP SAPI error message
 *
 *   This function captures PHP SAPI error message so that it is not written 
 * to stdout
 *
 * @param type
 *           type of the message
 * @param fmt
 *           the error string
 *****************************************************************************/
static void php_sapi_error(int type, const char* fmt, ...)
{
  printf("sapi_error: %d, %s\n", type, fmt);
}

/**
 ****************************************************************************
 * @brief Executes one line of PHP code and returns the result as string
 *
 *   This function is called to execute one line of PHP code.
 *
 * @param appl_name
 *           name of the application, this is only used for error messages
 * @param fmt  
 *           a line of PHP code, can contain printf typical output format 
 *           definitions like %d or %s
 * @param ...  
 *           optional arguments to that will substitute the output format 
 *           definitions in fmt
 * 
 * @return  returns a string if the executed code returned a string. 
 *          Otherwise NULL is returned
 *****************************************************************************/
static char * php_eval_string(const char *fmt, ...)
{
  enum eval_ret_t retval;
  char *data= NULL;
  zval *zv= (zval *)malloc(sizeof(zval));
  char *res= NULL;

  va_list ap;
  va_start(ap, fmt);

  zend_first_try {
    vspprintf(&data, 0, fmt, ap);

    retval = zend_eval_string(data, zv, (char*)APPLNAME TSRMLS_CC) == SUCCESS ? EVAL_RET_OK : EVAL_RET_ERROR;

    //check if the returned var is a string
    //if so, return it
    if ((retval == EVAL_RET_OK) && (Z_TYPE_P(zv) == IS_STRING))
        res= g_strdup(Z_STRVAL_P(zv));
  } zend_catch {
    //retval = EVAL_RET_BAIL;
  } zend_end_try();

  if (data)
    efree(data);

  va_end(ap);

  return res;
}

static long php_eval_long(const char *fmt, ...)
{
  enum eval_ret_t retval;
  char *data= NULL;
  zval *zv= (zval *)malloc(sizeof(zval));
  long res= 0;

  va_list ap;
  va_start(ap, fmt);

  zend_first_try {
    vspprintf(&data, 0, fmt, ap);

    retval = zend_eval_string(data, zv, (char*)APPLNAME TSRMLS_CC) == SUCCESS ? EVAL_RET_OK : EVAL_RET_ERROR;

    //check if the returned var is a string
    //if so, return it
    if ((retval == EVAL_RET_OK) && ((Z_TYPE_P(zv) == IS_LONG) || (Z_TYPE_P(zv) == IS_BOOL)))
        res= Z_LVAL_P(zv);
  } zend_catch {
    //retval = EVAL_RET_BAIL;
  } zend_end_try();

  if (data)
    efree(data);

  va_end(ap);

  return res;
}

static double php_eval_real(const char *fmt, ...)
{
  enum eval_ret_t retval;
  char *data= NULL;
  zval *zv= (zval *)malloc(sizeof(zval));
  double res= 0;

  va_list ap;
  va_start(ap, fmt);

  zend_first_try {
    vspprintf(&data, 0, fmt, ap);

    retval = zend_eval_string(data, zv, (char*)APPLNAME TSRMLS_CC) == SUCCESS ? EVAL_RET_OK : EVAL_RET_ERROR;

    //check if the returned var is a string
    //if so, return it
    if ((retval == EVAL_RET_OK) && (Z_TYPE_P(zv) == IS_DOUBLE) )
        res= Z_DVAL_P(zv);
  } zend_catch {
    //retval = EVAL_RET_BAIL;
  } zend_end_try();

  if (data)
    efree(data);

  va_end(ap);

  return res;
}



static MYX_GRT_ERROR php_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule)
{
  MYX_PHP_LOADER *jloader= loader->priv;
  MYX_GRT_MODULE *module;
  MYX_PHP_MODULE *pmodule;
  char *class_name;
  char *xml;
  char *module_name= NULL;
  MYX_GRT_VALUE *module_functions= NULL;
  char *extends= NULL;
  char *ptr;
  unsigned int i;
  const char *module_filename= file+6; //remove ./php

  // call class::getModuleinfo();
  class_name = g_path_get_basename(file);
  
  ptr= strchr(class_name, '.');
  if (ptr)
    *ptr= 0;

  ptr= class_name + strlen(class_name) - 4;

  if ( (class_name) && (strlen(class_name) > 4) && 
      (strcmp2(ptr, "Test") == 0))
      return MYX_GRT_BAD_MODULE;

  // include the file
  php_eval_string("include '%s'", module_filename);

  xml= php_eval_string("%s::getModuleInfo();", class_name);

  if (xml)
  {
    MYX_GRT_VALUE *grt_info= myx_grt_value_from_xml(NULL, xml, strlen(xml));

    if (!grt_info || myx_grt_value_get_type(grt_info) != MYX_DICT_VALUE)
      g_warning("could not parse xml response data from %s",
                file);
    else
    {
      char *name_start= g_strrstr(myx_grt_dict_item_get_as_string(grt_info, "name"), ".");
      const char *extends= myx_grt_dict_item_get_as_string(grt_info, "extends");

      if (name_start)
        module_name= g_strdup(name_start+1);
      else
        module_name= g_strdup(myx_grt_dict_item_get_as_string(grt_info, "name"));

      module_functions= myx_grt_dict_item_get_value(grt_info, "functions");
      if (module_functions && myx_grt_value_get_type(module_functions)==MYX_LIST_VALUE)
        myx_grt_value_retain(module_functions);
      else
        module_functions= NULL;

      if ((extends) && (extends[0]))
        extends= g_strdup(extends);
    }

    if (!grt_info)
      return MYX_GRT_BAD_MODULE;

    myx_grt_value_release(grt_info);
  }
  else
  {
    // No exception handling needed here because it is handled
    // directly in j_call_static_method

    if (getenv("GRT_VERBOSE"))
      g_warning("Module %s doesn't implement getModuleInfo", file);
    return MYX_GRT_BAD_MODULE;
  }

  if (!module_name || !module_functions)
  {
    if (getenv("GRT_VERBOSE"))
    {
      if (!module_name)
        g_warning("Module info from %s doesn't contain 'name'", file);
      if (!module_functions)
        g_warning("Module info from %s doesn't contain 'functions'", file);
    }
    g_free(module_name);
    g_free(extends);
    if (module_functions)
      myx_grt_value_release(module_functions);

    return MYX_GRT_BAD_MODULE;
  }

  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);
  pmodule= g_new0(MYX_PHP_MODULE, 1);

  module->loader= loader;
  module->priv= pmodule;
  module->name= module_name;
  module->path= class_name;
  module->functions_num= myx_grt_list_item_count(module_functions);
  module->functions= g_new0(MYX_GRT_FUNCTION, module->functions_num);
  for (i= 0; i < module->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    MYX_PHP_FUNCTION *pfunc= g_new0(MYX_PHP_FUNCTION, 1);
    char *tmp= g_strdup(myx_grt_value_as_string(myx_grt_list_item_get(module_functions, i)));
    char *return_type;
    
    func->module= module;

    // do not use myx_grt_parse_function_spec here
    // since we need special handling for the php signature
    func->name= g_strdup(strtok(tmp, ":"));
    pfunc->php_signature= g_strdup(strtok(NULL, ":"));
    func->param_struct_name= NULL;
    return_type= strtok(NULL, ":");
    if ((return_type) && (return_type[0]))
      func->return_struct_name= g_strdup(return_type);
  
    func->priv= pfunc;

    g_free(tmp);
  }
  myx_grt_value_release(module_functions);
  module->extends= extends;

  // php specific module info
  pmodule->classname= g_strdup(class_name);


  *retmodule= module;
  
  if (getenv("GRT_VERBOSE"))
    g_message("Initialized module %s", class_name);

  return MYX_GRT_NO_ERROR;
}

static MYX_GRT_ERROR php_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_GRT *grt= function->module->loader->grt;
  MYX_PHP_MODULE *pmodule= function->module->priv;
  MYX_PHP_FUNCTION *pfunc= function->priv;
  char *xml;
  char *value_as_xml= myx_grt_value_to_xml(grt, value);

  //escape "
  if (value_as_xml)
    value_as_xml= str_g_replace(value_as_xml, "\"", "\\\"");

  xml= php_eval_string("Grt::callModuleFunction(\"%s\", \"%s\", \"%s\", \"%s\");", pmodule->classname, function->name,
    pfunc->php_signature, value_as_xml);

  if (xml)
    *retval= myx_grt_value_from_xml(grt, xml, strlen(xml));
  else
    *retval= NULL;

  g_free(value_as_xml);
  g_free(xml);

  return MYX_GRT_NO_ERROR;
}

//-----------------------------------------------------------------------------
// Private Stuff
//-----------------------------------------------------------------------------


#endif
