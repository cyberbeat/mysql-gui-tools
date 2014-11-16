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
 * @file myx_grt.c 
 * @brief GRT environment functions
 * 
 * See also: <a href="../grt.html#GRT">GRT</a>
 */

#include <glib.h>

#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

#include "myx_xml_util_functions.h"
#include "myx_grt_private.h"
#include "myx_shared_util_functions.h"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <objbase.h>
#elif defined(__APPLE__)
#include <CoreFoundation/CoreFoundation.h>
#else
// Linux specific
#include <uuid/uuid.h>
#endif


static MYX_GRT_VALUE *unserialize_from_xml(MYX_GRT *grt, xmlNodePtr node, GHashTable *objects_by_id);
static MYX_GRT_VALUE *unserialize_from_xml_global_object(MYX_GRT *grt, xmlNodePtr node, GHashTable *objects_by_id);
static xmlNodePtr serialize_to_xml(MYX_GRT *grt, xmlNodePtr parent, MYX_GRT_VALUE *value, GHashTable *saved_ids);

static int refcache_callback(MYX_GRT* grt, MYX_GRT_VALUE* value, MYX_GRT_VALUE_CALLBACK_REASON reason, void* data);
static void refcache_callback2(gpointer key, gpointer value, gpointer data);


static void default_print(const char *msg, void *data)
{
  printf("%s", msg);
}


static void default_log(MYX_GRT *grt, int code, const char *msg, const char *detail)
{
  printf("%i: %s: %s\n", code, msg, detail?detail:"");
}

void myx_grt_init_threads()
{
  g_thread_init(NULL);
}

/**
 **************************************************************************** 
 * @brief Returns the version of the GRT environment
 *
 * @return a const char with the version information
 ****************************************************************************/
const char * myx_grt_version()
{
  return GRT_VERSION;
}


/**
 **************************************************************************** 
 * @brief Creates a new GRT environment.
 *
 *  Also:
 *  - sets a default print callback that's equivalent to printf() and
 * registers the built-in module loader type.
 *  - creates a server socket so that modules can connect to it for
 * communicating with the grt (passing progress info, for example)
 *   After creating the environment, you will want to call individual
 * initializers for each module loader you want to support.
 *
 * @return A newly created GRT.
 ****************************************************************************/
MYX_GRT *myx_grt_initialize(int options)
{
  MYX_GRT *grt= g_new0(MYX_GRT, 1);

  g_static_rec_mutex_init(&grt->global_mutex);

  grt->options= options;
  
  grt->print= default_print;
  grt->print_data= NULL;
  
  grt->logfunc= default_log;
  
  grt->root= myx_grt_dict_new(NULL, NULL);

  grt->struct_icon_cache= g_hash_table_new_full(g_str_hash, g_str_equal,
                                                g_free, g_free);

  grt->global_reference_cache= g_hash_table_new_full(g_str_hash, g_str_equal,
                                                g_free, NULL);

  grt->direct_reference_cache= g_hash_table_new_full(g_str_hash, g_str_equal,
                                                g_free, NULL);
  
  myx_grt_register_module_loader(grt, myx_builtin_init_loader(grt));

#ifdef remove_this_when_this_gets_used
  myx_grt_setup_messaging(grt);
#endif
  return grt;
}


/**
 ****************************************************************************
 * @brief Shutdown a GRT environment and frees all used resources
 *
 *   Will close and free all module loaders, modules and other resources
 *   allocated for it.
 * 
 * @param grt the GRT environment
 *
 * NOTE
 *   This is not freeing anything atm.
 ****************************************************************************/
void myx_grt_finalize(MYX_GRT *grt)
{
  //XXX TODO

  // Remove all value listener registrations.
  g_hash_table_foreach(grt->global_reference_cache, refcache_callback2, NULL); 
  g_hash_table_destroy(grt->global_reference_cache);

  // Remove all value listener registrations.
  g_hash_table_foreach(grt->direct_reference_cache, refcache_callback2, NULL); 
  g_hash_table_destroy(grt->direct_reference_cache);
}


void glib_message_handler(const gchar *log_domain, GLogLevelFlags log_level, 
                          const gchar *message, gpointer user_data)
{
  myx_grt_messages_stack_add_message((MYX_GRT *) user_data, message, NULL, 0);
}

/**
 ****************************************************************************
 * @brief Activates or deactivates the catching of glib messages
 * 
 *  Sets the function that is used when GRT needs to print messages.
 *
 * @param grt      the GRT environment
 * @param user_data a pointer to be passed to the callback function. 
 * @param process_output_func the callback function which should take 
 *        the text and the userdata pointer as arguments
 ****************************************************************************/
void myx_grt_catch_glib_messages(MYX_GRT *grt, int do_catch)
{
  if (do_catch)
    g_log_set_handler(NULL, G_LOG_LEVEL_MASK, glib_message_handler, grt);
  else
    g_log_default_handler(NULL, G_LOG_LEVEL_MASK, "", NULL);
}

/**
 ****************************************************************************
 * @brief Sets the function to be used to print values
 * 
 *  Sets the function that is used when GRT needs to print messages.
 *
 * @param grt      the GRT environment
 * @param user_data a pointer to be passed to the callback function. 
 * @param process_output_func the callback function which should take 
 *        the text and the userdata pointer as arguments
 ****************************************************************************/
void myx_grt_set_output_callback(MYX_GRT *grt, void *user_data, MYX_GRT_PRINT_CALLBACK process_output_func)
{
  GRT_ENTER(grt);

  grt->print= process_output_func;
  grt->print_data= user_data;
  
  GRT_LEAVE(grt);
}

/**
 ****************************************************************************
 * @brief Sets the function to be used to process messages
 * 
 *  Sets the function that is used when GRT needs to process messages.
 *
 * @param grt      the GRT environment
 * @param user_data a pointer to be passed to the callback function. 
 * @param process_output_func the callback function which should take 
 *        the text and the userdata pointer as arguments
 ****************************************************************************/
void myx_grt_set_message_callback(MYX_GRT *grt, void *user_data, 
                                  MYX_GRT_MESSAGE_CALLBACK process_messages_func)
{
  GRT_ENTER(grt);

  grt->process_messages= process_messages_func;
  grt->process_messages_data= user_data;
  
  GRT_LEAVE(grt);
}

/**
 ****************************************************************************
 * @brief Sets the function to be used to process messages
 * 
 *  Sets the function that is used when GRT needs to process messages.
 *
 * @param grt      the GRT environment
 * @param user_data a pointer to be passed to the callback function. 
 * @param process_output_func the callback function which should take 
 *        the text and the userdata pointer as arguments
 ****************************************************************************/
void myx_grt_set_input_callback(MYX_GRT *grt, void *user_data,
                                MYX_GRT_INPUT_CALLBACK process_input_func)
{
  GRT_ENTER(grt);

  grt->process_input= process_input_func;
  grt->process_input_data= user_data;
  
  GRT_LEAVE(grt);
}

void myx_grt_set_status_query_callback(MYX_GRT *grt, void *user_data,
                                 int (*process_status_query_func)(void *user_data))
{
  GRT_ENTER(grt);

  grt->process_status_query= process_status_query_func;
  grt->process_status_query_data= user_data;
  
  GRT_LEAVE(grt);
}


/**
 ****************************************************************************
 * @brief Sets callbacks for module logging
 * 
 *  This will set the callbacks that will be called when a module calls
 * a logging function, for errors or messages in general.
 *
 * @param grt      the GRT environment
 * @param log_func the function that will handle logging calls from modules
 ****************************************************************************/
void myx_grt_module_set_log_callback(MYX_GRT *grt, MYX_GRT_LOG_CALLBACK log_func)
{
  GRT_ENTER(grt);
  grt->logfunc= log_func;
  GRT_LEAVE(grt);
}


/**
 ****************************************************************************
 * @brief Registers a module loader with the GRT
 * 
 * Will register an already initialized module loader with the GRT.
 * You can only register only one module loader of each type.
 * After registration, you can scan for modules with myx_grt_scan_for_modules
 * or load them individually with myx_grt_module_init
 * 
 * @param grt    the GRT environment where the loader should be registered
 * @param loader an initialized loader object. 
 *
 * @return MYX_GRT_NO_ERROR, MYX_GRT_INTERNAL_ERROR
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_register_module_loader(MYX_GRT *grt, MYX_GRT_MODULE_LOADER *loader)
{  
  g_return_val_if_fail(grt != NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(loader != NULL, MYX_GRT_INTERNAL_ERROR);

  GRT_ENTER(grt);

  grt->loaders_num++;
  grt->loaders= g_realloc(grt->loaders, sizeof(MYX_GRT_MODULE_LOADER*)*grt->loaders_num);

  grt->loaders[grt->loaders_num-1]= loader;

  GRT_RETURN(grt, MYX_GRT_NO_ERROR, MYX_GRT_ERROR);
}

/**
 ****************************************************************************
 * @brief Returns a copied list of the structs registered in the GRT
 * 
 * @param grt the GRT
 *
 * @return A copy of the registered structs
 ****************************************************************************/
MYX_GRT_STRUCTS * myx_grt_structs_get(MYX_GRT *grt)
{
  MYX_GRT_STRUCTS *structs= g_malloc(sizeof(MYX_GRT_STRUCTS));

  g_return_val_if_fail(grt != NULL, NULL);

  GRT_ENTER(grt);

  structs->structs_num= grt->structs_num;
  structs->structs= g_memdup(grt->structs, sizeof(MYX_GRT_STRUCT)*structs->structs_num);

  GRT_RETURN(grt, structs, MYX_GRT_STRUCTS*);
}

MYX_STRINGLIST * myx_grt_struct_packages(MYX_GRT *grt)
{
  unsigned int i;

  g_return_val_if_fail(grt != NULL, NULL);

  GRT_ENTER(grt);
  
  {
    MYX_STRINGLIST *packages= g_new0(MYX_STRINGLIST, 1);

    for (i= 0; i<grt->structs_num; i++)
    {
      MYX_GRT_STRUCT *str= grt->structs + i;
      char *str_pak= g_strdup(str->name);
      unsigned int j, l= (unsigned int)strlen(str_pak);

      for (j= l-1; j>0; j--)
      {
        if (str_pak[j] == '.')
          break;
      }
      str_pak[j]= 0;

      //See if the package name is already stored
      for (j= 0; j<packages->strings_num; j++)
      {
        if (strcmp3(str_pak, packages->strings[j]) == 0)
          break;
      }
      //If not, add the package name
      if (j == packages->strings_num)
      {
        packages->strings_num++;
        packages->strings= g_realloc(packages->strings, sizeof(char *)*packages->strings_num);
        packages->strings[packages->strings_num-1]= g_strdup(str_pak);
      }

      g_free(str_pak);
    }

    GRT_RETURN(grt, packages, MYX_STRINGLIST*);
  }
}

int myx_grt_package_count(MYX_GRT *grt)
{
  unsigned int c;
  MYX_STRINGLIST *packages;
  GRT_ENTER(grt);
  
  packages= myx_grt_struct_packages(grt);

  c= packages->strings_num;

  myx_free_stringlist(packages);

  GRT_RETURN(grt, c, int);
}

char * myx_grt_package_by_index(MYX_GRT *grt, unsigned int index)
{
  char *s;
  MYX_STRINGLIST *packages;
  
  GRT_ENTER(grt);

  packages= myx_grt_struct_packages(grt);

  s= g_strdup(packages->strings[index]);

  myx_free_stringlist(packages);

  GRT_RETURN(grt, s, char*);
}

/**
 ****************************************************************************
 * @brief Returns the number of structs registered in the GRT
 * 
 * @param grt the GRT
 *
 * @return The number of structs
 ****************************************************************************/
int myx_grt_struct_get_count(MYX_GRT *grt)
{
  g_return_val_if_fail(grt != NULL, -1);
  
  GRT_ENTER(grt);

  GRT_RETURN(grt, grt->structs_num, int);
}

/**
 ****************************************************************************
 * @brief Returns the number of structs registered in the GRT that have
 * the struct with the struct_name as their parent
 * 
 * @param grt the GRT
 * @param struct_name The name of the parent struct
 *
 * @return The number of structs
 ****************************************************************************/
int myx_grt_struct_get_child_count(MYX_GRT *grt, const char *struct_name)
{
  unsigned int i, c= 0;

  g_return_val_if_fail(grt != NULL, -1);
  
  GRT_ENTER(grt);

  for (i= 0; i<grt->structs_num; i++)
  {
    MYX_GRT_STRUCT *str= grt->structs + i;

    if (strcmp3(str->parent_struct_name, struct_name) == 0)
    {
      c++;
    }
  }

  GRT_RETURN(grt, c, int);
}

int myx_grt_package_struct_count(MYX_GRT *grt, const char *package_name)
{
  unsigned int i, c= 0, l= (unsigned int)strlen(package_name);

  g_return_val_if_fail(grt != NULL, 0);
  g_return_val_if_fail(package_name != NULL, 0);

  GRT_ENTER(grt);
  
  for (i= 0; i<grt->structs_num; i++)
  {
    MYX_GRT_STRUCT *str= grt->structs + i;   

    if (package_name[0])
    {
      char *begin_str= g_strdup_printf("%s.", package_name);

      if (str_beginswith(str->name, begin_str) &&
          (sub_str_count(".", str->name+l+1) == 0))
        c++;

      g_free(begin_str);
    }
    else
    {
      if (sub_str_count(".", str->name) == 0)
        c++;
    }
  }

  GRT_RETURN(grt, c, int);
}

/**
 ****************************************************************************
 * @brief Returns the GRT struct given by an index
 * 
 * @param grt the GRT
 * @param index the index of the requested struct
 * 
 * @return The struct with the given index
 ****************************************************************************/
MYX_GRT_STRUCT * myx_grt_struct_get_by_index(MYX_GRT *grt, unsigned int index)
{
  g_return_val_if_fail(grt != NULL, NULL);
  
  GRT_ENTER(grt);

  GRT_RETURN_VAL_IF_FAIL(grt, index < grt->structs_num, NULL);

  GRT_RETURN(grt, grt->structs + index, MYX_GRT_STRUCT*);
}

/**
 ****************************************************************************
 * @brief Returns the child GRT struct of the struct defined by struct_name 
 * with the given by an
 * 
 * @param grt the GRT
 * @param struct_name the name of the parent strcut
 * @param index the index of the requested struct
 * 
 * @return The struct with the given index or null if not found
 ****************************************************************************/
MYX_GRT_STRUCT * myx_grt_struct_get_child_by_index(MYX_GRT *grt, const char *struct_name, unsigned int index)
{
  unsigned int i, c= 0;

  g_return_val_if_fail(grt != NULL, NULL);
  
  GRT_ENTER(grt);

  for (i= 0; i<grt->structs_num; i++)
  {
    MYX_GRT_STRUCT *str= grt->structs + i;

    if (strcmp3(str->parent_struct_name, struct_name) == 0)
    {
      if (c == index)
        GRT_RETURN(grt, str, MYX_GRT_STRUCT*);

      c++;
    }
  }

  GRT_RETURN(grt, NULL, MYX_GRT_STRUCT*);
}

MYX_GRT_STRUCT * myx_grt_package_struct_by_index(MYX_GRT *grt, const char *package_name, unsigned int index)
{
  unsigned int i, c= 0, l= (unsigned int)strlen(package_name);

  g_return_val_if_fail(grt != NULL, NULL);

  GRT_ENTER(grt);
  
  for (i= 0; i<grt->structs_num; i++)
  {
    MYX_GRT_STRUCT *str= grt->structs + i;

    if (package_name[0])
    {
      char *begin_str= g_strdup_printf("%s.", package_name);

      if (str_beginswith(str->name, begin_str) &&
          (sub_str_count(".", str->name+l+1) == 0))
      {
        if (c == index)
        {
          g_free(begin_str);
          GRT_RETURN(grt, str, MYX_GRT_STRUCT*);
        }

        c++;
      }

      g_free(begin_str);
    }
    else
    {
      if (sub_str_count(".", str->name) == 0)
      {
        if (c == index)
          GRT_RETURN(grt, str, MYX_GRT_STRUCT*);
        
        c++;
      }
    }
  }

  GRT_RETURN(grt, NULL, MYX_GRT_STRUCT*);
}


/**
 ****************************************************************************
 * @brief Fetches the root object of the GRT
 * 
 * @param grt the GRT
 *
 * @return The root GRT value.
 ****************************************************************************/
MYX_GRT_VALUE *myx_grt_get_root(MYX_GRT *grt)
{
  g_return_val_if_fail(grt != NULL, NULL);

  GRT_ENTER(grt);
  
  GRT_RETURN(grt, grt->root, MYX_GRT_VALUE*);
}


/**
 ****************************************************************************
 * @brief "Loads" a value into the GRT and makes it the root object/value
 * 
 *   This will replace the root object of the GRT with the passed one.
 * The old object will be completely freed. 
 *
 * The GRT will take over ownership of the whole object passed. That means
 * you should not free it.
 *
 * @param grt the GRT environment
 * @param new_root a GRT object/value that will become the root object. It has to
 *    be a MYX_DICT_VALUE
 *
 * @return MYX_GRT_NO_ERROR if success
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_set_root(MYX_GRT *grt, MYX_GRT_VALUE *new_root)
{
  g_return_val_if_fail(grt != NULL, MYX_GRT_INTERNAL_ERROR);
  g_return_val_if_fail(new_root != NULL, MYX_GRT_INTERNAL_ERROR);

  GRT_ENTER(grt);
  
  if (myx_grt_value_get_type(new_root) != MYX_DICT_VALUE)
    GRT_RETURN(grt, MYX_GRT_BAD_VALUE, MYX_GRT_ERROR);

  myx_grt_reference_cache_clear(grt);
  myx_grt_value_release(grt->root);

  grt->root= new_root;

  myx_grt_value_retain(new_root);
  myx_grt_reference_cache_rescan(grt);
  
  GRT_RETURN(grt, MYX_GRT_NO_ERROR, MYX_GRT_ERROR);
}


/**
 ****************************************************************************
 * @brief Loads a previously stored GRT value from a file
 * 
 *   Loads a GRT value from a GRT XML file. Use myx_grt_store_to_file to
 * create files in that format.
 *   Read myx_grt_store_to_file for more details.
 *
 * @param filename the name of the file to load. The file must be in the GRT XML format.
 *     Usually something stored by myx_grt_store_to_file
 *
 * @return NULL on error and the value if the file could be correctly loaded.
 ****************************************************************************/
MYX_GRT_VALUE *myx_grt_retrieve_from_file(MYX_GRT *grt, const char *filename)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  MYX_GRT_VALUE *value;

  if (!(doc= myx_xmlParseFile(filename)))
    return NULL;

  root= xmlDocGetRootElement(doc);
  if (root)
  {
    root= root->children;
    while (root && xmlStrcmp(root->name, (xmlChar*)"value")!=0) root= root->next;
    if (root)
    {
      GHashTable *objects_by_id= g_hash_table_new(g_str_hash, g_str_equal);
      value= unserialize_from_xml(grt, root, objects_by_id);
      g_hash_table_destroy(objects_by_id);
    }
  }

  xmlFreeDoc(doc);

  return value;
}


/**
 ****************************************************************************
 * @brief Stores a GRT value to a file
 * 
 *   This will serialize the value to XML and store it in a file that can 
 * later be retrieved with myx_grt_retrieve_from_file
 *
 * @param  value the GRT value to store
 * @param  filename name of file to store data
 *
 * @return  MYX_GRT_NO_ERROR if there were no errors.
 ****************************************************************************/
MYX_GRT_ERROR myx_grt_store_to_file(MYX_GRT *grt, MYX_GRT_VALUE *value, const char *filename)
{
  xmlDocPtr doc;
  int res;
  GHashTable *saved_ids;
  
  doc= xmlNewDoc((xmlChar*)"1.0");
  doc->children= xmlNewDocRawNode(doc, NULL, (xmlChar*)"data", NULL);
  
  saved_ids= g_hash_table_new(g_str_hash, g_str_equal);
  
  serialize_to_xml(grt, doc->children, value, saved_ids);

  g_hash_table_destroy(saved_ids);
  
  res= myx_xmlSaveFile(filename, doc);

  xmlFreeDoc(doc);

  return res == -1 ? MYX_GRT_CANT_OPEN_FILE : MYX_GRT_NO_ERROR;
}


/*static char *value_type_strings[]=
{
  "all",
    "int",
    "bigint",
    "real",
    "string",
    "list",
    "dict"
};*/


/**
 ****************************************************************************
 * @brief Prints a string using the print callback from GRT
 *
 *  Prints a formated message using the callback function set up in GRT.
 *
 * @param grt the GRT environment. A print callback must have been previously set.
 * @param fmt format string, accepts anything that printf() does.
 * @param ... arguments for the formatted message
 *****************************************************************************/
int myx_grt_printf(MYX_GRT *grt, const char *fmt, ...)
{
  char *tmp;
  
  va_list args;
  va_start(args, fmt);

  GRT_ENTER(grt);
  
  tmp= g_strdup_vprintf(fmt, args);
  va_end(args);
  
  MYX_PRINT(grt, tmp);
  g_free(tmp);
  
  GRT_RETURN(grt, 0, int);
}



static xmlNodePtr serialize_to_xml(MYX_GRT *grt, xmlNodePtr parent, MYX_GRT_VALUE *value,
                                   GHashTable *saved_ids)
{
  MYX_GRT_STRUCT *gstruct;
  unsigned int i;
  char buffer[100];
  const char *id;
  xmlNodePtr node= NULL;

  switch (value->type)
  {
  case MYX_INT_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%i", value->value.i);
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)buffer);

    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"int");
    break;
  case MYX_REAL_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%f", value->value.r);
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)buffer);

    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"real");
    break;
  case MYX_STRING_VALUE:
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)value->value.s);

    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"string");
    break;
  case MYX_LIST_VALUE:
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", NULL);
    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"list");
    xmlNewProp(node, (xmlChar*)"content-type", (xmlChar*)myx_get_value_type_as_string(value->value.l->content_type));

    if (value->value.l->content_struct_name)
      xmlNewProp(node, (xmlChar*)"content-struct-name", (xmlChar*)value->value.l->content_struct_name);

    for (i= 0; i < value->value.l->items_num; i++)
    {
      g_return_val_if_fail((value->value.l->items[i]->type == value->value.l->content_type) ||
         (value->value.l->content_type == MYX_ANY_VALUE), NULL);
      serialize_to_xml(grt, node, value->value.l->items[i], saved_ids);
    }
    break;
  case MYX_DICT_VALUE:
    gstruct= NULL;
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", NULL);
    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"dict");

    if ((value->value.d->struct_name) && (value->value.d->struct_name[0]))
    {
      xmlNewProp(node, (xmlChar*)"struct-name", (xmlChar*)value->value.d->struct_name);
      gstruct= myx_grt_struct_get(grt, value->value.d->struct_name);
    }

    if (value->value.d->content_type != MYX_ANY_VALUE)
      xmlNewProp(node, (xmlChar*)"content-type", (xmlChar*)myx_get_value_type_as_string(value->value.d->content_type));

    if ((value->value.d->content_struct_name) && (value->value.d->content_struct_name[0]))
      xmlNewProp(node, (xmlChar*)"content-struct-name", (xmlChar*)value->value.d->content_struct_name);

    if ((id= myx_grt_dict_id_item_as_string(value)))
    {
      if (g_hash_table_lookup(saved_ids, id))
      {
        // the object is already in the XML tree, just leave a link to it
        xmlNewProp(node, (xmlChar*)"link", (xmlChar*)id);

        return node;
      }
      else
        // object is not yet in XML tree. put it in and record the ID
        g_hash_table_insert(saved_ids, (gpointer)id, "X");      
    }

    for (i= 0; i < value->value.d->items_num; i++)
    {
      xmlNodePtr child;
      
      if (value->value.d->items[i].value)
      {
        child= serialize_to_xml(grt, node, value->value.d->items[i].value, saved_ids);
        xmlNewProp(child, (xmlChar*)"key", (xmlChar*)value->value.d->items[i].key);
        
        if (gstruct)
        {
          MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_name(grt, gstruct, value->value.d->items[i].key, 1);

          if (member && member->is_ref)
            xmlNewProp(child, (xmlChar*)"option", (xmlChar*)"ref");
        }
      }
    }
    break;
  case MYX_ANY_VALUE:
    break;
  }
  return node;
}


static MYX_GRT_VALUE *unserialize_from_xml(MYX_GRT *grt, xmlNodePtr node, GHashTable *objects_by_id)
{
  xmlChar *str;
  xmlChar *node_type= xmlGetProp(node, (xmlChar*)"type");
  MYX_GRT_VALUE *value= NULL;
  
  if (!node_type)
  {
    g_warning("Node '%s' in xml doesn't have a type property", node->name);
    return NULL;
  }

  if (strcmp((char*)node_type, "int")==0)
  {
    str= xmlNodeGetContent(node);
    value= myx_grt_value_from_int(strtol((char*)str, NULL, 0));
    xmlFree(str);
  }
  else if (strcmp((char*)node_type, "real")==0)
  {
    str= xmlNodeGetContent(node);
    value= myx_grt_value_from_real(strtod((char*)str, NULL));
    xmlFree(str);
  }
  else if (strcmp((char*)node_type, "string")==0)
  {
    str= xmlNodeGetContent(node);
    value= myx_grt_value_from_string((char*)str);
    xmlFree(str);
  }
  else if (strcmp((char*)node_type, "dict")==0)
  {
    xmlChar *global_object_path= xmlGetProp(node, (xmlChar*)"global-object-path");
    xmlChar *link_id= xmlGetProp(node, (xmlChar*)"link");
    xmlNodePtr child;

    if (global_object_path)
    {
      xmlFree(link_id);
      return unserialize_from_xml_global_object(grt, node, objects_by_id);
    }
    xmlFree(global_object_path);

    if (link_id)
    {
      MYX_GRT_VALUE *linked_object;

      linked_object= g_hash_table_lookup(objects_by_id, link_id);
      if (!linked_object)
      {
        g_warning("linked object %s was not yet parsed at the time it was found",
                  link_id);
      }
      else
      {
        myx_grt_value_retain(linked_object);
      }

      value= linked_object;
      xmlFree(link_id);
    }
    else
    {
      xmlChar *prop= xmlGetProp(node, (xmlChar*)"content-type");
      MYX_GRT_VALUE_TYPE content_type= MYX_ANY_VALUE;
      MYX_GRT_ERROR error;

      content_type= myx_get_value_type_from_string((char*)prop, &error);
      xmlFree(prop);

      if (content_type != MYX_ANY_VALUE)
      {
        char *content_struct_name= (char*)xmlGetProp(node, (xmlChar*)"content-struct-name");

        value= myx_grt_dict_new_typed(content_type, content_struct_name);

        xmlFree(content_struct_name);
      }
      else
      {
        xmlChar *struct_name= xmlGetProp(node, (xmlChar*)"struct-name");

        // this does the work of myx_grt_dict_new()
        value= g_new0(MYX_GRT_VALUE, 1);
        value->type= MYX_DICT_VALUE;
        value->value.d= g_new0(MYX_GRT_DICT, 1);
        value->refcount= 1;
        value->value.d->struct_name= g_strdup((char*)struct_name);

        xmlFree(struct_name);
      }

      child= node->children;
      while (child)
      {
        MYX_GRT_VALUE *sub_value;
        
        if (strcmp((char*)child->name, "value")==0)
        {
          xmlChar *key= xmlGetProp(child, (xmlChar*)"key");

          sub_value= unserialize_from_xml(grt, child, objects_by_id);
          if (sub_value)
          {
            myx_grt_dict_item_set_value(value, (char*)key, sub_value);
            myx_grt_value_release(sub_value);
          }
          else
          {
            myx_grt_value_release(value);
            value= NULL;
            break;
          }

          if (xmlStrcmp(key, (xmlChar*)"_id")==0 && sub_value)
          {
            if (sub_value->type == MYX_STRING_VALUE)
              g_hash_table_insert(objects_by_id, (gpointer)myx_grt_value_as_string(sub_value),
                                  value);
          }

          xmlFree(key);
        }

        child= child->next;
      }
    }
  }
  else if (strcmp((char*)node_type, "list")==0)
  {
    xmlChar *ctype= xmlGetProp(node, (xmlChar*)"content-type");
    xmlChar *cstruct_name= xmlGetProp(node, (xmlChar*)"content-struct-name");
    xmlNodePtr child;
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE_TYPE content_type= myx_get_value_type_from_string((char*)ctype, &error);

    /*for (i= 0; sizeof(value_type_strings)/sizeof(char*); i++)
      if (strcmp2(value_type_strings[i], ctype)==0)
      {
        value= myx_grt_list_new(i, NULL);
        break;
      }*/

    if (error == MYX_GRT_NO_ERROR)
    {
      // this does the work of myx_grt_list_new()
      value= g_new0(MYX_GRT_VALUE, 1);
      value->type= MYX_LIST_VALUE;
      value->value.l= g_new0(MYX_GRT_LIST, 1);
      value->value.l->content_type= content_type;
      value->refcount= 1;
      value->value.l->content_struct_name= g_strdup((char*)cstruct_name);

      child= node->children;
      while (child)
      {
        MYX_GRT_VALUE *sub_value;

        if (child->type == XML_ELEMENT_NODE && strcmp((char*)child->name, "value")==0)
        {
          sub_value= unserialize_from_xml(grt, child, objects_by_id);
          if (sub_value)
          {
            myx_grt_list_item_add(value, sub_value);
            myx_grt_value_release(sub_value);
          }
          else
          {
            myx_grt_value_release(value);
            value= NULL;
            break;
          }
        }

        child= child->next;
      }

      xmlFree(ctype);
      xmlFree(cstruct_name);
    }
  }
  xmlFree(node_type);

  return value;
}



static xmlNodePtr serialize_to_xml_global_object(const char *objectPath, xmlNodePtr parent, MYX_GRT_VALUE *value,
                                   GHashTable *saved_ids)
{
  //unsigned int i;
  char buffer[100];
  //const char *id;
  xmlNodePtr node;

  switch (value->type)
  {
  case MYX_INT_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%i", value->value.i);
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)buffer);
    
    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"int");
    xmlNewProp(node, (xmlChar*)"globalObjectPath", (xmlChar*)objectPath);
    break;
  case MYX_REAL_VALUE:
    g_snprintf(buffer, sizeof(buffer), "%f", value->value.r);
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)buffer);

    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"real");

    xmlNewProp(node, (xmlChar*)"global-object-path", (xmlChar*)objectPath);
    break;
  case MYX_STRING_VALUE:
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", (xmlChar*)value->value.s);

    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"string");
    xmlNewProp(node, (xmlChar*)"global-object-path", (xmlChar*)objectPath);
    break;
  case MYX_LIST_VALUE:
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", NULL);
    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"list");
    xmlNewProp(node, (xmlChar*)"content-type", (xmlChar*)myx_get_value_type_as_string(value->value.l->content_type));

    if (value->value.l->content_struct_name)
      xmlNewProp(node, (xmlChar*)"content-struct-name", (xmlChar*)value->value.l->content_struct_name);

    /*for (i= 0; i < value->value.l->items_num; i++)
    {
      g_return_val_if_fail((value->value.l->items[i]->type == value->value.l->content_type) ||
         (value->value.l->content_type == MYX_ANY_VALUE), NULL);
      serialize_to_xml(node, value->value.l->items[i], saved_ids);
    }*/
    xmlNewProp(node, (xmlChar*)"global-object-path", (xmlChar*)objectPath);
    break;
  case MYX_DICT_VALUE:
    node= xmlNewTextChild(parent, NULL, (xmlChar*)"value", NULL);
    xmlNewProp(node, (xmlChar*)"type", (xmlChar*)"dict");

    if ((value->value.d->struct_name) && (value->value.d->struct_name[0]))
      xmlNewProp(node, (xmlChar*)"struct-name", (xmlChar*)value->value.d->struct_name);

    if (value->value.d->content_type != MYX_ANY_VALUE)
      xmlNewProp(node, (xmlChar*)"content-type", (xmlChar*)myx_get_value_type_as_string(value->value.d->content_type));

    if ((value->value.d->content_struct_name) && (value->value.d->content_struct_name[0]))
      xmlNewProp(node, (xmlChar*)"content-struct-name", (xmlChar*)value->value.d->content_struct_name);

    /*if ((id= myx_grt_dict_id_item_as_string(value)))
    {
      if (g_hash_table_lookup(saved_ids, id))
      {
        // the object is already in the XML tree, just leave a link to it
        xmlNewProp(node, "link", id);

        return node;
      }
      else
        // object is not yet in XML tree. put it in and record the ID
        g_hash_table_insert(saved_ids, (gpointer)id, "X");      
    }

    for (i= 0; i < value->value.d->items_num; i++)
    {
      xmlNodePtr child;
      
      child= serialize_to_xml(node, value->value.d->items[i].value, saved_ids);
      xmlNewProp(child, "key", value->value.d->items[i].key);
    }*/
    xmlNewProp(node, (xmlChar*)"global-object-path", (xmlChar*)objectPath);
    break;
  case MYX_ANY_VALUE:
    break;
  }
  return node;
}

static MYX_GRT_VALUE *unserialize_from_xml_global_object(MYX_GRT *grt, xmlNodePtr node, GHashTable *objects_by_id)
{
  char *node_type= (char*)xmlGetProp(node, (xmlChar*)"type");
  MYX_GRT_VALUE *value= NULL;

  if (!node_type)
  {
    g_warning("Node '%s' in xml doesn't have a type property", node->name);
    return NULL;
  }

  GRT_ENTER(grt);
  
  // check if we have a global object
  if ((strcmp(node_type, "dict")==0) || (strcmp(node_type, "list")==0))
  {
    xmlChar *global_object_path= xmlGetProp(node, (xmlChar*)"global-object-path");

    if (global_object_path)
    {
      value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), (char*)global_object_path);
      xmlFree(global_object_path);
      GRT_RETURN(grt, value, MYX_GRT_VALUE*);
    }
  }

  GRT_RETURN(grt, unserialize_from_xml(grt, node, objects_by_id), MYX_GRT_VALUE*);
}

/**
 ****************************************************************************
 * @brief Convert a GRT value into a XML string
 *
 * Produces a XML representation of the GRT value.
 * 
 * @param value a GRT value
 *
 * @return String containing the value as a XML or NULL if there's an error.
 * 
 * @see myx_grt_value_from_xml
 *****************************************************************************/
char *myx_grt_value_to_xml(MYX_GRT *grt, MYX_GRT_VALUE *value)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  xmlChar *buffer= NULL;
  GHashTable *saved_ids;
  int size;
  
  if (value)
  {
    doc= xmlNewDoc((xmlChar*)"1.0");
    doc->children= root= xmlNewDocRawNode(doc, NULL, (xmlChar*)"data", NULL);
    
    saved_ids= g_hash_table_new(g_str_hash, g_str_equal);
    
    serialize_to_xml(grt, root, value, saved_ids);
    
    g_hash_table_destroy(saved_ids);

    xmlDocDumpFormatMemory(doc, &buffer, &size, 1);

    xmlFreeDoc(doc);

    return (char*)buffer;
  }
  else
    return NULL;
}


/**
 ****************************************************************************
 * @brief Parse a XML representation of a GRT value.
 *
 * Parses a XML string and rebuilds the GRT value that corresponds to it.
 *
 * @param str the string in XML format containing a serialized GRT value
 * @param size length of the string
 *
 * @return The value corresponding to the passed in string or NULL if there's
 *   an error.
 * 
 * @see myx_grt_value_to_xml
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_value_from_xml(MYX_GRT *grt, const char *str, size_t size)
{
  xmlDocPtr doc= xmlParseMemory(str, (int)size);
  xmlNodePtr root;
  MYX_GRT_VALUE *value;

  if (!doc)
  {
    xmlErrorPtr error= xmlGetLastError();

    if (error) 
    {
      myx_grt_messages_stack_add_error(grt, "Could not parse XML data. Line %d, %s", NULL, 0, 
        error->line, error->message);
    }
    else
    {
      g_warning("Could not parse XML data");
    }
    return NULL;
  }

  root= xmlDocGetRootElement(doc);

  //check if we have a <data> rootnode
  if (root && (xmlStrcmp(root->name, (xmlChar*)"data")==0))
    root= root->children;

  //skip all nodes that are no <value> nodes
  while (root && xmlStrcmp(root->name, (xmlChar*)"value")!=0) 
    root= root->next;

  if (root)
  {
    GHashTable *objects_by_id= g_hash_table_new(g_str_hash, g_str_equal);
    value= unserialize_from_xml(grt, root, objects_by_id);
    g_hash_table_destroy(objects_by_id);
  }
  else
    value= NULL;

  xmlFreeDoc(doc);

  return value;
}

/**
 ****************************************************************************
 * @brief Parse a XML representation of a GRT value that might be a global object.
 *
 * Parses a XML string and rebuilds the GRT value that corresponds to it.
 *
 * @param grt the GRT environment
 * @param str the string in XML format containing a serialized GRT value
 * @param size length of the string
 *
 * @return The value corresponding to the passed in string or NULL if there's
 *   an error.
 * 
 * @see myx_grt_value_to_xml
 *****************************************************************************/
MYX_GRT_VALUE *myx_grt_value_from_xml_global_object(MYX_GRT *grt, const char *str, size_t size)
{
  xmlDocPtr doc= xmlParseMemory(str, (int)size);
  xmlNodePtr root;
  MYX_GRT_VALUE *value;

  if (!doc)
  {
    g_warning("Could not parse XML data");
    return NULL;
  }

  GRT_ENTER(grt);
  
  root= xmlDocGetRootElement(doc);

  //check if we have a <data> rootnode
  if (root && (xmlStrcmp(root->name, (xmlChar*)"data")==0))
    root= root->children;

  //skip all nodes that are no <value> nodes
  while (root && xmlStrcmp(root->name, (xmlChar*)"value")!=0) 
    root= root->next;

  if (root)
  {
    GHashTable *objects_by_id= g_hash_table_new(g_str_hash, g_str_equal);
    value= unserialize_from_xml_global_object(grt, root, objects_by_id);
    g_hash_table_destroy(objects_by_id);
  }
  else
    value= NULL;

  xmlFreeDoc(doc);

  GRT_RETURN(grt, value, MYX_GRT_VALUE*);
}

/**
 ****************************************************************************
 * @brief Convert a GRT value into a XML global object string
 *
 * Produces a XML representation of the GRT value as a global object
 * 
 * @param value a GRT value
 *
 * @return String containing the value as a XML or NULL if there's an error.
 * 
 * @see myx_grt_value_to_xml
 *****************************************************************************/
char *myx_grt_value_to_xml_global_object(const char *objectPath, MYX_GRT_VALUE *value)
{
  xmlDocPtr doc;
  xmlNodePtr root;
  xmlChar *buffer= NULL;
  GHashTable *saved_ids;
  int size;
  
  if (value)
  {
    doc= xmlNewDoc((xmlChar*)"1.0");
    doc->children= root= xmlNewDocRawNode(doc, NULL, (xmlChar*)"data", NULL);
    
    saved_ids= g_hash_table_new(g_str_hash, g_str_equal);
    
    serialize_to_xml_global_object(objectPath, root, value, saved_ids);
    
    g_hash_table_destroy(saved_ids);

    xmlDocDumpFormatMemory(doc, &buffer, &size, 1);

    xmlFreeDoc(doc);

    return (char*)buffer;
  }
  else
    return NULL;
}


/** 
 ****************************************************************************
 * @brief Converts messages that are stored in a GRT value to a GRT 
 * message list
 *
 * @param msgs_list  the GRT list value holding the messages
 *
 * @return Converted messages or NULL. The returned struct must be freed 
 * with myx_grt_messages_free()
 *****************************************************************************/
MYX_GRT_MSGS * myx_grt_messages_convert(MYX_GRT_VALUE *msgs_list)
{
  MYX_GRT_MSGS *msgs= NULL;

  if (msgs_list)
  {
    unsigned int i;
    msgs= g_new0(MYX_GRT_MSGS, 1);
    msgs->msgs_num= myx_grt_list_item_count(msgs_list);
    msgs->msgs= g_new0(MYX_GRT_MSG, msgs->msgs_num);

    for (i= 0; i < msgs->msgs_num; i++)
    {
      MYX_GRT_MSG *msg= msgs->msgs + i;
      MYX_GRT_VALUE *msg_value= myx_grt_list_item_get(msgs_list, i);
      MYX_GRT_VALUE *msg_detail_value= myx_grt_dict_item_get_value(msg_value, "details");

      msg->msg= g_strdup(myx_grt_dict_item_get_as_string(msg_value, "msg"));
      msg->msg_type= myx_grt_dict_item_get_as_int(msg_value, "msgType");
      msg->progress= myx_grt_dict_item_get_as_int(msg_value, "progress");

      if (msg_detail_value)
      {
        int count= myx_grt_list_item_count(msg_detail_value);

        if (count > 0)
        {
          int j;

          msg->msg_detail= g_new0(MYX_STRINGLIST, 1);
          msg->msg_detail->strings_num= count;
          msg->msg_detail->strings= g_malloc0(sizeof(char *) * count);

          for (j= 0; j < count; j++)
          {
            msg->msg_detail->strings[j]= g_strdup(myx_grt_list_item_get_as_string(msg_detail_value, j));
          }
        }
      }
    }
  }

  return msgs;
}


/** 
 ****************************************************************************
 * @brief 
 *
 * @param msgs messages to be freed
 *****************************************************************************/
void myx_grt_messages_free(MYX_GRT_MSGS *msgs)
{
  unsigned int i;
  
  if (msgs)
  {
    for (i= 0; i < msgs->msgs_num; i++)
    {
      g_free(msgs->msgs[i].msg);
      if (msgs->msgs[i].msg_detail)
        myx_free_stringlist(msgs->msgs[i].msg_detail);
    }
    g_free(msgs->msgs);
    g_free(msgs);
  }
}



/** 
 ****************************************************************************
 * @brief Add a message to the GRT message queue.
 *
 * @param grt
 * @param msg_type  
 * @param message  message string
 * @param details  optional string list for details of the message
 * @param copy_details if 1, the function will make a copy of the details 
 *              parameter
 * @param progress the current progress value (1 ... 100) or -1 
 *
 *****************************************************************************/
void myx_grt_messages_stack_add(MYX_GRT *grt, int msg_type, const char *message, MYX_STRINGLIST *details,
                     int copy_details, int progress)
{
  MYX_GRT_MSG *msg;
  
  GRT_ENTER(grt);
  
  if (!grt->msgs)
    grt->msgs= g_new0(MYX_GRT_MSGS, 1);

  grt->msgs->msgs_num++;
  grt->msgs->msgs= g_realloc(grt->msgs->msgs,
                             grt->msgs->msgs_num*sizeof(MYX_GRT_MSG));
  msg= grt->msgs->msgs+grt->msgs->msgs_num-1;

  msg->msg_type= msg_type;
  msg->msg= g_strdup(message);
  if (copy_details && details)
  {
    unsigned int i;
    msg->msg_detail= g_new0(MYX_STRINGLIST, 1);
    msg->msg_detail->strings= g_new0(char*, details->strings_num);
    for (i= 0; i < details->strings_num; i++)
      msg->msg_detail->strings[i]= g_strdup(details->strings[i]);
  }
  else
    msg->msg_detail= details;
  
  GRT_LEAVE(grt);
}

void myx_grt_messages_stack_add_message(MYX_GRT *grt, const char *message, 
                                        MYX_STRINGLIST *details, int copy_details, ...)
{
  char *msg;
  va_list args;

  va_start(args, copy_details);

  msg= g_strdup_vprintf(message, args);

  va_end(args);


  myx_grt_messages_stack_add(grt, 0, msg, details, copy_details, 0);

  myx_grt_messages_stack_flush(grt, 0);
}

void myx_grt_messages_stack_add_error(MYX_GRT *grt, const char *message, 
                                      MYX_STRINGLIST *details, int copy_details, ...)
{
  char *msg;
  va_list args;

  va_start(args, copy_details);

  msg= g_strdup_vprintf(message, args);

  va_end(args);

  myx_grt_messages_stack_add(grt, 1, msg, details, copy_details, 0);

  myx_grt_messages_stack_flush(grt, 0);
}

/** 
 ****************************************************************************
 * @brief Processes the messages stored on the GRT message stack.
 *
 * @param grt  
 * @param count  number of messages to fetch. 0 will return all messages.
 *
 * @return Fetched messages or NULL, if there's none. The returned struct
 * must be freed with myx_grt_free_msgs()
 *****************************************************************************/
void myx_grt_messages_stack_flush(MYX_GRT *grt, unsigned int count)
{
  //unsigned int i;
  MYX_GRT_MSGS *msgs;
  
  GRT_ENTER(grt);
  
  // return what was requested

  if (!grt->msgs)
    GRT_LEAVE(grt);
  
  if (count == 0)
  {
    msgs= grt->msgs;
    grt->msgs= NULL;
  }
  else
  {
    unsigned int i;

    if (count > grt->msgs->msgs_num)
      count= grt->msgs->msgs_num;

    msgs= g_new0(MYX_GRT_MSGS, 1);
    msgs->msgs= g_new0(MYX_GRT_MSG, count);
    for (i= 0; i < count; i++)
    {
      msgs->msgs[i]= grt->msgs->msgs[i];
    }
    memmove(grt->msgs->msgs, grt->msgs->msgs+count,
            (grt->msgs->msgs_num-count)*sizeof(MYX_GRT_MSG));
    grt->msgs->msgs_num-= count;
  }

  // call the callback function to process the messages
  if(grt->process_messages != NULL)
    grt->process_messages(msgs, grt->process_messages_data);

  myx_grt_messages_free(msgs);

  GRT_LEAVE(grt);
}


/**
 ****************************************************************************
 * @brief Adds an object to the direct reference cache
 *
 * @param grt MYX_GRT pointer
 * @param obj The object to add
 ****************************************************************************
 */
void myx_grt_reference_cache_add(MYX_GRT *grt, MYX_GRT_VALUE *obj)
{
  // TODO Keep manually added values in a separate list, so they are re-added when the cache is rebuilt.
  g_hash_table_insert(grt->direct_reference_cache, g_strdup(myx_grt_dict_id_item_as_string(obj)), obj);
  myx_grt_value_listener_add(grt, obj, NULL, refcache_callback);
}


/**
 ****************************************************************************
 * @brief Called by the GRT when certain actions happen to a value, e.g. it is being deleted.
 *
 * @param grt The generic runtime that is acting.
 * @param value The value on which the runtime is acting.
 * @param reason The reason why this callback was called.
 * @param data User data given when this callback was registered.
 * @return 0
 ****************************************************************************
 */
int refcache_callback(MYX_GRT* grt, MYX_GRT_VALUE* value, MYX_GRT_VALUE_CALLBACK_REASON reason, void* data)
{
  if (reason == MYX_GVCR_DELETE)
  {
    const char *_id= myx_grt_dict_item_get_as_string(value, "_id");
    g_hash_table_remove(grt->global_reference_cache, _id);

    g_hash_table_remove(grt->direct_reference_cache, _id);
  };

  return 0;
}

/**
 ****************************************************************************
 * @brief Called by the GRT when the cache is cleared.
 *
 * @param key The key of the entry that is currently removed.
 * @param value The value part of the entry.
 * @param data User data (NULL).
 ****************************************************************************
 */
void refcache_callback2(gpointer key, gpointer value, gpointer data)
{
//  MYX_GRT_VALUE* grtValue = value;
  myx_grt_value_listener_remove(value, data, refcache_callback);
}

/** 
 ****************************************************************************
 * @brief Clears the current cache of _id strings in the root value
 *
 * @param grt the GRT environment
 *****************************************************************************/
void myx_grt_reference_cache_clear(MYX_GRT *grt)
{
  GRT_ENTER(grt);
  
  // Remove all value listener registrations.
  g_hash_table_foreach(grt->global_reference_cache, refcache_callback2, NULL); 

  // free current cache
  g_hash_table_destroy(grt->global_reference_cache);

  // reallocate cache
  grt->global_reference_cache= g_hash_table_new_full(g_str_hash, g_str_equal,
                                                g_free, NULL);
  
  GRT_LEAVE(grt);
}

static void grt_rescan_dict(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *object_path)
{
  unsigned int i;
  const char *struct_name;

  GRT_ENTER(grt);
  
  struct_name= myx_grt_dict_struct_get_name(dict);
  
  // if this dict inherits from GrtObject, cache its _id if it not empty
  // and set the object path
  if (struct_name && myx_grt_struct_inherits_from(grt, struct_name, "GrtObject"))
  {
    const char *_id= myx_grt_dict_item_get_as_string(dict, "_id");

    if (_id)
    {
      dict->value.d->grt= grt;
      dict->value.d->object_path= g_strdup(object_path);
      g_hash_table_insert(grt->global_reference_cache, g_strdup(_id), dict);
      myx_grt_value_listener_add(grt, dict, NULL, refcache_callback);
    }
  }

  for (i= 0; i<myx_grt_dict_item_count(dict); i++)
  {
    const char *key= myx_grt_dict_item_key_by_index(dict, i);
    MYX_GRT_VALUE *value= myx_grt_dict_item_get_value(dict, key);

    if (myx_grt_value_get_type(value) == MYX_DICT_VALUE) 
    {
      char *new_path= g_strdup(object_path);
      new_path= str_g_append(new_path, "/");
      new_path= str_g_append(new_path, key);

      grt_rescan_dict(grt, value, new_path);

      g_free(new_path);
    }
    else
      if ( (myx_grt_value_get_type(value) == MYX_LIST_VALUE) && 
        (myx_grt_list_content_get_type(value) == MYX_DICT_VALUE) )
      {
        unsigned int j;

        for (j= 0; j<myx_grt_list_item_count(value); j++)
        {
          char *new_path= g_strdup_printf("%s/%s/%d", object_path, key, j);

          grt_rescan_dict(grt, myx_grt_list_item_get(value, j), new_path);

          g_free(new_path);
        }
      }
  }

  GRT_LEAVE(grt);
}

/** 
 ****************************************************************************
 * @brief Rescans the root value to cache all _id strings
 *
 * @param grt the GRT environment
 *****************************************************************************/
void myx_grt_reference_cache_rescan(MYX_GRT *grt)
{
  GRT_ENTER(grt);
  // start scan
  grt_rescan_dict(grt, myx_grt_get_root(grt), "");
  GRT_LEAVE(grt);
}

/** 
 ****************************************************************************
 * @brief Performs a reference cache lookup for a specific _id
 *
 * @param grt the GRT environment
 * @param ref_id the _id that is looked up
 *
 * @return returns a value if it is found in the cache or NULL
 *****************************************************************************/
MYX_GRT_VALUE * myx_grt_reference_cache_lookup(MYX_GRT *grt, const char *ref_id)
{
  MYX_GRT_VALUE *retval= NULL;
  
  if (!ref_id || !ref_id[0])
    return NULL;

  GRT_ENTER(grt);
  if(ref_id)
  {
    // try to find the value in the cache
    MYX_GRT_VALUE *value= g_hash_table_lookup(grt->global_reference_cache, ref_id);

    if (value)
      retval= value;
    else
    {
      // try direct cache
      value= g_hash_table_lookup(grt->direct_reference_cache, ref_id);

      if (value)
        retval= value;
      else
      {
        // if it is not found, rescan and search again
        myx_grt_reference_cache_rescan(grt);

        retval= g_hash_table_lookup(grt->global_reference_cache, ref_id);
      }
    }
  }
  else
    retval= NULL;
  
  GRT_RETURN(grt, retval, MYX_GRT_VALUE*);
}


char * myx_grt_get_guid()
{
  /* GUIDs must be no more than 50 chars */
  
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  GUID guid;
  WCHAR guid_wstr[50];
  char guid_str[200];

  int len;

  CoCreateGuid(&guid);
  len= StringFromGUID2(&guid, (LPOLESTR) guid_wstr, 50);

  //Covert GUID from WideChar to utf8
  WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR) guid_wstr, len,
    (LPSTR) guid_str, 200, NULL, NULL);

  return g_strdup(guid_str);
#elif defined(__APPLE__)
  CFUUIDRef uid;
  CFStringRef str;
  char *data;
  
  uid= CFUUIDCreate(NULL);
  str= CFUUIDCreateString(NULL, uid);
  
  data= g_malloc(sizeof(gchar)*(CFStringGetLength(str)+1));
  
  CFStringGetCString(str, data, CFStringGetLength(str)+1, kCFStringEncodingUTF8);
  
  CFRelease(uid);
  CFRelease(str);
  
  return data;
#else
  {
    uuid_t gid;
    char buffer[40];
    uuid_generate_time(gid);
    uuid_unparse(gid, buffer);
    return g_strdup(buffer);
  }
#endif
}



const char *myx_grt_error_string(MYX_GRT_ERROR error)
{
  switch (error)
  {
  case MYX_GRT_NO_ERROR:
    return "Success";
  case MYX_GRT_INTERNAL_ERROR:
    return "Internal error";
  case MYX_GRT_BAD_PATH:
    return "Invalid path";
  case MYX_GRT_CANT_OPEN_FILE:
    return "Cannot open file";
  case MYX_GRT_BAD_FUNCTION:
    return "Invalid function";
  case MYX_GRT_DUPLICATE_ENTRY:
    return "Insertion of duplicate name";
  case MYX_GRT_BAD_VALUE:
    return "Invalid value";
  case MYX_GRT_BAD_DATA:
    return "Invalid data";
  case MYX_GRT_VALIDATION_ERROR:
    return "Validation error";
  case MYX_GRT_FUNCTION_CALL_ERROR:
    return "Error calling function";
  case MYX_GRT_MODULE_INIT_ERROR:
    return "Error initializing module";
  case MYX_GRT_BAD_MODULE:
    return "Invalid module";
  case MYX_GRT_UNKNOWN_MODULE_TYPE:
    return "Unknown module type";
  case MYX_GRT_JAVA_NOT_FOUND:
    return "Java JRE not found";
  case MYX_GRT_JAVA_REGISTRY_CORRUPTED:
    return "Java registry corrupted";
  case MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED:
    return "Java JRE cannot be loaded";

  case MYX_GRT_SHELL_UNSUPPORTED:
    return "Shell type is not supported";
  case MYX_GRT_SHELL_ALREADY_LOADED:
    return "Shell was already initialized";
  case MYX_GRT_SHELL_INIT_ERROR:
    return "Error initializing shell";

  default:
    return "Unknown error";
  }
}

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
int myx_free_lib_stringlist(MYX_STRINGLIST *stringlist)
{
  return myx_free_stringlist(stringlist);
}
#endif
