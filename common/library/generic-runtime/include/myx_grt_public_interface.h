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

#ifndef __MYX_GRT_H__
#define __MYX_GRT_H__

#define GRT_VERSION "2.0.12"

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
# define __LCC__
#define MYX_SOCKET SOCKET
#else
# define MYX_SOCKET int
#endif

#include <myx_util_public_interface.h>
#ifdef ENABLE_JAVA_MODULES
# ifdef __APPLE__
#  include <JavaVM/jni.h>
#  ifdef FROM_XCODE
#   include <MySQLGRT/com_mysql_grt_GrtCallbackNative.h>
#  else
#   include <com_mysql_grt_GrtCallbackNative.h>
#  endif
# else
#  include <jni.h>
#  include <com_mysql_grt_GrtCallbackNative.h>
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif


#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

/*
 * PUBLIC INTERFACE definition for MYSQLLibInterfaceMapper
 */

/// [SCRIPT::LibInterfaceMapper] -public_interface "libmysqlgrt"
#define libmysqlgrt_PUBLIC_INTERFACE_VERSION 10000

/// [SCRIPT::LibInterfaceMapper] -add_to_uses "myx_util_public_interface, myx_public_interface"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\utilities\include\myx_util_public_interface.h"
/// [SCRIPT::LibInterfaceMapper] -add_datatypes_from "..\..\base-library\include\myx_public_interface.h"


/**
 * Constants and defines
 */

#define MAX_NESTING 100 // The maximum number of path parts in a GRT path.

/*
 * Enums
 */

typedef enum
{
  MYX_GRT_NO_ERROR = 0,
  MYX_GRT_FIRST_ERROR = 10000,
  MYX_GRT_INTERNAL_ERROR = MYX_GRT_FIRST_ERROR,
  MYX_GRT_BAD_PATH,
  MYX_GRT_CANT_OPEN_FILE,
  MYX_GRT_BAD_FUNCTION,
  MYX_GRT_DUPLICATE_ENTRY,
  MYX_GRT_BAD_VALUE,
  MYX_GRT_BAD_DATA,

  MYX_GRT_VALIDATION_ERROR,
  MYX_GRT_FUNCTION_CALL_ERROR,
  MYX_GRT_MODULE_INIT_ERROR,
  MYX_GRT_BAD_MODULE,
  MYX_GRT_UNKNOWN_MODULE_TYPE,

  MYX_GRT_JAVA_NOT_FOUND,
  MYX_GRT_JAVA_REGISTRY_CORRUPTED,
  MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED,

  MYX_GRT_SHELL_UNSUPPORTED,
  MYX_GRT_SHELL_ALREADY_LOADED,
  MYX_GRT_SHELL_INIT_ERROR

} MYX_GRT_ERROR;

typedef enum
{
  MYX_GRTA_OK= 0,
  MYX_GRTA_BUSY,
  MYX_GRTA_EXECUTING,
  MYX_GRTA_FINISHED,
  MYX_GRTA_CANCELLED,
  MYX_GRTA_ERROR,
  
  MYX_GRTA_UNREACHABLE,
  MYX_GRTA_REMOTE_ERROR

} MYX_GRT_AGENT_STATUS;
  
typedef enum 
{
  MYX_ANY_VALUE= 0,
  MYX_INT_VALUE= 1,
  MYX_REAL_VALUE,
  MYX_STRING_VALUE,
  MYX_LIST_VALUE,
  MYX_DICT_VALUE
} MYX_GRT_VALUE_TYPE;

typedef enum
{
  MYX_GRT_FUNCTION_HAS_ARGUMENT = 1<<0,
  MYX_GRT_FUNCTION_RETURNS_VALUE = 1<<1,
  MYX_GRT_FUNCTION_IS_GUI = 1<<2,
  MYX_GRT_FUNCTION_IS_OPTIONAL = 1<<3
} MYX_GRT_FUNCTION_FLAGS;

typedef enum
{
  MYX_GRT_VERBOSE = 1,
  MYX_GRT_REMOTE_DEBUG = 2
} MYX_GRT_OPTIONS;

typedef enum
{
  MYX_IT_SMALL,
  MYX_IT_STANDARD,
  MYX_IT_MANY_STANDARD
} MYX_ICON_TYPE;

typedef enum
{
  MYX_GVCR_DELETE,
  MYX_GVCR_DICT_ITEM_CHANGE,
  MYX_GVCR_LIST_CHANGE,
  MYX_GVCR_LIST_ITEM_CHANGE
} MYX_GRT_VALUE_CALLBACK_REASON;

/*
 * Structs
 */

typedef struct MYX_GRT_MODULE_LOADER MYX_GRT_MODULE_LOADER;
typedef struct MYX_GRT MYX_GRT;

typedef enum MYX_GRT_SHELL_INTERFACE
{
  MYX_GRT_SHELL_LUA,
  MYX_GRT_SHELL_PYTHON
} MYX_GRT_SHELL_INTERFACE;
  
typedef enum MYX_GRT_SHELL_COMMAND
{
  MYX_GRT_SHELL_COMMAND_UNKNOWN = -1,
  MYX_GRT_SHELL_COMMAND_EXIT = 0,
  MYX_GRT_SHELL_COMMAND_ALL,
  MYX_GRT_SHELL_COMMAND_ERROR,
  MYX_GRT_SHELL_COMMAND_STATEMENT, // was _LUA
  MYX_GRT_SHELL_COMMAND_HELP,
  MYX_GRT_SHELL_COMMAND_LS,
  MYX_GRT_SHELL_COMMAND_CD,
  MYX_GRT_SHELL_COMMAND_RUN
} MYX_GRT_SHELL_COMMAND;


/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GRT_FUNCTION
typedef struct MYX_GRT_FUNCTION
{
  struct MYX_GRT_MODULE *module;
  
  char *name; // the function name must match the interface name it implements

  char *param_struct_name; // may be NULL
  char *return_struct_name; // may be NULL

  struct MYX_GRT_FUNCTION_PRIVATE *priv; // private data for each loader
} MYX_GRT_FUNCTION;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GRT_MODULE
typedef struct MYX_GRT_MODULE
{
  MYX_GRT_MODULE_LOADER *loader;
  char *name;
  char *path;
  unsigned int functions_num;
  MYX_GRT_FUNCTION *functions;
  char *extends;
  struct MYX_GRT_MODULE_PRIVATE *priv; // private data for each loader
  char dont_bridge_loaded_values;
} MYX_GRT_MODULE;


typedef struct MYX_GRT_STRUCT_MEMBER
{
  char *name;

  char *caption;
  char *desc;

  char *default_value;

  MYX_GRT_VALUE_TYPE value_type;
  char *struct_name; /* only if value_type == dict or string */

  MYX_GRT_VALUE_TYPE content_type; /* only if value_type == dict or list */
  char *content_struct_name; /* only if value_type == dict or list*/

  unsigned int is_ref; /* only if value_type == string or list*/
  char *overrides;
} MYX_GRT_STRUCT_MEMBER;


typedef struct MYX_GRT_STRUCT
{
  char *name;
  char *parent_struct_name;

  char *caption;
  char *desc;

  char *bridge;

  unsigned int members_num;
  MYX_GRT_STRUCT_MEMBER *members; // list of members
} MYX_GRT_STRUCT;

typedef struct MYX_GRT_STRUCTS
{
  unsigned int structs_num;
  MYX_GRT_STRUCT *structs;
} MYX_GRT_STRUCTS;

typedef struct 
{
  MYX_GRT_VALUE_TYPE content_type; /* only used if this is a typed list, meaning it can only hold values of the given type */
  char *content_struct_name; /* only if content_type == dict, used to make sure only dicts of the given struct can be added */

  //MYX_GRT *grt; // the grt member is only set if this is a proxy list
  //MYX_GRT_MODULE *proxy_module; // if set, this list becomes a proxy list and it's items are returned by the specified module
  //char *proxy_path; // only set if this is a proxy list

  unsigned int items_num;
  struct MYX_GRT_VALUE **items;
} MYX_GRT_LIST;

typedef struct
{
  char *key;
  struct MYX_GRT_VALUE *value;
} MYX_GRT_DICT_ITEM;

typedef struct
{
  char *struct_name;
  MYX_GRT *grt; // the grt member is only set if this struct has been added to the GRT reference cache
  char *object_path; // the path is only set if this struct has been added to the GRT reference cache

  MYX_GRT_VALUE_TYPE content_type; // only used if this is a typed dict, meaning it can only hold values of the given type 
  char *content_struct_name; // only if content_type == dict, used to make sure only dicts of the given struct can be added 

  //MYX_GRT_MODULE *proxy_module; // if set, this dict becomes a proxy object and it's items are returned by the specified module
  //char *proxy_path; // only set if this is a proxy object

  unsigned int items_num;
  MYX_GRT_DICT_ITEM *items; // this must be in strcmp() order of name
} MYX_GRT_DICT;

typedef struct MYX_GRT_VALUE_EXTENDED
{
  MYX_GRT_MODULE *bridge_module; // if set, callbacks to the bridge are made if the value is changed
  void *bridge_data_object; // data set by the bridge so it is possible to associate bridged objects with GRT values
  void *bridge_data_owner; // data set by the bridge so it is possible to associate bridged objects with GRT values
  char *bridge_dict_key; // if this is a dict item, store the key name so the bridge can identify the property associated with the value
  unsigned int bridge_list_index; // if this is a list item, store the index so the bridge can identify the item associated with the value

  unsigned int listeners_num;
  struct MYX_GRT_VALUE_LISTENER **listeners;
} MYX_GRT_VALUE_EXTENDED;

typedef struct MYX_GRT_VALUE 
{
  MYX_GRT_VALUE_TYPE type;
  unsigned int refcount;
  union {
    int i;
    double r;
    char *s;
    MYX_GRT_LIST *l;
    MYX_GRT_DICT *d;
  } value;

  MYX_GRT_VALUE_EXTENDED *extended;
} MYX_GRT_VALUE;  

typedef struct MYX_GRT_VALUE_LISTENER
{
  MYX_GRT *listeners_grt;
  int (*function)(MYX_GRT *, MYX_GRT_VALUE *, MYX_GRT_VALUE_CALLBACK_REASON, void *);
  void *user_data;
} MYX_GRT_VALUE_LISTENER;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GRT_MSG
typedef struct MYX_GRT_MSG {
  int msg_type;
  char *msg;
  MYX_STRINGLIST *msg_detail;
  int progress;
} MYX_GRT_MSG;

/// [SCRIPT::LibInterfaceMapper] -generate_classes MYX_GRT_MSGS
typedef struct MYX_GRT_MSGS {
  unsigned int msgs_num;
  MYX_GRT_MSG *msgs;
} MYX_GRT_MSGS;

typedef struct
{
  char *name;

  MYX_GRT_VALUE *(*function)(MYX_GRT_VALUE*,void*);
} MYX_GRT_BUILTIN_FUNCTION;


typedef struct
{
  char *name;
  char *extends;
  unsigned int functions_num;
  MYX_GRT_BUILTIN_FUNCTION *functions;
} MYX_GRT_BUILTIN_MODULE;
  

typedef enum
{
  MYX_BUILTIN_MODULE_TYPE,
    MYX_JAVA_MODULE_TYPE,
    MYX_LUA_MODULE_TYPE,
    MYX_PYTHON_MODULE_TYPE,
    MYX_PHP_MODULE_TYPE,
    MYX_DELPHI_MODULE_TYPE,
    MYX_OBJC_MODULE_TYPE,
    MYX_CPP_MODULE_TYPE
//    MYX_PERL_MODULE_TYPE,
} MYX_GRT_MODULE_TYPE;

typedef enum
{
  MYX_GRT_IO_STANDARD = 0,
  MYX_GR_IO_PASSWORD = 1
} MYX_GRT_INPUT_OPTIONS;

  
typedef struct MYX_GRT_AGENT_SESSION MYX_GRT_AGENT_SESSION;
  
/*
 * Functions
 */

typedef void (*MYX_GRT_PRINT_CALLBACK)(const char *, void*);
typedef void (*MYX_GRT_MESSAGE_CALLBACK)(MYX_GRT_MSGS *msgs, void *user_data);
typedef int (*MYX_GRT_INPUT_CALLBACK)(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data);
typedef int (*MYX_GRT_STATUS_QUERY_CALLBACK)(void *user_data);
typedef void (*MYX_GRT_LOG_CALLBACK)(MYX_GRT *, int, const char *, const char*);
typedef int (*MYX_GRT_VALUE_LISTENER_CALLBACK)(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *user_data);

MYX_PUBLIC_FUNC int myx_free_lib_stringlist(MYX_STRINGLIST *stringlist);

MYX_PUBLIC_FUNC MYX_GRT * myx_grt_initialize(int options);
MYX_PUBLIC_FUNC void myx_grt_finalize(MYX_GRT *grt);

MYX_PUBLIC_FUNC void myx_grt_catch_glib_messages(MYX_GRT *grt, int do_catch);

#ifdef ENABLE_JAVA_MODULES
JNIEXPORT jstring JNICALL Java_Grt_callGrtFunction
  (JNIEnv *, jobject, jlong, jstring, jstring, jstring);
#endif

const char *myx_grt_error_string(MYX_GRT_ERROR error);

// ----------------------------------------------------------------------------------------------------------------------
// Callbacks

MYX_PUBLIC_FUNC void myx_grt_set_output_callback(MYX_GRT *grt, void *user_data,
                                 void (*process_output_func)(const char *text, void *user_data));
MYX_PUBLIC_FUNC void myx_grt_set_message_callback(MYX_GRT *grt, void *user_data,
                                 void (*process_message_func)(MYX_GRT_MSGS *msgs, void *user_data));
MYX_PUBLIC_FUNC void myx_grt_set_input_callback(MYX_GRT *grt, void *user_data,
                                 int (*process_input_func)(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data));
MYX_PUBLIC_FUNC void myx_grt_set_status_query_callback(MYX_GRT *grt, void *user_data,
                                 int (*process_status_query_func)(void *user_data));


// ----------------------------------------------------------------------------------------------------------------------
// GRT global functions

MYX_PUBLIC_FUNC const char * myx_grt_version();

MYX_PUBLIC_FUNC void myx_grt_init_threads();

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_get_root(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_set_root(MYX_GRT *grt, MYX_GRT_VALUE *new_root);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_reference_cache_lookup(MYX_GRT *grt, const char *ref_id);
MYX_PUBLIC_FUNC void myx_grt_reference_cache_add(MYX_GRT *grt, MYX_GRT_VALUE *obj);
MYX_PUBLIC_FUNC void myx_grt_reference_cache_clear(MYX_GRT *grt);
MYX_PUBLIC_FUNC void myx_grt_reference_cache_rescan(MYX_GRT *grt);

// ----------------------------------------------------------------------------------------------------------------------
// GRT Messages
MYX_PUBLIC_FUNC MYX_GRT_MSGS * myx_grt_messages_convert(MYX_GRT_VALUE *msgs_list);
MYX_PUBLIC_FUNC void myx_grt_messages_free(MYX_GRT_MSGS *msgs);

MYX_PUBLIC_FUNC void myx_grt_messages_stack_add(MYX_GRT *grt, int msg_type, const char *message,
                                          MYX_STRINGLIST *details, int copy_details, int progress);
MYX_PUBLIC_FUNC void myx_grt_messages_stack_add_message(MYX_GRT *grt, const char *message,
                                          MYX_STRINGLIST *details, int copy_details, ...);
MYX_PUBLIC_FUNC void myx_grt_messages_stack_add_error(MYX_GRT *grt, const char *message, 
                                          MYX_STRINGLIST *details, int copy_details, ...);
MYX_PUBLIC_FUNC void myx_grt_messages_stack_flush(MYX_GRT *grt, unsigned int count);


// ----------------------------------------------------------------------------------------------------------------------
// Modules

MYX_PUBLIC_FUNC void myx_grt_module_set_log_callback(MYX_GRT *grt, 
                                                    void (*log_func)(MYX_GRT*, int, const char *, const char *));

MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_module_init(MYX_GRT *grt, const char *filename);

MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_module_register_builtin(MYX_GRT *grt, MYX_GRT_BUILTIN_MODULE *module, void *function_data);
MYX_PUBLIC_FUNC void myx_grt_module_unregister_builtin(MYX_GRT *grt, MYX_GRT_MODULE *module);

MYX_PUBLIC_FUNC int myx_grt_module_get_count(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_grt_module_get_names(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_module_get(MYX_GRT *grt, const char *name);
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_module_get_by_index(MYX_GRT *grt, unsigned int index);
MYX_PUBLIC_FUNC int myx_grt_modules_get_that_extend(MYX_GRT *grt, const char *module, MYX_GRT_MODULE **retmodules[]);
MYX_PUBLIC_FUNC MYX_GRT_MODULE_TYPE myx_grt_module_get_type(MYX_GRT_MODULE *module);

MYX_PUBLIC_FUNC int myx_grt_module_function_get_count(MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_grt_module_function_get_names(MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC MYX_GRT_FUNCTION * myx_grt_module_function_get(MYX_GRT_MODULE *module, const char *name);
MYX_PUBLIC_FUNC MYX_GRT_FUNCTION * myx_grt_module_function_get_by_index(MYX_GRT_MODULE *module, unsigned int index);
MYX_PUBLIC_FUNC char * myx_grt_module_function_get_params(MYX_GRT_FUNCTION *func);
MYX_PUBLIC_FUNC char * myx_grt_module_function_get_return_type(MYX_GRT_FUNCTION *func);

// Internal
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_grt_module_loader_create(MYX_GRT *grt, MYX_GRT_MODULE_TYPE loader_type,
                  MYX_STRINGLIST *extensions, 
                  MYX_GRT_ERROR (*init_module)(MYX_GRT_MODULE_LOADER *loader, const char *file_name, MYX_GRT_MODULE **retmodule),
                  MYX_GRT_ERROR (*call_function)(MYX_GRT_FUNCTION *func, MYX_GRT_VALUE *argument, MYX_GRT_VALUE **retval),
                  void *priv);
MYX_PUBLIC_FUNC void * myx_grt_module_get_private_data(MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC void * myx_grt_module_loader_get_private_data(MYX_GRT_MODULE_LOADER *loader);
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_module_create(MYX_GRT_MODULE_LOADER *loader, const char *name, 
                                                       const char *path, const char *extends, void *priv);
MYX_PUBLIC_FUNC void myx_grt_module_add_function(MYX_GRT_MODULE *module, const char *name, const char *param_struct_name, 
                                                 const char *return_struct_name, void *priv);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_add_module(MYX_GRT *grt, MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_register_module_loader(MYX_GRT *grt, MYX_GRT_MODULE_LOADER *loader);
MYX_PUBLIC_FUNC int myx_grt_scan_for_modules(MYX_GRT *grt, const char *directory, MYX_GRT_ERROR *error);

// ----------------------------------------------------------------------------------------------------------------------
// GRT module functions

MYX_PUBLIC_FUNC MYX_GRT_FUNCTION * myx_grt_function_get(MYX_GRT *grt, const char *module, const char *function_name, int search_parent);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_function_call(MYX_GRT *grt, MYX_GRT_FUNCTION *func,
                                    MYX_GRT_VALUE *argument, MYX_GRT_ERROR *error);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_function_get_and_call(MYX_GRT *grt, const char *module, const char *function_name, int search_parent,
                                    MYX_GRT_VALUE *argument, MYX_GRT_ERROR *error);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_function_create_result(MYX_GRT_VALUE *result);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_function_create_error_result(const char *error, const char *error_details);
MYX_PUBLIC_FUNC char * myx_grt_function_check_error(MYX_GRT_VALUE *res, int allow_null_as_result);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_function_extract_value_from_result(MYX_GRT_VALUE *result);

// ----------------------------------------------------------------------------------------------------------------------
// GRT Remote Agents

// Server Side
MYX_PUBLIC_FUNC void myx_grt_start_agent(MYX_GRT *grt, int port,
                                         char **allowed_modules, unsigned int allowed_modules_num);

// Client Side
MYX_PUBLIC_FUNC MYX_GRT_AGENT_SESSION * myx_grt_remote_connect(const char *host, int port,
                                              const char *passkey);
MYX_PUBLIC_FUNC MYX_GRT_AGENT_STATUS myx_grt_remote_function_invoke(MYX_GRT_AGENT_SESSION *sess,
                                                    const char *module, const char *function_name,
                                                    MYX_GRT_VALUE *argument);
MYX_PUBLIC_FUNC MYX_GRT_AGENT_STATUS myx_grt_remote_function_check(MYX_GRT_AGENT_SESSION *sess);
MYX_PUBLIC_FUNC MYX_GRT_VALUE *myx_grt_remote_function_finish(MYX_GRT_AGENT_SESSION *sess,
                                                              MYX_GRT_ERROR *error,
                                                              MYX_GRT_AGENT_STATUS *status);
MYX_PUBLIC_FUNC MYX_GRT_AGENT_STATUS myx_grt_remote_function_cancel(MYX_GRT_AGENT_SESSION *sess);
MYX_PUBLIC_FUNC MYX_GRT_MSGS * myx_grt_remote_get_messages(MYX_GRT_AGENT_SESSION *sess);
MYX_GRT_VALUE *myx_grt_remote_get_tree(MYX_GRT_AGENT_SESSION *sess, MYX_GRT_AGENT_STATUS *status);
void myx_grt_remote_set_tree(MYX_GRT_AGENT_SESSION *sess, MYX_GRT_VALUE *tree, MYX_GRT_AGENT_STATUS *status);
MYX_PUBLIC_FUNC int myx_grt_remote_session_close(MYX_GRT_AGENT_SESSION *sess);

// ----------------------------------------------------------------------------------------------------------------------
// GRT FastCGI

MYX_PUBLIC_FUNC void myx_grt_start_fcgi(MYX_GRT *grt, int port, char **allowed_modules, 
                   unsigned int allowed_modules_num);

// ----------------------------------------------------------------------------------------------------------------------
// GRT Structs

// General
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_struct_load_and_register(MYX_GRT *grt, const char *filename);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_struct_register(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct);

// Packages
MYX_PUBLIC_FUNC int myx_grt_package_count(MYX_GRT *grt);
MYX_PUBLIC_FUNC char * myx_grt_package_by_index(MYX_GRT *grt, unsigned int index);
MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_grt_packages(MYX_GRT *grt);

MYX_PUBLIC_FUNC int myx_grt_package_struct_count(MYX_GRT *grt, const char *package_name);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT * myx_grt_package_struct_by_index(MYX_GRT *grt, const char *package_name, unsigned int index);

// Structs
MYX_PUBLIC_FUNC MYX_GRT_STRUCTS * myx_grt_structs_get(MYX_GRT *grt);
MYX_PUBLIC_FUNC int myx_grt_structs_free(MYX_GRT_STRUCTS *gstructs);

MYX_PUBLIC_FUNC int myx_grt_struct_get_count(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT * myx_grt_struct_get_by_index(MYX_GRT *grt, unsigned int index);

MYX_PUBLIC_FUNC MYX_GRT_STRUCTS * myx_grt_struct_load_list(const char *filename, MYX_GRT_ERROR *error);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_struct_save_list(MYX_GRT_STRUCTS *gstructs, const char *filename);

// Struct handling
MYX_PUBLIC_FUNC MYX_GRT_STRUCT * myx_grt_struct_get(MYX_GRT *grt, const char *name);
MYX_PUBLIC_FUNC int myx_grt_struct_free(MYX_GRT_STRUCT *gstruct);

MYX_PUBLIC_FUNC int myx_grt_struct_get_child_count(MYX_GRT *grt, const char *struct_name);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT * myx_grt_struct_get_child_by_index(MYX_GRT *grt, const char *struct_name, unsigned int index);

MYX_PUBLIC_FUNC const char * myx_grt_struct_get_name(MYX_GRT_STRUCT *gstruct);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_parent_name(MYX_GRT_STRUCT *gstruct);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_caption(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, int *inherited_caption);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_desc(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct);

MYX_PUBLIC_FUNC const char * myx_grt_struct_get_bridge_name(MYX_GRT_STRUCT *gstruct);
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_struct_get_bridge(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct);

MYX_PUBLIC_FUNC int myx_grt_struct_inherits_from(MYX_GRT *grt, const char *struct_name, const char *parent_name);
MYX_PUBLIC_FUNC int myx_grt_struct_is_or_inherits_from(MYX_GRT *grt, const char *struct_name, const char *parent_name);

MYX_PUBLIC_FUNC char * myx_grt_struct_get_icon_path(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, MYX_ICON_TYPE icon_type);

MYX_PUBLIC_FUNC const char * myx_grt_struct_get_icon(MYX_GRT *grt, const char *source_path, 
                 MYX_GRT_STRUCT *gstruct, MYX_ICON_TYPE icon_type, unsigned int *length);

// Struct utilites
MYX_PUBLIC_FUNC char * myx_grt_struct_to_xml(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, int include_children);
MYX_PUBLIC_FUNC MYX_GRT_STRUCTS * myx_grt_struct_from_xml(const char *str, size_t size);

MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_struct_export_java_classes(MYX_GRT_STRUCTS *gstructs, 
                                                                 const char *package_name, const char *output_path);

MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_struct_export_php_classes(MYX_GRT_STRUCTS *gstructs, const char *output_path);


// GRT Structs Members
MYX_PUBLIC_FUNC unsigned int myx_grt_struct_get_member_count(MYX_GRT_STRUCT *gstruct);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_index(MYX_GRT_STRUCT *gstruct, unsigned int index);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_name(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *name, int check_parent_structs);

MYX_PUBLIC_FUNC int myx_grt_struct_get_member_count_total(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct);
MYX_PUBLIC_FUNC int myx_grt_struct_get_member_count_total_excluding_struct(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *excluding_struct_name);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT_MEMBER * myx_grt_struct_get_member_by_index_total(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, int index);

MYX_PUBLIC_FUNC const char * myx_grt_struct_get_member_name(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_member_default(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_member_caption(MYX_GRT *grt, MYX_GRT_STRUCT *gstruct, const char *member_name, int check_parent_structs);
MYX_PUBLIC_FUNC const char * myx_grt_struct_get_member_desc(MYX_GRT_STRUCT_MEMBER *member);

MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_grt_struct_member_get_type(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_grt_struct_member_get_content_type(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC const char * myx_grt_struct_member_get_struct_name(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC const char * myx_grt_struct_member_get_content_struct_name(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC unsigned int myx_grt_struct_member_get_is_ref(MYX_GRT_STRUCT_MEMBER *member);
MYX_PUBLIC_FUNC const char * myx_grt_struct_member_get_content_struct_name_overridden(MYX_GRT_STRUCT_MEMBER *member);

// Internal
MYX_PUBLIC_FUNC int myx_grt_scan_for_structs(MYX_GRT *grt, const char *directory, MYX_GRT_ERROR *error);

// ----------------------------------------------------------------------------------------------------------------------
// GRT Value handling

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_retain(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_value_release(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_value_get_current_reference_count(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_dup(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC void myx_grt_value_listener_add(MYX_GRT *grt, MYX_GRT_VALUE *value, void *user_data, 
                                                int (*listener_callback)(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *user_data));
MYX_PUBLIC_FUNC void myx_grt_value_listener_remove(MYX_GRT_VALUE *value, void *user_data, 
                                                int (*listener_callback_remove)(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *user_data));
MYX_PUBLIC_FUNC int myx_grt_value_listener_get(MYX_GRT *grt, MYX_GRT_VALUE *value, void *user_data, 
                                                int (*listener_callback_get)(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason, void *user_data));
MYX_PUBLIC_FUNC int myx_grt_value_listener_call(MYX_GRT_VALUE *value, MYX_GRT_VALUE_CALLBACK_REASON reason);


MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_diff_make(MYX_GRT *grt, MYX_GRT_VALUE *source, MYX_GRT_VALUE *target);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_diff_apply(MYX_GRT *grt, MYX_GRT_VALUE *value, MYX_GRT_VALUE *diff);
//MYX_GRT_VALUE * myx_grt_value_change_tree_make(MYX_GRT *grt, MYX_GRT_VALUE *original, MYX_GRT_VALUE *modified);

MYX_PUBLIC_FUNC char * myx_grt_value_to_xml(MYX_GRT *grt, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_from_xml(MYX_GRT *grt, const char *str, size_t size);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_retrieve_from_file(MYX_GRT *grt, const char *filename);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_store_to_file(MYX_GRT *grt, MYX_GRT_VALUE *value, const char *filename);

MYX_PUBLIC_FUNC char * myx_grt_value_to_xml_global_object(const char *objectPath, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_from_xml_global_object(MYX_GRT *grt, const char *str, size_t size);

MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_grt_value_get_type(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC const char * myx_get_value_type_as_string(MYX_GRT_VALUE_TYPE value_type);
MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_get_value_type_from_string(const char *value_type_name, MYX_GRT_ERROR *error);
MYX_PUBLIC_FUNC int myx_grt_value_is_simple_type(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_extend(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_value_is_bridged(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_value_has_listeners(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC int myx_grt_value_as_int(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC double myx_grt_value_as_real(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC const char * myx_grt_value_as_string(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC char * myx_grt_value_formated_as_string(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_LIST * myx_grt_value_as_list(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_DICT * myx_grt_value_as_dict(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_from_int(int i);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_from_real(double d);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_value_from_string(const char *s);

MYX_PUBLIC_FUNC int myx_grt_value_print(MYX_GRT *grt, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC char * myx_grt_value_as_lua_code(MYX_GRT_VALUE *value, int depth);

// ----------------------------------------------------------------------------------------------------------------------
// GRT List Values

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_new(MYX_GRT_VALUE_TYPE content_type, const char *struct_name);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_create_from_stringlist(MYX_STRINGLIST *sl);
/*MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_new_proxy(MYX_GRT *grt, MYX_GRT_MODULE *proxy_module, const char *proxy_path, 
                                                       MYX_GRT_VALUE_TYPE content_type, const char *struct_name);*/


MYX_PUBLIC_FUNC MYX_STRINGLIST * myx_grt_list_as_stringlist(MYX_GRT_VALUE *list);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_from_stringlist(MYX_STRINGLIST *str_list);

MYX_PUBLIC_FUNC int myx_grt_list_clear(MYX_GRT_VALUE *list);

// Items
MYX_PUBLIC_FUNC int myx_grt_list_item_insert(MYX_GRT_VALUE *list, int index, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_list_item_add(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_list_item_add_as_string(MYX_GRT_VALUE *list, const char *s);
MYX_PUBLIC_FUNC int myx_grt_list_item_del(MYX_GRT_VALUE *list, int index);
MYX_PUBLIC_FUNC int myx_grt_list_item_del_as_string(MYX_GRT_VALUE *list, const char *s);
MYX_PUBLIC_FUNC int myx_grt_list_item_del_value(MYX_GRT_VALUE *list, MYX_GRT_VALUE *value);

// Items, index based access
MYX_PUBLIC_FUNC unsigned int myx_grt_list_item_count(MYX_GRT_VALUE *list);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_item_get(MYX_GRT_VALUE *list, unsigned int index);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_item_get_reference_value(MYX_GRT *grt, MYX_GRT_VALUE *list, unsigned int index);
MYX_PUBLIC_FUNC const char * myx_grt_list_item_get_as_string(MYX_GRT_VALUE *list, unsigned int index);
MYX_PUBLIC_FUNC int myx_grt_list_item_set(MYX_GRT_VALUE *list, unsigned int index, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_item_get_by_object_name(MYX_GRT_VALUE *list, const char *name);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_list_item_get_reference_value_by_object_name(MYX_GRT *grt, MYX_GRT_VALUE *list, const char *name);
MYX_PUBLIC_FUNC int myx_grt_list_del_by_object_name(MYX_GRT_VALUE *list, const char *name);

// Struct/content handling
MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_grt_list_content_get_type(MYX_GRT_VALUE *list);
MYX_PUBLIC_FUNC const char * myx_grt_list_content_get_struct_name(MYX_GRT_VALUE *list);

MYX_PUBLIC_FUNC void myx_grt_list_content_set_type(MYX_GRT_VALUE *list, MYX_GRT_VALUE_TYPE content_type);
MYX_PUBLIC_FUNC int myx_grt_list_content_set_struct_name(MYX_GRT_VALUE *list, const char *struct_name);

// ----------------------------------------------------------------------------------------------------------------------
// GRT Dictionary Values

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_new(MYX_GRT *grt, const char *struct_name);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_new_typed(MYX_GRT_VALUE_TYPE content_type, const char *content_struct_name);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_create(MYX_GRT *grt, const char *struct_name, const char *key, ...);
//MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_create_from_list(MYX_GRT_VALUE *list);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_new_obj(MYX_GRT *grt, const char *struct_name, const char *name, 
                                                     const char *_id, const char *owner);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_init_obj(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *name, 
                                                      const char *_id, const char *owner);

// Item set functions
MYX_PUBLIC_FUNC int myx_grt_dict_item_set_value(MYX_GRT_VALUE *dict, const char *key, MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC int myx_grt_dict_item_set_value_from_string(MYX_GRT_VALUE *dict, const char *key, const char *s);
MYX_PUBLIC_FUNC int myx_grt_dict_item_set_value_from_int(MYX_GRT_VALUE *dict, const char *key, int i);
MYX_PUBLIC_FUNC int myx_grt_dict_item_set_value_from_real(MYX_GRT_VALUE *dict, const char *key, double d);

// Item get functions
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_item_get_value(MYX_GRT_VALUE *dict, const char *key);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_item_get_reference_value(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *key);
MYX_PUBLIC_FUNC const char * myx_grt_dict_item_get_as_string(MYX_GRT_VALUE *dict, const char *key);
MYX_PUBLIC_FUNC char * myx_grt_dict_item_get_formated_as_string(MYX_GRT_VALUE *dict, const char *key);
MYX_PUBLIC_FUNC int myx_grt_dict_item_get_as_int(MYX_GRT_VALUE *dict, const char *key);
MYX_PUBLIC_FUNC double myx_grt_dict_item_get_as_real(MYX_GRT_VALUE *dict, const char *key);

// Item del function
MYX_PUBLIC_FUNC int myx_grt_dict_item_del(MYX_GRT_VALUE *dict, const char *key);

// Items, index based access
MYX_PUBLIC_FUNC unsigned int myx_grt_dict_item_count(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC const char * myx_grt_dict_item_key_by_index(MYX_GRT_VALUE *dict, unsigned int index);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_item_value_by_index(MYX_GRT_VALUE *dict, unsigned int index);
MYX_PUBLIC_FUNC int myx_grt_dict_item_by_index(MYX_GRT_VALUE *dict, unsigned int index, 
                                          const char **retkey, MYX_GRT_VALUE **retvalue);

// Items, by path
MYX_PUBLIC_FUNC char * myx_get_parent_path(const char *path);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_item_get_by_path(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *path);
MYX_PUBLIC_FUNC int myx_grt_dict_item_set_by_path(MYX_GRT_VALUE *dict, const char *path, MYX_GRT_VALUE *new_value);


// Complex items (dicts, lists), index based access
MYX_PUBLIC_FUNC unsigned int myx_grt_dict_item_count_complex(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC const char * myx_grt_dict_item_key_by_index_complex(MYX_GRT_VALUE *dict, unsigned int index);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_item_value_by_index_complex(MYX_GRT_VALUE *dict, unsigned int index);

// GRT Object function
MYX_PUBLIC_FUNC const char * myx_grt_dict_name_item_as_string(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC const char * myx_grt_dict_id_item_as_string(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_dict_generate_id(MYX_GRT_VALUE *dict);

// Struct/content handling
MYX_PUBLIC_FUNC const char * myx_grt_dict_struct_get_name(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC MYX_GRT_STRUCT * myx_grt_dict_struct_get(MYX_GRT *grt, MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC int myx_grt_dict_struct_set_name(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name);
MYX_PUBLIC_FUNC int myx_grt_dict_struct_set_name_no_cache_register(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name);
MYX_PUBLIC_FUNC int myx_grt_dict_struct_validate(MYX_GRT *grt, MYX_GRT_VALUE *value, const char *struct_name, int strict);

MYX_PUBLIC_FUNC int myx_grt_dict_struct_inherits_from(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *parent_struct_name);
MYX_PUBLIC_FUNC int myx_grt_dict_struct_is_or_inherits_from(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *parent_struct_name);

MYX_PUBLIC_FUNC MYX_GRT_VALUE_TYPE myx_grt_dict_content_get_type(MYX_GRT_VALUE *dict);
MYX_PUBLIC_FUNC const char * myx_grt_dict_content_get_struct_name(MYX_GRT_VALUE *dict);

MYX_PUBLIC_FUNC void myx_grt_dict_content_set_type(MYX_GRT_VALUE *dict, MYX_GRT_VALUE_TYPE content_type);
MYX_PUBLIC_FUNC void myx_grt_dict_content_set_struct_name(MYX_GRT_VALUE *dict, const char *struct_name);

MYX_PUBLIC_FUNC const char * myx_grt_dict_get_object_path(MYX_GRT_VALUE *dict);

// ----------------------------------------------------------------------------------------------------------------------
// Bridge functions

MYX_PUBLIC_FUNC void myx_grt_value_bridge_module_set(MYX_GRT_VALUE *value, MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC MYX_GRT_MODULE * myx_grt_value_bridge_module_get(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC void myx_grt_value_bridge_data_object_set(MYX_GRT_VALUE *value, void *data);
MYX_PUBLIC_FUNC void * myx_grt_value_bridge_data_object_get(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC void myx_grt_value_bridge_data_owner_set(MYX_GRT_VALUE *value, void *data);
MYX_PUBLIC_FUNC void * myx_grt_value_bridge_data_owner_get(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC void myx_grt_value_bridge_dict_key_set(MYX_GRT_VALUE *value, const char *key);
MYX_PUBLIC_FUNC const char * myx_grt_value_bridge_dict_key_get(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC void myx_grt_value_bridge_list_index_set(MYX_GRT_VALUE *value, int index);
MYX_PUBLIC_FUNC int myx_grt_value_bridge_list_index_get(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_value_change_int(MYX_GRT_VALUE *value, int i);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_value_change_real(MYX_GRT_VALUE *value, double d);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_value_change_string(MYX_GRT_VALUE *value, const char *s);

MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value(MYX_GRT_VALUE *dict, const char *key, MYX_GRT_VALUE *value, int do_bridge_callback);
MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_string(MYX_GRT_VALUE *dict, const char *key, const char *s, int do_bridge_callback);
MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_int(MYX_GRT_VALUE *dict, const char *key, int i, int do_bridge_callback);
MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_set_value_from_real(MYX_GRT_VALUE *dict, const char *key, double d, int do_bridge_callback);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_dict_item_get_value(MYX_GRT_VALUE *dict, const char *key, int do_bridge_callback);

MYX_PUBLIC_FUNC unsigned int myx_grt_bridge_dict_item_count(MYX_GRT_VALUE *dict, int do_bridge_callback);
MYX_PUBLIC_FUNC const char * myx_grt_bridge_dict_item_key_by_index(MYX_GRT_VALUE *dict, unsigned int index, int do_bridge_callback);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_dict_item_value_by_index(MYX_GRT_VALUE *dict, unsigned int index, int do_bridge_callback);
MYX_PUBLIC_FUNC int myx_grt_bridge_dict_item_del(MYX_GRT_VALUE *dict, const char *key, int do_bridge_callback);

MYX_PUBLIC_FUNC int myx_grt_bridge_value_as_int(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC double myx_grt_bridge_value_as_real(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC const char * myx_grt_bridge_value_as_string(MYX_GRT_VALUE *value);

MYX_PUBLIC_FUNC unsigned int myx_grt_bridge_list_item_count(MYX_GRT_VALUE *list);
MYX_PUBLIC_FUNC int myx_grt_bridge_list_item_insert(MYX_GRT_VALUE *list, int index, MYX_GRT_VALUE *value, int do_bridge_callback);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_list_item_get(MYX_GRT_VALUE *list, unsigned int index, int do_bridge_callback);
MYX_PUBLIC_FUNC int myx_grt_bridge_list_item_del(MYX_GRT_VALUE *list, int index, int do_bridge_callback);

MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_bridge_dict_new(MYX_GRT *grt, const char *struct_name, void *bridge_data);
MYX_PUBLIC_FUNC int myx_grt_bridge_dict_struct_set_name(MYX_GRT *grt, MYX_GRT_VALUE *dict, const char *struct_name, 
                                                        int register_in_cache, void *bridge_data);

// ----------------------------------------------------------------------------------------------------------------------
// Java module

#ifdef ENABLE_JAVA_MODULES
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_java_init_loader(MYX_GRT *grt,
  const char *class_path, MYX_GRT_ERROR *error, const char *jvm_library,
  const char *grt_class_path_prefix);
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_java_init_loader_advanced(MYX_GRT *grt,
  const char *class_path, MYX_GRT_ERROR *error, const char *jvm_library,
  const char *grt_class_path_prefix, const char *jvm_max_heap);
#endif

// ----------------------------------------------------------------------------------------------------------------------
// PHP module

#ifdef ENABLE_PHP_MODULES
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_php_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error);
#endif

// ----------------------------------------------------------------------------------------------------------------------
// Lua module

#ifdef ENABLE_LUA_MODULES
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_lua_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error, const char *lua_module_path);
#endif

// ----------------------------------------------------------------------------------------------------------------------
// Python module

#ifdef ENABLE_PYTHON_MODULES
MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER * myx_python_init_loader(MYX_GRT *grt, MYX_GRT_ERROR *error, const char *python_module_path);
#endif

// ----------------------------------------------------------------------------------------------------------------------
// Shell Support  

MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_setup_shell(MYX_GRT *grt, MYX_GRT_SHELL_INTERFACE flavour);

MYX_PUBLIC_FUNC int myx_grt_shell_init(MYX_GRT *grt);
MYX_PUBLIC_FUNC char * myx_grt_shell_get_prompt(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_GRT_SHELL_COMMAND myx_grt_shell_execute(MYX_GRT *grt, const char *linebuf);
MYX_PUBLIC_FUNC int myx_grt_shell_run_file(MYX_GRT *grt, const char *file_name, int interactive);
MYX_PUBLIC_FUNC void myx_grt_shell_print_welcome(MYX_GRT *grt);
MYX_PUBLIC_FUNC void * myx_grt_shell_get_interpreter_data(MYX_GRT *grt);
MYX_PUBLIC_FUNC MYX_GRT_VALUE * myx_grt_shell_get_global_var(MYX_GRT *grt, const char *var_name);
MYX_PUBLIC_FUNC int myx_grt_shell_set_global_var(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value);



// Aux funcs
MYX_PUBLIC_FUNC int _myx_grt_get_refcount(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE *make_return_value(MYX_GRT_VALUE *value);
MYX_PUBLIC_FUNC MYX_GRT_VALUE *make_return_value_error(const char *message, const char *detail);

MYX_PUBLIC_FUNC char * myx_grt_get_guid();

#ifdef ENABLE_LUA_MODULES
#include <lua.h>
// Text User Interface stuff
MYX_PUBLIC_FUNC void myx_grt_init_textui(lua_State *l);
#endif


#ifdef __cplusplus
}
#endif

#endif
