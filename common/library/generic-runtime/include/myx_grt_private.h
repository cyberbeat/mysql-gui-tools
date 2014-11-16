/* Copyright (C) 2004, 2005 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */


#ifndef __GRT_PRIVATE_H__
#define __GRT_PRIVATE_H__

#include "myx_grt_public_interface.h"

#ifdef __cplusplus
extern "C" {   
#endif /* __cplusplus */

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#define MYX_PUBLIC_FUNC __declspec(dllexport)
#else
#define MYX_PUBLIC_FUNC
#endif

#ifdef __BORLANDC__
  #define __FUNCTION__ __FUNC__
#endif

struct MYX_GRT 
{
  // the external modules that we know about
  unsigned int modules_num;
  MYX_GRT_MODULE **modules;
  
  struct MYX_GRT_LISTENER *listeners;

  struct MYX_GRT_SHELL_CONTEXT *shell;

  GStaticRecMutex global_mutex;

  // the structure definitions that we know about
  unsigned int structs_num;
  MYX_GRT_STRUCT *structs;

  // the root GRT object
  MYX_GRT_VALUE *root;

  unsigned int loaders_num;
  MYX_GRT_MODULE_LOADER **loaders;
  
  MYX_GRT_PRINT_CALLBACK print;
  void *print_data;

  MYX_GRT_MESSAGE_CALLBACK process_messages;
  void *process_messages_data;

  MYX_GRT_INPUT_CALLBACK process_input;
  void *process_input_data;

  MYX_GRT_STATUS_QUERY_CALLBACK process_status_query;
  void *process_status_query_data;


  GHashTable *struct_icon_cache;

  GHashTable *global_reference_cache;
  GHashTable *direct_reference_cache;
  
  MYX_GRT_MSGS *msgs;

  MYX_GRT_LOG_CALLBACK logfunc;

  int options;
};
  

typedef struct MYX_GRT_LISTENER 
{
  char *wanted_name;
  void *userdata;
  void (*callback)(MYX_GRT *grt, char *name, void *argument, void *userdata);
  struct MYX_GRT_LISTENER *next;
} MYX_GRT_LISTENER;


struct MYX_GRT_MODULE_LOADER
{
  MYX_GRT *grt;
  MYX_GRT_MODULE_TYPE loader_type;

  unsigned int extensions_num;
  char **extensions;
  
  MYX_GRT_ERROR (*init_module)(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule);
  MYX_GRT_ERROR (*call_function)(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *argument, MYX_GRT_VALUE **retval);
  
  struct MYX_GRT_MODULE_LOADER_PRIVATE *priv;
};
  
typedef struct MYX_GRT_SHELL_CONTEXT
{
  MYX_GRT_SHELL_INTERFACE type;
  struct MYX_GRT_SHELL_PRIVATE *data;

  int (*init)(MYX_GRT *grt);
  void (*print_welcome)(MYX_GRT *grt);
  char *(*get_prompt)(MYX_GRT *grt);
  MYX_GRT_SHELL_COMMAND (*execute)(MYX_GRT *grt, const char *linebuf);
  int (*run_file)(MYX_GRT *grt, const char *file_name, int interactive);

  void *(*get_interpreter_data)(MYX_GRT *grt);

  MYX_GRT_VALUE *(*get_global_var)(MYX_GRT *grt, const char *var_name);
  int (*set_global_var)(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value);
} MYX_GRT_SHELL_CONTEXT;

  
#define MYX_SHELL_CURNODE "current"
  

MYX_GRT_ERROR myx_grt_module_init_loaders(MYX_GRT *grt);

MYX_PUBLIC_FUNC MYX_GRT_MODULE_LOADER *myx_grt_get_loader_of_type(MYX_GRT *grt, MYX_GRT_MODULE_TYPE type);
MYX_PUBLIC_FUNC MYX_GRT_ERROR myx_grt_add_module(MYX_GRT *grt, MYX_GRT_MODULE *module);
MYX_PUBLIC_FUNC MYX_GRT_MODULE* myx_grt_find_module(MYX_GRT *grt, char* name);
MYX_PUBLIC_FUNC void myx_grt_remove_module(MYX_GRT *grt, MYX_GRT_MODULE *module);

void myx_grt_shell_show_help(MYX_GRT *grt, const char *command);
void myx_grt_shell_show_command_help(MYX_GRT *grt, const char *command);

char *myx_grt_get_abspath(const char *curpath, const char *dir);

MYX_GRT_MODULE_LOADER *myx_builtin_init_loader(MYX_GRT *grt);

void myx_grt_parse_function_spec(const char *spec, MYX_GRT_FUNCTION *func);

#define MYX_PRINT(grt, msg) grt->print(msg, grt->print_data)

int myx_grt_printf(MYX_GRT *grt, const char *fmt, ...);

int myx_grt_free_list(MYX_GRT_LIST *list);
int myx_grt_free_dict(MYX_GRT_DICT *dict);

int myx_grt_setup_messaging(MYX_GRT *grt);

void myx_grt_rescan_reference_cache(MYX_GRT *grt);


// internal listeners
#define GRT_MODULE_ADD_NOTIFICATION "GrtModuleAdded"
  
void myx_grt_add_listener(MYX_GRT *grt, void (*callback)(MYX_GRT*,char*,void*,void*),
                          char *name, void *userdata);
void myx_grt_notify_listeners(MYX_GRT *grt, char *name, void *argument);


// set this to 0 to enable the debugging output for thread locking
#if 1

# define GRT_ENTER(grt) g_static_rec_mutex_lock(&grt->global_mutex)

# define GRT_LEAVE(grt) g_static_rec_mutex_unlock(&grt->global_mutex)

//# define GRT_RETURN(grt, retval) do { g_static_rec_mutex_unlock(&grt->global_mutex); return retval; } while (0)

# define GRT_RETURN(grt, retval, rettype) do { rettype __retval__= retval; g_static_rec_mutex_unlock(&grt->global_mutex); return __retval__; } while (0)

#ifdef __GNUC__
# define GRT_RETURN_VAL_IF_FAIL(grt, expr, val)  do { \
  if (!(expr)) \
  { \
    g_log (G_LOG_DOMAIN,                                           \
             G_LOG_LEVEL_CRITICAL,                                   \
             "file %s: line %d (%s): assertion `%s' failed",         \
             __FILE__,                                               \
             __LINE__,                                               \
             __PRETTY_FUNCTION__,                                    \
             #expr);                                                 \
    g_static_rec_mutex_unlock(&grt->global_mutex); \
    return (val);                                                  \
  };\
} while (0)
#else
# define GRT_RETURN_VAL_IF_FAIL(grt, expr, val)  do { \
  if (!(expr)) \
  { \
    g_log (G_LOG_DOMAIN,                                           \
             G_LOG_LEVEL_CRITICAL,                                   \
             "file %s: line %d (%s): assertion `%s' failed",         \
             __FILE__,                                               \
             __LINE__,                                               \
             __FUNCTION__,                                    \
             #expr);                                                 \
    g_static_rec_mutex_unlock(&grt->global_mutex); \
    return (val);                                                  \
  };\
} while (0)
#endif // !__GNUC__

#else

# define GRT_ENTER(grt) do {\
  g_message("Entering GRT function %s [pre %i]", __FUNCTION__, grt->global_mutex.depth);\
  g_static_rec_mutex_lock(&grt->global_mutex);\
} while (0)

# define GRT_LEAVE(grt) do {\
  g_static_rec_mutex_unlock(&grt->global_mutex);\
  g_message("Left GRT function %s [post %i]", __FUNCTION__, grt->global_mutex.depth);\
} while (0)

# define GRT_RETURN(grt, retval, rettype) do {\
  rettype __retval__= retval;\
  g_static_rec_mutex_unlock(&grt->global_mutex);\
  g_message("Return from GRT function %s [post %i]", __FUNCTION__, grt->global_mutex.depth);\
  return __retval__;\
} while (0)

#ifdef __GNUC__

# define GRT_RETURN_VAL_IF_FAIL(grt, expr, val)  do { \
  if (!(expr)) \
  { \
    g_log (G_LOG_DOMAIN,                                           \
             G_LOG_LEVEL_CRITICAL,                                   \
             "file %s: line %d (%s): assertion `%s' failed",         \
             __FILE__,                                               \
             __LINE__,                                               \
             __PRETTY_FUNCTION__,                                    \
             #expr);                                                 \
    g_static_rec_mutex_unlock(&grt->global_mutex); \
    g_message("Returning from GRT function %s [post %i]", __FUNCTION__, grt->global_mutex.depth);\
    return (val);                                                  \
  };\
} while (0)

#else /* !__GNUC__ */

# define GRT_RETURN_VAL_IF_FAIL(grt, expr, val)  do { \
  if (!(expr)) \
  { \
    g_log (G_LOG_DOMAIN,                                           \
             G_LOG_LEVEL_CRITICAL,                                   \
             "file %s: line %d (%s): assertion `%s' failed",              \
             __FILE__,                                               \
             __LINE__,                                               \
             __FUNCTION__,					     \
             #expr);                                                 \
    g_static_rec_mutex_unlock(&grt->global_mutex); \
    g_message("Return from GRT function %s [post %i]", __FUNCTION__, grt->global_mutex.depth);\
    return (val);                                                  \
  };\
} while (0)

#endif /* !__GNUC__ */

#endif


#if !defined(__WIN__) && !defined(_WIN32) && !defined(_WIN64)
#define NL "\n"
#define NLNL "\n\n"
#else
#define NL "\r\n"
#define NLNL "\r\n\r\n"
#endif

#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */

