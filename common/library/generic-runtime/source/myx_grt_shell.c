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
# include "myx_grt_python.h"
#endif

#include "myx_grt_private.h"
#ifdef ENABLE_LUA_MODULES
# include "myx_grt_lua.h"
#endif



/** 
 ****************************************************************************
 * @brief Setups up a GRT shell environment
 *
 * @param grt the GRT environment
 * @param flavour the desired type of shell. Support for the language should
 *        be compiled in or it will fail
 *
 * @return NO_ERROR if it succeeds
 *         SHELL_UNSUPPORTED if support for the language was not compiled in
 *         SHELL_ALREADY_LOADED if the shell was already initialized
 *****************************************************************************/
MYX_GRT_ERROR myx_grt_setup_shell(MYX_GRT *grt, MYX_GRT_SHELL_INTERFACE flavour)
{
  MYX_GRT_SHELL_CONTEXT *ctx= NULL;
  
  if (grt->shell)
    return MYX_GRT_SHELL_ALREADY_LOADED;
  
  switch (flavour)
  {
  case MYX_GRT_SHELL_LUA:
#ifdef ENABLE_LUA_MODULES
    ctx= myx_grt_setup_lua_shell(grt);
    if (!ctx)
      return MYX_GRT_SHELL_INIT_ERROR;
#endif
    break;
  
  case MYX_GRT_SHELL_PYTHON:
#ifdef ENABLE_PYTHON_MODULES
    ctx= myx_grt_setup_python_shell(grt);
    if (!ctx)
      return MYX_GRT_SHELL_INIT_ERROR;
#endif
    break;
  }
  
  if (!ctx)
    return MYX_GRT_SHELL_UNSUPPORTED;
  
  grt->shell= ctx;
  
  return MYX_GRT_NO_ERROR;
}


int myx_grt_shell_init(MYX_GRT *grt)
{
  g_return_val_if_fail(grt->shell != NULL, -1);

  return (*grt->shell->init)(grt);
}


char * myx_grt_shell_get_prompt(MYX_GRT *grt)
{
  g_return_val_if_fail(grt->shell != NULL, NULL);

  return (*grt->shell->get_prompt)(grt);
}


MYX_GRT_SHELL_COMMAND myx_grt_shell_execute(MYX_GRT *grt, const char *linebuf)
{
  g_return_val_if_fail(grt->shell != NULL, MYX_GRT_SHELL_COMMAND_UNKNOWN);
  
  return (*grt->shell->execute)(grt, linebuf);
}


int myx_grt_shell_run_file(MYX_GRT *grt, const char *file_name, int interactive)
{
  g_return_val_if_fail(grt->shell != NULL, -1);
  
  return (*grt->shell->run_file)(grt, file_name, interactive);
}


void myx_grt_shell_print_welcome(MYX_GRT *grt)
{
  g_return_if_fail(grt->shell != NULL);
  
  (*grt->shell->print_welcome)(grt);
}

  
void *myx_grt_shell_get_interpreter_data(MYX_GRT *grt)
{
  g_return_val_if_fail(grt->shell != NULL, NULL);
  
  return (*grt->shell->get_interpreter_data)(grt);
}


MYX_GRT_VALUE * myx_grt_shell_get_global_var(MYX_GRT *grt, const char *var_name)
{
  g_return_val_if_fail(grt->shell != NULL, NULL);
  
  return (*grt->shell->get_global_var)(grt, var_name);
}


int myx_grt_shell_set_global_var(MYX_GRT *grt, const char *var_name, MYX_GRT_VALUE *value)
{
  g_return_val_if_fail(grt->shell != NULL, -1);
  
  return (*grt->shell->set_global_var)(grt, var_name, value);
}





/**
 * Creates a new path out of the current path and the given dir string. Special folders as "." and ".." are properly
 * parsed.
 *
 * @param curpath The current path.
 * @param dir The new dir string to be attached to the current path.
 * @return A new path, which must yet be checked for validity.
 */
char *myx_grt_get_abspath(const char *curpath, const char *dir)
{
  if (dir == NULL || *dir == '\0' || strcmp(dir, ".")==0)
  {
    // No real path info to add. Simply return a duplicate of what is current.
    return g_strdup(curpath);
  }
  else if (*dir == '/')
    return g_strdup(dir);
  else
  {
    int I;
    gchar** Run;
    gchar* NewPath;
    gchar* New[MAX_NESTING];
    gchar** Current;
    gchar** Append;

    // Split the current and new paths into single tokens.
    Current = g_strsplit(curpath, "/", MAX_NESTING);
    Append = g_strsplit(dir, "/", MAX_NESTING);
    memset(New, 0, sizeof(New));

    // Fill the new parts array with the current path parts initially.
    // In any case we need the root slash, so start from 1 instead 0.
    I = 0;
    New[I++] = "";
    Run = Current;
    while (I < MAX_NESTING && *Run != NULL)
    {
      if (*Run != NULL && **Run != '\0')
        New[I++] = *Run;
      ++Run;
    };

    // Now look through the path to append piece by piece and collect the final path.
    Run = Append;
    while (I < MAX_NESTING && *Run != NULL)
    {
      // Nothing to do if only a single dot was given (meaning the current dir) or no part at all (e.g. //).
      if ((**Run != '\0') && (strcmp(*Run, ".") != 0))
      {
        if (strcmp(*Run, "..") == 0)
        {
          // One level up. Check that we do not go beyond the top level.
          if (I > 1)
            New[--I] = NULL;
        }
        else
        {
          // Anything else is considered a normal path part. Add it to the new list.
          New[I++] = *Run;
        };
      };
      ++Run;
    };

    // Finally create a new path by joining all new path parts.
    // If there is only the root part then the join call will not add a single slash. Do it manually.
    if (New[1] == NULL)
      NewPath = g_strdup("/");
    else
      NewPath = g_strjoinv("/", New);
    g_strfreev(Current);
    g_strfreev(Append);

    return NewPath;
  };
}
