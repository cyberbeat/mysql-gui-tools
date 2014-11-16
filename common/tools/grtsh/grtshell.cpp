/* Copyright (C) 2004 MySQL AB

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

#include <stdlib.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include <string>

#include <myx_grt_public_interface.h>
#ifdef ENABLE_JAVA_MODULES
#include <myx_grt_java.h>
#endif
// #include <myx_sql_resultset.h>

#include <myx_grt_builtin_module_public_interface.h>

#include <myx_util_functions.h>

#ifdef ENABLE_TEXTUI
extern "C" {
extern void myx_lua_init_textui(lua_State *lua);
};
#endif

static char *read_command(MYX_GRT *env)
{
  char *line= NULL;
  //int status;
#ifndef USE_READLINE
  char linebuf[1024];
#endif
  char *prompt= myx_grt_shell_get_prompt(env);

#ifdef USE_READLINE
  line= readline(prompt);
  g_free(prompt);
  if (!line)
    return NULL;
  if (*line)
    add_history(line);
  line= str_g_append(line, "\n");
#else
  printf(prompt);
  g_free(prompt);
  fflush(stdout);
  if (fgets(linebuf, sizeof(linebuf), stdin) <= 0)
  {
    return NULL;
  }
  line= g_strdup(linebuf);
#endif

  return line;
}


static int request_user_input(const char *caption, MYX_GRT_INPUT_OPTIONS options, const char **text, void *user_data)
{
  //int status;
  static char *linebuf= NULL;

#ifdef USE_READLINE
  if (linebuf)
    g_free(linebuf);

  linebuf= readline(caption);
  if (!linebuf)
     return -1;
  add_history(linebuf);
  *text= linebuf;
#else
  if (!linebuf) 
    linebuf= (char*)g_malloc(1024);

  printf(caption);
  fflush(stdout);
  if (fgets(linebuf, 1024, stdin) <= 0)
  {
    return -1;
  }
  *text= linebuf;
#endif

  return 0;
}


static int generate_java_classes(const char *structs_file, const char *output_path_param)
{
  MYX_GRT_STRUCTS *gstructs;
  MYX_GRT_ERROR error;
  char *filename= g_path_get_basename(structs_file);

  // Check that the struct file begins with "structs."
  if ((!str_beginswith(filename, "structs.")) || (!str_endswith(filename, ".xml")))
  {
    fprintf(stdout, "ERROR: The structs file has to begin with \"structs.\" and "
            "end with \".xml\" but is named %s.\n", filename);
    g_free(filename);
    return -1;
  }

  // Load structs
  gstructs= myx_grt_struct_load_list(structs_file, &error);
  if ((!gstructs) || (error != MYX_GRT_NO_ERROR))
  {
    fprintf(stdout, "ERROR (%d): Cannot load structs from %s.\n", error, structs_file);
    g_free(filename);
    return -1;
  }
  else
  {
    char *output_path= g_strdup(output_path_param);
    char *package_name= g_strdup(filename+8);
    char path_sep[]= {MYX_PATH_SEPARATOR, 0};
    int error_no;

    g_free(filename);

    package_name[strlen(package_name)-4]= 0;

    if (output_path[strlen(output_path)-1] != MYX_PATH_SEPARATOR)
      output_path= str_g_append(output_path, path_sep);
    
    myx_mkdir(output_path, 0755, &error_no);
    
    error= myx_grt_struct_export_java_classes(gstructs, package_name, output_path);
    g_free(package_name);
    
    if (error != MYX_GRT_NO_ERROR)
    {
      fprintf(stdout, "ERROR (%d): Cannot generate java class files in %s.\n", error, output_path);
      return -1;
    }
    g_free(output_path);
  }
  return 0;
}

static int generate_php_classes(const char *structs_file, const char *output_path_param)
{
  MYX_GRT_STRUCTS *gstructs;
  MYX_GRT_ERROR error;
  char *filename= g_path_get_basename(structs_file);

  // Check that the struct file begins with "structs."
  if ((!str_beginswith(filename, "structs.")) || (!str_endswith(filename, ".xml")))
  {
    fprintf(stdout, "ERROR: The structs file has to begin with \"structs.\" and "
            "end with \".xml\" but is named %s.\n", filename);
    g_free(filename);
    return -1;
  }

  // Load structs
  gstructs= myx_grt_struct_load_list(structs_file, &error);
  if ((!gstructs) || (error != MYX_GRT_NO_ERROR))
  {
    fprintf(stdout, "ERROR (%d): Cannot load structs from %s.\n", error, structs_file);
    g_free(filename);
    return -1;
  }
  else
  {
    char *output_path= g_strdup(output_path_param);
    char path_sep[]= {MYX_PATH_SEPARATOR, 0};
    int error_no;

    g_free(filename);

    if (output_path[strlen(output_path)-1] != MYX_PATH_SEPARATOR)
      output_path= str_g_append(output_path, path_sep);
    
    myx_mkdir(output_path, 0755, &error_no);
    
    error= myx_grt_struct_export_php_classes(gstructs, output_path);
    
    if (error != MYX_GRT_NO_ERROR)
    {
      fprintf(stdout, "ERROR (%d): Cannot generate php class files in %s.\n", error, output_path);
      return -1;
    }
    g_free(output_path);
  }
  return 0;
}


static void process_messages(MYX_GRT_MSGS *msgs, void *user_data)
{
  unsigned int i;

  if (!msgs) return;

  for (i= 0; i < msgs->msgs_num; i++)
  {
    MYX_GRT_MSG *msg= msgs->msgs+i;
    char *type;

    switch (msg->msg_type)
    {
      case 0: type= ""; break;
      case 1: type= " ERROR: "; break;
      case 2: type= ""; break;
      default: type= NULL; break;
    }

    if ((msg->msg) && (msg->msg[0]))
    {
      if (type)
        printf("%s%s\n", type, msg->msg);
      else
        printf("GRT%i: %s\n", msg->msg_type, msg->msg);
    }
    if (msg->msg_detail)
    {
      unsigned int j;
      for (j= 0; j < msg->msg_detail->strings_num; j++)
      {
        printf("\t%s\n", msg->msg_detail->strings[j]);
      }
    }
  }
}

void process_output(const char *text, void *user_data)
{
  printf("%s", text);
}


MYX_GRT *initialize_grt(MYX_GRT_SHELL_INTERFACE shell_type, 
                        std::string basedir,
                        const char *classpath, const char *jvm_path, int interactive, int use_forms, int grt_options,
                        int listen_port)
{
  MYX_GRT *grt= myx_grt_initialize(grt_options);
  MYX_GRT_MODULE_LOADER *loader;
  MYX_GRT_ERROR error;
  unsigned int c;
  
  if (basedir.empty())
    basedir= "./";

  if (myx_grt_setup_shell(grt, shell_type) != MYX_GRT_NO_ERROR)
  {
    g_error("could not initialize shell");
    exit(1);
  }
  myx_grt_set_message_callback(grt, NULL, &process_messages);
  myx_grt_set_output_callback(grt, NULL, &process_output);

  if (interactive)
    myx_grt_shell_print_welcome(grt);

  if (interactive)
    printf("\nScanning for struct definitions in %sxml ...\n", basedir.c_str());
  c= myx_grt_scan_for_structs(grt, std::string(basedir+"xml").c_str(), &error);
  if (error != MYX_GRT_NO_ERROR)
  {
    g_warning("Error while scanning for struct definitions (%i).", error);
  }
  else if (interactive)
    printf("Registered %i struct definition files.\n", c);

  if (interactive)
    printf("Initializing Builtin modules...\n");
  myx_register_builtin_grt_module_base(grt);
  myx_register_builtin_grt_module_reverse_engineer_mysql(grt);
  myx_register_builtin_grt_module_reverse_engineer_mysql_script(grt);
  myx_register_builtin_grt_module_transformation_mysql(grt);
  //myx_register_builtin_grt_module_query_mysql(grt);
  //myx_register_builtin_grt_module_result_set(grt);
  //myx_register_builtin_grt_module_result_set_source(grt);

#ifdef ENABLE_FORMS
#if !defined(__WIN__) && !defined(_WIN32) && !defined(_WIN64)
  // register forms module
  if (use_forms)
    myx_register_builtin_grt_module_forms(grt);
#endif
#endif

  // initialize the loaders

#ifdef ENABLE_JAVA_MODULES
  // initialize Java loader
  if (g_file_test(std::string(basedir+"java").c_str(), G_FILE_TEST_IS_DIR))
  {
    if (interactive)
      printf("Initializing Java loader...\n");
    loader= myx_java_init_loader(grt, classpath, &error, jvm_path, basedir.c_str());
    if (!loader)
    {
      g_warning("Error initializing Java module loader (%i).", error);
    }
    else
    {
      if (myx_grt_register_module_loader(grt, loader) < 0)
      {
        g_warning("Could not register Java module loader.");
      }
      else
      {
        // Scan for Java modules
        if (interactive)
          printf("Scanning for Java modules in %sjava/com/mysql/grt/modules ...\n", basedir.c_str());

        c= myx_grt_scan_for_modules(grt, std::string(basedir+"java/com/mysql/grt/modules").c_str(), &error);
        if (error!=MYX_GRT_NO_ERROR)
        {
          g_warning("Error while scanning for Java modules (%i).", error);
        }
        else if (interactive)
          printf("Registered %i modules.\n", c);
      }
    }
  }
#endif

#ifdef ENABLE_PHP_MODULES
  // initialize PHP loader
  if (g_file_test(std::string(basedir+"php").c_str(), G_FILE_TEST_IS_DIR))
  {
    if (interactive)
      printf("Initializing PHP loader...\n");
    loader= myx_php_init_loader(grt, &error);
    if (!loader)
    {
      g_warning("Error initializing PHP module loader (%i).", error);
    }
    else
    {
      if (myx_grt_register_module_loader(grt, loader) < 0)
      {
        g_warning("Could not register PHP module loader.");
      }
      else
      {
        // Scan for PHP modules
        if (interactive)
          printf("Scanning for php modules in %sphp/modules ...\n", basedir.c_str());

        c= myx_grt_scan_for_modules(grt, std::string(basedir+"php/modules").c_str(), &error);
        if (error!=MYX_GRT_NO_ERROR)
        {
          g_warning("Error while scanning for PHP modules (%i).", error);
        }
        else if (interactive)
          printf("Registered %i modules.\n", c);
      }
    }
  }
#endif

#ifdef ENABLE_PYTHON_MODULES
  if (interactive)
    printf("Initializing Python loader...\n");
  
  loader= myx_python_init_loader(grt, &error, std::string(basedir + "python").c_str());
  if (!loader)
  {
    g_warning("Error initializing Python module loader (%i).", error);
  }
  else
  {
    if (myx_grt_register_module_loader(grt, loader) < 0)
    {
      g_warning("Could not register Python module loader.");
    }
    else
    {
      if (g_file_test(std::string(basedir + "python").c_str(), G_FILE_TEST_IS_DIR))
      {
        // Scan for Python modules
		if (interactive)
          printf("Scanning for Python modules in %spython ...\n", basedir.c_str());
        c= myx_grt_scan_for_modules(grt, std::string(basedir + "python").c_str(), &error);
        if (error!=MYX_GRT_NO_ERROR)
        {
          g_warning("Error while scanning for Python modules (%i).", error);
        }
        else if (interactive)
          printf("Registered %i modules.\n", c);
      }
    }
  }
#endif

  // initialize Lua loader
  if (interactive)
    printf("Initializing Lua loader...\n");
  loader= myx_lua_init_loader(grt, &error, std::string(basedir+"lua").c_str());
  if (!loader)
  {
    g_warning("Error initializing Lua module loader (%i).", error);
  }
  else
  {
    if (myx_grt_register_module_loader(grt, loader) < 0)
    {
      g_warning("Could not register Lua module loader.");
    }
  }

  // register
  if (g_file_test(std::string(basedir+"lua").c_str(), G_FILE_TEST_IS_DIR))
  {
    if (loader)
    {
      // Scan for Lua modules
      if (interactive)
        printf("Scanning for Lua plugins in %slua ...\n", basedir.c_str());
      
      c= myx_grt_scan_for_modules(grt, std::string(basedir+"lua").c_str(), &error);
      if (error != MYX_GRT_NO_ERROR)
      {
        g_warning("Error while scanning for Lua modules (%i).", error);
      }
      else if (interactive)
        printf("Registered %i modules.\n", c);
    }
  }

  if (interactive)
    printf("\n");

  
  myx_grt_shell_init(grt);
  
#ifdef ENABLE_TEXTUI
  if (shell_type == MYX_GRT_SHELL_LUA)
    myx_lua_init_textui((lua_State*)myx_grt_shell_get_interpreter_data(grt));
#endif

  return grt;
}


static void show_help(const char *argv0)
{
    fprintf(stdout,
      "Usage: %s [-classpath path] [-modulepath path] [-jvm library] [-d path] [-listen port] [-verbose] [-x] [luafile] \n"
      "       %s -j structsfile outputdir\n"    
      "       %s -p structsfile outputdir\n"
      "\n"
      "  -lua ......... Use the Lua shell (default).\n"
#ifdef ENABLE_PYTHON_MODULES
      "  -py .......... Use the Python shell.\n"
#endif
      "  -classpath ... Sets the java classpath to the given value.\n"
      "  -modulepath .. Sets the location of the GRT module directory.\n"
      "  -jvm ......... The java virtual machine library to use (with absolute path).\n"
      "  -basedir ..... Path to the data files location.\n"
      "  -d path ...... Modules directory\n"
      "  -x ........... Exits the shell after running the specified file\n"
      "  luafile ...... File that is run at startup.\n\n"
      "  -listen port . Runs in 'remote agent' mode on the given port number.\n"
      "  -verbose ..... Prints detailed startup information.\n"
      "  -j ........... Generates Java classes from the given structs file.\n"
      "  -p ........... Generates PHP classes from the given structs file.\n"
      "  -D var=value . Sets a global shell variable to the given value.\n"
      "Environment variables:\n"
      "GRT_MODULE_PATH  Equivalent to -modulepath, must point to the directory\n"
      "                 where the grtsh binary resides\n"
      "\n", argv0, argv0, argv0);
}

int main(int argc, char **argv)
{
  MYX_GRT_ERROR status;
  MYX_GRT *grt;
  MYX_GRT_SHELL_INTERFACE shell_type= MYX_GRT_SHELL_LUA;
  int grt_options= 0;
  char *classpath= NULL;
  char *modulepath= NULL;
  char *jvm_path= NULL;
  char *linebuf= NULL;
  char *load_file= NULL;
  char *module_dir= NULL;
  std::string basedir;
  int interactive= 1;
  int use_forms= 0;
  int listen_port= 0;
  int fcgi_port= 0;
  char *startup_path= NULL;
  int c, i;
  char *prgname= strchr(argv[0], '/');

  g_set_prgname(prgname ? prgname+1 : prgname);
  
  // ---------------------------------------------------------------------------------------------------------------
  // If the shell is called without arguments, display usage
  if ((argc > 1) && (strcmp(argv[1], "-?") == 0))
  {
    show_help(argv[0]);
    exit(1);
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Handle java class generation

  if ((argc>1) && (strcmp(argv[1], "-j")==0))
  {    
    if (argc != 4)
    {
      show_help(argv[0]);
      exit(1);
    }
    else
    {
      if (generate_java_classes(argv[2], argv[3])!=0)
        exit(1);
      else
        exit(0);
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Handle php class generation

  if ((argc>1) && (strcmp(argv[1], "-p")==0))
  {    
    if (argc != 4)
    {
      show_help(argv[0]);
      exit(1);
    }
    else
    {
      if (generate_php_classes(argv[2], argv[3])!=0)
        exit(1);
      else
        exit(0);
    }
  }


  // ---------------------------------------------------------------------------------------------------------------
  // Handle module_dir and interactive etc
  for (i= 1; i < argc; i++)
  {
    if (strcmp(argv[i], "-lua")==0)
      shell_type= MYX_GRT_SHELL_LUA;
    else if (strcmp(argv[i], "-py")==0)
      shell_type= MYX_GRT_SHELL_PYTHON;
    else if (strcmp(argv[i], "-x") == 0)
      interactive= 0;
    else if (strcmp(argv[i], "-verbose") == 0)
      grt_options|= MYX_GRT_VERBOSE;
    else if (strcmp(argv[i], "-debug") == 0)
      grt_options|= 2;
    else if (strcmp(argv[i], "-listen") == 0)
    {
      interactive= 0;

      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        listen_port= atoi(argv[++i]);
    }
    else if (strcmp(argv[i], "-d")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        module_dir= argv[++i];
    }
    else if (strcmp(argv[i], "-classpath")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        classpath= argv[++i];
    }
    else if (strcmp(argv[i], "-modulepath")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        modulepath= argv[++i];
    }
    else if (strcmp(argv[i], "-basedir")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        basedir= argv[++i];
    }
    else if (strcmp(argv[i], "-jvm")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        jvm_path= argv[++i];
    }
    else if (strcmp(argv[i], "-D")==0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        i++;
    }
    else if (strcmp(argv[i], "-h")==0 || strcmp(argv[i], "-help")==0)
    {
      show_help(argv[0]);
      exit(1);
    }
#ifdef ENABLE_FASTCGI
    else if (strcmp(argv[i], "-fcgi")==0)
    {
      interactive= 0;

      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
        fcgi_port= atoi(argv[++i]);
    }
#endif
    else
      load_file= argv[i];
  }
  
  if (listen_port > 0)
    g_thread_init(NULL);
  
  if (!modulepath)
    modulepath= getenv("GRT_MODULE_PATH");

  if (!modulepath)
  {
    if (strchr(argv[0], '/'))
    {
      modulepath= g_path_get_dirname(argv[0]);
    }
  }

  if (modulepath)
  {
    startup_path= g_get_current_dir();
    if (myx_chdir(modulepath) < 0)
    {
      g_message("ERROR: error changing directory to %s: %s", modulepath,
                strerror(errno));
      exit(1);
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  //Initialize grt
  grt= initialize_grt(shell_type, basedir, classpath, jvm_path, interactive, use_forms, grt_options, listen_port);
  if (!grt)
  {
    g_message("ERROR: The GRT environment cannot be initialized.");
    exit(1);
  }

  if (startup_path)
  {
    myx_chdir(startup_path);
  }
  
  //g_assert(lua_gettop(myx_grt_lua_shell_get_lua(grt))==0);

  // ---------------------------------------------------------------------------------------------------------------
  //Set global values
  for (i= 1; i < argc; i++)
  {
    if (strcmp(argv[i], "-D") == 0)
    {
      if (argc <= i+1)
      {
        show_help(argv[0]);
        exit(1);
      }
      else
      {
        char *param= argv[++i];
        char *var_name= g_strdup(param);
        char *var_val= g_strdup(param);
          
        name_of_str(var_name, var_name);
        value_of_str(var_val, var_val);

        if (var_val[0])
        {
          MYX_GRT_VALUE *val= myx_grt_value_from_string(var_val);

          myx_grt_shell_set_global_var(grt, var_name, val);
        }
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Scan for plugins
  if (module_dir)
  {
    if (interactive)
      g_message("Scanning for plugins in %s...", module_dir);
    c= myx_grt_scan_for_modules(grt, module_dir, &status);

    if (interactive)
      g_message("Initialized %i modules", c);
  }
  //g_assert(lua_gettop(myx_grt_lua_shell_get_lua(grt))==0);

  if (listen_port == 0)
  {
    // Setup user input request handler
    myx_grt_set_input_callback(grt, NULL, request_user_input);
  }
  else
  {
    // Setup user input request handler for agent (needs to callback the caller)
  }
  
  // ---------------------------------------------------------------------------------------------------------------
  // Run script
  if ((!interactive) && (!fcgi_port))
  {
    if (load_file)
      exit(myx_grt_shell_run_file(grt, load_file, interactive));
    else
      exit(0);
  } 
  else if (load_file)
  {
    myx_grt_shell_run_file(grt, load_file, interactive);

    myx_grt_messages_stack_flush(grt, 0);
  }

  if ((listen_port == 0) && (fcgi_port == 0))
  {    
    // ---------------------------------------------------------------------------------------------------------------
    // Main interactive loop
    for (;;)
    {
      //MYX_GRT_VALUE *error;

      linebuf= read_command(grt);
      if (!linebuf)
      {
        break;
      }
      
      if(myx_grt_shell_execute(grt, linebuf) == MYX_GRT_SHELL_COMMAND_EXIT)
        break;

      // check for error
      /*error= myx_grt_shell_get_global_var(grt, "grtError");
      if (error)
      {
        printf("ERROR: %s\n", myx_grt_dict_item_get_as_string(error, "error"));
        if (myx_grt_dict_item_get_as_string(error, "detail"))
          printf("%s\n", myx_grt_dict_item_get_as_string(error, "detail"));
      }*/
      
      myx_grt_messages_stack_flush(grt, 0);
      
      g_free(linebuf);
    }
  }
  else if (fcgi_port)
  {
#ifdef ENABLE_FASTCGI
    g_message("Starting fcgi on port %i...", fcgi_port);
    myx_grt_start_fcgi(grt, fcgi_port, NULL, 0);
#else
    g_message("fcgi not enabled on binary");
#endif
  }
  else
  {
    g_message("Starting agent on port %i...", listen_port);
    myx_grt_start_agent(grt, listen_port, NULL, 0);
  }

  myx_grt_finalize(grt);
  
  return 0;
}
