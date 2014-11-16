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


#ifdef ENABLE_JAVA_MODULES

#include "myx_grt_java.h"
#include "myx_shared_util_functions.h"

#ifdef __GNUC__
#define JAVA_PATH_SEPARATOR ":"
#else
#define JAVA_PATH_SEPARATOR ";"
#endif

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
#include <Basetsd.h>
#define DLOAD_JVM
#else

#ifdef DLOAD_JVM
#include <dlfcn.h>
#endif

#define LongToPtr(l) ((void*)(long)(l))
#define PtrToLong(p) ((long)(p))
#endif

#define GREF(env, r) (*env)->NewGlobalRef(env, r)
#define GRTCLASSPREFIX "com/mysql/grt/"

static MYX_GRT_ERROR java_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval);
static MYX_GRT_ERROR java_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule);

static jobject j_xml_object_from_grt_value(MYX_GRT *grt, JNIEnv *env, MYX_GRT_VALUE *value);
static MYX_GRT_VALUE *j_grt_value_from_java_xml_object(MYX_GRT *grt, JNIEnv *env, jobject object);
static jobject j_call_method(JNIEnv *env, jclass theclass, jobject object, const char *name, const char *signature, jvalue *args);
static jobject j_call_static_method(JNIEnv *env, jclass theclass, const char *name, const char *signature, jvalue *args);
static char *j_get_string(JNIEnv *env, jobject strobj);
//static MYX_GRT_LIST *j_get_string_list(MYX_JAVA_LOADER *jloader, jobject lobj);

static MYX_GRT_ERROR java_cache_java_objects_and_methods(MYX_GRT *grt, MYX_JAVA_LOADER *priv);

#ifdef DLOAD_JVM
typedef jint (JNICALL *CreateJavaVM_t)(JavaVM **pvm, void **env, void *args);

static int get_java_runtimelib_filename(const char *version, char **runtimelib_filename);
#endif

static JavaVM *global_jvm= NULL;

MYX_GRT_MODULE_LOADER *myx_java_init_loader(MYX_GRT *grt, const char *class_path, MYX_GRT_ERROR *error, const char *jvm_library,
                                            const char *grt_class_path_prefix)
{
  myx_java_init_loader_advanced(grt, class_path, error, jvm_library, grt_class_path_prefix, "");
}

MYX_GRT_MODULE_LOADER *myx_java_init_loader_advanced(MYX_GRT *grt, const char *class_path, MYX_GRT_ERROR *error, const char *jvm_library,
                                            const char *grt_class_path_prefix, const char *jvm_max_heap)
{
  MYX_GRT_MODULE_LOADER *loader= g_new0(MYX_GRT_MODULE_LOADER, 1);
  MYX_JAVA_LOADER *priv= g_new0(MYX_JAVA_LOADER, 1);
  JavaVMInitArgs vm_init_args;
  JavaVMOption jvm_options[7];
  jclass grt_class;
  static char *file_extensions[]= {
    ".class"
  };
  int res;
  char *grt_class_path;
  char *grt_class_path_libraries_dir;
  char *grt_class_path_libraries= NULL;
  GDir *dir;
  const char *entry;
  jmethodID getInstance_methodID;
  jmethodID setMyxGrtPointer_methodID;
  jmethodID setCallback_methodID;
  jobject grt_object;
  jvalue *args= NULL;
  char *runtimelib_filename;
#ifdef DLOAD_JVM
# if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  HINSTANCE handle;
# else
  void *handle;
# endif
  CreateJavaVM_t create_java_vm;
#endif

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  grt_class_path_libraries_dir= g_build_filename(grt_class_path_prefix, "java/lib", NULL);

  dir= g_dir_open(grt_class_path_libraries_dir, 0, NULL);
  if (!dir)
  {
    grt_class_path= g_strdup(grt_class_path_prefix);

    grt_class_path_libraries_dir= g_build_filename(grt_class_path_prefix, "lib", NULL);

    dir= g_dir_open(grt_class_path_libraries_dir, 0, NULL);
    if (!dir)
    {
      g_free(grt_class_path_libraries_dir);
      grt_class_path_libraries_dir= g_build_filename(grt_class_path_prefix, "..\\lib", NULL);

      dir= g_dir_open(grt_class_path_libraries_dir, 0, NULL);
      if (!dir)
      {
        *error= MYX_GRT_BAD_PATH;
        goto errorfree;
      }
      else
        g_dir_close(dir);
    }
    else
      g_dir_close(dir);
  }
  else
  {
    g_dir_close(dir);
    grt_class_path= g_build_filename(grt_class_path_prefix, "java", NULL);
  };

  dir= g_dir_open(grt_class_path, 0, NULL);
  if (!dir)
  {
    *error= MYX_GRT_BAD_PATH;
    goto errorfree;
  }
  else
    g_dir_close(dir);

#else
  grt_class_path= g_build_filename(grt_class_path_prefix, "java", NULL);
  grt_class_path_libraries_dir= g_build_filename(grt_class_path_prefix, "java/lib", NULL);
#endif

  GRT_ENTER(grt);

  *error= MYX_GRT_NO_ERROR;

  loader->grt= grt;
  loader->loader_type= MYX_JAVA_MODULE_TYPE;
  loader->priv= priv;
  loader->init_module= java_init_module;
  loader->call_function= java_call_function;
  loader->extensions_num= 1;
  loader->extensions= file_extensions;

  memset(&vm_init_args, 0, sizeof(vm_init_args));
  vm_init_args.version = JNI_VERSION_1_4;
#ifndef DLOAD_JVM
  JNI_GetDefaultJavaVMInitArgs(&vm_init_args);
#endif

  if (grt->options & MYX_GRT_REMOTE_DEBUG)
  	vm_init_args.nOptions = 6;
  else
  	vm_init_args.nOptions = 2;

  if (jvm_library && jvm_library[0])
  {
    runtimelib_filename= g_strdup(jvm_library);
  }
#ifdef DLOAD_JVM
  else
  {
		//try to locate the JVM .dll

		// Developed against 1.5.0_08
		*error= get_java_runtimelib_filename("1.5", &runtimelib_filename);
		if (*error == MYX_GRT_JAVA_NOT_FOUND) {
			// tested against 1.4.2_08
			*error= get_java_runtimelib_filename("1.4", &runtimelib_filename);
			if (*error == MYX_GRT_JAVA_NOT_FOUND)
				// tested against 1.6.0
      	*error= get_java_runtimelib_filename("1.6", &runtimelib_filename);
		}

    if (*error != MYX_GRT_NO_ERROR)
    {
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
      myx_grt_messages_stack_add_error(grt, "The JRE 1.4 or newer cannot be found." _br
        "Please make sure the Sun JRE 1.4 or newer is installed correctly." _br
        "Verify the following registry key:" _br
        "HKEY_LOCAL_MACHINE\\SOFTWARE\\JavaSoft\\Java Runtime Environment\\1.4\\RuntimeLib",
                                       NULL, 0, 0);
#else
      myx_grt_messages_stack_add_error(grt, "The JRE 1.4 or newer cannot be found." _br
		"Please make sure the Sun JRE 1.4 or newer is installed correctly." _br
        "Verify the GRT_JVM_PATH is set to the directory libjvm.so is located.",
                                       NULL, 0, 0);
#endif
      goto errorfree;
    }
  }
#endif // DLOAD_JVM

  //scan for .jars in the lib directory
  dir= g_dir_open(grt_class_path_libraries_dir, 0, NULL);
  if (!dir)
  {
    *error= MYX_GRT_BAD_PATH;
    goto errorfree;
  }

  while ((entry= g_dir_read_name(dir)) != NULL)
  {
    char *path= g_build_filename(grt_class_path_libraries_dir, entry, NULL);

    grt_class_path_libraries= str_g_append_and_free(
		grt_class_path_libraries, g_strdup_printf("%s%s",
          path, JAVA_PATH_SEPARATOR));

    g_free(path);
  }
  g_dir_close(dir);

  //base class path
  jvm_options[0].optionString= g_strdup_printf("-Djava.class.path=.%s%s%s", JAVA_PATH_SEPARATOR, grt_class_path, JAVA_PATH_SEPARATOR);

  //add library path to class path
  if (grt_class_path_libraries)
    jvm_options[0].optionString= str_g_append_and_free(jvm_options[0].optionString,
      grt_class_path_libraries);

  //add user defined class_path to class path
  if (class_path)
    jvm_options[0].optionString= str_g_append(jvm_options[0].optionString,
      class_path);

  if (grt->options & MYX_GRT_REMOTE_DEBUG)
  {
    jvm_options[1].optionString= "-Xdebug";
    jvm_options[2].optionString= strdup("-Xrunjdwp:transport=dt_socket,server=y,address=8000,suspend=n");
    jvm_options[3].optionString= "-Xnoagent";
    jvm_options[4].optionString= "-Djava.compiler=NONE";

    if (jvm_max_heap && jvm_max_heap[0])
      jvm_options[5].optionString= jvm_max_heap;
    else
      jvm_options[5].optionString= "-Xmx256m"; // maximal java heap size

    // do not modify the stack size
    //jvm_options[5].optionString= "-Xss64m"; // java stack size
  }
  else
  {
    if (jvm_max_heap && jvm_max_heap[0])
      jvm_options[1].optionString= jvm_max_heap;
    else
      jvm_options[1].optionString= "-Xmx256m"; // maximal java heap size
  }

  vm_init_args.options = jvm_options;
  vm_init_args.ignoreUnrecognized = JNI_FALSE;

  if (grt->options & MYX_GRT_VERBOSE)
  {
    myx_grt_messages_stack_add_message(grt, "Launching Java VM", NULL, 0);
  	myx_grt_messages_stack_add_message(grt, "Classpath: %s", NULL, 0, jvm_options[0].optionString);
  	myx_grt_messages_stack_add_message(grt, "Runtime Library filename: %s", NULL, 0, runtimelib_filename);
  }

  // Try launch the Java VM
#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)
  // Try to load the jvm.dll
  if ((handle= LoadLibrary(runtimelib_filename)) == 0)
  {
    myx_grt_messages_stack_add_error(grt, "The JRE library cannot be found at %s.", NULL, 0, runtimelib_filename);

    *error= MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED;
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE_LOADER*);
  }

  // Locate the JNI_CreateJavaVM function
  create_java_vm=	(void *)GetProcAddress(handle, "JNI_CreateJavaVM");
  if (create_java_vm == 0)
  {
    myx_grt_messages_stack_add_error(grt, "The JRE cannot be loaded.", NULL, 0);

    *error= MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED;
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE_LOADER*);
  }

  // Call JNI_CreateJavaVM to get the Java environment
  res= create_java_vm(&priv->jvm, (void **)&priv->env, &vm_init_args);

#elif defined(DLOAD_JVM)

  if (!(handle= dlopen(runtimelib_filename, RTLD_LAZY)))
  {
    myx_grt_messages_stack_add_error(grt, "The JRE library cannot be found at %s.", NULL, 0, runtimelib_filename);
    
    *error= MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED;
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE_LOADER*);
  }

  // Locate the JNI_CreateJavaVM function
  create_java_vm= dlsym(handle, "JNI_CreateJavaVM");
  if (create_java_vm == 0)
  {
    myx_grt_messages_stack_add_error(grt, "The JRE cannot be loaded.", NULL, 0);

    *error= MYX_GRT_JAVA_JRE_CANNOT_BE_LOADED;
    GRT_RETURN(grt, NULL, MYX_GRT_MODULE_LOADER*);
  }    
    
  // Call JNI_CreateJavaVM to get the Java environment
  res= create_java_vm(&priv->jvm, (void **)&priv->env, &vm_init_args);

#else
  if (!global_jvm)
  {
    res= JNI_CreateJavaVM(&priv->jvm, (void **)&priv->env, &vm_init_args);
    global_jvm= priv->jvm;
  }
  else
  {
    vm_init_args.nOptions= 0;
    jvm_options[0].optionString= 0;
    
    res= (*global_jvm)->AttachCurrentThread(global_jvm, (void **)&priv->env, &vm_init_args);
  }
#endif  
  g_free(jvm_options[0].optionString);

  if (res < 0)
  {
    myx_grt_messages_stack_add_error(grt, "The Java VM cannot be created (%d).", NULL, 0, (int) res);

    goto error;
  }

  // --------------------------------------------------------------------------------------------------------------------
  // Search class reference for the java Grt class
  grt_class= (*priv->env)->FindClass(priv->env, GRTCLASSPREFIX "Grt");
  if (!grt_class)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class " GRTCLASSPREFIX "Grt.", NULL, 0);
    if ((*priv->env)->ExceptionOccurred(priv->env))
    {
      (*priv->env)->ExceptionDescribe(priv->env);
      (*priv->env)->ExceptionClear(priv->env);
    }
    goto error;
  }
  priv->grt_class= (*priv->env)->NewGlobalRef(priv->env, grt_class);

  // Search for the function callModuleFunction in the Grt class
  priv->grt_call_func= (*priv->env)->GetStaticMethodID(priv->env, 
     priv->grt_class, "callModuleFunction", "(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;");
  if (!priv->grt_call_func)
  {
    myx_grt_messages_stack_add_error(grt, "The java function callModuleFunction cannot be found "
      "in the java class " GRTCLASSPREFIX "Grt.", NULL, 0);
    goto error;
  }

  // --------------------------------------------------------------------------------------------------------------------
  //  Create Grt Java Object
  getInstance_methodID= (*priv->env)->GetStaticMethodID(priv->env, priv->grt_class, 
    "getInstance", "()L" GRTCLASSPREFIX "Grt;");
  if (!getInstance_methodID)
  {
    myx_grt_messages_stack_add_error(grt, "The method Grt.getInstance() cannot be found.", NULL, 0);
    goto error;
  }
  grt_object= (*priv->env)->CallStaticObjectMethodA(priv->env, priv->grt_class, getInstance_methodID, NULL);
  if (!grt_object)
  {
    myx_grt_messages_stack_add_error(grt, "The creation of the Java Grt object failed.", NULL, 0);
    goto error;
  }

  // --------------------------------------------------------------------------------------------------------------------
  // Set the grt pointer in the Java
  setMyxGrtPointer_methodID= (*priv->env)->GetMethodID(priv->env, priv->grt_class, 
    "setMyxGrtPointer", "(J)V");
  if (!setMyxGrtPointer_methodID)
  {
    myx_grt_messages_stack_add_error(grt, "The method Grt.setMyxGrtPointer() cannot be found.", NULL, 0);
    goto error;
  }

  args= g_new0(jvalue, 1);
  args[0].j= PtrToLong(grt);

  (*priv->env)->CallVoidMethodA(priv->env, grt_object, setMyxGrtPointer_methodID, args);

  g_free(args);
  args= NULL;

  // --------------------------------------------------------------------------------------------------------------------
  // Set callback object
  setCallback_methodID= (*priv->env)->GetMethodID(priv->env, priv->grt_class,
    "setCallback", "(Ljava/lang/String;Ljava/lang/String;)V");
  if (!setCallback_methodID)
  {
    myx_grt_messages_stack_add_error(grt, "The method Grt.setCallback() cannot be found.", NULL, 0);
    goto error;
  }

  args= g_new0(jvalue, 2);
  args[0].l= (*priv->env)->NewStringUTF(priv->env, "GrtCallbackNative");
  {
#ifdef FROM_XCODE
    args[1].l= (*priv->env)->NewStringUTF(priv->env, grt_class_path_prefix);
#else
    char *path= str_g_append(g_get_current_dir(), "/");
    args[1].l= (*priv->env)->NewStringUTF(priv->env, path);
    g_free(path);
#endif
  }

  (*priv->env)->CallVoidMethodA(priv->env, grt_object, setCallback_methodID, args);
  if ((*priv->env)->ExceptionOccurred(priv->env))
  {
    (*priv->env)->ExceptionDescribe(priv->env);
    (*priv->env)->ExceptionClear(priv->env);

    myx_grt_messages_stack_add_error(grt,
      "An exception occurred while setting up a native callback. Please check if all required libraries are present.",
      NULL, 0);
    goto error;
  }
  g_free(args);
  args= NULL;

  // Cache java objects and methods
  if (java_cache_java_objects_and_methods(grt, priv) != MYX_GRT_NO_ERROR)
    goto error;


  GRT_RETURN(grt, loader, MYX_GRT_MODULE_LOADER*);
  
error:
  *error= MYX_GRT_MODULE_INIT_ERROR;
errorfree:
  g_free(grt_class_path_libraries_dir);
  g_free(grt_class_path);
  g_free(priv);
  g_free(loader);
  g_free(args);
  
  GRT_RETURN(grt, NULL, MYX_GRT_MODULE_LOADER*);
}

static MYX_GRT_ERROR java_cache_java_objects_and_methods(MYX_GRT *grt, MYX_JAVA_LOADER *priv)
{
  // ---------------------------------------
  // Find Class class
  priv->java_class= (*priv->env)->FindClass(priv->env, "java/lang/Class");
  if (!priv->java_class)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class java.lang.Class.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_class= (*priv->env)->NewGlobalRef(priv->env, priv->java_class);

  // Find Class's getName method
  priv->java_class_getName= (*priv->env)->GetMethodID(priv->env, priv->java_class, 
    "getName", "()Ljava/lang/String;");
  if (!priv->java_class_getName)
  {
    myx_grt_messages_stack_add_error(grt, "The method Class.getName()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find Class's getMethods method
  priv->java_class_getMethods= (*priv->env)->GetMethodID(priv->env, priv->java_class, 
    "getMethods", "()[Ljava/lang/reflect/Method;");
  if (!priv->java_class_getMethods)
  {
    myx_grt_messages_stack_add_error(grt, "The method Class.getMethods()[Ljava/lang/Object; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // ---------------------------------------
  // Find Method class
  priv->java_method= (*priv->env)->FindClass(priv->env, "java/lang/reflect/Method");
  if (!priv->java_method)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class java.lang.reflect.Method.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_method= (*priv->env)->NewGlobalRef(priv->env, priv->java_method);

  // Find Method's getName method
  priv->java_method_getName= (*priv->env)->GetMethodID(priv->env, priv->java_method, 
    "getName", "()Ljava/lang/String;");
  if (!priv->java_method_getName)
  {
    myx_grt_messages_stack_add_error(grt, "The method Method.getName()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find Method's getReturnType method
  priv->java_method_getReturnType= (*priv->env)->GetMethodID(priv->env, priv->java_method, 
    "getReturnType", "()Ljava/lang/Class;");
  if (!priv->java_method_getReturnType)
  {
    myx_grt_messages_stack_add_error(grt, "The method Method.getReturnType()Ljava/lang/Class; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // ---------------------------------------
  // Find String class
  priv->java_string= (*priv->env)->FindClass(priv->env, "java/lang/String");
  if (!priv->java_string)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class java.lang.String.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_string= (*priv->env)->NewGlobalRef(priv->env, priv->java_string);


  // Find Integer class
  priv->java_integer= (*priv->env)->FindClass(priv->env, "java/lang/Integer");
  if (!priv->java_integer)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class java.lang.Integer.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_integer= (*priv->env)->NewGlobalRef(priv->env, priv->java_integer);

  // Find Integer's <init> constructor
  priv->java_integer_init= (*priv->env)->GetMethodID(priv->env, priv->java_integer, 
    "<init>", "(I)V");
  if (!priv->java_integer_init)
  {
    myx_grt_messages_stack_add_error(grt, "The constructor Integer(I) cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find Integer's intValue method
  priv->java_integer_intValue= (*priv->env)->GetMethodID(priv->env, priv->java_integer, 
    "intValue", "()I");
  if (!priv->java_integer_intValue)
  {
    myx_grt_messages_stack_add_error(grt, "The constructor Integer.intValue()I cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }


  // ---------------------------------------
  // Find Double class
  priv->java_double= (*priv->env)->FindClass(priv->env, "java/lang/Double");
  if (!priv->java_double)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class java.lang.Integer.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_double= (*priv->env)->NewGlobalRef(priv->env, priv->java_double);

  // Find Double's <init> constructor
  priv->java_double_init= (*priv->env)->GetMethodID(priv->env, priv->java_double, 
    "<init>", "(D)V");
  if (!priv->java_integer_init)
  {
    myx_grt_messages_stack_add_error(grt, "The constructor Double(D) cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find Double's intValue method
  priv->java_double_doubleValue= (*priv->env)->GetMethodID(priv->env, priv->java_double, 
    "doubleValue", "()D");
  if (!priv->java_double_doubleValue)
  {
    myx_grt_messages_stack_add_error(grt, "The method Double.doubleValue()D cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  
  // ---------------------------------------
  // Find GrtObject class
  priv->java_grtobject= (*priv->env)->FindClass(priv->env, GRTCLASSPREFIX "GrtObject");
  if (!priv->java_grtobject)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class " GRTCLASSPREFIX "GrtObject.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_grtobject= (*priv->env)->NewGlobalRef(priv->env, priv->java_grtobject);

  // Find GrtObject's get_id method
  priv->java_grtobject_get_id= (*priv->env)->GetMethodID(priv->env, priv->java_grtobject, 
    "get_id", "()Ljava/lang/String;");
  if (!priv->java_grtobject_get_id)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtObject.get_id()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // ---------------------------------------
  // Find GrtList class
  priv->java_grtlist= (*priv->env)->FindClass(priv->env, GRTCLASSPREFIX "GrtList");
  if (!priv->java_grtlist)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class " GRTCLASSPREFIX "GrtList.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_grtlist= (*priv->env)->NewGlobalRef(priv->env, priv->java_grtlist);

  // Find GrtList's size method
  priv->java_grtlist_size= (*priv->env)->GetMethodID(priv->env, priv->java_grtlist, 
    "size", "()I");
  if (!priv->java_grtlist_size)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtList.size()I cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtList's getObject method
  priv->java_grtlist_getObject= (*priv->env)->GetMethodID(priv->env, priv->java_grtlist, 
    "getObject", "(I)Ljava/lang/Object;");
  if (!priv->java_grtlist_getObject)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtList.getObject(I)Ljava/lang/Object; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtList's java_grtlist_getContentType method
  priv->java_grtlist_getContentType= (*priv->env)->GetMethodID(priv->env, priv->java_grtlist, 
    "getContentType", "()Ljava/lang/String;");
  if (!priv->java_grtlist_getContentType)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtList.getContentType()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtList's java_grtlist_getContentStructName method
  priv->java_grtlist_getContentStructName= (*priv->env)->GetMethodID(priv->env, priv->java_grtlist, 
    "getContentStructName", "()Ljava/lang/String;");
  if (!priv->java_grtlist_getContentStructName)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtList.getContentStructName()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }


  // ---------------------------------------
  // Find GrtHashMap class
  priv->java_grthashmap= (*priv->env)->FindClass(priv->env, GRTCLASSPREFIX "GrtHashMap");
  if (!priv->java_grthashmap)
  {
    myx_grt_messages_stack_add_error(grt, "Could not load java class " GRTCLASSPREFIX "GrtHashMap.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }
  else
    priv->java_grthashmap= (*priv->env)->NewGlobalRef(priv->env, priv->java_grthashmap);

  // Find GrtHashMap's getKeys method
  priv->java_grthashmap_getKeys= (*priv->env)->GetMethodID(priv->env, priv->java_grthashmap, 
    "getKeys", "()[Ljava/lang/String;");
  if (!priv->java_grthashmap_getKeys)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtHashMap.getKeys()[Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtHashMap's getKeys method
  priv->java_grthashmap_getObject= (*priv->env)->GetMethodID(priv->env, priv->java_grthashmap, 
    "getObject", "(Ljava/lang/String;)Ljava/lang/Object;");
  if (!priv->java_grthashmap_getObject)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtHashMap.getObject(Ljava/lang/String;)Ljava/lang/Object; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtHashMap's getContentType method
  priv->java_grthashmap_getContentType= (*priv->env)->GetMethodID(priv->env, priv->java_grthashmap, 
    "getContentType", "()Ljava/lang/String;");
  if (!priv->java_grthashmap_getContentType)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtHashMap.getContentType()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // Find GrtHashMap's java_grthashmap_getContentStructName method
  priv->java_grthashmap_getContentStructName= (*priv->env)->GetMethodID(priv->env, priv->java_grthashmap, 
    "getContentStructName", "()Ljava/lang/String;");
  if (!priv->java_grthashmap_getContentStructName)
  {
    myx_grt_messages_stack_add_error(grt, "The method GrtHashMap.getContentStructName()Ljava/lang/String; cannot be found.", NULL, 0);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  return MYX_GRT_NO_ERROR;
}


static MYX_GRT_ERROR java_init_module(MYX_GRT_MODULE_LOADER *loader, const char *file, MYX_GRT_MODULE **retmodule)
{
  MYX_JAVA_LOADER *jloader= loader->priv;
  MYX_GRT_MODULE *module;
  MYX_JAVA_MODULE *jmodule;
  jclass theclass;
  jobject module_info;
  unsigned int i;
  char *name, *ptr, *class_name;
  char *module_name= NULL;
  MYX_GRT_VALUE *module_functions= NULL;
  char *extends= NULL;

  class_name = g_path_get_basename(file);
  
  ptr= strchr(class_name, '.');
  if (ptr)
    *ptr= 0;

  name= g_strdup_printf(GRTCLASSPREFIX "modules/%s", class_name);
  g_free(class_name);

  theclass= (*jloader->env)->FindClass(jloader->env, name);
  if (!theclass)
  {
    (*jloader->env)->ExceptionDescribe(jloader->env);
    (*jloader->env)->ExceptionClear(jloader->env);

    myx_grt_messages_stack_add_error(loader->grt, "Could not load java class %s", NULL, 0);
    g_free(name);
    return MYX_GRT_MODULE_INIT_ERROR;
  }

  // fetch info about the module
  module_info= j_call_static_method(jloader->env, theclass,
                                    "getModuleInfo", "()Ljava/lang/String;", NULL);

  if (module_info)
  {
    MYX_GRT_VALUE *grt_info;

    grt_info= j_grt_value_from_java_xml_object(NULL, jloader->env, module_info);

    if (!grt_info || myx_grt_value_get_type(grt_info) != MYX_DICT_VALUE)
    {
      myx_grt_messages_stack_add_error(loader->grt, "could not parse xml response data from %s", 
        NULL, 0, file);
      return MYX_GRT_BAD_MODULE;
    }
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
    (*jloader->env)->DeleteLocalRef(jloader->env, module_info);

    myx_grt_value_release(grt_info);
  }
  else
  {
    // No exception handling needed here because it is handled
    // directly in j_call_static_method

    if (loader->grt->options & MYX_GRT_VERBOSE)
      myx_grt_messages_stack_add_message(loader->grt, "Module %s doesn't implement getModuleInfo", NULL, 0, file);
    return MYX_GRT_BAD_MODULE;
  }

  if (!module_name || !module_functions)
  {
    if (getenv("GRT_VERBOSE"))
    {
      if (!module_name)
        myx_grt_messages_stack_add_error(loader->grt, "Module info from %s doesn't contain 'name'", NULL, 0, file);
      if (!module_functions)
        myx_grt_messages_stack_add_error(loader->grt, "Module info from %s doesn't contain 'functions'", NULL, 0, file);
    }
    g_free(module_name);
    g_free(extends);
    if (module_functions)
      myx_grt_value_release(module_functions);
    g_free(name);
    return MYX_GRT_BAD_MODULE;
  }

  // init internal module descriptor
  module= g_new0(MYX_GRT_MODULE, 1);
  jmodule= g_new0(MYX_JAVA_MODULE, 1);

  module->loader= loader;
  module->priv= jmodule;
  module->name= module_name;
  module->path= name;
  module->functions_num= myx_grt_list_item_count(module_functions);
  module->functions= g_new0(MYX_GRT_FUNCTION, module->functions_num);
  for (i= 0; i < module->functions_num; i++)
  {
    MYX_GRT_FUNCTION *func= module->functions+i;
    MYX_JAVA_FUNCTION *jfunc= g_new0(MYX_JAVA_FUNCTION, 1);
    char *tmp= g_strdup(myx_grt_value_as_string(myx_grt_list_item_get(module_functions, i)));
    char *return_type;
    
    func->module= module;

    // do not use myx_grt_parse_function_spec here
    // since we need special handling for the java signature
    func->name= g_strdup(strtok(tmp, ":"));
    jfunc->java_signature= g_strdup(strtok(NULL, ":"));
    func->param_struct_name= NULL;
    return_type= strtok(NULL, ":");
    if ((return_type) && (return_type[0]))
      func->return_struct_name= g_strdup(return_type);
  
    func->priv= jfunc;

    g_free(tmp);
  }
  myx_grt_value_release(module_functions);
  module->extends= extends;

  // java specific module info
  jmodule->class_ref= (*jloader->env)->NewGlobalRef(jloader->env, theclass);

  *retmodule= module;
  
  if (loader->grt->options & MYX_GRT_VERBOSE)
    myx_grt_messages_stack_add_message(loader->grt, "Initialized module %s", NULL, 0, name);

  return MYX_GRT_NO_ERROR;
}

static MYX_GRT_ERROR java_call_function(MYX_GRT_FUNCTION *function, MYX_GRT_VALUE *value, MYX_GRT_VALUE **retval)
{
  MYX_JAVA_LOADER *jloader= function->module->loader->priv;
  MYX_JAVA_MODULE *jmodule= function->module->priv;
  MYX_JAVA_FUNCTION *jfunc= function->priv;
  JNIEnv *env= jloader->env;
  jobject res;
  jvalue args[4];

  args[0].l= jmodule->class_ref;
  args[1].l= (*env)->NewStringUTF(env, function->name);
  args[2].l= (*env)->NewStringUTF(env, jfunc->java_signature);
  args[3].l= j_xml_object_from_grt_value(function->module->loader->grt, env, value);
  
  res= (*env)->CallStaticObjectMethodA(env, jloader->grt_class, jloader->grt_call_func, args);
  if (res)
  {
    *retval= j_grt_value_from_java_xml_object(function->module->loader->grt, env, res);
    (*env)->DeleteLocalRef(env, res);
  }
  else
    *retval= NULL;
  (*env)->DeleteLocalRef(env, args[1].l);
  (*env)->DeleteLocalRef(env, args[2].l);
  (*env)->DeleteLocalRef(env, args[3].l);

  if (!res)
  {
    (*env)->ExceptionDescribe(env);
    (*env)->ExceptionClear(env);

    return MYX_GRT_FUNCTION_CALL_ERROR;
  }

  return MYX_GRT_NO_ERROR;
}


//-----------------------------------------------------------------------------
// Private Stuff
//-----------------------------------------------------------------------------


static jobject j_call_method(JNIEnv *env, jclass theclass, jobject object, const char *name, const char *signature, jvalue *args)
{
  jmethodID methodID;
  jobject res= NULL;

  methodID= (*env)->GetMethodID(env, theclass, name, signature);
  if (methodID)
    res= (*env)->CallObjectMethodA(env, object, methodID, args);
  else
  {
    if (getenv("GRT_VERBOSE"))
      g_warning("Method '%s' / %s not found in class", name, signature);
  }
    
  return res;
}


static jobject j_call_static_method(JNIEnv *env, jclass theclass, const char *name, const char *signature, jvalue *args)
{
  jobject res = NULL;
  jmethodID methodID;

  methodID= (*env)->GetStaticMethodID(env, theclass, name, signature);
  if (methodID)
    res= (*env)->CallStaticObjectMethodA(env, theclass, methodID, args);
  else
  {
    //Clear the exception if the Method cannot be found
    (*env)->ExceptionClear(env);

    if (getenv("GRT_VERBOSE"))
      g_warning("Static method '%s' / %s not found in class", name, signature);
  }

  return res;
}

static char *j_get_string(JNIEnv *env, jobject strobj)
{
  const char *jstr= (*env)->GetStringUTFChars(env, strobj, 0);
  char *str= NULL;

  if (jstr)
  {
    str= g_strdup(jstr);

    (*env)->ReleaseStringUTFChars(env, strobj, jstr);
  }
  else
    str= g_strdup("");

  return str;
}


static jobject j_xml_object_from_grt_value(MYX_GRT *grt, JNIEnv *env, MYX_GRT_VALUE *value)
{
  jobject jstr= NULL;
  char *data;

  data= myx_grt_value_to_xml(grt, value);
  if (data)
  {
    jstr= (*env)->NewStringUTF(env, data);

    g_free(data);
  }

  return jstr;
}

static jobject j_xml_global_object_from_grt_value(JNIEnv *env, const char *objectPath, MYX_GRT_VALUE *value)
{
  jobject jstr= NULL;
  char *data;

  data= myx_grt_value_to_xml_global_object(objectPath, value);
  if (data)
  {
    jstr= (*env)->NewStringUTF(env, data);

    g_free(data);
  }

  return jstr;
}


static MYX_GRT_VALUE *j_grt_value_from_java_xml_object(MYX_GRT *grt, JNIEnv *env, jobject strobj)
{
  MYX_GRT_VALUE *value= NULL;
  const char *jstr;
  
  if ((jstr= (*env)->GetStringUTFChars(env, strobj, 0)))
  {
    /*FILE *file= myx_fopen("C:\\test.xml", "w");
    fprintf(file, jstr);
    fclose(file);*/

    if (grt)
      value= myx_grt_value_from_xml_global_object(grt, jstr, strlen(jstr));
    else
      value= myx_grt_value_from_xml(grt, jstr, strlen(jstr));

    (*env)->ReleaseStringUTFChars(env, strobj, jstr);
  }
  return value;
}

static MYX_JAVA_LOADER *find_java_loader(MYX_GRT *grt)
{
  MYX_JAVA_LOADER *jloader= NULL;
  unsigned int i;
  
  // get Java loader
  for (i= 0; i < grt->loaders_num; i++)
  {
    MYX_GRT_MODULE_LOADER *loader= grt->loaders[i];
    if (loader->loader_type == MYX_JAVA_MODULE_TYPE)
    {
      jloader= loader->priv;
      break;
    }
  }

  return jloader;
}

static jobject java_global_object_from_grt_value(MYX_GRT *grt, JNIEnv *env, const char *objectPath, MYX_GRT_VALUE *value)
{
  jobject jobj= NULL;
  MYX_JAVA_LOADER *jloader= find_java_loader(grt);

  if ( (value) && (jloader) )
  {
    if (myx_grt_value_get_type(value) == MYX_STRING_VALUE)
    {
      jobj= (*env)->NewStringUTF(env, myx_grt_value_as_string(value));
    }
    else if (myx_grt_value_get_type(value) == MYX_INT_VALUE)
    {
      jvalue *args= g_new0(jvalue, 1);

      args[0].i= myx_grt_value_as_int(value);
      jobj= (*env)->NewObjectA(env, jloader->java_integer, jloader->java_integer_init, args);
      g_free(args);
    }
    else if (myx_grt_value_get_type(value) == MYX_REAL_VALUE)
    {
      jvalue *args= g_new0(jvalue, 1);

      args[0].d= myx_grt_value_as_real(value);
      jobj= (*env)->NewObjectA(env, jloader->java_double, jloader->java_double_init, args);
      g_free(args);
    }
    else if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
    {
      jvalue *args= g_new0(jvalue, 2);
      jclass j_class;
      jmethodID j_init;
      char *j_class_name= g_strdup(GRTCLASSPREFIX);

      if ( (myx_grt_list_content_get_type(value) == MYX_STRING_VALUE) && 
        (!myx_grt_list_content_get_struct_name(value)) )
        j_class_name= str_g_append(j_class_name, "GrtStringList");
      else if (myx_grt_list_content_get_struct_name(value))
      {
        j_class_name= str_g_append_and_free(j_class_name, 
          str_g_replace(g_strdup(myx_grt_list_content_get_struct_name(value)), ".", "/"));
        j_class_name= str_g_append(j_class_name, "List");
      }
      else
        j_class_name= str_g_append(j_class_name, "GrtList");
        

      // Find List class
      j_class= (*env)->FindClass(env, j_class_name);
      if (!j_class)
        return NULL;

      // Find List's <init> constructor
      j_init= (*env)->GetMethodID(env, j_class, 
        "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");
      if (!j_init)
        return NULL;

      if (myx_grt_list_content_get_struct_name(value))
        args[0].l= (*env)->NewStringUTF(env, myx_grt_list_content_get_struct_name(value));
      else
        args[0].l= (*env)->NewStringUTF(env, "");
      args[1].l= (*env)->NewStringUTF(env, objectPath);
      jobj= (*env)->NewObjectA(env, j_class, j_init, args);
      g_free(args);

      g_free(j_class_name);
    }
    else if (myx_grt_value_get_type(value) == MYX_DICT_VALUE)
    {
      jvalue *args;
      jclass j_class;
      jmethodID j_init;
      char *j_class_name= g_strdup(GRTCLASSPREFIX);
      
      if (myx_grt_dict_struct_get_name(value))
      {
        // this is a struct instance
        j_class_name= str_g_append_and_free(j_class_name, 
          str_g_replace(g_strdup(myx_grt_dict_struct_get_name(value)), ".", "/"));

        // Find Dict class
        j_class= (*env)->FindClass(env, j_class_name);
        if (!j_class)
          return NULL;
          
        // Find Dict's <init> constructor
        j_init= (*env)->GetMethodID(env, j_class, "<init>", "(Ljava/lang/Object;)V");
        if (!j_init)
          return NULL;

        /// Create new object instance
        args= g_new0(jvalue, 1);
        args[0].l= (*env)->NewStringUTF(env, objectPath);
        jobj= (*env)->NewObjectA(env, j_class, j_init, args);
        g_free(args);
      }
      else
      {
        if ( (myx_grt_dict_content_get_type(value) == MYX_STRING_VALUE) && (!myx_grt_dict_struct_get_name(value)) )
          j_class_name= str_g_append(j_class_name, "GrtStringHashMap");
        else
          j_class_name= str_g_append(j_class_name, "GrtHashMap");

        // Find List class
        j_class= (*env)->FindClass(env, j_class_name);
        if (!j_class)
          return NULL;
          
        // Find List's <init> constructor
        j_init= (*env)->GetMethodID(env, j_class, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");
        if (!j_init)
          return NULL;

        /// Create new object instance
        args= g_new0(jvalue, 2);
        args[0].l= (*env)->NewStringUTF(env, "");
        args[1].l= (*env)->NewStringUTF(env, objectPath);
        jobj= (*env)->NewObjectA(env, j_class, j_init, args);
        g_free(args);
      }

      g_free(j_class_name);
    }

    return jobj;
  }
  else
    return NULL;
}

static MYX_GRT_VALUE *java_object_to_global_grt_value(MYX_GRT *grt, JNIEnv *env, jobject value)
{
  MYX_GRT_VALUE *res= NULL;
  MYX_JAVA_LOADER *jloader= find_java_loader(grt);

  //FILE *f= myx_fopen("C:\\log.txt", "a");

  // check the class of the value
  if ((*env)->IsInstanceOf(env, value, jloader->java_grtlist))
  {
    // List
    jstring j_content_type= (*env)->CallObjectMethodA(env, value, jloader->java_grtlist_getContentType, NULL);
    jstring j_content_struct_name= (*env)->CallObjectMethodA(env, value, jloader->java_grtlist_getContentStructName, NULL);
    char *content_type_name= j_get_string(env, j_content_type);
    char *content_struct_name;
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE_TYPE content_type= myx_get_value_type_from_string(content_type_name, &error);
    jint item_count= (*env)->CallIntMethodA(env, value, jloader->java_grtlist_size, NULL);
    int i;

    if (j_content_struct_name)
      content_struct_name= j_get_string(env, j_content_struct_name);

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
      jobject j_item;
      MYX_GRT_VALUE *item_value;
      jvalue *args= g_new0(jvalue, 1);
      
      args[0].i= i;
      j_item= (*env)->CallObjectMethodA(env, value, jloader->java_grtlist_getObject, args);
      g_free(args);

      if (j_item)
      {
        item_value= java_object_to_global_grt_value(grt, env, j_item);
        myx_grt_list_item_add(res, item_value);
        myx_grt_value_release(item_value);
      }
    }
  } 
  else if ((*env)->IsInstanceOf(env, value, jloader->java_grthashmap))
  {
    // Hashmap
    jstring j_content_type= (*env)->CallObjectMethodA(env, value, jloader->java_grthashmap_getContentType, NULL);
    jstring j_content_struct_name= (*env)->CallObjectMethodA(env, value, jloader->java_grthashmap_getContentStructName, NULL);
    char *content_type_name= j_get_string(env, j_content_type);
    char *content_struct_name;
    MYX_GRT_ERROR error;
    MYX_GRT_VALUE_TYPE content_type= myx_get_value_type_from_string(content_type_name, &error);
    jobjectArray keys= (*env)->CallObjectMethodA(env, value, jloader->java_grthashmap_getKeys, NULL);
    int i;

    if (j_content_struct_name)
      content_struct_name= j_get_string(env, j_content_struct_name);

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
    for (i= 0; i < (*env)->GetArrayLength(env, keys); i++)
    {      
      jstring j_key= (*env)->GetObjectArrayElement(env, keys, i);
      char *key= j_get_string(env, j_key);
      jobject j_map_value;
      MYX_GRT_VALUE *map_value;
      jvalue *args= g_new0(jvalue, 1);

      args[0].l= j_key;
      j_map_value= (*env)->CallObjectMethodA(env, value, jloader->java_grthashmap_getObject, args);
      g_free(args);

      map_value= java_object_to_global_grt_value(grt, env, j_map_value);
      myx_grt_dict_item_set_value(res, key, map_value);
      myx_grt_value_release(map_value);

      g_free(key);
    }
  } 
  else if ((*env)->IsInstanceOf(env, value, jloader->java_grtobject))
  {
    // Object
    jclass java_obj_class= (*env)->GetObjectClass(env, value);
    jstring j_class_name= (*env)->CallObjectMethodA(env, java_obj_class, jloader->java_class_getName, NULL);
    char *class_name= j_get_string(env, j_class_name);
    char *struct_name;
    MYX_GRT_STRUCT *gstruct;
    unsigned int member_count;

    // TODO: remove call if not necessary.
    /*jobjectArray methods= (*env)->CallObjectMethodA(env, java_obj_class, jloader->java_class_getMethods, NULL); */
    unsigned int i;

    // Convert struct name to class name
    struct_name= g_strdup(class_name + strlen(GRTCLASSPREFIX));

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
      char *method_sig= g_strdup("()");
      int is_int= 0;
      int is_double= 0;
      jmethodID j_getter;

      method_name= str_g_append(method_name, myx_grt_struct_get_member_name(member));
      method_name[3]= g_ascii_toupper(method_name[3]);

      // build java method signature
      if (myx_grt_struct_member_get_type(member) == MYX_STRING_VALUE)
      {
        method_sig= str_g_append(method_sig, "Ljava/lang/String;");

        if (myx_grt_struct_member_get_is_ref(member) == 1)
          method_name= str_g_append(method_name, "ById");
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_INT_VALUE)
      {
        method_sig= str_g_append(method_sig, "I");
        is_int= 1;
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_REAL_VALUE)
      {
        method_sig= str_g_append(method_sig, "D");
        is_double= 1;
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_LIST_VALUE)
      {
        if (myx_grt_struct_member_get_content_struct_name_overridden(member) != NULL)
        {
          char *class_name= str_g_replace(g_strdup(myx_grt_struct_member_get_content_struct_name_overridden(member)), ".", "/");
          method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX);
          method_sig= str_g_append_and_free(method_sig, class_name);
          method_sig= str_g_append(method_sig, "List;");
        }
        else if (myx_grt_struct_member_get_content_type(member) == MYX_STRING_VALUE)
          method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX "GrtStringList;");
        else
          method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX "GrtList;");
      }
      else if (myx_grt_struct_member_get_type(member) == MYX_DICT_VALUE)
      {
        if (myx_grt_struct_member_get_content_struct_name_overridden(member) != NULL)
        {
          char *class_name= str_g_replace(g_strdup(myx_grt_struct_member_get_content_struct_name_overridden(member)), ".", "/");
          method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX);
          method_sig= str_g_append_and_free(method_sig, class_name);
          method_sig= str_g_append(method_sig, ";");
        }
        else if (myx_grt_struct_member_get_struct_name(member) != NULL)
        {
          char *class_name= str_g_replace(g_strdup(myx_grt_struct_member_get_struct_name(member)), ".", "/");
          method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX); 
          method_sig= str_g_append_and_free(method_sig, class_name);
          method_sig= str_g_append(method_sig, ";");
        }
        else
        {
          if (myx_grt_struct_member_get_content_type(member) == MYX_STRING_VALUE)
            method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX "GrtStringHashMap;");
          else if ( (myx_grt_struct_member_get_content_type(member) == MYX_DICT_VALUE) &&
            (myx_grt_struct_member_get_content_struct_name_overridden(member) != NULL) )
          {
            char *class_name= str_g_replace(g_strdup(myx_grt_struct_member_get_content_struct_name_overridden(member)), ".", "/");
            method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX);
            method_sig= str_g_append_and_free(method_sig, class_name);
            method_sig= str_g_append(method_sig, "HashMap;");
          }
          else
            method_sig= str_g_append(method_sig, "L" GRTCLASSPREFIX "GrtHashMap;");
        }
      }

      //fprintf(f, "method_name: %s - %s" _br, method_name, method_sig);

      j_getter= (*env)->GetMethodID(env, java_obj_class, method_name, method_sig);

      if (j_getter)
      {
        if (is_int)
        {
          jint j_int= (*env)->CallIntMethodA(env, value, j_getter, NULL);
          MYX_GRT_VALUE *member_value= myx_grt_value_from_int(j_int);
          myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
        }
        else if (is_double)
        {
          jdouble j_double= (*env)->CallDoubleMethodA(env, value, j_getter, NULL);
          MYX_GRT_VALUE *member_value= myx_grt_value_from_real(j_double);
          myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
        }
        else
        {
          jobject j_member= (*env)->CallObjectMethodA(env, value, j_getter, NULL);
          if (j_member)
          {
            if ( (myx_grt_struct_member_get_content_struct_name_overridden(member) != NULL)
                && (myx_grt_struct_member_get_content_type(member) == MYX_STRING_VALUE) )
            {
              //deal with reference lists
              jint item_count= (*env)->CallIntMethodA(env, j_member, jloader->java_grtlist_size, NULL);
              MYX_GRT_VALUE *ref_list= myx_grt_list_new(MYX_STRING_VALUE, 
                  myx_grt_struct_member_get_content_struct_name_overridden(member));
              int j;

              for (j= 0; j < item_count; j++)
              {
                jobject j_item;
                jvalue *args= g_new0(jvalue, 1);
                
                args[0].i= j;
                j_item= (*env)->CallObjectMethodA(env, j_member, jloader->java_grtlist_getObject, args);
                g_free(args);

                if (j_item)
                {
                  jstring j_ref_id= (*env)->CallObjectMethodA(env, j_item, jloader->java_grtobject_get_id, NULL);
                  char *ref_id= j_get_string(env, j_ref_id);
                  myx_grt_list_item_add_as_string(ref_list, ref_id);
                }
              }

              myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), ref_list);
              myx_grt_value_release(ref_list);
            }
            else
            {
              MYX_GRT_VALUE *member_value= java_object_to_global_grt_value(grt, env, j_member);
              if (member)
              {
                myx_grt_dict_item_set_value(res, myx_grt_struct_get_member_name(member), member_value);
                myx_grt_value_release(member_value);
              }
            }
          }
        }
      }
      else
        (*env)->ExceptionClear(env);

      g_free(method_sig);
      g_free(method_name);
    }

    g_free(class_name);
  }
  else if ((*env)->IsInstanceOf(env, value, jloader->java_integer))
  {
    // Integer
    jint i= (*env)->CallIntMethodA(env, value, jloader->java_integer_intValue, NULL);
    res= myx_grt_value_from_int(i);
  }
  else if ((*env)->IsInstanceOf(env, value, jloader->java_double))
  {
    // Real
    jdouble d= (*env)->CallDoubleMethodA(env, value, jloader->java_double_doubleValue, NULL);
    res= myx_grt_value_from_real(d);
  }
  else if ((*env)->IsInstanceOf(env, value, jloader->java_string))
  {
    // String
    res= myx_grt_value_from_string(j_get_string(env, value));
  }

  //fclose(f);
  return res;
}

// ------------------------------------------------------------------------------------------------------------------
// Function call
//

JNIEXPORT jstring JNICALL Java_com_mysql_grt_GrtCallbackNative_callGrtFunction
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring module, jstring name, jstring arguments)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  MYX_GRT_ERROR error;
  const char *c_str_module= (*env)->GetStringUTFChars(env, module, 0);
  const char *c_str_name= (*env)->GetStringUTFChars(env, name, 0);
  MYX_GRT_VALUE *result= NULL;

  GRT_ENTER(grt);

  if (c_str_module && c_str_name)
  {
    result= myx_grt_function_get_and_call(grt, c_str_module, c_str_name, 0, 
      j_grt_value_from_java_xml_object(grt, env, arguments), &error);

    if (error != MYX_GRT_NO_ERROR)
    {
      //return an error
    }
  }

  if (c_str_module)
    (*env)->ReleaseStringUTFChars(env, module, c_str_module);
  if (c_str_name)
    (*env)->ReleaseStringUTFChars(env, name, c_str_name);

  GRT_RETURN(grt, j_xml_object_from_grt_value(grt, env, result), jstring);
}

// ------------------------------------------------------------------------------------------------------------------
// getGrtGlobalById
//


JNIEXPORT jobject JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalById
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring id)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_id= (*env)->GetStringUTFChars(env, id, 0);
  MYX_GRT_VALUE *value;
  jobject res= NULL;

  GRT_ENTER(grt);
  
  value= myx_grt_reference_cache_lookup(grt, c_str_id);
  
  if (value)
    res= java_global_object_from_grt_value(grt, env, myx_grt_dict_get_object_path(value), value);

  if (c_str_id)
    (*env)->ReleaseStringUTFChars(env, id, c_str_id);

  GRT_RETURN(grt, res, jobject);
}

// ------------------------------------------------------------------------------------------------------------------
// Getters
//

JNIEXPORT jobject JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalAsObject
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  jobject res;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  //if the value doesn't exist yet, create it
  if (!value)
  {
    MYX_GRT_VALUE *parent_value;
    char *parent_path= g_strdup(c_str_objectPath);
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

  res= java_global_object_from_grt_value(grt, env, c_str_objectPath, value);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  GRT_RETURN(grt, res, jobject);
}

JNIEXPORT jstring JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalAsString
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value)
    GRT_RETURN(grt, (*env)->NewStringUTF(env, myx_grt_value_as_string(value)), jstring);
  else
    GRT_RETURN(grt, NULL, jstring);
}

JNIEXPORT jint JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalAsInt
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value)
    GRT_RETURN(grt, myx_grt_value_as_int(value), jint);
  else
    GRT_RETURN(grt, 0, jint);
}

JNIEXPORT jdouble JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalAsReal
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value)
    GRT_RETURN(grt, myx_grt_value_as_real(value), jdouble);
  else
    GRT_RETURN(grt, 0, jdouble);
}

// ------------------------------------------------------------------------------------------------------------------
// Setters
//

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_setGrtGlobalFromObject
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jobject value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  //MYX_GRT_VALUE *value_grt= myx_grt_value_from_xml(c_str_xml, strlen(c_str_xml));
  MYX_GRT_VALUE *value_grt;
  
  GRT_ENTER(grt);
  
  if (value)
  {
    value_grt= java_object_to_global_grt_value(grt, env, value);

    myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), c_str_objectPath, value_grt);
  }

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  
  GRT_LEAVE(grt);
}


JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_setGrtGlobalFromString
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jstring value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  const char *c_value_str= (*env)->GetStringUTFChars(env, value, 0);
  MYX_GRT_VALUE *value_grt= myx_grt_value_from_string(c_value_str);

  GRT_ENTER(grt);
  
  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), c_str_objectPath, value_grt);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  if (c_value_str)
    (*env)->ReleaseStringUTFChars(env, value, c_value_str);
  
  GRT_LEAVE(grt);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_setGrtGlobalFromInt
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jint value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value_grt= myx_grt_value_from_int(value);

  GRT_ENTER(grt);
  
  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), c_str_objectPath, value_grt);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  
  GRT_LEAVE(grt);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_setGrtGlobalFromReal
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jdouble value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value_grt= myx_grt_value_from_real(value);

  GRT_ENTER(grt);
  
  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), c_str_objectPath, value_grt);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  
  GRT_LEAVE(grt);
}


// ------------------------------------------------------------------------------------------------------------------
// List
//

JNIEXPORT jint JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalListSize
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  
  if (myx_grt_value_get_type(value) == MYX_LIST_VALUE)
    GRT_RETURN(grt, myx_grt_list_item_count(value), jint);
  else
    GRT_RETURN(grt, -1, jint);
}

JNIEXPORT jobject JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalListItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jint index)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *list;
  jobject jobj = NULL;
  
  GRT_ENTER(grt);
  
  list= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (myx_grt_value_get_type(list) == MYX_LIST_VALUE)
  {
    MYX_GRT_VALUE *list_item;
    char *item_path= g_strdup_printf("%s/%d", c_str_objectPath, (int)index);

    /*// if this is a reference list, add _id of list_item instead
    if ((myx_grt_list_content_get_type(list) == MYX_STRING_VALUE) && 
      (myx_grt_list_content_get_struct_name(list) != NULL))
      list_item= myx_grt_list_item_get_reference_value(grt, list, index);
    else*/
    list_item= myx_grt_list_item_get(list, index);

    jobj= java_global_object_from_grt_value(grt, env, item_path, list_item);
    g_free(item_path);
  }

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  GRT_RETURN(grt, jobj, jobject);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_addGrtGlobalListItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jobject value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *list;
  MYX_GRT_VALUE *list_item;

  GRT_ENTER(grt);
  
  list= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);
  list_item= java_object_to_global_grt_value(grt, env, value);
  
  if ( (list) && (list_item) )
  {
    // if this is a reference list, add _id of list_item instead
    if ((myx_grt_list_content_get_type(list) == MYX_STRING_VALUE) && 
      (myx_grt_list_content_get_struct_name(list) != NULL))
      myx_grt_list_item_add(list, myx_grt_dict_item_get_value(list_item, "_id"));
    else
      myx_grt_list_item_add(list, list_item);
  }

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  
  GRT_LEAVE(grt);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_removeGrtGlobalListItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jint value)
{

}


// ------------------------------------------------------------------------------------------------------------------
// Dict
//

JNIEXPORT jobject JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalDictItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jstring key)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  const char *c_str_key= (*env)->GetStringUTFChars(env, key, 0);
  MYX_GRT_VALUE *dict;
  jobject jobj= NULL;
  
  GRT_ENTER(grt);
  
  dict= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (myx_grt_value_get_type(dict) == MYX_DICT_VALUE)
  {
    char *value_path= g_strdup_printf("%s/%s", c_str_objectPath, c_str_key);
    MYX_GRT_VALUE *dict_value= myx_grt_dict_item_get_value(dict, c_str_key);

    jobj= java_global_object_from_grt_value(grt, env, value_path, dict_value);
    g_free(value_path);
  }

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);
  if (c_str_key)
    (*env)->ReleaseStringUTFChars(env, key, c_str_key);

  GRT_RETURN(grt, jobj, jobject);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_addGrtGlobalDictItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jstring key, jobject value)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  const char *c_str_key= (*env)->GetStringUTFChars(env, key, 0);
  MYX_GRT_VALUE *dict;
//  MYX_GRT_VALUE *result= NULL;

  GRT_ENTER(grt);
  
  dict= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);
  
  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value)
    myx_grt_dict_item_set_value(dict, c_str_key, java_object_to_global_grt_value(grt, env, value));

  if (c_str_key)
    (*env)->ReleaseStringUTFChars(env, key, c_str_key);
  
  GRT_LEAVE(grt);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_removeGrtGlobalDictItem
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jstring key)
{

}

JNIEXPORT jint JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalDictKeyCount
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;

  GRT_ENTER(grt);

  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value && (myx_grt_value_get_type(value) == MYX_DICT_VALUE))
    GRT_RETURN(grt, myx_grt_dict_item_count(value), jint);
  else
    GRT_RETURN(grt, -1, jint);
}

JNIEXPORT jstring JNICALL Java_com_mysql_grt_GrtCallbackNative_getGrtGlobalDictKey
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jint index)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  if (value && (myx_grt_value_get_type(value) == MYX_DICT_VALUE))
    GRT_RETURN(grt, (*env)->NewStringUTF(env, myx_grt_dict_item_key_by_index(value, index)), jstring);
  else
    GRT_RETURN(grt, NULL, jstring);
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_processMessages
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring msgs)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  
  if (grt->process_messages)
  {
    MYX_GRT_VALUE *value= j_grt_value_from_java_xml_object(grt, env, msgs);

    // convert the Grt values to Grt messages
    MYX_GRT_MSGS *msgs= myx_grt_messages_convert(value);

    // call the callback function to process the messages
    grt->process_messages(msgs, grt->process_messages_data);

    myx_grt_messages_free(msgs);

    myx_grt_value_release(value);
  }
}

JNIEXPORT jint JNICALL Java_com_mysql_grt_GrtCallbackNative_processStatusQuery
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  jint res= 0;
  
  if (grt->process_status_query)
  {
    // call the callback function to query the status, if returned a int <> 0 the execution will be aborted
    res= grt->process_status_query(grt->process_status_query_data);
  }

  return res;
}

JNIEXPORT void JNICALL Java_com_mysql_grt_GrtCallbackNative_setBridgeDataObject
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath, jobject dataObj)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  if (dataObj == NULL)
  {
    dataObj= myx_grt_value_bridge_data_object_get(value);
    (*env)->DeleteGlobalRef(env, dataObj);
    dataObj= NULL;
  }
  else
    dataObj= (*env)->NewGlobalRef(env, dataObj);

  myx_grt_value_bridge_data_object_set(value, dataObj);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  GRT_LEAVE(grt);
}

JNIEXPORT jobject JNICALL Java_com_mysql_grt_GrtCallbackNative_getBridgeDataObject
  (JNIEnv *env, jobject obj, jlong grtMyxGrtPointer, jstring objectPath)
{
  MYX_GRT *grt= LongToPtr(grtMyxGrtPointer);
  const char *c_str_objectPath= (*env)->GetStringUTFChars(env, objectPath, 0);
  MYX_GRT_VALUE *value;
  jobject dataObj;
  
  GRT_ENTER(grt);
  
  value= myx_grt_dict_item_get_by_path(grt, myx_grt_get_root(grt), c_str_objectPath);

  dataObj= myx_grt_value_bridge_data_object_get(value);

  if (c_str_objectPath)
    (*env)->ReleaseStringUTFChars(env, objectPath, c_str_objectPath);

  GRT_RETURN(grt, dataObj, jobject);
}

// -------------------------------------------------------------------------------------------------

#if defined(__WIN__) || defined(_WIN32) || defined(_WIN64)

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

static int get_java_runtimelib_filename(const char *version, char **runtimelib_filename)
{
  HKEY java_key;
  HKEY java_subkey;
  TCHAR subkey_name[MAX_KEY_LENGTH];   // buffer for subkey name
  DWORD subkey_length;
  DWORD cSubKeys=0;               // number of subkeys 
  DWORD i, retCode;
  char *best_hit= NULL;
  int usedSubversion = -1;
  int usedBuild = 0;

  // Find the Java key
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\JavaSoft\\Java Runtime Environment", 0, KEY_READ, &java_key) != 0)
	  return MYX_GRT_JAVA_NOT_FOUND;

  // Get the class name and the value count. 
  RegQueryInfoKey(java_key, NULL, NULL, NULL, &cSubKeys, NULL, NULL, NULL, NULL, NULL, NULL, NULL); 

  for (i= 0; i < cSubKeys; i++) 
  {
    subkey_length= MAX_KEY_LENGTH;
    retCode = RegEnumKeyEx(java_key, i, subkey_name, &subkey_length, NULL, NULL, NULL, NULL);
    if (retCode == ERROR_SUCCESS) 
    {
      char *better_hit= NULL;

      if (str_beginswith(subkey_name, version))
      {
        DWORD reg_value_type;
        BOOL fileExists = TRUE;
        char buffer[MAX_VALUE_NAME];
        int bufferSize = MAX_VALUE_NAME;

        // Check if the jvm files exists and keep its name if it does, otherwise skip the entry.
        RegOpenKeyEx(java_key, subkey_name, 0, KEY_READ, &java_subkey);
        retCode = RegQueryValueEx(java_subkey, "RuntimeLib", NULL, &reg_value_type, buffer, &bufferSize) == 0;
        RegCloseKey(java_subkey);
        if (retCode == ERROR_SUCCESS);
        {
          // We got the RuntimeLib entry. Check whether it is a string value and points to an exisiting file.
          if (reg_value_type == REG_SZ)
          {
            TCHAR filename[MAX_PATH];
            FILE* file;

            ExpandEnvironmentStrings(buffer, filename, MAX_PATH);
            file = fopen(filename, "r");
            if (file != NULL)
            {
              fileExists = TRUE;
              fclose(file);
              better_hit = g_strdup(filename);
            };
          };

          if (fileExists)
          {
            // Now check the version info to find the highest installed version.
            // Since the major and minor version numbers are given as parameters we only need 
            // to check the sub version and the build numbers (e.g. as in 1.5.0_02).
            gchar** parts = g_strsplit(subkey_name, ".", 3);
            
            // No need to process subversion or build if they aren't given (e.g. as in 1.5).
            if (parts[1] != NULL && parts[2] != NULL)
            {
              int subVersion, build;
              gchar** restParts = g_strsplit(parts[2], "_", 2);
              subVersion = atoi(restParts[0]);
              if (restParts[1] != NULL)
                build = atoi(restParts[1]);
              else
                build = 0;
              g_strfreev(restParts);
              if (usedSubversion < 0)
              {
                // Nothing found yet, so use this entry.
                best_hit = better_hit;
                usedSubversion = subVersion;
                usedBuild = build;
              }
              else
                if ((usedSubversion < subVersion) || (usedSubversion == subVersion && usedBuild < build))
                {
                  usedSubversion = subVersion;
                  usedBuild = build;
                  g_free(best_hit);
                  best_hit = better_hit;
                };
            }
            else
              if (usedSubversion < 0)
              {
                // Nothing found yet, so use this entry.
                best_hit = better_hit;
                usedSubversion = 0;
                usedBuild = 0;
              };

            g_strfreev(parts);
          };
        };
      };
    };
  };
  RegCloseKey(java_key);

  if (best_hit)
  {
    *runtimelib_filename= best_hit;
    return MYX_GRT_NO_ERROR;
  }
  else
    return MYX_GRT_JAVA_NOT_FOUND;
}

#else // !windows

#ifdef DLOAD_JVM
static int get_java_runtimelib_filename(const char *version, char **runtimelib_filename)
{
  char *path= getenv("GRT_JVM_PATH");
  if (!path)
  {
    return MYX_GRT_JAVA_NOT_FOUND;
  }
  
  if (access(path, F_OK)!=0)
    return MYX_GRT_JAVA_NOT_FOUND;
  
  *runtimelib_filename= g_strdup(path);

  return MYX_GRT_NO_ERROR;
}
#endif // DLOAD_JVM

#endif // !windows

#endif // ENABLE_JAVA_MODULES
