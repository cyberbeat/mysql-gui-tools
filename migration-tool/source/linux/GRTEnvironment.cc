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


#include "GRTEnvironment.h"
#include "myg_utils.h"

#define _(s) s

void GRTEnvironment::process_shell_output(const char *text, void *udata)
{
  GRTEnvironment *me= (GRTEnvironment*)udata;

  me->_shellOutputHandler(text);
}


GRTEnvironment::GRTEnvironment()
  : _env(0), _msgOffset(0)
{
  int error;
  
  _env= myx_grt_initialize();

  myx_grt_set_shell_output_callback(_env, this, process_shell_output);

  //Init LUA system right from the start
  myx_grt_lua_system_initialize(_env);
}


GRTEnvironment::~GRTEnvironment()
{  
  myx_grt_finalize(_env);
}


int GRTEnvironment::update_objects()
{
  int error;

  if (!_env->jni_sys)
  {
    _shellOutputHandler("ERROR: GRT system not initialized");
    return -1;
  }

  //Retrieve Classes
  myx_grt_get_jni_classes(_env, &error);
  if (error != MYX_MIGRATION_NO_ERROR)
  {
    _shellOutputHandler(ufmt(_("Cannot retrieve classes (%d)."), error));
    return -1;
  }

  myx_grt_get_jni_objs(_env, &error);
  if (error != MYX_MIGRATION_NO_ERROR)
  {
    _shellOutputHandler(ufmt(_("Cannot retrieve objects (%d)."), error));
    return -1;
  }
  return 0;
}


void GRTEnvironment::set_shell_output_handler(const sigc::slot<void,const Glib::ustring&> &slot)
{
  _shellOutputHandler= slot;

  //Print shell welcome message
  myx_grt_shell_print_welcome(_env);
}


int GRTEnvironment::init_jni(const std::string &classpath)
{
  int error;
  
  myx_grt_jni_system_initialize(_env, classpath.c_str(), &error);

  return error;
}

int GRTEnvironment::execute_shell_command(const Glib::ustring &command)
{
  int rc= myx_grt_shell_execute(_env, command.c_str());
  int error;

  error= myx_grt_shell_execute(_env, command.c_str());
  if (error!=MYX_MIGRATION_NO_ERROR) 
  {
    _shellOutputHandler(ufmt(_("Cannot execute command (%d)."), error));
    rc= -1;
  }
  return rc;
}


std::vector<Glib::ustring> GRTEnvironment::get_messages()
{
  std::vector<Glib::ustring> msgs;
  int error;
    
  if(!_env->jni_sys) return msgs;

  myx_grt_get_jni_messages(_env, _msgOffset, &error);

  if (error!=MYX_MIGRATION_NO_ERROR)
    msgs.push_back(ufmt(_("ERROR: Could not retieve messages. (%d)"), error));

  for (unsigned int i= 0; i < _env->msgs->msgs_num; i++)
  {
    msgs.push_back(_env->msgs->msgs[i].msg);
    if (_env->msgs->msgs[i].msg_detail && _env->msgs->msgs[i].msg_detail->strings_num > 0)
      for (unsigned int j= 0; j < _env->msgs->msgs[i].msg_detail->strings_num; j++)
        msgs.push_back("  "+Glib::ustring(_env->msgs->msgs[i].msg_detail->strings[j]));
  }

  _msgOffset+= _env->msgs->msgs_num;
  
  return msgs;
}


MYX_GRT_OBJ *GRTEnvironment::get_object(const Glib::ustring &name)
{
  return myx_grt_get_object(_env, name.c_str());
}

Glib::ustring GRTEnvironment::get_object_var_value(MYX_GRT_OBJ *obj, const Glib::ustring &name)
{
  return uswrapf(myx_grt_get_obj_var_value(obj, name.c_str()));
}

bool GRTEnvironment::object_implements_interface(MYX_GRT_OBJ *obj, const Glib::ustring &name)
{
  return myx_grt_obj_implements_interface(obj, name.c_str()) != 0;
}


MYX_GRT_OBJ_REFLIST *GRTEnvironment::get_object_reference_list(MYX_GRT_OBJ *obj, const Glib::ustring &name)
{
  return myx_grt_obj_reference_list(obj, name.c_str());
}

Glib::ustring GRTEnvironment::get_object_var_type(MYX_GRT_OBJ *obj, const Glib::ustring &name)
{
  return uswrapf(myx_grt_get_object_var_type(obj, name.c_str()));
}

unsigned int GRTEnvironment::get_object_var_count(MYX_GRT_OBJ *obj, bool objectListOnly)
{
  return myx_grt_get_obj_var_count(obj, objectListOnly);
}

MYX_GRT_VAR *GRTEnvironment::get_object_var(MYX_GRT_OBJ *obj, unsigned int index, bool objectListOnly)
{
  return myx_grt_get_obj_var_by_index(obj, index, objectListOnly);
}

int GRTEnvironment::get_object_func_count(MYX_GRT_OBJ *obj, bool publicOnly)
{
  return myx_grt_get_obj_func_count(obj, publicOnly);
}

MYX_GRT_FUNC *GRTEnvironment::get_object_func(MYX_GRT_OBJ *obj, unsigned int index, bool publicOnly)
{
  return myx_grt_get_obj_func_by_index(obj, index, publicOnly);
}

Glib::ustring GRTEnvironment::get_object_class_name(MYX_GRT_OBJ *obj)
{
  return uswrapf(myx_grt_get_obj_class_name(obj));
}


bool GRTEnvironment::compare_object_class_name(MYX_GRT_OBJ *obj, const Glib::ustring &className)
{
  char *tmp=myx_grt_get_obj_class_name(obj);
  bool res;
  
  res= (strcmp(tmp, className.c_str())==0);
  g_free(tmp);
  return res;
}

MYX_GRT_OBJ *GRTEnvironment::get_object_reference_list_obj(MYX_GRT_OBJ_REFLIST *reflist, unsigned int index)
{
  return myx_grt_obj_reference_list_obj(_env, reflist, index);
}

unsigned int GRTEnvironment::get_object_reference_list_count(MYX_GRT_OBJ_REFLIST *reflist)
{
  return myx_grt_obj_reference_list_obj_count(reflist);
}


Glib::ustring GRTEnvironment::get_var_name(MYX_GRT_VAR *var)
{
  return uswrapf(myx_grt_get_var_name(var));
}

Glib::ustring GRTEnvironment::get_func_name(MYX_GRT_FUNC *func)
{
  return uswrapf(myx_grt_get_func_name(func));
}

Glib::ustring GRTEnvironment::get_func_modifiers(MYX_GRT_FUNC *func)
{
  return uswrapf(myx_grt_get_func_modifiers(func));
}


