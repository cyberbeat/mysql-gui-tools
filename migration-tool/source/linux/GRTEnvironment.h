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

#ifndef _GRTENVIRONMENT_H_
#define _GRTENVIRONMENT_H_

#include <glibmm.h>
#include "myx_migration_public_interface.h"

class GRTEnvironment {
  protected:
    std::vector<Glib::ustring> get_messages();

    MYX_GRT_ENV *_env;
    int _msgOffset;
    std::vector<Glib::ustring> _sourceObjectNames;

    sigc::slot<void,const Glib::ustring&> _shellOutputHandler;
    
    static void process_shell_output(const char *text, void *udata);
        
  public:
    GRTEnvironment();
    ~GRTEnvironment();
    
    MYX_GRT_ENV *get_env() { return _env; };

    int update_objects();
    int init_jni(const std::string &classpath);
    int execute_shell_command(const Glib::ustring &command);

    void set_shell_output_handler(const sigc::slot<void,const Glib::ustring&> &slot);

    MYX_GRT_OBJ *get_object(const Glib::ustring &name);
    bool object_implements_interface(MYX_GRT_OBJ *obj, const Glib::ustring &name);
    Glib::ustring get_object_var_value(MYX_GRT_OBJ *obj, const Glib::ustring &name);

    MYX_GRT_OBJ_REFLIST *get_object_reference_list(MYX_GRT_OBJ *obj, const Glib::ustring &name);
    Glib::ustring get_object_var_type(MYX_GRT_OBJ *obj, const Glib::ustring &name);
    unsigned int get_object_var_count(MYX_GRT_OBJ *obj, bool objectListOnly);
    MYX_GRT_VAR *get_object_var(MYX_GRT_OBJ *obj, unsigned int index, bool objectListOnly);
    
    int get_object_func_count(MYX_GRT_OBJ *obj, bool publicOnly);
    MYX_GRT_FUNC *get_object_func(MYX_GRT_OBJ *obj, unsigned int index, bool publicOnly);
    Glib::ustring get_object_class_name(MYX_GRT_OBJ *obj);
    bool compare_object_class_name(MYX_GRT_OBJ *obj, const Glib::ustring &className);

    MYX_GRT_OBJ *get_object_reference_list_obj(MYX_GRT_OBJ_REFLIST *reflist, unsigned int index);
    unsigned int get_object_reference_list_count(MYX_GRT_OBJ_REFLIST *reflist);

    Glib::ustring get_var_name(MYX_GRT_VAR *var);
    Glib::ustring get_func_name(MYX_GRT_FUNC *func);
    Glib::ustring get_func_modifiers(MYX_GRT_FUNC *func);
};


#endif /* _GRTENVIRONMENT_H_ */
