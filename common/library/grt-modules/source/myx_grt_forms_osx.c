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

#include <myx_grt_forms.h>

// --------------------------------------------------------------------------
// module registration functions

MYX_GRT_MODULE* myx_register_builtin_grt_module_forms(MYX_GRT *grt)
{
  return myx_grt_module_register_builtin(grt, &grt_module_forms, grt);
}

/**
 ****************************************************************************
 * @brief Creates a forms application dict and adds it to the globals tree
 *
 *   Creates a forms application dict and adds it to the globals tree at the
 * given position
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_create(MYX_GRT_VALUE *param, void *data)
{
  MYX_GRT *grt= data;
  MYX_GRT_VALUE *app= myx_grt_dict_new_obj(grt, "forms.Application",
    "GTK Application Name", myx_grt_get_guid(), NULL);
  const char *path;

  if (myx_grt_value_get_type(param) == MYX_STRING_VALUE)
    path= myx_grt_value_as_string(param);
  else if (myx_grt_value_get_type(param) == MYX_LIST_VALUE)
    path= myx_grt_list_item_get_as_string(param, 0);

  myx_grt_dict_item_set_by_path(myx_grt_get_root(grt), path, app);

  return NULL;
}

/**
 ****************************************************************************
 * @brief Assoziates the given dict with the real world object
 *
 *   Sets the bridge info of the given MYX_GRT_VALUE
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_init_dict(MYX_GRT_VALUE *param, void *data)
{
  const char *struct_name= myx_grt_dict_struct_get_name(param);

  if (strcmp2(struct_name, "forms.Application") == 0)
  {
    // store a pointer to the Cocoa application object
    myx_grt_value_bridge_data_set(param, NULL);
  }

  return NULL;
}

/**
 ****************************************************************************
 * @brief Updates the given dict or list
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_update(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}

/**
 ****************************************************************************
 * @brief Sets the value in the real world object
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_set_value(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}

/**
 ****************************************************************************
 * @brief Gets the value from the real world object and updates the GRT value
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_get_value(MYX_GRT_VALUE *param, void *data)
{
  void *bridge_data= myx_grt_value_bridge_data_get(param);

  // check if data is pointing to the Cocoa application object
  if (bridge_data != NULL)
  {
    const char *key= myx_grt_value_bridge_dict_key_get(param);

    if (strcmp2(key, "Name") == 0)
      myx_grt_value_change_string(param, "Real Application Name");
  }

  return NULL;
}

/**
 ****************************************************************************
 * @brief Deletes the real world object
 *
 * @param param the path where the forms dict should be created
 * @param data buildin module private pointer to the GRT struct
 * 
 * @return NULL
 *****************************************************************************/
MYX_GRT_VALUE *forms_del_value(MYX_GRT_VALUE *param, void *data)
{
  return NULL;
}