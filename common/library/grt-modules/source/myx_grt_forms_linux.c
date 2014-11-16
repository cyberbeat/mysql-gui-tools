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
#include <gtk/gtk.h>



typedef struct {
  MYX_GRT *grt;

  MYX_GRT_VALUE *form_list;
  
  int initializing;
} GTK_FORM_DATA;


typedef enum {
  GTKO_APPLICATION,
  GTKO_FORM
} GTKF_OBJECT_TYPE;


typedef struct {
  GTKF_OBJECT_TYPE type;
  GtkWidget *widget;
} GTKF_OBJECT;

#ifndef ENABLE_FORMS
MYX_GRT_MODULE* myx_register_builtin_grt_module_forms(MYX_GRT *grt)
{
}
#else // !ENABLE_FORMS
// --------------------------------------------------------------------------
// module registration functions

MYX_GRT_MODULE* myx_register_builtin_grt_module_forms(MYX_GRT *grt)
{
  int argc= 1;
  char **argv;
  GTK_FORM_DATA *data= g_new0(GTK_FORM_DATA, 1);

  argv= g_new0(char*,2);
  argv[0]= g_strdup(g_get_prgname());
  argv[1]= NULL;

  gtk_init(&argc, (char***)&argv);
  
  data->grt= grt;
  data->initializing= 0;
  data->form_list= myx_grt_list_new(MYX_DICT_VALUE, NULL);
  myx_grt_value_bridge_dict_key_set(data->form_list, "forms");

  return myx_grt_module_register_builtin(grt, &grt_module_forms, data);
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
  GTK_FORM_DATA *fdata= (GTK_FORM_DATA*)data;
  MYX_GRT_VALUE *app;
  const char *path;
  
  fdata->initializing= 1;
  
  app= myx_grt_dict_new_obj(fdata->grt, "forms.Application",
    g_get_prgname(), myx_grt_get_guid(), NULL);

  fdata->initializing= 0;
  
  if (myx_grt_value_get_type(param) == MYX_STRING_VALUE)
    path= myx_grt_value_as_string(param);
  else if (myx_grt_value_get_type(param) == MYX_LIST_VALUE)
    path= myx_grt_list_item_get_as_string(param, 0);

  myx_grt_dict_item_set_by_path(myx_grt_get_root(fdata->grt), path, app);
  
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
//  GTK_FORM_DATA *fdata= (GTK_FORM_DATA*)data;

  g_message("CREATE %s", struct_name);
  
  if (strcmp2(struct_name, "forms.Application") == 0)
  {    
    GTKF_OBJECT *object= g_new0(GTKF_OBJECT, 1);

    object->type= GTKO_APPLICATION;
    object->widget= NULL;

    myx_grt_value_bridge_data_set(param, object);
  }
  else if (strcmp2(struct_name, "forms.Form")==0)
  {
    void *bdata= myx_grt_value_bridge_object_data_get(param);
    
    if (!bdata)
    {
      GtkWidget *window= gtk_window_new(GTK_WINDOW_TOPLEVEL);
      GTKF_OBJECT *object= g_new0(GTKF_OBJECT, 1);

      object->type= GTKO_FORM;
      object->widget= window;
      
      myx_grt_value_bridge_data_set(param, object);
    }
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
  GTK_FORM_DATA *fdata= (GTK_FORM_DATA*)data;
  GTKF_OBJECT *object= (GTKF_OBJECT*)myx_grt_value_bridge_data_get(param);
  const char *key= myx_grt_value_bridge_dict_key_get(param);

  if (object->type == GTKO_APPLICATION)
  {
    //XXX forms is NULL here, why???

    if (strcmp2(key, "forms")==0)
    {
      unsigned int count= myx_grt_bridge_list_item_count(param);
      unsigned int j;
      GList *toplevels= gtk_window_list_toplevels();
      GList *l;

      g_message("UPDAE FORMS %s", myx_get_value_type_as_string(param->type));
      
      l= toplevels;
      
      // add missing windows
      while (l)
      {
        MYX_GRT_VALUE *lvalue;
        GTKF_OBJECT *obj;
        int found= 0;
        
        for (j= 0; j < count; j++)
        {
          lvalue= myx_grt_bridge_list_item_get(param, j, 0);
          obj= myx_grt_value_bridge_data_get(lvalue);
          
          if (obj->widget == l->data)
          {
            found= 1;
            break;
          }
        }
        if (!found)
        {
          fdata->initializing= 1;
          
          obj= g_new0(GTKF_OBJECT, 1);
          
          obj->type= GTKO_APPLICATION;
          obj->widget= l->data;
          
          lvalue= myx_grt_bridge_dict_new(fdata->grt, "forms.Form", obj);
          
          myx_grt_dict_init_obj(fdata->grt, lvalue,
                                gtk_widget_get_name(obj->widget),
                                NULL, NULL);
          
          g_message("NEW FORM");
          myx_grt_bridge_list_item_insert(param, -1, lvalue, 0);
          
          fdata->initializing= 0;
        }
        l= g_list_next(l);
      }
      
      // remove gone windows
      for (j= 0; j < count; j++)
      {
        int found= 0;
        MYX_GRT_VALUE *lvalue= myx_grt_bridge_list_item_get(param, j, 0);
        GTKF_OBJECT *obj= myx_grt_value_bridge_data_get(lvalue);

        while (l)
        {
          if (obj->widget == l->data)
          {
            found= 1;
            break;
          }
        }
        if (!found)
        {          
          myx_grt_bridge_list_item_del(param, j, 0);
        }
        l= g_list_next(l);
      }
      
      g_list_free(toplevels);
    }
  }
  
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
  GTK_FORM_DATA *fdata= (GTK_FORM_DATA*)data;
  GTKF_OBJECT *object;
  const char *key;

  if (fdata->initializing)
    return NULL;
  
  object= (GTKF_OBJECT*)myx_grt_value_bridge_data_get(param);
  if (!object)
  {
    g_warning("trying to manipulate invalid forms object");
    return NULL;
  }
  
  key= myx_grt_value_bridge_dict_key_get(param);
  
  switch (object->type)
  {
  case GTKO_APPLICATION:
    if (strcmp(key, "forms")==0)
      ;
    g_message("APPKEY %s", key);
    
    break;
  case GTKO_FORM:
    if (strcmp(key, "top")==0)
    {
      int v= myx_grt_bridge_value_as_int(param);
      int x, y;
      gtk_window_get_position(GTK_WINDOW(object->widget), &x, &y);
      gtk_window_move(GTK_WINDOW(object->widget), x, v);
    } 
    else if (strcmp(key, "left")==0)
    {
      int v= myx_grt_bridge_value_as_int(param);
      int x, y;
      gtk_window_get_position(GTK_WINDOW(object->widget), &x, &y);
      gtk_window_move(GTK_WINDOW(object->widget), v, y);
    } 
    else if (strcmp(key, "width")==0)
    {
      int v= myx_grt_bridge_value_as_int(param);
      int w, h;
      if (v > 0)
      {
        gtk_window_get_size(GTK_WINDOW(object->widget), &w, &h);
        if (h < 1) h= 10;
        gtk_window_resize(GTK_WINDOW(object->widget), v, h);
      }
    }
    else if (strcmp(key, "height")==0)
    {
      int v= myx_grt_bridge_value_as_int(param);
      int w, h;
      if (v > 0)
      {
        gtk_window_get_size(GTK_WINDOW(object->widget), &w, &h);
        if (w < 1) w= 10;
        gtk_window_resize(GTK_WINDOW(object->widget), w, v);
      }
    }
//    g_message("FORMKEY %s", key);
    
  default: // handle common things here
    if (strcmp(key, "visible")==0)
    {
      if (myx_grt_bridge_value_as_int(param))
        gtk_widget_show(object->widget);
      else
        gtk_widget_hide(object->widget);
      break;
    }
    else if (strcmp(key, "name")==0)
    {
      g_message("set name to %s", myx_grt_bridge_value_as_string(param));
      gtk_widget_set_name(object->widget, myx_grt_bridge_value_as_string(param));
      break;
    }
    
  }

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
  GTK_FORM_DATA *fdata= (GTK_FORM_DATA*)data;
  GTKF_OBJECT *object= myx_grt_value_bridge_data_get(param);

  if (object != NULL)
  {
    const char *key= myx_grt_value_bridge_dict_key_get(param);

    switch (object->type)
    {
    case GTKO_APPLICATION:
      if (strcmp(key, "forms")==0)
      {
        param->value.l= fdata->form_list;
        g_message("GET FORMS");
        
        break;
      }

    case GTKO_FORM:
      if (strcmp(key, "top")==0)
      {
        gint x, y;
        gtk_window_get_position(GTK_WINDOW(object->widget), &x, &y);
        myx_grt_bridge_value_change_int(param, y);
        break;
      }
      else if (strcmp(key, "left")==0)
      {
        gint x, y;
        gtk_window_get_position(GTK_WINDOW(object->widget), &x, &y);
        myx_grt_bridge_value_change_int(param, x);
        break;
      }
      else if (strcmp(key, "width")==0)
      {
        gint w, h;
        gtk_window_get_size(GTK_WINDOW(object->widget), &w, &h);
        myx_grt_bridge_value_change_int(param, w);
        break;
      }
      else if (strcmp(key, "height")==0)
      {
        gint w, h;
        gtk_window_get_size(GTK_WINDOW(object->widget), &w, &h);
        myx_grt_bridge_value_change_int(param, h);
        break;
      }
      
    default:
      if (strcmp(key, "name")==0)
      {
        g_message("GET NAME");
        myx_grt_bridge_value_change_string(param, gtk_widget_get_name(object->widget));
      }
      break;
    }
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
#endif //!ENABLE_FORMS
