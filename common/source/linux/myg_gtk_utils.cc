/* Copyright (C) 2003 MySQL AB

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


/*
 * Misc. gtk related utilities.
 * 
 * TODO: clean up, convert to C++ and merge with myg_gtkutils
 */

#include "mygpriv.h"

#include <string.h>
#include <glib.h>
#include <gtk/gtk.h>


/**********************************************************************/

static void entry_destroy_handler(GtkObject *obj)
{
  GList *ptr;
  GList *list;

  list= (GList*)gtk_object_get_data(obj, "myg_history");

  for (ptr= g_list_first(list); ptr != NULL; ptr= g_list_next(ptr))
  {
    g_free(ptr->data);
  }

  g_list_free(list);
}


static gint entry_select_idle(gpointer data)
{
  gtk_editable_select_region(GTK_EDITABLE(data), 
                             gtk_editable_get_position(GTK_EDITABLE(data)), -1);

  return 0;
}


static void entry_changed_handler(GtkWidget *w, gpointer data)
{
  const char *text, *full_text;
  GList *list, *ptr;
  int res= -1;
  static int changing= 0;
  gint curpos;
  
  if (changing)
    return;
  changing= 1;

  list= (GList*)gtk_object_get_data(GTK_OBJECT(w), "myg_history");

  curpos= gtk_editable_get_position(GTK_EDITABLE(w));

  text= gtk_entry_get_text(GTK_ENTRY(w));

  if (*text && curpos == (int)strlen(text)-1) /* not empty and cursor at end */
  {
    /* find similar option */
    for (ptr= g_list_first(list); ptr != NULL; ptr= g_list_next(ptr))
    {
      full_text= (char*)ptr->data;
      res= strncasecmp(full_text, text, strlen(text));
      if (res >= 0)
          break;
    }
    
    if (res == 0)
    {    
      if (strcmp(full_text, text)!=0) 
      {        
        gtk_entry_set_text(GTK_ENTRY(w), full_text);
        gtk_editable_set_position(GTK_EDITABLE(w), curpos);
        /* this sucks, but putting the select_range() directly here doesnt work */
        gtk_idle_add(entry_select_idle, w);
      }
    }
  }
  
  changing= 0;
}



void myg_entry_history_attach(GtkWidget *entry)
{
  GList *history= NULL;

  gtk_object_set_data(GTK_OBJECT(entry), "myg_history", history);

  gtk_signal_connect(GTK_OBJECT(entry), "changed", 
                     (GtkSignalFunc)entry_changed_handler, NULL);
  gtk_signal_connect(GTK_OBJECT(entry), "destroy",
                     (GtkSignalFunc)entry_destroy_handler, NULL);
   
}


void myg_entry_history_add(GtkWidget *entry, const char *text)
{
  GList *history;
  
  history= (GList*)gtk_object_get_data(GTK_OBJECT(entry), "myg_history");

  history= (GList*)g_list_insert_sorted(history, g_strdup(text), 
                                        (GCompareFunc)strcasecmp);

  gtk_object_set_data(GTK_OBJECT(entry), "myg_history", history);
}



/**********************************************************************/

GtkWidget *myg_add_text_to_dialog(GtkDialog *dialog)
{
  GtkWidget *text;
  GtkWidget *scroll= gtk_scrolled_window_new(NULL, NULL);
  GtkWidget *frame;
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                 GTK_POLICY_NEVER,
                                 GTK_POLICY_AUTOMATIC);
  
  frame= gtk_frame_new(NULL);
  gtk_container_set_border_width(GTK_CONTAINER(frame), 10);
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_NONE);
  gtk_container_add(GTK_CONTAINER(frame), scroll);
  gtk_widget_show(scroll);
  
  text= gtk_text_view_new();
  gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
  gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(text), FALSE);

  gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroll), text);
  gtk_widget_show(text);
  
  gtk_widget_set_size_request(frame, -1, 100);

  gtk_box_pack_end(GTK_BOX(dialog->vbox), frame, TRUE, TRUE, 0);

  gtk_widget_show(frame);

  return text;
}



void myg_text_append(GtkWidget *text_view, const char *text)
{
  GtkTextIter iter;
  GtkTextBuffer *text_buf= gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view)); 
  
  gtk_text_buffer_get_end_iter(text_buf, &iter);

  gtk_text_buffer_insert(text_buf, &iter, text, -1);

  gtk_text_buffer_get_end_iter(text_buf, &iter);

  gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(text_view),
                               &iter,
                               0.0,
                               FALSE,
                               0, 0);
}


/**********************************************************************/



static gint search_entry_popup_handler(GtkWidget *widget, GdkEvent *event)
{
  GdkEventButton *event_button;

  event_button = (GdkEventButton *)event;
  gtk_menu_popup(GTK_MENU(widget), NULL, NULL, NULL, NULL, 
                 event_button->button, event_button->time);
  return TRUE;
}

static void search_entry_change_handler(GtkMenuItem *item, gpointer data)
{
  GtkWidget *entry= (GtkWidget*)data;
  GtkWidget *menu= (GtkWidget*)gtk_object_get_data(GTK_OBJECT(entry), 
                                                 "popup_menu");
  gtk_object_set_data(GTK_OBJECT(entry),
                      "selected_item",
                      (gpointer)g_list_index(GTK_MENU_SHELL(menu)->children,
                                             item));
}

/*
static gint myg_get_search_entry_option(GtkWidget *entry)
{  
  return (gint)gtk_object_get_data(GTK_OBJECT(entry), "selected_item");
}
 */


GtkWidget *myg_create_search_entry(const char **menu_options,
                                   GtkWidget **entry_ret)
{
  GtkWidget *frame;
  GtkWidget *box;
  GtkWidget *image;
  GtkWidget *entry;
  GtkWidget *menu;
  GtkWidget *item;
  GtkWidget *evbox;
  GSList *group= NULL;
  const char **s;

  frame= gtk_frame_new(NULL);
  gtk_frame_set_shadow_type(GTK_FRAME(frame), GTK_SHADOW_IN);


  box= gtk_hbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), box);
  
  evbox= gtk_event_box_new();
  gtk_box_pack_start(GTK_BOX(box), evbox, FALSE, FALSE, 0);
  gtk_widget_show(evbox);

  if (menu_options)
  {    
    image= gtk_image_new_from_file(std::string(myg_datadir+"/magnify_glass_with_popup.png").c_str());
  }
  else
  {
    image= gtk_image_new_from_file(std::string(myg_datadir+"/magnify_glass.png").c_str());
  }

  gtk_container_add(GTK_CONTAINER(evbox), image);
  gtk_widget_show(image);
  
  entry= gtk_entry_new();
  gtk_box_pack_start(GTK_BOX(box), entry, TRUE, TRUE, 0);
  gtk_entry_set_has_frame(GTK_ENTRY(entry), FALSE);
  gtk_widget_show(entry);
  
  gtk_widget_show(box);
  
    
  if (menu_options)
  {
    menu= gtk_menu_new();
    for (s= menu_options; *s; s++)
    {
      item= gtk_radio_menu_item_new_with_label(group, *s);
      gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
      gtk_widget_show(item);
      
      group= gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(item));
      
      g_signal_connect(GTK_OBJECT(item), "activate",
                       G_CALLBACK(search_entry_change_handler),
                       entry);
    }

    g_signal_connect_swapped(GTK_OBJECT(evbox), "button_press_event",
                             G_CALLBACK(search_entry_popup_handler),
                             GTK_OBJECT(menu));

    gtk_object_set_data(GTK_OBJECT(entry), "popup_menu", menu);
    gtk_object_set_data(GTK_OBJECT(entry), "selected_item", (gpointer)0);
  }

  if (entry_ret)
      *entry_ret= entry;
  
  return frame;
}


/**********************************************************************/


GdkPixbuf *myg_copy_scale_pixbuf(GdkPixbuf *icon, int to_width, int to_height)
{
  double ow, oh;
  GdkPixbuf *new_image;
  
  if (!icon)
    return NULL;
  
  ow= gdk_pixbuf_get_width(icon);
  oh= gdk_pixbuf_get_height(icon);

  new_image= gdk_pixbuf_new(gdk_pixbuf_get_colorspace(icon),
                            gdk_pixbuf_get_has_alpha(icon),
                            gdk_pixbuf_get_bits_per_sample(icon),
                            to_width, to_height);

  gdk_pixbuf_scale(icon, new_image,
                   0, 0, to_width, to_height,
                   0.0, 0.0,
                   (double)to_width/ow, (double)to_height/oh,
                   GDK_INTERP_BILINEAR);
  
  return new_image;
}


/**********************************************************************/

void myx_tree_make_row_visible(GtkTreeView *view, GtkTreeIter *node)
{
  GtkTreePath *path;
  
  path= gtk_tree_model_get_path(GTK_TREE_MODEL(gtk_tree_view_get_model(view)),
                                node);
  
  gtk_tree_view_scroll_to_cell(view, path, NULL, FALSE, 0, 0);
  
  gtk_tree_path_free(path);
}

