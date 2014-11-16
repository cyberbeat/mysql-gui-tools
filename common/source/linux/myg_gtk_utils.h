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


#ifndef _MYG_GTK_UTILS_H_
#define _MYG_GTK_UTILS_H_

#include <glib.h>
#include <gtk/gtk.h>


void myg_entry_history_attach(GtkWidget *entry);
void myg_entry_history_add(GtkWidget *entry, const char *text);


GtkWidget *myg_add_text_to_dialog(GtkDialog *dialog);
void myg_text_append(GtkWidget *text_view, const char *text);


GtkWidget *myg_create_search_entry(const char **menu_options,
                                   GtkWidget **entry_ret);

GdkPixbuf *myg_copy_scale_pixbuf(GdkPixbuf *icon, int to_width, int to_height);

void myx_tree_make_row_visible(GtkTreeView *view, GtkTreeIter *node);



#endif /* _MYG_GTK_UTILS_H_ */
