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



#include "MGIconPicker.h"
#include <libgnomeui/gnomeui.h>

MGIconPicker::MGIconPicker(GtkWidget *parent, const char *title,
                           const char *ok, const char *cancel)
    : _text(0), _icon(0)
{
  _dialog = gtk_dialog_new_with_buttons(title, parent,
                                        GTK_DIALOG_MODAL|GTK_DIALOG_NO_SEPARATOR,
                                        ok, cancel, NULL);
  
  _icon_chooser= gnome_icon_selection_new();
  
}


MGIconPicker::~MGIconPicker()
{
  if (_icon)
      g_object_unref(G_OBJECT(_icon));

  gtk_widget_destroy(_dialog);
}


void MGIconPicker::set_text(const char *text)
{
  if (!_text)
  {
    _text= gtk_label_new(text);
    gtk_box_pack_start(GTK_BOX(_dialog->vbox), _text, FALSE, TRUE, 0);
    gtk_widget_show(_text);
  }
  else
  {
    gtk_label_set_text(GTK_LABEL(_text), text);
  }
}


void MGIconPicker::set_image_widget(GtkImage *image)
{
  _image= image;
}


GdkPixbuf *MGIconPicker::get_icon()
{
  return _icon;
}

    
bool MGIconPicker::run()
{
  return gdk_dialog_run(_dialog)==GTK_RESPONSE_OK;
}
