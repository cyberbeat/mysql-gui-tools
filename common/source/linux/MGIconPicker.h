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


#ifndef _MGICONPICKER_H_
#define _MGICONPICKER_H_


#include <gnome.h>

#include "MGBase.h"

class MGIconPicker : MGBase {
    GtkImage *image;
    
    GtkWidget *_text;
    GtkWidget *_dialog;
    GtkWidget *_icon_chooser;

    GdkPixbuf *_icon;
    
  public:
    MGIconPicker(GtkWidget *parent, const char *title,
                 const char *ok, const char *cancel);
    ~MGIconPicker();

    virtual GtkWidget *widget() { return _dialog; };

    void set_title(const char *title);
    void set_max_size(int max_width, int max_height);
    
    void set_image_widget(GtkImage *image);
    
    GdkPixbuf *get_icon();
    
    bool run();
};


#endif /* _MGICONPICKER_H_ */
