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



#ifndef _MGGRAPHPLOTTER_H_
#define _MGGRAPHPLOTTER_H_

#include <gtkmm.h>


class MGGraphPlotter : public Gtk::DrawingArea {
  protected:
    bool _initialized;
    
    Glib::RefPtr<Gdk::Drawable> _buffer;

    int _width;
    int _height;
    bool _resized; 
    bool _use_buffer;
   
    bool _refreshing;

    virtual void refresh();

    virtual void update();

    virtual bool on_expose_event(GdkEventExpose *event);
    virtual bool on_configure_event(GdkEventConfigure *event);

  public:
    MGGraphPlotter(GtkDrawingArea *darea, bool use_buffer= true);
    MGGraphPlotter(bool use_buffer= true);

    void set_back_color(const Gdk::Color &color);
};


#endif /* _MGGRAPHPLOTTER_H_ */
