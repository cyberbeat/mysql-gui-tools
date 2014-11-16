/* Copyright (C) 2003, 2004 MySQL AB

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



#ifndef _MGMETERGRAPHPLOTTER_H_
#define _MGMETERGRAPHPLOTTER_H_

#include "MGGraphPlotter.h"


class MGMeterGraphPlotter : public MGGraphPlotter {
    Glib::RefPtr<Gdk::Pixmap> _back_image;
    Glib::RefPtr<Gdk::Pixmap> _fore_image;

    int _segment_number;
    int _value;
    
    Glib::ustring _text_format;

    virtual void update();
  public:
    MGMeterGraphPlotter(GtkDrawingArea *darea);
    MGMeterGraphPlotter();

    void set_images(const Glib::RefPtr<Gdk::Pixbuf> &back,
                    const Glib::RefPtr<Gdk::Pixbuf> &fore);
    void set_images(const Glib::RefPtr<Gdk::Pixmap> &back,
                    const Glib::RefPtr<Gdk::Pixmap> &fore);
    void set_text_format(const Glib::ustring &fmt);
    void set_segment_number(int n);
    void set_value(int value);
};


#endif /* _MGMETERGRAPHPLOTTER_H_ */
