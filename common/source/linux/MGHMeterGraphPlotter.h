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



#ifndef _MGHMETERGRAPHPLOTTER_H_
#define _MGHMETERGRAPHPLOTTER_H_

#include "MGGraphPlotter.h"


class MGHMeterGraphPlotter : public MGGraphPlotter {
  public:
    enum Unit {
      Percentage=0,
        Count,
        Byte,
        Seconds
    };
  protected:
    Glib::RefPtr<Gdk::Pixbuf> _back_image;
    Glib::RefPtr<Gdk::Pixbuf> _fore_image;

    Glib::RefPtr<Gdk::Pixmap> _back_pixmap;
    Glib::RefPtr<Gdk::Pixmap> _fore_pixmap;

    double _current_value;
    double _total_value;
    Unit _unit;

    Glib::ustring _current_format;
    Glib::ustring _total_format;

    virtual void update();
    
    Glib::ustring format_value(double value);
  public:
    MGHMeterGraphPlotter(GtkDrawingArea *darea);
    MGHMeterGraphPlotter();

    void set_images(const Glib::RefPtr<Gdk::Pixbuf> &back,
                    const Glib::RefPtr<Gdk::Pixbuf> &fore);
    // if _divide_multiples, use %s otherwise %f
    void set_current_format(const Glib::ustring &fmt);
    // if _divide_multiples, use %s otherwise %f
    void set_total_format(const Glib::ustring &fmt);
    void set_value_unit(Unit unit);
    void set_current_value(double value);
    void set_total_value(double value);
};


#endif /* _MGHMETERGRAPHPLOTTER_H_ */
