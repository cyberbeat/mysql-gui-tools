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



#ifndef _MGTIMEGRAPHPLOTTER_H_
#define _MGTIMEGRAPHPLOTTER_H_


#include <list>
#include <time.h>

#include "MGGraphPlotter.h"

class MGTimeGraphPlotter : public MGGraphPlotter {
    struct PointData {
      double value;
      time_t timestamp;
    };
    std::list<PointData> _data;

    double _range_min;
    double _range_max;
    
    time_t _last_time;
    int _grid_offset;

    double _pixels_per_second;
    int _grid_width;
    int _grid_height;

    Glib::RefPtr<Gdk::GC> _line_gc;
    Glib::RefPtr<Gdk::GC> _grid_gc;
    Gdk::Color _line_color;
    Gdk::Color _grid_color;
    
    Glib::ustring _timestamp_format;
    
    Glib::ustring _caption;

    virtual void update();
    
  public:
    MGTimeGraphPlotter(GtkDrawingArea *darea);
    MGTimeGraphPlotter();

    void set_range(double min, double max);
        
    void set_grid_size(int xsize, int ysize);
    void set_pixels_per_second(double p);

    void set_grid_color(const Gdk::Color &color);
    void set_line_color(const Gdk::Color &color);

    void set_caption(const Glib::ustring &caption);
    void set_timestamp_format(const Glib::ustring &fmt);

    void add_value(double value, time_t timestamp);
    
    void get_stats(double &min, double &max, double &avg);
};


#endif /* _MGTIMEGRAPHPLOTTER_H_ */
