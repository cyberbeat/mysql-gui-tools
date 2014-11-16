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



#include "MGTimeGraphPlotter.h"


MGTimeGraphPlotter::MGTimeGraphPlotter(GtkDrawingArea *darea)
  : MGGraphPlotter(darea), _range_min(0), _range_max(100), 
    _last_time(0), _grid_offset(0), _pixels_per_second(1)
{
  _grid_width= 20;
  _grid_height= (int)(_range_max/5);
}


MGTimeGraphPlotter::MGTimeGraphPlotter()
  : MGGraphPlotter(), _range_min(0), _range_max(100),
    _last_time(0), _grid_offset(0), _pixels_per_second(1)
{
  _grid_width= 20;
  _grid_height= (int)((_range_max-_range_min)/5);
}


void MGTimeGraphPlotter::set_range(double min, double max)
{
  _range_min= min;
  _range_max= max;
  
  _grid_height= (int)((_range_max-_range_min)/5);
}


void MGTimeGraphPlotter::set_caption(const Glib::ustring &caption)
{
  _caption= caption;
}


void MGTimeGraphPlotter::set_timestamp_format(const Glib::ustring &fmt)
{
  _timestamp_format= fmt;
}

void MGTimeGraphPlotter::set_pixels_per_second(double val)
{
  _pixels_per_second= val;
}


void MGTimeGraphPlotter::set_grid_size(int xsize, int ysize)
{
  _grid_width= xsize;
  _grid_height= ysize;
}


void MGTimeGraphPlotter::set_grid_color(const Gdk::Color &color)
{
  _grid_color= color;
  
  if (_grid_gc)
  {
    _grid_gc->set_foreground(_grid_color);
  }
}


void MGTimeGraphPlotter::set_line_color(const Gdk::Color &color)
{
  _line_color= color;

  if (_line_gc)
  {
    _line_gc->set_foreground(_line_color);
  }
}


void MGTimeGraphPlotter::add_value(double value, time_t timestamp)
{
  PointData data;

  data.value= value;
  data.timestamp= timestamp;

  if (value > _range_max)
    _range_max= value+_grid_height - (int)value%_grid_height;

  _data.push_front(data);

  if (is_realized() && is_visible())
    update();
}


void MGTimeGraphPlotter::update()
{
  MGGraphPlotter::update();

  // clear back
  _buffer->draw_rectangle(get_style()->get_bg_gc(Gtk::STATE_NORMAL), true,
                          1, 1, _width-2, _height-2);

  std::vector<Gdk::Point> points;
  double x, y;
  time_t time0;
  time_t prev_time= 0;
  int gheight= _height;
  
  points.reserve(_data.size());
  
  if (!_line_gc)
  {
    _line_gc= Gdk::GC::create(get_window());
    _line_gc->set_foreground(_line_color);
    
    _grid_gc= Gdk::GC::create(get_window());
    _grid_gc->set_foreground(_grid_color);
  }

  // draw horizontal gridlines
  {
    int parts= (int)(_range_max-_range_min)/_grid_height;

    for (int i= 0; i < parts; i++)
    {
      _buffer->draw_line(_grid_gc, 1, (gheight/parts)*i,
                         _width-2, (gheight/parts)*i);
    }
  }

  if (_data.empty())
    time0= 0;
  else
    time0= _data.begin()->timestamp;

  // draw vertical gridlines
  {
    char buffer[64];
    
    if (_last_time != time0)
    {
      if (_last_time > 0)
      {
        _grid_offset+= (int)((time0-_last_time)*_pixels_per_second);
      }
      _grid_offset%= _grid_width;
      _last_time= time0;
    }

    for (int i= _width-1-_grid_offset; i > 0; i-= _grid_width)
    {
      _buffer->draw_line(_grid_gc, i, 1, i, gheight-2);

      time_t t= time0 - (int)(i*(_grid_width/_pixels_per_second));

      if (!_timestamp_format.empty() && t > 0)
      {
        strftime(buffer, sizeof(buffer), _timestamp_format.c_str(),
                 localtime(&t)); 

        Glib::RefPtr<Pango::Layout> l= create_pango_layout(buffer);
        int tw, th;

        l->get_pixel_size(tw, th);

        _buffer->draw_layout(get_style()->get_text_gc(Gtk::STATE_NORMAL),
                             i - tw/2, gheight + 2, l);
      }
    }
  }

  // draw graph
  x= _width-2;
  for (std::list<PointData>::iterator i= _data.begin(); i != _data.end(); ++i)
  {
    double xdelta;

    xdelta= (prev_time ? (i->timestamp - prev_time) : 0.0);

    x = x + xdelta * _pixels_per_second;
    y= gheight - 2 - ((i->value - _range_min) * (gheight-3)) / (_range_max - _range_min);

    if (y < 1)
      y= 1;
    if (x < 0)
    {
      // interpolate
      // XXX
      // crop remaining values from the list
      while (i != _data.end())
      {
        std::list<PointData>::iterator next= i;
        ++next;
        _data.erase(i);
        i= next;
      }
      break;
    }
    points.push_back(Gdk::Point((int)x, (int)y));
    prev_time= i->timestamp;
  }
  if (x > 0)
  {
    // start at 0
    x -= 1;
    y = gheight-2;
    points.push_back(Gdk::Point((int)x, (int)y));
  }

  _buffer->draw_lines(_line_gc, points);
  
  // draw caption
  if (!_caption.empty())
  {
    Glib::RefPtr<Pango::Layout> l= create_pango_layout(_caption);
    int tw, th;

    l->get_pixel_size(tw, th);

    _buffer->draw_layout(get_style()->get_white_gc(),
                         5, _height - th - 2, l);
  }

  refresh();
}


void MGTimeGraphPlotter::get_stats(double &min, double &max, double &avg)
{
  int count= 0;
  avg= 0;
  min= 0;
  max= 0;
  for (std::list<PointData>::iterator i= _data.begin(); i != _data.end(); ++i)
  {
    avg+= i->value;
    min= std::min(min, i->value);
    max= std::max(max, i->value);
    count++;
  }
  avg= avg/count;
}
