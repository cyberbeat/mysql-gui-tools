/* Copyright (C) 2005 MySQL AB

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

#include "MGCanvas.h"
#include "MGCanvasScroller.h"
/**
 * @file  MGCanvasScroller.cc
 * @brief 
 */


MGCanvasScroller::MGCanvasScroller()
  : Gtk::Table(2, 2), _canvas(0)
{
  attach(_vscroll, 1, 2, 0, 1, Gtk::FILL, Gtk::FILL|Gtk::EXPAND);
  attach(_hscroll, 0, 1, 1, 2, Gtk::FILL|Gtk::EXPAND, Gtk::FILL);
  show_all();
  
  _hscroll.get_adjustment()->signal_changed().connect(sigc::mem_fun(*this,&MGCanvasScroller::scroller_changed));
  _vscroll.get_adjustment()->signal_changed().connect(sigc::mem_fun(*this,&MGCanvasScroller::scroller_changed));
  
  _hscroll.get_adjustment()->signal_value_changed().connect(sigc::mem_fun(*this,&MGCanvasScroller::scroller_changed));
  _vscroll.get_adjustment()->signal_value_changed().connect(sigc::mem_fun(*this,&MGCanvasScroller::scroller_changed));
  
  _hscroll.get_adjustment()->set_page_increment(50.0);
  _hscroll.get_adjustment()->set_step_increment(5.0);
  
  _vscroll.get_adjustment()->set_page_increment(50.0);
  _vscroll.get_adjustment()->set_step_increment(5.0);
}
    

void MGCanvasScroller::add(MGCanvas *canvas)
{
  _canvas= canvas;
  
  attach(*_canvas, 0, 1, 0, 1, Gtk::FILL|Gtk::EXPAND, Gtk::FILL|Gtk::EXPAND);
  _canvas->show();
}


Gtk::Adjustment *MGCanvasScroller::get_hadjustment()
{
  return _hscroll.get_adjustment();
}


Gtk::Adjustment *MGCanvasScroller::get_vadjustment()
{
  return _vscroll.get_adjustment();
}


void MGCanvasScroller::scroller_changed()
{
  if (_canvas)
    _canvas->update_from_scrollers();
}

