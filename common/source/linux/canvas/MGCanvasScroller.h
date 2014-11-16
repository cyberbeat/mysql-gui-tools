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

#ifndef _MGCANVASSCROLLER_H_
#define _MGCANVASSCROLLER_H_

#include <gtkmm/table.h>
#include <gtkmm/scrollbar.h>

class MGCanvas;

class MGCanvasScroller : public Gtk::Table {
    Gtk::HScrollbar _hscroll;
    Gtk::VScrollbar _vscroll;
    MGCanvas *_canvas;

    void scroller_changed();
    
  public:
    MGCanvasScroller();
    
    Gtk::Adjustment *get_hadjustment();
    Gtk::Adjustment *get_vadjustment();
    
    void add(MGCanvas *canvas);
};

#endif /* _MGCANVASSCROLLER_H_ */
