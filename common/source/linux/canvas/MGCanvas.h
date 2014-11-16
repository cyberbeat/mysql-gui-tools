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


#ifndef _MGCANVAS_H_
#define _MGCANVAS_H_

#include <gtkmm/layout.h>
#include <gtkmm/drawingarea.h>
#include <gdkmm/cursor.h>
#include <GL/gl.h>
#include <GL/glx.h>


#include <vector>

#include "myx_gc_canvas.h"


class MGCanvasScroller;

class MGCanvas : public Gtk::DrawingArea, CGCListenerInterface {
    typedef Gtk::DrawingArea superclass;
  public:
    struct Point {
      float x, y;
      Point() : x(0), y(0) {};
      Point(float ix, float iy) : x(ix), y(iy) {};
    };
    struct Size {
      float width, height;
      Size() : width(0), height(0) {};
      Size(float w, float h) : width(w), height(h) {};
    };
    struct Rectangle {
      Point origin;
      Size size;
      Rectangle(float x, float y, float w, float h) : origin(x,y), size(w,h) {};
      inline float get_x() { return origin.x; };
      inline float get_y() { return origin.y; };
      inline float get_width() { return size.width; };
      inline float get_height() { return size.height; };
    };
        
  protected:
    GLXContext _context;
    
    CGenericCanvas *_canvas;
    
    Size _baseSize;
    Point _offset;
    
    Gdk::Point _lastClickPoint;
    
    Gdk::Point _lastPressPoint;
    int _lastPressTime;

    Gdk::Rectangle _viewport;
    
    Gdk::Cursor _hand_cursor;
    Gdk::Cursor _hand_closed_cursor;
    
    double _zoom;
    int _currentZoomStep;
    
    bool _grabPanning;
    bool _changingScrollers;

    bool _refreshPending;

    Glib::Dispatcher _refresh;
    Glib::Dispatcher _switchView;
    
    struct {
      unsigned panning:2;
    } _state;
    
    sigc::slot<bool,MGCanvas*,int,int,Point> _handleMouseDown;
    sigc::slot<bool,MGCanvas*,int,int,Point> _handleMouseUp;
    sigc::slot<bool,MGCanvas*,int,Point> _handleMouseDrag;
    sigc::slot<void,CGCBase*,TAction*> _handleAction;

    virtual void on_realize();
    virtual bool on_expose_event(GdkEventExpose* event);
    virtual bool on_configure_event(GdkEventConfigure* event);

    virtual bool on_button_press_event(GdkEventButton* event);
    virtual bool on_button_release_event(GdkEventButton* event);
    virtual bool on_scroll_event(GdkEventScroll* event);
    virtual bool on_motion_notify_event(GdkEventMotion* event);
    
    virtual bool on_focus_in_event(GdkEventFocus* event);
    virtual bool on_focus_out_event(GdkEventFocus* event);
    
    virtual void onAction(CGCBase *sender, CGCBase *origin, TAction **action);
    virtual void onChange(CGCBase *sender, CGCBase *origin, TGCChangeReason Reason);
    virtual void onError(CGCBase *sender, CGCBase *origin, const char* Message);
    virtual void onDestroy(CGCBase *sender);

    bool pulse();
    void handle_switch_view();
    
    Point get_center_for_zoom();

    MGCanvasScroller *get_enclosing_scroller();
    
  public:
    MGCanvas();
    virtual ~MGCanvas();
    
    void set_hand_cursor(const Gdk::Cursor &hand, const Gdk::Cursor &closed_hand);
   
    void set_zoom(float zoom, Point center);
    void set_zoom_level(int level, Point center);
    void set_zoom_level(int level=-1);
    int zoom_level() { return _currentZoomStep; };
    
    void toggle_overview();
    
    void set_offset(Point offset);
    void set_base_size(Size size);
    Size base_size() { return _baseSize; };
    
    Point convert_to_canvas_point(Gdk::Point point);
    
    Size actual_size();
    Rectangle actual_visible_rect();
    
    bool load_layouts(const Glib::ustring &file);
    bool load_styles(const Glib::ustring &file, 
                     std::map<std::string,std::string> &variables);
    
    void render_scene();
    void update_from_scrollers();
    void update_scrollers();
    
    void force_reconfigure();
    
    void set_grab_panning(bool flag);
    
    void create_view(const Glib::ustring &name);
    void create_view(CGCView *view);
    void switch_to_view(CGCView *view);
    
    void set_grid_enabled(bool flag);
    
    void start_area_selection_at(Point point);
    Rectangle finish_area_selection();

    void set_mouse_down_handler(sigc::slot<bool,MGCanvas*,int,int,Point> handler);
    void set_mouse_up_handler(sigc::slot<bool,MGCanvas*,int,int,Point> handler);
    void set_mouse_move_handler(sigc::slot<bool,MGCanvas*,int,Point> handler);
    void set_action_handler(sigc::slot<void,CGCBase*,TAction*> handler);

    CGenericCanvas *canvas() { return _canvas; };

    std::vector<float> get_zoom_steps();
};

#endif /* _MGCANVAS_H_ */
