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

#include "myx_gc_figure.h"

#include <gdkmm.h>
#include <gdk/gdkx.h>
#include <sys/time.h>
#include <gtkmm/adjustment.h>
#include "MGCanvasScroller.h"

static float ZoomSteps[]= {
  0.1,
  0.2,
  0.3,
  0.4,
  0.5,
  0.6,
  0.65,
  0.7,
  0.75,
  0.8,
  0.85,
  0.9,
  1.0,
  1.1,
  1.2,
  1.5,
  2.0
};

#define ZOOM_STEP(s) (ZoomSteps[s]/1.45)

#define ZOOM_LEVEL_1 12

#define MAX_ZOOM_STEPS (sizeof(ZoomSteps)/sizeof(float))


// scroller support
// correct zooming
// font and sys-color support


static int getModifiers(int mods)
{
  int res= GC_MODIFIER_NONE;
  
  if (mods & GDK_SHIFT_MASK)
    res|= GC_MODIFIER_SHIFT;
  if (mods & GDK_CONTROL_MASK)
    res|= GC_MODIFIER_CONTROL;
  if (mods & GDK_MOD1_MASK)
    res|= GC_MODIFIER_ALT;

  return res;
}



static TGCViewport viewportFromRect(Gdk::Rectangle rect)
{
  TGCViewport viewport;

  viewport.top= rect.get_y();
  viewport.left= rect.get_x();
  viewport.width= rect.get_width();
  viewport.height= rect.get_height();

  return viewport;
}


MGCanvas::MGCanvas()
  : superclass(), _context(0), _canvas(0)
{
  set_flags(get_flags()|Gtk::CAN_FOCUS);

  add_events(Gdk::BUTTON_PRESS_MASK|Gdk::BUTTON_RELEASE_MASK|Gdk::BUTTON_MOTION_MASK|
             Gdk::KEY_PRESS_MASK|Gdk::KEY_RELEASE_MASK|Gdk::POINTER_MOTION_MASK);
  
  set_double_buffered(false);

  _zoom= 1.0;
  _grabPanning= false;
  _state.panning= 0;
  _changingScrollers= false;
  _refreshPending= false;
  
  _refresh.connect(sigc::mem_fun(*this, &Gtk::Widget::queue_draw));
  _switchView.connect(sigc::mem_fun(*this, &MGCanvas::handle_switch_view));
}


MGCanvas::~MGCanvas()
{ 
  if (_context)
    glXDestroyContext(gdk_display, _context);
  
  delete _canvas;
}


void MGCanvas::handle_switch_view()
{
  _canvas->beginUpdate();
  if (_canvas->currentViewGet())
  {
    _canvas->currentViewGet()->viewportSet(viewportFromRect(_viewport));
    _offset.x= _canvas->currentViewGet()->offsetXGet();
    _offset.y= _canvas->currentViewGet()->offsetYGet();
    _zoom= _canvas->currentViewGet()->zoomGet();
  }
  _canvas->endUpdate();
  update_scrollers();
}


void MGCanvas::set_mouse_down_handler(sigc::slot<bool,MGCanvas*,int,int,Point> handler)
{
  _handleMouseDown= handler;
}


void MGCanvas::set_mouse_up_handler(sigc::slot<bool,MGCanvas*,int,int,Point> handler)
{
  _handleMouseUp= handler;
}


void MGCanvas::set_mouse_move_handler(sigc::slot<bool,MGCanvas*,int,Point> handler)
{
  _handleMouseDrag= handler;
}


void MGCanvas::set_action_handler(sigc::slot<void,CGCBase*,TAction*> handler)
{
  _handleAction= handler;
}


void MGCanvas::force_reconfigure()
{
  _canvas->beginUpdate();
  _canvas->currentViewGet()->viewportSet(viewportFromRect(_viewport));
  _canvas->endUpdate();
}


bool MGCanvas::on_configure_event(GdkEventConfigure* event)
{
  superclass::on_configure_event(event);

  if (_canvas && _canvas->currentViewGet())
  {
    CGCView *view= _canvas->currentViewGet();
    
    _viewport.set_width(event->width);
    _viewport.set_height(event->height);
    
    _canvas->beginUpdate();
    view->viewportSet(viewportFromRect(_viewport));
    _canvas->endUpdate();    

    update_scrollers();
  }
  
  return true;
}


void MGCanvas::on_realize()
{  
  superclass::on_realize();
 
  XVisualInfo *visinfo;
  int attrib[] = { 
    GLX_RGBA,
      GLX_RED_SIZE, 1,
      GLX_GREEN_SIZE, 1,
      GLX_BLUE_SIZE, 1,
      GLX_DOUBLEBUFFER,
      GLX_DEPTH_SIZE, 1,
      None 
  };

  visinfo = glXChooseVisual(gdk_display,
                            gdk_x11_screen_get_screen_number(Gdk::Screen::get_default()->gobj()),
                            attrib);
  if (!visinfo) {
    g_warning("could not get visual for OpenGL");
    return;
  }

  _context= glXCreateContext(gdk_display, visinfo, NULL, GL_TRUE);
  XSync(gdk_display, False);
  if (!_context) 
  {
    g_warning("Could not initialize GLX context");
    XFree(visinfo);
  }
  XFree(visinfo);
  
  glXMakeCurrent(gdk_display, gdk_x11_drawable_get_xid(get_window()->gobj()), _context);
  
  _canvas= new CGenericCanvas(_context, L"");
  _canvas->addListener(this);
  Glib::signal_timeout().connect(sigc::mem_fun(*this,&MGCanvas::pulse), 10);
  
  CGCView *view= _canvas->createView("main");
  _canvas->currentViewSet(view);

  view->viewportSet(TGCViewport(0, 0, 
                             get_allocation().get_width(),
                             get_allocation().get_height()));
  view->color(1.0, 1.0, 1.0, 1.0);
  
  update_scrollers();
}


bool MGCanvas::pulse()
{
  _canvas->animationManager()->pulse();
  return true;
}


bool MGCanvas::on_expose_event(GdkEventExpose* event)
{
  _refreshPending= false;
  if (event->count == 0)
    render_scene();
  return true;
}


bool MGCanvas::on_button_press_event(GdkEventButton* event)
{
  Gdk::Point point((int)event->x, (int)event->y);
  bool doubleClick= false;

  switch (event->button)
  {
  case 1:
    grab_focus();
    if (abs(_lastPressPoint.get_x()-point.get_x()) < 3 &&
        abs(_lastPressPoint.get_y()-point.get_y()) < 3 &&
        event->time - _lastPressTime < 5)
    {
      doubleClick= true;
      _lastPressTime= 0;
    }
    else
      _lastPressTime= event->time;
    _lastPressPoint= point;

    if (_grabPanning)
    {
      _lastClickPoint= point;
      _state.panning= 1;
      get_window()->set_cursor(_hand_closed_cursor);
    }
    else if (_handleMouseDown.empty()
             || !_handleMouseDown(this, event->button, getModifiers(event->state),
                                  convert_to_canvas_point(point)))
    {
      _canvas->currentViewGet()->handleMouseInput(doubleClick ? GC_MOUSE_DBL_CLICK : GC_MOUSE_DOWN,
                                                  GC_MOUSE_BUTTON_LEFT,
                                                  getModifiers(event->state),
                                                  point.get_x(),
                                                  point.get_y());
    }
    break;
    
  case 2:
    _lastClickPoint= point;
    //if (_grabPanning)
    {
      _state.panning= 2;
      get_window()->set_cursor(_hand_closed_cursor);
    }
    /*
    else if (_handleMouseDown.empty() || !_handleMouseDown(this, event->button, convert_to_canvas_point(point)))
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_DOWN,
                                               GC_MOUSE_BUTTON_MIDDLE,
                                               getModifiers(event->state),
                                               point.get_x(),
                                               point.get_y());
     */
    break;
    
  case 3:
    if (_handleMouseDown.empty()
        || !_handleMouseDown(this, event->button, getModifiers(event->state),
                             convert_to_canvas_point(point)))
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_DOWN,
                                               GC_MOUSE_BUTTON_RIGHT,
                                               getModifiers(event->state),
                                               point.get_x(),
                                               point.get_y());
    break;
  }
  
  return true;
}

  
bool MGCanvas::on_button_release_event(GdkEventButton* event)
{
  Gdk::Point point((int)event->x, (int)event->y);

  switch (event->button)
  {
  case 1:
    if (_state.panning==1)
    {
      _state.panning= 0;
      get_window()->set_cursor(_hand_cursor);
    }
    else if (_handleMouseUp.empty() 
             || !_handleMouseUp(this, event->button, getModifiers(event->state),
                                convert_to_canvas_point(point)))
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,
                                               GC_MOUSE_BUTTON_LEFT,
                                               getModifiers(event->state),
                                               point.get_x(),
                                               point.get_y());
    break;
  case 2:
    if (_state.panning==2)
    {
      _state.panning= 0;
      if (_grabPanning)
        get_window()->set_cursor(_hand_cursor);
      else
        get_window()->set_cursor();
    }
    else
    {
      if (_handleMouseUp.empty()
          || !_handleMouseUp(this, event->button, getModifiers(event->state),
                             convert_to_canvas_point(point)))
        _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,
                                                 GC_MOUSE_BUTTON_MIDDLE,
                                                 getModifiers(event->state),
                                                 point.get_x(),
                                                 point.get_y());
    }
    break;
  case 3:
    if (_handleMouseUp.empty()
        || !_handleMouseUp(this, event->button, getModifiers(event->state),
                           convert_to_canvas_point(point)))
    {
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,
                                               GC_MOUSE_BUTTON_RIGHT,
                                               getModifiers(event->state),
                                               point.get_x(),
                                               point.get_y());
    }
    break;
  }
  
  return true;
}


bool MGCanvas::on_motion_notify_event(GdkEventMotion* event)
{
  Gdk::Point pos((int)event->x, (int)event->y);
  
  if (_state.panning)
  {
    int dx, dy;
    Point pt;
    
    dx= _lastClickPoint.get_x() - pos.get_x();
    dy= _lastClickPoint.get_y() - pos.get_y();
    
    _lastClickPoint= pos;
    
    pt.x= _offset.x+dx;
    pt.y= _offset.y+dy;

    set_offset(pt);
  }
  else
  {
    if (_handleMouseDrag.empty()
        || !_handleMouseDrag(this, event->state, convert_to_canvas_point(pos)))
    {
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_MOVE,
                                               GC_MOUSE_BUTTON_NONE,
                                               getModifiers(event->state),
                                               pos.get_x(),
                                               pos.get_y());
    }
  }
  
  return true;
}


bool MGCanvas::on_scroll_event(GdkEventScroll* event)
{
  bool moved= false;
  Point pt= _offset;
  
  if ((event->state & (GDK_CONTROL_MASK|GDK_SHIFT_MASK|GDK_MOD1_MASK))== 0)
  {
    switch (event->direction)
    {
    case GDK_SCROLL_UP:
      pt.y-= 10.0 / _zoom;
      moved= true;
      break;
    case GDK_SCROLL_DOWN:
      pt.y+= 10.0 / _zoom;
      moved= true;
      break;
    case GDK_SCROLL_LEFT:
      pt.x-= 10.0 / _zoom;
      moved= true;
      break;
    case GDK_SCROLL_RIGHT:
      pt.x+= 10.0 / _zoom;
      moved= true;
      break;
    }
  }
  else if ((event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK)
  {
    if (event->direction == GDK_SCROLL_UP)
    {
      pt.x-= 10.0;
      moved= true;
    }
    else if (event->direction == GDK_SCROLL_DOWN)
    {
      pt.x+= 10.0;
      moved= true;
    }
  }
  /*
  else if ((event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK)
  {
    double zoom_increment= 0.2;
    double swidth= get_allocation().get_width();
    double sheight= get_allocation().get_height();

    //calc stuff and do here

    if (event->direction == GDK_SCROLL_UP)
    {
      zoom+= zoom_increment;
      
      moved= true;
    }
    else if (event->direction == GDK_SCROLL_DOWN)
    {
      zoom-= zoom_increment;

      moved= true;
    }
    
    if (moved)
    {
      pt.x += (event->x/swidth)*zoom;
      pt.y += (event->y/sheight)*zoom;
    }
    
    set_zoom(zoom, pt);
  }
   */

  if (moved)
  {
    if (pt.x < 0) pt.x= 0;
    if (pt.y < 0) pt.y= 0;
    set_offset(pt);
  }

  return true;
}


void MGCanvas::onAction(CGCBase *sender, CGCBase *origin, TAction **action)
{
  if (_handleAction)
    _handleAction(sender, *action);
}


void MGCanvas::onChange(CGCBase *sender, CGCBase *origin, TGCChangeReason Reason)
{
  switch (Reason)
  {
  case GC_CHANGE_VIEW_ZOOM:
    _zoom= dynamic_cast<CGCView*>(origin)->zoomGet();
    update_scrollers();
    break;
  case GC_CHANGE_VIEW_OFFSET:
    update_scrollers();
    break;
  case GC_CHANGE_CANVAS_REFRESH:
  case GC_CHANGE_VIEW_PROPERTY:
    if (!_refreshPending)
    {
      _refreshPending= true;
      _refresh.emit();
    }
    break;
  case GC_CHANGE_CANVAS_SWITCH_VIEW:
    _switchView.emit();
    break;
    
  default:
    break;
  }
}


void MGCanvas::onError(CGCBase *sender, CGCBase *origin, const char* Message)
{
  g_warning("CANVAS ERROR: %s", Message);
}


void MGCanvas::onDestroy(CGCBase *sender)
{
}


std::vector<float> MGCanvas::get_zoom_steps()
{
  std::vector<float> steps;
  for (unsigned int i= 0; i < MAX_ZOOM_STEPS; i++)
  {
    steps.push_back(ZoomSteps[i]);
  }
  return steps;
}


void MGCanvas::set_zoom_level(int level)
{
  set_zoom_level(level < 0 ? ZOOM_LEVEL_1 : level, get_center_for_zoom());
}


void MGCanvas::set_zoom_level(int level, Point center)
{
  if (level < 0)
    level= 0;
  else if (level >= (int)MAX_ZOOM_STEPS)
    level= MAX_ZOOM_STEPS-1;
  
  _currentZoomStep= level;
  
  set_zoom(ZOOM_STEP(_currentZoomStep), center);
}


void MGCanvas::set_zoom(float zoom, Point center)
{
  int width, height;
  
  get_window()->get_size(width, height);
  
  _offset.x= center.x * zoom / _zoom + width * _zoom/2 - width * zoom / 2;
  _offset.y= center.y * zoom / _zoom + height * _zoom/2 - height * zoom / 2;
  
  _offset.x= MAX(_offset.x, 0);
  _offset.y= MAX(_offset.y, 0);
  
  _zoom= zoom;
  _canvas->beginUpdate();
  if (_canvas->currentViewGet())
    _canvas->currentViewGet()->zoomSet(zoom);
  _canvas->endUpdate();
  
  update_scrollers();
}


void MGCanvas::toggle_overview()
{
  if (_canvas->currentViewGet()->overviewActive())
    _canvas->currentViewGet()->overviewStop(true, true);
  else
    _canvas->currentViewGet()->overviewStart(true);
}


void MGCanvas::set_base_size(Size size)
{
  if (size.width != _baseSize.width && size.height != _baseSize.height)
  {
    _baseSize= size;
    
    _canvas->currentViewGet()->workspaceSet(_baseSize.width, _baseSize.height);
    
    update_scrollers();
  }
}


bool MGCanvas::on_focus_in_event(GdkEventFocus* event)
{
  _canvas->focusedSet(true);

  return false;
}


bool MGCanvas::on_focus_out_event(GdkEventFocus* event)
{
  _canvas->focusedSet(false);
  return false;
}
    

MGCanvas::Point MGCanvas::convert_to_canvas_point(Gdk::Point point)
{
  TVertex vert;
  
  _canvas->currentViewGet()->windowToView(point.get_x(), point.get_y(), vert);
  
  return Point(vert.x, vert.y);
}


bool MGCanvas::load_layouts(const Glib::ustring &file)
{
  TGCError error= _canvas->addLayoutsFromFile(file.c_str());
  
  return error == GC_NO_ERROR;
}


bool MGCanvas::load_styles(const Glib::ustring &file, 
                           std::map<std::string,std::string> &variables)
{
  TGCError error= _canvas->addStylesFromFile(file.c_str(), variables);
  
  return error == GC_NO_ERROR;
}


void MGCanvas::render_scene()
{
  struct timeval tm1, tm2;
  double fps;
  
  glXMakeCurrent(gdk_display, gdk_x11_drawable_get_xid(get_window()->gobj()), _context);

  gettimeofday(&tm1, NULL);
  _canvas->render(TGCRenderContent(GC_RENDER_GRID|GC_RENDER_CONNECTIONS|GC_RENDER_FEEDBACK|GC_RENDER_FRAME|GC_RENDER_FOCUS));
  gettimeofday(&tm2, NULL);

  glXSwapBuffers(gdk_display, gdk_x11_drawable_get_xid(get_window()->gobj()));

  fps= 1 / ((tm2.tv_sec - tm1.tv_sec) + (tm2.tv_usec - tm1.tv_usec) / 1000000.0);
  if (getenv("CANVAS_FPS"))
    g_message("FPS= %f", fps);
}


void MGCanvas::set_grab_panning(bool flag)
{
  _grabPanning= flag;
}
  


void MGCanvas::set_grid_enabled(bool flag)
{
  _canvas->currentViewGet()->grid()->visibleSet(flag);
  queue_draw();
}


MGCanvas::Size MGCanvas::actual_size()
{
  return MGCanvas::Size(_baseSize.width / _zoom,
                        _baseSize.height / _zoom);
}


MGCanvas::Rectangle MGCanvas::actual_visible_rect()
{
  int w, h;
  get_window()->get_size(w, h);
  
  return MGCanvas::Rectangle(_offset.x, _offset.y,
                             w / _zoom, h / _zoom);
}


void MGCanvas::set_offset(Point point)
{
  _offset= point;
  _canvas->currentViewGet()->offsetXSet(-_offset.x);
  _canvas->currentViewGet()->offsetYSet(-_offset.y);
  update_scrollers();
}


void MGCanvas::update_from_scrollers()
{
  if (get_enclosing_scroller() && !_changingScrollers)
  {
    Gtk::Adjustment *hadj= get_enclosing_scroller()->get_hadjustment();
    Gtk::Adjustment *vadj= get_enclosing_scroller()->get_vadjustment();
    
    if (hadj && vadj)
    {
      float hv= hadj->get_value();
      float vv= vadj->get_value();

      _offset.x= hv * _zoom;
      _offset.y= vv * _zoom;

      if (_canvas && _canvas->currentViewGet())
      {        
        _canvas->beginUpdate();
        _canvas->currentViewGet()->offsetXSet(-_offset.x);
        _canvas->currentViewGet()->offsetYSet(-_offset.y);
        _canvas->endUpdate();
      }
    }
  }
}



MGCanvasScroller *MGCanvas::get_enclosing_scroller()
{
  return (MGCanvasScroller*)get_parent();
}


void MGCanvas::update_scrollers()
{
  _changingScrollers= true;
  if (get_enclosing_scroller())
  {
    Gtk::Adjustment *hadj= get_enclosing_scroller()->get_hadjustment();
    Gtk::Adjustment *vadj= get_enclosing_scroller()->get_vadjustment();
  
    if (hadj && vadj)
    {
      Size size;
      int w, h;
      get_window()->get_size(w, h);
      size.width= w;
      size.height= h;
      _canvas->beginUpdate();
      
      _offset.x= -_canvas->currentViewGet()->offsetXGet();
      _offset.y= -_canvas->currentViewGet()->offsetYGet();

      if (_baseSize.width > size.width)
        _offset.x= MAX(MIN(_offset.x, _baseSize.width - size.width), 0);
      else
        _offset.x= 0;
        
      if (_baseSize.height > size.height)
        _offset.y= MAX(MIN(_offset.y, _baseSize.height - size.height), 0);
      else
        _offset.y= 0;
      if (_canvas->currentViewGet())
      {
        bool vchanged= false;
        bool hchanged= false;
        
        if (hadj->get_page_size() != w/_zoom)
        {
          hadj->set_page_size(w / _zoom);
          hchanged= true;
        }
        if (vadj->get_page_size() != h/_zoom)
        {
          vadj->set_page_size(h / _zoom);
          vchanged= true;
        }
        
        if (hadj->get_upper() != _baseSize.width)
        {
          hadj->set_upper(_baseSize.width);
          hchanged= true;
        }
        if (vadj->get_upper() != _baseSize.height)
        {
          vadj->set_upper(_baseSize.height);
          vchanged= true;
        }
        if (hchanged)
          hadj->changed();
        if (vchanged)
          vadj->changed();
        hadj->set_value(_offset.x / _zoom);
        vadj->set_value(_offset.y / _zoom);
        hadj->value_changed();
        vadj->value_changed();
      }
      _canvas->endUpdate();
    }
  }
  _changingScrollers= false;
}


void MGCanvas::create_view(const Glib::ustring &name)
{
  CGCView *view;
  
  _canvas->beginUpdate();

  view= _canvas->createView(name.c_str());
//  _baseSize= frame.size;

  view->color(1.0, 1.0, 1.0, 1.0);

  int w, h;
  get_window()->get_size(w, h);
  _viewport.set_x(0);
  _viewport.set_y(0);
  _viewport.set_width(w);
  _viewport.set_height(h);
  view->viewportSet(viewportFromRect(_viewport));

  view->workspaceSet(_baseSize.width, _baseSize.height);

  _canvas->endUpdate();

  set_zoom_level(ZOOM_LEVEL_1, get_center_for_zoom());
}


MGCanvas::Point MGCanvas::get_center_for_zoom()
{
  return Point(0,0);
}


void MGCanvas::create_view(CGCView *view)
{  
  _canvas->beginUpdate();

  view->color(1.0, 1.0, 1.0, 1.0);

  _viewport.set_x(0);
  _viewport.set_y(0);
  int w, h;
  get_window()->get_size(w,h);
  _viewport.set_width(w);
  _viewport.set_height(h);
  view->viewportSet(viewportFromRect(_viewport));

  view->workspaceSet(_baseSize.width, _baseSize.height);

  set_zoom_level(ZOOM_LEVEL_1, get_center_for_zoom());
  
  _canvas->endUpdate();
}


void MGCanvas::switch_to_view(CGCView *view)
{
  _canvas->beginUpdate();
  if (view != _canvas->currentViewGet())
    _canvas->currentViewSet(view);
  _canvas->endUpdate();
}


void MGCanvas::start_area_selection_at(MGCanvas::Point point)
{
  TVertex v;

  v.x= point.x;
  v.y= point.y;
  v.z= 0;
  v.w= 0;
  _canvas->currentViewGet()->rubberRectStart(GC_RRSTYLE_SOLID_THIN,
                                             v, false);
}


MGCanvas::Rectangle MGCanvas::finish_area_selection()
{
  TBoundingBox bounds;
  _canvas->currentViewGet()->getLastRubberBounds(bounds);
  _canvas->currentViewGet()->rubberRectStop();

  return Rectangle(bounds.upper.x, bounds.upper.y,
                   bounds.lower.x - bounds.upper.x, bounds.lower.y - bounds.upper.y);
}




void MGCanvas::set_hand_cursor(const Gdk::Cursor &hand, const Gdk::Cursor &closed_hand)
{
  _hand_cursor= hand;
  _hand_closed_cursor= closed_hand;
}


