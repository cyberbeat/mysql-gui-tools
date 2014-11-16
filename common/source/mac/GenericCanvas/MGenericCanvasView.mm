//
//  MGenericCanvasView.mm
//  GenericCanvas
//
//  Created by Alfredo Kojima on 05/6/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import "MCanvasScrollView.h"
#import "MGenericCanvasView.h"
#include "myx_gc.h"
#include "myx_gc_canvas.h"


static TGCViewport viewportFromRect(NSRect rect)
{
  TGCViewport viewport;
  
  viewport.top= (int)NSMinY(rect);
  viewport.left= (int)NSMinX(rect);
  viewport.width= (int)NSWidth(rect);
  viewport.height= (int)NSHeight(rect);
  
  return viewport;
}


@interface MGenericCanvasView(Private)
- (void)canvasDidChange:(CGCBase*)origin reason:(TGCChangeReason)reason;
- (void)canvasDidDestroy;
- (void)canvasError:(CGCBase*)origin message:(NSString*)message;
@end


@implementation MGenericCanvasView(Private)

- (void)handleSwitchView:(id)dummy
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
  [self updateScrollers];
}

- (void)canvasDidChange:(CGCBase*)origin reason:(TGCChangeReason)reason
{
  switch (reason)
  {
    case GC_CHANGE_VIEW_ZOOM:
      _zoom= _canvas->currentViewGet()->zoomGet();
      [(MCanvasScrollView*)[self enclosingScrollView] reflectContentRect];
      break;
    case GC_CHANGE_VIEW_OFFSET:
      _offset.x= -_canvas->currentViewGet()->offsetXGet();
      _offset.y= -_canvas->currentViewGet()->offsetYGet();
      [(MCanvasScrollView*)[self enclosingScrollView] reflectContentRect];
      break;
    case GC_CHANGE_CANVAS_REFRESH:
    case GC_CHANGE_VIEW_PROPERTY:
      [self setNeedsDisplay:YES];
      break;
    case GC_CHANGE_CANVAS_SWITCH_VIEW:
      [self performSelectorOnMainThread:@selector(handleSwitchView:)
                             withObject:nil
                          waitUntilDone:NO];
      break;
  }
  /*
  [_delegate performSelectorOnMainThread:@selector(canvas:handleChange:)
                              withObject:self
                              withObject:(int)reason
                           waitUntilDone:NO];
   */
  [_delegate canvas:self handleChange:reason];
}

- (void)canvasDidDestroy
{
}

- (void)canvasError:(CGCBase*)origin message:(NSString*)message
{
  NSLog(@"CANVAS ERROR: %@", message);
}
@end


class MGCVListener : public CGCBaseListener
{
  MGenericCanvasView *_owner;

  virtual void onAction(CGCBase* sender, CGCBase* origin, TActionType &actionType)
  {
  }
  
  virtual void onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
  {
    [_owner canvasDidChange:origin reason:reason];
  }
  
  virtual void onDestroy(CGCBase* object)
  {
    [_owner canvasDidDestroy];
  }
  
  virtual void onError(CGCBase* sender, CGCBase* origin, const char* message)
  {
    [_owner canvasError:origin message:[NSString stringWithUTF8String:message]];
  }
  
public:
    MGCVListener(MGenericCanvasView *canvas) : _owner(canvas) {};
};


@implementation MGenericCanvasView

/*static float zoomSteps[]= {
  0.6125 / 12, 
  0.675 / 12,
  0.75 / 12,
  1.0 / 12,
  2.0 / 12, 
  3.0 / 12,
  4.0 / 12,
  6.0 / 12,
  8.0 / 12,
  1, 
  2, 3, 4, 5, 6, 7, 8, 12, 16
};*/

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

#define ZOOM_LEVEL_1 DEFAULT_CANVAS_ZOOM_LEVEL

#define MAX_ZOOM_STEPS (sizeof(ZoomSteps)/sizeof(float))

static int getModifiers(NSEvent *event)
{
  int mods= [event modifierFlags];
  int res= GC_MODIFIER_NONE;
  
  if (mods & NSShiftKeyMask)
    res|= GC_MODIFIER_SHIFT;
  if (mods & NSCommandKeyMask)
    res|= GC_MODIFIER_ALT;
  if (mods & NSControlKeyMask)
    res|= GC_MODIFIER_CONTROL;
  if (mods & NSAlternateKeyMask)
    res|= GC_MODIFIER_OPTION;
  
  return res;
}

- (NSArray*)getZoomSteps
{
  unsigned int i;
  NSMutableArray *array= [NSMutableArray arrayWithCapacity:MAX_ZOOM_STEPS];
  for (i= 0; i < MAX_ZOOM_STEPS; i++)
  {
    [array addObject:[NSString stringWithFormat:@"%.0f %%", 100*ZoomSteps[i]]];
  }
  return array;
}


- (void)setZoomLevel:(int)level withCenter:(NSPoint)center
{  
  if (level < 0)
    level= 0;
  else if (level >= MAX_ZOOM_STEPS)
    level= MAX_ZOOM_STEPS-1;
  
  _currentZoomStep= level;

  [self setZoom:ZOOM_STEP(level)
     withCenter:center];
}


- (void)setZoomLevel:(int)level
{
  if (level < 0)
    level= 0;
  else if (level >= MAX_ZOOM_STEPS)
    level= MAX_ZOOM_STEPS-1;
  
  _currentZoomStep= level;
  
  [self setZoom:ZOOM_STEP(level)];
}


- (int)zoomLevel
{
  return _currentZoomStep;
}


- (void)setZoom:(float)zoom
{
  _zoom= zoom;
  
  _canvas->beginUpdate();
  if (_canvas->currentViewGet())
  {
    _canvas->currentViewGet()->zoomSet(zoom);
  }
  _canvas->endUpdate();
  
  [self updateScrollers];  
}


- (void)setZoom:(float)zoom withCenter:(NSPoint)center
{
//  NSPoint canvasPt;

  if (_autoCenterZoom)
  {
//    _offset.x= NSWidth([self frame]) / 2 - canvasPt.x;
//    _offset.y= NSHeight([self frame]) / 2 - canvasPt.y;
  }
  else
  {
    _offset.x= center.x/zoom*_zoom + NSWidth([self frame]) / _zoom/2 - NSWidth([self frame]) / zoom/2;
    _offset.y= center.y/zoom*_zoom + NSHeight([self frame]) / _zoom/2 - NSHeight([self frame]) / zoom/2;
  }
  
  _offset.x= MAX(floor(_offset.x), 0);
  _offset.y= MAX(floor(_offset.y), 0);

  _zoom= zoom;
  
  _canvas->beginUpdate();
  if (_canvas->currentViewGet())
  {
    _canvas->currentViewGet()->zoomSet(zoom);
  }
  _canvas->endUpdate();
  
  [self updateScrollers];
}


- (void)toggleOverview:(id)sender
{
  if (_canvas->currentViewGet()->overviewActive())
    _canvas->currentViewGet()->overviewStop(true, true);
  else
    _canvas->currentViewGet()->overviewStart(true);
  
//  [(MCanvasScrollView*)[self enclosingScrollView] reflectContentRect];
}


- (BOOL)isFlipped
{
  return YES;
}


- (id)initWithFrame:(NSRect)frame
{  
#if 0
  GLuint attribs[] = 
	{
		NSOpenGLPFANoRecovery,
		NSOpenGLPFAWindow,
		NSOpenGLPFAAccelerated,
		NSOpenGLPFADoubleBuffer,
		NSOpenGLPFAColorSize, 24,
		NSOpenGLPFAAlphaSize, 8,
		NSOpenGLPFADepthSize, 32,
		NSOpenGLPFAStencilSize, 8,
		NSOpenGLPFAAccumSize, 0,
		0
	};
	NSOpenGLPixelFormat* fmt= [[NSOpenGLPixelFormat alloc] initWithAttributes:(NSOpenGLPixelFormatAttribute*)attribs]; 
	
	if (!fmt)
		NSLog(@"No OpenGL pixel format");
  
  self= [super initWithFrame:frame pixelFormat:[fmt autorelease]];
#else
  self= [super initWithFrame:frame pixelFormat:[NSOpenGLView defaultPixelFormat]];
#endif
  if (self)
  {
    _canvas= new CGenericCanvas((GCContext)[[self openGLContext] CGLContextObj], 
                                utf8ToUtf16(""));

    _canvas->beginUpdate();

    _canvas->setTexturePath([[[NSBundle bundleForClass:[MGenericCanvasView class]] pathForResource:@"images"
                                                                                            ofType:nil] fileSystemRepresentation]);

    _listener= new MGCVListener(self);
    _canvas->addListener(_listener);
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(frameDidChange:)
                                                 name:NSViewFrameDidChangeNotification
                                               object:self];
    [self setPostsFrameChangedNotifications:YES];
    [[self window] setAcceptsMouseMovedEvents:YES];
    
    _currentZoomStep= ZOOM_LEVEL_1;
    _zoom= ZOOM_STEP(_currentZoomStep);
    _autoScrollInterval= 1;
    _autoScrollDelay= 1.0;
    
    _canvas->endUpdate();
    
    [self updateScrollers];
    
    //[self setFocusRingType:NSFocusRingTypeExterior];

    [[self openGLContext] makeCurrentContext];
    
    _timer= [NSTimer scheduledTimerWithTimeInterval:0.01
                                     target:self
                                   selector:@selector(timerTick:)
                                   userInfo:nil
                                    repeats:YES];
  }
  return self;
}


- (void)timerTick:(NSTimer*)timer
{
  _canvas->animationManager()->pulse();
}


- (void)dealloc
{
  [[NSNotificationCenter defaultCenter] removeObserver:self];
  
  [_timer invalidate];
  [_lastEvent release];
  
  if (_canvas)
  {
    [[self openGLContext] makeCurrentContext];
    _canvas->removeListener(_listener);
    delete _canvas;
  }  
  delete _listener;
  
  [super dealloc];
}

- (void)setDelegate:(id)delegate
{
  _delegate= delegate;
}

- (void)setBaseSize:(NSSize)size
{
  if (!NSEqualSizes(_baseSize, size))
  {
    _baseSize= size;
    
    [self updateScrollers];
  }
}

- (NSSize)baseSize
{
  return _baseSize;
}


- (void)setOffset:(NSPoint)offset
{
//  if (!NSEqualPoints(_offset, offset))
  {
    _offset= offset;
    
    [self updateScrollers];
  }
}


- (NSPoint)convertToCanvasPoint:(NSPoint)point
{
  TVertex vert;
  
  _canvas->currentViewGet()->windowToView((int)point.x, (int)point.y, vert);
  
  return NSMakePoint(vert.x, vert.y);
}


- (NSPoint)convertFromCanvasPoint:(NSPoint)point
{
  TVertex vert(point.x, point.y, 0.0, 0.0);
  int x, y;
  
  _canvas->currentViewGet()->viewToWindow(vert, x, y);

  return NSMakePoint(x, y);
}


- (BOOL)acceptsFirstResponder 
{ 
  return YES; 
} 

- (BOOL)becomeFirstResponder 
{ 
  [[self window] setAcceptsMouseMovedEvents: YES];
  _canvas->focusedSet(true);
  return YES; 
} 

- (BOOL)resignFirstResponder 
{ 
  _canvas->focusedSet(false);
  [[self window] setAcceptsMouseMovedEvents: NO]; 
  return YES; 
} 


- (void)keyDown:(NSEvent*)event
{
  [super keyDown:event];
}


- (void)mouseDown:(NSEvent*)event
{
  NSPoint point= [self convertPoint:[event locationInWindow]
                           fromView:nil];
  BOOL doubleClick= NO;
  
  [_lastEvent release];
  _lastEvent= [event retain];
  switch ([event type])
  {
    case NSLeftMouseDown:
    {
      if (fabs(_lastClickPoint.x - point.x) < 3 && fabs(_lastClickPoint.y - point.y) < 3
          &&  [event timestamp] - _lastClickTime < 0.5)
      {
        doubleClick= YES;
        _lastClickTime= 0.0;
      }
      else
        _lastClickTime= [event timestamp];
      _lastClickPoint= point;
      // panning
      if (_grabPanning
          || (getModifiers(event) & NSCommandKeyMask) == NSCommandKeyMask)
      {
        _state.panning= 1;
        [[NSCursor closedHandCursor] push];
      }
      else
      {
        if (![_delegate canvas:self handleMouseDown:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
          _canvas->currentViewGet()->handleMouseInput(doubleClick?GC_MOUSE_DBL_CLICK:GC_MOUSE_DOWN,
                                                      GC_MOUSE_BUTTON_LEFT,getModifiers(event),
                                                      (int)point.x, (int)point.y);
      }
      break;
    }
    case NSOtherMouseDown:
      if (![_delegate canvas:self handleMouseDown:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
        _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_DOWN,GC_MOUSE_BUTTON_MIDDLE,getModifiers(event),
                                                 (int)point.x, (int)point.y);      
      break;
      
    case NSRightMouseDown:
      if (![_delegate canvas:self handleMouseDown:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
        _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_DOWN,GC_MOUSE_BUTTON_RIGHT,getModifiers(event),
                                                 (int)point.x, (int)point.y);      
      break;
  }
}




- (void)mouseUp:(NSEvent*)event
{
  NSPoint point= [self convertPoint:[event locationInWindow]
                           fromView:nil];
  
  [_lastEvent release];
  _lastEvent= [event retain];

  switch ([event type])
  {
    case NSLeftMouseUp:
      if (_state.panning)
      {
        _state.panning= 0;
        [NSCursor pop];
      }
      else
      {
        if (![_delegate canvas:self handleMouseUp:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
          _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,GC_MOUSE_BUTTON_LEFT,getModifiers(event),
                                                   (int)point.x, (int)point.y);        
      }
      break;
    case NSOtherMouseUp:
      if (![_delegate canvas:self handleMouseUp:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
        _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,GC_MOUSE_BUTTON_MIDDLE,getModifiers(event),
                                                 (int)point.x, (int)point.y);
      break;
    case NSRightMouseUp:
      if (![_delegate canvas:self handleMouseUp:[event buttonNumber] atPoint:[self convertToCanvasPoint:point] modifiers:getModifiers(event)])
        _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_UP,GC_MOUSE_BUTTON_RIGHT,getModifiers(event),
                                                 (int)point.x, (int)point.y);
      break;
  }
}



- (void)scrollWheel:(NSEvent*)theEvent
{
  if (([theEvent modifierFlags] & NSCommandKeyMask) == NSCommandKeyMask)
  {
    int steps= abs((int)([theEvent deltaY] / 4));
    NSPoint center= [self convertPoint:[theEvent locationInWindow]
                             fromView:nil];
    
    if (steps == 0)
      steps= 1;
    
    center.x*= _zoom;
    center.y*= _zoom;
    
    center.x+= _offset.x;
    center.y+= _offset.y;
    
    if ([theEvent deltaY] < 0)
    {
      [self setZoomLevel:_currentZoomStep-steps
              withCenter:center];
    }
    else
    {
      [self setZoomLevel:_currentZoomStep+steps
              withCenter:center];
    }
  }
  else
  {
    _offset.x -= [theEvent deltaX] *3;
    _offset.y -= [theEvent deltaY] *3;
    
    [self updateScrollers];
  }
}



- (void)mouseMoved:(NSEvent*)event
{
  [self mouseDragged:event];
}


- (void)mouseDragged:(NSEvent*)event
{
  NSPoint pos= [self convertPoint:[event locationInWindow] fromView:nil];
  
  if (_state.panning)
  {
    NSPoint delta;
    
    delta.x= _lastClickPoint.x - pos.x;
    delta.y= _lastClickPoint.y - pos.y;
    
    _lastClickPoint= pos;
    
    _offset.x+= delta.x;
    _offset.y+= delta.y;

    [self updateScrollers];
  }
  else
  {
    if (![_delegate canvas:self handleMouseDrag:[self convertToCanvasPoint:pos] modifiers:getModifiers(event)])
      _canvas->currentViewGet()->handleMouseInput(GC_MOUSE_MOVE, GC_MOUSE_BUTTON_NONE,
                                               getModifiers(event),
                                               (int)pos.x, (int)pos.y);
  }
}


- (void)drawRect:(NSRect)rect
{
  [[self openGLContext] makeCurrentContext];

  [self renderScene]; 

  glFlush();
  [[self openGLContext] flushBuffer];
}


- (void)frameDidChange:(NSNotification*)notif
{
  NSRect frame= [self frame];
  
  if (_canvas->currentViewGet())
  {
    _viewport.size= frame.size;
    
    _canvas->beginUpdate();
    _canvas->currentViewGet()->viewportSet(viewportFromRect(_viewport));
    _canvas->endUpdate();
//    [[self openGLContext] update];
  }
  [self updateScrollers];
}


- (BOOL)canvasFits
{
  NSRect frame= [self frame];
  
  return ((_zoom * _baseSize.width) > NSWidth(frame)) || ((_zoom * _baseSize.height) > NSHeight(frame));
}




- (void)zoomInAt:(NSPoint)center
{
  if (_currentZoomStep < MAX_ZOOM_STEPS)
  {
    _currentZoomStep++;
    
    [self setZoom:ZOOM_STEP(_currentZoomStep)
       withCenter:center];
  }
}


- (void)zoomOutAt:(NSPoint)center
{
  if (_currentZoomStep > 0)
  {
    _currentZoomStep--;
    [self setZoom:ZOOM_STEP(_currentZoomStep)
       withCenter:center];
  }
}


- (BOOL)loadLayoutsFromFile:(NSString*)file
{
  TGCError error= _canvas->addLayoutsFromFile([file fileSystemRepresentation]);
  
  return error == GC_NO_ERROR;
}

- (BOOL)loadStylesFromFile:(NSString*)file withVariables:(NSDictionary*)variables
{
  map<string,string> vars;
  
  if (variables)
  {
    NSEnumerator *enumer= [variables keyEnumerator];
    NSString *key;
    while ((key= [enumer nextObject]))
    {
      vars[[key UTF8String]]= [[variables objectForKey:key] UTF8String];
    }
  }
  
  TGCError error= _canvas->addStylesFromFile([file fileSystemRepresentation], vars);
  
  return error == GC_NO_ERROR;
}


- (void)renderScene
{
  struct timeval tm1, tm2;
  gettimeofday(&tm1, NULL);
  _canvas->render(TGCRenderContent(GC_RENDER_GRID|GC_RENDER_CONNECTIONS|GC_RENDER_FEEDBACK|GC_RENDER_FRAME|GC_RENDER_FOCUS));
  gettimeofday(&tm2, NULL);
  
  _lastFPS= 1 / ((tm2.tv_sec - tm1.tv_sec) + (tm2.tv_usec - tm1.tv_usec) / 1000000.0);

//  NSLog(@"%.2f FPS", _lastFPS);
}


- (void)setCursor:(NSCursor*)cursor
{
  if (_cursor != cursor)
  {
    [_cursor release];
    _cursor= [cursor retain];
    [[self window] resetCursorRects];
  }
}


- (void)resetCursorRects
{
  [super resetCursorRects];
  if (_cursor)
  {
    [self addCursorRect:[self bounds] cursor:_cursor];
    [_cursor setOnMouseEntered:YES];
  }
}


- (void)setGrabPanning:(BOOL)flag
{
  _grabPanning= flag;
}


- (void)setGridEnabled:(BOOL)flag
{
  _canvas->currentViewGet()->grid()->visibleSet(flag);
  [self setNeedsDisplay:YES];
}


- (BOOL)gridEnabled
{
  return _canvas->currentViewGet()->grid()->visibleGet();
}


- (NSSize)actualSize
{
  NSSize actual;
  
  actual.width= _zoom * _baseSize.width;
  actual.height= _zoom * _baseSize.height;  
  
  return actual;
}


- (NSRect)actualVisibleRect
{
  NSRect frame= [self frame];
  
  frame.origin.x= _offset.x;
  frame.origin.y= _offset.y;
  frame.size.width/= _zoom;
  frame.size.height/= _zoom;
  
  return frame;
}


- (MCanvasScrollView*)enclosingScrollView
{
  return (MCanvasScrollView*)[self superview];
}


- (void)updateFromScrollers
{
  NSScroller *horiz= [[self enclosingScrollView] horizontalScroller];
  NSScroller *vert= [[self enclosingScrollView] verticalScroller];
  NSRect visibleRect= [self actualVisibleRect];
  
  _offset.x= [horiz floatValue] * (_baseSize.width - NSWidth(visibleRect)) * _zoom;
  _offset.y= [vert floatValue] * (_baseSize.height - NSHeight(visibleRect)) * _zoom;
  
  _canvas->beginUpdate();
  _canvas->currentViewGet()->offsetXSet(-_offset.x);
  _canvas->currentViewGet()->offsetYSet(-_offset.y);
  _canvas->endUpdate();
}


- (void)updateScrollers
{
  NSRect frame= [self frame];
  
  _canvas->beginUpdate();
  if (_baseSize.width > NSWidth(frame))
  {
    _offset.x = MAX(MIN(_offset.x, _baseSize.width - NSWidth(frame)), 0);
  }
  else
    _offset.x = 0;
  
  if (_baseSize.height > NSHeight(frame))
  {
    _offset.y = MAX(MIN(_offset.y, _baseSize.height - NSHeight(frame)), 0);
  }
  else
    _offset.y = 0;
  
  if (_canvas->currentViewGet())
  {
    _canvas->currentViewGet()->offsetXSet(-_offset.x);
    _canvas->currentViewGet()->offsetYSet(-_offset.y);
    
    [[self enclosingScrollView] reflectContentRect];
  }
  _canvas->endUpdate();
}

- (void)createCanvasView:(NSString*)viewName
{
  NSRect frame= [self frame];
  CGCView *view;
  
  _canvas->beginUpdate();
  
  view= _canvas->createView([viewName UTF8String]);
//  _baseSize= frame.size;
  
  view->color(1.0, 1.0, 1.0, 1.0);

  _viewport.origin= NSMakePoint(0,0);
  _viewport.size= frame.size;
  view->viewportSet(viewportFromRect(_viewport));
  
  view->workspaceSet(_baseSize.width, _baseSize.height);

  _canvas->endUpdate();
  
  [self setZoomLevel:ZOOM_LEVEL_1];
}


- (void)createCanvasViewWithView:(CGCView*)view
{
  NSRect frame= [self frame];
  
  _canvas->beginUpdate();
  
//  _baseSize= frame.size;
  
  view->color(1.0, 1.0, 1.0, 1.0);
  
  _viewport.origin= NSMakePoint(0,0);
  _viewport.size= frame.size;
  view->viewportSet(viewportFromRect(_viewport));

  view->workspaceSet(_baseSize.width, _baseSize.height);
  
  _canvas->endUpdate();
  
  [self setZoomLevel:ZOOM_LEVEL_1];
}


- (void)switchToCanvasView:(CGCView*)view
{  
  _canvas->beginUpdate();
  _canvas->currentViewSet(view);
  _canvas->endUpdate();
}


- (CGenericCanvas*)canvas
{
  return _canvas;
}


- (void)startAreaSelectionAt:(NSPoint)point
{
  TVertex v;
  
  v.x= point.x;
  v.y= point.y;
  v.z= 0;
  v.w= 0;

  _canvas->currentViewGet()->rubberRectStart(GC_RRSTYLE_BLENDED_DIAGONALS,
                                             v, false);
}


- (NSRect)finishAreaSelection
{
  TBoundingBox bounds;
  _canvas->currentViewGet()->getLastRubberBounds(bounds);
  _canvas->currentViewGet()->rubberRectStop();
  
  return NSMakeRect(bounds.upper.x, bounds.upper.y,
                    bounds.lower.x - bounds.upper.x, bounds.lower.y - bounds.upper.y);
}


- (NSEvent*)lastEvent
{
  return _lastEvent;
}



static struct {
  const char *name;
  SEL colorSelector;
  unsigned char color[3];
} colors[]= {
//  {"Highlight", @selector(selectedControlColor), {0, 0, 0}},
  {NULL}
};


+ (void)registerSystemColors
{
  CColorMap colormap;
  
  for (unsigned int i= 0; colors[i].name; i++)
  {
    if (colors[i].colorSelector)
    {
      NSColor *c= [NSColor performSelector:colors[i].colorSelector];
      c= [c colorUsingColorSpaceName:NSDeviceRGBColorSpace];
      colors[i].color[0]= (unsigned char)([c redComponent] * 255);
      colors[i].color[1]= (unsigned char)([c greenComponent] * 255);
      colors[i].color[2]= (unsigned char)([c blueComponent] * 255);
    }
    colormap[colors[i].name]= colors[i].color;
  }

  

  registerSystemColors(colormap);
}


@end




