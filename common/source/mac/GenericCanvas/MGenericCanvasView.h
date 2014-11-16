//
//  MGenericCanvasView.h
//  GenericCanvas
//
//  Created by Alfredo Kojima on 05/6/16.
//  Copyright 2005 MySQL AB. All rights reserved.
//

#import <Cocoa/Cocoa.h>


class MGCVListener;
class CGenericCanvas;
class CGCView;

@class MCanvasScrollView;
@class MGenericCanvasView;

@interface NSObject(MGenericCanvasDelegate)
- (BOOL)canvas:(MGenericCanvasView*)canvas handleMouseUp:(int)button atPoint:(NSPoint)pos modifiers:(int)state;
- (BOOL)canvas:(MGenericCanvasView*)canvas handleMouseDown:(int)button atPoint:(NSPoint)pos modifiers:(int)state;
- (BOOL)canvas:(MGenericCanvasView*)canvas handleMouseDrag:(NSPoint)pos modifiers:(int)state;
- (void)canvas:(MGenericCanvasView*)canvas handleChange:(int)reason;
@end

@interface MGenericCanvasView : NSOpenGLView 
{
  CGenericCanvas *_canvas;
  MGCVListener *_listener;

  NSEvent *_lastEvent;
  
  NSSize _baseSize;
  NSPoint _offset;
  
  NSPoint _lastClickPoint;  
  NSTimeInterval _lastClickTime;
  
  NSCursor *_cursor;
  NSTimer *_timer;
  
  NSRect _viewport;
  float _zoom;
  int _currentZoomStep;
  int _autoScrollInterval;
  float _autoScrollDelay;
  
  id _delegate;
  
  BOOL _autoCenterZoom;
  BOOL _grabPanning;
  
  float _lastFPS;

  struct {
    unsigned panning:1;
  } _state;
}

+ (void)registerSystemColors;

- (void)setZoom:(float)zoom withCenter:(NSPoint)center;
- (void)setZoomLevel:(int)level;
- (int)zoomLevel;

- (void)toggleOverview:(id)sender;

- (void)setDelegate:(id)delegate;

- (void)setCursor:(NSCursor*)cursor;

- (void)setOffset:(NSPoint)offset;
- (void)setBaseSize:(NSSize)size;
- (NSSize)baseSize;

- (NSPoint)convertToCanvasPoint:(NSPoint)point;
- (NSPoint)convertFromCanvasPoint:(NSPoint)point;

- (NSSize)actualSize;
- (NSRect)actualVisibleRect;

- (void)zoomInAt:(NSPoint)center;
- (void)zoomOutAt:(NSPoint)center;
- (void)setZoom:(float)zoom;

- (BOOL)loadLayoutsFromFile:(NSString*)file;
- (BOOL)loadStylesFromFile:(NSString*)file withVariables:(NSDictionary*)variables;

- (void)renderScene;
- (void)updateScrollers;

- (void)setGrabPanning:(BOOL)flag;

- (void)createCanvasView:(NSString*)viewName;
- (void)createCanvasViewWithView:(CGCView*)view;

- (void)setGridEnabled:(BOOL)flag;
- (BOOL)gridEnabled;

- (void)switchToCanvasView:(CGCView*)view;

- (void)startAreaSelectionAt:(NSPoint)point;
- (NSRect)finishAreaSelection;

- (CGenericCanvas*)canvas;

- (NSArray*)getZoomSteps;

- (NSEvent*)lastEvent;

@end



#define DEFAULT_CANVAS_ZOOM_LEVEL 12


