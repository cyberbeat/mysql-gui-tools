/* Copyright (C) 2004 MySQL AB

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

/**
 * @file myx_gc_view.cpp 
 * @brief Implementation of the view class.
 * 
 */

#include "myx_gc_canvas.h"
#include "myx_gc_feedback.h"
#include "gc_glext.h"
#include "myx_gc_gl_helper.h"

extern const TMatrix identity;

//----------------- CFigureInstanceEnumerator --------------------------------------------------------------------------

/**
 * Constructor of the enumerator class.
 *
 * @param view The view which contains the layers which are to be enumerated.
 */
CFigureInstanceEnumerator::CFigureInstanceEnumerator(CGCView* view)
{
  FView = view;
  reset();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if there is a next figure instance to enumerate.
 *
 * @return True if there is still a figure instance otherwise false.
 */
bool CFigureInstanceEnumerator::hasNext(void)
{
  return FLayerIterator != FView->FLayers.end();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the next figure instance in the sequence.
 *
 * @return The next figure instance.
 */
CFigureInstance* CFigureInstanceEnumerator::next(void)
{
  CFigureInstance* result = *FFigureInstanceIterator;

  // Advance to next instance.
  ++FFigureInstanceIterator;
  while (true)
  {
    CLayer* layer = *FLayerIterator;
    if (FFigureInstanceIterator != layer->FInstances.end())
      break;

    // This layer is exhausted. Get the next one.
    ++FLayerIterator;
    if (FLayerIterator == FView->FLayers.end())
    {
      // No more layers.
      break;
    };

    if (!(*FLayerIterator)->FEnabled)
      FFigureInstanceIterator = (*FLayerIterator)->FInstances.end();
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Frees this enumerator instance. Usually called by non-C++ languages as memory is managed by the C++ runtime.
 */
void CFigureInstanceEnumerator::release(void)
{
  delete this;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Resets the enumerator to the first figure instance in the canvas.
 */
void CFigureInstanceEnumerator::reset(void)
{
  FLayerIterator = FView->FLayers.begin();
  while (FLayerIterator != FView->FLayers.end())
  {
    if ((*FLayerIterator)->FEnabled)
    {
      CLayer* layer = *FLayerIterator;
      FFigureInstanceIterator = layer->FInstances.begin();
      if (FFigureInstanceIterator == layer->FInstances.end())
      {
        ++FLayerIterator;
      }
      else
        break; // Found the first instance.
    }
    else
      ++FLayerIterator;
  };
}

//----------------- CLayerListener -----------------------------------------------------------------------------

void CGCView::CLayerListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  view->action(origin, action);
}

//----------------------------------------------------------------------------------------------------------------------

void CGCView::CLayerListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  view->handleChange(origin, reason);
};

//----------------------------------------------------------------------------------------------------------------------

void CGCView::CLayerListener::onDestroy(CGCBase* object)
{
  view->removeLayer((CLayer*) object);
}

//----------------------------------------------------------------------------------------------------------------------

void CGCView::CLayerListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  view->error(origin, message);
}

//----------------- CGCView --------------------------------------------------------------------------------------------

CGCView::CGCView(CGenericCanvas* canvas, string name): CGCBase(canvas)
{
  _className = "CGCView";
  FName = utf8ToUtf16(name);
  FColor[0] = 0;
  FColor[1] = 0;
  FColor[2] = 0;
  FColor[3] = 1;
  FZoom = 1;
  FOffsetX = 0;
  FOffsetY = 0;
  FWorkspaceWidth = 1000;
  FWorkspaceHeight = 1000;
  FNearPlane = -1000;
  FFarPlane = 1000;
  FJitter = 0;
  FListener.view = this;
  FStates = 0;
  FOrigin[0] = 0;
  FOrigin[1] = 0;
  FOrigin[2] = 0;

  FGrid = new CGridLayer(this);
  FFeedbackLayer = new CFeedbackLayer(this);
  FFeedbackLayer->addListener(&FListener);
  FConnectionLayer = new CConnectionLayer(this);
  FConnectionLayer->addListener(&FListener);
  FExposeFloater = NULL;
  FPrimitiveLayer = new CPrimitiveLayer(canvas);
  FCache = NULL;
  FDirty = true;
  FInAnimation = false;
}

//----------------------------------------------------------------------------------------------------------------------

CGCView::~CGCView()
{
  delete FCache;
  FCache = NULL;

  for (CLayers::const_iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    (*iterator)->removeListener(&FListener);

  delete FGrid;
  delete FFeedbackLayer;
  delete FConnectionLayer;
  delete FPrimitiveLayer;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Used to set up things that need only to be done once if a view becomes active.
 */
void CGCView::activate(void)
{
  // TODO: make this dynamically configurable (where applicable).
  glFrontFace(GL_CCW);
  glDisable(GL_CULL_FACE);
  glDisable(GL_DITHER);
  glDisable(GL_DEPTH_TEST);

  // The following is only useful if a multisampling pixel format was set up.
  if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
    glEnable(GL_MULTISAMPLE_ARB); 
  
  // Line and polygon smoothing is done via the multisample extension (if existant).
  glDisable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glDisable(GL_POLYGON_SMOOTH);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_NORMALIZE);

  glDisable(GL_FOG);
  glDisable(GL_LOGIC_OP);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_TEXTURE_1D);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  
  // Disable extensions that could slow down glDrawPixels.
  if (canvas()->supportsExtension(GC_OE_CONVOLUTION))
  {
    // A recent glext.h include file is required for these constants.
    // Get one from www.opengl.org.
#ifdef GL_VERSION_1_3
    glDisable(GL_CONVOLUTION_1D);
    glDisable(GL_CONVOLUTION_2D);
    glDisable(GL_SEPARABLE_2D);
#else
    glDisable(GL_CONVOLUTION_1D_EXT);
    glDisable(GL_CONVOLUTION_2D_EXT);
    glDisable(GL_SEPARABLE_2D_EXT);
#endif
  };

  if (canvas()->supportsExtension(GC_OE_HISTOGRAM))
  {
#ifdef GL_VERSION_1_3
    glDisable(GL_HISTOGRAM);
    glDisable(GL_MINMAX);
#else
    glDisable(GL_HISTOGRAM_EXT);
    glDisable(GL_MINMAX_EXT);
#endif
  };

  if (canvas()->supportsExtension(GC_OE_TEXTURE3D))
  {
#ifdef GL_VERSION_1_3
    glDisable(GL_TEXTURE_3D);
#else
    glDisable(GL_TEXTURE_3D_EXT);
#endif
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the animation manager when an animation of this view starts freshly or after a suspension.
 */
void CGCView::animationStarted(void)
{
  FInAnimation = true;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the animation manager when an animation of this view is stopped (either finished or suspended).
 *
 * @param zoom If true then a notification is sent out about a zoom change.
 * @param offset If true then a notification is sent out about an offset change.
 */
void CGCView::animationStopped(bool zoom, bool offset)
{
  FInAnimation = false;
  notifyZoomChange();
  if (zoom)
    change(this, GC_CHANGE_VIEW_ZOOM);
  if (offset)
    change(this, GC_CHANGE_VIEW_OFFSET);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets up the current projection and modelview matrices for normal rendering.
 */
void CGCView::applyTransformations(void)
{
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-FOffsetX / FZoom - FJitter, (FViewport.width - FOffsetX) / FZoom - FJitter, 
    (FViewport.height - FOffsetY) / FZoom - FJitter, -FOffsetY / FZoom - FJitter, FNearPlane, FFarPlane);

  glViewport(FViewport.left, FViewport.top, FViewport.width, FViewport.height);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Draws a rectangle around the entire view to indicate the focused state of the canvas.
 */
void CGCView::drawFocusRect(void)
{
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, FViewport.width, FViewport.height, 0, FNearPlane, FFarPlane);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

#ifdef __WIN__
  glColor3f(0, 128.0f / 255, 192.0f / 255);
  glEnable (GL_LINE_STIPPLE);
  glLineStipple (1, 0x3333);  //  dotted
  glLineWidth(2);
  glBegin(GL_LINE_LOOP);
    glVertex3i(0, 0, 100);
    glVertex3i(FViewport.width, 0, 100);
    glVertex3i(FViewport.width, FViewport.height, 100);
    glVertex3i(0, FViewport.height, 100);
  glEnd();
  glLineWidth(1);
  glDisable(GL_LINE_STIPPLE);
#elif defined(__APPLE__)
  glColor4f(0.52, 0.62, 0.71, 0.8);
  glLineWidth(1);
  glBegin(GL_LINE_LOOP);
    glVertex3i(0, 0, 100);
    glVertex3i(FViewport.width, 0, 100);
    glVertex3i(FViewport.width, FViewport.height, 100);
    glVertex3i(0, FViewport.height, 100);
  glEnd();
  for (int i= 1; i <= 3; i++)
  {
    glColor4f(0.57, 0.72, 0.85, 0.8 - (float)i/6);
    glBegin(GL_LINE_LOOP);
      glVertex3i(i, i, 100);
      glVertex3i(FViewport.width-i, i-1, 100);
      glVertex3i(FViewport.width-i, FViewport.height-i, 100);
      glVertex3i(i, FViewport.height-i, 100);
    glEnd();
  }
#else
  glColor3f(0, 0, 0);
  glEnable (GL_LINE_STIPPLE);
  glLineStipple (1, 0xaaaa);  //  dotted
  glLineWidth(1);
  glBegin(GL_LINE_LOOP);
    glVertex3i(1, 1, 100);
    glVertex3i(FViewport.width, 1, 100);
    glVertex3i(FViewport.width, FViewport.height, 100);
    glVertex3i(1, FViewport.height, 100);
  glEnd();
  glDisable(GL_LINE_STIPPLE);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks for an action that can be executed in the view.
 *
 * @param button Which button has been pressed (left, middle, right, x1, x2).
 * @param event The mouse event that caused the handling chain to start (up, down etc.).
 * @param modifiers Special flags that control the processing.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @param feedbackInfo Info about previous feedback checks by the caller.
 * @param instance If set it holds a figure instance determined by previous feedback code.
 * @return The last action that was executed or NULL if there was none.
 */
TAction* CGCView::handleAction(TMouseButton button, TMouseEvent event, TModifiers modifiers, int windowX, int windowY, 
                               TVertex& viewCoords, TFeedbackInfo feedbackInfo, CFigureInstance* instance)
{
  static TAction defaultAction;
  defaultAction.type = GC_ACTION_NONE;
  TAction* lastAction = &defaultAction;

  bool keepHitInfo = false;
  if (instance == NULL || feedbackInfo == GC_FI_NONE)
  {
    // Determine if an element is hit.
    updateCache();

    CGraphicElement* hit = NULL;
    if (instance == NULL)
      hit = FCache->findElement(viewCoords);
    else
      hit = instance;
    // If an element was hit for which we have no further feedback info then just set the feedback to "on body".
    if (hit != NULL && feedbackInfo == GC_FI_NONE)
      feedbackInfo = GC_FI_ON_OBJECT;

    if (feedbackInfo != GC_FI_NONE)
    {
      // Since there is a small gap between object and selection decoration
      // hit can be NULL.
      if (hit != NULL)
      {
        instance = (CFigureInstance*) hit;
        TAction* action = instance->executeAssociatedActions(button, event, modifiers, viewCoords);
        if (action != NULL)
        {
          lastAction = action;
          if (action->type == GC_ACTION_DRAG)
          {
            // Ask app about dragging the figure.
            action = &defaultAction;
            action->type = GC_ACTION_DRAG_INSTANCE;
            instance->action(instance, &action);
            if (action->type == GC_ACTION_DRAG_INSTANCE)
            {
              lastAction = action;
              FStates |= GC_STATE_DRAG_PENDING;
              if (modifiers == GC_MODIFIER_NONE)
                FStates |= GC_STATE_CLEAR_PENDING;
              else
                if ((modifiers & GC_MODIFIER_SHIFT) != 0)
                  FFeedbackLayer->addToSelection(instance);
                else
                  if ((modifiers & GC_MODIFIER_CONTROL) != 0)
                    if (instance->selected())
                      FFeedbackLayer->removeFromSelection(instance);
                    else
                      FFeedbackLayer->addToSelection(instance);

              // If the hit is not yet selected then drag only this one instance,
              // otherwise the entire selection will be dragged.
              FDragSelection = instance->selected();
              keepHitInfo = true;
            };
          }
          else
            if (action->type == GC_ACTION_RESIZE)
            {
              feedbackInfo = action->feedbackInfo;
              keepHitInfo = true;
            };
        };
      };
    };
  };

  switch (feedbackInfo)
  {
    case GC_FI_RESIZE_NORTH:
    case GC_FI_RESIZE_NORTH_EAST:
    case GC_FI_RESIZE_EAST:
    case GC_FI_RESIZE_SOUTH_EAST:
    case GC_FI_RESIZE_SOUTH:
    case GC_FI_RESIZE_SOUTH_WEST:
    case GC_FI_RESIZE_WEST:
    case GC_FI_RESIZE_NORTH_WEST:
      {
        lastAction->type = GC_ACTION_RESIZE;
        action(this, &lastAction);
        if (lastAction != NULL && lastAction->type == GC_ACTION_RESIZE)
        {
          FStates |= GC_STATE_RESIZING;
          FResizeInfo = feedbackInfo;
          keepHitInfo = true;
        };
        break;
      };
  };

  if (keepHitInfo)
  {
    FLastHit = instance;
    FLastWindowX = windowX;
    FLastWindowY= windowY;
    FLastViewX = viewCoords.x;
    FLastViewY = viewCoords.y;
  };

  return lastAction;
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Main handler routine for double click handling.
 *
 * @param button Which button has been pressed (left, middle, right, x1, x2).
 * @param modifiers Special flags that control the processing.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @return True if the input was handled, otherwise false.
 */
bool CGCView::handleMouseDblClick(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  switch (button)
  {
    case GC_MOUSE_BUTTON_LEFT:
      {
        FStates |= GC_STATE_LBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_MIDDLE:
      {
        FStates |= GC_STATE_MBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_RIGHT:
      {
        FStates |= GC_STATE_RBUTTON_DOWN;
        break;
      };
  };

  TAction* action = handleAction(button, GC_MOUSE_DBL_CLICK, modifiers, windowX, windowY, viewCoords, GC_FI_NONE, NULL);

  return (action != NULL);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Main handler routine for mouse button down handling.
 *
 * @param button Which button has been pressed (left, middle, right, x1, x2).
 * @param modifiers Special flags that control the processing.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @return True if the input was handled, otherwise false.
 */
bool CGCView::handleMouseDown(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool result = false;
  TAction defaultAction;
  defaultAction.type = GC_ACTION_NONE;
  TAction* lastAction = &defaultAction;

  switch (button)
  {
    case GC_MOUSE_BUTTON_LEFT:
      {
        FStates |= GC_STATE_LBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_MIDDLE:
      {
        FStates |= GC_STATE_MBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_RIGHT:
      {
        FStates |= GC_STATE_RBUTTON_DOWN;
        break;
      };
  };

  // First check special layers with interactive objects.
  CFigureInstance* instance = NULL;

  TFeedbackInfo feedbackInfo = FFeedbackLayer->getFeedbackInfo(modifiers, viewCoords, FZoom, &instance);
  if (feedbackInfo == GC_FI_FLOATER)
  {
    lastAction->type = GC_ACTION_FLOATER_DRAG;
    action(this, &lastAction);
    if (lastAction != NULL && lastAction->type == GC_ACTION_FLOATER_DRAG)
    {
      FStates |= GC_STATE_OVERVIEW_DRAG;
      FLastWindowX = windowX;
      FLastWindowY= windowY;
      FLastViewX = viewCoords.x;
      FLastViewY = viewCoords.y;
      result = true;
    };
  };

  if (!result)
  {
    TAction* action = handleAction(button, GC_MOUSE_DOWN, modifiers, windowX, windowY, viewCoords, feedbackInfo, instance);
    if (action != NULL && action->type != GC_ACTION_NONE)
    {
      lastAction = action;
      result = true;
    };
  };

  if (feedbackInfo == GC_FI_NONE)
    result = FConnectionLayer->handleMouseDown(button, modifiers, windowX, windowY, viewCoords);

  if (!result && (lastAction->type == GC_ACTION_NONE) && (button == GC_MOUSE_BUTTON_LEFT))
  {
    // Nothing happend so far so start the rubber rectangle if allowed.
    lastAction->type = GC_ACTION_RUBBER_RECT;
    action(this, &lastAction);
    if (lastAction != NULL && lastAction->type == GC_ACTION_RUBBER_RECT)
    {
      rubberRectStart(GC_RRSTYLE_BLENDED_SOLID, viewCoords, modifiers <= GC_MODIFIER_NONE);
      FStates |= GC_STATE_SELECTION_RECTANGLE;
      result = true;
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Main handler routine for mouse button move handling.
 *
 * @param modifiers Any combination of TModifier. Specifies further how to handle the input.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @return True if the input was handled, otherwise false.
 */
bool CGCView::handleMouseMove(TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool result = false;

  if ((FStates & GC_STATE_RUBBER_RECTANGLE) != 0)
  {
    TRRSelectionAction action = GC_RRACTION_NONE;
    if ((FStates & GC_STATE_SELECTION_RECTANGLE) != 0)
    {
      action = GC_RRACTION_SELECT_REMOVE;
      if ((modifiers & GC_MODIFIER_SHIFT) != 0)
        action = GC_RRACTION_SELECT; // Always-add has higher prio than toggle.
      else
        if ((modifiers & GC_MODIFIER_CONTROL) != 0)
          action = GC_RRACTION_TOGGLE;
    };

    FFeedbackLayer->rubberRectResize(viewCoords, action);
    result = true;
  };

  if ((FStates & GC_STATE_RUBBER_BAND) != 0)
  {
    FFeedbackLayer->rubberBandResize(viewCoords);
    result = true;
  };

  if ((FStates & GC_STATE_DRAG_PENDING) != 0)
  {
    // Drag is still waiting to start. Check if the user moved the mouse a bit.
    float threshold = 4; // Empirically found.
    if (abs(FLastWindowX - windowX) >= threshold || abs(FLastWindowY - windowY) >= threshold)
    {
      FStates &= ~(GC_STATE_CLEAR_PENDING | GC_STATE_DRAG_PENDING);
      FStates |= GC_STATE_DRAGGING;
      change(this, GC_CHANGE_VIEW_DRAGGING_START);
    };
  };

  // Now the moved distance becomes important.
  float dX = viewCoords.x - FLastViewX;
  float dY = viewCoords.y - FLastViewY;
  FLastViewX = viewCoords.x;
  FLastViewY = viewCoords.y;
  
  if ((FStates & GC_STATE_DRAGGING) != 0)
  {
    if (FDragSelection)
      FFeedbackLayer->moveSelectedInstances(dX, dY, 0, true);
    else
      FLastHit->translate(dX, dY, 0, true);
    #if USE_BSP_TREE
      FCache->invalidate();
    #endif
    result = true;
  }
  else
    if ((FStates & GC_STATE_RESIZING) != 0)
    {
      FLastHit->resize(dX, dY, FResizeInfo);
      #if USE_BSP_TREE
        FCache->invalidate();
      #endif
      result = true;
    }
    else
      if ((FStates & GC_STATE_OVERVIEW_DRAG) != 0)
      {
        FLastOffsetX -= dX * FLastZoom;
        FLastOffsetY -= dY * FLastZoom;
        FExposeFloater->move(dX, dY);
        result = true;
      };

  if (!result)
    result = FConnectionLayer->handleMouseMove(modifiers, windowX, windowY, viewCoords);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Main handler routine for mouse button up handling.
 *
 * @param button Which button has been released (left, middle, right).
 * @param modifiers Special flags that control the processing.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @return True if the input was handled, otherwise false.
 */
bool CGCView::handleMouseUp(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool handled = false;

  if ((FStates & GC_STATE_SELECTION_RECTANGLE) != 0)
  {
    rubberRectStop();
    FStates &= ~GC_STATE_SELECTION_RECTANGLE;
    handled = true;
  };

  if (!handled)
    handled = FConnectionLayer->handleMouseUp(button, modifiers, windowX, windowY, viewCoords);

  // All following processing is always done regardless of the results from the special layers (except for actions).
  if ((FStates & GC_STATE_CLEAR_PENDING) != 0)
  {
    // A pending clear selection action must be finished. If there is an instance hit from mouse down
    // then select this single instance.
    FStates &= ~GC_STATE_CLEAR_PENDING;
    FFeedbackLayer->clearSelection();
    if (FLastHit != NULL)
      FFeedbackLayer->addToSelection(FLastHit);
  };

  FLastHit = NULL;

  if ((FStates & GC_STATE_DRAGGING) != 0)
  {
    FStates &= ~GC_STATE_DRAGGING;
    change(this, GC_CHANGE_VIEW_DRAGGING_STOP);
  };

  // Remove all states which can be stopped without further action.
  FStates &= ~(GC_STATE_DRAG_PENDING | GC_STATE_RESIZING | GC_STATE_OVERVIEW_DRAG);

  switch (button)
  {
    case GC_MOUSE_BUTTON_LEFT:
      {
        FStates &= ~GC_STATE_LBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_MIDDLE:
      {
        FStates &= ~GC_STATE_MBUTTON_DOWN;
        break;
      };
    case GC_MOUSE_BUTTON_RIGHT:
      {
        FStates &= ~GC_STATE_RBUTTON_DOWN;
        break;
      };
  };

  // Now handle actions bound to mouse up.
  if (!handled)
    handleAction(button, GC_MOUSE_UP, modifiers, windowX, windowY, viewCoords, GC_FI_NONE, NULL);

  return handled;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Internal render routine used by different other functions.
 *
 * @param content Determines what to render.
 * @param visibleBounds The visible area that needs to be rendered. Anything outside can be omitted.
 */
void CGCView::internalRender(TGCRenderContent content, const TBoundingBox& visibleBounds)
{
  if ((content & GC_RENDER_GRID) != 0)
    FGrid->render(visibleBounds);

  if ((content & GC_RENDER_FRAME) != 0)
  {
    // A simple frame marks the virtual workspace size.
    glColor3ub(0, 0, 0);
    glBegin(GL_LINE_LOOP); 
      glVertex2f(0, 0);
      glVertex2f(FWorkspaceWidth - 1, 0); 
      glVertex2f(FWorkspaceWidth - 1, FWorkspaceHeight - 1);
      glVertex2f(0, FWorkspaceHeight - 1); 
    glEnd();
  };

  for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    (*iterator)->render(visibleBounds);

  FPrimitiveLayer->render(visibleBounds);

  if ((content & GC_RENDER_CONNECTIONS) != 0)
    FConnectionLayer->render(visibleBounds);

  if ((content & GC_RENDER_FEEDBACK) != 0)
    FFeedbackLayer->render(visibleBounds);

  if (canvas()->focusedGet() && ((content & GC_RENDER_FOCUS) != 0))
    drawFocusRect();
 };

//----------------------------------------------------------------------------------------------------------------------

/**
 * Notfies all layers in this view about a zoom change. This is necessary to have them updated element captions.
 */
void CGCView::notifyZoomChange(void)
{
  // Tell all layers in this view about the change. They don't have a listener as they can appear
  // in more than one view
  FGrid->zoomChanged(FZoom);
  for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    (*iterator)->zoomChanged(FZoom);
  FPrimitiveLayer->zoomChanged(FZoom);
  FConnectionLayer->zoomChanged(FZoom);
  FFeedbackLayer->zoomChanged(FZoom);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * This is the main paint routine. It is called by the canvas if this view is the current view.
 *
 * @param The content to render.
 */
void CGCView::render(TGCRenderContent content)
{
  applyTransformations();

  // Transform viewport coordinates into view space. They are used for occlusion culling.
  TBoundingBox visibleBounds;
  visibleBounds.upper.x = -FOffsetX / FZoom;
  visibleBounds.upper.y = -FOffsetY / FZoom;
  visibleBounds.lower.x = (FViewport.width - FOffsetX) / FZoom;
  visibleBounds.lower.y = (FViewport.height - FOffsetY) / FZoom;

  internalRender(content, visibleBounds);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the entire content of the view to memory. The function tries to use an OpenGL extension (frame buffer objects)
 * which may not be available on all graphics boards. If this is the case or the set up of this extension fails then
 * a fall back is use, rendering the tradional (but slow and inaccurate) way.
 *
 * @param format Determines the order of the color components in the result.
 * @param content A set of flags indicating what additional info to render.
 * @param zoom The zoom level to be used for the output.
 * @param bounds [in/out] The coordinates of the area that should be rendered (in view space). When traditional rendering
 *                        is used then it might be necessary to implicitly restrict this area to the current viewport.
 *                        In this case bounds are used to return actual area to the caller.
 * @param memory The memory into which the view is to be rendered.
 */
void CGCView::renderToMemory(TGCColorFormat format, TGCRenderContent content, float zoom, TGCViewport& bounds, 
                             unsigned char* memory)
{
  GLenum glFormat = GL_RGBA;
  if (format == GC_COLOR_FORMAT_BGRA)
    glFormat = GL_BGRA;

  bool canUseFBOs = canvas()->supportsExtension(GC_OE_FRAME_BUFFER_OBJECTS);
  if (canUseFBOs)
  {
    // Get current draw buffer for later restoration.
    GLint currentDrawBuffer = 0;
    glGetIntegerv(GL_DRAW_BUFFER, &currentDrawBuffer);

    // Make sure we do not allocate more buffer space than supported.
    GLint maxBufferSize = 0x7FFFFFFF;
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE_EXT, &maxBufferSize);

    GLint maxViewport[2];
    glGetIntegerv(GL_MAX_VIEWPORT_DIMS, maxViewport);

    // Determine area to render.
    float left = (float) bounds.left;
    float top = (float) bounds.top;
    float width = (float) bounds.width;
    if (width < 0)
      width = FWorkspaceWidth;
    float height = (float) bounds.height;
    if (height < 0)
      height = FWorkspaceHeight;

    int bufferWidth = 0;
    int bufferHeight = 0;

    GLuint frameBuffer = 0;
    GLuint renderBuffer = 0;

    GLenum status = GL_FRAMEBUFFER_UNSUPPORTED_EXT;
    while (maxBufferSize >= 128)
    {
      // Set maximum possible viewport here (restore old one on exit).
      bufferWidth = (maxViewport[0] < maxBufferSize) ? maxViewport[0] : maxBufferSize;
      bufferHeight = (maxViewport[1] < maxBufferSize) ? maxViewport[1] : maxBufferSize;

      if (width < bufferWidth)
        bufferWidth = (int) width;
      if (height < bufferHeight)
        bufferHeight = (int) height;

      // Frame buffer objects allow for hardware accelerated off-screen rendering.
      glGenFramebuffersEXT(1, &frameBuffer);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, frameBuffer);
      glGenRenderbuffersEXT(1, &renderBuffer);
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, renderBuffer);
      glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_RENDERBUFFER_EXT, renderBuffer);

      glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_RGBA8, bufferWidth, bufferHeight); 
      
      // Check validity of the frame buffer and try other configs if possible. 
      // If all fails go back to traditional rendering, but don't expect good results then.
      status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
      if (status != GL_FRAMEBUFFER_UNSUPPORTED_EXT)
        break;

      // The FBO configuration did not work, so free the buffers and try again with smaller size.
      glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
      glDeleteRenderbuffersEXT(1, &renderBuffer);
      glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
      glDeleteFramebuffersEXT(1, &frameBuffer);
      maxBufferSize = maxBufferSize / 2;
    };

    if (status != GL_FRAMEBUFFER_COMPLETE_EXT)
      canUseFBOs = false;
    
    // Now that we know the dimensions we can set the viewport.
    glViewport(0, 0, bufferWidth, bufferHeight);

    if (canUseFBOs)
    {
      float originalZoom = FZoom;
      FZoom = zoom;
      notifyZoomChange();
      validate();

      glPixelStoref(GL_PACK_ROW_LENGTH, width);

      // Render the entire workspace by splitting it in pieces of the render buffer size.
      for (float y = 0; y < height; y += bufferHeight)
      {
        for (float x = 0; x < width; x += bufferWidth)
        {
          int currentWidth = (width - x < bufferWidth) ? int(width - x) : bufferWidth;
          int currentHeight = (height - y < bufferHeight) ? int(height - y) : bufferHeight;
          
          float verticalOffset = height - y - currentHeight;
          int offset = int(verticalOffset * width + x) * 4; // * 4 because we have 4 components per pixel.

          glMatrixMode(GL_PROJECTION);
          glLoadIdentity();

          // Set an orthogonal (parallel) projection, mapping the current view area in the entire workspace to the 
          // viewport coordinates.
          glOrtho((x + left) / zoom, (x + left + bufferWidth) / zoom, 
            (y + top + bufferHeight) / zoom, (y + top) / zoom, FNearPlane, FFarPlane);
          glMatrixMode(GL_MODELVIEW);
          glLoadIdentity();
          canvas()->clearBuffers();

          // Transform viewport coordinates into view space. They are used for occlusion culling.
          TBoundingBox visibleBounds;
          visibleBounds.upper.x = (x + left) / zoom;
          visibleBounds.upper.y = (y + top) / zoom;
          visibleBounds.lower.x = (x + left + currentWidth) / zoom;
          visibleBounds.lower.y = (y + top + currentHeight) / zoom;

          internalRender(content, visibleBounds);
          glReadPixels(0, bufferHeight - currentHeight, currentWidth, currentHeight, glFormat, GL_UNSIGNED_BYTE, memory + offset);
        };
      };
      glPixelStoref(GL_PACK_ROW_LENGTH, 0);
      FZoom = originalZoom;
      notifyZoomChange();
    };

    // Release frame buffer binding to enable normal rendering again.
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);
    glDeleteRenderbuffersEXT(1, &renderBuffer);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    glDeleteFramebuffersEXT(1, &frameBuffer);

    glViewport(FViewport.left, FViewport.top, FViewport.width, FViewport.height);

    // Return the actually used bounds to the caller. Particularly width and height are important to
    // handle following interpretation of the rendered data.
    bounds.left = (int) left;
    bounds.top = (int) top;
    bounds.width = (int) width;
    bounds.height = (int) height;
  };

  // Fallback in case either the frame buffer objects extension is not supported or could not properly be set up.
  if (!canUseFBOs)
  {
    // Determine area to render. We can here only render what's currently in the viewport, so limit the area to that.
    // For validation we need the coords in window space, though.
    int left;
    int top;
    viewToWindow(TVertex(bounds.left, bounds.top, 0), left, top);

    int right;
    int bottom;

    int width = bounds.width;
    if (width < 0)
      width = (int) FWorkspaceWidth;
    int height = bounds.height;
    if (height < 0)
      height = (int) FWorkspaceHeight;
    viewToWindow(TVertex(bounds.left + width, bounds.top + height, 0), right, bottom);
    if (left < 0)
      left = 0;
    if (top < 0)
      top = 0;
    if (right > FViewport.width)
      right = FViewport.width;
    if (bottom > FViewport.height)
      bottom = FViewport.height;
    width = right - left;
    height = bottom - top;

    glPixelStorei(GL_PACK_ROW_LENGTH, width);
    glReadBuffer(GL_BACK);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    glOrtho(-FOffsetX / zoom - FJitter, (FViewport.width - FOffsetX) / zoom - FJitter, 
      (FViewport.height - FOffsetY) / zoom - FJitter, -FOffsetY / zoom - FJitter, FNearPlane, FFarPlane);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    canvas()->clearBuffers();

    // Transform viewport coordinates into view space. They are used for occlusion culling.
    TBoundingBox visibleBounds;
    visibleBounds.upper.x = -FOffsetX / zoom;
    visibleBounds.upper.y = -FOffsetY / zoom;
    visibleBounds.lower.x = (FViewport.width - FOffsetX) / zoom;
    visibleBounds.lower.y = (FViewport.height - FOffsetY) / zoom;

    internalRender(content, visibleBounds);

    glReadPixels(left, FViewport.height - top - height, width, height, glFormat, GL_UNSIGNED_BYTE, memory);
    glPixelStoref(GL_PACK_ROW_LENGTH, 0);

    // Return the actually used bounds to the caller. Particularly width and height are important to
    // handle following interpretation of the rendered data.
    bounds.left = left;
    bounds.top = top;
    bounds.width = width;
    bounds.height = height;
  };

  canvas()->checkError();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks if the view's element cache must be created and validated.
 */
void CGCView::updateCache(void)
{
  if (FCache == NULL)
    FCache = new CElementLookupCache(FWorkspaceWidth, FWorkspaceHeight);

  if (!FCache->isValid())
  {
    CFigureInstanceEnumerator* enumerator = getFigureInstanceEnumerator();
    while (enumerator->hasNext())
      FCache->addElement(enumerator->next());
    delete enumerator;

    FCache->validate();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Validates all associated structures (layers etc.).
 */
void CGCView::validate(void)
{
  if (FDirty)
  {
    FDirty = false;
    FGrid->validate();

    glClearColor(FColor[0], FColor[1], FColor[2], FColor[3]);

    for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
      (*iterator)->validate();

    FPrimitiveLayer->validate();
    FConnectionLayer->validate();
    FFeedbackLayer->validate();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a layer to the internal list of layers that belong to this view (only if not yet there).
 *
 * @param layer The layer to add.
 */
void CGCView::addLayer(CLayer* layer)
{
  for (CLayers::const_iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    if (*iterator == layer)
      return;

  layer->addListener(&FListener);
  FLayers.push_back(layer);
  
  change(this, GC_CHANGE_VIEW_ADD_LAYER);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given instance to the current selection if it isn't already.
 *
 * @param instance The figure instance to add to the current selection. It must belong to one of the layers in this view
 *                 otherwise it will not be added.
 */
void CGCView::addToSelection(CFigureInstance* instance)
{
  if (instance != NULL && !instance->selected())
  {
    CLayer* layer = instance->layer();
    if (layer != NULL)
    {
      CLayers::iterator iterator = find(FLayers.begin(), FLayers.end(), layer);
      if (iterator != FLayers.end())
        FFeedbackLayer->addToSelection(instance);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the current content of this view.
 *
 * @param removeLayers If true then all currently defined (normal) layers are removed from this view (but not the canvas)
 *                     in addition to being cleared.
 */
void CGCView::clearContent(bool removeLayers)
{
  beginUpdate();

  FCache->invalidate();

  for (CLayers::const_iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    (*iterator)->clear();

  if (removeLayers)
    FLayers.clear();

  endUpdate();
  change(this, GC_CHANGE_VIEW_CLEAR);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all primitives.
 */
void CGCView::clearPrimitives(void)
{
  FPrimitiveLayer->clear();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all currently selected figure instances from the selection set.
 */
void CGCView::clearSelection(void)
{
  FFeedbackLayer->clearSelection();
  change(this, GC_CHANGE_SELECTION_CLEAR);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the new background color of this view.
 *
 * @param red The red color component.
 * @param green The green color component.
 * @param blue The blue color component.
 * @param alpha The transparency component.
 */
void CGCView::color(float red, float green, float blue, float alpha)
{
  FColor[0] = red;
  FColor[1] = green;
  FColor[2] = blue;
  FColor[3] = alpha;

  change(this, GC_CHANGE_VIEW_PROPERTY);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the new background color of this view
 *
 * @param newColor The new color to use.
 */
void CGCView::colorV(GLfloat* newColor)
{
  FColor[0] = newColor[0];
  FColor[1] = newColor[1];
  FColor[2] = newColor[2];
  FColor[3] = newColor[3];

  change(this, GC_CHANGE_VIEW_PROPERTY);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller whether this view contains a reference to the given layer.
 *
 * @param layer The layer to look for.
 * @param True if the layer is referenced in this view otherwise false.
 */
bool CGCView::contains(CLayer* layer)
{
  for (CLayers::const_iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
    if (*iterator == layer)
      return true;

  return false;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a circle primitive and returns it.
 *
 * @param center The center position of the circle.
 * @param outerRadius The overall radius of the circle.
 * @param innerRadius The radius of the circle's hole. Can be 0 to not produce that hole.
 * @param color The fill and line color of the primitive.
 * @param filled True if filled, otherwise only the outline will be drawn.
 * @result The newly created primitive.
 */
CPrimitive* CGCView::createCircle(const TVertex& center, float outerRadius, float innerRadius, GLfloat* color, bool filled)
{
  CPrimitive* result = new CCircle(center, outerRadius, innerRadius, color, filled);
  FPrimitiveLayer->addPrimitive(result);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Proxy function for the connection layer. Read there for a description.
 */
CConnectionInstance* CGCView::createConnectionInstance(CConnection* connection, CFigureInstance* endPoint1, 
                                                       CFigureInstance* endPoint2)
{
  return FConnectionLayer->createInstance(connection, endPoint1, endPoint2);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Create a floater on the feedback layer. Simply forwards the call there.
 */
CFloater* CGCView::createFloater(const TBoundingBox& bounds, GLfloat* color1, GLfloat* color2, TFloaterDecoration decoration)
{
  return FFeedbackLayer->createFloater(bounds, color1, color2, decoration);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a line primitive and returns it.
 *
 * @param start The start coordinate of the line.
 * @param end The end coordinate of the line.
 * @param color The fill and line color of the primitive.
 * @param width The line's thickness.
 * @param stipple If not 0 or 0xFFFF then it determinse the pattern used for the line.
 * @result The newly created primitive.
 */
CPrimitive* CGCView::createLine(const TVertex& start, const TVertex& end, GLfloat* color, GLfloat width, GLushort stipple)
{
  CPrimitive* result = new CLine(start, end, color, width, stipple);
  FPrimitiveLayer->addPrimitive(result);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a rectangle primitive and returns it.
 *
 * @param bounds The outer bounds of the primitive.
 * @param color The fill and line color of the primitive.
 * @param filled True if filled, otherwise only the outline will be drawn.
 * @result The newly created primitive.
 */
CPrimitive* CGCView::createRectangle(const TBoundingBox& bounds, GLfloat* color, bool filled)
{
  CPrimitive* result = new CRectangle(bounds, color, filled);
  FPrimitiveLayer->addPrimitive(result);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines what action could be executed at the given position. Considered are any feedback state (selection, resize etc.)
 * from the feedback layer as well as actions defined in a figure element. The returned info is usually used to 
 * set an indicator (e.g. the mouse pointer) to a certain state to reflect what is possible at that point.
 *
 * @param modifiers Any combination
 * @param windowX The horizontal target position in window coordinates.
 * @param windowY The vertical target position in window coordinate.
 * @return A flag indicating the possible action state.
 */
TFeedbackInfo CGCView::getFeedbackInfo(TModifiers modifiers, int windowX, int windowY)
{
  TFeedbackInfo result = GC_FI_NONE;

  updateCache();

  if ((FStates & GC_STATE_RESIZING) != 0)
    result = FResizeInfo;
  else
  {
    TVertex viewCoords; 
    windowToView(windowX, windowY, viewCoords);

    CFigureInstance* instance;
    result = FFeedbackLayer->getFeedbackInfo(modifiers, viewCoords, FZoom, &instance);
    
    if (result == GC_FI_NONE)
    {
      CGraphicElement* element = FCache->findElement(viewCoords);
      if (element != NULL)
        result = element->getFeedbackInfo(modifiers, viewCoords);
    };

    if (result == GC_FI_NONE)
      result = FConnectionLayer->getFeedbackInfo(modifiers, viewCoords);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates and returns a new figure instance enumerator instance. The caller is responsible for freeing
 * the returned instance.
 *
 * @return The new enumerator.
 */
CFigureInstanceEnumerator* CGCView::getFigureInstanceEnumerator(void)
{
  return new CFigureInstanceEnumerator(this);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the given coordinates and tries to find a graphic element that was rendered at this position.
 * Positions must be given in view space.
 *
 * @param point The point to check given in view space. If necessary convert window coordinates first by using windowToView.
 * @param singleHit If true then search for hits is stopped after the first one was found.
 * @return A hit result class is returned regardless of the actual number of hits. It must be freed by the caller.
 */
CHitResults* CGCView::getHitTestInfoAt(TVertex point, bool singleHit)
{
  CHitResults* result = new CHitResults();
                      
  updateCache();

  FConnectionLayer->getHitTestInfoAt(result, point, singleHit);
  if (result->count() == 0)
  {
    if (singleHit)
    {
      CGraphicElement* element = FCache->findElement(point);
      if (element != NULL)
        result->addHit(element);
    }
    else
      FCache->findElements(point, result);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current or last set rubber rect/band bounds.
 *
 * @param box [out] Receives the rubber bounds in view coordinates.
 */
void CGCView::getLastRubberBounds(TBoundingBox& box)
{
  box = FFeedbackLayer->rubberBounds();
  FFeedbackLayer->layerToView(box.lower, box.lower);
  FFeedbackLayer->layerToView(box.upper, box.upper);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current origin of the view.
 *
 * @param x [out] The x coordinate of the virtual origin.
 * @param y [out] The y coordinate of the virtual origin.
 * @param z [out] The z coordinate of the virtual origin.
 */
void CGCView::getOrigin(float* x, float* y, float* z)
{
  *x = FOrigin[0];
  *y = FOrigin[1];
  *z = FOrigin[2];
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves a selection enumerator from the feedback layer and returns it. The caller is reponsible for freeing
 * the returned instance.
 *
 * @param The new enumerator.
 */
CSelectionEnumerator* CGCView::getSelectionEnumerator(void)
{
  return FFeedbackLayer->getSelectionEnumerator();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current virtual workspace size within the view.
 *
 * @param width [out] The width of the workspace.
 * @param height [out] The height of the workspace.
 */
void CGCView::getWorkspace(float* width, float* height)
{
  *width = FWorkspaceWidth;
  *height = FWorkspaceHeight;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes care for certain change events that must be considered special.
 */
void CGCView::handleChange(CGCBase* origin, TGCChangeReason reason)
{
  if (!destroying())
  {
    FDirty = true;
    switch (reason)
    {
      case GC_CHANGE_LAYER_ADD_INSTANCE:
      case GC_CHANGE_LAYER_CLEAR:
      case GC_CHANGE_LAYER_REMOVE_INSTANCE:
      case GC_CHANGE_LAYER_ADD_GROUP:
      case GC_CHANGE_LAYER_REMOVE_GROUP:
        {
          if (FCache != NULL)
            FCache->invalidate();

          break;
        };
      default:
        change(origin, reason);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the viewer to let the current view handle user input with the mouse.
 *
 * @param event The actual event (e.g. mouse down or up).
 * @param button Specifies the mouse button for which this event was triggered.
 * @param modifiers Any combination of TModifier. Specifies further how to handle the input.
 * @param x The horizontal window coordinate of the mouse pointer.
 * @param y The vertical window coordinate of the mouse pointer.
 * @return True if the mouse input was handled in some way, otherwise false.
 */
bool CGCView::handleMouseInput(TMouseEvent event, TMouseButton button, TModifiers modifiers, int x, int y)
{
  bool result = false;

  TVertex point;
  windowToView(x, y, point);

  beginUpdate();
  switch (event)
  {
    case GC_MOUSE_DOWN:
      {
        result = handleMouseDown(button, modifiers, x, y, point);
        break;
      };
    case GC_MOUSE_UP:
      {
        result = handleMouseUp(button, modifiers, x, y, point);
        break;
      };
    case GC_MOUSE_MOVE:
      {
        result = handleMouseMove(modifiers, x, y, point);
        break;
      };
    case GC_MOUSE_DBL_CLICK:
      {
        result = handleMouseDblClick(button, modifiers, x, y, point);
        break;
      };
  };
  endUpdate();

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current jitter value.
 *
 * @return The current jitter value.
 */
float CGCView::jitterGet(void)
{
  return FJitter;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Jittering the viewport a little bit sometimes improves display quality (e.g. for thin lines). This function
 * sets this value for this view.
 *
 * @param value The new jitter value.
 */
void CGCView::jitterSet(float value)
{
  FJitter = value;
  change(this, GC_CHANGE_VIEW_PROPERTY);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current horizontal offset.
 *
 * @return The current horizontal offset.
 */
float CGCView::offsetXGet(void)
{
  return FOffsetX;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the horizontal offset of the view. The offset is value about which the content of the view is moved.
 *
 * @param value The new horizontal offset.
 */
void CGCView::offsetXSet(float value)
{
  FOffsetX = value;
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current vertical offset.
 *
 * @return The curretn vertical offset.
 */
float CGCView::offsetYGet(void)
{
  return FOffsetY;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the new vertical offset.
 *
 * @param value The new vertical offset.
 */
void CGCView::offsetYSet(float value)
{
  FOffsetY = value;
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller if this view is currently in overview mode.
 *
 * @result True if the overview mode is active.
 */
bool CGCView::overviewActive(void)
{
  return (FStates & GC_STATE_OVERVIEW) != 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts the overview mode for this view, that is, the view is zoomed out to workspace size and a floater is
 * displayed and can be moved to select a new display area.
 *
 * @param animated If true then a zoom animation is created for going into overview mode.
 */
void CGCView::overviewStart(bool animated)
{
  if ((FStates & GC_STATE_OVERVIEW) == 0)
  {
    FStates |= GC_STATE_OVERVIEW;
    FLastZoom = FZoom;

    // Convert the coordinates of the current viewport into view space. This is the area to which we return
    // after the overview mode is finished. It is indicated by a special floater on the feedback layer.
    TBoundingBox exposeBox;
    windowToView(FViewport.left, FViewport.top, exposeBox.upper);
    windowToView(FViewport.left + FViewport.width, FViewport.top + FViewport.height, exposeBox.lower);

    GLfloat color1[] = {0.0f, 0.0f, 0.0f, 0.04f};
    GLfloat color2[] = {0.0f, 0.0f, 0.0f, 0.3f};
    FExposeFloater = FFeedbackLayer->createFloater(exposeBox, color1, color2, GC_FLOATER_DECO_CUT_MARKERS);
    FFeedbackLayer->visibleParts(GC_FBPART_ALL);

    // Compute new zoom factor so that the full work area is displayed).
    float zoomX = FViewport.width / FWorkspaceWidth;
    float zoomY = FViewport.height / FWorkspaceHeight;
    float newZoom = (zoomX < zoomY) ? zoomX : zoomY;

    FLastOffsetX = FOffsetX;
    FLastOffsetY = FOffsetY;

    if (animated)
    {
      canvas()->animationManager()->createViewOffsetAnimation(0, 0, 200, false, this);
      canvas()->animationManager()->createViewZoomAnimation(newZoom, 200, false, this);
    }
    else
    {
      FOffsetX = 0;
      FOffsetY = 0;
      FZoom = newZoom;

      notifyZoomChange();
      canvas()->refresh();
    };
    change(this, GC_CHANGE_VIEW_OVERVIEW_START);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the overview mode in this view (if it is active) and optionally returns to the last zoom level and display offset.
 *
 * @param returnToZoomed If true then the view is set to the currently stored zoom level and display offset.
 * @param animated If true then a zoom animation is created for leaving overview mode but only if also returnToZoomed is true.
 */
void CGCView::overviewStop(bool returnToZoomed, bool animated)
{
  if ((FStates & GC_STATE_OVERVIEW) != 0)
  {
    FStates &= ~GC_STATE_OVERVIEW;

    delete FExposeFloater;
    FExposeFloater = NULL;
    FFeedbackLayer->visibleParts(GC_FBPART_SELECTION | GC_FBPART_RUBBERRECT | GC_FBPART_RUBBERBAND);

    if (returnToZoomed)
    {
      if (animated)
      {
        canvas()->animationManager()->createViewOffsetAnimation(FLastOffsetX, FLastOffsetY, 200, false, this);
        canvas()->animationManager()->createViewZoomAnimation(FLastZoom, 200, false, this);
      }
      else
      {
        FZoom = FLastZoom;
        FOffsetX = FLastOffsetX;
        FOffsetY = FLastOffsetY;
        notifyZoomChange();
        canvas()->refresh();
      };
    }
    else
      canvas()->refresh();

    change(this, GC_CHANGE_VIEW_OVERVIEW_STOP);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this parameter gives the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CGCView::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              result = utf16ToUtf8(FName);
              break;
            };
          case GC_PROPERTY_DESCRIPTION:
            {
              result = "A view comprising a set of layers.";
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = canvas();
              break;
            };
          case GC_PROPERTY_COLOR:
            {
              result = colorToString(FColor);
              break;
            };
          case GC_PROPERTY_ZOOM:
            {
              result = FZoom;
              break;
            };
          case GC_PROPERTY_X:
            {
              result = FOffsetX;
              break;
			};
          case GC_PROPERTY_Y:
            {
              result = FOffsetY;
              break;
            };
          case GC_PROPERTY_JITTER:
            {
              result = FJitter;
              break;
            };
        };
        break;
      };
    case GC_CONTAINER_LAYERS:
      {
        if (index < FLayers.size())
          result = (CGCBase*) FLayers[index];
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the value of the given property, which must be a simple property.
 *
 * @param name The name of the property.
 * @param index If the property is a list then this parameter gives the index into that list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CGCView::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getPropertyID(name))
  {
	case GC_PROPERTY_COLOR:
	  {
        stringToColor(value, FColor);
        FColor[3] = 1;
        change(this, GC_CHANGE_VIEW_PROPERTY);
        break;
      };
    case GC_PROPERTY_ZOOM:
      {
        FZoom = value;
        change(this, GC_CHANGE_VIEW_ZOOM);
        break;
      };
    case GC_PROPERTY_X:
      {
        FOffsetX = value;
        change(this, GC_CHANGE_VIEW_PROPERTY);
        break;
      };
    case GC_PROPERTY_Y:
      {
        FOffsetY = value;
        change(this, GC_CHANGE_VIEW_PROPERTY);
        break;
      };
    case GC_PROPERTY_JITTER:
      {
        FJitter = value;
        change(this, GC_CHANGE_VIEW_PROPERTY);
        break;
      };
  };
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given instance from the current selection if it selected.
 *
 * @param instance The figure instance to remove. It must belong to one of the layers in this view
 *                 otherwise it will not be removed.
 */
void CGCView::removeFromSelection(CFigureInstance* instance)
{
  if (instance != NULL && !instance->selected())
  {
    CLayer* layer = instance->layer();
    if (layer != NULL)
    {
      CLayers::iterator iterator = find(FLayers.begin(), FLayers.end(), layer);
      if (iterator != FLayers.end())
        FFeedbackLayer->removeFromSelection(instance);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 *  Removes the given layer from the list of layers that comprise this view.
 *
 * @param layer The layer to remove.
 */
void CGCView::removeLayer(CLayer* layer)
{
  if (!destroying())
  {
    for (CLayers::iterator iterator = FLayers.begin(); iterator != FLayers.end(); ++iterator)
      if (*iterator == layer)
      {
        if (!layer->destroying())
          layer->removeListener(&FListener);
        FLayers.erase(iterator);
        if (FCache)
          FCache->invalidate();

        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts the rubber band mode.
 *
 * @param style The visual style of the rubber band.
 * @param coords The start coordinates of the band in view space.
 */
void CGCView::rubberBandStart(TRubberBandStyle style, const TVertex& coords)
{
  // Implizitely stops any active rubber band.
  FFeedbackLayer->rubberBandStart(style, coords);
  FStates |= GC_STATE_RUBBER_BAND;
  change(this, GC_CHANGE_VIEW_RUBBERBAND_START);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the current rubber retangle action if there is one.
 */
void CGCView::rubberBandStop(void)
{
  FStates &= ~GC_STATE_RUBBER_BAND;
  FFeedbackLayer->rubberBandStop();
  change(this, GC_CHANGE_VIEW_RUBBERBAND_STOP);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts the rubber rectangle mode.
 *
 * @param style The visual style of the rubber rectangle.
 * @param coords The start coordinates of the rectangle in view space.
 * @param removeSelection If true then the current selection is cleared.
 */
void CGCView::rubberRectStart(TRubberRectStyle style, const TVertex& coords, bool removeSelection)
{
  // Implicitely stops any active rubber rectangle.
  FFeedbackLayer->rubberRectStart(style, coords, removeSelection);
  FStates |= GC_STATE_RUBBER_RECTANGLE;
  change(this, GC_CHANGE_VIEW_RUBBERRECT_START);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Stops the current rubber retangle action if there is one.
 */
void CGCView::rubberRectStop(void)
{
  FStates &= ~GC_STATE_RUBBER_RECTANGLE;
  FFeedbackLayer->rubberRectStop();
  change(this, GC_CHANGE_VIEW_RUBBERRECT_STOP);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the number of currently selected items.
 *
 * @result The number of currently selected items.
 */
int CGCView::selectionCount(void)
{
  return FFeedbackLayer->selectionCount();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the virtual center of the view.
 *
 * @param x The x coordinate of the new center.
 * @param y The x coordinate of the new center.
 * @param z The x coordinate of the new center.
 */
void CGCView::setOrigin(float x, float y, float z)
{
  FOrigin[0] = x;
  FOrigin[1] = y;
  FOrigin[2] = z;

  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new size for the current virtual workspace in this view.
 *
 * @param width The new workspace width.
 * @param height The new workspace height.
 */
void CGCView::workspaceSet(const float width, const float height)
{
  FWorkspaceWidth = width;
  FWorkspaceHeight = height;

  FConnectionLayer->workspaceChanged();

  change(this, GC_CHANGE_VIEW_WORKSPACE);

  // The workspace size may also directly influence the cache.
  #if USE_BSP_TREE
    delete FCache;
    FCache = NULL;
  #endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Gets the workspace size in this view.
 *
 * @param width The workspace width.
 * @param height The workspace height.
 */
void CGCView::workspaceGet(float &width, float &height)
{
  width= FWorkspaceWidth;
  height= FWorkspaceHeight;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Hides or shows the current selection.
 *
 * @param visible If true the selection is shown, otherwise not.
 */
void CGCView::showSelection(bool visible)
{
  FFeedbackLayer->visibleSet(visible);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the new viewport for this view.
 *
 * @param NewViewport The new viewport to be used.
 */
void CGCView::viewportSet(const TGCViewport& newViewport)
{
  FViewport = newViewport;
  glViewport(FViewport.left, FViewport.top, FViewport.width, FViewport.height);
  change(this, GC_CHANGE_VIEW_PROPERTY);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given view coordinates into window coordinates
 * @note This function resets the current projection and view matrices.
 *
 * @param coords A vertex containing view coordiantes.
 * @param x [out] Horizontal window coordinate in pixels.
 * @param y [out] Vertical window coordinate in pixels.
 */
void CGCView::viewToWindow(const TVertex& coords, int& x, int& y)
{
  applyTransformations();

  GLint viewport[4];
  glGetIntegerv(GL_VIEWPORT, viewport);

  GLdouble modelviewMatrix[16];
  glGetDoublev(GL_MODELVIEW_MATRIX, modelviewMatrix);

  GLdouble projectionMatrix[16];
  glGetDoublev(GL_PROJECTION_MATRIX, projectionMatrix);

  double winX, winY, winZ;
  gluProject(coords.x, coords.y, coords.z, modelviewMatrix, projectionMatrix, viewport, &winX, &winY, &winZ);

  x = int(winX);
  y = int(viewport[3] - winY); 
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given window (viewer) coordinates into local (view) space.
 * @note This function resets the current projection and view matrices.
 *
 * @param x Horizontal window coordinate in pixels.
 * @param y Vertical window coordinate in pixels.
 * @param coords [out] A vertex getting the local coordiantes.
 */
void CGCView::windowToView(int x, int y, TVertex& coords)
{
  applyTransformations();

  GLint viewport[4];
  glGetIntegerv(GL_VIEWPORT, viewport);

  GLdouble modelviewMatrix[16];
  glGetDoublev(GL_MODELVIEW_MATRIX, modelviewMatrix);

  GLdouble projectionMatrix[16];
  glGetDoublev(GL_PROJECTION_MATRIX, projectionMatrix);

  double localX, localY, localZ;
  gluUnProject(x, viewport[3] - y, 0, modelviewMatrix, projectionMatrix, viewport, &localX, &localY, &localZ);

  coords = TVertex(localX - FOrigin[0], localY - FOrigin[1], localZ - FOrigin[2]);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current zoom factor.
 *
 * @return The current zoom factor.
 */
float CGCView::zoomGet(void)
{
  return FZoom;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new zoom factor.
 *
 * @param value The new zoom factor.
 */
void CGCView::zoomSet(float value)
{
  FZoom = value;

  notifyZoomChange();
  change(this, GC_CHANGE_VIEW_ZOOM);
  canvas()->refresh();
}

//----------------------------------------------------------------------------------------------------------------------


