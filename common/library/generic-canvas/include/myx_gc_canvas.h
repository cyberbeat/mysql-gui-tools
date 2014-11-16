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
 * @file myx_gc_canvas.h 
 * @brief Generic canvas main class.
 * 
 */

#ifndef __GC_CANVAS_H__
#define __GC_CANVAS_H__

#include "myx_gc_view.h"
#include "myx_gc_font_manager.h"
#include "myx_gc_svgparser.h"
#include "myx_gc_figure_parser.h"
#include "myx_gc_animation.h"

//----------------------------------------------------------------------------------------------------------------------

class CFigure;
class CFigureInstance;
class CLayer;
class CGenericCanvas;
class CGCView;
class CConnection;
class CAnimationManager;

//----------------- CGenericCanvas -------------------------------------------------------------------------------------

/** States the canvas can enter. */
#define GC_STATE_PENDING_ACTIVATION 0x0001

/**
 * CGenericCanvas is the main class of the library and is the base for all further functionality (e.g. it creates and 
 * maintains the model). Instances are created via the exported CreateGenericCanvas function (if called from non C++ 
 * languages). CGenericCanvas serves as the controller in the model-view-controller pattern, which is used here and 
 * communicates with the viewer via callbacks.
 * The viewer is platform specific and must be implemented individually. It is responsible to create a canvas controller class.
 *
 * @see CreateGenericCanvas
 */
class GENERIC_CANVAS_API CGenericCanvas: public CGCBase 
{
  friend class CGCBase;
  friend class CGCModel;
  friend class CFeedbackLayer;
private:
  class CCanvasListener: public CGCBaseListener
  {
    friend class CGenericCanvas;
  protected:
    CGenericCanvas* canvas;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  wstring FName;
  GCContext FContext;                  // The OpenGL rendering cntext (platform specific).
  CGCModel* FModel;                    // The model this canvas is controlling.
  CLayers FLayers;                     // A list of layers currently in this canvas.
  CViews FViews;                       // A list of views that can be used by this canvas.
  CGCView* FCurrentView;               // The currently active set of layers.
  unsigned int FStates;               // Persistent storage for certain states in the canvas.
  CCanvasListener FListener;           // Listener used to have the canvas listen to changes and error messages from children.
  CAnimationManager* FAnimationManager;
#ifdef _WINDOWS
  CRITICAL_SECTION FLock;
#else
  GStaticRecMutex FLock;               // A lock to synchronize access to canvas objects.
#endif
  TOglExtensions FExtensions;          // A list of the support state of certain OpenGL extensions.
  bool FFocused;                       // Canvas has the input focus and should display a focus indicator (if enabled).
protected:
  void determineExtensions(void);
  void lock(void);
  void unlock(void);
public:
  CGenericCanvas(GCContext Context, wstring name);
  virtual ~CGenericCanvas(void);

  virtual void __cdecl addLayer(CLayer* layer);
  virtual TGCError __cdecl addLayoutsFromFile(const char* filename);
  virtual TGCError __cdecl addStylesFromFile(const char* filename, const char* variables);
  TGCError addStylesFromFile(const char* filename, map<wstring, wstring>& variables);
  TGCError addStylesFromFile(const char* filename, map<string, string>& variables);
  virtual TGCError __cdecl addStyleFromDefinition(const char* definition);
  virtual CAnimationManager* __cdecl animationManager(void) { return FAnimationManager; };
  virtual void __cdecl checkError(void);
  void clearBuffers(void);
  virtual void __cdecl clearContent(void);
  virtual void __cdecl clearLayouts(void);
  virtual void __cdecl clearStyles(void);
  virtual CConnection* __cdecl createConnection(const char* type, const char* layoutClass, CFigure* endPoint1, 
    CFigure* endPoint2);
  virtual CFigure* __cdecl createFigure(const char* type, const char* layoutClass);
  virtual CLayer* __cdecl createLayer(const char* name, bool addToCurrentView);
  virtual CGCView* __cdecl createView(const char* name);
  virtual CGCView* __cdecl currentViewGet(void);
  virtual void __cdecl currentViewSet(CGCView* View);
  virtual bool __cdecl focusedGet(void);
  virtual void __cdecl focusedSet(bool isFocused);
  CGCModel* getModel(void) { return FModel; }; // For special use only. Speaking MVC pattern, the viewer should not access the model!
  virtual CLayer* __cdecl layerByName(const char* name);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  virtual void __cdecl refresh(void);
  virtual void __cdecl removeLayer(CLayer* layer);
  virtual void __cdecl removeStyle(const char* name);
  virtual void __cdecl removeView(CGCView* View);
  virtual void __cdecl render(TGCRenderContent content);
  virtual bool __cdecl renderToFile(const char* filename, TGCFileFormat format, const char* title, const char* software, 
    TGCRenderContent content, float zoom, TGCViewport& bounds);
  virtual bool __cdecl renderToMemory(unsigned char* memory, TGCColorFormat format, TGCRenderContent content, float zoom, 
    TGCViewport& bounds);
  virtual void __cdecl setTexturePath(const char* path);
  virtual bool __cdecl supportsExtension(TOglExtension extension);
  virtual CGCView* __cdecl viewByName(const char* name);
};

//----------------------------------------------------------------------------------------------------------------------

// Factory function to create a generic canvas. This function is exported and must be used by the viewer implementations
// to actually create a canvas instance. This is the only way to get hold of a generic canvas instance for non-C++ languages.
extern "C" GENERIC_CANVAS_API CGenericCanvas* CreateGenericCanvas(GCContext Context, char* name);

#endif // __GC_CANVAS_H__
