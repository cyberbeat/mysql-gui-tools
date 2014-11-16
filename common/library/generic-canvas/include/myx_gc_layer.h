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
 * @file myx_gc_layer.h 
 * @brief Implementation of the GC layer class.
 * 
 */

#ifndef __GC_LAYER_H__
#define __GC_LAYER_H__

#include "myx_gc_base.h"

//----------------------------------------------------------------------------------------------------------------------

class CGenericCanvas;
class CFigureInstance;
class CFigure;
class CConnection;
class CConnectionInstance;

/**
 * This is the base layer class, which is used by views to display their content. There are descendants for special things
 * like feedback, grids and so on.
 */
class GENERIC_CANVAS_API CLayer: public CGCBase
{
  friend class CFigureInstance;
  friend class CFigureInstanceEnumerator;
  friend class CInstanceListener;
  friend class CGCView;
private:
  class CInstanceListener: private CGCBaseListener
  {
    friend class CLayer;
  protected:
    CLayer* layer;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* object);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  wstring FName;                  // The name of the layer.
  bool FDirty;                    // True if any of the properties changed that affect the display list.
  float FScaling[3];              // The factors to scale the layer in each direction.
  float FMinimalScale;            // The lesser of the x and y scale factors.
  float FTranslation[3];          // The factors to move the layer.
  CFigureInstances FInstances;    // A list of figures instances.
  CFigureInstances FGroups;       // A list of groups.
  bool FVisible;                  // true if the layer is visible, otherwise false. Default is true.
  bool FEnabled;                  // True if the layer answers to mouse and keyboard actions.
  CInstanceListener FListener;
protected:
  void applyTransformations();
  void checkError(void);
  void renderFeedback(CFigureInstance* instance);
  virtual void renderLayerContent(TBoundingBox bounds);
  virtual void validateLayerContent(void);
  virtual void zoomChanged(float newZoom);
public:
  CLayer(string name, CGenericCanvas* canvas);
  virtual ~CLayer(void);

  virtual void __cdecl addInstance(CFigureInstance* instance);
  virtual void __cdecl bringToFront(CFigureInstance* instance, CFigureInstance* relativeTo = NULL);
  virtual void __cdecl clear(void);
  virtual CFigureInstance* __cdecl createInstance(CFigure* figure);
  void getHitTestInfoAt(CHitResults* hits, TVertex point, bool singleHit);
  virtual bool __cdecl enabledGet(void);
  virtual void __cdecl enabledSet(bool isEnabled);
  virtual const CFigureInstances *instancesGet();
  virtual void __cdecl layerToView(TVertex original, TVertex& result);
  void makeDirty(void);
  wstring name(void) { return FName; };
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  virtual void __cdecl removeInstance(CFigureInstance* instance);
  virtual void __cdecl render(TBoundingBox bounds);
  virtual void __cdecl scale(float Sx, float Sy, float Sz, bool accumulative = false);
  virtual void __cdecl scaleV(const float Factor[3], bool accumulative = false);
  virtual void __cdecl sendToBack(CFigureInstance* instance, CFigureInstance* relativeTo = NULL);
  virtual void __cdecl translate(float Tx, float Ty, float Tz, bool accumulative = false);
  virtual void __cdecl translateV(const float Factor[3], bool accumulative = false);
  void validate(void);
  virtual void __cdecl viewToLayer(TVertex original, TVertex& result);
  virtual bool __cdecl visibleGet(void);
  virtual void __cdecl visibleSet(bool isVisible);
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * The grid layer is a special layer variant that renders itself as grid.
 */
class GENERIC_CANVAS_API CGridLayer: public CLayer
{
  friend class CGCView;
private:
  CGCView* FView;       // The view to which this layer belongs.
protected:
  virtual void renderLayerContent(TBoundingBox bounds);
public:
  CGridLayer(CGCView* view);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_LAYER_H__
