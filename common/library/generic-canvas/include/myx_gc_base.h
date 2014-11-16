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
 * @file myx_gc_base.h 
 * @brief Implementation of the GC base class from which most other GC classes are derived.
 * 
 */

#ifndef __GC_BASE_H__
#define __GC_BASE_H__

class CGCListener;

#include "myx_gc_datatypes.h"
#include "myx_gc_utilities.h"

#define USE_BSP_TREE 0  // In highly dynamic scenes like this canvas a BSP is not the optimal choice.
                        // Lookup is fast but it must frequently be rebuilt, which costs a lot of time.

#if USE_BSP_TREE
  #include "myx_gc_bsp.h"
  class CBspTree;
#endif

//----------------------------------------------------------------------------------------------------------------------

class CGCBase;
class CGenericCanvas;

/**
 * The general listener class is used to notify users of the canvas about general events like repaints and errors.
 * This class is only an abstract class and must get a concrete implemention in the application.
 * All Listener classes are meant to be a means for calling back the application. They are implemented and instantiated
 * in the application and must be freed there. Don't forget to remove the listener class before you free it!
 */
class GENERIC_CANVAS_API CGCListenerInterface
{
public:
  virtual ~CGCListenerInterface() {};
  virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action) = 0;
  virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason) = 0;
  virtual void __cdecl onDestroy(CGCBase* sender) = 0;
  virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message) = 0;
};

typedef set<CGCListenerInterface*> CGCListeners;
typedef set<CGCListenerInterface*>::iterator CGCListenerIterator;

/**
 * Basic implementation of a listener. Can be used as ancestor to allow overriding of only
 * those functions that are necessary for a particular class.
 */
class GENERIC_CANVAS_API CGCBaseListener: public CGCListenerInterface
{
  virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action) {};
  virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason) {};
  virtual void __cdecl onDestroy(CGCBase* sender) {};
  virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message) {};
};

/**
 * CGCBase serves as general base class for all generic canvas classes.
 */
class GENERIC_CANVAS_API CGCBase
{
  friend class CGenericCanvas;
private:
  CGenericCanvas* FCanvas;   // Reference to the owning canvas.
  CGCListeners FListeners;
  bool FDestroying;
  int FUpdateCount;
  bool FDoEvents;
  void* FUserData;
protected:
  string _className; // Used to determine the actual class.
  void setDestroying(void) { FDestroying = true; }; // Helper to set destroying state explicitely.
public:
  CGCBase(CGenericCanvas* canvas); 
  virtual ~CGCBase(void);

  virtual void __cdecl action(CGCBase* origin, TAction** action);
  virtual void __cdecl addListener(CGCListenerInterface* listener); 
  virtual void __cdecl beginUpdate(void);
  virtual CGenericCanvas* __cdecl canvas(void) { return FCanvas; };
  virtual void __cdecl change(CGCBase* origin, TGCChangeReason reason);
  virtual bool __cdecl classIs(const char* className);
  virtual const char* __cdecl className(void) { return _className.c_str(); };
  virtual bool __cdecl destroying(void) { return FDestroying; };
  virtual void __cdecl disableEvents(void) { FDoEvents = false; };
  virtual void __cdecl enableEvents(void) { FDoEvents = true; };
  virtual void __cdecl endUpdate(void);
  virtual void __cdecl error(CGCBase* origin, const char* message);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index) = 0;
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value) = 0;
  virtual void __cdecl release(void) { delete this; }; 
  virtual void __cdecl removeListener(CGCListenerInterface* listener);
  virtual bool __cdecl updating(void) { return FUpdateCount > 0; };
  virtual void* __cdecl userDataGet(void) { return FUserData; };
  virtual void __cdecl userDataSet(void* newData) { FUserData = newData; };
};

//----------------- CGraphicElement ------------------------------------------------------------------------------------

/**
 * CGraphicElement is the base class for all displayable elements in the generic canvas. It includes management for
 * certain common things like bounding boxes.
 */
class GENERIC_CANVAS_API CGraphicElement: public CGCBase
{
  friend class CAnimation;
private:
  TBoundingBox FBounds;
  CBoundingBoxComputer FBBComputer;
  bool FBoundsValid;
  bool FDirty;
  bool FInAnimation;
protected:
  void animationStarted(void);
  void animationStopped(void);
  void boundsAdd(const TBoundingBox& otherBounds);
  void boundsAdd(const TBoundingBox& otherBounds, TMatrix matrix);
  void boundsAdd(const TBoundingBox& otherBounds, const TVertex& offset);
  void boundsAdd(const TVertex& vertex);
  void boundsAdd(const TVertex& vertex, TMatrix matrix);
  void boundsAdd(const TVertex& vertex, const TVertex& offset);
  void boundsFinished(void);
  void boundsFinished(TConstraints constraints);
  void boundsNew(void);
  void boundsUse(TBoundingBox newBounds);
public:
  CGraphicElement(CGenericCanvas* canvas);

  virtual TBoundingBox __cdecl bounds(void);
  virtual bool __cdecl containsPoint(const float x, const float y);
  virtual bool __cdecl dirty(void) {return FDirty; };
  virtual TFeedbackInfo __cdecl getFeedbackInfo(TModifiers modifiers, const TVertex& coords);
  virtual float __cdecl height(void);
  virtual void __cdecl makeDirty(TGCChangeReason reason = GC_CHANGE_NONE);
  virtual bool __cdecl overlaps(const TBoundingBox& box);
  virtual void __cdecl validate(void);
  virtual bool __cdecl visible(void) { return true; };
  virtual float __cdecl width(void);
};

//----------------- CBaseTemplate --------------------------------------------------------------------------------------

/**
 * An base class used for certain templates. It keeps actions and provides a common interface for the parser.
 */
class CBaseTemplate
{
private:
  CActions FActions;    // Associated actions for the element.
public:
  virtual ~CBaseTemplate(void);

  CActions& actions(void) { return FActions; };
  void addAction(const TAction& action);
};

//----------------- Hit testing structures -----------------------------------------------------------------------------

typedef vector<CGraphicElement*> THitEntries;
typedef vector<CGraphicElement*>::iterator THitEntryIterator;

/**
 * The CHitResult class is used to collect a number of graphic elements that are located at a given point in the view.
 *
 * @note Never hold the given hit results record for a long time. The referenced elements may disappear at any time.
 */
class GENERIC_CANVAS_API CHitResults         
{
private:
  THitEntries FEntries;
  THitEntryIterator FCurrentEntry;
public:
  CHitResults(void);
  virtual ~CHitResults(void);

  void addHit(CGraphicElement* instance);
  virtual int __cdecl count(void);
  virtual bool __cdecl hasNext(void);
  virtual CGraphicElement* __cdecl next(void);
  virtual void __cdecl release(void);
  virtual void __cdecl reset(void);
};

//----------------- CElementLookupCache --------------------------------------------------------------------------------

/**
 * The element lookup cache is a class used to quickly find graphical elements by position. Depending on the settings
 * it can use different implementations.
 * Currently either a BSP tree is used or linear lists. Surprisingly, linear lists seem to be much faster in the GC
 * due to the dynamic character of it. This requires frequent rebuilds, which costs a lot in a BSP tree.
 */
class CElementLookupCache
{
private:
  bool FValid;
#if USE_BSP_TREE
  CBspTree* FCache;
#else
  CGraphicElementList FCache;
#endif
public:
  CElementLookupCache(float width, float height);
  virtual ~CElementLookupCache(void);

  void addElement(CGraphicElement* element);
  CGraphicElement* findElement(TVertex point);
  void findElements(TVertex point, CHitResults* hits);
  void invalidate(void);
  bool isValid(void) { return FValid; };
  void validate(void);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_BASE_H__
