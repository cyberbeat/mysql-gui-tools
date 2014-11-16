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
 * @file myx_gc_base.cpp 
 * @brief Implementation of the base generic canvas class.
 * 
 */

#include "myx_gc_base.h"
#include "myx_gc_canvas.h"

//----------------- CGCBase --------------------------------------------------------------------------------------------

CGCBase::CGCBase(CGenericCanvas* canvas) 
{ 
  _className = "CGCBase"; 
  FDestroying = false;
  FCanvas = canvas;
  FUpdateCount = 0;
  FDoEvents = true;
  FUserData = NULL;
};

//----------------------------------------------------------------------------------------------------------------------

CGCBase::~CGCBase(void)
{
  // Notify listers about the destruction. This cannot be prevented by the FDoEvents flag.
  FDestroying = true;
  for (CGCListeners::reverse_iterator iterator = FListeners.rbegin(); iterator != FListeners.rend(); ++iterator)
  {
    CGCListenerInterface* listener = *iterator;
    listener->onDestroy(this);
  };
  FListeners.clear();
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the onAction event of all registered listeners to notfied them about a particular action.
 *
 * @param origin The object where the action was triggered.
 * @param action [in, out] The action which is about to be executed? Can be changed by a listener.
 */
void CGCBase::action(CGCBase* origin, TAction** action)
{
  if (FDoEvents)
  {
    CGCListenerIterator iterator = FListeners.begin(); 
    while (iterator != FListeners.end())
    {
      CGCListenerIterator next = iterator; ++next;
      try
      {
        (*iterator)->onAction(this, origin, action);
        if (action == NULL)
          break;
      }
      catch(...)
      {
        // If there was an exception while executing the method the listener is removed from our list
        // to avoid further harm.
        FListeners.erase(iterator);
      };
      iterator = next;
    };
  };
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a listener to the internal list of listeners, if it is not already there.
 *
 * @param listener The new listener to add to the internal list. A listener is only added once.
 */
void CGCBase::addListener(CGCListenerInterface* listener) 
{ 
  if (!FDestroying)
    if (FListeners.find(listener) == FListeners.end()) 
      FListeners.insert(listener); 
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Increases the update count by 1 to stop any recursive update until (@see endUpdate()) was called.
 */
void CGCBase::beginUpdate(void)
{
  // Acquire yet another lock before accessing the update count.
  // If the current thread has already a lock then nothing will happen, otherwise the
  // thread will be blocked here until the lock owner releases the lock.
  if (FCanvas != NULL)
    FCanvas->lock();

  ++FUpdateCount;

  if (FCanvas != NULL)
  {
    if (FCanvas != this)
      FCanvas->beginUpdate();
    else
      // Place the main lock if we just have set the first update lock.
      if (FUpdateCount == 1)
        FCanvas->lock();
  };

  // Release the update count lock.
  if (FCanvas != NULL)
    FCanvas->unlock();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the onCange event of all registered listeners to notfied them about a particular change.
 *
 * @param origin The object where the change happend.
 * @param reason What was the reason of that change?
 */
void CGCBase::change(CGCBase* origin, TGCChangeReason reason)
{
  if (FDoEvents)
  {
    CGCListenerIterator iterator = FListeners.begin(); 
    while (iterator != FListeners.end())
    {
      CGCListenerIterator next = iterator; ++next;
      try
      {
        (*iterator)->onChange(this, origin, reason);
      }
      catch(...)
      {
        // If there was an exception while executing the method the listener is removed from our list
        // to avoid further harm.
        FListeners.erase(iterator);
      };
      iterator = next;
    };
  };
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if this class is of a specific type by comparing its class name to the given name.
 *
 * @param className Name to compare.
 */
bool CGCBase::classIs(const char* className)
{
  return strcmp(_className.c_str(), className) == 0;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * The counterpart to (@see beginUpdate). It releases one update lock and also the global lock if the count drops to 0.
 */
void CGCBase::endUpdate(void)
{
  // Acquire yet another lock before accessing the update count.
  // If the current thread has already a lock then nothing will happen, otherwise the
  // thread will be blocked here until the lock owner releases the lock.
  if (FCanvas != NULL)
    FCanvas->lock();

  if (FUpdateCount > 0)
  {
    --FUpdateCount;
    if (FCanvas != NULL)
    {
      // Release one update lock.
      if (FCanvas != this)
        FCanvas->endUpdate();
      else
        if (FUpdateCount == 0)
          FCanvas->unlock();
    };
  };

  // Release the update count lock.
  if (FCanvas != NULL)
    FCanvas->unlock();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the onError event of all registered listeners to notfied them about an error.
 *
 * @param origin The object where the error happend.
 * @param message A message describing the error.
 */
void CGCBase::error(CGCBase* origin, const char* message)
{
  if (FDoEvents)
  {
    CGCListenerIterator iterator = FListeners.begin(); 
    while (iterator != FListeners.end())
    {
      CGCListenerIterator next = iterator; ++next;
      try
      {
        (*iterator)->onError(this, origin, message);
      }
      catch(...)
      {
        // If there was an exception while executing the method the listener is removed from our list
        // to avoid further harm.
        FListeners.erase(iterator);
      };
      iterator = next;
    };
  };
};

//----------------------------------------------------------------------------------------------------------------------

void CGCBase::removeListener(CGCListenerInterface* listener)
{
  if (!FDestroying)
    FListeners.erase(listener);
};

//----------------- CGraphicElement ------------------------------------------------------------------------------------

CGraphicElement::CGraphicElement(CGenericCanvas* canvas):CGCBase(canvas)
{ 
  _className = "CGraphicElement"; 
  FBoundsValid = false;
  FDirty = true;
  FInAnimation = false;
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the animation manager when an animation of this element starts freshly or after a suspension.
 */
void CGraphicElement::animationStarted(void)
{
  FInAnimation = true;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the animation manager when an animation of this element is stopped (either finished or suspended).
 */
void CGraphicElement::animationStopped(void)
{
  FInAnimation = false;
  change(this, GC_CHANGE_FINSTANCE_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param otherBounds Coordinates to add.
 */
void CGraphicElement::boundsAdd(const TBoundingBox& otherBounds)
{
  FBBComputer.include(NULL, otherBounds);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param otherBounds Coordinates to add.
 * @param matrix A matrix to transform the given bounds before adding.
 */
void CGraphicElement::boundsAdd(const TBoundingBox& otherBounds, TMatrix matrix)
{
  FBBComputer.include(matrix, otherBounds);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param otherBounds Coordinates to add.
 * @param offset An offset to be added to the box.
 */
void CGraphicElement::boundsAdd(const TBoundingBox& otherBounds, const TVertex& offset)
{
  TBoundingBox newBounds;
  newBounds.lower = TVertex(otherBounds.lower.x + offset.x, otherBounds.lower.y + offset.y, otherBounds.lower.z + offset.z);
  newBounds.upper = TVertex(otherBounds.upper.x + offset.x, otherBounds.upper.y + offset.y, otherBounds.upper.z + offset.z);
  FBBComputer.include(NULL, newBounds);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param vertex Coordinates to add.
 */
void CGraphicElement::boundsAdd(const TVertex& vertex)
{
  FBBComputer.include(NULL, vertex);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param vertex Coordinates to add.
 * @param matrix Transformation matrix to be applied to the given vertex before adding it to the bounds rect.
 */
void CGraphicElement::boundsAdd(const TVertex& vertex, TMatrix matrix)
{
  FBBComputer.include(matrix, vertex);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds new coordinates to the bounds computer.
 *
 * @param vertex Coordinates to add.
 * @param offsetX An offset to be added to the vertex.
 * @param offsetY An offset to be added to the vertex.
 */
void CGraphicElement::boundsAdd(const TVertex& vertex, const TVertex& offset)
{
  FBBComputer.include(NULL, TVertex(vertex.x + offset.x, vertex.y + offset.y, vertex.z + offset.z));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Finishes the current bounding box computation cycle and determines the final box.
 */
void CGraphicElement::boundsFinished(void)
{
  FBounds = FBBComputer.boundingBox();
  FBoundsValid = true;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Finishes the current bounding box computation cycle and determines the final box.
 * Additionally, the bounds are tested against the given constraints and adjusted if necessary.
 *
 * @param constraints The constraints to apply to the current bounding box.
 */
void CGraphicElement::boundsFinished(TConstraints constraints)
{
  FBounds = FBBComputer.boundingBox();

  if (constraints.maxHeight > -1)
  {
    if (FBounds.lower.y - FBounds.upper.y > constraints.maxHeight)
      FBounds.lower.y = FBounds.upper.y + constraints.maxHeight;
  };
  if (constraints.maxWidth > -1)
  {
    if (FBounds.lower.x - FBounds.upper.x > constraints.maxWidth)
      FBounds.lower.x = FBounds.upper.x + constraints.maxWidth;
  };
  if (constraints.minHeight > -1)
  {
    if (FBounds.lower.y - FBounds.upper.y < constraints.minHeight)
      FBounds.lower.y = FBounds.upper.y + constraints.minHeight;
  };
  if (constraints.minWidth > -1)
  {
    if (FBounds.lower.x - FBounds.upper.x < constraints.minWidth)
      FBounds.lower.x = FBounds.upper.x + constraints.minWidth;
  };
  FBoundsValid = true;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Starts a new bounding box computation cycle.
 */
void CGraphicElement::boundsNew(void)
{
  FBoundsValid = false;
  FBBComputer.reset();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper function to set the bounds of this element in one step. Usually, the bounds are computed from vertex data,
 * however certain classes like CCaptionElement get their bounds from other sources.
 *
 * @param newBounds The bounds to be used now.
 */
void CGraphicElement::boundsUse(TBoundingBox newBounds)
{
  FBoundsValid = true;
  FBounds = newBounds;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current bounds. If there is currently a bounding box computation in progress then this function finishes
 * this mode before returning. Additionally, if the object is marked as dirty then validation is triggered too.
 *
 * @return The current bounding box.
 */
TBoundingBox CGraphicElement::bounds(void)
{
  if (FDirty)
    validate();
  if (!FBoundsValid)
    boundsFinished();

  return FBounds;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines if the current bounds contain the given point. Validation is done and bounding box computation is finished
 * if necessary before the test is performed.
 */
bool CGraphicElement::containsPoint(const float x, const float y)
{
  if (FDirty)
    validate();
  if (!FBoundsValid)
    boundsFinished();

  return boundsContainPoint(FBounds, x, y);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Overwritten by descendants to determine more specifically what action they could execute at the specified location.
 * This method is mainly used to determine visual feedback for user in a frequent manner (e.g. cursor image), so
 * actual implementations should be fast.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The coordinates in space of the figure to which this element belongs.
 * @return Returns GC_FI_NONE indicating so no action can take place. Descendants can return more specific values.
 */
TFeedbackInfo CGraphicElement::getFeedbackInfo(TModifiers modifiers, const TVertex& coords)
{
  return GC_FI_NONE;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current height. If there is currently a bounding box computation in progress then this function finishes
 * this mode before returning. Additionally, if the object is marked as dirty then validation is triggered too.
 *
 * @return The current height.
 */
float CGraphicElement::height(void)
{
  if (FDirty)
    validate();
  if (!FBoundsValid)
    boundsFinished();

  return FBounds.lower.y - FBounds.upper.y;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the object as dirty. Optionally, a reason can be given causing a change event to be triggered.
 *
 * @param reason An optional change reason.
 */
void CGraphicElement::makeDirty(TGCChangeReason reason)
{
  if (!FDirty)
  {
    FDirty = true;
    if (reason != GC_CHANGE_NONE)
      change(this, reason);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines whether this instance lies fully or partially within the given box.
 *
 * @param Box The coordinates to check against to learn if this figure instances is in.
 * @return True if the bounds of this instance at least partially overlap the given box bounds.
 */
bool CGraphicElement::overlaps(const TBoundingBox& box)
{
  return boundsIntersect(box, FBounds);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the object as clean. This function should be overwritten by descendants to perform additional validation tasks.
 */
void CGraphicElement::validate(void)
{
  FDirty = false;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current width. If there is currently a bounding box computation in progress then this function finishes
 * this mode before returning. Additionally, if the object is marked as dirty then validation is triggered too.
 *
 * @return The current width.
 */
float CGraphicElement::width(void)
{
  if (FDirty)
    validate();
  if (!FBoundsValid)
    boundsFinished();

  return FBounds.lower.x - FBounds.upper.x;
}

//----------------- CBaseTemplate --------------------------------------------------------------------------------------

CBaseTemplate::~CBaseTemplate(void)
{
  for (CActions::iterator iterator = FActions.begin(); iterator != FActions.end(); ++iterator)
    delete *iterator;
}

//----------------------------------------------------------------------------------------------------------------------

void CBaseTemplate::addAction(const TAction& action)
{
  TAction* newAction = new TAction();
  newAction->type = action.type;
  newAction->feedbackInfo = action.feedbackInfo;
  if (action.type == GC_ACTION_APPLICATION && action.parameters.size() > 0)
  {
    // Application defined action. Convert the first parameter (if there is one) to a number (if possible) and
    // add this value to the action type value. This way avoid frequent conversions and can just pass the
    // modfied action type along the notification chain.
    wstring s = action.parameters[0];
    size_t size = wcstombs(NULL, s.c_str(), 0);
    if (size > 0)
    {
      char* buffer = new char[size + 1];
      wcstombs(buffer, s.c_str(), size);
      buffer[size] = '\0';
      int value = atoi(buffer);
      delete [] buffer;
      if (value < GC_ACTION_APPLICATION)
        newAction->type = TActionType(GC_ACTION_APPLICATION + value);
      else
        newAction->type = TActionType(value);
    };
  };

  for (CActionParameters::const_iterator iterator = action.parameters.begin(); iterator != action.parameters.end(); ++iterator)
    newAction->parameters.push_back(*iterator);
  for (CTriggers::const_iterator iterator = action.triggers.begin(); iterator != action.triggers.end(); ++iterator)
    newAction->triggers.push_back(*iterator);

  FActions.push_back(newAction);
}

//----------------- CHitResults ----------------------------------------------------------------------------------------

CHitResults::CHitResults(void)
{
  FCurrentEntry = FEntries.end();
}

//----------------------------------------------------------------------------------------------------------------------

CHitResults::~CHitResults(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

void CHitResults::addHit(CGraphicElement* instance)
{
  FEntries.push_back(instance);
}

//----------------------------------------------------------------------------------------------------------------------

int CHitResults::count(void)
{
  return (int) FEntries.size();
}

//----------------------------------------------------------------------------------------------------------------------

bool CHitResults::hasNext(void)
{
  return FCurrentEntry != FEntries.end();
}

//----------------------------------------------------------------------------------------------------------------------

CGraphicElement* CHitResults::next(void)
{
  if (FCurrentEntry == FEntries.end())
    return NULL;
  else
    return *FCurrentEntry++;
}

//----------------------------------------------------------------------------------------------------------------------

void CHitResults::release(void)
{
  delete this;
}

//----------------------------------------------------------------------------------------------------------------------

void CHitResults::reset(void)
{
  FCurrentEntry = FEntries.begin();
}

//----------------- CElementLookupCache --------------------------------------------------------------------------------

CElementLookupCache::CElementLookupCache(float width, float height)
{
  FValid = false;
#if USE_BSP_TREE
  FCache = new CBspTree(width, height);
#else
#endif
}

//----------------------------------------------------------------------------------------------------------------------

CElementLookupCache::~CElementLookupCache(void)
{
#if USE_BSP_TREE
  delete FCache;
#else
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a new element to the cache.
 *
 * @param element The element to add.
 */
void CElementLookupCache::addElement(CGraphicElement* element)
{
#if USE_BSP_TREE
  FCache->addElement(element);
#else
  FCache.push_back(element);
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the first (top most) element in the cache at the given position or NULL if there is none.
 *
 * @param point The position to look at (view space).
 * @return NULL or the top most element at that position.
 */
CGraphicElement* CElementLookupCache::findElement(TVertex point)
{
  if (FValid)
  {
#if USE_BSP_TREE
    return FCache->findElement(point);
#else
    for (CGraphicElementList::reverse_iterator iterator = FCache.rbegin(); iterator != FCache.rend(); ++iterator)
    {
      CGraphicElement* element = *iterator;
      if (element->visible() && element->containsPoint(point.x, point.y))
        return *iterator;
    };
#endif
  }
  return NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Searches the cache for all elements that are visible and have bounds covering the given point.
 *
 * @param point The position for which elements are sought.
 * @param singleHit If true the funtion returns after the first hit was found.
 * @param hits A class getting all hits.
 */
void CElementLookupCache::findElements(TVertex point, CHitResults* hits)
{
  if (FValid)
  {
#if USE_BSP_TREE
    FCache->findElements(point, hits);
#else
    for (CGraphicElementList::reverse_iterator iterator = FCache.rbegin(); iterator != FCache.rend(); ++iterator)
    {
      CGraphicElement* element = *iterator;
      if (element->visible() && element->containsPoint(point.x, point.y))
        hits->addHit(*iterator);
    };
#endif
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Invalidates the current cache. What this means depends on the actually used implementation, but generally you 
 * cannot search elements as long as the cache is invalid.
 */
void CElementLookupCache::invalidate(void)
{
  FValid = false;
#if USE_BSP_TREE
  FCache->clear();
#else
  FCache.clear();
#endif
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the cache as valid. This is called by the owner once it has everything filled in.
 */
void CElementLookupCache::validate(void)
{
  FValid = true;
#if USE_BSP_TREE
#else
#endif
}

//----------------------------------------------------------------------------------------------------------------------

