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
 * @file myx_gc_layer.cpp 
 * @brief Implementation of the GC layer class.
 * 
 */

#include "myx_gc_layer.h"
#include "myx_gc_canvas.h"

//----------------- CInstanceListener ----------------------------------------------------------------------------------

void CLayer::CInstanceListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  layer->action(origin, action);
}

//----------------------------------------------------------------------------------------------------------------------

void CLayer::CInstanceListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  layer->change(origin, reason);
}

//----------------------------------------------------------------------------------------------------------------------

void CLayer::CInstanceListener::onDestroy(CGCBase* object)
{
  layer->removeInstance((CFigureInstance*) object);
}

//----------------------------------------------------------------------------------------------------------------------

void CLayer::CInstanceListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  layer->error(origin, message);
}

//----------------- CLayer ---------------------------------------------------------------------------------------------

CLayer::CLayer(string name, CGenericCanvas* canvas): CGCBase(canvas)
{
  _className = "CLayer";
  FName = utf8ToUtf16(name);

  // Initialize with useful values.
  FScaling[0] = 1;
  FScaling[1] = 1;
  FScaling[2] = 1;

  FTranslation[0] = 0;
  FTranslation[1] = 0;
  FTranslation[2] = 0;

  FDirty = true;
  FVisible = true;
  FEnabled = true;

  FListener.layer = this;
}

//----------------------------------------------------------------------------------------------------------------------

CLayer::~CLayer(void)
{
  beginUpdate();
  if ((canvas() != NULL) && (!canvas()->updating()))
    canvas()->removeLayer(this);
  clear();
  endUpdate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Applies the layer's transformations for rendering, feedback etc.
 */
void CLayer::applyTransformations()
{
  glTranslatef(FTranslation[0], FTranslation[1], FTranslation[2]);
  glScalef(FScaling[0], FScaling[1], FScaling[2]);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the error checking of the canvas.
 */
void CLayer::checkError(void)
{
  canvas()->checkError();
}

//----------------------------------------------------------------------------------------------------------------------

/*
 * Helper method to determine the transformed vertices of the given figure instance. The layer applies its own
 * transformations and only renders the figure instance.
 *
 * @param instance The figure instance for which feedback data is requested.
 */
void CLayer::renderFeedback(CFigureInstance* instance)
{
  applyTransformations();
  instance->render();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders layer content that is not determined by figure instances. This method might be overridden by descendants.
 *
 * @param bounds The area of the layer that only needs to be rendered.
 */
void CLayer::renderLayerContent(TBoundingBox bounds)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Prepares layer content that is not determined by figure instances. This method might be overridden by descendants.
 */
void CLayer::validateLayerContent(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * The zoom in the owner view has changed. Notify figures.
 *
 * @param view The view whose zoom value has changed.
 */
void CLayer::zoomChanged(float newZoom)
{
  for (CFigureInstances::iterator iterator = FInstances.begin(); iterator != FInstances.end(); ++iterator)
  {
    CFigureInstance* instance = *iterator;
    instance->zoomChanged(newZoom);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * If the given figure instance is currently on this layer then it is moved to the place immediately above
 * the relativeTo instance. If relativeTo is 0, then it's made the top most instance (they are rendered as stored in the instances array).
 *
 * @param instance The instance to bring to front.
 * @param relativeTo Instance it should be placed relative to (ie, it will be put above this one)
 */
void CLayer::bringToFront(CFigureInstance* instance, CFigureInstance* relativeTo)
{
  CFigureInstances::iterator figIterator = find(FInstances.begin(), FInstances.end(), instance);
  if (figIterator != FInstances.end())
  {
    FInstances.erase(figIterator);
    
    if (relativeTo != NULL)
    {  
      CFigureInstances::iterator relIterator = find(FInstances.begin(), FInstances.end(), instance);
      
      if (relIterator != FInstances.end())
      {
        ++relIterator; // go to the item next to the one we'll go on top of
        
        if (relIterator == FInstances.end())
          FInstances.push_back(instance); // this is the last one
        else
          FInstances.insert(relIterator, instance); // insert before the next one
        canvas()->refresh();
		return;
      }
    }
    FInstances.push_back(instance);
    canvas()->refresh();
  }
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given figure instance to the end of the instance list. If instance belongs to another layer currently 
 * it is removed from the other's instance list first.
 *
 * @param instance The figure instance to add.
 */
void CLayer::addInstance(CFigureInstance* instance)
{
  if (instance->FLayer != this)
  {
    beginUpdate();
    if (instance->FLayer != NULL)
      instance->FLayer->removeInstance(instance);
    instance->addListener(&FListener);
    FInstances.push_back(instance);
    instance->FLayer = this;
    endUpdate();
    change(instance, GC_CHANGE_LAYER_ADD_INSTANCE);
    makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all figure instances from this layer.
 */
void CLayer::clear(void)
{
  beginUpdate();

  for (CFigureInstances::iterator iterator = FInstances.begin(); iterator != FInstances.end(); ++iterator)
  {
    CFigureInstance* instance = *iterator;
    instance->FLayer = NULL;
    delete instance;
  };
  FInstances.clear();
  endUpdate();

  change(this, GC_CHANGE_LAYER_CLEAR);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new instance for the given figure and adds it to this layer.
 *
 * @param Figure The figure for which the instance is to be created.
 * @return A new figure instance.
 */
CFigureInstance* CLayer::createInstance(CFigure* Figure)
{
  beginUpdate();
  CFigureInstance* instance = new CFigureInstance(this, Figure);
  addInstance(instance);
  endUpdate();

  return instance;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Fills the hit results with all figure instances whose bounds contain the given coordinates.
 *
 * @param hits [out] The hit collection that is updated.
 * @param point The hit point coordinated given in view space.
 * @param singleHit If true only one hit is returned.
 */
void CLayer::getHitTestInfoAt(CHitResults* hits, TVertex point, bool singleHit)
{
  // Bounding boxes of figure instances are already in layer space coordinates. We only need to convert the
  // given hit point to our local coordinate system.
  TVertex localPoint;
  viewToLayer(point, localPoint);

  // Iterate backwards. Figures rendered later lay on top of earlier rendered figures.
  for (CFigureInstances::reverse_iterator iterator = FInstances.rbegin(); iterator != FInstances.rend(); ++iterator)
  {
    CFigureInstance* instance = *iterator;
    if (instance->containsPoint(localPoint.x, localPoint.y))
    {
      hits->addHit(instance);
      if (singleHit)
        break;
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the current enabled state.
 *
 * @return The current enabled state.
 */
bool CLayer::enabledGet(void)
{
  return FEnabled;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the layer's enabled state.
 *
 * @param IsEnabled Set it to true if you want the layer to be visible.
 */
void CLayer::enabledSet(bool IsEnabled)
{
  if (FEnabled != IsEnabled)
  {
	FEnabled = IsEnabled;
	makeDirty();
  };
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Return internal list of instances.
 */
const CFigureInstances *CLayer::instancesGet()
{
  return &FInstances;
}


//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from layer space to view space.
 *
 * @param coords The coordinates to convert to view space.
 * @param result [out] Converted coordinates.
 */
void CLayer::layerToView(TVertex original, TVertex& result)
{
  result.x = original.x * FScaling[0] + FTranslation[0];
  result.y = original.y * FScaling[1] + FTranslation[1];
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks the display list for this layer as invalid, hence it will be recreated next time validate is called.
 * If a list already exists then it is freed.
 */
void CLayer::makeDirty(void)
{
  if (!FDirty)
  {
    FDirty = true;
    if (!destroying() && !updating())
      canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index If the property is a list then this is the index into that list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CLayer::propertyGet(const char* name, unsigned int index)
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
              result = "A collection of figure instances.";
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = canvas();
              break;
            };
          case GC_PROPERTY_VISIBLE:
            {
              result = FVisible;
              break;
            };
          case GC_PROPERTY_ENABLED:
            {
              result = FEnabled;
              break;
            };
        };
        break;
      };
    case GC_CONTAINER_FIGURE_INSTANCES:
	  {
        if (index < FInstances.size())
          result = FInstances[index];
        break;
      };
    case GC_CONTAINER_GROUPS:
      {
        // TODO: Should actually be the groups.
        if (index < FInstances.size())
          result = FInstances[index];
        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
		  result = FScaling[index];
        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
          result = FTranslation[index];
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
 * @param index If the property is a list then this is the index into that list.
 * @param Value The new value of the property. Automatic conversion is performed where possible.
 */
void CLayer::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
		switch (getPropertyID(name))
        {
          case GC_PROPERTY_NAME:
            {
              FName = utf8ToUtf16(value);
              change(this, GC_CHANGE_LAYER_PROPERTY);

              break;
            };
          case GC_PROPERTY_VISIBLE:
            {
              FVisible = value;
              change(this, GC_CHANGE_LAYER_PROPERTY);
              
              break;
            };
          case GC_PROPERTY_ENABLED:
            {
              FEnabled = value;
              change(this, GC_CHANGE_LAYER_PROPERTY);
              
              break;
            };
        };
        break;
      };
    case GC_CONTAINER_SCALING:
      {
        if (index < 3)
        {
          FScaling[index] = value;
          change(this, GC_CHANGE_LAYER_PROPERTY);
        };

        break;
      };
    case GC_CONTAINER_TRANSLATION:
      {
        if (index < 3)
		{
          FTranslation[index] = value;
          change(this, GC_CHANGE_LAYER_PROPERTY);
        };

        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given figure instance from the instance list if it is currently there.
 * No error is raised if the instance does not belong to this layer.
 *
 * @param instance The instance to be removed.
 */
void CLayer::removeInstance(CFigureInstance* instance)
{
  if (!updating() && !destroying())
  {
    beginUpdate();
    for (CFigureInstances::iterator iterator = FInstances.begin(); iterator != FInstances.end(); ++iterator)
      if (*iterator == instance)
      {
        (*iterator)->removeListener(&FListener);
        FInstances.erase(iterator);
        instance->FLayer = NULL;
        change(this, GC_CHANGE_LAYER_REMOVE_INSTANCE);
        makeDirty();
        break;
      };
    endUpdate();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks the validity of the figure display list and executes it.
 *
 * @param bounds The area currently visible. No need to render anything outside that area.
 */
void CLayer::render(TBoundingBox bounds)
{
  if (FVisible && !updating()) 
  {
    glPushMatrix();
    applyTransformations();

    // Transform viewport coordinates into view space. They are used for occlusion culling.
    TBoundingBox visibleBounds;
    viewToLayer(bounds.lower, visibleBounds.lower);
    viewToLayer(bounds.upper, visibleBounds.upper);
    for (CFigureInstances::iterator iterator = FInstances.begin(); iterator != FInstances.end(); ++iterator)
    {
      CFigureInstance* instance = *iterator;
      if (instance->overlaps(visibleBounds))
        instance->render();
    };

    renderLayerContent(visibleBounds);
    glPopMatrix();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scales the layer by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of scale uses single float values as parameters.
 *
 * @param Sx scale factor for the x axis.
 * @param Sy scale factor for the y axis.
 * @param Sz scale factor for the z axis.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CLayer::scale(float Sx, float Sy, float Sz, bool Accumulative)
{
  if (Accumulative)
  {
    FScaling[0] += Sx;
    FScaling[1] += Sy;
    FScaling[2] += Sz;
  }
  else
  {
    FScaling[0] = Sx;
    FScaling[1] = Sy;
    FScaling[2] = Sz;
  };
  change(this, GC_CHANGE_LAYER_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Scales the layer by the amount given in Factor. If Accumulative is true then the new scale factors are multiplied
 * with the existing values. This version of scale uses an array of values in the parameter list.
 *
 * @param Factor An array of 3 scale values, one for each axis.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CLayer::scaleV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
	FScaling[0] += Factor[0];
	FScaling[1] += Factor[1];
	FScaling[2] += Factor[2];
  }
  else
  {
	FScaling[0] = Factor[0];
	FScaling[1] = Factor[1];
	FScaling[2] = Factor[2];
  };
  change(this, GC_CHANGE_LAYER_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * If the given figure instance is currently on this layer then it is moved to the first place in the list making it so
 * the bottom most instance (they are rendered as stored in the instances array).
 *
 * @param instance The instance to send to back.
 * @param relativeTo The instance it should be moved relative to.
 */
void CLayer::sendToBack(CFigureInstance* instance, CFigureInstance* relativeTo)
{
  CFigureInstances::iterator figIterator = find(FInstances.begin(), FInstances.end(), instance);
  if (figIterator != FInstances.end())
  {
    FInstances.erase(figIterator);
    if (relativeTo != NULL)
    {  
      CFigureInstances::iterator relIterator = find(FInstances.begin(), FInstances.end(), instance);
      
      if (relIterator != FInstances.end() && relIterator != figIterator)
      {
        FInstances.insert(relIterator, instance);
        canvas()->refresh();
        return;
      }
    }
    FInstances.insert(FInstances.begin(), instance); // insert before 1st element
    canvas()->refresh();
  }
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the layer by the amount given in Tx, Ty and Tz. If Accumulative is true then the new translation factors 
 * are multiplied with the existing values. This version of translate uses an array for the values in the parameter list.
 *
 * @param Tx scale factor for the x axis.
 * @param Ty scale factor for the y axis.
 * @param Tz scale factor for the z axis.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CLayer::translate(float Tx, float Ty, float Tz, bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Tx;
    FTranslation[1] += Ty;
    FTranslation[2] += Tz;
  }
  else
  {
    FTranslation[0] = Tx;
    FTranslation[1] = Ty;
    FTranslation[2] = Tz;
  };
  change(this, GC_CHANGE_LAYER_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the layer by the amount given in Factor. If Accumulative is true then the new translation factors are multiplied
 * with the existing values. This version of translate uses an array for the values in the parameter list.
 *
 * @param Factor An array of translation values, for each axis one.
 * @param Accumulative If true then the given values are added to any existing values otherwiese they are used as given.
 */
void CLayer::translateV(const float Factor[3], bool Accumulative)
{
  if (Accumulative)
  {
    FTranslation[0] += Factor[0];
    FTranslation[1] += Factor[1];
    FTranslation[2] += Factor[2];
  }
  else
  {
    FTranslation[0] = Factor[0];
    FTranslation[1] = Factor[1];
    FTranslation[2] = Factor[2];
  };
  change(this, GC_CHANGE_LAYER_PROPERTY);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Validates the content of this layer (e.g. appling all changes).
 */
void CLayer::validate(void)
{
  if (FDirty)
  {
    FDirty = false;

    FMinimalScale = FScaling[0] < FScaling[1] ? FScaling[0] : FScaling[1];
    validateLayerContent();

    // Give the instances the opportunity to trigger validation of their associated figures.
    for (CFigureInstances::iterator iterator = FInstances.begin(); iterator != FInstances.end(); ++iterator)
      (*iterator)->validate();

    checkError();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Converts the given coordinates from view space to layer space.
 *
 * @param coords The coordinates to convert to layer space.
 * @param result [oout] Converted coordinates.
 */
void CLayer::viewToLayer(TVertex original, TVertex& result)
{
  result.x = (original.x - FTranslation[0]) / FScaling[0];
  result.y = (original.y - FTranslation[1]) / FScaling[1];
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the visibility state.
 *
 * @return The current visibility state.
 */
bool CLayer::visibleGet(void)
{
  return FVisible;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the layer's visibility state.
 *
 * @param IsVisible Set it to true if you want the layer to be visible.
 */
void CLayer::visibleSet(bool IsVisible)
{
  if (FVisible != IsVisible)
  {
	FVisible = IsVisible;
	change(this, GC_CHANGE_LAYER_VISIBILITY);
  };
}

//----------------- CGridLayer -----------------------------------------------------------------------------------------

CGridLayer::CGridLayer(CGCView* view): CLayer("grid", view->canvas())
{
  _className = "CGridLayer";
  FView = view;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the grid depending on the current display part and the zoom state.
 *
 * @param bounds The area for the grid.
 */
void CGridLayer::renderLayerContent(TBoundingBox bounds)
{
#define Level3Distance   4
#define Level2Distance  20
#define Level1Distance 100

  if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
    glDisable(GL_MULTISAMPLE_ARB);

  bool showLevel2 = FView->zoomGet() >= 0.5;
  bool showLevel3 = FView->zoomGet() >= 2;

  float originX, originY, originZ;

  FView->getOrigin(&originX, &originY, &originZ);

  glColor4f(0.0f, 0.0f, 0.0f, 0.05f);

  if (showLevel3)
  {
    // Third level lines.
    glEnable (GL_LINE_STIPPLE);
    glLineStipple (1, 0x3333);  //  dotted
    glBegin(GL_LINES);

    // The actual paint start points depend on the origin and the area to draw.
    float left = originX - Level3Distance * ceil((originX - bounds.upper.x) / Level3Distance);
    float right = originX + Level3Distance * ceil((bounds.lower.x - originX) / Level3Distance);
    float top = originY - Level3Distance * ceil((originY - bounds.upper.y) / Level3Distance);
    float bottom = originY + Level3Distance * ceil((bounds.lower.y - originY) / Level3Distance);

    for (float y = top; y < bottom; y += Level3Distance)
    {
      glVertex2f(left, y);
      glVertex2f(right, y);
    };
    glVertex2f(left, bottom);
    glVertex2f(right, bottom);

    for (float x = left; x < right; x += Level3Distance)
    {
      glVertex2f(x, top);
      glVertex2f(x, bottom);
    };
    glVertex2f(right, top);
    glVertex2f(right, bottom);
    glEnd();
    glDisable(GL_LINE_STIPPLE);
  };

  if (showLevel2)
  {
    // Second level lines.
    glBegin(GL_LINES);

    // The actual paint start points depend on the origin and the area to draw.
    float left = originX - Level2Distance * ceil((originX - bounds.upper.x) / Level2Distance);
    float right = originX + Level2Distance * ceil((bounds.lower.x - originX) / Level2Distance);
    float top = originY - Level2Distance * ceil((originY - bounds.upper.y) / Level2Distance);
    float bottom = originY + Level2Distance * ceil((bounds.lower.y - originY) / Level2Distance);

    for (float y = top; y < bottom; y += Level2Distance)
    {
      glVertex2f(left, y);
      glVertex2f(right, y);
    };
    glVertex2f(left, bottom);
    glVertex2f(right, bottom);

    for (float x = left; x < right; x += Level2Distance)
    {
      glVertex2f(x, top);
      glVertex2f(x, bottom);
    };
    glVertex2f(right, top);
    glVertex2f(right, bottom);
    glEnd();
  };

  // Top level lines.
  glBegin(GL_LINES);

  // The actual paint start points depend on the origin and the area to draw.
  float left = originX - Level1Distance * ceil((originX - bounds.upper.x) / Level1Distance);
  float right = originX + Level1Distance * ceil((bounds.lower.x - originX) / Level1Distance);
  float top = originY - Level1Distance * ceil((originY - bounds.upper.y) / Level1Distance);
  float bottom = originY + Level1Distance * ceil((bounds.lower.y - originY) / Level1Distance);

  for (float y = top; y < bottom; y += Level1Distance)
  {
    glVertex2f(left, y);
    glVertex2f(right, y);
  };
  glVertex2f(left, bottom);
  glVertex2f(right, bottom);

  for (float x = left; x < right; x += Level1Distance)
  {
    glVertex2f(x, top);
    glVertex2f(x, bottom);
  };
  glVertex2f(right, top);
  glVertex2f(right, bottom);
  glEnd();

  // Finally draw a cross where the origin is located.
  glBegin(GL_LINES);
    glVertex2f(originX, bounds.upper.y);
    glVertex2f(originX, bounds.lower.y);
    glVertex2f(bounds.upper.x, originY);
    glVertex2f(bounds.lower.x, originY);
  glEnd();

  if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
    glEnable(GL_MULTISAMPLE_ARB);
}

//----------------------------------------------------------------------------------------------------------------------

