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
 * @file myx_gc_connection.cpp 
 * @brief Implementation of the connection class.
 * 
 */

#include "myx_gc_connection.h"
#include "myx_gc_canvas.h"

//----------------------------------------------------------------------------------------------------------------------

CConnectionDecor::CConnectionDecor(CGCModel* model, wstring type, wstring layoutClass): CGCBase(model->canvas())
{
  _className = "CConnectionDecor";
  FModel = model;
  FType = type;
  FClass = layoutClass;
}

//----------------------------------------------------------------------------------------------------------------------

CConnectionDecor::~CConnectionDecor(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index The index of the sub property to return if it is located in a list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CConnectionDecor::propertyGet(const char* name, unsigned int index)
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
              result = utf16ToUtf8(FType);
              break;
            };
          case GC_PROPERTY_OWNER:
            {
              result = FModel;
              break;
            };
        };

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
 * @param index The index of the sub property to return if it is located in a list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CConnectionDecor::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // There are currently no properties that could be changed. The name is a unique identifier and must not be changed.
}

//----------------- CFigureListener ------------------------------------------------------------------------------------

void CConnection::CFigureListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  // Do not forward actions triggered in the figures. They are handled on their own.
}

//----------------------------------------------------------------------------------------------------------------------

void CConnection::CFigureListener::onDestroy(CGCBase* object)
{
  connection->figureDestroyed((CFigure*) object);
}

//----------------------------------------------------------------------------------------------------------------------

void CConnection::CFigureListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  // Let the model report any errors up the chain. This is not our business.
}

//----------------- CConnection ----------------------------------------------------------------------------------------

CConnection::CConnection(CGenericCanvas* canvas, CConnectionDecor* decor, CFigure* endPoint1, CFigure* endPoint2): 
  CGraphicElement(canvas)
{
  _className = "CConnection";
  FDecor = decor;
  FListener.connection = this;
  FEnd1 = endPoint1;
  FEnd1->addListener(&FListener);
  FEnd2 = endPoint2;
  FEnd2->addListener(&FListener);

  FDecorations.end1Decoration = NULL;
  FDecorations.end1Label = NULL;
  FDecorations.end2Decoration = NULL;
  FDecorations.end2Label = NULL;
  FDecorations.centerDecoration = NULL;
  FDecorations.centerLabel = NULL;
}

//----------------------------------------------------------------------------------------------------------------------

CConnection::~CConnection(void)
{
  if (FEnd1)
    FEnd1->removeListener(&FListener);

  if (FEnd2)
    FEnd2->removeListener(&FListener);

  if (FDecorations.end1Decoration != NULL)
    FDecorations.end1Decoration->removeListener(&FListener);
  if (FDecorations.end1Label != NULL)
    FDecorations.end1Label->removeListener(&FListener);
  if (FDecorations.end2Decoration != NULL)
    FDecorations.end2Decoration->removeListener(&FListener);
  if (FDecorations.end2Label != NULL)
    FDecorations.end2Label->removeListener(&FListener);
  if (FDecorations.centerDecoration != NULL)
    FDecorations.centerDecoration->removeListener(&FListener);
  if (FDecorations.centerLabel != NULL)
    FDecorations.centerLabel->removeListener(&FListener);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Core action execution function.
 *
 * @param action The action to be executed.
 * @return The actual action that was executed (can be changed by the application in a callback, but is usually what was passed in).
 */
TAction* CConnection::doAction(TAction* action)
{                                                               
  TAction* result = action;

  // Notify listeners about the action and see if they confirm it.
  // Listeners can set the action to something else (including GC_ACTION_NONE) to control what happens.
  this->action(this, &result);
  if (result != NULL)
  {
    switch (result->type)
    {
      case GC_ACTION_CHANGE_STYLE:
        {
          // Just a place holder. Remove that once an action is executed here.
          break;
        };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets all decoration info in one call.
 *
 * @param data A list of CFigure instances that are to be used as decoration for the connection.
 *             Some or all entries can be NULL indicating so there should be no decoration for the corresponding part.
 */
void CConnection::decorationsSet(const TConnectionDecorations& data)
{
  if (FDecorations.end1Decoration != NULL)
    FDecorations.end1Decoration->removeListener(&FListener);
  if (FDecorations.end1Label != NULL)
    FDecorations.end1Label->removeListener(&FListener);
  if (FDecorations.end2Decoration != NULL)
    FDecorations.end2Decoration->removeListener(&FListener);
  if (FDecorations.end2Label != NULL)
    FDecorations.end2Label->removeListener(&FListener);
  if (FDecorations.centerDecoration != NULL)
    FDecorations.centerDecoration->removeListener(&FListener);
  if (FDecorations.centerLabel != NULL)
    FDecorations.centerLabel->removeListener(&FListener);

  FDecorations = data;

  if (FDecorations.end1Decoration != NULL)
    FDecorations.end1Decoration->addListener(&FListener);
  if (FDecorations.end1Label != NULL)
    FDecorations.end1Label->addListener(&FListener);
  if (FDecorations.end2Decoration != NULL)
    FDecorations.end2Decoration->addListener(&FListener);
  if (FDecorations.end2Label != NULL)
    FDecorations.end2Label->addListener(&FListener);
  if (FDecorations.centerDecoration != NULL)
    FDecorations.centerDecoration->addListener(&FListener);
  if (FDecorations.centerLabel != NULL)
    FDecorations.centerLabel->addListener(&FListener);

  change(this, GC_CHANGE_CONNECTION_DECORATION);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the actions of the figure element that is located at the given location. The given coordinates are
 * already in element space, so no further conversion is needed.
 * Only actions with the correct trigger conditions are executed.
 *
 * @param button The mouse button which triggered the call.
 * @param event The event which triggered the action.
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return The last executed action or NULL if none.
 */
TAction* CConnection::executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords)
{
  TAction* result = NULL;

  if (containsPoint(coords.x, coords.y) && FDecor != NULL)
  {
    // None of the child element has had an action that could be used.
    // So execute those assigned to this element, if there are any.
    // Only actions which fit the given trigger conditions are considered.
    if (FDecor->actions().size() > 0)
    {
      for (CActions::iterator iterator = FDecor->actions().begin(); iterator != FDecor->actions().end(); ++iterator)
      {
        TAction* action = (*iterator);

        // If the action has no triggeres assigned then execute it unconditionally.
        if (action->triggers.size() == 0)
          result = doAction(action);
        else
          for (CTriggers::iterator triggerIterator = action->triggers.begin(); triggerIterator != action->triggers.end(); ++triggerIterator)
          {
            if ((triggerIterator->button == GC_MOUSE_BUTTON_IGNORE || triggerIterator->button == button) &&
              (triggerIterator->modifiers == GC_MODIFIER_IGNORE || triggerIterator->modifiers == modifiers) &&
              (triggerIterator->event == GC_MOUSE_IGNORE || triggerIterator->event == event))
            {
              result = doAction(action);

              // Once we found a valid trigger we can continue with the next action.
              break;
            };
          };
      };
    };
  };


  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the figure listener notifying the connection about the destruction of a figure it is using.
 *
 * @param figure The figure that is being destroyed.
 */
void CConnection::figureDestroyed(CFigure* figure)
{
  // Deleting one of the endpoints means the connection is no longer valid, hence destroy it in that case.
  if (FEnd1 == figure)
  {
    FEnd1 = NULL;
    delete this;
  }
  else 
    if (FEnd2 == figure)
    {
      FEnd2 = NULL;
      delete this;
    }
    else
      if (FDecorations.end1Decoration == figure)
        FDecorations.end1Decoration = NULL;
      else
        if (FDecorations.end1Label == figure)
          FDecorations.end1Label = NULL;
        else
          if (FDecorations.centerDecoration == figure)
            FDecorations.centerDecoration = NULL;
          else
            if (FDecorations.centerLabel == figure)
              FDecorations.centerLabel = NULL;
            else
              if (FDecorations.end2Decoration == figure)
                FDecorations.end2Decoration = NULL;
              else
                if (FDecorations.end2Label == figure)
                  FDecorations.end2Label = NULL;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index The index of the sub property to return if it is located in a list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CConnection::propertyGet(const char* name, unsigned int index)
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
              // place holder
              break;
            };
        };

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
 * @param index The index of the sub property to return if it is located in a list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CConnection::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // There are currently no properties that could be changed.
}

//----------------- CConnectionListener --------------------------------------------------------------------------------

void CConnectionInstance::CConnectionListener::onAction(CGCBase* sender, CGCBase* origin, TAction** action)
{
  if (sender->classIs("CConnection"))
    instance->action(origin, action);
}

//----------------------------------------------------------------------------------------------------------------------

void CConnectionInstance::CConnectionListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  switch (reason)
  {
    case GC_CHANGE_CAPTION_PROPERTY:
      break; // Ignore
    case GC_CHANGE_CONNECTION_DECORATION:
      {
        instance->updateDecorations();
        instance->makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      }
    default:
      instance->makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
  };
}

//----------------------------------------------------------------------------------------------------------------------

void CConnectionInstance::CConnectionListener::onDestroy(CGCBase* object)
{
  delete instance;
}

//----------------- CConnectionInstance --------------------------------------------------------------------------------

/**
 * Constructor of the class.
 *
 * @param canvas The canvas to which this class belongs.
 * @param connection The connection which is instantiated in this class.
 * @param endPoint1 The first endpoint.
 * @param endPoint2 The second endpoint.
 */
CConnectionInstance::CConnectionInstance(CConnectionLayer* layer, CConnection* connection, CFigureInstance* endPoint1, 
                                         CFigureInstance* endPoint2): CGraphicElement(connection->canvas())
{
  _className = "CConnectionInstance";
  FLayer = layer;
  FListener.instance = this;
  FConnection = connection;
  FConnection->addListener(&FListener);
  FEnd1 = endPoint1;
  FEnd1->addListener(&FListener);
  FEnd2 = endPoint2;
  FEnd2->addListener(&FListener);
  FDisplayList = 0;
  FDirection1 = GC_DIR_NONE;
  FDirection2 = GC_DIR_NONE;
  FLineStyle = GC_CONNECTION_STYLE_SOLID;
  FColor[0] = 0;
  FColor[1] = 0;
  FColor[2] = 0;
  FColor[3] = 1;
  FHotColor[0] = 0;
  FHotColor[1] = 0;
  FHotColor[2] = 1;
  FHotColor[3] = 1;

  // Decorations and labels.
  FEnd1Decoration = NULL;
  FEnd1Label = NULL;
  FCenterDecoration = NULL;
  FCenterLabel = NULL;
  FEnd2Decoration = NULL;
  FEnd2Label = NULL;

  // If this connection is self referencing then move the center closer to the (one) end point otherwise center it.
  if (FEnd1 == FEnd2)
    FCenter = 0.01f;
  else
    FCenter = 0.5f;

  updateDecorations();
}

//----------------------------------------------------------------------------------------------------------------------

CConnectionInstance::~CConnectionInstance(void)
{
  FConnection->removeListener(&FListener);
  FEnd1->removeListener(&FListener);
  FEnd2->removeListener(&FListener);

  // Decorations and labels.
  if (FEnd1Decoration != NULL)
  {
    FEnd1Decoration->removeListener(&FListener);
    delete FEnd1Decoration;
  };
  if (FEnd1Label != NULL)
  {
    FEnd1Label->removeListener(&FListener);
    delete FEnd1Label;
  };
  if (FCenterDecoration != NULL)
  { 
    FCenterDecoration->removeListener(&FListener);
    delete FCenterDecoration;
  };
  if (FCenterLabel != NULL)
  { 
    FCenterLabel->removeListener(&FListener);
    delete FCenterLabel;
  };
  if (FEnd2Decoration != NULL)
  {  
    FEnd2Decoration->removeListener(&FListener);
    delete FEnd2Decoration;
  };
  if (FEnd2Label != NULL)
  {
    FEnd2Label->removeListener(&FListener);
    delete FEnd2Label;
  };

  if (FDisplayList != 0)
    glDeleteLists(FDisplayList, 1);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the correct orientation of the target element and applies all necessary transformations.
 *
 * @param target The figure instance that must be transformed.
 * @param position The position to which the target must be moved.
 * @param direction The orientation of the target. This is used to rotation or mirror the figure instance accordingly.
 */
void CConnectionInstance::applyTransformations(CFigureInstance* target, TVertex* position, TConnectionDirection direction)
{
  target->disableEvents();
  target->translate(position->x, position->y, 0);

  switch (direction)
  {
    case GC_DIR_EAST:
      {
        target->rotate(0, 0, 0, 1);
        target->scale(1, 1, 1);
        break;
      };
    case GC_DIR_SOUTH:
      {
        target->rotate(90, 0, 0, 1);
        target->scale(1, 1, 1);
        break;
      };
    case GC_DIR_WEST:
      {
        target->rotate(0, 0, 0, 1);
        target->scale(-1, 1, 1);
        break;
      };
    case GC_DIR_NORTH:
      {
        target->rotate(-90, 0, 0, 1);
        target->scale(1, 1, 1);
        break;
      };
  };

  target->enableEvents();
  target->validate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the current center coordinate depending on the connection orientation, the relative center value and the
 * current end points.
 */
float CConnectionInstance::computeCenterPosition(void)
{
  float result = 0;

  if (FEnd1 == FEnd2)
  {
    // Self referencing, means this connections starts and stops at the same end point.
    float fullWidth;
    float fullHeight;

    // Compute the center as percentage of the entire width/height.
    canvas()->currentViewGet()->getWorkspace(&fullWidth, &fullHeight);

    switch (FDirection1)
    {
      case GC_DIR_SOUTH:
      case GC_DIR_NORTH:
      {
        float offset = (FEnd1Decoration != NULL) ? FEnd1Decoration->height() : 0;
        if ((FEnd2Decoration != NULL) && FEnd2Decoration->height() > offset)
          offset = FEnd2Decoration->height();

        if (FDirection1 == GC_DIR_NORTH)
          result = FEndVertex1.y - offset - FCenter * fullHeight;
        else
          result = FEndVertex1.y + offset + FCenter * fullHeight;
        
        break;
      }
      default:
      {
        // GC_DIR_EAST/GC_DIR_WEST
        float offset = (FEnd1Decoration != NULL) ? FEnd1Decoration->width() : 0;
        if ((FEnd2Decoration != NULL) && FEnd2Decoration->width() > offset)
          offset = FEnd2Decoration->width();

        if (FDirection1 == GC_DIR_WEST)
          result = FEndVertex1.x - offset - FCenter * fullWidth;
        else
          result = FEndVertex1.x + offset + FCenter * fullWidth;

        break;
      };
    };
  }
  else
    switch (FDirection1)
    {
      case GC_DIR_SOUTH:
      case GC_DIR_NORTH:
      {
        float offset1 = (FEnd1Decoration != NULL) ? FEnd1Decoration->height() : 0;
        float offset2 = (FEnd2Decoration != NULL) ? FEnd2Decoration->height() : 0;
        if (FEndVertex1.y <= FEndVertex2.y)
          result = FEndVertex1.y + offset1 + FCenter * (FEndVertex2.y - offset2 - FEndVertex1.y - offset1);
        else
          result = FEndVertex2.y + offset2 + FCenter * (FEndVertex1.y - offset1 - FEndVertex2.y - offset2);

        break;
      }
      default:
      {
        // GC_DIR_EAST/GC_DIR_WEST
        float offset1 = (FEnd1Decoration != NULL) ? FEnd1Decoration->width() : 0;
        float offset2 = (FEnd2Decoration != NULL) ? FEnd2Decoration->width() : 0;
        if (FEndVertex1.x <= FEndVertex2.x)
          result = FEndVertex1.x + offset1 + FCenter * (FEndVertex2.x - offset2 - FEndVertex1.x - offset1);
        else
          result = FEndVertex2.x + offset2 + FCenter * (FEndVertex1.x - offset1 - FEndVertex2.x - offset2);

        break;
      };
    };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines the visual orienation of the center decoration, which depends not only on the direction of the 
 * connection but also the relation of its end points.
 */
TConnectionDirection CConnectionInstance::determineCenterDirection(void)
{
  TConnectionDirection result = GC_DIR_NONE;
  TBoundingBox bounds = FCenterDecoration->bounds();
  float centerWidth = bounds.lower.x - bounds.upper.x;
  float dX = FEndVertex2.x - FEndVertex1.x;
  float dY = FEndVertex2.y - FEndVertex1.y;

  // For the center we consider the connection to be oriented in a certain direction (from end point 1 to 2).
  switch (FDirection1)
  {
    case GC_DIR_NORTH:
      {
        // If the second end point is to the right of the first and its horizontal distance is large enough
        // then the center points east. If there is not enough distance to make up a meaningful horizontal
        // direction then point the center upwards. Finally if the second point is placed to the left of the
        // first point the center points to west. Similar for all other cases.
        if (fabs(dX) < centerWidth)
          result = GC_DIR_NORTH;
        else
          if (dX > 0)
            result = GC_DIR_EAST;
          else
            result = GC_DIR_WEST;
        break;
      };
    case GC_DIR_EAST:
      {
        if (fabs(dY) < centerWidth)
          result = GC_DIR_EAST;
        else
          if (dY > 0)
            result = GC_DIR_SOUTH;
          else
            result = GC_DIR_NORTH;
        break;
      };
    case GC_DIR_SOUTH:
      {
        if (fabs(dX) < centerWidth)
          result = GC_DIR_SOUTH;
        else
          if (dX > 0)
            result = GC_DIR_EAST;
          else
            result = GC_DIR_WEST;
        break;
      };
    case GC_DIR_WEST:
      {
        if (fabs(dY) < centerWidth)
          result = GC_DIR_WEST;
        else
          if (dY > 0)
            result = GC_DIR_SOUTH;
          else
            result = GC_DIR_NORTH;
        break;
      };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the actions of the connection instance that is located at the given location. The given coordinates are
 * in view space but are implicitely converted by the figure instances.
 * Only actions with the correct trigger conditions are executed.
 *
 * @param button The mouse button which triggered the call.
 * @param event The event which triggered the action.
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The mouse coordinates in element space.
 * @return The last executed action or NULL if none.
 */
TAction* CConnectionInstance::executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords)
{
  TAction* lastAction = NULL;

  // End points. They are always assigned.
  lastAction = FEnd1->executeAssociatedActions(button, event, modifiers, coords);
  if (lastAction == NULL)
    lastAction = FEnd2->executeAssociatedActions(button, event, modifiers, coords);

  // Decorations and labels. They may not always be assigned.
  if (lastAction == NULL && FEnd1Decoration != NULL)
    lastAction = FEnd1Decoration->executeAssociatedActions(button, event, modifiers, coords);
  if (lastAction == NULL && FEnd1Label != NULL)
    lastAction = FEnd1Label->executeAssociatedActions(button, event, modifiers, coords - FLabel1Offset);
  if (lastAction == NULL && FCenterDecoration != NULL)
    lastAction = FCenterDecoration->executeAssociatedActions(button, event, modifiers, coords);
  if (lastAction == NULL && FCenterLabel != NULL)
    lastAction = FCenterLabel->executeAssociatedActions(button, event, modifiers, coords - FCenterLabelOffset);
  if (lastAction == NULL && FEnd2Decoration != NULL)
    lastAction = FEnd2Decoration->executeAssociatedActions(button, event, modifiers, coords);
  if (lastAction == NULL && FEnd2Label != NULL)
    lastAction = FEnd2Label->executeAssociatedActions(button, event, modifiers, coords - FLabel2Offset);

  if (lastAction == NULL && FConnection != NULL)
    lastAction = FConnection->executeAssociatedActions(button, event, modifiers, coords);

  return lastAction;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders the given decoration and label if they are assigned.
 *
 * @param part The part to render.
 */
void CConnectionInstance::renderDecoration(TConnectionPart part)
{
  CFigureInstance* decoration = NULL;
  CFigureInstance* label = NULL;
  TVertex offset;

  switch (part)
  {
    case GC_CONNECTION_PART_1:
      {
        decoration = FEnd1Decoration;
        label = FEnd1Label;
        offset = FLabel1Offset;
        break;
      };
    case GC_CONNECTION_PART_CENTER:
      {
        decoration = FCenterDecoration;
        label = FCenterLabel;
        offset = FCenterLabelOffset;
        break;
      };
    case GC_CONNECTION_PART_2:
      {
        decoration = FEnd2Decoration;
        label = FEnd2Label;
        offset = FLabel2Offset;
        break;
      };
  };

  glPushAttrib(GL_ENABLE_BIT);
  glEnable(GL_LINE_SMOOTH);

  if (decoration != NULL)
    decoration->render();
  if (label != NULL)
  {
    glPushMatrix();
    glTranslatef(offset.x, offset.y, offset.z);
    label->render();
    glPopMatrix();
  };

  glPopAttrib();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the correct position and orientation of the decoration part indicated by part.
 *
 * @param part Determines the part (ends or center) to update.
 */
void CConnectionInstance::updateDecoration(TConnectionPart part)
{
  // niceDistance is a small offset to avoid packing elements too tightly.
  float niceDistance = 2;
  TBoundingBox decorationBounds;

  if (part == GC_CONNECTION_PART_CENTER)
  {
    if (FDirection1 == GC_DIR_NORTH || FDirection1 == GC_DIR_SOUTH)
    {
      if (FCenterDecoration != NULL)
      {
        TVertex position;
        position.x = (FEndVertex1.x + FEndVertex2.x) / 2;
        position.y = computeCenterPosition();
        applyTransformations(FCenterDecoration, &position, determineCenterDirection());
        decorationBounds = FCenterDecoration->bounds();
      };
      if (FCenterLabel != NULL)
      {
        TBoundingBox labelBounds = FCenterLabel->localBounds();
        float labelWidth = labelBounds.lower.x - labelBounds.upper.x;
        float labelHeight = labelBounds.lower.y - labelBounds.upper.y;
        float centerX = (FEndVertex1.x + FEndVertex2.x) / 2 - labelWidth / 2;
        float centerY = decorationBounds.upper.y - labelHeight - niceDistance;
        FCenterLabel->disableEvents();
        FCenterLabel->translate(centerX, centerY, 0);
        FCenterLabel->enableEvents();
      };
    }
    else
    {
      if (FCenterDecoration != NULL)
      {
        TVertex position;
        position.x = computeCenterPosition();
        position.y = (FEndVertex1.y + FEndVertex2.y) / 2;
        applyTransformations(FCenterDecoration, &position, determineCenterDirection());
        decorationBounds = FCenterDecoration->bounds();
      };
      if (FCenterLabel != NULL)
      {
        TBoundingBox labelBounds = FCenterLabel->localBounds();
        float labelHeight = labelBounds.lower.y - labelBounds.upper.y;
        float centerX = decorationBounds.lower.x + niceDistance;
        float centerY = (FEndVertex1.y + FEndVertex2.y) / 2 - labelHeight / 2;
        FCenterLabel->disableEvents();
        FCenterLabel->translate(centerX, centerY, 0);
        FCenterLabel->enableEvents();
      };
    };
  }
  else
  {
    CFigureInstance* decoration;
    CFigureInstance* label;
    TConnectionDirection direction;
    TVertex* position;

    if (part == GC_CONNECTION_PART_1)
    {
      decoration = FEnd1Decoration;
      label = FEnd1Label;
      direction = FDirection1;
      position = &FEndVertex1;
    }
    else
    {
      decoration = FEnd2Decoration;
      label = FEnd2Label;
      direction = FDirection2;
      position = &FEndVertex2;
    };

    // Start with the decoration figure.
    if (decoration != NULL)
      applyTransformations(decoration, position, direction);

    // Next step is the label.
    if (label != NULL)
    {
      label->validate();
      if (decoration != NULL)
        decorationBounds = decoration->bounds();
      TBoundingBox labelBounds = label->localBounds();
      float labelWidth = labelBounds.lower.x - labelBounds.upper.x;
      float labelHeight = labelBounds.lower.y - labelBounds.upper.y;

      float labelX = 0;
      float labelY = 0;

      // Position computation is based on the idea to keep the label as close as possible to the local origin,
      // that is, the coordinate of the end point.
      switch (direction)
      {
        case GC_DIR_NORTH:
          {
            float dX1 = fabs(position->x - decorationBounds.upper.x);
            float dX2 = fabs(decorationBounds.lower.x - position->x);
            float dY = fabs(position->y - decorationBounds.upper.y);
            float x = (dX1 <= dX2) ? decorationBounds.upper.x : decorationBounds.lower.x;
            if (dX1 < dY || dX2 < dY)
            {
              // Left or right to the decoration is closer.
              if (dX1 <= dX2)
                labelX = x -labelWidth - niceDistance; 
              else
                labelX = x + niceDistance; 
              labelY = position->y - labelHeight - niceDistance;
            }
            else
            {
              // Above the decoration is closer.
              labelX = position->x + niceDistance;
              labelY = decorationBounds.lower.y - labelHeight - niceDistance;
            };
            break;
          };
        case GC_DIR_EAST:
          {
            float dY1 = fabs(decorationBounds.upper.y - position->y);
            float dY2 = fabs(position->y - decorationBounds.lower.y);
            float dX = fabs(decorationBounds.lower.x - position->x);
            float y = (dY1 <= dY2) ? decorationBounds.upper.y : decorationBounds.lower.y;
            if (dY1 < dX || dY2 < dX)
            {
              // Above or below the decoration is closer.
              labelX = position->x + niceDistance;
              if (dY1 <= dY2)
                labelY = y - labelHeight - niceDistance; 
              else
                labelY = y + niceDistance; 
            }
            else
            {
              // To the right of the decoration is closer.
              labelX = decorationBounds.lower.x + niceDistance;
              labelY = position->y + niceDistance;
            };
            break;
          };
        case GC_DIR_SOUTH:
          {
            float dX1 = fabs(position->x - decorationBounds.upper.x);
            float dX2 = fabs(decorationBounds.lower.x - position->x);
            float dY = fabs(decorationBounds.lower.y - position->y);
            float x = (dX1 <= dX2) ? decorationBounds.upper.x : decorationBounds.lower.x;
            if (dX1 < dY || dX2 < dY)
            {
              // Left or right to the decoration is closer.
              if (dX1 <= dX2)
                labelX = x - labelWidth - niceDistance; 
              else
                labelX = x + niceDistance; 
              labelY = position->y + niceDistance;
            }
            else
            {
              // Below the decoration is closer.
              labelX = position->x + niceDistance;
              labelY = decorationBounds.lower.y + niceDistance;
            };
            break;
          };
        case GC_DIR_WEST:
          {
            float dY1 = fabs(decorationBounds.upper.y - position->y);
            float dY2 = fabs(position->y - decorationBounds.lower.y);
            float dX = fabs(position->x - decorationBounds.upper.x);
            float y = (dY1 <= dY2) ? decorationBounds.upper.y : decorationBounds.lower.y;
            if (dY1 < dX || dY2 < dX)
            {
              // Above or below the decoration is closer.
              labelX = position->x - labelWidth - niceDistance;
              if (dY1 <= dY2)
                labelY = y - labelHeight - niceDistance; 
              else
                labelY = y + niceDistance; 
            }
            else
            {
              // To the left of the decoration is closer.
              labelX = decorationBounds.upper.x - labelWidth - niceDistance;
              labelY = position->y + niceDistance;
            };
            break;
          };
      };
      label->disableEvents();
      label->translate(labelX, labelY, 0);
      label->enableEvents();
    };
  };
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Helper method for CConnectionInstance::updateDecorations. It checks if the given figure instance still corresponds
 * to the figure and if not updates the instance.
 */
void CConnectionInstance::updateFigureInstance(CFigureInstance** instance, CFigure* figure)
{
 if ((*instance != NULL) && (figure != NULL))
  {
    // Both references exist. Check if they refer to the same figure.
    if ((*instance)->figure() != figure)
    {
      // Our connection has a new figure as decoration/label. Delete the instance we created for it.
      // A new figure instance will be created by the following code then.
      (*instance)->removeListener(&FListener);
      delete *instance;
      *instance = NULL;
    };
  };

  if ((*instance == NULL) != (figure == NULL))
  {
    // One of both entries does not yet/no longer exist.
    if (*instance != NULL)
    {
      (*instance)->removeListener(&FListener);
      delete *instance;
      *instance = NULL;
    }
    else
    {
      // A new decoration/label was added, create a figure instance for that.
      *instance = new CFigureInstance(NULL, figure);
      (*instance)->addListener(&FListener);
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when the zoom level in the owner view changed. Tell the decorations about it.
 *
 * @param newZoom The new zoom level.
 */
void CConnectionInstance::zoomChanged(float newZoom)
{
  if (FEnd1Decoration != NULL)
    FEnd1Decoration->zoomChanged(newZoom);
  if (FEnd1Label != NULL)
    FEnd1Label->zoomChanged(newZoom);
  if (FCenterDecoration != NULL)
    FCenterDecoration->zoomChanged(newZoom);
  if (FCenterLabel != NULL)
    FCenterLabel->zoomChanged(newZoom);
  if (FEnd2Decoration != NULL)
    FEnd2Decoration->zoomChanged(newZoom);
  if (FEnd2Label != NULL)
    FEnd2Label->zoomChanged(newZoom);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Overrides the hit computation of the base class to precise the result further.
 *
 * @param x Horizontal hit coordinate.
 * @param y Vertical hit coordinate.
 */
bool CConnectionInstance::containsPoint(const float x, const float y)
{
  bool result = CGraphicElement::containsPoint(x, y);
  if (result)
  {
    TVertex vertex = TVertex(x, y, 0);
    result = getFeedbackInfo(GC_MODIFIER_IGNORE, vertex) != GC_FI_NONE;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the final coordinates of this connection at the given end point (which must be one of the end points
 * of this connection instance).
 *
 * @param point One of the end points of this connection.
 * @param direction Indicates the edge where the connection leaves the end point.
 * @param slot A distribution offset in the range (0..1) that controls the relative position along the edge.
 * @param alternative Indicates to to use the opposite end point instead what @point would refer to. 
 *                    Used, e.g., for self references.
 */
void CConnectionInstance::computeCoordinates(CFigureInstance* point, TConnectionDirection direction, float slot, 
                                             bool alternative)
{
  TVertex coordinates;

  if ((point == FEnd1) != alternative)
    FDirection1 = direction;
  else
    FDirection2 = direction;

  TBoundingBox bounds = point->bounds();
  switch (direction)
  {
    case GC_DIR_NORTH:
      {
        coordinates.x = (bounds.lower.x - bounds.upper.x) * slot + bounds.upper.x;
        coordinates.y = bounds.upper.y;
        break;
      };
    case GC_DIR_EAST:
      {
        coordinates.x = bounds.lower.x;
        coordinates.y = (bounds.lower.y - bounds.upper.y) * slot + bounds.upper.y;
        break;
      };
    case GC_DIR_SOUTH:
      {
        coordinates.x = (bounds.lower.x - bounds.upper.x) * slot + bounds.upper.x;
        coordinates.y = bounds.lower.y;
        break;
      };
    case GC_DIR_WEST:
      {
        coordinates.x = bounds.upper.x;
        coordinates.y = (bounds.lower.y - bounds.upper.y) * slot + bounds.upper.y;
        break;
      };
  };
  if ((point == FEnd1) != alternative)
    FEndVertex1 = coordinates;
  else
    FEndVertex2 = coordinates;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Determines in which direction this connection leaves the given point. This depends entirely on the positions of the
 * endpoints.
 *
 * @param point The point to consider, must be one of the end points of this connection.
 * @return One of the direction flags.
 */
TConnectionDirection CConnectionInstance::getDirection(CFigureInstance* point)
{
  TConnectionDirection result = GC_DIR_NONE;

  CFigureInstance* from = NULL;
  CFigureInstance* to = NULL;
  if (point == FEnd1)
  {
    from = FEnd1;
    to = FEnd2;
  }
  else
    if (point == FEnd2)
    {
      from = FEnd2;
      to = FEnd1;
    };

  if (from != NULL && to != NULL)
  {
    if (from == to)
      // The connection starts and ends on the same end point.
      result = GC_DIR_SELF;
    else
    {
      TBoundingBox fromBounds = from->bounds();
      TBoundingBox toBounds = to->bounds();

      result = GC_DIR_NORTH;
      float distance = fromBounds.upper.y - toBounds.lower.y;
      if (toBounds.upper.x - fromBounds.lower.x > distance)
      {
        result = GC_DIR_EAST;
        distance = toBounds.upper.x - fromBounds.lower.x;
      };
      if (toBounds.upper.y - fromBounds.lower.y > distance)
      {
        result = GC_DIR_SOUTH;
        distance = toBounds.upper.y - fromBounds.lower.y;
      };
      if (fromBounds.upper.x - toBounds.lower.x > distance)
      {
        result = GC_DIR_WEST;
        distance = toBounds.lower.x - fromBounds.upper.x;
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns info about the connection at the specified point.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The coordinates to check against.
 * @return The requested info.
 */
TFeedbackInfo CConnectionInstance::getFeedbackInfo(TModifiers modifiers, const TVertex& coords)
{
  TFeedbackInfo result;

  // End points. They are always assigned.
  result = FEnd1->getFeedbackInfo(modifiers, coords);
  if (result == GC_FI_NONE)
    result = FEnd2->getFeedbackInfo(modifiers, coords);

  // Decorations and labels. They may not always be assigned.
  if (result == GC_FI_NONE && FEnd1Decoration != NULL)
    result = FEnd1Decoration->getFeedbackInfo(modifiers, coords);
  if (result == GC_FI_NONE && FEnd1Label != NULL)
    result = FEnd1Label->getFeedbackInfo(modifiers, coords - FLabel1Offset);
  if (result == GC_FI_NONE && FCenterDecoration != NULL)
    result = FCenterDecoration->getFeedbackInfo(modifiers, coords);
  if (result == GC_FI_NONE && FCenterLabel != NULL)
    result = FCenterLabel->getFeedbackInfo(modifiers, coords - FCenterLabelOffset);
  if (result == GC_FI_NONE && FEnd2Decoration != NULL)
    result = FEnd2Decoration->getFeedbackInfo(modifiers, coords);
  if (result == GC_FI_NONE && FEnd2Label != NULL)
    result = FEnd2Label->getFeedbackInfo(modifiers, coords - FLabel2Offset);

  if (result == GC_FI_NONE)
  {
    float distance = 10; // Distance to the line segments to be considered as "on line".
    float centerPoint = computeCenterPosition();

    if (FDirection1 == GC_DIR_NORTH || FDirection1 == GC_DIR_SOUTH)
    {
      float minX = MIN(FEndVertex1.x, FEndVertex2.x);
      float maxX = MAX(FEndVertex1.x, FEndVertex2.x);
      if (fabs(centerPoint - coords.y) <= distance && (minX <= coords.x) && (coords.x <= maxX))
        // Close to the center line.
        result = GC_FI_HLINE_MOVE;
      else
      {
        // Check the other two segments.
        if (coords.y < centerPoint)
        {
          // Above the center.
          float actualX = (FEndVertex1.y <= centerPoint) ? FEndVertex1.x : FEndVertex2.x;
          if (fabs(actualX - coords.x) <= distance)
            result = GC_FI_CONNECTION;
        }
        else
        {
          // Below the center
          float actualX = (FEndVertex1.y > centerPoint) ? FEndVertex1.x : FEndVertex2.x;
          if (fabs(actualX - coords.x) <= distance)
            result = GC_FI_CONNECTION;
        };
      };
    }
    else
    {
      float minY = MIN(FEndVertex1.y, FEndVertex2.y);
      float maxY = MAX(FEndVertex1.y, FEndVertex2.y);
      if (fabs(centerPoint - coords.x) <= distance && (minY <= coords.y) && (coords.y <= maxY))
        // Close to the center line.
        result = GC_FI_VLINE_MOVE;
      else
      {
        // Check the other two segments.
        if (coords.x < centerPoint)
        {
          // Left to the center.
          float actualY = (FEndVertex1.x <= centerPoint) ? FEndVertex1.y : FEndVertex2.y;
          if (fabs(actualY - coords.y) <= distance)
            result = GC_FI_CONNECTION;
        }
        else
        {
          // Right to the center
          float actualY = (FEndVertex1.x > centerPoint) ? FEndVertex1.y : FEndVertex2.y;
          if (fabs(actualY - coords.y) <= distance)
            result = GC_FI_CONNECTION;
        };
      };
    };
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Returns the connection decoration part whose label is at the given position.
 *
 * @param x The x coordinate to check in layer space.
 * @param y The y coordinate.
 * @return The part that has a label at the given position.
 */
TConnectionPart CConnectionInstance::labelPartFromPoint(float x, float y)
{
  TConnectionPart result = GC_CONNECTION_PART_NONE;
  if (FEnd1Label != NULL && FEnd1Label->containsPoint(x - FLabel1Offset.x, y - FLabel1Offset.y))
    result = GC_CONNECTION_PART_1;
  else
    if (FEnd2Label != NULL && FEnd2Label->containsPoint(x - FLabel2Offset.x, y - FLabel2Offset.y))
      result = GC_CONNECTION_PART_2;
    else
      if (FCenterLabel != NULL && FCenterLabel->containsPoint(x - FCenterLabelOffset.x, y - FCenterLabelOffset.y))
        result = GC_CONNECTION_PART_CENTER;
  
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Changes the visual style of the connection lines.
 *
 * @param lineStyle The new line style to apply.
 */
void CConnectionInstance::lineStyleSet(TConnectionLineStyle newStyle)
{
  if (FLineStyle != newStyle)
  {
    FLineStyle = newStyle;
    makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the current center point to the given position. This position is converted into a procentual value for the
 * center and only one dimension (depending on the orientation of the connection) is considered.
 *
 * @param position The new position for the center.
 */
void CConnectionInstance::moveCenterTo(TVertex position)
{
  if (FEnd1 == FEnd2)
  {
    // Self referencing, means this connections starts and stops at the same end point.
    float fullWidth;
    float fullHeight;

    // Compute the center as percentage of the entire width/height.
    canvas()->currentViewGet()->getWorkspace(&fullWidth, &fullHeight);

    switch (FDirection1)
    {
      case GC_DIR_SOUTH:
      case GC_DIR_NORTH:
      {
        float offset = (FEnd1Decoration != NULL) ? FEnd1Decoration->height() : 0;
        if ((FEnd2Decoration != NULL) && FEnd2Decoration->height() > offset)
          offset = FEnd2Decoration->height();

        if (FDirection1 == GC_DIR_NORTH)
          FCenter = (FEndVertex1.y - offset - position.y) / fullHeight;
        else
          FCenter = (position.y - FEndVertex1.y - offset) / fullHeight;
        
        break;
      }
      default:
      {
        // GC_DIR_EAST/GC_DIR_WEST
        float offset = (FEnd1Decoration != NULL) ? FEnd1Decoration->width() : 0;
        if ((FEnd2Decoration != NULL) && FEnd2Decoration->width() > offset)
          offset = FEnd2Decoration->width();

        if (FDirection1 == GC_DIR_WEST)
          FCenter = (FEndVertex1.x - offset - position.x) / fullWidth;
        else
          FCenter = (position.x - FEndVertex1.x - offset) / fullWidth;

        break;
      };
    };
  }
  else
    switch (FDirection1)
    {
      case GC_DIR_SOUTH:
      case GC_DIR_NORTH:
      {
        if (FEndVertex1.y <= FEndVertex2.y)
          FCenter = (position.y - FEndVertex1.y) / (FEndVertex2.y - FEndVertex1.y);
        else
          FCenter = (position.y - FEndVertex2.y) / (FEndVertex1.y - FEndVertex2.y);

        break;
      }
      default:
      {
        // GC_DIR_EAST/GC_DIR_WEST
        if (FEndVertex1.x <= FEndVertex2.x)
          FCenter = (position.x - FEndVertex1.x) / (FEndVertex2.x - FEndVertex1.x);
        else
          FCenter = (position.x - FEndVertex2.x) / (FEndVertex1.x - FEndVertex2.x);

        break;
      };
    };

  if (FCenter < 0)
    FCenter = 0;
  if (FCenter > 1)
    FCenter = 1;

  makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Moves the label of the given part by the given amount.
 *
 * @param part The part whose label is to be moved.
 * @param dX The horizontal amount to move the label.
 * @param dY The vertical amount to move the label.
 */
void CConnectionInstance::moveLabel(TConnectionPart part, float dX, float dY)
{
  switch (part)
  {
    case GC_CONNECTION_PART_1:
      {
        FLabel1Offset.x += dX;
        FLabel1Offset.y += dY;
        break;
      };
    case GC_CONNECTION_PART_2:
      {
        FLabel2Offset.x += dX;
        FLabel2Offset.y += dY;
        break;
      };
    case GC_CONNECTION_PART_CENTER:
      {
        FCenterLabelOffset.x += dX;
        FCenterLabelOffset.y += dY;
        break;
      };
  };
  makeDirty(GC_CHANGE_CONNECTION_LABEL);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Retrieves the value of the property given by path. The path syntax is must be something like (here expressed as regex)
 * (container)*(property), where container is a slash and the name of a container class (e.g. layers, figures) and
 * property is the name of a simple property of that container.
 *
 * @param name The name of the property to return.
 * @param index The index of the sub property to return if it is located in a list.
 * @return A description of the property value and, if the property is simple, the actual value.
 */
TGCVariant CConnectionInstance::propertyGet(const char* name, unsigned int index)
{
  TGCVariant result;

  switch (getContainerID(name))
  {
    case GC_CONTAINER_UNKNOWN:
      {
        switch (getPropertyID(name))
        {
          case GC_PROPERTY_CENTER:
          {
            result = FCenter;
            break;
          };
		  case GC_PROPERTY_LABEL1_OFFSET_X:
          {
            result = FLabel1Offset.x;
            break;
          };
          case GC_PROPERTY_LABEL1_OFFSET_Y:
          {
            result = FLabel1Offset.y;
            break;
          };
          case GC_PROPERTY_LABEL2_OFFSET_X:
          {
            result = FLabel2Offset.x;
            break;
          };
          case GC_PROPERTY_LABEL2_OFFSET_Y:
          {
            result = FLabel2Offset.y;
            break;
          };
          case GC_PROPERTY_LABEL_CENTER_OFFSET_X:
          {
            result = FCenterLabelOffset.x;
            break;
          };
          case GC_PROPERTY_LABEL_CENTER_OFFSET_Y:
          {
            result = FCenterLabelOffset.y;
            break;
          };            
        };

        break;
      };
    case GC_CONTAINER_COLOR:
      {
        if (index < 4)
        {
          result = FColor[index];
		};
        break;
      };
    case GC_CONTAINER_HOT_COLOR:
      {
        if (index < 4)
        {
          result = FHotColor[index];
        };
        break;
      };
    case GC_CONTAINER_CONNECTION:
      {
        result = FConnection;
        break;
      };
    case GC_CONTAINER_END1:
      {
        result = FEnd1;
        break;
      };
    case GC_CONTAINER_END1_DECORATION:
      {
        result = FEnd1Decoration;
        break;
      };
    case GC_CONTAINER_END1_LABEL:
      {
        result = FEnd1Label;
        break;
      };
    case GC_CONTAINER_END2:
      {
        result = FEnd2;
        break;
      };
    case GC_CONTAINER_END2_DECORATION:
      {
        result = FEnd2Decoration;
		break;
      };
    case GC_CONTAINER_END2_LABEL:
      {
        result = FEnd2Label;
        break;
      };
    case GC_CONTAINER_CENTER_DECORATION:
      {
        result = FCenterDecoration;
        break;
      };
    case GC_CONTAINER_CENTER_LABEL:
      {
        result = FCenterLabel;
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
 * @param index The index of the sub property to return if it is located in a list.
 * @param value The new value of the property. Automatic conversion is performed where possible.
 */
void CConnectionInstance::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  switch (getContainerID(name))
  {
	case GC_CONTAINER_UNKNOWN:
	  {
		switch (getPropertyID(name))
		{
		  case GC_PROPERTY_CENTER:
          {
            FCenter = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL1_OFFSET_X:
          {
            FLabel1Offset.x = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL1_OFFSET_Y:
          {
            FLabel1Offset.y = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL2_OFFSET_X:
          {
            FLabel2Offset.x = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL2_OFFSET_Y:
          {
            FLabel2Offset.y = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL_CENTER_OFFSET_X:
          {
            FCenterLabelOffset.x = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };
          case GC_PROPERTY_LABEL_CENTER_OFFSET_Y:
          {
            FCenterLabelOffset.y = value;
            makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
            break;
          };            
        };

        break;
      };
    case GC_CONTAINER_COLOR:
      {
        if (index < 4)
        {
          FColor[index] = value;
          makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        };
        break;
      };
    case GC_CONTAINER_HOT_COLOR:
      {
        if (index < 4)
        {
          FHotColor[index] = value;
          makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        };
        break;
      };
    case GC_CONTAINER_CONNECTION:
      {
        FConnection = value.cast<CConnection>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END1:
      {
        FEnd1 = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END1_DECORATION:
      {
        FEnd1Decoration = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END1_LABEL:
      {
        FEnd1Label = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END2:
      {
        FEnd2 = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END2_DECORATION:
      {
        FEnd2Decoration = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_END2_LABEL:
      {
        FEnd2Label = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_CENTER_DECORATION:
      {
        FCenterDecoration = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
    case GC_CONTAINER_CENTER_LABEL:
      {
        FCenterLabel = value.cast<CFigureInstance>();
        makeDirty(GC_CHANGE_CONNECTION_INSTANCE);
        break;
      };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders this instance.
 *
 * @param isHot A marker if the current instance has the mouse cursor over it.
 */
void CConnectionInstance::render(bool isHot)
{
  if (isHot)
    glColor4fv(FHotColor);
  else
    glColor4fv(FColor);
  glCallList(FDisplayList);

  // Decorations
  renderDecoration(GC_CONNECTION_PART_1);
  renderDecoration(GC_CONNECTION_PART_CENTER);
  renderDecoration(GC_CONNECTION_PART_2);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the listener when something in the connection's decoration changed or any of used figure instance is being 
 * destroyed. Use this notification to (re)build the figure instances we use for decoration.
 */
void CConnectionInstance::updateDecorations(void)
{
  TConnectionDecorations decorations = FConnection->decorationsGet();

  updateFigureInstance(&FEnd1Decoration, decorations.end1Decoration);
  updateFigureInstance(&FEnd1Label, decorations.end1Label);
  updateFigureInstance(&FCenterDecoration, decorations.centerDecoration);
  updateFigureInstance(&FCenterLabel, decorations.centerLabel);
  updateFigureInstance(&FEnd2Decoration, decorations.end2Decoration);
  updateFigureInstance(&FEnd2Label, decorations.end2Label);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes all detail elements for this connection depending on computed start and end point as well as other properties.
 */
void CConnectionInstance::validate(void)
{
  if (dirty())
  {
    if (FLayer != NULL)
    {
      FLayer->validateEndPoint(FEnd1, true);
      FLayer->validateEndPoint(FEnd2, false);
    };
    if (FEnd1Decoration != NULL)
      FEnd1Decoration->validate();
    if (FEnd1Label != NULL)
      FEnd1Label->validate();
    if (FCenterDecoration != NULL)
      FCenterDecoration->validate();
    if (FCenterLabel != NULL)
      FCenterLabel->validate();
    if (FEnd2Decoration != NULL)
      FEnd2Decoration->validate();
    if (FEnd2Label != NULL)
      FEnd2Label->validate();

    if (FDisplayList == 0)
      FDisplayList = glGenLists(1);

    glNewList(FDisplayList, GL_COMPILE);
      glPushMatrix();
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_LINE_SMOOTH);
      glDisable(GL_POLYGON_SMOOTH);
      if (canvas()->supportsExtension(GC_OE_MULTISAMPLING))
        glDisable(GL_MULTISAMPLE_ARB); 

      switch (FLineStyle)
      {
        case GC_CONNECTION_STYLE_SOLID:
          break;
        case GC_CONNECTION_STYLE_DOTTED:
          {
            glEnable(GL_LINE_STIPPLE);
            glLineStipple (1, 0x3333);
            break;
          };
      };

      glBegin(GL_LINE_STRIP);
      float centerPoint = computeCenterPosition();
      if (FDirection1 == GC_DIR_NORTH || FDirection1 == GC_DIR_SOUTH)
      {
        // We need to distinguish here only if we have a vertical or horizontal run.
        // So checking only one direction is enough.
        glVertex2f(FEndVertex1.x, FEndVertex1.y);
        glVertex2f(FEndVertex1.x, centerPoint);
        glVertex2f(FEndVertex2.x, centerPoint);
        glVertex2f(FEndVertex2.x, FEndVertex2.y);
      }
      else
      {
        glVertex2f(FEndVertex1.x, FEndVertex1.y);
        glVertex2f(centerPoint, FEndVertex1.y);
        glVertex2f(centerPoint, FEndVertex2.y);
        glVertex2f(FEndVertex2.x, FEndVertex2.y);
      };
      glEnd();
      glPopAttrib();
      glPopMatrix();
    glEndList();

    // Valdate decoration and compute the overall bounding box.
    boundsNew();
    boundsAdd(FEndVertex1);
    boundsAdd(FEndVertex2);

    updateDecoration(GC_CONNECTION_PART_1);
    if (FEnd1Decoration != NULL)
      boundsAdd(FEnd1Decoration->bounds());
    if (FEnd1Label != NULL)
      boundsAdd(FEnd1Label->bounds(), FLabel1Offset);

    updateDecoration(GC_CONNECTION_PART_CENTER);
    if (FCenterDecoration != NULL)
      boundsAdd(FCenterDecoration->bounds());
    if (FCenterLabel != NULL)
      boundsAdd(FCenterLabel->bounds(), FCenterLabelOffset);

    updateDecoration(GC_CONNECTION_PART_2);
    if (FEnd2Decoration != NULL)
      boundsAdd(FEnd2Decoration->bounds());
    if (FEnd2Label != NULL)
      boundsAdd(FEnd2Label->bounds(), FLabel2Offset);

    boundsFinished();

    FConnection->boundsNew();
    FConnection->boundsAdd(FEndVertex1);
    FConnection->boundsAdd(FEndVertex2);
    FConnection->boundsFinished();

    CGraphicElement::validate();
  };
}

//----------------- CConnectionInstanceListener --------------------------------------------------------------------------

void CConnectionLayer::CConnectionInstanceListener::onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason)
{
  layer->handleChange((CConnectionInstance*) sender, reason);
}

//----------------------------------------------------------------------------------------------------------------------

void CConnectionLayer::CConnectionInstanceListener::onDestroy(CGCBase* object)
{
  layer->removeInstance((CConnectionInstance*) object);
}

//----------------------------------------------------------------------------------------------------------------------

void CConnectionLayer::CConnectionInstanceListener::onError(CGCBase* sender, CGCBase* origin, const char* message)
{
  layer->error(origin, message);
}

//----------------- CConnectionLayer ------------------------------------------------------------------------------------

CConnectionLayer::CConnectionLayer(CGCView* view): CLayer("connections", view->canvas())
{
  _className = "CConnectionLayer";
  FView = view;
  FListener.layer = this;
  FHotConnection = NULL;
  FStates = 0;
  FCache = NULL;
  FCurrentFeedback = GC_FI_NONE;
}

//----------------------------------------------------------------------------------------------------------------------

CConnectionLayer::~CConnectionLayer(void)
{
  setDestroying();

  delete FCache;

  for (CConnectionInstanceList::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
    delete *iterator;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks for an action that can be executed in the layer.
 *
 * @param button Which button has been pressed (left, middle, right, x1, x2).
 * @param event The mouse event that caused the handling chain to start (up, down etc.).
 * @param modifiers Special flags that control the processing.
 * @param windowX Horizontal mouse coordinate in window space.
 * @param windowY Vertical mouse coordinate in window space.
 * @param viewCoords Mouse coordinates converted to view space.
 * @return True if there was an action to execute, otherwise false.
 */
bool CConnectionLayer::handleAction(TMouseButton button, TMouseEvent event, TModifiers modifiers, int windowX, int windowY, 
                                    TVertex& viewCoords)
{
  bool result = false;
  bool keepHitInfo = false;

  updateCache();
  FLastHit = (CConnectionInstance*) FCache->findElement(viewCoords);
  if (FLastHit != NULL)
  {
    TAction* action = FLastHit->executeAssociatedActions(button, event, modifiers, viewCoords);
    if (action != NULL)
    {
      result = true;
      if (action != NULL)
      {
        if (action->type == GC_ACTION_DRAG)
        {
          FStates |= GC_CLAYER_STATE_DRAG_LABEL_PENDING;

          FLabelPart = FLastHit->labelPartFromPoint(viewCoords.x, viewCoords.y);
          keepHitInfo = true;
        }
      };
    }
    else
    {
      TFeedbackInfo info = FLastHit->getFeedbackInfo(modifiers, viewCoords);
      switch (info)
      {
      case GC_FI_CONNECTION:
        {
          // Select the connection.
          break;
        };
      case GC_FI_HLINE_MOVE:
        {
          FStates |= GC_CLAYER_STATE_HDRAG_PENDING;
          result = true;
          break;
        };
      case GC_FI_VLINE_MOVE:
        {
          FStates |= GC_CLAYER_STATE_VDRAG_PENDING;
          result = true;
          break;
        };
      };
    };
  };

  if (keepHitInfo)
  {
    FLastWindowX = windowX;
    FLastWindowY = windowY;  
    FLastViewX = viewCoords.x;
    FLastViewY = viewCoords.y;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Marks all connection instances the are connected to the given endpoint as dirty, if they are not already.
 *
 * @param point The endpoint to iterate.
 */
void CConnectionLayer::invalidateEndPoint(CFigureInstance* point)
{
  CFigureConnectionList::iterator pointIterator = FConnectionPointList.find(point);
  for (CConnectionInstanceList::iterator iterator = pointIterator->second.begin(); iterator != pointIterator->second.end(); 
    ++iterator)
  {
    CConnectionInstance* instance = *iterator;
    if (!instance->dirty())
    {
      instance->makeDirty();
      if (instance->endPoint1() == point)
        invalidateEndPoint(instance->endPoint2());
      else
        invalidateEndPoint(instance->endPoint1());
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

// Binary predicates used to sort the direction lists in the next method.
// Return whether first element is less than the second

bool moreLeft(CConnectionInstance* element1, CConnectionInstance* element2)
{
  // Both connections share one end point. Compare the other two.
  CFigureInstance* point1 = element1->endPoint2();
  CFigureInstance* point2 = element1->endPoint2();
  if (element1->endPoint1() == element2->endPoint2())
    point2 = element2->endPoint1();
  else
    if (element1->endPoint2() == element2->endPoint1())
      point1 = element1->endPoint1();
    else
      if (element1->endPoint2() == element2->endPoint2())
      {
        point1 = element1->endPoint1();
        point2 = element2->endPoint1();
      };

  TBoundingBox bounds1 = point1->bounds();
  float middle1 = (bounds1.lower.x + bounds1.upper.x) / 2;
  TBoundingBox bounds2 = point2->bounds();
  float middle2 = (bounds2.lower.x + bounds2.upper.x) / 2;

  return middle1 < middle2;;
}

//----------------------------------------------------------------------------------------------------------------------

bool moreTop(CConnectionInstance* element1, CConnectionInstance* element2)
{
  // Both connections share one end point. Compare the other two.
  CFigureInstance* point1 = element1->endPoint2();
  CFigureInstance* point2 = element1->endPoint2();
  if (element1->endPoint1() == element2->endPoint2())
    point2 = element2->endPoint1();
  else
    if (element1->endPoint2() == element2->endPoint1())
      point1 = element1->endPoint1();
    else
      if (element1->endPoint2() == element2->endPoint2())
      {
        point1 = element1->endPoint1();
        point2 = element2->endPoint1();
      };

  TBoundingBox bounds1 = point1->bounds();
  float middle1 = (bounds1.lower.y + bounds1.upper.y) / 2;
  TBoundingBox bounds2 = point2->bounds();
  float middle2 = (bounds2.lower.y + bounds2.upper.y) / 2;

  return middle1 < middle2;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Renders all connections, which have been validate before in validateLayerContent.
 */
void CConnectionLayer::renderLayerContent(TBoundingBox bounds)
{
  for (CConnectionInstanceList::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
  {
    CConnectionInstance* instance = *iterator;
    if (instance->overlaps(bounds))
      instance->render(instance == FHotConnection);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks if the layer's element cache must be created and validated.
 */
void CConnectionLayer::updateCache(void)
{
  if (FCache == NULL)
  {
    float width, height;
    FView->getWorkspace(&width, &height);
    FCache = new CElementLookupCache(width, height);
  };

  if (!FCache->isValid())
  {
    for (CConnectionInstanceList::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
      FCache->addElement(*iterator);

    FCache->validate();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the layout for all connection instances.
 * Note: the invalidation code (@see invalidateInstance) takes care that either all connection instances connected to an
 *       end point are dirty or none of them.
 */
void CConnectionLayer::validateLayerContent(void)
{
  // Iterate through all dirty connection instances and validates them.
  for (CConnectionInstanceList::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
    (*iterator)->validate();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when the zoom level in the owner view is changed. Forward the call to the connections.
 *
 * @param newZoom The new zoom level.
 */
void CConnectionLayer::zoomChanged(float newZoom)
{
  for (CConnectionInstanceList::iterator iterator = FConnections.begin(); iterator != FConnections.end(); ++iterator)
    (*iterator)->zoomChanged(newZoom);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new connection instance.
 *
 * @param connection The connection for which the instance is to be created.
 * @param endPoint1 One end point of the instnace.
 * @param endPoint2 The other end point of the instance.
 */
CConnectionInstance* CConnectionLayer::createInstance(CConnection* connection, CFigureInstance* endPoint1, 
                                                      CFigureInstance* endPoint2)
{
  CConnectionInstance* result = new CConnectionInstance(this, connection, endPoint1, endPoint2);
  result->addListener(&FListener);
  FConnections.insert(result);
  FConnectionPointList[endPoint1].insert(result);
  invalidateEndPoint(endPoint1);
  FConnectionPointList[endPoint2].insert(result);
  invalidateEndPoint(endPoint2);
  if (FCache != NULL)
    FCache->addElement(result);
  makeDirty();

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Checks the given position and hit element for a detailed hit information.
 *
 * @param modifiers Any combination of Alt, Control and Shift key.
 * @param coords The position to test against.
 * @return Info about the hit if any.
 */
TFeedbackInfo CConnectionLayer::getFeedbackInfo(TModifiers modifiers, TVertex& coords)
{
  TFeedbackInfo result = GC_FI_NONE;

  if (FStates == 0)
  {
    updateCache();

    CConnectionInstance* instance = (CConnectionInstance*) FCache->findElement(coords);
    if (instance != NULL)
      result = instance->getFeedbackInfo(modifiers, coords);
  }
  else
    result = FCurrentFeedback;

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Takes the given coordinates and tries to find a graphic element that was rendered at this position.
 * Positions are given in view space.
 *
 * @param hits [out] The hit collection that is updated.
 * @param point The point to check given in view space. If necessary convert window coordinates first by using windowToView.
 * @param singleHit If true then search for hits is stopped after the first one was found.
 * @return A hit result class is returned regardless of the actual number of hits. It must be freed by the caller.
 */
void CConnectionLayer::getHitTestInfoAt(CHitResults* hits, TVertex point, bool singleHit)
{
  updateCache();
  if (singleHit)
  {
    CGraphicElement* element = FCache->findElement(point);
    if (element != NULL)
      hits->addHit(element);
  }
  else
    FCache->findElements(point, hits);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Invalidates all connection instances that are connected to the end point of the given connection instance.
 *
 * @param The connection instance that has changed.
 */
void CConnectionLayer::handleChange(CConnectionInstance* instance, TGCChangeReason reason)
{
  switch (reason)
  {
    case GC_CHANGE_CONNECTION_INSTANCE:
      {
        #if USE_BSP_TREE
          if (FCache)
            FCache->invalidate();
        #endif

        invalidateEndPoint(instance->endPoint1());
        invalidateEndPoint(instance->endPoint2());
        break;
      };
  };
  change(instance, reason);
  makeDirty();
}

//----------------------------------------------------------------------------------------------------------------------

bool CConnectionLayer::handleMouseDown(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool result = handleAction(button, GC_MOUSE_DOWN, modifiers, windowX, windowY, viewCoords);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

bool CConnectionLayer::handleMouseMove(TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool result = false;

  if (FStates == 0)
  {
    // No special action active so highlight the current connection if hot tracking is enabled.
    updateCache();
    CGraphicElement* element = FCache->findElement(viewCoords);
    if (element != NULL)
    {
      if (element->classIs("CConnectionInstance") && element != FHotConnection)
      {
        FHotConnection = (CConnectionInstance*) element;
        canvas()->refresh();
      };
    }
    else
      if (FHotConnection != NULL)
      {
        FHotConnection = NULL;
        canvas()->refresh();
      };
  }
  else
  {
    float threshold = 4; // Empirically found.
    float dX = viewCoords.x - FLastViewX;
    float dY = viewCoords.y - FLastViewY;
    
    // Check for center line dragging.
    if ((FStates & GC_CLAYER_STATE_HDRAG_PENDING) != 0)
    {
      // Drag is still waiting to start. Check if the user moved the mouse a bit.
      if (fabs(dY) >= threshold)
      {
        FStates &= ~GC_CLAYER_STATE_HDRAG_PENDING;
        FStates |= GC_CLAYER_STATE_CENTER_DRAGGING;
        FCurrentFeedback = GC_FI_HLINE_MOVE;
      };
    }
    else
      if ((FStates & GC_CLAYER_STATE_VDRAG_PENDING) != 0)
      {
        // Drag is still waiting to start. Check if the user moved the mouse a bit.
        float threshold = 4; // Empirically found.
        if (fabs(dX) >= threshold)
        {
          FStates &= ~GC_CLAYER_STATE_VDRAG_PENDING;
          FStates |= GC_CLAYER_STATE_CENTER_DRAGGING;
          FCurrentFeedback = GC_FI_VLINE_MOVE;
        };
      }
      else
        if ((FStates & GC_CLAYER_STATE_DRAG_LABEL_PENDING) != 0)
        {
          // Label drag is still waiting to start. Check if the user moved the mouse a bit.
          if (fabs(dX) >= threshold || fabs(dY) >= threshold)
          {
            FStates &= ~GC_CLAYER_STATE_DRAG_LABEL_PENDING;
            FStates |= GC_CLAYER_STATE_DRAG_LABEL;
            FCurrentFeedback = GC_FI_DRAG_FIGURE;
          };
        };

    if ((FStates & GC_CLAYER_STATE_CENTER_DRAGGING) != 0)
    {
      FLastHit->moveCenterTo(viewCoords);
#if USE_BSP_TREE
      FCache->invalidate();
#endif
      result = true;
    }
    else
      if ((FStates & GC_CLAYER_STATE_DRAG_LABEL) != 0)
      {
        FLastHit->moveLabel(FLabelPart, dX, dY);
        result = true;
      };
  };

  if (result)
  {
    FLastViewX = viewCoords.x;
    FLastViewY = viewCoords.y;
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

bool CConnectionLayer::handleMouseUp(TMouseButton button, TModifiers modifiers, int windowX, int windowY, TVertex& viewCoords)
{
  bool result = handleAction(button, GC_MOUSE_UP, modifiers, windowX, windowY, viewCoords);

  FStates &= ~(GC_CLAYER_STATE_HDRAG_PENDING | GC_CLAYER_STATE_VDRAG_PENDING | GC_CLAYER_STATE_DRAG_LABEL_PENDING |
    GC_CLAYER_STATE_CENTER_DRAGGING | GC_CLAYER_STATE_DRAG_LABEL);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given instance from the internal list. The connection instance is not destroyed, though.
 *
 * @param instance The connection instance to be removed.
 */
void CConnectionLayer::removeInstance(CConnectionInstance* instance)
{
  if (!destroying())
  {
    if (FCache)
      FCache->invalidate();

    if (FHotConnection == instance)
      FHotConnection = NULL;

    CFigureInstance* point = instance->FEnd1;
    FConnectionPointList[point].erase(instance);
    if (FConnectionPointList[point].size() == 0)
      FConnectionPointList.erase(point);

    point = instance->FEnd2;
    FConnectionPointList[point].erase(instance);
    if (FConnectionPointList[point].size() == 0)
      FConnectionPointList.erase(point);

    FConnections.erase(instance);

    makeDirty();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the coordinates of all connection instances touching the given point.
 *
 * @param point The endpoint to validate.
 * @param handleSelfReferences If true then also consider connections that start and end at the same point.
 */
void CConnectionLayer::validateEndPoint(CFigureInstance* point, bool handleSelfReferences)
{
  typedef vector<CConnectionInstance*> CLocalConnectionList;

  CLocalConnectionList northConnections;
  CLocalConnectionList eastConnections;
  CLocalConnectionList southConnections;
  CLocalConnectionList westConnections;
  CLocalConnectionList selfConnections;

  // In order to validate a connection instance both end points must be validated, that is, all
  // connections attached to these end points must be validated and the coordinates of the connection lines
  // must be computed.
  CFigureConnectionList::iterator pointIterator = FConnectionPointList.find(point);

  // Go through all connections attached to this point and determine the direction of them (i.e. where they are
  // attached to the point).
  for (CConnectionInstanceList::iterator iterator = pointIterator->second.begin(); iterator != pointIterator->second.end(); 
    ++iterator)
  {
    switch ((*iterator)->getDirection(point))
    {
      case GC_DIR_SELF:
        {
          if (handleSelfReferences)
            selfConnections.push_back(*iterator);
          break;
        };
      case GC_DIR_NORTH:
        {
          northConnections.push_back(*iterator);
          break;
        };
      case GC_DIR_EAST:
        {
          eastConnections.push_back(*iterator);
          break;
        };
      case GC_DIR_SOUTH:
        {
          southConnections.push_back(*iterator);
          break;
        };
      case GC_DIR_WEST:
        {
          westConnections.push_back(*iterator);
          break;
        };
    };
  };

  // Sort each direction list so connections do not overlap and compute for each direction the connection coordinates.
  sort(northConnections.begin(), northConnections.end(), moreLeft);
  sort(eastConnections.begin(), eastConnections.end(), moreTop);
  sort(southConnections.begin(), southConnections.end(), moreLeft);
  sort(westConnections.begin(), westConnections.end(), moreTop);

  CLocalConnectionList::iterator instanceIterator;

  // Distribute all self references so that all sides are equally filled.
  for (instanceIterator = selfConnections.begin(); instanceIterator != selfConnections.end(); ++instanceIterator)
  {
    CLocalConnectionList* targetList = &northConnections;

    if (eastConnections.size() < targetList->size())
      targetList = &eastConnections;
    if (southConnections.size() < targetList->size())
      targetList = &southConnections;
    if (westConnections.size() < targetList->size())
      targetList = &westConnections;

    // Add the point twice. We need room for both ends.
    targetList->insert(targetList->begin() + targetList->size() / 2, *instanceIterator);
    targetList->insert(targetList->begin() + targetList->size() / 2, *instanceIterator);
  };

  int index;
  CConnectionInstance* lastInstance = NULL;
  for (instanceIterator = northConnections.begin(), index = 1; instanceIterator != northConnections.end(); 
    ++instanceIterator, ++index)
  {
    bool isSelfReference = lastInstance == *instanceIterator;
    lastInstance = *instanceIterator;
    lastInstance->computeCoordinates(point, GC_DIR_NORTH, (float) index / (northConnections.size() + 1), isSelfReference);
  };

  lastInstance = NULL;
  for (instanceIterator = eastConnections.begin(), index = 1; instanceIterator != eastConnections.end(); 
    ++instanceIterator, ++index)
  {
    bool isSelfReference = lastInstance == *instanceIterator;
    lastInstance = *instanceIterator;
    lastInstance->computeCoordinates(point, GC_DIR_EAST, (float) index / (eastConnections.size() + 1), isSelfReference);
  };

  lastInstance = NULL;
  for (instanceIterator = southConnections.begin(), index = 1; instanceIterator != southConnections.end(); 
    ++instanceIterator, ++index)
  {
    bool isSelfReference = lastInstance == *instanceIterator;
    lastInstance = *instanceIterator;
    lastInstance->computeCoordinates(point, GC_DIR_SOUTH, (float) index / (southConnections.size() + 1), isSelfReference);
  };

  lastInstance = NULL;
  for (instanceIterator = westConnections.begin(), index = 1; instanceIterator != westConnections.end(); 
    ++instanceIterator, ++index)
  {
    bool isSelfReference = lastInstance == *instanceIterator;
    lastInstance = *instanceIterator;
    lastInstance->computeCoordinates(point, GC_DIR_WEST, (float) index / (westConnections.size() + 1), isSelfReference);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called by the owning view to let us know when the workspace size has changed. This directly influences the element cache
 * (if it uses a BSP tree).
 */
void CConnectionLayer::workspaceChanged(void)
{
#if USE_BSP_TREE
  delete FCache;
  FCache = NULL;
#endif
}

//----------------------------------------------------------------------------------------------------------------------

