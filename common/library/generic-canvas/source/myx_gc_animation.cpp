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
 * @file myx_gc_animation.cpp 
 * @brief Implementation of the animation manager.
 * 
 */

#include "myx_gc_animation.h"

#define DEFAULT_INTERPOLATION GC_INTERPOLATION_LINEAR

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes a linear interpolation between 0 and the given end value, determined by parameter t.
 */
float interpolate(float endValue, float t, TInterpolation interpolation)
{
  float result = 0;
  switch (interpolation)
  {
    case GC_INTERPOLATION_LINEAR:
      {
        //         /
        //        /
        //       /
        //      /
        //     /
        //    /
        result = endValue * t;
        break;
      };
    case GC_INTERPOLATION_ACCELLERATED:
      {
        //           ----
        //          /
        //         |
        //         |
        //        /
        //    ---/
        result = endValue * (atan(5.0f * t - 2.5f) / (float) M_PI + 0.5f);
        break;
      };
  };
  return result;
}

//----------------- CAnimation -----------------------------------------------------------------------------------------

CAnimation::CAnimation(CAnimationManager* manager, int duration, bool suspended)
{
  FManager = manager;
  FDuration = (float) duration;
  FSuspended = suspended;
  FUsedTime = 0;
  if (!FSuspended)
    animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

CAnimation::~CAnimation(void)
{
  FManager->removeAllDependencies(this);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Base implementation for one animation step.
 *
 * @param step The number of milliseconds that passed by since the last animation step (not counted in suspend state).
 * @result True if the animation actually took place otherwise false.
 */
bool CAnimation::animate(int step)
{
  bool result = !FSuspended && !finished();
  if (result)
  {
    FUsedTime += step;

    // If we finished the animation with this step then trigger dependant animations.
    if (finished())
      FManager->triggerDependentAnimations(this);
  };

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a dependency between this animation and the animation given as parameter.
 *
 * @param animation That animation depends on this animation, that is, it is move from suspended to unsuspended state
 *                  when this animation has finished. The given animation must be in suspend state.
 */
void CAnimation::addDependency(CAnimation* animation)
{
  if (animation->suspendedGet())
    FManager->addDependency(this, animation);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller if the animation has used its full time slice already.
 *
 * @result True if the available time was used otherwise false.
 */
bool CAnimation::finished(void)
{
  return (FDuration == 0) || (FUsedTime >= FDuration);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets a new suspended state holding or continuing so effectively the animation.
 *
 * @param suspended It true then the animation is stopped and waits. If false then a waiting animation continues.
 */
void CAnimation::suspendedSet(bool suspended)
{
  if (FSuspended != suspended)
  {
    FSuspended = suspended;
    if (FSuspended)
      animationStopped();
    else
      animationStarted();
  };
}

//----------------- CFigureInstancePathAnimation -----------------------------------------------------------------------

CFigureInstancePathAnimation::CFigureInstancePathAnimation(CAnimationManager* manager, int duration, bool suspended, 
                                                           CGraphicElement* element): CAnimation(manager, duration, suspended)
{
  FInstance = (CFigureInstance*) element;
  if (!suspended)
    FInstance->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds a new point to the path.
 *
 * @param offset The values for x, y and z to be used for the move.
 * @param slice The duration this one move may use in relation to the full animation duration (0..1).
 *              Values <= 0 will prevent the new point from being added. A value > 1 is clamped to 1.
 */
void CFigureInstancePathAnimation::addPoint(const TVertex offset, float slice)
{
  if (slice > 1)
    slice = 1;
  if (slice > 0)
  {
    TAnimationPoint point;
    point.offset = offset;
    point.usedSlice = 0;
    point.usableSlice = slice * FDuration;
    FPoints.push_back(point);
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the necessary values to advance in the path animation and applies them to the target element.
 *
 * @param step The number of milliseconds that passed by since the last animation step (not counted in suspend state).
 * @result True if the animation actually took place otherwise false.
 */
bool CFigureInstancePathAnimation::animate(int step)
{
  bool result = CAnimation::animate(step) && (FPoints.size() > 0);
  if (result)
  {
    // The point at index 0 is always the active one.
    TAnimationPoint& point = FPoints[0];
    point.usedSlice += step;

    TVertex newOffset;
    float factor = point.usedSlice / point.usableSlice;
    if (factor > 1)
      factor = 1;
    newOffset.x = interpolate(point.offset.x, factor, DEFAULT_INTERPOLATION);
    newOffset.y = interpolate(point.offset.y, factor, DEFAULT_INTERPOLATION);
    newOffset.z = interpolate(point.offset.z, factor, DEFAULT_INTERPOLATION);

    FInstance->FTranslation[0] += newOffset.x - FLastOffset.x;
    FInstance->FTranslation[1] += newOffset.y - FLastOffset.y;
    FInstance->FTranslation[2] += newOffset.z - FLastOffset.z;
    FLastOffset = newOffset;
    
    // Remove the current point if we have used up its time slice.
    if (point.usedSlice >= point.usableSlice)
      FPoints.erase(FPoints.begin());
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CFigureInstancePathAnimation::animationStarted(void) 
{
  FInstance->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CFigureInstancePathAnimation::animationStopped(void) 
{
  FInstance->animationStopped();
}

//----------------- CViewZoomAnimation ---------------------------------------------------------------------------------

CViewZoomAnimation::CViewZoomAnimation(CAnimationManager* manager, float newZoom, int duration, bool suspended, CGCView* element): 
  CAnimation(manager, duration, suspended)
{
  FView = element;
  FTargetZoom = newZoom;
  FZoomDelta = newZoom - FView->FZoom;
  FLastStep = 0;
  if (!suspended)
    FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

CViewZoomAnimation::CViewZoomAnimation(CAnimationManager* manager, float from, float to, int duration, bool suspended, 
                                       CGCView* element): CAnimation(manager, duration, suspended)
{
  FView = element;
  FTargetZoom = to;
  FZoomDelta = to - from;
  FLastStep = 0;
  if (!suspended)
    FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the next zoom value and applies it.
 *
 * @param step The number of milliseconds that passed by since the last animation step (not counted in suspend state).
 * @result True if the animation actually took place otherwise false.
 */
bool CViewZoomAnimation::animate(int step)
{
  bool result = CAnimation::animate(step);
  if (result)
  {
    float newOffset = interpolate(FZoomDelta, FUsedTime / FDuration, DEFAULT_INTERPOLATION);
    FView->FZoom += newOffset - FLastStep;
    FLastStep = newOffset;

    // Set the end value explicitely because of computation inaccuracies
    // it can happen we did not hit the end point exactly.
    if (finished())
      FView->FZoom = FTargetZoom;
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CViewZoomAnimation::animationStarted(void) 
{
  FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CViewZoomAnimation::animationStopped(void) 
{
  FView->animationStopped(true, false);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller if the animation has used its full time slice already or no change was actually necessary.
 *
 * @result True if no further steps are needed otherwise false.
 */
bool CViewZoomAnimation::finished(void)
{
  return CAnimation::finished() || (FZoomDelta == 0);
}

//----------------- CViewOffsetAnimation -------------------------------------------------------------------------------

CViewOffsetAnimation::CViewOffsetAnimation(CAnimationManager* manager, float x, float y, int duration, bool suspended, 
                                           CGCView* element): CAnimation(manager, duration, suspended)
{
  FView = element;
  FTargetOffsetX = x;
  FTargetOffsetY = y;
  FOffsetXDelta = x - FView->FOffsetX;
  FOffsetYDelta = y - FView->FOffsetY;
  FLastStepX = 0;
  FLastStepY = 0;
  if (!suspended)
    FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

CViewOffsetAnimation::CViewOffsetAnimation(CAnimationManager* manager, float fromX, float fromY, float toX, float toY, 
                                           int duration, bool suspended, CGCView* element): CAnimation(manager, duration, suspended)
{
  FView = element;
  FTargetOffsetX = toX;
  FTargetOffsetY = toY;
  FOffsetXDelta = toX - fromX;
  FOffsetYDelta = toY - fromY;
  FLastStepX = 0;
  FLastStepY = 0;
  if (!suspended)
    FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Computes the next zoom value and applies it.
 *
 * @param step The number of milliseconds that passed by since the last animation step (not counted in suspend state).
 * @result True if the animation actually took place otherwise false.
 */
bool CViewOffsetAnimation::animate(int step)
{
  bool result = CAnimation::animate(step);
  if (result)
  {
    float newOffsetX = interpolate(FOffsetXDelta, FUsedTime / FDuration, DEFAULT_INTERPOLATION);
    float newOffsetY = interpolate(FOffsetYDelta, FUsedTime / FDuration, DEFAULT_INTERPOLATION);
    FView->FOffsetX += newOffsetX - FLastStepX;
    FView->FOffsetY += newOffsetY - FLastStepY;
    FLastStepX = newOffsetX;
    FLastStepY = newOffsetY;

    // Set the end value explicitely because of computation inaccuracies
    // it can happen we did not hit the end point exactly.
    if (finished())
    {
      FView->FOffsetX = FTargetOffsetX;
      FView->FOffsetY = FTargetOffsetY;
    };
  };
  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CViewOffsetAnimation::animationStarted(void) 
{
  FView->animationStarted();
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Triggers the animation notification for the animated element.
 */
void CViewOffsetAnimation::animationStopped(void) 
{
  FView->animationStopped(false, true);
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Tells the caller if the animation has used its full time slice already or no change was actually necessary.
 *
 * @result True if no further steps are needed otherwise false.
 */
bool CViewOffsetAnimation::finished(void)
{
  return CAnimation::finished() || (FOffsetXDelta == 0 && FOffsetYDelta == 0);
}

//----------------- CAnimationManager ----------------------------------------------------------------------------------

CAnimationManager::CAnimationManager(CGenericCanvas* canvas): CGCBase(canvas)
{
  _className = "CAnimationManager";
  FTimerBase = 10; // 10 milliseconds per tick in pulse() per default.
}

//----------------------------------------------------------------------------------------------------------------------

CAnimationManager::~CAnimationManager(void)
{
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Adds the given animation pair to the internal dependency list.
 *
 * @param animation The animation that enables @dependentAnimation once it is finished.
 * @param dependantAnimation The animation that is started after @animation finished. This animation must be created with
 *                           suspended state on.
 */
void CAnimationManager::addDependency(CAnimation* animation, CAnimation* dependentAnimation)
{
  FDependencies.insert(CAnimationDependanciesPair(animation, dependentAnimation));
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes all dependencies for the given animation.
 *
 * @param animation All animations that depend on this animation are removed from the internal dependancies list.
 */
void CAnimationManager::removeAllDependencies(CAnimation* animation)
{
  // First remove all entries that have the given animation as trigger.
  CAnimationDependancies::iterator iterator = FDependencies.find(animation);
  while (iterator != FDependencies.end())
  {
    // Stop loop if there are no more entries for the given animation.
    if (iterator->first != animation)
      break;
#ifdef __GNUC__
    FDependencies.erase(iterator++);
#else
    iterator = FDependencies.erase(iterator);
#endif
  };

  // Then remove all entries that trigger the given animation.
  iterator = FDependencies.begin();
  while (iterator != FDependencies.end())
  {
#ifdef __GNUC__
    if (iterator->second == animation)
      FDependencies.erase(iterator++);
    else
      ++iterator;
#else
    if (iterator->second == animation)
      iterator= FDependencies.erase(iterator);
    else
      ++iterator;
#endif
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Removes the given dependency.
 *
 * @param animation The animation on which @dependentAnimation depends.
 * @param dependentAnimation The animation that depends on @animation and which should be removed from the internal
 *                           dependancy list.
 */
void CAnimationManager::removeDependency(CAnimation* animation, CAnimation* dependentAnimation)
{
  CAnimationDependancies::iterator iterator = FDependencies.find(animation);
  while (iterator != FDependencies.end())
  {
    // Stop loop if there are no more entries for the given animation.
    if (iterator->first != animation)
      break;

    if (iterator->second == dependentAnimation)
    {
      // We found the dependent animation. Remove it and leave the loop.
      FDependencies.erase(iterator);
      break;
    };
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Called when an animation was finished to awake any dependent animations from their suspended state.
 *
 * @param animation The animation that just ended.
 */
void CAnimationManager::triggerDependentAnimations(CAnimation* animation)
{
  CAnimationDependancies::iterator iterator = FDependencies.find(animation);
  while (iterator != FDependencies.end())
  {
    // Stop loop if there are no more entries for the given animation.
    if (iterator->first != animation)
      break;

    iterator->second->suspendedSet(false);
#ifdef __GNUC__
    FDependencies.erase(iterator++);
#else
    iterator = FDependencies.erase(iterator);
#endif
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates and returns a new path animation. It accepts one offset which gets the full duration assigned.
 * This way the most commonly used path animation (a single move to a new location) is created with this one call.
 * However there can be more offsets added for which a time slice (percentage of the entire duration) must be given.
 * In that case sum of all time slices must not go beyond 1 (= 100%).
 *
 * @param offset The amount to move the target object.
 * @param duration The time the entire animation must last. It will neither be use less nor more than that (in milliseconds).
 * @param suspended Set to true if you want to add more target points (offsets) otherwise to false.
 * @element The element to be animated.
 * @result A new instance of a path animation for the given element or NULL, if the animation could not be created.
 */
CAnimation* CAnimationManager::createPathAnimation(const TVertex& offset, int duration, bool suspended,
                                                   CGraphicElement* element)
{
  CAnimation* result = NULL;

  if (element->classIs("CFigureInstance"))
  {
    CFigureInstancePathAnimation* animation = new CFigureInstancePathAnimation(this, duration, suspended, element);
    animation->addPoint(offset, 1);
    result = animation;
  };

  if (result != NULL)
    FAnimations.push_back(result);

  return result;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new view animation for offset changes and returns it.
 *
 * @param x The new horizontal offset for the view.
 * @param y The new vertical offset for the view.
 * @param duration The time the entire animation must last. It will neither be use less nor more than that (in milliseconds).
 * @param suspended Set to true if you want to add more target points (offsets) otherwise to false.
 * @element The element to be animated.
 * @result A new instance of an animation for the given element or NULL, if the animation could not be created.
 */
CAnimation* CAnimationManager::createViewOffsetAnimation(float x, float y, int duration, bool suspended, CGCView* element)
{
  CViewOffsetAnimation* animation = new CViewOffsetAnimation(this, x, y, duration, suspended, element);
  if (animation->finished())
  {
    delete animation;
    animation = NULL;
  }
  else
    FAnimations.push_back(animation);

  return animation;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new view animation for offset changes and returns it.
 *
 * @param fromX The horizontal offset for the view to start from.
 * @param fromY The vertical offset for the view to start from.
 * @param toX The horizontal offset for the view at the end.
 * @param toY The vertical offset for the view at the end.
 * @param duration The time the entire animation must last. It will neither be use less nor more than that (in milliseconds).
 * @param suspended Set to true if you want to add more target points (offsets) otherwise to false.
 * @element The element to be animated.
 * @result A new instance of an animation for the given element or NULL, if the animation could not be created.
 */
CAnimation* CAnimationManager::createViewOffsetAnimation2(float fromX, float fromY, float toX, float toY, int duration, 
                                                          bool suspended, CGCView* element)
{
  CViewOffsetAnimation* animation = new CViewOffsetAnimation(this, fromX, fromY, toX, toY, duration, suspended, element);
  if (animation->finished())
  {
    delete animation;
    animation = NULL;
  }
  else
    FAnimations.push_back(animation);

  return animation;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new view animation for zoom changes and returns it.
 *
 * @param newZoom The new zoom for the view.
 * @param duration The time the entire animation must last. It will neither be use less nor more than that (in milliseconds).
 * @param suspended Set to true if you want to add more target points (offsets) otherwise to false.
 * @element The element to be animated.
 * @result A new instance of an animation for the given element or NULL, if the animation could not be created.
 */
CAnimation* CAnimationManager::createViewZoomAnimation(float newZoom, int duration, bool suspended, CGCView* element)
{
  CViewZoomAnimation* animation = new CViewZoomAnimation(this, newZoom, duration, suspended, element);
  if (animation->finished())
  {
    delete animation;
    animation = NULL;
  }
  else
    FAnimations.push_back(animation);

  return animation;
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Creates a new view animation for zoom changes and returns it.
 *
 * @param from The zoom for the view to start from.
 * @param to The zoom for the view at the end.
 * @param duration The time the entire animation must last. It will neither be use less nor more than that (in milliseconds).
 * @param suspended Set to true if you want to add more target points (offsets) otherwise to false.
 * @element The element to be animated.
 * @result A new instance of an animation for the given element or NULL, if the animation could not be created.
 */
CAnimation* CAnimationManager::createViewZoomAnimation2(float from, float to, int duration, bool suspended, CGCView* element)
{
  CViewZoomAnimation* animation = new CViewZoomAnimation(this, from, to, duration, suspended, element);
  if (animation->finished())
  {
    delete animation;
    animation = NULL;
  }
  else
    FAnimations.push_back(animation);

  return animation;
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
TGCVariant CAnimationManager::propertyGet(const char* name, unsigned int index)
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
              result = "";
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
void CAnimationManager::propertySet(const char* name, unsigned int index, TGCVariant value)
{
  // There are currently no properties that could be changed. The name is a unique identifier and must not be changed.
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * External clock entry. Since there is no platform independant way of a reliable timer in milliseconds range
 * we let the application pulse the manager. The calling frequency of this method must correspond to the
 * timer base, which is by default 10ms per tick and can be set via method timerBase.
 */
void CAnimationManager::pulse(void)
{
  if (FAnimations.size() > 0)
  {
    bool needRefresh = false;

    CAnimations::iterator iterator = FAnimations.begin();
    while (iterator != FAnimations.end())
    {
      CAnimation* animation = *iterator;

      if (animation->animate(FTimerBase))
        needRefresh = true;
      if (animation->finished())
      {
        if (!animation->suspendedGet())
          animation->animationStopped();
        delete animation;
        iterator = FAnimations.erase(iterator);
      }
      else
        ++iterator;
    };

    // Finally repaint scene.
    if (needRefresh)
      canvas()->refresh();
  };
}

//----------------------------------------------------------------------------------------------------------------------

/**
 * Sets the internal time base used to compute the amount of time which passed between two calls to pulse().
 *
 * @param base The new time base in milliseconds.
 */
void CAnimationManager::timerBase(int base)
{
  FTimerBase = base;
}

//----------------------------------------------------------------------------------------------------------------------

