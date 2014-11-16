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
 * @file myx_gc_animation.h 
 * @brief Implementation of the animation manager.
 * 
 */

#ifndef __GC_ANIMATION_H__
#define __GC_ANIMATION_H__

#include "myx_gc_base.h"
#include "myx_gc_canvas.h"
#include "myx_gc_figure.h"

//----------------------------------------------------------------------------------------------------------------------

typedef enum tagInterpolation
{
  GC_INTERPOLATION_LINEAR,
  GC_INTERPOLATION_ACCELLERATED
} TInterpolation;

class CFigureInstance;
class CAnimationManager;
/**
 * This is the base animation class, which handles time computations and communication with the
 * animated elements.
 */
class GENERIC_CANVAS_API CAnimation
{
  friend class CAnimationManager;
protected:
  CAnimationManager* FManager;
  float FDuration;           // The time the entire animation can use (in milliseconds).
  float FUsedTime;           // The time the animation already consumed (in milliseconds, not counted in suspended mode).
  bool FSuspended;           // If true then the animation is stopped and has no effect.

  virtual bool __cdecl animate(int step);
public:
  CAnimation(CAnimationManager* manager, int duration, bool suspended);
  virtual ~CAnimation(void);

  virtual void __cdecl addDependency(CAnimation* animation);
  virtual void __cdecl animationStarted(void) {};
  virtual void __cdecl animationStopped(void) {};
  virtual bool __cdecl finished(void);
  virtual bool __cdecl suspendedGet(void) { return FSuspended; };
  virtual void __cdecl suspendedSet(bool suspended);
};

/**
 * Describes properties of one point in a series of movements commonly known as path.
 */
typedef struct tagAnimationPoint
{
  TVertex offset;            // The relative distance to move.
  float usableSlice;         // Duration for the move to this point (in milliseconds).
  float usedSlice;           // Already consumed time.
} TAnimationPoint;

typedef vector<TAnimationPoint> TPoints;

/**
 * A specialized class for an animation of a figure instance which involves travelling over one or more target 
 * points (offsets).
 */
class CFigureInstancePathAnimation: public CAnimation
{
  friend class CAnimationManager;
private:
  TPoints FPoints;
  TVertex FLastOffset;            // Keeps the last computed position to determine relative movements.
  CFigureInstance* FInstance;     
protected:
  void addPoint(const TVertex offset, float slice);
  virtual bool __cdecl animate(int step);
public:
  CFigureInstancePathAnimation(CAnimationManager* manager, int duration, bool suspended, CGraphicElement* element);

  virtual void __cdecl animationStarted(void);
  virtual void __cdecl animationStopped(void);
};

/**
 * A specialized class for an animation of the zoom of a view.
 */
class CViewZoomAnimation: public CAnimation
{
  friend class CAnimationManager;
private:
  float FTargetZoom;
  float FZoomDelta;
  float FLastStep;
  
  CGCView* FView;     
protected:
  virtual bool __cdecl animate(int step);
public:
  CViewZoomAnimation(CAnimationManager* manager, float newZoom, int duration, bool suspended, CGCView* element);
  CViewZoomAnimation(CAnimationManager* manager, float from, float to, int duration, bool suspended, CGCView* element);

  virtual void __cdecl animationStarted(void);
  virtual void __cdecl animationStopped(void);
  virtual bool __cdecl finished(void);
};

/**
 * A specialized class for an animation of the offset of a view.
 */
class CViewOffsetAnimation: public CAnimation
{
  friend class CAnimationManager;
private:
  float FTargetOffsetX;
  float FTargetOffsetY;
  float FOffsetXDelta;
  float FOffsetYDelta;
  float FLastStepX;
  float FLastStepY;

  CGCView* FView;     
protected:
  virtual bool __cdecl animate(int step);
public:
  CViewOffsetAnimation(CAnimationManager* manager, float x, float y, int duration, bool suspended, CGCView* element);
  CViewOffsetAnimation(CAnimationManager* manager, float fromX, float fromY, float toX, float toY, int duration, 
    bool suspended, CGCView* element);

  virtual void __cdecl animationStarted(void);
  virtual void __cdecl animationStopped(void);
  virtual bool __cdecl finished(void);
};

//----------------------------------------------------------------------------------------------------------------------

typedef vector<CAnimation*> CAnimations;
typedef multimap<CAnimation*, CAnimation*> CAnimationDependancies;
typedef pair<CAnimation*, CAnimation*> CAnimationDependanciesPair;

class CAnimationManager: public CGCBase
{
  friend class CAnimation;
private:
  int FTimerBase;
  CAnimations FAnimations;
  CAnimationDependancies FDependencies;
protected:
  void addDependency(CAnimation* animation, CAnimation* dependentAnimation);
  void removeAllDependencies(CAnimation* animation);
  void removeDependency(CAnimation* animation, CAnimation* dependentAnimation);
  void triggerDependentAnimations(CAnimation* animation);
public:
  CAnimationManager(CGenericCanvas* canvas);
  virtual ~CAnimationManager(void);

  virtual CAnimation* __cdecl createPathAnimation(const TVertex& offset, int duration, bool suspended, CGraphicElement* element);
  virtual CAnimation* __cdecl createViewOffsetAnimation(float x, float y, int duration, bool suspended, CGCView* element);
  // Need an own name for the overloaded method here because of limitations imposed through access by non-C++ languages (like Delphi).
  virtual CAnimation* __cdecl createViewOffsetAnimation2(float fromX, float fromY, float toX, float toY, int duration, 
    bool suspended, CGCView* element);
  virtual CAnimation* __cdecl createViewZoomAnimation(float newZoom, int duration, bool suspended, CGCView* element);
  virtual CAnimation* __cdecl createViewZoomAnimation2(float from, float to, int duration, bool suspended, CGCView* element);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  virtual void __cdecl pulse(void);
  virtual void __cdecl timerBase(int base);
};

//----------------------------------------------------------------------------------------------------------------------

#endif // __GC_ANIMATION_H__
