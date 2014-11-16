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
 * @file myx_gc_figure.h 
 * @brief Implementation of the model element class.
 * 
 */

#ifndef __GC_FIGURE_H__
#define __GC_FIGURE_H__

#include "myx_gc_style.h"
#include "myx_gc_layer.h"
#include "myx_gc_layout.h"
#include "myx_gc_font_manager.h"
#include "myx_gc_animation.h"

//----------------------------------------------------------------------------------------------------------------------

class CGCView;
class CGCModel;
class CLayer;
class CFigureInstance;
class CLayouter;
class CFigureController;

//----------------- Figure templates -----------------------------------------------------------------------------------

class CFigureElementTemplate;
class CCaptionElementTemplate;
class CFigureElement;
class CCaptionElement;

/** 
 * A figure element is one detail in a figure template and so also in a figure. There can be a hierarchy of figure
 * elements to form complex figures.
 */
class CFigureElementTemplate: public CBaseTemplate
{
  friend class CFigureElement;
private:
  class CStyleListener: private CGCBaseListener
  {
    friend class CFigureElementTemplate;
  protected:
    CFigureElementTemplate* template_;
  public:
    virtual void __cdecl onDestroy(CGCBase* sender);
  };

  wstring FId;                         // The XML element id.
  wstring FKey;                        // An identifier that can be used to search for this template.
  TFigureElementLayout FLayout;        // How are the element's children laid out.
  TFigureElementResize FResizeMode;    // Indicates how this element can be resized.
  TBoundingBox FBoundingBox;           // The box that includes this and all child elements.
  CElementTemplateList FChildren;      // A list of child elements attached to this element.
  CGCStyle* FStyle;                    // The style to be used for this element.
  CFigureElementTemplate* FParent;     // The parent element of this element (if at all).
  CFigure* FFigure;                    // The figure this element belongs to if not a child element of another element.
                                       // FParent and FFigure are mutual exclusive. Only one of both (or none) can be set.
  CCaptionElementTemplate* FCaption;   // An optional caption for this element.
  TConstraints FConstraints;           // Size constraints for the element. A value of -1 means no constraint.
  TOccurence FOccurence;               // Number of times this element may occur.
  CStyleListener FListener;            // A listener for changes in connected elements.
protected:
  void computeBoundingBox(void);
public:
  CFigureElementTemplate(wstring id, wstring key);
  virtual ~CFigureElementTemplate(void);

  void addChild(CFigureElementTemplate* Child);
  void freeNotification(CGCBase* object);
  CFigureElementTemplate* getListElement(void);
  void initialize(TFigureElementLayout Layout, TFigureElementResize resizeMode, CGCStyle* style, 
    const TConstraints& Constraints, TOccurence Occurence);
  bool isList(void);
  wstring key(void) { return FKey; };
  TOccurence occurence(void) { return FOccurence; };
  void setCaption(CCaptionElementTemplate* Caption);
};

/** Text alignment constants. */
typedef enum tagAlignment
{
  GC_ALIGN_LEFT_TOP,
  GC_ALIGN_CENTER,
  GC_ALIGN_RIGHT_BOTTOM
} TAlignment;

/**
 * Special text element class.
 * Note: captions are directly bound to their owning parent element and not handled as a separate child element.
 */
class CCaptionElementTemplate
{
  friend class CFigureElementTemplate;
  friend class CFigureElement;
private:
  wstring FText;                       // The text of the caption.
  wstring FKey;                        // An identifier that can be used to search for this template.
  string FFontFamily;                  // The font to be used for output (e.g. Arial, Verdana)
  int FFontSize;                       // Font size in points.
  int FWeight;                         // The "boldness" of the text.
  string FFontStyle;                   // normal or italic
  TAlignment FHorizontalAlignment;     // Left, center, right
  TAlignment FVerticalAlignment;       // Top, center, bottom
  TBoundingBox FBoundingBox;           // The box that includes the text.
  CFigureElementTemplate* FParent;     // The parent element of this element (if at all).
  GLfloat FColor[4];                   // The text color.
  bool FHasColor;                      // True, if this caption has an own color.
  bool FWrapText;
  TConstraints FConstraints;           // Size constraints for the element. A value of -1 means no constraint.
  float FOffsetX;                      // Horizontal fine tuning offset.
  float FOffsetY;                      // Vertical fine tuning offset.
public:
  CCaptionElementTemplate(wstring key);

  void initialize(wstring Text, float X, float Y, string FontFamily, int FontSize, int Weight, string FontStyle, 
    TAlignment HorzontalAlignment, TAlignment VerticalAlignment, GLubyte* Color, const TConstraints& Constraints,
    bool WrapText);
  wstring key(void) { return FKey; };
};

/**
 * CFigureTemplate is a description of how a concrete figure has to look and act. It is loaded from a description file
 * and created by the figure parser.
 */
class CFigureTemplate: public CGCBase
{
  friend class CFigureParser;
private:
  wstring FType;                       // The type for which this template can be used.
  wstring FClass;                      // A string describing the kind of layout realized in the template. 
  CFigureElementTemplate* FContent;    // The root element of the template.
  CGCModel* FModel;
public:
  CFigureTemplate(CGCModel* model, wstring type, wstring layoutClass);
  virtual ~CFigureTemplate(void);

  CFigureElementTemplate* content(void) { return FContent; };
  wstring layoutClass(void) { return FClass; };
  wstring type(void) { return FType; };
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * Instance for figure elements and captions.
 */
class CCaptionElement: public CGraphicElement
{
  friend class CFigureElement;
  friend class CAnimation;
private:
  struct CTextLineLayout
  {
    float xOffset;
    float yOffset;
    float width;
    wstring text;
  };
  typedef vector<CTextLineLayout> CTextLayout;
  
  wstring FOriginalText;               // The full text assigned to this element. We do not display it fully if it is
                                       // too much. FText contains the actually displayed text.
  wstring FText;                       // The display text of the caption.
  CTextLayout FTextLayout;             // The text that is actually displayed (might be shortend with ellipsis).
  string FFontFamily;                  // The font to be used for output (e.g. Arial, Verdana)
  int FFontSize;                       // Font size in points at 100% zoom.
  int FActualFontSize;                 // Actual font size in points, adjusted by current zoom.
  int FWeight;                         // The "boldness" of the text.
  string FFontStyle;                   // normal or italic
  string FFontId;                      // A unique identifier describing a font with the above properties.
  string FRenderFontId;                // Another identifier for this font, but in its actual size.
  float FAscender;                     // The number of units above the base line. Needed for the correct paint offest.
  TAlignment FHorizontalAlignment;     // Left, center, right
  TAlignment FVerticalAlignment;       // Top, center, bottom
  CFigureElement* FParent;             // The parent element of this element (if at all).
  GLfloat FColor[4];                   // The text color.
  bool FHasColor;                      // True, if this caption has an own color.
  bool FWrapText;                      // True if the text should be wrapped around.
  float FOffsetX;                      // Horizontal fine tuning offset given by the template.
  float FOffsetY;                      // Vertical fine tuning offset given by the template.
  TConstraints FConstraints;           // Size constraints for the element. A value of -1 means no constraint.
  TBidiMode FBidiMode;                 // Determines text directionality.
  float FLineHeight;                   // The height of line of text.

protected:
  TBoundingBox breakLine(const wstring& line, wstring::size_type  start, wstring::size_type & stop, 
    float availableWidth);
  void computeLayout(CTextLineLayout& layout, float availableWidth, float availableHeight, float ellipsisWidth, bool freeSpace);
  CTextLayout createTextLayout(float availableWidth, float availableHeight, float& totalHeight, bool freeSpace);
public:
  CCaptionElement(CGenericCanvas* canvas);
  virtual ~CCaptionElement(void);

  virtual void __cdecl makeDirty(TGCChangeReason reason = GC_CHANGE_NONE);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void render(void);
  void text(wstring newText);
  virtual void __cdecl validate(void);
  void zoomChanged(float ZoomFactor);
};

class GENERIC_CANVAS_API CFigureElement: public CGraphicElement
{
  friend class CCaptionElement;
  friend class CAnimation;
private:
  class CElementListener: private CGCBaseListener
  {
    friend class CFigureElement;
  protected:
      CFigureElement* element;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* sender);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  float FTranslation[3];          // Determines the location of the element after layouting.
  TFigureElementLayout FLayout;   // How are the element's children laid out.
  TFigureElementResize FResizeMode;  // Indicates how this element can be resized.
  CElementList FChildren;         // A list of child elements attached to this element.
  CGCStyle* FStyle;               // The style to be used for this element.
  CFigureElement* FParent;        // The parent element of this element (if at all).
  CFigure* FFigure;               // The figure this element belongs to.
  CCaptionElement* FCaption;      // An optional caption for this element.
  CLayouter* FLayouter;           // The class that computes the layout.
  bool FExpanded;                 // True if child elements are visible.
  TConstraints FConstraints;      // Size constraints for the element. A value of -1 means no constraint.
  CElementListener FListener;
  CFigureElementTemplate* FTemplate; // Reference to the template part for this element.
  float FForcedWidth;             // >= 0 if a certain width is assigned.
  float FForcedHeight;            // >= 0 if a certain height is assigned.
  float FStyleScaleX;             // Horizontal scale factor in case the style (if any) does not have the same size as the element.
  float FStyleScaleY;             // Dito for y direction.
  bool FNeedStyleScale;           // Flag for fast determination of needed scaling.
protected:
  void computeBoundingBox(void);
  TVertex computeOffset(CFigureElement* child);
  TAction* doAction(TAction* action);
public:
  CFigureElement(CFigureElementTemplate* aTemplate, CGenericCanvas* canvas);
  virtual ~CFigureElement(void);

  CFigureElement* addSubElement(void);
  bool canResize(bool vertical);
  CElementList* children(void) { return &FChildren; };
  static CFigureElement* createFromTemplate(CGenericCanvas* canvas, CFigure* Owner, CFigureElementTemplate* Template);
  CFigureElement* elementFromId(wstring id);
  CFigureElement* elementFromPoint(TVertex coords);
  TVertex elementToFigure(TVertex coords);
  TAction* executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords);
  TAction* executeAction(TActionType action);
  void expanded(bool isExpanded);
  bool expanded(void) { return FExpanded; };
  virtual CFigure* __cdecl figure(void) { return FFigure; };
  TVertex figureToElement(TVertex coords);
  virtual TFeedbackInfo __cdecl getFeedbackInfo(TModifiers modifiers, const TVertex& coords);
  TFigureElementLayout layout(void) { return FLayout; };
  virtual void __cdecl makeDirty(TGCChangeReason reason = GC_CHANGE_NONE);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void render(void);
  bool resize(float dX, float dY, TFeedbackInfo info);
  void setCaption(const char* text);
  void style(CGCStyle* NewStyle);
  CGCStyle* style(void);
  CFigureElementTemplate* template_(void) { return FTemplate; };
  virtual void __cdecl validate(void);
  void zoomChanged(float newZoom);
};

//----------------------------------------------------------------------------------------------------------------------

typedef map<wstring, CFigureElement*> CFigureElementMap;

/**
 * CFigure is the main element in the model and is created from a figure template. It cannot itself appear in a scene but
 * is represented by one or more figure instances.
 */
class GENERIC_CANVAS_API CFigure: public CGraphicElement
{
  friend class CGCModel;
  friend class CFigureInstance;
  friend class CAnimation;
private:
  class CFigureElementListener: private CGCBaseListener
  {
    friend class CFigure;
  protected:
      CFigure* figure;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* sender);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* message);
  };

  GLuint FDisplayList;

  CGCModel* FModel;          // The model to which this figure belongs.
  CFigureElement* FContent;  // The root element of the figure.
  float FScaling[3];         // The factors to scale the figure in each direction.
  float FMinimalScale;       // The lesser of x and y scale factors.
  float FTranslation[3];     // The factors to move the figure.
  float FRotation[4];        // The factors to rotate the figure. FRotation[0] is an angle in radians. Index 1-3 form a vector to rotate around.
                             // Note: Order of application of the 3 transformations is scaling first, then rotation, finally translation.
  TMatrix FMatrix;           // All three local transformations in one matrix.
  TMatrix FInverse;          // FMatrix inverted.
  float FLastZoom;           // The most recent minimal overall scale factor (including its own scale factors).
  CFigureTemplate* FTemplate; // The template from which this figure was built.
  CFigureElementListener FListener;
  CFigureElementMap FElementMap; // A map for quick lookup of figure elements given by a path.
protected:
  void applyTransformations(void);
  void buildFromTemplate(CFigureTemplate* Template);
  void updateMatrix(void);
  void zoomChanged(float newZoom);
public:
  CFigure(CGCModel* Owner, CFigureTemplate* Template);
  virtual ~CFigure(void);

  void addMapping(wstring path, CFigureElement* element);
  CFigureElement* content(void) { return FContent; };
  CFigureElement* elementFromId(const wstring& id);
  CFigureElement* elementFromKey(const char* key);
  CFigureElement* elementFromKey(wstring key);
  virtual CFigureElement* __cdecl elementFromPoint(TVertex coords);
  virtual void __cdecl figureToView(TVertex original, CFigureInstance* instance, TVertex& result);
  void freeNotification(CFigureElement* object);
  CGCModel* model(void) { return FModel; };
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  void removeMapping(CFigureElement* element);
  virtual void __cdecl render(void);
  virtual void __cdecl rotate(float Angle, float Rx, float Ry, float Rz);
  virtual void __cdecl rotateV(float Angle, const float Axis[3]);
  virtual void __cdecl scale(float Sx, float Sy, float Sz, bool Accumulative = false);
  virtual void __cdecl scaleV(const float Factor[3], bool Accumulative = false);
  virtual bool __cdecl setSize(float width, float height);
  CFigureTemplate* template_(void) { return FTemplate; };
  virtual void __cdecl translate(float Tx, float Ty, float Tz, bool Accumulative = false);
  virtual void __cdecl translateV(const float Factor[3], bool Accumulative = false);
  virtual void __cdecl validate(void);
  virtual void __cdecl viewToFigure(TVertex original, CFigureInstance* instance, TVertex& result);
};

//----------------------------------------------------------------------------------------------------------------------

/**
 * The figure instance class is a proxy for a figure on a particular layer. There can be more than one instance pointing 
 * to the same figure.
 */
class GENERIC_CANVAS_API CFigureInstance: public CGraphicElement
{
  friend class CGCView;
  friend class CLayer;
  friend class CFigure;
  friend class CFeedbackLayer;
  friend class CFigureListener;
  
  // For animations:
  friend class CFigureInstancePathAnimation;
private:
  class CFigureListener: public CGCBaseListener
  {
    friend class CFigureInstance;
  protected:
    CFigureInstance* instance;
  public:
    virtual void __cdecl onAction(CGCBase* sender, CGCBase* origin, TAction** action);
    virtual void __cdecl onChange(CGCBase* sender, CGCBase* origin, TGCChangeReason reason);
    virtual void __cdecl onDestroy(CGCBase* sender);
    virtual void __cdecl onError(CGCBase* sender, CGCBase* origin, const char* Message);
  };

  CLayer* FLayer;            // The layer on which this figure is displayed.
  CFigure* FFigure;          // The figure of which this class is an instance.

  // Drop shadow support.
  GLuint FShadow;            // The display list for the shadow, if any.
  GLuint FShadowTexture;     // The texture which is drawn as shadow. Create from the actual content.
  float FLastWidth;
  float FLastHeight;

  float FScaling[3];         // The factors to scale the figure in each direction.
  float FTranslation[3];     // The factors to move the figure.
  float FRotation[4];        // The factors to rotate the figure. FRotation[0] is an angle in radians. Index 1-3 form a vector to rotate around.
                             // Note: Order of apllication of the 3 transformations is scaling first, then rotation, finally translation.

  float FMinimalScale;       // The lesser of x and y scale factors.
  TMatrix FMatrix;           // All three local transformations in one matrix.
  TMatrix FInverse;          // FMatrix inverted.
  bool FSelected;            // True if this instance is currently selected.
  CFigureListener FListener;
protected:
  void applyTransformations(void);
  void makeShadow(void);
  void resize(float dX, float dY, TFeedbackInfo info);
  void updateMatrix(void);
public:
  CFigureInstance(CLayer* Owner, CFigure* Figure);
  virtual ~CFigureInstance(void);

  virtual CFigureElement* __cdecl elementFromPoint(TVertex coords);
  virtual TAction* __cdecl executeAssociatedActions(TMouseButton button, TMouseEvent event, TModifiers modifiers, const TVertex& coords);
  virtual CFigure* __cdecl figure(void) { return FFigure; };
  virtual TFeedbackInfo __cdecl getFeedbackInfo(TModifiers modifiers, const TVertex& coords);
  virtual void __cdecl instanceToView(TVertex original, TVertex& result);
  virtual CLayer* __cdecl layer(void) { return FLayer; };
  virtual TBoundingBox __cdecl localBounds(void);
  virtual void __cdecl makeDirty(TGCChangeReason reason = GC_CHANGE_NONE);
  void onDestroy(CGCBase* figure);
  virtual TGCVariant __cdecl propertyGet(const char* name, unsigned int index);
  virtual void __cdecl propertySet(const char* name, unsigned int index, TGCVariant value);
  virtual void __cdecl render(void);
  void replaceFigure(CFigure* figure);
  virtual void __cdecl rotate(float angle, float rx, float ry, float rz);
  virtual void __cdecl rotateV(float angle, const float axis[3]);
  virtual void __cdecl scale(float sx, float sy, float sz, bool accumulative = false);
  virtual void __cdecl scaleV(const float factor[3], bool accumulative = false);
  virtual bool __cdecl selected(void);
  virtual void __cdecl setSize(float width, float height);
  virtual void __cdecl translate(float tx, float ty, float tz, bool accumulative = false);
  virtual void __cdecl translateV(const float factor[3], bool accumulative = false);
  virtual void __cdecl validate(void);
  virtual void __cdecl viewToInstance(TVertex original, TVertex& result);
  void zoomChanged(float newZoom);
};

#endif // __GC_FIGURE_H__
