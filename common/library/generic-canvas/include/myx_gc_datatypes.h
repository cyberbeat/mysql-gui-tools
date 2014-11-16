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
 * @file myx_gc_datatypes.h 
 * @brief Some commonly used data types.
 * 
 */

#ifndef __GC_DATATYPES_H__
#define __GC_DATATYPES_H__

#include "myx_gc_variant.h"

//----------------- Macros ---------------------------------------------------------------------------------------------

// There is no ANSI C rounding function for float numbers, so define our own.
#define ROUND(X) (int)(floor((X) + 0.5))

#define XML_IS(node, type) (xmlStrcmp(node->name, (const xmlChar *) type) == 0)

//----------------- Data types -----------------------------------------------------------------------------------------

#ifdef _WINDOWS
  // An opaque handle to a rendering context. Must be provided by the viewer.
  typedef HDC GCContext;
#elif defined(__APPLE__)
  typedef CGLContextObj GCContext;
#else
  typedef GLXContext GCContext;
#endif // ifdef _WINDOWS

class CGCBase;
class CFigure;

typedef struct tagViewport
{
  int left, top, width, height;
  tagViewport(int iLeft, int iTop, int iWidth, int iHeight)
  {
    left = iLeft;
    top = iTop;
    width = iWidth;
    height = iHeight;
  }
  tagViewport()
  {
    left = 0;
    top = 0;
    width = 0;
    height = 0;
  };
} TGCViewport;

// Some geometric data types.
typedef struct tagVertex
{
  float x;
  float y;
  float z;
  float w;

  tagVertex(float aX, float aY, float aZ, float aW)
  {
    x = aX;
    y = aY;
    z = aZ;
    w = aW;
  };
  tagVertex(double aX, double aY, double aZ)
  {
    x = (float) aX;
    y = (float) aY;
    z = (float) aZ;
    w = 1;
  };
  tagVertex()
  {
    x = 0;
    y = 0;
    z = 0;
    w = 1;
  };
  tagVertex operator +(const tagVertex& other) const
  {
    tagVertex result = other;
    result.x = other.x / other.w + x / w;
    result.y = other.y / other.w + y / w;
    result.z = other.z / other.w + z / w;
    result.w = w;
    return result;
  };
  tagVertex operator -(const tagVertex& other) const
  {
    tagVertex result = other;
    result.x = x / w - other.x / other.w;
    result.y = y / w - other.y / other.w;
    result.z = z / w - other.z / other.w;
    result.w = w;
    return result;
  };
} TVertex;

typedef vector<TVertex> CText;
typedef vector<TVertex> CVertexVector;

typedef GLfloat TMatrix[16];

typedef struct tagBoundingBox
{
  TVertex upper;
  TVertex lower;

  tagBoundingBox()
  {
  };

  tagBoundingBox(TVertex aUpper, TVertex aLower)
  {
    upper = aUpper;
    lower = aLower;
  };

  float width(void)
  {
    return lower.x - upper.x;
  };

  float height(void)
  {
    return lower.y - upper.y;
  };
} TBoundingBox;

typedef struct tagConstraints
{
  float maxHeight;
  float maxWidth;
  float minHeight;
  float minWidth;

  tagConstraints()
  {
    maxHeight = -1;
    maxWidth = -1;
    minHeight = -1;
    minWidth = -1;
  };
} TConstraints;

typedef enum tagGCError
{
  GC_NO_ERROR = 0, 
  GC_CANT_OPEN_FILE, 
  GC_XML_PARSE_ERROR,
  GC_XML_INVALID_DOCUMENT, 
  GC_XML_EMPTY_DOCUMENT, 
  GC_OBJECT_NOT_FOUND, 
  GC_CANT_READ_FROM_FILE, 
  GC_CHARSET_CONVERSION_ERROR,
  GC_CHARSET_WRONG_CHARSET_SPECIFIED
} TGCError;

typedef enum tagChangeReason
{
  GC_CHANGE_NONE,

  // Selection related changes.
  GC_CHANGE_SELECTION_ADD,             // One or more figure instances were added to the current selection.
  GC_CHANGE_SELECTION_CLEAR,           // The current selection was cleared.
  GC_CHANGE_SELECTION_REMOVE,          // One or more figure instances were removed from the current selection.
  GC_CHANGE_SELECTION_CHANGE,          // One or more figure instances were added to or removed from the current selection.

  // Canvas related changes.
  GC_CHANGE_CANVAS_REFRESH,            // Used to indicate that the view must update the visual representation.
  GC_CHANGE_CANVAS_PROPERTY,           // The value of a property has been changed.
  GC_CHANGE_CANVAS_ADD_VIEW,           // A new view was added.
  GC_CHANGE_CANVAS_ADD_LAYER,          // A new layer was added.
  GC_CHANGE_CANVAS_SWITCH_VIEW,        // Another view was activated.
  GC_CHANGE_CANVAS_REMOVE_VIEW,        // A view was removed.
  GC_CHANGE_CANVAS_REMOVE_LAYER,       // A layer was removed.
  GC_CHANGE_CANVAS_CLEAR_CONTENT,      // All figures have been removed.
  GC_CHANGE_CANVAS_CLEAR_LAYOUTS,      // All layout definitions have been removed.
  GC_CHANGE_CANVAS_CLEAR_STYLES,       // All styles have been removed.

  // Model related changes.
  GC_CHANGE_MODEL_PROPERTY,            // The value of a property has been changed.
  GC_CHANGE_MODEL_ADD_FIGURE,          // A new figure was added.
  GC_CHANGE_MODEL_REMOVE_FIGURE,       // A figure was removed.
  GC_CHANGE_MODEL_ADD_STYLE,           // A new style was added.
  GC_CHANGE_MODEL_REMOVE_STYLE,        // A style was removed.

  // Caption element related changes.
  GC_CHANGE_CAPTION_PROPERTY,          // The value of a property has been changed.

  // Figure element related changes.
  GC_CHANGE_ELEMENT_PROPERTY,         // The value of a property has been changed.
  GC_CHANGE_ELEMENT_ADD_CHILD,        // The value of a property has been changed.

  // Figure related changes.
  GC_CHANGE_FIGURE_PROPERTY,           // The value of a property has been changed.
  GC_CHANGE_FIGURE_EXCHANGE,           // A figure is about to be replaced by another one.
  GC_CHANGE_FIGURE_RESIZE,             // A figure was resized.

  // Figure instance related changes.
  GC_CHANGE_FINSTANCE_PROPERTY,        // The value of a property has been changed.

  // View related changes.
  GC_CHANGE_VIEW_PROPERTY,             // The value of a property (other than the zoom) has been changed.
  GC_CHANGE_VIEW_ZOOM,                 // The zoom has been changed.
  GC_CHANGE_VIEW_OFFSET,               // The offset has been changed.
  GC_CHANGE_VIEW_ADD_LAYER,            // A new layer was added to a view.
  GC_CHANGE_VIEW_REMOVE_LAYER,         // A layer was removed.
  GC_CHANGE_VIEW_CLEAR,                // The view was cleared.
  GC_CHANGE_VIEW_OVERVIEW_START,       // The view started overview mode.
  GC_CHANGE_VIEW_OVERVIEW_STOP,        // The view stopped overview mode.
  GC_CHANGE_VIEW_RUBBERRECT_START,     // The view started rubber rect mode.
  GC_CHANGE_VIEW_RUBBERRECT_STOP,      // The view stopped rubber rect mode.
  GC_CHANGE_VIEW_RUBBERBAND_START,     // The view started rubber band mode.
  GC_CHANGE_VIEW_RUBBERBAND_STOP,      // The view stopped rubber band mode.
  GC_CHANGE_VIEW_DRAGGING_START,       // The view started dragging figures.
  GC_CHANGE_VIEW_DRAGGING_STOP,        // The view stopped dragging figures.
  GC_CHANGE_VIEW_WORKSPACE,            // The workspace size 

  // Layer related changes.
  GC_CHANGE_LAYER_CLEAR,               // All figure instances on the layer are removed.
  GC_CHANGE_LAYER_VISIBILITY,          // The visibility of a layer has been changed.
  GC_CHANGE_LAYER_PROPERTY,            // The value of a property has been changed.
  GC_CHANGE_LAYER_ADD_INSTANCE,        // A new figure instance was added.
  GC_CHANGE_LAYER_REMOVE_INSTANCE,     // A figure instance was removed.
  GC_CHANGE_LAYER_ADD_GROUP,           // A new group was added to a view.
  GC_CHANGE_LAYER_REMOVE_GROUP,        // A group was removed.

  // Connections
  GC_CHANGE_CONNECTION_DECORATION,     // A connection's decoration data has changed.
  GC_CHANGE_CONNECTION_LABEL,          // One of the labels of a connection's decorations has changed.
  GC_CHANGE_CONNECTION_INSTANCE        // A property in connection instance has changed.
} TGCChangeReason;

typedef enum tagFeedbackInfo
{
  GC_FI_NONE,                          // No hit found at all.
  GC_FI_ON_OBJECT,                     // Anywhere on the object, not more details for the hit available.
  GC_FI_HLINE_MOVE,                    // A horizontal line can be moved (e.g. connection center, guide).
  GC_FI_VLINE_MOVE,                    // A vertical line can be moved (e.g. connection center, guide).
  GC_FI_CONNECTION,                    // A connection was hit.
  GC_FI_SELECTION_BODY,                // A selected figure instance was hit (but not the corners or edges).
  GC_FI_RESIZE_NORTH,                  // The north edge of a selection was hit. Similarly for the other directions.
  GC_FI_RESIZE_NORTH_EAST,
  GC_FI_RESIZE_EAST,
  GC_FI_RESIZE_SOUTH_EAST,
  GC_FI_RESIZE_SOUTH,
  GC_FI_RESIZE_SOUTH_WEST,
  GC_FI_RESIZE_WEST,
  GC_FI_RESIZE_NORTH_WEST,
  GC_FI_TOGGLE,                        // A GC_ACTION_TOGGLE action can be exectuted.
  GC_FI_EXPAND,                        // A GC_ACTION_EXPAND action can be exectuted.
  GC_FI_COLLAPSE,                      // A GC_ACTION_COLLAPSE action can be exectuted.
  GC_FI_CHANGE_STYLE,                  // A GC_ACTION_CHANGE_STYLE action can be exectuted.
  GC_FI_DRAG_FIGURE,                   // A GC_ACTION_DRAG_FIGURE action can be exectuted.
  GC_FI_DRAG_ALL,                      // A GC_ACTION_DRAG_ALL action can be exectuted.
  GC_FI_FLOATER,                       // The coordinates are within the bounds of a floater.
  GC_FI_DRAG_LABEL                     // One of the labels of a connection can be dragged.
} TFeedbackInfo;

/**
 * TRubberRectStyle describes the look of the rubber rectangle in the feedback layer.
 */
typedef enum tagRubberRectStyle
{
  GC_RRSTYLE_SOLID_THIN,               // A simple black rectangle with a one pixel wide border.
  GC_RRSTYLE_SOLID_THICK,              // A simple black rectangle with a 3 pixel wide border.
  GC_RRSTYLE_DOTTED_THIN,              // A simple black rectangle with a one pixel wide dotted border.
  GC_RRSTYLE_DOTTED_THICK,             // A simple black rectangle with a 3 pixel wide dotted border.
  GC_RRSTYLE_BLENDED_CHECKERBOARD,     // A filled rectangle with a one pixel border and a translucent interior.
                                       // The system's selection color is used. The interior is a checker board.
  GC_RRSTYLE_BLENDED_DIAGONALS,        // A filled rectangle with a one pixel border and a translucent interior.
                                       // The system's selection color is used. The interior consists of diagonal bands.
  GC_RRSTYLE_BLENDED_SOLID
} TRubberRectStyle;

/**
 * TRubberBandStyle describes the look of the rubber band in the feedback layer.
 */
typedef enum tagRubberBandStyle
{
  GC_RBSTYLE_SOLID_THIN,               // A simple black rectangle with a one pixel wide border.
  GC_RBSTYLE_SOLID_THICK,              // A simple black rectangle with a 3 pixel wide border.
  GC_RBSTYLE_DOTTED_THIN,              // A simple black rectangle with a one pixel wide dotted border.
  GC_RBSTYLE_DOTTED_THICK,             // A simple black rectangle with a 3 pixel wide dotted border.
} TRubberBandStyle;

/**
 * TRRSelectionAction (rubber rect selection action) determines how to manipulate the selection state of figure
 * instances with regard to their bounding box intersecting with the rubber rectangle.
 */
typedef enum tagRRSelectionAction
{
  GC_RRACTION_NONE,               // Don't touch the selection state of any figure instance.
                                  // Usually used for non-selecting rubber rectangless (e.g. for figure creation).
  GC_RRACTION_SELECT,             // Always select figure instances if their bounding box intersects. Keep selected 
                                  // instances as their are if the do not intersect anymore.
                                  // Usually used for rubber rectangles with pressed shift key modifier.
  GC_RRACTION_SELECT_REMOVE,      // Select figure instances if they intersect, unselect those, which do not intersect.
                                  // Most common rubber rectangle selection mode.
  GC_RRACTION_TOGGLE              // Revert the selection state of figure instances, which intersect. Don't touch the others.
                                  // Usually used for rubber rectangles with pressed control key modifier.
} TRRSelectionAction;

/** Layout variants for a figure element. */
typedef enum tagFigureElementLayout
{
  GC_LAYOUT_ROW,     
  GC_LAYOUT_COLUMN
} TFigureElementLayout;

/** Resize variants for a figure element. */
typedef enum tagFigureElementResize
{
  GC_RESIZE_NONE,
  GC_RESIZE_HORIZONTAL_ONLY,     
  GC_RESIZE_VERTICAL_ONLY,
  GC_RESIZE_ALL
} TFigureElementResize;

/** Bidirectional mode */
typedef enum tagBidiMode
{
  GC_BIDI_LEFT_TO_RIGHT,          // Standard directionality (most languages).
  GC_BIDI_RIGHT_TO_LEFT           // Used for arabic and hebrew text.
} TBidiMode;

/** Style for a connection line. */
typedef enum tagConnectionLineStyle
{
  GC_CONNECTION_STYLE_SOLID,
  GC_CONNECTION_STYLE_DOTTED
} TConnectionLineStyle;

/** Info block for connection decoration and labeling. */
typedef struct tagConnectionDecorations
{
  CFigure* end1Decoration;
  CFigure* end1Label;
  CFigure* centerDecoration;
  CFigure* centerLabel;
  CFigure* end2Decoration;
  CFigure* end2Label;
} TConnectionDecorations;

/** Identifier for containers. Used when parsing pathes for properties. */
typedef enum tagContainerID
{
  GC_CONTAINER_UNKNOWN,
  GC_CONTAINER_LAYERS,
  GC_CONTAINER_FEEDBACK,
  GC_CONTAINER_FIGURE_INSTANCES,
  GC_CONTAINER_FIGURE_CONTENT,
  GC_CONTAINER_VIEWS,
  GC_CONTAINER_MODEL,
  GC_CONTAINER_STYLE,
  GC_CONTAINER_STYLES,
  GC_CONTAINER_LAYOUTS,
  GC_CONTAINER_FIGURE,
  GC_CONTAINER_FIGURES,
  GC_CONTAINER_SCALING,
  GC_CONTAINER_TRANSLATION,
  GC_CONTAINER_ROTATION,
  GC_CONTAINER_GROUPS,
  GC_CONTAINER_CHILDREN,
  GC_CONTAINER_CAPTION,
  GC_CONTAINER_CONTENT,
  GC_CONTAINER_COLOR,
  GC_CONTAINER_HOT_COLOR,

  // Connections
  GC_CONTAINER_CONNECTION,
  GC_CONTAINER_END1,
  GC_CONTAINER_END1_DECORATION,
  GC_CONTAINER_END1_LABEL,
  GC_CONTAINER_END2,
  GC_CONTAINER_END2_DECORATION,
  GC_CONTAINER_END2_LABEL,
  GC_CONTAINER_CENTER_DECORATION,
  GC_CONTAINER_CENTER_LABEL
} TContainerID;

/** Identifier for properties. Used when parsing property specifications. */
typedef enum tagPropertyID
{
  // Simple properties.
  GC_PROPERTY_UNKNOWN,
  GC_PROPERTY_NAME,
  GC_PROPERTY_ID,
  GC_PROPERTY_WIDTH,
  GC_PROPERTY_HEIGHT,
  GC_PROPERTY_X,
  GC_PROPERTY_Y,
  GC_PROPERTY_Z,
  GC_PROPERTY_DESCRIPTION,
  GC_PROPERTY_ZOOM,
  GC_PROPERTY_COLOR,
  GC_PROPERTY_JITTER,
  GC_PROPERTY_ANGLE,
  GC_PROPERTY_VISIBLE,
  GC_PROPERTY_ENABLED,
  GC_PROPERTY_SELECTED,
  GC_PROPERTY_LAYOUT,
  GC_PROPERTY_RESIZABLE,
  GC_PROPERTY_EXPANDED,
  GC_PROPERTY_MIN_WIDTH,
  GC_PROPERTY_MIN_HEIGHT,
  GC_PROPERTY_MAX_WIDTH,
  GC_PROPERTY_MAX_HEIGHT,
  GC_PROPERTY_TEXT,
  GC_PROPERTY_FONT_FAMILY,
  GC_PROPERTY_FONT_SIZE,
  GC_PROPERTY_FONT_WEIGHT,
  GC_PROPERTY_FONT_STYLE,
  GC_PROPERTY_ALIGNMENT_VERTICAL,
  GC_PROPERTY_ALIGNMENT_HORIZONTAL,
  GC_PROPERTY_BIDI_MODE,
  GC_PROPERTY_WRAP_TEXT,
  GC_PROPERTY_CENTER,
  GC_PROPERTY_OWNER,

  // Connections
  GC_PROPERTY_LABEL1_OFFSET_X,
  GC_PROPERTY_LABEL1_OFFSET_Y,
  GC_PROPERTY_LABEL2_OFFSET_X,
  GC_PROPERTY_LABEL2_OFFSET_Y,
  GC_PROPERTY_LABEL_CENTER_OFFSET_X,
  GC_PROPERTY_LABEL_CENTER_OFFSET_Y
} TPropertyID;

/** Determines how often a figure element is allowed to appear. */
typedef enum tagOccurence
{
  GC_OCC_ONCE,               // A single instance element (default).
  GC_OCC_ZERO_OR_MORE,       // An element in a list that can appear in any number.
  GC_OCC_ONE_OR_MORE         // An element in a list that must exist at least once.
} TOccurence;

/** Indicates which mouse event is to be handled. */
typedef enum tagMouseEvent
{
  GC_MOUSE_IGNORE,
  GC_MOUSE_DOWN,
  GC_MOUSE_UP,
  GC_MOUSE_MOVE,
  GC_MOUSE_DBL_CLICK
} TMouseEvent;

/**
 * Modifier keys used when handling mouse input. Any value can be combined with other values, except for 
 * GC_MODIFIER_IGNORE and GC_MODIFIER_NONE, which must appear alone. 
 */
#define GC_MODIFIER_IGNORE  0x00 // Don't care about the modifier.
#define GC_MODIFIER_NONE    0x01 // No modifier must be active.
#define GC_MODIFIER_SHIFT   0x02 // Shift key (all platforms).
#define GC_MODIFIER_CONTROL 0x04 // Control key (all platforms).
#define GC_MODIFIER_ALT     0x08 // Alternative key (on MacOS X Apple key).
#define GC_MODIFIER_COMMAND 0x10 // Command key (MacOS X only)
#define GC_MODIFIER_OPTION  0x20 // Option key (MacOS X only)

typedef unsigned int TModifiers;

/** 
 * Used in mouse events to specify which mouse button is involved. For one button only systems like MacOS 
 * the left button inidicator is used for this (only) button.
 */
typedef enum tagMouseButton
{
  GC_MOUSE_BUTTON_IGNORE,
  GC_MOUSE_BUTTON_NONE,
  GC_MOUSE_BUTTON_LEFT,
  GC_MOUSE_BUTTON_MIDDLE,
  GC_MOUSE_BUTTON_RIGHT,
  GC_MOUSE_BUTTON_X1,
  GC_MOUSE_BUTTON_X2
} TMouseButton;

/** Determines the type of action possible with a particular figure instance/element. */
typedef enum tagActionType
{
  GC_ACTION_NONE,            // No action is assigned.
  GC_ACTION_TOGGLE,          // Switch between expanded and collapsed state (scope: element).
  GC_ACTION_EXPAND,          // Expand the element (scope: element).
  GC_ACTION_COLLAPSE,        // Collapse the element (scope: element).
  GC_ACTION_CHANGE_STYLE,    // Switch to a new style (scope: element).
  GC_ACTION_DRAG,            // A figure element wants to start dragging.
  GC_ACTION_DRAG_INSTANCE,   // A view starts dragging a figure instance.
  GC_ACTION_CONNECTION,      // An action handled by the connection layer.
  GC_ACTION_RESIZE,          // The figure is resized.
  GC_ACTION_FLOATER_DRAG,    // A floater will be dragged.
  GC_ACTION_RUBBER_RECT,     // A rubber rectangle will be opened.
  GC_ACTION_RUBBER_BAND,     // Dito for rubber band.

  GC_ACTION_APPLICATION = 1000 // Application defined action codes start here. Do not define codes below this value.
} TActionType;

/** Used to query the canvas for OpenGL extension support. */
typedef enum tagOglExtension
{
  GC_OE_MULTISAMPLING,
  GC_OE_CONVOLUTION,
  GC_OE_HISTOGRAM,
  GC_OE_TEXTURE3D,
  GC_OE_FRAME_BUFFER_OBJECTS
} TOglExtension;

typedef vector<bool> TOglExtensions;

/** Decorations used for a floater on the feedback layer. */
typedef enum tagFloaterDecoration
{
  GC_FLOATER_DECO_NONE,          // No decoration. The floater is simply filled in one color and has a slightly darker border.
  GC_FLOATER_DECO_CHECKER_BOARD, // A checker board.
  GC_FLOATER_DECO_CUT_MARKERS,   // A rectangle with cut markers at the corners.
  GC_FLOATER_DECO_ROMAN_1,       // The roman number 1 on a simple background.
  GC_FLOATER_DECO_ROMAN_2,       // The roman number 2 etc.
  GC_FLOATER_DECO_ROMAN_3,
  GC_FLOATER_DECO_ROMAN_4,
  GC_FLOATER_DECO_ROMAN_5,
  GC_FLOATER_DECO_ROMAN_6,
  GC_FLOATER_DECO_ROMAN_7,
  GC_FLOATER_DECO_ROMAN_8,
  GC_FLOATER_DECO_ROMAN_9,
  GC_FLOATER_DECO_ROMAN_10
} TFloaterDecoration;

/** Types of primitives, which can be created in a view. */
typedef enum tagPrimitiveType
{
  GC_PRIMITIVE_LINE,
  GC_PRIMITIVE_CIRCLE,
  GC_PRIMITIVE_RECTANGLE,
  GC_PRIMITIVE_POLYGON
} TPrimitiveType;

/** The format used when rendering to a file. */
typedef enum tagGCFileFormat
{
  GC_FILE_FORMAT_PDF,   // Portable Document Format (Adobe)
  GC_FILE_FORMAT_PS,    // Postscript
  GC_FILE_FORMAT_EPS,   // Encapsulated postscript
  GC_FILE_FORMAT_PNG    // A PNG file
} TGCFileFormat;

/** The color format used when rendering to memory. */
typedef enum tagGCColorFormat
{
  GC_COLOR_FORMAT_RGBA, // The usual red, green, blue, alpha color component order.
  GC_COLOR_FORMAT_BGRA  // This format is used on Windows.
} TGCColorFormat;

/** Determines the content when rendering to memory. This type is used as a set. */
typedef enum tagGCRenderContent
{
  GC_RENDER_GRID        = 0x01,  // Render background grid.
  GC_RENDER_CONNECTIONS = 0x02,  // Render connection layer content.
  GC_RENDER_FEEDBACK    = 0x04,  // Render feedback layer content.
  GC_RENDER_FRAME       = 0x08,  // Render workspace rectangle.
  GC_RENDER_FOCUS       = 0x10,  // Render focus marker for the canvas itself
  GC_RENDER_EVERYTHING  = 0xFF   // Render all elements.
} TGCRenderContent;

typedef vector<wstring> CActionParameters;

/** A combination of mouse and key input used to trigger an action. */
typedef struct tagTrigger
{
  TMouseButton button;
  TMouseEvent event;
  TModifiers modifiers;
} TTrigger;

typedef vector<TTrigger> CTriggers;

/** An action with associated parameters. */
typedef struct tagAction
{
  TActionType type;
  CActionParameters parameters;
  CTriggers triggers;
  TFeedbackInfo feedbackInfo;     // Used e.g. for a resize action. Contains the related feedback flag.
} TAction;

typedef vector<TAction*> CActions;
typedef map<string, unsigned char*> CColorMap;
typedef map<string, unsigned char*>::const_iterator CColorMapIterator;
typedef pair<string, unsigned char*> CColorMapPair;

/** Lists of styles, figure templates, figures and figure instances. */
class CGCStyle;
#ifdef __GNUC__
  typedef map<wstring, CGCStyle*> CStyleList;
#else
  typedef map<wstring, CGCStyle*> CStyleList;
#endif

class CFigureElementTemplate;
typedef vector<CFigureElementTemplate*> CElementTemplateList;

class CFigureElement;
typedef vector<CFigureElement*> CElementList;

class CFigureTemplate;
typedef multimap<wstring, CFigureTemplate*> CLayoutList;
typedef pair<wstring, CFigureTemplate*> CLayoutPair;

class CFigure;
typedef vector<CFigure*> CFigureList;

class CFigureInstance;
typedef vector<CFigureInstance*> CFigureInstances;

// A list of connection decors, addressable by name.
class CConnectionDecor;
typedef multimap<wstring, CConnectionDecor*> CConnectionDecors;
typedef pair<wstring, CConnectionDecor*> CConnectionDecorPair;

// A list of connections.
class CConnection;
typedef vector<CConnection*> CConnections;

// A list of textures, addressable by name.
class CGCTexture;
typedef map<wstring, CGCTexture*> CTextures;

// Lists of listeners etc.

// A list of layers.
class CLayer;
typedef vector<CLayer*> CLayers;

// A list of views.
class CGCView;
typedef vector<CGCView*> CViews;

class CGraphicElement;
typedef vector<CGraphicElement*> CGraphicElementList;   

/**
 * A list of texture names each with the level-of-detail they are associated. The index in the vector is
 * also the LOD they stand for.
 */
typedef vector<wstring> TLODList;

//----------------------------------------------------------------------------------------------------------------------

#endif // #ifndef __GC_DATATYPES_H__

