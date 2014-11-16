unit GenericCanvasViewer;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//----------------------------------------------------------------------------------------------------------------------
//
//  Implementation of a viewer for the MySQL generic canvas used to visualize object models.
//
//----------------------------------------------------------------------------------------------------------------------

interface

{$include Compilers.inc}

uses
  Windows, Messages, Controls, Classes, Graphics, Sysutils, Forms, OpenGL_SG;

{$A8} // We need 8 byte alignment as used by C++.
{$Z4} // Enums are always integer size

const
  ScrollTimer = 1;           // The id of the time for scrolling.
  AnimationTimer = 2;        // The id of the time for animations.
  FPSBufferSize = 30;        // The number of values to keep for sliding average computation of the speed.

  WM_PENDING_ZOOM_CHANGE       = WM_APP + 3110; // The zoom was changed in the canvas. Notify application.
  WM_PENDING_UPDATE_SCROLLBARS = WM_APP + 3111; // Scrollbars must be adjusted.

  // Determines the content when rendering to memory.
  GC_RENDER_GRID: Cardinal        = $01;  // Render background grid.
  GC_RENDER_CONNECTIONS: Cardinal = $02;  // Render connection layer content.
  GC_RENDER_FEEDBACK: Cardinal    = $04;  // Render feedback layer content.
  GC_RENDER_FRAME: Cardinal       = $08;  // Render workspace rectangle.
  GC_RENDER_FOCUS: Cardinal       = $10;  // A focus indicator for the canvas itself.
  GC_RENDER_EVERYTHING: Cardinal  = $FF;  // Render all elements.

  // Modifier keys used when handling mouse input. Any value can be combined with other values.
  GC_MODIFIER_IGNORE  = $00; // Don't care about the modifier.
  GC_MODIFIER_NONE    = $01;  // No modifier must be active.
  GC_MODIFIER_SHIFT   = $02;  // Shift key (all platforms).
  GC_MODIFIER_CONTROL = $04;  // Control key (all platforms).
  GC_MODIFIER_ALT     = $08;  // Alternative key (on MacOS X Apple key).
  GC_MODIFIER_COMMAND = $10;  // Command key (MacOS X only)

type
  // Certain keyboard and mouse states must be tracked because they trigger special behavior.
  TGCInputStates = set of
  (
    ksAlt,              // Alt key is down.
    ksCtrl,             // Ctrl key is down.
    ksLeftButtonDown,   // Left mouse button is down.
    ksMiddleButtonDown, // Middle mouse button is down.
    ksRightButtonDown,  // Right mouse button is down.
    ksShift,            // Shift key is down.
    ksSpace             // Space key is down.
  );

const
  MouseStates = [ksLeftButtonDown, ksMiddleButtonDown, ksRightButtonDown];

type
  PViewport = ^TViewport;
  TViewport = record
    Left, Top,
    Width, Height: Integer;
  end;

  TVertex = record
    X, Y, Z, W: Single;
  end;

  TBoundingBox = record
    Upper: TVertex;
    Lower: TVertex;
  end;

  TPointD = record
    X, Y: Double;
  end;

  TGCError = (
    GC_NO_ERROR = 0,
    GC_CANT_OPEN_FILE,
    GC_XML_PARSE_ERROR,
    GC_XML_INVALID_DOCUMENT,
    GC_XML_EMPTY_DOCUMENT,
    GC_OBJECT_NOT_FOUND,
    GC_CANT_READ_FROM_FILE,
    GC_CHARSET_CONVERSION_ERROR,
    GC_CHARSET_WRONG_CHARSET_SPECIFIED
  );

  TGCChangeReason =
  (
    GC_CHANGE_NONE,
    
    // Selection related changes.
    GC_CHANGE_SELECTION_ADD,           // One or more figure instances were added to the current selection.
    GC_CHANGE_SELECTION_CLEAR,         // The current selection was cleared.
    GC_CHANGE_SELECTION_REMOVE,        // One or more figure instances were removed from the current selection.
    GC_CHANGE_SELECTION_CHANGE,        // One or more figure instances were added to or removed from the current selection.

    // Canvas related changes.
    GC_CHANGE_CANVAS_REFRESH,          // Used to indicate that the view must update the visual representation.
    GC_CHANGE_CANVAS_PROPERTY,         // The value of a property has been changed.
    GC_CHANGE_CANVAS_ADD_VIEW,         // A new view was added.
    GC_CHANGE_CANVAS_ADD_LAYER,        // A new layer was added.
    GC_CHANGE_CANVAS_SWITCH_VIEW,      // Another view was activated.
    GC_CHANGE_CANVAS_REMOVE_VIEW,      // A view was removed.
    GC_CHANGE_CANVAS_REMOVE_LAYER,     // A layer was removed.
    GC_CHANGE_CANVAS_CLEAR_CONTENT,    // All figures have been removed.
    GC_CHANGE_CANVAS_CLEAR_LAYOUTS,    // All layout definitions have been removed.
    GC_CHANGE_CANVAS_CLEAR_STYLES,     // All styles have been removed.

    // Model related changes.
    GC_CHANGE_MODEL_PROPERTY,          // The value of a property has been changed.
    GC_CHANGE_MODEL_ADD_FIGURE,        // A new figure was added.
    GC_CHANGE_MODEL_REMOVE_FIGURE,     // A figure was removed.
    GC_CHANGE_MODEL_ADD_STYLE,         // A new style was added.
    GC_CHANGE_MODEL_REMOVE_STYLE,      // A style was removed.

    // Caption element related changes.
    GC_CHANGE_CAPTION_PROPERTY,        // The value of a property has been changed.

    // Figure element related changes.
    GC_CHANGE_ELEMENT_PROPERTY,        // The value of a property has been changed.
    GC_CHANGE_ELEMENT_ADD_CHILD,       // The value of a property has been changed.

    // Figure related changes.
    GC_CHANGE_FIGURE_PROPERTY,         // The value of a property has been changed.
    GC_CHANGE_FIGURE_EXCHANGE,         // A figure is about to be replaced by another one.
    GC_CHANGE_FIGURE_RESIZE,           // A figure was resized.

    // Figure instance related changes.
    GC_CHANGE_FINSTANCE_PROPERTY,      // The value of a property has been changed.

    // View related changes.
    GC_CHANGE_VIEW_PROPERTY,           // The value of a property has been changed.
    GC_CHANGE_VIEW_ZOOM,               // The zoom has been changed.
    GC_CHANGE_VIEW_OFFSET,             // The offset has been changed.
    GC_CHANGE_VIEW_ADD_LAYER,          // A new layer was added to a view.
    GC_CHANGE_VIEW_REMOVE_LAYER,       // A layer was removed.
    GC_CHANGE_VIEW_CLEAR,              // The view was cleared.
    GC_CHANGE_VIEW_OVERVIEW_START,     // The view started overview mode.
    GC_CHANGE_VIEW_OVERVIEW_STOP,      // The view stopped overview mode.
    GC_CHANGE_VIEW_RUBBERRECT_START,   // The view started rubber rect mode.
    GC_CHANGE_VIEW_RUBBERRECT_STOP,    // The view stopped rubber rect mode.
    GC_CHANGE_VIEW_RUBBERBAND_START,   // The view started rubber band mode.
    GC_CHANGE_VIEW_RUBBERBAND_STOP,    // The view stopped rubber band mode.
    GC_CHANGE_VIEW_DRAGGING_START,     // The view started dragging figures.
    GC_CHANGE_VIEW_DRAGGING_STOP,      // The view stopped dragging figures.

    // Layer related changes.
    GC_CHANGE_LAYER_CLEAR,             // All figure instances on the layer are removed.
    GC_CHANGE_LAYER_VISIBILITY,        // The visibility of a layer has been changed.
    GC_CHANGE_LAYER_PROPERTY,          // The value of a property has been changed.
    GC_CHANGE_LAYER_ADD_INSTANCE,      // A new figure instance was added.
    GC_CHANGE_LAYER_REMOVE_INSTANCE,   // A figure instance was removed.
    GC_CHANGE_LAYER_ADD_GROUP,         // A new group was added to a view.
    GC_CHANGE_LAYER_REMOVE_GROUP,      // A group was removed.

    // Connections
    GC_CHANGE_CONNECTION_DECORATION,   // A connection's decoration data has changed.
    GC_CHANGE_CONNECTION_LABEL,        // One of the labels of a connection's decorations has changed.
    GC_CHANGE_CONNECTION_INSTANCE      // A property in connection instance has changed.
  );

  TFeedbackInfo =
  (
    GC_FI_NONE,                        // No hit found at all.
    GC_FI_ON_OBJECT,                   // Anywhere on the object, not more details for the hit available.
    GC_FI_HLINE_MOVE,                  // A horizontal line can be moved (e.g. connection center, guide).
    GC_FI_VLINE_MOVE,                  // A vertical line can be moved (e.g. connection center, guide).
    GC_FI_CONNECTION,                  // A connection was hit.
    GC_FI_SELECTION_BODY,              // A selected figure instance was hit (but not the corners or edges).
    GC_FI_RESIZE_NORTH,                // The north edge of a selection was hit. Similarly for the other directions.
    GC_FI_RESIZE_NORTH_EAST,
    GC_FI_RESIZE_EAST,
    GC_FI_RESIZE_SOUTH_EAST,
    GC_FI_RESIZE_SOUTH,
    GC_FI_RESIZE_SOUTH_WEST,
    GC_FI_RESIZE_WEST,
    GC_FI_RESIZE_NORTH_WEST,
    GC_FI_TOGGLE,                      // A GC_ACTION_TOGGLE action can be exectuted.
    GC_FI_EXPAND,                      // A GC_ACTION_EXPAND action can be exectuted.
    GC_FI_COLLAPSE,                    // A GC_ACTION_COLLAPSE action can be exectuted.
    GC_FI_CHANGE_STYLE,                // A GC_ACTION_CHANGE_STYLE action can be exectuted.
    GC_FI_DRAG_FIGURE,                 // A GC_ACTION_DRAG_FIGURE action can be exectuted.
    GC_FI_DRAG_ALL,                    // A GC_ACTION_DRAG_ALL action can be exectuted.
    GC_FI_FLOATER,                     // The coordinates are within the bounds of a floater.
    GC_FI_DRAG_LABEL                   // One of the labels of a connection can be dragged.
  );

  TRubberRectStyle =
  (
    GC_RRSTYLE_SOLID_THIN,             // A simple black rectangle with a one pixel wide border.
    GC_RRSTYLE_SOLID_THICK,            // A simple black rectangle with a 3 pixel wide border.
    GC_RRSTYLE_DOTTED_THIN,            // A simple black rectangle with a one pixel wide dotted border.
    GC_RRSTYLE_DOTTED_THICK,           // A simple black rectangle with a 3 pixel wide dotted border.
    GC_RRSTYLE_BLENDED_CHECKERBOARD,   // A filled rectangle with a one pixel border and a translucent interior.
                                       // The system's selection color is used. The interior is a checker board.
    GC_RRSTYLE_BLENDED_DIAGONALS       // A filled rectangle with a one pixel border and a translucent interior.
                                       // The system's selection color is used. The interior consists of diagonal bands.
  );

  // TRubberBandStyle describes the look of the rubber band in the feedback layer.
  TRubberBandStyle =
  (
    GC_RBSTYLE_SOLID_THIN,             // A simple black rectangle with a one pixel wide border.
    GC_RBSTYLE_SOLID_THICK,            // A simple black rectangle with a 3 pixel wide border.
    GC_RBSTYLE_DOTTED_THIN,            // A simple black rectangle with a one pixel wide dotted border.
    GC_RBSTYLE_DOTTED_THICK            // A simple black rectangle with a 3 pixel wide dotted border.
  );

  // TRRSelectionAction (rubber rectangle selection action) determines how to manipulate the selection state of figure
  // instances with regard to their bounding box intersecting with the rubber rectangle.

  TRRSelectionAction =
  (
    GC_RRACTION_NONE,               // Don't touch the selection state of any figure instance.
                                    // Usually used for non-selecting rubber bands (e.g. for figure creation).
    GC_RRACTION_SELECT,             // Always select figure instances if their bounding box intersects. Keep selected
                                    // instances as their are if the do not intersect anymore.
                                    // Usually used for rubber bands with pressed shift key modifier.
    GC_RRACTION_SELECT_REMOVE,      // Select figure instances if they intersect, unselect those, which do not intersect.
                                    // Most common rubber band selection mode.
    GC_RRACTION_TOGGLE              // Revert the selection state of figure instances, which intersect. Don't touch the others.
                                    // Usually used for rubber bands with pressed control key modifier.
  );

  // Indicates which mouse event is to be handled.
  TGCMouseInputEvent =
  (
    GC_MOUSE_IGNORE,
    GC_MOUSE_DOWN,
    GC_MOUSE_UP,
    GC_MOUSE_MOVE,
    GC_MOUSE_DBL_CLICK
  );

  // Used in mouse events to specify which mouse button is involved. For one button only systems like MacOS
  // the left button inidicator is used for this (only) button.
  TGCMouseButton =
  (
    GC_MOUSE_BUTTON_IGNORE,
    GC_MOUSE_BUTTON_NONE,
    GC_MOUSE_BUTTON_LEFT,
    GC_MOUSE_BUTTON_MIDDLE,
    GC_MOUSE_BUTTON_RIGHT,
    GC_MOUSE_BUTTON_X1,
    GC_MOUSE_BUTTON_X2
  );

  // Determines the type of action possible with a particular figure instance/element.
  TActionType =
  (
    GC_ACTION_NONE,          // No action is assigned.
    GC_ACTION_TOGGLE,        // Switch between expanded and collapsed state (scope: element).
    GC_ACTION_EXPAND,        // Expand the element (scope: element).
    GC_ACTION_COLLAPSE,      // Collapse the element (scope: element).
    GC_ACTION_CHANGE_STYLE,  // Switch to a new style (scope: element).
    GC_ACTION_DRAG,          // A figure element wants to start dragging.
    GC_ACTION_DRAG_INSTANCE, // A view starts dragging a figure instance.
    GC_ACTION_CONNECTION,    // An action handled by the connection layer.
    GC_ACTION_RESIZE,        // The figure is resized.
    GC_ACTION_FLOATER_DRAG,  // A floater will be dragged.
    GC_ACTION_RUBBER_RECT,   // A rubber rectangle will be opened.
    GC_ACTION_RUBBER_BAND,   // Dito for rubber band.

    GC_ACTION_APPLICATION = 1000 // Application defined action codes start here. Do not define codes below this value.
  );

  PGcAction = ^TGcAction;
  TGcAction = record
    Type_: TActionType;
    Internal: record end;
  end;

  // Style of the line of a connection.
  TConnectionLineStyle =
  (
    GC_CONNECTION_STYLE_SOLID,
    GC_CONNECTION_STYLE_DOTTED
  );

  // Decorations used for a floater on the feedback layer.
  TFloaterDecoration =
  (
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
  );

  // Types of primitives, which can be created in a view.
  TPrimitiveType =
  (
    GC_PRIMITIVE_LINE,
    GC_PRIMITIVE_CIRCLE,
    GC_PRIMITIVE_RECTANGLE,
    GC_PRIMITIVE_POLYGON
  );

  // The format when rendering to a file.
  TGCFileFormat =
  (
    GC_FILE_FORMAT_PDF,
    GC_FILE_FORMAT_PS,
    GC_FILE_FORMAT_EPS,
    GC_FILE_FORMAT_PNG
  );

  // Determines how to render canvas scene data to memory.
  TGCColorFormat =
  (
    GC_COLOR_FORMAT_RGBA, // The usual red, green, blue, alpha color component order.
    GC_COLOR_FORMAT_BGRA  // This format is used on Windows.
  );

  // Used to query the canvas for OpenGL extension support.
  TOglExtension =
  (
    GC_OE_MULTISAMPLING,
    GC_OE_CONVOLUTION,
    GC_OE_HISTOGRAM,
    GC_OE_TEXTURE3D,
    GC_OE_FRAME_BUFFER_OBJECTS
  );

  TGCListener = class;
  TGCBase = class;
  TFigure = class;
  TFigureInstance = class;
  TLayer = class;
  TGenericCanvas = class;
  TGenericCanvasViewer = class;

  // Info block for connection decoration and labeling.
  TConnectionDecorations = record
    End1Decoration: TFigure;
    End1Label: TFigure;
    CenterDecoration: TFigure;
    CenterLabel: TFigure;
    End2Decoration: TFigure;
    End2Label: TFigure;
  end;

  TFactor = array[0..2] of Single;
  TAxis = array[0..2] of Single;

  TGCVariant = record
    // You should not use this variant except for passing it around!
  end;

  // Opaque class used to keep primitive references. You can only create them (see CreatePrimitive) and destroy them.
  TPrimitive = class
  public
    procedure _CPrimitive; virtual; cdecl; abstract; // The destructor of the class. Never call that.

    procedure Release; virtual; cdecl; abstract;
    procedure SetEndPoint(const NewEnd: TVertex); virtual; cdecl; abstract;
  end;

  // The general listener class is used to notify users of the canvas about general events like repaints and errors.
  TGCListener = class
  private
    FViewer: TGenericCanvasViewer;
  public
    constructor Create(Viewer: TGenericCanvasViewer);
    procedure _GCListener; virtual; cdecl; // A place holder for the C++ destructor. Never call that.

    procedure OnAction(Sender, Origin: TGCBase; var Action: PGcAction); virtual; cdecl;
    procedure OnChange(Sender, Origin: TGCBase; Reason: TGCChangeReason); virtual; cdecl;
    procedure OnDestroy(Sender: TGCBase); virtual; cdecl;
    procedure OnError(Sender, Origin: TGCBase; const Message: PChar); virtual; cdecl;
  end;

  TGCBase = class
  public
    procedure _CGCBase; virtual; cdecl; abstract; // The destructor of the class. Never call that.

    procedure Action(Origin: TGCBase; ActionType: TActionType); virtual; cdecl; abstract;
    procedure AddListener(Listener: TGCListener); virtual; cdecl; abstract;
    procedure BeginUpdate; virtual; cdecl; abstract;
    function Canvas: TGenericCanvas; virtual; cdecl; abstract;
    procedure Change(Origin: TGCBase; Reason: TGCChangeReason); virtual; cdecl; abstract;
    function ClassIs(ClassName: PChar): Boolean; virtual; cdecl; abstract;
    function ClassName: PChar; virtual; cdecl; abstract;
    function Destroying: Boolean; virtual; cdecl; abstract;
    procedure DisableEvents; virtual; cdecl; abstract;
    procedure EnableEvents; virtual; cdecl; abstract;
    procedure EndUpdate; virtual; cdecl; abstract;
    procedure Error(Origin: TGCBase; Message: PChar); virtual; cdecl; abstract;
    function PropertyGet(Name: PChar; Index: Cardinal): TGCVariant; virtual; cdecl; abstract;
    procedure PropertySet(Name: PChar; Index: Cardinal; const Value: TGCVariant); virtual; cdecl; abstract;
    procedure Release; virtual; cdecl; abstract;
    procedure RemoveListener(Listener: TGCListener); virtual; cdecl; abstract;
    function Updating: Boolean; virtual; cdecl; abstract;
    function UserDataGet: Pointer; virtual; cdecl; abstract;
    procedure UserDataSet(NewData: Pointer); virtual; cdecl; abstract;
  end;

  TGraphicElement = class(TGCBase)
  public
    function Bounds: TBoundingBox; virtual; cdecl; abstract;
    function ContainsPoint(const Position: TVertex): Boolean; virtual; cdecl; abstract;
    function Dirty: Boolean; virtual; cdecl; abstract;
    function GetFeedbackInfo(Modifiers: Integer; const Position: TVertex): TFeedbackInfo; virtual; cdecl; abstract;
    function Height: Single; virtual; cdecl; abstract;
    procedure MakeDirty(reason: TGCChangeReason = GC_CHANGE_NONE); virtual; cdecl; abstract;
    Function Overlaps(const Box: TBoundingBox): Boolean; virtual; cdecl; abstract;
    procedure Validate; virtual; cdecl; abstract;
    function Visible: Boolean; virtual; cdecl; abstract;
    function Width: Single; virtual; cdecl; abstract;
  end;

  TFigureElement = class(TGraphicElement)
  public
    function Figure: TFigure; virtual; cdecl; abstract;
  end;

  TFigure = class(TGraphicElement)
  public
    function ElementFromPoint(const coords: TVertex): TFigureElement; virtual; cdecl; abstract;
    procedure FigureToView(Coords: TVertex; Instance: TFigureInstance; var Result: TVertex); virtual; cdecl; abstract;
    procedure Render; virtual; cdecl; abstract;
    procedure Rotate(Angle: Single; Rx, Ry, Rz: Single); virtual; cdecl; abstract;
    procedure RotateV(Angle: Single; Axis: TAxis); virtual; cdecl; abstract;
    procedure Scale(sX, sY, sZ: Single; Accumulative: Boolean); virtual; cdecl; abstract;
    procedure ScaleV(const Factor: TFactor; Accumulative: Boolean);  virtual; cdecl; abstract;
    function SetSize(Width, Height: Single): Boolean; virtual; cdecl; abstract;
    procedure Translate(Tx, Ty, Tz: Single; Accumulative: Boolean); virtual; cdecl; abstract;
    procedure TranslateV(const Factor: TFactor; Accumulative: Boolean); virtual; cdecl; abstract;
    procedure ViewToFigure(Coords: TVertex; Instance: TFigureInstance; var Result: TVertex); virtual; cdecl; abstract;
  end;

  TFigureInstance = class(TGraphicElement)
  public
    function ElementFromPoint(const coords: TVertex): TFigureElement; virtual; cdecl; abstract;
    function ExecuteAssociatedActions(Button: TMouseButton; Event: TMouseEvent; Modifiers: Integer;
      const Coords: TVertex): PGcAction;  virtual; cdecl; abstract;
    function Figure: TFigure; virtual; cdecl; abstract;
    procedure InstanceToView(Original: TVertex; var Result: TVertex); virtual; cdecl; abstract;
    function Layer: TLayer; virtual; cdecl; abstract;
    function LocalBounds: TBoundingBox; virtual; cdecl; abstract;
    procedure Render; virtual; cdecl; abstract;
    procedure Rotate(Angle, Rx, Ry, Rz: Single); virtual; cdecl; abstract;
    procedure RotateV(Angle: Single; const Axis:TFactor); virtual; cdecl; abstract;
    procedure Scale(Sx, Sy, Sz: Single; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure ScaleV(const Factor: TFactor; Accumulative: Boolean = False); virtual; cdecl; abstract;
    function Selected: Boolean; virtual; cdecl; abstract;
    procedure Translate(Tx, Ty, Tz: Single;  Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure TranslateV(const Factor: TFactor; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure ViewToInstance(Original: TVertex; var Result: TVertex); virtual; cdecl; abstract;
  end;

  TConnection = class(TGraphicElement)
    function DecorationsGet: TConnectionDecorations; virtual; cdecl; abstract;
    procedure DecorationsSet(const Data: TConnectionDecorations); virtual; cdecl; abstract;
  end;

  TConnectionInstance = class(TGraphicElement)
  public
    function Center: Single; virtual; cdecl; abstract;
    function EndPoint1: TFigureInstance; virtual; cdecl; abstract;
    function EndPoint2: TFigureInstance; virtual; cdecl; abstract;
    function LineStyleGet: TConnectionLineStyle; virtual; cdecl; abstract;
    procedure LineStyleSet(NewStyle: TConnectionLineStyle); virtual; cdecl; abstract;
  end;

  TFloater = class(TGraphicElement)
  end;
  
  TLayer = class(TGCBase)
  protected
    // Internal virtual methods. Never call them.
    procedure RenderLayerContent; virtual; cdecl; abstract;
    procedure ValidateLayerContent; virtual; cdecl; abstract;
    procedure ZoomChanged(NewZoom: Single); virtual; cdecl; abstract;
  public
    procedure AddInstance(Instance: TFigureInstance); virtual; cdecl; abstract;
    procedure BringToFront(Instance: TFigureInstance; RelativeTo: TFigureInstance = nil); virtual; cdecl; abstract;
    procedure Clear; virtual; cdecl; abstract;
    function CreateInstance(Figure: TFigure): TFigureInstance; virtual; cdecl; abstract;
    function EnabledGet: Boolean; virtual; cdecl; abstract;
    procedure EnabledSet(IsEnabled: Boolean); virtual; cdecl; abstract;
    procedure LayerToView(Original: TVertex; var Result: TVertex); virtual; cdecl; abstract;
    procedure RemoveInstance(Instance: TFigureInstance); virtual; cdecl; abstract;
    procedure Render; virtual; cdecl; abstract;
    procedure Scale(Sx, Sy, Sz: Single; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure ScaleV(const Factor: TFactor; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure SendToBack(Instance: TFigureInstance; RelativeTo: TFigureInstance = nil); virtual; cdecl; abstract;
    procedure Translate(Tx, Ty, Tz: Single; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure TranslateV(const Factor: TFactor; Accumulative: Boolean = False); virtual; cdecl; abstract;
    procedure ViewToLayer(Orignal: TVertex; var Result: TVertex); virtual; cdecl; abstract;
    function VisibleGet: Boolean; virtual; cdecl; abstract;
    procedure VisibleSet(IsVisible: Boolean); virtual; cdecl; abstract;
  end;

  THitResults = class
  public
    procedure _CHitResults; virtual; cdecl; abstract; // The destructor of the class. Never call that.

    function Count: Integer; virtual; cdecl; abstract;
    function HasNext: Boolean; virtual; cdecl; abstract;
    function Next: TFigureInstance; virtual; cdecl; abstract;
    procedure Release; virtual; cdecl; abstract;
    procedure Reset; virtual; cdecl; abstract;
  end;

  // The TFigureInstanceEnumerator class is for quick access to all figure instances on all (common) layers in a view.
  // Enumeration happens depth-first. That means for each layer first all instances are enumerated before the next layer
  // is taken.
  // An instance can be gotten by a view's GetFigureInstanceEnumerator method. The caller must free the so
  // retrieved instance by calling Release.
  TFigureInstanceEnumerator = class
  public
    function HasNext: Boolean; virtual; cdecl; abstract;
    function Next: TFigureInstance; virtual; cdecl; abstract;
    procedure Release; virtual; cdecl; abstract;
    procedure Reset; virtual; cdecl; abstract;
  end;

  // The TSelectionEnumerator class is for quick access to all selected figure instances.
  // An instance can be retreived by a view's GetSelectionEnumerator method. The caller must free the so
  // retrieved instance by calling Release.
  TSelectionEnumerator = class
  public
    function HasNext: Boolean; virtual; cdecl; abstract;
    function Next: TFigureInstance; virtual; cdecl; abstract;
    procedure Release; virtual; cdecl; abstract;
    procedure Reset; virtual; cdecl; abstract;
  end;

  TGridLayer = class(TLayer)
  end;

  TView = class(TGCBase)
  public
    procedure AddLayer(Layer: TLayer); virtual; cdecl; abstract;
    procedure AddToSelection(Instance: TFigureInstance); virtual; cdecl; abstract;
    procedure ClearContent(RemoveLayers: Boolean); virtual; cdecl; abstract;
    procedure ClearPrimitives; virtual; cdecl; abstract;
    procedure ClearSelection; virtual; cdecl; abstract;
    procedure Color(Red, Green, Blue, Alpha: Single); virtual; cdecl; abstract;
    procedure ColorV(NewColor: array of Single); virtual; cdecl; abstract;
    function Contains(Layer: TLayer): Boolean; virtual; cdecl; abstract;
    function CreateCircle(const Center: TVertex; OuiterRadius, InnerRadius: Single; Color: array of single;
      Filled: Boolean): TPrimitive; virtual; cdecl; abstract;
    function CreateConnectionInstance(Connection: TConnection; EndPoint1, EndPoint2: TFigureInstance): TConnectionInstance; virtual; cdecl; abstract;
    function CreateFloater(const Bounds: TBoundingBox; Color1, Color2: array of Single; Decoration: TFloaterDecoration): TFloater;  virtual; cdecl; abstract;
    function CreateLine(const Start, End_: TVertex; Color: array of Single; Width: Single; Stipple: GLushort): TPrimitive; virtual; cdecl; abstract;
    function CreateRectangle(const Bounds: TBoundingBox; Color: array of Single; Filled: Boolean): TPrimitive; virtual; cdecl; abstract;
    function GetFeedbackInfo(Modifiers: Integer; X, Y: Integer): TFeedbackInfo; virtual; cdecl; abstract;
    function GetFigureInstanceEnumerator: TFigureInstanceEnumerator; virtual; cdecl; abstract;
    function GetHitTestInfo(Point: TVertex; SingleHit: Boolean): THitResults; virtual; cdecl; abstract;
    procedure GetLastRubberBounds(var Box: TBoundingBox); virtual; cdecl; abstract;
    procedure GetOrigin(var X, Y, Z: Single); virtual; cdecl; abstract;
    function GetSelectionEnumerator: TSelectionEnumerator; virtual; cdecl; abstract;
    procedure GetWorkspace(var Width, Height: Single); virtual; cdecl; abstract;
    function Grid: TGridLayer; virtual; cdecl; abstract;
    function HandleMouseInput(Event: TGCMouseInputEvent; Buttons: TGCMouseButton; Modifiers, X, Y: Integer): Boolean; virtual; cdecl; abstract;
    function JitterGet: Single; virtual; cdecl; abstract;
    procedure JitterSet(Value: Single); virtual; cdecl; abstract;
    function OffsetXGet: Single; virtual; cdecl; abstract;
    procedure OffsetXSet(Value: Single); virtual; cdecl; abstract;
    function OffsetYGet: Single; virtual; cdecl; abstract;
    procedure OffsetYSet(Value: Single); virtual; cdecl; abstract;
    function OverviewActive: Boolean; virtual; cdecl; abstract;
    procedure OverviewStart(Animated: Boolean); virtual; cdecl; abstract;
    procedure OverviewStop(ReturnToZoomed, Animated: Boolean); virtual; cdecl; abstract;
    procedure RemoveFromSelection(Instance: TFigureInstance); virtual; cdecl; abstract;
    procedure RemoveLayer(Layer: TLayer); virtual; cdecl; abstract;
    procedure RubberBandStart(Style: TRubberBandStyle; const Coords: TVertex); virtual; cdecl; abstract;
    procedure RubberBandStop; virtual; cdecl; abstract;
    procedure RubberRectStart(Style: TRubberRectStyle; const Coords: TVertex; RemoveSelection: Boolean); virtual; cdecl; abstract;
    procedure RubberRectStop; virtual; cdecl; abstract;
    function SelectionCount: Integer; virtual; cdecl; abstract;
    procedure SetOrigin(X, Y, Z: Single); virtual; cdecl; abstract;
    procedure ShowSelection(Visible: Boolean); virtual; cdecl; abstract;
    procedure ViewportSet(const NewViewport: TViewport); virtual; cdecl; abstract;
    procedure ViewToWindow(const Coords: TVertex; var X, Y: Integer); virtual; cdecl; abstract;
    procedure WindowToView(X, Y: Integer; var Coords: TVertex); virtual; cdecl; abstract;
    procedure WorkspaceGet(var Width, Height: Single); virtual; cdecl; abstract;
    procedure WorkspaceSet(const Width, Height: Single); virtual; cdecl; abstract;
    function ZoomGet: Single; virtual; cdecl; abstract;
    procedure ZoomSet(Value: Single); virtual; cdecl; abstract;
  end;

  TAnimation = class
  protected
    function Animate(Step: Integer): Boolean; virtual; cdecl; abstract; // Triggered internally.
  public
    procedure AddDependency(Animaton: TAnimation); virtual; cdecl; abstract;
    procedure AnimationStarted; virtual; cdecl; abstract;
    procedure AnimationStopped; virtual; cdecl; abstract;
    function Finished: Boolean; virtual; cdecl; abstract;
    function SuspendedGet: Boolean; virtual; cdecl; abstract;
    procedure SuspendedSet(Suspended: Boolean); virtual; cdecl; abstract;
  end;

  TAnimationManager = class(TGCBase)
  public
    function CreatePathAnimation(const Offset: TVertex; Duration: Integer; Suspended: Boolean;
      Element: TGraphicElement): TAnimation; virtual; cdecl; abstract;
    function CreateViewOffsetAnimation(X, Y: Single; Duration: Integer; Suspended: Boolean;
      Element: TView): TAnimation; virtual; cdecl; abstract;
    function CreateViewOffsetAnimation2(FromX, FromY, ToX, ToY: Single; Duration: Integer; Suspended: Boolean;
      Element: TView): TAnimation; virtual; cdecl; abstract;
    function CreateViewZoomAnimation(NewZoom: Single; Duration: Integer; Suspended: Boolean;
      Element: TView): TAnimation; virtual; cdecl; abstract;
    function CreateViewZoomAnimation2(FromZoom, ToZoom: Single; Duration: Integer; Suspended: Boolean;
      Element: TView): TAnimation; virtual; cdecl; abstract;
    procedure Pulse; virtual; cdecl; abstract;
    procedure TimerBase(Base: Integer); virtual; cdecl; abstract;
  end;

  TGenericCanvas = class(TGCBase)
  public
    procedure AddLayer(const Layer: TLayer); virtual; cdecl; abstract;
    function AddLayoutsFromFile(FileName: PChar): TGCError; virtual; cdecl; abstract;
    function AddStylesFromFile(FileName, Variables: PChar): TGCError; virtual; cdecl; abstract;
    function AddStyleFromDefinition(Definition: PChar): TGCError; virtual; cdecl; abstract;
    function AnimationManager: TAnimationManager; virtual; cdecl; abstract;
    procedure CheckError; virtual; cdecl; abstract;
    procedure ClearContent; virtual; cdecl; abstract;
    procedure ClearLayouts; virtual; cdecl; abstract;
    procedure ClearStyles; virtual; cdecl; abstract;
    function CreateConnection(TypeName, ClassName: PChar; EndPoint1, EndPoint2: TFigure): TConnection; virtual; cdecl; abstract;
    function CreateFigure(TypeName, ClassName: PChar): TFigure; virtual; cdecl; abstract;
    function CreateLayer(const Name: PChar; AddToCurrentView: Boolean): TLayer; virtual; cdecl; abstract;
    function CreateView(Name: PChar): TView; virtual; cdecl; abstract;
    function GetCurrentView: TView; virtual; cdecl; abstract;
    procedure CurrentViewSet(View: TView); virtual; cdecl; abstract;
    function FocusGet: Boolean; virtual; cdecl; abstract;
    procedure FocusSet(IsFocused: Boolean); virtual; cdecl; abstract;
    function LayerByName(Name: PChar): TLayer; virtual; cdecl; abstract;
    procedure Refresh; virtual; cdecl; abstract;
    procedure RemoveLayer(Layer: TLayer); virtual; cdecl; abstract;
    procedure RemoveStyle(Name: PChar); virtual; cdecl; abstract;
    procedure RemoveView(View: TView); virtual; cdecl; abstract;
    procedure Render(Content: Cardinal); virtual; cdecl; abstract;
    function RenderToFile(const Filename: PChar; Format: TGCFileFormat; const Title,
      Software: PChar; Content: Cardinal; Zoom: Single; const Bounds: TViewport): Boolean; virtual; cdecl; abstract;
    function RenderToMemory(Memory: Pointer; Format: TGCColorFormat; Content: Cardinal; Zoom: Single;
      var Bounds: TViewport): Boolean; virtual; cdecl; abstract;
    procedure SetTexturePath(Path: PChar); virtual; cdecl; abstract;
    function SupportsExtension(Extension: TOglExtension): Boolean; virtual; cdecl; abstract;
    function ViewByName(Name: PChar): TView; virtual; cdecl; abstract;
  end;

  TActionEvent = procedure(Sender: TObject; Origin: TGCBase; var ActionType: TActionType) of object;
  TErrorEvent = procedure(Sender: TObject; const Message: string) of object;
  TFocusChangedEvent = procedure(Sender: TObject; HasFocus: Boolean) of object;
  TChangeEvent = procedure(Sender: TObject; Source: TGCBase; Reason: TGCChangeReason) of object;

  TCanvasViewerOptions = set of
  (
    cvoAlwaysShowScrollbars,           // Always show the scrollbars, even if not needed for scrolling.
    cvoAutoCenterCanvas,               // Center viewer content (according to base size and zoom factor) automatically.
    cvoAutoCenterZoom                  // When zooming make the zoom center point also the window center.
  );

  // Flags that control the inner working of the viewer.
  TGCViewerStates = set of
  (
    vsDragging,              // Dragging of figures is currently active.
    vsExpose,                // Exposé mode is active.
    vsExposeRectMoving,      // Moving the exposé rectangle is in progress.
    vsExposeRectMovePending, // Moving the exposé rectangle using space+left mouse button is about to start.
    vsExposeSwitchPending,   // Shift+Space is pressed. Switches exposé mode on or off.
    vsGrabPanning,           // Panning using Ctlr+Alt+Space is in progress.
    vsGrabPanningPending,    // Panning using Ctlr+Alt+Space is about to begin (left mouse button must yet be down).
    vsRubberBand,            // Rubber band is currently active.
    vsRubberRect,            // Rubber rectangle is currently active.
    vsScrollPending,         // Auto scrolling is about to start.
    vsScrolling,             // Auto scrolling (also wheel panning) is in progress.
    vsScrollbarUpdating,     // Avoid recursive scrollbar adjustments from WM_SIZE.
    vsWheelPanning,          // Wheel mouse panning is active or soon will be.
    vsWheelScrolling,        // Wheel mouse scrolling is active or soon will be.
    vsZoomInPending,         // Ctrl+Space is pressed. Next mouse down zooms one step in.
    vsZoomOutPending         // Ctrl+Alt+Space is pressed. Next mouse down zooms one step out.
  );

  // Limits the speed interval which can be used for auto scrolling and animation (milliseconds).
  TAutoScrollInterval = 1..1000;
  TAnimationInterval = 10..1000;

  // Auto scroll directions.
  TScrollDirections = set of
  (
    sdLeft,
    sdUp,
    sdRight,
    sdDown
  );

  // Determines what to render in the window.
  TRenderingContent = set of
  (
    rcGrid,             // The background grid.
    rcConnections,      // The connection layer.
    rcFeedback,         // The feedback layer.
    rcFrame,            // The frame that indicates the current working area.
    rcFocus             // A focus indicator for the canvas itself.
  );
  
  TGCMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
    var Handled: Boolean) of object;
  TGCMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer; var Handled: Boolean) of object;
  TGCHandleMouseInputEvent = procedure(Sender: TObject; Event: TGCMouseInputEvent; Buttons: TGCMouseButton; Modifiers,
    X, Y: Integer; var Handled: Boolean) of object;

  TGenericCanvasViewer = class(TWinControl)
  private
    FCanvas: TGenericCanvas;
    FDeviceContext: HDC;
    FListener: TGCListener;
    FOffset: TPoint;
    FOptions: TCanvasViewerOptions;
    FRenderingContext: HGLRC;
    FViewport: TViewport;
    FCurrentZoom: Single;                        // The zoom factor of the current view.
    FBaseSize: TPoint;                           // The size of the canvas at 100% zoom.
    FLastClick: TPoint;                          // The position of the last mouse down action.
    FStates: TGCViewerStates;                    // Flags controlling how things are processed in the viewer.
    FInputStates: TGCInputStates;                // Tracker for certain keyboard/mouse states which are important for the viewer.
    FCurrentZoomStep: Integer;                   // The current position in the zoom factor array.
    FContent: TRenderingContent;                 // What is to be displayed?
    FNativeContent: Cardinal;                    // The same as FContent but with number used by the C++ canvas.

    // Auto scrolling.
    FAutoScrollInterval: TAutoScrollInterval;    // Determines speed of auto scrolling.
    FScrollDirections: TScrollDirections;        // Directions to scroll client area into depending on mouse position.
    FAutoScrollDelay: Cardinal;                  // Amount of milliseconds to wait until autos crolling becomes active.
    FHorizontalScrollStep: Integer;              // Amount of pixel to scroll horizontally in one timer step.
    FVerticalScrollStep: Integer;                // Amount of pixel to scroll vertically in one timer step.

    // Wheel panning/scrolling support.
    FPanningWindow: HWND;                        // Helper window for wheel panning
    FPanningCursor: TCursor;                     // Current wheel panning cursor.
    FPanningImage: TBitmap;                      // A little 32x32 bitmap to indicate the panning reference point.

    // Speed computation.
    FFPS: Single;
    FTimeBuffer: array[0..FPSBufferSize - 1] of Single;
    FCounterFrequency: Int64;
    FCurrentTimeBufferIndex: Integer;
    FTotalTime: Single;

    // Multisampling
    FMultiSamplingPixelFormat: Integer;
    FNeedMultisamplingCheck: Boolean;

    // Animation
    FAnimationInterval: TAnimationInterval;
    FZoomAnimation: TAnimation;                  // Used when the zoom of the current view are changed via animation.
    FOffsetAnimation: TAnimation;                // Used when the offset of the current view are changed via animation.

    FOnChange: TChangeEvent;
    FOnError: TErrorEvent;
    FOnFocusChanged: TFocusChangedEvent;
    FOnAction: TActionEvent;
    FOnSetup: TNotifyEvent;
    FOnZoomChange: TNotifyEvent;
    FOnMouseDown: TGCMouseEvent;
    FOnMouseMove: TGCMouseMoveEvent;
    FOnMouseUp: TGCMouseEvent;
    FOnMouseInput: TGCHandleMouseInputEvent;
    function GetCanvas: TGenericCanvas;
    function GetPosition: TPoint;
    function GetZoomStepCount: Integer;
    function GetZoomStep(const Index: Integer): Single;
    function GetOverview: Boolean;
    procedure SetAnimationInterval(const Value: TAnimationInterval);
    procedure SetBaseSizeX(const Value: Integer);
    procedure SetBaseSizeY(const Value: Integer);
    procedure SetCurrentZoom(const Value: Single);
    procedure SetCurrentZoomStep(const Step: Integer);
    procedure SetOffsetX(Value: Integer);
    procedure SetOffsetY(Value: Integer);
    procedure SetOptions(const Value: TCanvasViewerOptions);
    procedure SetOverview(OverviewOn: Boolean);
    procedure SetPosition(Value: TPoint);
    procedure SetRenderingContent(const Value: TRenderingContent);

    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
    procedure WMPendingUpdateScrollbars(var Message: TMessage); message WM_PENDING_UPDATE_SCROLLBARS;
    procedure WMPendingZoomChange(var Message: TMessage); message WM_PENDING_ZOOM_CHANGE;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMSysKeyDown(var Message: TWMSysKeyDown); message WM_SYSKEYDOWN;
    procedure WMSysKeyUp(var Message: TWMSysKeyUp); message WM_SYSKEYUP;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure AdjustPanningCursor(X, Y: Integer); virtual;
    procedure ApplyZoom(NewZoom: Single; ZoomCenter: TPoint); virtual;
    procedure ApplyZoomStep(X, Y: Integer); virtual;
    function CanAutoScroll: Boolean; virtual;
    function CanvasExceedsClientArea: Boolean; virtual;
    procedure CheckKeys(var Message: TWMKey; Shift: TShiftState); virtual;
    procedure CheckNavigationStates(const X, Y: Integer);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CursorFromStates(Info: TFeedbackInfo): TCursor; virtual;
    procedure DestroyContexts; virtual;
    procedure DestroyWnd; override;
    function DetermineScrollDirections(X, Y: Integer): TScrollDirections;
    procedure DoAction(AObject: TGCBase; var Action: PGcAction); virtual;
    procedure DoAutoScroll(X, Y: Integer); virtual;
    procedure DoChange(AObject: TGCBase; Reason: TGCChangeReason); virtual;
    procedure DoError(const Message: string); virtual;
    function DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    function DoMouseInput(Event: TGCMouseInputEvent; Buttons: TGCMouseButton; Modifiers, X, Y: Integer): Boolean;
    function DoMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;
    function DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
    procedure DoTimerScroll;
    procedure DoZoomChange; virtual;
    function GetModifiers: Integer; overload;
    function GetModifiers(var Message: TWMMouse): Integer; overload;
    procedure PanningWindowProc(var Message: TMessage); virtual;
    procedure StartWheelPanning; virtual;
    procedure StopTimer(ID: Integer); virtual;
    procedure StopWheelPanning; virtual;
    procedure UpdateScrollbars; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyZoomAndPosition(NewZoom: Single; Position: TPoint; Animated: Boolean); virtual;
    function CopyToClipboard(const Bounds: TBoundingBox): Boolean; overload;
    function CopyToClipboard(const R: TRect): Boolean; overload;
    procedure LoadLayouts(FileName: string);
    procedure LoadStyles(FileName: string);
    procedure RenderScene;
    procedure ZoomIn(X, Y: Integer);
    procedure ZoomOut(X, Y: Integer);

    property BaseSizeX: Integer read FBaseSize.X write SetBaseSizeX;
    property BaseSizeY: Integer read FBaseSize.Y write SetBaseSizeY;
    property Canvas: TGenericCanvas read GetCanvas;
    property DeviceContext: HDC read FDeviceContext;
    property Font;
    property FPS: Single read FFPS;
    property ZoomStepCount: Integer read GetZoomStepCount;
    property MouseCapture;
    property OffsetX: Integer read FOffset.X write SetOffsetX default 0;
    property OffsetY: Integer read FOffset.Y write SetOffsetY default 0;
    property Position: TPoint read GetPosition write SetPosition;
    property RenderingContext: HGLRC read FRenderingContext;
    property ZoomStep[const Index: Integer]: Single read GetZoomStep;
  published
    property Align;
    property Anchors;
    property AnimationInterval: TAnimationInterval read FAnimationInterval write SetAnimationInterval default 10;
    property AutoScrollDelay: Cardinal read FAutoScrollDelay write FAutoScrollDelay default 1000;
    property AutoScrollInterval: TAutoScrollInterval read FAutoScrollInterval write FAutoScrollInterval default 1;
    property Constraints;
    property CurrentZoom: Single read FCurrentZoom write SetCurrentZoom;
    property CurrentZoomStep: Integer read FCurrentZoomStep write SetCurrentZoomStep;
    property DragCursor;
    property DragMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property Options: TCanvasViewerOptions read FOptions write SetOptions default [cvoAutoCenterCanvas];
    property Overview: Boolean read GetOverview write SetOverview;
    property PopupMenu;
    property RenderingContent: TRenderingContent read FContent write SetRenderingContent
      default [rcGrid, rcConnections, rcFeedback, rcFrame];
    property Visible;

    property OnAction: TActionEvent read FOnAction write FOnAction;
    property OnChange: TChangeEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnFocusChanged: TFocusChangedEvent read FOnFocusChanged write FOnFocusChanged;
    property OnMouseDown: TGCMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseInput: TGCHandleMouseInputEvent read FOnMouseInput write FOnMouseInput;
    property OnMouseMove: TGCMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TGCMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnSetup: TNotifyEvent read FOnSetup write FOnSetup;
    property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R ..\Resources\GenericCanvasViewer.res}

uses
  Math, Clipbrd;

const
  GCLibrary = 'libmysqlgc.dll';

  ScrollMasks: array[Boolean] of Cardinal = (0, SIF_DISABLENOSCROLL);
  MaxZoomSteps = 20;
  ZoomSteps: array[0..MaxZoomSteps] of Single = (
    0.6125 / 12, 0.675 / 12, 0.75 / 12, 1 / 12, 2 / 12, 3 / 12, 4 / 12, 6 / 12, 8 / 12, 9 / 12, 10 / 12,
    1, 2, 3, 4, 5, 6, 7, 8, 12, 16
  );

  // Action states are states that do not prevent mouse message to be forwarded to the GC library.
  ActionStates = [vsDragging, vsExpose, vsExposeRectMoving, vsRubberBand, vsRubberRect, vsScrolling, vsScrollPending,
    vsWheelPanning, vsWheelScrolling];

  crZoomIn         = TCursor(101);
  crZoomOut        = TCursor(102);
  crHandOpen       = TCursor(103);
  crhandClosed     = TCursor(104);
  crMoveAll        = TCursor(105);
  crMoveEast       = TCursor(106);
  crMoveEastWest   = TCursor(107);
  crMoveNorth      = TCursor(108);
  crMoveNorthEast  = TCursor(109);
  crMoveNorthSouth = TCursor(110);
  crMoveNorthWest  = TCursor(111);
  crMoveSouth      = TCursor(112);
  crMoveSouthEast  = TCursor(113);
  crMoveSouthWest  = TCursor(114);
  crMoveWest       = TCursor(115);

function CreateGenericCanvas(Context: HDC; Name: PChar): TGenericCanvas; cdecl; external GCLibrary name '_CreateGenericCanvas';

//----------------- TGCListener ----------------------------------------------------------------------------------------

constructor TGCListener.Create(Viewer: TGenericCanvasViewer);

begin
  FViewer := Viewer;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGCListener._GCListener;

begin
  // Only a place holder for the actual C++ destructor.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGCListener.OnAction(Sender, Origin: TGCBase; var Action: PGcAction);

begin
  FViewer.DoAction(Origin, Action);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGCListener.OnChange(Sender, Origin: TGCBase; Reason: TGCChangeReason);

// Triggered by the canvas for regular changes.

begin
  FViewer.DoChange(Origin, Reason);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGCListener.OnDestroy(Sender: TGCBase);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGCListener.OnError(Sender, Origin: TGCBase; const Message: PChar);

begin
  FViewer.DoError(Message);
end;

//----------------- TGenericCanvasViewer -------------------------------------------------------------------------------

constructor TGenericCanvasViewer.Create(AOwner: TComponent);

begin
  inherited;

  // We need csMenuEvents here to make the VCL send us the WM_SYSCOMMAND message, which we need to disable Alt+Space.
  ControlStyle := ControlStyle + [csMenuEvents];
  Width := 150;
  Height := 150;
  FListener := TGCListener.Create(Self);
  FCurrentZoom := 1;
  FCurrentZoomStep := 11;
  FOptions := [cvoAutoCenterCanvas];
  FAutoScrollInterval := 10;
  FAutoScrollDelay := 1000;
  FHorizontalScrollStep := 10;
  FVerticalScrollStep := 10;
  FNeedMultisamplingCheck := True;

  FContent := [rcGrid, rcConnections, rcFeedback, rcFrame];
  FNativeContent := GC_RENDER_GRID + GC_RENDER_CONNECTIONS + GC_RENDER_FEEDBACK + GC_RENDER_FRAME;

  FAnimationInterval := 10;
    
  QueryPerformanceFrequency(FCounterFrequency);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGenericCanvasViewer.Destroy;

begin
  StopWheelPanning;
  
  if Assigned(FCanvas) then
  begin
    FCanvas.RemoveListener(FListener);
    FCanvas.Release;
  end;
  FListener.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetCanvas: TGenericCanvas;

begin
  HandleNeeded;

  // Trigger handle creation a second time if multi sampling was just set up.
  if (Handle = 0) and (FMultiSamplingPixelFormat <> 0) then
    HandleNeeded;
  Result := FCanvas;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetPosition: TPoint;

var
  ScrollInfo: TScrollInfo;
  Value: TPoint;

begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL or ScrollMasks[cvoAlwaysShowScrollbars in FOptions];

  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  Value.X := ScrollInfo.nPos;

  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  Value.Y := ScrollInfo.nPos;

  Result := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetZoomStepCount: Integer;

begin
  Result := GenericCanvasViewer.MaxZoomSteps + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetZoomStep(const Index: Integer): Single;

begin
  if (Index >= 0) and (Index <= GenericCanvasViewer.MaxZoomSteps) then
    Result := ZoomSteps[Index]
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetOverview: Boolean;

begin
  if FCanvas.GetCurrentView <> nil then
    Result := Canvas.GetCurrentView.OverviewActive
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetAnimationInterval(const Value: TAnimationInterval);

begin
  if FAnimationInterval <> Value then
  begin
    FAnimationInterval := Value;
    if HandleAllocated then
    begin
      KillTimer(Handle, AnimationTimer);
      FCanvas.AnimationManager.TimerBase(FAnimationInterval);
      SetTimer(Handle, AnimationTimer, FAnimationInterval, nil);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetBaseSizeX(const Value: Integer);

begin
  if FBaseSize.X <> Value then
  begin
    FBaseSize.X := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetBaseSizeY(const Value: Integer);

begin
  if FBaseSize.Y <> Value then
  begin
    FBaseSize.Y := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetCurrentZoom(const Value: Single);

var
  I: Integer;
  
begin
  if FCurrentZoom <> Value then
  begin
    I := MaxZoomSteps;
    while (I >= 0) and (ZoomSteps[I] > Value) do
      Dec(I);

    if I < 0 then
      I := 0;
    FCurrentZoomStep := I;
    ApplyZoom(ZoomSteps[I], Point(ClientWidth div 2, ClientHeight div 2));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetCurrentZoomStep(const Step: Integer);

begin
  if (Step >= 0) and (Step <= MaxZoomSteps) then
  begin
    FCurrentZoomStep := Step;
    ApplyZoom(ZoomSteps[Step], Point(ClientWidth div 2, ClientHeight div 2));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetOffsetX(Value: Integer);

begin
  if FOffset.X <> Value then
  begin
    FOffset.X := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetOffsetY(Value: Integer);

begin
  if FOffset.Y <> Value then
  begin
    FOffset.Y := Value;
    if HandleAllocated then
      UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetOptions(const Value: TCanvasViewerOptions);

begin
  FOptions := Value;
  UpdateScrollbars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetOverview(OverviewOn: Boolean);

begin
  if (vsExpose in FStates) <> OverviewOn then
  begin
    if OverviewOn then
    begin
      Include(FStates, vsExpose);
      if FCanvas.GetCurrentView <> nil then
        Canvas.GetCurrentView.OverviewStart(True);
    end
    else
    begin
      Exclude(FStates, vsExpose);
      if FCanvas.GetCurrentView <> nil then
        Canvas.GetCurrentView.OverviewStop(True, True);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetPosition(Value: TPoint);

begin
  FOffset.X := -Value.X;
  FOffset.Y := -Value.Y;

  UpdateScrollbars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.SetRenderingContent(const Value: TRenderingContent);

begin
  if Value <> FContent then
  begin
    FContent := Value;
    FNativeContent := 0;
    if rcGrid in FContent then
      FNativeContent := FNativeContent + GC_RENDER_GRID;
    if rcConnections in FContent then
      FNativeContent := FNativeContent + GC_RENDER_CONNECTIONS;
    if rcFeedback in FContent then
      FNativeContent := FNativeContent + GC_RENDER_FEEDBACK;
    if rcFrame in FContent then
      FNativeContent := FNativeContent + GC_RENDER_FRAME;
    if rcFocus in FContent then
      FNativeContent := FNativeContent + GC_RENDER_FOCUS;

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.CMMouseWheel(var Message: TCMMouseWheel);

var
  ScrollCount: Integer;
  ScrollLines: Integer;

begin
  StopWheelPanning;

  inherited;

  if Message.Result = 0  then
  begin
    with Message do
    begin
      Result := 1;
      if ssAlt in ShiftState then
      begin
        if WheelDelta > 0 then
          ZoomOut(ClientWidth div 2, ClientHeight div 2)
        else
          ZoomIn(ClientWidth div 2, ClientHeight div 2);
      end
      else
      begin
        if ssCtrl in ShiftState then
        begin
          if WheelDelta > 0 then
            ZoomOut(XPos, YPos)
          else
            ZoomIn(XPos, YPos);
        end
        else
        begin
          if FBaseSize.Y * FCurrentZoom >  ClientHeight then
          begin
            // Scroll vertically if there's something to scroll...
            SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @ScrollLines, 0);
            if Cardinal(ScrollLines) = WHEEL_PAGESCROLL then
              ScrollCount := WheelDelta div WHEEL_DELTA
            else
              ScrollCount := ScrollLines * WheelDelta div WHEEL_DELTA;
            OffsetY := FOffset.Y + ScrollCount * FVerticalScrollStep;
          end
          else
          begin
            // ...else scroll horizontally.
            ScrollCount := WheelDelta div WHEEL_DELTA;
            OffsetX := FOffset.X + ScrollCount * FHorizontalScrollStep;
          end;
        end;
      end;

      if (FStates - ActionStates) = [] then
        if not DoMouseMove(ShiftState, Message.XPos, Message.YPos) and (FCanvas.GetCurrentView <> nil) then
          with ScreenToClient(Point(Message.XPos, Message.YPos)) do
            FCanvas.GetCurrentView.HandleMouseInput(GC_MOUSE_MOVE, GC_MOUSE_BUTTON_NONE, GetModifiers, X, Y);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  Message.Result := DLGC_WANTCHARS or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMHScroll(var Message: TWMHScroll);

  //--------------- local functions -------------------------------------------

  function GetRealScrollPosition: Integer;

  var
    SI: TScrollInfo;
    Code: Integer;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    GetScrollInfo(Handle, Code, SI);
    Result := SI.nTrackPos;
  end;

  //--------------- end local functions ---------------------------------------

begin
  case Message.ScrollCode of
    SB_BOTTOM:
      OffsetX := Round(-(FCurrentZoom - 1) * ClientWidth);
    SB_ENDSCROLL:
      UpdateScrollBars;
    SB_LINELEFT:
      OffsetX := FOffset.X + FHorizontalScrollStep;
    SB_LINERIGHT:
      OffsetX := FOffset.X - FHorizontalScrollStep;
    SB_PAGELEFT:
      OffsetX := FOffset.X + ClientWidth;
    SB_PAGERIGHT:
      OffsetX := FOffset.X - ClientWidth;
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      OffsetX := -GetRealScrollPosition;
    SB_TOP:
      OffsetX := 0;
  end;

  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMKeyDown(var Message: TWMKeyDown);

var
  Shift: TShiftState;

begin
  inherited;

  Shift := KeyDataToShiftState(Message.KeyData);
  CheckKeys(Message, Shift);

  case Message.CharCode of
    VK_HOME:
      begin
        OffsetX := 0;
        OffsetY := 0;
      end;
    VK_END:
      begin
        OffsetX := -MaxInt;
        OffsetY := -MaxInt;
      end;
    VK_PRIOR:
      OffsetY := FOffset.Y + ClientHeight;
    VK_NEXT:
      OffsetY := FOffset.Y - ClientHeight;
    VK_UP:
      OffsetY := FOffset.Y + FVerticalScrollStep;
    VK_DOWN:
      OffsetY := FOffset.Y - FVerticalScrollStep;
    VK_LEFT:
      OffsetX := FOffset.X + FHorizontalScrollStep;
    VK_RIGHT:
      OffsetX := FOffset.X - FHorizontalScrollStep;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMKeyUp(var Message: TWMKeyUp);

var
  Shift: TShiftState;

begin
  inherited;

  Shift := KeyDataToShiftState(Message.KeyData);
  CheckKeys(Message, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMKillFocus(var Msg: TWMKillFocus);

begin
  StopWheelPanning;
  FCanvas.FocusSet(False);

  inherited;

  if (Assigned(FOnFocusChanged)) then
    FOnFocusChanged(self, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMLButtonDblClk(var Message: TWMLButtonDblClk);

begin
  inherited;

  MouseCapture := True;

  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    StopWheelPanning
  else
  begin
    if not Focused then
      SetFocus;

    with Message do
    begin
      FLastClick.X := XPos;
      FLastClick.Y := YPos;
      Include(FInputStates, ksLeftButtonDown);
      CheckNavigationStates(XPos, YPos);

      if vsZoomInPending in FStates then
        ZoomIn(XPos, YPos)
      else
        if vsZoomOutPending in FStates then
          ZoomOut(XPos, YPos)
        else
          if not (vsGrabPanning in FStates) then
          begin
            // No special handling so forward the input to the canvas if the app says it is ok.
            if not DoMouseDown(mbLeft, KeysToShiftState(Message.Keys) + [ssDouble], Message.XPos, Message.YPos) then
              DoMouseInput(GC_MOUSE_DBL_CLICK, GC_MOUSE_BUTTON_LEFT, GetModifiers(TWMMouse(Message)), Message.XPos,
                Message.YPos);
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMLButtonDown(var Message: TWMLButtonDown);

begin
  inherited;

  MouseCapture := True;

  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    StopWheelPanning
  else
  begin
    if not Focused then
      SetFocus;

    with Message do
    begin
      FLastClick.X := XPos;
      FLastClick.Y := YPos;
      Include(FInputStates, ksLeftButtonDown);
      CheckNavigationStates(XPos, YPos);

      if vsZoomInPending in FStates then
        ZoomIn(XPos, YPos)
      else
        if vsZoomOutPending in FStates then
          ZoomOut(XPos, YPos)
        else
          if not (vsGrabPanning in FStates) then
          begin
            // No special handling so forward the input to the canvas if the app says it is ok.
            if not DoMouseDown(mbLeft, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
              DoMouseInput(GC_MOUSE_DOWN, GC_MOUSE_BUTTON_LEFT, GetModifiers(TWMMouse(Message)), Message.XPos,
                Message.YPos);
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMLButtonUp(var Message: TWMLButtonUp);

begin
  inherited;

  if (FStates - ActionStates) = [] then
    if not DoMouseUp(mbLeft, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
      DoMouseInput(GC_MOUSE_UP, GC_MOUSE_BUTTON_LEFT, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);

  Exclude(FInputStates, ksLeftButtonDown);
  StopTimer(ScrollTimer);
  CheckNavigationStates(Message.XPos, Message.YPos);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMMButtonDblClk(var Message: TWMMButtonDblClk);

begin
  inherited;

  MouseCapture := True;

  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    StopWheelPanning
  else
  begin
    if not Focused then
      SetFocus;

    with Message do
    begin
      FLastClick.X := XPos;
      FLastClick.Y := YPos;
      Include(FInputStates, ksMiddleButtonDown);
      CheckNavigationStates(XPos, YPos);

      if not (vsGrabPanning in FStates) then
      begin
        // No special handling so forward the input to the canvas if the app says it is ok.
        if not DoMouseDown(mbMiddle, KeysToShiftState(Message.Keys) + [ssDouble], Message.XPos, Message.YPos) then
          DoMouseInput(GC_MOUSE_DBL_CLICK, GC_MOUSE_BUTTON_MIDDLE, GetModifiers(TWMMouse(Message)), Message.XPos,
            Message.YPos);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMMButtonDown(var Message: TWMMButtonDown);

begin
  inherited;

  if not Focused then
    SetFocus;

  Include(FInputStates, ksMiddleButtonDown);
  if FInputStates * MouseStates = [ksMiddleButtonDown] then
  begin
    // Start wheel panning or scrolling if not already active, allowed and scrolling is useful at all.
    if ([vsWheelScrolling, vsWheelPanning] * FStates = []) and CanvasExceedsClientArea then
    begin
      FLastClick := SmallPointToPoint(Message.Pos);
      StartWheelPanning;
    end
    else
      StopWheelPanning;
    CheckNavigationStates(Message.XPos, Message.YPos);
    MouseCapture := True;

    if (FStates - ActionStates) = [] then
      if not DoMouseDown(mbMiddle, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
        DoMouseInput(GC_MOUSE_DOWN, GC_MOUSE_BUTTON_MIDDLE, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMMButtonUp(var Message: TWMMButtonUp);

begin
  inherited;

  Exclude(FInputStates, ksMiddleButtonDown);

  if FInputStates * MouseStates = [] then
  begin
    MouseCapture := False;

    // If wheel panning/scrolling is active and the mouse has not yet been moved then the user starts wheel auto scrolling.
    // Indicate this by removing the panning flag. Otherwise (the mouse has moved meanwhile) stop panning.
    if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    begin
      if vsWheelScrolling in FStates then
      begin
        Exclude(FStates, vsWheelPanning);
        MouseCapture := False;
      end
      else
        StopWheelPanning;
    end;

    CheckNavigationStates(Message.XPos, Message.YPos);

    if (FStates - ActionStates) = [] then
      if not DoMouseDown(mbMiddle, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
        DoMouseInput(GC_MOUSE_UP, GC_MOUSE_BUTTON_MIDDLE, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMMouseMove(var Message: TWMMouseMove);

var
  dX, dY: Integer;

begin
  with Message do
  begin
    // If both wheel panning and auto scrolling are pending then the user moved the mouse while holding down the
    // middle mouse button. This means panning is being used, hence remove the wheel scroll flag.
    if [vsWheelPanning, vsWheelScrolling] * FStates = [vsWheelPanning, vsWheelScrolling] then
    begin
      if ((Abs(FLastClick.X - XPos) >= Mouse.DragThreshold) or (Abs(FLastClick.Y - YPos) >= Mouse.DragThreshold)) then
        Exclude(FStates, vsWheelScrolling);
    end;

    if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
      AdjustPanningCursor(XPos, YPos)
    else
    begin
      if vsGrabPanning in FStates then
      begin
        dX := XPos - FlastClick.X;
        FLastClick.X := XPos;
        dY := YPos -FlastClick.Y;
        FLastClick.Y := YPos;

        Inc(FOffset.X, dX);
        Inc(FOffset.Y, dY);
        UpdateScrollbars;
      end;
    end;
  end;

  if (FStates - ActionStates) = [] then
    if not DoMouseMove(KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
      DoMouseInput(GC_MOUSE_MOVE, GC_MOUSE_BUTTON_NONE, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);

  if CanAutoScroll then
    DoAutoScroll(Message.XPos, Message.YPos);
    
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMPaint(var Message: TWMPaint);

var
  PS: TPaintStruct;

begin
  BeginPaint(Handle, PS);
  RenderScene;
  EndPaint(Handle, PS);
  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMPendingUpdateScrollbars(var Message: TMessage);

// Used to decouple events sent from the canvas to the application (multi threading).

begin
  UpdateScrollbars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMPendingZoomChange(var Message: TMessage);

// Used to decouple events sent from the canvas to the application (multi threading).

begin
  DoZoomChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMRButtonDblClk(var Message: TWMRButtonDblClk);

begin
  inherited;

  MouseCapture := True;

  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    StopWheelPanning
  else
  begin
    if not Focused then
      SetFocus;

    with Message do
    begin
      FLastClick.X := XPos;
      FLastClick.Y := YPos;
      Include(FInputStates, ksLeftButtonDown);
      CheckNavigationStates(XPos, YPos);

      if not (vsGrabPanning in FStates) then
      begin
        // No special handling so forward the input to the canvas if the app says it is ok.
        if not DoMouseDown(mbRight, KeysToShiftState(Message.Keys) + [ssDouble], Message.XPos, Message.YPos) then
          DoMouseInput(GC_MOUSE_DBL_CLICK, GC_MOUSE_BUTTON_RIGHT, GetModifiers(TWMMouse(Message)), Message.XPos,
            Message.YPos);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMRButtonDown(var Message: TWMRButtonDown);

begin
  inherited;

  if not Focused then
    SetFocus;

  Include(FInputStates, ksRightButtonDown);
  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    StopWheelPanning
  else
    CheckNavigationStates(Message.XPos, Message.YPos);

  if (FStates - ActionStates) = [] then
    if not DoMouseDown(mbRight, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
      DoMouseInput(GC_MOUSE_DOWN, GC_MOUSE_BUTTON_RIGHT, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMRButtonUp(var Message: TWMRButtonUp);

begin
  Exclude(FInputStates, ksRightButtonDown);
  CheckNavigationStates(Message.XPos, Message.YPos);

  if (FStates - ActionStates) = [] then
    if not DoMouseUp(mbRight, KeysToShiftState(Message.Keys), Message.XPos, Message.YPos) then
      DoMouseInput(GC_MOUSE_UP, GC_MOUSE_BUTTON_RIGHT, GetModifiers(TWMMouse(Message)), Message.XPos, Message.YPos);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSetCursor(var Message: TWMSetCursor);

// Sets a new cursor for the viewer.

var
  NewCursor: TCursor;
  P: TPoint;
  Info: TFeedbackInfo;

begin
  with Message do
  begin
    if (CursorWnd <> Handle) or (csDesigning in ComponentState) or (Cursor <> crDefault) then
      inherited
    else
      if Assigned(FCanvas) and ([vsWheelPanning, vsWheelScrolling] * FStates = []) then
      begin
        if FCanvas.GetCurrentView <> nil then
        begin
          NewCursor := CursorFromStates(GC_FI_NONE);
          if NewCursor = crDefault then
          begin
            GetCursorPos(P);
            with ScreenToClient(P) do
            begin
              FCanvas.BeginUpdate;
              Info := FCanvas.GetCurrentView.GetFeedbackInfo(GetModifiers, X, Y);
              FCanvas.EndUpdate;
            end;

            case Info of
              GC_FI_RESIZE_SOUTH,
              GC_FI_RESIZE_NORTH:
                NewCursor := crSizeNS;
              GC_FI_RESIZE_NORTH_EAST,
              GC_FI_RESIZE_SOUTH_WEST:
                NewCursor := crSizeNESW;
              GC_FI_RESIZE_EAST,
              GC_FI_RESIZE_WEST:
                NewCursor := crSizeWE;
              GC_FI_RESIZE_SOUTH_EAST,
              GC_FI_RESIZE_NORTH_WEST:
                NewCursor := crSizeNWSE;
              GC_FI_DRAG_LABEL,
              GC_FI_DRAG_FIGURE,
              GC_FI_DRAG_ALL:
                NewCursor := crSize;
              GC_FI_HLINE_MOVE:
                NewCursor := crVSplit;
              GC_FI_VLINE_MOVE:
                NewCursor := crHSplit;
            else
              NewCursor := crDefault;
            end;
          end;

          if NewCursor = crDefault then
            SetCursor(Screen.Cursors[Cursor])
          else
            SetCursor(Screen.Cursors[NewCursor]);
          Message.Result := 1;
        end
        else
          inherited;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSetFocus(var Message: TWMSetFocus);

begin
  FCanvas.FocusSet(True);

  inherited;

  if (Assigned(FOnFocusChanged)) then
    FOnFocusChanged(self, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSize(var Message: TWMSize);

begin
  inherited;
  
  // Update viewport (here always the entire window).
  with FViewport do
  begin
    Width := Message.Width;
    Height := Message.Height;
    if Height = 0 then
      Height := 1;
  end;

  if Assigned(FCanvas) then
  begin
    FHorizontalScrollStep := ClientWidth div 20;
    FVerticalScrollStep := ClientHeight div 20;

    if FCanvas.GetCurrentView <> nil then
    begin
      FCanvas.BeginUpdate;
      FCanvas.GetCurrentView.ViewportSet(FViewport);
      FCanvas.EndUpdate;
    end;

    if not (vsScrollbarUpdating in FStates) then
      UpdateScrollbars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSysCommand(var Message: TWMSysCommand);

begin
  inherited;
  { TODO: if there never was trouble with alt key removed this code:
  if Message.CmdType = SC_KEYMENU then
    Message.Result := 1
  else
    inherited;
  }
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSysKeyDown(var Message: TWMSysKeyDown);

var
  Shift: TShiftState;

begin
  Shift := KeyDataToShiftState(Message.KeyData);
  CheckKeys(Message, Shift);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMSysKeyUp(var Message: TWMSysKeyUp);

var
  Shift: TShiftState;

begin
  Shift := KeyDataToShiftState(Message.KeyData);
  CheckKeys(Message, Shift);
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMTimer(var Message: TWMTimer);

// Centralized timer handling happens here.

begin
  with Message do
  begin
    case TimerID of
      ScrollTimer:
        begin
          if vsScrollPending in FStates then
          begin
            Application.CancelHint;
            // Scroll delay has elapsed, set to normal scroll interval now.
            SetTimer(Handle, ScrollTimer, FAutoScrollInterval, nil);
            FStates := FStates + [vsScrolling] - [vsScrollPending];
          end;
          DoTimerScroll;
        end;
      AnimationTimer:
        FCanvas.AnimationManager.Pulse;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.WMVScroll(var Message: TWMVScroll);

  //---------------------------------------------------------------------------

  function GetRealScrollPosition: Integer;

  var
    SI: TScrollInfo;

  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    GetScrollInfo(Handle, SB_VERT, SI);
    Result := SI.nTrackPos;
  end;

  //---------------------------------------------------------------------------

begin
  case Message.ScrollCode of
    SB_BOTTOM:
      OffsetY := Round(-(FCurrentZoom - 1) * ClientHeight);
    SB_ENDSCROLL:
      UpdateScrollBars;
    SB_LINEUP:
      OffsetY := FOffset.Y + FVerticalScrollStep;
    SB_LINEDOWN:
      OffsetY := FOffset.Y - FVerticalScrollStep;
    SB_PAGEUP:
      OffsetY := FOffset.Y + ClientHeight;
    SB_PAGEDOWN:
      OffsetY := FOffset.Y - ClientHeight;
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      OffsetY := -GetRealScrollPosition;
    SB_TOP:
      OffsetY := 0;
  end;
  Message.Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.AdjustPanningCursor(X, Y: Integer);

// Triggered by a mouse move when wheel panning/scrolling is active.
// Loads the proper cursor which indicates into which direction scrolling is done.

var
  NewCursor: TCursor;
  ScrollHorizontal,
  ScrollVertical: Boolean;

begin
  ScrollHorizontal := Round(FCurrentZoom * FBaseSize.X) > ClientWidth;
  ScrollVertical := Round(FCurrentZoom * FBaseSize.Y) > ClientHeight;

  if (Abs(X - FLastClick.X) < 8) and (Abs(Y - FLastClick.Y) < 8) then
  begin
    // Mouse is in the neutral zone.
    if ScrollHorizontal then
    begin
      if ScrollVertical then
        NewCursor := crMoveAll
      else
        NewCursor := crMoveEastWest;
    end
    else
      NewCursor := crMoveNorthSouth;
  end
  else
  begin
    // One of 8 directions applies: north, north-east, east, south-east, south, south-west, west and north-west.
    // Check also if scrolling in the particular direction is possible.
    if ScrollVertical and ScrollHorizontal then
    begin
      // All directions allowed.
      if X - FLastClick.X < -8 then
      begin
        // Left hand side.
        if Y - FLastClick.Y < -8 then
          NewCursor := crMoveNorthWest
        else
          if Y - FLastClick.Y > 8 then
            NewCursor := crMoveSouthWest
          else
            NewCursor := crMoveWest;
      end
      else
        if X - FLastClick.X > 8 then
        begin
          // Right hand side.
          if Y - FLastClick.Y < -8 then
            NewCursor := crMoveNorthEast
          else
            if Y - FLastClick.Y > 8 then
              NewCursor := crMoveSouthEast
            else
              NewCursor := crMoveEast;
        end
        else
        begin
          // Up or down.
          if Y < FLastClick.Y then
            NewCursor := crMoveNorth
          else
            NewCursor := crMoveSouth;
        end;
    end
    else
      if ScrollHorizontal then
      begin
        // Only horizontal movement allowed.
        if X < FLastClick.X then
          NewCursor := crMoveWest
        else
          NewCursor := crMoveEast;
      end
      else
      begin
        // Only vertical movement allowed.
        if Y < FLastClick.Y then
          NewCursor := crMoveNorth
        else
          NewCursor := crMoveSouth;
      end;
  end;

  if FPanningCursor <> NewCursor then
  begin
    FPanningCursor := NewCursor;
    Screen.Cursor := FPanningCursor;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.ApplyZoom(NewZoom: Single; ZoomCenter: TPoint);

// Zooms the canvas around the given zoom center (zoom origin) by the given factors.
// The origin must be given in window coordinates.

var
  CanvasX,
  CanvasY: Integer;

begin
  // Convert the origin's window to canvas coordinates.
  CanvasX := ZoomCenter.X - FOffset.X;
  CanvasY := ZoomCenter.Y - FOffset.Y;

  // Compute the new coordinates as they are after applying the new zoom factors.
  CanvasX := Round(CanvasX * NewZoom / FCurrentZoom);
  CanvasY := Round(CanvasY * NewZoom / FCurrentZoom);

  FCurrentZoom := NewZoom;

  if cvoAutoCenterZoom in FOptions then
  begin
    FOffset.X := (ClientWidth div 2) - CanvasX;
    FOffset.Y := (ClientHeight div 2) - CanvasY;
  end
  else
  begin
    FOffset.X := ZoomCenter.X - CanvasX;
    FOffset.Y := ZoomCenter.Y - CanvasY;
  end;

  Canvas.BeginUpdate;
  if Canvas.GetCurrentView <> nil then
    Canvas.GetCurrentView.ZoomSet(FCurrentZoom);
  Canvas.EndUpdate;
  UpdateScrollbars;

  DoZoomChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.ApplyZoomStep(X, Y: Integer);

var
  NewWidth: Integer;
  NewHeight: Integer;

begin
  if Align <> alClient then
  begin
    NewWidth := Min(Round(FBaseSize.X * ZoomSteps[FCurrentZoomStep]), ClientWidth);
    NewHeight := Min(Round(FBaseSize.Y * ZoomSteps[FCurrentZoomStep]), ClientHeight);
    SetBounds(Left, Top, NewWidth, NewHeight);
  end;
  ApplyZoom(ZoomSteps[FCurrentZoomStep], Point(X, Y));
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.CanAutoScroll: Boolean;

// Determines if auto scrolling is currently allowed.

var
  IsDropTarget: Boolean;
  IsRubberBanding: Boolean;
  IsWheelPanning: Boolean;

begin
  // Do auto scroll only if there is a draw selection in progress or the tree is the current drop target or
  // wheel panning/scrolling is active.
  IsDropTarget := vsDragging in FStates;
  IsRubberBanding := [vsRubberBand, vsRubberRect] * FStates <> [];
  IsWheelPanning := [vsWheelPanning, vsWheelScrolling] * FStates <> [];
  Result := IsRubberBanding or IsDropTarget or IsWheelPanning;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.CanvasExceedsClientArea: Boolean;

// Determines if the canvas (with the current zoom state) is larger than the client area (in any direction) and
// returns True if so.

begin
  Result := (Round(FCurrentZoom * FBaseSize.X) > ClientWidth) or (Round(FCurrentZoom * FBaseSize.Y) > ClientHeight);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.CheckKeys(var Message: TWMKey; Shift: TShiftState);

// Checks the given key message record and updates the internal navigation states.

var
  Down: Boolean;
  NeedStateCheck: Boolean;
  P: TPoint;

begin
  NeedStateCheck := False;
  Down := (Message.KeyData and $80000000) = 0;

  // Modifier keys are usually not passed in if they have been released.
  if (ssCtrl in Shift) <> (ksCtrl in FInputStates) then
  begin
    NeedStateCheck := True;
    if ssCtrl in Shift then
      Include(FInputStates, ksCtrl)
    else
      Exclude(FInputStates, ksCtrl);
  end;

  if (ssShift in Shift) <> (ksShift in FInputStates) then
  begin
    NeedStateCheck := True;
    if ssShift in Shift then
      Include(FInputStates, ksShift)
    else
      Exclude(FInputStates, ksShift);
  end;

  if (ssAlt in Shift) <> (ksAlt in FInputStates) then
  begin
    NeedStateCheck := True;
    if ssAlt in Shift then
      Include(FInputStates, ksAlt)
    else
      Exclude(FInputStates, ksAlt);
  end;

  // Normal keys are usually also passed in if they have been released.
  case Message.CharCode of
    VK_SPACE:
      begin
        NeedStateCheck := True;
        if Down then
          Include(FInputStates, ksSpace)
        else
          Exclude(FInputStates, ksSpace);
      end;
  end;

  if NeedStateCheck then
  begin
    GetCursorPos(P);
    with ScreenToClient(P) do
      CheckNavigationStates(X, Y);
    Screen.Cursor := CursorFromStates(GC_FI_NONE);

    // Check for exposé mode switch.
    if vsExposeSwitchPending in FStates then
    begin
      Exclude(FStates, vsExposeSwitchPending);
      if vsExpose in FStates then
      begin
        Exclude(FStates, vsExpose);
        if FCanvas.GetCurrentView <> nil then
          Canvas.GetCurrentView.OverviewStop(True, True);
      end
      else
      begin
        Include(FStates, vsExpose);
        if FCanvas.GetCurrentView <> nil then
          Canvas.GetCurrentView.OverviewStart(True);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.CheckNavigationStates(const X, Y: Integer);

// Updates the internal working states depending on keyboard and mouse states.

var
  Info: TFeedbackInfo;
  
begin
  if FStates * [vsRubberRect, vsRubberBand, vsDragging] = [] then
  begin
    FStates := FStates - [vsZoomInPending, vsZoomOutPending, vsGrabPanningPending, vsExposeSwitchPending,
      vsExposeRectMovePending];

    FCanvas.BeginUpdate;
    if FCanvas.GetCurrentView <> nil then
      Info := FCanvas.GetCurrentView.GetFeedbackInfo(GetModifiers, X, Y)
    else
      Info := GC_FI_NONE;
    FCanvas.EndUpdate;

    if ksSpace in FInputStates then
    begin
      // Possible states: grab panning (pending), zoom in/out, exposé.
      // If neither ctrl nor alt nor shift are pressed then we are in grab panning mode.
      if (FInputStates - [ksSpace, ksLeftButtonDown]) = [] then
      begin
        if vsExpose in FStates then
        begin
          if not (ksLeftButtonDown in FInputStates) then
          begin
            if vsExposeRectMoving in FStates then
            begin
              // The user just released the left mouse button while dragging the expose rectangle.
              // This means we stop exposé mode here and zoom to the target location (which is tracked
              // by the GC).
              FStates := FStates - [vsExposeRectMoving, vsExpose];
              if FCanvas.GetCurrentView <> nil then
                Canvas.GetCurrentView.OverviewStop(True, True);
            end
            else
              // If the mouse is over the exposé rect then indicate a potential move.
              if Info = GC_FI_FLOATER then
                FStates := FStates + [vsExposeRectMovePending];
          end
          else
            // Start exposé rect move only if the mouse is actually on the exposé rectangle.
            if Info = GC_FI_FLOATER then
              Include(FStates, vsExposeRectMoving);
        end
        else
        begin
          if not (ksLeftButtonDown in FInputStates) then
            FStates := FStates + [vsGrabPanningPending] - [vsGrabPanning]
          else
            Include(FStates, vsGrabPanning);
        end;
      end
      else
      begin
        if [ksShift, ksLeftButtonDown] * FInputStates = [ksShift] then
          Include(FStates, vsExposeSwitchPending)
        else
          if not (vsExpose in FStates) then
          begin
            if ksAlt in FInputStates then
              Include(FStates, vsZoomOutPending)
            else
              if ksCtrl in FInputStates then
                Include(FStates, vsZoomInPending);
          end;
      end
    end
    else
    begin
      FStates := FStates - [vsGrabPanning, vsExposeRectMoving];
      Screen.Cursor := crDefault;
    end;

    if Screen.Cursor = crDefault then
      Screen.Cursor := CursorFromStates(Info);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.CreateParams(var Params: TCreateParams);

begin
  inherited;

  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    WindowClass.Style := CS_VREDRAW or CS_HREDRAW or CS_OWNDC or CS_DBLCLKS;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.CreateWnd;

var
  Dummy: HPALETTE;

begin
  inherited;

  FDeviceContext := GetDC(Handle);
  FRenderingContext := CreateRenderingContext(FDeviceContext, [opDoubleBuffered], 32, 8, 0, 0, 0,
    FMultiSamplingPixelFormat, Dummy);
  if FRenderingContext = 0 then
    raise Exception.Create('Creation of OpenGL rendering context failed.'#10 + SysErrorMessage(GetLastError));

  // Activate the new rendering context. It will stay active for the entire lifetime of the window.
  // We have a private DC that ensures our settings do not get destroyed between paint calls.
  ActivateRenderingContext(FDeviceContext, FRenderingContext);

  // If we have not yet checked for multi sampling support then do it now. This might require a
  // recreation of the window handle.
  if FNeedMultisamplingCheck then
  begin
    FNeedMultisamplingCheck := False;
    FMultiSamplingPixelFormat := InitMultisampling(FDeviceContext, [opDoubleBuffered], 32, 8, 0, 0, 0, 4);
    if FMultiSamplingPixelFormat <> 0 then
    begin
      RecreateWnd;
      Exit;
    end;
  end;

  FCanvas := CreateGenericCanvas(FDeviceContext, '');
  FCanvas.BeginUpdate;
  FCanvas.AddListener(FListener);

  // Set the viewport to the entire window size.
  with FViewPort do
  begin
    Left := 0;
    Top := 0;
    Width := Self.Width;
    Height := Self.Height;
  end;

  if FBaseSize.X = 0 then
    FBaseSize.X := Width;
  if FBaseSize.Y = 0 then
  FBaseSize.Y := Height;

  ApplyZoomStep(ClientWidth div 2, ClientHeight div 2);
  UpdateScrollbars;
  if Assigned(FOnSetup) then
    FOnSetup(Self);

  FCanvas.EndUpdate;

  FCanvas.AnimationManager.TimerBase(FAnimationInterval);
  SetTimer(Handle, AnimationTimer, FAnimationInterval, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.CursorFromStates(Info: TFeedbackInfo): TCursor;

// Checks the current state and returns an appropriate cursor for it.
// Default is crDefault if none of the special states is currently active.

begin
  Result := crDefault;
  if vsZoomInPending in FStates then
    Result := crZoomIn
  else
    if vsZoomOutPending in FStates then
      Result := crZoomOut
    else
      if [vsGrabPanningPending, vsExposeRectMovePending] * FStates <> [] then
        Result := crHandOpen
      else
        if [vsGrabPanning, vsExposeRectMoving] * FStates <> [] then
          Result := crHandClosed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DestroyContexts;

begin
  DeactivateRenderingContext;
  DestroyRenderingContext(FRenderingContext);
  FRenderingContext := 0;
  if HandleAllocated then
    ReleaseDC(Handle, FDeviceContext);
  FDeviceContext := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DestroyWnd;

begin
  KillTimer(Handle, AnimationTimer);
  DestroyContexts;

  // Caution: Never call free or destroy. This is a C++ class we have here. We don't own it. Release takes care.
  if Assigned(FCanvas) then
  begin
    FCanvas.Release;
    FCanvas := nil;
  end;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.DetermineScrollDirections(X, Y: Integer): TScrollDirections;

// Determines which direction the client area must be scrolled depending on the given position.

begin
  Result:= [];

  if CanAutoScroll then
  begin
    // Calculation for wheel panning/scrolling is a bit different to normal auto scroll.
    if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
    begin
      if (X - FLastClick.X) < -8 then
        Include(Result, sdLeft);
      if (X - FLastClick.X) > 8 then
        Include(Result, sdRight);

      if (Y - FLastClick.Y) < -8 then
        Include(Result, sdUp);
      if (Y - FLastClick.Y) > 8 then
        Include(Result, sdDown);
    end
    else
    begin
      if (X < FHorizontalScrollStep) and (FOffset.X < 0) then
        Include(Result, sdLeft);
      if (ClientWidth - FOffset.X < Round(FCurrentZoom * FBaseSize.X)) and (X > ClientWidth - FHorizontalScrollStep) then
        Include(Result, sdRight);

      if (Y < FVerticalScrollStep) and (FOffset.Y < 0) then
        Include(Result, sdUp);
      if (ClientHeight - FOffset.Y < Round(FCurrentZoom * FBaseSize.Y)) and (Y > ClientHeight - FVerticalScrollStep) then
        Include(Result, sdDown);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoAction(AObject: TGCBase; var Action: PGcAction);

begin
  if Assigned(FOnAction) then
    FOnAction(Self, AObject, Action.Type_);

  // Check if the planned action conforms with our internal states.
  case Action.Type_ of
    GC_ACTION_FLOATER_DRAG:
    begin
      if not (vsExposeRectMoving in FStates) then
        Action.Type_ := GC_ACTION_NONE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoAutoScroll(X, Y: Integer);

begin
  FScrollDirections := DetermineScrollDirections(X, Y);

  if FStates * [vsWheelPanning, vsWheelScrolling] = [] then
  begin
    if FScrollDirections = [] then
    begin
      if ((FStates * [vsScrollPending, vsScrolling]) <> []) then
      begin
        StopTimer(ScrollTimer);
        FStates := FStates - [vsScrollPending, vsScrolling];
      end;
    end
    else
    begin
      // start auto scroll if not yet done
      if (FStates * [vsScrollPending, vsScrolling]) = [] then
      begin
        FStates := FStates + [vsScrollPending];
        SetTimer(Handle, ScrollTimer, FAutoScrollDelay, nil);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoChange(AObject: TGCBase; Reason: TGCChangeReason);

// Triggered by the canvas for regular changes.

var
  View: TView;

begin
  // Let the application first do what must be done.
  if Assigned(FOnChange) then
    FOnChange(Self, AObject, Reason);

  // Viewer internal handling of changes.
  case Reason of
    GC_CHANGE_CANVAS_REFRESH:
      Invalidate;
    GC_CHANGE_VIEW_OVERVIEW_START,
    GC_CHANGE_VIEW_OVERVIEW_STOP:
      begin
        View := TView(AObject);
        if View = Canvas.GetCurrentView then
        begin
          FOffset.X := Round(View.OffsetXGet);
          FOffset.Y := Round(View.OffsetYGet);
          FCurrentZoom := View.ZoomGet;
          PostMessage(Handle, WM_PENDING_ZOOM_CHANGE, 0, 0);
          PostMessage(Handle, WM_PENDING_UPDATE_SCROLLBARS, 0, 0);
        end;
      end;
    GC_CHANGE_CANVAS_ADD_VIEW:
      begin
        // A new view was created. Assign it the current valus for zoom and offset.
        View := TView(AObject);
        View.BeginUpdate;
        View.ViewportSet(FViewport);
        if View = FCanvas.GetCurrentView then
        begin
          View.OffsetXSet(FOffset.X);
          View.OffsetYSet(FOffset.Y);
          View.ZoomSet(FCurrentZoom);

          // A new view cannot be in exposé mode, so removed that state if the last one had it set.
          Exclude(FStates, vsExpose);
        end;
        View.EndUpdate;
      end;
    GC_CHANGE_CANVAS_SWITCH_VIEW: // A new view has been activated.
      begin
        if FCanvas.GetCurrentView <> nil then
        begin
          FOffset.X := Round(FCanvas.GetCurrentView.OffsetXGet);
          FOffset.Y := Round(FCanvas.GetCurrentView.OffsetYGet);
          FCurrentZoom := FCanvas.GetCurrentView.ZoomGet;

          // Update also the exposé mode, as this is persistent over all views.
          if FCanvas.GetCurrentView.OverviewActive then
            Include(FStates, vsExpose)
          else
            Exclude(FStates, vsExpose);
          PostMessage(Handle, WM_PENDING_ZOOM_CHANGE, 0, 0);
        end;
        PostMessage(Handle, WM_PENDING_UPDATE_SCROLLBARS, 0, 0);
      end;
    GC_CHANGE_VIEW_ZOOM: // The zoom has been changed externally.
      begin
        FCurrentZoom := FCanvas.GetCurrentView.ZoomGet;
        PostMessage(Handle, WM_PENDING_ZOOM_CHANGE, 0, 0);
        PostMessage(Handle, WM_PENDING_UPDATE_SCROLLBARS, 0, 0);
      end;
    GC_CHANGE_VIEW_OFFSET: // The offset has been changed externally.
      begin
        FOffset.X := Round(FCanvas.GetCurrentView.OffsetXGet);
        FOffset.Y := Round(FCanvas.GetCurrentView.OffsetYGet);
        PostMessage(Handle, WM_PENDING_UPDATE_SCROLLBARS, 0, 0);
      end;
    GC_CHANGE_VIEW_RUBBERRECT_START:
      Include(FStates, vsRubberRect);
    GC_CHANGE_VIEW_RUBBERRECT_STOP:
      Exclude(FStates, vsRubberRect);
    GC_CHANGE_VIEW_RUBBERBAND_START:
      Include(FStates, vsRubberBand);
    GC_CHANGE_VIEW_RUBBERBAND_STOP:
      Exclude(FStates, vsRubberBand);
    GC_CHANGE_VIEW_DRAGGING_START:
      Include(FStates, vsDragging);
    GC_CHANGE_VIEW_DRAGGING_STOP:
      Exclude(FStates, vsDragging);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoError(const Message: string);

begin
  if Assigned(FOnError) then
    FOnError(Self, Message);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;

begin
  Result := False;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.DoMouseInput(Event: TGCMouseInputEvent; Buttons: TGCMouseButton; Modifiers, X,
  Y: Integer): Boolean;

begin
  Result := False;
  if Assigned(FOnMouseInput) then
    FOnMouseInput(Self, Event, Buttons, Modifiers, X, Y, Result);
  if not Result and (FCanvas.GetCurrentView <> nil) then
    FCanvas.GetCurrentView.HandleMouseInput(Event, Buttons, Modifiers, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.DoMouseMove(Shift: TShiftState; X, Y: Integer): Boolean;

begin
  Result := False;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;

begin
  Result := False;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoTimerScroll;

var
  P,
  ClientP: TPoint;
  InRect,
  Panning: Boolean;
  R,
  ClipRect: TRect;
  DeltaX,
  DeltaY: Integer;
  IncrementX,
  IncrementY: Integer;

begin
  GetCursorPos(P);
  R := ClientRect;
  ClipRect := R;
  MapWindowPoints(Handle, 0, R, 2);
  InRect := PtInRect(R, P);
  ClientP := ScreenToClient(P);
  Panning := [vsWheelPanning, vsWheelScrolling] * FStates <> [];
  IncrementX := FHorizontalScrollStep;
  IncrementY := FVerticalScrollStep;

  if ActionStates * FStates <> [] then
  begin
    DeltaX := 0;
    DeltaY := 0;

    if sdUp in FScrollDirections then
    begin
      if Panning then
        DeltaY := FLastClick.Y - ClientP.Y - 8
      else
        if InRect then
          DeltaY := Min(IncrementY, ClientHeight)
        else
          DeltaY := Min(IncrementY, ClientHeight) * Abs(R.Top - P.Y);
      if FOffset.Y = 0 then
        Exclude(FScrollDirections, sdUp);
    end;

    if sdDown in FScrollDirections then
    begin
      if Panning then
        DeltaY := FLastClick.Y - ClientP.Y + 8
      else
        if InRect then
          DeltaY := -Min(IncrementY, ClientHeight)
        else
          DeltaY := -Min(IncrementY, ClientHeight) * Abs(P.Y - R.Bottom);
      if (ClientHeight - FOffset.Y) = Round(FCurrentZoom * FBaseSize.Y) then
        Exclude(FScrollDirections, sdDown);
    end;

    if sdLeft in FScrollDirections then
    begin
      if Panning then
        DeltaX := FLastClick.X - ClientP.X - 8
      else
        if InRect then
          DeltaX := IncrementX
        else
          DeltaX := IncrementX * Abs(R.Left - P.X);
      if FOffset.X = 0 then
        Exclude(FScrollDirections, sdleft);
    end;

    if sdRight in FScrollDirections then
    begin
      if Panning then
        DeltaX := FLastClick.X - ClientP.X + 8
      else
        if InRect then
          DeltaX := -IncrementX
        else
          DeltaX := -IncrementX * Abs(P.X - R.Right);

      if (ClientWidth - FOffset.X) = Round(FCurrentZoom * FBaseSize.X) then
        Exclude(FScrollDirections, sdRight);
    end;

    if Panning then
    begin
      OffsetX := OffsetX + DeltaX;
      OffsetY := OffsetY + DeltaY;
    end
    else
    begin
      // Make sure we step at least one pixel. So the minimal step must be at the size of the multiplier.
      if Abs(DeltaX) < 30 then
        DeltaX := Sign(DeltaX) * 30;
      OffsetX := OffsetX + DeltaX div 30;
      if Abs(DeltaY) < 30 then
        DeltaY := Sign(DeltaY) * 30;
      OffsetY := OffsetY + DeltaY div 30;
    end;

    if FCanvas.GetCurrentView <> nil then
      FCanvas.GetCurrentView.HandleMouseInput(GC_MOUSE_MOVE, GC_MOUSE_BUTTON_NONE, GetModifiers, ClientP.X, ClientP.Y);

    if (FScrollDirections = []) and ([vsWheelPanning, vsWheelScrolling] * FStates = []) then
    begin
      StopTimer(ScrollTimer);
      FStates := FStates - [vsScrollPending, vsScrolling];
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.DoZoomChange;

begin
  if Assigned(FOnZoomChange) then
    FOnZoomChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetModifiers: Integer;

begin
  Result := GC_MODIFIER_IGNORE;
  if GetKeyState(VK_SHIFT) < 0 then
    Result := Result or GC_MODIFIER_SHIFT;
  if GetKeyState(VK_CONTROL) > 0 then
    Result := Result or GC_MODIFIER_CONTROL;
  if GetKeyState(VK_MENU) < 0 then
    Result := Result or GC_MODIFIER_ALT;

  if Result = GC_MODIFIER_IGNORE then
    Result := GC_MODIFIER_NONE;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.GetModifiers(var Message: TWMMouse): Integer;

begin
  Result := GC_MODIFIER_IGNORE;
  if Message.Keys and MK_SHIFT <> 0 then
    Result := Result or GC_MODIFIER_SHIFT;
  if Message.Keys and MK_CONTROL <> 0 then
    Result := Result or GC_MODIFIER_CONTROL;
  if GetKeyState(VK_MENU) < 0 then
    Result := Result or GC_MODIFIER_ALT;

  if Result = GC_MODIFIER_IGNORE then
    Result := GC_MODIFIER_NONE;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.PanningWindowProc(var Message: TMessage);

var
  PS: TPaintStruct;
  Canvas: TCanvas;

begin
  if Message.Msg = WM_PAINT then
  begin
    BeginPaint(FPanningWindow, PS);
    Canvas := TCanvas.Create;
    Canvas.Handle := PS.hdc;
    try
      Canvas.Draw(0, 0, FPanningImage);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
      EndPaint(FPanningWindow, PS);
    end;
    Message.Result := 0;
  end
  else
    with Message do
      Result := DefWindowProc(FPanningWindow, Msg, wParam, lParam);
end;

//----------------------------------------------------------------------------------------------------------------------

var
  PanningWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'GCPanningWindow'
  );

procedure TGenericCanvasViewer.StartWheelPanning;

// Called when wheel panning should start. A little helper window is created to indicate the reference position,
// which determines in which direction and how far wheel panning/scrolling will happen.

  //--------------- local function --------------------------------------------

  function CreateClipRegion: HRGN;

  // In order to avoid doing all the transparent drawing ourselves we use a
  // window region for the wheel window.
  // Since we only work on a very small image (32x32 pixels) this is acceptable.

  var
    Start, X, Y: Integer;
    Temp: HRGN;

  begin
    Assert(not FPanningImage.Empty, 'Invalid wheel panning image.');

    // Create an initial region on which we operate.
    Result := CreateRectRgn(0, 0, 0, 0);
    with FPanningImage, Canvas do
    begin
      for Y := 0 to Height - 1 do
      begin
        Start := -1;
        for X := 0 to Width - 1 do
        begin
          // Start a new span if we found a non-transparent pixel and no span is currently started.
          if (Start = -1) and (Pixels[X, Y] <> clFuchsia) then
            Start := X
          else
            if (Start > -1) and (Pixels[X, Y] = clFuchsia) then
            begin
              // A non-transparent span is finished. Add it to the result region.
              Temp := CreateRectRgn(Start, Y, X, Y + 1);
              CombineRgn(Result, Result, Temp, RGN_OR);
              DeleteObject(Temp);
              Start := -1;
            end;
        end;
        // If there is an open span then add this also to the result region.
        if Start > -1 then
        begin
          Temp := CreateRectRgn(Start, Y, Width, Y + 1);
          CombineRgn(Result, Result, Temp, RGN_OR);
          DeleteObject(Temp);
        end;
      end;
    end;

    // The resulting region is used as window region so we must not delete it.
    // Windows will own it after the assignment below.
  end;

  //--------------- end local function ----------------------------------------

var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
  ImageName: string;

begin
  // Set both panning and scrolling flag. One will be removed shortly depending on whether the middle mouse button is
  // released before the mouse is moved or vice versa. The first case is referred to as wheel scrolling while the
  // latter is called wheel panning.
  StopTimer(ScrollTimer);
  FStates := FStates + [vsWheelPanning, vsWheelScrolling];

  // Register the helper window class.
  PanningWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, PanningWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(PanningWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(PanningWindowClass);
  end;
  // Create the helper window and show it at the given position without activating it.
  with ClientToScreen(FLastClick) do
    FPanningWindow := CreateWindowEx(WS_EX_TOOLWINDOW, PanningWindowClass.lpszClassName, nil, WS_POPUP, X - 16, Y - 16,
      32, 32, 0, 0, HInstance, nil);

  FPanningImage := TBitmap.Create;
  if Round(FCurrentZoom * FBaseSize.X) > ClientWidth then
  begin
    if Round(FCurrentZoom * FBaseSize.Y) > ClientHeight then
      ImageName := 'GC_MoveAllBitmap'
    else
      ImageName := 'GC_MoveEastWestBitmap'
  end
  else
    ImageName := 'GC_MoveNorthSouthBitmap';
  FPanningImage.LoadFromResourceName(HInstance, ImageName);
  SetWindowRgn(FPanningWindow, CreateClipRegion, False);

  SetWindowLong(FPanningWindow, GWL_WNDPROC, Integer(Classes.MakeObjectInstance(PanningWindowProc)));
  ShowWindow(FPanningWindow, SW_SHOWNOACTIVATE);

  // Setup the panscroll timer and capture all mouse input.
  SetFocus;
  SetCapture(Handle);
  SetTimer(Handle, ScrollTimer, FAnimationInterval, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.StopTimer(ID: Integer);

begin
  if HandleAllocated then
    KillTimer(Handle, ID);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.StopWheelPanning;

// Stops panning if currently active and destroys the helper window.

var
  Instance: Pointer;

begin
  if [vsWheelPanning, vsWheelScrolling] * FStates <> [] then
  begin
    // Release the mouse capture and stop the panscroll timer.
    StopTimer(ScrollTimer);
    ReleaseCapture;
    FStates := FStates - [vsWheelPanning, vsWheelScrolling];

    // Destroy the helper window.
    Instance := Pointer(GetWindowLong(FPanningWindow, GWL_WNDPROC));
    DestroyWindow(FPanningWindow);
    if Instance <> @DefWindowProc then
      Classes.FreeObjectInstance(Instance);
    FPanningWindow := 0;
    FPanningImage.Free;
    FPanningImage := nil;
    DeleteObject(FPanningCursor);
    FPanningCursor := 0;
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.UpdateScrollbars;

var
  ScrollInfo: TScrollInfo;
  UpdateCanvas: Boolean;
  ActualWidth,
  ActualHeight: Integer;

begin
  Include(FStates, vsScrollbarUpdating);

  // Check if the virtual size is larger than the client area. In this case the offsets must correspond to the available
  // scroll range.
  ActualWidth := Round(FCurrentZoom * FBaseSize.X);
  if ActualWidth > ClientWidth then
  begin
    if FOffset.X > 0 then
      FOffset.X := 0;
    FOffset.X := Max(FOffset.X, Max(0, ClientWidth) - ActualWidth);
  end;
  ActualHeight := Round(FCurrentZoom * FBaseSize.Y);
  if Actualheight > ClientHeight then
  begin
    if FOffset.Y > 0 then
      FOffset.Y := 0;
    FOffset.Y := Max(FOffset.Y, Max(0, ClientHeight) - ActualHeight);
  end;

  UpdateCanvas := False;

  // Horizontal scrollbar.
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL or ScrollMasks[cvoAlwaysShowScrollbars in FOptions];

  ScrollInfo.nMax := ActualWidth;
  if (ActualWidth > ClientWidth) or (cvoAlwaysShowScrollbars in FOptions) then
  begin
    UpdateCanvas := True;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPos := -FOffset.X;
    ScrollInfo.nPage := Max(0, ClientWidth);
    SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    ShowScrollBar(Handle, SB_HORZ, True);
  end
  else
  begin
    ShowScrollBar(Handle, SB_HORZ, False);

    // No scrollbars, so we center the main area in the viewer.
    if cvoAutoCenterCanvas in FOptions then
    begin
      UpdateCanvas := True;
      // Use Width instead ClientWidth here. We just have hidden the scrollbar but that
      // change did not make it yet to ClientWidth here. Similar for ClientHeight below.
      FOffset.X := Round((Width - ActualWidth) / 2);
    end;
  end;

  // Vertical scrollbar.
  ScrollInfo.nMax := ActualHeight;
  if (ActualHeight > ClientHeight) or (cvoAlwaysShowScrollbars in FOptions) then
  begin
    UpdateCanvas := True;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPage := Max(0, ClientHeight);
    ScrollInfo.nPos := -FOffset.Y;
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    ShowScrollBar(Handle, SB_VERT, True);
  end
  else
  begin
    ShowScrollBar(Handle, SB_VERT, False);

    // No scrollbars, so we center the main area in the viewer.
    if cvoAutoCenterCanvas in FOptions then
    begin
      UpdateCanvas := True;
      FOffset.Y := Round((Height - ActualHeight) / 2);
    end;
  end;

  if UpdateCanvas and (Canvas.GetCurrentView <> nil) then
  begin
    FCanvas.BeginUpdate;
    Canvas.GetCurrentView.OffsetXSet(FOffset.X);
    Canvas.GetCurrentView.OffsetYSet(FOffset.Y);
    FCanvas.EndUpdate;
  end;

  Exclude(FStates, vsScrollbarUpdating);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.ApplyZoomAndPosition(NewZoom: Single; Position: TPoint; Animated: Boolean);

// Zooms the canvas and moves it to the given offset.
// The offset must be given in window coordinates.

begin
  if Animated and (FCanvas.GetCurrentView <> nil) then
  begin
    FOffsetAnimation := FCanvas.AnimationManager.CreateViewOffsetAnimation(-Position.X, -Position.Y, 300, False,
      FCanvas.GetCurrentView);
    FZoomAnimation := FCanvas.AnimationManager.CreateViewZoomAnimation(NewZoom, 200, False, FCanvas.GetCurrentView);
  end
  else
  begin
    FCurrentZoom := NewZoom;

    FOffset.X := -Position.X;
    FOffset.Y := -Position.Y;

    Canvas.BeginUpdate;
    if Canvas.GetCurrentView <> nil then
      Canvas.GetCurrentView.ZoomSet(FCurrentZoom);
    Canvas.EndUpdate;
    UpdateScrollbars;

    DoZoomChange;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.CopyToClipboard(const Bounds: TBoundingBox): Boolean;

// Copies the given viewer area (view coordinates) to the clipboard as a bitmap.

var
  Bitmap: TBitmap;
  Memory: PChar;
  Format: Word;
  Data: Cardinal;
  Palette: HPalette;
  ViewBounds: TViewport;

begin
  ViewBounds.Left := Round(Bounds.Upper.X);
  ViewBounds.Top := Round(Bounds.Upper.Y);
  ViewBounds.Width := Round(Bounds.Lower.X - Bounds.Upper.X);
  if ViewBounds.Width < 0 then
  begin
    ViewBounds.Width := -ViewBounds.Width;
    ViewBounds.Left := Round(Bounds.Lower.X);
  end;
  ViewBounds.Height := Round(Bounds.Lower.Y - Bounds.Upper.Y);
  if ViewBounds.Height < 0 then
  begin
    ViewBounds.Height := -ViewBounds.Height;
    ViewBounds.Top := Round(Bounds.Lower.Y);
  end;

  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32Bit;
    Bitmap.Width := ViewBounds.Width;
    Bitmap.Height := ViewBounds.Height;

    Memory := Bitmap.ScanLine[0];
    if Cardinal(Bitmap.ScanLine[1]) < Cardinal(Memory) then
      Memory := Bitmap.ScanLine[Bitmap.Height - 1];
    Canvas.RenderToMemory(Memory, GC_COLOR_FORMAT_BGRA, GC_RENDER_GRID or GC_RENDER_CONNECTIONS or GC_RENDER_FEEDBACK,
      FCurrentZoom, ViewBounds);

    // Finally set the content of the bitmap to the clipboard.
    Bitmap.SaveToClipboardFormat(Format, Data, Palette);
    Clipboard.SetAsHandle(Format, Data);
    Result := True;
  finally
    Bitmap.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGenericCanvasViewer.CopyToClipboard(const R: TRect): Boolean;

// Copies the given viewer area (window coordinates) to the clipboard as a bitmap.

var
  Bounds: TBoundingBox;

begin
  if FCanvas.GetCurrentView <> nil then
  begin
    Canvas.GetCurrentView.WindowToView(R.Left, R.Top, Bounds.Upper);
    Canvas.GetCurrentView.WindowToView(R.Right, R.Bottom, Bounds.Lower);

    Result := CopyToClipboard(Bounds);
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.LoadLayouts(FileName: string);

var
  Error: TGCError;

begin
  Error := Canvas.AddlayoutsFromFile(PChar(FileName));
  if Error <> GC_NO_ERROR then
    raise Exception.Create(Format('XML parse error of file %s', [FileName]));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.LoadStyles(FileName: string);

var
  Error: TGCError;

begin
  Error := Canvas.AddStylesFromFile(PChar(FileName), nil);
  if Error <> GC_NO_ERROR then
    raise Exception.Create(Format('XML parse error of file %s',
      [FileName]));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.RenderScene;

// This is the actual paint routine. All initialization must already have been done.

var
  Start,
  Stop: Int64;
  Time: Single;
  I: Integer;

begin
  QueryPerformanceCounter(Start);

  Canvas.Render(FNativeContent);
  SwapBuffers(FDeviceContext);

  // Compute rendering speed.
  QueryPerformanceCounter(Stop);
  Time := (Stop - Start) / FCounterFrequency;
  if FTimeBuffer[0] = 0 then
  begin
    // First run.
    for I := 0 to FPSBufferSize - 1 do
      FTimeBuffer[I] := Time;
    FTotalTime := FPSBufferSize * Time;
  end
  else
  begin
    FTotalTime := FTotalTime - FTimeBuffer[FCurrentTimeBufferIndex];
    FTimeBuffer[FCurrentTimeBufferIndex] := Time;
    FTotalTime := FTotalTime + FTimeBuffer[FCurrentTimeBufferIndex];
  end;
  Inc(FCurrentTimeBufferIndex);
  if FCurrentTimeBufferIndex = FPSBufferSize then
    FCurrentTimeBufferIndex := 0;
  FFPS := FPSBufferSize / FTotalTime;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.ZoomIn(X, Y: Integer);

begin
  if FCurrentZoomStep < MaxZoomSteps then
  begin
    Inc(FCurrentZoomStep);
    ApplyZoomStep(X, Y);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGenericCanvasViewer.ZoomOut(X, Y: Integer);

begin
  if FCurrentZoomStep > 0 then
  begin
    Dec(FCurrentZoomStep);
    ApplyZoomStep(X, Y);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InitializeGlobalStructures;

begin
  // Mask out exceptions for invalid floating point conversions.
  Set8087CW($133F);
  
  Screen.Cursors[crZoomIn] := LoadCursor(HInstance, 'GC_ZoomInCursor');
  Screen.Cursors[crZoomOut] := LoadCursor(HInstance, 'GC_ZoomOutCursor');
  Screen.Cursors[crHandOpen] := LoadCursor(HInstance, 'GC_PanningHandOpenCursor');
  Screen.Cursors[crHandClosed] := LoadCursor(HInstance, 'GC_PanningHandClosedCursor');

  Screen.Cursors[crMoveAll] := LoadCursor(HInstance, 'GC_MoveAllCursor');
  Screen.Cursors[crMoveEast] := LoadCursor(HInstance, 'GC_MoveEastCursor');
  Screen.Cursors[crMoveEastWest] := LoadCursor(HInstance, 'GC_MoveEastWestCursor');
  Screen.Cursors[crMoveNorth] := LoadCursor(HInstance, 'GC_MoveNorthCursor');
  Screen.Cursors[crMoveNorthEast] := LoadCursor(HInstance, 'GC_MoveNorthEastCursor');
  Screen.Cursors[crMoveNorthSouth] := LoadCursor(HInstance, 'GC_MoveNorthSouthCursor');
  Screen.Cursors[crMoveNorthWest] := LoadCursor(HInstance, 'GC_MoveNorthWestCursor');
  Screen.Cursors[crMoveSouth] := LoadCursor(HInstance, 'GC_MoveSouthCursor');
  Screen.Cursors[crMoveSouthEast] := LoadCursor(HInstance, 'GC_MoveSouthEastCursor');
  Screen.Cursors[crMoveSouthWest] := LoadCursor(HInstance, 'GC_MoveSouthWestCursor');
  Screen.Cursors[crMoveWest] := LoadCursor(HInstance, 'GC_MoveWestCursor');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure FinalizeGlobalStructures;

begin

end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  InitializeGlobalStructures;
finalization
  FinalizeGlobalStructures;
end.



