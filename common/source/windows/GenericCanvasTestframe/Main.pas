unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GenericCanvasViewer, StdCtrls, ComCtrls, ToolWin, ImgList, XPStyleActnCtrls, ActnList, ActnMan, XPMan,
  ExtCtrls, Menus;

type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ImageList1: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ActionManager: TActionManager;
    NewAction: TAction;
    NewFigureAction: TAction;
    ClearAction: TAction;
    ToolButton5: TToolButton;
    CreateMultiFiguresAction: TAction;
    XPManifest1: TXPManifest;
    ZoomInAction: TAction;
    ZoomOutAction: TAction;
    ToolButton6: TToolButton;
    Timer1: TTimer;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    MainMenu: TMainMenu;
    FileMI: TMenuItem;
    ExportResultsetMI: TMenuItem;
    ExportModelasImageMI: TMenuItem;
    ExportSelectedObjectsasImageMI: TMenuItem;
    CloseTabMI: TMenuItem;
    ImportErwin41MI: TMenuItem;
    ImportDBDesigner4ModelMi: TMenuItem;
    N4: TMenuItem;
    PageSetupMI: TMenuItem;
    PrintMI: TMenuItem;
    N10: TMenuItem;
    MenuItemExit: TMenuItem;
    SaveDialog: TSaveDialog;
    procedure ZoomOutActionExecute(Sender: TObject);
    procedure ZoomInActionExecute(Sender: TObject);
    procedure CreateMultiFiguresActionExecute(Sender: TObject);
    procedure NewFigureActionExecute(Sender: TObject);
    procedure ClearActionExecute(Sender: TObject);
    procedure ViewerChange(Sender: TObject; Source: TGCBase; Reason: TGCChangeReason);
    procedure ViewerError(Sender: TObject; const Message: string);
    procedure FormCreate(Sender: TObject);
    procedure ViewerZoomChange(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure ExportModelasImageMIClick(Sender: TObject);
  private
    FViewer1: TGenericCanvasViewer;
    FViewer2: TGenericCanvasViewer;
    FMainLayer1: TLayer;
    FMainLayer2: TLayer;

    // Connection decorations.
    FDecorations: array[1..2, 0..3] of TFigure;
    FLabelsAndCenter: array[1..2, 0..3] of TFigure;
    procedure CreateFigures(Number: Integer; Amount: Integer; FigureType: Integer = -1; ConnectionCount: Integer = 0);
    procedure CreateViewer(Number: Integer; L, T, W, H: Integer);
    procedure SetupCanvas;
  protected
  public
  end;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  MMSystem, Math;

const
  BaseWidth  = 7000;
  BaseHeight = 5000;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  SetupCanvas;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateFigures(Number: Integer; Amount: Integer; FigureType: Integer = -1; ConnectionCount: Integer = 0);

const
  Objects: array[0..4] of PChar = (
    'db.workbench.RoutinesElement',
    'db.workbench.LayerFrame',
    'db.workbench.TableElement',
    'db.workbench.ViewElement',
    'db.workbench.NoteElement'
  );

var
  Figure: TFigure;
  Scale: Double;
  I: Integer;
  Instance: TFigureInstance;
  FirstInstance: TFigureInstance;
  Connection: TConnection;
  ConnectionInstance: TConnectionInstance;
  ConnectionDecoration: TConnectionDecorations;

  Viewer: TGenericCanvasViewer;
  Layer: TLayer;

begin
  case Number of
    1:
      begin
        Viewer := FViewer1;
        Layer := FMainLayer1;
      end;
  else
    Viewer := FViewer2;
    Layer := FMainLayer2;
  end;

  FirstInstance := nil;
  for I := 0 to Amount - 1 do
  begin
    if FigureType > -1 then
    begin
      Figure := Viewer.Canvas.CreateFigure(Objects[FigureType], 'full');
      Scale := 1;
    end
    else
    begin
      Figure := Viewer.Canvas.CreateFigure(Objects[Random(5)], 'full');
      Scale := 0.3 + Random(1);
    end;
    Instance := Layer.CreateInstance(Figure);
    Instance.Scale(Scale, Scale, 1, False);
    Instance.Translate(Random(BaseWidth) * 0.9, Random(BaseHeight) * 0.9, 0, False);

    if ConnectionCount > 0 then
    begin
      if  (I mod ConnectionCount) = 0 then
        FirstInstance := Instance
      else
      begin
        // Create connections between the first and the current instance.
        Connection := Viewer.Canvas.CreateConnection(FirstInstance.Figure, Figure);
        FillChar(ConnectionDecoration, SizeOf(ConnectionDecoration), 0);
        with ConnectionDecoration do
        begin
          End1Decoration := FDecorations[Number, Random(4)];
          End1Label := FLabelsAndCenter[Number, 0];
          CenterDecoration := FLabelsAndCenter[Number, 1];
          CenterLabel := FLabelsAndCenter[Number, 2];
          End2Decoration := FDecorations[Number, Random(4)];
          End2Label := FLabelsAndCenter[Number, 3];
        end;
        Connection.DecorationsSet(ConnectionDecoration);

        ConnectionInstance := Viewer.Canvas.CurrentViewGet.CreateConnectionInstance(Connection, FirstInstance, Instance);
        ConnectionInstance.LineStyleSet(GC_CONNECTION_STYLE_SOLID);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateViewer(Number: Integer; L, T, W, H: Integer);

var
  Viewer: TGenericCanvasViewer;
  View: TView;
  Layer: TLayer;

begin
  Viewer := TGenericCanvasViewer.Create(Self);
  with Viewer do
  begin
    OnError := ViewerError;
    OnChange := ViewerChange;
    OnZoomChange := ViewerZoomChange;

    Parent := Self;
    HandleNeeded;
    SetBounds(L, T, W, H);
    BaseSizeX := BaseWidth;
    BaseSizeY := BaseHeight;

    Canvas.SetTexturePath('../images');
    LoadStyles('./xml/layout.styles.db.xml');
    LoadStyles('./xml/layout.styles.db.connection.xml');
    LoadStyles('./xml/layout.styles.db.routine.xml');
    LoadStyles('./xml/layout.styles.db.table.xml');
    LoadStyles('./xml/layout.styles.db.view.xml');
    LoadLayouts('./xml/layout.figures.db.xml');

    View := Canvas.CreateView('Default');
    View.SetWorkspace(BaseSizeX, BaseSizeY);

    Canvas.CurrentViewGet.Color(1, 1, 1, 1);

    AutoScrollDelay := 200;

    // The layers are automatically added to the canvas. We keep references, though, for adding figures to them.
    Layer := Canvas.CreateLayer('Main', True);

    // Prepare connection decorations. They are shared among all connections.
    FDecorations[Number, 0] := Canvas.CreateFigure('db.Connection.End.Single.Optional', nil);
    FDecorations[Number, 1] := Canvas.CreateFigure('db.Connection.End.Single.Mandatory', nil);
    FDecorations[Number, 2] := Canvas.CreateFigure('db.Connection.End.Multiple.Optional', nil);
    FDecorations[Number, 3] := Canvas.CreateFigure('db.Connection.End.Multiple.Mandatory', nil);

    FLabelsAndCenter[Number, 0] := Canvas.CreateFigure('db.Connection.End1.Text', nil);
    FLabelsAndCenter[Number, 1] := Canvas.CreateFigure('db.Connection.Center', nil);
    FLabelsAndCenter[Number, 2] := Canvas.CreateFigure('db.Connection.Center.Text', nil);
    FLabelsAndCenter[Number, 3] := Canvas.CreateFigure('db.Connection.End2.Text', nil);
  end;

  case Number of
    1:
      begin
        FViewer1 := Viewer;
        FMainLayer1 := Layer;
      end;
    2:
      begin
        FViewer2 := Viewer;
        FMainLayer2 := Layer;
      end;
  end;

  //Options := Options + [cvoAutoCenterZoom] - [];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ViewerChange(Sender: TObject; Source: TGCBase; Reason: TGCChangeReason);

var
  Bounds: TBoundingBox;

begin
  case Reason of
    GC_CHANGE_SELECTION_ADD: // Figure instance added to selection.
      ;
    GC_CHANGE_VIEW_RUBBERRECT_STOP:
      begin
        // Copy content to clipboard if the rubber rectangle was drawn together with the alt key.
        if (GetKeyState(VK_MENU) and $80) <> 0 then
        begin
          FViewer1.Canvas.CurrentViewGet.GetLastRubberBounds(Bounds);
          FViewer1.CopyToClipboard(Bounds);
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ViewerError(Sender: TObject; const Message: string);

begin
  StatusBar.SimpleText := Message;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ClearActionExecute(Sender: TObject);

begin
  FViewer1.Canvas.CurrentViewGet.ClearContent(False);
  //FViewer2.Canvas.CurrentView.ClearContent(False);
  StatusBar.SimpleText := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.NewFigureActionExecute(Sender: TObject);

begin
  CreateFigures(1, 1, 2);
  //CreateFigures(2, 1, 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateMultiFiguresActionExecute(Sender: TObject);

var
  StartTime: DWORD;

begin
  StartTime := timeGetTime;
  CreateFigures(1, 100, -1);
  //CreateFigures(2, 100, -1);
  Statusbar.SimpleText := Format('Time used: %d ms', [timeGetTime - StartTime]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ZoomInActionExecute(Sender: TObject);

begin
  FViewer1.ZoomIn(FViewer1.Width div 2, FViewer1.Height div 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ZoomOutActionExecute(Sender: TObject);

begin
  FViewer1.ZoomOut(FViewer1.Width div 2, FViewer1.Height div 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ViewerZoomChange(Sender: TObject);

begin
  // Both zoom factors should be the same to keep aspect ratio.
  if Assigned(FViewer1) then
    Caption := Format('Generic Canvas Test - %.2f%%', [FViewer1.CurrentZoom * 100]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton6Click(Sender: TObject);

begin
  CreateFigures(1, 100, 2);
  //CreateFigures(2, 100, 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Timer1Timer(Sender: TObject);

begin
  StatusBar.SimpleText := Format('%.3f FPS', [FViewer1.FPS]);
  StatusBar.Update;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ToolButton8Click(Sender: TObject);

// Destroy the current viewer and build a new one.

begin
  FreeAndNil(FViewer1);
  FreeAndNil(FViewer2);
  FMainLayer1 := nil;
  FMainLayer2 := nil;

  SetupCanvas;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SetupCanvas;

begin
  Randomize;

  CreateViewer(1, 0, 0, 350, 670);
  FViewer1.Align := alClient;

  //CreateViewer(2, 350, 0, 700, 670);

  ActiveControl := FViewer1;
  Caption := Format('Generic Canvas Test - %.2f%%', [FViewer1.CurrentZoom * 100]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MenuItemExitClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ExportModelasImageMIClick(Sender: TObject);

var
  Bounds: TViewport;
  
begin
  with SaveDialog do
    if Execute then
    begin
      Application.ProcessMessages;
      Screen.Cursor := crHourGlass;
      try
        Bounds.Left := 0;
        Bounds.Top := 0;
        Bounds.Width := -1;
        Bounds.Height := -1;
        FViewer1.Canvas.RenderToFile(PChar(Utf8Encode(FileName)), TGCFileFormat(FilterIndex - 1),
          'An image says more than thousand words', 'MySQL Workbench', 1, Bounds);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
