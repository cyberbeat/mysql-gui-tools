unit MyxRoutineEditorFrame;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UniCodeEditor, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls,
  Grt, UCEHighlighter, UCESQLHighlighter, ActnList, TntActnList, Menus,
  TntMenus, PngImage, PngTools, AuxFuncs;

type
  THeaderDragEvent = procedure(Sender: TObject; YOffset: Integer) of object;

  TRoutineFrame = class(TFrame)
    RoutinePanel: TTntPanel;
    RoutineHeaderPanel: TTntPanel;
    HeaderShape: TTntShape;
    RoutineLbl: TTntLabel;
    RoutineUce: TUniCodeEdit;
    UCESQLHighlighter: TUCESQLHighlighter;
    RoutinePopupMenu: TTntPopupMenu;
    TntActionList1: TTntActionList;
    DeleteRoutineAction: TTntAction;
    DeleteProcedureMI: TTntMenuItem;
    StateImg: TTntImage;
    SetBodyProcedureAction: TTntAction;
    ProcedureBodyMI: TTntMenuItem;
    N1: TTntMenuItem;
    SetBodyFunctionAction: TTntAction;
    FunctionBodyMI: TTntMenuItem;
    procedure DeleteRoutineActionExecute(Sender: TObject);
    procedure DeleteRoutineActionUpdate(Sender: TObject);
    procedure HeaderShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HeaderShapeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure HeaderShapeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StateImgClick(Sender: TObject);
    procedure SetBodyFunctionActionExecute(Sender: TObject);
    procedure SetBodyProcedureActionExecute(Sender: TObject);
  private
    { Private declarations }
    FSchema: Pointer;
    FRoutine: Pointer;
    FOnChange: TNotifyEvent;
    FOnDelete: TNotifyEvent;
    FOnHeaderDragStart: TNotifyEvent;
    FOnHeaderDragStop: TNotifyEvent;
    FOnHeaderDrag: THeaderDragEvent;

    FDragStarted: Boolean;
    FDragYStart: Integer;

    FAutoCalcSize,
    FExpanded: Boolean;
    FExpandedHeight: Integer;

    FCollapsedPngImg,
    FExpandedPngImg: TPngObject;
  protected
    function GetGrt: TGrt;
    procedure SetRoutine(Routine: Pointer); virtual;
    procedure DoSqlUceChange(Sender: TObject; Line: TUCELine);  virtual;

    procedure SetAutoCalcSize(AutoCalcSize: Boolean);
    procedure SetExpanded(Expanded: Boolean);

    procedure Changed;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property Schema: Pointer read FSchema write FSchema;
    property Routine: Pointer read FRoutine write SetRoutine;
    property Grt: TGrt read GetGrt;

    property OnChange: TNotifyEvent write FOnChange;
    property OnDelete: TNotifyEvent write FOnDelete;

    property OnHeaderDragStart: TNotifyEvent write FOnHeaderDragStart;
    property OnHeaderDragStop: TNotifyEvent write FOnHeaderDragStop;
    property OnHeaderDrag: THeaderDragEvent write FOnHeaderDrag;

    property AutoCalcSize: Boolean read FAutoCalcSize write SetAutoCalcSize;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ExpandedHeight: Integer read FExpandedHeight write FExpandedHeight;

    procedure ApplyChanges;
  end;

  TTriggerFrame = class(TRoutineFrame)
    procedure SetRoutine(Routine: Pointer); override;
    procedure ApplyChanges;
    procedure DoSqlUceChange(Sender: TObject; Line: TUCELine); override;
  end;

implementation

{$R *.dfm}


// -----------------------------------------------------------------------------

constructor TRoutineFrame.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  FRoutine := nil;
  FSchema := nil;
  FOnChange := nil;
  FOnDelete := nil;

  FDragStarted := False;

  FAutoCalcSize := False;
  FExpanded := True;

  RoutineUce.Content.OnChangeLine := DoSqlUceChange;

  FCollapsedPngImg := LoadPNGImageFromResource('source_collapsed');
  FExpandedPngImg := LoadPNGImageFromResource('source_expanded');

  StateImg.Picture.Assign(FExpandedPngImg);
end;

// -----------------------------------------------------------------------------

function TRoutineFrame.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.SetRoutine(Routine: Pointer);

begin
  FRoutine := Routine;

  if (FRoutine <> nil) then
  begin
    RoutineLbl.Caption := Grt.DictString[FRoutine, 'name'];

    RoutineUce.Content.Text :=
      Grt.DictString[FRoutine, 'routineCode'];
  end;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.ApplyChanges;

var
  Val: Pointer;
  Name: WideString;
  Sql: WideString;
  Task: IGrtTask;

begin
  if (FRoutine = nil) then
    Exit;

  Sql := Trim(RoutineUce.Content.Text);

  Task := Grt.CreateStandardTask(_('Getting a routine''s name'), 'DbUtils', 'getRoutineName', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];

    if (Name <> '') then
    begin
      Grt.DictString[FRoutine, 'name'] := Name;
      RoutineLbl.Caption := Name;
    end;

    Grt.ValueRelease(Val);
  end;

  Task := Grt.CreateStandardTask(_('Getting a routine''s type'), 'DbUtils', 'getRoutineType', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];
    Grt.DictString[FRoutine, 'routineType'] := WideUpperCase(Name);

    Grt.ValueRelease(Val);
  end;

  Grt.DictString[FRoutine, 'routineCode'] := Sql;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.DoSqlUceChange(Sender: TObject; Line: TUCELine);

begin
  if FRoutine = nil then
    FRoutine := Grt.ExecuteStandardTask(_('Adding new routine'), 'Workbench', 'addRoutine', [Schema, 'New_Routine'], True);

  Changed;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.Changed;

begin
  if (Assigned(FOnChange)) then
    FOnChange(self);
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.DeleteRoutineActionExecute(Sender: TObject);

begin
  if (Assigned(FOnDelete)) then
    FOnDelete(self);
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.DeleteRoutineActionUpdate(Sender: TObject);

begin
  if (Sender is TTntAction) then
    TTntAction(Sender).Enabled := (FRoutine <> nil);
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.HeaderShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (FExpanded) then
  begin
    FDragStarted := True;
    FDragYStart := Mouse.CursorPos.Y;

    if (Assigned(FOnHeaderDragStart)) then
      FOnHeaderDragStart(self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.HeaderShapeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

begin
  if FDragStarted and Assigned(FOnHeaderDrag) then
    FOnHeaderDrag(self, Mouse.CursorPos.Y - FDragYStart);
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.HeaderShapeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (FDragStarted) then
  begin
    FDragStarted := False;

    if (Assigned(FOnHeaderDragStop)) then
      FOnHeaderDragStop(self);

    if (Mouse.CursorPos.Y - FDragYStart <> 0) then
     Changed;
  end;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.SetAutoCalcSize(AutoCalcSize: Boolean);

begin

end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.SetExpanded(Expanded: Boolean);

begin
  if (FExpanded <> Expanded) then
  begin
    FExpanded := Expanded;

    if (FExpanded) then
      StateImg.Picture.Assign(FExpandedPngImg)
    else
      StateImg.Picture.Assign(FCollapsedPngImg);

    if (Not(Expanded)) then
    begin
      StateImg.Picture.Assign(FCollapsedPngImg);
      FExpandedHeight := Height;
      Height := 16;
    end
    else
    begin
      StateImg.Picture.Assign(FExpandedPngImg);
      Height := FExpandedHeight;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.StateImgClick(Sender: TObject);

begin
  Expanded := Not(Expanded);

  Changed;
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.SetBodyFunctionActionExecute(Sender: TObject);

begin
  if (Trim(RoutineUce.Content.Text) <> '') then
    if (ShowModalDialog(_('Replace Text'),
      _('Are you sure you want to replace the current routine text?'),
      myx_mtConfirmation, _('Replace') + #13#10 + _('Cancel')) <> 1) then
      Exit;

  RoutineUce.Content.Text :=
    '-- Comment' + #13#10 +
    '-- ' + #13#10#13#10 +
    'CREATE FUNCTION test() RETURNS INT' + #13#10 +
    'BEGIN' + #13#10 +
    '  RETURN 0;' + #13#10 +
    'END';
end;

// -----------------------------------------------------------------------------

procedure TRoutineFrame.SetBodyProcedureActionExecute(Sender: TObject);

begin
  if (Trim(RoutineUce.Content.Text) <> '') then
    if (ShowModalDialog(_('Replace Text'),
      _('Are you sure you want to replace the current routine text?'),
      myx_mtConfirmation, _('Replace') + #13#10 + _('Cancel')) <> 1) then
      Exit;

  RoutineUce.Content.Text :=
    '-- Comment' + #13#10 +
    '-- ' + #13#10#13#10 +
    'CREATE PROCEDURE test()' + #13#10 +
    'BEGIN' + #13#10#13#10 +
    'END';
end;

// -----------------------------------------------------------------------------

procedure TTriggerFrame.SetRoutine(Routine: Pointer);

var
  Table: Pointer;
  Stmt: WideString;

begin
  FRoutine := Routine;

  if (FRoutine <> nil) then
  begin
    RoutineLbl.Caption := Grt.DictString[FRoutine, 'name'];
    Table := Grt.DictRef[FRoutine, 'owner'];
    Stmt := Grt.DictString[FRoutine, 'statement'];

    RoutineUce.Content.Text :=
      'CREATE TRIGGER ' +
      Grt.DictString[FRoutine, 'name'] + ' ' +
      Grt.DictString[FRoutine, 'timing'] + ' ' +
      Grt.DictString[FRoutine, 'event'] + ' ON ' +
      Grt.DictString[Table, 'name'] + ' FOR EACH ROW' + #13 + #10 +
      Stmt;
  end
  else
    RoutineLbl.Caption := 'New_Trigger';
end;

// -----------------------------------------------------------------------------

function RemoveChar(Str : WideString; c: WideChar) : WideString;

var
  R: WideString;
  I: Integer;

begin
  for I := 1 to Length(Str) do
    if Str[i] <> c then
      R := R + Str[i];
  Result := R;    
end;

// -----------------------------------------------------------------------------

procedure TTriggerFrame.ApplyChanges;

var
  Val: Pointer;
  Name: WideString;
  Sql: WideString;
  Task: IGrtTask;

begin
  if (FRoutine = nil) then
    Exit;

  Sql := Trim(RoutineUce.Content.Text);

  Task := Grt.CreateStandardTask(_('Getting a trigger''s name'), 'DbUtils', 'getRoutineName', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];

    if (Name <> '') then
    begin
      Grt.DictString[FRoutine, 'name'] := Name;
      RoutineLbl.Caption := Name;
    end;

    Grt.ValueRelease(Val);
  end;

  Task := Grt.CreateStandardTask(_('Getting a trigger''s statement'), 'DbUtils', 'getTriggerStatement', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];
    Name := RemoveChar(Name, #13);
    Grt.DictString[FRoutine, 'statement'] := Name;

    Grt.ValueRelease(Val);
  end;

  Task := Grt.CreateStandardTask(_('Getting a trigger''s event'), 'DbUtils', 'getTriggerEvent', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];
    Grt.DictString[FRoutine, 'event'] := Name;

    Grt.ValueRelease(Val);
  end;

  Task := Grt.CreateStandardTask(_('Getting a trigger''s timing'), 'DbUtils', 'getTriggerTiming', [Sql], nil, nil, True);
  Grt.AddTaskAndWait(Task, True);
  Val := Task.Result;

  if (Val <> nil) then
  begin
    Name := Grt.ValueString[Val];
    Grt.DictString[FRoutine, 'timing'] := Name;

    Grt.ValueRelease(Val);
  end;

end;

// -----------------------------------------------------------------------------

procedure TTriggerFrame.DoSqlUceChange(Sender: TObject; Line: TUCELine);

var
  s: widestring;

begin
  if FRoutine = nil then
  begin
    s := Grt.DictStructName[Schema];
    s := Grt.DictString[Schema, 'name'];
    FRoutine := Grt.ExecuteStandardTask(_('Adding new trigger'), 'Workbench', 'createTrigger', [Schema, 'New_Trigger'],
      True);
  end;

  Changed;
end;

// -----------------------------------------------------------------------------

end.
