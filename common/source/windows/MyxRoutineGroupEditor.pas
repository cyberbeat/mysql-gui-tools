unit MyxRoutineGroupEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UCEHighlighter, UCESQLHighlighter, TntForms, StdCtrls,
  TntStdCtrls, ExtCtrls, TntExtCtrls, UniCodeEditor, MyxEditor,
  MyxRoutineEditorFrame, Contnrs, ComCtrls, TntComCtrls,
  Grt, ActnList, TntActnList, pngimage;

type
  TMyxRoutineGroupEditorForm = class(TMyxEditorForm)
    HeaderPnl: TTntPanel;
    BottomPnl: TTntPanel;
    ApplyChangesBtn: TTntButton;
    CancelBtn: TTntButton;
    LeftPnl: TTntPanel;
    RightPnl: TTntPanel;
    RoutinePageControl: TTntPageControl;
    CommentSheet: TTntTabSheet;
    RoutineGroupSheet: TTntTabSheet;
    RoutinesScrollBox: TTntScrollBox;
    TntShape1: TTntShape;
    TntShape2: TTntShape;
    TntShape3: TTntShape;
    TntShape4: TTntShape;
    TntActionList1: TTntActionList;
    ApplyChangesAndCloseAction: TTntAction;
    RoutineGroupSheetPnl: TTntPanel;
    CommentMemo: TTntMemo;
    EditorIcon: TTntImage;
    NameEd: TTntEdit;
    NameLbl: TTntLabel;
    TntLabel6: TTntLabel;
    TntLabel1: TTntLabel;
    RoutinesSqlPnl: TTntPanel;
    ApplyChangesAction: TTntAction;
    DiscardChangesAction: TTntAction;
    CloseAction: TTntAction;
    RoutineGroupSheetBgShape: TTntShape;
    CommentSheetPnl: TTntPanel;
    CommentSheetBgShape: TTntShape;
    TntImage1: TTntImage;
    TntLabel2: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelBtnClick(Sender: TObject);
    procedure NameEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ApplyChangesAndCloseActionExecute(Sender: TObject);
    procedure ApplyChangesActionExecute(Sender: TObject);
    procedure ApplyChangesActionUpdate(Sender: TObject);
    procedure DiscardChangesActionExecute(Sender: TObject);
    procedure DataChange(Sender: TObject);
  private
    { Private declarations }
    FRoutineFrames: TObjectList;
    FRoutineDragStartHeight: Integer;
  protected
    procedure SetModified(Modified: Boolean); override;
    procedure OnRoutineChange(Sender: TObject);
    procedure OnRoutineDelete(Sender: TObject);
    procedure AddRoutineBox(Routine: Pointer = nil);

    procedure DoRoutineHeaderDragStart(Sender: TObject);
    procedure DoRoutineHeaderDragStop(Sender: TObject);
    procedure DoRoutineHeaderDrag(Sender: TObject; YOffset: Integer);

    procedure DoCommentUceChange(Sender: TObject; Line: TUCELine);
  public
    { Public declarations }
    procedure StartEditObject(Obj: Pointer); override;
    procedure ApplyChanges; override;

    procedure DockEditor; override;

    procedure FocusFirstControl; override;
  end;

var
  MyxRoutineGroupEditorForm: TMyxRoutineGroupEditorForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.FormCreate(Sender: TObject);

begin
  FRoutineFrames := TObjectList.Create;

  RoutinePageControl.ActivePageIndex := 0;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.FormDestroy(Sender: TObject);

begin
  FRoutineFrames.Free;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);

begin
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.StartEditObject(Obj: Pointer);

var
  RoutineList: Pointer;
  I, Count: Integer;

begin
  inherited;

  // Initialize editor
  FInitializingContent := True;
  try
    NameEd.Text := Grt.DictString[Obj, 'name'];

    RoutineList := Grt.DictItem[Obj, 'routines'];
    Count := Grt.ListCount(RoutineList);

    for I := 0 to Count do
    begin
      if (I < Count) then
        AddRoutineBox(Grt.ListRefItem[RoutineList, I])
      else
        AddRoutineBox;
    end;

    CommentMemo.Text := Grt.DictString[Obj, 'comment'];
  finally
    FInitializingContent := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.SetModified(Modified: Boolean);

begin
  inherited;


  ApplyChangesActionUpdate(ApplyChangesAction);
  ApplyChangesActionUpdate(DiscardChangesAction);

  if (FEditorTabFrame <> nil) then
    FEditorTabFrame.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.ApplyChanges;

var
  I: Integer;
  ExpandedHeights: Pointer;
  ExpandedStates: Pointer;

begin
  if (NameEd.Text <> '') then
    Grt.DictString[ObjectCopy, 'name'] := NameEd.Text;

  ExpandedStates := Grt.ListNew(GrtIntValue);
  ExpandedHeights := Grt.ListNew(GrtIntValue);

  for I := 0 to FRoutineFrames.Count - 1 do
  begin
    TRoutineFrame(FRoutineFrames[i]).ApplyChanges;

    Grt.ListAdd(ExpandedStates, Grt.ValueFromInt(
      Ord(TRoutineFrame(FRoutineFrames[i]).Expanded)), False);

    Grt.ListAdd(ExpandedHeights, Grt.ValueFromInt(
      TRoutineFrame(FRoutineFrames[i]).ExpandedHeight), False);
  end;

  Grt.DictItem[ObjectCopy, 'routineExpandedStates'] := ExpandedStates;
  Grt.DictItem[ObjectCopy, 'routineExpandedHeights'] := ExpandedHeights;

  Grt.DictString[ObjectCopy, 'comment'] := CommentMemo.Text;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.OnRoutineChange(Sender: TObject);

var
  RoutineList: Pointer;

begin
  if (FInitializingContent) then
    Exit;

  // if this is the last RoutineFrame, add a new one
  if (FRoutineFrames[FRoutineFrames.Count - 1] = Sender) and
    (Length(TRoutineFrame(Sender).RoutineUce.Content.Text) > 0) then
  begin
    AddRoutineBox;

    if (Sender is TRoutineFrame) then
    begin
      RoutineList := Grt.DictItem[ObjectCopy, 'routines'];
      Grt.ListAdd(RoutineList, Grt.DictItem[TRoutineFrame(Sender).Routine, '_id']);
    end;
  end;

  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.AddRoutineBox(Routine: Pointer);

var
  RoutineFrame: TRoutineFrame;
  RoutineFrameHeight, RoutineFrameExpanded: Integer;
  List, Val: Pointer;

begin
  RoutineFrame := TRoutineFrame.Create(nil);
  RoutineFrame.Parent := RoutinesScrollBox;
  RoutineFrame.Top := 10000;
  RoutineFrame.Align := alTop;
  RoutineFrame.OnChange := OnRoutineChange;
  RoutineFrame.OnDelete := OnRoutineDelete;
  RoutineFrame.OnHeaderDrag := DoRoutineHeaderDrag;
  RoutineFrame.OnHeaderDragStart := DoRoutineHeaderDragStart;
  RoutineFrame.OnHeaderDragStop := DoRoutineHeaderDragStop;

  RoutineFrame.Schema := Grt.DictRef[ObjectCopy, 'owner'];
  RoutineFrame.Routine := Routine;

  RoutineFrameHeight := 130;
  List := Grt.DictItem[ObjectCopy, 'routineExpandedHeights'];
  if (List <> nil) then
  begin
    if (FRoutineFrames.Count < Grt.ListCount(List)) then
    begin
      Val := Grt.ListItem[List, FRoutineFrames.Count];
      if (Val <> nil) then
        RoutineFrameHeight := Grt.ValueInt[Val];
    end;
  end;


  RoutineFrame.AutoCalcSize := (RoutineFrameHeight = 0);
  RoutineFrame.Height := RoutineFrameHeight;
  RoutineFrame.ExpandedHeight := RoutineFrame.Height;

  RoutineFrameExpanded := 1;
  List := Grt.DictItem[ObjectCopy, 'routineExpandedStates'];
  if (List <> nil) then
  begin
    if (FRoutineFrames.Count < Grt.ListCount(List)) then
    begin
      Val := Grt.ListItem[List, FRoutineFrames.Count];

      if (Val <> nil) then
        RoutineFrameExpanded := Grt.ValueInt[Val];
    end;
  end;

  RoutineFrame.Expanded := (RoutineFrameExpanded = 1);

  FRoutineFrames.Add(RoutineFrame);
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.CancelBtnClick(Sender: TObject);

begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.NameEdKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

begin
  if (Key = VK_Return) and (FRoutineFrames.Count > 0) then
  begin
    TRoutineFrame(FRoutineFrames[0]).RoutineUce.SetFocus;
    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.OnRoutineDelete(Sender: TObject);

var
  RoutineList: Pointer;
  I, Count: Integer;

begin
  RoutineList := Grt.DictItem[ObjectCopy, 'routines'];
  Count := Grt.ListCount(RoutineList);

  for I := 0 to Count - 1 do
  begin
    if (Grt.ListRefItem[RoutineList, I] =
      TRoutineFrame(Sender).Routine) then
    begin
      Grt.ListDel(RoutineList, I);
      break;
    end;
  end;

  FRoutineFrames.Remove(Sender);

  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DoRoutineHeaderDrag(Sender: TObject; YOffset: Integer);

var
  Index: Integer;
  RoutineFrame: TRoutineFrame;
begin
  Index := FRoutineFrames.IndexOf(Sender);

  if (Index > 0) then
  begin
    RoutineFrame := TRoutineFrame(FRoutineFrames[Index - 1]);
    if (RoutineFrame.Expanded) then
      RoutineFrame.Height :=
        FRoutineDragStartHeight + YOffset;

    RoutineFrame.ExpandedHeight :=
      FRoutineDragStartHeight + YOffset;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DoRoutineHeaderDragStart(Sender: TObject);

var
  Index: Integer;

begin
  Index := FRoutineFrames.IndexOf(Sender);

  if (Index > 0) then
    FRoutineDragStartHeight :=
      TRoutineFrame(FRoutineFrames[Index - 1]).Height;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DoRoutineHeaderDragStop(Sender: TObject);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DoCommentUceChange(Sender: TObject; Line: TUCELine);

begin
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.ApplyChangesAndCloseActionExecute(
  Sender: TObject);

begin
  ApplyChanges;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.ApplyChangesActionExecute(
  Sender: TObject);

begin
  ApplyChanges;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.ApplyChangesActionUpdate(
  Sender: TObject);
begin
  if (Sender is TTntAction) then
    TTntAction(Sender).Enabled := Modified;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DiscardChangesActionExecute(
  Sender: TObject);

begin
  DiscardChanges;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DockEditor;

begin
  PrepareEditorDock(RoutinePageControl);

  FEditorTabFrame.ApplyAction := ApplyChangesAction;
  FEditorTabFrame.DiscardAction := DiscardChangesAction;

  RoutineGroupSheetBgShape.Visible := True;
  CommentSheetBgShape.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.DataChange(Sender: TObject);

begin
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxRoutineGroupEditorForm.FocusFirstControl;

begin
  if (FEditorTabFrame <> nil) then
  begin
    if (FEditorTabFrame.SelectedTab = 0) then
      NameEd.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

end.
