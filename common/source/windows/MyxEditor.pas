unit MyxEditor;

interface

uses
  Forms, MyxBaseForm, Grt, Classes, Controls, TntExtCtrls,
  TntComCtrls, Graphics,
  MyxTabHeader;

type
  TMyxEditorForm = class(TMyxBaseForm)
  protected
    FObj,
    FOriginalObj: Pointer;
    FObjStructName: WideString;
    FModified: Boolean;
  protected
    FDockPanel: TTntPanel;
    FEditorTabFrame: TMyxTabHeaderFrame;
    FInitializingContent: Boolean;

    procedure SetModified(Modified: Boolean); virtual;
    procedure PrepareEditorDock(PageControl: TTntPageControl);
  public
    constructor Create(AOwner: TComponent); overload; override;

    procedure ApplyChanges; virtual;
    function CloseQuery: Boolean; override;
    procedure DiscardChanges; virtual;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DockEditor; virtual;
    procedure EndEditObject; virtual;
    procedure FocusFirstControl; virtual;
    procedure StartEditObject(Obj: Pointer); virtual;

    property DockPanel: TTntPanel read FDockPanel;
    property Modified: Boolean read FModified write SetModified;
    property ObjectCopy: Pointer read FObj;
    property OriginalObject: Pointer read FOriginalObj;
    property ObjectStructName: WideString read FObjStructName;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  AuxFuncs, GnuGettext;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxEditorForm.Create(AOwner: TComponent);

begin
  inherited;

  FObj := nil;
  FOriginalObj := nil;
  FModified := False;
  FDockPanel := nil;
  FEditorTabFrame := nil;

  FObjStructName := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.StartEditObject(Obj: Pointer);

begin
  FOriginalObj := Obj;
  Grt.ValueRetain(FOriginalObj);
  FObj := Grt.ValueDuplicate(Obj);

  FObjStructName := Grt.DictStructName[Obj];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.ApplyChanges;

var
  Diff: Pointer;

begin
  Grt.ShellVar['editObj'] := FOriginalObj;
  Grt.ShellVar['editObjMod'] := FObj;

  // Apply diff to original object
  Diff := Grt.ValueDiffMake(FOriginalObj, FObj);

  Grt.ShellVar['editDiff'] := Diff;
  Grt.ValueDiffApply(FOriginalObj, Diff);

  Grt.ValueRelease(Diff);

  Modified := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxEditorForm.CloseQuery: Boolean;

var
  DialogResult: Integer;

begin
  Result := inherited CloseQuery;
  if Result and FModified then
  begin
    DialogResult := ShowModalDialog(_('Confirm'), _('There are unapplied changes. Would you like to apply them now?'),
      myx_mtConfirmation, _('Apply') + #13#10 + _('Ignore') + #13#10 + _('Cancel'));
    case DialogResult of
      1: // Apply and close
        ApplyChanges;
      2: // Ignore and close
        ;
      3: // Cancel and stop closing
        Result := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.DiscardChanges;

begin
  Grt.ValueRelease(FObj);
  FModified := False;
  StartEditObject(FOriginalObj);
  Grt.ValueRelease(FOriginalObj);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.EndEditObject;

begin
  Grt.ValueRelease(FObj);
  Grt.ValueRelease(FOriginalObj);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.DoClose(var Action: TCloseAction);

begin
  EndEditObject;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.SetModified(Modified: Boolean);

begin
  if not(FInitializingContent) then
    FModified := Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.PrepareEditorDock(PageControl: TTntPageControl);

var
  I: Integer;
  Tab: TTntTabSheet;
  Pnl, TabPnl, ContentPnl: TTntPanel;
  Shape: TTntShape;

begin
  // Create outer dock panel
  FDockPanel := TTntPanel.Create(self);
  FDockPanel.Parent := self;
  FDockPanel.Caption := '';
  FDockPanel.Width := 700;
  FDockPanel.Height := 550;
  FDockPanel.BevelOuter := bvNone;

  // Create white border
  Shape := TTntShape.Create(self);
  Shape.Pen.Color := clWhite;
  Shape.Brush.Color := clWhite;
  Shape.Parent := FDockPanel;
  Shape.Width := 6;
  Shape.Align := alLeft;

  Shape := TTntShape.Create(self);
  Shape.Pen.Color := clWhite;
  Shape.Brush.Color := clWhite;
  Shape.Parent := FDockPanel;
  Shape.Width := 6;
  Shape.Align := alRight;

  Shape := TTntShape.Create(self);
  Shape.Pen.Color := clWhite;
  Shape.Brush.Color := clWhite;
  Shape.Parent := FDockPanel;
  Shape.Height := 2;
  Shape.Align := alTop;

  ContentPnl := TTntPanel.Create(self);
  ContentPnl.Parent := FDockPanel;
  ContentPnl.Caption := '';
  ContentPnl.BevelOuter := bvNone;
  ContentPnl.Align := alClient;


  // add 1px border lines
  Shape := TTntShape.Create(self);
  Shape.Parent := ContentPnl;
  Shape.Pen.Color := $009C9B91;
  Shape.Brush.Color := $009C9B91;
  Shape.Height := 1;
  Shape.Align := alTop;

  Shape := TTntShape.Create(self);
  Shape.Parent := ContentPnl;
  Shape.Pen.Color := $009C9B91;
  Shape.Brush.Color := $009C9B91;
  Shape.Width := 1;
  Shape.Align := alLeft;

  Shape := TTntShape.Create(self);
  Shape.Pen.Color := $009C9B91;
  Shape.Brush.Color := $009C9B91;
  Shape.Parent := ContentPnl;
  Shape.Width := 1;
  Shape.Align := alRight;


  // Create TabFrame
  FEditorTabFrame := TMyxTabHeaderFrame.Create(self);
  FEditorTabFrame.Parent := ContentPnl;
  FEditorTabFrame.Height := 23;
  FEditorTabFrame.Align := alBottom;

  FEditorTabFrame.ShowButtons := True;
  FEditorTabFrame.HideNewButton := True;
  FEditorTabFrame.TabsAtBottom := True;
  FEditorTabFrame.InversColors := True;
  FEditorTabFrame.ShowDeleteButtons := False;

  FEditorTabFrame.ShowApplyDiscardChangesBtns := True;

  // move tabsheets from windows tabcontrol to EditorTabFrame
  for I := 0 to PageControl.PageCount - 1 do
  begin
    Tab := TTntTabSheet(PageControl.Pages[I]);

    if (Tab.Controls[0] <> nil) and
      (Tab.Controls[0] is TTntPanel) then
    begin
      TabPnl := TTntPanel(Tab.Controls[0]);
      TabPnl.Align := alNone;

      Pnl := TTntPanel.Create(self);
      Pnl.Parent := self;
      Pnl.Width := 700;
      Pnl.Height := 650;
      //Pnl.Align := alClient;
      Pnl.Caption := '';
      Pnl.BevelOuter := bvNone;

      Shape := TTntShape.Create(self);
      Shape.Pen.Color := clBtnFace;
      Shape.Brush.Color := clBtnFace;
      Shape.Parent := Pnl;
      Shape.Width := 7;
      Shape.Align := alLeft;

      Shape := TTntShape.Create(self);
      Shape.Pen.Color := clBtnFace;
      Shape.Brush.Color := clBtnFace;
      Shape.Parent := Pnl;
      Shape.Width := 7;
      Shape.Align := alRight;

      Shape := TTntShape.Create(self);
      Shape.Pen.Color := clBtnFace;
      Shape.Brush.Color := clBtnFace;
      Shape.Parent := Pnl;
      Shape.Height := 5;
      Shape.Align := alTop;

      Shape := TTntShape.Create(self);
      Shape.Pen.Color := clBtnFace;
      Shape.Brush.Color := clBtnFace;
      Shape.Parent := Pnl;
      Shape.Height := 7;
      Shape.Align := alBottom;

      TabPnl.Parent := Pnl;
      TabPnl.Align := alClient;

      FEditorTabFrame.AddTabSheet(Self, Tab.Caption, '',
        Pnl,
        nil,
        False, False, -1, [], True);
    end;
  end;

  FEditorTabFrame.SelectedTab := 0;

  PageControl.Visible := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.DockEditor;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxEditorForm.FocusFirstControl;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

end.
