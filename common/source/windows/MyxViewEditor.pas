unit MyxViewEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, UniCodeEditor, ComCtrls, TntComCtrls,
  UCEHighlighter, UCESQLHighlighter, ExtCtrls, TntExtCtrls,
  MyxEditor, ActnList, TntActnList, pngimage,
  UCEEditorKeyCommands, MyxOptions;

type
  TMyxViewEditorForm = class(TMyxEditorForm)
    BottomPnl: TTntPanel;
    ApplyChangesBtn: TTntButton;
    CancelBtn: TTntButton;
    HeaderPnl: TTntPanel;
    LeftPnl: TTntPanel;
    RightPnl: TTntPanel;
    ViewPageControl: TTntPageControl;
    CommentTabSheet: TTntTabSheet;
    ViewTabSheet: TTntTabSheet;
    UCESQLHighlighter: TUCESQLHighlighter;
    TntActionList1: TTntActionList;
    ApplyChangesAndCloseAction: TTntAction;
    ViewSheetPnl: TTntPanel;
    NameEd: TTntEdit;
    NameLbl: TTntLabel;
    EditorIcon: TTntImage;
    TntLabel6: TTntLabel;
    SqlUce: TUniCodeEdit;
    TntLabel1: TTntLabel;
    CommentSheetPnl: TTntPanel;
    CommentMemo: TTntMemo;
    ApplyChangesAction: TTntAction;
    DiscardChangesAction: TTntAction;
    ViewSheetBgShape: TTntShape;
    TntImage1: TTntImage;
    CommentsSheetBgShape: TTntShape;
    TntLabel2: TTntLabel;
    CloseAction: TTntAction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure ApplyChangesAndCloseActionExecute(Sender: TObject);
    procedure ApplyChangesActionExecute(Sender: TObject);
    procedure ApplyChangesActionUpdate(Sender: TObject);
    procedure DiscardChangesActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetModified(Modified: Boolean); override;
    procedure DoSqlUceChange(Sender: TObject; Line: TUCELine);
  public
    { Public declarations }
    procedure StartEditObject(Obj: Pointer); override;
    procedure ApplyChanges; override;

    procedure DockEditor; override;

    procedure FocusFirstControl; override;
  end;

var
  MyxViewEditorForm: TMyxViewEditorForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.FormCreate(Sender: TObject);

var Stroke: TKeyStroke;

begin
  ViewPageControl.ActivePageIndex := 0;

  // Setup StandardInsertsUCE
  with SqlUce do
  begin
    Content.OnChangeLine := DoSqlUceChange;

    Color := clWindow;
    Font.Name := CommonOptions.OptionString['DefaultCodeFontName'];
    Font.Height := CommonOptions.OptionInt['DefaultCodeFontHeight'];
    Highlighter := UCESQLHighlighter;
    Options := [eoAutoIndent, eoAutoUnindent, eoGroupUndo, eoInserting, eoLineNumbers, eoShowScrollHint, eoSmartTabs,
      eoTripleClicks, eoUndoAfterSave, eoUseSyntaxHighlighting, eoWantTabs, eoUseUndoRedo];

    GutterWidth := 0;
    GutterColor := clBtnFace;
    RightMargin := -1;
    MaxUndo := 32000;

    ScrollHintColor.Foreground := clWhite;
    ScrollHintColor.Background := clAppWorkSpace;
    SelectedColor.Foreground := clHighlightText;
    SelectedColor.Background := clHighlight;
    LineNumberFont.Name := 'Terminal';
    LineNumberFont.Size := 6;

    // Add own keystrokes.

    // Shift+Bk like Bk alone
    Stroke := Keystrokes.Add;
    Stroke.Command := ecDeleteLastChar;
    Stroke.Key := VK_BACK;
    Stroke.Shift := [ssShift];
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.FormDestroy(Sender: TObject);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);

begin
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.StartEditObject(Obj: Pointer);

begin
  inherited;

  // Initialize editor
  FInitializingContent := True;
  try
    NameEd.Text := Grt.DictString[Obj, 'name'];

    SqlUce.Content.Text := Grt.DictString[Obj, 'queryExpression'];

    CommentMemo.Text := Grt.DictString[Obj, 'comment'];
  finally
    FInitializingContent := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.SetModified(Modified: Boolean);

begin
  inherited;

  ApplyChangesActionUpdate(ApplyChangesAction);
  ApplyChangesActionUpdate(DiscardChangesAction);

  if (FEditorTabFrame <> nil) then
    FEditorTabFrame.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.ApplyChanges;

begin
  if (NameEd.Text <> '') then
    Grt.DictString[ObjectCopy, 'name'] := NameEd.Text;

  Grt.DictString[ObjectCopy, 'queryExpression'] := SqlUce.Content.Text;

  Grt.DictString[ObjectCopy, 'comment'] := CommentMemo.Text;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.DoSqlUceChange(Sender: TObject; Line: TUCELine);

begin
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.DataChange(Sender: TObject);

begin
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.ApplyChangesAndCloseActionExecute(
  Sender: TObject);

begin
  ApplyChanges;
  Close;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.ApplyChangesActionExecute(Sender: TObject);

begin
  ApplyChanges;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.ApplyChangesActionUpdate(Sender: TObject);

begin
  if (Sender is TTntAction) then
    TTntAction(Sender).Enabled := Modified;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.DiscardChangesActionExecute(Sender: TObject);

begin
  DiscardChanges;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.DockEditor;

begin
  PrepareEditorDock(ViewPageControl);

  FEditorTabFrame.ApplyAction := ApplyChangesAction;
  FEditorTabFrame.DiscardAction := DiscardChangesAction;

  ViewSheetBgShape.Visible := True;
  CommentsSheetBgShape.Visible := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.CloseActionExecute(Sender: TObject);

begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TMyxViewEditorForm.FocusFirstControl;

begin
  if (FEditorTabFrame <> nil) then
  begin
    if (FEditorTabFrame.SelectedTab = 0) then
      NameEd.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

end.
