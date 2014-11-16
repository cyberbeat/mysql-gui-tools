unit MyxSchemaEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls, ComCtrls,
  TntComCtrls, MyxEditor, MyxDbUtils;

type
  TMyxSchemaEditorForm = class(TMyxEditorForm)
    HeaderPnl: TTntPanel;
    NameLbl: TTntLabel;
    NameEd: TTntEdit;
    LeftPnl: TTntPanel;
    RightPnl: TTntPanel;
    BottomPnl: TTntPanel;
    ApplyChangesBtn: TTntButton;
    CancelBtn: TTntButton;
    TntLabel1: TTntLabel;
    CollationComboBox: TTntComboBox;
    PageControl: TTntPageControl;
    CommentTabSheet: TTntTabSheet;
    CommentMemo: TTntMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DataChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetModified(Modified: Boolean); override;
  public
    { Public declarations }
    procedure StartEditObject(Obj: Pointer); override;
  end;

var
  MyxSchemaEditorForm: TMyxSchemaEditorForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.FormCreate(Sender: TObject);

begin
  PageControl.ActivePageIndex := 0;

  UpdateCollations(CollationComboBox);
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.FormDestroy(Sender: TObject);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);

begin
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.StartEditObject(Obj: Pointer);

begin
  inherited;

  NameEd.Text := Grt.DictString[Obj, 'name'];

  CommentMemo.Text := Grt.DictString[Obj, 'comment'];

  SetCollations(CollationComboBox,
    Grt.DictString[Obj, 'defaultCollationName']);
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.SetModified(Modified: Boolean);

begin
  inherited;

  ApplyChangesBtn.Enabled := Modified;
end;


// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.ApplyChangesBtnClick(Sender: TObject);

begin
  if (NameEd.Text <> '') then
    Grt.DictString[ObjectCopy, 'name'] := NameEd.Text;

  Grt.DictString[ObjectCopy, 'comment'] := CommentMemo.Text;


  if (Copy(CollationComboBox.Text, 1, 4) = '____') then
    CollationComboBox.ItemIndex := 0;

  SetCharsetCollation(CollationComboBox.Text, ObjectCopy,
    'defaultCharacterSetName', 'defaultCollationName');

  ApplyChanges;
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.CancelBtnClick(Sender: TObject);

begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TMyxSchemaEditorForm.DataChange(Sender: TObject);

begin
  Modified := True;
end;

// -----------------------------------------------------------------------------

end.
