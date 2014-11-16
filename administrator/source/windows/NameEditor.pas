unit NameEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls;

type
  TNameEditorForm = class(TForm)
    TntButton1: TTntButton;
    NameEd: TTntEdit;
    TntLabel1: TTntLabel;
    TntButton2: TTntButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NameEditorForm: TNameEditorForm;

implementation

{$R *.dfm}

end.
