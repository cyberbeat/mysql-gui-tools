//CE_Desc_Include(helpdescriptions.txt)
{
 Adopted from the DelphiWrapper at http://www.pnotepad.com/scintilla
		 $Id: SciKeyEditForm.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $
 History:   29/09/2004 Initial Release with Delphi Scintilla Interface Components
            13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                       generate the help.
}
{$Include SciCommonDef.Inc}
unit SciKeyEditForm;
interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls, ExtCtrls, Buttons;

type
  TSciAssignKeyEvent = procedure (Sender: TObject;Key : TShortCut;var Allow : Boolean) of object;

  TKeyEditForm = class(TForm)
    pnlAlign: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    cmbCommand: TComboBox;
    HotKey: THotKey;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure HotKeyChange(Sender: TObject);
  private
    FOnTryAssign : TSciAssignKeyEvent;
    FInitialKey : TShortCut;

    { Private declarations }
    procedure SetInitialKey(Value : TShortCut);
  public
    { Public declarations }
    property InitialKey : TShortCut read FInitialKey write SetInitialKey;
    property OnTryAssign : TSciAssignKeyEvent read FOnTryAssign write FOnTryAssign;
  end;

implementation
{$R *.dfm}

Uses
  SciKeyBindings;

procedure TKeyEditForm.SetInitialKey(Value : TShortCut);
begin
  FInitialKey:=Value;
  HotKey.HotKey:=FInitialKey;
end;
procedure TKeyEditForm.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  cmbCommand.Clear;
  cmbCommand.Items.BeginUpdate;
  try
    for i := 0 to High(Sci_KeyboardCommandMap) do
      cmbCommand.AddItem(Sci_KeyboardCommandMap[i].Name, TObject(Sci_KeyboardCommandMap[i].Value));
    cmbCommand.ItemIndex := -1;
  finally
    cmbCommand.Items.EndUpdate;
  end;
end;

procedure TKeyEditForm.HotKeyChange(Sender: TObject);
var
  Allow : Boolean;
begin
  if (HotKey.HotKey<>FInitialKey) then
  begin
    Allow:=True;
    if assigned(FOnTryAssign) then
    begin
      FOnTryAssign(Self,HotKey.HotKey,Allow);
    end;
    if Allow=False then
      HotKey.HotKey:=FInitialKey;
  end;
end;

end.
