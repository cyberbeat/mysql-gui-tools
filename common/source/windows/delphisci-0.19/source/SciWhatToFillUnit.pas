//CE_Desc_Include(helpdescriptions.txt)
{
	Delphi Scintilla Interface Components
	Copyright (C) 2004,2005 Jan Martin Pettersen (hdalis)

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later
	version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
  Author : hdalis
	Created: 19/11/2004, 01:14:14
  History: 19/11/2004 Initial Release
	         30/05/2005 Added some changes.
			$Id: SciWhatToFillUnit.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $
}
{$Include SciCommonDef.Inc}
unit SciWhatToFillUnit;
interface
uses
  Forms,StdCtrls, Buttons, Classes, Controls, ExtCtrls, ComCtrls;

type
  TTSciWhatToFillForm = class(TForm)
    checkingPanel: TPanel;
    c0: TCheckBox;
    c1: TCheckBox;
    c2: TCheckBox;
    c3: TCheckBox;
    c4: TCheckBox;
    c5: TCheckBox;
    c6: TCheckBox;
    c7: TCheckBox;
    c8: TCheckBox;
    c9: TCheckBox;
    c10: TCheckBox;
    c11: TCheckBox;
    c12: TCheckBox;
    c13: TCheckBox;
    c14: TCheckBox;
    c15: TCheckBox;
    c16: TCheckBox;
    c17: TCheckBox;
    c18: TCheckBox;
    c19: TCheckBox;
    c20: TCheckBox;
    c21: TCheckBox;
    c22: TCheckBox;
    c23: TCheckBox;
    c24: TCheckBox;
    c25: TCheckBox;
    c26: TCheckBox;
    c27: TCheckBox;
    c28: TCheckBox;
    c29: TCheckBox;
    c31: TCheckBox;
    c32: TCheckBox;
    c33: TCheckBox;
    c34: TCheckBox;
    c35: TCheckBox;
    c30: TCheckBox;
    buttonPanel: TPanel;
    okButton: TBitBtn;
    cancelButton: TBitBtn;
    selectAllButton: TBitBtn;
    selectNoneButton: TBitBtn;
    c36: TCheckBox;
    c37: TCheckBox;
    c38: TCheckBox;
    c39: TCheckBox;
    c41: TCheckBox;
    c40: TCheckBox;
    c42: TCheckBox;
    c43: TCheckBox;
    c45: TCheckBox;
    c44: TCheckBox;
    c46: TCheckBox;
    c47: TCheckBox;
    c49: TCheckBox;
    c48: TCheckBox;
    c50: TCheckBox;
    c51: TCheckBox;
    c53: TCheckBox;
    c52: TCheckBox;
    c54: TCheckBox;
    c55: TCheckBox;
    c56: TCheckBox;
    copyrightlabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure selectAllButtonClick(Sender: TObject);
    procedure selectNoneButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TSciWhatToFillForm: TTSciWhatToFillForm;

implementation

{$R *.dfm}
uses SciLexer;
procedure TTSciWhatToFillForm.selectAllButtonClick(Sender: TObject);
var
cnt : Integer;
i : Integer;
begin
  cnt:=checkingPanel.ControlCount;
  for i:=0 to (cnt-1) do
  begin
    if checkingPanel.Controls[i] is TCheckBox then
    with checkingPanel.Controls[i] as TCheckBox do
    begin
      Checked:=True;
    end;
  end;
end;

procedure TTSciWhatToFillForm.selectNoneButtonClick(Sender: TObject);
var
cnt : Integer;
i : Integer;
begin
  cnt:=checkingPanel.ControlCount;
  for i:=0 to (cnt-1) do
  begin
    if checkingPanel.Controls[i] is TCheckBox then
    with checkingPanel.Controls[i] as TCheckBox do
    begin
      Checked:=False;
    end;
  end;
end;

procedure TTSciWhatToFillForm.FormCreate(Sender: TObject);
begin
  copyrightlabel.Caption:=GetDelphiSciVersionStr;
end;

end.
