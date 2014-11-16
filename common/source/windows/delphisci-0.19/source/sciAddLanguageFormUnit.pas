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
	Software Foundatifon, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
 Author : hdalis (hdalis@users.sourceforge.net)
 Created: 24/10/2004, 07:55:57
     $Id: sciAddLanguageFormUnit.pas,v 1.2 2004/11/13 04:29:51 hdalis Exp $
 History: 04/10/2004 Initial Release
}
{$Include SciCommonDef.Inc}
unit sciAddLanguageFormUnit;
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TsciAddLanguageForm = class(TForm)
    Label1: TLabel;
    languagename: TEdit;
    Label2: TLabel;
    LexerToUseCB: TComboBox;
    okButton: TBitBtn;
    cancelButton: TBitBtn;
    procedure languagenameChange(Sender: TObject);
  private
  public
  end;

var
  sciAddLanguageForm: TsciAddLanguageForm;

implementation
uses EdOptionsWin;
{$R *.dfm}




procedure TsciAddLanguageForm.languagenameChange(Sender: TObject);
begin
  if (Length(languagename.Text)=0) or (LexerToUseCB.ItemIndex<0) then okButton.Enabled:=false
  else okButton.Enabled:=True;
end;

end.
