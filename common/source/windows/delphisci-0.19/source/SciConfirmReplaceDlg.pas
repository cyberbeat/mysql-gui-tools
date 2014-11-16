//CE_Desc_Include(helpdescriptions.txt)
{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: dlgConfirmReplaceText.pas, released 2000-06-23.

The Original Code is part of the SearchReplaceDemo project, written by
Michael Hieke for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SciConfirmReplaceDlg.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
	History:   29/09/2004 Initial Release with Delphi Scintilla Interface Components
                        Removed Dialogs from the uses list.
             24/11/2004 Corrected a little bug. When All was selected, it didn't return
                        the mrYesToAll that was expected.. Now it does..
}
{$Include SciCommonDef.Inc}
unit SciConfirmReplaceDlg;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Buttons;

type
  TConfirmReplaceDialog = class(TForm)
    lblConfirmation: TLabel;
    Image1: TImage;
    btnReplace: TBitBtn;
    btnSkip: TBitBtn;
    btnCancel: TBitBtn;
    btnReplaceAll: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure PrepareShow(editorRect: TRect; X, Y1, Y2: Integer;AReplaceText: string);
  end;

var
  ConfirmReplaceDialog: TConfirmReplaceDialog;

implementation

{$R *.DFM}
uses SciResLang;

{ TConfirmReplaceDialog }

procedure TConfirmReplaceDialog.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TConfirmReplaceDialog.FormDestroy(Sender: TObject);
begin
  ConfirmReplaceDialog := nil;
end;

procedure TConfirmReplaceDialog.PrepareShow(editorRect: TRect;
  X, Y1, Y2: Integer; AReplaceText: string);
var
  nW, nH: Integer;
begin
  lblConfirmation.Caption := Format(SAskReplaceText, [AReplaceText]);
  nW := editorRect.Right - editorRect.Left;
  nH := editorRect.Bottom - editorRect.Top;

  if nW <= Width then
    X := editorRect.Left - (Width - nW) div 2
  else begin
    if X + Width > editorRect.Right then
      X := editorRect.Right - Width;
  end;
  if Y2 > editorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

end.




