//CE_Desc_Include(helpdescriptions.txt)
{$Include SciCommonDef.Inc}
{ 
 Unit    : SciLexerOptionsDlg
 Purpose : Component encapsulating the Options Dialog for Scintilla editors
 Created : 20/03/2003
      $Id: SciLexerOptionsDlg.pas,v 1.5 2004/11/13 04:29:50 hdalis Exp $
 Author  : Kiriakos Vlahos (kvlahos@london.edu)

 History:
 05/12/2003 Customized and extended by hdalis for use with the
            TScintilla and TScintillaAuto components.
            Added Options2Page
            (hdalis@users.sourceforge.net)
 29/09/2004 Initial Release with Delphi Scintilla Interface Components
 13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
            generate the help.
            Renamed the Scintilla properties to Editor
            (hdalis@users.sourceforge.net)
 29/10/2004 Removed the opKeywords from the TSciOptionPage enum. No longer needed.
            Renamed the opOptions2 to opColors
}
unit SciLexerOptionsDlg;
interface
Uses
  SysUtils, Classes, Controls, Forms, SciLexerMod,EdOptionsWin;

Type
	TSciOptionPage = (opOptions, opColors,opHighlighter, opKeyboard);
  TSciOptionPages = set of TSciOptionPage;

  TScintillaOptionsDlg = class(TComponent)
  private
    FEditor: TScintilla;
    FActivePage: TSciOptionPage;
    FPages: TSciOptionPages;
  protected
    procedure Notification(AComponent: TComponent;
              Operation: TOperation);  override;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property Editor: TScintilla read FEditor write FEditor;
    property ActivePage: TSciOptionPage read FActivePage write FActivePage default opOptions;
    property Pages: TSciOptionPages read FPages write FPages default [opOptions, opHighlighter, opKeyboard];
  end;


implementation
uses sciResLang;

{ TScintillaOptionsDlg }

constructor TScintillaOptionsDlg.Create(AOwner: TComponent);
begin
  inherited;
  FPages := [opOptions, opHighlighter, opKeyboard,opColors];
  FActivePage := opOptions;
end;

function TScintillaOptionsDlg.Execute: Boolean;
begin
  if FEditor = nil then
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,['TScintillaOptionsDlg']);
  with TEdOptionsWindow.Create(Self) do begin
    if not (FActivePage in FPages) then Include(FPages, FActivePage);
    ColorsPage.TabVisible := opColors in FPages;
    HighlighterPage.TabVisible := opHighlighter in FPages;
    OptionsPage.TabVisible := opOptions in FPages;
		KeyCommandsPage.TabVisible := opKeyboard in FPages;
		case FActivePage of
			opOptions : OptionPages.ActivePage := OptionsPage;
			opHighlighter : OptionPages.ActivePage := HighlighterPage;
			opKeyboard : OptionPages.ActivePage := KeyCommandsPage;
			opColors : OptionPages.ActivePage := colorsPage;
    end;
    Editor := FEditor;
    Result := ShowModal = mrOK;
    Free;
  end;
end;

procedure TScintillaOptionsDlg.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if (AComponent = FEditor) and (Operation = opRemove) then FEditor := nil;
end;

end.
