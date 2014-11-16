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
   Author: hdalis (hdalis@users.sourceforge.net)
      $Id: SciLexerReg.pas,v 1.5 2004/11/13 04:29:50 hdalis Exp $
  Purpose: Register components and component editors.
  History: 29/09/2004 Initial Release
           17/10/2004 Added property editor for the Lexer property of
                      TSciLangItem
}

{$Include SciCommonDef.Inc}
unit SciLexerReg;
interface
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

procedure Register;
implementation
Uses
{$Ifdef COMPILER5}DsgnIntf,{$Else}DesignIntf, DesignEditors, {$Endif}
  SciSupport,SciLexerMemo,SciAutoComplete,SciCallTips,EdOptionsWin, SciLexerOptionsDlg, SciSearchReplace,
	SciKeyBindings,SciLexerMod,ScintillaLanguageManager,scilangfiller,SciPropertyMgr,SciDetectUtils
  ,SciDocuments,SciMacroRecording,SciResLangDcl,SciAbbrevationManager,tcFontComboBox,SciLexer,
  SciPrint
  ;

{$R scipropmgr.dcr}
{$R tcFontCombobox.dcr}
{$R scidocuments.dcr}
{$R scilexer.dcr}
Type
	TScintillaComponentEditor = class(TDefaultEditor)
	public
		procedure ExecuteVerb(Index: Integer); override;
		function GetVerb(Index: Integer): string; override;
		function GetVerbCount: Integer; override;
    procedure Edit;override;
	end;

	TSciKeyCommandsPropertyEditor = class(TPropertyEditor)
	public
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
	end;

  TSciLexerPropertyEditor=class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings);
    procedure GetValues(Proc: TGetStrProc);override;
  end;

  TSciSelectedLanguagePropertyEditor=class(TStringProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings);
    procedure GetValues(Proc: TGetStrProc);override;
  end;

  TSciHighlighterPropertyEditor=class(TClassProperty)
	public
		function GetAttributes: TPropertyAttributes; override;
    procedure Edit;override;
  end;


procedure Register;
begin
	RegisterComponents( 'Scintilla', [ TScintilla,TScintillaMemo, TSciAutoComplete,TSciCallTips,
  {$Ifndef NOMACRORECORD}
  TSciMacroRecorder,
  {$Endif}
  TScintillaOptionsDlg, TSciSearchReplace,TSciDocumentTabControl,TSciPropertyLoader,TSciAbbrevManager,TSciPrinter]);
  RegisterClass(TSciLanguageManager);
  RegisterPropertyEditor(TypeInfo(TSciLanguageManager), TScintilla, 'LanguageManager', TSciHighlighterPropertyEditor);
	RegisterComponentEditor(TScintilla, TScintillaComponentEditor);
	RegisterPropertyEditor(TypeInfo(TSciKeyCommandCollection), TScintilla, '', TSciKeyCommandsPropertyEditor);
 	RegisterPropertyEditor(TypeInfo(String), TSciLangItem, 'Lexer', TSciLexerPropertyEditor);
 	RegisterPropertyEditor(TypeInfo(String), TSciLanguageManager, 'SelectedLanguage', TSciSelectedLanguagePropertyEditor);
{$Ifndef COMPILER5}
  RegisterPropertiesInCategory('Visual',TScintillaMemo,['Gutter0','Gutter1','Gutter2','EdgeColor','EdgeMode','EdgeColumn','Indentation','IndentWidth','MarginLeft','MarginRight','MarkerType','WordWrapVisualFlags','WordWrapVisualFlagsLocation','EOLStyle','KeyCommands','Colors','ActiveHotSpot','HideSelect','Caret']);
  RegisterPropertiesInCategory('Visual',TScintilla,['LanguageManager','BraceHilite','CodeFolding']);
  RegisterPropertiesInCategory('Input',TScintillaMemo,['UseTabs','UseUnicode','WordChars','WordWrap','ClearUndoAfterSave','Lines','TabWidth','MouseDwellTime']);
{$Endif}
  RegisterComponents('Additional', [TtcFontComboBox]);


end;

{ TScintillaComponentEditor }
procedure TScintillaComponentEditor.Edit;
begin
  with TEdOptionsWindow.Create(nil) do
  begin
    Editor := Component as TScintilla;
    ShowModal;
    Free;
    if Designer<>nil then  Designer.Modified;
  end;

end;

procedure TScintillaComponentEditor.ExecuteVerb(Index: Integer);
var
odia : TOpenDialog;
sdia : TSaveDialog;
PropLdr : TSciPropertyLoader;
begin
	case Index of
		0 : Edit;
		1:;
		2 : begin
					with Component as TScintilla do
					begin
            if Color=clWindow then Color:=clBlack;
            if Font.Color=clWindowText then Font.Color:=clSilver;
            if Caret.ForeColor=clBlack then Caret.ForeColor:=clRed;
						FillLanguages(LanguageManager.LanguageList,false);
						LanguageManager.Update;
            Designer.Modified;
						//if Designer<>nil then Designer.Modified;
					end;
				end;
		3 : begin
					with Component as TScintilla do
					begin
            if Color=clWindow then Color:=clBlack;
            if Font.Color=clWindowText then Font.Color:=clSilver;
            if Caret.ForeColor=clBlack then Caret.ForeColor:=clRed;

						FillLanguages(LanguageManager.LanguageList,true);
            LanguageManager.SelectedLanguage:='null';
						Designer.Modified;
						MessageBox(Handle,PChar(scithelanguagelistofcontrol+Name+sciiscleared),PChar(sciinformationheader),MB_OK);
					end;
				end;
		4:;
		5: begin
					with Component as TScintilla do
					begin
						odia:=TOpenDialog.Create(nil);
						if odia<>nil then
						begin
							odia.Filter:=scipropertyfilesfilter;
							odia.DefaultExt:=scipropertyfiledefaultextension;
							odia.Title:=scipropertyfileloadtitle;
							odia.Options:=[ofPathMustExist,ofFileMustExist,ofEnableSizing];
							if (odia.Execute) then
							begin
                propldr:=TSciPropertyLoader.Create(nil);
                propldr.Editor:=TScintilla(Component);
                propldr.FileName:=odia.FileName;
                propldr.Load;
                propldr.Free;
                LanguageManager.Update;
							end;
							odia.Free;
						end;
					end;
			 end;
		6: begin
					with Component as TScintilla do
					begin
						sdia:=TSaveDialog.Create(nil);
						if sdia<>nil then
						begin
							sdia.Filter:=scipropertyfilesfilter;
							sdia.DefaultExt:=scipropertyfiledefaultextension;
							sdia.Title:=scipropertyfilesavetitle;
							sdia.Options:=[ofEnableSizing];
							if (sdia.Execute) then
							begin
                propldr:=TSciPropertyLoader.Create(nil);
                propldr.Editor:=TScintilla(Component);
                propldr.FileName:=sdia.FileName;
                propldr.Save;
                propldr.Free;
							end;
							sdia.Free;
						end;
					end;
			 end;
    else
      inherited ExecuteVerb(Index);

  end;
end;

function TScintillaComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result :=ssciOptions;
		1 : Result :='-';
		2 : Result :=ssciSelectPredefinedLanguage;
		3 : Result :=ssciClearLanguageList;
    4:  Result :='-';
    5:  Result := ssciLoadStylesFromPropertyFile;
    6:  Result := ssciSaveStylesToPropertyFile;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

function TScintillaComponentEditor.GetVerbCount: Integer;
begin
  Result := 7;
end;

{ TSciKeyCommandsPropertyEditor }


procedure TSciKeyCommandsPropertyEditor.Edit;
begin
  with TScintillaOptionsDlg.Create(nil) do begin
    Editor := GetComponent(0) as TScintilla;
    if Assigned(Editor) then
    begin
      Pages := [opKeyboard];
      ActivePage := opKeyboard;
      Execute;
    end;
    Free;
  end;
end;

function TSciKeyCommandsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TSciKeyCommandsPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [sscikeycommands]);
end;

{ TSciLexerPropertyEditor }

function TSciLexerPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;
procedure TSciLexerPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TSciLexerPropertyEditor.GetValueList(List: TStrings);
var
tmp : TSciLanguageManager;
langitm : TSciLangItem;
begin
  tmp:=nil;
  langitm :=GetComponent(0) as TSciLangItem; //Get the first selected TSciLangItem, no need to walk them since they're all identical.
  if langitm<>nil then
  begin
    if (langitm.Collection<>nil) and (langitm.Collection is TSciLangList) then // Test if the parent is a TSciLangList, and it isn't nil.
    begin
      if (langitm.Collection.Owner<>nil) and (langitm.Collection.Owner is TSciLanguageManager) then // Test if the parent is a TSciLanguageManager, and it isn't nil.
      begin
        tmp:=TSciLanguageManager(langitm.Collection.Owner);
      end;
    end;
  end;
  if tmp=nil then // Test if a TSciLanguageManager belonging to the current component was found, and if not, create a temporary object.
  begin
    tmp:=TSciLanguageManager.Create(nil);
    tmp.FillListWithLanguages(List,[sciLexers]);
    List.Add(scicontainerconst);
    tmp.Free;
  end else
  begin
    tmp.FillListWithLanguages(List,[sciLexers]);
    List.Add(scicontainerconst);
  end;
end;

{ TSciSelectedLanguagePropertyEditor }


function TSciSelectedLanguagePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;
procedure TSciSelectedLanguagePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TSciSelectedLanguagePropertyEditor.GetValueList(List: TStrings);
var
tmp : TSciLanguageManager;
begin
  tmp :=GetComponent(0) as TSciLanguageManager;
  tmp.FillListWithLanguages(List,[sciLanguages]);
  if (List.IndexOf(scicontainerconst)=-1) then
  begin
    List.Add(scicontainerconst);
  end;
end;

function TSciHighlighterPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog,paSubProperties];
end;
procedure TSciHighlighterPropertyEditor.Edit;
var
  Component : TScintilla;
begin
  Component:=GetComponent(0) as TScintilla;
  with TEdOptionsWindow.Create(nil) do
  begin
    Editor := Component;
    ShowModal;
    Free;
    if Designer<>nil then Designer.Modified;
  end;

end;
end.
