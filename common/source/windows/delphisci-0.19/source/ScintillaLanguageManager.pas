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
	Software Foundatifon, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{$Include SciCommonDef.Inc}
unit ScintillaLanguageManager;
{ 
 Unit    : ScintillaLanguageManager
 Purpose : Manages defined languages and lexers..
 Created : 05/03/2001
 Original Author  : Kiriakos Vlahos (kvlahos@london.edu)
 Author  : Jan Martin Pettersen (hdalis)
      $Id: ScintillaLanguageManager.pas,v 1.5 2004/11/13 04:29:51 hdalis Exp $
 History : 15/03/2001 Lexer sturtures and encapsulation of some common Lexers

           29/09/2004 Initial Release with Delphi Scintilla Interface Components
                      Added additional encapsulation, the possibility to
                      define your own languages based on the existing lexers.
                      No longer enumerated lexer type, changed to strings
                      Added the possibility to define languages based on
                      known lexers. Multiple languages can use the same
                      lexer. ie. resourcedefinition use the same lexer as the
                      c++ language.
                      Added function to dynamically add languages to the
                      Optionsdialog etc..
                      (hdalis@users.sourceforge.net)
           05/10/2004 Removed the typedef for TSciLangName (no longer needed)
                      Renamed the SetLLanguage procedure to SetLanguageStr
           13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                      generate the help.
                      (hdalis@users.sourceforge.net)
           27/10/2004 Renamed the TSciLanguageManager.LanguageStr to SelectedLanguage,
                      and the SetLanguageStr to SetSelectedLanguage.
                      Renamed TSciLangList.GetStyleListStr to GetStyleList.
           30/10/2004 Added Comment* properties to TSciLangItem, used by
                      CommentBox/CommentBlock functions in TScintilla.
           09/01/2005 Added the property ExtraData to TSciLangItem and TSciLanguageManager
                      to allow storing of more data with each language..
                      Added AddLexer,HasLexer,RemoveLexer to TSciLanguageManager

                      Those above was added to support adding an Automation interface for
                      each of the languages for example.. as used in the MyEditor demo..
                      or it may be called an editor now I think.. with internal scripting,
                      fully supporting automation etc..

                      Added CharSet property to TSciStyle.
                      (hdalis@users.sourceforge.net)
           14/02/2005 Changed the Comment* properties in TSciLangItem,
                      to be more corrent..
                      CommentOneliner = CommentBlock, CommentBox* = Comment*
                      Two new properties.. CommentStreamStart, CommentStreamEnd.
                      Added AssignmentOperator and EndOfStatementOperator to same..
                      Added a include for MoreLexers.inc to make it easy for developers
                      to add lexers, and to maintain it without sifting through all the
                      sourcecode.
                      (hdalis@users.sourceforge.net)
           10/03/2005 Removed php and asp from the lexerlist at it is deprecated (temporarily?)..
                      Use hypertext lexer instead.
           12/04/2005 Added TSciStyleList.HasStyle and TSciKeyWordsList.HasList
					 30/05/2005 Added the SameStyle functions, and more.
 Bugfixes:
           15/10/2004 Fixed a bug in GetStyleListStr. Didn't set the LanguageStr when
                      nullitm didn't exits, and it was created. Caused
                      TScintillaOptionsDlg to throw an exception with list error
                      when you tried to add a style and no nullstyles existed.
                      (hdalis@users.sourceforge.net)

}
 interface
uses SysUtils, Classes, Graphics,SciLexerMemo;

Type
{$Ifndef NOLANGCHANGEDEVENT}
	TSCEvent_languagechanged = procedure(Sender : TObject; newlang : String) of object;
{$Endif}

TSciCase = (CASE_MIXED,CASE_UPPER,CASE_LOWER);
TSciLangKeywords = Record
	Name : string;
	KeywordListNumber : Integer;
	KeyWords : string;
end;

TSciLangStyle = Record
	StyleNumber : Integer;
	Name : string;
	FontName : string;
	FontSize : Integer;
	FontStyles : TFontStyles;
	ForeColor : TColor;
	BackColor : TColor;
	CharCase : TSciCase;
	Visible: Boolean;
	Changeable : Boolean;
	Hotspot : Boolean;
	EOLFilled : Boolean;
  CharSet : Integer;
end;

TSciKeywords = class(TCollectionItem)
private
	fKeyWords : TStrings;
	fName : string;
	fKeywordListNumber : Integer;
	procedure SetKeywords(const Value: TStrings);
	procedure SetName(const Value: string);
	procedure SetKeywordListNumber(const Value: Integer);
public
	constructor Create(Collection: TCollection); override;
	destructor Destroy; override;
	procedure Assign(Source: TPersistent); override;
	procedure AssignRec(Rec : TSciLangKeywords);
	procedure AssignToRec(var Rec : TSciLangKeywords);
published
	property KeywordListNumber : Integer read FKeywordListNumber write SetKeywordListNumber;
	property Name : string read fName write SetName;
	property Keywords : TStrings read fKeywords write SetKeywords;
end;


TSciStyle = class(TCollectionItem)
private
	fStyleNumber : Integer;
	fName : string;
	fFontName : string;
	fFontSize : Integer;
	fFontStyles : TFontStyles;
	fForeColor : TColor;
	fBackColor : TColor;
	fCharCase : TSciCase;
	fVisible: Boolean;
	fChangeable: Boolean;
	fHotspot : boolean;
	fEOLFilled: Boolean;
  fCharSet : Integer;
	procedure SetBackColor(const Value: TColor);
	procedure SetFontName(const Value: string);
	procedure SetFontSize(const Value: Integer);
	procedure SetFontStyles(const Value: TFontStyles);
	procedure SetForeColor(const Value: TColor);
	procedure SetName(const Value: string);
	procedure SetStyleNumber(const Value: Integer);
	procedure SetCharCase(const Value: TSciCase);
	procedure SetVisible(const Value: Boolean);
	procedure SetChangeable(const Value: Boolean);
	procedure SetEOLFilled(const Value: Boolean);
	procedure SetHotspot(const Value: Boolean);
  procedure SetCharSet(const Value : Integer);
public
	constructor Create(Collection: TCollection); override;
	procedure Assign(Source: TPersistent); override;
	procedure AssignRec(Rec : TSciLangStyle);
	procedure AssignToRec(var Rec : TSciLangStyle);
published
	property FontName : string read fFontName write SetFontName;
	property FontSize : Integer read fFontSize write SetFontSize;
	property FontStyles : TFontStyles read FFontStyles write SetFontStyles;
	property ForeColor : TColor read fForeColor write SetForeColor default clDefault;
	property BackColor : TColor read fBackColor write SetBackColor default clDefault;
	property CharCase : TSciCase read fCharCase write SetCharCase;
	property Visible : Boolean read fVisible write SetVisible default true;
	property Changeable : Boolean read fChangeable write SetChangeable default true;
	property EOLFilled : Boolean read fEOLFilled write SetEOLFilled default false;
	property Hotspot : Boolean read fHotspot write SetHotspot default false;
	property Name : string read fName write SetName;
	property StyleNumber : Integer read fStyleNumber write SetStyleNumber;
  property CharSet : Integer read fCharSet write SetCharSet default -1;
end;

TSciKeyWordsList = class(TOwnedCollection)
private
	fEditor : TPersistent; //TScintilla
	procedure SetEditor(Editor : TPersistent);
public
	procedure Update(Item: TCollectionItem); override;
  function HasList(const KeywordListNumber : LongInt) : Boolean;
  function GetKeywordList(const KeywordListNumber : LongInt) : TSciKeywords;
end;

TSciStyleList = class(TOwnedCollection)
private
	fEditor : TPersistent; //TScintilla
	procedure SetEditor(Editor : TPersistent);
public
  function HasStyle(const StyleNumber : LongInt) : Boolean;
  function GetStyle(const StyleNumber : LongInt) : TSciStyle;
	procedure Update(Item: TCollectionItem); override;
end;

TSciLangItem = class(TCollectionItem)
private
	fLexer : String;
	fStyles : TSciStyleList;
	fKeyWords : TSciKeyWordsList;
	fLanguageName : String;
	fEditor : TPersistent; //TScintilla
  fExtraData : Pointer;
  fAssignmentOperator : String;
  fEndOfStatementOperator : String;
  FCommentBoxStart,FCommentBoxMiddle,FCommentBoxEnd,FCommentBlock,
  FCommentStreamStart,FCommentStreamEnd : String;
  FCommentAtLineStart : Boolean;
  fNumStyleBits : Integer;

	procedure SetLexer(const Value: String);
	procedure SetName(const Value: string);
	procedure SetKeywordsList(const Value: TSciKeyWordsList);
	procedure SetStyleList(const Value: TSciStyleList);
  procedure SetNumStyleBits(const Value : Integer);

	procedure SetEditor(Value : TPersistent);
	procedure Update;
public
	constructor Create(Collection: TCollection); override;
	destructor Destroy; override;
	procedure Assign(Source: TPersistent); override;

  property ExtraData : Pointer read fExtraData write fExtraData;
published
	// The Language name
	property Name : string read fLanguageName write SetName;
	// The lexer to use with this language.
	property Lexer : String read fLexer write SetLexer;

	property Styles : TSciStyleList read fStyles write SetStyleList;
	property Keywords : TSciKeyWordsList read fKeyWords write SetKeywordsList;
  property AssignmentOperator : String read fAssignmentOperator write fAssignmentOperator;
  property EndOfStatementOperator : String read FEndOfStatementOperator write FEndOfStatementOperator;

  property CommentBoxStart : String read FCommentBoxStart write FCommentBoxStart;
  property CommentBoxEnd : String read FCommentBoxEnd write FCommentBoxEnd;
  property CommentBoxMiddle : String read FCommentBoxMiddle write FCommentBoxMiddle;
  property CommentBlock : String read FCommentBlock write FCommentBlock;
  property CommentAtLineStart : Boolean read FCommentAtLineStart write FCommentAtLineStart;
  property CommentStreamStart : String read FCommentStreamStart write FCommentStreamStart;
  property CommentStreamEnd : String read FCommentStreamEnd write FCommentStreamEnd;
  property NumStyleBits : Integer read fNumStyleBits write SetNumStyleBits;

end;


TSciLangList = class(TOwnedCollection)
  private
    fEditor : TPersistent; //TScintilla
    procedure SetEditor(Editor : TPersistent);

  public
		procedure Select(const fLanguage : String);
		function  Find(const fLanguage : String) : TSciLangItem;
		function  GetStyleList(const fLanguage : String;const fLexerName : String='') : TSciLangItem;
	end;

	TSCEvent_lexupdate = procedure(Sender : TObject; Editor : TPersistent;lang : String;Item : TSciLangItem) of object;
  sciLexerSelectType=(sciLexers,sciLanguages);
  sciLexerSelectTypes=set of sciLexerSelectType;

	TSciLanguageManager = class (TPersistent)
	private
		fEditor : TPersistent; //TScintilla
		fSelectedLanguage : String;
		fLanguages : TSciLangList;
		flexerlist : TStrings;
    fExtraData : Pointer;
    fDisabled : Boolean;
	{$Ifndef NOLANGCHANGEDEVENT}
		fOnLanguageChanged : TSCEvent_languagechanged;
	{$Endif}
		fOnLexUpdate : TSCEvent_lexupdate;

		procedure SetLanguageList(const Value : TSciLangList);
		procedure SetSelectedLanguage(const Value : String);
		{Retrieve the lexer corresponding to the 'Lang'}
		function  LangToLex(const Lang : String) : String;
   	procedure FillLexerNameMap;
    procedure SetDisabled(const Value : Boolean);
	protected
		function  GetOwner: TPersistent; override;
	public
		constructor Create(Editor: TPersistent);
		destructor  Destroy; override;
		procedure   Assign(Source: TPersistent); override;
    procedure   Update;

		procedure   FillListWithLanguages(lst : TStrings;const which : sciLexerSelectTypes=[sciLexers,sciLanguages]);
    procedure   AddLexer(const Lexer : String);
    function    HasLexer(const Lexer : String) : Boolean;
    procedure   RemoveLexer(const Lexer : String);

	  function    AddLanguage(const Lang,Lexer : String) : TSciLangItem;
	  function    DupLanguage(const Langtocopy : String;const langname : String='') : TSciLangItem;
	  function    RemoveLanguage(const Langtoremove : String;var notinstaticlist : Boolean) : Boolean;
    function    HasLanguage(const LanguageName : String) : Boolean;
    property    ExtraData : Pointer read fExtraData write fExtraData;
	published
		property    LanguageList : TSciLangList read fLanguages write SetLanguageList;
		property    SelectedLanguage : String read fSelectedLanguage write SetSelectedLanguage;
    property    Disabled : Boolean read fDisabled write SetDisabled default False;
	{$Ifndef NOLANGCHANGEDEVENT}
		property    OnLanguageChanged : TSCEvent_languagechanged read fOnLanguageChanged write fOnLanguageChanged;
	{$Endif}
		property    OnLexUpdate : TSCEvent_lexupdate read fOnLexUpdate write fOnLexUpdate;
	end;

  {Returns True if Style1 and Style2 is exactly equal, except Name and StyleNumber}
  function SameStyle(Style1,Style2 : TSciStyle) : Boolean; overload;
  function SameStyle(Style1,Style2 : TSciLangStyle) : Boolean; overload;
const
scicontainerconst='container';
cDefaultLexer='null';

implementation

Uses
	SciSupport,SciLexer,SciLexerMod,SciResLang,sciUtils,Windows;

procedure TSciLangList.SetEditor(Editor : TPersistent);
var
  i : Integer;
begin
  try
    BeginUpdate;
    self.fEditor:=Editor;
    for i := 0 to Count - 1 do
    begin
      TSciLangItem(Items[i]).SetEditor(Editor);
    end;
  finally
	  EndUpdate;
  end;
end;


function  TSciLangList.Find(const fLanguage : String) : TSciLangItem;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
	begin
    if AnsiCompareStr(TSciLangItem(Items[i]).Name,fLanguage)=0 then
		begin
			Result :=TSciLangItem(Items[i]);
			Exit;
		end;
  end;
  Result :=nil;
end;

procedure TSciLangList.Select(const fLanguage : String);
var
  itm : TSciLangItem;
begin
	if not assigned(fEditor) then Exit;
  itm:=Find(fLanguage);
  if itm<>nil then itm.Update;
end;


function  TSciLangList.GetStyleList(const fLanguage : String;const fLexerName : String) : TSciLangItem;
var
  tmp : TSciLangItem;
  nullitm : TSciLangItem;
begin
  tmp:=Find(fLanguage);
  if tmp<>nil then
  begin
    Result:=tmp;
    if (fLexerName<>'') and (tmp.Lexer<>fLexerName) then
    tmp.Lexer:=fLexerName;
    Exit;
  end;
	tmp:=TSciLangItem(Add);
  nullitm:=Find('null');
  if nullitm<>nil then
  begin
    try
      tmp.Styles.BeginUpdate;
      if fEditor<>nil then tmp.SetEditor(fEditor);
      tmp.Styles.Assign(nullitm.Styles);
      tmp.Name:=fLanguage;
      if fLexerName='' then
        tmp.Lexer:=fLanguage
      else
        tmp.Lexer:=fLexerName;
    finally
      tmp.Styles.EndUpdate;
    end;
  end else
  begin
    tmp.Name:='null';
    tmp.Lexer:='null';
    tmp:=GetStyleList(fLanguage);
  end;
	Result:=tmp;
end;

procedure TSciLangItem.SetNumStyleBits(const Value : Integer);
begin
  fNumStyleBits:=Value;
  Changed(false);
end;

procedure TSciLangItem.SetEditor(Value : TPersistent);
begin
	fEditor:=Value;
	Styles.SetEditor(fEditor);
	Keywords.SetEditor(fEditor);
end;

procedure TSciLangItem.SetName(const Value: string);
begin
	fLanguageName:=Value;
	Changed(false);
end;

procedure TSciLangItem.SetLexer(const Value: String);
begin
  if(fLexer<>Value) then
  begin
    //if fLexer=fLanguageName then
    //begin
    //  fLanguageName:=Value;
    //end;
    fLexer :=Value;
    Changed(false);
  end;
end;


procedure TSciLangItem.SetKeywordsList(const Value: TSciKeyWordsList);
begin
	fKeywords.Assign(Value);
	Changed(false);
end;

procedure TSciLangItem.SetStyleList(const Value: TSciStyleList);
begin
	fStyles.Assign(Value);
	Changed(false);
end;

procedure TSciLangItem.Update;
begin
	fKeyWords.Update(nil);
	fStyles.Update(nil);
	if fEditor<>nil then
  begin
    TScintilla(fEditor).Colourise(0, -1);
  end;

end;

constructor TSciLangItem.Create(Collection: TCollection);
begin
	inherited;
	fStyles:=TSciStyleList.Create(self,TSciStyle);
	fKeyWords:=TSciKeyWordsList.Create(self,TSciKeywords);
	fLanguageName := sLMLanguage+'['+IntToStr(Index)+']';
  fLexer :='null';
  fNumStyleBits:=5;
  fExtraData:=nil;
  
  fAssignmentOperator:='=';
  fEndOfStatementOperator:=';';
  FCommentBoxStart:='/*';
  FCommentBoxMiddle:='*';
  FCommentBoxEnd:='*/';
  FCommentBlock:='//';
  FCommentStreamStart:='/*';
  FCommentStreamEnd:='*/';
  FCommentAtLineStart:=True;
end;

destructor TSciLangItem.Destroy;
begin
	inherited;
	if assigned(fStyles) then
	begin
		fStyles.Free;
		fStyles:=nil;
	end;
	if assigned(fKeyWords) then
	begin
		fKeyWords.Free;
		fKeyWords:=nil;
	end;
end;

procedure TSciLangItem.Assign(Source: TPersistent);
begin
	if Source is TSciLangItem then
	begin
		fLanguageName := TSciLangItem(Source).Name;
    fStyles.Assign(TSciLangItem(Source).Styles);
    fKeywords.Assign(TSciLangItem(Source).Keywords);
    fLexer:=TSciLangItem(Source).Lexer;
    fExtraData:=TSciLangItem(Source).ExtraData;
    fAssignmentOperator:=TSciLangItem(Source).AssignmentOperator;
    fEndOfStatementOperator:=TSciLangItem(Source).EndOfStatementOperator;
    FCommentBoxStart:=TSciLangItem(Source).CommentBoxStart;
    FCommentBoxMiddle:=TSciLangItem(Source).CommentBoxMiddle;
    FCommentBoxEnd:=TSciLangItem(Source).CommentBoxEnd;
    FCommentBlock:=TSciLangItem(Source).CommentBlock;
    FCommentAtLineStart :=TSciLangItem(Source).CommentAtLineStart;
    FCommentStreamStart:=TSciLangItem(Source).CommentStreamStart;
    FCommentStreamEnd:=TSciLangItem(Source).CommentStreamEnd;
    fNumStyleBits:=TSciLangItem(Source).NumStyleBits;
  end else
    inherited;
end;

procedure TSciKeywords.Assign(Source: TPersistent);
begin
	if Source is TSciKeywords then
  begin
		fName := TSciKeywords(Source).Name;
    fKeyWordListNumber := TSciKeywords(Source).KeywordListNumber;
    fKeywords.Assign(TSciKeywords(Source).Keywords);
  end else
    inherited;
end;

procedure TSciKeywords.AssignToRec(var Rec : TSciLangKeywords);
begin
  Rec.Name :=fName;
  Rec.KeywordListNumber:=fKeywordListNumber;
  Rec.KeyWords :=fKeywords.Text;
end;

procedure TSciKeywords.AssignRec(Rec : TSciLangKeywords);
begin
  fName := Rec.Name;
  fKeywordListNumber := Rec.KeywordListNumber;
{$Ifdef COMPILER6_UP}
  fKeywords.Delimiter := ' ';
  fKeywords.DelimitedText := Rec.KeyWords;
{$Else}
  fKeywords.Text := Rec.KeyWords;
{$Endif}
end;

constructor TSciKeywords.Create(Collection: TCollection);
begin
  fKeywords := TStringList.Create;
  inherited;
  fName := sLMKeywords+'['+IntToStr(Index)+']';
end;

destructor TSciKeywords.Destroy;
begin
  fKeywords.Free;
  inherited;
end;

procedure TSciKeywords.SetKeywordListNumber(const Value: Integer);
begin
  FKeywordListNumber := Value;
  Changed(False);
end;

procedure TSciKeywords.SetKeywords(const Value: TStrings);
begin
  fKeyWords.Assign(Value);
  Changed(False);
end;

procedure TSciKeywords.SetName(const Value: string);
begin
  fName := Value;
  Changed(False);
end;

{ TSciStyle }

procedure TSciStyle.Assign(Source: TPersistent);
begin
  if Source is TSciStyle then
	begin
		fStyleNumber := TSciStyle(Source).StyleNumber;
		fName := TSciStyle(Source).Name;
		fFontName := TSciStyle(Source).FontName;
		fFontSize := TSciStyle(Source).FontSize;
		fFontStyles := TSciStyle(Source).FontStyles;
		fForeColor :=TSciStyle(Source).ForeColor;
		fBackColor := TSciStyle(Source).BackColor;
		fCharCase := TSciStyle(Source).CharCase;
		fVisible := TSciStyle(Source).Visible;
		fChangeable := TSciStyle(Source).Changeable;
		fHotspot := TSciStyle(Source).HotSpot;
		fEOLFilled := TSciStyle(Source).EOLFilled;
    FCharSet:= TSciStyle(Source).CharSet;
  end else
    inherited;
end;

procedure TSciStyle.SetCharSet(const Value : Integer);
begin
  fCharSet:=Value;
  Changed(False);
end;

procedure TSciStyle.AssignRec(Rec: TSciLangStyle);
begin
  fStyleNumber := Rec.StyleNumber;
  fName := Rec.Name;
  fFontName := Rec.FontName;
  fFontSize := Rec.FontSize;
  fFontStyles := Rec.FontStyles;
  fForeColor :=Rec.ForeColor;
  fBackColor := Rec.BackColor;
  fCharCase := Rec.CharCase;
  fVisible := Rec.Visible;
  fChangeable := Rec.Changeable;
  fHotspot := Rec.Hotspot;
  fEOLFilled := Rec.EOLFilled;
  FCharSet:=Rec.CharSet;
end;

procedure TSciStyle.AssignToRec(var Rec : TSciLangStyle);
begin
  Rec.StyleNumber:=fStyleNumber;
  Rec.Name:=fName;
  Rec.FontName :=fFontName;
  Rec.FontSize :=fFontSize;
  Rec.FontStyles :=fFontStyles;
  Rec.ForeColor :=fForeColor;
  Rec.BackColor :=fBackColor;
  Rec.CharCase :=fCharCase;
  Rec.Visible :=fVisible;
  Rec.Changeable :=fChangeable;
  Rec.Hotspot :=fHotspot;
  Rec.EOLFilled :=fEOLFilled;
  Rec.CharSet:=FCharSet;
end;
constructor TSciStyle.Create(Collection: TCollection);
begin
  inherited;
  fStyleNumber := Index;
  FCharSet:=-1;
  fName := sLMStyle+'['+IntToStr(Index)+']';
  fFontName := '';
  fFontSize := 0;
	fFontStyles:=[];
	fCharCase:=CASE_MIXED;
  fForeColor := clDefault;
  fBackColor := clDefault;
  fVisible := True;
  fEOLFilled := False;
  fChangeable := True;
  fHotSpot := False;
end;

procedure TSciStyle.SetBackColor(const Value: TColor);
begin
  fBackColor := Value;
  Changed(False);
end;

procedure TSciStyle.SetChangeable(const Value: Boolean);
begin
  fChangeable := Value;
  Changed(False);
end;

procedure TSciStyle.SetCharCase(const Value: TSciCase);
begin
  fCharCase := Value;
  Changed(False);
end;

procedure TSciStyle.SetEOLFilled(const Value: Boolean);
begin
  fEOLFilled := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontName(const Value: string);
begin
  fFontName := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontSize(const Value: Integer);
begin
  fFontSize := Value;
  Changed(False);
end;

procedure TSciStyle.SetFontStyles(const Value: TFontStyles);
begin
  FFontStyles := Value;
  Changed(False);
end;

procedure TSciStyle.SetForeColor(const Value: TColor);
begin
  fForeColor := Value;
  Changed(False);
end;

procedure TSciStyle.SetHotspot(const Value: Boolean);
begin
  fHotSpot := Value;
  Changed(False);
end;

procedure TSciStyle.SetName(const Value: string);
begin
  fName := Value;
  Changed(False);
end;

procedure TSciStyle.SetStyleNumber(const Value: Integer);
begin
  fStyleNumber := Value;
  Changed(False);
end;

procedure TSciStyle.SetVisible(const Value: Boolean);
begin
  fVisible := Value;
  Changed(False);
end;

{ TSciStyleList }

function TSciStyleList.GetStyle(const StyleNumber : LongInt) : TSciStyle;
var
i,cnt : Integer;
begin
  Result:=nil;
  cnt:=Count;
  for i:=0 to (cnt-1) do
  begin
    if TSciStyle(Items[i]).fStyleNumber=StyleNumber then
    begin
      Result:=TSciStyle(Items[i]);
      Exit;
    end;
  end;
end;

function TSciStyleList.HasStyle(const StyleNumber : LongInt) : Boolean;
var
i,cnt : Integer;
begin
  Result:=False;
  cnt:=Count;
  for i:=0 to (cnt-1) do
  begin
    if TSciStyle(Items[i]).fStyleNumber=StyleNumber then
    begin
      Result:=True;
      Exit;
    end;
  end;
end;

procedure TSciStyleList.SetEditor(Editor: TPersistent);
begin
  self.fEditor := Editor;
end;

procedure TSciStyleList.Update(Item: TCollectionItem);
Var
  i : Integer;
begin
  inherited;

  if not Assigned(fEditor) then Exit;
  if Assigned(Item) then with (Item as TSciStyle), TScintilla(fEditor) do
  begin
    if FCharSet<>-1 then StyleSetCharacterSet(StyleNumber, FCharSet);
    if fFontName <> '' then StyleSetFont(StyleNumber, PChar(fFontName));
    if fFontSize > 0 then StyleSetSize (StyleNumber, fFontSize);
    if fForeColor <> clDefault then StyleSetFore(StyleNumber, fForeColor);
    if fBackColor <> clDefault then StyleSetBack(StyleNumber, fBackColor);
    StyleSetBold(StyleNumber, fsBold in fFontStyles);
    StyleSetItalic(StyleNumber, fsItalic in fFontStyles);
    StyleSetUnderline(StyleNumber, fsUnderline in fFontStyles);
    StyleSetCase(StyleNumber, Ord(fCharCase));
    StyleSetVisible(StyleNumber, fVisible);
    StyleSetChangeable(StyleNumber, fChangeable);
    StyleSetHotSpot(StyleNumber, fHotSpot);
    StyleSetEOLFilled(StyleNumber, fEOLFilled);
  end else
  begin
    for i := 0 to Count - 1 do Update(Items[i]);
  end;
end;

{ TSciKeyWordsList }

function TSciKeyWordsList.GetKeywordList(const KeywordListNumber : LongInt) : TSciKeywords;
var
i,cnt : Integer;
begin
  Result:=nil;
  cnt:=Count;
  for i:=0 to (cnt-1) do
  begin
    if TSciKeywords(Items[i]).fKeywordListNumber=KeywordListNumber then
    begin
      Result:=TSciKeywords(Items[i]);
      Exit;
    end;
  end;
end;

function TSciKeyWordsList.HasList(const KeywordListNumber : LongInt) : Boolean;
var
i,cnt : Integer;
begin
  Result:=False;
  cnt:=Count;
  for i:=0 to (cnt-1) do
  begin
    if TSciKeywords(Items[i]).fKeywordListNumber=KeywordListNumber then
    begin
      Result:=True;
      Exit;
    end;
  end;
end;

procedure TSciKeyWordsList.SetEditor(Editor: TPersistent);
begin
  self.fEditor := Editor;
end;

procedure TSciKeyWordsList.Update(Item: TCollectionItem);
Var
  i : Integer;
begin
  inherited;
  if not Assigned(fEditor) then Exit;
  if Assigned(Item) then with (Item as TSciKeywords) do begin
{$Ifdef COMPILER6_UP}
    Keywords.Delimiter := ' ';
    TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(Keywords.DelimitedText));
{$Else}
    TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(MergeStrings(Keywords)));
{$Endif}
  end else begin
    for i := 0 to Count - 1 do with (Items[i] as TSciKeywords) do
    begin
{$Ifdef COMPILER6_UP}
      Keywords.Delimiter := ' ';
      TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(Keywords.DelimitedText));
{$Else}
      TScintilla(fEditor).SetKeywords(KeywordListNumber, PChar(MergeStrings(Keywords)));
{$Endif}
    end;
  end;
end;

{ TSciLanguageManager }

procedure TSciLanguageManager.Assign(Source: TPersistent);
begin
  if Source is TSciLanguageManager then
	begin
    //fLexerNamesMapper.Assign(TSciLanguageManager(Source).fLexerNamesMapper);
    flexerlist.Assign(TSciLanguageManager(Source).flexerlist);
		fLanguages.Assign(TSciLanguageManager(Source).LanguageList);
    fSelectedLanguage := TSciLanguageManager(Source).SelectedLanguage;
    //fExtraData := TSciLanguageManager(Source).fExtraData;
		Update;
{$Ifndef NOLANGCHANGEDEVENT}
		if assigned(fOnLanguageChanged) then
			fOnLanguageChanged(self,fSelectedLanguage);
{$Endif}

	end else
		inherited;
end;

constructor TSciLanguageManager.Create(Editor: TPersistent);
begin
	inherited Create;
  fDisabled:=false;
	fEditor := Editor;
  fExtraData:=nil;
{$Ifdef COMPILER6_UP}
  flexerlist:=TStringList.Create;
  TStringList(flexerlist).CaseSensitive:=True;
{$Else}
  flexerlist:=TMyStringList.Create;
  TMyStringList(flexerlist).CaseSensitive:=True;
{$Endif}
  FillLexerNameMap;
  fSelectedLanguage:='null';
	fLanguages:=TSciLangList.Create(self,TSciLangItem);
	fLanguages.SetEditor(fEditor);
end;

function TSciLanguageManager.LangToLex(const Lang : String) : String;
var
i : Integer;
cnt : Integer;
begin
  if (Lang='') or (AnsiCompareText(Lang,scicontainerconst)=0) then
  begin
    Result:=scicontainerconst;
  end;
  if fLanguages<>nil then
  begin
    cnt:=fLanguages.Count;
    for i:=0 to (cnt-1) do
    begin
      if AnsiCompareStr(TSciLangItem(fLanguages.Items[i]).Name,Lang)=0 then
      begin
        Result:=TSciLangItem(fLanguages.Items[i]).Lexer;
        Exit;
      end;
    end;
  end;
  Result:=Lang;
end;
destructor TSciLanguageManager.Destroy;
begin
	fLanguages.Free;
  if assigned(flexerlist) then flexerlist.Free;
	inherited;
end;

function TSciLanguageManager.GetOwner: TPersistent;
begin
	Result := fEditor;
end;


procedure TSciLanguageManager.SetSelectedLanguage(const Value : String);
begin
	fSelectedLanguage:=Value;
  if (AnsiCompareText(fSelectedLanguage,scicontainerconst)<>0) and (fSelectedLanguage<>'') then
    fLanguages.Select(Value);
  Update;
	{$Ifndef NOLANGCHANGEDEVENT}
	if (assigned(fOnLanguageChanged)) then
		fOnLanguageChanged(self,Value);
	{$Endif}

end;

procedure TSciLanguageManager.Update;
var
itm : TSciLangItem;
mappedstr : String;
//hyper : Boolean;
//sbits : Integer;
begin
	inherited;
	if (not Assigned(fEditor)) or (fDisabled=True) then Exit;
	with TScintilla(fEditor) do
	begin
    //
    mappedstr:=LangToLex(fSelectedLanguage);
    if AnsiCompareText(mappedstr,scicontainerconst)=0 then
    begin
      SetLexer(SCLEX_CONTAINER);
      StyleClearAll;
      if assigned(fOnLexUpdate) then //
        fOnLexUpdate(Self,fEditor,fSelectedLanguage,nil);//
    end else
    begin
      SetLexerLanguage(PChar(mappedstr));
      if (AnsiCompareStr(mappedstr,'xml')=0) then
      begin
        SetProperty('html.tags.case.sensitive','1');
      end else if (AnsiCompareStr(mappedstr,'hypertext')=0) then
      begin
        SetProperty('html.tags.case.sensitive','0');
      end;
      fLanguages.SetEditor(fEditor);
      itm:=fLanguages.GetStyleList(fSelectedLanguage);
      ClearDocumentStyle;
      SetStyleBits(itm.fNumStyleBits);
      StyleClearAll;

      // If assigned, perform other updates in the event.
      if assigned(fOnLexUpdate) then fOnLexUpdate(Self,fEditor,fSelectedLanguage,itm);
      itm.Update;
    end;
  end;
  if AnsiCompareText(mappedstr,scicontainerconst)<>0 then
  begin
    fLanguages.SetEditor(fEditor);
    itm:=fLanguages.GetStyleList(fSelectedLanguage);
    // If assigned, perform other updates in the event.
    if assigned(fOnLexUpdate) then fOnLexUpdate(Self,fEditor,fSelectedLanguage,itm);
    itm.Update;
  end else
    if assigned(fOnLexUpdate) then
      fOnLexUpdate(Self,fEditor,fSelectedLanguage,nil);
end;

procedure TSciLanguageManager.FillListWithLanguages(lst : TStrings;const which : sciLexerSelectTypes);
var
  cnt : Integer;
  i : Integer;
  tmpstr : String;
begin
  if lst=nil then Exit;
  try
    lst.BeginUpdate;
    lst.Clear;
    if sciLexers in which then
    begin
      cnt:=flexerlist.Count;
      for i:=0 to (cnt-1) do
      begin
        tmpstr:=flexerlist.Strings[i];
        lst.Add(tmpstr);
      end;
    end;
    if (sciLanguages in which) then
    begin
      cnt:=fLanguages.Count;
      for i:=0 to (cnt-1) do
      begin
        tmpstr:=TSciLangItem(fLanguages.Items[i]).Name;
        if lst.IndexOf(tmpstr)=-1 then
        lst.Add(tmpstr);
      end;
    end;
  finally
    lst.EndUpdate;
  end;
end;

function    TSciLanguageManager.HasLexer(const Lexer : String) : Boolean;
begin
  Result:=flexerlist.IndexOf(Lexer)<>-1;
end;

procedure   TSciLanguageManager.RemoveLexer(const Lexer : String);
var
  idx : Integer;
begin
  idx:=flexerlist.IndexOf(Lexer);
  if(idx<>-1) then
    flexerlist.Delete(idx);
end;

procedure   TSciLanguageManager.AddLexer(const Lexer : String);
begin
  if flexerlist.IndexOf(Lexer)=-1 then
    flexerlist.Add(Lexer);
end;

procedure TSciLanguageManager.SetLanguageList(const Value : TSciLangList);
begin
	fLanguages.Assign(Value);
	Update;
end;

function TSciLanguageManager.HasLanguage(const LanguageName : String) : Boolean;
begin
  Result:=False;
  if LanguageList.Find(LanguageName)<>nil then
    Result:=True;
  Exit;
end;
procedure TSciLanguageManager.FillLexerNameMap;
begin
{Fill the namelist with all the lexers we have}
  with flexerlist do
  begin
    Add('ada');
    Add('apdl');
    Add('asm');
    Add('asn1');
    Add('au3');
    Add('ave');
    Add('bash');
    Add('batch');
    Add('bullant');
    Add('baan');
    Add('clw');
    Add('clwnocase');
    Add('conf');
    Add('cpp');
    Add('cppnocase');
    Add('css');
    Add('diff');
    Add('eiffel');
    Add('eiffelkw');
    Add('email');
    Add('erlang');
    Add('errorlist');
    Add('escript');
    Add('f77');
    Add('forth');
    Add('fortran');
    Add('gui4cli');
    Add('hypertext');
    Add('kix');
    Add('latex');
    Add('lisp');
    Add('lot');
    Add('lout');
    Add('lua');
    Add('makefile');
    Add('matlab');
    Add('metapost');
    Add('mmixal');
    Add('mssql');
    Add('nncrontab');
    Add('nsis');
    Add('null');
    Add('octave');
    Add('pascal');
    Add('perl');
    Add('phpscript');
    Add('pov');
    Add('powerbasic');
    Add('props');
    Add('ps');
    Add('python');
    Add('ruby');
    Add('scriptol');
    Add('specman');
    Add('sql');
    Add('tcl');
    Add('tex');
    Add('vb');
    Add('vbscript');
    Add('verilog');
    Add('vhdl');
    Add('xml');
    Add('yaml');
    Add('purebasic');
    Add('blitzbasic');
    Add('caml');
    Add('haskell');


    // Include more lexers, if there are some added by a developer
    // for easy maintaining..
    {$INCLUDE morelexers.inc}
  end;
end;

function TSciLanguageManager.AddLanguage(const Lang,Lexer : String) : TSciLangItem;
var
  itm : TSciLangItem;
begin
  itm:=fLanguages.GetStyleList(Lang,Lexer);
  Result:=itm;
end;
function TSciLanguageManager.DupLanguage(const Langtocopy : String;const langname : String) : TSciLangItem;
var
	itm : TSciLangItem;
	src : TSciLangItem;
begin
	src:=fLanguages.Find(Langtocopy);
	if src<>nil then
	begin
	itm:=TSciLangItem(fLanguages.Add);
	itm.Assign(src);
	end else itm:=nil;
	Result:=itm;
end;
function TSciLanguageManager.RemoveLanguage(const Langtoremove : String;var notinstaticlist : Boolean) : Boolean;
var
	itm : TSciLangItem;
begin
	itm:=fLanguages.Find(Langtoremove);
	if itm<>nil then
	begin
    if flexerlist.IndexOf(Langtoremove)<>-1 then
    begin
      notinstaticlist:=False;
      if (Langtoremove<>'null') and (Langtoremove<>'container') then
        fLanguages.Delete(itm.Index);
    end else
    begin
		  fLanguages.Delete(itm.Index);
      notinstaticlist:=True;
    end;
		Result:=True;
		Exit;
	end;
	Result:=false;
end;
procedure TSciLanguageManager.SetDisabled(const Value : Boolean);
begin
  if (fDisabled<>Value) then
  begin
    fDisabled:=Value;
    if(fDisabled=False) then Update;
  end;
end;


function SameStyle(Style1,Style2 : TSciStyle) : Boolean;
begin
	Result:=((Style1.FontName=Style2.FontName) and
	(Style1.FontSize=Style2.FontSize) and
	(Style1.FontStyles=Style2.FontStyles) and
	(Style1.ForeColor=Style2.ForeColor) and
	(Style1.BackColor=Style2.BackColor) and
	(Style1.CharCase=Style2.CharCase) and
	(Style1.Visible=Style2.Visible) and
	(Style1.Changeable=Style2.Changeable) and
	(Style1.EOLFilled=Style2.EOLFilled) and
	(Style1.Hotspot=Style2.Hotspot) and
  (Style1.CharSet=Style2.CharSet));
end;

function SameStyle(Style1,Style2 : TSciLangStyle) : Boolean;
begin
	Result:=((Style1.FontName=Style2.FontName) and
	(Style1.FontSize=Style2.FontSize) and
	(Style1.FontStyles=Style2.FontStyles) and
	(Style1.ForeColor=Style2.ForeColor) and
	(Style1.BackColor=Style2.BackColor) and
	(Style1.CharCase=Style2.CharCase) and
	(Style1.Visible=Style2.Visible) and
	(Style1.Changeable=Style2.Changeable) and
	(Style1.EOLFilled=Style2.EOLFilled) and
	(Style1.Hotspot=Style2.Hotspot) and
  (Style1.CharSet=Style2.CharSet));
end;
end.
