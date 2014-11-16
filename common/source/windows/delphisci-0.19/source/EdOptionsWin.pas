//CE_Desc_Include(helpdescriptions.txt)
{$Include SciCommonDef.Inc}
unit EdOptionsWin;
{
Unit    : EdOptionsWin
Purpose : Options Dialog for Scintilla editors
Created : 20/03/2003
     $Id: EdOptionsWin.pas,v 1.7 2004/12/03 17:18:58 hdalis Exp $
Original Author: Kiriakos Vlahos (kvlahos@london.edu)
Current Author : hdalis
History 29/09/2004 Initial Release with Delphi Scintilla Interface Components
                   Extended and customized the optionsbox
                   No longer accepts enums, now it accepts strings.
                   Color,Caret,Hotspot etc properties added.
                   (hdalis@users.sourceforge.net)
        13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                   generate the help.
                   Renamed the Scintilla property to Editor
                   (hdalis@users.sourceforge.net)
        25/10/2004 Redesigned the whole form. Added add,copy,remove languages buttons.
                   Added Active Hotspot options.
                   The Keywords page doesn't exist anymore, it is now a part of the
                   Highlighter page.
                   (hdalis@users.sourceforge.net)
        02/11/2004 No longer displays a list of all lexers, now only defined languages
                   are displayed, i.e those defined in the Highlighter.LanguageList.
                   If you want to add a language, click Add and set the languagename and
                   the lexer to use for it. You can have as many languages defined as you wish.
        09/11/2004 Added EdgeColumn,EdgeType,EdgeColor
                   Some redesign of the Options page to
                   make it fit.. To be adjusted later.
				18/11/2004 Changed the TSciSynLexer to TSciLanguageManager, and changed all properties etc to reflect that.
        26/11/2004 Renamed the SetScintilla function to SetEditor to conform with the rest of them..
BUGFIXES:
        04/11/2004 When a new keylist was added, and you typed a description then the text
                   in the listbox wasn't updated. Fixed.
        15/01/2005 Added Comment* and AssignmentOperator,EndOfStatementOperator to the dialog.
                   Changed the Update* procs to take a TSciLangItem, and use that instead of
                   looking up the language all the time.. Improves performance of the dialog somewhat,
                   microscopic but..
				27/05/2005 Swapped the columns in the KeyBindings listview. Added sort.
				           Fixed a couple of memoryleaks.

}
interface
uses
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, ExtCtrls, StdCtrls, Buttons,SciLexer,SciLexerMod,SciLexerMemo,SciKeyBindings,
	ComCtrls, ScintillaLanguageManager, tcFontCombobox, Menus;

type
	TEditorOptions = class(TPersistent)
	private
		FReadOnly : boolean;
		FBraceHilite : boolean;
		FGutter: boolean;
		FWordWrap: TWordWrapType;
		FLineNumbers: boolean;
		FIndentation : TIndentationOptions;
		FCodeFolding: sciCodeFoldingFlags;
		FCaretFore : TColor;
		FUseUnicode: boolean;
		FIndentWidth: Integer;
		FTabWidth: Integer;
		FEOLStyle: TEOLStyle;
		FCaretLineVisible : Boolean;
		FCaretBack : TColor;
		FSaveClearsUndo : Boolean;
		FFont: TFont;
		FLanguageManager: TSciLanguageManager;
		fKeyCommands : TSciKeyCommandCollection;
		FCaretWidth: Integer;
		FSelFore : TColor;
		FSelBack : TColor;
		FFoldLo : TColor;
		FFoldHi : TColor;
		FMarkerFore : TColor;
		FMarkerBack : TColor;
		FBMarkFore : TColor;
		FBMarkBack : TColor;
    FHotActiveFore : TColor;
    FHotActiveBack : TColor;
    FHotActiveUnderline : Boolean;
    FHotActiveSingleLine : Boolean;
    FEdgeColor : TColor;
    FEdgeColumn : TColor;
    FEdgeType : sciEdgeType;
    FColor : TColor;
    FMarkerType : sciMarkerType;
    procedure SetFont(const Value: TFont);
		procedure SetLanguageManager(const Value: TSciLanguageManager);
		procedure SetKeyCommands(const Value: TSciKeyCommandCollection);
	public
		constructor Create;
		destructor Destroy; override;
		procedure Assign(Source: TPersistent);override;
		procedure GetOptions(Scintilla : TScintilla);
		procedure SetOptions(Scintilla : TScintilla);
	published
		property LanguageManager : TSciLanguageManager read FLanguageManager write SetLanguageManager;
		property ReadOnly : boolean read fReadOnly write fReadOnly default False;
		property BraceHilite : boolean read FBraceHilite write FBraceHilite default True;
		property Gutter : boolean read FGutter write FGutter default True;
		property Indentation : TIndentationOptions read FIndentation write fIndentation;
		property LineNumbers : boolean read FLineNumbers write FLineNumbers default False;
		property UseUnicode : boolean read FUseUnicode write FUseUnicode default True;
		property WordWrap : TWordWrapType read FWordWrap write FWordWrap default sciNoWrap;
		property CodeFolding : sciCodeFoldingFlags read FCodeFolding write FCodeFolding default [foldFold,foldCompact,foldComment,foldPreprocessor,foldAtElse,foldHTML,foldHTMLPreProcessor];
		property EOLStyle : TEOLStyle read FEOLStyle write FEOLStyle default eolCRLF;
		property TabWidth : Integer read FTabWidth write FTabWidth  default 8;
		property IndentWidth : Integer read FIndentWidth write FIndentWidth default 0;
		property KeyCommands : TSciKeyCommandCollection read FKeyCommands write SetKeyCommands;
		property CaretFore : TColor read FCaretFore Write FCaretFore;
		property CaretBack : TColor read FCaretBack Write FCaretBack;
		property Font : TFont read FFont write SetFont;
		property CaretWidth : Integer read FCaretWidth write FCaretWidth;
		property CaretLineVisible : Boolean read FCaretLineVisible write FCaretLineVisible;
		property SelFore : TColor read FSelFore write FSelFore default clDefault;
		property SelBack : TColor read FSelBack write FSelBack default clDefault;
		property FoldLo : TColor read FFoldLo write FFoldLo default clDefault;
		property FoldHi : TColor read FFoldHi write FFoldHi default clDefault;
		property MarkerFore : TColor read FMarkerFore write FMarkerFore default clDefault;
		property MarkerBack : TColor read FMarkerBack write FMarkerBack default clDefault;
		property BMarkFore : TColor read FBMarkFore write FBMarkFore default clDefault;
		property BMarkBack : TColor read FBMarkBack write FBMarkBack default clDefault;
		property XSaveClearsUndo : Boolean read FSaveClearsUndo write FSaveClearsUndo default false;
    property HotActiveFore : TColor read FHotActiveFore write FHotActiveFore default clDefault;
    property HotActiveBack : TColor read FHotActiveBack write FHotActiveBack default clDefault;
    property HotActiveUnderline : Boolean read FHotActiveUnderline write FHotActiveUnderline;
    property HotActiveSingleLine : Boolean read FHotActiveSingleLine write FHotActiveSingleLine;
    property EdgeColor : TColor read FEdgeColor write FEdgeColor;
    property EdgeColumn : TColor read FEdgeColumn write FEdgeColumn;
    property EdgeType : sciEdgeType read FEdgeType write FEdgeType;
    property MarkerType : sciMarkerType read FMarkerType write FMarkerType;
    property Color : TColor read FColor write FColor;
	end;


	TEdOptionsWindow = class(TForm)
		OptionPages: TPageControl;
		OptionsPage: TTabSheet;
    ColorsPage: TTabSheet;
    colorsPanel: TPanel;
    optionsPanel: TPanel;
    viewOptionsBox: TGroupBox;
		CodeFoldingCB: TCheckBox;
		GutterCB: TCheckBox;
		LineNumbersCB: TCheckBox;
		IndentationGuidesCB: TCheckBox;
    indentationBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    KeepIndentCB: TCheckBox;
    TabIndentsCB: TCheckBox;
    BackSpaceUnIndentsCB: TCheckBox;
    buttonPanel: TPanel;
    CancelBtn: TBitBtn;
    ApplyBtn: TBitBtn;
    OKBtn: TBitBtn;
    HighlighterPage: TTabSheet;
    highlighterPanel: TPanel;
    FontDialog: TFontDialog;
    languageCBBox: TGroupBox;
    LanguageCB: TComboBox;
		Label13: TLabel;
		CaretCB: TColorBox;
		Label14: TLabel;
    CaretWidthSE: TEdit;
		KeyCommandsPage: TTabSheet;
    keycommandsPanel: TPanel;
		KeyCmdList: TListView;
    TabWidthSE: TEdit;
    IndentWidthSE: TEdit;
		CaretLineVisCB : TCheckBox;
		CaretBackCB : TColorBox;
		Label15: TLabel;
		Label16: TLabel;
		Label17: TLabel;
		Label20: TLabel;
		Label21: TLabel;
		SelForeCB: TColorBox;
		SelBackCB: TColorBox;
		MarkerForeCB: TColorBox;
		MarkerBackCB: TColorBox;
		BMarkForeCB	: TColorBox;
		BMarkBackCB	: TColorBox;
		Label22 : TLabel;
		Label23 : TLabel;
    addLangButton: TBitBtn;
    remLangButton: TBitBtn;
    copyLangButton: TBitBtn;
    keybuttonsPanel: TPanel;
    btnAdd: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnReset: TBitBtn;
    HotspotGB: TGroupBox;
    Label24: TLabel;
    Label25: TLabel;
    HotActiveForeCB: TColorBox;
    HotActiveBackCB: TColorBox;
    HotActiveUnderlineCB: TCheckBox;
    HotActiveSingleLineCB: TCheckBox;
    FoldingGB: TGroupBox;
    Label26: TLabel;
    Label27: TLabel;
    FoldLoCB: TColorBox;
    FoldHiCB: TColorBox;
    MarkersGB: TGroupBox;
    highlighterPageCtrl: TPageControl;
    stylesTabSheet: TTabSheet;
    keywordsTabSheet: TTabSheet;
    topStylePanel: TPanel;
    GroupBox5: TGroupBox;
    StylesLB: TListBox;
    stylenoBox: TGroupBox;
    Label4: TLabel;
    Label9: TLabel;
    StyleNumberSE: TEdit;
    DescriptionEB: TEdit;
    AddStyleB: TBitBtn;
    DeleteStyleB: TBitBtn;
    topKeywordsPanel: TPanel;
    keywordlistBox: TGroupBox;
    KeyListsLB: TListBox;
    KeywordListGB: TGroupBox;
    Label11: TLabel;
    Label12: TLabel;
    KeyListNumberSE: TEdit;
    KeyListDescriptionEB: TEdit;
    KeyListAdd: TBitBtn;
    KeyListDelete: TBitBtn;
    KeywordsM: TMemo;
    Label18: TLabel;
    Label19: TLabel;
    StyleDefGB: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    DefaultFontCB: TCheckBox;
    SizeSE: TEdit;
    ForeCB: TColorBox;
    BackCB: TColorBox;
    ItalicCB: TCheckBox;
    BoldCB: TCheckBox;
    UnderlineCB: TCheckBox;
    VisibleCB: TCheckBox;
    ChangeableCB: TCheckBox;
    EOLFilledCB: TCheckBox;
    CaseCB: TComboBox;
    HotspotCB: TCheckBox;
    FontCB: TtcFontCombobox;
    gbRightEdge: TGroupBox;
    Label28: TLabel;
    Label29: TLabel;
    EdgeColumnSE: TEdit;
    EdgeColorCB: TColorBox;
    BraceHiliteCB: TCheckBox;
    Label30: TLabel;
    EdgeTypeCB: TComboBox;
    markerTypeCB: TComboBox;
    Label31: TLabel;
    OtherGB: TGroupBox;
    UseUnicodeCB: TCheckBox;
    SaveClearsUndoCB: TCheckBox;
    ReadOnlyCB: TCheckBox;
    Label3: TLabel;
    EOLStyleCB: TComboBox;
    DefaultsPage: TTabSheet;
    Label35: TLabel;
    defaultBackgroundCB: TColorBox;
    FontButton: TBitBtn;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label36: TLabel;
    otherPage: TTabSheet;
    langOperators: TGroupBox;
    Label37: TLabel;
    Label38: TLabel;
    AssignmentOperatorED: TEdit;
    EndOfStatementED: TEdit;
    langComments: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    commentBoxStart: TEdit;
    commentBoxEnd: TEdit;
    commentBoxMiddle: TEdit;
    commentBlock: TEdit;
    commentStreamStart: TEdit;
    commentStreamEnd: TEdit;
    langSettingsBox: TGroupBox;
    Label53: TLabel;
    Label54: TLabel;
    NumStyleBitsED: TEdit;
    lexerforlangCB: TComboBox;
    WordWrapCB: TComboBox;
    KeyMenu: TPopupMenu;
    LoadKeyCommands1: TMenuItem;
    SaveKeycommands1: TMenuItem;
    odia: TOpenDialog;
    sdia: TSaveDialog;
		procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure ApplyBtnClick(Sender: TObject);
		procedure FontButtonClick(Sender: TObject);
		procedure OKBtnClick(Sender: TObject);
		procedure CancelBtnClick(Sender: TObject);
		procedure StylesLBClick(Sender: TObject);
		procedure LanguageCBChange(Sender: TObject);
		procedure StyleElementChanged(Sender: TObject);
		procedure DeleteStyleBClick(Sender: TObject);
		procedure AddStyleBClick(Sender: TObject);
		procedure KeyListsLBClick(Sender: TObject);
		procedure KeyListElementsChange(Sender: TObject);
		procedure KeyListDeleteClick(Sender: TObject);
		procedure KeyListAddClick(Sender: TObject);
		procedure btnDeleteClick(Sender: TObject);
		procedure btnResetClick(Sender: TObject);
		procedure btnEditClick(Sender: TObject);
		procedure btnAddClick(Sender: TObject);
		procedure StyleNumberSEKeyPress(Sender : TObject;var ch : Char);
    procedure TestNumericOnly(Sender: TObject; var Key: Char);
    procedure addLangButtonClick(Sender: TObject);
    procedure remLangButtonClick(Sender: TObject);
    procedure copyLangButtonClick(Sender: TObject);
    procedure OptionPagesChange(Sender: TObject);
    procedure KeyCmdListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure AssignmentOperatorEDChange(Sender: TObject);
    procedure EndOfStatementEDChange(Sender: TObject);
    procedure commentBoxStartChange(Sender: TObject);
    procedure commentBoxMiddleChange(Sender: TObject);
    procedure commentBoxEndChange(Sender: TObject);
    procedure commentBlockChange(Sender: TObject);
    procedure commentStreamStartChange(Sender: TObject);
    procedure commentStreamEndChange(Sender: TObject);
    procedure lexerforlangCBExit(Sender: TObject);
    procedure NumStyleBitsEDExit(Sender: TObject);
    procedure LoadKeyCommands1Click(Sender: TObject);
    procedure SaveKeycommands1Click(Sender: TObject);
	private
		{ Private declarations }
		FEditor : TScintilla;
		fEditorOptions : TEditorOptions;
		fOldEditorOptions : TEditorOptions;
		UpdatingStyle : Boolean;
		UpdatingKeyList : Boolean;
		procedure SetEditor(const Value: TScintilla);
		procedure UpdateStyles(curlng : TSciLangItem=nil);
		procedure UpdateKeywords(curlng : TSciLangItem=nil);
		procedure UpdateKeyCommands;
    procedure UpdateOther(curlng : TSciLangItem=nil);
    function  CurrentLang : TSciLangItem;    
	public
		{ Public declarations }
		property Editor : TScintilla read FEditor write SetEditor;
    procedure   RescanLanguageList;

	end;

var
	EdOptionsWindow: TEdOptionsWindow;

implementation

{$R *.dfm}

Uses
	SciSupport,SciKeyEditForm,SciAddLanguageFormUnit,SciResLang,sciUtils;

{ TEditorOptions }

procedure TEditorOptions.Assign(Source: TPersistent);
begin
  if Source is TEditorOptions then
    with TEditorOptions(Source) do
    begin
      Self.FReadOnly := ReadOnly;
      Self.FBraceHilite := BraceHilite;
      Self.FGutter := Gutter;
      Self.FWordWrap := WordWrap;
      Self.FLineNumbers := LineNumbers;
      Self.FIndentation := Indentation;
      Self.FCodeFolding := CodeFolding;
      Self.FUseUnicode := UseUnicode;
      Self.FTabWidth := TabWidth ;
      Self.FIndentWidth := IndentWidth;
      Self.FEOLStyle := EOLStyle;
      Self.SetFont(Font);
      Self.FLanguageManager.Assign(LanguageManager);
      Self.KeyCommands.Assign(KeyCommands);
      Self.FCaretBack :=CaretBack;
      Self.FCaretFore :=CaretFore;
      Self.FSelFore := SelFore;
      Self.FFoldLo :=FoldLo;
      Self.FFoldHi :=FoldHi;
      Self.FMarkerFore :=MarkerFore;
      Self.FMarkerBack :=MarkerBack;
      Self.FBMarkFore :=BMarkFore;
      Self.FBMarkBack :=BMarkBack;
      Self.FSelBack := SelBack;
      Self.FCaretWidth :=CaretWidth;
      Self.FCaretLineVisible :=CaretLineVisible;
      Self.FSaveClearsUndo :=XSaveClearsUndo;
      Self.FHotActiveFore:=HotActiveFore;
      Self.FHotActiveBack:=HotActiveBack;
      Self.FHotActiveUnderline:=HotActiveUnderline;
      Self.FHotActiveSingleLine:=HotActiveSingleLine;
      Self.FEdgeColor:=EdgeColor;
      Self.FEdgeColumn:=EdgeColumn;
      Self.FEdgeType:=EdgeType;
      Self.FMarkerType:=MarkerType;
      Self.FColor:=Color;
	  end
  else if Source is TScintilla then
  begin
		GetOptions(Source as TScintilla);
	end else
		inherited;
end;

constructor TEditorOptions.Create;
begin
	inherited;
	FFont := TFont.Create;
	FLanguageManager := TSciLanguageManager.Create(nil);
	fKeyCommands := TSciKeyCommandCollection.Create(nil);
end;

destructor TEditorOptions.Destroy;
begin
	if assigned(FFont) then FFont.Free;
	if assigned (FLanguageManager) then FLanguageManager.Free;
	if assigned (fKeyCommands) then fKeyCommands.Free; //added to avoid a memoryleak after a tip by mak
	inherited;
end;

procedure TEditorOptions.GetOptions(Scintilla: TScintilla);
begin
	with Scintilla do
  begin
		FReadOnly := ReadOnly;
		FBraceHilite := BraceHilite;
		FGutter := Gutter1.Width <> 0;
		FWordWrap := WordWrap;
		FLineNumbers := Gutter0.Width <> 0;
		FIndentation := Indentation;
		FCodeFolding := Folding;
		FUseUnicode := UseUnicode;
		FTabWidth := TabWidth ;
		FIndentWidth := IndentWidth;
		FEOLStyle := EOLStyle;
		Self.SetFont(Font);
		FLanguageManager.Assign(LanguageManager);
		fKeyCommands.Assign(Scintilla.KeyCommands);
		FSelFore:=Colors.SelFore;
		FSelBack:=Colors.SelBack;
		FFoldLo :=Colors.FoldLo;
		FFoldHi :=Colors.FoldHi;
		FMarkerFore :=Colors.MarkerFore;
		FMarkerBack :=Colors.MarkerBack;
		FBMarkFore :=Colors.BookMarkFore;
		FBMarkBack :=Colors.BookMarkBack;
    FHotActiveFore:=ActiveHotSpot.ForeColor;
    FHotActiveBack:=ActiveHotSpot.BackColor;
    Self.FHotActiveUnderline:=ActiveHotSpot.Underlined;
    Self.FHotActiveSingleLine:=ActiveHotSpot.SingleLine;
    Self.FEdgeColor:=EdgeColor;
    Self.FEdgeColumn:=EdgeColumn;
    Self.FEdgeType:=EdgeMode;
    Self.FMarkerType:=FoldMarkerType;
    Self.FColor:=Color;
		FSaveClearsUndo :=ClearUndoAfterSave;
		FCaretFore :=Caret.ForeColor;
		FCaretBack :=Caret.LineBackColor;
		FCaretWidth :=Caret.Width;
		FCaretLineVisible :=Caret.LineVisible;
	end;
end;

procedure TEditorOptions.SetFont(const Value: TFont);
begin
	FFont.Assign(Value);
end;

procedure TEditorOptions.SetKeyCommands(const Value: TSciKeyCommandCollection);
begin
	FKeyCommands.Assign(Value);
end;

procedure TEditorOptions.SetOptions(Scintilla: TScintilla);
begin
	with Scintilla do begin
    Color:=FColor;
		Caret.ForeColor:=CaretFore;
    //ColorToRGB(CaretFore);
		Caret.Width:=FCaretWidth;
		Caret.LineVisible :=FCaretLineVisible;
		Caret.LineBackColor :=FCaretBack;
		Colors.SelFore :=SelFore;
		Colors.SelBack :=SelBack;
		Colors.FoldLo :=FFoldLo;
		Colors.FoldHi :=FFoldHi;
		Colors.MarkerFore :=FMarkerFore;
		Colors.MarkerBack :=FMarkerBack;
		Colors.BookMarkBack :=FBMarkBack;
		Colors.BookMarkFore :=FBMarkFore;
    ActiveHotSpot.ForeColor:=FHotActiveFore;
    ActiveHotSpot.BackColor:=FHotActiveBack;
    ActiveHotSpot.Underlined :=Self.FHotActiveUnderline;
    ActiveHotSpot.SingleLine :=Self.FHotActiveSingleLine;
		ClearUndoAfterSave :=FSaveClearsUndo;
    EdgeColor:=FEdgeColor;
    EdgeColumn:=FEdgeColumn;
    EdgeMode:=FEdgeType;
    Scintilla.FoldMarkerType:=FMarkerType;
		if ReadOnly <> FReadOnly then ReadOnly := FReadOnly;
		BraceHilite := FBraceHilite;
		if FGutter <> (Gutter1.Width <> 0) then
    begin
			if FGutter then	Gutter1.Width := 16
			  else
          Gutter1.Width := 0;
		end;
		if WordWrap <> FWordWrap then WordWrap := FWordWrap;
		if FLineNumbers <> (Gutter0.Width <> 0) then
    begin
			if FLineNumbers then
      begin
				Gutter0.Width := 32;
				Gutter0.MarginType := gutLineNumber;
			end else
				Gutter0.Width := 0;
		end;
		if Indentation <> FIndentation then
      Indentation := FIndentation;
		if CodeFolding<>Folding then
    begin
      Folding:=CodeFolding;
    end;
		if UseUnicode <> FUseUnicode then
      UseUnicode := FUseUnicode;
		if IndentWidth <> FIndentWidth then
      IndentWidth := FIndentWidth;
		if TabWidth <> FTabWidth then
      TabWidth := FTabWidth;
		if EOLStyle <> FEOLStyle then
      EOLStyle := FEOLStyle;
		if (Font.Name <> FFont.Name) or (Font.Size <> FFont.Size) or (Font.Charset <> FFont.Charset) or (Font.Style <> FFont.Style) or (Font.Color<>FFont.Color) then
		  Font := FFont;
		LanguageManager.Assign(Self.FLanguageManager);
		LanguageManager.Update;
		KeyCommands.Assign(FKeyCommands);
	end;
end;

procedure TEditorOptions.SetLanguageManager(const Value: TSciLanguageManager);
begin
	FLanguageManager.Assign(Value);
end;

{ TEdOptionsWindow }

function TEdOptionsWindow.CurrentLang : TSciLangItem;
begin
  Result:=FEditorOptions.LanguageManager.LanguageList.GetStyleList(LanguageCB.Items[LanguageCB.ItemIndex]);
end;

procedure TEdOptionsWindow.StyleNumberSEKeyPress(Sender : TObject;var ch : Char);
begin
  TestNumericOnly(Sender,ch);
	if ch<>#0 then StyleElementChanged(Sender);
end;

procedure TEdOptionsWindow.SetEditor(const Value: TScintilla);
var
xitm : TSciLangItem;
i : Integer;
begin
	FEditor := Value;
  RescanLanguageList;
	fEditorOptions.GetOptions(Editor);
	with fEditorOptions do
  begin
		ReadOnlyCB.Checked := ReadOnly;
		BraceHiliteCB.Checked := BraceHilite;
		GutterCB.Checked := Gutter;
		WordWrapCB.ItemIndex:= LongInt(WordWrap);
		LineNumbersCB.Checked := LineNumbers;
		KeepIndentCB.Checked := SciLexerMemo.KeepIndent in Indentation;
		TabIndentsCB.Checked := SciLexerMemo.TabIndents in Indentation;
		BackSpaceUnindentsCB.Checked := SciLexerMemo.BackSpaceUnindents in Indentation;
		IndentationGuidesCB.Checked := SciLexerMemo.IndentationGuides in Indentation;
		CodeFoldingCB.Checked := (foldFold in CodeFolding);
		UseUnicodeCB.Checked := UseUnicode;
		TabWidthSE.Text := IntToStr(TabWidth);
		IndentWidthSE.Text := IntToStr(IndentWidth);
		EOLStyleCB.ItemIndex := Integer(EOLStyle);
		CaretCB.Selected:=CaretFore;
		SelForeCB.Selected:=SelFore;
		SelBackCB.Selected:=SelBack;
		CaretWidthSE.Text:=IntToStr(CaretWidth);
		CaretLineVisCB.Checked :=CaretLineVisible;
		CaretBackCB.Selected :=CaretBack;
		FoldLoCB.Selected :=FoldLo;
		FoldHiCB.Selected :=FoldHi;
		MarkerForeCB.Selected :=MarkerFore;
		MarkerBackCB.Selected :=MarkerBack;
		BMarkForeCB.Selected :=BMarkFore;
		BMarkBackCB.Selected :=BMarkBack;
    HotActiveForeCB.Selected:=HotActiveFore;
    HotActiveBackCB.Selected:=HotActiveBack;
    HotActiveUnderlineCB.Checked:=HotActiveUnderline;
    HotActiveSingleLineCB.Checked:=HotActiveSingleLine;
		SaveClearsUndoCB.Checked :=XSaveClearsUndo;
    EdgeColorCB.Selected:=EdgeColor;
    EdgeColumnSE.Text:=IntToStr(EdgeColumn);
    EdgeTypeCB.ItemIndex:=LongInt(EdgeType);
    markerTypeCB.ItemIndex:=LongInt(MarkerType);
    defaultBackgroundCB.Selected:=Color;
		//  Syntax Highlighter
		LanguageCB.ItemIndex := LanguageCB.Items.IndexOf(LanguageManager.SelectedLanguage);
    xitm:=LanguageManager.LanguageList.Find(LanguageManager.SelectedLanguage);
		UpdateStyles(xitm);
		UpdateKeywords(xitm);
		UpdateKeyCommands;
    UpdateOther(xitm);
	end;
  fEditor.LanguageManager.FillListWithLanguages(lexerforlangCB.Items,[sciLexers]);
  i:=FindStrInList(xitm.Lexer,lexerforlangCB.Items);
  if (i>=0) then
  begin
    lexerforlangCB.ItemIndex:=i;
  end;

	fOldEditorOptions.Assign(FEditorOptions);
end;

procedure TEdOptionsWindow.UpdateOther(curlng : TSciLangItem);
var
itm : TSciLangItem;
langstr : String;
i : Integer;
begin
  if curlng=nil then
  begin
    langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
    itm:=FEditorOptions.FLanguageManager.LanguageList.GetStyleList(langstr);
  end else
    itm:=curlng;
  commentBoxStart.Text:=itm.CommentBoxStart;
  commentBoxEnd.Text:=itm.CommentBoxEnd;
  commentBoxMiddle.Text:=itm.CommentBoxMiddle;
  commentStreamStart.Text:=itm.CommentStreamStart;
  commentStreamEnd.Text:=itm.CommentStreamEnd;
  commentBlock.Text:=itm.CommentBlock;
  AssignmentOperatorED.Text:=itm.AssignmentOperator;
  EndOfStatementED.Text:=itm.EndOfStatementOperator;
  NumStyleBitsED.Text:=ToStr(itm.NumStyleBits);
  i:=FindStrInList(itm.Lexer,lexerforlangCB.Items);
  if (i>=0) then
  begin
    lexerforlangCB.ItemIndex:=i;
  end;

end;

procedure TEdOptionsWindow.FormCreate(Sender: TObject);
begin
	fEditorOptions := TEditorOptions.Create;
	fOldEditorOptions := TEditorOptions.Create;
end;

procedure TEdOptionsWindow.FormDestroy(Sender: TObject);
begin
	fEditorOptions.Free;
	fOldEditorOptions.Free;
end;

procedure TEdOptionsWindow.ApplyBtnClick(Sender: TObject);
begin
    with fEditorOptions do
    begin
    Color:=defaultBackgroundCB.Selected;
		ReadOnly := ReadOnlyCB.Checked;
		BraceHilite := BraceHiliteCB.Checked;
		Gutter := GutterCB.Checked;
		WordWrap := TWordWrapType(WordWrapCB.ItemIndex);
		LineNumbers := LineNumbersCB.Checked;
		XSaveClearsUndo :=SaveClearsUndoCB.Checked;
		Indentation := [];
		if KeepIndentCB.Checked then
      Indentation := Indentation + [SciLexerMemo.KeepIndent];
		if TabIndentsCB.Checked then
      Indentation := Indentation + [SciLexerMemo.TabIndents];
		if BackSpaceUnindentsCB.Checked then
      Indentation := Indentation + [SciLexerMemo.BackSpaceUnindents];
		if IndentationGuidesCB.Checked then
      Indentation := Indentation + [SciLexerMemo.IndentationGuides];
    if CodeFoldingCB.Checked then
		CodeFolding := CodeFolding+[foldFold]
    else
    CodeFolding := CodeFolding-[foldFold];

		UseUnicode := UseUnicodeCB.Checked;
		TabWidth := ToInt(TabWidthSE.Text);
		IndentWidth := ToInt(IndentWidthSE.Text);
		EOLStyle := TEOLStyle(EOLStyleCB.ItemIndex);
		CaretFore :=CaretCB.Selected;
		SelFore :=SelForeCB.Selected;
		SelBack :=SelBackCB.Selected;
		FoldLo :=FoldLoCB.Selected;
		FoldHi :=FoldHiCB.Selected;
		MarkerFore:=MarkerForeCB.Selected;
		MarkerBack:=MarkerBackCB.Selected;
		BMarkFore :=BMarkForeCB.Selected;
		BMarkBack :=BMarkBackCB.Selected;

    HotActiveFore:=HotActiveForeCB.Selected;
    HotActiveBack :=HotActiveBackCB.Selected;
    HotActiveUnderline :=HotActiveUnderlineCB.Checked;
    HotActiveSingleLine :=HotActiveSingleLineCB.Checked;
		CaretWidth :=ToInt(CaretWidthSE.Text);
		CaretBack :=CaretBackCB.Selected;
		CaretLineVisible :=CaretLineVisCB.Checked;
    EdgeColor:=EdgeColorCB.Selected;
    MarkerType:=sciMarkerType(markerTypeCB.ItemIndex);
    EdgeColumn:=ToInt(EdgeColumnSE.Text);
    EdgeType:=sciEdgeType(EdgeTypeCB.ItemIndex);

		if Assigned(Editor) then
      SetOptions(FEditor);
	end;
end;

procedure TEdOptionsWindow.FontButtonClick(Sender: TObject);
begin
	FontDialog.Font := fEditorOptions.Font;
	if FontDialog.Execute then
		fEditorOptions.Font := FontDialog.Font;
end;

procedure TEdOptionsWindow.OKBtnClick(Sender: TObject);
begin
	ApplyBtnClick(Self);
end;

procedure TEdOptionsWindow.CancelBtnClick(Sender: TObject);
begin
// removed by hdalis.. No need to refresh the styles if nothing isn't changed.. I thing..
//	if not Assigned(Scintilla) then Exit;
//	fOldEditorOptions.SetOptions(FEditor);
end;

procedure TEdOptionsWindow.StylesLBClick(Sender: TObject);
Var
	SciStyle : TSciStyle;
  langstr : String;
begin
	if StylesLB.ItemIndex >= 0 then
  begin
		// To avoid getting into StyleElementChanged
		UpdatingStyle := True;

    langstr :=LanguageCB.Items[LanguageCB.ItemIndex];
    SciStyle := FEditorOptions.LanguageManager.LanguageList.GetStyleList(langstr).Styles.Items[StylesLB.ItemIndex] as TSciStyle;
		StyleNumberSE.Text := IntToStr(SciStyle.StyleNumber);
		DescriptionEB.Text := SciStyle.Name;
		if SciStyle.FontName = '' then
    begin
			FontCB.FontName := '';
      FontCB.Enabled:=False;
			DefaultFontCB.Checked := True;
		end else
    begin
			FontCB.FontName := SciStyle.FontName;
      FontCB.Enabled:=True;
			DefaultFontCB.Checked := False;
		end;
		SizeSE.Text := IntToStr(SciStyle.FontSize);
    if SciStyle.StyleNumber=STYLE_CONTROLCHAR then
    begin
      ForeCB.Enabled:=False;
      BackCB.Enabled:=False;
    end else
    begin
      if ForeCB.Enabled=False then ForeCB.Enabled:=True;
      if BackCB.Enabled=False then BackCB.Enabled:=True;

		  ForeCB.Selected := SciStyle.ForeColor;
		  ForeCB.Repaint;
		  BackCB.Selected := SciStyle.BackColor;
		  BackCB.Repaint;
    end;
		CaseCB.ItemIndex := Integer(SciStyle.CharCase);
		ItalicCB.Checked := fsItalic in SciStyle.FontStyles;
		BoldCB.Checked := fsBold in SciStyle.FontStyles;
		UnderlineCB.Checked := fsUnderline in SciStyle.FontStyles;
		VisibleCB.Checked := SciStyle.Visible;
		ChangeableCB.Checked := SciStyle.Changeable;
		EOLFilledCB.Checked := SciStyle.EOLFilled;
		HotspotCB.Checked := SciStyle.Hotspot;
		// Now set it to false
		UpdatingStyle := False;
	end;
end;

procedure TEdOptionsWindow.LanguageCBChange(Sender: TObject);
var
langstr : String;
itm : TSciLangItem;
begin
	langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
	fEditorOptions.LanguageManager.SelectedLanguage := langstr;
  itm:=fEditorOptions.LanguageManager.LanguageList.Find(langstr);
  UpdateStyles(itm);
  UpdateKeywords(itm);
  UpdateOther(itm);
end;

procedure TEdOptionsWindow.UpdateStyles(curlng : TSciLangItem);
Var
	i : Integer;
	itm : TSciLangItem;
	langstr : String;
begin
  langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
	with FEditorOptions do
  begin
		StylesLB.Items.BeginUpdate;
		try
			StylesLB.Items.Clear;
      if AnsiCompareText(langstr,scicontainerconst)=0 then
      begin
        highlighterPageCtrl.Enabled:=False;
        remLangButton.Enabled:=False;
        copyLangButton.Enabled:=False;

        StylesLB.Items.Add(sSetByCodeOnly);
        StylesLB.Items.Add(sSeeTheHelp);
      end else
      begin
        if (highlighterPageCtrl.Enabled=False) then
          highlighterPageCtrl.Enabled:=True;
        if remLangButton.Enabled=False then remLangButton.Enabled:=True;
        if copyLangButton.Enabled=False then copyLangButton.Enabled:=True;
        if assigned(curlng) then
          itm:=curlng
        else itm:=FLanguageManager.LanguageList.GetStyleList(langstr);
        for i := 0 to itm.Styles.Count - 1 do
          with itm.Styles.Items[i] as TSciStyle do
            StylesLB.Items.Add(Name);
      end;
		finally
      StylesLB.Items.EndUpdate;
    end;
    if AnsiCompareText(langstr,scicontainerconst)<>0 then
    begin
      if StylesLB.Count > 0 then
      begin
        StyleDefGB.Enabled := True;
        StylesLB.ItemIndex := 0;
        StylesLBClick(Self);
      end else
        StyleDefGB.Enabled := False;
    end;
  end;
end;

procedure TEdOptionsWindow.StyleElementChanged(Sender: TObject);
Var
	SciStyle : TSciStyle;
begin
	if UpdatingStyle then
    Exit;
	if StylesLB.ItemIndex >= 0 then
  begin
    SciStyle := FEditorOptions.LanguageManager.LanguageList.GetStyleList(LanguageCB.Items[LanguageCB.ItemIndex]).Styles.Items[StylesLB.ItemIndex] as TSciStyle;
		SciStyle.StyleNumber := ToInt(StyleNumberSE.Text);
		SciStyle.Name := DescriptionEB.Text;
		StylesLB.Items[StylesLB.ItemIndex] := DescriptionEB.Text;
		if DefaultFontCB.Checked=True then
    begin
			SciStyle.FontName := '';
      FontCB.Enabled:=False;
    end	else
    begin
			SciStyle.FontName := FontCB.FontName;
      FontCB.Enabled:=True;
    end;
		SciStyle.FontSize := ToInt(SizeSE.Text);
		SciStyle.ForeColor := ForeCB.Selected;
		SciStyle.BackColor := BackCB.Selected;
		SciStyle.CharCase := TSciCase(CaseCB.ItemIndex);
		SciStyle.FontStyles := [];
		if ItalicCB.Checked then
      SciStyle.FontStyles := SciStyle.FontStyles + [fsItalic];
		if BoldCB.Checked then
      SciStyle.FontStyles := SciStyle.FontStyles + [fsBold];
		if UnderlineCB.Checked then
      SciStyle.FontStyles := SciStyle.FontStyles + [fsUnderline];
		SciStyle.Visible := VisibleCB.Checked;
		SciStyle.Changeable := ChangeableCB.Checked;
		SciStyle.EOLFilled := EOLFilledCB.Checked;
		SciStyle.Hotspot := HotspotCB.Checked;
	end;
end;

procedure TEdOptionsWindow.DeleteStyleBClick(Sender: TObject);
var
itm : TSciLangItem;
begin
	if StylesLB.ItemIndex >= 0 then
  begin
    itm:=CurrentLang;
    (itm.Styles.Items[StylesLB.ItemIndex] as TSciStyle).Free;
		StylesLB.Items.Delete(StylesLB.ItemIndex);
		UpdateStyles(itm);
	end;
end;

procedure TEdOptionsWindow.AddStyleBClick(Sender: TObject);
var
  langstr : String;
  stylescnt : Integer;
  itm : TSciLangItem;

begin
  langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
  itm:=FEditorOptions.LanguageManager.LanguageList.GetStyleList(langstr);
  with itm.Styles.Add as TSciStyle do
  begin
    Name := sNewStyle;
    StyleNumber :=  itm.Styles.Count-1;
	end;
	UpdateStyles(itm);
  stylescnt:=StylesLB.Items.Count;
	StylesLB.Selected[stylescnt-1] := True;
	StylesLBClick(Self);
end;

procedure TEdOptionsWindow.UpdateKeywords(curlng : TSciLangItem);
Var
	i : Integer;
	itm : TSciLangItem;
  langstr : String;
begin
  langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
	with FEditorOptions do
  begin
		KeyListsLB.Items.BeginUpdate;
		try
			KeyListsLB.Items.Clear;
      if AnsiCompareText(langstr,scicontainerconst)=0 then
      begin
        highlighterPageCtrl.Enabled:=False;
        remLangButton.Enabled:=False;
        copyLangButton.Enabled:=False;
        KeylistsLB.Items.Add(sSetByCodeOnly);
        KeylistsLB.Items.Add(sSeeTheHelp);
      end else
      begin
        if (highlighterPageCtrl.Enabled=False) then
          highlighterPageCtrl.Enabled:=True;
        if remLangButton.Enabled=False then remLangButton.Enabled:=True;
        if copyLangButton.Enabled=False then copyLangButton.Enabled:=True;
        if assigned(curlng) then
          itm:=curlng
        else
          itm:=FLanguageManager.LanguageList.GetStyleList(langstr);
			  for i := 0 to itm.Keywords.Count - 1 do
			    with itm.Keywords.Items[i] as TSciKeywords do
				    KeyListsLB.Items.Add(Name);
      end;
		finally
			KeyListsLB.Items.EndUpdate;
		end;
    if AnsiCompareText(langstr,scicontainerconst)<>0 then
    begin
		  if KeyListsLB.Count > 0 then
      begin
			  KeywordsM.Enabled := True;
			  KeyListsLB.ItemIndex := 0;
			  KeyListsLBClick(Self);
		  end else
      begin
			  KeywordsM.Text := '';
			  KeywordsM.Enabled := False;
		  end;
    end;
	end;
end;

procedure TEdOptionsWindow.KeyListsLBClick(Sender: TObject);
var
	SciKeywords : TSciKeywords;
  itm : TSciLangItem;
begin
	if KeyListsLB.ItemIndex >= 0 then
  begin
		// To avoid getting into KeyListElementChanged
		UpdatingKeyList := True;
    itm:=CurrentLang;
    SciKeywords := itm.Keywords.Items[KeyListsLB.ItemIndex] as TSciKeywords;
		KeyListNumberSE.Text := IntToStr(SciKeywords.KeywordListNumber);
		KeyListDescriptionEB.Text := SciKeywords.Name;
{$Ifdef COMPILER6_UP}
		SciKeywords.Keywords.Delimiter := ' ';
		KeywordsM.Text := SciKeywords.Keywords.DelimitedText;
{$Else}
    KeywordsM.Text:=MergeStrings(SciKeywords.Keywords);
{$Endif}
		UpdatingKeyList := False;
	end;
end;

procedure TEdOptionsWindow.KeyListElementsChange(Sender: TObject);
Var
	SciKeywords : TSciKeywords;
begin
	if UpdatingKeyList then
    Exit;
	if KeyListsLB.ItemIndex >= 0 then
  begin
    SciKeywords := CurrentLang.Keywords.Items[KeyListsLB.ItemIndex] as TSciKeywords;
		SciKeywords.Name := KeyListDescriptionEB.Text;
    KeyListsLB.Items[KeyListsLB.ItemIndex] := KeyListDescriptionEB.Text;
		SciKeywords.KeywordListNumber := ToInt(KeyListNumberSE.Text);
{$Ifdef COMPILER6_UP}
		SciKeywords.Keywords.Delimiter := ' ';
		SciKeywords.Keywords.DelimitedText := KeywordsM.Text;
{$Else}
    SplitStrings(KeywordsM.Text,SciKeywords.Keywords);
{$Endif}
	end;
end;

procedure TEdOptionsWindow.KeyListDeleteClick(Sender: TObject);
begin
	if KeyListsLB.ItemIndex >= 0 then
  begin
    (CurrentLang.Keywords.Items[KeyListsLB.ItemIndex] as TSciKeywords).Free;
		KeyListsLB.Items.Delete(KeyListsLB.ItemIndex);
		UpdateKeywords;
	end;
end;

procedure TEdOptionsWindow.KeyListAddClick(Sender: TObject);
var
  langstr : String;
  itm : TSciLangItem;
begin
  langstr:=LanguageCB.Items[LanguageCB.ItemIndex];
  itm:=FEditorOptions.LanguageManager.LanguageList.GetStyleList(langstr);
  with itm.Keywords.Add as TSciKeywords do
  begin
    Name := sNewKeyList;
    KeywordListNumber:=  itm.Keywords.Count-1;
	end;
	UpdateKeywords(itm);
	KeyListsLB.Selected[KeyListsLB.Items.Count-1] := True;
	KeyListsLBClick(Self);
end;

procedure TEdOptionsWindow.UpdateKeyCommands;
Var
	i : Integer;
	KeyCommand : TSciKeyCommand;
	Item : TListItem;
	Ident : string;
begin
	KeyCmdList.Items.BeginUpdate;
	try
		KeyCmdList.Clear;
		for i := 0 to fEditorOptions.fKeyCommands.Count - 1 do
    begin
			KeyCommand := fEditorOptions.fKeyCommands.Items[i] as TSciKeyCommand;
			Item := KeyCmdList.Items.Add;
      Ident:='Unknown';
			IntToIdent(KeyCommand.Command, Ident, Sci_KeyboardCommandMap);
		  Item.Caption:=Ident;
      Item.SubItems.Add(ShortCutToText(KeyCommand.ShortCut));
			Item.Data := KeyCommand;
		end;
	finally
		KeyCmdList.Items.EndUpdate;
	end;
end;

procedure TEdOptionsWindow.btnDeleteClick(Sender: TObject);
begin
	if Assigned(KeyCmdList.Selected) then
  begin
		TSciKeyCommand(KeyCmdList.Selected.Data).Free;
		UpdateKeyCommands;
	end;
end;

procedure TEdOptionsWindow.btnResetClick(Sender: TObject);
begin
	fEditorOptions.fKeyCommands.ResetDefaultCommands;
	UpdateKeyCommands;
end;

procedure TEdOptionsWindow.btnEditClick(Sender: TObject);
Var
	KeyCommand : TSciKeyCommand;
begin
	if Assigned(KeyCmdList.Selected) then
  begin
		KeyCommand := TSciKeyCommand(KeyCmdList.Selected.Data);
    with TKeyEditForm.Create(Self) do
    begin
      cmbCommand.ItemIndex :=  cmbCommand.Items.IndexOf(KeyCmdList.Selected.Caption);
      HotKey.HotKey := KeyCommand.ShortCut;
      HotKey.Modifiers := HotKey.Modifiers + [hkExt];
      if (ShowModal = mrOK) and (cmbCommand.ItemIndex >= 0) and (HotKey.HotKey <> 0) then
      begin
        KeyCommand.ShortCut := HotKey.HotKey;
        KeyCommand.Command :=  Integer(cmbCommand.Items.Objects[cmbCommand.ItemIndex]);
        UpdateKeyCommands;
      end;
      Release;
    end;
  end;
end;


procedure TEdOptionsWindow.btnAddClick(Sender: TObject);
Var
  KeyCommand : TSciKeyCommand;
begin
    with TKeyEditForm.Create(Self) do
    begin
      if (ShowModal = mrOK) and (cmbCommand.ItemIndex >= 0) and (HotKey.HotKey <> 0) then
      begin
        KeyCommand := fEditorOptions.fKeyCommands.FindShortCut(HotKey.HotKey);
        if not Assigned(KeyCommand) then
          KeyCommand := fEditorOptions.fKeyCommands.Add as TSciKeyCommand;
        KeyCommand.ShortCut := HotKey.HotKey;
        KeyCommand.Command :=  Integer(cmbCommand.Items.Objects[cmbCommand.ItemIndex]);
        UpdateKeyCommands;
			end;
      Release;
    end;
end;

procedure TEdOptionsWindow.TestNumericOnly(Sender: TObject; var Key: Char);
begin
  if (Key>#31) and ((Key<'0') or (Key>'9')) then Key:=#0;
end;

procedure TEdOptionsWindow.addLangButtonClick(Sender: TObject);
var
  itm : TSciLangItem;
  itmpos : Integer;
begin
  sciAddLanguageForm:=TsciAddLanguageForm.Create(self);
  FEditorOptions.FLanguageManager.FillListWithLanguages(sciAddLanguageForm.LexerToUseCB.Items,[sciLexers]);
  sciAddLanguageForm.LexerToUseCB.ItemIndex:=sciAddLanguageForm.LexerToUseCB.Items.IndexOf('null');
  if sciAddLanguageForm.ShowModal=mrOk then
  begin
    itm:=FEditorOptions.FLanguageManager.AddLanguage(sciAddLanguageForm.languagename.Text,sciAddLanguageForm.LexerToUseCB.Items[sciAddLanguageForm.LexerToUseCB.ItemIndex]);
    if itm<>nil then
    begin
      itmpos:=LanguageCB.Items.Add(sciAddLanguageForm.languagename.Text);
      if itmpos>=0 then
      begin
        LanguageCB.ItemIndex:=itmpos;
        LanguageCBChange(self);
        if (LanguageCB.Items.Count>0) then
          highlighterPageCtrl.Enabled:=True
        else
          highlighterPageCtrl.Enabled:=False;
      end;
    end else
      raise Exception.Create(sCouldntAddTheLanguage);
  end;
end;

procedure TEdOptionsWindow.remLangButtonClick(Sender: TObject);
var
  itmpos : Integer;
  langstr : String;
  notinstat : Boolean;
begin
  itmpos:=LanguageCB.ItemIndex;
  if itmpos=-1 then Exit;
  langstr:=LanguageCB.Items[itmpos];
  if AnsiCompareText(langstr,scicontainerconst)=0 then
  begin
    raise Exception.Create(sCantRemoveLanguage);
    Exit;
  end;

  if(FEditorOptions.FLanguageManager.RemoveLanguage(langstr,notinstat)=True) then
  begin
    if notinstat=True then //if not a static lexer
    begin
      try
        LanguageCB.Items.BeginUpdate;
        LanguageCB.Items.Delete(itmpos);
        if itmpos>=LanguageCB.Items.Count then
          itmpos:=(LanguageCB.Items.Count-1)
        else
          Dec(itmpos);
        LanguageCB.ItemIndex:=itmpos;
        LanguageCBChange(self);
      finally
        LanguageCB.Items.EndUpdate;
      end;
      if (LanguageCB.Items.Count>0) then
        highlighterPageCtrl.Enabled:=True
      else
        highlighterPageCtrl.Enabled:=False;

    end else //it was a static lexer, just update the styles and keywords.
    begin
	    UpdateStyles;
	    UpdateKeywords;
    end;
  end;

end;

procedure TEdOptionsWindow.copyLangButtonClick(Sender: TObject);
var
  srclang,langstr : String;
  itm : TSciLangItem;
begin
  srclang:=LanguageCB.Items[LanguageCB.ItemIndex];
  if AnsiCompareText(srclang,scicontainerconst)=0 then
  begin
    raise Exception.Create(sCantCopyLanguage);
    Exit;
  end;

  itm:=FEditorOptions.FLanguageManager.DupLanguage(srclang);
  if itm<>nil then
  begin
    langstr:=sLMLanguage+'['+IntToStr(itm.ID)+']';
    if(InputQuery(Format(sCopyLanguageQueryTitle,[srclang]),sNameOfNewLanguage,langstr)) then
    begin
    itm.Name:=langstr;
    LanguageCB.ItemIndex:=LanguageCB.Items.Add(itm.Name);
    LanguageCBChange(self);
    end else
    FEditorOptions.FLanguageManager.LanguageList.Delete(itm.Index);
  end else
  raise Exception.CreateResFmt(@sCouldntCopyLangTo,[srclang,langstr]);
end;

procedure TEdOptionsWindow.OptionPagesChange(Sender: TObject);
begin
  if OptionPages.Showing then
  begin
    case OptionPages.ActivePageIndex of
      0: GutterCB.SetFocus;
      1: SelForeCB.SetFocus;
      2: LanguageCB.SetFocus;
      3: KeyCmdList.SetFocus;
    end;
  end;
end;

procedure   TEdOptionsWindow.RescanLanguageList;
var
cnt,i : Integer;
begin
  LanguageCB.Items.Clear;
  cnt :=FEditor.LanguageManager.LanguageList.Count;
  for i:=0 to (cnt-1) do
  begin
    LanguageCB.Items.Add(TSciLangItem(FEditor.LanguageManager.LanguageList.Items[i]).Name);
  end;
  if (LanguageCB.Items.IndexOf(scicontainerconst)=-1) then
  begin
    LanguageCB.Items.Add(scicontainerconst);
  end;

end;

procedure TEdOptionsWindow.KeyCmdListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=CompStr(Item1.Caption,Item2.Caption);
end;

procedure TEdOptionsWindow.AssignmentOperatorEDChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.AssignmentOperator:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.EndOfStatementEDChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.EndOfStatementOperator:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.commentBoxStartChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentBoxStart:=TEdit(Sender).Text;

end;

procedure TEdOptionsWindow.commentBoxMiddleChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentBoxMiddle:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.commentBoxEndChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentBoxEnd:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.commentBlockChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentBlock:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.commentStreamStartChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentStreamStart:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.commentStreamEndChange(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.CommentStreamEnd:=TEdit(Sender).Text;
end;

procedure TEdOptionsWindow.lexerforlangCBExit(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  if itm.Lexer<>lexerforlangCB.Text then
    itm.Lexer:=lexerforlangCB.Text;
end;

procedure TEdOptionsWindow.NumStyleBitsEDExit(Sender: TObject);
var
  itm : TSciLangItem;
begin
  itm:=CurrentLang;
  itm.NumStyleBits:=ToInt(TEdit(Sender).Text);
end;

procedure TEdOptionsWindow.LoadKeyCommands1Click(Sender: TObject);
begin
  if odia.Execute then
  begin
    LoadKeyCommands(fEditorOptions.fKeyCommands,odia.FileName);
    UpdateKeyCommands;
  end;
end;

procedure TEdOptionsWindow.SaveKeycommands1Click(Sender: TObject);
begin
  if sdia.Execute then
    SaveKeyCommands(fEditorOptions.fKeyCommands,sdia.FileName);
end;

end.
