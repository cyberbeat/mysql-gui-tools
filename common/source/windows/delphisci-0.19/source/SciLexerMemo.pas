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
  Author : hdalis
      $Id: scilexermemo.pas,v 1.5 2004/11/13 04:29:51 hdalis Exp $
  History: See SciLexerMod.Pas for the initial history
           19/11/2004 Initial Release
           08/01/2005 Removed CharAddedAutocomplete.. no longer used.
                      Added AutoCloseBraces,AutoCloseQuotes.
           04/03/2005 HasPrefix was changed to use AnsiCompare* instead of
                      StrL[I]Comp as the other caused some errors
           15/04/2005 Removed the Add*Handler and Remove*Handler, and replaced them
                      with AddHandler and RemoveHandler wich takes a enum parameter
                      to tell which type handler it is.

                      Fixed the doSci* functions to no longer call Execute*Handler
                      as the inherited TScintillaBase calls it via the events..
                      Earlier it caused the handlers to be called twice (or more), if there
                      was any handlers assigned via TSciController. A bug..
					 20/04/2005	Added LocateIndicator, LengthOfIndicator, RemoveIndicator,ShowIndicatorAt.
											
										
}
{$Include SciCommonDef.Inc}
unit SciLexerMemo;
interface
uses Classes, Windows, Controls, Messages, SysUtils, Graphics,SciLexer,SciKeyBindings,
SciSupport,SciControllerHandler;

type
	sciWordWrapVisualFlag = (sciWWEnd,sciWWStart);
	sciWordWrapVisualFlags = set of sciWordWrapVisualFlag;
	sciWordWrapVisualFlagLocation = (sciEndByText,sciStartByText);
	sciWordWrapVisualFlagLocations = set of sciWordWrapVisualFlagLocation;
	TMarginType = (gutSymbol, gutLineNumber);
  
  // Display whitespace?
  sciWSMode = (sciWsInvisible,sciWsVisibleAlways,sciWsVisibleAfterIndent);
	sciCacheType =(sciCacheNone,sciCacheCaret,sciCachePage,sciCacheDocument);
	sciEdgeType =(sciEdgeNone,sciEdgeLine,sciEdgeBackground);
  TIndentationOption = (KeepIndent, TabIndents, BackSpaceUnIndents, IndentationGuides);
	TIndentationOptions = Set of TIndentationOption;
  TEOLStyle = (eolCRLF, eolCR, eolLF);
	TScintillaMemo =class;
 

  TMargin = class(TPersistent)
  private
    fScintilla : TScintillaBase;
    fNumber    : Integer;
		function  GetWidth: Integer;
		procedure SetWidth(const Value: Integer);
		function  GetMarginType: TMarginType;
		procedure SetMarginType(const Value: TMarginType);
	public
		constructor Create(Scintilla : TScintillaBase; Number : Integer);
		procedure   Assign(Source: TPersistent); override;
  published
		property Width      : Integer     read GetWidth      write SetWidth;
		property MarginType : TMarginType read GetMarginType write SetMarginType;
	end;

	TCaret = class(TPersistent)
	private
		fScintilla : TScintillaBase;
		function  GetCaretFore: TColor;
		procedure SetCaretFore(const Value: TColor);
		function  GetCaretLineBack: TColor;
		procedure SetCaretLineBack(const Value: TColor);
		function  GetCaretLineVisible : Boolean;
		procedure SetCaretLineVisible(const Value : Boolean);
		function  GetCaretWidth : LongInt;
		procedure SetCaretWidth(const Value : LongInt);
		procedure SetCaretPeriod(const Value : LongInt);
		function  GetCaretPeriod : LongInt;
	public
		constructor Create(Scintilla : TScintillaBase);
		procedure   Assign(Source: TPersistent); override;
	published
		property ForeColor        : TColor   read GetCaretFore        write SetCaretFore;
		property LineBackColor    : TColor   read GetCaretLineBack    write SetCaretLineBack;
		property LineVisible      : Boolean read GetCaretLineVisible write SetCaretLineVisible;
		property Width            : LongInt  read GetCaretWidth       write SetCaretWidth;
		property Period           : LongInt  read GetCaretPeriod      write SetCaretPeriod;
  end;

	TSciHotSpot = class(TPersistent)
	private
		fScintilla : TScintillaBase;
    FHotActiveFore : TColor;
    FHotActiveBack : TColor;
    FHotActiveUnderline,
    FHotActiveSingleLine : Boolean;
		procedure SetHotActiveFore(const Value : TColor);
		procedure SetHotActiveBack(const Value : TColor);
    procedure SetHotActiveUnderline(Value : Boolean);
    procedure SetHotActiveSingleLine(Value : Boolean);
  public
		constructor Create(Scintilla : TScintillaBase);
		procedure   Assign(Source: TPersistent); override;
  published
		property BackColor : TColor read FHotActiveBack Write SetHotActiveBack nodefault;
		property ForeColor : TColor read FHotActiveFore Write SetHotActiveFore nodefault;
    property Underlined : Boolean read FHotActiveUnderline write SetHotActiveUnderline;
    property SingleLine : Boolean read FHotActiveSingleLine write SetHotActiveSingleLine;
  end;


	TSciColors = class(TPersistent)
	private
		fScintilla : TScintillaBase;
		FForeSelColor : TColor;
		FBackSelColor : TColor;
		FBookMarkBackColor : TColor;
		FBookMarkForeColor : TColor;
		FMarkerFore : TColor;
		FMarkerBack : TColor;
		FFoldHiColor : TColor;
		FFoldLoColor : TColor;

		procedure SetForeSel(const Value : TColor);
		procedure SetBackSel(const Value : TColor);
		procedure SetMarkerFore(const Value : TColor);
		procedure SetMarkerBack(const Value : TColor);
		procedure SetFoldHi(const Value : TColor);
		procedure SetFoldLo(const Value : TColor);
		procedure SetBookMarkFore(const Value : TColor);
		procedure SetBookMarkBack(const Value : TColor);
	public
		constructor Create(Scintilla : TScintillaBase);
		procedure Assign(Source: TPersistent); override;
	published
		property SelFore : TColor read FForeSelColor write SetForeSel nodefault;
		property SelBack : TColor read FBackSelColor write SetBackSel nodefault;
		property MarkerFore : TColor read FMarkerFore write SetMarkerFore nodefault;
		property MarkerBack : TColor read FMarkerBack write SetMarkerBack nodefault;
		property FoldHi : TColor read FFoldHiColor write SetFoldHi nodefault;
		property FoldLo : TColor read FFoldLoColor write SetFoldLo nodefault;
		property BookMarkBack : TColor read FBookMarkBackColor Write SetBookMarkBack nodefault;
		property BookMarkFore : TColor read FBookMarkForeColor Write SetBookMarkFore nodefault;
  end;

	TDivOptions = class(TPersistent)
	private
		fScintilla : TScintillaBase;
		function  GetBool(const Index : Integer) : Boolean;
		procedure SetBool(const Index : Integer;const Value : Boolean);
    function  GetWSMode : sciWSMode;
    procedure SetWSMode(value : sciWSMode);
	public
		constructor Create(Scintilla : TScintillaBase);
		procedure   Assign(Source: TPersistent); override;
	published
		property ViewWSpace : sciWSMode read GetWSMode write SetWSMode;
		property UsePalette : Boolean index 0 read GetBool write SetBool;
		property OverType : Boolean index 1 read GetBool write SetBool;
		property ViewEOL : Boolean index 2 read GetBool write SetBool;
		property EndAtLastLine : Boolean index 3 read GetBool write SetBool;
		property ScrollBarH : Boolean index 4 read GetBool write SetBool;
		property ScrollBarV : Boolean index 5 read GetBool write SetBool;
	end;

TScintillaMemo = class(TScintillaBase)
  private
		FLines : TStrings;
		fKeepIndent : Boolean;
		fCaret : TCaret;
		fColors : TSciColors;
		fDivOptions : TDivOptions;
    fHotSpot : TSciHotSpot;
		fKeyCommands : TSciKeyCommandCollection;
		FHideSelect : Boolean;
		fMargin0 : TMargin;
    fMargin1 : TMargin;
		fMargin2 : TMargin;
    FAutoCloseQuotes,FAutoCloseBraces : Boolean;
    FController : TSciController;
		// Property getters/setters
		procedure SetKeyCommands(const Value : TSciKeyCommandCollection);
		procedure SetLines(Const Value : TStrings);
		function  GetEOLStyle: TEOLStyle;
		procedure SetEOLStyle(const Value: TEOLStyle);
		function 	GetIndentation: TIndentationOptions;
		procedure SetIndentation(const Value: TIndentationOptions);
		function  GetWordWrapVisual : sciWordWrapVisualFlags;
		procedure SetWordWrapVisual(const flags : sciWordWrapVisualFlags);
		function  GetWordWrapVisualLoc : sciWordWrapVisualFlagLocations;
		procedure SetWordWrapVisualLoc(const flags : sciWordWrapVisualFlagLocations);
		procedure SetLCache(const value : sciCacheType);
		function  GetLCache : sciCacheType;
		procedure SetHideSelect(const value : Boolean);
		procedure inSetEdgeMode(const value : sciEdgeType);
		function  inGetEdgeMode : sciEdgeType;
		procedure SetMargins(const Index: Integer; const Value: TMargin);
    procedure InSetControlCharSymbol(const Value : Char);
    function  InGetControlCharSymbol : Char;

protected
		procedure MaintainIndentation(ch : Integer);
		procedure doSciCharAdded(const ch : Integer); override;
		procedure doSciCalltipClick(const position : Integer);override;
    procedure doSciMacroRecord(const msg : Integer;const wParam : uptr_t;const lParam : sptr_t);override;
    procedure doSciAutoCSelection(const text : PChar);override;
		procedure Loaded; override;
public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CopyFrom(Source: TScintillaBase); override;

		// Define a marker
		procedure DefineMarker(MarkNum, Marker : Integer;ForeColor : TColor=clDefault;BackColor : TColor=clDefault);
		procedure BookmarkAdd(lineno : LongInt=-1);                 // Adds the marker SCITE_MARKER_BOOKMARK on the line 'lineno'
		procedure BookmarkDelete(lineno : LongInt=-1);              // Deletes the marker SCITE_MARKER_BOOKMARK on the line 'lineno'
		procedure BookmarkToggle(lineno : LongInt=-1);              // Toggles the marker SCITE_MARKER_BOOKMARK on the line 'lineno'
		function  BookmarkPresent(lineno : LongInt=-1) : Boolean;   // Tests if SCITE_MARKER_BOOKMARK marker is on the line 'lineno'
		function  BookmarkNext(forwardScan : Boolean=True) : Boolean;
    procedure AutoAdjustLineNumberWidth;
    procedure StripTrailingSpaces;

    {These functions attach/detach other components that needs to detect keys,click on calltips or macrorecording.}
    procedure AddHandler(aHandler : Pointer;HandlerType : TControllerHandlerType);
    procedure RemoveHandler(aHandler : Pointer;HandlerType : TControllerHandlerType);
public
    function LocateIndicator(indicatorNumber,startSearchAtPos : Integer): Integer;
    function LengthOfIndicator(indicatorNumber,atPos: Integer): Integer;
    procedure RemoveIndicator(indicatorNumber,atPos : Integer);
    procedure ShowIndicatorAt(indicatorNumber,atPos,Length: Integer;Visible: Boolean=True);
		//Property for customising and saving the keyboard commands
    property KeyCommands : TSciKeyCommandCollection read FKeyCommands write SetKeyCommands;

published
    property OnStyleNeeded;
    property OnCharAdded;
    property OnSavePointReached;
    property OnSavePointLeft;
    property OnModifyAttemptRO;
{Don't know if this is really needed, since the standard OnDblClick also fires whenever the
control is doubleclicked at runtime, so it's commented out.}
    property OnDoubleClick;
    property OnUpdateUI;
    property OnModified;
		property OnMacroRecord;
    property OnMarginClick;
		property OnNeedShown;
    property OnPainted;
    property OnDblClick;
    property OnUserListSelection;
    property OnDwellStart;
    property OnDwellEnd;
    property OnZoom;
		property OnHotSpotClick;
    property OnHotSpotDoubleClick;
		property OnCallTipClick;
		property OnMsgSent;
		property OnKeyUp;
		property OnKeyDown;
		property OnKeyPress;
		property OnEnter;
		property OnExit;
		// Events End
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnAutoCSelection;
		property  Lines : TStrings read FLines write SetLines;
		//Get or set the EOF style: EOL_CRLF, EOL_CR, EOL_LF
		property EOLStyle : TEOLStyle read GetEOLStyle write SetEOLStyle;
		//Get or set indentation options: - KeepIndent, - TabIndents, - BackSpaceUnIndents, - IndentationGuides
		property Indentation : TIndentationOptions read GetIndentation write SetIndentation;
		//Number of characters for code (un)indending.	If set to zero then the TabWidth is used instead
		property IndentWidth : Integer read GetIndent write SetIndent;
		//Left Margin in pixels
		property MarginLeft : LongInt read GetMarginLeft write SetMarginLeft;
		//Right Margin in pixels
		property MarginRight : LongInt read GetMarginRight write SetMarginRight;
		//Tab width in characters
    property TabWidth : Integer read GetTabWidth write SetTabWidth default 8;
		property ReadOnly;
		//If false tab characters are replaced by spaces
    property UseTabs : Boolean read GetUseTabs write SetUseTabs default True;
		//Setting to true allows the use of multiple language in the same document.If true all text in and out of the control is interpreted as UTF8
    property UseUnicode;
		property Caret : TCaret read fCaret Write fCaret;
		property DivOptions : TDivOptions read fDivOptions write fDivOptions;
    property ActiveHotSpot : TSciHotSpot read fHotSpot write fHotSpot;
		property Colors : TSciColors read fColors write fColors;
		property MouseDwellTime : LongInt read GetMouseDwellTime write SetMouseDwellTime default SC_TIME_FOREVER;
		property ClearUndoAfterSave;
		//Set the type of Gutter(margin) 0: gutSymbol = 0,gutLineNumber = 1
		property Gutter0 : TMargin  index 0 read fMargin0 write SetMargins;
		//Set the type of Gutter(margin) 1: gutSymbol = 0,gutLineNumber = 1
    property Gutter1 : TMargin index 1 read fMargin1 write SetMargins;
		//Set the type of Gutter(margin) 2: gutSymbol = 0,gutLineNumber = 1
    property Gutter2 : TMargin index 2 read fMargin2 write SetMargins;

		//Enable/disable word wrap
		property WordWrapVisualFlags : sciWordWrapVisualFlags read GetWordWrapVisual write SetWordWrapVisual;
		property WordWrapVisualFlagsLocation : sciWordWrapVisualFlagLocations read GetWordWrapVisualLoc write SetWordWrapVisualLoc;
		property LayoutCache : sciCacheType read GetLCache write SetLCache;
		property HideSelect : Boolean read FHideSelect write SetHideSelect;
		property WordWrap;
		property EdgeMode : sciEdgeType read inGetEdgeMode write inSetEdgeMode default sciEdgeNone;
		property EdgeColumn : LongInt read GetEdgeColumn write SetEdgeColumn default 0;
		property EdgeColor : TColor read GetEdgeColour write SetEdgeColour default clDefault;
		property WordChars;
    property AutoCloseQuotes : Boolean read FAutoCloseQuotes write FAutoCloseQuotes default False;
    property AutoCloseBraces : Boolean read FAutoCloseBraces write FAutoCloseBraces default False;
    property ControlCharSymbol : Char read InGetControlCharSymbol write InSetControlCharSymbol;
end;


procedure FillMatching(const startwith : String;var deststr : String;fromlist : TStrings;const ignorecase : Boolean);
function HasPrefix(const s : AnsiString;const prefix : AnsiString;const ignorecase : Boolean) : Boolean;
implementation
uses Math,sciUtils,SciResLang;

const
  DefaultLinenumbersWidth = 4;

//////  Beginning of TScintillaStrings

Type
{ TScintillaStrings }
  TScintillaStrings = class(TStrings)
  private
    Memo: TScintillaMemo;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetTextStr: string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure SetTextStr(const Value: string); override;
		procedure SetUpdateState(Updating: Boolean); override;
  public 
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  
function TScintillaStrings.GetCount: Integer;
begin
  Result := 0;
  if Memo.HandleAllocated then
  begin
    Result := Memo.GetLineCount;
    if Memo.GetLineEndPosition(Result-1) - Memo.PositionFromLine(Result-1) = 0 then Dec(Result);
  end;
end;

function TScintillaStrings.Get(Index: Integer): string;
var
  S : string;
  Len : Integer;
begin
  Len := Memo.GetLineEndPosition(Index)-Memo.PositionFromLine(Index);
  if Len > 0 then
  begin
    SetLength(S, Len+2);
    Memo.GetLine(Index, PChar(S));
    Result := Copy(S, 1, Len);
  end else
    Result := '';
end;

procedure TScintillaStrings.Put(Index: Integer; const S: string);
var
  SelStart: Integer;
begin
  SelStart := Memo.PositionFromLine(Index);
  if SelStart >= 0 then
	begin
    Memo.SetTargetStart(SelStart);
    Memo.SetTargetEnd(Memo.GetLineEndPosition(Index));
    Memo.ReplaceTarget(-1, PChar(S));
  end;
end;

procedure TScintillaStrings.Insert(Index: Integer; const S: string);
var
  SelStart, LineLen: Integer;
  Line: string;
  EndOfLine : string;
begin
  if Index >= 0 then
  begin
    Case Memo.GetEOLMode of
      SC_EOL_CRLF : EndOfLine := CrLf;
      SC_EOL_CR   : EndOfLine := #13;
      SC_EOL_LF : EndOfLine := #10;
    end;
    SelStart := Memo.PositionFromLine(Index);
    if SelStart >= 0 then 
      Line := S + EndOfLine 
    else
    begin
      SelStart := Memo.PositionFromLine(Index-1);
      if SelStart < 0 then 
        Exit;
      LineLen := Memo.GetLineEndPosition(Index-1) - SelStart;
      if LineLen = 0 then 
        Exit;
      Inc(SelStart, LineLen);
      Line := EndOfLine + s;
    end;
    Memo.SetTargetStart(SelStart);
    Memo.SetTargetEnd(SelStart);
    Memo.ReplaceTarget(-1, PChar(Line));
  end;
end;

procedure TScintillaStrings.Delete(Index: Integer);
const
  Empty: PChar = '';
var
  SelStart, SelEnd: Integer;
begin
  SelStart := Memo.PositionFromLine(Index);
  if SelStart >= 0 then
  begin
    SelEnd := Memo.PositionFromLine(Index+1);
    if SelEnd < 0 then SelEnd := SelStart + Memo.LineLength(Index);
    Memo.SetTargetStart(SelStart);
    Memo.SetTargetEnd(SelEnd);
    Memo.ReplaceTarget(-1, Empty);
  end;
end;

procedure TScintillaStrings.Clear;
begin
  Memo.ClearAll;
end;

procedure TScintillaStrings.SetUpdateState(Updating: Boolean);
begin
  if Memo.HandleAllocated then
  begin
    SendMessage(Memo.Handle, WM_SETREDRAW, Ord(not Updating), 0);
    if not Updating then
    begin   // WM_SETREDRAW causes visibility side effects in memo controls
      Memo.Perform(CM_SHOWINGCHANGED,0,0); // This reasserts the visibility we want
      Memo.Refresh;
    end;
  end;
end;

function TScintillaStrings.GetTextStr: string;
begin
  Result := Memo.Text;
end;


procedure TScintillaStrings.SetTextStr(const Value: string);
var
  NewText: string;
{$Ifndef COMPILE_BCB5}
  Style: TTextLineBreakStyle;
{$Endif}
begin
{$Ifndef COMPILE_BCB5}
  Case Memo.GetEOLMode of
    SC_EOL_CRLF : Style := tlbsCRLF;
    SC_EOL_LF : Style := tlbsLF;
  else
    Style := tlbsCRLF;   // no other line break style available
  end;
  NewText := AdjustLineBreaks(Value, Style);
{$Else}
  NewText := AdjustLineBreaks(Value);
{$Endif}
  if (Length(NewText) <> Memo.GetTextLength) or (NewText <> Memo.Text) then
  begin
    Memo.SetText(PChar(NewText));
    Memo.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

//////  End of TScintillaStrings
constructor TScintillaMemo.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
  FLines := TScintillaStrings.Create;
  TScintillaStrings(FLines).Memo := Self;
	fCaret :=TCaret.Create(Self);
	fColors :=TSciColors.Create(Self);
	fDivOptions :=TDivOptions.Create(Self);
  fHotSpot :=TSciHotSpot.Create(Self);
  fMargin0 := TMargin.Create(Self, 0);
  fMargin1:= TMargin.Create(Self, 1);
  fMargin2:= TMargin.Create(Self, 2);
	fKeyCommands := TSciKeyCommandCollection.Create(Self);
  FController:=TSciController.Create(Self);
	FHideSelect:=false;
  FAutoCloseBraces:=False;
  FAutoCloseQuotes:=False;
end;

destructor TScintillaMemo.Destroy;
begin
  FLines.Free;
	fCaret.Free;
  fColors.Free;
  fDivOptions.Free;
  fHotSpot.Free;
  fMargin0.Free;
  fMargin1.Free;
  fMargin2.Free;
  fKeyCommands.Free;
  FController.Free;
  inherited;
end;

procedure TScintillaMemo.AutoAdjustLineNumberWidth;
var
  lineCount,lineNumWidth,pixelWidth : Integer;
begin
	lineNumWidth := 1;
  lineCount:=GetLineCount;
  while (lineCount >= 10) do
  begin
    lineCount := lineCount div 10;
    Inc(lineNumWidth);
  end;
  if (lineNumWidth < DefaultLinenumbersWidth) then
    lineNumWidth := DefaultLinenumbersWidth;

  pixelWidth := 4 + lineNumWidth * TextWidth(STYLE_LINENUMBER, '9');
  Gutter0.Width:=pixelWidth;
end;

procedure TScintillaMemo.AddHandler(aHandler : Pointer;HandlerType : TControllerHandlerType);
begin
  case HandlerType of
    CharAddedHandler: FController.AddCharAddedHandler(TCharAddedProc(aHandler));
    MacroRecordHandler: FController.AddMacroRecordHandler(TMacroRecordProc(aHandler));
    CallTipClickHandler: FController.AddCallTipClickHandler(TCallTipClickProc(aHandler));
    AutoCSelectionHandler: FController.AddAutoCSelectionHandler(TAutoCSelectionProc(aHandler));
  end;
end;
procedure TScintillaMemo.RemoveHandler(aHandler : Pointer;HandlerType : TControllerHandlerType);
begin
  case HandlerType of
    CharAddedHandler: FController.RemoveCharAddedHandler(TCharAddedProc(aHandler));
    MacroRecordHandler: FController.RemoveMacroRecordHandler(TMacroRecordProc(aHandler));
    CallTipClickHandler: FController.RemoveCallTipClickHandler(TCallTipClickProc(aHandler));
    AutoCSelectionHandler: FController.RemoveAutoCSelectionHandler(TAutoCSelectionProc(aHandler));
  end;
end;

procedure TScintillaMemo.CopyFrom(Source: TScintillaBase);
begin
	inherited;
	if Source is TScintillaMemo then
	begin
		EOLStyle:=TScintillaMemo(Source).EOLStyle;
		Indentation:=TScintillaMemo(Source).Indentation;
		IndentWidth:=TScintillaMemo(Source).IndentWidth;
		KeyCommands.Assign(TScintillaMemo(Source).KeyCommands);
		MarginLeft:=TScintillaMemo(Source).MarginLeft;
		MarginRight:=TScintillaMemo(Source).MarginRight;
		TabWidth:=TScintillaMemo(Source).TabWidth;
		UseTabs:=TScintillaMemo(Source).UseTabs;
		Caret.Assign(TScintillaMemo(Source).Caret);
		DivOptions.Assign(TScintillaMemo(Source).DivOptions);
		Colors.Assign(TScintillaMemo(Source).Colors);
		MouseDwellTime:=TScintillaMemo(Source).MouseDwellTime;
		WordWrap:=TScintillaMemo(Source).WordWrap;
		WordWrapVisualFlags:=TScintillaMemo(Source).WordWrapVisualFlags;
		WordWrapVisualFlagsLocation:=TScintillaMemo(Source).WordWrapVisualFlagsLocation;
		LayoutCache:=TScintillaMemo(Source).LayoutCache;
		HideSelect:=TScintillaMemo(Source).HideSelect;
		EdgeColor:=TScintillaMemo(Source).EdgeColor;
		EdgeColumn:=TScintillaMemo(Source).EdgeColumn;
		EdgeMode:=TScintillaMemo(Source).EdgeMode;
    ActiveHotSpot.Assign(TScintillaMemo(Source).ActiveHotSpot);
 		Gutter0.Assign(TScintillaMemo(Source).Gutter0);
		Gutter1.Assign(TScintillaMemo(Source).Gutter1);
		Gutter2.Assign(TScintillaMemo(Source).Gutter2);
	end;
end;

procedure TScintillaMemo.inSetEdgeMode(const value : sciEdgeType);
begin
	SetEdgeMode(LongInt(value));
end;
function  TScintillaMemo.inGetEdgeMode : sciEdgeType;
begin
	Result:=sciEdgeType(GetEdgeMode);
end;

procedure TScintillaMemo.SetHideSelect(const value : Boolean);
begin
	FHideSelect:=value;
	inherited HideSelection(value);
end;

procedure TScintillaMemo.DefineMarker(MarkNum, Marker : Integer;ForeColor,BackColor : TColor);
begin
	MarkerDefine(MarkNum, Marker);
	if(ForeColor=clDefault) then
		MarkerSetFore(MarkNum, Colors.MarkerFore)
	else
		MarkerSetFore(MarkNum, ForeColor);

	if(BackColor=clDefault) then
		MarkerSetBack(MarkNum, Colors.MarkerBack)
	else
		MarkerSetBack(MarkNum, BackColor)
end;

procedure TScintillaMemo.Loaded;
begin
	inherited;
	DefineMarker(SCITE_MARKER_BOOKMARK,SC_MARK_CIRCLE,Colors.BookMarkFore,Colors.BookMarkBack);
  if assigned(FController) then
    FController.Connect;
end;

procedure TScintillaMemo.SetLCache(const value : sciCacheType);
begin
	SetLayoutCache(LongInt(value));
end;
function  TScintillaMemo.GetLCache : sciCacheType;
begin
	result :=sciCacheType(GetLayoutCache);
end;

function TScintillaMemo.GetWordWrapVisual : sciWordWrapVisualFlags;
var
  tmp : sciWordWrapVisualFlags;
  x : LongInt;
begin
	tmp:=[];
	x:=GetWrapVisualFlags;
	if (Boolean(x and SC_WRAPVISUALFLAG_END)) then Include(tmp,sciWWEnd)
		else Exclude(tmp,sciWWEnd);
	if (Boolean(x and SC_WRAPVISUALFLAG_START)) then Include(tmp,sciWWStart)
		else Exclude(tmp,sciWWStart);
	result :=tmp;
end;

procedure TScintillaMemo.SetWordWrapVisual(const flags : sciWordWrapVisualFlags);
var
  lflags : LongInt;
begin
	lflags:=0;
	if sciWWEnd in flags then lflags:=lflags+SC_WRAPVISUALFLAG_END;
	if sciWWStart in flags then lflags:=lflags+SC_WRAPVISUALFLAG_START;
	SetWrapVisualFlags(lflags);
end;

function TScintillaMemo.GetWordWrapVisualLoc : sciWordWrapVisualFlagLocations;
var
  tmp : sciWordWrapVisualFlagLocations;
  lflags : LongInt;
begin
	tmp:=[];
	lflags:=GetWrapVisualFlagsLocation;
	if (Boolean(lflags and SC_WRAPVISUALFLAGLOC_END_BY_TEXT)) then
    Include(tmp,sciEndByText)
  else 
    Exclude(tmp,sciEndByText);
	if (Boolean(lflags and SC_WRAPVISUALFLAGLOC_START_BY_TEXT)) then 
    Include(tmp,sciStartByText)
  else 
    Exclude(tmp,sciStartByText);
	result :=tmp;
end;

procedure TScintillaMemo.SetWordWrapVisualLoc(const flags : sciWordWrapVisualFlagLocations);
var
  lflags : LongInt;
begin
	lflags:=0;
	if sciEndByText in flags then lflags:=lflags+SC_WRAPVISUALFLAGLOC_END_BY_TEXT;
	if sciStartByText in flags then lflags:=lflags+SC_WRAPVISUALFLAGLOC_START_BY_TEXT;
	SetWrapVisualFlagsLocation(lflags);
end;

procedure TScintillaMemo.BookmarkAdd(lineno : LongInt);
	begin
	if lineno = -1 then
		lineno := GetCurrentLineNumber;
	if not BookmarkPresent(lineno) then
		MarkerAdd(lineno, SCITE_MARKER_BOOKMARK);
end;

procedure TScintillaMemo.BookmarkDelete(lineno : LongInt);
begin
	if lineno = -1 then
		lineno := GetCurrentLineNumber();
	if BookmarkPresent(lineno) then
		MarkerDelete(lineno, SCITE_MARKER_BOOKMARK);
end;

function TScintillaMemo.BookmarkPresent(lineno : LongInt) : Boolean;
var
  state : LongInt;
begin
	if lineno = -1 then
		lineno := GetCurrentLineNumber();
	state := MarkerGet(lineno);
	result :=Boolean(state and (1 shl SCITE_MARKER_BOOKMARK));
end;

procedure TScintillaMemo.BookmarkToggle(lineno : LongInt);
begin
	if lineno = -1 then
		lineno := GetCurrentLineNumber();
	if BookmarkPresent(lineno) then
		BookmarkDelete(lineno)
	else
		BookmarkAdd(lineno);
end;

function TScintillaMemo.BookmarkNext(forwardScan : Boolean) : Boolean;
var
  lineno,
  sci_marker,
  lineStart,
  lineRetry,
  nextLine : LongInt;
begin
	lineno := GetCurrentLineNumber();
	sci_marker := SCI_MARKERNEXT;
	lineStart := lineno + 1;	//Scan starting from next line
	lineRetry := 0;				//If not found, try from the beginning
	if not forwardScan then
	begin
		lineStart := lineno - 1;		//Scan starting from previous line
		lineRetry := GetLineCount;	//If not found, try from the end
		sci_marker := SCI_MARKERPREVIOUS;
	end;
	nextLine := SPerform(sci_marker, lineStart, (1 shl SCITE_MARKER_BOOKMARK));
	if nextLine < 0 then
		nextLine := SPerform(sci_marker, lineRetry, (1 shl SCITE_MARKER_BOOKMARK));
	if (nextLine < 0) or (nextLine = lineno) then // No bookmark (of the given type) or only one, and already on it
	begin
		result :=false;
		Exit;
	end	else begin
		GotoLineEnsureVisible(nextLine);
		result :=true;
		Exit;
	end;
	result :=false;
end;

function TScintillaMemo.GetIndentation: TIndentationOptions;
begin
  Result := [];
  if fKeepIndent then Result := [KeepIndent];
  if GetTabIndents then Result := Result + [TabIndents];
  if GetBackSpaceUnIndents then Result := Result + [BackSpaceUnIndents];
  if GetIndentationGuides then Result := Result + [IndentationGuides];
end;

procedure TScintillaMemo.SetIndentation(const Value: TIndentationOptions);
begin
  fKeepIndent := KeepIndent in Value;
  SetTabIndents(TabIndents in Value);
  SetBackSpaceUnIndents(BackSpaceUnIndents in Value);
  SetIndentationGuides(IndentationGuides in Value);
end;

procedure TScintillaMemo.MaintainIndentation(ch : Integer);
Var
	eolMode,
  curLine,
  lastLine,
  indentAmount : LongInt;
begin
	eolMode := GetEOLMode;
	curLine := LineFromPosition(GetCurrentPos);
	lastLine := curLine - 1;
	indentAmount := 0;

	if (((eolMode = SC_EOL_CRLF) or (eolMode = SC_EOL_LF)) and (ch = 10)) or ((eolMode = SC_EOL_CR) and (ch = 13)) then
  begin
		while (lastLine >= 0) and	(GetLineEndPosition(lastLine) - PositionFromLine(lastline) = 0) do
			Dec(lastLine);
		if (lastLine >= 0) then
      indentAmount := GetLineIndentation(lastLine);
		if (indentAmount > 0) then
    begin
			SetLineIndentation(curLine, indentAmount);
			SetCurrentPos(Self.GetLineIndentPosition(curLine));
			Self.SetSel(GetCurrentPos, GetCurrentPos);
		end;
	end;
end;

procedure TScintillaMemo.doSciAutoCSelection(const text : PChar);
begin
  //FController.ExecuteAutoCSelection(Self,text);
  inherited doSciAutoCSelection(text);
end;

procedure TScintillaMemo.doSciCharAdded(const ch : Integer);
var
  cpos: Integer;
begin
	if fKeepIndent then MaintainIndentation(ch);
  //FController.ExecuteCharAdded(Self,ch);

  if FAutoCloseBraces=True then
  begin
    if Char(ch) in ['(','{','['] then
    begin
      cpos:=GetCurrentPos;
      case Char(ch) of
        '(': begin AddText(1,')');SetAnchor(cpos);SetCurrentPos(cpos);end;
        '{': begin AddText(1,'}');SetAnchor(cpos);SetCurrentPos(cpos);end;
        '[': begin AddText(1,']');SetAnchor(cpos);SetCurrentPos(cpos);end;
      end;
    end;
  end;
  if FAutoCloseQuotes then
  begin
    cpos:=GetCurrentPos;
    case Char(ch) of
      '"': begin AddText(1,'"');SetAnchor(cpos);SetCurrentPos(cpos);end;
      '''': begin AddText(1,'''');SetAnchor(cpos);SetCurrentPos(cpos);end;
    end;
  end;
  inherited doSciCharAdded(ch);
end;

function TScintillaMemo.GetEOLStyle: TEOLStyle;
begin
	Result := TEOLStyle(GETEOLMode);
end;

procedure TScintillaMemo.SetEOLStyle(const Value: TEOLStyle);
begin
	SetEOLMode(Ord(Value));
  Self.ConvertEOLs(Ord(Value));
end;

procedure TScintillaMemo.SetKeyCommands(const Value: TSciKeyCommandCollection);
begin
	fKeyCommands.Assign(Value);
end;

procedure TScintillaMemo.SetLines(Const Value : TStrings);
begin
  FLines.Assign(Value);
end;

procedure TScintillaMemo.InSetControlCharSymbol(const Value : Char);
begin
  SetControlCharSymbol(Integer(Value));
end;
function  TScintillaMemo.InGetControlCharSymbol : Char;
begin
  Result:=Char(GetControlCharSymbol);
end;
procedure TScintillaMemo.doSciCalltipClick(const position : Integer);
begin
  //FController.ExecuteCallTipClick(Self,position);
  inherited;
end;
{$Ifndef NOMACRORECORD}
procedure TScintillaMemo.doSciMacroRecord(const msg : Integer;const wParam : uptr_t;const lParam : sptr_t);
begin
  //FController.ExecuteMacroRecord(Self,msg,LongInt(wParam),LongInt(lParam));
  inherited;
end;
{$Endif}

procedure TScintillaMemo.SetMargins(const Index: Integer; const Value: TMargin);
begin
	case Index of
    0 : fMargin0.Assign(Value);
    1 : fMargin1.Assign(Value);
    2 : fMargin2.Assign(Value);
  end;
end;

procedure TScintillaMemo.StripTrailingSpaces;
var
  maxLines,line,lineStart,lineEnd,i,ch : Integer;
begin
  maxLines:=GetLineCount;
  for line:=0 to (maxLines-1) do
  begin
    lineStart:=PositionFromLine(line);
    lineEnd:=GetLineEndPosition(line);
    i:=lineEnd-1;
    ch:=GetCharAt(i);
    while (i>=lineStart) and (IsSpace(ch)=True) do
    begin
      Dec(i);
      ch:=GetCharAt(i);
    end;
    if (i<lineEnd-1) then
    begin
      SetTargetStart(i+1);
      SetTargetEnd(lineEnd);
      ReplaceTarget(0,'');
    end;
  end;
end;

// TDivOptions class
constructor TDivOptions.Create(Scintilla: TScintillaBase);
begin
	fScintilla := Scintilla;
end;

procedure TDivOptions.Assign(Source: TPersistent);
begin
	if Source is TDivOptions then
  begin
		ScrollBarV := TDivOptions(Source).ScrollBarV;
		ScrollBarH := TDivOptions(Source).ScrollBarH;
		UsePalette:= TDivOptions(Source).UsePalette;
		OverType:= TDivOptions(Source).OverType;
		ViewEOL:= TDivOptions(Source).ViewEOL;
		EndAtLastLine:= TDivOptions(Source).EndAtLastLine;
		ViewWSpace:=TDivOptions(Source).ViewWSpace;
	end else
		inherited;
end;

function  TDivOptions.GetWSMode : sciWSMode;
var
  wsm : LongInt;
begin
  wsm:=fScintilla.GetViewWS;
  Result:=sciWSMode(wsm);
end;

procedure TDivOptions.SetWSMode(value : sciWSMode);
begin
  fScintilla.SetViewWS(LongInt(value));
end;
function  TDivOptions.GetBool(const Index : Integer) : Boolean;
begin
	case Index of
	0:result :=fScintilla.GetUsePalette;
	1:result :=fScintilla.GetOverType;
	2:result :=fScintilla.GetViewEOL;
	3:result :=Boolean(fScintilla.GetEndAtLastLine);
	4:result :=fScintilla.GetHScrollBar;
	5:result :=fScintilla.GetVScrollBar;
	else Result:=false;
	end;
end;

procedure  TDivOptions.SetBool(const Index : Integer;const Value : Boolean);
begin
	case Index of
	0:fScintilla.SetUsePalette(Value);
	1:fScintilla.SetOverType(Value);
	2:fScintilla.SetViewEOL(Value);
	3:fScintilla.SetEndAtLastLine(Value=True);
	4:fScintilla.SetHScrollBar(Value);
	5:fScintilla.SetVScrollBar(Value);
	end;
end;

//TSciHotSpot class
constructor TSciHotSpot.Create(Scintilla: TScintillaBase);
begin
	fScintilla := Scintilla;
  FHotActiveFore:=clDefault;
  FHotActiveBack:=clDefault;
  FHotActiveUnderline:=True;
  FHotActiveSingleLine:=False;
end;

procedure TSciHotSpot.SetHotActiveFore(const Value : TColor);
begin
  FHotActiveFore:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetHotspotActiveFore(true,Value);
	end else
	begin
		fScintilla.SetHotspotActiveFore(false,Value);
	end;
end;

procedure TSciHotSpot.SetHotActiveBack(const Value : TColor);
begin
  FHotActiveBack:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetHotspotActiveBack(true,Value);
	end else
	begin
		fScintilla.SetHotspotActiveBack(false,Value);
	end;
end;

procedure TSciHotSpot.SetHotActiveUnderline(Value : Boolean);
begin
  FHotActiveUnderline:=Value;
  fScintilla.SetHotspotActiveUnderline(Value);
end;

procedure TSciHotSpot.SetHotActiveSingleLine(Value : Boolean);
begin
  FHotActiveSingleLine:=Value;
  fScintilla.SetHotspotSingleLine(Value);
end;

procedure TSciHotSpot.Assign(Source: TPersistent);
begin
	if Source is TSciHotSpot then
  begin
		FHotActiveBack := TSciHotSpot(Source).BackColor;
		FHotActiveFore:=TSciHotSpot(Source).ForeColor;
    FHotActiveUnderline:=TSciHotSpot(Source).Underlined;
    FHotActiveSingleLine:=TSciHotSpot(Source).SingleLine;
	end else
		inherited;
end;

// TSciColors class
constructor TSciColors.Create(Scintilla: TScintillaBase);
begin
	fScintilla := Scintilla;
	FFoldHiColor :=clBlack;
	FFoldLoColor :=clBlack;
	FMarkerFore :=clYellow;
	FMarkerBack :=clBlue;
	FBookMarkForeColor :=clWhite;
	FBookMarkBackColor :=clGray;
	FForeSelColor :=clHighLightText;
	FBackSelColor :=clHighLight;
end;

procedure TSciColors.Assign(Source: TPersistent);
begin
	if Source is TSciColors then
  begin
		SelFore := TSciColors(Source).SelFore;
		SelBack:=TSciColors(Source).SelBack;
		MarkerFore:= TSciColors(Source).MarkerFore;
		MarkerBack:= TSciColors(Source).MarkerBack;
		FoldHi:=TSciColors(Source).FoldHi;
		FoldLo:=TSciColors(Source).FoldLo;
		BookMarkBack:=TSciColors(Source).BookMarkBack;
		BookMarkFore:=TSciColors(Source).BookMarkFore;
	end else
		inherited;
end;

procedure TSciColors.SetFoldHi(const Value : TColor);
begin
	FFoldHiColor:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetFoldMarginHiColour(true,Value);
	end else
	begin
		fScintilla.SetFoldMarginHiColour(false,Value);
	end;
end;

procedure TSciColors.SetFoldLo(const Value : TColor);
begin
	FFoldLoColor:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetFoldMarginColour(true,Value);
	end else
	begin
		fScintilla.SetFoldMarginColour(false,Value);
	end;
end;

procedure TSciColors.SetBookMarkFore(const Value : TColor);
begin
	FBookMarkForeColor :=Value;
	fScintilla.MarkerSetFore(SCITE_MARKER_BOOKMARK, Value);
end;

procedure TSciColors.SetBookMarkBack(const Value : TColor);
begin
	FBookMarkBackColor :=Value;
	fScintilla.MarkerSetBack(SCITE_MARKER_BOOKMARK, Value);
end;

procedure TSciColors.SetMarkerFore(const Value : TColor);
	procedure MarkFore(MarkNum, BackCol : TColor);
	begin
		fScintilla.MarkerSetFore(MarkNum, BackCol);
	end;
begin
	FMarkerFore :=Value;
	if(Value<>clDefault) then
	begin
		MarkFore(SC_MARKNUM_FOLDEROPEN,Value);
		MarkFore(SC_MARKNUM_FOLDER,Value);
		MarkFore(SC_MARKNUM_FOLDERSUB,Value);
		MarkFore(SC_MARKNUM_FOLDERTAIL,Value);
		MarkFore(SC_MARKNUM_FOLDEREND,Value);
		MarkFore(SC_MARKNUM_FOLDEROPENMID, Value);
		MarkFore(SC_MARKNUM_FOLDERMIDTAIL, Value);
		if BookMarkFore=clDefault then
			MarkFore(SCITE_MARKER_BOOKMARK,Value);
	end;
end;

procedure TSciColors.SetMarkerBack(const Value : TColor);
	procedure MarkBack(MarkNum, BackCol : TColor);
	begin
		fScintilla.MarkerSetBack(MarkNum, BackCol);
	end;
begin
	FMarkerBack :=Value;
	if(Value<>clDefault) then
	begin
		MarkBack(SC_MARKNUM_FOLDEROPEN,Value);
		MarkBack(SC_MARKNUM_FOLDER,Value);
		MarkBack(SC_MARKNUM_FOLDERSUB,Value);
		MarkBack(SC_MARKNUM_FOLDERTAIL,Value);
		MarkBack(SC_MARKNUM_FOLDEREND,Value);
		MarkBack(SC_MARKNUM_FOLDEROPENMID, Value);
		MarkBack(SC_MARKNUM_FOLDERMIDTAIL, Value);
		if BookMarkBack=clDefault then
			MarkBack(SCITE_MARKER_BOOKMARK,Value);
	end;
end;

procedure TSciColors.SetForeSel(const Value : TColor);
begin
	FForeSelColor:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetSelFore(true,Value);
	end else
	begin
		fScintilla.SetSelFore(false,Value);
	end;
end;

procedure TSciColors.SetBackSel(const Value : TColor);
begin
	FBackSelColor:=Value;
	if Value<>clDefault then
	begin
		fScintilla.SetSelBack(true,Value);
	end else
	begin
		fScintilla.SetSelBack(false,Value);
	end;
end;

// TCaret class
constructor TCaret.Create(Scintilla: TScintillaBase);
begin
	fScintilla := Scintilla;
end;

procedure TCaret.Assign(Source: TPersistent);
begin
	if Source is TCaret then 
  begin
		LineBackColor := TCaret(Source).LineBackColor;
		ForeColor:=TCaret(Source).ForeColor;
		LineVisible := TCaret(Source).LineVisible;
		Period :=TCaret(Source).Period;
		Width :=TCaret(Source).Width;
	end else
		inherited;
end;

procedure TCaret.SetCaretPeriod(const Value : LongInt);
begin
	fScintilla.SetCaretPeriod(Value);
end;
function  TCaret.GetCaretPeriod : LongInt;
begin
	result :=fScintilla.GetCaretPeriod;
end;
function TCaret.GetCaretWidth : LongInt;
begin
	Result:=fScintilla.GetCaretWidth;
end;

procedure TCaret.SetCaretWidth(const Value : LongInt);
begin
	fScintilla.SetCaretWidth(Value);
end;

function TCaret.GetCaretFore: TColor;
begin
  Result:=fScintilla.GetCaretFore;
end;

procedure TCaret.SetCaretFore(const Value: TColor);
begin
  fScintilla.SetCaretFore(Value);
end;

function TCaret.GetCaretLineBack: TColor;
begin
  Result:=fScintilla.GetCaretLineBack;
end;

procedure TCaret.SetCaretLineBack(const Value: TColor);
begin
  fScintilla.SetCaretLineBack(Value);
end;

function TCaret.GetCaretLineVisible : Boolean;
begin
  Result:=fScintilla.GetCaretLineVisible;
end;

procedure TCaret.SetCaretLineVisible(const Value : Boolean);
begin
  fScintilla.SetCaretLineVisible(Value);
end;


// TMargin class
constructor TMargin.Create(Scintilla: TScintillaBase; Number : Integer);
begin
  fScintilla := Scintilla;
  fNumber := Number;
end;

procedure TMargin.Assign(Source: TPersistent);
begin
	if Source is TMargin then
  begin
		MarginType := TMargin(Source).MarginType;
		Width := TMargin(Source).Width;
	end else
		inherited;
end;

function TMargin.GetMarginType: TMarginType;
begin
  Result := TMarginType(fScintilla.GetMarginTypeN(fNumber))
end;

function TMargin.GetWidth: Integer;
begin
  Result := fScintilla.GetMarginWidthN(fNumber)
end;

procedure TMargin.SetMarginType(const Value: TMarginType);
begin
	fScintilla.SetMarginTypeN(fNumber, Ord(Value));
end;

procedure TMargin.SetWidth(const Value: Integer);
begin
  fScintilla.SetMarginWidthN(fNumber, Value)
end;


function HasPrefix(const s : AnsiString;const prefix : AnsiString;const ignorecase : Boolean) : Boolean;
begin
	if ignorecase then
		Result:=(StrLIComp(PChar(prefix),PChar(s),Min(Length(prefix),Length(s)))=0)
	else
		Result:=(StrLComp(PChar(prefix),PChar(s),Min(Length(prefix),Length(s)))=0);
end;

procedure FillMatching(const startwith : String;var deststr : String;fromlist : TStrings;const ignorecase : Boolean);
var
cnt,i,numadded : LongInt;
tmp : String;
begin
	deststr:='';
	numadded:=0;
	cnt:=fromlist.Count;
	for i:=0 to (cnt-1) do
	begin
		tmp:=fromlist.Strings[i];
    if HasPrefix(tmp,startwith,ignorecase)=true then
    begin
      if numadded>0 then deststr:=deststr+' ';
      deststr:=deststr+tmp;
      numadded:=numadded+1;
    end;
	end;
end;

function TScintillaMemo.LocateIndicator(indicatorNumber,startSearchAtPos: Integer): Integer;
var
  i,len : Integer;
  mask : Integer;
begin
  Result:=-1;
  len:=GetLength;
  case indicatorNumber of
    0: mask:=INDIC0_MASK;
    1: mask:=INDIC1_MASK;
    2: mask:=INDIC2_MASK;
    else
      mask:=indicatorNumber;
  end;

  for i:=startSearchAtPos to len do
  begin
    if (GetStyleAt(i) and mask)<>0 then
    begin
      Result:=i;
      Exit;
    end;
  end;
end;

function TScintillaMemo.LengthOfIndicator(indicatorNumber,atPos: Integer): Integer;
var
  i,len : Integer;
  mask : Integer;
begin
  Result:=-1;
  len:=GetLength;
  case indicatorNumber of
    0: mask:=INDIC0_MASK;
    1: mask:=INDIC1_MASK;
    2: mask:=INDIC2_MASK;
    else
      mask:=indicatorNumber;
  end;
  for i:=atPos to len do
  begin
    if (GetStyleAt(i) and mask)=0 then
    begin
      Result:=i-atPos;
      Exit;
    end;
  end;
end;

function BitsToMask(numbits : Integer) : Integer;
begin
  Result:=$1f;
  case numbits of
    6: Result:=$3f;
    7: Result:=$7f;
  end;
end;

function OppositeMask(numbits : Integer) : Integer;
begin
  Result:=INDICS_MASK;
  case numbits of
    6: Result:=(INDIC1_MASK and INDIC2_MASK);
    7: Result:=INDIC2_MASK;
  end;
end;

procedure TScintillaMemo.RemoveIndicator(indicatorNumber, atPos : Integer);
var
  len : Integer;
begin
  if atPos=-1 then
  begin
    ShowIndicatorAt(-1,0,GetLength,False);
  end else
  begin
    len:=LengthOfIndicator(indicatorNumber,atPos);
    if len>0 then
    begin
      ShowIndicatorAt(indicatorNumber,atPos,len,False);
    end;
  end;
end;

procedure TScintillaMemo.ShowIndicatorAt(indicatorNumber,atPos, Length: Integer;Visible: Boolean);
var
  oldPos : Integer;
  mask : Integer;
  oldMask : Integer;
  nbits : Integer;
  ilen : Integer;
begin
  nbits :=GetStyleBits;
  case indicatorNumber of
    -1: mask:=OppositeMask(nbits);
    0: mask:=INDIC0_MASK;
    1: mask:=INDIC1_MASK;
    2: mask:=INDIC2_MASK;
    else
      mask:=indicatorNumber;
  end;
  if length<>-1 then
    ilen:=length
  else
    ilen:=LengthOfIndicator(indicatorNumber,atPos);
  if ilen>0 then
  begin
    oldMask:=BitsToMask(nbits);
    oldPos:=GetEndStyled;
    StartStyling(atPos,mask);
    if (Visible) and (indicatorNumber<>-1) then
      SetStyling(ilen,mask)
    else
      SetStyling(ilen,0);
    StartStyling(oldPos,oldMask);
  end;
end;

end.