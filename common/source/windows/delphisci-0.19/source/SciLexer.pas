//CE_Desc_Include(helpdescriptions.txt)
{
 Unit    : SciLexer
 Purpose : Interface for Scintilla Lexer DLL
 Created : 05/03/2001
 Original Author  : Simon Steele (ss@pnotepad.org)
 Author: Jan Martin Pettersen (hdalis@users.sourceforge.net) (starting from 29/09/2004)
      $Id: SciLexer.pas,v 1.5 2004/11/13 04:29:50 hdalis Exp $
 History : 05/03/2001 Turned this into a proper VCL wrapper for Scintilla
                      class window controls. SetFocus is not entirely right,
                      and is overridden by the scintilla thing. To set focus do:
                      Scintilla1.SetFocus(True);
                      Windows.SetFocus(Scintilla1.Handle);
                      There's no error checking yet, so make sure the DLL is
                      present!
                     	Daytime :): Added Event handling generation stuff, and
                      some ported	types from scintilla.h so that event handling can work!
           19/03/2001 Added FindText stuff, needs updating in the python...
           25/03/2001 Changed SCI_REPLACESEL Wrapper variable order - it was wrong!
		       27/05/2001 Re-Wrote the python generator, and changed the structure of the
					            control.
           09/06/2001 Removed some function-less functions.
           03/12/2001 Added WM_ERASEBKGND function to remove scroll-flicker. Thanks
                  	  to Gertjan Schuurmans and Jeff Cogswell.
           15/02/2003 Extensive changes to make the control active at design time and
                      expose a large number of properties (see KV comments)
                      (kvlahos@london.edu)
           15/03/2003 LoadFromStream and SaveToStream added which now
                      properly handle Unicode.  Property CodeFolding added
                      (kvlahos@london.edu)
           05/11/2003 Workaround for Scintilla bug in GetSelText
                      Updated for Scintilla 1.56 (works with later versions)
                      Other improvements and bug fixes
                      (kvlahos@london.edu)
           01/03/2004 Python and Ruby Keywords and Styles added
                      Keyboard customisation
                      Help file added
           29/09/2004 Updated for Scintilla 1.61
                      Changed the component into a baseclass for other components.
                      Removed some properties, and placed them in the derived
                      TScintilla component. Added some select functions etc.
                      Added WordChars property to allow use by autocomplete etc.
                      Most properties are protected now, to be exposed in derived
                      in derived subclasses.
                      Changed the notificationinterface to call virtual functions
                      so they can be overridden in derived classes.
                      Adopted and translated to delphi some functions from SciTE
                      for use with the components.
                      Added option to clear undo after save.
                      Changed propertynames to resemble other edit controls such
                      as TRichEdit etc. (SelText,SelStart,SelLength etc)

                      All styledefinitions moved to ScintillaSynLexer.inc
                      and scilangfiller.pas. No longer used anywhere but at designtime.
                      Many more language keywords/styles added.
                      These are only used at designtime via the 'Fill with default Languages'
                      menu option.
                      Changed the enumerated lexerselection to strings (more flexibility)
                      (hdalis@users.sourceforge.net)
           13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                      generate the help.
                      hdalis@users.sourceforge.net
           02/11/2004 Refreshed against version 1.62 of scintilla.iface
           09/11/2004 SelectionFileName added two extra bytes (nullbytes), Fixed.
                      A bug was found in RangeIsAllWhiteSpace. It did exactly the opposite
                      of what it should do.
                      hdalis@users.sourceforge.net
           11/11/2004 Removed the IsDirty property. Modified did just the same actually.
                      hdalis@users.sourceforge.net
           22/11/2004 The scintilla control wouldn't let go of the mouse when the
                      mouse was clicked inside a selection and you then moved the mouse
                      outside the control (to another control)
                      Fixed it with a small hack, added the MouseMove procedure to
                      detect if we are outside the control when both the left and right
                      mousebutton wasn't down.
                      There is still a little problem in the designer, doubleclick on the
                      scintilla control sends only mousedown so the componenteditor isn't
                      executed on a doubleclick , don't know why..
                      It all works at runtime however..
                      hdalis@users.sourceforge.net
           11/12/2004 Changed all LongBool to Boolean 'cause the LongBool style wasn't very appreciated
                      by Scintilla when the .dll was optimized for Pentium, and all optimizations enabled.
                      If fired the same assert in scintillas code all the time. Fixed.
                      hdalis@users.sourceforge.net
           12/03/2005 Added GetDelphiSciVersion and GetDelphiSciVersionStr
                      hdalis@users.sourceforge.net
           04/05/2005 BUGFIX 01:
                      Fixed a bug which caused that if any char keys pressed triggered
                      the mnemoniced control when the key matched the mnemonics without ALT.
                      hdalis@users.sourceforge.net
					 04/05/2005 Added the GetDelphiSciVersion,GetDelphiSciVersionStr,GetDelphiSciURL,
					            for easy use in an aboutbox.
 }
{$Include SciCommonDef.Inc}
{ Most of this file is automatically generated from the Scintilla.iface interface definition
  file which contains any comments about the definitions. PasGen.py does the generation.
  Please use the corresponding scintilla.iface file when you regenerate the code.
}

unit SciLexer;
interface
uses Classes, Windows, Controls, Forms, Messages, SysUtils, Graphics,  StdActns, SciSupport;

const
	BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
	SCITE_MARKER_BOOKMARK=1; //The marker number for the bookmark functions.
const
//These constants are defined to easily assign to the WordChars property.
sci_alphachars='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
sci_numericchars='0123456789';
sci_accentedchars='äöåúüˇ¿‡¡·¬‚√„ƒ‰≈Â∆Ê«Á»Ë…È ÍÀÎÃÏÕÌŒÓœÔ–—Ò“Ú”Û‘Ù’ı÷ÿ¯Ÿ˘⁄˙€˚‹¸›˝ﬁ˛ﬂˆ';
type
	  TTestingFunction=function (const ch : LongInt) : boolean;
	//++EventTypes
    TSCEvent_styleneeded = procedure(Sender : TObject; const position : LongInt) of object;
    TSCEvent_charadded = procedure(Sender : TObject; const ch : LongInt) of object;
    TSCEvent_savepointreached = procedure(Sender : TObject) of object;
    TSCEvent_savepointleft = procedure(Sender : TObject) of object;
    TSCEvent_modifyattemptro = procedure(Sender : TObject) of object;
    TSCEvent_key = procedure(Sender : TObject; const ch : LongInt; const modifiers : LongInt) of object;
    TSCEvent_doubleclick = procedure(Sender : TObject) of object;
    TSCEvent_updateui = procedure(Sender : TObject) of object;
    TSCEvent_modified = procedure(Sender : TObject; const position : LongInt; const modificationType : LongInt; text : PChar; const len : LongInt; const linesAdded : LongInt; const line : LongInt; const foldLevelNow : LongInt; const foldLevelPrev : LongInt) of object;
    TSCEvent_macrorecord = procedure(Sender : TObject; const message : LongInt; const wParam : LongInt; const lParam : LongInt) of object;
    TSCEvent_marginclick = procedure(Sender : TObject; const modifiers : LongInt; const position : LongInt; const margin : LongInt) of object;
    TSCEvent_needshown = procedure(Sender : TObject; const position : LongInt; const len : LongInt) of object;
    TSCEvent_painted = procedure(Sender : TObject) of object;
    TSCEvent_userlistselection = procedure(Sender : TObject; const listType : LongInt; text : PChar) of object;
    TSCEvent_dwellstart = procedure(Sender : TObject; const position : LongInt) of object;
    TSCEvent_dwellend = procedure(Sender : TObject; const position : LongInt) of object;
    TSCEvent_zoom = procedure(Sender : TObject) of object;
    TSCEvent_hotspotclick = procedure(Sender : TObject; const modifiers : LongInt; const position : LongInt) of object;
    TSCEvent_hotspotdoubleclick = procedure(Sender : TObject; const modifiers : LongInt; const position : LongInt) of object;
    TSCEvent_calltipclick = procedure(Sender : TObject; const position : LongInt) of object;
    TSCEvent_autocselection = procedure(Sender : TObject; text : PChar) of object;
//--EventTypes
    TSCEventMsgSent = procedure(Sender : TObject; Msg : Integer; wParam, lParam : LongInt) of object;

  //Lex
  ExtLexerFn=procedure(lexernum,startPos : Cardinal;length,initStyle : Integer;words : array of PChar;window : THandle;props : PChar);stdcall;
  //Fold
  ExtFoldFn=procedure(lexernum,startPos : Cardinal;length,initStyle : Integer;words : array of PChar;window : THandle;props : PChar);stdcall;
  ////Not used yet it seems.
  GetLexerFn=function (Index : Integer) : Pointer;stdcall;
  //GetLexerCount
  GetLexerCountFn=function : Integer;stdcall;
  //GetLexerName
  GetlexerNameFn=procedure(Index : Cardinal;name : PChar;buflen : Integer);stdcall;
  TWordWrapType=(sciNoWrap,sciWrap,sciWrapChar);

	TScintillaBase = class(TWinControl)
	private
    FBorderStyle : TBorderStyle;
    FCreating    : Boolean;
    SCPerform    : TScintillaMessageFnc;
		sccmdctr     : Pointer;
		FWantReturns : Boolean;
		FWantTabs    : Boolean;
		FDirty       : Boolean;
    FOnMsgSent   : TSCEventMsgSent;
		FDummy       : Boolean;
    initing      : Boolean;
		FWordChars   : String;
		FClearUndoAfterSave : Boolean;
    oldmodeventmask : LongInt;
		fStateStream : TMemoryStream;
    FForceMouseRelease : Boolean;
    //++EventPrivates
        FOnstyleneeded : TSCEvent_styleneeded;
        FOncharadded : TSCEvent_charadded;
        FOnsavepointreached : TSCEvent_savepointreached;
        FOnsavepointleft : TSCEvent_savepointleft;
        FOnmodifyattemptro : TSCEvent_modifyattemptro;
        FOnkey : TSCEvent_key;
        FOndoubleclick : TSCEvent_doubleclick;
        FOnupdateui : TSCEvent_updateui;
        FOnmodified : TSCEvent_modified;
        FOnmacrorecord : TSCEvent_macrorecord;
        FOnmarginclick : TSCEvent_marginclick;
        FOnneedshown : TSCEvent_needshown;
        FOnpainted : TSCEvent_painted;
        FOnuserlistselection : TSCEvent_userlistselection;
        FOndwellstart : TSCEvent_dwellstart;
        FOndwellend : TSCEvent_dwellend;
        FOnzoom : TSCEvent_zoom;
        FOnhotspotclick : TSCEvent_hotspotclick;
        FOnhotspotdoubleclick : TSCEvent_hotspotdoubleclick;
        FOncalltipclick : TSCEvent_calltipclick;
        FOnautocselection : TSCEvent_autocselection;
    //--EventPrivates
		procedure doSciKey(const ch : Integer;const modifiers : Integer);//Not used by windows
	protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override; //handles the case when scintilla wont let go of the mouse.
		procedure WndDestroy;virtual;
    // Protected instead of private to allow derived classes to call inherited.
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure Loaded; override;
		procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
		procedure DestroyWnd; override;
		// Scintilla Event Functions
		procedure doSciCharAdded(const ch : Integer); virtual;
		procedure doSciUserListSelection(const listType : LongInt; text : PChar); virtual;
		procedure doSciUpdateUI;virtual;
		procedure doSciModified(const position : LongInt; const modificationType : LongInt;
														text : PChar; const len : LongInt; const linesAdded : LongInt; const line : LongInt;
														const foldLevelNow : LongInt; const foldLevelPrev : LongInt);virtual;
		procedure doSciMarginClick(const modifiers : LongInt; const position : LongInt; const margin : LongInt);virtual;
		procedure doSciSavePointReached;virtual;
		procedure doSciSavePointLeft;virtual;
		procedure doSciStyleNeeded(const position : Integer);virtual;
		procedure doSciModifyAttemptRO;virtual;
		procedure doSciDoubleClick;virtual;
		procedure doSciNeedShown(const position : Integer;const len : Integer);virtual;
		procedure doSciPainted;virtual;
		procedure doSciDwellStart(const position : Integer);virtual;
		procedure doSciDwellEnd(const position : Integer);virtual;
		procedure doSciZoom;virtual;
		procedure doSciHotspotClick(const modifiers : Integer;const position : Integer);virtual;
		procedure doSciHotspotDoubleClick(const modifiers : Integer;const position : Integer);virtual;
		procedure doSciMacroRecord(const msg : Integer;const wParam : uptr_t;const lParam : sptr_t);virtual;
		procedure doSciCalltipClick(const position : Integer);virtual;
    procedure doSciAutoCSelection(const text : PChar);virtual;
		// End of Scintilla Event Functions
	private
		procedure SetBorderStyle(Value: TBorderStyle);
		procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
		procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
		procedure WMNCDestroy(var Message: TWMNCDestroy); message WM_NCDESTROY;
		procedure WMNotify(var Message: TWMNotify); message CN_NOTIFY;
		procedure inSetWordChars(const Value : String);
{$Ifdef USEWMERASE}
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
{$Endif}
		procedure SetDummy(Value : Boolean);
		function  GetUseUnicode: Boolean;
		procedure SetUseUnicode(const Value: Boolean);
		function  GetWordWrap: TWordWrapType;
		procedure SetWordWrap(const Value: TWordWrapType);
		function  GetSelection: string;
		procedure SetSelection(const Value: string);
		procedure SetSelStart(const value : LongInt);
		function  GetSelectionLength : LongInt;                // Returns length of selection
		procedure SetSelectionLength(const Value : LongInt);
  public
		constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
		procedure   DefaultHandler(var Message); override;
		function    SPerform(Msg : LongInt; wParam : LongInt=0; lParam : LongInt=0) : LongInt;
    procedure   DisableAllEvents(disable : Boolean);
//++FuncDef
    procedure AddText(length : LongInt; text : PChar);
    procedure AddStyledText(length : LongInt; c : PChar);
    procedure InsertText(pos : LongInt; text : PChar);
    procedure ClearAll;
    procedure ClearDocumentStyle;
    function GetLength : LongInt;
    function GetCharAt(pos : LongInt) : LongInt;
    function GetCurrentPos : LongInt;
    function GetAnchor : LongInt;
    function GetStyleAt(pos : LongInt) : LongInt;
    procedure Redo;
    procedure SetUndoCollection(collectUndo : Boolean);
    procedure SelectAll;
    procedure SetSavePoint;
    function GetStyledText(tr : PTextRange) : LongInt;
    function CanRedo : Boolean;
    function MarkerLineFromHandle(handle : LongInt) : LongInt;
    procedure MarkerDeleteHandle(handle : LongInt);
    function GetUndoCollection : Boolean;
    function GetViewWS : LongInt;
    procedure SetViewWS(viewWS : LongInt);
    function PositionFromPoint(x : LongInt; y : LongInt) : LongInt;
    function PositionFromPointClose(x : LongInt; y : LongInt) : LongInt;
    procedure GotoLine(line : LongInt);
    procedure GotoPos(pos : LongInt);
    procedure SetAnchor(posAnchor : LongInt);
    function GetCurLine(length : LongInt; text : PChar) : LongInt;
    function GetEndStyled : LongInt;
    procedure ConvertEOLs(eolMode : LongInt);
    function GetEOLMode : LongInt;
    procedure SetEOLMode(eolMode : LongInt);
    procedure StartStyling(pos : LongInt; mask : LongInt);
    procedure SetStyling(length : LongInt; style : LongInt);
    function GetBufferedDraw : Boolean;
    procedure SetBufferedDraw(buffered : Boolean);
    procedure SetTabWidth(tabWidth : LongInt);
    function GetTabWidth : LongInt;
    procedure SetCodePage(codePage : LongInt);
    procedure SetUsePalette(usePalette : Boolean);
    procedure MarkerDefine(markerNumber : LongInt; markerSymbol : LongInt);
    procedure MarkerSetFore(markerNumber : LongInt; fore : TColor);
    procedure MarkerSetBack(markerNumber : LongInt; back : TColor);
    function MarkerAdd(line : LongInt; markerNumber : LongInt) : LongInt;
    procedure MarkerDelete(line : LongInt; markerNumber : LongInt);
    procedure MarkerDeleteAll(markerNumber : LongInt);
    function MarkerGet(line : LongInt) : LongInt;
    function MarkerNext(lineStart : LongInt; markerMask : LongInt) : LongInt;
    function MarkerPrevious(lineStart : LongInt; markerMask : LongInt) : LongInt;
    procedure MarkerDefinePixmap(markerNumber : LongInt; pixmap : PChar);
    procedure SetMarginTypeN(margin : LongInt; marginType : LongInt);
    function GetMarginTypeN(margin : LongInt) : LongInt;
    procedure SetMarginWidthN(margin : LongInt; pixelWidth : LongInt);
    function GetMarginWidthN(margin : LongInt) : LongInt;
    procedure SetMarginMaskN(margin : LongInt; mask : LongInt);
    function GetMarginMaskN(margin : LongInt) : LongInt;
    procedure SetMarginSensitiveN(margin : LongInt; sensitive : Boolean);
    function GetMarginSensitiveN(margin : LongInt) : Boolean;
    procedure StyleClearAll;
    procedure StyleSetFore(style : LongInt; fore : TColor);
    procedure StyleSetBack(style : LongInt; back : TColor);
    procedure StyleSetBold(style : LongInt; bold : Boolean);
    procedure StyleSetItalic(style : LongInt; italic : Boolean);
    procedure StyleSetSize(style : LongInt; sizePoints : LongInt);
    procedure StyleSetFont(style : LongInt; fontName : PChar);
    procedure StyleSetEOLFilled(style : LongInt; filled : Boolean);
    procedure StyleResetDefault;
    procedure StyleSetUnderline(style : LongInt; underline : Boolean);
    procedure StyleSetCase(style : LongInt; caseForce : LongInt);
    procedure StyleSetCharacterSet(style : LongInt; characterSet : LongInt);
    procedure StyleSetHotSpot(style : LongInt; hotspot : Boolean);
    procedure SetSelFore(useSetting : Boolean; fore : TColor);
    procedure SetSelBack(useSetting : Boolean; back : TColor);
    procedure SetCaretFore(fore : TColor);
    procedure AssignCmdKey(km : LongInt; msg : LongInt);
    procedure ClearCmdKey(km : LongInt);
    procedure ClearAllCmdKeys;
    procedure SetStylingEx(length : LongInt; styles : PChar);
    procedure StyleSetVisible(style : LongInt; visible : Boolean);
    function GetCaretPeriod : LongInt;
    procedure SetCaretPeriod(periodMilliseconds : LongInt);
    procedure SetWordChars(characters : PChar);
    procedure BeginUndoAction;
    procedure EndUndoAction;
    procedure IndicSetStyle(indic : LongInt; style : LongInt);
    function IndicGetStyle(indic : LongInt) : LongInt;
    procedure IndicSetFore(indic : LongInt; fore : TColor);
    function IndicGetFore(indic : LongInt) : TColor;
    procedure SetWhitespaceFore(useSetting : Boolean; fore : TColor);
    procedure SetWhitespaceBack(useSetting : Boolean; back : TColor);
    procedure SetStyleBits(bits : LongInt);
    function GetStyleBits : LongInt;
    procedure SetLineState(line : LongInt; state : LongInt);
    function GetLineState(line : LongInt) : LongInt;
    function GetMaxLineState : LongInt;
    function GetCaretLineVisible : Boolean;
    procedure SetCaretLineVisible(show : Boolean);
    function GetCaretLineBack : TColor;
    procedure SetCaretLineBack(back : TColor);
    procedure StyleSetChangeable(style : LongInt; changeable : Boolean);
    procedure AutoCShow(lenEntered : LongInt; itemList : PChar);
    procedure AutoCCancel;
    function AutoCActive : Boolean;
    function AutoCPosStart : LongInt;
    procedure AutoCComplete;
    procedure AutoCStops(characterSet : PChar);
    procedure AutoCSetSeparator(separatorCharacter : LongInt);
    function AutoCGetSeparator : LongInt;
    procedure AutoCSelect(text : PChar);
    procedure AutoCSetCancelAtStart(cancel : Boolean);
    function AutoCGetCancelAtStart : Boolean;
    procedure AutoCSetFillUps(characterSet : PChar);
    procedure AutoCSetChooseSingle(chooseSingle : Boolean);
    function AutoCGetChooseSingle : Boolean;
    procedure AutoCSetIgnoreCase(ignoreCase : Boolean);
    function AutoCGetIgnoreCase : Boolean;
    procedure UserListShow(listType : LongInt; itemList : PChar);
    procedure AutoCSetAutoHide(autoHide : Boolean);
    function AutoCGetAutoHide : Boolean;
    procedure AutoCSetDropRestOfWord(dropRestOfWord : Boolean);
    function AutoCGetDropRestOfWord : Boolean;
    procedure RegisterImage(type_ : LongInt; xpmData : PChar);
    procedure ClearRegisteredImages;
    function AutoCGetTypeSeparator : LongInt;
    procedure AutoCSetTypeSeparator(separatorCharacter : LongInt);
    procedure AutoCSetMaxWidth(characterCount : LongInt);
    function AutoCGetMaxWidth : LongInt;
    procedure AutoCSetMaxHeight(rowCount : LongInt);
    function AutoCGetMaxHeight : LongInt;
    procedure SetIndent(indentSize : LongInt);
    function GetIndent : LongInt;
    procedure SetUseTabs(useTabs : Boolean);
    function GetUseTabs : Boolean;
    procedure SetLineIndentation(line : LongInt; indentSize : LongInt);
    function GetLineIndentation(line : LongInt) : LongInt;
    function GetLineIndentPosition(line : LongInt) : LongInt;
    function GetColumn(pos : LongInt) : LongInt;
    procedure SetHScrollBar(show : Boolean);
    function GetHScrollBar : Boolean;
    procedure SetIndentationGuides(show : Boolean);
    function GetIndentationGuides : Boolean;
    procedure SetHighlightGuide(column : LongInt);
    function GetHighlightGuide : LongInt;
    function GetLineEndPosition(line : LongInt) : LongInt;
    function GetCodePage : LongInt;
    function GetCaretFore : TColor;
    function GetUsePalette : Boolean;
    function GetReadOnly : Boolean;
    procedure SetCurrentPos(pos : LongInt);
    procedure SetSelectionStart(pos : LongInt);
    function GetSelectionStart : LongInt;
    procedure SetSelectionEnd(pos : LongInt);
    function GetSelectionEnd : LongInt;
    procedure SetPrintMagnification(magnification : LongInt);
    function GetPrintMagnification : LongInt;
    procedure SetPrintColourMode(mode : LongInt);
    function GetPrintColourMode : LongInt;
    function FindTextX(flags : LongInt; ft : PTextToFind) : LongInt;
    function FormatRange(draw : Boolean; fr : PRangeToFormat) : LongInt;
    function GetFirstVisibleLine : LongInt;
    function GetLine(line : LongInt; text : PChar) : LongInt;
    function GetLineCount : LongInt;
    procedure SetMarginLeft(pixelWidth : LongInt);
    function GetMarginLeft : LongInt;
    procedure SetMarginRight(pixelWidth : LongInt);
    function GetMarginRight : LongInt;
    function GetModify : Boolean;
    procedure SetSel(start : LongInt; end_ : LongInt);
    function GetSelText(text : PChar) : LongInt;
    function GetTextRange(tr : PTextRange) : LongInt;
    procedure HideSelection(normal : Boolean);
    function PointXFromPosition(pos : LongInt) : LongInt;
    function PointYFromPosition(pos : LongInt) : LongInt;
    function LineFromPosition(pos : LongInt) : LongInt;
    function PositionFromLine(line : LongInt) : LongInt;
    procedure LineScroll(columns : LongInt; lines : LongInt);
    procedure ScrollCaret;
    procedure ReplaceSel(text : PChar);
    procedure SetReadOnly(readOnly : Boolean);
    procedure Null;
    function CanPaste : Boolean;
    function CanUndo : Boolean;
    procedure EmptyUndoBuffer;
    procedure Undo;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Clear;
    procedure SetText(text : PChar);
    function GetText(length : LongInt; text : PChar) : LongInt;
    function GetTextLength : LongInt;
    function GetDirectFunction : LongInt;
    function GetDirectPointer : LongInt;
    procedure SetOvertype(overtype : Boolean);
    function GetOvertype : Boolean;
    procedure SetCaretWidth(pixelWidth : LongInt);
    function GetCaretWidth : LongInt;
    procedure SetTargetStart(pos : LongInt);
    function GetTargetStart : LongInt;
    procedure SetTargetEnd(pos : LongInt);
    function GetTargetEnd : LongInt;
    function ReplaceTarget(length : LongInt; text : PChar) : LongInt;
    function ReplaceTargetRE(length : LongInt; text : PChar) : LongInt;
    function SearchInTarget(length : LongInt; text : PChar) : LongInt;
    procedure SetSearchFlags(flags : LongInt);
    function GetSearchFlags : LongInt;
    procedure CallTipShow(pos : LongInt; definition : PChar);
    procedure CallTipCancel;
    function CallTipActive : Boolean;
    function CallTipPosStart : LongInt;
    procedure CallTipSetHlt(start : LongInt; end_ : LongInt);
    procedure CallTipSetBack(back : TColor);
    procedure CallTipSetFore(fore : TColor);
    procedure CallTipSetForeHlt(fore : TColor);
    function VisibleFromDocLine(line : LongInt) : LongInt;
    function DocLineFromVisible(lineDisplay : LongInt) : LongInt;
    function WrapCount(line : LongInt) : LongInt;
    procedure SetFoldLevel(line : LongInt; level : LongInt);
    function GetFoldLevel(line : LongInt) : LongInt;
    function GetLastChild(line : LongInt; level : LongInt) : LongInt;
    function GetFoldParent(line : LongInt) : LongInt;
    procedure ShowLines(lineStart : LongInt; lineEnd : LongInt);
    procedure HideLines(lineStart : LongInt; lineEnd : LongInt);
    function GetLineVisible(line : LongInt) : Boolean;
    procedure SetFoldExpanded(line : LongInt; expanded : Boolean);
    function GetFoldExpanded(line : LongInt) : Boolean;
    procedure ToggleFold(line : LongInt);
    procedure EnsureVisible(line : LongInt);
    procedure SetFoldFlags(flags : LongInt);
    procedure EnsureVisibleEnforcePolicy(line : LongInt);
    procedure SetTabIndents(tabIndents : Boolean);
    function GetTabIndents : Boolean;
    procedure SetBackSpaceUnIndents(bsUnIndents : Boolean);
    function GetBackSpaceUnIndents : Boolean;
    procedure SetMouseDwellTime(periodMilliseconds : LongInt);
    function GetMouseDwellTime : LongInt;
    function WordStartPosition(pos : LongInt; onlyWordCharacters : Boolean) : LongInt;
    function WordEndPosition(pos : LongInt; onlyWordCharacters : Boolean) : LongInt;
    procedure SetWrapMode(mode : LongInt);
    function GetWrapMode : LongInt;
    procedure SetWrapVisualFlags(wrapVisualFlags : LongInt);
    function GetWrapVisualFlags : LongInt;
    procedure SetWrapVisualFlagsLocation(wrapVisualFlagsLocation : LongInt);
    function GetWrapVisualFlagsLocation : LongInt;
    procedure SetWrapStartIndent(indent : LongInt);
    function GetWrapStartIndent : LongInt;
    procedure SetLayoutCache(mode : LongInt);
    function GetLayoutCache : LongInt;
    procedure SetScrollWidth(pixelWidth : LongInt);
    function GetScrollWidth : LongInt;
    function TextWidth(style : LongInt; text : PChar) : LongInt;
    procedure SetEndAtLastLine(endAtLastLine : Boolean);
    function GetEndAtLastLine : LongInt;
    function TextHeight(line : LongInt) : LongInt;
    procedure SetVScrollBar(show : Boolean);
    function GetVScrollBar : Boolean;
    procedure AppendText(length : LongInt; text : PChar);
    function GetTwoPhaseDraw : Boolean;
    procedure SetTwoPhaseDraw(twoPhase : Boolean);
    procedure TargetFromSelection;
    procedure LinesJoin;
    procedure LinesSplit(pixelWidth : LongInt);
    procedure SetFoldMarginColour(useSetting : Boolean; back : TColor);
    procedure SetFoldMarginHiColour(useSetting : Boolean; fore : TColor);
    procedure LineDown;
    procedure LineDownExtend;
    procedure LineUp;
    procedure LineUpExtend;
    procedure CharLeft;
    procedure CharLeftExtend;
    procedure CharRight;
    procedure CharRightExtend;
    procedure WordLeft;
    procedure WordLeftExtend;
    procedure WordRight;
    procedure WordRightExtend;
    procedure Home;
    procedure HomeExtend;
    procedure LineEnd;
    procedure LineEndExtend;
    procedure DocumentStart;
    procedure DocumentStartExtend;
    procedure DocumentEnd;
    procedure DocumentEndExtend;
    procedure PageUp;
    procedure PageUpExtend;
    procedure PageDown;
    procedure PageDownExtend;
    procedure EditToggleOvertype;
    procedure Cancel;
    procedure DeleteBack;
    procedure Tab;
    procedure BackTab;
    procedure NewLine;
    procedure FormFeed;
    procedure VCHome;
    procedure VCHomeExtend;
    procedure ZoomIn;
    procedure ZoomOut;
    procedure DelWordLeft;
    procedure DelWordRight;
    procedure LineCut;
    procedure LineDelete;
    procedure LineTranspose;
    procedure LineDuplicate;
    procedure LowerCase;
    procedure UpperCase;
    procedure LineScrollDown;
    procedure LineScrollUp;
    procedure DeleteBackNotLine;
    procedure HomeDisplay;
    procedure HomeDisplayExtend;
    procedure LineEndDisplay;
    procedure LineEndDisplayExtend;
    procedure HomeWrap;
    procedure HomeWrapExtend;
    procedure LineEndWrap;
    procedure LineEndWrapExtend;
    procedure VCHomeWrap;
    procedure VCHomeWrapExtend;
    procedure LineCopy;
    procedure MoveCaretInsideView;
    function LineLength(line : LongInt) : LongInt;
    procedure BraceHighlight(pos1 : LongInt; pos2 : LongInt);
    procedure BraceBadLight(pos : LongInt);
    function BraceMatch(pos : LongInt) : LongInt;
    function GetViewEOL : Boolean;
    procedure SetViewEOL(visible : Boolean);
    function GetDocPointer : LongInt;
    procedure SetDocPointer(pointer : LongInt);
    procedure SetModEventMask(mask : LongInt);
    function GetEdgeColumn : LongInt;
    procedure SetEdgeColumn(column : LongInt);
    function GetEdgeMode : LongInt;
    procedure SetEdgeMode(mode : LongInt);
    function GetEdgeColour : TColor;
    procedure SetEdgeColour(edgeColour : TColor);
    procedure SearchAnchor;
    function SearchNext(flags : LongInt; text : PChar) : LongInt;
    function SearchPrev(flags : LongInt; text : PChar) : LongInt;
    function LinesOnScreen : LongInt;
    procedure UsePopUp(allowPopUp : Boolean);
    function SelectionIsRectangle : Boolean;
    procedure SetZoom(zoom : LongInt);
    function GetZoom : LongInt;
    function CreateDocument : LongInt;
    procedure AddRefDocument(doc : LongInt);
    procedure ReleaseDocument(doc : LongInt);
    function GetModEventMask : LongInt;
    procedure SetFocusEx(focus : Boolean);
    function GetFocus : Boolean;
    procedure SetStatus(statusCode : LongInt);
    function GetStatus : LongInt;
    procedure SetMouseDownCaptures(captures : Boolean);
    function GetMouseDownCaptures : Boolean;
    procedure SetCursor(cursorType : LongInt);
    function GetCursor : LongInt;
    procedure SetControlCharSymbol(symbol : LongInt);
    function GetControlCharSymbol : LongInt;
    procedure WordPartLeft;
    procedure WordPartLeftExtend;
    procedure WordPartRight;
    procedure WordPartRightExtend;
    procedure SetVisiblePolicy(visiblePolicy : LongInt; visibleSlop : LongInt);
    procedure DelLineLeft;
    procedure DelLineRight;
    procedure SetXOffset(newOffset : LongInt);
    function GetXOffset : LongInt;
    procedure ChooseCaretX;
    procedure GrabFocus;
    procedure SetXCaretPolicy(caretPolicy : LongInt; caretSlop : LongInt);
    procedure SetYCaretPolicy(caretPolicy : LongInt; caretSlop : LongInt);
    procedure SetPrintWrapMode(mode : LongInt);
    function GetPrintWrapMode : LongInt;
    procedure SetHotspotActiveFore(useSetting : Boolean; fore : TColor);
    procedure SetHotspotActiveBack(useSetting : Boolean; back : TColor);
    procedure SetHotspotActiveUnderline(underline : Boolean);
    procedure SetHotspotSingleLine(singleLine : Boolean);
    procedure ParaDown;
    procedure ParaDownExtend;
    procedure ParaUp;
    procedure ParaUpExtend;
    function PositionBefore(pos : LongInt) : LongInt;
    function PositionAfter(pos : LongInt) : LongInt;
    procedure CopyRange(start : LongInt; end_ : LongInt);
    procedure CopyText(length : LongInt; text : PChar);
    procedure SetSelectionMode(mode : LongInt);
    function GetSelectionMode : LongInt;
    function GetLineSelStartPosition(line : LongInt) : LongInt;
    function GetLineSelEndPosition(line : LongInt) : LongInt;
    procedure LineDownRectExtend;
    procedure LineUpRectExtend;
    procedure CharLeftRectExtend;
    procedure CharRightRectExtend;
    procedure HomeRectExtend;
    procedure VCHomeRectExtend;
    procedure LineEndRectExtend;
    procedure PageUpRectExtend;
    procedure PageDownRectExtend;
    procedure StutteredPageUp;
    procedure StutteredPageUpExtend;
    procedure StutteredPageDown;
    procedure StutteredPageDownExtend;
    procedure WordLeftEnd;
    procedure WordLeftEndExtend;
    procedure WordRightEnd;
    procedure WordRightEndExtend;
    procedure SetWhitespaceChars(characters : PChar);
    procedure SetCharsDefault;
    function AutoCGetCurrent : LongInt;
    procedure Allocate(bytes : LongInt);
    function FindColumn(line : LongInt; column : LongInt) : LongInt;
    function GetCaretSticky : Boolean;
    procedure SetCaretSticky(useCaretStickyBehaviour : Boolean);
    procedure ToggleCaretSticky;
    procedure StartRecord;
    procedure StopRecord;
    procedure SetLexer(lexer : LongInt);
    function GetLexer : LongInt;
    procedure Colourise(start : LongInt; end_ : LongInt);
    procedure SetProperty(key : PChar; value : PChar);
    procedure SetKeyWords(keywordSet : LongInt; keyWords : PChar);
    procedure SetLexerLanguage(language : PChar);
    procedure LoadLexerLibrary(path : PChar);
//--FuncDef
    procedure DeleteWordRightAndSkip;
  protected
    //++EventProperties
    property OnStyleNeeded : TSCEvent_styleneeded read FOnstyleneeded write FOnstyleneeded;
    property OnCharAdded : TSCEvent_charadded read FOncharadded write FOncharadded;
    property OnSavePointReached : TSCEvent_savepointreached read FOnsavepointreached write FOnsavepointreached;
    property OnSavePointLeft : TSCEvent_savepointleft read FOnsavepointleft write FOnsavepointleft;
    property OnModifyAttemptRO : TSCEvent_modifyattemptro read FOnmodifyattemptro write FOnmodifyattemptro;
    property OnKey : TSCEvent_key read FOnkey write FOnkey;
    property OnDoubleClick : TSCEvent_doubleclick read FOndoubleclick write FOndoubleclick;
    property OnUpdateUI : TSCEvent_updateui read FOnupdateui write FOnupdateui;
    property OnModified : TSCEvent_modified read FOnmodified write FOnmodified;
    property OnMacroRecord : TSCEvent_macrorecord read FOnmacrorecord write FOnmacrorecord;
    property OnMarginClick : TSCEvent_marginclick read FOnmarginclick write FOnmarginclick;
    property OnNeedShown : TSCEvent_needshown read FOnneedshown write FOnneedshown;
    property OnPainted : TSCEvent_painted read FOnpainted write FOnpainted;
    property OnUserListSelection : TSCEvent_userlistselection read FOnuserlistselection write FOnuserlistselection;
    property OnDwellStart : TSCEvent_dwellstart read FOndwellstart write FOndwellstart;
    property OnDwellEnd : TSCEvent_dwellend read FOndwellend write FOndwellend;
    property OnZoom : TSCEvent_zoom read FOnzoom write FOnzoom;
    property OnHotSpotClick : TSCEvent_hotspotclick read FOnhotspotclick write FOnhotspotclick;
    property OnHotSpotDoubleClick : TSCEvent_hotspotdoubleclick read FOnhotspotdoubleclick write FOnhotspotdoubleclick;
    property OnCallTipClick : TSCEvent_calltipclick read FOncalltipclick write FOncalltipclick;
    property OnAutoCSelection : TSCEvent_autocselection read FOnautocselection write FOnautocselection;
    //--EventProperties
    property OnMsgSent : TSCEventMsgSent read FOnMsgSent Write FOnMsgSent;
  public
		procedure CopyFrom(Source: TScintillaBase); virtual;
		procedure LoadFromStream(Stream : TStream);
		procedure LoadFromFile(const FileName : TFileName);
		procedure  SaveToStream(Stream : TStream);
		procedure SaveToFile(const FileName : TFileName);
		procedure ClearDocument;


		// These functions retrieves text
		// Get the range from 'start' to 'end_' into the buffer 'text'
	  procedure GetRange(const start, end_ : LongInt; text : PChar);
		// Get the line 'linenum' into the buffer 'textbuf' of size 'sizeText'
	  procedure GetLineInBuf(textbuf : PChar; const sizeText : LongInt; linenum : LongInt=-1);
    // Get the line as a AnsiString
		function  GetLineS(const Index: LongInt=-1): string;

		//***** Assorted selection functions BEGIN *****//
    // Returns start and end position for selection
		function  GetSelectionRng      : TCharacterRange;
    // Returns start and end_ positions for the current word.
		procedure FindWordAtCaret(var start : LongInt;var end_ : LongInt);
			// Extend selection both ways while ischarforsel function returns true
		function 	RangeExtendAndGrab(var selStart,selEnd : LongInt;ischarforsel : TTestingFunction;const stripEol : Boolean=true) : String;
			// Extend selection both ways while ischarforsel function returns true
		function 	SelectionExtend(ischarforsel : TTestingFunction;const stripEol : Boolean =true) : String;
		procedure SetSelectionS(const anchor, end_ : LongInt);      // Set selection start and end

    // Selects the word under the cursor.
		function 	SelectWordAtCaret    : boolean;
    // Selects the word under the cursor determined by IsWordChar.
		function  SelectionWord(const stripEol : Boolean =true) : String;
    // Retrieved text by style 'styleNum'
    function  SelectByStyle(const styleNum : Integer;const xpos : Integer=-1;const mask : Integer=$1f) : String;
    // Sets selection by style 'styleNum'
    function  SetSelectionByStyle(const styleNum : Integer;const xpos : Integer=-1;const mask : Integer=$1f) : Boolean;
    // Selects the filename under the cursor determined by IsFilenameChar.
		function  SelectionFileName    : String;

		//***** Assorted selection functions END		*****//
    // Get length of line, -1 is current line
		function  GetLineLength(const line : LongInt=-1) : LongInt;
		function  GetCurrentScrollPosition : LongInt;
    // Get Caret Position in current line
	  function  GetCaretInLine       : LongInt;
    // Returns the current linenumber
	  function  GetCurrentLineNumber : LongInt;
    // Test if the range are only whitespace
		function  RangeIsAllWhiteSpace(const start, end_ : LongInt) : boolean;
		//Ensure that the range is visible
		procedure EnsureRangeVisible(const PosStart, PosEnd: LongInt);
    // Goto line and ensure it's visible
		procedure GotoLineEnsureVisible(const line : LongInt);

			{ Ensure a text range is visible - make visible if necessary }
    function   ExecuteAction(Action: TBasicAction): boolean; override;
      { Standard Delphi edit action support }
		function   UpdateAction(Action: TBasicAction): boolean; override;
      { Standard Delphi edit action support }
		procedure  SetModified(const Value: Boolean);
		//Make the text buffer read-only. Can't use the lines property either when readonly.

		property   ReadOnly    				 : Boolean read GetReadOnly write SetReadOnly default false;
		//Setting to true allows the use of multiple language in the same document.
    //If true all text in and out of the control is interpreted as UTF8
    property   UseUnicode  				 : Boolean read GetUseUnicode write SetUseUnicode default false;
		property   SelLength           : LongInt read GetSelectionLength write SetSelectionLength;
		property   SelStart            : LongInt read GetSelectionStart write SetSelStart;
		property   SelText             : string read GetSelection write SetSelection;
		property   WordWrap    				 : TWordWrapType read GetWordWrap write SetWordWrap;
		property   ClearUndoAfterSave  : boolean read FClearUndoAfterSave write FClearUndoAfterSave default False;
    property   Dummy               : Boolean read FDummy Write SetDummy; //  Added by KV
		//Flag to indicate whether the text buffer has been modified
		property   Modified            : Boolean read GetModify write SetModified;
		property   WordChars			     : String read FWordChars write inSetWordChars;
	published
		property   Color;
		property   Font;
		property   PopupMenu;
    property   Align;
    property   Hint;
    property   ShowHint;
    property   ParentShowHint;
		property   ParentFont default False;
    property   BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
		property   Visible;
    property   ForceMouseRelease : Boolean read FForceMouseRelease write FForceMouseRelease default True;
	end;
  //Returns the current version number of DelphiSci
  function GetDelphiSciVersion : Integer;
  //Returns the Copyright string for DelphiSci
  function GetDelphiSciVersionStr : String;
  //Returns the URL where you can obtain a copy of DelphiSci
  function GetDelphiSciURL : String;
var
	fMsgSentDummyProc : TSCEventMsgSent;
implementation
Uses
	Math,sciUtils,SciResLang;
var
	scmod : HMODULE;

//Include the file with the version string.
{$Include SciVer.inc}

const DelphiSciVersionStr='Delphi Scintilla Interface Components';

function GetDelphiSciVersion : Integer;
begin
  Result:=DelphiSciVersion;
end;
function GetDelphiSciVersionStr : String;
var
  NowYear : String;
begin
  NowYear:=FormatDateTime('yyyy',Date());
  Result:=Format(DelphiSciVersionStr+' v%d.%d Copyright © 2004-%s, Jan Martin Pettersen',[HIWORD(DelphiSciVersion),LOWORD(DelphiSciVersion),NowYear]);
end;
function GetDelphiSciURL : String;
begin
  Result:='http://delphisci.sourceforge.net';
end;

// TScintilla
constructor TScintillaBase.Create(AOwner : TComponent);
begin
	if scmod = 0 then
		raise Exception.CreateRes(@sSciLexerNotFound);
	inherited;
  initing:=false;
	Width := 185;
	Height := 89;
	AutoSize := False;
	TabStop := True;
	FDirty :=False;
	FBorderStyle := bsSingle;
	FWantTabs := True;
	FWantReturns := True;
	ParentFont := False;
	ParentColor := False;
	ParentShowHint := True;
	ParentBiDiMode := False;
	FWordChars :='_'+sci_alphachars+sci_numericchars;
  FClearUndoAfterSave:=False;
  FForceMouseRelease:=True;
	ControlStyle:=ControlStyle+[csClickEvents,csDoubleClicks,csCaptureMouse,csReflector];
end;

destructor TScintillaBase.Destroy;
begin
  if Assigned(FStateStream) then
    FreeAndNil(FStateStream);
  inherited;
end;

procedure TScintillaBase.CreateWnd;
begin
  FCreating := True;
  try
		inherited CreateWnd;
  finally
    FCreating := False;
  end;
    @SCPerform := Pointer( SendMessage(WindowHandle, SCI_GETDIRECTFUNCTION,0,0) );
    sccmdctr := Pointer( SendMessage(WindowHandle, SCI_GETDIRECTPOINTER,0,0) );
  if (ComponentState * [csLoading, csReading] = []) and assigned(FStateStream) then
	begin
    fStateStream.Position := 0;
    fStateStream.ReadComponent(Self);
    FreeAndNil(fStateStream);
  end;
    Perform(CM_FONTCHANGED, 0, 0);
	 	Perform(CM_COLORCHANGED, 0, 0);
end;

procedure TScintillaBase.WndDestroy;
begin
	if not (csDestroying in ComponentState) then
	begin
		if Assigned(FStateStream) then
			FStateStream.Position := 0
		else
			FStateStream := TMemoryStream.Create;
		FStateStream.WriteComponent(Self);
  end;
end;

procedure TScintillaBase.DestroyWnd;
begin
	WndDestroy;
  inherited DestroyWnd;
end;

procedure   TScintillaBase.DisableAllEvents(disable : Boolean);
begin
  if disable then
  begin
    oldmodeventmask:=GetModEventMask;
    SetModEventMask(0);
  end
  else
    begin
      if oldmodeventmask<>0 then
        SetModEventMask(oldmodeventmask);
      oldmodeventmask:=0;
    end;
end;
procedure TScintillaBase.inSetWordChars(const Value : String);
begin
	FWordChars:=Value;
	if(FWordChars='') then
		FWordChars:='_'+sci_alphachars;
	SetWordChars(PChar(FWordChars));
end;

procedure TScintillaBase.CopyFrom(Source: TScintillaBase);
begin
  AutoSize := False;
  TabStop := TScintillaBase(Source).TabStop;
  ParentShowHint := TScintillaBase(Source).ParentShowHint;
  ParentBiDiMode := TScintillaBase(Source).ParentBiDiMode;
  UseUnicode:=TScintillaBase(Source).UseUnicode;
  ReadOnly:=TScintillaBase(Source).ReadOnly;
  ClearUndoAfterSave:=TScintillaBase(Source).ClearUndoAfterSave;
  Color :=TScintillaBase(Source).Color;
  WordChars:=TScintillaBase(Source).WordChars;
  Font.Assign(TScintillaBase(Source).Font);
end;


//haer
procedure TScintillaBase.GotoLineEnsureVisible(const line : LongInt);
begin
	EnsureVisibleEnforcePolicy(line);
	GotoLine(line);
end;

function TScintillaBase.GetLineS(const Index: LongInt): string;
var
  S : string;
  Len,linenum : LongInt;
begin
	if(Index<0) then
    linenum:=GetCurrentLineNumber
  else
		linenum :=Index;
  Len := GetLineLength(linenum);
  if Len > 0 then
  begin
    SetLength(S, Len+2);
    GetLine(linenum, PChar(S));
		Result := System.Copy(S, 1, Len);
  end else
    Result := '';
end;

procedure TScintillaBase.GetLineInBuf(textbuf : PChar; const sizeText : LongInt; linenum : LongInt);
var
	lineStart,lineEnd,lineMax: LongInt;
begin
	if linenum < 0 then
		linenum := GetCurrentLineNumber;
	lineStart := PositionFromLine(linenum);
	lineEnd := GetLineEndPosition(linenum);
	lineMax := lineStart + sizeText - 1;
	if lineEnd > lineMax then
    lineEnd := lineMax;
	GetRange(lineStart, lineEnd, textbuf);
	textbuf[lineEnd - lineStart] := Char(0);
end;

function TScintillaBase.GetCurrentLineNumber : LongInt;
begin
	Result :=LineFromPosition(GetCurrentPos);
end;

function TScintillaBase.GetCaretInLine : LongInt;
var
	caret,line,lineStart : LongInt;
begin
	caret := GetCurrentPos;
	line := LineFromPosition(caret);
	lineStart := PositionFromLine(line);
	Result :=caret - lineStart;
end;

procedure TScintillaBase.GetRange(const start, end_ : LongInt; text: PChar);
var
	tr : TTextRange;
begin
	tr.chrg.cpMin := start;
	tr.chrg.cpMax := end_;
	tr.lpstrText := text;
	GetTextRange(@tr);
end;

procedure TScintillaBase.SetSelectionS(const anchor, end_ : LongInt); // Set selection start and end
begin
	SetSel(anchor, end_);
end;

function  TScintillaBase.GetSelectionRng : TCharacterRange;
var
	crange : TCharacterRange;
begin
	crange.cpMin := GetSelectionStart;
	crange.cpMax := GetSelectionEnd;
	Result :=crange;
end;

function TScintillaBase.SelectionWord(const stripEol : boolean) : String;
begin
	Result:=SelectionExtend(IsWordChar, stripEol);
end;

function TScintillaBase.SelectionFileName : String;
begin
	Result:=SelectionExtend(IsFilenameChar, true);
end;

function IsEOL(const ch : Integer) : Boolean;
begin
  if (ch=10) or (ch=13) then
    Result:=true
  else
    Result:=false
end;

function  TScintillaBase.SetSelectionByStyle(const styleNum : Integer;const xpos : Integer;const mask : Integer) : Boolean;
var
  start,end_ : Integer;
  doclen : Integer;
  sstyl : Integer;
begin
  doclen :=GetTextLength;
  if xpos=-1 then
  begin
    start:=GetSelectionStart;
    end_:=GetSelectionEnd;
    if(end_-start<=0) then
    begin
      start:=GetCurrentPos;
      end_:=start;
    end;
  end else
  begin
    start:=xpos;
    end_:=start;
  end;    
  Result:=False;
  if(start>0) then
  begin
    repeat
      sstyl:=GetStyleAt(start);
      if sstyl=styleNum then
        start:=start-1
      else
        start:=start+1;
    until (start<=0) or (sstyl<>styleNum);
  end;
  if(end_<(doclen-1)) then
  begin
    repeat
      sstyl:=GetStyleAt(end_);
      if sstyl=styleNum then
        end_:=end_+1;
    until (end_>=(doclen-1)) or (sstyl<>styleNum);
  end;
  if (end_-start>0) then
  begin
    SetSel(start,end_);
    Result:=True;
  end;
end;
function  TScintillaBase.SelectByStyle(const styleNum : Integer;const xpos : Integer;const mask : Integer) : String;
var
  sellen : Integer;
  selected : String;
  start,end_ : Integer;
  doclen : Integer;
  sstyl : Integer;
begin
  doclen :=GetTextLength;
  if xpos=-1 then
  begin
    start:=GetSelectionStart;
    end_:=GetSelectionEnd;
    if(end_-start<=0) then
    begin
      start:=GetCurrentPos;
      end_:=start;
    end;
  end else
  begin
    start:=xpos;
    end_:=start;
  end; 
  Result:='';
  if(start>0) then
  begin
    repeat
      sstyl:=GetStyleAt(start);
      if sstyl=styleNum then
        start:=start-1
      else
        start:=start+1;
    until (start<=0) or (sstyl<>styleNum);
  end;
  if(end_<(doclen-1)) then
  begin
    repeat
      sstyl:=GetStyleAt(end_);
      if sstyl=styleNum then
        end_:=end_+1;
    until (end_>=(doclen-1)) or (sstyl<>styleNum);
  end;
  sellen:=end_-start;
  if (sellen)>0 then
  begin
    SetLength(selected,sellen);
    GetRange(start,end_,PChar(selected));
  end;
  sellen :=Length(selected);
  if (sellen>=2) and (selected[sellen-2]=#13) and (selected[sellen-1]=#10) then
  begin
    Delete(selected,sellen-2,sellen+2);
  end else if (sellen>=1) and ((selected[sellen-1]=#13) or (selected[sellen-1]=#10)) then
  begin
    Delete(selected,sellen-1,sellen+2);
  end;
  Result:=selected;
end;

function TScintillaBase.SelectionExtend(ischarforsel : TTestingFunction;const stripEol : boolean) : String;
var
	selStart,selEnd : LongInt;
begin
	selStart := GetSelectionStart;
	selEnd := GetSelectionEnd;
	result :=RangeExtendAndGrab(selStart, selEnd, ischarforsel, stripEol);
end;

procedure TScintillaBase.FindWordAtCaret(var start : LongInt;var end_ : LongInt);
begin
	start := GetSelectionStart;
	end_ := GetSelectionEnd;
	// Call just to update start & end
	RangeExtendAndGrab(start, end_, IsWordChar, false);
end;

function TScintillaBase.SelectWordAtCaret : boolean;
var
	selStart,selEnd : LongInt;
begin
	selStart :=0;
	selEnd :=0;
	FindWordAtCaret(selStart, selEnd);
	SetSelectionS(selStart, selEnd);
	result :=boolean(selStart <> selEnd);
end;

function TScintillaBase.RangeExtendAndGrab(var selStart,selEnd : LongInt;ischarforsel : TTestingFunction;const stripEol : Boolean) : String;
var
	lengthDoc,len,sellen : LongInt;
	selected,sel : String;
begin
	if (selStart = selEnd) and assigned(ischarforsel) then
	begin
		lengthDoc :=GetLength;
		while ((selStart > 0) and (ischarforsel(GetCharAt(selStart - 1)))) do
		begin
			selStart:=selStart-1;
		end;
		while ((selEnd < lengthDoc) and (ischarforsel(GetCharAt(selEnd)))) do
		begin
			selEnd:=selEnd+1;
		end;
		len:=selEnd-selStart;
		if len>0 then
		begin
			SetLength(sel,len);
			GetRange(selStart, selEnd, PChar(sel));
			selected := sel;
		end;
		if stripEol then
		begin
			sellen :=Length(selected);
			if (sellen>=2) and (selected[sellen-2]=#13) and (selected[sellen-1]=#10) then
			begin
				Delete(selected,sellen-2,sellen+2);
			end else if (sellen>=1) and ((selected[sellen-1]=#13) or (selected[sellen-1]=#10)) then
			begin
				Delete(selected,sellen-1,sellen+2);
			end;
		end;
	end;
	result :=selected;
end;

function TScintillaBase.RangeIsAllWhiteSpace(const start, end_ : LongInt) : boolean;
var
	i : LongInt;
begin
	for i:=start to (end_-1) do
	begin
		if not ((Char(GetCharAt(i)) in [' ',#9])) then
		begin
			result :=false;
			Exit;
		end;
	end;
	result :=true;
end;
function  TScintillaBase.GetLineLength(const line : LongInt) : LongInt;
var
	linenum : LongInt;
begin
	if line=-1 then
		linenum:=GetCurrentLineNumber
	else linenum:=line;
	result :=GetLineEndPosition(linenum)-PositionFromLine(linenum);
end;

procedure TScintillaBase.EnsureRangeVisible(const PosStart, PosEnd: LongInt);
Var
  LineStart, LineEnd, i : LongInt;
begin
  LineStart := LineFromPosition(PosStart);
  LineEnd := LineFromPosition(PosEnd);
  for i := Min(LineStart, LineEnd) to Max(LineStart, LineEnd) do
    EnsureVisible(i);
end;

//haer
function TScintillaBase.GetSelectionLength : LongInt;
begin
	result:=GetSelectionEnd-GetSelectionStart;
end;

procedure TScintillaBase.SetSelectionLength(const Value : LongInt);
var
	start : LongInt;
begin
	start :=GetSelectionStart;
	SetSelectionEnd(start+Value);
end;


procedure TScintillaBase.SetSelStart(const value : LongInt);
begin
	SetSel(Value,value);
end;

function TScintillaBase.GetSelection: string;
Var
  L : LongInt;
begin
  L := GetSelectionEnd - GetSelectionStart;
  if L > 0 then
  begin
    //SetLength(Result, L);
    // bug in Scintilla >= 1.54
    // was introduced sometime after v1.49
    SetLength(Result, L+1);
    GetSelText(Pchar(Result));
    Result := System.Copy(Result, 1, L);
  end else
    Result := '';
end;

procedure TScintillaBase.SetSelection(const Value: string);
begin
  ReplaceSel(PChar(Value));
end;

procedure TScintillaBase.SetBorderStyle(Value: TBorderStyle);
begin
  If FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TScintillaBase.CMCtl3DChanged(var Message: TMessage);
begin
	if NewStyleControls and (FBorderStyle = bsSingle) then
  begin
    RecreateWnd;
  end;
  inherited;
end;

procedure TScintillaBase.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'Scintilla');
  with Params do
  begin
    Style := WS_CHILD or WS_VSCROLL or WS_HSCROLL or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;


procedure TScintillaBase.DefaultHandler(var Message);
begin

  case TMessage(Message).Msg of
    WM_SETFOCUS:
      if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and not IsWindow(TWMSetFocus(Message).FocusedWnd) then
				TWMSetFocus(Message).FocusedWnd := 0;
  end;

  inherited;
end;

procedure TScintillaBase.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTCHARS or DLGC_WANTARROWS; //BUGFIX 01
  if FWantTabs then
		Message.Result := Message.Result or DLGC_WANTTAB
  else
		Message.Result := Message.Result and not DLGC_WANTTAB;
	if not FWantReturns then
    Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TScintillaBase.WMNCDestroy(var Message: TWMNCDestroy);
begin
  inherited;
end;

procedure TScintillaBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Inherited;
  if (FForceMouseRelease) then
  if (Shift * [ssLeft,ssRight]=[]) and
     ((X<0) or (Y<0) or (Y>Height) or (X>Width)) then // If we are outside of the control
    SetCaptureControl(nil); // Release capture
end;
function TScintillaBase.GetWordWrap: TWordWrapType;
begin
  Result := TWordWrapType(Self.GetWrapMode);
end;

procedure TScintillaBase.SetWordWrap(const Value: TWordWrapType);
begin
  SetWrapMode(LongInt(Value));
end;

procedure TScintillaBase.WMNotify(var Message: TWMNotify);
var nmh : PNMHdr;
  scn : PSCNotification;
begin
  nmh := Message.NMHdr;
  if (nmh^.hwndFrom = Handle) then
  begin
    scn := PSCNotification(TMessage(Message).LParam);
    case nmh^.code of
      0 : ;
    //++EventImpl
      2000 : doSciStyleNeeded(scn^.position);
      2001 : doSciCharAdded(scn^.ch);
      2002 : doSciSavePointReached;
      2003 : doSciSavePointLeft;
      2004 : doSciModifyAttemptRO;
      2005 : doSciKey(scn^.ch, scn^.modifiers);
      2006 : doSciDoubleClick;
      2007 : doSciUpdateUI;
      2008 : doSciModified(scn^.position, scn^.modificationType, scn^.text, scn^.length, scn^.linesAdded, scn^.line, scn^.foldLevelNow, scn^.foldLevelPrev);
      2009 : doSciMacroRecord(scn^.message, scn^.wParam, scn^.lParam);
      2010 : doSciMarginClick(scn^.modifiers, scn^.position, scn^.margin);
      2011 : doSciNeedShown(scn^.position, scn^.length);
      2013 : doSciPainted;
      2014 : doSciUserListSelection(scn^.listType, scn^.text);
      2016 : doSciDwellStart(scn^.position);
      2017 : doSciDwellEnd(scn^.position);
      2018 : doSciZoom;
      2019 : doSciHotSpotClick(scn^.modifiers, scn^.position);
      2020 : doSciHotSpotDoubleClick(scn^.modifiers, scn^.position);
      2021 : doSciCallTipClick(scn^.position);
      2022 : doSciAutoCSelection(scn^.text);
    //--EventImpl
    end;
  end;
  inherited;
end;


function TScintillaBase.SPerform(Msg, wParam, lParam: Integer) : LongInt;
begin
  HandleNeeded;  // KV
  Result := SCPerform(sccmdctr, Msg, wParam, lParam);
end;


procedure TScintillaBase.doSciCharAdded(const ch : Integer);
begin
	if Assigned(FOncharadded) then FOncharadded(Self,ch);
end;

{$Ifdef USEWMERASE}
procedure TScintillaBase.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	// If we don't do this, then in design mode we'll look really ugly...
	if ((csDesigning in ComponentState) and not (TMessage(Message).wParam = TMessage(Message).lParam)) then
		FillRect(Message.DC, ClientRect, GetStockObject(WHITE_BRUSH));

	Message.Result := 1;
end;
{$Endif}
// The following code is used to allow an external object to receive all the
// calls to the scintilla control - instead of calling scintilla we just pass
// on the messages. To enable this, set up a OnMsgSent handler _FIRST_, and then
// set Dummy to true. This cannot be un-toggled!

function dummySCPerform(ptr: Pointer; Msg, wParam,lParam: Integer): LongInt; cdecl;
begin
  if Assigned(fMsgSentDummyProc) then
    fMsgSentDummyProc(nil, Msg, wParam, lParam);
  Result := 0;
end;

procedure TScintillaBase.SetDummy(Value: Boolean);
begin
  SCPerform := @dummySCPerform;
  fMsgSentDummyProc := FOnMsgSent;
  FDummy := True;
end;


procedure TScintillaBase.CMFontChanged(var Message: TMessage);
begin
  inherited;
  StyleSetFont(STYLE_DEFAULT, PChar(Font.Name));
  StyleSetSize(STYLE_DEFAULT, Font.Size);
	StyleSetCharacterSet(STYLE_DEFAULT, Font.Charset);
  StyleSetItalic(STYLE_DEFAULT, fsItalic in Font.Style);
  StyleSetBold(STYLE_DEFAULT, fsBold in Font.Style);
  StyleSetUnderline(STYLE_DEFAULT, fsUnderline in Font.Style);
  StyleSetFore(STYLE_DEFAULT, ColorToRGB(Font.Color));
  StyleClearAll;
end;

procedure TScintillaBase.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Self.StyleSetBack(STYLE_DEFAULT, ColorToRGB(Color));
  StyleClearAll;
end;

procedure TScintillaBase.SetModified(const Value: Boolean);
begin
  if not Value then SetSavePoint;  // is there a way to tell Scintilla the buffer is modified?
end;

function TScintillaBase.ExecuteAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then
  begin
    Result := TRUE;
		if Action is TEditCut then
      Cut
    else if Action is TEditCopy then
      Copy
    else if Action is TEditPaste then
			Paste
    else if Action is TEditDelete then
      Clear
    else if Action is TEditUndo then
      Undo
		else if Action is TEditSelectAll then
      SelectAll;
  end else
    Result := inherited ExecuteAction(Action);
end;

function TScintillaBase.UpdateAction(Action: TBasicAction): boolean;
begin
  if Action is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if (Action is TEditCut) or (Action is TEditCopy) then
        TEditAction(Action).Enabled := (GetSelectionStart-GetSelectionEnd) <> 0
      else if Action is TEditPaste then
        TEditAction(Action).Enabled := (ReadOnly=False) and (CanPaste)
      else if Action is TEditDelete then
        TEditAction(Action).Enabled := not ReadOnly
      else if Action is TEditUndo then
        TEditAction(Action).Enabled := CanUndo
      else if Action is TEditSelectAll then
        TEditAction(Action).Enabled := TRUE;
    end;
  end else
    Result := inherited UpdateAction(Action);
end;

procedure TScintillaBase.Loaded;
begin
	inherited Loaded;
	SetBufferedDraw(True);
	Self.SetSavePoint;
  SetMouseDownCaptures(False);
end;

const bufSize = 131072;


procedure TScintillaBase.LoadFromStream(Stream: TStream);
{
  Load ANSI text. We need a separate functions to load UTF8 or WideChars
  Alternatively the Lines.Text property allows to load UTF8 if needed (when
  UseUnicode is true)
}
var
  buf : array[0..bufSize] of Char;
  read : LongInt;
  OldUseUnicode : Boolean;
begin
  // Use the SCI_ADDTEXT method to add text to the control
  // from a file, having cleared it first!
  ClearAll;
  //  With UseUnicode option Scintilla expects UTF8 and not ANSI
  OldUseUnicode := UseUnicode;
  if OldUseUnicode then
  begin
    UseUnicode := False;
  end;

  read := Stream.read(buf, SizeOf(buf));
  while (read > 0) do
  begin
    AddText(read, buf);
    read := Stream.Read(buf, SizeOf(buf));
  end;

  // Restore Unicode
  if OldUseUnicode then UseUnicode := True;

  EmptyUndoBuffer;
  SetSavePoint;
  GotoPos(0);
end;

procedure TScintillaBase.SaveToStream(Stream: TStream);
{
  Saves ANSI text. We would need separate functions to save as UTF8 or WideChars
  Alternatively the Lines.Text property would give us raw UTF8 if needed (when
  UseUnicode is true)
}
var
  buf : array[0..bufSize+1] of Char;
	lengthdoc,i,grabsize : Cardinal;
  range     : TTextRange;
  prange    : PTextRange;
  OldUseUnicode : Boolean;
begin
  //  With UseUnicode option Scintilla will give us UTF8 and not ANSI
  OldUseUnicode := UseUnicode;
  if OldUseUnicode then
  begin
    UseUnicode := False;
  end;

  prange := @range;
  lengthdoc := Self.GetLength;
  if lengthdoc = 0 then Exit;
  i := 0;
  while i < lengthdoc do
  begin
    grabsize := lengthdoc - i;
    if grabsize > bufSize then
      grabsize := bufSize;
    range.chrg.cpMin := i;
    range.chrg.cpMax := i + grabsize;
    range.lpstrText := @buf;
    Self.GetTextRange(prange);
    Stream.Write(buf, grabsize);
    i := i + bufSize;
  end;

 // Restore Unicode
  if OldUseUnicode then UseUnicode := True;
  SetSavePoint;
  if FClearUndoAfterSave then
  begin
    EmptyUndoBuffer;
  end;
end;

procedure TScintillaBase.LoadFromFile(const FileName: TFileName);
var
  op   : TFileStream;
begin
  ClearAll;
  op := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
	try
    LoadFromStream(op);
    SetSavePoint;
  finally
    op.Free;
	end;
end;

procedure TScintillaBase.SaveToFile(const FileName: TFileName);
var
  sv : TFileStream;
begin
  sv := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(sv);
  finally
    sv.Free;
  end;
end;

function TScintillaBase.GetUseUnicode: Boolean;
begin
  Result := GetCodePage = SC_CP_UTF8;
end;

procedure TScintillaBase.SetUseUnicode(const Value: Boolean);
begin
  if Value then
		SetCodePage(SC_CP_UTF8)
  else
    SetCodePage(0);
end;

function TScintillaBase.GetCurrentScrollPosition() : LongInt;
var
	lineDisplayTop : LongInt;
begin
	lineDisplayTop := GetFirstVisibleLine;
	result:=DocLineFromVisible(lineDisplayTop);
end;

procedure TScintillaBase.ClearDocument;
begin
	ClearAll;
	EmptyUndoBuffer;
	SetSavePoint;
end;

procedure TScintillaBase.doSciMarginClick(const modifiers : LongInt; const position : LongInt; const margin : LongInt);
begin
	if assigned(FOnmarginclick) then
    FOnmarginclick(Self, modifiers, position, margin);
end;

procedure TScintillaBase.doSciModified(const position : LongInt; const modificationType : LongInt; text : PChar; const len : LongInt; const linesAdded : LongInt; const line : LongInt; const foldLevelNow : LongInt; const foldLevelPrev : LongInt);
begin
	if assigned(FOnmodified) then
    FOnmodified(Self, position, modificationType, text, len, linesAdded, line, foldLevelNow, foldLevelPrev);
end;

procedure TScintillaBase.doSciUserListSelection(const listType : LongInt; text : PChar);
begin
	if assigned(FOnuserlistselection) then
    FOnuserlistselection(Self, listType, text);
end;

procedure TScintillaBase.doSciUpdateUI;
begin
 if assigned(FOnupdateui) then
   FOnupdateui(Self);
end;


procedure TScintillaBase.doSciStyleNeeded(const position : Integer);
begin
	if assigned(FOnstyleneeded) then
    FOnstyleneeded(Self, position);
end;
procedure TScintillaBase.doSciModifyAttemptRO;
begin
	if assigned(FOnmodifyattemptro) then
    FOnmodifyattemptro(Self);
end;
procedure TScintillaBase.doSciDoubleClick;
begin
	if assigned(FOndoubleclick) then
    FOndoubleclick(Self);
end;

procedure TScintillaBase.doSciNeedShown(const position : Integer;const len : Integer);
begin
	if assigned(FOnneedshown) then
    FOnneedshown(Self,position, len);
end;

procedure TScintillaBase.doSciPainted;
begin
	if assigned(FOnpainted) then
    FOnpainted(Self);
end;

procedure TScintillaBase.doSciDwellStart(const position : Integer);
begin
	if assigned(FOndwellstart) then
    FOndwellstart(Self, position);
end;

procedure TScintillaBase.doSciDwellEnd(const position : Integer);
begin
	if assigned(FOndwellend) then
    FOndwellend(Self, position);
end;

procedure TScintillaBase.doSciZoom;
begin
	if assigned(FOnzoom) then
    FOnzoom(Self);
end;

procedure TScintillaBase.doSciHotspotClick(const modifiers : Integer;const position : Integer);
begin
	if assigned(FOnhotspotclick) then
    FOnhotspotclick(Self, modifiers, position);
end;

procedure TScintillaBase.doSciHotspotDoubleClick(const modifiers : Integer;const position : Integer);
begin
	if assigned(FOnhotspotdoubleclick) then
    FOnhotspotdoubleclick(Self, modifiers, position);
end;
procedure TScintillaBase.doSciAutoCSelection(const text : PChar);
begin
	if assigned(FOnAutoCSelection) then
    FOnAutoCSelection(Self, text);
end;

procedure TScintillaBase.doSciCalltipClick(const position : Integer);
begin
	if assigned(FOncalltipclick) then
    FOncalltipclick(Self, position);
end;

procedure TScintillaBase.doSciMacroRecord(const msg : Integer;const wParam : uptr_t;const lParam : sptr_t);
begin
	if assigned(FOnmacrorecord) then
    FOnmacrorecord(Self, msg, wParam, lParam);
end;

procedure TScintillaBase.doSciKey(const ch : Integer;const modifiers : Integer);
begin
	{I'm never called}
end;

procedure TScintillaBase.doSciSavePointReached;
begin
	FDirty :=false;
	if assigned(FOnsavepointreached) then
    FOnsavepointreached(Self);
end;

procedure TScintillaBase.doSciSavePointLeft;
begin
	FDirty:=true;
	if assigned(FOnsavepointleft) then
    FOnsavepointleft(Self);
end;

// TScintillaBase Implementation
//The Python generator will insert the function definitions below this line...
//++FuncImp
procedure TScintillaBase.AddText(length : LongInt; text : PChar);
begin
  SPerform(SCI_ADDTEXT, length, LongInt(text));
end;

procedure TScintillaBase.AddStyledText(length : LongInt; c : PChar);
begin
  SPerform(SCI_ADDSTYLEDTEXT, length, LongInt(c));
end;

procedure TScintillaBase.InsertText(pos : LongInt; text : PChar);
begin
  SPerform(SCI_INSERTTEXT, pos, LongInt(text));
end;

procedure TScintillaBase.ClearAll;
begin
  SPerform(SCI_CLEARALL, 0, 0);
end;

procedure TScintillaBase.ClearDocumentStyle;
begin
  SPerform(SCI_CLEARDOCUMENTSTYLE, 0, 0);
end;

function TScintillaBase.GetLength : LongInt;
begin
  result := SPerform(SCI_GETLENGTH, 0, 0);
end;

function TScintillaBase.GetCharAt(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETCHARAT, pos, 0);
end;

function TScintillaBase.GetCurrentPos : LongInt;
begin
  result := SPerform(SCI_GETCURRENTPOS, 0, 0);
end;

function TScintillaBase.GetAnchor : LongInt;
begin
  result := SPerform(SCI_GETANCHOR, 0, 0);
end;

function TScintillaBase.GetStyleAt(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETSTYLEAT, pos, 0);
end;

procedure TScintillaBase.Redo;
begin
  SPerform(SCI_REDO, 0, 0);
end;

procedure TScintillaBase.SetUndoCollection(collectUndo : Boolean);
begin
  SPerform(SCI_SETUNDOCOLLECTION, LongInt(collectUndo), 0);
end;

procedure TScintillaBase.SelectAll;
begin
  SPerform(SCI_SELECTALL, 0, 0);
end;

procedure TScintillaBase.SetSavePoint;
begin
  SPerform(SCI_SETSAVEPOINT, 0, 0);
end;

function TScintillaBase.GetStyledText(tr : PTextRange) : LongInt;
begin
  result := SPerform(SCI_GETSTYLEDTEXT, 0, LongInt(tr));
end;

function TScintillaBase.CanRedo : Boolean;
begin
  result := Boolean(SPerform(SCI_CANREDO, 0, 0));
end;

function TScintillaBase.MarkerLineFromHandle(handle : LongInt) : LongInt;
begin
  result := SPerform(SCI_MARKERLINEFROMHANDLE, handle, 0);
end;

procedure TScintillaBase.MarkerDeleteHandle(handle : LongInt);
begin
  SPerform(SCI_MARKERDELETEHANDLE, handle, 0);
end;

function TScintillaBase.GetUndoCollection : Boolean;
begin
  result := Boolean(SPerform(SCI_GETUNDOCOLLECTION, 0, 0));
end;

function TScintillaBase.GetViewWS : LongInt;
begin
  result := SPerform(SCI_GETVIEWWS, 0, 0);
end;

procedure TScintillaBase.SetViewWS(viewWS : LongInt);
begin
  SPerform(SCI_SETVIEWWS, viewWS, 0);
end;

function TScintillaBase.PositionFromPoint(x : LongInt; y : LongInt) : LongInt;
begin
  result := SPerform(SCI_POSITIONFROMPOINT, x, y);
end;

function TScintillaBase.PositionFromPointClose(x : LongInt; y : LongInt) : LongInt;
begin
  result := SPerform(SCI_POSITIONFROMPOINTCLOSE, x, y);
end;

procedure TScintillaBase.GotoLine(line : LongInt);
begin
  SPerform(SCI_GOTOLINE, line, 0);
end;

procedure TScintillaBase.GotoPos(pos : LongInt);
begin
  SPerform(SCI_GOTOPOS, pos, 0);
end;

procedure TScintillaBase.SetAnchor(posAnchor : LongInt);
begin
  SPerform(SCI_SETANCHOR, posAnchor, 0);
end;

function TScintillaBase.GetCurLine(length : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_GETCURLINE, length, LongInt(text));
end;

function TScintillaBase.GetEndStyled : LongInt;
begin
  result := SPerform(SCI_GETENDSTYLED, 0, 0);
end;

procedure TScintillaBase.ConvertEOLs(eolMode : LongInt);
begin
  SPerform(SCI_CONVERTEOLS, eolMode, 0);
end;

function TScintillaBase.GetEOLMode : LongInt;
begin
  result := SPerform(SCI_GETEOLMODE, 0, 0);
end;

procedure TScintillaBase.SetEOLMode(eolMode : LongInt);
begin
  SPerform(SCI_SETEOLMODE, eolMode, 0);
end;

procedure TScintillaBase.StartStyling(pos : LongInt; mask : LongInt);
begin
  SPerform(SCI_STARTSTYLING, pos, mask);
end;

procedure TScintillaBase.SetStyling(length : LongInt; style : LongInt);
begin
  SPerform(SCI_SETSTYLING, length, style);
end;

function TScintillaBase.GetBufferedDraw : Boolean;
begin
  result := Boolean(SPerform(SCI_GETBUFFEREDDRAW, 0, 0));
end;

procedure TScintillaBase.SetBufferedDraw(buffered : Boolean);
begin
  SPerform(SCI_SETBUFFEREDDRAW, LongInt(buffered), 0);
end;

procedure TScintillaBase.SetTabWidth(tabWidth : LongInt);
begin
  SPerform(SCI_SETTABWIDTH, tabWidth, 0);
end;

function TScintillaBase.GetTabWidth : LongInt;
begin
  result := SPerform(SCI_GETTABWIDTH, 0, 0);
end;

procedure TScintillaBase.SetCodePage(codePage : LongInt);
begin
  SPerform(SCI_SETCODEPAGE, codePage, 0);
end;

procedure TScintillaBase.SetUsePalette(usePalette : Boolean);
begin
  SPerform(SCI_SETUSEPALETTE, LongInt(usePalette), 0);
end;

procedure TScintillaBase.MarkerDefine(markerNumber : LongInt; markerSymbol : LongInt);
begin
  SPerform(SCI_MARKERDEFINE, markerNumber, markerSymbol);
end;

procedure TScintillaBase.MarkerSetFore(markerNumber : LongInt; fore : TColor);
begin
  SPerform(SCI_MARKERSETFORE, markerNumber, ColorToRGB(fore));
end;

procedure TScintillaBase.MarkerSetBack(markerNumber : LongInt; back : TColor);
begin
  SPerform(SCI_MARKERSETBACK, markerNumber, ColorToRGB(back));
end;

function TScintillaBase.MarkerAdd(line : LongInt; markerNumber : LongInt) : LongInt;
begin
  result := SPerform(SCI_MARKERADD, line, markerNumber);
end;

procedure TScintillaBase.MarkerDelete(line : LongInt; markerNumber : LongInt);
begin
  SPerform(SCI_MARKERDELETE, line, markerNumber);
end;

procedure TScintillaBase.MarkerDeleteAll(markerNumber : LongInt);
begin
  SPerform(SCI_MARKERDELETEALL, markerNumber, 0);
end;

function TScintillaBase.MarkerGet(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_MARKERGET, line, 0);
end;

function TScintillaBase.MarkerNext(lineStart : LongInt; markerMask : LongInt) : LongInt;
begin
  result := SPerform(SCI_MARKERNEXT, lineStart, markerMask);
end;

function TScintillaBase.MarkerPrevious(lineStart : LongInt; markerMask : LongInt) : LongInt;
begin
  result := SPerform(SCI_MARKERPREVIOUS, lineStart, markerMask);
end;

procedure TScintillaBase.MarkerDefinePixmap(markerNumber : LongInt; pixmap : PChar);
begin
  SPerform(SCI_MARKERDEFINEPIXMAP, markerNumber, LongInt(pixmap));
end;

procedure TScintillaBase.SetMarginTypeN(margin : LongInt; marginType : LongInt);
begin
  SPerform(SCI_SETMARGINTYPEN, margin, marginType);
end;

function TScintillaBase.GetMarginTypeN(margin : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETMARGINTYPEN, margin, 0);
end;

procedure TScintillaBase.SetMarginWidthN(margin : LongInt; pixelWidth : LongInt);
begin
  SPerform(SCI_SETMARGINWIDTHN, margin, pixelWidth);
end;

function TScintillaBase.GetMarginWidthN(margin : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETMARGINWIDTHN, margin, 0);
end;

procedure TScintillaBase.SetMarginMaskN(margin : LongInt; mask : LongInt);
begin
  SPerform(SCI_SETMARGINMASKN, margin, mask);
end;

function TScintillaBase.GetMarginMaskN(margin : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETMARGINMASKN, margin, 0);
end;

procedure TScintillaBase.SetMarginSensitiveN(margin : LongInt; sensitive : Boolean);
begin
  SPerform(SCI_SETMARGINSENSITIVEN, margin, LongInt(sensitive));
end;

function TScintillaBase.GetMarginSensitiveN(margin : LongInt) : Boolean;
begin
  result := Boolean(SPerform(SCI_GETMARGINSENSITIVEN, margin, 0));
end;

procedure TScintillaBase.StyleClearAll;
begin
  SPerform(SCI_STYLECLEARALL, 0, 0);
end;

procedure TScintillaBase.StyleSetFore(style : LongInt; fore : TColor);
begin
  SPerform(SCI_STYLESETFORE, style, ColorToRGB(fore));
end;

procedure TScintillaBase.StyleSetBack(style : LongInt; back : TColor);
begin
  SPerform(SCI_STYLESETBACK, style, ColorToRGB(back));
end;

procedure TScintillaBase.StyleSetBold(style : LongInt; bold : Boolean);
begin
  SPerform(SCI_STYLESETBOLD, style, LongInt(bold));
end;

procedure TScintillaBase.StyleSetItalic(style : LongInt; italic : Boolean);
begin
  SPerform(SCI_STYLESETITALIC, style, LongInt(italic));
end;

procedure TScintillaBase.StyleSetSize(style : LongInt; sizePoints : LongInt);
begin
  SPerform(SCI_STYLESETSIZE, style, sizePoints);
end;

procedure TScintillaBase.StyleSetFont(style : LongInt; fontName : PChar);
begin
  SPerform(SCI_STYLESETFONT, style, LongInt(fontName));
end;

procedure TScintillaBase.StyleSetEOLFilled(style : LongInt; filled : Boolean);
begin
  SPerform(SCI_STYLESETEOLFILLED, style, LongInt(filled));
end;

procedure TScintillaBase.StyleResetDefault;
begin
  SPerform(SCI_STYLERESETDEFAULT, 0, 0);
end;

procedure TScintillaBase.StyleSetUnderline(style : LongInt; underline : Boolean);
begin
  SPerform(SCI_STYLESETUNDERLINE, style, LongInt(underline));
end;

procedure TScintillaBase.StyleSetCase(style : LongInt; caseForce : LongInt);
begin
  SPerform(SCI_STYLESETCASE, style, caseForce);
end;

procedure TScintillaBase.StyleSetCharacterSet(style : LongInt; characterSet : LongInt);
begin
  SPerform(SCI_STYLESETCHARACTERSET, style, characterSet);
end;

procedure TScintillaBase.StyleSetHotSpot(style : LongInt; hotspot : Boolean);
begin
  SPerform(SCI_STYLESETHOTSPOT, style, LongInt(hotspot));
end;

procedure TScintillaBase.SetSelFore(useSetting : Boolean; fore : TColor);
begin
  SPerform(SCI_SETSELFORE, LongInt(useSetting), ColorToRGB(fore));
end;

procedure TScintillaBase.SetSelBack(useSetting : Boolean; back : TColor);
begin
  SPerform(SCI_SETSELBACK, LongInt(useSetting), ColorToRGB(back));
end;

procedure TScintillaBase.SetCaretFore(fore : TColor);
begin
  SPerform(SCI_SETCARETFORE, ColorToRGB(fore), 0);
end;

procedure TScintillaBase.AssignCmdKey(km : LongInt; msg : LongInt);
begin
  SPerform(SCI_ASSIGNCMDKEY, km, msg);
end;

procedure TScintillaBase.ClearCmdKey(km : LongInt);
begin
  SPerform(SCI_CLEARCMDKEY, km, 0);
end;

procedure TScintillaBase.ClearAllCmdKeys;
begin
  SPerform(SCI_CLEARALLCMDKEYS, 0, 0);
end;

procedure TScintillaBase.SetStylingEx(length : LongInt; styles : PChar);
begin
  SPerform(SCI_SETSTYLINGEX, length, LongInt(styles));
end;

procedure TScintillaBase.StyleSetVisible(style : LongInt; visible : Boolean);
begin
  SPerform(SCI_STYLESETVISIBLE, style, LongInt(visible));
end;

function TScintillaBase.GetCaretPeriod : LongInt;
begin
  result := SPerform(SCI_GETCARETPERIOD, 0, 0);
end;

procedure TScintillaBase.SetCaretPeriod(periodMilliseconds : LongInt);
begin
  SPerform(SCI_SETCARETPERIOD, periodMilliseconds, 0);
end;

procedure TScintillaBase.SetWordChars(characters : PChar);
begin
  SPerform(SCI_SETWORDCHARS, 0, LongInt(characters));
end;

procedure TScintillaBase.BeginUndoAction;
begin
  SPerform(SCI_BEGINUNDOACTION, 0, 0);
end;

procedure TScintillaBase.EndUndoAction;
begin
  SPerform(SCI_ENDUNDOACTION, 0, 0);
end;

procedure TScintillaBase.IndicSetStyle(indic : LongInt; style : LongInt);
begin
  SPerform(SCI_INDICSETSTYLE, indic, style);
end;

function TScintillaBase.IndicGetStyle(indic : LongInt) : LongInt;
begin
  result := SPerform(SCI_INDICGETSTYLE, indic, 0);
end;

procedure TScintillaBase.IndicSetFore(indic : LongInt; fore : TColor);
begin
  SPerform(SCI_INDICSETFORE, indic, ColorToRGB(fore));
end;

function TScintillaBase.IndicGetFore(indic : LongInt) : TColor;
begin
  result := TColor(SPerform(SCI_INDICGETFORE, indic, 0));
end;

procedure TScintillaBase.SetWhitespaceFore(useSetting : Boolean; fore : TColor);
begin
  SPerform(SCI_SETWHITESPACEFORE, LongInt(useSetting), ColorToRGB(fore));
end;

procedure TScintillaBase.SetWhitespaceBack(useSetting : Boolean; back : TColor);
begin
  SPerform(SCI_SETWHITESPACEBACK, LongInt(useSetting), ColorToRGB(back));
end;

procedure TScintillaBase.SetStyleBits(bits : LongInt);
begin
  SPerform(SCI_SETSTYLEBITS, bits, 0);
end;

function TScintillaBase.GetStyleBits : LongInt;
begin
  result := SPerform(SCI_GETSTYLEBITS, 0, 0);
end;

procedure TScintillaBase.SetLineState(line : LongInt; state : LongInt);
begin
  SPerform(SCI_SETLINESTATE, line, state);
end;

function TScintillaBase.GetLineState(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINESTATE, line, 0);
end;

function TScintillaBase.GetMaxLineState : LongInt;
begin
  result := SPerform(SCI_GETMAXLINESTATE, 0, 0);
end;

function TScintillaBase.GetCaretLineVisible : Boolean;
begin
  result := Boolean(SPerform(SCI_GETCARETLINEVISIBLE, 0, 0));
end;

procedure TScintillaBase.SetCaretLineVisible(show : Boolean);
begin
  SPerform(SCI_SETCARETLINEVISIBLE, LongInt(show), 0);
end;

function TScintillaBase.GetCaretLineBack : TColor;
begin
  result := TColor(SPerform(SCI_GETCARETLINEBACK, 0, 0));
end;

procedure TScintillaBase.SetCaretLineBack(back : TColor);
begin
  SPerform(SCI_SETCARETLINEBACK, ColorToRGB(back), 0);
end;

procedure TScintillaBase.StyleSetChangeable(style : LongInt; changeable : Boolean);
begin
  SPerform(SCI_STYLESETCHANGEABLE, style, LongInt(changeable));
end;

procedure TScintillaBase.AutoCShow(lenEntered : LongInt; itemList : PChar);
begin
  SPerform(SCI_AUTOCSHOW, lenEntered, LongInt(itemList));
end;

procedure TScintillaBase.AutoCCancel;
begin
  SPerform(SCI_AUTOCCANCEL, 0, 0);
end;

function TScintillaBase.AutoCActive : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCACTIVE, 0, 0));
end;

function TScintillaBase.AutoCPosStart : LongInt;
begin
  result := SPerform(SCI_AUTOCPOSSTART, 0, 0);
end;

procedure TScintillaBase.AutoCComplete;
begin
  SPerform(SCI_AUTOCCOMPLETE, 0, 0);
end;

procedure TScintillaBase.AutoCStops(characterSet : PChar);
begin
  SPerform(SCI_AUTOCSTOPS, 0, LongInt(characterSet));
end;

procedure TScintillaBase.AutoCSetSeparator(separatorCharacter : LongInt);
begin
  SPerform(SCI_AUTOCSETSEPARATOR, separatorCharacter, 0);
end;

function TScintillaBase.AutoCGetSeparator : LongInt;
begin
  result := SPerform(SCI_AUTOCGETSEPARATOR, 0, 0);
end;

procedure TScintillaBase.AutoCSelect(text : PChar);
begin
  SPerform(SCI_AUTOCSELECT, 0, LongInt(text));
end;

procedure TScintillaBase.AutoCSetCancelAtStart(cancel : Boolean);
begin
  SPerform(SCI_AUTOCSETCANCELATSTART, LongInt(cancel), 0);
end;

function TScintillaBase.AutoCGetCancelAtStart : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCGETCANCELATSTART, 0, 0));
end;

procedure TScintillaBase.AutoCSetFillUps(characterSet : PChar);
begin
  SPerform(SCI_AUTOCSETFILLUPS, 0, LongInt(characterSet));
end;

procedure TScintillaBase.AutoCSetChooseSingle(chooseSingle : Boolean);
begin
  SPerform(SCI_AUTOCSETCHOOSESINGLE, LongInt(chooseSingle), 0);
end;

function TScintillaBase.AutoCGetChooseSingle : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCGETCHOOSESINGLE, 0, 0));
end;

procedure TScintillaBase.AutoCSetIgnoreCase(ignoreCase : Boolean);
begin
  SPerform(SCI_AUTOCSETIGNORECASE, LongInt(ignoreCase), 0);
end;

function TScintillaBase.AutoCGetIgnoreCase : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCGETIGNORECASE, 0, 0));
end;

procedure TScintillaBase.UserListShow(listType : LongInt; itemList : PChar);
begin
  SPerform(SCI_USERLISTSHOW, listType, LongInt(itemList));
end;

procedure TScintillaBase.AutoCSetAutoHide(autoHide : Boolean);
begin
  SPerform(SCI_AUTOCSETAUTOHIDE, LongInt(autoHide), 0);
end;

function TScintillaBase.AutoCGetAutoHide : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCGETAUTOHIDE, 0, 0));
end;

procedure TScintillaBase.AutoCSetDropRestOfWord(dropRestOfWord : Boolean);
begin
  SPerform(SCI_AUTOCSETDROPRESTOFWORD, LongInt(dropRestOfWord), 0);
end;

function TScintillaBase.AutoCGetDropRestOfWord : Boolean;
begin
  result := Boolean(SPerform(SCI_AUTOCGETDROPRESTOFWORD, 0, 0));
end;

procedure TScintillaBase.RegisterImage(type_ : LongInt; xpmData : PChar);
begin
  SPerform(SCI_REGISTERIMAGE, type_, LongInt(xpmData));
end;

procedure TScintillaBase.ClearRegisteredImages;
begin
  SPerform(SCI_CLEARREGISTEREDIMAGES, 0, 0);
end;

function TScintillaBase.AutoCGetTypeSeparator : LongInt;
begin
  result := SPerform(SCI_AUTOCGETTYPESEPARATOR, 0, 0);
end;

procedure TScintillaBase.AutoCSetTypeSeparator(separatorCharacter : LongInt);
begin
  SPerform(SCI_AUTOCSETTYPESEPARATOR, separatorCharacter, 0);
end;

procedure TScintillaBase.AutoCSetMaxWidth(characterCount : LongInt);
begin
  SPerform(SCI_AUTOCSETMAXWIDTH, characterCount, 0);
end;

function TScintillaBase.AutoCGetMaxWidth : LongInt;
begin
  result := SPerform(SCI_AUTOCGETMAXWIDTH, 0, 0);
end;

procedure TScintillaBase.AutoCSetMaxHeight(rowCount : LongInt);
begin
  SPerform(SCI_AUTOCSETMAXHEIGHT, rowCount, 0);
end;

function TScintillaBase.AutoCGetMaxHeight : LongInt;
begin
  result := SPerform(SCI_AUTOCGETMAXHEIGHT, 0, 0);
end;

procedure TScintillaBase.SetIndent(indentSize : LongInt);
begin
  SPerform(SCI_SETINDENT, indentSize, 0);
end;

function TScintillaBase.GetIndent : LongInt;
begin
  result := SPerform(SCI_GETINDENT, 0, 0);
end;

procedure TScintillaBase.SetUseTabs(useTabs : Boolean);
begin
  SPerform(SCI_SETUSETABS, LongInt(useTabs), 0);
end;

function TScintillaBase.GetUseTabs : Boolean;
begin
  result := Boolean(SPerform(SCI_GETUSETABS, 0, 0));
end;

procedure TScintillaBase.SetLineIndentation(line : LongInt; indentSize : LongInt);
begin
  SPerform(SCI_SETLINEINDENTATION, line, indentSize);
end;

function TScintillaBase.GetLineIndentation(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINEINDENTATION, line, 0);
end;

function TScintillaBase.GetLineIndentPosition(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINEINDENTPOSITION, line, 0);
end;

function TScintillaBase.GetColumn(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETCOLUMN, pos, 0);
end;

procedure TScintillaBase.SetHScrollBar(show : Boolean);
begin
  SPerform(SCI_SETHSCROLLBAR, LongInt(show), 0);
end;

function TScintillaBase.GetHScrollBar : Boolean;
begin
  result := Boolean(SPerform(SCI_GETHSCROLLBAR, 0, 0));
end;

procedure TScintillaBase.SetIndentationGuides(show : Boolean);
begin
  SPerform(SCI_SETINDENTATIONGUIDES, LongInt(show), 0);
end;

function TScintillaBase.GetIndentationGuides : Boolean;
begin
  result := Boolean(SPerform(SCI_GETINDENTATIONGUIDES, 0, 0));
end;

procedure TScintillaBase.SetHighlightGuide(column : LongInt);
begin
  SPerform(SCI_SETHIGHLIGHTGUIDE, column, 0);
end;

function TScintillaBase.GetHighlightGuide : LongInt;
begin
  result := SPerform(SCI_GETHIGHLIGHTGUIDE, 0, 0);
end;

function TScintillaBase.GetLineEndPosition(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINEENDPOSITION, line, 0);
end;

function TScintillaBase.GetCodePage : LongInt;
begin
  result := SPerform(SCI_GETCODEPAGE, 0, 0);
end;

function TScintillaBase.GetCaretFore : TColor;
begin
  result := TColor(SPerform(SCI_GETCARETFORE, 0, 0));
end;

function TScintillaBase.GetUsePalette : Boolean;
begin
  result := Boolean(SPerform(SCI_GETUSEPALETTE, 0, 0));
end;

function TScintillaBase.GetReadOnly : Boolean;
begin
  result := Boolean(SPerform(SCI_GETREADONLY, 0, 0));
end;

procedure TScintillaBase.SetCurrentPos(pos : LongInt);
begin
  SPerform(SCI_SETCURRENTPOS, pos, 0);
end;

procedure TScintillaBase.SetSelectionStart(pos : LongInt);
begin
  SPerform(SCI_SETSELECTIONSTART, pos, 0);
end;

function TScintillaBase.GetSelectionStart : LongInt;
begin
  result := SPerform(SCI_GETSELECTIONSTART, 0, 0);
end;

procedure TScintillaBase.SetSelectionEnd(pos : LongInt);
begin
  SPerform(SCI_SETSELECTIONEND, pos, 0);
end;

function TScintillaBase.GetSelectionEnd : LongInt;
begin
  result := SPerform(SCI_GETSELECTIONEND, 0, 0);
end;

procedure TScintillaBase.SetPrintMagnification(magnification : LongInt);
begin
  SPerform(SCI_SETPRINTMAGNIFICATION, magnification, 0);
end;

function TScintillaBase.GetPrintMagnification : LongInt;
begin
  result := SPerform(SCI_GETPRINTMAGNIFICATION, 0, 0);
end;

procedure TScintillaBase.SetPrintColourMode(mode : LongInt);
begin
  SPerform(SCI_SETPRINTCOLOURMODE, mode, 0);
end;

function TScintillaBase.GetPrintColourMode : LongInt;
begin
  result := SPerform(SCI_GETPRINTCOLOURMODE, 0, 0);
end;

function TScintillaBase.FindTextX(flags : LongInt; ft : PTextToFind) : LongInt;
begin
  result := SPerform(SCI_FINDTEXT, flags, LongInt(ft));
end;

function TScintillaBase.FormatRange(draw : Boolean; fr : PRangeToFormat) : LongInt;
begin
  result := SPerform(SCI_FORMATRANGE, LongInt(draw), LongInt(fr));
end;

function TScintillaBase.GetFirstVisibleLine : LongInt;
begin
  result := SPerform(SCI_GETFIRSTVISIBLELINE, 0, 0);
end;

function TScintillaBase.GetLine(line : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_GETLINE, line, LongInt(text));
end;

function TScintillaBase.GetLineCount : LongInt;
begin
  result := SPerform(SCI_GETLINECOUNT, 0, 0);
end;

procedure TScintillaBase.SetMarginLeft(pixelWidth : LongInt);
begin
  SPerform(SCI_SETMARGINLEFT, 0, pixelWidth);
end;

function TScintillaBase.GetMarginLeft : LongInt;
begin
  result := SPerform(SCI_GETMARGINLEFT, 0, 0);
end;

procedure TScintillaBase.SetMarginRight(pixelWidth : LongInt);
begin
  SPerform(SCI_SETMARGINRIGHT, 0, pixelWidth);
end;

function TScintillaBase.GetMarginRight : LongInt;
begin
  result := SPerform(SCI_GETMARGINRIGHT, 0, 0);
end;

function TScintillaBase.GetModify : Boolean;
begin
  result := Boolean(SPerform(SCI_GETMODIFY, 0, 0));
end;

procedure TScintillaBase.SetSel(start : LongInt; end_ : LongInt);
begin
  SPerform(SCI_SETSEL, start, end_);
end;

function TScintillaBase.GetSelText(text : PChar) : LongInt;
begin
  result := SPerform(SCI_GETSELTEXT, 0, LongInt(text));
end;

function TScintillaBase.GetTextRange(tr : PTextRange) : LongInt;
begin
  result := SPerform(SCI_GETTEXTRANGE, 0, LongInt(tr));
end;

procedure TScintillaBase.HideSelection(normal : Boolean);
begin
  SPerform(SCI_HIDESELECTION, LongInt(normal), 0);
end;

function TScintillaBase.PointXFromPosition(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_POINTXFROMPOSITION, 0, pos);
end;

function TScintillaBase.PointYFromPosition(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_POINTYFROMPOSITION, 0, pos);
end;

function TScintillaBase.LineFromPosition(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_LINEFROMPOSITION, pos, 0);
end;

function TScintillaBase.PositionFromLine(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_POSITIONFROMLINE, line, 0);
end;

procedure TScintillaBase.LineScroll(columns : LongInt; lines : LongInt);
begin
  SPerform(SCI_LINESCROLL, columns, lines);
end;

procedure TScintillaBase.ScrollCaret;
begin
  SPerform(SCI_SCROLLCARET, 0, 0);
end;

procedure TScintillaBase.ReplaceSel(text : PChar);
begin
  SPerform(SCI_REPLACESEL, 0, LongInt(text));
end;

procedure TScintillaBase.SetReadOnly(readOnly : Boolean);
begin
  SPerform(SCI_SETREADONLY, LongInt(readOnly), 0);
end;

procedure TScintillaBase.Null;
begin
  SPerform(SCI_NULL, 0, 0);
end;

function TScintillaBase.CanPaste : Boolean;
begin
  result := Boolean(SPerform(SCI_CANPASTE, 0, 0));
end;

function TScintillaBase.CanUndo : Boolean;
begin
  result := Boolean(SPerform(SCI_CANUNDO, 0, 0));
end;

procedure TScintillaBase.EmptyUndoBuffer;
begin
  SPerform(SCI_EMPTYUNDOBUFFER, 0, 0);
end;

procedure TScintillaBase.Undo;
begin
  SPerform(SCI_UNDO, 0, 0);
end;

procedure TScintillaBase.Cut;
begin
  SPerform(SCI_CUT, 0, 0);
end;

procedure TScintillaBase.Copy;
begin
  SPerform(SCI_COPY, 0, 0);
end;

procedure TScintillaBase.Paste;
begin
  SPerform(SCI_PASTE, 0, 0);
end;

procedure TScintillaBase.Clear;
begin
  SPerform(SCI_CLEAR, 0, 0);
end;

procedure TScintillaBase.SetText(text : PChar);
begin
  SPerform(SCI_SETTEXT, 0, LongInt(text));
end;

function TScintillaBase.GetText(length : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_GETTEXT, length, LongInt(text));
end;

function TScintillaBase.GetTextLength : LongInt;
begin
  result := SPerform(SCI_GETTEXTLENGTH, 0, 0);
end;

function TScintillaBase.GetDirectFunction : LongInt;
begin
  result := SPerform(SCI_GETDIRECTFUNCTION, 0, 0);
end;

function TScintillaBase.GetDirectPointer : LongInt;
begin
  result := SPerform(SCI_GETDIRECTPOINTER, 0, 0);
end;

procedure TScintillaBase.SetOvertype(overtype : Boolean);
begin
  SPerform(SCI_SETOVERTYPE, LongInt(overtype), 0);
end;

function TScintillaBase.GetOvertype : Boolean;
begin
  result := Boolean(SPerform(SCI_GETOVERTYPE, 0, 0));
end;

procedure TScintillaBase.SetCaretWidth(pixelWidth : LongInt);
begin
  SPerform(SCI_SETCARETWIDTH, pixelWidth, 0);
end;

function TScintillaBase.GetCaretWidth : LongInt;
begin
  result := SPerform(SCI_GETCARETWIDTH, 0, 0);
end;

procedure TScintillaBase.SetTargetStart(pos : LongInt);
begin
  SPerform(SCI_SETTARGETSTART, pos, 0);
end;

function TScintillaBase.GetTargetStart : LongInt;
begin
  result := SPerform(SCI_GETTARGETSTART, 0, 0);
end;

procedure TScintillaBase.SetTargetEnd(pos : LongInt);
begin
  SPerform(SCI_SETTARGETEND, pos, 0);
end;

function TScintillaBase.GetTargetEnd : LongInt;
begin
  result := SPerform(SCI_GETTARGETEND, 0, 0);
end;

function TScintillaBase.ReplaceTarget(length : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_REPLACETARGET, length, LongInt(text));
end;

function TScintillaBase.ReplaceTargetRE(length : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_REPLACETARGETRE, length, LongInt(text));
end;

function TScintillaBase.SearchInTarget(length : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_SEARCHINTARGET, length, LongInt(text));
end;

procedure TScintillaBase.SetSearchFlags(flags : LongInt);
begin
  SPerform(SCI_SETSEARCHFLAGS, flags, 0);
end;

function TScintillaBase.GetSearchFlags : LongInt;
begin
  result := SPerform(SCI_GETSEARCHFLAGS, 0, 0);
end;

procedure TScintillaBase.CallTipShow(pos : LongInt; definition : PChar);
begin
  SPerform(SCI_CALLTIPSHOW, pos, LongInt(definition));
end;

procedure TScintillaBase.CallTipCancel;
begin
  SPerform(SCI_CALLTIPCANCEL, 0, 0);
end;

function TScintillaBase.CallTipActive : Boolean;
begin
  result := Boolean(SPerform(SCI_CALLTIPACTIVE, 0, 0));
end;

function TScintillaBase.CallTipPosStart : LongInt;
begin
  result := SPerform(SCI_CALLTIPPOSSTART, 0, 0);
end;

procedure TScintillaBase.CallTipSetHlt(start : LongInt; end_ : LongInt);
begin
  SPerform(SCI_CALLTIPSETHLT, start, end_);
end;

procedure TScintillaBase.CallTipSetBack(back : TColor);
begin
  SPerform(SCI_CALLTIPSETBACK, ColorToRGB(back), 0);
end;

procedure TScintillaBase.CallTipSetFore(fore : TColor);
begin
  SPerform(SCI_CALLTIPSETFORE, ColorToRGB(fore), 0);
end;

procedure TScintillaBase.CallTipSetForeHlt(fore : TColor);
begin
  SPerform(SCI_CALLTIPSETFOREHLT, ColorToRGB(fore), 0);
end;

function TScintillaBase.VisibleFromDocLine(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_VISIBLEFROMDOCLINE, line, 0);
end;

function TScintillaBase.DocLineFromVisible(lineDisplay : LongInt) : LongInt;
begin
  result := SPerform(SCI_DOCLINEFROMVISIBLE, lineDisplay, 0);
end;

function TScintillaBase.WrapCount(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_WRAPCOUNT, line, 0);
end;

procedure TScintillaBase.SetFoldLevel(line : LongInt; level : LongInt);
begin
  SPerform(SCI_SETFOLDLEVEL, line, level);
end;

function TScintillaBase.GetFoldLevel(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETFOLDLEVEL, line, 0);
end;

function TScintillaBase.GetLastChild(line : LongInt; level : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLASTCHILD, line, level);
end;

function TScintillaBase.GetFoldParent(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETFOLDPARENT, line, 0);
end;

procedure TScintillaBase.ShowLines(lineStart : LongInt; lineEnd : LongInt);
begin
  SPerform(SCI_SHOWLINES, lineStart, lineEnd);
end;

procedure TScintillaBase.HideLines(lineStart : LongInt; lineEnd : LongInt);
begin
  SPerform(SCI_HIDELINES, lineStart, lineEnd);
end;

function TScintillaBase.GetLineVisible(line : LongInt) : Boolean;
begin
  result := Boolean(SPerform(SCI_GETLINEVISIBLE, line, 0));
end;

procedure TScintillaBase.SetFoldExpanded(line : LongInt; expanded : Boolean);
begin
  SPerform(SCI_SETFOLDEXPANDED, line, LongInt(expanded));
end;

function TScintillaBase.GetFoldExpanded(line : LongInt) : Boolean;
begin
  result := Boolean(SPerform(SCI_GETFOLDEXPANDED, line, 0));
end;

procedure TScintillaBase.ToggleFold(line : LongInt);
begin
  SPerform(SCI_TOGGLEFOLD, line, 0);
end;

procedure TScintillaBase.EnsureVisible(line : LongInt);
begin
  SPerform(SCI_ENSUREVISIBLE, line, 0);
end;

procedure TScintillaBase.SetFoldFlags(flags : LongInt);
begin
  SPerform(SCI_SETFOLDFLAGS, flags, 0);
end;

procedure TScintillaBase.EnsureVisibleEnforcePolicy(line : LongInt);
begin
  SPerform(SCI_ENSUREVISIBLEENFORCEPOLICY, line, 0);
end;

procedure TScintillaBase.SetTabIndents(tabIndents : Boolean);
begin
  SPerform(SCI_SETTABINDENTS, LongInt(tabIndents), 0);
end;

function TScintillaBase.GetTabIndents : Boolean;
begin
  result := Boolean(SPerform(SCI_GETTABINDENTS, 0, 0));
end;

procedure TScintillaBase.SetBackSpaceUnIndents(bsUnIndents : Boolean);
begin
  SPerform(SCI_SETBACKSPACEUNINDENTS, LongInt(bsUnIndents), 0);
end;

function TScintillaBase.GetBackSpaceUnIndents : Boolean;
begin
  result := Boolean(SPerform(SCI_GETBACKSPACEUNINDENTS, 0, 0));
end;

procedure TScintillaBase.SetMouseDwellTime(periodMilliseconds : LongInt);
begin
  SPerform(SCI_SETMOUSEDWELLTIME, periodMilliseconds, 0);
end;

function TScintillaBase.GetMouseDwellTime : LongInt;
begin
  result := SPerform(SCI_GETMOUSEDWELLTIME, 0, 0);
end;

function TScintillaBase.WordStartPosition(pos : LongInt; onlyWordCharacters : Boolean) : LongInt;
begin
  result := SPerform(SCI_WORDSTARTPOSITION, pos, LongInt(onlyWordCharacters));
end;

function TScintillaBase.WordEndPosition(pos : LongInt; onlyWordCharacters : Boolean) : LongInt;
begin
  result := SPerform(SCI_WORDENDPOSITION, pos, LongInt(onlyWordCharacters));
end;

procedure TScintillaBase.SetWrapMode(mode : LongInt);
begin
  SPerform(SCI_SETWRAPMODE, mode, 0);
end;

function TScintillaBase.GetWrapMode : LongInt;
begin
  result := SPerform(SCI_GETWRAPMODE, 0, 0);
end;

procedure TScintillaBase.SetWrapVisualFlags(wrapVisualFlags : LongInt);
begin
  SPerform(SCI_SETWRAPVISUALFLAGS, wrapVisualFlags, 0);
end;

function TScintillaBase.GetWrapVisualFlags : LongInt;
begin
  result := SPerform(SCI_GETWRAPVISUALFLAGS, 0, 0);
end;

procedure TScintillaBase.SetWrapVisualFlagsLocation(wrapVisualFlagsLocation : LongInt);
begin
  SPerform(SCI_SETWRAPVISUALFLAGSLOCATION, wrapVisualFlagsLocation, 0);
end;

function TScintillaBase.GetWrapVisualFlagsLocation : LongInt;
begin
  result := SPerform(SCI_GETWRAPVISUALFLAGSLOCATION, 0, 0);
end;

procedure TScintillaBase.SetWrapStartIndent(indent : LongInt);
begin
  SPerform(SCI_SETWRAPSTARTINDENT, indent, 0);
end;

function TScintillaBase.GetWrapStartIndent : LongInt;
begin
  result := SPerform(SCI_GETWRAPSTARTINDENT, 0, 0);
end;

procedure TScintillaBase.SetLayoutCache(mode : LongInt);
begin
  SPerform(SCI_SETLAYOUTCACHE, mode, 0);
end;

function TScintillaBase.GetLayoutCache : LongInt;
begin
  result := SPerform(SCI_GETLAYOUTCACHE, 0, 0);
end;

procedure TScintillaBase.SetScrollWidth(pixelWidth : LongInt);
begin
  SPerform(SCI_SETSCROLLWIDTH, pixelWidth, 0);
end;

function TScintillaBase.GetScrollWidth : LongInt;
begin
  result := SPerform(SCI_GETSCROLLWIDTH, 0, 0);
end;

function TScintillaBase.TextWidth(style : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_TEXTWIDTH, style, LongInt(text));
end;

procedure TScintillaBase.SetEndAtLastLine(endAtLastLine : Boolean);
begin
  SPerform(SCI_SETENDATLASTLINE, LongInt(endAtLastLine), 0);
end;

function TScintillaBase.GetEndAtLastLine : LongInt;
begin
  result := SPerform(SCI_GETENDATLASTLINE, 0, 0);
end;

function TScintillaBase.TextHeight(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_TEXTHEIGHT, line, 0);
end;

procedure TScintillaBase.SetVScrollBar(show : Boolean);
begin
  SPerform(SCI_SETVSCROLLBAR, LongInt(show), 0);
end;

function TScintillaBase.GetVScrollBar : Boolean;
begin
  result := Boolean(SPerform(SCI_GETVSCROLLBAR, 0, 0));
end;

procedure TScintillaBase.AppendText(length : LongInt; text : PChar);
begin
  SPerform(SCI_APPENDTEXT, length, LongInt(text));
end;

function TScintillaBase.GetTwoPhaseDraw : Boolean;
begin
  result := Boolean(SPerform(SCI_GETTWOPHASEDRAW, 0, 0));
end;

procedure TScintillaBase.SetTwoPhaseDraw(twoPhase : Boolean);
begin
  SPerform(SCI_SETTWOPHASEDRAW, LongInt(twoPhase), 0);
end;

procedure TScintillaBase.TargetFromSelection;
begin
  SPerform(SCI_TARGETFROMSELECTION, 0, 0);
end;

procedure TScintillaBase.LinesJoin;
begin
  SPerform(SCI_LINESJOIN, 0, 0);
end;

procedure TScintillaBase.LinesSplit(pixelWidth : LongInt);
begin
  SPerform(SCI_LINESSPLIT, pixelWidth, 0);
end;

procedure TScintillaBase.SetFoldMarginColour(useSetting : Boolean; back : TColor);
begin
  SPerform(SCI_SETFOLDMARGINCOLOUR, LongInt(useSetting), ColorToRGB(back));
end;

procedure TScintillaBase.SetFoldMarginHiColour(useSetting : Boolean; fore : TColor);
begin
  SPerform(SCI_SETFOLDMARGINHICOLOUR, LongInt(useSetting), ColorToRGB(fore));
end;

procedure TScintillaBase.LineDown;
begin
  SPerform(SCI_LINEDOWN, 0, 0);
end;

procedure TScintillaBase.LineDownExtend;
begin
  SPerform(SCI_LINEDOWNEXTEND, 0, 0);
end;

procedure TScintillaBase.LineUp;
begin
  SPerform(SCI_LINEUP, 0, 0);
end;

procedure TScintillaBase.LineUpExtend;
begin
  SPerform(SCI_LINEUPEXTEND, 0, 0);
end;

procedure TScintillaBase.CharLeft;
begin
  SPerform(SCI_CHARLEFT, 0, 0);
end;

procedure TScintillaBase.CharLeftExtend;
begin
  SPerform(SCI_CHARLEFTEXTEND, 0, 0);
end;

procedure TScintillaBase.CharRight;
begin
  SPerform(SCI_CHARRIGHT, 0, 0);
end;

procedure TScintillaBase.CharRightExtend;
begin
  SPerform(SCI_CHARRIGHTEXTEND, 0, 0);
end;

procedure TScintillaBase.WordLeft;
begin
  SPerform(SCI_WORDLEFT, 0, 0);
end;

procedure TScintillaBase.WordLeftExtend;
begin
  SPerform(SCI_WORDLEFTEXTEND, 0, 0);
end;

procedure TScintillaBase.WordRight;
begin
  SPerform(SCI_WORDRIGHT, 0, 0);
end;

procedure TScintillaBase.WordRightExtend;
begin
  SPerform(SCI_WORDRIGHTEXTEND, 0, 0);
end;

procedure TScintillaBase.Home;
begin
  SPerform(SCI_HOME, 0, 0);
end;

procedure TScintillaBase.HomeExtend;
begin
  SPerform(SCI_HOMEEXTEND, 0, 0);
end;

procedure TScintillaBase.LineEnd;
begin
  SPerform(SCI_LINEEND, 0, 0);
end;

procedure TScintillaBase.LineEndExtend;
begin
  SPerform(SCI_LINEENDEXTEND, 0, 0);
end;

procedure TScintillaBase.DocumentStart;
begin
  SPerform(SCI_DOCUMENTSTART, 0, 0);
end;

procedure TScintillaBase.DocumentStartExtend;
begin
  SPerform(SCI_DOCUMENTSTARTEXTEND, 0, 0);
end;

procedure TScintillaBase.DocumentEnd;
begin
  SPerform(SCI_DOCUMENTEND, 0, 0);
end;

procedure TScintillaBase.DocumentEndExtend;
begin
  SPerform(SCI_DOCUMENTENDEXTEND, 0, 0);
end;

procedure TScintillaBase.PageUp;
begin
  SPerform(SCI_PAGEUP, 0, 0);
end;

procedure TScintillaBase.PageUpExtend;
begin
  SPerform(SCI_PAGEUPEXTEND, 0, 0);
end;

procedure TScintillaBase.PageDown;
begin
  SPerform(SCI_PAGEDOWN, 0, 0);
end;

procedure TScintillaBase.PageDownExtend;
begin
  SPerform(SCI_PAGEDOWNEXTEND, 0, 0);
end;

procedure TScintillaBase.EditToggleOvertype;
begin
  SPerform(SCI_EDITTOGGLEOVERTYPE, 0, 0);
end;

procedure TScintillaBase.Cancel;
begin
  SPerform(SCI_CANCEL, 0, 0);
end;

procedure TScintillaBase.DeleteBack;
begin
  SPerform(SCI_DELETEBACK, 0, 0);
end;

procedure TScintillaBase.Tab;
begin
  SPerform(SCI_TAB, 0, 0);
end;

procedure TScintillaBase.BackTab;
begin
  SPerform(SCI_BACKTAB, 0, 0);
end;

procedure TScintillaBase.NewLine;
begin
  SPerform(SCI_NEWLINE, 0, 0);
end;

procedure TScintillaBase.FormFeed;
begin
  SPerform(SCI_FORMFEED, 0, 0);
end;

procedure TScintillaBase.VCHome;
begin
  SPerform(SCI_VCHOME, 0, 0);
end;

procedure TScintillaBase.VCHomeExtend;
begin
  SPerform(SCI_VCHOMEEXTEND, 0, 0);
end;

procedure TScintillaBase.ZoomIn;
begin
  SPerform(SCI_ZOOMIN, 0, 0);
end;

procedure TScintillaBase.ZoomOut;
begin
  SPerform(SCI_ZOOMOUT, 0, 0);
end;

procedure TScintillaBase.DelWordLeft;
begin
  SPerform(SCI_DELWORDLEFT, 0, 0);
end;

procedure TScintillaBase.DelWordRight;
begin
  SPerform(SCI_DELWORDRIGHT, 0, 0);
end;

procedure TScintillaBase.LineCut;
begin
  SPerform(SCI_LINECUT, 0, 0);
end;

procedure TScintillaBase.LineDelete;
begin
  SPerform(SCI_LINEDELETE, 0, 0);
end;

procedure TScintillaBase.LineTranspose;
begin
  SPerform(SCI_LINETRANSPOSE, 0, 0);
end;

procedure TScintillaBase.LineDuplicate;
begin
  SPerform(SCI_LINEDUPLICATE, 0, 0);
end;

procedure TScintillaBase.LowerCase;
begin
  SPerform(SCI_LOWERCASE, 0, 0);
end;

procedure TScintillaBase.UpperCase;
begin
  SPerform(SCI_UPPERCASE, 0, 0);
end;

procedure TScintillaBase.LineScrollDown;
begin
  SPerform(SCI_LINESCROLLDOWN, 0, 0);
end;

procedure TScintillaBase.LineScrollUp;
begin
  SPerform(SCI_LINESCROLLUP, 0, 0);
end;

procedure TScintillaBase.DeleteBackNotLine;
begin
  SPerform(SCI_DELETEBACKNOTLINE, 0, 0);
end;

procedure TScintillaBase.HomeDisplay;
begin
  SPerform(SCI_HOMEDISPLAY, 0, 0);
end;

procedure TScintillaBase.HomeDisplayExtend;
begin
  SPerform(SCI_HOMEDISPLAYEXTEND, 0, 0);
end;

procedure TScintillaBase.LineEndDisplay;
begin
  SPerform(SCI_LINEENDDISPLAY, 0, 0);
end;

procedure TScintillaBase.LineEndDisplayExtend;
begin
  SPerform(SCI_LINEENDDISPLAYEXTEND, 0, 0);
end;

procedure TScintillaBase.HomeWrap;
begin
  SPerform(SCI_HOMEWRAP, 0, 0);
end;

procedure TScintillaBase.HomeWrapExtend;
begin
  SPerform(SCI_HOMEWRAPEXTEND, 0, 0);
end;

procedure TScintillaBase.LineEndWrap;
begin
  SPerform(SCI_LINEENDWRAP, 0, 0);
end;

procedure TScintillaBase.LineEndWrapExtend;
begin
  SPerform(SCI_LINEENDWRAPEXTEND, 0, 0);
end;

procedure TScintillaBase.VCHomeWrap;
begin
  SPerform(SCI_VCHOMEWRAP, 0, 0);
end;

procedure TScintillaBase.VCHomeWrapExtend;
begin
  SPerform(SCI_VCHOMEWRAPEXTEND, 0, 0);
end;

procedure TScintillaBase.LineCopy;
begin
  SPerform(SCI_LINECOPY, 0, 0);
end;

procedure TScintillaBase.MoveCaretInsideView;
begin
  SPerform(SCI_MOVECARETINSIDEVIEW, 0, 0);
end;

function TScintillaBase.LineLength(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_LINELENGTH, line, 0);
end;

procedure TScintillaBase.BraceHighlight(pos1 : LongInt; pos2 : LongInt);
begin
  SPerform(SCI_BRACEHIGHLIGHT, pos1, pos2);
end;

procedure TScintillaBase.BraceBadLight(pos : LongInt);
begin
  SPerform(SCI_BRACEBADLIGHT, pos, 0);
end;

function TScintillaBase.BraceMatch(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_BRACEMATCH, pos, 0);
end;

function TScintillaBase.GetViewEOL : Boolean;
begin
  result := Boolean(SPerform(SCI_GETVIEWEOL, 0, 0));
end;

procedure TScintillaBase.SetViewEOL(visible : Boolean);
begin
  SPerform(SCI_SETVIEWEOL, LongInt(visible), 0);
end;

function TScintillaBase.GetDocPointer : LongInt;
begin
  result := SPerform(SCI_GETDOCPOINTER, 0, 0);
end;

procedure TScintillaBase.SetDocPointer(pointer : LongInt);
begin
  SPerform(SCI_SETDOCPOINTER, 0, pointer);
end;

procedure TScintillaBase.SetModEventMask(mask : LongInt);
begin
  SPerform(SCI_SETMODEVENTMASK, mask, 0);
end;

function TScintillaBase.GetEdgeColumn : LongInt;
begin
  result := SPerform(SCI_GETEDGECOLUMN, 0, 0);
end;

procedure TScintillaBase.SetEdgeColumn(column : LongInt);
begin
  SPerform(SCI_SETEDGECOLUMN, column, 0);
end;

function TScintillaBase.GetEdgeMode : LongInt;
begin
  result := SPerform(SCI_GETEDGEMODE, 0, 0);
end;

procedure TScintillaBase.SetEdgeMode(mode : LongInt);
begin
  SPerform(SCI_SETEDGEMODE, mode, 0);
end;

function TScintillaBase.GetEdgeColour : TColor;
begin
  result := TColor(SPerform(SCI_GETEDGECOLOUR, 0, 0));
end;

procedure TScintillaBase.SetEdgeColour(edgeColour : TColor);
begin
  SPerform(SCI_SETEDGECOLOUR, ColorToRGB(edgeColour), 0);
end;

procedure TScintillaBase.SearchAnchor;
begin
  SPerform(SCI_SEARCHANCHOR, 0, 0);
end;

function TScintillaBase.SearchNext(flags : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_SEARCHNEXT, flags, LongInt(text));
end;

function TScintillaBase.SearchPrev(flags : LongInt; text : PChar) : LongInt;
begin
  result := SPerform(SCI_SEARCHPREV, flags, LongInt(text));
end;

function TScintillaBase.LinesOnScreen : LongInt;
begin
  result := SPerform(SCI_LINESONSCREEN, 0, 0);
end;

procedure TScintillaBase.UsePopUp(allowPopUp : Boolean);
begin
  SPerform(SCI_USEPOPUP, LongInt(allowPopUp), 0);
end;

function TScintillaBase.SelectionIsRectangle : Boolean;
begin
  result := Boolean(SPerform(SCI_SELECTIONISRECTANGLE, 0, 0));
end;

procedure TScintillaBase.SetZoom(zoom : LongInt);
begin
  SPerform(SCI_SETZOOM, zoom, 0);
end;

function TScintillaBase.GetZoom : LongInt;
begin
  result := SPerform(SCI_GETZOOM, 0, 0);
end;

function TScintillaBase.CreateDocument : LongInt;
begin
  result := SPerform(SCI_CREATEDOCUMENT, 0, 0);
end;

procedure TScintillaBase.AddRefDocument(doc : LongInt);
begin
  SPerform(SCI_ADDREFDOCUMENT, 0, doc);
end;

procedure TScintillaBase.ReleaseDocument(doc : LongInt);
begin
  SPerform(SCI_RELEASEDOCUMENT, 0, doc);
end;

function TScintillaBase.GetModEventMask : LongInt;
begin
  result := SPerform(SCI_GETMODEVENTMASK, 0, 0);
end;

procedure TScintillaBase.SetFocusEx(focus : Boolean);
begin
  SPerform(SCI_SETFOCUSEX, LongInt(focus), 0);
end;

function TScintillaBase.GetFocus : Boolean;
begin
  result := Boolean(SPerform(SCI_GETFOCUS, 0, 0));
end;

procedure TScintillaBase.SetStatus(statusCode : LongInt);
begin
  SPerform(SCI_SETSTATUS, statusCode, 0);
end;

function TScintillaBase.GetStatus : LongInt;
begin
  result := SPerform(SCI_GETSTATUS, 0, 0);
end;

procedure TScintillaBase.SetMouseDownCaptures(captures : Boolean);
begin
  SPerform(SCI_SETMOUSEDOWNCAPTURES, LongInt(captures), 0);
end;

function TScintillaBase.GetMouseDownCaptures : Boolean;
begin
  result := Boolean(SPerform(SCI_GETMOUSEDOWNCAPTURES, 0, 0));
end;

procedure TScintillaBase.SetCursor(cursorType : LongInt);
begin
  SPerform(SCI_SETCURSOR, cursorType, 0);
end;

function TScintillaBase.GetCursor : LongInt;
begin
  result := SPerform(SCI_GETCURSOR, 0, 0);
end;

procedure TScintillaBase.SetControlCharSymbol(symbol : LongInt);
begin
  SPerform(SCI_SETCONTROLCHARSYMBOL, symbol, 0);
end;

function TScintillaBase.GetControlCharSymbol : LongInt;
begin
  result := SPerform(SCI_GETCONTROLCHARSYMBOL, 0, 0);
end;

procedure TScintillaBase.WordPartLeft;
begin
  SPerform(SCI_WORDPARTLEFT, 0, 0);
end;

procedure TScintillaBase.WordPartLeftExtend;
begin
  SPerform(SCI_WORDPARTLEFTEXTEND, 0, 0);
end;

procedure TScintillaBase.WordPartRight;
begin
  SPerform(SCI_WORDPARTRIGHT, 0, 0);
end;

procedure TScintillaBase.WordPartRightExtend;
begin
  SPerform(SCI_WORDPARTRIGHTEXTEND, 0, 0);
end;

procedure TScintillaBase.SetVisiblePolicy(visiblePolicy : LongInt; visibleSlop : LongInt);
begin
  SPerform(SCI_SETVISIBLEPOLICY, visiblePolicy, visibleSlop);
end;

procedure TScintillaBase.DelLineLeft;
begin
  SPerform(SCI_DELLINELEFT, 0, 0);
end;

procedure TScintillaBase.DelLineRight;
begin
  SPerform(SCI_DELLINERIGHT, 0, 0);
end;

procedure TScintillaBase.SetXOffset(newOffset : LongInt);
begin
  SPerform(SCI_SETXOFFSET, newOffset, 0);
end;

function TScintillaBase.GetXOffset : LongInt;
begin
  result := SPerform(SCI_GETXOFFSET, 0, 0);
end;

procedure TScintillaBase.ChooseCaretX;
begin
  SPerform(SCI_CHOOSECARETX, 0, 0);
end;

procedure TScintillaBase.GrabFocus;
begin
  SPerform(SCI_GRABFOCUS, 0, 0);
end;

procedure TScintillaBase.SetXCaretPolicy(caretPolicy : LongInt; caretSlop : LongInt);
begin
  SPerform(SCI_SETXCARETPOLICY, caretPolicy, caretSlop);
end;

procedure TScintillaBase.SetYCaretPolicy(caretPolicy : LongInt; caretSlop : LongInt);
begin
  SPerform(SCI_SETYCARETPOLICY, caretPolicy, caretSlop);
end;

procedure TScintillaBase.SetPrintWrapMode(mode : LongInt);
begin
  SPerform(SCI_SETPRINTWRAPMODE, mode, 0);
end;

function TScintillaBase.GetPrintWrapMode : LongInt;
begin
  result := SPerform(SCI_GETPRINTWRAPMODE, 0, 0);
end;

procedure TScintillaBase.SetHotspotActiveFore(useSetting : Boolean; fore : TColor);
begin
  SPerform(SCI_SETHOTSPOTACTIVEFORE, LongInt(useSetting), ColorToRGB(fore));
end;

procedure TScintillaBase.SetHotspotActiveBack(useSetting : Boolean; back : TColor);
begin
  SPerform(SCI_SETHOTSPOTACTIVEBACK, LongInt(useSetting), ColorToRGB(back));
end;

procedure TScintillaBase.SetHotspotActiveUnderline(underline : Boolean);
begin
  SPerform(SCI_SETHOTSPOTACTIVEUNDERLINE, LongInt(underline), 0);
end;

procedure TScintillaBase.SetHotspotSingleLine(singleLine : Boolean);
begin
  SPerform(SCI_SETHOTSPOTSINGLELINE, LongInt(singleLine), 0);
end;

procedure TScintillaBase.ParaDown;
begin
  SPerform(SCI_PARADOWN, 0, 0);
end;

procedure TScintillaBase.ParaDownExtend;
begin
  SPerform(SCI_PARADOWNEXTEND, 0, 0);
end;

procedure TScintillaBase.ParaUp;
begin
  SPerform(SCI_PARAUP, 0, 0);
end;

procedure TScintillaBase.ParaUpExtend;
begin
  SPerform(SCI_PARAUPEXTEND, 0, 0);
end;

function TScintillaBase.PositionBefore(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_POSITIONBEFORE, pos, 0);
end;

function TScintillaBase.PositionAfter(pos : LongInt) : LongInt;
begin
  result := SPerform(SCI_POSITIONAFTER, pos, 0);
end;

procedure TScintillaBase.CopyRange(start : LongInt; end_ : LongInt);
begin
  SPerform(SCI_COPYRANGE, start, end_);
end;

procedure TScintillaBase.CopyText(length : LongInt; text : PChar);
begin
  SPerform(SCI_COPYTEXT, length, LongInt(text));
end;

procedure TScintillaBase.SetSelectionMode(mode : LongInt);
begin
  SPerform(SCI_SETSELECTIONMODE, mode, 0);
end;

function TScintillaBase.GetSelectionMode : LongInt;
begin
  result := SPerform(SCI_GETSELECTIONMODE, 0, 0);
end;

function TScintillaBase.GetLineSelStartPosition(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINESELSTARTPOSITION, line, 0);
end;

function TScintillaBase.GetLineSelEndPosition(line : LongInt) : LongInt;
begin
  result := SPerform(SCI_GETLINESELENDPOSITION, line, 0);
end;

procedure TScintillaBase.LineDownRectExtend;
begin
  SPerform(SCI_LINEDOWNRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.LineUpRectExtend;
begin
  SPerform(SCI_LINEUPRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.CharLeftRectExtend;
begin
  SPerform(SCI_CHARLEFTRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.CharRightRectExtend;
begin
  SPerform(SCI_CHARRIGHTRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.HomeRectExtend;
begin
  SPerform(SCI_HOMERECTEXTEND, 0, 0);
end;

procedure TScintillaBase.VCHomeRectExtend;
begin
  SPerform(SCI_VCHOMERECTEXTEND, 0, 0);
end;

procedure TScintillaBase.LineEndRectExtend;
begin
  SPerform(SCI_LINEENDRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.PageUpRectExtend;
begin
  SPerform(SCI_PAGEUPRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.PageDownRectExtend;
begin
  SPerform(SCI_PAGEDOWNRECTEXTEND, 0, 0);
end;

procedure TScintillaBase.StutteredPageUp;
begin
  SPerform(SCI_STUTTEREDPAGEUP, 0, 0);
end;

procedure TScintillaBase.StutteredPageUpExtend;
begin
  SPerform(SCI_STUTTEREDPAGEUPEXTEND, 0, 0);
end;

procedure TScintillaBase.StutteredPageDown;
begin
  SPerform(SCI_STUTTEREDPAGEDOWN, 0, 0);
end;

procedure TScintillaBase.StutteredPageDownExtend;
begin
  SPerform(SCI_STUTTEREDPAGEDOWNEXTEND, 0, 0);
end;

procedure TScintillaBase.WordLeftEnd;
begin
  SPerform(SCI_WORDLEFTEND, 0, 0);
end;

procedure TScintillaBase.WordLeftEndExtend;
begin
  SPerform(SCI_WORDLEFTENDEXTEND, 0, 0);
end;

procedure TScintillaBase.WordRightEnd;
begin
  SPerform(SCI_WORDRIGHTEND, 0, 0);
end;

procedure TScintillaBase.WordRightEndExtend;
begin
  SPerform(SCI_WORDRIGHTENDEXTEND, 0, 0);
end;

procedure TScintillaBase.SetWhitespaceChars(characters : PChar);
begin
  SPerform(SCI_SETWHITESPACECHARS, 0, LongInt(characters));
end;

procedure TScintillaBase.SetCharsDefault;
begin
  SPerform(SCI_SETCHARSDEFAULT, 0, 0);
end;

function TScintillaBase.AutoCGetCurrent : LongInt;
begin
  result := SPerform(SCI_AUTOCGETCURRENT, 0, 0);
end;

procedure TScintillaBase.Allocate(bytes : LongInt);
begin
  SPerform(SCI_ALLOCATE, bytes, 0);
end;

function TScintillaBase.FindColumn(line : LongInt; column : LongInt) : LongInt;
begin
  result := SPerform(SCI_FINDCOLUMN, line, column);
end;

function TScintillaBase.GetCaretSticky : Boolean;
begin
  result := Boolean(SPerform(SCI_GETCARETSTICKY, 0, 0));
end;

procedure TScintillaBase.SetCaretSticky(useCaretStickyBehaviour : Boolean);
begin
  SPerform(SCI_SETCARETSTICKY, LongInt(useCaretStickyBehaviour), 0);
end;

procedure TScintillaBase.ToggleCaretSticky;
begin
  SPerform(SCI_TOGGLECARETSTICKY, 0, 0);
end;

procedure TScintillaBase.StartRecord;
begin
  SPerform(SCI_STARTRECORD, 0, 0);
end;

procedure TScintillaBase.StopRecord;
begin
  SPerform(SCI_STOPRECORD, 0, 0);
end;

procedure TScintillaBase.SetLexer(lexer : LongInt);
begin
  SPerform(SCI_SETLEXER, lexer, 0);
end;

function TScintillaBase.GetLexer : LongInt;
begin
  result := SPerform(SCI_GETLEXER, 0, 0);
end;

procedure TScintillaBase.Colourise(start : LongInt; end_ : LongInt);
begin
  SPerform(SCI_COLOURISE, start, end_);
end;

procedure TScintillaBase.SetProperty(key : PChar; value : PChar);
begin
  SPerform(SCI_SETPROPERTY, LongInt(key), LongInt(value));
end;

procedure TScintillaBase.SetKeyWords(keywordSet : LongInt; keyWords : PChar);
begin
  SPerform(SCI_SETKEYWORDS, keywordSet, LongInt(keyWords));
end;

procedure TScintillaBase.SetLexerLanguage(language : PChar);
begin
  SPerform(SCI_SETLEXERLANGUAGE, 0, LongInt(language));
end;

procedure TScintillaBase.LoadLexerLibrary(path : PChar);
begin
  SPerform(SCI_LOADLEXERLIBRARY, 0, LongInt(path));
end;

//--FuncImp

procedure TScintillaBase.DeleteWordRightAndSkip;
var
len : Integer;
start,end_ : Integer;
c : Integer;
begin
  BeginUndoAction;
  DelWordRight;
  start:=GetCurrentPos;
  end_:=start;
  len:=GetLength;
  c:=GetCharAt(end_);
  while (len>end_) and isspace(c) do
  begin
    Inc(end_);
    c:=GetCharAt(end_);
  end;
  if (end_-start)>0 then
  begin

    SetTargetStart(start);
    SetTargetEnd(end_);
    ReplaceTarget(0,'');
    InsertText(start,' ');
  end;
  EndUndoAction;
end;

initialization
	scmod := LoadLibrary('SciLexer.DLL');
finalization
	if scmod > 0 then FreeLibrary(scmod);
end.


