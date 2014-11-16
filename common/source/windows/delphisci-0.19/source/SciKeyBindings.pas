//CE_Desc_Include(helpdescriptions.txt)
{
 Adopted from the DelphiWrapper at http://www.pnotepad.com/scintilla
     $Id: SciKeyBindings.pas,v 1.4 2004/11/13 04:29:50 hdalis Exp $
 History:   29/09/2004 Initial Release with Delphi Scintilla Interface Components
            13/10/2004 Added help using ClassExplorer 6.0, Use the helpgenerator to
                       generate the help.
            29/10/2004 Changed the shortcuts for Paraup/Paradown/Wordpartleft/wordpartright * commands 'cause it didn't work with
                       []/\ in all languages. Seemed more logical also.
                       removed the commenting of the stuttered commands.. Don't know if these work, but..
                       (hdalis@users.sourceforge.net)
            19/04/2005 Added the TSciCommandRecHeader, TSciCommandRec records.
                       Added the StickyCaret to the command list. (v1.63)
                       Added VCStyle property..
						20/05/2005 Added LoadKeyCommands and SaveKeyCommands (both stream and file).
}
{$Include SciCommonDef.Inc}
unit SciKeyBindings;

interface
Uses
  Windows, Classes, Controls, SciSupport,SciResLang;

const KeyFileHeaderSignature =$EDEDED;

Type
  TSciKey = Cardinal;

  //This is the header structure you could use when saving the keys to
  //a file..
  TSciCommandRecHeader=record
    Signature : Integer;
    NumberOfKeys : Integer;
  end;

  //This is the actual key/command.
  TSciCommandRec = record
    ShortCut : TShortCut;
    Command : Integer;
  end;

  function KeyToSciKey(keyIn : word) : word;
  function SciKeyToKey(keyIn : word) : word;
  function ShiftStateToSciMode(Shift : TShiftState) : word;
  function SciModeToShiftState(SciMode : word) : TShiftState;
  function SciKey(Key, Mode : word): TSciKey;
  function SciKeyToShortCut(Key : TSciKey) : TShortCut;
  function ShortCutToSciKey(ShortCut : TShortCut) : TSciKey;
  
const
  SCI_NORM : word = 0;
  SCI_SHIFT : word = SCMOD_SHIFT;
  SCI_CTRL : word  = SCMOD_CTRL;
  SCI_ALT : word  = SCMOD_ALT;
  SCI_ASHIFT : word = SCMOD_ALT or SCMOD_SHIFT;
  SCI_ACTRL : word = SCMOD_ALT or SCMOD_CTRL;
  SCI_CSHIFT : word = SCMOD_CTRL or SCMOD_SHIFT;
  SCI_ACSHIFT : word = SCMOD_ALT or SCMOD_CTRL or SCMOD_SHIFT;
                                   //85

  Sci_KeyboardCommandMap: array[0..94] of TIdentMapEntry = (
    (Value: SCI_Null;                     Name: sKeyNoCommand),
    (Value: SCI_CLEAR;                    Name: sKeyClear),
    (Value: SCI_COPY;                     Name: sKeyCopy),
    (Value: SCI_CUT;                      Name: sKeyCut),
    (Value: SCI_PASTE;                    Name: sKeyPaste),
    (Value: SCI_ZOOMIN;                   Name: sKeyZoomIn),
    (Value: SCI_ZOOMOUT;                  Name: sKeyZoomOut),
    (Value: SCI_SETZOOM;                  Name: sKeyZoomReset),
    (Value: SCI_SELECTALL;                Name: sKeySelectAll),
    (Value: SCI_UNDO;                     Name: sKeyUndo),

    (Value: SCI_REDO;                     Name: sKeyRedo),
    (Value: SCI_LINEDOWN;                 Name: sKeyLineDown),
    (Value: SCI_LINEDOWNEXTEND;           Name: sKeyLineDownExtend),
    (Value: SCI_LINEDOWNRECTEXTEND;       Name: sKeyLineDownRectExtend),
    (Value: SCI_LINESCROLLDOWN;           Name: sKeyLineScrollDown),
    (Value: SCI_LINEUP;                   Name: sKeyLineUp),
    (Value: SCI_LINEUPEXTEND;             Name: sKeyLineUpExtend),
    (Value: SCI_LINEUPRECTEXTEND;         Name: sKeyLineUpRectExtend),
    (Value: SCI_LINESCROLLUP;             Name: sKeyLineScrollUp),
    (Value: SCI_PARADOWN;                 Name: sKeyLineParaDown),
    (Value: SCI_PARADOWNEXTEND;           Name: sKeyLineParaDownExtend),
    (Value: SCI_PARAUP;                   Name: sKeyLineParaUp),
    (Value: SCI_PARAUPEXTEND;             Name: sKeyLineParaUpExtend),
    (Value: SCI_CHARLEFT;                 Name: sKeyCharLeft),
    (Value: SCI_CHARLEFTEXTEND;           Name: sKeyCharLeftExtend),
    (Value: SCI_CHARLEFTRECTEXTEND;       Name: sKeyCharLeftRectExtend),
    (Value: SCI_CHARRIGHT;                Name: sKeyCharRight),
    (Value: SCI_CHARRIGHTEXTEND;          Name: sKeyCharRightExtend),
    (Value: SCI_CHARRIGHTRECTEXTEND;      Name: sKeyCharRightRectExtend),
    (Value: SCI_WORDLEFT;                 Name: sKeyWordLeft),
    (Value: SCI_WORDLEFTEXTEND;           Name: sKeyWordLeftExtend),
    (Value: SCI_WORDRIGHT;                Name: sKeyWordRight),
    (Value: SCI_WORDRIGHTEXTEND;          Name: sKeyWordRightExtend),
    //{
    (Value: SCI_WORDLEFTEND;              Name: sKeyWordLeftEnd),
    (Value: SCI_WORDLEFTENDEXTEND;        Name: sKeyWordLeftEndExtend),
		(Value: SCI_WORDRIGHTEND;             Name: sKeyWordRightEnd),
    (Value: SCI_WORDRIGHTENDEXTEND;       Name: sKeyWordRightEndExtend),
    //}
    (Value: SCI_WORDPARTLEFT;             Name: sKeyWordPartLeft),
    (Value: SCI_WORDPARTLEFTEXTEND;       Name: sKeyWordPartLeftExtend),
    (Value: SCI_WORDPARTRIGHT;            Name: sKeyWordPartRight),
    (Value: SCI_WORDPARTRIGHTEXTEND;      Name: sKeyWordPartRightExtend),
    (Value: SCI_HOME;                     Name: sKeyHome),
    (Value: SCI_HOMEEXTEND;               Name: sKeyHomeExtend),
    (Value: SCI_HOMERECTEXTEND;           Name: sKeyHomeRectExtend),
    (Value: SCI_HOMEDISPLAY;              Name: sKeyHomeDisplay),
    (Value: SCI_HOMEDISPLAYEXTEND;        Name: sKeyHomeDisplayExtend),
    (Value: SCI_HOMEWRAP;                 Name: sKeyHomeWrap),
    (Value: SCI_HOMEWRAPEXTEND;           Name: sKeyHomeWrapExtend),
    (Value: SCI_VCHOME;                   Name: sKeyVCHome),
    (Value: SCI_VCHOMEEXTEND;             Name: sKeyVCHomeExtend),
    (Value: SCI_VCHOMERECTEXTEND;         Name: sKeyVCHomeRectExtend),
    (Value: SCI_VCHOMEWRAP;               Name: sKeyVCHomeWrap),
    (Value: SCI_VCHOMEWRAPEXTEND;         Name: sKeyVCHomeWrapExtend),
    (Value: SCI_LINEEND;                  Name: sKeyLineEnd),
    (Value: SCI_LINEENDEXTEND;            Name: sKeyLineEndExtend),
    (Value: SCI_LINEENDRECTEXTEND;        Name: sKeyLineEndRectExtend),
    (Value: SCI_LINEENDDISPLAY;           Name: sKeyLineEndDisplay),
    (Value: SCI_LINEENDDISPLAYEXTEND;     Name: sKeyLineEndDisplayExtend),
    (Value: SCI_LINEENDWRAP;              Name: sKeyLineEndWrap),
    (Value: SCI_LINEENDWRAPEXTEND;        Name: sKeyLineEndWrapExtend),
    (Value: SCI_DOCUMENTSTART;            Name: sKeyDocumentStart),
    (Value: SCI_DOCUMENTSTARTEXTEND;      Name: sKeyDocumentStartExtend),
    (Value: SCI_DOCUMENTEND;              Name: sKeyDocumentEnd),
    (Value: SCI_DOCUMENTENDEXTEND;        Name: sKeyDocumentEndExtend),
    (Value: SCI_PAGEUP;                   Name: sKeyPageUp),
    (Value: SCI_PAGEUPEXTEND;             Name: sKeyPageUpExtend),
    (Value: SCI_PAGEUPRECTEXTEND;         Name: sKeyPageUpRectExtend),
    (Value: SCI_PAGEDOWN;                 Name: sKeyPageDown),
    (Value: SCI_PAGEDOWNEXTEND;           Name: sKeyPageDownExtend),
    (Value: SCI_PAGEDOWNRECTEXTEND;       Name: sKeyPageDownRectExtend),
    //{
    (Value: SCI_STUTTEREDPAGEUP;          Name: sKeyStutteredPageUp),
    (Value: SCI_STUTTEREDPAGEUPEXTEND;    Name: sKeyStutteredPageUpExtend),
		(Value: SCI_STUTTEREDPAGEDOWN;        Name: sKeyStutteredPageDown),
    (Value: SCI_STUTTEREDPAGEDOWNEXTEND;  Name: sKeyStutteredPageDownExtend),
    //}
    (Value: SCI_DELETEBACK;               Name: sKeyDeleteBack),
    (Value: SCI_DELETEBACKNOTLINE;        Name: sKeyDeleteBackNotLine),
    (Value: SCI_DELWORDLEFT;              Name: sKeyDeleteWordLeft),
    (Value: SCI_DELWORDRIGHT;             Name: sKeyDeleteWordRight),
    (Value: SCI_DELLINELEFT;              Name: sKeyDeleteLineLeft),
    (Value: SCI_DELLINERIGHT;             Name: sKeyDeleteLineRight),
    (Value: SCI_LINEDELETE;               Name: sKeyDeleteLine),
    (Value: SCI_DELWORDRIGHT;             Name: sKeyDeleteWordRight),
    (Value: SCI_LINECUT;                  Name: sKeyLineCut),
    (Value: SCI_LINECOPY;                 Name: sKeyLineCopy),
    (Value: SCI_LINETRANSPOSE;            Name: sKeyLineTranspose),
    (Value: SCI_LINEDUPLICATE;            Name: sKeyLineDuplicate),
    (Value: SCI_LOWERCASE;                Name: sKeyLowerCase),
    (Value: SCI_UPPERCASE;                Name: sKeyUpperCase),
    (Value: SCI_CANCEL;                   Name: sKeyCancel),
    (Value: SCI_EDITTOGGLEOVERTYPE;       Name: sKeyToggleOvertype),
    (Value: SCI_NEWLINE;                  Name: sKeyNewLine),
    (Value: SCI_FORMFEED;                 Name: sKeyFormFeed),
    (Value: SCI_TAB;                      Name: sKeyTab),
    (Value: SCI_BACKTAB;                  Name: sKeyBackTab),
    (Value: SCI_TOGGLECARETSTICKY;        Name: sKeyToggleSticky)
    );

Type

  TSciKeyCommand = class(TCollectionItem)
  private
    FCommand: Integer;
    FShortCut : TShortCut;
    procedure SetCommand(const Value: Integer);
		procedure SetShortCut(const Value: TShortCut);
	protected
		function GetDisplayName: string; override;
	public
		constructor Create(Collection: TCollection); override;
		procedure Assign(Source: TPersistent); override;
	published
		property Command: Integer read FCommand write SetCommand;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
  end;

  TSciKeyCommandCollection = class(TCollection)
  private
		fEditor : TWinControl; //TScintilla
    fVCStyle : Boolean;
    procedure SetEditor(Editor : TWinControl);
    procedure SetVCStyle(const Value : Boolean);
	protected
	public
    constructor Create(Editor : TWinControl);
    procedure Assign(Source: TPersistent); override;
    procedure Update(Item: TCollectionItem); override;
    procedure AddCommandKey(Key, Mode : word; Command : Integer);
    procedure AddRec(const Rec : TSciCommandRec);
    procedure ResetDefaultCommands;
    function FindShortCut(ShortCut: TShortCut) : TSciKeyCommand;
    property Editor : TWinControl read FEditor write SetEditor;
    property VCStyle : Boolean read fVCStyle write SetVCStyle;
  end;


function LoadKeyCommands(Collection : TSciKeyCommandCollection;const filename : String) : Boolean;overload;
function LoadKeyCommands(Collection : TSciKeyCommandCollection;stream : TStream) : Boolean;overload;
function SaveKeyCommands(Collection : TSciKeyCommandCollection;stream : TStream) : Boolean;overload;
function SaveKeyCommands(Collection : TSciKeyCommandCollection;const filename : String) : Boolean;overload;


implementation

Uses
  SciLexerMemo,Menus,SysUtils;

const
	VK_OEM_1 : word = $BA;
	VK_OEM_2 : word = $BF;
	VK_OEM_3 : word = $C0;
	VK_OEM_4 : word = $DB;
	VK_OEM_5 : word = $DC;
	VK_OEM_6 : word = $DD;
function KeyToSciKey(keyIn : word) : word;
begin
	case keyIn of
		VK_DOWN:		 Result := SCK_DOWN;
		VK_UP:		   Result := SCK_UP;
		VK_LEFT:		 Result := SCK_LEFT;
		VK_RIGHT:	   Result := SCK_RIGHT;
		VK_HOME:		 Result := SCK_HOME;
		VK_END:		   Result := SCK_END;
		VK_PRIOR:	   Result := SCK_PRIOR;
		VK_NEXT:		 Result := SCK_NEXT;
		VK_DELETE:	 Result := SCK_DELETE;
		VK_INSERT:	 Result := SCK_INSERT;
		VK_ESCAPE:	 Result := SCK_ESCAPE;
		VK_BACK:		 Result := SCK_BACK;
		VK_TAB:		   Result := SCK_TAB;
		VK_Return:   Result := SCK_RETURN;
		VK_ADD:		   Result := SCK_ADD;
		VK_SUBTRACT: Result := SCK_SUBTRACT;
		VK_DIVIDE:	 Result := SCK_DIVIDE;
		else			   Result := keyIn;
	end;
end;


function SciKeyToKey(keyIn : word) : word;
begin
	case keyIn of
		SCK_DOWN:		 Result := VK_DOWN;
		SCK_UP:		   Result := VK_UP;
		SCK_LEFT:		 Result := VK_LEFT;
		SCK_RIGHT:	 Result := VK_RIGHT;
		SCK_HOME:		 Result := VK_HOME;
		SCK_END:		 Result := VK_END;
		SCK_PRIOR:	 Result := VK_PRIOR;
		SCK_NEXT:		 Result := VK_NEXT;
		SCK_DELETE:	 Result := VK_DELETE;
		SCK_INSERT:	 Result := VK_INSERT;
		SCK_ESCAPE:	 Result := VK_ESCAPE;
		SCK_BACK:		 Result := VK_BACK;
		SCK_TAB:		 Result := VK_TAB;
		SCK_Return:  Result := VK_RETURN;
		SCK_ADD:		 Result := VK_ADD;
		SCK_SUBTRACT:Result := VK_SUBTRACT;
		SCK_DIVIDE:	 Result := VK_DIVIDE;

    Ord(';'):    Result := VK_OEM_1;
    Ord('/'):    Result := VK_OEM_2;
    Ord('`'):    Result := VK_OEM_3;
    Ord('['):    Result := VK_OEM_4;
    Ord('\'):    Result := VK_OEM_5;
    Ord(']'):    Result := VK_OEM_6;

		else			   Result := keyIn;
	end;
end;

function ShiftStateToSciMode(Shift : TShiftState) : word;
begin
  Result := 0;
  if ssShift in Shift then Result := Result or SCMOD_SHIFT;
  if ssCtrl  in Shift then Result := Result or SCMOD_CTRL;
  if ssAlt in Shift then Result := Result or SCMOD_ALT;
end;

function SciKey(Key, Mode : word): TSciKey;
begin
  Result := (Cardinal(Mode) shl 16) or Cardinal(Key);
end;

function SciModeToShiftState(SciMode : word) : TShiftState;
begin
  Result := [];
  if SciMode and SCMOD_SHIFT <> 0 then Include(Result, ssShift);
  if SciMode and SCMOD_CTRL <> 0 then Include(Result, ssCtrl);
  if SciMode and SCMOD_ALT <> 0 then Include(Result, ssAlt);
end;

function SciKeyToShortCut(Key : TSciKey) : TShortCut;
begin
  Result := ShortCut(SciKeyToKey(LoWord(Key)), SciModeToShiftState(HiWord(Key)));
end;

function ShortCutToSciKey(ShortCut : TShortCut) : TSciKey;
var
  Key: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(ShortCut, Key, Shift);
  Result := (Cardinal(ShiftStateToSciMode(Shift)) shl 16) or Cardinal(KeyToSciKey(Key));
end;


{ TSciKeyCommandCollection }

procedure TSciKeyCommandCollection.AddCommandKey(Key, Mode: word;
  Command: Integer);
Var
  KeyCommand : TSciKeyCommand;
begin
  KeyCommand := Add as TSciKeyCommand;
  KeyCommand.FCommand := Command;
  KeyCommand.FShortCut := SciKeyToShortCut(SciKey(Key, Mode));
end;

procedure TSciKeyCommandCollection.AddRec(const Rec : TSciCommandRec);
Var
  KeyCommand : TSciKeyCommand;
begin
  KeyCommand := Add as TSciKeyCommand;
  KeyCommand.FCommand := Rec.Command;
  KeyCommand.FShortCut := Rec.ShortCut;
end;

procedure TSciKeyCommandCollection.SetVCStyle(const Value : Boolean);
var
  i,cnt : Integer;
  cmd : TSciKeyCommand;
begin
  fVCStyle:=Value;
  cnt:=Count;
  for i:=0 to (cnt-1) do
  begin
    cmd :=TSciKeyCommand(Items[i]);
    if fVCStyle=False then
    begin
      case cmd.FCommand of
        SCI_VCHOME: cmd.Command:=SCI_HOME;
        SCI_VCHOMEEXTEND: cmd.Command:=SCI_HOMEEXTEND;
        SCI_VCHOMEWRAP: cmd.Command:=SCI_HOMEWRAP;
        SCI_VCHOMEWRAPEXTEND: cmd.Command:=SCI_HOMEWRAPEXTEND;
        SCI_VCHOMERECTEXTEND: cmd.Command:=SCI_HOMERECTEXTEND;
      end;
    end else
    begin
      case cmd.FCommand of
        SCI_HOME: cmd.Command:=SCI_VCHOME;
        SCI_HOMEEXTEND: cmd.Command:=SCI_VCHOMEEXTEND;
        SCI_HOMEWRAP: cmd.Command:=SCI_VCHOMEWRAP;
        SCI_HOMEWRAPEXTEND: cmd.Command:=SCI_VCHOMEWRAPEXTEND;
        SCI_HOMERECTEXTEND: cmd.Command:=SCI_VCHOMERECTEXTEND;
      end;
    end;
  end;
end;

constructor TSciKeyCommandCollection.Create(Editor: TWinControl);
begin
  fVCStyle:=True;
  inherited Create(TSciKeyCommand);
  FEditor := Editor;
  ResetDefaultCommands;
end;

function TSciKeyCommandCollection.FindShortCut(
  ShortCut: TShortCut): TSciKeyCommand;
var
  i : Integer;
  KeyCommand : TSciKeyCommand;
begin
  Result := nil;
  for i := 0 to Count - 1 do begin
    KeyCommand := Items[i] as TSciKeyCommand;
    if KeyCommand.ShortCut = ShortCut then
    begin
      Result := KeyCommand;
      Exit;
    end;
  end;
end;

procedure TSciKeyCommandCollection.ResetDefaultCommands;
begin
  // Set Default Commands
  BeginUpdate;
  try
    Clear;
    AddCommandKey(SCK_DOWN,		SCI_NORM,	  SCI_LINEDOWN);
    AddCommandKey(SCK_DOWN,		SCI_SHIFT,	SCI_LINEDOWNEXTEND);
    AddCommandKey(SCK_DOWN,		SCI_CTRL,	  SCI_LINESCROLLDOWN);
    AddCommandKey(SCK_DOWN,		SCI_ASHIFT,	SCI_LINEDOWNRECTEXTEND);
    AddCommandKey(SCK_UP,		  SCI_NORM,	  SCI_LINEUP);
    AddCommandKey(SCK_UP,			SCI_SHIFT,	SCI_LINEUPEXTEND);
    AddCommandKey(SCK_UP,			SCI_CTRL,	  SCI_LINESCROLLUP);
    AddCommandKey(SCK_UP,		  SCI_ASHIFT,	SCI_LINEUPRECTEXTEND);
    AddCommandKey(SCK_UP,		  SCI_ACTRL, 	SCI_PARAUP);
    AddCommandKey(SCK_UP,		 SCI_ACSHIFT,	SCI_PARAUPEXTEND);
    AddCommandKey(SCK_DOWN,		SCI_ACTRL,		SCI_PARADOWN);
    AddCommandKey(SCK_DOWN,		SCI_ACSHIFT,	SCI_PARADOWNEXTEND);
    AddCommandKey(SCK_LEFT,		SCI_NORM,	  SCI_CHARLEFT);
    AddCommandKey(SCK_LEFT,		SCI_SHIFT,	SCI_CHARLEFTEXTEND);
    AddCommandKey(SCK_LEFT,		SCI_CTRL,	  SCI_WORDLEFT);
    AddCommandKey(SCK_LEFT,		SCI_CSHIFT,	SCI_WORDLEFTEXTEND);
    AddCommandKey(SCK_LEFT,		SCI_ASHIFT,	SCI_CHARLEFTRECTEXTEND);
    AddCommandKey(SCK_RIGHT,	SCI_NORM,	  SCI_CHARRIGHT);
    AddCommandKey(SCK_RIGHT,	SCI_SHIFT,	SCI_CHARRIGHTEXTEND);
    AddCommandKey(SCK_RIGHT,	SCI_CTRL,	  SCI_WORDRIGHT);
    AddCommandKey(SCK_RIGHT,	SCI_CSHIFT,	SCI_WORDRIGHTEXTEND);
    AddCommandKey(SCK_RIGHT,	SCI_ASHIFT,	SCI_CHARRIGHTRECTEXTEND);
    AddCommandKey(SCK_LEFT,		SCI_ACTRL,		SCI_WORDPARTLEFT);
    AddCommandKey(SCK_LEFT,		SCI_ACSHIFT,	SCI_WORDPARTLEFTEXTEND);
    AddCommandKey(SCK_RIGHT,	SCI_ACTRL,		SCI_WORDPARTRIGHT);
    AddCommandKey(SCK_RIGHT,	SCI_ACSHIFT,	SCI_WORDPARTRIGHTEXTEND);
    if fVCStyle=True then
    begin
      AddCommandKey(SCK_HOME,		SCI_NORM,	  SCI_VCHOME);
      AddCommandKey(SCK_HOME, 	SCI_SHIFT, 	SCI_VCHOMEEXTEND);
    end else
    begin
      AddCommandKey(SCK_HOME,		SCI_NORM,	  SCI_HOME);
      AddCommandKey(SCK_HOME, 	SCI_SHIFT, 	SCI_HOMEEXTEND);
    end;
    AddCommandKey(SCK_HOME, 	SCI_CTRL, 	SCI_DOCUMENTSTART);
    AddCommandKey(SCK_HOME, 	SCI_CSHIFT, SCI_DOCUMENTSTARTEXTEND);
    AddCommandKey(SCK_HOME, 	SCI_ALT, 	  SCI_HOMEDISPLAY);
    if fVCStyle=True then
    begin
      AddCommandKey(SCK_HOME,		SCI_ASHIFT,	SCI_VCHOMERECTEXTEND);
    end else
    begin
      AddCommandKey(SCK_HOME,		SCI_ASHIFT,	SCI_HOMERECTEXTEND);
    end;
    AddCommandKey(SCK_END,	 	SCI_NORM,	  SCI_LINEEND);
    AddCommandKey(SCK_END,	 	SCI_SHIFT, 	SCI_LINEENDEXTEND);
    AddCommandKey(SCK_END, 		SCI_CTRL, 	SCI_DOCUMENTEND);
    AddCommandKey(SCK_END, 		SCI_CSHIFT, SCI_DOCUMENTENDEXTEND);
    AddCommandKey(SCK_END, 		SCI_ALT, 	  SCI_LINEENDDISPLAY);
    AddCommandKey(SCK_END,		SCI_ASHIFT,	SCI_LINEENDRECTEXTEND);
    AddCommandKey(SCK_PRIOR,	SCI_NORM,	  SCI_PAGEUP);
    AddCommandKey(SCK_PRIOR,	SCI_SHIFT, 	SCI_PAGEUPEXTEND);
    AddCommandKey(SCK_PRIOR,	SCI_ASHIFT,	SCI_PAGEUPRECTEXTEND);
    AddCommandKey(SCK_NEXT, 	SCI_NORM, 	SCI_PAGEDOWN);
    AddCommandKey(SCK_NEXT, 	SCI_SHIFT, 	SCI_PAGEDOWNEXTEND);
    AddCommandKey(SCK_NEXT,		SCI_ASHIFT,	SCI_PAGEDOWNRECTEXTEND);
    AddCommandKey(SCK_DELETE, SCI_NORM,	  SCI_CLEAR);
    AddCommandKey(SCK_DELETE, SCI_SHIFT,	SCI_CUT);
    AddCommandKey(SCK_DELETE, SCI_CTRL,	  SCI_DELWORDRIGHT);
    AddCommandKey(SCK_DELETE,	SCI_CSHIFT,	SCI_DELLINERIGHT);
    AddCommandKey(SCK_INSERT, SCI_NORM,	  SCI_EDITTOGGLEOVERTYPE);
    AddCommandKey(SCK_INSERT, SCI_SHIFT,	SCI_PASTE);
    AddCommandKey(SCK_INSERT, SCI_CTRL,	  SCI_COPY);
    AddCommandKey(SCK_ESCAPE, SCI_NORM,	  SCI_CANCEL);
    AddCommandKey(SCK_BACK,		SCI_NORM, 	SCI_DELETEBACK);
    AddCommandKey(SCK_BACK,		SCI_SHIFT, 	SCI_DELETEBACK);
    AddCommandKey(SCK_BACK,		SCI_CTRL, 	SCI_DELWORDLEFT);
    AddCommandKey(SCK_BACK, 	SCI_ALT,	  SCI_UNDO);
    AddCommandKey(SCK_BACK,		SCI_CSHIFT,	SCI_DELLINELEFT);
    AddCommandKey(Ord('Z'), 	SCI_CTRL,	  SCI_UNDO);
    AddCommandKey(Ord('Y'), 	SCI_CTRL,	  SCI_REDO);
    AddCommandKey(Ord('X'), 	SCI_CTRL,	  SCI_CUT);
    AddCommandKey(Ord('C'), 	SCI_CTRL,	  SCI_COPY);
    AddCommandKey(Ord('V'), 	SCI_CTRL,	  SCI_PASTE);
    AddCommandKey(Ord('A'), 	SCI_CTRL,	  SCI_SELECTALL);
    AddCommandKey(SCK_TAB,		SCI_NORM,	  SCI_TAB);
    AddCommandKey(SCK_TAB,		SCI_SHIFT,	SCI_BACKTAB);
    AddCommandKey(SCK_RETURN, SCI_NORM,	  SCI_NEWLINE);
    AddCommandKey(SCK_RETURN, SCI_SHIFT,	SCI_NEWLINE);
    AddCommandKey(SCK_ADD, 		SCI_CTRL,	  SCI_ZOOMIN);
    AddCommandKey(SCK_SUBTRACT,SCI_CTRL,	SCI_ZOOMOUT);
    AddCommandKey(SCK_DIVIDE,	SCI_CTRL,	  SCI_SETZOOM);
    AddCommandKey(Ord('L'), 	SCI_CTRL,	  SCI_LINECUT);
    AddCommandKey(Ord('L'), 	SCI_CSHIFT,	SCI_LINEDELETE);
    AddCommandKey(Ord('T'), 	SCI_CSHIFT,	SCI_LINECOPY);
    AddCommandKey(Ord('T'), 	SCI_CTRL,	  SCI_LINETRANSPOSE);
    AddCommandKey(Ord('D'), 	SCI_CTRL,	  SCI_LINEDUPLICATE);
    AddCommandKey(Ord('U'), 	SCI_CTRL,	  SCI_LOWERCASE);
    AddCommandKey(Ord('U'), 	SCI_CSHIFT,	SCI_UPPERCASE);
  finally
    EndUpdate;
  end;
end;

procedure TSciKeyCommandCollection.Assign(Source: TPersistent);
begin
  if Source is TSciKeyCommandCollection then
  begin
    fVCStyle:=TSciKeyCommandCollection(Source).VCStyle;
  end;
  inherited Assign(Source);
end;
procedure TSciKeyCommandCollection.SetEditor(Editor: TWinControl);
begin
  FEditor := Editor;
end;

procedure TSciKeyCommandCollection.Update(Item: TCollectionItem);
Var
  i : Integer;
begin
  inherited;
  if (not Assigned(fEditor)) or (not FEditor.HandleAllocated) then Exit;
  if Assigned(Item) then with (Item as TSciKeyCommand) do
      TScintillaMemo(fEditor).AssignCmdKey(ShortCutToSciKey(FShortCut), FCommand)
  else begin
    TScintillaMemo(fEditor).ClearAllCmdKeys;
    for i := 0 to Count - 1 do with (Items[i] as TSciKeyCommand) do
      TScintillaMemo(fEditor).AssignCmdKey(ShortCutToSciKey(FShortCut), FCommand);
  end;
end;


{ TSciKeyCommand }

procedure TSciKeyCommand.Assign(Source: TPersistent);
begin
  if Source is TSciKeyCommand then begin
    FShortCut := TSciKeyCommand(Source).FShortCut;
    FCommand := TSciKeyCommand(Source).FCommand;
  end else
    inherited;
end;

constructor TSciKeyCommand.Create(Collection: TCollection);
begin
  inherited;
  FCommand := SCI_NULL;
end;

function TSciKeyCommand.GetDisplayName: string;
begin
  if ShortCut = 0 then
    Result := sNoKey
  else
    Result := ShortCutToText(FShortCut);
end;

procedure TSciKeyCommand.SetCommand(const Value: Integer);
begin
  FCommand := Value;
  Changed(False);
end;

procedure TSciKeyCommand.SetShortCut(const Value: TShortCut);
begin
  FShortCut := Value;
  Changed(False);
end;

function SaveKeyCommands(Collection : TSciKeyCommandCollection;stream : TStream) : Boolean;
var
  cmd : TSciKeyCommand;
  rec : TSciCommandRec;
  rechdr : TSciCommandRecHeader;
  i,cnt : Integer;
begin
  Result:=False;
  if assigned(stream) then
  begin
    cnt:=Collection.Count;
    rechdr.Signature:=KeyFileHeaderSignature;
    rechdr.NumberOfKeys:=cnt;
    stream.Write(rechdr,SizeOf(rechdr));
    for i:=0 to (cnt-1) do
    begin
      cmd:=TSciKeyCommand(Collection.Items[i]);
      rec.ShortCut:=cmd.ShortCut;
      rec.Command:=cmd.Command;
      stream.Write(rec,SizeOf(rec));
    end;
    Result:=true;
  end;
end;

function SaveKeyCommands(Collection : TSciKeyCommandCollection;const filename : String) : Boolean;
var
  fs : TFileStream;
begin
  try
    fs:=TFileStream.Create(filename,fmCreate);
    Result:=SaveKeyCommands(Collection,fs);
    fs.Free;
  except
    on Exception do
      begin
        Result:=false;
      end;
  end;
end;

function LoadKeyCommands(Collection : TSciKeyCommandCollection;stream : TStream) : Boolean;
var
  i,cnt : Integer;
  rec : TSciCommandRec;
  rechdr : TSciCommandRecHeader;
begin
    Result:=false;
    try
      if assigned(stream) then
      begin
        Collection.Clear;
        stream.Read(rechdr,SizeOf(rechdr));
        if rechdr.Signature=KeyFileHeaderSignature then
        begin
          cnt:=rechdr.NumberOfKeys;
          for i:=0 to (cnt-1) do
          begin
            stream.Read(rec,SizeOf(rec));
            Collection.AddRec(rec);
          end;
          Result:=true;
        end else
          Result:=False;
      end;
    except
      on Exception do
        begin
          Result:=false;
        end;
    end;
end;

function LoadKeyCommands(Collection : TSciKeyCommandCollection;const filename : String) : Boolean;
var
  fs : TFileStream;
begin
  Result:=false;
  if FileExists(filename) then
  begin
    try
      fs:=TFileStream.Create(filename,fmOpenRead);
      Result:=LoadKeyCommands(Collection,fs);
      fs.Free;
    except
      on Exception do
        begin
          Result:=false;
        end;
    end;
  end;
end;

end.
