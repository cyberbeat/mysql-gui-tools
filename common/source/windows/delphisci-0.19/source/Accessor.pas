//CE_Desc_Include(helpdescriptions.txt)
{
 Unit    : Accessor
 Purpose : Delphi implementation of Accessor class.
 Created : 05/03/2001
 Author  : Simon Steele (ss@pnotepad.org)
      $Id: Accessor.pas,v 1.4 2004/11/13 04:29:49 hdalis Exp $
 History : 03/12/2001 Fixed indentation (uses tabs).
	         29/09/2004 Initial Release with Delphi Scintilla Interface Components
	                    Not really in use yet.
											hdalis (hdalis@users.sourceforge.net)
           25/02/2005 Changed this to fit the TScintillaBase instead of dealing with handles
                      and such.. Swapped the contents of LevelAt and LineStart functions as it was
                      swapped before..
                      It's a bit more useful for the components now I think..
											hdalis (hdalis@users.sourceforge.net)
                      
}
{$Include SciCommonDef.Inc}
unit Accessor;

interface

uses scisupport,scilexer, classes, windows;

const
    extremePosition = $7FFFFFFF;
    bufferSize = 4000;
    slopSize = (bufferSize div 8);
    wsSpace = 1;
    wsTab = 2;
    wsSpaceTab = 4;
    wsInconsistent = 8;
(*

void WindowAccessor::StartSegment(unsigned int pos) {
void WindowAccessor::ColourTo(unsigned int pos, int chAttr) {
*)

type
    PAccessor = ^TAccessor;
    TfnIsCommentLeader = function(styler : PAccessor; pos, len : Integer) : Boolean;
    TAccessor = class(TObject)
    private
        function Get(Position: Integer): Char;
        function InternalIsLeadByte(ch : Char) : Boolean;
    protected
        id : TScintillaBase;
        buf : array[0..bufferSize] of Char;
        styleBuf : array[0..bufferSize] of Char;
        startPos : Integer;
        endPos : Integer;
        codePage : Integer;
        lenDoc : Integer;
        validLen : Integer;
        chFlags : Char;
        chWhile : Char;
        startSeg : Cardinal;

        procedure Fill(Position : Integer);
	public
        function StyleAt(Position : Integer) : Char;
        function GetLine(Position : Integer) : Integer;
        function LineStart(line : Integer) : Integer;
        function LevelAt(line : Integer) : Integer;
        property Chars[Position: Integer]: Char read Get; default;
        // Safe version of [] returns default value for invalid position.
        function SafeGetCharAt(Position : Integer; chDefault : Char = ' ') : Char;
        procedure SetCodePage(codePage_ : Integer);
        function Length : Integer;
        function GetLineState(Line : Integer) : Integer;
        procedure SetLineState(Line, State : Integer);
        procedure StartAt(start : Cardinal; chMask : Char = #31);
        procedure StartSegment(Pos : Cardinal);
        procedure ColourTo(Pos : Cardinal; chAttr : Integer);
        procedure Flush;
        procedure SetLevel(Line, Level : Integer);
        function IndentAmount(Line : Integer; var Flags : Integer; pfnIsCommentLeader : TfnIsCommentLeader) : Integer;
        function IsLeadByte(ch : Char) : Boolean;
        procedure SetFlags(chFlags_, chWhile_ : Char);
        function GetStartSegment : Integer;

        constructor Create(windowID : TScintillaBase);
        destructor Destroy; override;
  end;

function isspacechar(ch : Char) : Boolean;
function tolower(ch : Char) : Char;
function isspace(ch : Char) : Boolean;
function isdigit(ch : Char) : Boolean;
function isoperator(ch : Char) : Boolean;
function isascii(ch : Char) : Boolean;
function isalnum(ch : Char) : Boolean;
function iswordchar(ch : Char) : Boolean;
function iswordstart(ch : Char) : Boolean;

implementation

function isascii(ch : Char) : Boolean;
begin
	Result := Ord(ch) < $80;
end;

function isalnum(ch : Char) : Boolean;
var i : Integer;
begin
	i := Ord(ch);
	Result := (
         ((i >= 48) and (i <= 57)) or
         ((i >= 65) and (i <= 90)) or
         ((i >= 97) and (i <= 122))
         );
end;

function isoperator(ch : Char) : Boolean;
begin
	if (isascii(ch) and isalnum(ch)) then
	begin
    	Result := false;
    	Exit;
	end;
	// '.' left out as it is used to make up numbers
	Result := ((ch = '%') or (ch = '^') or (ch = '&') or (ch = '*') or
	        (ch = '(') or (ch = ')') or (ch = '-') or (ch = '+') or
	        (ch = '=') or (ch = '|') or (ch = '{') or (ch = '}') or
	        (ch = '[') or (ch = ']') or (ch = ':') or (ch = ';') or
	        (ch = '<') or (ch = '>') or (ch = ',') or (ch = '/') or
	        (ch = '?') or (ch = '!') or (ch = '.') or (ch = '~'));
end;

function isspacechar(ch : Char) : Boolean;
begin
	Result := ((ch = #09) or (ch = #13) or (ch = #32));
end;

function isspace(ch : Char) : Boolean;
begin
	Result := ((ch = #09) or (ch = #10) or (ch = #13) or (ch = #32));
end;

function tolower(ch : Char) : Char;
var i : Integer;
begin
	i := Ord(ch);
	if (i >= 65) and (i <= 90) then
		Result := Chr(i + 32)
	else
		Result := ch;
end;

function isdigit(ch : Char) : Boolean;
var i : Integer;
begin
	i := Ord(ch);
	Result := ( (i >= 48) and (i <= 57));
end;

function iswordchar(ch : Char) : Boolean;
begin
	Result := isascii(ch) and (isalnum(ch) or (ch = '.') or (ch = '_'));
end;

function iswordstart(ch : Char) : Boolean;
begin
	Result := isascii(ch) and (isalnum(ch) or (ch = '_'));
end;

{ TAccessor }

procedure TAccessor.ColourTo(Pos: Cardinal; chAttr: Integer);
var i : Cardinal;
begin
    if Pos <> (StartSeg - 1) then
    begin
		if Pos < startSeg then
			OutputDebugString('Bad Colour Positions'); // %d - %d : startSeg, pos

		if Cardinal(validLen) + (Pos - startSeg + 1) >= bufferSize then
			Flush;

		if Cardinal(validLen) + (Pos - startSeg + 1) >= bufferSize then
		begin
			// Too big for buffer size so send directly
      id.SetStyling(Pos - startSeg + 1, chAttr);
		end else
		begin
			if (Char(chAttr) <> chWhile) then
				chFlags := #0;
			chAttr := chAttr or Ord(chFlags);
			for i := startSeg to Pos do
			begin
				styleBuf[validLen] := Char(chAttr);
				Inc(validLen);
			end;
		end;
	end;
	startSeg := Pos + 1;
end;

constructor TAccessor.Create(windowID: TScintillaBase);
begin
    id := windowID;
    lenDoc := -1;
    startPos := extremePosition;
    endPos := 0;
    codePage := 0;
    validLen := 0;
    chFlags := #0;
    chWhile := #0;
  //TODO Properties...
end;

destructor TAccessor.Destroy;
begin
  inherited;
end;

procedure TAccessor.Fill(Position: Integer);
var
	tr : TTextRange;
begin
	if lenDoc = -1 then
		lenDoc := id.GetTextLength;
	startPos := Position - slopSize;
	if (startPos + bufferSize > lenDoc) then
		startPos := lenDoc - bufferSize;
	if (startPos < 0) then
		startPos := 0;
	endPos := startPos + bufferSize;
	if (endPos > lenDoc) then
		endPos := lenDoc;

	tr.chrg.cpMin := startPos;
	tr.chrg.cpMax := endPos;
	tr.lpstrText := buf;

	id.GetTextRange(@tr);
end;

procedure TAccessor.Flush;
begin
    startPos := extremePosition;
    lenDoc := -1;
    if (validLen > 0) then
    begin
    id.SetStylingEx(validLen, PChar(@styleBuf));
		validLen := 0;
	end;
end;

function TAccessor.Get(Position: Integer): Char;
begin
	if (Position < startPos) or (Position >= endPos) then
		Fill(Position);
	Result := buf[Position - startPos];
end;

function TAccessor.GetLine(Position: Integer): Integer;
begin
	Result := id.LineFromPosition(Position);
end;

function TAccessor.GetLineState(Line: Integer): Integer;
begin
	Result := id.GetLineState(Line);
end;

function TAccessor.Length: Integer;
begin
	Result := id.GetTextLength;
end;

function TAccessor.LevelAt(line: Integer): Integer;
begin
  Result := id.GetFoldLevel(line);
end;

function TAccessor.LineStart(line: Integer): Integer;
begin
 	Result := id.PositionFromLine(line);
end;

function TAccessor.SafeGetCharAt(Position: Integer; chDefault: Char): Char;
begin
	if (Position < startPos) or (Position >= endPos) then
	begin
		Fill(Position);
		if (Position < startPos) or (Position >= endPos) then
		begin
			Result := chDefault;
			Exit;
    	end;
  	end;
	Result := buf[Position - startPos];
end;

procedure TAccessor.SetCodePage(codePage_: Integer);
begin
	codePage := codePage_;
end;

procedure TAccessor.SetLevel(Line, Level: Integer);
begin
	id.SetFoldLevel(Line, Level);
end;

procedure TAccessor.SetLineState(Line, State: Integer);
begin
	id.SetLineState(Line, State);
end;

procedure TAccessor.StartAt(start: Cardinal; chMask: Char);
begin
  id.StartStyling(start, Ord(chMask));
end;

procedure TAccessor.StartSegment(Pos: Cardinal);
begin
	startSeg := Pos;
end;

function TAccessor.StyleAt(Position: Integer): Char;
begin
	Result := Char( id.GetStyleAt(Position) );
end;

function TAccessor.IndentAmount(Line : Integer; var Flags : Integer; pfnIsCommentLeader : TfnIsCommentLeader) : Integer;
var
	iend : Integer;
	spaceFlags : Integer;
	Pos : Integer;
	ch : Char;
	chPrev : Char;
	indent : Integer;
	inPrevPrefix : Boolean;
	posPrev : Integer;
begin
    iend := Length;
    spaceFlags := 0;

	// Determines the indentation level of the current line and also checks for consistent
	// indentation compared to the previous line.
	// Indentation is judged consistent when the indentation whitespace of each line lines
	// the same or the indentation of one line is a prefix of the other.

	pos := LineStart(line);
	ch := Chars[Pos];
	indent := 0;
	inPrevPrefix := line > 0;
	if inPrevPrefix then
    	posPrev := LineStart(line-1)
    else
    	posPrev := 0;

	while ( ((ch = ' ') or (ch = #09)) and (pos < iend) ) do
	begin
		if (inPrevPrefix) then
		begin
			chPrev := Chars[posPrev];
			Inc(posPrev);
			if ((chPrev = ' ') or (chPrev = #09)) then
			begin
				if (chPrev <> ch) then
					spaceFlags := spaceFlags or wsInconsistent;
			end else
			begin
				inPrevPrefix := false;
			end;
		end;
		if (ch = ' ') then
		begin
			spaceFlags := spaceFlags or wsSpace;
			Inc(indent);
		end else
		begin	// Tab
			spaceFlags := spaceFlags or wsTab;
			if ((spaceFlags and wsSpace) = wsSpace) then
				spaceFlags := spaceFlags or wsSpaceTab;
			indent := (indent div 8 + 1) * 8;
		end;
		Inc(Pos);
		ch := Chars[Pos];
	end;

	Flags := spaceFlags;
	indent := indent + SC_FOLDLEVELBASE;
	// if completely empty line or the start of a comment...
	if ( isspace(ch) or ((@pfnIsCommentLeader <> nil) and pfnIsCommentLeader(@self, pos, iend-pos) ) ) then
	begin
		result :=  indent or SC_FOLDLEVELWHITEFLAG;
	end else
		result := indent;
end;

function TAccessor.IsLeadByte(ch: Char): Boolean;
begin
	Result := (codePage > 0) and InternalIsLeadByte(ch);
end;

function TAccessor.InternalIsLeadByte(ch: Char): Boolean;
begin
	if (SC_CP_UTF8 = codePage) then
		// For lexing, all characters >= 0x80 are treated the
		// same so none is considered a lead byte.
		Result :=  false
	else
		Result := IsDBCSLeadByteEx(codePage, Byte(ch));
end;

function TAccessor.GetStartSegment: Integer;
begin
	Result := startSeg;
end;

procedure TAccessor.SetFlags(chFlags_, chWhile_: Char);
begin
	chFlags := chFlags_;
	chWhile := chWhile_;
end;

end.
