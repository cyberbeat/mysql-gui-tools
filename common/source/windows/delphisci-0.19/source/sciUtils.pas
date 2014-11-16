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
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
	Author : hdalis (Jan Martin Pettersen)
	Created: 02/12/2004
			$Id:  $
	Purpose: Assorted functions used by the components

	History: 02/12/2004 First release
           05/01/2005 Added
           15/05/2005 Renamed StrToken to WordBefore, and ReplaceChar to ReplaceAChar because the old names clashed
                      with JVCL.
                      Added ToBool, and ToIntAndCheck.
                      Removed all references to ValueExtract, as it was unneccesary.                      
}
{$Include SciCommonDef.Inc}
unit sciUtils;
interface
uses Graphics,Classes,SysUtils;
{$Ifndef COMPILER6_UP}
{This class, and functions is a partial support for BCB5 for the CaseSensitive property etc.. It might work, and it might not..}
type
  TMyStringList=class(TStringList)
    private
    FCaseSensitive : Boolean;
    procedure SetCaseSensitive(Value : Boolean);
    public
      function Find(const S: string; var Index: Integer): Boolean; override;
      function IndexOf(const S: string): Integer;override;
      function CompareStrings(const S1, S2: string): Integer;
    published
      property CaseSensitive : Boolean read FCaseSensitive write SetCaseSensitive;
  end;

procedure SplitStrings(const src : String;lst : TStrings);
function MergeStrings(lst : TStrings) : String;
{$Endif}

{Convert a string to the corresponding TColor, and if s='' then returns defcolor.}
function ColourFromString(const s : String;const defcolor : TColor) : TColor;
{Converts a TColor to a string readable by ColourFromString.}
function ColourToString(const clr : TColor) : String;

{ Converts True/Yes or 1 to True, else returns false}
function StringToBool(const s : String) : Boolean;

{Converts a string to boolean via the above function, or if the string is empty, returns the 'defaultvalue'}
function ToBool(const s : String;const defaultvalue : Boolean=False) : Boolean;

{Converts a bool value to the text True or False}
function BoolToString(const value : Boolean) : String;

{Converts an Integer to string.}
function ToInt(const value : String) : Integer;
{Converts a string to an Integer}
function ToStr(const value : Integer) : String;

{Returns the string 'value' as a integer, and if the string also contains
 nonnumeric, then it strips them before converting.}
function ToIntAndCheck(const value : String) : Integer;

{Compares s1 to s2, if ignorecase is true then the comparing is caseless.
 If maxlen<>-1 then only maxlen chars are compared.}
function CompStr(const s1,s2 : String;ignorecase : Boolean=True;maxlen : Integer=-1) : Integer;

{Returns the string before the separator 'separator', and returns the rest of the
 string in 'S'.}
function WordBefore(var S: AnsiString; const Separator: AnsiChar): AnsiString;

{Replaces all chars 'Source' with the char 'Replace'.}
function ReplaceAChar(const S: AnsiString; const Source, Replace: Char): AnsiString;

{This is a wrapper for TStrings.ValueFromIndex found in Delphi 7 and newer to allow older versions
 to get the same functionality}
function ValueFromIndex(lst : TStrings;index : Integer) : String;

{If two or more of any of the characters in 'whitespacechars' are adjacent then
 all but one of them is removed. If both space (#32) and Tab is in the 'whitespacechars' then
 all Tabs are converted to a space before returning the result.}
function AllButOne(const s : String;const whitespacechars : TSysCharSet) : String;

{Converts a string to sentence style. The character after one of the chars in
 'sentenceendings' are uppercased, the rest is lowercased.}
function SentenceString(const s : String;sentenceendings : TSysCharSet) : String;

{Inverts the charactercase of a string}
function InvertCase(const s : String) : String;

{Unslashes C style strings. Doesn't support octals however}
function Unslash(const s : String) : String;

{Slashes C style strings. Doesn't support octals however}
function Slash(const s : String) : String;

{Returns True if the char 'ch' is a word character. old name: iswordcharforsel}
function IsWordChar(const ch : LongInt) : boolean;

{Returns True if the char 'ch' is a filename character. old name: isfilenamecharforsel}
function IsFilenameChar(const ch : LongInt) : boolean;

{Returns true if the char 'ch' is a whitespace. old name: isaspace}
function IsSpace(const ch : LongInt) : boolean;

{Returns the position of the first char 'C', starting from 'StartPos', or 0 if it isn't found.}
function CharPos(const S: AnsiString; const C: AnsiChar; const StartPos: LongInt=1): LongInt;

{Extracts a Integer from a hex byte}
function IntFromHexByte(const hexByte : PChar) : Integer;

{Extracts a Integer from a hex digit}
function IntFromHexDigit(const ch : Char) : Integer;

{Returns the index of the matching string if 's' is found in the list 'lst', compared 'casesensitive'
 otherwise, returns -1.}
function FindStrInList(const s : String;lst : TStrings;const casesensitive : Boolean=true) : Integer;

{Returns true if the word 'searchfor' is within the string 's' where the items are separated by 'separator'}
function IsWordInString(const s : String;const searchfor : String;IgnoreCase : Boolean=True;const separator : AnsiChar=';') : Boolean;

{Returns the string 's' except the word 'exceptwhat'. Items are separated by 'separator'.}
function BuildWordlistExcept(const s : String;const exceptwhat : String;IgnoreCase : Boolean=True;const separator : AnsiChar=';') : String;

{Processes the commandlinelike 'cmd', optionally starting with the first arg.
Handles "arg arg" as one argument. Returns the arguments separated by #13.}
function ProcessArgs(cmd : PChar;startwithfirst : Boolean=false) : String;

implementation
uses SciResLang,Windows;

{$Ifdef NOBUILTINCLR}
	Type
		{ Auxiliary structure to support TColor manipulation }
		TColorRec = packed record
			case Integer of
				0: (Value: LongInt);
				1: (Red, Green, Blue: Byte);
				2: (R, G, B, Flag: Byte);
				3: (Index: Word); // GetSysColor, PaletteIndex
		end;

	//Sets a TColor from RGB.
	function internSetRGBValue(const Red, Green, Blue: Byte): TColor;
	begin
		TColorRec(Result).Red := Red;
		TColorRec(Result).Green := Green;
		TColorRec(Result).Blue := Blue;
		TColorRec(Result).Flag := 0;
	end;
{$Endif}
{Extracts a Integer from a hex digit}
function IntFromHexDigit(const ch : Char) : Integer;
begin
  if ((ch>='0') and (ch<='9')) then
  begin
    Result :=Integer(ch)-Integer('0');
  end else if ((ch >= 'A') and (ch <= 'F')) then
  begin
    Result :=Integer(ch)-Integer('A')+10;
  end else if ((ch >= 'a') and (ch <= 'f')) then
  begin
    Result :=Integer(ch)-Integer('a')+10;
  end	else
  begin
    Result:=0;
  end;
end;

{Extracts a Integer from a hex byte}
function IntFromHexByte(const hexByte : PChar) : Integer;
begin
	Result:=IntFromHexDigit(hexByte[0]) * 16 + IntFromHexDigit(hexByte[1]);
end;

{This function exists in the TStrings class in Delphi7 but not in CBuilder6
 therefore a wrapper/substitute for it.}
function ValueFromIndex(lst : TStrings;index : Integer) : String;
{$Ifndef VER150}
var
name : String;
value : String;
begin
	name:=lst.Names[index];
	value:=lst.Values[name];
	Result:=value;
{$Else}
begin
  Result:=lst.ValueFromIndex[index];
{$Endif}
end;

//Convert a string to the corresponding TColor, and if s='' then returns defcolor.
function ColourFromString(const s : String;const defcolor : TColor) : TColor;
var
{$Ifdef NOBUILTINCLR}
	r: Integer;
	g: Integer;
	b: Integer;
{$Else}
	tmp : array [0..10] of Char ;
{$Endif}
begin
	if (Length(s)>0) then
	begin
{$Ifdef NOBUILTINCLR}
		//tmp:=PChar(s);
		r := IntFromHexByte(PChar(s) + 1);
		g := IntFromHexByte(PChar(s) + 3);
		b := IntFromHexByte(PChar(s) + 5);
		Result := internSetRGBValue(r,g,b);
{$Else}

    if s[1]='#' then  //Rotate htmlcolorcode to resemble delphi colorcodes
    begin
      tmp[0]:='$';
      tmp[1]:='0';
      tmp[2]:='0';
      tmp[3]:=s[6];
      tmp[4]:=s[7];
      tmp[5]:=s[4];
      tmp[6]:=s[5];
      tmp[7]:=s[2];
      tmp[8]:=s[3];
      tmp[9]:=#0;
		  Result:=StringToColor(tmp);
      Exit;
    end;
		Result:=StringToColor(s);
{$Endif}
	end else
	begin
		Result:=defcolor;
	end;
end;

//Converts a TColor to a string readable by ColourFromString.
function ColourToString(const clr :TColor) : String;
{$Ifndef NOBUILTINCLR}
var
	tmp : String;
	tmp2 : array [0..10] of Char;
{$Endif}
begin
{$Ifdef NOBUILTINCLR}
	Result:=Format('#%2.2x%2.2x%2.2x',[TColorRec(clr).Red,TColorRec(clr).Green,TColorRec(clr).Blue]);
{$Else}
	tmp:=ColorToString(clr);
	if (tmp[1]='$') then  //Rotate string to resemble html colorcodes
	begin
		Delete(tmp,1,3);
		tmp2[0]:='#';
		tmp2[1]:=tmp[5];
		tmp2[2]:=tmp[6];
		tmp2[3]:=tmp[3];
		tmp2[4]:=tmp[4];
		tmp2[5]:=tmp[1];
		tmp2[6]:=tmp[2];
		tmp2[7]:=#0;
		Result:=tmp2;
		Exit;
	end;

	Result:=tmp;
{$Endif}
end;

function ToInt(const value : String) : Integer;
begin
  Result:=StrToIntDef(value,0);
end;

function ToStr(const value : Integer) : String;
begin
  Result:=IntToStr(value);
end;


//Replaces the all 'source' char with 'replace' char, and returns the result.
function ReplaceAChar(const S: AnsiString; const Source, Replace: Char): AnsiString;
var
  I: Integer;
begin
  Result := S;
  for I := 1 to Length(S) do
    if Result[I] = Source then
      Result[I] := Replace;
end;


//Extracts the string from start to 'separator'. Returns that, and returns the rest of the string
//in 's'
function WordBefore(var S: AnsiString; const Separator: AnsiChar): AnsiString;
var
  I: Integer;
begin
  I := Pos(Separator, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end else
	begin
    Result := S;
    S := '';
  end;
end;

function IsWordInString(const s : String;const searchfor : String;IgnoreCase : Boolean;const separator : AnsiChar) : Boolean;
var
  tmp : String;
  match : String;
begin
  tmp:=s;
  Result:=False;
  repeat
    match:=WordBefore(tmp,separator);
    if CompStr(match,searchfor,IgnoreCase)=0 then
    begin
      Result:=True;
      Exit;
    end;
  until (tmp='');
end;

function BuildWordlistExcept(const s : String;const exceptwhat : String;IgnoreCase : Boolean;const separator : AnsiChar) : String;
var
  tmp : String;
  match : String;
  newlist : String;
  isfirst : Boolean;
begin
  tmp:=s;
  isfirst:=True;
  repeat
    match:=WordBefore(tmp,separator);
    if CompStr(match,exceptwhat,IgnoreCase)<>0 then
    begin
      if isfirst then
      begin
        newlist:=match;
        isfirst:=False;
      end
      else
        newlist:=newlist+separator+match;
    end;
  until (tmp='');
  Result:=newlist;
end;

function CompStr(const s1,s2 : String;ignorecase : Boolean;maxlen : Integer) : Integer;
begin
  if maxlen=-1 then
  begin
    if ignorecase then
      Result:=StrIComp(PChar(s1),PChar(s2))
    else
      Result:=StrComp(PChar(s1),PChar(s2));
  end else
  begin
    if ignorecase then
      Result:=StrLIComp(PChar(s1),PChar(s2),maxlen)
    else
      Result:=StrLComp(PChar(s1),PChar(s2),maxlen);
  end;
end;

function StringToBool(const s : String) : Boolean;
begin
  if (CompStr(s,sTrue)=0) or (CompStr(s,'1')=0) or (CompStr(s,sYes)=0) then
    Result:=True
  else
    Result:=False;
end;

function BoolToString(const value : Boolean) : String;
begin
	if value=true then Result:=sTrue
	else Result:=sFalse;
end;

function AllButOne(const s : String;const whitespacechars : TSysCharSet) : String;
var
  i,cnt : Integer;
  tmp : String;
  lastwasspace : Boolean;
  c : Char;
  ps : Integer;
begin
  if s='' then
  begin
    Result:=s;
    Exit;
  end;
  tmp:=s;
  lastwasspace:=True;
  cnt:=Length(tmp);
  for i:=cnt downto 1 do
  begin
    c:=tmp[i];
    if c in whitespacechars then
    begin
      if (lastwasspace) then Delete(tmp,i,1);
      lastwasspace:=True;
    end else
      lastwasspace:=False;
  end;
  if (#9 in whitespacechars) and (#32 in whitespacechars) then
  begin
    ps:=Pos(#9,tmp);
    while ps>0 do
    begin
      tmp[ps]:=#32;
      ps:=Pos(#9,tmp);
    end;
  end;
  Result:=tmp;
end;

function SentenceString(const s : String;sentenceendings : TSysCharSet) : String;
var
  tmp : String;
  cnt,i : Integer;
  c : Char;
  lastwasending : Boolean;
begin
  if s='' then
  begin
    Result:=s;
    Exit;
  end;
  tmp:=s;
  cnt:=Length(tmp);
  lastwasending:=True;
  for i:=1 to cnt do
  begin
    c:=tmp[i];
    if c in sentenceendings then lastwasending:=True
    else
    begin
      if c in [#32,#9] then
      begin
        Continue;
      end;
      if lastwasending=True then
      begin
        tmp[i]:=AnsiUpperCase(c)[1];
        lastwasending:=False;
      end else
      begin
        tmp[i]:=AnsiLowerCase(tmp[i])[1];
        lastwasending:=False;
      end;
    end;
  end;
  Result:=tmp;
end;

function InvertCase(const s : String) : String;
var
  tmp : String;
  cnt,i : Integer;
  c : Char;
begin
  if s='' then
  begin
    Result:=s;
    Exit;
  end;
  tmp:=s;
  cnt:=Length(tmp);
  for i:=1 to cnt do
  begin
    c:=tmp[i];
    case c of
      'a'..'z','æ','ø','å': tmp[i]:=AnsiUpperCase(c)[1];
      'A'..'Z','Æ','Ø','Å': tmp[i]:=AnsiLowerCase(c)[1];
    end;
  end;
  Result:=tmp;
end;

function Slash(const s : String) : String;
var
  I: Integer;
  len : Integer;
begin
  Result := '';
  len:=Length(S);
  for I := 1 to len do
  begin
    case S[I] of
      #8:
        Result := Result + '\b';
      #7:
        Result := Result + '\a';
      #13:
        begin
          if not ((I<len) and (S[I+1]=#10)) then //Replaces the sequence #13#10 with only #10 because the AbbrevManager handles '\n' as the current eolmode.
            Result := Result + '\r';
        end;
      #15:
        Result := Result + '\f';
      #10:
        Result := Result + '\n';
      #9:
        Result := Result + '\t';
      #11:
        Result := Result + '\v';
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
    else
      if S[I] < #32 then
        Result := Result + Format('\x%.2x',[Integer(S[I])])
      else
        Result := Result + S[I];
    end;
  end;
end;

function Unslash(const s : String) : String;
const
  hexchars = ['0'..'9','a'..'f','A'..'F'];
var
  Len,I : Integer;
  hbuf : array[0..2] of Char;
begin
  Result := '';
  hbuf[2]:=#0;

  I := 1;
  Len := Length(S);
  while I <= Len do
  begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else
    begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + #7;
        'b':
          Result := Result + #8;
        'f':
          Result := Result + #15;
        'n':
          Result := Result + #10;
        'r':
          Result := Result + #13;
        't':
          Result := Result + #9;
        'v':
          Result := Result + #11;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped

        'x': begin
               if I<(Len-1) then
               begin
                 if S[I+1] in hexchars then
                 begin
                   hbuf[0]:=S[I+1];
                   hbuf[1]:=S[I+2];
                   Result:=Result+Chr(IntFromHexByte(hbuf));
                   Inc(i,2);
                 end;
               end else
               begin
                 Result := Result + '\x';
               end;
             end;
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;

end;

function CharPos(const S: AnsiString; const C: AnsiChar; const StartPos: LongInt): LongInt;
var
  P: PAnsiChar;
begin
  Result := 0;
  if (StartPos > 0) and (StartPos <= Length(S)) then
  begin
    P := PAnsiChar(S);
    Result := StartPos - 1;
    Inc(P, Result);
    while P^ <> #0 do
    begin
      Inc(Result);
      if P^ = C then
        Break;
      Inc(P);
    end;
    if P^ = #0 then
      Result := 0;
  end;

end;

function IsSpace(const ch : LongInt) : boolean;
begin
  Result := (Char(ch) in [#9,#10,#13,#32]);
end;

function IsWordChar(const ch : LongInt) : boolean;
begin
  Result := Char(ch) in ['a'..'z','A'..'Z','0'..'9','_'];
	//Result := not (Char(ch) in [#9,#10,#13,#32,'!','"','#','$','%','&','''','(',',',')','*','+',',','-','.','/',':',';','<','=','>','?','@','[','\',']','^','`','{','|','}','~',']']);
end;

function IsFilenameChar(const ch : LongInt) : boolean;
begin
	result := not (Char(ch) in [#9,#10,#13,#32,'"','*',';','<','>','?','^','|',',']);
end;

function  ProcessArgs(cmd : PChar;startwithfirst : Boolean) : String;
var
  tmparg,args : String;
  startArg : PChar;
  endArg : PChar;
  argpos : Integer;
begin
  startArg:=cmd;
  argpos:=0;
  while (startArg^<>#0) do
  begin
    while IsSpace(Integer(startArg^)) do Inc(startArg);
    endArg:=startArg;
    if startArg^='"' then
    begin
      Inc(startArg);
      endArg:=startArg;
      while ((endArg^<>#0) and (endArg^<>'"')) do Inc(endArg);
    end else
    begin
      while (endArg^<>#0) and (IsSpace(Integer(endArg^))=False) do Inc(endArg);
    end;
    tmparg:=Copy(startArg,1,endArg-startArg);
    if (startwithfirst=true) or (argpos>0) then
      if args<>'' then
        args:=args+#13+tmparg
      else
        args:=tmparg;
    startArg:=endArg;
    Inc(argpos);
    if (startArg^<>#0) then Inc(startArg);
  end;
  Result:=args;
end;

function FindStrInList(const s : String;lst : TStrings;const casesensitive : Boolean) : Integer;
var
  cnt,i : Integer;
begin
  Result:=-1;
  if(casesensitive) then
  begin
    cnt:=lst.Count;
    if(cnt>0) then
    begin
      for i:=0 to (cnt-1) do
      begin
        if AnsiCompareStr(s,lst.Strings[i])=0 then
        begin
          Result:=i;
          Exit;
        end;
      end;
    end;
  end else
  begin
    Result:=lst.IndexOf(s);
  end;
end;

function ToBool(const s : String;const defaultvalue : Boolean) : Boolean;
begin
  if s='' then
  begin
    Result:=defaultvalue;
    Exit;
  end else
    Result:=StringToBool(s);
end;

function ToIntAndCheck(const value : String) : Integer;
var
  cnt,i,ps : Integer;
  ishex : Boolean;
begin
  cnt:=Length(value);
  if cnt>0 then
    ishex:=(value[1]='$')
  else
  begin
    Result:=0;
    Exit;
  end;
  ps:=-1;
  for i:=1 to cnt do
  begin
    if ishex=True then
    begin
      if not (value[i] in ['$','0'..'9','a'..'f','A'..'F']) then
      begin
        ps:=i;
        break;
      end;
    end else
    begin
      if not (value[i] in ['0'..'9']) then
      begin
        ps:=i;
        break;
      end;
    end;
  end;
  if (ps<>-1) then
  begin
    Result:=StrToIntDef(Copy(value,1,ps-1),0);
  end else
  Result:=StrToIntDef(value,0);
end;


{$IFNDEF COMPILER6_UP}
function MergeStrings(lst : TStrings) : String;
var
i,cnt : Integer;
s : String;
P : PChar;
begin
  cnt:=lst.Count;
  Result:='';
  if cnt=1 then
  begin
    Result:=lst.Strings[0];
    Exit;
  end;
  for i:=0 to (cnt-1) do
  begin
    s:=lst.Strings[i];
    P:=PChar(s);
      while not (P^ in [#0..' ']) do
        Inc(P);
      Result := Result + S + ' ';
  end;
  System.Delete(Result, Length(Result), 1);
end;

procedure SplitStrings(const src : String;lst : TStrings);
var
  P, Start: PChar;
  S: string;
begin
  lst.Clear;
  P := Pointer(src);
  if P <> nil then
  begin
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, #10, #13]) do Inc(P);
      SetString(S, Start, P - Start);
      lst.Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;
end;

function TMyStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(Strings[I], S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TMyStringList.IndexOf(const S: string): Integer;
var
cnt : Integer;
begin
  if Sorted=True then
  begin
    if not Find(S, Result) then Result := -1;
  end else
  begin
    cnt:=Count;
    for Result := 0 to (cnt - 1) do
      if CompareStrings(Strings[Result], S) = 0 then Exit;
    Result := -1;
  end;
end;

function TMyStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;
procedure TMyStringList.SetCaseSensitive(Value : Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted=True then Sort;
  end;
end;
{$ENDIF}
end.
