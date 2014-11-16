unit xgettexttools;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Classes;

{$ifdef MSWINDOWS}
var
  DefCP:integer=CP_ACP;
{$endif}

function ConvertWhitespaceToSpaces (s:widestring):widestring;
function is_identifier(ws:widestring):boolean;
function poscode (substr,line:widestring):integer;
function StreamReadln (s:TStream; var line:widestring):boolean; // Returns false if end of line
function measureindent(s: widestring): word;
function scope2comment(sl: TStrings; name: widestring): widestring;
function RemoveFilenameSpaces (s:widestring):widestring;
function WindowsPathDelim2LinuxPathDelim (path:widestring):widestring;
function StringToWidestring (s:ansistring):Widestring;



implementation

uses
  SysUtils, gnugettext;

function measureindent(s: widestring): word;
// Returns number of spaces this line used to indent this line
begin
  Result := 0;
  while (copy(s, Result + 1, 1) = ' ') do
    inc(Result);
end;

function WindowsPathDelim2LinuxPathDelim (path:widestring):widestring;
var
  i:integer;
begin
  for i:=1 to length(path) do
    if path[i]='\' then
      path[i]:='/';
  Result:=path;
end;

function ConvertWhitespaceToSpaces (s:widestring):widestring;
var
  i:integer;
begin
  for i:=1 to length(s) do
    if s[i]<=#32 then
      s[i]:=' ';
  Result:=s;
end;

function is_identifier(ws:widestring):boolean;
var
  i:integer;
  s:ansistring;
begin
  if ws='' then begin
    Result:=False;
    exit;
  end;

  s:=UTF8Encode(ws);
  Result:=s[1] in ['a'..'z','A'..'Z','_'];
  if Result then begin
    for i:=2 to length(s) do begin
      Result:=Result and (s[1] in ['a'..'z','A'..'Z','_','0'..'9']);
      if not Result then
        break;
    end;
  end;
end;

function poscode (substr,line:widestring):integer;
// Same as pos(), but ignores everything inside strings
var
  i:integer;
  ssl:integer;
  quotemode:boolean;
begin
  ssl:=length(substr);
  quotemode:=False;
  for i:=1 to length(line)-ssl+1 do begin
    if line[i]='''' then
      quotemode:=not quotemode;
    if (not quotemode) and (copy(line,i,ssl)=substr) then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=0;
end;

function RemoveFilenameSpaces (s:widestring):widestring;
var
  i:integer;
begin
  for i:=1 to length(s) do begin
    if s[i]=' ' then s[i]:='_';
  end;
  Result:=s;
end;

function StringToWidestring (s:ansistring):Widestring;
{$ifdef MSWINDOWS}
var
  res:integer;
{$endif}
begin
{$ifdef MSWINDOWS}
  if s='' then
    Result:=''
  else begin
    SetLength (Result, length(s));
    res:=MultiByteToWideChar(DefCP, 0, PChar(s), length(s), PWideChar(Result),length(s));
    if res=0 then
      raise Exception.Create(_('Cannot convert ansistring to widestring with specified codepage.'));
    SetLength (Result, res);
  end;
{$endif}
{$ifdef LINUX}
  Result:=s;
{$endif}
end;

function StreamReadln (s:TStream; var line:widestring):boolean; // Returns false if end of line. reads single-bytes and converts these.
var
  c:char;
  aline:ansistring;
begin
  Assert (s<>nil,_('StreamReadln requires the stream to be not nil.'));
  Result:=True;
  aline:='';
  while true do begin
    if s.Read(c,1)=0 then begin
      Result:=False;
      break;
    end;
    if c=#10 then begin
      break;
    end;
    if c<>#13 then
      aline:=aline+c;
  end;
  line:=StringToWidestring(aline);
end;

function scope2comment(sl: TStrings; name: widestring): widestring;
// Converts a list of strings to a single-line comment separated by dots
var
  i: integer;
begin
  Result := '';
  for i := 0 to sl.Count - 1 do begin
    if Result <> '' then Result := Result + '.';
    Result := Result + sl.Strings[i];
  end;
  if Result <> '' then Result := Result + '.';
  Result := Result + name;
end;


end.
