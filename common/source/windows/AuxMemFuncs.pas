unit AuxMemFuncs;

interface

uses
  SysUtils;

function malloc(size: Integer): Pointer; cdecl; external 'msvcrt.dll';
function realloc(p: pointer; size: Integer): Pointer; cdecl; external 'msvcrt.dll';
procedure free(p: Pointer); cdecl; external 'msvcrt.dll';

function strdup(s: string): PChar;

implementation

function strdup(s: string): PChar;
var p: PChar;
begin
  p:=malloc(Length(s)+1);

  StrMove(p, PChar(s), Length(s)+1);

  Result:=p;
end;

end.
