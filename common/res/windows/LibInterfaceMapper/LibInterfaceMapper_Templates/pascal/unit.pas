unit %file_name%;

interface

uses
  Classes, SysUtils, StrUtils, Contnrs, TNTClasses%AddToUses%;

{$Z4}

type
  va_list = PChar;

%header%
procedure g_free(P: Pointer); cdecl; external 'libglib-2.0-0.dll';

type
  %file_name%Dummy = Integer; // Added in case there is no further type definition.

  //----------------------------------------------------------------------------
  // class definitions

%class%

implementation

function ConvertString(s: PChar): WideString;

begin
  if s = nil then
    Result := ''
  else
    Result := UTF8Decode(s);
end;

  //----------------------------------------------------------------------------
  // class implementations

%classimp%

end.
