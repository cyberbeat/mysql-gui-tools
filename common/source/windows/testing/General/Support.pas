unit Support;

interface

function DataPath(const Filename: string): string;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Classes, SysUtils;
  
var
  Pathes: TStringList;

//----------------------------------------------------------------------------------------------------------------------

function DataPath(const Filename: string): string;

// Searches through all defined data pathes and tries to find the given file in them. If found the full path including
// the file name is returned, otherwise an empty string.

var
  I: Integer;

begin
  Result := '';
  for I := 0 to Pathes.Count - 1 do
    if FileExists(Pathes[I] + Filename) then
    begin
      Result := Pathes[I] + Filename;
      Break;
    end;

  // As a fall back we use the apps dir.
  if Result = '' then
    Result := ExtractFilePath(ParamStr(0)) + Filename;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ParseCommandLine;

var
  I: Integer;
  Parameter: string;

begin
  Pathes := TStringList.Create;

  for I := 1 to ParamCount do
  begin
    Parameter := ParamStr(I);
    if Pos('--data=', Parameter) = 1 then
    begin
      // Found a data path parameter. Extract the path and store it.
      Pathes.Add(IncludeTrailingPathDelimiter(Copy(Parameter, 8, MaxInt)));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  ParseCommandLine;
finalization
  Pathes.Free;
end.
