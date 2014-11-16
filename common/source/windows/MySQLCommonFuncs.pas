unit MySQLCommonFuncs;

interface

uses
  gnugettext, SysUtils, AuxFuncs, Windows, Forms, TntRegistry, TntClasses, TntSysUtils;

function GetMySQLCommandLineClientPath: WideString;
function GetMySQLAdministratorCmd: WideString;
function GetMySQLQueryBrowserCmd: WideString;

procedure RegisterMySQLApplication(Name: WideString; Version: WideString; InstallPath: WideString);
function ScanRegistryForProductVersion(theRoot: HKEY; RegMainPath: WideString; RegAppName: WideString;
  Key: WideString; KeyType: Integer): WideString;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math;
  
//----------------------------------------------------------------------------------------------------------------------

function GetMySQLCommandLineClientPath: WideString;

var
  InstallDir: WideString;

begin
  InstallDir := ScanRegistryForProductVersion(HKEY_LOCAL_MACHINE, 'SOFTWARE\MySQL AB', 'MySQL Server', 'Location', 0);
  if InstallDir <> '' then
  begin
    Result := InstallDir + 'bin\mysql.exe';
    if not FileExists(Result) then
      Result := '';
  end
  else
  begin
    // No registered server found. Try to find one by looking at usual installation locations.
    if FileExists(GetProgramFilesDir + 'MySQL\bin\mysql.exe') then
      Result := GetProgramFilesDir + 'MySQL\bin\mysql.exe'
    else
      if FileExists('c:\mysql\bin\mysql.exe') then
        Result := 'c:\mysql\bin\mysql.exe'
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetMySQLAdministratorCmd: WideString;

var
  InstallDir: WideString;

begin
  InstallDir := ScanRegistryForProductVersion(HKEY_CURRENT_USER, 'SOFTWARE\MySQL AB', 'MySQL Administrator', 'Location', 0);
  if InstallDir <> '' then
    Result := InstallDir + 'MySQLAdministrator.exe'
  else
    if FileExists(ExtractFilePath(Application.ExeName) + 'MySQLAdministrator.exe') then
      Result := ExtractFilePath(Application.ExeName) + 'MySQLAdministrator.exe'
end;

//----------------------------------------------------------------------------------------------------------------------

function GetMySQLQueryBrowserCmd: WideString;

var
  InstallDir: WideString;

begin
  InstallDir := ScanRegistryForProductVersion(HKEY_CURRENT_USER, 'SOFTWARE\MySQL AB', 'MySQL Query Browser', 'Location', 0);
  if InstallDir <> '' then
    Result := InstallDir + 'MySQLQueryBrowser.exe'
  else
    if FileExists(ExtractFilePath(Application.ExeName) + 'MySQLQueryBrowser.exe') then
      Result := ExtractFilePath(Application.ExeName) + 'MySQLQueryBrowser.exe'
end;

//----------------------------------------------------------------------------------------------------------------------

function GetSubkeyNameByPrefix(theRoot: HKEY; RegMainPath: WideString; Prefix: WideString) : WideString;

// Returns the last subkey under the main path, which begins with the given prefix.
// This is usually used to find a product that is installed under different version numbers and the assumption is
// that newer versions are later added (and thus come later in the enumeration) than older versions.

var
  ParentKey: HKEY;
  I: Integer;
  NumSubKeys: Integer;
  MaxKeySize: Integer;
  KeyName: WideString;
  KeyLen: Cardinal;
  L: Integer;
  RegResult: Integer;
  
begin
  Result := '';
  if RegOpenKeyExW(theRoot, PWideChar(RegMainPath), 0, KEY_READ, ParentKey) = ERROR_SUCCESS then
  begin
    if RegQueryInfoKey(ParentKey, nil, nil, nil, @NumSubKeys, @MaxKeySize, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS then
    begin
      // Add room for terminating 0.
      Inc(MaxKeySize);
      L := Length(Prefix);
      SetLength(KeyName, MaxKeySize);
      for I := 0 to NumSubKeys - 1 do
      begin
        KeyLen := MaxKeySize;
        RegResult := RegEnumKeyExW(ParentKey, I, PWideChar(KeyName), KeyLen, nil, nil, nil, nil);
        if RegResult = ERROR_SUCCESS then
        begin
          if WideSameText(Prefix, Copy(KeyName, 1, Min(L, KeyLen))) then
          begin
            Result := Copy(KeyName, 1, KeyLen);
            // Dont break as we are looking for the last key (in the hope the last key is also that with the
            // highest version number).
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ScanRegistryForProductVersion(theRoot: HKEY; RegMainPath: WideString; RegAppName: WideString; Key: WideString;
  KeyType: Integer): WideString;

var
  FullAppKey: WideString;

begin
  FullAppKey := RegMainPath + '\' + GetSubkeyNameByPrefix(theRoot, RegMainPath, RegAppName);
  Result := ReadFromReg(theRoot, FullAppKey, Key, KeyType, '', False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RegisterMySQLApplication(Name: WideString; Version: WideString; InstallPath: WideString);

var
  RegVersion: WideString;
  KeyName: WideString;
  MainVersion: WideString;
  p, p2: integer;
  
begin
  MainVersion := '';
  p := Pos('.', Version);
  if p > 0 then
  begin
    p2 := Pos('.', Copy(Version, p + 1, Length(Version)));
    if p2 > 0 then
      MainVersion := Copy(Version, 1, p2 + 1);
  end;

  if MainVersion <> '' then
    KeyName := Name + ' ' + MainVersion
  else
    KeyName := Name;

  RegVersion := ReadFromReg(HKEY_CURRENT_USER, '\SOFTWARE\MySQL AB\'+KeyName, 'Version', 0, '');

  if RegVersion <> Version then
  begin
    WriteToReg(HKEY_CURRENT_USER, '\SOFTWARE\MySQL AB\' + KeyName, 'Version', 0, Version);
    WriteToReg(HKEY_CURRENT_USER, '\SOFTWARE\MySQL AB\' + KeyName, 'Location', 0, InstallPath);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
