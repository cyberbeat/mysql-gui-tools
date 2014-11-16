program SetVersion;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Registry,
  StrUtils,
  Windows, Messages,
  RegExpr in '..\LibInterfaceMapper\RegExpr.pas';

procedure WritePascalIncludeFile(version: string; build_level: string;
  pascalIncludeFile: string);
var f: TextFile;
begin
  AssignFile(f, pascalIncludeFile);
  Rewrite(f);
  WriteLn(f, 'const product_version='''+version+''';');
  WriteLn(f, 'const product_build_level='''+build_level+''';');
  CloseFile(f);
end;

function SetGlobalEnvironment(const Name, Value: string;
  const User: Boolean = True): Boolean;
resourcestring
  REG_MACHINE_LOCATION = 'System\CurrentControlSet\Control\Session Manager\Environment';
  REG_USER_LOCATION = 'Environment';
begin
  with TRegistry.Create do
    try
      if User then { User Environment Variable }
        Result := OpenKey(REG_USER_LOCATION, True)
      else { System Environment Variable }
      begin
        RootKey := HKEY_LOCAL_MACHINE;
        Result  := OpenKey(REG_MACHINE_LOCATION, True);
      end;
      if Result then
      begin
        WriteString(Name, Value); { Write Registry for Global Environment }
        { Update Current Process Environment Variable }
        SetEnvironmentVariable(PChar(Name), PChar(Value));
        { Send Message To All Top Window for Refresh }
        SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
      end;
    finally
      Free;
    end;
end;

function GetNewGUID: string;
var Guid: TGUID;
  s: string;
begin
  CreateGUID(Guid);

  try
    s:=GUIDToString(Guid);
    Result:=Copy(s, 2, Length(s)-2);
  except
    WriteLn('Error: Cannot get a new GUID.');
  end;
end;

procedure UpdateWIXFile(version: string; build_level: string;
  WIXFileName: string);
var WixFile: TStringList;
  RegExpr: TRegExpr;
  version_main, version_min, version_max: String;
begin
  if(Not(FileExists(WIXFileName)))then
  begin
    WriteLn('The file '+WIXFileName+' cannot be found');
    Exit;
  end;

  WixFile:=TStringList.Create;
  try
    RegExpr:=TRegExpr.Create;
    try
      WixFile.LoadFromFile(WIXFileName);

      RegExpr.Expression:='(\d+)\.(\d+)\.(\d+)';
      if(RegExpr.Exec(Version))then
      begin
        version_main:=RegExpr.Match[1]+'.'+RegExpr.Match[2];
        version_min:=version_main+'.0';
        version_max:=version_main+'.999';
      end;

      //RegExpr.Expression:=' Version="([0-9\.]{0,})"';
      //WixFile.Text:=RegExpr.Replace(WixFile.Text,
      //  ' Version="'+version+'"', True);

      //RegExpr.Expression:='Maximum="([0-9\.]{0,})"';
      //WixFile.Text:=RegExpr.Replace(WixFile.Text,
      //  'Maximum="'+version+'"', True);

      RegExpr.Expression:='<Product(.+?)Name="(.+?)\s(\d+\.\d+)"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Product$1Name="$2 '+version_main+'"', True);

      RegExpr.Expression:='<Product(.+?)Version="(\d+\.\d+\.\d+)"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Product$1Version="'+version+'"', True);

      RegExpr.Expression:='"PREVIOUSVERSIONINSTALLED"\s+Minimum="([0-9\.]{0,})"\s+Maximum="([0-9\.]{0,})"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '"PREVIOUSVERSIONINSTALLED" Minimum="'+version_min+'" Maximum="'+version+'"', True);

      RegExpr.Expression:='"NEWERPRODUCTFOUND" Minimum="([0-9\.]{0,})"\s+Maximum="([0-9\.]{0,})"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '"NEWERPRODUCTFOUND" Minimum="'+version+'" Maximum="'+version_max+'"', True);

      RegExpr.Expression:='<Product([^>]+?)Id="([0-9A-F\-]{0,})"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Product$1Id="'+GetNewGUID+'"', True);

      RegExpr.Expression:='<Package([^>]+?)Id="([0-9A-F\-]{0,})"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Package$1Id="'+GetNewGUID+'"', True);

      RegExpr.Expression:='"Pversion">([0-9\.]{0,})<';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '"Pversion">'+version+'<', True);

      RegExpr.Expression:='"QualifiedProductName">([a-zA-Z\s]{0,})(\s[0-9\.]{0,})<';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '"QualifiedProductName">$1 '+version+'<', True);

      WixFile.SaveToFile(WIXFileName);
    finally
      RegExpr.Free;
    end;
  finally
    WixFile.Free;
  end;
end;

function GetNewGUIDString: string;
begin
  Result:='Guid="'+GetNewGUID+'"';
end;

procedure UpdateWIXMergeFile(version: string; build_level: string;
  WIXFileName: string);
var WixFile: TStringList;
  RegExpr: TRegExpr;
begin
  if(Not(FileExists(WIXFileName)))then
  begin
    WriteLn('The file '+WIXFileName+' cannot be found');
    Exit;
  end;

  WixFile:=TStringList.Create;
  try
    RegExpr:=TRegExpr.Create;
    try
      WixFile.LoadFromFile(WIXFileName);

      RegExpr.Expression:=' Version="([0-9\.]{0,})"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        ' Version="'+version+'"', True);

      RegExpr.Expression:='Guid=''([0-9A-F\-]{0,})''';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        'Guid='''+GetNewGUID+'''', True);

      RegExpr.Expression:='Guid="([0-9A-F\-]{0,})"';
      WixFile.Text:=RegExpr.ReplaceWithCallback(WixFile.Text,
        GetNewGUIDString, True);

      WixFile.SaveToFile(WIXFileName);
    finally
      RegExpr.Free;
    end;
  finally
    WixFile.Free;
  end;
end;

procedure UpdateWixFragmentFile(version: string; build_level: string;
  WIXFileName: string);
var WixFile: TStringList;
  RegExpr: TRegExpr;
  version_main, version_main_short: String;
begin
  if(Not(FileExists(WIXFileName)))then
  begin
    WriteLn('The file '+WIXFileName+' cannot be found');
    Exit;
  end;

  WixFile:=TStringList.Create;
  try
    RegExpr:=TRegExpr.Create;
    try
      WixFile.LoadFromFile(WIXFileName);

      RegExpr.Expression:='(\d+)\.(\d+)\.(\d+)';
      if(RegExpr.Exec(Version))then
      begin
        version_main:=RegExpr.Match[1]+'.'+RegExpr.Match[2];
        version_main_short:=RegExpr.Match[1]+RegExpr.Match[2];
      end;

      RegExpr.Expression:='<Directory Id="MysqlServerDir(.+?)"(.+?)LongName="(.+?)\s(\d+\.\d+)"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Directory Id="MysqlServerDir$1"$2LongName="$3 '+version_main+'"', True);

      RegExpr.Expression:='<Directory Id="MysqlProductMenuFolder"(.+?)LongName="(.+?)\s(\d+\.\d+)"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Directory Id="MysqlProductMenuFolder"$1LongName="$2 '+version_main+'"', True);

      RegExpr.Expression:='<Directory Id="MysqlGuiProductDir"(.+?)LongName="(.+?)\s(\d+\.\d+)"';
      WixFile.Text:=RegExpr.Replace(WixFile.Text,
        '<Directory Id="MysqlGuiProductDir"$1LongName="$2 '+version_main+'"', True);

      WixFile.SaveToFile(WIXFileName);
    finally
      RegExpr.Free;
    end;
  finally
    WixFile.Free;
  end;
end;

procedure SetEnvVar(filename: string; version: string; build_level: string;
  CommercialPrefix: string; product_name: string; platform_name: string);
var SetEnvVarLines: TStringList;
  RegExpr: TRegExpr;
  version_main, version_main_short, version_main_bundle: String;
begin
  SetEnvVarLines:=TStringList.Create;
  try
    if(CommercialPrefix<>'')then
      CommercialPrefix:=CommercialPrefix+'-';

    RegExpr:=TRegExpr.Create;
    try
      RegExpr.Expression:='(\d+)\.(\d+)\.(\d+)';
      if(RegExpr.Exec(Version))then
      begin
        version_main:=RegExpr.Match[1]+'.'+RegExpr.Match[2];
        version_main_short:=RegExpr.Match[1]+RegExpr.Match[2];
        version_main_bundle:=RegExpr.Match[1]+'.'+RegExpr.Match[2]+'-r'+
          RegExpr.Match[3];
      end;
    finally
      RegExpr.Free;
    end;

    if (product_name <> '') then
      product_name := product_name + '-';

    if (platform_name = '') then
      platform_name := 'win32';

    SetEnvVarLines.Add('@echo off');
    if(build_level<>'')then
    begin
      SetEnvVarLines.Add('set SOURCE_DIR=.\bin\mysql-'+product_name+CommercialPrefix+
        version+'-'+build_level+'-'+platform_name);
      SetEnvVarLines.Add('set SETUP_VERSION='+CommercialPrefix+
        version+'-'+build_level);
    end
    else
    begin
      SetEnvVarLines.Add('set SOURCE_DIR=.\bin\mysql-'+product_name+CommercialPrefix+
        version+'-'+platform_name);
      SetEnvVarLines.Add('set SETUP_VERSION='+CommercialPrefix+version);
    end;
    SetEnvVarLines.Add('set SETUP_VERSION_MAIN='+version_main);
    SetEnvVarLines.Add('set SETUP_VERSION_MAIN_SHORT='+version_main_short);
    SetEnvVarLines.Add('set SETUP_VERSION_BUNDLE='+version_main_bundle);

    SetEnvVarLines.Add('');
    SetEnvVarLines.SaveToFile(filename);
  finally
    SetEnvVarLines.Free;
  end;
end;

procedure UpdateDofFiles(version: string; build_level: string;
  DofFiles: TStringList);
var FileLines: TStringList;
  i: integer;
  RegExpr: TRegExpr;
begin
  FileLines:=TStringList.Create;
  RegExpr:=TRegExpr.Create;
  try
    for i:=0 to DofFiles.Count-1 do
    begin
      if(Not(FileExists(DofFiles[i])))then
      begin
        WriteLn('The file '+DofFiles[i]+' cannot be found.');
        Exit;
      end;

      FileLines.LoadFromFile(DofFiles[i]);

      RegExpr.Expression:='FileVersion=([0-9\.]{0,})';
      FileLines.Text:=RegExpr.Replace(FileLines.Text,
        'FileVersion='+version, True);

      FileLines.SaveToFile(DofFiles[i]);
    end;
  finally
    FileLines.Free;
    RegExpr.Free;
  end;
end;

procedure UpdateRcFile(version: string; build_level: string;
  ProjectResourceFile: string);
var FileLines: TStringList;
  RegExpr: TRegExpr;
  version_main, version_main_short, version_comma: String;
begin
  if(Not(FileExists(ProjectResourceFile)))then
  begin
    WriteLn('The file '+ProjectResourceFile+' cannot be found');
    Exit;
  end;

  FileLines:=TStringList.Create;
  try
    RegExpr:=TRegExpr.Create;
    try
      FileLines.LoadFromFile(ProjectResourceFile);

      RegExpr.Expression:='(\d+)\.(\d+)\.(\d+)';
      if(RegExpr.Exec(Version))then
      begin
        version_main:=RegExpr.Match[1]+'.'+RegExpr.Match[2];
        version_main_short:=RegExpr.Match[1]+RegExpr.Match[2];
      end;
      version_comma:=AnsiReplaceStr(version, '.', ', ');

      RegExpr.Expression:='FILEVERSION ([\d, ]+)';
      FileLines.Text:=RegExpr.Replace(FileLines.Text,
        'FILEVERSION '+version_comma, True);

      RegExpr.Expression:='PRODUCTVERSION ([\d, ]+)';
      FileLines.Text:=RegExpr.Replace(FileLines.Text,
        'PRODUCTVERSION '+version_comma, True);

      RegExpr.Expression:='VALUE "FileVersion", "([\d\.]+)\\000"';
      FileLines.Text:=RegExpr.Replace(FileLines.Text,
        'VALUE "FileVersion", "'+version+'\\000"', True);

      RegExpr.Expression:='VALUE "ProductVersion", "([\d\.]+)\\000"';
      FileLines.Text:=RegExpr.Replace(FileLines.Text,
        'VALUE "ProductVersion", "'+version_main+'\\000"', True);

      FileLines.SaveToFile(ProjectResourceFile);
    finally
      RegExpr.Free;
    end;
  finally
    FileLines.Free;
  end;
end;

procedure DisplayUsage;
begin
  WriteLn('MySQL SetVersion Version 1.0.0.4, for Win2k/XP');
  WriteLn('This software comes with ABSOLUTELY NO WARRANTY. This is free software,');
  WriteLn('and you are welcome to modify and redistribute it under the GPL license.');
  WriteLn('');
  WriteLn('Setting version constants for GUI tools/Server setup. ');
  WriteLn('Usage: setversion -p[Pascal Include File] -x[WiX wxs File] ');
  WriteLn('                  -m[WiX Merge Module File] -f[WiX fragment File]');
  WriteLn('');
  WriteLn('Optional parameters:');
  WriteLn('                  -v[Version] -b[Build type] -s[ENV Var script file]');
  WriteLn('                  -c[commercial prefix]');
  WriteLn('');
end;

procedure ParseParams;
var pascalIncludeFile: string;
  WIXFile, WIXMergeFile, WixFragmentFile: string;
  version: string;
  build_level: string;
  env_var_filename: string;
  somethingToDo: Boolean;
  i: integer;
  RegExpr: TRegExpr;
  CommercialPrefix: string;
  DofFiles: TStringList;
  ProjectResourceFile: string;
  product_name: string;
  platform_name: string;
begin
  pascalIncludeFile:='';
  WIXFile:='';
  WIXMergeFile:='';
  WixFragmentFile:='';

  somethingToDo:=False;
  version:='';
  build_level:='';
  env_var_filename:='';
  CommercialPrefix:='';
  ProjectResourceFile:='';
  product_name:='';
  platform_name:='';

  DofFiles:=TStringList.Create;
  try
    //Check all parameters
    for i:=1 to ParamCount do
    begin
      //-p Write Pascal Include File
      if(Copy(ParamStr(i), 1, 2)='-p')then
      begin
        pascalIncludeFile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //-x Process Wix file
      else if(Copy(ParamStr(i), 1, 2)='-x')then
      begin
        WIXFile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //-m Process Wix Merge file
      else if(Copy(ParamStr(i), 1, 3)='-m')then
      begin
        WIXMergeFile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //-v version
      else if(Copy(ParamStr(i), 1, 2)='-v')then
      begin
        version:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
      end
      //-b build level
      else if(Copy(ParamStr(i), 1, 2)='-b')then
      begin
        build_level:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
      end
      //-s env_var_filename
      else if(Copy(ParamStr(i), 1, 2)='-s')then
      begin
        env_var_filename:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //-n product name
      else if(Copy(ParamStr(i), 1, 2)='-n')then
      begin
        product_name:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
      end
      //Wix Fragment File
      else if(Copy(ParamStr(i), 1, 2)='-f')then
      begin
        WixFragmentFile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //commercial prefix
      else if(Copy(ParamStr(i), 1, 2)='-c')then
      begin
        CommercialPrefix:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //VersionInfo in .dof files
      else if(Copy(ParamStr(i), 1, 2)='-d')then
      begin
        DofFiles.Add(Copy(ParamStr(i), 3, Length(ParamStr(i))-1));
        somethingToDo:=True;
      end
      //-r Change project resource file
      else if(Copy(ParamStr(i), 1, 2)='-r')then
      begin
        ProjectResourceFile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
        somethingToDo:=True;
      end
      //-l platform
      else if(Copy(ParamStr(i), 1, 2)='-l')then
      begin
        platform_name:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
      end;
    end;

    if(build_level='a')then
      build_level:='alpha'
    else if(build_level='b')then
      build_level:='beta'
    else if(build_level='r')then
      build_level:='rc'
    else if(build_level='p')then
      build_level:=''
    else if(build_level='i')then
      build_level:='internal';

    if(version='')and(build_level='')then
    begin
      WriteLn('MySQL SetVersion Version 1.0.1.5, for Win2k/XP'#13#10);

      version:='';
      while(version='')do
      begin
        Write('Please enter the version number (e.g. 1.0.0): ');
        ReadLn(version);

        if(version='q')or(version='exit')then
          Exit;

        RegExpr:=TRegExpr.Create;
        try
          RegExpr.Expression:='(\d{1,})\.(\d{1,})\.(\d{1,})';
          if(Not(RegExpr.Exec(version)))then
            version:='';
        finally
          RegExpr.Free;
        end;
      end;

      //Get build level
      build_level:='...';
      while(build_level<>'alpha')and
        (build_level<>'beta')and
        (build_level<>'rc')and
        (build_level<>'internal')and
        (build_level<>'')do
      begin
        Write('Please enter the build level (a=alpha, b=beta, r=rc, p=production, i=internal): ');
        ReadLn(build_level);

        if(version='q')or(version='exit')then
          Exit
        else if(build_level='a')then
          build_level:='alpha'
        else if(build_level='b')then
          build_level:='beta'
        else if(build_level='r')then
          build_level:='rc'
        else if(build_level='p')then
          build_level:=''
        else if(build_level='i')then
          build_level:='internal'
        else
          build_level:='...';
      end;
    end;

    if(Not(somethingToDo))then
      DisplayUsage
    else
    begin
      if(pascalIncludeFile<>'')then
        WritePascalIncludeFile(version, build_level, pascalIncludeFile);

      if(WIXFile<>'')then
        UpdateWIXFile(version, build_level, WIXFile);

      if(WIXMergeFile<>'')then
        UpdateWIXMergeFile(version, build_level, WIXMergeFile);

      if(WixFragmentFile<>'')then
        UpdateWixFragmentFile(version, build_level, WixFragmentFile);

      if(env_var_filename<>'')then
        SetEnvVar(env_var_filename, version, build_level, CommercialPrefix,
          product_name, platform_name);

      if(DofFiles.Count>0)then
        UpdateDofFiles(version, build_level, DofFiles);

      if(ProjectResourceFile<>'')then
        UpdateRcFile(version, build_level, ProjectResourceFile);
    end;
  finally
    DofFiles.Free;
  end;
end;

begin
  //If params are specified
  if(ParamCount>0)and(ParamStr(1)<>'--help')then
    ParseParams
  else
    //else write usage
    DisplayUsage;
end.
