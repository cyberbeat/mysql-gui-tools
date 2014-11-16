unit MyxOptions;

interface

uses
  Windows, GnuGetText, Classes, TntForms, SysUtils, TntSysUtils, TntSystem,
  TntClasses,
  Grt, AuxFuncs, Printers;

type
  // Universal connection between option storage and option user.
  IOptionChangeListener = interface
    procedure OptionChanged;
  end;

  TMyxOptions = class(TInterfacedObject, IGrtValueCallbackListener)
    constructor Create(Filename: WideString;
      GrtGlobalOptionPath: WideString); virtual;
    destructor Destroy; override;
  private
    FFileName: WideString;
    FOptionPath: WideString;
    FPOptionDict: Pointer;
    FFormatSettings: TFormatSettings;

    FChangeListeners: TInterfaceList;

    FFixedOptions: TTntStringList;
    FDefaultOptions: TTntStringList;

    function GetOptionInt(Name: WideString): Integer; virtual;
    function GetOptionString(Name: WideString): WideString; virtual;
    procedure SetOptionInt(Name: WideString; Value: Integer);
    procedure SetOptionString(Name: WideString; Value: WideString);
    function GetOptionFloat(Name: WideString): Single;
    procedure SetOptionFloat(Name: WideString; const Value: Single);
  protected
    function GetGrt: TGrt;
    function GetOptionDict: Pointer;

    procedure SetFixedOptions; virtual;
    procedure SetDefaultOptions; virtual;

    procedure Changed;
    function ValueChange(Value: Pointer; Reason: GrtValueCallbackReason): Integer;

    property OptionDict: Pointer read GetOptionDict;
  public
    property Grt: TGrt read GetGrt;

    property OptionFloat[Name: WideString]: Single read GetOptionFloat write SetOptionFloat;
    property OptionInt[Name: WideString]: Integer read GetOptionInt write SetOptionInt;
    property OptionString[Name: WideString]: WideString read GetOptionString write SetOptionString;

    property FixedOptions: TTntStringList read FFixedOptions;
    property DefaultOptions: TTntStringList read FDefaultOptions;

    procedure AddListener(Listener: IOptionChangeListener);
    procedure RemoveListener(Listener: IOptionChangeListener);

    procedure LoadFromFile(FileName: WideString = '');
    procedure SaveToFile(FileName: WideString = '');

    function GetLastFileDialogPaths(DlgName: WideString): WideString;
    procedure SetLastFileDialogPaths(DlgName: WideString; Path: WideString);
  end;

  TMyxCommonOptions = class(TMyxOptions)
  protected
    procedure SetFixedOptions; override;
    procedure SetDefaultOptions; override;
  end;

  function GetStringOptionFromCommandLine(Param: WideString): WideString;
  function GetBoolOptionFromCommandLine(Param: WideString): Boolean;

  function GlobalAppDict: Pointer;

  function CommonOptions: TMyxOptions;

//----------------------------------------------------------------------------------------------------------------------

implementation

var
  CommonOptionsInstance: TMyxOptions;

//----------------------------------------------------------------------------------------------------------------------

function GlobalAppDict: Pointer;

var
  Grt: TGrt;

begin
  Grt := RuntimeEnvironment;

  // Make sure /app is available
  Result := Grt.Global['/app'];
  if (Result = nil) then
  begin
    Result := Grt.DictNew('base.ApplicationData');
    Grt.Global['/app'] := Result;
  end;
end;

// -----------------------------------------------------------------------------

function CommonOptions: TMyxOptions;

var
  S: WideString;

begin
  if (CommonOptionsInstance = nil) then
  begin
    // UserDataDir
    S := GetStringOptionFromCommandLine('UserDataDir');

    if (S <> '') then
      S := WideIncludeTrailingBackslash(S)
    else
      S := GetApplicationDataDir + 'MySQL\';

    // Make sure /app exists
    GlobalAppDict;

    CommonOptionsInstance := TMyxCommonOptions.Create(
      S + 'Common Options.xml', '/app/commonOptions');
  end;

  Result := CommonOptionsInstance;
end;

// -----------------------------------------------------------------------------

constructor TMyxOptions.Create(Filename: WideString;
  GrtGlobalOptionPath: WideString);

begin
  inherited Create;

  FFileName := Filename;
  FOptionPath := GrtGlobalOptionPath;
  FPOptionDict := nil;

  FFixedOptions := TTntStringList.Create;
  FDefaultOptions := TTntStringList.Create;

  // Fill the format settings with language neutral values (actually english is used but for all system languages).
  GetLocaleFormatSettings(MakeLong(LANG_ENGLISH, SUBLANG_NEUTRAL), FFormatSettings);

  SetFixedOptions;
  SetDefaultOptions;

  LoadFromFile;
end;

// -----------------------------------------------------------------------------

destructor TMyxOptions.Destroy;

begin
  if (FPOptionDict <> nil) then
    Grt.ValueListenerRemove(FPOptionDict, self);

  FChangeListeners.Free;

  FFixedOptions.Free;
  FDefaultOptions.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

function TMyxOptions.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

// -----------------------------------------------------------------------------

function TMyxOptions.GetOptionDict: Pointer;

begin
  if (FPOptionDict = nil) then
  begin
    FPOptionDict := Grt.Global[FOptionPath];

    if (FPOptionDict = nil) then
    begin
      FPOptionDict := Grt.DictNew('');
      Grt.ValueListenerAdd(FPOptionDict, self);

      Grt.Global[FOptionPath] := FPOptionDict;
    end;
  end;

  Result := FPOptionDict;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.LoadFromFile(FileName: WideString);

var I: Integer;

begin
  if (FileName = '') then
    FileName := FFileName;

  if (FileName <> '') then
  begin
    FPOptionDict := Grt.ValueLoadFromFile(FileName);

    if (FPOptionDict = nil) then
      FPOptionDict := Grt.DictNew('');

    // ensure default options are set
    for I := 0 to FDefaultOptions.Count - 1 do
      if (Grt.DictItem[FPOptionDict, FDefaultOptions.Names[I]] = nil) then
        Grt.DictString[FPOptionDict, FDefaultOptions.Names[I]] :=
          FDefaultOptions.ValueFromIndex[I];

    // Attach listener
    Grt.ValueListenerAdd(FPOptionDict, self);

    // Set GRT global
    Grt.Global[FOptionPath] := FPOptionDict;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SaveToFile(FileName: WideString);

begin
  if (FPOptionDict <> nil) then
  begin
    if (FileName = '') then
      FileName := FFileName;

    WideForceDirectories(WideExtractFilePath(FileName));

    Grt.ValueSaveToFile(FileName, FPOptionDict);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.AddListener(Listener: IOptionChangeListener);

begin
  if FChangeListeners = nil then
    FChangeListeners := TInterfaceList.Create;
  if FChangeListeners.IndexOf(Listener) = -1 then
    FChangeListeners.Add(Listener);
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.RemoveListener(Listener: IOptionChangeListener);

begin
  if Assigned(FChangeListeners) then
    FChangeListeners.Remove(Listener);
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.Changed;

var
  I: Integer;

begin
  if Assigned(FChangeListeners) then
  begin
    for I := 0 to FChangeListeners.Count - 1 do
      IOptionChangeListener(FChangeListeners[I]).OptionChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.ValueChange(Value: Pointer; Reason: GrtValueCallbackReason): Integer;

begin
  if (Reason = GrtVcrDictItemChange) then
    Changed;

  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.GetOptionFloat(Name: WideString): Single;

var Value: WideString;
  PeriodPos,
  CommaPos: Integer;

begin
  Value := GetOptionString(name);

  if (Value <> '') then
  begin
    PeriodPos := Pos('.', Value);
    CommaPos := Pos(',', Value);

    if (PeriodPos < CommaPos) then
    begin
      FFormatSettings.DecimalSeparator := ',';
      FFormatSettings.ThousandSeparator := '.';
    end
    else
    begin
      FFormatSettings.DecimalSeparator := '.';
      FFormatSettings.ThousandSeparator := ',';
    end;

    Result := StrToFloatDef(Value, 0, FFormatSettings);
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.GetOptionInt(Name: WideString): Integer;

begin
  Result := StrToIntDef(GetOptionString(name), 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SetOptionFloat(Name: WideString; const Value: Single);

begin
  SetOptionString(Name, FloatToStr(Value, FFormatSettings));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.SetOptionInt(Name: WideString; Value: Integer);

begin
  SetOptionString(Name, IntToStr(Value));
end;

// -----------------------------------------------------------------------------

function TMyxOptions.GetOptionString(Name: WideString): WideString;

var
  Value: Pointer;
  Index: Integer;

begin
  // look if there is a fixed option with this name
  Index := FFixedOptions.IndexOfName(Name);
  if (Index > -1) then
    Result := FFixedOptions.ValueFromIndex[Index]
  else
  begin
    Value := Grt.DictItem[OptionDict, Name];

    if (Value <> nil) then
      Result := Grt.ValueString[Value]
    else
    begin
      // if the value is not set yet, look at the defaults
      // and set the option with the default value
      Index := FDefaultOptions.IndexOfName(Name);
      if (Index > -1) then
        Result := FDefaultOptions.ValueFromIndex[Index]
      else
        Result := '';

      Grt.DictString[OptionDict, Name] := Result;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SetOptionString(Name: WideString; Value: WideString);

begin
  if (FFixedOptions.IndexOfName(Name) > -1) then
    raise Exception.CreateFmt(
      _('The option %s is a fixed option and cannot be set.'), [Name]);
  Grt.DictString[OptionDict, Name] := Value;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SetFixedOptions;

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SetDefaultOptions;

begin
  //
end;

// -----------------------------------------------------------------------------

function TMyxOptions.GetLastFileDialogPaths(DlgName: WideString): WideString;

var
  PathList: TTntStringList;

begin
  PathList := TTntStringList.Create;
  try
    PathList.Text := OptionString['DialogPaths'];

    Result := PathList.Values['DlgName'];
  finally
    PathList.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxOptions.SetLastFileDialogPaths(DlgName: WideString; Path: WideString);

var
  DialogPaths: WideString;

begin
  DialogPaths := OptionString['DialogPaths'];

  if (DialogPaths = '') then
    DialogPaths := DlgName + '=' + Path
  else
    DialogPaths := DialogPaths + #13#10 + DlgName + '=' + Path;

  OptionString['DialogPaths'] := DialogPaths;
end;

// -----------------------------------------------------------------------------

function GetStringOptionFromCommandLine(Param: WideString): WideString;

var
  I: Integer;

begin
  Result := '';

  for I := 1 to WideParamCount do
    if (WideSameText(WideParamStr(I), '-' + Param)) and
      (I+1 <= WideParamCount) then
    begin
      Result := WideParamStr(I + 1);

      break;
    end;
end;

// -----------------------------------------------------------------------------

function GetBoolOptionFromCommandLine(Param: WideString): Boolean;

var
  I: Integer;

begin
  Result := False;

  for I := 1 to WideParamCount do
    if (WideSameText(WideParamStr(I), '-' + Param)) then
    begin
      Result := True;

      break;
    end;
end;

// -----------------------------------------------------------------------------
// TMyxCommonOptions
// -----------------------------------------------------------------------------

procedure TMyxCommonOptions.SetFixedOptions;

var
  S: WideString;

begin
  // UserDataDir
  S := GetStringOptionFromCommandLine('UserDataDir');

  if (S <> '') then
    S := WideIncludeTrailingBackslash(S)
  else
    S := GetApplicationDataDir + 'MySQL\';

  FixedOptions.Add('UserDataDir=' + S);

  // XMLDir
  S := GetStringOptionFromCommandLine('XMLDir');

  if (S <> '') then
    S := WideIncludeTrailingBackslash(S)
  else
    S := AuxFuncs.GetApplDir + 'XML\';

  FixedOptions.Add('XMLDir=' + S);

  // NoRegEntries
  FixedOptions.Add('NoRegEntries=' +
    IntToStr(Ord(GetBoolOptionFromCommandLine('NoRegEntries'))));
end;

// -----------------------------------------------------------------------------

procedure TMyxCommonOptions.SetDefaultOptions;

begin
  DefaultOptions.Add('DefaultFontName=Tahoma');
  DefaultOptions.Add('DefaultFontHeight=13');

  DefaultOptions.Add('DefaultCodeFontName=Bitstream Vera Sans Mono');
  DefaultOptions.Add('DefaultCodeFontWidth=7');
  DefaultOptions.Add('DefaultCodeFontHeight=-11');
  DefaultOptions.Add('DefaultCodeFontCharset=1');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
