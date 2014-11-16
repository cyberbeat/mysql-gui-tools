unit Options;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  Windows, Classes, TntClasses, Forms, TntForms, SysUtils, Messages, ComCtrls,
  TntComCtrls, myx_util_public_interface, myx_public_interface, AuxFuncs, Types, StrUtils,
  TntSystem, TntSysUtils;

const
  WM_OptionsChanged = WM_USER + 100;

type
  // Universal connection between option storage and option user.
  IOptionChangeListener = interface
    procedure OptionChanged;
  end;

  IOptionProvider = interface
    ['{B37BCB77-4A85-4863-8615-C3ED9AB96C4D}']
    procedure AddListener(Listener: IOptionChangeListener);
    function GetOptionAsBoolean(Name: WideString): Boolean;
    function GetOptionAsDouble(Name: WideString): Double;
    function GetOptionAsInteger(Name: WideString): Integer;
    function GetOptionAsString(Name: WideString): WideString;
    procedure RemoveListener(Listener: IOptionChangeListener);
    procedure SetOptionAsBoolean(Name: WideString; Value: Boolean);
    procedure SetOptionAsDouble(Name: WideString; Value: Double);
    procedure SetOptionAsInteger(Name: WideString; Value: Integer);
    procedure SetOptionAsString(Name: WideString; Value: WideString);

    property OptionAsBoolean[Name: WideString]: Boolean read GetOptionAsBoolean write SetOptionAsBoolean;
    property OptionAsDouble[Name: WideString]: Double read GetOptionAsDouble write SetOptionAsDouble;
    property OptionAsInteger[Name: WideString]: Integer read GetOptionAsInteger write SetOptionAsInteger;
    property OptionAsString[Name: WideString]: WideString read GetOptionAsString write SetOptionAsString;

    procedure AddWindowPos(Win: TTntForm);
    function RestoreWindowPos(Win: TTntForm; DoSize: Boolean = True): Boolean;
  end;

  TDoPageContentChanged = procedure(Sender: TObject) of object;

  TMyxOptions = class(TInterfacedObject, IOptionProvider)
  private
    // Options stored as associated list.
    // TODO: Use Hashmap.
    FOptions: TTntStringList;
    FFormatSettings: TFormatSettings; // Used to convert float values language neutrally.
    FChangeListeners: TInterfaceList;
    FApplicationID: string;

    // Option provider implementation.
    function GetOptionAsBoolean(Name: WideString): Boolean;
    function GetOptionAsDouble(Name: WideString): Double;
    function GetOptionAsInteger(Name: WideString): Integer;
    function GetOptionAsString(Name: WideString): WideString;
    // TODO: Set method must use groups for options.
    procedure SetOptionAsBoolean(Name: WideString; Value: Boolean);
    procedure SetOptionAsDouble(Name: WideString; Value: Double);
    procedure SetOptionAsInteger(Name: WideString; Value: Integer);
    procedure SetOptionAsString(Name: WideString; Value: WideString);
  public
    WindowPosList: TTntStringList;
    IgnoreWarningsList: TTntStringList;

    constructor Create(const ApplicationID: string); virtual;
    destructor Destroy; override;

    procedure AddListener(Listener: IOptionChangeListener);
    procedure Changed;
    procedure LoadOptions; virtual; abstract;
    procedure RemoveListener(Listener: IOptionChangeListener);
    procedure StoreOptions; virtual; abstract;

    function CheckBoolStr(s: WideString): Boolean;
    procedure AddParam(OptionGroup: TMYX_OPTION_GROUP; name: WideString; value: WideString);
    procedure AddWindowPos(Win: TTntForm);
    function RestoreWindowPos(Win: TTntForm; DoSize: Boolean = True): Boolean; virtual;
    procedure StoreListOptions(Groups: TMYX_OPTION_GROUP_List);
  end;

  TMyxWindowPos = class
    constructor Create; overload;
    constructor Create(AsWideString: WideString); overload;
    function AsWideString: WideString;
  public
    Name: WideString;
    State: TWindowState;
    Pos: TPoint;
    Size: TPoint;
    MonitorNum: Integer;
  end;

  TMYXCommonOptions = class(TMyxOptions)
  public
    ConnectionToUse: WideString;
    ConnectionUsername: WideString;
    ConnectionPassword: WideString;
    ConnectionHost: WideString;
    ConnectionPort: Integer;
    ConnectionSchema: WideString;

    AvailableLanguages: WideString;
    AvailableLanguageCodes: WideString;
    AvailablePasswordStorageTypes: WideString;

    StoreWindowsPositions,
    ShowTipOfDay: Boolean;

    Language: WideString;

    XPStyleEnabled: Boolean;

    XMLDir, UserDataDir: WideString;

    PasswordStorageType: MYX_PASSWORD_STORAGE_TYPE;

    EditorKeepRoutineEditorOnTop,
    EditorTableShowSQLBeforeApplying,
    EditorTableAllColumnsNotNullPerDef,
    EditorTableIntegerUnsignedPerDef: Boolean;
    EditorTablePKDataType,
    EditorTableDefColumnDataType,
    EditorTablePKAutoNaming,
    EditorTableIndexAutoNaming,
    EditorTableFKAutoNaming,
    EditorTableDefaultStorageEngine: WideString;
    Datatypes: PMYX_DBM_DATATYPES;

    DefaultFontName,
    DataFontName,
    CodeFontName: WideString;
    DefaultFontHeight,
    DataFontHeight,
    CodeFontHeight,
    CodeFontWidth,
    CodeFontCharset: Integer;

    DisableTransparencyEffects: Boolean;

    NoRegistry: Boolean;
  public
    destructor Destroy; override;

    procedure LoadOptions; override;
    procedure StoreOptions; override;

    procedure GetAvailableLanguages(var LangNames: WideString; var LangIds: WideString);
    function RestoreWindowPos(Win: TTntForm; DoSize: Boolean = True): Boolean; override;
  end;

  TApplicationOptionsForm = class(TTntForm)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetControls(PageNr: integer); virtual; abstract;
    procedure ApplyChanges(PageNr: integer); virtual; abstract;
  public
    OptionsImgNames: TTntStringList;
    DockOptionPageControl: TTntPageControl;
    DoPageContentChanged: TDoPageContentChanged;
  end;

function GetWinPos(Win: TTntForm; WindowPosList: TTntStringList): TMyxWindowPos;
procedure SetWinPos(Win: TTntForm; WinPos: TMyxWindowPos; DoSize: Boolean = True);

var
  MYXCommonOptions: TMYXCommonOptions;
  MYXCommonOptionProvider: IOptionProvider;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  gnugettext;

const
  oleaut = 'oleaut32.dll';

function SysAllocStringLen(P: PWideChar; Len: Integer): PWideChar; stdcall; external oleaut name 'SysAllocStringLen';
procedure SysFreeString(const S: PWideChar); stdcall; external oleaut name 'SysFreeString';

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxOptions.Create(const ApplicationID: string);

begin
  inherited Create;

  FApplicationID := ApplicationID;
  WindowPosList := TTntStringList.Create;
  IgnoreWarningsList := TTntStringList.Create;
  FOptions := TTntStringList.Create;

  // Fill the format settings with language neutral values (actually english is used but for all system languages).
  GetLocaleFormatSettings(MakeLong(LANG_ENGLISH, SUBLANG_NEUTRAL), FFormatSettings);
  LoadOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMyxOptions.Destroy;

var
  I: integer;

begin
  for I := 0 to WindowPosList.Count - 1 do
    WindowPosList.Objects[I].Free;
  WindowPosList.Free;

  IgnoreWarningsList.Free;

  for I := 0 to FOptions.Count - 1 do
    SysFreeString(PWideChar(FOptions.Objects[I]));

  FOptions.Free;
  FChangeListeners.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.AddListener(Listener: IOptionChangeListener);

begin
  if FChangeListeners = nil then
    FChangeListeners := TInterfaceList.Create;
  if FChangeListeners.IndexOf(Listener) = -1 then
    FChangeListeners.Add(Listener);
end;

//----------------------------------------------------------------------------------------------------------------------

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

function TMyxOptions.GetOptionAsBoolean(Name: WideString): Boolean;

var
  Value: WideString;
  Index: Integer;

begin
  if FOptions.Find(Name, Index) then
  begin
    Value := PWideChar(FOptions.Objects[Index]);
    // Value will be converted to ANSI using the current locale.
    Result := StrToBoolDef(Value, False);
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.GetOptionAsDouble(Name: WideString): Double;

var
  Value: WideString;
  Index: Integer;

begin
  if FOptions.Find(Name, Index) then
  begin
    Value := PWideChar(FOptions.Objects[Index]);
    // Value will be converted to ANSI using the current locale.
    Result := StrToFloatDef(Value, 0, FFormatSettings);
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.GetOptionAsInteger(Name: WideString): Integer;

var
  Value: WideString;
  Index: Integer;

begin
  if FOptions.Find(Name, Index) then
  begin
    Value := PWideChar(FOptions.Objects[Index]);
    // Value will be converted to ANSI using the current locale.
    Result := StrToIntDef(Value, 0);
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.GetOptionAsString(Name: WideString): WideString;

var
  Index: Integer;

begin
  if FOptions.Find(Name, Index) then
    Result := PWideChar(FOptions.Objects[Index])
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.RemoveListener(Listener: IOptionChangeListener);

begin
  if Assigned(FChangeListeners) then
    FChangeListeners.Remove(Listener);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.SetOptionAsBoolean(Name: WideString; Value: Boolean);

var
  ValueString: WideString;

begin
  ValueString := BoolToStr(Value, True);
  SetOptionAsString(Name, ValueString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.SetOptionAsDouble(Name: WideString; Value: Double);

var
  ValueString: WideString;

begin
  ValueString := FloatToStr(Value, FFormatSettings);
  SetOptionAsString(Name, ValueString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.SetOptionAsInteger(Name: WideString; Value: Integer);

var
  ValueString: WideString;

begin
  ValueString := IntToStr(Value);
  SetOptionAsString(Name, ValueString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.SetOptionAsString(Name, Value: WideString);

var
  Index: Integer;
  Temp: PWideChar;
  NeedSort: Boolean;

begin
  if not FOptions.Find(Name, Index) then
  begin
    Index := FOptions.Add(Name);
    NeedSort := True;
  end
  else
  begin
    Temp := Pointer(FOptions.Objects[Index]);
    SysFreeString(Temp);
    NeedSort := False;
  end;

  Temp := SysAllocStringLen(PWideChar(Value), Length(Value));
  FOptions.Objects[Index] := Pointer(Temp);

  if NeedSort then
    FOptions.Sort;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.CheckBoolStr(s: WideString): Boolean;

begin
  Result := (StrToIntDef(s, 1) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.AddParam(OptionGroup: TMYX_OPTION_GROUP; name: WideString; value: WideString);

begin
  OptionGroup.name_value_pairs.Add(TMYX_NAME_VALUE_PAIR.create(name, value));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.AddWindowPos(Win: TTntForm);

var
  WinPos: TMyxWindowPos;
  I: Integer;

begin
  WinPos := GetWinPos(Win, WindowPosList);

  repeat
    I := WindowPosList.IndexOf(Win.Name);
    if I < 0 then
      Break;
    WindowPosList.Objects[I].Free;
    WindowPosList.Delete(I);
  until False;

  WindowPosList.AddObject(Win.Name, WinPos);

  StoreOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxOptions.RestoreWindowPos(Win: TTntForm; DoSize: Boolean): Boolean;

var
  i: integer;
  WinPos: TMyxWindowPos;
  
begin
  WinPos := nil;

  i := WindowPosList.IndexOf(Win.Name);

  if (i > -1) then
    WinPos := TMyxWindowPos(WindowPosList.Objects[i]);

  if Assigned(WinPos) then
  begin
    Win.DefaultMonitor := dmDesktop;
    SetWinPos(Win, WinPos, DoSize);
    Result := True;
  end
  else
  begin
    Win.Position := poScreenCenter;
    Result := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxOptions.StoreListOptions(Groups: TMYX_OPTION_GROUP_List);

var
  I: Integer;
  OptionGroup: TMYX_OPTION_GROUP;

begin
  OptionGroup := TMYX_OPTION_GROUP.create('main');
  Groups.Add(OptionGroup);
  for I := 0 to FOptions.Count - 1 do
    AddParam(OptionGroup, FOptions[I], PWideChar(FOptions.Objects[I]));
end;

//----------------- TMyWindowPos ---------------------------------------------------------------------------------------

function GetNextNumValue(var S: WideString): integer;

var
  p, l: Integer;
  
begin
  p := Pos('=', S);
  l := Pos(';', S);
  Result := StrToIntDef(Copy(S, p + 1, l - p - 1), 0);
  S := Copy(S, l + 1, Length(S));
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxWindowPos.Create;

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxWindowPos.Create(AsWideString: WideString);

begin
  inherited Create;

  Pos.X := GetNextNumValue(AsWideString);
  Pos.Y := GetNextNumValue(AsWideString);

  Size.X := GetNextNumValue(AsWideString);
  Size.Y := GetNextNumValue(AsWideString);

  State := TWindowState(GetNextNumValue(AsWideString));

  MonitorNum := GetNextNumValue(AsWideString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxWindowPos.AsWideString: WideString;

var
  S: WideString;
  
begin
  S := S + 'x=' + IntToStr(Pos.X) + '; y=' + IntToStr(Pos.Y) +
    '; w=' + IntToStr(Size.X) + '; h=' + IntToStr(Size.Y) +
    '; s=' + IntToStr(Ord(State)) + '; m=' + IntToStr(MonitorNum);

  Result := S;
end;

function GetWinPos(Win: TTntForm;
  WindowPosList: TTntStringList): TMyxWindowPos;
var
  WinPos: TMyxWindowPos;
  i: integer;
begin
  WinPos := TMyxWindowPos.Create;

  WinPos.Name := Win.Name;
  WinPos.State := Win.WindowState;
  WinPos.MonitorNum := Win.Monitor.MonitorNum;

  //P:=win.ClientToScreen(Point(0, 0));

  if (Win.WindowState <> wsMaximized) then
  begin
    WinPos.Pos := Point(Win.Left, Win.Top);
    WinPos.Size := Point(Win.Width, Win.Height);
  end
  else
  begin
    i := WindowPosList.IndexOf(Win.Name);
    if (i > -1) then
    begin
      WinPos.Pos := TMyxWindowPos(WindowPosList.Objects[i]).Pos;
      WinPos.Size := TMyxWindowPos(WindowPosList.Objects[i]).Size;

      //Check if this position is on the right monitor
      if (WinPos.Pos.X < Screen.Monitors[WinPos.MonitorNum].Left) or
        (WinPos.Pos.X >= Screen.Monitors[WinPos.MonitorNum].Left +
        Screen.Monitors[WinPos.MonitorNum].Width) then
      begin
        WinPos.Pos.X := Screen.Monitors[WinPos.MonitorNum].Left;
        WinPos.Pos.Y := Screen.Monitors[WinPos.MonitorNum].Top;
      end;
    end
    else
    begin
      WinPos.Pos := Point(0, 0);
      WinPos.Size := Point(0, 0);
    end;
  end;

  Result := WinPos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SetWinPos(Win: TTntForm; WinPos: TMyxWindowPos; DoSize: Boolean);

begin
  if (WinPos.Pos.X < 0) then
    Win.Left := 0
  else
    if (WinPos.Pos.X >= Screen.DesktopWidth - 10) then
      Win.Left := Screen.Width - 10
    else
      Win.Left := WinPos.Pos.X;

  if (WinPos.Pos.Y < 0) then
    Win.Top := 0
  else
    if (WinPos.Pos.Y >= Screen.DesktopHeight - 10) then
      Win.Left := Screen.Height - 10
    else
      Win.Top := WinPos.Pos.Y;

  if (DoSize) then
  begin
    if (WinPos.Size.X < 100) then
      Win.Width := 100
    else
      Win.Width := WinPos.Size.X;

    if (WinPos.Size.Y < 40) then
      Win.Height := 40
    else
      Win.Height := WinPos.Size.Y;
  end;

  Win.WindowState := WinPos.State;
end;

//----------------- TMYXCommonOptions ----------------------------------------------------------------------------------

destructor TMYXCommonOptions.Destroy;

begin
  StoreOptions;
  
  if Assigned(Datatypes) then
    myx_free_datatype(Datatypes);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMYXCommonOptions.LoadOptions;

var
  POptions: PMYX_APPLICATION_OPTIONS;
  Options: TMYX_APPLICATION_OPTIONS;
  i, j: integer;
  error: MYX_LIB_ERROR;
  OptionGroupName,
  OptionName,
  OptionValue: WideString;
  ExePath: WideString;

begin
  ExePath := WideExtractFilePath(TntApplication.ExeName);

  NoRegistry := False;

  //check if a UserDataDir was given as command line param
  for i := 1 to WideParamCount do
    if (WideSameText(WideParamStr(I), '-datadir')) and (I+1 <= WideParamCount) then
    begin
      UserDataDir := WideIncludeTrailingBackslash(WideParamStr(I + 1));

      if (Copy(UserDataDir, 1, 1) = '\') or
        (Copy(UserDataDir, 1, 1) = '/') then
        UserDataDir := ExePath +
          Copy(UserDataDir, 2, MaxInt);
    end
    else
      if (WideSameText(WideParamStr(I), '-noreg')) then
        NoRegistry := True;


  if (UserDataDir = '') then
    UserDataDir := GetApplicationDataDir + 'MySQL\';
  ForceDirectories(UserDataDir);

  //Detect if XPStyle is enabled
  XPStyleEnabled := GetXPStyleEnabled;

  GetAvailableLanguages(AvailableLanguages, AvailableLanguageCodes);
  AvailablePasswordStorageTypes := 'Plaintext'#13#10'Obscured';
  if (not (IsWinNT)) then
    AvailablePasswordStorageTypes := AvailablePasswordStorageTypes + #13#10'OS Specific';

  XMLDir := ExtractFilePath(Application.ExeName) + 'XML\';

  //------------------------------------------------------------
  //Initialize Values
  Language := '-';

  //Initialize Default Values
  PasswordStorageType := MYX_PASSWORD_NOT_STORED;

  StoreWindowsPositions := True;
  ShowTipOfDay := False;

  EditorKeepRoutineEditorOnTop := True;
  EditorTableShowSQLBeforeApplying := True;
  EditorTableAllColumnsNotNullPerDef := True;
  EditorTableIntegerUnsignedPerDef := True;
  EditorTablePKDataType := 'INTEGER';
  EditorTableDefColumnDataType := 'VARCHAR(45)';
  EditorTablePKAutoNaming := 'id%tablename%';
  EditorTableIndexAutoNaming := 'Index_%nr%';
  EditorTableFKAutoNaming := 'FK_%tablename%_%nr%';

  EditorTableShowSQLBeforeApplying := True;
  EditorTableAllColumnsNotNullPerDef := True;
  EditorTableIntegerUnsignedPerDef := True;
  EditorTablePKAutoNaming := 'id%tablename%';
  EditorTableFKAutoNaming := 'FK_%tablename%_%nr%';
  EditorTableDefaultStorageEngine := 'InnoDB';

  DefaultFontName := 'MS Sans Serif';
  DataFontName := 'MS Sans Serif';
  CodeFontName := 'Bitstream Vera Sans Mono';
  CodeFontCharset := 1; //DEFAULT_CHARSET

  DefaultFontHeight := -11;
  SetOptionAsInteger('DefaultFontHeight', -11);
  DataFontHeight := -11;
  SetOptionAsInteger('DataFontHeight', -11);
  CodeFontHeight := -11;
  SetOptionAsInteger('CodeFontHeight', -11);
  CodeFontWidth := 8;
  SetOptionAsInteger('CodeFontWidth', 8);

  DisableTransparencyEffects := False;

  // Read list of all known column datatypes.
  Datatypes := myx_datatype_load(XMLDir + 'mysqlx_dbm_datatypes.xml', @error);
  if (Datatypes = nil) or (error <> MYX_NO_ERROR) then
  if (ShowModalDialog(_('Cannot fetch datatype information.'),
    Format(_('The data types file `%s` could not be loaded. You won''t be able to to use the table editor.'#10#10 +
      'Do you want to exit now or continue anyway?'), [XMLDir + 'mysqlx_dbm_datatypes.xml']),
    myx_mtWarning, _('Exit') + #13#10 + _('Continue')) = 1) then
    Halt;

  // Read common options file.
  if (FileExists(UserDataDir + 'mysqlx_common_options.xml')) then
  begin
    POptions := myx_get_application_options(
      UserDataDir + 'mysqlx_common_options.xml',
      @error);
    try
      if (error <> MYX_NO_ERROR) then
      begin
        ShowModalDialog('XML Error', 'Error while loading Options file ''' + UserDataDir + 'mysqlx_common_options.xml' + ''''#13#10 +
          'Error Number ' + IntToStr(Ord(error)), myx_mtError);
      end
      else
      begin
        Options := TMYX_APPLICATION_OPTIONS.Create(POptions);
        try
          for i := 0 to Options.option_groups.Count - 1 do
            for j := 0 to Options.option_groups[i].name_value_pairs.Count - 1 do
            begin
              OptionGroupName := Options.option_groups[i].name;
              OptionName := Options.option_groups[i].name_value_pairs[j].name;
              OptionValue := Options.option_groups[i].name_value_pairs[j].value;

              SetOptionAsString(OptionName, OptionValue);
              if (CompareText(OptionGroupName, 'General') = 0) then
              begin
                if (CompareText(OptionName, 'Language') = 0) then
                  Language := OptionValue
                else
                  if (CompareText(OptionName, 'PasswordStorageType') = 0) then
                    PasswordStorageType := MYX_PASSWORD_STORAGE_TYPE(StrToIntDef(OptionValue, 1))
                  else
                    if (CompareText(OptionName, 'StoreWindowsPositions') = 0) then
                      StoreWindowsPositions := (OptionValue = '1')
                    else
                      if (CompareText(OptionName, 'ShowTipOfDay') = 0) then
                        ShowTipOfDay := (OptionValue = '1')
                      else
                        if (CompareText(OptionName, 'IgnoreWarningsList') = 0) then
                          IgnoreWarningsList.Text :=
                            AnsiReplaceText(OptionValue, '#13#10', #13#10)
                        else
                          if (CompareText(OptionName, 'DisableTransparencyEffects') = 0) then
                            DisableTransparencyEffects := (OptionValue = '1')
                          else
                            if (CompareText(OptionName, 'PasswordStorageType') = 0) then
                              PasswordStorageType := MYX_PASSWORD_STORAGE_TYPE(StrToIntDef(OptionValue, 1))
                            else
                              if (CompareText(OptionName, 'StoreWindowsPositions') = 0) then
                                StoreWindowsPositions := (OptionValue = '1')
                              else
                                if (CompareText(OptionName, 'ShowTipOfDay') = 0) then
                                  ShowTipOfDay := (OptionValue = '1')
                                else
                                    ;

              end
              else
                if (CompareText(OptionGroupName, 'Editors') = 0) then
                begin
                  if (CompareText(OptionName, 'EditorKeepRoutineEditorOnTop') = 0) then
                    EditorKeepRoutineEditorOnTop := (OptionValue = '1')
                  else
                    if (CompareText(OptionName, 'EditorTableShowSQLBeforeApplying') = 0) then
                      EditorTableShowSQLBeforeApplying := (OptionValue = '1')
                    else
                      if (CompareText(OptionName, 'EditorTableAllColumnsNotNullPerDef') = 0) then
                        EditorTableAllColumnsNotNullPerDef := (OptionValue = '1')
                      else
                        if (CompareText(OptionName, 'EditorTableIntegerUnsignedPerDef') = 0) then
                          EditorTableIntegerUnsignedPerDef := (OptionValue = '1')
                        else
                          if (CompareText(OptionName, 'EditorTablePKDataType') = 0) then
                            EditorTablePKDataType := OptionValue
                          else
                            if (CompareText(OptionName, 'EditorTablePKAutoNaming') = 0) then
                              EditorTablePKAutoNaming := OptionValue
                            else
                              if (CompareText(OptionName, 'EditorTableIndexAutoNaming') = 0) then
                                EditorTableIndexAutoNaming := OptionValue
                              else
                                if (CompareText(OptionName, 'EditorTableFKAutoNaming') = 0) then
                                  EditorTableFKAutoNaming := OptionValue
                                else
                                  if (CompareText(OptionName, 'EditorTableDefaultStorageEngine') = 0) then
                                    EditorTableDefaultStorageEngine := OptionValue
                                  else
                                    if (CompareText(OptionName, 'EditorTableDefColumnDataType') = 0) then
                                      EditorTableDefColumnDataType := OptionValue
                                    else
                                      ;
                end
                else
                  if (CompareText(OptionGroupName, 'Fonts') = 0) then
                  begin
                    if (CompareText(OptionName, 'DefaultFontName') = 0) then
                      DefaultFontName := OptionValue
                    else
                      if (CompareText(OptionName, 'DataFontName') = 0) then
                        DataFontName := OptionValue
                      else
                        if (CompareText(OptionName, 'CodeFontName') = 0) then
                          CodeFontName := OptionValue
                        else
                          if (CompareText(OptionName, 'DefaultFontHeight') = 0) then
                            DefaultFontHeight := StrToIntDef(OptionValue, -11)
                          else
                            if (CompareText(OptionName, 'DataFontHeight') = 0) then
                              DataFontHeight := StrToIntDef(OptionValue, -11)
                            else
                              if (CompareText(OptionName, 'CodeFontWidth') = 0) then
                                CodeFontWidth := StrToIntDef(OptionValue, 8)
                              else
                                if (CompareText(OptionName, 'CodeFontHeight') = 0) then
                                  CodeFontHeight := StrToIntDef(OptionValue, -11)
                                else
                                  if (CompareText(OptionName, 'CodeFontCharset') = 0) then
                                    CodeFontCharset := StrToIntDef(OptionValue, 8)
                                  else
                                    ;
                  end
                  else
                    if (CompareText(OptionGroupName, 'WindowPos') = 0) then
                    begin
                      WindowPosList.AddObject(OptionName, TMyxWindowPos.Create(OptionValue));
                    end;
            end;
        finally
          Options.Free;
        end;
      end;
    finally
      //Free Application Options
      myx_free_application_options(POptions);
    end;
  end;

  AuxFuncs.DefaultFontName := DefaultFontName;
  AuxFuncs.DataFontName := DataFontName;
  AuxFuncs.DefaultFontHeight := DefaultFontHeight;
  AuxFuncs.DataFontHeight := DataFontHeight;

  Changed;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMYXCommonOptions.RestoreWindowPos(Win: TTntForm; DoSize: Boolean): Boolean;

begin
  if (StoreWindowsPositions) then
    Result := inherited RestoreWindowpos(Win, DoSize)
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMYXCommonOptions.StoreOptions;

var
  Options: TMYX_APPLICATION_OPTIONS;
  OptionGroup: TMYX_OPTION_GROUP;
  ExePath: WideString;
  i: integer;

begin
  ExePath := ExtractFilePath(Application.ExeName);

  //Create Application Options
  Options := TMYX_APPLICATION_OPTIONS.create;
  try
    StoreListOptions(Options.option_groups);

    // General options group
    OptionGroup := TMYX_OPTION_GROUP.create('General');
    Options.option_groups.Add(OptionGroup);

    AddParam(OptionGroup, 'Language', Language);
    AddParam(OptionGroup, 'PasswordStorageType', IntToStr(Ord(PasswordStorageType)));
    AddParam(OptionGroup, 'StoreWindowsPositions', IntToStr(Ord(StoreWindowsPositions)));
    AddParam(OptionGroup, 'ShowTipOfDay', IntToStr(Ord(ShowTipOfDay)));
    AddParam(OptionGroup, 'DisableTransparencyEffects', IntToStr(Ord(DisableTransparencyEffects)));
    AddParam(OptionGroup, 'IgnoreWarningsList', AnsiReplaceText(IgnoreWarningsList.Text, #13#10, '#13#10'));

    // Editors options group
    OptionGroup := TMYX_OPTION_GROUP.create('Editors');
    Options.option_groups.Add(OptionGroup);

    AddParam(OptionGroup, 'EditorKeepRoutineEditorOnTop', IntToStr(Ord(EditorKeepRoutineEditorOnTop)));
    AddParam(OptionGroup, 'EditorTableShowSQLBeforeApplying', IntToStr(Ord(EditorTableShowSQLBeforeApplying)));
    AddParam(OptionGroup, 'EditorTableAllColumnsNotNullPerDef', IntToStr(Ord(EditorTableAllColumnsNotNullPerDef)));
    AddParam(OptionGroup, 'EditorTableIntegerUnsignedPerDef', IntToStr(Ord(EditorTableIntegerUnsignedPerDef)));
    AddParam(OptionGroup, 'EditorTablePKDataType', EditorTablePKDataType);
    AddParam(OptionGroup, 'EditorTablePKAutoNaming', EditorTablePKAutoNaming);
    AddParam(OptionGroup, 'EditorTableIndexAutoNaming', EditorTableIndexAutoNaming);
    AddParam(OptionGroup, 'EditorTableFKAutoNaming', EditorTableFKAutoNaming);
    AddParam(OptionGroup, 'EditorTableDefaultStorageEngine', EditorTableDefaultStorageEngine);
    AddParam(OptionGroup, 'EditorTableDefColumnDataType', EditorTableDefColumnDataType);

    // Font options group
    OptionGroup := TMYX_OPTION_GROUP.create('Fonts');
    Options.option_groups.Add(OptionGroup);

    AddParam(OptionGroup, 'DefaultFontName', DefaultFontName);
    AddParam(OptionGroup, 'DataFontName', DataFontName);
    AddParam(OptionGroup, 'CodeFontName', CodeFontName);
    AddParam(OptionGroup, 'DefaultFontHeight', IntToStr(DefaultFontHeight));
    AddParam(OptionGroup, 'DataFontHeight', IntToStr(DataFontHeight));
    AddParam(OptionGroup, 'CodeFontWidth', IntToStr(CodeFontWidth));
    AddParam(OptionGroup, 'CodeFontHeight', IntToStr(CodeFontHeight));
    AddParam(OptionGroup, 'CodeFontCharset', IntToStr(CodeFontCharset));

    // Window positions group
    OptionGroup := TMYX_OPTION_GROUP.create('WindowPos');
    Options.option_groups.Add(OptionGroup);

    //Store all window positions
    for i := 0 to WindowPosList.Count - 1 do
      AddParam(OptionGroup, WindowPosList[i],
        TMyxWindowPos(WindowPosList.Objects[i]).AsWideString);

    myx_store_application_options(Options.get_record_pointer, UserDataDir + 'mysqlx_common_options.xml');
    Changed;
  finally
    Options.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMYXCommonOptions.GetAvailableLanguages(var LangNames: WideString; var LangIds: WideString);

var
  Langs: TTntStringList;
  I: Integer;
  LangName, LangId: WideString;
  ID: string;

begin
  LangNames := 'System Default' + #13#10 + 'English' + #13#10;
  LangIds := '-' + #13#10 + 'en' + #13#10;

  Langs := TTntStringList.Create;
  try
    Langs.LoadFromFile(WideExtractFilePath(TntApplication.ExeName) + 'locale\languages_list.txt');

    for I := 0 to Langs.Count - 1 do
    begin
      LangName := Langs.ValueFromIndex[I];
      LangId := Langs.Names[I];

      if FApplicationID = '' then
        ID := 'default'
      else
        ID := FApplicationID;
        
      if WideFileExists(WideExtractFilePath(TntApplication.ExeName) + 'locale\' + LangId + '\LC_MESSAGES\' + ID +
        '.mo') then
      begin
        LangNames := LangNames + LangName + #13#10;
        LangIds := LangIds + LangId + #13#10;
      end;
    end;
  finally
    Langs.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TApplicationOptionsForm.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  OptionsImgNames := TTntStringList.Create;
  DockOptionPageControl := nil;
  DoPageContentChanged := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TApplicationOptionsForm.Destroy;

begin
  OptionsImgNames.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

