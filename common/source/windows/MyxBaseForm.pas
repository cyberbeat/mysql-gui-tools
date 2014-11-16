unit MyxBaseForm;

interface

uses
  GnuGetText, TntForms, Classes, Grt, TntSysUtils, Forms, AuxFuncs,
  SysUtils, MyxOptions;

type
  TMyxBaseForm = class(TTntForm)
  private
    FWindowPositionRestore: Boolean;
    FWindowPositionRestoreSize: Boolean;
  protected
    function GetGrt: TGrt; virtual;
    procedure RestoreWindowPosition;

    procedure StoreWindowPosition;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; RestoreWindowPos: Boolean; RestoreWindowSize: Boolean); reintroduce; overload; 
    destructor Destroy; override;

    class procedure StoreWindowPositions(FileName: WideString = '');

    property Grt: TGrt read GetGrt;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TMyxBaseForm.Create(AOwner: TComponent);

begin
  Create(AOwner, True, True);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxBaseForm.Create(AOwner: TComponent;  RestoreWindowPos: Boolean; RestoreWindowSize: Boolean);

begin
  inherited Create(AOwner);

  // Make sure GRT is available
  if (Grt = nil) then
    Abort;

  if (AuxFuncs.DefaultFontName = '') then
  begin
    AuxFuncs.DefaultFontName :=
      CommonOptions.OptionString['DefaultFontName'];
    AuxFuncs.DefaultFontHeight :=
      StrToIntDef(CommonOptions.OptionString['DefaultFontHeight'], 8);
  end;

  InitForm(self);

  FWindowPositionRestore := RestoreWindowPos;
  FWindowPositionRestoreSize := RestoreWindowSize;

  RestoreWindowPosition;
end;

// -----------------------------------------------------------------------------

destructor TMyxBaseForm.Destroy;

begin
  inherited;
end;

// -----------------------------------------------------------------------------

function TMyxBaseForm.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

// -----------------------------------------------------------------------------

procedure TMyxBaseForm.RestoreWindowPosition;

var
  PFormPositions,
  PFormPosition: Pointer;
  PosLeft,
  PosTop,
  PosWidth,
  PosHeight: Integer;
  FileName: WideString;

begin
  // make sure the form can be set on the complete desktop
  DefaultMonitor := dmDesktop;

  PFormPositions := Grt.Global['/app/formPositions'];
  PFormPosition := nil;

  if (PFormPositions = nil) then
  begin
    // Make sure /app exists
    GlobalAppDict;

    FileName := CommonOptions.OptionString['UserDataDir'] +
      TntApplication.Title + ' Window Positions.xml';

    if (FileExists(FileName)) then
      PFormPositions := Grt.ValueLoadFromFile(FileName)
    else
      PFormPositions := Grt.DictNewTyped(GrtListValue,
        'forms.Position');

    Grt.Global['/app/formPositions'] := PFormPositions;
  end;

  if (PFormPositions <> nil) then
    PFormPosition := Grt.DictItem[PFormPositions, Name];

  if (PFormPosition <> nil) then
  begin
    PosLeft := Grt.DictInt[PFormPosition, 'left'];
    PosTop := Grt.DictInt[PFormPosition, 'top'];
    PosWidth := Grt.DictInt[PFormPosition, 'width'];
    PosHeight  := Grt.DictInt[PFormPosition, 'height'];

    if FWindowPositionRestoreSize then
    begin
      Width := Max(100, PosWidth);
      Height := Max(40, PosHeight);
    end;

    if FWindowPositionRestore then
    begin
      // Make sure the form is fully visible on the current desktop (or at least most of it).
      if PosLeft + Width > Screen.DesktopWidth then
        PosLeft := Screen.DesktopWidth - Width;
      if PosTop + Height > Screen.DesktopHeight then
        PosTop := Screen.DesktopHeight - Height;
      Left := Max(0, PosLeft);
      Top := Max(0, PosTop);
    end;

    WindowState := TWindowState(Grt.DictInt[PFormPosition, 'state']);
  end
  else
  begin
    if (Screen.Width >= 1024) then
    begin
      Left := (Screen.Width + Width) div 2 - Width;
      Top := (Screen.Height + Height) div 2 - Height;
    end
    else
    begin
      Left := (Screen.Width + Width) div 2 - Width;
      Top := 0;

      WindowState := wsMaximized;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxBaseForm.StoreWindowPosition;

var
  PApp,
    PFormPositions,
    PFormPosition,
    POldFormPos: Pointer;

begin
  PApp := Grt.Global['/app'];
  if (PApp = nil) then
  begin
    PApp := Grt.DictNew('');

    Grt.Global['/app'] := PApp;
  end;

  PFormPositions := Grt.DictItem[PApp, 'formPositions'];
  if (PFormPositions = nil) then
  begin
    PFormPositions :=
      Grt.DictNewTyped(GrtDictValue, 'forms.Position');
  end;

  PFormPosition := Grt.DictNew('forms.Position');
  Grt.DictInt[PFormPosition, 'state'] := Ord(WindowState);
  Grt.DictInt[PFormPosition, 'monitor'] := Monitor.MonitorNum;

  if (WindowState <> wsMaximized) then
  begin
    Grt.DictInt[PFormPosition, 'left'] := Left;
    Grt.DictInt[PFormPosition, 'top'] := Top;
    Grt.DictInt[PFormPosition, 'width'] := Width;
    Grt.DictInt[PFormPosition, 'height'] := Height;
  end
  else
  begin
    POldFormPos := Grt.DictItem[PFormPositions, name];

    // make sure to keep existing pos + size when
    // the form is maximized
    if (POldFormPos <> nil) then
    begin
      Grt.DictInt[PFormPosition, 'left'] :=
        Grt.DictInt[POldFormPos, 'left'];
      Grt.DictInt[PFormPosition, 'top'] :=
        Grt.DictInt[POldFormPos, 'top'];
      Grt.DictInt[PFormPosition, 'width'] :=
        Grt.DictInt[POldFormPos, 'width'];
      Grt.DictInt[PFormPosition, 'height'] :=
        Grt.DictInt[POldFormPos, 'height'];

      if (Grt.DictInt[PFormPosition, 'left'] <
          Screen.Monitors[Monitor.MonitorNum].Left) or
        (Grt.DictInt[PFormPosition, 'left'] >=
          Screen.Monitors[Monitor.MonitorNum].Left +
          Screen.Monitors[Monitor.MonitorNum].Width) then
      begin
        Grt.DictInt[PFormPosition, 'left'] :=
          Screen.Monitors[Monitor.MonitorNum].Left;
        Grt.DictInt[PFormPosition, 'top'] :=
          Screen.Monitors[Monitor.MonitorNum].Top;
      end;
    end;
  end;

  // store position
  Grt.DictItem[PFormPositions, Name] := PFormPosition;
end;

// -----------------------------------------------------------------------------

class procedure TMyxBaseForm.StoreWindowPositions(FileName: WideString);

var
  Grt: TGrt;

var
  I: Integer;

begin
  // Close all application forms, if they can be closed
  for I := 0 to Screen.FormCount - 1 do
    if (Screen.Forms[I] is TMyxBaseForm) then
      TMyxBaseForm(Screen.Forms[I]).StoreWindowPosition;

  Grt := RuntimeEnvironment;

  if (FileName = '') then
    FileName := CommonOptions.OptionString['UserDataDir'] +
      TntApplication.Title + ' Window Positions.xml';

  WideForceDirectories(WideExtractFilePath(FileName));

  Grt.ValueSaveToFile(FileName, Grt.Global['/app/formPositions']);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
