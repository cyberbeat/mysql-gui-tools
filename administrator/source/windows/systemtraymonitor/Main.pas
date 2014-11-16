unit Main;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ShellAPI, TntForms, Menus, TntMenus, ExtCtrls, StdCtrls,
  TntStdCtrls, Registry, PDH, AdminService, Contnrs, AuxFuncs, PNGImage,
  WinSvc, IniFiles, TntClasses, MySQLCommonFuncs, Instance;

const
  // TODO: There should be a central unit collecting all defined message constants to avoid duplication.
  WM_ICONTRAY = WM_USER + 1;
  RSP_SIMPLE_SERVICE = 1;
  RSP_UNREGISTER_SERVICE = 0;

type
  TMainForm = class(TTntForm)
    PopupMenu: TTntPopupMenu;
    CloseMonitorMI: TTntMenuItem;
    Timer: TTimer;
    DisplayCPULoadMI: TTntMenuItem;
    N1: TTntMenuItem;
    MontorOptions1: TTntMenuItem;
    ActionsMI: TTntMenuItem;
    ManageInstancesMI: TTntMenuItem;
    MySQLAdministratorMI: TTntMenuItem;
    MySQLQueryBrowserMI: TTntMenuItem;
    N2: TTntMenuItem;
    StartMonitorAutomaticallyMI: TTntMenuItem;
    OnlyScanServicesStartingWithmysqlMI: TTntMenuItem;
    ScanIntervallMI: TTntMenuItem;
    N1Second1: TTntMenuItem;
    N2Seconds1: TTntMenuItem;
    N5Seconds1: TTntMenuItem;
    N10Sec1: TTntMenuItem;
    N30Sec1: TTntMenuItem;
    N1Minute1: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseMonitorMIClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure DisplayCPULoadMIClick(Sender: TObject);
    procedure DoApplicationDeactivate(Sender: TObject);

    procedure RefreshServiceList;

    procedure MeasureItem(Sender: TObject;
      ACanvas: TCanvas; var Width, Height: Integer);
    procedure MeasureSubMenuCheckItem(Sender: TObject;
      ACanvas: TCanvas; var Width, Height: Integer);

    procedure DrawItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure DrawInstanceStatusItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure CloseMonitorMIDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure DrawSubMenuCheckItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure DrawSubMenuItem(Sender: TObject;
      ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

    procedure ManageInstancesMIClick(Sender: TObject);
    procedure IgnoreInstanceMIClick(Sender: TObject);
    procedure StartStopInstanceMIClick(Sender: TObject);
    procedure ConfigureInstanceMIClick(Sender: TObject);
    procedure MySQLAdministratorMIClick(Sender: TObject);
    procedure MySQLQueryBrowserMIClick(Sender: TObject);

    procedure LoadOptions;
    procedure StoreOptions;
    procedure PopupMenuPopup(Sender: TObject);
    procedure StartMonitorAutomaticallyMIClick(Sender: TObject);
    procedure OnlyScanServicesStartingWithmysqlMIClick(Sender: TObject);
    procedure N1Second1Click(Sender: TObject);
  private
    PdhQuery: HQUERY;
    PdhCounter: HCOUNTER;

    CollectingPerformanceDataStarted: Boolean;
    ShowProcessorLoad: Boolean;
    InstancesInstalled: Boolean;
    AllNeededInstancesRunning: Boolean;
    CurrentProcessorLoad: Double;
    CPULoadPerformanceDataPath: string;

    ServiceList: TObjectList;

    ServiceStartedPNGImg: TPNGObject;
    ServiceStoppedPNGImg: TPNGObject;
    ClosePNGImg: TPNGObject;
    MenuCheckedPNGImg: TPNGObject;
    MenuUncheckedPNGImg: TPNGObject;

    PopupMenuWidth: Integer;

    MenuImageList: TObjectList;

    IgnoreInstances: TStringList;

    PerviousIconName: string;

    ServerRefreshInterval,
    ServerRefreshPos: Cardinal;

    function WindowHook(var Message: TMessage): Boolean;

    procedure StartCollectingPerformanceData;
    procedure StopCollectingPerformanceData;

    function GetIconName: string;
    procedure CreateTrayIcon;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure TrayMessage(var Msg: TMessage); message WM_ICONTRAY;
  end;

function CreateValidComponentName(someNameW: wideString): AnsiString;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  CommonTypes, ComCtrls, PNGTools;

var
  WM_TASKBARCREATED: Cardinal;
  TrayIconData: TNotifyIconDataAIE6;

//----------------------------------------------------------------------------------------------------------------------

function CreateValidComponentName(someNameW: wideString): AnsiString;

var
  ResultString: AnsiString;
  ResultPointer: PChar;
  ResultPointerLen: Integer;
  
begin
  ResultPointerLen := length(someNameW) * 4 + 1;
  GetMem(ResultPointer, ResultPointerLen);
  try
    BinToHex(PChar(Pointer(someNameW)), ResultPointer, Length(someNameW) * 2);
    ResultPointer[ResultPointerLen - 1] := #0;
    ResultString := ResultPointer;
  finally
    FreeMem(ResultPointer);
  end;
  Result := '_' + ResultString;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CollectingPerformanceDataStarted := False;
  ShowProcessorLoad := True;
  CurrentProcessorLoad := 0;
  InstancesInstalled := False;
  AllNeededInstancesRunning := False;

  IgnoreInstances := TStringList.Create;

  ServiceList := TObjectList.Create;

  PerviousIconName:='';
  ServerRefreshInterval:=1000;
  ServerRefreshPos:=0;

  MenuImageList := TObjectList.Create;
  MenuImageList.Add(LoadPNGImageFromResource('MySQLIcon_Admin_16x16'));
  MenuImageList.Add(LoadPNGImageFromResource('MySQLIcon_QueryBrowser_16x16'));

  ServiceStartedPNGImg := LoadPNGImageFromResource('service_started');
  ServiceStoppedPNGImg := LoadPNGImageFromResource('service_stopped');
  ClosePNGImg := LoadPNGImageFromResource('close');
  MenuCheckedPNGImg := LoadPNGImageFromResource('menu_checked');
  MenuUncheckedPNGImg := LoadPNGImageFromResource('menu_unchecked');

  //No button in Taskbar
  ShowWindow(Application.Handle, SW_HIDE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE)
    or WS_EX_TOOLWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);

  LoadOptions;

  // Prepare Tray Icon
  CreateTrayIcon;

  //Get List of services
  RefreshServiceList;

  //Perpare Performance Data display
  StartCollectingPerformanceData;

  MySQLAdministratorMI.Enabled := (GetMySQLAdministratorCmd <> '');
  MySQLQueryBrowserMI.Enabled := (GetMySQLQueryBrowserCmd <> '');

  if (not (CollectingPerformanceDataStarted)) then
    ShowProcessorLoad := False;

  Timer.Enabled := True;

  Application.OnDeactivate := DoApplicationDeactivate;
  WM_TASKBARCREATED := RegisterWindowMessage('TaskbarCreated');
  Application.HookMainWindow(WindowHook);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  Timer.Enabled := False;
  Application.UnhookMainWindow(WindowHook);
  StoreOptions;

  MenuImageList.Free;

  ServiceList.Free;
  ServiceStartedPNGImg.Free;
  ServiceStoppedPNGImg.Free;
  ClosePNGImg.Free;
  MenuCheckedPNGImg.Free;
  MenuUncheckedPNGImg.Free;

  IgnoreInstances.Free;

  Shell_NotifyIcon(NIM_DELETE, @TrayIconData);

  StopCollectingPerformanceData;

  Application.ProcessMessages;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateParams(var Params: TCreateParams);

const
  CS_DROPSHADOW = $00020000;

begin
  inherited;

  if (IsWinXP) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoApplicationDeactivate(Sender: TObject);

begin
end;

//----------------------------------------------------------------------------------------------------------------------

function TMainForm.GetIconName: string;

var
  IconName: WideString;

begin
  IconName := 'STM';

  if (InstancesInstalled) then
  begin
    if (AllNeededInstancesRunning) then
      IconName := IconName + '_STARTED'
    else
      IconName := IconName + '_STOPPED';
  end;

  if (ShowProcessorLoad) then
    IconName := IconName + '_' +
      FormatFloat('0', Round(CurrentProcessorLoad / 100 * 7));

  Result := IconName;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TrayMessage(var Msg: TMessage);

var
  CursorPos: TPoint;

begin
  case Msg.lParam of
    NIN_BALLOONUSERCLICK:
      begin
        // Remove baloon tooltip if
        with TrayIconData do
        begin
          szInfo[0] := #0;
          szInfoTitle[0] := #0;
        end;
        Shell_NotifyIcon(NIM_MODIFY, @TrayIconData);
      end;
    WM_RBUTTONDOWN:
      begin
        SetForegroundWindow(Handle);
        GetCursorPos(CursorPos);
        PopupMenu.Popup(CursorPos.X, CursorPos.Y);
        PostMessage(Handle, WM_NULL, 0, 0);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseMonitorMIClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TimerTimer(Sender: TObject);

var
  CPUU: TPDH_FMT_COUNTERVALUE;
  Icon: HICON;
  IconName: string;

begin
  try
    if (CollectingPerformanceDataStarted) and
      (ShowProcessorLoad) then
    begin
      PdhCollectQueryData(PdhQuery);
      PdhGetFormattedCounterValue(PdhCounter, PDH_FMT_DOUBLE, nil, @CPUU);

      CurrentProcessorLoad := CPUU.doubleValue;
    end;

    ServerRefreshPos:=ServerRefreshPos+Timer.Interval;
    if(ServerRefreshPos>=ServerRefreshInterval)then
    begin
      RefreshServiceList;
      ServerRefreshPos:=0;
    end;

    IconName := GetIconName;
    if (IconName <> PerviousIconName) then
    begin
      PerviousIconName := IconName;

      Icon := LoadImage(hInstance, PChar(IconName), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
      try
        TrayIconData.hIcon := Icon;
        Shell_NotifyIcon(NIM_Modify, @TrayIconData);
      finally
        DestroyIcon(Icon);
      end;
    end;
  except
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StartCollectingPerformanceData;

var
  CounterPath: string;
  
begin
  if (IsWinNT) then
    Exit;

  CounterPath := '';
  if (CPULoadPerformanceDataPath <> '') then
    if (PdhValidatePath(PChar(CPULoadPerformanceDataPath)) = 0) then
      CounterPath := CPULoadPerformanceDataPath;

  if (CounterPath = '') then
  begin
    if (PdhValidatePath('\Processor(_Total)\% Processor Time') = 0) then
      CounterPath := '\Processor(_Total)\% Processor Time'
    else
      if (PdhValidatePath('\Prozessor(_Total)\Prozessorzeit (%)') = 0) then
        CounterPath := '\Prozessor(_Total)\Prozessorzeit (%)';
  end;

  if (CounterPath <> '') then
  begin
    PdhOpenQuery(nil, 0, @PdhQuery);
    PdhAddCounter(PdhQuery, PChar(CounterPath), 0,
      @PdhCounter);
    PdhCollectQueryData(PdhQuery);

    CollectingPerformanceDataStarted := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StopCollectingPerformanceData;

begin
  if (CollectingPerformanceDataStarted) then
  begin
    PdhRemoveCounter(PdhCounter);
    PdhCloseQuery(PdhQuery);

    CollectingPerformanceDataStarted := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DisplayCPULoadMIClick(Sender: TObject);

begin
  ShowProcessorLoad := not (ShowProcessorLoad);
  DisplayCPULoadMI.Checked := ShowProcessorLoad;

  TimerTimer(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.RefreshServiceList;

var
  I, J: Integer;
  InstanceStatusMI,
  StartStopMI: TTntMenuItem;
  MenuItem, SubMenuItem: TTntMenuItem;
  AtLeastOneInstance, Found: Boolean;
  ListedInstances: TTntStringList;
  w, w1, w2: integer;

begin
  ListedInstances := TTntStringList.Create;
  try
    for I := 0 to ServiceList.Count - 1 do
      ListedInstances.Add(TMySQLService(ServiceList[I]).ServiceName);

    ServiceList.Clear;
    ScanForServices(nil, 0, ServiceList,
      OnlyScanServicesStartingWithmysqlMI.Checked);

    InstancesInstalled := ServiceList.Count > 0;

    //Check if all needed instances running
    AllNeededInstancesRunning := True;
    AtLeastOneInstance := False;
    for I := 0 to ServiceList.Count - 1 do
    begin
      if (IgnoreInstances.IndexOf(TMySQLService(ServiceList[I]).ServiceName) = -1) then
        AtLeastOneInstance := True;

      if (TMySQLService(ServiceList[I]).Status <> SERVICE_RUNNING) and
        (IgnoreInstances.IndexOf(TMySQLService(ServiceList[I]).ServiceName) = -1) then
      begin
        AllNeededInstancesRunning := False;
        break;
      end;
    end;

    Canvas.Font.Name := 'Tahoma';
    Canvas.Font.Size := 11;

    //Get maximum text width
    w := GetWideStringTextWidth(Canvas, _('Configure Instance'));
    w2 := GetWideStringTextWidth(Canvas, _('- Running'));
    for I := 0 to ServiceList.Count - 1 do
    begin
      Canvas.Font.Style := [fsBold];

      w1 := GetWideStringTextWidth(Canvas,
        TMySQLService(ServiceList[I]).ServiceName);

      if (w1 + w2 > w) then
        w := w1 + w2;
    end;

    PopupMenuWidth := w + 62;

    if (not (AtLeastOneInstance)) then
      InstancesInstalled := False;

    for I := 0 to ServiceList.Count - 1 do
    begin
      InstanceStatusMI := nil;
      for J := 0 to PopupMenu.Items.Count - 1 do
      begin
        if (CompareText(PopupMenu.Items[J].Name,
          CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_StatusMI') = 0) then
        begin
          InstanceStatusMI := TTntMenuItem(PopupMenu.Items[J]);
          break;
        end;
      end;

      if (InstanceStatusMI = nil) then
      begin
        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_StatusMI';
        if (TMySQLService(ServiceList[I]).Status = SERVICE_RUNNING) then
          MenuItem.Caption := TMySQLService(ServiceList[I]).ServiceName + ' - Running'
        else
          MenuItem.Caption := TMySQLService(ServiceList[I]).ServiceName + ' - Stopped';
        MenuItem.OnDrawItem := DrawInstanceStatusItem;
        MenuItem.OnMeasureItem := MeasureItem;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);
        if (I = 0) then
          MenuItem.Tag := 1;

        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_UnderlineMI';
        MenuItem.Caption := '-';
        MenuItem.OnMeasureItem := MeasureItem;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);

        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_StartStopMI';
        MenuItem.Caption := _('Shutdown Instance');
        MenuItem.OnDrawItem := DrawItem;
        MenuItem.OnMeasureItem := MeasureItem;
        MenuItem.Tag := I;
        MenuItem.OnClick := StartStopInstanceMIClick;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);

        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_ConfigureMI';
        MenuItem.Caption := _('Configure Instance');
        MenuItem.OnDrawItem := DrawItem;
        MenuItem.OnMeasureItem := MeasureItem;
        MenuItem.Tag := I;
        MenuItem.OnClick := ConfigureInstanceMIClick;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);

        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_OptionsMI';
        MenuItem.Caption := _('Monitoring Options');
        MenuItem.OnDrawItem := DrawItem;
        MenuItem.OnMeasureItem := MeasureItem;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);

        SubMenuItem := TTntMenuItem.Create(PopupMenu);
        SubMenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_IgnoreInstanceMI';
        SubMenuItem.Caption := _('Ignore Instance State');
        SubMenuItem.OnDrawItem := DrawSubMenuCheckItem;
        SubMenuItem.OnMeasureItem := MeasureSubMenuCheckItem;
        SubMenuItem.Checked := (IgnoreInstances.IndexOf(TMySQLService(ServiceList[I]).ServiceName) > -1);
        SubMenuItem.Tag := I;
        SubMenuItem.OnClick := IgnoreInstanceMIClick;
        MenuItem.Add(SubMenuItem);

        MenuItem := TTntMenuItem.Create(PopupMenu);
        MenuItem.Name := CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_SepMI';
        MenuItem.Caption := '-';
        MenuItem.OnMeasureItem := MeasureItem;
        PopupMenu.Items.Insert(PopupMenu.Items.IndexOf(MySQLAdministratorMI),
          MenuItem);
      end
      else
      begin
        StartStopMI := nil;
        for J := 0 to PopupMenu.Items.Count - 1 do
        begin
          if (CompareText(PopupMenu.Items[J].Name,
            CreateValidComponentName(TMySQLService(ServiceList[I]).ServiceName) + '_StartStopMI') = 0) then
          begin
            StartStopMI := TTntMenuItem(PopupMenu.Items[J]);
            break;
          end;
        end;

        if (TMySQLService(ServiceList[I]).Status = SERVICE_RUNNING) then
        begin
          InstanceStatusMI.Caption := TMySQLService(ServiceList[I]).ServiceName + _(' - Running');
          if (StartStopMI <> nil) then
            StartStopMI.Caption := _('Shutdown Instance');
        end
        else
        begin
          InstanceStatusMI.Caption := TMySQLService(ServiceList[I]).ServiceName + _(' - Stopped');
          if (StartStopMI <> nil) then
            StartStopMI.Caption := _('Start Instance');
        end;

      end;
    end;

    //Remove Services that have been uninstalled
    for I := 0 to ListedInstances.Count - 1 do
    begin
      Found := False;
      for J := 0 to ServiceList.Count - 1 do
        if (ListedInstances[I] = TMySQLService(ServiceList[J]).ServiceName) then
          Found := True;

      if (not (Found)) then
      begin
        J := 0;
        while (J < PopupMenu.Items.Count) do
        begin
          if (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_StatusMI') or
            (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_UnderlineMI') or
            (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_StartStopMI') or
            (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_ConfigureMI') or
            (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_OptionsMI') or
            (PopupMenu.Items[J].Name = CreateValidComponentName(ListedInstances[I]) + '_SepMI') then
            PopupMenu.Items.Delete(J)
          else
            inc(J);
        end;
      end;
    end;
  finally
    ListedInstances.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

begin
  Width := PopupMenuWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MeasureSubMenuCheckItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

begin
  Width := GetWideStringTextWidth(ACanvas, TTntMenuItem(Sender).Caption) + 20;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

var
  BRect: TRect;

begin
  with ACanvas do
  begin
    if (TTntMenuItem(Sender).ImageIndex >= 0) and
      (TTntMenuItem(Sender).ImageIndex < MenuImageList.Count) then
    begin
      BRect := Rect(7, ARect.Top + 2,
        7 + TPNGObject(MenuImageList[TTntMenuItem(Sender).ImageIndex]).Width,
        ARect.Top + 2 + TPNGObject(MenuImageList[TTntMenuItem(Sender).ImageIndex]).Height);

      Brush.Color := clMenu;
      FillRect(BRect);
      TPNGObject(MenuImageList[TTntMenuItem(Sender).ImageIndex]).Draw(
        ACanvas, BRect);
    end;

    if (Selected) then
      Brush.Color := clHighlight
    else
      Brush.Color := clMenu;

    ARect.Left := 34;
    FillRect(ARect);
    ARect.Left := 38;

    if (TTntMenuItem(Sender).Enabled) then
      if (Selected) then
        Font.Color := clHighlightText
      else
        Font.Color := clMenuText
    else
      Font.Color := clGrayText;

    DrawWideStringText(ACanvas.Handle, PWideChar(TTntMenuItem(Sender).Caption), Length(TTntMenuItem(Sender).Caption),
      ARect, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DrawInstanceStatusItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

begin
  with ACanvas do
  begin
    Brush.Color := clMenu;

    ARect.Left := 34;
    FillRect(ARect);
    ARect.Left := 38;

    Font.Color := clBlack;
    Font.Style := [fsBold];

    DrawWideStringText(ACanvas.Handle,
      PWideChar(TTntMenuItem(Sender).Caption),
      Length(TTntMenuItem(Sender).Caption),
      ARect, DT_LEFT or DT_SINGLELINE or DT_VCENTER);

    ARect.Left := 38 + GetWideStringTextWidth(ACanvas,
      TTntMenuItem(Sender).Caption);

    Font.Style := [];

    if (TTntMenuItem(Sender).Tag = 1) then
      ClosePNGImg.Draw(ACanvas, Rect(ARect.Right - 16,
        4, ARect.Right - 16 + ClosePNGImg.Width,
        4 + ClosePNGImg.Height));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DrawSubMenuCheckItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

begin
  with ACanvas do
  begin
    if (Selected) then
      Brush.Color := clHighlight
    else
      Brush.Color := clMenu;

    FillRect(ARect);
    ARect.Left := 20;

    if (TTntMenuItem(Sender).Enabled) then
      if (Selected) then
        Font.Color := clHighlightText
      else
        Font.Color := clMenuText
    else
      Font.Color := clGrayText;

    if (TTntMenuItem(Sender).Checked) then
      MenuCheckedPNGImg.Draw(ACanvas, Rect(6, 7 + ARect.Top,
        6 + MenuCheckedPNGImg.Width, 7 + ARect.Top + MenuCheckedPNGImg.Height))
    else
      MenuUncheckedPNGImg.Draw(ACanvas, Rect(6, 7 + ARect.Top,
        6 + MenuCheckedPNGImg.Width, 7 + ARect.Top + MenuCheckedPNGImg.Height));

    DrawWideStringText(ACanvas.Handle,
      PWideChar(TTntMenuItem(Sender).Caption),
      Length(TTntMenuItem(Sender).Caption),
      ARect, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DrawSubMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

begin
  with ACanvas do
  begin
    if (Selected) then
      Brush.Color := clHighlight
    else
      Brush.Color := clMenu;

    FillRect(ARect);
    ARect.Left := 20;

    if (TTntMenuItem(Sender).Enabled) then
      if (Selected) then
        Font.Color := clHighlightText
      else
        Font.Color := clMenuText
    else
      Font.Color := clGrayText;

    DrawWideStringText(ACanvas.Handle,
      PWideChar(TTntMenuItem(Sender).Caption),
      Length(TTntMenuItem(Sender).Caption),
      ARect, DT_LEFT or DT_SINGLELINE or DT_VCENTER);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseMonitorMIDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

var
  I: integer;
  
begin
  DrawItem(Sender, ACanvas, ARect, Selected);

  with ACanvas do
  begin
    Brush.Color := clMenu;

    if (ServiceList.Count > 0) then
      ClosePNGImg.Draw(ACanvas, Rect(ARect.Right - 16,
        4, ARect.Right - 16 + ClosePNGImg.Width,
        4 + ClosePNGImg.Height));

    for I := 0 to ServiceList.Count - 1 do
    begin
      FillRect(Rect(1, 91 * I + 1, 34, 91 * (I + 1) - 6));

      if (TMySQLService(ServiceList[I]).Status = SERVICE_RUNNING) then
        ServiceStartedPNGImg.Draw(ACanvas, Rect(4, 91 * I + 3,
          4 + ServiceStartedPNGImg.Width, 91 * I + 3 + ServiceStartedPNGImg.Height))
      else
        ServiceStoppedPNGImg.Draw(ACanvas, Rect(4, 91 * I + 3,
          4 + ServiceStartedPNGImg.Width, 91 * I + 3 + ServiceStartedPNGImg.Height));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ManageInstancesMIClick(Sender: TObject);

begin
  CreateSubProcess('MySQLAdministrator -serviceconfig', '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.IgnoreInstanceMIClick(Sender: TObject);

var
  Index: integer;

begin
  if (TTntMenuItem(Sender).Tag < ServiceList.Count) then
  begin
    Index := IgnoreInstances.IndexOf(TMySQLService(ServiceList[TTntMenuItem(Sender).Tag]).ServiceName);

    if (TTntMenuItem(Sender).Checked) then
    begin
      if (Index > -1) then
        IgnoreInstances.Delete(Index);
    end
    else
    begin
      if (Index = -1) then
        IgnoreInstances.Add(TMySQLService(ServiceList[TTntMenuItem(Sender).Tag]).ServiceName);
    end;

    TTntMenuItem(Sender).Checked := not (TTntMenuItem(Sender).Checked);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StartStopInstanceMIClick(Sender: TObject);

var
  Instance: TMySQLService;

begin
  if (TTntMenuItem(Sender).Tag < ServiceList.Count) then
  begin
    Instance := TMySQLService(ServiceList[TTntMenuItem(Sender).Tag]);

    if (Instance.Status = SERVICE_RUNNING) then
    begin
      if not (ServiceStop('', Instance.ServiceName)) then
        ShowMessage('Could not stop service.');
    end
    else
    begin
      if not (ServiceStart('', Instance.ServiceName)) then
        ShowMessage('Could not start service.');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ConfigureInstanceMIClick(Sender: TObject);

var
  Instance: TMySQLService;

begin
  if (TTntMenuItem(Sender).Tag < ServiceList.Count) then
  begin
    Instance := TMySQLService(ServiceList[TTntMenuItem(Sender).Tag]);

    CreateSubProcess('MySQLAdministrator -serviceconfig -instance=' +
      Instance.ServiceName, '');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLAdministratorMIClick(Sender: TObject);

begin
  CreateSubProcess('MySQLAdministrator', '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLQueryBrowserMIClick(Sender: TObject);

var
  cmd: WideString;

begin
  cmd := GetMySQLQueryBrowserCmd;
  if (cmd <> '') then
    CreateSubProcess(cmd, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.LoadOptions;

var
  theIni: TMemIniFile;
  S: string;
  I: integer;

begin
  theIni := TMemIniFile.Create(
    GetApplicationDataDir + 'MySQL\mysqladmin_systray.ini');
  try
    ShowProcessorLoad := (StrToInt(theIni.ReadString('GeneralSettings', 'ShowProcessorLoad',
      '1')) = 1);

    CPULoadPerformanceDataPath := theIni.ReadString('GeneralSettings',
      'CPULoadPerformanceDataPath', '');

    OnlyScanServicesStartingWithmysqlMI.Checked :=
      (theIni.ReadString('GeneralSettings',
      'OnlyScanServicesStartingWithMYSQL', '1') = '1');

    S := theIni.ReadString('GeneralSettings',
      'ScanInterval', '2');

    for I := 0 to ScanIntervallMI.Count - 1 do
      if (ScanIntervallMI.Items[I].Tag = StrToIntDef(S, 2)) then
      begin
        ScanIntervallMI.Items[I].Checked:=True;
        ServerRefreshInterval:=StrToIntDef(S, 2)*1000;
      end
      else
        ScanIntervallMI.Items[I].Checked := False;

    theIni.ReadSection('IgnoreInstances', IgnoreInstances);
  finally
    theIni.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StoreOptions;

var
  Path: WideString;
  theIni: TMemIniFile;
  i: integer;

begin
  Path := GetApplicationDataDir + 'MySQL\';
  ForceDirectories(Path);
  theIni := TMemIniFile.Create(Path + 'mysqladmin_systray.ini');
  try
    theIni.WriteString('GeneralSettings', 'ShowProcessorLoad', IntToStr(Ord(ShowProcessorLoad)));

    theIni.WriteString('GeneralSettings',
      'CPULoadPerformanceDataPath', CPULoadPerformanceDataPath);

    theIni.WriteString('GeneralSettings',
      'CPULoadPerformanceDataPath',
      IntToStr(Ord(OnlyScanServicesStartingWithmysqlMI.Checked)));

    for i := 0 to ScanIntervallMI.Count - 1 do
      if (ScanIntervallMI.Items[i].Checked) then
      begin
        theIni.WriteString('GeneralSettings',
          'ScanInterval',
          IntToStr(ScanIntervallMI.Items[i].Tag));
      end;

    theIni.EraseSection('IgnoreInstances');
    for i := 0 to IgnoreInstances.Count - 1 do
      theIni.WriteString('IgnoreInstances', IgnoreInstances[i], 'Ignore');

    theIni.UpdateFile;
  finally
    theIni.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PopupMenuPopup(Sender: TObject);

begin
  DisplayCPULoadMI.Enabled := CollectingPerformanceDataStarted;
  DisplayCPULoadMI.Checked := ShowProcessorLoad;

  StartMonitorAutomaticallyMI.Checked := FileExists(
    IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_STARTUP)) +
    'MySQL System Tray Monitor' + '.lnk');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StartMonitorAutomaticallyMIClick(Sender: TObject);

begin
  if (not (StartMonitorAutomaticallyMI.Checked)) then
  begin
    if (not (FileExists(GetSpecialFolder(CSIDL_STARTUP) +
      'MySQL System Tray Monitor' + '.lnk'))) then
    begin
      CreateShortcut('MySQL System Tray Monitor',
        GetSpecialFolder(CSIDL_STARTUP), Application.ExeName,
        ExtractFilePath(Application.ExeName), '');
    end;

    StartMonitorAutomaticallyMI.Checked := True;
  end
  else
  begin
    if (FileExists(IncludeTrailingPathDelimiter(
      GetSpecialFolder(CSIDL_STARTUP)) +
      'MySQL System Tray Monitor' + '.lnk')) then
      DeleteFile(IncludeTrailingPathDelimiter(
        GetSpecialFolder(CSIDL_STARTUP)) +
        'MySQL System Tray Monitor' + '.lnk');

    StartMonitorAutomaticallyMI.Checked := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlyScanServicesStartingWithmysqlMIClick(Sender: TObject);

begin
  OnlyScanServicesStartingWithmysqlMI.Checked := not OnlyScanServicesStartingWithmysqlMI.Checked;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.N1Second1Click(Sender: TObject);

var
  I: integer;

begin
  if Sender is TMenuItem then
  begin
    for I := 0 to ScanIntervallMI.Count - 1 do
      ScanIntervallMI.Items[I].Checked := False;

    TMenuItem(Sender).Checked := True;
    ServerRefreshInterval := TMenuItem(Sender).Tag * 1000;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMainForm.WindowHook(var Message: TMessage): Boolean;

begin
  if Message.Msg = WM_OTHERINSTANCE then
  begin
    if not IsWindow(TrayIconData.hWnd) then
      Shell_NotifyIcon(NIM_DELETE, @TrayIconData);
    Shell_NotifyIcon(NIM_ADD, @TrayIconData);
    Result := True;
  end
  else
    if Message.Msg = WM_TASKBARCREATED then
    begin
      CreateTrayIcon;
      Result := True;
    end
    else
      Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateTrayIcon;

begin
  FillChar(TrayIconData, SizeOf(TrayIconData), 0);
  with TrayIconData do
  begin
    if GetComCtlVersion >= ComCtlVersionIE6 then
      cbSize := SizeOf(TNotifyIconDataAIE6)
    else
      if GetComCtlVersion >= ComCtlVersionIE5 then
        cbSize := SizeOf(TNotifyIconDataAIE5)
      else
        cbSize := SizeOf(TNotifyIconData);
    hWnd := Handle;
    uID := 0;

    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    StrPCopy(szTip, 'MySQL System Tray Monitor');

    uCallbackMessage := WM_ICONTRAY;
    hIcon := LoadImage(hInstance, PChar('STM'), IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR);
  end;

  // Add Tray Icon and set it to new version if on Win 2K or newer.
  if Shell_NotifyIcon(NIM_ADD, @TrayIconData) and IsWin2K then
  begin
    TrayIconData.TimeoutOrVersion := NOTIFYICON_VERSION;
    Shell_NotifyIcon(NIM_SETVERSION, @TrayIconData);
    Shell_NotifyIcon(NIM_SETFOCUS, @TrayIconData);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

