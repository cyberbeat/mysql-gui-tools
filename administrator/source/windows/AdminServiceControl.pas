unit AdminServiceControl;

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
  gnugettext, Windows, Messages, SysUtils, Variants, Classes,
  TntClasses, Graphics, TntControls, TntForms,
  Dialogs, TntExtCtrls, TntStdCtrls, TntButtons, Contnrs,
  Sections, InstanceSections, PNGImage,
  AuxFuncs, ComCtrls, myx_public_interface,
  myx_admin_public_interface, AdminService,
  Registry, WinSvc, Menus, Options, MyxError, ExtCtrls, TntMenus,
  TntComCtrls, StdCtrls, Forms, Buttons, Controls, TntRegistry,
  TntDialogs;

type
  TAdminServiceControlForm = class;

  TStartStopServiceThread = class(TThread)
  private
    FAdminServiceControlForm: TAdminServiceControlForm;
    FStartService: Boolean;
    FServiceName: WideString;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; AdminServiceControlForm: TAdminServiceControlForm;
      StartService: Boolean; ServiceName: WideString); reintroduce;
    destructor Destroy; override;
  end;

  TAdminServiceControlForm = class(TInstanceSectionForm)
    ServiceControlPnl: TTntPanel;
    PageControl: TTntPageControl;
    StartStopServiceSheet: TTabSheet;
    ServiceConfigSheet: TTabSheet;
    SettingsScrollBox: TTntScrollBox;
    ServiceControlCBox: TTntGroupBox;
    LogMessageLbl: TTntLabel;
    StartupLogMemo: TTntMemo;
    SubTreePnl: TTntPanel;
    Panel2: TTntPanel;
    Label15: TTntLabel;
    ServicesTreeView: TTntTreeView;
    Panel1: TTntPanel;
    UserInfoBevel: TTntBevel;
    ServiceNameHeaderLbl: TTntLabel;
    PageHeaderImg: TTntImage;
    UserInfoLbl: TTntLabel;
    PopupMenu: TTntPopupMenu;
    InstallnewServiceMI: TTntMenuItem;
    UninstallselectedServiceMI: TTntMenuItem;
    Panel4: TTntPanel;
    Bevel1: TTntBevel;
    ServiceNameHeader2Lbl: TTntLabel;
    PageHeader2Img: TTntImage;
    Label14: TTntLabel;
    ServiceStatusGBox: TTntGroupBox;
    ServerStatusLbl: TTntLabel;
    Label1: TTntLabel;
    ServiceStatusImg: TTntImage;
    RefreshServiceStatusMI: TTntMenuItem;
    N1: TTntMenuItem;
    StartServiceBtn: TTntBitBtn;
    StopServiceBtn: TTntBitBtn;
    ServiceLabel: TTntLabel;
    ConnectToInstanceAni: TAnimate;
    ServiceSettingsGBox: TTntGroupBox;
    Label44: TTntLabel;
    Label3: TTntLabel;
    Label4: TTntLabel;
    Label5: TTntLabel;
    Label6: TTntLabel;
    ServiceAutoStartCBox: TTntCheckBox;
    ServiceDisplayNameEd: TTntEdit;
    ServiceDescriptionEd: TTntEdit;
    ServiceConfigFileGBox: TTntGroupBox;
    Label7: TTntLabel;
    Label8: TTntLabel;
    Label9: TTntLabel;
    Label10: TTntLabel;
    ConfigFilenameEd: TTntEdit;
    ServiceSectionNameEd: TTntEdit;
    ServiceFeaturesGBox: TTntGroupBox;
    Label57: TTntLabel;
    Label58: TTntLabel;
    Label62: TTntLabel;
    Label11: TTntLabel;
    PathToBinaryEd: TTntEdit;
    DebugCheckbox: TTntCheckBox;
    NamedPipeCheckbox: TTntCheckBox;
    BDBCheckbox: TTntCheckBox;
    PathToBinaryBrowseBtn: TTntBitBtn;
    Panel3: TTntPanel;
    InstallServiceBtn: TTntButton;
    UninstallServiceBtn: TTntButton;
    ApplyChangesBtn: TTntButton;
    DiscardChangesBtn: TTntButton;
    LocalhostOnlyPnl: TTntPanel;
    LocalhostWarningLbl: TTntLabel;
    ServiceCheckerTmr: TTimer;
    TntLabel1: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StopServiceBtnClick(Sender: TObject);
    procedure StartServiceBtnClick(Sender: TObject);

    procedure UpdateLogMemo(ServerStarted: Boolean);
    procedure DoThreadTerminatedStartServiceAction;
    procedure DoThreadTerminatedStopServiceAction;
    procedure ThreadTerminated(Sender: TObject);

    procedure EnableDisableServiceControls(Enable: Boolean);

    procedure DoPageContentChanged(Sender: TObject);

    procedure SetCurrentService(MySQLService: TMySQLService);

    procedure ServiceFeatureClicked(Sender: TObject);
    procedure ValidateFileName(Sender: TObject);

    procedure ApplyChanges;
    procedure DiscardChanges;
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DiscardChangesBtnClick(Sender: TObject);

    procedure CheckUniqueServiceName(ServiceName,
      DisplayName: WideString);
    procedure RefreshServiceList;
    procedure ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);

    procedure InstallnewServiceMIClick(Sender: TObject);
    procedure UninstallselectedServiceMIClick(Sender: TObject);

    procedure RefreshServiceStatusMIClick(Sender: TObject);
    procedure RefreshServiceStatus;
    procedure PathToBinaryEdChange(Sender: TObject);
    procedure PathToBinaryBrowseBtnClick(Sender: TObject);

    procedure SetServerBinary(PathToBinary: WideString; ServerType: WideString);
    procedure PathToBinaryEdExit(Sender: TObject);
    procedure ConfigFilenameEdChange(Sender: TObject);
    procedure ServiceCheckerTmrTimer(Sender: TObject);
    procedure ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  private
    FStartStopServiceThread: TStartStopServiceThread;
    FStartStopServiceThreadHandle: THandle;
    FStartStopServiceRunning: Boolean;

    FServiceStatusRunningPNGImg: TPNGObject;
    FServiceStatusStoppedPNGImg: TPNGObject;
    FServiceConfigPNGImg: TPNGObject;
    FServiceStatusImgPicture: TPNGObject;

    FPageContentChanged: Boolean;

    FLastServiceStatus: Integer;
    
    // Time range to look for in the event log (the time need to start/stop a service).
    FStartTime: TDateTime;

    procedure AddLogEntry(const S: string);
    function IsValidBinary(FileName: WideString): Boolean;
    procedure ChangeCurrentService(ServiceName: WideString = '');
  protected
    procedure ActivateSection; override;
  end;

var
  AdminServiceControlForm: TAdminServiceControlForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  DateUtils,
  ApplicationDataModule, ConnectToInstance, MySQLConnection, PNGTools,
  Main;

{$R *.dfm}

const
  // Various binary names. Note: all servers have InnoDB built-in.
  ServerBinaryDebugOld = 'mysqld';              // Named used before version 4.1.2
  ServerBinaryStandardOld = 'mysqld-opt';       // Named used before version 4.1.2
  ServerBinaryDebug = 'mysqld-debug';
  ServerBinaryStandard = 'mysqld';
  ServerBinaryNamedPipes = 'mysqld-nt';         // This is the only server in the essential package.
  ServerBinaryBdb = 'mysqld-max';
  ServerBinaryNamedPipesBdb = 'mysqld-max-nt';

  // Taken from winnt.h
  EVENTLOG_SEQUENTIAL_READ = $0001;
  EVENTLOG_SEEK_READ       = $0002;
  EVENTLOG_FORWARDS_READ   = $0004;
  EVENTLOG_BACKWARDS_READ  = $0008;
  
type
  // Taken from winnt.h
  PEventLogRecord = ^TEventLogRecord;
  TEventLogRecord = record
    Length: DWORD;                // Length of full record
    Reserved: DWORD;              // Used by the service
    RecordNumber: DWORD;          // Absolute record number
    TimeGenerated: DWORD;         // Seconds since 1-1-1970
    TimeWritten: DWORD;           // Seconds since 1-1-1970
    EventID: DWORD;
    EventType: Word;
    NumStrings: Word;
    EventCategory: Word;
    ReservedFlags: Word;          // For use with paired events (auditing)
    ClosingRecordNumber: DWORD;   // For use with paired events (auditing)
    StringOffset: DWORD;          // Offset from beginning of record
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength: DWORD;
    DataOffset: DWORD;            // Offset from beginning of record
    //
    // Then follow:
    //
    // WCHAR SourceName[]
    // WCHAR Computername[]
    // SID   UserSid
    // WCHAR Strings[]
    // BYTE  Data[]
    // CHAR  Pad[]
    // DWORD Length;
    //
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.FormCreate(Sender: TObject);

begin
  InitForm(Self);

  DockedPanel := ServiceControlPnl;
  SubTreePanel := SubTreePnl;

  FLastServiceStatus := -1;
  PageControl.ActivePageIndex := 0;
  FStartStopServiceRunning := False;
  ConnectToInstanceAni.ResName := 'progress_indicator';

  if not (MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections) then
  begin
    FServiceConfigPNGImg := LoadPNGImageFromResource('service_config_gray', PageHeaderImg);
    DisableEnableControls(StartStopServiceSheet, False);
    DisableEnableControls(ServiceConfigSheet, False);
    FServiceStatusRunningPNGImg := LoadPNGImageFromResource('service_status_running_gray');
  end
  else
  begin
    FServiceConfigPNGImg := LoadPNGImageFromResource('service_config', PageHeaderImg);
    FServiceStatusRunningPNGImg := LoadPNGImageFromResource('service_status_running');
  end;

  FServiceStatusStoppedPNGImg := LoadPNGImageFromResource('service_status_stopped');
  PageHeader2Img.Picture.Assign(PageHeaderImg.Picture);

  ServiceStatusImg.Picture.Graphic := nil;
  FServiceStatusImgPicture := nil;

  if (MYXCommonOptions.XPStyleEnabled) then
    SettingsScrollBox.Color := clWhite;

  InstallServiceBtn.Visible := ApplicationDM.Options.ShowOnlyServiceSections;
  UninstallServiceBtn.Visible := ApplicationDM.Options.ShowOnlyServiceSections;

  //Get all MySQL Services
  if not (MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections) then
    LocalhostOnlyPnl.Show;

  ServiceCheckerTmr.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.FormDestroy(Sender: TObject);

begin
  if FStartStopServiceRunning then
  begin
    TerminateThread(FStartStopServiceThreadHandle, 0);
    FStartStopServiceThread.Free;
    FStartStopServiceThread := nil;
  end;

  FServiceConfigPNGImg.Free;
  FServiceStatusRunningPNGImg.Free;
  FServiceStatusStoppedPNGImg.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.StartServiceBtnClick(Sender: TObject);

begin
  if Assigned(ApplicationDM.CurrentService) then
  begin
    ServiceCheckerTmr.Enabled := False;

    if (ApplicationDM.CurrentService.ErrorLog <> '') then
      ApplicationDM.CurrentService.logfile_size := GetFileSize(ApplicationDM.CurrentService.ErrorLog);

    AddLogEntry('Trying to start the server ...');
    StartServiceBtn.Enabled := False;
    Application.ProcessMessages;

    FStartTime := Now;

    // Give it a few milliseconds to start, in order to log events correctly after the start time.
    // It appears that (maybe because of rounding) it can happen that the first event is sharp before the
    // starting time.
    Sleep(1000);
    
    FStartStopServiceThread := TStartStopServiceThread.Create(True, Self, True, ApplicationDM.CurrentService.ServiceName);
    try
      FStartStopServiceThreadHandle := FStartStopServiceThread.Handle;
      FStartStopServiceThread.Priority := tpNormal;

      FStartStopServiceThread.OnTerminate := ThreadTerminated;
      FStartStopServiceThread.FreeOnTerminate := True;

      FStartStopServiceThread.Resume;
    except
      FStartStopServiceThread.Free;

      raise;
    end;

    //Show animation
    ConnectToInstanceAni.Visible := True;
    ConnectToInstanceAni.Active := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.DoThreadTerminatedStartServiceAction;

begin
  Sleep(1000);

  if not ApplicationDM.Options.ShowOnlyServiceSections then
    if not MySQLConn.Reconnect then
      AddLogEntry(_('Could not re-connect to the MySQL Server.'));

  UpdateLogMemo(True);

  ConnectToInstanceAni.Visible := False;
  ConnectToInstanceAni.Active := False;

  StartServiceBtn.Enabled := True;

  ServiceCheckerTmr.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

// Handle Server stop request

procedure TAdminServiceControlForm.StopServiceBtnClick(Sender: TObject);

begin
  if (ApplicationDM.CurrentService <> nil) then
  begin
    ServiceCheckerTmr.Enabled := False;

    if (ApplicationDM.CurrentService.ErrorLog <> '') then
      ApplicationDM.CurrentService.logfile_size := GetFileSize(ApplicationDM.CurrentService.ErrorLog);

    AddLogEntry('Trying to stop the server ...');
    StopServiceBtn.Enabled := False;
    Application.ProcessMessages;

    FStartTime := Now;
    Sleep(1000);

    FStartStopServiceThread := TStartStopServiceThread.Create(True, Self, False, ApplicationDM.CurrentService.ServiceName);
    try
      FStartStopServiceThreadHandle := FStartStopServiceThread.Handle;
      FStartStopServiceThread.Priority := tpNormal;

      FStartStopServiceThread.OnTerminate := ThreadTerminated;
      FStartStopServiceThread.FreeOnTerminate := True;

      FStartStopServiceThread.Resume;
    except
      FStartStopServiceThread.Free;

      raise;
    end;

    //Show animation
    ConnectToInstanceAni.Visible := True;
    ConnectToInstanceAni.Active := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.DoThreadTerminatedStopServiceAction;

begin
  Sleep(1000);
  UpdateLogMemo(False);

  ConnectToInstanceAni.Visible := False;
  ConnectToInstanceAni.Active := False;

  StopServiceBtn.Enabled := True;

  ServiceCheckerTmr.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.UpdateLogMemo(ServerStarted: Boolean);

var
  new_logfile_size: longword;
  s: TTntStringList;
  fs: TTntFileStream;
  ActionResult: Integer;

  Buffer: Pointer;
  Event: PEventLogRecord;
  BytesRead: DWORD;
  BytesNeeded: DWORD;
  EventHandle: DWORD;
  EventTime: TDateTime;
  Source: PWideChar;
  Strings: PWideChar;

begin
  if Assigned(ApplicationDM.CurrentService) then
  begin
    // First give info whether the service could be started or not.
    if ServerStarted then
    begin
      // Check if service really running
      ActionResult := ServiceStatus('', ApplicationDM.CurrentService.ServiceName);

      if (ActionResult = SERVICE_RUNNING) then
        AddLogEntry('Server was started.'#13#10)
      else
        if (ActionResult = -1) then
          AddLogEntry(Format('Server state could not be fetched (%s [%d]).'#13#10,
            [SysErrorMessage(GetLastError), GetLastError]))
        else
          StartupLogMemo.Lines.Add('Server could not be started.'#13#10);
    end
    else
    begin
      //Check if service really stopped
      ActionResult := ServiceStatus('', ApplicationDM.CurrentService.ServiceName);

      if (ActionResult = SERVICE_STOPPED) then
        AddLogEntry('Server was stopped.'#13#10)
      else
        if (ActionResult = -1) then
          AddLogEntry(Format('Server state could not be fetched (%s [%d]).'#13#10,
            [SysErrorMessage(GetLastError), GetLastError]))
        else
          AddLogEntry('Server could not be stopped.'#13#10);
    end;

    // Good opportunity to update the start and stop buttons.
    if ActionResult = SERVICE_RUNNING then
    begin
      StopServiceBtn.Visible := True;
      StartServiceBtn.Visible := False;
    end
    else
    begin
      StopServiceBtn.Visible := False;
      StartServiceBtn.Visible := True;
    end;

    // Check if there is a log file and display what changed if so.
    new_logfile_size := ApplicationDM.CurrentService.logfile_size;
    if (ApplicationDM.CurrentService.ErrorLog <> '') then
      new_logfile_size := GetFileSize(ApplicationDM.CurrentService.ErrorLog);

    if FileExists(ApplicationDM.CurrentService.ErrorLog) then
    begin
      if (new_logfile_size <> ApplicationDM.CurrentService.logfile_size) then
      begin
        try
          fs := TTntFileStream.Create(ApplicationDM.CurrentService.ErrorLog, fmOpenRead or fmShareDenyNone);
          s := TTntStringList.Create;
          try
            fs.Seek(ApplicationDM.CurrentService.logfile_size, soFromBeginning);
            s.LoadFromStream(fs);
            AddLogEntry(s.Text);
          finally
            s.Free;
            fs.Free;
          end;
        except
        end;
      end;
    end
    else
    begin
      // If there is no log file read the application event log from Windows.
      Buffer := AllocMem(10000);
      try
        Event := Buffer;
        EventHandle := OpenEventLog(nil, 'Application');
        if EventHandle <> 0 then
        begin
          while ReadEventLogW(EventHandle, EVENTLOG_FORWARDS_READ or EVENTLOG_SEQUENTIAL_READ, 0, Buffer, 10000,
            BytesRead, BytesNeeded) do
          begin
            while BytesRead > 0 do
            begin
              // Extract content of event. This is a bit tricky as it contains variable sized info.
              // Consider only entries within the time frame given by FStartTime and FEndTime.
              EventTime := UTCToLocalTime(UnixToDateTime(Event.TimeGenerated));
              if FStartTime <= EventTime then
              begin
                Source := Pointer(Cardinal(@Event.DataOffset) + SizeOf(DWORD));
                if WideSameText(Source, 'MySQL') then
                begin
                  Strings := Pointer(Cardinal(Event) + Event.StringOffset);
                  AddLogEntry(Strings);
                end;
              end;
              
              Dec(BytesRead, Event.Length);
              Event := Pointer(Cardinal(Event) + Event.Length);
            end;

            // Reset start event position to the buffer itself.
            Event := Buffer;
          end;

          CloseEventLog(EventHandle);
        end;
      finally
        FreeMem(Buffer);
      end;
    end;

    RefreshServiceStatus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TStartStopServiceThread.Create(CreateSuspended: Boolean; AdminServiceControlForm: TAdminServiceControlForm;
  StartService: Boolean; ServiceName: WideString);

begin
  inherited Create(CreateSuspended);

  FStartService := StartService;
  FServiceName := ServiceName;
  FAdminServiceControlForm := AdminServiceControlForm;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TStartStopServiceThread.Destroy;

begin
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStartStopServiceThread.Execute;

begin
  if (FStartService) then
    ServiceStart('', FServiceName)
  else
    ServiceStop('', FServiceName);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ThreadTerminated(Sender: TObject);

begin
  if (TStartStopServiceThread(Sender).FStartService) then
    TStartStopServiceThread(Sender).Synchronize(DoThreadTerminatedStartServiceAction)
  else
    TStartStopServiceThread(Sender).Synchronize(DoThreadTerminatedStopServiceAction);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.RefreshServiceStatus;

var
  I: integer;

begin
  if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
  begin

    //If there are no Items in the ServicesTreeView, refresh it
    if (ServicesTreeView.Items.Count = 0) then
      RefreshServiceList;

    for I := 0 to ServicesTreeView.Items.Count - 1 do
    begin
      if (ServicesTreeView.Items[I].Data <> nil) then
      begin
        TMySQLService(ServicesTreeView.Items[I].Data).Status := ServiceStatus('',
          TMySQLService(ServicesTreeView.Items[I].Data).ServiceName);

        ServicesTreeView.Items[I].ImageIndex := ServicesImageIndex +
          Ord(TMySQLService(ServicesTreeView.Items[I].Data).Status <> SERVICE_RUNNING);
        ServicesTreeView.Items[I].SelectedIndex := ServicesTreeView.Items[I].ImageIndex;
      end;
    end;

    SetCurrentService(ApplicationDM.CurrentService);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.EnableDisableServiceControls(Enable: Boolean);

begin
  StopServiceBtn.Enabled := Enable;

  StartServiceBtn.Enabled := Enable;
  LogMessageLbl.Enabled := Enable;
  StartupLogMemo.Enabled := Enable;

  DisableEnableControls(ServiceStatusGBox, Enable);
  DisableEnableControls(ServiceControlCBox, Enable);

  DisableEnableControls(ServiceSettingsGBox, Enable);
  DisableEnableControls(ServiceConfigFileGBox, Enable);
  DisableEnableControls(ServiceFeaturesGBox, Enable);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.DoPageContentChanged(Sender: TObject);

begin
  if (not (InitControls)) then
  begin
    FPageContentChanged := True;

    ApplyChangesBtn.Enabled := True;
    DiscardChangesBtn.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ServiceFeatureClicked(Sender: TObject);

var
  IsOldServer: (yes, no, unknown);

begin
  // Names were changed between 4.1.2 and 4.1.4 so we have to check what version we are managing.
  // Since this must work also with non-active servers we have to do some guesswork.
  // If we are connected however then we know for sure.
  IsOldServer := unknown;
  if MySQLConn.Connected and MySQLConn.IsLocalServer then
    if mysql_full_version_is_later_or_equal_than(MySQLConn.MySQL, 4, 1, 4) = 0 then
      IsOldServer := yes
    else
      IsOldServer := no
  else
  begin
    // Keep the order how this is checked. Binaries weren't renamed in one step.
    // It can be (for version 4.1.2 + 3) that there is the old -opt and the new -debug binary.
    if IsValidBinary(ApplicationDM.CurrentService.PathToBinary + ServerBinaryDebug) then
      IsOldServer := no;
    if IsValidBinary(ApplicationDM.CurrentService.PathToBinary + ServerBinaryStandardOld) then
      IsOldServer := yes;
  end;

  if Sender = NamedPipeCheckBox then
    DebugCheckbox.Enabled := not NamedPipeCheckBox.Checked
  else
    if Sender = DebugCheckbox then
      NamedPipeCheckBox.Enabled := not DebugCheckbox.Checked;

  if DebugCheckbox.Enabled and DebugCheckbox.Checked then
    BDBCheckbox.Checked := True;
  
  if (ApplicationDM.CurrentService <> nil) and not InitControls then
  begin
    if DebugCheckbox.Checked then
    begin
      if IsOldServer = yes then
        ApplicationDM.CurrentService.Binary := ServerBinaryDebugOld
      else
        ApplicationDM.CurrentService.Binary := ServerBinaryDebug;
    end
    else
      if NamedPipeCheckbox.Checked then
      begin
        if BDBCheckbox.Checked then
          ApplicationDM.CurrentService.Binary := ServerBinaryNamedPipesBdb
        else
          ApplicationDM.CurrentService.Binary := ServerBinaryNamedPipes;
      end
      else
        if BDBCheckbox.Checked then
          ApplicationDM.CurrentService.Binary := ServerBinaryBdb
        else
        begin
          if IsOldServer = no then
            ApplicationDM.CurrentService.Binary := ServerBinaryStandard
          else
            ApplicationDM.CurrentService.Binary := ServerBinaryStandardOld;
        end;

    PathToBinaryEd.Text := ApplicationDM.CurrentService.PathToBinary + ApplicationDM.CurrentService.Binary;
    ValidateFileName(PathToBinaryEd);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminServiceControlForm.IsValidBinary(FileName: WideString): Boolean;

begin
  Result := FileExists(FileName) or FileExists(FileName + '.exe');
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ValidateFileName(Sender: TObject);

begin
  if Sender is TTntEdit then
  begin
    if not IsValidBinary(TTntEdit(Sender).Text) then
      TTntEdit(Sender).Font.Color := clRed
    else
      TTntEdit(Sender).Font.Color := clWindowText;

    DoPageContentChanged(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ActivateSection;

begin
  inherited;
  
  ChangeCurrentService;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ChangeCurrentService(ServiceName: WideString);

// Changes internally to either the given service or the one currently stored in the datamodule.

var
  I: Integer;

begin
  // Activate the last selected service or the first one if none was before.
  if ServicesTreeView.Items.Count > 0 then
  begin
    if (ServiceName = '') and Assigned(ApplicationDM.CurrentService) then
      ServiceName := ApplicationDM.CurrentService.ServiceName;
    I := SelectService(ServicesTreeview, ServiceName);
    if I = -1 then
      I := 0;
    SetCurrentService(ServicesTreeView.Items[I].Data);
    ServicesTreeView.Selected := ServicesTreeView.Items[I];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.AddLogEntry(const S: string);

var
  I: Integer;

// Adds a string to the log memo. The string might contain line breaks, which requires to split the input before logging.

begin
  with TStringList.Create do
  try
    Text := S;
    for I := 0 to Count - 1 do
      StartupLogMemo.Lines.Add(Strings[I]);
  finally
    Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ApplyChanges;

var
  Key: HKEY;
  Command: WideString;
  SCMHandle,
  ServiceHandle: SC_Handle;

begin
  if (FPageContentChanged) and (ApplicationDM.CurrentService <> nil) then
  begin
    if not IsValidBinary(ApplicationDM.CurrentService.PathToBinary + ApplicationDM.CurrentService.Binary) then
      if (ShowModalDialog(_('File not found'),
        _('The path to binary points to a non existing file. Please note that ' +
        'the Essential Package only includes the most common used option. For a ' +
        'full installation please choose a different package.'),
        myx_mtError, _('Continue') + #13#10 + _('Abort')) = 2) then
        Exit;

    //Apply changes to ApplicationDM.CurrentService
    ApplicationDM.CurrentService.DisplayName := ServiceDisplayNameEd.Text;
    ApplicationDM.CurrentService.Description := ServiceDescriptionEd.Text;
    ApplicationDM.CurrentService.StartType := Ord(not (ServiceAutoStartCBox.Checked)) + 2;
    ApplicationDM.CurrentService.ConfigFile := ConfigFilenameEd.Text;
    ApplicationDM.CurrentService.ConfigFileSection := ServiceSectionNameEd.Text;

    // For existing services update the registry.
    ApplicationDM.CurrentService.PathToBinary := IncludeTrailingPathDelimiter(ApplicationDM.CurrentService.PathToBinary);

    Command := '"' + ApplicationDM.CurrentService.PathToBinary + ApplicationDM.CurrentService.Binary + '" ' +
      '--defaults-file="' + ApplicationDM.CurrentService.ConfigFile + '" "' + ApplicationDM.CurrentService.ServiceName + '"';
    if ApplicationDM.CurrentService.ExistingService then
    begin
      if (RegCreateKeyW(HKEY_LOCAL_MACHINE, PWideChar('SYSTEM\' + 'CurrentControlSet\Services\' +
        ApplicationDM.CurrentService.ServiceName), Key) = ERROR_SUCCESS) then
      try
        RegSetValueEx(Key, 'Start', 0, REG_DWORD, @ApplicationDM.CurrentService.StartType, 4);
        RegSetValueExW(Key, 'DisplayName', 0, REG_SZ, PWideChar(ApplicationDM.CurrentService.DisplayName),
          Length(ApplicationDM.CurrentService.DisplayName) * 2 + 2);
        RegSetValueExW(Key, 'Description', 0, REG_SZ, PWideChar(ApplicationDM.CurrentService.Description),
          Length(ApplicationDM.CurrentService.Description) * 2 + 2);

        RegSetValueExW(Key, 'ImagePath', 0, REG_EXPAND_SZ, PWideChar(Command), Length(Command) * 2 + 2);
      finally
        RegCloseKey(Key);
      end;
    end
    else
    begin
      // For new services use Service Manager
      CheckUniqueServiceName(ApplicationDM.CurrentService.ServiceName, ApplicationDM.CurrentService.DisplayName);

      SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
      if SCMHandle > 0 then
      begin
        try
          //Create new service
          ServiceHandle := CreateServiceW(SCMHandle,
            PWideChar(ApplicationDM.CurrentService.ServiceName), //lpServiceName,
            PWideChar(ApplicationDM.CurrentService.DisplayName), //lpDisplayName,
            SERVICE_ALL_ACCESS, //dwDesiredAccess,
            SERVICE_WIN32_OWN_PROCESS, //dwServiceType,
            Ord(not (ServiceAutoStartCBox.Checked)) + 2, //dwStartType,
            SERVICE_ERROR_NORMAL, //dwErrorControl,
            PWideChar(Command),
            nil, //lpLoadOrderGroup,
            nil, //lpdwTagId,
            nil, //lpDependencies,
            nil, //lpServiceStartName,
            nil); //lpPassword

          if (ServiceHandle = 0) then
            raise EMyxSystemError.Create('Cannot create new service.', GetLastError);

          CloseServiceHandle(ServiceHandle);

          //Set Description
          if (RegCreateKeyW(HKEY_LOCAL_MACHINE, PWideChar('SYSTEM\' + 'CurrentControlSet\Services\' +
            ApplicationDM.CurrentService.ServiceName), Key) = ERROR_SUCCESS) then
          begin
            try
              RegSetValueExW(Key, 'Description', 0, REG_SZ, PWideChar(ApplicationDM.CurrentService.Description),
                Length(ApplicationDM.CurrentService.Description) + 2);
            finally
              RegCloseKey(Key);
            end;
          end;

          ApplicationDM.CurrentService.ExistingService := True;
        finally
          CloseServiceHandle(SCMHandle);
        end;
      end
      else
        raise EMyxSystemError.Create('Could not connect to the Service Control Manager.', GetLastError);
    end;
  end;

  FPageContentChanged := False;
  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;

  InstallServiceBtn.Enabled := True;
  UninstallServiceBtn.Enabled := (ApplicationDM.CurrentService <> nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.DiscardChanges;

begin
  if FPageContentChanged and Assigned(ApplicationDM.CurrentService) then
  begin
    if ApplicationDM.CurrentService.ExistingService then
      SetCurrentService(ApplicationDM.CurrentService)
    else
      RefreshServiceList;
  end;

  FPageContentChanged := False;
  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;

  InstallServiceBtn.Enabled := True;
  UninstallServiceBtn.Enabled := (ApplicationDM.CurrentService <> nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.SetCurrentService(MySQLService: TMySQLService);

begin
  if (ApplicationDM.CurrentService <> MySQLService) or (Assigned(MySQLService) and (FLastServiceStatus <> MySQLService.Status)) then
  begin
    ApplicationDM.CurrentService := MySQLService;
    if Assigned(ApplicationDM.CurrentService) then
      FLastServiceStatus := MySQLService.Status
    else
      FLastServiceStatus := 0;

    if Assigned(ApplicationDM.CurrentService) then
    begin
      EnableDisableServiceControls(True);

      ServiceNameHeaderLbl.Caption := ApplicationDM.CurrentService.ServiceName;
      ServiceNameHeader2Lbl.Caption := ApplicationDM.CurrentService.ServiceName;

      case ApplicationDM.CurrentService.Status of
        SERVICE_RUNNING:
          begin
            ServerStatusLbl.Caption := ApplicationDM.CurrentService.ServiceName + ' ' + _('Service is running.');
            if (FServiceStatusImgPicture <> FServiceStatusRunningPNGImg) then
            begin
              ServiceStatusImg.Picture.Graphic := FServiceStatusRunningPNGImg;
              FServiceStatusImgPicture := FServiceStatusRunningPNGImg;
            end;

            ServiceLabel.Caption := _('To shut down the instance click the Stop Service button. Be aware that all users ' +
              'connected to the database will be disconnected.');

            StartServiceBtn.Visible := False;
            StopServiceBtn.Visible := True;
          end;
        SERVICE_STOPPED:
          begin
            ServerStatusLbl.Caption := ApplicationDM.CurrentService.ServiceName + ' ' + _('Service is stopped.');
            if (FServiceStatusImgPicture <> FServiceStatusStoppedPNGImg) then
            begin
              ServiceStatusImg.Picture.Graphic := FServiceStatusStoppedPNGImg;
              FServiceStatusImgPicture := FServiceStatusStoppedPNGImg;
            end;

            ServiceLabel.Caption := _('Click the Start Service button to start this service. If the service cannot be ' +
              'started please check the Log Messages.');

            StartServiceBtn.Visible := True;
            StopServiceBtn.Visible := False;
          end;
      else
        StartServiceBtn.Enabled := False;
        StopServiceBtn.Enabled := False;
      end;

      InitControls := True;
      try
        ServiceAutoStartCBox.Checked := ApplicationDM.CurrentService.StartType = 2;

        ServiceDisplayNameEd.Text := ApplicationDM.CurrentService.DisplayName;
        ServiceDescriptionEd.Text := ApplicationDM.CurrentService.Description;

        ConfigFilenameEd.Text := ApplicationDM.CurrentService.ConfigFile;
        ServiceSectionNameEd.Text := ApplicationDM.CurrentService.ConfigFileSection;

        PathToBinaryEd.Text := ApplicationDM.CurrentService.PathToBinary + ApplicationDM.CurrentService.Binary;

        SetServerBinary(ApplicationDM.CurrentService.PathToBinary, ApplicationDM.CurrentService.Binary);

        UninstallServiceBtn.Enabled := True;
      finally
        InitControls := False;
      end;
    end
    else
    begin
      EnableDisableServiceControls(False);

      ServiceDisplayNameEd.Text := '';
      ServiceDescriptionEd.Text := '';
      ConfigFilenameEd.Text := '';
      ServiceSectionNameEd.Text := '';
      PathToBinaryEd.Text := '';

      ServerStatusLbl.Caption := _('No Service selected.');
      ServiceStatusImg.Picture.Graphic := nil;
      FServiceStatusImgPicture := nil;

      UninstallServiceBtn.Enabled := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.SetServerBinary(PathToBinary: WideString; ServerType: WideString);

var
  IsOldServer: (yes, no, unknown);

begin
  if Assigned(ApplicationDM.CurrentService) then
  begin
    // Names were changed between 4.1.2 and 4.1.4 so we have to check what version we are managing.
    // Since this must work also with non-active servers we have to do some guesswork.
    // If we are connected however then we know for sure.
    IsOldServer := unknown;
    if MySQLConn.Connected and MySQLConn.IsLocalServer then
      if mysql_full_version_is_later_or_equal_than(MySQLConn.MySQL, 4, 1, 4) = 0 then
        IsOldServer := yes
      else
        IsOldServer := no
    else
    begin
      // Keep the order how this is checked. Binaries weren't renamed in one step.
      // It can be (for version 4.1.2 + 3) that there is the old -opt and the new -debug binary.
      if IsValidBinary(ApplicationDM.CurrentService.PathToBinary + ServerBinaryDebug) then
        IsOldServer := no;
      if IsValidBinary(ApplicationDM.CurrentService.PathToBinary + ServerBinaryStandardOld) then
        IsOldServer := yes;
    end;

    InitControls := True;
    try
      BDBCheckbox.Checked := False;
      NamedPipeCheckbox.Checked := False;
      NamedPipeCheckbox.Enabled := True;
      DebugCheckbox.Checked := False;
      DebugCheckbox.Enabled := True;

      ApplicationDM.CurrentService.PathToBinary := PathToBinary;
      ApplicationDM.CurrentService.Binary := ServerType;

      // Derive supported features from the binary name.
      if ((IsOldServer = yes) and WideSameText(ApplicationDM.CurrentService.Binary, ServerBinaryDebugOld)) or
        WideSameText(ApplicationDM.CurrentService.Binary, ServerBinaryDebug) then
      begin
        // Debug binary.
        DebugCheckbox.Checked := True;
        NamedPipeCheckbox.Enabled := False;
      end
      else
        if WideSameText(ApplicationDM.CurrentService.Binary, ServerBinaryNamedPipes) then
        begin
          // Optimized binary with named pipes support.
          NamedPipeCheckbox.Checked := True;
          DebugCheckbox.Enabled := False;
        end
        else
          if WideSameText(ApplicationDM.CurrentService.Binary, ServerBinaryBdb) then
            BDBCheckbox.Checked := True
          else
            if WideSameText(ApplicationDM.CurrentService.Binary, ServerBinaryNamedPipesBdb) then
            begin
              // Optimized binary with named pipes and BDB support.
              NamedPipeCheckbox.Checked := True;
              BDBCheckbox.Checked := True;
              DebugCheckbox.Enabled := False;
            end;
            // Else it is the standard server.

      ValidateFileName(PathToBinaryEd);
    finally
      InitControls := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.CheckUniqueServiceName(ServiceName, DisplayName: WideString);

var
  ServiceReg: TTntRegistry;
  KeyInfo: TRegKeyInfo;
  SubKeyNames: TTntStringList;
  ServiceKeyNames: TTntStringList;
  i: integer;
  DisplayName2: WideString;

begin
  ServiceReg := TTntRegistry.Create;
  SubKeyNames := TTntStringList.Create;
  ServiceKeyNames := TTntStringList.Create;
  try
    ServiceReg.RootKey := HKEY_LOCAL_MACHINE;

    ServiceReg.OpenKey('SYSTEM\' +
      'CurrentControlSet\Services', False);

    ServiceReg.GetKeyInfo(KeyInfo);
    ServiceReg.GetKeyNames(SubKeyNames);

    ServiceReg.CloseKey;

    for i := 0 to SubKeyNames.Count - 1 do
    begin
      if (ServiceReg.OpenKey('SYSTEM\' +
        'CurrentControlSet\Services\' + SubKeyNames[i], False)) then
      begin
        ServiceReg.GetValueNames(ServiceKeyNames);

        if (ServiceKeyNames.IndexOf('DisplayName') <> -1) then
        begin
          try
            try
              DisplayName2 := ServiceReg.ReadString('DisplayName');
            except
              DisplayName2 := '';
            end;

            if (AnsiCompareText(SubKeyNames[i],
              ServiceName) = 0) or
              (AnsiCompareText(DisplayName2,
              ServiceName) = 0) then
              raise EMyxError.Create('The name ' + ServiceName + ' is already used. ' +
                'You have to specify a unique ServiceName.');

            if (AnsiCompareText(SubKeyNames[i],
              DisplayName) = 0) or
              (AnsiCompareText(DisplayName2,
              DisplayName) = 0) then
              raise EMyxError.Create('The name ' + DisplayName + ' is already used. ' +
                'You have to specify a unique DisplayName.');
          finally
            ServiceReg.CloseKey;
          end;
        end;
      end;
    end;

  finally
    SubKeyNames.Free;
    ServiceKeyNames.Free;
    ServiceReg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.RefreshServiceList;

var
  LastService: WideString;

begin
  if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    if Assigned(ApplicationDM.CurrentService) then
      LastService := ApplicationDM.CurrentService.ServiceName;
    SetCurrentService(nil);
    ServicesTreeView.Items.Clear;

    // Get all MySQL Services.
    ScanForServices(ServicesTreeView, ServicesImageIndex);
    ChangeCurrentService(LastService);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ApplyChangesBtnClick(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.DiscardChangesBtnClick(Sender: TObject);

begin
  DiscardChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not InitControls and not (csDestroying in ComponentState) then
  begin
    if (FPageContentChanged) then
      DiscardChanges;

    ApplicationDM.CurrentService := nil;
    StartupLogMemo.Clear;

    if Assigned(Node) then
      SetCurrentService(Node.Data);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.InstallnewServiceMIClick(Sender: TObject);

var
  servicename: WideString;
  MySQLService: TMySQLService;

begin
  if (FPageContentChanged) then
    DiscardChanges;

  PageControl.ActivePage := ServiceConfigSheet;

  servicename := 'MySQL';

  if (ShowModalEditDialog('New Service',
    'Please enter a name for the new service. This service name will ' +
    'be used to start and stop the service. You can only use letters ' +
    'and numbers, not even spaces.'#13#10#13#10 +
    'Please note that you can specify a different display name for ' +
    'the service later, which can be more descriptive.',
    myx_mtEdit, 'OK'#13#10'Abort',
    True, 'Service name:', servicename) = 1) then
  begin
    //This procedure will raise an exception if the servicename is not unique
    CheckUniqueServiceName(servicename, servicename);

    MySQLService := TMySQLService.Create;
    MySQLService.ExistingService := False;

    MySQLService.ServiceName := servicename;
    MySQLService.DisplayName := servicename;
    MySQLService.Description := 'MySQL Server';
    MySQLService.ConfigFileSection := 'mysqld';
    MySQLService.StartType := SERVICE_AUTO_START; //Automatic
    MySQLService.Binary := 'mysqld';
    MySQLService.Status := SERVICE_STOPPED;

    MySQLService.ConfigFile := ApplicationDM.Options.MySQLInstallPath +
      'MySQL Server 5.1\' + 'my.ini';
      //GetWindowsDir+'my.ini';
    MySQLService.PathToBinary := ApplicationDM.Options.MySQLInstallPath +
      'MySQL Server 5.1\' + 'bin\';

    //Add new service to tree
    ServicesTreeView.Selected :=
      AddTreeViewChildNode(ServicesTreeView, nil,
      servicename, ServicesImageIndex, MySQLService);

    //SetCurrentService(MySQLService);

    InstallServiceBtn.Enabled := False;
    UninstallServiceBtn.Enabled := False;

    DoPageContentChanged(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.UninstallselectedServiceMIClick(Sender: TObject);

var
  SCMHandle,
  ServiceHandle: SC_Handle;

begin
  if Assigned(ApplicationDM.CurrentService) then
  begin
    // If the service was just created, simply discard it.
    if not ApplicationDM.CurrentService.ExistingService then
      DiscardChanges
    else
    begin
      if ApplicationDM.CurrentService.Status <> SERVICE_STOPPED then
      begin
        if ShowModalDialog(_('Stop Service'), Format(_('Please stop the service %s before deinstallation.'),
          [ApplicationDM.CurrentService.ServiceName]) + '#13#10#13#10' + _('Do you want to stop the service now?'),
          myx_mtConfirmation, _('Yes') + #13#10 + _('Abort')) = 1 then
        begin
          PageControl.ActivePage := StartStopServiceSheet;
          StopServiceBtnClick(Self);
        end;

        Exit;
      end;

      if ShowModalDialog('Delete Service', 'Are you sure you want to remove the selected service ' +
        ApplicationDM.CurrentService.ServiceName + '?', myx_mtConfirmation, 'Yes'#13#10'No') = 1 then
      begin
        SCMHandle := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
        if SCMHandle > 0 then
        begin
          SetStatusText('Deleting Service ...');

          try
            //Open service
            ServiceHandle := OpenServiceW(SCMHandle, PWideChar(ApplicationDM.CurrentService.ServiceName),
              SERVICE_ALL_ACCESS);
            try
              if ServiceHandle = 0 then
                raise EMyxSystemError.Create('Cannot open service.', GetLastError);

              if not DeleteService(ServiceHandle) then
                raise EMyxSystemError.Create('Cannot delete service.', GetLastError);

            finally
              CloseServiceHandle(ServiceHandle);
            end;
          finally
            CloseServiceHandle(SCMHandle);
          end;

          ApplicationDM.CurrentService := nil;

          RefreshServiceList;

          FPageContentChanged := False;
          ApplyChangesBtn.Enabled := False;
          DiscardChangesBtn.Enabled := False;

          SetStatusText('');
        end
        else
          raise EMyxSystemError.Create('Could not connect to the Service Control Manager.', GetLastError);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.RefreshServiceStatusMIClick(Sender: TObject);

begin
  RefreshServiceStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.PathToBinaryEdChange(Sender: TObject);

begin
  if InitControls then
    Exit;
    
  ValidateFileName(Sender);

  if (ApplicationDM.CurrentService <> nil) then
    ApplicationDM.CurrentService.PathToBinary := ExtractFilePath(PathToBinaryEd.Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.PathToBinaryBrowseBtnClick(Sender: TObject);

var
  OpenDlg: TTntOpenDialog;

begin
  OpenDlg := TTntOpenDialog.Create(Self);
  try
    OpenDlg.Filter := 'Executable files (*.exe)|*.exe';
    OpenDlg.InitialDir := ExtractFilePath(PathToBinaryEd.Text);

    if OpenDlg.Execute then
    begin
      if (Pos('mysqld', ExtractFileName(OpenDlg.FileName)) = 0) then
        if (ShowModalDialog('Correct file?',
          'The file you selected does not seem to be a MySQL server binary.'#13#10 +
          'Are you sure you want to use this file?',
          myx_mtConfirmation, 'Yes'#13#10'No') = 2) then
          Exit;

      SetServerBinary(ExtractFilePath(OpenDlg.FileName), ChangeFileExt(ExtractFileName(OpenDlg.FileName), ''));

      PathToBinaryEd.Text := OpenDlg.FileName;
      ValidateFileName(PathToBinaryEd);
    end;
  finally
    OpenDlg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.PathToBinaryEdExit(Sender: TObject);

begin
  SetServerBinary(ExtractFilePath(PathToBinaryEd.Text), ChangeFileExt(ExtractFileName(PathToBinaryEd.Text), ''));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ConfigFilenameEdChange(Sender: TObject);

begin
  ValidateFileName(sender);
  DoPageContentChanged(sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ServiceCheckerTmrTimer(Sender: TObject);

begin
  RefreshServiceStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);

begin
  if Node.Data = ApplicationDM.CurrentService then
    SetCurrentService(nil);
  TObject(Node.Data).Free;
  Node.Data := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

var
  Size: TSize;
  Item: TTntMenuItem;

begin
  if Sender is TTntMenuItem then
  begin
    Item := Sender as TTntMenuItem;
    ACanvas.Font := Font;

    if Item.IsLine then
    begin
      Width := 10; // This will actually have no effect, because other entries are much wider.
      Height := 6;
    end
    else
    begin
      GetTextExtentPoint32W(ACanvas.Handle, PWideChar(Item.Caption), Length(Item.Caption), Size);

      // Border around each entry.
      Width := Size.cx + 4;
      Height := Size.cy + 6;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServiceControlForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

var
  Item: TTntMenuItem;

begin
  if Sender is TTntMenuItem then
  begin
    Item := Sender as TTntMenuItem;
    ACanvas.Font := Font;

    if Item.IsLine then
    begin
      // A menu separator.
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(ARect.Left + 2, (ARect.Bottom + ARect.Top) div 2);
      ACanvas.LineTo(ARect.Right - 2, (ARect.Bottom + ARect.Top) div 2);
    end
    else
    begin
      // Top level items have an invisible parent, so have to check the parent of the parent.
      if (Item.Parent.Parent = nil) and not (Item.GetParentMenu is TPopupMenu) then
      begin
        if [odHotLight, odSelected] * State <> [] then
          ACanvas.Brush.Color := clHighlight
        else
          ACanvas.Brush.Color := clBtnFace;
      end;
      ACanvas.FillRect(ARect);
      Inc(ARect.Left, 8);
      SetBKMode(ACanvas.Handle, TRANSPARENT);
      Windows.DrawTextW(ACanvas.Handle, PWideChar(Item.Caption), Length(Item.Caption), ARect, DT_LEFT + DT_SINGLELINE +
        DT_HIDEPREFIX + DT_VCENTER);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

