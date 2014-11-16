unit Main;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ImgList, Contnrs, Menus,
  ApplicationDataModule, MySQLConnection, AdditionalClasses,
  AdminServerInfo, AdminServiceControl, AdminUsers,
  AdminStartupVariables, AdminServerHealth, AdminServerConnections,
  AdminCatalog, AdminBackup, AdminServerLogs, AdminReplication,
  AdminRestore, ClipBrd,
  AuxFuncs, PNGImage, myx_public_interface, myx_admin_public_interface,
  Sections, Options, OptionsEditor, AdminOptionPages,
  CommonFuncs, TntForms, TntComCtrls, TntExtCtrls, TntMenus,
  MySQLCommonFuncs, AppInstanceMgmt;

{$include Consts.ini}

type
  TMainForm = class(TTntForm)
    StatusBar: TTntStatusBar;
    TopPnl: TTntPanel;
    Panel1: TTntPanel;
    Panel2: TTntPanel;
    MainMenu: TTntMainMenu;
    FileMI: TTntMenuItem;
    ConnecttoServerMI: TTntMenuItem;
    SaveConnectionMI: TTntMenuItem;
    N3: TTntMenuItem;
    CopyActivePageAsTextMI: TTntMenuItem;
    N2: TTntMenuItem;
    CloseMI: TTntMenuItem;
    Edit1: TTntMenuItem;
    CopyMI: TTntMenuItem;
    CutMI: TTntMenuItem;
    PasteMI: TTntMenuItem;
    HelpMI: TTntMenuItem;
    OnlineHelpMI: TTntMenuItem;
    N6: TTntMenuItem;
    ReportBugMI: TTntMenuItem;
    VisitMySQLcomMI: TTntMenuItem;
    N7: TTntMenuItem;
    AboutMI: TTntMenuItem;
    ManageConnectionsMI: TTntMenuItem;
    OptionsMI: TTntMenuItem;
    ToolsMI: TTntMenuItem;
    ViewMI: TTntMenuItem;
    MySQLCommandlineclientMI: TTntMenuItem;
    WindowsCommandLineMI: TTntMenuItem;
    ReconnectMI: TTntMenuItem;
    SelectAllMI: TTntMenuItem;
    MySQLQueryBrowserMI: TTntMenuItem;
    N1: TTntMenuItem;
    MySQLSystemTrayMonitorMI: TTntMenuItem;
    ListopenMySQLAdministratorBugsMI: TTntMenuItem;
    N5: TTntMenuItem;
    N9: TTntMenuItem;
    WindowMI: TTntMenuItem;
    N01: TTntMenuItem;
    N4: TTntMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure BuildSectionTree;
    procedure DoCurrentSectionChanged(Sender: TObject);
    function CreateSectionForm(AOwner: TComponent; SidebarSectionType: integer): TSectionForm;

    procedure ConnecttoServerMIClick(Sender: TObject);
    procedure CloseMIClick(Sender: TObject);

    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const R: TRect);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SaveConnectionMIClick(Sender: TObject);
    procedure PasteMIClick(Sender: TObject);
    procedure OptionsMIClick(Sender: TObject);
    procedure ManageConnectionsMIClick(Sender: TObject);
    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;

    procedure MySQLCommandlineclientMIClick(Sender: TObject);
    procedure WindowsCommandLineMIClick(Sender: TObject);
    procedure ReportBugMIClick(Sender: TObject);
    procedure VisitMySQLcomMIClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnlineHelpMIClick(Sender: TObject);
    procedure AboutMIClick(Sender: TObject);
    procedure ReconnectMIClick(Sender: TObject);

    procedure HandleIdle(Sender: TObject; var Done: Boolean);
    procedure CutMIClick(Sender: TObject);
    procedure CopyMIClick(Sender: TObject);
    procedure SelectAllMIClick(Sender: TObject);

    procedure CopyActivePageAsTextMIClick(Sender: TObject);
    procedure MySQLQueryBrowserMIClick(Sender: TObject);
    procedure MySQLSystemTrayMonitorMIClick(Sender: TObject);
    procedure ListopenMySQLAdministratorBugsMIClick(Sender: TObject);

    procedure DoException(Sender: TObject; E: Exception);

    procedure WMInitMenuPopup(var MSG: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure TntFormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
  private
    CatalogSection, BackupRestoreSection,
    UserAdministrationSection: TSidebarSection;

    PNGThreadStop: TPNGObject;
  public
    SectionControls: TSectionControls;
    procedure PrepareGUI();
  end;

const
  SSTAdmin_ServerInfo = 1;
  SSTAdmin_ServerConfig = 2;
  SSTAdmin_ServiceControl = 3;
  SSTAdmin_StartupVariables = 4;
  SSTAdmin_ServerConnections = 5;
  SSTAdmin_UserAdministration = 6;
  SSTAdmin_ServerHealth = 7;
  SSTAdmin_ServerLogs = 8;
  SSTAdmin_Backup = 9;
  SSTAdmin_Restore = 10;
  SSTAdmin_Replication = 11;
  SSTAdmin_Catalog = 12;

var
  MainForm: TMainForm;

implementation

uses
  About, PNGTools, TntClipbrd;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  RegisterMySQLApplication(Application.Title, product_version, ExtractFilePath(Application.ExeName));

  ApplicationDM.CurrentConnection.UseStatusBar(StatusBar);
  MainMenu.AutoHotkeys := maAutomatic;

  PNGThreadStop := LoadPNGImageFromResource('thread_stop', nil);

  if not ApplicationDM.Options.RestoreWindowPos(Self) then
  begin
    if(Screen.Width>=1024)then
    begin
      if(Screen.Height>=1024)then
      begin
        Width := 900;
        Height := 720;
      end
      else
      begin
        Width := 900;
        Height := 680;
      end;

      //Align center (till position is stored)
      Left := (Screen.Width+Width) div 2 - Width;
      Top := (Screen.Height+Height) div 2 - Height;
    end
    else
    begin
      Width := 800;
      Height := 572;

      Left := (Screen.Width+Width) div 2 - Width;
      Top := 0;

      WindowState := wsMaximized;
    end;
  end;

  SectionControls := TSectionControls.Create(self,
    CreateSectionForm,
    ViewMI,
    ApplicationDM.Options.SectionSidebarHidden,
    ApplicationDM.Options.SectionSidebarWidth,
    ApplicationDM.SectionImageList,
    11);


  Application.OnIdle := HandleIdle;
  Application.OnException := DoException;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  SectionControls.Free;

  ApplicationDM.Options.AddWindowPos(self);

  PNGThreadStop.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  if GetKeyState(VK_CONTROL) < 0 then
    ConnectToServerMIClick(Self);

  ApplicationDM.ApplicationIsTerminating := True;

  Action := caFree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PrepareGUI();

begin
  // Spit out a warning if the user connected to an unsupported server.
  with ApplicationDM.CurrentConnection do
  begin
    if MajorVersion > 5 then
      ShowModalDialog(_('Unsupported server version'), _('You connected to a server which is not supported by this tool.'#13#10 +
        'This means some parts of it may not work as expected or are disabled entirely.'), myx_mtWarning, 'Ok');
  end;

  // Set the Caption
  if ApplicationDM.CurrentConnection.GetConnectionCaption <> '' then
    Caption := _('MySQL Administrator') + ' - ' + ApplicationDM.CurrentConnection.GetConnectionCaption;

  // Build the Admin Tree
  BuildSectionTree;

  // Select first page
  if(ApplicationDM.Options.StartSection>0)and
    (ApplicationDM.Options.StartSection<=
      SectionControls.AdminTreeView.Items.Count)then
    SectionControls.AdminTreeView.Selected := 
      SectionControls.AdminTreeView.Items[
      ApplicationDM.Options.StartSection-1];

  // Enable Tools Menu Items
  MySQLQueryBrowserMI.Enabled := GetMySQLQueryBrowserCmd <> '';
  MySQLCommandlineclientMI.Enabled := (GetMySQLCommandLineClientPath <> '') and
    Assigned(ApplicationDM.CurrentConnection.UserConnection);
  WindowsCommandLineMI.Enabled := FileExists(GetSystemDir+'cmd.exe');
  MySQLSystemTrayMonitorMI.Enabled := FileExists(
    ExtractFilePath(Application.ExeName)+'MySQLSystemTrayMonitor.exe');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ConnecttoServerMIClick(Sender: TObject);

begin
  CreateSubProcess(Application.ExeName, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseMIClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const R: TRect);

begin
  if (Panel.Index = 0) and ApplicationDM.CurrentConnection.FetchingData then
    PNGThreadStop.Draw(StatusBar.Canvas, Rect(R.Left + 1, R.Top + 1, R.Left + 1 + 13, R.Top + 1 + 13));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (X < 20) and ApplicationDM.CurrentConnection.FetchingData then
  begin
    ApplicationDM.CurrentConnection.ClearWorkList;

    Statusbar.Panels[1].Text := '';
    StatusBar.Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TControlCast = class(TControl);

procedure TMainForm.TntFormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);

// Forward mouse wheel messages as scroll messages to scroll boxes (if there is one under the mouse).

var
  Control: TControl;
  Message: TMessage;

begin
  Control := FindDragTarget(MousePos, False);
  while Assigned(Control) and not (Control is TScrollBox) do
    Control := Control.Parent;

  if Control is TScrollBox then
  begin
    FillChar(Message, SizeOf(Message), 0);
    Message.Msg := WM_VSCROLL;
    if WheelDelta < 0 then
      Message.WParam := MakeWParam(SB_LINEDOWN, 0)
    else
      Message.WParam := MakeWParam(SB_LINEUP, 0);

    TControlCast(Control).WndProc(Message);
    Handled := True;
  end
  else
    Handled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OptionsMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  OptionsForm := TOptionsForm.Create(self, TAdminOptionPagesForm.Create(self), ApplicationDM.CurrentConnection.MySQL);
  try
    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SaveConnectionMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  OptionsForm := TOptionsForm.Create(self, TAdminOptionPagesForm.Create(self), ApplicationDM.CurrentConnection.MySQL);
  try
    // Activate connections page.
    OptionsForm.ShowOptionPage(ConnectionsPage);

    OptionsForm.AddConnection(ApplicationDM.CurrentConnection.UserConnection);
    OptionsForm.ActiveControl := OptionsForm.ConnectionEd;

    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ManageConnectionsMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;
  
begin
  OptionsForm := TOptionsForm.Create(self, TAdminOptionPagesForm.Create(self), ApplicationDM.CurrentConnection.MySQL);
  try
    //Select Connections Page
    OptionsForm.ShowOptionPage(ConnectionsPage);

    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ConnectionLost(var Message: TMessage);

begin
  with ApplicationDM do
    SectionControls.RefreshSidebarIcons(False, CurrentConnection.IsLocalServer, Options.ShowOnlyServiceSections);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ConnectionEstablished(var Message: TMessage);

begin
  with ApplicationDM do
    SectionControls.RefreshSidebarIcons(True, CurrentConnection.IsLocalServer, Options.ShowOnlyServiceSections);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLCommandlineclientMIClick(Sender: TObject);

var
  Command: WideString;

begin
  Command := GetMySQLCommandLineClientPath;
  with ApplicationDM do
    if (Command<>'') and Assigned(CurrentConnection.UserConnection) then
      CreateSubProcess(Command + ' -h' + CurrentConnection.UserConnection.hostname +
        ' -u' + CurrentConnection.UserConnection.username +
        ' -p' + CurrentConnection.UserConnection.password +
        ' -P' + IntToStr(CurrentConnection.UserConnection.port), '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WindowsCommandLineMIClick(Sender: TObject);

var
  cmd: WideString;

begin
  cmd := GetSystemDir+'cmd.exe';
  if(FileExists(cmd))then
    CreateSubProcess(cmd, GetHomeDir);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ReportBugMIClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VisitMySQLcomMIClick(Sender: TObject);

begin
  BrowseWebPage('http://www.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if(Key=VK_F1)then
    ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlineHelpMIClick(Sender: TObject);

begin
  ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AboutMIClick(Sender: TObject);

begin
  ShowAboutDialog('MySQL Administrator',
    product_version+' '+product_build_level,
    _('Michael G. Zinner, main concept, graphical design, '+
    'Windows development, library coding | '+
    'Mike Lischke, Windows development, library coding, UI, QA |' +
    'Alfredo Kengi Kojima, Linux development, library coding | '+
    'Ulrich Bayer, library coding, WIX | Victor Vagin, library coding, '+
    ' QA | '+
    'Brian Aker, conceptual design, supervising | '+
    'Stefan Hinz, documentation | '+
    'Mike Hillyer, documentation'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.BuildSectionTree;

var
  UnsupportedServer: Boolean;

begin
  SectionControls.AdminTreeView.Items.Clear;

  // Create all AdminTree nodes and attach the corresponding action.
  if not ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    // Create Server Information Section
    SectionControls.AddSection(_('Server Information'), TSidebarSection.Create(SSTAdmin_ServerInfo, 0, 1, True, False));
  end;

  // Create Service Control Section
  SectionControls.AddSection(_('Service Control'),
    TSidebarSection.Create(SSTAdmin_ServiceControl, 2, 3, False, True, nil,
      ApplicationDM.Options.ShowOnlyServiceSections));

  // Create Server Variables Section
  SectionControls.AddSection(_('Startup Variables'),
    TSidebarSection.Create(SSTAdmin_StartupVariables, 4, 5, False, True, nil,
      ApplicationDM.Options.ShowOnlyServiceSections));

  if not ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    UnsupportedServer := mysql_full_version_is_later_or_equal_than(ApplicationDM.CurrentConnection.MySQL, 6, 0, 0) <> 0;

    // Create User Administration Section
    UserAdministrationSection :=
      TSidebarSection.Create(SSTAdmin_UserAdministration, 8, 9, True, False, nil, True, nil, nil, UnsupportedServer);
    SectionControls.AddSection(_('User Administration'), UserAdministrationSection);

    //Create Server Connections Section
    SectionControls.AddSection(_('Server Connections'),
      TSidebarSection.Create(SSTAdmin_ServerConnections, 6, 7, True, False));

    //Create Health Section
    SectionControls.AddSection(_('Health'),
      TSidebarSection.Create(SSTAdmin_ServerHealth, 10, 11, True, False));
  end;

  //Create Server Logs Section
  SectionControls.AddSection(_('Server Logs'),
    TSidebarSection.Create(SSTAdmin_ServerLogs, 12, 13, False, True, nil,
      ApplicationDM.Options.ShowOnlyServiceSections));

  if not ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    //Create Replication Status Section
    SectionControls.AddSection(_('Replication Status'),
      TSidebarSection.Create(SSTAdmin_Replication, 18, 19, True, False));

    //Create Backup Section
    BackupRestoreSection := TSidebarSection.Create(SSTAdmin_Backup, 14, 15, True, False, nil, True);
    SectionControls.AddSection(_('Backup'), BackupRestoreSection);

    //Create Restore Section
    SectionControls.AddSection(_('Restore'), TSidebarSection.Create(SSTAdmin_Restore, 16, 17, True, False, nil, True));

    //Create Catalogs Section
    CatalogSection := TSidebarSection.Create(SSTAdmin_Catalog, 20, 21, True, False, nil, True);
    SectionControls.AddSection(_('Catalogs'), CatalogSection);
  end;

  with ApplicationDM do
    SectionControls.RefreshSidebarIcons(CurrentConnection.Connected, CurrentConnection.IsLocalServer,
      Options.ShowOnlyServiceSections);

  if ApplicationDM.Options.ShowOnlyServiceSections then
    SaveConnectionMI.Enabled := False;

  SectionControls.OnCurrentSectionChanged := DoCurrentSectionChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoCurrentSectionChanged(Sender: TObject);

var
  S: WideString;

begin
  S := SectionControls.CurrentSidebarSection.SectionForm.GetFormContentAsText;
  CopyActivePageAsTextMI.Enabled := S <> '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TMainForm.CreateSectionForm(AOwner: TComponent; SidebarSectionType: integer): TSectionForm;

begin
  Result := nil;

  with ApplicationDM do
  begin
    // Do *not* give the forms an owner otherwise they will be implicitly destroyed.
    // However the section control takes care for that so this would collide.
    case SidebarSectionType of
      SSTAdmin_ServerInfo:
        Result := TAdminServerInfoForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_ServiceControl:
        Result := TAdminServiceControlForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_StartupVariables:
        Result := TAdminStartupVariablesForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_ServerHealth:
        Result := TAdminServerHealthForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_ServerConnections:
        Result := TAdminServerConnectionsForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_UserAdministration:
        Result := TAdminUsersForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_Catalog:
        Result := TAdminCatalogForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_Backup:
        Result := TAdminBackupForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_Restore:
        Result := TAdminRestoreForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_ServerLogs:
        Result := TAdminServerLogsForm.Create(nil, CurrentConnection, StatusBar);
      SSTAdmin_replication:
        Result := TAdminReplicationForm.Create(nil, CurrentConnection, StatusBar);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ReconnectMIClick(Sender: TObject);

begin
  ConnecttoServerMIClick(self);

  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.HandleIdle(Sender: TObject; var Done: Boolean);

var
  Enable: Boolean;

begin
  if(ActiveControl is TCustomEdit)then
  begin
    Enable := TCustomEdit(Self.ActiveControl).SelLength>0;
    CutMI.Enabled := Enable;
    CopyMI.Enabled := Enable;
    Enable := Clipboard.HasFormat(CF_TEXT);
    PasteMI.Enabled := Enable;
    SelectAllMI.Enabled := Enable;
  end
  else
  begin
    CutMI.Enabled := False;
    CopyMI.Enabled := False;
    PasteMI.Enabled := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CutMIClick(Sender: TObject);

begin
  SendMessage(ActiveControl.Handle, WM_Cut, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CopyMIClick(Sender: TObject);

begin
  SendMessage(ActiveControl.Handle, WM_COPY, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PasteMIClick(Sender: TObject);

begin
  SendMessage(ActiveControl.Handle, WM_Paste, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SelectAllMIClick(Sender: TObject);

begin
  if(ActiveControl is TCustomEdit)then
    TCustomEdit(ActiveControl).SelectAll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CopyActivePageAsTextMIClick(Sender: TObject);

var
  S: WideString;

begin
  S := SectionControls.CurrentSidebarSection.SectionForm.GetFormContentAsText;
  if S <> '' then
    TntClipboard.AsWideText := S;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLQueryBrowserMIClick(Sender: TObject);

  //---------------------------------------------------------------------------

  function ConvertToHex(const S: WideString): string;

  var
    Utf8String: string;
    
  begin
    Utf8String := Utf8Encode(S);
    SetLength(Result, 2 * Length(Utf8String));
    BinToHex(PChar(Utf8String), PChar(Result), Length(Utf8String));
  end;

  //---------------------------------------------------------------------------

var
  Command: WideString;

begin
  Command := GetMySQLQueryBrowserCmd;

  with ApplicationDM do
    if Assigned(CurrentConnection.UserConnection) then
    begin
      Command := Command + ' "-u' + CurrentConnection.UserConnection.username + '"';
      if CurrentConnection.UserConnection.password <> '' then
        Command := Command + ' -x' + ConvertToHex(CurrentConnection.UserConnection.password);
      Command := Command+' "-h' + CurrentConnection.UserConnection.hostname + '"';
      Command := Command+' "-P' + IntToStr(CurrentConnection.UserConnection.port) + '"';
    end;

  CreateSubProcess(Command, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLSystemTrayMonitorMIClick(Sender: TObject);

begin
  CreateSubProcess(ExtractFilePath(Application.ExeName) + 'MySQLSystemTrayMonitor.exe', '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ListopenMySQLAdministratorBugsMIClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com/search.php?search_for=&limit=All&order_by=&direction=ASC&cmd=display&status=Active&severity=&showstopper=&bug_type=MySQL+Administrator&php_os=&phpver=&assign=&bug_age=0');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoException(Sender: TObject; E: Exception);

begin
  ShowModalDialog(Application.Title + ' ' + _('Exception'), E.Message, myx_mtError, _('OK'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMInitMenuPopup(var MSG: TWMInitMenuPopup);

begin
  if (Msg.MenuPopup = WindowMI.Handle) then
    BuildRegisterApplicationMenuItems(WindowMI);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
