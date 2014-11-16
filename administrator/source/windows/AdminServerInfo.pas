unit AdminServerInfo;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, AdditionalClasses, Buttons, PNGImage,
  InstanceSections, AuxFuncs, myx_public_interface,
  ApplicationDataModule, MySQLConnection, MyxError, TntExtCtrls,
  TntStdCtrls;

type
  TAdminServerInfoForm = class(TInstanceSectionForm)
    ServerInfoPnl: TTntPanel;
    LogoImg: TTntImage;
    RightBGImg: TTntImage;
    ConnectionGBox: TTntGroupBox;
    UserLbl: TTntLabel;
    HostLbl: TTntLabel;
    ClientInfoGBox: TTntGroupBox;
    ClientVersionCaptionLbl: TTntLabel;
    ClientHardwareCaptionLbl: TTntLabel;
    ClientOSCaptionLbl: TTntLabel;
    ClientNetworkNameCaptionLbl: TTntLabel;
    UserCaptionLbl: TTntLabel;
    HostCaptionLbl: TTntLabel;
    PortCaptionLbl: TTntLabel;
    PortLbl: TTntLabel;
    InstanceCaptionLbl: TTntLabel;
    InstanceNameLbl: TTntLabel;
    ClientNetworkNameLbl: TTntLabel;
    ClientOSLbl: TTntLabel;
    ClientHardwareLabel: TTntLabel;
    ClientVersionLbl: TTntLabel;
    ClientIPCaptionLabel: TTntLabel;
    ClientIPLbl: TTntLabel;
    ServiceStatusImg: TTntImage;
    Label5: TTntLabel;
    ServerStatusLbl: TTntLabel;
    ServerInfoGBox: TTntGroupBox;
    ServerVersionCaptionLbl: TTntLabel;
    ServerHardwareCaptionLbl: TTntLabel;
    ServerOSCaptionLbl: TTntLabel;
    ServerNetworkNameCaptionLbl: TTntLabel;
    ServerNetworkNameLbl: TTntLabel;
    ServerOSLbl: TTntLabel;
    ServerHardwareLbl: TTntLabel;
    ServerVersionLbl: TTntLabel;
    ServerIPCaptionLbl: TTntLabel;
    ServerIPLbl: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ServerStatusLblMouseEnter(Sender: TObject);
    procedure ServerStatusLblMouseLeave(Sender: TObject);
    procedure ServerStatusLblClick(Sender: TObject);
    procedure ServerInfoPnlResize(Sender: TObject);

    procedure SetServerInfos(Sender: TObject);
    procedure SetClientInfos(Sender: TObject);
    procedure FetchInfos(Sender: TObject);

    procedure GetInfos;
    procedure ClearInfos;

    function GetFormContentAsText: WideString; override;

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
  private
    FRightBGPNGImg,
    FMySQLLogoPNGImg: TPNGObject;

    FServiceStatusRunningPNGImg,
    FServiceStatusUnknownPNGImg: TPNGObject;

    FMaschineInfo: PMYX_MACHINE_INFO;
  end;

const
  CaptionTextWidth = 25;

var
  AdminServerInfoForm: TAdminServerInfoForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Main, PNGTools;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  RightBGImg.Width  :=  RightBGImg.Width;

  DockedPanel := ServerInfoPnl;

  FRightBGPNGImg := LoadPNGImageFromResource('dockpnl_bg', RightBGImg);
  FMySQLLogoPNGImg := LoadPNGImageFromResource('mysql_logo', LogoImg);

  FServiceStatusRunningPNGImg := LoadPNGImageFromResource('service_status_running');
  FServiceStatusUnknownPNGImg := LoadPNGImageFromResource('service_status_unknown');

  ServiceStatusImg.Picture.Graphic := FServiceStatusRunningPNGImg;

  GetInfos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.FormDestroy(Sender: TObject);

begin
  FRightBGPNGImg.Free;
  FMySQLLogoPNGImg.Free;
  FServiceStatusRunningPNGImg.Free;
  FServiceStatusUnknownPNGImg.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ServerStatusLblMouseEnter(Sender: TObject);

begin
  ServerStatusLbl.Font.Style := [fsBold, fsUnderline];
  ServerStatusLbl.Font.Color := clBlue;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ServerStatusLblMouseLeave(Sender: TObject);

begin
  ServerStatusLbl.Font.Style := [fsBold];
  ServerStatusLbl.Font.Color := clBlack;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ServerStatusLblClick(Sender: TObject);

begin
  if(Owner is TMainForm)then
  begin
    TMainForm(Owner).SectionControls.AdminTreeView.Selected := TMainForm(Owner).SectionControls.AdminTreeView.Items[1];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ServerInfoPnlResize(Sender: TObject);

begin
  LogoImg.Left := ServerInfoPnl.Width - 636 + 484;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.SetServerInfos(Sender: TObject);

begin
  ServerVersionLbl.Caption := FMaschineInfo.version;
  ServerNetworkNameLbl.Caption := FMaschineInfo.network_name;
  ServerIPLbl.Caption := FMaschineInfo.IP;
  ServerOSLbl.Caption := FMaschineInfo.OS;
  ServerHardwareLbl.Caption := FMaschineInfo.hardware;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.SetClientInfos(Sender: TObject);

begin
  ClientVersionLbl.Caption := FMaschineInfo.version;
  if FMaschineInfo.network_name <> '' then
    ClientNetworkNameLbl.Caption := FMaschineInfo.network_name
  else
  begin
    if FMaschineInfo.IP = '127.0.0.1' then
      ClientNetworkNameLbl.Caption := _('localhost')
    else
      ClientNetworkNameLbl.Caption := _('unknown');
  end;
  ClientIPLbl.Caption := FMaschineInfo.IP;
  ClientOSLbl.Caption := FMaschineInfo.OS;
  ClientHardwareLabel.Caption := FMaschineInfo.hardware;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.FetchInfos(Sender: TObject);

var
  PMySQL: Pointer;

begin
  PMySQL := myx_mysql_init;
  try
    if myx_connect_to_instance(MySQLConn.UserConnection.get_record_pointer, PMySQL) <> 0 then
      raise EMyxSQLError.Create(_('Cannot clone MySQL connection.'), myx_mysql_errno(PMySQL), myx_mysql_error(PMySQL));

    // Display client infos
    FMaschineInfo := myx_get_client_info(PMySQL);

    TFetchDataThread(Sender).ExecuteSynchronized(SetClientInfos);

    myx_free_pc_info(FMaschineInfo);

    // Display server infos
    FMaschineInfo := myx_get_server_info(MySQLConn.UserConnection.get_record_pointer, PMySQL);

    TFetchDataThread(Sender).ExecuteSynchronized(SetServerInfos);

    myx_free_pc_info(FMaschineInfo);
  finally
    myx_mysql_close(PMySQL);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.GetInfos;

var
  NewHeight: integer;
  
begin
  // Start Thread fetching data
  MySQLConn.FetchData(dkStatusVars, FetchInfos, nil, nil, _('Fetching Server Status ...'), True);

  // Display connection Infos
  UserLbl.Caption := MySQLConn.UserConnection.username;
  HostLbl.Caption := MySQLConn.UserConnection.hostname;
  PortLbl.Caption := IntToStr(MySQLConn.UserConnection.port);
  InstanceNameLbl.Caption := '';

  //Hide unknown fields
  NewHeight := 105;
  if(InstanceNameLbl.Caption='-')or
    (InstanceNameLbl.Caption='')then
  begin
    InstanceCaptionLbl.Hide;
    InstanceNameLbl.Hide;
    NewHeight := NewHeight-18;
  end;
  ConnectionGBox.Height := NewHeight;

  ServerInfoGBox.Top := ConnectionGBox.Top+ConnectionGBox.Height+9;

  NewHeight := 127;

  if(ServerOSLbl.Caption='-')then
  begin
    ServerOSCaptionLbl.Hide;
    ServerOSLbl.Hide;
    NewHeight := NewHeight-18;
  end;

  if(ServerHardwareLbl.Caption='-')then
  begin
    ServerHardwareCaptionLbl.Hide;
    ServerHardwareLbl.Hide;
    NewHeight := NewHeight-18;
  end;

  ServerInfoGBox.Height := NewHeight;

  ClientInfoGBox.Top := ServerInfoGBox.Top+ServerInfoGBox.Height+9;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ClearInfos;

begin
  UserLbl.Caption := '-';
  HostLbl.Caption := '-';
  PortLbl.Caption := '-';
  InstanceNameLbl.Caption := '-';

  ServerVersionLbl.Caption := '-';
  ServerNetworkNameLbl.Caption := '-';
  ServerIPLbl.Caption := '-';
  ServerOSLbl.Caption := '-';
  ServerHardwareLbl.Caption := '-';

  ClientVersionLbl.Caption := '-';
  ClientNetworkNameLbl.Caption := '-';
  ClientIPLbl.Caption := '-';
  ClientOSLbl.Caption := '-';
  ClientHardwareLabel.Caption := '-';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ConnectionLost(var Message: TMessage);

begin
  ClearInfos;

  ServerStatusLbl.Caption := _('Disconnected from Server.');
  ServiceStatusImg.Picture.Assign(FServiceStatusUnknownPNGImg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerInfoForm.ConnectionEstablished(var Message: TMessage);

begin
  GetInfos;

  ServerStatusLbl.Caption := _('MySQL Server is running.');
  ServiceStatusImg.Picture.Assign(FServiceStatusRunningPNGImg);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminServerInfoForm.GetFormContentAsText: WideString;

var
  S: WideString;

begin
  S := _('MySQL Server Information')+#13#10+
    StringOfChar('-', 80)+#13#10+
    ConnectionGBox.Caption+#13#10+
    '  '+StringAlignLeft(UserCaptionLbl.Caption, CaptionTextWidth)+
      UserLbl.Caption+#13#10+
    '  '+StringAlignLeft(HostCaptionLbl.Caption, CaptionTextWidth)+
      HostLbl.Caption+#13#10+
    '  '+StringAlignLeft(PortCaptionLbl.Caption, CaptionTextWidth)+
      PortLbl.Caption+#13#10;

  if(InstanceCaptionLbl.Visible)then
    S := S+'  '+StringAlignLeft(InstanceCaptionLbl.Caption, CaptionTextWidth)+
      InstanceNameLbl.Caption+#13#10;

  S := S+StringOfChar('-', 80)+#13#10+
    ServerInfoGBox.Caption+#13#10+
    '  '+StringAlignLeft(ServerVersionCaptionLbl.Caption, CaptionTextWidth)+
      ServerVersionLbl.Caption+#13#10+
    '  '+StringAlignLeft(ServerNetworkNameCaptionLbl.Caption, CaptionTextWidth)+
      ServerNetworkNameLbl.Caption+#13#10+
    '  '+StringAlignLeft(ServerIPCaptionLbl.Caption, CaptionTextWidth)+
      ServerIPLbl.Caption+#13#10;

  if(ServerOSLbl.Visible)then
    S := S+'  '+StringAlignLeft(ServerOSCaptionLbl.Caption, CaptionTextWidth)+ServerOSLbl.Caption+#13#10;
  if(ServerHardwareLbl.Visible)then
    S := S+'  '+StringAlignLeft(ServerHardwareCaptionLbl.Caption, CaptionTextWidth)+ServerHardwareLbl.Caption+#13#10;

  S := S+StringOfChar('-', 80)+#13#10+
    ClientInfoGBox.Caption+#13#10+
    '  '+StringAlignLeft(ClientVersionCaptionLbl.Caption, CaptionTextWidth)+
      ClientVersionLbl.Caption+#13#10+
    '  '+StringAlignLeft(ClientNetworkNameCaptionLbl.Caption, CaptionTextWidth)+
      ClientNetworkNameLbl.Caption+#13#10+
    '  '+StringAlignLeft(ClientIPCaptionLabel.Caption, CaptionTextWidth)+
      ClientIPLbl.Caption+#13#10+
    '  '+StringAlignLeft(ClientOSCaptionLbl.Caption, CaptionTextWidth)+
      ClientOSLbl.Caption+#13#10+
    '  '+StringAlignLeft(ClientHardwareCaptionLbl.Caption, CaptionTextWidth)+
      ClientHardwareLabel.Caption+#13#10;

  Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
