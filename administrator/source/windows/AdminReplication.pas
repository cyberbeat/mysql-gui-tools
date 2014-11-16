unit AdminReplication;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, InstanceSections, ComCtrls, ImgList,
  AdditionalClasses, myx_admin_public_interface, myx_public_interface,
  ApplicationDataModule, MySQLConnection, AuxFuncs,
  Options, MyxError, TntExtCtrls, TntComCtrls, TntStdCtrls, gnugettext;

type
  TAdminReplicationForm = class(TInstanceSectionForm)
    ReplicationPnl: TTntPanel;
    ReplicationPageControl: TTntPageControl;
    OverviewTabSheet: TTabSheet;
    Panel5: TTntPanel;
    OverviewBevel: TTntBevel;
    Label8: TTntLabel;
    Image3: TTntImage;
    Panel3: TTntPanel;
    BottomBtnPnl: TTntPanel;
    Button1: TTntButton;
    LogImageList: TImageList;
    ServerInformationTabSheet: TTabSheet;
    Panel1: TTntPanel;
    InformationBevel: TTntBevel;
    Label1: TTntLabel;
    Image1: TTntImage;
    Panel2: TTntPanel;
    ServerInformationListView: TTntListView;
    ReplOverviewPnl: TTntPanel;
    AddInstanceToMonitoringListBtn: TTntButton;
    RefreshBtn: TTntButton;
    RemoveInstancefromMonitoringListBtn: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReplicationPnlResize(Sender: TObject);
    procedure GenerateReplicationOverview;
    procedure ReplicationPageControlChange(Sender: TObject);
    procedure RefreshHostList;

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure ServerInformationListViewSelectItem(Sender: TObject;
      Item: TListItem; Selected: Boolean);
    procedure AddInstanceToMonitoringListBtnClick(Sender: TObject);
    procedure RemoveInstancefromMonitoringListBtnClick(Sender: TObject);

    procedure CheckUserReplHostBtns;
    procedure RefreshBtnClick(Sender: TObject);
  private
    FReplicationHosts: TMYX_REPL_HOSTS;
    FUserReplicationHosts: TMYX_USER_REPL_HOSTS;
    FUserReplicationHostsFilename: WideString;
  end;

var
  AdminReplicationForm: TAdminReplicationForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel:=ReplicationPnl;

  FReplicationHosts:=nil;
  FUserReplicationHosts:=nil;

  FUserReplicationHostsFilename := MYXCommonOptions.UserDataDir + 'mysqlx_replication_hosts_' + MySQLConn.UserConnection.hostname +
    '.xml';

  if MySQLConn.Connected then
    RefreshHostList
  else
    DisableEnablePages(ReplicationPageControl, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.FormDestroy(Sender: TObject);

begin
  FReplicationHosts.Free;
  FUserReplicationHosts.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.ReplicationPnlResize(Sender: TObject);

var
  InitSheetWidth,
  InitSheetHeight: integer;

begin
  InitSheetWidth:=563;
  InitSheetHeight:=451;

  //Adjust Page Control
  ReplicationPageControl.Width:=ReplicationPnl.Width-591+571;
  ReplicationPageControl.Height:=ReplicationPnl.Height-501+479;

  //Overview Tab Sheet
  OverviewBevel.Width:=OverviewTabSheet.Width-InitSheetWidth+535;

  ReplOverviewPnl.Width:=OverviewTabSheet.Width-InitSheetWidth+535;
  ReplOverviewPnl.Height:=OverviewTabSheet.Height-InitSheetHeight+353;

  //Information Tab Sheet
  InformationBevel.Width:=ServerInformationTabSheet.Width-InitSheetWidth+535;

  ServerInformationListView.Width:=OverviewTabSheet.Width-InitSheetWidth+535;
  ServerInformationListView.Height:=OverviewTabSheet.Height-InitSheetHeight+353;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.ReplicationPageControlChange(Sender: TObject);

begin
  ReplicationPnlResize(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.GenerateReplicationOverview;

var
  theReplModel: TReplicationModel;
  LizReplNode,
  AnnaReplNode,
  BettyReplNode,
  GraceReplNode: TReplicationNode;
  
begin
  theReplModel:=TReplicationModel.Create(ReplOverviewPnl);
  theReplModel.Parent:=ReplOverviewPnl;
  theReplModel.Align:=alClient;

  LizReplNode:=theReplModel.AddNode('Liz', rsRunning, 300, 40);

  AnnaReplNode:=theReplModel.AddNode('Anna', rsRunning, 180, 140);
  theReplModel.LinkNodes(LizReplNode, AnnaReplNode);
  BettyReplNode:=theReplModel.AddNode('Betty', rsRunning, 300, 160);
  theReplModel.LinkNodes(LizReplNode, BettyReplNode);
  GraceReplNode:=theReplModel.AddNode('Grace', rsRunning, 420, 140);
  theReplModel.LinkNodes(LizReplNode, GraceReplNode);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.RefreshHostList;

var
  PUserHosts: PMYX_USER_REPL_HOSTS;
  PReplHosts: PMYX_REPL_HOSTS;
  error: MYX_ADMIN_LIB_ERROR;
  I: integer;
  ListItem: TListItem;

begin
  ServerInformationListView.Items.Clear;

  FReplicationHosts.Free;

  if(FUserReplicationHosts=nil)then
    if(FileExists(FUserReplicationHostsFilename))then
    begin
      PUserHosts:=myx_read_repl_user_hosts(FUserReplicationHostsFilename, @error);
      if(PUserHosts<>nil)then
      begin
        try
          FUserReplicationHosts:=TMYX_USER_REPL_HOSTS.Create(PUserHosts);
        finally
          if(PUserHosts<>nil)then
            myx_free_repl_user_hosts(PUserHosts);
        end;
      end
      else
        FUserReplicationHosts:=TMYX_USER_REPL_HOSTS.Create;
    end
    else
    begin
      FUserReplicationHosts:=TMYX_USER_REPL_HOSTS.Create;
    end;

  if(FUserReplicationHosts<>nil)then
  begin
    PReplHosts:=myx_show_repl_hosts_status(MySQLConn.MySQL,
      FUserReplicationHosts.get_record_pointer, @error);
    if(error<>MYX_ADMIN_NO_ERROR)then
      raise EMyxSQLError.Create(_('Error while fetching the replication information.'),
        myx_mysql_errno(MySQLConn.MySQL),
        myx_mysql_error(MySQLConn.MySQL));

    if(PReplHosts<>nil)then
    begin
      try
        FReplicationHosts:=TMYX_REPL_HOSTS.create(PReplHosts);

        for I:=0 to FReplicationHosts.hosts.Count-1 do
        begin
          ListItem:=AddListViewItem(ServerInformationListView, nil,
            FReplicationHosts.hosts[I].host,
            Ord(FReplicationHosts.hosts[I].status),
            FReplicationHosts.hosts[I]);

          if(FReplicationHosts.hosts[I].server_id<>0)then
            ListItem.SubItems.Add(IntToStr(FReplicationHosts.hosts[I].server_id))
          else
            ListItem.SubItems.Add('');

          if(FReplicationHosts.hosts[I].port<>0)then
            ListItem.SubItems.Add(IntToStr(FReplicationHosts.hosts[I].port))
          else
            ListItem.SubItems.Add('');

          if(FReplicationHosts.hosts[I].is_master=1)then
            ListItem.SubItems.Add('MASTER')
          else
            ListItem.SubItems.Add('SLAVE');

          case FReplicationHosts.hosts[I].status of
            MYX_RHS_AVAILABLE:
              ListItem.SubItems.Add(_('Available'));
            MYX_RHS_NEW_HOST:
              ListItem.SubItems.Add(_('New Instance'));
          else
            ListItem.SubItems.Add(_('Not Available'));
          end;

          if(FReplicationHosts.hosts[I].is_master=1)then
          begin
            ListItem.SubItems.Add(FReplicationHosts.hosts[I].binlog_file);
            ListItem.SubItems.Add(FReplicationHosts.hosts[I].binlog_pos);
          end;
        end;
      finally
        myx_free_repl_hosts_status(PReplHosts);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.ConnectionLost(var Message: TMessage);

begin
  DisableEnablePages(ReplicationPageControl, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.ConnectionEstablished(var Message: TMessage);

begin
  DisableEnablePages(ReplicationPageControl, True);

  RefreshHostList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.CheckUserReplHostBtns;

begin
  AddInstanceToMonitoringListBtn.Enabled:=False;
  RemoveInstancefromMonitoringListBtn.Enabled:=False;

  if(ServerInformationListView.Selected<>nil)then
    if(ServerInformationListView.Selected.Data<>nil)then
    begin
      if(TMYX_REPL_HOST(ServerInformationListView.Selected.Data).status=MYX_RHS_NEW_HOST)then
        AddInstanceToMonitoringListBtn.Enabled:=True
      else
        RemoveInstancefromMonitoringListBtn.Enabled:=True;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.ServerInformationListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

begin
  CheckUserReplHostBtns;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.AddInstanceToMonitoringListBtnClick(Sender: TObject);

var
  UserReplHost: TMYX_USER_REPL_HOST;
  ReplHost: TMYX_REPL_HOST;

begin
  if(ServerInformationListView.Selected<>nil)then
    if(ServerInformationListView.Selected.Data<>nil)then
    begin
      ReplHost:=TMYX_REPL_HOST(ServerInformationListView.Selected.Data);

      UserReplHost:=TMYX_USER_REPL_HOST.create(ReplHost.host);

      FUserReplicationHosts.hosts.Add(UserReplHost);

      ReplHost.status:=MYX_RHS_AVAILABLE;
      ServerInformationListView.Selected.ImageIndex:=Ord(ReplHost.status);

      CheckUserReplHostBtns;

      if(FUserReplicationHosts<>nil)then
        myx_save_repl_user_hosts(FUserReplicationHosts.get_record_pointer,
          FUserReplicationHostsFilename);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.RemoveInstancefromMonitoringListBtnClick(Sender: TObject);

var
  I: integer;
  ReplHost: TMYX_REPL_HOST;

begin
  if(ServerInformationListView.Selected<>nil)then
    if(ServerInformationListView.Selected.Data<>nil)then
    begin
      ReplHost:=TMYX_REPL_HOST(ServerInformationListView.Selected.Data);

      for I:=0 to FUserReplicationHosts.hosts.Count-1 do
        if(CompareText(FUserReplicationHosts.hosts[I].name, ReplHost.host)=0)then
        begin
          FUserReplicationHosts.hosts.Delete(I);

          ReplHost.status:=MYX_RHS_NEW_HOST;
          ServerInformationListView.Selected.ImageIndex:=Ord(ReplHost.status);
          break;
        end;

      CheckUserReplHostBtns;

      if(FUserReplicationHosts<>nil)then
        myx_save_repl_user_hosts(FUserReplicationHosts.get_record_pointer,
          FUserReplicationHostsFilename);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminReplicationForm.RefreshBtnClick(Sender: TObject);

begin
  RefreshHostList;
  CheckUserReplHostBtns;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
