unit AdminServerConnections;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, InstanceSections, Menus,
  ApplicationDataModule, MySQLConnection, AuxFuncs,
  myx_public_interface, myx_admin_public_interface,
  PNGImage, MyxError, TntMenus, TntExtCtrls, TntStdCtrls,
  TntComCtrls, gnugettext;

type
  TAdminServerConnectionsForm = class(TInstanceSectionForm)
    ServerProcessesPnl: TTntPanel;
    BottomPnl: TTntPanel;
    UserPopupMenu: TTntPopupMenu;
    KillUserMI: TTntMenuItem;
    ServerProcessesPageControl: TTntPageControl;
    UserConnTabSheet: TTabSheet;
    ThreadsTabSheet: TTabSheet;
    ProcessPopupMenu: TTntPopupMenu;
    KillThreadMI: TTntMenuItem;
    ThreadsListView: TTntListView;
    Panel5: TTntPanel;
    ThreadsBevel: TTntBevel;
    Label8: TTntLabel;
    PageHeaderImg: TTntImage;
    UserConnPnl: TTntPanel;
    UserConnListView: TTntListView;
    UserConnThreadsListView: TTntListView;
    Splitter1: TTntSplitter ;
    Panel1: TTntPanel;
    UserConnBevel: TTntBevel;
    Label1: TTntLabel;
    PageHeader2Img: TTntImage;
    Label2: TTntLabel;
    Label3: TTntLabel;
    N1: TTntMenuItem;
    ShowUserInfoMI: TTntMenuItem;
    KillThreadBtn: TTntButton;
    KillUserBtn: TTntButton;
    RefreshBtn: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure InitializeControls; override;

    procedure RefreshProcessList;
    procedure RefreshUserList;
    procedure GetProcessList(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure ServerProcessesPageControlChange(Sender: TObject);
    procedure UserConnListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure UserConnThreadsListViewChange(Sender: TObject;
      Item: TListItem; Change: TItemChange);
    procedure ThreadsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure KillThreadBtnClick(Sender: TObject);
    procedure KillUserBtnClick(Sender: TObject);
    procedure UserPopupMenuPopup(Sender: TObject);

    procedure RefreshProcessLists(Sender: TObject);
    procedure ShowUserInfoMIClick(Sender: TObject);

    procedure ShowHideUserListViewColumns;

    procedure RefreshUserThreads(PrevID: WideString = '');

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
  private
    FProcessList: TMYX_PROCESS_LIST;
    FShowUserNameAndDescription: Boolean;
    FServerConnectionsPNGImg: TPNGObject;
  end;

var
  AdminServerConnectionsForm: TAdminServerConnectionsForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  PNGTools;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.FormCreate(Sender: TObject);

begin
  InitForm(self);
  
  DockedPanel:=ServerProcessesPnl;

  FProcessList:=nil;

  ServerProcessesPageControl.ActivePageIndex:=0;

  FServerConnectionsPNGImg:=LoadPNGImageFromResource('server_connections', PageHeaderImg);
  PageHeader2Img.Picture.Assign(FServerConnectionsPNGImg);

  FShowUserNameAndDescription:=False;

  ShowHideUserListViewColumns;

  if(Not(MySQLConn.Connected))then
  begin
    DisableEnableControls(ThreadsTabSheet, False);
    DisableEnableControls(UserConnTabSheet, False);

    DisableEnableControls(BottomPnl, False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.FormDestroy(Sender: TObject);

begin
  if(FProcessList<>nil)then
    FProcessList.Free;

  FServerConnectionsPNGImg.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.InitializeControls;

begin
  RefreshProcessList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.GetProcessList(Sender: TObject);

var
  ProcessList: PMYX_PROCESS_LIST;

begin
  ProcessList:=myx_get_process_list(TFetchDataThread(Sender).Connection.MySQL);

  if(ProcessList=nil)then
    raise EMyxSQLError.Create(_('Could not fetch Process List.'),
      myx_mysql_errno(TFetchDataThread(Sender).Connection.MySQL),
      myx_mysql_error(TFetchDataThread(Sender).Connection.MySQL));

  FProcessList:=TMYX_PROCESS_LIST.create(ProcessList);
  myx_free_process_list(ProcessList);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.RefreshProcessLists(Sender: TObject);

begin
  RefreshProcessList;
  RefreshUserList;

  if(ServerProcessesPageControl.ActivePage=ThreadsTabSheet)then
    KillThreadBtn.Enabled:=ThreadsListView.Selected<>nil
  else
    KillThreadBtn.Enabled:=UserConnThreadsListView.Selected<>nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.RefreshProcessList;

var
  I: integer;
  Item: TTntListItem;
  PrevID: WideString;

begin
  if(Not(MySQLConn.Connected))then
    Exit;

  if(FProcessList=nil)then
    MySQLConn.FetchData(dkProcessList,
      GetProcessList, RefreshProcessLists,
      nil, _('Fetching Process List ...'))
  else
  begin
    ThreadsListView.Items.BeginUpdate;
    try
      if(ThreadsListView.Selected<>nil)then
        PrevID:=ThreadsListView.Selected.Caption
      else
        PrevID:='';

      ThreadsListView.Clear;


      for I:=0 to FProcessList.process_infos.Count-1 do
      begin
        Item:=AddListViewItem(ThreadsListView, nil,
          FProcessList.process_infos[I].id, 22, FProcessList.process_infos[I]);
        Item.SubItems.Add(FProcessList.process_infos[I].user);
        Item.SubItemImages[0]:=14;
        Item.SubItems.Add(FProcessList.process_infos[I].host);
        Item.SubItems.Add(FProcessList.process_infos[I].db);
        Item.SubItems.Add(FProcessList.process_infos[I].command);
        Item.SubItems.Add(FProcessList.process_infos[I].time);
        Item.SubItems.Add(FProcessList.process_infos[I].state);
        Item.SubItems.Add(FProcessList.process_infos[I].info);

        if(PrevID=FProcessList.process_infos[I].id)then
        begin
          ThreadsListView.Selected:=Item;
        end;
      end;
    finally
      ThreadsListView.Items.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.RefreshUserList;

var
  I, J: Integer;
  UserConnItem: TTntListItem;
  PUser: PMYX_USER;
  User: TMYX_USER;
  ConnectionCount: integer;
  FullUserName, Description,
  PrevUser, PrevID: WideString;
  
begin
  if(FProcessList<>nil)then
  begin
    UserConnListView.Items.BeginUpdate;
    InitControls:=True;
    try
      if(UserConnListView.Selected<>nil)then
        PrevUser:=UserConnListView.Selected.Caption
      else
        PrevUser:='';

      if(UserConnThreadsListView.Selected<>nil)then
        PrevID:=UserConnThreadsListView.Selected.Caption
      else
        PrevID:='';

      UserConnListView.Clear;
      UserConnThreadsListView.Clear;

      for I:=0 to FProcessList.process_infos.Count-1 do
      begin
        //Look if the user has already been added to the list
        UserConnItem:=nil;
        for J:=0 to UserConnListView.Items.Count-1 do
          if(UserConnListView.Items[J].Caption=FProcessList.process_infos[I].user)then
          begin
            UserConnItem:=UserConnListView.Items[J];
            break;
          end;

        // If not there already add the user.
        if UserConnItem = nil then
        begin
          FullUserName := '';
          Description := '';
          User := nil;

          if FShowUserNameAndDescription then
          begin
            PUser := myx_get_user(MySQLConn.MySQL,FProcessList.process_infos[I].user);
            if Assigned(PUser) then
            begin
              User := TMYX_USER.create(PUser);
              try
                FullUserName := User.full_name;
                Description := User.description;
              finally
                User.Free;
                myx_free_user(PUser);
              end;
            end;
          end;

          UserConnItem := AddListViewItem(UserConnListView, nil, FProcessList.process_infos[I].user, 14, User);

          ConnectionCount:=0;
          for J := 0 to FProcessList.process_infos.Count - 1 do
            if FProcessList.process_infos[J].user = FProcessList.process_infos[I].user then
              Inc(ConnectionCount);

          UserConnItem.SubItems.Add(IntToStr(ConnectionCount));

          if FShowUserNameAndDescription then
          begin
            UserConnItem.SubItems.Add(FullUserName);
            UserConnItem.SubItems.Add(Description);
          end;

          if PrevUser = FProcessList.process_infos[I].user then
          begin
            UserConnListView.Selected:=UserConnItem;

            RefreshUserThreads(PrevID);
          end;
        end;
      end;
    finally
      UserConnListView.Items.EndUpdate;
      InitControls:=False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.RefreshUserThreads(PrevID: WideString);

var i: integer;
  NewItem: TTntListItem;
begin
  KillUserBtn.Enabled:=(UserConnListView.Selected<>nil);

  UserConnThreadsListView.Items.BeginUpdate;
  try
    UserConnThreadsListView.Clear;

    if(UserConnListView.Selected<>nil)then
    begin
      for i:=0 to FProcessList.process_infos.Count-1 do
        if(UserConnListView.Selected.Caption=
          FProcessList.process_infos[i].user)then
        begin
          NewItem:=AddListViewItem(UserConnThreadsListView, nil,
            FProcessList.process_infos[i].id, 22, FProcessList.process_infos[i]);
          NewItem.SubItems.Add(FProcessList.process_infos[i].user);
          NewItem.SubItemImages[0]:=14;
          NewItem.SubItems.Add(FProcessList.process_infos[i].host);
          NewItem.SubItems.Add(FProcessList.process_infos[i].db);
          NewItem.SubItems.Add(FProcessList.process_infos[i].command);
          NewItem.SubItems.Add(FProcessList.process_infos[i].time);
          NewItem.SubItems.Add(FProcessList.process_infos[i].state);
          NewItem.SubItems.Add(FProcessList.process_infos[i].info);

          if(PrevID=FProcessList.process_infos[i].id)then
            UserConnThreadsListView.Selected:=NewItem;
        end;
    end;
  finally
    UserConnThreadsListView.Items.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.UserConnListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);

begin
  if not (InitControls) and not (csDestroying in ComponentState) then
    RefreshUserThreads;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.RefreshBtnClick(Sender: TObject);

begin
  //Clear current ProcessList
  if(FProcessList<>nil)then
  begin
    FProcessList.Free;
    FProcessList:=nil;
  end;

  //Get new one
  RefreshProcessList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ServerProcessesPageControlChange(Sender: TObject);

begin
  if(ServerProcessesPageControl.ActivePage=ThreadsTabSheet)then
  begin
    KillThreadBtn.Enabled:=ThreadsListView.Selected<>nil;
    KillUserBtn.Enabled:=False;
  end
  else
  begin
    KillThreadBtn.Enabled:=UserConnThreadsListView.Selected<>nil;
    KillUserBtn.Enabled:=UserConnListView.Selected<>nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.UserConnThreadsListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);

begin
  if not (csDestroying in ComponentState) then
    KillThreadBtn.Enabled:= Assigned(UserConnThreadsListView.Selected);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ThreadsListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);

begin
  if not (csDestroying in ComponentState) then
    KillThreadBtn.Enabled:= Assigned(ThreadsListView.Selected);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.KillThreadBtnClick(Sender: TObject);

var
  I: integer;
  CurrentID: integer;
  
begin
  CurrentID:=myx_get_thread_id(MySQLConn.MySQL);

  if(ServerProcessesPageControl.ActivePage=ThreadsTabSheet)then
  begin
    for I:=0 to ThreadsListView.Items.Count-1 do
      if(ThreadsListView.Items[I].Selected)then
        if(CurrentID<>StrToInt(ThreadsListView.Items[I].Caption))then
          myx_kill_thread(MySQLConn.MySQL, StrToInt(ThreadsListView.Items[I].Caption));

  end
  else
    for I:=0 to UserConnThreadsListView.Items.Count-1 do
      if(UserConnThreadsListView.Items[I].Selected)then
        if(CurrentID<>StrToInt(UserConnThreadsListView.Items[I].Caption))then
          myx_kill_thread(MySQLConn.MySQL, StrToInt(UserConnThreadsListView.Items[I].Caption));

  RefreshBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.KillUserBtnClick(Sender: TObject);

var
  I, J: integer;
  CurrentID: integer;

begin
  if(UserConnListView.SelCount>0)then
  begin
    CurrentID:=myx_get_thread_id(MySQLConn.MySQL);

    for I:=0 to UserConnListView.Items.Count-1 do
      if(UserConnListView.Items[I].Selected)then
      begin
        for J:=0 to FProcessList.process_infos.Count-1 do
          if(FProcessList.process_infos[J].user=UserConnListView.Items[I].Caption)and
            (CurrentID<>StrToInt(FProcessList.process_infos[J].id))then
            myx_kill_thread(MySQLConn.MySQL, StrToInt(FProcessList.process_infos[J].id));
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.UserPopupMenuPopup(Sender: TObject);

begin
  KillUserMI.Enabled := UserConnListView.SelCount > 0;
  ShowUserInfoMI.Enabled := myx_check_mysql_user_info_table(MySQLConn.MySQL, 1) <> 0;
  if ShowUserInfoMI.Enabled then
    ShowUserInfoMI.Checked := FShowUserNameAndDescription
  else
    ShowUserInfoMI.Checked := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ShowUserInfoMIClick(Sender: TObject);

begin
  FShowUserNameAndDescription := not FShowUserNameAndDescription;
  ShowHideUserListViewColumns;
  RefreshUserList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ShowHideUserListViewColumns;

var
  ListColumn: TListColumn;

begin
  if not FShowUserNameAndDescription then
  begin
    if(UserConnListView.Columns.Count>2)then
      UserConnListView.Columns.Delete(2);
    if(UserConnListView.Columns.Count>2)then
      UserConnListView.Columns.Delete(2);

    UserConnListView.Columns[1].AutoSize:=True;
  end
  else
  begin
    UserConnListView.Columns[1].AutoSize:=False;
    UserConnListView.Columns[1].Width:=80;

    ListColumn:=UserConnListView.Columns.Add;
    ListColumn.Caption:=_('Full Name');
    ListColumn.Width:=150;

    ListColumn:=UserConnListView.Columns.Add;
    ListColumn.Caption:=_('Description');
    ListColumn.Width:=135;
    ListColumn.AutoSize:=True;

    ListColumn.Width:=UserConnListView.Width-24-
      (UserConnListView.Columns[0].Width+
      UserConnListView.Columns[1].Width+
      UserConnListView.Columns[2].Width);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ConnectionLost(var Message: TMessage);

var
  I: Integer;

begin
  for I:=0 to ServerProcessesPageControl.PageCount-1 do
    DisableEnableControls(ServerProcessesPageControl.Pages[I], False);

  DisableEnableControls(BottomPnl, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerConnectionsForm.ConnectionEstablished(var Message: TMessage);

var
  I: Integer;

begin
  for I:=0 to ServerProcessesPageControl.PageCount-1 do
    DisableEnableControls(ServerProcessesPageControl.Pages[I], True);

  DisableEnableControls(BottomPnl, True);

  RefreshBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
