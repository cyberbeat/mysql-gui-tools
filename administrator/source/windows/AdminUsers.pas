unit AdminUsers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, TntForms,
  TntDialogs, ExtCtrls, ComCtrls, TntStdCtrls, StrUtils,
  Buttons, Menus, Contnrs, ApplicationDataModule, SyncObjs,
  AuxFuncs, myx_public_interface, myx_admin_public_interface,
  MySQLConnection, AdvancedEdit, Grids, ValEdit, SchemataTreeView,
  Sections, InstanceSections, PNGImage, Options, MyxError, TntSysUtils,
  VirtualTrees, TntMenus, TntExtCtrls, TntComCtrls, Forms, StdCtrls,
  TntButtons, gnugettext, ActnList, XPStyleActnCtrls, ActnMan;

type
  TAdminUsersForm = class(TInstanceSectionForm)
    UserPnl: TTntPanel;
    UserPageControl: TTntPageControl;
    SchemataPrivSheet: TTabSheet;
    Label7: TTntLabel;
    Label6: TTntLabel;
    Panel2: TTntPanel;
    DBBevel: TTntBevel;
    SchemaPrivNameLbl: TTntLabel;
    Header3Img: TTntImage;
    AssignSchemaPrivBtn: TTntBitBtn;
    RemoveSchemaPrivBtn: TTntBitBtn;
    SchemaPrivAvailListView: TTntListView;
    SchemaPrivAssignedListView: TTntListView;
    TablePrivSheet: TTabSheet;
    Panel5: TTntPanel;
    TableBevel: TTntBevel;
    TableColumnPrivNameLbl: TTntLabel;
    Header4Img: TTntImage;
    SubTreePnl: TTntPanel;
    UserTreeView: TTntTreeView;
    Splitter1: TTntSplitter ;
    UserInfoSheet: TTabSheet;
    Panel1: TTntPanel;
    UserInfoBevel: TTntBevel;
    UserInfoNameLbl: TTntLabel;
    HeaderImg: TTntImage;
    UserInfoScrollBox: TTntScrollBox;
    UserInfoLoginGBox: TTntGroupBox;
    Label27: TTntLabel;
    Label28: TTntLabel;
    Label3: TTntLabel;
    Label11: TTntLabel;
    Label12: TTntLabel;
    Label13: TTntLabel;
    UserNameEd: TTntEdit;
    PasswordEd: TTntEdit;
    PasswordConfirmationEd: TTntEdit;
    UserInfoAdditionalGBox: TTntGroupBox;
    Label23: TTntLabel;
    Label25: TTntLabel;
    Label24: TTntLabel;
    Label26: TTntLabel;
    Label29: TTntLabel;
    Label30: TTntLabel;
    FullNameEd: TTntEdit;
    DescriptionEd: TTntEdit;
    LoadImgBtn: TTntBitBtn;
    Panel7: TTntPanel;
    UserIcon: TTntImage;
    UserSubTreePopupMenu: TTntPopupMenu;
    CloneUserMI: TTntMenuItem;
    AddHostMI: TTntMenuItem;
    N1: TTntMenuItem;
    AddUserMI: TTntMenuItem;
    DeleteUserMI: TTntMenuItem;
    GlobalPrivSheet: TTabSheet;
    Panel9: TTntPanel;
    GlobalBevel: TTntBevel;
    GlobalPrivNameLbl: TTntLabel;
    Header2Img: TTntImage;
    Label15: TTntLabel;
    AssignGlobalPrivBtn: TTntBitBtn;
    RemoveGlobalPrivBtn: TTntBitBtn;
    GlobalPrivListView: TTntListView;
    GlobalAssignedPrivListView: TTntListView;
    Label16: TTntLabel;
    Panel3: TTntPanel;
    BottomBtnPnl: TTntPanel;
    DiscardChangesBtn: TTntButton;
    NewUserBtn: TTntButton;
    SubTreeSearchPnl: TTntPanel;
    Label1: TTntLabel;
    Label5: TTntLabel;
    Label18: TTntLabel;
    Label19: TTntLabel;
    EmailEd: TTntEdit;
    Label20: TTntLabel;
    ContactInfoMemo: TTntMemo;
    UserInfoLbl: TTntLabel;
    GlobalPrivInfoLbl: TTntLabel;
    SchemaPrivInfoLbl: TTntLabel;
    TableColumnPrivInfoLbl: TTntLabel;
    N3: TTntMenuItem;
    RefreshUserListMI: TTntMenuItem;
    ApplyChangesBtn: TTntButton;
    AdvancedEdit: TAdvancedEditFrame;
    Panel4: TTntPanel;
    SchemaPrivSchemataFrame: TSchemataFrame;
    ResourceSheet: TTabSheet;
    Panel6: TTntPanel;
    ResourceBevel: TTntBevel;
    ResourceNameLbl: TTntLabel;
    Header5Img: TTntImage;
    ResourcesInfoLbl: TTntLabel;
    ScrollBox2: TTntScrollBox;
    UserResourceLimitGBox: TTntGroupBox;
    RemoveHostMI: TTntMenuItem;
    Label2: TTntLabel;
    Label4: TTntLabel;
    AssignTblColPrivBtn: TTntBitBtn;
    RemoveTblColPrivBtn: TTntBitBtn;
    TablePrivAvailListView: TTntListView;
    TablePrivAssignedListView: TTntListView;
    TblColSchemataFrame: TSchemataFrame;
    AssignAllGlobalPrivBtn: TTntBitBtn;
    RemoveAllGlobalPrivBtn: TTntBitBtn;
    AssignAllSchemaPrivBtn: TTntBitBtn;
    RemoveAllSchemaPrivBtn: TTntBitBtn;
    AssignAllTblColPrivBtn: TTntBitBtn;
    RemoveAllTblColPrivBtn: TTntBitBtn;
    ClearUserImg: TTntBitBtn;
    CatalogTreePopup: TTntPopupMenu;
    RefreshCatalogTreeMI: TTntMenuItem;
    N4: TTntMenuItem;
    AddSchemawithWidcardsMI: TTntMenuItem;
    ShowHostsMI: TTntMenuItem;
    UserActionManager: TActionManager;
    AddUserAction: TAction;
    ApplyAction: TAction;
    DiscardAction: TAction;
    CloneUserAction: TAction;
    DeleteUserAction: TAction;
    AddHostAction: TAction;
    RemoveHostAction: TAction;
    ShowHostsAction: TAction;
    RefreshAction: TAction;
    SectionDisabledLabel: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure InitializeControls; override;

    procedure RefreshUserTree(SearchText: WideString = ''; RestoreSelection: Boolean = True);

    procedure UserPageControlChange(Sender: TObject);
    procedure UserPnlResize(Sender: TObject);

    procedure AssignGlobalPrivBtnClick(Sender: TObject);
    procedure MovePriv(SourceListView: TTntListView;
      DestListView: TTntListView;
      priv_object_name: WideString; AssignPrivilege: Boolean);
    procedure RemoveGlobalPrivBtnClick(Sender: TObject);
    procedure AdvancedSchemaEditSearchEdChange(Sender: TObject);
    procedure CreateNewUser(Clone: TMYX_USER = nil);
    procedure RefreshSchemaPrivileges(Schema: TMYX_SCHEMA);
    procedure AssignSchemaPrivBtnClick(Sender: TObject);
    procedure RemoveSchemaPrivBtnClick(Sender: TObject);
    procedure SchemataFrameSchemaTreeViewExpanding(Sender: TObject;
      Node: TTntTreeNode; var AllowExpansion: Boolean);
    procedure UserNameEdChange(Sender: TObject);
    procedure UserTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure LoadImgBtnClick(Sender: TObject);

    procedure CurrentUserChanged;

    procedure UserTreeViewChange(Sender: TObject; Node: TTreeNode);

    procedure FetchUserData(Sender: TObject);
    procedure UserDataFetched(Sender: TObject);

    procedure ApplyChanges;

    function DiscardChanges(AskToSaveChanges: Boolean; RefreshTheDisplayedUser: Boolean = True): Integer;

    procedure ClearUsers;
    procedure ClearUserControls;
    procedure DeleteUser(UserName: WideString);
    procedure DisableEnableUserPageControl(Enable: Boolean);

    procedure OnResourceEditChange(Sender: TObject);
    procedure UserSubTreePopupMenuPopup(Sender: TObject);
    procedure AdvancedEditSearchEdChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure GetUserNameList(Sender: TObject);
    procedure RefreshUserNameList(Sender: TObject);

    procedure GetSchemaTables(Sender: TObject);
    procedure TblColSchemataFrameSchemaTreeViewExpanding(Sender: TObject;
      Node: TTntTreeNode; var AllowExpansion: Boolean);

    procedure RefreshTablePrivileges(Schema: TMYX_SCHEMA;
      SchemaTable: TMYX_SCHEMA_TABLE;
      SchemaTableColumn: TMYX_SCHEMA_TABLE_COLUMN);
    procedure RefreshProcPrivileges(Schema: TMYX_SCHEMA;
      SchemaProc: TMYX_SCHEMA_STORED_PROCEDURE);
    procedure AssignTblColPrivBtnClick(Sender: TObject);
    procedure RemoveTblColPrivBtnClick(Sender: TObject);

    procedure ClearGlobalPrivListViews;
    procedure ClearSchemaPrivListViews;
    procedure ClearTableColumnPrivListViews;

    procedure ApplyOptionSettings;
    procedure OptionsChanged(var Message: TMessage); message WM_OptionsChanged;
    procedure AssignAllGlobalPrivBtnClick(Sender: TObject);
    procedure RemoveAllGlobalPrivBtnClick(Sender: TObject);
    procedure AssignAllSchemaPrivBtnClick(Sender: TObject);
    procedure RemoveAllSchemaPrivBtnClick(Sender: TObject);
    procedure AssignAllTblColPrivBtnClick(Sender: TObject);
    procedure RemoveAllTblColPrivBtnClick(Sender: TObject);

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure SchemaListChanged(var Message: TMessage); message WM_SchemaListChanged;
    procedure ClearUserImgClick(Sender: TObject);
    procedure SchemaPrivSchemataFrameCatalogVSTChange(
      Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TblColSchemataFrameCatalogVSTChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure AddSchemawithWidcardsMIClick(Sender: TObject);
    procedure RefreshCatalogTreeMIClick(Sender: TObject);

    function GetCurrentUserSchema: TMYX_SCHEMA;
    function GetCurrentUserTblColSchema: TMYX_SCHEMA;
    function GetCurrentUserTable: TMYX_SCHEMA_TABLE;
    function GetCurrentUserProc: TMYX_SCHEMA_STORED_PROCEDURE;
    function GetCurrentUserColumn: TMYX_SCHEMA_TABLE_COLUMN;
    procedure UserTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure AddUserActionExecute(Sender: TObject);
    procedure UserActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DeleteUserActionExecute(Sender: TObject);
    procedure AddHostActionExecute(Sender: TObject);
    procedure RemoveHostActionExecute(Sender: TObject);
    procedure CloneUserActionExecute(Sender: TObject);
    procedure ShowHostsActionExecute(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure ApplyActionExecute(Sender: TObject);
    procedure DiscardActionExecute(Sender: TObject);
    procedure SchemaPrivSchemataFrameRefreshCatalogsSchemataListMIClick(Sender: TObject);
    procedure TblColSchemataFrameRefreshCatalogsSchemataListMIClick(Sender: TObject);
  private
    FCurrentUser: TMYX_USER;
    FCurrentHost: WideString;
    FClearingUsers: Boolean;

    FInitUserControls: Boolean;
    FDirty: Boolean;

    FUserIconPNGImage: TPNGObject;
    FHeaderPNGImg: TPNGObject;

    FConnectionLock: TCriticalSection;
    FNewUserPending: Boolean;
    FUserRenamed: Boolean;                    // Set when a new user was created and has not yet been saved.

    procedure LockConnection;
    procedure ReleaseConnection;
    procedure UpdateUserDisplay;
  public
    UserNameList: TMYX_USER_NAMES;
  end;

var
  AdminUsersForm: TAdminUsersForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Main, Math, PNGTools, TntClasses;

type
  PPrivilege = ^TPrivilege;
  TPrivilege = record
    id: WideString;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.FormCreate(Sender: TObject);

var
  UnsupportedServer: Boolean;

begin
  InitForm(Self);

  // Hide additional info for the user for servers equal or past 6.0 (not supported).
  UnsupportedServer := mysql_full_version_is_later_or_equal_than(MySQLConn.MySQL, 6, 0, 0) <> 0;
  UserInfoAdditionalGBox.Visible := not UnsupportedServer;

  FConnectionLock := TCriticalSection.Create;

  SchemaPrivSchemataFrame.MySQLConnection := MySQLConn;
  SchemaPrivSchemataFrame.ShowSchemaAssets := False;
  SchemaPrivSchemataFrame.SearchTargets := [smSchemata];
  SchemaPrivSchemataFrame.ShowEscapedNames := True;
  SchemaPrivSchemataFrame.FetchTables := False;

  TblColSchemataFrame.MySQLConnection := MySQLConn;
  TblColSchemataFrame.ShowAssetsOnSchemaExpansion := True;
  TblColSchemataFrame.PopupMenuEditEntries := False;

  DockedPanel := UserPnl;
  SubTreePanel := SubTreePnl;

  UserPageControl.ActivePageIndex := 0;

  GlobalPrivSheet.TabVisible := False;
  TablePrivSheet.TabVisible := False;
  ResourceSheet.TabVisible := False;

  UserPnlResize(self);

  UserNameList := nil;
  FUserIconPNGImage := nil;

  FCurrentUser := nil;
  FCurrentHost := '';

  FClearingUsers := False;
  FInitUserControls := False;

  UserTreeView.Items.Clear;

  ClearUserControls;
  DisableEnableUserPageControl(False);

  FDirty := False;

  FHeaderPNGImg := LoadPNGImageFromResource('user_admin', HeaderImg);
  Header2Img.Picture.Assign(FHeaderPNGImg);
  Header3Img.Picture.Assign(FHeaderPNGImg);
  Header4Img.Picture.Assign(FHeaderPNGImg);
  Header5Img.Picture.Assign(FHeaderPNGImg);

  ApplyOptionSettings;

  SectionDisabledLabel.Visible := UnsupportedServer;
  DisableEnablePages(UserPageControl, not UnsupportedServer);
  UserTreeview.Enabled := not UnsupportedServer;
  NewUserBtn.Enabled := not UnsupportedServer;
  ApplyChangesBtn.Enabled := not UnsupportedServer;
  DiscardChangesBtn.Enabled := not UnsupportedServer;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.FormDestroy(Sender: TObject);

begin
  ClearUsers;
  UserNameList.Free;
  FHeaderPNGImg.Free;
  FConnectionLock.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.InitializeControls;

begin
  RefreshUserTree('', False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.GetUserNameList(Sender: TObject);

var
  usernames: PMYX_USER_NAMES;

begin
  usernames := myx_get_user_names(TFetchDataThread(Sender).Connection.MySQL);
  UserNameList := TMYX_USER_NAMES.create(usernames);
  if usernames = nil then
    raise EMyxSQLError.Create(_('Could not fetch user names.'), myx_mysql_errno(TFetchDataThread(Sender).Connection.MySQL),
      myx_mysql_error(TFetchDataThread(Sender).Connection.MySQL))
    else
      myx_free_user_names(usernames);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshUserNameList(Sender: TObject);

begin
  RefreshUserTree(AdvancedEdit.SearchEd.Text, Assigned((TFetchDataThread(Sender).Target)));
  InitControls := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshUserTree(SearchText: WideString; RestoreSelection: Boolean);

var
  I: Integer;
  UserName,
  LastUserName: WideString;
  Node: TTntTreeNode;

begin
  if  MySQLConn.Connected then
  begin
    if UsernameList = nil then
    begin
      if RestoreSelection then
        Node := UserTreeView.Selected
      else
        Node := nil;

      MySQLConn.FetchData(dkUserNames, GetUserNameList, RefreshUserNameList, Node, _('Fetching user names ...'));
    end
    else
    begin
      // Store current values to restore selected node after refresh.
      if Assigned(FCurrentUser) then
        LastUserName := FCurrentUser.user_name;

      ClearUsers;

      for I := 0 to UserNameList.user_names.Count-1 do
      begin
        UserName := UserNameList.user_names[I];

        if SearchText <> '' then
          if myx_match_pattern(UserName, SearchText, 0, 1) = 0 then
            Continue;

        Node := AddTreeViewChildNode(UserTreeView, nil, UserName, 14);

        if RestoreSelection and (LastUserName = UserName) then
          UserTreeView.Selected := Node;
      end;

      if UserTreeView.Selected = nil then
        DisableEnableUserPageControl(False);

      Application.ProcessMessages;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ReleaseConnection;

begin
  FConnectionLock.Release;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserPageControlChange(Sender: TObject);

begin
  UserPnlResize(self);

  //Fetch Catalogs when Sheet becomes visible
  if(UserPageControl.ActivePage=SchemataPrivSheet)and
    ((SchemaPrivSchemataFrame.CatalogList=nil)or
    (SchemaPrivSchemataFrame.CatalogVST.RootNodeCount=0))then
    SchemaPrivSchemataFrame.FillSchemaTree;

  //Fetch Catalogs when Sheet becomes visible
  if(UserPageControl.ActivePage=TablePrivSheet)and
    ((TblColSchemataFrame.CatalogList=nil)or
    (TblColSchemataFrame.CatalogVST.RootNodeCount=0))then
    TblColSchemataFrame.FillSchemaTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserPnlResize(Sender: TObject);

var
  InitSheetWidth: Integer;
  InitSheetHeight: Integer;

begin
  InitSheetWidth := 563;
  InitSheetHeight := 439;

  //Adjust Page Control
  UserPageControl.Width := UserPnl.Width-20;
  UserPageControl.Height := UserPnl.Height-55;

  //Adjust User Information page
  UserInfoBevel.Width := UserInfoSheet.Width-(InitSheetWidth-535);

  //Adjust Global Priv Page
  GlobalBevel.Width := GlobalPrivSheet.Width-(InitSheetWidth-535);

  GlobalAssignedPrivListView.Height := GlobalPrivSheet.Height-(InitSheetHeight-359);

  GlobalPrivListView.Width := GlobalPrivSheet.Width-(InitSheetWidth-327);
  GlobalPrivListView.Height := GlobalPrivSheet.Height-(InitSheetHeight-359);

  //Adjust Schema Level Sheet
  DBBevel.Width := SchemataPrivSheet.Width-(InitSheetWidth-535);

  SchemaPrivSchemataFrame.Height := SchemataPrivSheet.Height-(InitSheetHeight-376);

  SchemaPrivAssignedListView.Height := SchemataPrivSheet.Height-(InitSheetHeight-359);

  SchemaPrivAvailListView.Width := SchemataPrivSheet.Width-(InitSheetWidth-187);
  SchemaPrivAvailListView.Height := SchemataPrivSheet.Height-(InitSheetHeight-359);

  //Adjust Table/Column Level Sheet
  TableBevel.Width := TablePrivSheet.Width-(InitSheetWidth-535);

  TblColSchemataFrame.Height := TablePrivSheet.Height-(InitSheetHeight-376);

  TablePrivAssignedListView.Height := TablePrivSheet.Height-(InitSheetHeight-359);

  TablePrivAvailListView.Width := TablePrivSheet.Width-(InitSheetWidth-143);
  TablePrivAvailListView.Height := TablePrivSheet.Height-(InitSheetHeight-359);

  //Adjust Resource Sheet
  ResourceBevel.Width := SchemataPrivSheet.Width-(InitSheetWidth-535);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ApplyActionExecute(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ApplyChanges;

var
  PreviousUser: TMYX_USER;
  UserNode: TTntTreeNode;

begin
  if Assigned(FCurrentUser) and FDirty then
  begin
    if PasswordEd.Text <> PasswordConfirmationEd.Text then
    begin
      UserPageControl.ActivePageIndex := 0;
      PasswordConfirmationEd.Text := '';
      PasswordConfirmationEd.SetFocus;

      ShowModalDialog(_('Password not confirmed'), _('The password was not confirmed correctly. Please enter it again.'),
        myx_mtError, _('OK'));
      Abort;
    end;

    if FUserRenamed then
    begin
      // Too long user name.
      if Length(UserNameEd.Text) > 16 then
      begin
        UserPageControl.ActivePageIndex := 0;
        ShowModalDialog(_('User name too long'), _('The user name is longer that 16 characters.'),
          myx_mtError, _('OK'));
        Abort;
      end;

      // Empty user name.
      if Trim(UserNameEd.Text) = '' then
      begin
        UserPageControl.ActivePageIndex := 0;
        ShowModalDialog(_('Empty user name'), _('Please enter a valid user name.'), myx_mtError, _('OK'));
        Abort;
      end;

      // If this is a new user then check if a user with the new name already exists.
      UserNode := UserTreeView.Items.GetFirstNode;
      while Assigned(UserNode) do
      begin
        if UserNode.Text = UserNameEd.Text then
        begin
          ShowModalDialog(_('Duplicate user name'), _('User with such name already exists. Please enter another name.'),
            myx_mtConfirmation, _('OK'));
          Abort;
        end;
        UserNode := UserNode.GetNextSibling;
      end;
    end;

    PreviousUser := TMYX_USER.Create;
    PreviousUser.user_name := FCurrentUser.user_name;

    // Find top level node for the current user.
    UserNode := UserTreeView.Selected;
    while Assigned(UserNode.Parent) do
      UserNode := UserNode.Parent;

    // Check if a new user has been created.
    if FUserRenamed then
    begin
      // Replace old user name in UserTreeView.
      UserNode.Text := UserNameEd.Text;

      UserNameList.user_names.Delete(UserNameList.user_names.IndexOf(PreviousUser.user_name));
      UserNameList.user_names.Add(UserNameEd.Text);

      PreviousUser.user_name := '';
    end;

    // If no host is active currently then we don't need to back current values up.
    if FCurrentHost <> '' then
    begin
      PreviousUser.user_name := FCurrentUser.user_name;
      PreviousUser.password := FCurrentUser.password;
      PreviousUser.full_name := FCurrentUser.full_name;
      PreviousUser.description := FCurrentUser.description;
      PreviousUser.email := FCurrentUser.email;
      PreviousUser.contact_information := FCurrentUser.contact_information;

      FCurrentUser.user_name := UserNameEd.Text;
      FCurrentUser.password := PasswordEd.Text;
      FCurrentUser.full_name := FullNameEd.Text;
      FCurrentUser.description := DescriptionEd.Text;
      FCurrentUser.email := EmailEd.Text;
      FCurrentUser.contact_information := ContactInfoMemo.Text;
    end;

    // Store changes in database.
    LockConnection;
    try
      if myx_set_user(MySQLConn.MySQL, FCurrentUser.get_record_pointer, PreviousUser.user_name,
        Ord(FNewUserPending)) <> 0 then
      begin
        // Failed - revert to old user info (if a host is currently active)
        if FCurrentHost <> '' then
        begin
          UserNameList.user_names[UserNameList.user_names.IndexOf(FCurrentUser.user_name)] := PreviousUser.user_name;

          FCurrentUser.user_name := PreviousUser.user_name;
          FCurrentUser.password := PreviousUser.password;
          FCurrentUser.full_name := PreviousUser.full_name;
          FCurrentUser.description := PreviousUser.description;
          FCurrentUser.email := PreviousUser.email;
          FCurrentUser.contact_information := PreviousUser.contact_information;
          UserNode.Text := FCurrentUser.user_name;
        end;

        PreviousUser.Free;

        if myx_mysql_errno(MySQLConn.MySQL) = 0 then
          raise EMyxError.Create(_('Error while storing the user information. The user might have been deleted.') + #13#10 +
            _('Please refresh the user list.'))
        else
          raise EMyxSQLError.Create(_('Error while storing the user information.'), myx_mysql_errno(MySQLConn.MySQL),
            myx_mysql_error(MySQLConn.MySQL));
      end
      else
      begin
        FNewUserPending := False;
        FUserRenamed := False;

        // Reconnect if the just saved user is the same as the one we connected with and its name or password
        // has been changed.
        if WideSameText(PreviousUser.user_name, MySQLConn.UserConnection.username) then
        begin
          if (FCurrentUser.user_name <> PreviousUser.user_name) or
            (FCurrentUser.password <> PreviousUser.password) then
          begin
            MySQLConn.Disconnect(False);
            MySQLConn.ConnectToServer(False);
            if not MySQLConn.Connected then
              Application.Terminate;
          end;
        end;
      end;
    finally
      ReleaseConnection;
    end;
    PreviousUser.Free;

    FDirty := False;
    UpdateUserDisplay;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.DiscardActionExecute(Sender: TObject);

begin
  DiscardChanges(False);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.DiscardChanges(AskToSaveChanges: Boolean; RefreshTheDisplayedUser: Boolean): Integer;

var
  res: Integer;

begin
  // Be pessemistic.
  Result := -1;

  if Assigned(FCurrentUser) and FDirty then
  begin
    if AskToSaveChanges then
    begin
      res := ShowModalDialog(_('Store changes?'),
        WideFormat(_('The user %s has been modified. Do you want to store the changes?'), [FCurrentUser.user_name]),
          myx_mtConfirmation, _('Yes') + #13#10 + _('No') + #13#10 + _('Abort'));

      if res = 1 then
      begin
        ApplyChanges;

        // If changes are successfully applied then FDirty flag should be false
        // otherwise there was a problem and we don't switch.
        if not FDirty then
          Result := 1;

        Exit;
      end
      else
        if res = 3 then
          Exit;
    end;

    // If the creation of a new User is canceled.
    if FNewUserPending then
    begin
      UserNameList.user_names.Delete(UserNameList.user_names.IndexOf(FCurrentUser.user_name));
      RefreshUserTree(AdvancedEdit.SearchEd.Text);
      FCurrentUser := nil;

      FDirty := False;
      FNewUserPending := False;
      ApplyChangesBtn.Enabled := False;
      DiscardChangesBtn.Enabled := False;
      Exit;
    end
    else
    begin
      // If an existing user has been modified.
      if Assigned(UserTreeView.Selected) and (UserTreeView.Selected.Data = FCurrentUser) then
        if not ApplicationDM.ApplicationIsTerminating and RefreshTheDisplayedUser then
        begin
          FDirty := False;
          RefreshAction.Execute;
        end;

      ClearUserControls;
    end;

    FDirty := False;
    ApplyChangesBtn.Enabled := False;
    DiscardChangesBtn.Enabled := False;
  end;

  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearUserControls;

begin
  if FCurrentUser = nil then
    UserInfoNameLbl.Caption := _('No user selected')
  else
    UserInfoNameLbl.Caption := _('User without any-host (%) entry selected');
  GlobalPrivNameLbl.Caption := UserInfoNameLbl.Caption;
  SchemaPrivNameLbl.Caption := UserInfoNameLbl.Caption;
  TableColumnPrivNameLbl.Caption := UserInfoNameLbl.Caption;
  ResourceNameLbl.Caption := UserInfoNameLbl.Caption;

  UserNameEd.Text := '';
  PasswordEd.Text := '';
  PasswordConfirmationEd.Text := '';
  FullNameEd.Text := '';
  DescriptionEd.Text := '';
  EmailEd.Text := '';
  ContactInfoMemo.Text := '';

  ClearGlobalPrivListViews;
  ClearSchemaPrivListViews;
  ClearTableColumnPrivListViews;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.DisableEnableUserPageControl(Enable: Boolean);

var
  I: Integer;

begin
  for I := 0 to UserPageControl.PageCount-1 do
    DisableEnableControls(UserPageControl.Pages[I], Enable);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);

begin
  AllowChange := not FClearingUsers and (MySQLConn.DataBeingFetched = []);
  if AllowChange then
  begin
    // If a different node (either another user or another host) is selected
    // then ask if changes should be applied before switching.
    if Assigned(Node) and FDirty then
      try
        AllowChange := DiscardChanges(True, False) <> -1;
      except
        on EAbort do
          AllowChange := False;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not (InitControls or FClearingUsers) then
  begin
    // If no user is selected then clear controls.
    if Node = nil then
    begin
      FCurrentUser := nil;
      FCurrentHost := '';

      ClearUserControls;
      DisableEnableUserPageControl(False);
    end
    else
    begin
      // If there is no user data assigned to the node yet then fetch the data and
      // create childnodes for the user's hosts.
      if Node.Data = nil then
      begin
        SchemaPrivSchemataFrame.SchemaPatterns.Clear;
        MySQLConn.FetchData(dkUserData, FetchUserData, UserDataFetched, Node, _('Fetching User Data ...'));
      end
      else
      begin
        FCurrentUser := Node.Data;
        if Node.Level = 0 then
          FCurrentHost := FCurrentUser.hosts[0]
        else
          FCurrentHost := FCurrentUser.hosts[Node.Index + 1];
        UpdateUserDisplay;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.FetchUserData(Sender: TObject);

var
  Puser: PMYX_USER;

begin
  // First remove all redundant entries.
  myx_compact_privs(MySQLConn.MySQL);

  // Then get what left over.
  PUser := myx_get_user_with_privileges(MySQLConn.MySQL, TTntTreeNode(TFetchDataThread(Sender).Target).Text);

  // Don't throw an exception in the fetch thread. Instead let the main thread handle error condition.
  if Assigned(PUser) then
  try
    TTntTreeNode(TFetchDataThread(Sender).Target).Data := TMYX_USER.create(Puser);
  finally
    myx_free_user(PUser);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function HostCompare(List: TTntStringList; Index1, Index2: Integer): Integer;

// Helper function to sort host entries.

var
  Left: WideString;
  Right: WideString;
  
begin
  // Sort host entries so that always % is the first entry (if there is such one) then "localhost" and then
  // all others (alphabetically).
  if Index1 = Index2 then
    Result := 0
  else
  begin
    Left := List[Index1];
    if Left = '%' then
      Left := #1;
    if Left = 'localhost' then
      Left := #2;
    Right := List[Index2];
    if Right = '%' then
      Right := #1;
    if Right = 'localhost' then
      Right := #2;

    Result := WideCompareText(Left, Right);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);

var
  ConnectedWithUser: Boolean;

begin
  Handled := True;

  ConnectedWithUser := MySQLConn.Connected and Assigned(FCurrentUser);

  ApplyAction.Enabled := MySQLConn.Connected;
  DiscardAction.Enabled := MySQLConn.Connected;
  AddUserAction.Enabled := MySQLConn.Connected and not FNewUserPending and (Length(AdvancedEdit.SearchEd.Text) = 0);
  CloneUserAction.Enabled := ConnectedWithUser and not FNewUserPending;
  DeleteUserAction.Enabled := ConnectedWithUser;

  AddHostAction.Enabled := ConnectedWithUser and Assigned(UserTreeView.Selected);
  RemoveHostAction.Enabled := AddHostAction.Enabled and
    (Assigned(UserTreeView.Selected.Parent) or (FCurrentUser.hosts[0] <> ''));

  ShowHostsAction.Enabled := MySQLConn.Connected;
  ShowHostsAction.Checked := ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'];
  RefreshAction.Enabled := MySQLConn.Connected;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserDataFetched(Sender: TObject);

var
  Node: TTntTreeNode;
  I: Integer;
  User: TMYX_USER;

begin
  try
    Node := TTntTreeNode(TFetchDataThread(Sender).Target);

    if Assigned(Node.Data) then
    begin
      User := Node.Data;
      User.hosts.CustomSort(HostCompare);

      // If there is no any host entry then add a dummy one.
      if User.hosts[0] <> '%' then
        User.hosts.Insert(0, '');

      FCurrentUser := User;
      FCurrentHost := User.hosts[0];

      if ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
      begin
        for I := 1 to User.hosts.Count - 1 do
          AddTreeViewChildNode(UserTreeView, Node, User.hosts[I], 22, User);

        if Node.HasChildren then
          Node.Expand(False);
      end;

      UpdateUserDisplay;
    end;
  except
    DisableEnableUserPageControl(False);
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.OnResourceEditChange(Sender: TObject);

var
  I, J: Integer;

begin
  if(FCurrentUser<>nil)and(Not(FInitUserControls))then
  begin
    //Get User Privileges
    for I := 0 to FCurrentUser.user_object_privileges.Count-1 do
    begin
      //if object_name is '', these are the global privileges
      if(FCurrentUser.user_object_privileges[I].host=FCurrentHost)and
        (FCurrentUser.user_object_privileges[I].object_name='')then
      begin
        J := FCurrentUser.user_object_privileges[I].user_privileges.IndexOfName(
          Copy(TTntEdit(Sender).Name, 1, Length(TTntEdit(Sender).Name)-2));
        if(J>-1)then
          FCurrentUser.user_object_privileges[I].user_privileges.ValueFromIndex[J] :=
            IntToStr(StrToIntDef(TTntEdit(Sender).Text, 0));

        CurrentUserChanged;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearUsers;

var
  I: Integer;

begin
  if Assigned(UserTreeView) then
  begin
    FClearingUsers := True;
    try
      for I := 0 to UserTreeView.Items.Count - 1 do
        if Assigned(UserTreeView.Items[I].Data) then
        begin
          if UserTreeView.Items[I].Level = 0 then
            TMYX_USER(UserTreeView.Items[I].Data).Free;
          UserTreeView.Items[I].Data := nil;
        end;

      FCurrentUser := nil;
      FCurrentHost := '';
      UserTreeView.Items.Clear;
      ClearUserControls;
    finally
      FClearingUsers := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.CloneUserActionExecute(Sender: TObject);

begin
  CreateNewUser(FCurrentUser);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.MovePriv(SourceListView: TTntListView; DestListView: TTntListView; priv_object_name: WideString;
  AssignPrivilege: Boolean);

var
  I, J: Integer;
  Item: TListItem;
  UserPriv: TMYX_USER_OBJECT_PRIVILEGES;

begin
  if(FCurrentUser<>nil)and(SourceListView.SelCount>0)then
  begin
    //Find privileges by priv_object_name
    UserPriv := nil;
    for I := 0 to FCurrentUser.user_object_privileges.Count-1 do
    begin
      if(FCurrentUser.user_object_privileges[I].host=FCurrentHost)and
        (FCurrentUser.user_object_privileges[I].object_name=priv_object_name)then
      begin
        UserPriv := FCurrentUser.user_object_privileges[I];
        break;
      end;
    end;

    if(UserPriv<>nil)then
    begin
      I := SourceListView.Items.IndexOf(SourceListView.Selected);
      while(I<SourceListView.Items.Count)do
      begin
        if(SourceListView.Items[I].Selected)and
          (SourceListView.Items[I].Data<>nil)then
        begin
          for J := 0 to UserPriv.user_privileges.Count-1 do
            if(UserPriv.user_privileges.Names[J]=PPrivilege(SourceListView.Items[I].Data).id)then
            begin
              if(AssignPrivilege)then
                UserPriv.user_privileges.ValueFromIndex[J] := 'Y'
              else
                UserPriv.user_privileges.ValueFromIndex[J] := 'N';

              Item := AddListViewItem(DestListView, nil,
                SourceListView.Items[I].Caption,
                15, SourceListView.Items[I].Data);

              Item.SubItems.Assign(SourceListView.Items[I].SubItems);

              SourceListView.Items.Delete(I);

              break;
            end;
        end
        else
          inc(I);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignGlobalPrivBtnClick(Sender: TObject);

begin
  if GlobalPrivListView.SelCount > 0 then
  begin
    MovePriv(GlobalPrivListView, GlobalAssignedPrivListView, '', True);

    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveGlobalPrivBtnClick(Sender: TObject);

begin
  if GlobalAssignedPrivListView.SelCount > 0 then
  begin
    MovePriv(GlobalAssignedPrivListView, GlobalPrivListView, '', False);
    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveHostActionExecute(Sender: TObject);

var
  I: Integer;
  Node: TTntTreeNode;
  DialogResult: Integer;
  Host: WideString;

begin
  if not ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
  begin
    if ShowModalDialog(_('Option change'), WideFormat(_('In order to view and edit host entries they must be switched' +
      ' on in the user management view by enabling the global option "%s".') + #13#10#13#10 + _('Do you want this?'),
      [_('Show hosts in user list')]), myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1 then
    begin
      ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] := True;
      RefreshUserTree(AdvancedEdit.SearchEd.Text, True);
    end;
  end
  else
  begin
    if (FCurrentUser.hosts.Count = 1) or
      ((FCurrentUser.hosts.Count = 2) and (FCurrentUser.hosts[0] = '')) then
    begin
      DialogResult := ShowModalDialog(_('No hosts left'),
        _('This is the last host from which the user is allowed to connect. Removing this host means deleting this user.') +
          #13#10#13#10 + _('Do you want this?'), myx_mtConfirmation, _('Continue') + #13#10 + _('Cancel'));
      if DialogResult = 1 then
        DeleteUserAction.Execute;
    end
    else
    begin
      Node := UserTreeView.Selected;

      if Node.Parent = nil then
      begin
        // Special case any-host (%).
        FCurrentUser.hosts[0] := '';
        Host := '%';

        // Tell the user. The change is not obvious.
        ShowModalDialog(_('Host entry deleted'), _('The any-host (%) entry has been deleted.'), myx_mtInformation, _('OK'));
      end
      else
      begin
        Host := Node.Text;
        FCurrentUser.hosts.Delete(FCurrentUser.hosts.IndexOf(Host));
      end;

      I := 0;
      while I < FCurrentUser.user_object_privileges.Count do
      begin
        if FCurrentUser.user_object_privileges[I].host = Host then
          FCurrentUser.user_object_privileges.Delete(I)
        else
          Inc(I);
      end;

      if Assigned(Node.Parent) then
      begin
        UserTreeView.Selected := Node.Parent;
        UserTreeView.Items.Delete(Node);
      end
      else
      begin
        UserTreeView.Selected := nil;
        UserTreeView.Selected := Node;
      end;

      CurrentUserChanged;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AdvancedSchemaEditSearchEdChange(Sender: TObject);

begin
  SchemaPrivSchemataFrame.AdvancedEditSearchEdChange(Sender);

  RefreshSchemaPrivileges(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.CreateNewUser(Clone: TMYX_USER);

var
  I: Integer;
  UserPriv: TMYX_USER_OBJECT_PRIVILEGES;
  PUserPriv: PMYX_USER_OBJECT_PRIVILEGES;
  TreeNode: TTntTreeNode;
  NewUser: TMYX_USER;
  
begin
  if UserNameList = nil then
  begin
    ShowModalDialog(_('User list not fetched'),
      _('Before a new user account can be created the list of '+
        'current user accounts needs to be retreived. Please select '+
        'Refresh User List from the popup menu of the User Account list.'),
        myx_mtError, _('OK'));
    Exit;
  end;

  FNewUserPending := True;
  DiscardChanges(True);
  UserTreeView.Selected := nil;

  if Clone = nil then
  begin
    NewUser := TMYX_USER.create(_('New User'), '', '', '', '', '', 0, '');

    // Add % host
    NewUser.hosts.Add('%');

    // Add privileges for %
    LockConnection;
    try
      PUserPriv := myx_get_privilege_struct(MySQLConn.MySQL, '', MYX_UOP_GLOBAL);
      UserPriv := TMYX_USER_OBJECT_PRIVILEGES.create(PUserPriv);
      myx_free_user_priv(PUserPriv);
    finally
      ReleaseConnection;
    end;
    UserPriv.host := '%';
    UserPriv.object_name := '';

    //Add privileges to user
    NewUser.user_object_privileges.Add(UserPriv);
  end
  else
  begin
    NewUser := TMYX_USER.create(Clone.get_record_pointer);
    NewUser.user_name := _('Clone of ') + NewUser.user_name;
  end;

  // Add username to username list.
  UserNameList.user_names.Add(NewUser.user_name);

  // Rebuild user name tree.
  RefreshUserTree(AdvancedEdit.SearchEd.Text, False);
  FCurrentUser := NewUser;
  FCurrentHost := NewUser.hosts[0];

  // Attach user data to user node and select it.
  TreeNode := UserTreeView.Items.GetFirstNode;
  while TreeNode.Text <> FCurrentUser.user_name do
    TreeNode := TreeNode.GetNextSibling;

  if ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
  begin
    InitControls := True;
    try
        // Add the clone's hosts.
      if Assigned(TreeNode) and Assigned(Clone) then
      begin
        // The % host entry is represented by the user node itself (at index 0).
        // So there is no need to add another node for it.
        for I := 1 to FCurrentUser.hosts.Count - 1 do
          AddTreeViewChildNode(UserTreeView, TreeNode, FCurrentUser.hosts[I], 22, FCurrentUser);
      end;
    finally
      InitControls := False;
    end;
  end;

  // Now select new node.
  TreeNode.Data := FCurrentUser;
  TreeNode.Expand(False);
  FCurrentUser := nil;
  UserTreeView.Selected := TreeNode;

  UserNameEd.Text := '';
  PasswordEd.Text := '';
  PasswordConfirmationEd.Text := '';

  UserPageControl.ActivePage := UserInfoSheet;

  UserNameEd.SetFocus;
  CurrentUserChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshSchemaPrivileges(Schema: TMYX_SCHEMA);

var
  I: Integer;
  UserPriv: TMYX_USER_OBJECT_PRIVILEGES;
  PUserPriv: PMYX_USER_OBJECT_PRIVILEGES;
  Item: TListItem;
  PrivNode: PPrivilege;

begin
  ClearSchemaPrivListViews;

  if(Schema=nil)then
    Exit;

  //Find schema privileges
  UserPriv := nil;
  for I := 0 to FCurrentUser.user_object_privileges.Count-1 do
  begin
    //if object_name is 'xxxx', these are the schema privileges
    if(FCurrentUser.user_object_privileges[I].host=FCurrentHost)and
      (FCurrentUser.user_object_privileges[I].object_name=Schema.escaped_schema_name)then
    begin
      UserPriv := FCurrentUser.user_object_privileges[I];
      break;
    end;
  end;

  if UserPriv = nil then
  begin
    LockConnection;
    try
      PUserPriv := myx_get_privilege_struct(MySQLConn.MySQL, Schema.escaped_schema_name, MYX_UOP_SCHEMA);
      UserPriv := TMYX_USER_OBJECT_PRIVILEGES.create(PUserPriv);
      myx_free_user_priv(PUserPriv);
    finally
      ReleaseConnection;
    end;

    UserPriv.host := FCurrentHost;
    UserPriv.object_name := Schema.escaped_schema_name;

    FCurrentUser.user_object_privileges.Add(UserPriv);
  end;

  if(UserPriv<>nil)then
  begin
    for I := 0 to UserPriv.user_privileges.Count-1 do
    begin
      if(Copy(UserPriv.user_privileges.Names[I],
        Length(UserPriv.user_privileges.Names[I])-4, 5)=
          '_priv')then
      begin
        PrivNode := New(PPrivilege);
        PrivNode.id := UserPriv.user_privileges.Names[I];

        if(UserPriv.user_privileges.ValueFromIndex[I]='Y')then
          Item := AddListViewItem(SchemaPrivAssignedListView, nil,
            Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[I], '_priv', '')),
            15, PrivNode)
        else
          Item := AddListViewItem(SchemaPrivAvailListView, nil,
            Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[I], '_priv', '')),
            15, PrivNode);

        Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
          [Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[I], '_priv', ''))]));
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignSchemaPrivBtnClick(Sender: TObject);

begin
  if(GetCurrentUserSchema<>nil)then
  begin
    MovePriv(SchemaPrivAvailListView, SchemaPrivAssignedListView,
      GetCurrentUserSchema.escaped_schema_name, True);

    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveSchemaPrivBtnClick(Sender: TObject);

begin
  if(GetCurrentUserSchema<>nil)then
  begin
    MovePriv(SchemaPrivAssignedListView, SchemaPrivAvailListView,
      GetCurrentUserSchema.escaped_schema_name, False);

    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.SchemataFrameSchemaTreeViewExpanding(Sender: TObject; Node: TTntTreeNode;
  var AllowExpansion: Boolean);

begin
  AllowExpansion := not Assigned(Node.Data) or not ((TObject(Node.Data) is TMYX_SCHEMA));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ShowHostsActionExecute(Sender: TObject);

var
  Allow: Boolean;

begin
  Allow := not FClearingUsers and (MySQLConn.DataBeingFetched = []);
  if Allow and FDirty then
    Allow := DiscardChanges(True, False) <> -1;

  if not Allow then
    Exit;

  with ApplicationDM.OptionProvider do
    OptionAsBoolean['ShowUserHosts'] := not OptionAsBoolean['ShowUserHosts'];
  RefreshUserTree(AdvancedEdit.SearchEd.Text, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.CurrentUserChanged;

begin
  if not FClearingUsers and not FInitUserControls and Assigned(FCurrentUser) then
  begin
    FDirty := True;

    // If the current host is empty then we have selected the user node itself and there is no % entry. 
    FUserRenamed := (FCurrentHost <> '') and not WideSameText(FCurrentUser.user_name, UserNameEd.Text);
    ApplyChangesBtn.Enabled := True;
    DiscardChangesBtn.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserNameEdChange(Sender: TObject);

begin
  CurrentUserChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.LoadImgBtnClick(Sender: TObject);

var
  OpenDlg: TTntOpenDialog;
  MemStream: TMemoryStream;

begin
  if(FCurrentUser<>nil)then
  begin
    OpenDlg := TTntOpenDialog.Create(nil);
    try
      OpenDlg.InitialDir := '';
      OpenDlg.Filter := 'PNG Files|*.png';

      if(OpenDlg.Execute)then
      begin
        FCurrentUser.icon := LoadAnsiTextFromFile(OpenDlg.FileName);
        FCurrentUser.icon_length := Length(FCurrentUser.icon);

        MemStream := TMemoryStream.Create;
        try
          MemStream.WriteBuffer(FCurrentUser.icon[1], FCurrentUser.icon_length);

          MemStream.Position := 0;
          FUserIconPNGImage.LoadFromStream(MemStream);
          UserIcon.Picture.Graphic := FUserIconPNGImage;
          if(UserIcon.Picture.Width>48)or
            (UserIcon.Picture.Height>48)then
          begin
            FUserIconPNGImage :=
              LoadPNGImageFromResource('user_icon', UserIcon);

            ShowModalDialog(_('User icon too large'),
              _('Please note that the user icon must not be larger than 48x48 pixels'), myx_mtError, 'OK');

            FCurrentUser.icon_length := 0;
            FCurrentUser.icon := '';
          end;
          UserIcon.Invalidate;
        finally
          MemStream.Free;
        end;
      end;

      CurrentUserChanged;
    finally
      OpenDlg.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.LockConnection;

begin
  FConnectionLock.Acquire;

  // Ensure we have a working connection.
  MySQLConn.Ping;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearUserImgClick(Sender: TObject);

begin
  FUserIconPNGImage := LoadPNGImageFromResource('user_icon', UserIcon);
  FCurrentUser.icon_length := 0;

  CurrentUserChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.DeleteUser(UserName: WideString);

var
  I: Integer;
  Node: TTntTreeNode;

begin
  for I := 0 to UserTreeView.Items.Count-1 do
    if(UserTreeView.Items[I].Level=0)and
      (UserTreeView.Items[I].Text=UserName)then
    begin
      Node := UserTreeView.Items[I];

      if(Node.Data<>nil)then
        TMYX_USER(Node.Data).Free;

      UserTreeView.Items.Delete(Node);
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.DeleteUserActionExecute(Sender: TObject);

var
  UserName: WideString;

begin
  UserName := FCurrentUser.user_name;

  if SameText(UserName, 'root') then
    raise EMyxError.Create(_('You cannot delete the root user.'));

  if ShowModalDialog(_('Delete user?'),
    WideFormat(_('Are you sure you want to delete the user %s?'), [FCurrentUser.user_name]),
    myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1 then
  begin
    LockConnection;
    try
      myx_del_user(MySQLConn.MySQL, UserName);
    finally
      ReleaseConnection;
    end;

    if Assigned(UserNameList) then
      UserNameList.Free;
    UserNameList := nil;

    FDirty := False;
    ApplyChangesBtn.Enabled := False;
    DiscardChangesBtn.Enabled := False;

    RefreshUserTree(AdvancedEdit.SearchEd.Text, False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AddHostActionExecute(Sender: TObject);

var
  NewHost: WideString;
  user_priv: TMYX_USER_OBJECT_PRIVILEGES;
  puser_priv: PMYX_USER_OBJECT_PRIVILEGES;
  Node: TTntTreeNode;
  Child: TTntTreeNode;

begin
  if not ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
  begin
    if ShowModalDialog(_('Option change'), WideFormat(_('In order to view and edit host entries they must be switched' +
      ' on in the user management view by enabling the global option "%s".') + #13#10#13#10 + _('Do you want this?'),
      [_('Show hosts in user list')]), myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1 then
    begin
      ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] := True;
      RefreshUserTree(AdvancedEdit.SearchEd.Text, True);
    end;
  end;

  if ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
  begin
    if ShowModalEditDialog(_('Add Host'), _('Enter a new Host the User can connect from.'), myx_mtEdit, _('OK') +
      #13#10 + _('Abort'), True, _('Host:'), NewHost) = 1 then
    begin
      LockConnection;
      try
        puser_priv := myx_get_privilege_struct(MySQLConn.MySQL, '', MYX_UOP_GLOBAL);
        try
          user_priv := TMYX_USER_OBJECT_PRIVILEGES.create(puser_priv);
        finally
          myx_free_user_priv(puser_priv);
        end;
      finally
        ReleaseConnection;
      end;

      NewHost := Trim(NewHost);
      if NewHost = '' then
        ShowModalDialog(_('Invalid Host name'), _('Cannot add empty host.'), myx_mtError, _('OK'))
      else
      begin
        // Don't add another any-host (%) entry if there is one already.
        if FCurrentUser.hosts[0] = NewHost then
        begin
          ShowModalDialog(_('Host entry already exists'), _('The any-host (%) entry is already set. Select the user node ' +
            'to edit its details.'), myx_mtError, _('OK'));
        end
        else
        begin
          // Find top level node for the current user.
          Node := UserTreeView.Selected;
          while Assigned(Node.Parent) do
            Node := Node.Parent;

          // Set the any-host value in the reserved index zero slot if the use gave us %.
          if NewHost = '%' then
          begin
            FCurrentUser.hosts[0] := '%';
            ShowModalDialog(_('Host entry added'), _('The any-host (%) entry has been added and can be edited by ' +
              'selecting the user node.'), myx_mtInformation, _('OK'));
            Child := Node;
          end
          else
          begin
            // Any other given host goes here.

            // Check that no entry is listed twice.
            Child := Node.GetFirstChild;
            while Assigned(Child) and (Child.Text <> NewHost) do
              Child := Child.GetNextSibling;

            if Child = nil then
            begin
              user_priv.host := NewHost;
              FCurrentUser.hosts.Add(NewHost);
              FCurrentUser.user_object_privileges.Add(user_priv);

              Child := AddTreeViewChildNode(UserTreeView, Node, NewHost, 22, FCurrentUser);
              Node.Expand(False);
            end
            else
              ShowModalDialog(_('Host entry already exists'), WideFormat(_('The host entry "%s" is already set for ' +
                'user "%s".'), [NewHost, FCurrentUser.user_name]), myx_mtError, _('OK'));
          end;

          if Assigned(Child) then
          begin
            UserTreeView.Selected := nil;
            UserTreeView.Selected := Child;
            CurrentUserChanged;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserSubTreePopupMenuPopup(Sender: TObject);

var
  Node: TTntTreeNode;
  MPos: TPoint;

begin
  MPos := UserTreeView.ScreenToClient(Mouse.CursorPos);
  Node := UserTreeView.GetNodeAt(MPos.X, MPos.Y);
  if Assigned(Node) then
    UserTreeView.Selected := Node;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AdvancedEditSearchEdChange(Sender: TObject);

begin
  AdvancedEdit.SearchEdChange(self);

  DiscardChanges(True);
  RefreshUserTree(AdvancedEdit.SearchEd.Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  CanClose := DiscardChanges(True) <> -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.GetSchemaTables(Sender: TObject);

var
  SchemaTables: PMYX_SCHEMA_TABLES;

begin
  SchemaTables := myx_get_schema_tables(TFetchDataThread(Sender).Connection.MySQL,'',
    TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name);
  if(SchemaTables=nil)then
    raise EMyxSQLError.Create(_('Could not fetch Schema Tables.'),
      myx_mysql_errno(TFetchDataThread(Sender).Connection.MySQL),
      myx_mysql_error(TFetchDataThread(Sender).Connection.MySQL));

  TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_tables :=
    TMYX_SCHEMA_TABLES.create(SchemaTables);
  myx_free_schema_tables(SchemaTables);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.TblColSchemataFrameSchemaTreeViewExpanding(Sender: TObject; Node: TTntTreeNode;
  var AllowExpansion: Boolean);
  
begin
  if(Node.Data<>nil)then
    if(TObject(Node.Data) is TMYX_SCHEMA)then
      if(TMYX_SCHEMA(Node.Data).schema_tables=nil)then
      begin
        Node.DeleteChildren;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshTablePrivileges(Schema: TMYX_SCHEMA; SchemaTable: TMYX_SCHEMA_TABLE;
  SchemaTableColumn: TMYX_SCHEMA_TABLE_COLUMN);

var i: Integer;
  UserPriv: TMYX_USER_OBJECT_PRIVILEGES;
  PUserPriv: PMYX_USER_OBJECT_PRIVILEGES;
  ObjName: WideString;
  Item: TListItem;
  PrivNode: PPrivilege;
  PriviledgeType: MYX_USER_OBJECT_PRIVILEGE_TYPE;
  
begin
  ClearTableColumnPrivListViews;

  if(Schema=nil)then
    Exit;

  if (SchemaTable = nil) and (SchemaTableColumn = nil) then
  begin
    ObjName := Schema.schema_name;
    PriviledgeType := MYX_UOP_SCHEMA;
  end
  else
    if SchemaTableColumn = nil then
    begin
      ObjName := Schema.schema_name + '.' + SchemaTable.table_name;
      PriviledgeType := MYX_UOP_TABLE;
    end
    else
    begin
      ObjName := Schema.schema_name + '.' + SchemaTable.table_name + '.' + SchemaTableColumn.column_name;
      PriviledgeType := MYX_UOP_COLUMN;
    end;

  //Find schema privileges
  UserPriv := nil;
  for i := 0 to FCurrentUser.user_object_privileges.Count-1 do
  begin
    //if object_name is 'xxxx', these are the schema privileges
    if(FCurrentUser.user_object_privileges[i].host=FCurrentHost)and
      (FCurrentUser.user_object_privileges[i].object_name=ObjName)then
    begin
      UserPriv := FCurrentUser.user_object_privileges[i];
      break;
    end;
  end;

  if UserPriv = nil then
  begin
    LockConnection;
    try
      PUserPriv := myx_get_privilege_struct(MySQLConn.MySQL, ObjName, PriviledgeType);
      if Assigned(PUserPriv) then
      begin
        try
          UserPriv := TMYX_USER_OBJECT_PRIVILEGES.create(PUserPriv);
        finally
          myx_free_user_priv(PUserPriv);
        end;

        UserPriv.host := FCurrentHost;
        UserPriv.object_name := ObjName;

        FCurrentUser.user_object_privileges.Add(UserPriv);
      end;
    finally
      ReleaseConnection;
    end;
  end;

  if Assigned(UserPriv) then
  begin
    for i := 0 to UserPriv.user_privileges.Count-1 do
    begin
      PrivNode := New(PPrivilege);
      PrivNode.id := UserPriv.user_privileges.Names[i];

      if(UserPriv.user_privileges.ValueFromIndex[i]='Y')then
        Item := AddListViewItem(TablePrivAssignedListView, nil,
          Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', '')),
          15, PrivNode)
      else
        Item := AddListViewItem(TablePrivAvailListView, nil,
          Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', '')),
          15, PrivNode);

      Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
        [Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', ''))]));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshProcPrivileges(Schema: TMYX_SCHEMA; SchemaProc: TMYX_SCHEMA_STORED_PROCEDURE);

var i: Integer;
  UserPriv: TMYX_USER_OBJECT_PRIVILEGES;
  PUserPriv: PMYX_USER_OBJECT_PRIVILEGES;
  ObjName: WideString;
  Item: TListItem;
  PrivNode: PPrivilege;
  PriviledgeType: MYX_USER_OBJECT_PRIVILEGE_TYPE;

begin
  ClearTableColumnPrivListViews;

  if(Schema=nil)then
    Exit;

  if SchemaProc = nil then
  begin
    ObjName := Schema.schema_name;
    PriviledgeType := MYX_UOP_SCHEMA;
  end
  else
  begin
    ObjName := Schema.schema_name + '.' + SchemaProc.name;
    PriviledgeType := MYX_UOP_ROUTINE;
  end;

  //Find schema privileges
  UserPriv := nil;
  for i := 0 to FCurrentUser.user_object_privileges.Count-1 do
  begin
    //if object_name is 'xxxx', these are the schema privileges
    if(FCurrentUser.user_object_privileges[i].host=FCurrentHost)and
      (FCurrentUser.user_object_privileges[i].object_name=ObjName)then
    begin
      UserPriv := FCurrentUser.user_object_privileges[i];
      break;
    end;
  end;

  if UserPriv = nil then
  begin
    LockConnection;
    try
      PUserPriv := myx_get_privilege_struct(MySQLConn.MySQL, ObjName, PriviledgeType);
      try
        UserPriv := TMYX_USER_OBJECT_PRIVILEGES.create(PUserPriv);
      finally
        myx_free_user_priv(PUserPriv);
      end;
    finally
      ReleaseConnection;
    end;

    UserPriv.host := FCurrentHost;
    UserPriv.object_name := ObjName;

    FCurrentUser.user_object_privileges.Add(UserPriv);
  end;

  if(UserPriv<>nil)then
  begin
    for i := 0 to UserPriv.user_privileges.Count-1 do
    begin
      PrivNode := New(PPrivilege);
      PrivNode.id := UserPriv.user_privileges.Names[i];

      if(UserPriv.user_privileges.ValueFromIndex[i]='Y')then
        Item := AddListViewItem(TablePrivAssignedListView, nil,
          Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', '')),
          15, PrivNode)
      else
        Item := AddListViewItem(TablePrivAvailListView, nil,
          Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', '')),
          15, PrivNode);

      Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
        [Uppercase(AnsiReplaceStr(UserPriv.user_privileges.Names[i], '_priv', ''))]));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignTblColPrivBtnClick(Sender: TObject);

begin
  if(GetCurrentUserTblColSchema<>nil)then
  begin
    if(GetCurrentUserColumn=nil)and(GetCurrentUserTable=nil)and(GetCurrentUserProc=nil)then
      MovePriv(TablePrivAvailListView, TablePrivAssignedListView,
        GetCurrentUserTblColSchema.schema_name, True)
    else if(GetCurrentUserColumn=nil)then
      if(GetCurrentUserProc <> nil) then
      begin
        MovePriv(TablePrivAvailListView, TablePrivAssignedListView,
          GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserProc.name, True)
      end
      else
      begin
        MovePriv(TablePrivAvailListView, TablePrivAssignedListView,
          GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name, True)
      end
    else
      MovePriv(TablePrivAvailListView, TablePrivAssignedListView,
        GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name + '.' +
        GetCurrentUserColumn.column_name, True);

    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveTblColPrivBtnClick(Sender: TObject);

begin
  if(GetCurrentUserTblColSchema<>nil)then
  begin
    if(GetCurrentUserColumn=nil)and(GetCurrentUserTable=nil)then
      MovePriv(TablePrivAssignedListView, TablePrivAvailListView,
        GetCurrentUserTblColSchema.schema_name, False)
    else if(GetCurrentUserColumn=nil)then
      MovePriv(TablePrivAssignedListView, TablePrivAvailListView,
        GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name, False)
    else
      MovePriv(TablePrivAssignedListView, TablePrivAvailListView,
        GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name + '.' +
        GetCurrentUserColumn.column_name, False);

    CurrentUserChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearGlobalPrivListViews;

var
  I: Integer;

begin
  for I := 0 to GlobalAssignedPrivListView.Items.Count-1 do
    if(GlobalAssignedPrivListView.Items[I].Data<>nil)then
      Dispose(GlobalAssignedPrivListView.Items[I].Data);
  GlobalAssignedPrivListView.Clear;

  for I := 0 to GlobalPrivListView.Items.Count-1 do
    if(GlobalPrivListView.Items[I].Data<>nil)then
      Dispose(GlobalPrivListView.Items[I].Data);
  GlobalPrivListView.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearSchemaPrivListViews;

var
  I: Integer;

begin
  for I := 0 to SchemaPrivAssignedListView.Items.Count-1 do
    if(SchemaPrivAssignedListView.Items[I].Data<>nil)then
      Dispose(SchemaPrivAssignedListView.Items[I].Data);
  SchemaPrivAssignedListView.Clear;

  for I := 0 to SchemaPrivAssignedListView.Items.Count-1 do
    if(SchemaPrivAssignedListView.Items[I].Data<>nil)then
      Dispose(SchemaPrivAssignedListView.Items[I].Data);
  SchemaPrivAvailListView.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ClearTableColumnPrivListViews;

var
  I: Integer;

begin
  for I := 0 to TablePrivAvailListView.Items.Count-1 do
    if(TablePrivAvailListView.Items[I].Data<>nil)then
      Dispose(TablePrivAvailListView.Items[I].Data);
  TablePrivAvailListView.Clear;

  for I := 0 to TablePrivAssignedListView.Items.Count-1 do
    if(TablePrivAssignedListView.Items[I].Data<>nil)then
      Dispose(TablePrivAssignedListView.Items[I].Data);
  TablePrivAssignedListView.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ApplyOptionSettings;

begin
  GlobalPrivSheet.TabVisible := ApplicationDM.Options.ShowUserGlobalPrivileges;
  TablePrivSheet.TabVisible := ApplicationDM.Options.ShowUserTableColumnPrivileges;

  SetDataFont([UserInfoNameLbl, GlobalPrivNameLbl, SchemaPrivNameLbl, TableColumnPrivNameLbl, ResourceNameLbl,
    UserNameEd, PasswordEd, PasswordConfirmationEd, FullNameEd, DescriptionEd, EmailEd, ContactInfoMemo,
    UserTreeView, AdvancedEdit.SearchEd]);
    
  RefreshUserTree(AdvancedEdit.SearchEd.Text, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.OptionsChanged(var Message: TMessage);

begin
  ApplyOptionSettings;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignAllGlobalPrivBtnClick(Sender: TObject);

begin
  GlobalPrivListView.SelectAll;

  AssignGlobalPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveAllGlobalPrivBtnClick(Sender: TObject);

begin
  GlobalAssignedPrivListView.SelectAll;

  RemoveGlobalPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignAllSchemaPrivBtnClick(Sender: TObject);

begin
  SchemaPrivAvailListView.SelectAll;

  AssignSchemaPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveAllSchemaPrivBtnClick(Sender: TObject);

begin
  SchemaPrivAssignedListView.SelectAll;

  RemoveSchemaPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AssignAllTblColPrivBtnClick(Sender: TObject);

begin
  TablePrivAvailListView.SelectAll;

  AssignTblColPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RemoveAllTblColPrivBtnClick(Sender: TObject);

begin
  TablePrivAssignedListView.SelectAll;

  RemoveTblColPrivBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ConnectionLost(var Message: TMessage);

begin
  DiscardChanges(False, False);

  ClearUsers;
  DisableEnableUserPageControl(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.ConnectionEstablished(var Message: TMessage);

begin
  RefreshUserTree('', False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.SchemaListChanged(var Message: TMessage);

begin
  SchemaPrivSchemataFrame.ReloadSchemaTree;

  TblColSchemataFrame.ReloadSchemaTree;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.GetCurrentUserSchema: TMYX_SCHEMA;

var
  NodeData: ^TObject;

begin
  Result := nil;

  NodeData := SchemaPrivSchemataFrame.CatalogVST.GetNodeData(
    SchemaPrivSchemataFrame.CatalogVST.FocusedNode);
  if(NodeData<>nil)then
    if(NodeData^ is TMYX_SCHEMA)then
      Result := TMYX_SCHEMA(NodeData^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.SchemaPrivSchemataFrameCatalogVSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: ^TObject;
  
begin
  SchemaPrivSchemataFrame.CatalogVSTChange(Sender, Node);

  if Assigned(Node) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if NodeData^ is TMYX_SCHEMA then
      RefreshSchemaPrivileges(TMYX_SCHEMA(NodeData^));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.SchemaPrivSchemataFrameRefreshCatalogsSchemataListMIClick(Sender: TObject);

begin
  //SchemaPrivSchemataFrame.RefreshCatalogsSchemataListMIClick(Sender);
  RefreshUserTree('', True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.GetCurrentUserTblColSchema: TMYX_SCHEMA;

var
  NodeData,
  ParentNodeData,
  ParentParentNodeData: ^TObject;
  
begin
  Result := nil;

  NodeData := TblColSchemataFrame.CatalogVST.GetNodeData(TblColSchemataFrame.CatalogVST.FocusedNode);

  if(NodeData<>nil)then
    if(NodeData^<>nil)then
      if(NodeData^ is TMYX_SCHEMA)then
      begin
        Result := TMYX_SCHEMA(NodeData^)
      end
      else if((NodeData^ is TMYX_SCHEMA_TABLE) or (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE))and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
          TblColSchemataFrame.CatalogVST.NodeParent[
            TblColSchemataFrame.CatalogVST.FocusedNode]);

        if(ParentNodeData<>nil)then
          if(ParentNodeData^<>nil)then
            Result := TMYX_SCHEMA(ParentNodeData^);
      end
      else if(NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
          TblColSchemataFrame.CatalogVST.NodeParent[
            TblColSchemataFrame.CatalogVST.FocusedNode]);

        if(ParentNodeData<>nil)and
          (TblColSchemataFrame.CatalogVST.NodeParent[
            TblColSchemataFrame.CatalogVST.NodeParent[
              TblColSchemataFrame.CatalogVST.FocusedNode]]<>nil)then
        begin
          ParentParentNodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
            TblColSchemataFrame.CatalogVST.NodeParent[
              TblColSchemataFrame.CatalogVST.NodeParent[
                TblColSchemataFrame.CatalogVST.FocusedNode]]);

          if(ParentNodeData^<>nil)and
            (ParentParentNodeData<>nil)then
            if(ParentParentNodeData^<>nil)then
              Result := TMYX_SCHEMA(ParentParentNodeData^);
        end;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.GetCurrentUserTable: TMYX_SCHEMA_TABLE;

var
  NodeData,
  ParentNodeData: ^TObject;

begin
  Result := nil;

  NodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
    TblColSchemataFrame.CatalogVST.FocusedNode);

  if(NodeData<>nil)then
    if(NodeData^<>nil)then
      if(NodeData^ is TMYX_SCHEMA_TABLE)and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        Result := TMYX_SCHEMA_TABLE(NodeData^);
      end
      else if(NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
          TblColSchemataFrame.CatalogVST.NodeParent[
            TblColSchemataFrame.CatalogVST.FocusedNode]);

        if(ParentNodeData<>nil)then
          if(ParentNodeData^<>nil)then
            Result := TMYX_SCHEMA_TABLE(ParentNodeData^);
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.GetCurrentUserProc: TMYX_SCHEMA_STORED_PROCEDURE;

var
  NodeData: ^TObject;

begin
  Result := nil;

  NodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
    TblColSchemataFrame.CatalogVST.FocusedNode);

  if(NodeData<>nil)then
    if(NodeData^<>nil)then
      if(NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE)and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        Result := TMYX_SCHEMA_STORED_PROCEDURE(NodeData^);
      end
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminUsersForm.GetCurrentUserColumn: TMYX_SCHEMA_TABLE_COLUMN;

var
  NodeData: ^TObject;

begin
  Result := nil;

  NodeData := TblColSchemataFrame.CatalogVST.GetNodeData(
    TblColSchemataFrame.CatalogVST.FocusedNode);

  if(NodeData<>nil)then
    if(NodeData^<>nil)then
      if(NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)and
        (TblColSchemataFrame.CatalogVST.NodeParent[
          TblColSchemataFrame.CatalogVST.FocusedNode]<>nil)and
        (FCurrentUser<>nil)then
      begin
        Result := TMYX_SCHEMA_TABLE_COLUMN(NodeData^);
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.TblColSchemataFrameCatalogVSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData,
  ParentNodeData,
  ParentParentNodeData: ^TObject;
  CurrentUserTblColSchema: TMYX_SCHEMA;
  CurrentUserTable: TMYX_SCHEMA_TABLE;
  CurrentUserProc: TMYX_SCHEMA_STORED_PROCEDURE;
  CurrentUserColumn: TMYX_SCHEMA_TABLE_COLUMN;
  
begin
  TblColSchemataFrame.CatalogVSTChange(Sender, Node);

  NodeData := Sender.GetNodeData(Node);

  if(NodeData<>nil)then
    if(NodeData^<>nil)then
    begin
      if(NodeData^ is TMYX_SCHEMA)then
      begin
        CurrentUserTblColSchema := TMYX_SCHEMA(NodeData^);

        CurrentUserTblColSchema.schema_name := 
          Tnt_WideStringReplace(CurrentUserTblColSchema.schema_name,
          '_', '\_', [rfReplaceAll]);
        CurrentUserTblColSchema.schema_name := 
          Tnt_WideStringReplace(CurrentUserTblColSchema.schema_name,
          '%', '\%', [rfReplaceAll]);
        RefreshTablePrivileges(CurrentUserTblColSchema, nil, nil);
        CurrentUserTblColSchema.schema_name := 
          Tnt_WideStringReplace(CurrentUserTblColSchema.schema_name,
          '\_', '_', [rfReplaceAll]);
        CurrentUserTblColSchema.schema_name := 
          Tnt_WideStringReplace(CurrentUserTblColSchema.schema_name,
          '\%', '%', [rfReplaceAll]);
      end;

      if(NodeData^ is TMYX_SCHEMA_TABLE)and
        (Sender.NodeParent[Node]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := Sender.GetNodeData(Sender.NodeParent[Node]);

        if(ParentNodeData<>nil)then
          if(ParentNodeData^<>nil)then
          begin
            CurrentUserTblColSchema := TMYX_SCHEMA(ParentNodeData^);
            CurrentUserTable := TMYX_SCHEMA_TABLE(NodeData^);

            RefreshTablePrivileges(CurrentUserTblColSchema,
              CurrentUserTable, nil);
          end;
      end;

      if(NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE)and
        (Sender.NodeParent[Node]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := Sender.GetNodeData(Sender.NodeParent[Node]);

        if(ParentNodeData<>nil)then
          if(ParentNodeData^<>nil)then
          begin
            CurrentUserTblColSchema := TMYX_SCHEMA(ParentNodeData^);
            CurrentUserProc := TMYX_SCHEMA_STORED_PROCEDURE(NodeData^);

            RefreshProcPrivileges(CurrentUserTblColSchema,
              CurrentUserProc);
          end;
      end;

      if(NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)and
        (Sender.NodeParent[Node]<>nil)and
        (FCurrentUser<>nil)then
      begin
        ParentNodeData := Sender.GetNodeData(Sender.NodeParent[Node]);

        if(ParentNodeData<>nil)and
          (Sender.NodeParent[Sender.NodeParent[Node]]<>nil)then
        begin
          ParentParentNodeData := Sender.GetNodeData(Sender.NodeParent[Sender.NodeParent[Node]]);

          if(ParentNodeData^<>nil)and
            (ParentParentNodeData<>nil)then
          begin
            if(ParentParentNodeData^<>nil)then
            begin
              CurrentUserTblColSchema := TMYX_SCHEMA(ParentParentNodeData^);
              CurrentUserTable := TMYX_SCHEMA_TABLE(ParentNodeData^);
              CurrentUserColumn := TMYX_SCHEMA_TABLE_COLUMN(NodeData^);

              RefreshTablePrivileges(CurrentUserTblColSchema,
                CurrentUserTable, CurrentUserColumn);
            end;
          end;
        end;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.TblColSchemataFrameRefreshCatalogsSchemataListMIClick(Sender: TObject);

begin
  RefreshUserTree('', True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AddSchemawithWidcardsMIClick(Sender: TObject);

var
  catalog_name: WideString;
  schema_name: WideString;
  res: Integer;
  
begin
  catalog_name := 'def';
  schema_name := '';

  res := ShowModalEditDialog(_('Add schema'), _('Please enter the name of a new schema including wildcards like % and ' +
    '_, e.g. test%') + #13#10#13#10, myx_mtEdit, _('Store') + #13#10 + _('Cancel'), True, _('Schema name:'), schema_name,
    1, False);

  if (res = 1) and (schema_name <> '') then
  begin
    SchemaPrivSchemataFrame.SchemaPatterns.Add(catalog_name + '/' + schema_name);
    SchemaPrivSchemataFrame.FillSchemaTree(SchemaPrivSchemataFrame.AdvancedEdit.SearchEd.Text);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.AddUserActionExecute(Sender: TObject);

var
  AllowChange: Boolean;
  
begin
  AllowChange := not FClearingUsers and (MySQLConn.DataBeingFetched = []);
  if AllowChange then
  begin
    // Ask if changes should be applied before the new user can be created.
    if FDirty then
      AllowChange := DiscardChanges(True, False) <> -1;
  end;
  if AllowChange then
    CreateNewUser;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshActionExecute(Sender: TObject);

begin
  ClearUsers;
  FreeAndNil(UserNameList);
  RefreshUserTree(AdvancedEdit.SearchEd.Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.RefreshCatalogTreeMIClick(Sender: TObject);

begin
  RefreshUserTree('', True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.UserTreeViewDeletion(Sender: TObject; Node: TTreeNode);

begin
  // TODO: Improve user data handling.
  // Due to the way user data objects are handled here it is not simply possible to delete them.
  // Without the Free class, though, memory leaks remain. This must be further investigated but is too involved for the moment.
  // TMYX_USER(Node.Data).Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TAdminUsersForm.UpdateUserDisplay;

var
  i: Integer;
  MemStream: TMemoryStream;
  Title: WideString;
  ypos: Integer;
  j: Integer;
  PrivNode: PPrivilege;
  Item: TListItem;
  aEdit: TTntEdit;
  aLabel: TTntLabel;
  R: TRect;

begin
  if Assigned(FCurrentUser) and (FCurrentHost <> '') then
  begin
    DisableEnableUserPageControl(True);

    UserNameEd.Text := FCurrentUser.user_name;
    PasswordEd.Text := '________';
    PasswordConfirmationEd.Text := '________';
    FullNameEd.Text := FCurrentUser.full_name;
    DescriptionEd.Text := FCurrentUser.description;
    EmailEd.Text := FCurrentUser.email;
    ContactInfoMemo.Text := FCurrentUser.contact_information;

    // Get icon.
    if FCurrentUser.icon_length = 0 then
    begin
      if Assigned(FUserIconPNGImage) then
        FUserIconPNGImage.Free;
      FUserIconPNGImage := LoadPNGImageFromResource('user_icon', UserIcon);
    end
    else
    begin
      MemStream := TMemoryStream.Create;
      try
        MemStream.WriteBuffer(PChar(FCurrentUser.icon)^, FCurrentUser.icon_length);
        MemStream.Position := 0;
        if FUserIconPNGImage = nil then
          FUserIconPNGImage := TPNGObject.Create;
        try
          FUserIconPNGImage.LoadFromStream(MemStream);
          UserIcon.Picture.Graphic := FUserIconPNGImage;
        except
          ShowModalDialog(_('Icon image corrupted.'), _('The User icon image file is corrupted.'), myx_mtError, 'Ok');
        end;
        UserIcon.Invalidate;
      finally
        MemStream.Free;
      end;
    end;

    // Set page header texts.
    if FCurrentHost = '%' then
    begin
      Title := FCurrentUser.user_name;
      if ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] then
        Title := Title + '@' + _('any host');
    end
    else
      Title := FCurrentUser.user_name + '@' + FCurrentHost;
    if (FCurrentUser.full_name <> '') then
      Title := Title + ' (' + FCurrentUser.full_name + ')';
    if (FCurrentUser.user_name = '') then
      Title := _('anonymous') + Title;
    UserInfoNameLbl.Caption := Title;
    GlobalPrivNameLbl.Caption := Title;
    SchemaPrivNameLbl.Caption := Title;
    TableColumnPrivNameLbl.Caption := Title;
    ResourceNameLbl.Caption := Title;

    //Start at y26
    ypos := 26;
    ClearGlobalPrivListViews;
    ClearSchemaPrivListViews;
    ClearTableColumnPrivListViews;

    //Get User Privileges
    for i := 0 to FCurrentUser.user_object_privileges.Count - 1 do
    begin
      //if object_name is '', these are the global privileges
      if (FCurrentUser.user_object_privileges[i].host = FCurrentHost) and (FCurrentUser.user_object_privileges[i].object_name = '') then
      begin
        for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
        begin
          //Separate _priv from max_
          if (Copy(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], Length(FCurrentUser.user_object_privileges[i].user_privileges.Names[j]) - 4, 5) = '_priv') then
          begin
            PrivNode := New(PPrivilege);
            PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
            if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
              Item := AddListViewItem(GlobalAssignedPrivListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
            else
              Item := AddListViewItem(GlobalPrivListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
            Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
              [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
          end
          else
          begin
            //Update ResourceSheet
            if (not (ResourceSheet.TabVisible)) then
              ResourceSheet.TabVisible := True;
            aEdit := TTntEdit(FindComponent(FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + 'Ed'));
            if aEdit = nil then
            begin
              aLabel := TTntLabel.Create(self);
              aLabel.Parent := UserResourceLimitGBox;
              aLabel.Name := FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + 'TitleLbl';
              aLabel.Left := 14;
              aLabel.Top := ypos;
              aLabel.Caption := FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + ':';
              aLabel.AutoSize := True;
              aEdit := TTntEdit.Create(self);
              aEdit.Parent := UserResourceLimitGBox;
              aEdit.Name := FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + 'Ed';
              aEdit.Left := 160;
              aEdit.Top := ypos - 2;
              aEdit.Width := 61;
              aEdit.Height := aLabel.Height + 4;
              aEdit.Text := FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j];
              aEdit.OnChange := OnResourceEditChange;
              aLabel := TTntLabel.Create(self);
              aLabel.Parent := UserResourceLimitGBox;
              aLabel.Name := FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + 'InfoLbl';
              aLabel.Left := 236;
              aLabel.Top := ypos - 5;
              aLabel.Width := 285;
              aLabel.AutoSize := False;
              aLabel.WordWrap := True;
              if (FCurrentUser.user_object_privileges[i].user_privileges.Names[j] = 'max_connections') then
                Title := _('Number of connections to the server the user can open within one hour.')
              else if (FCurrentUser.user_object_privileges[i].user_privileges.Names[j] = 'max_questions') then
                Title := _('Number of queries the user can execute within one hour.')
              else if (FCurrentUser.user_object_privileges[i].user_privileges.Names[j] = 'max_updates') then
                Title := _('Number of update the user can execute within one hour.')
              else if (FCurrentUser.user_object_privileges[i].user_privileges.Names[j] = 'max_user_connections') then
                Title := _('Number of simultaneous connections to the server from this account.');
              // Compute label size to correctly increase vertical offset.
              R := Rect(aLabel.Left, aLabel.Top, aLabel.Left + aLabel.Width, 0);
              Windows.DrawTextW(Canvas.Handle, PWideChar(Title), Length(Title), R, DT_CALCRECT or DT_WORDBREAK);
              aLabel.Height := R.Bottom - R.Top;
              aLabel.Caption := Title;
            end
            else
            begin
              aEdit.Text := FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j];
              aLabel := FindComponent(FCurrentUser.user_object_privileges[i].user_privileges.Names[j] + 'InfoLbl') as TTntLabel;
            end;
            Inc(ypos, Max(aLabel.Height, aEdit.Height) + 8);
          end;
        end;
      end
      else
      begin
        //Check if there are any schemas with wildcards
        if (FCurrentUser.user_object_privileges[i].host = FCurrentHost) and (Pos('.', FCurrentUser.user_object_privileges[i].object_name) = 0) then
        begin
          //Add the schema to the catalog if it is not found
          //For version 4 databases, add schemas to catalog def
          if (CompareText(FCurrentUser.user_object_privileges[i].object_name, 'mysql') <> 0) then
            if (SchemaPrivSchemataFrame.SchemaPatterns.IndexOf('def/' + FCurrentUser.user_object_privileges[i].object_name) = -1) then
            begin
              SchemaPrivSchemataFrame.SchemaPatterns.Add('def/' + FCurrentUser.user_object_privileges[i].object_name);
            end;
        end;
        //Set schema privileges
        if (FCurrentUser.user_object_privileges[i].host = FCurrentHost) and (GetCurrentUserSchema <> nil) then
        begin
          if (FCurrentUser.user_object_privileges[i].host = FCurrentHost) and (FCurrentUser.user_object_privileges[i].object_name = GetCurrentUserSchema.schema_name) then
          begin
            for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
            begin
              PrivNode := New(PPrivilege);
              PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
              if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
                Item := AddListViewItem(SchemaPrivAssignedListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
              else
                Item := AddListViewItem(SchemaPrivAvailListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
              Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
                [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
            end;
          end;
        end;
        //Set table/column privileges
        if (FCurrentUser.user_object_privileges[i].host = FCurrentHost) and (GetCurrentUserTblColSchema <> nil) then
        begin
          if (GetCurrentUserTable = nil) and (GetCurrentUserColumn = nil) then
          begin
            if (GetCurrentUserProc <> nil) then
            begin
              if (FCurrentUser.user_object_privileges[i].object_name = GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserProc.name) then
              begin
                for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
                begin
                  PrivNode := New(PPrivilege);
                  PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
                  if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
                    Item := AddListViewItem(TablePrivAssignedListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
                  else
                    Item := AddListViewItem(TablePrivAvailListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
                  Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
                    [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
                end;
              end;
            end
            else
            begin
              if (FCurrentUser.user_object_privileges[i].object_name = GetCurrentUserTblColSchema.schema_name) then
              begin
                for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
                begin
                  PrivNode := New(PPrivilege);
                  PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
                  if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
                    Item := AddListViewItem(TablePrivAssignedListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
                  else
                    Item := AddListViewItem(TablePrivAvailListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
                  Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
                    [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
                end;
              end;
            end;
          end
          else if (GetCurrentUserTable <> nil) and (GetCurrentUserColumn = nil) then
          begin
            if (FCurrentUser.user_object_privileges[i].object_name = GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name) then
            begin
              for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
              begin
                PrivNode := New(PPrivilege);
                PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
                if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
                  Item := AddListViewItem(TablePrivAssignedListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
                else
                  Item := AddListViewItem(TablePrivAvailListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
                Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
                  [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
              end;
            end;
          end
          else //CurrentUserColumn not nil
          if (GetCurrentUserTable <> nil) and (GetCurrentUserColumn <> nil) then
          begin
            if (FCurrentUser.user_object_privileges[i].object_name = GetCurrentUserTblColSchema.schema_name + '.' + GetCurrentUserTable.table_name + '.' + GetCurrentUserColumn.column_name) then
            begin
              for j := 0 to FCurrentUser.user_object_privileges[i].user_privileges.Count - 1 do
              begin
                PrivNode := New(PPrivilege);
                PrivNode.id := FCurrentUser.user_object_privileges[i].user_privileges.Names[j];
                if (FCurrentUser.user_object_privileges[i].user_privileges.ValueFromIndex[j] = 'Y') then
                  Item := AddListViewItem(TablePrivAssignedListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode)
                else
                  Item := AddListViewItem(TablePrivAvailListView, nil, Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', '')), 15, PrivNode);
                Item.SubItems.Add(WideFormat(_('Grants the %s privilege to the user'),
                  [Uppercase(AnsiReplaceStr(FCurrentUser.user_object_privileges[i].user_privileges.Names[j], '_priv', ''))]));
              end;
            end;
          end;
        end;
      end;
    end;
    UserResourceLimitGBox.Height := ypos;
    SchemaPrivSchemataFrame.RefreshCatalogList(SchemaPrivSchemataFrame.AdvancedEdit.SearchEd.Text);
  end
  else
    if not FClearingUsers then
    begin
      ClearUserControls;
      DisableEnableUserPageControl(False);
    end;
      
  FDirty := False;
  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminUsersForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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
