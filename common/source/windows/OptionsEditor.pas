unit OptionsEditor;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, TntForms,
  Dialogs, TntStdCtrls, TntExtCtrls, TntComCtrls, ComCtrls, ImgList, AuxFuncs,
  myx_util_public_interface, myx_public_interface, Grids, ValEdit,
  Options, PNGImage, MyxError, Forms, StdCtrls, ExtCtrls, TntClasses;

type
  TOptionsForm = class(TTntForm)
    HeaderPnl: TTntPanel;
    HeaderImg: TTntImage;
    TitleLbl: TTntLabel;
    OptionTreeImageList: TImageList;
    OptionPageControl: TTntPageControl;
    GeneralTabSheet: TTabSheet;
    ConnectionsTabSheet: TTabSheet;
    GroupBox1: TTntGroupBox;
    ShowTipOfDayCBox: TTntCheckbox;
    StoreWindowsPosCBox: TTntCheckbox;
    GroupBox2: TTntGroupBox;
    CoverPnl: TTntPanel;
    CloseBtn: TTntButton;
    Label2: TTntLabel;
    ApplicationFontBtn: TTntButton;
    ConnsTreeView: TTntTreeView;
    ConnectionPageControl: TTntPageControl;
    ConnGeneralTabSheet: TTabSheet;
    ConnAdvTabSheet: TTabSheet;
    ConnectionLbl: TTntLabel;
    UsernameLbl: TTntLabel;
    PasswordLbl: TTntLabel;
    PortLbl: TTntLabel;
    HostnameLbl: TTntLabel;
    TypeLbl: TTntLabel;
    SchemataEdLbl: TTntLabel;
    PasswordEd: TTntEdit;
    PortEd: TTntEdit;
    TypeCBox: TTntComboBox;
    SchemataEd: TTntEdit;
    UsernameEd: TTntEdit;
    ConnectionEd: TTntEdit;
    HostnameEd: TTntEdit;
    Label1: TTntLabel;
    ConnMemo: TTntMemo;
    ApplyChangesBtn: TTntButton;
    DiscardChangesBtn: TTntButton;
    ConnValueListEditor: TValueListEditor;
    Label5: TTntLabel;
    AddConnectionButton: TTntButton;
    DeleteConnectionButton: TTntButton;
    SidebarScrollBox: TTntScrollbox;
    Panel1: TTntPanel;
    EditorsTabSheet: TTabSheet;
    GroupBox4: TTntGroupBox;
    EdTblShowSQLBeforeApplyingCBox: TTntCheckbox;
    EdTblAllColsNotNullCBox: TTntCheckbox;
    Label6: TTntLabel;
    EdTblPKAutoNamingEd: TTntComboBox;
    EdTblAllIntUnsignedCBox: TTntCheckbox;
    Label7: TTntLabel;
    EdTblIndexNamingEd: TTntComboBox;
    EdTblFKNamingEd: TTntComboBox;
    Label8: TTntLabel;
    Label9: TTntLabel;
    EdTblPKDatatypeEd: TTntComboBox;
    Label10: TTntLabel;
    DefaultDatatypeComboBox: TTntComboBox;
    HeaderBGShape: TTntShape;
    TntLabel1: TTntLabel;
    DataFontBtn: TTntButton;
    TntLabel2: TTntLabel;
    CodeFontBtn: TTntButton;
    Label3: TTntLabel;
    LanguageCBox: TTntComboBox;
    TntLabel3: TTntLabel;
    TntLabel4: TTntLabel;
    TntLabel5: TTntLabel;
    TntGroupBox1: TTntGroupBox;
    StorePasswordCBox: TTntCheckBox;
    PasswordStorageTypeLbl: TTntLabel;
    PasswordStorageTypeCBox: TTntComboBox;
    TntGroupBox2: TTntGroupBox;
    IgnoreWarningsList: TTntListBox;
    TntLabel6: TTntLabel;
    RemoveWarningFromIgnoreListBtn: TTntButton;
    TntLabel7: TTntLabel;
    DefaultFontLU: TTntComboBox;
    DefaultFontSizeLU: TTntComboBox;
    DataFontSizeLU: TTntComboBox;
    DataFontLU: TTntComboBox;
    CodeFontLU: TTntComboBox;
    CodeFontSizeLU: TTntComboBox;
    CodeFontWidthLU: TTntComboBox;
    DisableTransparencyEffectsCBox: TTntCheckBox;
    TntLabel8: TTntLabel;
    TntLabel9: TTntLabel;
    TntLabel10: TTntLabel;
    TntLabel11: TTntLabel;
    DefaultStorageEngineLU: TTntComboBox;
    GroupBox3: TGroupBox;
    KeepRoutineEditorOnTopCBox: TCheckBox;
    CloneConnectionButton: TTntButton;
    ShowSpecialCharsCheckBox: TCheckBox;

    constructor Create(AOwner: TComponent; ApplOptionForm: TApplicationOptionsForm; MySQL: Pointer); reintroduce;
    destructor Destroy; override;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ShowOptionPage(PageNr: Integer);
    procedure OptionImgClick(Sender: TObject);
    procedure AddOptionPage(IconName: WideString; TopPos: Integer;
      PageNr: Integer; PageTitle: WideString);
    procedure BuildOptionSideBar;

    procedure CloseBtnClick(Sender: TObject);
    procedure OptionTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure OptionPageControlChange(Sender: TObject);

    procedure LoadPageData(PageNr: Integer);
    procedure DoPageContentChanged(Sender: TObject);
    procedure ApplyChanges;
    function DiscardChanges(ConfirmDiscard: Boolean = True): Boolean; virtual;

    procedure OptionTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DiscardChangesBtnClick(Sender: TObject);
    procedure ConnsTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean); virtual;
    procedure DeleteConnectionButtonClick(Sender: TObject); virtual;

    procedure AddConnection(UserConn: TMYX_USER_CONNECTION = nil); virtual;
    procedure AddConnectionButtonClick(Sender: TObject);

    procedure StorePasswordCBoxClick(Sender: TObject);

    procedure DisableEnableConnectionPages(Enable: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConnsTreeViewChange(Sender: TObject; Node: TTreeNode); virtual;
    procedure ApplicationFontBtnClick(Sender: TObject);
    procedure DataFontBtnClick(Sender: TObject);
    procedure CodeFontBtnClick(Sender: TObject);
    procedure RemoveWarningFromIgnoreListBtnClick(Sender: TObject);
    procedure ConnsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ConnsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CloneConnectionButtonClick(Sender: TObject);
  private
    FStoredConnections: TMYX_USER_CONNECTIONS;
    FDirty: Boolean;
    FInitControls: Boolean;
    FDiscardingChanges: Boolean;
    FOptionImages: TList;
    FApplicationOptionsForm: TApplicationOptionsForm;
    FCodeFontCharset: TFontCharset;
    FFavoritesNode,
    FHistoryNode: TTntTreeNode;
  protected
    procedure LoadConnectionPage; virtual;
    procedure ApplyConnectionChanges; virtual;
  public
    SelectedConnection: Integer;
  end;

const
  ConnectionsPage = -2;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  PNGTools, gnugettext;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TOptionsForm.Create(AOwner: TComponent; ApplOptionForm: TApplicationOptionsForm; MySQL: Pointer);

var
  I: Integer;
  RawEngines: PMYX_ENGINES;
  Engines: TMYX_ENGINES;

begin
  inherited Create(AOwner);

  Self.FApplicationOptionsForm := ApplOptionForm;

  // The option editor itself has no info about an established connection but gets (optionally) one here.
  // This connection is only used to fill the engine combobox with values supported by the current server.
  // If no connection is given then we simply use a generic list.
  if Assigned(MySQL) then
  begin
    RawEngines := myx_get_engines(MySQL);
    try
      Engines := TMYX_ENGINES.Create(RawEngines);
      try
        DefaultStorageEngineLU.Items.Clear;
        for I := 0 to Engines.engines.count - 1 do
          DefaultStorageEngineLU.Items.Add(Engines.engines[I].name);
      finally
        Engines.Free;
      end;
    finally
      myx_free_engines(RawEngines);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TOptionsForm.Destroy;

begin
  FApplicationOptionsForm.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.FormCreate(Sender: TObject);

begin
  FInitControls := True;
  try
    InitForm(Self);

    if (not (MYXCommonOptions.RestoreWindowPos(Self, False))) then
      Position := poMainFormCenter;

    FStoredConnections := nil;
    FDirty := False;
    SelectedConnection := -1;

    OptionPageControl.ActivePageIndex := 0;
    ConnectionPageControl.ActivePageIndex := 0;

    OptionPageControl.Top := 20;
    OptionPageControl.Height := OptionPageControl.Height - 16;
    HeaderPnl.BringToFront;

    Height := Height - 16;

    PasswordEd.PasswordChar := '*';

    BuildOptionSideBar;

    ShowOptionPage(0);

    DisableEnableConnectionPages(False);
  finally
    FInitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  MYXCommonOptions.AddWindowPos(Self);

  for i := 0 to FOptionImages.Count - 1 do
    TPNGObject(FOptionImages[i]).Free;

  FOptionImages.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  if (not (DiscardChanges)) then
    Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ShowOptionPage(PageNr: Integer);

var
  I: Integer;
  ALabel: TTntLabel;

begin
  if DiscardChanges then
  begin
    if PageNr < 0 then
      PageNr := OptionPageControl.PageCount + PageNr;

    OptionPageControl.ActivePageIndex := PageNr;
    TitleLbl.Caption := OptionPageControl.ActivePage.Caption;

    for I := 0 to SidebarScrollBox.ComponentCount - 1 do
      if TComponent(SidebarScrollBox.Components[I]) is TTntLabel then
      begin
        ALabel := TComponent(SidebarScrollBox.Components[I]) as TTntLabel;
        if ALabel.Tag = PageNr then
        begin
          ALabel.Color := clHighlight;
          ALabel.Font.Color := clHighlightText;
          ALabel.Transparent := False;
        end
        else
        begin
          ALabel.Transparent := True;
          ALabel.Font.Color := clWindowText;
        end;
      end;

    OptionPageControlChange(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.OptionImgClick(Sender: TObject);

begin
  ShowOptionPage(TComponent(Sender).Tag);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.AddOptionPage(IconName: WideString; TopPos: Integer; PageNr: Integer; PageTitle: WideString);

var
  Img: TTntImage;
  Lbl: TTntLabel;
  PNGImg: TPNGObject;

begin
  Img := TTntImage.Create(SidebarScrollBox);
  Img.Parent := SidebarScrollBox;
  Img.Width := 48;
  Img.Height := 48;
  Img.Left := (SidebarScrollBox.Width - Img.Width - 2) div 2;
  Img.Top := TopPos;
  PNGImg := LoadPNGImageFromResource(IconName, Img);
  FOptionImages.Add(PNGImg);
  Img.Tag := PageNr;
  Img.OnClick := OptionImgClick;

  Lbl := TTntLabel.Create(SidebarScrollBox);
  Lbl.Parent := SidebarScrollBox;
  Lbl.AutoSize := False;
  Lbl.Width := SidebarScrollBox.Width;
  Lbl.WordWrap := True;
  Lbl.Alignment := taCenter;
  Lbl.Caption := ' ' + PageTitle + ' ';
  Lbl.Top := TopPos + 48 + 4;
  Lbl.Height := TopPos + 84 - Lbl.Top;
  Lbl.Tag := PageNr;
  Lbl.Transparent := True;
  Lbl.OnClick := OptionImgClick;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.BuildOptionSideBar;

var 
  ypos, i: Integer;
  TabSheet: TTabSheet;
  ApplPagesCount: Integer;

begin
  FOptionImages := TList.Create;

  //Start at 25
  ypos := 25;
  ApplPagesCount := 0;
  if (FApplicationOptionsForm <> nil) then
  begin
    FApplicationOptionsForm.DoPageContentChanged := DoPageContentChanged;

    if (FApplicationOptionsForm.DockOptionPageControl <> nil) then
    begin
      ApplPagesCount := FApplicationOptionsForm.DockOptionPageControl.PageCount;

      for i := 0 to ApplPagesCount - 1 do
      begin
        TabSheet := FApplicationOptionsForm.DockOptionPageControl.Pages[0];
        TabSheet.PageControl := OptionPageControl;
        TabSheet.PageIndex := i;

        if (i < FApplicationOptionsForm.OptionsImgNames.Count) then
          AddOptionPage(FApplicationOptionsForm.OptionsImgNames[i],
            ypos, i, TabSheet.Caption);

        ypos := ypos + 90;
      end;
    end;
  end;

  AddOptionPage('options_general', ypos, ApplPagesCount, _('General Options'));

  ypos := ypos + 90;
  AddOptionPage('options_connections', ypos, ApplPagesCount + 1, _('Connections'));

  ypos := ypos + 90;
  AddOptionPage('options_editors', ypos, ApplPagesCount + 2, _('Editors'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.DisableEnableConnectionPages(Enable: Boolean);

var
  i: Integer;

begin
  // Enable/Disable Controls on pages
  for i := 0 to ConnectionPageControl.PageCount - 1 do
    DisableEnableControls(ConnectionPageControl.Pages[i], Enable);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.CloneConnectionButtonClick(Sender: TObject);

begin
  AddConnection(TMYX_USER_CONNECTION(ConnsTreeView.Selected.Data));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.CloseBtnClick(Sender: TObject);

begin
  ModalResult := mrOK;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.OptionTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if (Node <> nil) then
    if (Node.Data <> nil) then
      if (TObject(Node.Data) is TTabSheet) then
      begin
        OptionPageControl.ActivePage := Node.Data;
        TitleLbl.Caption := Node.Text;

        OptionPageControlChange(Self);
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.OptionPageControlChange(Sender: TObject);

begin
  LoadPageData(OptionPageControl.ActivePageIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ConnsTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  DeleteConnectionButton.Enabled := False;

  if Assigned(Node) then
    if Assigned(Node.Data) then
    begin
      FInitControls := True;
      try
        ConnectionEd.Text := TMYX_USER_CONNECTION(Node.Data).connection_name;
        UsernameEd.Text := TMYX_USER_CONNECTION(Node.Data).username;
        PasswordEd.Text := TMYX_USER_CONNECTION(Node.Data).password;
        HostnameEd.Text := TMYX_USER_CONNECTION(Node.Data).hostname;
        PortEd.Text := IntToStr(TMYX_USER_CONNECTION(Node.Data).port);
        TypeCBox.ItemIndex := Ord(TMYX_USER_CONNECTION(Node.Data).connection_type);
        SchemataEd.Text := TMYX_USER_CONNECTION(Node.Data).schema;
        ConnMemo.Text := TMYX_USER_CONNECTION(Node.Data).notes;

        ConnValueListEditor.Strings.Assign(TMYX_USER_CONNECTION(Node.Data).advanced_options);

        // Enable Controls on pages.
        DisableEnableConnectionPages(True);
      finally
        FInitControls := False;
      end;

      //Set last_connection
      SelectedConnection := FStoredConnections.user_connections.IndexOf(TMYX_USER_CONNECTION(Node.Data));

      if (Visible) and (ConnectionEd.Enabled) and
        (ConnectionPageControl.ActivePage = ConnGeneralTabSheet) then
        ConnectionEd.SetFocus;

      DeleteConnectionButton.Enabled := True;
      CloneConnectionButton.Enabled := True;
    end
    else
    begin
      FInitControls := True;
      try
        ConnectionEd.Text := '';
        UsernameEd.Text := '';
        PasswordEd.Text := '';
        HostnameEd.Text := '';
        PortEd.Text := '';
        TypeCBox.ItemIndex := -1;
        SchemataEd.Text := '';
        ConnMemo.Text := '';

        ConnValueListEditor.Strings.Clear;

        // Disable controls on pages
        DisableEnableConnectionPages(False);
        CloneConnectionButton.Enabled := False;
      finally
        FInitControls := False;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.DoPageContentChanged(Sender: TObject);

begin
  if not FInitControls then
  begin
    FDirty := True;
    ApplyChangesBtn.Enabled := True;
    DiscardChangesBtn.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TOptionsForm.DiscardChanges(ConfirmDiscard: Boolean): Boolean;

var
  res: Integer;

begin
  DiscardChanges := True;

  //Set DiscardingChanges so there will be no recursive calls
  FDiscardingChanges := True;
  try
    if (FDirty) then
    begin
      if (ConfirmDiscard) then
      begin
        res := ShowModalDialog(_('Save changes?'),
          _('Some of the settings have changed. Do you want to apply these changes permanently?'), myx_mtConfirmation,
          _('Yes') + #13#10 + _('No') + #13#10 + _('Cancel'));

        if (res = 1) then
          ApplyChanges
        else
          if (res = 3) then
          begin
            DiscardChanges := False;
            Exit;
          end;
      end;

      if OptionPageControl.ActivePage = ConnectionsTabSheet then
      begin
        // Reset connection edits.
        ConnsTreeViewChange(Self, ConnsTreeView.Selected);
      end
      else
        LoadPageData(OptionPageControl.ActivePageIndex);

      FDirty := False;
      ApplyChangesBtn.Enabled := False;
      DiscardChangesBtn.Enabled := False;
    end;
  finally
    FDiscardingChanges := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.OptionTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);

begin
  DiscardChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ApplyChangesBtnClick(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.DiscardChangesBtnClick(Sender: TObject);

begin
  DiscardChanges(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ConnsTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);

begin
  if (not (FDiscardingChanges)) then
    AllowChange := DiscardChanges
  else
    AllowChange := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ConnsTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  Connection: TMYX_USER_CONNECTION;

begin
  // Since we checked it in drag over we can safely assume the sender is the connections treeview.

  // First move the node in our storage...
  Connection := TMYX_USER_CONNECTION(ConnsTreeview.Selected.Data);
  FStoredConnections.user_connections.Extract(Connection);
  FStoredConnections.user_connections.Insert(ConnsTreeview.DropTarget.Index, Connection);

  // ..then in the treeview.
  ConnsTreeView.Selected.MoveTo(ConnsTreeview.DropTarget, naInsert);
  if ConnsTreeview.DropTarget.Parent = FFavoritesNode then
    Connection.storage_type := MYX_FAVORITE_USER_CONNECTION
  else
    Connection.storage_type := MYX_HISTORY_USER_CONNECTION;

  DoPageContentChanged(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ConnsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

var
  Node: TTreeNode;

begin
  if Sender <> ConnsTreeView then
    Accept := False
  else
    with ConnsTreeview do
    begin
      Node := DropTarget;
      Accept := (Selected <> DropTarget) and (Selected.Parent <> nil) and Assigned(Node) and (Node.Parent <> nil);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.DeleteConnectionButtonClick(Sender: TObject);

var
  pos: Integer;
  Node: TTreeNode;
  
begin
  if (ConnsTreeView.Selected <> nil) and (FStoredConnections <> nil) then
    if (ConnsTreeView.Selected.Data <> nil) then
    begin
      pos := FStoredConnections.user_connections.IndexOf(
        TMYX_USER_CONNECTION(ConnsTreeView.Selected.Data));

      if (pos <> -1) then
      begin
        if (ShowModalDialog(_('Delete connection?'), _('Are you sure you want to delete the connection?') +
          TMYX_USER_CONNECTION(ConnsTreeView.Selected.Data).connection_name + '?',
          myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1) then
        begin
          FDirty := False;
          ApplyChangesBtn.Enabled := False;
          DiscardChangesBtn.Enabled := False;

          FStoredConnections.user_connections.Delete(pos);
          Node := ConnsTreeView.Selected;
          ConnsTreeView.Selected := ConnsTreeView.Selected.GetPrev;
          ConnsTreeView.Items.Delete(Node);

          DoPageContentChanged(Self);
        end;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.AddConnection(UserConn: TMYX_USER_CONNECTION = nil);

// Adds a new connection to the connections treeview. If UserConn is not nil then we have to clone
// this connection and create a copy of it. Otherwise a new connection is created.

var
  Node: TTntTreeNode;
  NewConnection: TMYX_USER_CONNECTION;
  
begin
  if UserConn = nil then
  begin
    NewConnection := TMYX_USER_CONNECTION.Create(_('New Connection'), '', '', '', 3306, '', '', '',
      MYX_MYSQL_CONN, MYX_FAVORITE_USER_CONNECTION);
  end
  else
  begin
    NewConnection := TMYX_USER_CONNECTION.Create(UserConn.get_record_pointer);
    NewConnection.connection_name := _('Copy of ') + NewConnection.connection_name;
  end;

  Node := AddTreeViewChildNode(ConnsTreeView, ConnsTreeView.Items[0], NewConnection.connection_name, 6, NewConnection);
  FStoredConnections.user_connections.Add(NewConnection);
  ConnsTreeView.Selected := Node;

  ConnectionEd.SelectAll;

  DoPageContentChanged(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.AddConnectionButtonClick(Sender: TObject);

begin
  AddConnection;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.StorePasswordCBoxClick(Sender: TObject);

begin
  PasswordStorageTypeLbl.Enabled := StorePasswordCBox.Checked;
  PasswordStorageTypeCBox.Enabled := StorePasswordCBox.Checked;

  if (StorePasswordCBox.Checked) then
    PasswordStorageTypeCBox.ItemIndex := 0;

  DoPageContentChanged(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.LoadConnectionPage;

var
  I: Integer;
  user_conns: PMYX_USER_CONNECTIONS;
  error: MYX_LIB_ERROR;

begin
  if (FStoredConnections = nil) then
  begin
    ConnsTreeView.Items.Clear;

    //Fetch connections
    user_conns := myx_load_user_connections(
      MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml', @error);
    if (error <> MYX_NO_ERROR) then
      raise EMyxError.Create(_('Error while loading stored connections.') + _('Error Number ') + IntToStr(Ord(error)));

    try
    FStoredConnections := TMYX_USER_CONNECTIONS.create(user_conns);
    finally
      myx_free_user_connections(user_conns);
    end;

    FFavoritesNode := AddTreeViewChildNode(ConnsTreeView, nil, _('Connections'), 5, nil);
    FHistoryNode := AddTreeViewChildNode(ConnsTreeView, nil, _('History'), 5, nil);

    for i := 0 to FStoredConnections.user_connections.Count - 1 do
      if (FStoredConnections.user_connections[i].storage_type =
        MYX_FAVORITE_USER_CONNECTION) then
      begin
        AddTreeViewChildNode(ConnsTreeView, FFavoritesNode,
          FStoredConnections.user_connections[i].connection_name, 6,
          FStoredConnections.user_connections[i]);
      end
      else
      begin
        AddTreeViewChildNode(ConnsTreeView, FHistoryNode,
          FStoredConnections.user_connections[i].username + '@' +
          FStoredConnections.user_connections[i].hostname, 6,
          FStoredConnections.user_connections[i]);
      end;

      FFavoritesNode.Expand(True);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.LoadPageData(PageNr: Integer);

var
  StrList: TTntStringList;
  FontSize: Single;
  I: Integer;
  Datatype: PMYX_DBM_DATATYPE;
  Index: Integer;

begin
  FInitControls := True;
  try
    if (PageNr = OptionPageControl.PageCount - 3) then
    begin
      ShowTipOfDayCBox.Checked := MYXCommonOptions.ShowTipOfDay;
      StoreWindowsPosCBox.Checked := MYXCommonOptions.StoreWindowsPositions;
      DisableTransparencyEffectsCBox.Checked := MYXCommonOptions.DisableTransparencyEffects;

      LanguageCBox.Items.Text := MYXCommonOptions.AvailableLanguages;

      StrList := TTntStringList.Create;
      try
        StrList.Text := MYXCommonOptions.AvailableLanguageCodes;
        LanguageCBox.ItemIndex := StrList.IndexOf(MYXCommonOptions.Language);
      finally
        StrList.Free;
      end;

      StorePasswordCBox.Checked := not (
        (MYXCommonOptions.PasswordStorageType = MYX_PASSWORD_NOT_STORED));
      StorePasswordCBoxClick(Self);

      PasswordStorageTypeCBox.Items.Text := MYXCommonOptions.AvailablePasswordStorageTypes;
      PasswordStorageTypeCBox.ItemIndex := Ord(MYXCommonOptions.PasswordStorageType) - 2;

      DefaultFontLU.Items.Assign(Screen.Fonts);
      DataFontLU.Items.Assign(Screen.Fonts);
      CodeFontLU.Items.Assign(Screen.Fonts);

      Screen.ResetFonts;

      DefaultFontLU.ItemIndex := DefaultFontLU.Items.IndexOf(
        MYXCommonOptions.DefaultFontName);
      if (DefaultFontLU.ItemIndex = -1) then
        DefaultFontLU.ItemIndex := DefaultFontLU.Items.IndexOf('MS Sans Serif');

      // The font dialog only allows to pick fonts based on their font size, but we have its height here.
      // So we have to convert from height to size (assuming MM_TEXT as current DC mapping).
      FontSize := -MYXCommonOptions.DefaultFontHeight * 72 / GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
      DefaultFontSizeLU.Text := FloatToStrF(FontSize, ffGeneral, 1, 3);

      DataFontLU.ItemIndex := DataFontLU.Items.IndexOf(
        MYXCommonOptions.DataFontName);
      if (DataFontLU.ItemIndex = -1) then
        DataFontLU.ItemIndex := DataFontLU.Items.IndexOf('MS Sans Serif');

      FontSize := -MYXCommonOptions.DataFontHeight * 72 / GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
      DataFontSizeLU.Text := FloatToStrF(FontSize, ffGeneral, 1, 3);

      CodeFontLU.ItemIndex := CodeFontLU.Items.IndexOf(
        MYXCommonOptions.CodeFontName);
      if (CodeFontLU.ItemIndex = -1) then
        CodeFontLU.ItemIndex := CodeFontLU.Items.IndexOf('MS Sans Serif');

      CodeFontWidthLU.Text := IntToStr(MYXCommonOptions.CodeFontWidth);
      FontSize := -MYXCommonOptions.CodeFontHeight * 72 / GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
      CodeFontSizeLU.Text := FloatToStrF(FontSize, ffGeneral, 1, 3);
      FCodeFontCharset := MYXCommonOptions.CodeFontCharset;

      IgnoreWarningsList.Items.Text := MYXCommonOptions.IgnoreWarningsList.Text;
    end
    else
      if (PageNr = OptionPageControl.PageCount - 2) then
      begin
        LoadConnectionPage;
      end
      else
        if (PageNr = OptionPageControl.PageCount - 1) then
        begin
          KeepRoutineEditorOnTopCBox.Checked := MYXCommonOptions.EditorKeepRoutineEditorOnTop;
          ShowSpecialCharsCheckBox.Checked := MYXCommonOptionProvider.OptionAsBoolean['EditorShowWhitespaces'];
          EdTblShowSQLBeforeApplyingCBox.Checked := MYXCommonOptions.EditorTableShowSQLBeforeApplying;
          EdTblAllColsNotNullCBox.Checked := MYXCommonOptions.EditorTableAllColumnsNotNullPerDef;
          EdTblAllIntUnsignedCBox.Checked := MYXCommonOptions.EditorTableIntegerUnsignedPerDef;

          DefaultStorageEngineLU.ItemIndex := DefaultStorageEngineLU.Items.IndexOf(MYXCommonOptions.EditorTableDefaultStorageEngine);

          Index := EdTblPKAutoNamingEd.Items.IndexOf(MYXCommonOptions.EditorTablePKAutoNaming);
          if Index > -1 then
            EdTblPKAutoNamingEd.ItemIndex := Index
          else
            EdTblPKAutoNamingEd.Text := MYXCommonOptions.EditorTablePKAutoNaming;
          EdTblPKAutoNamingEd.SelStart := 0;

          Index := EdTblIndexNamingEd.Items.IndexOf(MYXCommonOptions.EditorTableIndexAutoNaming);
          if Index > -1 then
            EdTblIndexNamingEd.ItemIndex := Index
          else
            EdTblIndexNamingEd.Text := MYXCommonOptions.EditorTableIndexAutoNaming;
          EdTblIndexNamingEd.SelStart := 0;

          Index := EdTblFKNamingEd.Items.IndexOf(MYXCommonOptions.EditorTableFKAutoNaming);
          if Index > -1 then
            EdTblFKNamingEd.ItemIndex := Index
          else
            EdTblFKNamingEd.Text := MYXCommonOptions.EditorTableFKAutoNaming;
          EdTblFKNamingEd.SelStart := 0;

          EdTblPKDatatypeEd.Text := MYXCommonOptions.EditorTablePKDataType;
          DefaultDatatypeComboBox.Clear;

          with MYXCommonOptions do
          begin
            for I := 0 to MYXCommonOptions.Datatypes.datatypes_num - 1 do
            begin
              Datatype := PMYX_DBM_DATATYPE(Integer(Datatypes.datatypes) + SizeOf(MYX_DBM_DATATYPE) * I);
              DefaultDatatypeComboBox.Items.Add(Datatype.name);
            end;
          end;

          DefaultDatatypeComboBox.Text := MYXCommonOptions.EditorTableDefColumnDataType;
        end
        else
          if (FApplicationOptionsForm <> nil) then
            FApplicationOptionsForm.SetControls(PageNr);
  finally
    FInitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ApplyConnectionChanges;

begin
  if (ConnsTreeView.Selected <> nil) then
    if (ConnsTreeView.Selected.Data <> nil) then
    begin
      with TMYX_USER_CONNECTION(ConnsTreeView.Selected.Data) do
      begin
        connection_name := ConnectionEd.Text;
        username := UsernameEd.Text;
        password := PasswordEd.Text;
        hostname := HostnameEd.Text;
        port := StrToIntDef(PortEd.Text, 3306);
        connection_type := MYX_USER_CONNECTION_TYPE(TypeCBox.ItemIndex);
        schema := SchemataEd.Text;
        notes := ConnMemo.Text;

        advanced_options.Assign(ConnValueListEditor.Strings);
      end;

      ConnsTreeView.Selected.Text := ConnectionEd.Text;
    end;

  myx_store_user_connections(FStoredConnections.get_record_pointer, MYXCommonOptions.PasswordStorageType,
    MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ApplyChanges;

var
  StrList: TTntStringList;
  FontSize: Single;

begin
  if (OptionPageControl.ActivePage = GeneralTabSheet) then
  begin
    MYXCommonOptions.ShowTipOfDay := ShowTipOfDayCBox.Checked;
    MYXCommonOptions.StoreWindowsPositions := StoreWindowsPosCBox.Checked;
    MYXCommonOptions.DisableTransparencyEffects := DisableTransparencyEffectsCBox.Checked;

    StrList := TTntStringList.Create;
    try
      StrList.Text := MYXCommonOptions.AvailableLanguageCodes;
      if (LanguageCBox.ItemIndex > -1) then
        MYXCommonOptions.Language := StrList[LanguageCBox.ItemIndex];
    finally
      StrList.Free;
    end;

    if (not (StorePasswordCBox.Checked)) then
      MYXCommonOptions.PasswordStorageType := MYX_PASSWORD_NOT_STORED
    else
      MYXCommonOptions.PasswordStorageType := MYX_PASSWORD_STORAGE_TYPE(PasswordStorageTypeCBox.ItemIndex + 2);

    MYXCommonOptions.DefaultFontName := DefaultFontLU.Text;

    // The font dialog only allows to pick fonts based on their font size, but we have its height here.
    // So we have to convert from height to size (assuming MM_TEXT as current DC mapping).
    FontSize := Abs(StrToFloatDef(DefaultFontSizeLU.Text, 11));
    MYXCommonOptions.DefaultFontHeight := -Round(FontSize * GetDeviceCaps(Canvas.Handle, LOGPIXELSY) / 72);
    MYXCommonOptionProvider.OptionAsInteger['DefaultFontHeight'] := MYXCommonOptions.DefaultFontHeight;

    MYXCommonOptions.DataFontName := DataFontLU.Text;
    FontSize := Abs(StrToFloatDef(DataFontSizeLU.Text, 11));
    MYXCommonOptions.DataFontHeight := -Round(FontSize * GetDeviceCaps(Canvas.Handle, LOGPIXELSY) / 72);
    MYXCommonOptionProvider.OptionAsInteger['DataFontHeight'] := MYXCommonOptions.DataFontHeight;

    MYXCommonOptions.CodeFontName := CodeFontLU.Text;
    MYXCommonOptionProvider.OptionAsString['CodeFontName'] := MYXCommonOptions.CodeFontName;

    MYXCommonOptions.CodeFontWidth := StrToIntDef(CodeFontWidthLU.Text, 8);
    MYXCommonOptionProvider.OptionAsInteger['CodeFontWidth'] := MYXCommonOptions.CodeFontWidth;

    FontSize := Abs(StrToFloatDef(CodeFontSizeLU.Text, 11));
    MYXCommonOptions.CodeFontHeight := -Round(FontSize * GetDeviceCaps(Canvas.Handle, LOGPIXELSY) / 72);
    MYXCommonOptionProvider.OptionAsInteger['CodeFontHeight'] := MYXCommonOptions.CodeFontHeight;

    MYXCommonOptions.CodeFontCharset := FCodeFontCharset;
    MYXCommonOptionProvider.OptionAsInteger['CodeFontCharset'] := MYXCommonOptions.CodeFontCharSet;

    MYXCommonOptions.IgnoreWarningsList.Text := IgnoreWarningsList.Items.Text;

    MYXCommonOptions.StoreOptions;
  end
  else
    if (OptionPageControl.ActivePage = ConnectionsTabSheet) then
    begin
      ApplyConnectionChanges;
    end
    else
      if (OptionPageControl.ActivePage = EditorsTabSheet) then
      begin
        MYXCommonOptions.EditorKeepRoutineEditorOnTop := KeepRoutineEditorOnTopCBox.Checked;
        MYXCommonOptionProvider.OptionAsBoolean['EditorShowWhitespaces'] := ShowSpecialCharsCheckBox.Checked;
        MYXCommonOptions.EditorTableShowSQLBeforeApplying := EdTblShowSQLBeforeApplyingCBox.Checked;
        MYXCommonOptions.EditorTableAllColumnsNotNullPerDef := EdTblAllColsNotNullCBox.Checked;
        MYXCommonOptions.EditorTableIntegerUnsignedPerDef := EdTblAllIntUnsignedCBox.Checked;

        MYXCommonOptions.EditorTableDefaultStorageEngine := DefaultStorageEngineLU.Text;

        MYXCommonOptions.EditorTablePKAutoNaming := EdTblPKAutoNamingEd.Text;
        MYXCommonOptions.EditorTableIndexAutoNaming := EdTblIndexNamingEd.Text;
        MYXCommonOptions.EditorTableFKAutoNaming := EdTblFKNamingEd.Text;

        MYXCommonOptions.EditorTablePKDataType := EdTblPKDatatypeEd.Text;
        MYXCommonOptions.EditorTableDefColumnDataType := Uppercase(DefaultDatatypeComboBox.Text);

        MYXCommonOptions.StoreOptions;
      end
      else
        if (FApplicationOptionsForm <> nil) then
        begin
          FApplicationOptionsForm.ApplyChanges(OptionPageControl.ActivePageIndex);

          MYXCommonOptions.StoreOptions;
        end;

  FDirty := False;
  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;

  MessageToAllForms(WM_OptionsChanged, OptionPageControl.ActivePageIndex, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key = VK_F1) then
    ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.ApplicationFontBtnClick(Sender: TObject);

var
  FontDialog: TFontDialog;
  NewIndex: Integer;

begin
  FontDialog := TFontDialog.Create(nil);
  FontDialog.Options := [fdForceFontExist, fdNoStyleSel];
  try
    FontDialog.Font.Name := DefaultFontLU.Text;
    FontDialog.Font.Size := StrToIntDef(DefaultFontSizeLU.Text, 8);
    if FontDialog.Execute then
    begin
      NewIndex := DefaultFontLU.Items.IndexOf(FontDialog.Font.Name);
      if NewIndex > -1 then
      begin
        DefaultFontLU.ItemIndex := NewIndex;
        DefaultFontSizeLU.Text := IntToStr(FontDialog.Font.Size);
        DoPageContentChanged(Self);
      end;
    end;
  finally
    FontDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.DataFontBtnClick(Sender: TObject);

var
  FontDialog: TFontDialog;
  NewIndex: Integer;

begin
  FontDialog := TFontDialog.Create(nil);
  FontDialog.Options := [fdForceFontExist, fdNoStyleSel];
  try
    FontDialog.Font.Name := DataFontLU.Text;
    FontDialog.Font.Size := StrToIntDef(DataFontSizeLU.Text, 8);
    if FontDialog.Execute then
    begin
      NewIndex := DataFontLU.Items.IndexOf(FontDialog.Font.Name);
      if NewIndex > -1 then
      begin
        DataFontLU.ItemIndex := NewIndex;
        DataFontSizeLU.Text := IntToStr(FontDialog.Font.Size);
        DoPageContentChanged(Self);
      end;
    end;
  finally
    FontDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.CodeFontBtnClick(Sender: TObject);

var
  FontDialog: TFontDialog;
  NewIndex: Integer;

begin
  FontDialog := TFontDialog.Create(nil);
  FontDialog.Options := [fdForceFontExist, fdNoStyleSel];
  try
    FontDialog.Font.Name := CodeFontLU.Text;
    FontDialog.Font.Size := StrToIntDef(CodeFontSizeLU.Text, 8);
    FontDialog.Font.Charset := FCodeFontCharset;
    if (FontDialog.Execute) then
    begin
      NewIndex := CodeFontLU.Items.IndexOf(FontDialog.Font.Name);
      if NewIndex > -1 then
      begin
        CodeFontLU.ItemIndex := NewIndex;
        CodeFontSizeLU.Text := IntToStr(FontDialog.Font.Size);
        CodeFontWidthLU.Text := IntToStr(FontDialog.Font.Size);
        FCodeFontCharset := FontDialog.Font.Charset;
        DoPageContentChanged(Self);
      end;
    end;
  finally
    FontDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TOptionsForm.RemoveWarningFromIgnoreListBtnClick(Sender: TObject);

var
  i: Integer;

begin
  i := 0;
  while (i < IgnoreWarningsList.Items.Count) do
  begin
    if (IgnoreWarningsList.Selected[i]) then
      IgnoreWarningsList.Items.Delete(i)
    else
      inc(i);
  end;

  DoPageContentChanged(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

