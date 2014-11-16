unit AdminServerHealth;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, StrUtils, Contnrs, Clipbrd,
  Series, InstanceSections, TntClasses, TntForms,
  ApplicationDataModule, AuxFuncs, 
  myx_admin_public_interface, myx_util_public_interface,
  myx_public_interface,
  AdditionalClasses,
  AdminServerHealthLineGraph,
  AdminServerHealthBarGraph, ImgList, Menus,
  PNGImage, MySQLConnection,
  Options, MyxError, TntExtCtrls,
  AdminServerHealthGraphSettings, TntMenus, TntStdCtrls, TntComCtrls;

type
  TVarListId = class (TObject)
    mysql_id: WideString;

    constructor Create(mysql_id: WideString);
  end;

  TAdminServerHealthForm = class(TInstanceSectionForm)
    ServerStatusPnl: TTntPanel;
    HealthPageControl: TTntPageControl;
    ServerStatusTabSheet: TTabSheet;
    ServerVariablesTabSheet: TTabSheet;
    ServerStatusMainPnl: TTntPanel;
    ServerStatusTreeView: TTntTreeView;
    StatusVarSplitter: TTntSplitter ;
    ServerStatusListView: TTntListView;
    ServerVariablesMainPnl: TTntPanel;
    ServerVarSplitter: TTntSplitter ;
    ServerVariablesTreeView: TTntTreeView;
    VariablesListView: TTntListView;
    GraphRefreshTmr: TTimer;
    VarImageList: TImageList;
    VariablesPopupMenu: TTntPopupMenu;
    CopyVariablestoClipboardMI: TTntMenuItem;
    CopyselectedVariableMI: TTntMenuItem;
    HeaderPnl: TTntPanel;
    MemoryHealthBevel: TTntBevel;
    HeaderDescriptionLbl: TTntLabel;
    HeaderImg: TTntImage;
    HeaderLbl: TTntLabel;
    BottomPnl: TTntPanel;
    RefreshBtn: TTntButton;
    Panel1: TTntPanel;
    Button1: TTntButton;
    CopyselectedVariableNameMI: TTntMenuItem;
    PageControlPopupMenu: TTntPopupMenu;
    AddaPageMI: TTntMenuItem;
    AddaGroupMI: TTntMenuItem;
    DeletePageMI: TTntMenuItem;
    N1: TTntMenuItem;
    GroupBoxPopupMenu: TTntPopupMenu;
    AddaGraphMI: TTntMenuItem;
    DeleteGroupMI: TTntMenuItem;
    N2: TTntMenuItem;
    GraphPopupMenu: TTntPopupMenu;
    EditGraphMI: TTntMenuItem;
    N3: TTntMenuItem;
    DeleteGraphMI: TTntMenuItem;
    AddaGraph2MI: TTntMenuItem;
    DeleteGroup2MI: TTntMenuItem;
    AddaGroup2MI: TTntMenuItem;
    AddaGroup3MI: TTntMenuItem;
    RestoreDefaultsMI: TTntMenuItem;
    RestoreDefaults2MI: TTntMenuItem;
    N4: TTntMenuItem;
    N5: TTntMenuItem;
    N6: TTntMenuItem;
    N7: TTntMenuItem;
    RestoreDefaults3MI: TTntMenuItem;
    DeletePage2MI: TTntMenuItem;
    DeletePage3MI: TTntMenuItem;
    AddPage2MI: TTntMenuItem;
    AddaPage3MI: TTntMenuItem;
    MoveGraphUpMI: TTntMenuItem;
    MoveGraphDownMI: TTntMenuItem;
    N8: TTntMenuItem;
    N9: TTntMenuItem;
    GraphPopupMenuImgs: TImageList;
    MultiInstanceMonitoringSheet: TTntTabSheet;
    MerlinHeaderPnl: TTntPanel;
    MerlinScrollBox: TTntScrollBox;
    TntImage1: TTntImage;
    TntImage2: TTntImage;
    TntImage3: TTntImage;
    TntLabel1: TTntLabel;
    MerlinRichEdit: TTntRichEdit;
    MerlinRichEdit2: TTntRichEdit;
    MerlinInfo1Btn: TTntButton;
    MerlinInfo2Btn: TTntButton;
    SpacerPanel: TTntPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ServerStatusPnlResize(Sender: TObject);
    procedure HealthPageControlChange(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure BuildVariablesTreeViews;
    procedure RefreshVars(Treeview: TTntTreeView; var CurrentVarGroup: Pointer;
      ListView: TTntListView; Vars: TMYX_VARIABLES;
      VarListing: TMYX_VARIABLES_LISTING; var NotListedVars: TTntStringList);

    procedure FetchStatusVars(Sender: TObject);
    procedure FetchedStatusVars(Sender: TObject);
    procedure RefreshStatusVars;
    procedure FetchServerVars(Sender: TObject);
    procedure FetchedServerVars(Sender: TObject);
    procedure RefreshServerVars;
    procedure ServerStatusTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ServerVariablesTreeViewChange(Sender: TObject;
      Node: TTreeNode);

    procedure FetchStatusVarsTimer(Sender: TObject);
    procedure FetchedStatusVarsTimer(Sender: TObject);
    procedure GraphRefreshTmrTimer(Sender: TObject);
    function GetVarIndex(vars: TMYX_VARIABLES;
      VarName: WideString): Integer;
    procedure PrepareGraphFormulars;

    procedure LoadGraphs;
    procedure SaveGraphs;
    procedure BuildGraphs;
    procedure AddPage(page: TMYX_HEALTH_PAGE;
      var PageTabSheet: TTabSheet; var ScrollBox: TTntScrollBox);
    procedure AddGraphToGroupBox(GroupBox: TTntGroupBox;
      graph: TMYX_HEALTH_GRAPH; var current_widget_ypos: Integer);

    procedure CopyVariablestoClipboardMIClick(Sender: TObject);
    procedure VariablesListViewDblClick(Sender: TObject);
    procedure CopyselectedVariableMIClick(Sender: TObject);
    procedure CopyselectedVariableNameMIClick(Sender: TObject);

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;

    procedure AddaPageMIClick(Sender: TObject);
    procedure DeletePageMIClick(Sender: TObject);
    procedure AddaGroupMIClick(Sender: TObject);
    procedure EditGraphMIClick(Sender: TObject);
    procedure ServerStatusListViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AddaGraphMIClick(Sender: TObject);

    procedure OptionsChanged(var Message: TMessage); message WM_OptionsChanged;
    procedure DeleteGraphMIClick(Sender: TObject);
    procedure DeleteGraph(Graph: TControl);
    procedure DeleteGroupMIClick(Sender: TObject);
    procedure DeleteGroupBox(GroupBox: TTntGroupBox);
    procedure RestoreDefaultsMIClick(Sender: TObject);
    procedure DeletePage(pos: Integer; AskBeforeDelete: Boolean = True);
    procedure PageControlPopupMenuPopup(Sender: TObject);
    procedure MoveGraphUpMIClick(Sender: TObject);
    procedure MoveGraph(Frame: TTntFrame; MoveUp: Boolean);
    procedure MoveGraphDownMIClick(Sender: TObject);
    procedure ListViewDeletion(Sender: TObject; Item: TListItem);
    procedure MerlinInfo1BtnClick(Sender: TObject);
    procedure MultiInstanceMonitoringSheetShow(Sender: TObject);
  private
    server_vars,
    status_vars,
    status_vars_timer,
    status_vars_timer_old: TMYX_VARIABLES;

    StatusVarListing,
    ServerVarListing: TMYX_VARIABLES_LISTING;

    NotListedStatusVars,
    NotListedServerVars: TTntStringList;

    CurrentStatusVarGroup,
    CurrentServerVarGroup: Pointer;

    StatusVar_PMySQL: Pointer;

    GraphFrameBundleList: TObjectList;

    ClosingApplication: Boolean;

    HeaderPNGImg: TPNGObject;

    AdminServerHealthGraphSettingsForm: TAdminServerHealthGraphSettingsForm;

    GraphControlCount: Integer;
  public
    HealthPages: TMYX_HEALTH_PAGES;

    RefreshSystemVarsInFormulas: Boolean;
  end;

const
  GroupBoxWidth = 535;
  GroupBoxVSpace = 10;

var
  AdminServerHealthForm: TAdminServerHealthForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  PNGTools;

//----------------------------------------------------------------------------------------------------------------------

constructor TVarListId.Create(mysql_id: WideString);

begin
  self.mysql_id := mysql_id;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel := ServerStatusPnl;

  server_vars := nil;
  status_vars := nil;
  status_vars_timer := nil;
  status_vars_timer_old := nil;

  StatusVarListing := nil;
  ServerVarListing := nil;

  CurrentStatusVarGroup := nil;
  CurrentServerVarGroup := nil;

  NotListedStatusVars := nil;
  NotListedServerVars := nil;

  StatusVar_PMySQL := nil;

  GraphControlCount := 0;

  AdminServerHealthGraphSettingsForm := nil;

  ClosingApplication := False;

  HealthPages := nil;

  RefreshSystemVarsInFormulas := False;

  HeaderPNGImg := LoadPNGImageFromResource('health', HeaderImg);

  BuildVariablesTreeViews;

  ServerStatusTreeView.FullExpand;
  ServerVariablesTreeView.FullExpand;

  if MYXCommonOptions.XPStyleEnabled then
  begin
    StatusVarSplitter.Color := clWhite;
    ServerVarSplitter.Color := clWhite;
  end;

  GraphFrameBundleList := TObjectList.Create;

  if MySQLConn.Connected then
  begin
    BuildGraphs;
    GraphRefreshTmr.Enabled := True;
  end
  else
  begin
    DisableEnableControls(ServerStatusTabSheet, False);
    DisableEnableControls(ServerVariablesTabSheet, False);
  end;

  HealthPageControl.ActivePageIndex := 0;
  HealthPageControlChange(self);

  MerlinHeaderPnl.Visible := False;

  LoadRtfTextFromResource(MerlinRichEdit, 'merlin_graph_text');
  LoadRtfTextFromResource(MerlinRichEdit2, 'merlin_grid_graph_text');

  MerlinScrollBox.VertScrollBar.Position := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FormDestroy(Sender: TObject);

begin
  HeaderPNGImg.Free;
  AdminServerHealthGraphSettingsForm.Free;
  GraphFrameBundleList.Free;

  server_vars.Free;
  status_vars.Free;
  status_vars_timer.Free;
  status_vars_timer_old.Free;

  StatusVarListing.Free;
  ServerVarListing.Free;

  NotListedStatusVars.Free;
  NotListedServerVars.Free;

  if(StatusVar_PMySQL<>nil)then
    myx_mysql_close(StatusVar_PMySQL);

  if Assigned(HealthPages) then
  begin
    myx_save_health_pages(HealthPages.get_record_pointer,
      MYXCommonOptions.UserDataDir+'mysqladmin_health.xml');

    HealthPages.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  if(AdminServerHealthGraphSettingsForm<>nil)then
    AdminServerHealthGraphSettingsForm.Close;
    
  ClosingApplication := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ServerStatusPnlResize(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to GraphFrameBundleList.Count-1 do
    if(TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame is TAdminServerHealthLineGraphFrame)then
      TAdminServerHealthLineGraphFrame(TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame).FrameResize(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.HealthPageControlChange(Sender: TObject);

begin
  HeaderPnl.Parent := HealthPageControl.ActivePage;
  HeaderPnl.Align := alTop;
  HeaderPnl.Visible := True;

  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
  begin
    HeaderLbl.Caption := _('Status Variables');
    HeaderDescriptionLbl.Caption := _('Current status variables of the server.');
  end
  else if(HealthPageControl.ActivePage=ServerVariablesTabSheet)then
  begin
    HeaderLbl.Caption := _('System Variables');
    HeaderDescriptionLbl.Caption := _('Overview of the system variables.');
  end
  else if(HealthPageControl.ActivePage=MultiInstanceMonitoringSheet)then
  begin
    HeaderLbl.Caption := _('MySQL Monitoring && Advisors');
    HeaderDescriptionLbl.Caption := _('The Enterprise Dashboard.');
  end
  else
  begin
    if(HealthPageControl.ActivePageIndex<=HealthPages.pages.Count)then
    begin
      HeaderLbl.Caption := HealthPages.pages[HealthPageControl.ActivePageIndex].caption;
      HeaderDescriptionLbl.Caption := HealthPages.pages[HealthPageControl.ActivePageIndex].description;
    end;
  end;

  ServerStatusPnlResize(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.RefreshBtnClick(Sender: TObject);

begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
  begin
    FreeAndNil(status_vars);
    RefreshStatusVars;
  end
  else if(HealthPageControl.ActivePage=ServerVariablesTabSheet)then
  begin
    FreeAndNil(server_vars);
    RefreshServerVars;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.BuildVariablesTreeViews;

var
  PVarListing: PMYX_VARIABLES_LISTING;
  error: MYX_ADMIN_LIB_ERROR;
  I, J: Integer;
  GroupNode: TTntTreeNode;

begin
  PVarListing := myx_get_variables_listing(
    MYXCommonOptions.XMLDir+'mysqladmin_status_variables.xml', @error);
  if(error<>MYX_ADMIN_NO_ERROR)then
    raise EMyxError.Create(_('The status variables definition could '+
      'not be loaded from the XML file.')+#13#10+
      _('Error Nr.')+IntToStr(Ord(error)));
  try
    StatusVarListing := TMYX_VARIABLES_LISTING.create(PVarListing);
  finally
    myx_free_variables_listing(PVarListing);
  end;

  ServerStatusTreeView.Items.BeginUpdate;
  try
    ServerStatusTreeView.Items.Clear;

    for I := 0 to StatusVarListing.groups.Count-1 do
    begin
      GroupNode := AddTreeViewChildNode(ServerStatusTreeView, nil,
        StatusVarListing.groups[I].name, 4, StatusVarListing.groups[I]);

      for J := 0 to StatusVarListing.groups[I].subgroups.Count-1 do
      begin
        AddTreeViewChildNode(ServerStatusTreeView, GroupNode,
          StatusVarListing.groups[I].subgroups[J].name, 4,
          StatusVarListing.groups[I].subgroups[J]);
      end;
    end;
  finally
    ServerStatusTreeView.Items.EndUpdate;
  end;


  PVarListing := myx_get_variables_listing(
   MYXCommonOptions.XMLDir+'mysqladmin_system_variables.xml', @error);
  if(error<>MYX_ADMIN_NO_ERROR)then
    raise EMyxError.Create(_('The status variables definition could '+
      'not be loaded from the XML file.') + #13#10+
      _('Error Nr.')+IntToStr(Ord(error)));
  try
    ServerVarListing := TMYX_VARIABLES_LISTING.create(PVarListing);
  finally
    myx_free_variables_listing(PVarListing);
  end;

  ServerVariablesTreeView.Items.BeginUpdate;
  try
    ServerVariablesTreeView.Items.Clear;

    for I := 0 to ServerVarListing.groups.Count-1 do
    begin
      GroupNode := AddTreeViewChildNode(ServerVariablesTreeView, nil,
        ServerVarListing.groups[I].name, 4, ServerVarListing.groups[I]);

      for J := 0 to ServerVarListing.groups[I].subgroups.Count-1 do
      begin
        AddTreeViewChildNode(ServerVariablesTreeView, GroupNode,
          ServerVarListing.groups[I].subgroups[J].name, 4,
          ServerVarListing.groups[I].subgroups[J]);
      end;
    end;
  finally
    ServerVariablesTreeView.Items.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.RefreshVars(Treeview: TTntTreeView; var CurrentVarGroup: Pointer;
  ListView: TTntListView; Vars: TMYX_VARIABLES; VarListing: TMYX_VARIABLES_LISTING; var NotListedVars: TTntStringList);

var
  I, J, K, L: Integer;
  Item: TListItem;
  found: Boolean;
  VarGrp: TMYX_VARIABLES_GROUP;

begin
  if(VarListing=nil)then
    Exit;

  //get all vars that are not listed
  if(NotListedVars=nil)then
  begin
    NotListedVars := TTntStringList.Create;

    for K := 0 to Vars.variables.Count-1 do
    begin
      found := False;

      //search for a description of Vars.variables[k].name
      for I := 0 to VarListing.groups.Count-1 do
      begin
        //compare with all variables of this group
        for J := 0 to VarListing.groups[I].variables.Count-1 do
        begin
          if(CompareText(VarListing.groups[I].variables[J].mysql_id,
            Vars.variables[K].name)=0)then
          begin
            found := True;
            break;
          end;
        end;

        //compare with all variables in all subgroups of this group
        if(Not(found))then
        begin
          for J := 0 to VarListing.groups[I].subgroups.Count-1 do
          begin
            for L := 0 to VarListing.groups[I].subgroups[J].variables.Count-1 do
              if(CompareText(VarListing.groups[I].subgroups[J].variables[L].mysql_id,
                Vars.variables[K].name)=0)then
              begin
                found := True;
                break;
              end;

            if(found)then
              break;
          end;
        end;

        if(found)then
          break;
      end;

      if(Not(found))then
        NotListedVars.Add(Vars.variables[K].name);
    end;

    if(NotListedVars.Text<>'')then
    begin
      VarGrp := TMYX_VARIABLES_GROUP.create(VarListing.groups.Count,
        _('New Variables'), '');
      VarListing.groups.Add(VarGrp);

      for I := 0 to NotListedVars.Count-1 do
        VarGrp.variables.Add(TMYX_VARIABLE_ELEMENT.create(
          NotListedVars[I], _('New Variable'), 0));

      AddTreeViewChildNode(Treeview, nil,
        _('New Variables'), 4, VarGrp);
    end;
  end;


  ListView.Items.BeginUpdate;
  try
    if(Treeview.Selected<>nil)then
      if(Treeview.Selected.Data<>nil)then
      begin
        if(CurrentVarGroup<>Treeview.Selected.Data)then
        begin
          if(TObject(Treeview.Selected.Data) is TMYX_VARIABLES_GROUP)then
          begin
            ListView.Items.Clear;

            for I := 0 to Vars.variables.Count-1 do
              for J := 0 to TMYX_VARIABLES_GROUP(Treeview.Selected.Data).variables.Count-1 do
                if(Vars.variables[I].name=
                  TMYX_VARIABLES_GROUP(Treeview.Selected.Data).variables[J].mysql_id)then
                begin
                  Item := AddListViewItem(ListView, nil,
                    Vars.variables[I].name,
                    TMYX_VARIABLES_GROUP(Treeview.Selected.Data).variables[J].editable,
                    TVarListId.Create(Vars.variables[I].name));

                  Item.SubItems.Add(Vars.variables[I].value);
                  Item.SubItems.Add(_(TMYX_VARIABLES_GROUP(Treeview.Selected.Data).variables[J].desc_id));
                end;

            CurrentVarGroup := Treeview.Selected.Data;
          end
          else if(TObject(Treeview.Selected.Data) is TMYX_VARIABLES_SUBGROUP)then
          begin
            if(CurrentVarGroup<>Treeview.Selected.Data)then
            begin
              ListView.Items.Clear;

              for I := 0 to Vars.variables.Count-1 do
                for J := 0 to TMYX_VARIABLES_SUBGROUP(Treeview.Selected.Data).variables.Count-1 do
                  if(Vars.variables[I].name=
                    TMYX_VARIABLES_SUBGROUP(Treeview.Selected.Data).variables[J].mysql_id)then
                  begin
                    Item := AddListViewItem(ListView, nil,
                      Vars.variables[I].name, TMYX_VARIABLES_SUBGROUP(Treeview.Selected.Data).variables[J].editable,
                      TVarListId.Create(Vars.variables[I].name));

                    Item.SubItems.Add(Vars.variables[I].value);
                    Item.SubItems.Add(_(TMYX_VARIABLES_SUBGROUP(Treeview.Selected.Data).variables[J].desc_id));
                  end;

              CurrentVarGroup := Treeview.Selected.Data;
            end;
          end;
        end
        else
        begin
          //Current Group has NOT changed, so simply refresh Values
          for I := 0 to Vars.variables.Count-1 do
            for J := 0 to ListView.Items.Count-1 do
              if(ListView.Items[J].Data<>nil)then
                if(TObject(ListView.Items[J].Data) is TVarListId)then
                  if(Vars.variables[I].name=
                    TVarListId(ListView.Items[J].Data).mysql_id)then
                  begin
                    if(ListView.Items[J].SubItems.Count>0)then
                      ListView.Items[J].SubItems[0] := Vars.variables[I].value;
                  end;

        end;
      end;
  finally
    ListView.Items.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchStatusVars(Sender: TObject);

var
  pvars: PMYX_VARIABLES;

begin
  pvars := myx_get_status_variables(MySQLConn.MySQL);
  if(pvars=nil)then
    raise EMyxError.Create(_('Error while fetching Status Variables.'));
  try
    status_vars := TMYX_VARIABLES.create(pvars);
  finally
    myx_free_variables(pvars);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchedStatusVars(Sender: TObject);

begin
  RefreshStatusVars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.RefreshStatusVars;

begin
  if(Not(MySQLConn.Connected))then
    Exit;

  if(status_vars=nil)then
    MySQLConn.FetchData(dkStatusVars,
      FetchStatusVars, FetchedStatusVars,
      nil, _('Fetching Status Variables ...'))
  else
    RefreshVars(ServerStatusTreeView, CurrentStatusVarGroup,
      ServerStatusListView, status_vars,
      StatusVarListing, NotListedStatusVars);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchServerVars(Sender: TObject);

var
  pvars: PMYX_VARIABLES;

begin
  pvars := myx_get_server_variables(MySQLConn.MySQL);
  if(pvars=nil)then
    raise EMyxError.Create(_('Error while fetching Server Variables.'));
  try
    server_vars := TMYX_VARIABLES.create(pvars);
  finally
    myx_free_variables(pvars);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchedServerVars(Sender: TObject);

begin
  RefreshServerVars;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.RefreshServerVars;

begin
  if(Not(MySQLConn.Connected))then
    Exit;

  if(server_vars=nil)then
    MySQLConn.FetchData(djServerVars,
      FetchServerVars, FetchedServerVars,
      nil, _('Fetching System Variables ...'))
  else
  begin
    RefreshVars(ServerVariablesTreeView, CurrentServerVarGroup,
      VariablesListView, server_vars,
      ServerVarListing, NotListedServerVars);

    RefreshSystemVarsInFormulas := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ServerStatusTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not (csDestroying in ComponentState) then
  begin
    if status_vars = nil then
      RefreshStatusVars
    else
      RefreshVars(ServerStatusTreeView, CurrentStatusVarGroup, ServerStatusListView, status_vars, StatusVarListing,
        NotListedStatusVars);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ServerVariablesTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not (csDestroying in ComponentState) then
  begin
    if server_vars = nil then
      RefreshServerVars
    else
      RefreshVars(ServerVariablesTreeView, CurrentServerVarGroup, VariablesListView, server_vars, ServerVarListing, NotListedServerVars);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.CopyVariablestoClipboardMIClick(Sender: TObject);

var
  I: Integer;
  txt: WideString;
  ListView: TTntListView;

begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
    ListView := ServerStatusListView
  else
    ListView := VariablesListView;

  with ListView do
  begin
    txt := '';
    for I := 0 to Items.Count-1 do
    begin
      txt := txt+StringAlignLeft(Items[I].Caption, 32);
      if(Items[I].SubItems.Count>0)then
        txt := txt+Items[I].SubItems[0];
      txt := txt+#13#10;
    end;

    Clipboard.AsText := txt;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.CopyselectedVariableMIClick(Sender: TObject);

var
  txt: WideString;
  ListView: TTntListView;

begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
    ListView := ServerStatusListView
  else
    ListView := VariablesListView;

  if(ListView.Selected<>nil)then
  begin
    txt := txt+StringAlignLeft(ListView.Selected.Caption, 32);
    if(ListView.Selected.SubItems.Count>0)then
      txt := txt+ListView.Selected.SubItems[0];

    Clipboard.AsText := txt;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.CopyselectedVariableNameMIClick(Sender: TObject);

var
  txt: WideString;
  ListView: TTntListView;

begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
    ListView := ServerStatusListView
  else
    ListView := VariablesListView;

  if(ListView.Selected<>nil)then
  begin
    txt := txt+ListView.Selected.Caption;

    Clipboard.AsText := txt;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.VariablesListViewDblClick(Sender: TObject);

var
  ListView: TTntListView;
  Value: WideString;

begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)then
    ListView := ServerStatusListView
  else
    ListView := VariablesListView;

  if(ListView.Selected<>nil)then
    if(ListView.Selected.ImageIndex=1)and
      (ListView.Selected.Data<>nil)then
    begin
      if(ListView.Selected.SubItems.Count>0)then
        Value := ListView.Selected.SubItems[0]
      else
        Value := '';

      if(ShowModalEditDialog(_('Set Variable'),
        _('Please enter the new value of the variable.'),
        myx_mtEdit, _('Set')+#13#10+_('Abort'),
        True, TVarListId(ListView.Selected.Data).mysql_id+':',
        Value)=1)then
      begin
        if(myx_set_variable(MySQLConn.MySQL,
          TVarListId(ListView.Selected.Data).mysql_id,
          Value)<>0)then
        begin
          raise EMyxSQLError.Create(
            Format(_('Variable %s cannot be set.'), [TVarListId(ListView.Selected.Data).mysql_id]),
            myx_mysql_errno(MySQLConn.MySQL),
            myx_mysql_error(MySQLConn.MySQL));
        end
        else
          RefreshBtnClick(self);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchStatusVarsTimer(Sender: TObject);

var
  pvars: PMYX_VARIABLES;
  MySQLErrorNr: Integer;

begin
  if(StatusVar_PMySQL=nil)then
  begin
    StatusVar_PMySQL := myx_mysql_init();
    if(StatusVar_PMySQL=nil)then
      raise EMyxError.Create(_('Error while allocating memory for MySQL Struct.'));

    if myx_connect_to_instance(MySQLConn.UserConnection.get_record_pointer, StatusVar_PMySQL) <> 0 then
    begin
      StatusVar_PMySQL := nil;
      Exit;
    end;
  end;

  pvars := myx_get_status_variables(StatusVar_PMySQL);
  if pvars = nil then
  begin
    MySQLErrorNr := myx_mysql_errno(StatusVar_PMySQL);
    TFetchDataThread(Sender).StatusBarText := format(_('Error fetching Status Variables (%s)'), [IntToStr(MySQLErrorNr)]);
    Exit;
  end;

  try
    status_vars_timer := TMYX_VARIABLES.create(pvars);
  finally
    myx_free_variables(pvars);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.FetchedStatusVarsTimer(Sender: TObject);

var
  I: Integer;
  val, MaxVal: Double;
  Pcompiled_expr,
  Pcompiled_MaxExpr: PMYX_COMPILED_EXPRESSION;
  expr_error: MYX_EXPRESSION_ERROR;
  PVars, POldVars: PMYX_VARIABLES;
  graph: TMYX_HEALTH_GRAPH;
  
begin
  if(HealthPages=nil)or(ClosingApplication)or(Not(MySQLConn.Connected))then
    Exit;

  if(status_vars_timer<>nil)and(status_vars_timer_old<>nil)then
  begin
    PVars := status_vars_timer.get_record_pointer;
    POldVars := status_vars_timer_old.get_record_pointer;

    if(RefreshSystemVarsInFormulas)then
    begin
      PrepareGraphFormulars;
      RefreshSystemVarsInFormulas := False;
    end;

    for I := 0 to GraphFrameBundleList.Count-1 do
    begin
      graph := TGraphFrameBundle(GraphFrameBundleList[I]).Graph;

      Pcompiled_expr := myx_compile_expression(
        TGraphFrameBundle(GraphFrameBundleList[I]).value_formular,
        @expr_error);
      if(expr_error=MYX_EXPRESSION_SYNTAX_ERROR)then
      begin
        SetStatusText(format(_('Expression error in graph %s'),[graph.graph_caption])+': '+
          TGraphFrameBundle(GraphFrameBundleList[I]).value_formular);
        continue;
      end;

      if(TGraphFrameBundle(GraphFrameBundleList[I]).max_formular<>'')then
      begin
        Pcompiled_MaxExpr := myx_compile_expression(
          TGraphFrameBundle(GraphFrameBundleList[I]).max_formular,
          @expr_error);
        if(expr_error=MYX_EXPRESSION_SYNTAX_ERROR)then
        begin
          SetStatusText(format(_('Expression error in graph %s'),[graph.graph_caption])+': '+
            TGraphFrameBundle(GraphFrameBundleList[I]).max_formular);
          continue;
        end;
      end
      else
        Pcompiled_MaxExpr := nil;

      try
        val := Abs(myx_eval_expression(Pcompiled_expr, POldVars, PVars, @expr_error));
        if(expr_error=MYX_EXPRESSION_DIVISION_BY_ZERO)then
          val := 0
        else if(expr_error=MYX_EXPRESSION_SYNTAX_ERROR)then
        begin
          SetStatusText(format(_('Expression error in graph %s'),[graph.graph_caption])+': '+
            TGraphFrameBundle(GraphFrameBundleList[I]).value_formular);
          continue;
        end;
      finally
        myx_free_expression(Pcompiled_expr);
      end;

      if(Pcompiled_MaxExpr<>nil)then
      begin
        try
          MaxVal := myx_eval_expression(Pcompiled_MaxExpr, POldVars, PVars, @expr_error);
          if(expr_error=MYX_EXPRESSION_DIVISION_BY_ZERO)then
            val := 0
          else if(expr_error=MYX_EXPRESSION_SYNTAX_ERROR)then
          begin
            SetStatusText(format(_('Expression error in graph %s'),[graph.graph_caption])+': '+
              TGraphFrameBundle(GraphFrameBundleList[I]).max_formular);
            continue;
          end;
        finally
          myx_free_expression(Pcompiled_MaxExpr);
        end;
      end
      else
        MaxVal := graph.max;

      //Add new value to graph
      if(graph.graphtype=MYX_LINE_GRAPH)then
      begin
        TAdminServerHealthLineGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetValue(val);

        TAdminServerHealthLineGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetMaxValue(MaxVal);
      end
      else if(graph.graphtype=MYX_BAR_GRAPH)then
      begin
        TAdminServerHealthBarGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetMaxValue(MaxVal);

        TAdminServerHealthBarGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetValue(val);
      end;
    end;
  end
  else
  begin
    PrepareGraphFormulars;

    //Initialize graphs with 0 values
    for I := 0 to GraphFrameBundleList.Count-1 do
    begin
      if(TGraphFrameBundle(GraphFrameBundleList[I]).Graph.graphtype=
        MYX_LINE_GRAPH)then
      begin
        TAdminServerHealthLineGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetValue(0);
      end
      else if(TGraphFrameBundle(GraphFrameBundleList[I]).Graph.graphtype=
        MYX_BAR_GRAPH)then
      begin
        TAdminServerHealthBarGraphFrame(
          TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame
          ).SetValue(0);
      end;
    end;
  end;

  if(status_vars_timer_old<>nil)then
    status_vars_timer_old.Free;

  status_vars_timer_old := status_vars_timer;
  status_vars_timer := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.PrepareGraphFormulars;

var
  I, J: Integer;

begin
  if(status_vars_timer=nil)or(server_vars=nil)then
    Exit;

  for I := 0 to GraphFrameBundleList.Count-1 do
  begin
    TGraphFrameBundle(GraphFrameBundleList[I]).value_formular :=
      TGraphFrameBundle(GraphFrameBundleList[I]).graph.value_formula;
    TGraphFrameBundle(GraphFrameBundleList[I]).max_formular :=
      TGraphFrameBundle(GraphFrameBundleList[I]).graph.max_formula;
  end;

  //Replace all status variable names with index numbers
  for I := 0 to status_vars_timer.variables.Count-1 do
    for J := 0 to GraphFrameBundleList.Count-1 do
    begin
      TGraphFrameBundle(GraphFrameBundleList[J]).value_formular :=
        AnsiReplaceText(TGraphFrameBundle(GraphFrameBundleList[J]).value_formular,
        '['+status_vars_timer.variables[I].name+']',
        '['+IntToStr(I)+']');

      TGraphFrameBundle(GraphFrameBundleList[J]).max_formular :=
        AnsiReplaceText(TGraphFrameBundle(GraphFrameBundleList[J]).max_formular,
        '['+status_vars_timer.variables[I].name+']',
        '['+IntToStr(I)+']');
    end;

  //Replace all system variable names with values
  for I := 0 to server_vars.variables.Count-1 do
    for J := 0 to GraphFrameBundleList.Count-1 do
    begin
      TGraphFrameBundle(GraphFrameBundleList[J]).value_formular :=
        AnsiReplaceText(TGraphFrameBundle(GraphFrameBundleList[J]).value_formular,
        '['+server_vars.variables[I].name+']',
        server_vars.variables[I].value);

      TGraphFrameBundle(GraphFrameBundleList[J]).max_formular :=
        AnsiReplaceText(TGraphFrameBundle(GraphFrameBundleList[J]).max_formular,
        '['+server_vars.variables[I].name+']',
        server_vars.variables[I].value);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.GraphRefreshTmrTimer(Sender: TObject);

begin
  if not ClosingApplication and (MySQLConn.Ping = 0) then
  begin
    // Get new status vars every time
    // TODO: Use only one thread that awakes every second and collects the data.
    if(status_vars_timer<>nil)then
    begin
      status_vars_timer.Free;
      status_vars_timer := nil;
    end;

    MySQLConn.FetchData(dkStatusVars, FetchStatusVarsTimer, FetchedStatusVarsTimer, nil, '', False, False, False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminServerHealthForm.GetVarIndex(vars: TMYX_VARIABLES; VarName: WideString): Integer;

var
  I, VarPos: Integer;

begin
  VarPos := 0;

  for I := 0 to vars.variables.Count-1 do
    if(CompareText(vars.variables[I].Name, VarName)=0)then
    begin
      VarPos := I;
      break;
    end;

  Result := VarPos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.LoadGraphs;

var
  phealth_pages: PMYX_HEALTH_PAGES;
  error: MYX_ADMIN_LIB_ERROR;

begin
  if(Not(FileExists(MYXCommonOptions.UserDataDir+'mysqladmin_health.xml')))then
    CopyDiskFile(MYXCommonOptions.XMLDir+'mysqladmin_health.xml',
      MYXCommonOptions.UserDataDir+'mysqladmin_health.xml', False);

  phealth_pages := myx_read_in_health_pages(
    MYXCommonOptions.UserDataDir+'mysqladmin_health.xml',
    @error);
  if(phealth_pages=nil)or(error<>MYX_ADMIN_NO_ERROR)then
    raise EMyxAdminLibError.Create(
      Format(_('Error while loading the health graphs from %s.'),
        [MYXCommonOptions.UserDataDir+'mysqladmin_health.xml']),
      Ord(error),
      MYXCommonOptions.XMLDir+'mysqladmin_health.xml');

  try
    HealthPages.Free;
    HealthPages := TMYX_HEALTH_PAGES.Create(phealth_pages);
  finally
    myx_free_health_pages(phealth_pages);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.SaveGraphs;

begin
  if(HealthPages<>nil)then
    myx_save_health_pages(HealthPages.get_record_pointer, MYXCommonOptions.UserDataDir+'mysqladmin_health.xml');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.BuildGraphs;

var
  I, J, K: Integer;
  PageTabSheet: TTabSheet;
  ScrollBox: TTntScrollBox;
  GroupBox: TTntGroupBox;
  page: TMYX_HEALTH_PAGE;
  current_widget_ypos,
  current_group_height,
  current_group_ypos: Integer;

begin
  LoadGraphs;

  if(HealthPages<>nil)then
  begin
    for I := 0 to HealthPages.pages.Count-1 do
    begin
      page := HealthPages.pages[I];

      PageTabSheet := nil;
      ScrollBox := nil;
      AddPage(page, PageTabSheet, ScrollBox);

      current_group_ypos := 8;

      for J := 0 to page.groups.Count-1 do
      begin
        GroupBox := TTntGroupBox.Create(ScrollBox);
        GroupBox.Parent := ScrollBox;
        GroupBox.Name := 'GBox'+IntToStr(GraphControlCount);
        GroupBox.Caption := _(page.groups[J].caption);
        GroupBox.Left := 12;
        GroupBox.Top := current_group_ypos;
        GroupBox.Width := GroupBoxWidth;
        GroupBox.Anchors := [akLeft, akTop, akRight];
        GroupBox.PopupMenu := GroupBoxPopupMenu;
        inc(GraphControlCount);

        current_group_height := 25;

        current_widget_ypos := 22;
        for K := 0 to page.groups[J].graphs.Count-1 do
        begin
          AddGraphToGroupBox(GroupBox, page.groups[J].graphs[K],
            current_widget_ypos);

          current_group_height := current_widget_ypos;
        end;

        GroupBox.Height := current_group_height;

        inc(current_group_ypos, GroupBox.Height+GroupBoxVSpace);
      end;
    end;

    //Now display Pages
    for I := 0 to HealthPageControl.PageCount-1 do
      HealthPageControl.Pages[I].TabVisible := True;
  end;

  ServerStatusTabSheet.PageIndex := HealthPageControl.PageCount-1;
  ServerVariablesTabSheet.PageIndex := HealthPageControl.PageCount-1;
  MultiInstanceMonitoringSheet.PageIndex := HealthPageControl.PageCount - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.AddPage(page: TMYX_HEALTH_PAGE; var PageTabSheet: TTabSheet; var ScrollBox: TTntScrollBox);

begin
  PageTabSheet := TTabSheet.Create(self);
  PageTabSheet.Name := 'HeathPage'+IntToStr(GraphControlCount);
  PageTabSheet.Caption := _(page.caption);
  PageTabSheet.TabVisible := False;
  PageTabSheet.PageControl := HealthPageControl;
  PageTabSheet.Width := 563;
  PageTabSheet.Height := 412;
  inc(GraphControlCount);

  ScrollBox := TTntScrollBox.Create(PageTabSheet);
  ScrollBox.Parent := PageTabSheet;
  ScrollBox.Name := 'SB'+IntToStr(GraphControlCount);
  ScrollBox.Top := 46;
  ScrollBox.Left := 0;
  ScrollBox.Height := PageTabSheet.Height-ScrollBox.Top;
  ScrollBox.Width := PageTabSheet.Width;
  ScrollBox.Anchors := [akLeft, akTop, akRight, akBottom];
  ScrollBox.BorderStyle := bsNone;
  if(MYXCommonOptions.XPStyleEnabled)then
    ScrollBox.Color := clWhite;
  ScrollBox.VertScrollBar.Smooth := True;
  ScrollBox.VertScrollBar.Tracking := True;
  ScrollBox.AutoScroll := True;
  ScrollBox.Width := PageTabSheet.Width;
  inc(GraphControlCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.AddGraphToGroupBox(GroupBox: TTntGroupBox; graph: TMYX_HEALTH_GRAPH; var current_widget_ypos: Integer);

var
  LineGraph: TAdminServerHealthLineGraphFrame;
  BarGraph: TAdminServerHealthBarGraphFrame;

begin
  if(graph.graphtype=MYX_LINE_GRAPH)then
  begin
    LineGraph := TAdminServerHealthLineGraphFrame.Create(
      GroupBox,
      graph,
      _(graph.value_caption),
      graph.min,
      graph.max,
      graph.value_unit);
    LineGraph.Parent := GroupBox;
    LineGraph.Name := 'HealthGraph_'+IntToStr(GraphControlCount);
    LineGraph.Left := 16;
    LineGraph.Top := current_widget_ypos;
    LineGraph.Width := GroupBox.Width-30; //504;
    LineGraph.Anchors := [akLeft, akTop, akRight];
    LineGraph.PopupMenu := GraphPopupMenu;
    LineGraph.OnDblClick := EditGraphMIClick;
    inc(GraphControlCount);

    GraphFrameBundleList.Add(TGraphFrameBundle.Create(graph, LineGraph));

    Inc(current_widget_ypos, LineGraph.Height+10);
  end
  else if(graph.graphtype=MYX_BAR_GRAPH)then
  begin
    BarGraph := TAdminServerHealthbarGraphFrame.Create(
      GroupBox,
      _(graph.graph_caption),
      graph.display_graph_caption,
      _(graph.value_caption),
      _(graph.max_caption),
      graph.min,
      graph.max,
      graph.value_unit);
    BarGraph.Parent := GroupBox;
    BarGraph.Name := 'HealthGraph_'+IntToStr(GraphControlCount);
    BarGraph.Left := 16;
    BarGraph.Top := current_widget_ypos;
    BarGraph.Width := GroupBox.Width-30;
    BarGraph.Anchors := [akLeft, akTop, akRight];
    BarGraph.PopupMenu := GraphPopupMenu;
    BarGraph.OnDblClick := EditGraphMIClick;
    inc(GraphControlCount);

    GraphFrameBundleList.Add(TGraphFrameBundle.Create(graph, BarGraph));

    Inc(current_widget_ypos, 19+10);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ConnectionLost(var Message: TMessage);

begin
  GraphRefreshTmr.Enabled := False;
  if Assigned(StatusVar_PMySQL)then
    myx_mysql_close(StatusVar_PMySQL);
  StatusVar_PMySQL := nil;

  DisableEnablePages(HealthPageControl, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ConnectionEstablished(var Message: TMessage);

begin
  DisableEnablePages(HealthPageControl, True);

  if(HealthPages=nil)then
    BuildGraphs;

  if(server_vars=nil)then
    FetchServerVars(self);

  if(status_vars=nil)then
    FetchStatusVars(self);

  GraphRefreshTmr.Enabled := True;

  HealthPageControl.ActivePageIndex := 0;
  HealthPageControlChange(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.AddaPageMIClick(Sender: TObject);

var
  Caption,
  Description: Widestring;
  pos: Integer;
  page: TMYX_HEALTH_PAGE;
  PageTabSheet: TTabSheet;
  ScrollBox: TTntScrollBox;

begin
  if(ShowModalEditDialog(_('Add a Page'),
    _('Enter the caption of the page to add.'),
    myx_mtEdit, _('Continue')+#13#10+_('Cancel'),
    True, _('Page Caption:'), Caption)=1)then
  begin
    if(ShowModalEditDialog(_('Page Description'),
      format(_('Enter the page description of the new %s page'),[Caption]),
      myx_mtEdit, _('Create Page')+#13#10+_('Cancel'),
      True, _('Page Description:'), Description)=1)then
    begin
      pos := HealthPageControl.ActivePageIndex+1;
      if(pos>HealthPages.pages.Count-1)then
        pos := HealthPages.pages.Count-1;
      if(pos<0)then
        pos := 0;

      page := TMYX_HEALTH_PAGE.create(Caption, Caption,
        Description, Description,
        pos);
      HealthPages.pages.Insert(pos, page);


      PageTabSheet := nil;
      ScrollBox := nil;
      AddPage(page, PageTabSheet, ScrollBox);

      PageTabSheet.PageIndex := pos;
      PageTabSheet.TabVisible := True;

      HealthPageControl.ActivePage := PageTabSheet;

      HealthPageControlChange(self);

      SaveGraphs;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeletePage(pos: Integer; AskBeforeDelete: Boolean);

var
  TabSheet: TTabSheet;
  I, J, K: Integer;
  Res: Integer;

begin
  if(pos<HealthPageControl.PageCount-2)and
    (pos<HealthPages.pages.Count)then
  begin
    if(AskBeforeDelete)then
      Res := ShowModalDialog(_('Delete Page?'),
        Format(_('Are you sure you want to delete the selected page %s?'), [HealthPages.pages[pos].caption]),
        myx_mtConfirmation, _('Yes')+ #13#10 + _('No'))
    else
      Res := 1;

    if(Res=1)then
    begin
      GraphRefreshTmr.Enabled := False;

      HealthPageControl.ActivePageIndex := pos+1;

      //Hide Tab since it cannot be removed
      TabSheet := HealthPageControl.Pages[pos];
      //TabSheet.TabVisible := False;
      TabSheet.PageIndex := HealthPageControl.PageCount-1;

      while(TabSheet.ComponentCount>0)do
        DeleteComponentWithChildren(TabSheet.Components[0]);

      for I := 0 to HealthPages.pages[pos].groups.Count-1 do
        for J := 0 to HealthPages.pages[pos].groups[I].graphs.Count-1 do
          for K := 0 to GraphFrameBundleList.Count-1 do
            if(TGraphFrameBundle(GraphFrameBundleList[K]).Graph=
              HealthPages.pages[pos].groups[I].graphs[J])then
            begin
              GraphFrameBundleList.Delete(K);
              break;
            end;

      HealthPages.pages.Delete(pos);

      HealthPageControlChange(self);

      TabSheet.Free;

      GraphRefreshTmr.Enabled := True;

      SaveGraphs;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeletePageMIClick(Sender: TObject);

begin
  DeletePage(HealthPageControl.ActivePageIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.AddaGroupMIClick(Sender: TObject);

var
  Caption: WideString;
  pos: Integer;
  group: TMYX_HEALTH_GROUP;
  GroupBox: TTntGroupBox;
  ScrollBox: TTntScrollBox;
  current_group_ypos: Integer;
  I: Integer;

begin
  if(HealthPageControl.ActivePage.Controls[0] is TTntScrollBox)then
    ScrollBox := TTntScrollBox(HealthPageControl.ActivePage.Controls[0])
  else
    Exit;

  if(ShowModalEditDialog(_('Add a Group'),
    _('Enter the caption of the group to add.'),
    myx_mtEdit, _('Create Group') + #13#10 + _('Cancel'),
    True, _('Group Caption:'), Caption)=1)then
  begin
    pos := HealthPages.pages[HealthPageControl.ActivePageIndex].groups.Count;

    group := TMYX_HEALTH_GROUP.create(Caption, Caption, pos+1);

    HealthPages.pages[HealthPageControl.ActivePageIndex].groups.Add(group);

    current_group_ypos := 0;

    for I := 0 to ScrollBox.ComponentCount-1 do
      if(ScrollBox.Components[I] is TTntGroupBox)then
        if(TTntGroupBox(ScrollBox.Components[I]).Top+
          TTntGroupBox(ScrollBox.Components[I]).Height+GroupBoxVSpace>
            current_group_ypos)then
          current_group_ypos := TTntGroupBox(ScrollBox.Components[I]).Top+
            TTntGroupBox(ScrollBox.Components[I]).Height+GroupBoxVSpace;

    GroupBox := TTntGroupBox.Create(ScrollBox);
    GroupBox.Parent := ScrollBox;
    GroupBox.Name := 'GBox'+IntToStr(GraphControlCount);
    GroupBox.Caption := Caption;
    GroupBox.Left := 12;
    GroupBox.Top := current_group_ypos;
    GroupBox.Width := ScrollBox.Width-28;
    GroupBox.Anchors := [akLeft, akTop, akRight];
    GroupBox.PopupMenu := GroupBoxPopupMenu;
    GroupBox.Height := 25;
    inc(GraphControlCount);

    SaveGraphs;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.EditGraphMIClick(Sender: TObject);

var
  Frame: TTntFrame;
  I: Integer;
  
begin
  if Sender is TTntFrame then
    Frame := Sender as TTntFrame
  else
    if TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent.InheritsFrom(TTntFrame) then
    Frame := TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent as TTntFrame
  else
    Frame := nil;

  if Assigned(Frame) then
  begin
    if AdminServerHealthGraphSettingsForm = nil then
      AdminServerHealthGraphSettingsForm := TAdminServerHealthGraphSettingsForm.Create(self);

    for I := 0 to GraphFrameBundleList.Count - 1 do
      if TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame = Frame then
      begin
        AdminServerHealthGraphSettingsForm.SetGraph(TGraphFrameBundle(GraphFrameBundleList[I]));
        Break;
      end;

    AdminServerHealthGraphSettingsForm.Show;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ServerStatusListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  //TTntListView(Sender).BeginDrag(False, 5);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.AddaGraphMIClick(Sender: TObject);

var
  GroupBox: TTntGroupBox;
  Component: TComponent;

begin
  Component := TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent;
  if Component is TTntGroupBox then
    GroupBox := Component as TTntGroupBox
  else
    if Component.InheritsFrom(TTntFrame) then
    GroupBox := Component.Owner as TTntGroupBox
  else
    GroupBox := nil;

  if Assigned(GroupBox) then
  begin
    if AdminServerHealthGraphSettingsForm = nil then
      AdminServerHealthGraphSettingsForm := TAdminServerHealthGraphSettingsForm.Create(self);

    AdminServerHealthGraphSettingsForm.NewGraph(GroupBox);
    AdminServerHealthGraphSettingsForm.Show;
    AdminServerHealthGraphSettingsForm.ActiveControl := AdminServerHealthGraphSettingsForm.GraphTypeCBox;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.OptionsChanged(var Message: TMessage);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeleteGraphMIClick(Sender: TObject);

begin
  if(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent is TAdminServerHealthBarGraphFrame)or
    (TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent is TAdminServerHealthLineGraphFrame)then
    DeleteGraph(TControl(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeleteGraph(Graph: TControl);

var
  GBox: TTntGroupBox;
  I, J, GHeight: Integer;

begin
  GBox := TTntGroupBox(Graph.Parent);

  GraphRefreshTmr.Enabled := False;

  GHeight := Graph.Height;

  for I := 0 to GBox.ControlCount-1 do
  begin
    if(GBox.Controls[I]<>Graph)then
      if(TControl(GBox.Components[I]).Top>Graph.Top)then
        TControl(GBox.Components[I]).Top :=
          TControl(GBox.Components[I]).Top-(GHeight+10);
  end;

  //Free Graph and GraphFrameBundleList Item
  for I := 0 to GraphFrameBundleList.Count-1 do
    if(TGraphFrameBundle(GraphFrameBundleList[I]).GraphFrame=Graph)then
    begin
      for J := 0 to HealthPages.pages[HealthPageControl.ActivePageIndex].groups.Count-1 do
      begin
        if(HealthPages.pages[HealthPageControl.ActivePageIndex].groups[J].graphs.IndexOf(
          TGraphFrameBundle(GraphFrameBundleList[I]).Graph)>=0)then
          HealthPages.pages[HealthPageControl.ActivePageIndex].groups[J].graphs.Remove(
            TGraphFrameBundle(GraphFrameBundleList[I]).Graph);
      end;

      GraphFrameBundleList.Delete(I);
      break;
    end;

  GBox.Height := GBox.Height-(GHeight+10);

  for I := 0 to GBox.Parent.ControlCount-1 do
    if(GBox.Parent.Controls[I].Top>GBox.Top)then
      GBox.Parent.Controls[I].Top := GBox.Parent.Controls[I].Top-(GHeight+10);

  Graph.Free;

  GraphRefreshTmr.Enabled := True;

  SaveGraphs;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeleteGroupMIClick(Sender: TObject);

begin
  if(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent is TTntGroupBox)then
    DeleteGroupBox(TTntGroupBox(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent))
  else if(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent is TAdminServerHealthBarGraphFrame)or
    (TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent is TAdminServerHealthLineGraphFrame)then
    DeleteGroupBox(TTntGroupBox(TControl(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent).Parent))

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.DeleteGroupBox(GroupBox: TTntGroupBox);

var
  I, GroupNr: Integer;

begin
  I := 0;
  while(I<GroupBox.ControlCount)do
  begin
    if(GroupBox.Controls[I] is TAdminServerHealthBarGraphFrame)or
      (GroupBox.Controls[I] is TAdminServerHealthLineGraphFrame)then
      DeleteGraph(GroupBox.Controls[I])
    else
      inc(I);
  end;

  for I := 0 to GroupBox.Parent.ControlCount-1 do
    if(GroupBox.Parent.Controls[I].Top>GroupBox.Top)then
      GroupBox.Parent.Controls[I].Top := GroupBox.Parent.Controls[I].Top-
        (GroupBox.Height+GroupBoxVSpace);

  //delete group from HealthPages
  GroupNr := 0;
  for I := 0 to GroupBox.Parent.ControlCount-1 do
    if(GroupBox=GroupBox.Parent.Controls[I])then
    begin
      GroupNr := I;
      break;
    end;

  if(GroupNr<HealthPages.pages[HealthPageControl.ActivePageIndex].groups.Count)then
    HealthPages.pages[
      HealthPageControl.ActivePageIndex].groups.Delete(GroupNr);

  GroupBox.Free;

  SaveGraphs;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.RestoreDefaultsMIClick(Sender: TObject);

var
  I, Index: Integer;

begin
  Index := HealthPageControl.PageCount-2;
  for I := 0 to Index do
    DeletePage(0, False);

  //Restore original file
  CopyDiskFile(MYXCommonOptions.XMLDir+'mysqladmin_health.xml',
    MYXCommonOptions.UserDataDir+'mysqladmin_health.xml', False);


  status_vars_timer_old.Free;
  status_vars_timer_old := nil;

  BuildGraphs;

  HealthPageControl.ActivePageIndex := 0;
  HealthPageControlChange(self);
end;

procedure TAdminServerHealthForm.PageControlPopupMenuPopup(
  Sender: TObject);
begin
  if(HealthPageControl.ActivePage=ServerStatusTabSheet)or
    (HealthPageControl.ActivePage=ServerVariablesTabSheet)then
  begin
    AddaGroupMI.Enabled := False;
    RestoreDefaults3MI.Enabled := False;
  end
  else
  begin
    AddaGroupMI.Enabled := True;
    RestoreDefaults3MI.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.MoveGraphUpMIClick(Sender: TObject);

begin
  if(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent.InheritsFrom(TTntFrame))then
    MoveGraph(TTntFrame(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent),
    True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.MultiInstanceMonitoringSheetShow(Sender: TObject);

begin
  // There is a bug with right aligned anchors on a scrollbox or so.
  // Work around that by setting the right anchor here.
  with MultiInstanceMonitoringSheet do
  begin
    TntImage1.Left := Width - 15 - TntImage1.Width;
    TntImage1.Anchors := TntImage1.Anchors + [akRight];
    TntImage2.Left := Width - 20 - TntImage2.Width;
    TntImage2.Anchors := TntImage2.Anchors + [akRight];
    TntImage3.Left := Width - 20 - TntImage3.Width;
    TntImage3.Anchors := TntImage3.Anchors + [akRight];

    MerlinRichEdit.Width := TntImage2.Left - MerlinRichEdit.Left - 20;
    MerlinRichEdit.Anchors := MerlinRichEdit.Anchors + [akRight];
    MerlinRichEdit2.Width := TntImage2.Left - MerlinRichEdit.Left - 20;
    MerlinRichEdit2.Anchors := MerlinRichEdit2.Anchors + [akRight];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.MoveGraphDownMIClick(Sender: TObject);

begin
  if(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent.InheritsFrom(TTntFrame))then
    MoveGraph(TTntFrame(TTntPopupMenu(TTntMenuItem(Sender).GetParentMenu).PopupComponent), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.MerlinInfo1BtnClick(Sender: TObject);

begin
  BrowseWebPage('http://mysql.com/mashowcase');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.MoveGraph(Frame: TTntFrame; MoveUp: Boolean);

var
  GroupBox: TTntGroupBox;
  i, FlipIndex, FlipPos: Integer;
  Graph, OtherGraph: TMYX_HEALTH_GRAPH;
  OtherFrame: TTntFrame;
  
begin
  if(Frame.Parent is TTntGroupBox)then
  begin
    GroupBox := TTntGroupBox(Frame.Parent);
    if(GroupBox<>nil)then
    begin
      FlipIndex := -1;
      FlipPos := 1000*Ord(Not(MoveUp));

      for i := 0 to GroupBox.ControlCount-1 do
        if(GroupBox.Controls[i]<>Frame)and
          (GroupBox.Controls[i].Top<Frame.Top)and
          (MoveUp)then
        begin
          if(FlipPos<GroupBox.Controls[i].Top)then
          begin
            FlipPos := GroupBox.Controls[i].Top;
            FlipIndex := i;
          end;
        end
        else if(GroupBox.Controls[i]<>Frame)and
          (GroupBox.Controls[i].Top>Frame.Top)and
          (Not(MoveUp))then
        begin
          if(FlipPos>GroupBox.Controls[i].Top)then
          begin
            FlipPos := GroupBox.Controls[i].Top;
            FlipIndex := i;
          end;
        end;

      if(FlipIndex>=0) and (GroupBox.Controls[FlipIndex] is TTntFrame)then
      begin
        OtherFrame := TTntFrame(GroupBox.Controls[FlipIndex]);

        if not(MoveUp) then
        begin
          FlipPos := OtherFrame.Top;
          OtherFrame.Top := Frame.Top;
          Frame.Top := FlipPos + OtherFrame.Height - Frame.Height;
        end
        else
        begin
          FlipPos := OtherFrame.Top;
          OtherFrame.Top := Frame.Top - OtherFrame.Height + Frame.Height;
          Frame.Top := FlipPos;
        end;

        // simply flip pos Integer in the graph definition
        Graph := nil;
        OtherGraph := nil;

        // find the graphs
        for i := 0 to GraphFrameBundleList.Count-1 do
          if(TGraphFrameBundle(GraphFrameBundleList[i]).GraphFrame=
            Frame)then
          begin
            Graph := TGraphFrameBundle(GraphFrameBundleList[i]).Graph;
            break;
          end;

        for i := 0 to GraphFrameBundleList.Count-1 do
          if(TGraphFrameBundle(GraphFrameBundleList[i]).GraphFrame=
            OtherFrame)then
          begin
            OtherGraph := TGraphFrameBundle(GraphFrameBundleList[i]).Graph;
            break;
          end;

        // flip their position
        if(Graph<>nil)and(OtherGraph<>nil)then
        begin
          FlipPos := Graph.pos;
          Graph.pos := OtherGraph.pos;
          OtherGraph.pos := FlipPos;
        end;

        SaveGraphs;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthForm.ListViewDeletion(Sender: TObject; Item: TListItem);

begin
  TObject(Item.Data).Free;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
