unit AdminBackup;

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

{$Include Compilers.inc}

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  InstanceSections, AdditionalClasses,              
  ImgList, Buttons, Contnrs, Menus, ApplicationDataModule, AuxFuncs,
  myx_public_interface, myx_admin_public_interface, SchemataTreeView,
  AdvancedEdit, PNGImage,
  AdminBackupProgress, AuxAdminBackupRestore,
  MySQLConnection, Options, MyxError, TntExtCtrls, 
  Math, OptionsEditor, AdminOptionPages,
  TntMenus, TntStdCtrls, TntForms, TntClasses, TntDialogs,
  ActiveX, MSTask, VirtualTrees, TntComCtrls;

type
  TBackupThread = class;

  TAdminBackupForm = class(TInstanceSectionForm)
    ServerBackupRestorePnl: TTntPanel;
    BackupRestorePageControl: TTntPageControl;
    BackupContentTabSheet: TTabSheet;
    Panel1: TTntPanel;
    RestoreBevel: TTntBevel;
    Label3: TTntLabel;
    Backup2Img: TTntImage;
    SubTreePnl: TTntPanel;
    BackupTreeViewPopupMenu: TTntPopupMenu;
    RemoveBackupNodeMI: TTntMenuItem;
    ProfilesPopupMenu: TTntPopupMenu;
    RefreshProfilesMI: TTntMenuItem;
    SchemataFrame: TSchemataFrame;
    ContentSplitter: TTntSplitter;
    Panel2: TTntPanel;
    TopPnl: TTntPanel;
    SchemataLbl: TTntLabel;
    AdvancedEditFrame: TAdvancedEditFrame;
    SpacerPnl: TTntPanel;
    ProjectListView: TTntListView;
    AddSchemaToBackupBtn: TTntButton;
    RemoveSchemaFromBackup: TTntButton;
    Panel3: TTntPanel;
    BackupTreeView: TVirtualStringTree;
    Panel4: TTntPanel;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Panel7: TTntPanel;
    N1: TTntMenuItem;
    DeleteProfileMI: TTntMenuItem;
    ClearCompleteContentMI: TTntMenuItem;
    AdvancedOptionsTabSheet: TTabSheet;
    Panel5: TTntPanel;
    Bevel1: TTntBevel;
    Label15: TTntLabel;
    Backup3Img: TTntImage;
    Label16: TTntLabel;
    NewProjectBtn: TTntButton;
    ApplyBtn: TTntButton;
    StartBackupBtn: TTntButton;
    Panel9: TTntPanel;
    GroupBox1: TTntGroupBox;
    Label4: TTntLabel;
    Label5: TTntLabel;
    ProjectNameEd: TTntEdit;
    ScheduleTabSheet: TTabSheet;
    ScheduleGBox: TTntGroupBox;
    ScheduleTargetDirCaptionLbl: TTntLabel;
    ScheduleTargetDirLbl: TTntLabel;
    SchedulePrefixCaptionLbl: TTntLabel;
    SchedulePrefixLbl: TTntLabel;
    ScheduleLbl: TTntLabel;
    ScheduleTargetDirEd: TTntEdit;
    ScheduleBrowseTargetDirBtn: TTntButton;
    ScheduleFilenamePrefixEd: TTntEdit;
    ScheduleCBox: TTntCheckBox;
    Panel6: TTntPanel;
    BackupBevel: TTntBevel;
    Label6: TTntLabel;
    BackupImg: TTntImage;
    Label7: TTntLabel;
    ExecutionTimeGBox: TTntGroupBox;
    SchedulePageControl: TTntPageControl;
    DailyTabSheet: TTabSheet;
    Label26: TTntLabel;
    WeeklyTabSheet: TTabSheet;
    Label21: TTntLabel;
    ScheduleMondayCBox: TTntCheckBox;
    ScheduleTuesdayCBox: TTntCheckBox;
    ScheduleWednesdayCBox: TTntCheckBox;
    ScheduleThursdayCBox: TTntCheckBox;
    ScheduleFridayCBox: TTntCheckBox;
    ScheduleSaturdayCBox: TTntCheckBox;
    ScheduleSundayCBox: TTntCheckBox;
    MonthlyTabSheet: TTabSheet;
    Label22: TTntLabel;
    Label20: TTntLabel;
    Label25: TTntLabel;
    ScheduleDayOfMonthCBox: TTntComboBox;
    TimePnl: TTntPanel;
    Label23: TTntLabel;
    Label24: TTntLabel;
    ScheduleTimeEd: TTntEdit;
    ScheduleTypeCbox: TTntComboBox;
    Panel10: TTntPanel;
    AdvOptionsScrollBox: TTntScrollBox;
    BackExecuteGBox: TTntGroupBox;
    Label17: TTntLabel;
    Label18: TTntLabel;
    Label19: TTntLabel;
    Label14: TTntLabel;
    LockAllTablesRButton: TTntRadioButton;
    SingleTransRButton: TTntRadioButton;
    NormalBackupRadioButton: TTntRadioButton;
    CompleteSchematasCBox: TTntCheckBox;
    OutputFileGBox: TTntGroupBox;
    Label12: TTntLabel;
    Label13: TTntLabel;
    NoCreatesCBox: TTntCheckBox;
    NoExtendedInsertsCBox: TTntCheckBox;
    AddDropTableCBox: TTntCheckBox;
    CompleteInsertsCBox: TTntCheckBox;
    CommentCBox: TTntCheckBox;
    FullPathCBox: TTntCheckBox;
    AnsiQuotesCBox: TTntCheckBox;
    DisableKeysCBox: TTntCheckBox;
    BackupFileTypesLU: TTntComboBox;
    CompatibilityCheckbox: TCheckBox;
    PointInTimeRBtn: TTntRadioButton;
    PointInTimeLbl: TTntLabel;
    OptimizedCommitCBox: TTntCheckBox;
    procedure BackupTreeViewCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure FillSchemaTree(searchStr: WideString = '');

    procedure AddSchemaToBackupBtnClick(Sender: TObject);
    procedure AddSchemaToBackup(Schema: TMYX_SCHEMA; SelectIt: Boolean = True);
    procedure BackupTreeViewPopupMenuPopup(Sender: TObject);
    procedure RemoveBackupNodeMIClick(Sender: TObject);
    procedure SchemataFrameSchemaTreeViewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartBackupBtnClick(Sender: TObject);

    procedure NewProjectBtnClick(Sender: TObject);
    procedure BackupTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ClearControls(EnableControls: Boolean);
    procedure ProjectNameEdExit(Sender: TObject);
    procedure ScheduleBrowseTargetDirBtnClick(Sender: TObject);
    procedure DoChange(Sender: TObject);
    procedure DiscardBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);

    procedure RefreshBackupProfiles(SelectProfile: WideString = '');

    procedure ApplyChanges;
    procedure DiscardChanges(AskBeforeDiscard: Boolean);

    procedure LoadProfile(filename: WideString);
    function GetSelectedTablesCount: Integer;

    function GetOptions: Integer;
    procedure AddOption(var options: Integer;
      option: MYX_BACKUP_OPTIONS; Add: Boolean);
    function InOptions(options: Integer; option: MYX_BACKUP_OPTIONS): Boolean;
    procedure RefreshProfilesMIClick(Sender: TObject);
    procedure DeleteProfileMIClick(Sender: TObject);
    procedure ProfilesPopupMenuPopup(Sender: TObject);
    procedure ClearCompleteContentMIClick(Sender: TObject);
    procedure ProjectListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure SchemaListChanged(var Message: TMessage); message WM_SchemaListChanged;
    procedure AdvancedEditFrameSearchEdChange(Sender: TObject);

    procedure DisableEnableScheduleControls;
    procedure ScheduleCBoxClick(Sender: TObject);

    procedure ScheduleTypeCboxCloseUp(Sender: TObject);
    procedure SchemataFrameCatalogVSTMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SchemataFrameCatalogVSTDblClick(Sender: TObject);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure BackupTreeViewDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
      Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure BackupTreeViewDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure BackupTreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure BackupTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure BackupTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure BackupTreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure BackupTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    MousePos: TPoint;

    BackupPNGImg: TPNGObject;

    BackupProfileDir: WideString;

    BackupThread: TBackupThread;

    OriginalProjectName: WideString;
    FTaskScheduler: ITaskScheduler;
  protected
    function StartScheduler: DWORD;
    procedure WarnAboutFQI;
  public
    CurrentCatalog: TMYX_CATALOG;
    CurrentSchema: TMYX_SCHEMA;

    // User must not expand SchemaTree manually
    LockSchemaTreeExpansion: Boolean;
  end;

  TBackupThread = class(TThread)
  protected
    procedure Execute; override;
  private
    FMySQL: Pointer;

    FBackupForm: TAdminBackupForm;
    FFilename: WideString;

    FCurrentTablename: WideString;
    FTableCount: Integer;
    FTablesProcessed: Integer;
    FRowCount: Integer;
    FRowsProcessed: Integer;

    FBackupError: MYX_BACKUP_ERROR;
    FProgressForm: TAdminBackupProgressForm;
  public
    constructor Create(AdminBackupForm: TAdminBackupForm; AdminBackupProgressForm: TAdminBackupProgressForm;
      filename: WideString);
    destructor Destroy; override;

    procedure UpdateProgressForm;
    procedure FinishedBackup;
    procedure FreeAdminBackupProgressForm;
    procedure ShowError;
    procedure CleanUp;
  end;

function BackupProgress(current_table_name: PChar; num_tables: Integer; num_tables_processed: Integer;
  num_rows: Integer; num_rows_processed: Integer; user_data: Pointer): Integer; cdecl;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  {$ifndef COMPILER_9_UP}
    WinError,
  {$endif COMPILER_9_UP}
  Main, DateUtils, WinSvc, Password, PNGTools;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.FormCreate(Sender: TObject);

var
  Result: DWORD;
  
begin
  InitForm(self);

  // Initialize scheduler interfaces.
  Result := CoCreateInstance(CLSID_CSchedulingAgent, nil, CLSCTX_INPROC_SERVER, IID_ITaskScheduler, FTaskScheduler);
  if not Succeeded(Result) then
    raise EInOutError.Create(_('Could not initialize task scheduler interface.'));

  DockedPanel := ServerBackupRestorePnl;
  SubTreePanel := SubTreePnl;

  BackupRestorePageControl.ActivePageIndex := 0;

  CurrentCatalog := nil;
  CurrentSchema := nil;

  BackupThread := nil;

  InitControls := True;

  LockSchemaTreeExpansion := True;

  SchemataFrame.MySQLConnection := MySQLConn;
  SchemataFrame.ShowSchemaAssets := False;
  SchemataFrame.ShowAssetsOnSchemaExpansion := True;
  SchemataFrame.FillSchemaTree;

  BackupPNGImg := LoadPNGImageFromResource('backup', BackupImg);
  Backup2Img.Picture.Assign(BackupPNGImg);
  Backup3Img.Picture.Assign(BackupPNGImg);

  if (MYXCommonOptions.XPStyleEnabled) then
  begin
    ContentSplitter.Color := clWhite;
    ExecutionTimeGBox.Color := clWhite;
    AdvOptionsScrollBox.Color := clWhite;
  end;

  ClearControls(False);

  if (not (MySQLConn.Connected)) then
    NewProjectBtn.Enabled := False;

  BackupProfileDir := MYXCommonOptions.UserDataDir;

  RefreshBackupProfiles;

  ScheduleTypeCbox.ItemIndex := 1;
  SchedulePageControl.ActivePageIndex := 1;
  ScheduleDayOfMonthCBox.ItemIndex := 0;

  BackupTreeView.NodeDataSize := SizeOf(TBackupNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.FormDestroy(Sender: TObject);

begin
  BackupPNGImg.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  ClearListView(ProjectListView, myx_ndt_pointer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.FillSchemaTree(searchStr: WideString = '');

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.AddSchemaToBackup(Schema: TMYX_SCHEMA; SelectIt: Boolean);

  //----------------------------------------------------------------------------

  function IsNodeForObject(Node: TBackupNode; AType: TBackupObjType; Name: string): Boolean;

  // Checks if the given backup node represents the given object.
  
  begin
    Result := Assigned(Node) and (Node.ObjectType = AType) and
      SameText(Node.ObjectName, Name);
  end;

  //----------------------------------------------------------------------------

var
  Run: PVirtualNode;
  SchemaNode: PVirtualNode;
  I: Integer;
  RawEntityStatus: PMYX_SCHEMA_ENTITY_STATUS;
  EntityStatus: TMYX_SCHEMA_ENTITY_STATUS;

  RawTableStatus: PMYX_TABLE_STATUS;
  TableStatus: TMYX_TABLE_STATUS;
  RawViewStatus: PMYX_VIEW_STATUS;
  ViewStatus: TMYX_VIEW_STATUS;
  RawPSPStatus: PMYX_SCHEMA_STORED_PROCEDURE;
  PSPStatus: TMYX_SCHEMA_STORED_PROCEDURE;

  NodeData: PBackupNodeData;
  BackupNode: TBackupNode;
  Finished: Boolean;

begin
  if Schema.schema_name = 'information_schema' then
  begin
    ShowModalDialog(_('Backup of information_schema'),
      _('Backing up the information_schema database is not permitted as it is likely to make your server unusable on ' +
        'later restoration.'), myx_mtError, _('OK'));
  end
  else
  begin
    Finished := False;

    // Check if schema not already in backup list
    with BackupTreeView do
    begin
      Run := GetFirst;
      while Assigned(Run) and not Finished do
      begin
        NodeData := GetNodeData(Run);
        if IsNodeForObject(NodeData.BackupNode, BOTSchema, Schema.schema_name) then
        begin
          // If Node has a parent, check catalog name, too.
          if Assigned(NodeParent[Run]) then
          begin
            NodeData := GetNodeData(NodeParent[Run]);
            if IsNodeForObject(NodeData.BackupNode, BOTCatalog, Schema.catalog_name) then
              Finished := True;
          end
          else
            Finished := True;
        end;
        Run := BackupTreeView.GetNext(Run);
      end;

      if not Finished then
      begin
        BeginUpdate;

        // Add a new schema node to the catalog.
        SchemaNode := AddChild(nil);
        NodeData := GetNodeData(SchemaNode);
        NodeData.BackupNode := TBackupNode.Create(Schema.schema_name, BOTSchema, 0);
        NodeData.ImageIndex := 9;
        NodeData.Caption := Schema.schema_name;

        // Retrieve all tables in the schema.
        RawEntityStatus := myx_get_schema_entity_status(MySQLConn.MySQL, '', Schema.schema_name);
        EntityStatus := TMYX_SCHEMA_ENTITY_STATUS.Create(RawEntityStatus);

        for I := 0 to EntityStatus.schema_entities.Count - 1 do
        begin
          case EntityStatus.schema_entities[I].entity_type of
            MYX_ENTITY_TABLE:
              begin
                RawTableStatus := EntityStatus.schema_entities[I].entity;
                TableStatus := TMYX_TABLE_STATUS.Create(RawTableStatus);
                try
                  BackupNode := TBackupNode.Create(TableStatus.table_name, botTable, 0);
                  BackupNode[0] := TableStatus.table_type;
                  BackupNode[1] := TableStatus.rows;
                  BackupNode[2] := TableStatus.data_length;
                  BackupNode[3] := TableStatus.update_time;
                  Run := AddChild(SchemaNode);
                  NodeData := GetNodeData(Run);
                  NodeData.BackupNode := BackupNode;
                  NodeData.ImageIndex := 10;
                  NodeData.Caption:= TableStatus.table_name;
                finally
                  TableStatus.Free;
                end;
              end;
            MYX_ENTITY_VIEW:
              begin
                RawViewStatus := EntityStatus.schema_entities[I].entity;
                ViewStatus := TMYX_VIEW_STATUS.Create(RawViewStatus);
                try
                  BackupNode := TBackupNode.Create(ViewStatus.view_name, botTable, 0);
                  BackupNode[0] := 'VIEW';
                  Run := AddChild(SchemaNode);
                  NodeData := GetNodeData(Run);
                  NodeData.BackupNode := BackupNode;
                  NodeData.ImageIndex := 27;
                  NodeData.Caption:= ViewStatus.view_name;
                finally
                  ViewStatus.Free;
                end;
              end;
            MYX_ENTITY_PROC:
              begin
                RawPSPStatus := EntityStatus.schema_entities[I].entity;
                PSPStatus := TMYX_SCHEMA_STORED_PROCEDURE.Create(RawPSPStatus);
                try
                  BackupNode := TBackupNode.Create(PSPStatus.name, botTable, 0);
                  BackupNode[0] := 'PROCEDURE';
                  Run := AddChild(SchemaNode);
                  NodeData := GetNodeData(Run);
                  NodeData.BackupNode := BackupNode;
                  NodeData.ImageIndex := 13;
                  NodeData.Caption:= PSPStatus.name;
                finally
                  PSPStatus.Free;
                end;
              end;
            MYX_ENTITY_FUNC:
              begin
                RawPSPStatus := EntityStatus.schema_entities[I].entity;
                PSPStatus := TMYX_SCHEMA_STORED_PROCEDURE.Create(RawPSPStatus);
                try
                  BackupNode := TBackupNode.Create(PSPStatus.name, botTable, 0);
                  BackupNode[0] := 'FUNCTION';
                  Run := AddChild(SchemaNode);
                  NodeData := GetNodeData(Run);
                  NodeData.BackupNode := BackupNode;
                  NodeData.ImageIndex := 13;
                  NodeData.Caption:= PSPStatus.name;
                finally
                  PSPStatus.Free;
                end;
              end;
          end;
        end;

        // Select new Schema and all subnodes (tables).
        if SelectIt then
          SetNodeSelectState(BackupTreeView, SchemaNode, bctAll);

        EntityStatus.Free;
        EndUpdate;

        // Expand the schema, if this is the first one.
        if RootNodeCount = 1 then
          Expanded[GetFirst] := True;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

begin
  if Source = SchemataFrame.CatalogVST then
    Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PBackupNodeData;

begin
  NodeData := Sender.GetNodeData(Node);
  NodeData.BackupNode.Free;
  Finalize(NodeData^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  NodeData: PBackupNodeData;

begin
  if Column = 0 then
  begin
    NodeData := Sender.GetNodeData(Node);
    case Kind of
      ikNormal,
      ikSelected:
        ImageIndex := NodeData.ImageIndex;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PBackupNodeData;

begin
  NodeData := Sender.GetNodeData(Node);
  case Column of
    -1, 0:
      CellText := NodeData.Caption;
  else
    CellText := NodeData.BackupNode[Column - 1];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

begin
  Sender.CheckType[Node] := ctTriStateCheckBox;

  if Sender.GetNodeLevel(Node) < 2 then
    Include(InitialStates, ivsHasChildren);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

begin
  AddSchemaToBackupBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewPopupMenuPopup(Sender: TObject);

var
  Data: PBackupNodeData;

begin
  // Tables cannot be removed
  RemoveBackupNodeMI.Enabled := True;
  with BackupTreeview do
  begin
    if Assigned(FocusedNode) then
    begin
      Data := GetNodeData(FocusedNode);
      if Data.BackupNode.ObjectType = botTable then
        RemoveBackupNodeMI.Enabled := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.RemoveBackupNodeMIClick(Sender: TObject);

begin
  with BackupTreeView do
  begin
    if Assigned(FocusedNode) then
    begin
      DeleteNode(FocusedNode);
      DoChange(self);
      StartBackupBtn.Enabled := (GetSelectedTablesCount > 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.AddSchemaToBackupBtnClick(Sender: TObject);

var
  I, J: Integer;
  Selection: TNodeArray;
  NodeData: ^TObject;

begin
  Selection := SchemataFrame.CatalogVST.GetSortedSelection(False);

  for I := 0 to SchemataFrame.CatalogVST.SelectedCount - 1 do
  begin
    NodeData := SchemataFrame.CatalogVST.GetNodeData(Selection[I]);
    if (NodeData <> nil) then
      if (NodeData^ <> nil) then
      begin
        if (NodeData^ is TMYX_SCHEMA) then
          AddSchemaToBackup(TMYX_SCHEMA(NodeData^));

        if (NodeData^ is TMYX_CATALOG) then
          for J := 0 to TMYX_CATALOG(NodeData^).Schemata.Count - 1 do
            AddSchemaToBackup(TMYX_CATALOG(NodeData^).Schemata[J]);
      end;
  end;

  DoChange(self);

  StartBackupBtn.Enabled := (GetSelectedTablesCount > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.SchemataFrameSchemaTreeViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
  
begin
  if (Button = mbLeft) then
  begin
    MousePos := Mouse.CursorPos;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.StartBackupBtnClick(Sender: TObject);

var
  SaveDialog: TTntSaveDialog;
  filename: WideString;
  AdminBackupProgressForm: TAdminBackupProgressForm;

begin
  SaveDialog := TTntSaveDialog.Create(self);
  try
    if (ApplicationDM.Options.AddDateTimeToBackupFiles) then
      SaveDialog.FileName := ProjectNameEd.Text +
        FormatDateTime(' yyyymmdd hhnn', Now) + '.sql'
    else
      SaveDialog.FileName := ProjectNameEd.Text + '.sql';

    SaveDialog.Filter := 'SQL Backup Files|*.sql|All Files|*.*';

    if (not (SaveDialog.Execute)) then
      Exit;

    filename := SaveDialog.Filename;
  finally
    SaveDialog.Free;
  end;

  AdminBackupProgressForm := TAdminBackupProgressForm.Create(Self, True, ProjectNameEd.Text);
  AdminBackupProgressForm.Show;

  BackupThread := TBackupThread.Create(self, AdminBackupProgressForm, filename);
  try
    // Thread will be freed after execution
    BackupThread.FreeOnTerminate := True;

    BackupThread.Resume;
  except
    BackupThread.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);

begin
  if GetSelectedTablesCount < Integer(BackupTreeView.TotalCount) then
    CompleteSchematasCBox.Checked := false;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

const
  CheckStateToBackupCheckType: array[TCheckState] of TBackupCheckType = (
    bctNone, bctNone, bctAll, bctAll, bctSome, bctSome);
  
begin
  if Sender.UpdateCount = 0 then
  begin
    SetNodeSelectState(BackupTreeView, Node, CheckStateToBackupCheckType[Node.CheckState]);
    DoChange(Self);
    StartBackupBtn.Enabled := (GetSelectedTablesCount > 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ClearControls(EnableControls: Boolean);

begin
  InitControls := True;

  DisableEnableControls(BackupContentTabSheet, EnableControls);
  DisableEnableControls(AdvancedOptionsTabSheet, EnableControls);
  DisableEnableControls(ScheduleTabSheet, EnableControls);
  DisableEnableScheduleControls;

  ApplyBtn.Enabled := False;
  StartBackupBtn.Enabled := False;

  ProjectNameEd.Text := '';
  ScheduleTargetDirEd.Text := '';
  ScheduleFilenamePrefixEd.Text := '';

  NoCreatesCBox.Checked := False;
  NoExtendedInsertsCBox.Checked := False;
  AddDropTableCBox.Checked := True;
  CompleteInsertsCBox.Checked := True;
  CommentCBox.Checked := True;
  FullPathCBox.Checked := False;
  AnsiQuotesCBox.Checked := False;
  OptimizedCommitCBox.Checked := False;

  SingleTransRButton.Checked := True;

  DisableKeysCBox.Checked := True;
  CompleteSchematasCBox.Checked := False;
  CompatibilityCheckbox.Checked := False;

  BackupFileTypesLU.ItemIndex := 0;

  BackupTreeView.Clear;

  ScheduleCBox.Checked := False;
  ScheduleTypeCbox.ItemIndex := 1;
  SchedulePageControl.ActivePageIndex := 1;

  ScheduleMondayCBox.Checked := False;
  ScheduleTuesdayCBox.Checked := False;
  ScheduleWednesdayCBox.Checked := False;
  ScheduleThursdayCBox.Checked := False;
  ScheduleFridayCBox.Checked := False;
  ScheduleSaturdayCBox.Checked := False;
  ScheduleSundayCBox.Checked := False;

  ScheduleTimeEd.Text := TimeToStr(IncMinute(Now, 5));

  InitControls := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.DisableEnableScheduleControls;

begin
  ScheduleTargetDirCaptionLbl.Enabled := ScheduleCBox.Checked;
  ScheduleTargetDirEd.Enabled := ScheduleCBox.Checked;
  ScheduleBrowseTargetDirBtn.Enabled := ScheduleCBox.Checked;
  ScheduleTargetDirLbl.Enabled := ScheduleCBox.Checked;
  SchedulePrefixCaptionLbl.Enabled := ScheduleCBox.Checked;
  ScheduleFilenamePrefixEd.Enabled := ScheduleCBox.Checked;
  SchedulePrefixLbl.Enabled := ScheduleCBox.Checked;

  DisableEnableControls(ExecutionTimeGBox, ScheduleCBox.Checked);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.NewProjectBtnClick(Sender: TObject);

begin
  DiscardChanges(True);

  OriginalProjectName := '';

  BackupRestorePageControl.ActivePageIndex := 0;

  ProjectListView.Selected := nil;
  ClearControls(True);

  ProjectNameEd.Text := 'New Project';
  ProjectNameEd.SetFocus;
  ProjectNameEd.SelectAll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ProjectNameEdExit(Sender: TObject);

begin
  if (ExtractText(ProjectNameEd.Text) <> ProjectNameEd.Text) then
  begin
    ProjectNameEd.SetFocus;
    raise EInOutError.Create(_('You can only use alphanumeric characters for project name.'));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ScheduleBrowseTargetDirBtnClick(Sender: TObject);

var
  SaveDialog: TTntSaveDialog;

begin
  SaveDialog := TTntSaveDialog.Create(self);
  SaveDialog.Title := _('Choose Target Directory');
  SaveDialog.InitialDir := '';
  SaveDialog.FileName := _('Browse to the Target Directory');

  if (SaveDialog.Execute) then
  begin
    ScheduleTargetDirEd.Text := ExtractFilePath(SaveDialog.FileName);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.DoChange(Sender: TObject);

var
  Node: PVirtualNode;
  NodeData: PBackupNodeData;

begin
  if not InitControls then
  begin
    if Sender = FullPathCBox then
      WarnAboutFQI;

    ApplyBtn.Enabled := True;
    if Sender = CompleteSchematasCBox then
    begin
      if CompleteSchematasCBox.Checked then
        with BackupTreeView do
        begin
          Node := GetFirst;
          while Assigned(Node) do
          begin
            NodeData := GetNodeData(Node);
            NodeData.BackupNode.SelectedState := bctAll;
            Node := GetNext(Node);
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.DiscardChanges(AskBeforeDiscard: Boolean);

var
  res: Integer;

begin
  if (not (ApplyBtn.Enabled)) then
  begin
    OriginalProjectName := '';

    Exit;
  end;

  if (AskBeforeDiscard) then
  begin
    res := ShowModalDialog(_('Apply changes?'),
      _('The current backup profile has been changed. ' +
      'Do you want to apply your changes?'),
      myx_mtConfirmation,
      _('Yes') + #13#10 + _('No') + #13#10 + _('Abort'));
    if (res = 1) then
    begin
      ApplyChanges;
      Exit;
    end
    else
      if (res = 3) then
        Abort;
  end;

  if (ProjectListView.Selected = nil) then
    ClearControls(False)
  else
  begin
    ApplyBtn.Enabled := False;

    // Reload profile
    LoadProfile(UTF8Decode(PChar(ProjectListView.Selected.Data)));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminBackupForm.GetOptions: Integer;

var
  options: Integer;

begin
  options := 0;

  AddOption(options, MYX_B_NO_CREATES, NoCreatesCBox.Checked);
  AddOption(options, MYX_B_NO_EXTENDED_INSERT, NoExtendedInsertsCBox.Checked);
  AddOption(options, MYX_B_ADD_DROP_TABLE, AddDropTableCBox.Checked);
  AddOption(options, MYX_B_COMPLETE_INSERTS, CompleteInsertsCBox.Checked);
  AddOption(options, MYX_B_COMMENT, CommentCBox.Checked);
  AddOption(options, MYX_B_DONT_WRITE_FULL_PATH, not FullPathCBox.Checked);
  AddOption(options, MYX_B_ANSI_QUOTES, AnsiQuotesCBox.Checked);
  AddOption(options, MYX_B_LOCK_ALL_TABLES, LockAllTablesRButton.Checked);
  AddOption(options, MYX_B_SINGLE_TRANSACTION, SingleTransRButton.Checked);
  AddOption(options, MYX_B_DISABLE_KEYS, DisableKeysCBox.Checked);
  AddOption(options, MYX_B_COMPLETE_SCHEMATAS, CompleteSchematasCBox.Checked);
  AddOption(options, MYX_B_COMPATIBILITY_MODE, CompatibilityCheckbox.Checked);
  AddOption(options, MYX_B_POINT_IN_TIME_BACKUP, PointInTimeRBtn.Checked);
  AddOption(options, MYX_B_OPTIMIZED_COMMIT, OptimizedCommitCBox.Checked);

  Result := options;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ApplyChanges;

var
  Profile: TMYX_BACKUP_PROFILE;
  Content: TMYX_BACKUP_CONTENT;
  BackupType: MYX_BACKUP_TYPE;
  filename: WideString;

  S: WideString;
  Index: Word;
  Unknown: IUnknown;
  Task: ITask;
  PersistFile: IPersistFile;
  Trigger: ITaskTrigger;
  TriggerData: TASK_TRIGGER;
  Result: HResult;

  Year,
  Month,
  Day,
  Second,
  MilliSecond: Word;
  DaysFlag: Word;
  DeleteJob: Boolean;
  
begin
  if ScheduleCBox.Checked then
  begin
    if Trim(ScheduleTargetDirEd.Text) = '' then
      raise EInOutError.Create(_('You have to specify a target directory for the backup.'));

    if Trim(ScheduleFilenamePrefixEd.Text) = '' then
      raise EInOutError.Create(_('You have to specify a filename prefix for the backup.'));

    if GetSelectedTablesCount = 0 then
      raise EInOutError.Create(_('You have to specify some content for the backup.'));
  end;

  // Get Backup FileType.
  case BackupFileTypesLU.ItemIndex of
    0:
      BackupType := MYX_BT_SQL_SCRIPT;
  else
    BackupType := MYX_BT_SQL_SCRIPT;
  end;

  Content := GetBackupContent(BackupTreeView);
  try
    Profile := TMYX_BACKUP_PROFILE.create(ProjectNameEd.Text, '', GetOptions, BackupType, Content.get_record_pointer);
    try
      filename := BackupProfileDir + ExtractText(ProjectNameEd.Text) + '.mbp';

      // Detect project name change.
      if (OriginalProjectName <> ProjectNameEd.Text) then
        if (FileExists(BackupProfileDir + OriginalProjectName + '.mbp')) then
        begin
          DeleteFile(filename);
          DeleteFile(ChangeFileExt(filename, '.bak'));
          RenameFile(BackupProfileDir + OriginalProjectName + '.mbp', ChangeFileExt(filename, '.bak'));
        end;

      if (FileExists(filename)) then
      begin
        DeleteFile(ChangeFileExt(filename, '.bak'));
        RenameFile(filename, ChangeFileExt(filename, '.bak'));
      end;

      myx_save_profile(ExtractFileName(filename), ExtractFilePath(filename), Profile.get_record_pointer);
      OriginalProjectName := Profile.profile_name;
    finally
      Profile.Free;
    end;

  finally
    Content.Free;
  end;

  DeleteJob := not ScheduleCBox.Checked;
  
  // If the user scheduled a project.
  if not DeleteJob then
  begin
    // Make sure the service is running...
    StartScheduler;

    // Reuse existing task entry if there is one.
    Result := FTaskScheduler.Activate(PWideChar(OriginalProjectName), IID_ITask, Unknown);
    if Result = S_OK then
      Task := Unknown as ITask
    else
    begin
      Result := FTaskScheduler.NewWorkItem(PWideChar(OriginalProjectName), CLSID_CTask, IID_IScheduledWorkItem, Unknown);
      if Result = S_OK then
        Task := Unknown as ITask
      else
        raise EMyxSystemError.Create(_('Cannot create new backup task.'), Result);
    end;

    if TPasswordDialog.DetermineUserCredentials(Task) then
    begin
      S := Application.ExeName;
      Task.SetApplicationName(PWideChar(S));
      Task.SetFlags(0);

      S := '"-UD' + MYXCommonOptions.UserDataDir + '" ' +
           '"-c' + MySQLConn.UserConnection.connection_name + '" ' +
           '"-bp' + ProjectNameEd.Text + '" ' +
           '"-bt' + Trim(ScheduleTargetDirEd.Text) + '" ' +
           '"-bx' + Trim(ScheduleFilenamePrefixEd.Text) + '"';
      Task.SetParameters(PWideChar(S));
      Task.SetWorkingDirectory(PWideChar(MYXCommonOptions.UserDataDir));
      Task.SetPriority(NORMAL_PRIORITY_CLASS);
      Task.SetCreator('MySQL Administrator');
      Task.SetMaxRunTime(INFINITE);

      // Task is ready now. Add a trigger when to run it.
      // Delete existing triggers before adding the new one.
      // TODO: Allow working with more than one trigger.
      Task.GetTriggerCount(Index);
      while Index > 0 do
      begin
        Task.DeleteTrigger(0);
        Dec(Index);
      end;
      Result := Task.CreateTrigger(Index, Trigger);
      if Result <> S_OK then
        raise EMyxSystemError.Create(_('Cannot create new backup task.'), Result);

      FillChar(TriggerData, SizeOf(TriggerData), 0);
      TriggerData.cbTriggerSize := SizeOf(TriggerData);

      // Start day, month and year is today.
      DecodeDate(Now, TriggerData.wBeginYear, TriggerData.wBeginMonth, TriggerData.wBeginDay);
      // Execution time must be splittet as well.
      DecodeDateTime(StrToDateTime(ScheduleTimeEd.Text), Year, Month, Day, TriggerData.wStartHour, TriggerData.wStartMinute,
        Second, Millisecond);

      case ScheduleTypeCbox.ItemIndex of
        0: // Daily.
          begin
            TriggerData.TriggerType := TASK_TIME_TRIGGER_DAILY;
            TriggerData.Type_.Daily.DaysInterval := 1;
          end;
        1: // Weekly at certain days.
          begin
            TriggerData.TriggerType := TASK_TIME_TRIGGER_WEEKLY;
            TriggerData.Type_.Weekly.WeeksInterval := 1;
            DaysFlag := 0;
            if ScheduleMondayCBox.Checked then
              DaysFlag := DaysFlag or TASK_MONDAY;
            if ScheduleTuesdayCBox.Checked then
              DaysFlag := DaysFlag or TASK_TUESDAY;
            if ScheduleWednesdayCBox.Checked then
              DaysFlag := DaysFlag or TASK_WEDNESDAY;
            if ScheduleThursdayCBox.Checked then
              DaysFlag := DaysFlag or TASK_THURSDAY;
            if ScheduleFridayCBox.Checked then
              DaysFlag := DaysFlag or TASK_FRIDAY;
            if ScheduleSaturdayCBox.Checked then
              DaysFlag := DaysFlag or TASK_SATURDAY;
            if ScheduleSundayCBox.Checked then
              DaysFlag := DaysFlag or TASK_SUNDAY;
            TriggerData.Type_.Weekly.rgfDaysOfTheWeek := DaysFlag;
          end;
        2: // Monthly.
          begin
            TriggerData.TriggerType := TASK_TIME_TRIGGER_MONTHLYDATE;

            // Create bitmask for day of month (zero-based) and add all months for execution.
            TriggerData.Type_.MonthlyDate.rgfDays := 1 shl ScheduleDayOfMonthCBox.ItemIndex;
            TriggerData.Type_.MonthlyDate.rgfMonths := TASK_JANUARY or TASK_FEBRUARY or TASK_MARCH or TASK_APRIL or
              TASK_MAY or TASK_JUNE or TASK_JULY or TASK_AUGUST or TASK_SEPTEMBER or TASK_OCTOBER or TASK_NOVEMBER or TASK_DECEMBER;
          end;
      end;

      Trigger.SetTrigger(TriggerData);

      // Now that the task is setup save it to make it active.
      PersistFile := Task as IPersistFile;
      PersistFile.Save(nil, True);
    end
    else
      DeleteJob := True;
  end;

  if DeleteJob then
  begin
    FTaskScheduler.Delete(PWideChar(OriginalProjectName));
    ScheduleCBox.Checked := False;
    DisableEnableScheduleControls;
  end;

  ApplyBtn.Enabled := False;
  RefreshBackupProfiles(filename);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.DiscardBtnClick(Sender: TObject);

begin
  DiscardChanges(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ApplyBtnClick(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

function StrMatchesSearchStr(Str: WideString; searchStr: WideString): Boolean;

begin
  Result := True;

  if (searchStr <> '') then
    Result := myx_match_pattern(Str, searchStr, 0, 1) <> 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.RefreshBackupProfiles(SelectProfile: WideString);

var
  CurrentProfileName: WideString;
  SR: TSearchRec;
  filename: PChar;
  i: Integer;

begin
  CurrentProfileName := SelectProfile;

  InitControls := True;
  ProjectListView.Items.BeginUpdate;
  try
    ClearListView(ProjectListView, myx_ndt_pointer);

    if (FindFirst(BackupProfileDir + '*.mbp', faAnyFile and not
      (8 {faVolumeID} or faDirectory), sr) = 0) then
    begin
      repeat
        begin
          if (AdvancedEditFrame.SearchEd.Text <> '') then
            if (not (StrMatchesSearchStr(sr.Name, AdvancedEditFrame.SearchEd.Text))) then
              continue;

          GetMem(filename, Length(UTF8Encode(BackupProfileDir + sr.Name)) + 1);
          StrPCopy(filename, UTF8Encode(BackupProfileDir + sr.Name));

          AddListViewItem(ProjectListView, nil,
            ExtractFileName(Copy(sr.Name, 1, Length(sr.Name) - Length(ExtractFileExt(sr.Name)))),
            -1, filename);
        end;
      until FindNext(sr) <> 0;

      FindClose(sr);
    end;

    for i := 0 to ProjectListView.Items.Count - 1 do
      if (ProjectListView.Items[i].Data <> nil) then
        if (CompareText(CurrentProfileName,
          UTF8Decode(PChar(ProjectListView.Items[i].Data))) = 0) then
        begin
          ProjectListView.Selected := ProjectListView.Items[i];
          break;
        end;
  finally
    ProjectListView.Items.EndUpdate;
    InitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminBackupForm.InOptions(options: Integer; option: MYX_BACKUP_OPTIONS): Boolean;

begin
  Result := ((options and ord(option)) = Ord(option));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.AddOption(var Options: Integer; option: MYX_BACKUP_OPTIONS; Add: Boolean);

begin
  if Add then
    Options := Options + Ord(option);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.LoadProfile(filename: WideString);

  //----------------------------------------------------------------------------

  function ExtractParameter(S: WideString; Start: Integer): WideString;

  // Extracts a parameter from the given string by scanning for the closing
  // quotation mark starting at Start. Search stops also on string end.
  // Note: S must at least contain 1 character.

  var
    Head, Tail: PWideChar;

  begin
    Head := @S[Start];
    // Scan for closing quotation mark.
    Tail := Head + 1;
    while not (Tail^ in [WideChar('"'), WideChar(#0)]) do
      Inc(Tail);
    SetString(Result, Head, Tail - Head);
  end;

  //----------------------------------------------------------------------------

var
  Profile: TMYX_BACKUP_PROFILE;
  PProfile: PMYX_BACKUP_PROFILE;
  error: MYX_ADMIN_LIB_ERROR;

  I: Integer;
  Raw: PWideChar;
  Index: Integer;
  Command: WideString;
  Result: HResult;
  Task: ITask;
  Unknown: IUnknown;
  Trigger: ITaskTrigger;
  TriggerData: TASK_TRIGGER;
  DaysFlag: Word;
  Mask: DWORD;

begin
  if MySQLConn.Connected then
  begin
    PProfile := myx_load_profile(ExtractFileName(filename), ExtractFilePath(filename), @error);
    if (error <> MYX_ADMIN_NO_ERROR) or (PProfile = nil) then
      raise EMyxAdminLibError.Create(_('Error while loading backup profile.'), Ord(error), filename);

    InitControls := True;
    try
      Profile := TMYX_BACKUP_PROFILE.create(PProfile);
    finally
      myx_free_profile(PProfile);
    end;

    try
      //Store original project name
      OriginalProjectName := Profile.profile_name;

      ProjectNameEd.Text := Profile.profile_name;

      BackupFileTypesLU.ItemIndex := Ord(Profile.backup_type);

      NoCreatesCBox.Checked := InOptions(Profile.options, MYX_B_NO_CREATES);
      NoExtendedInsertsCBox.Checked := InOptions(Profile.options, MYX_B_NO_EXTENDED_INSERT);
      AddDropTableCBox.Checked := InOptions(Profile.options, MYX_B_ADD_DROP_TABLE);
      CompleteInsertsCBox.Checked := InOptions(Profile.options, MYX_B_COMPLETE_INSERTS);
      CommentCBox.Checked := InOptions(Profile.options, MYX_B_COMMENT);
      FullPathCBox.Checked := not InOptions(Profile.options, MYX_B_DONT_WRITE_FULL_PATH);
      WarnAboutFQI;
      
      AnsiQuotesCBox.Checked := InOptions(Profile.options, MYX_B_ANSI_QUOTES);
      NormalBackupRadioButton.Checked := True;
      LockAllTablesRButton.Checked := InOptions(Profile.options, MYX_B_LOCK_ALL_TABLES);
      SingleTransRButton.Checked := InOptions(Profile.options, MYX_B_SINGLE_TRANSACTION);
      DisableKeysCBox.Checked := InOptions(Profile.options, MYX_B_DISABLE_KEYS);
      CompleteSchematasCBox.Checked := InOptions(Profile.options, MYX_B_COMPLETE_SCHEMATAS);
      CompatibilityCheckbox.Checked := InOptions(Profile.options, MYX_B_COMPATIBILITY_MODE);
      PointInTimeRBtn.Checked := InOptions(Profile.options, MYX_B_POINT_IN_TIME_BACKUP);
      OptimizedCommitCBox.Checked := InOptions(Profile.options, MYX_B_OPTIMIZED_COMMIT);

      BuildContentTreeFromBackupContent(MySQLConn.MySQL,
        BackupTreeView,
        Profile.backup_content,
        SchemataFrame,
        CompleteSchematasCBox.Checked);

      // Check if it is scheduled
      Result := FTaskScheduler.Activate(PWideChar(ProjectNameEd.Text), IID_ITask, Unknown);
      if Result = S_OK then
      begin
        ScheduleCBox.Checked := True;

        Task := Unknown as ITask;

        // For now we ignore result values of the getters. If something fails then we will just
        // have strange or no values in the controls. This must be then fixed anyway.
        Task.GetParameters(Raw);
        Command := Raw;
        CoTaskMemFree(Raw);
        
        ScheduleTargetDirEd.Text := '';
        Index := Pos('"-bt', Command);
        if Index > 0 then
          ScheduleTargetDirEd.Text := ExtractParameter(Command, Index + 4);

        ScheduleFilenamePrefixEd.Text := '';
        Index := Pos('"-bx', Command);
        if Index > 0 then
          ScheduleFilenamePrefixEd.Text := ExtractParameter(Command, Index + 4);

        // TODO: Currently only one trigger can be handled here. Enhance this for any trigger count.
        Result := Task.GetTrigger(0, Trigger);
        if Result = S_OK then
        begin
          Trigger.GetTrigger(TriggerData);
          ScheduleTimeEd.Text := Format('%.2d:%.2d', [TriggerData.wStartHour, TriggerData.wStartMinute]);

          case TriggerData.TriggerType of
            TASK_TIME_TRIGGER_DAILY:
              begin
                ScheduleTypeCbox.ItemIndex := 0;
              end;
            TASK_TIME_TRIGGER_WEEKLY:
              begin
                ScheduleTypeCbox.ItemIndex := 1;

                DaysFlag := TriggerData.Type_.Weekly.rgfDaysOfTheWeek;
                ScheduleMondayCBox.Checked := (DaysFlag and TASK_MONDAY) <> 0;
                ScheduleTuesdayCBox.Checked := (DaysFlag and TASK_TUESDAY) <> 0;
                ScheduleWednesdayCBox.Checked := (DaysFlag and TASK_WEDNESDAY) <> 0;
                ScheduleThursdayCBox.Checked := (DaysFlag and TASK_THURSDAY) <> 0;
                ScheduleFridayCBox.Checked := (DaysFlag and TASK_FRIDAY) <> 0;
                ScheduleSaturdayCBox.Checked := (DaysFlag and TASK_SATURDAY) <> 0;
                ScheduleSundayCBox.Checked := (DaysFlag and TASK_SUNDAY) <> 0;
              end;
            TASK_TIME_TRIGGER_MONTHLYDATE:
              begin
                ScheduleTypeCbox.ItemIndex := 2;
                ScheduleDayOfMonthCBox.ItemIndex := 0;
                
                // Scan for the first set bit. We cannot handle more than one day currently.
                Mask := 1;
                for I := 0 to 30 do
                begin
                  if (TriggerData.Type_.MonthlyDate.rgfDays and Mask) <> 0 then
                  begin
                    ScheduleDayOfMonthCBox.ItemIndex := I;
                    Break;
                  end;
                  Mask := Mask shl 1;
                end;
              end;
          end;
        end;
        
        ScheduleTypeCboxCloseUp(self);
      end
      else
        ScheduleCBox.Checked := False;

      DisableEnableScheduleControls;
    finally
      InitControls := False;
      Profile.Free;
    end;

    StartBackupBtn.Enabled := (GetSelectedTablesCount > 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminBackupForm.GetSelectedTablesCount: Integer;

var
  Run: PVirtualNode;
  NodeData: PBackupNodeData;

begin
  Result := 0;

  with BackupTreeView do
  begin
    Run := GetFirst;
    while Assigned(Run) do
    begin
      NodeData := GetNodeData(Run);
      if (NodeData.BackupNode.ObjectType = botTable) and (NodeData.BackupNode.SelectedState = bctAll) then
        Inc(Result);
      Run := GetNext(Run);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.RefreshProfilesMIClick(Sender: TObject);

begin
  RefreshBackupProfiles;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.DeleteProfileMIClick(Sender: TObject);

var
  I: Integer;
  Filename: WideString;
  Profile: PMYX_BACKUP_PROFILE;
  error: MYX_ADMIN_LIB_ERROR;
  S: WideString;

begin
  if (ShowModalDialog(_('Delete Profiles?'), _('Are you sure you want to delete the selected profiles?'),
    myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1) then
  begin
    InitControls := True;
    try
      for I := 0 to ProjectListView.Items.Count - 1 do
        if ProjectListView.Items[i].Selected then
          if Assigned(ProjectListView.Items[I].Data) then
          begin
            Filename := UTF8Decode(PChar(ProjectListView.Items[I].Data));
            Profile := myx_load_profile(ExtractFileName(Filename), ExtractFilePath(Filename), @error);

            // TODO: Profile still uses ANSI strings.
            S := Profile.profile_name;
            FTaskScheduler.Delete(PWideChar(S));

            DeleteFile(filename);
          end;

      RefreshBackupProfiles;

      ProjectListView.Selected := nil;
      ClearControls(False);
    finally
      InitControls := False;
    end;
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ProfilesPopupMenuPopup(Sender: TObject);

begin
  DeleteProfileMI.Enabled := ProjectListView.SelCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ClearCompleteContentMIClick(Sender: TObject);

begin
  BackupTreeView.Clear;
  DoChange(self);
  StartBackupBtn.Enabled := (GetSelectedTablesCount > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ProjectListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

var
  filename: WideString;

begin
  if not InitControls and not (csDestroying in ComponentState) then
  begin
    if (ProjectListView.SelCount = 1) and MySQLConn.Connected then
    begin
      if Assigned(ProjectListView.Selected.Data) then
      begin
        filename := UTF8Decode(PChar(ProjectListView.Selected.Data));

        if ApplyBtn.Enabled then
          DiscardChanges(True);

        LoadProfile(filename);

        DisableEnableControls(BackupContentTabSheet, True);
        DisableEnableControls(AdvancedOptionsTabSheet, True);
        DisableEnableControls(ScheduleTabSheet, True);
        DisableEnableScheduleControls;
      end;
    end
    else
      ClearControls(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TBackupThread.Create(AdminBackupForm: TAdminBackupForm; AdminBackupProgressForm: TAdminBackupProgressForm;
  filename: WideString);

begin
  inherited Create(True);

  self.FBackupForm := AdminBackupForm;
  self.FProgressForm := AdminBackupProgressForm;
  self.FFilename := filename;

  FMySQL := myx_mysql_init();
  if (FMySQL = nil) then
    raise EMyxError.Create(_('Error while allocating memory for MySQL Struct in Backup Thread.'));

  if (myx_connect_to_instance(
    AdminBackupForm.MySQLConn.UserConnection.get_record_pointer,
    FMySQL) <> 0) then
    raise EMyxError.Create(_('Backup Thread cannot connect to MySQL'));
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBackupThread.Destroy;

begin
  CleanUp;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.CleanUp;

begin
  myx_mysql_close(FMySQL);

  FBackupForm.BackupThread := nil;

  Synchronize(FreeAdminBackupProgressForm);
end;

//----------------------------------------------------------------------------------------------------------------------

function BackupProgress(current_table_name: PChar; num_tables: Integer; num_tables_processed: Integer;
  num_rows: Integer; num_rows_processed: Integer; user_data: Pointer): Integer;

var
  PSender: TBackupThread;
  
begin
  PSender := user_data;

  with PSender.FProgressForm do
  begin
    PSender.FCurrentTablename := current_table_name;
    PSender.FTableCount := num_tables;
    PSender.FTablesProcessed := num_tables_processed;
    PSender.FRowCount := num_rows;
    PSender.FRowsProcessed := num_rows_processed;
  end;

  PSender.Synchronize(PSender.UpdateProgressForm);

  Result := Ord(PSender.FProgressForm.Stopping);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.Execute;

var
  backup_content: TMYX_BACKUP_CONTENT;

begin
  with FBackupForm do
  begin
    backup_content := GetBackupContent(BackupTreeView);
    try
      FBackupError := myx_make_backup(FMySQL, FFilename, backup_content.get_record_pointer, MYX_BT_SQL_SCRIPT,
        GetOptions, 256, BackupProgress, Self);

      if (FBackupError = MYX_BACKUP_NO_ERROR) then
        Synchronize(FinishedBackup)
      else
        Synchronize(ShowError);

    finally
      backup_content.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.UpdateProgressForm;

begin
  with FProgressForm do
  begin
    CurrentTableNameLbl.Caption := FCurrentTablename;

    TablesLbl.Caption := IntToStr(FTableCount);
    CurrentTableLbl.Caption := IntToStr(FTablesProcessed);
    TablesProgressBar.Max := FTableCount;
    TablesProgressBar.Position := FTablesProcessed - 1;

    TotalRowsLbl.Caption := IntToStr(FRowCount);
    CurrentRowLbl.Caption := IntToStr(FRowsProcessed);
    RowProgressBar.Max := FRowCount;
    RowProgressBar.Position := FRowsProcessed - 1;

    Update;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.FinishedBackup;

begin
  //Set Progress Bars to 100%
  FTablesProcessed := FTableCount + 1;
  FRowsProcessed := FRowCount + 1;

  UpdateProgressForm;

  ShowModalDialog(_('Backup finished'),
    _('The Backup was finished successfully.') + #13#10#13#10 +
    Format(_('The File %s has been created.'), [FFilename]),
    myx_mtInformation, _('OK'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.FreeAdminBackupProgressForm;

begin
  FProgressForm.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBackupThread.ShowError;

var
  ErrorTxt: WideString;

begin
  if Ord(FBackupError) = -1 then
    ShowModalDialog(_('Backup stopped'), _('The Backup has been stopped.'), myx_mtConfirmation, _('OK'))
  else
  begin
    ErrorTxt := myx_get_backup_error_string(FBackupError);
    case FBackupError of
      MYX_BACKUP_SERVER_ERROR:
        ErrorTxt := WideFormat(ErrorTxt, [myx_mysql_errno(FBackupForm.MySQLConn.MySQL),
          myx_mysql_error(FBackupForm.MySQLConn.MySQL)]);
      MYX_BACKUP_CANT_OPEN_FILE:
        ErrorTxt := WideFormat(ErrorTxt, [FFilename]);
      MYX_BACKUP_OUTPUTDEVICE_FULL:
        ErrorTxt := WideFormat(ErrorTxt, [FFilename]);
      MYX_BACKUP_CANT_GET_SQL:
        ErrorTxt := ErrorTxt + #13#10#13#10 + WideFormat(_('Current Object is "%s" '), [FCurrentTablename]);
    end;

    ShowModalDialog(_('Backup error'), _('An Error occured while executing the backup.') + #13#10#13#10 + ErrorTxt,
      myx_mtError, _('OK'));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ConnectionLost(var Message: TMessage);

begin
  ClearControls(False);

  NewProjectBtn.Enabled := False;

  ProjectListView.Selected := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ConnectionEstablished(var Message: TMessage);

begin
  NewProjectBtn.Enabled := True;

  SchemataFrame.FillSchemaTree;

  ProjectListView.Selected := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.SchemaListChanged(var Message: TMessage);

begin
  SchemataFrame.ReloadSchemaTree;
  SchemataFrame.FillSchemaTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.AdvancedEditFrameSearchEdChange(Sender: TObject);

begin
  AdvancedEditFrame.SearchEdChange(Sender);

  RefreshBackupProfiles;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ScheduleCBoxClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  // Check if service is available
  StartScheduler;

  if (ScheduleCBox.Checked) then
  begin
    if MySQLConn.UserConnection.connection_name = '' then
    begin
      ScheduleCBox.Checked := False;
      if ShowModalDialog(_('Attention!'),
        _('A scheduled backup requires a stored connection. You are currently connected without using such a stored ' +
        'connection. Please use the connection manager to create one and use it then for the login.'),
        myx_mtError, _('OK') + #13#10 + _('Open Options')) = 2 then
      begin
        OptionsForm := TOptionsForm.Create(self, TAdminOptionPagesForm.Create(self), ApplicationDM.CurrentConnection.MySQL);

        try
          // Select Connections Page
          OptionsForm.ShowOptionPage(ConnectionsPage);
          OptionsForm.ActiveControl := OptionsForm.ConnsTreeView;
          OptionsForm.ShowModal;
        finally
          OptionsForm.Free;
        end;
      end;
    end;

    if (MYXCommonOptions.PasswordStorageType <> MYX_PASSWORD_OBSCURED) then
      if (ShowModalDialog(_('Attention!'),
        _('You have to enable the Password Storage option on the ' +
        'programm''s General Options tabsheet and select Obscured as ' +
        'password storage method to execute scheduled ' +
        'backups. Otherwise the backup task cannot log into ' +
        'the database.'),
        myx_mtError, _('OK') + #13#10 + _('Open Options')) = 2) then
      begin
        OptionsForm := TOptionsForm.Create(self, TAdminOptionPagesForm.Create(Self), ApplicationDM.CurrentConnection.MySQL);
        try
          OptionsForm.ShowModal;
        finally
          OptionsForm.Free;
        end;
      end;

    if (Trim(ScheduleTargetDirEd.Text) = '') then
      ScheduleTargetDirEd.Text := 'C:\';

    if (Trim(ScheduleFilenamePrefixEd.Text) = '') then
      ScheduleFilenamePrefixEd.Text := ProjectNameEd.Text;
  end;

  DisableEnableScheduleControls;
  DoChange(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.ScheduleTypeCboxCloseUp(Sender: TObject);

begin
  SchedulePageControl.ActivePageIndex := ScheduleTypeCbox.ItemIndex;

  DoChange(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.SchemataFrameCatalogVSTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if (ssLeft in Shift) then
  begin
    //Simulate Treshold before Drag
    if ((abs(MousePos.X - Mouse.CursorPos.X) > 5) or
      (abs(MousePos.Y - Mouse.CursorPos.Y) > 5)) then
      SchemataFrame.CatalogVST.BeginDrag(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.SchemataFrameCatalogVSTDblClick(Sender: TObject);

begin
  //Prevent user from doubleclicking
  Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminBackupForm.StartScheduler: DWORD;

var
  hsc: SC_HANDLE;
  hSchSvc: SC_HANDLE;
  SvcStatus: TServiceStatus;
  ServiceArgVectors: PChar;

begin
  hsc := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);

  if hsc = 0 then
    Result := GetLastError
  else
  begin
    hSchSvc := OpenService(hsc, 'Schedule', SERVICE_START or SERVICE_QUERY_STATUS);
    CloseServiceHandle(hsc);

    if hSchSvc = 0 then
      Result := GetLastError
    else
    begin
      if not QueryServiceStatus(hSchSvc, SvcStatus) then
      begin
        CloseServiceHandle(hSchSvc);
        Result := GetLastError;
      end
      else
      begin
        if SvcStatus.dwCurrentState = SERVICE_RUNNING then
        begin
          CloseServiceHandle(hSchSvc);
          Result := ERROR_SUCCESS;
        end
        else
        begin
          ServiceArgVectors := nil;
          if not StartService(hSchSvc, 0, ServiceArgVectors) then
          begin
            CloseServiceHandle(hSchSvc);
            Result := GetLastError;
          end
          else
          begin
            CloseServiceHandle(hSchSvc);
            Result := ERROR_SUCCESS;
          end;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.WarnAboutFQI;

begin
  Application.ProcessMessages;
  
  if FullPathCBox.Checked then
    ShowModalDialog(_('Fully qualified identifiers'),
      _('You have fully qualified identifiers enabled for your backup. '#10'Please be aware that enabling them means you ' +
        'can later not restore the dump to a different schema, if that is required.'), myx_mtWarning, _('OK'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.BackupTreeViewCompare(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer;
  var Compare: Integer);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminBackupForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TAdminBackupForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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

var
  NeedToUnitialize: Boolean;

initialization
  // Initialize OLE subsystem for drag'n drop and clipboard operations.
  NeedToUnitialize := Succeeded(OleInitialize(nil));
finalization
  if NeedToUnitialize then
    OleUninitialize;
end.

