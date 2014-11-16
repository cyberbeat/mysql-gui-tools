unit AdminRestore;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  InstanceSections, AdditionalClasses,
  ImgList, Buttons, Contnrs, Menus, ApplicationDataModule, AuxFuncs,
  myx_public_interface, myx_admin_public_interface, SchemataTreeView,
  PNGImage, AuxAdminBackupRestore, MySQLConnection, MyxError,
  TntExtCtrls, TntMenus, TntComCtrls, TntStdCtrls, TntDialogs,gnugettext,
  AdminRestoreProgress, VirtualTrees;

type
  TRestoreOperations = set of TRestoreOperation;

  TAdminRestoreForm = class;


  TRestoreThread = class(TThread)
  private
    FBackupError: MYX_BACKUP_ERROR;
    FBackupContent: TMYX_BACKUP_CONTENT;
    FAdminRestoreForm: TAdminRestoreForm;
    FAdminRestoreProgressForm: TAdminRestoreProgressForm;
    PMysql: Pointer;
    FRestoreType: TRestoreOperations;
    FInternalErrorMsg: WideString;
    FInterrupted: Boolean;
    FCurrentOperation: TRestoreOperation;
    FOptions: Integer;
    FTargetSchema: WideString;

    procedure Ending(Sender: TObject);
    procedure ShowError;
    procedure ShowInterrupted;
    procedure ShowSuccess;
    procedure UpdateForm;
    function GetErrorMessage: WideString;
  protected
    procedure Execute; override;
  public
    constructor Create(AdminRestoreForm: TAdminRestoreForm; AdminRestoreProgressForm: TAdminRestoreProgressForm;
      RestoreType: TRestoreOperations; Options: Integer; Target: WideString);
    destructor Destroy;  override;
    procedure BuildContentTree();

    procedure RestoreWarning(msg: WideString);
    function RestoreProgress(bytes_read: Int64; bytes_total: Int64):Integer;
  end;

  TAdminRestoreForm = class(TInstanceSectionForm)
    ServerBackupRestorePnl: TTntPanel;
    BackupRestorePageControl: TTntPageControl;
    ContentSheet: TTabSheet;
    Panel1: TTntPanel;
    RestoreBevel: TTntBevel;
    Label3: TTntLabel;
    RestoreImg: TTntImage;
    RestoreTreeView: TVirtualStringTree;
    BackupTreeViewPopupMenu: TTntPopupMenu;
    RemoveBackupNodeMI: TTntMenuItem;
    SubTreeSearchPopupMenu: TTntPopupMenu;
    AllcatalogsMI: TTntMenuItem;
    system1: TTntMenuItem;
    dev1: TTntMenuItem;
    world1: TTntMenuItem;
    GeneralSheet: TTabSheet;
    Panel6: TTntPanel;
    BackupBevel: TTntBevel;
    Label6: TTntLabel;
    Restore2Img: TTntImage;
    Label7: TTntLabel;
    Label1: TTntLabel;
    GroupBox1: TTntGroupBox;
    Label9: TTntLabel;
    Label10: TTntLabel;
    Label11: TTntLabel;
    Label13: TTntLabel;
    Label14: TTntLabel;
    FileToRestoreEd: TTntEdit;
    BackupFileTypesLU: TTntComboBox;
    TargetSchemaCBox: TTntComboBox;
    Panel7: TTntPanel;
    Panel8: TTntPanel;
    StartRestoreBtn: TTntButton;
    OpenBackupFileBtn: TTntButton;
    GroupBox2: TTntGroupBox;
    Label5: TTntLabel;
    Label8: TTntLabel;
    CharsetCBox: TTntComboBox;
    AnalyzeBackupContentBtn: TTntButton;
    TntGroupBox1: TTntGroupBox;
    ForceCheckbox: TTntCheckBox;
    CreateDBsCheckbox: TTntCheckBox;
    OrgSchemaRadioButton: TRadioButton;
    OtherSchemaRadioButton: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure RefreshSchemaTree(searchStr: WideString = '');
    procedure SubTreeSeachEdMagnifyGlassImgClick(Sender: TObject);


    procedure OpenBackupFileBtnClick(Sender: TObject);
    procedure StartRestoreBtnClick(Sender: TObject);
    procedure ClearControls;

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure AnalyzeBackupContentBtnClick(Sender: TObject);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure RestoreTreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RestoreTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RestoreTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure RestoreTreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RestoreTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure SchemaRadioButtonClick(Sender: TObject);
  private
    CatalogList: TMYX_CATALOGS;
    RestorePNGImg: TPNGObject;
    AdminRestoreProgressForm: TAdminRestoreProgressForm;

    procedure GetAllSchemas();
  public
    CurrentCatalog: TMYX_CATALOG;
    CurrentSchema: TMYX_SCHEMA;

    // User must not expand SchemaTree manually.
    LockSchemaTreeExpansion: Boolean;
    function getRestoreOptions(): Integer;
    procedure GetCatalogList(Sender: TObject);
    procedure RefreshCatalogList(Sender: TObject);
  end;

  function AnalyzeProgress(bytes_read: Int64; bytes_total: Int64; user_data: Pointer):Integer cdecl;
  function RestoreProgress(bytes_read: Int64; bytes_total: Int64; user_data: Pointer):Integer cdecl;
  procedure RestoreWarning(msg: PChar; user_data: Pointer) cdecl;

var
  AdminRestoreForm: TAdminRestoreForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Main, AdminBackup, NameEditor, PNGTools;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.FormCreate(Sender: TObject);

begin
  InitForm(self);
  
  DockedPanel:=ServerBackupRestorePnl;

  BackupRestorePageControl.ActivePageIndex:=0;

  CurrentCatalog:=nil;
  CurrentSchema:=nil;

  LockSchemaTreeExpansion:=True;

  RestorePNGImg:=LoadPNGImageFromResource('restore', RestoreImg);
  Restore2Img.Picture.Assign(RestorePNGImg);

  DisableEnableControls(GeneralSheet, False);
  DisableEnableControls(ContentSheet, False);

  if(Not(MySQLConn.Connected))then
    OpenBackupFileBtn.Enabled:=False;

  BackupFileTypesLU.ItemIndex:=0;

  // No freeing necessary since the form should be automatically freed when
  // this instance of TAdminRestoreForms is destroyed.
  AdminRestoreProgressForm := TAdminRestoreProgressForm.Create(Self);

  RestoreTreeview.NodeDataSize := SizeOf(TBackupNodeData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.FormDestroy(Sender: TObject);

begin
  RestorePNGImg.Free;
  CatalogList.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.RefreshSchemaTree(searchStr: WideString = '');

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.SubTreeSeachEdMagnifyGlassImgClick(Sender: TObject);

begin
  SubTreeSearchPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.OpenBackupFileBtnClick(Sender: TObject);

var
  OpenDialog: TTntOpenDialog;

begin
  BackupRestorePageControl.ActivePage := GeneralSheet;

  OpenDialog := TTntOpenDialog.Create(self);
  try
    OpenDialog.Filter := _('SQL Files') + '|*.sql|' + _('All Files') + '|*.*';

    if OpenDialog.Execute then
    begin
      RestoreTreeView.Clear;

      DisableEnablePages(BackupRestorePageControl, True);
      RestoreTreeView.Enabled := False;
      SchemaRadioButtonClick(nil);

      FileToRestoreEd.Text := OpenDialog.FileName;

      StartRestoreBtn.Enabled := True;
      GetAllSchemas();
    end;
  finally
    OpenDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.SchemaRadioButtonClick(Sender: TObject);

begin
  TargetSchemaCBox.Enabled := OtherSchemaRadioButton.Checked;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.GetAllSchemas();

begin
  FreeAndNil(CatalogList);

  //(Re-)load CatalogList
  //AllowDuplicatedKindOfData
  MySQLConn.FetchData(dkCatalogSchema,
    GetCatalogList, RefreshCatalogList,
    nil, _('Fetching Catalogs/Schemata data ...'),
    False, True);

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.GetCatalogList(Sender: TObject);

var
  catalogs: PMYX_CATALOGS;

begin
  catalogs:=myx_get_catalogs(TFetchDataThread(Sender).Connection.MySQL);
  if(catalogs=nil)then
    raise EMyxSQLError.Create(_('Could not fetch Catalogs/Schemata data.'),
      myx_mysql_errno(TFetchDataThread(Sender).Connection.MySQL),
      myx_mysql_error(TFetchDataThread(Sender).Connection.MySQL));

  try
    CatalogList:=TMYX_CATALOGS.Create(catalogs);
  finally
    myx_free_catalogs(catalogs);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.RefreshCatalogList(Sender: TObject);

var
  I, J: integer;

begin
  TargetSchemaCBox.Clear;

  for I := 0 to CatalogList.Catalogs.Count - 1 do
    for J := 0 to CatalogList.Catalogs[I].schemata.Count-1 do
      TargetSchemaCBox.AddItem(CatalogList.Catalogs[I].schemata[J].schema_name, nil);
  if CatalogList.Catalogs.Count > 0 then
    TargetSchemaCBox.ItemIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function AnalyzeProgress(bytes_read: Int64; bytes_total: Int64; user_data: Pointer):Integer;

var
  PSender: ^TRestoreThread;

begin
  PSender:=user_data;
  Result:= PSender.RestoreProgress(bytes_read, bytes_total);
end;

//----------------------------------------------------------------------------------------------------------------------

function RestoreProgress(bytes_read: Int64; bytes_total: Int64; user_data: Pointer):Integer;

var
  PSender: ^TRestoreThread;

begin
  PSender:=user_data;
  Result:= PSender.RestoreProgress(bytes_read, bytes_total);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RestoreWarning(msg: PChar; user_data: Pointer);

var
  PSender: ^TRestoreThread;

begin
  PSender:=user_data;
  PSender.RestoreWarning(UTF8Decode(msg));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.StartRestoreBtnClick(Sender: TObject);

var
  RestoreThread: TRestoreThread;
  Target: WideString;
  
begin
  if TargetSchemaCBox.Enabled then
    Target := TargetSchemaCBox.Text
  else
    Target := '';

  if RestoreTreeView.TotalCount = 0 then
    RestoreThread:= TRestoreThread.Create(Self, AdminRestoreProgressForm, [TR_ANALYZE, TR_RESTORE], GetRestoreOptions,
      Target)
  else
    RestoreThread := TRestoreThread.Create(Self, AdminRestoreProgressForm, [TR_RESTORE], GetRestoreOptions, Target);

  RestoreThread.FreeOnTerminate := True;

  // Start the thread.
  RestoreThread.Resume;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.ClearControls;

begin
  FileToRestoreEd.Text := '';
  TargetSchemaCBox.ItemIndex := -1;
  BackupFileTypesLU.ItemIndex := 0;
  RestoreTreeView.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.ConnectionLost(var Message: TMessage);

begin
  DisableEnablePages(BackupRestorePageControl, False);
  ClearControls;

  OpenBackupFileBtn.Enabled:=False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.ConnectionEstablished(var Message: TMessage);

begin
  OpenBackupFileBtn.Enabled:=True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.AnalyzeBackupContentBtnClick(Sender: TObject);

var
  Thread: TRestoreThread;
  Target: WideString;

begin
  // Create a new thread where the file is analyzed.
  if FileToRestoreEd.Text <> '' then
  begin
    if TargetSchemaCBox.Enabled then
      Target := TargetSchemaCBox.Text
    else
      Target := '';

    RestoreTreeView.Clear;
    Thread := TRestoreThread.Create(self, AdminRestoreProgressForm, [TR_ANALYZE], GetRestoreOptions, Target);
    Thread.FreeOnTerminate := True;
    Thread.Resume;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminRestoreForm.getRestoreOptions(): Integer;

begin
  Result := 0;
  if ForceCheckbox.Checked then
    Result := Result or Integer(MYX_RBS_FORCE);
  if not CreateDBsCheckbox.Checked then
    Result := Result or Integer(MYX_RBS_DONT_CREATE_TARGETS);

  Result:= Result;
end;

//----------------- TRestoreThread -------------------------------------------------------------------------------------

constructor TRestoreThread.Create(AdminRestoreForm: TAdminRestoreForm; AdminRestoreProgressForm: TAdminRestoreProgressForm;
  RestoreType: TRestoreOperations; Options: Integer; Target: WideString);

begin
  inherited Create(True);

  FRestoreType := RestoreType;
  FAdminRestoreForm := AdminRestoreForm;
  FAdminRestoreProgressForm := Adminrestoreprogressform;
  OnTerminate := Ending;
  FOptions := Options;
  FTargetSchema := Target;

  FInternalErrorMsg := '';
  FInterrupted := false;

  PMySQL := myx_mysql_init();
  if PMySQL = nil then
  begin
    FInternalErrorMsg:= _('Error while allocating memory for MySQL Struct in the Restore Thread.');
    FBackupError := MYX_BACKUP_MALLOC_FAILED;
  end;

  if myx_connect_to_instance(AdminRestoreForm.MySQLConn.UserConnection.get_record_pointer, PMySQL) <> 0 then
  begin
    FInternalErrorMsg:= _('Restore Thread cannot connect to MySQL');
    FBackupError := MYX_BACKUP_SERVER_ERROR;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TRestoreThread.Destroy;

begin
  myx_mysql_close(PMySQL);

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.Ending(Sender: TObject);

begin
  if FBackupError <> MYX_BACKUP_NO_ERROR then
    Synchronize(ShowError)
  else
    if FInterrupted then
      Synchronize(ShowInterrupted)
    else
      Synchronize(ShowSuccess);

  FAdminRestoreForm.MySQLConn.SchemaListChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.BuildContentTree;

// Simple wrapper around BuildcontentTreeFromRestoreContent that can
// be used together with Synchronize.

begin
  BuildContentTreeFromRestoreContent(FAdminRestoreForm.RestoreTreeView, FBackupContent);
  FAdminRestoreForm.RestoreTreeView.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRestoreThread.RestoreProgress(bytes_read: Int64; bytes_total: Int64):Integer;

// Updates the ProgressForm.

begin
  FAdminRestoreProgressForm.UpdateProgress(bytes_read, bytes_total);
  if (FAdminRestoreProgressForm.Stopping) then
  begin
    Result := 1;
    FInterrupted:= true;
  end
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRestoreThread.GetErrorMessage: WideString;

begin
  if FInternalErrorMsg <> '' then
    Result := FInternalErrorMsg
  else
  begin
    Result := myx_get_backup_error_string(FBackupError);
    case FBackupError of
      MYX_BACKUP_SERVER_ERROR:
        Result := Format(Result, [myx_mysql_errno(PMySQL), myx_mysql_error(PMySQL)]);
      MYX_BACKUP_CANT_OPEN_FILE:
        Result := Format(Result, [FAdminRestoreForm.FileToRestoreEd.Text]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.RestoreWarning(msg: WideString);

begin
  FAdminRestoreProgressForm.AddWarning(msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.UpdateForm;

// Simple wrapper that can be used together with Synchronize.

begin
  FAdminRestoreProgressForm.UpdateForm(FCurrentOperation, FAdminRestoreForm.FileToRestoreEd.Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.Execute;

var
  BackupType: MYX_BACKUP_TYPE;
  PBackupContent: PMYX_BACKUP_CONTENT;
  Size: Int64;
  Interval: Integer;
  
begin
  FBackupError := MYX_BACKUP_NO_ERROR;

  // Get Backup FileType
  case FAdminRestoreForm.BackupFileTypesLU.ItemIndex of
    0:
      BackupType := MYX_BT_SQL_SCRIPT;
  else
    BackupType := MYX_BT_SQL_SCRIPT;
  end;

  Synchronize(FAdminRestoreProgressForm.ResetProgress);
  Synchronize(FAdminRestoreProgressForm.Show);

  // Determine update interval depending on file size.
  Size := GetFileSize(FAdminRestoreForm.FileToRestoreEd.Text);
  Interval := Size div 100;
  if Interval < 1 then
    Interval := 1;
  if Interval > 1000000 then
    Interval := 1000000;

  if TR_ANALYZE in FRestoreType then
  begin
    FCurrentOperation := TR_ANALYZE;
    Synchronize(UpdateForm);
    FBackupError := MYX_BACKUP_NO_ERROR;

    //Get tables from backup file
    try
      PBackupContent := myx_get_backup_content(FAdminRestoreForm.FileToRestoreEd.Text, FAdminRestoreForm.CharsetCBox.Text,
        BackupType, Interval, AnalyzeProgress, AdminRestore.RestoreWarning, Addr(self), @FBackupError,
          Ord(FTargetSchema <> ''), FOptions and Integer(MYX_RBS_FORCE));

      if Assigned(PBackupContent) then
      begin
        FBackupContent := TMYX_BACKUP_CONTENT.create(PBackupContent);
        myx_free_backup_content(PBackupContent);

        Synchronize(self.BuildContentTree);
      end;
    except
      FBackupError := MYX_BACKUP_UNKNOWN;
    end;
  end;

  if (FBackupError <> MYX_BACKUP_NO_ERROR) and (FBackupError <> MYX_BACKUP_DIFFERENT_SCHEMA_IMPOSSIBLE) then
  begin
    if (FOptions and Integer(MYX_RBS_FORCE)) <> 0 then
      FBackupError := MYX_BACKUP_NO_ERROR;
  end;
  
  FBackupContent.Free;

  if (FBackupError = MYX_BACKUP_NO_ERROR) and (TR_RESTORE in FRestoreType) then
  begin
    Synchronize(FAdminRestoreProgressForm.ResetProgress);

    FBackupContent:= GetBackupContent(FAdminRestoreForm.RestoreTreeView);
    FCurrentOperation:= TR_RESTORE;
    Synchronize(UpdateForm);

    try
      FBackupError:=myx_restore_backup(PMySQL,
        FAdminRestoreForm.FileToRestoreEd.Text,
        FAdminRestoreForm.CharsetCBox.Text,
        FBackupContent.get_record_pointer,
        '', FTargetSchema, BackupType, FOptions, Interval, AdminRestore.RestoreProgress, Addr(Self),
          AdminRestore.RestoreWarning, Addr(Self));

      if FBackupError = MYX_BACKUP_NO_ERROR then
        FBackupContent.Free;
    except
      FBackupError := MYX_BACKUP_UNKNOWN;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.ShowError;

var
  ErrorTxt: WideString;

begin
  ErrorTxt := GetErrorMessage;

  FAdminRestoreProgressForm.AddWarning(ErrorTxt);
  if (FCurrentOperation= TR_ANALYZE) then
    FAdminRestoreProgressForm.OperationFinished(_('The analyzation failed.'), True)
  else
    FAdminRestoreProgressForm.OperationFinished(_('The restoration failed.'), True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.ShowInterrupted;

begin
  if (FCurrentOperation= TR_ANALYZE) then
    FAdminRestoreProgressForm.OperationFinished( _('The analyze operation was stopped.'), False)
  else
    FAdminRestoreProgressForm.OperationFinished( _('The restore operation was stopped.'), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRestoreThread.ShowSuccess;

begin
  if (FCurrentOperation= TR_ANALYZE) then
    FAdminRestoreProgressForm.OperationFinished(_('The analyze operation was finished successfully.'), False)
  else
    FAdminRestoreProgressForm.OperationFinished(_('The restore operation was finished successfully.'), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.RestoreTreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

const
  CheckStateToBackupCheckType: array[TCheckState] of TBackupCheckType = (
    bctNone, bctNone, bctAll, bctAll, bctSome, bctSome);
  
begin
  SetNodeSelectState(RestoreTreeView, Node, CheckStateTOBackupCheckType[Node.CheckState]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.RestoreTreeViewFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PBackupNodeData;

begin
  NodeData := Sender.GetNodeData(Node);
  NodeData.BackupNode.Free;
  Finalize(NodeData^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.RestoreTreeViewGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

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

procedure TAdminRestoreForm.RestoreTreeViewGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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

procedure TAdminRestoreForm.RestoreTreeViewInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

begin
  Sender.CheckType[Node] := ctTriStateCheckbox;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TAdminRestoreForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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
      if Item.Parent.Parent = nil then
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
