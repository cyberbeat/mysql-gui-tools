unit EditorTable;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, ExtCtrls,
  VirtualTrees, AuxFuncs, CheckLst, TntCheckLst, Buttons, TntButtons,
  MySQLConnection, myx_util_public_interface, myx_public_interface, MyxError, ImgList, PngImage,
  TntForms, Options, EditorTableVTEdit, EditorTableVTFKEdit,
  TntMenus, TntExtCtrls, Menus, ActiveX, TntSysUtils, Contnrs;

type
  DBMEditMode = (DBMEditMode_Online = 1, DBMEditMode_Offline);

  TEditorTableForm = class(TTntForm)
    TableEditorPnl: TTntPanel;
    HeaderPnl: TTntPanel;
    LeftPnl: TTntPanel;
    RightPnl: TTntPanel;
    BottomPnl: TTntPanel;
    GirdOptionPnl: TTntPanel;
    TableNameLbl: TTntLabel;
    TableNameEd: TTntEdit;
    TntLabel2: TTntLabel;
    DatabaseCBox: TTntComboBox;
    TntLabel3: TTntLabel;
    CommentEd: TTntEdit;
    DiscardChangesBtn: TTntButton;
    ApplyChangesBtn: TTntButton;
    ColumnImgList: TImageList;
    ColumnHeaderImgList: TImageList;
    MainPageControl: TTntPageControl;
    ColumnTabSheet: TTabSheet;
    TabSheet2: TTabSheet;
    StdInsertsTabSheet: TTabSheet;
    DescTabSheet: TTabSheet;
    ColumnVST: TVirtualStringTree;
    ColumnOptionSplitter: TTntSplitter;
    ColumnsPageControl: TTntPageControl;
    TntTabSheet1: TTntTabSheet;
    TntLabel4: TTntLabel;
    TntLabel5: TTntLabel;
    TntLabel6: TTntLabel;
    TntLabel7: TTntLabel;
    TntLabel8: TTntLabel;
    TntLabel36: TTntLabel;
    TntLabel37: TTntLabel;
    ColumnNameEd: TTntEdit;
    ColumnDatatypeEd: TTntEdit;
    ColumnFlagsCheckListBox: TTntCheckListBox;
    ColumnCommentMemo: TTntMemo;
    ColumnDefaultValueEd: TTntEdit;
    ColumnCharsetCBox: TTntComboBox;
    ColumnCollateCBox: TTntComboBox;
    TntGroupBox5: TTntGroupBox;
    ColumnPKCBox: TTntCheckBox;
    ColumnNotNullCBox: TTntCheckBox;
    ColumnAutoIncCBox: TTntCheckBox;
    IndexTabsheet: TTntTabSheet;
    TntGroupBox1: TTntGroupBox;
    IndexNameLbl: TTntLabel;
    IndexKindLbl: TTntLabel;
    TntLabel12: TTntLabel;
    IndexTypeLbl: TTntLabel;
    IndexNameEd: TTntEdit;
    IndexKindCBox: TTntComboBox;
    IndexColumnsListBox: TTntListBox;
    IndexTypeCBox: TTntComboBox;
    FKTabsheet: TTntTabSheet;
    TntGroupBox2: TTntGroupBox;
    TntLabel14: TTntLabel;
    TntLabel15: TTntLabel;
    TntLabel16: TTntLabel;
    TntLabel17: TTntLabel;
    FKNameEd: TTntEdit;
    FKOnDeleteComboBox: TTntComboBox;
    FKColsVST: TVirtualStringTree;
    FKOnUpdateComboBox: TTntComboBox;
    FKDestTablesComboBox: TTntComboBox;
    TableOptionScrollBox: TTntScrollBox;
    TntGroupBox6: TTntGroupBox;
    TableCharsetCaptionLbl: TTntLabel;
    TableCollationCaptionLbl: TTntLabel;
    TableCharsetComboBox: TTntComboBox;
    TableCollationComboBox: TTntComboBox;
    TntMemo3: TTntMemo;
    TntLabel40: TTntLabel;
    TntLabel41: TTntLabel;
    TntMemo2: TTntMemo;
    AdvTableOptionsSheet: TTabSheet;
    AdvancedOptionsScrollBox: TTntScrollBox;
    StorageOptionsGBox: TTntGroupBox;
    TntLabel27: TTntLabel;
    TntLabel28: TTntLabel;
    DataDirectoryEd: TTntEdit;
    IndexDirectoryEd: TTntEdit;
    RowOptionsGBox: TTntGroupBox;
    TntLabel22: TTntLabel;
    TntLabel24: TTntLabel;
    TntLabel25: TTntLabel;
    TntLabel26: TTntLabel;
    AvgRowLengthEd: TTntEdit;
    MinRowsEd: TTntEdit;
    MaxRowsEd: TTntEdit;
    UseChecksumCBox: TTntCheckBox;
    RowFormatComboBox: TTntComboBox;
    RaidOptionsGBox: TTntGroupBox;
    TntLabel29: TTntLabel;
    NumberOfChunksCaptionLbl: TTntLabel;
    ChunkSizeCaptionEd: TTntLabel;
    ChunkSizeUnitLbl: TTntLabel;
    RaidTypeComboBox: TTntComboBox;
    NumberOfChunksEd: TTntEdit;
    ChunkSizeEd: TTntEdit;
    ChunkSizeUpDown: TTntUpDown;
    NumberOfChunksUpDown: TTntUpDown;
    VariousGBox: TTntGroupBox;
    TntLabel20: TTntLabel;
    AutoIncEd: TTntEdit;
    TntLabel21: TTntLabel;
    TablePasswordEd: TTntEdit;
    DelayKeyUpdatesCBox: TTntCheckBox;
    TntLabel19: TTntLabel;
    PackKeysComboBox: TTntComboBox;
    StorageEngineGBox: TTntGroupBox;
    TableEngineDescLbl: TTntLabel;
    TableCharsetDescLbl: TTntLabel;
    TableCollationDescLbl: TTntLabel;
    TntLabel51: TTntLabel;
    TntLabel52: TTntLabel;
    TntLabel53: TTntLabel;
    TntLabel54: TTntLabel;
    TntLabel55: TTntLabel;
    TntLabel56: TTntLabel;
    TntLabel57: TTntLabel;
    NumberOfChunksDescLbl: TTntLabel;
    ChunkSizeDescLbl: TTntLabel;
    TntLabel39: TTntLabel;
    TntLabel60: TTntLabel;
    TntLabel61: TTntLabel;
    TntLabel62: TTntLabel;
    TntLabel63: TTntLabel;
    MergeOptionGBox: TTntGroupBox;
    MergeInsertMethodCapionLbl: TTntLabel;
    UnionTablesCaptionLbl: TTntLabel;
    UnionTablesDescLbl: TTntLabel;
    MergeInsertMethodDescLbl: TTntLabel;
    MergeInsertMethodComboBox: TTntComboBox;
    UnionTablesEd: TTntEdit;
    CloseBtn: TTntButton;
    ScrollSpacerAdvOptShape: TTntShape;
    ScrollSpacerTableOptionsShape: TTntShape;
    DetailsBtn: TTntButton;
    AddFKBtn: TTntSpeedButton;
    DropFKBtn: TTntSpeedButton;
    AddIndexBtn: TTntSpeedButton;
    DeleteIndexBtn: TTntSpeedButton;
    AddIndexColumnBtn: TTntSpeedButton;
    DeleteIndexColumnBtn: TTntSpeedButton;
    AdvIndexColumnBtn: TTntSpeedButton;
    IndexVST: TVirtualStringTree;
    TntLabel9: TTntLabel;
    IndexColumnPopupMenu: TTntPopupMenu;
    SetIndexColumnLengthMI: TTntMenuItem;
    TopPnl: TTntPanel;
    FKVST: TVirtualStringTree;
    ColumnPopupMenu: TTntPopupMenu;
    DeleteTableColumnsMI: TTntMenuItem;
    DockedHeaderPnl: TTntPanel;
    HeaderImg: TTntImage;
    TntLabel1: TTntLabel;
    TntLabel10: TTntLabel;
    Bevel1: TTntBevel;
    FKColumnPopupMenu: TTntPopupMenu;
    RemoveFKColumnMI: TTntMenuItem;
    SetDefValNullBtn: TTntBitBtn;
    TableEngineLU: TTntComboBox;
    TntLabel11: TTntLabel;
    TntGroupBox3: TTntGroupBox;
    TntLabel18: TTntLabel;
    TntLabel23: TTntLabel;
    FederatedConnectionEd: TTntEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ClearCurrentVariables;

    procedure SetDockLayout;

    function EditStatusTable(catalog: WideString; schema: WideString; table: WideString;
      TableStatusList: TMYX_SCHEMA_TABLE_STATUS = nil): Boolean; overload;
    function EditSchemaTable(catalog: WideString; schema: WideString; table: WideString;
      SchemaTableList: TMYX_SCHEMA_TABLES = nil): Boolean; overload;
    procedure RetrieveTableData(catalog: WideString;
      schema: WideString; table: WideString);

    procedure SetDatabaseVersion(DBMajorVersion: Integer;
      DBMinorVersion: Integer);
    procedure SetTableData(TableData: TMYX_DBM_TABLE_DATA);
    procedure SetCurrentColumn(Column: TMYX_DBM_COLUMN_DATA);
    procedure SetCurrentIndex(Index: TMYX_DBM_INDEX_DATA);
    procedure SetCurrentFK(FK: TMYX_DBM_FK_DATA);
    procedure RefreshFKTableComboBox;

    procedure ColumnVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);

    procedure FreeTableData;
    procedure ColumnVSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ColumnVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure ColumnVSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure MergeRBtnClick(Sender: TObject);

    procedure RefreshCollationCBox(CharsetComboBox: TTntComboBox;
      CollationComboBox: TTntComboBox; SelectCollation: WideString = '');
    procedure TableCharsetComboBoxChange(Sender: TObject);
    procedure ColumnCharsetCBoxCloseUp(Sender: TObject);
    procedure SetCharset(CharsetComboBox: TTntComboBox;
      SelectCharset: WideString);

    procedure ColumnVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure DoColumnGridEdit(var Message: TMessage); message WM_DoCellEdit;

    procedure ColumnVSTKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TableNameEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ColumnVSTEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ColumnVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColumnVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure ColumnVSTDblClick(Sender: TObject);
    procedure ColumnNameEdChange(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);

    procedure ShowHideDetails(StoreHeight: Boolean = True);
    procedure IndexVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure IndexVSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure IndexVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure IndexVSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure IndexColumnsListBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IndexColumnsListBoxDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure IndexColumnsListBoxDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure IndexColumnsListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DeleteIndexColumnBtnClick(Sender: TObject);
    procedure AddIndexColumnBtnClick(Sender: TObject);
    procedure AdvIndexColumnBtnClick(Sender: TObject);
    procedure SetIndexColumnLengthMIClick(Sender: TObject);
    procedure AddIndexBtnClick(Sender: TObject);
    procedure IndexNameEdChange(Sender: TObject);
    procedure IndexKindCBoxChange(Sender: TObject);
    procedure IndexTypeCBoxChange(Sender: TObject);
    procedure DeleteIndexBtnClick(Sender: TObject);
    procedure AddFKBtnClick(Sender: TObject);
    procedure FKColsVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure FKColsVSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure FKColsVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure FKDestTablesComboBoxCloseUp(Sender: TObject);
    procedure FKColsVSTDblClick(Sender: TObject);
    procedure FKColsVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ValueChanged(Sender: TObject);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DeleteSelectedColumns(Confirm: Boolean = True);
    procedure DeleteTableColumnsMIClick(Sender: TObject);
    function AddIndex: TMYX_DBM_INDEX_DATA;
    procedure FKOnDeleteComboBoxChange(Sender: TObject);
    procedure FKNameEdChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DiscardChangesBtnClick(Sender: TObject);
    procedure FKColsVSTDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure DropFKBtnClick(Sender: TObject);
    procedure UpdatePrimaryKeyIndex;
    procedure RefreshIndexVT;
    procedure FKDestTablesComboBoxChange(Sender: TObject);
    procedure RemoveFKColumnMIClick(Sender: TObject);
    procedure SetDefValNullBtnClick(Sender: TObject);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure TableEngineLUChange(Sender: TObject);
    procedure MainPageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure MainPageControlChange(Sender: TObject);
    procedure TableEngineLUDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure ColumnsPageControlChange(Sender: TObject);
  private
    PServerVersion: PMYX_DBM_SERVER_VERSION;

    Charsets: TMYX_DBM_CHARSETS;
    FSchemaTableList: TMYX_SCHEMA_TABLES;
    FTableStatusList: TMYX_SCHEMA_TABLE_STATUS;

    FTableData: TMYX_DBM_TABLE_DATA;
    FCurrentColumn: TMYX_DBM_COLUMN_DATA;
    FCurrentIndex: TMYX_DBM_INDEX_DATA;
    FCurrentFK: TMYX_DBM_FK_DATA;
    FCurrentFKRefTableStatus: TMYX_TABLE_STATUS;
    FCurrentFKRefSchemaTable: TMYX_SCHEMA_TABLE;

    DBMajorVersion,
    DBMinorVersion: Integer;

    ColumnPNGImg,
    ColumnFKPNGImg,
    ColumnPKPNGImg: TPNGObject;

    CheckmarkPNGImg,
    OptionsCheckboxPNGImg,
    OptionsCheckboxCheckedPNGImg: TPNGObject;

    DatatypeNumericPNGImg,
    DatatypeDateTimePNGImg,
    DatatypeStringPNGImg,
    DatatypeBlobPNGImg,
    DatatypeSpatialPNGImg,
    DatatypeUserdefinedPNGImg: TPNGObject;

    IsNullPNGImg: TPNGObject;

    AssetIndexPNGImg,
    AssetFKPNGImg: TPNGObject;

    HeaderPNGImg: TPNGObject;

    FSettingTableControls,
    FSettingColumnControls,
    FSettingIndexControls,
    FSettingFKControls: Boolean;

    ShowDetails: Boolean;
    HeightWithDetails,
    HeightWithoutDetails: Integer;

    FColumnNameInitialText: WideString;

    FIndexDragStartRow: Integer;

    FDocked: Boolean;
    FEditorClose: TNotifyEvent;
    FApplyedChanges: TNotifyEvent;

    FModified: Boolean;

    FConnection: TMySQLConn;
    FEditMode: DBMEditMode;

    FInitControls: Boolean;
    FEngines: TMYX_ENGINES;
    FInnoDbIsDefault: Boolean;

    procedure SetModified(Modified: Boolean);
    procedure AssignDatatypeToColumn(Datatype: WideString; column: TMYX_DBM_COLUMN_DATA; PDatatypes: PMYX_DBM_DATATYPES);
    procedure ShowMySQLMessagesInDialog(MySQL: Pointer);
    procedure UpdateEngines;
  protected
    procedure CreateParams(Var Params: TCreateParams); override;
  public

    property OnEditorClose: TNotifyEvent read FEditorClose write FEditorClose;
    property OnApplyedChanges: TNotifyEvent read FApplyedChanges write FApplyedChanges;
    property Modified: Boolean read FModified write SetModified;

    procedure SetEditMode(EditMode: DBMEditMode; MySQLConn: TMySQLConn = nil);
    procedure Show;

    property InitControls: Boolean read FInitControls write FInitControls;
  end;

  PColumnNodeData = ^TColumnNodeData;
  TColumnNodeData = record
    Column: TMYX_DBM_COLUMN_DATA;
  end;

var
  EditorTableForm: TEditorTableForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  PNGTools;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FormCreate(Sender: TObject);

begin
  InitForm(Self);

  PServerVersion := nil;
  Charsets := nil;
  FTableData := nil;
  FSchemaTableList := nil;
  FTableStatusList := nil;

  ClearCurrentVariables;

  FEditorClose := nil;
  FApplyedChanges := nil;
  FModified := False;

  FEditMode := DBMEditMode_Offline;

  FInitControls := False;
  FSettingTableControls := False;
  FSettingColumnControls := False;
  FSettingIndexControls := False;
  FSettingFKControls := False;

  DBMajorVersion := 4;
  DBMinorVersion := 0;

  FDocked := False;

  MainPageControl.ActivePageIndex := 0;
  ColumnsPageControl.ActivePageIndex := 0;

  TableOptionScrollBox.VertScrollBar.Position := 0;
  AdvancedOptionsScrollBox.VertScrollBar.Position := 0;

  ColumnPNGImg := LoadPNGImageFromResource('column');
  ColumnFKPNGImg := LoadPNGImageFromResource('column_fk');
  ColumnPKPNGImg := LoadPNGImageFromResource('column_pk');

  CheckmarkPNGImg := LoadPNGImageFromResource('checkmark');
  OptionsCheckboxPNGImg := LoadPNGImageFromResource('options_checkbox');
  OptionsCheckboxCheckedPNGImg := LoadPNGImageFromResource('options_checkbox_checked');

  DatatypeNumericPNGImg := LoadPNGImageFromResource('datatype_numeric');
  DatatypeDateTimePNGImg := LoadPNGImageFromResource('datatype_datetime');
  DatatypeStringPNGImg := LoadPNGImageFromResource('datatype_string');
  DatatypeBlobPNGImg := LoadPNGImageFromResource('datatype_blob');
  DatatypeSpatialPNGImg := LoadPNGImageFromResource('datatype_spatial');
  DatatypeUserdefinedPNGImg := LoadPNGImageFromResource('datatype_userdefined');

  AssetIndexPNGImg := LoadPNGImageFromResource('asset_index_16x16');
  AssetFKPNGImg := LoadPNGImageFromResource('asset_index_16x16');

  IsNullPNGImg := LoadPNGImageFromResource('field_overlay_null');

  HeaderPNGImg := LoadPNGImageFromResource('asset_table', HeaderImg);

  if(MYXCommonOptions.XPStyleEnabled)then
  begin
    TableOptionScrollBox.Color := clWhite;
    AdvancedOptionsScrollBox.Color := clWhite;
    ColumnOptionSplitter.Color := clWhite;
  end;

  IndexKindCBox.Items.Clear;
  IndexKindCBox.Items.Add('INDEX');
  IndexKindCBox.Items.Add('PRIMARY');
  IndexKindCBox.Items.Add('UNIQUE');
  IndexKindCBox.Items.Add('FULLTEXT');
  IndexKindCBox.Items.Add('SPATIAL');

  IndexTypeCBox.Items.Clear;
  IndexTypeCBox.Items.Add('DEFAULT');
  IndexTypeCBox.Items.Add('BTREE');
  IndexTypeCBox.Items.Add('HASH');
  IndexTypeCBox.Items.Add('RTREE');

  MergeInsertMethodComboBox.Items.Clear;
  MergeInsertMethodComboBox.Items.Add(_('No Inserts'));
  MergeInsertMethodComboBox.Items.Add(_('Inserts into first table'));
  MergeInsertMethodComboBox.Items.Add(_('Inserts into last table'));

  PackKeysComboBox.Items.Clear;
  PackKeysComboBox.Items.Add(_('Default'));
  PackKeysComboBox.Items.Add(_('Pack None'));
  PackKeysComboBox.Items.Add(_('Pack All'));

  RowFormatComboBox.Items.Clear;
  RowFormatComboBox.Items.Add(_('Default'));
  RowFormatComboBox.Items.Add(_('Dynamic'));
  RowFormatComboBox.Items.Add(_('Fixed'));
  RowFormatComboBox.Items.Add(_('Compressed'));

  RaidTypeComboBox.Items.Clear;
  RaidTypeComboBox.Items.Add(_('None'));
  RaidTypeComboBox.Items.Add(_('Striped'));

  FKOnDeleteComboBox.Items.Clear;
  FKOnDeleteComboBox.Items.Add(_('No Action'));
  FKOnDeleteComboBox.Items.Add(_('Cascade'));
  FKOnDeleteComboBox.Items.Add(_('Set Null'));
  FKOnDeleteComboBox.Items.Add(_('Restrict'));

  FKOnUpdateComboBox.Items.Assign(FKOnDeleteComboBox.Items);

  StdInsertsTabSheet.TabVisible := False;
  DescTabSheet.TabVisible := False;

  ShowDetails := True;
  HeightWithDetails := 580;
  HeightWithoutDetails := 448;

  ShowHideDetails(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FormDestroy(Sender: TObject);

begin
  if(FTableStatusList<>nil)then
  begin
    FTableStatusList.Free;
    FTableStatusList := nil;
  end;

  if(self.FSchemaTableList<>nil)then
  begin
    FSchemaTableList.Free;
    FSchemaTableList := nil;
  end;

  if(PServerVersion<>nil)then
    myx_dbm_free_server_version(PServerVersion);

  if(Charsets<>nil)then
    Charsets.Free;

  ColumnPNGImg.Free;
  ColumnFKPNGImg.Free;
  ColumnPKPNGImg.Free;

  CheckmarkPNGImg.Free;
  OptionsCheckboxPNGImg.Free;
  OptionsCheckboxCheckedPNGImg.Free;

  DatatypeNumericPNGImg.Free;
  DatatypeDateTimePNGImg.Free;
  DatatypeStringPNGImg.Free;
  DatatypeBlobPNGImg.Free;
  DatatypeSpatialPNGImg.Free;
  DatatypeUserdefinedPNGImg.Free;

  AssetIndexPNGImg.Free;
  AssetFKPNGImg.Free;

  IsNullPNGImg.Free;

  HeaderPNGImg.Free;

  FEngines.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  FreeTableData;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ClearCurrentVariables;

begin
  FCurrentColumn := nil;
  FCurrentIndex := nil;
  FCurrentFK := nil;
  FCurrentFKRefTableStatus := nil;
  FCurrentFKRefSchemaTable := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.Show;

begin
  if(MYXCommonOptions.EditorKeepRoutineEditorOnTop)then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.UpdateEngines;

var
  I: Integer;
  RawEngines: PMYX_ENGINES;

begin
  // Update engine list if the connection changed.
  FreeAndNil(FEngines);
  FInnoDbIsDefault := False;
  if Assigned(FConnection) then
  begin
    RawEngines := myx_get_engines(FConnection.MySQL);
    try
      FEngines := TMYX_ENGINES.Create(RawEngines);
      TableEngineLU.Items.Clear;
      for I := 0 to FEngines.engines.count - 1 do
      begin
        TableEngineLU.Items.Add(FEngines.engines[I].name);
        if SameText(FEngines.engines[I].name, 'InnoDB') and (FEngines.engines[I].isdefault = 1) then
          FInnoDBIsDefault := True;
      end;
    finally
      myx_free_engines(RawEngines);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetEditMode(EditMode: DBMEditMode; MySQLConn: TMySQLConn = nil);

begin
  FEditMode := EditMode;
  FConnection := MySQLConn;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetModified(Modified: Boolean);

begin
  if not FInitControls and not FSettingTableControls and not FSettingColumnControls and not FSettingIndexControls and
     not FSettingFKControls then
  begin
    FModified := Modified;

    ApplyChangesBtn.Enabled := FModified;
    DiscardChangesBtn.Enabled := FModified;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetDockLayout;

begin
  FDocked := True;

  DockedHeaderPnl.Visible := True;

  TopPnl.Height := 2;
  LeftPnl.Width := 8;
  RightPnl.Width := 8;
  BottomPnl.Height := 45;

  CloseBtn.Caption := _('Tables Overview');
  CloseBtn.Top := CloseBtn.Top-2;
  CloseBtn.Left := CloseBtn.Left+4;
  ApplyChangesBtn.Left := ApplyChangesBtn.Left+4;
  ApplyChangesBtn.Top := ApplyChangesBtn.Top-2;
  DiscardChangesBtn.Left := DiscardChangesBtn.Left+4;
  DiscardChangesBtn.Top := DiscardChangesBtn.Top-2;

  DetailsBtn.Top := DetailsBtn.Top-2;
  DetailsBtn.Left := DetailsBtn.Left-5;

  TableNameLbl.Left := TableNameLbl.Left-5;
  TableNameEd.Left := TableNameEd.Left-5;
  TableNameEd.Width := TableNameEd.Width+5;

  CommentEd.Width := CommentEd.Width+5;

  ColumnVST.Header.Columns[0].Width := 120;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEditorTableForm.EditStatusTable(catalog: WideString; schema: WideString; table: WideString;
  TableStatusList: TMYX_SCHEMA_TABLE_STATUS): Boolean;

begin
  Result := True;

  UpdateEngines;

  if Modified then
  begin
    if(ShowModalDialog(_('Discard changes'), _('Are you sure you want to discard your changes?'),
      myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 2) then
      Result := False
  end;

  if Result then
  begin
    ClearCurrentVariables;

    // If the TableStatusList is not nil, clone it.
    if Assigned(TableStatusList) then
    begin
      FTableStatusList := TMYX_SCHEMA_TABLE_STATUS.create(TableStatusList.get_record_pointer);

      FSettingTableControls := True;
      try
        RefreshFKTableComboBox;
      finally
        FSettingTableControls := False;
      end;
    end;

    RetrieveTableData(catalog, schema, table);
    if(table='')then
      ActiveControl := TableNameEd
    else
      ActiveControl := CloseBtn;
  end;
end;


//----------------------------------------------------------------------------------------------------------------------

function TEditorTableForm.EditSchemaTable(catalog: WideString; schema: WideString; table: WideString;
  SchemaTableList: TMYX_SCHEMA_TABLES): Boolean;

begin
  Result := True;
  if Modified then
  begin
    if ShowModalDialog(_('Discard changes'), _('Are you sure you want to discard your current changes?'),
      myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 2 then
      Result := False;
  end;

  UpdateEngines;

  if Result then
  begin
    ClearCurrentVariables;

    RetrieveTableData(catalog, schema, table);
    if table = '' then
      ActiveControl := TableNameEd
    else
      ActiveControl := CloseBtn;

    // If the SchemaTableList is not nil, clone it.
    if Assigned(SchemaTableList) then
    begin
      FSchemaTableList.Free;
      FSchemaTableList := TMYX_SCHEMA_TABLES.create(SchemaTableList.get_record_pointer);
      FSettingTableControls := True;
      try
        RefreshFKTableComboBox;
      finally
        FSettingTableControls := False;
      end;
    end;

  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.RetrieveTableData(catalog: WideString; schema: WideString; table: WideString);

var
  PTableData: PMYX_DBM_TABLE_DATA;
  error: MYX_LIB_ERROR;
  PCharsets: PMYX_DBM_CHARSETS;
  StorageEngine: PMYX_ENGINE;
  I: Integer;

begin
  if(PServerVersion=nil)then
    PServerVersion := myx_dbm_retrieve_server_version(FConnection.MySQL);

  if(Charsets=nil)then
  begin
    if(DBMajorVersion > 4) or ((DBMajorVersion = 4) and (DBMinorVersion >= 1)) then
      PCharsets := myx_dbm_retrieve_charsets(FConnection.MySQL, @error)
    else
      PCharsets := myx_charsets_load(MYXCommonOptions.XMLDir+
        'mysqlx_dbm_charsets.xml', @error);

    if(PCharsets=nil)or(error<>MYX_NO_ERROR)then
      raise EMyxCommonLibraryError.Create(_('Cannot fetch charset information.'),
        Ord(error), MYXCommonOptions.XMLDir+'mysqlx_dbm_datatypes.xml');

    try
      Charsets := TMYX_DBM_CHARSETS.create(PCharsets);
    finally
      myx_free_charsets(PCharsets);
    end;
  end;

  if table <> '' then
  begin
    PTableData := myx_dbm_retrieve_table_data(FConnection.MySQL, MYXCommonOptions.Datatypes, FEngines.get_record_pointer,
      catalog, schema, table, @error);
    if (PTableData = nil) or (error <> MYX_NO_ERROR) then
      raise EMyxSQLError.Create(_('Cannot fetch table information.'),
        myx_mysql_errno(FConnection.MySQL), myx_mysql_error(FConnection.MySQL));

    try
      SetTableData(TMYX_DBM_TABLE_DATA.create(PTableData));
    finally
      myx_dbm_free_table_data(PTableData);
    end;
  end
  else
  begin
    StorageEngine := nil;
    for I := 0 to FEngines.engines.count - 1 do
      if WideSameText(FEngines.engines[I].name, MYXCommonOptions.EditorTableDefaultStorageEngine) then
      begin
        StorageEngine := FEngines.engines[I].get_record_pointer;
        Break;
      end;

    SetTableData(TMYX_DBM_TABLE_DATA.create(_('New Table'), _('New Table'), schema, catalog, '', '', '', 0, '', '', '',
      '', MYX_DBM_TMI_NO, '', '', MYX_DBM_TPK_DEFAULT, MYX_DBM_TRT_NONE, '2', '64', 0, MYX_DBM_TRF_DEFAULT, '', '', '',
      '', StorageEngine));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FreeTableData;

begin
  Modified := False;

  ColumnVST.Clear;
  FreeAndNil(FTableData);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetTableData(TableData: TMYX_DBM_TABLE_DATA);

var
  I: Integer;
  NodeData: PColumnNodeData;
  Node: PVirtualNode;

begin
  FreeTableData;

  FSettingTableControls := True;
  try
    FTableData := TableData;
    TableNameEd.Text := TableData.name;

    DatabaseCBox.Items.Clear;
    DatabaseCBox.Items.Add(TableData.schema);
    DatabaseCBox.ItemIndex := 0;

    CommentEd.Text := TableData.comment;

    if Assigned(TableData.storage_engine) then
      TableEngineLU.ItemIndex := TableEngineLU.Items.IndexOf(TableData.storage_engine.name)
    else
      TableEngineLU.ItemIndex := -1;
    TableEngineLUChange(TableEngineLU);

    TableCharsetComboBox.Enabled := True;
    TableCharsetCaptionLbl.Enabled := TableCharsetComboBox.Enabled;
    TableCharsetDescLbl.Enabled := TableCharsetComboBox.Enabled;
    TableCollationComboBox.Enabled := ((DBMajorVersion = 4) and (DBMinorVersion >= 1)) or (DBMajorVersion >= 5);
    TableCollationCaptionLbl.Enabled := TableCollationComboBox.Enabled;
    TableCollationDescLbl.Enabled := TableCollationComboBox.Enabled;

    MergeRBtnClick(Self);

    UnionTablesEd.Text := TableData.merge_union;
    MergeInsertMethodComboBox.ItemIndex := Ord(TableData.merge_insert);

    FederatedConnectionEd.Text := TableData.federated_connection;

    PackKeysComboBox.ItemIndex := Ord(TableData.pack_keys);

    TablePasswordEd.Text := TableData.password;
    AutoIncEd.Text := TableData.next_auto_inc;

    DelayKeyUpdatesCBox.Checked := (TableData.delay_key_write=1);

    RowFormatComboBox.ItemIndex := Ord(TableData.row_format);
    UseChecksumCBox.Checked := (TableData.checksum=1);
    AvgRowLengthEd.Text := TableData.avg_row_length;
    MinRowsEd.Text := TableData.min_rows;
    MaxRowsEd.Text := TableData.max_rows;

    DataDirectoryEd.Text := TableData.table_data_dir;
    IndexDirectoryEd.Text := TableData.table_index_dir;

    RaidTypeComboBox.ItemIndex := Ord(TableData.raid_type);
    NumberOfChunksUpDown.Position := StrToIntDef(TableData.raid_chunks, 2);
    NumberOfChunksEd.Enabled := (RaidTypeComboBox.ItemIndex>0);
    NumberOfChunksUpDown.Enabled := NumberOfChunksEd.Enabled;
    NumberOfChunksCaptionLbl.Enabled := NumberOfChunksEd.Enabled;
    NumberOfChunksDescLbl.Enabled := NumberOfChunksEd.Enabled;

    ChunkSizeUpDown.Position := StrToIntDef(TableData.raid_chunk_size, 64);
    ChunkSizeEd.Enabled := (RaidTypeComboBox.ItemIndex>0);
    ChunkSizeUpDown.Enabled := ChunkSizeEd.Enabled;
    ChunkSizeCaptionEd.Enabled := ChunkSizeEd.Enabled;
    ChunkSizeDescLbl.Enabled := ChunkSizeEd.Enabled;
    ChunkSizeUnitLbl.Enabled := ChunkSizeEd.Enabled;

    // Fill charsets.
    TableCharsetComboBox.Items.Clear;
    if FTableData.name = _('New Table') then
      TableCharsetComboBox.Items.Add(_('Default'))
    else
      TableCharsetComboBox.Items.Add('');  // Add an empty string to keep the correct indices.
    for I := 0 to Charsets.charsets.Count - 1 do
    begin
      TableCharsetComboBox.Items.Add(Charsets.charsets[I].name);
      if SameText(Charsets.charsets[I].name, TableData.charset) then
      begin
        TableCharsetComboBox.ItemIndex := I + 1;
        TableCharsetComboBoxChange(Self);
      end;
    end;

    if(TableCharsetComboBox.ItemIndex=-1)then
      TableCharsetComboBox.ItemIndex := 0;

    //Set Column Charset and Collation
    if ((DBMajorVersion = 4) and (DBMinorVersion >= 1)) or (DBMajorVersion >= 5) then
      ColumnCharsetCBox.Items.Assign(TableCharsetComboBox.Items)
    else
    begin
      ColumnCharsetCBox.Clear;
      ColumnCollateCBox.Clear;

      ColumnCharsetCBox.Enabled := False;
      ColumnCollateCBox.Enabled := False;
    end;

    //Fill columns grid
    ColumnVST.NodeDataSize := sizeof(TColumnNodeData);

    if(TableData.columns.Count>=0)then
    begin
      for I := 0 to TableData.columns.Count-1 do
      begin
        Node := ColumnVST.AddChild(nil);
        NodeData := ColumnVST.GetNodeData(Node);
        NodeData.Column := TableData.columns[I];

        if(I=0)then
          SetCurrentColumn(TableData.columns[0]);
      end;
    end
    else
      SetCurrentColumn(nil);

    //Add node to make adding new rows possible
    Node := ColumnVST.AddChild(nil);
    NodeData := ColumnVST.GetNodeData(Node);
    NodeData.Column := nil;

    //Set focused and selected node
    ColumnVST.FocusedNode := ColumnVST.GetFirst;

    //Indices
    RefreshIndexVT;

    //FKs
    FKVST.BeginUpdate;
    try
      FKVST.Clear;
      FKVST.NodeDataSize := SizeOf(Pointer);

      if(TableData.fks<>nil)then
        if(TableData.fks.Count>0)then
        begin
          for I := 0 to TableData.fks.Count-1 do
          begin
            Node := FKVST.AddChild(nil, TableData.fks[I]);

            if(I=0)then
            begin
              FKVST.FocusedNode := Node;
              FKVST.Selected[Node] := True;
              SetCurrentFK(TableData.fks[I]);
            end;
          end;
        end
        else
          SetCurrentFK(nil);
    finally
      FKVST.EndUpdate;
    end;

  finally
    FSettingTableControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.RefreshIndexVT;

var
  I: Integer;
  Node: PVirtualNode;

begin
  IndexVST.BeginUpdate;
  try
    IndexVST.Clear;
    IndexVST.NodeDataSize := SizeOf(Pointer);

    if(FTableData.indices<>nil)then
      if(FTableData.indices.Count>0)then
      begin
        for I := 0 to FTableData.indices.Count-1 do
        begin
          Node := IndexVST.AddChild(nil, FTableData.indices[I]);

          if(I=0)then
          begin
            IndexVST.FocusedNode := Node;
            IndexVST.Selected[Node] := True;
            SetCurrentIndex(FTableData.indices[I]);
          end;
        end;
      end
      else
        SetCurrentIndex(nil);
  finally
    IndexVST.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetDatabaseVersion(DBMajorVersion: Integer; DBMinorVersion: Integer);

begin
  self.DBMajorVersion := DBMajorVersion;
  self.DBMinorVersion := DBMinorVersion;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetCurrentColumn(Column: TMYX_DBM_COLUMN_DATA);

var
  I: Integer;
  DataType: TMYX_DBM_DATATYPE;

begin
  FSettingColumnControls := True;
  try
    FCurrentColumn := Column;

    if(FCurrentColumn<>nil)then
    begin
      ColumnNameEd.Text := FCurrentColumn.name;
      ColumnDatatypeEd.Text := FCurrentColumn.datatype_name+
        FCurrentColumn.datatype_params;
      ColumnDefaultValueEd.Text := FCurrentColumn.default_value;

      ColumnPKCBox.Checked := (FCurrentColumn.primary_key=1);
      ColumnNotNullCBox.Checked := (FCurrentColumn.not_null=1);
      ColumnAutoIncCBox.Checked := (FCurrentColumn.auto_inc=1);

      ColumnFlagsCheckListBox.Items.Clear;
      if(FCurrentColumn.datatype_pointer<>nil)then
      begin
        DataType := TMYX_DBM_DATATYPE.create(FCurrentColumn.datatype_pointer);
        try
          ColumnFlagsCheckListBox.Items.Assign(DataType.flags);
          for I := 0 to DataType.flags.Count-1 do
            if(FCurrentColumn.datatype_flags.IndexOf(
              DataType.flags[I])>-1)then
            ColumnFlagsCheckListBox.Checked[I] := True;
        finally
          DataType.Free;
        end;
      end;

      ColumnCommentMemo.Text := FCurrentColumn.comment;

      SetCharset(ColumnCharsetCBox, FCurrentColumn.Charset);
    end
    else
    begin
      ColumnNameEd.Text := '';
      ColumnDatatypeEd.Text := '';
      ColumnDefaultValueEd.Text := '';

      ColumnPKCBox.Checked := False;
      ColumnNotNullCBox.Checked := False;
      ColumnAutoIncCBox.Checked := False;

      ColumnFlagsCheckListBox.Items.Clear;

      ColumnCommentMemo.Text := '';

      SetCharset(ColumnCharsetCBox, 'Default');
    end;
  finally
    FSettingColumnControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetCurrentIndex(Index: TMYX_DBM_INDEX_DATA);

var
  I: Integer;

begin
  FSettingIndexControls := True;
  try
    FCurrentIndex := Index;

    if(FCurrentIndex<>nil)then
    begin
      IndexNameEd.Text := FCurrentIndex.name;
      IndexKindCBox.ItemIndex := Ord(FCurrentIndex.index_kind);
      IndexTypeCBox.ItemIndex := Ord(FCurrentIndex.index_type);

      IndexNameEd.Enabled := (FCurrentIndex.index_kind<>MYX_DBM_IK_PRIMARY);
      IndexNameLbl.Enabled := IndexNameEd.Enabled;

      IndexKindCBox.Enabled := (FCurrentIndex.index_kind<>MYX_DBM_IK_PRIMARY);
      IndexKindLbl.Enabled := IndexKindCBox.Enabled;

      IndexTypeCBox.Enabled := ((DBMajorVersion=4)and(DBMinorVersion>=1))
          or (DBMajorVersion>4);
      IndexTypeLbl.Enabled := IndexTypeCBox.Enabled;


      IndexColumnsListBox.Items.Clear;
      for I := 0 to FCurrentIndex.columns.Count-1 do
      begin
        if(FCurrentIndex.columns[I].len<>'')then
          IndexColumnsListBox.Items.Add(FCurrentIndex.columns[I].name+
            '('+FCurrentIndex.columns[I].len+')')
        else
          IndexColumnsListBox.Items.Add(FCurrentIndex.columns[I].name);
      end;
    end
    else
    begin
      IndexNameEd.Text := '';
      IndexKindCBox.ItemIndex := 0;
      IndexTypeCBox.ItemIndex := 0;
      IndexColumnsListBox.Items.Clear;
    end;
  finally
    FSettingIndexControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetCurrentFK(FK: TMYX_DBM_FK_DATA);

var
  I, Index: Integer;
  NodeData: ^TMYX_NAME_VALUE_PAIR;

begin
  FSettingFKControls := True;
  try
    FCurrentFK := FK;
    if Assigned(FK) then
    begin
      FKNameEd.Text := FCurrentFK.name;
      FKOnDeleteComboBox.ItemIndex := Ord(FCurrentFK.on_delete);
      FKOnUpdateComboBox.ItemIndex := Ord(FCurrentFK.on_update);

      //Try to find Reference Table
      if FCurrentFK.reference_schema_name='' then
      begin
        Index := FKDestTablesComboBox.Items.IndexOf(FCurrentFK.reference_table_name);

        if Index > -1 then
          FKDestTablesComboBox.ItemIndex := Index
        else
        begin
          FKDestTablesComboBox.ItemIndex := -1;
          FKDestTablesComboBox.Text := FCurrentFK.reference_table_name;
        end;
      end
      else
      begin
        FKDestTablesComboBox.ItemIndex := -1;
        FKDestTablesComboBox.Text := FCurrentFK.reference_schema_name + '.' + FCurrentFK.reference_table_name;
      end;

      //Set CurrentFKRefTable
      if(FSchemaTableList<>nil)then
      begin
        if(FKDestTablesComboBox.ItemIndex>=0)and
          (FKDestTablesComboBox.ItemIndex<FSchemaTableList.schema_tables.Count)then
          FCurrentFKRefSchemaTable := FSchemaTableList.schema_tables[FKDestTablesComboBox.ItemIndex]
        else
          FCurrentFKRefSchemaTable := nil;
      end
      else if(FTableStatusList<>nil)then
      begin
        if(FKDestTablesComboBox.ItemIndex>=0)and
          (FKDestTablesComboBox.ItemIndex<FTableStatusList.schema_tables.Count)then
          FCurrentFKRefTableStatus := FTableStatusList.schema_tables[FKDestTablesComboBox.ItemIndex]
        else
          FCurrentFKRefTableStatus := nil;
      end;


      FKColsVST.BeginUpdate;
      try
        FKColsVST.Clear;
        FKColsVST.NodeDataSize := SizeOf(Pointer);
        for I := 0 to FCurrentFK.column_mapping.Count-1 do
          FKColsVST.AddChild(nil, FCurrentFK.column_mapping[I]);

        //Add another child for adding new rows
        NodeData := FKColsVST.GetNodeData(FKColsVST.AddChild(nil, nil));
        NodeData^ := nil;
      finally
        FKColsVST.EndUpdate;
      end;
    end
    else
    begin
      FKNameEd.Text := '';
      FKOnDeleteComboBox.ItemIndex := 0;
      FKOnUpdateComboBox.ItemIndex := 0;
      FKDestTablesComboBox.ItemIndex := -1;
      FCurrentFKRefSchemaTable := nil;
      FKColsVST.Clear;
    end;
  finally
    FSettingFKControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.RefreshFKTableComboBox;

var
  I: Integer;

begin
  FKDestTablesComboBox.Items.Clear;

  if(FSchemaTableList<>nil)then
  begin
    for I := 0 to FSchemaTableList.schema_tables.Count-1 do
      FKDestTablesComboBox.Items.Add(FSchemaTableList.schema_tables[I].table_name);
  end
  else if(FTableStatusList<>nil)then
  begin
    for I := 0 to FTableStatusList.schema_tables.Count-1 do
      FKDestTablesComboBox.Items.Add(FTableStatusList.schema_tables[I].table_name);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PColumnNodeData;

begin
  if(Node<>nil)then
  begin
    NodeData := ColumnVST.GetNodeData(Node);

    if(NodeData<>nil)then
    begin
      if(NodeData.Column<>nil)then
      begin
        if(Column=0)then
          CellText := NodeData.Column.name
        else if(Column=1)then
          CellText := NodeData.Column.datatype_name+
            NodeData.Column.datatype_params
        else if(Column=5)then
        begin
          if (NodeData.Column.default_value_is_null <> 1) then
            CellText := NodeData.Column.default_value
          else
            CellText := '';
        end
        else
          if Column = 6 then
          CellText := NodeData.Column.comment
        else
          CellText := '';
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if Column in [0, 1] then
    ImageIndex := 0
  else
    ImageIndex := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PColumnNodeData;
  I, Pos, Len: Integer;
  S: WideString;

begin
  if(Node<>nil)then
  begin
    NodeData := ColumnVST.GetNodeData(Node);

    if(NodeData<>nil)then
    begin
      if(NodeData.Column<>nil)then
      begin
        if(Column=0)then
          if(NodeData.Column.primary_key=1)then
            ColumnPKPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
              CellRect.Top+1, CellRect.Left+17, CellRect.Top+17))
          else
            ColumnPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
              CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));

        if(Column=1)then
        begin
          if(NodeData.Column.datatype_pointer<>nil)then
          begin
            case NodeData.Column.datatype_pointer.group of
              MYX_DBM_DTG_NUMERIC:
                DatatypeNumericPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                  CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
              MYX_DBM_DTG_DATETIME:
                DatatypeDateTimePNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                  CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
              MYX_DBM_DTG_STRING:
                DatatypeStringPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                  CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
              MYX_DBM_DTG_BLOB:
                DatatypeBlobPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                  CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
              MYX_DBM_DTG_SPATIAL:
                DatatypeSpatialPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                  CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
            else
              DatatypeUserdefinedPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
                CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
            end;
          end
          else
            DatatypeUserdefinedPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
              CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
        end;

        //Not null checkmark
        if(Column=2)and(NodeData.Column.not_null=1)then
          CheckmarkPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+5,
            CellRect.Top+1, CellRect.Left+21, CellRect.Top+17));

        //Auto Increment checkmark
        if(Column=3)and(NodeData.Column.auto_inc=1)then
          CheckmarkPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+5,
            CellRect.Top+1, CellRect.Left+21, CellRect.Top+17));

        if(Column=4)and(NodeData.Column.datatype_pointer<>nil)then
        begin
          Pos := 5;

          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          TargetCanvas.Font.Color := clBlack;

          for I := 0 to NodeData.Column.datatype_pointer.flags_num-1 do
          begin
            S := PPChar(Integer(NodeData.Column.datatype_pointer.flags)+sizeof(PChar)*I)^;

            if(NodeData.Column.datatype_flags.IndexOf(
              S)=-1)then
              OptionsCheckboxPNGImg.Draw(TargetCanvas,
                Rect(CellRect.Left+Pos, CellRect.Top+1,
                  CellRect.Left+16+Pos, CellRect.Top+17))
            else
              OptionsCheckboxCheckedPNGImg.Draw(TargetCanvas,
                Rect(CellRect.Left+Pos, CellRect.Top+1,
                  CellRect.Left+16+Pos, CellRect.Top+17));

            Pos := Pos+16+5;
            Len := GetWideStringTextWidth(TargetCanvas, S);

            DrawWideStringText(TargetCanvas.Handle,
              PWideChar(UTF8Decode(S)),
              Length(S),
              Rect(CellRect.Left+Pos, CellRect.Top+2,
                CellRect.Left+Pos+Len, CellRect.Top+16));

            Pos := Pos+Len+10;
          end;
        end;

        if (Column = 5) and (NodeData.Column.default_value_is_null = 1) then
        begin
          IsNullPNGImg.Draw(TargetCanvas,
            Rect(CellRect.Left + 2, CellRect.Top + 2,
              CellRect.Left + 2 + IsNullPNGImg.Width,
              CellRect.Top + 2 + IsNullPNGImg.Height));
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: PColumnNodeData;

begin
  if(Node<>nil)then
  begin
    NodeData := ColumnVST.GetNodeData(Node);

    if(NodeData<>nil)then
    begin
      if(NodeData.Column<>nil)then
        if(FCurrentColumn<>NodeData.Column)then
          SetCurrentColumn(NodeData.Column);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.MergeRBtnClick(Sender: TObject);

begin
  MergeOptionGBox.Enabled := (TableEngineLU.ItemIndex = TableEngineLU.Items.IndexOf('Merge'));
  UnionTablesEd.Enabled := MergeOptionGBox.Enabled;
  UnionTablesCaptionLbl.Enabled := UnionTablesEd.Enabled;
  UnionTablesDescLbl.Enabled := UnionTablesEd.Enabled;
  MergeInsertMethodComboBox.Enabled := MergeOptionGBox.Enabled;
  MergeInsertMethodCapionLbl.Enabled := MergeInsertMethodComboBox.Enabled;
  MergeInsertMethodDescLbl.Enabled := MergeInsertMethodComboBox.Enabled;
  FederatedConnectionEd.Enabled := (TableEngineLU.ItemIndex = TableEngineLU.Items.IndexOf('Federated'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.RefreshCollationCBox(CharsetComboBox: TTntComboBox; CollationComboBox: TTntComboBox;
  SelectCollation: WideString);

var
  I, J: Integer;
  Charset: TMYX_DBM_CHARSET;
  
begin
  CollationComboBox.Items.Clear;

  if CharsetComboBox.ItemIndex = 0 then
  begin
    if  CharsetComboBox.Text = _('Default') then
      CollationComboBox.Items.Add(_('Default'))
    else
      CollationComboBox.Items.Add('');
    CollationComboBox.ItemIndex := 0;
  end
  else
    if (CharsetComboBox.ItemIndex >= 0) and (CharsetComboBox.ItemIndex - 1 < Charsets.charsets.Count) then
    begin
      Charset := Charsets.charsets[CharsetComboBox.ItemIndex - 1];

      CollationComboBox.ItemIndex := -1;
      J := 0;
      for I := 0 to Charset.collations.Count-1 do
      begin
        CollationComboBox.Items.Add(Charset.collations[I].name);

        if(Charset.collations[I].is_default=1)then
          J := I;

        if(CompareText(SelectCollation, Charset.collations[I].name)=0)then
          CollationComboBox.ItemIndex := I;
      end;

      if(CollationComboBox.ItemIndex=-1)then
        CollationComboBox.ItemIndex := J;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.TableCharsetComboBoxChange(Sender: TObject);

begin
  RefreshCollationCBox(TableCharsetComboBox, TableCollationComboBox, FTableData.collation);
  Modified := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnCharsetCBoxCloseUp(Sender: TObject);

begin
  if(FCurrentColumn<>nil)then
    RefreshCollationCBox(ColumnCharsetCBox, ColumnCollateCBox, FCurrentColumn.collation);

  ColumnNameEdChange(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetCharset(CharsetComboBox: TTntComboBox; SelectCharset: WideString);

var
  I: Integer;

begin
  if Assigned(Charsets) then
  begin
    for I := 0 to Charsets.charsets.Count-1 do
      if SameText(Charsets.charsets[I].name, SelectCharset) then
      begin
        CharsetComboBox.ItemIndex := I + 1;
        if(Assigned(CharsetComboBox.OnCloseUp))then
          CharsetComboBox.OnCloseUp(self);

        Exit;
      end;

    CharsetComboBox.ItemIndex := 0;
    if Assigned(CharsetComboBox.OnCloseUp) then
      CharsetComboBox.OnCloseUp(self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetDatatype(name: WideString; PDatatypes: PMYX_DBM_DATATYPES): PMYX_DBM_DATATYPE;

var
  I: Integer;
  PDatatype: PMYX_DBM_DATATYPE;

begin
  if(PDatatypes<>nil)then
  begin
    for I := 0 to PDatatypes.datatypes_num-1 do
    begin
      PDatatype := PMYX_DBM_DATATYPE(Integer(PDatatypes.datatypes)+
        sizeof(MYX_DBM_DATATYPE)*I);

      if(CompareText(name, PDatatype.name)=0)then
      begin
        Result := PDatatype;
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.AssignDatatypeToColumn(Datatype: WideString; column: TMYX_DBM_COLUMN_DATA;
  PDatatypes: PMYX_DBM_DATATYPES);

begin
  if Pos('(', Datatype) < 1 then
  begin
    column.datatype_name := Datatype;
    column.datatype_params := '';
  end
  else
  begin
    column.datatype_name := Copy(Datatype, 1, Pos('(', Datatype)-1);
    column.datatype_params := Copy(Datatype, Pos('(', Datatype), Length(Datatype));
  end;

  column.datatype_pointer := GetDatatype(column.datatype_name, PDatatypes);
  if Assigned(column.datatype_pointer) then
  begin
    if (column.datatype_pointer.group <> MYX_DBM_DTG_NUMERIC) and (column.auto_inc = 1) then
      column.auto_inc := 0;
    if column.datatype_pointer.group <> MYX_DBM_DTG_STRING then
    begin
      column.charset := '';
      column.collation := '';
      RefreshCollationCBox(ColumnCharsetCBox, ColumnCollateCBox, '');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData, NewNodeData: PColumnNodeData;
  NewColumn: TMYX_DBM_COLUMN_DATA;
  NewPKColumn: Boolean;
  I, J: Integer;

begin
  if(Node<>nil)then
  begin
    NodeData := ColumnVST.GetNodeData(Node);
    NewPKColumn := False;

    if (NodeData<>nil) and
      ((Trim(NewText)<>'') or (Column=5) or (Column=6))then
    begin
      Modified := True;

      //Add new column
      if(NodeData.Column=nil)then
      begin
        //Add new column
        //make first column PK and AI
        if(FTableData.columns.Count=0)then
        begin
          NewColumn := TMYX_DBM_COLUMN_DATA.create('', '',
            nil,
            'Integer', '', '', '',
            1,
            1,
            1,
            '', 0,
            '');

          NewPKColumn := True;
        end
        else
          NewColumn := TMYX_DBM_COLUMN_DATA.create('', '',
            nil,
            'Integer', '', '', '',
            0,
            Ord(MYXCommonOptions.EditorTableAllColumnsNotNullPerDef),
            0,
            '', 0,
            '');


        //Add new column to TableData.columns
        FTableData.columns.Add(NewColumn);
        NodeData.Column := NewColumn;

        //Assign default datatypes for PK or normal column
        if(FTableData.columns.Count=1)then
          AssignDatatypeToColumn(MYXCommonOptions.EditorTablePKDataType,
            NodeData.Column, MYXCommonOptions.Datatypes)
        else
          AssignDatatypeToColumn(MYXCommonOptions.EditorTableDefColumnDataType,
            NodeData.Column, MYXCommonOptions.Datatypes);

        //Check EditorTableIntegerUnsignedPerDef
        if(MYXCommonOptions.EditorTableIntegerUnsignedPerDef)then
          if(NodeData.Column<>nil)then
            if((CompareText(NodeData.Column.datatype_name, 'TINYINT')=0)or
              (CompareText(NodeData.Column.datatype_name, 'SMALLINT')=0)or
              (CompareText(NodeData.Column.datatype_name, 'MEDIUMINT')=0)or
              (CompareText(NodeData.Column.datatype_name, 'INT')=0)or
              (CompareText(NodeData.Column.datatype_name, 'Integer')=0)or
              (CompareText(NodeData.Column.datatype_name, 'BIGINT')=0))then
              if(NodeData.Column.datatype_flags.IndexOf('UNSIGNED')=-1)then
                NodeData.Column.datatype_flags.Add('UNSIGNED');


        //Add node for adding new rows
        NewNodeData := ColumnVST.GetNodeData(ColumnVST.AddChild(nil));
        NewNodeData.Column := nil;

        FCurrentColumn := NewColumn;
      end;

      if(Column=0)then
      begin
        //Update indices
        for J := 0 to FTableData.indices.Count-1 do
            for I := 0 to FTableData.indices[J].columns.Count-1 do
              if (FTableData.indices[J].columns[I].name=
                NodeData.Column.name) then
                  FTableData.indices[J].columns[I].name := NewText;

        SetCurrentIndex(FCurrentIndex);

        //Update column
        NodeData.Column.name := NewText;
      end
      else if(Column=1)then
      begin
        AssignDatatypeToColumn(NewText, NodeData.Column, MYXCommonOptions.Datatypes);

        //Clear old datatype_flags
        NodeData.Column.datatype_flags.Clear;

        if(MYXCommonOptions.EditorTableIntegerUnsignedPerDef)then
          if((CompareText(NodeData.Column.datatype_name, 'TINYINT')=0)or
            (CompareText(NodeData.Column.datatype_name, 'SMALLINT')=0)or
            (CompareText(NodeData.Column.datatype_name, 'MEDIUMINT')=0)or
            (CompareText(NodeData.Column.datatype_name, 'INT')=0)or
            (CompareText(NodeData.Column.datatype_name, 'Integer')=0)or
            (CompareText(NodeData.Column.datatype_name, 'BIGINT')=0))then
            if(NodeData.Column.datatype_flags.IndexOf('UNSIGNED')=-1)then
              NodeData.Column.datatype_flags.Add('UNSIGNED');

        ColumnVST.InvalidateNode(Node);
      end
      else if(Column=5)then
      begin
        NodeData.Column.default_value_is_null := 0;
        if (CompareText(NewText, 'NULL')=0) or
          IsNumeric(NewText) or
          (CompareText(NewText, 'CURRENT_TIMESTAMP')=0) then
          NodeData.Column.default_value := NewText
        else
        begin
          // if the default value was empty before, add ''
          if (NodeData.Column.default_value = '') and
            (Copy(NewText, 1, 1) <> '''')then
            NodeData.Column.default_value := '''' +
              NewText + ''''
          else
            // if there was a value before, use the new text as it is
            // so the user might have removed the '' if he likes to
            NodeData.Column.default_value := NewText;
        end;
      end
      else if(Column=6)then
        NodeData.Column.comment := NewText;

    end;

    if(NewPKColumn)then
      UpdatePrimaryKeyIndex;

    SetCurrentColumn(FCurrentColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ConnectionEstablished(var Message: TMessage);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ConnectionLost(var Message: TMessage);

begin
  if Visible then
  begin
    ShowModalDialog(_('Server connection lost'), _('Lost server connection during table manipulation. Changes will be ' +
      'cancelled and the editor will be closed.'), myx_mtWarning, _('OK'));
    Modified := False;
    FreeTableData;
    Hide;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DoColumnGridEdit(var Message: TMessage);

begin
  if(FKColsVST.Handle=HWND(Message.WParam))then
  begin
    if(FKColsVST.FocusedNode<>nil)then
      FKColsVST.EditNode(FKColsVST.FocusedNode, FKColsVST.FocusedColumn)
  end
  else
  begin
    if(ColumnVST.FocusedNode=nil)then
      ColumnVST.FocusedNode := ColumnVST.GetFirst;

    if(ColumnVST.FocusedNode<>nil)then
      ColumnVST.EditNode(ColumnVST.FocusedNode, ColumnVST.FocusedColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DeleteSelectedColumns(Confirm: Boolean);

var
  Index: Integer;
  NodeData: PColumnNodeData;
  Run: PVirtualNode;
  IndexRun: PVirtualNode;
  IndexNodeData: TObject;
  IndexData: TMYX_DBM_INDEX_DATA;
  I: Integer;

begin
  // Delete nodes from TableData
  if ColumnVST.SelectedCount > 0 then
  begin
    if ShowModalDialog(_('Deleting Columns'), _('Are you sure you want to delete the selected columns?'),
      myx_mtConfirmation, _('Yes'#13#10'No')) = 1 then
    begin
      Run := ColumnVST.GetFirstSelected;
      while Assigned(Run) do
      begin
        NodeData := ColumnVST.GetNodeData(Run);
        Index := FTableData.columns.IndexOf(NodeData.Column);
        if Index > -1 then
        begin
          // Remove this column from all indices.
          IndexRun := IndexVST.GetFirst;
          while Assigned(IndexRun) do
          begin
            IndexNodeData := TObject(IndexVST.GetNodeData(IndexRun)^);
            if IndexNodeData is TMYX_DBM_INDEX_DATA then
            begin
              IndexData := TMYX_DBM_INDEX_DATA(IndexNodeData);
              for I := 0 to IndexData.columns.Count - 1 do
              begin
                if IndexData.columns[I].name = NodeData.Column.name then
                begin
                  IndexData.columns.Delete(I);

                  // Stop looking here. A column cannot be in an index more than once.
                  Break;
                end;
              end;
            end;
            IndexRun := IndexVST.GetNext(IndexRun);
          end;

          FTableData.columns.Delete(Index);
         end;

        Run := ColumnVST.GetNextSelected(Run);
      end;

      ColumnVST.DeleteSelectedNodes;

      // Update also the current index display in case the just deleted columns are shown there.
      IndexVSTFocusChanged(IndexVST, IndexVST.FocusedNode, 0);
      
      Modified := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_Return then
  begin
    if ColumnVST.FocusedNode = nil then
      ColumnVST.FocusedNode := ColumnVST.GetFirst;

    if not (tsEditing in ColumnVST.TreeStates) and Assigned(ColumnVST.FocusedNode) then
      ColumnVST.EditNode(ColumnVST.FocusedNode, ColumnVST.FocusedColumn);
  end
  else
    if Key = VK_Delete then
      DeleteSelectedColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

begin
  if(Column=0)or(Column=1)or(Column=5)or(Column=6)then
    Allowed := True
  else
    Allowed := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.TableNameEdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  Mgs: TMsg;

begin
  if(Key=VK_Return)then
  begin
    if(FTableData<>nil)then
      if(FTableData.columns<>nil)then
        if(FTableData.columns.Count=0)then
        begin
          ColumnVST.SetFocus;
          ColumnVST.FocusedNode := ColumnVST.GetFirst;
          ColumnVST.ClearSelection;

          if(ColumnVST.FocusedNode<>nil)then
          begin
            ColumnVST.Selected[ColumnVST.FocusedNode] := True;

            FColumnNameInitialText := ReplaceTags(
              MYXCommonOptions.EditorTablePKAutoNaming,
              '%tablename%='+TableNameEd.Text);

            ColumnVST.EditNode(ColumnVST.FocusedNode, 0);
          end;
        end;

    Key := 0;
    PeekMessage(Mgs, 0, WM_CHAR, WM_CHAR, PM_REMOVE);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  HitInfo: THitInfo;
  CLeft, CRight: Integer;
  NodeData: PColumnNodeData;
  xpos, I, len: Integer;
  FlagIndex: Integer;
  S: WideString;

begin
  ColumnVST.GetHitTestInfoAt(X, Y, True, HitInfo);
  if Assigned(HitInfo.HitNode) then
  begin
    NodeData := ColumnVST.GetNodeData(HitInfo.HitNode);

    // Switch Primary Key
    if HitInfo.HitColumn = 0 then
    begin
      ColumnVST.Header.Columns.GetColumnBounds(HitInfo.HitColumn, CLeft, CRight);

      if X < 16 then
      begin
        if Assigned(NodeData.Column) then
        begin
          NodeData.Column.primary_key := (NodeData.Column.primary_key + 1) mod 2;

          if NodeData.Column.primary_key = 1 then
            NodeData.Column.not_null := 1;

          ColumnVST.InvalidateNode(HitInfo.HitNode);
          UpdatePrimaryKeyIndex;
          Modified := True;
        end;
      end;
    end
    else
      if HitInfo.HitColumn = 4 then
      begin
        // Switch Datatype Flags
        ColumnVST.Header.Columns.GetColumnBounds(HitInfo.HitColumn, CLeft, CRight);

        xpos := 5;
        if Assigned(NodeData.Column) and Assigned(NodeData.Column.datatype_pointer) and
          Assigned(NodeData.Column.datatype_pointer.flags) then
        begin
          for I := 0 to NodeData.Column.datatype_pointer.flags_num - 1 do
          begin
            S := PPChar(Integer(NodeData.Column.datatype_pointer.flags) + sizeof(PChar) * I)^;
            len := GetWideStringTextWidth(ColumnVST.Canvas, S);
            if (X >= CLeft + xpos) and (X <= CLeft + xpos + 16 + 5 + len + 10) then
            begin
              FlagIndex := NodeData.Column.datatype_flags.IndexOf(S);
              if FlagIndex = -1 then
              begin
                //Special processing for CHAR
                if SameText(NodeData.Column.datatype_name, 'CHAR') then
                begin
                  if SameText(S, 'ASCII') then
                    if NodeData.Column.datatype_flags.IndexOf('UNICODE') > -1 then
                      NodeData.Column.datatype_flags.Delete(NodeData.Column.datatype_flags.IndexOf('UNICODE'));
                  if SameText(S, 'UNICODE') then
                    if NodeData.Column.datatype_flags.IndexOf('ASCII') > -1 then
                      NodeData.Column.datatype_flags.Delete(NodeData.Column.datatype_flags.IndexOf('ASCII'));
                end;

                NodeData.Column.datatype_flags.Add(S);
              end
              else
                NodeData.Column.datatype_flags.Delete(FlagIndex);

              ColumnVST.InvalidateNode(HitInfo.HitNode);
              SetCurrentColumn(FCurrentColumn);
              Modified := True;
              Break;
            end;

            xpos := xpos + 16 + 5 + len + 10;
          end;
        end;
      end
      else
        if HitInfo.HitColumn = 2 then
        begin
          if Assigned(NodeData.Column) then
          begin
            NodeData.Column.not_null := Ord(not Boolean(NodeData.Column.not_null));
            if Boolean(NodeData.Column.not_null) then
              NodeData.Column.default_value_is_null := 0;

            Modified := True;
          end;

          ColumnVST.InvalidateNode(HitInfo.HitNode);
          SetCurrentColumn(FCurrentColumn);
        end
        else
          if HitInfo.HitColumn = 3 then
          begin
            if Assigned(NodeData.Column) then
            begin
              if NodeData.Column.auto_inc = 0 then
              begin
                for I := 0 to FTableData.columns.Count - 1 do
                  FTableData.columns[I].auto_inc := 0;

                NodeData.Column.auto_inc := 1;
                NodeData.Column.not_null := 1;
                NodeData.Column.default_value := '';
              end
              else
                NodeData.Column.auto_inc := 0;

              Modified := True;
            end;

            ColumnVST.InvalidateNode(HitInfo.HitNode);
          end;

    SetCurrentColumn(FCurrentColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  EditLink := TColumnGridEditLink.Create(MYXCommonOptions.Datatypes, FColumnNameInitialText);
  FColumnNameInitialText := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.CloseBtnClick(Sender: TObject);

begin
  if(Assigned(FEditorClose))then
    FEditorClose(self)
  else
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnNameEdChange(Sender: TObject);

var
  I: Integer;
  InvalidateAll: Boolean;
  OldDataType: Pointer;

begin
  if(not(FSettingColumnControls))and(not(FSettingTableControls))and
    (FCurrentColumn<>nil)and(ColumnVST.FocusedNode<>nil)then
  begin
    Modified := True;

    InvalidateAll := False;

    FCurrentColumn.name := ColumnNameEd.Text;

    OldDataType := FCurrentColumn.datatype_pointer;
    AssignDatatypeToColumn(ColumnDatatypeEd.Text, FCurrentColumn, MYXCommonOptions.Datatypes);
    if(OldDataType<>FCurrentColumn.datatype_pointer)then
    begin

    end;

    FCurrentColumn.default_value := ColumnDefaultValueEd.Text;
    if (ColumnDefaultValueEd.Text <> '') then
      FCurrentColumn.default_value_is_null := 0;


    FCurrentColumn.primary_key := Ord(ColumnPKCBox.Checked);
    FCurrentColumn.not_null := Ord(ColumnNotNullCBox.Checked);

    //If this column is the auto_inc, clear auto_inc flag in all other cols
    if(FCurrentColumn.auto_inc=0)and(ColumnAutoIncCBox.Checked)then
    begin
      for I := 0 to FTableData.columns.Count-1 do
        FTableData.columns[I].auto_inc := 0;

      InvalidateAll := True;
    end;

    FCurrentColumn.auto_inc := Ord(ColumnAutoIncCBox.Checked);

    FCurrentColumn.datatype_flags.Clear;
    for I := 0 to ColumnFlagsCheckListBox.Items.Count-1 do
      if(ColumnFlagsCheckListBox.Checked[I])then
        FCurrentColumn.datatype_flags.Add(ColumnFlagsCheckListBox.Items[I]);

    if(ColumnCharsetCBox.ItemIndex=0)then
    begin
      FCurrentColumn.charset := '';
      FCurrentColumn.collation := '';
    end
    else
    begin
      FCurrentColumn.charset := ColumnCharsetCBox.Text;
      FCurrentColumn.collation := ColumnCollateCBox.Text;
    end;

    FCurrentColumn.comment := ColumnCommentMemo.Text;

    if(Not(InvalidateAll))then
      ColumnVST.InvalidateNode(ColumnVST.FocusedNode)
    else
      ColumnVST.Invalidate;

    UpdatePrimaryKeyIndex;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ColumnsPageControlChange(Sender: TObject);

begin
  if ColumnsPageControl.ActivePage = FKTabSheet then
  begin
    if SameText(TableEngineLU.Text, 'InnoDB') or ((TableEngineLU.ItemIndex = -1) and FInnoDbIsDefault) then
      FKTabsheet.Enabled := True
    else
    begin
      FKTabsheet.Enabled := False;
      ShowModalDialog(_('Unsupported feature'), _('Foreign keys are currently only supported for the InnoDB storage engine.') +
        #13#10 + _('Your foreign key definitions will be ignored.'), myx_mtWarning, _('OK'));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DetailsBtnClick(Sender: TObject);

begin
  ShowDetails := Not(ShowDetails);
  ShowHideDetails;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ShowHideDetails(StoreHeight: Boolean);

begin
  if(ShowDetails)then
  begin
    DetailsBtn.Caption := '<< Details';

    if(StoreHeight)then
      HeightWithoutDetails := Height;
    Height := HeightWithDetails;

    ColumnOptionSplitter.Visible := True;
    ColumnsPageControl.Visible := True;
    ColumnOptionSplitter.Top := 0;
    AdvTableOptionsSheet.TabVisible := True;
  end
  else
  begin
    DetailsBtn.Caption := 'Details >>';

    if(StoreHeight)then
      HeightWithDetails := Height;
    Height := HeightWithoutDetails;

    ColumnOptionSplitter.Visible := False;
    ColumnsPageControl.Visible := False;
    AdvTableOptionsSheet.TabVisible := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: Pointer;

begin
  NodeData := Sender.GetNodeData(Node);
  if(NodeData<>nil)then
  begin
    if(TObject(NodeData^) is TMYX_DBM_INDEX_DATA)then
      CellText := TMYX_DBM_INDEX_DATA(NodeData^).name
    else if(TObject(NodeData^) is TMYX_DBM_FK_DATA)then
      CellText := TMYX_DBM_FK_DATA(NodeData^).name;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  ImageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: Pointer;

begin
  if(Node<>nil)then
  begin
    NodeData := Sender.GetNodeData(Node);

    if(NodeData<>nil)then
    begin
      if(TObject(NodeData^) is TMYX_DBM_INDEX_DATA)then
      begin
        AssetIndexPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
          CellRect.Top+1, CellRect.Left+17, CellRect.Top+17))
      end
      else if(TObject(NodeData^) is TMYX_DBM_FK_DATA)then
      begin
        AssetFKPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+1,
          CellRect.Top+1, CellRect.Left+17, CellRect.Top+17))
      end
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexVSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: Pointer;

begin
  if(Node<>nil)then
  begin
    NodeData := Sender.GetNodeData(Node);

    if(NodeData<>nil)then
    begin
      if(TObject(NodeData^) is TMYX_DBM_INDEX_DATA)then
        SetCurrentIndex(TMYX_DBM_INDEX_DATA(NodeData^))
      else if(TObject(NodeData^) is TMYX_DBM_FK_DATA)then
        SetCurrentFK(TMYX_DBM_FK_DATA(NodeData^));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexColumnsListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

begin
  if(Button=mbLeft)then
  begin
    FIndexDragStartRow := IndexColumnsListBox.ItemAtPos(Point(X, Y), True);
    if(FIndexDragStartRow>-1)then
      IndexColumnsListBox.BeginDrag(False, 5);
  end
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexColumnsListBoxDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

begin
  if(Source=Sender)or(Source=ColumnVST)then
    Accept := True
  else
    Accept := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexColumnsListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  NewPos: Integer;
  S: WideString;
  IndexColumn: TMYX_DBM_INDEX_COLUMN_DATA;

begin
  // Drag from column grid
  if Source is TVirtualStringTree then
  begin
    if TVirtualStringTree(Source).Name = 'ColumnVST' then
      AddIndexColumnBtnClick(Self);
  end
  else
    // Drag from column list itself
    if Source is TTntListBox then
    begin
      if TTntListBox(Source).Name = 'IndexColumnsListBox' then
      begin
        with IndexColumnsListBox do
        begin
          NewPos := ItemAtPos(Point(X, Y), True);

          if (NewPos > -1) and (NewPos <> FIndexDragStartRow) then
          begin
            S := Items[FIndexDragStartRow];

            Items.Delete(FIndexDragStartRow);
            Items.Insert(NewPos, S);

            IndexColumn := TMYX_DBM_INDEX_COLUMN_DATA.create(FCurrentIndex.columns[FIndexDragStartRow].get_record_pointer);
            try
              FCurrentIndex.columns.Delete(FIndexDragStartRow);
              FCurrentIndex.columns.Insert(NewPos, TMYX_DBM_INDEX_COLUMN_DATA.create(IndexColumn.get_record_pointer));
            finally
              IndexColumn.Free;
            end;

            IndexColumnsListBox.Refresh;
          end;

          Modified := True;
        end;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexColumnsListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if Key = VK_Delete then
    DeleteIndexColumnBtnClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DeleteIndexColumnBtnClick(Sender: TObject);

var
  I, J: Integer;

begin
  if Assigned(FCurrentIndex) then
  begin
    for I := IndexColumnsListBox.Items.Count - 1 downto 0 do
      if IndexColumnsListBox.Selected[I] then
      begin
        // Change column PK flag.
        if FCurrentIndex.index_kind = MYX_DBM_IK_PRIMARY then
        begin
          for J := 0 to FTableData.columns.Count-1 do
            if(CompareText(FTableData.columns[J].name,
              FCurrentIndex.columns[I].name)=0)then
            begin
              FTableData.columns[J].primary_key := 0;
              ColumnVST.Invalidate;
              break;
            end;
        end;

        FCurrentIndex.columns.Delete(I);
      end;

    IndexColumnsListBox.DeleteSelected;
    Modified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.AddIndexColumnBtnClick(Sender: TObject);

var
  I, NewPos: Integer;
  Node: PVirtualNode;
  NodeData: PColumnNodeData;
  AlreadyAdded: Boolean;

begin
  if FCurrentIndex = nil then
    FCurrentIndex := AddIndex;

  if Assigned(FCurrentIndex) then
  begin
    Node := ColumnVST.GetFirstSelected;
    while Assigned(Node) do
    begin
      NodeData := ColumnVST.GetNodeData(Node);
      if Assigned(NodeData.Column) then
      begin
        AlreadyAdded := False;
        for I := 0 to FCurrentIndex.columns.Count - 1 do
          if CompareText(NodeData.Column.name, FCurrentIndex.columns[I].name) = 0 then
          begin
            AlreadyAdded := True;
            Break;
          end;

        if not AlreadyAdded then
        begin
          NewPos := FCurrentIndex.columns.Add(TMYX_DBM_INDEX_COLUMN_DATA.create(NodeData.Column.name, '', ''));
          IndexColumnsListBox.Items.Add(FCurrentIndex.columns[NewPos].name);
          Modified := True;

          if FCurrentIndex.index_kind = MYX_DBM_IK_PRIMARY then
          begin
            NodeData.Column.primary_key := 1;
            ColumnVST.Invalidate;
          end;

          Modified := True;
        end;
      end;

      Node := ColumnVST.GetNextSelected(Node);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.AdvIndexColumnBtnClick(Sender: TObject);

begin
  IndexColumnPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetIndexColumnLengthMIClick(Sender: TObject);

var
  S: WideString;
  I: Integer;

begin
  if(FCurrentIndex<>nil)then
  begin
    if ShowModalEditDialog(_('Index Column Length'), _('Enter the number of character that are used for the index.'),
      myx_mtEdit, _('OK') + #13#10 + _('Cancel'), True, 'Length:', S) = 1 then
    begin
      S := IntToStr(Abs(StrToIntDef(S, 0)));
      if S = '0' then
        S := '';

      for I := 0 to IndexColumnsListBox.Items.Count - 1 do
        if(IndexColumnsListBox.Selected[I])then
        begin
          FCurrentIndex.columns[I].len := S;
          IndexColumnsListBox.Items[I] := FCurrentIndex.columns[I].name + '(' + S + ')';
        end;

      Modified := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEditorTableForm.AddIndex: TMYX_DBM_INDEX_DATA;

var
  S: WideString;
  Index: TMYX_DBM_INDEX_DATA;
  Node: PVirtualNode;

begin
  Index := nil;

  S := ReplaceTags(MYXCommonOptions.EditorTableIndexAutoNaming,
    '%tablename%='+TableNameEd.Text+#13#10+
    '%nr%='+IntToStr(FTableData.indices.Count+1));

  if(ShowModalEditDialog(_('Add Index'),
    _('Please enter the name of the new index.'),
    myx_mtEdit, 'OK'#13#10'Cancel',
    True, 'Index Name:', S)=1)then
  begin
    Index := TMYX_DBM_INDEX_DATA.create(S, '', MYX_DBM_IK_INDEX, MYX_DBM_IT_DEFAULT);
    try
      FTableData.indices.Add(Index);
      FCurrentIndex := Index;

      Node := IndexVST.AddChild(nil, Index);

      IndexVST.FocusedNode := Node;
      IndexVST.ClearSelection;
      IndexVST.Selected[Node] := True;
      SetCurrentIndex(Index);

      Modified := True;
    except
      on x: Exception do
      begin
        Index.Free;
        raise;
      end;
    end;
  end;

  Result := Index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.AddIndexBtnClick(Sender: TObject);

begin
  AddIndex;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexNameEdChange(Sender: TObject);

begin
  if(FCurrentIndex<>nil)then
    FCurrentIndex.name := IndexNameEd.Text;

  Modified := True;
  IndexVST.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexKindCBoxChange(Sender: TObject);

var
  I: Integer;

begin
  if(FCurrentIndex<>nil)then
  begin
    //There can only be one PRIMARY index
    if(IndexKindCBox.ItemIndex=1)then
    begin
      for I := 0 to FTableData.indices.Count-1 do
        if(FTableData.indices[I].index_kind=MYX_DBM_IK_PRIMARY)then
        begin
          ShowModalDialog(_('Only one Primary Key'),
            _('A primary key has already been defined.'),
            myx_mtError, _('OK'));

          IndexKindCBox.ItemIndex := Ord(FCurrentIndex.index_kind);

          break;
        end;
    end;

    case IndexKindCBox.ItemIndex of
      1:
        FCurrentIndex.index_kind := MYX_DBM_IK_PRIMARY;
      2:
        FCurrentIndex.index_kind := MYX_DBM_IK_UNIQUE;
      3:
        FCurrentIndex.index_kind := MYX_DBM_IK_FULLTEXT;
      4:
        FCurrentIndex.index_kind := MYX_DBM_IK_SPATIAL;
    else
      FCurrentIndex.index_kind := MYX_DBM_IK_INDEX;
    end;

    Modified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.IndexTypeCBoxChange(Sender: TObject);

begin
  if Assigned(FCurrentIndex) then
  begin
    //case IndexKindCBox.ItemIndex of
    case IndexTypeCBox.ItemIndex of
      1:
        FCurrentIndex.index_type := MYX_DBM_IT_BTREE;
      2:
        FCurrentIndex.index_type := MYX_DBM_IT_HASH;
      3:
        FCurrentIndex.index_type := MYX_DBM_IT_RTREE;
    else
      FCurrentIndex.index_type := MYX_DBM_IT_DEFAULT;
    end;

    Modified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DeleteIndexBtnClick(Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;
  IndexNodeData: ^TMYX_DBM_INDEX_DATA;

begin
  Selection := IndexVST.GetSortedSelection(False);

  IndexVST.BeginUpdate;
  try
    for I := 0 to IndexVST.SelectedCount-1 do
    begin
      IndexNodeData := IndexVST.GetNodeData(Selection[I]);
      FTableData.indices.Remove(IndexNodeData^);
    end;

    IndexVST.DeleteSelectedNodes;

    SetCurrentIndex(nil);

    Modified := True;
  finally
    IndexVST.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.AddFKBtnClick(Sender: TObject);

var
  S: WideString;
  FK: TMYX_DBM_FK_DATA;
  Node: PVirtualNode;

begin
  S := ReplaceTags(MYXCommonOptions.EditorTableFKAutoNaming,
    '%tablename%='+TableNameEd.Text+#13#10+
    '%nr%='+IntToStr(FTableData.fks.Count+1));

  if(ShowModalEditDialog(_('Add Foreign Key'),
    _('Please enter the name of the new foreign key.'),
    myx_mtEdit, 'OK'#13#10'Cancel',
    True, 'Foreign Key Name:', S)=1)then
  begin
    FK := TMYX_DBM_FK_DATA.create(S, '', '', '',
      MYX_DBM_FA_RESTRICT, MYX_DBM_FA_RESTRICT);
    try
      FTableData.fks.Add(FK);
      FCurrentFK := FK;

      Node := FKVST.AddChild(nil, FK);

      FKVST.ClearSelection;
      FKVST.FocusedNode := Node;
      FKVST.Selected[Node] := True;
      SetCurrentFK(FK);

      Modified := True;
    except
      on x: Exception do
      begin
        FK.Free;
        raise;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: ^TMYX_NAME_VALUE_PAIR;
  
begin
  NodeData := Sender.GetNodeData(Node);
  if(NodeData<>nil)then
    if(NodeData^<>nil)then
    begin
      if(Column=0)then
        CellText := NodeData.name
      else if(Column=1)then
        CellText := NodeData.value;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: ^TMYX_NAME_VALUE_PAIR;
  NewNameValue: TMYX_NAME_VALUE_PAIR;

begin
  NodeData := Sender.GetNodeData(Node);
  if(NodeData<>nil)then
  begin
    if(NodeData^=nil)and(FCurrentFK<>nil)then
    begin
      NewNameValue := TMYX_NAME_VALUE_PAIR.create('', '');
      FCurrentFK.column_mapping.Add(NewNameValue);
      NodeData^ := NewNameValue;
      Sender.AddChild(nil);
    end;

    if(Column=0)then
      NodeData.name := NewText
    else if(Column=1)then
      NodeData.value := NewText;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

begin
  if(Source=ColumnVST)and(FCurrentFK<>nil)then
    Accept := True
  else
    Accept := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  if(FCurrentFKRefTableStatus<>nil)then
    EditLink := TFKGridEditLink.Create('', FTableData, FCurrentFKRefTableStatus)
  else if(FCurrentFKRefSchemaTable<>nil)then
    EditLink := TFKGridEditLink.Create('', FTableData, FCurrentFKRefSchemaTable)
  else
    EditLink := TStringEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKDestTablesComboBoxCloseUp(Sender: TObject);

var
  I, J: Integer;
  ColumnName: WideString;

begin
  if(Not(FSettingFKControls))then
  begin
    if(FKDestTablesComboBox.ItemIndex>=0)then
    begin
      if(FTableStatusList<>nil)and(FCurrentFK<>nil)then
      begin
        if(FKDestTablesComboBox.ItemIndex<FTableStatusList.schema_tables.Count)then
        begin
          FCurrentFKRefTableStatus := FTableStatusList.schema_tables[FKDestTablesComboBox.ItemIndex];
          FCurrentFK.reference_table_name := FCurrentFKRefTableStatus.table_name;

          FCurrentFK.column_mapping.Clear;
          for I := 0 to FCurrentFKRefTableStatus.columns.Count-1 do
            if(FCurrentFKRefTableStatus.columns[I].primary_key=1)then
            begin
              ColumnName := '';
              for J := 0 to FTableData.columns.Count-1 do
                if(CompareText(FCurrentFKRefTableStatus.columns[I].column_name,
                  FTableData.columns[J].name)=0)then
                begin
                  ColumnName := FTableData.columns[J].name;
                  break;
                end;

              FCurrentFK.column_mapping.Add(TMYX_NAME_VALUE_PAIR.create(
                ColumnName,
                FCurrentFKRefTableStatus.columns[I].column_name));
            end;

          if(Not(FSettingFKControls))then
            SetCurrentFK(FCurrentFK);

          Modified := True;
        end;
      end
      else if(FSchemaTableList<>nil)and(FCurrentFK<>nil)then
      begin
        if(FKDestTablesComboBox.ItemIndex<FSchemaTableList.schema_tables.Count)then
        begin
          FCurrentFKRefSchemaTable := FSchemaTableList.schema_tables[FKDestTablesComboBox.ItemIndex];
          FCurrentFK.reference_table_name := FCurrentFKRefSchemaTable.table_name;

          FCurrentFK.column_mapping.Clear;
          for I := 0 to FCurrentFKRefSchemaTable.columns.Count-1 do
            if(FCurrentFKRefSchemaTable.columns[I].primary_key=1)then
            begin
              ColumnName := '';
              for J := 0 to FTableData.columns.Count-1 do
                if(CompareText(FCurrentFKRefSchemaTable.columns[I].column_name,
                  FTableData.columns[J].name)=0)then
                begin
                  ColumnName := FTableData.columns[J].name;
                  break;
                end;

              FCurrentFK.column_mapping.Add(TMYX_NAME_VALUE_PAIR.create(
                ColumnName,
                FCurrentFKRefSchemaTable.columns[I].column_name));
            end;

          if(Not(FSettingFKControls))then
            SetCurrentFK(FCurrentFK);

          Modified := True;
        end;
      end
      else
      begin
        FCurrentFKRefTableStatus := nil;
        FCurrentFKRefSchemaTable := nil;

        SetCurrentFK(FCurrentFK);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, FKColsVST.Handle, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ValueChanged(Sender: TObject);

begin
  Modified := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ShowMySQLMessagesInDialog(MySQL: Pointer);

var
  PMsgs: PMYX_MYSQL_ERROR_MSGS;
  PMsg: PMYX_MYSQL_ERROR_MSG;
  I: Integer;
  m: string;
  ws: WideString; 

begin
  PMsgs := myx_mysql_error_msgs_fetch(MySQL);

  if (PMsgs <> nil) then
  begin
    for I := 0 to PMsgs.errors_num - 1 do
    begin
      PMsg := PMYX_MYSQL_ERROR_MSG(Integer(PMsgs.errors) +
        sizeof(MYX_MYSQL_ERROR_MSG) * I);

      if(PMsg.level = MYX_QEL_NOTE) then
      begin
        m := m + #13#10 + 'Note: ' + PMsg.text;
      end
      else if(PMsg.level = MYX_QEL_WARNING) then
      begin
        m := m + #13#10 + 'Warning: ' + PMsg.text;
      end
      else if(PMsg.level = MYX_QEL_ERROR) then
      begin
        m := m + #13#10 + 'Error: ' + '(' + string(PMsg.error) + ') ' + PMsg.text;
      end;
    end;

    ws := m;
    ShowModalEditDialog(_('Server Messages'),
          'Server Messages                                           '+
          '                                                          ',
          myx_mtInformation, _('OK'),
          True, '', ws, 15, True);

    myx_mysql_error_msgs_free(PMsgs);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.ApplyChangesBtnClick(Sender: TObject);

var
  PTableData: PMYX_DBM_TABLE_DATA;
  Error: MYX_LIB_ERROR;
  Sql: WideString;
  UserResult: Integer;
  DoApplyChanges: Boolean;
  I: Integer;
  SavedCursor: TCursor;
  
begin
  if (FTableData.columns.Count = 0) then
  begin
    ShowModalDialog(_('Error'),
      _('You have to add at least one column to the table'),
      myx_mtError, _('OK'));

    Exit;
  end;

  // Apply changes to TableData.
  FTableData.name := TableNameEd.Text;
  FTableData.schema := DatabaseCBox.Text;
  FTableData.catalog := '';

  if TableEngineLU.ItemIndex > -1 then
    // Duplicate the engine entry. It must be independantly deletable.
    FTableData.storage_engine := TMYX_ENGINE.Create(FEngines.engines[TableEngineLU.ItemIndex].get_record_pointer);

  FTableData.next_auto_inc := AutoIncEd.Text;
  FTableData.password := TablePasswordEd.Text;
  FTableData.delay_key_write := Ord(DelayKeyUpdatesCBox.Checked);

  if TableCharsetComboBox.ItemIndex > 0 then
  begin
    FTableData.charset := TableCharsetComboBox.Text;
    FTableData.collation := TableCollationComboBox.Text;
  end
  else
  begin
    FTableData.charset := '';
    FTableData.collation := '';
  end;

  FTableData.comment := CommentEd.Text;

  FTableData.merge_union := UnionTablesEd.Text;
  case MergeInsertMethodComboBox.ItemIndex of
    0:
      FTableData.merge_insert := MYX_DBM_TMI_NO;
    1:
      FTableData.merge_insert := MYX_DBM_TMI_FIRST;
    2:
      FTableData.merge_insert := MYX_DBM_TMI_LAST;
  end;

  FTableData.table_data_dir := DataDirectoryEd.Text;
  FTableData.table_index_dir := IndexDirectoryEd.Text;

  case RaidTypeComboBox.ItemIndex of
    0:
      FTableData.raid_type := MYX_DBM_TRT_NONE;
    1:
      FTableData.raid_type := MYX_DBM_TRT_STRIPED;
  end;
  FTableData.raid_chunks := NumberOfChunksEd.Text;
  FTableData.raid_chunk_size := ChunkSizeEd.Text;

  FTableData.checksum := Ord(UseChecksumCBox.Checked);
  case RowFormatComboBox.ItemIndex of
    0:
      FTableData.row_format := MYX_DBM_TRF_DEFAULT;
    1:
      FTableData.row_format := MYX_DBM_TRF_DYNAMIC;
    2:
      FTableData.row_format := MYX_DBM_TRF_FIXED;
    3:
      FTableData.row_format := MYX_DBM_TRF_COMPRESSED;
  end;
  FTableData.avg_row_length := AvgRowLengthEd.Text;
  FTableData.min_rows := MinRowsEd.Text;
  FTableData.max_rows := MaxRowsEd.Text;

  case PackKeysComboBox.ItemIndex of
    0:
      FTableData.pack_keys := MYX_DBM_TPK_DEFAULT;
    1:
      FTableData.pack_keys := MYX_DBM_TPK_NONE;
    2:
      FTableData.pack_keys := MYX_DBM_TPK_ALL;
  end;

  FTableData.federated_connection := FederatedConnectionEd.Text;

  // Remove foreign keys if anything but InnoDB as engine is set.
  if (FTableData.storage_engine = nil) and not FInnoDbIsDefault or
    Assigned(FTableData.storage_engine) and not SameText(FTableData.storage_engine.name, 'InnoDB') then
    FTableData.fks.Clear;

  // If the auto increment check box is checked then we cannot allow a default value.
  // Not even an empty value. Adjust the the corresponding fields to avoid
  // creation of a DEFAULT clause.
  if ColumnAutoIncCBox.Checked then
    FCurrentColumn.default_value_is_null := 1;

  //If in DBMEditType_Online mode, apply changes to table directly
  if(FEditMode=DBMEditMode_Online)then
  begin
    //Get current table
    PTableData := myx_dbm_retrieve_table_data(FConnection.MySQL, MYXCommonOptions.Datatypes, FEngines.get_record_pointer,
      FTableData.catalog, FTableData.schema, FTableData.original_name, @Error);
    if (Error <> MYX_NO_ERROR) and (Error <> MYX_OBJECT_NOT_FOUND) then
      raise EMyxSQLError.Create(_('Cannot fetch table information for diff.'),
        myx_mysql_errno(FConnection.MySQL), myx_mysql_error(FConnection.MySQL));

    try
      Sql := myx_dbm_get_table_sql_diff(PTableData,
        FTableData.get_record_pointer, PServerVersion, @Error);

      if(Error<>MYX_NO_ERROR)and(Error<>MYX_OBJECT_NOT_FOUND)then
        raise EMyxSQLError.Create(_('Cannot generate the diff between the edited and the existing table.'),
          myx_mysql_errno(FConnection.MySQL), myx_mysql_error(FConnection.MySQL));
    finally
      myx_dbm_free_table_data(PTableData);
    end;

    DoApplyChanges := False;

    if(Sql<>'')then
    begin
      if(MYXCommonOptions.EditorTableShowSQLBeforeApplying)then
      begin
        UserResult := ShowModalEditDialog(_('Confirm Table Edit'),
          _('Are you sure you want to execute the following SQL command to '+
            'apply the changes to the table?'),
          myx_mtConfirmation, _('Execute'#13#10'Cancel'),
          True, '', Sql, 15, True);

        //Execute Script
        if(UserResult=1)then
          DoApplyChanges := True;
      end
      else
        DoApplyChanges := True;
    end
    else
      ShowModalDialog(_('No Changes'),
        _('The changes you made did not result in the need to alter the table.'),
        myx_mtInformation, _('OK'));

    if DoApplyChanges then
    begin
      SavedCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        if FConnection.ExecuteDirectMulti(Sql, 5000) then
        begin
          ShowMySQLMessagesInDialog(FConnection.MySQL);
          //Reset original names
          FTableData.original_name := FTableData.name;
          for I := 0 to FTableData.columns.Count-1 do
          begin
            FTableData.columns[I].original_name :=
              FTableData.columns[I].name;
          end;
          for I := 0 to FTableData.indices.Count-1 do
          begin
            FTableData.indices[I].original_name :=
              FTableData.indices[I].name;
          end;
          for I := 0 to FTableData.fks.Count-1 do
          begin
            FTableData.fks[I].original_name :=
              FTableData.fks[I].name;
          end;

          if(Assigned(FApplyedChanges))then
            FApplyedChanges(self);

          RetrieveTableData(FTableData.catalog, FTableData.schema, FTableData.name);
          Modified := False;
        end;
      finally
        Screen.Cursor := SavedCursor;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DeleteTableColumnsMIClick(Sender: TObject);

begin
  DeleteSelectedColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKOnDeleteComboBoxChange(Sender: TObject);

begin
  if Assigned(FCurrentFK) then
  begin
    case FKOnDeleteComboBox.ItemIndex of
      1:
        FCurrentFK.on_delete := MYX_DBM_FA_CASCADE;
      2:
        FCurrentFK.on_delete := MYX_DBM_FA_SET_NULL;
      3:
        FCurrentFK.on_delete := MYX_DBM_FA_RESTRICT;
    else
      FCurrentFK.on_delete := MYX_DBM_FA_NO_ACTION;
    end;

    case FKOnUpdateComboBox.ItemIndex of
      1:
        FCurrentFK.on_update := MYX_DBM_FA_CASCADE;
      2:
        FCurrentFK.on_update := MYX_DBM_FA_SET_NULL;
      3:
        FCurrentFK.on_update := MYX_DBM_FA_RESTRICT;
    else
      FCurrentFK.on_update := MYX_DBM_FA_NO_ACTION;
    end;

    Modified := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKNameEdChange(Sender: TObject);

begin
  if(FCurrentFK<>nil)then
    FCurrentFK.name := FKNameEd.Text;

  Modified := True;
  FKVST.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  CanClose := False;

  if(Modified)then
  begin
    if(ShowModalDialog(_('Discard changes'),
      _('Are you sure you want to quit the table editor without '+
        'applying the changes?'),
      myx_mtConfirmation, _('Yes')+#13#10+_('No'))=1)then
    begin
      FreeTableData;
      Hide;
    end;
  end
  else
  begin
    FreeTableData;
    Hide;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DiscardChangesBtnClick(Sender: TObject);

var
  WasUndone: Boolean;

begin
  WasUndone := False;
  if Assigned(FTableStatusList) then
    WasUndone := EditStatusTable(FTableData.catalog, FTableData.schema, FTableData.original_name, nil)
  else
    if Assigned(FSchemaTableList) then
      WasUndone := EditSchemaTable(FTableData.catalog, FTableData.schema, FTableData.original_name, nil);

  if WasUndone then
    Modified := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKColsVSTDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

var
  NodeData: PColumnNodeData;
  Selection: TNodeArray;
  NodeDataFK: ^TMYX_NAME_VALUE_PAIR;
  I: Integer;
  
begin
  Selection := nil;
  
  //Drag from column grid
  if(Source.ClassNameIs('TVirtualStringTree'))and
    (FCurrentFK<>nil)then
  begin
    if(TVirtualStringTree(Source).Name='ColumnVST')then
    begin
      Selection := ColumnVST.GetSortedSelection(False);

      NodeDataFK := nil;

      if(FKColsVST.DropTargetNode<>nil)then
        NodeDataFK := FKColsVST.GetNodeData(FKColsVST.DropTargetNode);

      if(NodeDataFK<>nil)then
      begin
        if(ColumnVST.SelectedCount>0)then
        begin
          NodeData := ColumnVST.GetNodeData(Selection[0]);
          if(NodeData<>nil)then
          begin
            NodeDataFK := FKColsVST.GetNodeData(FKColsVST.DropTargetNode);
            if(NodeDataFK<>nil)then
            begin
              if(NodeDataFK^<>nil)then
              begin
                NodeDataFK.name := NodeData.Column.name;
              end;
            end;
          end;
        end;
      end
      else
      begin
        for I := 0 to ColumnVST.SelectedCount-1 do
        begin
          NodeData := ColumnVST.GetNodeData(Selection[I]);
          FCurrentFK.column_mapping.Add(TMYX_NAME_VALUE_PAIR.create(NodeData.Column.name, ''));
        end;

        SetCurrentFK(FCurrentFK);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.DropFKBtnClick(Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;
  FKNodeData: ^TMYX_DBM_FK_DATA;

begin
  Selection := FKVST.GetSortedSelection(False);

  FKVST.BeginUpdate;
  try
    for I := 0 to FKVST.SelectedCount-1 do
    begin
      FKNodeData := FKVST.GetNodeData(Selection[I]);
      FTableData.fks.Remove(FKNodeData^);
    end;

    FKVST.DeleteSelectedNodes;

    SetCurrentFK(nil);

    Modified := True;
  finally
    FKVST.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.CreateParams(var Params: TCreateParams);

begin
  inherited CreateParams(Params);

  Params.exStyle := Params.exStyle or WS_EX_APPWINDOW;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.UpdatePrimaryKeyIndex;

var
  I, J: Integer;
  PKIndex: TMYX_DBM_INDEX_DATA;
  IndexColCount: Integer;
  Found: Boolean;

begin
  PKIndex := nil;
  IndexColCount := 0;

  //Find PK Index if it already has been created
  for I := 0 to FTableData.indices.Count-1 do
    if(FTableData.indices[I].index_kind=MYX_DBM_IK_PRIMARY)then
    begin
      PKIndex := FTableData.indices[I];
      break;
    end;

  //Scan columns to find all in PK
  for I := 0 to FTableData.columns.Count-1 do
  begin
    if(FTableData.columns[I].primary_key=1)then
    begin
      inc(IndexColCount);

      //Create the PK index if not already existing
      if(PKIndex=nil)then
      begin
        PKIndex := TMYX_DBM_INDEX_DATA.create('PRIMARY', '',
          MYX_DBM_IK_PRIMARY, MYX_DBM_IT_DEFAULT);

        FTableData.indices.Insert(0, PKIndex);
      end;

      Found := False;
      for J := 0 to PKIndex.columns.Count-1 do
        if(WideSameText(PKIndex.columns[J].name,
          FTableData.columns[I].name))then
        begin
          Found := True;
          break;
        end;

      if(Not(Found))then
        PKIndex.columns.Add(TMYX_DBM_INDEX_COLUMN_DATA.create(
          FTableData.columns[I].name, '', 'ASC'));
    end;
  end;

  //Scan to remove columns no longer index
  if(PKIndex<>nil)then
  begin
    I := 0;
    while(I<PKIndex.columns.Count)do
    begin
      Found := False;
      for J := 0 to FTableData.columns.Count-1 do
        if(WideSameText(PKIndex.columns[I].name,
          FTableData.columns[J].name))and
          (FTableData.columns[J].primary_key=1)then
        begin
          Found := True;
          break;
        end;

      if(Not(Found))then
        PKIndex.columns.Delete(I)
      else
        inc(I);
    end;
  end;

  //Remove PK Index if there are no columns left
  if(IndexColCount=0)and(PKIndex<>nil)then
    if(FTableData.indices.IndexOf(PKIndex)>=0)then
      FTableData.indices.Delete(FTableData.indices.IndexOf(PKIndex));

  RefreshIndexVT;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.FKDestTablesComboBoxChange(Sender: TObject);

var
  NewName,
  NewSchemaName,
  NewTableName: WideString;

begin
  if (Not(FSettingFKControls)) and (FCurrentFK<>nil) then
  begin
    NewSchemaName := '';
    NewTableName := '';

    NewName := FKDestTablesComboBox.Text;
    if (Pos('.', NewName)=-1) then
    begin
      NewTableName := NewName;
    end
    else
    begin
      NewSchemaName := Copy(NewName, 1, Pos('.', NewName)-1);
      NewTableName := Copy(NewName, Pos('.', NewName)+1, MaxInt);
    end;

    if (Not(WideSameStr(FCurrentFK.reference_table_name, NewTableName))) or
      (Not(WideSameStr(FCurrentFK.reference_schema_name, NewSchemaName))) then
    begin
      FCurrentFK.reference_schema_name := NewSchemaName;
      FCurrentFK.reference_table_name := NewTableName;

      Modified := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.RemoveFKColumnMIClick(Sender: TObject);

var
  Selection: TNodeArray;
  NodeDataFK: ^TMYX_NAME_VALUE_PAIR;
  I: Integer;

begin
  Selection := FKColsVST.GetSortedSelection(False);

  for I := FKColsVST.SelectedCount-1 downto 0 do
  begin
    NodeDataFK := FKColsVST.GetNodeData(Selection[I]);
    FKColsVST.DeleteNode(Selection[I]);
    if(NodeDataFK<>nil)then
      if(NodeDataFK^<>nil)then
      begin
        // column_mapping owns the objects in its list, so it frees them on removal.
        FCurrentFK.column_mapping.Remove(NodeDataFK^);
        NodeDataFK^ := nil;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.SetDefValNullBtnClick(Sender: TObject);

begin
  if(not(FSettingColumnControls))and(not(FSettingTableControls))and
    (FCurrentColumn<>nil)and(ColumnVST.FocusedNode<>nil)then
  begin
    if (FCurrentColumn.default_value_is_null = 0) then
    begin
      ColumnNotNullCBox.State := cbUnchecked;
      FCurrentColumn.not_null := 0;

      FCurrentColumn.default_value_is_null := 1;
      ColumnDefaultValueEd.Text := '';
      Modified := True;

      ColumnVST.InvalidateNode(ColumnVST.FocusedNode);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TEditorTableForm.MainPageControlChange(Sender: TObject);

begin
  FInitControls := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.MainPageControlChanging(Sender: TObject; var AllowChange: Boolean);

begin
  FInitControls := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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

procedure TEditorTableForm.TableEngineLUChange(Sender: TObject);

begin
  if TableEngineLU.ItemIndex = -1 then
    TableEngineDescLbl.Caption := _('Nothing selected')
  else
  begin
    TableEngineDescLbl.Caption := _(FEngines.engines[TableEngineLU.ItemIndex].Description);

    if FEngines.engines[TableEngineLU.ItemIndex].enabled = 0 then
      ShowModalDialog(_('Disabled storage engine'), _('The storage engine you selected is not enabled currently.'),
        myx_mtWarning, _('OK'));

    if not SameText(TableEngineLU.Text, 'InnoDB') and (FTableData.fks.Count > 0) then
    begin
      FKTabsheet.Enabled := False;
      if ColumnsPageControl.ActivePage = FKTabsheet then
        ColumnsPageControl.ActivePage := IndexTabsheet;

      ShowModalDialog(_('Unsupported feature'), _('You have foreign keys defined but selected a storage engine which does not support them.') +
        #13#10 + _('These definitions will be ignored.'), myx_mtWarning, _('OK'));
    end;
  end;
  
  Modified := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TEditorTableForm.TableEngineLUDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  DC: HDC;
  S: WideString;

begin
  DC := TableEngineLU.Canvas.Handle;
  if FEngines.engines[Index].enabled = 0 then
    SetTextColor(DC, ColorToRGB(clRed))
  else
    if odSelected in State then
      SetTextColor(DC, ColorToRGB(clHighlightText))
    else
      SetTextColor(DC, ColorToRGB(clWindowText));

  S := FEngines.engines[Index].name;
  InflateRect(Rect, -2, 0);
  ExtTextOutW(DC, Rect.Left, Rect.Top, ETO_OPAQUE, @Rect, PWideChar(S), Length(S), nil);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

