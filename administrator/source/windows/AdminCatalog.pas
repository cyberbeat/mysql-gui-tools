unit AdminCatalog;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, InstanceSections, Buttons,
  Contnrs, Menus, AuxFuncs, myx_public_interface, AdvancedEdit,
  SchemataTreeView, MySQLConnection, AdditionalClasses,
  AdminCatalogTableCheck, Options, MyxError,
  EditorTable, TntMenus, VirtualTrees, PNGImage, ImgList,
  CommonFuncs, Math, TntExtCtrls, TntComCtrls, TntStdCtrls, TntForms,
  TntClasses, MySQLCommonFuncs, EditorSql, SyncObjs, myx_util_public_interface;

type
  TAdminCatalogForm = class(TInstanceSectionForm)
    CatalogSubTreePnl: TTntPanel;
    Splitter1: TTntSplitter;
    CatalogPnl: TTntPanel;
    CatalogPageControl: TTntPageControl;
    CatalogSheet: TTabSheet;
    SchemaSheet: TTabSheet;
    TopPnl: TTntPanel;
    SchemaPageControl: TTntPageControl;
    TablesTabSheet: TTabSheet;
    IndicesTabSheet: TTabSheet;
    StoredProcTabSheet: TTabSheet;
    DevDocTabSheet: TTabSheet;
    UsersTabSheet: TTabSheet;
    Panel3: TTntPanel;
    Panel5: TTntPanel;
    TablesTitleLbl: TTntLabel;
    TableHeaderImg: TTntImage;
    Panel1: TTntPanel;
    IndicesTitleLbl: TTntLabel;
    IndexHeaderImg: TTntImage;
    Panel2: TTntPanel;
    StoredProcBevel: TTntBevel;
    StoredProcTitleLbl: TTntLabel;
    Image2: TTntImage;
    Panel4: TTntPanel;
    DevDocBevel: TTntBevel;
    DevObjTitleLbl: TTntLabel;
    Image4: TTntImage;
    Panel6: TTntPanel;
    UsersBevel: TTntBevel;
    UsersTitleLbl: TTntLabel;
    Image5: TTntImage;
    Panel7: TTntPanel;
    Panel10: TTntPanel;
    Panel11: TTntPanel;
    EditSPBtn: TTntButton;
    AddSPBtn: TTntButton;
    DelSPBtn: TTntButton;
    RefreshSPBtn: TTntButton;
    DevDocTreeView: TTntTreeView;
    DevDocTreeViewHeaderControl: THeaderControl;
    Panel12: TTntPanel;
    Panel13: TTntPanel;
    Button10: TTntButton;
    Button11: TTntButton;
    Button12: TTntButton;
    Button13: TTntButton;
    UsersListView: TTntListView;
    TablesOptionsPageControl: TTntPageControl;
    TabSheet2: TTabSheet;
    GroupBox2: TTntGroupBox;
    Label27: TTntLabel;
    RowFormatLbl: TTntLabel;
    TableTypeLbl: TTntLabel;
    Label24: TTntLabel;
    Label40: TTntLabel;
    AutoIncLbl: TTntLabel;
    Label48: TTntLabel;
    CreateOptionsLbl: TTntLabel;
    ViewsTabSheet: TTabSheet;
    Panel15: TTntPanel;
    Panel16: TTntPanel;
    EditViewBtn: TTntButton;
    CreateViewBtn: TTntButton;
    DropViewBtn: TTntButton;
    RefreshViewsBtn: TTntButton;
    SelectSchemaLbl: TTntLabel;
    TabSheet3: TTabSheet;
    GroupBox3: TTntGroupBox;
    Label28: TTntLabel;
    RowsLbl: TTntLabel;
    AvgRowLengthLbl: TTntLabel;
    Label31: TTntLabel;
    Label32: TTntLabel;
    DataLengthLbl: TTntLabel;
    Label34: TTntLabel;
    MaxDataLenLbl: TTntLabel;
    Label36: TTntLabel;
    IndexLenLbl: TTntLabel;
    Label38: TTntLabel;
    DataFreeLbl: TTntLabel;
    TableTimeGBox: TTntGroupBox;
    Label2: TTntLabel;
    TblCreateTimeLbl: TTntLabel;
    Label4: TTntLabel;
    TblUpdateTimeLbl: TTntLabel;
    Label6: TTntLabel;
    TblCheckTimeLbl: TTntLabel;
    EventsTabSheet: TTabSheet;
    Panel17: TTntPanel;
    EventsBevel: TTntBevel;
    EventsTitleLbl: TTntLabel;
    Image7: TTntImage;
    TablesSchemaLbl: TTntLabel;
    IndicesSchemaLbl: TTntLabel;
    SchemataFrame: TSchemataFrame;
    ListView1: TTntListView;
    ThreadsBevel: TTntBevel;
    TableInfoSplitter: TTntSplitter ;
    CheckTablesBtn: TTntButton;
    RefreshBtn: TTntButton;
    Label1: TTntLabel;
    Label9: TTntLabel;
    AddTableBtn: TTntButton;
    Button1: TTntButton;
    TablesPopupMenu: TTntPopupMenu;
    EditTableMI: TTntMenuItem;
    CreateTableMI: TTntMenuItem;
    N1: TTntMenuItem;
    DropTableMI: TTntMenuItem;
    OptimizeTableMI: TTntMenuItem;
    CheckTableMI: TTntMenuItem;
    RepairTableMI: TTntMenuItem;
    N2: TTntMenuItem;
    RefreshMI: TTntMenuItem;
    TableDetailsBtn: TTntButton;
    EditTableBtn: TTntButton;
    TableMaintenanceMI: TTntMenuItem;
    TablesVST: TVirtualStringTree;
    TableSumPnl: TTntPanel;
    Image3: TTntImage;
    NumberOfTablesLbl: TTntLabel;
    NumberOfTablesCaptionLbl: TTntLabel;
    RowsCountLbl: TTntLabel;
    RowsCountCaptionLbl: TTntLabel;
    Image8: TTntImage;
    Image9: TTntImage;
    TableDataLenCaptionLbl: TTntLabel;
    TableDataLenLbl: TTntLabel;
    TableIndexLenCaptionLbl: TTntLabel;
    TableIndexLenLbl: TTntLabel;
    Image10: TTntImage;
    AssetSheet: TTabSheet;
    TablenamePnl: TTntPanel;
    TablenameLbl: TTntLabel;
    TableImg: TTntImage;
    TableCommentLbl: TTntLabel;
    Label3: TTntLabel;
    IndicesVST: TVirtualStringTree;
    Bevel1: TTntBevel;
    RefreshIndicesBtn: TTntButton;
    Panel8: TTntPanel;
    Image1: TTntImage;
    Image11: TTntImage;
    Image12: TTntImage;
    NumberOfIndicesLbl: TTntLabel;
    NumberOfIndicesCaptionLbl: TTntLabel;
    IndexColCountLbl: TTntLabel;
    Label10: TTntLabel;
    Image13: TTntImage;
    Label11: TTntLabel;
    IndexUniqueCountLbl: TTntLabel;
    Label13: TTntLabel;
    IndexNotNullCountLbl: TTntLabel;
    EditTableDataMI: TTntMenuItem;
    ViewsVST: TVirtualStringTree;
    StoredProcsVST: TVirtualStringTree;
    Panel14: TTntPanel;
    ViewsBevel: TTntBevel;
    ViewsTitleLbl: TTntLabel;
    Image6: TTntImage;
    ViewsSchemaLbl: TTntLabel;
    TntLabel2: TTntLabel;
    StoredProcSchemaLbl: TTntLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CatalogPnlResize(Sender: TObject);

    procedure SchemaPageControlChange(Sender: TObject);
    procedure DevDocTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure DevDocTreeViewHeaderControlSectionResize(
      HeaderControl: THeaderControl; Section: THeaderSection);

    procedure BuildSchemaPages(Schema: TMYX_SCHEMA = nil);
    procedure SchemataTreeAdvEditSeachEdChange(Sender: TObject);
    procedure SchemataFrameSchemaTreeViewChange(Sender: TObject;
      Node: TTntTreeNode);
    procedure TablesOptionsPageControlChange(Sender: TObject);

    procedure FetchSchemaTableStatus(Sender: TObject);
    procedure FetchedSchemaTableStatus(Sender: TObject);
    procedure FetchSchemaViewStatus(Sender: TObject);
    procedure FetchedSchemaViewStatus(Sender: TObject);
    procedure FetchSchemaSPStatus(Sender: TObject);
    procedure FetchedSchemaSPStatus(Sender: TObject);

    procedure FetchSchemaSPBody(Sender: TObject);
    procedure FetchedSchemaSPBody(Sender: TObject);
    procedure DropSchemaSP(Sender: TObject);
    procedure DroppedSchemaSP(Sender: TObject);
    procedure FetchSchemaViewDef(Sender: TObject);
    procedure FetchedSchemaViewDef(Sender: TObject);
    procedure DropSchemaView(Sender: TObject);
    procedure DroppedSchemaView(Sender: TObject);

    procedure ClearTableInfos;
    procedure ClearControls;

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure SchemaListChanged(var Message: TMessage); message WM_SchemaListChanged;
    procedure CheckTablesBtnClick(Sender: TObject);
    procedure SchemataFrameRefreshCatalogsSchemataListMIClick(
      Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure EditTableMIClick(Sender: TObject);
    procedure CreateTableMIClick(Sender: TObject);

    procedure ShowHideTableDetails;
    procedure TableDetailsBtnClick(Sender: TObject);
    procedure TablesVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure ViewsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure StoredProcsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TablesVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure ViewsVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure StoredProcsVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TablesVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure ViewsVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure StoredProcsVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure TablesVSTDblClick(Sender: TObject);
    procedure TablesVSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TablesVSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TablesVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure ViewsVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure StoredProcsVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure TablesVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TablesVSTAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      ItemRect: TRect);
    procedure PaintNodeBKBarGraph(TargetCanvas: TCanvas; Node: PVirtualNode; Column: Integer; ItemRect: TRect;
      ValueWidth: Integer; PenColor: TColor; BrushColor: TColor);

    procedure EditTable(Catalog: WideString; Schema: WideString; Table: WideString);
    procedure EditorTableClose(Sender: TObject);
    procedure DropTableMIClick(Sender: TObject);
    procedure IndicesVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure IndicesVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellRect: TRect);
    procedure IndicesVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure IndicesVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IndicesVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);

    procedure EditorTableApplyedChanges(Sender: TObject);
    procedure SchemataFrameSchemaTreeViewChanging(Sender: TObject; Node: TTntTreeNode; var AllowChange: Boolean);
    procedure EditTableDataMIClick(Sender: TObject);
    procedure DoSchemaTreeReloaded(Sender: TObject);
    procedure SchemataFrameCatalogVSTFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure SchemataFrameCatalogVSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure SchemataFrameCreateNewSchemaMIClick(Sender: TObject);
    procedure SchemataFrameCopyAssetSQLMIClick(Sender: TObject);
    procedure AddSPBtnClick(Sender: TObject);
    procedure RefreshSPBtnClick(Sender: TObject);
    procedure EditSPBtnClick(Sender: TObject);
    procedure DelSPBtnClick(Sender: TObject);
    procedure CreateViewBtnClick(Sender: TObject);
    procedure EditViewBtnClick(Sender: TObject);
    procedure DropViewBtnClick(Sender: TObject);
    procedure RefreshViewsBtnClick(Sender: TObject);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure TablesVSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure SchemataFrameDropMIClick(Sender: TObject);
    procedure TableInfoSplitterPaint(Sender: TObject);
    procedure ViewsVSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure SchemataFrameSchemaTreeViewPopupMenuPopup(Sender: TObject);
    procedure StoredProcsVSTDblClick(Sender: TObject);
    procedure ViewsVSTDblClick(Sender: TObject);
  private
    CurrentSchema: TMYX_SCHEMA;
    PrevTableList,
    CurrentTableList: TMYX_SCHEMA_TABLE_STATUS;
    PrevViewList,
    CurrentViewList: TMYX_SCHEMA_VIEW_STATUS;
    PrevSPList,
    CurrentSPList: TMYX_SCHEMA_STORED_PROCEDURES;
    CurrentSchemaDataLen,
    CurrentSchemaIndexLen: int64;

    ShowTableDetails: Boolean;

    procEditData: WideString;
    procEditData2: WideString;
    procEditType: MYX_SCHEMA_STORED_PROCEDURE_TYPE;

    procDropData: array of WideString;
    procDropType: array of MYX_SCHEMA_STORED_PROCEDURE_TYPE;

    viewEditData: WideString;
    viewEditData2: WideString;
    viewDropData: array of WideString;

    TreeBtnClosedPNGImg,
    TreeBtnOpenPNGImg,
    TablePNGImg,
    TableHeaderPNGImg,
    ViewPNGImg,
    StoredProcPNGImg,
    IndexPNGImg,
    IndexHeaderPNGImg,
    ColumnPNGImg,
    ColumnPKPNGImg: TPNGObject;

    TreeSpacerImgList: TImageList;
    function FetchMySQLMessages: WideString;
  public
    AdminCatalogTableCheckForm: TAdminCatalogTableCheckForm;

    EditorTableForm: TEditorTableForm;
    EditorSqlForm: TEditorSqlForm;
  end;

  PIndexNodeData= ^IndexNodeData;
  IndexNodeData = record
    Table: TMYX_TABLE_STATUS;
    Index: TMYX_TABLE_INDEX;
    Column: TMYX_TABLE_INDEX_COLUMN;
  end;

  PViewNodeData= ^ViewNodeData;
  ViewNodeData = record
    View: TMYX_VIEW_STATUS;
  end;

  PSPNodeData= ^SPNodeData;
  SPNodeData = record
    Proc: TMYX_SCHEMA_STORED_PROCEDURE;
  end;

var
  AdminCatalogForm: TAdminCatalogForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main, ApplicationDataModule, PNGTools, Themes;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  SchemataFrame.MySQLConnection := MySQLConn;
  SchemataFrame.ShowSchemaAssets := False;
  SchemataFrame.FetchTables := False;
  SchemataFrame.SearchTargets := [smSchemata];

  DockedPanel := CatalogPnl;
  SubTreePanel := CatalogSubTreePnl;

  //Init PageControls
  CatalogPageControl.ActivePageIndex := 0;
  SchemaPageControl.ActivePageIndex := 0;
  TablesOptionsPageControl.ActivePageIndex := 0;

  //Hide CatalogPageControl Buttons
  TopPnl.Height := 9;

  CurrentSchema := nil;
  CurrentTableList := nil;
  CurrentViewList := nil;
  CurrentSPList := nil;

  EditorTableForm := nil;
  EditorSqlForm := nil;

  AdminCatalogTableCheckForm := nil;

  // For MySQL Versions < 5 hide Views, StoredProcedures and Events.
  ViewsTabSheet.TabVisible := MySQLConn.MajorVersion > 4;
  StoredProcTabSheet.TabVisible := MySQLConn.MajorVersion > 4;

  // Events are not yet implemented.
  EventsTabSheet.TabVisible := False; //(MySQLConn.MajorVersion > 5) or ((MySQLConn.MajorVersion = 5) and (MySQLConn.MinorVersion > 0));
  DevDocTabSheet.TabVisible := False;
  UsersTabSheet.TabVisible := False;

  SchemataFrame.FillSchemaTree;
  SchemataFrame.OnCreateTable := CreateTableMIClick;
  SchemataFrame.OnSchemaTreeReloaded := DoSchemaTreeReloaded;

  CurrentSchemaDataLen := 0;

  TreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');
  TreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  TablePNGImg := LoadPNGImageFromResource('asset_table_16x16', TableImg);
  TableHeaderPNGImg := LoadPNGImageFromResource('asset_table', TableHeaderImg);
  ViewPNGImg := LoadPNGImageFromResource('asset_view_16x16');
  StoredProcPNGImg := LoadPNGImageFromResource('asset_sp_16x16');
  IndexPNGImg := LoadPNGImageFromResource('asset_index_16x16');
  IndexHeaderPNGImg := LoadPNGImageFromResource('asset_index', IndexHeaderImg);
  ColumnPNGImg := LoadPNGImageFromResource('column');
  ColumnPKPNGImg := LoadPNGImageFromResource('column_pk');


  //Disable Users for now
  DisableEnableControls(UsersTabSheet, False);

  ShowTableDetails := False;
  ShowHideTableDetails;

  TreeSpacerImgList := GetTransparentImgList(6);
  IndicesVST.Images := TreeSpacerImgList;

  //Enable Menu Items
  EditTableDataMI.Enabled := (GetMySQLQueryBrowserCmd<>'');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FormDestroy(Sender: TObject);

begin
  PrevTableList.Free;
  CurrentTableList.Free;
  PrevViewList.Free;
  CurrentViewList.Free;
  PrevSPList.Free;
  CurrentSPList.Free;

  TreeBtnClosedPNGImg.Free;
  TreeBtnOpenPNGImg.Free;
  TablePNGImg.Free;
  TableHeaderPNGImg.Free;
  ViewPNGImg.Free;
  StoredProcPNGImg.Free;
  IndexPNGImg.Free;
  IndexHeaderPNGImg.Free;
  ColumnPNGImg.Free;
  ColumnPKPNGImg.Free;

  TreeSpacerImgList.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  Action := caFree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.CatalogPnlResize(Sender: TObject);

var
  InitSheetWidth,
  InitSheetHeight: Integer;

begin
  //Position Catalog PageControl
  CatalogPageControl.Top := 0;
  CatalogPageControl.Left := 0;
  CatalogPageControl.Width := CatalogPnl.Width;
  CatalogPageControl.Height := CatalogPnl.Height;

  SelectSchemaLbl.Width := CatalogPnl.Width-591+555;
  SelectSchemaLbl.Top := CatalogPnl.Height div 2-18;

  //Size Schema PageControl
  SchemaPageControl.Width := SchemaSheet.Width-583+569;
  SchemaPageControl.Height := SchemaSheet.Height-487+479;

  InitSheetWidth := 561; //of SchemaPageControl
  InitSheetHeight := 451;

  //Views TabSheet
  ViewsBevel.Width := IndicesTabSheet.Width-InitSheetWidth+535;

  ViewsVST.Width := IndicesTabSheet.Width-InitSheetWidth+535;
  ViewsVST.Height := IndicesTabSheet.Height-InitSheetHeight+349;

  //StoredProc TabSheet
  StoredProcBevel.Width := StoredProcTabSheet.Width-InitSheetWidth+535;

  StoredProcsVST.Width := StoredProcTabSheet.Width-InitSheetWidth+535;
  StoredProcsVST.Height := StoredProcTabSheet.Height-InitSheetHeight+349;

  //Events TabSheet
  EventsBevel.Width := StoredProcTabSheet.Width-InitSheetWidth+535;

  //DevDoc TabSheet
  DevDocBevel.Width := DevDocTabSheet.Width-InitSheetWidth+535;

  DevDocTreeView.Width := DevDocTabSheet.Width-InitSheetWidth+535;
  DevDocTreeView.Height := DevDocTabSheet.Height-InitSheetHeight+349;

  DevDocTreeViewHeaderControl.Width := DevDocTabSheet.Width-InitSheetWidth+516;

  //Users TabSheet
  UsersBevel.Width := UsersTabSheet.Width-InitSheetWidth+535;
end;

procedure TAdminCatalogForm.SchemaPageControlChange(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------


begin
  CatalogPnlResize(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DevDocTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);

var
  NodeRect: TRect;
  XPos: Integer;
  theHeaderControl: THeaderControl;
  I: Integer;

begin
  if (TTntTreeView(Sender).Name='TableIndexTreeView') then
    theHeaderControl := DevDocTreeViewHeaderControl
  else
    theHeaderControl := nil;

  if (Stage=cdPrePaint) then
  begin
    Sender.Canvas.Pen.Style := psSolid;
    if (cdsFocused in State) then
      Sender.Canvas.Pen.Color := clGray
    else
      Sender.Canvas.Pen.Color := clSilver;

    NodeRect := Node.DisplayRect(False);

    if (cdsSelected in State) then
    begin
      Sender.Canvas.Brush.Color := clSilver;
      Sender.Canvas.Pen.Color := clGray;
      Sender.Canvas.Rectangle(NodeRect.Left+19+Node.Level*19+5-2, NodeRect.Top,
        Sender.Width-4-18, NodeRect.Bottom);
    end
    else
    begin
      Sender.Canvas.Brush.Color := clWhite;
      Sender.Canvas.FillRect(Rect(NodeRect.Left{+19}+Node.Level*19+5-2, NodeRect.Top,
        Sender.Width-4-18, NodeRect.Bottom+1));
    end;

    Sender.Canvas.Brush.Color := clWhite;

    DefaultDraw := True;
  end
  else if (Stage=cdPostPaint) then
  begin
    if (Node.Data<>nil) then
    begin
      NodeRect := Node.DisplayRect(False);

      if (cdsFocused in State) then
      begin
        Sender.Canvas.Pen.Color := clGray;
        Sender.Canvas.Pen.Style := psSolid;
        Sender.Canvas.Brush.Color := clSilver;

        Sender.Canvas.Rectangle(NodeRect.Left+19+Node.Level*19+5-2, NodeRect.Top,
          Sender.Width-4-18, NodeRect.Bottom);

        Sender.Canvas.Brush.Style := bsClear;

        Sender.Canvas.TextRect(Rect(
          NodeRect.Left+19+Node.Level*19+5, NodeRect.Top+1,
          theHeaderControl.Sections[0].Width+2, NodeRect.Top+16),
          NodeRect.Left+19+Node.Level*19+5, NodeRect.Top+1,
          Node.Text);
      end;

      XPos := 0;
      for I := 0 to theHeaderControl.Sections.Count-2 do
      begin
        XPos := XPos+theHeaderControl.Sections[I].Width;

        if (TTntTreeView(Sender).Name='IndexTreeView')or
          (TTntTreeView(Sender).Name='TableIndexTreeView') then
        begin
          if (Node.Data<>nil) then
            if (I<TTntStringList(Node.Data).Count) then
              Sender.Canvas.TextRect(Rect(XPos+2, NodeRect.Top+1,
                XPos+theHeaderControl.Sections[I+1].Width-4, NodeRect.Top+16),
                XPos+2, NodeRect.Top+1,
                TTntStringList(Node.Data)[I]);
        end
        else
          Sender.Canvas.TextOut(XPos+2, NodeRect.Top+2, TDevDocNode(Node.Data).GetSubItems[I]);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DevDocTreeViewHeaderControlSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);

begin
  DevDocTreeView.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataTreeAdvEditSeachEdChange(Sender: TObject);

begin
  SchemataTreeAdvEditSeachEdChange(Sender);

  //if there is no Node selected
  if (SchemataFrame.CatalogVST.SelectedCount=0) then
    CatalogPageControl.ActivePageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameSchemaTreeViewChange(Sender: TObject; Node: TTntTreeNode);

begin

  if Assigned(Node) then
    if (Node.Data<>nil) then
    begin
      if (TObject(Node.Data) is TMYX_CATALOG) then
        CatalogPageControl.ActivePage := CatalogSheet
      else if (TObject(Node.Data) is TMYX_SCHEMA) then
      begin
        CatalogPageControl.ActivePage := SchemaSheet;

        BuildSchemaPages(TMYX_SCHEMA(Node.Data));
      end
      else if (TObject(Node.Data) is TSchemaSubNode) then
      begin
        CatalogPageControl.ActivePage := SchemaSheet;

        case TSchemaSubNode(Node.Data).SubNodeType of
          TSNTables:
            SchemaPageControl.ActivePage := TablesTabSheet;
          TSNIndices:
            SchemaPageControl.ActivePage := IndicesTabSheet;
          TSNViews:
            SchemaPageControl.ActivePage := ViewsTabSheet;
          TSNStoredProcedures:
            SchemaPageControl.ActivePage := StoredProcTabSheet;
          TSNEvents:
            SchemaPageControl.ActivePage := EventsTabSheet;
          TSNDevDocs:
            SchemaPageControl.ActivePage := DevDocTabSheet;
          TSNUsers:
            SchemaPageControl.ActivePage := UsersTabSheet;
        end;
      end
      else if (TObject(Node.Data) is TMYX_SCHEMA_TABLE) then
      begin
        CatalogPageControl.ActivePage := AssetSheet;

        EditTable('', CurrentSchema.schema_name,
          TMYX_SCHEMA_TABLE(Node.Data).table_name);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminCatalogForm.FetchMySQLMessages: WideString;

// Reads all warnings and errors that have been caused by the last command.

var
  Messages: PMYX_MYSQL_ERROR_MSGS;
  Message: PMYX_MYSQL_ERROR_MSG;
  I: Integer;

begin
  Result := '';
  Messages := myx_mysql_error_msgs_fetch(MySQLConn.MySQL);

  if Assigned(Messages) then
  begin
    for I := 0 to Messages.errors_num - 1 do
    begin
      Result := Result + #13#10;
      Message := PMYX_MYSQL_ERROR_MSG(Integer(Messages.errors) + sizeof(MYX_MYSQL_ERROR_MSG) * I);
      case Message.level of
        MYX_QEL_NOTE:
          Result := Result + 'Note: ';
        MYX_QEL_WARNING:
          Result := Result + 'Warning: ';
        MYX_QEL_ERROR:
          Result := Result + 'Error: ';
      end;
      Result := Result + WideFormat('(%d), %s', [Message.error, UTF8Decode(Message.text)]);
    end;

    myx_mysql_error_msgs_free(Messages);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchSchemaTableStatus(Sender: TObject);

var
  ptables: PMYX_SCHEMA_TABLE_STATUS;
  SchemaName: WideString;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;

    ptables := myx_get_schema_table_status(MySQLConn.MySQL, '', SchemaName);
    try
      CurrentTableList := TMYX_SCHEMA_TABLE_STATUS.Create(ptables);
      if ptables = nil then
        raise EMyxMultiSQLError.Create(_('Could not fetch schema table status.'), FetchMySQLMessages);
    finally
      myx_free_schema_table_status(ptables);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchSchemaViewStatus(Sender: TObject);

var
  pviews: PMYX_SCHEMA_VIEW_STATUS;
  SchemaName: WideString;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;

    pviews := myx_get_schema_view_status(MySQLConn.MySQL, '', SchemaName);
    try
      CurrentViewList := TMYX_SCHEMA_VIEW_STATUS.Create(pviews);
      if pviews = nil then
        raise EMyxMultiSQLError.Create(_('Could not fetch schema view status.'), FetchMySQLMessages);
    finally
      myx_free_schema_view_status(pviews);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchSchemaSPStatus(Sender: TObject);

var
  psps: PMYX_SCHEMA_STORED_PROCEDURES;
  SchemaName: WideString;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;

    psps := myx_get_schema_sps(MySQLConn.MySQL, '', SchemaName);
    try
      CurrentSPList := TMYX_SCHEMA_STORED_PROCEDURES.Create(psps);
      if psps = nil then
        raise EMyxMultiSQLError.Create(_('Could not fetch schema routine status.'), FetchMySQLMessages);
    finally
      myx_free_schema_sps(psps);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchedSchemaTableStatus(Sender: TObject);

begin
  CurrentSchema := TMYX_SCHEMA(TFetchDataThread(Sender).Target);
  BuildSchemaPages(CurrentSchema);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchedSchemaViewStatus(Sender: TObject);

begin
  CurrentSchema := TMYX_SCHEMA(TFetchDataThread(Sender).Target);
  BuildSchemaPages(CurrentSchema);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchedSchemaSPStatus(Sender: TObject);

begin
  CurrentSchema := TMYX_SCHEMA(TFetchDataThread(Sender).Target);
  BuildSchemaPages(CurrentSchema);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ClearControls;

begin
  //Clear Table lists
  InitControls := True;
  try
    TablesVST.BeginUpdate;
    try
      TablesVST.Clear;
      ClearTableInfos;
    finally
      TablesVST.EndUpdate;
    end;

    IndicesVST.BeginUpdate;
    try
      IndicesVST.Clear;
    finally
      IndicesVST.EndUpdate;
    end;

    ViewsVST.BeginUpdate;
    try
      ViewsVST.Clear;
    finally
      ViewsVST.EndUpdate;
    end;

  finally
    InitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.BuildSchemaPages(Schema: TMYX_SCHEMA);

var
  I, J, K: Integer;
  Node: PVirtualNode;
  rows, DataLenMyISAM: Int64;
  NodeData: PIndexNodeData;
  IndexCount, IndexColCount,
  IndexUniqueCount,
  IndexNotNullCount: Integer;

begin
  ClearControls;

  //if nil is passed as schema, refresh current schema
  if (Schema=nil) then
  begin
    Schema := CurrentSchema;
    CurrentSchema := nil;
  end;

  // we don't free trees here
  // because they could be used from concurrent threads
  // instead free them after we build new trees in code below
  if (CurrentSchema<>Schema) then
  begin
    PrevTableList := CurrentTableList;
    CurrentTableList := nil;

    PrevViewList := CurrentViewList;
    CurrentViewList := nil;

    PrevSPList := CurrentSPList;
    CurrentSPList := nil;
  end;

  if (CurrentTableList=nil) then
  begin
    MySQLConn.FetchData(dkSchemaTableStatus,
      FetchSchemaTableStatus, FetchedSchemaTableStatus,
      Schema, _('Fetching Schema Table Status ...'));
    Exit;
  end;

  if (CurrentViewList=nil) then
  begin
    MySQLConn.FetchData(dkSchemaViewStatus,
      FetchSchemaViewStatus, FetchedSchemaViewStatus,
      Schema, _('Fetching Schema View Status ...'));
    Exit;
  end;

  if (CurrentSPList=nil) then
  begin
    MySQLConn.FetchData(dkSchemaSPStatus,
      FetchSchemaSPStatus, FetchedSchemaSPStatus,
      Schema, _('Fetching Schema View Status ...'));
    Exit;
  end;

  TablesSchemaLbl.Caption := Schema.schema_name;
  TablesTitleLbl.Caption := format(_('All tables of the %s schema'),[Schema.schema_name]);

  IndicesSchemaLbl.Caption := Schema.schema_name;
  IndicesTitleLbl.Caption := format(_('All %s indices'),[Schema.schema_name]);

  ViewsSchemaLbl.Caption := Schema.schema_name;
  ViewsTitleLbl.Caption := Schema.schema_name+' views';

  StoredProcTitleLbl.Caption := Schema.schema_name+' stored procedures';
  StoredProcSchemaLbl.Caption := Schema.schema_name;

  DevObjTitleLbl.Caption := format(_('Developer documents stored in %s'),[Schema.schema_name]);
  UsersTitleLbl.Caption := format(_('Users with privileges to connect to %s'),[Schema.schema_name]);

  //Table Page

  InitControls := True;

  TablesVST.BeginUpdate;
  try
    TablesVST.Clear;

    TablesVST.NodeDataSize := sizeof(Pointer);

    rows := 0;
    CurrentSchemaDataLen := 0;
    CurrentSchemaIndexLen := 0;
    DataLenMyISAM := 0;
    for I := 0 to CurrentTableList.schema_tables.Count-1 do
    begin
      TablesVST.AddChild(nil, CurrentTableList.schema_tables[I]);
      rows := rows + StrToInt64Def(CurrentTableList.schema_tables[I].rows, 0);

      CurrentSchemaDataLen := CurrentSchemaDataLen + StrToInt64Def(CurrentTableList.schema_tables[I].data_length, 0);
      CurrentSchemaIndexLen := CurrentSchemaIndexLen + StrToInt64Def(CurrentTableList.schema_tables[I].index_length, 0);
      if (CurrentTableList.schema_tables[I].table_type='MyISAM') then
        DataLenMyISAM := DataLenMyISAM+StrToInt64Def(CurrentTableList.schema_tables[I].data_length, 0);
    end;

    if (CurrentTableList.schema_tables.Count>0) then
      TablesVST.FocusedNode := TablesVST.GetFirst;
  finally
    TablesVST.EndUpdate;
    InitControls := False;
    FreeAndNil(PrevTableList);
  end;

  NumberOfTablesLbl.Caption := IntToStr(CurrentTableList.schema_tables.Count);
  RowsCountLbl.Caption := FormatFloat('#,###,###,##0', rows);
  TableDataLenLbl.Caption := FormatFileSize(CurrentSchemaDataLen);
  TableIndexLenLbl.Caption := FormatFileSize(CurrentSchemaIndexLen);

  //Index Page

  InitControls := True;

  IndicesVST.BeginUpdate;
  try
    IndicesVST.Clear;

    IndicesVST.NodeDataSize := sizeof(IndexNodeData);

    IndexCount := 0;
    IndexColCount := 0;
    IndexUniqueCount := 0;
    IndexNotNullCount := 0;
    for I := 0 to CurrentTableList.schema_tables.Count-1 do
    begin
      for J := 0 to CurrentTableList.schema_tables[I].indexes.Count-1 do
      begin
        Node := IndicesVST.AddChild(nil);
        NodeData := IndicesVST.GetNodeData(Node);
        NodeData.Table := CurrentTableList.schema_tables[I];
        NodeData.Index := CurrentTableList.schema_tables[I].indexes[J];
        NodeData.Column := nil;

        inc(IndexCount);
        if (CurrentTableList.schema_tables[I].indexes[J].unique=1) then
          inc(IndexUniqueCount);
        if (CurrentTableList.schema_tables[I].indexes[J].not_null=1) then
          inc(IndexNotNullCount);


        for K := 0 to CurrentTableList.schema_tables[I].indexes[J].index_columns.Count-1 do
        begin
          NodeData := IndicesVST.GetNodeData(IndicesVST.AddChild(Node,
            CurrentTableList.schema_tables[I].indexes[J].index_columns[K]));
          NodeData.Table := nil;
          NodeData.Index := nil;
          NodeData.Column := CurrentTableList.schema_tables[I].indexes[J].index_columns[K];

          inc(IndexColCount);
        end;
      end;
    end;
  finally
    IndicesVST.EndUpdate;
    InitControls := False;
  end;

  NumberOfIndicesLbl.Caption := IntToStr(IndexCount);
  IndexColCountLbl.Caption := IntToStr(IndexColCount);
  IndexUniqueCountLbl.Caption := IntToStr(IndexUniqueCount);
  IndexNotNullCountLbl.Caption := IntToStr(IndexNotNullCount);

  // Views Page

  InitControls := True;

  ViewsVST.BeginUpdate;
  try
    ViewsVST.Clear;
    ViewsVST.NodeDataSize := sizeof(ViewNodeData);

    for I := 0 to CurrentViewList.schema_views.Count-1 do
    begin
      ViewsVST.AddChild(nil, CurrentViewList.schema_views[I]);
    end;

    if (CurrentViewList.schema_views.Count>0) then
      ViewsVST.FocusedNode := ViewsVST.GetFirst;

  finally
    ViewsVST.EndUpdate;
    InitControls := False;
    if (Assigned(PrevViewList)) then
    begin
      PrevViewList.Free;
      PrevViewList := nil;
    end;
  end;

  // SPs Page

  InitControls := True;
  StoredProcsVST.BeginUpdate;
  try
    StoredProcsVST.Clear;
    StoredProcsVST.NodeDataSize := sizeof(SPNodeData);

    for I := 0 to CurrentSPList.schema_sps.Count-1 do
    begin
      StoredProcsVST.AddChild(nil, CurrentSPList.schema_sps[I]);
    end;

    if (CurrentSPList.schema_sps.Count>0) then
      StoredProcsVST.FocusedNode := StoredProcsVST.GetFirst;

  finally
    StoredProcsVST.EndUpdate;
    InitControls := False;
    if (Assigned(PrevSPList)) then
    begin
      PrevSPList.Free;
      PrevSPList := nil;
    end;
  end;

  //Reselect the node if the user tryed it select a different node
  //while fetching
  if (SchemataFrame.CatalogVST.FocusedNode<>nil) then
  begin
    SchemataFrame.CatalogVST.ClearSelection;
    SchemataFrame.CatalogVST.Selected[
      SchemataFrame.CatalogVST.FocusedNode] := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ClearTableInfos;

begin
  TablenameLbl.Caption := '';

  TableTypeLbl.Caption := '';
  RowFormatLbl.Caption := '';
  AutoIncLbl.Caption := '';
  CreateOptionsLbl.Caption := '';
  TableCommentLbl.Caption := '';

  RowsLbl.Caption := '';
  AvgRowLengthLbl.Caption := '';
  DataLengthLbl.Caption := '';
  MaxDataLenLbl.Caption := '';
  IndexLenLbl.Caption := '';
  DataFreeLbl.Caption := '';

  TblCreateTimeLbl.Caption := '';
  TblUpdateTimeLbl.Caption := '';
  TblCheckTimeLbl.Caption := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesOptionsPageControlChange(Sender: TObject);

begin
  TablenamePnl.Parent := TablesOptionsPageControl.ActivePage;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ConnectionLost(var Message: TMessage);

begin
  ClearControls;
  SchemataFrame.ClearCatalogTree;

  DisableEnablePages(SchemaPageControl, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ConnectionEstablished(var Message: TMessage);

begin
  SchemataFrame.ReloadSchemaTree;
  SchemataFrame.FillSchemaTree;

  DisableEnablePages(SchemaPageControl, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemaListChanged(var Message: TMessage);

begin
  SchemataFrame.ReloadSchemaTree;
  SchemataFrame.FillSchemaTree;

  DisableEnablePages(SchemaPageControl, True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.CheckTablesBtnClick(Sender: TObject);

var
  TableList: TTntStringList;
  I: Integer;
  Action: TableAction;
  ActionIndex: Integer;
  Selection: TNodeArray;
  NodeData: ^TMYX_TABLE_STATUS;

begin
  Selection := nil;

  if (AdminCatalogTableCheckForm=nil)and(CurrentSchema<>nil) then
  begin
    if TablesVST.SelectedCount=0 then
      TablesVST.SelectAll(False);

    Selection := TablesVST.GetSortedSelection(False);

    TableList := TTntStringList.Create;
    try
      for I := 0 to Length(Selection)-1 do
      begin
        NodeData := TablesVST.GetNodeData(Selection[I]);
        TableList.Add('`'+CurrentSchema.schema_name+'`.`'+
          NodeData.table_name+'`');
      end;

      if (Sender.ClassNameIs('TTntButton')) then
        ActionIndex := TTntButton(Sender).Tag
      else if (Sender.ClassNameIs('TTntMenuItem')) then
        ActionIndex := TTntMenuItem(Sender).Tag
      else
        ActionIndex := 1;

      case ActionIndex of
        1: Action := taOptimize;
        2: Action := taCheck;
      else
        Action := taRepair;
      end;

      AdminCatalogTableCheckForm := TAdminCatalogTableCheckForm.Create(self,
        TableList.Text, MySQLConn, Action);

      AdminCatalogTableCheckForm.Show;
    finally
      TableList.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameRefreshCatalogsSchemataListMIClick(Sender: TObject);

begin
  SchemataFrame.RefreshCatalogsSchemataListMIClick(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.RefreshBtnClick(Sender: TObject);

begin
  if (CurrentSchema<>nil) then
    BuildSchemaPages;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditTableMIClick(Sender: TObject);

var
  NodeData: ^TMYX_TABLE_STATUS;

begin
  if (TablesVST.FocusedNode<>nil)and(CurrentSchema<>nil) then
  begin
    NodeData := TablesVST.GetNodeData(TablesVST.FocusedNode);

    if (NodeData<>nil) then
      EditTable('', CurrentSchema.schema_name, NodeData.table_name);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditorTableClose(Sender: TObject);

begin
  SchemaPageControl.ActivePage := TablesTabSheet;
  CatalogPageControl.ActivePage := SchemaSheet;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditorTableApplyedChanges(Sender: TObject);

begin
  if (CurrentSchema<>nil) then
    BuildSchemaPages;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditTable(Catalog: WideString; Schema: WideString; Table: WideString);

begin
  if EditorTableForm = nil then
  begin
    EditorTableForm := TEditorTableForm.Create(self);
    EditorTableForm.SetEditMode(DBMEditMode_Online, MySQLConn);
    EditorTableForm.OnApplyedChanges := EditorTableApplyedChanges;
  end;

  EditorTableForm.InitControls := True;
  try
    EditorTableForm.SetDatabaseVersion(MySQLConn.MajorVersion, MySQLConn.MinorVersion);
    EditorTableForm.EditStatusTable(Catalog, Schema, Table, CurrentTableList);
    EditorTableForm.Show;
  finally
    EditorTableForm.InitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.CreateTableMIClick(Sender: TObject);

begin
  if (CurrentSchema<>nil) then
    EditTable('', CurrentSchema.schema_name, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ShowHideTableDetails;

begin
  if (ShowTableDetails) then
  begin
    TableDetailsBtn.Caption := _('<< Details');

    TableInfoSplitter.Visible := True;
    TablesOptionsPageControl.Visible := True;
    TableInfoSplitter.Top := 0;
    TableSumPnl.Top := 0;
  end
  else
  begin
    TableDetailsBtn.Caption := _('Details >>');

    TableInfoSplitter.Visible := False;
    TablesOptionsPageControl.Visible := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TableDetailsBtnClick(Sender: TObject);

begin
  ShowTableDetails := Not(ShowTableDetails);
  ShowHideTableDetails;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TableInfoSplitterPaint(Sender: TObject);

var
  R: TRect;

begin
  with TableInfoSplitter do
  begin
    R := ClientRect;
    InflateRect(R, 0, -4);
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(R);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
  TColumnIndex; TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: ^TMYX_TABLE_STATUS;

begin
  NodeData := Sender.GetNodeData(Node);

  if Assigned(NodeData) then
  begin
    case Column of
      0:
        CellText := NodeData.table_name;
      1:
        CellText := NodeData.table_type;
      2:
        CellText := NodeData.rows;
      3:
        CellText := FormatFileSize(StrToInt64Def(NodeData.data_length, 0));
      4:
        CellText := FormatFileSize(StrToInt64Def(NodeData.index_length, 0));
      5:
        CellText := NodeData.update_time;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: ^TMYX_VIEW_STATUS;

begin
  NodeData := ViewsVST.GetNodeData(Node);
  if (NodeData<>nil) then
  begin
    case Column of
      0:
        CellText := NodeData.view_name;
      1:
        CellText := NodeData.comment;
      2:
        CellText := 'n/a';
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.StoredProcsVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: ^TMYX_SCHEMA_STORED_PROCEDURE;
  
begin
  NodeData := StoredProcsVST.GetNodeData(Node);
  if (NodeData<>nil) then
  begin
    case Column of
      0:
        CellText := NodeData.name;
      1:
        CellText := NodeData.definer;
      2:
        CellText := NodeData.created;
      3:
        CellText := NodeData.modified;
      4:
        CellText := NodeData.return_datatype;
      5:
        CellText := NodeData.comment;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if (Column<>Sender.SortColumn) then
    Sender.SortColumn := Column
  else
    if (Sender.SortDirection=sdAscending) then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if (Column<>Sender.SortColumn) then
    Sender.SortColumn := Column
  else
    if (Sender.SortDirection=sdAscending) then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  NodeData: ^TMYX_VIEW_STATUS;

begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.invalid = 1 then
    TargetCanvas.Font.Color := clRed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.StoredProcsVSTHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if (Column<>Sender.SortColumn) then
    Sender.SortColumn := Column
  else
    if (Sender.SortDirection=sdAscending) then
      Sender.SortDirection := sdDescending
    else
      Sender.SortDirection := sdAscending;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  NodeData1: ^TMYX_TABLE_STATUS;
  NodeData2: ^TMYX_TABLE_STATUS;
  FormatSettings: TFormatSettings;
  Difference: Int64;

begin
  NodeData1 := TablesVST.GetNodeData(Node1);
  NodeData2 := TablesVST.GetNodeData(Node2);
  if (NodeData1<>nil)and(NodeData2<>nil) then
  begin
    case Column of
      0:
        Result := CompareText(NodeData1.table_name, NodeData2.table_name);
      1:
      begin
        // First sort for the type then the name.
        Result := CompareText(NodeData1.table_type, NodeData2.table_type);
        if Result = 0 then
          Result := CompareText(NodeData1.table_name, NodeData2.table_name);
      end;
      2:
        begin
          Difference := StrToInt64Def(NodeData1.rows, 0) - StrToInt64Def(NodeData2.rows, 0);

          // Differences can be more than what an Integer can hold. So we need to check this.
          if Difference < 0 then
            Result := -1
          else
            if Difference > 0 then
              Result := 1
            else
              Result := 0;
        end;
      3:
        begin
          Difference := StrToInt64Def(NodeData1.data_length, 0) - StrToInt64Def(NodeData2.data_length, 0);
          if Difference < 0 then
            Result := -1
          else
            if Difference > 0 then
              Result := 1
            else
              Result := 0;
        end;
      4:
        begin
          Difference := StrToInt64Def(NodeData1.index_length, 0) - StrToInt64Def(NodeData2.index_length, 0);
          if Difference < 0 then
            Result := -1
          else
            if Difference > 0 then
              Result := 1
            else
              Result := 0;
        end;
      5:
      begin
        GetLocaleFormatSettings(GetSystemDefaultLCID, FormatSettings);

        FormatSettings.DateSeparator := '-';
        FormatSettings.TimeSeparator := ':';
        FormatSettings.ShortDateFormat := 'YYYY-MM-DD HH:NN:SS';
        FormatSettings.LongDateFormat := 'YYYY-MM-DD HH:NN:SS';

        Result := Round(StrToDateTimeDef(NodeData1.update_time, Now, FormatSettings)*60*60 -
          StrToDateTimeDef(NodeData2.update_time, Now, FormatSettings)*60*60);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  NodeData1: ^TMYX_VIEW_STATUS;
  NodeData2: ^TMYX_VIEW_STATUS;

begin
  NodeData1 := ViewsVST.GetNodeData(Node1);
  NodeData2 := ViewsVST.GetNodeData(Node2);
  if (NodeData1<>nil)and(NodeData2<>nil) then
  begin
    case Column of
      0:
        Result := CompareText(NodeData1.view_name, NodeData2.view_name);
      1:
        Result := CompareText(NodeData1.comment, NodeData2.comment);
      2:
        Result := 0;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTDblClick(Sender: TObject);

begin
  EditViewBtnClick(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.StoredProcsVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  NodeData1,
  NodeData2: ^TMYX_SCHEMA_STORED_PROCEDURE;

begin
  NodeData1 := StoredProcsVST.GetNodeData(Node1);
  NodeData2 := StoredProcsVST.GetNodeData(Node2);
  if (NodeData1<>nil)and(NodeData2<>nil) then
  begin
    case Column of
      0:
        Result := CompareText(NodeData1.name, NodeData2.name);
      1:
        Result := CompareText(NodeData1.definer, NodeData2.definer);
      2:
        Result := CompareText(NodeData1.created, NodeData2.created);
      3:
        Result := CompareText(NodeData1.modified, NodeData2.modified);
      4:
        Result := CompareText(NodeData1.return_datatype, NodeData2.return_datatype);
      5:
        Result := CompareText(NodeData1.comment, NodeData2.comment);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.StoredProcsVSTDblClick(Sender: TObject);

begin
  EditSPBtnClick(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTDblClick(Sender: TObject);

begin
  EditTableMIClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key=Ord('A'))and(ssCtrl in Shift) then
    TablesVST.SelectAll(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

var
  NodeData: ^TMYX_TABLE_STATUS;

begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData.invalid = 1 then
    TargetCanvas.Font.Color := clRed;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: ^TMYX_TABLE_STATUS;
  
begin
  if not InitControls then
  if Assigned(Node) then
  begin
    NodeData := TablesVST.GetNodeData(Node);
    TableNameLbl.Caption := NodeData.table_name;

    TableTypeLbl.Caption := NodeData.table_type;
    RowFormatLbl.Caption := NodeData.row_format;

    if (NodeData.auto_increment<>'') then
      AutoIncLbl.Caption := NodeData.auto_increment
    else
      AutoIncLbl.Caption := '-';

    if (NodeData.create_options<>'') then
      CreateOptionsLbl.Caption := NodeData.create_options
    else
      CreateOptionsLbl.Caption := '-';

    if (NodeData.comment<>'') then
      TableCommentLbl.Caption := NodeData.comment
    else
      TableCommentLbl.Caption := '-';

    RowsLbl.Caption := NodeData.rows;
    AvgRowLengthLbl.Caption := NodeData.avg_row_length;
    DataLengthLbl.Caption := NodeData.data_length;
    MaxDataLenLbl.Caption := NodeData.max_data_length;
    IndexLenLbl.Caption := NodeData.index_length;
    DataFreeLbl.Caption := NodeData.data_free;

    TblCreateTimeLbl.Caption := NodeData.create_time;
    TblUpdateTimeLbl.Caption := NodeData.update_time;
    TblCheckTimeLbl.Caption := NodeData.check_time;
  end
  else
    ClearTableInfos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

begin
  if Column = 0 then
    TablePNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 5, CellRect.Top + 1, CellRect.Left + 21, CellRect.Top + 17));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.ViewsVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

begin
  if Column = 0 then
    ViewPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 5, CellRect.Top + 1, CellRect.Left + 21, CellRect.Top + 17));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.StoredProcsVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

begin
  if Column = 0 then
    StoredProcPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 5, CellRect.Top + 1, CellRect.Left + 21, CellRect.Top + 17));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if Column = 0 then
    ImageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.PaintNodeBKBarGraph(TargetCanvas: TCanvas; Node: PVirtualNode; Column: Integer;
  ItemRect: TRect; ValueWidth: Integer; PenColor: TColor; BrushColor: TColor);

// Draws a colored graph similar to a progress bar to visualize the value "Width" relative to the entire column width.

var
  R, G, B: Byte;
  Background: COLORREF;

begin
  // Compute a background color that fits nicely to the current window background.
  Background := ColorToRGB(clWindow);
  R := Round(GetRValue(Background) * 0.9 + $F6 * 0.1);
  G := Round(GetGValue(Background) * 0.9 + $EF * 0.1);
  B := Round(GetBValue(Background) * 0.9 + $F6 * 0.1);
  TargetCanvas.Brush.Color := RGB(R, G, B);
  TargetCanvas.Pen.Color := $00D0C9C8;

  with TablesVST, Header.Columns[Column] do
  begin
    TargetCanvas.Rectangle(Rect(Left - OffsetX, ItemRect.Top, Left - OffsetX + Width - 1, ItemRect.Bottom - 1));
    if ValueWidth > 0 then
    begin
      TargetCanvas.Brush.Color := BrushColor;
      TargetCanvas.Pen.Color := PenColor;
      TargetCanvas.Rectangle(Rect(Left - OffsetX, ItemRect.Top, Left - OffsetX + ValueWidth - 1, ItemRect.Bottom - 1));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.TablesVSTAfterItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect);

var
  NodeData: ^TMYX_TABLE_STATUS;

begin
  NodeData := TablesVST.GetNodeData(Node);
  if (NodeData<>nil) then
  begin
    if (CurrentSchemaDataLen>0) then
    begin
      PaintNodeBKBarGraph(TargetCanvas, Node, 3, ItemRect,
        Round((StrToInt64Def(NodeData.data_length, 0)/CurrentSchemaDataLen)*
        TablesVST.Header.Columns[3].Width),
        $00D79588, $00E9B1A6);
    end;

    if (CurrentSchemaIndexLen>0) then
    begin
      PaintNodeBKBarGraph(TargetCanvas, Node, 4, ItemRect,
        Round((StrToInt64Def(NodeData.index_length, 0)/CurrentSchemaIndexLen)*
        TablesVST.Header.Columns[4].Width),
        $00ECA1C3, $00ECBCD2);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DropTableMIClick(Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;
  NodeData: ^TMYX_TABLE_STATUS;
begin
  Selection := TablesVST.GetSortedSelection(False);

  if ShowModalDialog(_('Drop Table(s)'), _('Are you sure you want to drop the selected table(s)?'), myx_mtConfirmation,
    _('Drop Table(s)') + #13#10 + _('Cancel')) = 1 then
  begin
    for I := 0 to TablesVST.SelectedCount - 1 do
    begin
      NodeData := TablesVST.GetNodeData(Selection[I]);
      MySQLConn.ExecuteDirect('DROP TABLE `' + CurrentSchema.schema_name + '`.' + '`' + NodeData.table_name + '`');
    end;

    BuildSchemaPages;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.IndicesVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PIndexNodeData;
  
begin
  if Assigned(Node) then
  begin
    NodeData := IndicesVST.GetNodeData(Node);
    if (NodeData<>nil) then
    begin
      if (NodeData.Index<>nil) then
      begin
        case Column of
          0:
            CellText := NodeData.Index.key_name;
          2:
            CellText := NodeData.Index.index_type;
          3:
            if (NodeData.Index.unique=1) then
              CellText := 'UNIQUE';
          4:
            if (NodeData.Index.not_null=1) then
              CellText := 'NOT NULL';
        end;
      end
      else if (NodeData.Column<>nil) then
      begin
        case Column of
          0:
            CellText := NodeData.Column.column_name;
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.IndicesVSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PIndexNodeData;
  TxtRect: TRect;
  X: Integer;

begin
  if not (csDestroying in ComponentState) then
  begin
    case Column of
      0:
        begin
          NodeData := Sender.GetNodeData(Node);
          if (NodeData<>nil) then
          begin
            TxtRect := Sender.GetDisplayRect(Node, Column, True);

            X := TxtRect.Left-Sender.OffsetX;

            if (NodeData.Index<>nil) then
            begin
              if (Node.ChildCount>0) then
              begin
                if (Sender.Expanded[Node]) then
                  TreeBtnOpenPNGImg.Draw(TargetCanvas,
                    Rect(X-16-12, CellRect.Top+4, X-16-4, CellRect.Top+16+4))
                else
                  TreeBtnClosedPNGImg.Draw(TargetCanvas,
                    Rect(X-16-12, CellRect.Top+4, X-16-4, CellRect.Top+16+4))
              end;

              IndexPNGImg.Draw(TargetCanvas,
                Rect(X-16, CellRect.Top+1, X, CellRect.Top+17));
            end
            else
              ColumnPNGImg.Draw(TargetCanvas,
                Rect(X-16, CellRect.Top+1, X, CellRect.Top+17));
          end;
        end;
      1:
        begin
          NodeData := Sender.GetNodeData(Node);
          if NodeData.Column = nil then
          begin
            TablePNGImg.Draw(TargetCanvas, Rect(CellRect.Left, CellRect.Top + 1, CellRect.Left + 16, CellRect.Top + 17));
            SetBkMode(TargetCanvas.Handle, TRANSPARENT);
            if vsSelected in Node.States then
              TargetCanvas.Font.Color := clHighlightText
            else
              TargetCanvas.Font.Color := IndicesVST.Font.Color;
            DrawWideStringText(TargetCanvas.Handle, PWideChar(NodeData.Table.table_name),
              Length(NodeData.Table.table_name), Rect(CellRect.Left + 20, CellRect.Top, CellRect.Right, CellRect.Bottom),
              DT_LEFT, False);
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.IndicesVSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if Column = 0 then
    ImageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.IndicesVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  HitInfo: THitInfo;

begin
  if (Sender.InheritsFrom(TBaseVirtualTree)) then
  begin
    TBaseVirtualTree(Sender).GetHitTestInfoAt(X, Y, True, HitInfo);

    if (HitInfo.HitNode<>nil)and(HitInfo.HitColumn = 0) then
    begin
      if (X<14)and(HitInfo.HitNode.ChildCount>0) then
      begin
        TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode] := 
          Not(TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]);
      end;
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.IndicesVSTCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);

var
  NodeData1,
  NodeData2: PIndexNodeData;

begin
  NodeData1 := Sender.GetNodeData(Node1);
  NodeData2 := Sender.GetNodeData(Node2);
  if (NodeData1<>nil)and(NodeData2<>nil) then
  begin
    if (NodeData1.Table<>nil)and(NodeData1.Table<>nil) then
    begin
      case Column of
        0:
        begin
          if (NodeData1.Index.key_name=NodeData2.Index.key_name) then
            Result := CompareText(NodeData1.table.table_name, NodeData2.table.table_name)
          else
            Result := CompareText(NodeData1.Index.key_name, NodeData2.Index.key_name);
        end;
        1:
        begin
          if (NodeData1.table.table_name=NodeData2.table.table_name) then
            Result := CompareText(NodeData1.Index.key_name, NodeData2.Index.key_name)
          else
            Result := CompareText(NodeData1.table.table_name, NodeData2.table.table_name);
        end;
        2:
          Result := CompareText(NodeData1.Index.index_type, NodeData2.Index.index_type);
        3:
          Result := NodeData1.Index.unique-NodeData2.Index.unique;
        4:
          Result := NodeData1.Index.not_null-NodeData2.Index.not_null;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameSchemaTreeViewChanging(Sender: TObject; Node: TTntTreeNode;
  var AllowChange: Boolean);

begin
  // Do not allow change while fetching data.
  AllowChange := MySQLConn.DataBeingFetched = [];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameSchemaTreeViewPopupMenuPopup(Sender: TObject);

begin
  // Do not allow the popup to appear while fetching data.
  if MySQLConn.DataBeingFetched = [] then
    SchemataFrame.SchemaTreeViewPopupMenuPopup(Sender)
  else
    Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditTableDataMIClick(Sender: TObject);

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
  NodeData: ^TMYX_TABLE_STATUS;

begin
  if (TablesVST.FocusedNode<>nil) then
  begin
    NodeData := TablesVST.GetNodeData(TablesVST.FocusedNode);
    if (NodeData<>nil) then
      if (NodeData^<>nil) then
      begin
        Command := GetMySQLQueryBrowserCmd;
        Command := Command+' "-u' + MySQLConn.UserConnection.username + '"';

        if MySQLConn.UserConnection.password <> '' then
          Command := Command + ' -x' + ConvertToHex(MySQLConn.UserConnection.password);

        Command := Command + ' "-h' + MySQLConn.UserConnection.hostname + '"';
        Command := Command + ' "-P' + IntToStr(MySQLConn.UserConnection.port) + '"';
        Command := Command + ' "-D' + CurrentSchema.schema_name + '"';
        Command := Command + ' "-selecttable=`' + CurrentSchema.schema_name + '`.`' + NodeData.table_name + '`"';

        CreateSubProcess(Command, '');
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DoSchemaTreeReloaded(Sender: TObject);

begin
  CatalogPageControl.ActivePage := CatalogSheet;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameCatalogVSTFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
  NewColumn: TColumnIndex; var Allowed: Boolean);

begin
  Allowed := MySQLConn.DataBeingFetched = [];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameCatalogVSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);

var
  NodeData,
  ParentNodeData: ^TObject;
  
begin
  SchemataFrame.CatalogVSTChange(Sender, Node);

  NodeData := Sender.GetNodeData(Node);

  if (NodeData<>nil) then
    if (NodeData^<>nil) then
    begin
      if (TObject(NodeData^) is TMYX_CATALOG) then
        CatalogPageControl.ActivePage := CatalogSheet
      else if (TObject(NodeData^) is TMYX_SCHEMA) then
      begin
        CatalogPageControl.ActivePage := SchemaSheet;
        BuildSchemaPages(TMYX_SCHEMA(NodeData^));
      end
      else if (TObject(NodeData^) is TSchemaSubNode) then
      begin
        if (CurrentSchema<>TSchemaSubNode(NodeData^).Schema) then
          BuildSchemaPages(TSchemaSubNode(NodeData^).Schema);

        CatalogPageControl.ActivePage := SchemaSheet;

        case TSchemaSubNode(NodeData^).SubNodeType of
          TSNTables:
            SchemaPageControl.ActivePage := TablesTabSheet;
          TSNIndices:
            SchemaPageControl.ActivePage := IndicesTabSheet;
          TSNViews:
            SchemaPageControl.ActivePage := ViewsTabSheet;
          TSNStoredProcedures:
            SchemaPageControl.ActivePage := StoredProcTabSheet;
          TSNEvents:
            SchemaPageControl.ActivePage := EventsTabSheet;
          TSNDevDocs:
            SchemaPageControl.ActivePage := DevDocTabSheet;
          TSNUsers:
            SchemaPageControl.ActivePage := UsersTabSheet;
        end;
      end
      else if (TObject(NodeData^) is TMYX_SCHEMA_TABLE) then
      begin
        if (Node.Parent<>nil) then
        begin
          ParentNodeData := Sender.GetNodeData(Node.Parent);
          if (ParentNodeData<>nil) then
            if (ParentNodeData^<>nil) then
            begin
              if (TObject(ParentNodeData^) is TMYX_SCHEMA) then
              begin
                BuildSchemaPages(TMYX_SCHEMA(ParentNodeData^));

                CurrentSchema := TMYX_SCHEMA(ParentNodeData^)
              end
              else if (TObject(ParentNodeData^) is TSchemaSubNode) then
              begin
                BuildSchemaPages(TSchemaSubNode(ParentNodeData^).Schema);

                CurrentSchema := TSchemaSubNode(ParentNodeData^).Schema
              end
              else
                CurrentSchema := nil;

              if (CurrentSchema<>nil) then
              begin
                CatalogPageControl.ActivePage := AssetSheet;

                EditTable('', CurrentSchema.schema_name,
                  TMYX_SCHEMA_TABLE(NodeData^).table_name);
              end;
            end;
        end;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameCreateNewSchemaMIClick(Sender: TObject);

begin
  SchemataFrame.CreateNewSchemaMIClick(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameDropMIClick(Sender: TObject);

begin
  SchemataFrame.DropMIClick(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.SchemataFrameCopyAssetSQLMIClick(
  Sender: TObject);

begin
  SchemataFrame.CopyAssetSQLMIClick(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.AddSPBtnClick(Sender: TObject);

var
  SPType,
  SPName,
  FuncReturnType: WideString;
  Res: Integer;

begin
  if (CurrentSchema = nil) or (CurrentSchema.schema_name = '') then
    ShowModalDialog(_('Choose schema'), _('Please select a schema before creating a stored procedure.'),
      myx_mtInformation, _('OK'))
  else
  begin
    Res := ShowModalEditDialog(_('Enter Stored Procedure / Function Name'),
      _('Please enter the name of the PROCEDURE / FUNCTION you want to create. A code template will be created.'),
        myx_mtEdit, _('Create PROCEDURE') + #13#10 + _('Create FUNCTION') + #13#10 + _('Cancel'), True, _('Name:'),
        SPName);

    if Res in [1, 2] then
    begin
      SPName := Trim(SPName);
      if Res = 1 then
      begin
        SPType := 'PROCEDURE';
        FuncReturnType := '';
        if SPName = '' then
          SPName := 'procedure1';
      end
      else
      begin
        SPType := 'FUNCTION';
        FuncReturnType := ' RETURNS INT';
        if SPName = '' then
          SPName := 'function1';
      end;

      if EditorSqlForm = nil then
      begin
        EditorSqlForm := TEditorSqlForm.Create(Self);
        EditorSqlForm.MySQLConn := MySQLConn;
        EditorSqlForm.RefreshSchemaObjects := RefreshSPBtnClick;
      end;

      EditorSqlForm.Sql := Format(
        'CREATE %s `%s`.`%s` ()%s'+#13#10+
        'BEGIN'+#13#10+
        '  ' + #13#10+
        'END'+#13#10,
        [SPType, CurrentSchema.schema_name, SPName, FuncReturnType]);

      EditorSqlForm.SqlUCE.CaretXY := Point(2, 2);
      EditorSqlForm.DropSQL := '';
      EditorSqlForm.Show;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.RefreshSPBtnClick(Sender: TObject);

begin
  if (CurrentSchema<>nil) then
    BuildSchemaPages;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditSPBtnClick(Sender: TObject);

var
  NodeData: ^TMYX_SCHEMA_STORED_PROCEDURE;

begin
  NodeData := StoredProcsVST.GetNodeData(StoredProcsVST.FocusedNode);
  if (NodeData <> nil) then
  begin
    procEditData := NodeData.name;
    procEditType := NodeData.sp_type;
    MySQLConn.FetchData(dkSchemaProcBody,
      FetchSchemaSPBody, FetchedSchemaSPBody,
      CurrentSchema, _('Fetching Schema Routine Data ...'));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchedSchemaSPBody(Sender: TObject);

begin
  if (EditorSqlForm = nil) then
  begin
    EditorSqlForm := TEditorSqlForm.Create(Self);
    EditorSqlForm.MySQLConn := MySQLConn;
    EditorSqlForm.RefreshSchemaObjects := RefreshSPBtnClick;
  end;
  EditorSqlForm.Sql := procEditData;
  EditorSqlForm.SetContentType(StoredRoutine);
  EditorSqlForm.SqlSchema := CurrentSchema.schema_name;
  EditorSqlForm.DropSQL := procEditData2;
  EditorSqlForm.SqlUCE.CaretXY := Point(2, 2);
  EditorSqlForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchSchemaSPBody(Sender: TObject);

var
  pproc: PMYX_DBM_STORED_PROCEDURE_DATA;
  SchemaName: WideString;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;
    pproc := myx_dbm_get_sp_data(MySQLConn.MySQL, '', SchemaName, procEditData, procEditType, '`', 0);
    if Assigned(pproc) then
    begin
      procEditData := Utf8Decode(pproc.definition);
      if procEditType = MSPT_PROCEDURE then
        procEditData2 := 'DROP PROCEDURE IF EXISTS ' + SchemaName + '.' + Utf8Decode(pproc.name)
      else
        procEditData2 := 'DROP FUNCTION IF EXISTS ' + SchemaName + '.' + Utf8Decode(pproc.name);
      myx_dbm_free_sp_data(pproc);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DelSPBtnClick(Sender: TObject);

var
  NodeData: ^TMYX_SCHEMA_STORED_PROCEDURE;
  PNode: PVirtualNode;
  NSel, I: Integer;

begin
  NSel := StoredProcsVST.SelectedCount;
  if NSel > 0 then
  begin
    if ShowModalDialog(_('Drop Stored Procedure(s)'), _('Are you sure you want to drop the selected stored procedure(s)?'),
      myx_mtConfirmation, _('Drop Stored Procedure(s)') + #13#10 + _('Cancel')) = 1 then
    begin
      SetLength(procDropData, NSel);
      SetLength(procDropType, NSel);
      PNode := StoredProcsVST.GetFirstSelected;
      NodeData := StoredProcsVST.GetNodeData(PNode);
      procDropData[0] := NodeData.name;
      procDropType[0] := NodeData.sp_type;

      for I := 1 to NSel - 1 do
      begin
        PNode := StoredProcsVST.GetNextSelected(PNode);
        NodeData := StoredProcsVST.GetNodeData(PNode);
        procDropData[I] := NodeData.name;
        procDropType[I] := NodeData.sp_type;
      end;

      MySQLConn.FetchData(dkSchemaProcDrop,
        DropSchemaSP, DroppedSchemaSP,
        CurrentSchema, _('Dropping Schema Routine(s) ...'));
      StoredProcsVST.BeginUpdate;
      StoredProcsVST.DeleteSelectedNodes;
      StoredProcsVST.EndUpdate;
    end;
  end;  
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DropSchemaSP(Sender: TObject);

var
  SchemaName: WideString;
  NSel, I: Integer;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;
    NSel := Length(procDropData);
    for I := 0 to NSel-1 do
      myx_dbm_drop_sp(MySQLConn.MySQL, '', SchemaName, procDropData[I], procDropType[I]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DroppedSchemaSP(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.CreateViewBtnClick(Sender: TObject);

var
  ViewName: WideString;
  Res: Integer;

begin
  if (CurrentSchema = nil) or (CurrentSchema.schema_name = '') then
    ShowModalDialog(_('Choose schema'), _('Please select a schema before creating a view.'), myx_mtInformation, _('OK'))
  else
  begin
    Res := ShowModalEditDialog(_('Enter View Name'), _('Please enter the name of the VIEW you want to create. A code ' +
      'template will be created.'), myx_mtEdit, _('Create VIEW')+#13#10+_('Cancel'), True, _('Name:'), ViewName);

    if Res <> 2 then
    begin
      if EditorSqlForm = nil then
      begin
        EditorSqlForm := TEditorSqlForm.Create(Self);
        EditorSqlForm.MySQLConn := MySQLConn;
        EditorSqlForm.RefreshSchemaObjects := RefreshSPBtnClick;
      end;

      EditorSqlForm.SqlSchema := CurrentSchema.schema_name;
      EditorSqlForm.DropSQL := '';
      EditorSqlForm.Sql := Format('CREATE OR REPLACE VIEW `%s`.`%s` AS SELECT ', [CurrentSchema.schema_name, ViewName]);
      EditorSqlForm.SqlUCE.CaretXY := Point(100, 1);
      EditorSqlForm.Show;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.EditViewBtnClick(Sender: TObject);

var
  NodeData: ^TMYX_VIEW_STATUS;

begin
  NodeData := ViewsVST.GetNodeData(ViewsVST.FocusedNode);
  if (NodeData <> nil) then
  begin
    viewEditData := NodeData.view_name;
    MySQLConn.FetchData(dkSchemaViewDef,
      FetchSchemaViewDef, FetchedSchemaViewDef,
      CurrentSchema, _('Fetching Schema View Data ...'));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchSchemaViewDef(Sender: TObject);

var
  pview: PMYX_DBM_VIEW_DATA;
  SchemaName: WideString;

begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;
    pview := myx_dbm_get_view_data(MySQLConn.MySQL, '', SchemaName, viewEditData, '`');
    if Assigned(pview) then
    begin
      viewEditData := Utf8Decode(pview.definition);
      viewEditData2 := 'DROP VIEW IF EXISTS `' + Utf8Decode(pview.schema) + '`.`' + Utf8Decode(pview.name) + '`';
      myx_dbm_free_view_data(pview);
    end
    else
      raise EMyxSQLError.Create(_('Could not get view description.'),
        myx_mysql_errno(TFetchDataThread(Sender).Connection.MySQL),
        myx_mysql_error(TFetchDataThread(Sender).Connection.MySQL));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.FetchedSchemaViewDef(Sender: TObject);

begin
  if (EditorSqlForm = nil) then
  begin
    EditorSqlForm := TEditorSqlForm.Create(Self);
    EditorSqlForm.MySQLConn := MySQLConn;
    EditorSqlForm.RefreshSchemaObjects := RefreshSPBtnClick;
  end;

  // Ask if the editor can be closed. It will deny if it was used before and has
  // unsaved changes.
  if EditorSqlForm.CloseQuery then
  begin
    EditorSqlForm.SqlSchema := CurrentSchema.schema_name;
    EditorSqlForm.Sql := viewEditData;
    EditorSqlForm.DropSQL := viewEditData2;
    EditorSqlForm.SqlUCE.CaretXY := Point(1, 1);
    EditorSqlForm.Show;
    EditorSqlForm.SetContentType(View);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DropViewBtnClick(Sender: TObject);

var
  PNode: PVirtualNode;
  NodeData: ^TMYX_VIEW_STATUS;
  NSel, I: Integer;
begin
  //NodeData := ViewsVST.GetNodeData(ViewsVST.FocusedNode);
  NSel := ViewsVST.SelectedCount;

  if (NSel > 0) then
  begin
    if (ShowModalDialog(_('Drop View(s)'), _('Are you sure you want to drop the selected view(s)?'), myx_mtConfirmation,
      _('Drop View(s)') + #13#10 + _('Cancel')) = 1) then
    begin
      SetLength(viewDropData, NSel);
      PNode := ViewsVST.GetFirstSelected;
      NodeData := ViewsVST.GetNodeData(PNode);
      viewDropData[0] := NodeData.view_name;

      for I := 1 to NSel - 1 do
      begin
        PNode := ViewsVST.GetNextSelected(PNode);
        NodeData := ViewsVST.GetNodeData(PNode);
        viewDropData[I] := NodeData.view_name;
      end;

      MySQLConn.FetchData(dkSchemaViewDrop, DropSchemaView, DroppedSchemaView, CurrentSchema,
        _('Dropping Schema View(s) ...'));
      ViewsVST.BeginUpdate;
      ViewsVST.DeleteSelectedNodes;
      ViewsVST.EndUpdate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DropSchemaView(Sender: TObject);

var
  SchemaName: WideString;
  NSel, I: Integer;
begin
  if MySQLConn.Connected then
  begin
    MySQLConn.Ping;
    SchemaName := TMYX_SCHEMA(TFetchDataThread(Sender).Target).schema_name;
    NSel := Length(viewDropData);
    for I := 0 to NSel - 1 do
      myx_dbm_drop_view(MySQLConn.MySQL, '', SchemaName, viewDropData[I]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.DroppedSchemaView(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.RefreshViewsBtnClick(Sender: TObject);

begin
  if (CurrentSchema<>nil) then
    BuildSchemaPages;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TAdminCatalogForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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

