unit SchemataTreeView;

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
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, SyncObjs,
  Controls, Forms, Dialogs, AdvancedEdit, StdCtrls, ExtCtrls, ComCtrls, Menus,
  MySQLConnection, AuxFuncs, myx_public_interface, MyxError, ImgList,
  EditorTable, VirtualTrees, PNGImage, Contnrs, TntClasses, TntForms, TntSysUtils,
  TntMenus, TntStdCtrls, TntExtCtrls, TntClipBrd, Options, CommonTypes, Unicode,
  myx_util_public_interface;

const
  // Timer constants for delayed actions.
  SearchTimer = 1;
  SearchTimeout  = 500;           // Number of milliseconds after which search starts when there was a keypress.

type
  TSchemaVirtualNode = class;

  TSchemaObjectEvent = procedure(Sender: TObject; CatalogName: WideString; SchemaName: WideString;
    SchemaObj: TObject) of object;

  TSearchTarget = (
    smSchemata,
    smTables,
    smColumns,
    smIndices,
    smStoredProcAndFunc,
    smViews,
    smTriggers
  );
  TSearchTargets = set of TSearchTarget;

  TSchemataFrame = class(TTntFrame, ISchemaInfo)
    TopPnl: TTntPanel;
    SchemataLbl: TTntLabel;
    SpacerPnl: TTntPanel;
    AdvancedEdit: TAdvancedEditFrame;
    SchemaSearchPopupMenu: TTntPopupMenu;
    SchemaTreeViewPopupMenu: TTntPopupMenu;
    SchemataAssetsImageList: TImageList;
    CatalogVST: TVirtualStringTree;
    SearchSchemataMI: TTntMenuItem;
    RefreshCatalogsSchemataListMI: TTntMenuItem;
    CreateNewTableMI: TTntMenuItem;
    CreateNewSchemaMI: TTntMenuItem;
    N2: TTntMenuItem;
    CreateNewViewMI: TTntMenuItem;
    CreateNewStoredProcedureMI: TTntMenuItem;
    N1: TTntMenuItem;
    EditMI: TTntMenuItem;
    DropMI: TTntMenuItem;
    RenameMI: TTntMenuItem;
    SearchAssetsMI: TTntMenuItem;
    SearchColumnsMI: TTntMenuItem;
    SearchAllMI: TTntMenuItem;
    CopyAssetSQLMI: TTntMenuItem;
    CustomMI: TTntMenuItem;
    N3: TTntMenuItem;
    SchemataSubMI: TTntMenuItem;
    TableSubMI: TTntMenuItem;
    ColumnsSubMI: TTntMenuItem;
    SPSubMI: TTntMenuItem;
    ViewsSubMI: TTntMenuItem;
    TriggersSubMI: TTntMenuItem;
    IndicesSubMI: TTntMenuItem;
    procedure AdvancedEditSearchEdChange(Sender: TObject);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReloadSchemaTree;
    procedure FillSchemaTree(searchStr: WideString = ''; ReloadSchemata: Boolean = False);
    procedure RefreshCatalogsSchemataListMIClick(Sender: TObject);
    procedure RebuildSchemaTree(MySQLMajorVersion: Integer; CatalogList: TMYX_CATALOGS;
      Catalog: TMYX_CATALOG; searchStr: WideString);

    procedure GetCatalogList(Sender: TObject);

    procedure FetchSchemaTables(Sender: TObject);
    procedure FetchSchemaTablesNoLock(Sender: TObject);
    procedure SchemaTablesFetched(Sender: TObject);
    procedure SchemaTablesFetchedForSearch(Sender: TObject);
    procedure RefreshTableNames(SchemaNode: TSchemaVirtualNode);
    procedure FillTableNamesDirectly(SchemaNode: TSchemaVirtualNode);

    procedure FetchSchemaIndices(Sender: TObject);
    procedure SchemaIndicesFetched(Sender: TObject);
    procedure RefreshIndicesNames(SchemaNode: TSchemaVirtualNode);

    procedure FillSpacerPnl;
    procedure CreateNewSchemaMIClick(Sender: TObject);
    procedure SchemaTreeViewPopupMenuPopup(Sender: TObject);
    procedure CreateNewTableMIClick(Sender: TObject);

    procedure EditTable(Catalog: WideString;
      Schema: WideString; Table: WideString);
    procedure CatalogVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure CatalogVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure CatalogVSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure CatalogVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CatalogVSTExpanding(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var Allowed: Boolean);
    procedure CatalogVSTChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ClearCatalogTree;

    procedure DoTableEditorApplyedChanges(Sender: TObject);

    procedure GetExpandedNodes(NodesList: TTntStringList;
      Tree: TVirtualStringTree; Node: PVirtualNode);
    function ReExpandedNodes(NodesList: TTntStringList; index: Integer;
      Tree: TVirtualStringTree; Node: PVirtualNode): Integer;
    function GetCatalogVSTFocusedObject(MatchClass: TClass = nil): TObject;
    procedure CatalogVSTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure CatalogVSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure DropMIClick(Sender: TObject);
    procedure EditMIClick(Sender: TObject);
    procedure CreateNewStoredProcedureMIClick(Sender: TObject);
    procedure CreateNewViewMIClick(Sender: TObject);
    procedure RenameMIClick(Sender: TObject);
    procedure SearchMIClick(Sender: TObject);
    procedure CopyAssetSQLMIClick(Sender: TObject);
    function GetAssetSQLCreateText: WideString;
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

    procedure SchemaPrivCountCallback(Sender: TObject);

  private
    SchemaPrivCount: Integer;

    LastCatalogName, LastSchemaName: WideString;

    FCreateTable,
    FEditTable,
    FSchemaTreeReloaded: TNotifyEvent;
    FLastVerticalOffset: Integer;

    FCreateView,
    FCreateStoredProcedure,
    FEditView,
    FEditStoredProcedure: TSchemaObjectEvent;

    TreeSpacerImgList: TImageList;

    TreeBtnOpenPNGImg,
    TreeBtnClosedPNGImg: TPNGObject;

    CatalogPNGImg,
    SchemaPNGImg,
    TablePNGImg,
    ColumnPKPNGImg,
    ColumnPNGImg,
    IndexPNGImg,
    UserPNGImg,
    SPPNGImg,
    ViewPNGImg: TPNGObject;

    ExpandedNodesList: TTntStringList;
    ThreadsDisabled: Boolean;

    FPaintDefaultSchemaBold: Boolean;

    FDefaultSchema: WideString;
    FDefaultSchemaNode: TSchemaVirtualNode;
    FMySQLConnection: TMySQLConn;
    FSearchTargets: TSearchTargets;
    FSearchLock: TCriticalSection;
    FSearchString: WideString;

    FOptionProvider: IOptionProvider;
    FShowEscapedNames: Boolean;
    FFetchTables: Boolean;

    function QC: WideString;

    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    function FetchMySQLMessages: WideString;
  protected
    procedure DoSearch(Thread: TThread);
    function GetOptionProvider: IOptionProvider;
    procedure PrepareSearch(Text: WideString);
    procedure SetShowSchemaAssets(ShowSchemaAssets: Boolean);
    function GetShowSchemaAssets: Boolean;
    procedure LoadOptions;
    procedure SaveOptions;

    procedure SetPopupMenuEditEntries(PopupMenuEditEntries: Boolean);
    function GetPopupMenuEditEntries: Boolean;
    procedure SetDefaultSchema(DefaultSchema: WideString);
    procedure SearchTerminated(Sender: TObject);
    procedure StopSearch;
    procedure FetchSchemaTablesFromNode(FMySQLConnection: TMySQLConn; SchemaNode: TSchemaVirtualNode;
      WithLock: Boolean = True);
  public
    CatalogList: TMYX_CATALOGS;
    SchemaPatterns: TTntStringList;
    AddedSchemataList: TObjectList;

    CurrentCatalog: TMYX_CATALOG;
    CurrentSchema: TMYX_SCHEMA;
    CurrentSchemaNodes: TList;
    CurrentTable: TMYX_SCHEMA_TABLE;

    //User must not expand SchemaTree manually
    LockSchemaTreeExpansion: Boolean;
    LockSchemaTreeSelection: Boolean;

    EditorTableForm: TEditorTableForm;

    ExpandSchemaOnLaunch: WideString;

    FShowSchemaAssets,
    FShowAssetsOnSchemaExpansion,
    FPopupMenuEditEntries: Boolean;

    SelectTableAfterTablesNodeExpand: WideString;

    procedure GetSelectedTables(List: TWidestringList);
    procedure ResetCatalogDisplay;

    procedure RefreshCatalogList(Sender: TObject); overload;
    procedure RefreshCatalogList(const FilterString: WideString); overload;

    property DefaultSchema: WideString read FDefaultSchema write SetDefaultSchema;
    property FetchTables: Boolean read FFetchTables write FFetchTables;
    property ShowEscapedNames: Boolean read FShowEscapedNames write FShowEscapedNames;
    property MySQLConnection: TMySQLConn read FMySQLConnection write FMySQLConnection;
    property PaintDefaultSchemaBold: Boolean read FPaintDefaultSchemaBold write FPaintDefaultSchemaBold;
    property PopupMenuEditEntries: Boolean read GetPopupMenuEditEntries write SetPopupMenuEditEntries;
    property ShowAssetsOnSchemaExpansion: Boolean read FShowAssetsOnSchemaExpansion write FShowAssetsOnSchemaExpansion;
    property ShowSchemaAssets: Boolean read GetShowSchemaAssets write SetShowSchemaAssets;

    property OnCreateTable: TNotifyEvent read FCreateTable write FCreateTable;
    property OnCreateView: TSchemaObjectEvent read FCreateView write FCreateView;
    property OnCreateStoredProcedure: TSchemaObjectEvent read FCreateStoredProcedure write FCreateStoredProcedure;
    property OnEditTable: TNotifyEvent read FEditTable write FEditTable;
    property OnEditView: TSchemaObjectEvent read FEditView write FEditView;
    property OnEditStoredProcedure: TSchemaObjectEvent read FEditStoredProcedure write FEditStoredProcedure;
    property OnSchemaTreeReloaded: TNotifyEvent read FSchemaTreeReloaded write FSchemaTreeReloaded;
  published
    property SearchTargets: TSearchTargets read FSearchTargets write FSearchTargets;
  end;

  TSchemaSubNodeType = (
    TSNTables,
    TSNIndices,
    TSNViews,
    TSNStoredProcedures,
    TSNEvents,
    TSNDevDocs,
    TSNUsers
  );

  TSchemaSubNode = class(TObject)
    constructor Create(Catalog: TMYX_CATALOG; Schema: TMYX_SCHEMA; SubNodeType: TSchemaSubNodeType);
  private
    FCatalog: TMYX_CATALOG;
    FSchema: TMYX_SCHEMA;
    FSubNodeType: TSchemaSubNodeType;
  published
    property Catalog: TMYX_CATALOG read FCatalog;
    property Schema: TMYX_SCHEMA read FSchema;
    property SubNodeType: TSchemaSubNodeType read FSubNodeType;
  end;

  TSchemaVirtualNode = class(TObject)
    constructor Create(Node: PVirtualNode; Schema: TMYX_SCHEMA);
  private
    FSchema: TMYX_SCHEMA;
    FNode: PVirtualNode;
  public
    property Schema: TMYX_SCHEMA read FSchema write FSchema;
    property Node: PVirtualNode read FNode write FNode;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  TypInfo, PNGTools;
  
{$R *.dfm}

const
  AllSearchTargets = [smSchemata, smTables, smColumns, smIndices, smStoredProcAndFunc, smViews, smTriggers];

type
  TSearchMethod = procedure(Thread: TThread) of object;

  TSearchThread = class(TThread)
  private
    FMethod: TSearchMethod; // This method is called in the context of the thread.
  protected
    procedure Execute; override;
  public
    constructor Create(Method: TSearchMethod);

    property Terminated;
  end;

var
  SearchThread: TSearchThread; // This is a singleton. There must be no two search threads at the same time!

//----------------- TSearchThread --------------------------------------------------------------------------------------

constructor TSearchThread.Create(Method: TSearchMethod);

begin
  inherited Create(True);

  FMethod := Method;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSearchThread.Execute;

begin
  if not Terminated then
    FMethod(Self);
end;

//----------------- TSchemaSubNote -------------------------------------------------------------------------------------

constructor TSchemaSubNode.Create(Catalog: TMYX_CATALOG; Schema: TMYX_SCHEMA; SubNodeType: TSchemaSubNodeType);

begin
  FCatalog := Catalog;
  FSchema := Schema;
  FSubNodeType := SubNodeType;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSchemaVirtualNode.Create(Node: PVirtualNode; Schema: TMYX_SCHEMA);

begin
  self.Node := Node;
  FSchema := Schema;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSchemataFrame.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  FMySQLConnection := nil;
  CatalogList := nil;
  FSearchLock := TCriticalSection.Create;
  SchemaPatterns := TTntStringList.Create;
  AddedSchemataList := TObjectList.Create;
  FShowEscapedNames := False;
  FFetchTables := True;

  ThreadsDisabled := False;

  //Create Lists
  CurrentSchemaNodes := TList.Create;

  //Init variables
  CurrentCatalog := nil; //Catalog;
  CurrentSchema := nil;
  CurrentTable := nil;
  LockSchemaTreeExpansion := True;
  LockSchemaTreeSelection := True;

  FDefaultSchemaNode := TSchemaVirtualNode.Create(nil, nil);
  FDefaultSchema := '';

  FPaintDefaultSchemaBold := False;

  FShowAssetsOnSchemaExpansion := False;
  FShowSchemaAssets := True;
  FPopupMenuEditEntries := True;

  ExpandSchemaOnLaunch := '';
  SelectTableAfterTablesNodeExpand := '';

  ExpandedNodesList := TTntStringList.Create;

  EditorTableForm := nil;
  FCreateTable := nil;
  FEditTable := nil;
  FSchemaTreeReloaded := nil;

  TreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  TreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  CatalogPNGImg := LoadPNGImageFromResource('catalog_16x16');
  SchemaPNGImg := LoadPNGImageFromResource('schema_16x16');
  TablePNGImg := LoadPNGImageFromResource('asset_table_16x16');
  ColumnPKPNGImg := LoadPNGImageFromResource('column_pk');
  ColumnPNGImg := LoadPNGImageFromResource('column');
  IndexPNGImg := LoadPNGImageFromResource('asset_index_16x16');
  UserPNGImg := LoadPNGImageFromResource('users_16x16');
  SPPNGImg := LoadPNGImageFromResource('asset_sp_16x16');
  ViewPNGImg := LoadPNGImageFromResource('asset_view_16x16');

  TreeSpacerImgList := GetTransparentImgList(10);
  CatalogVST.Images := TreeSpacerImgList;

  CatalogVST.NodeDataSize := Sizeof(Pointer);

  LoadOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSchemataFrame.Destroy;

begin
  CurrentSchemaNodes.Free;

  if (EditorTableForm <> nil) then
    EditorTableForm.Free;

  if (CatalogList <> nil) then
    CatalogList.Free;

  FDefaultSchemaNode.Free;

  SchemaPatterns.Free;
  AddedSchemataList.Free;

  TreeSpacerImgList.Free;

  TreeBtnOpenPNGImg.Free;
  TreeBtnClosedPNGImg.Free;

  CatalogPNGImg.Free;
  SchemaPNGImg.Free;
  IndexPNGImg.Free;
  TablePNGImg.Free;
  ColumnPKPNGImg.Free;
  ColumnPNGImg.Free;
  UserPNGImg.Free;
  SPPNGImg.Free;
  ViewPNGImg.Free;

  ExpandedNodesList.Free;
  FSearchLock.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.PrepareSearch(Text: WideString);

// Prepares a search run by stopping any pending run, keeping the new search type and string for the search thread and
// starting a timer that is used to collect number of characters before the actual search starts.
// If the search string is empty then no new search run is prepared.

begin
  StopSearch;

  if Length(Text) > 0 then
  begin
    FSearchString := Text;
    SetTimer(Handle, SearchTimer, SearchTimeout, nil);
  end
  else
    // No search is active anymore. Make everything visible again in the treeview.
    ResetCatalogDisplay;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SetShowSchemaAssets(ShowSchemaAssets: Boolean);

begin
  FShowSchemaAssets := ShowSchemaAssets;

  if (not (FShowSchemaAssets) and (not (FShowAssetsOnSchemaExpansion))) then
    CatalogVST.Images := nil
  else
    CatalogVST.Images := TreeSpacerImgList;

  if (FShowSchemaAssets) or (ShowAssetsOnSchemaExpansion) then
    AdvancedEdit.PopupMenu := SchemaSearchPopupMenu
  else
    AdvancedEdit.PopupMenu := nil;

  AdvancedEdit.Invalidate;

  {if (ShowSchemaAssets) then
  begin
    EditMI.Visible := True;
    N1.Visible := True;
  end
  else
  begin
    EditMI.Visible := False;
    N1.Visible := False;
  end;}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.DoSearch(Thread: TThread);

// Searches through the catalog treeview and hides all nodes whose captions do not fit the search text.

var
  Node: PVirtualNode;
  Text: WideString;
  NodeData: ^TObject;
  SchemaNode: TSchemaVirtualNode;
  
begin
  CatalogVST.Cursor := crAppStart;
  FSearchLock.Acquire;
  try
    // First hide all nodes in one rush.
    CatalogVST.BeginUpdate;
    try
      // Use the NoInit methods for walking the tree. This way only already existing nodes are traversed.
      // No need to make nodes visible that aren't loaded yet.
      Node := CatalogVST.GetFirstNoInit;
      while Assigned(Node) do
      begin
        CatalogVST.IsVisible[Node] := False;
        Node := CatalogVST.GetNextNoInit(Node);
      end;
    finally
      CatalogVST.EndUpdate;
    end;

    if FMySQLConnection.MySQL = nil then
      FMySQLConnection.Reconnect;
    
    // Reshow nodes step by step if they match, giving so a kind of progress when searching.
    Node := CatalogVST.GetFirst;
    while Assigned(Node) and not TSearchThread(Thread).Terminated do
    begin
      // Check each node type and consider it only if it is allowed to be searched.
      NodeData := CatalogVST.GetNodeData(Node);
      if NodeData^ is TMYX_SCHEMA then
      begin
        // Schema nodes need a bit more work.
        if smSchemata in FSearchTargets then
        begin
          CatalogVSTGetText(CatalogVST, Node, 0, ttNormal, Text);
          CatalogVST.FullyVisible[Node] := myx_match_pattern(Text, FSearchString, 0, 0) <> 0;
        end;

        // Check if the schema node's subdata has been loaded already.
        if FFetchTables and (TMYX_SCHEMA(NodeData^).schema_tables = nil) then
        begin
          SchemaNode := TSchemaVirtualNode.Create(Node, TMYX_SCHEMA(NodeData^));
          try
            FMySQLConnection.FetchData(dkSchemaTables, FetchSchemaTablesNoLock, SchemaTablesFetchedForSearch,
              SchemaNode, _('Fetching assets ...'), False, True, True, SearchThread);
          finally
            SchemaNode.Free;
          end;
        end;
      end
      else
      begin
        // All other nodes.
        // TODO: views and triggers must yet be considered for search once they are available.
        if (smTables in FSearchTargets) and (NodeData^ is TMYX_SCHEMA_TABLE) or
          (smColumns in FSearchTargets) and ((NodeData^ is TMYX_TABLE_INDEX_COLUMN) or (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)) or
          (smIndices in FSearchTargets) and (NodeData^ is TMYX_TABLE_INDEX) or
          (smStoredProcAndFunc in FSearchTargets) and (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) or
          (smColumns in FSearchTargets) and (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
        begin
          CatalogVSTGetText(CatalogVST, Node, 0, ttNormal, Text);
          CatalogVST.FullyVisible[Node] := myx_match_pattern(Text, FSearchString, 0, 0) <> 0;
        end;
      end;
      Node := CatalogVST.GetNext(Node);
    end;
  finally
    FSearchLock.Release;
    CatalogVST.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.GetOptionProvider: IOptionProvider;

// Gets an option provider from one of our owners, if there's any.
// Returns nil if none could be found.

var
  Intf: IAdaptable;
  Component: TComponent;

begin
  Result := nil;
  Component := Self;
  while Component.Owner <> nil do
  begin
    if (Component is TCustomForm) and Supports(Component, IAdaptable, Intf) then
    begin
      Result := Intf.GetAdapter(IOptionProvider) as IOptionProvider;
      Break;
    end;

    Component := Component.Owner;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.GetShowSchemaAssets: Boolean;

begin
  Result := FShowSchemaAssets;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.LoadOptions;

var
  S: string;

begin
  // Read the search options.
  FOptionProvider := GetOptionProvider;
  if Assigned(FOptionProvider) then
  begin
    S := FOptionProvider.OptionAsString['SchemaSearchTargets'];
    if (S='')then
      S := 'smSchemata';

    SetSetProp(Self, GetPropInfo(Self, 'SearchTargets'), S);

    // Adjust menu entries according to the options.
    SchemataSubMI.Checked := smSchemata in FSearchTargets;
    TableSubMI.Checked := smTables in FSearchTargets;
    ColumnsSubMI.Checked := smColumns in FSearchTargets;
    SPSubMI.Checked := smStoredProcAndFunc in FSearchTargets;
    ViewsSubMI.Checked := smViews in FSearchTargets;
    TriggersSubMI.Checked := smTriggers in FSearchTargets;
    IndicesSubMI.Checked := smIndices in FSearchTargets;

    if FSearchTargets = AllSearchTargets then
      SearchAllMI.Checked := True
    else
      if FSearchTargets = [smSchemata] then
        SearchSchemataMI.Checked := True
      else
        if FSearchTargets = [smTables, smStoredProcAndFunc, smViews, smTriggers] then
          SearchAssetsMI.Checked := True
        else
          if FSearchTargets = [smColumns, smIndices] then
            SearchColumnsMI.Checked := True
          else
            CustomMI.Checked := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SaveOptions;

var
  Provider: IOptionProvider;
  S: string;

begin
  // Store the new search options.
  Provider := GetOptionProvider;
  if Assigned(Provider) then
  begin
    S := GetSetProp(Self, GetPropInfo(Self, 'SearchTargets'));
    Provider.OptionAsString['SchemaSearchTargets'] := S;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SetPopupMenuEditEntries(PopupMenuEditEntries: Boolean);

begin
  FPopupMenuEditEntries := PopupMenuEditEntries;

  if (not (FPopupMenuEditEntries)) then
  begin
    EditMI.Visible := False;
    DropMI.Visible := False;
    CreateNewSchemaMI.Visible := False;
    CreateNewTableMI.Visible := False;
    CreateNewViewMI.Visible := False;
    CreateNewStoredProcedureMI.Visible := False;
    N1.Visible := False;
  end
  else
  begin
    EditMI.Visible := True;
    DropMI.Visible := True;
    CreateNewSchemaMI.Visible := True;
    CreateNewTableMI.Visible := True;
    CreateNewViewMI.Visible := True;
    CreateNewStoredProcedureMI.Visible := True;
    N1.Visible := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.GetPopupMenuEditEntries: Boolean;

begin
  Result := FPopupMenuEditEntries;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FillSpacerPnl;

var
  Shape: TTntShape;

begin
  Shape := TTntShape.Create(self);
  Shape.Parent := SpacerPnl;
  Shape.Align := alClient;
  Shape.Pen.Color := clWhite;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.ReloadSchemaTree;

begin
  FillSchemaTree('', True);

  if (Assigned(FSchemaTreeReloaded)) then
    FSchemaTreeReloaded(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.GetExpandedNodes(NodesList: TTntStringList; Tree: TVirtualStringTree; Node: PVirtualNode);

var
  caption, state: WideString;
  NodeData: ^TObject;

begin
  if (Node = nil) then
    Exit;

  if (Tree.Expanded[Node]) or
    (Tree.Selected[Node]) or
    (Tree.FocusedNode = Node) then
  begin
    caption := '';
    if (Assigned(Tree.OnGetText)) then
      Tree.OnGetText(TBaseVirtualTree(Tree), Node, 0, ttNormal, caption)
    else
      Exit;

    state := '';
    if (Tree.Selected[Node]) then
      state := 'S' + state;
    if (Tree.Expanded[Node]) then
      state := 'E' + state;
    if (Tree.FocusedNode = Node) then
      state := 'F' + state;

    NodeData := Tree.GetNodeData(Node);
    if (NodeData <> nil) then
      if (NodeData^ <> nil) then
        if (NodeData^ is TMYX_SCHEMA) then
          state := '1' + state
        else
          if (NodeData^ is TMYX_SCHEMA_TABLE) then
            state := '2' + state
          else
            if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
              state := '3' + state;

    if (caption <> '') then
      NodesList.Add(state + '=' + caption);
  end;

  //Get next Child
  GetExpandedNodes(NodesList, Tree, Node.FirstChild);

  //Get next Sibling
  GetExpandedNodes(NodesList, Tree, Node.NextSibling);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.ReExpandedNodes(NodesList: TTntStringList; index: Integer;
  Tree: TVirtualStringTree; Node: PVirtualNode): Integer;
var
  caption: WideString;
  NodeData: ^TObject;
  NodeType: WideString;
begin
  if (Node = nil) or (index >= NodesList.Count) then
  begin
    Result := index;
    Exit;
  end;

  caption := '';
  if (Assigned(Tree.OnGetText)) then
    Tree.OnGetText(TBaseVirtualTree(Tree), Node, 0, ttNormal, caption)
  else
  begin
    Result := index;
    Exit;
  end;

  if (caption = NodesList.ValueFromIndex[index]) then
  begin
    //Check NodeType
    NodeData := Tree.GetNodeData(Node);
    NodeType := '-';

    if (NodeData <> nil) then
      if (NodeData^ <> nil) then
        if (NodeData^ is TMYX_SCHEMA) then
          NodeType := '1'
        else
          if (NodeData^ is TMYX_SCHEMA_TABLE) then
            NodeType := '2'
          else
            if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
              NodeType := '3';

    if (Pos(NodeType, NodesList.Names[index]) > 0) then
    begin
      if (Pos('E', NodesList.Names[index]) > 0) then
      begin
        if (NodeType = '1') then
          RefreshTableNames(TSchemaVirtualNode.Create(Node, TMYX_SCHEMA(NodeData^)))
        else
          Tree.Expanded[Node] := True;
      end;

      if (Pos('S', NodesList.Names[index]) > 0) then
        Tree.Selected[Node] := True;

      if (Pos('F', NodesList.Names[index]) > 0) then
        Tree.FocusedNode := Node;

      inc(index);
    end;
  end;

  //Get next Child
  index := ReExpandedNodes(NodesList, index, Tree, Node.FirstChild);

  //Get next Sibling
  index := ReExpandedNodes(NodesList, index, Tree, Node.NextSibling);

  //If the node was not found, continue with the next
  {if(Found=0)then
  begin
    Result := ReExpandedNodes(NodesList, StartIndex+1, Tree, Node);
  end
  else}
  Result := index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FillSchemaTree(searchStr: WideString; ReloadSchemata: Boolean);

begin
  if Assigned(FMySQLConnection) and FMySQLConnection.Connected then
  begin
    if (CatalogList = nil) or (ReloadSchemata) then
    begin
      //Store current selected and expanded nodes
      ExpandedNodesList.Clear;
      GetExpandedNodes(ExpandedNodesList,
        CatalogVST, CatalogVST.GetFirst);

      FLastVerticalOffset := CatalogVST.OffsetY;
      ClearCatalogTree;

      if (CatalogList <> nil) then
      begin
        CatalogList.Free;
        CatalogList := nil;
      end;

      // (Re-)load CatalogList, AllowDuplicatedKindOfData
      FMySQLConnection.FetchData(dkCatalogSchema, GetCatalogList, RefreshCatalogList, nil,
        _('Fetching Catalogs/Schemata data ...'), False, True);
    end
    else
    begin
      // Refresh SchemaTree
      LockSchemaTreeExpansion := False;
      try
        ExpandSchemaOnLaunch := FDefaultSchema;
        RebuildSchemaTree(FMySQLConnection.MajorVersion, CatalogList, CurrentCatalog, searchStr);
      finally
        LockSchemaTreeExpansion := True;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.GetCatalogList(Sender: TObject);

var
  Catalogs: PMYX_CATALOGS;

begin
  Catalogs := myx_get_catalogs(TFetchDataThread(Sender).Connection.MySQL);
  try
    CatalogList := TMYX_CATALOGS.Create(Catalogs);
  finally
    myx_free_catalogs(Catalogs);
  end;

  if Catalogs = nil then
    raise EMyxMultiSQLError.Create(_('Could not fetch catalogs/schemata data.'), FetchMySQLMessages);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RefreshCatalogList(Sender: TObject);

begin
  FillSchemaTree();

  // Update the current schema reference.
  DefaultSchema := FDefaultSchema;
  CatalogVST.OffsetY := FLastVerticalOffset;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RefreshCatalogList(const FilterString: WideString);

var
  LastFocused: WideString;
  Node: PVirtualNode;

begin
  if Assigned(CatalogVST.FocusedNode) then
    LastFocused := CatalogVST.Text[CatalogVST.FocusedNode, -1];
  FillSchemaTree(FilterString);

  // Update the current schema reference.
  DefaultSchema := FDefaultSchema;
  CatalogVST.OffsetY := FLastVerticalOffset;

  // Find and select last focused node again.
  if LastFocused <> '' then
  begin
    Node := CatalogVST.GetFirst;
    while Assigned(Node) do
    begin
      if WideSameText(LastFocused, CatalogVST.Text[Node, -1]) then
      begin
        CatalogVST.FocusedNode := Node;
        CatalogVST.Selected[Node] := True;
        Break;
      end;

      Node := CatalogVST.GetNext(Node);
    end;
  end;
  
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RefreshCatalogsSchemataListMIClick(Sender: TObject);

begin
  ReloadSchemaTree;
end;

//----------------------------------------------------------------------------------------------------------------------

function StrMatchesSearchStr(Str: WideString; searchStr: WideString): Boolean;

begin
  StrMatchesSearchStr := True;

  if (searchStr <> '') then
    StrMatchesSearchStr := (myx_match_pattern(Str, searchStr, 0, 0) <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function AddSchemaToCatalogTree(MySQLMajorVersion: Integer; TreeView: TVirtualStringTree;
  CatalogNode: PVirtualNode; Catalog: TMYX_CATALOG; Schema: TMYX_SCHEMA;
  searchStr: WideString; ShowSchemaAssets: Boolean;
  ShowTablesOnSchemaExpansion: Boolean): PVirtualNode;
var
  SchemaNode, Node: PVirtualNode;
begin
  SchemaNode := TreeView.AddChild(CatalogNode, Schema);

  if (ShowSchemaAssets) then
  begin
    //Add SchemaSubNodes

    //Add Tables node and subnode dummy, so the user can expand the node
    Node := TreeView.AddChild(SchemaNode,
      TSchemaSubNode.Create(Catalog, Schema, TSNTables));

    TreeView.AddChild(Node);

    //Add Indices
    Node := TreeView.AddChild(SchemaNode,
      TSchemaSubNode.Create(Catalog, Schema, TSNIndices));

    TreeView.AddChild(Node);

    //MySQL 5.0 (not now)
    if (MySQLMajorVersion >= 5 + 100) then
    begin
      TreeView.AddChild(SchemaNode,
        TSchemaSubNode.Create(Catalog, Schema, TSNViews));
      TreeView.AddChild(SchemaNode,
        TSchemaSubNode.Create(Catalog, Schema, TSNStoredProcedures));
      TreeView.AddChild(SchemaNode,
        TSchemaSubNode.Create(Catalog, Schema, TSNEvents));
    end;

    {AddTreeViewChildNode(TreeView, SchemaNode, 'Developer Docs', 18,
      TSchemaSubNode.Create(Schema, TSNDevDocs));}

    //Add Users
    TreeView.AddChild(SchemaNode,
      TSchemaSubNode.Create(Catalog, Schema, TSNUsers));
  end
  else
    if (ShowTablesOnSchemaExpansion) then
      TreeView.AddChild(SchemaNode);

  Result := SchemaNode;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RebuildSchemaTree(MySQLMajorVersion: Integer; CatalogList: TMYX_CATALOGS; Catalog: TMYX_CATALOG;
  searchStr: WideString);
  
var
  I, J, K: Integer;
  SchemaName: WideString;
  Found: boolean;
  PCatalogNode, PNode: PVirtualNode;
  AddedSchema: TMYX_SCHEMA;

begin
  if (CatalogList = nil) then
    Exit;

  PCatalogNode := nil;

  FSearchLock.Acquire;
  try
    CatalogVST.BeginUpdate;
    try
      ClearCatalogTree;

      if (Catalog = nil) then
      begin
        for I := 0 to CatalogList.Catalogs.Count - 1 do
        begin
          for J := 0 to CatalogList.Catalogs[I].schemata.Count - 1 do
            if (StrMatchesSearchStr(
              CatalogList.Catalogs[I].schemata[J].schema_name, searchStr)) then
            begin
              PNode := AddSchemaToCatalogTree(MySQLMajorVersion, CatalogVST,
                PCatalogNode, CatalogList.Catalogs[I],
                CatalogList.Catalogs[I].schemata[J], searchStr,
                FShowSchemaAssets, FShowAssetsOnSchemaExpansion);

              if (CatalogList.Catalogs[I].catalog_name = LastCatalogName) and
                (CatalogList.Catalogs[I].schemata[J].schema_name = LastSchemaName) then
              begin
                CatalogVST.Selected[PNode] := True;
                CatalogVST.FocusedNode := PNode;
              end;

              if (CompareText(ExpandSchemaOnLaunch,
                CatalogList.Catalogs[I].schemata[J].schema_name) = 0) then
              begin
                ExpandSchemaOnLaunch := '';
                CatalogVST.Expanded[PNode] := True;
              end;
            end;

          for J := 0 to SchemaPatterns.Count - 1 do
          begin
            if (CompareText(
              Copy(SchemaPatterns[J], 1, Pos('/', SchemaPatterns[J]) - 1),
              CatalogList.Catalogs[I].catalog_name) = 0) then
            begin
              Found := False;
              SchemaName := Copy(SchemaPatterns[J], Pos('/', SchemaPatterns[J]) + 1,
                Length(SchemaPatterns[J]));

              for K := 0 to CatalogList.Catalogs[I].schemata.Count - 1 do
                if (CompareText(SchemaName,
                  CatalogList.Catalogs[I].schemata[K].escaped_schema_name) = 0) then
                begin
                  Found := True;
                  break;
                end;

              if not Found and StrMatchesSearchStr(SchemaName, searchStr) then
              begin
                // Patterns for user rights must not be impicitely escaped. The user must do this explicitely.
                AddedSchema := TMYX_SCHEMA.create(SchemaName, SchemaName, CatalogList.Catalogs[I].catalog_name,
                  nil, nil, nil);

                AddedSchemataList.Add(AddedSchema);

                PNode := AddSchemaToCatalogTree(MySQLMajorVersion, CatalogVST,
                  PCatalogNode, CatalogList.Catalogs[I],
                  AddedSchema, searchStr, FShowSchemaAssets,
                  FShowAssetsOnSchemaExpansion);

                if (CatalogList.Catalogs[I].catalog_name = LastCatalogName) and
                  (SchemaName = LastSchemaName) then
                begin
                  CatalogVST.FocusedNode := PNode;
                  CatalogVST.ClearSelection;
                  CatalogVST.Selected[PNode] := True;
                end;
              end;
            end;
          end;

          if (PCatalogNode <> nil) then
            if (CatalogVST.ChildCount[PCatalogNode] > 0) then
              CatalogVST.Expanded[PCatalogNode] := True
            else
              CatalogVST.DeleteNode(PCatalogNode);
        end;
        CatalogVST.Sort(PCatalogNode, -1, sdAscending);
      end;

      //Re-expand tree nodes
      ThreadsDisabled := True;
      try
        ReExpandedNodes(ExpandedNodesList, 0, CatalogVST, CatalogVST.GetFirst);
      finally
        ThreadsDisabled := False;
      end;
    finally
      CatalogVST.EndUpdate;
    end;

    if (CatalogVST.FocusedNode <> nil) then
      CatalogVST.ScrollIntoView(CatalogVST.FocusedNode, True);

    SetDefaultSchema(FDefaultSchema);
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FetchSchemaTablesFromNode(FMySQLConnection: TMySQLConn; SchemaNode: TSchemaVirtualNode;
  WithLock: Boolean = True);

var
  SchemaTables: PMYX_SCHEMA_TABLES;
  SchemaSPs: PMYX_SCHEMA_STORED_PROCEDURES;

begin
  if WithLock then
    FSearchLock.Acquire;
  try
    SchemaTables := myx_get_schema_tables(FMySQLConnection.MySQL, '', SchemaNode.Schema.schema_name);
    try
      SchemaNode.Schema.schema_tables := TMYX_SCHEMA_TABLES.create(SchemaTables);
    finally
      myx_free_schema_tables(SchemaTables);
    end;

    SchemaSPs := myx_get_schema_sps(FMySQLConnection.MySQL, '', SchemaNode.Schema.schema_name);
    try
      SchemaNode.Schema.schema_sps := TMYX_SCHEMA_STORED_PROCEDURES.create(SchemaSPs);
    finally
      myx_free_schema_sps(SchemaSPs);
    end;

    if SchemaTables = nil then
      raise EMyxMultiSQLError.Create(_('Could not fetch schema tables.'), FetchMySQLMessages)
    else
      if SchemaSPs = nil then
        raise EMyxMultiSQLError.Create(_('Could not fetch stored procedures.'), FetchMySQLMessages);
  finally
    if WithLock then
      FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.FetchMySQLMessages: WideString;

// Reads all warnings and errors that have been caused by the last command.

var
  Messages: PMYX_MYSQL_ERROR_MSGS;
  Message: PMYX_MYSQL_ERROR_MSG;
  I: Integer;

begin
  Result := '';
  Messages := myx_mysql_error_msgs_fetch(FMySQLConnection.MySQL);

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

procedure TSchemataFrame.FetchSchemaTables(Sender: TObject);

begin
  // Do not allow the user to start a search until this operation is finished.
  AdvancedEdit.Enabled := False;

  FetchSchemaTablesFromNode(TFetchDataThread(Sender).Connection, TSchemaVirtualNode(TFetchDataThread(Sender).Target));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FetchSchemaTablesNoLock(Sender: TObject);

begin
  FetchSchemaTablesFromNode(TFetchDataThread(Sender).Connection, TSchemaVirtualNode(TFetchDataThread(Sender).Target), False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SchemaTablesFetched(Sender: TObject);

begin
  RefreshTableNames(TSchemaVirtualNode(TFetchDataThread(Sender).Target));

  // Reenabled seach input.
  AdvancedEdit.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SchemaTablesFetchedForSearch(Sender: TObject);

begin
  FillTableNamesDirectly(TSchemaVirtualNode(TFetchDataThread(Sender).Target));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RefreshTableNames(SchemaNode: TSchemaVirtualNode);

var
  I, J: Integer;
  TreeNode: PVirtualNode;
  WasExpanded: Boolean;

begin
  if (SchemaNode.schema.schema_tables = nil) and (not (ThreadsDisabled)) then
    FMySQLConnection.FetchData(dkSchemaTables, FetchSchemaTables, SchemaTablesFetched, SchemaNode,
      _('Fetching assets ...'), False, False)
  else
  begin
    //If ThreadsDisabled is turned on, fetch SchemaTables now
    if (SchemaNode.schema.schema_tables = nil) and (ThreadsDisabled) then
      FetchSchemaTablesFromNode(FMySQLConnection, SchemaNode);

    FSearchLock.Acquire;
    try
      CatalogVST.BeginUpdate;
      try
        // Attach tables to schema node
        WasExpanded := CatalogVST.Expanded[SchemaNode.Node];
        CatalogVST.DeleteChildren(SchemaNode.Node);
        for I := 0 to SchemaNode.schema.schema_tables.schema_tables.Count - 1 do
        begin
          TreeNode := CatalogVST.AddChild(SchemaNode.Node, SchemaNode.schema.schema_tables.schema_tables[I]);

          for J := 0 to SchemaNode.schema.schema_tables.schema_tables[I].columns.Count - 1 do
            CatalogVST.AddChild(TreeNode,
              SchemaNode.schema.schema_tables.schema_tables[I].columns[J]);

          if (SelectTableAfterTablesNodeExpand <> '') then
            if (SchemaNode.schema.schema_tables.schema_tables[I].table_name =
              SelectTableAfterTablesNodeExpand) then
            begin
              CatalogVST.ClearSelection;
              CatalogVST.Selected[TreeNode] := True;
              CatalogVST.FocusedNode := TreeNode;
              CatalogVST.ScrollIntoView(TreeNode, False);

              SelectTableAfterTablesNodeExpand := '';
            end;
        end;

        for I := 0 to SchemaNode.Schema.schema_sps.schema_sps.Count-1 do
          with SchemaNode.schema.schema_sps do
          begin
            TreeNode := CatalogVST.AddChild(SchemaNode.Node, schema_sps[I]);
            for J := 0 to schema_sps[I].params.Count-1 do
              CatalogVST.AddChild(TreeNode, schema_sps[I].params[J]);
          end;

        // Restore expansion state.
        CatalogVST.Expanded[SchemaNode.Node] := WasExpanded;
      finally
        CatalogVST.EndUpdate;
        CatalogVST.Sort(SchemaNode.Node, -1, sdAscending);
        SchemaNode.Free;
      end;
    finally
      FSearchLock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FillTableNamesDirectly(SchemaNode: TSchemaVirtualNode);

// This is a specialized method to fill table nodes for a schema. It is only called during search.

var
  I, J: Integer;
  TreeNode: PVirtualNode;
  
begin
  CatalogVST.BeginUpdate;
  try
    // Attach tables to schema node
    CatalogVST.DeleteChildren(SchemaNode.Node);
    for I := 0 to SchemaNode.schema.schema_tables.schema_tables.Count - 1 do
    begin
      TreeNode := CatalogVST.AddChild(SchemaNode.Node, SchemaNode.schema.schema_tables.schema_tables[I]);

      for J := 0 to SchemaNode.schema.schema_tables.schema_tables[I].columns.Count - 1 do
        CatalogVST.AddChild(TreeNode, SchemaNode.schema.schema_tables.schema_tables[I].columns[J]);
    end;

    for I := 0 to SchemaNode.Schema.schema_sps.schema_sps.Count-1 do
      with SchemaNode.schema.schema_sps do
      begin
        TreeNode := CatalogVST.AddChild(SchemaNode.Node, schema_sps[I]);
        for J := 0 to schema_sps[I].params.Count-1 do
          CatalogVST.AddChild(TreeNode, schema_sps[I].params[J]);
      end;

  finally
    CatalogVST.EndUpdate;
    CatalogVST.Sort(SchemaNode.Node, -1, sdAscending);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.FetchSchemaIndices(Sender: TObject);

var
  SchemaIndices: PMYX_SCHEMA_INDICES;

begin
  SchemaIndices := myx_get_schema_indices(TFetchDataThread(Sender).Connection.MySQL,
    '', TSchemaVirtualNode(TFetchDataThread(Sender).Target).Schema.schema_name);

  try
    TSchemaVirtualNode(TFetchDataThread(Sender).Target).Schema.schema_indices :=
      TMYX_SCHEMA_INDICES.create(SchemaIndices);
  finally
    myx_free_schema_indices(SchemaIndices);
  end;

  if SchemaIndices = nil then
    raise EMyxMultiSQLError.Create(_('Could not fetch schema indices.'), FetchMySQLMessages);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SchemaIndicesFetched(Sender: TObject);

begin
  RefreshIndicesNames(TSchemaVirtualNode(TFetchDataThread(Sender).Target));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.RefreshIndicesNames(SchemaNode: TSchemaVirtualNode);

var
  I, J: Integer;
  IndexNode: PVirtualNode;

begin
  if SchemaNode.schema.schema_indices = nil then
    FMySQLConnection.FetchData(dkSchemaIndices, FetchSchemaIndices, SchemaIndicesFetched, SchemaNode,
      'Fetching Indices ...', False, True)
  else
  begin
    FSearchLock.Acquire;
    try
      CatalogVST.BeginUpdate;
      try
        // Attach tables to schema node.
        CatalogVST.DeleteChildren(SchemaNode.Node);
        for I := 0 to SchemaNode.schema.schema_indices.indices.Count - 1 do
        begin
          IndexNode := CatalogVST.AddChild(SchemaNode.Node, SchemaNode.schema.schema_indices.indices[I]);

          for J := 0 to SchemaNode.schema.schema_indices.indices[I].index_columns.Count - 1 do
            CatalogVST.AddChild(IndexNode, SchemaNode.schema.schema_indices.indices[I].index_columns[J]);

          CatalogVST.Expanded[SchemaNode.Node] := True;
        end;
      finally
        CatalogVST.EndUpdate;
        CatalogVST.Sort(SchemaNode.Node, -1, sdAscending);
        SchemaNode.Free;
      end;
    finally
      FSearchLock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CreateNewSchemaMIClick(Sender: TObject);

var
  name: WideString;

begin
  name := '';

  if (ShowModalEditDialog(_('Create new Schema'), _('Please enter a name for the new schema.'), myx_mtEdit, _('OK') +
    #13#10 + _('Cancel'), True, _('Schema name:'), name) = 1) then
  begin
    if name <> '' then
    begin
      FMySQLConnection.ExecuteDirect('CREATE DATABASE `' + name + '`', 1000);
      ReloadSchemaTree;
      FMySQLConnection.SchemaListChanged;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SchemaTreeViewPopupMenuPopup(Sender: TObject);

var
  NodeData: ^TObject;

begin
  if (CatalogVST.FocusedNode<>nil) and (CurrentSchema <> nil) then
  begin
    NodeData := CatalogVST.GetNodeData(CatalogVST.FocusedNode);

    if (NodeData<>nil) and (NodeData^<>nil) then
    begin
      EditMI.Enabled := 
        ((NodeData^ is TMYX_SCHEMA_TABLE) and
          (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_BASE_TABLE)) or
        ((NodeData^ is TMYX_SCHEMA_TABLE) and
          (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_VIEW) and
          (Assigned(FEditView))) or
        ((NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) and
          (Assigned(FEditStoredProcedure)));

      DropMI.Enabled := (NodeData^ is TMYX_SCHEMA) or
        (NodeData^ is TMYX_SCHEMA_TABLE) or
        (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE);

      if (NodeData^ is TMYX_SCHEMA) then
      begin
        EditMI.Caption := _('Edit Schema');
        DropMI.Caption := _('Drop Schema');
        RenameMI.Caption := _('Rename Schema');
      end
      else
        if ((NodeData^ is TMYX_SCHEMA_TABLE) and
          (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_BASE_TABLE)) then
        begin
          EditMI.Caption := _('Edit Table');
          DropMI.Caption := _('Drop Table');
          RenameMI.Caption := _('Rename Table');
        end
        else
          if ((NodeData^ is TMYX_SCHEMA_TABLE) and
            (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_VIEW)) then
          begin
            EditMI.Caption := _('Edit View');
            DropMI.Caption := _('Drop View');
            RenameMI.Caption := _('Rename View');
          end
          else
            if ((NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) and
              (TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_PROCEDURE)) then
            begin
              EditMI.Caption := _('Edit Procedure');
              DropMI.Caption := _('Drop Procedure');
              RenameMI.Caption := _('Rename Procedure');
            end
            else
              if ((NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) and
                (TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_FUNCTION)) then
              begin
                EditMI.Caption := _('Edit Function');
                DropMI.Caption := _('Drop Function');
                RenameMI.Caption := _('Rename Function');
              end
              else
              begin
                EditMI.Caption := _('Edit');
                DropMI.Caption := _('Drop');
                RenameMI.Caption := _('Rename');
              end;
    end;
  end
  else
  begin
    EditMI.Enabled := False;
    DropMI.Enabled := False;
    RenameMI.Enabled := False;
  end;

      if FMySQLConnection<>nil then
      begin
        // 4.0
        if (FMySQLConnection.MajorVersion = 4) and (FMySQLConnection.MinorVersion = 0) then
        begin
        end
        else
          // >=4.1 <5.0
          if (FMySQLConnection.MajorVersion = 4) and (FMySQLConnection.MinorVersion > 0) then
          begin
          end
          else
            // 5.x
            if FMySQLConnection.MajorVersion = 5 then
            begin
            end

      end;

  if FMySQLConnection<>nil then
  begin
    CreateNewTableMI.Enabled := (CurrentSchema <> nil);
    CreateNewViewMI.Enabled := (CurrentSchema <> nil) and Assigned(FCreateView) and
      (FMySQLConnection.MajorVersion >= 5);
    CreateNewStoredProcedureMI.Enabled := (CurrentSchema <> nil) and Assigned(FCreateStoredProcedure) and
      (FMySQLConnection.MajorVersion >= 5);
  end
  else
  begin
    CreateNewTableMI.Enabled := False;
    CreateNewViewMI.Enabled := False;
    CreateNewStoredProcedureMI.Enabled := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CreateNewTableMIClick(Sender: TObject);

begin
  if (Assigned(FCreateTable)) then
    FCreateTable(self)
  else
  begin
    if (CurrentCatalog = nil) then
      EditTable('', CurrentSchema.schema_name, '')
    else
      EditTable(CurrentCatalog.catalog_name, CurrentSchema.schema_name, '');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.DoTableEditorApplyedChanges(Sender: TObject);

begin
  ReloadSchemaTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.EditTable(Catalog: WideString; Schema: WideString; Table: WideString);

begin
  if Assigned(MYXCommonOptions.Datatypes) then
  begin
    if (EditorTableForm = nil) then
    begin
      EditorTableForm := TEditorTableForm.Create(self);
      EditorTableForm.SetEditMode(DBMEditMode_Online, FMySQLConnection);
      EditorTableForm.OnApplyedChanges := DoTableEditorApplyedChanges
    end;

    EditorTableForm.SetDatabaseVersion(
      myx_get_mysql_major_version(FMySQLConnection.MySQL),
      myx_get_mysql_minor_version(FMySQLConnection.MySQL));

    EditorTableForm.EditSchemaTable(Catalog, Schema, Table,
      CurrentSchema.schema_tables);

    EditorTableForm.Show;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CatalogVSTAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  NodeData: ^TObject;
  TxtRect: TRect;
  x: Integer;
  PNGImg: TPNGObject;
begin
  NodeData := Sender.GetNodeData(Node);
  if (NodeData^ <> nil) then
  begin
    TxtRect := Sender.GetDisplayRect(Node, Column, True);

    x := TxtRect.Left - Sender.OffsetX;

    //Draw >
    if (Node.ChildCount > 0) or (vsHasChildren in Node.States) then
    begin
      if (Sender.Expanded[Node]) then
        TreeBtnOpenPNGImg.Draw(TargetCanvas,
          Rect(x - 16 - 12 - 1, CellRect.Top + 4, x - 16 - 4 - 1, CellRect.Top + 16 + 4))
      else
        TreeBtnClosedPNGImg.Draw(TargetCanvas,
          Rect(x - 16 - 12 - 1, CellRect.Top + 4, x - 16 - 4 - 1, CellRect.Top + 16 + 4))
    end;

    PNGImg := nil;
    if (NodeData^ is TMYX_SCHEMA) then
      PNGImg := SchemaPNGImg
    else
      if (NodeData^ is TSchemaSubNode) then
      begin
        if (TSchemaSubNode(NodeData^).SubNodeType = TSNTables) then
          PNGImg := TablePNGImg
        else
          if (TSchemaSubNode(NodeData^).SubNodeType = TSNIndices) then
            PNGImg := IndexPNGImg
          else
            if (TSchemaSubNode(NodeData^).SubNodeType = TSNUsers) then
              PNGImg := UserPNGImg;
      end
      else
        if (NodeData^ is TMYX_SCHEMA_TABLE) then
        begin
          if(TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_VIEW)then
            PNGImg := ViewPNGImg
          else
            PNGImg := TablePNGImg;
        end
        else
          if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
          begin
            if (TMYX_SCHEMA_TABLE_COLUMN(NodeData^).primary_key = 1) then
              PNGImg := ColumnPKPNGImg
            else
              PNGImg := ColumnPNGImg;
          end
          else
            if (NodeData^ is TMYX_TABLE_INDEX) then
              PNGImg := IndexPNGImg
            else
              if (NodeData^ is TMYX_TABLE_INDEX_COLUMN) then
                PNGImg := ColumnPNGImg
              else
                if (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) then
                  PNGImg := SPPNGImg;

    if PNGImg<>nil then
      PNGImg.Draw(TargetCanvas,
        Rect(x - 16 - 1, CellRect.Top + 1, x - 1, CellRect.Top + 17));
  end;
end;

procedure TSchemataFrame.CatalogVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: ^TObject;

begin
  NodeData := Sender.GetNodeData(Node);
  if (NodeData <> nil) then
    if (NodeData^ <> nil) then
    begin
      Font.Style := [];

      if (NodeData^ is TMYX_SCHEMA) then
      begin
        if FShowEscapedNames then
          CellText := TMYX_SCHEMA(NodeData^).escaped_schema_name
        else
          CellText := TMYX_SCHEMA(NodeData^).schema_name;
      end
      else
        if (NodeData^ is TSchemaSubNode) then
        begin
          if (TSchemaSubNode(NodeData^).SubNodeType = TSNTables) then
            CellText := _('Tables')
          else
            if (TSchemaSubNode(NodeData^).SubNodeType = TSNIndices) then
              CellText := _('Indices')
            else
              if (TSchemaSubNode(NodeData^).SubNodeType = TSNUsers) then
                CellText := _('Users');
        end
        else
          if (NodeData^ is TMYX_SCHEMA_TABLE) then
            CellText := TMYX_SCHEMA_TABLE(NodeData^).table_name
          else
            if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
              CellText := TMYX_SCHEMA_TABLE_COLUMN(NodeData^).column_name
            else
              if (NodeData^ is TMYX_TABLE_INDEX) then
                CellText := TMYX_TABLE_INDEX(NodeData^).key_name + ' (' +
                  TMYX_TABLE_INDEX(NodeData^).table_name + ')'
              else
                if (NodeData^ is TMYX_TABLE_INDEX_COLUMN) then
                  CellText := TMYX_TABLE_INDEX_COLUMN(NodeData^).column_name
                else
                  if (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) then
                  begin
                    if TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_PROCEDURE then
                      CellText := TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name
                    else
                      CellText := TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name+': '+
                        TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).return_datatype;
                  end
                  else
                    if (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE_PARAM) then
                      CellText := TMYX_SCHEMA_STORED_PROCEDURE_PARAM(NodeData^).name+
                        ' '+TMYX_SCHEMA_STORED_PROCEDURE_PARAM(NodeData^).datatype;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CatalogVSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := 0;
end;

procedure TSchemataFrame.CatalogVSTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
  TxtRect: TRect;
begin
  if (Sender.InheritsFrom(TBaseVirtualTree)) then
  begin
    TBaseVirtualTree(Sender).GetHitTestInfoAt(X, Y, True, HitInfo);

    if (HitInfo.HitNode <> nil) then
    begin
      TxtRect := TBaseVirtualTree(Sender).GetDisplayRect(
        HitInfo.HitNode, -1, True);

      if (X > TxtRect.Left - 16 - 16 - 1) and (X < TxtRect.Left - 16 - 1) and
        ((HitInfo.HitNode.ChildCount > 0) or (vsHasChildren in HitInfo.HitNode.States)) then
      begin
        TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode] := 
          not (TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]);
      end;
    end
  end;
end;

procedure TSchemataFrame.CatalogVSTExpanding(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);

var
  NodeData,
  ChildNodeData: ^TObject;
  
begin
  if (FShowAssetsOnSchemaExpansion) then
  begin
    NodeData := Sender.GetNodeData(Node);

    if (NodeData^ <> nil) then
      if (NodeData^ is TMYX_SCHEMA) then
      begin
        CurrentSchema := TMYX_SCHEMA(NodeData^);
        if (TMYX_SCHEMA(NodeData^).schema_tables = nil) then
          RefreshTableNames(TSchemaVirtualNode.Create(Node, TMYX_SCHEMA(NodeData^)));
      end
      else
        if tsLeftDblClick in Sender.TreeStates then
          Allowed := False;
  end
  else
  begin
    NodeData := Sender.GetNodeData(Node);

    //Expand Tables only if
    // Parent node is a TMYX_SCHEMA
    // The node itself holds a TSchemaSubNode of type TSNTables in Data
    // the first childnode's data is null
    if (NodeData^ <> nil) and (Sender.GetFirstChild(Node) <> nil) then
    begin
      ChildNodeData := Sender.GetNodeData(Sender.GetFirstChild(Node));

      if (TObject(NodeData^) is TSchemaSubNode) then
        if (TSchemaSubNode(NodeData^).SubNodeType = TSNTables) and
          (ChildNodeData^ = nil) then
        begin
          Sender.DeleteChildren(Node);
          RefreshTableNames(TSchemaVirtualNode.Create(
            Node, TSchemaSubNode(NodeData^).Schema));
        end;
    end;

    //Expand Indices only if
    // The node itself holds a TSchemaSubNode of type TSNIndices in Data
    // the first childnode's data is null
    if (NodeData^ <> nil) and (Sender.GetFirstChild(Node) <> nil) then
    begin
      ChildNodeData := Sender.GetNodeData(Sender.GetFirstChild(Node));

      if (TObject(NodeData^) is TSchemaSubNode) then
        if (TSchemaSubNode(NodeData^).SubNodeType = TSNIndices) and
          (ChildNodeData^ = nil) then
        begin
          Sender.DeleteChildren(Node);
          RefreshIndicesNames(TSchemaVirtualNode.Create(Node, TSchemaSubNode(NodeData^).Schema));
        end;
    end;
  end;
end;

procedure TSchemataFrame.CatalogVSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData, ParentNodeData: ^TObject;
  ParentNode: PVirtualNode;
begin
  //CurrentSchema := nil;
  CurrentTable := nil;

  NodeData := Sender.GetNodeData(Node);

  if (NodeData <> nil) then
    if (NodeData^ <> nil) then
      if (TObject(NodeData^) is TMYX_SCHEMA) then
      begin
        CurrentSchema := TMYX_SCHEMA(NodeData^);
      end
      else
        if (TObject(NodeData^) is TSchemaSubNode) then
        begin
          CurrentSchema := TSchemaSubNode(NodeData^).Schema;
        end
        else
          if (NodeData^ is TMYX_SCHEMA_TABLE) or
            (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE)then
          begin
            ParentNode := Sender.NodeParent[Node];
            if (ParentNode <> nil) then
            begin
              ParentNodeData := Sender.GetNodeData(ParentNode);

              if (ParentNodeData <> nil) then
                if (ParentNodeData^ <> nil) then
                begin
                  if (TObject(ParentNodeData^) is TSchemaSubNode) then
                    CurrentSchema := TSchemaSubNode(ParentNodeData^).Schema
                  else
                    if (TObject(ParentNodeData^) is TMYX_SCHEMA) then
                      CurrentSchema := TMYX_SCHEMA(ParentNodeData^);
                end;

              if (CurrentSchema <> nil) then
                CurrentTable := TMYX_SCHEMA_TABLE(NodeData^);
            end;
          end;
end;

procedure TSchemataFrame.ClearCatalogTree;
begin
  FSearchLock.Acquire;
  try
    CatalogVST.Clear;
    //Free mem for AddedSchemata
    AddedSchemataList.Clear;
  finally
    FSearchLock.Release;
  end;
end;

function TSchemataFrame.GetCatalogVSTFocusedObject(MatchClass: TClass = nil): TObject;
var
  NodeData: ^TObject;
begin
  Result := nil;

  if (CatalogVST.FocusedNode <> nil) then
  begin
    NodeData := CatalogVST.GetNodeData(CatalogVST.FocusedNode);
    if (NodeData <> nil) then
      if (NodeData^ <> nil) then
        if (NodeData^ is MatchClass) then
          Result := NodeData^;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CatalogVSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
  
begin
  if (FPaintDefaultSchemaBold) then
  begin
    if (TargetCanvas.Font.Style <> []) then
      TargetCanvas.Font.Style := [];

    if (FDefaultSchemaNode.Node = Node) then
      TargetCanvas.Font.Style := [fsBold];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SetDefaultSchema(DefaultSchema: WideString);

var
  Node: PVirtualNode;
  NodeData: ^TObject;

begin
  FDefaultSchema := DefaultSchema;

  Node := CatalogVST.GetFirst;
  while (Node <> nil) do
  begin
    NodeData := CatalogVST.GetNodeData(Node);
    if (NodeData <> nil) and
      (NodeData^ <> nil) and
      (NodeData^ is TMYX_SCHEMA) and
      (TMYX_SCHEMA(NodeData^).schema_name = FDefaultSchema) then
    begin
      FDefaultSchemaNode.Schema := TMYX_SCHEMA(NodeData^);
      FDefaultSchemaNode.Node := Node;

      CatalogVST.InvalidateNode(Node);

      break;
    end
    else
      Node := CatalogVST.GetNextSibling(Node);
  end;

  if (FDefaultSchemaNode.Schema<>nil) and
    (FDefaultSchemaNode.Schema.schema_name<>FDefaultSchema) then
  begin
    FDefaultSchemaNode.Schema := nil;
    FDefaultSchemaNode.Node := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SearchTerminated(Sender: TObject);

begin
  SearchThread := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.StopSearch;

// Stops any pending or in progress search.

begin
  KillTimer(Handle, SearchTimer);
  FSearchString := '';

  // The main search thread never dead-locks. The code to execute is in the QB form too, so this
  // can (and must) check if the thread must be stopped.
  if Assigned(SearchThread) then
  begin
    SearchThread.Terminate;
    SearchThread := nil;

    // Don't wait here. The thread method is desigend to return as soon as possible when the thread is terminated.
    // Waiting means provoking an invalid handle error because of the FreeOnTerminate value.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.GetSelectedTables(List: TWidestringList);

var
  SelectedNodes: TNodeArray;
  I: Integer;
  NodeData: ^TObject;
  ParentData: ^TObject;
  S: WideString;
  SchemaString: WideString;
  
begin
  List.Clear;
  SelectedNodes := CatalogVST.GetSortedSelection(True);
  for I := 0 to High(SelectedNodes) do
  begin
    NodeData := CatalogVST.GetNodeData(SelectedNodes[I]);

    if (NodeData^ is TMYX_SCHEMA_TABLE) then
    begin
      ParentData := CatalogVST.GetNodeData(SelectedNodes[I].Parent);
      if FDefaultSchema <> TMYX_SCHEMA(ParentData^).schema_name then
        SchemaString := QC + TMYX_SCHEMA(ParentData^).schema_name+ QC + '.'
      else
        SchemaString := '';

      S := SchemaString + QC + TMYX_SCHEMA_TABLE(NodeData^).table_name + QC;
      List.Add(S);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TSchemataFrame.ResetCatalogDisplay;

var
  Node: PVirtualNode;

begin
  CatalogVST.BeginUpdate;
  try
    // Use the NoInit methods for walking the tree. This way only already existing nodes are traversed.
    // No need to make nodes visible that aren't loaded yet.
    Node := CatalogVST.GetFirstNoInit;
    while Assigned(Node) do
    begin
      CatalogVST.IsVisible[Node] := True;
      Node := CatalogVST.GetNextNoInit(Node);
    end;

    CatalogVST.FullCollapse;
    CatalogVST.Expanded[FDefaultSchemaNode.Node] := True;
  finally
   CatalogVST.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CatalogVSTCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

var
  NodeData1, NodeData2: ^TObject;
  ParentNodeData: ^TObject;
  V1, V2: Integer;

begin
  NodeData1 := Sender.GetNodeData(Node1);
  NodeData2 := Sender.GetNodeData(Node2);
  ParentNodeData := Sender.GetNodeData(Node1.Parent);

  if (NodeData1^ is TMYX_SCHEMA) then
    V1 := 1
  else
    if (NodeData1^ is TMYX_SCHEMA_TABLE) then
    begin
      if (TMYX_SCHEMA_TABLE(NodeData1^).table_type=MSTT_BASE_TABLE) then
        V1 := 100
      else
        V1 := 1000;
    end
    else
      if (NodeData1^ is TMYX_SCHEMA_STORED_PROCEDURE) then
        V1 := 10000
      else
        V1 := 100000;


  if (NodeData2^ is TMYX_SCHEMA) then
    V2 := 1
  else
    if (NodeData2^ is TMYX_SCHEMA_TABLE) then
    begin
      if (TMYX_SCHEMA_TABLE(NodeData2^).table_type=MSTT_BASE_TABLE) then
        V2 := 100
      else
        V2 := 1000;
    end
    else
      if (NodeData2^ is TMYX_SCHEMA_STORED_PROCEDURE) then
        V2 := 10000
      else
        V2 := 100000;

  if (V1=v2) then
  begin
    if (NodeData1^ is TMYX_SCHEMA_TABLE_COLUMN) and
      (NodeData2^ is TMYX_SCHEMA_TABLE_COLUMN) and
      (ParentNodeData^ is TMYX_SCHEMA_TABLE) then
      Result := TMYX_SCHEMA_TABLE(ParentNodeData^).columns.IndexOf(NodeData1^)-
        TMYX_SCHEMA_TABLE(ParentNodeData^).columns.IndexOf(NodeData2^)
    else
      Result := WideCompareText(TVirtualStringTree(Sender).Text[Node1, Column],
        TVirtualStringTree(Sender).Text[Node2, Column], LOCALE_USER_DEFAULT);
  end
  else
    Result := V1 - V2;

end;

procedure TSchemataFrame.SchemaPrivCountCallback(Sender: TObject);

var
  ErrorCode: MYX_LIB_ERROR;

begin
  SchemaPrivCount := myx_get_resultset_as_int(TMySQLConn(Sender).MySQL, @ErrorCode);
end;

procedure TSchemataFrame.DropMIClick(Sender: TObject);

var
  NodeData: ^TObject;
  SelectedNodes: TNodeArray;
  I: Integer;
  Names: WideString;
  SchemaListChanged: Boolean;
  EscapedSchemaName: string;
  Question: WideString;
  OldCursor: TCursor;

begin
  SelectedNodes := nil;

  if CatalogVST.SelectedCount > 0 then
  begin
    FSearchLock.Acquire;
    OldCursor := Screen.Cursor;
    try
      SelectedNodes := CatalogVST.GetSortedSelection(True);
      Names := '';
      SchemaListChanged := False;

      for I := 0 to High(SelectedNodes) do
      begin
        NodeData := CatalogVST.GetNodeData(SelectedNodes[I]);

        if NodeData^ is TMYX_SCHEMA then
          Names := Names + TMYX_SCHEMA(NodeData^).schema_name
        else
          if NodeData^ is TMYX_SCHEMA_TABLE then
            Names := Names + TMYX_SCHEMA_TABLE(NodeData^).table_name
          else
            if NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE then
              Names := Names + TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name;

        Names := Names + #13#10;
      end;

      if Length(SelectedNodes) = 1 then
      begin
        // Remove trailing new line. We don't need it here.
        SetLength(Names, Length(Names) - 2);
        Question := _('Are you sure you want to delete the object') + ' "' + Names + '"?';
      end
      else
        Question := _('Are you sure you want to delete these objects') + #13#10#13#10 + Names + '?';

      if ShowModalDialog(_('Delete Objects'), Question + #13#10#13#10 +
        _('Attention: this action cannot be rolled back!'), myx_mtConfirmation,
        _('OK') + #13#10 + _('Cancel')) = 1 then
      begin
        Screen.Cursor := crAppStart;

        for I := 0 to CatalogVST.SelectedCount-1 do
        begin
          NodeData := CatalogVST.GetNodeData(SelectedNodes[I]);

          if NodeData^ is TMYX_SCHEMA then
          begin
            if (CompareText(TMYX_SCHEMA(NodeData^).schema_name,
              'mysql') = 0) then
              ShowModalDialog(_('Cannot Drop MySQL Schema'),
                _('You cannot drop the MySQL schema'),
                myx_mtError, 'OK')
            else
            begin
              SchemaPrivCount := 0;

              // Disable the error display as the current user might not have select privilege for schema mysql.
              if FMySQLConnection.ExecuteDirectQuery('SELECT COUNT(*) FROM mysql.db WHERE db=''' +
                TMYX_SCHEMA(NodeData^).schema_name + '''', self.SchemaPrivCountCallback, 0, False) then
              begin
                if(SchemaPrivCount > 0) then
                begin
                  if (ShowModalDialog(_('Delete Objects'),
                    WideFormat(_('Do you want do remove the associated schema privileges?') + #13#10#13#10 +
                    _('Please note that this action cannot be rolled back.'),
                    []),
                    myx_mtConfirmation, 'Yes'#13#10'No') = 1) then
                    begin
                      EscapedSchemaName := Tnt_WideStringReplace(TMYX_SCHEMA(NodeData^).schema_name,
                        '_', '\_', [rfReplaceAll]);
                      FMySQLConnection.ExecuteDirect('DELETE FROM mysql.db WHERE db=''' + EscapedSchemaName + '''');
                      FMySQLConnection.ExecuteDirect('FLUSH PRIVILEGES');
                    end;
                end;
              end;

              //If the user deleted the current default schema, reset it
              if WideSameStr(FMySQLConnection.DefaultSchema, TMYX_SCHEMA(NodeData^).schema_name) then
                FMySQLConnection.DefaultSchema := '';

              FMySQLConnection.ExecuteDirect('/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;');
              FMySQLConnection.ExecuteDirect('DROP DATABASE `' + TMYX_SCHEMA(NodeData^).schema_name + '`');
              FMySQLConnection.ExecuteDirect('/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;');

              SchemaListChanged := True;
            end;
          end
          else
          begin
            if NodeData^ is TMYX_SCHEMA_TABLE then
            begin
              if TMYX_SCHEMA_TABLE(NodeData^).table_type = MSTT_BASE_TABLE then
                FMySQLConnection.ExecuteDirect('DROP TABLE `' + CurrentSchema.schema_name + '`.' + '`' +
                  TMYX_SCHEMA_TABLE(NodeData^).table_name + '`')
              else
                FMySQLConnection.ExecuteDirect('DROP VIEW `' + CurrentSchema.schema_name + '`.' + '`' +
                  TMYX_SCHEMA_TABLE(NodeData^).table_name + '`');

            end
            else
            begin
              if NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE then
              begin
                if TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_PROCEDURE then
                  FMySQLConnection.ExecuteDirect('DROP PROCEDURE `' +
                    CurrentSchema.schema_name + '`.' + '`' + TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name + '`')
                else
                  FMySQLConnection.ExecuteDirect('DROP FUNCTION `' +
                    CurrentSchema.schema_name + '`.' + '`' + TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name + '`');
              end;
            end;
          end;
        end;

        if SchemaListChanged then
          FMySQLConnection.SchemaListChanged;

        ReloadSchemaTree;
      end;
    finally
      FSearchLock.Release;
      Screen.Cursor := OldCursor;
    end;
  end;
end;

procedure TSchemataFrame.EditMIClick(Sender: TObject);

var
  NodeData: ^TObject;

begin
  if (CatalogVST.FocusedNode<>nil) and (CurrentSchema <> nil) then
  begin
    NodeData := CatalogVST.GetNodeData(CatalogVST.FocusedNode);

    if (NodeData<>nil) and (NodeData^<>nil) then
    begin
      if (NodeData^ is TMYX_SCHEMA_TABLE) and
        (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_BASE_TABLE) then
      begin
        if (Assigned(FEditTable)) then
          FEditTable(self)
        else
        begin
          if (CurrentTable <> nil) then
          begin
            if (CurrentCatalog = nil) then
              EditTable('', CurrentSchema.schema_name, CurrentTable.table_name)
            else
              EditTable(CurrentCatalog.catalog_name, CurrentSchema.schema_name, CurrentTable.table_name);
          end;
        end;
      end
      else
        if (NodeData^ is TMYX_SCHEMA_TABLE) and
          (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_VIEW) then
        begin
          if (Assigned(FEditView)) then
            if (CurrentCatalog<>nil) then
              FEditView(self, CurrentCatalog.catalog_name, CurrentSchema.schema_name, NodeData^)
            else
              FEditView(self, '', CurrentSchema.schema_name, NodeData^);
        end
        else
          if (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) then
          begin
            if (Assigned(FEditStoredProcedure)) then
              if (CurrentCatalog<>nil) then
                FEditStoredProcedure(self, CurrentCatalog.catalog_name, CurrentSchema.schema_name, NodeData^)
              else
                FEditStoredProcedure(self, '', CurrentSchema.schema_name, NodeData^);
          end;
    end;
  end;
end;

procedure TSchemataFrame.CreateNewStoredProcedureMIClick(Sender: TObject);
begin
  if (Assigned(FEditStoredProcedure)) and (CurrentSchema<>nil) then
    if (CurrentCatalog<>nil) then
      FEditStoredProcedure(self, CurrentCatalog.catalog_name, CurrentSchema.schema_name, nil)
    else
      FEditStoredProcedure(self, '', CurrentSchema.schema_name, nil);
end;

procedure TSchemataFrame.CreateNewViewMIClick(Sender: TObject);
begin
  if (Assigned(FEditView)) and (CurrentSchema<>nil) then
    if (CurrentCatalog<>nil) then
      FEditView(self, CurrentCatalog.catalog_name, CurrentSchema.schema_name, nil)
    else
      FEditView(self, '', CurrentSchema.schema_name, nil);
end;

procedure TSchemataFrame.RenameMIClick(Sender: TObject);

var
  NodeData: ^TObject;
  NewName: WideString;

begin
  if (CatalogVST.FocusedNode<>nil) and (CurrentSchema <> nil) and
    (FMySQLConnection<>nil) then
  begin
    NodeData := CatalogVST.GetNodeData(CatalogVST.FocusedNode);

    if (NodeData^ is TMYX_SCHEMA) then
    begin
      //
    end
    else
      if ((NodeData^ is TMYX_SCHEMA_TABLE) and
        (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_BASE_TABLE)) then
      begin
        NewName := TMYX_SCHEMA_TABLE(NodeData^).table_name;

        if ShowModalEditDialog(_('Rename Table'),
          WideFormat(_('Please enter the new name for the table %s.'), [TMYX_SCHEMA_TABLE(NodeData^).table_name]),
          myx_mtEdit, _('Rename') + #13#10 + _('Cancel'), True, _('Name:'), NewName)=1 then
        begin
          FMySQLConnection.ExecuteDirect(WideFormat(_('RENAME TABLE `%s`.`%s` TO `%s`.`%s`'),
            [CurrentSchema.schema_name, TMYX_SCHEMA_TABLE(NodeData^).table_name,
            CurrentSchema.schema_name, NewName]));

          ReloadSchemaTree;
        end;
      end
      else
        if ((NodeData^ is TMYX_SCHEMA_TABLE) and
          (TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_VIEW)) then
        begin

        end
        else
          if ((NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) and
            (TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_PROCEDURE)) then
          begin

          end
          else
            if ((NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) and
              (TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_FUNCTION)) then
            begin

            end
            else
            begin

            end;
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.SearchMIClick(Sender: TObject);

var
  MenuItem: TTntMenuItem;

begin
  MenuItem := TTntMenuItem(Sender);

  if MenuItem <> CustomMI then
  begin
    // Select custom menu items according to main items if the sender is one of them.
    if (MenuItem = SearchAllMI) or (MenuItem = SearchSchemataMI) or (MenuItem = SearchAssetsMI) or
      (MenuItem = SearchColumnsMI) then
    begin
      SchemataSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchSchemataMI);
      TableSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchAssetsMI);
      ColumnsSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchColumnsMI);
      SPSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchAssetsMI);
      ViewsSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchAssetsMI);
      TriggersSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchAssetsMI);
      IndicesSubMI.Checked := (MenuItem = SearchAllMI) or (MenuItem = SearchColumnsMI);
    end
    else
      CustomMI.Checked := True;

    FSearchTargets := [];
    if SchemataSubMI.Checked then
      Include(FSearchTargets, smSchemata);
    if TableSubMI.Checked then
      Include(FSearchTargets, smTables);
    if ColumnsSubMI.Checked then
      Include(FSearchTargets, smColumns);
    if SPSubMI.Checked then
      Include(FSearchTargets, smStoredProcAndFunc);
    if ViewsSubMI.Checked then
      Include(FSearchTargets, smViews);
    if TriggersSubMI.Checked then
      Include(FSearchTargets, smTriggers);
    if IndicesSubMI.Checked then
      Include(FSearchTargets, smIndices);

    SaveOptions;

    PrepareSearch(AdvancedEdit.SearchEd.Text);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.GetAssetSQLCreateText: WideString;

var
  NodeData, SchemaNodeData: ^TObject;
  SelectedNodes: TNodeArray;
  I: Integer;
  SQL, Definition, ObjType: WideString;
  SchemaName: WideString;

begin
  SQL := '';
  SelectedNodes := nil;

  if CatalogVST.SelectedCount >0 then
  begin
    SelectedNodes := CatalogVST.GetSortedSelection(True);

    for I := 0 to CatalogVST.SelectedCount-1 do
    begin
      NodeData := CatalogVST.GetNodeData(SelectedNodes[I]);

      if (NodeData^ is TMYX_SCHEMA_TABLE) or (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) then
      begin
        SchemaNodeData := CatalogVST.GetNodeData(SelectedNodes[I].Parent);
        SchemaName := TMYX_SCHEMA(SchemaNodeData^).schema_name;
      end
      else
        if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) then
        begin
          NodeData := CatalogVST.GetNodeData(SelectedNodes[I].Parent);
          SchemaNodeData := CatalogVST.GetNodeData(SelectedNodes[I].Parent.Parent);
          SchemaName := TMYX_SCHEMA(SchemaNodeData^).schema_name;
        end;


      if (NodeData^ is TMYX_SCHEMA) then
        SQL := SQL + myx_dbm_get_create_sql(FMySQLConnection.MySQL, '', TMYX_SCHEMA(NodeData^).schema_name, '',
          MYX_DBM_OT_SCHEMA, 0, '`', 0) + ';' + #13#10 + #13#10
      else
        if (NodeData^ is TMYX_SCHEMA_TABLE) then
        begin
          if TMYX_SCHEMA_TABLE(NodeData^).table_type=MSTT_BASE_TABLE then
          begin
            ObjType := 'TABLE';
            Definition := myx_dbm_get_create_sql(FMySQLConnection.MySQL,
              '', SchemaName, TMYX_SCHEMA_TABLE(NodeData^).table_name,
              MYX_DBM_OT_TABLE, 1, '`', 0);
          end
          else
          begin
            ObjType := 'VIEW';
            Definition := myx_dbm_get_create_sql(FMySQLConnection.MySQL,
              '', SchemaName, TMYX_SCHEMA_TABLE(NodeData^).table_name,
              MYX_DBM_OT_VIEW, 1, '`', 0);
          end;

          SQL := SQL + WideFormat(
            'DROP %s IF EXISTS ' + QC + '%s' + QC + '.' + QC + '%s' + QC + ';'#13#10 + '%s;',
            [ObjType, SchemaName, TMYX_SCHEMA_TABLE(NodeData^).table_name,
            Definition]);
        end
        else
          if (NodeData^ is TMYX_SCHEMA_STORED_PROCEDURE) then
          begin
            if TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).sp_type=MSPT_PROCEDURE then
            begin
              ObjType := 'PROCEDURE';
              Definition := myx_dbm_get_create_sql(FMySQLConnection.MySQL, '', SchemaName,
                TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name, MYX_DBM_OT_PROCEDURE, 1, '`', 0);
            end
            else
            begin
              ObjType := 'FUNCTION';
              Definition := myx_dbm_get_create_sql(FMySQLConnection.MySQL, '', SchemaName,
                TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name, MYX_DBM_OT_FUNCTION, 1, '`', 0) + ';' + #13#10 + #13#10;
            end;

            SQL := SQL + WideFormat(
              'DELIMITER $$'#13#10#13#10 + 'DROP %s IF EXISTS ' + QC + '%s' + QC + '.' + QC + '%s' + QC + '$$'#13#10 +
              '%s $$'+#13#10#13#10 + 'DELIMITER ;', [ObjType, SchemaName, TMYX_SCHEMA_STORED_PROCEDURE(NodeData^).name,
                Definition]);
          end;

      if I < CatalogVST.SelectedCount - 1 then
        SQL := SQL + #13#10#13#10;
    end;
  end;

  Result := SQL;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.CopyAssetSQLMIClick(Sender: TObject);

begin
  TntClipboard.AsWideText := GetAssetSQLCreateText;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSchemataFrame.QC: WideString;

begin
  Result := FMySQLConnection.QuoteChar;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.WMTimer(var Msg: TWMTimer);

begin
  case Msg.TimerID of
    SearchTimer:
      begin
        KillTimer(Handle, SearchTimer);
        Application.CancelHint;
        SearchThread := TSearchThread.Create(DoSearch);
        SearchThread.OnTerminate := SearchTerminated;
        SearchThread.Resume;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.AdvancedEditSearchEdChange(Sender: TObject);

begin
  AdvancedEdit.SearchEdChange(Sender);
  PrepareSearch((Sender as TTntEdit).Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSchemataFrame.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

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

procedure TSchemataFrame.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

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

initialization
finalization
  if Assigned(SearchThread) then
    SearchThread.Free;
end.

