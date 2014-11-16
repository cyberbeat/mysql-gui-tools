unit QueryBrowser;

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
  Windows, Messages, ShellAPI, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, MySQLConnection, AuxFuncs, Contnrs, SyncObjs,
  VirtualTrees, MyxError, PNGImage, Toolbar, gnugettext,
  TabHeader, TntExtCtrls, TntComCtrls, AdvancedEdit,
  SchemataTreeView, TntForms, Menus,
  TntClasses, TntStdCtrls, MySQLResultSetControls, InstanceSections,
  ImgList, Options, ActiveX, OleCtrls, SHDocVw,
  EmbeddedWB, MSHTML, Sections, TableDrag, TntMenus,
  myx_public_interface, myx_qb_public_interface, TntControls,
  UniCodeEditor, UCEHighlighter, UCESQLHighlighter, UCEShared,
  ScriptPanel, TntDialogs, AuxApplicationFuncs,
  StrUtils, UnicodeConsole, CommonTypes, MySQLResultSet,
  TntClipbrd, Unicode, myx_util_public_interface;

const
  WM_CursorChanged      = WM_USER + 200;
  WM_DoVTEdit           = WM_USER + 201;
  WM_UPDATE_COMMAND     = WM_USER + 202;
  WM_DO_INIT            = WM_USER + 203;

  // Timer constants for delayed actions.
  SearchTimer = 1;

type
  TActivePerspective = (
    apUnknown,
    apResultSet,
    apScript,
    apHelp
  );

  // Which kind of info is about or currently being searched? (Schemata search is done in SchemataTreeview.pas).
  TSearchType = (
    stNone,        // Nothing is being searched.
    stBookmarks,   // Bookmark entries.
    stHistory      // History entries.
  );

  PPMYX_HISTORY_ENTRY = ^PMYX_HISTORY_ENTRY;

  TSQLEditor = class(TScriptEditor)
  protected
    procedure ChangeLine(Sender: TObject; Line: TUCELine); override;
    procedure DeleteLine(Sender: TObject; Line: TUCELine); override;
  end;

  // Indicator for the central handler routine what must be performed with the editor.
  TEditorCommand =
  (
    edExecute,
    edBack,
    edNext,
    edExplain,
    edCompare,
    edRefresh
  );

  TQueryBrowserForm = class(TInstanceSectionForm, IAdaptable, IProgressMonitor, IMySQLConnTransactionStatusChangeListener)
    DockPnl: TTntPanel;
    ToolbarPnl: TTntPanel;
    QueryToolbarPnl: TTntPanel;
    Bevel1: TTntBevel;
    AdvancedQueryToolbarPnl: TTntPanel;
    AdvancedQueryToolbarSepBevel: TTntBevel;
    AnimPnl: TTntPanel;
    AnimStillImg: TTntImage;
    Shape1: TTntShape;
    TabsPnl: TTntPanel;
    MainTabHeaderFrame: TTabHeaderFrame;
    SidebarPnl: TTntPanel;
    UpperTabHeaderFrame: TTabHeaderFrame;
    LowerTabHeaderFrame: TTabHeaderFrame;
    SidebarSepShape: TTntShape;
    UpperPageControl: TTntPageControl;
    TntTabSheet1: TTntTabSheet;
    TntTabSheet2: TTntTabSheet;
    TntTabSheet3: TTntTabSheet;
    SchemataPnl: TTntPanel;
    SchemataFrame: TSchemataFrame;
    QueryExecutePopupMenu: TTntPopupMenu;
    ExecuteFromToolbarMI: TTntMenuItem;
    ExecuteInNewTabFromToolbarMI: TTntMenuItem;
    SplitTabAndExecuteFromToolbarMI: TTntMenuItem;
    LowerPageControl: TTntPageControl;
    ParamSheet: TTntTabSheet;
    FuncSheet: TTntTabSheet;
    SynSheet: TTntTabSheet;
    ParamPnl: TTntPanel;
    ParamVT: TVirtualStringTree;
    QueryPopupMenu: TTntPopupMenu;
    SideBarImageList: TImageList;
    BookmarksPnl: TTntPanel;
    BookmarksAdvancedEdit: TAdvancedEditFrame;
    BookmarkSpacerShape: TTntShape;
    BookmarkVT: TVirtualStringTree;
    BookmarkPopupMenu: TTntPopupMenu;
    CreateBookmarkFolderMI: TTntMenuItem;
    N1: TTntMenuItem;
    DeleteBookmarkMI: TTntMenuItem;
    HistoryPanel: TTntPanel;
    HistorySpacerShape: TTntShape;
    HistoryAdvancedEdit: TAdvancedEditFrame;
    HistoryVT: TVirtualStringTree;
    FunctionPanel: TTntPanel;
    BusyAnimate: TAnimate;
    RSGridPopupMenu: TTntPopupMenu;
    AddNewRSTabMI: TTntMenuItem;
    SplitResultsetTabVerticallyMI: TTntMenuItem;
    N2: TTntMenuItem;
    SplitTabHorizontallyMI: TTntMenuItem;
    N3: TTntMenuItem;
    RemoveResultSetMI: TTntMenuItem;
    RemoveTabsheetMI: TTntMenuItem;
    N4: TTntMenuItem;
    DeleteRowMI: TTntMenuItem;
    ClearQueryEditorMI: TTntMenuItem;
    N5: TTntMenuItem;
    OpenQueryMI: TTntMenuItem;
    SaveQueryAsMI: TTntMenuItem;
    SQLEditCopyMI: TTntMenuItem;
    SQLEditCutMI: TTntMenuItem;
    SQLEditPasteMI: TTntMenuItem;
    MainMenu: TTntMainMenu;
    QueryMI: TTntMenuItem;
    AddBookmarkMI: TTntMenuItem;
    N6: TTntMenuItem;
    N7: TTntMenuItem;
    ExportResultsetMI: TTntMenuItem;
    UCESQLHighlighter: TUCESQLHighlighter;
    ScriptMemoPopupMenu: TTntPopupMenu;
    AddNewScriptTabMI: TTntMenuItem;
    SplitScriptTabHorizontallyMI: TTntMenuItem;
    TntMenuItem2: TTntMenuItem;
    OpenScriptMI: TTntMenuItem;
    DebugToolbarPnl: TTntPanel;
    DebugToolbarSepBevel: TTntBevel;
    SaveScriptMI: TTntMenuItem;
    SaveScriptAsMI: TTntMenuItem;
    N8: TTntMenuItem;
    QueryExecuteMI: TTntMenuItem;
    QueryRefreshMI: TTntMenuItem;
    QueryStopMI: TTntMenuItem;
    ScriptMI: TTntMenuItem;
    ScriptExecuteMI: TTntMenuItem;
    ScriptRunSelectionMI: TTntMenuItem;
    ScriptStepOverMI: TTntMenuItem;
    ScriptStopMI: TTntMenuItem;
    N9: TTntMenuItem;
    N10: TTntMenuItem;
    ToggleBreakpointMI: TTntMenuItem;
    N11: TTntMenuItem;
    ExplainMI: TTntMenuItem;
    CompareResultsetsMI: TTntMenuItem;
    QueryExecuteInNewTabMI: TTntMenuItem;
    SplitTabandExecuteMI: TTntMenuItem;
    ViewMI: TTntMenuItem;
    ShowSidebarMI: TTntMenuItem;
    N12: TTntMenuItem;
    OnlyTabsheetsMI: TTntMenuItem;
    ToolbarSepPnl: TTntPanel;
    ToolbarSepShape: TTntShape;
    N14: TTntMenuItem;
    MainAreaPnl: TTntPanel;
    N15: TTntMenuItem;
    PasteClipboardContentasPHPcodeMI: TTntMenuItem;
    PasteClipboardContentasJavaCodeMI: TTntMenuItem;
    N16: TTntMenuItem;
    CopySQLasPHPcodeMI: TTntMenuItem;
    CopySQLasJavaCodeMI: TTntMenuItem;
    RSAddNewScriptTabMI: TTntMenuItem;
    ScriptAddNewResultsetTabMI: TTntMenuItem;
    N19: TTntMenuItem;
    HistoryPopupMenu: TTntPopupMenu;
    DeleteSelectedHistoryEntriesMI: TTntMenuItem;
    N20: TTntMenuItem;
    ClearHistoryMI: TTntMenuItem;
    AddHistoryItemasBookmarkMI: TTntMenuItem;
    N21: TTntMenuItem;
    FunctionsVT: TVirtualStringTree;
    SyntaxPnl: TTntPanel;
    SyntaxVT: TVirtualStringTree;
    CopyRowValuesMI: TTntMenuItem;
    MaximizeQueryEditMI: TTntMenuItem;
    SQLEditMaximizedPnl: TTntPanel;
    SidebarSplitter: TTntSplitter;
    SQLEditMaximizedSplitterPBox: TTntPaintBox;
    N13: TTntMenuItem;
    ViewFieldinPopupEditorMI: TTntMenuItem;
    EditFieldinPopupEditorMI: TTntMenuItem;
    ClearFieldContentMI: TTntMenuItem;
    LoadFieldContentMI: TTntMenuItem;
    SaveFieldContentMI: TTntMenuItem;
    N17: TTntMenuItem;
    AddRowMI: TTntMenuItem;
    RSGridPopupImageList: TImageList;
    CopyFieldContentMI: TTntMenuItem;
    N18: TTntMenuItem;
    FindMI: TTntMenuItem;
    QueryAreaHeaderPBox: TTntPaintBox;
    EditAllStoredProceduresFunctionsMI: TTntMenuItem;
    CreateStoredProcedureFunctionMI: TTntMenuItem;
    ScriptContinueMI: TTntMenuItem;
    N22: TTntMenuItem;
    ClearAllBreakpointsMI: TTntMenuItem;
    TrxTabSheet: TTntTabSheet;
    TransactionDisplay: TVirtualStringTree;
    TrxPnl: TTntPanel;
    ParamPopupMenu: TTntPopupMenu;
    AddParameterMI: TTntMenuItem;
    N23: TTntMenuItem;
    RefreshParametersMI: TTntMenuItem;
    DeleteParameterMI: TTntMenuItem;
    MoveParametertoGlobalParametersMI: TTntMenuItem;
    BookmarkImageList: TImageList;
    TrxPopupMenu: TTntPopupMenu;
    TrxCopySQLMI: TTntMenuItem;
    ScriptToolbarPnl: TTntPanel;
    TntBevel1: TTntBevel;
    TntShape1: TTntShape;
    TntPanel2: TTntPanel;
    ScriptAnimStillImg: TTntImage;
    ScriptBusyAnimate: TAnimate;
    CreateViewfromSelectMI: TTntMenuItem;
    N24: TTntMenuItem;
    SelectAll1: TTntMenuItem;
    N25: TTntMenuItem;
    Undo1: TTntMenuItem;
    Redo1: TTntMenuItem;
    N26: TTntMenuItem;
    SaveBookmarksMI: TTntMenuItem;
    LoadBookmarksMI: TTntMenuItem;
    procedure MainTabHeaderFrameDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MainTabHeaderFrameDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure HistoryAdvancedEditSearchEdChange(Sender: TObject);
    procedure BookmarksAdvancedEditSearchEdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure BuildToolbars;

    procedure QueryGoBackClick(Sender: TObject);
    procedure QueryGoBackLblClick(Sender: TObject);
    procedure QueryNextClick(Sender: TObject);
    procedure QueryRefreshClick(Sender: TObject);
    procedure QueryExecuteClick(Sender: TObject);
    procedure QueryExecuteInNewTabClick(Sender: TObject);
    procedure QuerySplitAndExecuteClick(Sender: TObject);
    procedure QueryExecuteLblClick(Sender: TObject);
    procedure QueryStopClick(Sender: TObject);
    procedure QueryLockTablesClick(Sender: TObject);

    function CreateNextRSForMultipleRSQuery(Sender: TObject): TMySQLRS;
    procedure RemoveRSForMultipleRSQuery(Sender: TObject);
    procedure ShowRSForMultipleRSQuery(Sender: TObject);

    function AddNewRSTabSheet(Caption: WideString = ''): TRSTabSheet;
    function AddNewScriptTabSheet(Caption: WideString = ''): TScriptTabSheet;

    //Events
    procedure DoBeforeMainTabChange(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
      NewSelectedTab: Integer; obj: TObject);
    procedure DoMainTabChange(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
      NewSelectedTab: Integer; obj: TObject);
    procedure DoRequestNewPage(Sender: TObject);
    procedure ActivePanelChanging(Sender: TObject);
    procedure DoSetActiveRSTab(TabIndex: Integer);
    procedure ActivePanelChanged(PreviousSelectedPanel: TObject; SelectedPanel: TObject);
    procedure DoQueryExecute(Sender: TObject);
    procedure DoQueryExecuted(Sender: TObject);
    procedure DoQuerySuccess(Sender: TObject);
    procedure DoQueryStopped(Sender: TObject);
    procedure DoQueryError(Sender: TObject);
    procedure DoRefreshParams(Sender: TObject);

    procedure DoActiveScriptPanelChanged(Sender: TObject);
    procedure DoScriptTypeChange(Sender: TObject);
    procedure ScriptExecuteClick(Sender: TObject);
    procedure ScriptContinueClick(Sender: TObject);
    procedure ScriptStepIntoClick(Sender: TObject);
    procedure ScriptStepOverClick(Sender: TObject);
    procedure ScriptRunUntilReturnClick(Sender: TObject);
    procedure ScriptStopClick(Sender: TObject);
    procedure ScriptPauseClick(Sender: TObject);
    procedure ScriptExecute(Options: TExecutionOptions);
    procedure CheckStopBtnStatus;

    procedure ScriptLoadClick(Sender: TObject);
    procedure ScriptSaveClick(Sender: TObject);
    procedure ScriptSearchClick(Sender: TObject);

    procedure TransStartClick(Sender: TObject);
    procedure TransCommitClick(Sender: TObject);
    procedure TransRollbackClick(Sender: TObject);

    procedure DoCompare(Sender: TObject);
    procedure DoExplain(Sender: TObject);
    procedure QueryExecute;

    procedure MouseCursorClick(Sender: TObject);

    procedure DockPnlIgnoreWMEraseBkGnd(var Msg: TMessage);
    procedure AnimPnlIgnoreWMEraseBkGnd(var Msg: TMessage);
    procedure DoQueryEditorChange;
    procedure RefreshQueryNavButtons;

    procedure DoUpperTabChange(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
      NewSelectedTab: Integer; obj: TObject);

    procedure BuildParamTV;
    procedure ParamVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ParamVTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AddNewRSTabMIClick(Sender: TObject);
    procedure SplitScriptTabHorizontallyMIClick(Sender: TObject);
    procedure ParamVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure SchemataFrameSchemaTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);

    procedure DeleteRowMIClick(Sender: TObject);
    procedure ClearQueryEditorMIClick(Sender: TObject);

    procedure DoOnFreeVTNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

    // Bookmark handling
    procedure LoadBookmarks;
    procedure StoreBookmarks;
    procedure AddBookmarkgroupToVT(parentnode: PVirtualNode; groups: TMYX_BOOKMARK_GROUP_List);
    procedure AddBookmarkGroup(ParentMenuItem: TTntMenuItem; groups: TMYX_BOOKMARK_GROUP_List;
      DoNotCreateGroup: Boolean = False);

    procedure RefreshBookmarkMenu;
    procedure BookmarkPopupMenuPopup(Sender: TObject);
    procedure DeleteBookmarkNodeMIClick(Sender: TObject);
    procedure DeleteBookmarks(AskBeforeDelete: Boolean = True);
    procedure DeleteBookmark(Node: PVirtualNode;
      AskBeforeDelete: Boolean = True);
    procedure DeleteBookmarkFolder(Node: PVirtualNode;
      AskBeforeDelete: Boolean = True);

    procedure AddBookmark(Cmd: WideString);
    procedure BookmarkMenuItemSelected(Sender: TObject);

    procedure BookmarkVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure BookmarkVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure BookmarkVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var CellText: WideString);
    procedure BookmarkVTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BookmarkVTDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
      Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure BookmarkVTDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure BookmarkVTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CreateBookmarkFolderMIClick(Sender: TObject);
    procedure BookmarkVTDblClick(Sender: TObject);

    // History handling
    procedure LoadHistory;
    procedure StoreHistory;
    procedure RefreshHistoryTree;
    procedure HistoryVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure HistoryVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);

    //SQL Functions handling
    function LoadFunctions(filename: WideString): TMYX_SQL_FUNCTIONINDEX;
    procedure BuildFunctionTree(Tree: TVirtualStringTree; Functions: TMYX_SQL_FUNCTIONINDEX);
    procedure FunctionsVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure FunctionsVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FunctionsVTDblClick(Sender: TObject);
    procedure ShowHelpTabSheet(url: WideString);
    procedure DoHelpDocCompleted(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure BusyAnimateStop(Sender: TObject);
    procedure SplitTabHorizontallyMIClick(Sender: TObject);
    procedure RemoveResultSetMIClick(Sender: TObject);
    procedure RSGridPopupMenuPopup(Sender: TObject);
    procedure RemoveTabsheetMIClick(Sender: TObject);
    procedure HistoryVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var CellText: WideString);
    procedure AnimStillImgClick(Sender: TObject);
    procedure QueryPopupMenuPopup(Sender: TObject);
    procedure AddBookmarkMIClick(Sender: TObject);
    procedure BookmarkVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure SchemataFrameCatalogVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SchemataFrameCatalogVSTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SchemataFrameSchemaTreeViewPopupMenuPopup(Sender: TObject);

    procedure AddCatalogTreePopupMenuItems;
    procedure DoMakeDefaultSchema(Sender: TObject);
    procedure HistoryVTDblClick(Sender: TObject);

    procedure DoOptionsChanged;
    procedure OptionsChanged(var Message: TMessage); message WM_OptionsChanged;

    procedure DoMainTabPageDelete(Sender: TObject);
    procedure DoMainTabBeforePageDelete(Sender: TObject; var CanClose: Boolean);

    function DoConfirm(Sender: TObject; Msg: WideString): Boolean;
    procedure DoDropTables(Sender: TObject; List: TWideStringList; Shift: TShiftState);
    procedure SchemataFrameCatalogVSTMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SchemataFrameCatalogVSTDblClick(Sender: TObject);
    procedure QueryEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure QueryEditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure QueryEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure QueryEditorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure QueryEditorDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure QueryEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SchemataFrameCatalogVSTDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
      State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

    procedure DefaultSchemaChanged(var Message: TMessage); message WM_DefaultSchemaChanged;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;
    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure MySQLConnUpdateState;

    procedure OpenScriptMIClick(Sender: TObject);

    procedure EditCaretChange(Sender: TCustomUnicodeEdit; X, Y: Integer);
    procedure SplitResultsetTabVerticallyMIClick(Sender: TObject);
    procedure SaveScriptMIClick(Sender: TObject);
    procedure AddNewScriptTabMIClick(Sender: TObject);
    procedure ScriptMemoPopupMenuPopup(Sender: TObject);
    procedure SaveScriptAsMIClick(Sender: TObject);
    procedure OpenQueryMIClick(Sender: TObject);
    procedure SaveQueryAsMIClick(Sender: TObject);
    procedure ShowSidebarMIClick(Sender: TObject);

    procedure AddResultsetExporterMenuItems(ParentMenuItem: TMenuItem);
    procedure ResultsetExportMIClick(Sender: TObject);
    procedure SaveResultsetAs(ExportFormat: WideString;
      Filename: WideString = '');
    procedure OnlyTabsheetsMIClick(Sender: TObject);
    procedure QueryEditorEnter(Sender: TObject);
    function GetDragResult: Integer;
    procedure PasteClipboardContentasPHPcodeMIClick(Sender: TObject);
    procedure PasteClipboardContentasJavaCodeMIClick(Sender: TObject);
    function CaptureSQLFromClipboard(CopySQLLang: MYX_Q_SQL_STRIPPED_CODE_LANGUAGE;
      CopySQLMode: MYX_Q_SQL_STRIPPED_COPY_MODE): PMYX_Q_SQL_STRIPPED;
    procedure CopySQLasPHPcodeMIClick(Sender: TObject);
    procedure CopySQLasJavaCodeMIClick(Sender: TObject);
    procedure DeleteSelectedHistoryEntriesMIClick(Sender: TObject);
    procedure ClearHistoryMIClick(Sender: TObject);
    procedure HistoryPopupMenuPopup(Sender: TObject);
    procedure AddHistoryItemasBookmarkMIClick(Sender: TObject);
    procedure ShowQuickStartGuide;
    procedure SchemataFrameCatalogVSTEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure AdvancedQueryPnlBGShapeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure CopyRowValuesMIClick(Sender: TObject);
    procedure MaximizeQueryEditMIClick(Sender: TObject);
    procedure SchemataFrameCatalogVSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure ShowResultsetHits;
    procedure HideResultsetHits;
    procedure SQLEditMaximizedSplitterPBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SQLEditMaximizedSplitterPBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SQLEditMaximizedSplitterPBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SQLEditMaximizedSplitterPBoxPaint(Sender: TObject);
    procedure LoadFieldContentMIClick(Sender: TObject);
    procedure SaveFieldContentMIClick(Sender: TObject);
    procedure ClearFieldContentMIClick(Sender: TObject);
    procedure ViewFieldinPopupEditorMIClick(Sender: TObject);
    procedure EditFieldinPopupEditorMIClick(Sender: TObject);
    procedure AddRowMIClick(Sender: TObject);
    procedure CopyFieldContentMIClick(Sender: TObject);

    procedure DoEditStoredProcedure(Sender: TObject; CatalogName: WideString; SchemaName: WideString; SchemaObj: TObject);
    procedure DoEditView(Sender: TObject; CatalogName: WideString; SchemaName: WideString; SchemaObj: TObject);
    procedure FindMIClick(Sender: TObject);
    procedure RSGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure QueryAreaHeaderPBoxPaint(Sender: TObject);

    procedure SQLCreateViewClick(Sender: TObject);
    procedure SQLEditSPClick(Sender: TObject);
    procedure EditAllStoredProceduresFunctionsMIClick(Sender: TObject);
    procedure CreateStoredProcedureFunctionMIClick(Sender: TObject);
    procedure ToggleBreakpointMIClick(Sender: TObject);
    procedure ClearAllBreakpointsMIClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure TransactionQueryExecuted(SQLCmd: WideString);
    procedure TransactionDisplayGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TransactionStarted;
    procedure AddParameterMIClick(Sender: TObject);
    procedure ParamPopupMenuPopup(Sender: TObject);
    procedure RefreshParametersMIClick(Sender: TObject);
    procedure DeleteParameterMIClick(Sender: TObject);
    procedure ParamVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ParamVTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure MoveParametertoGlobalParametersMIClick(Sender: TObject);
    procedure TrxCopySQLMIClick(Sender: TObject);
    procedure RefreshPerspective;

    procedure ScriptUndoClick(Sender: TObject);
    procedure ScriptRedoClick(Sender: TObject);

    procedure ScriptDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ScriptDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ParamVTDblClick(Sender: TObject);

    procedure DoVTEdit(var Message: TMessage); message WM_DoVTEdit;

    procedure ScriptRunSelectionMIClick(Sender: TObject);
    procedure SaveBookmarksMIClick(Sender: TObject);
    procedure LoadBookmarksMIClick(Sender: TObject);
  private
    FSQLEditor: TSQLEditor;
    FIsLoading: Boolean;

    FCurrentCommandStart: TPoint;      // The line/column position in FSQLEditor of the beginning of the current command.
    FCurrentCommandEnd: TPoint;        // The end point of that command.
    FCurrentCommand: WideString;       // The text of the current command.
    FCurrentCommandIsSingle: Boolean;  // True if only one command is on the given line.
    FCurrentCommandStyle: IUCELineStyle;

    Toolbars: TToolbars;

    FNextResultSetID: Integer;
    PNGImages: TObjectList;

    DockPnlWindowProc: TWndMethod;
    AnimPnlWindowProc: TWndMethod;

    BusyAnimationStart: Boolean;

    GlobalParamsNode,
    LocalParamsNode,
    DynamicParamsNode: PVirtualNode;

    HelpPnl: TSectionPanel;
    HelpWebBrowser: TEmbeddedWB;

    QueryEditorDragStartPoint: TPoint;
    QueryEditorRightMouseDown: Boolean;

    IdCopySQLHotKey, IdPasteSQLHotKey: Integer;
    hOtherWin, hFocusWin: HWND;
    OtherThreadID: Integer;

    ChangeDefaultSchemaMI: TTntMenuItem;

    FCurrentPerspective: TActivePerspective;
    FActiveRSTabSheet: TRSTabSheet;
    FActiveScriptTabSheet: TScriptTabSheet;

    TableDragForm: TTableDragForm;

    FSQLEditorMaximized: Boolean;

    FSQLEditMaximizedPnlHeight: Integer;
    FSQLEditMaximizedPnlMouseY: Integer;
    FSQLEditMaximizedPnlMouseDown: Boolean;

    FRSCount, FScriptCount: Integer;

    FQueryRunningCount: Integer;

    FMaximizedQueryAreaHeaderBGBmp: TBitmap;

    FSetFocusToQueryEditorAfterMouseUp: Boolean;

    FTrxQueryList: TObjectList;

    FGlobalParams: TTntStringList;
    FHistory: PMYX_HISTORY;
    FHistoryTree: PMYX_HISTORY_TREE;

    // Progress display
    FProgressBar: TProgressBar;
    FProgressMax: Integer;

    FSearchType: TSearchType;
    FSearchString: WideString;
    FSearchLock: TCriticalSection;               // Serialize access to UI elements during search.
    FQueryMenuInitialCount: Integer;

    function GetActiveResultset: TMySQLRS;
    function GetActiveResultsetPanel: TMySQLRSPanel;
    function GetActiveScriptPanel: TScriptPanel;
    procedure SetCurrentCommand(const SQL: WideString);
    procedure SetCurrentPerspective(CurrentPerspective: TActivePerspective);
    procedure SetSQLEditorMaximized(SQLEditorMaximized: Boolean);

    procedure WMCursorChanged(var Msg: TMessage); message WM_CursorChanged;
    procedure WMDoInit(var Msg: TMessage); message WM_DO_INIT;
    procedure WMHotKey(var Msg: TWMHotKey); message WM_HOTKEY;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMUpdateCommand(var Msg: TMessage); message WM_UPDATE_COMMAND;
  protected
    procedure AdvanceToNextCommand;
    procedure EditScript(Caption: WideString; Text: WideString; CaretXY: TPoint); overload;
    procedure EditScript(Caption: WideString; Text: WideString; SelStart: Integer); overload;
    procedure EditScript(Caption: WideString; Text: WideString); overload;
    function GetAdapter(IID: TGUID): IInterface;
    procedure HandleEditorCommand(Command: TEditorCommand);
    procedure LineDeleted(Line: TUCELine);
    procedure PrepareQueryAreaHeaderBitmap;
    procedure PrepareSearch(SearchType: TSearchType; Text: WideString);
    procedure ProgressFinish;
    procedure ProgressInit(const Max: Integer);
    procedure ProgressPosition(const Position: Integer);
    procedure ProgressStep(const Amount: Integer = 1);
    procedure SearchBookmarks(Worker: TThread);
    procedure SearchHistory(Worker: TThread);
    procedure ShowError(const Title, Text: WideString; Parameters: array of const);
    procedure StopSearch;
    procedure TransactionStatusChanged;
    procedure UpdateCommandDisplay(X, Y: Integer; Force: Boolean);
  public
    HelpTabSheetIndex: Integer;

    ScriptTypeCBox: TTntComboBox;

    Bookmarks: TMYX_BOOKMARKS;
    FunctionsOverviewTree: TMYX_SQL_FUNCTIONINDEX;
    SyntaxOverviewTree: TMYX_SQL_FUNCTIONINDEX;

    PSQLStripped: PMYX_Q_SQL_STRIPPED;

    LastFocusedControl: TWinControl;
    procedure OpenScript(Filename: WideString = ''; OpenInNewTabSheet: Boolean = False);
    procedure OpenQuery(Filename: WideString = ''; OpenInNewTabSheet: Boolean = False);

    procedure Save;
    procedure SaveAs;
    procedure SaveQueryAs(Filename: WideString = '');
    procedure SaveScript;
    function SaveScriptAs(ScriptPanel: TScriptPanel): Boolean;
    procedure UpdateActions; override;

    property ActiveResultset: TMySQLRS read GetActiveResultset;
    property ActiveResultsetPanel: TMySQLRSPanel read GetActiveResultsetPanel;
    property ActiveScriptPanel: TScriptPanel read GetActiveScriptPanel;
    property CurrentCommand: WideString read FCurrentCommand write SetCurrentCommand;
    property CurrentCommandStart: TPoint read FCurrentCommandStart;
    property CurrentPerspective: TActivePerspective read FCurrentPerspective write SetCurrentPerspective;
    property SQLEditor: TSQLEditor read FSQLEditor;
    property SQLEditorMaximized: Boolean read FSQLEditorMaximized write SetSQLEditorMaximized;
  end;

  TDataMenuItem = class(TTntMenuItem)
    constructor Create(AOwner: TComponent; Data: Pointer); reintroduce;
  private
    FData: Pointer;
  public
    property Data: Pointer read FData;
  end;

  TTrxQueryNode = class
    constructor Create(Query: WideString; Success: Boolean);
  private
    FQuery: WideString;
    FSuccess: Boolean;
  public
    property Query: WideString read FQuery write FQuery;
    property Success: Boolean read FSuccess write FSuccess;
  end;


  PParamRowData = ^ParamRowData;
  ParamRowData = record
    param_index: Integer;
    params: TTntStringList;
  end;

  PBookmarkNodeData = ^BookmarkNodeData;
  BookmarkNodeData = record
    group: TMYX_BOOKMARK_GROUP;
    bookmark: TMYX_BOOKMARK;
  end;

  THistoryNodeType = (HISTORY_INTERVAL_TYPE, HISTORY_CATALOG_TYPE,
    HISTORY_SCHEMA_TYPE, HISTORY_ENTRY_TYPE);

  PHistoryNodeData = ^HistoryNodeData;
  HistoryNodeData = record
    NodeCaption: WideString;
    NodeType: THistoryNodeType;
    Data: Pointer;
  end;

  PFunctionNodeData = ^FunctionNodeData;
  FunctionNodeData = record
    group: TMYX_SQL_FUNCTIONGROUP;
    func: TMYX_SQL_FUNCTION;
  end;

const
  crSQLSelect = 1;
  crSQLFrom = 2;
  crSQLWhere = 3;
  crSQLGroup = 4;
  crSQLHaving = 5;
  crSQLOrder = 6;
  crSQLSet = 7;

var
  QueryBrowserForm: TQueryBrowserForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  ApplicationDataModule, Main, Math, PNGTools,
  TntSysUtils, UCEEditorKeyCommands;

const
  // Some values that likely never going to change.
  TabBorderColor = $009C9B91;
  SearchTimeout  = 500;           // Number of milliseconds after which search starts when there was a keypress.

type
  TCurrentCommandStyle = class(TInterfacedObject, IUCELineStyle)
  protected
    function GetBackground: TColor;
    function GetFontStyles: TFontStyles;
    function GetForceFontStyles: Boolean;
    function GetForeground: TColor;
  end;
  
  TInactiveCommandStyle = class(TInterfacedObject, IUCELineStyle)
  protected
    function GetBackground: TColor;
    function GetFontStyles: TFontStyles;
    function GetForceFontStyles: Boolean;
    function GetForeground: TColor;
  end;

  TSearchMethod = procedure(Worker: TThread) of object;
  
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
  SearchThread: TSearchThread;

//----------------- TSearchThread --------------------------------------------------------------------------------------

constructor TSearchThread.Create(Method: TSearchMethod);

begin
  FMethod := Method;

  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSearchThread.Execute;

begin
  if not Terminated then
    FMethod(Self);
end;

//----------------- TQueryBrowserForm ----------------------------------------------------------------------------------

procedure TQueryBrowserForm.FormCreate(Sender: TObject);

var
  Stroke: TKeyStroke;

begin
  // Since this form is created dynamically we have to set the reference variable manually.
  QueryBrowserForm := Self;
  
  InitForm(Self);

  MySQLConn.AddTransactionStatusListener(self);

  FRSCount := 0;
  FScriptCount := 0;

  FNextResultSetID := 1;
  DockedPanel := DockPnl;

  ToolbarPnl.Height := 93 - 4;

  FActiveRSTabSheet := nil;
  FActiveScriptTabSheet := nil;

  PNGImages := TObjectList.Create;
  FTrxQueryList := TObjectList.Create;
  FSearchLock := TCriticalSection.Create;

  Toolbars := TToolbars.Create;

  FQueryMenuInitialCount := QueryMI.Count;


  //Create Toolbar Container
  Toolbars.AddToolbar(QueryToolbarPnl,
    'Query', ApplicationDM.QBOptions.UseToolbarGradient);
  Toolbars.AddToolbar(ScriptToolbarPnl,
    'Script', ApplicationDM.QBOptions.UseToolbarGradient);
  Toolbars.AddToolbar(AdvancedQueryToolbarPnl,
    'AdvancedQuery', ApplicationDM.QBOptions.UseToolbarGradient).OnDragOver :=
      AdvancedQueryPnlBGShapeDragOver;
  Toolbars.AddToolbar(DebugToolbarPnl,
    'Debug', ApplicationDM.QBOptions.UseToolbarGradient);

  Bookmarks := nil;
  FHistory := nil;
  FHistoryTree := nil;
  FunctionsOverviewTree := nil;
  SyntaxOverviewTree := nil;

  PSQLStripped := nil;

  LastFocusedControl := nil;

  TableDragForm := nil;

  FGlobalParams := TTntStringList.Create;

  //Create Background for toolbars
  BuildToolbars;

  HelpTabSheetIndex := -1;

  BusyAnimate.ResName := 'dolphin_loop';
  FQueryRunningCount := 0;

  DockPnl.FullRepaint := False;
  DockPnlWindowProc := DockPnl.WindowProc;
  DockPnl.WindowProc := DockPnlIgnoreWMEraseBkGnd;
  AnimPnl.FullRepaint := False;
  AnimPnlWindowProc := AnimPnl.WindowProc;
  AnimPnl.WindowProc := AnimPnlIgnoreWMEraseBkGnd;

  //Build lists
  AddResultsetExporterMenuItems(ExportResultsetMI);

  BuildParamTV;

  //-----------------------------------------------------
  //Set properties of Main Tabcontrol
  MainTabHeaderFrame.ShowButtons := True;
  MainTabHeaderFrame.AutoHideTabHeader := ApplicationDM.QBOptions.HideTabWhenOneOpen;
  MainTabHeaderFrame.Visible := not (ApplicationDM.QBOptions.HideTabWhenOneOpen);
  MainTabHeaderFrame.OnBeforePageChange := DoBeforeMainTabChange;
  MainTabHeaderFrame.OnPageChange := DoMainTabChange;
  MainTabHeaderFrame.OnRequestNewPage := DoRequestNewPage;
  MainTabHeaderFrame.OnBeforePageDelete := DoMainTabBeforePageDelete;
  MainTabHeaderFrame.OnPageDelete := DoMainTabPageDelete;

  //-----------------------------------------------------
  //Set properties of upper sidebar Tabcontrol
  UpperTabHeaderFrame.DrawBottom := True;
  UpperTabHeaderFrame.ShowDeleteButtons := False;
  UpperTabHeaderFrame.ClearTabSheets;
  UpperTabHeaderFrame.AddTabSheet(SidebarPnl, _('Schemata'), '', SchemataPnl, nil, True, True, -1, []);
  UpperTabHeaderFrame.AddTabSheet(SidebarPnl, _('Bookmarks'), '', BookmarksPnl, nil, False, True, -1, []);
  UpperTabHeaderFrame.AddTabSheet(SidebarPnl, _('History'), '', HistoryPanel, nil, False, True, -1, []);
  UpperTabHeaderFrame.OnPageChange := DoUpperTabChange;

  SchemataFrame.PaintDefaultSchemaBold := True;
  SchemataFrame.DefaultSchema := MySQLConn.DefaultSchema;
  SchemataFrame.TopPnl.Visible := False;
  SchemataFrame.MySQLConnection := MySQLConn;
  SchemataFrame.FillSpacerPnl;
  SchemataFrame.ShowAssetsOnSchemaExpansion := True;
  SchemataFrame.ShowSchemaAssets := False;
  SchemataFrame.OnEditStoredProcedure := DoEditStoredProcedure;
  SchemataFrame.OnCreateStoredProcedure := DoEditStoredProcedure;
  SchemataFrame.OnEditView := DoEditView;
  SchemataFrame.OnCreateView := DoEditView;

  //-----------------------------------------------------
  //Set properties of lower sidebar Tabcontrol
  LowerTabHeaderFrame.DrawBottom := True;
  LowerTabHeaderFrame.ShowDeleteButtons := False;
  LowerTabHeaderFrame.ClearTabSheets;
  LowerTabHeaderFrame.AddTabSheet(SidebarPnl, _('Syntax'), '', SyntaxPnl, nil, True, True, -1, []);
  LowerTabHeaderFrame.AddTabSheet(SidebarPnl, _('Functions'), '', FunctionPanel, nil, False, True, -1, []);
  LowerTabHeaderFrame.AddTabSheet(SidebarPnl, _('Params'), '', ParamPnl, nil, False, True, -1, []);
  LowerTabHeaderFrame.AddTabSheet(SidebarPnl, _('Trx'), '', TrxPnl, nil, False, True, -1, []);

  LoadBookmarks;
  LoadHistory;
  RefreshHistoryTree;

  FunctionsOverviewTree := LoadFunctions(MYXCommonOptions.XMLDir + 'mysqlqb_functions.xml');
  BuildFunctionTree(FunctionsVT, FunctionsOverviewTree);
  SyntaxOverviewTree := LoadFunctions(MYXCommonOptions.XMLDir + 'mysqlqb_statements.xml');
  BuildFunctionTree(SyntaxVT, SyntaxOverviewTree);

  //Register global CopySQL Hotkey
  IdCopySQLHotKey := GlobalAddAtom('CopySQLHotkey');
  IdPasteSQLHotKey := GlobalAddAtom('PasteSQLHotkey');
  RegisterHotKey(Handle, IdCopySQLHotKey, MOD_CONTROL + MOD_ALT + MOD_SHIFT, Ord('C'));
  RegisterHotKey(Handle, IdPasteSQLHotKey, MOD_CONTROL + MOD_ALT + MOD_SHIFT, Ord('V'));
  hOtherWin := 0;

  if IsWinXP then // ... or higher
  begin
    Screen.Cursors[crSQLSelect] := LoadCursor(HInstance, 'sql_query_select_shadow');
    Screen.Cursors[crSQLFrom] := LoadCursor(HInstance, 'sql_query_from_shadow');
    Screen.Cursors[crSQLWhere] := LoadCursor(HInstance, 'sql_query_where_shadow');
    Screen.Cursors[crSQLGroup] := LoadCursor(HInstance, 'sql_query_group_shadow');
    Screen.Cursors[crSQLHaving] := LoadCursor(HInstance, 'sql_query_having_shadow');
    Screen.Cursors[crSQLOrder] := LoadCursor(HInstance, 'sql_query_order_shadow');
    Screen.Cursors[crSQLSet] := LoadCursor(HInstance, 'sql_query_set_shadow');
  end
  else
  begin
    Screen.Cursors[crSQLSelect] := LoadCursor(HInstance, 'sql_query_select');
    Screen.Cursors[crSQLFrom] := LoadCursor(HInstance, 'sql_query_from');
    Screen.Cursors[crSQLWhere] := LoadCursor(HInstance, 'sql_query_where');
    Screen.Cursors[crSQLGroup] := LoadCursor(HInstance, 'sql_query_group');
    Screen.Cursors[crSQLHaving] := LoadCursor(HInstance, 'sql_query_having');
    Screen.Cursors[crSQLOrder] := LoadCursor(HInstance, 'sql_query_order');
    Screen.Cursors[crSQLSet] := LoadCursor(HInstance, 'sql_query_set');
  end;

  // Setup the SQL highlighter.
  with UCESQLHighlighter do
  begin
    CommentAttributes.Foreground := clGray;
    CommentAttributes.Style := [fsItalic];
    KeyAttributes.Foreground := clBlue;
    StringAttributes.Foreground := $0080FF;

    IdentifierAttributes.Foreground := clWindowText;
    QuotedIDAttributes.Foreground := $804000;
    QuotedIDAttributes.Style := [fsBold];

    NumberAttributes.Foreground := clFuchsia;
    SpaceAttributes.Foreground := $808080;
    SymbolAttributes.Foreground := clWindowText;

    EmbeddedCommandAttributes.Foreground := clNavy;
    EmbeddedCommandAttributes.Background := $F0F0F0;

    SystemVariableAttributes.Foreground := $808000;
    SystemVariableAttributes.Style := [fsBold];
    UserVariableAttributes.Foreground := $C08080;
    UserVariableAttributes.Style := [fsBold];
  end;

  // Create and setup the actual SQL script editor.
  FSQLEditor := TSQLEditor.Create(Self, MYXCommonOptionProvider);
  FSQLEditor.Parent := QueryToolbarPnl;
  with FSQLEditor do
  begin
    Left := 144;
    Top := 3;
    Width := 541;
    Height := 46;
    Cursor := crIBeam;
    Anchors := [akLeft, akTop, akRight];
    PopupMenu := QueryPopupMenu;
    ScrollBars := ssNone;
    TabOrder := 1;
    TabSize := 2;
    Name := 'QueryArea';

    OnDragDrop := QueryEditorDragDrop;
    OnDragOver := QueryEditorDragOver;
    OnEnter := QueryEditorEnter;
    OnKeyDown := QueryEditorKeyDown;
    OnMouseDown := QueryEditorMouseDown;
    OnMouseMove := QueryEditorMouseMove;
    OnMouseUp := QueryEditorMouseUp;
    OnCaretChange := EditCaretChange;

    DefaultStyle := TInactiveCommandStyle.Create;

    Color := clWindow;
    Font.Name := MYXCommonOptionProvider.OptionAsString['CodeFontName'];
    Font.Height := MYXCommonOptionProvider.OptionAsInteger['CodeFontHeight'];
    Highlighter := UCESQLHighlighter;
    Options := [eoAutoIndent, eoAutoUnindent, eoGroupUndo, eoInserting, eoLineNumbers, eoShowScrollHint, eoTripleClicks,
      eoUndoAfterSave, eoUseSyntaxHighlighting, eoWantTabs, eoUseUndoRedo];

    if MYXCommonOptionProvider.OptionAsBoolean['EditorShowWhitespaces'] then
      Options := Options + [eoShowControlChars];

    GutterWidth := 0;
    GutterColor := clBtnFace;
    RightMargin := -1;
    MaxUndo := 32000;

    ScrollHintColor.Foreground := clWhite;
    ScrollHintColor.Background := clAppWorkSpace;
    SelectedColor.Foreground := clHighlightText;
    SelectedColor.Background := clHighlight;
    LineNumberFont.Name := 'Terminal';
    LineNumberFont.Size := 6;

    // Add own keystrokes.

    // Shift+Bk like Bk alone
    Stroke := Keystrokes.Add;
    Stroke.Command := ecDeleteLastChar;
    Stroke.Key := VK_BACK;
    Stroke.Shift := [ssShift];
  end;

  FCurrentCommandStart.Y := -1;
  FCurrentCommandEnd.Y := -1;
  FCurrentCommandStyle := TCurrentCommandStyle.Create;

  SQLEditorMaximized := ApplicationDM.QBOptions.MaximizeSQLEdit;

  if ApplicationDM.QBOptions.ShowQuickStart then
    ShowQuickStartGuide;
  if not (MySQLConn.Connected) then
    ApplicationDM.QBOptions.OnlyTabsheets := True;

  DoOptionsChanged;
  MySQLConnUpdateState;
  AddCatalogTreePopupMenuItems;

  FSetFocusToQueryEditorAfterMouseUp := False;

  PrepareQueryAreaHeaderBitmap;
  DoQueryEditorChange;

  PostMessage(Handle, WM_DO_INIT, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FormDestroy(Sender: TObject);

begin
  FreeAndNil(Bookmarks);

  if (FHistory <> nil) then
    myx_history_free(FHistory);
  FHistory := nil;

  if (FHistoryTree <> nil) then
    myx_history_free_tree(FHistoryTree);
  FHistoryTree := nil;

  FreeAndNil(SyntaxOverviewTree);
  FreeAndNil(FunctionsOverviewTree);

  if (PSQLStripped <> nil) then
    myx_free_stripped_sql(PSQLStripped);

  PNGImages.Free;
  FMaximizedQueryAreaHeaderBGBmp.Free;

  UnRegisterHotKey(Handle, IdCopySQLHotKey);
  GlobalDeleteAtom(IdCopySQLHotKey);

  FTrxQueryList.Free;
  FGlobalParams.Free;
  FSearchLock.Free;

  MySQLConn.RemoveTransactionStatusListener(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  try
    StopSearch;

    StoreBookmarks;
    StoreHistory;

    FreeAndNil(Toolbars);

    ApplicationDM.QBOptions.GlobalParametersAsStrings := FGlobalParams.Text;
  except
    //Catch all exceptions and ignore them
  end;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddCatalogTreePopupMenuItems;
var
  MenuItem: TTntMenuItem;
begin
  MenuItem := TTntMenuItem.Create(SchemataFrame);
  MenuItem.Caption := '-';
  SchemataFrame.SchemaTreeViewPopupMenu.Items.Add(MenuItem);

  ChangeDefaultSchemaMI := TTntMenuItem.Create(SchemataFrame);
  ChangeDefaultSchemaMI.Caption := _('Make Default Schema');
  ChangeDefaultSchemaMI.OnClick := DoMakeDefaultSchema;
  SchemataFrame.SchemaTreeViewPopupMenu.Items.Add(ChangeDefaultSchemaMI);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BuildToolbars;

var
  PNGImg: TPNGObject;
  ToolbarBGBitmap: TBitmap;
  CurrentGroup: TToolbarItemGroup;
  QueryToolbarContainer,
  ScriptToolbarContainer,
  AdvQueryToolbarContainer,
  DebugToolbarContainer: TToolbarContainer;

begin
  QueryToolbarContainer := Toolbars.Toolbars['Query'];
  ScriptToolbarContainer := Toolbars.Toolbars['Script'];
  AdvQueryToolbarContainer := Toolbars.Toolbars['AdvancedQuery'];
  DebugToolbarContainer := Toolbars.Toolbars['Debug'];

  //Make background
  if (ApplicationDM.QBOptions.UseToolbarGradient) then
  begin
    PNGImg := LoadPNGImageFromResource('toolbar_bg1');
    ToolbarBGBitmap := TBitmap.Create;
    ToolbarBGBitmap.Width := 12;
    ToolbarBGBitmap.Height := 150; //Make it big enough so we don't need to handle area below image
    ToolbarBGBitmap.Canvas.Brush.Color := clBtnFace;
    ToolbarBGBitmap.Canvas.FillRect(Rect(0, 0,
      ToolbarBGBitmap.Width, ToolbarBGBitmap.Height));
    PNGImg.Draw(ToolbarBGBitmap.Canvas, Rect(0, 0,
      PNGImg.Width, PNGImg.Height));
    PNGImg.Free;

    QueryToolbarContainer.BGBitmap := ToolbarBGBitmap;
    ScriptToolbarContainer.BGBitmap := ToolbarBGBitmap;

    ToolbarBGBitmap.Free;

    //Advanced Toolbar BGs
    PNGImg := LoadPNGImageFromResource('toolbar_bg2');
    ToolbarBGBitmap := TBitmap.Create;
    ToolbarBGBitmap.Width := PNGImg.Width;
    ToolbarBGBitmap.Height := PNGImg.Height;
    ToolbarBGBitmap.Canvas.Brush.Color := clBtnFace;
    ToolbarBGBitmap.Canvas.FillRect(Rect(0, 0,
      ToolbarBGBitmap.Width, ToolbarBGBitmap.Height));
    PNGImg.Draw(ToolbarBGBitmap.Canvas, Rect(0, 0,
      PNGImg.Width, PNGImg.Height));
    PNGImg.Free;

    AdvQueryToolbarContainer.BGBitmap := ToolbarBGBitmap;
    DebugToolbarContainer.BGBitmap := ToolbarBGBitmap;

    ToolbarBGBitmap.Free;
  end;

  //Load
  PNGImages.Add(LoadPNGImageFromResource('sakila', AnimStillImg));
  PNGImages.Add(LoadPNGImageFromResource('sakila', ScriptAnimStillImg));

  // Todo: make QueryGoBackLblClick drop down list of executed queries

  //------------------------------------------------------------
  // Query Panel

  QueryToolbarContainer.RightSpace := 88;
  CurrentGroup := QueryToolbarContainer.AddGroup('History', False);
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'QueryBackBtn', 'query_back',
    _('Go back'), _('Go back in query history'),
    6, True, True, False, QueryGoBackClick,
    nil, nil, False); //, QueryGoBackLblClick);

  CurrentGroup.AddItem(
    'QueryNextBtn', 'query_next',
    _('Next'), _('Go forward in query history'),
    6, True, True, False, QueryNextClick,
    nil, nil, False);

  CurrentGroup.AddItem(
    'QueryRefreshBtn', 'query_refresh',
    _('Refresh'), _('Refresh the result grid'),
    6, True, True, False, QueryRefreshClick,
    nil, nil, False);

  CurrentGroup := QueryToolbarContainer.AddGroup('Query', False, True);
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'QueryExecuteBtn', 'query_execute',
    _('Execute'), _('Execute the entered query'),
    6, True, True, True, QueryExecuteClick, QueryExecuteLblClick);

  CurrentGroup.AddItem(
    'QueryStopBtn', 'query_stop',
    _('Stop'), _('Stop the execution of the query'),
    6, True, True, True, QueryStopClick,
    nil, nil, False);

  //------------------------------------------------------------
  // Script Panel

  ScriptToolbarContainer.RightSpace := 93;
  CurrentGroup := ScriptToolbarContainer.AddGroup('Undo', False);
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'ScriptUndoBtn', 'script_undo',
    _('Undo'), _('Undo the last change'),
    6, True, True, False, ScriptUndoClick,
    nil, nil, False); //, QueryGoBackLblClick);

  CurrentGroup.AddItem(
    'ScriptRedoBtn', 'script_redo',
    _('Redo'), _('Apply the last undone action again'),
    6, True, True, False, ScriptRedoClick,
    nil, nil, False);

  CurrentGroup := ScriptToolbarContainer.AddGroup('Edit');
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'ScriptLoadBtn', 'script_load',
    _('Load'), _('Load a script'),
    6, True, True, False, ScriptLoadClick);

  CurrentGroup.AddItem(
    'ScriptSaveBtn', 'script_save',
    _('Save'), _('Save the script'),
    6, True, True, False, ScriptSaveClick);

  CurrentGroup.AddItem(
    'ScriptSearchBtn', 'script_search',
    _('Search'), _('Search and replace text'),
    6, True, True, False, ScriptSearchClick);

  CurrentGroup := ScriptToolbarContainer.AddGroup('ScriptExecute', True, True);
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'ScriptExecuteBtn', 'script_execute',
    _('Execute'), _('Execute the script'),
    6, True, True, True, ScriptExecuteClick);

  CurrentGroup.AddItem(
    'ScriptStopBtn', 'script_stop',
    _('Stop'), _('Stop the execution of the script'),
    6, True, True, True, ScriptStopClick,
    nil, nil, False);

  CurrentGroup := ScriptToolbarContainer.AddGroup('ScriptDebug', True, True);
  CurrentGroup.Space := 12;

  CurrentGroup.AddItem(
    'ScriptPauseBtn', 'script_pause',
    _('Pause'), _('Pauses the execution of the script'),
    6, True, True, True,
    ScriptPauseClick,
    nil, nil, False);

  CurrentGroup.AddItem(
    'ScriptContinueBtn', 'script_continue',
    _('Continue'), _('Executes the script and stops on errors'),
    6, True, True, True,
    ScriptContinueClick);

  CurrentGroup.AddItem(
    'ScriptStopOverBtn', 'script_step_over',
    _('Step'), _('Executes the next statement but steps over functions'),
    6, True, True, True,
    ScriptStepOverClick);

  {CurrentGroup.AddItem(
    'ScriptStepIntoBtn', 'script_step_into',
    'Step Into', _('Executes the next statement and steps into functions'),
    6, True, True, True,
    ScriptStepIntoClick, nil,
    nil, False);

  CurrentGroup.AddItem(
    'ScriptReturnBtn', 'script_step_return',
    'Return', _('Runs the script until the functions returns'),
    6, True, True, True,
    ScriptRunUntilReturnClick, nil,
    nil, False);}


  //------------------------------------------------------------
  // F11 Query Panel

  AdvQueryToolbarContainer.RightSpace := 24;

  CurrentGroup := AdvQueryToolbarContainer.AddGroup('History');

  CurrentGroup.AddItem(
    'QueryBackBtn', 'small_query_back',
    _('Go back'), _('Go back in query history'),
    3, False, False, False, QueryGoBackClick,
    nil, nil, False); //, QueryGoBackLblClick);

  CurrentGroup.AddItem(
    'QueryNextBtn', 'small_query_next',
    _('Next'), _('Go forward in query history'),
    3, False, False, False, QueryNextClick,
    nil, nil, False);

  CurrentGroup.AddItem(
    'QueryRefreshBtn', 'small_query_refresh',
    _('Refresh'), _('Refresh the result grid'),
    3, False, False, False, QueryRefreshClick,
    nil, nil, False);

  CurrentGroup := AdvQueryToolbarContainer.AddGroup('Execute');

  CurrentGroup.AddItem(
    'QueryExecuteBtn', 'small_query_execute',
    _('Execute'), _('Execute the entered query'),
    3, False, False, False, QueryExecuteClick, QueryExecuteLblClick);

  CurrentGroup.AddItem(
    'QueryStopBtn', 'small_query_stop',
    _('Stop'), _('Stop the execution of the query'),
    3, False, False, False, QueryStopClick,
    nil, nil, False);

  CurrentGroup := AdvQueryToolbarContainer.AddGroup('Transaction');

  CurrentGroup.AddItem(
    'TrxLbl', '',
    _('Transaction'), '',
    10, True, False, False, nil);

  CurrentGroup.AddItem(
    'TrxStartBtn', 'small_query_start',
    '', _('Start a new transaction'),
    3, False, False, False, TransStartClick);

  CurrentGroup.AddItem(
    'TrxCommitBtn', 'small_query_commit',
    '', _('Commit the transaction'),
    3, False, False, False, TransCommitClick,
    nil, nil, False);

  CurrentGroup.AddItem(
    'TrxRollbackBtn', 'small_query_rollback',
    '', _('Rollback the transaction'),
    3, False, False, False, TransRollbackClick,
    nil, nil, False);

  {//Locks
  Lbl:=TTntLabel.Create(self);
  Lbl.Parent:=FunctionToolbarPnl;
  Lbl.Left:=xpos;
  Lbl.Top:=10;
  Lbl.Transparent:=True;
  Lbl.Caption:=_('Table Locks');
  xpos:=xpos+Lbl.Width+15;

  LockEnableToolbarItem:=TToolbarItem.Create(
    FunctionToolbarPnl, 'lock_tables',
    '', _('Locks all selected tables automatically'),
    xpos, 3, False, False, False, QueryLockTablesClick);
  ToolbarItemsList.Add(LockEnableToolbarItem);
  xpos:=xpos+35;}

  {LockDisableToolbarItem:=TToolbarItem.Create(
    FunctionToolbarPnl, 'lock_disable',
    '', _('Releases all locked tables'),
    xpos, 3, False, False, False, TransCommitClick);
  ToolbarItemsList.Add(LockDisableToolbarItem);
  LockDisableToolbarItem.Enabled:=False;
  xpos:=xpos+35;}

  CurrentGroup := AdvQueryToolbarContainer.AddGroup('Special');

  //Special
  CurrentGroup.AddItem(
    'ExplainBtn', 'small_query_explain',
    _('Explain'), _('Calls the explain command to examine the query'),
    3, True, False, False, DoExplain, nil, nil, False);

  CurrentGroup.AddItem(
    'CompareBtn', 'small_query_compare',
    _('Compare'), _('Executes the query and compares it with the current resultset'),
    3, True, False, False, DoCompare, nil, nil, False);


  //Mouse
  CurrentGroup:=AdvQueryToolbarContainer.AddGroup('Mouse');

  CurrentGroup.AddItem(
    'MouseStdBtn', 'mouse_std',
    '', _('Sets the standard mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick,
    nil, True, True);

  CurrentGroup.AddItem(
    'MouseSelectBtn', 'mouse_select',
    '', _('Sets the SELECT clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseFromBtn', 'mouse_from',
    '', _('Sets the FROM clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseWhereBtn', 'mouse_where',
    '', _('Sets the WHERE clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseGroupBtn', 'mouse_group',
    '', _('Sets the GROUP clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseHavingBtn', 'mouse_having',
    '', _('Sets the HAVING clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseOrderBtn', 'mouse_order',
    '', _('Sets the ORDER clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.AddItem(
    'MouseSetBtn', 'mouse_set',
    '', _('Sets the SET clause mouse cursor'),
    3, False, False, False,
    MouseCursorClick, MouseCursorClick);

  CurrentGroup.Visible:=ApplicationDM.QBOptions.ShowMouseCursorToolbarGroup;

  //Mouse
  CurrentGroup:=AdvQueryToolbarContainer.AddGroup('SQL');

  CurrentGroup.AddItem('SQLCreateViewBtn', 'sql_create_view', '', _('Create a view from the current select'), 3, False,
    False, False, SQLCreateViewClick, nil, nil, MySQLConn.MajorVersion >= 5);

  //-------------------------------------------------------------
  // F11 Script toolbar

  CurrentGroup := DebugToolbarContainer.AddGroup('ScriptSmallUndo');

  CurrentGroup.AddItem(
    'ScriptSmallUndoBtn', 'small_script_undo',
    _('Undo'), _('Undo the last change'),
    3, False, False, False,
    ScriptContinueClick);

  CurrentGroup.AddItem(
    'ScriptSmallRedoBtn', 'small_script_redo',
    _('Redo'), _('Apply the last undone action again'),
    3, False, False, False,
    ScriptContinueClick);


  CurrentGroup := DebugToolbarContainer.AddGroup('ScriptSmallEdit');

  CurrentGroup.AddItem(
    'ScriptSmallLoadBtn', 'small_script_load',
    _('Load'), _('Load a script'),
    3, False, False, False, ScriptLoadClick);

  CurrentGroup.AddItem(
    'ScriptSmallSaveBtn', 'small_script_save',
    _('Save'), _('Save the script'),
    3, False, False, False, ScriptSaveClick);

  CurrentGroup.AddItem(
    'ScriptSmallSearchBtn', 'small_script_search',
    _('Search'), _('Search and replace text'),
    3, False, False, False, ScriptSearchClick);


  CurrentGroup := DebugToolbarContainer.AddGroup('ScriptSmallDebug');

  CurrentGroup.AddItem(
    'ScriptSmallPauseBtn', 'small_script_pause',
    _('Pause'), _('Pauses the execution'),
    3, True, False, False,
    ScriptPauseClick,
    nil, nil, False);

  CurrentGroup.AddItem(
    'ScriptSmallContinueBtn', 'small_script_continue',
    _('Continue'), _('Executes the script and stops on errors and breakpoints'),
    3, True, False, False,
    ScriptContinueClick);

  CurrentGroup.AddItem(
    'ScriptSmallStopOverBtn', 'small_script_step_over',
    _('Step'), _('Executes the next statement but steps over functions'),
    3, True, False, False,
    ScriptStepOverClick);

  {CurrentGroup.AddItem(
    'ScriptSmallStepIntoBtn', 'small_script_step_into',
    'Step Into', _('Executes the next statement and steps into functions'),
    3, True, False, False,
    ScriptStepIntoClick, nil,
    nil, False);

  CurrentGroup.AddItem(
    'ScriptSmallRunUntilReturnBtn', 'small_script_step_return',
    'Run until Return', _('Runs the script until the functions returns'),
    3, True, False, False,
    ScriptRunUntilReturnClick, nil,
    nil, False);}

  CurrentGroup := DebugToolbarContainer.AddGroup('ScriptSmallExecute');

  CurrentGroup.AddItem(
    'ScriptSmallExecuteBtn', 'small_script_execute',
    _('Execute'), _('Executes the script without stopping on errors and breakpoints'),
    3, True, False, False, ScriptExecuteClick);

  CurrentGroup.AddItem(
    'ScriptSmallStopBtn', 'small_script_stop',
    _('Stop'), _('Stops the execution of the script'),
    3, True, False, False,
    ScriptStopClick, nil,
    nil, False);

  {CurrentGroup := DebugToolbarContainer.AddGroup('RunAs');

  CurrentGroup.AddItem(
    'RunAsLbl', '',
    _('Run as'), '',
    10, True, False, False, nil);

  ScriptTypeCBox := TTntComboBox.Create(DebugToolbarPnl);
  ScriptTypeCBox.Parent := DebugToolbarPnl;
  ScriptTypeCBox.Font.Name := 'Tahoma';
  ScriptTypeCBox.Font.Size := 8;
  ScriptTypeCBox.Top := 4;
  ScriptTypeCBox.Width := 120;
  ScriptTypeCBox.Items.Add(_('Normal Script'));
  ScriptTypeCBox.Items.Add(_('Stored Procedure'));
  ScriptTypeCBox.Style := csDropDownList;
  ScriptTypeCBox.TabStop := False;
  ScriptTypeCBox.ItemIndex := 0;
  ScriptTypeCBox.OnChange := DoScriptTypeChange;
  ScriptTypeCBox.OnCloseUp := DoScriptTypeChange;

  CurrentGroup.AddItem(
    'ScriptTypeCBox', '',
    '', '',
    10, True, False, False, nil, nil, ScriptTypeCBox);

  CurrentGroup.Visible := False;}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RefreshQueryNavButtons;

var
  Resultset: TMySQLRS;
  HasText: Boolean;

begin
  Resultset := ActiveResultset;

  if Assigned(Resultset) then
  begin
    Toolbars.ItemsEnabled['QueryBackBtn'] := Resultset.CommandList.HasBack;
    Toolbars.ItemsEnabled['QueryNextBtn'] := Resultset.CommandList.HasNext;

    HasText := Resultset.SQL <> '';
    Toolbars.ItemsEnabled['QueryRefreshBtn'] := HasText;
    Toolbars.ItemsEnabled['ExplainBtn'] := HasText;
    Toolbars.ItemsEnabled['CompareBtn'] := HasText;
  end
  else
  begin
    Toolbars.ItemsEnabled['QueryBackBtn'] := False;
    Toolbars.ItemsEnabled['QueryNextBtn'] := False;
    Toolbars.ItemsEnabled['QueryRefreshBtn'] := False;
    Toolbars.ItemsEnabled['ExplainBtn'] := False;
    Toolbars.ItemsEnabled['CompareBtn'] := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryGoBackClick(Sender: TObject);

begin
  if Assigned(ActiveResultset) then
    HandleEditorCommand(edBack);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryGoBackLblClick(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryNextClick(Sender: TObject);

begin
  if Assigned(ActiveResultset) then
    HandleEditorCommand(edNext);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryRefreshClick(Sender: TObject);

begin
  if Assigned(ActiveResultset) then
  begin
    if FSQLEditor.Modified then
      HandleEditorCommand(edBack);
    HandleEditorCommand(edRefresh);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryExecuteClick(Sender: TObject);

begin
  HandleEditorCommand(edExecute);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryExecuteInNewTabClick(Sender: TObject);

var
  S: WideString;

begin
  S := FCurrentCommand;
  FActiveRSTabSheet := AddNewRSTabSheet;
  FCurrentCommandStart.Y := -1;
  FCurrentCommandEnd.Y := -1;
  FSQLEditor.Text := S + ';';
  HandleEditorCommand(edExecute);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QuerySplitAndExecuteClick(Sender: TObject);

var
  S: WideString;

begin
  if Assigned(FActiveRSTabSheet) then
  begin
    S := FCurrentCommand;
    FActiveRSTabSheet.AddRSPanel(MySQLConn);
    FCurrentCommandStart.Y := -1;
    FCurrentCommandEnd.Y := -1;
    FSQLEditor.Text := S + ';';
    HandleEditorCommand(edExecute);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryExecuteLblClick(Sender: TObject);

var
  Pnt: TPoint;
  
begin
  Pnt := TControl(Sender).ClientToScreen(Point(0, 0));

  QueryExecutePopupMenu.Popup(Pnt.X, Pnt.Y + TControl(Sender).Height + 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryStopClick(Sender: TObject);

begin
  if Assigned(ActiveResultset) then
  begin
    if ActiveResultset.StopQuery and ActiveResultset.QueryExecuting then
    begin
      // Query is already being terminated. If the users sent us here then query is likely to hang
      // and did not come back to the application so that the thread could be stopped normally.
      if ShowModalDialog(_('Query thread does not respond'), _('It looks like the thread executing your query hangs ' +
        'on the server. This makes it impossible to tell it to stop working. If you continue now then the thread itself ' +
        'will be killed and a new one is created next time you issue a query. Do you want this?'), myx_mtWarning,
        _('Yes') + #13#10 + _('No')) = 1 then
        ActiveResultset.ForceStop;
    end
    else
      ActiveResultset.StopQuery := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransStartClick(Sender: TObject);

begin
  MySQLConn.ExecuteDirect('START TRANSACTION');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransCommitClick(Sender: TObject);

begin
  MySQLConn.ExecuteDirect('COMMIT');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransRollbackClick(Sender: TObject);

begin
  MySQLConn.ExecuteDirect('ROLLBACK');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryLockTablesClick(Sender: TObject);

begin
  {LockEnableToolbarItem.Down :=
    not (LockEnableToolbarItem.Down);}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoExplain(Sender: TObject);

begin
  HandleEditorCommand(edExplain);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoCompare(Sender: TObject);

begin
  HandleEditorCommand(edCompare);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MouseCursorClick(Sender: TObject);

var ItemName: WideString;

begin
  Toolbars.ItemsDown['MouseStdBtn']:= False;
  Toolbars.ItemsDown['MouseSelectBtn']:= False;
  Toolbars.ItemsDown['MouseFromBtn']:= False;
  Toolbars.ItemsDown['MouseWhereBtn']:= False;
  Toolbars.ItemsDown['MouseGroupBtn']:= False;
  Toolbars.ItemsDown['MouseHavingBtn']:= False;
  Toolbars.ItemsDown['MouseOrderBtn']:= False;
  Toolbars.ItemsDown['MouseSetBtn']:= False;

  ItemName := Toolbars.GetItemNameByObject(Sender);

  if (ItemName = 'MouseSelectBtn') then
    SchemataFrame.CatalogVST.Cursor := crSQLSelect
  else
    if (ItemName = 'MouseFromBtn') then
      SchemataFrame.CatalogVST.Cursor := crSQLFrom
    else
      if (ItemName = 'MouseWhereBtn') then
        SchemataFrame.CatalogVST.Cursor := crSQLWhere
      else
        if (ItemName = 'MouseGroupBtn') then
          SchemataFrame.CatalogVST.Cursor := crSQLGroup
        else
          if (ItemName = 'MouseHavingBtn') then
            SchemataFrame.CatalogVST.Cursor := crSQLHaving
          else
            if (ItemName = 'MouseOrderBtn') then
              SchemataFrame.CatalogVST.Cursor := crSQLOrder
            else
              if (ItemName = 'MouseSetBtn') then
                SchemataFrame.CatalogVST.Cursor := crSQLSet
              else
                SchemataFrame.CatalogVST.Cursor := crDefault;

  Perform(WM_CursorChanged, SchemataFrame.CatalogVST.Cursor, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DockPnlIgnoreWMEraseBkGnd(var Msg: TMessage);

begin
  if (Msg.Msg = WM_ERASEBKGND) then
    Msg.Result := 0
  else
    DockPnlWindowProc(Msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AnimPnlIgnoreWMEraseBkGnd(var Msg: TMessage);

begin
  if (Msg.Msg = WM_ERASEBKGND) then
    Msg.Result := 0
  else
    AnimPnlWindowProc(Msg);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQueryEditorChange;

var
  Lines: Integer;

begin
  if not FIsLoading then
  begin
    // Recalculate height
    if (not SQLEditorMaximized) and (
      (FSQLEditor.Height <> FSQLEditor.Content.Count * FSQLEditor.LineHeight + 7) or
      ((FSQLEditor.Content.Count > 10) and (FSQLEditor.Height <> 10 * FSQLEditor.LineHeight + 7))) then
    begin
      if (FSQLEditor.Content.Count >= 3) and (FSQLEditor.Content.Count <= 10) then
      begin
        Lines := FSQLEditor.Content.Count;
        FSQLEditor.ScrollBars := ssNone;
      end
      else
        if (FSQLEditor.Content.Count > 10) then
        begin
          Lines := 10;
          FSQLEditor.ScrollBars := ssVertical;
        end
        else
        begin
          Lines := 3;
          FSQLEditor.ScrollBars := ssNone;
        end;

      // Add 4 pixels for the inner border above and below.
      FSQLEditor.Height := Max(47, Min(200, Lines * FSQLEditor.LineHeight + 4));
      QueryToolbarPnl.Height := FSQLEditor.Height + 9;
      ToolbarPnl.Height := QueryToolbarPnl.Height +
        AdvancedQueryToolbarPnl.Height * Ord(ApplicationDM.QBOptions.ShowAdvancedToolbar);
    end;

    // Reset command indicators if the edit was just cleared.
    if FSQLEditor.Content.Count = 0 then
    begin
      FCurrentCommandStart.Y := -1;
      FCurrentCommandEnd.Y := -1;
      FCurrentCommand := '';
    end;

    PostMessage(Handle, WM_UPDATE_COMMAND, 0, 0);
    
    // Refresh nav buttons, because the current query may have become different than the last
    RefreshQueryNavButtons;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.CreateNextRSForMultipleRSQuery(Sender: TObject): TMySQLRS;

begin
  if Assigned(FActiveRSTabSheet) then
    Result := FActiveRSTabSheet.AddRSPanel(MySQLConn, True, True).MySQLRS
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RemoveRSForMultipleRSQuery(Sender: TObject);

begin
  FActiveRSTabSheet.DeleteRSPanel(ActiveResultsetPanel);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowRSForMultipleRSQuery(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    ActiveResultsetPanel.TotalHeight := 150;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.AddNewRSTabSheet(Caption: WideString): TRSTabSheet;

var
  RSTabSheet: TRSTabSheet;
  RSPnl: TMySQLRSPanel;
  
begin
  //Create tabSheet
  RSTabSheet := TRSTabSheet.Create(TabsPnl);

  RSTabSheet.Align := alClient;
  RSTabSheet.Constraints.MinHeight := 40;

  RSTabSheet.OnPanelChanging := ActivePanelChanging;
  RSTabSheet.OnPanelChanged := ActivePanelChanged;

  RSTabSheet.InitControls := True;
  try
    //Create RSPanel
    RSPnl := RSTabSheet.AddRSPanel(MySQLConn);
    if (RSPnl <> nil) then
    begin
      RSPnl.MySQLRS.OnParamChange := DoRefreshParams;
      RSPnl.MySQLRS.OnQueryExecute := DoQueryExecute;
      RSPnl.MySQLRS.OnQueryExecuted := DoQueryExecuted;
      RSPnl.MySQLRS.OnQuerySuccess := DoQuerySuccess;
      RSPnl.MySQLRS.OnQueryStopped := DoQueryStopped;
      RSPnl.MySQLRS.OnQueryError := DoQueryError;
      RSPnl.MySQLRS.OnConfirmDeletion := DoConfirm;
      RSPnl.MySQLRS.CreateNextRSForMultipleRSQuery := CreateNextRSForMultipleRSQuery;
      RSPnl.MySQLRS.RemoveRSForMultipleRSQuery := RemoveRSForMultipleRSQuery;
      RSPnl.MySQLRS.ShowRSForMultipleRSQuery := ShowRSForMultipleRSQuery;
      RSPnl.MySQLRS.GlobalParams := FGlobalParams;

      RSPnl.RSGrid.PopupMenu := RSGridPopupMenu;
      RSPnl.RSGrid.OnFocusChanged := RSGridFocusChanged;
      RSPnl.RSGrid.OnDropTables := DoDropTables;
    end;
  finally
    RSTabSheet.InitControls := False;
  end;

  Inc(FRSCount);
  if Caption='' then
    Caption := Format(_('Resultset %d'), [FRSCount]);

  MainTabHeaderFrame.AddTabSheet(self, Caption, 'tabsheet_icon_resultset', TTntPanel(RSTabSheet), RSTabSheet, True,
    False);


  if (MainTabHeaderFrame.TabCount > 1) and
    (not (MainTabHeaderFrame.Visible)) then
    MainTabHeaderFrame.Show;

  RSTabSheet.ShowFieldOverlayImages := ApplicationDM.QBOptions.ShowFieldOverlayImages;

  Result := RSTabSheet;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.AddNewScriptTabSheet(Caption: WideString): TScriptTabSheet;

var
  ScriptTabSheet: TScriptTabSheet;
  ScriptPanel: TScriptPanel;
  ParentForm: TCustomForm;

begin
  //Create tabSheet
  ScriptTabSheet := TScriptTabSheet.Create(TabsPnl);

  ScriptTabSheet.Align := alClient;

  //Create ScriptPanel
  ScriptPanel := ScriptTabSheet.AddScriptPanel(MySQLConn, MYXCommonOptionProvider);
  ScriptPanel.ScriptEditor.PopupMenu := ScriptMemoPopupMenu;
  ScriptPanel.ScriptEditor.OnCaretChange := EditCaretChange;
  ScriptPanel.ScriptEditor.OnDragOver := ScriptDragOver;
  ScriptPanel.ScriptEditor.OnDragDrop := ScriptDragDrop;
  ScriptPanel.ProgressMonitor := Self;

  Inc(FScriptCount);
  if Caption='' then
    Caption := 'Script ' + IntToStr(FScriptCount);

  ScriptTabSheet.TabIndex :=
    MainTabHeaderFrame.AddTabSheet(self, Caption, 'tabsheet_icon_script', TTntPanel(ScriptTabSheet), ScriptTabSheet,
    True, False);

  //ScriptTabSheet.OnBeforeActiveScriptPanelChanged:=DoBeforeActiveScriptPanelChanged;
  ScriptTabSheet.OnActivateScriptPanel := DoActiveScriptPanelChanged;

  if (MainTabHeaderFrame.TabCount > 1) and
    (not (MainTabHeaderFrame.Visible)) then
    MainTabHeaderFrame.Show;

  ParentForm := GetParentForm(ScriptPanel);
  if (ParentForm <> nil) then
    if (ParentForm.Visible and ParentForm.Enabled) then
      ScriptPanel.ScriptEditor.SetFocus;

  Result := ScriptTabSheet;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoActiveScriptPanelChanged(Sender: TObject);

var
  Editor: TScriptEditor;

begin
  if (Sender<>nil) and (Sender is TScriptPanel)then
  begin
    FActiveScriptTabSheet := TScriptTabSheet(TScriptPanel(Sender).Parent);
    Editor := TScriptPanel(Sender).ScriptEditor;
    EditCaretChange(Editor, Editor.CaretX, Editor.CaretY);
    TScriptPanel(Sender).Active := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoOnFreeVTNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

begin
  Finalize(PHistoryNodeData(Sender.GetNodeData(Node))^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoBeforeMainTabChange(Sender: TObject; PreviousSelectedTab: Integer;
  PreviousSelectedObj: TObject; NewSelectedTab: Integer; obj: TObject);

var
  Triggered: Boolean;

begin
  Triggered := False;

  if PreviousSelectedObj is TRSTabSheet then
  begin
    ActivePanelChanging(TRSTabSheet(PreviousSelectedObj).ActiveRSPanel);
    Triggered := True;
  end
  else
    if (PreviousSelectedObj is TScriptTabSheet) and Assigned(TScriptTabSheet(PreviousSelectedObj).ActiveScriptPanel) then
      TScriptTabSheet(PreviousSelectedObj).ActiveScriptPanel.Active := False;

  if not (Triggered) then
    ActivePanelChanging(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoMainTabChange(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
  NewSelectedTab: Integer; obj: TObject);

var
  Grid: TMySQLRSGrid;
  QueryIsRunning: Boolean;
  
begin
  QueryIsRunning := False;
  
  // It is necessary to handle every combination of page types a user can switch.
  // Determine previously shown sheet type.
  if PreviousSelectedObj is TRSTabSheet then
  begin
    // Switching from a result set.
    // Now see where we go to.
    if obj is TRSTabSheet then
    begin
      // The new page is again a result set.
      ActivePanelChanged(TRSTabSheet(PreviousSelectedObj).ActiveRSPanel, TRSTabSheet(obj).ActiveRSPanel);

      QueryIsRunning := TRSTabSheet(obj).ActiveRSPanel.MySQLRS.QueryExecuting;
      Grid := TRSTabSheet(obj).ActiveRSPanel.RSGrid;
      RSGridFocusChanged(Grid, Grid.FocusedNode, Grid.FocusedColumn);
    end
    else
    begin
      // Going to a script or help page. Hide the SQL input area if maximized.
      if SQLEditorMaximized then
        SQLEditMaximizedPnl.Hide;
      FActiveRSTabSheet := nil;

      if obj is TScriptTabSheet then
      begin
        CurrentPerspective := apScript;
        QueryIsRunning := TScriptTabSheet(obj).ActiveScriptPanel.Running;
        DoActiveScriptPanelChanged(TScriptTabSheet(obj).ActiveScriptPanel);
      end
      else
      begin
        // Assuming a help page.
        CurrentPerspective := apHelp;
      end;
    end;
  end
  else
    if PreviousSelectedObj is TScriptTabSheet then
    begin
      // Switching from a script page.
      // Now see where we go to.
      if obj is TRSTabSheet then
      begin
        // The new page is a result set. Restore SQL area if it was maximized.
        if SQLEditorMaximized then
        begin
          SQLEditMaximizedPnl.Height := ApplicationDM.QBOptions.SQLEditMaximizedRSTabSheetHeight;
          SQLEditMaximizedPnl.Show;
        end;

        CurrentPerspective := apResultSet;
        FActiveScriptTabSheet := nil;
        ActivePanelChanged(nil, TRSTabSheet(obj).ActiveRSPanel);

        QueryIsRunning := TRSTabSheet(obj).ActiveRSPanel.MySQLRS.QueryExecuting;
        Grid := TRSTabSheet(obj).ActiveRSPanel.RSGrid;
        RSGridFocusChanged(Grid, Grid.FocusedNode, Grid.FocusedColumn);
      end
      else
      begin
        if obj is TScriptTabSheet then
        begin
          // New page is again a script page.
          DoActiveScriptPanelChanged(TScriptTabSheet(obj).ActiveScriptPanel);
          CurrentPerspective := apScript;
          QueryIsRunning := TScriptTabSheet(obj).ActiveScriptPanel.Running;
        end
        else
        begin
          // Assuming a help page.
          FActiveScriptTabSheet := nil;
          CurrentPerspective := apHelp;
        end;
      end;
    end
    else
    begin
      // Old page must be a manual (help) page.
      // Now see where we go to.
      if obj is TRSTabSheet then
      begin
        // The new page is a result set. Restore SQL area if it was maximized.
        if SQLEditorMaximized then
        begin
          SQLEditMaximizedPnl.Height := ApplicationDM.QBOptions.SQLEditMaximizedRSTabSheetHeight;
          SQLEditMaximizedPnl.Show;
        end;

        CurrentPerspective := apResultSet;
        ActivePanelChanged(nil, TRSTabSheet(obj).ActiveRSPanel);

        Grid := TRSTabSheet(obj).ActiveRSPanel.RSGrid;
        RSGridFocusChanged(Grid, Grid.FocusedNode, Grid.FocusedColumn);
      end
      else
      begin
        if obj is TScriptTabSheet then
        begin
          // New page is a script page.
          DoActiveScriptPanelChanged(TScriptTabSheet(obj).ActiveScriptPanel);
          CurrentPerspective := apScript;
        end
        else
        begin
          // From help to help? Can't be currently, we only have one help page. But who knows...?
        end;
      end;
    end;

  Toolbars.ItemsEnabled['QueryStopBtn'] := QueryIsRunning;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoRequestNewPage(Sender: TObject);

begin
  AddNewRSTabMIClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoSetActiveRSTab(TabIndex: Integer);

begin
  MainTabHeaderFrame.SelectedTab := TabIndex;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ActivePanelChanging(Sender: TObject);

var
  Panel: TMySQLRSPanel;

begin
  if (Sender is TMySQLRSPanel) and
    (TRSTabSheet(TMySQLRSPanel(Sender).Parent).InitControls) then
      Exit;

  // Keep the current editor text and caret position for later reuse.
  Panel := ActiveResultsetPanel;
  if Assigned(Panel) then
  begin
    Panel.LastEditorPosition := FSQLEditor.CaretXY;
    Panel.LastEditorContent := FSQLEditor.Text;
    Panel.LastEditorSelectionStart := FSQLEditor.BlockBegin;
    Panel.LastEditorSelectionEnd := FSQLEditor.BlockEnd;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ActivePanelChanged(PreviousSelectedPanel: TObject; SelectedPanel: TObject);

// Called after a new tab has been activated.

var
  ActiveRSPanel: TMySQLRSPanel;

begin
  if SelectedPanel is TMySQLRSPanel then
  begin
    if not TRSTabSheet(TMySQLRSPanel(SelectedPanel).Parent).InitControls then
    begin
      // Set new active controls.
      ActiveRSPanel := TMySQLRSPanel(SelectedPanel);
      FActiveRSTabSheet := TRSTabSheet(ActiveRSPanel.Parent);

      // Activate Stop button when query is executing and not already stopped.
      Toolbars.ItemsEnabled['QueryStopBtn'] := ActiveRSPanel.MySQLRS.QueryExecuting and not ActiveRSPanel.MySQLRS.StopQuery;

      // Refresh the parameter tree.
      DoRefreshParams(ActiveResultset);

      // Restore last text and caret position for that page.
      FIsLoading := True;
      try
        FSQLEditor.ClearAll(false);
        FSQLEditor.Text := ActiveRSPanel.LastEditorContent;
        FSQLEditor.CaretXY := ActiveRSPanel.LastEditorPosition;
        FSQLEditor.BlockBegin := ActiveRSPanel.LastEditorSelectionStart;
        FSQLEditor.BlockEnd := ActiveRSPanel.LastEditorSelectionEnd;
        FSQlEditor.Modified := False;
        PostMessage(Handle, WM_UPDATE_COMMAND, 0, 0);
      finally
        FIsLoading := False;
        DoQueryEditorChange;
      end;
    end;
  end
  else
  begin
    FActiveRSTabSheet := nil;

    RefreshQueryNavButtons;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQueryExecute(Sender: TObject);

begin
  // Only change QueryStopToolbarItem if the query of the current RS is executed.
  if Sender = ActiveResultset then
  begin
    Toolbars.ItemsEnabled['QueryStopBtn'] := True;

    BusyAnimate.Show;
    BusyAnimationStart := True;
    BusyAnimate.Play(1, 74, -1);

    if FQueryRunningCount = 0 then
      Screen.Cursor := crSQLWait;
    Inc(FQueryRunningCount);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQueryExecuted(Sender: TObject);

begin
  Toolbars.ItemsEnabled['QueryStopBtn'] := False;

  Dec(FQueryRunningCount);

  if FQueryRunningCount <= 0 then
  begin
    Screen.Cursor := crDefault;
    
    ActiveResultset.StopQuery := False;

    BusyAnimate.Hide;
    BusyAnimationStart := False;
    BusyAnimate.Stop;

    FQueryRunningCount := 0;
  end;

  RefreshQueryNavButtons;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQuerySuccess(Sender: TObject);

begin
  // Add a new history entry.
  if FHistory = nil then
    FHistory := myx_history_new;

  myx_history_add_entry(FHistory, '', MySQLConn.UserConnection.schema, Trim(FCurrentCommand), 200);
  RefreshHistoryTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQueryStopped(Sender: TObject);

begin
  // Add a new history entry.
  if FHistory = nil then
    FHistory := myx_history_new;

  myx_history_add_entry(FHistory, '', MySQLConn.UserConnection.schema, Trim(FCurrentCommand), 200);
  RefreshHistoryTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoQueryError(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BuildParamTV;

var
  NodeData: PParamRowData;

begin
  FGlobalParams.Text := ApplicationDM.QBOptions.GlobalParametersAsStrings;


  ParamVT.Clear;
  ParamVT.NodeDataSize := sizeof(ParamRowData);

  GlobalParamsNode := ParamVT.AddChild(nil);
  NodeData := ParamVT.GetNodeData(GlobalParamsNode);
  NodeData.param_index := -1;
  NodeData.params := nil;

  LocalParamsNode := ParamVT.AddChild(nil);
  NodeData := ParamVT.GetNodeData(LocalParamsNode);
  NodeData.param_index := -1;
  NodeData.params := nil;

  DynamicParamsNode := ParamVT.AddChild(nil);
  NodeData := ParamVT.GetNodeData(DynamicParamsNode);
  NodeData.param_index := -1;
  NodeData.params := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoRefreshParams(Sender: TObject);

var
  I: Integer;
  Node: PVirtualNode;
  NodeData: PParamRowData;
  Resultset: TMySQLRS;

begin
  Resultset := ActiveResultset;
  if (Resultset = Sender) then
  begin
    ParamVT.DeleteChildren(GlobalParamsNode);
    if Assigned(Resultset.GlobalParams) then
      for i := 0 to Resultset.GlobalParams.Count - 1 do
      begin
        Node := ParamVT.AddChild(GlobalParamsNode);
        NodeData := ParamVT.GetNodeData(Node);
        NodeData.param_index := i;
        NodeData.params := Resultset.GlobalParams;
        // ParamsSynCompletionProposal.ItemList.Add(Resultset.GlobalParams.Names[i]);
      end;
    ParamVT.Expanded[GlobalParamsNode] := True;

    ParamVT.DeleteChildren(LocalParamsNode);
    for i := 0 to Resultset.LocalParams.Count - 1 do
    begin
      Node := ParamVT.AddChild(LocalParamsNode);
      NodeData := ParamVT.GetNodeData(Node);
      NodeData.param_index := i;
      NodeData.params := Resultset.LocalParams;
      // ParamsSynCompletionProposal.ItemList.Add(Resultset.LocalParams.Names[i]);
    end;
    ParamVT.Expanded[LocalParamsNode] := True;

    ParamVT.DeleteChildren(DynamicParamsNode);
    if Assigned(Resultset.ParentRS) then
      for i := 0 to Resultset.ParentRS.DynamicParams.Count - 1 do
      begin
        Node := ParamVT.AddChild(DynamicParamsNode);
        NodeData := ParamVT.GetNodeData(Node);
        NodeData.param_index := i;
        NodeData.params := Resultset.ParentRS.DynamicParams;
        // ParamsSynCompletionProposal.ItemList.Add(Resultset.ParentRS.DynamicParams.Names[i]);
      end;
    ParamVT.Expanded[DynamicParamsNode] := True;
  end
  else
  begin
    ParamVT.DeleteChildren(GlobalParamsNode);
    ParamVT.ChildCount[GlobalParamsNode] := FGlobalParams.Count;
    Node := ParamVT.GetFirstChild(GlobalParamsNode);
    while Assigned(Node) do
    begin
      NodeData := ParamVT.GetNodeData(Node);
      NodeData.param_index := Node.Index;
      NodeData.params := FGlobalParams;

      Node := ParamVT.GetNextSibling(Node);
    end;

    ParamVT.Expanded[GlobalParamsNode] := True;
    ParamVT.DeleteChildren(LocalParamsNode);
    ParamVT.DeleteChildren(DynamicParamsNode);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: PParamRowData;

begin
  CellText := '';

  if (Node = GlobalParamsNode) and (Column = 0) then
    CellText := _('Global Params')
  else
    if (Node = LocalParamsNode) and (Column = 0) then
      CellText := _('Local Params')
    else
      if (Node = DynamicParamsNode) and (Column = 0) then
        CellText := _('Dynamic Params')
      else
      begin
        NodeData := ParamVT.GetNodeData(Node);
        if (NodeData <> nil) then
        begin
          if Assigned(NodeData.params) and (NodeData.params.Count > 0) then
          begin
            if (Column = 0) then
              CellText := NodeData.params.Names[NodeData.param_index]
            else
              CellText := NodeData.params.ValueFromIndex[NodeData.param_index];
          end;
        end;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  ImageIndex := -1;
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    if ((Node = GlobalParamsNode) or (Node = LocalParamsNode) or
      (Node = DynamicParamsNode)) and (Column = 0) then
      ImageIndex := Ord(ParamVT.Expanded[Node])
    else
      if (Column = 0) then
        ImageIndex := 2;

    Ghosted := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  NodeData: PParamRowData;
begin
  if (ssLeft in Shift) then
  begin
    if (Sender = ParamVT) then
    begin
      Node := ParamVT.GetNodeAt(X, Y);
      if (Node <> nil) then
      begin
        ParamVT.FocusedNode := Node;
        NodeData := ParamVT.GetNodeData(Node);

        if (NodeData <> nil) then
          if (NodeData.params <> nil) then
            ParamVT.BeginDrag(False, 5);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddNewRSTabMIClick(Sender: TObject);

begin
  FActiveRSTabSheet := AddNewRSTabSheet;
  if (ToolbarPnl.Visible) and (FSQLEditor.Visible) then
    FSQLEditor.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SplitResultsetTabVerticallyMIClick(Sender: TObject);

begin
  if Assigned(FActiveRSTabSheet) then
    FActiveRSTabSheet.AddRSPanel(MySQLConn, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SplitTabHorizontallyMIClick(Sender: TObject);

begin
  if Assigned(FActiveRSTabSheet) then
    FActiveRSTabSheet.AddRSPanel(MySQLConn);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddNewScriptTabMIClick(Sender: TObject);

begin
  FActiveScriptTabSheet := AddNewScriptTabSheet;
  ActiveScriptPanel.ScriptEditor.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SplitScriptTabHorizontallyMIClick(Sender: TObject);

begin
  if Assigned(FActiveScriptTabSheet) then
    FActiveScriptTabSheet.AddScriptPanel(MySQLConn, MYXCommonOptionProvider);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RemoveResultSetMIClick(Sender: TObject);

var
  Button: TToolbarItem;

begin
  if Assigned(ActiveResultsetPanel) then
    begin
      if FActiveRSTabSheet.RSPanels.Count = 1 then
        MainTabHeaderFrame.DeleteTab(MainTabHeaderFrame.SelectedTab)
      else
      begin
        FActiveRSTabSheet.DeleteRSPanel(ActiveResultsetPanel);

        // Switch off compare mode.
        ActiveResultsetPanel.RSGrid.CompareActive := False;
        Button := Toolbars.Items['ExplainBtn'];
        if Assigned(Button) then
          Button.Enabled := True;

      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RemoveTabsheetMIClick(Sender: TObject);

begin
  MainTabHeaderFrame.DeleteTab(MainTabHeaderFrame.SelectedTab);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RSGridPopupMenuPopup(Sender: TObject);

var
  Resultset: TMySQLRS;
  
begin
  Resultset := ActiveResultset;
  if Assigned(Resultset) then
    begin
      RemoveTabsheetMI.Enabled := MainTabHeaderFrame.TabCount > 1;
      RemoveResultSetMI.Enabled := FActiveRSTabSheet.RSPanels.Count > 1;
      AddRowMI.Enabled := Resultset.EditingAllowed;
      DeleteRowMI.Enabled := Resultset.EditingAllowed;
      ExportResultsetMI.Enabled := Resultset.RowCount > 0;
      CopyRowValuesMI.Enabled := Resultset.RowCount > 0;

      LoadFieldContentMI.Enabled := Resultset.EditingAllowed;
      SaveFieldContentMI.Enabled := Resultset.RowCount > 0;
      ClearFieldContentMI.Enabled := Resultset.EditingAllowed;
      ViewFieldinPopupEditorMI.Enabled := Resultset.RowCount > 0;
      EditFieldinPopupEditorMI.Enabled := Resultset.EditingAllowed;
      CopyFieldContentMI.Enabled := Resultset.RowCount > 0;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.LoadBookmarks;

var
  error: MYX_LIB_ERROR;
  PBookmarks: PMYX_BOOKMARKS;

begin
  StopSearch;

  if (WideFileExists(MYXCommonOptions.UserDataDir + 'mysqlqb_bookmark.xml')) then
  begin
    PBookmarks := myx_bookmarks_load(MYXCommonOptions.UserDataDir + 'mysqlqb_bookmark.xml', @error);
    if (error <> MYX_NO_ERROR) then
    begin
      SHowError(_('Error while loading bookmarks.'), MYXCommonOptions.UserDataDir + 'mysqlqb_bookmark.xml (error: %d)',
        [Ord(error)]);
      Bookmarks := TMYX_BOOKMARKS.create;
    end
    else
      try
        Bookmarks := TMYX_BOOKMARKS.create(PBookmarks);
      finally
        myx_bookmarks_free(PBookmarks);
      end;
  end
  else
    Bookmarks := TMYX_BOOKMARKS.create;

  BookmarkVT.Clear;
  BookmarkVT.NodeDataSize := sizeof(BookmarkNodeData);
  if (Bookmarks.bookmark_groups <> nil) then
    if (Bookmarks.bookmark_groups.Count > 0) then
      AddBookmarkgroupToVT(nil, Bookmarks.bookmark_groups)
    else
    begin
      Bookmarks.bookmark_groups.Add(TMYX_BOOKMARK_GROUP.create(_('Bookmarks'), 0));

      AddBookmarkgroupToVT(nil, Bookmarks.bookmark_groups);
    end;

  RefreshBookmarkMenu;
end;

procedure TQueryBrowserForm.LoadBookmarksMIClick(Sender: TObject);
begin
  LoadBookmarks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.StoreBookmarks;

var
  error: MYX_LIB_ERROR;

begin
  if (Bookmarks <> nil) then
  begin
    error := myx_bookmarks_store(MYXCommonOptions.UserDataDir + 'mysqlqb_bookmark.xml',
      Bookmarks.get_record_pointer);

    if (error <> MYX_NO_ERROR) then
      ShowError(_('Error while storing bookmarks.'), MYXCommonOptions.UserDataDir + 'mysqlqb_bookmark.xml (error: %d)',
        [Ord(error)]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddBookmarkgroupToVT(parentnode: PVirtualNode; groups: TMYX_BOOKMARK_GROUP_List);

var
  I, J: Integer;
  GroupNode,
  Node: PVirtualNode;
  NodeData: PBookmarkNodeData;
  
begin
  FSearchLock.Acquire;
  try
    for I := 0 to groups.Count - 1 do
    begin
      GroupNode := BookmarkVT.AddChild(parentnode);
      NodeData := BookmarkVT.GetNodeData(GroupNode);
      NodeData.group := groups[I];
      NodeData.bookmark := nil;

      if (groups[I].bookmark_groups <> nil) then
        AddBookmarkgroupToVT(GroupNode, groups[I].bookmark_groups);

      for J := 0 to groups[I].bookmarks.Count - 1 do
      begin
        Node := BookmarkVT.AddChild(GroupNode);
        NodeData := BookmarkVT.GetNodeData(Node);
        NodeData.group := groups[I];
        NodeData.bookmark := groups[I].bookmarks[J];
      end;

      if (parentnode = nil) then
        BookmarkVT.Expanded[GroupNode] := True;
    end;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PBookmarkNodeData;
  
begin
  NodeData := Sender.GetNodeData(Node);
  if (NodeData <> nil) then
    if (NodeData.bookmark = nil) then
    begin
      if (NodeData.group <> nil) then
        CellText := NodeData.group.caption;
    end
    else
      CellText := NodeData.bookmark.caption;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  NodeData: PBookmarkNodeData;

begin
  ImageIndex := -1;
  if (Kind = ikNormal) or (Kind = ikSelected) then
  begin
    NodeData := Sender.GetNodeData(Node);
    if (NodeData <> nil) then
      if (NodeData.bookmark = nil) then
        ImageIndex := Ord(Sender.Expanded[Node])
      else
        ImageIndex := 5;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var CellText: WideString);

var
  NodeData: PBookmarkNodeData;

begin
  LineBreakStyle := hlbForceMultiLine;

  NodeData := Sender.GetNodeData(Node);
  if (NodeData <> nil) then
    if (NodeData.bookmark <> nil) then
      CellText := NodeData.bookmark.sql;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  HitInfo: THitInfo;

begin
  FSearchLock.Acquire;
  try
    BookmarkVT.GetHitTestInfoAt(X, Y, False, HitInfo);
    case Button of
      mbLeft:
        if Assigned(HitInfo.HitNode) and (HitInfo.HitNode.Parent <> BookmarkVT.RootNode) then
          BookmarkVT.BeginDrag(False, 5);
      mbRight:
        if Assigned(HitInfo.HitNode) then
          BookmarkVT.FocusedNode := nil;
    end;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameSchemaTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
  
begin
  //when the user drags a SQL command, automatically select the bookmarks
  if (Source = FSQLEditor) then
    UpperTabHeaderFrame.SelectedTab := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

var
  NodeData: PBookmarkNodeData;
  
begin
  if (Source = Sender) and Assigned(Sender.DropTargetNode) then
  begin
    NodeData := Sender.GetNodeData(Sender.DropTargetNode);
    // Script nodes can only be accepted if we drop above or below them (not on them).
    Accept := ((NodeData.bookmark = nil) or (Mode in [dmAbove, dmBelow])) and
      ((Sender.DropTargetNode.Parent <> Sender.RootNode) or (Mode = dmOnNode));
  end
  else
    Accept := Source = FSQLEditor;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MoveInList(List: TList; Source, Target: TObject; InsertBefore: Boolean);

var
  IndexSource,
  IndexTarget: Integer;

begin
  IndexSource := List.IndexOf(Source);
  IndexTarget := List.IndexOf(Target);

  if (IndexSource > -1) and (IndexTarget > -1) then
  begin
    //Source below Target
    if (IndexSource > IndexTarget) then
    begin
      if (InsertBefore) then
        List.Move(IndexSource, IndexTarget)
      else
        List.Move(IndexSource, IndexTarget + 1);
    end
    else
    begin
      if (InsertBefore) then
        List.Move(IndexSource, IndexTarget - 1)
      else
        List.Move(IndexSource, IndexTarget);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddBookmark(Cmd: WideString);

var
  title: WideString;
  Node: PVirtualNode;
  Group: TMYX_BOOKMARK_GROUP;
  NodeData: PBookmarkNodeData;
  NewBookmark: TMYX_BOOKMARK;

begin
  if (Cmd <> '') then
  begin
    if (ShowModalEditDialog(_('Enter Bookmark Caption'),
      _('Please enter the caption for this bookmark'),
      myx_mtEdit, _('OK'#13#10'Cancel'), True,
      _('Caption:'), title) = 1) then
    begin
      FSearchLock.Acquire;
      try
        Node := BookmarkVT.GetFirst;
        NodeData := BookmarkVT.GetNodeData(Node);
        Group := NodeData.group;

        Node := BookmarkVT.InsertNode(Node, amAddChildLast);
        NodeData := BookmarkVT.GetNodeData(Node);
        NodeData.group := Group;

        NewBookmark := TMYX_BOOKMARK.create(title,
          Node.ChildCount,
          '', '', Cmd,
          MYX_QT_SELECT, 0, '', '', '');
        NodeData.bookmark := NewBookmark;

        Group.bookmarks.Add(NewBookmark);

        RefreshBookmarkMenu;
      finally
        FSearchLock.Release;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
  
var
  title: WideString;
  DropTargetNode, Node: PVirtualNode;
  NodeData, DropTargetNodeData: PBookmarkNodeData;
  Group, ParentGroup, SelectedParentGroup: TMYX_BOOKMARK_GROUP;
  NewBookmark: TMYX_BOOKMARK;
  InsertIndex: Integer;
  InsertMode: TVTNodeAttachMode;
  i, j: Integer;
  SelectedNodes: TNodeArray;
  
begin
  SelectedNodes := nil;

  if Source = FSQLEditor then
  begin
    if (Mode = dmOnNode) and (BookmarkVT.DropTargetNode <> nil) then
    begin
      NodeData := BookmarkVT.GetNodeData(BookmarkVT.DropTargetNode);

      if (NodeData.bookmark <> nil) then
      begin
        if (ShowModalDialog(_('Overwrite Bookmark'), _('Do you want to overwrite this bookmark?'), myx_mtConfirmation,
          _('Yes' + #13#10 + 'No')) = 1) then
        begin
          NodeData.bookmark.query_type := MYX_QT_SELECT;
          NodeData.bookmark.sql := FCurrentCommand;
          Exit;
        end;
      end;
    end;

    if (ShowModalEditDialog(_('Enter Bookmark Caption'), _('Please enter the caption for this bookmark'), myx_mtEdit,
      _('OK'#13#10'Cancel'), True, _('Caption:'), title) = 1) then
    begin
      DropTargetNode := BookmarkVT.DropTargetNode;
      if DropTargetNode = nil then
        DropTargetNode := BookmarkVT.GetFirst;

      NodeData := BookmarkVT.GetNodeData(DropTargetNode);
      Group := NodeData.group;

      // If the sql was dragged onto a Group-Node, insert at last pos.
      if (NodeData.bookmark = nil) then
      begin
        InsertIndex := Group.bookmarks.Count + 1;
        InsertMode := amAddChildLast;
      end
      else
      begin
        // Insert above or below.
        if (Mode = dmAbove) or (Mode = dmOnNode) then
        begin
          InsertIndex := NodeData.bookmark.pos;
          InsertMode := amInsertBefore;
        end
        else
        begin
          InsertIndex := NodeData.bookmark.pos + 1;
          InsertMode := amInsertAfter;
        end;
      end;

      Node := BookmarkVT.InsertNode(DropTargetNode, InsertMode);
      NodeData := BookmarkVT.GetNodeData(Node);
      NodeData.group := Group;

      NewBookmark := TMYX_BOOKMARK.create(title, InsertIndex, '', '', FCurrentCommand, MYX_QT_SELECT, 0, '', '', '');
      if InsertIndex = Group.bookmarks.Count + 1 then
        Group.bookmarks.Add(NewBookmark)
      else
        Group.bookmarks.Insert(InsertIndex - 1, NewBookmark);

      // Update positions.
      for i := 0 to Group.bookmarks.Count - 1 do
        Group.bookmarks[i].pos := i + 1;

      NodeData.bookmark := NewBookmark;
      BookmarkVT.Expanded[DropTargetNode] := True;

      RefreshBookmarkMenu;
    end;
  end
  else
    if (Source = BookmarkVT) and Assigned(BookmarkVT.DropTargetNode) then
    begin
      DropTargetNode := BookmarkVT.DropTargetNode;
      DropTargetNodeData := BookmarkVT.GetNodeData(DropTargetNode);

      case Mode of
        dmAbove:
          InsertMode := amInsertBefore;
        dmOnNode:
          InsertMode := amAddChildFirst;
        dmBelow:
          InsertMode := amInsertAfter;
      else
        InsertMode := amNoWhere;
      end;

      Group := DropTargetNodeData.group;
      SelectedNodes := BookmarkVT.GetSortedSelection(True);
      try
        for i := Length(SelectedNodes) - 1 downto 0 do
        begin
          if not BookmarkVT.HasAsParent(DropTargetNode, SelectedNodes[i]) and (DropTargetNode <> SelectedNodes[i]) then
          begin
            NodeData := BookmarkVT.GetNodeData(SelectedNodes[i]);
            SelectedParentGroup := PBookmarkNodeData(BookmarkVT.GetNodeData(SelectedNodes[i].Parent)).group;
            if DropTargetNode.Parent <> BookmarkVT.RootNode then
              ParentGroup := PBookmarkNodeData(BookmarkVT.GetNodeData(DropTargetNode.Parent)).group
            else
              ParentGroup := nil;

            if NodeData.bookmark = nil then
            begin
              // Node to be moved represents a folder (group).
              if (SelectedParentGroup <> ParentGroup) or (Mode = dmOnNode) then
              begin
                BookmarkVT.MoveTo(SelectedNodes[i], DropTargetNode, InsertMode, False);

                // Remove from old group.
                SelectedParentGroup.bookmark_groups.Extract(NodeData.group);
                Group.bookmark_groups.Add(NodeData.group);

                // Refresh positions.
                for j := 0 to Group.bookmark_groups.Count - 1 do
                  Group.bookmark_groups[j].pos := j + 1;
                for j := 0 to SelectedParentGroup.bookmark_groups.Count - 1 do
                  SelectedParentGroup.bookmark_groups[j].pos := j + 1;
              end
              else
              begin
                BookmarkVT.MoveTo(SelectedNodes[i], DropTargetNode, InsertMode, False);
                MoveInList(ParentGroup.bookmark_groups, NodeData.group, Group, (InsertMode = amInsertBefore));

                // Refresh positions.
                for j := 0 to NodeData.group.bookmark_groups.Count - 1 do
                  NodeData.group.bookmark_groups[j].pos := j + 1;
              end;
            end
            else
            begin
              // A script entry is being moved. Determine if the group changes in this process.
              if Mode = dmOnNode then
                ParentGroup := Group;
              if NodeData.Group <> ParentGroup then
              begin
                BookmarkVT.MoveTo(SelectedNodes[i], DropTargetNode, InsertMode, False);

               // Remove from old group.
                NodeData.group.bookmarks.Extract(NodeData.bookmark);
                Group.bookmarks.Add(NodeData.bookmark);
                NodeData.group := Group;

                // Refresh positions.
                for j := 0 to NodeData.group.bookmarks.Count - 1 do
                  NodeData.group.bookmarks[j].pos := j + 1;
              end
              else
              begin
                // Nodes moved within a single group.
                BookmarkVT.MoveTo(SelectedNodes[i], DropTargetNode, InsertMode, False);

                MoveInList(NodeData.group.bookmarks,
                  NodeData.bookmark, DropTargetNodeData.bookmark, (InsertMode = amInsertBefore));
              end;
            end;
            BookmarkVT.VisiblePath[SelectedNodes[i]] := True;
          end;
        end;

      //Refresh pos
        for i := 0 to Group.bookmarks.Count - 1 do
          Group.bookmarks[i].pos := i + 1;
      finally
        SelectedNodes := nil;
      end;

      RefreshBookmarkMenu;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkPopupMenuPopup(Sender: TObject);

begin
  DeleteBookmarkMI.Enabled := BookmarkVT.SelectedCount > 0;
  CreateBookmarkFolderMI.Enabled := BookmarkVT.SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteBookmarkNodeMIClick(Sender: TObject);

begin
  DeleteBookmarks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteBookmarks(AskBeforeDelete: Boolean);

var
  NodeData: PBookmarkNodeData;
  I: Integer;
  SelectedNodes: TNodeArray;

begin
  FSearchLock.Acquire;
  try
    SelectedNodes := BookmarkVT.GetSortedSelection(True);
    try
      for I := 0 to Length(SelectedNodes) - 1 do
      begin
        NodeData := BookmarkVT.GetNodeData(SelectedNodes[I]);
        if (NodeData.bookmark = nil) then
          DeleteBookmarkFolder(SelectedNodes[I], AskBeforeDelete)
        else
          DeleteBookmark(SelectedNodes[I], AskBeforeDelete);
      end;
    finally
      SelectedNodes := nil;
    end;

    RefreshBookmarkMenu;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteBookmark(Node: PVirtualNode; AskBeforeDelete: Boolean);

var
  I: Integer;
  NodeData: PBookmarkNodeData;

begin
  if Assigned(Node) then
  begin
    NodeData := BookmarkVT.GetNodeData(Node);

    if AskBeforeDelete then
      if ShowModalDialog(_('Confirm Deletion'), Format(_('Are you sure you want to delete the bookmark "%s"?'),
        [NodeData.bookmark.caption]), myx_mtConfirmation, _('Yes') + #13#10 + _('Cancel')) = 1 then
      begin
        if (NodeData.group.bookmarks.IndexOf(NodeData.bookmark) > -1) then
        begin
          FSearchLock.Acquire;
          try
            NodeData.group.bookmarks.Delete(NodeData.group.bookmarks.IndexOf(NodeData.bookmark));
            BookmarkVT.DeleteNode(Node);

            // Update positions
            for I := 0 to NodeData.group.bookmarks.Count - 1 do
              NodeData.group.bookmarks[I].pos := I + 1;
          finally
            FSearchLock.Release;
          end;
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteBookmarkFolder(Node: PVirtualNode; AskBeforeDelete: Boolean);

var
  ParentNodeData,
  NodeData: PBookmarkNodeData;

begin
  if (Node <> nil) and (Node <> BookmarkVT.GetFirst) then
  begin
    NodeData := BookmarkVT.GetNodeData(Node);
    ParentNodeData := BookmarkVT.GetNodeData(Node.Parent);

    if (AskBeforeDelete) then
      if (ShowModalDialog(_('Confirm Deletion'),
        Format(_('Are you sure you want to delete the bookmark folder "%s"?'),
        [NodeData.group.caption]), myx_mtError,
        _('Yes'#13#10'Cancel')) <> 1) then
        Exit;

    FSearchLock.Acquire;
    try
      if (ParentNodeData.group.bookmark_groups.IndexOf(NodeData.group) > -1) then
        ParentNodeData.group.bookmark_groups.Delete(ParentNodeData.group.bookmark_groups.IndexOf(NodeData.group));

      BookmarkVT.DeleteNode(Node);
    finally
      FSearchLock.Release;
    end;
  end
  else
    if Node = BookmarkVT.GetFirst then
      ShowError(_('Cannot delete Bookmark Folder'), _('You cannot delete the root folder.'), []);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key = VK_DELETE) then
  begin
    if (ssCtrl in Shift) or (ssShift in Shift) then
      DeleteBookmarks(False)
    else
      DeleteBookmarks(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CreateBookmarkFolderMIClick(Sender: TObject);

var
  title: WideString;
  ParentNode, Node, ChildNode: PVirtualNode;
  NodeData: PBookmarkNodeData;
  group: TMYX_BOOKMARK_GROUP;
  i: Integer;

begin
  FSearchLock.Acquire;
  try
    ParentNode := BookmarkVT.FocusedNode;

    if (ShowModalEditDialog(_('Enter Bookmark Folder Caption'), _('Please enter the caption for this bookmark folder.'),
      myx_mtEdit, _('OK'#13#10'Cancel'), True, _('Caption:'), title) = 1) then
    begin
      if Assigned(ParentNode) then
      begin
        ChildNode := ParentNode.FirstChild;
        if Assigned(ChildNode) then
        begin
          for i := 0 to ParentNode.ChildCount - 1 do
          begin
            NodeData := BookmarkVT.GetNodeData(ChildNode);
            if NodeData.bookmark = nil then
              ChildNode := ChildNode.NextSibling
            else
              Break;
          end;
        end;

        NodeData := BookmarkVT.GetNodeData(ParentNode);
        group := TMYX_BOOKMARK_GROUP.Create(title, NodeData.group.bookmark_groups.Count + 1);

        NodeData.group.bookmark_groups.Add(group);

        if ChildNode = nil then
          Node := BookmarkVT.InsertNode(ParentNode, amAddChildFirst)
        else
          Node := BookmarkVT.InsertNode(ChildNode, amInsertBefore);
        NodeData := BookmarkVT.GetNodeData(Node);
        NodeData.group := group;
        NodeData.bookmark := nil;

        BookmarkVT.Expanded[ParentNode] := True;
      end;
      RefreshBookmarkMenu;
    end
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.LoadHistory;

var
  error: MYX_LIB_ERROR;

begin
  StopSearch;

  if (WideFileExists(MYXCommonOptions.UserDataDir + 'mysqlqb_history.xml')) then
  begin
    FHistory := myx_history_load(MYXCommonOptions.UserDataDir + 'mysqlqb_history.xml', @error);
    if (error <> MYX_NO_ERROR) then
    begin
      ShowError(_('Error while loading history.'), MYXCommonOptions.UserDataDir + 'mysqlqb_history.xml (error: %d)',
        [Ord(error) ]);
      FHistory := myx_history_new;
    end;
  end
  else
    FHistory := myx_history_new;

  HistoryVT.NodeDataSize := SizeOf(HistoryNodeData);
  HistoryVT.OnFreeNode := DoOnFreeVTNode;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.StoreHistory;

var
  error: MYX_LIB_ERROR;

begin
  if (FHistory <> nil) then
  begin
    error := myx_history_store(MYXCommonOptions.UserDataDir + 'mysqlqb_history.xml',
      FHistory);
    if (error <> MYX_NO_ERROR) then
      ShowError(_('Error while storing history.'), MYXCommonOptions.UserDataDir + 'mysqlqb_history.xml (error: %d)',
        [Ord(error)]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RefreshHistoryTree;

var
  J, K, L: Integer;
  IntervalNode,
  EntriesNode: PVirtualNode;
  NodeData: PHistoryNodeData;
  HistoryInterval: PMYX_HISTORY_INTERVAL;
  HistoryCatalog: PMYX_HISTORY_CATALOG;
  HistorySchema: PMYX_HISTORY_SCHEMA;
  HistoryEntry: PPMYX_HISTORY_ENTRY;

begin
  FSearchLock.Acquire;
  try
    HistoryVT.Clear;

    if Assigned(FHistory) then
    begin
      HistoryVT.BeginUpdate;
      try
        if Assigned(FHistoryTree) then
          myx_history_free_tree(FHistoryTree);

        FHistoryTree := myx_history_get_tree(FHistory);

        HistoryVT.RootNodeCount := FHistoryTree.history_intervals_num;
        IntervalNode := HistoryVT.GetFirst;
        HistoryInterval := FHistoryTree.history_intervals;
        while Assigned(IntervalNode) do
        begin
          NodeData := HistoryVT.GetNodeData(IntervalNode);
          NodeData.NodeType := HISTORY_INTERVAL_TYPE;
          NodeData.Data := HistoryInterval;

          HistoryCatalog := HistoryInterval.catalogs;
          for J := 0 to HistoryInterval.catalogs_num - 1 do
          begin
            HistorySchema := HistoryCatalog.schemata;
            for K := 0 to HistoryCatalog.schemata_num - 1 do
            begin
              HistoryEntry := HistorySchema.entries;
              for L := 0 to HistorySchema.entries_num - 1 do
              begin
                if (HistoryEntry^.marked_deleted <> 1) then
                begin
                  EntriesNode := HistoryVT.AddChild(IntervalNode);
                  NodeData := HistoryVT.GetNodeData(EntriesNode);
                  NodeData.NodeType := HISTORY_ENTRY_TYPE;
                  NodeData.Data := HistoryEntry;
                  NodeData.NodeCaption := UTF8Decode(PPMYX_HISTORY_ENTRY(NodeData.Data)^.sql);
                end;
                Inc(HistoryEntry);
              end;
              Inc(HistorySchema);
            end;
            Inc(HistoryCatalog);
          end;

          IntervalNode := HistoryVT.GetNextSibling(IntervalNode);
          Inc(HistoryInterval);
        end;

        IntervalNode := HistoryVT.GetLastChild(nil);
        if Assigned(IntervalNode) then
          HistoryVT.Expanded[IntervalNode] := True;
      finally
        HistoryVT.EndUpdate;
      end;
      HistoryVT.OffsetY := -MaxInt;
    end;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
  
var
  NodeData: PHistoryNodeData;

begin
  CellText := '';

  NodeData := HistoryVT.GetNodeData(Node);
  if (NodeData <> nil) then
    if (NodeData.Data <> nil) then
    begin
      if (NodeData.NodeType = HISTORY_INTERVAL_TYPE) then
      begin
        case PMYX_HISTORY_INTERVAL(NodeData.Data).interval_type of
          MYX_HIT_TODAY:
            CellText := _('Today');
          MYX_HIT_MONDAY:
            CellText := _('Monday');
          MYX_HIT_TUESDAY:
            CellText := _('Tuesday');
          MYX_HIT_WEDNESDAY:
            CellText := _('Wednesday');
          MYX_HIT_THURSDAY:
            CellText := _('Thursday');
          MYX_HIT_FRIDAY:
            CellText := _('Friday');
          MYX_HIT_SATURDAY:
            CellText := _('Saturday');
          MYX_HIT_SUNDAY:
            CellText := _('Sunday');
          MYX_HIT_LAST_WEEK:
            CellText := _('Last Week');
          MYX_HIT_BEFORE_LAST_WEEK:
            CellText := _('Before Last Week');
          MYX_HIT_YESTERDAY:
            CellText := _('Yesterday');
        end;
      end
      else
        if (NodeData.NodeType = HISTORY_ENTRY_TYPE) then
        begin
          CellText := NodeData.NodeCaption;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryVTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  NodeData: PHistoryNodeData;

begin
  ImageIndex := -1;

  NodeData := HistoryVT.GetNodeData(Node);
  if (NodeData <> nil) and ((Kind = ikNormal) or (Kind = ikSelected)) then
  begin
    if (NodeData.NodeType = HISTORY_INTERVAL_TYPE) then
      ImageIndex := 6
    else
      if (NodeData.NodeType = HISTORY_ENTRY_TYPE) then
        ImageIndex := 7;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.LoadFunctions(filename: WideString): TMYX_SQL_FUNCTIONINDEX;

var
  error: MYX_LIB_ERROR;
  PFunctions: PMYX_SQL_FUNCTIONINDEX;

begin
  PFunctions := myx_load_sql_function_list(filename, @error);
  if (error <> MYX_NO_ERROR) then
    ShowError(_('Error while loading sql function index.'), filename + ' (error: %d)', [Ord(error)]);
  try
    Result := TMYX_SQL_FUNCTIONINDEX.Create(PFunctions);
  finally
    myx_free_sql_function_list(PFunctions);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BuildFunctionTree(Tree: TVirtualStringTree; Functions: TMYX_SQL_FUNCTIONINDEX);

  //----------------------------------------------------------------------------

  procedure LoadGroup(Parent: PVirtualNode; Group: TMYX_SQL_FUNCTIONGROUP);

  var
    I: Integer;
    GroupNode: PVirtualNode;
    NodeData: PFunctionNodeData;
    FuncNode: PVirtualNode;

  begin
    GroupNode := Tree.AddChild(Parent);
    NodeData := Tree.GetNodeData(GroupNode);
    NodeData.group := Group;
    NodeData.func := nil;

    for I := 0 to Group.Subgroups.Count - 1 do
      LoadGroup(GroupNode, Group.Subgroups[I]);

    for I := 0 to Group.Functions.Count - 1 do
    begin
      FuncNode := Tree.AddChild(GroupNode);
      NodeData := Tree.GetNodeData(FuncNode);
      NodeData.group := Group;
      NodeData.func := Group.functions[I];
    end;
  end;

  //----------------------------------------------------------------------------

var
  I: Integer;

begin
  Tree.NodeDataSize := sizeof(FunctionNodeData);

  Tree.BeginUpdate;
  try
    Tree.Clear;

    for I := 0 to Functions.groups.Count - 1 do
      LoadGroup(nil, Functions.groups[I]);
  finally
    Tree.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FunctionsVTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  NodeData: PFunctionNodeData;
begin
  CellText := '';

  NodeData := Sender.GetNodeData(Node);
  if (NodeData <> nil) then
  begin
    if (NodeData.func = nil) then
      CellText := NodeData.group.caption
    else
      CellText := NodeData.func.caption;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FunctionsVTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

var
  NodeData: PFunctionNodeData;

begin
  ImageIndex := -1;

  NodeData := Sender.GetNodeData(Node);
  if (NodeData <> nil) and ((Kind = ikNormal) or (Kind = ikSelected)) then
  begin
    if (NodeData.func = nil) then
      ImageIndex := Ord(Sender.Expanded[Node])
    else
      if (LowerTabHeaderFrame.SelectedTab = 0) then
        ImageIndex := 4
      else
        ImageIndex := 3;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FunctionsVTDblClick(Sender: TObject);

var
  NodeData: PFunctionNodeData;

begin
  if (Sender is TVirtualStringTree) and Assigned((TVirtualStringTree(Sender).FocusedNode)) then
  begin
    NodeData := TVirtualStringTree(Sender).GetNodeData(TVirtualStringTree(Sender).FocusedNode);
    if Assigned(NodeData.func) then
    begin
      if (LowerTabHeaderFrame.SelectedTab = 0) then
        ShowHelpTabSheet('mysqlqb_statements.html#' + NodeData.func.id)
      else
        if (LowerTabHeaderFrame.SelectedTab = 1) then
          ShowHelpTabSheet('mysqlqb_functions.html#' + NodeData.func.id);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowHelpTabSheet(url: WideString);

begin
  if (HelpTabSheetIndex = -1) then
  begin
    HelpPnl := TSectionPanel.Create(self);
    HelpPnl.Parent := TabsPnl;

    HelpPnl.Align := alClient;
    HelpPnl.BorderColor := $009C9B91;

    HelpWebBrowser := TEmbeddedWB.Create(HelpPnl);
    TWinControl(HelpWebBrowser).Name := 'HelpBrowser';
    TWinControl(HelpWebBrowser).Parent := HelpPnl;
    HelpWebBrowser.Align := alClient;
    HelpWebBrowser.Loaded;
    HelpWebBrowser.OnDocumentComplete := DoHelpDocCompleted;
    HelpWebBrowser.UserInterfaceOptions := [{SCROLL_NO, }NO3DBORDER];

    HelpTabSheetIndex := MainTabHeaderFrame.AddTabSheet(self, _('Inline Help'), 'tabsheet_icon_inlinehelp', HelpPnl,
      nil, True, False);

    if (MainTabHeaderFrame.TabCount > 1) and
      (not (MainTabHeaderFrame.Visible)) then
      MainTabHeaderFrame.Show;
  end
  else
    DoSetActiveRSTab(HelpTabSheetIndex);

  //Set ActiveRSTabSheet nil
  ActivePanelChanged(nil, nil);

  if Pos('http://', url) = 1 then
    HelpWebBrowser.Navigate(url)
  else
    HelpWebBrowser.Navigate('file://' + ExtractFilePath(Application.ExeName) + 'doc\' + url);

  CurrentPerspective := apHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoHelpDocCompleted(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);

begin
  try
    HelpWebBrowser.OleObject.Document.Body.Style.overflowX := 'hidden';
  except
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.WMHotKey(var Msg: TWMHotKey);

var
  lookup_word: WideString;
  aDwordVar: DWORD;
  S: array[0..255] of Char;
  CopySQLLang: MYX_Q_SQL_STRIPPED_CODE_LANGUAGE;
  CopySQLMode: MYX_Q_SQL_STRIPPED_COPY_MODE;

begin
  if (Msg.HotKey = IdCopySQLHotKey) then
  begin
    try
      TntClipboard.clear;
    except
    end;

    hOtherWin := GetForegroundWindow;
    GetWindowText(hOtherWin, S, 255);
    if (CompareText(Copy(S, 1, 4), 'zend') = 0) then
    begin
      CopySQLMode := MYX_QSSCM_KEYSTROKES;
      CopySQLLang := MYX_QSSCL_PHP;
    end
    else
      if (CompareText(Copy(S, 1, 11), 'weaverslave') = 0) then
      begin
        CopySQLMode := MYX_QSSCM_MESSAGE;
        CopySQLLang := MYX_QSSCL_PHP;
      end
      else
        if (Pos('Eclipse Platform', S) > 0) and (Copy(S, 1, 4) = 'Java') then
        begin
          CopySQLMode := MYX_QSSCM_KEYSTROKES;
          CopySQLLang := MYX_QSSCL_JAVA;
        end
        else
        begin
          CopySQLMode := MYX_QSSCM_KEYSTROKES;
          CopySQLLang := MYX_QSSCL_PHP;
        end;

    OtherThreadID := GetWindowThreadProcessID(hOtherWin, @aDwordvar);
    if (AttachThreadInput(GetCurrentThreadID, OtherThreadID, True)) then
    begin
      hFocusWin := GetFocus;
      if (hFocusWin <> 0) then
      begin
        GetWindowText(hFocusWin, S, 255);

        if (CopySQLMode = MYX_QSSCM_KEYSTROKES) then
        begin
          keybd_event(VK_CONTROL, 0, 0, 0);
          keybd_event(Ord('C'), 0, 0, 0);
          keybd_event(Ord('C'), 0, KEYEVENTF_KEYUP, 0);
          keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
          Sleep(100);
        end
        else
          if (CopySQLMode = MYX_QSSCM_MESSAGE) then
            SendMessage(hFocusWin, WM_COPY, 0, 0);

        AttachThreadInput(GetCurrentThreadID, OtherThreadID, False);

        Application.ProcessMessages;

        PSQLStripped := CaptureSQLFromClipboard(CopySQLLang,
          CopySQLMode);
      end;
    end;

    if (lookup_word <> '') then
    begin
      Application.Restore;
      Application.BringToFront;
      FSQLEditor.SetFocus;
    end;
  end
  else
    if (Msg.HotKey = IdPasteSQLHotKey) and (hOtherWin <> 0) then
    begin
      if (PSQLStripped <> nil) then
      begin
        TntClipboard.AsWideText := myx_reconstruct_embedded_sql(PSQLStripped, FCurrentCommand);
      end
      else
        TntClipboard.AsWideText := FSQLEditor.Text;

      SetForegroundWindow(hOtherWin);
      Windows.SetFocus(hFocusWin);

      if (PSQLStripped.copy_mode = MYX_QSSCM_KEYSTROKES) then
      begin
        Sleep(100);
        keybd_event(VK_CONTROL, 0, 0, 0);
        keybd_event(Ord('V'), 0, 0, 0);
        keybd_event(Ord('V'), 0, KEYEVENTF_KEYUP, 0);
        keybd_event(VK_CONTROL, 0, KEYEVENTF_KEYUP, 0);
        Sleep(100);
        Application.ProcessMessages;
      end
      else
        if (PSQLStripped.copy_mode = MYX_QSSCM_MESSAGE) then
          SendMessage(hFocusWin, WM_PASTE, 0, 0);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.WMTimer(var Msg: TWMTimer);

begin
  case Msg.TimerID of
    SearchTimer:
      begin
        KillTimer(Handle, SearchTimer);
        Application.CancelHint;
        case FSearchType of
          stBookmarks:
            SearchThread := TSearchThread.Create(SearchBookmarks);
          stHistory:
            SearchThread := TSearchThread.Create(SearchHistory);
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.WMUpdateCommand(var Msg: TMessage);

var
  Temp: TMsg;

begin
  // Sometimes two messages are posted for one change. Remove the superfluous one.
  PeekMessage(Temp, Handle, WM_UPDATE_COMMAND, WM_UPDATE_COMMAND, 1);
  UpdateCommandDisplay(FSQLEditor.CaretX, FSQLEditor.CaretY, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetClauseTypeFromCursor(Cur: TCursor): MYX_Q_CLAUSE_TYPE;

begin
  case Cur of
    crSQLSelect:
      Result := MYX_QCT_SELECT_CLAUSE;
    crSQLFrom:
      Result := MYX_QCT_FROM_CLAUSE;
    crSQLWhere:
      Result := MYX_QCT_WHERE_CLAUSE;
    crSQLGroup:
      Result := MYX_QCT_GROUP_CLAUSE;
    crSQLHaving:
      Result := MYX_QCT_HAVING_CLAUSE;
    crSQLOrder:
      Result := MYX_QCT_ORDER_CLAUSE;
    crSQLSet:
      Result := MYX_QCT_SET_CLAUSE;
  else
    Result := MYX_QCT_NO_CLAUSE;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.WMCursorChanged(var Msg: TMessage);

begin
  Toolbars.ItemsDown['MouseStdBtn']:= (Msg.WParam = crDefault);
  Toolbars.ItemsDown['MouseSelectBtn']:= (Msg.WParam = crSQLSelect);
  Toolbars.ItemsDown['MouseFromBtn']:= (Msg.WParam = crSQLFrom);
  Toolbars.ItemsDown['MouseWhereBtn']:= (Msg.WParam = crSQLWhere);
  Toolbars.ItemsDown['MouseGroupBtn']:= (Msg.WParam = crSQLGroup);
  Toolbars.ItemsDown['MouseHavingBtn']:= (Msg.WParam = crSQLHaving);
  Toolbars.ItemsDown['MouseOrderBtn']:= (Msg.WParam = crSQLOrder);
  Toolbars.ItemsDown['MouseSetBtn']:= (Msg.WParam = crSQLSet);

  SchemataFrame.CatalogVST.Cursor:=TCursor(Msg.WParam);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.WMDoInit(var Msg: TMessage);

var
  I: Integer;

begin
  Update;

  // If the StartTable is set, do select.
  if ApplicationDM.QBOptions.StartTable <> '' then
  begin
    FActiveRSTabSheet := AddNewRSTabSheet;
    FCurrentCommandStart.Y := -1;
    FCurrentCommandEnd.Y := -1;
    FSQLEditor.Text := 'SELECT * FROM ' + ApplicationDM.QBOptions.StartTable;
    HandleEditorCommand(edExecute);
    ApplicationDM.QBOptions.StartTable := '';
  end
  else
    if (ApplicationDM.QBOptions.StartSQL1 <> '') then
    begin
      FActiveRSTabSheet := AddNewRSTabSheet;

      FCurrentCommandStart.Y := -1;
      FCurrentCommandEnd.Y := -1;
      FSQLEditor.Text := ApplicationDM.QBOptions.StartSQL1;
      HandleEditorCommand(edExecute);

      ApplicationDM.QBOptions.StartSQL1 := '';

      if (ApplicationDM.QBOptions.StartSQL2 <> '') then
      begin
        I := 0;
        while (FActiveRSTabSheet.ActiveRSPanel.MySQLRS.QueryExecuting and (I < 50)) do
        begin
          Application.ProcessMessages;
          Sleep(100);
          inc(i);
        end;
              
        SplitTabHorizontallyMIClick(self);

        FCurrentCommandStart.Y := -1;
        FCurrentCommandEnd.Y := -1;

        FSQLEditor.Text := ApplicationDM.QBOptions.StartSQL2;
        HandleEditorCommand(edExecute);

        ApplicationDM.QBOptions.StartSQL2 := '';

        if (ApplicationDM.QBOptions.StartSQL3 <> '') then
        begin
          I := 0;
          while (FActiveRSTabSheet.ActiveRSPanel.MySQLRS.QueryExecuting and (I < 50)) do
          begin
            Application.ProcessMessages;
            Sleep(100);
            inc(i);
          end;

          SplitTabHorizontallyMIClick(self);

          FCurrentCommandStart.Y := -1;
          FCurrentCommandEnd.Y := -1;

          FSQLEditor.Text := ApplicationDM.QBOptions.StartSQL3;
          HandleEditorCommand(edExecute);

          ApplicationDM.QBOptions.StartSQL3 := '';
        end;
      end;
    end
    else
      // Otherwise display normal result set tabsheet.
      if not ApplicationDM.HasScripts then
        FActiveRSTabSheet := AddNewRSTabSheet;

  if Assigned(FActiveRSTabSheet) then
    DoRefreshParams(ActiveResultset);

  if MySQLConn.Connected then
  begin
    SchemataFrame.ExpandSchemaOnLaunch := MySQLConn.UserConnection.schema;
    SchemataFrame.FillSchemaTree;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BusyAnimateStop(Sender: TObject);

begin
  {if(BusyAnimationStart)then
  begin
    BusyAnimationStart:=False;
    BusyAnimate.Play(2, 10, -1);
  end;}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteRowMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    ActiveResultsetPanel.RSGrid.DeleteSelectedRows;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ClearQueryEditorMIClick(Sender: TObject);

begin
  FSQLEditor.Content.Clear;
  DoQueryEditorChange;
  FSQLEditor.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTDblClick(Sender: TObject);

var
  BookmarkNodeData: PBookmarkNodeData;

begin
  if (BookmarkVT.FocusedNode <> nil) then
  begin
    BookmarkNodeData := BookmarkVT.GetNodeData(BookmarkVT.FocusedNode);

    if (BookmarkNodeData.bookmark <> nil) then
    begin
      if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
      begin
        if WideSameText(Trim(FCurrentCommand), Trim(BookmarkNodeData.bookmark.sql)) then
          HandleEditorCommand(edExecute)
        else
        begin
          CurrentCommand := BookmarkNodeData.bookmark.sql;
          if (FSQLEditor.CanFocus) then
            FSQLEditor.SetFocus;
        end;
      end
      else
        if (LastFocusedControl is TUniCodeConsole) then
        begin
          TUniCodeConsole(LastFocusedControl).ConsoleCommand := BookmarkNodeData.bookmark.sql;
          TUniCodeConsole(LastFocusedControl).ConsoleCommandSelStart := Length(BookmarkNodeData.bookmark.sql);

          if (TUniCodeConsole(LastFocusedControl).CanFocus) then
            TUniCodeConsole(LastFocusedControl).SetFocus;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryVTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var CellText: WideString);

var
  NodeData: PHistoryNodeData;

begin
  LineBreakStyle := hlbForceMultiLine;

  NodeData := Sender.GetNodeData(Node);
  if (NodeData.NodeType = HISTORY_ENTRY_TYPE) then
    CellText := NodeData.NodeCaption;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AnimStillImgClick(Sender: TObject);

begin
  BrowseWebPage('http://www.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryPopupMenuPopup(Sender: TObject);

begin
  SQLEditCutMI.Enabled := FSQLEditor.SelectionAvailable;
  SQLEditCopyMI.Enabled := SQLEditCutMI.Enabled;
  SQLEditPasteMI.Enabled := TntClipboard.HasFormat(CF_TEXT);
  ClearQueryEditorMI.Enabled := FSQLEditor.Content.Count > 0;
  SaveQueryAsMI.Enabled := ClearQueryEditorMI.Enabled;

  PasteClipboardContentasPHPcodeMI.Enabled :=
    (TntClipboard.HasFormat(CF_UNICODETEXT)) or (TntClipboard.HasFormat(CF_TEXT));
  PasteClipboardContentasJavaCodeMI.Enabled :=
    PasteClipboardContentasPHPcodeMI.Enabled;

  CopySQLasPHPcodeMI.Enabled := False;
  if (PSQLStripped <> nil) then
    if (PSQLStripped.code_lang = MYX_QSSCL_PHP) then
      CopySQLasPHPcodeMI.Enabled := True;

  CopySQLasJavaCodeMI.Enabled := False;
  if (PSQLStripped <> nil) then
    if (PSQLStripped.code_lang = MYX_QSSCL_PHP) then
      CopySQLasJavaCodeMI.Enabled := True;

  CopySQLasPHPcodeMI.Enabled := False;
  if ((FSQLEditor.Content.Count > 0) and
    (PSQLStripped <> nil)) then
    if (PSQLStripped.code_lang = MYX_QSSCL_PHP) then
      CopySQLasPHPcodeMI.Enabled := True;

  CopySQLasJavaCodeMI.Enabled := False;
  if ((FSQLEditor.Content.Count > 0) and
    (PSQLStripped <> nil)) then
    if (PSQLStripped.code_lang = MYX_QSSCL_JAVA) then
      CopySQLasJavaCodeMI.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddBookmarkMIClick(Sender: TObject);

begin
  if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
  begin
    if FSQLEditor.SelectionAvailable then
      AddBookmark(FSQLEditor.SelectedText)
    else
      AddBookmark(FCurrentCommand);
  end
  else
    if (LastFocusedControl is TUniCodeConsole) then
      AddBookmark(TUniCodeConsole(LastFocusedControl).ConsoleCommandExcludeDelim);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TDataMenuItem.Create(AOwner: TComponent; Data: Pointer);

begin
  inherited Create(AOwner);

  FData := Data;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RefreshBookmarkMenu;

var
  groups: TMYX_BOOKMARK_GROUP_List;

begin
  //delete old MIs
  while (QueryMI.Count > FQueryMenuInitialCount) do
  begin
    QueryMI.Delete(QueryMI.Count - 1);
  end;

  if (Bookmarks <> nil) then
    if (Bookmarks.bookmark_groups <> nil) then
    begin
      groups := Bookmarks.bookmark_groups;

      AddBookmarkGroup(QueryMI, groups, True);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddBookmarkGroup(ParentMenuItem: TTntMenuItem; groups: TMYX_BOOKMARK_GROUP_List;
  DoNotCreateGroup: Boolean);

var
  i, j: Integer;
  MenuItem: TTntMenuItem;
  DataMenuItem: TDataMenuItem;

begin
  if (groups <> nil) then
  begin
    for i := 0 to groups.Count - 1 do
    begin
      if (not (DoNotCreateGroup)) then
      begin
        MenuItem := TTntMenuItem.Create(self);
        MenuItem.Caption := groups[i].caption;
        MenuItem.ImageIndex := 0;
        ParentMenuItem.Add(MenuItem);
      end
      else
        MenuItem := ParentMenuItem;

      if (groups[i].bookmark_groups <> nil) then
        AddBookmarkGroup(MenuItem, groups[i].bookmark_groups);

      for j := 0 to groups[i].bookmarks.Count - 1 do
      begin
        DataMenuItem := TDataMenuItem.Create(self, groups[i].bookmarks[j]);
        DataMenuItem.Caption := groups[i].bookmarks[j].caption;
        DataMenuItem.OnClick := BookmarkMenuItemSelected;
        DataMenuItem.ImageIndex := 1;
        MenuItem.Add(DataMenuItem);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkMenuItemSelected(Sender: TObject);

begin
  if (Sender is TDataMenuItem) then
    if (TDataMenuItem(Sender).Data <> nil) then
      CurrentCommand := TMYX_BOOKMARK(TDataMenuItem(Sender).Data).sql;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarkVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PBookmarkNodeData;

begin
  FSearchLock.Acquire;
  try
    NodeData := BookmarkVT.GetNodeData(Node);
    if (NodeData <> nil) then
    begin
      if (NodeData.bookmark <> nil) then
      begin
        NodeData.bookmark.caption := NewText;
      end
      else
      begin
        NodeData.group.caption := NewText;
      end;

      RefreshBookmarkMenu;
    end;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

var
  col: TMYX_SCHEMA_TABLE_COLUMN;
  table: TMYX_SCHEMA_TABLE;
  schema: TMYX_SCHEMA;
  CursorPos: Integer;
  NodeData, ParentNodeData, ParentParentNodeData: ^TObject;
  HitInfo: THitInfo;
  TxtRect: TRect;
  
begin
  SchemataFrame.CatalogVSTMouseDown(Sender, Button, Shift, X, Y);

  //Keep code for expanding the nodes
  if (Sender.InheritsFrom(TBaseVirtualTree)) and (Button = mbLeft) then
  begin
    TBaseVirtualTree(Sender).GetHitTestInfoAt(X, Y, True, HitInfo);

    if (HitInfo.HitNode <> nil) then
    begin
      TxtRect := TBaseVirtualTree(Sender).GetDisplayRect(
        HitInfo.HitNode, -1, True);

      if (X < TxtRect.Left - 16) then
        Exit;

      NodeData := TBaseVirtualTree(Sender).GetNodeData(HitInfo.HitNode);

      if (NodeData <> nil) then
        if (NodeData^ <> nil) then
        begin
          if (NodeData^ is TMYX_SCHEMA_TABLE_COLUMN) and
            (SchemataFrame.CatalogVST.Cursor >= crSQLSelect) and
            (SchemataFrame.CatalogVST.Cursor <= crSQLSet) then
          begin
            ParentNodeData := TBaseVirtualTree(Sender).GetNodeData(HitInfo.HitNode.Parent);
            ParentParentNodeData := TBaseVirtualTree(Sender).GetNodeData(HitInfo.HitNode.Parent.Parent);

            col := TMYX_SCHEMA_TABLE_COLUMN(NodeData^);
            table := TMYX_SCHEMA_TABLE(ParentNodeData^);
            schema := TMYX_SCHEMA(ParentParentNodeData^);

            if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
            begin
              CurrentCommand := myx_query_add_column_to_sql(MySQLConn.MySQL, MySQLConn.DefaultSchema, '',
                schema.schema_name, table.table_name, col.column_name, FCurrentCommand,
                GetClauseTypeFromCursor(SchemataFrame.CatalogVST.Cursor), @CursorPos);

              FSQLEditor.CaretOffset(FCurrentCommandStart, CursorPos);
              if (FSQLEditor.CanFocus) then
                FSQLEditor.SetFocus;

              FSetFocusToQueryEditorAfterMouseUp := True;
            end
            else
              if (LastFocusedControl is TUniCodeConsole) then
              begin
                TUniCodeConsole(LastFocusedControl).ConsoleCommandExcludeDelim :=
                  myx_query_add_column_to_sql(MySQLConn.MySQL, MySQLConn.DefaultSchema, '', schema.schema_name,
                  table.table_name, col.column_name, TUniCodeConsole(LastFocusedControl).ConsoleCommandExcludeDelim,
                  GetClauseTypeFromCursor(SchemataFrame.CatalogVST.Cursor), @CursorPos);

                TUniCodeConsole(LastFocusedControl).ConsoleCommandSelStart := CursorPos + 1;

                if (TUniCodeConsole(LastFocusedControl).CanFocus) then
                  TUniCodeConsole(LastFocusedControl).SetFocus;
              end;
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
{var TableDragForm: TTableDragForm;
  DragResult: Integer;
  identifier: WideString;}
begin
  {if(DoDragStart)and(DraggedTable<>nil)and(DraggedSchema<>nil)and
    ((Abs(DragStartPoint.X-X)>5)or(Abs(DragStartPoint.Y-Y)>5))then
  begin
    DoDragStart:=False;
    SchemataFrame.CatalogVST.BeginDrag(True);
  end;}

{  if(Shift=[ssLeft])and(DoDragStart)and(DraggedTable<>nil)and(DraggedSchema<>nil)and
    ((Abs(DragStartPoint.X-X)>5)or(Abs(DragStartPoint.Y-Y)>5))then
  begin
    DoDragStart:=False;

    TableDragForm:=TTableDragForm.Create(nil);
    try
      TableDragForm.Left:=Mouse.CursorPos.X-TableDragForm.Width div 2;
      TableDragForm.Top:=Mouse.CursorPos.Y-TableDragForm.Height div 2;

      if(TableDragForm.Left+TableDragForm.Width>Screen.Width)then
        TableDragForm.Left:=Screen.Width-TableDragForm.Width;
      if(TableDragForm.Top<0)then
        TableDragForm.Top:=0;
      if(TableDragForm.Top+TableDragForm.Height>Screen.Height)then
        TableDragForm.Top:=Screen.Height-TableDragForm.Height;

      DragResult:=TableDragForm.ShowModal;

      if(CompareText(DraggedSchema.schema_name, MySQLConn.DefaultSchema)=0)then
      begin
        if(myx_identifier_needs_quotes(PWideChar(DraggedTable.table_name))=1)then
          identifier:='`'+DraggedTable.table_name+'`'
        else
          identifier:=DraggedTable.table_name;
      end
      else
      begin
        if(myx_identifier_needs_quotes(PWideChar(DraggedSchema.schema_name+'.'+DraggedTable.table_name))=1)then
          identifier:='`'+DraggedSchema.schema_name+'`.`'+DraggedTable.table_name+'`'
        else
          identifier:=DraggedSchema.schema_name+'.'+DraggedTable.table_name;
      end;

      //New commands
      if(DragResult=DragCommandSelect)then
      begin
        FSQLEditor.Text:='SELECT * FROM '+identifier+' '+
          Copy(DraggedTable.table_name, 1, 1);
      end
      else if(DragResult=DragCommandSelectAdd)then
      begin
        if(FSQLEditor.Text='')then
        begin
          FSQLEditor.Text:='SELECT * FROM '+identifier+' '+
            Copy(DraggedTable.table_name, 1, 1);
        end
        else
          ;
      end

      else if(DragResult=DragCommandUpdate)then
        FSQLEditor.Text:='UPDATE '+identifier+' SET '
      else if(DragResult=DragCommandInsert)then
        FSQLEditor.Text:='INSERT INTO '+identifier+'('
      else if(DragResult=DragCommandDelete)then
        FSQLEditor.Text:='DELETE FROM '+identifier+' ';

      if(DragResult<>DragCommandSelect)then
        if(SchemataFrame.CatalogVST.FocusedNode<>nil)then
          SchemataFrame.CatalogVST.Expanded[
            SchemataFrame.CatalogVST.FocusedNode]:=True;

      QueryEditorChange(self);

      FSQLEditor.SetCaretToEditorBottom;
      FSQLEditor.SetFocus;
    finally
      TableDragForm.Free;
    end;
  end;}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (FSetFocusToQueryEditorAfterMouseUp) then
  begin
    FSQLEditor.SetFocus;
    FSetFocusToQueryEditorAfterMouseUp := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameSchemaTreeViewPopupMenuPopup(
  Sender: TObject);
begin
  SchemataFrame.SchemaTreeViewPopupMenuPopup(Sender);

  ChangeDefaultSchemaMI.Enabled :=
    (SchemataFrame.GetCatalogVSTFocusedObject(TMYX_SCHEMA) <> nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoMakeDefaultSchema(Sender: TObject);
var
  schema: TMYX_SCHEMA;
begin
  schema := TMYX_SCHEMA(SchemataFrame.GetCatalogVSTFocusedObject(TMYX_SCHEMA));
  if (schema <> nil) then
    MySQLConn.DefaultSchema := schema.schema_name;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryVTDblClick(Sender: TObject);
var
  NodeData: PHistoryNodeData;
  SQL: WideString;
begin
  if (HistoryVT.FocusedNode <> nil) then
  begin
    NodeData := HistoryVT.GetNodeData(HistoryVT.FocusedNode);
    if (NodeData <> nil) then
      if (NodeData.NodeType = HISTORY_ENTRY_TYPE) then
      begin
        SQL := NodeData.NodeCaption;

        // this caused bug #11246
        //if (Copy(SQL, Length(SQL) - 2, 2) = #13#10) then
        //  SQL := Copy(SQL, 2, Length(SQL));

        if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
        begin
          if WideSameText(Trim(FCurrentCommand), Trim(SQL)) then
            HandleEditorCommand(edExecute)
          else
          begin
            CurrentCommand := SQL;

            DoQueryEditorChange;
            if (FSQLEditor.CanFocus) then
              FSQLEditor.SetFocus;
          end;
        end
        else
          if (LastFocusedControl is TUniCodeConsole) then
          begin
            TUniCodeConsole(LastFocusedControl).ConsoleCommand := SQL;
            TUniCodeConsole(LastFocusedControl).ConsoleCommandSelStart := Length(SQL);

            if (TUniCodeConsole(LastFocusedControl).CanFocus) then
              TUniCodeConsole(LastFocusedControl).SetFocus;
          end;

        Abort;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OptionsChanged(var Message: TMessage);

begin
  DoOptionsChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoOptionsChanged;

var
  ShowIt: Boolean;
  i: Integer;

begin
  ApplicationDM.QBOptions.Changed;

  if (ApplicationDM.QBOptions.HideTabWhenOneOpen) and
    (MainTabHeaderFrame.Visible) and
    (MainTabHeaderFrame.TabCount = 1) then
  begin
    MainTabHeaderFrame.Visible := False;
    MainTabHeaderFrame.TabSheets[0].TabPanel.Top := 0;
    MainTabHeaderFrame.TabSheets[0].TabPanel.Height :=
      MainTabHeaderFrame.TabSheets[0].TabPanel.Height +
      MainTabHeaderFrame.Height;
  end
  else
    if (not (ApplicationDM.QBOptions.HideTabWhenOneOpen)) and
      (not (MainTabHeaderFrame.Visible)) and
      (MainTabHeaderFrame.TabCount = 1) then
    begin
      MainTabHeaderFrame.Visible := True;
      MainTabHeaderFrame.TabSheets[0].TabPanel.Top :=
        MainTabHeaderFrame.Height;
      MainTabHeaderFrame.TabSheets[0].TabPanel.Height :=
        MainTabHeaderFrame.TabSheets[0].TabPanel.Height -
        MainTabHeaderFrame.Height;
    end;

  MaximizeQueryEditMI.Checked := ApplicationDM.QBOptions.MaximizeSQLEdit;
  OnlyTabsheetsMI.Checked := ApplicationDM.QBOptions.OnlyTabsheets;
  ShowSidebarMI.Enabled := not (OnlyTabsheetsMI.Checked);

  Toolbars.GroupVisible['Mouse']:= ApplicationDM.QBOptions.ShowMouseCursorToolbarGroup;

  AdvancedQueryToolbarPnl.Visible := (CurrentPerspective = apResultSet) and ApplicationDM.QBOptions.MaximizeSQLEdit;

  RefreshPerspective;

  DoQueryEditorChange;

  //Sidebar
  ShowSidebarMI.Checked := ApplicationDM.QBOptions.ShowQueryBrowserSidebar;

  if (ApplicationDM.QBOptions.OnlyTabsheets) then
    ShowIt := False
  else
    ShowIt := ApplicationDM.QBOptions.ShowQueryBrowserSidebar;

  if (SidebarPnl.Visible) and (not (ShowIt)) then
  begin
    SidebarSplitter.Visible := False;
    SidebarPnl.Visible := False;
  end
  else
    if (not (SidebarPnl.Visible)) and (ShowIt) then
    begin
      SidebarPnl.Visible := True;
      SidebarSplitter.Visible := True;
    end;

  if (Application.MainForm.Visible) then
  begin
    if (LastFocusedControl <> nil) then
    begin
      if (LastFocusedControl.CanFocus) then
        LastFocusedControl.SetFocus
    end
    else
      if (ApplicationDM.QBOptions.ShowQueryToolbar) then
      begin
        if (FSQLEditor.CanFocus) then
          FSQLEditor.SetFocus;
      end;
  end;

  for i := 0 to MainTabHeaderFrame.TabCount - 1 do
    if (MainTabHeaderFrame.TabSheets[i].TabPanel is TRSTabSheet) then
    begin
      TRSTabSheet(MainTabHeaderFrame.TabSheets[i].TabPanel
        ).ShowFieldOverlayImages :=
        ApplicationDM.QBOptions.ShowFieldOverlayImages;

      // Font changes are distributed via options listeners.
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoMainTabBeforePageDelete(Sender: TObject; var CanClose: Boolean);

var
  I: Integer;
  RSTabSheet: TRSTabSheet;
  ScriptTabSheet: TScriptTabSheet;
  Panel: TScriptPanel;
  Name: WideString;
  Resultset: TMySQLRS;

begin
  if (Sender <> nil) then
    if (Sender is TTab) then
    begin
      if (TTab(Sender).TabPanel is TRSTabSheet) then
      begin
        RSTabSheet := TRSTabSheet(TTab(Sender).TabPanel);
        for I := 0 to RSTabSheet.RSPanels.Count - 1 do
        begin
          Resultset := TMySQLRSPanel(RSTabSheet.RSPanels[i]).MySQLRS;

          // Force this result to stop its query if it has one running.
          // Does nothing if no query is active.
          Resultset.ForceStop;
          if Resultset.Edited then
          begin
            if ShowModalDialog(_('Discard Changes'),
               _('You have made changes to a resultset. Are you sure you want to discard your changes?'),
               myx_mtConfirmation, _('&Discard') + #13#10 + _('&Cancel')) = 2 then
              CanClose:=False;

            Break;
          end;
        end;
      end
      else
        if (TTab(Sender).TabPanel is TScriptTabSheet) then
        begin
          ScriptTabSheet := TTab(Sender).TabPanel as TScriptTabSheet;
          I := ScriptTabSheet.ScriptPanelCount;
          while I > 0 do
          begin
            Panel := ScriptTabSheet[I - 1];
            if Panel.ScriptEditor.Modified then
            begin
              Name := WideExtractFileName(Panel.ScriptEditor.FileName);
              if Name = '' then
                Name := '<unnamed>';
              case ShowModalDialog(_('Confirm'), Format(_('Save changes to %s?'), [Name]), myx_mtConfirmation,
                  _('&Yes') + #13#10 + _('&No') + #13#10 + _('&Cancel')) of
                1:
                  begin
                    if WideFileExists(Panel.ScriptEditor.FileName) then
                      Panel.ScriptEditor.SaveToFile(Panel.ScriptEditor.FileName, Panel.ScriptEditor.OriginalTextFormat)
                    else
                      CanClose := SaveScriptAs(Panel);
                  end;
                2:
                  begin
                    // Simply ignore the changes.
                  end;
                3:
                  begin
                    // User requested to stop closing the tab.
                    CanClose := False;
                    Break;
                  end;
              end;
            end
            else
              ScriptTabSheet.RemoveScriptPanel(Panel);
            Dec(I);
          end;
        end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoMainTabPageDelete(Sender: TObject);
var
  i, TabCount: Integer;
begin
  if (Sender <> nil) then
    if (Sender is TTab) then
    begin
      if (TTab(Sender).TabPanel = HelpPnl) then
      begin
        HelpTabSheetIndex := -1;

        // We have to free the WB here otherwise it is not deactivated properly and causes an AV.
        FreeAndNil(HelpWebBrowser);
      end
      else
        if (TTab(Sender).TabPanel is TRSTabSheet) then
        begin
        //Check how many TRSTabSheet are left
          TabCount := 0;
          for i := 0 to MainTabHeaderFrame.TabCount - 1 do
            if (MainTabHeaderFrame.TabSheets[i].Obj is TRSTabSheet) then
              inc(TabCount);

          // If this is the last, set active to nil.
          if TabCount = 1 then
            FActiveRSTabSheet := nil;

          TTab(Sender).TabPanel.Free;
          TTab(Sender).TabPanel := nil;
          TTab(Sender).Obj := nil;

        //Keep HelpTabSheetIndex in sync
          if (HelpTabSheetIndex > -1) then
            if (MainTabHeaderFrame.GetTabIndex(TTab(Sender)) > -1) and
              (MainTabHeaderFrame.GetTabIndex(TTab(Sender)) < HelpTabSheetIndex) then
              dec(HelpTabSheetIndex);
        end
        else
          if (TTab(Sender).TabPanel is TScriptTabSheet) then
          begin
            // Check how many TScriptTabSheet are left.
            TabCount := 0;
            for i := 0 to MainTabHeaderFrame.TabCount - 1 do
              if (MainTabHeaderFrame.TabSheets[i].Obj is TScriptTabSheet) then
                inc(TabCount);

            // If this is the last, set active to nil.
            if (TabCount = 1) then
              FActiveScriptTabSheet := nil;

            TTab(Sender).TabPanel.Free;
            TTab(Sender).TabPanel := nil;
            TTab(Sender).Obj := nil;
          end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.DoConfirm(Sender: TObject; Msg: WideString): Boolean;
begin
  Result := (ShowModalDialog(_('User Confirmation'),
    Msg, myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoDropTables(Sender: TObject; List: TWideStringList; Shift: TShiftState);

// Called when the user dropped a couple of tables on any of the result grids.
// If only one table is passed the we replace the content of the drop target otherwise we open new tabs.

var
  I: Integer;

begin
  Screen.Cursor := crHourGlass;
  try
    if (List.Count = 1) and (Shift - [ssShift] = []) then
    begin
      CurrentCommand := 'SELECT * FROM ' + List[0] + ';';
      HandleEditorCommand(edExecute);
    end
    else
      for I := 0 to List.Count - 1 do
      begin
        if (ssAlt in Shift) and (not (ssCtrl in Shift) or (I > 0)) then
          FActiveRSTabSheet.AddRSPanel(MySQLConn)
        else
          FActiveRSTabSheet := AddNewRSTabSheet;
        FCurrentCommandStart.Y := -1;
        FCurrentCommandEnd.Y := -1;
        FSQLEditor.Text := 'SELECT * FROM ' + List[I] + ';';
        HandleEditorCommand(edExecute);
      end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (Key = VK_Return) or (Key = Ord('E')) then
  begin
    if ssCtrl in Shift then
    begin
      Key := 0;

      if (ssShift in Shift) then
        QueryExecuteInNewTabClick(self)
      else
        QueryExecuteClick(Self);
    end
    else
      if [ssAlt] = Shift then
      begin
        Key := 0;
        QueryExecuteClick(Self);
        AdvanceToNextCommand;
      end
  end
  else
    if (Shift = []) and (Key = VK_ESCAPE) then
      FSQLEditor.Content.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Shift = [ssRight]) then
  begin
    QueryEditorRightMouseDown := True;
    QueryEditorDragStartPoint := Point(X, Y);
  end
  else
    QueryEditorRightMouseDown := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if (QueryEditorRightMouseDown) and
    ((Abs(QueryEditorDragStartPoint.X - X) > 5) or
    (Abs(QueryEditorDragStartPoint.Y - Y) > 5)) then
  begin
    QueryEditorRightMouseDown := False;

    FSQLEditor.BeginDrag(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Shift = [ssRight]) then
    QueryPopupMenu.Popup(X, Y);

  QueryEditorRightMouseDown := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: PVirtualNode;
  BookmarkNodeData: PBookmarkNodeData;
  HistoryNodeData: PHistoryNodeData;
  SchemaNodeData: ^TObject;
begin
  Accept := False;

  if (Source = SchemataFrame.CatalogVST) then
  begin
    LastFocusedControl := FSQLEditor;

    //Show Table-Drag-Panel
    if (GetKeyState(VK_MENU) < 0) or
      (not (ApplicationDM.QBOptions.ShowDragTargetWindowOnAltPressedOnly)) then
    begin
      //only if a Table is dragged
      SchemaNodeData := SchemataFrame.CatalogVST.GetNodeData(
        SchemataFrame.CatalogVST.FocusedNode);
      if (SchemaNodeData <> nil) and
        (SchemaNodeData^ <> nil) and
        ((SchemaNodeData^ is TMYX_SCHEMA_TABLE) or
          (SchemaNodeData^ is TMYX_SCHEMA_TABLE_COLUMN)) then
      begin
        Accept := True;

        if (TableDragForm = nil) then
        begin
          TableDragForm := TTableDragForm.Create(nil);
          TableDragForm.PlaceFormBelow(FSQLEditor, True);
          TableDragForm.DragTarget := SchemaNodeData^.ClassType;
          TableDragForm.ShowNoActivate;
        end;
      end
      else
        if (SchemaNodeData <> nil) and
          (SchemaNodeData^ <> nil) and
          (SchemaNodeData^ is TMYX_SCHEMA) then
          Accept := True;
    end;
  end
  else
    if (Source = ParamVT) then
      Accept := True
    else
      if (Source = BookmarkVT) then
      begin
        Node := BookmarkVT.FocusedNode;
        if (Node <> nil) then
        begin
          BookmarkNodeData := BookmarkVT.GetNodeData(Node);
          if (BookmarkNodeData.bookmark <> nil) then
            Accept := True;
        end;
      end
      else
        if (Source = HistoryVT) then
        begin
          Node := HistoryVT.FocusedNode;
          if (Node <> nil) then
          begin
            HistoryNodeData := HistoryVT.GetNodeData(Node);
            if (HistoryNodeData <> nil) then
              if (HistoryNodeData.NodeType = HISTORY_ENTRY_TYPE) then
                Accept := True;
          end;
        end
        else
          if (Source = SchemataFrame.CatalogVST) then
            Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  Node: PVirtualNode;
  NodeData: PParamRowData;
  BookmarkNodeData: PBookmarkNodeData;
  HistoryNodeData: PHistoryNodeData;
  param: WideString;
  NewSelStart: Integer;
begin
  if (Source = ParamVT) then
  begin
    Node := ParamVT.FocusedNode;
    if (Node <> nil) then
    begin
      NodeData := ParamVT.GetNodeData(Node);
      if (NodeData.params.Names[NodeData.param_index] <> '') then
      begin
        param := ':' + NodeData.params.Names[NodeData.param_index];
        FSQLEditor.SelectedText := param;

        if (FSQLEditor.CanFocus) then
          FSQLEditor.SetFocus;
      end;
    end;

    DoQueryEditorChange;
  end
  else
    if (Source = BookmarkVT) then
    begin
      Node := BookmarkVT.FocusedNode;
      if (Node <> nil) then
      begin
        BookmarkNodeData := BookmarkVT.GetNodeData(Node);
        if (BookmarkNodeData.bookmark <> nil) then
          CurrentCommand := BookmarkNodeData.bookmark.sql;
      end;

      DoQueryEditorChange;
    end
    else
      if (Source = HistoryVT) then
      begin
        Node := HistoryVT.FocusedNode;
        if (Node <> nil) then
        begin
          HistoryNodeData := HistoryVT.GetNodeData(Node);
          if (HistoryNodeData <> nil) then
            if (HistoryNodeData.NodeType = HISTORY_ENTRY_TYPE) then
              CurrentCommand := Trim(PPMYX_HISTORY_ENTRY(HistoryNodeData.Data)^.sql);
        end;

        DoQueryEditorChange;
      end
      else
        if (Source = SchemataFrame.CatalogVST) then
        begin
          NewSelStart := FSQLEditor.SelStart;
          CurrentCommand := BuildDragSQLCommand(MySQLConn, SchemataFrame.CatalogVST, FCurrentCommand, NewSelStart);

          // Setting the current command causes an update message for the query browser.
          // Handle this first before continuing.
          Application.ProcessMessages;
          FSQLEditor.CaretOffset(FCurrentCommandStart, NewSelStart);
          if (FSQLEditor.CanFocus) then
            FSQLEditor.SetFocus;
        end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
  
begin
  if (Source = FSQLEditor) and (UpperTabHeaderFrame.SelectedTab <> 1) then
    UpperTabHeaderFrame.SelectedTab := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryExecute;

var
  S: WideString;

begin
  // Catch: use database
  S := myx_parse_sqlmemo_command_use(FCurrentCommand);
  if S <> '' then
    MySQLConn.DefaultSchema := S
  else
    if not ActiveResultset.QueryExecuting then
    begin
      // Warn about multiple commands on one line.
      if not FCurrentCommandIsSingle and (FCurrentCommand <> '') then
        ShowModalDialog(_('Multiple commands'), _('There are more than one commands on this line. ') + #13#10 +
          _('Note that only the first complete command is executed.'), myx_mtWarning, _('OK'));

      ActiveResultset.ExecuteQuery(FCurrentCommand);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DefaultSchemaChanged(var Message: TMessage);

var
  i, j: Integer;
  error: Integer;
  AffectedRows: Int64;

begin
  SchemataFrame.DefaultSchema := MySQLConn.DefaultSchema;
  SchemataFrame.CatalogVST.Invalidate;

  for i := 0 to DockPnl.ComponentCount - 1 do
    if (DockPnl.Components[i] is TUniCodeConsole) then
      TUniCodeConsole(DockPnl.Components[i]).ConsolePrompt :=
        MySQLConn.DefaultSchema + '> ';

  //Make this schema the default schema for all tabsheets
  for i := 0 to MainTabHeaderFrame.TabCount - 1 do
  begin
    if (MainTabHeaderFrame.TabSheets[i].Obj is TRSTabSheet) then
      for j := 0 to TRSTabSheet(MainTabHeaderFrame.TabSheets[i].Obj).RSPanels.Count - 1 do
      begin
        with TMySQLRSPanel(TRSTabSheet(MainTabHeaderFrame.TabSheets[i].Obj).RSPanels[j]) do
        begin
          myx_query_execute_direct(MySQLRS.MySQLConn.MySQL, 'use `' + MySQLConn.DefaultSchema + '`', @error, @AffectedRows);
        end
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditCaretChange(Sender: TCustomUnicodeEdit; X, Y: Integer);

begin
  StatusBar.Panels[0].Text := Format(' %3d: %d', [Sender.CaretY + 1, Sender.CaretX + 1]);
  if Sender = FSQLEditor then
    PostMessage(Handle, WM_UPDATE_COMMAND, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoScriptTypeChange(Sender: TObject);

begin
  ScriptTypeCBox.ItemIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptExecuteClick(Sender: TObject);

var
  Options: TExecutionOptions;
  OptionProvider: IOptionProvider;

begin
  OptionProvider := ApplicationDM.QBOptions as IOptionProvider;
  Options := [exoStartOver, exoStopOnBreakpoints];
  if not OptionProvider.OptionAsBoolean['ForceScriptOnErrors'] then
    Include(Options, exoStopOnErrors);
  ScriptExecute(Options);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptContinueClick(Sender: TObject);

var
  Options: TExecutionOptions;
  OptionProvider: IOptionProvider;

begin
  OptionProvider := ApplicationDM.QBOptions as IOptionProvider;
  Options := [exoStartOver, exoStopOnBreakpoints];
  if not OptionProvider.OptionAsBoolean['ForceScriptOnErrors'] then
    Include(Options, exoStopOnErrors);
  ScriptExecute(Options);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptExecute(Options: TExecutionOptions);

begin
  Screen.Cursor := crAppStart;
  try
    Toolbars.ItemsEnabled['ScriptExecuteBtn'] := False;
    Toolbars.ItemsEnabled['ScriptSmallExecuteBtn'] := False;
    Toolbars.GroupsEnabled['ScriptDebug'] := False;
    Toolbars.GroupsEnabled['ScriptSmallDebug'] := False;

    Toolbars.ItemsEnabled['ScriptStopBtn'] := True;
    Toolbars.ItemsEnabled['ScriptSmallStopBtn'] := True;
    Toolbars.ItemsEnabled['ScriptPauseBtn'] := True;
    Toolbars.ItemsEnabled['ScriptSmallPauseBtn'] := True;

    DebugToolbarPnl.Refresh;
    ScriptToolbarPnl.Refresh;

    ScriptStopMI.Enabled := True;
    ScriptExecuteMI.Enabled := False;
    ScriptRunSelectionMI.Enabled := False;
    ScriptContinueMI.Enabled := False;
    ScriptStepOverMI.Enabled := False;

    ActiveScriptPanel.ExecuteScript(Options);

    SchemataFrame.ReloadSchemaTree;
  finally
    Screen.Cursor := crDefault;

    Toolbars.ItemsEnabled['ScriptExecuteBtn'] := True;
    Toolbars.ItemsEnabled['ScriptSmallExecuteBtn'] := True;

    Toolbars.GroupsEnabled['ScriptDebug'] := True;
    Toolbars.GroupsEnabled['ScriptSmallDebug'] := True;

    CheckStopBtnStatus;

    // If the user has switched the panels while executing a script there might no longer me an active panel.
    if Assigned(ActiveScriptPanel) then
    begin
      ScriptStopMI.Enabled := False;
      ScriptExecuteMI.Enabled := True;
      ScriptRunSelectionMI.Enabled := ActiveScriptPanel.ScriptEditor.SelectionAvailable;
      ScriptContinueMI.Enabled := True;
      ScriptStepOverMI.Enabled := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CheckStopBtnStatus;

var
  IsRunning: Boolean;

begin
  IsRunning := Assigned(ActiveScriptPanel) and ActiveScriptPanel.Running;
  Toolbars.ItemsEnabled['ScriptStopBtn'] := IsRunning;
  Toolbars.ItemsEnabled['ScriptSmallStopBtn'] := IsRunning;
  Toolbars.ItemsEnabled['ScriptPauseBtn'] := IsRunning;
  Toolbars.ItemsEnabled['ScriptSmallPauseBtn'] := IsRunning;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptStepIntoClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.StepInto;
  CheckStopBtnStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptStepOverClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.StepOver;
  CheckStopBtnStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptRunUntilReturnClick(Sender: TObject);

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptPauseClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.Pause;
  CheckStopBtnStatus;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptStopClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.Stop;

  CheckStopBtnStatus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptLoadClick(Sender: TObject);

begin
  OpenScript;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptSaveClick(Sender: TObject);

begin
  Save;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptSearchClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.DoDisplaySearch;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.GetAdapter(IID: TGUID): IInterface;

begin
  if IsEqualIID(IID, IOptionProvider) then
    Result := ApplicationDM.QBOptions
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HandleEditorCommand(Command: TEditorCommand);

// Called when a certain command with the SQL editor must be performed.

var
  Resultset: TMySQLRS;
  CommandEntry: PCommandEntry;
  LastPanel: TMySQLRSPanel;
  Button: TToolbarItem;
  LastCommand: WideString;

begin
  Application.ProcessMessages;
  Resultset := ActiveResultset;

  if Assigned(Resultset) then
  begin
    case Command of
      edExecute:
        begin
          with FSQLEditor do
            Resultset.CommandList.MakeTOC(Text, CaretXY, BlockBegin, BlockEnd);
          QueryExecute;
        end;
      edBack,
      edNext:
        begin
          if Command = edBack then
            CommandEntry := Resultset.CommandList.Back
          else
            CommandEntry := Resultset.CommandList.Next;

          if Assigned(CommandEntry) then
          begin
            with FSQLEditor do
            begin
              BeginUpdate;
              Text := CommandEntry.SQL;
              CaretXY := CommandEntry.Caret;
              BlockBegin := CommandEntry.SelectionStart;
              BlockEnd := CommandEntry.SelectionEnd;
              EndUpdate;
            end;

            PostMessage(Handle, WM_UPDATE_COMMAND, 0, 0);

            RefreshQueryNavButtons;
          end;
        end;
      edExplain:
        begin
          if FCurrentCommand = '' then
            ShowError(_('SQL Error'), _('Please enter a SELECT statment before you use the EXPLAIN function.'), []);

          if Assigned(FActiveRSTabSheet) and not ActiveResultset.QueryExecuting then
          begin
            LastCommand := FCurrentCommand;
            LastPanel := FActiveRSTabSheet.ActiveRSPanel;
            FActiveRSTabSheet.ActivateExplainPanel(MySQLConn);

            FCurrentCommandStart.Y := -1;
            FCurrentCommandEnd.Y := -1;
            if (CompareText(UpperCase(Copy(FSQLEditor.Text, 1, 7)), 'EXPLAIN')<>0) then
              FSQLEditor.Text := 'EXPLAIN EXTENDED ' + LastCommand + ';';
            HandleEditorCommand(edExecute);
            FActiveRSTabSheet.ActiveRSPanel := LastPanel;
          end;
        end;
      edCompare:
        begin
          // Deactivate EXPLAIN when comparing.
          if Assigned(FActiveRSTabSheet) then
          begin
            Button := Toolbars.Items['ExplainBtn'];
            if Assigned(Button) then
              Button.Enabled := False;

            FActiveRSTabSheet.ActivateCompareMode;
          end;
        end;
      edRefresh:
        if not ActiveResultset.QueryExecuting then
          ActiveResultset.Refresh;
    end;

    FSQLEditor.Modified := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.LineDeleted(Line: TUCELine);

// A line is about to be deleted. Check if it is part of the current statement and adjust the command fields
// accordingly.

begin
  if not (csDestroying in ComponentState) then
  begin
    if (FCurrentCommandStart.Y > -1) and (FCurrentCommandStart.Y <= Line.Index) and
      ((FCurrentCommandEnd.Y = -1) or (FCurrentCommandEnd.Y >= Line.Index)) then
    begin
      if (FCurrentCommandStart.Y = Line.Index) and ((FSQLEditor.Content.Count - 1) = Line.Index) then
      begin
        // Special case: current command starts at the last line in the editor and this line will be removed.
        FCurrentCommandStart.Y := -1;
        FCurrentCommandEnd.Y := -1;
      end
      else
        // Usual case: move the end index up by one if set.
        if FCurrentCommandEnd.Y > -1 then
        begin
          Dec(FCurrentCommandEnd.Y);
          if FCurrentCommandEnd.Y < FCurrentCommandStart.Y then
          begin
            // If the end is now before the start then start and end were on the same line and
            // the command will be entirely removed.
            FCurrentCommandStart.Y := -1;
            FCurrentCommandEnd.Y := -1;
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.PrepareQueryAreaHeaderBitmap;

var
  PNGImg: TPNGObject;
  R: TRect;
  Title: WideString;

begin
  PNGImg := LoadPNGImageFromResource('maximized_query_area_header');

  // Determine correct size of the query area header bar.
  with QueryAreaHeaderPBox, Canvas do
  begin
    R := ClientRect;
    Title := _('SQL Query Area');
    Windows.DrawTextW(Handle, PWideChar(Title), Length(Title), R, DT_SINGLELINE or DT_NOCLIP or DT_CALCRECT);
    Height := R.Bottom + 8;
  end;

  try
    FMaximizedQueryAreaHeaderBGBmp := TBitmap.Create;
    with FMaximizedQueryAreaHeaderBGBmp do
    begin
      FMaximizedQueryAreaHeaderBGBmp.Width := PNGImg.Width;
      FMaximizedQueryAreaHeaderBGBmp.Height := QueryAreaHeaderPBox.Height;

      Canvas.Brush.Color := clBtnFace;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(Rect(0, 0,
        FMaximizedQueryAreaHeaderBGBmp.Width,
        FMaximizedQueryAreaHeaderBGBmp.Height));

      PNGImg.Draw(Canvas, Rect(0,
        FMaximizedQueryAreaHeaderBGBmp.Height-PNGImg.Height,
        FMaximizedQueryAreaHeaderBGBmp.Width,
        FMaximizedQueryAreaHeaderBGBmp.Height));
    end;
  finally
    PNGImg.Free;
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.PrepareSearch(SearchType: TSearchType; Text: WideString);

// Prepares a search run by stopping any pending run, keeping the new search type and string for the search thread and
// starting a timer that is used to collect number of characters before the actual search starts.
// If the search string is empty then no new search run is prepared.

var
  Node: PVirtualNode;
  Tree: TVirtualStringTree;
  
begin
  StopSearch;

  if Length(Text) > 0 then
  begin
    FSearchType := SearchType;
    FSearchString := Text;
    SetTimer(Handle, SearchTimer, SearchTimeout, nil);
  end
  else
  begin
    // No search is active anymore. Make everything visible again in the treeview.
    case SearchType of
      stBookmarks:
        Tree := BookmarkVT;
      stHistory:
        Tree := HistoryVT;
    else
      // stSchema
      SchemataFrame.ResetCatalogDisplay;
      Tree := nil;
    end;

    if Assigned(Tree) then
    begin
      // Use the NoInit methods for walking the tree. This way only already existing nodes are traversed.
      // No need to make nodes visible that aren't loaded yet.
      Node := Tree.GetFirstNoInit;
      while Assigned(Node) do
      begin
        Tree.IsVisible[Node] := True;
        Node := Tree.GetNextNoInit(Node);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ProgressFinish;

begin
  with StatusBar do
  begin
    FreeAndNil(FProgressBar);
    Panels[2].Bevel := pbLowered;
    Panels[2].Text := '';
    SizeGrip := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ProgressInit(const Max: Integer);

var
  X: Integer;
  W: Integer;
  TextWidth: Integer;
  
begin
  with Statusbar do
  begin
    Panels[2].Bevel := pbNone;
    Panels[2].Text := _(' Progress') + ' (  0.00%): ';

    SizeGrip := False;

    FProgressBar := TProgressBar.Create(nil);
    FProgressBar.Parent := StatusBar;
    FProgressMax := Max;
    FProgressBar.Position := 0;
    FProgressBar.Max := 100; // Always go in percent.
    TextWidth := GetWideStringTextWidth(Canvas, Panels[2].Text);
    X := 4 + Panels[0].Width + Panels[1].Width + TextWidth + 8;

    W := Panels[2].Width - TextWidth - 8;

    FProgressBar.SetBounds(X, 4, X + W, Height - 6);
    FProgressBar.Show;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ProgressPosition(const Position: Integer);

var
  NewPosition: Integer;

begin
  NewPosition := Round(100 * Position / FProgressMax);

  if Assigned(FProgressBar) and (FProgressBar.Position <> NewPosition) then
  begin
    FProgressBar.Position := NewPosition;
    Statusbar.Panels[2].Text := Format('%s (%.2f%%): ', [_(' Progress'), 100 * Position / FProgressMax]);
    Statusbar.Update;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ProgressStep(const Amount: Integer);

begin
  FProgressBar.Position := FProgressBar.Position + Amount;
  FProgressBar.Update;
  Statusbar.Update;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SearchBookmarks(Worker: TThread);

// Searches the bookmark treeview for all nodes whose caption match the current search expression.
// This method is executed in the context of a background thread (Worker).

var
  Node: PVirtualNode;
  Text: WideString;

begin
  FSearchLock.Acquire;
  try
    Node := BookmarkVT.GetFirst;
    while Assigned(Node) and not TSearchThread(Worker).Terminated do
    begin
      BookmarkVTGetText(HistoryVT, Node, 0, ttNormal, Text);
      BookmarkVT.FullyVisible[Node] := myx_match_pattern(Text, FSearchString, 0, 1) <> 0;
      Node := BookmarkVT.GetNext(Node);
    end;
  finally
    FSearchLock.Release
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SearchHistory(Worker: TThread);

var
  Node: PVirtualNode;
  Text: WideString;

begin
  FSearchLock.Acquire;
  try
    Node := HistoryVT.GetFirst;
    while Assigned(Node) and not TSearchThread(Worker).Terminated do
    begin
      HistoryVTGetText(HistoryVT, Node, 0, ttNormal, Text);
      HistoryVT.FullyVisible[Node] := myx_match_pattern(Text, FSearchString, 0, 1) <> 0;
      Node := HistoryVT.GetNext(Node);
    end;
  finally
    FSearchLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowError(const Title, Text: WideString; Parameters: array of const);

var
  S: WideString;

begin
  S := WideFormat(Text, Parameters);
  ShowModalDialog(Title, S, myx_mtError, _('OK'));
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.GetActiveResultset: TMySQLRS;

var
  RSPanel: TMySQLRSPanel;

begin
  Result := nil;
  RSPanel := ActiveResultsetPanel;
  if Assigned(RSPanel) then
    Result := RSPanel.MySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.GetActiveResultsetPanel: TMySQLRSPanel;

begin
  Result := nil;
  if (FCurrentPerspective = apResultSet) and Assigned(FActiveRSTabSheet) then
    Result := FActiveRSTabSheet.ActiveRSPanel;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.GetActiveScriptPanel: TScriptPanel;

begin
  Result := nil;
  if Assigned(FActiveScriptTabSheet) then
    Result := FActiveScriptTabSheet.ActiveScriptPanel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SetCurrentCommand(const SQL: WideString);

// Sets the text of the SQL editor. Either it replaces everything if no command was entered yet or
// the current command is replaced.

begin
  if FCurrentCommandStart.Y = -1 then
  begin
    // No active command.
    if FSQLEditor.Content.Count = 0 then
      FSQLEditor.Text := SQL
    else
    begin
      // There are already commands but none is selected. Insert the new command at the caret position in this case.
      FSQLEditor.SelectedText := SQL;
    end;
  end
  else
  begin
    FSQLEditor.BlockBegin := FCurrentCommandStart;

    // Also delete the command delimiter if the new SQL command has already one.
    if SQL[Length(SQL)] = ';' then
      Inc(FCurrentCommandEnd.X); // Does not work if something else but a semicolon is used as delimiter.
    FSQLEditor.BlockEnd := FCurrentCommandEnd;
    FSQLEditor.SelectedText := SQL;
  end;
  FSQLEditor.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SetCurrentPerspective(CurrentPerspective: TActivePerspective);

begin
  if (FCurrentPerspective <> CurrentPerspective) then
  begin
    FCurrentPerspective := CurrentPerspective;

    RefreshPerspective;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SetSQLEditorMaximized(SQLEditorMaximized: Boolean);

begin
  if (SQLEditorMaximized <> FSQLEditorMaximized) then
  begin
    FSQLEditorMaximized := SQLEditorMaximized;

    //Keep Application Options in sync
    ApplicationDM.QBOptions.MaximizeSQLEdit := FSQLEditorMaximized;

    if (FSQLEditorMaximized) then
    begin
      {AdvancedQueryToolbarPnl.Top := -1;

      Toolbars.GroupVisible['AdvancedQuery/History'] := True;
      Toolbars.GroupVisible['AdvancedQuery/Query'] := True;

      ToolbarPnl.Height := AdvancedQueryToolbarPnl.Height;}

      FSQLEditor.Parent := SQLEditMaximizedPnl;
      FSQLEditor.Anchors := [];
      FSQLEditor.Align := alClient;
      FSQLEditor.GutterWidth := 60;

      FSQLEditor.Options := FSQLEditor.Options + [eoLineNumbers] - [eoScrollPastEOL];

      FSQLEditor.ScrollBars := ssBoth;

      if CurrentPerspective = apResultSet then
        SQLEditMaximizedPnl.Height := ApplicationDM.QBOptions.SQLEditMaximizedRSTabSheetHeight
      else
        SQLEditMaximizedPnl.Height := ApplicationDM.QBOptions.SQLEditMaximizedScriptTabSheetHeight;

      SQLEditMaximizedPnl.Visible :=
        (CurrentPerspective = apResultSet){ or
        (CurrentPerspective = apScript)};
    end
    else
    begin
      QueryToolbarPnl.Top := -1;

      SQLEditMaximizedPnl.Visible := False;

      FSQLEditor.Options := FSQLEditor.Options + [eoLineNumbers] - [eoScrollPastEOL];
      FSQLEditor.ScrollBars := ssVertical;
      FSQLEditor.Align := alNone;
      FSQLEditor.Parent := QueryToolbarPnl;
      FSQLEditor.GutterWidth := 0;

      // No refresh button
      FSQLEditor.Left := 144;

      FSQLEditor.Top := 3;

      //No refresh button
      //FSQLEditor.Width := QueryToolbarPnl.Width - (862 - 587);
      FSQLEditor.Width := QueryToolbarPnl.Width - (856 - 545);

      FSQLEditor.Anchors := [akLeft, akTop, akRight];

      {QueryToolbarPnl.Visible := (CurrentPerspective = apResultSet);

      Toolbars.GroupVisible['AdvancedQuery/History'] := False;
      Toolbars.GroupVisible['AdvancedQuery/Query'] := False;}

      DoQueryEditorChange;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RefreshPerspective;

var
  ActiveToolbarPnl: TTntPanel;

begin
  MainAreaPnl.Align := alNone;
  SideBarPnl.Align := alNone;
  try
    // Select the correct toolbar
    ActiveToolbarPnl := nil;

    if (Not(ApplicationDM.QBOptions.OnlyTabsheets)) then
    begin
      if (Not(ApplicationDM.QBOptions.MaximizeSQLEdit)) then
      begin
        if (FCurrentPerspective = apResultSet) then
          ActiveToolbarPnl := QueryToolbarPnl
        else if (FCurrentPerspective = apScript) then
          ActiveToolbarPnl := ScriptToolbarPnl;
      end
      else
      begin
        if (FCurrentPerspective = apResultSet) then
          ActiveToolbarPnl := AdvancedQueryToolbarPnl
        else if (FCurrentPerspective = apScript) then
          ActiveToolbarPnl := DebugToolbarPnl;
      end;
    end;

    // display the correct toolbar
    if (ActiveToolbarPnl <> QueryToolbarPnl) and
      (QueryToolbarPnl.Visible) then
      QueryToolbarPnl.Visible := False;

    if (ActiveToolbarPnl <> ScriptToolbarPnl) and
      (ScriptToolbarPnl.Visible) then
      ScriptToolbarPnl.Visible := False;

    if (ActiveToolbarPnl <> AdvancedQueryToolbarPnl) and
      (AdvancedQueryToolbarPnl.Visible) then
      AdvancedQueryToolbarPnl.Visible := False;

    if (ActiveToolbarPnl <> DebugToolbarPnl) and
      (DebugToolbarPnl.Visible) then
      DebugToolbarPnl.Visible := False;

    if (ActiveToolbarPnl <> nil) then
      ActiveToolbarPnl.Visible := True;

    if (Not(ApplicationDM.QBOptions.MaximizeSQLEdit)) and
      (FCurrentPerspective = apResultSet) then
    begin
      AdvancedQueryToolbarPnl.Visible := (CurrentPerspective = apResultSet) and
        (ApplicationDM.QBOptions.ShowAdvancedToolbar);
      QueryToolbarPnl.Top := -1;

      Toolbars.Toolbars['AdvancedQuery'].Groups['History'].Visible := False;
      Toolbars.Toolbars['AdvancedQuery'].Groups['Execute'].Visible := False;
    end
    else
      if (ApplicationDM.QBOptions.MaximizeSQLEdit) and
        (FCurrentPerspective = apResultSet) then
      begin
        Toolbars.Toolbars['AdvancedQuery'].Groups['History'].Visible := True;
        Toolbars.Toolbars['AdvancedQuery'].Groups['Execute'].Visible := True;
      end;

        
  finally
    SendMessage(ToolbarPnl.Handle, WM_SETREDRAW, 1, 0);
    ToolbarPnl.Invalidate;

    if (SideBarPnl.Align <> alRight) then
      SideBarPnl.Align := alRight;

    if (MainAreaPnl.Align <> alClient) then
      MainAreaPnl.Align := alClient;

    SidebarSplitter.Left := 0;

    // Note: Setting an update lock and invalidating the panels afterwards has been removed as it does not work here.
    //       The main area and the side bar don't repaint correctly if done that way. For the toolbars it works, though.
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.StopSearch;

// Stops any pending or in progress search.

begin
  KillTimer(Handle, SearchTimer);
  FSearchType := stNone;
  FSearchString := '';

  // The main search thread never dead-locks. The code to execute is in the QB form too, so this
  // can (and must) check if the thread must be stopped.
  FreeAndNil(SearchThread);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OpenScriptMIClick(Sender: TObject);

begin
  OpenScript;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OpenScript(Filename: WideString; OpenInNewTabSheet: Boolean);

const
  TextFormats: array[1..4] of TTextFormat = (tfANSI, tfUTF8, tfUTF16, tfANSI);

var
  OpenDialog: TTntOpenDialog;
  FilterIndex: Integer;

begin
  FilterIndex := 1;
  if Filename = '' then
  begin
    OpenDialog := TTntOpenDialog.Create(nil);
    try
      OpenDialog.Title := 'Open Script File ..';

      OpenDialog.Filter :=
        _('SQL Script File') + ' ANSI (*.sql)|*.sql|' +
        _('SQL Script File') + ' UTF-8 (*.sql)|*.sql|' +
        _('SQL Script File') + ' UTF-16 (*.sql)|*.sql|' +
        _('Any File') + ' ANSI assumed (*.*)|*.*';


      OpenDialog.FilterIndex := 2;  
      if (OpenDialog.Execute) then
      begin
        Filename := OpenDialog.FileName;
        FilterIndex := OpenDialog.FilterIndex;
      end;
    finally
      OpenDialog.Free;
    end;
  end;

  Update;
  
  if Filename <> '' then
  begin
    if OpenInNewTabSheet or (CurrentPerspective <> apScript) or (FActiveScriptTabSheet = nil) then
      FActiveScriptTabSheet := AddNewScriptTabSheet;

    if Assigned(ActiveScriptPanel) then
    begin
      FIsLoading := True;
      with ActiveScriptPanel do
      begin
        ScriptEditor.LoadFromFile(Filename, TextFormats[FilterIndex]);
        ScriptEditor.FileName := Filename;
        ScriptEditor.OriginalTextFormat := TextFormats[FilterIndex];
        ScriptEditor.ClearUndo;
      end;

      ActiveScriptPanel.ScriptEditor.Modified := False;

      UpdateActions;
      FIsLoading := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OpenQuery(Filename: WideString; OpenInNewTabSheet: Boolean);

const
  TextFormats: array[1..7] of TTextFormat = (tfANSI, tfUTF8, tfUTF16, tfANSI, tfUTF8, tfUTF16, tfANSI);

var
  OpenDialog: TTntOpenDialog;
  QueryLines: TTntStringList;
  FilterIndex: Integer;

begin
  FilterIndex := 1;
  if Filename = '' then
  begin
    OpenDialog := TTntOpenDialog.Create(nil);
    try
      OpenDialog.Title := 'Open Query From File ..';

      OpenDialog.Filter :=
        _('Query File') + ' ANSI (*.qbquery)|*.qbquery|' +
        _('Query File') + ' UTF-8 (*.qbquery)|*.qbquery|' +
        _('Query File') + ' UTF-16 (*.qbquery)|*.qbquery|' +
        _('SQL Script File') + ' ANSI (*.sql)|*.sql|' +
        _('SQL Script File') + ' UTF-8 (*.sql)|*.sql|' +
        _('SQL Script File') + ' UTF-16 (*.sql)|*.sql|' +
        _('Any File') + ' ANSI assumed (*.*)|*.*';

      OpenDialog.FilterIndex := 2;
      if OpenDialog.Execute then
      begin
        Filename := OpenDialog.FileName;
        FilterIndex := OpenDialog.FilterIndex;
      end;
    finally
      OpenDialog.Free;
    end;
  end;

  if Filename <> '' then
  begin
    if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
    begin
      if (CurrentPerspective <> apResultSet) or (FActiveRSTabSheet = nil) or (OpenInNewTabSheet) then
        FActiveRSTabSheet := AddNewRSTabSheet;

      FIsLoading := True;

      // Determine text encoding from selected filter entry.
      FCurrentCommandStart.Y := -1;
      FCurrentCommandEnd.Y := -1;
      FSQLEditor.LoadFromFile(Filename, TextFormats[FilterIndex]);
      FSQLEditor.ClearUndo;
      FIsLoading := False;
      
      DoQueryEditorChange;
      if (FSQLEditor.CanFocus) then
        FSQLEditor.SetFocus;
    end
    else
      if (LastFocusedControl is TUniCodeConsole) then
      begin
        QueryLines := TTntStringList.Create;
        try
          QueryLines.LoadFromFile(Filename);

          TUniCodeConsole(LastFocusedControl).ConsoleCommand := QueryLines.Text;
          TUniCodeConsole(LastFocusedControl).ConsoleCommandSelStart := Length(QueryLines.Text);
          TUniCodeConsole(LastFocusedControl).ClearUndo;

          if (TUniCodeConsole(LastFocusedControl).CanFocus) then
            TUniCodeConsole(LastFocusedControl).SetFocus;
        finally
          QueryLines.Free;
        end;
      end;

  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.Save;

begin
  if (CurrentPerspective = apResultSet) then
    SaveQueryAs
  else
    if (CurrentPerspective = apScript) then
      SaveScript;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveAs;

begin
  if (CurrentPerspective = apResultSet) then
    SaveQueryAs
  else
    if (CurrentPerspective = apScript) and Assigned(ActiveScriptPanel) then
      SaveScriptAs(ActiveScriptPanel);
end;

procedure TQueryBrowserForm.SaveBookmarksMIClick(Sender: TObject);
begin
  StoreBookmarks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveScript;

var
  ScriptPanel: TScriptPanel;

begin
  if Assigned(ActiveScriptPanel) then
  begin
    ScriptPanel := ActiveScriptPanel;
    if ScriptPanel.ScriptEditor.FileName = '' then
      SaveScriptAs(ScriptPanel)
    else
      with ScriptPanel do
      begin
        ScriptEditor.SaveToFile(ScriptEditor.Filename, ScriptEditor.OriginalTextFormat);
        ScriptEditor.Modified := False;

        UpdateActions;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.SaveScriptAs(ScriptPanel: TScriptPanel): Boolean;

// Asks the user for a new file name to store the script text of the given panel in.
// Returns True if the user selected a name and the script was saved otherwise False.

const
  TextFormats: array[1..4] of TTextFormat = (tfANSI, tfUTF8, tfUTF16, tfANSI);

var
  SaveDialog: TTntSaveDialog;

begin
  SaveDialog := TTntSaveDialog.Create(nil);
  try
    SaveDialog.Title := _('Save Script to File ...');
    SaveDialog.DefaultExt := 'sql';
    SaveDialog.Filter :=
      _('SQL Script File') + ' ANSI (*.sql)|*.sql|' +
      _('SQL Script File') + ' UTF-8 (*.sql)|*.sql|' +
      _('SQL Script File') + ' UTF-16 (*.sql)|*.sql|' +
      _('Any File') + ' ANSI assumed (*.*)|*.*';

    SaveDialog.FilterIndex := 2;
    Result := SaveDialog.Execute;
    if Result then
    begin
      ScriptPanel.ScriptEditor.Filename := SaveDialog.FileName;
      with ScriptPanel do
      begin
        try
          ScriptEditor.SaveToFile(ScriptEditor.Filename, TextFormats[SaveDialog.FilterIndex]);
          ScriptEditor.OriginalTextFormat := TextFormats[SaveDialog.FilterIndex];
          ScriptEditor.Modified := False;
        except
          on x: Exception do
          begin
            ShowModalDialog(_('Output Error'), _('The file could not be created.'),
              myx_mtError, _('Ok'));
          end;
        end;
        
        UpdateActions;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveQueryAs(Filename: WideString);

const
  TextFormats: array[1..7] of TTextFormat = (tfANSI, tfUTF8, tfUTF16, tfANSI, tfUTF8, tfUTF16, tfANSI);

var
  SaveDialog: TTntSaveDialog;
  FilterIndex: Integer;

begin
  if (FSQLEditor.Text <> '') then
  begin
    FilterIndex := 1;

    if (Filename = '') then
    begin
      SaveDialog := TTntSaveDialog.Create(nil);
      try
        SaveDialog.Title := _('Save Query to File ...');
        SaveDialog.Filter :=
          _('Query File') + ' ANSI (*.qbquery)|*.qbquery|' +
          _('Query File') + ' UTF-8 (*.qbquery)|*.qbquery|' +
          _('Query File') + ' UTF-16 (*.qbquery)|*.qbquery|' +
          _('SQL Script File') + ' ANSI (*.sql)|*.sql|' +
          _('SQL Script File') + ' UTF-8 (*.sql)|*.sql|' +
          _('SQL Script File') + ' UTF-16 (*.sql)|*.sql|' +
          _('Any File') + ' ANSI assumed (*.*)|*.*';

        SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
        SaveDialog.FilterIndex := 2;
        if SaveDialog.Execute then
        begin
          Filename := SaveDialog.FileName;
          FilterIndex := SaveDialog.FilterIndex;

          //Make sure we get the right file extension
          if (SaveDialog.FilterIndex < 3) then
            Filename := ChangeFileExt(Filename, '.qbquery')
          else
            if (SaveDialog.FilterIndex < 6) then
              Filename := ChangeFileExt(Filename, '.sql');
        end;
      finally
        SaveDialog.Free;
      end;
    end;

    if (Filename <> '') then
    begin
      try
        FSQLEditor.SaveToFile(Filename, TextFormats[FilterIndex]);
      except
        on x: Exception do
        begin
          ShowModalDialog(_('Output Error'), _('The file could not be created.'),
            myx_mtError, _('Ok'));
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveScriptMIClick(Sender: TObject);

begin
  SaveScript;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveScriptAsMIClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    SaveScriptAs(ActiveScriptPanel);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptMemoPopupMenuPopup(Sender: TObject);

begin
  SaveScriptMI.Enabled := False;
  if Assigned(ActiveScriptPanel) then
  begin
    if ActiveScriptPanel.ScriptEditor.FileName <> '' then
      SaveScriptMI.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OpenQueryMIClick(Sender: TObject);

begin
  FSQLEditor.SetFocus;
  OpenQuery;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveQueryAsMIClick(Sender: TObject);

begin
  SaveQueryAs;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowSidebarMIClick(Sender: TObject);

begin
  ApplicationDM.QBOptions.ShowQueryBrowserSidebar := not ShowSidebarMI.Checked;

  DoOptionsChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddResultsetExporterMenuItems(ParentMenuItem: TMenuItem);

var
  I: Integer;
  PMyxStringList: PMYX_STRINGLIST;
  MyxStringList: TMYX_STRINGLIST;
  PMyxTableExporter: PMYX_TABLE_EXPORTER;
  MenuItem: TTntMenuItem;

begin
  PMyxStringList := myx_get_table_export_formats;
  if (PMyxStringList <> nil) then
  begin
    try
      MyxStringList := TMYX_STRINGLIST.create(PMyxStringList);
      try
        for I := 0 to MyxStringList.strings.Count - 1 do
        begin
          PMyxTableExporter := myx_get_table_exporter(MyxStringList.strings[I]);
          if (PMyxTableExporter <> nil) then
          begin
            MenuItem := TTntMenuItem.Create(ParentMenuItem);
            MenuItem.Name := 'ResultsetExportMI' + IntToStr(I);
            MenuItem.Caption := Format(_('Export As %s File...'), [UTF8Decode(PMyxTableExporter.name)]);
            MenuItem.Tag := I;
            MenuItem.OnClick := ResultsetExportMIClick;

            ParentMenuItem.Add(MenuItem);
          end;
        end;
      finally
        MyxStringList.Free;
      end;
    finally
      myx_free_lib_stringlist(PMyxStringList);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ResultsetExportMIClick(Sender: TObject);

var
  I: Integer;
  PMyxStringList: PMYX_STRINGLIST;
  MyxStringList: TMYX_STRINGLIST;
  PMyxTableExporter: PMYX_TABLE_EXPORTER;

begin
  PMyxStringList := myx_get_table_export_formats;
  if (PMyxStringList <> nil) then
  begin
    try
      MyxStringList := TMYX_STRINGLIST.create(PMyxStringList);
      try
        for I := 0 to MyxStringList.strings.Count - 1 do
        begin
          PMyxTableExporter := myx_get_table_exporter(MyxStringList.strings[I]);
          if (TMenuItem(Sender).Tag = I) and
            (PMyxTableExporter <> nil) then
          begin
            SaveResultsetAs(PMyxTableExporter.name);
            break;
          end;
        end;
      finally
        MyxStringList.Free;
      end;
    finally
      myx_free_lib_stringlist(PMyxStringList);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveResultsetAs(ExportFormat: WideString; Filename: WideString);

var
  I: Integer;
  PMyxStringList: PMYX_STRINGLIST;
  MyxStringList: TMYX_STRINGLIST;
  PMyxTableExporter: PMYX_TABLE_EXPORTER;
  PMyxTableExporterInfo: PMYX_TABLE_EXPORTER_INFO;
  SaveDialog: TTntSaveDialog;
  Success, DoExport: Boolean;
  DetailQuery: WideString;

begin
  if Assigned(ActiveResultset) and (ActiveResultset.RowCount > 0) then
  begin
    //find Exporter by format name
    PMyxStringList := myx_get_table_export_formats;
    if (PMyxStringList <> nil) then
    begin
      try
        MyxStringList := TMYX_STRINGLIST.create(PMyxStringList);
        try
          for I := 0 to MyxStringList.strings.Count - 1 do
          begin
            //if Exporter is found
            if (CompareText(ExportFormat, MyxStringList.strings[I]) = 0) then
            begin
              PMyxTableExporter := myx_get_table_exporter(MyxStringList.strings[I]);
              if (PMyxTableExporter <> nil) then
              begin
                if (Filename = '') then
                begin
                  SaveDialog := TTntSaveDialog.Create(nil);
                  try
                    SaveDialog.Title := _('Save result set to file ...');

                    SaveDialog.DefaultExt := PMyxTableExporter.file_extension;

                    SaveDialog.Filter := PMyxTableExporter.file_description +
                      ' (*.' + PMyxTableExporter.file_extension + ')|*.' +
                      PMyxTableExporter.file_extension + '|' +
                      _('Any File') + ' (*.*)|*.*';

                    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];

                    if (SaveDialog.Execute) then
                      Filename := SaveDialog.FileName;
                  finally
                    SaveDialog.Free;
                  end;
                end;

                if (Filename <> '') then
                begin
                  DoExport := True;

                  {if FileExists(Filename) then
                    if (ShowModalDialog(_('Confirm Overwrite'),
                      _('Are you sure you want to overwrite the selected file?'),
                      myx_mtConfirmation, _('Yes') + #13#10 + _('Cancel')) = 2) then
                      DoExport := False;}

                  if (DoExport) then
                  begin
                    PMyxTableExporterInfo := myx_get_table_exporter_info(MyxStringList.strings[I]);
                    if (PMyxTableExporterInfo <> nil) then
                    begin
                      if ActiveResultset.ConnectedDetailRSCount > 0 then
                        DetailQuery := ActiveResultset.ConnectedDetailRS[0].SQL
                      else
                        DetailQuery := '';

                      try
                        Success := (myx_export_resultset(MySQLConn.MySQL, PMyxTableExporterInfo,
                          Filename, 'Query $QUERY$, $DATE$', ActiveResultset.ResultSet, DetailQuery) = 0);
                      finally
                        myx_free_table_exporter_info(PMyxTableExporterInfo);
                      end;

                      if Success then
                      begin
                        if (not (ApplicationDM.QBOptions.OpenExportedResultset)) then
                          ShowOptionalModalDialog(_('Resultset Exported Successfully.'),
                            Format(_('The resultset has been exported to %s successfully.'),
                            [Filename]), myx_mtInformation, _('OK'))
                        else
                          ShellExecuteW(Handle, 'OPEN', PWideChar(Filename), nil, nil, SW_SHOWNORMAL);
                      end
                      else
                        ShowError('Error', _('The file %s cannot be written.'), [Filename]);
                    end;
                  end;
                end;
              end;

              break;
            end;
          end;
        finally
          MyxStringList.Free;
        end;
      finally
        myx_free_lib_stringlist(PMyxStringList);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.OnlyTabsheetsMIClick(Sender: TObject);

begin
  ApplicationDM.QBOptions.OnlyTabsheets := not OnlyTabsheetsMI.Checked;

  DoOptionsChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ConnectionEstablished(var Message: TMessage);

begin
  MySQLConnUpdateState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ConnectionLost(var Message: TMessage);

begin
  MySQLConnUpdateState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MySQLConnUpdateState;

begin
  Toolbars.GroupsEnabled['Query'] := MySQLConn.Connected;
  Toolbars.GroupsEnabled['Execute'] := MySQLConn.Connected;
  Toolbars.ItemsEnabled['QueryStopBtn'] := False;

  Toolbars.GroupsEnabled['Debug'] := MySQLConn.Connected;
  Toolbars.ItemsEnabled['DebugStepIntoBtn'] := False;
  Toolbars.ItemsEnabled['DebugRunUntilReturnBtn'] := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryEditorEnter(Sender: TObject);

begin
  if (LastFocusedControl <> Sender) then
    if (Sender.InheritsFrom(TWinControl)) then
      LastFocusedControl := TWinControl(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

function TrimSQL(Sql: Widestring): Widestring;

begin
  Result := Sql;
  if Result <> '' then
  begin
    // Trim white spaces also before an eventual semicolon.
    Result := Trim(Result);
    if (Result <> '') and (Result[Length(Result)] = ';') then
    begin
      SetLength(Result, Length(Result) - 1);
      Result := TrimRight(Result);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTDblClick(Sender: TObject);

var
  NewSelStart: Integer;
  SQLText: WideString;
  Nodedata: ^TObject;
  Console: TUniCodeConsole;
  Position: TPoint;
  HitInfo: THitInfo;

begin
  if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
  begin
    Position := SchemataFrame.CatalogVST.ScreenToClient(Mouse.CursorPos);
    SchemataFrame.CatalogVST.GetHitTestInfoAt(Position.X, Position.Y, True, HitInfo);
    if hiOnItemLabel in HitInfo.HitPositions then
    begin
      Nodedata := SchemataFrame.CatalogVST.GetNodeData(SchemataFrame.CatalogVST.FocusedNode);

      if Assigned(Nodedata) and Assigned(Nodedata^) then
      begin
        if (Nodedata^ is TMYX_SCHEMA)then
        begin
          if MySQLConn.FetchingData then
            // Do not change the current schema if there is already data fetching in progress.
            // Ignore the entire double click event chain.
            Abort
          else
            MySQLConn.DefaultSchema := TMYX_SCHEMA(Nodedata^).schema_name;
          Exit;
        end;

        if (Nodedata^ is TMYX_SCHEMA_STORED_PROCEDURE)then
        begin
          // Create a new RS Tabsheet if necessary
          if (CurrentPerspective <> apResultSet) then
            FActiveRSTabSheet := AddNewRSTabSheet;

          if TMYX_SCHEMA_STORED_PROCEDURE(Nodedata^).sp_type=MSPT_PROCEDURE then
            CurrentCommand := Format('CALL %s()', [TMYX_SCHEMA(Nodedata^).schema_name])
          else
            CurrentCommand := Format('SELECT %s()', [TMYX_SCHEMA(Nodedata^).schema_name]);

          FSQLEditor.SetFocus;

          Exit;
        end;
      end;

      SQLText := BuildDragSQLCommand(MySQLConn, SchemataFrame.CatalogVST, FCurrentCommand, NewSelStart);
      if not WideSameText(TrimSQL(SQLText), TrimSQL(FCurrentCommand)) then
      begin
        // Create a new RS Tabsheet if necessary
        if (CurrentPerspective <> apResultSet) then
          FActiveRSTabSheet := AddNewRSTabSheet;

        CurrentCommand := SQLText;
        if FCurrentCommandStart.Y = -1 then
          FSQLEditor.CaretOffset(Point(0, FSQLEditor.CaretY), NewSelStart)
        else
          FSQLEditor.CaretOffset(FCurrentCommandStart, NewSelStart);
        if FSQLEditor.CanFocus then
          FSQLEditor.SetFocus;
      end
      else
        if SQLText <> '' then
        begin
          HandleEditorCommand(edExecute);
          if Assigned(ActiveResultsetPanel) and (ActiveResultsetPanel.RSGrid.CanFocus)then
            ActiveResultsetPanel.RSGrid.SetFocus;
        end;
    end
    else
      if (LastFocusedControl is TUniCodeConsole) then
      begin
        Console := LastFocusedControl as TUniCodeConsole;
        Console.ConsoleCommandExcludeDelim := BuildDragSQLCommand(MySQLConn, SchemataFrame.CatalogVST,
          Console.ConsoleCommandExcludeDelim, NewSelStart);
        Console.ConsoleCommandSelStart := NewSelStart;
        if (Console.CanFocus) then
          Console.SetFocus;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.GetDragResult: Integer;

var
  TableDragForm: TTableDragForm;

begin
  TableDragForm := TTableDragForm.Create(nil);
  try
    TableDragForm.Left := Mouse.CursorPos.X - TableDragForm.Width div 2;
    TableDragForm.Top := Mouse.CursorPos.Y - TableDragForm.Height div 2;

    if (TableDragForm.Left + TableDragForm.Width > Screen.Width) then
      TableDragForm.Left := Screen.Width - TableDragForm.Width;
    if (TableDragForm.Top < 0) then
      TableDragForm.Top := 0;
    if (TableDragForm.Top + TableDragForm.Height > Screen.Height) then
      TableDragForm.Top := Screen.Height - TableDragForm.Height;

    Result := TableDragForm.ShowModal;
  finally
    TableDragForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.PasteClipboardContentasPHPcodeMIClick(Sender: TObject);

begin
  LastFocusedControl := FSQLEditor;
  FSQLEditor.SetFocus;

  PSQLStripped := CaptureSQLFromClipboard(MYX_QSSCL_PHP, MYX_QSSCM_QB_MENU);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.PasteClipboardContentasJavaCodeMIClick(Sender: TObject);

begin
  LastFocusedControl := FSQLEditor;
  FSQLEditor.SetFocus;

  PSQLStripped := CaptureSQLFromClipboard(MYX_QSSCL_JAVA, MYX_QSSCM_QB_MENU);
end;

//----------------------------------------------------------------------------------------------------------------------

function TQueryBrowserForm.CaptureSQLFromClipboard(CopySQLLang: MYX_Q_SQL_STRIPPED_CODE_LANGUAGE;
  CopySQLMode: MYX_Q_SQL_STRIPPED_COPY_MODE): PMYX_Q_SQL_STRIPPED;

var
  Data: THandle;
  CapturedSQL: WideString;
  Params: TMYX_STRINGLIST;
  I: Integer;
  Resultset: TMySQLRS;

begin
  CapturedSQL := '';

  try
    if (TntClipboard.HasFormat(CF_UNICODETEXT)) then
    begin
      Data := TntClipboard.GetAsHandle(CF_UNICODETEXT);

      try
        if (Data <> 0) then
          CapturedSQL := PWideChar(GlobalLock(Data));

      finally
        if (Data <> 0) then
          GlobalUnlock(Data);
      end;
    end
    else
      if (TntClipboard.HasFormat(CF_TEXT)) then
        CapturedSQL := TntClipboard.AsText;
  except
  end;

  if (CapturedSQL <> '') then
  begin
    Result := myx_strip_embedded_sql(CapturedSQL, CopySQLLang, CopySQLMode);

    if (Result.query_stripped <> '') then
    begin
      if (LastFocusedControl = nil) or (LastFocusedControl = FSQLEditor) then
      begin
        CurrentCommand := Result.query_stripped;
        DoQueryEditorChange;
      end
      else
        if (LastFocusedControl is TUniCodeConsole) then
        begin
          TUniCodeConsole(LastFocusedControl).ConsoleCommand := Result.query_stripped;
          TUniCodeConsole(LastFocusedControl).ConsoleCommandSelStart :=
            Length(Result.query_stripped);
        end;

      Resultset := ActiveResultset;
      if Assigned(Resultset) then
      begin
        Resultset.LocalParams.Clear;
        Params := TMYX_STRINGLIST.create(myx_get_params_from_stripped_query(Result));
        try
          for I := 0 to Params.strings.Count-1 do
            if (FGlobalParams.IndexOfName(Params.strings[I]) = -1) then
              Resultset.LocalParams.Add(Params.strings[I] + '=' + 'NULL');

            DoRefreshParams(ActiveResultset);
        finally
          Params.Free;
        end;
      end;
    end;

    LowerTabHeaderFrame.SelectedTab := LowerTabHeaderFrame.TabIndexFromPanel[ParamPnl];
  end
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CopySQLasPHPcodeMIClick(Sender: TObject);

begin
  TntClipboard.AsText := myx_reconstruct_embedded_sql(PSQLStripped, FCurrentCommand);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CopySQLasJavaCodeMIClick(Sender: TObject);

begin
  TntClipboard.AsText := myx_reconstruct_embedded_sql(PSQLStripped, FCurrentCommand);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteSelectedHistoryEntriesMIClick(Sender: TObject);

var
  I, J, K, L: Integer;
  Selection: TNodeArray;
  NodeData: PHistoryNodeData;
  HistoryInterval: PMYX_HISTORY_INTERVAL;
  PHistoryCatalog: PMYX_HISTORY_CATALOG;
  PHistorySchema: PMYX_HISTORY_SCHEMA;
  PPHistoryEntry: PPMYX_HISTORY_ENTRY;
  
begin
  Selection := HistoryVT.GetSortedSelection(True);

  for I := 0 to HistoryVT.SelectedCount - 1 do
  begin
    NodeData := HistoryVT.GetNodeData(Selection[I]);

    if (NodeData <> nil) then
      if (NodeData.Data <> nil) then
      begin
        if (NodeData.NodeType = HISTORY_INTERVAL_TYPE) then
        begin
          HistoryInterval := PMYX_HISTORY_INTERVAL(NodeData.Data);

          for J := 0 to HistoryInterval.catalogs_num - 1 do
          begin
            PHistoryCatalog := PMYX_HISTORY_CATALOG(
              (Integer(HistoryInterval.catalogs) + sizeof(MYX_HISTORY_CATALOG) * J)
              );

            for K := 0 to PHistoryCatalog.schemata_num - 1 do
            begin
              PHistorySchema :=
                PMYX_HISTORY_SCHEMA(
                (Integer(PHistoryCatalog.schemata) + sizeof(MYX_HISTORY_SCHEMA) * K)
                );

              for L := 0 to PHistorySchema.entries_num - 1 do
              begin
                PPHistoryEntry :=
                  PPMYX_HISTORY_ENTRY(
                  (Integer(PHistorySchema.entries) + sizeof(Pointer) * L)
                  );

                PPHistoryEntry^.marked_deleted := 1;
              end;
            end;
          end;
        end
        else
          if (NodeData.NodeType = HISTORY_ENTRY_TYPE) then
          begin
            PPHistoryEntry := PPMYX_HISTORY_ENTRY(NodeData.Data);

            PPHistoryEntry^.marked_deleted := 1;
          end;
      end;
  end;

  RefreshHistoryTree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ClearHistoryMIClick(Sender: TObject);

begin
  if (ShowModalDialog(_('Clear history'),
    _('Are you sure you want to clear the complete history?'),
    myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1) then
  begin
    HistoryVT.Clear;
    myx_history_free(FHistory);
    FHistory := myx_history_new;
    RefreshHistoryTree;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryPopupMenuPopup(Sender: TObject);

var
  HistoryNodeData: PHistoryNodeData;

begin
  DeleteSelectedHistoryEntriesMI.Enabled := (HistoryVT.SelectedCount > 0);

  AddHistoryItemasBookmarkMI.Enabled := False;
  if (HistoryVT.FocusedNode <> nil) then
  begin
    HistoryNodeData := HistoryVT.GetNodeData(HistoryVT.FocusedNode);
    if (HistoryNodeData <> nil) then
      if (HistoryNodeData.NodeType = HISTORY_ENTRY_TYPE) then
        AddHistoryItemasBookmarkMI.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddHistoryItemasBookmarkMIClick(Sender: TObject);

var
  HistoryNodeData: PHistoryNodeData;

begin
  if (HistoryVT.FocusedNode <> nil) then
  begin
    HistoryNodeData := HistoryVT.GetNodeData(HistoryVT.FocusedNode);
    if (HistoryNodeData <> nil) then
      if (HistoryNodeData.NodeType = HISTORY_ENTRY_TYPE) then
        AddBookmark(Trim(PPMYX_HISTORY_ENTRY(HistoryNodeData.Data)^.sql));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowQuickStartGuide;

begin
  ShowHelpTabSheet('mysqlqb_quickstart.html');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTEndDrag(Sender, Target: TObject; X, Y: Integer);

begin
  FreeAndNil(TableDragForm);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AdvancedQueryPnlBGShapeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

var
  NodeData: ^TObject;

begin
  if ((GetKeyState(VK_MENU) < 0) or
      (not (ApplicationDM.QBOptions.ShowDragTargetWindowOnAltPressedOnly))) and
    (Source=SchemataFrame.CatalogVST) and
    (SchemataFrame.CatalogVST.FocusedNode <> nil)then
  begin
    LastFocusedControl := FSQLEditor;

    NodeData:=SchemataFrame.CatalogVST.GetNodeData(SchemataFrame.CatalogVST.FocusedNode);

    if (NodeData^<>nil) and
      (NodeData^ is TMYX_SCHEMA_TABLE) and
      (TableDragForm = nil) then
    begin
      TableDragForm := TTableDragForm.Create(nil);
      TableDragForm.PlaceFormBelow(FSQLEditor, True);
      TableDragForm.ShowNoActivate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CopyRowValuesMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
      TntClipboard.AsText := ActiveResultsetPanel.RSGrid.GetValuesOfFocusedAndSelectedRows;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MaximizeQueryEditMIClick(Sender: TObject);

begin
  SQLEditorMaximized := not (SQLEditorMaximized);

  DoOptionsChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SchemataFrameCatalogVSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  NewCursor: TCursor;
  
begin
  if Shift = [] then
  begin
    case Key of
      Ord('X'):
        NewCursor := crDefault;
      Ord('S'):
        NewCursor := crSQLSelect;
      Ord('F'):
        NewCursor := crSQLFrom;
      Ord('W'):
        NewCursor := crSQLWhere;
      Ord('G'):
        NewCursor := crSQLGroup;
      Ord('H'):
        NewCursor := crSQLHaving;
      Ord('O'):
        NewCursor := crSQLOrder;
      Ord('T'):
        NewCursor := crSQLSet;
    else
      NewCursor := crNo;
    end;

    if NewCursor <> crNo then
    begin
      MessageToAllForms(WM_CursorChanged, NewCursor, 0);
      Key := 0;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ShowResultsetHits;

var
  I, J: Integer;

begin
  for I := 0 to MainTabHeaderFrame.TabCount - 1 do
    if (MainTabHeaderFrame.TabSheets[I].Obj is TRSTabSheet) then
      for J := 0 to TRSTabSheet(MainTabHeaderFrame.TabSheets[I].Obj).RSPanels.Count - 1 do
      begin
        TMySQLRSPanel(TRSTabSheet(MainTabHeaderFrame.TabSheets[I].Obj).RSPanels[J]).RSGrid.ShowHint := True;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HideResultsetHits;

var
  I, J: Integer;

begin
  for I := 0 to MainTabHeaderFrame.TabCount - 1 do
    if (MainTabHeaderFrame.TabSheets[I].Obj is TRSTabSheet) then
      for J := 0 to TRSTabSheet(MainTabHeaderFrame.TabSheets[I].Obj).RSPanels.Count - 1 do
        TMySQLRSPanel(TRSTabSheet(MainTabHeaderFrame.TabSheets[I].Obj).RSPanels[J]).RSGrid.ShowHint := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SQLEditMaximizedSplitterPBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  
begin
  FSQLEditMaximizedPnlHeight := SQLEditMaximizedPnl.Height;
  FSQLEditMaximizedPnlMouseY := Mouse.CursorPos.Y;
  FSQLEditMaximizedPnlMouseDown := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SQLEditMaximizedSplitterPBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if FSQLEditMaximizedPnlMouseDown then
    SQLEditMaximizedPnl.Height := FSQLEditMaximizedPnlHeight +
      (Mouse.CursorPos.Y - FSQLEditMaximizedPnlMouseY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SQLEditMaximizedSplitterPBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

begin
  if FSQLEditMaximizedPnlMouseDown then
  begin
    if CurrentPerspective=apResultSet then
      ApplicationDM.QBOptions.SQLEditMaximizedRSTabSheetHeight := SQLEditMaximizedPnl.Height
    else
      if CurrentPerspective=apScript then
        ApplicationDM.QBOptions.SQLEditMaximizedScriptTabSheetHeight := SQLEditMaximizedPnl.Height;

    FSQLEditMaximizedPnlMouseDown := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SQLEditMaximizedSplitterPBoxPaint(Sender: TObject);

begin
  with SQLEditMaximizedSplitterPBox.Canvas do
  begin
    Brush.Color:=clWhite;
    Pen.Color:=TabBorderColor;
    Rectangle(0, -1,
      SQLEditMaximizedSplitterPBox.Width,
      SQLEditMaximizedSplitterPBox.Height+1);

    Pen.Color := TabBorderColor + $00404040;
    MoveTo(1, 0);
    LineTo(SQLEditMaximizedSplitterPBox.Width-1, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.LoadFieldContentMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_Load);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SaveFieldContentMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_Save);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ClearFieldContentMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_Clear);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ViewFieldinPopupEditorMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_View);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditFieldinPopupEditorMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_Edit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CopyFieldContentMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel.RSGrid do
      GridFieldAction(FocusedNode, FocusedColumn, TGFAT_Copy);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddRowMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultsetPanel) then
    with ActiveResultsetPanel do
    begin
      RSGrid.ClearSelection;
      RSGrid.FocusedNode := RSGrid.GetLast;
      RSGrid.FocusedColumn := 1;
      RSGrid.EditNode(RSGrid.FocusedNode, RSGrid.FocusedColumn);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoEditStoredProcedure(Sender: TObject; CatalogName: WideString; SchemaName: WideString;
  SchemaObj: TObject);

var
  PSPData: PMYX_DBM_STORED_PROCEDURE_DATA;
  SPData: TMYX_DBM_STORED_PROCEDURE_DATA;

  Temp,
  SPType,
  SPName,
  FuncReturnType: WideString;
  Res: Integer;

begin
  // If there is no object given, a new one has to be created.
  if SchemaObj= nil then
  begin
    Res := ShowModalEditDialog(_('Enter Stored Procedure / Function Name'),
      _('Please enter the name of the PROCEDURE / FUNCTION you want '+
      'to create. A code template will be created.'), myx_mtEdit,
        _('Create PROCEDURE')+#13#10+_('Create FUNCTION')+ #13#10 +
        _('Cancel'), True, _('Name:'),
        SPName);

    if not (Res in [0, 3]) then
    begin
      if Res=1 then
      begin
        SPType := 'PROCEDURE';
        FuncReturnType := '';
      end
      else
      begin
        SPType := 'FUNCTION';
        FuncReturnType := ' RETURNS INT';
      end;

      EditScript(SPName, Format(
        'DELIMITER $$'#13#10#13#10+
        'DROP %s IF EXISTS `%s`.`%s` $$'#13#10+
        'CREATE %s `%s`.`%s` ()%s'+#13#10+
        'BEGIN'+#13#10+
        #13#10+
        'END $$'+#13#10#13#10+
        'DELIMITER ;',
        [SPType, SchemaName, SPName, SPType, SchemaName, SPName, FuncReturnType]), Point(2, 5));
    end;
  end
  else
    if (SchemaObj is TMYX_SCHEMA_STORED_PROCEDURE)then
    begin
      //Get SP code
      PSPData := myx_dbm_get_sp_data(MySQLConn.MySQL, CatalogName, SchemaName,
        TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).name,
        TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).sp_type, '`', 0);

      if PSPData = nil then
        ShowError(_('SQL error'), _('Could not fetch data for `%s`.`%s`'#13'Error code: %d'#13'%s'),
          [SchemaName, TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).name,
          myx_mysql_errno(MySQLConn.MySQL),
          myx_mysql_error(MySQLConn.MySQL)]);

      if TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).sp_type=MSPT_PROCEDURE then
        SPType:='PROCEDURE'
      else
        SPType:='FUNCTION';

      try
        SPData := TMYX_DBM_STORED_PROCEDURE_DATA.Create(PSPData);
        try
          // split into two parts to avoid #11860
          Temp := Format(
              'DELIMITER $$'#13#10#13#10+
              'DROP %s IF EXISTS `%s`.`%s` $$'#13#10,
              [SPType, SchemaName, TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).name]);
          Temp := Temp + SPData.definition + ' $$' + #13#10#13#10 + 'DELIMITER ;';

          EditScript(TMYX_SCHEMA_STORED_PROCEDURE(SchemaObj).name, Temp);
        finally
          SPData.Free;
        end;
      finally
        myx_dbm_free_sp_data(PSPData);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoEditView(Sender: TObject; CatalogName: WideString; SchemaName: WideString; SchemaObj: TObject);

var
  ViewName: WideString;
  SelTbl: WideString;
  NodeData: ^TObject;
  PViewData: PMYX_DBM_VIEW_DATA;
  ViewData: TMYX_DBM_VIEW_DATA;
  SQL: WideString;

begin
  // If there is no object given, a new one has to be created.
  if SchemaObj = nil then
  begin
    if ShowModalEditDialog(_('Enter View Name'), _('Please enter the name of the VIEW you want '+
      'to create. A code template will be created.'), myx_mtEdit, _('Create View')+#13#10+_('Cancel'), True, _('Name:'),
        ViewName) in [0, 2] then
      Exit;

    SelTbl := 'foo';

    if SchemataFrame.CatalogVST.FocusedNode<>nil then
    begin
      NodeData := SchemataFrame.CatalogVST.GetNodeData(
        SchemataFrame.CatalogVST.FocusedNode);

      if (NodeData^<>nil) and (NodeData^ is TMYX_SCHEMA_TABLE) then
        SelTbl := TMYX_SCHEMA_TABLE(NodeData^).table_name;
    end;

    SQL := Format(
        'CREATE VIEW `%s`.`%s` AS'#13#10+
        '  SELECT * FROM %s;',
        [SchemaName, ViewName, SelTbl]);

    EditScript(ViewName,
      SQL, Length(SQL)-Length(SelTbl)-8);
  end
  else
  begin
    if (SchemaObj is TMYX_SCHEMA_TABLE) and
      (TMYX_SCHEMA_TABLE(SchemaObj).table_type=MSTT_VIEW) then
    begin
      //Get SP code
      PViewData := myx_dbm_get_view_data(MySQLConn.MySQL, CatalogName, SchemaName,
        TMYX_SCHEMA_TABLE(SchemaObj).table_name, '`');

      if (PViewData=nil) then
        ShowError(_('SQL error'), _('Could not fetch data for `%s`.`%s`'#13'Error code: %d'#13'%s'),
          [SchemaName, TMYX_SCHEMA_TABLE(SchemaObj).table_name,
          myx_mysql_errno(MySQLConn.MySQL),
          myx_mysql_error(MySQLConn.MySQL)]);

      try
        ViewData := TMYX_DBM_VIEW_DATA.Create(PViewData);
        try
          //Write script
          EditScript(TMYX_SCHEMA_TABLE(SchemaObj).table_name,
            Format(
              'DROP VIEW IF EXISTS `%s`.`%s`;'#13#10'%s;',
              [SchemaName, TMYX_SCHEMA_TABLE(SchemaObj).table_name, ViewData.definition]));
        finally
          ViewData.Free;
        end;
      finally
        myx_dbm_free_view_data(PViewData);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FindMIClick(Sender: TObject);

begin
  if CurrentPerspective = apHelp then
    HelpWebBrowser.Find
  else
    if Assigned(ActiveScriptPanel) then
      ActiveScriptPanel.ScriptEditor.DoDisplaySearch
    else
      if Assigned(ActiveResultsetPanel) then
        ActiveResultsetPanel.RSGrid.MySQLRS.DoDisplaySearch;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RSGridFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

begin
  if Sender.FocusedNode<>nil then
  begin
    StatusBar.Panels[0].Text := Format(' %3d: %8d',
      [Ord(Sender.FocusedColumn), Sender.FocusedNode.Index + 1]);
  end
  else
    StatusBar.Panels[0].Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.QueryAreaHeaderPBoxPaint(Sender: TObject);

var
  Title: WideString;
  CaptionRect: TRect;
  DrawFlags: Cardinal;

begin
  with QueryAreaHeaderPBox.Canvas do
  begin
    CopyRect(Rect(0, 0, QueryAreaHeaderPBox.Width, QueryAreaHeaderPBox.Height), FMaximizedQueryAreaHeaderBGBmp.Canvas,
      Rect(0, 0, FMaximizedQueryAreaHeaderBGBmp.Width, FMaximizedQueryAreaHeaderBGBmp.Height));

    Pen.Style := psSolid;

    Pen.Color := clWhite;

    MoveTo(0, 1);
    LineTo(QueryAreaHeaderPBox.Width-1, 1);

    Pen.Color := $009C9B91;

    MoveTo(0, 0);
    LineTo(QueryAreaHeaderPBox.Width-1, 0);

    MoveTo(0, 0);
    LineTo(0, QueryAreaHeaderPBox.Height);

    MoveTo(28, 4);
    LineTo(28, QueryAreaHeaderPBox.Height-2);

    MoveTo(QueryAreaHeaderPBox.Width-1, 0);
    LineTo(QueryAreaHeaderPBox.Width-1, QueryAreaHeaderPBox.Height);

    Title := _('SQL Query Area');

    Font.Height := MYXCommonOptions.DefaultFontHeight;
    Font.Color := clBtnShadow;


    CaptionRect := Rect(33, 2, 200, QueryAreaHeaderPBox.Height - 1);
    DrawFlags := DT_SINGLELINE or DT_NOCLIP or DT_VCENTER;

    SetBkMode(Handle, TRANSPARENT);
    Windows.DrawTextW(Handle, PWideChar(Title), Length(Title), CaptionRect, DrawFlags);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.SQLCreateViewClick(Sender: TObject);

var
  ViewName: WideString;

begin
  if Assigned(ActiveResultset) and (ActiveResultset.ResultSet.query.query_type = MYX_QT_SELECT)then
  begin
    if ShowModalEditDialog(_('Enter View Name'),
      _('Please enter the name of the VIEW you want '+
      'to create.'), myx_mtEdit,
        _('Create View')+#13#10+_('Cancel'), True, _('Name:'),
        ViewName)=2 then
      Exit;

    MySQLConn.ExecuteDirect(Format('CREATE VIEW `%s` AS %s', [ViewName, Utf8Decode(ActiveResultset.ResultSet.query.sql)]));
    SchemataFrame.ReloadSchemaTree;
  end
  else
    ShowError('Creation error', _('A view can only be created from a active resultset of SELECT command.'), []);
end;

//----------------------------------------------------------------------------------------------------------------------


procedure TQueryBrowserForm.SQLEditSPClick(Sender: TObject);

begin
  EditScript(SchemataFrame.CurrentSchema.schema_name+' procedures',
    myx_dbm_make_script_from_sps(MySQLConn.MySQL, '',
      MySQLConn.DefaultSchema, SchemataFrame.CurrentSchema.schema_sps.get_record_pointer, '`'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AdvanceToNextCommand;

// Moves the caret to the next command after the current (if there is one).

var
  Y: Integer;
  X: Integer;
  Line: TUCELine;
  Data: TLineData;

begin
  Y := FCurrentCommandEnd.Y;
  if Y > -1 then
  begin
    repeat
      Inc(Y);
      if Y >= FSQLEditor.Content.Count then
        Break;

      Line := FSQLEditor.Content[Y];
      Data := Line.Data as TLineData;
      X := Data.FirstStatementStart;
      if X > -1 then
        Break;
    until False;
  end;

  // Set the caret to it. The update command code will take care of the rest.
  FSQLEditor.CaretXY := Point(0, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditScript(Caption: WideString; Text: WideString;
  CaretXY: TPoint);

begin
  EditScript(Caption, Text);

  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.CaretXY := CaretXY;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditScript(Caption: WideString; Text: WideString;
  SelStart: Integer);

begin
  EditScript(Caption, Text);

  if Assigned(ActiveScriptPanel) then
  begin
    ActiveScriptPanel.ScriptEditor.SelStart := SelStart;
    ActiveScriptPanel.ScriptEditor.CaretXY := ActiveScriptPanel.ScriptEditor.BlockBegin;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditScript(Caption: WideString; Text: WideString);

begin
  if (SchemataFrame.CurrentSchema<>nil) and
    (SchemataFrame.CurrentSchema.schema_sps<>nil) then
  begin
    // Add new tabsheet if no tabsheet with empty ScriptEditor is available.
    if (Assigned(ActiveScriptPanel) and
      (ActiveScriptPanel.ScriptEditor.Content.Count > 0)) or (FActiveScriptTabSheet = nil) then
      FActiveScriptTabSheet := AddNewScriptTabSheet(Caption)
    else
    begin
      MainTabHeaderFrame.SelectedTab := MainTabHeaderFrame.TabIndexFromObject[FActiveScriptTabSheet];
      MainTabHeaderFrame.TabSheets[MainTabHeaderFrame.SelectedTab].Caption := Caption;
    end;

    if Assigned(ActiveScriptPanel) then
      ActiveScriptPanel.ScriptEditor.Text := Text;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.EditAllStoredProceduresFunctionsMIClick(
  Sender: TObject);

begin
  SQLEditSPClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.CreateStoredProcedureFunctionMIClick(
  Sender: TObject);

begin
  DoEditStoredProcedure(self, '', MySQLConn.DefaultSchema, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ToggleBreakpointMIClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.ToggleBreakpoint(ActiveScriptPanel.ScriptEditor.CaretY);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ClearAllBreakpointsMIClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.ClearBreakpoints;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);

begin
  try
    while MainTabHeaderFrame.TabCount > 0 do
      if not MainTabHeaderFrame.DeleteTab(MainTabHeaderFrame.TabCount-1, True) then
      begin
        CanClose := False;
        Break;
      end;
    Application.ProcessMessages;
  except
    // Allow close even when there was an exception
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransactionStatusChanged;

begin
  Toolbars.ItemsEnabled['TrxStartBtn'] := not MySQLConn.InTransaction;
  Toolbars.ItemsEnabled['TrxCommitBtn'] := MySQLConn.InTransaction;
  Toolbars.ItemsEnabled['TrxRollbackBtn'] := MySQLConn.InTransaction;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransactionQueryExecuted(SQLCmd: WideString);

begin
  FTrxQueryList.Add(TTrxQueryNode.Create(SQLCmd, True));
  TransactionDisplay.RootNodeCount := FTrxQueryList.Count;
  TransactionDisplay.OffsetY := -MaxInt;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransactionDisplayGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

begin
  CellText := TTrxQueryNode(FTrxQueryList[Node.Index]).Query;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TransactionStarted;

begin
  TransactionDisplay.RootNodeCount := 0;
  FTrxQueryList.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TTrxQueryNode.Create(Query: WideString; Success: Boolean);

begin
  FQuery := Query;
  FSuccess := Success;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.AddParameterMIClick(Sender: TObject);

var
  ParamGroupNode: PVirtualNode;
  Name: WideString;

begin
  if Assigned(ActiveResultset) and Assigned(ParamVT.FocusedNode) then
  begin
    if (ParamVT.FocusedNode = GlobalParamsNode) or (ParamVT.FocusedNode = LocalParamsNode) then
      ParamGroupNode := ParamVT.FocusedNode
    else
      if (ParamVT.FocusedNode.Parent = GlobalParamsNode) or (ParamVT.FocusedNode.Parent = LocalParamsNode) then
        ParamGroupNode := ParamVT.FocusedNode.Parent
      else
        ParamGroupNode := nil;

    if (ParamGroupNode = DynamicParamsNode) then
    begin
      ShowError(_('Cannot add dynamic parameter'),
        _('You cannot add a dynamic parameter manually. The dynamic '+
          'paramteters are defined automatically by the columns of '+
          'a master resultset.'), []);
      Exit;
    end;

    if (ParamGroupNode = LocalParamsNode) or (ParamGroupNode = GlobalParamsNode) then
    begin
      if ShowModalEditDialog(_('New Parameter'),
        _('Please enter a name for the new parameter.'),
        myx_mtEdit, _('Create')+#13#10+_('Cancel'),
        True, _('Name'), Name)=1 then
      begin
        if (ParamGroupNode = GlobalParamsNode) then
          ActiveResultset.GlobalParams.Add(Name + '=NULL')
        else
          ActiveResultset.LocalParams.Add(Name + '=NULL');

        DoRefreshParams(ActiveResultset);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamPopupMenuPopup(Sender: TObject);

var
  ParamGroupNode: PVirtualNode;

begin
  AddParameterMI.Enabled := False;

  if Assigned(ActiveResultset) and Assigned(ParamVT.FocusedNode) then
  begin
    if (ParamVT.FocusedNode = GlobalParamsNode) or
      (ParamVT.FocusedNode = LocalParamsNode) then
      ParamGroupNode := ParamVT.FocusedNode
    else
      if (ParamVT.FocusedNode.Parent = GlobalParamsNode) or
        (ParamVT.FocusedNode.Parent = LocalParamsNode) then
        ParamGroupNode := ParamVT.FocusedNode.Parent
      else
        ParamGroupNode := nil;

    if (ParamGroupNode = LocalParamsNode) or (ParamGroupNode = GlobalParamsNode) then
      AddParameterMI.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.RefreshParametersMIClick(Sender: TObject);

begin
  if Assigned(ActiveResultset) then
    DoRefreshParams(ActiveResultset)
  else
    DoRefreshParams(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DeleteParameterMIClick(Sender: TObject);

var
  ParamGroupNode: PVirtualNode;
  NodeData: PParamRowData;

begin
  if Assigned(ActiveResultset) and Assigned(ParamVT.FocusedNode) then
  begin
    if (ParamVT.FocusedNode = GlobalParamsNode) or
      (ParamVT.FocusedNode = LocalParamsNode) then
      ParamGroupNode := ParamVT.FocusedNode
    else
      if (ParamVT.FocusedNode.Parent = GlobalParamsNode) or
        (ParamVT.FocusedNode.Parent = LocalParamsNode) or
        (ParamVT.FocusedNode.Parent = DynamicParamsNode)then
        ParamGroupNode := ParamVT.FocusedNode.Parent
      else
        ParamGroupNode := nil;

    if (ParamGroupNode = DynamicParamsNode) then
      ShowError(_('Cannot delete dynamic parameter'), _('You cannot delete a dynamic parameter.'), [])
    else
    begin
      NodeData := ParamVT.GetNodeData(ParamVT.FocusedNode);

      if (Integer(ParamVT.FocusedNode.Index) < NodeData.params.Count) then
        NodeData.params.Delete(ParamVT.FocusedNode.Index);

      DoRefreshParams(ActiveResultset);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PParamRowData;

begin
  if Assigned(ActiveResultset) and
    not ((Node = GlobalParamsNode) or (Node = LocalParamsNode) or (Node = DynamicParamsNode)) then
  begin
    NodeData := Sender.GetNodeData(Node);

    if (NodeData.params<>nil) and
      (Integer(Node.Index) < NodeData.params.Count) then
      if Column = 0 then
        NodeData.params[Node.Index] := NewText+'=' + NodeData.params.ValueFromIndex[Node.Index]
      else
        if Column = 1 then
          NodeData.params.ValueFromIndex[Node.Index] := NewText;

    DoRefreshParams(ActiveResultset);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
  
begin
  Allowed := Assigned(ActiveResultset) and
    not ((Node = GlobalParamsNode) or (Node = LocalParamsNode) or (Node = DynamicParamsNode));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MoveParametertoGlobalParametersMIClick(Sender: TObject);

var
  Node: PVirtualNode;
  NodeData: PParamRowData;

begin
  Node := ParamVT.FocusedNode;

  if Assigned(ActiveResultset) and
    not ((Node = GlobalParamsNode) or (Node = LocalParamsNode) or (Node = DynamicParamsNode) or
      (Node.Parent = GlobalParamsNode)) then
  begin
    NodeData := ParamVT.GetNodeData(Node);

    if (Integer(Node.Index) < NodeData.params.Count) then
    begin
      FGlobalParams.Values[NodeData.params.Names[Node.Index]] := NodeData.params.ValueFromIndex[Node.Index];
      NodeData.params.Delete(Node.Index);
      DoRefreshParams(ActiveResultset);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoUpperTabChange(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
  NewSelectedTab: Integer; obj: TObject);

begin
  if (NewSelectedTab=
    UpperTabHeaderFrame.TabIndexFromPanel[HistoryPanel]) then
    HistoryVT.OffsetY := -MaxInt;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.BookmarksAdvancedEditSearchEdChange(Sender: TObject);

begin
  BookmarksAdvancedEdit.SearchEdChange(Sender);
  PrepareSearch(stBookmarks, (Sender as TTntEdit).Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.HistoryAdvancedEditSearchEdChange(Sender: TObject);

begin
  HistoryAdvancedEdit.SearchEdChange(Sender);
  PrepareSearch(stHistory, (Sender as TTntEdit).Text);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.TrxCopySQLMIClick(Sender: TObject);
begin
  if (TransactionDisplay.FocusedNode<>nil) and
    (Integer(TransactionDisplay.FocusedNode.Index)<FTrxQueryList.Count) then
    TntClipboard.AsText :=
      TTrxQueryNode(FTrxQueryList[TransactionDisplay.FocusedNode.Index]).Query;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MainTabHeaderFrameDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

var
  SchemaInfo: ISchemaInfo;
  SchemaNodeData: ^TObject;

begin
  if (Source = SchemataFrame.CatalogVST) then
  begin
    LastFocusedControl := FSQLEditor;

    //Show Table-Drag-Panel
    if (GetKeyState(VK_MENU) < 0) or
      (not (ApplicationDM.QBOptions.ShowDragTargetWindowOnAltPressedOnly)) then
    begin
      //only if a Table is dragged
      SchemaNodeData := SchemataFrame.CatalogVST.GetNodeData(
        SchemataFrame.CatalogVST.FocusedNode);
      if (SchemaNodeData <> nil) and
        (SchemaNodeData^ <> nil) and
        ((SchemaNodeData^ is TMYX_SCHEMA_TABLE) or
          (SchemaNodeData^ is TMYX_SCHEMA_TABLE_COLUMN)) then
      begin
        Accept := True;

        if (TableDragForm = nil) then
        begin
          TableDragForm := TTableDragForm.Create(nil);
          TableDragForm.PlaceFormBelow(FSQLEditor, True);
          TableDragForm.DragTarget := SchemaNodeData^.ClassType;
          TableDragForm.ShowNoActivate;
        end;
      end;
    end;
  end
  else
  begin
    // TObject itself is not compatible to any interface, so we cannot use compiler magic to
    // assign it to an interface variable.
    SchemataFrame.GetInterface(ISchemaInfo, SchemaInfo);
    Accept := Assigned(SchemaInfo);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.MainTabHeaderFrameDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  SchemaInfo: ISchemaInfo;
  TableList: TWideStringList;

begin
  // TObject itself is not compatible to any interface, so we cannot use compiler magic to
  // assign it to an interface variable.
  SchemataFrame.GetInterface(ISchemaInfo, SchemaInfo);
  if Assigned(SchemaInfo) then
  begin
    TableList := TWideStringList.Create;
    try
      SchemaInfo.GetSelectedTables(TableList);
      DoDropTables(Sender, TableList, KeyboardStateToShiftState);
    finally
      TableList.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptUndoClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.Undo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptRedoClick(Sender: TObject);

begin
  if Assigned(ActiveScriptPanel) then
    ActiveScriptPanel.ScriptEditor.Redo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.UpdateActions;

var
  Editor: TUnicodeEdit;

begin
  inherited;

  if Assigned(ActiveScriptPanel) then
  begin
    Editor := ActiveScriptPanel.ScriptEditor;
    ScriptRunSelectionMI.Enabled := Editor.SelectionAvailable;
    
    if (Toolbars.ItemsEnabled['ScriptRedoBtn'] <> Editor.CanRedo) then
    begin
      Toolbars.ItemsEnabled['ScriptRedoBtn'] := Editor.CanRedo;
      Toolbars.ItemsEnabled['ScriptSmallRedoBtn'] := Editor.CanRedo;
    end;

    if (Toolbars.ItemsEnabled['ScriptUndoBtn'] <> Editor.CanUndo) then
    begin
      Toolbars.ItemsEnabled['ScriptUndoBtn'] := Editor.CanUndo;
      Toolbars.ItemsEnabled['ScriptSmallUndoBtn'] := Editor.CanUndo;
    end;

    if (Toolbars.ItemsEnabled['ScriptSaveBtn'] <> Editor.Modified) then
    begin
      Toolbars.ItemsEnabled['ScriptSaveBtn'] := Editor.Modified;
      Toolbars.ItemsEnabled['ScriptSmallSaveBtn'] := Editor.Modified;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);

begin
  Accept := False;

  if (Source = SchemataFrame.CatalogVST) and
    (SchemataFrame.CatalogVST.SelectedCount > 0) then
    Accept := True;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptDragDrop(Sender, Source: TObject; X, Y: Integer);

var
  Editor: TScriptEditor;
  
begin
  if (Source = SchemataFrame.CatalogVST) and (Sender is TScriptEditor) then
  begin
    Editor := Sender as TScriptEditor;
    Editor.Content.Text := SchemataFrame.GetAssetSQLCreateText;
    Editor.ClearSelection;
    Editor.CaretXY := Point(0, 0);

    if Editor.CanFocus then
      Editor.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ParamVTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoVTEdit, ParamVT.Handle, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.DoVTEdit(var Message: TMessage);

begin
  if (ParamVT.Handle = HWND(Message.WParam)) then
  begin
    if (ParamVT.FocusedNode<>nil) then
      ParamVT.EditNode(ParamVT.FocusedNode, ParamVT.FocusedColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.ScriptRunSelectionMIClick(Sender: TObject);

begin
  ScriptExecute([exoStartOver, exoStopOnBreakpoints, exoSelectionOnly]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TQueryBrowserForm.UpdateCommandDisplay(X, Y: Integer; Force: Boolean);

// Checks the current SQL editor line (given by Y) and tries to find the last SQL command that begins before or on this
// line. If this has changed from the last command we found then the procedure updates the command position
// variables and the display.

var
  Line: TUCELine;
  Data: TLineData;
  I: Integer;
  Start: Integer;
  Stop: Integer;
  Statement: PStatement;
  LookingForStart: Boolean;
  NewCommandEnd: TPoint;

begin
  // Note: all lines at least up to the current line are usually validated.
  if Y < FSQLEditor.Content.Count then
  begin
    // Search the start of the first command on the line (might start on earlier lines).
    X := -1;

    // Set to True if we need the start of a multi-line command.
    LookingForStart := False;
    repeat
      Line := FSQLEditor.Content[Y];
      Data := Line.Data as TLineData;
      if LookingForStart then
      begin
        Statement := Data.LastStatement;

        // Reset current command end marker as we are moving backward to find the
        // command's start and have to scan later again for the real end.
        NewCommandEnd.X := -2;
        if Assigned(Statement) then
        begin
          X := Statement.Start;
          NewCommandEnd.Y := Y;
        end;
      end
      else
      begin
        Statement := Data.FirstStatementRaw;
        if Assigned(Statement) then
        begin
          NewCommandEnd.X := Statement.Stop;
          X := Statement.Start;
        end;
        NewCommandEnd.Y := Y;
      end;

      if ((Statement = nil) and not Data.CommandPending) or (X > -1) then
        Break;
      Dec(Y);
      LookingForStart := True;
    until Y < 0;

    // Does this command start differ from what we have already currently?
    if (FCurrentCommandStart.Y <> Y) or Force then
    begin
      // Remove special style from the old command.
      if FCurrentCommandStart.Y > -1 then
      begin
        // If no command end is set or the line does no longer exist take the entire rest of the content
        // (limit end to currently available lines).
        Start := FCurrentCommandStart.Y;
        if Start < FSQLEditor.Content.Count then
        begin
          Stop := FCurrentCommandEnd.Y;
          if (Stop = -1) or (Stop >= FSQLEditor.Content.Count) then
            Stop := FSQLEditor.Content.Count - 1;
          for I := Start to Stop do
          begin
            FSQLEditor.Content[I].RemoveStyle(FCurrentCommandStyle);
            FSQLEditor.RefreshLine(I);
          end;
        end;
      end;

      FCurrentCommand := '';
      
      // Now prepare the new command.
      FCurrentCommandEnd := NewCommandEnd;
      FCurrentCommandStart.X := X;
      FCurrentCommandStart.Y := Y;

      if (Y > -1) and (X > -1) then
      begin
        FCurrentCommandIsSingle := Data.StatementCount = 1;

        // Search the end of the current command.
        while (FCurrentCommandEnd.X < 0) and (FCurrentCommandEnd.Y < FSQLEditor.Content.Count - 1) do
        begin
          Inc(FCurrentCommandEnd.Y);
          Line := FSQLEditor.Content[FCurrentCommandEnd.Y];
          Data := Line.Data as TLineData;
          if not Data.Analyzed then
            FSQLEditor.AnalyzeLine(True, Line, False);
          FCurrentCommandEnd.X := Data.FirstStatementEnd;
        end;

        // If no command end could be found then FCurrentCommandEnd.Y has the last line as command end line.
        // Use the entire line in this case for the statement.
        if (FCurrentCommandEnd.Y = FSQLEditor.Content.Count - 1) and (FCurrentCommandEnd.X < 0) then
          FCurrentCommandEnd.X := Length(FSQLEditor.Content[FCurrentCommandEnd.Y].Text);

        FCurrentCommand := FSQLEditor.Content.CollectTextFromPosition(FCurrentCommandStart, FCurrentCommandEnd);

        // Check end of the command.
        for I := FCurrentCommandStart.Y to FCurrentCommandEnd.Y do
        begin
          FSQLEditor.Content[I].PushStyle(FCurrentCommandStyle);
          FSQLEditor.RefreshLine(I);
        end;
      end
      else
      begin
        FCurrentCommandIsSingle := True;
        FCurrentCommandStart.Y := -1;
        FCurrentCommandEnd.Y := -1;
      end;
    end;
  end;
end;

//----------------- TCurrentCommandStyle -------------------------------------------------------------------------------

function TCurrentCommandStyle.GetBackground: TColor;

begin
  Result := clWindow;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentCommandStyle.GetFontStyles: TFontStyles;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentCommandStyle.GetForceFontStyles: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentCommandStyle.GetForeground: TColor;

begin
  Result := clDefault;
end;

//----------------- TInactiveCommandStyle ------------------------------------------------------------------------------

function TInactiveCommandStyle.GetBackground: TColor;

begin
  Result := $00EEEEEE;
end;

//----------------------------------------------------------------------------------------------------------------------

function TInactiveCommandStyle.GetFontStyles: TFontStyles;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TInactiveCommandStyle.GetForceFontStyles: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TInactiveCommandStyle.GetForeground: TColor;

begin
  Result := clDefault;
end;

//----------------- TSQLEditor -----------------------------------------------------------------------------------------

procedure TSQLEditor.ChangeLine(Sender: TObject; Line: TUCELine);

begin
  inherited;

  if Owner is TQueryBrowserForm then
    (Owner as TQueryBrowserForm).DoQueryEditorChange;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLEditor.DeleteLine(Sender: TObject; Line: TUCELine);

// Triggered when a line in the editor is deleted. Make sure the QB form can update its current command parts.

begin
  inherited;

  if Owner is TQueryBrowserForm then
    (Owner as TQueryBrowserForm).LineDeleted(Line);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

