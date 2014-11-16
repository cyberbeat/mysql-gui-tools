unit MySQLResultSetControls;

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

{$include Compilers.inc}

interface

uses
  // Delphi units.
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms, ActiveX, Menus,
  // Third party units.
  VirtualTrees, Unicode,
  TntExtCtrls, TntComCtrls, TntClasses, TntStdCtrls, TntForms, TntMenus, TntSysUtils, TntDialogs, TnTClipbrd,
  PNGImage,
  gnugettext,
  // Own units.
  myx_public_interface, auxfuncs, AuxLists, Options, StrUtils,
  MySQLConnection, MySQLResultSet, MySQLResultSetFieldViewer,
  CommonTypes, TextSearch, MyxError;

const
  WM_EditCurrentCell = WM_USER + 300;

type
  TMySQLRSPanel = class;
  TMySQLRSGrid = class;
  TMySQLRSNav = class;
  TMySQLRSErrorGrid = class;

  TGridFieldActionType = (
    TGFAT_Clear,
    TGFAT_Save,
    TGFAT_Load,
    TGFAT_View,
    TGFAT_Edit,
    TGFAT_Copy
  );

  TPanelChanged = procedure (PreviousSelectedPanel: TObject; SelectedPanel: TObject) of object;

  TRSTabSheet = class(TTntPanel)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    function GetActiveRSPanel: TMySQLRSPanel;
    procedure SetActiveRSPanel(ActiveRSPanel: TMySQLRSPanel);

    function GetParallelGridScroll: Boolean;
    procedure SetParallelGridScroll(ParallelGridScroll: Boolean);

    procedure SetShowFieldOverlayImages(ShowImgs: Boolean);
  private
    FActiveRSPanel: TMySQLRSPanel;
    FHaveExplainPanel: Boolean;        // If true then the last panel shows an EXPLAIN result.

    FOnPanelChanging: TNotifyEvent;
    FOnPanelChanged: TPanelChanged;

    FParallelGridScroll: Boolean;

    SplitDirection: Integer;

    FShowFieldOverlayImages: Boolean;
  public
    RSPanels: TList;
    Splitters: TList;
    GridsAreScrolling: Boolean;

    InitControls: Boolean;

    procedure ActivateCompareMode;
    procedure ActivateExplainPanel(MySQLConn: TMySQLConn);
    function AddRSPanel(MySQLConn: TMySQLConn; SplitVertically: Boolean = True; DoFocus: Boolean = True;
      Size: Integer = -1): TMySQLRSPanel;
    procedure DeleteRSPanel(RSPanel: TMySQLRSPanel);

    property ActiveRSPanel: TMySQLRSPanel read GetActiveRSPanel write SetActiveRSPanel;
    property ParallelGridScroll: Boolean read GetParallelGridScroll write SetParallelGridScroll default False;
    property ShowFieldOverlayImages: Boolean read FShowFieldOverlayImages write SetShowFieldOverlayImages;

    property OnPanelChanging: TNotifyEvent read FOnPanelChanging write FOnPanelChanging;
    property OnPanelChanged: TPanelChanged read FOnPanelChanged write FOnPanelChanged;
  end;

  TMySQLRSPanel = class(TTntPanel)
  private
    FMySQLRS: TMySQLRS;
    FActive: Boolean;
    FRSGrid: TMySQLRSGrid;
    FRSNav: TMySQLRSNav;
    FRSErrorGrid: TMySQLRSErrorGrid;

    FLastEditorContent: WideString;
    FLastEditorPosition: TPoint;
    FLastSelectionStart: TPoint;
    FLastSelectionEnd: TPoint;
  protected
    function GetMySQLRS: TMySQLRS;
    procedure SetMySQLRS(MySQLRS: TMySQLRS);

    function GetActive: Boolean;
    procedure SetActive(Active: Boolean);

    procedure DoChildControlEnter(Sender: TObject);
    function GetTotalHeight: Integer;
    procedure SetTotalHeight(Height: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateMySQLRS(MySQLConn: TMySQLConn);
  published
    property Active: Boolean read GetActive write SetActive;
    property LastEditorContent: WideString read FLastEditorContent write FLastEditorContent;
    property LastEditorPosition: TPoint read FLastEditorPosition write FLastEditorPosition;
    property LastEditorSelectionEnd: TPoint read FLastSelectionEnd write FLastSelectionEnd;
    property LastEditorSelectionStart: TPoint read FLastSelectionStart write FLastSelectionStart;
    property MySQLRS: TMySQLRS read GetMySQLRS write SetMySQLRS default nil;
    property RSGrid: TMySQLRSGrid read FRSGrid;
    property TotalHeight: Integer read GetTotalHeight write SetTotalHeight;
  end;

  // Need an own class since we have to remove entries out-of-order at times.
  TCaptionQueue = class(TQueue)
  private
    FLimit: Integer;
  protected
    procedure PushItem(AItem: Pointer); override;
  public
    constructor Create(Limit: Integer);

    procedure Remove(Entry: Pointer);
  end;

  TDropTablesEvent = procedure(Sender: TObject; List: TWideStringList; Shift: TShiftState) of object;

  TMySQLRSGrid = class(TVirtualStringTree, IMySQLRSControlInterface, IOptionChangeListener)
  private
    FMySQLRS: TMySQLRS;
    FActive: Boolean;
    FMRUCaptions: TCaptionQueue;
    FCompareActive: Boolean;

    NodesWithActionList,
    NodesWithDeleteActionList: TList;

    FieldClearPNGImg,
    FieldEditPNGImg,
    FieldLoadPNGImg,
    FieldSavePNGImg,
    FieldViewPNGImg,
    FieldNullPNGImg,
    BlobPNGImg: TPNGObject;
    FColumnIndicators: array of TPNGObject;

    FShowFieldOverlayImages: Boolean;

    FHotRSNode: PVirtualNode;
    FHotRSColumn: TColumnIndex;

    HeaderPopupMenu: TPopupMenu;

    // Options
    FFriendlyLineBreaks: Boolean;
    FFriendlyLineBreaksLF: WideString;
    FFriendlyLineBreaksCR: WideString;
    FAlignNumericColsRight: Boolean;
    FAutoEdit: Boolean;

    FLastSearchNode: PVirtualNode;
    FLastSearchCol: TColumnIndex;
    FLastSearchText: WideString;

    FMainRowColor: TColor;
    FAlternativeRowColor: TColor;

    FOnDropTables: TDropTablesEvent;
    procedure SetCompareActive(const Value: Boolean);
    function InternalGetNodeRSText(Node: PVirtualNode; Column: TColumnIndex): WideString;
  protected
    //Interface functions
    function GetMySQLRS: TMySQLRS;
    procedure SetMySQLRS(MySQLRS: TMySQLRS);
    function GetControl: TObject;

    function GetActive: Boolean;
    procedure SetActive(Active: Boolean);

    procedure DoCurrentRowChanged;
    procedure DoEditStateChanged;
    procedure DoStatusCaptionChanged;
    procedure DoMessagesChanged;
    //End Interface functions

    procedure ComputeRowColors;
    procedure DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
      var Effect: Integer; Mode: TDropMode); override;
    function DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer): Boolean; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType); override;
    function DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex): Boolean; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column:
      TColumnIndex; CellRect: TRect); override;
    procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoFreeNode(Node: PVirtualNode); override;

    function GetNodeRSAction(Node: PVirtualNode; Column: TColumnIndex;
      IgnoreDeleteActions: Boolean = False): PMYX_RS_ACTION;
    function GetNodeRSRow(Node: PVirtualNode): PMYX_RS_ROW;
    function GetNodeRSField(Node: PVirtualNode; Column: TColumnIndex): PMYX_RS_FIELD;
    function GetNodeRSValue(Node: PVirtualNode; Column: TColumnIndex): TRSFieldValue;
    function GetNodeRSText(Node: PVirtualNode; Column: TColumnIndex; ForEdit: Boolean): WideString;
    function GetOptionProvider: IOptionProvider;

    procedure AddAction(action_type: MYX_RS_ACTION_TYPE; row: Cardinal;
      Column: TColumnIndex; Node: PVirtualNode;
      new_value: WideString; new_value_length: Integer = -1; new_binary_value: string = '');
    procedure DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: WideString); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
      var EraseAction: TItemEraseAction); override;
    function DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle): WideString; override;

    procedure DoGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);

    function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;

    procedure DblClick; override;
    procedure WndProc(var Message: TMessage); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OptionChanged;

    procedure SetShowFieldOverlayImages(ShowImgs: Boolean);

    procedure CopyColumnNames(Sender: TObject);
    procedure RefreshOptions;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure HeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BuildColumns;
    procedure BuildRows(StartRow: Integer; EndRow: Integer; SetFocusOnFirstNode: Boolean = False; RS: TMySQLRS =nil);
    procedure BuildRowForAppending;
    procedure ClearValues;
    function GetValuesOfFocusedAndSelectedRows: WideString;
    procedure GridFieldAction(Node: PVirtualNode; Column: TColumnIndex; GridFieldActionType: TGridFieldActionType);

    property CompareActive: Boolean read FCompareActive write SetCompareActive;

    function DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
      SearchOptions: TTextSearchOptions): Integer;
    procedure DeleteSelectedRows;
    procedure ApplyDeleteActionsToGrid;
    procedure SynchronizeActions;
    procedure FinishedBuilding;

    property MySQLRS: TMySQLRS read GetMySQLRS write SetMySQLRS default nil;
    property Active: Boolean read GetActive write SetActive;
    property ShowFieldOverlayImages: Boolean read FShowFieldOverlayImages write SetShowFieldOverlayImages;

    property OnDropTables: TDropTablesEvent read FOnDropTables write FOnDropTables;
  end;

  TWideStringArray = array of WideString;

  PRSRowData = ^TRSRowData;
  TRSRowData = record
    row_index: Integer;
    actions: TIntegerList;
    Captions: TWideStringArray;
    FriendlyLinebreaksCaptions: TWideStringArray;
  end;

  TMySQLRSNav = class(TTntPanel, IMySQLRSControlInterface)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearValues;
  private
    FMySQLRS: TMySQLRS;

    FActive: Boolean;

    RSEditPNGImg,
    RSEditDisabledPNGImg,
    RSEditActivePNGImg,
    RSApplyPNGImg,
    RSApplyDisabledPNGImg,
    RSDiscardPNGImg,
    RSDiscardDisabledPNGImg,
    RSFirstPNGImg,
    RSFirstDisabledPNGImg,
    RSLastPNGImg,
    RSLastDisabledPNGImg,
    RSSearchPNGImg,
    RSSearchDisabledPNGImg,
    RSBtnDownBgImg,
    RSBtnDownLeftBgImg: TPNGObject;

    RSEditXPos,
    RSApplyXPos,
    RSDiscardXPos,
    RSFirstXPos,
    RSLastXPos,
    RSSearchXPos: Integer;
  protected
    //Interface functions
    function GetMySQLRS: TMySQLRS;
    procedure SetMySQLRS(MySQLRS: TMySQLRS);
    function GetControl: TObject;

    procedure DoCurrentRowChanged;
    procedure DoEditStateChanged;
    procedure DoStatusCaptionChanged;
    procedure DoMessagesChanged;
    //End Interface functions

    function GetActive: Boolean;
    procedure SetActive(Active: Boolean);

    procedure Paint; override;
    function PaintEditButton(Caption: WideString; var xpos: Integer;
      PNGImg: TPNGObject; DisabledPNGImg: TPNGObject; Enabled: Boolean;
      ActivePNGImg: TPNGObject = nil; Active: Boolean = False): Integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  published
    property MySQLRS: TMySQLRS read GetMySQLRS write SetMySQLRS default nil;
    property Active: Boolean read GetActive write SetActive;
  end;

  TMySQLRSErrorGrid = class(TVirtualStringTree, IMySQLRSControlInterface)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearValues;
  private
    FMySQLRS: TMySQLRS;

    RSErrorPNGImg,
    RSWarningPNGImg,
    RSNotePNGImg: TPNGObject;

    MsgPopupMenu: TTntPopupMenu;
  protected
    //Interface functions
    function GetMySQLRS: TMySQLRS;
    procedure SetMySQLRS(MySQLRS: TMySQLRS);
    function GetControl: TObject;

    procedure DoCurrentRowChanged;
    procedure DoEditStateChanged;
    procedure DoActionsChanged;
    procedure DoStatusCaptionChanged;
    procedure DoMessagesChanged;
    procedure MsgPopup(Sender: TObject);

    //End Interface functions

    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var Text: WideString); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column:
      TColumnIndex; CellRect: TRect); override;

    procedure CopyMsgText(Sender: TObject);
    procedure ClearMsgText(Sender: TObject);
  published
    property MySQLRS: TMySQLRS read GetMySQLRS write SetMySQLRS default nil;
  end;

  PRSMsg = ^TRSMsg;
  TRSMsg = record
    MsgIndex: Integer;
  end;

  TRSFieldEditLink = class;

  TRSStringEdit = class(TTntEdit)
  private
    FRefLink: IVTEditLink;
    FLink: TRSFieldEditLink;
    procedure CMAutoAdjust(var Message: TMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
  protected
    procedure AutoAdjustSize;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TRSFieldEditLink); reintroduce;

    procedure Release; virtual;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property PasswordChar;
  end;

  TRSFieldEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TRSStringEdit;
    FTree: TMySQLRSGrid; // A back reference to the tree calling.
    FNode: PVirtualNode; // The node to be edited.
    FColumn: TColumnIndex; // The column of the node.
    FAlignment: TAlignment;
    FTextBounds: TRect; // Smallest rectangle around the text.
    FStopping: Boolean; // Set to True when the edit link requests stopping the edit action.
    procedure SetEdit(const Value: TRSStringEdit);
  public
    constructor Create;
    destructor Destroy; override;

    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    property Edit: TRSStringEdit read FEdit write SetEdit;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, CommCtrl, SchemataTreeview,
  PNGTools, ColorTypes, ColorTools;

//----------------------------------------------------------------------------------------------------------------------

constructor TRSTabSheet.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  Width := 650;
  Height := 500;
  SplitDirection := -1;
  FShowFieldOverlayImages := True;

  InitControls := False;

  RSPanels := TList.Create;
  Splitters := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TRSTabSheet.Destroy;

begin
  RSPanels.Free;
  Splitters.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.ActivateCompareMode;

var
  Result1,
  Result2: TMySQLRS;
  Grid1,
  Grid2: TMySQLRSGrid;

begin
  if RSPanels.Count = 2 then
  begin
    Result1 := TMySQLRSPanel(RSPanels[0]).MySQLRS;
    Result2 := TMySQLRSPanel(RSPanels[1]).MySQLRS;
    Grid1 := TMySQLRSPanel(RSPanels[0]).FRSGrid;
    Grid2 := TMySQLRSPanel(RSPanels[1]).FRSGrid;

    if myx_query_compare_possible(Result1.ResultSet, Result2.ResultSet) = 1 then
    begin
      // Clear old grid values
      Grid1.ClearValues;
      Grid2.ClearValues;

      // Compute the actual differences.
      myx_query_compare_results(Result1.ResultSet, Result2.ResultSet);

      Grid1.BuildColumns;
      Grid2.BuildColumns;

      Grid1.BuildRows(0, Result1.ResultSet.rows_num - 1, True, Result2);
      Grid2.BuildRows(0, Result2.ResultSet.rows_num - 1, True, nil);

      Grid1.CompareActive := True;
      Grid2.CompareActive := True;
      ParallelGridScroll := True;
    end
    else
      raise EMyxError.Create(_('The two resultsets cannot be compared. Please ' +
        'make sure that they have a primary key and the same column names.'));
  end
  else
    raise EMyxError.Create(_('Only two resultsets can be compared. Use the ' +
      '[Split Tab] function in the grid''s popup menu to get a second grid.'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.ActivateExplainPanel(MySQLConn: TMySQLConn);

// Activates the panel for the EXPLAIN result. If it does not yet exist then one is created implicitely.

var
  Panel: TMySQLRSPanel;
  
begin
  if not FHaveExplainPanel then
  begin
    Panel := AddRSPanel(MySQLConn);
    Panel.Height := 100;
    Panel.FRSNav.Visible := False;
    FHaveExplainPanel := True;
  end;
  ActiveRSPanel := RSPanels[RSPanels.Count - 1];
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSTabSheet.AddRSPanel(MySQLConn: TMySQLConn; SplitVertically: Boolean; DoFocus: Boolean;
  Size: Integer): TMySQLRSPanel;

var
  RSPanel, MasterRSPanel: TMySQLRSPanel;
  Splitter: TTntSplitter;
  ParentForm: TCustomForm;

begin
  if (RSPanels.Count >= 1) then
    if (SplitDirection > -1) and (Ord(SplitVertically) <> SplitDirection) then
    begin
      ShowModalDialog(_('Error'),
        _('You can split the panel only in one direction.'),
        myx_mtError, 'OK');

      Result := nil;

      Exit;
    end;

  RSPanel := TMySQLRSPanel.Create(self);
  RSPanel.Parent := self;
  RSPanel.CreateMySQLRS(MySQLConn);

  if not FHaveExplainPanel then
    RSPanels.Add(RSPanel)
  else
    // If there is already an explain panel then insert the new panel before that
    // so the explain panel always is the last one.
    RSPanels.Insert(RSPanels.Count - 1, RSPanel);

  if (RSPanels.Count > 1) then
  begin
    SplitDirection := Ord(SplitVertically);

    //connect this new ResultSet to the prior one
    MasterRSPanel := TMySQLRSPanel(RSPanels[RSPanels.Count - 2]);
    RSPanel.MySQLRS.ParentRS := MasterRSPanel.MySQLRS;

    //Add events from ParentRS
    RSPanel.MySQLRS.OnParamChange := MasterRSPanel.MySQLRS.OnParamChange;
    RSPanel.MySQLRS.OnQueryExecute := MasterRSPanel.MySQLRS.OnQueryExecute;
    RSPanel.MySQLRS.OnQueryExecuted := MasterRSPanel.MySQLRS.OnQueryExecuted;
    RSPanel.MySQLRS.OnQuerySuccess := MasterRSPanel.MySQLRS.OnQuerySuccess;    
    RSPanel.MySQLRS.OnQueryStopped := MasterRSPanel.MySQLRS.OnQueryStopped;
    RSPanel.MySQLRS.OnQueryError := MasterRSPanel.MySQLRS.OnQueryError;
    RSPanel.MySQLRS.OnConfirmDeletion := MasterRSPanel.MySQLRS.OnConfirmDeletion;
    RSPanel.MySQLRS.CreateNextRSForMultipleRSQuery :=
      MasterRSPanel.MySQLRS.CreateNextRSForMultipleRSQuery;
    RSPanel.MySQLRS.RemoveRSForMultipleRSQuery :=
      MasterRSPanel.MySQLRS.RemoveRSForMultipleRSQuery;
    RSPanel.MySQLRS.ShowRSForMultipleRSQuery :=
      MasterRSPanel.MySQLRS.ShowRSForMultipleRSQuery;

    RSPanel.MySQLRS.GlobalParams :=
      MasterRSPanel.MySQLRS.GlobalParams;

    RSPanel.FRSGrid.PopupMenu := MasterRSPanel.FRSGrid.PopupMenu;
    RSPanel.FRSGrid.OnFocusChanged := MasterRSPanel.FRSGrid.OnFocusChanged;
    RSPanel.FRSGrid.OnDropTables := MasterRSPanel.FRSGrid.OnDropTables;

    Splitter := TTntSplitter.Create(self);
    Splitter.Parent := self;
    Splitter.MinSize := 50;
    Splitter.AutoSnap := False;

    Splitters.Add(Splitter);

    if (SplitVertically) then
    begin
      if Size=-1 then
      begin
        RSPanel.Height := MasterRSPanel.Height div 2;
        MasterRSPanel.Height := MasterRSPanel.Height div 2;
      end
      else
      begin
        RSPanel.Height := Size;
        MasterRSPanel.Height := MasterRSPanel.Height - Size;
      end;

      RSPanel.Align := alBottom;
      Splitter.Align := alBottom;

      if Size=0 then
        Splitter.Height := 0
      else
        Splitter.Height := 9;
      Splitter.Top := MasterRSPanel.Top + MasterRSPanel.Height + 10;
      RSPanel.Top := MasterRSPanel.Top + MasterRSPanel.Height + 10;

      if (RSPanel.Height < 50) then
      begin
        MasterRSPanel.Height := MasterRSPanel.Height - 200;
        RSPanel.Height := 200;
      end;
    end
    else
    begin
      RSPanel.Width := MasterRSPanel.Width div 2;
      MasterRSPanel.Width := MasterRSPanel.Width div 2;

      RSPanel.Align := alRight;
      Splitter.Align := alRight;

      Splitter.Width := 11;
      Splitter.Left := MasterRSPanel.Left + MasterRSPanel.Width + 12;
      RSPanel.Left := MasterRSPanel.Left + MasterRSPanel.Width + 12;
    end;
  end
  else
    RSPanel.Align := alClient;

  if DoFocus then
  begin
    // Set focus to new RSPanel if told so.
    ParentForm := GetParentForm(RSPanel);
    if (ParentForm <> nil) then
      if (ParentForm.Visible and ParentForm.Enabled) then
        RSPanel.SetFocus;

    RSPanel.Active := True;
  end;

  Result := RSPanel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.DeleteRSPanel(RSPanel: TMySQLRSPanel);

var
  Index: Integer;
  Splitter: TTntSplitter;
begin
  if RSPanels.Count > 1 then
  begin
    Index := RSPanels.IndexOf(RSPanel);

    if (Index > 0) then
    begin
      Splitter := Splitters[Index - 1];
      Splitters.Delete(Index - 1);
      Splitter.Free;

      //Assign RSParent from one before
      if (Index < RSPanels.Count - 1) then
        TMySQLRSPanel(RSPanels[Index + 1]).MySQLRS.ParentRS := TMySQLRSPanel(RSPanels[Index - 1]).MySQLRS
      else
        FHaveExplainPanel := False; // If this was the EXPLAIN panel then reset our flag (if not, it does not harm).

      RSPanels.Delete(Index);
      RSPanel.Free;

      FActiveRSPanel := nil;
      ActiveRSPanel := RSPanels[Index - 1];
      ActiveRSPanel.Active := True;
    end
    else
    begin
      Splitter := Splitters[0];
      Splitters.Delete(0);
      Splitter.Free;

      TMySQLRSPanel(RSPanels[1]).MySQLRS.ParentRS := nil;

      RSPanels.Delete(0);
      RSPanel.Free;

      FActiveRSPanel := nil;
      ActiveRSPanel := RSPanels[0];
      ActiveRSPanel.Align := alClient;
      ActiveRSPanel.Active := True;
    end;
  end;

  if (RSPanels.Count <= 1) then
    SplitDirection := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSTabSheet.GetActiveRSPanel: TMySQLRSPanel;

begin
  Result := FActiveRSPanel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.SetActiveRSPanel(ActiveRSPanel: TMySQLRSPanel);

var
  I: Integer;
  PrevSelPanel: TMySQLRSPanel;

begin
  if (FActiveRSPanel <> ActiveRSPanel) then
  begin
    PrevSelPanel := FActiveRSPanel;

    if (Assigned(FOnPanelChanging)) then
      FOnPanelChanging(FActiveRSPanel);

    FActiveRSPanel := ActiveRSPanel;

    for I := 0 to RSPanels.Count - 1 do
      TMySQLRSPanel(RSPanels[I]).Active := RSPanels[I] = FActiveRSPanel;

    {if(Assigned(FActiveRSPanel.MySQLRS.OnParamChange))then
      FActiveRSPanel.MySQLRS.OnParamChange(ActiveRSPanel.MySQLRS);}

    if (Assigned(FOnPanelChanged)) then
      FOnPanelChanged(PrevSelPanel, ActiveRSPanel);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSTabSheet.GetParallelGridScroll: Boolean;

begin
  Result := FParallelGridScroll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.SetParallelGridScroll(ParallelGridScroll: Boolean);

var
  i: Integer;

begin
  if (FParallelGridScroll) and (not (ParallelGridScroll)) then
  begin
    for i := 0 to RSPanels.Count - 1 do
      TMySQLRSPanel(RSPanels[i]).FRSGrid.OnScroll := nil;
  end;

  FParallelGridScroll := ParallelGridScroll;

  if (FParallelGridScroll) then
  begin
    for i := 0 to RSPanels.Count - 1 do
      TMySQLRSPanel(RSPanels[i]).FRSGrid.OnScroll :=
        TMySQLRSPanel(RSPanels[i]).FRSGrid.DoGridScroll;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSTabSheet.SetShowFieldOverlayImages(ShowImgs: Boolean);

var
  i: Integer;

begin
  FShowFieldOverlayImages := ShowImgs;

  for i := 0 to RSPanels.Count - 1 do
    TMySQLRSPanel(RSPanels[i]).FRSGrid.ShowFieldOverlayImages :=
      ShowImgs;
end;

//----------------- TMySQLRSPanel --------------------------------------------------------------------------------------

constructor TMySQLRSPanel.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  BevelOuter := bvNone;
  Caption := '';

  FActive := True;

  //Create RS Grid
  FRSGrid := TMySQLRSGrid.Create(self);
  FRSGrid.Parent := self;
  FRSGrid.Font.Name := MYXCommonOptions.DataFontName;
  FRSGrid.Header.Font.Name := MYXCommonOptions.DataFontName;
  FRSGrid.Align := alClient;
  FRSGrid.OnEnter := DoChildControlEnter;

  //Create RS Navigator
  FRSNav := TMySQLRSNav.Create(self);
  FRSNav.Parent := self;
  FRSNav.Align := alBottom;
  FRSNav.OnEnter := DoChildControlEnter;

  //Create RS Error Grid
  FRSErrorGrid := TMySQLRSErrorGrid.Create(self);
  FRSErrorGrid.Parent := self;
  FRSErrorGrid.Font.Name := MYXCommonOptions.DataFontName;
  FRSErrorGrid.Header.Font.Name := MYXCommonOptions.DataFontName;
  FRSErrorGrid.OnEnter := DoChildControlEnter;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLRSPanel.Destroy;

begin
  FMySQLRS.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSPanel.GetMySQLRS: TMySQLRS;

begin
  Result := FMySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSPanel.SetMySQLRS(MySQLRS: TMySQLRS);

begin
  FMySQLRS := MySQLRS;
  FRSGrid.MySQLRS := MySQLRS;
  FRSNav.MySQLRS := MySQLRS;
  FRSErrorGrid.MySQLRS := MySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSPanel.CreateMySQLRS(MySQLConn: TMySQLConn);

begin
  FMySQLRS := TMySQLRS.Create;
  FMySQLRS.MySQLConn := MySQLConn;

  FRSGrid.MySQLRS := MySQLRS;
  FRSNav.MySQLRS := MySQLRS;
  FRSErrorGrid.MySQLRS := MySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSPanel.GetActive: Boolean;

begin
  Result := FActive;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSPanel.SetActive(Active: Boolean);

begin
  FActive := Active;

  FRSGrid.Active := FActive;
  FRSNav.Active := FActive;

  // Deactivate other RSPanels if this one got active.
  if (FActive) and (Parent.InheritsFrom(TRSTabSheet)) then
    TRSTabSheet(Parent).ActiveRSPanel := self;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSPanel.DoChildControlEnter(Sender: TObject);

begin
  Active := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSPanel.GetTotalHeight: Integer;

begin
  Result := Height;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSPanel.SetTotalHeight(Height: Integer);

var
  i: Integer;
  Tab: TRSTabSheet;

begin
  if (Height=0) and (Parent is TRSTabSheet) then
  begin
    Tab := TRSTabSheet(Parent);

    i := Tab.Splitters.IndexOf(self);
    if (i>-1) and (i<Tab.Splitters.Count-1-1) then
      TTntSplitter(Tab.Splitters[i-1]).Height := 9;
  end;

  self.Height := Height;
end;


//----------------- TCaptionQueue ------------------------------------------------------------------------------------

constructor TCaptionQueue.Create(Limit: Integer);

begin
  inherited Create;
  FLimit := Limit;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCaptionQueue.PushItem(AItem: Pointer);

var
  Entry: PRSRowData;

begin
  // If this entry already exists then move it to the most recent position in the queue.
  List.Remove(AItem);
  while List.Count >= FLimit do
  begin
    Entry := Pop;
    Finalize(Entry^);
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCaptionQueue.Remove(Entry: Pointer);

begin
  Finalize(PRSRowData(Entry)^);
  List.Remove(Entry);
end;

//----------------- TMySQLRSGrid ---------------------------------------------------------------------------------------

constructor TMySQLRSGrid.Create(AOwner: TComponent);

var
  MenuItem: TMenuItem;

begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TRSRowData);

  Header.Options := Header.Options + [hoDblClickResize, hoVisible];
  Font.Name := MYXCommonOptions.DataFontName;
  Font.Height := MYXCommonOptions.DataFontHeight;

  // Add 8 pixels, which correspond to 4 pixels distance above and below the text to the cell border.
  DefaultNodeHeight := Abs(MYXCommonOptions.DataFontHeight) + 8;
  Header.Font := Font;
  Header.Font.Height := MYXCommonOptions.DataFontHeight;
  Header.Height := 2 * Abs(Header.Font.Height);
  Header.Options := [hoColumnResize, hoDrag, hoHotTrack, hoVisible, hoShowHint, hoShowImages, hoShowImages];

  SetLength(FColumnIndicators, 3);
  FColumnIndicators[0] := LoadPNGImageFromResource('column_indicator');
  FColumnIndicators[1] := LoadPNGImageFromResource('column_indicator_pk');
  FColumnIndicators[2] := LoadPNGImageFromResource('column_indicator_fk');

  // Dummy image list (bug in VT: draw code does not consider correct image width in advanced custom draw mode).
  Header.Images := TImageList.Create(Self);
  Header.Images.Width := 11;
  Header.Images.Height := 11;

  ComputeRowColors;
  ShowHint := False;
  HintMode := hmTooltip;
  LineStyle := lsSolid;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable, toGridExtensions] - [toToggleOnDblClick];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowHorzGridLines, toShowVertGridLines] -
    [toShowRoot, toShowTreeLines, toShowDropmark];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus, toRightClickSelect, toMultiSelect,
    toDisableDrawSelection];
  TreeOptions.StringOptions := TreeOptions.StringOptions - [toSaveCaptions];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toVariableNodeHeight];
  WantTabs := True;

  Header.Options := Header.Options  + [hoOwnerDraw];
  OnHeaderDrawQueryElements := HeaderDrawQueryElements;
  OnAdvancedHeaderDraw := AdvancedHeaderDraw;

  FActive := True;
  FMRUCaptions := TCaptionQueue.Create(100);

  NodesWithActionList := TList.Create;
  NodesWithDeleteActionList := TList.Create;

  FShowFieldOverlayImages := True;
  FHotRSNode := nil;
  FHotRSColumn := -1;

  FLastSearchNode := nil;
  FLastSearchCol := 0;
  FLastSearchText := '';

  FieldClearPNGImg := LoadPNGImageFromResource('field_overlay_clear');
  FieldEditPNGImg := LoadPNGImageFromResource('field_overlay_edit');
  FieldLoadPNGImg := LoadPNGImageFromResource('field_overlay_load');
  FieldSavePNGImg := LoadPNGImageFromResource('field_overlay_save');
  FieldViewPNGImg := LoadPNGImageFromResource('field_overlay_view');
  FieldNullPNGImg := LoadPNGImageFromResource('field_overlay_null');
  BlobPNGImg := LoadPNGImageFromResource('blob_icon');

  HeaderPopupMenu := TPopupMenu.Create(self);
  MenuItem := TMenuItem.Create(HeaderPopupMenu);
  MenuItem.Caption := _('Copy Column Names');
  MenuItem.OnClick := CopyColumnNames;
  HeaderPopupMenu.Items.Add(MenuItem);

  Header.PopupMenu := HeaderPopupMenu;

  RefreshOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLRSGrid.Destroy;

var
  I: Integer;
  NodeData: PRSRowData;
  Provider: IOptionProvider;

begin
  Provider := GetOptionProvider;
  if Assigned(Provider) then
    Provider.RemoveListener(Self);

  MySQLRS := nil;

  for I := 0 to High(FColumnIndicators) do
    FColumnIndicators[I].Free;

  for I := 0 to NodesWithActionList.Count - 1 do
  begin
    NodeData := GetNodeData(PVirtualNode(NodesWithActionList[I]));
    NodeData.actions.Free;
  end;

  FMRUCaptions.Free;
  NodesWithActionList.Free;
  NodesWithDeleteActionList.Free;

  FieldClearPNGImg.Free;
  FieldEditPNGImg.Free;
  FieldLoadPNGImg.Free;
  FieldSavePNGImg.Free;
  FieldViewPNGImg.Free;
  FieldNullPNGImg.Free;
  BlobPNGImg.Free;

  HeaderPopupMenu.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.ClearValues;

begin
  BeginUpdate;
  try
    Clear;
    Header.Columns.Clear;
    FHotRSNode := nil;
    FHotRSColumn := NoColumn;

    NodesWithActionList.Clear;
    NodesWithDeleteActionList.Clear;
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.SetCompareActive(const Value: Boolean);

begin
  if FCompareActive <> Value then
  begin
    FCompareActive := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ConvertText(const Input: PChar; const Length: Integer): WideString;

// Converts the given input from UTF-8 to UTF-16 and replaces every non-displayable character with an error char.

var
  ActualLength: Integer;

begin
  ActualLength := MultiByteToWideChar(CP_UTF8, 0, Input, Length, nil, 0);
  SetLength(Result, ActualLength);
  MultiByteToWideChar(CP_UTF8, 0, Input, Length, PWideChar(Result), ActualLength);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.InternalGetNodeRSText(Node: PVirtualNode; Column: TColumnIndex): WideString;

var
  action: PMYX_RS_ACTION;
  field: PMYX_RS_FIELD;
  ValueFromAction: Boolean;
  col: PMYX_RS_COLUMN;

begin
  Result := '';

  MySQLRS.ResultsetLock.Acquire;
  try
    action := GetNodeRSAction(Node, Column);
    ValueFromAction := False;

    if (Column > 0) and (Column - 1 < Integer(MySQLRS.ResultSet.columns_num)) then
      col := PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + sizeof(MYX_RS_COLUMN) * (Column - 1))
    else
      col := nil;

    if Assigned(action) then
      if (action.action <> MYX_RSA_DELETE) then
      begin
        if Assigned(col) then
        begin
          if (col^.column_type <> MYX_RSCT_BLOB) then
            Result := ConvertText(action.new_value, action.new_value_length);
        end
        else
          Result := ConvertText(action.new_value, action.new_value_length);
        ValueFromAction := True;
      end
      else
      begin
        action := GetNodeRSAction(Node, Column, True);
        if Assigned(action) then
        begin
          if Assigned(col) then
          begin
            if (col^.column_type <> MYX_RSCT_BLOB) then
              Result := ConvertText(action.new_value, action.new_value_length);
          end
          else
            Result := ConvertText(action.new_value, action.new_value_length);
          ValueFromAction := True;
        end;
      end;

    if not ValueFromAction then
    begin
      field := GetNodeRSField(Node, Column);
      if Assigned(field) and Assigned(field.value) and (col.column_type <> MYX_RSCT_BLOB) then
        Result := ConvertText(field.value, field.value_length);
    end;
  finally
    MySQLRS.ResultsetLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetMySQLRS: TMySQLRS;

begin
  Result := FMySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.SetMySQLRS(MySQLRS: TMySQLRS);

begin
  if (FMySQLRS <> nil) and (MySQLRS = nil) then
    FMySQLRS.DisconnectControl(self);

  FMySQLRS := MySQLRS;

  if (MySQLRS <> nil) then
    MySQLRS.ConnectControl(self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetControl: TObject;

begin
  Result := self;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetActive: Boolean;

begin
  Result := FActive;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.SetActive(Active: Boolean);

begin
  FActive := Active;

  if (Header.Columns.Count > 0) then
  begin
    if (FActive) then
      Header.Columns[0].Color := clBtnFace
    else
      Header.Columns[0].Color := $00E0E0E0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoCurrentRowChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoEditStateChanged;

var
  I: Integer;
  NodeData: PRSRowData;
  action: PMYX_RS_ACTION;

begin
  // Accept any pending edit action.
  DoEndEdit;
  
  // If actions have been removed, remove them from nodes, too.
  if (FMySQLRS.ResultSet.actions = nil) then
  begin
    for I := 0 to NodesWithActionList.Count - 1 do
    begin
      NodeData := GetNodeData(PVirtualNode(NodesWithActionList[I]));
      NodeData.actions.Free;
      NodeData.actions := nil;
    end;

    NodesWithActionList.Clear;
  end
  else
  begin
    I := 0;
    while (I < NodesWithActionList.Count) do
    begin
      NodeData := GetNodeData(PVirtualNode(NodesWithActionList[I]));

      //Check if last action is MYX_RSA_DISCARDED
      action := nil;
      if (NodeData.actions <> nil) then
        if (NodeData.actions.Count > 0) then
          action := PMYX_RS_ACTION(Integer(MySQLRS.ResultSet.actions.actions) +
            sizeof(MYX_RS_ACTION) * NodeData.actions[NodeData.actions.Count - 1]);

      //If it is, remove action from NodeData
      if (action <> nil) then
      begin
        if (action.status = MYX_RSAS_DISCARDED) then
        begin
          NodeData.actions.Free;
          NodeData.actions := nil;
          NodeData.Captions := nil;
          NodeData.FriendlyLinebreaksCaptions := nil;

          NodesWithActionList.Delete(I);
        end
        else
          inc(I);
      end
      else
        inc(I);
    end;
  end;

  //changes may be discarded, so redraw
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoStatusCaptionChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.FinishedBuilding;

// Called when the grid was just built (by the query thread). Can be used to set certain states
// of the grid once it was loaded.

begin
  if FAutoEdit then
    FMySQLRS.DoEdit;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoMessagesChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.BuildColumns;

var
  VTColumn: TVirtualTreeColumn;
  RSColumn: PMYX_RS_COLUMN;
  QueryColumn: PMYX_Q_TABLE_COLUMN;
  I: Integer;
  W,
    C: Integer;

begin
  Header.Columns.Clear;
  FHotRSColumn := NoColumn;

  VTColumn := Header.Columns.Add;
  VTColumn.Width := 15;
  VTColumn.Color := clBtnFace;
  VTColumn.Options := [coVisible];

  FocusedNode := nil;

  RSColumn := FMySQLRS.ResultSet.columns;
  QueryColumn := RSColumn.table_column;
  for I := 0 to FMySQLRS.ResultSet.columns_num_to_display - 1 do
  begin
    VTColumn := Header.Columns.Add;
    VTColumn.Text := UTF8Decode(RSColumn.name);
    VTColumn.Hint := VTColumn.Text;
    if Assigned(QueryColumn) and LongBool(QueryColumn.is_pk) then
      VTColumn.ImageIndex := 1;

    C := 10;
    case RSColumn.column_type of
      MYX_RSCT_INTEGER:
        C := 10;
      MYX_RSCT_STRING:
        C := RSColumn.type_size;
      MYX_RSCT_TEXT:
        C := 25;
      MYX_RSCT_DATE:
        C := 10;
      MYX_RSCT_DATETIME:
        C := 19;
      MYX_RSCT_BLOB:
        VTColumn.Width := 90
    end;

    W := C * ((Abs(Font.Height) * 10) div 13) + 16;
    if W > 300 then
      VTColumn.Width := 300
    else
      VTColumn.Width := W;


    if (FAlignNumericColsRight) and
      ((RSColumn.column_type = MYX_RSCT_INTEGER) or
        (RSColumn.column_type = MYX_RSCT_FLOAT) or
        (RSColumn.column_type =MYX_RSCT_DECIMAL)) then
      VTColumn.Alignment := taRightJustify;

    Inc(RSColumn);
    QueryColumn := RSColumn.table_column;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.BuildRows(StartRow: Integer; EndRow: Integer; SetFocusOnFirstNode: Boolean; RS: TMySQLRS);

var
  i: Integer;
  Node: PVirtualNode;
  Nodedata: PRSRowData;

begin
  Node := GetLast;
  RootNodeCount := Integer(RootNodeCount) + EndRow - StartRow + 1;

  if Node = nil then
    Node := GetFirst
  else
    Node := GetNextNoInit(Node);
  I := StartRow;
  while Node <> nil do
  begin
    NodeData := GetNodeData(Node);
    // We need a separate index in case rows are deleted.
    NodeData.row_index := I;
    Inc(I);
    Node := GetNextNoInit(Node);
  end;
  if SetFocusOnFirstNode then
  begin
    FocusedNode := GetFirst;
    Selected[FocusedNode] := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.BuildRowForAppending;

var
  Node: PVirtualNode;
  Nodedata: PRSRowData;

begin
  //Only append row if there is a resultset and
  //the resultset is editable
  if (FMySQLRS.ResultSet = nil) or (not (FMySQLRS.Editable)) then
    Exit;

  BeginUpdate;
  try
    Node := AddChild(nil);
    if (Node <> nil) then
    begin
      NodeData := GetNodeData(Node);
      if (NodeData <> nil) then
      begin
        NodeData.row_index := FMySQLRS.ResultSet.rows_num;
        NodeData.actions := nil;
      end
      else
        raise EInOutError.Create('NodeData=nil');
    end
    else
      raise EInOutError.Create('Node=nil');
  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetNodeRSAction(Node: PVirtualNode; Column: TColumnIndex; IgnoreDeleteActions: Boolean):
  PMYX_RS_ACTION;

var
  NodeData: PRSRowData;
  action: PMYX_RS_ACTION;
  action_Column: TColumnIndex;
  I: Integer;

begin
  action := nil;

  NodeData := GetNodeData(Node);

  if (NodeData.actions <> nil) then
  begin
    for i := NodeData.actions.Count - 1 downto 0 do
    begin
      action := PMYX_RS_ACTION(Integer(MySQLRS.ResultSet.actions.actions) + sizeof(MYX_RS_ACTION) *
        NodeData.actions[i]);

      action_column := (Integer(action.column) - Integer(MySQLRS.ResultSet.columns)) div sizeof(MYX_RS_COLUMN);

      //Check column of the action
      if (Column - 1 = action_column) and
        ((not (IgnoreDeleteActions)) or (action.action <> MYX_RSA_DELETE)) then
        break
      else
        if (action_column = 0) and (action.action = MYX_RSA_DELETE) and
          (not (IgnoreDeleteActions)) then
          break
        else
          action := nil;
    end;
  end;

  Result := action;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetNodeRSRow(Node: PVirtualNode): PMYX_RS_ROW;

var
  NodeData: PRSRowData;
  row: PMYX_RS_ROW;

begin
  row := nil;

  NodeData := GetNodeData(Node);
  if (NodeData <> nil) then
  begin
    //Consider added rows
    if (NodeData.row_index < FMySQLRS.ResultSet.rows_num) then
      row := PMYX_RS_ROW(Integer(MySQLRS.ResultSet.rows) + sizeof(MYX_RS_ROW) * Integer(NodeData.row_index));
  end;

  Result := row;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetNodeRSField(Node: PVirtualNode; Column: TColumnIndex): PMYX_RS_FIELD;

var
  NodeData: PRSRowData;
  RowData: PMYX_RS_ROW;
  field: PMYX_RS_FIELD;

begin
  field := nil;

  NodeData := GetNodeData(Node);
  if (NodeData <> nil) then
  begin
    //Consider added rows
    if (NodeData.row_index < FMySQLRS.ResultSet.rows_num) then
    begin
      RowData := PMYX_RS_ROW(Integer(MySQLRS.ResultSet.rows) + sizeof(MYX_RS_ROW) * Integer(NodeData.row_index));

      //Consider diff rows
      try
        if (RowData.fields <> nil) then
          field := PMYX_RS_FIELD(Integer(RowData.fields) + (SizeOf(MYX_RS_FIELD) * (Column - 1)));
      except
        field := nil;
      end;
    end;
  end;

  Result := field;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetNodeRSValue(Node: PVirtualNode; Column: TColumnIndex): TRSFieldValue;

var
  action: PMYX_RS_ACTION;
  field: PMYX_RS_FIELD;
  ValueFromAction: Boolean;
  RSValue: TRSFieldValue;
  col: PMYX_RS_COLUMN;

begin
  RSValue.Length := 0;
  RSValue.IsNull := True;

  MySQLRS.ResultsetLock.Acquire;
  try
    if (Column > 0) and (Column - 1 < Integer(MySQLRS.ResultSet.columns_num)) then
      col := PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + sizeof(MYX_RS_COLUMN) * (Column - 1))
    else
      Exit;

    RSValue.BinaryData := (col^.column_type = MYX_RSCT_BLOB);

    action := GetNodeRSAction(Node, Column);
    ValueFromAction := False;

    if (action <> nil) then
      if (action.action <> MYX_RSA_DELETE) then
      begin
        RSValue.Value := action^.new_value;
        RSValue.Length := action^.new_value_length;
        if (RSValue.Value<>nil) then
          RSValue.IsNull := False;
        ValueFromAction := True;
      end
      else
      begin
        action := GetNodeRSAction(Node, Column, True);
        if (action <> nil) then
        begin
          RSValue.Value := action^.new_value;
          RSValue.Length := action^.new_value_length;
          if (RSValue.Value<>nil) then
            RSValue.IsNull := False;

          ValueFromAction := True;
        end;
      end;

    if (not (ValueFromAction)) then
    begin
      field := GetNodeRSField(Node, Column);
      if (field <> nil) then
        if (field.value <> nil) then
        begin
          RSValue.Value := field^.value;
          RSValue.Length := field^.value_length;
          RSValue.IsNull := False;
        end;
    end;

    Result := RSValue;
  finally
    MySQLRS.ResultsetLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{$ifndef DELPHI_10_UP}
type
  TWideStringReplaceFunc = function(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags;
    WholeWord: Boolean = False): WideString;

var
  // TODO: check necessity of this code once the next TNT package is out.
  WideStringReplace: TWideStringReplaceFunc;
{$endif DELPHI_10_UP}

function TMySQLRSGrid.GetNodeRSText(Node: PVirtualNode; Column: TColumnIndex; ForEdit: Boolean): WideString;

var
  NodeData: PRSRowData;

begin
  NodeData := GetNodeData(Node);
  if High(NodeData.Captions) < Column then
    SetLength(NodeData.Captions, Column + 1);
  if High(NodeData.FriendlyLinebreaksCaptions) < Column then
    SetLength(NodeData.FriendlyLinebreaksCaptions, Column + 1);

  if (NodeData.Captions[Column] = '') or (NodeData.FriendlyLinebreaksCaptions[Column] = '') then
  begin
    if NodeData.Captions[Column] = '' then
    begin
      NodeData.Captions[Column] := InternalGetNodeRSText(Node, Column);
      FMRUCaptions.Push(NodeData);
    end;

    if NodeData.FriendlyLinebreaksCaptions[Column] = '' then
    begin
      if FFriendlyLineBreaks then
      begin
        NodeData.FriendlyLinebreaksCaptions[Column] := WideStringReplace(NodeData.Captions[Column], #13,
          FFriendlyLineBreaksCR, [rfReplaceAll]);
        NodeData.FriendlyLinebreaksCaptions[Column] := WideStringReplace(NodeData.FriendlyLinebreaksCaptions[Column],
          #10,
          FFriendlyLineBreaksLF, [rfReplaceAll]);
      end
      else
        NodeData.FriendlyLinebreaksCaptions[Column] := NodeData.Captions[Column];
    end;
  end;
  if ForEdit then
    Result := NodeData.Captions[Column]
  else
    Result := NodeData.FriendlyLineBreaksCaptions[Column];
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetOptionProvider: IOptionProvider;

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

procedure TMySQLRSGrid.ComputeRowColors;

// Computes the background colors for the rows in the result set. Special care must be taken to produce
// readable colors even on machines with odd color settings.

var
  MainColor: TRGB;
  AlternativeColor: THLS;
  HLS: THLS;

begin
  // The way used is to look for the background window color. If it is darker than a
  // medium color then lighten the alternative color, otherwise darken it.
  MainColor := MakeRGB(clWindow);
  HLS := RGBToHLS(MainColor);
  AlternativeColor := HLS;

  // If the luminance is close to the middle then we just offset it a bit otherwise
  // compute a new luminance that is shifted towards the middle luminance.
  if Abs(HLS.L - 0.5) <= 0.125 then
    AlternativeColor.L := AlternativeColor.L + 0.0625
  else
    AlternativeColor.L := HLS.L + (0.5 - HLS.L) * 0.125;
  FMainRowColor := clWindow;
  FAlternativeRowColor := MakeColorRef(HLSToRGB(AlternativeColor));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoDragDrop(Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);

// Handles drop actions on a result grid.
// Currently only the catalog treeview is supported as drag source, and only table names dropped on the grid are
// considered. A list of these names will be created and passed on to the current OnDropTables event handler.

var
  SchemaInfo: ISchemaInfo;
  TableList: TWideStringList;

begin
  inherited;

  if Assigned(FOnDropTables) and (Source is TCustomVirtualStringTree) then
  begin
    // Activate result grid that got the tables dropped onto.
    SetFocus;

    // TObject itself is not compatible to any interface, so we cannot use compiler magic to
    // assign it to an interface variable.
    TCustomVirtualStringTree(Source).Owner.GetInterface(ISchemaInfo, SchemaInfo);
    if Assigned(SchemaInfo) then
    begin
      TableList := TWideStringList.Create;
      try
        SchemaInfo.GetSelectedTables(TableList);
        if Assigned(FOnDropTables) then
          FOnDropTables(Self, TableList, Shift);
      finally
        TableList.Free;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.DoDragOver(Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer): Boolean;

var
  SchemaInfo: ISchemaInfo;
  
begin
  if Assigned(OnDragOver) then
    Result := inherited DoDragOver(Source, Shift, State, Pt, Mode, Effect) and (Source is TCustomVirtualStringTree)
  else
    Result := Source is TCustomVirtualStringTree;
  if Result then
  begin
    TCustomVirtualStringTree(Source).Owner.GetInterface(ISchemaInfo, SchemaInfo);
    Result := Assigned(SchemaInfo);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text:
  WideString);

var
  ForEditing: Boolean;

begin
  if (Assigned(OnGetText)) then
    OnGetText(Self, Node, Column, TextType, Text)
  else
  begin
    if (Column = 0) then
    begin
      Text := '';
    end
    else
      if (MySQLRS.ResultSet <> nil) then
      begin
        ForEditing := (tsEditing in TreeStates) and (Column = EditColumn) and (Node = FocusedNode);
        if (Column - 1 < Integer(MySQLRS.ResultSet.columns_num)) then
          Text := GetNodeRSText(Node, Column, ForEditing)
        else
          Text := '';
      end
      else
        Text := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.DoFocusChanging(OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex): Boolean;

begin
  Result := True;

  if (NewColumn = 0) then
    Result := False
  else
  begin
    FLastSearchNode := NewNode;
    FLastSearchCol := NewColumn;

    if Assigned(OnFocusChanging) then
      OnFocusChanging(Self, OldNode, NewNode, OldColumn, NewColumn, Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

begin
  Allowed := False;

  if (FMySQLRS.EditingAllowed) then
    Allowed := True;

  if Assigned(OnEditing) then
    OnEditing(Self, Node, Column, Allowed);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.KeyDown(var Key: Word; Shift: TShiftState);

begin
  if (Key = VK_Return) and (FocusedNode <> nil) then
    EditNode(FocusedNode, FocusedColumn)
  else
    if (Key = VK_F2) and (FMySQLRS.EditingAllowed = False) then
      FMySQLRS.DoEdit
    else
      if (Key = Ord('F')) and (Shift = [ssCtrl]) then
        FMySQLRS.DoDisplaySearch
      else
        if (Key = Ord('R')) and (Shift = [ssCtrl]) then
          FMySQLRS.DoDisplaySearch(True)
        else
          if (Assigned(OnKeyDown)) then
            OnKeyDown(self, Key, Shift);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: PRSRowData;
  RowIndex: Integer;

begin
  NodeData := GetNodeData(Node);
  if (NodeData <> nil) then
    RowIndex := NodeData.row_index
  else
    RowIndex := -1;

  //Notify RS that current row has changed
  FMySQLRS.DoCurrentRowChanged(self, RowIndex);

  if (Column = 0) then
    FocusedColumn := Header.Columns.Count - 1;

  if (Assigned(OnFocusChanged)) then
    OnFocusChanged(self, Node, Column);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoFreeNode(Node: PVirtualNode);

var
  NodeData: PRSRowData;

begin
  NodeData := GetNodeData(Node);
  // NodeData is finalized in the cache class.
  FMRUCaptions.Remove(NodeData);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.AddAction(action_type: MYX_RS_ACTION_TYPE; row: Cardinal; Column: TColumnIndex; Node:
  PVirtualNode;
  new_value: WideString; new_value_length: Integer; new_binary_value: string);

var
  NewNode: PVirtualNode;
  NodeData: PRSRowData;
  action_index: Integer;
  new_action: PMYX_RS_ACTION;
  col: PMYX_RS_COLUMN;
  len: Integer;
  s: UTF8String;
  FirstNodeAction: Boolean;

begin
  NodeData := nil;
  if (Node <> nil) then
    NodeData := GetNodeData(Node);

  if (NodeData <> nil) and (Column > 0) and
    (Column - 1 < Integer(FMySQLRS.ResultSet.columns_num)) then
  begin
    col := (PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + (sizeof(MYX_RS_COLUMN) * (Column - 1))));

    //if length=-1 then get length from WideString
    //if length=-2 it is a NULL value
    //Otherwise we have binary data
    if (new_value_length = -1) then
    begin
      s := UTF8Encode(new_value);
      len := Length(s);
      new_action := myx_query_create_action(
        action_type, row, col, len, PChar(s));
    end
    else if (new_value_length = -2) then
    begin
      new_action := _myx_query_create_action(
        action_type, row, col, 0, nil);
    end
    else
    begin
      new_action := _myx_query_create_action(
        action_type, row, col, new_value_length, PChar(new_binary_value));
    end;

    try
      //Add action to node (maybe overwrite older actions)
      action_index := myx_query_add_action(FMySQLRS.ResultSet, new_action);
    finally
      myx_query_free_action(new_action);
    end;

    //Add action
    if (NodeData.actions = nil) then
    begin
      NodeData.actions := TIntegerList.Create;
      FirstNodeAction := True;
    end
    else
      FirstNodeAction := False;

    NodeData.actions.Add(action_index);

    //Add this Node to list of NodesWithAction
    NodesWithActionList.Add(Node);
    if (action_type = MYX_RSA_DELETE) then
      NodesWithDeleteActionList.Add(Node);

    //if this was the first action in the additional row for inserts,
    //add another row
    if (NodeData.row_index = FMySQLRS.ResultSet.rows_num + FMySQLRS.InsertedRows) and
      (FirstNodeAction) then
    begin
      FMySQLRS.InsertedRows := FMySQLRS.InsertedRows + 1;

      //Insert new row
      NewNode := AddChild(nil);

      NodeData := GetNodeData(NewNode);
      NodeData.row_index := FMySQLRS.ResultSet.rows_num + FMySQLRS.InsertedRows;
      NodeData.actions := nil;
    end;

    FMySQLRS.Edited := True;

    // Invalidate the caption for this cell so it gets reread the next time it is needed.
    if Column < Length(NodeData.Captions) then
      NodeData.Captions[Column] := '';
    if Column < Length(NodeData.FriendlyLinebreaksCaptions) then
      NodeData.FriendlyLinebreaksCaptions[Column] := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoNewText(Node: PVirtualNode; Column: TColumnIndex; Text: WideString);

var
  NodeData: PRSRowData;
  action_type: MYX_RS_ACTION_TYPE;

begin
  if (Assigned(OnNewText)) then
    OnNewText(Self, Node, Column, Text)
  else
  begin
    MySQLRS.ResultsetLock.Acquire;
    try
      NodeData := GetNodeData(Node);

      if Assigned(NodeData) and (Column > 0) and
        (Column - 1 < Integer(FMySQLRS.ResultSet.columns_num)) then
      begin
        if (NodeData.row_index < FMySQLRS.ResultSet.rows_num) then
          action_type := MYX_RSA_UPDATE
        else
          action_type := MYX_RSA_ADD;

        AddAction(action_type, NodeData.row_index, Column, Node, Text);

        // Invalidate the caption for this cell so it gets reread the next time it is needed.
        if Column < Length(NodeData.Captions) then
          NodeData.Captions[Column] := '';
        if Column < Length(NodeData.FriendlyLinebreaksCaptions) then
          NodeData.FriendlyLinebreaksCaptions[Column] := '';
      end;
    finally
      MySQLRS.ResultsetLock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

var
  Action: PMYX_RS_ACTION;
  Erase: Boolean;
  Row: PMYX_RS_ROW;
  ColumnMask: Int64;

begin
  inherited;

  // Column 0 holds the current row indicator.
  if Column > 0 then
  begin
    Erase := False;
    MySQLRS.ResultsetLock.Acquire;
    try
      // If comparation mode is active then first check for column differences and display them.
      if FCompareActive then
      begin
        Row := GetNodeRSRow(Node);
        if Assigned(Row) then
        begin
          Erase := True;
          // Highlight differences after compare.
          case Row.diff and $F of
            Ord(MYX_RD_THIS_ONLY): // Row exists only in left result set.
              Canvas.Brush.Color := $00E0FFE0;
            Ord(MYX_RD_OTHER_ONLY): // Row exists only in right result set.
              Canvas.Brush.Color := $00E0E0FF;
            Ord(MYX_RD_DIFFERS): // One or more columns differ.
              begin
                // The column bitmask is shifted 4 bits up to make room for the flag in the lowest nibble.
                // Since the first column (index 0) is used as indicator we have to take one bit less to shift.
                ColumnMask := 1 shl (Column + 3);
                if (Row.diff and ColumnMask) <> 0 then
                  Canvas.Brush.Color := $00FFE0E0;
              end;
          end;
        end
      end;

      // User actions override comparation results (if there are any).
      Action := GetNodeRSAction(Node, Column);

      if Assigned(Action) then
      begin
        Erase := True;
        case Action.status of
          MYX_RSAS_NEW:
          case Action.action of
            MYX_RSA_UPDATE:
              Canvas.Brush.Color := $00FFBBBB;
            MYX_RSA_ADD:
              Canvas.Brush.Color := $00BBFFBB;
            MYX_RSA_DELETE:
              Canvas.Brush.Color := $00BBBBFF;
          end;
          MYX_RSAS_APPLIED:
            Canvas.Brush.Color := $0080FF80;
          MYX_RSAS_FAILED:
            Canvas.Brush.Color := $000000FF;
          MYX_RSAS_DISCARDED:
            Canvas.Brush.Color := $00CCCCCC;
        else
          Erase := False;
        end;
      end;

      if Erase then
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(CellRect);
      end;
    finally
      MySQLRS.ResultsetLock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoBeforeItemErase(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var Color: TColor;
  var EraseAction: TItemEraseAction);

begin
  if Assigned(OnBeforeItemErase) then
    OnBeforeItemErase(Self, Canvas, Node, ItemRect, Color, EraseAction)
  else
  begin
    EraseAction := eaColor;

    if not FActive and not FCompareActive then
      Color := $00F0F0F0
    else
    begin
      // Use the standard background colors, alternating darker/lighter window color,
      // unless the node is one of many selected nodes.
      if (vsSelected in Node.States) and (SelectedCount > 1) then
        Color := Colors.FocusedSelectionColor
      else
      begin
        if (Node.Index mod 2) = 0 then
          Color := FMainRowColor
        else
          Color := FAlternativeRowColor;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType:
  TVSTTextType);

var
  Action: PMYX_RS_ACTION;

begin
  if (Assigned(OnPaintText)) then
    OnPaintText(Self, Canvas, Node, Column, TextType)
  else
  begin
    MySQLRS.ResultsetLock.Acquire;
    try
      Action := GetNodeRSAction(Node, Column);

      if Assigned(Action) then
      begin
        if Action.status <> MYX_RSAS_APPLIED then
        begin
          case Action.action of
            MYX_RSA_UPDATE:
              begin
                Canvas.Font.Color := clBlue;
              end;
            MYX_RSA_ADD:
              begin
                Canvas.Font.Color := clGreen;
              end;
          else
            Canvas.Font.Color := $00000000;
          end;
        end;

        // Make cell text with errors white on red, for better reading.
        if Action.status = MYX_RSAS_FAILED then
          Canvas.Font.Color := clWhite;
      end;
          
      if (vsSelected in Node.States) and (SelectedCount > 1) then
        Canvas.Font.Color := clWhite
    finally
      MySQLRS.ResultsetLock.Release;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

var
  mx, my: Integer;
  col: PMYX_RS_COLUMN;
  Rec: TRect;
  NotNull: Boolean;
  xpos: Integer;
  TransValue: Word;

begin
  if (Column = 0) and (Node = FocusedNode) then
  begin
    mx := (CellRect.Right - CellRect.Left) div 2;
    my := (CellRect.Bottom - CellRect.Top) div 2;
    with Canvas do
    begin
      if (Active) then
      begin
        Pen.Color := clBlack;
        Brush.Color := clBlack;
      end
      else
      begin
        Pen.Color := $00C0C0C0;
        Brush.Color := $00C0C0C0;
      end;

      Polygon([Point(mx - 3, my - 5), Point(mx + 2, my), Point(mx - 3, my + 5)]);
    end;
  end
  else
    if (FShowFieldOverlayImages) then
    begin
      MySQLRS.ResultsetLock.Acquire;
      try
        if (Column > 0) and (Column - 1 < Integer(MySQLRS.ResultSet.columns_num)) then
          col := PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + sizeof(MYX_RS_COLUMN) * (Column - 1))
        else
          col := nil;

        if (col <> nil) then
        begin
          NotNull := Not(GetNodeRSValue(Node, Column).IsNull);

          //Blob fields
          if col.column_type in [MYX_RSCT_TEXT, MYX_RSCT_BLOB] then
          begin
            if NotNull then
            begin
              if (col^.column_type = MYX_RSCT_BLOB) then
              begin
                Rec := Rect(CellRect.Left + 1,
                  CellRect.Top + 1,
                  CellRect.Left + BlobPNGImg.Width + 1,
                  CellRect.Top + BlobPNGImg.Height + 1);

                BlobPNGImg.Draw(Canvas, Rec);
              end;
            end;

            if (FHotRSNode = Node) and (FHotRSColumn = Column) then
              TransValue := 0
            else
              TransValue := 180;

            xpos := CellRect.Right;

            if (MySQLRS.EditingAllowed) and (NotNull) then
            begin
              xpos := xpos - FieldClearPNGImg.Width;
              Rec := Rect(xpos,
                CellRect.Top + 2,
                xpos + FieldClearPNGImg.Width,
                CellRect.Top + FieldClearPNGImg.Height + 2);
              FieldClearPNGImg.DrawFaded(Canvas, Rec, TransValue);
            end;

            if (NotNull) then
            begin
              xpos := xpos - FieldSavePNGImg.Width;
              Rec := Rect(xpos,
                CellRect.Top + 2,
                xpos + FieldSavePNGImg.Width,
                CellRect.Top + FieldSavePNGImg.Height + 2);
              FieldSavePNGImg.DrawFaded(Canvas, Rec, TransValue);
            end;

            if (MySQLRS.EditingAllowed) and (NotNull) then
            begin
              xpos := xpos - FieldEditPNGImg.Width;
              Rec := Rect(xpos,
                CellRect.Top + 2,
                xpos + FieldEditPNGImg.Width,
                CellRect.Top + FieldEditPNGImg.Height + 2);
              FieldEditPNGImg.DrawFaded(Canvas, Rec, TransValue);
            end;

            if (NotNull) then
            begin
              xpos := xpos - FieldViewPNGImg.Width;
              Rec := Rect(xpos,
                CellRect.Top + 2,
                xpos + FieldViewPNGImg.Width,
                CellRect.Top + FieldViewPNGImg.Height + 2);
              FieldViewPNGImg.DrawFaded(Canvas, Rec, TransValue);
            end;

            if (MySQLRS.EditingAllowed) then
            begin
              xpos := xpos - FieldLoadPNGImg.Width;
              Rec := Rect(xpos,
                CellRect.Top + 2,
                xpos + FieldLoadPNGImg.Width,
                CellRect.Top + FieldLoadPNGImg.Height + 2);
              FieldLoadPNGImg.DrawFaded(Canvas, Rec, TransValue);
            end;
          end;

          if Not(NotNull) and
            (Integer(Node.Index)<MySQLRS.ResultSet.rows_num+
              MySQLRS.InsertedRows-MySQLRS.DeletedRows) then
          begin
            Rec := Rect(CellRect.Left + 1,
              CellRect.Top + 1,
              CellRect.Left + FieldNullPNGImg.Width + 1,
              CellRect.Top + FieldNullPNGImg.Height + 1);

            FieldNullPNGImg.Draw(Canvas, Rec);
          end;
        end;
      finally
        MySQLRS.ResultsetLock.Release;
      end;
    end;

  if (Assigned(OnAfterCellPaint)) then
    OnAfterCellPaint(Self, Canvas, Node, Column, CellRect);
end;


//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.DoGetNodeTooltip(Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle): WideString;

begin
  Result := GetNodeRSText(Node, Column, True);
  if (Length(Result) > 200) or (StrPosW(PWideChar(Result), #10) <> nil) then
    LineBreakStyle := hlbForceMultiLine
  else
    LineBreakStyle := hlbForceSingleLine;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DoGridScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);

var
  i: Integer;

begin
  if (Parent is TMySQLRSPanel) then
    if (Parent.Parent is TRSTabSheet) then
      if (not (TRSTabSheet(Parent.Parent).GridsAreScrolling)) then
      begin
        TRSTabSheet(Parent.Parent).GridsAreScrolling := True;
        try
          Update;
          for i := 0 to TRSTabSheet(Parent.Parent).RSPanels.Count - 1 do
          begin
            TMySQLRSPanel(TRSTabSheet(Parent.Parent).RSPanels[i]).FRSGrid.OffsetXY :=
              OffsetXY;

            TMySQLRSPanel(TRSTabSheet(Parent.Parent).RSPanels[i]).FRSGrid.Update;
          end;
        finally
          TRSTabSheet(Parent.Parent).GridsAreScrolling := False;
        end;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DeleteSelectedRows;

var
  i: Integer;
  NodeData: PRSRowData;
  SelectedNodes: TNodeArray;

begin
  FHotRSNode := nil;
  SelectedNodes := GetSortedSelection(True);

  if (FMySQLRS.EditingAllowed) and (Length(SelectedNodes) > 0) then
  begin
    MySQLRS.ResultsetLock.Acquire;
    try
      for i := 0 to Length(SelectedNodes) - 1 do
      begin
        NodeData := GetNodeData(SelectedNodes[i]);

        if (NodeData.row_index >= FMySQLRS.ResultSet.rows_num +
          FMySQLRS.InsertedRows - FMySQLRS.DeletedRows) then
          Continue;

        AddAction(MYX_RSA_DELETE, NodeData.row_index, 1, SelectedNodes[i], '');
      end;
    finally
      MySQLRS.ResultsetLock.Release;
    end;
  end;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function CompareNodesByRowIndex(P1: Pointer; P2: Pointer): Integer;

var
  NodeData1, NodeData2: PRSRowData;
  VT: TBaseVirtualTree;

begin
  VT := TreeFromNode(P1);
  NodeData1 := VT.GetNodeData(P1);
  NodeData2 := VT.GetNodeData(P2);

  Result := NodeData1.row_index - NodeData2.row_index;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);

var
  PNG: TPNGObject;

begin
  with PaintInfo do
  begin
    if (Column.ImageIndex > -1) and (Column.ImageIndex < Length(FColumnIndicators)) then
    begin
      PNG := FColumnIndicators[Column.ImageIndex];
      PNG.Draw(TargetCanvas, Rect(GlyphPos.X, GlyphPos.Y, 0, 0)); // Width and height do not matter here.
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.ApplyDeleteActionsToGrid;

var
  i, j, DelIndex: Integer;
  NodeData: PRSRowData;
  DeleteActionApplyed: Boolean;
  action: PMYX_RS_ACTION;
  NodesToDeleteList: TList;
  Node,
  PrevNode: PVirtualNode;

begin
  // Make sure that the RS has been aquired before with
  // MySQLRS.ResultsetLock.Acquire;

  FHotRSNode := nil;
  Screen.Cursor := crHourGlass;
  NodesToDeleteList := TList.Create;
  try
    i := 0;
    while (i < NodesWithDeleteActionList.Count) do
    begin
      Node := NodesWithDeleteActionList[i];
      NodeData := GetNodeData(Node);

      DeleteActionApplyed := True;
      for j := NodeData.actions.Count - 1 downto 0 do
      begin
        action := PMYX_RS_ACTION(Integer(MySQLRS.ResultSet.actions.actions) + sizeof(MYX_RS_ACTION) *
          NodeData.actions[j]);

        if (action.action = MYX_RSA_DELETE) and
          (action.status <> MYX_RSAS_APPLIED)then
        begin
          DeleteActionApplyed := False;
          Break;
        end;
      end;

      if DeleteActionApplyed then
      begin
        NodesToDeleteList.Add(Node);
        NodesWithDeleteActionList.Delete(I);
        NodesWithActionList.Remove(Node);
      end
      else
        Inc(i);
    end;

    //If there are any deleted nodes where the
    //delete action didn't fail
    if (NodesToDeleteList.Count>0) then
    begin
      //Sort NodesToDeleteList ascending by row_index
      NodesToDeleteList.Sort(CompareNodesByRowIndex);

      //Starting from the bottom, update VT
      BeginUpdate;
      try
        DelIndex := NodesToDeleteList.Count-1;
        Node := GetLast;
        while (Node<>nil) and (DelIndex>=0) do
        begin
          NodeData := GetNodeData(Node);
          PrevNode := GetPrevious(Node);

          if (Node=NodesToDeleteList[DelIndex]) then
          begin
            //Do not trigger an automatic VT Node Index rebuild
            DeleteNode(NodesToDeleteList[DelIndex], False);

            dec(DelIndex);
          end
          else
          begin
            dec(NodeData.row_index, DelIndex+1);

            //Manipulate the VT Node Index manually
            dec(Node.Index, DelIndex+1);
          end;

          Node := PrevNode;
        end;
      finally
        EndUpdate;
      end;
    end;

    Invalidate;
  finally
    Screen.Cursor := crDefault;
    NodesToDeleteList.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.SynchronizeActions;

var
  I: Integer;
  RSAction: PMYX_RS_ACTION;
  NodeData: PRSRowData;
  Node: PVirtualNode;

begin
  // Make sure old nodes get refreshed.
  for I:=0 to NodesWithActionList.Count - 1 do
  begin
    InvalidateNode(NodesWithActionList[I]);

    // Remove action list from node data.
    NodeData := GetNodeData(NodesWithActionList[I]);
    if Assigned(NodeData.actions) then
    begin
      NodeData.actions.Free;
      NodeData.actions := nil;
    end;
  end;

  NodesWithActionList.Clear;
  NodesWithDeleteActionList.Clear;

  MySQLRS.InsertedRows := 0;
  MySQLRS.DeletedRows := 0;

  // Check if there are any failed actions.
  if Assigned(MySQLRS.ResultSet.actions) then
  begin
    // Deal with actions that failed.

    // Loop over all rows and add corresponding actions.
    Node := GetFirst;
    while Assigned(Node) do
    begin
      NodeData := GetNodeData(Node);

      // Clear all previous actions of the node (should be deleted already).
      if Assigned(NodeData.actions) then
        NodeData.actions.Clear;

      // Check if a new action belongs to a node.
      for I:=0 to MySQLRS.ResultSet.actions.actions_num-1 do
      begin
        RSAction := PMYX_RS_ACTION(Integer(MySQLRS.ResultSet.actions.actions) + SizeOf(MYX_RS_ACTION) * I);

        // If the action is in the same row as the current node add the action.
        if NodeData.row_index = RSAction.row then
        begin
          if NodeData.actions = nil then
            NodeData.actions := TIntegerList.Create;

          NodeData.actions.Add(I);
          NodesWithActionList.Add(Node);

          //deal with inserted data
          if RSAction.action = MYX_RSA_ADD then
            Inc(MySQLRS.InsertedRows)
          else
            if (RSAction.action = MYX_RSA_DELETE) and (RSAction.status <> MYX_RSAS_DISCARDED) then
            begin
              Inc(MySQLRS.DeletedRows);
              NodesWithDeleteActionList.Add(Node);
            end;
        end;
      end;
      Node := GetNext(Node);
    end;

    Invalidate;
  end;
end;


//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;

begin
  Result := TRSFieldEditLink.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.DblClick;

begin
  if (Assigned(OnDblClick)) then
    OnDblClick(Self)
  else
  begin
    if (MySQLRS.EditingAllowed) and (MySQLRS.Editable) then
      PostMessage(Handle, WM_EditCurrentCell, 0, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.WndProc(var Message: TMessage);

begin
  inherited WndProc(Message);

  if (Message.Msg = WM_EditCurrentCell) then
  begin
    EditColumn := FocusedColumn;
    DoEdit;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.GridFieldAction(Node: PVirtualNode; Column: TColumnIndex; GridFieldActionType: TGridFieldActionType);

var
  NodeData: PRSRowData;
  action_type: MYX_RS_ACTION_TYPE;
  field: PMYX_RS_FIELD;
  SaveDlg: TTntSaveDialog;
  LoadDlg: TTntOpenDialog;
  Data: string;
  FileStream: TFileStream;
  MySQLResultSetFieldViewerForm: TMySQLResultSetFieldViewerForm;
  S: WideString;

begin
  if (Node<>nil) then
  begin
    NodeData := GetNodeData(Node);

    //Check if update or insert
    if (not (NodeData.row_index < FMySQLRS.ResultSet.rows_num)) then
      action_type := MYX_RSA_ADD
    else
      action_type := MYX_RSA_UPDATE;

    if (GridFieldActionType=TGFAT_Clear) then
      AddAction(action_type, NodeData.row_index,
        Column, Node, '', -2)
    else
      if (GridFieldActionType=TGFAT_Save) then
      begin
        field := GetNodeRSField(Node, Column);
        if (field <> nil) then
          if (field.value <> nil) then
          begin
            SaveDlg := TTntSaveDialog.Create(nil);
            try
              {SaveDlg.InitialDir := ApplicationDM.GetLastFileDialogPaths(
                'ResultGridFieldDataStoreDialog');}
              SaveDlg.Filter := _('Any File') + ' (*.*)|*.*';
              if (SaveDlg.Execute) then
              begin
                {ApplicationDM.SetLastFileDialogPaths(
                  'ResultGridFieldDataStoreDialog', ExtractFilePath(SaveDlg.FileName));}

                FileStream := TFileStream.Create(SaveDlg.FileName, fmCreate);
                try
                  SetString(Data, field.value, field.value_length);

                  FileStream.Write(Data[1], field.value_length);
                finally
                  FileStream.Free;
                end;
              end;
            finally
              SaveDlg.Free;
            end;
          end;
      end
      else
        if GridFieldActionType = TGFAT_Edit then
        begin
            MySQLResultSetFieldViewerForm := TMySQLResultSetFieldViewerForm.Create(nil, ViewerModeEdit);
            try
              MySQLResultSetFieldViewerForm.SetContent(GetNodeRSValue(Node, Column));

              if (MySQLResultSetFieldViewerForm.ShowModal = mrOK) then
              begin
                if MySQLResultSetFieldViewerForm.ContentType = ViewerContentTypeText then
                  AddAction(action_type, NodeData.row_index, Column, Node, MySQLResultSetFieldViewerForm.TextMemo.Text, -1);
              end;

              SetFocus;
            finally
              MySQLResultSetFieldViewerForm.Free;
            end;
        end
        else
          if (GridFieldActionType = TGFAT_View) then
          begin
              MySQLResultSetFieldViewerForm := TMySQLResultSetFieldViewerForm.Create(nil, ViewerModeView);
              try
                MySQLResultSetFieldViewerForm.SetContent(GetNodeRSValue(Node, Column));
                MySQLResultSetFieldViewerForm.ShowModal;
                SetFocus;
              finally
                MySQLResultSetFieldViewerForm.Free;
              end;
          end
          else
            if (GridFieldActionType=TGFAT_Load) then
            begin
              LoadDlg := TTntOpenDialog.Create(nil);
              try
                LoadDlg.Filter := _('Any File') + ' (*.*)|*.*';
                if (LoadDlg.Execute) then
                begin
                  Data := LoadAnsiTextFromFile(LoadDlg.FileName);

                  AddAction(action_type, NodeData.row_index,
                    Column,
                    Node,
                    '', Length(Data), Data);
                end;
              finally
                LoadDlg.Free;
              end;
            end
            else
              if (GridFieldActionType=TGFAT_Copy) then
              begin
                S := GetNodeRSText(Node, Column, True);
                TntClipboard.AsWideText := S;
              end;

  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.HeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  var Elements: THeaderPaintElements);

begin
  Include(Elements, hpeHeaderGlyph);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  HitInfo: THitInfo;
  col: PMYX_RS_COLUMN;
  NodeRect: TRect;
  xpos: Integer;
  NotNull: Boolean;
  FieldValue: TRSFieldValue;

begin
  if (not (FShowFieldOverlayImages)) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    Exit;
  end;

  GetHitTestInfoAt(X, Y, True, HitInfo);

  if (HitInfo.HitNode <> nil) then
  begin
    if (HitInfo.HitColumn > 0) and (HitInfo.HitColumn - 1 < Integer(MySQLRS.ResultSet.columns_num)) then
      col := PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + sizeof(MYX_RS_COLUMN) * (HitInfo.HitColumn - 1))
    else
      col := nil;

    if Assigned(col) then
    begin
      if col.column_type in [MYX_RSCT_TEXT, MYX_RSCT_BLOB] then
      begin
        NodeRect := GetDisplayRect(
          HitInfo.HitNode, HitInfo.HitColumn, False);

        if (Y > NodeRect.Top + 2) and (Y < NodeRect.Top + 8 + 2) then
        begin
          NotNull := False;
          if (col <> nil) then
          begin
            FieldValue := GetNodeRSValue(HitInfo.HitNode, HitInfo.HitColumn);
            NotNull := not FieldValue.IsNull;
          end;

          xpos := NodeRect.Right;

          //Clear
          if (MySQLRS.EditingAllowed) and (NotNull) then
          begin
            xpos := xpos - FieldClearPNGImg.Width ;

            if (X >= xpos) and (X <= xpos + FieldClearPNGImg.Width) then
            begin
              GridFieldAction(HitInfo.HitNode, HitInfo.HitColumn, TGFAT_Clear);
              Abort;
            end;
          end;

          if (NotNull) then
          begin
            //Save
            xpos := xpos -  FieldSavePNGImg.Width;

            if (X >= xpos) and (X <= xpos + FieldSavePNGImg.Width) then
            begin
              GridFieldAction(HitInfo.HitNode, HitInfo.HitColumn, TGFAT_Save);
              Abort;
            end;
          end;

          if (MySQLRS.EditingAllowed) and (NotNull) then
          begin
            xpos := xpos -  FieldEditPNGImg.Width;

            if (X >= xpos) and (X <= xpos + FieldEditPNGImg.Width) then
            begin
              GridFieldAction(HitInfo.HitNode, HitInfo.HitColumn, TGFAT_Edit);
              Abort;
            end;
          end;

          if (NotNull) then
          begin
            xpos := xpos -  FieldViewPNGImg.Width;

            if (X >= xpos) and (X <= xpos + FieldViewPNGImg.Width) then
            begin
              GridFieldAction(HitInfo.HitNode, HitInfo.HitColumn, TGFAT_View);
              Abort;
            end;

          end;

          if (MySQLRS.EditingAllowed) then
          begin
            xpos := xpos -  FieldLoadPNGImg.Width;

            if (X >= xpos) and (X <= xpos + FieldLoadPNGImg.Width) then
            begin
              GridFieldAction(HitInfo.HitNode, HitInfo.HitColumn, TGFAT_Load);
              Abort;
            end;
          end;
        end;
      end;
    end;
  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.OptionChanged;

var
  I: Integer;
  NodeData: PRSRowData;
  Node: PVirtualNode;
  col: MYX_RS_COLUMN;

begin
  BeginUpdate;
  try
    RefreshOptions;

    // Reset the computed friendly strings for display.
    for I := 0 to FMRUCaptions.Count - 1 do
    begin
      NodeData := FMRUCaptions.List[I];
      NodeData.FriendlyLinebreaksCaptions := nil;
    end;

    Font.Name := MYXCommonOptions.DataFontName;
    Font.Height := MYXCommonOptions.DataFontHeight;

    // Add 8 pixels, which correspond to 4 pixels distance above and below the text to the cell border.
    DefaultNodeHeight := Abs(MYXCommonOptions.DataFontHeight) + 8;
    Header.Font := Font;
    Header.Font.Height := MYXCommonOptions.DataFontHeight;
    Header.Height := 2 * Abs(Header.Font.Height);

    // Resize existing rows.
    Node := GetFirstNoInit;
    while Assigned(Node) do
    begin
      NodeHeight[Node] := DefaultNodeHeight;
      Node := GetNextNoInit(Node);
    end;

    //Update Column orientation
    if (FMySQLRS<>nil) and (FMySQLRS.ResultSet<>nil) then
      for I := 0 to FMySQLRS.ResultSet.columns_num_to_display - 1 do
      begin
        col := PMYX_RS_COLUMN(Integer(FMySQLRS.ResultSet.columns) + sizeof(MYX_RS_COLUMN) * i)^;

        if (I+1<Header.Columns.Count) and
          ((col.column_type = MYX_RSCT_INTEGER) or
            (col.column_type = MYX_RSCT_FLOAT)) then
          if FAlignNumericColsRight then
            Header.Columns[I+1].Alignment := taRightJustify
          else
            Header.Columns[I+1].Alignment := taLeftJustify;
      end;

  finally
    EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.RefreshOptions;

var
  Provider: IOptionProvider;

begin
  Provider := GetOptionProvider;
  if Assigned(Provider) then
  begin
    Provider.AddListener(Self);
    FFriendlyLineBreaks := Provider.OptionAsBoolean['FriendlyLineBreaks'];
    FFriendlyLineBreaksLF := Provider.OptionAsString['FriendlyLineBreaksLF'];
    FFriendlyLineBreaksCR := Provider.OptionAsString['FriendlyLineBreaksCR'];
    FAlignNumericColsRight := Provider.OptionAsBoolean['AlignNumericColsRight'];
    FAutoEdit := Provider.OptionAsBoolean['ResultsetAutoEdit'];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.SetShowFieldOverlayImages(ShowImgs: Boolean);

begin
  FShowFieldOverlayImages := ShowImgs;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.CopyColumnNames(Sender: TObject);

var
  I: Integer;
  S: WideString;

begin
  S := '';

  for I := 1 to Header.Columns.Count - 1 do
    S := S + Header.Columns[I].Text + ', ';

  TntClipboard.AsWideText := Copy(S, 1, Length(S) - 2);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.GetValuesOfFocusedAndSelectedRows: WideString;

var
  i, j: Integer;
  s: WideString;
  SelectedNodes: TNodeArray;
  FocusedNodeInSelection: Boolean;
  Quote: WideString;
  RSColumn: PMYX_RS_COLUMN;

begin
  s := '';
  FocusedNodeInSelection := False;

  SelectedNodes := GetSortedSelection(True);

  for j := 0 to Length(SelectedNodes)-1 do
  begin
    RSColumn := FMySQLRS.ResultSet.columns;

    for i := 1 to Header.Columns.Count - 1 do
    begin
      if (RSColumn.column_type = MYX_RSCT_STRING) or
        (RSColumn.column_type = MYX_RSCT_TEXT) or
        (RSColumn.column_type = MYX_RSCT_DATE) or
        (RSColumn.column_type = MYX_RSCT_DATETIME) or
        (RSColumn.column_type = MYX_RSCT_BLOB) then
        Quote := ''''
      else
        Quote := '';

      s := s + Quote +
        GetNodeRSText(SelectedNodes[j], i, True) + Quote;

      if (i<Header.Columns.Count - 1) then
        s := s + ', ';

      inc(RSColumn);
    end;

    s := s + #13#10;

    if (SelectedNodes[j] = FocusedNode) then
      FocusedNodeInSelection := True;
  end;

  if (FocusedNode <> nil) and (Not(FocusedNodeInSelection)) then
  begin
    RSColumn := FMySQLRS.ResultSet.columns;

    for i := 1 to Header.Columns.Count - 1 do
    begin
      if (RSColumn.column_type = MYX_RSCT_STRING) or
        (RSColumn.column_type = MYX_RSCT_TEXT) or
        (RSColumn.column_type = MYX_RSCT_DATE) or
        (RSColumn.column_type = MYX_RSCT_DATETIME) or
        (RSColumn.column_type = MYX_RSCT_BLOB) then
        Quote := ''''
      else
        Quote := '';

      s := s + Quote +
        GetNodeRSText(FocusedNode, i, True) + Quote;

      if (i<Header.Columns.Count - 1) then
        s := s + ', ';

      inc(RSColumn);
    end;

    s := s + #13#10;
  end;

  Result := Copy(s, 1, Length(s) - 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSGrid.MouseMove(Shift: TShiftState; X, Y: Integer);

var HitInfo: THitInfo;

begin
  inherited MouseMove(Shift, X, Y);

  GetHitTestInfoAt(X, Y, True, HitInfo);
  if (HitInfo.HitNode <> FHotRSNode) or
    (HitInfo.HitColumn <> FHotRSColumn) then
  begin
    if Assigned(FHotRSNode) then
      InvalidateNode(FHotRSNode);
    FHotRSNode := HitInfo.HitNode;
    FHotRSColumn := HitInfo.HitColumn;
    if Assigned(FHotRSNode) then
      InvalidateNode(FHotRSNode);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSGrid.DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
  SearchOptions: TTextSearchOptions): Integer;

var
  NodeText: WideString;
  P: Integer;
  Col: Integer;
  NodeData: PRSRowData;
  action_type: MYX_RS_ACTION_TYPE;
  STLen: Integer;
  TextLen: Integer;
  OriginalSearchText,
  OriginalNodeText: WideString;

begin
  if ((tsoReplace in SearchOptions) or (tsoReplaceAll in SearchOptions)) and not FMySQLRS.EditingAllowed then
    raise EMyxError.Create(_('You have to enable editing first.'));

  Result := 1;
  OriginalSearchText := SearchText;

  while(True)do
  begin
    if (FLastSearchNode = nil) or
      ((tsoEntireScope in SearchOptions) and
      (FLastSearchText<>OriginalSearchText))then
    begin
      FLastSearchNode := GetFirst;
      if (tsoSelectedOnly in SearchOptions) then
      begin
        FLastSearchCol := FocusedColumn;
      end
      else
      begin
        FLastSearchCol := 1;
      end;
      FLastSearchText := SearchText;
    end
    else
    begin
      if (tsoSelectedOnly in SearchOptions) then
      begin
        FLastSearchNode := GetNextSibling(FLastSearchNode);
      end
      else
      begin
        inc(FLastSearchCol);
        if (FLastSearchCol>=Header.Columns.Count) then
        begin
          FLastSearchCol := 1;
          FLastSearchNode := GetNextSibling(FLastSearchNode);
        end;
      end;
    end;

    if FLastSearchNode = nil then
    begin
      Result := 0;
      Break;
    end;

    NodeText := GetNodeRSText(FLastSearchNode, FLastSearchCol, True);
    OriginalNodeText := NodeText;

    if not (tsoMatchCase in SearchOptions) then
    begin
      NodeText := WideUpperCase(NodeText);
      SearchText := WideUpperCase(SearchText);
    end;

    P := Pos(SearchText, NodeText);
    if P > 0 then
    begin
      STLen := Length(SearchText);
      TextLen := Length(NodeText);

      if (tsoWholeWord in SearchOptions) then
        if Not(
          ((P=1) or (NodeText[P-1]=' ')) and
          ((P=TextLen) or (NodeText[P+STLen]=' '))
          ) then
          continue;

      if (tsoReplace in SearchOptions) or
        (tsoReplaceAll in SearchOptions) then
      begin
        NodeData := GetNodeData(FLastSearchNode);

        if (NodeData.row_index < FMySQLRS.ResultSet.rows_num) then
          action_type := MYX_RSA_UPDATE
        else
          action_type := MYX_RSA_ADD;

        AddAction(action_type, NodeData.row_index, FLastSearchCol,
          FLastSearchNode,
          Copy(OriginalNodeText, 1, P-1)+ReplaceText+
            Copy(OriginalNodeText, P+STLen, TextLen));

        Invalidate;
      end
      else
      begin
        //Store col to prevent override by FocusChange
        Col := FLastSearchCol;

        ClearSelection;
        FocusedNode := FLastSearchNode;
        FocusedColumn := Col;
        Selected[FocusedNode] := True;
      end;


      if Not(tsoReplaceAll in SearchOptions) then
        break;
    end
  end;
end;


//----------------- TMySQLRSNav ----------------------------------------------------------------------------------------

constructor TMySQLRSNav.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  Height := 20;
  BevelOuter := bvNone;
  Caption := '';

  RSEditPNGImg := LoadPNGImageFromResource('rs_edit');
  RSEditDisabledPNGImg := LoadPNGImageFromResource('rs_edit_disabled');
  RSEditActivePNGImg := LoadPNGImageFromResource('rs_edit_active');
  RSApplyPNGImg := LoadPNGImageFromResource('rs_apply');
  RSApplyDisabledPNGImg := LoadPNGImageFromResource('rs_apply_disabled');
  RSDiscardPNGImg := LoadPNGImageFromResource('rs_discard');
  RSDiscardDisabledPNGImg := LoadPNGImageFromResource('rs_discard_disabled');

  RSFirstPNGImg := LoadPNGImageFromResource('rs_first');
  RSFirstDisabledPNGImg := LoadPNGImageFromResource('rs_first_disabled');

  RSLastPNGImg := LoadPNGImageFromResource('rs_last');
  RSLastDisabledPNGImg := LoadPNGImageFromResource('rs_last_disabled');

  RSSearchPNGImg := LoadPNGImageFromResource('rs_search');
  RSSearchDisabledPNGImg := LoadPNGImageFromResource('rs_search_disabled');

  RSBtnDownBgImg := LoadPNGImageFromResource('rs_btn_down_bg');
  RSBtnDownLeftBgImg := LoadPNGImageFromResource('rs_btn_down_left_bg');
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLRSNav.Destroy;

begin
  MySQLRS := nil;

  RSEditPNGImg.Free;
  RSEditDisabledPNGImg.Free;
  RSEditActivePNGImg.Free;
  RSApplyPNGImg.Free;
  RSApplyDisabledPNGImg.Free;
  RSDiscardPNGImg.Free;
  RSDiscardDisabledPNGImg.Free;
  RSFirstPNGImg.Free;
  RSFirstDisabledPNGImg.Free;
  RSLastPNGImg.Free;
  RSLastDisabledPNGImg.Free;
  RSSearchPNGImg.Free;
  RSSearchDisabledPNGImg.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.ClearValues;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSNav.GetMySQLRS: TMySQLRS;

begin
  Result := FMySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.SetMySQLRS(MySQLRS: TMySQLRS);

begin
  if (FMySQLRS <> nil) and (MySQLRS = nil) then
    FMySQLRS.DisconnectControl(self);

  FMySQLRS := MySQLRS;

  if (MySQLRS <> nil) then
    MySQLRS.ConnectControl(self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSNav.GetControl: TObject;

begin
  Result := self;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSNav.GetActive: Boolean;

begin
  Result := FActive;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.SetActive(Active: Boolean);

begin
  FActive := Active;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.DoCurrentRowChanged;

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.DoEditStateChanged;

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.DoStatusCaptionChanged;

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.DoMessagesChanged;

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.Paint;

var
  xpos: Integer;

begin
  with Canvas do
  begin
    Font.Name := 'Tahoma';
    Font.Size := 7;

    Pen.Color := $009C9B91;
    if (FActive) then
      Brush.Color := clWhite
    else
      Brush.Color := $00EEEEEE;
    Rectangle(Rect(0, -1, Width, Height));

    xpos := Width;

    RSSearchXPos := PaintEditButton(_('Search'), xpos, RSSearchPNGImg,
      RSSearchDisabledPNGImg, MySQLRS.AtLeastOneRecord);

    RSLastXPos := PaintEditButton(_('Last'), xpos, RSLastPNGImg,
      RSLastDisabledPNGImg, not (MySQLRS.LastRowSelected) and
      MySQLRS.AtLeastOneRecord);

    RSFirstXPos := PaintEditButton(_('First'), xpos, RSFirstPNGImg,
      RSFirstDisabledPNGImg, not (MySQLRS.FirstRowSelected) and
      MySQLRS.AtLeastOneRecord);

    xpos := xpos - 3;
    MoveTo(xpos, 0);
    LineTo(xpos, 20);

    RSDiscardXPos := PaintEditButton(_('Discard Changes'), xpos, RSDiscardPNGImg,
      RSDiscardDisabledPNGImg, (MySQLRS.Edited));

    RSApplyXPos := PaintEditButton(_('Apply Changes'), xpos, RSApplyPNGImg,
      RSApplyDisabledPNGImg, (MySQLRS.Edited));

    RSEditXPos := PaintEditButton(_('Edit'), xpos,
      RSEditPNGImg,
      RSEditDisabledPNGImg,
      (MySQLRS.Editable) and (not (MySQLRS.Edited)) and
      (not (MySQLRS.EditingAllowed)),
      RSEditActivePNGImg,
      (MySQLRS.EditingAllowed));

    xpos := xpos - 3;
    MoveTo(xpos, 0);
    LineTo(xpos, 20);

    Font.Color := clBlack;

    DrawWideStringText(Canvas.Handle, PWideChar(MySQLRS.StatusCaption),
      Length(MySQLRS.StatusCaption), Rect(4, 4, xpos - 3, 4 + 10));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSNav.PaintEditButton(Caption: WideString; var xpos: Integer; PNGImg: TPNGObject; DisabledPNGImg:
  TPNGObject;
  Enabled: Boolean; ActivePNGImg: TPNGObject; Active: Boolean): Integer;

var
  i, l, w, x: Integer;
  s: WideString;

begin
  with Canvas do
  begin
    if (Width > 550) then
    begin
      s := _(Caption);

      l := GetWideStringTextWidth(Canvas, s);

      if (Active) then
      begin
        x := xpos - 7 - l - 21 - 4;
        w := 7 + l + 21 + 4;

        RSBtnDownLeftBgImg.Draw(Canvas,
          Rect(x, 0,
            x+RSBtnDownLeftBgImg.Width, RSBtnDownLeftBgImg.Height));

        i := x+RSBtnDownLeftBgImg.Width;
        while (i<x+w-RSBtnDownBgImg.Width) do
        begin
          RSBtnDownBgImg.Draw(Canvas,
            Rect(i, 0, i+RSBtnDownBgImg.Width, RSBtnDownBgImg.Height));

          inc(i, RSBtnDownBgImg.Width);
        end;

        RSBtnDownBgImg.Draw(Canvas,
          Rect(x+w-RSBtnDownBgImg.Width, 0,
            x+w, RSBtnDownBgImg.Height));
      end;

      xpos := xpos - 7 - l;

      if (Enabled) or (Active) then
        Font.Color := clBlack
      else
        Font.Color := $00CCCCCC;

      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawWideStringText(Canvas.Handle, PWideChar(s),
        Length(s), Rect(xpos+2*Ord(Active), 4+2*Ord(Active),
          xpos + l + 2*Ord(Active), 4 + 18 + 2*Ord(Active)));

      xpos := xpos - 21;
    end
    else
    begin
      if (Active) then
      begin
        x := xpos - 7 - 16 - 4;
        w := 7 + 16 + 4;

        RSBtnDownLeftBgImg.Draw(Canvas,
          Rect(x, 0,
            RSBtnDownLeftBgImg.Width, RSBtnDownLeftBgImg.Height));

        i := RSBtnDownLeftBgImg.Width;
        while (i<w) do
        begin
          RSBtnDownBgImg.Draw(Canvas,
            Rect(x+i, 0, RSBtnDownBgImg.Width, RSBtnDownBgImg.Height));

          inc(i, RSBtnDownBgImg.Width);
        end;

        RSBtnDownBgImg.Draw(Canvas,
          Rect(x+w-RSBtnDownBgImg.Width, 0,
            x+w, RSBtnDownBgImg.Height));
      end;

      xpos := xpos - 7 - 16;
    end;

    if (Active) then
      PNGImg.Draw(Canvas,
        Rect(xpos + 1 + 2*Ord(Active), 2 + 2*Ord(Active),
          xpos + 1 + ActivePNGImg.Width + 2*Ord(Active), 2 + ActivePNGImg.Height + 2*Ord(Active)))
    else
      if (Enabled) then
        PNGImg.Draw(Canvas,
          Rect(xpos + 1, 2, xpos + 1 + PNGImg.Width, 2 + PNGImg.Height))
      else
        DisabledPNGImg.Draw(Canvas,
          Rect(xpos + 1, 2, xpos + 1 + DisabledPNGImg.Width, 2 + DisabledPNGImg.Height));

    xpos := xpos - 5;
    MoveTo(xpos, 0);
    LineTo(xpos, 20);
  end;

  Result := xpos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSNav.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (Assigned(OnEnter)) then
    OnEnter(self);

  if (MySQLRS.Editable) and
    (X > RSEditXPos) and (X < RSApplyXPos) then
    MySQLRS.DoEdit;

  if (MySQLRS.Editable) and (MySQLRS.EditingAllowed) and (MySQLRS.Edited) and
    (X > RSApplyXPos) and (X < RSDiscardXPos) then
    MySQLRS.DoApplyChanges;

  if (MySQLRS.Editable) and (MySQLRS.EditingAllowed) and (MySQLRS.Edited) and
    (X > RSDiscardXPos) and (X < RSFirstXPos) then
    MySQLRS.DoDiscardChanges;

  if (not (MySQLRS.FirstRowSelected) and (MySQLRS.AtLeastOneRecord)) and
    (X > RSFirstXPos) and (X < RSLastXPos) then
    MySQLRS.GotoFirstRow;

  if (not (MySQLRS.LastRowSelected) and (MySQLRS.AtLeastOneRecord)) and
    (X > RSLastXPos) and (X < RSSearchXPos) then
    MySQLRS.GotoLastRow;

  if (MySQLRS.AtLeastOneRecord) and
    (X > RSSearchXPos) then
    MySQLRS.DoDisplaySearch;
end;

//----------------- TMySQLRSErrorGrid ----------------------------------------------------------------------------------

constructor TMySQLRSErrorGrid.Create(AOwner: TComponent);

var
  VTColumn: TVirtualTreeColumn;
  MsgMenuItem: TTntMenuItem;

begin
  inherited Create(AOwner);

  NodeDataSize := SizeOf(TRSMsg);

  Header.Options := Header.Options + [hoDblClickResize, hoVisible];
  VTColumn := Header.Columns.Add;
  VTColumn.Text := '!';
  VTColumn.Width := 16;
  VTColumn := Header.Columns.Add;
  VTColumn.Text := _('Description');
  VTColumn := Header.Columns.Add;
  VTColumn.Text := _('ErrorNr.');
  VTColumn.Width := 70;
  Header.AutoSizeIndex := 1;
  Header.Options := Header.Options + [hoAutoResize] - [hoColumnResize];
  ParentBackground := False;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions, toVariableNodeHeight];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions -[toShowRoot, toShowTreeLines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect, toRightClickSelect];
  TreeOptions.StringOptions := TreeOptions.StringOptions - [toSaveCaptions];
  HintMode := hmTooltip;
  ShowHint := True;
  DefaultNodeHeight := 18;
  Align := alBottom;
  Height := 0;

  //Load PNG Images
  RSErrorPNGImg := LoadPNGImageFromResource('rs_error');
  RSWarningPNGImg := LoadPNGImageFromResource('rs_warning');
  RSNotePNGImg := LoadPNGImageFromResource('rs_notice');

  //Create PopupMenu
  MsgPopupMenu := TTntPopupMenu.Create(nil);
  MsgPopupMenu.OnPopup := MsgPopup;

  MsgMenuItem := TTntMenuItem.Create(self);
  MsgMenuItem.Caption := _('Copy Message Text');
  MsgMenuItem.OnClick := CopyMsgText;
  MsgPopupMenu.Items.Add(MsgMenuItem);

  MsgMenuItem := TTntMenuItem.Create(self);
  MsgMenuItem.Caption := _('Clear Messages');
  MsgMenuItem.OnClick := ClearMsgText;
  MsgPopupMenu.Items.Add(MsgMenuItem);


  Self.PopupMenu := MsgPopupMenu;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLRSErrorGrid.Destroy;

begin
  MySQLRS := nil;

  RSErrorPNGImg.Free;
  RSWarningPNGImg.Free;
  RSNotePNGImg.Free;

  MsgPopupMenu.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.ClearValues;

begin
  Clear;
  Height := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSErrorGrid.GetMySQLRS: TMySQLRS;

begin
  Result := FMySQLRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.MsgPopup(Sender: TObject);

begin
  MsgPopupMenu.Items[0].Enabled := SelectedCount > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRSErrorGrid.GetControl: TObject;

begin
  Result := self;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoCurrentRowChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoEditStateChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoStatusCaptionChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoMessagesChanged;

var
  Node: PVirtualNode;
  Nodedata: PRSMsg;

begin
  if FMySQLRS.ErrorMessages.Count = 0 then
  begin
    Height := 0;
    Clear;
  end
  else
  begin
    Canvas.Font := Font;
    RootNodeCount := FMySQLRS.ErrorMessages.Count;
    Node := GetFirst;
    while Assigned(Node) do
    begin
      NodeData := GetNodeData(Node);
      NodeData.MsgIndex := Node.Index;
      Multiline[Node] := True;
      NodeHeight[Node] := ComputeNodeHeight(Canvas, Node, 1);
      Node := GetNext(Node);
    end;

    if RootNodeCount = 1 then
      ClientHeight := 1; // Client height bug. The first setting seems not to be honoured.
    ClientHeight := Min(RootNode.TotalHeight - DefaultNodeHeight, 200);

    Top := 2000;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoActionsChanged;

begin
  //
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.SetMySQLRS(MySQLRS: TMySQLRS);

begin
  if (FMySQLRS <> nil) and (MySQLRS = nil) then
    FMySQLRS.DisconnectControl(self);

  FMySQLRS := MySQLRS;

  if (MySQLRS <> nil) then
    MySQLRS.ConnectControl(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text:
  WideString);

var
  NodeData: PRSMsg;

begin
  if (Assigned(OnGetText)) then
    OnGetText(Self, Node, Column, TextType, Text)
  else
  begin
    if (Column = 0) then
    begin
      Text := '';
    end
    else
    begin
      NodeData := GetNodeData(Node);

      if (NodeData <> nil) then
        if (NodeData.MsgIndex < FMySQLRS.ErrorMessages.Count) then
          case Column of
            1:
              Text := TRSError(FMySQLRS.ErrorMessages[NodeData.MsgIndex]).Msg;
            2:
              if (TRSError(FMySQLRS.ErrorMessages[NodeData.MsgIndex]).MsgNr <> 0) then
                Text := IntToStr(TRSError(FMySQLRS.ErrorMessages[NodeData.MsgIndex]).MsgNr)
              else
                Text := '';
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect:
  TRect);

var
  NodeData: PRSMsg;

begin
  if (Column = 0) then
  begin
    NodeData := nil;
    if (Node <> nil) then
      NodeData := GetNodeData(Node);

    if (NodeData <> nil) then
      if (NodeData.MsgIndex < FMySQLRS.ErrorMessages.Count) then
      begin
        case TRSError(FMySQLRS.ErrorMessages[NodeData.MsgIndex]).MsgType of
          MYX_QEL_ERROR:
            RSErrorPNGImg.Draw(Canvas, Rect(4, 2,
              RSErrorPNGImg.Width + 1, RSErrorPNGImg.Height + 1));
          MYX_QEL_WARNING:
            RSWarningPNGImg.Draw(Canvas, Rect(4, 2,
              RSWarningPNGImg.Width + 1, RSWarningPNGImg.Height + 1));
          MYX_QEL_NOTE:
            RSWarningPNGImg.Draw(Canvas, Rect(4, 2,
              RSNotePNGImg.Width + 1, RSWarningPNGImg.Height + 1));
        end;
      end;
  end;

  if (Assigned(OnAfterCellPaint)) then
    OnAfterCellPaint(Self, Canvas, Node, Column, CellRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.CopyMsgText(Sender: TObject);

var
  NodeData: PRSMsg;

begin
  if (FocusedNode <> nil) then
  begin
    NodeData := GetNodeData(FocusedNode);
    if (NodeData <> nil) then
      if (NodeData.MsgIndex < FMySQLRS.ErrorMessages.Count) then
      begin
        TntClipboard.AsWideText := TRSError(FMySQLRS.ErrorMessages[NodeData.MsgIndex]).Msg;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRSErrorGrid.ClearMsgText(Sender: TObject);

begin
  MySQLRS.ClearRSMessages;
end;


//----------------- TRSStringEdit --------------------------------------------------------------------------------------

// Implementation of a generic node caption editor.

constructor TRSStringEdit.Create(Link: TRSFieldEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.CMAutoAdjust(var Message: TMessage);

begin
  AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.CMExit(var Message: TMessage);

begin
  if Assigned(FLink) and not FLink.FStopping then
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        DoEndEdit
      else
        DoCancelEdit;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.CMRelease(var Message: TMessage);

begin
  Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.CNCommand(var Message: TWMCommand);

begin
  if Assigned(FLink) and Assigned(FLink.FTree) and (Message.NotifyCode = EN_UPDATE) and
    not (toGridExtensions in FLink.FTree.TreeOptions.MiscOptions) and
    not (vsMultiline in FLink.FNode.States) then
    // Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
    // and eventual resizing. Hence we use a message to accomplish that.
    if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
      AutoAdjustSize
    else
      PostMessage(Handle, CM_AUTOADJUST, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.WMChar(var Message: TWMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.WMDestroy(var Message: TWMDestroy);

begin
  // If editing stopped by other means than accept or cancel then we have to do default processing for
  // pending changes.
  if Assigned(FLink) and not FLink.FStopping then
  begin
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FNode, FColumn] := FEdit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.WMKeyDown(var Message: TWMKeyDown);

// Handles some control keys.

var
  Shift: TShiftState;
  EndEdit: Boolean;
  Tree: TBaseVirtualTree;
  Node: PVirtualNode;

begin
  Shift := KeyDataToShiftState(Message.KeyData);

  Tree := FLink.FTree;

  if (Message.CharCode = VK_ESCAPE) then
  begin
    FLink.FTree.DoCancelEdit;
    Tree.SetFocus;
  end
  else
    if (Message.CharCode = VK_RETURN) then
    begin
      EndEdit := Shift <> [ssAlt];
      if EndEdit then
      begin
        FLink.FTree.InvalidateNode(FLink.FNode);
        FLink.FTree.DoEndEdit;
        Tree.SetFocus;
      end;
      // Alt key handling is done in WM_SYSKEYDOWN;
    end
    else
      if (Message.CharCode = VK_UP) and (Shift = [ssCtrl]) then
      begin
        if (Tree.FocusedNode = nil) then
          Node := Tree.GetLastVisible
        else
          Node := Tree.GetPreviousVisible(Tree.FocusedNode);

        if (Node <> nil) then
        begin
          Tree.InvalidateNode(FLink.FNode);
          FLink.FTree.DoEndEdit;
          Tree.SetFocus;

          Tree.FocusedNode := Node;
          Tree.ClearSelection;
          Tree.Selected[Tree.FocusedNode] := True;

          PostMessage(Tree.Handle, WM_EditCurrentCell, 0, 0);
        end;
      end
      else
        if (Message.CharCode = VK_DOWN) and (Shift = [ssCtrl]) then
        begin
          if (Tree.FocusedNode = nil) then
            Node := Tree.GetFirstVisible
          else
            Node := Tree.GetNextVisible(Tree.FocusedNode);

          if (Node <> nil) then
          begin
            Tree.InvalidateNode(FLink.FNode);
            FLink.FTree.DoEndEdit;
            Tree.SetFocus;

            Tree.FocusedNode := Node;
            Tree.ClearSelection;
            Tree.Selected[Tree.FocusedNode] := True;

            PostMessage(Tree.Handle, WM_EditCurrentCell, 0, 0);
          end;
        end
        else
          if (((Message.CharCode = VK_LEFT) and (Shift = [ssCtrl])) or
            ((Message.CharCode = VK_TAB) and (Shift = [ssShift]))) then
          begin
            if (Tree.FocusedColumn > 1) then
            begin
              Tree.InvalidateNode(FLink.FNode);
              FLink.FTree.DoEndEdit;
              Tree.SetFocus;

              Tree.FocusedColumn := Tree.FocusedColumn - 1;

              PostMessage(Tree.Handle, WM_EditCurrentCell, 0, 0);
            end;
          end
          else
            if (((Message.CharCode = VK_RIGHT) and (Shift = [ssCtrl])) or
              ((Message.CharCode = VK_TAB) and (Shift = []))) then
            begin
              if (Tree.FocusedColumn < FLink.FTree.Header.Columns.Count - 1) then
              begin
                Tree.InvalidateNode(FLink.FNode);
                FLink.FTree.DoEndEdit;
                Tree.SetFocus;

                Tree.FocusedColumn := Tree.FocusedColumn + 1;

                PostMessage(Tree.Handle, WM_EditCurrentCell, 0, 0);
              end
              else
              begin
                Node := Tree.GetNextVisible(Tree.FocusedNode);

                if (Node <> nil) then
                begin
                  Tree.InvalidateNode(FLink.FNode);
                  FLink.FTree.DoEndEdit;
                  Tree.SetFocus;

                  Tree.FocusedNode := Node;
                  Tree.FocusedColumn := 1;
                  Tree.ClearSelection;
                  Tree.Selected[Tree.FocusedNode] := True;

                  PostMessage(Tree.Handle, WM_EditCurrentCell, 0, 0);
                end;
              end;
            end
            else
              inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.WMSysKeyDown(var Message: TWMKeyDown);

var
  NeededHeight: Integer;
  
begin
  if Message.CharCode = VK_RETURN then
  begin
    // Add a line break where the cursor is currently.
    SelText := #13#10;
    SendMessage(Handle, EM_SCROLLCARET, 0, 0);

        // Make the node multi-lined if there are hard line breaks in it.
    if StrPosW(PWideChar(Text), #10) <> nil then
    begin
      FLink.FTree.MultiLine[FLink.FNode] := True;

      // Node height should not exceed a certain value, in order to avoid extremely large editors.
      // Add 8 pixels, which correspond to 4 pixels distance above and below the text to the edit border.
      NeededHeight := Min(FLink.FTree.ComputeNodeHeight(FLink.FTree.Canvas, FLink.FNode, FLink.FColumn, Text) + 8, 150);
      FLink.FTree.NodeHeight[FLink.FNode] := NeededHeight;
    end;

  end
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.AutoAdjustSize;

// Changes the size of the edit to accomodate as much as possible of its text within its container window.
// NewChar describes the next character which will be added to the edit's text.

var
  DC: HDC;
  Size: TSize;
  LastFont: THandle;

begin
  if not (vsMultiline in FLink.FNode.States) then
  begin
    // avoid flicker
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

    DC := GetDC(Handle);
    LastFont := SelectObject(DC, Font.Handle);
    try
      // Read needed space for the current text.
      GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Size);
      Inc(Size.cx, 2 * FLink.FTree.TextMargin);

      // Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.FTree.InvalidateNode(FLink.FNode);

      if FLink.FAlignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Height))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Height));
    finally
      SelectObject(DC, LastFont);
      ReleaseDC(Handle, DC);
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.CreateParams(var Params: TCreateParams);

begin
  inherited;

  // Only with multiline style we can use the text formatting rectangle.
  // This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if vsMultiline in FLink.FNode.States then
      Style := Style and not (ES_AUTOHSCROLL or WS_HSCROLL) or WS_VSCROLL or ES_AUTOVSCROLL;
    if tsUseThemes in FLink.FTree.TreeStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSStringEdit.Release;

begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TRSFieldEditLink -----------------------------------------------------------------------------------

constructor TRSFieldEditLink.Create;

begin
  inherited;
  FEdit := TRSStringEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TRSFieldEditLink.Destroy;

begin
  FEdit.Release;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSFieldEditLink.BeginEdit: Boolean;

// Notifies the edit link that editing can start now. Descentants may cancel node edit
// by returning False.

begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    FEdit.SelectAll;
    FEdit.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSFieldEditLink.SetEdit(const Value: TRSStringEdit);

begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSFieldEditLink.CancelEdit: Boolean;

begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;

    FTree.MultiLine[FNode] := False;
    // Restore node height to default node height.
    FTree.NodeHeight[FNode] := FTree.DefaultNodeHeight;

    FEdit.Hide;
    FTree.CancelEditNode;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSFieldEditLink.EndEdit: Boolean;

begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;

    FTree.MultiLine[FNode] := False;

    // Restore node height to default height.
    FTree.NodeHeight[FNode] := FTree.DefaultNodeHeight;

    if FEdit.Modified then
      FTree.Text[FNode, FColumn] := FEdit.Text;
    FEdit.Hide;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  except
    FStopping := False;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSFieldEditLink.GetBounds: TRect;

begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRSFieldEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Retrieves the true text bounds from the owner tree.

var
  Text: WideString;
  NeededHeight: Integer;

begin
  Result := Tree is TMySQLRSGrid;
  if Result then
  begin
    FTree := Tree as TMySQLRSGrid;
    FNode := Node;
    FColumn := Column;

    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, Text);

    Text := TntAdjustLineBreaks(FTree.GetNodeRSText(Node, Column, True));
    FEdit.Text := Text;

    FTree.ScrollIntoView(FNode, False);

    FEdit.Font.Color := clBlack;
    FEdit.Parent := Tree;

    if Column <= NoColumn then
    begin
      FEdit.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
      FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

    if FEdit.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);

    // Make the node multi-lined if there are hard line breaks in it. Adjust that after everything else has been
    // set for the edit window.
    if StrPosW(PWideChar(Text), #10) <> nil then
    begin
      FTree.MultiLine[FNode] := True;

      // Node height should not exceed a certain value, in order to avoid extremely large editors.
      // Add 8 pixels, which correspond to 4 pixels distance above and below the text to the edit border.
      NeededHeight := Min(FTree.ComputeNodeHeight(FTree.Canvas, FNode, Column) + 8, 150);
      FTree.NodeHeight[FNode] := NeededHeight;
    end;

  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSFieldEditLink.ProcessMessage(var Message: TMessage);

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRSFieldEditLink.SetBounds(R: TRect);

// Sets the outer bounds of the edit control and the actual edit area in the control.

var
  Dummy: Integer;
  
begin
  if not FStopping then
  begin
    with R do
    begin
      // Make use of the gridline-space and set the edit to cover all the cell.
      Dec(Left);
      Dec(Top);
      Inc(Bottom);

      if FAlignment = taLeftJustify then
        FTree.Header.Columns.GetColumnBounds(FTree.EditColumn, R.Left, Dummy)
      else
        FTree.Header.Columns.GetColumnBounds(FTree.EditColumn, Dummy, R.Right);

      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FEdit.BoundsRect := R;

      // The selected text shall exclude the text margins (add 1 for the left border).
      R := FEdit.ClientRect;
      Inc(R.Left, FTree.TextMargin + 1);
      SendMessage(FEdit.Handle, EM_SETRECTNP, 0, Integer(@R));
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

begin
{$ifndef DELPHI_10_UP}
  // TODO: check necessity of this code once the next TNT package is out.
  @WideStringReplace := @Tnt_WideStringReplace;
{$endif DELPHI_10_UP}
end.

