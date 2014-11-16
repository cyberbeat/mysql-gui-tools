unit MyxTableEditor;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ImgList, ExtCtrls, TntExtCtrls, Buttons, TntButtons, StdCtrls,
  TntStdCtrls, CheckLst, TntCheckLst, VirtualTrees, ComCtrls, TntComCtrls,
  MyxEditor, TntForms, TntClasses, AuxFuncs, PngImage, PngTools,
  MyxTableEditorVTEdit, MyxAppOptions, ActiveX, Contnrs,
  Grt, MyxTableEditorIndexVTEdit, MyxTableEditorIndexColumnVTEdit,
  ActnList, TntActnList, Menus, TntMenus, MyxRoutineEditorFrame,
  MyxTableEditorFkVTEdit, MyxTableEditorFkColumnVTEdit,
  MyxDbUtils, UCEHighlighter, UCESQLHighlighter,
  UniCodeEditor, UCEEditorKeyCommands, MyxOptions;

type
  RdbmsDatatypeGroup = (
    RDG_NUMERIC = 0,
    RDG_STRING,
    RDG_TEXT,
    RDG_BLOB,
    RDG_DATETIME,
    RDG_GEO,
    RDG_VARIOUS,
    RDG_USER,
    RDG_STRUCTURED
  );

  PColumnNodeData = ^TColumnNodeData;
  TColumnNodeData = record
    Column: Pointer;
    SimpleDatatype,
    StructuredDatatype: Pointer;
    Caption,
    DatatypeFullCaption,
    Default,
    Comment: WideString;
    PrimaryKey,
    ForeignKey,
    NotNull,
    AutoIncrement,
    DefaultIsNull: Boolean;
    DatatypeGroup: RdbmsDatatypeGroup;
    DatatypeFlags: TTntStringList;
    ColumnFlags: TTntStringList;
  end;

  PIndexNodeData = ^TIndexNodeData;
  TIndexNodeData = record
    Index: Pointer;
    Name: WideString;
    IndexType: WideString;
    Comment: WideString;
  end;

  PIndexColumnNodeData = ^TIndexColumnNodeData;
  TIndexColumnNodeData = record
    IndexColumn: Pointer;
    Name: WideString;
    ReferedColumn: Pointer;
    ColumnLength: Integer;
    Descend: Boolean;
    StoredFunction: WideString;
    Comment: WideString;
  end;

  PFkNodeData = ^TFkNodeData;
  TFkNodeData = record
    ForeignKey: Pointer;
    Name: WideString;
    ReferedTable: Pointer;
    ReferedTableSchemaName: WideString;
    ReferedTableName: WideString;
    OnDelete: WideString;
    OnUpdate: WideString;
    Comment: WideString;
  end;

  PFkColumnNodeData = ^TFkColumnNodeData;
  TFkColumnNodeData = record
    Column: Pointer;
    Name: WideString;
    ReferedColumn: Pointer;
    ReferedColumnName: WideString;
  end;

  TMyxTableEditorForm = class(TMyxEditorForm)
    HeaderPnl: TTntPanel;
    LeftPnl: TTntPanel;
    RightPnl: TTntPanel;
    BottomPnl: TTntPanel;
    ApplyChangesBtn: TTntButton;
    TablePageControl: TTntPageControl;
    ColumnSheet: TTntTabSheet;
    ColumnVST: TVirtualStringTree;
    ColumnImgList: TImageList;
    ColumnHeaderImgList: TImageList;
    TntTabSheet1: TTntTabSheet;
    TntTabSheet4: TTntTabSheet;
    AdvancedOptionsScrollBox: TTntScrollBox;
    ScrollSpacerAdvOptShape: TTntShape;
    CancelBtn: TTntButton;
    IndicesSheet: TTntTabSheet;
    IndexVST: TVirtualStringTree;
    IndexColumnVST: TVirtualStringTree;
    IndexColumnSplitter: TTntSplitter;
    ForeignKeySheet: TTntTabSheet;
    IndexDetailsPnl: TTntPanel;
    AdvancedBtn: TTntButton;
    ForeignKeyColumnVST: TVirtualStringTree;
    ForeignKeyColumnSplitter: TTntSplitter;
    ForeignKeyVST: TVirtualStringTree;
    TntGroupBox3: TTntGroupBox;
    ColumnsDetailsPnl: TTntPanel;
    TntActionList: TTntActionList;
    NextTabsheetAction: TTntAction;
    PreviousTabsheetAction: TTntAction;
    ColumnPopupMenu: TTntPopupMenu;
    DeleteColumnsItem: TTntMenuItem;
    DeleteSelectedColumnsAction: TTntAction;
    ApplyChangeAndCloseAction: TTntAction;
    VariousGBox: TTntGroupBox;
    TntLabel19: TTntLabel;
    TntLabel20: TTntLabel;
    TntLabel21: TTntLabel;
    TntLabel51: TTntLabel;
    TntLabel52: TTntLabel;
    TntLabel53: TTntLabel;
    TntLabel54: TTntLabel;
    PackKeysComboBox: TTntComboBox;
    AutoIncEd: TTntEdit;
    TablePasswordEd: TTntEdit;
    DelayKeyUpdatesCBox: TTntCheckBox;
    RowOptionsGBox: TTntGroupBox;
    TntLabel22: TTntLabel;
    TntLabel24: TTntLabel;
    TntLabel25: TTntLabel;
    TntLabel26: TTntLabel;
    TntLabel39: TTntLabel;
    TntLabel60: TTntLabel;
    TntLabel61: TTntLabel;
    TntLabel62: TTntLabel;
    TntLabel63: TTntLabel;
    AvgRowLengthEd: TTntEdit;
    MinRowsEd: TTntEdit;
    MaxRowsEd: TTntEdit;
    UseChecksumCBox: TTntCheckBox;
    RowFormatComboBox: TTntComboBox;
    StorageOptionsGBox: TTntGroupBox;
    TntLabel27: TTntLabel;
    TntLabel28: TTntLabel;
    TntLabel55: TTntLabel;
    TntLabel56: TTntLabel;
    DataDirectoryEd: TTntEdit;
    IndexDirectoryEd: TTntEdit;
    MergeOptionGBox: TTntGroupBox;
    MergeInsertMethodCapionLbl: TTntLabel;
    UnionTablesCaptionLbl: TTntLabel;
    UnionTablesDescLbl: TTntLabel;
    MergeInsertMethodDescLbl: TTntLabel;
    MergeInsertMethodComboBox: TTntComboBox;
    UnionTablesEd: TTntEdit;
    ShowAdvancedAction: TTntAction;
    ApplyChangesAction: TTntAction;
    CloseAction: TTntAction;
    TableSheet: TTntTabSheet;
    TableSheetPnl: TTntPanel;
    EditorIcon: TTntImage;
    TableNameEd: TTntEdit;
    TableNameLbl: TTntLabel;
    TntLabel3: TTntLabel;
    TableEngineComboBox: TTntComboBox;
    TntLabel1: TTntLabel;
    CollationComboBox: TTntComboBox;
    TntMemo1: TTntMemo;
    TntLabel2: TTntLabel;
    TntLabel6: TTntLabel;
    TntLabel9: TTntLabel;
    TntLabel10: TTntLabel;
    IndexContentPnl: TTntPanel;
    FkSheetPnl: TTntPanel;
    AdvancedOptionsSheetPnl: TTntPanel;
    StdInsertsSheetPnl: TTntPanel;
    TableSheetBgShape: TTntShape;
    ColumnsContentPnl: TTntPanel;
    ColumnDetailsGroupBox: TTntGroupBox;
    TntLabel4: TTntLabel;
    TntLabel5: TTntLabel;
    TntLabel37: TTntLabel;
    TntLabel7: TTntLabel;
    TntLabel8: TTntLabel;
    ColumnNameEd: TTntEdit;
    ColumnDatatypeEd: TTntEdit;
    ColumnFlagsCheckListBox: TTntCheckListBox;
    ColumnCharsetCBox: TTntComboBox;
    ColumnCommentMemo: TTntMemo;
    SetDefValNullBtn: TTntBitBtn;
    ColumnDefaultValueEd: TTntEdit;
    N2: TTntMenuItem;
    ShowAdvancedItem: TTntMenuItem;
    RoutinesScrollBox: TTntScrollBox;
    StandardInsertsUCE: TUniCodeEdit;
    UCESQLHighlighter: TUCESQLHighlighter;
    DiscardChangesAction: TTntAction;
    RoutinesSheetTab: TTntPanel;
    TntShape1: TTntShape;
    TntShape2: TTntShape;
    TntShape3: TTntShape;
    TntShape4: TTntShape;
    ColumnsDetailsBgShape: TTntShape;
    IndexDetailsBgShape: TTntShape;
    CopyToClipboardAction: TTntAction;
    PasteFromClipboardAction: TTntAction;
    SelectAllAction: TTntAction;
    SelectAllColumnsItem: TTntMenuItem;
    CopyToClipboardItem: TTntMenuItem;
    PastFromClipboardItem: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TableNameEdChange(Sender: TObject);
    procedure TableEngineComboBoxCloseUp(Sender: TObject);
    procedure ColumnVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ColumnVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ColumnVSTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ColumnVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure ColumnVSTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ColumnVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColumnVSTEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ColumnVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ColumnVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TableNameEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure DoColumnGridEdit(var Message: TMessage); message WM_DoCellEdit;
    procedure ColumnVSTDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure ColumnVSTDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure ColumnVSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ColumnDetailChange(Sender: TObject);
    procedure SetDefValNullBtnClick(Sender: TObject);
    procedure IndexVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure IndexVSTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure IndexVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure IndexVSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure IndexVSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure IndexVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure IndexVSTDblClick(Sender: TObject);
    procedure IndexColumnVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure IndexColumnVSTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure IndexColumnVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure IndexColumnVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure IndexColumnVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure IndexColumnVSTDblClick(Sender: TObject);
    procedure ColumnVSTDblClick(Sender: TObject);
    procedure AdvancedBtnClick(Sender: TObject);
    procedure NextTabsheetActionExecute(Sender: TObject);
    procedure PreviousTabsheetActionExecute(Sender: TObject);
    procedure TablePageControlChange(Sender: TObject);
    procedure DeleteSelectedColumnsActionExecute(Sender: TObject);
    procedure ForeignKeyVSTFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ForeignKeyVSTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ForeignKeyVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ForeignKeyVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ForeignKeyVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ForeignKeyVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure ForeignKeyColumnVSTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ForeignKeyColumnVSTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ForeignKeyColumnVSTInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure ForeignKeyColumnVSTNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ForeignKeyVSTDblClick(Sender: TObject);
    procedure ForeignKeyColumnVSTDblClick(Sender: TObject);
    procedure ForeignKeyColumnVSTCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure ApplyChangeAndCloseActionExecute(Sender: TObject);
    procedure CollationComboBoxCloseUp(Sender: TObject);
    procedure PackKeysComboBoxCloseUp(Sender: TObject);
    procedure TablePasswordChange(Sender: TObject);
    procedure NextAutoIncChanged(Sender: TObject);
    procedure OnDelayKeyUpdatesChange(Sender: TObject);
    procedure RowFormatComboBoxCloseUp(Sender: TObject);
    procedure UseChecksumCheckBoxClicked(Sender: TObject);
    procedure AvgRowLengthChanged(Sender: TObject);
    procedure MinRowChanged(Sender: TObject);
    procedure MaxRowsChanged(Sender: TObject);
    procedure DataDirChanged(Sender: TObject);
    procedure IndexDirChanged(Sender: TObject);
    procedure UnionTablesChanged(Sender: TObject);
    procedure MergeInsertMethodComboBoxCloseUp(Sender: TObject);
    procedure ShowAdvancedActionExecute(Sender: TObject);
    procedure ApplyChangesActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure ApplyChangesActionUpdate(Sender: TObject);
    procedure DiscardChangesActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure PasteFromClipboardActionExecute(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure ColumnVSTSaveNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure ColumnVSTLoadNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Stream: TStream);
    procedure DeleteSelectedColumnsActionUpdate(Sender: TObject);
    procedure PasteFromClipboardActionUpdate(Sender: TObject);
  private
    FEngineMapping: TTntStringList;
    FPackKeysMapping: TTntStringList;
    FRowFormatMapping: TTntStringList;
    FMergeInsertMapping: TTntStringList;

    FRoutineFrames: TObjectList;

    FSchema: Pointer;
    FCatalog: Pointer;

    FColumnPNGImg,
    FColumnFKPNGImg,
    FColumnPKPNGImg,
    FColumnPKFKPNGImg: TPNGObject;

    FIsNullPNGImg: TPNGObject;

    FCheckmarkPNGImg,
    FOptionsCheckboxPNGImg,
    FOptionsCheckboxCheckedPNGImg: TPNGObject;

    FDatatypeImgList: TList;

    FSimpleDatatypeRefList: Pointer;

    FColumnNameInitialText: WideString;

    FInitializingColumnContent: Boolean;
    FCurrentColumn,
    FCurrentIndex: Pointer;

    FIndexTypeList: Pointer;
    FTableList: TList;
    FCurrentForeignKey: Pointer;
  protected
    procedure AddNewColumn(NodeData: PColumnNodeData; DataTypeName: WideString);
    procedure SetModified(Modified: Boolean); override;

    procedure UpdateColumnNodeData(NodeData: PColumnNodeData);

    function SetColumnPK(NodeData: PColumnNodeData;
      IsPkColumn: Boolean): Boolean;

    procedure SetCurrentColumn(Column: Pointer);

    procedure AddIndex(IndexName: WideString = '');
    procedure AddFk(FkName: WideString = '');
    procedure AddFkColumn(Column: WideString = '');

    procedure RefreshTableCache;
    procedure AddRoutineBox(Routine: Pointer = nil);

    procedure OnRoutineChange(Sender: TObject);
    procedure OnRoutineDelete(Sender: TObject);

  public
    procedure ApplyChanges; override;
    procedure StartEditObject(ObjectCopy: Pointer); override;
    procedure EndEditObject; override;

    procedure DockEditor; override;

    procedure FocusFirstControl; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Clipbrd;

//----------------------------------------------------------------------------------------------------------------------

function ColumnCanBeAutoIncrement(Column: Pointer): Boolean;

// Determines whether the datatype of the given column allows it to be also an auto incremented column. It does not
// determine if that is the case. Only the possibility is checked.
// If it can be an autoinc column the result is True, otherwise False.

var
  TypeString: WideString;

begin
  TypeString := RuntimeEnvironment.DictString[Column, 'datatypeName'];
  Result := SameText(TypeString, 'int') or SameText(TypeString, 'integer') or SameText(TypeString, 'tinyint') or
    SameText(TypeString, 'smallint') or SameText(TypeString, 'mediumint') or SameText(TypeString, 'bigint');   
end;

//----------------------------------------------------------------------------------------------------------------------

function GetColumnDatatypeAsString(Column: Pointer): WideString;

var
  SimpleDatatype: Pointer;
  Grt: TGrt;
  TypeString: WideString;
  Param: Integer;

begin
  Grt := RuntimeEnvironment;

  Result := '';

  SimpleDatatype := Grt.DictItem[Column, 'simpleDatatype'];

  if (SimpleDatatype <> nil) then
  begin
    TypeString := Grt.DictString[SimpleDatatype, 'name'];

    if (Grt.DictInt[SimpleDatatype, 'numericPrecision'] > 0) then
    begin
      Param := Grt.DictInt[Column, 'precision'];

      if (Param > 0) then
      begin
        TypeString := TypeString + '(' + IntToStr(Param);

        if (Grt.DictInt[SimpleDatatype, 'numericScale'] > 0) then
        begin
          Param := Grt.DictInt[Column, 'scale'];

          if (Param > 0) then
          begin
            TypeString := TypeString + ', ' + IntToStr(Param);
          end;
        end;

        TypeString := TypeString + ')';
      end;
    end
    else
      if (Grt.DictInt[SimpleDatatype, 'characterMaximumLength'] > 0) then
      begin
        Param := Grt.DictInt[Column, 'length'];

        TypeString := TypeString + '(' + IntToStr(Param) + ')';
      end
      else
        TypeString := TypeString + Grt.DictString[Column, 'datatypeExplicitParams']
  end
  else
  begin
    TypeString := Grt.DictString[Column, 'datatypeName'];

    Param := Grt.DictInt[Column, 'precision'];
    if (Param > 0) then
    begin
      TypeString := TypeString + '(' + IntToStr(Param);

      Param := Grt.DictInt[Column, 'scale'];
      if (Param > 0) then
      begin
        TypeString := TypeString + ', ' + IntToStr(Param);
      end;

      TypeString := TypeString + ')';
    end
    else
    begin
      Param := Grt.DictInt[Column, 'length'];

      if (Param > 0) then
      begin
        TypeString := TypeString + '(' + IntToStr(Param) + ')';
      end
      else
        TypeString := TypeString + Grt.DictString[Column, 'datatypeExplicitParams'];
    end;
  end;

  Result := TypeString;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.FormCreate(Sender: TObject);

var Stroke: TKeyStroke;

begin
  FColumnPNGImg := LoadPNGImageFromResource('column');
  FColumnFKPNGImg := LoadPNGImageFromResource('column_fk');
  FColumnPKPNGImg := LoadPNGImageFromResource('column_pk');
  FColumnPKFKPNGImg := LoadPNGImageFromResource('column_pk');

  FIsNullPNGImg := LoadPNGImageFromResource('field_overlay_null');

  FCheckmarkPNGImg := LoadPNGImageFromResource('checkmark');
  FOptionsCheckboxPNGImg := LoadPNGImageFromResource('options_checkbox');
  FOptionsCheckboxCheckedPNGImg := LoadPNGImageFromResource('options_checkbox_checked');

  FEngineMapping := TTntStringList.Create;
  TableEngineComboBox.Items.Clear;
  FEngineMapping.Add('InnoDB');
  TableEngineComboBox.Items.Add('Transactional (InnoDB)');
  FEngineMapping.Add('jstar');
  TableEngineComboBox.Items.Add('Transactional MVCC (FALCON)');
  FEngineMapping.Add('MyISAM');
  TableEngineComboBox.Items.Add('Non transactional (MyISAM)');
  FEngineMapping.Add('Memory');
  TableEngineComboBox.Items.Add('Memory only (Memory)');
  FEngineMapping.Add('Merge');
  TableEngineComboBox.Items.Add('Merge (Merge)');
  FEngineMapping.Add('NDB');
  TableEngineComboBox.Items.Add('Cluster (NDB)');
  FEngineMapping.Add('BDB');
  TableEngineComboBox.Items.Add('Berkeley DB (BDB)');
  FEngineMapping.Add('Archive');
  TableEngineComboBox.Items.Add('Archive (Archive)');
  FEngineMapping.Add('CSV');
  TableEngineComboBox.Items.Add('Comma Separated Values (CSV)');
  FEngineMapping.Add('Federated');
  TableEngineComboBox.Items.Add('Remote Tables (Federated)');

  // The order of the images has to match the RdbmsDatatypeGroup type
  FDatatypeImgList := TList.Create;
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_numeric'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_string'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_text'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_blob'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_datetime'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_spatial'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_userdefined'));
  FDatatypeImgList.Add(LoadPNGImageFromResource('datatype_structured'));

  FInitializingContent := False;
  FInitializingColumnContent := False;
  FCurrentColumn := nil;
  FCurrentIndex := nil;

  FIndexTypeList := Grt.ListNew(GrtStringValue);
  FCurrentForeignKey := nil;

  Grt.ListAddString(FIndexTypeList, 'INDEX');
  Grt.ListAddString(FIndexTypeList, 'PRIMARY');
  Grt.ListAddString(FIndexTypeList, 'UNIQUE');
  Grt.ListAddString(FIndexTypeList, 'FULLTEXT');
  Grt.ListAddString(FIndexTypeList, 'SPATIAL');

  TablePageControl.ActivePageIndex := 0;

  if (IsWinXP) then
  begin
    IndexColumnSplitter.Color := clWhite;
    ForeignKeyColumnSplitter.Color := clWhite;
    AdvancedOptionsScrollBox.Color := clWhite;
  end;

  MergeInsertMethodComboBox.Items.Clear;
  MergeInsertMethodComboBox.Items.Add(_('No Inserts'));
  MergeInsertMethodComboBox.Items.Add(_('Inserts into first table'));
  MergeInsertMethodComboBox.Items.Add(_('Inserts into last table'));

  FPackKeysMapping := TTntStringList.Create;
  PackKeysComboBox.Items.Clear;
  PackKeysComboBox.Items.Add(_('Default'));
  FPackKeysMapping.Add('DEFAULT');
  PackKeysComboBox.Items.Add(_('Pack None'));
  FPackKeysMapping.Add('0');
  PackKeysComboBox.Items.Add(_('Pack All'));
  FPackKeysMapping.Add('1');

  FRowFormatMapping := TTntStringList.Create;
  RowFormatComboBox.Items.Clear;
  RowFormatComboBox.Items.Add(_('Default'));
  FRowFormatMapping.Add('DEFAULT');
  RowFormatComboBox.Items.Add(_('Dynamic'));
  FRowFormatMapping.Add('DYNAMIC');
  RowFormatComboBox.Items.Add(_('Fixed'));
  FRowFormatMapping.Add('FIXED');
  RowFormatComboBox.Items.Add(_('Compressed'));
  FRowFormatMapping.Add('COMPRESSED');
  RowFormatComboBox.Items.Add(_('Redundant'));
  FRowFormatMapping.Add('REDUNDANT');
  RowFormatComboBox.Items.Add(_('Compact'));
  FRowFormatMapping.Add('COMPACT');

  FMergeInsertMapping := TTntStringList.Create;
  MergeInsertMethodComboBox.Items.Clear;
  MergeInsertMethodComboBox.Items.Add(_('First'));
  FMergeInsertMapping.Add('FIRST');
  MergeInsertMethodComboBox.Items.Add(_('Last'));
  FMergeInsertMapping.Add('LAST');

  FRoutineFrames := TObjectList.Create;
  
  UpdateCollations(CollationComboBox);

  // Setup the SQL highlighter.
  with UCESQLHighlighter do
  begin
    CommentAttributes.Foreground := clGray;
    CommentAttributes.Style := [fsItalic];
    KeyAttributes.Foreground := clBlue;
    StringAttributes.Foreground := $0080FF;
    IdentifierAttributes.Foreground := clWindowText;
    NumberAttributes.Foreground := clFuchsia;
    SpaceAttributes.Foreground := clWindow;
    SymbolAttributes.Foreground := clWindowText;

    IdentifierAttributes.Foreground := clWindowText;
    QuotedIDAttributes.Foreground := $804000;
    QuotedIDAttributes.Style := [fsBold];

    EmbeddedCommandAttributes.Foreground := clNavy;
    EmbeddedCommandAttributes.Background := $F0F0F0;

    SystemVariableAttributes.Foreground := $808000;
    SystemVariableAttributes.Style := [fsBold];
    UserVariableAttributes.Foreground := $C08080;
    UserVariableAttributes.Style := [fsBold];
  end;

  // Setup StandardInsertsUCE
  with StandardInsertsUCE do
  begin
    Color := clWindow;
    Font.Name := CommonOptions.OptionString['DefaultCodeFontName'];
    Font.Height := CommonOptions.OptionInt['DefaultCodeFontHeight'];
    Highlighter := UCESQLHighlighter;
    Options := [eoAutoIndent, eoAutoUnindent, eoGroupUndo, eoInserting, eoLineNumbers, eoShowScrollHint, eoSmartTabs,
      eoTripleClicks, eoUndoAfterSave, eoUseSyntaxHighlighting, eoWantTabs, eoUseUndoRedo];

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
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AddRoutineBox(Routine: Pointer);

var
  RoutineFrame: TRoutineFrame;
  RoutineFrameHeight, RoutineFrameExpanded: Integer;
  List, Val: Pointer;

begin
  RoutineFrame := TTriggerFrame.Create(nil);
  RoutineFrame.Parent := RoutinesScrollBox;
  RoutineFrame.Top := 10000;
  RoutineFrame.Align := alTop;
  RoutineFrame.OnChange := OnRoutineChange;
  RoutineFrame.OnDelete := OnRoutineDelete;
  //RoutineFrame.OnHeaderDrag := DoRoutineHeaderDrag;
  //RoutineFrame.OnHeaderDragStart := DoRoutineHeaderDragStart;
  //RoutineFrame.OnHeaderDragStop := DoRoutineHeaderDragStop;

  RoutineFrame.Schema := ObjectCopy;
  RoutineFrame.Routine := Routine;

  RoutineFrameHeight := 130;
  List := Grt.DictItem[ObjectCopy, 'routineExpandedHeights'];
  if (List <> nil) then
  begin
    Val := Grt.ListItem[List, FRoutineFrames.Count];
    if (Val <> nil) then
      RoutineFrameHeight := Grt.ValueInt[Val];
  end;


  RoutineFrame.AutoCalcSize := (RoutineFrameHeight = 0);
  RoutineFrame.Height := RoutineFrameHeight;
  RoutineFrame.ExpandedHeight := RoutineFrame.Height;

  RoutineFrameExpanded := 1;
  List := Grt.DictItem[ObjectCopy, 'routineExpandedStates'];
  if (List <> nil) then
  begin
    Val := Grt.ListItem[List, FRoutineFrames.Count];
    if (Val <> nil) then
      RoutineFrameExpanded := Grt.ValueInt[Val];
  end;

  RoutineFrame.Expanded := (RoutineFrameExpanded = 1);

  FRoutineFrames.Add(RoutineFrame);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.OnRoutineChange(Sender: TObject);

var
  RoutineList: Pointer;

begin
  if (FInitializingContent) then
    Exit;

  // if this is the last RoutineFrame, add a new one
  if (FRoutineFrames[FRoutineFrames.Count - 1] = Sender) and
    (Length(TRoutineFrame(Sender).RoutineUce.Content.Text) > 0) then
  begin
    AddRoutineBox;

    if (Sender is TRoutineFrame) then
    begin
      RoutineList := Grt.DictItem[ObjectCopy, 'triggers'];

      Grt.ListAdd(RoutineList, TRoutineFrame(Sender).Routine);
      //Grt.ListAdd(RoutineList,
      //  Grt.DictItem[TRoutineFrame(Sender).Routine, '_id']);
    end;
  end;

  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.OnRoutineDelete(Sender: TObject);

var
  RoutineList: Pointer;
  I, Count: Integer;

begin
  RoutineList := Grt.DictItem[FObj, 'triggers'];
  Count := Grt.ListCount(RoutineList);

  for I := 0 to Count - 1 do
  begin
    if (Grt.ListItem[RoutineList, I] =
      TRoutineFrame(Sender).Routine) then
    begin
      Grt.ListDel(RoutineList, I);
      break;
    end;
  end;

  FRoutineFrames.Remove(Sender);

  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.FormDestroy(Sender: TObject);

var
  I: Integer;

begin
  FEngineMapping.Free;

  FCheckmarkPNGImg.Free;
  FOptionsCheckboxPNGImg.Free;
  FOptionsCheckboxCheckedPNGImg.Free;

  FColumnPNGImg.Free;
  FColumnFKPNGImg.Free;
  FColumnPKPNGImg.Free;

  FIsNullPNGImg.Free;

  for I := 0 to FDatatypeImgList.Count - 1 do
    TPNGObject(FDatatypeImgList[I]).Free;
  FDatatypeImgList.Free;

  Grt.ValueRelease(FIndexTypeList);

  FTableList.Free;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);

begin
  Action := caFree;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.StartEditObject(ObjectCopy: Pointer);

var
  RoutineList: Pointer;
  I, Count: Integer;

begin
  inherited;

  // Initialize editor
  FInitializingContent := True;
  try
    // get RDBMS
    // schema
    FSchema := Grt.DictRef[ObjectCopy, 'owner'];
    if (FSchema <> nil) then
    begin
      // catalog
      FCatalog := Grt.DictRef[FSchema, 'owner'];

      if (FCatalog <> nil) then
        FSimpleDatatypeRefList := Grt.DictItem[
          FCatalog, 'simpleDatatypes'];
    end;

    // name
    TableNameEd.Text := Grt.DictString[ObjectCopy, 'name'];

    if (ObjectStructName = 'db.mysql.Table') then
    begin
      // engine
      TableEngineComboBox.ItemIndex :=
        FEngineMapping.IndexOf(Grt.DictString[ObjectCopy, 'tableEngine']);
      TableEngineComboBox.Enabled := True;

      SetCollations(CollationComboBox,
        Grt.DictString[ObjectCopy, 'defaultCollationName'],
        Grt.DictString[FSchema, 'defaultCollationName']);
    end
    else
    begin
      TableEngineComboBox.ItemIndex := -1;
      TableEngineComboBox.Enabled := False;

      CollationComboBox.ItemIndex := -1;
      CollationComboBox.Enabled := False;
    end;

    // columns
    ColumnVST.NodeDataSize := sizeof(TColumnNodeData);

    ColumnVST.RootNodeCount :=
      Grt.ListCount(Grt.DictItem[ObjectCopy, 'columns']) + 1;

    // indices
    IndexVST.NodeDataSize := sizeof(TIndexNodeData);
    IndexVST.RootNodeCount :=
      Grt.ListCount(Grt.DictItem[ObjectCopy, 'indices']) + 1;

    // foreign keys
    ForeignKeyVST.NodeDataSize := sizeof(TFkNodeData);
    ForeignKeyVST.RootNodeCount :=
      Grt.ListCount(Grt.DictItem[ObjectCopy, 'foreignKeys']) + 1;

    // table options
    PackKeysComboBox.ItemIndex := FPackKeysMapping.IndexOf(Grt.DictString[ObjectCopy, 'packKeys']);
    RowFormatComboBox.ItemIndex := FRowFormatMapping.IndexOf(Grt.DictString[ObjectCopy, 'rowFormat']);
    MergeInsertMethodComboBox.ItemIndex := FMergeInsertMapping.IndexOf(Grt.DictString[ObjectCopy, 'mergeInsert']);
    UnionTablesEd.Text := Grt.DictString[ObjectCopy, 'mergeUnion'];
    AutoIncEd.Text := Grt.DictString[ObjectCopy, 'nextAutoInc'];
    AvgRowLengthEd.Text := Grt.DictString[ObjectCopy, 'avgRowLength'];
    TablePasswordEd.Text := Grt.DictString[ObjectCopy, 'password'];
    MinRowsEd.Text := Grt.DictString[ObjectCopy, 'minRows'];
    MaxRowsEd.Text := Grt.DictString[ObjectCopy, 'maxRows'];
    DataDirectoryEd.Text := Grt.DictString[ObjectCopy, 'tableDataDir'];
    IndexDirectoryEd.Text := Grt.DictString[ObjectCopy, 'tableIndexDir'];

    if(Grt.DictInt[ObjectCopy, 'delayKeyWrite'] = 1)then
      DelayKeyUpdatesCBox.Checked := true
    else
      DelayKeyUpdatesCBox.Checked := false;

    if(Grt.DictInt[ObjectCopy, 'checksum'] = 1)then
      UseChecksumCBox.Checked := true
    else
      UseChecksumCBox.Checked := false;


    RoutineList := Grt.DictItem[FObj, 'triggers'];
    Count := Grt.ListCount(RoutineList);

    for I := 0 to Count do
    begin
      if (I < Count) then
        AddRoutineBox(Grt.ListItem[RoutineList, I])
      else
        AddRoutineBox;
    end;

  finally
    FInitializingContent := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.EndEditObject;

begin
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.TableNameEdChange(Sender: TObject);

begin
  if (Grt.DictString[ObjectCopy, 'name'] <> TableNameEd.Text) then
  begin
    Grt.DictString[ObjectCopy, 'name'] := TableNameEd.Text;

    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.TableEngineComboBoxCloseUp(Sender: TObject);

begin
  if (Grt.DictString[ObjectCopy, 'tableEngine'] <>
    FEngineMapping[TableEngineComboBox.ItemIndex]) then
  begin
    Grt.DictString[ObjectCopy, 'tableEngine'] :=
      FEngineMapping[TableEngineComboBox.ItemIndex];

    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PColumnNodeData;
  Columns: Pointer;

begin
  NodeData := Sender.GetNodeData(Node);

  Columns := Grt.DictItem[ObjectCopy, 'columns'];
  if (Node.Index < Cardinal(Grt.ListCount(Columns))) then
    NodeData.Column := Grt.ListItem[Columns, Node.Index];

  UpdateColumnNodeData(NodeData);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.UpdateColumnNodeData(NodeData: PColumnNodeData);

var
  Column,
  FlagList,
  PrimaryKey,
  PrimaryKeyColumns: Pointer;
  PrimaryKeyColumnId: WideString;
  Id: WideString;
  I: Integer;

begin
  Column := NodeData.Column;

  if (Column <> nil) then
  begin
    NodeData.Caption := Grt.DictString[Column, 'name'];

    // PK
    Id := Grt.DictString[Column, '_id'];
    NodeData.PrimaryKey := False;
    PrimaryKey := Grt.DictRef[ObjectCopy, 'primaryKey'];
    if (PrimaryKey <> nil) then
    begin
      PrimaryKeyColumns := Grt.DictItem[PrimaryKey, 'columns'];
      for I := 0 to Grt.ListCount(PrimaryKeyColumns) - 1 do
      begin
        PrimaryKeyColumnId := Grt.DictString[
          Grt.ListItem[PrimaryKeyColumns, I], 'referedColumn'];

        if (PrimaryKeyColumnId = Id) then
        begin
          NodeData.PrimaryKey := True;
          break;
        end;
      end;
    end;

    // NN, AI
    NodeData.NotNull := (Grt.DictInt[Column, 'isNullable'] = 0);
    NodeData.AutoIncrement := (Grt.DictInt[Column, 'autoIncrement'] = 1);

    //Default value
    NodeData.DefaultIsNull := (Grt.DictInt[Column, 'defaultValueIsNull'] = 1);
    if (Not(NodeData.DefaultIsNull)) then
      NodeData.Default := Grt.DictString[Column, 'defaultValue']
    else
      NodeData.Default := '';

    // Comment
    NodeData.Comment := Grt.DictString[Column, 'comment'];

    // Datatype
    NodeData.DatatypeFullCaption :=
      Grt.DictString[Column, 'datatypeName'];

    NodeData.SimpleDatatype := Grt.DictRef[Column, 'simpleType'];
    NodeData.StructuredDatatype := Grt.DictRef[Column, 'structuredDatatype'];
    if (NodeData.SimpleDatatype <> nil) then
    begin
      Id := Grt.DictString[NodeData.SimpleDatatype, 'group'];

      if (Id = '{A0B5D6E6-E94C-482A-8608-8EF7AF4FB560}') then
        NodeData.DatatypeGroup := RDG_NUMERIC
      else
        if (Id = '{92F0945A-4658-47A0-A3EA-423F4AB13694}') then
          NodeData.DatatypeGroup := RDG_STRING
        else
          if (Id = '{F48E12A6-C67D-43EC-8FA6-EC30D75E5528}') then
            NodeData.DatatypeGroup := RDG_TEXT
          else
            if (Id = '{640A39CD-A559-4366-BBCD-734F638B361D}') then
              NodeData.DatatypeGroup := RDG_BLOB
            else
              if (Id = '{06431E19-2E93-4DE3-8487-A8DF4A25DA96}') then
                NodeData.DatatypeGroup := RDG_DATETIME
              else
                if (Id = '{C1ABB46A-9AF9-420C-B31B-95AFE921C085}') then
                  NodeData.DatatypeGroup := RDG_GEO
                else
                  if (Id = '{BBE4BC2C-D462-46B7-AC2E-9F0CE17C3686}') then
                    NodeData.DatatypeGroup := RDG_VARIOUS
                  else
                    if (Id = '{3D44C889-03BA-4783-A02D-ACDF4943257E}') then
                      NodeData.DatatypeGroup := RDG_USER;

      // get the datatypes full caption
      NodeData.DatatypeFullCaption := GetColumnDatatypeAsString(Column);
    end
    else
      if (NodeData.StructuredDatatype <> nil) then
      begin
        NodeData.DatatypeGroup := RDG_STRUCTURED;
      end
      else
      begin
        // if no simple or structured datatype is set,
        // simply take the parameters
        NodeData.DatatypeFullCaption := GetColumnDatatypeAsString(Column);
      end;
  end;

  // All available datatype flags
  if (NodeData.DatatypeFlags = nil) then
    NodeData.DatatypeFlags := TTntStringList.Create
  else
    NodeData.DatatypeFlags.Clear;

  if (NodeData.SimpleDatatype <> nil) then
  begin
    FlagList := Grt.DictItem[NodeData.SimpleDatatype, 'flags'];
    for I := 0 to Grt.ListCount(FlagList) - 1 do
      NodeData.DatatypeFlags.Add(Grt.ListString[FlagList, I]);
  end;

  // All set datatype flags
  if (NodeData.ColumnFlags = nil) then
    NodeData.ColumnFlags := TTntStringList.Create
  else
    NodeData.ColumnFlags.Clear;

  if (NodeData.Column <> nil) then
  begin
    FlagList := Grt.DictItem[NodeData.Column, 'flags'];
    for I := 0 to Grt.ListCount(FlagList) - 1 do
      NodeData.ColumnFlags.Add(Grt.ListString[FlagList, I]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  NodeData.DatatypeFlags.Free;
  NodeData.ColumnFlags.Free;

  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: PColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  case Column of
    0: CellText := NodeData.Caption;
    1: CellText := NodeData.DatatypeFullCaption;
    5: CellText := NodeData.Default;
    6: CellText := NodeData.Comment;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PColumnNodeData;
  I, P, L: integer;
  S: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.Column = nil) then
    Exit;

  case Column of
    0:
      if (Not(NodeData.PrimaryKey)) and (Not(NodeData.ForeignKey)) then
        FColumnPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 1,
          CellRect.Top + 1, CellRect.Left + 17, CellRect.Top + 17))
      else
        if (NodeData.PrimaryKey) and (Not(NodeData.ForeignKey)) then
          FColumnPKPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 1,
            CellRect.Top + 1, CellRect.Left + 17, CellRect.Top + 17))
        else
          if (NodeData.PrimaryKey) and (NodeData.ForeignKey) then
            FColumnPKFKPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 1,
              CellRect.Top + 1, CellRect.Left + 17, CellRect.Top + 17))
          else
            FColumnFKPNGImg.Draw(TargetCanvas, Rect(CellRect.Left + 1,
              CellRect.Top + 1, CellRect.Left + 17, CellRect.Top + 17));
    1:
      if (Ord(NodeData.DatatypeGroup) < FDatatypeImgList.Count) then
        TPNGObject(FDatatypeImgList[Ord(NodeData.DatatypeGroup)]).Draw(
          TargetCanvas, Rect(CellRect.Left+1,
          CellRect.Top+1, CellRect.Left+17, CellRect.Top+17))
      else
        TPNGObject(FDatatypeImgList[FDatatypeImgList.Count - 2]).Draw(
          TargetCanvas, Rect(CellRect.Left+1,
          CellRect.Top+1, CellRect.Left+17, CellRect.Top+17));
    2:
      //Not null checkmark
      if (NodeData.NotNull) then
        FCheckmarkPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+5,
          CellRect.Top+1, CellRect.Left+21, CellRect.Top+17));
    3:
      //Auto Increment checkmark
      if (NodeData.AutoIncrement) then
        FCheckmarkPNGImg.Draw(TargetCanvas, Rect(CellRect.Left+5,
          CellRect.Top+1, CellRect.Left+21, CellRect.Top+17));
    4:
      begin
        if (NodeData.DatatypeFlags <> nil) then
        begin
          P:=5;

          SetBkMode(TargetCanvas.Handle, TRANSPARENT);
          TargetCanvas.Font.Color:=clBlack;

          for I := 0 to NodeData.DatatypeFlags.Count - 1 do
          begin
            S := NodeData.DatatypeFlags[I];

            if (NodeData.ColumnFlags.IndexOf(S) > -1) then
              FOptionsCheckboxCheckedPNGImg.Draw(TargetCanvas,
                Rect(CellRect.Left + P, CellRect.Top + 1,
                  CellRect.Left + 16 + P, CellRect.Top + 17))
            else
              FOptionsCheckboxPNGImg.Draw(TargetCanvas,
                Rect(CellRect.Left + P, CellRect.Top + 1,
                  CellRect.Left + 16 + P, CellRect.Top + 17));

            P := P + 16 + 5;
            L := GetWideStringTextWidth(TargetCanvas, S);

            DrawWideStringText(TargetCanvas.Handle,
              PWideChar(S),
              Length(S),
              Rect(CellRect.Left + P, CellRect.Top + 2,
                CellRect.Left + P + L, CellRect.Top + 16));

            P := P + L + 10;
          end;
        end;
      end;
    5:
      if (NodeData.DefaultIsNull) then
        FIsNullPNGImg.Draw(TargetCanvas,
          Rect(CellRect.Left + 2, CellRect.Top + 2,
            CellRect.Left + 2 + FIsNullPNGImg.Width,
            CellRect.Top + 2 + FIsNullPNGImg.Height));
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if (Column <= 1) then
    ImageIndex := 0;
end;

// -----------------------------------------------------------------------------

function TMyxTableEditorForm.SetColumnPK(NodeData: PColumnNodeData;
  IsPkColumn: Boolean): Boolean;

var
  PrimaryKey,
  PKColumns,
  PKColumn: Pointer;
  PKColumnId,
  StructName,
  Id: WideString;
  I: Integer;
  IndexList: Pointer;

begin
  Result := False;

  IndexList := Grt.DictItem[ObjectCopy, 'indices'];

  if (NodeData.PrimaryKey) and (Not(IsPkColumn)) then
  begin
    PrimaryKey := Grt.DictRef[ObjectCopy, 'primaryKey'];

    if (PrimaryKey <> nil) then
    begin
      PKColumns := Grt.DictItem[PrimaryKey, 'columns'];

      Id := Grt.DictString[NodeData.Column, '_id'];

      for i := 0 to Grt.ListCount(PKColumns) do
      begin
        PKColumnId := Grt.DictString[
          Grt.ListItem[PKColumns, I], 'referedColumn'];

        if (PKColumnId = Id) then
        begin
          Grt.ListDel(PKColumns, I);
          break;
        end;
      end;

      // if there are no more columns left,
      // remove to PK completely
      if (Grt.ListCount(PKColumns) = 0) then
      begin
        Grt.DictDel(ObjectCopy, 'primaryKey');

        Grt.ListDel(IndexList, PrimaryKey);
      end;
    end;

    NodeData.PrimaryKey := False;

    Result := True;
  end
  else
    if (Not(NodeData.PrimaryKey)) and (IsPkColumn) then
    begin
      PrimaryKey := Grt.DictRef[ObjectCopy, 'primaryKey'];

      // Create PK if it is not already there
      if (PrimaryKey = nil) then
      begin
        StructName := Grt.GetStructMemberContentStructName(
          ObjectStructName, 'primaryKey');

        PrimaryKey := Grt.ObjectNew(StructName,
          'PRIMARY', '', Grt.DictString[ObjectCopy, '_id']);

        // make sure the PK is cached
        Grt.AddObjToReferenceCache(PrimaryKey);


        Grt.ListAdd(IndexList, PrimaryKey, False);

        Grt.DictInt[PrimaryKey, 'isPrimary'] := 1;
        Grt.DictInt[PrimaryKey, 'unique'] := 1;
        Grt.DictString[PrimaryKey, 'indexType'] := 'PRIMARY';

        // set PK 
        Grt.DictString[ObjectCopy, 'primaryKey'] :=
          Grt.DictString[PrimaryKey, '_id'];
      end
      else
        StructName := Grt.DictStructName[PrimaryKey];

      // Create new index column
      StructName := Grt.GetStructMemberContentStructName(
        StructName, 'columns');

      PKColumn := Grt.ObjectNew(StructName,
        Grt.DictString[NodeData.Column, 'name'], '',
        Grt.DictString[PrimaryKey, '_id']);

      Grt.DictString[PKColumn, 'referedColumn'] :=
        Grt.DictString[NodeData.Column, '_id'];

      // make sure the column is cached
      Grt.AddObjToReferenceCache(NodeData.Column);        

      // Add column's _id to the primaryKey's columns list
      PKColumns := Grt.DictItem[PrimaryKey, 'columns'];

      Grt.ListAdd(PKColumns, PKColumn);

      NodeData.PrimaryKey := True;

      Result := True;
    end;

  // if the PK has changed, update the index list
  if (Result) then
  begin
    IndexVST.Clear;
    IndexVST.RootNodeCount := Grt.ListCount(IndexList) + 1;
    IndexVST.FocusedNode := IndexVST.GetFirst;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

var HitInfo: THitInfo;
  CLeft, CRight: integer;
  NodeData: PColumnNodeData;
  ColumnFlags: Pointer;
  Xpos,
  I,
  L,
  FlagIndex: integer;
  S: WideString;

begin
  ColumnVST.GetHitTestInfoAt(X, Y, True, HitInfo);

  if Assigned(HitInfo.HitNode) then
  begin
    if Button = mbRight then
    begin
      ColumnVST.FocusedNode := HitInfo.HitNode;
      if not ColumnVST.Selected[HitInfo.HitNode] then
      begin
        if Shift * [ssCtrl, ssShift] = [] then
          ColumnVST.ClearSelection;
        ColumnVST.Selected[HitInfo.HitNode] := True;
      end;
    end;

    if Button = mbLeft then
    begin
      NodeData := ColumnVST.GetNodeData(HitInfo.HitNode);
      case HitInfo.HitColumn of
        0: //Switch Primary Key
          begin
            ColumnVST.Header.Columns.GetColumnBounds(HitInfo.HitColumn,
              CLeft, CRight);

            if X < 16 then
            begin
              Modified := SetColumnPK(NodeData, Not(NodeData.PrimaryKey));

              ColumnVST.InvalidateNode(HitInfo.HitNode);
              SetCurrentColumn(NodeData.Column);
            end;
          end;
        2: // Switch NotNull
          begin
            NodeData.NotNull := not NodeData.NotNull;
            Grt.DictInt[NodeData.Column, 'isNullable'] := Ord(not NodeData.NotNull);
            Modified := True;
            ColumnVST.InvalidateNode(HitInfo.HitNode);
            SetCurrentColumn(NodeData.Column);
          end;
        3: // Switch AutoIncrement if a numeric column
          if ColumnCanBeAutoIncrement(NodeData.Column) then
          begin
            NodeData.AutoIncrement := not NodeData.AutoIncrement;
            Grt.DictInt[NodeData.Column, 'autoIncrement'] := Ord(NodeData.AutoIncrement);
            Modified := True;
            ColumnVST.InvalidateNode(HitInfo.HitNode);
            SetCurrentColumn(NodeData.Column);
          end;
        4: //Switch Datatype Flags
          begin
            ColumnVST.Header.Columns.GetColumnBounds(HitInfo.HitColumn, CLeft, CRight);
            Xpos := 5;
            if NodeData.DatatypeFlags.Count > 0 then
            begin
              ColumnFlags := Grt.DictItem[NodeData.Column, 'flags'];
              for I := 0 to NodeData.DatatypeFlags.Count - 1 do
              begin
                S := NodeData.DatatypeFlags[I];
                L := GetWideStringTextWidth(ColumnVST.Canvas, S);

                if (X >= CLeft + Xpos) and (X <= CLeft + Xpos + 16 + 5 + L + 10) then
                begin
                  FlagIndex := NodeData.ColumnFlags.IndexOf(S);

                  // Handle flag toggle
                  if FlagIndex = -1 then
                  begin
                    //Special processing for CHAR
                    if SameText(Grt.DictString[NodeData.SimpleDatatype, 'name'], 'CHAR') then
                    begin
                      if SameText(S, 'ASCII') then
                        if(NodeData.ColumnFlags.IndexOf('UNICODE')>-1)then
                          NodeData.ColumnFlags.Delete(NodeData.ColumnFlags.IndexOf('UNICODE'));
                      if SameText(s, 'UNICODE') then
                        if(NodeData.ColumnFlags.IndexOf('ASCII')>-1)then
                          NodeData.ColumnFlags.Delete(NodeData.ColumnFlags.IndexOf('ASCII'));
                    end;

                    Grt.ListAddString(ColumnFlags, s);
                  end
                  else
                    Grt.ListDel(ColumnFlags, FlagIndex);

                  // Only one flag can be changed at a time so we can stop processing here.
                  Break;
                end;

                Xpos := Xpos + 16 + 5 + L + 10;
              end;

              Modified := True;

              ColumnVST.InvalidateNode(HitInfo.HitNode);
              UpdateColumnNodeData(NodeData);
              SetCurrentColumn(NodeData.Column);
            end;
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

begin
  if (Column<=1) or (Column >= 5) then
  begin
    Allowed := True;

    if (ColumnVST.RootNodeCount = 1) and (Column = 0) then
      FColumnNameInitialText := ReplaceTags(
        AppOptions.OptionString['PkColumnNameTemplate'],
        '%tablename%='+TableNameEd.Text);
  end
  else
    Allowed := False;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);

var
  NodeData: PColumnNodeData;
  StructName: WideString;
  Columns: Pointer;
  DatatypeName: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Trim(NewText) <> '') or (Column = 5) or (Column = 6) then
  begin
    Modified:=True;

    //Add new column
    if (NodeData.Column = nil) then
    begin
      StructName := Grt.GetStructMemberContentStructName(
        ObjectStructName, 'columns');

      NodeData.Column := Grt.ObjectNew(StructName, 'new_column',
        '', Grt.DictString[ObjectCopy, '_id']);

      if (Node.Index = 0) then
        DatatypeName := 'INT'
      else
        DatatypeName := 'VARCHAR';

      Grt.DictString[NodeData.Column, 'datatypeName'] := DatatypeName;

      // Set simple datatype
      if (FSimpleDatatypeRefList <> nil) then
        Grt.DictString[NodeData.Column, 'simpleType'] :=
          Grt.DictString[
            Grt.ListRefValueByObjectName[
              FSimpleDatatypeRefList, DatatypeName],
            '_id'];

      Grt.DictInt[NodeData.Column, 'defaultValueIsNull'] := 1;

      if (Node.Index = 0) then
        Grt.DictInt[NodeData.Column, 'precision'] := 0
      else
        Grt.DictInt[NodeData.Column, 'length'] := 45;

      Columns := Grt.DictItem[ObjectCopy, 'columns'];
      if (Grt.ListCount(Columns) = 0) then
      begin
        Grt.DictInt[NodeData.Column, 'isNullable'] := 0;
        Grt.DictInt[NodeData.Column, 'autoIncrement'] := 1;

        Grt.DictInt[NodeData.Column, 'unsigned'] := 1;
        Grt.ListAdd(Grt.DictItem[NodeData.Column, 'flags'],
          Grt.ValueFromString('UNSIGNED'));
      end
      else
        Grt.DictInt[NodeData.Column, 'isNullable'] := 1;

      UpdateColumnNodeData(NodeData);

      if (Grt.ListCount(Columns) = 0) then
        SetColumnPK(NodeData, True);

      Grt.ListAdd(Grt.DictItem[ObjectCopy, 'columns'], NodeData.Column);

      ColumnVST.RootNodeCount := ColumnVST.RootNodeCount + 1;
    end;

    case Column of
      0:
        begin
          Grt.DictString[NodeData.Column, 'name'] := NewText;

          // update indices
          IndexVST.OnFocusChanged(IndexVST, IndexVST.FocusedNode,
            IndexVST.FocusedColumn);
        end;
      1:
        Grt.ExecuteStandardTask(_('Setting column datatype'), 'DbUtils', 'setColumnDatatypeByString',
          [FSimpleDatatypeRefList, NodeData.Column, WideUpperCase(NewText)], True);
      5:
        begin
          Grt.DictString[NodeData.Column, 'defaultValue'] := NewText;
          if (NewText <> '') then
            Grt.DictInt[FCurrentColumn, 'defaultValueIsNull'] := 0;
        end;
      6:
        Grt.DictString[NodeData.Column, 'comment'] := NewText;
    end;

    if not ColumnCanBeAutoIncrement(NodeData.Column) then
      Grt.DictInt[NodeData.Column, 'autoIncrement'] := 0;

    // update column grid
    UpdateColumnNodeData(NodeData);
    ColumnVST.InvalidateNode(Node);

    // udate column details
    SetCurrentColumn(NodeData.Column);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  EditLink := TColumnGridEditLink.Create(
    FSimpleDatatypeRefList, FColumnNameInitialText);

  FColumnNameInitialText:='';
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.TableNameEdKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

var
  Mgs: TMsg;
  Columns: Pointer;

begin
  if (Key = VK_Return) and (ssShift in Shift) then
  begin
    FEditorTabFrame.SelectedTab := 1;

    Columns := Grt.DictItem[ObjectCopy, 'columns'];
    if (Grt.ListCount(Columns) = 0) then
    begin
      ColumnVST.SetFocus;
      ColumnVST.FocusedNode := ColumnVST.GetFirst;
      ColumnVST.ClearSelection;

      if(ColumnVST.FocusedNode<>nil)then
      begin
        ColumnVST.Selected[ColumnVST.FocusedNode] := True;

        ColumnVST.EditNode(ColumnVST.FocusedNode, 0);
      end;
    end;

    Key:=0;
    PeekMessage(Mgs, 0, WM_CHAR, WM_CHAR, PM_REMOVE);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.DoColumnGridEdit(var Message: TMessage);

var
  Tree: TVirtualStringTree;

begin
  Tree := nil;

  if (HWND(Message.WParam) = ColumnVST.Handle) then
    Tree := ColumnVST
  else
    if (HWND(Message.WParam) = IndexVST.Handle) then
      Tree := IndexVST
  else
    if (HWND(Message.WParam) = IndexColumnVST.Handle) then
      Tree := IndexColumnVST
  else
    if (HWND(Message.WParam) = ForeignKeyVST.Handle) then
      Tree := ForeignKeyVST
  else
    if (HWND(Message.WParam) = ForeignKeyColumnVST.Handle) then
      Tree := ForeignKeyColumnVST;

  if (Tree <> nil) then
  begin
    if (Tree.FocusedNode = nil) then
      Tree.FocusedNode := Tree.GetFirst;

    if (Tree.FocusedNode <> nil) then
      Tree.EditNode(Tree.FocusedNode, Tree.FocusedColumn);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.AddNewColumn(NodeData: PColumnNodeData; DataTypeName: WideString);

// Adds a new column to the columns treeview and fills it with certain default values.

var
  StructName: WideString;
  Columns: Pointer;

begin
  StructName := Grt.GetStructMemberContentStructName(ObjectStructName, 'columns');
  NodeData.Column := Grt.ObjectNew(StructName, 'new_column', '', Grt.DictString[ObjectCopy, '_id']);
  Grt.DictString[NodeData.Column, 'datatypeName'] := DatatypeName;

  // Set simple datatype
  if (FSimpleDatatypeRefList <> nil) then
    Grt.DictString[NodeData.Column, 'simpleType'] :=
      Grt.DictString[Grt.ListRefValueByObjectName[FSimpleDatatypeRefList, DatatypeName], '_id'];

  Grt.DictInt[NodeData.Column, 'defaultValueIsNull'] := 1;

  if SameText(DataTypeName, 'int') then
    Grt.DictInt[NodeData.Column, 'precision'] := 0
  else
    Grt.DictInt[NodeData.Column, 'length'] := 45;

  Columns := Grt.DictItem[ObjectCopy, 'columns'];
  if (Grt.ListCount(Columns) = 0) then
  begin
    Grt.DictInt[NodeData.Column, 'isNullable'] := 0;
    Grt.DictInt[NodeData.Column, 'autoIncrement'] := 1;

    Grt.DictInt[NodeData.Column, 'unsigned'] := 1;
    Grt.ListAdd(Grt.DictItem[NodeData.Column, 'flags'], Grt.ValueFromString('UNSIGNED'));
  end
  else
    Grt.DictInt[NodeData.Column, 'isNullable'] := 1;

  UpdateColumnNodeData(NodeData);

  // Create a primary key on that new column if it is the first one.
  if Grt.ListCount(Columns) = 0 then
    SetColumnPK(NodeData, True);

  Grt.ListAdd(Grt.DictItem[ObjectCopy, 'columns'], NodeData.Column);

  // Add a new dummy node to the tree which might later serve as the start for a new real column node.
  ColumnVST.RootNodeCount := ColumnVST.RootNodeCount + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.SetModified(Modified: Boolean);

begin
  inherited;

  if (FEditorTabFrame <> nil) then
    FEditorTabFrame.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);

var
  I: Integer;
  StartIndexOriginal,
  StartIndexColumnList: Integer;
  NodeData: PColumnNodeData;
  Selection: TNodeArray;
  ColumnList: Pointer;

begin
  Selection := nil;

  // re-ordering of columns
  if (Source = ColumnVST) and (Sender.DropTargetNode <> nil) then
  begin
    // Don't let the tree manipulate any node. We are going to rebuild it completely.
    Effect := DROPEFFECT_NONE;
    Selection := ColumnVST.GetSortedSelection(False);
    if (Mode = dmAbove) then
      StartIndexOriginal := Sender.DropTargetNode.Index - 1
    else
      StartIndexOriginal := Sender.DropTargetNode.Index;
    StartIndexColumnList := StartIndexOriginal;

    ColumnList := Grt.DictItem[ObjectCopy, 'columns'];

    for I := 0 to Length(Selection) - 1 do
    begin
      NodeData := Sender.GetNodeData(Selection[I]);

      // if a column from above the StartIndex is moved
      if (Integer(Selection[I].Index) < StartIndexOriginal) then
      begin
        //add the column after the StartIndexColumnList
        Grt.ListInsert(ColumnList, StartIndexColumnList + 1 + I,
          NodeData.Column);
        //remove column from original position
        Grt.ListDel(ColumnList, Selection[I].Index);

        //move StartIndexColumnList one up since a column
        //from above was moved below the column
        Dec(StartIndexColumnList);
      end
      else
        // if a column from below the StartIndex is moved
        if (Integer(Selection[I].Index) > StartIndexOriginal) then
        begin
          //add the column after the StartIndexColumnList
          Grt.ListInsert(ColumnList, StartIndexColumnList + 1 + I,
            NodeData.Column);
          //remove column from original position
          Grt.ListDel(ColumnList, Selection[I].Index + 1);
        end;
    end;

    // rebuild columns
    ColumnVST.Clear;
    ColumnVST.RootNodeCount := Grt.ListCount(ColumnList) + 1;

    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

begin
  if (Source = ColumnVST) then
    Accept := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: PColumnNodeData;

begin
  if Assigned(Node) then
  begin
    NodeData := ColumnVST.GetNodeData(Node);

    SetCurrentColumn(NodeData.Column);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.SetCurrentColumn(Column: Pointer);

var
  I, J: integer;
  SimpleDatatype, Flags, ColumnFlags: Pointer;
  Flag: WideString;
  FlagOffset: Integer;

begin
  FInitializingColumnContent := True;
  try
    FCurrentColumn := Column;

    if (FCurrentColumn <> nil) then
    begin
      // set name
      ColumnNameEd.Text := Grt.DictString[Column, 'name'];

      // set datatype as string
      ColumnDatatypeEd.Text := GetColumnDatatypeAsString(FCurrentColumn);

      // default value
      ColumnDefaultValueEd.Text := Grt.DictString[Column, 'defaultValue'];

      // character set & collation
      //ColumnCharsetCBox.ItemIndex :=

      // build and check flags
      SimpleDatatype := Grt.DictRef[Column, 'simpleType'];
      ColumnFlags := Grt.DictItem[Column, 'flags'];

      ColumnFlagsCheckListBox.Items.Clear;
      if(SimpleDatatype <> nil)then
      begin
        FlagOffset := 0;

        ColumnFlagsCheckListBox.Items.Add(_('NOT NULL'));
        ColumnFlagsCheckListBox.Checked[0] :=
          (Grt.DictInt[Column, 'isNullable'] = 0);
        inc(FlagOffset);

        ColumnFlagsCheckListBox.Items.Add(_('AUTO INCREMENT'));
        ColumnFlagsCheckListBox.Checked[1] :=
          (Grt.DictInt[Column, 'autoIncrement'] = 1);
        inc(FlagOffset);

        Flags := Grt.DictItem[SimpleDatatype, 'flags'];

        for I := 0 to Grt.ListCount(Flags) - 1 do
        begin
          Flag := Grt.ListString[Flags, I];
          ColumnFlagsCheckListBox.Items.Add(Flag);

          for J := 0 to Grt.ListCount(ColumnFlags) - 1 do
            if SameText(Flag, Grt.ListString[ColumnFlags, J]) then
            begin
              ColumnFlagsCheckListBox.Checked[I + FlagOffset] := True;
              break;
            end;
        end;
      end;

      // Comment
      ColumnCommentMemo.Text := Grt.DictString[Column, 'comment'];
    end
    else
    begin
      ColumnNameEd.Text:='';
      ColumnDatatypeEd.Text:='';
      ColumnDefaultValueEd.Text:='';

      ColumnCharsetCBox.ItemIndex := -1;

      ColumnFlagsCheckListBox.Items.Clear;

      ColumnCommentMemo.Text:='';
    end;
  finally
    FInitializingColumnContent := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnDetailChange(Sender: TObject);

var
  I: Integer;
  ColumnFlags: Pointer;
  NodeData: PColumnNodeData;

begin
  if not FInitializingColumnContent and not FInitializingContent then
  begin
    // Focus the last entry in the tree automatically if none is focused so far.
    // There is always at least one node in the tree.
    if ColumnVST.FocusedNode = nil then
      ColumnVST.FocusedNode := ColumnVST.GetLast;

    // If FCurrentColumn is nil at this point then the dummy column node is focused currently
    // and we can create a new column.
    if FCurrentColumn = nil then
    begin
      NodeData := ColumnVST.GetNodeData(ColumnVST.FocusedNode);
      if ColumnVST.RootNodeCount = 1 then
        AddNewColumn(NodeData, 'INT')
      else
        AddNewColumn(NodeData, 'VARCHAR');
      SetCurrentColumn(NodeData.Column);
    end;

    if Assigned(FCurrentColumn) then
    begin
      Modified := True;

      // column name
      Grt.DictString[FCurrentColumn, 'name'] := ColumnNameEd.Text;

      // datatype
      Grt.ExecuteStandardTask(_('Setting column data type'), 'DbUtils', 'setColumnDatatypeByString',
        [FSimpleDatatypeRefList, FCurrentColumn, WideUpperCase(ColumnDatatypeEd.Text)], True);

      // default value
      Grt.DictString[FCurrentColumn, 'defaultValue'] :=
        ColumnDefaultValueEd.Text;
      if (ColumnDefaultValueEd.Text <> '') then
        Grt.DictInt[FCurrentColumn, 'defaultValueIsNull'] := 0;

      // character set & collation
      //ColumnCharsetCBox.ItemIndex :=

      // build and check flags
      ColumnFlags := Grt.DictItem[FCurrentColumn, 'flags'];
      Grt.ListClear(ColumnFlags);

      Grt.DictInt[FCurrentColumn, 'isNullable'] := 1;
      Grt.DictInt[FCurrentColumn, 'autoIncrement'] := 0;

      for I := 0 to ColumnFlagsCheckListBox.Items.Count - 1 do
      begin
        if ColumnFlagsCheckListBox.Checked[I] then
        begin
          if (ColumnFlagsCheckListBox.Items[I] = _('NOT NULL')) then
            Grt.DictInt[FCurrentColumn, 'isNullable'] := 0
          else
            if (ColumnFlagsCheckListBox.Items[I] = _('AUTO INCREMENT')) then
            begin
              if ColumnCanBeAutoIncrement(FCurrentColumn) then
                Grt.DictInt[FCurrentColumn, 'autoIncrement'] := 1
              else
                ColumnFlagsCheckListBox.Checked[I] := False;
            end
            else
              Grt.ListAddString(ColumnFlags, ColumnFlagsCheckListBox.Items[I]);
        end;
      end;

      // comment
      Grt.DictString[FCurrentColumn, 'comment'] := ColumnCommentMemo.Text;

      // Update grid
      NodeData := ColumnVST.GetNodeData(ColumnVST.FocusedNode);
      UpdateColumnNodeData(NodeData);
      ColumnVST.InvalidateNode(ColumnVST.FocusedNode);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.SetDefValNullBtnClick(Sender: TObject);

var
  NodeData: PColumnNodeData;

begin
  if (not(FInitializingColumnContent)) and
    (not(FInitializingContent))and
    (FCurrentColumn <> nil) and
    (ColumnVST.FocusedNode <> nil) then
  begin
    if (Grt.DictInt[FCurrentColumn, 'defaultValueIsNull'] = 0) then
    begin
      Grt.DictInt[FCurrentColumn, 'defaultValueIsNull'] := 1;
      Grt.DictString[FCurrentColumn, 'defaultValue'] := '';
      ColumnDefaultValueEd.Text := '';

      Modified := True;

      // Update grid
      NodeData := ColumnVST.GetNodeData(ColumnVST.FocusedNode);
      UpdateColumnNodeData(NodeData);
      ColumnVST.InvalidateNode(ColumnVST.FocusedNode);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PIndexNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Node.Index < TVirtualStringTree(Sender).RootNodeCount - 1) then
  begin
    NodeData.Index := Grt.ListItem[
      Grt.DictItem[ObjectCopy, 'indices'], Node.Index];

    NodeData.Name := Grt.DictString[NodeData.Index, 'name'];
    NodeData.IndexType := Grt.DictString[NodeData.Index, 'indexType'];
    NodeData.Comment := Grt.DictString[NodeData.Index, 'comment'];
  end
  else
    NodeData.Index := nil;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PIndexNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: PIndexNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.Index <> nil) then
  begin
    case Column of
      0:
        CellText := NodeData.Name;
      1:
        CellText := NodeData.IndexType;
      2:
        CellText := NodeData.Comment;
    end;
  end
  else
    CellText := '';
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AddIndex(IndexName: WideString);

var
  Indices: Pointer;
  StructName: WideString;
  NodeData: PIndexNodeData;

begin
  NodeData := IndexVST.GetNodeData(IndexVST.GetLast);

  Indices := Grt.DictItem[ObjectCopy, 'indices'];

  StructName := Grt.GetStructMemberContentStructName(
    ObjectStructName, 'indices');

  if (IndexName = '') then
    IndexName := 'index_' + IntToStr(Grt.ListCount(Indices) + 1);

  NodeData.Index := Grt.ObjectNew(StructName, IndexName, '',
    Grt.DictString[ObjectCopy, '_id']);
  NodeData.Name := IndexName;
  NodeData.IndexType := 'INDEX';

  Grt.ListAdd(Indices, NodeData.Index, False);

  IndexVST.InvalidateNode(IndexVST.GetLast);

  // add new empty line
  IndexVST.RootNodeCount := IndexVST.RootNodeCount + 1;

  IndexVST.FocusedNode := IndexVST.GetLast;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: PIndexNodeData;

begin
  if (Node <> nil) then
  begin
    NodeData := Sender.GetNodeData(Node);

    FCurrentIndex := NodeData.Index;

    // build
    IndexColumnVST.NodeDataSize := SizeOf(TIndexColumnNodeData);

    IndexColumnVST.Clear;
    if (FCurrentIndex <> nil) then
      IndexColumnVST.RootNodeCount :=
        Grt.ListCount(Grt.DictItem[FCurrentIndex, 'columns']) + 1;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);

var
  NodeData: PIndexNodeData;
  JumpNextGrid: Boolean;
  NextNode: PVirtualNode;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NewText = '') and (Column < 2) then
    Exit;

  if (NodeData.Index = nil) then
  begin
    if (Column = 0) then
    begin
      AddIndex(NewText);

      Exit;
    end
    else
      AddIndex;
  end;

  Modified := True;

  case Column of
    0:
    begin
      Grt.DictString[NodeData.Index, 'name'] := NewText;

      NodeData.Name := NewText;
    end;

    1:
    begin
      Grt.DictString[NodeData.Index, 'indexType'] := NewText;
      NodeData.IndexType := NewText;

      if (CompareText(NewText, 'UNIQUE') = 0) then
        Grt.DictInt[NodeData.Index, 'unique'] := 1
      else
        Grt.DictInt[NodeData.Index, 'unique'] := 0;
    end;

    2:
    begin
      Grt.DictString[NodeData.Index, 'comment'] := NewText;
      NodeData.Comment := NewText;

      JumpNextGrid:=False;
      NextNode := Sender.GetNextVisible(Node);
      if (NextNode <> nil) then
      begin
        NodeData := Sender.GetNodeData(NextNode);
        if (NodeData.Index = nil)then
          JumpNextGrid:=True;
      end
      else
        JumpNextGrid:=True;

      if (JumpNextGrid) then
      begin
        IndexColumnVST.SetFocus;
        IndexColumnVST.Selected[IndexColumnVST.GetFirst] := True;

        PostMessage(Handle, WM_DoCellEdit, IndexColumnVST.Handle, 0);
      end;
    end;
  end;

  Sender.InvalidateNode(Node);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  EditLink := TIndexGridEditLink.Create(FIndexTypeList, '');
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, IndexVST.Handle, 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PIndexColumnNodeData;
  IndexColumns: Pointer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (FCurrentIndex <> nil) and
    (Node.Index < IndexColumnVST.RootNodeCount - 1) then
  begin
    IndexColumns := Grt.DictItem[FCurrentIndex, 'columns'];

    NodeData.IndexColumn :=
      Grt.ListItem[IndexColumns, Node.Index];

    NodeData.ReferedColumn :=
      Grt.DictRef[NodeData.IndexColumn, 'referedColumn'];

    if (NodeData.ReferedColumn <> nil) then
      NodeData.Name :=
        Grt.DictString[NodeData.ReferedColumn, 'name']
    else
      NodeData.Name :=
        Grt.DictString[NodeData.IndexColumn, 'name'];

    NodeData.ColumnLength :=
      Grt.DictInt[NodeData.IndexColumn, 'columnLength'];
    NodeData.Descend :=
      (Grt.DictInt[NodeData.IndexColumn, 'descend'] = 1);
    NodeData.StoredFunction :=
      Grt.DictString[NodeData.IndexColumn, 'storedFunction'];
    NodeData.Comment :=
      Grt.DictString[NodeData.IndexColumn, 'comment'];
  end
  else
    NodeData.IndexColumn := nil;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PIndexColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PIndexColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.IndexColumn <> nil) then
  begin
    case Column of
      0:
        CellText := NodeData.Name;
      1:
        if (NodeData.Descend) then
          CellText := _('Descendant')
        else
          CellText := _('Ascendant');
      2:
        if (NodeData.ColumnLength > 0) then
          CellText := IntToStr(NodeData.ColumnLength)
        else
          CellText := '';
      3:
        CellText := NodeData.StoredFunction;
      4:
        CellText := NodeData.Comment;
    end;
  end
  else
    CellText := '';
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  EditLink := TIndexColumnGridEditLink.Create(
    Grt.DictItem[ObjectCopy, 'columns'], '');
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PIndexColumnNodeData;
  StructName: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NewText = '') and ((NodeData.IndexColumn = nil) or (Column <> 2)) then
    Exit;

  Modified := True;

  if (NodeData.IndexColumn = nil) then
  begin
    StructName := Grt.GetStructMemberContentStructName(
      Grt.DictStructName[FCurrentIndex], 'columns');

    NodeData.IndexColumn := Grt.ObjectNew(StructName,
      'IndexCol', '', Grt.DictString[FCurrentIndex, '_id']);

    Grt.ListAdd(Grt.DictItem[FCurrentIndex, 'columns'],
      NodeData.IndexColumn);

    IndexColumnVST.RootNodeCount := IndexColumnVST.RootNodeCount + 1;
  end;

  case Column of
    0:
      begin
        NodeData.Name := NewText;
        Grt.DictString[NodeData.IndexColumn, 'name'] :=
          NodeData.Name;

        NodeData.ReferedColumn := Grt.ListItemByObjectName[
          Grt.DictItem[ObjectCopy, 'columns'], NodeData.Name];
        Grt.DictRef[NodeData.IndexColumn, 'referedColumn'] :=
          NodeData.ReferedColumn;
      end;
    1:
      begin
        NodeData.Descend := (NewText = _('Descendant'));

        Grt.DictInt[NodeData.IndexColumn, 'descend'] :=
          Ord(NodeData.Descend);
      end;
    2:
      begin
        NodeData.ColumnLength := StrToIntDef(NewText, 0);
        Grt.DictInt[NodeData.IndexColumn, 'columnLength'] :=
          NodeData.ColumnLength;
      end;
    3:
      begin
        NodeData.StoredFunction := NewText;
        Grt.DictString[NodeData.IndexColumn, 'storedFunction'] :=
          NodeData.StoredFunction;
      end;
    4:
      begin
        NodeData.Comment := NewText;
        Grt.DictString[NodeData.IndexColumn, 'comment'] :=
          NodeData.Comment;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexColumnVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, IndexColumnVST.Handle, 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, ColumnVST.Handle, 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AdvancedBtnClick(Sender: TObject);

begin

end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.NextTabsheetActionExecute(Sender: TObject);

begin
  if (TablePageControl.ActivePageIndex <
    TablePageControl.PageCount - 1) then
    TablePageControl.ActivePageIndex := TablePageControl.ActivePageIndex + 1
  else
    TablePageControl.ActivePageIndex := 0;

  TablePageControlChange(self);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.PreviousTabsheetActionExecute(
  Sender: TObject);

begin
  if (TablePageControl.ActivePageIndex > 0) then
    TablePageControl.ActivePageIndex := TablePageControl.ActivePageIndex - 1
  else
    TablePageControl.ActivePageIndex := TablePageControl.PageCount - 1;

  TablePageControlChange(self);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.TablePageControlChange(Sender: TObject);

begin
  if (TablePageControl.ActivePage = ColumnSheet) then
    ColumnVST.SetFocus
  else
    if (TablePageControl.ActivePage = IndicesSheet) then
    begin
      IndexVST.SetFocus;

      if (IndexVST.FocusedNode = nil) then
      begin
        IndexVST.FocusedNode := IndexVST.GetFirst;
        IndexVST.Selected[IndexVST.FocusedNode] := True;
      end;
    end
    else
      if (TablePageControl.ActivePage = ForeignKeySheet) then
      begin
        ForeignKeyVST.SetFocus;

        if (ForeignKeyVST.FocusedNode = nil) then
        begin
          ForeignKeyVST.FocusedNode := ForeignKeyVST.GetFirst;
          ForeignKeyVST.Selected[ForeignKeyVST.FocusedNode] := True;
        end;
      end;

end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.DeleteSelectedColumnsActionExecute(
  Sender: TObject);

var
  Selection: TNodeArray;
  Tree: TVirtualStringTree;
  I: Integer;
  List: Pointer;

begin
  Selection := nil;
  if (ActiveControl is TVirtualStringTree) then
  begin
    Tree := TVirtualStringTree(ActiveControl);
    Selection := Tree.GetSortedSelection(True);

    if (Tree = ColumnVST) then
      List := Grt.DictItem[ObjectCopy, 'columns']
    else if (Tree = IndexVST) then
      List := Grt.DictItem[ObjectCopy, 'indices']
    else if (Tree =  IndexColumnVST) and (FCurrentIndex <> nil) then
      List := Grt.DictItem[FCurrentIndex, 'columns']
    else if (Tree = ForeignKeyVST) then
      List := Grt.DictItem[ObjectCopy, 'foreignKeys']
    else if (Tree =  ForeignKeyColumnVST) and (FCurrentForeignKey <> nil) then
      List := Grt.DictItem[FCurrentForeignKey, 'columns']
    else
      List := nil;

    if Assigned(List) then
      for I := Length(Selection) - 1 downto 0 do
        Grt.ListDel(List, Selection[I].Index);

    Tree.Clear;
    Tree.RootNodeCount := Grt.ListCount(List) + 1;
    Tree.FocusedNode := Tree.GetFirst;

    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTFocusChanged(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

var
  NodeData: PFkNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  FCurrentForeignKey := NodeData.ForeignKey;

  // build
  ForeignKeyColumnVST.NodeDataSize := SizeOf(TFkColumnNodeData);

  ForeignKeyColumnVST.Clear;
  if (FCurrentForeignKey <> nil) then
    ForeignKeyColumnVST.RootNodeCount :=
      Grt.ListCount(Grt.DictItem[FCurrentForeignKey, 'columns']) + 1;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PFkNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PFkNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.ForeignKey <> nil) then
    case Column of
      0:
        CellText := NodeData.Name;
      1:
        if (NodeData.ReferedTableSchemaName <> '') then
          CellText := NodeData.ReferedTableSchemaName + '.' + NodeData.ReferedTableName
        else
          CellText := NodeData.ReferedTableName;
      2:
        CellText := NodeData.OnUpdate;
      3:
        CellText := NodeData.OnDelete;
      5:
        CellText := NodeData.Comment;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PFkNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Node.Index < TVirtualStringTree(Sender).RootNodeCount - 1) then
  begin
    NodeData.ForeignKey := Grt.ListItem[
      Grt.DictItem[ObjectCopy, 'foreignKeys'], Node.Index];

    NodeData.Name := Grt.DictString[NodeData.ForeignKey, 'name'];

    NodeData.ReferedTable :=
      Grt.DictItem[NodeData.ForeignKey, 'referedTable'];
    NodeData.ReferedTableName :=
      Grt.DictString[NodeData.ForeignKey, 'referedTableName'];
    NodeData.ReferedTableSchemaName :=
      Grt.DictString[NodeData.ForeignKey, 'referedTableSchemaName'];

    NodeData.OnUpdate :=
      Grt.DictString[NodeData.ForeignKey, 'updateRule'];
    NodeData.OnDelete :=
      Grt.DictString[NodeData.ForeignKey, 'deleteRule'];

    NodeData.Comment :=
      Grt.DictString[NodeData.ForeignKey, 'comment'];
  end
  else
    NodeData.ForeignKey := nil;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PFkNodeData;
  JumpNextGrid: Boolean;
  NextNode: PVirtualNode;
  I: Integer;
  SchemaList, Schema, TableList: Pointer;
  SchemaCount: Integer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NewText = '') and (Column < 2) then
    Exit;

  if (NodeData.ForeignKey = nil) then
  begin
    if (Column = 0) then
    begin
      AddFk(NewText);

      Exit;
    end
    else
      AddFk;
  end;

  Modified := True;

  case Column of
    0:
    begin
      Grt.DictString[NodeData.ForeignKey, 'name'] := NewText;

      NodeData.Name := NewText;
    end;

    1:
    begin
      if (Pos('.', NewText) > -1) then
      begin
        NodeData.ReferedTableSchemaName :=
          Copy(NewText, 1, Pos('.', NewText) - 1);
        NodeData.ReferedTableName :=
          Copy(NewText, Pos('.', NewText) + 1, MaxInt);
      end
      else
      begin
        NodeData.ReferedTableSchemaName :=
          Grt.DictString[Grt.DictRef[ObjectCopy, 'owner'], 'name'];
        NodeData.ReferedTableName :=
          Copy(NewText, Pos('.', NewText) + 1, MaxInt);
      end;

      Grt.DictString[NodeData.ForeignKey, 'referedTableSchemaName'] :=
        NodeData.ReferedTableSchemaName;
      Grt.DictString[NodeData.ForeignKey, 'referedTableName'] :=
        NodeData.ReferedTableName;

      // set real refered table
      SchemaList := Grt.DictItem[FCatalog, 'schemata'];
      SchemaCount := Grt.ListCount(SchemaList);
      TableList := nil;
      for I := 0 to SchemaCount - 1 do
      begin
        Schema := Grt.ListItem[SchemaList, I];

        if (Grt.DictString[Schema, 'name'] =
          NodeData.ReferedTableSchemaName) then
        begin
          TableList := Grt.DictItem[Schema, 'tables'];
          break;
        end;
      end;

      if (TableList <> nil) then
        NodeData.ReferedTable := Grt.ListItemByObjectName[
          TableList, NodeData.ReferedTableName];

      Grt.DictRef[NodeData.ForeignKey, 'referedTable'] :=
        NodeData.ReferedTable;

      // jump to column grid
      JumpNextGrid:=False;
      NextNode := Sender.GetNextVisible(Node);
      if (NextNode <> nil) then
      begin
        NodeData := Sender.GetNodeData(NextNode);
        if (NodeData.ForeignKey = nil)then
          JumpNextGrid:=True;
      end
      else
        JumpNextGrid:=True;

      if (JumpNextGrid) then
      begin
        ForeignKeyColumnVST.SetFocus;
        ForeignKeyColumnVST.Selected[ForeignKeyColumnVST.GetFirst] := True;

        PostMessage(Handle, WM_DoCellEdit, ForeignKeyColumnVST.Handle, 0);
      end;

    end;

    2:
    begin
      Grt.DictString[NodeData.ForeignKey, 'updateRule'] := NewText;
      NodeData.OnUpdate := NewText;
    end;

    3:
    begin
      Grt.DictString[NodeData.ForeignKey, 'deleteRule'] := NewText;
      NodeData.OnDelete := NewText;
    end;

    4:
    begin
      Grt.DictString[NodeData.ForeignKey, 'comment'] := NewText;
      NodeData.Comment := NewText;
    end;
  end;

  Sender.InvalidateNode(Node);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AddFk(FkName: WideString = '');

var
  ForeignKeys: Pointer;
  StructName: WideString;
  NodeData: PFkNodeData;

begin
  NodeData := ForeignKeyVST.GetNodeData(ForeignKeyVST.GetLast);

  ForeignKeys := Grt.DictItem[ObjectCopy, 'foreignKeys'];

  StructName := Grt.GetStructMemberContentStructName(
    ObjectStructName, 'foreignKeys');

  if (FkName = '') then
    FkName := 'fk_' + Grt.DictString[ObjectCopy, 'name']
      + '_' + IntToStr(Grt.ListCount(ForeignKeys) + 1);

  NodeData.ForeignKey := Grt.ObjectNew(StructName, FkName, '',
    Grt.DictString[ObjectCopy, '_id']);
  NodeData.Name := FkName;
  NodeData.OnUpdate := 'NO ACTION';
  NodeData.OnDelete := 'NO ACTION';

  Grt.ListAdd(ForeignKeys, NodeData.ForeignKey, False);

  ForeignKeyVST.InvalidateNode(ForeignKeyVST.GetLast);

  // add new empty line
  ForeignKeyVST.RootNodeCount := ForeignKeyVST.RootNodeCount + 1;

  ForeignKeyVST.FocusedNode := ForeignKeyVST.GetLast;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

begin
  if (Column = 1) then
    RefreshTableCache;

  EditLink := TForeignKeyGridEditLink.Create(
    FTableList, '');
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.RefreshTableCache;

var
  I, J: Integer;
  TableList, SchemaList, Schema: Pointer;
  TableCount, SchemaCount: Integer;

begin
  if (FTableList = nil) then
    FTableList := TList.Create;

  FTableList.Clear;

  TableList := Grt.DictItem[FSchema, 'tables'];
  TableCount := Grt.ListCount(TableList);
  for I := 0 to TableCount - 1 do
    FTableList.Add(Grt.ListItem[TableList, I]);

  SchemaList := Grt.DictItem[FCatalog, 'schemata'];
  SchemaCount := Grt.ListCount(SchemaList);
  for I := 0 to SchemaCount - 1 do
  begin
    Schema := Grt.ListItem[SchemaList, I];
    if (Schema <> FSchema) then
    begin
      TableList := Grt.DictItem[Schema, 'tables'];
      TableCount := Grt.ListCount(TableList);

      for J := 0 to TableCount - 1 do
        FTableList.Add(Grt.ListItem[TableList, J]);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, ForeignKeyVST.Handle, 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PFkColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PFkColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.Column <> nil) then
  begin
    case Column of
      0:
        CellText := NodeData.Name;
      1:
        CellText := NodeData.ReferedColumnName;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PFkColumnNodeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Node.Index < TVirtualStringTree(Sender).RootNodeCount - 1) then
  begin
    NodeData.Column := Grt.ListRefItem[
      Grt.DictItem[FCurrentForeignKey, 'columns'], Node.Index];

    NodeData.Name := Grt.DictString[NodeData.Column, 'name'];

    NodeData.ReferedColumn := Grt.ListRefItem[
      Grt.DictItem[FCurrentForeignKey, 'referedColumns'], Node.Index];

    if (NodeData.ReferedColumn <> nil) then
      NodeData.ReferedColumnName :=
        Grt.DictString[NodeData.ReferedColumn, 'name']
    else
      NodeData.ReferedColumnName := Grt.ListString[
        Grt.DictItem[FCurrentForeignKey, 'referedColumnNames'], Node.Index];
  end
  else
    NodeData.Column := nil;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PFkColumnNodeData;
  ReferedTable, ColumnsList: Pointer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NewText = '') or
    (FCurrentForeignKey = nil) then
    Exit;

  if (NodeData.Column = nil) then
  begin
    if (Column = 0) then
    begin
      AddFkColumn(NewText);

      Exit;
    end
    else
      AddFkColumn;
  end;

  Modified := True;

  case Column of
    0:
    begin
      ColumnsList := Grt.DictItem[ObjectCopy, 'columns'];

      NodeData.Column :=
        Grt.ListItemByObjectName[ColumnsList, NewText];
      if (NodeData.Column <> nil) then
      begin
        ColumnsList := Grt.DictItem[FCurrentForeignKey, 'columns'];
        Grt.ListItem[ColumnsList, Node.Index] :=
          Grt.DictItem[NodeData.Column, '_id'];
      end;
    end;

    1:
    begin
      NodeData.ReferedColumnName := NewText;

      ReferedTable := Grt.DictRef[FCurrentForeignKey, 'referedTable'];
      if (ReferedTable <> nil) then
      begin
        ColumnsList := Grt.DictItem[ReferedTable, 'columns'];

        NodeData.ReferedColumn :=
          Grt.ListItemByObjectName[ColumnsList, NewText];
        if (NodeData.ReferedColumn <> nil) then
        begin
          ColumnsList := Grt.DictItem[FCurrentForeignKey, 'referedColumns'];

          if (Integer(Node.Index) > Grt.ListCount(ColumnsList) - 1) then
            Grt.ListAdd(ColumnsList,
              Grt.DictItem[NodeData.ReferedColumn, '_id'])
          else
            Grt.ListItem[ColumnsList, Node.Index] :=
              Grt.DictItem[NodeData.ReferedColumn, '_id'];
        end;
      end;
    end;
  end;

  Sender.InvalidateNode(Node);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AddFkColumn(Column: WideString = '');

var
  ColumnsList: Pointer;
  NodeData: PFkColumnNodeData;

begin
  NodeData := ForeignKeyColumnVST.GetNodeData(ForeignKeyColumnVST.GetLast);


  if (Column <> '') then
  begin
    ColumnsList := Grt.DictItem[ObjectCopy, 'columns'];

    NodeData.Column :=
      Grt.ListItemByObjectName[ColumnsList, Column];
  end;

  if (NodeData.Column = nil) then
  begin
    ColumnsList := Grt.DictItem[ObjectCopy, 'columns'];

    if (Grt.ListCount(ColumnsList) > 0) then
      NodeData.Column := Grt.ListItem[ColumnsList, 0];
  end;

  if (NodeData.Column = nil) then
    raise Exception.Create(_('Please columns to the table '+
      'before adding the foreign keys.'));

  NodeData.Name := Grt.DictString[NodeData.Column, 'name'];

  // add list items
  ColumnsList := Grt.DictItem[FCurrentForeignKey, 'referedColumnNames'];
  Grt.ListAdd(ColumnsList, Grt.ValueFromString(''));

  ColumnsList := Grt.DictItem[FCurrentForeignKey, 'referedColumns'];
  Grt.ListAdd(ColumnsList, nil);

  ColumnsList := Grt.DictItem[FCurrentForeignKey, 'columns'];
  Grt.ListAdd(ColumnsList, Grt.DictItem[NodeData.Column, '_id']);

  ForeignKeyColumnVST.InvalidateNode(ForeignKeyColumnVST.GetLast);

  // add new empty line
  ForeignKeyColumnVST.RootNodeCount :=
    ForeignKeyColumnVST.RootNodeCount + 1;

  ForeignKeyColumnVST.FocusedNode := ForeignKeyColumnVST.GetLast;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTDblClick(Sender: TObject);

begin
  PostMessage(Handle, WM_DoCellEdit, ForeignKeyColumnVST.Handle, 0);
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ForeignKeyColumnVSTCreateEditor(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  out EditLink: IVTEditLink);

var
  ColumnList: Pointer;
  ReferedTable: Pointer;

begin
  ColumnList := nil;

  if (Column = 0) then
    ColumnList := Grt.DictItem[ObjectCopy, 'columns']
  else
  begin
    if (FCurrentForeignKey <> nil) then
    begin
      ReferedTable := Grt.DictRef[FCurrentForeignKey, 'referedTable'];

      if (ReferedTable <> nil) then
        ColumnList := Grt.DictItem[ReferedTable, 'columns'];
    end
  end;

  EditLink := TForeignKeyColumnGridEditLink.Create(
    ColumnList, '');
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.CollationComboBoxCloseUp(Sender: TObject);

begin
  if (Copy(CollationComboBox.Text, 1, 4) = '____') then
    CollationComboBox.ItemIndex := 0;


  SetCharsetCollation(CollationComboBox.Text, ObjectCopy,
    'defaultCharacterSetName', 'defaultCollationName');

  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.PackKeysComboBoxCloseUp(Sender: TObject);

var
  Index: Integer;

begin
  Index := TTntComboBox(Sender).ItemIndex;
  if(Index >= 0)then
  begin
    Grt.DictString[ObjectCopy, 'packKeys'] := FPackKeysMapping[Index];
    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.TablePasswordChange(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'password'] := TablePasswordEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.NextAutoIncChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'nextAutoInc'] := AutoIncEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.OnDelayKeyUpdatesChange(Sender: TObject);

begin
  if(DelayKeyUpdatesCBox.Checked)then
    Grt.DictInt[ObjectCopy, 'delayKeyWrite'] := 1
  else
    Grt.DictInt[ObjectCopy, 'delayKeyWrite'] := 0;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.RowFormatComboBoxCloseUp(Sender: TObject);

var
  Index: Integer;

begin
  Index := TTntComboBox(Sender).ItemIndex;
  if(Index >= 0)then
  begin
    Grt.DictString[ObjectCopy, 'rowFormat'] := FRowFormatMapping[Index];
    Modified := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.UseChecksumCheckBoxClicked(Sender: TObject);

begin
  if(UseChecksumCBox.Checked)then
    Grt.DictInt[ObjectCopy, 'checksum'] := 1
  else
    Grt.DictInt[ObjectCopy, 'checksum'] := 0;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.AvgRowLengthChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'avgRowLength'] := AvgRowLengthEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.MinRowChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'minRows'] := MinRowsEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.MaxRowsChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'maxRows'] := MaxRowsEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.DataDirChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'tableDataDir'] := DataDirectoryEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.IndexDirChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'tableIndexDir'] := IndexDirectoryEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.UnionTablesChanged(Sender: TObject);

begin
  Grt.DictString[ObjectCopy, 'mergeUnion'] := UnionTablesEd.Text;
  Modified := True;
end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.MergeInsertMethodComboBoxCloseUp(
  Sender: TObject);

var
  Index: Integer;

begin
  Index := TTntComboBox(Sender).ItemIndex;
  if(Index >= 0)then
  begin
    Grt.DictString[ObjectCopy, 'mergeInsert'] := FMergeInsertMapping[Index];
    Modified := True;
  end;

end;

// -----------------------------------------------------------------------------

procedure TMyxTableEditorForm.ShowAdvancedActionExecute(Sender: TObject);

begin
  if not(ShowAdvancedAction.Checked) then
  begin
    AdvancedBtn.Caption := _('Advanced >>');

    ColumnsDetailsPnl.Visible := False;
    //IndexDetailsPnl.Visible := False;

    Height := Height - 100;
  end
  else
  begin
    AdvancedBtn.Caption := _('<< Advanced');

    ColumnsDetailsPnl.Visible := True;

    Height := Height + 100;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ApplyChangesActionExecute(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ApplyChangeAndCloseActionExecute(Sender: TObject);

begin
  ApplyChanges;
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.CloseActionExecute(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.DockEditor;

begin
  PrepareEditorDock(TablePageControl);

  FEditorTabFrame.ApplyAction := ApplyChangesAction;
  FEditorTabFrame.DiscardAction := DiscardChangesAction;

  IndexColumnSplitter.Color := clBtnFace;
  ForeignKeyColumnSplitter.Color := clBtnFace;
  AdvancedOptionsScrollBox.Color := clBtnFace;

  TableSheetBgShape.Visible := True;
  ColumnsDetailsBgShape.Visible := True;
  IndexDetailsBgShape.Visible := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ApplyChangesActionUpdate(Sender: TObject);

begin
  if (Sender is TTntAction) then
    TTntAction(Sender).Enabled := Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.DiscardChangesActionExecute(Sender: TObject);

begin
  DiscardChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.FocusFirstControl;

begin
  if (FEditorTabFrame <> nil) then
  begin
    if (FEditorTabFrame.SelectedTab = 0) then
      TableNameEd.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.CopyToClipboardActionExecute(Sender: TObject);

begin
  ColumnVST.CopyToClipBoard;
  ColumnVST.FlushClipboard;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.PasteFromClipboardActionExecute(Sender: TObject);

var
  DummyNode: PVirtualNode;
  NodeData: PColumnNodeData;

begin
  DummyNode := nil;

  // Remove the current node if it is only the dummy node.
  // But do this only if there was actually something to paste.
  if Assigned(ColumnVST.FocusedNode) then
  begin
    NodeData := ColumnVST.GetNodeData(ColumnVST.FocusedNode);
    if NodeData.Column = nil then
      DummyNode := ColumnVST.FocusedNode;
  end
  else
    if ColumnVST.ChildCount[nil] = 1 then
    begin
      // No node is focused but perhaps there is only one node in the tree (the dummy node)?
      // This can happen if we just opened the table editor.
      NodeData := ColumnVST.GetNodeData(ColumnVST.GetFirst);
      if NodeData.Column = nil then
        DummyNode := ColumnVST.GetFirst;
    end;
  
  if ColumnVST.PasteFromClipboard and Assigned(DummyNode) then
  begin
    ColumnVST.DeleteNode(DummyNode);

    // Add a new dummy node at the end of the list.
    ColumnVST.RootNodeCount := ColumnVST.RootNodeCount + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.SelectAllActionExecute(Sender: TObject);

begin
  ColumnVST.SelectAll(True);

  // Deselect the last node. It is always a dummy node for new columns.
  ColumnVST.Selected[ColumnVST.GetLast] := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTSaveNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);

// Saves those parts of the node into the given stream that are necessary to reconstruct it later.

var
  NodeData: PColumnNodeData;
  Size: Integer;
  S: WideString;
  Flag: Byte;

begin
  NodeData := Sender.GetNodeData(Node);
  if Assigned(NodeData.Column) then
  begin
    S := Grt.DictString[NodeData.Column, 'name'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    Flag := Grt.DictInt[NodeData.Column, 'autoIncrement'];
    Stream.Write(Flag, SizeOf(Flag));

    S := Grt.DictString[NodeData.Column, 'characterSetName'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    S := Grt.DictString[NodeData.Column, 'collationName'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    S := Grt.DictString[NodeData.Column, 'comment'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    S := Grt.DictString[NodeData.Column, 'datatypeExplicitParams'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    S := Grt.DictString[NodeData.Column, 'datatypeName'];
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);

    Flag := Grt.DictInt[NodeData.Column, 'defaultValueIsNull'];
    Stream.Write(Flag, SizeOf(Flag));

    // Write default value only if cannot be null.
    if Flag = 0 then
    begin
      S := Grt.DictString[NodeData.Column, 'defaultValue'];
      Size := Length(S);
      Stream.Write(Size, SizeOf(Size));
      Stream.Write(PWideChar(S)^, 2 * Size);
    end;

    Flag := Grt.DictInt[NodeData.Column, 'isNullable'];
    Stream.Write(Flag, SizeOf(Flag));

    Size := Grt.DictInt[NodeData.Column, 'length'];
    Stream.Write(Size, SizeOf(Size));

    Size := Grt.DictInt[NodeData.Column, 'precision'];
    Stream.Write(Size, SizeOf(Size));

    Size := Grt.DictInt[NodeData.Column, 'scale'];
    Stream.Write(Size, SizeOf(Size));

    Flag := Grt.DictInt[NodeData.Column, 'unsigned'];
    Stream.Write(Flag, SizeOf(Flag));

    S := NodeData.ColumnFlags.CommaText;
    Size := Length(S);
    Stream.Write(Size, SizeOf(Size));
    Stream.Write(PWideChar(S)^, 2 * Size);
  end
  else
  begin
    // If there is nothing to write then indicate that with a 0 size
    // to allow LoadNode to work correctly.
    Size := 0;
    Stream.Write(Size, SizeOf(Size));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ColumnVSTLoadNode(Sender: TBaseVirtualTree; Node: PVirtualNode; Stream: TStream);

var
  NodeData: PColumnNodeData;
  Size: Integer;
  S: WideString;
  StructName: WideString;
  Flag: Byte;
  Columns: Pointer;
  Flags: TStringList;
  GRTFlags: Pointer;
  I: Integer;

begin
  NodeData := Sender.GetNodeData(Node);
  Modified:=True;

  Assert(NodeData.Column = nil);

  // Check if something was actually written.
  Stream.Read(Size, SizeOf(Size));
  if Size > 0 then
  begin
    // Add new column.
    StructName := Grt.GetStructMemberContentStructName(ObjectStructName, 'columns');
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    NodeData.Column := Grt.ObjectNew(StructName, S, '', Grt.DictString[ObjectCopy, '_id']);

    Stream.Read(Flag, SizeOf(Flag));
    Grt.DictInt[NodeData.Column, 'autoIncrement'] := Flag;

    Stream.Read(Size, SizeOf(Size));
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    Grt.DictString[NodeData.Column, 'characterSetName'] := S;

    Stream.Read(Size, SizeOf(Size));
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    Grt.DictString[NodeData.Column, 'collationName'] := S;

    Stream.Read(Size, SizeOf(Size));
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    Grt.DictString[NodeData.Column, 'comment'] := S;

    Stream.Read(Size, SizeOf(Size));
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    Grt.DictString[NodeData.Column, 'datatypeExplicitParams'] := S;

    Stream.Read(Size, SizeOf(Size));
    SetLength(S, Size);
    Stream.Read(PWideChar(S)^, 2 * Size);
    Grt.DictString[NodeData.Column, 'datatypeName'] := S;

    // Set simple datatype
    if Assigned(FSimpleDatatypeRefList) then
      Grt.DictString[NodeData.Column, 'simpleType'] :=
        Grt.DictString[Grt.ListRefValueByObjectName[FSimpleDatatypeRefList, S], '_id'];

    Stream.Read(Flag, SizeOf(Flag));
    Grt.DictInt[NodeData.Column, 'defaultValueIsNull'] := Flag;

    // Read default value only if it cannot be null.
    if Flag = 0 then
    begin
      Stream.Read(Size, SizeOf(Size));
      SetLength(S, Size);
      Stream.Read(PWideChar(S)^, 2 * Size);
      Grt.DictString[NodeData.Column, 'defaultValue'] := S;
    end;

    Stream.Read(Flag, SizeOf(Flag));
    Grt.DictInt[NodeData.Column, 'isNullable'] := Flag;

    Stream.Read(Size, SizeOf(Size));
    Grt.DictInt[NodeData.Column, 'length'] := Size;

    Stream.Read(Size, SizeOf(Size));
    Grt.DictInt[NodeData.Column, 'precision'] := Size;

    Stream.Read(Size, SizeOf(Size));
    Grt.DictInt[NodeData.Column, 'scale'] := Size;

    Stream.Read(Flag, SizeOf(Flag));
    Grt.DictInt[NodeData.Column, 'unsigned'] := Flag;

    Flags := TStringList.Create;
    try
      Stream.Read(Size, SizeOf(Size));
      SetLength(S, Size);
      Stream.Read(PWideChar(S)^, 2 * Size);
      Flags.CommaText := S;

      GRTFlags := Grt.DictItem[NodeData.Column, 'flags'];
      for I := 0 to FLags.Count - 1 do
        Grt.ListAdd(GRTFlags, Grt.ValueFromString(Flags[I]));
    finally
      Flags.Free;
    end;

    UpdateColumnNodeData(NodeData);

    Columns := Grt.DictItem[ObjectCopy, 'columns'];
    if (Grt.ListCount(Columns) = 0) then
      SetColumnPK(NodeData, True);

    Grt.ListAdd(Grt.DictItem[ObjectCopy, 'columns'], NodeData.Column);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.DeleteSelectedColumnsActionUpdate(Sender: TObject);

var
  Tree: TVirtualStringTree;
  Enable: Boolean;

begin
  Enable := False;
  if ActiveControl is TVirtualStringTree then
  begin
    Tree := ActiveControl as TVirtualStringTree;
    if Tree.SelectedCount > 0 then
    begin
      // Do not allow clipboard operation on only the dummy node.
      if (Tree.SelectedCount > 1) or not Tree.Selected[Tree.GetLast] then
        Enable := True;
    end;
  end;

  TTntAction(Sender).Enabled := Enable;
end;

//----------------------------------------------------------------------------------------------------------------------

var
  VTOLEFormat: TFormatEtc = (
    // Format must later be set.
    cfFormat: 0;
    // No specific target device to render on.
    ptd: nil;
    // Normal content to render.
    dwAspect: DVASPECT_CONTENT;
    // No specific page of multipage data (we don't use multipage data by default).
    lindex: -1;
    // Acceptable storage formats are IStream and global memory.
    tymed: TYMED_ISTREAM or TYMED_HGLOBAL;
  );

procedure TMyxTableEditorForm.PasteFromClipboardActionUpdate(Sender: TObject);

var
  Enable: Boolean;
  Data: IDataObject;

begin
  // Check if there is data from a VT is on the clipboard.
  Enable := False;
  if ActiveControl = ColumnVST then
  begin
    if OleGetClipboard(Data) = S_OK then
    begin
      VTOLEFormat.cfFormat := CF_VIRTUALTREE;
      Enable := Data.QueryGetData(VTOLEFormat) = S_OK;
    end;
  end;
  TTntAction(Sender).Enabled := Enable;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTableEditorForm.ApplyChanges;

var
  I: Integer;
  ExpandedHeights: Pointer;
  ExpandedStates: Pointer;

begin
  ExpandedStates := Grt.ListNew(GrtIntValue);
  ExpandedHeights := Grt.ListNew(GrtIntValue);

  for I := 0 to FRoutineFrames.Count - 1 do
  begin
    TTriggerFrame(FRoutineFrames[i]).ApplyChanges;

    Grt.ListAdd(ExpandedStates, Grt.ValueFromInt(
      Ord(TRoutineFrame(FRoutineFrames[i]).Expanded)), False);

    Grt.ListAdd(ExpandedHeights, Grt.ValueFromInt(
      TRoutineFrame(FRoutineFrames[i]).ExpandedHeight), False);
  end;

  Grt.DictItem[ObjectCopy, 'routineExpandedStates'] := ExpandedStates;
  Grt.DictItem[ObjectCopy, 'routineExpandedHeights'] := ExpandedHeights;

  inherited;

  // build relationships
  Grt.ExecuteStandardTask(_('Creating relationships from foreign keys'), 'Workbench', 'createRelationshipsFromFKs',
    [Grt.DictRef[Grt.Global['/workbench/model'], 'currentView']], True);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
