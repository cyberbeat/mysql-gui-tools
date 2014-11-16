unit XGrtShell;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, TntComCtrls,
  TntForms, ExtCtrls, TntExtCtrls, ImgList, Menus, TntMenus,
  StdCtrls, TntStdCtrls, TntSysUtils, StrUtils, TntDialogs,
  TntClipbrd, TntClasses,
  UniCodeEditor, UniCodeConsole, VirtualTrees,
  myx_public_interface, myx_grt_public_interface,
  PNGImage, AuxFuncs,
  Grt, GrtObjectTree, MyxBaseForm,
  MyxOptions, MyxAppOptions,
  CommonFuncs, ActnList, StdActns, TntStdActns, TntActnList, MyxTabHeader;

type
  NativeGrtObjInspectorData = ^TGRTObjInspectorData;
  TGRTObjInspectorData = record
    Key: WideString;
    Text: WideString;
    Value: Pointer;
  end;

  GRTStructTreeDataType =
  (
    GrtStPackage, GrtStStruct, GrtStMember
  );

  NativeGrtStructTreeData = ^TGrtStructTreeData;
  TGrtStructTreeData = record
    DataType: GRTStructTreeDataType;
    Struct: PMYX_GRT_STRUCT;
    Member: PMYX_GRT_STRUCT_MEMBER;
    PackageName: WideString;
  end;


  GRTModuleTreeDataType =
  (
    GrtMtModule, GrtMtFunction
  );

  NativeGrtModuleTreeData = ^TGRTModuleTreeData;
  TGRTModuleTreeData = record
    DataType: GRTModuleTreeDataType;
    Module: PMYX_GRT_MODULE;
    ModuleFunction: PMYX_GRT_FUNCTION;
  end;

  GrtStructSortType = (
    GrtStSTName,
    GrtStSTHierachy,
    GrtStSTPackage
  );

  TXGrtShellForm = class(TMyxBaseForm)
    TntStatusBar: TTntStatusBar;
    CommandLinePnl: TTntPanel;
    CommandLineUCE: TUniCodeConsole;
    ObjClassPnl: TTntPanel;
    TntSplitter2: TTntSplitter;
    ObjInspectorPnl: TTntPanel;
    ObjInspectorTree: TVirtualStringTree;
    ObjInspectorLookupPnl: TTntPanel;
    ObjectTreeLookup: TTntComboBox;
    TntMainMenu1: TTntMainMenu;
    FileMI: TTntMenuItem;
    HelpMI: TTntMenuItem;
    ExitMI: TTntMenuItem;
    CommandLineSplitter: TTntSplitter;
    RefreshMI: TTntMenuItem;
    TreeImageList: TImageList;
    ViewMI: TTntMenuItem;
    N1: TTntMenuItem;
    DisplayObjectValuesMI: TTntMenuItem;
    AboutMI: TTntMenuItem;
    SaveGRTEnvironmentMI: TTntMenuItem;
    OpenGRTEnvironmentMI: TTntMenuItem;
    N2: TTntMenuItem;
    N3: TTntMenuItem;
    ConvertGRTStructFileToJavaClassesMI: TTntMenuItem;
    SidebarPnl: TTntPanel;
    SidePageControl: TTntPageControl;
    ValuesTabsheet: TTntTabSheet;
    StructsTabsheet: TTntTabSheet;
    ModulesTabSheet: TTntTabSheet;
    LeftSpacerPnl: TTntPanel;
    RightSpacerPnl: TTntPanel;
    TopSpacerPnl: TTntPanel;
    ModulesTree: TVirtualStringTree;
    ModuleInfoPnl: TTntPanel;
    TntSplitter3: TTntSplitter;
    ShellPopupMenu: TTntPopupMenu;
    FontSizeMI: TTntMenuItem;
    FontSizeMediumMI: TTntMenuItem;
    FontSizeLargeMI: TTntMenuItem;
    FontSizeSmallMI: TTntMenuItem;
    TntLabel1: TTntLabel;
    ModuleNameLbl: TTntLabel;
    TntLabel2: TTntLabel;
    PathLbl: TTntLabel;
    TntLabel3: TTntLabel;
    ExtendsLbl: TTntLabel;
    TypeCaptionLbl: TTntLabel;
    ModuleTypeLbl: TTntLabel;
    StructsTree: TVirtualStringTree;
    TntPanel2: TTntPanel;
    TntShape1: TTntShape;
    TntPanel3: TTntPanel;
    InspectedObjectEd: TTntEdit;
    MainPnl: TTntPanel;
    TntPanel4: TTntPanel;
    MainPageControl: TTntPageControl;
    GtrShellSheet: TTntTabSheet;
    SnippetsSheet: TTntTabSheet;
    TntPanel5: TTntPanel;
    TntPanel6: TTntPanel;
    StructPopupMenu: TTntPopupMenu;
    StructsOrderbyNameMI: TTntMenuItem;
    StructsOrderbyHierachyMI: TTntMenuItem;
    StructsOrderbyPackagesMI: TTntMenuItem;
    ObjInspectorTreePopupMenu: TTntPopupMenu;
    ObjInspectorCopyValueMI: TTntMenuItem;
    CurrentReverenceValuesMI: TTntMenuItem;
    GlobalObjTreePopupMenu: TTntPopupMenu;
    RemoveObjectMI: TTntMenuItem;
    N4: TTntMenuItem;
    RefreshInspectorMI: TTntMenuItem;
    ObjInspectorCopyAllValuesMI: TTntMenuItem;
    OpenScriptFileMI: TTntMenuItem;
    N5: TTntMenuItem;
    TntActionList1: TTntActionList;
    ScriptOpenAction: TTntFileOpen;
    ScriptSaveAction: TTntAction;
    ScriptRunAction: TTntAction;
    TabSheetsPopupMenu: TTntPopupMenu;
    RemoveTabsheetMI: TTntMenuItem;
    RefreshDisplayAction: TTntAction;
    StructsPnl: TTntPanel;
    ModulesPnl: TTntPanel;
    GrtShellTabHeaderFrame: TMyxTabHeaderFrame;
    GrtShellPalettePnl: TTntPanel;
    TntShape2: TTntShape;
    TntShape3: TTntShape;
    TntShape4: TTntShape;
    CodeSnippetPnl: TTntPanel;
    CodeSnippetUCE: TUniCodeEdit;
    DisplayObjectValuesAction: TTntAction;
    N6: TTntMenuItem;
    DisplayObjectValues1: TTntMenuItem;
    DisplayObjectRefcountAction: TTntAction;
    DisplayObjectRefcount1: TTntMenuItem;
    DisplayTypeInfoAction: TTntAction;
    DisplayTypeInfo1: TTntMenuItem;
    DisplayTypeInfo2MI: TTntMenuItem;
    RefreshGlobalsTreeMI: TTntMenuItem;
    N7: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    function CommandLineUCEIsMultilineCommand(Cmd: WideString; Key: Word;
      Shift: TShiftState): Boolean;
    procedure CommandLineUCEExecuteCommand(Cmd: WideString; Key: Word;
      Shift: TShiftState);

    procedure BuildObjTreeFromLookupSelection;
    procedure BuildObjTree(ObjectTreeRoot: WideString);

    procedure RefreshMIClick(Sender: TObject);
    procedure GRTObjTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);

    procedure RefreshGRTObjInspector;
    procedure ObjInspectorTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);

    procedure ExitMIClick(Sender: TObject);
    procedure AboutMIClick(Sender: TObject);
    procedure OpenGRTEnvironmentMIClick(Sender: TObject);
    procedure SaveGRTEnvironmentMIClick(Sender: TObject);
    procedure ConvertGRTStructFileToJavaClassesMIClick(Sender: TObject);

    procedure BuildModulesTree;
    procedure ModulesTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ModulesTreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure ModulesTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure ModulesTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ModulesTreeDblClick(Sender: TObject);
    procedure FontSizeMIClick(Sender: TObject);
    procedure ModulesTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);

    procedure BuildStructsTree;

    procedure StructsTreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure StructsTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure StructsTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure StructsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ObjectTreeLookupCloseUp(Sender: TObject);
    procedure ObjectTreeLookupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StructsTreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure StructsOrderbyNameMIClick(Sender: TObject);
    procedure StructsOrderbyHierachyMIClick(Sender: TObject);
    procedure StructPopupMenuPopup(Sender: TObject);
    procedure StructsOrderbyPackagesMIClick(Sender: TObject);
    procedure StructsTreeGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure ObjInspectorTreeGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    function GetInspectorObjectText(Node: PVirtualNode; Column: TColumnIndex): WideString;
    procedure ModulesTreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure CommandLineUCEDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure CommandLineUCEDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure ObjInspectorCopyValueMIClick(Sender: TObject);
    procedure RemoveObjectMIClick(Sender: TObject);
    procedure ObjInspectorTreeEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ObjInspectorTreeNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure ObjInspectorTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure RefreshInspectorMIClick(Sender: TObject);
    procedure ObjInspectorTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure StructsTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure ObjInspectorCopyAllValuesMIClick(Sender: TObject);
    procedure ScriptOpenActionAccept(Sender: TObject);
    procedure ScriptSaveActionExecute(Sender: TObject);
    procedure ScriptRunActionExecute(Sender: TObject);
    procedure ScriptOpenActionBeforeExecute(Sender: TObject);
    procedure RemoveTabsheetMIClick(Sender: TObject);
    procedure TabSheetsPopupMenuPopup(Sender: TObject);
    procedure RefreshDisplayActionExecute(Sender: TObject);
    procedure DisplayObjectValuesActionExecute(Sender: TObject);
    procedure DisplayObjectRefcountActionExecute(Sender: TObject);
    procedure DisplayTypeInfoActionExecute(Sender: TObject);
  protected
    procedure OutputCommandLine(Text: WideString);
    procedure SetObjInspectorObject(ObjInspectorObject: Pointer);
    procedure SetSortStructsBy(SortStructsBy: GrtStructSortType);
    procedure TaskFinished(Task: IGrtGenericTask);
    function TaskHandleError: Boolean;
  private
    FTreeBtnOpenPNGImg,
    FTreeBtnClosedPNGImg: TPNGObject;

    FGrtValueDictPNGImg,
    FGrtValueListPNGImg,
    FGrtValueSimplePNGImg,
    FGrtValueStructPNGImg,
    FGrtModulePNGImg,
    FGrtFunctionPNGImg: TPNGObject;

    FOnlyObjectStructure: Boolean;

    FObjInspectorObject: Pointer;

    FSortStructsBy: GrtStructSortType;

    FScriptFilenameList: TTntStringList;

    // Task handling.
    FCommandTask: IGrtShellTask;
    FShowExtraInfo: Boolean;
  public
    { Public declarations }
    GrtObjectTree: TGrtObjectTree;

    destructor Destroy; override;

    procedure InitializeShell;
    procedure RefreshDisplay;

    property ObjInspectorObject: Pointer read FObjInspectorObject write SetObjInspectorObject;
    property SortStructsBy: GrtStructSortType read FSortStructsBy write SetSortStructsBy;
  end;

var
  XGrtShellForm: TXGrtShellForm;

implementation

{$R *.dfm}

uses
  PNGTools;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.FormCreate(Sender: TObject);

begin
  Application.HintHidePause := MaxInt;

  MainPageControl.ActivePageIndex := 0;
  SidePageControl.ActivePageIndex := 0;
  ActiveControl := CommandLineUCE;

  // Set shell to async execution
  CommandLineUCE.AsyncExecution := True;

  FTreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  FTreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  FGrtValueDictPNGImg := LoadPNGImageFromResource('grt_value_dict');
  FGrtValueListPNGImg := LoadPNGImageFromResource('grt_value_list');
  FGrtValueSimplePNGImg := LoadPNGImageFromResource('grt_value_simple');
  FGrtValueStructPNGImg := LoadPNGImageFromResource('grt_value_struct');
  FGrtModulePNGImg := LoadPNGImageFromResource('grt_module');
  FGrtFunctionPNGImg := LoadPNGImageFromResource('grt_function');

  FOnlyObjectStructure := True;

  // Dock Palettes
  GrtShellTabHeaderFrame.ShowDeleteButtons := False;
  GrtShellTabHeaderFrame.DrawBottom := True;

  GrtShellTabHeaderFrame.AddTabSheet(Self,
    _(ValuesTabsheet.Caption),
    '',
    ObjClassPnl,
    nil,
    True, True, -1, []);

  GrtShellTabHeaderFrame.AddTabSheet(Self,
    _(StructsTabsheet.Caption),
    '',
    StructsPnl,
    nil,
    False, True, -1, []);

  GrtShellTabHeaderFrame.AddTabSheet(Self,
    _(ModulesTabSheet.Caption),
    '',
    ModulesPnl,
    nil,
    False, True, -1, []);


  // Create GRT Object Tree
  GrtObjectTree := TGrtObjectTree.Create(nil);
  GrtObjectTree.Parent := ObjClassPnl;
  GrtObjectTree.Align := alClient;
  GrtObjectTree.OnChange := GRTObjTreeChange;
  GrtObjectTree.PopupMenu := GlobalObjTreePopupMenu;
  GrtObjectTree.DisplayValueTypes := True;
  GRTObjectTree.DragMode := dmAutomatic;

  FObjInspectorObject := nil;

  FSortStructsBy := GrtStSTPackage;

  FontSizeMIClick(FontSizeMediumMI);

  ObjectTreeLookup.ItemIndex := 0;

  // load shell history
  if (FileExists(CommonOptions.OptionString['UserDataDir']+'mysqlgrt_history.xml')) then
  begin
    CommandLineUCE.ConsoleHistory := LoadAnsiTextFromFile(
      CommonOptions.OptionString['UserDataDir']+'mysqlgrt_history.xml');
  end;

  // load spreadsheet
  if (FileExists(CommonOptions.OptionString['UserDataDir']+'mysqlgrt_spreadsheet.txt')) then
    CodeSnippetUCE.Text := LoadTextFromFile(CommonOptions.OptionString['UserDataDir']+'mysqlgrt_spreadsheet.txt');

  InitializeShell;

  FScriptFilenameList := TTntStringList.Create;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.FormDestroy(Sender: TObject);

begin
  // store shell history
  SaveAnsiTextToFile(CommonOptions.OptionString['UserDataDir'] + 'mysqlgrt_history.xml',
    CommandLineUCE.ConsoleHistory);

  // store spreadsheet
  if (CodeSnippetUCE.Text <> '') then
    SaveAnsiTextToFile(
      CommonOptions.OptionString['UserDataDir'] + 'mysqlgrt_spreadsheet.txt',
      UTF8Encode(CodeSnippetUCE.Text))
  else
    DeleteFile(CommonOptions.OptionString['UserDataDir'] + 'mysqlgrt_spreadsheet.txt');

  FTreeBtnOpenPNGImg.Free;
  FTreeBtnClosedPNGImg.Free;

  FGrtValueDictPNGImg.Free;
  FGrtValueListPNGImg.Free;
  FGrtValueSimplePNGImg.Free;
  FGrtValueStructPNGImg.Free;
  FGrtModulePNGImg.Free;
  FGrtFunctionPNGImg.Free;

  FScriptFilenameList.Free;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  if (Application.MainForm = self) then
  begin
    Action := caFree;

    // Release ObjInspectorObject
    ObjInspectorObject := nil;

    // Release GrtObjectTree
    GrtObjectTree.Free;
  end
  else
    Action := caHide;

  // In case the application is being terminated and the shell is waiting
  // for scripted user input we tell the waiting GRT thread we are done now.
  if Application.Terminated and CommandLineUCE.ReadScriptInput then
    Grt.ScriptInputRead;
end;

// -----------------------------------------------------------------------------

destructor TXGrtShellForm.Destroy;

begin
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.InitializeShell;

begin
  if (Grt <> nil) then
  begin
    CommandLineUCE.Text := Grt.OutputBuffer;

    CommandLineUCE.ConsolePrompt := myx_grt_shell_get_prompt(Grt.NativeGrt);

    CommandLineUCE.PrepareNextConsoleCommand;

    GrtObjectTree.Grt := Grt;

    RefreshMIClick(self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.OutputCommandLine(Text: WideString);

begin
  CommandLineUCE.AddOutput(#13#10 + Text);
end;

// -----------------------------------------------------------------------------

function TXGrtShellForm.CommandLineUCEIsMultilineCommand(Cmd: WideString;
  Key: Word; Shift: TShiftState): Boolean;

begin
  Result := False;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.CommandLineUCEExecuteCommand(Cmd: WideString;
  Key: Word; Shift: TShiftState);

begin
  if not(CommandLineUCE.ReadScriptInput) then
  begin
    // Ignore new command if there is still an old one pending.
    if FCommandTask = nil then
    begin
      CommandLineUCE.Executing := True;

      CommandLineUCE.AddOutput(#13#10);
      FCommandTask := Grt.CreateShellTask(Cmd, nil, -1, TaskFinished);
      FShowExtraInfo := ssCtrl in Shift;
      Grt.AddTask(FCommandTask);
    end;
  end
  else
  begin
    //Send command to continue script execution
    Grt.ScriptInputRead;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.BuildObjTreeFromLookupSelection;
begin
  if (ObjectTreeLookup.ItemIndex = 0) then
    BuildObjTree('/')
  else
    BuildObjTree(ObjectTreeLookup.Text);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RefreshDisplay;

begin
  BuildObjTreeFromLookupSelection;
  BuildModulesTree;
  BuildStructsTree;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RefreshMIClick(Sender: TObject);

begin
  RefreshDisplay;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.BuildObjTree(ObjectTreeRoot: WideString);


begin
  if (Grt=nil) then
    Exit;

  InspectedObjectEd.Text := '';
  ObjInspectorTree.RootNodeCount := 0;

  GrtObjectTree.ObjectTreeRoot := ObjectTreeRoot;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.GrtObjTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PGrtObjTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData<>nil) and
    ((NodeData.ValueType=MYX_DICT_VALUE) or
    (NodeData.ValueType=MYX_LIST_VALUE)) then
  begin
    ObjInspectorObject := NodeData.Value;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RefreshGRTObjInspector;

var
  PName: PChar;
  ValueType: MYX_GRT_VALUE_TYPE;
  DictStructName: WideString;

begin
  if (ObjInspectorObject<>nil) then
  begin
    ValueType := myx_grt_value_get_type(ObjInspectorObject);

    if ((ValueType = MYX_LIST_VALUE) or
      (ValueType = MYX_DICT_VALUE)) then
    begin
      if (ValueType = MYX_DICT_VALUE) then
      begin
        //Get struct name if there is any
        PName := _myx_grt_dict_struct_get_name(ObjInspectorObject);
        if (PName<>nil) then
          DictStructName := UTF8Decode(PName);

        //Get name
        PName := _myx_grt_dict_name_item_as_string(ObjInspectorObject);
        if (PName<>nil) then
          InspectedObjectEd.Text := UTF8Decode(PName) + ' (' +
            DictStructName + ')'
        else
          InspectedObjectEd.Text := 'DICT';
      end
      else
      begin
        //Get struct name if there is any
        PName := _myx_grt_list_content_get_struct_name(ObjInspectorObject);
        if (PName<>nil) then
          DictStructName := UTF8Decode(PName);

        InspectedObjectEd.Text := '[' +
          UTF8Decode(_myx_get_value_type_as_string(myx_grt_list_content_get_type(ObjInspectorObject)));

        if (DictStructName<>'') then
          InspectedObjectEd.Text := InspectedObjectEd.Text +
            ': '+DictStructName;

        InspectedObjectEd.Text := InspectedObjectEd.Text +
          ']';
      end;



      ObjInspectorTree.NodeDataSize:=sizeof(TGRTObjInspectorData);

      ObjInspectorTree.BeginUpdate;
      try
        ObjInspectorTree.Clear;

        if (ValueType = MYX_DICT_VALUE) then
          ObjInspectorTree.RootNodeCount := myx_grt_dict_item_count(ObjInspectorObject)
        else
          ObjInspectorTree.RootNodeCount := myx_grt_list_item_count(ObjInspectorObject);
      finally
        ObjInspectorTree.EndUpdate;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.SetObjInspectorObject(ObjInspectorObject: Pointer);

begin
  if (FObjInspectorObject <> nil) then
    myx_grt_value_release(FObjInspectorObject);

  FObjInspectorObject := ObjInspectorObject;

  if (FObjInspectorObject <> nil) then
    myx_grt_value_retain(ObjInspectorObject);

  RefreshGRTObjInspector;
end;

// -----------------------------------------------------------------------------

function TXGrtShellForm.GetInspectorObjectText(Node: PVirtualNode; Column: TColumnIndex): WideString;

var
  NodeData: NativeGrtObjInspectorData;

begin
  NodeData := ObjInspectorTree.GetNodeData(Node);

  if Column=0 then
    Result := NodeData^.Key
  else
    Result := NodeData^.Text;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

begin
  CellText := GetInspectorObjectText(Node, Column);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

var
  NodeData: NativeGrtObjInspectorData;

begin
  NodeData := ObjInspectorTree.GetNodeData(Node);

  if Column=0 then
    HintText := NodeData^.Key
  else
    HintText := NodeData^.Text;
end;

// -----------------------------------------------------------------------------

{procedure TXGrtShellForm.NativeGrtObjTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

begin
  if (TextType=ttStatic) then
    TargetCanvas.Font.Color := clGray;
end;}

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ExitMIClick(Sender: TObject);

begin
  Close;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.AboutMIClick(Sender: TObject);

begin
  ShowAboutDialog('MySQL GRT Shell',
    'GRT ' + myx_grt_version,
    'Michael G. Zinner, main concept, ' +
    'Windows development, library coding | ' +
    'Alfredo Kengi Kojima, OS X development, Linux development, ' +
    'library coding | '+
    'Mike Lischke, Windows development, library coding | ' +
    'Vladimir Kolesnikov | library coding | ' +
    'Mike Hillyer, documentation');
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.OpenGRTEnvironmentMIClick(Sender: TObject);

var
  OpenDialog: TTntOpenDialog;
  Error: MYX_GRT_ERROR;
  NewRootValue: Pointer;

begin
  OpenDialog := TTntOpenDialog.Create(self);
  try
    OpenDialog.Filter := _('GRT Environment')+' (*.xml)|*.xml|'+
      _('Any file')+' (*.*)|*.*';

    if (OpenDialog.Execute) then
    begin
      NewRootValue := myx_grt_retrieve_from_file(Grt.NativeGrt, OpenDialog.FileName);
      Error := myx_grt_set_root(Grt.NativeGrt, NewRootValue);

      myx_grt_value_release(NewRootValue);

      if (Error<>MYX_GRT_NO_ERROR) then
      begin
        ShowModalDialog(_('Error'),
          Format(
            _('The following error occured while loading the ' +
              'snapshot. Error: %d.'), [Ord(Error)]),
          myx_mtError, _('Ok'));

        Exit;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.SaveGRTEnvironmentMIClick(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ConvertGRTStructFileToJavaClassesMIClick(Sender: TObject);

var
  PStructs: Pointer;
  Path: WideString;
  Error: MYX_GRT_ERROR;
  OpenDialog: TTntOpenDialog;
  Package: WideString;

begin
  OpenDialog := TTntOpenDialog.Create(self);

  try
    OpenDialog.Filter := _('GRT Struct Definition')+' (*.xml)|*.xml|'+
      _('Any file')+' (*.*)|*.*';

    if (OpenDialog.Execute) then
    begin
      Package := ChangeFileExt(ExtractFileName(OpenDialog.FileName), '');

      if (Copy(Package, 1, 8)<>'structs_') then
        raise EInOutError.Create('The struct definition file must start with structs_');

      Package := AnsiReplaceStr(Copy(Package, 9, Length(Package)), '_', '.');

      PStructs := myx_grt_struct_load_list(OpenDialog.FileName, Addr(Error));
      try
        Path := ExtractFilePath(Application.ExeName)+'java\';
        ForceDirectories(Path);

        myx_grt_struct_export_java_classes(PStructs, Package, Path);
      finally
        myx_grt_structs_free(PStructs);
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.BuildModulesTree;

begin
  if (Grt=nil) then
    Exit;

  ModulesTree.BeginUpdate;
  try
    ModulesTree.Clear;
    ModulesTree.NodeDataSize:=sizeof(TGRTModuleTreeData);

    ModulesTree.RootNodeCount := myx_grt_module_get_count(Grt.NativeGrt);
  finally
    ModulesTree.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: NativeGrtModuleTreeData;
  ParamTypes,
    ReturnType: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.DataType = GrtMtModule) then
  begin
    if (TextType = ttNormal) then
      CellText := NodeData.Module.name;
  end
  else
    if (NodeData.DataType = GrtMtFunction) then
    begin
      if (TextType = ttNormal) then
      begin
        CellText := NodeData.ModuleFunction.name;

        ParamTypes := Trim(myx_grt_module_function_get_params(NodeData.ModuleFunction));

        if (ParamTypes<>'') then
          CellText := CellText + ' '+ParamTypes
        else
          CellText := CellText + ' ()';

        ReturnType := Trim(myx_grt_module_function_get_return_type(NodeData.ModuleFunction));
        if (ReturnType<>'') then
          CellText := CellText + ': '+ReturnType;

        CellText := CellText + ';';
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);

var
  NodeData: NativeGrtModuleTreeData;
  TxtRect: TRect;
  x: integer;
  Icon: TPNGObject;

begin
  if(Column=0)then
  begin
    NodeData := Sender.GetNodeData(Node);

    TxtRect:=Sender.GetDisplayRect(Node, Column, True);

    x:=TxtRect.Left-Sender.OffsetX;

    //Draw > / v
    if (Node.ChildCount>0) or (vsHasChildren in Node.States) then
    begin
      if(Sender.Expanded[Node])then
        FTreeBtnOpenPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
      else
        FTreeBtnClosedPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
    end;

    if (NodeData.DataType = GrtMtModule) then
      Icon := FGrtModulePNGImg
    else
      Icon := FGrtFunctionPNGImg;

    Icon.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData, ParentNodeData: NativeGrtModuleTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (ParentNode = nil) then
  begin
    NodeData.DataType := GrtMtModule;

    NodeData.Module := myx_grt_module_get_by_index(Grt.NativeGrt, Node.Index);

    Include(Node.States, vsHasChildren);
  end
  else
  begin
    ParentNodeData := Sender.GetNodeData(ParentNode);

    if (ParentNodeData.DataType = GrtMtModule) then
    begin
      NodeData.DataType := GrtMtFunction;

      NodeData.ModuleFunction := myx_grt_module_function_get_by_index(
        ParentNodeData.Module, Node.Index);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);

var
  NodeData: NativeGrtModuleTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.DataType = GrtMtModule) then
    ChildCount := myx_grt_module_function_get_count(NodeData.Module)
  else
    ChildCount := 0;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.TntFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
  if (Key = VK_F5) then
    RefreshMIClick(self);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeDblClick(Sender: TObject);

var NodeData, ParentNodeData: NativeGrtModuleTreeData;
  Cmd: WideString;

begin
  if (Sender = ModulesTree) then
  begin
    NodeData := ModulesTree.GetNodeData(ModulesTree.FocusedNode);

    if (NodeData.DataType = GrtMtFunction) then
    begin
      ParentNodeData := ModulesTree.GetNodeData(
        ModulesTree.FocusedNode.Parent);

      Cmd := CommandLineUCE.ConsoleCommand;
      if (Copy(Cmd, Length(Cmd)-1, 2) = #13#10) then
        Cmd := Copy(Cmd, 1, Length(Cmd)-2);

      Cmd := Cmd +
        ParentNodeData.Module.name + ':' +
        NodeData.ModuleFunction.name + '(';

      CommandLineUCE.ConsoleCommand := Cmd;

      try
        CommandLineUCE.SetFocus;
      except
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

var
  NodeData1,
    NodeData2: NativeGrtModuleTreeData;

begin
  NodeData1 := Sender.GetNodeData(Node1);
  NodeData2 := Sender.GetNodeData(Node2);

  if (NodeData1.DataType = GrtMtModule) and
    (NodeData2.DataType = GrtMtModule) then
    Result := CompareText(NodeData1.Module.name, NodeData2.Module.name);
end;


// -----------------------------------------------------------------------------

procedure TXGrtShellForm.FontSizeMIClick(Sender: TObject);

var
  NewFontHeight: Integer;

begin
  if (Sender is TTntMenuItem) then
  begin
    TTntMenuItem(Sender).Checked := True;

    NewFontHeight := TTntMenuItem(Sender).Tag * -1;

    if (NewFontHeight<=-10) then
      CommandLineUCE.Font.Height := NewFontHeight;

    if (NewFontHeight<=-11) then
      CommandLineUCE.Font.Style := [fsBold]
    else
      CommandLineUCE.Font.Style := [];

    CommandLineUCE.Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ModulesTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: NativeGrtModuleTreeData;
  Module: PMYX_GRT_MODULE;

begin
  if (Node<>nil) then
  begin
    NodeData := Sender.GetNodeData(Node);

    if (NodeData.DataType = GrtMtModule) then
      Module := NodeData.Module
    else
      Module := NodeData.ModuleFunction.module;

    ModuleNameLbl.Caption := Module.name;
    PathLbl.Caption := Module.path;
    ExtendsLbl.Caption := Module.extends;
    case myx_grt_module_get_type(Module) of
      MYX_BUILTIN_MODULE_TYPE:
        ModuleTypeLbl.Caption := 'C Module';
      MYX_JAVA_MODULE_TYPE:
        ModuleTypeLbl.Caption := 'Java Module';
      MYX_LUA_MODULE_TYPE:
        ModuleTypeLbl.Caption := 'Lua Module';
      MYX_PYTHON_MODULE_TYPE:
        ModuleTypeLbl.Caption := 'Python Module';
    else
      ModuleTypeLbl.Caption := '-';
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.BuildStructsTree;

begin
  if (Grt=nil) then
    Exit;

  StructsTree.BeginUpdate;
  try
    StructsTree.Clear;
    StructsTree.NodeDataSize:=sizeof(TGRTStructTreeData);

    if (SortStructsBy = GrtStSTHierachy) then
      StructsTree.RootNodeCount := myx_grt_struct_get_child_count(Grt.NativeGrt, '')
    else
      if (SortStructsBy = GrtStSTName) then
        StructsTree.RootNodeCount := myx_grt_struct_get_count(Grt.NativeGrt)
      else
        if (SortStructsBy = GrtStSTPackage) then
          StructsTree.RootNodeCount := myx_grt_package_count(Grt.NativeGrt)
  finally
    StructsTree.EndUpdate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
  
var
  NodeData: NativeGrtStructTreeData;
  TxtRect: TRect;
  x: integer;
  Icon: TPNGObject;

begin
  if(Column=0)then
  begin
    NodeData := Sender.GetNodeData(Node);

    TxtRect:=Sender.GetDisplayRect(Node, Column, True);
    x:=TxtRect.Left-Sender.OffsetX;

    //Draw > / v
    if (Node.ChildCount>0) or (vsHasChildren in Node.States) then
    begin
      if(Sender.Expanded[Node])then
        FTreeBtnOpenPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
      else
        FTreeBtnClosedPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
    end;

    if (NodeData.DataType = GrtStStruct) then
      Icon := FGrtValueStructPNGImg
    else
      Icon := FGrtValueSimplePNGImg;

    Icon.Draw(TargetCanvas,
      Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData, ParentNodeData: NativeGrtStructTreeData;
  ParentName: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (SortStructsBy = GrtStSTHierachy) then
  begin
    NodeData.DataType := GrtStStruct;

    if (ParentNode = nil) then
    begin
      NodeData.Struct := myx_grt_struct_get_child_by_index(
        Grt.NativeGrt, '', Node.Index);

      if (myx_grt_struct_get_child_count(Grt.NativeGrt, '')>0) then
        Include(InitialStates, ivsHasChildren);
    end
    else
    begin
      ParentNodeData := Sender.GetNodeData(ParentNode);

      if (ParentNodeData.DataType = GrtStStruct) then
      begin
        ParentName := Utf8Decode(
          _myx_grt_struct_get_name(ParentNodeData.Struct));

        NodeData.Struct := myx_grt_struct_get_child_by_index(
          Grt.NativeGrt, ParentName, Node.Index);

        if (myx_grt_struct_get_child_count(Grt.NativeGrt, ParentName)>0) then
          Include(InitialStates, ivsHasChildren);
      end;
    end;
  end
  else
    if (SortStructsBy = GrtStSTName) then
    begin
      if (ParentNode = nil) then
      begin
        NodeData.DataType := GrtStStruct;
        NodeData.Struct := myx_grt_struct_get_by_index(Grt.NativeGrt, Node.Index);

        Include(InitialStates, ivsHasChildren);
      end
      else
      begin
        ParentNodeData := Sender.GetNodeData(ParentNode);

        if (ParentNodeData.DataType = GrtStStruct) then
        begin
          NodeData.DataType := GrtStMember;
          NodeData.Member := myx_grt_struct_get_member_by_index(
            ParentNodeData.Struct, Node.Index);
        end;
      end;
    end
    else
      if (SortStructsBy = GrtStSTPackage) then
      begin
        if (ParentNode = nil) then
        begin
          NodeData.DataType := GrtStPackage;
          NodeData.PackageName := myx_grt_package_by_index(Grt.NativeGrt, Node.Index);

          Include(InitialStates, ivsHasChildren);
        end
        else
        begin
          ParentNodeData := Sender.GetNodeData(ParentNode);

          if (ParentNodeData.DataType = GrtStPackage) then
          begin
            NodeData.DataType := GrtStStruct;
            NodeData.Struct := myx_grt_package_struct_by_index(
              Grt.NativeGrt, ParentNodeData.PackageName, Node.Index);

            if (myx_grt_struct_get_member_count(NodeData.Struct)>0) then
              Include(InitialStates, ivsHasChildren);
          end
          else
            if (ParentNodeData.DataType = GrtStStruct) then
            begin
              NodeData.DataType := GrtStMember;
              NodeData.Member := myx_grt_struct_get_member_by_index(
                ParentNodeData.Struct, Node.Index);
            end;
        end;
      end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);

var
  NodeData: NativeGrtStructTreeData;
  Name: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (SortStructsBy = GrtStSTHierachy) then
  begin
    Name := Utf8Decode(_myx_grt_struct_get_name(NodeData.Struct));
    ChildCount := myx_grt_struct_get_child_count(Grt.NativeGrt, Name);
  end
  else
    if (SortStructsBy = GrtStSTName) then
    begin
      if (NodeData.DataType = GrtStStruct) then
        ChildCount := myx_grt_struct_get_member_count(NodeData.Struct)
      else
        ChildCount := 0;
    end
    else
      if (SortStructsBy = GrtStSTPackage) then
      begin
        if (NodeData.DataType = GrtStPackage) then
          ChildCount := myx_grt_package_struct_count(Grt.NativeGrt, NodeData.PackageName)
        else
          if (NodeData.DataType = GrtStStruct) then
            ChildCount := myx_grt_struct_get_member_count(NodeData.Struct)
          else
            ChildCount := 0;
      end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: NativeGrtStructTreeData;
  InheritedCaption: Integer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.DataType = GrtStPackage) then
  begin
    if (TextType = ttNormal) then
    begin
      if (NodeData.PackageName<>'') then
        CellText := NodeData.PackageName
      else
        CellText := '<base>';
    end;
  end
  else
    if (NodeData.DataType = GrtStStruct) then
    begin
      if (TextType = ttNormal) then
        CellText := Utf8Decode(_myx_grt_struct_get_name(NodeData.Struct))
      else
      begin
        CellText := Utf8Decode(_myx_grt_struct_get_caption(Grt.NativeGrt, NodeData.Struct, @InheritedCaption));
        if (CellText<>'') and (InheritedCaption=1) then
          CellText := '<<' + CellText + '>>'
        else
          CellText := '(' + CellText + ')';
      end;
    end
    else
    begin
      if (TextType = ttNormal) then
        CellText :=
          Utf8Decode(_myx_grt_struct_get_member_name(NodeData.Member)) +
          ': ' + Utf8Decode(_myx_get_value_type_as_string(
            myx_grt_struct_member_get_type(NodeData.Member))) + ';'
      else
        CellText := Utf8Decode(_myx_grt_struct_get_member_desc(NodeData.Member));
    end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjectTreeLookupCloseUp(Sender: TObject);

begin
  BuildObjTreeFromLookupSelection;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjectTreeLookupKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);

var
  Index: Integer;

begin
  if (Key = VK_RETURN) then
  begin
    Index := ObjectTreeLookup.Items.IndexOf(ObjectTreeLookup.Text);
    if (Index = -1) then
      Index := ObjectTreeLookup.Items.Add(ObjectTreeLookup.Text);

    ObjectTreeLookup.ItemIndex := Index;
    BuildObjTreeFromLookupSelection;

    GRTObjTreeChange(GRTObjectTree, GRTObjectTree.GetFirst);

    Key := 0;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

var
  NodeData1, NodeData2: NativeGrtStructTreeData;
  DotCount1, DotCount2: Integer;
  Name1, Name2: WideString;

begin
  NodeData1 := Sender.GetNodeData(Node1);
  NodeData2 := Sender.GetNodeData(Node2);

  if (NodeData1.DataType=GrtStPackage) and
    (NodeData2.DataType=GrtStPackage) then
  begin
    DotCount1 := GetSubstringCount('.', NodeData1.PackageName);
    DotCount2 := GetSubstringCount('.', NodeData2.PackageName);

    if (DotCount1<>DotCount2) then
      Result := DotCount1-DotCount2
    else
      Result := WideCompareText(NodeData1.PackageName,
        NodeData2.PackageName);
  end
  else
    if (NodeData1.DataType=GrtStStruct) and
      (NodeData2.DataType=GrtStStruct) then
    begin
      Name1 := Utf8Decode(_myx_grt_struct_get_name(NodeData1.Struct));
      Name2 := Utf8Decode(_myx_grt_struct_get_name(NodeData2.Struct));

      DotCount1 := GetSubstringCount('.', Name1);
      DotCount2 := GetSubstringCount('.', Name2);

      if (DotCount1<>DotCount2) then
        Result := DotCount1-DotCount2
      else
        Result := WideCompareText(Name1, Name2);
    end
    else
      if (NodeData1.DataType=GrtStMember) and
        (NodeData2.DataType=GrtStMember) then
      begin
        Name1 := Utf8Decode(_myx_grt_struct_get_member_name(NodeData1.Member));
        Name2 := Utf8Decode(_myx_grt_struct_get_member_name(NodeData2.Member));

        Result := WideCompareText(Name1, Name2);
      end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.SetSortStructsBy(SortStructsBy: GrtStructSortType);

begin
  if (FSortStructsBy<>SortStructsBy) then
  begin
    FSortStructsBy := SortStructsBy;

    BuildStructsTree;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsOrderbyNameMIClick(Sender: TObject);

begin
  SortStructsBy := GrtStSTName;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsOrderbyHierachyMIClick(Sender: TObject);

begin
  SortStructsBy := GrtStSTHierachy;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsOrderbyPackagesMIClick(Sender: TObject);

begin
  SortStructsBy := GrtStSTPackage;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructPopupMenuPopup(Sender: TObject);

begin
  if (SortStructsBy = GrtStSTHierachy) then
    StructsOrderbyHierachyMI.Checked := True
  else
    if (SortStructsBy = GrtStSTName) then
      StructsOrderbyNameMI.Checked := True
    else
      if (SortStructsBy = GrtStSTPackage) then
        StructsOrderbyPackagesMI.Checked := True;

end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

var
  NodeData: NativeGrtStructTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.DataType = GrtStStruct) then
    HintText := Utf8Decode(_myx_grt_struct_get_desc(Grt.NativeGrt, NodeData.Struct))
  else if (NodeData.DataType = GrtStMember) then
    HintText := Utf8Decode(_myx_grt_struct_get_member_desc(NodeData.Member));
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.CommandLineUCEDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

begin
  if (Source = GRTObjectTree) then
    Accept := True
  else
    Accept := False;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.CommandLineUCEDragDrop(Sender, Source: TObject; X,
  Y: Integer);

begin
  if (Source = GRTObjectTree) then
  begin
    CommandLineUCE.SelectedText := 'grtV.getGlobal("'+
      GRTObjectTree.GetObjectPath(GRTObjectTree.FocusedNode) +'")';

    {Cmd := CommandLineUCE.ConsoleCommand;
    if (Copy(Cmd, Length(Cmd)-1, 2) = #13#10) then
      Cmd := Copy(Cmd, 1, Length(Cmd)-2);

    Cmd := Cmd + 'grtV.getGlobal("'+
      GRTObjectTree.GetObjectPath(GRTObjectTree.FocusedNode) +'")';

    CommandLineUCE.ConsoleCommand := Cmd;}

    CommandLineUCE.SetFocus;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorCopyValueMIClick(Sender: TObject);

begin
  if (ObjInspectorTree.FocusedNode<>nil) then
    TntClipboard.AsWideText :=
      GetInspectorObjectText(ObjInspectorTree.FocusedNode,
        ObjInspectorTree.FocusedColumn);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RemoveObjectMIClick(Sender: TObject);

var
  NodeData,
  ParentNodeData: PGrtObjTreeData;

begin
  if (GRTObjectTree.FocusedNode <> nil) and
    (GRTObjectTree.FocusedNode <> GRTObjectTree.GetFirst) then
  begin
    NodeData := GRTObjectTree.GetNodeData(GRTObjectTree.FocusedNode);
    ParentNodeData := GRTObjectTree.GetNodeData(GRTObjectTree.FocusedNode.Parent);

    if (ParentNodeData.ValueType = MYX_DICT_VALUE) then
    begin
      myx_grt_dict_item_del(ParentNodeData.Value,
        NodeData.Key);
    end
    else
      if (ParentNodeData.ValueType = MYX_LIST_VALUE) then
      begin
        myx_grt_list_item_del(ParentNodeData.Value,
          GRTObjectTree.FocusedNode.Index);
      end;

    RefreshMIClick(self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

var
  PValue: Pointer;
  ValueType: MYX_GRT_VALUE_TYPE;

begin
  Allowed := False;

  if (Column = 1) then
  begin
    if (ObjInspectorObject<>nil) then
    begin
      ValueType := myx_grt_value_get_type(ObjInspectorObject);
      PValue := nil;

      if (ValueType = MYX_DICT_VALUE) then
      begin
        if (Integer(Node.Index) <
          myx_grt_dict_item_count(ObjInspectorObject)) then
          PValue := myx_grt_dict_item_value_by_index(ObjInspectorObject, Node.Index);
      end
      else
        if (ValueType = MYX_LIST_VALUE) then
        begin
          if (Integer(Node.Index) <
            myx_grt_list_item_count(ObjInspectorObject)) then
            PValue := myx_grt_list_item_get(ObjInspectorObject, Node.Index);
        end;

      if (PValue <> nil) then
        Allowed := myx_grt_value_is_simple_type(PValue) = 1;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);

var
  ValueType: MYX_GRT_VALUE_TYPE;
  NodeData: NativeGrtObjInspectorData;

begin
  if (ObjInspectorObject <> nil) then
  begin
    NodeData := Sender.GetNodeData(Node);
    ValueType := myx_grt_value_get_type(ObjInspectorObject);

    case ValueType of
      MYX_DICT_VALUE:
        Grt.DictString[ObjInspectorObject, NodeData.Key] := NewText;
      MYX_LIST_VALUE:
        Grt.ListString[ObjInspectorObject, Node.Index] := NewText;
    end;
    Sender.ReinitNode(Node, False);
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: NativeGrtObjInspectorData;

begin
  NodeData := ObjInspectorTree.GetNodeData(Node);

  NodeData^.Value := nil;

  case myx_grt_value_get_type(ObjInspectorObject) of
    MYX_LIST_VALUE:
    begin
      NodeData^.Key := '[' + IntToStr(Node.Index + 1) + ']';
      NodeData^.Value := myx_grt_list_item_get(
        ObjInspectorObject, Node.Index);
    end;

    MYX_DICT_VALUE:
    begin
      NodeData^.Key := myx_grt_dict_item_key_by_index(
        ObjInspectorObject, Node.Index);
      NodeData^.Value := myx_grt_dict_item_value_by_index(
        ObjInspectorObject, Node.Index);
    end;
  end;

  if (NodeData^.Value <> nil) then
    NodeData^.Text := myx_grt_value_formated_as_string(
      NodeData^.Value);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RefreshInspectorMIClick(Sender: TObject);

begin
  RefreshGRTObjInspector;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: NativeGrtObjInspectorData;

begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.StructsTreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: NativeGrtStructTreeData;

begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ObjInspectorCopyAllValuesMIClick(Sender: TObject);

var
  S: WideString;
  Node: PVirtualNode;

begin
  Node := ObjInspectorTree.GetFirst;
  S := '';

  repeat
    S := S + GetInspectorObjectText(Node, 1) + #13#10;

    Node := ObjInspectorTree.GetNext(Node);
  until (Node = nil);

  TntClipboard.AsWideText := S;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ScriptOpenActionAccept(Sender: TObject);

var
  TabSheet: TTntTabSheet;
  Editor: TUniCodeEdit;

begin
  TabSheet := TTntTabSheet.Create(self);
  TabSheet.Caption := WideExtractFileName(ScriptOpenAction.Dialog.FileName);

  TabSheet.PageControl := MainPageControl;

  Editor := TUniCodeEdit.Create(self);
  Editor.Parent := TabSheet;
  Editor.Align := alClient;
  Editor.Font.Name := 'Bitstream Vera Sans Mono';
  Editor.Font.Size := 8;
  Editor.GutterColor := clBtnFace;
  Editor.GutterWidth := 30;
  Editor.Options := Editor.Options +
    [eoLineNumbers, eoKeepTrailingBlanks];
  Editor.SelectedColor.Background := clGray;
  Editor.IndentSize := 2;

  Editor.LoadFromFile(ScriptOpenAction.Dialog.FileName, tfUTF8);

  FScriptFilenameList.AddObject(ScriptOpenAction.Dialog.FileName,
    TabSheet);

  MainPageControl.ActivePage := TabSheet;

  AppOptions.OptionString['LastScriptFile'] :=
    ScriptOpenAction.Dialog.FileName;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ScriptSaveActionExecute(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to FScriptFilenameList.Count - 1 do
    if (FScriptFilenameList.Objects[I] =
      MainPageControl.ActivePage) and
      (MainPageControl.ActivePage.Controls[0] is TUniCodeEdit) then
    begin
      TUniCodeEdit(MainPageControl.ActivePage.Controls[0]
        ).SaveToFile(FScriptFilenameList[I], tfUTF8);

      break;
    end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ScriptRunActionExecute(Sender: TObject);

var
  I: Integer;

begin
  if (MainPageControl.ActivePage = SnippetsSheet) then
  begin
    CommandLineUCE.ConsoleCommand :=
      CodeSnippetUCE.SelectedText;

    MainPageControl.ActivePage := GtrShellSheet;

    Application.ProcessMessages;

    CommandLineUCE.ExecuteCommand;
  end
  else
  begin
    for I := 0 to FScriptFilenameList.Count - 1 do
      if (FScriptFilenameList.Objects[I] =
        MainPageControl.ActivePage) and
        (MainPageControl.ActivePage.Controls[0] is TUniCodeEdit) then
      begin
        {TUniCodeEdit(MainPageControl.ActivePage.Controls[0]
          ).SaveToFile(FScriptFilenameList[I], tfUTF8);

        CommandLineUCE.ConsoleCommand := 'run ' +
          WideStringReplace(FScriptFilenameList[I], '\', '/',
            [rfReplaceAll]);}

        CommandLineUCE.ConsoleCommand :=
          TUniCodeEdit(MainPageControl.ActivePage.Controls[0]).Content.Text;

        MainPageControl.ActivePage := GtrShellSheet;

        Application.ProcessMessages;

        CommandLineUCE.ExecuteCommand;

        break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.ScriptOpenActionBeforeExecute(Sender: TObject);

var
  LastScriptFile: WideString;

begin
  LastScriptFile := AppOptions.OptionString['LastScriptFile'];
  if (LastScriptFile <> '') then
  begin
    ScriptOpenAction.Dialog.FileName :=
      WideExtractFileName(LastScriptFile);
    ScriptOpenAction.Dialog.InitialDir :=
      WideExtractFilePath(LastScriptFile);
  end
  else
    ScriptOpenAction.Dialog.InitialDir :=
      GetApplDir + 'scripts';


end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RemoveTabsheetMIClick(Sender: TObject);

begin
  MainPageControl.ActivePage.Free;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.TabSheetsPopupMenuPopup(Sender: TObject);

begin
  RemoveTabsheetMI.Enabled :=
    (MainPageControl.ActivePage <> GtrShellSheet) and
    (MainPageControl.ActivePage <> SnippetsSheet);
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.RefreshDisplayActionExecute(Sender: TObject);

begin
  RefreshDisplay;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TXGrtShellForm.TaskFinished(Task: IGrtGenericTask);

begin
  if Task = FCommandTask then
  begin
    if FCommandTask.Result = MYX_GRT_SHELL_COMMAND_EXIT then
      Application.Terminate
    else
    begin
      if FShowExtraInfo then
        CommandLineUCE.AddOutput(Format(_('Executed in %f seconds.') + #13#10, [Task.ExecutionTime / 1000]));
      CommandLineUCE.PrepareNextConsoleCommand;
      CommandLineUCE.ConsolePrompt := myx_grt_shell_get_prompt(Grt.NativeGrt);
    end;
    FCommandTask := nil;
  end;

  CommandLineUCE.Executing := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TXGrtShellForm.TaskHandleError: Boolean;

// Checks if the task has returned an error. If so the error message is shown, the error images are set and
// True is returned. Otherwise False is the result.

begin
  Result := False;

  CommandLineUCE.Content.AddLine(_('A problem occured while executing shell command.'));
  CommandLineUCE.Content.AddLine(Format(_('The error is: %s (code: %d).'),
    [FCommandTask.ErrorString, Ord(FCommandTask.ErrorCode)]))
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.DisplayObjectValuesActionExecute(Sender: TObject);

begin
  GrtObjectTree.DisplayObjectValues := TTntAction(Sender).Checked;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.DisplayObjectRefcountActionExecute(
  Sender: TObject);

begin
  GRTObjectTree.DisplayReferenceCountValues := TTntAction(Sender).Checked;
  GRTObjectTree.Invalidate;
end;

// -----------------------------------------------------------------------------

procedure TXGrtShellForm.DisplayTypeInfoActionExecute(Sender: TObject);

begin
  GRTObjectTree.DisplayValueTypes := TTntAction(Sender).Checked;
  GRTObjectTree.Invalidate;
end;

// -----------------------------------------------------------------------------

end.
