unit MigrationCreateObjs;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees, TntSysUtils,
  MigrationObjMapDetail, PngTools,
  Grt, WizardPage, UniCodeEditor, UCEHighlighter, UCESQLHighlighter;

type
  TMigrationCreateObjsForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    CreateObjTabSheet: TTntTabSheet;
    TntGroupBox6: TTntGroupBox;
    CreateImg: TTntImage;
    TntLabel10: TTntLabel;
    TntLabel11: TTntLabel;
    TntLabel15: TTntLabel;
    ConnectImg: TTntImage;
    ConnectLbl: TTntLabel;
    ResultLbl: TTntLabel;
    MessageLogGBox: TTntGroupBox;
    GRTMessageMemo: TTntMemo;
    CreateOptionsTabSheet: TTntTabSheet;
    TntGroupBox2: TTntGroupBox;
    TntLabel1: TTntLabel;
    TntLabel3: TTntLabel;
    CreateObjectsCBox: TTntCheckBox;
    TntLabel6: TTntLabel;
    CreateScriptCBox: TTntCheckBox;
    ScriptFileNameEd: TTntEdit;
    ScriptFileNameLbl: TTntLabel;
    ChooseCreatesFileBtn: TTntButton;
    TntLabel5: TTntLabel;
    GrtMessageLbl: TTntLabel;
    GrtProgressBar: TTntProgressBar;
    GrtProgressLbl: TTntLabel;
    CreateResultsTabSheet: TTntTabSheet;
    LogOptionPnl: TTntPanel;
    LogOptionsGBox: TTntGroupBox;
    SqlUCE: TUniCodeEdit;
    ApplyChangesBtn: TTntButton;
    DiscardChangesBtn: TTntButton;
    DragPnl: TTntPanel;
    LogMainPnl: TTntPanel;
    LogHeaderLbl: TTntLabel;
    LogFilterLbl: TTntLabel;
    MigLogVT: TVirtualStringTree;
    LogFilterComboBox: TTntComboBox;
    NoMapProblemsPnl: TTntPanel;
    TntShape1: TTntShape;
    TntLabel2: TTntLabel;
    TntLabel4: TTntLabel;
    TntImage1: TTntImage;
    UCESQLHighlighter: TUCESQLHighlighter;
    RecreateObjectsBtn: TTntButton;
    TntBevel1: TTntBevel;
    TntLabel7: TTntLabel;
    CommentOutCBox: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DoCreateObjs;
    procedure DockPnlResize(Sender: TObject);
    procedure DoProgressError(
      msg: WideString; TaskImg: TTntImage);
    procedure ChooseCreatesFileBtnClick(Sender: TObject);
    procedure CreateScriptCBoxClick(Sender: TObject);

    procedure ProcessGrtOutput(S: WideString);
    procedure ProcessGrtMessages(Messages: TMYX_GRT_MSGS);
    function ProcessGrtStatusQuery: Integer;
    procedure MigLogVTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure MigLogVTBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure MigLogVTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MigLogVTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure MigLogVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure MigLogVTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure MigLogVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MigLogVTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure MigLogVTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MigLogVTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MigLogVTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure LogFilterComboBoxCloseUp(Sender: TObject);

    procedure RefreshLog;
    procedure DragPnlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragPnlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RecreateObjectsBtnClick(Sender: TObject);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DiscardChangesBtnClick(Sender: TObject);
    procedure CommentOutCBoxClick(Sender: TObject);

    procedure CheckMatchingSourceTarget;
  private
    { Private declarations }
    FTreeBtnOpenPNGImg,
      FTreeBtnClosedPNGImg: TPNGObject;

    FIconList: TTntStringList;
    FGrtValueListPNGImg: TPNGObject;

    FLogError,
      FLogErrorSmall,
      FLogWarning,
      FLogWarningSmall: TPNGObject;

    FMigLogMainObjList,
      FMigLogObjList: TList;

    FMouseY,
      FOldOptionPnlHeight: Integer;

    FInitSQLEdit: Boolean;
  public
    { Public declarations }
    procedure BeforeSubPageIndexChange(SectionIndex: Integer); override;
  protected
    procedure SetSubPageIndex(NewSubPageIndex: Integer); override;
    function GetSubPageIndex: Integer; override;

    function GetSubPageCount: integer; override;

    function GetSectionTitle: WideString; override;
    function GetSectionInfo: WideString; override;

    function GetSupportAdvancedOptions: Boolean; override;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); override;
    function GetAdvancedOptionsState: Boolean; override;

    procedure SqlUCEChange(Sender: TObject; Line: TUCELine);
    function MigLogGetCellText(
      Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex;
      TextType: TVSTTextType): WideString;
    function HideLogNodeIfNoErrorsWarnings(
      Node: PVirtualNode): Boolean;

  end;

  TLogTreeDataType = (
    LTDT_Object,
    LTDT_ListMember
  );

  PLogTreeData = ^TLogTreeData;
  TLogTreeData = record
    NodeType: TLogTreeDataType;

    PStruct: Pointer;
    StructName: WideString;
    PMember: Pointer;
    MemberName: WideString;
    Value: Pointer;

    PSourceObject: Pointer;
    SourceCaption: WideString;

    PLogEntry: Pointer;
    ContainsError: Boolean;
    ContainsWarning: Boolean;
    PMessages: Pointer;

    PTargetObject: Pointer;
    TargetCaption: WideString;
    TargetStructName: WideString;
  end;

var
  MigrationCreateObjsForm: TMigrationCreateObjsForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.FormCreate(Sender: TObject);

begin
  InitForm(self);
  
  DockedPanel := DockPnl;

  MainPngControl.Align := alNone;
  MainPngControl.Left := -4;
  MainPngControl.Top := -27;
  MainPngControl.Width := 765+4+4;
  MainPngControl.Height := 529+274;
  MainPngControl.ActivePageIndex := 0;

  FMigLogMainObjList := nil;
  FMigLogObjList := nil;

  FTreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  FTreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  FGrtValueListPNGImg := LoadPNGImageFromResource('grt_value_list');

  FLogError := LoadPNGImageFromResource('log_error_16x16');
  FLogErrorSmall := LoadPNGImageFromResource('log_error_12x12');
  FLogWarning := LoadPNGImageFromResource('log_warning_16x16');
  FLogWarningSmall := LoadPNGImageFromResource('log_warning_12x12');

  FIconList := TTntStringList.Create;

  FInitSQLEdit := False;

  ScriptFileNameEd.Text :=
    WideIncludeTrailingBackslash(
      GetSpecialFolder(CSIDL_PERSONAL)) + 'Creates.sql';

  SqlUCE.Content.OnChangeLine := SqlUCEChange;

  LogFilterComboBox.ItemIndex := 0;

  MigLogVT.NodeDataSize := SizeOf(TLogTreeData);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.FormDestroy(Sender: TObject);

var
  I: Integer;

begin
  FTreeBtnOpenPNGImg.Free;
  FTreeBtnClosedPNGImg.Free;

  FGrtValueListPNGImg.Free;

  FLogError.Free;
  FLogErrorSmall.Free;
  FLogWarning.Free;
  FLogWarningSmall.Free;

  for I := 0 to FIconList.Count-1 do
    FIconList.Objects[i].Free;
  FIconList.Free;
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
  begin
    CheckMatchingSourceTarget();
    MainPngControl.ActivePageIndex := 0;
  end
  else
    if (NewSubPageIndex=1) then
    begin
      // Set script checkpoint
      Grt.GlobalAsInt['/migration/applicationData/' +
        'ScriptGenerationCheckpoint'] := 5;

      if (MainPngControl.ActivePageIndex = 0) then
        DoCreateObjs
      else
        MainPngControl.ActivePageIndex := 1;
    end
    else
      RefreshLog;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Object Creation Options');
    1:
      Result:=_('Creating Objects');
    2:
      Result:=_('Creation Results');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Please define how the object creation should be performed.');
    1:
      Result:=_('The object creation is executed.');
    2:
      Result:=_('Please check if errors have occured during the object creation.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    1:
      Result := True;
    2:
      Result := True;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    1:
      MessageLogGBox.Visible := State;
    2:
      LogOptionPnl.Visible := State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    1:
      Result := MessageLogGBox.Visible;
    2:
      Result := LogOptionPnl.Visible;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.BeforeSubPageIndexChange(SectionIndex: Integer);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------
// ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DoCreateObjs;

var
  ModuleName: WideString;
  PSql: Pointer;
  F: TextFile;
  Task: IGrtTask;

begin
  ConnectImg.Picture.Assign(TaskUncheckedPNGImg);
  CreateImg.Picture.Assign(TaskUncheckedPNGImg);

  if (Not(CreateObjectsCBox.Checked)) then
  begin
    ConnectImg.Picture.Assign(TaskDisabledPNGImg);
    ConnectLbl.Enabled := False;
  end
  else
  begin
    ConnectImg.Picture.Assign(TaskUncheckedPNGImg);
    ConnectLbl.Enabled := True;
  end;

  // initialize progress widgets
  ResultLbl.Caption := _('Executing ...');
  ResultLbl.Show;
  ResultLbl.Font.Color := clBlack;
  GrtMessageLbl.Caption := '';
  GrtMessageLbl.Show;
  GRTMessageMemo.Text := '';
  GrtProgressBar.Visible := False;
  GrtProgressLbl.Visible := False;

  MainPngControl.ActivePage := CreateObjTabSheet;

  //Start animation
  DoProcessStart;
  try
    Application.ProcessMessages;

    //Do Create objects
    try
      if (CreateScriptCBox.Checked) and
        (ScriptFileNameEd.Text <> '') then
      begin
        ModuleName := Grt.DictString[
            Grt.DictItem[
              Grt.Global['/migration/targetConnection'], 'modules'],
          'TransformationModule'];

        Task := Grt.CreateStandardTask(
          _('Getting SQL script'),
          ModuleName,
          'getSqlScript',
          [Grt.GetGlobalAsParam('/migration/targetCatalog'),
            Grt.Global['/migration/objectCreationParams']],
          ProcessGrtOutput,
          ProcessGrtMessages,
          True);
        Grt.AddTaskAndWaitWithMessages(Task, True);
        PSql := Task.Result;
        
        AssignFile(F, ScriptFileNameEd.Text);
        Rewrite(F);
        Write(F, _myx_grt_value_as_string(PSql));
        CloseFile(F);

        Grt.ValueRelease(PSql);
      end;

      if (CreateObjectsCBox.Checked) then
      begin
        ModuleName := Grt.DictString[
            Grt.DictItem[
              Grt.Global['/migration/targetConnection'], 'modules'],
          'TransformationModule'];

        // Clear log
        myx_grt_list_clear(Grt.Global['/migration/creationLog']);

        Task := Grt.CreateStandardTask(
          _('Executing SQL statements'),
          ModuleName, 'executeSqlStatements',
          [Grt.GetGlobalAsParam('/migration/targetConnection'),
            Grt.GetGlobalAsParam('/migration/targetCatalog'),
            Grt.GetGlobalAsParam('/migration/creationLog')],
          ProcessGrtOutput,
          ProcessGrtMessages,
          True,
          True,
          -1,
          nil,
          ProcessGrtStatusQuery);
        Grt.AddTaskAndWaitWithMessages(Task, True);
      end;
    except
      on x: EGrtError do
      begin
        DoProgressError(Format(
            _('The database objects cannot be migrated '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          ConnectImg);

        CreateImg.Picture.Assign(TaskErrorPNGImg);

        Exit;
      end;
    end;

    if (CreateObjectsCBox.Checked) then
      ConnectImg.Picture.Assign(TaskCheckedPNGImg);

    CreateImg.Picture.Assign(TaskCheckedPNGImg);

    if (FWizardInterface.OperationCanceled) then
      ResultLbl.Caption := _('Canceled by user.')
    else
      ResultLbl.Caption := _('Execution completed successfully.');

    GrtMessageLbl.Hide;
    GrtProgressBar.Hide;
    GrtProgressLbl.Hide;
  finally
    //Stop animation
    DoProcessEnd;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.ProcessGrtOutput(S: WideString);

begin
  GRTMessageMemo.Lines.Add(S);
  GRTMessageMemo.SelStart := GRTMessageMemo.GetTextLen;
  GRTMessageMemo.Perform(EM_SCROLLCARET, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.ProcessGrtMessages(
  Messages: TMYX_GRT_MSGS);

var
  Msg: TMYX_GRT_MSG;

begin
  // display last message in ResultLbl
  if (Messages.msgs.Count > 0) then
  begin
    Msg := Messages.msgs[Messages.msgs.Count - 1];

    if (Msg.msg_type = 2) then
    begin
      GrtProgressLbl.Caption := Msg.msg;
      GrtProgressLbl.Update;

      if (Msg.progress = -1) then
      begin
        GrtProgressBar.Hide;
        GrtProgressLbl.Hide;
      end
      else
      begin
        GrtProgressBar.Position := Msg.progress;

        if (Not(GrtProgressBar.Visible)) then
        begin
          GrtProgressBar.Show;
          GrtProgressLbl.Show;
        end;
      end;
    end
    else
    begin
      GrtMessageLbl.Caption := Msg.msg;
      GrtMessageLbl.Update;
    end;

    ProcessGrtOutput(FormatGrtMessagesAsString(Messages));
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.ProcessGrtStatusQuery: Integer;

begin
  Result := Ord(FWizardInterface.OperationCanceled);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DoProgressError(
  msg: WideString; TaskImg: TTntImage);

begin
  ResultLbl.Caption:=_('An Error has occured. Press [Advanced >>] '+
    'to see the log.');
  ResultLbl.Show;

  if(TaskImg<>nil)then
    TaskImg.Picture.Assign(TaskErrorPNGImg);

  GRTMessageMemo.Lines.Add(msg);

  //Stop animation
  if(Assigned(OnProcessEnd))then
    OnProcessEnd(self);
end;


procedure TMigrationCreateObjsForm.ChooseCreatesFileBtnClick(
  Sender: TObject);

var
  SaveDlg: TSaveDialog;
  FileName: WideString;

begin
  SaveDlg := TSaveDialog.Create(self);
  try
    SaveDlg.Title := 'SQL Script File';

    SaveDlg.Filter := _('SQL Creates Script')+
      ' (*.sql)|*.sql|'+
      _('All files')+' (*.*)|*.*';

    if (SaveDlg.Execute) then
    begin
      FileName := SaveDlg.Filename;

      if (Pos('.', FileName)=0) then
        FileName := FileName + '.sql';

      ScriptFileNameEd.Text := FileName;
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TMigrationCreateObjsForm.CreateScriptCBoxClick(Sender: TObject);
begin
  ScriptFileNameLbl.Enabled := CreateScriptCBox.Checked;
  ScriptFileNameEd.Enabled := CreateScriptCBox.Checked;
  ChooseCreatesFileBtn.Enabled := CreateScriptCBox.Checked;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PLogTreeData;
  Index: Integer;
  PngImg: TPNGObject;
  PImageData: PChar;
  DataLength: Integer;
  TxtRect: TRect;
  x: integer;

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

  if (Column = 2) then
    inc(x, 2);


  if (Column = 0) or
    ((Column = 2) and (NodeData.PTargetObject<>nil)) then
  begin
    if (NodeData.NodeType = LTDT_Object) then
    begin
      Index := FIconList.IndexOf(NodeData.StructName);
      if (Index = -1) then
      begin
        PImageData := _myx_grt_struct_get_icon(Grt.NativeGrt,
          PChar(ExtractFilePath(Application.ExeName)+'images\structs\'),
          NodeData.PStruct, MYX_IT_SMALL, @DataLength);

        PngImg := LoadPNGImageFromPChar(PImageData, DataLength);

        FIconList.AddObject(NodeData.StructName, PngImg);
      end
      else
        PngImg := TPNGObject(FIconList.Objects[Index]);

      PngImg.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));

      if (NodeData.ContainsError) then
        FLogErrorSmall.Draw(TargetCanvas,
          Rect(x-16+8, CellRect.Top+1+6, x+8, CellRect.Top+1+16+6))
      else
        if (NodeData.ContainsWarning) then
          FLogWarningSmall.Draw(TargetCanvas,
            Rect(x-16+8, CellRect.Top+1+7, x+8, CellRect.Top+1+16+7))
    end
    else
    begin
      FGrtValueListPNGImg.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
var
  NodeData: PLogTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType = LTDT_ListMember) then
  begin
    EraseAction := eaColor;
    ItemColor := $00EEEEEE;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PLogTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  FInitSQLEdit := True;
  try
    if (NodeData<>nil) and
      (NodeData.NodeType = LTDT_Object) then
    begin
      if (myx_grt_struct_inherits_from(Grt.NativeGrt,
        NodeData.StructName, 'db.DatabaseObject') = 1) and
        (NodeData.PSourceObject <> nil) then
      begin
        SqlUCE.Text := TntAdjustLineBreaks(
          UTF8Decode(_myx_grt_dict_item_get_as_string(
            NodeData.PSourceObject, 'sql')));

        CommentOutCBox.Checked :=
          (Grt.DictInt[NodeData.PSourceObject, 'commentedOut'] = 1);

        SqlUCE.Enabled := Not(CommentOutCBox.Checked);
        CommentOutCBox.Enabled := True;

        SqlUCE.ScrollBars := ssNone;
        Application.ProcessMessages;
        SqlUCE.ScrollBars := ssBoth;
      end;
    end
    else
    begin
      SqlUCE.Text := '';
      SqlUCE.Enabled := False;
      CommentOutCBox.Enabled := False;

      SqlUCE.ScrollBars := ssNone;
      Application.ProcessMessages;
      SqlUCE.ScrollBars := ssBoth;
    end;
  finally
    FInitSQLEdit := False;
  end;

  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTEditing(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

begin
  Allowed := False;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTGetHint(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

begin
  HintText := MigLogGetCellText(Sender, Node, Column, ttNormal);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if (Column = 0) or (Column = 2) then
    ImageIndex := 1;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

begin
  CellText := MigLogGetCellText(Sender, Node, Column, TextType);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

var
  NodeData: PLogTreeData;

begin
  NodeData := Sender.GetNodeData(Node);
                               
  if (NodeData.NodeType = LTDT_Object) then
    ChildCount := GetListMemberCount(Grt.NativeGrt, NodeData.StructName, False)
    else if (NodeData.NodeType = LTDT_ListMember) then
      ChildCount := myx_grt_list_item_count(NodeData.Value);
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData,
    ParentNodeData: PLogTreeData;
  PParentSourceObject: Pointer;
  ParentMemberName: WideString;
  ParentNodeType : TLogTreeDataType;
  PLog,
    PLogEntry,
    PMessage: Pointer;
  I,
    J: Integer;

  SourceObjectId,
    LogSourceObjectId: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Sender.GetNodeLevel(Node) < 2) then
    include(InitialStates, ivsExpanded);

  if (ParentNode = nil) then
  begin
    ParentNodeData := nil;
    ParentNodeType := LTDT_ListMember;
    PParentSourceObject :=
      Grt.Global['/migration/targetCatalog'];
    ParentMemberName := 'schemata';
  end
  else
  begin
    ParentNodeData := Sender.GetNodeData(ParentNode);

    ParentNodeType := ParentNodeData.NodeType;
    PParentSourceObject := ParentNodeData.PSourceObject;
    ParentMemberName := ParentNodeData.MemberName;
  end;

  if (ParentNodeType = LTDT_ListMember) then
  begin
    NodeData.NodeType := LTDT_Object;

    NodeData.PSourceObject := myx_grt_list_item_get(
      myx_grt_dict_item_get_value(PParentSourceObject,
        ParentMemberName), Node.Index);

    NodeData.SourceCaption := UTF8Decode(
      _myx_grt_dict_name_item_as_string(NodeData.PSourceObject));

    NodeData.StructName := myx_grt_dict_struct_get_name(
      NodeData.PSourceObject);

    NodeData.PStruct := myx_grt_struct_get(Grt.NativeGrt, NodeData.StructName);

    //Special handling for columns
    if (WideSameText(NodeData.StructName, 'db.Column')) or
      (myx_grt_struct_inherits_from(Grt.NativeGrt, NodeData.StructName,
      'db.Column') = 1) then
    begin
      NodeData.SourceCaption := NodeData.SourceCaption + ' '+
        myx_grt_dict_item_get_as_string(
          NodeData.PSourceObject, 'datatypeName');
    end;


    // Seatch log entries
    NodeData.ContainsError := False;
    NodeData.ContainsWarning := False;
    NodeData.PLogEntry := nil;
    NodeData.PMessages := nil;

    SourceObjectId := _myx_grt_dict_item_get_as_string(
      NodeData.PSourceObject, '_id');

    PLog := Grt.Global['/migration/creationLog'];
    for I := 0 to myx_grt_list_item_count(PLog)-1 do
    begin
      PLogEntry := myx_grt_list_item_get(PLog, I);

      LogSourceObjectId := myx_grt_dict_item_get_as_string(
          PLogEntry, 'logObject');

      if (WideSameText(SourceObjectId, LogSourceObjectId)) then
      begin
        NodeData.PLogEntry := PLogEntry;

        // Check if there is an error in the message list
        NodeData.PMessages := myx_grt_dict_item_get_value(
          PLogEntry, 'entries');

        for J := 0 to myx_grt_list_item_count(NodeData.PMessages)-1 do
        begin
          PMessage := myx_grt_list_item_get(NodeData.PMessages, J);
          if (myx_grt_dict_item_get_as_int(
            PMessage, 'entryType') = 2) then
          begin
            NodeData.ContainsError := True;
            break;
          end
          else
            if (myx_grt_dict_item_get_as_int(
              PMessage, 'entryType') = 1) then
              NodeData.ContainsWarning := True;
        end;

        // See if there is a target object
        NodeData.PTargetObject :=
          myx_grt_dict_item_get_reference_value(
            Grt.NativeGrt, PLogEntry, 'refObject');

        if (NodeData.PTargetObject <> nil) then
        begin
          NodeData.TargetCaption :=
            UTF8Decode(_myx_grt_dict_name_item_as_string(
              NodeData.PTargetObject));

          NodeData.TargetStructName :=
            UTF8Decode(_myx_grt_dict_struct_get_name(
              NodeData.PTargetObject));

          //Special handling for columns
          if (WideSameText(NodeData.TargetStructName, 'db.Column')) or
            (myx_grt_struct_inherits_from(Grt.NativeGrt, NodeData.TargetStructName,
            'db.Column') = 1) then
          begin
            NodeData.TargetCaption := NodeData.TargetCaption + ' '+
              UTF8Decode(_myx_grt_dict_item_get_as_string(
                NodeData.PTargetObject, 'datatypeName'));
          end;
        end;

        break;
      end;
    end;

    if (GetListMemberCount(Grt.NativeGrt, NodeData.StructName, True) > 0) then
      include(InitialStates, ivsHasChildren);
  end
  else
    if (ParentNodeData.NodeType = LTDT_Object) then
    begin
      NodeData.NodeType := LTDT_ListMember;

      NodeData.PSourceObject := ParentNodeData.PSourceObject;

      NodeData.StructName := ParentNodeData.StructName;

      NodeData.PMember := GetListMember(Grt.NativeGrt, ParentNodeData.StructName, Node.Index);

      NodeData.MemberName := UTF8Decode(
        _myx_grt_struct_get_member_name(NodeData.PMember));


      NodeData.Value := myx_grt_dict_item_get_value(
        NodeData.PSourceObject, NodeData.MemberName);

      // try to find member caption
      NodeData.SourceCaption := UTF8Decode(
        _myx_grt_struct_get_member_caption(Grt.NativeGrt,
          ParentNodeData.PStruct,
          PChar(UTF8Encode(NodeData.MemberName)), 1));

      // use MemberName is caption is not found
      if (NodeData.SourceCaption = '') then
        NodeData.SourceCaption := NodeData.MemberName;

      if (myx_grt_list_item_count(NodeData.Value) > 0) then
        include(InitialStates, ivsHasChildren);
    end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var HitInfo: THitInfo;
  TxtRect: TRect;
  xpos: integer;

begin
  if(Sender.InheritsFrom(TBaseVirtualTree))then
  begin
    TBaseVirtualTree(Sender).GetHitTestInfoAt(X, Y, True, HitInfo);

    if(HitInfo.HitNode<>nil)then
    begin
      TxtRect:=TBaseVirtualTree(Sender).GetDisplayRect(
        HitInfo.HitNode, -1, True);

      xpos:=TxtRect.Left-TBaseVirtualTree(Sender).OffsetX;

      if(X>xpos-16-4)and(X<xpos+2)and
        ((HitInfo.HitNode.ChildCount>0)or(vsHasChildren in HitInfo.HitNode.States))then
      begin
        TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]:=
          Not(TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]);
      end;
    end
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.MigLogVTPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

var
  NodeData: PLogTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType = LTDT_ListMember) then
    TargetCanvas.Font.Color := clGray
  else
    if Sender.FocusedNode <> Node then
      TargetCanvas.Font.Color := clBlack;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.LogFilterComboBoxCloseUp(
  Sender: TObject);

begin
  RefreshLog;

  Application.ProcessMessages;

  MigLogVT.OffsetY := 0;
end;

// -----------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.RefreshLog;

begin
  MigLogVT.BeginUpdate;
  try
    MigLogVT.Clear;

    MigLogVT.RootNodeCount := Grt.ListCount(
      Grt.Global['/migration/targetCatalog/schemata']);


    // Filter only warnings and errors
    if (LogFilterComboBox.ItemIndex = 0) then
    begin
      HideLogNodeIfNoErrorsWarnings(MigLogVT.GetFirst);

      if (MigLogVT.VisibleCount = 0) then
      begin
        NoMapProblemsPnl.Left := MigLogVT.Left;
        NoMapProblemsPnl.Top := MigLogVT.Top;
        NoMapProblemsPnl.Width := MigLogVT.Width;
        NoMapProblemsPnl.Height := MigLogVT.Height;

        NoMapProblemsPnl.Visible := True;
      end;
    end
    else
    begin
      NoMapProblemsPnl.Visible := False;
    end;
  finally
    MigLogVT.EndUpdate;
  end;

  MainPngControl.ActivePage := CreateResultsTabSheet;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.SqlUCEChange(Sender: TObject; Line: TUCELine);

begin
  if (Not(FInitSQLEdit)) then
  begin
    ApplyChangesBtn.Enabled := SqlUCE.Content.Modified;
    DiscardChangesBtn.Enabled := SqlUCE.Content.Modified;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMigrationCreateObjsForm.MigLogGetCellText(
  Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex;
  TextType: TVSTTextType): WideString;

var
  NodeData: PLogTreeData;
  PMessage: Pointer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Column = 0) or
    ((Column = 2) and (NodeData.NodeType = LTDT_ListMember)) then
    Result := NodeData.SourceCaption
  else
    if (Column = 1) and (NodeData.NodeType = LTDT_Object) then
    begin
      if (NodeData.PMessages<>nil) and
        (myx_grt_list_item_count(NodeData.PMessages) > 0) then
      begin
        PMessage := myx_grt_list_item_get(NodeData.PMessages, 0);

        Result := UTF8Decode(_myx_grt_dict_item_get_as_string(
          PMessage, 'name'));
      end
      else
        Result := _('Object created successfully.');

    end
    else
      if (Column = 2) then
        Result := NodeData.TargetCaption;
end;

// -----------------------------------------------------------------------------

function TMigrationCreateObjsForm.HideLogNodeIfNoErrorsWarnings(
  Node: PVirtualNode): Boolean;

var
  NextNode: PVirtualNode;
  NodeData: PLogTreeData;

begin
  Result := True;

  NodeData := MigLogVT.GetNodeData(Node);

  if (NodeData.ContainsWarning) or (NodeData.ContainsError) then
    Result := False;

  // check all children
  if (Node.ChildCount>0) then
  begin
    NextNode := MigLogVT.GetFirstChild(Node);
    if (NextNode <> nil) then
      if (Not(HideLogNodeIfNoErrorsWarnings(NextNode))) then
        Result := False;
  end;

  if (Result) then
    MigLogVT.IsVisible[Node] := False;

  NextNode := MigLogVT.GetNextSibling(Node);
  if (NextNode <> nil) and
    (Not(HideLogNodeIfNoErrorsWarnings(NextNode))) then
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DragPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (Button = mbLeft) then
  begin
    FOldOptionPnlHeight := LogOptionPnl.Height;
    FMouseY := Mouse.CursorPos.Y;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DragPnlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  
begin
  if (ssLeft in Shift) then
  begin
    LogOptionPnl.Height := FOldOptionPnlHeight -
      (Mouse.CursorPos.Y - FMouseY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.RecreateObjectsBtnClick(Sender: TObject);
begin
  SetSubPageIndex(0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.ApplyChangesBtnClick(Sender: TObject);

var
  NodeData: PLogTreeData;

begin
  if (MigLogVT.FocusedNode <> nil) then
  begin
    NodeData := MigLogVT.GetNodeData(MigLogVT.FocusedNode);

    if (NodeData<>nil) and
      (NodeData.NodeType = LTDT_Object) then
    begin
      myx_grt_dict_item_set_value_from_string(
        NodeData.PSourceObject, 'sql', SqlUCE.Text);

      Grt.DictInt[NodeData.PSourceObject, 'commentedOut'] :=
        Ord(CommentOutCBox.Checked);

      ApplyChangesBtn.Enabled := False;
      DiscardChangesBtn.Enabled := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.DiscardChangesBtnClick(Sender: TObject);

begin
  if (MigLogVT.FocusedNode <> nil) then
    MigLogVTChange(MigLogVT, MigLogVT.FocusedNode);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.CommentOutCBoxClick(Sender: TObject);

var
  NodeData: PLogTreeData;
  ModuleName: WideString;
  Task: IGrtTask;
  OrigState: Integer;

begin
  if (MigLogVT.FocusedNode <> nil) and
    (Not(FInitSQLEdit)) then
  begin
    NodeData := MigLogVT.GetNodeData(MigLogVT.FocusedNode);

    if (NodeData<>nil) and
      (NodeData.NodeType = LTDT_Object) then
    begin
      OrigState :=
        Grt.DictInt[NodeData.PSourceObject, 'commentedOut'];

      Grt.DictInt[NodeData.PSourceObject, 'commentedOut'] :=
        Ord(CommentOutCBox.Checked);


      //Generate SQL create statements
      try
        ModuleName := Grt.DictString[
            Grt.DictItem[
              Grt.Global['/migration/targetConnection'], 'modules'],
          'TransformationModule'];

        Task := Grt.CreateStandardTask(
          _('Getting SQL script'), 
          ModuleName,
          'getSqlCreate',
          [NodeData.PSourceObject],
          ProcessGrtOutput,
          ProcessGrtMessages,
          True);
        Grt.AddTaskAndWait(Task, True);

        SqlUCE.Text := Grt.ValueString[Task.Result];
      except
        on x: EGrtError do
        begin
          DoProgressError(Format(
              _('The SQL create statement could not be created '+
                '(error: %d).'+#13#10+
                '%s'),
              [x.ErrorNumber, x.Description]),
            nil);

          Exit;
        end;
      end;

      Grt.DictInt[NodeData.PSourceObject, 'commentedOut'] :=
        OrigState;
    end;
  end;

  SqlUCE.Enabled := Not(CommentOutCBox.Checked);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationCreateObjsForm.CheckMatchingSourceTarget;

begin
  if (CompareText(Grt.GlobalAsString['/migration/sourceConnection/parameterValues/host'],
      Grt.GlobalAsString['/migration/targetConnection/parameterValues/host']) = 0)
    and (CompareText(Grt.GlobalAsString['/migration/sourceConnection/parameterValues/port'],
      Grt.GlobalAsString['/migration/targetConnection/parameterValues/port']) = 0)
    and (CompareText(Grt.GlobalAsString['/migration/sourceCatalog/schemata/0/name'],
      Grt.GlobalAsString['/migration/targetCatalog/schemata/0/name']) = 0) then
    ShowModalDialog(_('Warning'), _('The source and target schema are on the same '
      + 'host and use the same schema name. This is not a valid setup and will lead '
      + 'lead to data loss. Please rename the target schema or choose a different '
      + 'target host.'), myx_mtWarning);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
