unit MigrationObjManEdit;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees,
  MigrationObjMapDetail, TntSysUtils,
  UCEHighlighter, UCESQLHighlighter, UniCodeEditor,
  PngTools, WizardInterface, WizardPage,
  Grt;

type
  TMigrationObjManEditForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    ObjMapEditTabSheet: TTntTabSheet;
    MigOptsTabSheet: TTntTabSheet;
    MigGBox: TTntGroupBox;
    GenerateSQLCreateStmtsImg: TTntImage;
    MigTaskHeaderLbl: TTntLabel;
    GenSQLLbl: TTntLabel;
    MigTaskLbl: TTntLabel;
    MigrateImg: TTntImage;
    MigExecLbl: TTntLabel;
    MessageLogGBox: TTntGroupBox;
    GRTMessageMemo: TTntMemo;
    LogMainPnl: TTntPanel;
    LogHeaderLbl: TTntLabel;
    MigLogVT: TVirtualStringTree;
    LogFilterLbl: TTntLabel;
    LogFilterComboBox: TTntComboBox;
    LogOptionPnl: TTntPanel;
    LogOptionsGBox: TTntGroupBox;
    TreeImageList: TImageList;
    DragPnl: TTntPanel;
    SqlUCE: TUniCodeEdit;
    UCESQLHighlighter: TUCESQLHighlighter;
    ApplyChangesBtn: TTntButton;
    DiscardChangesBtn: TTntButton;
    GrtMessageLbl: TTntLabel;
    GrtProgressBar: TTntProgressBar;
    GrtProgressLbl: TTntLabel;
    ResultLbl: TTntLabel;
    NoMapProblemsPnl: TTntPanel;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    TntShape1: TTntShape;
    TntImage1: TTntImage;
    CommentOutCBox: TTntCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DoMigrationTasks;
    procedure DockPnlResize(Sender: TObject);
    procedure DoProgressError(
      msg: WideString; TaskImg: TTntImage);
    procedure RefreshLog;
    procedure MigLogVTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MigLogVTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure MigLogVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MigLogVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    function MigLogGetCellText(
      Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType): WideString;
    procedure MigLogVTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure MigLogVTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MigLogVTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure MigLogVTBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    function HideLogNodeIfNoErrorsWarnings(
      Node: PVirtualNode): Boolean;
    procedure LogFilterComboBoxCloseUp(Sender: TObject);
    procedure MigLogVTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure DragPnlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragPnlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MigLogVTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure SqlUCEChange(Sender: TObject; Line: TUCELine);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DiscardChangesBtnClick(Sender: TObject);

    procedure ProcessGrtOutput(S: WideString);
    procedure ProcessGrtMessages(Messages: TMYX_GRT_MSGS);
    function ProcessGrtStatusQuery: Integer;
    procedure MigLogVTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure MigLogVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
    procedure MigLogVTFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure CommentOutCBoxClick(Sender: TObject);
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
  MigrationObjMapForm: TMigrationObjManEditForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel := DockPnl;

  MainPngControl.Align := alNone;
  MainPngControl.Left := -4;
  MainPngControl.Top := -27;
  MainPngControl.Width := 765+4+4;
  MainPngControl.Height := 529+274;
  MainPngControl.ActivePageIndex := 0;

  TaskUncheckedPNGImg := LoadPNGImageFromResource('task_unchecked');
  TaskCheckedPNGImg := LoadPNGImageFromResource('task_checked');
  TaskErrorPNGImg := LoadPNGImageFromResource('task_error');
  TaskDisabledPNGImg := LoadPNGImageFromResource('task_disabled');

  FTreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  FTreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  FGrtValueListPNGImg := LoadPNGImageFromResource('grt_value_list');

  FLogError := LoadPNGImageFromResource('log_error_16x16');
  FLogErrorSmall := LoadPNGImageFromResource('log_error_12x12');
  FLogWarning := LoadPNGImageFromResource('log_warning_16x16');
  FLogWarningSmall := LoadPNGImageFromResource('log_warning_12x12');

  FIconList := TTntStringList.Create;

  MigLogVT.NodeDataSize := SizeOf(TLogTreeData);

  LogFilterComboBox.ItemIndex := 0;

  FInitSQLEdit := False;

  SqlUCE.Content.OnChangeLine := SqlUCEChange;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.FormDestroy(Sender: TObject);

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

procedure TMigrationObjManEditForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
  begin
    // Set script checkpoint
    Grt.GlobalAsInt['/migration/applicationData/' +
      'ScriptGenerationCheckpoint'] := 4;

    if (MainPngControl.ActivePageIndex <> 1) then
      DoMigrationTasks
    else
      MainPngControl.ActivePageIndex := 0;
  end
  else
    if (NewSubPageIndex=1) then
      RefreshLog;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetSubPageIndex: Integer;

begin
  Result := MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Migration');
    1:
      Result:=_('Manual Editing');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('In this step the selected object will be migrated.');
    1:
      Result:=_('Check the list of migrated objects.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := True;
    1:
      Result := True;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    0:
      MessageLogGBox.Visible := State;
    1:
      LogOptionPnl.Visible := State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := MessageLogGBox.Visible;
    1:
      Result := LogOptionPnl.Visible;
  else
    Result := False;
  end;
end;


// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.BeforeSubPageIndexChange(SectionIndex: Integer);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------
// ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.DoMigrationTasks;

var
  ModuleName,
  TargetRdbmsGlobalPath: WideString;
  TargetRdbms,
  RdbmsList: Pointer;
  I: Integer;

  Task: IGrtTask;
  
begin
  MigrateImg.Picture.Assign(TaskUncheckedPNGImg);
  GenerateSQLCreateStmtsImg.Picture.Assign(TaskUncheckedPNGImg);

  // initialize progress widgets
  ResultLbl.Caption := _('Executing ...');
  ResultLbl.Show;
  ResultLbl.Font.Color := clBlack;
  GrtMessageLbl.Caption := '';
  GrtMessageLbl.Show;
  GRTMessageMemo.Text := '';
  GrtProgressBar.Visible := False;
  GrtProgressLbl.Visible := False;

  MainPngControl.ActivePage := MigOptsTabSheet;

  //Start animation
  DoProcessStart;
  try
    Application.ProcessMessages;

    myx_grt_list_clear(Grt.Global['/migration/migrationLog']);

    //Do migration
    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/sourceConnection'], 'modules'],
        'MigrationModule'];

      TargetRdbms := Grt.DictRef[
        Grt.DictRef[
          Grt.Global['/migration/targetConnection'], 'driver'],
        'owner'];

      TargetRdbmsGlobalPath := '';
      RdbmsList := Grt.Global['/rdbmsMgmt/rdbms'];
      for I := 0 to Grt.ListCount(RdbmsList) do
        if (Grt.ListItem[RdbmsList, I] = TargetRdbms) then
        begin
          TargetRdbmsGlobalPath := '/rdbmsMgmt/rdbms/' + IntToStr(I);
          break;
        end;

      Task := Grt.CreateStandardTask(
        _('Starting migration'),
        ModuleName,
        'migrate',
        [Grt.GetGlobalAsParam('/migration'),
          Grt.GetGlobalAsParam(TargetRdbmsGlobalPath),
          Grt.GetGlobalAsParam('/migration/targetVersion')],
        ProcessGrtOutput,
        ProcessGrtMessages,
        True,
        False,
        -1,
        nil,
        ProcessGrtStatusQuery);
      Grt.AddTaskAndWaitWithMessages(Task, True);
    except
      on x: EGrtError do
      begin
        DoProgressError(Format(
            _('The source object cannot be migrated '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          MigrateImg);

        Exit;
      end;
    end;

    MigrateImg.Picture.Assign(TaskCheckedPNGImg);
    Application.ProcessMessages;
    
    //Generate SQL create statements
    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/targetConnection'], 'modules'],
        'TransformationModule'];

      Task := Grt.CreateStandardTask(
        _('Generating SQL script'),
        ModuleName,
        'generateSqlCreateStatements',
        [Grt.GetGlobalAsParam('/migration/targetCatalog'),
          Grt.Global['/migration/objectCreationParams']],
        ProcessGrtOutput,
        ProcessGrtMessages,
        True);
      Grt.AddTaskAndWait(Task, True);
    except
      on x: EGrtError do
      begin
        DoProgressError(Format(
            _('The SQL create statements could not be created '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          GenerateSQLCreateStmtsImg);

        Exit;
      end;
    end;

    GenerateSQLCreateStmtsImg.Picture.Assign(TaskCheckedPNGImg);

    if (FWizardInterface.OperationCanceled) then
      ResultLbl.Caption := _('Canceled by user.')
    else
      ResultLbl.Caption := _('Execution completed successfully.');

    GrtMessageLbl.Hide;
    GrtProgressBar.Hide;
    GrtProgressLbl.Hide;
  finally
    DoProcessEnd;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.ProcessGrtOutput(S: WideString);

begin
  GRTMessageMemo.Text := GRTMessageMemo.Text + S;
  GRTMessageMemo.Update;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.ProcessGrtMessages(
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

    GRTMessageMemo.Text := GRTMessageMemo.Text +
      FormatGrtMessagesAsString(Messages);
    GRTMessageMemo.Refresh;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.ProcessGrtStatusQuery: Integer;

begin
  Result := Ord(FWizardInterface.OperationCanceled);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.DoProgressError(
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

// -----------------------------------------------------------------------------

{procedure TMigrationObjManEditForm.StoreSQLScriptBtnClick(Sender: TObject);

var
  SaveDlg: TExOpenDialog;
  FileName: WideString;
  PSql: Pointer;
  F: TextFile;

begin
  SaveDlg := TExOpenDialog.Create(self);
  try
    SaveDlg.Title := 'Store SQL Script';
    SaveDlg.OpenButton.Caption := 'Save';

    SaveDlg.Filter := _('SQL Script File')+
      ' (*.sql)|*.sql|'+
      _('All files')+' (*.*)|*.*';

    if (SaveDlg.Execute) then
    begin
      FileName := SaveDlg.Filename;

      if (Pos('.', FileName)=0) then
        FileName := FileName + '.sql';

      //Generate SQL create script
      try
        PSql := CallGrtModuleMethod(Mig.NativeGrt,
          Mig.TargetDBConn.JdbcDriver.TransformationModule,
          'getSqlCreateScript',
          BuildGrtParamList(['global::/migration/targetCatalog']),
          GRTMessageMemo.Lines, True, True);
      except
        on x: EGrtError do
        begin
          DoProgressError(Format(
              _('The SQL create statements could not be created '+
                '(error: %d).'+#13#10+
                '%s'),
              [x.ErrorNumber, x.Description]),
            GenerateSQLCreateStmtsImg);

          Exit;
        end;
      end;

      AssignFile(F, FileName);
      Rewrite(F);
      Write(F, _myx_grt_value_as_string(PSql));
      CloseFile(F);
    end;
  finally
    SaveDlg.Free;
  end;
end;}

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.RefreshLog;

begin
  MigLogVT.BeginUpdate;
  try
    MigLogVT.Clear;

    {MigLogVT.RootNodeCount := myx_grt_list_item_count(
      myx_grt_dict_item_get_value(Mig.SourceCatalog, 'schemata'));}

    MigLogVT.RootNodeCount := myx_grt_list_item_count(
      Grt.Global['/migration/sourceCatalog/schemata']);


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

      // Hide Migration Message
      if (LogFilterComboBox.ItemIndex = 1) then
      begin
        MigLogVT.Header.Columns[1].Options :=
          MigLogVT.Header.Columns[1].Options - [coVisible];
        MigLogVT.Header.Columns[0].Width := 400;
        MigLogVT.Header.Columns[2].Width := 300;
      end
      else
      begin
        MigLogVT.Header.Columns[1].Options :=
          MigLogVT.Header.Columns[1].Options + [coVisible];
        MigLogVT.Header.Columns[0].Width := 200;
        MigLogVT.Header.Columns[2].Width := 160;
      end;
    end;
  finally
    MigLogVT.EndUpdate;
  end;

  MainPngControl.ActivePage := ObjMapEditTabSheet;
end;

// -----------------------------------------------------------------------------

function TMigrationObjManEditForm.HideLogNodeIfNoErrorsWarnings(
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

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTInitNode(
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
      Grt.Global['/migration/sourceCatalog'];
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

    NodeData.StructName := UTF8Decode(_myx_grt_dict_struct_get_name(
      NodeData.PSourceObject));

    NodeData.PStruct := myx_grt_struct_get(Grt.NativeGrt, NodeData.StructName);

    //Special handling for columns
    if (WideSameText(NodeData.StructName, 'db.Column')) or
      (myx_grt_struct_inherits_from(Grt.NativeGrt, NodeData.StructName,
      'db.Column') = 1) then
    begin
      NodeData.SourceCaption := NodeData.SourceCaption + ' '+
        UTF8Decode(_myx_grt_dict_item_get_as_string(
          NodeData.PSourceObject, 'datatypeName'));
    end;


    // Seatch log entries
    NodeData.ContainsError := False;
    NodeData.ContainsWarning := False;
    NodeData.PLogEntry := nil;
    NodeData.PMessages := nil;

    SourceObjectId := _myx_grt_dict_item_get_as_string(
      NodeData.PSourceObject, '_id');

    PLog := Grt.Global['/migration/migrationLog'];
    for I := 0 to myx_grt_list_item_count(PLog)-1 do
    begin
      PLogEntry := myx_grt_list_item_get(PLog, I);

      LogSourceObjectId := UTF8Decode(
        _myx_grt_dict_item_get_as_string(
          PLogEntry, 'logObject'));

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

procedure TMigrationObjManEditForm.MigLogVTInitChildren(
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

function TMigrationObjManEditForm.MigLogGetCellText(
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
        (Grt.ListCount(NodeData.PMessages) > 0) then
      begin
        PMessage := Grt.ListItem[NodeData.PMessages, 0];

        Result := Grt.DictString[PMessage, 'name'];
      end
      else
        if (Grt.ListCount(NodeData.PMessages) = 0) then
          Result := _('Object migrated successfully.');

    end
    else
      if (Column = 2) then
        Result := NodeData.TargetCaption;

end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

begin
  CellText := MigLogGetCellText(Sender, Node, Column, TextType);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTGetHint(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

begin
  HintText := MigLogGetCellText(Sender, Node, Column, ttNormal);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTAfterCellPaint(
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

procedure TMigrationObjManEditForm.MigLogVTMouseDown(Sender: TObject;
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

procedure TMigrationObjManEditForm.MigLogVTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if (Column = 0) or (Column = 2) then
    ImageIndex := 1;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTBeforeItemErase(
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

procedure TMigrationObjManEditForm.LogFilterComboBoxCloseUp(
  Sender: TObject);
  
begin
  RefreshLog;

  Application.ProcessMessages;

  MigLogVT.OffsetY := 0;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTChange(Sender: TBaseVirtualTree;
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
        (NodeData.PTargetObject <> nil) then
      begin
        SqlUCE.Text := TntAdjustLineBreaks(
          UTF8Decode(_myx_grt_dict_item_get_as_string(
            NodeData.PTargetObject, 'sql')));

        CommentOutCBox.Checked :=
          (Grt.DictInt[NodeData.PTargetObject, 'commentedOut'] = 1);

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

procedure TMigrationObjManEditForm.DragPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (Button = mbLeft) then
  begin
    FOldOptionPnlHeight := LogOptionPnl.Height;
    FMouseY := Mouse.CursorPos.Y;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjManEditForm.DragPnlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

begin
  if (ssLeft in Shift) then
  begin
    LogOptionPnl.Height := FOldOptionPnlHeight -
      (Mouse.CursorPos.Y - FMouseY);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
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

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.SqlUCEChange(Sender: TObject; Line: TUCELine);

begin
  if (Not(FInitSQLEdit)) then
  begin
    ApplyChangesBtn.Enabled := SqlUCE.Content.Modified;
    DiscardChangesBtn.Enabled := SqlUCE.Content.Modified;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.ApplyChangesBtnClick(Sender: TObject);

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
        NodeData.PTargetObject, 'sql', SqlUCE.Text);

      Grt.DictInt[NodeData.PTargetObject, 'commentedOut'] :=
        Ord(CommentOutCBox.Checked);

      ApplyChangesBtn.Enabled := False;
      DiscardChangesBtn.Enabled := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.DiscardChangesBtnClick(Sender: TObject);

begin
  if (MigLogVT.FocusedNode <> nil) then
    MigLogVTChange(MigLogVT, MigLogVT.FocusedNode);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTEditing(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);

begin
  Allowed := (Column = 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTNewText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  NewText: WideString);

var
  NodeData: PLogTreeData;
  ModuleName: WideString;
  Task: IGrtTask;

begin
  NodeData := MigLogVT.GetNodeData(Node);

  if (myx_grt_struct_inherits_from(Grt.NativeGrt,
    NodeData^.TargetStructName, 'base.NamedGrtObject') = 1) then
  begin
    myx_grt_dict_item_set_value_from_string(NodeData^.PTargetObject,
      'name', NewText);

    NodeData.TargetCaption := NewText;

    MigLogVT.InvalidateNode(Node);

    //Generate SQL create statements
    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/targetConnection'], 'modules'],
        'TransformationModule'];

      Task := Grt.CreateStandardTask(
        _('Generating SQL script'),
        ModuleName,
        'generateSqlCreateStatements',
        [Grt.GetGlobalAsParam('/migration/targetCatalog'),
          Grt.Global['/migration/objectCreationParams']],
        ProcessGrtOutput,
        ProcessGrtMessages,
        True);
      Grt.AddTaskAndWait(Task, True);
    except
      on x: EGrtError do
      begin
        DoProgressError(Format(
            _('The SQL create statements could not be created '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          GenerateSQLCreateStmtsImg);

        Exit;
      end;
    end;

    MigLogVTChange(Sender, Node);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.MigLogVTFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);

var
  NodeData: PLogTreeData;

begin
  NodeData := MigLogVT.GetNodeData(Node);

  Finalize(NodeData^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMigrationObjManEditForm.CommentOutCBoxClick(Sender: TObject);

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
        Grt.DictInt[NodeData.PTargetObject, 'commentedOut'];

      Grt.DictInt[NodeData.PTargetObject, 'commentedOut'] :=
        Ord(CommentOutCBox.Checked);


      //Generate SQL create statements
      try
        ModuleName := Grt.DictString[
            Grt.DictItem[
              Grt.Global['/migration/targetConnection'], 'modules'],
          'TransformationModule'];

        Task := Grt.CreateStandardTask(
          _('Getting SQL create statement'), 
          ModuleName,
          'getSqlCreate',
          [NodeData.PTargetObject],
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
            GenerateSQLCreateStmtsImg);

          Exit;
        end;
      end;

      Grt.DictInt[NodeData.PTargetObject, 'commentedOut'] :=
        OrigState;
    end;
  end;

  SqlUCE.Enabled := Not(CommentOutCBox.Checked);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
