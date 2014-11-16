unit Main;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Menus, ShellApi, TntSysUtils, TntSystem,
  ActnList, AppInstanceMgmt,
  TntMenus, ComCtrls, TntComCtrls, TntForms, ExtCtrls, AuxFuncs,
  TntExtCtrls, Sections, ExecutionPlan, WizardHeader, WizardBottom,
  JdbcDBConns, XGrtShell, MigrationObjSel, StdCtrls,
  MigrationObjSelFilter, MigrationObjMap, MigrationObjManEdit,
  MigrationCreateObjs, MigrationDataMap, MigrationBulkTransfer,
  MigrationReport, CommonFuncs, PngTools,
  Grt, WizardInterface, TntClasses,
  MyxConnectionDialog, MyxBaseForm, MyxOptions;

{$include Consts.ini}

type
  TMainForm = class(TMyxBaseForm, IWizardInterface)
    MainMenu: TTntMainMenu;
    FileMI: TTntMenuItem;
    EditMI: TTntMenuItem;
    ToolsMI: TTntMenuItem;
    HelpMI: TTntMenuItem;
    StatusBar: TTntStatusBar;
    SplitterPnl: TTntPanel;
    SplitterImg: TTntImage;
    LeftPnl: TTntPanel;
    TntPanel2: TTntPanel;
    TntPanel3: TTntPanel;
    TntPanel4: TTntPanel;
    MigrationPlanPnl: TTntPanel;
    ExitMI: TTntMenuItem;
    N1: TTntMenuItem;
    StoreApplicationSnapshotMI: TTntMenuItem;
    N2: TTntMenuItem;
    ObjectShellMI: TTntMenuItem;
    CopyMI: TTntMenuItem;
    CutMI: TTntMenuItem;
    PasteMI: TTntMenuItem;
    AboutMI: TTntMenuItem;
    LoadApplicationSnapshotMI: TTntMenuItem;
    N3: TTntMenuItem;
    OnlineHelpMI: TTntMenuItem;
    N4: TTntMenuItem;
    GenerateMigrationScriptMI: TTntMenuItem;
    WindowMI: TTntMenuItem;
    N01: TTntMenuItem;
    N5: TTntMenuItem;
    ListReportedBugs: TTntMenuItem;
    ReportBugMI: TTntMenuItem;
    procedure TntFormCreate(Sender: TObject);

    procedure BuildSectionTree;
    procedure DoCurrentSectionChanging(Sender: TObject);
    procedure DoCurrentSectionChanged(Sender: TObject);
    procedure ShowSection(SectionNr: integer);

    function CreateSectionForm(AOwner: TComponent;
      SidebarSectionType: integer): TSectionForm;

    function AddWizardHeader(SectionPanel: TSectionPanel): TWizardHeaderFrame;
    function AddWizardBottom(SectionPanel: TSectionPanel): TWizardBottomFrame;

    procedure DoProcessStart(Sender: TObject);
    procedure DoProcessEnd(Sender: TObject);


    procedure NextBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);

    procedure EnableDisableBottomBtns;
    procedure TntFormDestroy(Sender: TObject);
    procedure ObjectShellMIClick(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TntFormResize(Sender: TObject);

    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    procedure CheckInputFile(FileName: WideString);
    procedure StoreApplicationSnapshotMIClick(Sender: TObject);
    procedure LoadApplicationSnapshotMIClick(Sender: TObject);

    procedure RestoreSnapshot(FileName: WideString);
    procedure AboutMIClick(Sender: TObject);
    procedure ExitMIClick(Sender: TObject);
    procedure OnlineHelpMIClick(Sender: TObject);
    procedure GenerateMigrationScriptMIClick(Sender: TObject);

    procedure WMInitMenuPopup(var MSG: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure ListReportedBugsClick(Sender: TObject);
    procedure ReportBugMIClick(Sender: TObject);
    procedure SplitterImgClick(Sender: TObject);
  protected
    function GetNextBtn: TButton;
    function GetBackBtn: TButton;
    function GetDetailsBtn: TButton;

    function GetDetailsActivated: Boolean;
    procedure SetDetailsActivated(Activated: Boolean);

    function GetOperationCanceled: Boolean;
    procedure SetOperationCanceled(Canceled: Boolean);

    procedure ProcessTextOutput(Text: WideString);

    procedure GrtShutDown(Grt: TGrt);

    procedure FillGrtScriptVariables(Params: TTntStringList;
      Checkpoint: Integer);

    function InitializeGrt: Boolean;

    procedure RunScript;
  private
    { Private declarations }
    SectionPnl: TSectionPanel;
    ExecutionPlan: TExecutionPlan;

    SectionControls: TSectionControls;
    CurrentSectionIndex: Integer;

    FXGrtShellForm: TXGrtShellForm;

    FOperationCanceled: Boolean;
    FBackBtnEnabled: Boolean;
    FNextBtnEnabled: Boolean;

    FGrtInitialized: Boolean;
  public
    { Public declarations }
    WizardHeaderFrame: TWizardHeaderFrame;
    WizardBottomFrame: TWizardBottomFrame;

    property GrtInitialized: Boolean read FGrtInitialized;

    procedure InitializeGui;

    property NextBtn: TButton read GetNextBtn;
    property BackBtn: TButton read GetBackBtn;
    property DetailsBtn: TButton read GetDetailsBtn;
    property DetailsActivated: Boolean read GetDetailsActivated write SetDetailsActivated;

    property OperationCanceled: Boolean read GetOperationCanceled write SetOperationCanceled;
  end;

const
    SSTMT_SourceTargetDB = 1;
    SSTMT_ObjSelection = 2;
    SSTMT_ObjMapping = 3;
    SSTMT_ManualEditing = 4;
    SSTMT_SchemaCreation = 5;
    SSTMT_DataMapping = 6;
    SSTMT_BulkTransfer = 7;
    SSTMT_Summary = 8;

{procedure BeforeGRTRefresh(user_data: Pointer); cdecl;
procedure AfterGRTRefresh(user_data: Pointer); cdecl;}

var
  MainForm: TMainForm;

implementation

uses
  MigrationSourceTarget;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMainForm.TntFormCreate(Sender: TObject);

begin
  FGrtInitialized := False;
  FXGrtShellForm := nil;

  DragAcceptFiles(Handle, True);

  CurrentSectionIndex:=0;

  FGrtInitialized := InitializeGrt;

  if GrtInitialized then
  begin
    ExecutionPlan:=TExecutionPlan.Create(self);
    ExecutionPlan.Parent:=MigrationPlanPnl;
    ExecutionPlan.Align:=alClient;

    SectionPnl:=TSectionPanel.Create(self);
    SectionPnl.Parent:=self;
    SectionPnl.Align:=alClient;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.TntFormDestroy(Sender: TObject);

begin
  FXGrtShellForm.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.InitializeGui;

begin
  //Initialize Migration Main Class
  SectionControls:=TSectionControls.Create(self,
    CreateSectionForm,
    nil,
    True,
    0,
    nil,
    8, //Section Count
    False,
    True);

  WizardHeaderFrame:=AddWizardHeader(SectionControls.DockPnl);
  WizardBottomFrame:=AddWizardBottom(SectionControls.DockPnl);

  ActiveControl := WizardBottomFrame.NextBtn;

  BuildSectionTree;
  ShowSection(0);

  // Once everything is initialized execute the given script.
  RunScript;
end;

// -----------------------------------------------------------------------------

function TMainForm.InitializeGrt: Boolean;

var
  Task: IGrtTask;

begin
  Result := True;


  // create workbench environment
  try
    Task := Grt.CreateStandardTask(_('Initializing migration'), 'Migration',
      'initMigration',
      [],
      ProcessTextOutput,
      nil,
      False, False);
    Grt.AddTaskAndWait(Task, True);

    if (Grt.ValueInt[Task.Result] = 0) then
      Result := False
    else
    begin
      Grt.OnShutDown := GrtShutDown;

      Grt.ValueRelease(Task.Result);

      FXGrtShellForm := TXGrtShellForm.Create(nil);
      Grt.Console := FXGrtShellForm.CommandLineUCE;
    end;
  except
    on X: Exception do
    begin
      Result := False;

      ShowModalDialog(_('Migration Toolkit Error'),
        Format(_('The GRT environment could not be initialized.' + #13#10 +
        'Please check your Java installation then try to restart ' +
        'the tool.' + #13#10#13#10 + 'Error details:' + #13#10 +
        '%s' + #13#10#13#10 +
        'Shell output: '+ #13#10 +
        Grt.OutputBuffer),
        [X.Message]), myx_mtError);
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.GrtShutDown(Grt: TGrt);

var
  Task: IGrtTask;
  
begin
  Task := Grt.CreateStandardTask(_('Shutting down migration'), 'Migration',
    'shutdownMigration',
    [],
    ProcessTextOutput,
    nil);
  Grt.AddTaskAndWait(Task, True);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.BuildSectionTree;

begin
  SectionControls.AddSection(_('Source/Target'),
    TSidebarSection.Create(SSTMT_SourceTargetDB,
    0, 0, False, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Source/Target'),
    _('Specify Source and Target Schema'), 0));

  SectionControls.AddSection(_('Object Selection'),
    TSidebarSection.Create(SSTMT_ObjSelection,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Object Selection'),
    _('Select all Objects which should be migrated'), 1));

  SectionControls.AddSection(_('Object Mapping'),
    TSidebarSection.Create(SSTMT_ObjMapping,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Object Mapping'),
    _('Define the Mapping Methods and Trans- formation Scripts'), 2));

  SectionControls.AddSection(_('Manual Editing'),
    TSidebarSection.Create(SSTMT_ManualEditing,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Manual Editing'),
    _('Manual Edit the generated Objects'), 3));

  SectionControls.AddSection(_('Schema Creation'),
    TSidebarSection.Create(SSTMT_SchemaCreation,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Schema Creation'),
    _('Execute DDL Script to create Target Schema'), 4));

  SectionControls.AddSection(_('Data Mapping'),
    TSidebarSection.Create(SSTMT_DataMapping,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Data Mapping'),
    _('Setup Data Trans- formations and Column Mappings'), 5));

  SectionControls.AddSection(_('Bulk Transfer'),
    TSidebarSection.Create(SSTMT_BulkTransfer,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Bulk Transfer'),
    _('Configure Server- side Bulk Data Transfer'), 6));

  SectionControls.AddSection(_('Summary'),
    TSidebarSection.Create(SSTMT_Summary,
    0, 0, True, False));

  ExecutionPlan.PlanTasks.Add(TPlanTask.Create(_('Summary'),
    _('Target Schema created and Data transfered'), 7));

  SectionControls.OnCurrentSectionChanged:=DoCurrentSectionChanged;
  SectionControls.OnCurrentSectionChanging:=DoCurrentSectionChanging;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.DoCurrentSectionChanging(Sender: TObject);

begin
  {if(SectionControls.CurrentSidebarSection.SidebarSectionType=SSTQB_QueryBrowser)then
    MainMenu.UnMerge(TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).Menu);}
end;

// -----------------------------------------------------------------------------

procedure TMainForm.DoCurrentSectionChanged(Sender: TObject);

begin
  {if(SectionControls.CurrentSidebarSection.SidebarSectionType=SSTQB_QueryBrowser)then
    MainMenu.Merge(TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).Menu);

  StatusBar.Invalidate;}
end;

// -----------------------------------------------------------------------------

procedure TMainForm.ShowSection(SectionNr: integer);

begin
  if(SectionNr<SectionControls.SectionList.Count)then
  begin
    SectionControls.ShowSidebarSection(SectionControls.SectionList[SectionNr] as TSidebarSection);
    ExecutionPlan.CurrentTask:=SectionNr;

    //Check state of Next/Back buttons
    EnableDisableBottomBtns;
  end;
end;

// -----------------------------------------------------------------------------

function TMainForm.CreateSectionForm(AOwner: TComponent;
  SidebarSectionType: integer): TSectionForm;

begin
  Result:=nil;

  case SidebarSectionType of
    SSTMT_SourceTargetDB:
      Result := TMigrationSourceTargetForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_ObjSelection:
      Result := TMigrationObjSelForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_ObjMapping:
      Result := TMigrationObjMapForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_ManualEditing:
      Result := TMigrationObjManEditForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_SchemaCreation:
      Result := TMigrationCreateObjsForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_DataMapping:
      Result := TMigrationDataMapForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_BulkTransfer:
      Result := TMigrationBulkTransferForm.Create(AOwner, StatusBar, self, Grt);
    SSTMT_Summary:
      Result := TMigrationReportForm.Create(AOwner, StatusBar, self, Grt);
  end;

  if(Result<>nil)then
  begin
    Result.OnProcessStart:=DoProcessStart;
    Result.OnProcessEnd:=DoProcessEnd;
  end;
end;

// -----------------------------------------------------------------------------

function TMainForm.AddWizardHeader(SectionPanel: TSectionPanel): TWizardHeaderFrame;

var
  WizardHeader: TWizardHeaderFrame;

begin
  WizardHeader:=TWizardHeaderFrame.Create(self);
  WizardHeader.Parent:=SectionPanel;
  WizardHeader.Height:=57;
  WizardHeader.Align:=alTop;

  Result:=WizardHeader;
end;

// -----------------------------------------------------------------------------

function TMainForm.AddWizardBottom(SectionPanel: TSectionPanel): TWizardBottomFrame;

var
  WizardBottom: TWizardBottomFrame;

begin
  WizardBottom:=TWizardBottomFrame.Create(self);
  WizardBottom.Parent:=SectionPanel;
  WizardBottom.Height:=52;
  WizardBottom.Align:=alBottom;

  Result:=WizardBottom;

  WizardBottom.OnNextBtnClick := NextBtnClick;
  WizardBottom.OnBackBtnClick := BackBtnClick;
  WizardBottom.OnCancelBtnClick := CancelBtnClick;
  WizardBottom.OnDetailsBtnClick := DetailsBtnClick;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.DoProcessStart(Sender: TObject);

begin
  WizardHeaderFrame.BusyAnimate.Show;
  WizardHeaderFrame.BusyAnimate.Play(1, 74, -1);
  WizardHeaderFrame.BusyAnimate.Active:=True;

  //FDetailsBtnEnabled := WizardBottomFrame.DetailsBtn.Enabled;
  FBackBtnEnabled := WizardBottomFrame.BackBtn.Enabled;
  FNextBtnEnabled := WizardBottomFrame.NextBtn.Enabled;

  FOperationCanceled := False;

  //WizardBottomFrame.DetailsBtn.Enabled := False;
  WizardBottomFrame.BackBtn.Enabled := False;
  WizardBottomFrame.NextBtn.Enabled := False;
  WizardBottomFrame.CancelBtn.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.DoProcessEnd(Sender: TObject);

begin
  WizardHeaderFrame.BusyAnimate.Hide;
  WizardHeaderFrame.BusyAnimate.Stop;

  //WizardBottomFrame.DetailsBtn.Enabled := FDetailsBtnEnabled;
  WizardBottomFrame.BackBtn.Enabled := FBackBtnEnabled;
  WizardBottomFrame.NextBtn.Enabled := FNextBtnEnabled;
  WizardBottomFrame.CancelBtn.Enabled := False;

  FOperationCanceled := False;
end;

// -----------------------------------------------------------------------------

function TMainForm.GetOperationCanceled: Boolean;

begin
  Result := FOperationCanceled;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.SetOperationCanceled(Canceled: Boolean);

begin
  FOperationCanceled := Canceled;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.NextBtnClick(Sender: TObject);

var
  SectionForm: TSectionForm;
  SubPageIndex: Integer;

begin
  if(SectionControls.CurrentSidebarSection=nil)then
    Exit;

  if(SectionControls.CurrentSidebarSection.SectionForm=nil)then
    Exit;

  SectionForm:=SectionControls.CurrentSidebarSection.SectionForm;
  SubPageIndex:=SectionForm.SubPageIndex;

  SectionForm.BeforeSubPageIndexChange(SubPageIndex);

  if(SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex<
    SectionControls.CurrentSidebarSection.SectionForm.SubPageCount-1)then
  begin
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex:=
      SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex+1;
  end
  else if(CurrentSectionIndex<SectionControls.SectionCount-1)then
  begin
    inc(CurrentSectionIndex);
    ShowSection(CurrentSectionIndex);
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex:=0;
  end
  else if(CurrentSectionIndex=SectionControls.SectionCount-1)then
  begin
    Close;
    Exit;
  end;

  EnableDisableBottomBtns;

  SectionForm.AfterSubPageIndexChange(SubPageIndex);

  //WizardBottomFrame.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.BackBtnClick(Sender: TObject);

begin
  SectionControls.CurrentSidebarSection.SectionForm.BeforeSubPageIndexChange(
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex);

  if(SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex>0)then
  begin
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex:=
      SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex-1;
    EnableDisableBottomBtns;
  end
  else if(CurrentSectionIndex>0)then
  begin
    dec(CurrentSectionIndex);
    ShowSection(CurrentSectionIndex);
  end;

  if (WizardBottomFrame.NextBtn.Enabled) then
  begin
    try
      WizardBottomFrame.NextBtn.SetFocus;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.CancelBtnClick(Sender: TObject);

begin
  if(ShowModalDialog(_('Cancel'),
    _('Are you sure you want to cancel the current process?'),
    myx_mtConfirmation, _('Yes')+#13#10+_('No'))=1)then
    OperationCanceled := True;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.DetailsBtnClick(Sender: TObject);

begin
  if(SectionControls.CurrentSidebarSection<>nil)then
    if(SectionControls.CurrentSidebarSection.SectionForm<>nil)then
      SectionControls.CurrentSidebarSection.SectionForm.AdvancedOptionsVisible:=
        WizardBottomFrame.DetailsActivated;

  if (WizardBottomFrame.NextBtn.Enabled) then
  begin
    try
      WizardBottomFrame.NextBtn.SetFocus;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.EnableDisableBottomBtns;

begin
  if(CurrentSectionIndex=0)and(
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex=0)then
    WizardHeaderFrame.Visible:=False
  else
    WizardHeaderFrame.Visible:=True;

  WizardHeaderFrame.HeaderTitleLbl.Caption:=
    SectionControls.CurrentSidebarSection.SectionForm.SectionTitle;
  WizardHeaderFrame.HeaderInfoLbl.Caption:=
    SectionControls.CurrentSidebarSection.SectionForm.SectionInfo;

  //Next Button
  if(CurrentSectionIndex=SectionControls.SectionCount-1)and
    (SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex=
      SectionControls.CurrentSidebarSection.SectionForm.SubPageCount-1)then
  begin
    WizardBottomFrame.NextBtn.Caption := 'Finish';
  end
  else
  begin
    WizardBottomFrame.NextBtn.Caption := 'Next >';
  end;

  //Back Button
  WizardBottomFrame.BackBtn.Enabled:=Not((CurrentSectionIndex=0)and(
    SectionControls.CurrentSidebarSection.SectionForm.SubPageIndex=0));

  //Show/Hide Advanced options
  if(SectionControls.CurrentSidebarSection<>nil)then
    if(SectionControls.CurrentSidebarSection.SectionForm<>nil)then
    begin
      WizardBottomFrame.DetailsBtnVisible:=
        SectionControls.CurrentSidebarSection.SectionForm.SupportAdvancedOptions;

      WizardBottomFrame.DetailsActivated:=
        SectionControls.CurrentSidebarSection.SectionForm.AdvancedOptionsState;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.ObjectShellMIClick(Sender: TObject);

begin
  FXGrtShellForm.RefreshMIClick(self);
  FXGrtShellForm.Show;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.TntFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
  if (Key=VK_F4) and (Shift=[]) then
    ObjectShellMIClick(self);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.TntFormResize(Sender: TObject);

begin
  if (SectionControls<>nil) and
    (SectionControls.CurrentSidebarSection<>nil) and
    (SectionControls.CurrentSidebarSection.SectionForm<>nil) and
    (SectionControls.CurrentSidebarSection.SectionForm.DockedPanel<>nil) and
    (Assigned(SectionControls.CurrentSidebarSection.SectionForm.DockedPanel.OnResize)) then
    SectionControls.CurrentSidebarSection.SectionForm.DockedPanel.OnResize(self);
end;

// -----------------------------------------------------------------------------

procedure TMainForm.SplitterImgClick(Sender: TObject);
begin
  if (MigrationPlanPnl.Width > 0) then
    MigrationPlanPnl.Width := 0
  else
    MigrationPlanPnl.Width := 239;
end;

// -----------------------------------------------------------------------------

function StrAllocW(Size: Cardinal): PWideChar;
begin
  Size := SizeOf(Cardinal) + (Size * SizeOf(WideChar));
  GetMem(Result, Size);
  PCardinal(Result)^ := Size;
  Inc(PAnsiChar(Result), SizeOf(Cardinal));
end;

// -----------------------------------------------------------------------------

procedure StrDisposeW(Str: PWideChar);
begin
  if Str <> nil then
  begin
    Dec(PAnsiChar(Str), SizeOf(Cardinal));
    FreeMem(Str, Cardinal(Pointer(Str)^));
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.WMDROPFILES(var Message: TWMDROPFILES);

var
  NumFiles,
    FileNameSize: Longint;
  i: Longint;
  FileName: WideString;
  PFileName: PWideChar;

begin
  PFileName := nil;
  NumFiles := DragQueryFileW(Message.Drop, $FFFFFFFF, PFileName, 255);

  if (NumFiles > 0) and
    (SectionControls.CurrentSectionIndex = 0) then
  begin
    // only take first file
    for i := 0 to (NumFiles - 1) do
    begin
      FileNameSize := DragQueryFileW(Message.Drop, i, nil, 0) + 1;
      PFileName := StrAllocW(FileNameSize);
      DragQueryFileW(Message.Drop, i, PFileName, FileNameSize);

      FileName := PFileName;
      StrDisposeW(PFileName);

      break;
    end;

    CheckInputFile(FileName);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.CheckInputFile(FileName: WideString);

var
  SourceTargetForm: TMigrationSourceTargetForm;
  MyxConnDlgForm: TMyxConnectionDialogForm;
  FileExt: WideString;
  I,
    J,
    K: Integer;
  PRdbmsList,
    PRdbms,
    PJdbcDrivers,
    PJdbcDriver,
    PParameters,
    PParameter,
    PParamTypeDetails: Pointer;


begin
  if (WideSameText(WideExtractFileExt(FileName), '.lnk')) then
    FileName := GetShellLinkFileName(FileName);

  FileExt := WideExtractFileExt(FileName);

  // A snapshot file
  if (WideSameText(FileExt, '.xml')) then
  begin
    RestoreSnapshot(FileName);

    Exit;
  end;


  // Scan all JDBC drivers for a know extension
  PRdbmsList := Grt.Global['/rdbmsMgmt/rdbms'];
  for I := 0 to Grt.ListCount(PRdbmsList) - 1 do
  begin
    PRdbms := Grt.ListItem[PRdbmsList, I];

    PJdbcDrivers := Grt.DictItem[PRdbms, 'drivers'];

    for J := 0 to Grt.ListCount(PJdbcDrivers) - 1 do
    begin
      PJdbcDriver := Grt.ListItem[PJdbcDrivers, J];

      PParameters := Grt.DictItem[PJdbcDriver, 'parameters'];

      for K := 0 to Grt.ListCount(PParameters) - 1 do
      begin
        PParameter := Grt.ListItem[PParameters, K];

        if (PParameter <> nil) then
        begin
          PParamTypeDetails := Grt.DictItem[
            PParameter, 'paramTypeDetails'];

          if (PParamTypeDetails <> nil) and
            (WideSameText(FileExt,
              '.' + Grt.DictString[PParamTypeDetails, 'fileExtension'])) then
          begin
            // Filetype found
            SourceTargetForm :=
              TMigrationSourceTargetForm(
                SectionControls.CurrentSidebarSection.SectionForm);

            MyxConnDlgForm := SourceTargetForm.MyxConnDlgForm;

            SourceTargetForm.SubPageIndex := 2;

            EnableDisableBottomBtns;

            MyxConnDlgForm.RDBMSComboBox.ItemIndex :=
              MyxConnDlgForm.RDBMSComboBox.Items.IndexOfObject(PRdbms);

            MyxConnDlgForm.RDBMSComboBoxCloseUp(self);

            MyxConnDlgForm.DriverComboBox.ItemIndex :=
              MyxConnDlgForm.DriverComboBox.Items.IndexOfObject(PJdbcDriver);

            MyxConnDlgForm.DriverComboBoxCloseUp(self);

            MyxConnDlgForm.ParamValue['databaseFile'] := FileName;

            break;
          end;
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.StoreApplicationSnapshotMIClick(Sender: TObject);

var
  PApplicationOptions: Pointer;
  SaveDlg: TSaveDialog;
  SectionForm: TSectionForm;
  FileName: WideString;

begin
  SaveDlg := TSaveDialog.Create(self);
  try
    SaveDlg.Title := 'Store Application Snapshot';

    SaveDlg.Filter := _('MySQL Migration Toolkit Snapshot')+
      ' (*.xml)|*.xml|'+
      _('All files')+' (*.*)|*.*';

    if (SaveDlg.Execute) then
    begin
      FileName := SaveDlg.Filename;

      if (Pos('.', FileName)=0) then
        FileName := FileName + '.xml';

      //Store current section and active page in migration object
      PApplicationOptions := Grt.Global['/migration/applicationData'];

      Grt.DictString[PApplicationOptions,
        'CurrentSection'] := IntToStr(CurrentSectionIndex);

      SectionForm := SectionControls.CurrentSidebarSection.SectionForm;

      Grt.DictString[PApplicationOptions,
        'CurrentPage'] := IntToStr(SectionForm.SubPageIndex);


      Grt.ValueSaveToFile(FileName, Grt.Global['/']);
    end;
  finally
    SaveDlg.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.LoadApplicationSnapshotMIClick(Sender: TObject);

var
  OpenDlg: TOpenDialog;

begin
  OpenDlg := TOpenDialog.Create(self);
  try
    OpenDlg.Title := 'Load Application Snapshot';

    OpenDlg.Filter := _('MySQL Migration Toolkit Snapshot')+
      ' (*.xml)|*.xml|'+
      _('All files')+' (*.*)|*.*';

    if OpenDlg.Execute then
    begin
      RestoreSnapshot(OpenDlg.FileName);
    end;
  finally
    OpenDlg.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.RestoreSnapshot(FileName: WideString);

var
  Error: Integer;
  CurrentPage: Integer;
  SectionForm: TSectionForm;
  NewRootValue: Pointer;

begin
  if (Not(FileExists(FileName))) then
    raise Exception.Create(Format(_('The file %s cannot be found.'), [FileName]));

  NewRootValue := Grt.ValueLoadFromFile(FileName);
  Error := Grt.GlobalSetRoot(NewRootValue);

  Grt.ValueRelease(NewRootValue);

  if (Error<>0) then
  begin
    ShowModalDialog(_('Error'),
      Format(
        _('The following error occured while loading the ' +
          'snapshot. Error: %d.'), [Error]),
      myx_mtError, _('Ok'));

    Exit;
  end;

  // Restore current page
  CurrentSectionIndex := StrToIntDef(
    Grt.GlobalAsString['/migration/applicationData/CurrentSection'],
    0);

  ShowSection(CurrentSectionIndex);

  // Restore current section
  CurrentPage := StrToIntDef(
    Grt.GlobalAsString['/migration/applicationData/CurrentPage'],
    0);

  SectionForm := SectionControls.CurrentSidebarSection.SectionForm;

  SectionForm.SubPageIndex := CurrentPage;

  BackBtn.Enabled := (CurrentSectionIndex > 0) or
    ((CurrentSectionIndex = 0) and (CurrentPage > 0));

  NextBtn.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.AboutMIClick(Sender: TObject);
begin
  ShowAboutDialog('MySQL Migration Toolkit',
    product_version+' '+product_build_level,
    _('Michael G. Zinner, main concept, graphical design, '+
    'Windows development, library coding, Java coding | '+
    'Alfredo Kengi Kojima, library coding | '+
    'Mike Lischke, Windows development | '+
    'Eric Herman, Java development supervising | '+
    'Mike Hillyer, documentation'));
end;

// -----------------------------------------------------------------------------

function TMainForm.GetNextBtn: TButton;

begin
  Result := WizardBottomFrame.NextBtn;
end;

// -----------------------------------------------------------------------------

function TMainForm.GetBackBtn: TButton;

begin
  Result := WizardBottomFrame.BackBtn;
end;

// -----------------------------------------------------------------------------

function TMainForm.GetDetailsBtn: TButton;

begin
  Result := WizardBottomFrame.DetailsBtn;
end;

// -----------------------------------------------------------------------------

function TMainForm.GetDetailsActivated: Boolean;

begin
  Result := WizardBottomFrame.DetailsActivated;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.SetDetailsActivated(Activated: Boolean);

begin
  WizardBottomFrame.DetailsActivated := Activated;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.ExitMIClick(Sender: TObject);
begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ProcessTextOutput(Text: WideString);

begin
  if (Trim(Text) <> '') then
    ShowModalDialog(Application.Title,
      _(Text), myx_mtInformation, _('Ok'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlineHelpMIClick(Sender: TObject);

begin
  ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.GenerateMigrationScriptMIClick(Sender: TObject);

var
  SaveDlg: TSaveDialog;
  FileName: WideString;
  Script: WideString;
  AnsiScript: String;
  I,
    ScriptCheckPoint,
    ScriptEndPos: Integer;
  ScriptGenerationParams: TTntStringList;

begin
  SaveDlg := TSaveDialog.Create(self);
  try
    SaveDlg.Title := 'Store Migration Script';
    SaveDlg.DefaultExt := '.lua';

    SaveDlg.Filter := _('MySQL Migration Script')+
      ' (*.lua)|*.lua|'+
      _('All files')+' (*.*)|*.*';

    if (SaveDlg.Execute) then
    begin
      FileName := SaveDlg.Filename;

      Script := LoadTextFromFile(GetApplDir + 'scripts\migration_script_template.lua');

      ScriptCheckPoint := Grt.GlobalAsInt[
        '/migration/applicationData/ScriptGenerationCheckpoint'];

      ScriptGenerationParams := TTntStringList.Create;
      try
        FillGrtScriptVariables(ScriptGenerationParams, ScriptCheckPoint);

        for I := 0 to ScriptGenerationParams.Count - 1 do
        begin
          Script := Tnt_WideStringReplace(Script, '%' +
            ScriptGenerationParams.Names[I] + '%',
            ScriptGenerationParams.ValueFromIndex[I],
            [rfReplaceAll]);
        end;
      finally
        ScriptGenerationParams.Free;
      end;

      AnsiScript := Utf8Encode(Script);

      // Cut the script at the current checkpoint
      ScriptEndPos := Pos('-- checkpoint ' +
        IntToStr(ScriptCheckPoint), AnsiScript);

      if (ScriptEndPos > 0) then
        AnsiScript := Copy(AnsiScript, 1, ScriptEndPos - 1);

      SaveAnsiTextToFile(FileName, AnsiScript);

      CreateSubProcess('notepad ' + FileName, '');
    end;
  finally
    SaveDlg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FillGrtScriptVariables(Params: TTntStringList;
  Checkpoint: Integer);

var
  I: Integer;
  J: Integer;
  Count: Integer;
  PList: Pointer;
  S: WideString;
  S2: WideString;
  Key: WideString;

begin
  // -- Set source and target connection
  if (Checkpoint >= 1) then
  begin
    // Params sourceConnection & targetConnection
    for J := 0 to 1 do
    begin
      if (J = 0) then
        S2 := 'source'
      else
        S2 := 'target';

      S := '{' + #13#10 +
        '    name= "' + S2 + 'Conn",' + #13#10 +
        '    _id= grt.newGuid(),' + #13#10 +
        '    driver= "' + Grt.GlobalAsString['/migration/' + S2 + 'Connection/driver'] + '",' + #13#10 +
        '    parameterValues= {' + #13#10;

      PList := Grt.Global['/migration/' + S2 + 'Connection/parameterValues'];
      for I := 0 to Grt.DictItemCount(PList) - 1 do
      begin
        S := S + '      ' + Grt.DictKey[PList, I] + '= "' +
          Tnt_WideStringReplace(
            Tnt_WideStringReplace(
              Grt.DictString[PList, Grt.DictKey[PList, I]],
              '\', '\\', [rfReplaceAll]),
            '"', '\"', [rfReplaceAll])
           + '"';

        if (I < Grt.DictItemCount(PList) - 1) then
          S := S + ',' + #13#10
        else
          S := S + #13#10;
      end;

      S := S + '    },' + #13#10;

      S := S +'    modules= {' + #13#10;

      PList := Grt.Global['/migration/' + S2 + 'Connection/modules'];
      for I := 0 to Grt.DictItemCount(PList) - 1 do
      begin
        S := S + '      ' + Grt.DictKey[PList, I] + '= "' +
          Grt.DictString[PList, Grt.DictKey[PList, I]] + '"';

        if (I < Grt.DictItemCount(PList) - 1) then
          S := S + ',' + #13#10
        else
          S := S + #13#10;
      end;

      S := S + '    }' + #13#10 +
        '  }';

      Params.Values[S2 + 'Connection'] := S;
    end;
  end;

  // -- Do the reverse engineering
  if (Checkpoint >= 2) then
  begin
    Params.Values['reverseEngineerOnlyTableObjects'] := IntToStr(
      Grt.GlobalAsInt['/migration/applicationData/' +
        'reverseEngineerOnlyTableObjects']);

    PList := Grt.Global['/migration/selectedSchemataNames'];
    S := '';

    Count := Grt.ListCount(PList);
    for I := 0 to Count - 1 do
    begin
      S := S + '"' + Grt.ListString[PList, I] + '"';

      if (I < Count -1) then
        S := S + ', ';
    end;

    if (S <> '') then
      Params.Values['sourceSchemataList'] := S
    else
      Params.Values['sourceSchemataList'] := '""';
  end;

  // -- Migration methods and ignore list
  if (Checkpoint >= 3) then
  begin
    PList := Grt.Global['/migration/ignoreList'];
    S := '';

    Count := Grt.ListCount(PList);
    for I := 0 to Count - 1 do
    begin
      S := S + '"' + Grt.ListString[PList, I] + '"';

      if (I < Count -1) then
        S := S + ', ' + #13#10 + '  ';
    end;

    if (S <> '') then
      Params.Values['ignoreList'] := S
    else
      Params.Values['ignoreList'] := '""';
  end;

  // -- Set object mappings and to migration
  if (Checkpoint >= 4) then
  begin
    PList := Grt.Global['/migration/mappingDefaults'];
    S := '{' + #13#10;
    S2 := '';

    Count := Grt.ListCount(PList);
    for I := 0 to Count - 1 do
    begin
      S := S + '    ' + Trim(Grt.ValueAsLuaCode(
        Grt.ListItem[PList, I], 4));

      if (I < Count -1) then
        S := S + ',' + #13#10;

      if (S2 <> '') then
        S2 := S2 + '  ';

      S2 := S2 + 'grtS.set(grtV.getGlobal("/migration/mappingDefaults/' +
        IntToStr(I) + '"), "db.migration.Mapping")' + #13#10 +
        '  grtV.setContentType(grtV.getGlobal("/migration/mappingDefaults/' +
        IntToStr(I) + '/params"), "string")' + #13#10;
    end;

    S := S + '  }';

    Params.Values['mappingDefaults'] := S;
    Params.Values['mappingDefaultsStructs'] := S2;

    // Object creation parameters.
    S := '';
    PList := Grt.Global['/migration/objectCreationParams'];
    Count := Grt.DictItemCount(PList);
    for I := 0 to Count - 1 do
    begin
      Key := Grt.DictKey[PList, I];
      if S <> '' then
        S := S + #13#10 + '  ';
      S := S + 'grtV.setGlobal("/migration/objectCreationParams/' + Key + '", "' +
        Grt.DictString[PList, Key] + '")';
    end;
    Params.Values['objectCreationParams'] := S;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMInitMenuPopup(var MSG: TWMInitMenuPopup);

begin
  if (Msg.MenuPopup = WindowMI.Handle) then
    BuildRegisterApplicationMenuItems(WindowMI);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.RunScript;

var
  Snapshot,
  ScriptFileName: WideString;

begin
  Snapshot := GetStringOptionFromCommandLine('snapshot');
  if (Snapshot <> '') and (FileExists(Snapshot)) then
    RestoreSnapshot(Snapshot);

  ScriptFileName := GetStringOptionFromCommandLine('script');
  if (ScriptFileName <> '') and (FileExists(ScriptFileName)) then
    Grt.ExecuteScriptTask(ScriptFileName, True, ProcessTextOutput);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ListReportedBugsClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com/search.php?search_for=&limit=All&order_by=&direction=ASC&cmd=display&status=Active&severity=&priority=all&bug_type=MySQL+Migration+Toolkit&php_os=&phpver=&assign=&reviewer=&actionby=&bug_age=0&notinreleasedversions=&affectscustomer=');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ReportBugMIClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
