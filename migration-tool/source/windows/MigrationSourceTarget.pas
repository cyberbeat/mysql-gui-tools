unit MigrationSourceTarget;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, AdvancedEdit, CheckLst,
  TntCheckLst, PngTools, myx_public_interface, TntClasses,
  myx_grt_public_interface, WizardInterface, WizardPage,
  XGrtShell, Grt, StrUtils, TntSysUtils,
  MyxConnectionDialog, TntForms;

type
  TMigrationSourceTargetForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    DBConnTabsheet: TTntTabSheet;
    SetupTabSheet: TTntTabSheet;
    BGImage: TTntImage;
    TntTabSheet7: TTntTabSheet;
    TntGroupBox2: TTntGroupBox;
    TwoWayImg: TTntImage;
    ThreeWayImg: TTntImage;
    TntLabel15: TTntLabel;
    TntLabel16: TTntLabel;
    TwoWayCaptionLbl: TTntLabel;
    ThreeWayCaptionLbl: TTntLabel;
    TwoWayRBtn: TTntRadioButton;
    ThreeWayRBtn: TTntRadioButton;
    TntLabel18: TTntLabel;
    TntImage3: TTntImage;
    TntLabel19: TTntLabel;
    WelcomeHint1Lbl: TTntLabel;
    WelcomeHint2Lbl: TTntLabel;
    TntImage5: TTntImage;
    TntLabel30: TTntLabel;
    TntLabel31: TTntLabel;
    TntLabel32: TTntLabel;
    TntLabel33: TTntLabel;
    TntLabel34: TTntLabel;
    TntTabSheet8: TTntTabSheet;
    DatabaseConnMainGBox: TTntGroupBox;
    ConnParamGBox: TTntGroupBox;
    DBConnAdvancedSettingsGBox: TTntGroupBox;
    TntGroupBox1: TTntGroupBox;
    TntLabel1: TTntLabel;
    ConnSourceImg: TTntImage;
    TntLabel3: TTntLabel;
    TntLabel4: TTntLabel;
    RetrieveSchemaNamesImg: TTntImage;
    ConnTargetImg: TTntImage;
    TntLabel6: TTntLabel;
    TntLabel7: TTntLabel;
    TntImage7: TTntImage;
    TntLabel8: TTntLabel;
    MessageLogGBox: TTntGroupBox;
    GRTMessageMemo: TTntMemo;
    ResultLbl: TTntLabel;
    TntLabel10: TTntLabel;
    InitRuntimeSystemImg: TTntImage;
    InitJavaLoaderImg: TTntImage;
    TntLabel11: TTntLabel;
    TntLabel13: TTntLabel;
    InitErrorLbl: TTntLabel;
    ProgressLbl: TTntLabel;
    JdbcDriverDescLbl: TTntLabel;
    DriverParamHeaderLbl: TTntLabel;
    SourceTargetDBConnImg: TImage;
    AdvParamSBox: TTntScrollBox;
    ParamSBox: TTntScrollBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DisplayWelcomeSheet;
    procedure DisplaySourceDatabaseConnSheet;
    procedure DisplayTargetDatabaseConnSheet;
    procedure DisplayConnectingSheet;

    procedure TwoWayImgClick(Sender: TObject);
    procedure ThreeWayImgClick(Sender: TObject);

    procedure DoConnections;
    procedure DoConnectionsError(msg: WideString; TaskImg: TTntImage);

    procedure ProcessGrtOutput(S: WideString);
    procedure ProcessGrtMessages(Messages: TMYX_GRT_MSGS);
    function ProcessGrtStatusQuery: Integer;
  private
    { Private declarations }
    FSettingConnectionValues: Boolean;

    SourceDBConnPNGImg,
    TargetDBConnPNGImg: TPNGObject;

    FCurrentConnectionSource: Boolean;

    FComponentParamMapping: TTntStringList;

    FSourceFileEdit: TTntEdit;

    FMyxConnDlgForm: TMyxConnectionDialogForm;

    function SourceDriverFilter(PDriver: Pointer): Boolean;
    function TargetDriverFilter(PDriver: Pointer): Boolean;
  protected
    procedure SetSubPageIndex(NewSubPageIndex: Integer); override;
    function GetSubPageIndex: Integer; override;

    function GetSubPageCount: integer; override;

    function GetSectionTitle: WideString; override;
    function GetSectionInfo: WideString; override;

    function GetSupportAdvancedOptions: Boolean; override;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); override;
    function GetAdvancedOptionsState: Boolean; override;
  public
    { Public declarations }
    property SourceFileEdit: TTntEdit read FSourceFileEdit;
    property MyxConnDlgForm: TMyxConnectionDialogForm read FMyxConnDlgForm;

    procedure BeforeSubPageIndexChange(SectionIndex: Integer); override;
  end;


// -----------------------------------------------------------------------------

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel:=DockPnl;

  FComponentParamMapping := TTntStringList.Create;
  FSettingConnectionValues := False;
  FSourceFileEdit := nil;

  MainPngControl.Align:=alNone;
  MainPngControl.Left:=-4;
  MainPngControl.Top:=-27;
  MainPngControl.Width:=765+4+4;
  MainPngControl.Height:=529+274;
  MainPngControl.ActivePageIndex:=0;

  //SettingConnectionValues:=False;

  SourceDBConnPNGImg:=LoadPNGImageFromResource('source_dbconn');
  TargetDBConnPNGImg:=LoadPNGImageFromResource('target_dbconn');

  TaskUncheckedPNGImg:=LoadPNGImageFromResource('task_unchecked');
  TaskCheckedPNGImg:=LoadPNGImageFromResource('task_checked');
  TaskErrorPNGImg:=LoadPNGImageFromResource('task_error');
  TaskDisabledPNGImg:=LoadPNGImageFromResource('task_disabled');

  // Init connection form
  FMyxConnDlgForm := TMyxConnectionDialogForm.Create(nil);
  FMyxConnDlgForm.ConnInfoPath := '/rdbmsMgmt';
  FMyxConnDlgForm.DisplaySchemaSelection := False;
  FMyxConnDlgForm.DisplayDescriptions := True;
  FMyxConnDlgForm.DisplayOnlyJdbcDrivers := True;

  FMyxConnDlgForm.ConnTypePnl.Parent := DatabaseConnMainGBox;
  FMyxConnDlgForm.ConnTypePnl.Left := 28;
  FMyxConnDlgForm.ConnTypePnl.Width := DatabaseConnMainGBox.Width - 40;
  FMyxConnDlgForm.StoredConnPnl.Parent := ParamSBox;
  FMyxConnDlgForm.StoredConnPnl.Top := 0;

  FMyxConnDlgForm.ParamsPnl.Parent := ParamSBox;
  FMyxConnDlgForm.ParamsPnl.Top := FMyxConnDlgForm.StoredConnPnl.Height;
  FMyxConnDlgForm.ParamsPnl.Width := ParamSBox.Width - 20;

  FMyxConnDlgForm.AdvParamPnl.Parent := AdvParamSBox;
  FMyxConnDlgForm.AdvParamPnl.Top := 0;
  FMyxConnDlgForm.AdvParamPnl.Width := AdvParamSBox.Width - 20;

  FMyxConnDlgForm.DriverNotInstalledPnl.Parent := ParamSBox;
  FMyxConnDlgForm.DriverNotInstalledPnl.Top := 0;


  if (IsWinXP) then
  begin
    TwoWayCaptionLbl.Left:=TwoWayCaptionLbl.Left-3;
    ThreeWayCaptionLbl.Left:=ThreeWayCaptionLbl.Left-3;
  end;

  DisplayWelcomeSheet;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.FormDestroy(Sender: TObject);

begin
  FMyxConnDlgForm.Free;

  SourceDBConnPNGImg.Free;
  TargetDBConnPNGImg.Free;

  FComponentParamMapping.Free;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  //Simulate Target Database Connection sheet
  if (NewSubPageIndex = 0) then
  begin
    DisplayWelcomeSheet;
  end
  else
    if (NewSubPageIndex = 1) then
    begin
      MainPngControl.ActivePageIndex := 1;
      FWizardInterface.NextBtn.Enabled := True;
    end
    else
      if (NewSubPageIndex = 2) then
        DisplaySourceDatabaseConnSheet
      else
        if (NewSubPageIndex = 3) then
        begin
          // Set script checkpoint
          Grt.GlobalAsInt['/migration/applicationData/' +
            'ScriptGenerationCheckpoint'] := 0;

          DisplayTargetDatabaseConnSheet;
        end
        else
          if (NewSubPageIndex = 4) then
          begin
            // Set script checkpoint
            Grt.GlobalAsInt['/migration/applicationData/' +
              'ScriptGenerationCheckpoint'] := 1;

            DisplayConnectingSheet;
          end
          else
            if (NewSubPageIndex >= 0) and
              (NewSubPageIndex<MainPngControl.PageCount+1) then
              MainPngControl.ActivePageIndex := NewSubPageIndex - 1;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetSubPageIndex: Integer;

begin
  //Simulate page 3, Target Database Connection
  if(MainPngControl.ActivePageIndex<2)then
    Result := MainPngControl.ActivePageIndex
  else
    if(MainPngControl.ActivePageIndex=2)then
      Result := 3 - Ord(FCurrentConnectionSource)
    else
      Result := MainPngControl.ActivePageIndex + 1;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetSubPageCount: integer;

begin
  Result := MainPngControl.PageCount+1;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    1:
      Result := _('Configuration Type');
    2:
      if (FCurrentConnectionSource) then
        Result := _('Source Database')
      else
        Result := _('Target Database');
    3:
      Result := _('Connecting to Servers');
  else
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    1:
      Result := _('Choose the type of configuration you have set up.');
    2:
      if(FCurrentConnectionSource)then
        Result := _('Select the source database you want to migrate from.')
      else
        Result := _('Select the destination database.');
    3:
      Result := _('Establishing database connections.');
  else
    Result := '';
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    1:
      Result := False;
    2:
      Result := True;
    3:
      Result := True;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    2:
      DBConnAdvancedSettingsGBox.Visible := State;
    3:
      MessageLogGBox.Visible := State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    2:
      Result := DBConnAdvancedSettingsGBox.Visible;
    3:
      Result := MessageLogGBox.Visible;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DisplayWelcomeSheet;

begin
  // Display GRT init state
  case Grt.BuiltinLoaderState of
    GrtLsNotInitialized, GrtLsInitialized:
      InitRuntimeSystemImg.Picture.Assign(TaskUncheckedPNGImg);
    GrtLsModulesLoaded:
      InitRuntimeSystemImg.Picture.Assign(TaskCheckedPNGImg);
    GrtLsInitializeFailed:
    begin
      InitRuntimeSystemImg.Picture.Assign(TaskErrorPNGImg);

      InitErrorLbl.Caption :=
        _('An error occured during the initialization of the ' +
          'runtime system. Please make sure the tool is installed ' +
          'correctly.');
    end;
  end;

  case Grt.JavaLoaderState of
    GrtLsNotInitialized, GrtLsInitialized:
      InitJavaLoaderImg.Picture.Assign(TaskUncheckedPNGImg);
    GrtLsModulesLoaded:
      InitJavaLoaderImg.Picture.Assign(TaskCheckedPNGImg);
    GrtLsInitializeFailed:
    begin
      InitJavaLoaderImg.Picture.Assign(TaskErrorPNGImg);
      InitErrorLbl.Caption :=
        _('An error occured during the initialization of the ' +
          'runtime system. Please make sure you have the Java ' +
          'Runtime Environment (JRE) 5.0 Update 8 or newer installed.');
      InitErrorLbl.Visible := True;
    end;
  end;

  if (Grt.BuiltinLoaderState = GrtLsModulesLoaded) and
    (Grt.JavaLoaderState = GrtLsModulesLoaded) then
  begin
    WelcomeHint1Lbl.Show;
    WelcomeHint2Lbl.Show;

    FWizardInterface.NextBtn.Enabled := True;
  end
  else
  begin
    FWizardInterface.NextBtn.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DisplayConnectingSheet;

begin
  MainPngControl.ActivePageIndex:=3;

  // Connect and retrieve source schemata
  DoConnections;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.TwoWayImgClick(Sender: TObject);

begin
  TwoWayRBtn.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.ThreeWayImgClick(Sender: TObject);

begin
  ThreeWayRBtn.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DoConnections;

var
  ModuleName: WideString;
  Task: IGrtTask;

begin
  ConnSourceImg.Picture.Assign(TaskUncheckedPNGImg);
  RetrieveSchemaNamesImg.Picture.Assign(TaskUncheckedPNGImg);
  ConnTargetImg.Picture.Assign(TaskUncheckedPNGImg);

  // initialize progress widgets
  ResultLbl.Caption := _('Executing ...');
  ResultLbl.Show;
  ResultLbl.Font.Color := clBlack;
  ProgressLbl.Caption := '';
  ProgressLbl.Show;
  GRTMessageMemo.Text := '';

  FWizardInterface.NextBtn.Enabled := False;

  DoProcessStart;
  try
    RetrieveSchemaNamesImg.Picture.Assign(TaskUncheckedPNGImg);

    //Retrieve source schemata
    GRTMessageMemo.Lines.Add('Connecting to source database and retrieve schemata names.');

    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/sourceConnection'], 'modules'],
        'ReverseEngineeringModule'];

      Task := Grt.CreateStandardTask(
        _('Getting schemata names'), 
        ModuleName,
        'getSchemata',
        [Grt.GetGlobalAsParam('/migration/sourceConnection')],
        ProcessGrtOutput,
        ProcessGrtMessages,
        True,
        True,
        -1,
        nil,
        ProcessGrtStatusQuery
      );
      Grt.AddTaskAndWaitWithMessages(Task, True);
      Grt.Global['/migration/sourceSchemataNames'] := Task.Result;

      Grt.ValueRelease(Task.Result);
    except
      on x: EGrtError do
      begin
        DoConnectionsError(Format(
            _('The list of schema names could not be retrieved '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          RetrieveSchemaNamesImg);

        ConnSourceImg.Picture.Assign(TaskErrorPNGImg);

        Exit;
      end;
    end;

    GRTMessageMemo.Lines.Add('Schemata names retrieved successfully.');
    ConnSourceImg.Picture.Assign(TaskCheckedPNGImg);
    RetrieveSchemaNamesImg.Picture.Assign(TaskCheckedPNGImg);

    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/targetConnection'], 'modules'],
        'ReverseEngineeringModule'];

      Task := Grt.CreateStandardTask(
        _('Connecting to target schema'), 
        ModuleName,
        'getVersion',
        [Grt.GetGlobalAsParam('/migration/targetConnection')],
        ProcessGrtOutput,
        ProcessGrtMessages);
      Grt.AddTaskAndWaitWithMessages(Task, True);
      Grt.Global['/migration/targetVersion'] := Task.Result;

      myx_grt_value_release(Task.Result);
    except
      on x: EGrtError do
      begin
        DoConnectionsError(Format(
            _('The connection to the target database could not be '+
              'established (error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          ConnTargetImg);

        Exit;
      end;
    end;

    ConnTargetImg.Picture.Assign(TaskCheckedPNGImg);

    if (FWizardInterface.OperationCanceled) then
      ResultLbl.Caption := _('Canceled by user.')
    else
      ResultLbl.Caption := _('Execution completed successfully.');

    ProgressLbl.Hide;
  finally
    DoProcessEnd;
  end;

  FWizardInterface.NextBtn.Enabled := True;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.ProcessGrtOutput(S: WideString);

begin
  GRTMessageMemo.Text := GRTMessageMemo.Text + S;
  GRTMessageMemo.Update;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.ProcessGrtMessages(
  Messages: TMYX_GRT_MSGS);

begin
  // display last message in ResultLbl
  if (Messages.msgs.Count > 0) then
  begin
    ProgressLbl.Caption := Messages.msgs[Messages.msgs.Count - 1].msg;
    ProgressLbl.Update;

    GRTMessageMemo.Text := GRTMessageMemo.Text +
      FormatGrtMessagesAsString(Messages);
    GRTMessageMemo.Update;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.ProcessGrtStatusQuery: Integer;

begin
  Result := Ord(FWizardInterface.OperationCanceled);
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DoConnectionsError(
  msg: WideString; TaskImg: TTntImage);

begin
  ResultLbl.Font.Color := clRed;
  ResultLbl.Caption:=_('An Error has occured. Press [Advanced >>] '+
    'to see the log.');
  ResultLbl.Show;

  if(TaskImg<>nil)then
    TaskImg.Picture.Assign(TaskErrorPNGImg);

  GRTMessageMemo.Lines.Add(msg);

  //Stop animation
  DoProcessEnd;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DisplaySourceDatabaseConnSheet;

begin
  FCurrentConnectionSource := True;

  FMyxConnDlgForm.ConnTargetPath := '/migration/sourceConnection';

  FMyxConnDlgForm.DriverFilter := SourceDriverFilter;

  FMyxConnDlgForm.RefreshConnInfo;

  FMyxConnDlgForm.OKButton := FWizardInterface.NextBtn as TTntButton;

  MainPngControl.ActivePageIndex := 2;

  FMyxConnDlgForm.SetConnection(
    Grt.Global[FMyxConnDlgForm.ConnTargetPath]);

  DatabaseConnMainGBox.Caption := _('Source Database Connection');
  SourceTargetDBConnImg.Picture.Assign(SourceDBConnPNGImg);
  DriverParamHeaderLbl.Caption := _('Source Connection Parameter');
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.DisplayTargetDatabaseConnSheet;

begin
  FCurrentConnectionSource := False;

  FMyxConnDlgForm.ConnTargetPath := '/migration/targetConnection';

  FMyxConnDlgForm.DriverFilter := TargetDriverFilter;

  FMyxConnDlgForm.RefreshConnInfo;

  FMyxConnDlgForm.OKButton := FWizardInterface.NextBtn as TTntButton;

  MainPngControl.ActivePageIndex := 2;

  FMyxConnDlgForm.SetConnection(
    Grt.Global[FMyxConnDlgForm.ConnTargetPath]);

  DatabaseConnMainGBox.Caption := _('Target Database Connection');
  SourceTargetDBConnImg.Picture.Assign(TargetDBConnPNGImg);
  DriverParamHeaderLbl.Caption := _('Target Connection Parameter');

  if (FMyxConnDlgForm.RdbmsComboBox.ItemIndex = -1) and
    (FMyxConnDlgForm.RdbmsComboBox.Items.Count > 0) then
  begin
    FMyxConnDlgForm.RdbmsComboBox.ItemIndex := 0;
    FMyxConnDlgForm.RdbmsComboBoxCloseup(nil);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationSourceTargetForm.BeforeSubPageIndexChange(SectionIndex: Integer);
begin
  if (SectionIndex = 2) or (SectionIndex = 3) then
  begin
    FMyxConnDlgForm.WriteConnectionToTarget;

    FMyxConnDlgForm.OKButton := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.SourceDriverFilter(PDriver: Pointer): Boolean;

begin
  Result := False;

  if (Grt.DictString[
    Grt.DictItem[PDriver, 'defaultModules'],
    'ReverseEngineeringModule'] = '') then
    Result := True;
end;

// -----------------------------------------------------------------------------

function TMigrationSourceTargetForm.TargetDriverFilter(PDriver: Pointer): Boolean;

begin
  Result := False;

  if (Grt.DictString[
    Grt.DictItem[PDriver, 'defaultModules'],
    'TransformationModule'] = '') then
    Result := True;
end;

end.
