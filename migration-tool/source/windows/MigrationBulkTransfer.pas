unit MigrationBulkTransfer;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees,
  MigrationObjMapDetail, PngTools,
  Grt, WizardPage;

type
  TMigrationBulkTransferForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    BulkTransferTabSheet: TTntTabSheet;
    MigGBox: TTntGroupBox;
    BulkTransImg: TTntImage;
    MigTaskHeaderLbl: TTntLabel;
    GenSQLLbl: TTntLabel;
    MigTaskLbl: TTntLabel;
    AnalyzeImg: TTntImage;
    MigExecLbl: TTntLabel;
    ResultLbl: TTntLabel;
    MessageLogGBox: TTntGroupBox;
    GRTMessageMemo: TTntMemo;
    GrtMessageLbl: TTntLabel;
    GrtProgressBar: TTntProgressBar;
    GrtProgressLbl: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DoTasks;
    procedure DockPnlResize(Sender: TObject);
    procedure DoProgressError(
      msg: WideString; TaskImg: TTntImage);

    procedure ProcessGrtOutput(S: WideString);
    procedure ProcessGrtMessages(Messages: TMYX_GRT_MSGS);
    function ProcessGrtStatusQuery: Integer;
  private
    { Private declarations }
    FMigLogMainObjList,
      FMigLogObjList: TList;
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
    LTDT_LogEntry,
    LTDT_ListMember
  );

  PLogTreeData = ^TLogTreeData;
  TLogTreeData = record
    NodeType: TLogTreeDataType;

    PLogEntry: Pointer;
    
    PSourceObject: Pointer;
    SourceCaption: WideString;
    PTargetObject: Pointer;
    TargetCaption: WideString;
    Msg: WideString;

    StructName: WideString;
    MemberCaption: WideString;
  end;

var
  MigrationObjMapForm: TMigrationBulkTransferForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.FormCreate(Sender: TObject);

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
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.FormDestroy(Sender: TObject);

begin
  //
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
  begin
    // Set script checkpoint
    Grt.GlobalAsInt['/migration/applicationData/' +
      'ScriptGenerationCheckpoint'] := 6;

    DoTasks;
  end
  else
    MainPngControl.ActivePageIndex := NewSubPageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Bulk Data Transfer');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('The data is copied from the source database to the target database.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := True;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    0:
      MessageLogGBox.Visible := State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationBulkTransferForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := MessageLogGBox.Visible;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.BeforeSubPageIndexChange(SectionIndex: Integer);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------
// ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.DoTasks;

var
  ModuleName: WideString;
  Task: IGrtTask;

begin
  AnalyzeImg.Picture.Assign(TaskUncheckedPNGImg);
  BulkTransImg.Picture.Assign(TaskUncheckedPNGImg);

  // initialize progress widgets
  ResultLbl.Caption := _('Executing ...');
  ResultLbl.Show;
  ResultLbl.Font.Color := clBlack;
  GrtMessageLbl.Caption := '';
  GrtMessageLbl.Show;
  GRTMessageMemo.Text := '';
  GrtProgressBar.Visible := False;
  GrtProgressLbl.Visible := False;

  MainPngControl.ActivePage := BulkTransferTabSheet;

  //Start animation
  DoProcessStart;
  try
    Application.ProcessMessages;

    AnalyzeImg.Picture.Assign(TaskCheckedPNGImg);
    Application.ProcessMessages;

    //Generate SQL create statements
    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/sourceConnection'], 'modules'],
        'MigrationModule'];

      // Clear log
      myx_grt_list_clear(Grt.Global['/migration/dataTransferLog']);

      Task := Grt.CreateStandardTask(
        _('Doing a bulk transfer of data'), 
        ModuleName,
        'dataBulkTransfer',
        [
          Grt.GetGlobalAsParam('/migration/sourceConnection'),
          Grt.GetGlobalAsParam('/migration/sourceCatalog'),
          Grt.GetGlobalAsParam('/migration/targetConnection'),
          Grt.GetGlobalAsParam('/migration/targetCatalog'),
          Grt.Global['/migration/dataBulkTransferParams'],
          Grt.GetGlobalAsParam('/migration/dataTransferLog')
        ],
        ProcessGrtOutput,
        ProcessGrtMessages,
        True,
        True,
        -1,
        nil,
        ProcessGrtStatusQuery
      );
      Grt.AddTaskAndWaitWithMessages(Task, True);
    except
      on x: EGrtError do
      begin
        DoProgressError(Format(
            _('The SQL create statements could not be created '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          BulkTransImg);

        Exit;
      end;
    end;

    BulkTransImg.Picture.Assign(TaskCheckedPNGImg);

    if (FWizardInterface.OperationCanceled) then
      ResultLbl.Caption := _('Canceled by user.')
    else
      ResultLbl.Caption := _('Execution completed successfully.');

    GrtMessageLbl.Hide;
    GrtProgressBar.Hide;
    GrtProgressLbl.Hide;

    //RefreshMigrationLog;
  finally
    DoProcessEnd;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.ProcessGrtOutput(S: WideString);

begin
  GRTMessageMemo.Lines.Add(S);
  GRTMessageMemo.SelStart := GRTMessageMemo.GetTextLen;
  GRTMessageMemo.Perform(EM_SCROLLCARET, 0, 0);
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.ProcessGrtMessages(
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

function TMigrationBulkTransferForm.ProcessGrtStatusQuery: Integer;

begin
  Result := Ord(FWizardInterface.OperationCanceled);
end;

// -----------------------------------------------------------------------------

procedure TMigrationBulkTransferForm.DoProgressError(
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


end.
