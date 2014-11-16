unit MigrationDataMap;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees, 
  MigrationObjMapDetail, TntSysUtils, PngTools,
  WizardPage;

type
  TMigrationDataMapForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    OptionsTabSheet: TTntTabSheet;
    MigGBox: TTntGroupBox;
    TntLabel10: TTntLabel;
    TntLabel15: TTntLabel;
    CreateScriptCBox: TTntCheckBox;
    ScriptFileNameLbl: TTntLabel;
    ScriptFileNameEd: TTntEdit;
    TntLabel5: TTntLabel;
    TransferDataCBox: TTntCheckBox;
    TntLabel6: TTntLabel;
    AdvancedOptionsCBox: TTntGroupBox;
    MaxRowNumberCBox: TTntCheckBox;
    TntLabel1: TTntLabel;
    MaxRowNumberLbl: TTntLabel;
    MaxRowNumberEd: TTntEdit;
    ExcludeBlobsCBox: TTntCheckBox;
    TntLabel3: TTntLabel;
    OverrideBlobLimitCBox: TTntCheckBox;
    OverrideBlobLimitLbl: TTntLabel;
    ChooseInsertFileBtn: TTntButton;
    BlobStreamingCheckBox: TTntCheckBox;
    BlobStreamingLbl: TTntLabel;
    RTrimCBox: TTntCheckBox;
    TntLabel2: TTntLabel;
    DoNotQuoteSourceIdsCBox: TTntCheckBox;
    TntLabel4: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DockPnlResize(Sender: TObject);
    procedure MaxRowNumberCBoxClick(Sender: TObject);
    procedure CreateScriptCBoxClick(Sender: TObject);
    procedure ChooseInsertFileBtnClick(Sender: TObject);
    procedure BlobStreamingCheckBoxClick(Sender: TObject);
    procedure ExcludeBlobsCBoxClick(Sender: TObject);
  private
    { Private declarations }
    TaskUncheckedPNGImg,
    TaskCheckedPNGImg,
    TaskErrorPNGImg,
    TaskDisabledPNGImg: TPNGObject;
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
  MigrationObjMapForm: TMigrationDataMapForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.FormCreate(Sender: TObject);

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

  ScriptFileNameEd.Text :=
    WideIncludeTrailingBackslash(
      GetSpecialFolder(CSIDL_PERSONAL)) + 'Inserts.sql';
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.FormDestroy(Sender: TObject);

begin
  TaskUncheckedPNGImg.Free;
  TaskCheckedPNGImg.Free;
  TaskErrorPNGImg.Free;
  TaskDisabledPNGImg.Free;
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  {if (NewSubPageIndex=0) then
    DoMigrationTasks
  else}
    MainPngControl.ActivePageIndex := NewSubPageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Data Mapping Options');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('The selected object will now be migrated.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := True;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    0:
      AdvancedOptionsCBox.Visible := State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationDataMapForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := AdvancedOptionsCBox.Visible;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.BeforeSubPageIndexChange(SectionIndex: Integer);

var
  TransferData,
    CreateScript,
    RTrim,
    ExcludeBlob,
    OverrideBlobLimit,
    BlobStreaming,
    MaxRowsNum,
    DoNotQuoteSourceIds: WideString;

begin
  // Build bulk transfer params
  if (TransferDataCBox.Checked) then
    TransferData := 'yes'
  else
    TransferData := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/TransferData'] :=
    TransferData;

  if (CreateScriptCBox.Checked) then
    CreateScript := 'yes'
  else
    CreateScript := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/CreateScript'] :=
    CreateScript;

  if (CreateScriptCBox.Checked) then
    Grt.GlobalAsString[
      '/migration/dataBulkTransferParams/ScriptFileName'] :=
      ScriptFileNameEd.Text;


  if (RTrimCBox.Checked) then
    RTrim := 'yes'
  else
    RTrim := 'no';
  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/RightTrimForText'] :=
    RTrim;


  if (ExcludeBlobsCBox.Checked) then
    ExcludeBlob := 'yes'
  else
    ExcludeBlob := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/ExcludeBlob'] :=
    ExcludeBlob;



  if (OverrideBlobLimitCBox.Checked) then
    OverrideBlobLimit := 'yes'
  else
    OverrideBlobLimit := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/OverrideBlobLimit'] :=
    OverrideBlobLimit;



  if (BlobStreamingCheckBox.Checked) then
    BlobStreaming := 'yes'
  else
    BlobStreaming := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/BlobStreaming'] :=
    BlobStreaming;



  if (MaxRowNumberCBox.Checked) then
    MaxRowsNum := MaxRowNumberEd.Text
  else
    MaxRowsNum := '-1';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/MaxRowsNum'] :=
    MaxRowsNum;


  if (DoNotQuoteSourceIdsCBox.Checked) then
    DoNotQuoteSourceIds := 'yes'
  else
    DoNotQuoteSourceIds := 'no';

  Grt.GlobalAsString[
    '/migration/dataBulkTransferParams/DoNotQuoteSourceIds'] :=
    DoNotQuoteSourceIds;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.MaxRowNumberCBoxClick(Sender: TObject);

begin
  MaxRowNumberLbl.Enabled := MaxRowNumberCBox.Checked;
  MaxRowNumberEd.Enabled := MaxRowNumberCBox.Checked;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.CreateScriptCBoxClick(Sender: TObject);

begin
  ScriptFileNameLbl.Enabled := CreateScriptCBox.Checked;
  ScriptFileNameEd.Enabled := CreateScriptCBox.Checked;
  ChooseInsertFileBtn.Enabled := CreateScriptCBox.Checked;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.ChooseInsertFileBtnClick(Sender: TObject);

var
  OpenDialog: TOpenDialog;
  FileName: WideString;

begin
  OpenDialog := TOpenDialog.Create(self);
  try
    OpenDialog.Title := 'SQL Script File';
    OpenDialog.DefaultExt := '.sql';

    OpenDialog.Filter := _('SQL Inserts Script')+
      ' (*.sql)|*.sql|'+
      _('All files')+' (*.*)|*.*';

    if OpenDialog.Execute then
    begin
      FileName := OpenDialog.Filename;
      ScriptFileNameEd.Text := FileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.BlobStreamingCheckBoxClick(
  Sender: TObject);

begin
  if (BlobStreamingCheckBox.Checked) then
    OverrideBlobLimitCBox.Checked := False;

  OverrideBlobLimitCBox.Enabled := Not(BlobStreamingCheckBox.Checked);
  OverrideBlobLimitLbl.Enabled := OverrideBlobLimitCBox.Enabled;
end;

// -----------------------------------------------------------------------------

procedure TMigrationDataMapForm.ExcludeBlobsCBoxClick(Sender: TObject);

begin
  if (ExcludeBlobsCBox.Checked) then
  begin
    OverrideBlobLimitCBox.Checked := False;
    BlobStreamingCheckBox.Checked := False;
  end;

  OverrideBlobLimitCBox.Enabled := Not(ExcludeBlobsCBox.Checked);
  OverrideBlobLimitLbl.Enabled := OverrideBlobLimitCBox.Enabled;

  BlobStreamingCheckBox.Enabled := Not(ExcludeBlobsCBox.Checked);
  BlobStreamingLbl.Enabled := BlobStreamingCheckBox.Enabled;

end;

// -----------------------------------------------------------------------------

end.
