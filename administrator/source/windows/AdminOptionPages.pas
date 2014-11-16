unit AdminOptionPages;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Options, AuxFuncs, TntComCtrls, TntStdCtrls,
  TntDialogs;

type
  TAdminOptionPagesForm = class(TApplicationOptionsForm)
    OptionPageControl: TTntPageControl;
    AdminGeneralTabSheet: TTabSheet;
    GroupBox4: TTntGroupBox;
    AdminShowGlobalPrivCBox: TTntCheckBox;
    AdminShowTblColPrivCBox: TTntCheckBox;
    GroupBox1: TTntGroupBox;
    UsePeakLevelCBox: TTntCheckBox;
    ResetPeakLevelCBox: TTntCheckBox;
    PeakLevelResetTicksEd: TTntEdit;
    PeakLevelResetTicksUpDown: TTntUpDown;
    PeakLevelResetTicksLbl: TTntLabel;
    GroupBox2: TTntGroupBox;
    AddDateTimeToBackupFilesCBox: TTntCheckBox;
    WriteBackupLogfilePathCBox: TTntCheckBox;
    LogFilePathLbl: TTntLabel;
    BackupLogFilePathEd: TTntEdit;
    BackupLogFileBrowseBtn: TTntButton;
    LogRowProcessingProgressCBox: TTntCheckBox;
    LogEveryLbl: TTntLabel;
    LogEveryEd: TTntEdit;
    LogEveryRowLbl: TTntLabel;
    TntLabel1: TTntLabel;
    AdminShowHostsCheckbox: TTntCheckBox;
    procedure FormCreate(Sender: TObject);

    procedure SetControls(PageNr: integer); override;
    procedure ApplyChanges(PageNr: integer); override;

    procedure DoChange(Sender: TObject);
    procedure EnableDisableControls;

    procedure ResetPeakLevelCBoxClick(Sender: TObject);
    procedure BackupLogFileBrowseBtnClick(Sender: TObject);
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ApplicationDataModule;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.FormCreate(Sender: TObject);

begin
  InitForm(Self);
  OptionsImgNames.Add('options_admin');

  DockOptionPageControl := OptionPageControl;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.SetControls(PageNr: integer);

begin
  if PageNr = 0 then
  begin
    AdminShowGlobalPrivCBox.Checked := ApplicationDM.Options.ShowUserGlobalPrivileges;
    AdminShowTblColPrivCBox.Checked := ApplicationDM.Options.ShowUserTableColumnPrivileges;

    AdminShowHostsCheckbox.Checked := ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'];

    UsePeakLevelCBox.Checked := ApplicationDM.Options.UsePeakLevel;
    ResetPeakLevelCBox.Checked := ApplicationDM.Options.ResetPeakLevel;
    PeakLevelResetTicksUpDown.Position := ApplicationDM.Options.PeakLevelResetTicks;

    AddDateTimeToBackupFilesCBox.Checked := ApplicationDM.Options.AddDateTimeToBackupFiles;
    WriteBackupLogfilePathCBox.Checked := ApplicationDM.Options.WriteBackupLog;
    BackupLogFilePathEd.Text := ApplicationDM.Options.BackupLogDir;
    LogRowProcessingProgressCBox.Checked := ApplicationDM.Options.BackupLogEntryAfterRows > 0;
    if ApplicationDM.Options.BackupLogEntryAfterRows > 0 then
      LogEveryEd.Text := IntToStr(ApplicationDM.Options.BackupLogEntryAfterRows)
    else
      LogEveryEd.Text := '';

    EnableDisableControls;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.ApplyChanges(PageNr: integer);

begin
  if PageNr = 0 then
  begin
    ApplicationDM.Options.ShowUserGlobalPrivileges := AdminShowGlobalPrivCBox.Checked;
    ApplicationDM.Options.ShowUserTableColumnPrivileges := AdminShowTblColPrivCBox.Checked;

    ApplicationDM.OptionProvider.OptionAsBoolean['ShowUserHosts'] := AdminShowHostsCheckbox.Checked;

    ApplicationDM.Options.UsePeakLevel := UsePeakLevelCBox.Checked;
    ApplicationDM.Options.ResetPeakLevel := ResetPeakLevelCBox.Checked;
    ApplicationDM.Options.PeakLevelResetTicks := PeakLevelResetTicksUpDown.Position;

    ApplicationDM.Options.AddDateTimeToBackupFiles := AddDateTimeToBackupFilesCBox.Checked;
    ApplicationDM.Options.WriteBackupLog := WriteBackupLogfilePathCBox.Checked;
    ApplicationDM.Options.BackupLogDir := BackupLogFilePathEd.Text;
    if not LogRowProcessingProgressCBox.Checked then
      ApplicationDM.Options.BackupLogEntryAfterRows := 0
    else
      ApplicationDM.Options.BackupLogEntryAfterRows := StrToIntDef(LogEveryEd.Text, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.DoChange(Sender: TObject);

begin
  if Assigned(DoPageContentChanged) then
    DoPageContentChanged(Sender);

  EnableDisableControls;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.EnableDisableControls;

begin
  PeakLevelResetTicksEd.Enabled := ResetPeakLevelCBox.Checked;
  PeakLevelResetTicksUpDown.Enabled := ResetPeakLevelCBox.Checked;
  PeakLevelResetTicksLbl.Enabled := ResetPeakLevelCBox.Checked;

  if(Not(WriteBackupLogfilePathCBox.Checked))then
  begin
    LogFilePathLbl.Enabled := False;
    BackupLogFilePathEd.Enabled := False;
    BackupLogFileBrowseBtn.Enabled := False;
    LogRowProcessingProgressCBox.Enabled := False;
    LogEveryLbl.Enabled := False;
    LogEveryEd.Enabled := False;
    LogEveryRowLbl.Enabled := False;
  end
  else
  begin
    LogFilePathLbl.Enabled := True;
    BackupLogFilePathEd.Enabled := True;
    BackupLogFileBrowseBtn.Enabled := True;
    LogRowProcessingProgressCBox.Enabled := True;

    if(Not(LogRowProcessingProgressCBox.Checked))then
    begin
      LogEveryLbl.Enabled := False;
      LogEveryEd.Enabled := False;
      LogEveryRowLbl.Enabled := False;
    end
    else
    begin
      LogEveryLbl.Enabled := True;
      LogEveryEd.Enabled := True;
      LogEveryRowLbl.Enabled := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.ResetPeakLevelCBoxClick(Sender: TObject);

begin
  DoChange(Sender);

  EnableDisableControls;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminOptionPagesForm.BackupLogFileBrowseBtnClick(Sender: TObject);

var
  OpenDialog: TTntOpenDialog;

begin
  OpenDialog := TTntOpenDialog.Create(self);
  try
    OpenDialog.Title := 'Select Backup Log File Path...';
    OpenDialog.DefaultExt := '*.*';
    OpenDialog.FileName := 'MySQLAdminBackupLog.txt';
    OpenDialog.Options := OpenDialog.Options-[ofFileMustExist] + [ofPathMustExist];
    if OpenDialog.Execute then
      BackupLogFilePathEd.Text := ExtractFilePath(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
