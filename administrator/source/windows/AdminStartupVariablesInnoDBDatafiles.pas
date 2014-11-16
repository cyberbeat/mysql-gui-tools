unit AdminStartupVariablesInnoDBDatafiles;

interface

uses
  Windows, Messages, SysUtils, Variants, TntClasses, Graphics, Controls,
  Forms, TntForms,
  TntDialogs, TntStdCtrls, AuxFuncs, TeEngine, Series, ExtCtrls,
  TeeProcs, Chart, ComCtrls, TntComCtrls, StdCtrls, Classes ;

type
  TAdminStartupVariablesInnoDBDatafilesForm = class(TTntForm)
    DriveInfoCBox: TTntGroupBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Label4: TTntLabel;
    VolumeNameLbl: TTntLabel;
    FileSystemLbl: TTntLabel;
    Label6: TTntLabel;
    FreeSpaceLbl: TTntLabel;
    Label8: TTntLabel;
    TotalSizeLbl: TTntLabel;
    OKBtn: TTntButton;
    CancelBtn: TTntButton;
    TableSpaceGBox: TTntGroupBox;
    Label7: TTntLabel;
    SizeEd: TTntEdit;
    SizeUpDown: TTntUpDown;
    UnitCBox: TTntComboBox;
    TablespaceEd: TTntEdit;
    Label9: TTntLabel;
    DiskChart: TChart;
    Series1: TPieSeries;
    DriveCBox: TTntComboBox;
    Label3: TTntLabel;
    Label5: TTntLabel;
    Label10: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure DriveCBoxChange(Sender: TObject);
    procedure RefreshGraph;
    procedure SizeEdChange(Sender: TObject);
    procedure TablespaceEdChange(Sender: TObject);
  private
  public
    FileSize: Int64;
    TotalSize: Int64;
    FreeSpace: int64;
  end;

var
  AdminStartupVariablesInnoDBDatafilesForm: TAdminStartupVariablesInnoDBDatafilesForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  gnugettext;

procedure TAdminStartupVariablesInnoDBDatafilesForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  FileSize := 10 * 1024 * 1024;

  DriveCBox.Items.Text := GetDrives;
  if DriveCBox.Items.Count > 0 then
    if CompareText(DriveCBox.Items[0], 'A:') = 0 then
      DriveCBox.Items.Delete(0);
  if DriveCBox.Items.Count > 0 then
    if CompareText(DriveCBox.Items[0], 'B:') = 0 then
      DriveCBox.Items.Delete(0);

  DriveCBox.ItemIndex := 0;
  DriveCBoxChange(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesInnoDBDatafilesForm.DriveCBoxChange(Sender: TObject);

var
  DriveInfo: PMYX_DRIVE_INFO;

begin
  if DriveCBox.Text <> '' then
  begin
    DriveInfo := GetDriveInfo(DriveCBox.Text);
    try
      VolumeNameLbl.Caption := DriveInfo.VolumeLabel;
      FileSystemLbl.Caption := DriveInfo.FileSystem;

      TotalSize := DriveInfo.TotalSize;
      FreeSpace := DriveInfo.FreeSpace;

      TotalSizeLbl.Caption := FormatFileSize(TotalSize);
      FreeSpaceLbl.Caption := FormatFileSize(FreeSpace);

      RefreshGraph;
    finally
      dispose(DriveInfo);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesInnoDBDatafilesForm.RefreshGraph;

begin
  Series1.Clear;
  if(TotalSize>0)then
  begin
    Series1.AddPie(TotalSize-FreeSpace - FileSize, _('Used'), $00FF0000);
    Series1.AddPie(FreeSpace, _('Free'), $00FF00FF);
    Series1.AddPie(FileSize, _('Table space'), $0000FF00);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesInnoDBDatafilesForm.SizeEdChange(Sender: TObject);

begin
  if(UnitCBox.ItemIndex=0)then
    FileSize:=int64(ExtractNumber(SizeEd.Text)) * 1024
  else
    FileSize:=int64(ExtractNumber(SizeEd.Text)) * 1024 * 1024;

  RefreshGraph;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesInnoDBDatafilesForm.TablespaceEdChange(Sender: TObject);

begin
  if Copy(TablespaceEd.Text, 2, 2)=':\' then
    if DriveCBox.Items.IndexOf(Copy(TablespaceEd.Text, 1, 2)) > -1 then
    begin
      DriveCBox.ItemIndex:= DriveCBox.Items.IndexOf(Copy(TablespaceEd.Text, 1, 2));
      DriveCBoxChange(Self);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
