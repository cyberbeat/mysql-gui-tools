unit AdminRestoreProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, AuxFuncs, TntForms, TntStdCtrls, TntComCtrls,
  gnugettext, ExtCtrls;

type
  TRestoreOperation = (TR_ANALYZE, TR_RESTORE);

  TAdminRestoreProgressForm = class(TTntForm)
    ProgressGBox: TTntGroupBox;
    StopButton: TTntButton;
    ProgressBar: TTntProgressBar;
    Lbl1: TTntLabel;
    TotalBytesLbl: TTntLabel;
    Lbl2: TTntLabel;
    ProcBytesLbl: TTntLabel;
    StatusLbl: TTntLabel;
    ProblemsMemo: TTntMemo;
    ProblemLabel: TTntLabel;
    CloseButton: TTntButton;
    BackupFileLbl: TTntLabel;
    BackupFileCaptionLbl: TTntLabel;
    TntLabel1: TTntLabel;
    TntLabel2: TTntLabel;
    ElapsedTimeLabel: TTntLabel;
    RemainingTimeLabel: TTntLabel;
    Timer1: TTimer;
    constructor Create(AOwner: TComponent);  override;
    procedure StopButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFirstTime: Boolean;
    FProblemsFound: Boolean;
    FLastBytesRead: Int64;
    FOriginalHeight: Integer;
    FPercentDone: Single;
    FStartTime: TDateTime;
  protected
    procedure CreateParams(Var Params: TCreateParams); override;
  public
    Stopping: Boolean;
    procedure UpdateProgress(bytes_read: Int64; bytes_total: Int64);
    procedure AddWarning(msg: WideString);
    procedure UpdateForm(op: TRestoreOperation; SqlFilename: WideString);
    procedure OperationFinished(msg: WideString; Failed: Boolean);
    procedure ResetProgress;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  WideStrings, DateUtils;

var
  ErrorAreaSize: Integer;

//----------------------------------------------------------------------------------------------------------------------

constructor TAdminRestoreProgressForm.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  InitForm(Self);
  ErrorAreaSize := StopButton.Top - ProblemLabel.Top;
  FOriginalHeight := Height - ErrorAreaSize;

  ResetProgress();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.OperationFinished(msg: WideString; Failed: Boolean);

begin
  Timer1.Enabled := False;
  StopButton.Visible := False;
  CloseButton.Visible := True;
  StatusLbl.Caption := msg;
  if Failed then
    StatusLbl.Font.Color := clRed
  else
    StatusLbl.Font.Color := clBlack;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.UpdateForm(op: TRestoreOperation; SqlFilename: WideString);

begin
  if op = TR_ANALYZE then
  begin
    Caption := Format(_('Analyzing %s'), [SqlFileName]);

    ProgressGBox.Caption := _('Analyzing');
    BackupFileLbl.Caption := SqlFileName;
  end
  else
  begin
    Caption := Format(_('Restoring %s'), [SqlFileName]);

    ProgressGBox.Caption := _('Restoring');
    BackupFileLbl.Caption := SqlFileName;
  end;

  ResetProgress();
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.UpdateProgress(bytes_read: Int64; bytes_total: Int64);

begin
  if FFirstTime then
  begin
    TotalBytesLbl.Caption := IntToStr(bytes_total);
    ProgressBar.Min := 0;
    ProgressBar.Max := 100;
    ProgressBar.Position := 0;
    FStartTime := Now;
    FFirstTime := False;
    Timer1.Enabled := True;
    Timer1Timer(nil);
  end;

  FPercentDone := bytes_read / bytes_total;

  ProgressBar.Position := Round(100 * FPercentDone);
  ProcBytesLbl.Caption := IntToStr(bytes_read);

  Update;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.ResetProgress;

begin
  Stopping := False;
  FFirstTime := True;
  FLastBytesRead := 0;
  ProblemsMemo.Clear;
  FProblemsFound := False;
  Constraints.MinHeight := 0;
  Constraints.MaxHeight := 0;
  Height := FOriginalHeight;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  ProblemsMemo.Visible := False;
  ProblemLabel.Visible := False;
  StopButton.Visible := True;
  CloseButton.Visible := False;
  StatusLbl.Caption := '';
  ElapsedTimeLabel.Caption := _('unknown');
  RemainingTimeLabel.Caption := _('unknown');
  FPercentDone := 0;
  Timer1.Enabled := False;
  ProgressBar.Position := 0;
  ProcBytesLbl.Caption := '0';
  TotalBytesLbl.Caption := '0';

  Update;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TadminRestoreProgressForm.AddWarning(msg: WideString);

var
  Strings: TWideStringList;
  
begin
  if not FProblemsFound then
  begin
    ProblemLabel.Visible := True;
    ProblemsMemo.Visible := True;

    Constraints.MinHeight := 0;
    Constraints.MaxHeight := 0;
    Height := Height + ErrorAreaSize;
    Constraints.MinHeight := Height;

    // Adjust height of the memo as it became 0 when we blended out the warnings reporting area.
    // However the vertical resize was larger than the height of the memo (because there is also a label)
    // so it will become larger when we show this part again. Make a one time adjustment to account for that.
    ProblemsMemo.Height := CloseButton.Top - ProblemsMemo.Top - 12; // -12 as a distance between lower memo border and button.

    FProblemsFound := True;
  end;

  Strings := TWideStringList.Create;
  try
    // Split several lines. The memo strings can only handle DOS line breaks, not Unix type breaks.
    Strings.Text := msg;
    ProblemsMemo.Lines.AddStrings(Strings);
  finally
    Strings.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.StopButtonClick(Sender: TObject);

begin
  StatusLbl.Caption := 'Stopping...';
  Stopping := True;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  SecsPerHour = SecsPerMin * MinsPerHour;

procedure TAdminRestoreProgressForm.Timer1Timer(Sender: TObject);

  //----------------------------------------------------------------------------

  function CreateTimeString(Days, Hours, Minutes, Seconds: Word): WideString;

  // Creates the minimal necessary time string out of the given values.
  
  begin
    Result := '';
    if Days = 1 then
      Result := _('1 day')
    else
      if Days > 0 then
        Result := WideFormat(_('%d days'), [Days]);
    if Hours > 0 then
    begin
      if Result <> '' then
        Result := Result + ', ';
      if Hours = 1 then
        Result := Result + _('1 hour')
      else
        Result := Result + WideFormat(_('%d hours'), [Hours]);
    end;
    if Minutes > 0 then
    begin
      if Result <> '' then
        Result := Result + ', ';
      if Minutes = 1 then
        Result := Result + _('1 minute')
      else
        Result := Result + WideFormat(_('%d minutes'), [Minutes]);
    end;
    if Seconds > 0 then
    begin
      if Result <> '' then
        Result := Result + ', ';
      if Seconds = 1 then
        Result := Result + _('1 second')
      else
        Result := Result + WideFormat(_('%d seconds'), [Seconds]);
    end;

    if Result = '' then
      Result := _('none');
  end;

  //----------------------------------------------------------------------------

var
  Elapsed: TDateTime;
  Remaining: TDateTime;
  Days: Word;
  Hours: Word;
  Minutes: Word;
  Seconds: Word;
  Milliseconds: Word;

begin
  Elapsed := Now - FStartTime;

  Days := DaysBetween(Now, FStartTime);
  DecodeTime(Elapsed, Hours, Minutes, Seconds, Milliseconds);
  ElapsedTimeLabel.Caption := CreateTimeString(Days, Hours, Minutes, Seconds);

  // Compute remaining time.
  if FPercentDone < 0.00001 then
    RemainingTimeLabel.Caption := _('unknown')
  else
  begin
    Remaining := (1 - FPercentDone) * Elapsed / FPercentDone;
    Days := Trunc(Remaining);
    DecodeTime(Remaining, Hours, Minutes, Seconds, Milliseconds);
    RemainingTimeLabel.Caption := CreateTimeString(Days, Hours, Minutes, Seconds);
  end;

  Update;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.TntFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  // Select error text if there is one and user pressed Ctrl+A.
  // Copy to clipboard with Ctrl+C.
  if Shift * [ssCtrl] <> []then
  begin
    case Key of
      Ord('A'):
        ProblemsMemo.SelectAll;
      Ord('C'):
        ProblemsMemo.CopyToClipboard;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.CloseButtonClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminRestoreProgressForm.CreateParams(var Params: TCreateParams);

begin
  Inherited CreateParams(Params);

  Params.exStyle := Params.exStyle or WS_EX_APPWINDOW;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
