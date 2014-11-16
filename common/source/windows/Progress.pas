unit Progress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, TntForms,
  Dialogs, ComCtrls,TntComCtrls, TntStdCtrls, AuxFuncs, StdCtrls, Forms;

type
  TProgressForm = class(TTntForm)
    ProgressGBox: TTntGroupBox;
    StoppingLbl: TTntLabel;
    StopBtn: TTntButton;
    ActionCaptionLbl: TTntLabel;
    ActionLbl: TTntLabel;
    InternalProgressBar: TTntProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    function GetMax: Integer;
    procedure SetMax(const Value: Integer);
  public
    Stopping: Boolean;
    procedure Advance(Amount: Integer; Relative: Boolean);
    procedure Reset;

    property Max: Integer read GetMax write SetMax;
  end;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  InitForm(self);
  
  Stopping:=False;
end;

procedure TProgressForm.FormDestroy(Sender: TObject);
begin
  //
end;

function TProgressForm.GetMax: Integer;

begin
  Result := InternalProgressbar.Max;
end;

procedure TProgressForm.Reset;

begin
  InternalProgressBar.Position := 0;
end;

procedure TProgressForm.Advance(Amount: Integer; Relative: Boolean);

var
  NewValue: Integer;
  
begin
  if Relative then
    NewValue := InternalProgressBar.Position + Amount
  else
    NewValue := Amount;

  if InternalProgressBar.Position <> NewValue then
  begin
    InternalProgressBar.Position := NewValue;
    InternalProgressBar.Update;
  end;
end;

procedure TProgressForm.SetMax(const Value: Integer);

begin
  InternalProgressbar.Max := Value;
end;

procedure TProgressForm.StopBtnClick(Sender: TObject);
begin
  StoppingLbl.Visible:=True;

  Stopping:=True;
end;

end.
