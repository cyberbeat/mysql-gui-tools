unit WizardBottom;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls;

type
  TWizardBottomFrame = class(TFrame)
    BottomPnl: TTntPanel;
    BackBtn: TTntButton;
    NextBtn: TTntButton;
    CancelBtn: TTntButton;
    TntBevel1: TTntBevel;
    DetailsBtn: TTntButton;
    procedure NextBtnClick(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
  private
    { Private declarations }
    FOnNextBtnClick,
    FOnBackBtnClick,
    FOnCancelBtnClick,
    FOnDetailsBtnClick: TNotifyEvent;

    FDetailsActivated: Boolean;

    procedure SetDetailsBtnVisibleState(Visible: Boolean);
    procedure SetDetailsState(State: Boolean);
  public
    { Public declarations }
    property OnNextBtnClick: TNotifyEvent read FOnNextBtnClick write FOnNextBtnClick;
    property OnBackBtnClick: TNotifyEvent read FOnBackBtnClick write FOnBackBtnClick;
    property OnCancelBtnClick: TNotifyEvent read FOnCancelBtnClick write FOnCancelBtnClick;

    property OnDetailsBtnClick: TNotifyEvent read FOnDetailsBtnClick write FOnDetailsBtnClick;
    property DetailsBtnVisible: Boolean write SetDetailsBtnVisibleState;
    property DetailsActivated: Boolean read FDetailsActivated write SetDetailsState;
  end;

implementation

{$R *.dfm}

procedure TWizardBottomFrame.NextBtnClick(Sender: TObject);
begin
  if(Assigned(FOnNextBtnClick))then
    FOnNextBtnClick(Sender);
end;

procedure TWizardBottomFrame.BackBtnClick(Sender: TObject);
begin
  if(Assigned(FOnBackBtnClick))then
    FOnBackBtnClick(Sender);
end;

procedure TWizardBottomFrame.CancelBtnClick(Sender: TObject);
begin
  if(Assigned(FOnCancelBtnClick))then
    FOnCancelBtnClick(Sender);
end;

procedure TWizardBottomFrame.DetailsBtnClick(Sender: TObject);
begin
  DetailsActivated:=Not(DetailsActivated);

  if(Assigned(FOnDetailsBtnClick))then
    FOnDetailsBtnClick(Sender);
end;

procedure TWizardBottomFrame.SetDetailsBtnVisibleState(Visible: Boolean);
begin
  if(Visible)then
  begin
    DetailsBtn.Caption:=_('Advanced >>');
    FDetailsActivated:=False;
    DetailsBtn.Show;
  end
  else
  begin
    DetailsBtn.Hide;
  end;
end;

procedure TWizardBottomFrame.SetDetailsState(State: Boolean);
begin
  FDetailsActivated:=State;

  if(Not(FDetailsActivated))then
    DetailsBtn.Caption:=_('Advanced >>')
  else
    DetailsBtn.Caption:=_('<< Advanced');
end;

end.
