unit AdminBackupProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, AuxFuncs, TntForms, TntStdCtrls, TntComCtrls,
  gnugettext;

type
  TAdminBackupProgressForm = class(TTntForm)
    ProgressGBox: TTntGroupBox;
    StopBtn: TTntButton;
    TablesProgressBar: TTntProgressBar;
    Label1: TTntLabel;
    TablesLbl: TTntLabel;
    Label2: TTntLabel;
    Label3: TTntLabel;
    CurrentTableLbl: TTntLabel;
    CurrentTableNameLbl: TTntLabel;
    Label4: TTntLabel;
    TotalRowsLbl: TTntLabel;
    RowProgressBar: TTntProgressBar;
    Label6: TTntLabel;
    CurrentRowLbl: TTntLabel;
    ProjectCaptionLbl: TTntLabel;
    ProjectLbl: TTntLabel;
    StoppingLbl: TTntLabel;
    constructor Create(AOwner: TComponent; UseForBackup: Boolean;
      ProjectName: WideString); reintroduce;
    procedure StopBtnClick(Sender: TObject);
  protected
    procedure CreateParams(Var Params: TCreateParams); override;
  private
    { Private declarations }
  public
    { Public declarations }
    Stopping: Boolean;
  end;

implementation

{$R *.dfm}

constructor TAdminBackupProgressForm.Create(AOwner: TComponent;
  UseForBackup: Boolean; ProjectName: WideString);
begin
  inherited Create(AOwner);

  InitForm(self);

  if(Not(UseForBackup))then
  begin
    Caption:=_('Restore Progress ...');
    ProgressGBox.Caption:=_('Restore Progress');
    ProjectCaptionLbl.Caption:=_('Restore Project:');
  end;

  ProjectLbl.Caption:=ProjectName;

  Stopping:=False;
end;

procedure TAdminBackupProgressForm.StopBtnClick(Sender: TObject);
begin
  StoppingLbl.Visible:=True;

  Stopping:=True;
end;

procedure TAdminBackupProgressForm.CreateParams(var Params: TCreateParams);
begin
  Inherited CreateParams(Params);

  Params.exStyle:=Params.exStyle or WS_EX_APPWINDOW;
end;

end.
