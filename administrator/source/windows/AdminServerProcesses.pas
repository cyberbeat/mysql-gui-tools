unit AdminServerProcesses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, AdditionalClasses;

type
  TAdminServerProcessesForm = class(TDockedAdminForm)
    ServerProcessesPnl: TTntPanel;
    BottomPnl: TTntPanel;
    BottomBtnPnl: TTntPanel;
    Button4: TTntButton;
    ListView1: TTntListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdminServerProcessesForm: TAdminServerProcessesForm;

implementation

{$R *.dfm}

procedure TAdminServerProcessesForm.FormCreate(Sender: TObject);
begin
  SetDockedPanel(ServerProcessesPnl);
end;

procedure TAdminServerProcessesForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TAdminServerProcessesForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

end.
