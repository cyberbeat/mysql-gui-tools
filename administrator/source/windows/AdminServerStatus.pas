unit AdminServerStatus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, AdditionalClasses;

type
  TAdminServerStatusForm = class(TDockedAdminForm)
    ServerStatusPnl: TTntPanel;
    BottomPnl: TTntPanel;
    BottomBtnPnl: TTntPanel;
    Button4: TTntButton;
    ServerStatusPageControl: TTntPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ListView1: TTntListView;
    ListView2: TTntListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ServerStatusPnlResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AdminServerStatusForm: TAdminServerStatusForm;

implementation

{$R *.dfm}

procedure TAdminServerStatusForm.FormCreate(Sender: TObject);
begin
  SetDockedPanel(ServerStatusPnl);

  ServerStatusPageControl.ActivePageIndex:=0;
end;

procedure TAdminServerStatusForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TAdminServerStatusForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TAdminServerStatusForm.ServerStatusPnlResize(
  Sender: TObject);
begin
  //Adjust Page Control
  ServerStatusPageControl.Width:=ServerStatusPnl.Width-591+571;
  ServerStatusPageControl.Height:=ServerStatusPnl.Height-484+429;
end;

end.
