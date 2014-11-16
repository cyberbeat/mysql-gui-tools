unit About;

interface

{$include MySQLQueryBrowser.ini}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ExtCtrls, AuxFuncs;

type
  TAboutForm = class(TTntForm)
    Image1: TTntImage;
    VersionLbl: TTntLabel;
    InfoMemo: TTntMemo;
    Label2: TTntLabel;
    LicenceLbl: TTntLabel;
    CloseBtn: TTntButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  InitForm(self);

{$ifdef commercial_build}
  LicenceLbl.Caption:='This is commercial software and is distributed with NO WARRANTY OF ANY KIND.  No author or '+
    'distributor accepts any responsibility for the consequences of using it. '+
    'Refer to the MySQLEULA.txt file for details.';
{$endif}
end;

end.
