unit About;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Dialogs, StdCtrls, PngImage, ExtCtrls, AuxFuncs, TntStdCtrls, TntExtCtrls, TntForms;

{$include Defines.ini}

type
  TAboutForm = class(TTntForm)
    AboutImg: TTntImage;
    VersionLbl: TTntLabel;
    CopyrightLbl: TTntLabel;
    LicenceLbl: TTntLabel;
    CloseBtn: TTntButton;
    TextLbl: TTntLabel;
    CreditsBtn: TTntButton;
    TntBevel1: TTntBevel;
    CloseTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure CreditsBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseTimerTimer(Sender: TObject);
  private
    { Private declarations }
    AboutPNGImg: TPNGObject;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  PNGTools;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  InitForm(self);

  AboutPNGImg:=LoadPNGImageFromResource('about', AboutImg);

  if (FileExists('MySQLEULA.txt')) then
    LicenceLbl.Caption:='This is commercial software and is distributed with NO WARRANTY OF ANY KIND.  No author or '+
      'distributor accepts any responsibility for the consequences of using it. '+
      'Refer to the MySQLEULA.txt file for details.';

  TextLbl.Caption:='Michael G. Zinner, main concept, graphical design, '+
    'Windows development, library coding | '+
    'Alfredo Kengi Kojima, Linux development, library coding | '+
    'Vladimir Kolesnikov, library coding | '+
    'Brian Aker, conceptual design, supervising | '+
    'Mike Hillyer, documentation';

  TextLbl.Visible:=False;
end;

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  AboutPNGImg.Free;
end;

procedure TAboutForm.CreateParams(var Params: TCreateParams);
const CS_DROPSHADOW = $00020000;
begin
  inherited;
  if(IsWinXP)then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TAboutForm.CreditsBtnClick(Sender: TObject);
begin
  if(Not(TextLbl.Visible))then
  begin
    CopyrightLbl.Visible:=False;
    LicenceLbl.Visible:=False;

    TextLbl.Visible:=True;

    CreditsBtn.Caption:=_('About');
  end
  else
  begin
    CopyrightLbl.Visible:=True;
    LicenceLbl.Visible:=True;

    TextLbl.Visible:=False;

    CreditsBtn.Caption:=_('Credits');
  end;
end;

procedure TAboutForm.CloseTimerTimer(Sender: TObject);
begin
  Close;
end;

end.
