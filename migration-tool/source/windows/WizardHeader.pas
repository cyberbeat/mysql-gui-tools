unit WizardHeader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, TntExtCtrls, StdCtrls, TntStdCtrls, ComCtrls,
  AuxFuncs, PngImage, PngTools;

type
  TWizardHeaderFrame = class(TFrame)
    HeaderInfoLbl: TTntLabel;
    UserInfoBevel: TTntBevel;
    TntShape1: TTntShape;
    HeaderTitleLbl: TTntLabel;
    Shape1: TTntShape;
    AnimPnl: TTntPanel;
    AnimStillImg: TTntImage;
    BusyAnimate: TAnimate;
    HeaderPnl: TTntPanel;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    { Private declarations }
    AnimStillPNGImg: TPngObject;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

constructor TWizardHeaderFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AnimStillPNGImg:=LoadPNGImageFromResource('sakila', AnimStillImg);
  BusyAnimate.ResName:='dolphin_loop';
end;

destructor TWizardHeaderFrame.Destroy;
begin
  AnimStillPNGImg.Free;

  inherited;
end;

end.
