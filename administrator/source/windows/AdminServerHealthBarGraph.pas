unit AdminServerHealthBarGraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, AuxFuncs, StdCtrls, ExtCtrls, PNGImage, TntForms, TntStdCtrls,
  TntExtCtrls, myx_admin_public_interface;

type
  TAdminServerHealthBarGraphFrame = class(TTntFrame)
    ValueImg: TTntImage;
    MainPnl: TTntPanel;
    EmptyImg: TTntImage;
    ValueLbl: TTntLabel;
    MaxLbl: TTntLabel;
    CaptionPnl: TTntPanel;
    CaptionLbl: TTntLabel;

    constructor Create(AOwner: TComponent;
      GraphCaption: WideString = '';
      DisplayGraphCaption: integer = 0;
      ValueCaption: WideString = '';
      MaxCaption: WideString = '';
      MinValue: Double = 0; MaxValue: Double = 100;
      ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT = MYX_HGVU_COUNT); reintroduce;
    destructor Destroy; override;
    procedure SetValue(value: double);
    procedure SetMaxValue(MaxValue: double);
    procedure FrameResize(Sender: TObject);
    procedure TntFrameDblClick(Sender: TObject);
    function GetValueCaption(ValueCaption: WideString; value: double): WideString;
  private
    FEmptyPNGImg,
    FFilledPNGImg: TPNGObject;
    FMinValue: Double;
    FMaxValue: Double;
    FValue: Double;
  public
    ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT;
    ValueCaption,
    MaxCaption: WideString;
  end;

implementation

{$R *.dfm}

uses
  PNGTools;
  
constructor TAdminServerHealthBarGraphFrame.Create(AOwner: TComponent;
  GraphCaption: WideString;
  DisplayGraphCaption: integer;
  ValueCaption: WideString;
  MaxCaption: WideString;
  MinValue: Double; MaxValue: Double;
  ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT);
begin
  inherited Create(AOwner);

  FEmptyPNGImg := LoadPNGImageFromResource('health_bar_empty', EmptyImg);
  FFilledPNGImg := LoadPNGImageFromResource('health_bar_filled', ValueImg);

  ValueImg.Width:=0;

  self.ValueCaption:=ValueCaption;
  self.MaxCaption:=MaxCaption;

  ValueLbl.Caption:='';
  MaxLbl.Caption:='';

  FMinValue:=MinValue;
  FMaxValue:=MaxValue;

  self.ValueUnit:=ValueUnit;

  CaptionLbl.Caption:=GraphCaption+':';

  if(DisplayGraphCaption=0)then
    CaptionPnl.Width:=0;
end;

destructor TAdminServerHealthBarGraphFrame.Destroy;
begin
  FEmptyPNGImg.Free;
  FFilledPNGImg.Free;

  inherited Destroy;
end;

procedure TAdminServerHealthBarGraphFrame.SetValue(value: double);
begin
  if value > FMaxValue then
    value := FMaxValue;

  ValueLbl.Caption:=GetValueCaption(ValueCaption, Value);
  ValueImg.Width := Round((value - FMinValue) / (FMaxValue - FMinValue) * EmptyImg.Width);
  FValue := value;
end;

function TAdminServerHealthBarGraphFrame.GetValueCaption(ValueCaption: WideString; value: double): WideString;
var s: WideString;
begin
  case ValueUnit of
    MYX_HGVU_PERCENTAGE:
      if(Trunc(value)=value)then
        s:=ValueCaption+' '+FormatFloat('##0', value)+' %'
      else
        s:=ValueCaption+' '+FormatFloat('##0.0', value)+' %';
    MYX_HGVU_COUNT:
      if(Trunc(value)=value)then
        s:=ValueCaption+' '+FormatFloat('###,###,##0', value)
      else
        s:=ValueCaption+' '+FormatFloat('###,###,##0.00', value);
    MYX_HGVU_BYTE:
      s:=ValueCaption+' '+FormatFileSize(value);
    MYX_HGVU_SECONDS:
      if(Trunc(value)=value)then
        s:=ValueCaption+' '+FormatFloat('###,###,##0', value)+' s'
      else
        s:=ValueCaption+' '+FormatFloat('###,###,##0.00', value)+' s';
  end;

  Result:=s;
end;

procedure TAdminServerHealthBarGraphFrame.SetMaxValue(MaxValue: double);
begin
  MaxLbl.Caption:=GetValueCaption(MaxCaption, MaxValue);

  FMaxValue:=MaxValue;
  if FMaxValue = 0 then
    FMaxValue := 1;

  SetValue(FValue);
end;

procedure TAdminServerHealthBarGraphFrame.FrameResize(Sender: TObject);
begin
  SetValue(FValue);
end;

procedure TAdminServerHealthBarGraphFrame.TntFrameDblClick(
  Sender: TObject);
begin
  if(Assigned(OnDblClick))then
    OnDblClick(self);
end;

end.
