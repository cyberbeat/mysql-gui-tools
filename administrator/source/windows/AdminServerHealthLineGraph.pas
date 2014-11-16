unit AdminServerHealthLineGraph;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PNGImage, AuxFuncs, TntForms, TntExtCtrls,
  TntStdCtrls, myx_admin_public_interface;

type
  THealthGraph = class;

  TGraphFrameBundle = class(TObject)
    Graph: TMYX_HEALTH_GRAPH;
    GraphFrame: TObject;
    value_formular: WideString;
    max_formular: WideString;

    constructor Create(Graph: TMYX_HEALTH_GRAPH;
      GraphFrame: TObject);
  end;

  TAdminServerHealthLineGraphFrame = class(TTntFrame)
    QueryCacheHitrateBarPnl: TTntPanel;
    BarImg: TTntImage;
    BarLbl: TTntLabel;
    BarValueImg: TTntImage;
    QryCacheHitratePnl: TTntPanel;
    HealthGraphImg: TTntImage;
    QryCacheTotalSizeLbl: TTntLabel;
    SpacerPnl: TTntPanel;
    DetailPnl: TTntPanel;
    DetailBG1Img: TTntImage;
    DetailBG2Img: TTntImage;
    DetailBG3Img: TTntImage;
    DetailBG4Img: TTntImage;
    CurrentCaptionLbl: TTntLabel;
    CurrentLbl: TTntLabel;
    MinCaptionLbl: TTntLabel;
    MinLbl: TTntLabel;
    MaxLbl: TTntLabel;
    MaxCaptionLbl: TTntLabel;
    AvgCaptionLbl: TTntLabel;
    AvgLbl: TTntLabel;
    PeakLeftShape: TTntShape;
    PeakRightShape: TTntShape;

    constructor Create(Owner: TComponent; graph: TMYX_HEALTH_GRAPH; BarCaption: WideString = 'Hitrate';
      MinValue: Double = 0; MaxValue: Double = 100; ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT = MYX_HGVU_COUNT); reintroduce;
    destructor Destroy; override;

    procedure SetValue(value: Double);
    procedure SetMaxValue(value: Double);
    procedure FrameResize(Sender: TObject);
    procedure SetBarCaption(value: Double);
    procedure HealthGraphImgDblClick(Sender: TObject);
  private
    BarCaption: WideString;
    DetailBGPNGImg: TPNGObject;
    graph: TMYX_HEALTH_GRAPH;
    BarPeak, BarPeakResetCounter: Integer;
  public
    ProgressGraph: THealthGraph;
    ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT;
  end;

  THealthGraph = class(TBitmap)
  private
    FCurrentPos: Integer;
    FValues: array[0..1280] of Double;
    FMinValue: Double;
    FMaxValue: Double;
    FCurrentValue: Integer;
    FValuesWraped: Boolean;

    FTotalValSum: Double;
    FNumberOfVal: Integer;
    FCurrentMinVal: Double;
    FCurrentMaxVal: Double;
  public
    constructor Create(width, height: Integer; MinValue, MaxValue: Double); reintroduce; overload;

    function MoveToNextValue(value: Double): Integer;
    procedure DrawProgressGraphTo(theCanvas: TCanvas; x, y, w, h: Integer);
    procedure SetNewMaxMin(MinValue, MaxValue: Double);
    procedure ClearGraph;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ApplicationDataModule, PNGTools;

{$R *.dfm}

const
  HealthGraphGridColor  = $00801000;
  HealthGraphValueColor = $00FFA000;
  
//----------------- TGraphFrameBundle ----------------------------------------------------------------------------------

constructor TGraphFrameBundle.Create(Graph: TMYX_HEALTH_GRAPH; GraphFrame: TObject);

begin
  self.Graph := Graph;
  self.GraphFrame := GraphFrame;

  self.value_formular := Graph.value_formula;
  self.max_formular := Graph.max_formula;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor THealthGraph.Create(width, height: Integer; MinValue, MaxValue: Double);

begin
  inherited Create;

  self.Width := width + 2;
  self.Height := height;

  FMinValue := MinValue;
  FMaxValue := MaxValue;
  if (FMaxValue = 0) then
    FMaxValue := 1;

  FCurrentPos := 0;
  FCurrentValue := -1;
  FValuesWraped := False;

  FTotalValSum := 0;
  FNumberOfVal := 0;
  FCurrentMinVal := 0;
  FCurrentMaxVal := 0;

  ClearGraph;
end;

//----------------------------------------------------------------------------------------------------------------------

function THealthGraph.MoveToNextValue(value: Double): Integer;

var
  ypos: Integer;
  I: Integer;

begin
  Inc(FCurrentValue);
  if (FCurrentValue >= 1280) then
  begin
    FCurrentValue := 0;
    FValuesWraped := True;
  end;

  if (value < FMinValue) then
    value := FMinValue
  else
    if (value > FMaxValue) then
      value := FMaxValue;

  FValues[FCurrentValue] := value;

  // Move existing values to the left and initialize the invalidated space.
  Canvas.CopyRect(Rect(0, 0, Width - 2, Height), Canvas, Rect(2, 0, Width, Height));
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(Width - 2, 0, Width, Height));
  Canvas.Pen.Color := HealthGraphGridColor;
  I := Height - 1;
  while I >= 0 do
  begin
    Canvas.MoveTo(Width - 2, I);
    Canvas.LineTo(Width, I);
    Dec(I, 12);
  end;

  if (FCurrentValue mod 6) = 0 then
  begin
    Canvas.MoveTo(Width - 3, 0);
    Canvas.LineTo(Width - 3, Height - 1);
  end;

  ypos := Round(((value - FMinValue) / FMaxValue) * (Height - 3));

  Canvas.Pen.Color := HealthGraphValueColor;
  Canvas.MoveTo(Width - 7, Height - 2 - FCurrentPos);
  Canvas.LineTo(Width - 5, Height - 2 - ypos);

  FCurrentPos := ypos;

  FTotalValSum := FTotalValSum + value;
  Inc(FNumberOfVal);
  if (value < FCurrentMinVal) then
    FCurrentMinVal := value;
  if (value > FCurrentMaxVal) then
    FCurrentMaxVal := value;

  Result := Round(((value - FMinValue) / FMaxValue) * 100);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THealthGraph.DrawProgressGraphTo(theCanvas: TCanvas; x, y, w, h: Integer);

begin
  theCanvas.CopyRect(Rect(0, 0, w - 1, h - 1),
    Canvas,
    Rect(Width - w - 2, Height - h, Width - 2 - 2, Height - 1));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THealthGraph.ClearGraph;

var
  I: Integer;

begin
  Canvas.Brush.Color := clBlack;
  Canvas.FillRect(Rect(0, 0, Self.Width - 1, Self.Height - 1));

  Canvas.Pen.Color := HealthGraphGridColor;
  I := Width - 1;
  while I >= 0 do
  begin
    Canvas.MoveTo(I, 0);
    Canvas.LineTo(I, Height);
    Dec(I, 12);
  end;
  I := Height - 1;
  while I >= 0 do
  begin
    Canvas.MoveTo(0, I);
    Canvas.LineTo(Width, I);
    Dec(I, 12);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THealthGraph.SetNewMaxMin(MinValue, MaxValue: Double);

var
  i, xpos, ypos: Integer;

begin
  FMinValue := MinValue;
  FMaxValue := MaxValue;
  if FMaxValue = 0 then
    FMaxValue := 1;

  ClearGraph;

  //Redraw scaled values
  FCurrentPos := Round(((FValues[FCurrentValue] - FMinValue) / FMaxValue) * (Height - 3));

  Canvas.Pen.Color := HealthGraphValueColor;
  Canvas.MoveTo(Width - 5, Height - 2 - FCurrentPos);

  xpos := Width - 7;
  for i := FCurrentValue - 1 downto 0 do
  begin
    ypos := Round(((FValues[i] - FMinValue) / FMaxValue) * (Height - 3));

    Canvas.LineTo(xpos, Height - 2 - ypos);

    dec(xpos, 2);
    if (xpos <= 0) then
      break;
  end;

  if (FValuesWraped) and (xpos > 0) then
    for i := 1280 - 1 downto FCurrentValue + 1 do
    begin
      ypos := Round(((FValues[i] - FMinValue) / FMaxValue) * (Height - 3));

      Canvas.LineTo(xpos, Height - 2 - ypos);

      Dec(xpos, 2);
      if (xpos <= 0) then
        break;
    end;
end;

//----------------- TAdminServerHealthLineGraphFrame -------------------------------------------------------------------

constructor TAdminServerHealthLineGraphFrame.Create(Owner: TComponent;
  graph: TMYX_HEALTH_GRAPH;
  BarCaption: WideString;
  MinValue: Double; MaxValue: Double;
  ValueUnit: MYX_HEALTH_GRAPH_VALUE_UNIT);

begin
  inherited Create(Owner);

  QryCacheHitratePnl.FullRepaint := False;

  DetailBGPNGImg := LoadPNGImageFromResource('health_detailbg', DetailBG1Img);
  DetailBG2Img.Picture.Assign(DetailBGPNGImg);
  DetailBG3Img.Picture.Assign(DetailBGPNGImg);
  DetailBG4Img.Picture.Assign(DetailBGPNGImg);

  ProgressGraph := THealthGraph.Create(1028, 57,
    MinValue, MaxValue);

  FrameResize(self);

  self.BarCaption := BarCaption;
  self.graph := graph;
  self.ValueUnit := ValueUnit;

  BarPeak := 0;
  //if the PeakLevel shall never be reseted, start with 1
  BarPeakResetCounter := Ord(not (ApplicationDM.Options.ResetPeakLevel));

  if (not (ApplicationDM.Options.UsePeakLevel)) then
  begin
    PeakLeftShape.Visible := False;
    PeakRightShape.Visible := False;
  end;

  SetBarCaption(0);

  if (Owner.InheritsFrom(TWinControl)) then
    Parent := TWinControl(Owner);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TAdminServerHealthLineGraphFrame.Destroy;

begin
  ProgressGraph.Free;
  DetailBGPNGImg.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthLineGraphFrame.SetMaxValue(value: Double);

begin
  if (graph.autoextend_max = 0) and (ProgressGraph.FMaxValue <> value) then
    ProgressGraph.SetNewMaxMin(ProgressGraph.FMinValue, value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthLineGraphFrame.SetValue(value: Double);

var
  BarValue: Integer;

begin
  if (graph.autoextend_max = 1) and (ProgressGraph.FMaxValue < value) then
    ProgressGraph.SetNewMaxMin(ProgressGraph.FMinValue, value);

  BarValue := ProgressGraph.MoveToNextValue(value);

  if (ApplicationDM.Options.UsePeakLevel) then
  begin
    if (not (PeakLeftShape.Visible)) then
    begin
      PeakLeftShape.Visible := True;
      PeakRightShape.Visible := True;
    end;

    //Peak level
    if (BarValue > BarPeak) or (BarPeakResetCounter = 0) then
    begin
      BarPeak := BarValue;

      PeakLeftShape.Top := Round(37 - BarPeak / 100 * 29);
      PeakRightShape.Top := Round(37 - BarPeak / 100 * 29);
    end;

    if (ApplicationDM.Options.ResetPeakLevel) then
    begin
      Inc(BarPeakResetCounter);
      if (BarPeakResetCounter >= ApplicationDM.Options.PeakLevelResetTicks) then
        BarPeakResetCounter := 0;
    end;
  end
  else
  begin
    if (PeakLeftShape.Visible) then
    begin
      PeakLeftShape.Visible := False;
      PeakRightShape.Visible := False;
    end;
  end;

  //Set Bar Graph
  BarValueImg.Height := Round(29 - BarValue / 100 * 29);

  if (ValueUnit = MYX_HGVU_PERCENTAGE) then
    SetBarCaption(BarValue)
  else
    SetBarCaption(value);

  CurrentLbl.Caption := FormatFloat('###,###,##0', value);
  MinLbl.Caption := FormatFloat('###,###,##0', ProgressGraph.FCurrentMinVal);
  MaxLbl.Caption := FormatFloat('###,###,##0', ProgressGraph.FCurrentMaxVal);
  AvgLbl.Caption := FormatFloat('###,###,##0', ProgressGraph.FTotalValSum / ProgressGraph.FNumberOfVal);

  ProgressGraph.DrawProgressGraphTo(
    HealthGraphImg.Picture.Bitmap.Canvas,
    0, 0, HealthGraphImg.Width, HealthGraphImg.Height);
  HealthGraphImg.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthLineGraphFrame.SetBarCaption(value: Double);

var
  s: WideString;

begin
  case ValueUnit of
    MYX_HGVU_PERCENTAGE:
      if (Trunc(value) = value) then
        s := BarCaption + ' ' + FormatFloat('##0', value) + ' %'
      else
        s := BarCaption + ' ' + FormatFloat('##0.0', value) + ' %';
    MYX_HGVU_COUNT:
      if (Trunc(value) = value) then
        s := BarCaption + ' ' + FormatFloat('###,###,##0', value)
      else
        s := BarCaption + ' ' + FormatFloat('###,###,##0.00', value);
    MYX_HGVU_BYTE:
      s := FormatFileSize(value);
    MYX_HGVU_SECONDS:
      if (Trunc(value) = value) then
        s := BarCaption + ' ' + FormatFloat('###,###,##0', value) + ' s'
      else
        s := BarCaption + ' ' + FormatFloat('###,###,##0.00', value) + ' s';
  end;

  BarLbl.Caption := s;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthLineGraphFrame.FrameResize(Sender: TObject);

begin
  HealthGraphImg.Picture.Bitmap.Width := HealthGraphImg.Width;
  HealthGraphImg.Picture.Bitmap.Height := HealthGraphImg.Height;

  ProgressGraph.DrawProgressGraphTo(
    HealthGraphImg.Picture.Bitmap.Canvas,
    0, 0, HealthGraphImg.Width, HealthGraphImg.Height);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerHealthLineGraphFrame.HealthGraphImgDblClick(Sender: TObject);

begin
  if (Assigned(OnDblClick)) then
    OnDblClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

