{
	Delphi Scintilla Interface Components
	Copyright (C) 2004,2005 Jan Martin Pettersen (hdalis)

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later
	version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free
	Software Foundatifon, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
	Thanx to deusi for contributing the basic code for printing a scintilla
	control, and to both he and kiriakos vlahos for suggesting printer support.
	
	This component is still rather basic however, and not very much tested.
}
{
	Author: Jan Martin Pettersen (hdalis)
	History: 28/05/2005 Initial release
}
{$Include SciCommonDef.Inc}
unit sciPrint;
interface
uses Classes,SciLexer,Graphics,Windows,Dialogs,SciSupport;

type
	scipType=(scipHeader,scipFooter);
	scipColorMode=(sccmNormal,sccmInvertLight,sccmBlackOnWhite,sccmColorOnWhite,sccmColorOnWhiteDefaultBG);
	TSCEvent_margintext = procedure(Sender : TObject; FCurrentPageNumber : Integer;hType : scipType;var TextToPrint : String) of object;
{$Ifndef COMPILER7_UP}
	TPageMeasureUnits = (pmDefault, pmMillimeters, pmInches);
{$Endif}
  TSciPrinter=class(TComponent)
  private
    FCurrentPageNumber: Integer;

    FEditor : TScintillaBase;
    FOnBeforeNextPage : TNotifyEvent;
    FHeaderFont,FFooterFont : TFont;
    FMarginBottom,FMarginTop,FMarginLeft,FMarginRight : Integer;
    FFromPage,FToPage : Integer;
    FUnits : TPageMeasureUnits;
    FPrintRange : TPrintRange;
    FTitle : String;
    FPrintPagenumber : Boolean;
    FOnMarginText : TSCEvent_margintext;
    FOnStartPrinting,
    FOnDonePrinting : TNotifyEvent;
    FColorMode : scipColorMode;
    FMagnification : Integer;
    FWordWrap : TWordWrapType;
    procedure SetEditor(Value : TScintillaBase);
    procedure SetHeaderFont(Value : TFont);
    procedure SetFooterFont(Value : TFont);
    function GetNumPages : Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    procedure DrawMarginText(Canvas : TCanvas;margin : scipType;r : TRect);
    procedure GetSettingsFor(var fmt : TRangeToFormat);
    procedure DoPrint;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    {Readonly property containing the current pagenumber. Or -1 if not printing.}
    property CurrentPageNumber : Integer read FCurrentPageNumber;
    procedure Print;
    property NumberOfPages : Integer read GetNumPages;
  published
    property Editor : TScintillaBase read FEditor write SetEditor;
    property OnBeforeNextPage : TNotifyEvent read FOnBeforeNextPage write FOnBeforeNextPage;
    property MarginTop : Integer read FMarginTop write FMarginTop default 1500;
    property MarginBottom : Integer read FMarginBottom write FMarginBottom default 1500;
    property MarginLeft : Integer read FMarginLeft write FMarginLeft default 1500;
    property MarginRight : Integer read FMarginRight write FMarginRight default 1500;
    property Units: TPageMeasureUnits read FUnits write FUnits default pmDefault;
    property PrintRange : TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property FromPage : Integer read FFromPage write FFromPage default 0;
    property ToPage : Integer read FToPage write FToPage default 0;
    property Title : String read FTitle write FTitle;
    property HeaderFont : TFont read FHeaderFont write SetHeaderFont;
    property FooterFont : TFont read FFooterFont write SetFooterFont;

    property PrintPageNumber : Boolean read FPrintPageNumber write FPrintPageNumber default True;
    property ColorMode : scipColorMode read FColorMode write FColorMode;
    property WordWrap : TWordWrapType read FWordWrap write FWordWrap;
    property Magnification : Integer read FMagnification write FMagnification;
    property OnMarginText : TSCEvent_margintext read FOnMarginText write FOnMarginText;
    property OnStartPrinting : TNotifyEvent read FOnStartPrinting write FOnStartPrinting;
    property OnDonePrinting : TNotifyEvent read FOnDonePrinting write FOnDonePrinting;
  end;

{Sets all pen settings, and then draws a line} 
procedure DrawPenLine(Canvas : TCanvas;startX,startY : Integer;endX : Integer=-1;endY : Integer=-1;PenWidth : Integer=1;PenColor : TColor=clBlack;PenStyle : TPenStyle=psSolid;PenMode : TPenMode=pmCopy);
implementation
uses Math,Printers,SysUtils,sciResLang;

constructor TSciPrinter.Create(AOwner : TComponent);
begin
  FEditor:=nil;
  FMarginLeft := 1500;
  FMarginTop := 1500;
  FMarginRight := 1500;
  FMarginBottom := 1500;
  FCurrentPageNumber:=-1;
  FFromPage:=0;
  FToPage:=0;
  FTitle:='DelphiSci';
  FMagnification:=0;
  FColorMode:=sccmNormal;
  FPrintPageNumber :=True;
  FPrintRange:=prAllPages;
  FFooterFont:=TFont.Create;
  FHeaderFont:=TFont.Create;
  FHeaderFont.Name:='Arial';
  FHeaderFont.Color:=clBlack;
  FHeaderFont.Size:=8;
  FFooterFont.Assign(FHeaderFont);
  inherited;
end;
destructor TSciPrinter.Destroy;
begin
  if assigned(FHeaderFont) then FHeaderFont.Free;
  if assigned(FFooterFont) then FFooterFont.Free;
  inherited;
end;
procedure TSciPrinter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FEditor) then
    FEditor:=nil;
  inherited;
end;

procedure DrawPenLine(Canvas : TCanvas;startX,startY : Integer;endX : Integer;endY : Integer;PenWidth : Integer;PenColor : TColor;PenStyle : TPenStyle;PenMode : TPenMode);
begin
  if endX=-1 then endX:=startX;
  if endY=-1 then endY:=startY;
  Canvas.Pen.Color:=PenColor;
  Canvas.Pen.Mode:=PenMode;
  Canvas.Pen.Width:=PenWidth;
  Canvas.Pen.Style:=PenStyle;
  Canvas.MoveTo(startX, startY);
  Canvas.LineTo(endX, endY);
end;

procedure TSciPrinter.DrawMarginText(Canvas : TCanvas;margin : scipType;r : TRect);
var
  ta: Integer;
  rcw: TRect;
  headerLineHeight: Integer;
  footerLineHeight: Integer;
  s:String;
begin
  Canvas.Brush.Style:=bsClear;
  case margin of
    scipHeader:
      begin
        Canvas.Font:=FHeaderFont;
        headerLineHeight:=Canvas.TextHeight('W');
        ta := SetTextAlign(Canvas.Handle, TA_BOTTOM);
        rcw := Rect(r.left,r.top - headerLineHeight - (headerLineHeight div 2),r.right,r.top - (headerLineHeight div 2));
        rcw.bottom := rcw.top + headerLineHeight;
        if assigned(FOnMarginText) then
        begin
          s:='';
          FOnMarginText(Self,FCurrentPageNumber,scipHeader,s);
          if s<>'' then
            Canvas.TextRect(rcw,r.left,r.top - (headerLineHeight div 2),s);
        end;
        if FPrintPageNumber then
        begin
          s := Format(sPageNo, [FCurrentPageNumber]);
          Canvas.TextRect(rcw,r.Right - Canvas.TextWidth(s),r.top - (headerLineHeight div 2),s);
        end;
        SetTextAlign(Canvas.Handle, ta);
        DrawPenLine(Canvas,r.left,r.top - headerLineHeight div 4,r.right,-1,2);
      end;
    scipFooter:
      begin
        footerLineHeight:=Canvas.TextHeight('W');
        Canvas.Font:=FFooterFont;
        ta := SetTextAlign(Canvas.Handle, TA_TOP);
        rcw := Rect(r.left, r.bottom + (footerLineHeight div 2), r.right, r.bottom + footerLineHeight + (footerLineHeight div 2));
        if assigned(FOnMarginText) then
        begin
          s:='';
          FOnMarginText(Self,FCurrentPageNumber,scipFooter,s);
          if s<>'' then
          Canvas.TextRect(rcw,r.left,r.bottom + (footerLineHeight div 2),s);
        end;
        if FPrintPageNumber then
        begin
          s := Format(sPageNo, [FCurrentPageNumber]);
          Canvas.TextRect(rcw,r.Right - Canvas.TextWidth(s),
                                    r.bottom + (footerLineHeight div 2),s);
        end;
        SetTextAlign(Canvas.Handle, ta);
        DrawPenLine(Canvas,r.left,r.bottom + footerLineHeight div 4,r.right,-1,2);
      end;
  end;
end;

{ GetSettingsFor:
  Set both rectangles based on the device capabilities and the margins specified.
  Input: TRangeToFormat with AT LEAST a valid hdcTarget member.
  On return: Both rc and rcTarget are filled with valid data. No other members are touched.
}
procedure TSciPrinter.GetSettingsFor(var fmt : TRangeToFormat);
var
  dc: HDC;
  ptDpi, ptPage: TPoint;
  rectMargins, rectSetup,
  rectPhysMargins: TRect;
  locinfo : array[0..3] of Char;
begin
  dc:=fmt.hdcTarget;
  ptDpi.X := GetDeviceCaps(dc, LOGPIXELSX);
  ptDpi.Y := GetDeviceCaps(dc, LOGPIXELSY);
  ptPage.X := GetDeviceCaps(dc, PHYSICALWIDTH);
  ptPage.Y := GetDeviceCaps(dc, PHYSICALHEIGHT);
  rectPhysMargins.Left := GetDeviceCaps(dc,PHYSICALOFFSETX);
  rectPhysMargins.Top := GetDeviceCaps(dc,PHYSICALOFFSETY);
  rectPhysMargins.Right := ptPage.X - GetDeviceCaps(dc,HORZRES) - rectPhysMargins.Left;
  rectPhysMargins.Bottom := ptPage.Y - GetDeviceCaps(dc, VERTRES) - rectPhysMargins.Top;
  if FUnits=pmDefault then
    GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, locinfo, 3);
  if (FUnits=pmInches) or ((FUnits=pmDefault) and (locinfo[0]='0')) then
  begin
    rectSetup.left := MulDiv (FMarginLeft, ptDpi.x,2540);
    rectSetup.top := MulDiv (FMarginTop,ptDpi.y, 2540);
    rectSetup.right	:= MulDiv(FMarginRight, ptDpi.x, 2540);
    rectSetup.bottom	:= MulDiv(FMarginBottom, ptDpi.y, 2540);
  end else
  begin
    rectSetup.left := MulDiv (FMarginLeft, ptDpi.x,1000);
    rectSetup.top := MulDiv (FMarginTop,ptDpi.y, 1000);
    rectSetup.right	:= MulDiv(FMarginRight, ptDpi.x, 1000);
    rectSetup.bottom	:= MulDiv(FMarginBottom, ptDpi.y, 1000);
  end;
  rectMargins.left	:= Max(rectPhysMargins.left,rectSetup.left);
  rectMargins.top	:= Max(rectPhysMargins.top, rectSetup.top);
  rectMargins.right	:= Max(rectPhysMargins.right, rectSetup.right);
  rectMargins.bottom	:= Max(rectPhysMargins.bottom, rectSetup.bottom);

  DPtoLP(dc, rectMargins, 2);
  DPtoLP(dc, rectPhysMargins, 2);
  DPtoLP(dc, ptPage, 1);

  fmt.rc.Left := rectMargins.left - rectPhysMargins.left;
  fmt.rc.top := rectMargins.top - rectPhysMargins.top;
  fmt.rc.right := ptPage.x - rectMargins.right -rectPhysMargins.left;
  fmt.rc.bottom := ptPage.y - rectMargins.bottom -rectPhysMargins.top;
  fmt.rcPage.left := 0;
  fmt.rcPage.top := 0;
  fmt.rcPage.right := ptPage.x - rectPhysMargins.left -rectPhysMargins.right - 1;
  fmt.rcPage.bottom := ptPage.y - rectPhysMargins.top - rectPhysMargins.bottom - 1;
end;

function TSciPrinter.GetNumPages : Integer;
var
  fmtRange: TRangeToFormat;
  Canvas : TCanvas;
  lengthDoc,lengthPrinted : Integer;
  cnt : Integer;
begin
  try
    Result:=0;
    FEditor.SetPrintColourMode(Integer(FColorMode));
    FEditor.SetPrintMagnification(FMagnification);
    FEditor.SetPrintWrapMode(Integer(FWordWrap));
    lengthPrinted:=0;
    lengthDoc:=FEditor.GetLength;
    Printer.BeginDoc; 
    Canvas:=Printer.Canvas;
    fmtRange.hdc := Canvas.Handle;
    fmtRange.hdcTarget := Canvas.Handle;
    fmtRange.chrg.cpMin:=0;
    fmtRange.chrg.cpMax:=lengthDoc;
    GetSettingsFor(fmtRange);
    cnt:=1;
    while (lengthPrinted < lengthDoc) do
    begin
      lengthPrinted := FEditor.FormatRange(false, @fmtRange);
      fmtRange.chrg.cpMin := lengthPrinted;
      if lengthPrinted < lengthDoc then
      begin
        Inc(cnt);
      end;
    end;
  finally
    Printer.Abort;
  end;
    Result:=cnt;
end;
procedure TSciPrinter.DoPrint;
var
  fmtRange: TRangeToFormat;
  crange : TCharacterRange;
  lengthPrinted: Integer;
  startPos,endPos,lengthDoc,lengthDocMax : Integer;
  printPage : Boolean;
  Canvas : TCanvas;
begin
  if not assigned(FEditor) then
  begin
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,['TSciPrinter']);
    Exit;
  end;
  FEditor.HandleNeeded;
  //Set the scintilla's printerrelated properties of to those of my properties.
  FEditor.SetPrintColourMode(Integer(FColorMode));
  FEditor.SetPrintMagnification(FMagnification);
  FEditor.SetPrintWrapMode(Integer(FWordWrap));
  try
    Printer.BeginDoc;
    Canvas:=Printer.Canvas;
    Printer.Title := FTitle;
    fmtRange.hdc := Canvas.Handle;
    fmtRange.hdcTarget := Canvas.Handle;
    GetSettingsFor(fmtRange);
    startPos:=0;
    endPos:=0;
    lengthDoc:=FEditor.GetLength;
    lengthDocMax:=lengthDoc;
    crange:=FEditor.GetSelectionRng;
    if FPrintRange=prSelection then
    begin
      startPos:=crange.cpMin;
      endPos:=crange.cpMax;
    end;
    if startPos=endPos then
    begin
      startPos:=0;
      endPos:=FEditor.GetLength;
    end;
    if startPos>endPos then
    begin
      lengthPrinted:=endPos;
      lengthDoc:=startPos;
    end else
    begin
      lengthPrinted:=startPos;
      lengthDoc:=endPos;
    end;
    if lengthPrinted<0 then lengthPrinted:=0;
    if lengthDoc>lengthDocMax then lengthDoc:=lengthDocMax;
    fmtRange.chrg.cpMin := startPos;
    fmtRange.chrg.cpMax := endPos;

    FCurrentPageNumber := 1;
    Canvas.Brush.Color:=clYellow;
    if assigned(FOnStartPrinting) then
      FOnStartPrinting(Self);

    while (lengthPrinted < lengthDoc) and (Printer.Aborted=False) do
    begin
      printPage := (not(FPrintRange=prPageNums) or (FCurrentPageNumber >= FFromPage) and (FCurrentPageNumber <= FToPage));
      if printPage then
      begin
        DrawMarginText(Canvas,scipHeader,fmtRange.rc);
        lengthPrinted := FEditor.FormatRange(true, @fmtRange);
        DrawMarginText(Canvas,scipFooter,fmtRange.rc);
        fmtRange.chrg.cpMin := lengthPrinted;
        if assigned(FOnBeforeNextPage) then
          FOnBeforeNextPage(Self);
      end;
      if (lengthPrinted < lengthDoc) and (not Printer.Aborted) then
      begin
        Printer.NewPage;
        Inc(FCurrentPageNumber);
      end;
    end;
    FEditor.FormatRange(false, nil);
    Printer.EndDoc;
    if assigned(FOnDonePrinting) then FOnDonePrinting(Self);
    FCurrentPageNumber:=-1;
  except
    on EPrinter do
    begin
      raise;
    end;
  end;
end;
procedure TSciPrinter.Print;
begin
  DoPrint;
end;

procedure TSciPrinter.SetEditor(Value : TScintillaBase);
begin
  FEditor :=Value;
end;
procedure TSciPrinter.SetHeaderFont(Value : TFont);
begin
  FHeaderFont.Assign(Value);
end;
procedure TSciPrinter.SetFooterFont(Value : TFont);
begin
  FFooterFont.Assign(Value);
end;

end.
