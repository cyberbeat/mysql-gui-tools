//CE_Desc_Include(helpdescriptions.txt)
{--------------------------------------------------------------------------------
* Dates : March 2002
* Version : 1.0
* Author : Stefan Cruysberghs
* Email : stefancr@scip.be
* Website : http://www.scip.be
  Visit Stefan's Homepage for other components and tools
--------------------------------------------------------------------------------
* $Archive: /Component Library/SC_Public/scFontCombobox.pas $
* $Author: hdalis $
* $Date: 2004/11/13 04:29:51 $
* $Modtime: 3/09/02 21:53 $
* $Revision: 1.4 $
--------------------------------------------------------------------------------
* This component is free of charge.
* The author doesn't give a warranty for error free running
  of this component and he doesn't give any support.
* Suggestions and bugs can be send by email.
--------------------------------------------------------------------------------
}
{

	History: 29/09/2004 Initial Release with Delphi Scintilla Interface Components
                      Used only in the EdOptionsWin.pas form.
                      Renamed the component for use with the Delphi Scintilla Interface Components
											project to avoid clashes with other components.
                      Jan Martin Pettersen (hdalis@users.sourceforge.net)
           26/10/2004 Fixed a bug that caused the fontname to overwrite the dropdownbutton.
                      Couldn't get the component to return the proper fontname,
                      it only returned fontname=whatever so
                      created GetFontName to get it to return the fontname alone.
                      hdalis (hdalis@users.sourceforge.net)
	    $Id: tcFontCombobox.pas,v 1.4 2004/11/13 04:29:51 hdalis Exp $
}
{$Include SciCommonDef.Inc}
unit tcFontCombobox;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls;

type
  TFormExampleFont = class(TForm)
    PanelPreview: TPanel;
    PanelFontName: TPanel;
  private
  public
  end;

  TscFontType = (ftTrueTypeAnsi, ftTrueTypeSymbol, ftRaster);
  TscFontTypes = set of TscFontType;

  TtcFontCombobox = class(TCustomComboBox)
  private
    FormExample : TFormExampleFont;
    BitmapTrueTypeAnsi : TBitmap;
    BitmapTrueTypeSymbol : TBitmap;
    BitmapRaster : TBitmap;
    IntCountUsed : Integer;
    BlnDown : Boolean;

    FIntPreviewWidth : Integer;
    FIntPreviewHeight : Integer;
    FStrFontName : String;
    FStrPreviewText : String;
    FBlnMoveUsedToTop : Boolean;
    FIntMaxUsed : Integer;
    FColorUsed : TColor;
    FFontTypes : TscFontTypes;
    FBlnShowPreviewInList: Boolean;
    FBlnShowPreview: Boolean;
    FBlnShowImagesFontType: Boolean;
    FBlnShowPreviewFontName: Boolean;

    procedure SetPopupHeight(const Value: Integer);
    procedure SetPopupWidth(const Value: Integer);
    procedure SetFontName(const Value: String);
    function  GetFontName : String;
    procedure SetPreviewText(const Value: String);
    procedure SetShowPreviewFontName(const Value: Boolean);
  protected
    procedure OnCloseup(var Message: TWMCommand); message CN_COMMAND;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);override;
    procedure ChooseFont;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

    procedure DropDown; override;
    procedure Click; override;

    // Refill items of listbox depening the fonttypes property
    procedure GetFontNames;
  published
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;

    property Anchors;
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Left;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    // Add used fonts to top of listbox
    property MoveUsedToTop : Boolean read FBlnMoveUsedToTop write FBlnMoveUsedToTop default True;
    property MaxUsed : Integer read FIntMaxUsed write FIntMaxUsed default 5;
    property ColorUsed : TColor read FColorUsed write FColorUsed default clNavy;

    // Preview popup window
    // When previewtext is not specified, AaBbYyZz will be shown
    property PreviewText : String read FStrPreviewText write SetPreviewText;
    property PreviewWidth : Integer read FIntPreviewWidth write SetPopupWidth default 250;
    property PreviewHeight : Integer read FIntPreviewHeight write SetPopupHeight default 45;

    // Get or set font
    property FontName : String read GetFontName write SetFontName;

    // Fonttypes which will be visible (ftTrueTypeAnsi, ftTrueTypeSymbol, ftRaster)
    property FontTypes : TscFontTypes read FFontTypes write FFontTypes;

    // Show preview popup window
    property ShowPreview : Boolean read FBlnShowPreview write FBlnShowPreview default True;
    // Show small panel with fontname in preview popup window
    property ShowPreviewFontName : Boolean read FBlnShowPreviewFontName write SetShowPreviewFontName default True;
    // Show preview of item in listbox
    property ShowPreviewInList : Boolean read FBlnShowPreviewInList write FBlnShowPreviewInList default True;
    // Show small images depending the fonttype
    property ShowImagesFontType : Boolean read FBlnShowImagesFontType write FBlnShowImagesFontType default True;
  end;

implementation

{$R tcFontCombobox.DFM}
{$R tcFontCombobox.res}


//------------------------------------------------------------------------------
constructor TtcFontCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
  begin
    FormExample := TFormExampleFont.Create(nil);
    FormExample.Height := 0;
    FormExample.Width := 0;
    FormExample.Color := ClWhite;
    FormExample.Visible := True;
    ShowWindow(FormExample.Handle,SW_HIDE);

    BitmapTrueTypeAnsi := TBitmap.Create;
    BitmapTrueTypeAnsi.LoadFromResourceName(HInstance,'FONTTRUETYPEANSI');
    BitmapTrueTypeAnsi.Transparent:=True;
    BitmapTrueTypeSymbol := TBitmap.Create;
    BitmapTrueTypeSymbol.LoadFromResourceName(HInstance,'FONTTRUETYPESYMBOL');
    BitmapTrueTypeSymbol.Transparent:=True;
    BitmapRaster := TBitmap.Create;
    BitmapRaster.LoadFromResourceName(HInstance,'FONTRASTER');
    BitmapRaster.Transparent:=True;

    FormExample.Width := 240;
    FormExample.Height := 50;
  end;

  FIntPreviewWidth := 240;
  FIntPreviewHeight := 60;
  FFontTypes := [ftTrueTypeAnsi,ftTrueTypeSymbol];
  FBlnMoveUsedToTop := True;
  FIntMaxUsed := 5;
  FColorUsed := clNavy;
  FBlnShowPreviewInList := True;
  FBlnShowPreview := True;
  FBlnShowPreviewFontName := True;
  FBlnShowImagesFontType := True;

  IntCountUsed := 0;
  BlnDown := False;

  Style := csOwnerDrawFixed;
  ItemHeight := 18;
  DropDownCount := 12;
  Width := 200;
  Font.Size := 10;
end;

//------------------------------------------------------------------------------
destructor TtcFontCombobox.Destroy;
begin
  BitmapTrueTypeAnsi.Free;
  BitmapTrueTypeSymbol.Free;
  BitmapRaster.Free;
  FormExample.Free;
  inherited Destroy;
end;

function TtcFontCombobox.GetFontName : String;
var
ps : Integer;
begin
  ps:=Pos('=',FStrFontName);
  if (ps>0) then
    Result :=Copy(FStrFontName,1,ps-1)
  else
  Result :=FStrFontName;
end;
//------------------------------------------------------------------------------
procedure TtcFontCombobox.Loaded;
begin
  inherited;
  GetFontNames;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  StrFont : String;
  StrFontType : String;
  r : TRect;
begin
  if csDesigning in ComponentState then
    Exit;

  with Canvas do
  begin
    StrFont := Items.Names[Index];
    StrFontType := Items.Values[StrFont];
    FillRect(Rect);

    // Show small image depending the fonttype
    if FBlnShowImagesFontType then
    begin
      if (StrFontType = 'TA') then
        Draw(2,Rect.Top+1,BitmapTrueTypeAnsi)
      else
        if (StrFontType = 'TS') then
          Draw(2,Rect.Top+1,BitmapTrueTypeSymbol)
        else
          Draw(2,Rect.Top+1,BitmapRaster);
    end;

    // Color for used itmes can be different
    if (BlnDown) and (Index < IntCountUsed) then
      begin
        if (odFocused in State) then
          Font.Color := clHighlightText
        else
          Font.Color := FColorUsed
      end
    else
    begin
      if (odFocused in State) then
        Font.Color := clHighlightText
      else
        Font.Color := Self.Font.Color;
    end;
    // If property ShowPreviewInList is true the current
    // font will be the fontname of the item
    if FBlnShowPreviewInList then
    begin
      if Items.Values[StrFont] <> 'TS' then
        Font.Name := StrFont
      else
        Font.Name := Self.Font.Name;
    end;
    r:=Rect;

    if FBlnShowImagesFontType then
    begin
      r.Left:=r.Left+15;
      TextRect(r,20,r.Top+1,StrFont);
    end
    else
    begin
      r.Left:=r.Left+4;
      TextRect(r,4,r.Top+1,StrFont);
    end;

    // Show preview popupwindow for focused item
    if (FBlnShowPreview) and (odFocused in State) then
    begin
      if Assigned(FormExample) then
      begin
        FormExample.PanelPreview.Font.Name := StrFont;
        if Trim(FStrPreviewText) = '' then
          FormExample.PanelPreview.Caption := 'AaBbYyZz';

        FormExample.PanelFontName.Caption := StrFont;
      end;
    end;

    // Draw line after used fonts
    if (BlnDown) and (FBlnMoveUsedToTop) and (Index = (IntCountUsed - 1)) then
    begin
      MoveTo(0,Rect.Bottom-1);
      LineTo(Width,Rect.Bottom-1);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.DropDown;
var
  Point : TPoint;
begin
  BlnDown := True;

  inherited Dropdown;

  // Set position of preview popup window
  if FBlnShowPreview then
  begin
    Point.x := (Self.Left)+ Self.width;
    Point.y := (Self.Top)+ Self.height ;
    Point := Parent.ClientToScreen(Point);
    FormExample.Top := Point.y;
    FormExample.Left := Point.x;

    if FormExample.Left + FormExample.Width > Screen.Width then
    begin
      Point.x := (Self.Left);
      Point := Parent.ClientToScreen(Point);
      FormExample.Left := Point.x - FormExample.Width;
    end;

    if FormExample.Top + FormExample.Height > Screen.Height then
    begin
      Point.y := (Self.Top);
      Point := Parent.ClientToScreen(Point);
      FormExample.Top := Point.y - FormExample.Height;
    end;

    ShowWindow(FormExample.Handle, SW_SHOWNA);
  end;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.Click;
begin
  if BlnDown = False then
  begin
    FStrFontName := Items.Names[ItemIndex];
    inherited Click;
  end;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.OnCloseup(var Message: TWMCommand);
begin
  if Message.NotifyCode = CBN_CLOSEUP then
  begin
    Self.SetFocus;
    ShowWindow(FormExample.Handle,SW_HIDE);

    ChooseFont;

    inherited Click;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.ChooseFont;
var
  IntIndex : Integer;
  BlnAlreadyUsed : Boolean;
begin
  if ItemIndex = -1 then
    Exit;

  FStrFontName := Items[ItemIndex];
  Text := FStrFontName;

  BlnDown := False;
  if (FBlnMoveUsedToTop = True) and (ItemIndex <> 0) then
  begin

    // Test if font has already been used
    BlnAlreadyUsed := False;
    IntIndex:=0;
    while (not BlnAlreadyUsed) and (IntIndex < IntCountUsed) do
    begin
      BlnAlreadyUsed := (Items[IntIndex] = FStrFontName);
      Inc(IntIndex);
    end;

    // Insert item at top when font is not used yet
    // Otherwise move item from used list to top
    if not BlnAlreadyUsed then
    begin
      Items.Insert(0,FStrFontName);
      Inc(IntCountUsed);
    end
    else
    begin
      Items.Move(IntIndex-1,0);
    end;

    // When maximum used items is reached, delete last item
    if (FIntMaxUsed <> 0) and (IntCountUsed > FIntMaxUsed) then
    begin
      Items.Delete(FIntMaxUsed);
      Dec(IntCountUsed);
    end;

    ItemIndex := 0;
  end;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.GetFontNames;
type
  TFontRec = record
    FontTypes : TscFontTypes;
    FontItems : TStrings;
  end;
var
  DC: HDC;
  FontRec : TFontRec;

  function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
    FontType: Integer; Data: Pointer): Integer; stdcall;
  var
    StrType : String;
  begin
    StrType :='';
    if FontType = TRUETYPE_FONTTYPE then
      case LogFont.lfCharset of
        ANSI_CHARSET : StrType := 'TA';
        SYMBOL_CHARSET : StrType := 'TS';
      end
    else
      if FontType = RASTER_FONTTYPE then
        StrType := 'R';

    // Check which fonts have to be added to listbox
    if (LogFont.lfFaceName[0] <> '@')
      and
        (((StrType = 'TA') and (ftTrueTypeAnsi in TFontRec(Data^).FontTypes)) or
         ((StrType = 'TS') and (ftTrueTypeSymbol in TFontRec(Data^).FontTypes)) or
         ((StrType = 'R') and (ftRaster in TFontRec(Data^).FontTypes))) then
    begin
      TStrings(TFontRec(Data^).FontItems).Add(LogFont.lfFaceName+'='+StrType);
    end;

    Result := 1;
  end;
begin
  IntCountUsed := 0;
  Items.Clear;

  // Make record to provide a pointer to the items
  // of the listbox and the property fonttypes
  FontRec.FontItems := Pointer(Items);
  FontRec.FontTypes := FFontTypes;

  DC := GetDC(0);
  try
    EnumFonts(DC, nil, @EnumFontsProc, @FontRec);
  finally
    ReleaseDC(0, DC);
  end;
  Sorted := True;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.SetPopupHeight(const Value: Integer);
begin
  FIntPreviewHeight := Value;
  if Assigned(FormExample) then
    FormExample.Height := Value;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.SetPopupWidth(const Value: Integer);
begin
  FIntPreviewWidth := Value;
  if Assigned(FormExample) then
    FormExample.Width := Value;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.SetFontName(const Value: String);
var
  i : Integer;
begin
  // Run through items to see if font is available in list
  i:=0;
  while i < Items.Count do
  begin
    if Pos(Value+'=',Items[i]) > 0 then
    begin
      ItemIndex := i;
      ChooseFont;
      Exit;
    end;
    inc(i);
  end;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.SetPreviewText(const Value: String);
begin
  FStrPreviewText := Value;
  if Assigned(FormExample) then
    FormExample.PanelPreview.Caption := Value;
end;

//------------------------------------------------------------------------------
procedure TtcFontCombobox.SetShowPreviewFontName(const Value: Boolean);
begin
  FBlnShowPreviewFontName := Value;
  if Assigned(FormExample) then
    FormExample.PanelFontName.Visible := FBlnShowPreviewFontName;
end;

end.

