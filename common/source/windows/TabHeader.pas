unit TabHeader;

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
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, TntClasses, TntExtCtrls, TntForms,
  PNGImage, AuxFuncs, Contnrs, tntmenus, VirtualTrees, Menus;

type
  TPageChangeEvent = procedure(Sender: TObject; PreviousSelectedTab: Integer; PreviousSelectedObj: TObject;
    NewSelectedTab: Integer; Obj: TObject) of object;

  // Determines how the header frame should act on the given panels and objects.
  TFreeOptions = set of
  (
    foFreePanel,             // Free the associated panel (if no longer referenced by another tab).
    foFreeObject             // Free the associated object (if no longer referenced by another tab).
  );

  TTab = class;

  TExtTntPaintBox = class;

  TTabHeaderFrame = class(TTntFrame)
    TabHeaderPopupMenu: TTntPopupMenu;
    CloseTabsheetMI: TTntMenuItem;
    N1: TTntMenuItem;
    RenameTabsheetMI: TTntMenuItem;
    procedure TntFrameResize(Sender: TObject);
    procedure RenameTabsheetMIClick(Sender: TObject);
    procedure CloseTabsheetMIClick(Sender: TObject);
    procedure TabHeaderPopupMenuPopup(Sender: TObject);
  private
    FPaintBox: TExtTntPaintBox;

    FTabs: TObjectList;
    FTabListBtnShown: Boolean;

    FNewTabPNGImg,
    FListTabPNGImg,
    FTabCloseImage,
    FInlineHelpTabImage,
    FResultSetTabImage,
    FScriptTabImage: TPNGObject;

    FSelectedTab,
    FHighlightedTab,
    FHighlightedButton: Integer;

    FOnBeforePageChange,
    FOnPageChange: TPageChangeEvent;


    FOnRequestNewPage: TNotifyEvent;
    FOnPageDelete: TNotifyEvent;
    FOnBeforePageDelete: TCloseQueryEvent;

    FTabsPopupMenu: TTntPopupMenu;

    FPaintBoxBufferedImage: TBitmap;
    FShowButtons,
    FDrawBottom,
    FTopSpacer: Boolean;
    FAutoHideTabHeader: Boolean;
    FShowDeleteButtons: Boolean;

    FClickedTabIndex: Integer;
    FOffsetX: Integer;
    FAvailableWidth: Integer;

    procedure SetAutoHideTabHeader(const Value: Boolean);
    procedure SetDrawBottom(const Value: Boolean);
    procedure SetShowButtons(const Value: Boolean);
    procedure SetShowDeleteButtons(const Value: Boolean);
    procedure SetOffsetX(Value: Integer);
  protected
    procedure ComputeTabs;
    procedure ComputeAvailableWidth;
    procedure DragOver(Source: TObject; X: Integer; Y: Integer; State: TDragState; var Accept: Boolean); override;
    function GetSelectedTab: Integer;
    function GetTab(index: Integer): TTab;
    function GetTabByName(Caption: WideString): TTab;
    function GetTabIndexFromPos(X,Y: Integer): Integer;
    function GetTabIndexFromObject(Obj: TObject): Integer;
    function GetTabIndexFromPanel(TabPanel: TTntPanel): Integer;
    procedure SetSelectedTab(SelectedTabIndex: Integer);
    procedure TabDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TabDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TabMenuItemSelected(Sender: TObject);
    procedure TabPaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TabPaintBoxMouseLeave(Sender: TObject);
    procedure TabPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TabPaintBoxPaint(Sender: TObject);
    function GetSelectedTabsheet: TTab;
    procedure SetSelectedTabsheet(Tab: TTab);
    procedure ScrollIntoView(Index: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddTabSheet(AOwner: TComponent; Caption, ImageName: WideString; TabPanel: TTntPanel; Obj: TObject = nil;
      SelectNewPage: Boolean = True; DockTabPanel: Boolean = True; TabPanelHeight: Integer = -1;
      FreeOptions: TFreeOptions = [foFreePanel, foFreeObject]): Integer;
    procedure ClearTabSheets;
    function DeleteTab(TabIndex: Integer; AllowDeletionOfLastTab: Boolean = False): Boolean;
    function TabCount: Integer;
    procedure Show;
    procedure Hide;
    procedure SelectNextTabSheet;
    procedure SelectPreviousTabSheet;
    function GetTabIndex(Tab: TTab): Integer;

    property AutoHideTabHeader: Boolean read FAutoHideTabHeader write SetAutoHideTabHeader;
    property DrawBottom: Boolean read FDrawBottom write SetDrawBottom;
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property SelectedTab: Integer read GetSelectedTab write SetSelectedTab;
    property SelectedTabSheet: TTab read GetSelectedTabsheet write SetSelectedTabsheet;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ShowDeleteButtons: Boolean read FShowDeleteButtons write SetShowDeleteButtons default True;
    property TabSheets[Index: Integer]: TTab read GetTab;
    property TabSheetsByName[Caption: WideString]: TTab read GetTabByName;
    property TabIndexFromObject[Obj: TObject]: Integer read GetTabIndexFromObject;
    property TabIndexFromPanel[TabPanel: TTntPanel]: Integer read GetTabIndexFromPanel;

    property OnBeforePageChange: TPageChangeEvent read FOnBeforePageChange write FOnBeforePageChange;
    property OnPageChange: TPageChangeEvent read FOnPageChange write FOnPageChange;
    property OnBeforePageDelete: TCloseQueryEvent read FOnBeforePageDelete write FOnBeforePageDelete;
    property OnPageDelete: TNotifyEvent read FOnPageDelete write FOnPageDelete;
    property OnRequestNewPage: TNotifyEvent read FOnRequestNewPage write FOnRequestNewPage;
  end;

  TTab = class(TObject)
  private
    FImage: TPngObject;
    FBounds: TRect;
    FFreeOptions: TFreeOptions;
  protected
    procedure SetCaption(Caption: Widestring);
  public
    FCaption: WideString;
    FTabHeader: TTabHeaderFrame;

    TabPanel: TTntPanel;
    Obj: TObject;
    DockTabPanel: Boolean;
    RegularTextWidth: Integer;
    BoldTextWidth: Integer;

    constructor Create(AOwner: TComponent; TabHeader: TTabHeaderFrame; Caption, ImageName: WideString;
      TabPanel: TTntPanel; Obj: TObject; DockTabPanel: Boolean; FreeOptions: TFreeOptions); reintroduce;
    destructor Destroy; override;

    property Caption: WideString read FCaption write SetCaption;
  end;

  TExtTntPaintBox = class(TTntPaintBox)
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  private
    FOnMouseLeave: TNotifyEvent;
  published
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Options, ColorTypes, ColorTools, CommonTypes, PNGTools;

const
  SeparatorWidth = 4;         // The space between the tab text and the images in a tab.
  TabBorderColor = $009C9B91; // The color of the tab border.
  BottomSpace    = 4;         // The space below the tabs.
  Tabheight      = 30;        // Height of the tab area.

//----------------------------------------------------------------------------------------------------------------------

procedure TExtTntPaintBox.CMMouseLeave(var Message: TMessage);

begin
  if (Assigned(FOnMouseLeave)) then
    FOnMouseLeave(self);
end;

//----------------- TTabFrameHeader  -----------------------------------------------------------------------------------

constructor TTabHeaderFrame.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  InitFrame(self);

  FOffsetX := 0;
  ControlStyle := ControlStyle + [csOpaque];

  // Set font according to user settings.
  Font.Name := MyxCommonOptions.DefaultFontName;
  Font.Height := MyxCommonOptions.DefaultFontHeight;

  FOnPageChange := nil;
  FOnBeforePageChange := nil;

  FPaintBox := TExtTntPaintBox.Create(self);
  FPaintBox.Parent := self;
  FPaintBox.Align := alClient;
  FPaintBox.OnMouseDown := TabPaintBoxMouseDown;
  FPaintBox.OnMouseMove := TabPaintBoxMouseMove;
  FPaintBox.OnMouseLeave := TabPaintBoxMouseLeave;
  FPaintBox.OnPaint := TabPaintBoxPaint;
  FPaintBox.OnDragOver := TabDragOver;
  FPaintBox.OnDragDrop := TabDragDrop;

  FPaintBoxBufferedImage := nil;

  FShowButtons := False;
  FShowDeleteButtons := True;
  DrawBottom := False;
  FTopSpacer := False;
  FSelectedTab := -1;
  FHighlightedTab := -1;
  FHighlightedButton := -1;
  FAutoHideTabHeader := False;
  FClickedTabIndex := -1;

  FTabListBtnShown := False;

  FTabsPopupMenu := TTntPopupMenu.Create(nil);

  FNewTabPNGImg := LoadPNGImageFromResource('tab_new');
  FListTabPNGImg := LoadPNGImageFromResource('tab_list');
  FTabCloseImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FInlineHelpTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FResultSetTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FScriptTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');

  FTabs := TObjectList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TTabHeaderFrame.Destroy;

begin
  FShowButtons := False;

  if (FPaintBoxBufferedImage <> nil) then
    FPaintBoxBufferedImage.Free;

  ClearTabsheets;
  FTabs.Free;

  FNewTabPNGImg.Free;
  FListTabPNGImg.Free;
  FTabCloseImage.Free;
  FInlineHelpTabImage.Free;
  FResultSetTabImage.Free;
  FScriptTabImage.Free;

  FTabsPopupMenu.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetAutoHideTabHeader(const Value: Boolean);

begin
  FAutoHideTabHeader := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetDrawBottom(const Value: Boolean);

begin
  FDrawBottom := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetOffsetX(Value: Integer);

begin
  // Sanity checks.
  if FTabs.Count = 0 then
    Value := 0
  else
  begin
    if (FAvailableWidth - TTab(FTabs[FTabs.Count - 1]).FBounds.Right) > Value then
      Value := FAvailableWidth - TTab(FTabs[FTabs.Count - 1]).FBounds.Right;
    if Value > 0 then
      Value := 0;
  end;

  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetShowButtons(const Value: Boolean);

begin
  if FShowButtons <> Value then
  begin
    FShowButtons := Value;
    ComputeAvailableWidth;
    OffsetX := FOffsetX;   // Set it again. It will consider all necessary conditions and adjust itself.

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetShowDeleteButtons(const Value: Boolean);

begin
  if FShowDeleteButtons <> Value then
  begin
    FShowDeleteButtons := Value;
    ComputeTabs;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.ComputeTabs;

var
  I: Integer;
  DrawFlags: Cardinal;
  R, CaptionRect: TRect;
  Tab: TTab;
  TopSpace: Integer;

begin
  FPaintBox.Canvas.Font.Assign(Font);

  // Compute space for bold font style to have enough room.
  FPaintBox.Canvas.Font.Style := [fsBold];

  if FTopSpacer then
    TopSpace := 4
  else
    TopSpace := 0;

  DrawFlags := DT_CALCRECT or DT_SINGLELINE;
  CaptionRect := Rect(0, 0, 0, 0);

  R := Rect(SeparatorWidth, TopSpace, 0, Tabheight - 7 - BottomSpace + TopSpace);
  if FShowButtons then
    Inc(R.Left, FNewTabPNGImg.Width);

  for I := 0 to FTabs.Count - 1 do
  begin
    Tab := TTab(FTabs[I]);

    // Compute regular text width.
    FPaintBox.Canvas.Font.Style := [];
    Windows.DrawTextW(FPaintBox.Canvas.Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags);

    TTab(FTabs[I]).RegularTextWidth := CaptionRect.Right;

    // Compute bold text width.
    FPaintBox.Canvas.Font.Style := [fsBold];

    // Compute overall width of the tab.
    if IsWinNTPlatform then
      Windows.DrawTextW(FPaintBox.Canvas.Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags)
    else
      DrawTextW(FPaintBox.Canvas.Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags, True);

    TTab(FTabs[I]).BoldTextWidth := CaptionRect.Right;

    R.Right := R.Left + 2 * SeparatorWidth + CaptionRect.Right;
    if FShowDeleteButtons and (FTabCloseImage <> nil) then
      Inc(R.Right, FTabCloseImage.Width + SeparatorWidth);
    if Tab.FImage <> nil then
      Inc(R.Right, Tab.FImage.Width + SeparatorWidth);

    Tab.FBounds := R;
    R.Left := R.Right;
  end;

  ComputeAvailableWidth;
  OffsetX := FOffsetX;   // Set it again. It will consider all necessary conditions and adjust itself.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.ComputeAvailableWidth;

begin
  FAvailableWidth := ClientWidth;
  if FShowButtons then
    Dec(FAvailableWidth, FNewTabPNGImg.Width + FListTabPNGImg.Width);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

var
  SchemaInfo: ISchemaInfo;

begin
  inherited;

  if Accept then
  begin
    TComponent(Source).Owner.GetInterface(ISchemaInfo, SchemaInfo);
    Accept := Assigned(SchemaInfo);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetSelectedTab: Integer;

begin
  Result := FSelectedTab;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTab(index: Integer): TTab;

begin
  if (index < FTabs.Count) then
    Result := TTab(FTabs[index])
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTabByName(Caption: WideString): TTab;

var
  I: Integer;

begin
  Result := nil;

  // find tab by name
  for I := 0 to FTabs.Count - 1 do
    if (TTab(FTabs[I]).Caption = Caption) then
    begin
      Result := TTab(FTabs[I]);
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetSelectedTabsheet: TTab;

begin
  Result := GetTab(FSelectedTab);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetSelectedTabsheet(Tab: TTab);

var
  Index: Integer;

begin
  Index := FTabs.IndexOf(Tab);

  if (Index > -1) then
    SetSelectedTab(Index);
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTabIndexFromPos(X, Y: Integer): Integer;

var
  I: Integer;
  Tab: TTab;
  
begin
  Result := -1;
  Dec(X, FOffsetX);
  for I := 0 to FTabs.Count - 1 do
  begin
    Tab := TTab(FTabs[I]);
    if (X >= Tab.FBounds.Left + SeparatorWidth) and
      (X < Tab.FBounds.Right - SeparatorWidth) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTabIndexFromObject(Obj: TObject): Integer;

var
  I: Integer;

begin
  Result := -1;

  for I:=0 to FTabs.Count-1 do
    if(TTab(FTabs[i]).Obj=Obj)then
    begin
      Result := I;
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTabIndexFromPanel(TabPanel: TTntPanel): Integer;

var
  I: Integer;

begin
  Result := -1;

  for I:=0 to FTabs.Count-1 do
    if(TTab(FTabs[i]).TabPanel=TabPanel)then
    begin
      Result := I;
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SetSelectedTab(SelectedTabIndex: Integer);

var
  prevSelectedTab: Integer;
  prevSelectedObj: TObject;
  Tab: TTab;
  TabObject: TObject;

begin
  if (SelectedTabIndex > -1) and not (csDestroying in ComponentState) then
  begin
    if SelectedTabIndex > FTabs.Count - 1 then
      SelectedTabIndex := FTabs.Count - 1;
    if SelectedTabIndex <> FSelectedTab then
    begin
      prevSelectedTab := FSelectedTab;
      if (prevSelectedTab >= 0) and (prevSelectedTab < FTabs.Count) then
        prevSelectedObj := TTab(FTabs[FSelectedTab]).Obj
      else
        prevSelectedObj := nil;

      // Call event if assigned.
      if (Assigned(FOnBeforePageChange)) then
      begin
        if prevSelectedTab > -1 then
          FOnBeforePageChange(self,
            prevSelectedTab, prevSelectedObj,
            SelectedTabIndex, TTab(FTabs[prevSelectedTab]).Obj)
        else
          FOnBeforePageChange(self,
            prevSelectedTab, prevSelectedObj,
            SelectedTabIndex, nil);
      end;

      if (FSelectedTab > -1) and (FSelectedTab < FTabs.Count) then
        if (Assigned(TTab(FTabs[FSelectedTab]).TabPanel)) then
          TTab(FTabs[FSelectedTab]).TabPanel.Hide;

      FSelectedTab := SelectedTabIndex;

      if (FSelectedTab > -1) and (FSelectedTab < FTabs.Count) then
      begin
        Tab := TTab(FTabs[FSelectedTab]);

        // Ensure the new tab is in the visible area.
        ScrollIntoView(FSelectedTab);

        if Assigned(Tab.TabPanel) then
        begin
          Tab.TabPanel.Show;
          Tab.TabPanel.BringToFront;
        end;
      end
      else
        Tab := nil;

      // Call event if assigned.
      TabObject := nil;
      if FSelectedTab > -1 then
        TabObject := Tab.Obj;
      if Assigned(FOnPageChange) then
        FOnPageChange(Self, prevSelectedTab, prevSelectedObj, FSelectedTab, TabObject);

      ComputeTabs;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabDragDrop(Sender, Source: TObject; X, Y: Integer);

begin
  SelectedTab := GetTabIndexFromPos(X, Y);
  if Assigned(OnDragDrop) then
    OnDragDrop(Sender, Source, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

begin
  FHighlightedTab := GetTabIndexFromPos(X, Y);
  Invalidate;
  Update;

  if Assigned(OnDragOver) then
    OnDragOver(Sender, Source, X, Y, State, Accept);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabMenuItemSelected(Sender: TObject);

begin
  SelectedTab := TTntMenuItem(Sender).Tag;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabPaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);

var
  I: Integer;
  Pnt: TPoint;
  MenuItem: TTntMenuItem;
  PreAreaWidth: Integer;
  ActiveHeight: Integer;
  Tab: TTab;
  TopSpace: Integer;

begin
  PreAreaWidth := SeparatorWidth;
  if FShowButtons then
    Inc(PreAreaWidth, FNewTabPNGImg.Width);

  if FTopSpacer then
    TopSpace := 4
  else
    TopSpace := 0;

  FClickedTabIndex := -1;

  ActiveHeight := Tabheight - 7 - BottomSpace + TopSpace;

  if (Y < ActiveHeight) then
  begin
    // User clicked on New Tab icon
    if (X < PreAreaWidth) and FShowButtons then
    begin
      if (Assigned(FOnRequestNewPage)) then
        FOnRequestNewPage(self);
    end
    else
      if X >= (Width - PreAreaWidth) then
      begin
        // User clicked on Tab List icon.
        Pnt := TControl(Sender).ClientToScreen(Point(0, 0));

        FTabsPopupMenu.Items.Clear;
        for i := 0 to FTabs.Count - 1 do
        begin
          MenuItem := TTntMenuItem.Create(FTabsPopupMenu);
          MenuItem.Caption := TTab(FTabs[i]).Caption;
          MenuItem.Tag := i;
          MenuItem.OnClick := TabMenuItemSelected;
          FTabsPopupMenu.Items.Add(MenuItem);
        end;

        FTabsPopupMenu.Popup(Pnt.X + Width - 2 * PreAreaWidth + 1, Pnt.Y + 16);
      end
      else
      begin
        // User clicked on Tabsheet
        Dec(X, FOffsetX);
        for I := 0 to FTabs.Count - 1 do
        begin
          Tab := TTab(FTabs[I]);
          if (X >= Tab.FBounds.Left + SeparatorWidth) and
            (X < Tab.FBounds.Right - SeparatorWidth) then
          begin
            // We found a tab under mouse.
            if (Button=mbRight) and (FShowButtons) then
            begin
              FClickedTabIndex := I;
              TabHeaderPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y)
            end
            else
              //See if it is to be closed.
              if FShowDeleteButtons and (FTabCloseImage <> nil) then
              begin
                if X > (Tab.FBounds.Right - 2 * SeparatorWidth - FTabCloseImage.Width) then
                  DeleteTab(I)
                else
                  SelectedTab := I;
              end
              else
                SelectedTab := I;
            Break;
          end;
        end;
      end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabPaintBoxMouseLeave(Sender: TObject);

begin
  FHighlightedTab := -1;
  FHighlightedButton := -1;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

var
  PreAreaWidth: Integer;
  ActiveHeight: Integer;
  TopSpace: Integer;

begin
  FHighlightedButton := -1;
  FHighlightedTab := -1;

  PreAreaWidth := SeparatorWidth;
  if FShowButtons then
    Inc(PreAreaWidth, FNewTabPNGImg.Width);

  if FTopSpacer then
    TopSpace := 4
  else
    TopSpace := 0;

  ActiveHeight := Tabheight - 7 - BottomSpace + TopSpace;

  if (Y < ActiveHeight) then
  begin
    // User clicked on New Tab icon
    if (X < PreAreaWidth) and FShowButtons then
      FHighlightedButton := 0
    else
      if (X >= Width - PreAreaWidth) then
      begin
        // User clicked on Tab List icon.
        FHighlightedButton := 2;
      end
      else
      begin
        // User hit a tabsheet.
        FHighlightedTab := GetTabIndexFromPos(X, Y);
      end;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabPaintBoxPaint(Sender: TObject);

var
  I: Integer;
  TopSpace: Integer;
  CurrentLeft, Y: Integer;
  TabRect: TRect;
  Tab: TTab;
  CaptionRect: TRect;
  DrawFlags: Cardinal;
  Bottom: Integer;
  TempColor: TRGB;
  PaintRegion: HRGN;

begin
  if (FPaintBoxBufferedImage = nil) then
    FPaintBoxBufferedImage := TBitmap.Create;

  FPaintBoxBufferedImage.Width := FPaintBox.Width;
  FPaintBoxBufferedImage.Height := FPaintBox.Height;

  with FPaintBoxBufferedImage.Canvas do
  begin
    Brush.Style := bsSolid;

    if FTopSpacer then
      TopSpace := 4
    else
      TopSpace := 0;

    Font.Assign(Self.Font);

    // Initialize background. Since we are double-buffering it is more efficient to erase the whole background in
    // one go instead in several steps.
    Brush.Color := clBtnFace;
    FillRect(ClientRect);

    // Bottom white line
    Brush.Color := clWhite;
    FillRect(Rect(1, 23 - 4 + TopSpace, Width - 1, Height));

    CurrentLeft := SeparatorWidth;
    Bottom := Tabheight - 7 - BottomSpace + TopSpace;

    // Left vertical dark border line.
    Pen.Color := TabBorderColor;
    MoveTo(0, Tabheight - BottomSpace + TopSpace);
    LineTo(0, Bottom - 1);

    // Draw Buttons if enabled.
    if FShowButtons then
    begin
      if FHighlightedButton = 0 then
        FNewTabPNGImg.Draw(FPaintBoxBufferedImage.Canvas, Rect(3, 0, 3 + FNewTabPNGImg.Width, FNewTabPNGImg.Height))
      else
        FNewTabPNGImg.DrawFaded(FPaintBoxBufferedImage.Canvas, Rect(3, 0, 3 + FNewTabPNGImg.Width,
          FNewTabPNGImg.Height), 60);

      Inc(CurrentLeft, FNewTabPNGImg.Width);
    end;

    // Draw horizontal line left to the first tab.
    LineTo(CurrentLeft, Bottom - 1);

    // Limit drawing to tab content area only.
    PaintRegion := CreateRectRgn(CurrentLeft, 0, Width - FListTabPNGImg.Width, Height);
    SelectClipRgn(Handle, PaintRegion);
    DeleteObject(PaintRegion);

    // Draw tabulators.
    DrawFlags := DT_SINGLELINE or DT_NOCLIP or DT_VCENTER;
    CaptionRect := Rect(0, 0, 0, 0);
    for I := 0 to FTabs.Count - 1 do
    begin
      Tab := TTab(FTabs[I]);
      TabRect := Tab.FBounds;
      OffsetRect(TabRect, FOffsetX, 0);

      // Skip tabs if they aren't visible.
      if TabRect.Right <= 0 then
        Continue;
        
      // Stop painting if we filled up the available space.
      if TabRect.Right > FAvailableWidth + FNewTabPNGImg.Width - SeparatorWidth then
        Break;

      if FSelectedTab = I then
      begin
        // This is the active tab.
        Brush.Color := clWhite;
        FillRect(TabRect);

        with TabRect do
        begin
          LineTo(Left, Top);
          LineTo(Right, Top);
          LineTo(Right, Bottom - 1);
        end;
      end
      else
      begin
        if FHighlightedTab = I then
        begin
          // This is the hot tab.
          TempColor := MakeRGB(clBtnFace);
          Brush.Color := MakeColorRef(DarkenColor(TempColor, 0.05), 1);
          Pen.Color := Brush.Color;
          with TabRect do
            RoundRect(Left + 2, Top + 1, Right - 2, Bottom - 2, 3, 3);

          Pen.Color := TabBorderColor;
        end;
        LineTo(TabRect.Right, TabRect.Bottom - 1);
      end;

      CurrentLeft := TabRect.Left + SeparatorWidth;

      if not (FSelectedTab = I) and (Tab.RegularTextWidth <> 0) then
        CurrentLeft := CurrentLeft + (Tab.BoldTextWidth - Tab.RegularTextWidth) div 2;

      if Tab.FImage <> nil then
      begin
        Y := (TabRect.Bottom - TabRect.Top - Tab.FImage.Height) div 2;
        if FSelectedTab = I then
          Tab.FImage.Draw(FPaintBoxBufferedImage.Canvas, Rect(CurrentLeft, Y, Tab.FImage.Width, Tab.FImage.Height))
        else
          Tab.FImage.DrawFaded(FPaintBoxBufferedImage.Canvas, Rect(CurrentLeft, Y, Tab.FImage.Width,
            Tab.FImage.Height), 200);

        Inc(CurrentLeft, Tab.FImage.Width + SeparatorWidth);
      end;

      CaptionRect := TabRect;
      CaptionRect.Left := CurrentLeft;
      if FShowDeleteButtons and (FTabCloseImage <> nil) then
      begin
        Dec(CaptionRect.Right, FTabCloseImage.Width + SeparatorWidth);

        // The close button image is only drawn if this is the hot or the active tab.
        if ((FHighlightedTab = I) or (FSelectedTab = I)) and (FTabs.Count > 1) then
        begin
          Y := (TabRect.Bottom - TabRect.Top - FTabCloseImage.Height) div 2 + 1;

          if not (FHighlightedTab = I) then
            FTabCloseImage.DrawGrayscale(FPaintBoxBufferedImage.Canvas,
              Rect(CaptionRect.Right, Y,
              FTabCloseImage.Width, FTabCloseImage.Height), 150)
          else
            FTabCloseImage.Draw(FPaintBoxBufferedImage.Canvas,
              Rect(CaptionRect.Right, Y,
              FTabCloseImage.Width, FTabCloseImage.Height));
        end;
      end;

      if FSelectedTab = I then
      begin
        Font.Color := clBlack;
        Font.Style := [fsBold];
      end
      else
      begin
        if FHighlightedTab = I then
          Font.Color := clWhite
        else
          Font.Color := clBtnShadow;
        Font.Style := [];
      end;

      SetBkMode(Handle, TRANSPARENT);
      if IsWinNTPlatform then
        Windows.DrawTextW(Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags)
      else
        DrawTextW(Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags, True);
    end;

    SelectClipRgn(Handle, 0);

    // Draw border line from last tab to the right and then down.
    LineTo(Width - 1, Bottom - 1);
    LineTo(Width - 1, Tabheight - BottomSpace + TopSpace);

    if FShowButtons and (FTabs.Count > 0) then
    begin
      if TTab(FTabs[FTabs.Count - 1]).FBounds.Right > FAvailableWidth  + FNewTabPNGImg.Width - SeparatorWidth then
      begin
        if (FHighlightedButton = 2) then
          FListTabPNGImg.Draw(FPaintBoxBufferedImage.Canvas,
            Rect(Width - SeparatorWidth - FListTabPNGImg.Width, 0, Width - SeparatorWidth, FListTabPNGImg.Height))
        else
          FListTabPNGImg.DrawFaded(FPaintBoxBufferedImage.Canvas,
            Rect(Width - SeparatorWidth - FListTabPNGImg.Width, 0, Width - SeparatorWidth, FListTabPNGImg.Height), 60);

        FTabListBtnShown := True;
      end
      else
        FTabListBtnShown := False;
    end;

    if FDrawBottom then
    begin
      MoveTo(Width - 1, TabHeight - 4 + TopSpace);
      LineTo(Width - 1, Height - 1);
      LineTo(0, Height - 1);
      LineTo(0, TabHeight - 4 + TopSpace);
    end;
  end;

  FPaintBox.Canvas.Draw(0, 0, FPaintBoxBufferedImage);
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.DeleteTab(TabIndex: Integer; AllowDeletionOfLastTab: Boolean): Boolean;

var
  CanClose: Boolean;
  LastActiveTab: Integer;
  Count: Integer;
  I: Integer;
  Reference: TObject;
  Tab: TTab;

begin
  Result := True;

  if ((FTabs.Count > 1) and (TabIndex > -1) and (TabIndex < FTabs.Count)) or
    (AllowDeletionOfLastTab and (FTabs.Count = 1)) then
  begin
    CanClose := True;

    if Assigned(FOnBeforePageDelete) then
      FOnBeforePageDelete(FTabs[TabIndex], CanClose);

    if not CanClose then
    begin
      Result := False;
      Exit;
    end;

    if TabIndex = FHighlightedTab then
      FHighlightedTab := -1;

    LastActiveTab := FSelectedTab;
    if TabIndex = FSelectedTab then
    begin
      if TabIndex = FTabs.Count - 1 then
      begin
        SelectedTab := FSelectedTab - 1;
        // Avoid the adjustment done below. In this special case the index is already correct.
        Dec(LastActiveTab);
      end
      else
        SelectedTab := TabIndex + 1;
    end;

    if (Assigned(FOnPageDelete)) then
      FOnPageDelete(FTabs[TabIndex]);

    // Determine usage count to know if the object can be freed or not (same for the panel).
    Tab := TTab(FTabs[TabIndex]);
    if Assigned(Tab.Obj) and (foFreeObject in Tab.FFreeOptions) then
    begin
      Count := 0;
      Reference := Tab.Obj;
      for I := 0 to FTabs.Count - 1 do
        if TTab(FTabs[I]).Obj = Reference then
          Inc(Count);
      if Count = 1 then
        TTab(FTabs[TabIndex]).Obj.Free;
    end;
    if Assigned(Tab.TabPanel)  and (foFreePanel in Tab.FFreeOptions) then
    begin
      Count := 0;
      Reference := Tab.TabPanel;
      for I := 0 to FTabs.Count - 1 do
        if TTab(FTabs[I]).TabPanel = Reference then
          Inc(Count);
      if Count = 1 then
        TTab(FTabs[TabIndex]).TabPanel.Free;
    end;
    FTabs.Delete(TabIndex);

    if not (csDestroying in ComponentState) then
    begin
      // If the active tabsheet or one before it was deleted then we have to update the selected tab index.
      if TabIndex <= LastActiveTab then
        Dec(FSelectedTab);
      ComputeTabs;
      if (FTabs.Count = 1) and (FAutoHideTabHeader) then
        Hide
      else
        Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.AddTabSheet(AOwner: TComponent; Caption, ImageName: WideString; TabPanel: TTntPanel;
  Obj: TObject; SelectNewPage: Boolean; DockTabPanel: Boolean; TabPanelHeight: Integer; FreeOptions: TFreeOptions): Integer;

begin
  FTabs.Add(TTab.Create(AOwner, self, Caption, ImageName, TabPanel, Obj, DockTabPanel, FreeOptions));
  ComputeTabs;
  Invalidate;

  if (TabPanel <> nil) then
  begin
    TabPanel.Visible := False;

    if (DockTabPanel) then
    begin
      TabPanel.Parent := Parent;
      //Get new Handles for all docked controls
      InitHandles(TabPanel);

      TabPanel.Align := alNone;
      TabPanel.Left := Left + 4;
      TabPanel.Top := Top + 23;
      TabPanel.Width := Width - 9;
      TabPanel.Height := Height - 28;
    end
    else
      if (TabPanelHeight > -1) then
      begin
        TabPanel.Left := 0;
        TabPanel.Width := Width;

        if (Visible) then
        begin
          TabPanel.Top := Top + 24;
          TabPanel.Height := TabPanelHeight - 24;
        end
        else
        begin
          TabPanel.Top := Top;
          TabPanel.Height := TabPanelHeight;
        end;
      end;

    if (Align = alBottom) then
      TabPanel.Anchors := [akLeft, akRight, akBottom]
    else
      TabPanel.Anchors := [akLeft, akTop, akRight, akBottom];

    TabPanel.BringToFront;
  end;

  if (SelectNewPage) then
    SelectedTab := FTabs.Count - 1;

  Result := FTabs.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.ClearTabSheets;

begin
  while FTabs.Count > 0 do
    DeleteTab(0, True);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.TabCount: Integer;

begin
  Result := FTabs.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.Show;
var
  i: Integer;
begin
  if (not (Visible)) then
  begin
    for i := 0 to FTabs.Count - 1 do
    begin
      TTab(FTabs[i]).TabPanel.Height := TTab(FTabs[i]).TabPanel.Height - 24;
      TTab(FTabs[i]).TabPanel.Top := TTab(FTabs[i]).TabPanel.Top + 24;
    end;
  end;

  inherited Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.Hide;

var
  i: Integer;

begin
  if (Visible) then
  begin
    for i := 0 to FTabs.Count - 1 do
    begin
      TTab(FTabs[i]).TabPanel.Top := TTab(FTabs[i]).TabPanel.Top - 24;
      TTab(FTabs[i]).TabPanel.Height := TTab(FTabs[i]).TabPanel.Height + 24;
    end;
  end;

  inherited Hide;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TntFrameResize(Sender: TObject);

var
  I: Integer;

begin
  ComputeTabs;
    
  for I := 0 to FTabs.Count - 1 do
  begin
    if (TTab(FTabs[I]).DockTabPanel) and (TTab(FTabs[I]).TabPanel <> nil) then
    begin
      TTab(FTabs[I]).TabPanel.Left := Left + 4;
      TTab(FTabs[I]).TabPanel.Top := Top + 23;
      TTab(FTabs[I]).TabPanel.Width := Width - 9;
      TTab(FTabs[I]).TabPanel.Height := Height - 28;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.ScrollIntoView(Index: Integer);

var
  Tab: TTab;
  MaxWidth: Integer;
  LeftBorder: Integer;
  
begin
  if (Index > -1) and (Index < FTabs.Count) then
  begin
    Tab := TTab(FTabs[Index]);

    // Ensure the new tab is in the visible area.
    LeftBorder := SeparatorWidth + Ord(FShowButtons) * FNewTabPNGImg.Width;
    if Tab.FBounds.Left + FOffsetX < LeftBorder then
      OffsetX := LeftBorder - Tab.FBounds.Left;
    MaxWidth := ClientWidth - Ord(FShowButtons) * FListTabPNGImg.Width - SeparatorWidth;
    if Tab.FBounds.Right + FOffsetX > MaxWidth then
      OffsetX := FOffsetX - Tab.FBounds.Right + MaxWidth;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SelectNextTabSheet;

var
  NextTab: Integer;

begin
  NextTab := FSelectedTab + 1;
  if (NextTab >= TabCount) then
    NextTab := 0;

  SelectedTab := NextTab;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.SelectPreviousTabSheet;

var
  NextTab: Integer;

begin
  NextTab := FSelectedTab - 1;
  if (NextTab < 0 ) then
    NextTab := TabCount - 1;

  SelectedTab := NextTab;
end;

//----------------------------------------------------------------------------------------------------------------------

function TTabHeaderFrame.GetTabIndex(Tab: TTab): Integer;

begin
  Result := FTabs.IndexOf(Tab);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.RenameTabsheetMIClick(Sender: TObject);

var
  TabName: WideString;

begin
  if (FClickedTabIndex>=0) and (TabSheets[FClickedTabIndex]<>nil) then
  begin
    TabName := TabSheets[FClickedTabIndex].Caption;

    if ShowModalEditDialog(_('Tabsheet Caption'),
      _('Please enter the new name of the tabsheet.'), myx_mtEdit,
        _('Rename')+#13#10+_('Cancel'), True, _('Name:'),
        TabName)=1 then
    begin
      TabSheets[FClickedTabIndex].Caption := TabName;
      ComputeTabs;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.TabHeaderPopupMenuPopup(Sender: TObject);

begin
  CloseTabsheetMI.Enabled := TabCount>1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTabHeaderFrame.CloseTabsheetMIClick(Sender: TObject);

begin
  if (FClickedTabIndex >= 0) and (FClickedTabIndex < TabCount) then
    DeleteTab(FClickedTabIndex);
end;

//----------------- TTab -----------------------------------------------------------------------------------------------

constructor TTab.Create(AOwner: TComponent; TabHeader: TTabHeaderFrame; Caption, ImageName: WideString;
  TabPanel: TTntPanel; Obj: TObject; DockTabPanel: Boolean; FreeOptions: TFreeOptions);

begin
  FCaption := Caption;
  FTabHeader := TabHeader;

  Self.TabPanel := TabPanel;
  Self.Obj := Obj;
  Self.DockTabPanel := DockTabPanel;
  FFreeOptions := FreeOptions;
  if ImageName <> '' then
    FImage := LoadPNGImageFromResource(ImageName);
  RegularTextWidth := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TTab.Destroy;

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTab.SetCaption(Caption: Widestring);

begin
  FCaption := Caption;

  FTabHeader.ComputeTabs;
  FTabHeader.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

