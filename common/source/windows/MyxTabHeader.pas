unit MyxTabHeader;

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
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ExtCtrls, TntClasses, TntExtCtrls,
  TntForms,  Contnrs, TntMenus, Menus,
  VirtualTrees, TntActnList,
  PNGImage, AuxFuncs, MyxOptions;

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

  TMyxTabHeaderFrame = class(TTntFrame, IOptionChangeListener)
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
    FOnCanClosePage: TCloseQueryEvent;

    FTabsPopupMenu: TTntPopupMenu;

    FPaintBoxBufferedImage: TBitmap;
    FShowButtons,
    FDrawBottom,
    FTopSpacer: Boolean;
    FAutoHideTabHeader: Boolean;
    FShowDeleteButtons: Boolean;
    FAllowToCloseLastTab: Boolean;
    FTabsAtBottom: Boolean;
    FInversColors: Boolean;
    FShowApplyDiscardChangesBtns: Boolean;
    FHideNewButton: Boolean;

    FClickedTabIndex: Integer;

    FActionBtnDiscardXPos,
    FActionBtnApplyXPos: Integer;
    FDiscardPNGImg,
    FDiscardDisabledPNGImg,
    FApplyPNGImg,
    FApplyDisabledPNGImg: TPngObject;
    FBtnDownBgImg,
    FBtnDownLeftBgImg: TPNGObject;

    FApplyAction,
    FDiscardAction: TTntAction;

    FActionBarStart: Integer;

    procedure SetAutoHideTabHeader(const Value: Boolean);
    procedure SetDrawBottom(const Value: Boolean);
    procedure SetShowButtons(const Value: Boolean);
    procedure SetShowDeleteButtons(const Value: Boolean);
  protected
    procedure ComputeTabs;
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
    function YC(Y: Integer): Integer;
    function PaintEditButton(Canvas: TCanvas; Caption: WideString;
      var xpos: Integer; PNGImg: TPNGObject; DisabledPNGImg: TPNGObject;
      Enabled: Boolean; ActivePNGImg: TPNGObject = nil; Active: Boolean = False): Integer;


    procedure OptionChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddTabSheet(AOwner: TComponent; Caption, ImageName: WideString; TabPanel: TTntPanel; Obj: TObject = nil;
      SelectNewPage: Boolean = True; DockTabPanel: Boolean = True; TabPanelHeight: Integer = -1;
      FreeOptions: TFreeOptions = [foFreePanel, foFreeObject];
      AlignTabPanel: Boolean = False): Integer;
    function CanClose: Boolean;
    procedure ClearTabSheets;
    function DeleteTab(TabIndex: Integer; AllowDeletionOfLastTab: Boolean = False;
      ForceDeletion: Boolean = False): Boolean;
    function TabCount: Integer;
    procedure Show;
    procedure Hide;
    procedure SelectNextTabSheet;
    procedure SelectPreviousTabSheet;
    function GetTabIndex(Tab: TTab): Integer;

    property AutoHideTabHeader: Boolean read FAutoHideTabHeader write SetAutoHideTabHeader;
    property DrawBottom: Boolean read FDrawBottom write SetDrawBottom;
    property SelectedTab: Integer read GetSelectedTab write SetSelectedTab;
    property SelectedTabSheet: TTab read GetSelectedTabsheet write SetSelectedTabsheet;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ShowDeleteButtons: Boolean read FShowDeleteButtons write SetShowDeleteButtons default True;
    property TabSheets[Index: Integer]: TTab read GetTab;
    property TabSheetsByName[Caption: WideString]: TTab read GetTabByName;
    property TabIndexFromObject[Obj: TObject]: Integer read GetTabIndexFromObject;
    property TabIndexFromPanel[TabPanel: TTntPanel]: Integer read GetTabIndexFromPanel;
    property AllowToCloseLastTab: Boolean read FAllowToCloseLastTab write FAllowToCloseLastTab;
    property TabsAtBottom: Boolean read FTabsAtBottom write FTabsAtBottom;
    property InversColors: Boolean read FInversColors write FInversColors;
    property ShowApplyDiscardChangesBtns: Boolean read FShowApplyDiscardChangesBtns write FShowApplyDiscardChangesBtns;
    property HideNewButton: Boolean read FHideNewButton write FHideNewButton;
    property ApplyAction: TTntAction read FApplyAction write FApplyAction;
    property DiscardAction: TTntAction read FDiscardAction write FDiscardAction;

    property OnBeforePageChange: TPageChangeEvent read FOnBeforePageChange write FOnBeforePageChange;
    property OnPageChange: TPageChangeEvent read FOnPageChange write FOnPageChange;
    property OnCanClosePage: TCloseQueryEvent read FOnCanClosePage write FOnCanClosePage;
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
    FTabHeader: TMyxTabHeaderFrame;

    TabPanel: TTntPanel;
    Obj: TObject;
    DockTabPanel: Boolean;
    RegularTextWidth: Integer;
    BoldTextWidth: Integer;

    constructor Create(AOwner: TComponent; TabHeader: TMyxTabHeaderFrame; Caption, ImageName: WideString;
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

const
  TabBorderColor = $009C9B91; // The color of the tab border.

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Options, ColorTypes, ColorTools, CommonTypes, PNGTools;

const
  SeparatorWidth = 4;         // The space between the tab text and the images in a tab.
  BottomSpace    = 4;         // The space below the tabs.
  Tabheight      = 30;        // Height of the tab area.

//----------------------------------------------------------------------------------------------------------------------

procedure TExtTntPaintBox.CMMouseLeave(var Message: TMessage);

begin
  if (Assigned(FOnMouseLeave)) then
    FOnMouseLeave(self);
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMyxTabHeaderFrame.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);

  InitFrame(self);

  ControlStyle := ControlStyle + [csOpaque];

  // Set font according to user settings.
  Font.Name := CommonOptions.OptionString['DefaultFontName'];
  Font.Height := CommonOptions.OptionInt['DefaultFontHeight'];
  CommonOptions.AddListener(self);

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
  FAllowToCloseLastTab := False;
  FInversColors := False;
  FShowApplyDiscardChangesBtns := False;
  FHideNewButton := False;
  FActionBarStart := Width;

  FApplyAction := nil;
  FDiscardAction := nil;

  FTabListBtnShown := False;

  FTabsPopupMenu := TTntPopupMenu.Create(nil);

  FNewTabPNGImg := LoadPNGImageFromResource('tab_new');
  FListTabPNGImg := LoadPNGImageFromResource('tab_list');
  FTabCloseImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FInlineHelpTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FResultSetTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');
  FScriptTabImage := LoadPNGImageFromResource('tabsheet_icon_close2');

  FApplyPNGImg := LoadPNGImageFromResource('action_btn_apply');
  FApplyDisabledPNGImg := LoadPNGImageFromResource('action_btn_apply_disabled');
  FDiscardPNGImg := LoadPNGImageFromResource('action_btn_discard');
  FDiscardDisabledPNGImg := LoadPNGImageFromResource('action_btn_discard_disabled');

  FBtnDownBgImg := LoadPNGImageFromResource('action_btn_down_bg');
  FBtnDownLeftBgImg := LoadPNGImageFromResource('action_btn_down_left_bg');


  FTabs := TObjectList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMyxTabHeaderFrame.Destroy;

begin
  CommonOptions.RemoveListener(self);

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

  FApplyPNGImg.Free;
  FApplyDisabledPNGImg.Free;
  FDiscardPNGImg.Free;
  FDiscardDisabledPNGImg.Free;
  FBtnDownBgImg.Free;
  FBtnDownLeftBgImg.Free;

  FTabsPopupMenu.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.OptionChanged;

begin
  Font.Name := CommonOptions.OptionString['DefaultFontName'];
  Font.Height := CommonOptions.OptionInt['DefaultFontHeight'];

  ComputeTabs;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SetAutoHideTabHeader(const Value: Boolean);

begin
  FAutoHideTabHeader := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SetDrawBottom(const Value: Boolean);

begin
  FDrawBottom := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SetShowButtons(const Value: Boolean);

begin
  FShowButtons := Value;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SetShowDeleteButtons(const Value: Boolean);

begin
  if FShowDeleteButtons <> Value then
  begin
    FShowDeleteButtons := Value;
    ComputeTabs;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.ComputeTabs;

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
  if (FShowButtons) and (not(FHideNewButton)) then
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
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

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

function TMyxTabHeaderFrame.GetSelectedTab: Integer;

begin
  Result := FSelectedTab;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.GetTab(index: Integer): TTab;

begin
  if (index < FTabs.Count) then
    Result := TTab(FTabs[index])
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.GetTabByName(Caption: WideString): TTab;

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

function TMyxTabHeaderFrame.GetSelectedTabsheet: TTab;

begin
  if (FSelectedTab > -1) then
    Result := GetTab(FSelectedTab)
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SetSelectedTabsheet(Tab: TTab);

var
  Index: Integer;

begin
  Index := FTabs.IndexOf(Tab);

  if (Index > -1) then
    SetSelectedTab(Index);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.GetTabIndexFromPos(X, Y: Integer): Integer;

var
  I: Integer;
  Tab: TTab;
  
begin
  Result := -1;
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

function TMyxTabHeaderFrame.GetTabIndexFromObject(Obj: TObject): Integer;

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

function TMyxTabHeaderFrame.GetTabIndexFromPanel(TabPanel: TTntPanel): Integer;

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

procedure TMyxTabHeaderFrame.SetSelectedTab(SelectedTabIndex: Integer);

var
  prevSelectedTab: Integer;
  prevSelectedObj: TObject;

begin
  if (SelectedTabIndex > -1) and not (csDestroying in ComponentState) then
  begin
    if SelectedTabIndex > FTabs.Count - 1 then
      SelectedTabIndex := FTabs.Count - 1;
    if SelectedTabIndex <> FSelectedTab then
    begin
      prevSelectedTab := FSelectedTab;
      if (prevSelectedTab>=0) and (prevSelectedTab<FTabs.Count) then
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
        if (Assigned(TTab(FTabs[FSelectedTab]).TabPanel)) then
        begin
          TTab(FTabs[FSelectedTab]).TabPanel.Show;
          TTab(FTabs[FSelectedTab]).TabPanel.BringToFront;
        end;

      // Call event if assigned.
      if (Assigned(FOnPageChange)) then
        FOnPageChange(self,
          prevSelectedTab, prevSelectedObj,
          FSelectedTab, TTab(FTabs[FSelectedTab]).Obj);

      ComputeTabs;
      Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabDragDrop(Sender, Source: TObject; X, Y: Integer);

begin
  SelectedTab := GetTabIndexFromPos(X, Y);
  if Assigned(OnDragDrop) then
    OnDragDrop(Sender, Source, X, Y);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);

begin
  FHighlightedTab := GetTabIndexFromPos(X, Y);
  Invalidate;
  Update;

  if Assigned(OnDragOver) then
    OnDragOver(Sender, Source, X, Y, State, Accept);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabMenuItemSelected(Sender: TObject);

begin
  SelectedTab := TTntMenuItem(Sender).Tag;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabPaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y:
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
  if (FShowButtons) and (not(FHideNewButton)) then
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
    if (X < PreAreaWidth) and (FShowButtons) and (not(FHideNewButton)) then
    begin
      if (Assigned(FOnRequestNewPage)) then
        FOnRequestNewPage(self);
    end
    else
      // User clicked on Tabsheet
      if (X < FActionBarStart -
        (PreAreaWidth +
          (SeparatorWidth + FListTabPNGImg.Width) * Ord(FShowButtons))) then
      begin
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
      else
        // User clicked on Tab List icon.
        if (X >= FActionBarStart - SeparatorWidth - FListTabPNGImg.Width) and
          (X < FActionBarStart) then
        begin
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

          FTabsPopupMenu.Popup(Pnt.X + FActionBarStart - 2 * PreAreaWidth + 1, Pnt.Y + 16);
        end
        else
          if (X >= FActionBarStart) then
          begin
            if (X >= FActionBtnApplyXPos) and
              (X < FActionBtnDiscardXPos) then
            begin
              if (Assigned(FApplyAction)) then
                FApplyAction.Execute;
            end
            else
              if (X >= FActionBtnDiscardXPos) and
                (X < Width) then
              begin
                if (Assigned(FDiscardAction)) then
                  FDiscardAction.Execute;
              end;
          end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabPaintBoxMouseLeave(Sender: TObject);

begin
  FHighlightedTab := -1;
  FHighlightedButton := -1;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

var
  PreAreaWidth: Integer;
  ActiveHeight: Integer;
  TopSpace: Integer;

begin
  FHighlightedButton := -1;
  FHighlightedTab := -1;

  PreAreaWidth := SeparatorWidth;
  if FShowButtons and (not(FHideNewButton)) then
    Inc(PreAreaWidth, FNewTabPNGImg.Width);

  if FTopSpacer then
    TopSpace := 4
  else
    TopSpace := 0;

  ActiveHeight := Tabheight - 7 - BottomSpace + TopSpace;

  if (Y < ActiveHeight) then
  begin
    // User clicked on New Tab icon
    if (X < PreAreaWidth) and FShowButtons and (not(FHideNewButton)) then
    begin
      FHighlightedButton := 0;
      Invalidate;
    end
    else
      // User clicked on Tabsheet
      if (X < FActionBarStart -
        (PreAreaWidth +
          (SeparatorWidth + FListTabPNGImg.Width) * Ord(FShowButtons))) then
      begin
        FHighlightedTab := GetTabIndexFromPos(X, Y);
        if FHighlightedTab > -1 then
          Invalidate;
      end
      else
        // User clicked on Tab List icon.
        if (X >= FActionBarStart - SeparatorWidth - FListTabPNGImg.Width) and
          (X < FActionBarStart) then
        begin
          FHighlightedButton := 2;
          Invalidate;
        end
        else
          if (X >= FActionBarStart) then
          begin
            Invalidate;

            if (X >= FActionBtnApplyXPos) and
              (X < FActionBtnDiscardXPos) then
            begin
            end
            else
              if (X >= FActionBtnDiscardXPos) and
                (X < Width) then
              begin
              end;
          end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.YC(Y: Integer): Integer;

begin
  if not(FTabsAtBottom) then
    Result := Y
  else
    Result := Height - Y - 1 - 4;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.PaintEditButton(Canvas: TCanvas; Caption: WideString;
  var xpos: Integer; PNGImg: TPNGObject; DisabledPNGImg: TPNGObject;
  Enabled: Boolean; ActivePNGImg: TPNGObject; Active: Boolean): Integer;

var
  i, l, w, x, YO, OldFontSize: Integer;
  s: WideString;

begin
  YO := 3;

  with Canvas do
  begin
    Pen.Color := $009C9B91;

    if (Width > 550) then
    begin
      s := _(Caption);

      l := GetWideStringTextWidth(Canvas, s);

      if (Active) then
      begin
        x := xpos - 7 - l - 21 - 4;
        w := 2 + l + 21 + 4;

        FBtnDownLeftBgImg.Draw(Canvas,
          Rect(x, 0,
            x+FBtnDownLeftBgImg.Width, FBtnDownLeftBgImg.Height));

        i := x+FBtnDownLeftBgImg.Width;
        while (i<x+w-FBtnDownBgImg.Width) do
        begin
          FBtnDownBgImg.Draw(Canvas,
            Rect(i, 0, i+FBtnDownBgImg.Width, FBtnDownBgImg.Height));

          inc(i, FBtnDownBgImg.Width);
        end;

        FBtnDownBgImg.Draw(Canvas,
          Rect(x+w-FBtnDownBgImg.Width, 0,
            x+w, FBtnDownBgImg.Height));
      end;

      xpos := xpos - 2 - l;

      if (Enabled) or (Active) then
        Font.Color := clBlack
      else
        Font.Color := $00CCCCCC;

      OldFontSize := Font.Size;
      Font.Size := 7;

      SetBkMode(Canvas.Handle, TRANSPARENT);
      DrawWideStringText(Canvas.Handle, PWideChar(s),
        Length(s), Rect(xpos+2*Ord(Active), 4+2*Ord(Active) + YO,
          xpos + l + 2*Ord(Active), 4 + 18 + 2*Ord(Active) + YO));

      Font.Size := OldFontSize;

      xpos := xpos - 21;
    end
    else
    begin
      if (Active) then
      begin
        x := xpos - 7 - 16 - 4;
        w := 2 + 16 + 4;

        FBtnDownLeftBgImg.Draw(Canvas,
          Rect(x, 0,
            FBtnDownLeftBgImg.Width, FBtnDownLeftBgImg.Height));

        i := FBtnDownLeftBgImg.Width;
        while (i<w) do
        begin
          FBtnDownBgImg.Draw(Canvas,
            Rect(x+i, 0,
              FBtnDownBgImg.Width, FBtnDownBgImg.Height));

          inc(i, FBtnDownBgImg.Width);
        end;

        FBtnDownBgImg.Draw(Canvas,
          Rect(x+w-FBtnDownBgImg.Width, 0,
            x+w, FBtnDownBgImg.Height));
      end;

      xpos := xpos - 2 - 16;
    end;

    if (Active) then
      ActivePNGImg.Draw(Canvas,
        Rect(xpos + 1 + 2*Ord(Active), 2 + 2*Ord(Active) + YO,
          xpos + 1 + ActivePNGImg.Width + 2*Ord(Active),
          2 + ActivePNGImg.Height + 2*Ord(Active) + YO))
    else
      if (Enabled) then
        PNGImg.Draw(Canvas,
          Rect(xpos + 1, 2 + YO,
            xpos + 1 + PNGImg.Width, 2 + PNGImg.Height + YO))
      else
        DisabledPNGImg.Draw(Canvas,
          Rect(xpos + 1, 2 + YO,
            xpos + 1 + DisabledPNGImg.Width,
            2 + DisabledPNGImg.Height + YO));

    xpos := xpos - 5;

    MoveTo(xpos, 0);
    LineTo(xpos, Height);
  end;

  Result := xpos;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.TabPaintBoxPaint(Sender: TObject);

var
  I, TopSpace: Integer;
  CurrentLeft, Y: Integer;
  Tab: TTab;
  CaptionRect: TRect;
  DrawFlags: Cardinal;
  DrawListButton: Boolean;
  Bottom: Integer;
  TempColor: TRGB;
  R: TRect;
  YO: Integer;

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

    if (FTabsAtBottom) then
      YO := 4
    else
      YO := 0;

    Font.Assign(Self.Font);

    // Initialize background. Since we are double-buffering it is more efficient to erase the whole background in
    // one go instead in several steps.
    if not(FInversColors) then
      Brush.Color := clBtnFace
    else
      Brush.Color := clWhite;
    FillRect(ClientRect);

    // Bottom white line
    if not(FInversColors) then
      Brush.Color := clWhite
    else
      Brush.Color := clBtnFace;

    FillRect(Rect(1, YC(23 - 4 + TopSpace),
      Width - 1, YC(Height)));


    // draw action buttons
    if (FShowApplyDiscardChangesBtns) then
    begin
      FActionBarStart := Width;

      FActionBtnDiscardXPos := PaintEditButton(
        FPaintBoxBufferedImage.Canvas,
        _('Discard Changes'), FActionBarStart, FDiscardPNGImg,
        FDiscardDisabledPNGImg,
        ((DiscardAction <> nil) and (DiscardAction.Enabled)));

      FActionBtnApplyXPos := PaintEditButton(
        FPaintBoxBufferedImage.Canvas,
        _('Apply Changes'), FActionBarStart, FApplyPNGImg,
        FApplyDisabledPNGImg,
        ((ApplyAction <> nil) and (ApplyAction.Enabled)));

      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
    end
    else
      FActionBarStart := Width;


    CurrentLeft := SeparatorWidth;
    Bottom := Tabheight - 7 - BottomSpace + TopSpace;

    // Left vertical dark border line.
    Pen.Color := TabBorderColor;
    MoveTo(0, YC(Tabheight - BottomSpace + TopSpace));
    LineTo(0, YC(Bottom - 1));

    // Draw Buttons if enabled.
    if (FShowButtons) and (not(FHideNewButton)) then
    begin
      if FHighlightedButton = 0 then
        FNewTabPNGImg.Draw(FPaintBoxBufferedImage.Canvas, Rect(3, YO + 0, 3 +
          FNewTabPNGImg.Width, YO + FNewTabPNGImg.Height))
      else
        FNewTabPNGImg.DrawFaded(FPaintBoxBufferedImage.Canvas, Rect(3, YO + 0,
          3 + FNewTabPNGImg.Width, YO + FNewTabPNGImg.Height), 60);

      Inc(CurrentLeft, FNewTabPNGImg.Width);
    end;

    // Draw horizontal line left to the first tab.
    LineTo(CurrentLeft, YC(Bottom - 1));

    // Draw tabulators.
    DrawFlags := DT_SINGLELINE or DT_NOCLIP or DT_VCENTER;
    CaptionRect := Rect(0, 0, 0, 0);
    DrawListButton := False;
    for I := 0 to FTabs.Count - 1 do
    begin
      Tab := TTab(FTabs[I]);

      R := Tab.FBounds;
      R.Top := YC(R.Top);
      R.Bottom := YC(R.Bottom);

      if R.Right > (FActionBarStart - SeparatorWidth) then
      begin
        // Stop painting if we filled up the client area.
        DrawListButton := True;
        Break;
      end;

      if FSelectedTab = I then
      begin
        // This is the active tab.
        if not(FInversColors) then
          Brush.Color := clWhite
        else
          Brush.Color := clBtnFace;
        FillRect(R);

        with R do
        begin
          LineTo(Left, Top);
          LineTo(Right, Top);
          LineTo(Right, YC(Tab.FBounds.Bottom - 1));
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
          with R do
          begin
            if not(FTabsAtBottom) then
              RoundRect(Left + 2, Tab.FBounds.Top + 1,
                Right - 2, Tab.FBounds.Bottom - 2, 3, 3)
            else
              RoundRect(Left + 2, Tab.FBounds.Top + 1 + 1,
                Right - 2, Tab.FBounds.Bottom - 2 + 1, 3, 3);
          end;

          Pen.Color := TabBorderColor;
        end;
        LineTo(R.Right, YC(Tab.FBounds.Bottom - 1));
      end;

      CurrentLeft := R.Left + SeparatorWidth;

      if not (FSelectedTab = I) and (Tab.RegularTextWidth <> 0) then
        CurrentLeft := CurrentLeft + (Tab.BoldTextWidth - Tab.RegularTextWidth) div 2;

      if Tab.FImage <> nil then
      begin
        Y := (Tab.FBounds.Bottom - Tab.FBounds.Top - Tab.FImage.Height) div 2;
        if FSelectedTab = I then
          Tab.FImage.Draw(FPaintBoxBufferedImage.Canvas,
            Rect(CurrentLeft, Y,
              Tab.FImage.Width, Tab.FImage.Height))
        else
          Tab.FImage.DrawFaded(FPaintBoxBufferedImage.Canvas,
            Rect(CurrentLeft, Y,
              Tab.FImage.Width, Tab.FImage.Height), 200);

        Inc(CurrentLeft, Tab.FImage.Width + SeparatorWidth);
      end;

      CaptionRect := Tab.FBounds;
      CaptionRect.Left := CurrentLeft;
      if FShowDeleteButtons and (FTabCloseImage <> nil) then
      begin
        Dec(CaptionRect.Right, FTabCloseImage.Width + SeparatorWidth);

        // The close button image is only drawn if this is the hot or the active tab.
        if ((FHighlightedTab = I) or (FSelectedTab = I)) and
          ((FTabs.Count > 1) or (FAllowToCloseLastTab)) then
        begin
          Y := (Tab.FBounds.Bottom - Tab.FBounds.Top -
            FTabCloseImage.Height) div 2 + 1;

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

      CaptionRect.Top := CaptionRect.Top;
      CaptionRect.Bottom := CaptionRect.Bottom;

      SetBkMode(Handle, TRANSPARENT);
      if IsWinNTPlatform then
        Windows.DrawTextW(Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags)
      else
        DrawTextW(Handle, PWideChar(Tab.Caption), Length(Tab.Caption), CaptionRect, DrawFlags, True);
    end;

    // Draw border line from last tab to the right and then down.
    LineTo(Width - 1, YC(Bottom - 1));
    LineTo(Width - 1, YC(Tabheight - BottomSpace + TopSpace));

    if FShowButtons then
    begin
      if DrawListButton then
      begin
        if (FHighlightedButton = 2) then
          FListTabPNGImg.Draw(FPaintBoxBufferedImage.Canvas,
            Rect(FActionBarStart - SeparatorWidth - FListTabPNGImg.Width, YO + 0,
              FActionBarStart - SeparatorWidth, YO + FListTabPNGImg.Height))
        else
          FListTabPNGImg.DrawFaded(FPaintBoxBufferedImage.Canvas,
            Rect(FActionBarStart - SeparatorWidth - FListTabPNGImg.Width, YO + 0,
              FActionBarStart - SeparatorWidth, YO + FListTabPNGImg.Height), 60);

        FTabListBtnShown := True;
      end
      else
        FTabListBtnShown := False;
    end;

    if FDrawBottom then
    begin
      MoveTo(Width - 1, YC(TabHeight - 4 + TopSpace));
      LineTo(Width - 1, YC(Height - 1));
      LineTo(0, YC(Height - 1));
      LineTo(0, YC(TabHeight - 4 + TopSpace));
    end;
  end;

  FPaintBox.Canvas.Draw(0, 0, FPaintBoxBufferedImage);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.DeleteTab(TabIndex: Integer; AllowDeletionOfLastTab: Boolean; ForceDeletion: Boolean): Boolean;

var
  CanClose: Boolean;
  LastActiveTab: Integer;
  Count: Integer;
  I: Integer;
  Reference: TObject;
  Tab: TTab;

begin
  Result := True;

  if (((FTabs.Count > 1) or AllowToCloseLastTab) and
    (TabIndex > -1) and (TabIndex < FTabs.Count)) or
    (AllowDeletionOfLastTab and (FTabs.Count = 1)) then
  begin
    CanClose := True;

    if not ForceDeletion then
    begin
      if Assigned(FOnCanClosePage) then
        FOnCanClosePage(FTabs[TabIndex], CanClose);
      if not CanClose then
        Result := False;
    end;

    if CanClose then
    begin
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

      if (TabCount = 0) then
        FSelectedTab := -1;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.AddTabSheet(AOwner: TComponent; Caption, ImageName: WideString; TabPanel: TTntPanel;
  Obj: TObject; SelectNewPage: Boolean; DockTabPanel: Boolean; TabPanelHeight: Integer; FreeOptions: TFreeOptions;
  AlignTabPanel: Boolean): Integer;

begin
  FTabs.Add(TTab.Create(AOwner, self, Caption, ImageName, TabPanel, Obj, DockTabPanel, FreeOptions));
  ComputeTabs;
  Invalidate;

  if (TabPanel <> nil) then
  begin
    TabPanel.Visible := False;

    if (AlignTabPanel) then
    begin
      TabPanel.Parent := Parent;
      //Get new Handles for all docked controls
      InitHandles(TabPanel);

      TabPanel.Align := alClient;
    end
    else
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
  begin
    TabPanel.Visible := True;
    SelectedTab := FTabs.Count - 1;
  end;

  Result := FTabs.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.CanClose: Boolean;

// Determines if all tabs can be closed and returns True if so, otherwise False.
// FOnCanClosePage must be assigned otherwise True is always returned.

var
  I: Integer;
  CanClosePage: Boolean;

begin
  Result := True;
  if Assigned(FOnCanClosePage) then
  begin
    CanClosePage := True;
    for I := 0 to FTabs.Count - 1 do
    begin
      FOnCanClosePage(FTabs[I], CanClosePage);
      if not CanClosePage then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.ClearTabSheets;

begin
  while FTabs.Count > 0 do
    DeleteTab(0, True);
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.TabCount: Integer;

begin
  Result := FTabs.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.Show;
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

procedure TMyxTabHeaderFrame.Hide;

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

procedure TMyxTabHeaderFrame.TntFrameResize(Sender: TObject);

var
  i: Integer;

begin
  for i := 0 to FTabs.Count - 1 do
  begin
    if (TTab(FTabs[i]).DockTabPanel) and (TTab(FTabs[i]).TabPanel <> nil) then
    begin
      TTab(FTabs[i]).TabPanel.Left := Left + 4;
      TTab(FTabs[i]).TabPanel.Top := Top + 23;
      TTab(FTabs[i]).TabPanel.Width := Width - 9;
      TTab(FTabs[i]).TabPanel.Height := Height - 28;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SelectNextTabSheet;

var
  NextTab: Integer;

begin
  NextTab := FSelectedTab + 1;
  if (NextTab >= TabCount) then
    NextTab := 0;

  SelectedTab := NextTab;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.SelectPreviousTabSheet;

var
  NextTab: Integer;

begin
  NextTab := FSelectedTab - 1;
  if (NextTab < 0 ) then
    NextTab := TabCount - 1;

  SelectedTab := NextTab;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMyxTabHeaderFrame.GetTabIndex(Tab: TTab): Integer;

begin
  Result := FTabs.IndexOf(Tab);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.RenameTabsheetMIClick(Sender: TObject);

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

procedure TMyxTabHeaderFrame.TabHeaderPopupMenuPopup(Sender: TObject);

begin
  CloseTabsheetMI.Enabled := (TabCount>1) or FAllowToCloseLastTab;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMyxTabHeaderFrame.CloseTabsheetMIClick(Sender: TObject);

begin
  if (FClickedTabIndex >= 0) and (FClickedTabIndex < TabCount) then
    DeleteTab(FClickedTabIndex);
end;

//----------------- TTab -----------------------------------------------------------------------------------------------

constructor TTab.Create(AOwner: TComponent; TabHeader: TMyxTabHeaderFrame; Caption, ImageName: WideString;
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

