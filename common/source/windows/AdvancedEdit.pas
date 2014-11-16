unit AdvancedEdit;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, TntForms,
  Dialogs, ExtCtrls, StdCtrls, Themes, PNGImage, AuxFuncs, TntStdCtrls;

type
  TAdvancedEditFrame = class(TTntFrame)
    SearchEd: TTntEdit;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure SearchEdChange(Sender: TObject);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FrameResize(Sender: TObject);
  private
    FNHCanvas: TCanvas;
    SearchPNGImg: TPNGObject;
    SearchWithPopupPNGImg: TPNGObject;
    ClearSearchPNGImg: TPNGObject;
    XPStyleEnabled: Boolean;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure DoPaint;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  PNGTools;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TAdvancedEditFrame.Create(Owner: TComponent);

begin
  inherited Create(Owner);

  SearchPNGImg := LoadPNGImageFromResource('magnify_glass', nil);
  SearchWithPopupPNGImg := LoadPNGImageFromResource('magnify_glass_with_popup', nil);
  ClearSearchPNGImg := LoadPNGImageFromResource('clear_search', nil);

  FNHCanvas := TControlCanvas.Create;
  TControlCanvas(FNHCanvas).Control := Self;

  XPStyleEnabled := GetXPStyleEnabled;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TAdvancedEditFrame.Destroy;

begin
  FNHCanvas.Free;
  SearchPNGImg.Free;
  SearchWithPopupPNGImg.Free;
  ClearSearchPNGImg.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.WMPaint(var Message: TWMPaint);

begin
  PaintHandler(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.PaintWindow(DC: HDC);

begin
  FNHCanvas.Handle := DC;
  try
    DoPaint;
  finally
    FNHCanvas.Handle := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.DoPaint;

begin
  if (XPStyleEnabled) then
  begin
    ThemeServices.DrawElement(FNHCanvas.Handle, ThemeServices.GetElementDetails(teEditTextNormal), Rect(0, 0, Width, Height));
  end
  else
  begin
    FNHCanvas.Brush.Color := clWhite;
    FNHCanvas.Pen.Color := $00B99D7F;
    FNHCanvas.Rectangle(0, 0, Width, Height);
  end;

  if (PopupMenu <> nil) then
  begin
    if SearchEd.Enabled then
      SearchWithPopupPNGImg.Draw(FNHCanvas, Rect(3, 2, 3 + 19, 2 + 18))
    else
      SearchWithPopupPNGImg.DrawFaded(FNHCanvas, Rect(3, 2, 3 + 19, 2 + 18), 200)
  end
  else
  begin
    if SearchEd.Enabled then
      SearchPNGImg.Draw(FNHCanvas, Rect(3, 2, 3 + 19, 2 + 18))
    else
      SearchPNGImg.DrawFaded(FNHCanvas, Rect(3, 2, 3 + 19, 2 + 18));
  end;

  if (SearchEd.Text <> '') then
    ClearSearchPNGImg.Draw(FNHCanvas, Rect(Width - 16 - 4, 3, Width - 4, 3 + 16));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.SearchEdChange(Sender: TObject);

begin
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if SearchEd.Enabled and (ssLeft in Shift) then
  begin
    if (PopupMenu <> nil) then
      if (X < 19 + 3) then
        PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    if (X > Width - 16 - 4) then
    begin
      SearchEd.Text := '';
      SearchEdChange(self);
    end;
  end;

  if SearchEd.CanFocus then
    SearchEd.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.FrameResize(Sender: TObject);

begin
  SearchEd.Top := 5;
  SearchEd.Left := 26;
  SearchEd.Width := Width - 186 + 138;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdvancedEditFrame.CMEnabledChanged(var Message: TMessage);

begin
  inherited;
  SearchEd.Enabled := Enabled;
  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

