unit SQLErrorGrid;

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
  Windows, SysUtils, Classes, Contnrs, Graphics, Controls,
  VirtualTrees, gnugettext, PNGImage,
  MySQLConnection, MySQLResultSet, AuxLists, TntMenus, Forms,
  UCESQLHighlighter, UCEHighlighter, UCEEditorKeyCommands,
  UCEHTMLHighlighter, UCEShared, UniCodeEditor, LexicalTools,
  TextSearch, Options, CommonTypes;

type
  TScriptMessageType = (
    smtError,
    smtWarning
  );

  TSQLErrorGrid = class;
  
  TSelectLineEvent = procedure(Sender: TSQLErrorGrid; Line, Position: Integer) of object;
  
  TSQLErrorGrid = class(TVirtualStringTree, IOptionChangeListener)
  private
    ErrorPopupMenu: TTntPopupMenu;
    FOptionProvider: IOptionProvider;
    RSErrorPNGImg,
    RSWarningPNGImg: TPNGObject;

    FOnSelectLine: TSelectLineEvent;
  protected
    procedure ClearMessagesItemClick(Sender: TObject);
    procedure CopyToClipboardItemClick(Sender: TObject);
    procedure CopyToClipboardSelectedItemClick(Sender: TObject);
    procedure DblClick; override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString); override;
    procedure DoSelectLine(Line, Position: Integer); virtual;
    procedure OptionChanged;
    procedure UpdateFontsAndHeights;
  public
    constructor Create(AOwner: TComponent; OptionProvider: IOptionProvider); reintroduce;
    destructor Destroy; override;
    
    procedure AddError(Msg: WideString; MsgType: TScriptMessageType; MsgNr: Integer; Line: Integer; CharPos: Integer);
    procedure Clear; override;
  published
    property OnSelectLine: TSelectLineEvent read FOnSelectLine write FOnSelectLine;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, PNGTools, TntClipbrd;
  
type
  PScriptError = ^TScriptError;
  TScriptError = record
    Msg: WideString;
    MsgType: TScriptMessageType;
    MsgNr: Integer;
    MsgNrAsString: WideString;
    Line: Integer;
    LineAsString: WideString;
    CharPos: Integer;
  end;


//----------------- TScriptErrorGrid -----------------------------------------------------------------------------------

constructor TSQLErrorGrid.Create(AOwner: TComponent; OptionProvider: IOptionProvider);

var
  Column: TVirtualTreeColumn;
  PopupItem: TTntMenuItem;

begin
  inherited Create(AOwner);

  FOptionProvider := OptionProvider;
  OptionProvider.AddListener(Self);
  NodeDataSize := SizeOf(TScriptError);

  Header.Options := Header.Options + [hoDblClickResize, hoVisible];
  UpdateFontsAndHeights;
  Column := Header.Columns.Add;
  Column.Text := '!';
  Column.Width := 16;
  Column := Header.Columns.Add;
  Column.Text := 'Line';
  Column.Width := 50;
  Column := Header.Columns.Add;
  Column.Text := 'Description';
  Column := Header.Columns.Add;
  Column.Text := 'ErrorNr.';
  Column.Width := 70;
  Header.AutoSizeIndex := 2;
  Header.Options := Header.Options + [hoAutoResize] - [hoColumnResize];
  ParentBackground := False;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines] + [toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect, toMultiSelect];
  TreeOptions.StringOptions := TreeOptions.StringOptions - [toSaveCaptions];
  HintMode := hmTooltip;
  ShowHint := True;
  DefaultNodeHeight := 16;
  Align := alBottom;
  Height := 0;

  ErrorPopupMenu := TTntPopupMenu.Create(AOwner);
  PopupMenu := ErrorPopupMenu;

  PopupItem := TTntMenuItem.Create(ErrorPopupMenu);
  PopupItem.Caption := _('Copy all to clipboard');
  PopupItem.OnClick := CopyToClipboardItemClick;
  ErrorPopupMenu.Items.Add(PopupItem);

  PopupItem := TTntMenuItem.Create(ErrorPopupMenu);
  PopupItem.Caption := _('Copy selected lines to clipboard');
  PopupItem.OnClick := CopyToClipboardSelectedItemClick;
  ErrorPopupMenu.Items.Add(PopupItem);

  PopupItem := TTntMenuItem.Create(ErrorPopupMenu);
  PopupItem.Caption := '-';
  ErrorPopupMenu.Items.Add(PopupItem);

  PopupItem := TTntMenuItem.Create(ErrorPopupMenu);
  PopupItem.Caption := _('Clear Messages');
  PopupItem.OnClick := ClearMessagesItemClick;
  ErrorPopupMenu.Items.Add(PopupItem);

  RSErrorPNGImg := LoadPNGImageFromResource('rs_error');
  RSWarningPNGImg := LoadPNGImageFromResource('rs_warning');
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSQLErrorGrid.Destroy;

begin
  FOptionProvider.RemoveListener(Self);

  RSErrorPNGImg.Free;
  RSWarningPNGImg.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.AddError(Msg: WideString; MsgType: TScriptMessageType; MsgNr: Integer; Line: Integer;
  CharPos: Integer);

var
  Node: PVirtualNode;
  Nodedata: PScriptError;

begin
  Node := AddChild(nil);
  NodeData := GetNodeData(Node);
  NodeData.Msg := Msg;
  NodeData.MsgType := MsgType;
  NodeData.MsgNr := MsgNr;
  if MsgNr <> 0 then
    NodeData.MsgNrAsString := IntToStr(MsgNr)
  else
    NodeData.MsgNrAsString := '';
  NodeData.Line := Line;
  NodeData.LineAsString := IntToStr(Line);
  NodeData.CharPos := CharPos;
  InvalidateNode(Node);
  Multiline[Node] := True;
  NodeHeight[Node] := ComputeNodeHeight(Canvas, Node, 2, Msg);

  FocusedNode := Node;

  if RootNodeCount = 1 then
   ClientHeight := 1; // Client height bug. The first setting seems not to be honoured.
  ClientHeight := Min(RootNode.TotalHeight - DefaultNodeHeight, 200);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.Clear;

begin
  Height := 0;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.ClearMessagesItemClick(Sender: TObject);

begin
  Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.CopyToClipboardItemClick(Sender: TObject);

var
  NodeData: PScriptError;
  Run: PVirtualNode;
  Text: WideString;
  
begin
  Text := '';
  Run := GetFirst;
  while Assigned(Run) do
  begin
    NodeData := GetNodeData(Run);
    Text := WideFormat('%sScript line: %s'#9'%s'#13#10, [Text, NodeData.LineAsString, NodeData.Msg]);
    Run := GetNext(Run);
  end;
  TntClipboard.AsWideText := Text;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.CopyToClipboardSelectedItemClick(Sender: TObject);

var
  NodeData: PScriptError;
  Run: PVirtualNode;
  Text: WideString;
  
begin
  Text := '';
  Run := GetFirst;
  while Assigned(Run) do
  begin
    if Selected[Run] then
    begin
      NodeData := GetNodeData(Run);
      Text := WideFormat('%sScript line: %s'#9'%s'#13#10, [Text, NodeData.LineAsString, NodeData.Msg]);
    end;
    Run := GetNext(Run);
  end;
  TntClipboard.AsWideText := Text;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.DblClick;

var
  NodeData: PScriptError;

begin
  if (Assigned(OnDblClick)) then
    OnDblClick(Self)
  else
  begin
    NodeData := nil;
    if Assigned(FocusedNode) then
      NodeData := GetNodeData(FocusedNode);

    if Assigned(NodeData) then
      DoSelectLine(NodeData.Line - 1, NodeData.CharPos - 1);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PScriptError;

begin
  if Column = 0 then
  begin
    NodeData := GetNodeData(Node);

    case NodeData.MsgType of
      smtError:
        RSErrorPNGImg.Draw(Canvas, Rect(4, 2, RSErrorPNGImg.Width + 1, RSErrorPNGImg.Height + 1));
      smtWarning:
        RSWarningPNGImg.Draw(Canvas, Rect(4, 2, RSWarningPNGImg.Width + 1, RSWarningPNGImg.Height + 1));
    end;
  end;

  if Assigned(OnAfterCellPaint) then
    OnAfterCellPaint(Self, Canvas, Node, Column, CellRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.DoFreeNode(Node: PVirtualNode);

var
  NodeData: PScriptError;

begin
  inherited;

  NodeData := GetNodeData(Node);
  Finalize(NodeData^);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: PScriptError;

begin
  if (Assigned(OnGetText)) then
    OnGetText(Self, Node, Column, TextType, CellText)
  else
  begin
    NodeData := GetNodeData(Node);
    case Column of
      0:
        CellText := '';
      1:
        CellText := NodeData.LineAsString;
      2:
        CellText := NodeData.Msg;
      3:
        CellText := NodeData.MsgNrAsString;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.DoSelectLine(Line, Position: Integer);

begin
  if Assigned(FOnSelectLine) then
    FOnSelectLine(Self, Line, Position);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.OptionChanged;

begin
  UpdateFontsAndHeights;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLErrorGrid.UpdateFontsAndHeights;

begin
  Font.Name := FOptionProvider.OptionAsString['CodeFontName'];
  Font.Height := FOptionProvider.OptionAsInteger['CodeFontHeight'];
  Header.Font.Name := FOptionProvider.OptionAsString['DataFontName'];
  Header.Font.Size := -12;

  Canvas.Font := Font;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

