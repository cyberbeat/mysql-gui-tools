unit MyxTableEditorIndexColumnVTEdit;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, ExtCtrls,
  VirtualTrees, AuxFuncs, CheckLst, TntCheckLst, Buttons, TntButtons,
  MySQLConnection, myx_public_interface, MyxError, ImgList, PngImage,
  TntForms, Options, Grt;

const
  WM_DoCellEdit = WM_USER+301;
  WM_EditCurrentCell = WM_USER+300;

type
  TIndexColumnGridEditLink = class;

  TIndexColumnGridEdit = class(TTntCustomComboBox)
  private
    FRefLink: IVTEditLink;
    FLink: TIndexColumnGridEditLink;
    FModified: Boolean;

    procedure CMAutoAdjust(var Message: TMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    {procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyDown); message WM_KEYUP;}
  protected
    procedure AutoAdjustSize;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    function GetGrt: TGrt;
  public
    constructor Create(Link: TIndexColumnGridEditLink); reintroduce;

    procedure Release; virtual;

    //property AutoSelect;
    property AutoSize;
    //property BorderStyle;
    property CharCase;
    //property HideSelection;
    property MaxLength;
    //property OEMConvert;
    //property PasswordChar;

    property Modified: Boolean read FModified write FModified;

    property Grt: TGrt read GetGrt;
  end;

  TIndexColumnGridEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TIndexColumnGridEdit;
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FAlignment: TAlignment;
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
    procedure SetEdit(const Value: TIndexColumnGridEdit);
  protected
    function GetGrt: TGrt;
  public
    FColumnList: Pointer;
    InitialText: WideString;

    constructor Create(ColumnList: Pointer;
      InitialText: WideString);
    destructor Destroy; override;

    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    property Edit: TIndexColumnGridEdit read FEdit write SetEdit;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;

    property Grt: TGrt read GetGrt;
  end;


implementation

uses MyxTableEditor;


//----------------- TIndexColumnGridEdit --------------------------------------------------------------------------------------------

// Implementation of a generic node caption editor.

constructor TIndexColumnGridEdit.Create(Link: TIndexColumnGridEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;

  FModified:=False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEdit.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.CMAutoAdjust(var Message: TMessage);

begin
  AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.CMExit(var Message: TMessage);

begin
  if Assigned(FLink) and not FLink.FStopping then
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        EndEditNode
      else
        CancelEditNode;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.CMRelease(var Message: TMessage);

begin
  Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.CNCommand(var Message: TWMCommand);

begin
  case Message.NotifyCode of
    EN_UPDATE:
    begin
      if Assigned(FLink) and Assigned(FLink.FTree) and (Message.NotifyCode = EN_UPDATE) and
        not (toGridExtensions in FLink.FTree.TreeOptions.MiscOptions) and
        not (vsMultiline in FLink.FNode.States) then
        // Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
        // and eventual resizing. Hence we use a message to accomplish that.
        if false and ((Win32Platform and VER_PLATFORM_WIN32_NT) <> 0) then
          AutoAdjustSize
        else
          PostMessage(Handle, CM_AUTOADJUST, 0, 0);
    end;
    CBN_CLOSEUP:
    begin
      Modified:=True;
      PostMessage(Handle, WM_KEYDOWN, VK_RETURN, 0);
    end;
    CBN_EDITCHANGE:
      Modified:=True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.WMChar(var Message: TWMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.WMDestroy(var Message: TWMDestroy);

begin
  // If editing stopped by other means than accept or cancel then we have to do default processing for
  // pending changes.
  if Assigned(FLink) and not FLink.FStopping then
  begin
    with FLink, FTree do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FNode, FColumn] := FEdit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.KeyDown(var Key: Word; Shift: TShiftState);

// Handles some control keys.

var
  Tree: TBaseVirtualTree;
  Node, NextNode: PVirtualNode;
  Column: integer;
  NodeData: PIndexColumnNodeData;
  JumpNextCell: Boolean;
  Mgs: TMsg;

begin
  Tree := FLink.FTree;

  // Pretend these keycodes were send to the tree.
  if (Key = VK_ESCAPE) then
  begin
    FLink.FTree.CancelEditNode;
    Tree.SetFocus;
  end
  else if (Key = VK_RETURN) then
  begin
    Key:=0;
    PeekMessage(Mgs, 0, WM_CHAR, WM_CHAR, PM_REMOVE);

    Modified := True;

    Node:=FLink.FNode;
    Column:=FLink.FColumn;

    JumpNextCell:=False;
    NextNode:=Tree.GetNextVisible(Node);
    if (NextNode <> nil) then
    begin
      NodeData:=Tree.GetNodeData(NextNode);
      if (NodeData.IndexColumn = nil) and (Column = 1) then
        JumpNextCell:=True;
    end
    else
      JumpNextCell:=True;


    FLink.FTree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    if (JumpNextCell) and ((Text <> '') or (Column = 2)) then
    begin
      if (Column < 2) then
      begin
        Tree.FocusedColumn := Tree.FocusedColumn + 1;
        PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, Tree.Handle, 0);
      end
      else if (Column = 2) then
      begin
        Tree.FocusedColumn := 0;
        Tree.FocusedNode := Tree.GetNextVisible(Node);
        Tree.ClearSelection;
        if(Tree.FocusedNode<>nil)then
          Tree.Selected[Tree.FocusedNode]:=True;

        PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, Tree.Handle, 0);
      end;
    end;
  end
  else if (Key = VK_UP) and (Tree.FocusedColumn <> 2) then
  begin
    if(Tree.FocusedNode = nil)then
      Node:=Tree.GetLastVisible
    else
      Node:=Tree.GetPreviousVisible(Tree.FocusedNode);

    if(Node<>nil)then
    begin
      Tree.InvalidateNode(FLink.FNode);
      FLink.FTree.EndEditNode;
      Tree.SetFocus;

      Tree.FocusedNode:=Node;
      Tree.ClearSelection;
      Tree.Selected[Tree.FocusedNode]:=True;

      PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
    end;
  end
  else if (Key = VK_DOWN) and (Tree.FocusedColumn <> 2) then
  begin
    if(Tree.FocusedNode = nil)then
      Node:=Tree.GetFirstVisible
    else
      Node:=Tree.GetNextVisible(Tree.FocusedNode);

    if(Node<>nil)then
    begin
      Tree.InvalidateNode(FLink.FNode);
      FLink.FTree.EndEditNode;
      Tree.SetFocus;

      Tree.FocusedNode:=Node;
      Tree.ClearSelection;
      Tree.Selected[Tree.FocusedNode]:=True;

      PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
    end;
  end
  else if (((Key=VK_LEFT) and (Shift=[ssCtrl]))or
    ((Key=VK_TAB) and (Shift=[ssCtrl])))and
    (Tree.FocusedColumn>0) then
  begin
    Tree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    Tree.FocusedColumn:=Tree.FocusedColumn-1;

    PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
  end
  else if (((Key=VK_RIGHT) and (Shift=[ssCtrl])) or
    ((Key=VK_TAB) and (Shift=[]))) and
    (Tree.FocusedColumn<FLink.FTree.Header.Columns.Count-1) then
  begin
    Tree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    Tree.FocusedColumn:=Tree.FocusedColumn+1;

    PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
  end
  else
    inherited;
end;

procedure TIndexColumnGridEdit.KeyUp(var Key: Word; Shift: TShiftState);

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.AutoAdjustSize;

// Changes the size of the edit to accomodate as much as possible of its text within its container window.
// NewChar describes the next character which will be added to the edit's text.

var
  DC: HDC;
  Size: TSize;
  LastFont: THandle;

begin
  if not (vsMultiline in FLink.FNode.States) then
  begin
    // avoid flicker
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

    DC := GetDC(Handle);
    LastFont := SelectObject(DC, Font.Handle);
    try
      // Read needed space for the current text.
      {.$ifdef TntSupport}
        GetTextExtentPoint32W(DC, PWideChar(Text), Length(Text), Size);
      {.$else}
        //GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
      {.$endif TntSupport}
      Inc(Size.cx, 2 * FLink.FTree.TextMargin);

      // Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.FTree.InvalidateNode(FLink.FNode);

      if FLink.FAlignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Height))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Height));
    finally
      SelectObject(DC, LastFont);
      ReleaseDC(Handle, DC);
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.CreateParams(var Params: TCreateParams);

begin
  inherited;

  if Not(Assigned(FLink)) then
    Exit;

  // Only with multiline style we can use the text formatting rectangle.
  // This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if vsMultiline in FLink.FNode.States then
      Style := Style and not (ES_AUTOHSCROLL or WS_HSCROLL) or WS_VSCROLL or ES_AUTOVSCROLL;
    if tsUseThemes in FLink.FTree.TreeStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEdit.Release;

begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TIndexColumnGridEditLink ------------------------------------------------------------------------------------

constructor TIndexColumnGridEditLink.Create(ColumnList: Pointer;
  InitialText: WideString);

begin
  inherited Create;
  FEdit := TIndexColumnGridEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    //BorderStyle := bsSingle;
    AutoSize := False;
  end;

  self.FColumnList := ColumnList;
  self.InitialText := InitialText;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TIndexColumnGridEditLink.Destroy;

begin
  FEdit.Release;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;


//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.BeginEdit: Boolean;

// Notifies the edit link that editing can start now. Descentants may cancel node edit
// by returning False.

begin
  Result := not FStopping;
  if Result then
  begin
    FEdit.Show;
    FEdit.SelectAll;
    FEdit.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEditLink.SetEdit(const Value: TIndexColumnGridEdit);

begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.CancelEdit: Boolean;

begin
  Result := not FStopping;
  if Result then
  begin
    FStopping := True;
    FEdit.Hide;
    FTree.CancelEditNode;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.EndEdit: Boolean;

begin
  Result := not FStopping;
  if Result then
  try
    FStopping := True;
    if FEdit.Modified then
      FTree.Text[FNode, FColumn] := FEdit.Text;
    FEdit.Hide;
    FEdit.FLink := nil;
    FEdit.FRefLink := nil;
  except
    FStopping := False;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.GetBounds: TRect;

begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIndexColumnGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Retrieves the true text bounds from the owner tree.

var
  Text, ItemText: WideString;
  I: Integer;

begin
  Result := Tree is TVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    Text := FTree.Text[Node, Column];
    FTextBounds := FTree.GetDisplayRect(Node, Column, False);

    // some columns need dropdown
    if (Column < 2) then
      FEdit.Style := csDropdownList
    else
      FEdit.Style := csSimple;

    FEdit.Font.Color := clBlack;
    FEdit.Parent := Tree;
    FEdit.RecreateWnd;
    FEdit.HandleNeeded;
    if (InitialText <> '') then
    begin
      FEdit.Text := InitialText;
      FEdit.Modified := True;
    end
    else
      FEdit.Text := Text;

    // fill items
    if (Column = 0) then
    begin
      FEdit.Items.Clear;

      for I := 0 to Grt.ListCount(FColumnList) - 1 do
      begin
        ItemText := Grt.DictString[
          Grt.ListItem[FColumnList, I], 'name'];

        FEdit.Items.Add(ItemText);

        if (ItemText = Text) then
          FEdit.ItemIndex := I;
      end;
    end
    else
      if (Column = 1) then
      begin
        FEdit.Items.Clear;
        ItemText := _('Ascendant');
        FEdit.Items.Add(ItemText);
        if (ItemText = Text) then
          FEdit.ItemIndex := 0;

        ItemText := _('Descendant');
        FEdit.Items.Add(ItemText);
        if (ItemText = Text) then
          FEdit.ItemIndex := 1;
      end;

    if Column <= NoColumn then
    begin
      FEdit.BidiMode := FTree.BidiMode;
      FAlignment := FTree.Alignment;
    end
    else
    begin
      FEdit.BidiMode := FTree.Header.Columns[Column].BidiMode;
      FAlignment := FTree.Header.Columns[Column].Alignment;
    end;

    if FEdit.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);

    FEdit.AutoAdjustSize;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEditLink.ProcessMessage(var Message: TMessage);

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIndexColumnGridEditLink.SetBounds(R: TRect);

begin
  if not FStopping then
  begin
    R := Rect(FTextBounds.Left, FTextBounds.Top,
      FTextBounds.Right, FTextBounds.Bottom);

    FEdit.BoundsRect := R;

    SendMessage(FEdit.Handle, EM_SETRECTNP, 0, Integer(@R));
  end;
end;

end.
