unit EditorTableVTEdit;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls, ExtCtrls,
  VirtualTrees, AuxFuncs, CheckLst, TntCheckLst, Buttons, TntButtons,
  MySQLConnection, myx_public_interface, MyxError, ImgList, PngImage,
  TntForms, Options;

const
  WM_DoCellEdit = WM_USER+301;
  WM_EditCurrentCell = WM_USER+300;

type
  TColumnGridEditLink = class;

  TColumnGridEdit = class(TTntEdit)
  private
    FRefLink: IVTEditLink;
    FLink: TColumnGridEditLink;
    procedure CMAutoAdjust(var Message: TMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure CMRelease(var Message: TMessage); message CM_RELEASE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyDown); message WM_KEYUP;
  protected
    procedure AutoAdjustSize;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(Link: TColumnGridEditLink); reintroduce;

    procedure Release; virtual;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property PasswordChar;
  end;

  TColumnGridEditLink = class(TInterfacedObject, IVTEditLink)
  private
    FEdit: TColumnGridEdit;
    FTree: TVirtualStringTree; // A back reference to the tree calling.
    FNode: PVirtualNode;             // The node to be edited.
    FColumn: TColumnIndex;           // The column of the node.
    FAlignment: TAlignment;
    FTextBounds: TRect;              // Smallest rectangle around the text.
    FStopping: Boolean;              // Set to True when the edit link requests stopping the edit action.
    procedure SetEdit(const Value: TColumnGridEdit);
  public
    PDatatypes: PMYX_DBM_DATATYPES;
    InitialText: WideString;

    constructor Create(PDatatypes: PMYX_DBM_DATATYPES;
      InitialText: WideString);
    destructor Destroy; override;

    function BeginEdit: Boolean; virtual; stdcall;
    function CancelEdit: Boolean; virtual; stdcall;
    property Edit: TColumnGridEdit read FEdit write SetEdit;
    function EndEdit: Boolean; virtual; stdcall;
    function GetBounds: TRect; virtual; stdcall;
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message: TMessage); virtual; stdcall;
    procedure SetBounds(R: TRect); virtual; stdcall;
  end;


implementation

uses EditorTable;


//----------------- TColumnGridEdit --------------------------------------------------------------------------------------------

// Implementation of a generic node caption editor.

constructor TColumnGridEdit.Create(Link: TColumnGridEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.CMAutoAdjust(var Message: TMessage);

begin
  AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.CMExit(var Message: TMessage);

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

procedure TColumnGridEdit.CMRelease(var Message: TMessage);

begin
  Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.CNCommand(var Message: TWMCommand);

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

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.WMChar(var Message: TWMChar);

begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.WMDestroy(var Message: TWMDestroy);

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

procedure TColumnGridEdit.WMGetDlgCode(var Message: TWMGetDlgCode);

begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.WMKeyDown(var Message: TWMKeyDown);

// Handles some control keys.

var
  Shift: TShiftState;
  Tree: TBaseVirtualTree;
  Node, NextNode: PVirtualNode;
  Column: integer;
  NodeData: PColumnNodeData;
  JumpNextCell, DoIt: Boolean;
  i: integer;
  PDatatype: PMYX_DBM_DATATYPE;
begin
  Shift := KeyDataToShiftState(Message.KeyData);
  Tree := FLink.FTree;

  // Pretend these keycodes were send to the tree.
  if(Message.CharCode=VK_ESCAPE)then
  begin
    FLink.FTree.CancelEditNode;
    Tree.SetFocus;
  end
  else if(Message.CharCode=VK_RETURN)then
  begin
    Node:=FLink.FNode;
    Column:=FLink.FColumn;

    JumpNextCell:=False;
    NextNode:=Tree.GetNextVisible(Node);
    if(NextNode<>nil)then
    begin
      NodeData:=Tree.GetNodeData(NextNode);
      if(NodeData<>nil)then
        if(NodeData.Column=nil)and(Column=1)then
          JumpNextCell:=True;
    end
    else
      JumpNextCell:=True;


    FLink.FTree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    if(JumpNextCell)and(Text<>'')then
    begin
      if(Column=0)then
      begin
        Tree.FocusedColumn:=1;
        PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
      end
      else
      begin
        Tree.FocusedColumn:=0;
        Tree.FocusedNode:=Tree.GetNextVisible(Node);
        Tree.ClearSelection;
        if(Tree.FocusedNode<>nil)then
          Tree.Selected[Tree.FocusedNode]:=True;

        PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
      end;
    end;
  end
  else if(Message.CharCode=VK_UP)then
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
  else if(Message.CharCode=VK_DOWN)then
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
  else if(((Message.CharCode=VK_LEFT)and(Shift=[ssCtrl]))or
    ((Message.CharCode=VK_TAB)and(Shift=[ssCtrl])))and
    (Tree.FocusedColumn>0)then
  begin
    Tree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    Tree.FocusedColumn:=Tree.FocusedColumn-1;

    PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
  end
  else if(((Message.CharCode=VK_RIGHT)and(Shift=[ssCtrl]))or
    ((Message.CharCode=VK_TAB)and(Shift=[])))and
    (Tree.FocusedColumn<FLink.FTree.Header.Columns.Count-1)then
  begin
    Tree.InvalidateNode(FLink.FNode);
    FLink.FTree.EndEditNode;
    Tree.SetFocus;

    Tree.FocusedColumn:=Tree.FocusedColumn+1;

    PostMessage(TTntForm(Tree.Owner).Handle, WM_DoCellEdit, 0, 0);
  end
  else if(GetCharFromVKey(Message.CharCode)='(')then
  begin
    DoIt:=False;
    PDatatype:=nil;
    for i:=0 to FLink.PDatatypes.datatypes_num-1 do
    begin
      PDatatype:=PMYX_DBM_DATATYPE(integer(FLink.PDatatypes.datatypes)+
        sizeof(MYX_DBM_DATATYPE)*i);

      if(CompareText(PDatatype.name, Text)=0)then
      begin
        DoIt:=True;
        break;
      end;
    end;

    if(DoIt)then
    begin
      Text:=PDatatype.name+')';
      SelStart:=Length(Text)-1;
      SelLength:=0;
    end;
  end
  else
    inherited;
end;

procedure TColumnGridEdit.WMKeyUp(var Message: TWMKeyDown);
var i, xpos: integer;
  PDatatype: PMYX_DBM_DATATYPE;
begin
  inherited;

  //Autocompletion for datatypes
  if(((Message.CharCode>=Ord('a'))and(Message.CharCode<Ord('z')))or
    ((Message.CharCode>=Ord('A'))and(Message.CharCode<Ord('Z')))or
    (Message.CharCode=Ord(' '))or(Message.CharCode=Ord('_')))and
    (FLink<>nil)and(Text<>'')and(FLink.FColumn=1)then
  begin
    if(FLink.PDatatypes<>nil)then
    begin
      for i:=0 to FLink.PDatatypes.datatypes_num-1 do
      begin
        PDatatype:=PMYX_DBM_DATATYPE(integer(FLink.PDatatypes.datatypes)+
          sizeof(MYX_DBM_DATATYPE)*i);
        if(CompareText(
          Copy(PDatatype.name, 1, Length(Text)),
          Text)=0)then
        begin
          xpos:=SelStart;
          Text:=PDatatype.name;
          SelStart:=xpos;
          SelLength:=(Length(PDatatype.name)-xpos);
        end;
      end;
    end;

    Modified:=True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEdit.AutoAdjustSize;

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

procedure TColumnGridEdit.CreateParams(var Params: TCreateParams);

begin
  inherited;

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

procedure TColumnGridEdit.Release;

begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TColumnGridEditLink ------------------------------------------------------------------------------------

constructor TColumnGridEditLink.Create(PDatatypes: PMYX_DBM_DATATYPES;
  InitialText: WideString);

begin
  inherited Create;
  FEdit := TColumnGridEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;

  self.PDatatypes:=PDatatypes;
  self.InitialText:=InitialText;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TColumnGridEditLink.Destroy;

begin
  FEdit.Release;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColumnGridEditLink.BeginEdit: Boolean;

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

procedure TColumnGridEditLink.SetEdit(const Value: TColumnGridEdit);

begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColumnGridEditLink.CancelEdit: Boolean;

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

function TColumnGridEditLink.EndEdit: Boolean;

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

function TColumnGridEditLink.GetBounds: TRect;

begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

function TColumnGridEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;

// Retrieves the true text bounds from the owner tree.

var
  Text: WideString;

begin
  Result := Tree is TVirtualStringTree;
  if Result then
  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    // Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, FEdit.Font, FTextBounds, Text);
    FEdit.Font.Color := clBlack;
    FEdit.Parent := Tree;
    FEdit.RecreateWnd;
    FEdit.HandleNeeded;
    if(InitialText<>'')then
    begin
      FEdit.Text:=InitialText;
      FEdit.Modified:=True;
    end
    else
      FEdit.Text := Text;

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
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEditLink.ProcessMessage(var Message: TMessage);

begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TColumnGridEditLink.SetBounds(R: TRect);

// Sets the outer bounds of the edit control and the actual edit area in the control.

var
  Offset: Integer;

begin
  if not FStopping then
  begin
    with R do
    begin
      // Set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's left/right border.
      if Left < 0 then
        Left := 0;
      if Right - Left < 30 then
      begin
        if FAlignment = taRightJustify then
          Left := Right - 30
        else
          Right := Left + 30;
      end;
      if Right > FTree.ClientWidth then
        Right := FTree.ClientWidth;
      FEdit.BoundsRect := R;

      // The selected text shall exclude the text margins and be centered vertically.
      // We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
      // control leaves around the (selected) text.
      R := FEdit.ClientRect;
      Offset := 2;
      if tsUseThemes in FTree.TreeStates then
        Inc(Offset);
      InflateRect(R, -FTree.TextMargin + Offset, Offset);
      if not (vsMultiline in FNode.States) then
        OffsetRect(R, 0, FTextBounds.Top - FEdit.Top);

      SendMessage(FEdit.Handle, EM_SETRECTNP, 0, Integer(@R));
    end;
  end;
end;

end.
