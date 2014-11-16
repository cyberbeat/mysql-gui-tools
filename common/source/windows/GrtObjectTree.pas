unit GrtObjectTree;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, ComCtrls, TntComCtrls,
  TntForms, ExtCtrls, TntExtCtrls, ImgList, Menus, TntMenus,
  StdCtrls, TntStdCtrls, TntSysUtils, StrUtils, TntDialogs,
  TntClipbrd, TntClasses,
  UniCodeEditor, UniCodeConsole, VirtualTrees,
  myx_public_interface, myx_grt_public_interface,
  myx_grt_builtin_module_public_interface,
  PNGImage, AuxFuncs, PngTools,
  Grt;

type
  PGrtObjTreeData = ^TGRTObjTreeData;
  TGRTObjTreeData = record
    Value: Pointer;
    ValueType: MYX_GRT_VALUE_TYPE;
    Key: WideString;
    StructName: WideString;
  end;

  TGrtObjectTree = class (TVirtualStringTree)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  protected
    procedure SetObjectTreeRoot(ObjectTreeRoot: WideString);
    procedure SetDisplayReferenceCountValues(DisplayReferenceCountValues: Boolean);
    procedure SetDisplayObjectValues(DisplayObjectValues: Boolean);
    procedure SetDisplayValueTypes(DisplayValueTypes: Boolean);

    procedure GRTObjTreeAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure GRTObjTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure GRTObjTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure GRTObjTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure GRTObjTreeInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure GRTObjTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure GRTObjTreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GRTObjTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);

  private
    FGrt: TGrt;
    FObjectTreeRootObject: PGrtObjTreeData;
    FObjectTreeRoot: WideString;

    FTreeBtnOpenPNGImg,
    FTreeBtnClosedPNGImg,
    FGrtValueDictPNGImg,
    FGrtValueListPNGImg,
    FGrtValueSimplePNGImg,
    FGrtValueStructPNGImg: TPNGObject;

    FDisplayReferenceCountValues: Boolean;
    FDisplayObjectValues: Boolean;
    FDisplayValueTypes: Boolean;

    FTreeImageList: TImageList;

    FExpandedNodeList: TTntStringList;

    procedure CollectExpandedNodes(Node: PVirtualNode;
      ExpandedNodeList: TTntStringList; Path: WideString = '');
    procedure ReExpandNodes(Node: PVirtualNode;
      ExpandedNodeList: TTntStringList; SelectedObjectPath: WideString = '';
      Path: WideString = '');
  public
    property Grt: TGrt read FGrt write FGrt;
    property ObjectTreeRoot: WideString read FObjectTreeRoot write SetObjectTreeRoot;

    property DisplayReferenceCountValues: Boolean read FDisplayReferenceCountValues write SetDisplayReferenceCountValues;
    property DisplayObjectValues: Boolean read FDisplayObjectValues write SetDisplayObjectValues;
    property DisplayValueTypes: Boolean read FDisplayValueTypes write SetDisplayValueTypes;

    function GetObjectPath(Node: PVirtualNode): WideString;
    procedure RefreshTree;
  end;

implementation

// -----------------------------------------------------------------------------

constructor TGrtObjectTree.Create(AOwner: TComponent);

var
  Bmp: TBitmap;
  tmpStream: TTntResourceStream;
  Col: TVirtualTreeColumn;

begin
  inherited Create(AOwner);

  NodeDataSize := sizeof(TGRTObjTreeData);

  FGrt := Grt;
  FObjectTreeRootObject := nil;

  FDisplayReferenceCountValues := False;
  FDisplayObjectValues := False;
  FDisplayValueTypes := False;
  FExpandedNodeList := TTntStringList.Create;

  OnAfterCellPaint := GRTObjTreeAfterCellPaint;
  OnFreeNode := GRTObjTreeFreeNode;
  OnGetImageIndex := GRTObjTreeGetImageIndex;
  OnGetText := GRTObjTreeGetText;
  OnInitChildren := GRTObjTreeInitChildren;
  OnInitNode := GRTObjTreeInitNode;
  OnMouseDown := GRTObjTreeMouseDown;
  OnPaintText := GRTObjTreePaintText;

  Header.Options := Header.Options - [hoShowSortGlyphs] + [hoAutoResize, hoVisible];

  Col := Header.Columns.Add;
  Col.Text := 'Object Tree';

  TreeOptions.AutoOptions := TreeOptions.AutoOptions -
    [toAutoScrollOnExpand];
  TreeOptions.MiscOptions := TreeOptions.MiscOptions +
    [toGridExtensions];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions -
    [toShowButtons, toShowDropmark, toShowTreeLines] +
    [toShowHorzGridLines, toShowVertGridLines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions +
    [toRightClickSelect];
  TreeOptions.StringOptions := TreeOptions.StringOptions -
    [toSaveCaptions];

  Indent := 16;
  DragType := dtVCL;

  FTreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  FTreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  FGrtValueDictPNGImg := LoadPNGImageFromResource('grt_value_dict');
  FGrtValueListPNGImg := LoadPNGImageFromResource('grt_value_list');
  FGrtValueSimplePNGImg := LoadPNGImageFromResource('grt_value_simple');
  FGrtValueStructPNGImg := LoadPNGImageFromResource('grt_value_struct');

  tmpStream:=TTntResourceStream.Create(HInstance, 'white_8x8', 'BMP');
  try
    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromStream(tmpStream);

      FTreeImageList := TImageList.Create(nil);
      FTreeImageList.Width := 8;
      FTreeImageList.Height := 8;
      FTreeImageList.Masked := True;
      FTreeImageList.Add(Bmp, Bmp);

      Images := FTreeImageList;
    finally
      Bmp.Free;
    end;
  finally
    tmpStream.Free;
  end;
end;

// -----------------------------------------------------------------------------

destructor TGrtObjectTree.Destroy;

begin
  if (FObjectTreeRootObject <> nil) then
    Dispose(FObjectTreeRootObject);

  FTreeBtnOpenPNGImg.Free;
  FTreeBtnClosedPNGImg.Free;

  FGrtValueDictPNGImg.Free;
  FGrtValueListPNGImg.Free;
  FGrtValueSimplePNGImg.Free;
  FGrtValueStructPNGImg.Free;

  FTreeImageList.Free;

  FExpandedNodeList.Free;

  Images := nil;

  inherited;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.CollectExpandedNodes(Node: PVirtualNode;
  ExpandedNodeList: TTntStringList; Path: WideString);

var
  ParentNode: PVirtualNode;
  NodeData,
    ParentNodeData: PGrtObjTreeData;
  Key,
    NewPath: WideString;

begin
  if (Node = nil) then
    Exit;

  NodeData := GetNodeData(Node);
  if (NodeData = nil) then
    Exit;

  ParentNode := Node.Parent;
  ParentNodeData := GetNodeData(ParentNode);

  if (ParentNodeData = nil) then
    key := ''
  else if (ParentNodeData.ValueType = MYX_LIST_VALUE) then
    Key := IntToStr(Node.Index)
  else
    Key := NodeData^.Key;

  if (Path <> '/') then
    NewPath := Path + '/' + Key
  else
    NewPath := Path + Key;

  if (Expanded[Node]) then
  begin
    ExpandedNodeList.Add(NewPath);

    CollectExpandedNodes(GetFirstChild(Node), ExpandedNodeList, NewPath);
  end;

  CollectExpandedNodes(GetNextSibling(Node), ExpandedNodeList, Path);
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.ReExpandNodes(Node: PVirtualNode;
  ExpandedNodeList: TTntStringList; SelectedObjectPath: WideString;
  Path: WideString);

var
  ParentNode: PVirtualNode;
  NodeData,
    ParentNodeData: PGrtObjTreeData;
  Key,
    NewPath: WideString;
  Index: Integer;

begin
  if (Node = nil) then
    Exit;

  NodeData := GetNodeData(Node);
  if (NodeData = nil) then
    Exit;

  ParentNode := Node.Parent;
  ParentNodeData := GetNodeData(ParentNode);

  if (ParentNodeData = nil) then
    Key := ''
  else if (ParentNodeData.ValueType = MYX_LIST_VALUE) then
    Key := IntToStr(Node.Index)
  else
    Key := NodeData^.Key;

  if (Path <> '/') then
    NewPath := Path + '/' + Key
  else
    NewPath := Path + Key;

  if (NewPath = SelectedObjectPath) then
  begin
    Selected[Node] := True;
    FocusedNode := Node;
  end;

  Index := ExpandedNodeList.IndexOf(NewPath);
  if (Index > -1) then
  begin
    ExpandedNodeList.Delete(Index);

    Expanded[Node] := True;

    ReExpandNodes(GetFirstChild(Node), ExpandedNodeList,
      SelectedObjectPath, NewPath);
  end;

  ReExpandNodes(GetNextSibling(Node), ExpandedNodeList,
    SelectedObjectPath, Path);
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.SetObjectTreeRoot(ObjectTreeRoot: WideString);

var
  SelectedObjectPath: WideString;

begin
  SelectedObjectPath := '';
  FExpandedNodeList.Clear;
  if (FObjectTreeRoot = ObjectTreeRoot) then
  begin
    SelectedObjectPath := GetObjectPath(FocusedNode);
    CollectExpandedNodes(GetFirst, FExpandedNodeList);
  end
  else
    FObjectTreeRoot := ObjectTreeRoot;

  if (FGrt = nil) or (FGrt.NativeGrt = nil) then
    Exit;

  if (FObjectTreeRootObject = nil) then
    new(FObjectTreeRootObject);

  FObjectTreeRootObject.Key := ObjectTreeRoot;
  FObjectTreeRootObject.Value := nil;

  // if the given ObjectTreeRoot is / then display the global root
  if (ObjectTreeRoot = '/') then
    FObjectTreeRootObject.Value := myx_grt_get_root(FGrt.NativeGrt)
  else
    // if ObjectTreeRoot starts with /, display the global
    if (Copy(ObjectTreeRoot, 1, 1) = '/') then
      FObjectTreeRootObject.Value :=
        myx_grt_dict_item_get_by_path(FGrt.NativeGrt,
          myx_grt_get_root(FGrt.NativeGrt), ObjectTreeRoot)
    else
      // display the lua shell variable
      FObjectTreeRootObject.Value :=
        myx_grt_shell_get_global_var(FGrt.NativeGrt, ObjectTreeRoot);

  if (FObjectTreeRootObject.Value <> nil) then
    FObjectTreeRootObject.ValueType := myx_grt_value_get_type(
      FObjectTreeRootObject.Value);


  BeginUpdate;
  try
    Clear;

    if (FObjectTreeRootObject.Value <> nil) then
    begin
      if (FObjectTreeRootObject.ValueType = MYX_DICT_VALUE) then
        RootNodeCount := 1
      else
        if (FObjectTreeRootObject.ValueType = MYX_LIST_VALUE) then
          RootNodeCount :=
            myx_grt_list_item_count(FObjectTreeRootObject.Value)
        else
          RootNodeCount := 0;
    end
    else
      RootNodeCount := 0;
  finally
    EndUpdate;
  end;

  ReExpandNodes(GetFirst, FExpandedNodeList, SelectedObjectPath);
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.SetDisplayReferenceCountValues(DisplayReferenceCountValues: Boolean);

begin
  FDisplayReferenceCountValues := DisplayReferenceCountValues;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.SetDisplayObjectValues(DisplayObjectValues: Boolean);

var
  Col: TVirtualTreeColumn;

begin
  FDisplayObjectValues := DisplayObjectValues;

  if (FDisplayObjectValues) then
  begin
    if (Header.Columns.Count = 1) then
    begin
      Col := Header.Columns.Add;
      Col.Text := _('Values');
      Col.Width := 150;
    end;
  end
  else
  begin
    if (Header.Columns.Count > 1) then
      Header.Columns.Delete(1);
  end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.SetDisplayValueTypes(DisplayValueTypes: Boolean);

begin
  if (FDisplayValueTypes <> DisplayValueTypes) then
  begin
    FDisplayValueTypes := DisplayValueTypes;

    if (FDisplayValueTypes) then
      TreeOptions.StringOptions :=
        TreeOptions.StringOptions + [toShowStaticText]
    else
      TreeOptions.StringOptions :=
        TreeOptions.StringOptions - [toShowStaticText];

    Invalidate;
  end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);

var
  NodeData: PGrtObjTreeData;
  TxtRect: TRect;
  x,
  IconIndex: Integer;
  Icon: TPNGObject;
  PImageData: PChar;
  DataLength: Integer;
  PStruct: Pointer;

begin
  if (Column=0) then
  begin
    NodeData := Sender.GetNodeData(Node);

    TxtRect:=Sender.GetDisplayRect(Node, Column, True);

    x:=TxtRect.Left-Sender.OffsetX;

    // Draw > / v
    if (Node.ChildCount>0) or (vsHasChildren in Node.States) then
    begin
      if(Sender.Expanded[Node])then
        FTreeBtnOpenPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
      else
        FTreeBtnClosedPNGImg.Draw(TargetCanvas,
          Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
    end;


    // Draw Icons
    if (NodeData.ValueType=MYX_LIST_VALUE) then
      Icon := FGrtValueListPNGImg
    else
      if (NodeData.ValueType=MYX_DICT_VALUE) and
        (NodeData.StructName='') then
        Icon := FGrtValueDictPNGImg
      else
        if (NodeData.ValueType=MYX_DICT_VALUE) and
          (NodeData.StructName='') then
          Icon := FGrtValueSimplePNGImg
        else
        begin
          IconIndex := FGrt.StructIconsList.IndexOf(NodeData.StructName);

          // check if there is a cached icon for the struct already
          if (IconIndex > -1) then
          begin
            // if the struct was cached, use its icon, if there is any
            Icon := TPNGObject(FGrt.StructIconsList.Objects[IconIndex]);

            // if there is no special icon for the struct, use the default one
            if (Icon = nil) then
              Icon := FGrtValueStructPNGImg;
          end
          else
          begin
            PStruct := myx_grt_struct_get(FGrt.NativeGrt, NodeData.StructName);

            if (PStruct <> nil) then
            begin
              PImageData := _myx_grt_struct_get_icon(FGrt.NativeGrt,
                PChar(ExtractFilePath(Application.ExeName)+'images\structs\'),
                PStruct, MYX_IT_SMALL, @DataLength);

              Icon := LoadPNGImageFromPChar(PImageData, DataLength);

              FGrt.StructIconsList.AddObject(NodeData.StructName, Icon);
            end
            else
              Icon := FGrtValueStructPNGImg;
          end;

        end;

    if (Icon <> nil) then
      Icon.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));
  end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PGrtObjTreeData;

begin
  if (Node<>Sender.RootNode) and (Node<>nil) then
  begin
    NodeData := Sender.GetNodeData(Node);

    if (NodeData.Value<>nil) then
      myx_grt_value_release(NodeData.Value);
    Finalize(NodeData^);
  end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if(Column=0)then
    ImageIndex:=0;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

var
  NodeData: PGrtObjTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Column=0) and (TextType=ttNormal) then
  begin
     CellText := NodeData.Key;
  end
  else
    if (Column=0) and (TextType=ttStatic) then
    begin
      CellText := UTF8Decode(_myx_get_value_type_as_string(NodeData.ValueType));

      if (NodeData.ValueType=MYX_LIST_VALUE) then
      begin
        CellText := CellText + ' [' +
          UTF8Decode(_myx_get_value_type_as_string(myx_grt_list_content_get_type(NodeData.Value)));

        if (NodeData.StructName<>'') then
          CellText := CellText + ': '+NodeData.StructName;

        CellText := CellText + ']';
      end
      else
        if (NodeData.ValueType=MYX_DICT_VALUE) and
          (NodeData.StructName<>'') then
          CellText := CellText + ': '+NodeData.StructName;

      if (FDisplayReferenceCountValues) then
        CellText := CellText + ' (' + IntToStr(
          myx_grt_value_get_current_reference_count(NodeData.Value)) +
          ')';
    end
    else if (Column=1) and (TextType=ttNormal) then
    begin
      case NodeData.ValueType of
        MYX_INT_VALUE:
          CellText := IntToStr(myx_grt_value_as_int(NodeData.Value));
        MYX_REAL_VALUE:
          CellText := FormatFloat('###,###,##0.00',
            (myx_grt_value_as_real(NodeData.Value)));
        MYX_STRING_VALUE:
          CellText := UTF8Decode(_myx_grt_value_as_string(NodeData.Value));
      else
        CellText := '';
      end;
    end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);

var
  NodeData: PGrtObjTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.ValueType=MYX_DICT_VALUE) then
  begin
    if (FDisplayObjectValues) then
      ChildCount := myx_grt_dict_item_count(NodeData.Value)
    else
      ChildCount := myx_grt_dict_item_count_complex(NodeData.Value);
  end
  else
    if (NodeData.ValueType=MYX_LIST_VALUE) then
      ChildCount := myx_grt_list_item_count(NodeData.Value);
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData, ParentNodeData: PGrtObjTreeData;
  ParentNodeData_Type: MYX_GRT_VALUE_TYPE;
  PName: PChar;
  ListContentType: MYX_GRT_VALUE_TYPE;

begin
  NodeData := Sender.GetNodeData(Node);

  // Deal with root node (either "/" or a lua shell variable)
  if ParentNode=nil then
  begin
    if (Node.Index = 0) and
      (FObjectTreeRootObject.Key = '/') then
    begin
      // handle global root value
      NodeData.Key := _('root');
      NodeData.Value := FObjectTreeRootObject.Value;

      // get value type
      NodeData.ValueType := myx_grt_value_get_type(NodeData.Value);
    end
    else
    begin
      // handle dicts and lists
      if (FObjectTreeRootObject.ValueType = MYX_DICT_VALUE) then
      begin
        NodeData.Value := FObjectTreeRootObject.Value;

        if (NodeData.Value = nil) then
        begin
          RootNodeCount := 0;
          Exit;
        end;

        NodeData.StructName := myx_grt_dict_struct_get_name(NodeData.Value);
      end
      else
        if (FObjectTreeRootObject.ValueType = MYX_LIST_VALUE) then
        begin
          NodeData.Value := myx_grt_list_item_get(
            FObjectTreeRootObject.Value, Node.Index);
        end;

      // get value type
      NodeData.ValueType := myx_grt_value_get_type(NodeData.Value);

      // get struct name and Key name
      if (NodeData.ValueType = MYX_DICT_VALUE) then
      begin
        NodeData.StructName := myx_grt_dict_struct_get_name(NodeData.Value);
        NodeData.Key := myx_grt_dict_item_get_as_string(NodeData.Value, 'name');
      end;

      if (NodeData.Key = '') then
      begin
        if (NodeData.ValueType = MYX_DICT_VALUE) then
          NodeData.Key := FObjectTreeRootObject.Key
        else
          NodeData.Key := '[' + IntToStr(Node.Index) + ']';
      end;
    end;

    myx_grt_value_retain(NodeData.Value);

    //The root value must be a dict
    {if NodeData.ValueType<>MYX_DICT_VALUE then
      raise EInOutError.Create(_('The root node / variable has to be a dict value.'));}

    if (FObjectTreeRootObject.ValueType <> MYX_LIST_VALUE) then
      InitialStates := InitialStates + [ivsExpanded];
  end
  else
  begin
    ParentNodeData := Sender.GetNodeData(ParentNode);
    ParentNodeData_Type := myx_grt_value_get_type(ParentNodeData.Value);

    if (ParentNodeData_Type=MYX_LIST_VALUE) then
    begin
      //Get value and retain it
      NodeData.Value :=
        myx_grt_list_item_get(ParentNodeData.Value, Node.Index);
      myx_grt_value_retain(NodeData.Value);

      NodeData.ValueType := myx_grt_value_get_type(NodeData.Value);


      // if this is a reference _id, search value
      if (NodeData.ValueType = MYX_STRING_VALUE) and
        (myx_grt_list_content_get_struct_name(ParentNodeData.Value) <> '') then
      begin
        myx_grt_value_release(NodeData.Value);

        NodeData.Value := myx_grt_reference_cache_lookup(
          FGrt.NativeGrt, myx_grt_value_as_string(NodeData.Value));

        NodeData.ValueType := myx_grt_value_get_type(NodeData.Value);
      end;


      if (NodeData.ValueType = MYX_DICT_VALUE) then
      begin
        PName := _myx_grt_dict_name_item_as_string(NodeData.Value);
        if (PName<>nil) then
          NodeData.Key := UTF8Decode(PName)
        else
          NodeData.Key := '';
      end;
    end
    else
      if (ParentNodeData_Type=MYX_DICT_VALUE) then
      begin
        //Get value and retain it
        if (FDisplayObjectValues) then
        begin
          NodeData.Key := myx_grt_dict_item_key_by_index(
            ParentNodeData.Value, Node.Index);
          NodeData.Value := myx_grt_dict_item_value_by_index(
            ParentNodeData.Value, Node.Index);
        end
        else
        begin
          NodeData.Key := myx_grt_dict_item_key_by_index_complex(
            ParentNodeData.Value, Node.Index);
          NodeData.Value := myx_grt_dict_item_value_by_index_complex(
            ParentNodeData.Value, Node.Index);
        end;

        myx_grt_value_retain(NodeData.Value);

        NodeData.ValueType := myx_grt_value_get_type(NodeData.Value);
      end;

    PName := nil;
    if (NodeData.ValueType=MYX_LIST_VALUE) then
      PName := _myx_grt_list_content_get_struct_name(NodeData.Value);

    if (NodeData.ValueType=MYX_DICT_VALUE) then
      PName := _myx_grt_dict_struct_get_name(NodeData.Value);

    if (PName<>nil) then
      NodeData.StructName := UTF8Decode(PName)
    else
      NodeData.StructName := '';
  end;

  if NodeData.ValueType=MYX_DICT_VALUE then
  begin
    if (Not(FDisplayObjectValues)) then
    begin
      if myx_grt_dict_item_count_complex(NodeData.Value)>0 then
        InitialStates := InitialStates + [ivsHasChildren];
    end
    else
      if myx_grt_dict_item_count(NodeData.Value)>0 then
        InitialStates := InitialStates + [ivsHasChildren];
  end
  else
    if NodeData.ValueType=MYX_LIST_VALUE then
    begin
      ListContentType := myx_grt_list_content_get_type(NodeData.Value);

      // if OnlyObjectStructure is selected, the items are only
      // displayed for lists of dicts or lists of lists or
      // untyped lists or stringlists that hold reference ids
      if ((FDisplayObjectValues) or
        ((ListContentType = MYX_DICT_VALUE) or
        (ListContentType = MYX_LIST_VALUE) or
        (ListContentType = MYX_ANY_VALUE) or
        ((ListContentType = MYX_STRING_VALUE) and
          (myx_grt_list_content_get_struct_name(NodeData.Value) <> ''))
        )) and (myx_grt_list_item_count(NodeData.Value) > 0) then
          InitialStates := InitialStates + [ivsHasChildren];
    end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var HitInfo: THitInfo;
  TxtRect: TRect;
  xpos: integer;

begin
  if(Sender.InheritsFrom(TBaseVirtualTree))then
  begin
    TBaseVirtualTree(Sender).GetHitTestInfoAt(X, Y, True, HitInfo);

    if(HitInfo.HitNode<>nil)then
    begin
      TxtRect:=TBaseVirtualTree(Sender).GetDisplayRect(
        HitInfo.HitNode, -1, True);

      xpos:=TxtRect.Left-TBaseVirtualTree(Sender).OffsetX;

      if(X>xpos-16-4)and(X<xpos+2)and
        ((HitInfo.HitNode.ChildCount>0)or(vsHasChildren in HitInfo.HitNode.States))then
      begin
        TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]:=
          Not(TBaseVirtualTree(Sender).Expanded[HitInfo.HitNode]);
      end;
    end
  end;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.GrtObjTreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

begin
  if (TextType = ttStatic) and not (vsSelected in Node.States) then
    TargetCanvas.Font.Color := clGray;
end;

// -----------------------------------------------------------------------------

function TGrtObjectTree.GetObjectPath(Node: PVirtualNode): WideString;

var
  ParentNode: PVirtualNode;
  NodeData,
  ParentNodeData: PGrtObjTreeData;
  Key: WideString;

begin
  Result := '';

  if (Node = nil) then
    Exit;

  while True do
  begin
    NodeData := GetNodeData(Node);

    if Node.Parent = RootNode then
      Break;

    ParentNode := Node.Parent;
    ParentNodeData := GetNodeData(ParentNode);

    if (ParentNodeData.ValueType = MYX_LIST_VALUE) then
      Key := IntToStr(Node.Index)
    else
      Key := NodeData.Key;

    if (Result <> '') then
      Result := Key + '/' + Result
    else
      Result := Key;

    Node := ParentNode;
  end;

  Result := '/' + Result;
end;

// -----------------------------------------------------------------------------

procedure TGrtObjectTree.RefreshTree;

begin
  SetObjectTreeRoot(FObjectTreeRoot);
end;

end.
