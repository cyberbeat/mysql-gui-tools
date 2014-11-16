unit MigrationObjSelFilter;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls, TntForms,
  AdvancedEdit, VirtualTrees, ImgList, AuxFuncs, Contnrs,
  myx_public_interface, myx_grt_public_interface,
  PNGImage, TntClasses, PngTools, Grt;

type
  TMigrationObjSelFilterFrame = class(TFrame)
    ObjectSelGBox: TTntGroupBox;
    StructImage: TTntImage;
    SelObjCountLbl: TTntLabel;
    ObjectsToMigrateLbl: TTntLabel;
    DetailFilterObjectBtn: TTntButton;
    DetailFilterPnl: TTntPanel;
    ObjIgnoreVT: TVirtualStringTree;
    RemoveFromIgnoreListBtn: TTntButton;
    AddToIgnoreListBtn: TTntButton;
    ObjListVT: TVirtualStringTree;
    FilterMigrateListEd: TAdvancedEditFrame;
    SmallSchemaListViewImgList: TImageList;
    MigrateCBox: TTntCheckBox;
    TntLabel1: TTntLabel;
    StructTypeLbl: TTntLabel;
    TntLabel2: TTntLabel;
    DragPnl: TTntPanel;
    AddIgnoreListExprBtn: TTntButton;
    TntPanel1: TTntPanel;
    AddAllToIgnoreListBtn: TTntButton;
    RemoveAllFromIgnoreListBtn: TTntButton;

    procedure ObjListVTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DetailFilterObjectBtnClick(Sender: TObject);
    procedure DragPnlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragPnlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ObjListVTGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure ObjListVTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ObjListVTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure ObjIgnoreVTGetText(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure AddToIgnoreListBtnClick(Sender: TObject);
    procedure RemoveFromIgnoreListBtnClick(Sender: TObject);
    procedure MigrateCBoxClick(Sender: TObject);
    procedure AddIgnoreListExprBtnClick(Sender: TObject);
    procedure FilterMigrateListEdSearchEdChange(Sender: TObject);
    procedure AddAllToIgnoreListBtnClick(Sender: TObject);
  private
    { Private declarations }
    FGrt: TGrt;
    FStructName: WideString;
    FDetailedSelection: Boolean;
    FExpandedHeight: Integer;
    FHeightDragStart,
      FMouseYDragStart: Integer;
    FObjList,
      FObjIgnoreList: TObjectList;
    FIgnoreList: TTntStringList;
    FListImg: TPNGObject;

    procedure SetStructName(StructName: WideString);

    procedure SetDetailedSelection(DetailedSelection: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; Grt: TGrt); reintroduce;

    destructor Destroy; override;
    procedure RefreshLists;

    property StructName: WideString read FStructName write SetStructName;
    property DetailedSelection: Boolean read FDetailedSelection write SetDetailedSelection;

    property Grt: TGrt read FGrt;
  end;

  TObjListItem = class
    ObjName: WideString;
    Obj: Pointer;
    OwnerObj: Pointer;
  end;

  PObjTreeData = ^TObjTreeData;
  TObjTreeData = record
    Item: TObjListItem;
  end;


implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

constructor TMigrationObjSelFilterFrame.Create(
  AOwner: TComponent; Grt: TGrt);

begin
  inherited Create(AOwner);

  FGrt := Grt;

  ObjListVT.NodeDataSize := SizeOf(TObjTreeData);
  ObjIgnoreVT.NodeDataSize := 0;

  FDetailedSelection := True;
  FExpandedHeight := 350;

  FObjList := TObjectList.Create;
  FObjIgnoreList := TObjectList.Create;
  FIgnoreList := TTntStringList.Create;

  DetailedSelection := False;

  FListImg := nil;
end;

// -----------------------------------------------------------------------------

destructor TMigrationObjSelFilterFrame.Destroy;

begin
  FObjList.Free;
  FObjIgnoreList.Free;
  FIgnoreList.Free;

  if (Assigned(FListImg)) then
    FListImg.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.ObjListVTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

begin
  ImageIndex:=1; //transparent Image
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.SetDetailedSelection(DetailedSelection: Boolean);

begin
  if (FDetailedSelection<>DetailedSelection) then
  begin
    FDetailedSelection := DetailedSelection;

    if (FDetailedSelection) then
    begin
      Height := FExpandedHeight;
      DetailFilterPnl.Visible := True;
      DetailFilterObjectBtn.Caption := _('<< Hide Details');
      DragPnl.Cursor := crSizeNS;

      ObjListVT.Height := DetailFilterPnl.Height - 223 + 185;
      ObjIgnoreVT.Height := DetailFilterPnl.Height - 223 + 185;
    end
    else
    begin
      DetailFilterPnl.Visible := False;
      DetailFilterObjectBtn.Caption := _('Detailed selection >>');
      Height := 98;
      DragPnl.Cursor := crDefault;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.SetStructName(StructName: WideString);

var
  StructCaption: WideString;
  PStruct: Pointer;
  InheritedCaption: Integer;
  Canvas: TCanvas;
  CanvasControl: TControl;
  PImageData: PChar;
  DataLength: Integer;

begin
  FStructName := StructName;

  PStruct := myx_grt_struct_get(Grt.NativeGrt, FStructName);

  if (PStruct<>nil) then
    StructCaption := UTF8Decode(_myx_grt_struct_get_caption(
      Grt.NativeGrt, PStruct, @InheritedCaption))
  else
    StructCaption := FStructName;

  StructTypeLbl.Caption := StructCaption;

  MigrateCBox.Caption :=
    Format(_('Objects of type %s'),
      [StructCaption]);

  MigrateCBox.Font.Color := clHotLight;


  //Get text width from parent form
  CanvasControl := self;
  while (CanvasControl.Parent<>nil) or
    (CanvasControl.ClassNameIs('TTntForm')) or
    (CanvasControl.ClassNameIs('TForm')) do
    CanvasControl := CanvasControl.Parent;

  if (CanvasControl<>nil) then
  begin
    Canvas := TForm(CanvasControl).Canvas;

    MigrateCBox.Width := 20 +
      GetWideStringTextWidth(Canvas, MigrateCBox.Caption);
  end;

  PImageData := _myx_grt_struct_get_icon(Grt.NativeGrt,
    PChar(ExtractFilePath(Application.ExeName)+'images\structs\'),
    PStruct, MYX_IT_MANY_STANDARD, @DataLength);

  LoadPNGImageFromPChar(PImageData, DataLength, StructImage, True);


  PImageData := _myx_grt_struct_get_icon(Grt.NativeGrt,
    PChar(ExtractFilePath(Application.ExeName)+'images\structs\'),
    PStruct, MYX_IT_SMALL, @DataLength);

  if (Assigned(FListImg)) then
    FListImg.Free;

  FListImg := LoadPNGImageFromPChar(PImageData, DataLength);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.DetailFilterObjectBtnClick(
  Sender: TObject);

begin
  DetailedSelection := Not(DetailedSelection);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.DragPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (DetailedSelection) then
  begin
    FHeightDragStart := Height;
    FMouseYDragStart := Mouse.CursorPos.Y;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.DragPnlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Shift = [ssLeft]) and (DetailedSelection) then
  begin
    Height := FHeightDragStart +
      (Mouse.CursorPos.Y - FMouseYDragStart);

    FExpandedHeight := Height;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.ObjListVTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData: PObjTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  NodeData^.Item := TObjListItem(FObjList.Items[Node.Index]);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.ObjListVTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  NodeData: PObjTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  CellText := NodeData^.Item.ObjName;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.ObjListVTAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  TxtRect: TRect;
  x: integer;

begin
  if (Assigned(FListImg)) then
  begin
    TxtRect:=Sender.GetDisplayRect(Node, Column, True);
    x:=TxtRect.Left-Sender.OffsetX-FListImg.Width;

    FListImg.Draw(TargetCanvas,
      Rect(x, 1, x+FListImg.Width, 1+FListImg.Height));
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.ObjIgnoreVTGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);

var
  P: Integer;

begin
  P := Pos(':', FIgnoreList[Node.Index]);

  if (P > 0) then
    CellText := Copy(FIgnoreList[Node.Index], P+1, MaxInt)
  else
    CellText := FIgnoreList[Node.Index];
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.RefreshLists;

var
  I, J: Integer;
  PObj: Pointer;
  ObjListItem: TObjListItem;
  Filter: WideString;
  FilterHit: Boolean;
  SearchIgnores: Integer;
  TotalObjCount: Integer;
  PIgnoreList: Pointer;
  PSourceObjects: Pointer;
  S: WideString;
  ObjectStructName: WideString;

begin
  FIgnoreList.Clear;
  PIgnoreList := Grt.Global['/migration/ignoreList'];

  if (PIgnoreList <> nil) then
    for I:=0 to myx_grt_list_item_count(PIgnoreList)-1 do
    begin
      S := Grt.ListString[PIgnoreList, I];

      if (Copy(S, 1, Length(FStructName) + 1) =
        FStructName + ':') then
        FIgnoreList.Add(S);

      {FIgnoreList.Add(UTF8Decode(_myx_grt_list_item_get_as_string(
        PIgnoreList, I)));}
    end;

  FObjList.Clear;
  FObjIgnoreList.Clear;
  SearchIgnores := 0;
  TotalObjCount := 0;
  PSourceObjects := Grt.Global['/migration/sourceObjects'];
  for I:=0 to myx_grt_list_item_count(PSourceObjects)-1 do
  begin
    PObj := myx_grt_list_item_get_reference_value(
      Grt.NativeGrt, PSourceObjects, I);
    ObjectStructName := myx_grt_dict_struct_get_name(PObj);

    //If the object is of the same struct
    if (PObj<>nil) and
      (myx_grt_struct_is_or_inherits_from(Grt.NativeGrt,
        ObjectStructName, StructName) = 1) then
    begin
      inc(TotalObjCount);

      ObjListItem := TObjListItem.Create;
      ObjListItem.Obj := PObj;

      //Find owner schema
      ObjListItem.OwnerObj :=
        myx_grt_dict_item_get_reference_value(Grt.NativeGrt,
          ObjListItem.Obj, 'owner');

      //Build name
      if (ObjListItem.OwnerObj<>nil) then
        ObjListItem.ObjName :=
          UTF8Decode(_myx_grt_dict_item_get_as_string(ObjListItem.OwnerObj, 'name')) + '.' +
          UTF8Decode(_myx_grt_dict_item_get_as_string(ObjListItem.Obj, 'name'))
      else
      begin
        ObjListItem.Free;

        raise EInOutError.Create(Format(
          _('Owner object of %s not found'),
          [UTF8Decode(_myx_grt_dict_item_get_as_string(PObj, 'name'))]));
      end;


      //Check object name against filter list
      FilterHit := False;
      for J:=0 to FIgnoreList.Count-1 do
      begin
        Filter := FIgnoreList[J];

        if (myx_match_pattern(FStructName + ':' + ObjListItem.ObjName,
          Filter, 1, 1)=1) then
        begin
          FilterHit := True;
          break;
        end;
      end;

      //Check if the object was searched
      if (Not(FilterHit)) then
      begin
        if (myx_match_pattern(ObjListItem.ObjName,
          FilterMigrateListEd.SearchEd.Text + '*', 0, 1)=1) then
          FObjList.Add(ObjListItem)
        else
          Inc(SearchIgnores);
      end
      else
        FObjIgnoreList.Add(ObjListItem);
    end;
  end;



  ObjListVT.BeginUpdate;
  try
    ObjListVT.Clear;

    ObjListVT.RootNodeCount := FObjList.Count;
  finally
    ObjListVT.EndUpdate;
  end;

  ObjIgnoreVT.BeginUpdate;
  try
    ObjIgnoreVT.Clear;

    ObjIgnoreVT.RootNodeCount := FIgnoreList.Count;
  finally
    ObjIgnoreVT.EndUpdate;
  end;

  SelObjCountLbl.Caption := IntToStr(FObjList.Count+SearchIgnores) + ' / ' +
    IntToStr(TotalObjCount);

  if (TotalObjCount = 0) then
  begin
    DisableEnableControls(self, False);
    MigrateCBox.Checked := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.AddToIgnoreListBtnClick(
  Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;
  NodeData: PObjTreeData;

begin
  Selection := ObjListVT.GetSortedSelection(False);

  for I:=0 to ObjListVT.SelectedCount-1 do
  begin
    NodeData := ObjListVT.GetNodeData(Selection[I]);

    myx_grt_list_item_add_as_string(
      Grt.Global['/migration/ignoreList'],
      FStructName + ':' +
      NodeData^.Item.ObjName);
  end;

  RefreshLists;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.RemoveFromIgnoreListBtnClick(
  Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;

begin
  Selection := ObjIgnoreVT.GetSortedSelection(False);

  for I:=0 to ObjIgnoreVT.SelectedCount-1 do
    myx_grt_list_item_del_as_string(
      Grt.Global['/migration/ignoreList'],
      FIgnoreList[Selection[I].Index]);

  RefreshLists;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.MigrateCBoxClick(Sender: TObject);

begin
  if (Enabled) then
  begin
    if (MigrateCBox.Checked) then
    begin
      DisableEnableControls(ObjectSelGBox, True);
      myx_grt_list_item_del_as_string(
        Grt.Global['/migration/ignoreList'],
        FStructName + ':*');
    end
    else
    begin
      DisableEnableControls(ObjectSelGBox, False);
      myx_grt_list_item_add_as_string(
        Grt.Global['/migration/ignoreList'],
        FStructName + ':*');
    end;

    RefreshLists;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.AddIgnoreListExprBtnClick(
  Sender: TObject);

var
  pattern: WideString;

begin
  if (ShowModalEditDialog(_('Add Ignore Pattern'),
    _('Please enter the pattern to ignore. You can use * and ? '+
      'to match the objects you would like to ignore. Please note '+
      'that the string is matched case sensitive'),
      myx_mtEdit, _('OK')+#13#10+_('Cancel'),
      True, _('Pattern'), pattern)=1) and (pattern<>'') then
  begin
    myx_grt_list_item_add_as_string(
      Grt.Global['/migration/ignoreList'],
      FStructName + ':' + pattern);

    RefreshLists;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.FilterMigrateListEdSearchEdChange(
  Sender: TObject);

begin
  FilterMigrateListEd.SearchEdChange(Sender);

  RefreshLists;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelFilterFrame.AddAllToIgnoreListBtnClick(
  Sender: TObject);

var
  I: Integer;
  Selection: TNodeArray;
  PIgnoreList: Pointer;
  StructNameLength: Integer;

begin
  Selection := ObjListVT.GetSortedSelection(False);

  PIgnoreList := Grt.Global['/migration/ignoreList'];

  StructNameLength := Length(FStructName);

  // clear existing entries
  for I := Grt.ListCount(PIgnoreList) - 1 downto 0 do
    if (Copy(Grt.ListString[PIgnoreList, I], 1, StructNameLength + 1) =
      FStructName + ':') then
      myx_grt_list_item_del(PIgnoreList, I);

  RefreshLists;

  if (Sender = AddAllToIgnoreListBtn) then
  begin
    ObjListVT.SelectAll(False);

    AddToIgnoreListBtnClick(Sender);
  end;
end;

end.
