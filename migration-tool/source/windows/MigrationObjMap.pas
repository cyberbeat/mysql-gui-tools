unit MigrationObjMap;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees,
  MigrationObjMapDetail, WizardPage, Grt, PngTools;

type
  TMigrationObjMapForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    ObjTypeSelMapSheet: TTntTabSheet;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    DefaultFramesScrollBox: TTntScrollBox;
    ObjMapSpacerPnl: TTntPanel;
    AdvancedOptionsPnl: TTntPanel;
    AdvancedOptionsGBox: TTntGroupBox;
    DetailedMappingsCBox: TTntCheckBox;
    ObjMapDetailsTabSheet: TTntTabSheet;
    TreeImageList: TImageList;
    ObjMapDetailMainPnl: TTntPanel;
    MigLogVT: TVirtualStringTree;
    LogHeaderLbl: TTntLabel;
    ObjMapDetailMethodPnl: TTntPanel;
    KeepSchemaCBox: TTntCheckBox;
    TntLabel6: TTntLabel;
    TntLabel1: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DockPnlResize(Sender: TObject);

    procedure DisplayObjMappingDefaults;

    procedure DisplayObjMappingDetails;
    procedure MigLogVTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MigLogVTInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure MigLogVTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure MigLogVTPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure MigLogVTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure MigLogVTGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: WideString);
    procedure MigLogVTBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure MigLogVTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure MigLogVTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MigLogVTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MigLogVTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure MigLogVTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: WideString);
  private
    { Private declarations }
    FTreeBtnOpenPNGImg,
      FTreeBtnClosedPNGImg: TPNGObject;
    FLogError,
      FLogErrorSmall,
      FLogWarning,
      FLogWarningSmall: TPNGObject;
    FGrtValueListPNGImg: TPNGObject;

    FIconList: TTntStringList;

    DefaultFrames: TList;
  public
    { Public declarations }
    ObjMapDetailFrame: TMigrationObjMapDetailFrame;

    procedure BeforeSubPageIndexChange(SectionIndex: Integer); override;
  protected
    procedure SetSubPageIndex(NewSubPageIndex: Integer); override;
    function GetSubPageIndex: Integer; override;

    function GetSubPageCount: integer; override;

    function GetSectionTitle: WideString; override;
    function GetSectionInfo: WideString; override;

    function GetSupportAdvancedOptions: Boolean; override;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); override;
    function GetAdvancedOptionsState: Boolean; override;

    function MigLogGetCellText(
      Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex;
      TextType: TVSTTextType): WideString;

    procedure UpdateObjMapDetailFrame(Sender: TObject);
    procedure DoDetailedMethodChanged(Sender: TObject);

    function IsDefaultMapping(PMapping: Pointer): Pointer;
  end;

  TMapTreeDataType = (
    LTDT_Object,
    LTDT_ListMember
  );

  PMapTreeData = ^TMapTreeData;
  TMapTreeData = record
    NodeType: TMapTreeDataType;

    PStruct: Pointer;
    StructName: WideString;
    PMember: Pointer;
    MemberName: WideString;
    Value: Pointer;

    PSourceObject: Pointer;
    SourceCaption: WideString;

    PMapping: Pointer;
    ContainsError: Boolean;
    ContainsWarning: Boolean;

    TargetCaption: WideString;
  end;

var
  MigrationObjMapForm: TMigrationObjMapForm;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel := DockPnl;

  FTreeBtnOpenPNGImg := LoadPNGImageFromResource('tree_button_open');
  FTreeBtnClosedPNGImg := LoadPNGImageFromResource('tree_button_closed');

  FLogError := LoadPNGImageFromResource('log_error_16x16');
  FLogErrorSmall := LoadPNGImageFromResource('log_error_12x12');
  FLogWarning := LoadPNGImageFromResource('log_warning_16x16');
  FLogWarningSmall := LoadPNGImageFromResource('log_warning_12x12');

  FGrtValueListPNGImg := LoadPNGImageFromResource('grt_value_list');

  MainPngControl.Align := alNone;
  MainPngControl.Left := -4;
  MainPngControl.Top := -27;
  MainPngControl.Width := 765+4+4;
  MainPngControl.Height := 529+274;
  MainPngControl.ActivePageIndex := 0;

  DefaultFrames := TList.Create;

  FIconList := TTntStringList.Create;

  MigLogVT.NodeDataSize := SizeOf(TMapTreeData);

  ObjMapDetailFrame := TMigrationObjMapDetailFrame.Create(self, Grt);
  ObjMapDetailFrame.Name := 'ObjMapDetailFrame';
  ObjMapDetailFrame.Parent := ObjMapDetailMethodPnl;
  ObjMapDetailFrame.Left := 0;
  ObjMapDetailFrame.Top := 0;
  ObjMapDetailFrame.OnDetailSelectionChange := UpdateObjMapDetailFrame;
  ObjMapDetailFrame.OnMethodChanged := DoDetailedMethodChanged;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.FormDestroy(Sender: TObject);

var
  I: Integer;

begin
  FTreeBtnOpenPNGImg.Free;
  FTreeBtnClosedPNGImg.Free;

  FLogError.Free;
  FLogErrorSmall.Free;
  FLogWarning.Free;
  FLogWarningSmall.Free;

  FGrtValueListPNGImg.Free;

  DefaultFrames.Free;

  for I := 0 to FIconList.Count-1 do
    FIconList.Objects[i].Free;
  FIconList.Free;
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
  begin
    // Set script checkpoint
    Grt.GlobalAsInt['/migration/applicationData/' +
      'ScriptGenerationCheckpoint'] := 3;

    DisplayObjMappingDefaults
  end
  else
    if (NewSubPageIndex=1) then
      DisplayObjMappingDetails;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetSubPageCount: integer;

begin
  if (DetailedMappingsCBox.Checked) then
    Result := MainPngControl.PageCount
  else
    Result := MainPngControl.PageCount - 1;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Object Mapping');
    1:
      Result:=_('Detailed Object Mapping');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Please define how to map the database objects.');
    1:
      Result:=_('Please define the detailed database object mapping.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := True;
  else
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    0:
      AdvancedOptionsPnl.Visible := State;
    1:
      AdvancedOptionsPnl.Visible := False;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=AdvancedOptionsPnl.Visible;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.BeforeSubPageIndexChange(SectionIndex: Integer);

var
  I: Integer;
  PMappingDefauts,
    PParams: Pointer;
  DefaultFrame: TMigrationObjMapDetailFrame;
  PMappingDefault: Pointer;
  ParamGroupName,
  OptionString: WideString;

begin
  if (SectionIndex = 0) then
  begin
    //build defaults mapping list
    PMappingDefauts := Grt.Global['/migration/mappingDefaults'];

    myx_grt_list_clear(PMappingDefauts);

    for I:=0 to DefaultFrames.Count-1 do
    begin
      DefaultFrame := TMigrationObjMapDetailFrame(
        DefaultFrames[I]);

      if (DefaultFrame.SelectedMethod <> nil) then
      begin
        // Create new mapping
        PMappingDefault := myx_grt_dict_new(
          nil, 'db.migration.Mapping');
        Grt.DictString[PMappingDefault, '_id'] := myx_grt_get_guid();

        Grt.DictString[PMappingDefault, 'methodName'] :=
          DefaultFrame.MigMethodName;
        Grt.DictString[PMappingDefault, 'moduleName'] :=
          DefaultFrame.MigModuleName;
        Grt.DictString[PMappingDefault, 'sourceStructName'] :=
          DefaultFrame.MigSourceStructName;

        // Store reference to method
        Grt.DictRef[PMappingDefault, 'method'] :=
          DefaultFrame.SelectedMethod;

        ParamGroupName := DefaultFrame.ParamGroupName;
        if (ParamGroupName<>'')then
        begin
          Grt.DictString[PMappingDefault, 'paramGroupName'] :=
            ParamGroupName;

          PParams := DefaultFrame.ParamGroupParams;
        end
        else
          PParams := DefaultFrame.UserDefinedParams;

        if (PParams<>nil) then
        begin
          Grt.DictItem[PMappingDefault, 'params'] := PParams;

          //release PParams one time, since it is referenced
          //by the PMappingDefault now
          myx_grt_value_release(PParams);
        end;

        //Add to mapping defaults list
        Grt.ListAdd(PMappingDefauts, PMappingDefault);
      end;
    end;

    //Set options
    Grt.GlobalAsString[
      '/migration/objectCreationParams/AppName'] :=
      'MySQL Migration Toolkit';

    Grt.GlobalAsString[
      '/migration/objectCreationParams/ScriptType'] :=
      'SQL Create Script';

    //Skip schema creation
    if (KeepSchemaCBox.Checked) then
      OptionString := 'yes'
    else
      OptionString := 'no';

    Grt.GlobalAsString[
      '/migration/objectCreationParams/KeepSchema'] :=
      OptionString;
    Grt.GlobalAsString[
      '/migration/objectCreationParams/GenerateUseSchemaCommand'] :=
      'yes';
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------
// ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.DisplayObjMappingDefaults;

var
  I, J, K, C: Integer;
  PSchemata, PSchema: Pointer;
  PSchemaStruct, PSchemaStructMember, PSchemaItemList,
    PSchemaItem: Pointer;
  MemberName,
    ContentStructName: WideString;
  DefaultFrame: TMigrationObjMapDetailFrame;
  PSourceObjects: Pointer;

begin
  MainPngControl.ActivePage := ObjTypeSelMapSheet;

  try
    DefaultFramesScrollBox.Visible := False;

    //Get schemata list from GRT Migration object
    PSchemata := myx_grt_dict_item_get_value(Grt.Global['/migration/sourceCatalog'],
      'schemata');

    PSourceObjects := Grt.Global['/migration/sourceObjects'];

    //Clear Filter Frames if there are some already
    for I:=0 to DefaultFrames.Count-1 do
      TMigrationObjMapDetailFrame(DefaultFrames[I]).Free;
    DefaultFrames.Clear;

    //Check if there is at lease one schema
    if (myx_grt_list_item_count(PSchemata)>0) then
    begin

      //Get the struct type of the schemata
      PSchema := myx_grt_list_item_get(PSchemata, 0);
      PSchemaStruct := myx_grt_dict_struct_get(Grt.NativeGrt, PSchema);

      //Create a filter frame for each member deriving from db.DatabaseObject
      for I:=myx_grt_struct_get_member_count(PSchemaStruct)-1 downto 0 do
      begin
        PSchemaStructMember :=
          myx_grt_struct_get_member_by_index(PSchemaStruct, I);

        MemberName :=
          UTF8Decode(_myx_grt_struct_get_member_name(PSchemaStructMember));

        ContentStructName :=
          UTF8Decode(_myx_grt_struct_member_get_content_struct_name(PSchemaStructMember));

        //Only consider structs that derive from db.DatabaseObject
        if (myx_grt_struct_inherits_from(Grt.NativeGrt,
          ContentStructName, 'db.DatabaseObject')=1) then
        begin
          C := 0;

          //Collect all objects for the sourceObject list
          //Loop over all schemata
          for J:=0 to myx_grt_list_item_count(PSchemata)-1 do
          begin
            PSchema := myx_grt_list_item_get(PSchemata, J);

            PSchemaItemList :=
              myx_grt_dict_item_get_value(PSchema, MemberName);

            if (PSchemaItemList<>nil)then
            begin
              //Add objects to sourceObject list
              for K:=0 to myx_grt_list_item_count(PSchemaItemList)-1 do
              begin
                PSchemaItem := myx_grt_list_item_get(PSchemaItemList, K);

                myx_grt_list_item_add_as_string(PSourceObjects,
                  myx_grt_dict_id_item_as_string(
                    PSchemaItem));

                inc(C);
              end;
            end;
          end;


          //Create DefaultFrame
          if (C > 0) then
          begin
            DefaultFrame := TMigrationObjMapDetailFrame.Create(self, Grt);
            DefaultFrames.Add(DefaultFrame);
            DefaultFrame.Name := 'DefaultFrame' + IntToStr(DefaultFrames.Count);
            DefaultFrame.Parent := DefaultFramesScrollBox;

            DefaultFrame.StructName := ContentStructName;
          end;
        end;
      end;

      //Create DefaultFrame for schema
      DefaultFrame := TMigrationObjMapDetailFrame.Create(self, Grt);
      DefaultFrames.Add(DefaultFrame);
      DefaultFrame.Name := 'DefaultFrame' + IntToStr(DefaultFrames.Count);
      DefaultFrame.Parent := DefaultFramesScrollBox;

      DefaultFrame.StructName := UTF8Decode(
        _myx_grt_dict_struct_get_name(PSchema));
    end
  finally
    DefaultFramesScrollBox.Visible := True;
  end;

  // Refresh scrollbar
  DefaultFramesScrollBox.Align := alNone;
  DefaultFramesScrollBox.Align := alClient;
end;

// -----------------------------------------------------------------------------
// Detailed ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.DisplayObjMappingDetails;

begin
  MigLogVT.BeginUpdate;
  try
    MigLogVT.Clear;

    MigLogVT.RootNodeCount := myx_grt_list_item_count(
      Grt.Global['/migration/sourceCatalog/schemata']);

  finally
    MigLogVT.EndUpdate;
  end;

  MainPngControl.ActivePage := ObjMapDetailsTabSheet;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

var
  NodeData,
    ParentNodeData: PMapTreeData;
  PParentSourceObject: Pointer;
  ParentMemberName: WideString;
  ParentNodeType : TMapTreeDataType;
  I: Integer;
  PMappings,
    PMapping,
    PMethods,
    PMethod,
    PParamGroups: Pointer;
  ObjId,
    SourceStructName: WideString;
  BestRating: Integer;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Sender.GetNodeLevel(Node) < 2) then
    include(InitialStates, ivsExpanded);

  if (ParentNode = nil) then
  begin
    ParentNodeData := nil;
    ParentNodeType := LTDT_ListMember;
    PParentSourceObject :=
      Grt.Global['/migration/sourceCatalog'];
    ParentMemberName := 'schemata';
  end
  else
  begin
    ParentNodeData := Sender.GetNodeData(ParentNode);

    ParentNodeType := ParentNodeData.NodeType;
    PParentSourceObject := ParentNodeData.PSourceObject;
    ParentMemberName := ParentNodeData.MemberName;
  end;

  if (ParentNodeType = LTDT_ListMember) then
  begin
    NodeData.NodeType := LTDT_Object;

    NodeData.PSourceObject := myx_grt_list_item_get(
      myx_grt_dict_item_get_value(PParentSourceObject,
        ParentMemberName), Node.Index);

    NodeData.SourceCaption := Grt.DictString[NodeData.PSourceObject, 'name'];

    NodeData.StructName := myx_grt_dict_struct_get_name(
      NodeData.PSourceObject);

    NodeData.PStruct := myx_grt_struct_get(Grt.NativeGrt, NodeData.StructName);

    NodeData.TargetCaption := NodeData.SourceCaption;

    // find mapping
    NodeData.PMapping := nil;
    ObjId := Grt.DictString[NodeData.PSourceObject, '_id'];

    PMappings := Grt.Global['/migration/mappingDefinitions'];
    for I := 0 to Grt.ListCount(PMappings) - 1 do
    begin
      PMapping := Grt.ListItem[PMappings, I];

      if (ObjId = Grt.DictString[PMapping, 'sourceObject']) then
      begin
        NodeData.PMapping := PMapping;

        break;
      end;
    end;

    // if there is no mapping, look at default mappings
    if (NodeData.PMapping = nil) then
    begin
      PMappings := Grt.Global['/migration/mappingDefaults'];
      for I := 0 to Grt.ListCount(PMappings) - 1 do
      begin
        PMapping := Grt.ListItem[PMappings, I];

        SourceStructName := Grt.DictString[PMapping,
          'sourceStructName'];

        if (NodeData.StructName = SourceStructName) or
          (myx_grt_struct_inherits_from(Grt.NativeGrt,
            NodeData.StructName, SourceStructName) = 1) then
        begin
          NodeData.PMapping := PMapping;

          break;
        end;
      end;
    end;

    // if there is no default mapping for this struct type yet,
    // find the best method and add the default mapping
    if (NodeData.PMapping = nil) then
    begin
      PMethods := Grt.Global['/migration/migrationMethods'];
      BestRating := 0;

      PMappings := Grt.Global['/migration/mappingDefaults'];
      PMapping := nil;

      for I := 0 to Grt.ListCount(PMethods) - 1 do
      begin
        PMethod := Grt.ListItem[PMethods, I];

        SourceStructName := Grt.DictString[PMethod,
          'sourceStructName'];

        if ((NodeData.StructName = SourceStructName) or
          (myx_grt_struct_inherits_from(Grt.NativeGrt,
            NodeData.StructName, SourceStructName) = 1)) and
          (Grt.DictInt[PMethod, 'rating'] >= BestRating) then
        begin
          if (PMapping = nil) then
          begin
            // Create new mapping
            PMapping := myx_grt_dict_new(
              nil, 'db.migration.Mapping');
            Grt.DictString[PMapping, '_id'] := myx_grt_get_guid();
          end;

          Grt.DictString[PMapping, 'methodName'] :=
            Grt.DictString[PMethod, 'name'];
          Grt.DictString[PMapping, 'moduleName'] :=
            Grt.DictString[PMethod, 'moduleName'];
          Grt.DictString[PMapping, 'sourceStructName'] :=
            Grt.DictString[PMethod, 'sourceStructName'];

          // Store reference to method
          Grt.DictRef[PMapping, 'method'] := PMethod;

          PParamGroups := Grt.DictItem[PMethod, 'paramGroups'];
          if (Grt.ListCount(PParamGroups) > 0) then
            Grt.DictString[PMapping, 'paramGroupName'] :=
              Grt.DictString[Grt.ListItem[PParamGroups, 0],
                'name'];

          BestRating := Grt.DictInt[PMethod, 'rating'];
        end;
      end;

      if (PMapping <> nil) then
      begin
        NodeData.PMapping := PMapping;

        // add new mapping to default mapping list
        Grt.ListAdd(PMappings, PMapping);
      end;
    end;


    //Special handling for columns
    if (WideSameText(NodeData.StructName, 'db.Column')) or
      (myx_grt_struct_inherits_from(Grt.NativeGrt, NodeData.StructName,
      'db.Column') = 1) then
    begin
      NodeData.SourceCaption := NodeData.SourceCaption + ' '+
        UTF8Decode(_myx_grt_dict_item_get_as_string(
          NodeData.PSourceObject, 'datatypeName'));
    end;


    if (GetListMemberCount(Grt.NativeGrt, NodeData.StructName, True) > 0) then
      include(InitialStates, ivsHasChildren);
  end
  else
    if (ParentNodeData.NodeType = LTDT_Object) then
    begin
      NodeData.NodeType := LTDT_ListMember;

      NodeData.PSourceObject := ParentNodeData.PSourceObject;

      NodeData.StructName := ParentNodeData.StructName;

      NodeData.PMember := GetListMember(Grt.NativeGrt, ParentNodeData.StructName, Node.Index);

      NodeData.MemberName := UTF8Decode(
        _myx_grt_struct_get_member_name(NodeData.PMember));


      NodeData.Value := myx_grt_dict_item_get_value(
        NodeData.PSourceObject, NodeData.MemberName);

      // try to find member caption
      NodeData.SourceCaption := UTF8Decode(
        _myx_grt_struct_get_member_caption(Grt.NativeGrt,
          ParentNodeData.PStruct,
          PChar(UTF8Encode(NodeData.MemberName)), 1));

      // use MemberName is caption is not found
      if (NodeData.SourceCaption = '') then
        NodeData.SourceCaption := NodeData.MemberName;

      if (myx_grt_list_item_count(NodeData.Value) > 0) then
        include(InitialStates, ivsHasChildren);
    end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);

var
  NodeData: PMapTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType = LTDT_Object) then
    ChildCount := GetListMemberCount(Grt.NativeGrt, NodeData.StructName, False)
    else if (NodeData.NodeType = LTDT_ListMember) then
      ChildCount := myx_grt_list_item_count(NodeData.Value);
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.MigLogGetCellText(
  Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex;
  TextType: TVSTTextType): WideString;

var
  NodeData: PMapTreeData;
  PMethod: Pointer;
  ParamGroup: WideString;

begin
  NodeData := Sender.GetNodeData(Node);

  if (Column = 0) or
    ((Column = 2) and (NodeData.NodeType = LTDT_ListMember)) then
    Result := NodeData.SourceCaption
  else
    if (Column = 1) and (NodeData.NodeType = LTDT_Object) then
    begin
      if (NodeData.PMapping <> nil) then
      begin
        PMethod := Grt.DictRef[NodeData.PMapping, 'method'];

        if (PMethod <> nil) then
        begin
          ParamGroup := Grt.DictString[NodeData.PMapping,
            'paramGroupName'];
          if (ParamGroup = '') then
            ParamGroup := _('Userdefined Parameter');


          Result := Grt.DictString[PMethod, 'caption'] + ' (' +
            ParamGroup + ')';
        end;
      end;
    end
    else
      if (Column = 2) then
        Result := NodeData.TargetCaption;

end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

begin
  CellText := MigLogGetCellText(Sender, Node, Column, TextType);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

var
  NodeData: PMapTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType = LTDT_ListMember) then
    TargetCanvas.Font.Color := clGray
  else
    if Sender.FocusedNode <> Node then
      TargetCanvas.Font.Color := clBlack;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);

begin
  if (Column = 0) or (Column = 2) then
    ImageIndex := 1;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);

begin
  HintText := MigLogGetCellText(Sender, Node, Column, ttNormal);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);

var
  NodeData: PMapTreeData;

begin
  NodeData := Sender.GetNodeData(Node);

  if (NodeData.NodeType = LTDT_ListMember) then
  begin
    EraseAction := eaColor;
    ItemColor := $00EEEEEE;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PMapTreeData;
  Index: Integer;
  PngImg: TPNGObject;
  PImageData: PChar;
  DataLength: Integer;
  TxtRect: TRect;
  x: integer;

begin
  NodeData := Sender.GetNodeData(Node);

  TxtRect:=Sender.GetDisplayRect(Node, Column, True);

  x:=TxtRect.Left-Sender.OffsetX;

  //Draw > / v
  if (Node.ChildCount>0) or (vsHasChildren in Node.States) then
  begin
    if(Sender.Expanded[Node])then
      FTreeBtnOpenPNGImg.Draw(TargetCanvas,
        Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
    else
      FTreeBtnClosedPNGImg.Draw(TargetCanvas,
        Rect(x-16-12, CellRect.Top+4, x-16-4, CellRect.Top+16+4))
  end;

  if (Column = 2) then
    inc(x, 2);


  if (Column = 0) or
    (Column = 2) then
  begin
    if (NodeData.NodeType = LTDT_Object) then
    begin
      Index := FIconList.IndexOf(NodeData.StructName);
      if (Index = -1) then
      begin
        PImageData := _myx_grt_struct_get_icon(Grt.NativeGrt,
          PChar(ExtractFilePath(Application.ExeName)+'images\structs\'),
          NodeData.PStruct, MYX_IT_SMALL, @DataLength);

        PngImg := LoadPNGImageFromPChar(PImageData, DataLength);

        FIconList.AddObject(NodeData.StructName, PngImg);
      end
      else
        PngImg := TPNGObject(FIconList.Objects[Index]);

      PngImg.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));

      if (NodeData.ContainsError) then
        FLogErrorSmall.Draw(TargetCanvas,
          Rect(x-16+8, CellRect.Top+1+6, x+8, CellRect.Top+1+16+6))
      else
        if (NodeData.ContainsWarning) then
          FLogWarningSmall.Draw(TargetCanvas,
            Rect(x-16+8, CellRect.Top+1+7, x+8, CellRect.Top+1+16+7))
    end
    else
    begin
      FGrtValueListPNGImg.Draw(TargetCanvas,
        Rect(x-16, CellRect.Top+1, x, CellRect.Top+1+16));
    end;
  end;

end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTMouseDown(Sender: TObject;
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

procedure TMigrationObjMapForm.MigLogVTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

var
  NodeData: PMapTreeData;
  ParamGroupName: WideString;

begin
  if (MigLogVT.SelectedCount > 0) then
  begin
    DisableEnableControls(ObjMapDetailMethodPnl, True);

    NodeData := MigLogVT.GetNodeData(MigLogVT.GetFirstSelected);

    if (NodeData.PMapping = nil) then
      Exit;

    try
      ObjMapDetailFrame.SettingMethod := True;

      // Set the struct name
      ObjMapDetailFrame.StructName := NodeData.StructName;

      // Set the method
      if (NodeData.PMapping <> nil) then
        ObjMapDetailFrame.SelectedMethod :=
          Grt.DictRef[NodeData.PMapping, 'method'];

      ParamGroupName := Grt.DictString[NodeData.PMapping, 'paramGroupName'];
      // Set the params
      if (ParamGroupName <> '') then
        ObjMapDetailFrame.ParamGroupName :=
          ParamGroupName
      else
        ObjMapDetailFrame.UserDefinedParams :=
          Grt.DictItem[NodeData.PMapping, 'params'];
    finally
      ObjMapDetailFrame.SettingMethod := False;
    end;

    ObjMapDetailMethodPnl.Height := ObjMapDetailFrame.Height - 10;

    ObjMapDetailMethodPnl.Visible := True;
  end
  else
    DisableEnableControls(ObjMapDetailMethodPnl, False);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.UpdateObjMapDetailFrame(Sender: TObject);

begin
  ObjMapDetailMethodPnl.Height := ObjMapDetailFrame.Height - 10;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.DoDetailedMethodChanged(Sender: TObject);

var
  I: Integer;
  Node: PVirtualNode;
  NodeData: PMapTreeData;
  PMappingDefinitions,
    PMappingDefinition,
    PMapping,
    PParams: Pointer;
  ObjId,
    ParamGroupName: WideString;

begin
  PMappingDefinitions := Grt.Global['/migration/mappingDefinitions'];

  Node := MigLogVT.GetFirstSelected;
  while (Node <> nil) do
  begin
    NodeData := MigLogVT.GetNodeData(Node);

    ObjId := Grt.DictString[NodeData.PSourceObject, '_id'];

    // find existing mapping
    PMapping := nil;

    for I := Grt.ListCount(PMappingDefinitions) - 1 downto 0 do
    begin
      PMappingDefinition := Grt.ListItem[PMappingDefinitions, I];

      if (Grt.DictString[PMappingDefinition, 'sourceObject'] = ObjId) then
      begin
        PMapping := PMappingDefinition;
        break;
      end;
    end;

    // Create new mapping
    if (PMapping = nil) then
    begin
      PMapping := myx_grt_dict_new(
        nil, 'db.migration.Mapping');
      Grt.DictString[PMapping, '_id'] := myx_grt_get_guid();
      Grt.DictString[PMapping, 'sourceObject'] := ObjId;

      //Add to mapping list
      Grt.ListAdd(PMappingDefinitions, PMapping);
    end;

    Grt.DictString[PMapping, 'methodName'] :=
      ObjMapDetailFrame.MigMethodName;
    Grt.DictString[PMapping, 'moduleName'] :=
      ObjMapDetailFrame.MigModuleName;
    Grt.DictString[PMapping, 'sourceStructName'] :=
      ObjMapDetailFrame.MigSourceStructName;

    // Store reference to method
    Grt.DictRef[PMapping, 'method'] :=
      ObjMapDetailFrame.SelectedMethod;

    ParamGroupName := ObjMapDetailFrame.ParamGroupName;
    if (ParamGroupName<>'')then
    begin
      Grt.DictString[PMapping, 'paramGroupName'] :=
        ParamGroupName;

      PParams := ObjMapDetailFrame.ParamGroupParams;
    end
    else
      PParams := ObjMapDetailFrame.UserDefinedParams;

    if (PParams<>nil) then
    begin
      Grt.DictItem[PMapping, 'params'] := PParams;

      // make sure the new targetName is set
      if (Grt.DictString[PParams, 'targetName'] = '') then
        Grt.DictString[PParams, 'targetName'] :=
          NodeData^.TargetCaption;

      //release PParams one time, since it is referenced
      //by the PMappingDefault now
      myx_grt_value_release(PParams);
    end;

    NodeData.PMapping := PMapping;

    MigLogVT.InvalidateNode(Node);

    Node := MigLogVT.GetNextSelected(Node);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

begin
  // Disable the renaming for now
  //if (Column <> 2) then
  Allowed := (Column = 2);
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapForm.IsDefaultMapping(PMapping: Pointer): Pointer;

var
  PMappings: Pointer;
  I: Integer;

begin
  Result := nil;

  // check if current mapping is a global mapping
  PMappings := Grt.Global['/migration/mappingDefaults'];
  for I := 0 to Grt.ListCount(PMappings) - 1 do
    if (PMapping = Grt.ListItem[PMappings, I]) then
    begin
      Result := PMapping;
      break;
    end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapForm.MigLogVTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);

var
  NodeData: PMapTreeData;
  PDefaultMapping,
  PParams: Pointer;

begin
  NodeData := MigLogVT.GetNodeData(Node);

  if (myx_grt_struct_inherits_from(Grt.NativeGrt,
    myx_grt_dict_struct_get_name(NodeData^.PSourceObject),
    'base.NamedGrtObject') = 1) then
  begin
    PDefaultMapping := IsDefaultMapping(NodeData^.PMapping);

    // if so, create a new mapping
    if (PDefaultMapping <> nil) then
    begin
      NodeData^.PMapping := myx_grt_value_dup(PDefaultMapping);

      Grt.DictString[NodeData^.PMapping, '_id'] := myx_grt_get_guid();
      Grt.DictString[NodeData^.PMapping, 'sourceObject'] :=
        Grt.DictString[NodeData^.PSourceObject, '_id'];

      Grt.DictString[NodeData^.PMapping, 'paramGroupName'] := '';

      //Add to mapping list
      Grt.ListAdd(Grt.Global['/migration/mappingDefinitions'],
        NodeData^.PMapping);

      myx_grt_value_release(NodeData^.PMapping);
    end;

    PParams := Grt.DictItem[NodeData^.PMapping, 'params'];

    Grt.DictString[PParams, 'targetName'] := NewText;

    NodeData^.TargetCaption := NewText;

    MigLogVT.InvalidateNode(Node);

    MigLogVTChange(MigLogVT, Node);
  end;
end;

end.
