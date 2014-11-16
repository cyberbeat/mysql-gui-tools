unit MigrationObjMapDetail;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls, TntForms,
  AdvancedEdit, VirtualTrees, ImgList, AuxFuncs, Contnrs,
  myx_public_interface, myx_grt_public_interface,
  PNGImage, TntClasses, PngTools, Grt;

type
  TMigrationObjMapDetailFrame = class(TFrame)
    ObjectSelGBox: TTntGroupBox;
    StructImage: TTntImage;
    TntPanel1: TTntPanel;
    TntLabel2: TTntLabel;
    DefMethodDescLbl: TTntLabel;
    DetailBtn: TTntButton;
    MethodComboBox: TTntComboBox;
    DetailsPnl: TTntPanel;
    TntLabel5: TTntLabel;
    TntRadioButton2: TTntRadioButton;
    TntLabel3: TTntLabel;
    TntRadioButton1: TTntRadioButton;
    UserDefinedParamsPnl: TTntPanel;
    ParamsUserDefinedCBox: TTntRadioButton;
    ParamUserDefinedStrEd: TTntEdit;
    ParameterLbl: TTntLabel;
    procedure ObjListVTGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure DetailBtnClick(Sender: TObject);
    procedure MethodComboBoxCloseUp(Sender: TObject);
    procedure ParamsCBoxClick(Sender: TObject);
    procedure ParamUserDefinedStrEdExit(Sender: TObject);
  private
    { Private declarations }
    FGrt: TGrt;
    FStructName: WideString;
    //FMethodsList: TList;
    FParamGroupList: TTntStringList;
    FListImg: TPNGObject;
    FSelectedMethod: Pointer;
    FDetailedSelection: Boolean;
    FExpandedHeight: Integer;
    FRedXImg: TTntImage;

    FOnDetailSelectionChange: TNotifyEvent;
    FOnMethodChanged: TNotifyEvent;
    FSettingMethod: Boolean;

    FTargetDatabaseObjectPackage: WideString;

    procedure SetStructName(StructName: WideString);
    procedure SetSelectedMethod(SelectedMethod: Pointer);
    procedure SetDetailedSelection(DetailedSelection: Boolean);

    function GetMethodName: WideString;
    function GetModuleName: WideString;
    function GetMigSourceStructName: WideString;
    function GetParamGroupName: WideString;
    procedure SetParamGroupName(ParamGroupName: WideString);
    function GetParamGroupParams: Pointer;
    function GetUserDefinedParams: Pointer;
    procedure SetUserDefinedParams(Params: Pointer);
  protected
    function GetParamString(Params: Pointer): WideString;
    function SetParamsEditBasedOnGroupName(
      ParamGroupName: WideString): Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; Grt: TGrt); reintroduce;
    destructor Destroy; override;

    property Grt: TGrt read FGrt write FGrt;

    property DetailedSelection: Boolean read FDetailedSelection write SetDetailedSelection;
    property OnDetailSelectionChange: TNotifyEvent read FOnDetailSelectionChange write FOnDetailSelectionChange;

    property StructName: WideString read FStructName write SetStructName;
    property SelectedMethod: Pointer read FSelectedMethod write SetSelectedMethod;
    property MigMethodName: WideString read GetMethodName;
    property MigModuleName: WideString read GetModuleName;
    property MigSourceStructName: WideString read GetMigSourceStructName;
    property ParamGroupName: WideString read GetParamGroupName write SetParamGroupName;
    property ParamGroupParams: Pointer read GetParamGroupParams;
    property UserDefinedParams: Pointer read GetUserDefinedParams write SetUserDefinedParams;

    property SettingMethod: Boolean read FSettingMethod write FSettingMethod;
    property OnMethodChanged: TNotifyEvent read FOnMethodChanged write FOnMethodChanged;

    property TargetDatabaseObjectPackage: WideString read FTargetDatabaseObjectPackage write FTargetDatabaseObjectPackage;
  end;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

constructor TMigrationObjMapDetailFrame.Create(
  AOwner: TComponent; Grt: TGrt);

begin
  inherited Create(AOwner);

  FGrt := Grt;

  //FMethodsList := TList.Create;
  FParamGroupList := TTntStringList.Create;

  FListImg := nil;
  FSelectedMethod := nil;
  FDetailedSelection := True;
  FRedXImg := nil;
  FSettingMethod := False;
end;

// -----------------------------------------------------------------------------

destructor TMigrationObjMapDetailFrame.Destroy;

begin
  //FMethodsList.Free;
  FParamGroupList.Free;

  if (FRedXImg <> nil) then
    FRedXImg.Free;

  if (Assigned(FListImg)) then
    FListImg.Free;

  inherited Destroy;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.ObjListVTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

begin
  ImageIndex:=1; //transparent Image
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.SetStructName(StructName: WideString);

var
  StructCaption: WideString;
  PStruct: Pointer;
  InheritedCaption: Integer;
  PImageData: PChar;
  DataLength: Integer;
  I: Integer;
  PMethod,
    PBestRatedMethod: Pointer;
  SourceStructName,
    TargetPackageName,
    TargetDriverPackageName,
    MethodCaption: WideString;
  SelectedMethodRating,
    MethodRating: Integer;
  PMigrationMethods: Pointer;

begin
  if (FStructName = StructName) then
    Exit;

  FStructName := StructName;

  PStruct := myx_grt_struct_get(Grt.NativeGrt, FStructName);

  if (PStruct<>nil) then
    StructCaption := UTF8Decode(_myx_grt_struct_get_caption(
      Grt.NativeGrt, PStruct, @InheritedCaption))
  else
    StructCaption := FStructName;

  ObjectSelGBox.Caption :=
    Format(_('Migration of type %s'),
      [StructCaption]);

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


  MethodComboBox.Items.Clear;

  while (DetailsPnl.ControlCount>0) do
    DetailsPnl.Controls[0].Free;

  //FMethodsList.Clear;
  PBestRatedMethod := nil;
  SelectedMethodRating := -1;
  PMigrationMethods := Grt.Global['/migration/migrationMethods'];
  if (FTargetDatabaseObjectPackage <> '') then
    TargetDriverPackageName := FTargetDatabaseObjectPackage
  else
    TargetDriverPackageName := Grt.DictString[
      Grt.DictRef[
        Grt.DictRef[
          Grt.Global['/migration/targetConnection'], 'driver'],
        'owner'],
      'databaseObjectPackage'];

  for I:=0 to myx_grt_list_item_count(PMigrationMethods)-1 do
  begin
    PMethod := myx_grt_list_item_get(PMigrationMethods, I);

    SourceStructName := Grt.DictString[PMethod, 'sourceStructName'];

    TargetPackageName := Grt.DictString[PMethod, 'targetPackageName'];

    MethodCaption := Grt.DictString[PMethod, 'caption'];

    MethodRating := myx_grt_dict_item_get_as_int(PMethod, 'rating');

    //Check if the method can migration a FStructName to
    //a Mig.TargetDBConn.JdbcDriver.ObjectPackageName package object
    if (((myx_grt_struct_inherits_from(Grt.NativeGrt,
        FStructName, SourceStructName) = 1) or
        (WideSameText(FStructName, SourceStructName)) ) and
      (WideSameText(TargetDriverPackageName, TargetPackageName))) then
    begin
      //FMethodsList.Add(PMethod);

      MethodComboBox.Items.AddObject(MethodCaption, PMethod);

      if (MethodRating>SelectedMethodRating) then
      begin
        PBestRatedMethod := PMethod;
        SelectedMethodRating := MethodRating;
      end;
    end;
  end;

  if (PBestRatedMethod<>nil) then
    SelectedMethod := PBestRatedMethod;

  // If there is not at least one method for this struct
  // mark with an X
  if (SelectedMethod=nil) then
  begin
    DisableEnableControls(ObjectSelGBox, False);

    FRedXImg := TTntImage.Create(self);
    FRedXImg.Parent := ObjectSelGBox;
    FRedXImg.Left := 26;
    FRedXImg.Top := 26;

    LoadPNGImageFromResource('red_x', FRedXImg);

    DetailBtn.Visible := False;
  end;

  DetailedSelection := False;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.SetSelectedMethod(SelectedMethod: Pointer);

var
  I: Integer;
  PParams,
    PParamGroupsList,
    PParamGroup: Pointer;
  ParamGroupRBtn: TTntRadioButton;
  ParamGroupLbl: TTntLabel;
  SingleLineCount: Integer;

begin
  if (SelectedMethod <> FSelectedMethod) then
  begin
    FSelectedMethod := SelectedMethod;

    if (FSelectedMethod<>nil) then
    begin
      // Set Method caption and description
      MethodComboBox.ItemIndex :=
        MethodComboBox.Items.IndexOfObject(FSelectedMethod);

      DefMethodDescLbl.Caption :=
        UTF8Decode(_myx_grt_dict_item_get_as_string(
          FSelectedMethod, 'desc'));

      // Build parameter groups
      PParamGroupsList := myx_grt_dict_item_get_value(FSelectedMethod,
        'paramGroups');

      FParamGroupList.Clear;
      if (PParamGroupsList<>nil)then
      begin
        SingleLineCount := 0;

        for I:=0 to myx_grt_list_item_count(PParamGroupsList) - 1 do
        begin
          PParamGroup := myx_grt_list_item_get(PParamGroupsList, I);

          ParamGroupRBtn := TTntRadioButton.Create(self);
          ParamGroupRBtn.Parent := DetailsPnl;
          ParamGroupRBtn.Left := 0;
          ParamGroupRBtn.Top := I * 58 - SingleLineCount * 13;
          ParamGroupRBtn.Width := 343;
          ParamGroupRBtn.Font.Style := [fsBold];
          ParamGroupRBtn.Caption :=
            UTF8Decode(_myx_grt_dict_item_get_as_string(
              PParamGroup, 'name'));
          ParamGroupRBtn.Tag := I;
          ParamGroupRBtn.OnClick := ParamsCBoxClick;

          FParamGroupList.AddObject(
            UTF8Decode(_myx_grt_dict_item_get_as_string(
              PParamGroup, 'name')),
            ParamGroupRBtn);

          ParamGroupLbl := TTntLabel.Create(self);
          ParamGroupLbl.Parent := DetailsPnl;
          ParamGroupLbl.AutoSize := False;
          ParamGroupLbl.Left := 48;
          ParamGroupLbl.Top := 18 + I * 58 - SingleLineCount * 13;
          ParamGroupLbl.Width := 459;
          ParamGroupLbl.Height := 28;
          ParamGroupLbl.WordWrap := True;
          ParamGroupLbl.Caption :=
            UTF8Decode(_myx_grt_dict_item_get_as_string(
              PParamGroup, 'desc'));

          if (I=0) then
            ParamGroupRBtn.Checked := True;

          if (GetWideStringTextWidth(Application.MainForm.Canvas,
            ParamGroupLbl.Caption)<ParamGroupLbl.Width) then
            inc(SingleLineCount);
        end;

        FExpandedHeight := 126 + 49 +
          58 * myx_grt_list_item_count(PParamGroupsList) -
          SingleLineCount * 13;
      end;
    end;
  end;

  // Build user parameters string from defaults
  ParamUserDefinedStrEd.Text := '';

  PParams := myx_grt_dict_item_get_value(FSelectedMethod,
    'params');

  if (PParams <> nil) then
  begin
    for I:=myx_grt_dict_item_count(PParams)-1 downto 0 do
    begin
      ParamUserDefinedStrEd.Text :=
        myx_grt_dict_item_key_by_index(
          PParams, I) + '=' +
        myx_grt_value_as_string(
            myx_grt_dict_item_value_by_index(
              PParams, I))+
        ParamUserDefinedStrEd.Text;

      if (I > 0) then
        ParamUserDefinedStrEd.Text := ', ' +
          ParamUserDefinedStrEd.Text;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.SetDetailedSelection(DetailedSelection: Boolean);

begin
  if (FDetailedSelection<>DetailedSelection) then
  begin
    FDetailedSelection := DetailedSelection;

    if (FDetailedSelection) then
    begin
      Height := FExpandedHeight;
      DetailsPnl.Visible := True;
      UserDefinedParamsPnl.Visible := True;
      ParameterLbl.Visible := True;
      DetailBtn.Caption := _('<< Hide Details');
    end
    else
    begin
      DetailsPnl.Visible := False;
      UserDefinedParamsPnl.Visible := False;
      ParameterLbl.Visible := False;
      DetailBtn.Caption := _('Set Parameter >>');

      if (DetailBtn.Visible) then
        Height := 98
      else
        Height := 78;
    end;

    if (Assigned(FOnDetailSelectionChange)) then
      FOnDetailSelectionChange(self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.DetailBtnClick(Sender: TObject);

begin
  DetailedSelection := Not(DetailedSelection);
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetMethodName: WideString;

begin
  if (SelectedMethod<>nil) then
    Result := UTF8Decode(_myx_grt_dict_item_get_as_string(
      SelectedMethod, 'name'))
  else
    Result := '';
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetModuleName: WideString;

begin
  Result := '';

  if (SelectedMethod<>nil) then
    Result := UTF8Decode(_myx_grt_dict_item_get_as_string(
      SelectedMethod, 'moduleName'));
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetMigSourceStructName: WideString;

begin
  Result := '';

  if (SelectedMethod<>nil) then
    Result := UTF8Decode(_myx_grt_dict_item_get_as_string(
      SelectedMethod, 'sourceStructName'));
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetParamGroupName: WideString;

var
  I: Integer;

begin
  Result := '';

  if (SelectedMethod<>nil) then
  begin
    for I:=0 to FParamGroupList.Count-1 do
      if (TTntRadioButton(FParamGroupList.Objects[I]).Checked) then
      begin
        Result := FParamGroupList[I];
        break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.SetParamsEditBasedOnGroupName(
  ParamGroupName: WideString): Integer;

var
  PParamGroupsList,
    PParamGroup: Pointer;
  I: Integer;

begin
  Result := FParamGroupList.IndexOf(ParamGroupName);

  if (Result > -1) then
  begin
    // set the correct parameters
    PParamGroupsList := Grt.DictItem[FSelectedMethod, 'paramGroups'];
    for I:=0 to Grt.ListCount(PParamGroupsList) - 1 do
    begin
      PParamGroup := Grt.ListItem[PParamGroupsList, I];

      if (Grt.DictString[PParamGroup, 'name'] = ParamGroupName) then
      begin
        ParamUserDefinedStrEd.Text :=
          GetParamString(Grt.DictItem[PParamGroup, 'params']);
        break;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.SetParamGroupName(ParamGroupName: WideString);

var
  I: Integer;

begin
  for I := 0 to DetailsPnl.ControlCount - 1 do
    if (DetailsPnl.Controls[I] is TTntRadioButton) then
      TTntRadioButton(DetailsPnl.Controls[I]).Checked := False;
  ParamsUserDefinedCBox.Checked := False;

  // Set parameter edit text and select checkbox
  I := SetParamsEditBasedOnGroupName(ParamGroupName);
  if (I > -1) then
    TTntRadioButton(FParamGroupList.Objects[I]).Checked := True;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetParamGroupParams: Pointer;

var
  I: Integer;
  PParamGroupsList,
    PParamGroup: Pointer;

begin
  Result := nil;

  if (SelectedMethod<>nil) then
  begin
    for I:=0 to FParamGroupList.Count-1 do
      if (TTntRadioButton(FParamGroupList.Objects[I]).Checked) then
      begin
        PParamGroupsList := myx_grt_dict_item_get_value(
          SelectedMethod, 'paramGroups');

        PParamGroup := myx_grt_list_item_get(PParamGroupsList, I);
        Result := myx_grt_value_dup(
          myx_grt_dict_item_get_value(PParamGroup, 'params'));
        break;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetUserDefinedParams: Pointer;

var
  I: Integer;
  PParams: Pointer;
  Params: TTntStringList;

begin
  Result := nil;
  
  if (ParamsUserDefinedCBox.Checked) then
  begin
    PParams := myx_grt_dict_new_typed(MYX_STRING_VALUE, '');
    Params := TTntStringList.Create;
    try
      Params.CommaText := ParamUserDefinedStrEd.Text;

      for I:=0 to Params.Count-1 do
        if (Params[I]<>'') then
          myx_grt_dict_item_set_value_from_string(PParams,
            Params.Names[I], Params.ValueFromIndex[I]);
    finally
      Params.Free;
    end;

    Result := PParams;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjMapDetailFrame.GetParamString(Params: Pointer): WideString;

var
  I,
    Count: Integer;
  Value: Pointer;

begin
  Result := '';

  if (Params = nil) then
    Exit;

  Count := myx_grt_dict_item_count(Params);

  for I := 0 to count - 1 do
  begin
    Result := Result +
      myx_grt_dict_item_key_by_index(Params, I) + '=';

    Value := myx_grt_dict_item_value_by_index(Params, I);

    if (Value <> nil) and
      (myx_grt_value_get_type(Value) = MYX_STRING_VALUE) then
      Result := Result +
        myx_grt_value_as_string(Value);

    if (I < Count - 1) then
      Result := Result + ', ';
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.SetUserDefinedParams(Params: Pointer);

var
  I: Integer;

begin
  ParamUserDefinedStrEd.Text := GetParamString(Params);

  // Check radio box
  for I := 0 to DetailsPnl.ControlCount - 1 do
    if (DetailsPnl.Controls[I] is TTntRadioButton) then
      TTntRadioButton(DetailsPnl.Controls[I]).Checked := False;
  ParamsUserDefinedCBox.Checked := True;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.MethodComboBoxCloseUp(
  Sender: TObject);
begin
  if (MethodComboBox.ItemIndex > -1) then
    SelectedMethod := MethodComboBox.Items.Objects[
      MethodComboBox.ItemIndex];

  if (Assigned(FOnMethodChanged)) and (Not(SettingMethod)) then
    FOnMethodChanged(self);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.ParamsCBoxClick(
  Sender: TObject);

var
  I: Integer;

begin
  if (Sender = ParamsUserDefinedCBox) then
  begin
    for I := 0 to DetailsPnl.ControlCount - 1 do
      if (DetailsPnl.Controls[I] is TTntRadioButton) then
        TTntRadioButton(DetailsPnl.Controls[I]).Checked := False;
  end
  else
  begin
    ParamsUserDefinedCBox.Checked := False;
    
    SetParamsEditBasedOnGroupName(ParamGroupName);
  end;

  if (Assigned(FOnMethodChanged)) and (Not(SettingMethod)) then
    FOnMethodChanged(self);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjMapDetailFrame.ParamUserDefinedStrEdExit(
  Sender: TObject);
begin
  if (Assigned(FOnMethodChanged)) and (Not(SettingMethod)) then
    FOnMethodChanged(self);
end;

end.
