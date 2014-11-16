unit MyxConnectionDialog;

interface

uses
  GnuGetText, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, TntButtons, StdCtrls, TntStdCtrls, VirtualTrees,
  ExtCtrls, TntExtCtrls, ComCtrls, PNGImage, PNGTools,
  AuxFuncs, TntForms, TntClasses, Grt, TntSysUtils,
  Menus, TntMenus, MyxBaseForm, myx_grt_public_interface;

{$include Consts.ini}

const ParamLineHeight = 32;

type
  TDriverFilter = function (PDriver: Pointer): Boolean of object;

  TMyxConnectionDialogForm = class(TMyxBaseForm)
    HeaderPnl: TTntPanel;
    HeaderImg: TTntImage;
    ParamsMainPnl: TTntPanel;
    AdvParamsMainPnl: TTntPanel;
    ConnectGBox: TTntGroupBox;
    BottomPnl: TTntPanel;
    OKBtn: TTntButton;
    CancelBtn: TTntButton;
    ClearFieldsBtn: TTntButton;
    AdvancedBtn: TTntBitBtn;
    ConnTypeMainPnl: TTntPanel;
    ConnTypeGBox: TTntGroupBox;
    ConnTypePnl: TTntPanel;
    DriverLbl: TTntLabel;
    DriverComboBox: TTntComboBox;
    RdbmsComboBox: TTntComboBox;
    RdbmsLbl: TTntLabel;
    StoredConnPnl: TTntPanel;
    ConnectionLbl: TTntLabel;
    StoredConnComboBox: TTntComboBox;
    StoredConnAddBtn: TTntSpeedButton;
    StoredConnDelBtn: TTntSpeedButton;
    ParamsPnl: TTntPanel;
    ConnectToInstanceAni: TAnimate;
    DriverNotInstalledPnl: TTntPanel;
    TntLabel2: TTntLabel;
    TntLabel9: TTntLabel;
    LocateDriverBtn: TTntButton;
    DownloadDriverPnl: TTntPanel;
    TntLabel12: TTntLabel;
    DownloadDriverBtn: TTntButton;
    LookupMenu: TTntPopupMenu;
    TntGroupBox1: TTntGroupBox;
    AdvParamPnl: TTntPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboBoxCloseUp(Sender: TObject);
    procedure RdbmsComboBoxCloseUp(Sender: TObject);
    procedure StoredConnComboBoxCloseUp(Sender: TObject);
    procedure AdvancedBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure StoredConnAddBtnClick(Sender: TObject);
    procedure ClearFieldsBtnClick(Sender: TObject);
    procedure StoredConnDelBtnClick(Sender: TObject);
    procedure LocateDriverBtnClick(Sender: TObject);
    procedure DownloadDriverBtnClick(Sender: TObject);
  private
    { Private declarations }
    FParamMappingList: TList;
    FSettingConnectionValues,
      FDisplayRdbmsSelection,
      FDisplayDescriptions,
      FDisplaySchemaSelection,
      FDisplayOnlyJdbcDrivers,
      FSchemaSelectionRequired: Boolean;

    FConnInfoPath,
      FConnTargetPath: WideString;

    FOkBtn: TTntButton;
    FFirstParamControl: TWinControl;

    FDriverFilter: TDriverFilter;

    FRdbmsDescLbl,
      FDriverDescLbl: TTntLabel;

    procedure FreeChildControls(Control: TWinControl);
    procedure BuildDriverControls(
      Target: TWinControl; PDriver: Pointer;
      DoAdvancedParams: Boolean = False);

    procedure DriverParamValueChanged(Sender: TObject);
    procedure ParamLookupFuncOutput(Text: WideString);
    procedure ParamLookupBtnClick(Sender: TObject);
    procedure ParamLookupMIClick(Sender: TObject);
    procedure BrowseDirBtnClick(Sender: TObject);
    procedure BrowseFileBtnClick(Sender: TObject);

    function BuildDriverComboBoxItems(PRdbms: Pointer): Integer;

    procedure FillDropdownWithStoredConnections(
      StoredConnComboBox: TTntComboBox; PDriver: Pointer);

    procedure SetSelectedRdbms(SelectedRdbms: WideString);
    function GetSelectedRdbms: WideString;

    procedure SetDisplayRdbmsSelection(DisplayRdbmsSelection: Boolean);

    function GetParamValue(const Name: WideString): WideString;
    procedure SetParamValue(const Name: WideString; Value: WideString);

    procedure SetDisplayDescriptions(DisplayDescriptions: Boolean);

    procedure ParamLookupBtnClickCallback(Task: IGrtGenericTask);
  public
    { Public declarations }
    property ConnInfoPath: WideString read FConnInfoPath write FConnInfoPath;
    property ConnTargetPath: WideString read FConnTargetPath write FConnTargetPath;

    property DisplayRdbmsSelection: Boolean read FDisplayRdbmsSelection write SetDisplayRdbmsSelection;
    property DisplayDescriptions: Boolean read FDisplayDescriptions write SetDisplayDescriptions;
    property DisplaySchemaSelection: Boolean read FDisplaySchemaSelection write FDisplaySchemaSelection;
    property SchemaSelectionRequired: Boolean read FSchemaSelectionRequired write FSchemaSelectionRequired;
    property DisplayOnlyJdbcDrivers: Boolean read FDisplayOnlyJdbcDrivers write FDisplayOnlyJdbcDrivers;

    property DriverFilter: TDriverFilter read FDriverFilter write FDriverFilter;
    property OkButton: TTntButton read FOkBtn write FOkBtn;
    property SelectedRdbms: WideString read GetSelectedRdbms write SetSelectedRdbms;

    property ParamValue[const Name: WideString]: WideString read GetParamValue write SetParamValue;

    procedure RefreshConnInfo;
    function WriteConnectionToTarget: Pointer;

    procedure SetConnection(PConnection: Pointer);   
  end;

  PParamMapping = ^TParamMapping;
  TParamMapping = record
    PParam: Pointer;
    ParamControl: TControl;
    Lbl: TTntLabel;
    BrowseBtn: TTntButton;
  end;

implementation

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  //Load resources
  LoadPNGImageFromResource('connection_dialog_header', HeaderImg, True);
  ConnectToInstanceAni.ResName := 'progress_indicator';

  Caption := Application.Title + ' '+
    product_version + ' ' +
    product_build_level;

  FParamMappingList := TList.Create;
  FSettingConnectionValues := False;
  FDisplayRdbmsSelection := False;
  FDisplayDescriptions := False;
  FDisplaySchemaSelection := True;
  FDisplayOnlyJdbcDrivers := False;
  FSchemaSelectionRequired := False;

  FDriverFilter := nil;

  FOkBtn := OKBtn;
  FFirstParamControl := nil;

  FRdbmsDescLbl := nil;
  FDriverDescLbl := nil;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.FormDestroy(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to FParamMappingList.Count - 1 do
    dispose(FParamMappingList[I]);
  FParamMappingList.Free;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.RefreshConnInfo;

var
  I: integer;
  PRdbmsList,
    PRdbms: Pointer;

begin
  PRdbmsList := Grt.Global[FConnInfoPath + '/rdbms'];

  // Clear controls
  RDBMSComboBox.Clear;
  DriverComboBox.Items.Clear;
  DriverComboBoxCloseUp(self);

  //Set RDBMSCBox Items
  for I := 0 to Grt.ListCount(PRdbmsList) - 1 do
  begin
    PRdbms := Grt.ListItem[PRdbmsList, I];

    if (BuildDriverComboBoxItems(PRdbms) > 0) then
      RDBMSComboBox.AddItem(
        Grt.DictString[PRdbms, 'caption'],
        PRdbms);
  end;

  RDBMSComboBoxCloseUp(self);
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.FreeChildControls(Control: TWinControl);

begin
  while (Control.ControlCount > 0) do
    Control.Controls[0].Free;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.BuildDriverControls(
  Target: TWinControl; PDriver: Pointer; DoAdvancedParams: Boolean);

var
  I: Integer;
  PParams,
    PParam: Pointer;
  Caption,
    Desc,
    ParamType: WideString;
  OffsetLeft,
    CurrentRow,
    MaxRow,
    CurrentTop: Integer;
  CurrentLeft: Array [1 .. 30] of Integer;
  RowDescription: TTntStringList;
  ParamMapping: PParamMapping;
  DescLbl: TTntLabel;

begin
  RowDescription := TTntStringList.Create;
  PParams := Grt.DictItem[PDriver, 'parameters'];

  OffsetLeft := 116;
  MaxRow := 0;

  for I := 1 to 30 do
  begin
    CurrentLeft[I] := 0;
    RowDescription.Add('');
  end;

  for I := 0 to Grt.ListCount(PParams) - 1 do
  begin
    PParam := Grt.ListItem[PParams, I];

    if (Grt.DictInt[PParam, 'layoutAdvanced'] <>
      Ord(DoAdvancedParams)) then
      continue;

    // if the "schema" should not be displayed
    if (Not(FDisplaySchemaSelection)) and
      (WideSameText(Grt.DictString[PParam, 'name'], 'schema')) then
      continue;

    CurrentRow := Grt.DictInt[PParam, 'layoutRow'];
    if (CurrentRow = -1) then
      CurrentRow := MaxRow + 1;
    MaxRow := CurrentRow;

    CurrentTop := 1 + (CurrentRow - 1) * ParamLineHeight;

    Caption := Grt.DictString[PParam, 'caption'];
    Desc := Grt.DictString[PParam, 'description'];
    ParamType := Grt.DictString[PParam, 'paramType'];

    // Create mapping
    New(ParamMapping);
    FParamMappingList.Add(ParamMapping);
    ParamMapping.PParam := PParam;


    if (Not(WideSameText(ParamType, 'boolean'))) and
      (Not(WideSameText(ParamType, 'tristate'))) then
    begin
      // Create Edit with Caption
      if (Caption <> '') then
      begin
        ParamMapping.Lbl := TTntLabel.Create(self);

        //CaptionLbl.Name := 'ParamLbl' + IntToStr(I);
        ParamMapping.Lbl.Caption := _(Caption);

        // if this is the first param on that row,
        // move the CaptionLbl to the left
        // so the Param edits are aligned left
        if (CurrentLeft[CurrentRow] = 0) then
          ParamMapping.Lbl.Left := OffsetLeft + CurrentLeft[CurrentRow] -
            ParamMapping.Lbl.Width - 10
        else
        begin
          ParamMapping.Lbl.Left := OffsetLeft + CurrentLeft[CurrentRow];
          CurrentLeft[CurrentRow] := CurrentLeft[CurrentRow] +
            ParamMapping.Lbl.Width + 10;
        end;

        ParamMapping.Lbl.Top := CurrentTop + 4;

        ParamMapping.Lbl.Parent := Target;
      end;


      // Create Param Edit
      ParamMapping.ParamControl := TTntEdit.Create(self);

      ParamMapping.ParamControl.Left :=
        OffsetLeft + CurrentLeft[CurrentRow];

      TTntEdit(ParamMapping.ParamControl).OnChange :=
        DriverParamValueChanged;

      TTntEdit(ParamMapping.ParamControl).Text :=
        Grt.DictString[PParam, 'defaultValue'];

      // Set password char for password fields
      if (WideSameText(ParamType, 'password')) then
        TTntEdit(ParamMapping.ParamControl).PasswordChar := '*';
    end
    else
    begin
      // Create Checkbox
      ParamMapping.ParamControl := TTntCheckbox.Create(self);

      CurrentLeft[CurrentRow] := CurrentLeft[CurrentRow] - 100;

      ParamMapping.ParamControl.Left :=
        OffsetLeft + CurrentLeft[CurrentRow];

      TTntCheckbox(ParamMapping.ParamControl).OnClick :=
        DriverParamValueChanged;

      TTntCheckbox(ParamMapping.ParamControl).Checked :=
        (Grt.DictString[PParam, 'defaultValue'] = '1');

      TTntCheckbox(ParamMapping.ParamControl).Caption := _(Caption);
    end;

    if (FParamMappingList.Count = 1) then
      FFirstParamControl := TWinControl(ParamMapping.ParamControl);

    // Set common options
    //ParamControl.Name := 'Param' + IntToStr(I);
    ParamMapping.ParamControl.Top := CurrentTop;

    ParamMapping.ParamControl.Width :=
      Grt.DictInt[PParam, 'layoutWidth'];

    if (Desc <> '') then
    begin
      ParamMapping.ParamControl.Hint := Desc;
      ParamMapping.ParamControl.ShowHint := True;
    end;

    // move CurrentLeft
    CurrentLeft[CurrentRow] := CurrentLeft[CurrentRow] +
      ParamMapping.ParamControl.Width + 20;

    ParamMapping.ParamControl.Parent := Target;

    // Add lookup button
    if (Grt.DictString[PParam, 'lookupValueMethod'] <> '') or
      WideSameText(ParamType, 'file') or
      WideSameText(ParamType, 'dir') then
    begin
      ParamMapping.BrowseBtn := TTntButton.Create(self);
      //BrowseBtn.Name := 'LookupBtn' + IntToStr(I);
      ParamMapping.BrowseBtn.Tag := FParamMappingList.Count - 1;

      ParamMapping.BrowseBtn.Caption := _('...');
      ParamMapping.BrowseBtn.Width := 27;
      ParamMapping.BrowseBtn.Height := 23;
      ParamMapping.BrowseBtn.Left :=
        ParamMapping.ParamControl.Left +
        ParamMapping.ParamControl.Width -
        ParamMapping.BrowseBtn.Width;
      ParamMapping.BrowseBtn.Top := CurrentTop;

      ParamMapping.ParamControl.Width :=
        ParamMapping.ParamControl.Width -
        ParamMapping.BrowseBtn.Width - 6;

      // Add lookup action
      if (Grt.DictString[PParam, 'lookupValueMethod'] <> '') then
        ParamMapping.BrowseBtn.OnClick := ParamLookupBtnClick
      else
        if (WideSameText(ParamType, 'file')) then
          ParamMapping.BrowseBtn.OnClick := BrowseFileBtnClick
        else
          if (WideSameText(ParamType, 'dir')) then
            ParamMapping.BrowseBtn.OnClick := BrowseDirBtnClick;

      ParamMapping.BrowseBtn.Parent := Target;
    end;

    // build parameter description
    if (Desc <> '') and (CurrentRow - 1 >= 0) and
      (CurrentRow - 1 < RowDescription.Count) then
    begin
      if (RowDescription[CurrentRow - 1] = '') then
        RowDescription[CurrentRow - 1] := _(Desc)
      else
        RowDescription[CurrentRow - 1] :=
          RowDescription[CurrentRow - 1] + ' - ' + _(Desc);
    end;
  end;

  // Add descriptions for all lines
  if (FDisplayDescriptions) then
    for I := 0 to 29 do
    begin
      CurrentTop := I * ParamLineHeight;

      if (RowDescription[I] <> '') then
      begin
        DescLbl := TTntLabel.Create(self);
        //CaptionLbl.Name := 'ParamDescLbl' + IntToStr(I + 1);
        DescLbl.Caption := RowDescription[I];

        DescLbl.Left := OffsetLeft + CurrentLeft[I + 1];

        DescLbl.Top := CurrentTop + 4;

        DescLbl.Parent := Target;
      end;
    end;

  RowDescription.Free;

  Target.Height := MaxRow * ParamLineHeight;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.DriverParamValueChanged(Sender: TObject);

var
  I: Integer;
  RequiredParamsSet: Boolean;
  ParamMapping: PParamMapping;

begin
  if (Not(FSettingConnectionValues)) then
  begin
    // Check if all required fields are set
    RequiredParamsSet := True;
    for I := 0 to FParamMappingList.Count - 1 do
    begin
      ParamMapping := FParamMappingList[I];

      if (Grt.DictInt[ParamMapping.PParam, 'required'] = 1) or
        (FSchemaSelectionRequired and
          (WideSameText(Grt.DictString[ParamMapping.PParam, 'name'], 'schema'))
        ) then
      begin
        if (ParamMapping.ParamControl is TTntEdit) then
          if (TTntEdit(ParamMapping.ParamControl).Text = '') then
          begin
            RequiredParamsSet := False;
            break;
          end;
      end;
    end;

    FOkBtn.Enabled := RequiredParamsSet;

    StoredConnAddBtn.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.ParamLookupFuncOutput(Text: WideString);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.ParamLookupBtnClickCallback(Task: IGrtGenericTask);

var
  MenuItem: TTntMenuItem;
  I: Integer;
  ScreenPoint: TPoint;
  S: WideString;

begin
  try
    if (Length(Task.ErrorString) > 0) then
    begin
      MenuItem := TTntMenuItem.Create(LookupMenu);
        MenuItem.Caption := _('Fetching of list failed.');
      MenuItem.Tag := -1;
      LookupMenu.Items.Add(MenuItem);

      S := Task.ErrorString;

      if (Pos(#13#10, S) > 0) then
        S := Copy(S, 1, Pos(#13#10, S) - 1);

      MenuItem := TTntMenuItem.Create(LookupMenu);
        MenuItem.Caption := S;
      MenuItem.Tag := -1;
      LookupMenu.Items.Add(MenuItem);
    end
    else
    begin
      for I := 0 to Grt.ListCount(IGrtTask(Task).Result) - 1 do
      begin
        MenuItem := TTntMenuItem.Create(LookupMenu);
        MenuItem.Caption := Grt.ListString[IGrtTask(Task).Result, I];

        MenuItem.OnClick := ParamLookupMIClick;
        MenuItem.Tag := Task.Tag;

        LookupMenu.Items.Add(MenuItem);
      end;

      Grt.ValueRelease(IGrtTask(Task).Result);
    end;

    if (Assigned(Task.DataObj)) then
    begin
      ScreenPoint := TControl(Task.DataObj).ClientToScreen(Point(0, 0));

      LookupMenu.Popup(ScreenPoint.X, ScreenPoint.Y + TControl(Task.DataObj).Height);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.ParamLookupBtnClick(Sender: TObject);

var
  //Res: Pointer;
  MenuItem: TTntMenuItem;
  ParamMapping: PParamMapping;
  S: WideString;
  Task: IGrtTask;
  Module: WideString;
  Method: WideString;

begin
  if (Sender is TControl) and
    (TControl(Sender).Tag < FParamMappingList.Count) then
  begin
    ParamMapping := FParamMappingList[TControl(Sender).Tag];

    LookupMenu.Items.Clear;

    // Store current settings
    WriteConnectionToTarget;

    try
      // Call LookupValueMethod
      Module := Grt.DictString[ParamMapping.PParam, 'lookupValueModule'];
      Method := Grt.DictString[ParamMapping.PParam, 'lookupValueMethod'];
      Task := Grt.CreateStandardTask('Fetching parameter list', Module, Method, [Grt.GetGlobalAsParam(FConnTargetPath)],
        ParamLookupFuncOutput, nil, False, False, -1, nil,
        nil, ParamLookupBtnClickCallback, TControl(Sender).Tag, Sender);
      Grt.AddTask(Task);

      Screen.Cursor := crHourglass;
    except
      on x: Exception do
      begin
        MenuItem := TTntMenuItem.Create(LookupMenu);
          MenuItem.Caption := _('Fetching of list failed.');
        MenuItem.Tag := -1;
        LookupMenu.Items.Add(MenuItem);

        S := x.Message;

        if (Pos(#13#10, S) > 0) then
          S := Copy(S, 1, Pos(#13#10, S) - 1);

        MenuItem := TTntMenuItem.Create(LookupMenu);
          MenuItem.Caption := S;
        MenuItem.Tag := -1;
        LookupMenu.Items.Add(MenuItem);
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.ParamLookupMIClick(Sender: TObject);

var
  ParamMapping: PParamMapping;

begin
  if (Sender is TTntMenuItem) and
    (TTntMenuItem(Sender).Tag > -1) then
  begin
    ParamMapping := FParamMappingList[TTntMenuItem(Sender).Tag];

    if (ParamMapping.ParamControl is TTntEdit) then
      TTntEdit(ParamMapping.ParamControl).Text :=
        Tnt_WideStringReplace(TTntMenuItem(Sender).Caption, '&', '',
          [rfReplaceAll]);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.BrowseFileBtnClick(
  Sender: TObject);

var
  OpenDlg: TOpenDialog;
  ParamMapping: PParamMapping;
  PParamTypeDetails: Pointer;
  FileType,
    FileExtension: WideString;

begin
  if (Sender is TControl) and
    (TControl(Sender).Tag < FParamMappingList.Count) then
  begin
    ParamMapping := FParamMappingList[TControl(Sender).Tag];

    PParamTypeDetails :=
      Grt.DictItem[ParamMapping.PParam, 'paramTypeDetails'];

    OpenDlg := TOpenDialog.Create(self);
    try
      if (PParamTypeDetails <> nil) then
      begin
        OpenDlg.Title :=
          Grt.DictString[PParamTypeDetails, 'fileOpenDialogCaption'];

        FileType :=
          Grt.DictString[PParamTypeDetails, 'fileType'];

        FileExtension :=
          Grt.DictString[PParamTypeDetails, 'fileExtension'];
      end;

      if (OpenDlg.Title = '') then
        OpenDlg.Title := _('Open File ...');

      if (FileType <> '') and (FileExtension <> '') then
        OpenDlg.Filter := FileType +
          ' (*.' + FileExtension + ')|*.' + FileExtension + '|' +
          _('All files') + ' (*.*)|*.*'
      else
        OpenDlg.Filter := _('All files') + ' (*.*)|*.*';

      if (OpenDlg.Execute) then
      begin
        if (ParamMapping.ParamControl is TTntEdit) then
          TTntEdit(ParamMapping.ParamControl).Text := OpenDlg.FileName;
      end;
    finally
      OpenDlg.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.BrowseDirBtnClick(Sender: TObject);

begin
  //
end;

// -----------------------------------------------------------------------------

function TMyxConnectionDialogForm.BuildDriverComboBoxItems(PRdbms: Pointer): Integer;

var
  I: integer;
  PDrivers,
    PDriver: Pointer;

begin
  PDrivers := Grt.DictItem[PRdbms, 'drivers'];

  DriverComboBox.Clear;

  for I := 0 to Grt.ListCount(PDrivers) - 1 do
  begin
    PDriver := Grt.ListItem[PDrivers, I];

    if (FDisplayOnlyJdbcDrivers) and
      (Grt.DictStructName[PDriver] <> 'db.mgmt.JdbcDriver') then
      continue;

    if (Assigned(FDriverFilter)) then
      if (DriverFilter(PDriver)) then
        continue;

    // place default driver on top of dropdown
    if (Grt.DictRef[PRdbms, 'defaultDriver'] = PDriver) then
      DriverComboBox.Items.InsertObject(
        0,
        Grt.DictString[PDriver, 'caption'],
        PDriver)
    else
      DriverComboBox.AddItem(
        Grt.DictString[PDriver, 'caption'],
        PDriver);
  end;

  DriverComboBox.Enabled := (DriverComboBox.Items.Count > 1);
  DriverLbl.Enabled := (DriverComboBox.Items.Count > 1);
  if (FDriverDescLbl <> nil) then
    FDriverDescLbl.Enabled := DriverComboBox.Enabled;

  Result := DriverComboBox.Items.Count;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.DriverComboBoxCloseUp(Sender: TObject);

var
  //PRdbms: Pointer;
  PDriver: Pointer;
  I: Integer;

begin
  // Clear mapping
  for I := 0 to FParamMappingList.Count - 1 do
    dispose(FParamMappingList[I]);
  FParamMappingList.Clear;
  FFirstParamControl := nil;

  FreeChildControls(ParamsPnl);
  FreeChildControls(AdvParamPnl);


  if (DriverComboBox.ItemIndex > -1) then
  begin
    //PRdbms := RdbmsComboBox.Items.Objects[RdbmsComboBox.ItemIndex];
    PDriver :=
      DriverComboBox.Items.Objects[DriverComboBox.ItemIndex];

    if (Grt.DictInt[PDriver, 'isInstalled'] = 1) then
    begin
      DriverNotInstalledPnl.Visible := False;

      StoredConnComboBox.ItemIndex := -1;
      StoredConnDelBtn.Enabled := False;

      // Build params
      BuildDriverControls(ParamsPnl, PDriver);
      ParamsMainPnl.Height := StoredConnPnl.Height +
        ParamsPnl.Height + 38;

      // Build advanced params
      BuildDriverControls(AdvParamPnl, PDriver, True);
      AdvParamsMainPnl.Height := AdvParamPnl.Height + 38;

      // Get stored connections
      FillDropdownWithStoredConnections(StoredConnComboBox, PDriver);

      {JdbcDriverNameLbl.Caption := _(myx_grt_dict_item_get_as_string(
        PRdbms, 'caption'));
      JdbcDriverDescLbl.Caption := _(myx_grt_dict_item_get_as_string(
        PJdbcDriver, 'description'));}

      StoredConnAddBtn.Enabled := False;

      StoredConnPnl.Visible := True;
    end
    else
    begin
      DownloadDriverPnl.Visible :=
        (Grt.DictString[PDriver, 'downloadUrl'] <> '');

      DriverNotInstalledPnl.BringToFront;
      DriverNotInstalledPnl.Visible := True;
      ParamsMainPnl.Height := 201;

      AdvParamsMainPnl.Height := 38;

      StoredConnPnl.Visible := False;
    end;
  end
  else
  begin
    {JdbcDriverNameLbl.Caption := _('No RDBMS selected');
    JdbcDriverDescLbl.Caption := _('Please choose a Database System from the list above.');}

    StoredConnPnl.Visible := False;

    StoredConnAddBtn.Enabled := False;

    FreeChildControls(ParamsPnl);
  end;

  try
    if (FFirstParamControl <> nil) then
      GetParentForm(FFirstParamControl).ActiveControl :=
        FFirstParamControl;
  except
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.FillDropdownWithStoredConnections(
  StoredConnComboBox: TTntComboBox; PDriver: Pointer);

var
  I: Integer;
  PConnections,
    PConnection: Pointer;

begin
  PConnections := Grt.Global[FConnInfoPath + '/storedConns'];

  StoredConnComboBox.Items.Clear;
  StoredConnComboBox.ItemIndex := -1;

  for I := 0 to Grt.ListCount(PConnections) - 1 do
  begin
    PConnection := Grt.ListItem[PConnections, I];

    if (Grt.DictString[PDriver, '_id'] =
        Grt.DictString[PConnection, 'driver']) then
    begin
      StoredConnComboBox.Items.AddObject(
        Grt.DictString[PConnection, 'name'],
        PConnection);
    end;
  end;

  StoredConnComboBox.Items.Add(_('<New Connection>'));
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.SetConnection(PConnection: Pointer);

var
  PParameterValues,
    PRdbmsList,
    PRdbms,
    PDrivers,
    PDriver: Pointer;
  I,
    J,
    Index: Integer;
  ParamMapping: PParamMapping;
  DriverId: WideString;

begin
  StoredConnDelBtn.Enabled := (PConnection <> nil);
  StoredConnAddBtn.Enabled := False;

  // if the last entry is selected <New Connection>, clear edits
  if (PConnection = nil) then
  begin
    for I := 0 to FParamMappingList.Count - 1 do
    begin
      ParamMapping := FParamMappingList[I];

      if (ParamMapping^.ParamControl is TTntEdit) then
        TTntEdit(ParamMapping^.ParamControl).Text :=
          Grt.DictString[ParamMapping^.PParam, 'defaultValue']
      else
        if (ParamMapping^.ParamControl is TTntCheckbox) then
          TTntCheckbox(ParamMapping^.ParamControl).Checked :=
            StrToIntDef(Grt.DictString[ParamMapping^.PParam, 'defaultValue'], 0) = 1;
    end;

    StoredConnComboBox.ItemIndex := -1;

    OKButton.Enabled := False;
  end
  else
  begin
    PRdbmsList := Grt.Global[FConnInfoPath + '/rdbms'];
    PDriver := Grt.DictRef[PConnection, 'driver'];
    DriverId := Grt.DictString[PDriver, '_id'];
    PRdbms := nil;

    PDriver := nil;
    for I := 0 to Grt.ListCount(PRdbmsList) - 1 do
    begin
      PRdbms := Grt.ListItem[PRdbmsList, I];
      PDrivers := Grt.DictItem[PRdbms, 'drivers'];

      for J := 0 to Grt.ListCount(PDrivers) - 1 do
      begin
        PDriver := Grt.ListItem[PDrivers, J];

        if (Grt.DictString[
          Grt.ListItem[PDrivers, J], '_id'] = DriverId) then
          break;

        PDriver := nil;
      end;

      if (PDriver <> nil) then
        break;

      PRdbms := nil;
    end;

    if (PDriver = nil) then
      raise Exception.Create(_('The driver used by the given ' +
        'connection is not available.'));

    // Select correct RDBMS
    Index := RDBMSComboBox.Items.IndexOfObject(PRdbms);
    if (Index > -1) then
    begin
      if (RDBMSComboBox.ItemIndex <> Index) then
      begin
        RDBMSComboBox.ItemIndex := Index;
        RDBMSComboBoxCloseUp(self);
      end;

      // Select correct Driver
      Index := DriverComboBox.Items.IndexOfObject(PDriver);
      if (Index > -1) then
      begin
        if (DriverComboBox.ItemIndex <> Index) then
        begin
          DriverComboBox.ItemIndex := Index;
          DriverComboBoxCloseUp(self);
        end;
      end
      else
        raise Exception.CreateFmt(_('The Driver %s is not '+
          'available for selection'),
          [Grt.DictString[PDriver, 'caption']]);
    end
    else
      raise Exception.CreateFmt(_('The RDBMS %s is not '+
        'available for selection'),
        [Grt.DictString[PRdbms, 'caption']]);

    PParameterValues := Grt.DictItem[PConnection, 'parameterValues'];

    for I := 0 to FParamMappingList.Count - 1 do
    begin
      ParamMapping := FParamMappingList[I];
      if (ParamMapping.ParamControl is TTntEdit) then
        TTntEdit(ParamMapping.ParamControl).Text :=
          Grt.DictString[PParameterValues,
            Grt.DictString[ParamMapping.PParam, 'name']]
      else
        if (ParamMapping.ParamControl is TTntCheckBox) then
          TTntCheckBox(ParamMapping.ParamControl).Checked :=
            (Grt.DictString[PParameterValues,
              Grt.DictString[ParamMapping.PParam, 'name']] = '1');
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.RdbmsComboBoxCloseUp(Sender: TObject);

begin
  if (RDBMSComboBox.ItemIndex > -1) then
  begin
    BuildDriverComboBoxItems(
      RDBMSComboBox.Items.Objects[RDBMSComboBox.ItemIndex]);

    DriverComboBox.ItemIndex := 0;
    DriverComboBoxCloseUp(self);
  end
  else
  begin
    DriverLbl.Enabled := False;
    DriverComboBox.Enabled := False;
    if (FDriverDescLbl <> nil) then
      FDriverDescLbl.Enabled := False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.SetDisplayRdbmsSelection(DisplayRdbmsSelection: Boolean);

begin
  ConnTypeMainPnl.Visible := DisplayRdbmsSelection;
  FDisplayRdbmsSelection := DisplayRdbmsSelection;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.SetSelectedRdbms(SelectedRdbms: WideString);

var
  I: Integer;

begin
  for I := 0 to RdbmsComboBox.Items.Count - 1 do
  begin
    if (WideSameText(
        Grt.DictString[RdbmsComboBox.Items.Objects[I], 'name'],
        SelectedRdbms)) then
    begin
      RdbmsComboBox.ItemIndex := I;
      RdbmsComboBoxCloseUp(self);
      break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TMyxConnectionDialogForm.GetSelectedRdbms: WideString;

begin
  Result := '';

  if (RdbmsComboBox.ItemIndex > -1) then
  begin
    Result := Grt.DictString[
      RdbmsComboBox.Items.Objects[RdbmsComboBox.ItemIndex],
      'name'];
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.StoredConnComboBoxCloseUp(Sender: TObject);

begin
  if (StoredConnComboBox.ItemIndex =
    StoredConnComboBox.Items.Count - 1) or
    (StoredConnComboBox.ItemIndex = -1) then
    SetConnection(nil)
  else
    SetConnection(StoredConnComboBox.Items.Objects[
      StoredConnComboBox.ItemIndex]);

  if (FFirstParamControl <> nil) then
    GetParentForm(FFirstParamControl).ActiveControl :=
      FFirstParamControl;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.AdvancedBtnClick(Sender: TObject);

begin
  AdvParamsMainPnl.Visible := Not(AdvParamsMainPnl.Visible);

  if (Not(FDisplayRdbmsSelection)) then
    ConnTypeMainPnl.Visible := AdvParamsMainPnl.Visible;

  BottomPnl.Top := 1000;

  if (AdvParamsMainPnl.Visible) then
    AdvancedBtn.Caption := _('<< Advanced')
  else
    AdvancedBtn.Caption := _('Advanced >>');
end;

// -----------------------------------------------------------------------------

function TMyxConnectionDialogForm.WriteConnectionToTarget: Pointer;

var
  I: Integer;
  PConnection,
    //PRdbms,
    PDriver,
    PParamValues: Pointer;
  ParamMapping: PParamMapping;

begin
  Result := nil;

  if ((RdbmsComboBox.ItemIndex = -1) or
      (DriverComboBox.ItemIndex = -1)) then
    Exit;

  //PRdbms := RdbmsComboBox.Items.Objects[RdbmsComboBox.ItemIndex];
  PDriver := DriverComboBox.Items.Objects[DriverComboBox.ItemIndex];

  // create new connection
  PConnection := Grt.ObjectNew('db.mgmt.Connection', '', '', '');

  // add a reference to the driver
  Grt.DictRef[PConnection, 'driver'] := PDriver;

  // create parameter dict
  PParamValues := Grt.DictNewTyped(GrtStringValue, '');
  Grt.DictItem[PConnection, 'parameterValues'] := PParamValues;

  // set values from controls
  for I := 0 to FParamMappingList.Count - 1 do
  begin
    ParamMapping := FParamMappingList[I];

    // set the new value
    if (ParamMapping.ParamControl is TTntEdit) then
      Grt.DictString[PParamValues,
        Grt.DictString[ParamMapping.PParam, 'name']] :=
          TTntEdit(ParamMapping.ParamControl).Text
    else
      if (ParamMapping.ParamControl is TTntCheckbox) then
        Grt.DictString[PParamValues,
          Grt.DictString[ParamMapping.PParam, 'name']] :=
            IntToStr(Ord(TTntCheckbox(ParamMapping.ParamControl).Checked));
  end;

  // create modules list
  Grt.DictItem[PConnection, 'modules'] :=
    Grt.ValueDuplicate(Grt.DictItem[PDriver, 'defaultModules']);

  Grt.Global[FConnTargetPath] := PConnection;

  Result := PConnection;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.OKBtnClick(Sender: TObject);

begin
  WriteConnectionToTarget;

  ModalResult := mrOK;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.CancelBtnClick(Sender: TObject);

begin
  ModalResult := mrCancel;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.StoredConnAddBtnClick(Sender: TObject);

var
  ConnectionName: WideString;
  PNewConnection,
    PConnections,
    PConnection: Pointer;
  I: Integer;

begin
  // if not the last item (<New Connection>) is selected
  if (StoredConnComboBox.ItemIndex <
    StoredConnComboBox.Items.Count - 1) then
    ConnectionName := StoredConnComboBox.Text;

  if (ShowModalEditDialog(_('Store Connection.'),
      _('Please enter a name for the connection.'),
      myx_mtInformation, _('Ok') + #13#10 + _('Cancel'),
      True, _('Name:'),
      ConnectionName) = 1) then
  begin
    WriteConnectionToTarget;

    PNewConnection := Grt.Global[FConnTargetPath];

    // set the connection's name
    Grt.DictString[PNewConnection, 'name'] := ConnectionName;

    // check if there already is a connection with this name stored
    PConnections := Grt.Global[FConnInfoPath + '/storedConns'];

    for I := 0 to Grt.ListCount(PConnections) - 1 do
    begin
      PConnection := Grt.ListItem[PConnections, I];

      // if so, delete it
      if (WideSameText(Grt.DictString[PConnection, 'driver'],
        Grt.DictString[PNewConnection, 'driver'])) and
        (WideSameText(Grt.DictString[PConnection, 'name'],
        ConnectionName)) then
      begin
        Grt.ListDel(PConnections, I);
        break;
      end;
    end;

    // Store connection
    PNewConnection := Grt.ValueDuplicate(PNewConnection);

    Grt.ListAdd(PConnections, PNewConnection);

    FillDropdownWithStoredConnections(StoredConnComboBox,
      DriverComboBox.Items.Objects[DriverComboBox.ItemIndex]);
    StoredConnComboBox.ItemIndex :=
      StoredConnComboBox.Items.Count - 2;
    StoredConnComboBoxCloseUp(self);
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.ClearFieldsBtnClick(Sender: TObject);

begin
  SetConnection(nil);

  if (FFirstParamControl <> nil) then
    FFirstParamControl.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.StoredConnDelBtnClick(Sender: TObject);

var
  PConnections,
    PConnection,
    PDelConnection: Pointer;
  I: Integer;

begin
  if (StoredConnComboBox.ItemIndex <> -1) then
  begin
    if (ShowModalDialog(_('Delete stored connection.'),
      _('Are you sure you want to delete the stored connection?'),
      myx_mtInformation, _('Yes') + #13#10 + _('No')) = 1) then
    begin
      PDelConnection := StoredConnComboBox.Items.Objects[
        StoredConnComboBox.ItemIndex];

      PConnections := Grt.Global[FConnInfoPath + '/storedConns'];

      for I := 0 to Grt.ListCount(PConnections) - 1 do
      begin
        PConnection := Grt.ListItem[PConnections, I];

        // if so, delete it
        if (WideSameText(Grt.DictString[PConnection, 'driver'],
          Grt.DictString[PDelConnection, 'driver'])) and
          (WideSameText(Grt.DictString[PConnection, 'name'],
          Grt.DictString[PDelConnection, 'name'])) then
        begin
          Grt.ListDel(PConnections, I);

          DriverComboBoxCloseUp(self);

          break;
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.LocateDriverBtnClick(Sender: TObject);

var
  OpenDlg: TOpenDialog;
  SourceDir,
    TargetDir: WideString;
  FilenameList: TTntStringList;
  I: Integer;
  PDriver,
    PFiles: Pointer;

begin
  if (DriverComboBox.ItemIndex = -1) then
    Exit;

  PDriver := DriverComboBox.Items.Objects[
    DriverComboBox.ItemIndex];

  PFiles := Grt.DictItem[PDriver, 'files'];

  TargetDir := GetApplDir + Tnt_WideStringReplace(
    Grt.DictString[PDriver, 'filesTarget'], '/', '\',
    [rfReplaceAll]);
  WideForceDirectories(TargetDir);

  FilenameList := TTntStringList.Create;
  try
    for I := 0 to Grt.ListCount(PFiles) - 1 do
      FilenameList.Add(Grt.ListString[PFiles, I]);

    OpenDlg := TOpenDialog.Create(self);
    try
      OpenDlg.Title := 'Attach Driver';
      while (FilenameList.Count > 0) do
      begin
        OpenDlg.Filter := _('Driver file')+
          ' (' + FilenameList[0] + ')|' + FilenameList[0] + '|'+
          _('All files')+' (*.*)|*.*';

        if (OpenDlg.Execute) then
        begin
          SourceDir := WideIncludeTrailingBackslash(
            WideExtractFilePath(OpenDlg.FileName));

          I := FilenameList.Count - 1;
          while (I >= 0) do
          begin
            if (FileExists(SourceDir + FilenameList[I])) then
            begin
              CopyDiskFile(SourceDir + FilenameList[I],
                TargetDir + FilenameList[I], False);

              FilenameList.Delete(I);
            end;

            dec(I);
          end;
        end
        else
          break;
      end;
    finally
      OpenDlg.Free;
    end;

    if (FilenameList.Count = 0) then
    begin
      if (ShowModalDialog(_('The application needs to be restarted.'),
        _('The application needs to be restarted. ' +
          'Please click on [Close] to close the application.'),
          myx_mtConfirmation, _('Close') + #13#10 + _('Cancel')) = 1) then
        Application.Terminate;
    end;
  finally
    FilenameList.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.DownloadDriverBtnClick(Sender: TObject);

var
  PDriver: Pointer;

begin
  if (DriverComboBox.ItemIndex = -1) then
    Exit;

  PDriver := DriverComboBox.Items.Objects[
    DriverComboBox.ItemIndex];

  BrowseWebPage(Grt.DictString[PDriver, 'downloadUrl']);
end;

// -----------------------------------------------------------------------------

function TMyxConnectionDialogForm.GetParamValue(const Name: WideString): WideString;

var
  I: Integer;
  ParamMapping: PParamMapping;

begin
  for I := 0 to FParamMappingList.Count - 1 do
  begin
    ParamMapping := FParamMappingList[I];

    if (WideSameText(Grt.DictString[ParamMapping^.PParam, 'name'],
      name)) then
    begin
      if (ParamMapping^.ParamControl is TTntEdit) then
        Result := TTntEdit(ParamMapping^.ParamControl).Text
      else
        if (ParamMapping^.ParamControl is TTntCheckbox) then
          Result := IntToStr(Ord(TTntCheckbox(ParamMapping^.ParamControl).Checked));

      break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.SetParamValue(const Name: WideString; Value: WideString);

var
  I: Integer;
  ParamMapping: PParamMapping;

begin
  for I := 0 to FParamMappingList.Count - 1 do
  begin
    ParamMapping := FParamMappingList[I];

    if (WideSameText(Grt.DictString[ParamMapping^.PParam, 'name'],
      name)) then
    begin
      if (ParamMapping^.ParamControl is TTntEdit) then
      begin
        TTntEdit(ParamMapping^.ParamControl).Text := Value;
        TTntEdit(ParamMapping^.ParamControl).SelStart :=
          Length(TTntEdit(ParamMapping^.ParamControl).Text);
      end
      else
        if (ParamMapping^.ParamControl is TTntCheckbox) then
          TTntCheckbox(ParamMapping^.ParamControl).Checked := (Value = '1');

      break;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMyxConnectionDialogForm.SetDisplayDescriptions(DisplayDescriptions: Boolean);

begin
  FDisplayDescriptions := DisplayDescriptions;

  if (FDisplayDescriptions) and (FRdbmsDescLbl = nil) then
  begin
    FRdbmsDescLbl := TTntLabel.Create(self);
    FRdbmsDescLbl.Caption := _('Select a RDBMS from the list of supported systems');
    FRdbmsDescLbl.Top := RdbmsLbl.Top;
    FRdbmsDescLbl.Left := RdbmsComboBox.Left +
      RdbmsComboBox.Width + 20;
    FRdbmsDescLbl.Parent := ConnTypePnl;

    FDriverDescLbl := TTntLabel.Create(self);
    FDriverDescLbl.Caption := _('Choose from the list of available ' +
      'drivers for this RDBMS');
    FDriverDescLbl.Top := DriverLbl.Top;
    FDriverDescLbl.Left := RdbmsComboBox.Left +
      RdbmsComboBox.Width + 20;
    FDriverDescLbl.Parent := ConnTypePnl;
  end
  else
    if (FDisplayDescriptions) and (FRdbmsDescLbl <> nil) then
    begin
      FRdbmsDescLbl.Free;
      FRdbmsDescLbl := nil;
      FDriverDescLbl.Free;
      FDriverDescLbl := nil;
    end;
end;

// -----------------------------------------------------------------------------

end.
