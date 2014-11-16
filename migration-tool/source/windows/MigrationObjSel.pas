unit MigrationObjSel;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_util_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees,
  WizardInterface, WizardPage,
  MigrationObjSelFilter, PngTools, Grt;

type
  TMigrationObjSelForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    ObjTypeSelTabSheet: TTntTabSheet;
    SchemataTabSheet: TTntTabSheet;
    SchemataGBox: TTntGroupBox;
    RevEngTabSheet: TTntTabSheet;
    SchemaListView: TTntListView;
    SchemaListViewImgList: TImageList;
    TntLabel9: TTntLabel;
    SchemaSearchEd: TAdvancedEditFrame;
    TntGroupBox6: TTntGroupBox;
    BuildObjSelImg: TTntImage;
    TntLabel10: TTntLabel;
    TntLabel11: TTntLabel;
    GetMigMethodsImg: TTntImage;
    ObjectsLbl: TTntLabel;
    TntLabel15: TTntLabel;
    RevEngImg: TTntImage;
    TntLabel16: TTntLabel;
    MessageLogGBox: TTntGroupBox;
    GRTMessageMemo: TTntMemo;
    ResultLbl: TTntLabel;
    SmallSchemaListViewImgList: TImageList;
    SchemaSelectionCountLbl: TTntLabel;
    ShowSchemataAsListCBox: TTntCheckBox;
    ObjSelScrollBox: TTntScrollBox;
    ObjTypeSelSpacerPnl: TTntPanel;
    GrtMessageLbl: TTntLabel;
    GrtProgressBar: TTntProgressBar;
    GrtProgressLbl: TTntLabel;
    AdvOptsGBox: TTntGroupBox;
    OnlyTablesCBox: TTntCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DisplaySchemataSheet;
    procedure UpdateSchemaSelectionCountLbl;
    procedure SchemaListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure SchemaListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ShowSchemataAsListCBoxClick(Sender: TObject);

    procedure DoReverseEngineer;
    procedure DoReverseEngineerError(
      msg: WideString; TaskImg: TTntImage);
    procedure DockPnlResize(Sender: TObject);

    procedure DoBuildObjectSelection;

    procedure ProcessGrtOutput(S: WideString);
    procedure ProcessGrtMessages(Messages: TMYX_GRT_MSGS);
    function ProcessGrtStatusQuery: Integer;

    procedure AdvancedEditFrame1SearchEdChange(Sender: TObject);
  private
    { Private declarations }
    RevEngSchemaNameList: TMYX_STRINGLIST;

    FilterFrames: TList;
  public
    { Public declarations }
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

    procedure RefreshSchemaListView(SearchStr: WideString = '');
  end;

var
  MigrationObjSelForm: TMigrationObjSelForm;

implementation

uses Main;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel := DockPnl;

  MainPngControl.Align := alNone;
  MainPngControl.Left := -4;
  MainPngControl.Top := -27;
  MainPngControl.Width := 765+4+4;
  MainPngControl.Height := 529+274;
  MainPngControl.ActivePageIndex := 0;

  TaskUncheckedPNGImg := LoadPNGImageFromResource('task_unchecked');
  TaskCheckedPNGImg := LoadPNGImageFromResource('task_checked');
  TaskErrorPNGImg := LoadPNGImageFromResource('task_error');
  TaskDisabledPNGImg := LoadPNGImageFromResource('task_disabled');

  SchemataGBox.Height := 399 + 70;

  RevEngSchemaNameList := nil;

  FilterFrames := TList.Create;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.FormDestroy(Sender: TObject);

begin
  FilterFrames.Free;
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
    DisplaySchemataSheet
  else
    if (NewSubPageIndex=1) then
    begin
      Grt.GlobalAsInt['/migration/applicationData/' +
        'reverseEngineerOnlyTableObjects'] := Ord(OnlyTablesCBox.Checked);

      DoReverseEngineer;

      // Set script checkpoint
      Grt.GlobalAsInt['/migration/applicationData/' +
        'ScriptGenerationCheckpoint'] := 2;
    end
    else
      if (NewSubPageIndex=2) then
        DoBuildObjectSelection;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Source Schemata Selection');
    1:
      Result:=_('Reverse Engineering');
    2:
      Result:=_('Object Type Selection');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Choose the schemata you want to migrate.');
    1:
      Result:=_('Reverse engineering the source database.');
    2:
      Result:=_('Select all object types that have to be migrated.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetSupportAdvancedOptions: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=True;
    1:
      Result:=True;
    2:
      Result:=False;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  case MainPngControl.ActivePageIndex of
    0:
      begin
        AdvOptsGBox.Visible := State;
        SchemataGBox.Height := 399 + 70 * Ord(Not(State));
      end;
    1:
      MessageLogGBox.Visible:=State;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.GetAdvancedOptionsState: Boolean;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result := AdvOptsGBox.Visible;
    1:
      Result:=MessageLogGBox.Visible;
  else
    Result:=False;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.BeforeSubPageIndexChange(SectionIndex: Integer);

var
  i: integer;

begin
  //Before the user switches way from Source Schemata Selection
  if(SectionIndex=0)then
  begin
    if(RevEngSchemaNameList<>nil)then
      RevEngSchemaNameList.Free;

    RevEngSchemaNameList:=TMYX_STRINGLIST.Create;

    for i:=0 to SchemaListView.Items.Count-1 do
      if(SchemaListView.Items[i].Selected)then
        RevEngSchemaNameList.strings.Add(
          SchemaListView.Items[i].Caption)
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;


// -----------------------------------------------------------------------------
// Schemata page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.DisplaySchemataSheet;

begin
  FWizardInterface.NextBtn.Enabled := False;

  RefreshSchemaListView;

  MainPngControl.ActivePageIndex:=0;
  try
    if(SchemaListView.CanFocus)then
      SchemaListView.SetFocus;
  except
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.RefreshSchemaListView(SearchStr: WideString);

var
  I: integer;
  ListItem: TTntListItem;
  PSourceSchemataNames: Pointer;
  S: WideString;

begin
  SchemaListView.Items.Clear;

  PSourceSchemataNames := Grt.Global['/migration/sourceSchemataNames'];

  if (PSourceSchemataNames <> nil) then
  begin
    for I:=0 to myx_grt_list_item_count(PSourceSchemataNames) - 1 do
    begin
      S := myx_grt_list_item_get_as_string(PSourceSchemataNames, I);

      if (SearchStr = '') or
        (myx_match_pattern(S, SearchStr + '*', 0, 0) = 1) then
      begin
        ListItem := SchemaListView.Items.Add;
        ListItem.Caption := S;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.UpdateSchemaSelectionCountLbl;

begin
  if (SchemaListView.SelCount=0) then
  begin
    SchemaSelectionCountLbl.Caption :=
      _('No schemata selected.');

    FWizardInterface.NextBtn.Enabled := False;
  end
  else
  begin
    if(SchemaListView.SelCount=1)then
      SchemaSelectionCountLbl.Caption :=
        _('1 schema selected.')
    else
      SchemaSelectionCountLbl.Caption :=
        Format(_('%d schemata selected.'), [SchemaListView.SelCount]);

    FWizardInterface.NextBtn.Enabled := True;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.SchemaListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);

begin
  UpdateSchemaSelectionCountLbl;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.SchemaListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);

begin
  UpdateSchemaSelectionCountLbl;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.ShowSchemataAsListCBoxClick(
  Sender: TObject);

begin
  if (ShowSchemataAsListCBox.Checked) then
    SchemaListView.ViewStyle:=vsList
  else
    SchemaListView.ViewStyle:=vsIcon;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.DoReverseEngineer;

var
  SelectedSchemata: TMYX_STRINGLIST;
  I: Integer;
  PSchemata: Pointer;
  ModuleName: WideString;
  Task: IGrtTask;

begin
  RevEngImg.Picture.Assign(TaskUncheckedPNGImg);
  BuildObjSelImg.Picture.Assign(TaskUncheckedPNGImg);
  GetMigMethodsImg.Picture.Assign(TaskUncheckedPNGImg);

  // initialize progress widgets
  ResultLbl.Caption := _('Executing ...');
  ResultLbl.Show;
  ResultLbl.Font.Color := clBlack;
  GrtMessageLbl.Caption := '';
  GrtMessageLbl.Show;
  GRTMessageMemo.Text := '';
  GrtProgressBar.Visible := False;
  GrtProgressLbl.Visible := False;

  MainPngControl.ActivePageIndex:=1;

  MainForm.EnableDisableBottomBtns;

  Application.ProcessMessages;

  DoProcessStart;
  try
    //Do reverse engineering
    SelectedSchemata := TMYX_STRINGLIST.Create;
    try
      // Collect all schemata the user selected
      for I:=0 to SchemaListView.Items.Count-1 do
        if (SchemaListView.Items[I].Selected) then
          SelectedSchemata.strings.Add(SchemaListView.Items[I].Caption);

      PSchemata := myx_grt_list_from_stringlist(
        SelectedSchemata.get_record_pointer);

      Grt.Global['/migration/selectedSchemataNames'] := PSchemata;

      try
        ModuleName := Grt.DictString[
            Grt.DictItem[
              Grt.Global['/migration/sourceConnection'], 'modules'],
          'ReverseEngineeringModule'];

        //Call the function
        Task := Grt.CreateStandardTask(_('Starting reverse engineering'), 
            ModuleName,
            'reverseEngineer',
            [Grt.GetGlobalAsParam('/migration/sourceConnection'),
              PSchemata],
            ProcessGrtOutput,
            ProcessGrtMessages,
            True,
            True,
            -1,
            nil,
            ProcessGrtStatusQuery);
        Grt.AddTaskAndWaitWithMessages(Task, True);
        Grt.Global['/migration/sourceCatalog'] := Task.Result;
        Grt.ValueRelease(Task.Result);
      except
        on x: EGrtError do
        begin
          DoReverseEngineerError(Format(
              _('The schema could not be reverse engineered '+
                '(error: %d).'+#13#10+
                '%s'),
              [x.ErrorNumber, x.Description]),
            RevEngImg);

          Exit;
        end;
      end;
    finally
      SelectedSchemata.Free;
    end;

    RevEngImg.Picture.Assign(TaskCheckedPNGImg);
    Application.ProcessMessages;

    //Check if there is at lease one schema
    {PSchemata := myx_grt_dict_item_get_value(Mig.SourceCatalog,
      'schemata');}
    PSchemata := Grt.Global['/migration/sourceCatalog/schemata'];

    if (myx_grt_list_item_count(PSchemata)=0) then
    begin
      DoReverseEngineerError('No schematas have been reverse engineered.',
        BuildObjSelImg);

      Exit;
    end;


    BuildObjSelImg.Picture.Assign(TaskCheckedPNGImg);
    Application.ProcessMessages;

    try
      ModuleName := Grt.DictString[
          Grt.DictItem[
            Grt.Global['/migration/sourceConnection'], 'modules'],
        'MigrationModule'];

      Task := Grt.CreateStandardTask(
        _('Getting migration methods list'),
        ModuleName,
        'migrationMethods', [],
        ProcessGrtOutput,
        ProcessGrtMessages
      );
      Grt.AddTaskAndWaitWithMessages(Task, True);
      Grt.Global['/migration/migrationMethods'] := Task.Result;
      Grt.ValueRelease(Task.Result);
    except
      on x: EGrtError do
      begin
        DoReverseEngineerError(Format(
            _('The list of available migration methods could not be created '+
              '(error: %d).'+#13#10+
              '%s'),
            [x.ErrorNumber, x.Description]),
          RevEngImg);

        Exit;
      end;
    end;

    GetMigMethodsImg.Picture.Assign(TaskCheckedPNGImg);
    Application.ProcessMessages;

    if (FWizardInterface.OperationCanceled) then
      ResultLbl.Caption := _('Canceled by user.')
    else
      ResultLbl.Caption := _('Execution completed successfully.');

    GrtMessageLbl.Hide;
    GrtProgressBar.Hide;
    GrtProgressLbl.Hide;
  finally
    DoProcessEnd;
  end;

  FWizardInterface.NextBtn.SetFocus;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.ProcessGrtOutput(S: WideString);

begin
  GRTMessageMemo.Text := GRTMessageMemo.Text + S;
  GRTMessageMemo.Update;
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.ProcessGrtMessages(
  Messages: TMYX_GRT_MSGS);

var
  Msg: TMYX_GRT_MSG;

begin
  // display last message in ResultLbl
  if (Messages.msgs.Count > 0) then
  begin
    Msg := Messages.msgs[Messages.msgs.Count - 1];

    if (Msg.msg_type = 2) then
    begin
      GrtProgressLbl.Caption := Msg.msg;
      GrtProgressLbl.Update;

      if (Msg.progress = -1) then
      begin
        GrtProgressBar.Hide;
        GrtProgressLbl.Hide;
      end
      else
      begin
        GrtProgressBar.Position := Msg.progress;

        if (Not(GrtProgressBar.Visible)) then
        begin
          GrtProgressBar.Show;
          GrtProgressLbl.Show;
        end;
      end;
    end
    else
    begin
      GrtMessageLbl.Caption := Msg.msg;
      GrtMessageLbl.Update;
    end;

    GRTMessageMemo.Text := GRTMessageMemo.Text +
      FormatGrtMessagesAsString(Messages);
    GRTMessageMemo.Refresh;
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationObjSelForm.ProcessGrtStatusQuery: Integer;

begin
  Result := Ord(FWizardInterface.OperationCanceled);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.DoReverseEngineerError(
  msg: WideString; TaskImg: TTntImage);

begin
  ResultLbl.Caption:=_('An Error has occured. Press [Advanced >>] '+
    'to see the log.');
  ResultLbl.Show;

  if(TaskImg<>nil)then
    TaskImg.Picture.Assign(TaskErrorPNGImg);

  GRTMessageMemo.Lines.Add(msg);

  GrtProgressBar.Visible := False;
  GrtProgressLbl.Visible := False;

  //Stop animation
  if(Assigned(OnProcessEnd))then
    OnProcessEnd(self);
end;

// -----------------------------------------------------------------------------

procedure TMigrationObjSelForm.DoBuildObjectSelection;

var
  I, J, K, C: Integer;
  PSchemata, PSchema: Pointer;
  PSchemaStruct, PSchemaStructMember: Pointer;
  PSchemaItemList,
    PSchemaItem: Pointer;
  MemberName,
    ContentStructName: WideString;
  FilterFrame: TMigrationObjSelFilterFrame;
  PSourceObjects: Pointer;

begin
  //Clear Filter Frames if there are some already
  for I:=0 to FilterFrames.Count-1 do
    TMigrationObjSelFilterFrame(FilterFrames[I]).Free;
  FilterFrames.Clear;

  PSchemata := Grt.Global['/migration/sourceCatalog/schemata'];

  //Get the struct type of the schemata
  PSchema := myx_grt_list_item_get(PSchemata, 0);
  PSchemaStruct := myx_grt_dict_struct_get(Grt.NativeGrt, PSchema);

  //Clear the Mig.SourceObjects list
  PSourceObjects := Grt.Global['/migration/sourceObjects'];
  myx_grt_list_clear(PSourceObjects);

  //Create a filter frame for each member deriving from db.DatabaseObject
  for I:=myx_grt_struct_get_member_count_total(Grt.NativeGrt, PSchemaStruct)-1 downto 0 do
  begin
    PSchemaStructMember :=
      myx_grt_struct_get_member_by_index_total(Grt.NativeGrt, PSchemaStruct, I);

    // if the member is not a list, continue
    if (Grt.GetStructMemberType(PSchemaStructMember) <> GrtListValue) then
      continue;

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

      //Create FilterFrame
      if (C > 0) then
      begin
        FilterFrame := TMigrationObjSelFilterFrame.Create(self, Grt);
        FilterFrames.Add(FilterFrame);
        FilterFrame.Name := 'FilterFrame' + IntToStr(FilterFrames.Count);
        FilterFrame.Parent := ObjSelScrollBox;

        FilterFrame.StructName := ContentStructName;
      end;
    end;
  end;

  for I:=0 to FilterFrames.Count-1 do
  begin
    FilterFrame := TMigrationObjSelFilterFrame(FilterFrames[I]);

    if (myx_grt_struct_is_or_inherits_from(Grt.NativeGrt,
      FilterFrame.StructName, 'db.Table') <> 1) then
    begin
      FilterFrame.MigrateCBox.Checked := False;
      FilterFrame.MigrateCBoxClick(FilterFrame.MigrateCBox);
    end
    else
      FilterFrame.RefreshLists;
  end;

  MainPngControl.ActivePageIndex:=2;

  FWizardInterface.NextBtn.Enabled := True;
end;

procedure TMigrationObjSelForm.AdvancedEditFrame1SearchEdChange(
  Sender: TObject);
begin
  SchemaSearchEd.SearchEdChange(Sender);

  RefreshSchemaListView(SchemaSearchEd.SearchEd.Text);
end;

end.
