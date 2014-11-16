unit AdminStartupVariables;

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, StrUtils,
  InstanceSections, PNGImage, AuxFuncs, TntExtCtrls,
  ApplicationDataModule, myx_admin_public_interface,
  Contnrs, Registry, WinSvc, AdminService,
  AdminStartupVariablesInnoDBDatafiles, Menus, Options, Sections,
  AdminStartupVariablesOptionFile, MyxError, TntComCtrls,
  TntStdCtrls, TntForms, TntMenus, TntClasses,
  myx_public_interface, myx_util_public_interface;

type
  TGUIWidgetCollection = class(TObject)
    constructor Create(widget: TMYX_GUI_WIDGET);
    destructor Destroy; override;

    procedure SetMainControl(Control: TControl);
  public
    id: WideString;
    widget: TMYX_GUI_WIDGET;

    prev_page_index: Integer;
    prev_group_index: Integer;
    prev_widget_index: Integer;

    ControlList: TList;
    MainControl: TControl;
    ActivateControl: TControl;

    Data: TObject;
  end;

  TAdminStartupVariablesForm = class(TInstanceSectionForm)
    StartupParametersPnl: TTntPanel;
    StartupParamsPageControl: TTntPageControl;
    BottomPanel: TTntPanel;
    SheetHeaderPnl: TTntPanel;
    ServiceSettingsHeaderImg: TTntImage;
    SheetHeaderLbl: TTntLabel;
    ServiceSettingsBevel: TTntBevel;
    Label1: TTntLabel;
    SubTreePnl: TTntPanel;
    ServicesTreeView: TTntTreeView;
    Panel2: TTntPanel;
    Label15: TTntLabel;
    LocalhostOnlyPnl: TTntPanel;
    AvailabilityLabel: TTntLabel;
    ApplyChangesBtn: TTntButton;
    DiscardChangesBtn: TTntButton;
    OptionFileCaptionLabel: TTntLabel;
    SectionLbl: TTntLabel;
    SectionCaptionLbl: TTntLabel;
    ChooseCnfFileBtn: TTntButton;
    OptionFileLabel: TTntLabel;
    ButtonPanel: TPanel;
    LabelPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    function AddDescriptionLabel(GroupBox: TTntGroupBox; Name: WideString; Description: WideString; Top: Integer;
      Left: Integer = 242; Width: Integer = 367): TTntLabel;
    function AddCaptionLabel(GroupBox: TTntGroupBox; Name: WideString; Description: WideString; Top: Integer;
      Hint: WideString): TTntLabel;
    function ReplaceTokensInValue(value: WideString): WideString;

    procedure BuildStartupParamPages;
    procedure EnableDisableGUIWidgetCollection(
      GUIWidgetCollection: TGUIWidgetCollection; State: Boolean);
    procedure SetInnoDBFilePathValues(
      GUIWidgetCollection: TGUIWidgetCollection);

    procedure DoWidgetChange(Sender: TObject);
    procedure AddInnoDBTablespace(Sender: TObject);
    procedure RemoveInnoDBTablespace(Sender: TObject);
    procedure ActiveImgMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChangeWidgetActivationState(GUIWidgetCollection: TGUIWidgetCollection;
      State: Boolean);
    function ScanGUIWidgetCollList4ActivateControl(
      ActivateControl: TTntImage): TGUIWidgetCollection;

    procedure DoFileWidgetChange(Sender: TObject);
    procedure DoDirWidgetChange(Sender: TObject);

    procedure ValidateFileName(Sender: TObject);

    procedure DoPageContentChanged(Sender: TObject);
    procedure ApplyChangesBtnClick(Sender: TObject);
    procedure DiscardChangesBtnClick(Sender: TObject);

    procedure ApplyChanges;
    procedure DiscardChanges;
    procedure StartupParamsPageControlChange(Sender: TObject);

    procedure RefreshStartupParametersFromCnfFile;
    procedure ReassignGUIWidgetCollections;
    procedure RefreshStartupParametersControls;

    procedure RefreshServiceList;
    procedure SetCurrentService(MySQLService: TMySQLService);
    procedure ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ParamSectionNameEdChange(Sender: TObject);
    procedure ChooseCnfFileBtnClick(Sender: TObject);

    procedure UpdateConfigFileLabels;
    procedure ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure MerlinInfo1BtnClick(Sender: TObject);
  private
    FServerParameterPNGImg: TPNGObject;
    FEditEnabledPNGImg,
    FEditDisabledPNGImg: TPNGObject;
    FGuiDesc: TMYX_GUI_DESCRIPTION;
    FGUIWidgetCollectionList: TObjectList;
    FPageContentChanged: Boolean;
    FShowConfigFileGBox: Boolean;
    FCurrentSection: WideString;
    procedure ChangeCurrentService(ServiceName: WideString = '');
    procedure CreateInnoDBFilepathListview(GroupBox: TTntGroupBox; var current_widget_ypos: Integer; var widget: TMYX_GUI_WIDGET;
      WidgetNamePostfix: WideString; GUIWidgetCollection: TGUIWidgetCollection);
    procedure CreateReplicationDoDBListbox(value: Widestring; GroupBox: TTntGroupBox; var current_widget_ypos: Integer; var widget: TMYX_GUI_WIDGET;
      WidgetNamePostfix: WideString; GUIWidgetCollection: TGUIWidgetCollection);
  protected
    procedure ActivateSection; override;
  end;

const
  GroupBoxWidth = 623;
  TablespaceImg = 9;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  Main, PNGTools, gnugettext;

//----------------------------------------------------------------------------------------------------------------------

function ComputeHeight(const Canvas: TCanvas; const S: WideString; const AvailableWidth: Integer): Integer;

var
  R: TRect;

begin
  R := Rect(0, 0, AvailableWidth, 0);
  Windows.DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_CALCRECT or DT_WORDBREAK);
  Result := R.Bottom - R.Top;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TGUIWidgetCollection.Create(widget: TMYX_GUI_WIDGET);

begin
  inherited Create;

  self.widget := widget;
  id := widget.id;
  MainControl := nil;
  ActivateControl := nil;
  Data := nil;

  ControlList := TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TGUIWidgetCollection.Destroy;

begin
  ControlList.Free;

  if (Assigned(Data)) then
    Data.Free;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGUIWidgetCollection.SetMainControl(Control: TControl);

begin
  MainControl := Control;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  DockedPanel := StartupParametersPnl;
  SubTreePanel := SubTreePnl;

  FGUIWidgetCollectionList := TObjectList.Create;

  InitControls := False;
  FGuiDesc := nil;

  FPageContentChanged := False;

  FShowConfigFileGBox := False;

  FServerParameterPNGImg := LoadPNGImageFromResource('server_parameter', ServiceSettingsHeaderImg);

  FEditEnabledPNGImg := LoadPNGImageFromResource('edit_enabled');
  FEditDisabledPNGImg := LoadPNGImageFromResource('edit_disabled');

  FCurrentSection := '';
  SectionLbl.Caption := 'No section selected';

  // Get all MySQL Services.
  InitControls := True;
  try
    if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
    begin
      RefreshServiceList;
      BuildStartupParamPages;

      StartupParamsPageControlChange(self);
    end
    else
    begin
      DisableEnableControls(BottomPanel, False);
      LocalhostOnlyPnl.Visible := True;
      StartupParamsPageControl.Visible := False;
    end;

    if ApplicationDM.Options.ShowOnlyServiceSections then
    begin
      SectionCaptionLbl.Visible := True;
      SectionLbl.Visible := True;
      ChooseCnfFileBtn.Visible := True;
    end;
  finally
    InitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.FormDestroy(Sender: TObject);

begin
  FServerParameterPNGImg.Free;

  FEditEnabledPNGImg.Free;
  FEditDisabledPNGImg.Free;

  FGUIWidgetCollectionList.Free;

  FGuiDesc.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.MerlinInfo1BtnClick(Sender: TObject);

begin
  BrowseWebPage('http://mysql.com/mashowcase');
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminStartupVariablesForm.AddDescriptionLabel(GroupBox: TTntGroupBox; Name: WideString; Description: WideString;
  Top: Integer; Left: Integer; Width: Integer): TTntLabel;

var
  Lbl: TTntLabel;

begin
  Lbl := TTntLabel.Create(GroupBox);
  Lbl.Parent := GroupBox;
  Lbl.Name := Name;
  Lbl.WordWrap := True;
  Lbl.Autosize := False;
  Lbl.Left := Left;
  Lbl.Top := Top;
  //Lbl.Width := Width;
  Lbl.Width := GroupBox.Width - Left;
  Lbl.Anchors := [akLeft, akTop, akRight];
  Lbl.Height := ComputeHeight(Canvas, Description, Width);
  Lbl.Caption := _(Description);

  Result := Lbl;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminStartupVariablesForm.AddCaptionLabel(GroupBox: TTntGroupBox; Name: WideString; Description: WideString;
  Top: Integer; Hint: WideString): TTntLabel;

var
  Lbl: TTntLabel;

begin
  Lbl := TTntLabel.Create(GroupBox);
  Lbl.Parent := GroupBox;
  Lbl.Name := Name;
  Lbl.WordWrap := True;
  Lbl.Autosize := False;
  Lbl.Left := 14 + 16;
  Lbl.Top := Top;
  Lbl.Width := 100;
  Lbl.Height := ComputeHeight(Canvas, Description, Width);
  Lbl.Caption := _(Description);

  if (Hint <> '') then
  begin
    Lbl.Hint := _('MySQL variable name: ') + Hint;
    Lbl.ShowHint := True;
  end;

  Result := Lbl;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.BuildStartupParamPages;

var
  pgui_desc: PMYX_GUI_DESCRIPTION;
  ErrorCode: MYX_ADMIN_LIB_ERROR;
  PageTabSheet: TTabSheet;
  ScrollBox: TTntScrollBox;
  Page: TMYX_GUI_PAGE;
  Group: TMYX_GUI_GROUP;
  GroupBox: TTntGroupBox;
  current_group_ypos, current_group_height: Integer;
  widget: TMYX_GUI_WIDGET;
  current_widget_ypos: Integer;
  ActiveImg: TTntImage;
  CheckBox: TTntCheckBox;
  Lbl: TTntLabel;
  Edit: TTntEdit;
  ComboBox: TTntComboBox;
  i, j, k, l: Integer;
  WidgetNamePostfix: WideString;
  Value: WideString;
  GUIWidgetCollection: TGUIWidgetCollection;

begin
  pgui_desc := nil;
  if Assigned(ApplicationDM.CurrentService) then
    if FileExists(ApplicationDM.CurrentService.ConfigFile) then
      pgui_desc := myx_get_gui_description(
        MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml',
        ApplicationDM.Options.MySQLVersion, MYX_WINDOWS, @ErrorCode,
        ApplicationDM.CurrentService.ConfigFile, FCurrentSection)
    else
      pgui_desc := myx_get_gui_description(
        MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml',
        ApplicationDM.Options.MySQLVersion,
        MYX_WINDOWS,
        @ErrorCode,
        '', '');

  if (ErrorCode <> MYX_ADMIN_NO_ERROR) then
    raise EMyxError.Create(WideFormat(_('Error while loading the Startup Parameter descriptions from %s') +
      'mysqladmin_startup_variables_description.xml' + #13#10#13#10 +
      _('Error no.: %d'), [MYXCommonOptions.XMLDir, Ord(ErrorCode)]));

  try
    FGuiDesc := TMYX_GUI_DESCRIPTION.Create(pgui_desc);
  finally
    myx_free_gui_description(pgui_desc);
  end;

  FGUIWidgetCollectionList.Clear;

  for i := 0 to FGuiDesc.pages.Count - 1 do
  begin
    Page := FGuiDesc.pages[i];

    PageTabSheet := TTabSheet.Create(self);
    PageTabSheet.Name := 'StartupParamsPage' + IntToStr(i);
    PageTabSheet.Caption := _(Page.caption);
    PageTabSheet.TabVisible := False;
    PageTabSheet.PageControl := StartupParamsPageControl;

    ScrollBox := TTntScrollBox.Create(PageTabSheet);
    ScrollBox.Parent := PageTabSheet;
    ScrollBox.Name := 'SB' + IntToStr(i);
    ScrollBox.Align := alClient;
    ScrollBox.BorderStyle := bsNone;
    if (MYXCommonOptions.XPStyleEnabled) then
      ScrollBox.Color := clWhite;
    ScrollBox.VertScrollBar.Smooth := True;
    ScrollBox.VertScrollBar.Tracking := True;
    ScrollBox.AutoScroll := False;
    ScrollBox.Padding.Left := 10;
    ScrollBox.Padding.Right := 10;
    ScrollBox.Padding.Top := 10;
    ScrollBox.Padding.Bottom := 10;

    current_group_ypos := 8;

    for j := 0 to Page.groups.Count - 1 do
    begin
      Group := Page.groups[j];

      current_group_height := 25;

      GroupBox := TTntGroupBox.Create(ScrollBox);
      GroupBox.Parent := ScrollBox;
      GroupBox.Name := 'GBox' + IntToStr(i) + '_' + IntToStr(j);
      GroupBox.Caption := _(Group.caption);
      GroupBox.Left := 12;
      GroupBox.Top := current_group_ypos;
      GroupBox.Align := alTop;

      current_widget_ypos := 24;

      for k := 0 to Group.widgets.Count - 1 do
      begin
        widget := Group.widgets[k];
        WidgetNamePostfix := IntToStr(i) + '_' + IntToStr(j) + '_' + IntToStr(k);

        GUIWidgetCollection := TGUIWidgetCollection.Create(widget);
        FGUIWidgetCollectionList.Add(GUIWidgetCollection);

        GUIWidgetCollection.prev_page_index := i;
        GUIWidgetCollection.prev_group_index := j;
        GUIWidgetCollection.prev_widget_index := k;

        if (widget.value = '') then
          Value := ReplaceTokensInValue(widget.default_value)
        else
          Value := ReplaceTokensInValue(widget.value);

        //Create Activate Image for all widgets except checkboxes
        if (widget.widget_type <> MYX_CHECKBOX) then
        begin
          ActiveImg := TTntImage.Create(GroupBox);
          ActiveImg.Parent := GroupBox;
          ActiveImg.Name := 'ActiveImg' + WidgetNamePostfix;
          ActiveImg.Left := 14;
          ActiveImg.Top := current_widget_ypos;
          ActiveImg.AutoSize := True;
          if (widget.active = 1) then
            ActiveImg.Picture.Assign(FEditEnabledPNGImg)
          else
            ActiveImg.Picture.Assign(FEditDisabledPNGImg);

          GUIWidgetCollection.ActivateControl := ActiveImg;
          //Store index for faster access
          ActiveImg.Tag := k;

          ActiveImg.OnMouseDown := ActiveImgMouseDown;

          ActiveImg.Hint := _('If enabled, the option will be written to the option file');
          ActiveImg.ShowHint := True;
        end;

        //Create control(s)
        case widget.widget_type of
          MYX_CHECKBOX:
            begin
              CheckBox := TTntCheckBox.Create(GroupBox);
              CheckBox.Parent := GroupBox;
              CheckBox.Name := 'CBox' + WidgetNamePostfix;
              CheckBox.Caption := _(widget.caption);
              CheckBox.Left := 14;
              CheckBox.Top := current_widget_ypos - 2;
              CheckBox.Width := 201;
              CheckBox.OnClick := DoWidgetChange;
              CheckBox.Checked := SameText(Value, 'Checked');

              CheckBox.Hint := _('MySQL variable name: ') + widget.id;
              CheckBox.ShowHint := True;

              GUIWidgetCollection.ControlList.Add(CheckBox);
              GUIWidgetCollection.SetMainControl(CheckBox);

              Lbl := AddDescriptionLabel(GroupBox, 'CBoxDesc' + WidgetNamePostfix, widget.description,
                current_widget_ypos);
              GUIWidgetCollection.ControlList.Add(Lbl);

              Inc(current_widget_ypos, Lbl.Height + 16);
            end;
          MYX_SPINEDIT:
            begin
              Lbl := AddCaptionLabel(GroupBox, 'SEdLbl' + WidgetNamePostfix, widget.caption, current_widget_ypos,
                widget.id);
              GUIWidgetCollection.ControlList.Add(Lbl);

              Edit := TTntEdit.Create(GroupBox);
              Edit.Parent := GroupBox;
              Edit.Name := 'SEd' + WidgetNamePostfix;
              Edit.Left := 136;
              Edit.Top := current_widget_ypos - 4;
              Edit.Width := 45;
              Edit.Text := IntToStr(ExtractNumber(Value));
              Edit.OnChange := DoWidgetChange;
              GUIWidgetCollection.ControlList.Add(Edit);
              GUIWidgetCollection.SetMainControl(Edit);

              if (widget.spinedit.unitcontrolbox <> '') then
              begin
                ComboBox := TTntComboBox.Create(GroupBox);
                ComboBox.Parent := GroupBox;
                ComboBox.Name := 'UnitCBox' + WidgetNamePostfix;
                ComboBox.Left := Edit.Left + Edit.Width {+ UpDown.Width} + 4;
                ComboBox.Top := Edit.Top;
                ComboBox.Width := 35;
                ComboBox.Items.Text :=
                  AnsiReplaceStr(widget.spinedit.unitcontrolbox,
                  ';', #13#10);
                ComboBox.ItemIndex := ComboBox.Items.IndexOf(
                  ExtractString(Value));
                if (ComboBox.ItemIndex = -1) and (ComboBox.Items.Count > 0) then
                  ComboBox.ItemIndex := 0;
                ComboBox.OnClick := DoWidgetChange;
                GUIWidgetCollection.ControlList.Add(ComboBox);
              end;

              Lbl := AddDescriptionLabel(GroupBox, 'SEdDesc' + WidgetNamePostfix, widget.description,
                current_widget_ypos);
              GUIWidgetCollection.ControlList.Add(Lbl);

              Inc(current_widget_ypos, Lbl.Height + 16);
            end;
          MYX_TEXTEDIT:
            begin
              // First check for special controls.
              if (widget.textedit.edit_type = 'innodbfilepath') then
                CreateInnoDBFilepathListview(GroupBox, current_widget_ypos, widget, WidgetNamePostfix, GUIWidgetCollection)
              else
                if (widget.textedit.edit_type = 'repldodb') then
                  CreateReplicationDoDBListbox(value, GroupBox, current_widget_ypos, widget, WidgetNamePostfix, GUIWidgetCollection)
                else
                begin
                  // A normal text edit.
                  Lbl := AddCaptionLabel(GroupBox, 'EdLbl' + WidgetNamePostfix, widget.caption, current_widget_ypos,
                    widget.id);
                  GUIWidgetCollection.ControlList.Add(Lbl);

                  Edit := TTntEdit.Create(GroupBox);
                  Edit.Parent := GroupBox;
                  Edit.Name := 'SEd' + WidgetNamePostfix;
                  Edit.Left := 136;
                  Edit.Top := current_widget_ypos - 4;
                  Edit.Width := 179;

                  // Set onChange event
                  if (widget.textedit.edit_type = 'file') then
                    Edit.OnChange := DoFileWidgetChange
                  else
                    if (widget.textedit.edit_type = 'directory') then
                      Edit.OnChange := DoDirWidgetChange
                    else
                      Edit.OnChange := DoWidgetChange;

                  // Finally set the text. Now the change check can step in.
                  // A value of 'checked' means only the option was given, no actual value.
                  if WideSameText(Value, 'checked') then
                    Edit.Text := ''
                  else
                    Edit.Text := Value;

                  GUIWidgetCollection.ControlList.Add(Edit);
                  GUIWidgetCollection.SetMainControl(Edit);
                                                                                    
                  Lbl := AddDescriptionLabel(GroupBox, 'SEdDesc' + WidgetNamePostfix, widget.description,
                    current_widget_ypos, 328, 291);
                  GUIWidgetCollection.ControlList.Add(Lbl);

                  inc(current_widget_ypos, Lbl.Height + 16);
                end;
            end;
          MYX_DROPDOWNBOX:
            begin
              Lbl := AddCaptionLabel(GroupBox, 'DBoxLbl' + WidgetNamePostfix, widget.caption, current_widget_ypos,
                widget.id);
              GUIWidgetCollection.ControlList.Add(Lbl);

              ComboBox := TTntComboBox.Create(GroupBox);
              ComboBox.Parent := GroupBox;
              ComboBox.Name := 'DBox' + WidgetNamePostfix;
              ComboBox.Left := 136;
              ComboBox.Top := current_widget_ypos - 4;
              ComboBox.Width := 255;
              if (widget.dropdownbox.editable = 0) then
                ComboBox.Style := csDropDownList
              else
                ComboBox.Style := csDropDown;
              for l := 0 to widget.dropdownbox.items.Count - 1 do
              begin
                ComboBox.Items.Add(widget.dropdownbox.items.Names[l]);

                if (WideSameText(Value, widget.dropdownbox.items.ValueFromIndex[l])) then
                  ComboBox.ItemIndex := l;
              end;
              ComboBox.OnChange := DoWidgetChange;
              GUIWidgetCollection.ControlList.Add(ComboBox);
              GUIWidgetCollection.SetMainControl(ComboBox);
              GUIWidgetCollection.Data := TTntStringList.Create;
              TTntStringList(GUIWidgetCollection.Data).Text := _(widget.dropdownbox.items.Text);

              Lbl := AddDescriptionLabel(GroupBox, 'DBoxDesc' + WidgetNamePostfix, widget.description,
                current_widget_ypos, 408, 211);
              GUIWidgetCollection.ControlList.Add(Lbl);

              inc(current_widget_ypos, Lbl.Height + 16);
            end;
        end;

        if (widget.widget_type <> MYX_CHECKBOX) then
          EnableDisableGUIWidgetCollection(GUIWidgetCollection, (widget.active = 1));

        current_group_height := current_widget_ypos;
      end;

      GroupBox.Height := current_group_height;

      inc(current_group_ypos, GroupBox.Height + 10);
    end;

    ScrollBox.VertScrollBar.Range := current_group_ypos + 8;
  end;

  //Now display Pages
  for i := 0 to StartupParamsPageControl.PageCount - 1 do
    StartupParamsPageControl.Pages[i].TabVisible := True;

  StartupParamsPageControl.ActivePageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.SetInnoDBFilePathValues(GUIWidgetCollection: TGUIWidgetCollection);

var
  value: WideString;
  maxval: WideString;
  filesval: WideString;
  nextfile: WideString;
  filename: WideString;
  filesize: WideString;
  i: Integer;
  Item: TListItem;

begin
  // Make sure we get \ and not /
  value := AnsiReplaceText(GUIWidgetCollection.widget.value, '/', '\');

  maxval := '';
  if (Pos(':max:', LowerCase(value)) > 0) then
    maxval := Copy(value, Pos(':max:', LowerCase(value)) + 5, Length(value));
  filesval := Copy(value, 1, Length(value) - Length(maxval));

  TTntListView(GUIWidgetCollection.MainControl).Clear;
  while (filesval <> '') do
  begin
    if (Pos(';', filesval) > 0) then
    begin
      nextfile := Copy(filesval, 1, Pos(';', filesval) - 1);
      filesval := Copy(filesval, Pos(';', filesval) + 1, Length(filesval));
    end
    else
    begin
      nextfile := filesval;
      filesval := '';
    end;

    filename := Copy(nextfile, 1, Pos(':', nextfile) - 1);
    //Ignore first : when drive is used (c:\)
    if (Length(nextfile) > 3) then
      if (Copy(nextfile, 2, 2) = ':\') then
        filename := Copy(nextfile, 1, Pos(':', Copy(nextfile, 3, Length(nextfile))) + 2 - 1);

    nextfile := Copy(nextfile, Length(filename) + 2, Length(nextfile));

    if (Pos(':', nextfile) > 0) then
      filesize := Copy(nextfile, 1, Pos(':', nextfile) - 1)
    else
      filesize := nextfile;

    Item := AddListViewItem(TTntListView(GUIWidgetCollection.MainControl), nil,
      filename, TablespaceImg, nil);
    Item.SubItems.Add(filesize);

  end;

  for i := 0 to GUIWidgetCollection.ControlList.Count - 1 do
  begin
    if (TObject(GUIWidgetCollection.ControlList[i]) is TTntCheckBox) then
      TTntCheckBox(GUIWidgetCollection.ControlList[i]).Checked :=
        (Pos(':autoextend', LowerCase(value)) > 0);
    if (TObject(GUIWidgetCollection.ControlList[i]) is TTntUpDown) and
      (maxval <> '') then
      TTntUpDown(GUIWidgetCollection.ControlList[i]).Position :=
        ExtractNumber(maxval);
    if (TObject(GUIWidgetCollection.ControlList[i]) is TTntEdit) and
      (maxval <> '') then
      TTntEdit(GUIWidgetCollection.ControlList[i]).Text :=
        IntToStr(ExtractNumber(maxval));
    if (TObject(GUIWidgetCollection.ControlList[i]) is TTntComboBox) and
      (maxval <> '') then
      TTntComboBox(GUIWidgetCollection.ControlList[i]).ItemIndex :=
        TTntComboBox(GUIWidgetCollection.ControlList[i]).Items.IndexOf(
        ExtractString(maxval));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.EnableDisableGUIWidgetCollection(GUIWidgetCollection: TGUIWidgetCollection;
  State: Boolean);

var
  i: Integer;

begin
  for i := 0 to GUIWidgetCollection.ControlList.Count - 1 do
  begin
    if (TObject(GUIWidgetCollection.ControlList[i]).InheritsFrom(TControl)) then
      TControl(GUIWidgetCollection.ControlList[i]).Enabled := State;
  end;
end;

procedure TAdminStartupVariablesForm.ActiveImgMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  GUIWidgetCollection: TGUIWidgetCollection;
begin
  if (Sender is TTntImage) then
  begin
    DoPageContentChanged(Self);

    if (TTntImage(Sender).Tag < FGUIWidgetCollectionList.Count) then
    begin
      if (TGUIWidgetCollection(FGUIWidgetCollectionList[TTntImage(Sender).Tag]).ActivateControl =
        Sender) then
        GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[TTntImage(Sender).Tag])
      else
        GUIWidgetCollection := ScanGUIWidgetCollList4ActivateControl(TTntImage(Sender));
    end
    else
      GUIWidgetCollection := ScanGUIWidgetCollList4ActivateControl(TTntImage(Sender));

    if (GUIWidgetCollection <> nil) then
      ChangeWidgetActivationState(GUIWidgetCollection, not (GUIWidgetCollection.widget.active = 1));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ChangeWidgetActivationState(GUIWidgetCollection: TGUIWidgetCollection;
  State: Boolean);

begin
  if (GUIWidgetCollection <> nil) then
  begin
    EnableDisableGUIWidgetCollection(GUIWidgetCollection,
      State);

    GUIWidgetCollection.widget.active := Ord(State);

    if (GUIWidgetCollection.ActivateControl <> nil) then
    begin
      if (GUIWidgetCollection.widget.active = 1) then
        TTntImage(GUIWidgetCollection.ActivateControl).Picture.Assign(FEditEnabledPNGImg)
      else
        TTntImage(GUIWidgetCollection.ActivateControl).Picture.Assign(FEditDisabledPNGImg);
    end;
  end;
end;

function TAdminStartupVariablesForm.ScanGUIWidgetCollList4ActivateControl(
  ActivateControl: TTntImage): TGUIWidgetCollection;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FGUIWidgetCollectionList.Count - 1 do
    if (TGUIWidgetCollection(FGUIWidgetCollectionList[i]).ActivateControl =
      ActivateControl) then
    begin
      Result := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);
      break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.AddInnoDBTablespace(Sender: TObject);

var
  AdminStartupVariablesInnoDBDatafilesForm: TAdminStartupVariablesInnoDBDatafilesForm;
  i: Integer;
  GUIWidgetCollection: TGUIWidgetCollection;
  Item: TListItem;

begin
  GUIWidgetCollection := nil;

  for i := 0 to FGUIWidgetCollectionList.Count - 1 do
    if (TGUIWidgetCollection(FGUIWidgetCollectionList[i]).ControlList.IndexOf(Sender) > -1) then
    begin
      GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);
      break;
    end;

  if (GUIWidgetCollection <> nil) then
  begin
    AdminStartupVariablesInnoDBDatafilesForm := TAdminStartupVariablesInnoDBDatafilesForm.Create(self);
    try
      //Button was clicked
      if (TObject(Sender) is TTntButton) or (TObject(Sender) is TTntMenuItem) then
        AdminStartupVariablesInnoDBDatafilesForm.TablespaceEd.Text :=
          'ibdata' +
          IntToStr(TTntListView(GUIWidgetCollection.MainControl).Items.Count + 1)
      else
      //TreeView was doubleclicked
        if (TObject(Sender) is TTntListView) then
        begin
          if (TTntListView(Sender).Selected <> nil) then
          begin
          //Set name
            AdminStartupVariablesInnoDBDatafilesForm.TablespaceEd.Text :=
              TTntListView(Sender).Selected.Caption;

          //Set size
            if (TTntListView(Sender).Selected.SubItems.Count > 0) then
            begin
              AdminStartupVariablesInnoDBDatafilesForm.SizeUpDown.Position :=
                ExtractNumber(TTntListView(Sender).Selected.SubItems[0]);

              if (AdminStartupVariablesInnoDBDatafilesForm.UnitCBox.Items.IndexOf(
                ExtractString(TTntListView(Sender).Selected.SubItems[0])) > -1) then
                AdminStartupVariablesInnoDBDatafilesForm.UnitCBox.ItemIndex :=
                  AdminStartupVariablesInnoDBDatafilesForm.UnitCBox.Items.IndexOf(
                  ExtractString(TTntListView(Sender).Selected.SubItems[0]));

            end;
          end;
        end;

      if (AdminStartupVariablesInnoDBDatafilesForm.ShowModal = mrOK) then
      begin
        DoPageContentChanged(self);

        if (TObject(Sender) is TTntButton) or (TObject(Sender) is TTntMenuItem) then
        begin
          Item := AddListViewItem(TTntListView(GUIWidgetCollection.MainControl), nil,
            AdminStartupVariablesInnoDBDatafilesForm.TablespaceEd.Text, TablespaceImg, nil);
          Item.SubItems.Add(AdminStartupVariablesInnoDBDatafilesForm.SizeEd.Text +
            AdminStartupVariablesInnoDBDatafilesForm.UnitCBox.Text);
        end
        else
          if (TObject(Sender) is TTntListView) then
          begin
            if (TTntListView(Sender).Selected <> nil) then
            begin
              TTntListView(Sender).Selected.Caption :=
                AdminStartupVariablesInnoDBDatafilesForm.TablespaceEd.Text;

              if (TTntListView(Sender).Selected.SubItems.Count > 0) then
              begin
                TTntListView(Sender).Selected.SubItems[0] :=
                  AdminStartupVariablesInnoDBDatafilesForm.SizeEd.Text +
                  AdminStartupVariablesInnoDBDatafilesForm.UnitCBox.Text
              end;
            end;
          end;

        DoWidgetChange(Sender);
      end;
    finally
      AdminStartupVariablesInnoDBDatafilesForm.Free;
    end;
  end;

  DoPageContentChanged(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.RemoveInnoDBTablespace(Sender: TObject);

var
  i: Integer;
  GUIWidgetCollection: TGUIWidgetCollection;

begin
  GUIWidgetCollection := nil;

  for i := 0 to FGUIWidgetCollectionList.Count - 1 do
    if (TGUIWidgetCollection(FGUIWidgetCollectionList[i]).ControlList.IndexOf(Sender) > -1) then
    begin
      GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);
      break;
    end;

  if (GUIWidgetCollection <> nil) then
  begin
    if (TTntListView(GUIWidgetCollection.MainControl).Selected <> nil) then
    begin
      if (ShowModalDialog(_('Remove data file'),
        _('Are you sure you want to remove the selected data file:') + #13#10 +
        TTntListView(GUIWidgetCollection.MainControl).Selected.Caption + #13#10#13#10 +
        _('Please note that you have to delete the file manually after the ' +
        'Service has been stopped or restarted.'),
        myx_mtConfirmation,
        _('Remove') + #13#10 + _('Cancel')) = 1) then
      begin
        TTntListView(GUIWidgetCollection.MainControl).DeleteSelected;
        DoPageContentChanged(self);
      end;
    end;
  end;

  DoPageContentChanged(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DoWidgetChange(Sender: TObject);

var
  i, j: Integer;
  GUIWidgetCollection: TGUIWidgetCollection;
  OldValue, s: WideString;
  MaxWasSet: Boolean;

begin
  if (not (InitControls)) then
  begin
    GUIWidgetCollection := nil;

    //Check main controls
    for i := 0 to FGUIWidgetCollectionList.Count - 1 do
      if (TGUIWidgetCollection(FGUIWidgetCollectionList[i]).MainControl =
        Sender) then
      begin
        GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);
        break;
      end;

    //if widget is not found, check all controls
    if (GUIWidgetCollection = nil) then
    begin
      for i := 0 to FGUIWidgetCollectionList.Count - 1 do
        for j := 0 to TGUIWidgetCollection(FGUIWidgetCollectionList[i]).ControlList.Count - 1 do
          if (TGUIWidgetCollection(FGUIWidgetCollectionList[i]).ControlList[j] =
            Sender) then
          begin
            GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);
            break;
          end;
    end;

    if (GUIWidgetCollection <> nil) then
    begin
      OldValue := GUIWidgetCollection.widget.value;

      //Set widget.value
      if (GUIWidgetCollection.widget.widget_type = MYX_CHECKBOX) then
      begin
        //Set checked/unchecked
        if (TTntCheckBox(GUIWidgetCollection.MainControl).Checked) then
          GUIWidgetCollection.widget.value := 'Checked'
        else
          GUIWidgetCollection.widget.value := 'Unchecked';
      end
      else
        if (GUIWidgetCollection.widget.widget_type = MYX_SPINEDIT) then
        begin
        //Get Value
          GUIWidgetCollection.widget.value :=
            TTntEdit(GUIWidgetCollection.MainControl).Text;
        //Add Unit
          for i := 0 to GUIWidgetCollection.ControlList.Count - 1 do
            if (TObject(GUIWidgetCollection.ControlList[i]) is TTntComboBox) then
              GUIWidgetCollection.widget.value :=
                GUIWidgetCollection.widget.value +
                TTntComboBox(GUIWidgetCollection.ControlList[i]).Text;
        end
        else
          if (GUIWidgetCollection.widget.widget_type = MYX_TEXTEDIT) then
          begin
            if (GUIWidgetCollection.widget.textedit <> nil) then
              if (GUIWidgetCollection.widget.textedit.edit_type = 'innodbfilepath') then
              begin
                s := '';
            //get data files
                for i := 0 to TTntListView(GUIWidgetCollection.MainControl).Items.Count - 1 do
                begin
                  if (s <> '') then
                    //s := s + ':';
                    s := s + ';';

                  s := s + TTntListView(GUIWidgetCollection.MainControl).Items[i].Caption + ':';

                  if (TTntListView(GUIWidgetCollection.MainControl).Items[i].SubItems.Count > 0) then
                    s := s + TTntListView(GUIWidgetCollection.MainControl).Items[i].SubItems[0]
                  else
                    s := s + '10M';
                end;
            //Get auto extend
                for i := 0 to GUIWidgetCollection.ControlList.Count - 1 do
                  if (TObject(GUIWidgetCollection.ControlList[i]) is TTntCheckBox) then
                  begin
                    if (TTntCheckBox(GUIWidgetCollection.ControlList[i]).Checked) then
                    begin
                      s := s + ':autoextend';

                  //Get Max
                      MaxWasSet := False;
                      for j := 0 to GUIWidgetCollection.ControlList.Count - 1 do
                        if (TObject(GUIWidgetCollection.ControlList[j]) is TTntEdit) then
                          if (StrToIntDef(
                            TTntEdit(GUIWidgetCollection.ControlList[j]).Text, 0) > 0) then
                          begin
                            s := s + ':max:' + TTntEdit(GUIWidgetCollection.ControlList[j]).Text;
                            MaxWasSet := True;
                            break;
                          end;

                      if MaxWasSet then
                        for j := 0 to GUIWidgetCollection.ControlList.Count - 1 do
                          if (TObject(GUIWidgetCollection.ControlList[j]) is TTntComboBox) then
                          begin
                            s := s + TTntComboBox(GUIWidgetCollection.ControlList[j]).Text;
                          end;

                    end;
                    break;
                  end;

                GUIWidgetCollection.widget.value := s;
              end
              else
                GUIWidgetCollection.widget.value :=
                  TTntEdit(GUIWidgetCollection.MainControl).Text;
          end
          else
            if (GUIWidgetCollection.widget.widget_type = MYX_DROPDOWNBOX) then
            begin
              if (TTntComboBox(GUIWidgetCollection.MainControl).ItemIndex >= 0) and
                (TTntComboBox(GUIWidgetCollection.MainControl).ItemIndex <
                GUIWidgetCollection.widget.dropdownbox.items.Count) then
                GUIWidgetCollection.widget.value :=
                  GUIWidgetCollection.widget.dropdownbox.items.ValueFromIndex[TTntComboBox(GUIWidgetCollection.MainControl).ItemIndex];
            end;

      // Check if the value has really changed.
      // Ignore Upper/Lowercase for MYX_CHECKBOX, MYX_SPINEDIT, MYX_DROPDOWNBOX.
      if (GUIWidgetCollection.widget.widget_type = MYX_CHECKBOX) or
        (GUIWidgetCollection.widget.widget_type = MYX_SPINEDIT) or
        (GUIWidgetCollection.widget.widget_type = MYX_DROPDOWNBOX) then
      begin
        if (CompareText(GUIWidgetCollection.widget.value, OldValue) <> 0) then
          DoPageContentChanged(self);
      end
      else
        if (GUIWidgetCollection.widget.value <> OldValue) then
          DoPageContentChanged(self);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DoFileWidgetChange(Sender: TObject);

var
  FileName: WideString;
  
begin
  FileName := TTntEdit(Sender).Text;

  // If the file name is relative then take the server path into account.
  // All relative pathes are considered relative to the server's database root.
  if not ((Length(FileName) > 2) and (FileName[2] = ':') and (FileName[3] in [WideChar('/'), WideChar('\')])) then
    FileName := ApplicationDM.DataDir + FileName;

  if FileExists(FileName) then
    TTntEdit(Sender).Font.Color := clBlack
  else
    TTntEdit(Sender).Font.Color := clRed;

  DoWidgetChange(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DoDirWidgetChange(Sender: TObject);

begin
  if (DirectoryExists(TTntEdit(Sender).Text)) then
    TTntEdit(Sender).Font.Color := clBlack
  else
    TTntEdit(Sender).Font.Color := clRed;

  DoWidgetChange(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ValidateFileName(Sender: TObject);

begin
  if (Sender is TTntEdit) then
  begin
    if (not (FileExists(TTntEdit(Sender).Text))) then
    begin
      if (not (FileExists(TTntEdit(Sender).Text + '.exe'))) then
        TTntEdit(Sender).Font.Color := clRed
      else
        TTntEdit(Sender).Font.Color := clBlack;
    end
    else
      TTntEdit(Sender).Font.Color := clBlack;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DoPageContentChanged(Sender: TObject);

begin
  if (not (InitControls)) then
  begin
    FPageContentChanged := True;

    ApplyChangesBtn.Enabled := True;
    DiscardChangesBtn.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DiscardChangesBtnClick(Sender: TObject);

begin
  DiscardChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ApplyChangesBtnClick(Sender: TObject);

begin
  ApplyChanges;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ApplyChanges;

var
  Error: MYX_ADMIN_LIB_ERROR;
  i: Integer;

begin
  if ((FPageContentChanged) and (ApplicationDM.CurrentService <> nil)) then
  begin
    // Make backup file.
    if FileExists(ApplicationDM.CurrentService.ConfigFile) then
    begin
      i := 1;
      while (FileExists(ChangeFileExt(ApplicationDM.CurrentService.ConfigFile, '.bak' + IntToStr(i)))) do
        inc(i);

      CopyDiskFile(ApplicationDM.CurrentService.ConfigFile, ChangeFileExt(ApplicationDM.CurrentService.ConfigFile, '.bak' + IntToStr(i)),
        False);
    end;

    Error := myx_update_mysql_cnf_file(FGuiDesc.get_record_pointer, ApplicationDM.CurrentService.ConfigFile, FCurrentSection);

    if Error <> MYX_ADMIN_NO_ERROR then
      raise EMyxError.Create(WideFormat(_('The configuration file %s (section %s) cannot be updated.') + #13#10 +
        _('Error no.: %d'), [ApplicationDM.CurrentService.ConfigFile, FCurrentSection, Ord(Error)]));
  end;

  FPageContentChanged := False;
  ApplyChangesBtn.Enabled := False;
  DiscardChangesBtn.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.DiscardChanges;

begin
  if (FPageContentChanged) then
  begin
    if ShowModalDialog(_('Discard Changes?'), _('The current settings have been modified.') + #13#10 +
      _('Are you sure you want to discard the changes?'), myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1 then
    begin
      RefreshStartupParametersFromCnfFile;

      FPageContentChanged := False;
      ApplyChangesBtn.Enabled := False;
      DiscardChangesBtn.Enabled := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.StartupParamsPageControlChange(Sender: TObject);

begin
  if (StartupParamsPageControl.ActivePage <> nil) then
  begin
    SheetHeaderLbl.Caption := StartupParamsPageControl.ActivePage.Caption;
    SheetHeaderPnl.Parent := StartupParamsPageControl.ActivePage;
    SheetHeaderPnl.Align := alTop;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.RefreshStartupParametersFromCnfFile;

var
  pgui_desc: PMYX_GUI_DESCRIPTION;
  gui_desc_new: TMYX_GUI_DESCRIPTION;
  error_code: MYX_ADMIN_LIB_ERROR;

begin
  if (FGuiDesc = nil) then
    BuildStartupParamPages
  else
  begin
    pgui_desc := nil;
    if ApplicationDM.CurrentService <> nil then
      if FileExists(ApplicationDM.CurrentService.ConfigFile) then
        pgui_desc := myx_get_gui_description(
          MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml',
          ApplicationDM.Options.MySQLVersion, MYX_WINDOWS, @error_code, ApplicationDM.CurrentService.ConfigFile, FCurrentSection)
      else
        pgui_desc := myx_get_gui_description(
          MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml',
          ApplicationDM.Options.MySQLVersion, MYX_WINDOWS, @error_code, '', '');

    if (error_code <> MYX_ADMIN_NO_ERROR) then
      raise EMyxError.Create(WideFormat(_('Error while loading the Startup Parameter descriptions from %s') +
        'mysqladmin_startup_variables_description.xml' + #13#10#13#10 + _('Error no.: %d'), [MYXCommonOptions.XMLDir,
        Ord(error_code)]));

    gui_desc_new := nil;
    try
      // Create new TMYX_GUI_DESCRIPTION
      gui_desc_new := TMYX_GUI_DESCRIPTION.create(pgui_desc);

      //Free old TMYX_GUI_DESCRIPTION
      FGuiDesc.Free;

      //Assign new TMYX_GUI_DESCRIPTION to class var
      FGuiDesc := gui_desc_new;

      //Reassign GUIWidgetCollections based on ids
      ReassignGUIWidgetCollections;

      //Refresh Controls
      RefreshStartupParametersControls;
    except
      if (gui_desc_new <> nil) then
        gui_desc_new.Free;

      raise;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ReassignGUIWidgetCollections;

var
  i, j, k, l: Integer;
  GUIWidgetCollection: TGUIWidgetCollection;
  AllFound: Boolean;

begin
  //Clear all widget
  AllFound := True;
  for l := 0 to FGUIWidgetCollectionList.Count - 1 do
  begin
    GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[l]);
    GUIWidgetCollection.widget := nil;

    //Try to find widgets in the same place
    if (GUIWidgetCollection.prev_page_index <= FGuiDesc.pages.Count) then
      if (GUIWidgetCollection.prev_group_index <=
        FGuiDesc.pages[GUIWidgetCollection.prev_page_index].groups.Count) then
        if (GUIWidgetCollection.prev_widget_index <=
          FGuiDesc.pages[GUIWidgetCollection.prev_page_index].groups[
          GUIWidgetCollection.prev_group_index].widgets.Count) then
        begin
          GUIWidgetCollection.widget :=
            FGuiDesc.pages[GUIWidgetCollection.prev_page_index].groups[
            GUIWidgetCollection.prev_group_index].widgets[
            GUIWidgetCollection.prev_widget_index];

          GUIWidgetCollection.widget.value :=
            ReplaceTokensInValue(GUIWidgetCollection.widget.value);
        end
        else
          AllFound := False;
  end;

  //if not all widgets were found at their previous position,
  //scan all widgets
  if (not (AllFound)) then
  begin
    //Reassign widgets
    for i := 0 to FGuiDesc.pages.Count - 1 do
      for j := 0 to FGuiDesc.pages[i].groups.Count - 1 do
        for k := 0 to FGuiDesc.pages[i].groups[j].widgets.Count - 1 do
          for l := 0 to FGUIWidgetCollectionList.Count - 1 do
            if (FGuiDesc.pages[i].groups[j].widgets[k].id =
              TGUIWidgetCollection(FGUIWidgetCollectionList[l]).id) then
            begin
              TGUIWidgetCollection(FGUIWidgetCollectionList[l]).widget :=
                FGuiDesc.pages[i].groups[j].widgets[k];

              TGUIWidgetCollection(FGUIWidgetCollectionList[l]).widget.value :=
                ReplaceTokensInValue(TGUIWidgetCollection(FGUIWidgetCollectionList[l]).widget.value);

              break;
            end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TAdminStartupVariablesForm.ReplaceTokensInValue(value: WideString): WideString;

begin
  Result := AnsiReplaceStr(value, '$windows_program_dir$', GetProgramFilesDir);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.RefreshStartupParametersControls;

var
  i, j: Integer;
  GUIWidgetCollection: TGUIWidgetCollection;
  value: WideString;
  
begin

  InitControls := True;
  try
    for i := 0 to FGUIWidgetCollectionList.Count - 1 do
    begin
      GUIWidgetCollection := TGUIWidgetCollection(FGUIWidgetCollectionList[i]);

      ChangeWidgetActivationState(GUIWidgetCollection,
        (GUIWidgetCollection.widget.active = 1));

      //Replace Tokens
      value := ReplaceTokensInValue(GUIWidgetCollection.widget.value);

      if (GUIWidgetCollection.widget <> nil) then
      begin
        if (GUIWidgetCollection.widget.widget_type = MYX_SPINEDIT) then
        begin
          for j := 0 to GUIWidgetCollection.ControlList.Count - 1 do
            if (TControl(GUIWidgetCollection.ControlList[j]) is TTntUpDown) then
              TTntUpDown(GUIWidgetCollection.ControlList[j]).Position :=
                ExtractNumber(value)
            else
            if (TObject(GUIWidgetCollection.ControlList[j]) is TTntEdit) then
              TTntEdit(GUIWidgetCollection.ControlList[j]).Text :=
                IntToStr(ExtractNumber(value))
            else
              if (TControl(GUIWidgetCollection.ControlList[j]) is TTntComboBox) then
                TTntComboBox(GUIWidgetCollection.ControlList[j]).ItemIndex :=
                  TTntComboBox(GUIWidgetCollection.ControlList[j]).Items.IndexOf(ExtractString(value));
        end
        else
          if (GUIWidgetCollection.widget.widget_type = MYX_TEXTEDIT) then
          begin
            if (GUIWidgetCollection.widget.textedit.edit_type = 'innodbfilepath') then
            begin
            end
            else
              if (GUIWidgetCollection.MainControl <> nil) then
                TTntEdit(GUIWidgetCollection.MainControl).Text := value;
          end
          else
            if (GUIWidgetCollection.widget.widget_type = MYX_CHECKBOX) then
            begin
              TTntCheckBox(GUIWidgetCollection.MainControl).Checked :=
                (CompareText(value, 'checked') = 0);
            end
            else
              if (GUIWidgetCollection.widget.widget_type = MYX_DROPDOWNBOX) then
              begin
                for j := 0 to TTntComboBox(GUIWidgetCollection.MainControl).Items.Count - 1 do
                  if (WideSameText(
                    TTntStringList(GUIWidgetCollection.Data).ValueFromIndex[j],
                    value)) then
                  begin
                    TTntComboBox(GUIWidgetCollection.MainControl).ItemIndex := j;
                    break;
                  end;
              end;
      end;
    end;
  finally
    InitControls := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.RefreshServiceList;

var
  LastService: WideString;

begin
  if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    if Assigned(ApplicationDM.CurrentService) then
      LastService := ApplicationDM.CurrentService.ServiceName;
    SetCurrentService(nil);
    ServicesTreeView.Items.Clear;

    // Get all MySQL Services.
    ScanForServices(ServicesTreeView, ServicesImageIndex);
    ChangeCurrentService(LastService);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ActivateSection;

begin
  inherited;

  ChangeCurrentService;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.CreateInnoDBFilepathListview(GroupBox: TTntGroupBox; var current_widget_ypos: Integer;
  var widget: TMYX_GUI_WIDGET; WidgetNamePostfix: WideString; GUIWidgetCollection: TGUIWidgetCollection);

var
  Size: Int64;
  CheckBox: TTntCheckBox;
  Lbl: TTntLabel;
  ComboBox: TTntComboBox;
  Edit: TTntEdit;
  ListView: TTntListView;
  ListColumn: TListColumn;
  Button: TTntButton;
  PopupMenu: TTntPopupMenu;
  MenuItem: TTntMenuItem;

begin
  begin
    // If there is no individual table space active then the widget only
    // contains a dummy value, which must be adjusted to the actual value of the
    // (only) table space InnoDB uses by default.
    if Widget.active = 0 then
    begin
      Size := GetFileSize(ApplicationDM.DataDir + 'ibdata1');
      if Size > 0 then
        Widget.value := WideFormat('ibdata1:%d:autoextend', [Size]);
    end;
    Lbl := AddCaptionLabel(GroupBox, 'EdLbl' + WidgetNamePostfix, widget.caption, current_widget_ypos, widget.id);
    GUIWidgetCollection.ControlList.Add(Lbl);
    ListView := TTntListView.Create(GroupBox);
    ListView.Parent := GroupBox;
    ListView.Name := 'ListView' + WidgetNamePostfix;
    ListView.Left := 136;
    ListView.Top := current_widget_ypos - 4;
    ListView.Width := 255 + 90;
    ListView.Height := 91;
    ListView.RowSelect := True;
    ListView.SmallImages := ApplicationDM.AdminTree16ImageList;
    ListView.ReadOnly := True;
    ListView.HideSelection := False;
    ListView.OnDblClick := AddInnoDBTablespace;
    ListView.ViewStyle := vsReport;
    ListColumn := ListView.Columns.Add;
    ListColumn.Caption := _('File name');
    ListColumn.Width := 170 + 90;
    ListColumn := ListView.Columns.Add;
    ListColumn.Caption := _('Size');
    ListColumn.Width := 60;
    GUIWidgetCollection.ControlList.Add(ListView);
    GUIWidgetCollection.SetMainControl(ListView);
    Button := TTntButton.Create(GroupBox);
    Button.Parent := GroupBox;
    Button.Name := 'ListViewAddBtn' + WidgetNamePostfix;
    Button.Left := 494;
    Button.Top := current_widget_ypos - 4;
    Button.Width := 121;
    Button.Height := 25;
    Button.Caption := _('Add data file');
    Button.OnClick := AddInnoDBTablespace;
    GUIWidgetCollection.ControlList.Add(Button);
    Button := TTntButton.Create(GroupBox);
    Button.Parent := GroupBox;
    Button.Name := 'ListViewDelBtn' + WidgetNamePostfix;
    Button.Left := 494;
    Button.Top := current_widget_ypos - 4 + 34;
    Button.Width := 121;
    Button.Height := 25;
    Button.Caption := _('Remove data file');
    Button.OnClick := RemoveInnoDBTablespace;
    GUIWidgetCollection.ControlList.Add(Button);
    PopupMenu := TTntPopupMenu.Create(GroupBox);
    PopupMenu.Name := 'PopupMenu' + WidgetNamePostfix;
    ListView.PopupMenu := PopupMenu;
    GUIWidgetCollection.ControlList.Add(PopupMenu);
    MenuItem := TTntMenuItem.Create(PopupMenu);
    MenuItem.Name := 'AddDataFile' + WidgetNamePostfix;
    MenuItem.Caption := _('Add Data File');
    MenuItem.OnClick := AddInnoDBTablespace;
    PopupMenu.Items.Add(MenuItem);
    GUIWidgetCollection.ControlList.Add(MenuItem);
    MenuItem := TTntMenuItem.Create(PopupMenu);
    MenuItem.Name := 'RemoveDataFile' + WidgetNamePostfix;
    MenuItem.Caption := _('Remove Data File');
    MenuItem.OnClick := RemoveInnoDBTablespace;
    PopupMenu.Items.Add(MenuItem);
    GUIWidgetCollection.ControlList.Add(MenuItem);
    // Automatic tablespace extension CheckBox.
    CheckBox := TTntCheckBox.Create(GroupBox);
    CheckBox.Parent := GroupBox;
    CheckBox.Name := 'AutoExtCBox' + WidgetNamePostfix;
    CheckBox.Caption := _('Extend last file automatically');
    CheckBox.Left := 136;
    CheckBox.Top := current_widget_ypos - 2 + 91 + 8;
    CheckBox.Width := 201;
    CheckBox.OnClick := DoWidgetChange;
    GUIWidgetCollection.ControlList.Add(CheckBox);
    // Max extension Spin Edit.
    Lbl := AddDescriptionLabel(GroupBox, 'SEdLbl' + WidgetNamePostfix, 'Max:', current_widget_ypos + 91 + 8, 346, 50);
    GUIWidgetCollection.ControlList.Add(Lbl);
    Edit := TTntEdit.Create(GroupBox);
    Edit.Parent := GroupBox;
    Edit.Name := 'AutoExtMaxEd' + WidgetNamePostfix;
    Edit.Left := 380;
    Edit.Top := current_widget_ypos - 4 + 91 + 8;
    Edit.Width := 45;
    Edit.Text := '0';
    Edit.OnChange := DoWidgetChange;
    GUIWidgetCollection.ControlList.Add(Edit);
    ComboBox := TTntComboBox.Create(GroupBox);
    ComboBox.Parent := GroupBox;
    ComboBox.Name := 'AutoExtMaxUnitCBox' + WidgetNamePostfix;
    ComboBox.Left := Edit.Left + Edit.Width + 4;
    ComboBox.Top := Edit.Top;
    ComboBox.Width := 35;
    ComboBox.Items.Text := 'k'#13''#10'M';
    ComboBox.ItemIndex := 1;
    ComboBox.OnChange := DoWidgetChange;
    GUIWidgetCollection.ControlList.Add(ComboBox);
    SetInnoDBFilePathValues(GUIWidgetCollection);
    Inc(current_widget_ypos, Lbl.Height + 91 + 34);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.CreateReplicationDoDBListbox(value: WideString; GroupBox: TTntGroupBox;
  var current_widget_ypos: Integer; var widget: TMYX_GUI_WIDGET; WidgetNamePostfix: WideString;
  GUIWidgetCollection: TGUIWidgetCollection);

var
  Lbl: TTntLabel;
  Edit: TTntEdit;

begin
  Lbl := AddCaptionLabel(GroupBox, 'EdLbl' + WidgetNamePostfix, widget.caption, current_widget_ypos,
    widget.id);
  GUIWidgetCollection.ControlList.Add(Lbl);

  Edit := TTntEdit.Create(GroupBox);
  Edit.Parent := GroupBox;
  Edit.Name := 'SEd' + WidgetNamePostfix;
  Edit.Left := 136;
  Edit.Top := current_widget_ypos - 4;
  Edit.Width := 179;

  // Set onChange event
  if (widget.textedit.edit_type = 'file') then
    Edit.OnChange := DoFileWidgetChange
  else
    if (widget.textedit.edit_type = 'directory') then
      Edit.OnChange := DoDirWidgetChange
    else
      Edit.OnChange := DoWidgetChange;

  // Finally set the text. Now the change check can step in.
  Edit.Text := Value;

  GUIWidgetCollection.ControlList.Add(Edit);
  GUIWidgetCollection.SetMainControl(Edit);
                                                                                    
  Lbl := AddDescriptionLabel(GroupBox, 'SEdDesc' + WidgetNamePostfix, widget.description,
    current_widget_ypos, 328, 291);
  GUIWidgetCollection.ControlList.Add(Lbl);

  inc(current_widget_ypos, Lbl.Height + 16);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ChangeCurrentService(ServiceName: WideString);

var
  I: Integer;

begin
  // Activate the last selected service or the first one if none was before.
  if ServicesTreeView.Items.Count > 0 then
  begin
    if (ServiceName = '') and Assigned(ApplicationDM.CurrentService) then
      ServiceName := ApplicationDM.CurrentService.ServiceName;
    I := SelectService(ServicesTreeview, ServiceName);
    if I = -1 then
      I := 0;
    SetCurrentService(ServicesTreeView.Items[I].Data);
    ServicesTreeView.Selected := ServicesTreeView.Items[I];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.SetCurrentService(MySQLService: TMySQLService);

var stringlist: TMYX_STRINGLIST;
  pstringlist: PMYX_STRINGLIST;
  error: MYX_ADMIN_LIB_ERROR;
  I: Integer;
  Found: Boolean;

begin
  ApplicationDM.CurrentService := MySQLService;

  // If there is a service, take the config section defined by the service.
  if Assigned(MySQLService) then
  begin
    FCurrentSection := MySQLService.ConfigFileSection;

    // Get config file sections
    if FileExists(ApplicationDM.CurrentService.ConfigFile) then
    begin
      pstringlist := myx_get_all_cnf_sections(ApplicationDM.CurrentService.ConfigFile, @error);
      try
        stringlist := TMYX_STRINGLIST.create(pstringlist);
        try
          Found := False;
          for I := 0 to stringlist.strings.Count - 1 do
            if WideSameText(FCurrentSection, stringlist.strings[i]) then
            begin
              Found := True;
              break;
            end;

          if not Found then
            FCurrentSection := 'mysqld';
        finally
          stringlist.Free;
        end;
      finally
        myx_free_admin_lib_stringlist(pstringlist);
      end;
    end;
  end
  else
    FCurrentSection := 'mysqld';

  UpdateConfigFileLabels;

  // Refresh Startup Parameters.
  if not InitControls and Assigned(MySQLService) then
    RefreshStartupParametersFromCnfFile;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.UpdateConfigFileLabels;

begin
  if Assigned(ApplicationDM.CurrentService) then
    OptionFileLabel.Caption:= ApplicationDM.CurrentService.ConfigFile;

  if SectionLbl.Caption <> '' then
    SectionLbl.Caption := '[' + FCurrentSection + ']'
  else
    SectionLbl.Caption := 'No section selected';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not InitControls and not (csDestroying in ComponentState) then
  begin
    if FPageContentChanged then
      DiscardChanges;

    ApplicationDM.CurrentService := nil;

    if (ServicesTreeView.Selected <> nil) then
      if (ServicesTreeView.Selected.Data <> nil) then
        ApplicationDM.CurrentService := TMySQLService(ServicesTreeView.Selected.Data);

    SetCurrentService(ApplicationDM.CurrentService);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ParamSectionNameEdChange(Sender: TObject);

begin
  if (not (InitControls)) then
    RefreshStartupParametersFromCnfFile;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ChooseCnfFileBtnClick(Sender: TObject);

var
  AdminStartupVariablesOptionFileForm: TAdminStartupVariablesOptionFileForm;
  
begin
  AdminStartupVariablesOptionFileForm := TAdminStartupVariablesOptionFileForm.Create(self);
  try
    AdminStartupVariablesOptionFileForm.SetConfigfFile(ApplicationDM.CurrentService.ConfigFile, FCurrentSection);

    if AdminStartupVariablesOptionFileForm.ShowModal = mrOK then
    begin
      ApplicationDM.CurrentService.ConfigFile := AdminStartupVariablesOptionFileForm.ParamConfigFileEd.Text;
      FCurrentSection := AdminStartupVariablesOptionFileForm.ParamSectionNameCBox.Text;

      UpdateConfigFileLabels;

      if (not (InitControls)) then
        RefreshStartupParametersFromCnfFile;
    end;
  finally
    AdminStartupVariablesOptionFileForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminStartupVariablesForm.ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);

begin
  if not (csDestroying in ComponentState) and not (csRecreating in ServicesTreeView.ControlState) then
  begin
    if Node.Data = ApplicationDM.CurrentService then
      SetCurrentService(nil);
    TObject(Node.Data).Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

