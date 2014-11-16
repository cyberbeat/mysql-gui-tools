unit Main;

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
  TntMenus, TntComCtrls, TntForms, Windows, Messages, SysUtils,
  Variants, Classes, Graphics, Controls, Forms, Options,
  Dialogs, Menus, MySQLConnection, AuxFuncs, ComCtrls, Sections,
  gnugettext, QueryBrowser, Contnrs, StdCtrls, Clipbrd,
  OptionsEditor, QueryBrowserOptionPages, PNGImage, CommonFuncs,
  TntDialogs, SchemaSelection, myx_public_interface, About,
  UniCodeEditor, MySQLCommonFuncs, RegExTextImporter, Types, ExtCtrls,
  AppInstanceMgmt, StdActns, ActnList, XPStyleActnCtrls, ActnMan;

{$include Consts.ini}

type
  TMainForm = class(TTntForm)
    MainMenu: TTntMainMenu;
    FileMI: TTntMenuItem;
    Edit1: TTntMenuItem;
    ToolsMI: TTntMenuItem;
    HelpMI: TTntMenuItem;
    StatusBar: TTntStatusBar;
    ViewMenuItem: TTntMenuItem;
    ChangeDefaultSchemaMI: TTntMenuItem;
    NewInstanceConnectionMI: TTntMenuItem;
    N2: TTntMenuItem;
    ManageConnectionsMI: TTntMenuItem;
    SavecurrentConnectionMI: TTntMenuItem;
    N3: TTntMenuItem;
    N4: TTntMenuItem;
    CloseMI: TTntMenuItem;
    CutMI: TTntMenuItem;
    CopyMI: TTntMenuItem;
    PasteMI: TTntMenuItem;
    OptionsMI: TTntMenuItem;
    LaunchHelpMI: TTntMenuItem;
    N6: TTntMenuItem;
    VisitMySQLcomMI: TTntMenuItem;
    ReportBugMI: TTntMenuItem;
    N7: TTntMenuItem;
    AboutMI: TTntMenuItem;
    OnlineDocsMI: TTntMenuItem;
    OnlineReferenceCAPIMI: TTntMenuItem;
    OnlineDocsPHPAPIMI: TTntMenuItem;
    SelectAllMI: TTntMenuItem;
    NewResultsetTabMI: TTntMenuItem;
    NewSQLScriptTabMI: TTntMenuItem;
    N1: TTntMenuItem;
    PageSetupMI: TTntMenuItem;
    N10: TTntMenuItem;
    PrintMI: TTntMenuItem;
    PrinttoPDFMI: TTntMenuItem;
    MySQLAdministratorMI: TTntMenuItem;
    MySQLCommandLineClientMI: TTntMenuItem;
    N11: TTntMenuItem;
    WindowsCommandLineMI: TTntMenuItem;
    OpenSQLScriptMI: TTntMenuItem;
    N12: TTntMenuItem;
    N13: TTntMenuItem;
    ListreportedBugsMI: TTntMenuItem;
    SwitchMI: TTntMenuItem;
    N5: TTntMenuItem;
    SaveMI: TTntMenuItem;
    OpenQueryMI: TTntMenuItem;
    ReopenMI: TTntMenuItem;
    SaveAsMI: TTntMenuItem;
    CloseTabMI: TTntMenuItem;
    ExportResultsetMI: TTntMenuItem;
    FindMI: TTntMenuItem;
    QuickStartGuideMI: TTntMenuItem;
    N8: TTntMenuItem;
    RegExTextImporterMI: TTntMenuItem;
    N9: TTntMenuItem;
    WindowMI: TTntMenuItem;
    WindowDummyMI: TTntMenuItem;
    N14: TTntMenuItem;
    MainActionManager: TActionManager;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditRedo1: TEditDelete;
    Undo1: TTntMenuItem;
    Redo1: TTntMenuItem;
    N15: TTntMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure PrepareGUI;

    procedure BuildSectionTree;
    procedure DoCurrentSectionChanged(Sender: TObject);
    procedure ShowSection(SectionNr: integer);

    procedure UpdateMenuItems;

    function CreateSectionForm(AOwner: TComponent;
      SidebarSectionType: integer): TSectionForm;
    procedure CloseMIClick(Sender: TObject);
    procedure OptionsMIClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure ReportBugMIClick(Sender: TObject);
    procedure VisitMySQLcomMIClick(Sender: TObject);
    procedure ActivateQueryBrowserSection;
    procedure OnlineDocsMIClick(Sender: TObject);
    procedure OnlineReferenceCAPIMIClick(Sender: TObject);
    procedure OnlineDocsPHPAPIMIClick(Sender: TObject);

    procedure HandleIdle(Sender: TObject; var Done: Boolean);
    procedure FindMIClick(Sender: TObject);
    procedure NewInstanceConnectionMIClick(Sender: TObject);
    procedure ManageConnectionsMIClick(Sender: TObject);
    procedure SavecurrentConnectionMIClick(Sender: TObject);

    procedure CheckFonts;
    procedure NewResultsetTabMIClick(Sender: TObject);
    procedure NewSQLScriptTabMIClick(Sender: TObject);

    procedure MySQLAdministratorMIClick(Sender: TObject);
    procedure MySQLCommandLineClientMIClick(Sender: TObject);
    procedure WindowsCommandLineMIClick(Sender: TObject);
    procedure ChangeDefaultSchemaMIClick(Sender: TObject);
    procedure OpenSQLScriptMIClick(Sender: TObject);
    procedure AboutMIClick(Sender: TObject);
    procedure ListreportedBugsMIClick(Sender: TObject);

    procedure DefaultSchemaChanged(var Message: TMessage); message WM_DefaultSchemaChanged;
    procedure SwitchMIClick(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
    procedure OpenQueryMIClick(Sender: TObject);
    procedure SaveAsMIClick(Sender: TObject);
    procedure QuickStartGuideMIClick(Sender: TObject);
    procedure LaunchHelpMIClick(Sender: TObject);
    procedure CloseTabMIClick(Sender: TObject);
    procedure TntFormActivate(Sender: TObject);
    procedure TntFormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RegExTextImporterMIClick(Sender: TObject);

    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure WMInitMenuPopup(var MSG: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure EditCut1Execute(Sender: TObject);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure EditSelectAll1Execute(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure MainActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure EditRedo1Execute(Sender: TObject);
    procedure EditUndo1Execute(Sender: TObject);
    procedure EMKnowledgeBaseMIClick(Sender: TObject);
  private
    FTextToClear: Boolean;
    FStatusbarClearPNGImg: TPNGObject;
  public
    MySQLConn: TMySQLConn;

    SectionControls: TSectionControls;

    procedure UpdateActions; override;
  end;

const
  SSTQB_QueryBrowser = 1;

var
  MainForm: TMainForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ApplicationDataModule, MyxError, PNGTools;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  if (Not(MYXCommonOptions.NoRegistry)) then
    RegisterMySQLApplication(Application.Title,
      product_version, ExtractFilePath(Application.ExeName));

  Application.Title := _('MySQL Query Browser');

  if (not (ApplicationDM.QBOptions.RestoreWindowPos(self))) then
  begin
    if (Screen.Width >= 1024) then
    begin
      Width := 1024;
      Height := 768 - 48;

      Left := (Screen.Width + Width) div 2 - Width;
      Top := 0;
    end
    else
    begin
      Width := 800;
      Height := 572;

      Left := (Screen.Width + Width) div 2 - Width;
      Top := 0;

      WindowState := wsMaximized;
    end;
  end;

  MainMenu.AutoHotkeys := maAutomatic;

  MySQLConn := TMySQLConn.Create(StatusBar);
  MySQLConn.AllowConnectionSkip := False;

  SectionControls := TSectionControls.Create(self,
    CreateSectionForm,
    nil,
    True,
    0,
    nil,
    1,
    False,
    False);

  Application.OnIdle := HandleIdle;
  Application.HintHidePause := 1000000;

  CheckFonts;

  // Enable Tools Menu Items
  MySQLAdministratorMI.Enabled := (GetMySQLAdministratorCmd <> '');
  MySQLCommandlineclientMI.Enabled := (GetMySQLCommandLineClientPath <> '');
  WindowsCommandLineMI.Enabled := FileExists(GetSystemDir + 'cmd.exe');

  FTextToClear := False;
  FStatusbarClearPNGImg := LoadPNGImageFromResource('statusbar_clear');
  StatusBar.Font.Size := ((MYXCommonOptions.DefaultFontHeight * 10) div 12);
  StatusBar.Font.Color := clGray;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  ApplicationDM.QBOptions.AddWindowPos(self);

  FStatusbarClearPNGImg.Free;

  // Shutdown embedded server
  if (ApplicationDM.QBOptions.Embedded) then
    myx_mysql_embedded_shutdown;

  ApplicationDM.Free;
  MYXCommonOptions := nil;
  MYXCommonOptionProvider := nil; // Sets the ref count to 0 and frees the class.

  // Explicitely free the sections here to cause all sub forms to get destroyed, as they might access the
  // connection instance a last time.
  SectionControls.Free;

  MySQLConn.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);

var
  i: integer;

begin
  try
    ApplicationDM.ApplicationIsTerminating := True;

    //Close all Sub-windows
    for i := 0 to SectionControls.SectionList.Count - 1 do
      if (TSidebarSection(SectionControls.SectionList[i]).SectionForm <> nil) then
        TSidebarSection(SectionControls.SectionList[i]).SectionForm.Close;
  except
    //Allow close even when there was an exception
  end;

  Action := caFree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TntFormCloseQuery(Sender: TObject; var CanClose: Boolean);

var
  i: integer;

begin
  //try
    //Check if all Sub-Windows can be closed
  for i := 0 to SectionControls.SectionList.Count - 1 do
    if (TSidebarSection(SectionControls.SectionList[i]).SectionForm <> nil) and
      (Assigned(TSidebarSection(SectionControls.SectionList[i]).SectionForm.OnCloseQuery)) then
    begin
      TSidebarSection(SectionControls.SectionList[i]
        ).SectionForm.OnCloseQuery(self, CanClose);

      if not (CanClose) then
        break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PrepareGUI;

begin
  Caption := _('MySQL Query Browser') + ' - ' + MySQLConn.GetConnectionCaption;

  //Build the Section List
  BuildSectionTree;

  ShowSection(0);

  UpdateMenuItems;

  if (ApplicationDM.QBOptions.AssociateFileExtensions) then
  begin
    RegisterFileType('qbquery', Application.ExeName, 1, '"%1"');
    RegisterFileType('sql', Application.ExeName, 2, '"%1"');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.BuildSectionTree;

begin
  SectionControls.AddSection(_('Query Browser'), TSidebarSection.Create(SSTQB_QueryBrowser, 0, 1, True, False));
  SectionControls.OnCurrentSectionChanged := DoCurrentSectionChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditCopy1Execute(Sender: TObject);

begin
    if Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.ActiveControl) then
    SendMessage(Screen.ActiveForm.ActiveControl.Handle, WM_COPY, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditCut1Execute(Sender: TObject);

begin
    if Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.ActiveControl) then
    SendMessage(Screen.ActiveForm.ActiveControl.Handle, WM_Cut, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditPaste1Execute(Sender: TObject);

begin
  if Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.ActiveControl) then
    SendMessage(Screen.ActiveForm.ActiveControl.Handle, WM_Paste, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditRedo1Execute(Sender: TObject);

begin
  // This action can only appear if this form is active.
  if ActiveControl is TUniCodeEdit then
    TUniCodeEdit(ActiveControl).Redo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditSelectAll1Execute(Sender: TObject);

begin
  // This action can only appear if this form is active.
  if ActiveControl is TUniCodeEdit then
    TUniCodeEdit(ActiveControl).SelectAll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.EditUndo1Execute(Sender: TObject);

begin
  // This action can only appear if this form is active.
  if ActiveControl is TUniCodeEdit then
    TUniCodeEdit(ActiveControl).Undo;
end;

procedure TMainForm.EMKnowledgeBaseMIClick(Sender: TObject);
begin
  BrowseWebPage('https://kb.mysql.com/index.php');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DoCurrentSectionChanged(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    MainMenu.Merge(TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).Menu);

  StatusBar.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ShowSection(SectionNr: integer);

begin
  if (SectionNr < SectionControls.SectionList.Count) then
    SectionControls.ShowSidebarSection(SectionControls.SectionList[SectionNr] as TSidebarSection);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMainForm.CreateSectionForm(AOwner: TComponent; SidebarSectionType: integer): TSectionForm;

begin
  Result := nil;

  case SidebarSectionType of
    SSTQB_QueryBrowser:
      Result := TQueryBrowserForm.Create(AOwner, MySQLConn, StatusBar);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.UpdateMenuItems;

begin
  if (SectionControls <> nil) then
    if (SectionControls.CurrentSidebarSection <> nil) then
      if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
      begin
        with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
        begin
          self.ExportResultsetMI.Clear;
          AddResultsetExporterMenuItems(self.ExportResultsetMI);
        end;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseMIClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OptionsMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  OptionsForm := TOptionsForm.Create(self, TQueryBrowserOptionPagesForm.Create(self), MySQLConn.MySQL);
  try
    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  NewCursor: TCursor;

begin
  if (Key = VK_F1) then
    ShowHelp;

  if ((Key = Ord('I')) and (Shift = [ssCtrl])) then
  begin
    RegExTextImporterMIClick(self);
    Key := 0;
  end;

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
  begin
    if (Shift = []) then
    begin
      with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
      begin
        if (Key = VK_F12) then
        begin
          ApplicationDM.QBOptions.OnlyTabsheets :=
            not (ApplicationDM.QBOptions.OnlyTabsheets);

          DoOptionsChanged;

          Key := 0;
        end
        else
          if (Key = VK_F11) then
          begin
            SQLEditorMaximized := not (SQLEditorMaximized);

            DoOptionsChanged;

            Key := 0;
          end;
      end;
    end
    else
    begin
      if (Shift = [ssAlt, ssCtrl]) {or
      (CurrentControl=TQueryBrowserForm(
        SectionControls.CurrentSidebarSection.SectionForm
          ).SchemataFrame.CatalogVST.Handle)}then
      begin
        if (Key = Ord('X')) then
          NewCursor := crDefault
        else
          if (Key = Ord('S')) then
            NewCursor := crSQLSelect
          else
            if (Key = Ord('F')) then
              NewCursor := crSQLFrom
            else
              if (Key = Ord('W')) then
                NewCursor := crSQLWhere
              else
                if (Key = Ord('G')) then
                  NewCursor := crSQLGroup
                else
                  if (Key = Ord('H')) then
                    NewCursor := crSQLHaving
                  else
                    if (Key = Ord('O')) then
                      NewCursor := crSQLOrder
                    else
                      if (Key = Ord('T')) then
                        NewCursor := crSQLSet
                      else
                        NewCursor := crNo;

        if (NewCursor <> crNo) then
        begin
          MessageToAllForms(WM_CursorChanged, NewCursor, 0);
          Key := 0;
        end;

        if (Key = VK_Return) then
        begin                                
          TQueryBrowserForm(
            SectionControls.CurrentSidebarSection.SectionForm
            ).QuerySplitAndExecuteClick(self);
        end;
      end
      else
      begin
        if (ssAlt in Shift) then
        begin
          with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
          begin
        //History Nav
            if (Key = VK_LEFT) then
              QueryGoBackClick(self);

            if (Key = VK_Right) then
              QueryNextClick(self);

        //Jump to specific Tab
            if (Key >= Ord('1')) and (Key <= Ord('9')) then
            begin
              DoSetActiveRSTab(Key - Ord('1'));
              Key := 0;
            end;
          end;
        end
        else
        begin
          if (ssCtrl in Shift) then
          begin
            with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
            begin
              if (Key = 17) then
                ShowResultsetHits;

              if (Key = Ord('N')) then
              begin
                ClearQueryEditorMIClick(self);
                Key := 0;
              end;

              if (Key = Ord('O')) then
              begin
                OpenScript('', ssShift in Shift);
                Key := 0;
              end;

              if (Key = Ord('S')) then
              begin
                Save;
                Key := 0;
              end;

              if (Key = Ord('T')) then
              begin
                if ssShift in Shift then
                  AddNewScriptTabMIClick(self)
                else
                  AddNewRSTabMIClick(self);
                Key := 0;
              end;

              if (Key = Ord('L')) then
              begin
                SQLEditor.SetFocus;
                SQLEditor.SelectAll;
              end;

              if (Key = Ord('B')) and (CurrentPerspective = apResultSet) then
              begin
                AddBookmarkMIClick(self);
                Key := 0;
              end;

              if (Key = Ord('W')) then
              begin
                // Close tab
                MainTabHeaderFrame.DeleteTab(MainTabHeaderFrame.SelectedTab);
                Key := 0;
              end;

              // Move to next tab.
              if (Key = VK_TAB) or (Key = VK_NEXT) then
              begin
                if ssShift in Shift then
                  MainTabHeaderFrame.SelectPreviousTabSheet
                else
                  MainTabHeaderFrame.SelectNextTabSheet;
                Key := 0;
              end;

              if (Key = VK_PRIOR) then
              begin
                MainTabHeaderFrame.SelectPreviousTabSheet;
                Key := 0;
              end;

              if (Key = Ord('0')) then
              begin
                DoSetActiveRSTab(10);
                Key := 0;
              end;
            end;
          end
        end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TntFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
  begin
    with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
    begin
      if (Key = 17) then
        HideResultsetHits;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ReportBugMIClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.VisitMySQLcomMIClick(Sender: TObject);

begin
  BrowseWebPage('http://www.mysql.com');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ActivateQueryBrowserSection;

var
  i: integer;

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType <> SSTQB_QueryBrowser) then
  begin
    for i := 0 to SectionControls.SectionList.Count - 1 do
      if (TSidebarSection(SectionControls.SectionList[i]).SidebarSectionType = SSTQB_QueryBrowser) then
      begin
        SectionControls.ShowSidebarSection(TSidebarSection(SectionControls.SectionList[i]));
        break;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlineDocsMIClick(Sender: TObject);

begin
  ActivateQueryBrowserSection;

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).ShowHelpTabSheet(
      'http://dev.mysql.com/doc/mysql/en/index.html');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlineReferenceCAPIMIClick(Sender: TObject);

begin
  ActivateQueryBrowserSection;

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).ShowHelpTabSheet(
      'http://dev.mysql.com/doc/mysql/en/C.html');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OnlineDocsPHPAPIMIClick(Sender: TObject);

begin
  ActivateQueryBrowserSection;

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).ShowHelpTabSheet(
      'http://www.php.net/manual/en/ref.mysqli.php');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FindMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).FindMIClick(Sender);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.HandleIdle(Sender: TObject; var Done: Boolean);

var
  Enable: Boolean;
  I: Integer;

begin
  Enable := IsConnectedToInternet;
  OnlineDocsMI.Enabled := Enable;
  OnlineReferenceCAPIMI.Enabled := Enable;
  OnlineDocsPHPAPIMI.Enabled := Enable;
  ReportBugMI.Enabled := Enable;
  VisitMySQLcomMI.Enabled := Enable;

  SaveMI.Enabled := False;
  SaveAsMI.Enabled := False;
  CloseTabMI.Enabled := False;
  if (SectionControls <> nil) then
    if (SectionControls.CurrentSidebarSection <> nil) then
      if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
      begin
        with TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm) do
        begin
          if (CurrentPerspective = apScript) and Assigned(ActiveScriptPanel) then
          begin
            SaveMI.Enabled := True;
            SaveAsMI.Enabled := True;
          end
          else
            if (CurrentPerspective = apResultset) then
            begin
              if (SQLEditor.Content.Count > 0) then
                SaveAsMI.Enabled := True;

              if Assigned(ActiveResultset) then
              begin
                for I := 0 to self.ExportResultsetMI.Count - 1 do
                  self.ExportResultsetMI.Items[i].Enabled := (ActiveResultset.RowCount > 0);
              end
              else
                for I := 0 to self.ExportResultsetMI.Count - 1 do
                  self.ExportResultsetMI.Items[i].Enabled := False;
            end;

          CloseTabMI.Enabled := (MainTabHeaderFrame.TabCount > 1);

          for i := 0 to ScriptMI.Count - 1 do
            if (ScriptMI.Items[i].Tag = 1) then
              ScriptMI.Items[i].Enabled := (CurrentPerspective = apScript);

          if (MySQLConn <> nil) then
            if (MySQLConn.MajorVersion < 5) then
            begin
              CreateStoredProcedureFunctionMI.Enabled := False;
              EditAllStoredProceduresFunctionsMI.Enabled := False;
            end
            else
            begin
              CreateStoredProcedureFunctionMI.Enabled := True;
              EditAllStoredProceduresFunctionsMI.Enabled := True;
            end;
        end;
      end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.NewInstanceConnectionMIClick(Sender: TObject);

begin
  CreateSubProcess(Application.ExeName, '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MainActionManagerUpdate(Action: TBasicAction; var Handled: Boolean);

var
  Enable: Boolean;
  Control: TCustomUnicodeEdit;
  CurrentControl: TWinControl;

begin
  if Assigned(Screen.ActiveForm) and Assigned(Screen.ActiveForm.ActiveControl) then
  begin
    CurrentControl := Screen.ActiveForm.ActiveControl;
    if CurrentControl is TCustomUnicodeEdit then
    begin
      Control := TCustomUniCodeEdit(CurrentControl);
      Enable := Control.SelectionAvailable;
      EditCut1.Enabled := Enable;
      EditCopy1.Enabled := Enable;
      EditPaste1.Enabled := Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT);
      EditSelectAll1.Enabled := Control.Content.Count > 0;
      EditUndo1.Enabled := Control.CanUndo;
      EditRedo1.Enabled := Control.CanRedo;
    end
    else
    begin
      if (CurrentControl is TCustomEdit) or (CurrentControl is TCustomCombobox) then
      begin
        // There is no standard way for all edit controls to get a hint about
        // an available selection. So we enable cut and copy anyway.
        EditCut1.Enabled := True;
        EditCopy1.Enabled := True;
        EditPaste1.Enabled := Clipboard.HasFormat(CF_TEXT) or Clipboard.HasFormat(CF_UNICODETEXT);
        EditSelectAll1.Enabled := False;
        EditUndo1.Enabled := False;
        EditRedo1.Enabled := False;
      end;
    end;
  end
  else
  begin
    EditCut1.Enabled := False;
    EditCopy1.Enabled := False;
    EditPaste1.Enabled := False;
    EditSelectAll1.Enabled := False;
    EditUndo1.Enabled := False;
    EditRedo1.Enabled := False;
  end;

  Handled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ManageConnectionsMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  OptionsForm := TOptionsForm.Create(self, TQueryBrowserOptionPagesForm.Create(self), MySQLConn.MySQL);
  try
    //Select Connections Page
    OptionsForm.ShowOptionPage(ConnectionsPage);

    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMainForm.SavecurrentConnectionMIClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;

begin
  OptionsForm := TOptionsForm.Create(self, TQueryBrowserOptionPagesForm.Create(self), MySQLConn.MySQL);
  try
    //Select Connections Page
    OptionsForm.ShowOptionPage(ConnectionsPage);

    OptionsForm.AddConnection(MySQLConn.UserConnection);
    OptionsForm.ActiveControl := OptionsForm.ConnectionEd;

    OptionsForm.ShowModal;
  finally
    OptionsForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CheckFonts;

var
  I: integer;
  FontsInstalled: Boolean;

begin
  FontsInstalled := False;
  for I := 0 to Screen.Fonts.Count - 1 do
    if (CompareText(Screen.Fonts[I], 'Bitstream Vera Sans Mono') = 0) then
      FontsInstalled := True;

  if not FontsInstalled then
  begin
    if (not (FileExists(ExtractFilePath(Application.ExeName) + 'fonts\VeraMoBd.ttf'))) then
      raise EInOutError.Create(Format(_('The Bitstream Vera Sans Mono fonts cannot be found. Please download ' +
        'the font files from http://www.gnome.org/fonts and copy them into the fonts directory %s. The following files are needed: VeraMoBd.ttf, ' +
        'VeraMoBI.ttf, VeraMoIt.ttf, VeraMono.ttf.'),
        [ExtractFilePath(Application.ExeName) + 'fonts\']));

    if not FileExists(GetWindowsDir + 'fonts\VeraMoBd.ttf') then
    begin
      CopyDiskFile(ExtractFilePath(Application.ExeName) + 'fonts\VeraMoBd.ttf', GetWindowsDir + 'fonts\VeraMoBd.ttf',
        False);
      AddFontResourceW(PWideChar(GetWindowsDir + 'fonts\VeraMoBd.ttf'));
    end;

    if not FileExists(GetWindowsDir + 'fonts\VeraMoBI.ttf') then
    begin
      CopyDiskFile(ExtractFilePath(Application.ExeName) + 'fonts\VeraMoBI.ttf', GetWindowsDir + 'fonts\VeraMoBI.ttf',
      False);
    end;

    if not FileExists(GetWindowsDir + 'fonts\VeraMoIt.ttf') then
    begin
      CopyDiskFile(ExtractFilePath(Application.ExeName) + 'fonts\VeraMoIt.ttf', GetWindowsDir + 'fonts\VeraMoIt.ttf',
      False);
    end;

    if not FileExists(GetWindowsDir + 'fonts\VeraMono.ttf') then
    begin
      CopyDiskFile(ExtractFilePath(Application.ExeName) + 'fonts\VeraMono.ttf', GetWindowsDir + 'fonts\VeraMono.ttf',
      False);
    end;

    PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);

    Screen.ResetFonts;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.NewResultsetTabMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType <> SSTQB_QueryBrowser) then
    ShowSection(0);

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).AddNewRSTabMIClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.NewSQLScriptTabMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType <> SSTQB_QueryBrowser) then
    ShowSection(0);

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).AddNewScriptTabMIClick(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLAdministratorMIClick(Sender: TObject);

  //---------------------------------------------------------------------------

  function ConvertToHex(const S: WideString): string;

  var
    Utf8String: string;
    
  begin
    Utf8String := Utf8Encode(S);
    SetLength(Result, 2 * Length(Utf8String));
    BinToHex(PChar(Utf8String), PChar(Result), Length(Utf8String));
  end;

  //---------------------------------------------------------------------------

var
  cmd: WideString;

begin
  cmd := GetMySQLAdministratorCmd;
  if cmd <> '' then
  begin
    if (MySQLConn.UserConnection <> nil) then
    begin
      cmd := cmd + ' "-u' + MySQLConn.UserConnection.username + '"';
      if MySQLConn.UserConnection.password <> '' then
        cmd := cmd + ' -x' + ConvertToHex(MySQLConn.UserConnection.password);
      cmd := cmd + ' "-h' + MySQLConn.UserConnection.hostname + '"';
      cmd := cmd + ' "-P' + IntToStr(MySQLConn.UserConnection.port) + '"';
    end;

    CreateSubProcess(cmd, '');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.MySQLCommandLineClientMIClick(Sender: TObject);

var
  cmd: WideString;

begin
  cmd := GetMySQLCommandLineClientPath;
  if (cmd <> '') then
    CreateSubProcess(cmd + ' -h' + MySQLConn.UserConnection.hostname +
      ' -u' + MySQLConn.UserConnection.username +
      ' -p' + MySQLConn.UserConnection.password +
      ' -P' + IntToStr(MySQLConn.UserConnection.port), '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WindowsCommandLineMIClick(Sender: TObject);

var
  cmd: WideString;

begin
  cmd := GetSystemDir + 'cmd.exe';
  if (FileExists(cmd)) then
    CreateSubProcess(cmd, GetHomeDir);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ChangeDefaultSchemaMIClick(Sender: TObject);

var
  SchemaSelectionForm: TSchemaSelectionForm;

begin
  SchemaSelectionForm := TSchemaSelectionForm.Create(self, MySQLConn);
  try
    if (SchemaSelectionForm.ShowModal = mrOK) then
    begin
      if (SchemaSelectionForm.SelectedSchema <> nil) then
      begin
        MySQLConn.DefaultSchema := SchemaSelectionForm.SelectedSchema.schema_name;
        MySQLConn.UserConnection.schema := SchemaSelectionForm.SelectedSchema.schema_name;
        Application.MainForm.Caption := _('MySQL Query Browser') + ' - ' + MySQLConn.GetConnectionCaption;
      end;
    end;
  finally
    SchemaSelectionForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OpenQueryMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
  begin
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).OpenQuery;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.OpenSQLScriptMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
  begin
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).OpenScript('', True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.AboutMIClick(Sender: TObject);

begin
  ShowAboutDialog('MySQL Query Browser',
    product_version + ' ' + product_build_level,
    'Michael G. Zinner, graphical design, ' +
    'Windows development, library coding | ' +
    'Alfredo Kengi Kojima, Linux development, library coding | ' +
    'Mike Lischke, Windows development, library coding | ' +
    'Victor Vagin, library coding, QA | ' +
    'Ulrich Bayer, library coding, WIX | ' +
    'Brian Aker, main concept | ' +
    'Mike Hillyer, documentation');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ListreportedBugsMIClick(Sender: TObject);

begin
  BrowseWebPage('http://bugs.mysql.com/search.php?search_for=&limit=All&order_by=&direction=ASC&cmd=display&status=Active&severity=&showstopper=&bug_type=MySQL+Query+Browser&php_os=&phpver=&assign=&bug_age=0');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.DefaultSchemaChanged(var Message: TMessage);

begin
  if MySQLConn.GetConnectionCaption <> '' then
    MainForm.Caption := _('MySQL Query Browser') + ' - ' + MySQLConn.GetConnectionCaption
  else
    MainForm.Caption := _('MySQL Query Browser');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SwitchMIClick(Sender: TObject);

begin
  // Connect using a different connection (same instance).
  if MySQLConn.ConnectToServer(True, True, True) = 1 then
  begin
    PrepareGUI;
    QueryBrowserForm.SchemataFrame.ReloadSchemaTree;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SaveMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
  begin
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).Save;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.SaveAsMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SectionForm is TQueryBrowserForm) then
  begin
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).SaveAs;
  end;

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.QuickStartGuideMIClick(Sender: TObject);

begin
  ActivateQueryBrowserSection;

  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
    TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).ShowQuickStartGuide;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.LaunchHelpMIClick(Sender: TObject);

begin
  ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CloseTabMIClick(Sender: TObject);

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
  begin
    with TQueryBrowserForm(
      SectionControls.CurrentSidebarSection.SectionForm).MainTabHeaderFrame do
    begin
      if (TabCount > 1) then
        DeleteTab(SelectedTab);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TntFormActivate(Sender: TObject);

begin
  DoCurrentSectionChanged(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Redo1Click(Sender: TObject);

begin
  if ActiveControl is TUnicodeEdit then
    TUnicodeEdit(ActiveControl).Redo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.RegExTextImporterMIClick(Sender: TObject);

begin
  if RegExTextImporterForm = nil then
  begin
    RegExTextImporterForm := TRegExTextImporterForm.Create(Self);
    RegExTextImporterForm.MySQLConn := MySQLConn;
  end;

  RegExTextImporterForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);

begin
  if (FTextToClear) then
    FStatusbarClearPNGImg.Draw(StatusBar.Canvas,
      Types.Rect(StatusBar.Panels[0].Width + 4, 4,
      StatusBar.Panels[0].Width + 4 + FStatusbarClearPNGImg.Width,
      4 + FStatusbarClearPNGImg.Height));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StatusBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  if (X > StatusBar.Panels[0].Width) and
    (X < StatusBar.Panels[0].Width + StatusBar.Panels[1].Width) then
  begin
    StatusBar.Panels[2].Text := '';
    FTextToClear := False;
    StatusBar.Invalidate;
    StatusBar.Update;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Undo1Click(Sender: TObject);

begin
  if ActiveControl is TUnicodeEdit then
    TUnicodeEdit(ActiveControl).Undo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.UpdateActions;

begin
  if (SectionControls.CurrentSidebarSection.SidebarSectionType = SSTQB_QueryBrowser) then
  begin
    TQueryBrowserForm(SectionControls.CurrentSidebarSection.SectionForm).UpdateActions;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.WMInitMenuPopup(var MSG: TWMInitMenuPopup);

begin
  if (Msg.MenuPopup = WindowMI.Handle) then
    BuildRegisterApplicationMenuItems(WindowMI);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

