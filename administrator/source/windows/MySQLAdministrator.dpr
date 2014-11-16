program MySQLAdministrator;

{$R '..\..\res\windows\mysqladmin.res' '..\..\res\windows\mysqladmin.rc'}
{$R '..\..\..\common\res\windows\mysqlcommon.res' '..\..\..\common\res\windows\mysqlcommon.rc'}
{$R '..\..\..\common\res\windows\Manifest.res' '..\..\..\common\res\windows\Manifest.rc'}
{$R '..\..\res\windows\MySQLAdministrator.res' '..\..\res\windows\MySQLAdministrator.rc'}
{%File '..\..\PrepareApplicationFolder.cmd'}

uses
  Instance in '..\..\..\common\source\windows\Instance.pas',
  gnugettext in '..\..\..\common\source\windows\gnugettext.pas',
  SysUtils,
  Forms,
  Main in 'Main.pas' {MainForm},
  ApplicationDataModule in 'ApplicationDataModule.pas' {ApplicationDM: TDataModule},
  AdditionalClasses in 'AdditionalClasses.pas',
  AdvancedEdit in '..\..\..\common\source\windows\AdvancedEdit.pas' {AdvancedEditFrame: TTntFrame},
  AuxFuncs in '..\..\..\common\source\windows\AuxFuncs.pas',
  AuxLists in '..\..\..\common\source\windows\AuxLists.pas',
  MySQLConnection in '..\..\..\common\source\windows\MySQLConnection.pas',
  ConnectToInstance in '..\..\..\common\source\windows\ConnectToInstance.pas' {ConnectToInstanceForm},
  SchemataTreeView in '..\..\..\common\source\windows\SchemataTreeView.pas' {SchemataFrame: TTntFrame},
  Sections in '..\..\..\common\source\windows\Sections.pas',
  InstanceSections in '..\..\..\common\source\windows\InstanceSections.pas',
  OptionsEditor in '..\..\..\common\source\windows\OptionsEditor.pas' {OptionsForm},
  Options in '..\..\..\common\source\windows\Options.pas',
  AdminReplication in 'AdminReplication.pas' {AdminReplicationForm},
  AdminServerInfo in 'AdminServerInfo.pas' {AdminServerInfoForm},
  AdminServiceControl in 'AdminServiceControl.pas' {AdminServiceControlForm},
  AdminUsers in 'AdminUsers.pas' {AdminUsersForm},
  AdminServerHealth in 'AdminServerHealth.pas' {AdminServerHealthForm},
  AdminServerConnections in 'AdminServerConnections.pas' {AdminServerConnectionsForm},
  AdminCatalog in 'AdminCatalog.pas' {AdminCatalogForm},
  AdminServerLogs in 'AdminServerLogs.pas' {AdminServerLogsForm},
  AdminStartupVariables in 'AdminStartupVariables.pas' {AdminStartupVariablesForm},
  AdminStartupVariablesInnoDBDatafiles in 'AdminStartupVariablesInnoDBDatafiles.pas' {AdminStartupVariablesInnoDBDatafilesForm},
  AdminStartupVariablesOptionFile in 'AdminStartupVariablesOptionFile.pas' {AdminStartupVariablesOptionFileForm},
  AdminServerHealthLineGraph in 'AdminServerHealthLineGraph.pas' {AdminServerHealthLineGraphFrame: TTntFrame},
  AdminServerHealthBarGraph in 'AdminServerHealthBarGraph.pas' {AdminServerHealthBarGraphFrame: TTntFrame},
  AdminRestore in 'AdminRestore.pas' {AdminRestoreForm},
  AdminBackupProgress in 'AdminBackupProgress.pas' {AdminBackupProgressForm},
  AuxAdminBackupRestore in 'AuxAdminBackupRestore.pas',
  AdminCatalogTableCheck in 'AdminCatalogTableCheck.pas' {AdminCatalogTableCheckForm},
  Progress in '..\..\..\common\source\windows\Progress.pas' {ProgressForm},
  About in '..\..\..\common\source\windows\About.pas' {AboutForm},
  MyxError in '..\..\..\common\source\windows\MyxError.pas',
  AdminOptionPages in 'AdminOptionPages.pas' {AdminOptionPagesForm},
  AdminServerHealthGraphSettings in 'AdminServerHealthGraphSettings.pas' {AdminServerHealthGraphSettingsForm},
  ScheduleAPI in '..\..\..\common\source\windows\ScheduleAPI.pas',
  AdminService in 'AdminService.pas',
  EditorTable in '..\..\..\common\source\windows\EditorTable.pas' {EditorTableForm},
  EditorTableVTFKEdit in '..\..\..\common\source\windows\EditorTableVTFKEdit.pas',
  EditorTableVTEdit in '..\..\..\common\source\windows\EditorTableVTEdit.pas',
  CommonFuncs in '..\..\..\common\source\windows\CommonFuncs.pas',
  AdminRestoreProgress in 'AdminRestoreProgress.pas' {AdminRestoreProgressForm: TTntForm},
  AuxApplicationFuncs in '..\..\..\common\source\windows\AuxApplicationFuncs.pas',
  NameEditor in 'NameEditor.pas' {NameEditorForm},
  MySQLCommonFuncs in '..\..\..\common\source\windows\MySQLCommonFuncs.pas',
  Password in 'Password.pas' {PasswordDialog},
  AppInstanceMgmt in '..\..\..\common\source\windows\AppInstanceMgmt.pas',
  EditorSql in '..\..\..\common\source\windows\EditorSql.pas' {EditorSqlForm},
  myx_public_interface in '..\..\..\common\library\public-interface\myx_public_interface.pas',
  myx_util_public_interface in '..\..\..\common\library\public-interface\myx_util_public_interface.pas',
  myx_admin_public_interface in 'myx_admin_public_interface.pas',
  AdminBackup in 'AdminBackup.pas' {AdminBackupForm},
  TestMonkeySnoop in '..\..\..\common\source\windows\Snoop\TestMonkeySnoop.pas',
  myx_sql_parser_public_interface in '..\..\..\common\library\public-interface\myx_sql_parser_public_interface.pas';

var
  ConnectionResult: Integer;
  LocalePath: string;

begin
  //Init the global vars
  InitGlobalVars;
  LocalePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'locale';

  //Create Common Options
  MYXCommonOptions := TMYXCommonOptions.Create('administrator');
  MYXCommonOptionProvider := MYXCommonOptions; // Increases the reference count to 1. This is necessary as we for a short
                                               // period of time need both the interface and the implementing class.
                                               // Once we switch entirely to the interface that problem is gone.
                                               // Without this assignment the class is automatically freed on next access
                                               // so that the access after that crashs badly.

  // Bind to our own text domain instead of default.
  bindtextdomain('administrator', LocalePath);
  textdomain('administrator');

  // Add extra domain for runtime library translations
  AddDomainForResourceString('administrator');

  // Force program to use user defined instead of the current Windows settings
  if (MYXCommonOptions.Language <> '-') then
    UseLanguage(MYXCommonOptions.Language);


  //Create Application Data Module
  ApplicationDM := TApplicationDM.Create(nil);

  ApplicationDM.LoadOptions;

  //Application Title
  Application.Title:='MySQL Administrator';
  Application.HelpFile:='';
  Application.Initialize;
  Application.HintHidePause := 10000; // 10 seconds for a hint to stay.

  //Check files
  if(Not(ApplicationDM.CheckFiles))then
    Exit;

  //Check Common Commandline parameter
  CheckCommonCommandlineParameter;

  //Check Commandline parameter
  ApplicationDM.CheckCommandlineParameter;

  if(ApplicationDM.Options.BackupProfile<>'')then
  begin
    ApplicationDM.MakeBackup;

    Exit;
  end;

  //Create Main Form
  Application.CreateForm(TMainForm, MainForm);
  if not ApplicationDM.Options.ShowOnlyServiceSections then
    ConnectionResult := ApplicationDM.PrepareConnection
  else
    ConnectionResult := -1;

  if (ConnectionResult = 1) or ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    RegisterApplication(Application, ApplicationDM.CurrentConnection.GetConnectionCaption(False));
    try
      MainForm.PrepareGUI;

      Application.Run;
    finally
      UnregisterApplication(Application);
    end;
  end;

  // We don't need to free the options class. It is reference-counted and will automatically be freed.

  // Save the list of translations not yet found in our po files.
  {$ifopt d+}
  SaveTrackedTranslations(LocalePath);
  {$endif}
end.
