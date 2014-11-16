program MySQLMigrationTool;

{$R '..\..\..\common\res\windows\mysqlcommon.res' '..\..\..\common\res\windows\mysqlcommon.rc'}
{$R '..\..\..\common\res\windows\Manifest.res' '..\..\..\common\res\windows\Manifest.rc'}
{$R '..\..\res\windows\mysqlmigration.res' '..\..\res\windows\mysqlmigration.rc'}
{%File '..\..\PrepareApplicationFolder.cmd'}

uses
  Forms,
  SysUtils,
  Main in 'Main.pas' {MainForm},
  AuxFuncs in '..\..\..\common\source\windows\AuxFuncs.pas',
  PNGTools in '..\..\..\common\source\windows\tools\PNGTools.pas',
  gnugettext in '..\..\..\common\source\windows\gnugettext.pas',
  Sections in '..\..\..\common\source\windows\Sections.pas',
  MyxError in '..\..\..\common\source\windows\MyxError.pas',
  myx_public_interface in '..\..\..\common\library\public-interface\myx_public_interface.pas',
  myx_grt_public_interface in '..\..\..\common\library\public-interface\myx_grt_public_interface.pas',
  myx_grt_builtin_module_public_interface in '..\..\..\common\library\public-interface\myx_grt_builtin_module_public_interface.pas',
  AdvancedEdit in '..\..\..\common\source\windows\AdvancedEdit.pas' {AdvancedEditFrame: TFrame},
  ExecutionPlan in 'ExecutionPlan.pas',
  WizardHeader in 'WizardHeader.pas' {WizardHeaderFrame: TFrame},
  WizardBottom in 'WizardBottom.pas' {WizardBottomFrame: TFrame},
  JdbcDBConns in 'JdbcDBConns.pas',
  MigrationSourceTarget in 'MigrationSourceTarget.pas' {MigrationSourceTargetForm},
  MigrationObjSel in 'MigrationObjSel.pas' {MigrationObjSelForm},
  MigrationObjSelFilter in '..\..\..\common\source\windows\MigrationObjSelFilter.pas' {MigrationObjSelFilterFrame: TFrame},
  MigrationObjMap in 'MigrationObjMap.pas' {MigrationObjMapForm},
  MigrationObjMapDetail in '..\..\..\common\source\windows\MigrationObjMapDetail.pas' {MigrationObjMapDetailFrame: TFrame},
  MigrationObjManEdit in 'MigrationObjManEdit.pas' {MigrationObjManEditForm},
  MigrationCreateObjs in 'MigrationCreateObjs.pas' {MigrationCreateObjsForm},
  MigrationDataMap in 'MigrationDataMap.pas' {MigrationDataMapForm},
  MigrationBulkTransfer in 'MigrationBulkTransfer.pas' {MigrationBulkTransferForm},
  MigrationReport in 'MigrationReport.pas' {MigrationReportForm},
  About in '..\..\..\common\source\windows\About.pas' {AboutForm: TTntForm},
  CommonFuncs in '..\..\..\common\source\windows\CommonFuncs.pas',
  Grt in '..\..\..\common\source\windows\Grt.pas',
  XGrtShell in '..\..\..\common\source\windows\xgrtsh\XGrtShell.pas' {XGrtShellForm: TTntForm},
  WizardInterface in '..\..\..\common\source\windows\WizardInterface.pas',
  WizardPage in '..\..\..\common\source\windows\WizardPage.pas',
  GrtSections in '..\..\..\common\source\windows\GrtSections.pas',
  MyxBaseForm in '..\..\..\common\source\windows\MyxBaseForm.pas',
  MyxConnectionDialog in '..\..\..\common\source\windows\MyxConnectionDialog.pas' {MyxConnectionDialogForm},
  MyxOptions in '..\..\..\common\source\windows\MyxOptions.pas',
  MyxAppOptions in 'MyxAppOptions.pas',
  AppInstanceMgmt in '..\..\..\common\source\windows\AppInstanceMgmt.pas',
  myx_util_public_interface in '..\..\..\common\library\public-interface\myx_util_public_interface.pas';

{$R *.res}

var
  SplashScreen: TAboutForm;

begin
  // Mask out certain FPU exceptions, which the JVM is not capable to catch.
  Set8087CW($133F);

  // Bind to our own text domain instead of default.
  bindtextdomain('migration-tool', IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'locale');
  textdomain('migration-tool');

  // Add extra domain for runtime library translations
  AddDomainForResourceString('migration-tool');

  // Force program to use user defined instead of the current Windows settings
  UseLanguage('en');//MYXCommonOptions.Language);

  //Application Title
  Application.Title := 'MySQL Migration Tool';
  Application.HelpFile := '';
  Application.Initialize;

  if (RegisterApplication(Application, '', True)) then
  begin
    try
      // Display splashscreen
      if (Not(GetBoolOptionFromCommandLine('nologo'))) then
        SplashScreen := ShowSplashScreen('MySQL Migration Tool')
      else
        SplashScreen := nil;
      
      Application.CreateForm(TMainForm, MainForm);
  if (MainForm.GrtInitialized) then
      begin
        MainForm.InitializeGui;

        // Close splashscreen
        if (SplashScreen <> nil) then
          SplashScreen.Close;

        Application.Run;

        MainForm.Hide;

        TMyxBaseForm.StoreWindowPositions;
      end
      else
        if (SplashScreen <> nil) then
          SplashScreen.Close;
    finally
      UnregisterApplication(Application);
    end;

    // Store options
    if (CommonOptions <> nil) then
      CommonOptions.SaveToFile;
    if (AppOptions <> nil) then
      AppOptions.SaveToFile;

    // Shutdown the GRT
    RuntimeEnvironment.Free;
  end;
end.
