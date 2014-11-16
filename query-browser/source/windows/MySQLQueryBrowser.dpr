program MySQLQueryBrowser;

{$R '..\..\..\common\res\windows\mysqlcommon.res' '..\..\..\common\res\windows\mysqlcommon.rc'}
{$R '..\..\..\common\res\windows\Manifest.res' '..\..\..\common\res\windows\Manifest.rc'}
{$R '..\..\res\windows\mysqlqb.res' '..\..\res\windows\mysqlqb.rc'}
{%File '..\..\PrepareApplicationFolder.cmd'}

uses
  gnugettext in '..\..\..\common\source\windows\gnugettext.pas',
  Forms,
  SHDocVw,
  SysUtils,
  Main in 'Main.pas' {MainForm},
  AuxFuncs in '..\..\..\common\source\windows\AuxFuncs.pas',
  AuxLists in '..\..\..\common\source\windows\AuxLists.pas',
  MyxError in '..\..\..\common\source\windows\MyxError.pas',
  myx_public_interface in '..\..\..\common\library\public-interface\myx_public_interface.pas',
  MySQLConnection in '..\..\..\common\source\windows\MySQLConnection.pas',
  MySQLResultSet in '..\..\..\common\source\windows\MySQLResultSet.pas',
  Sections in '..\..\..\common\source\windows\Sections.pas',
  InstanceSections in '..\..\..\common\source\windows\InstanceSections.pas',
  SchemataTreeView in '..\..\..\common\source\windows\SchemataTreeView.pas' {SchemataFrame: TFrame},
  AdvancedEdit in '..\..\..\common\source\windows\AdvancedEdit.pas' {AdvancedEditFrame: TFrame},
  ConnectToInstance in '..\..\..\common\source\windows\ConnectToInstance.pas' {ConnectToInstanceForm},
  Options in '..\..\..\common\source\windows\Options.pas',
  OptionsEditor in '..\..\..\common\source\windows\OptionsEditor.pas' {OptionsForm},
  Toolbar in '..\..\..\common\source\windows\Toolbar.pas',
  TabHeader in '..\..\..\common\source\windows\TabHeader.pas' {TabHeaderFrame: TFrame},
  ApplicationDataModule in 'ApplicationDataModule.pas' {ApplicationDM: TDataModule},
  QueryBrowserOptionPages in 'QueryBrowserOptionPages.pas' {QueryBrowserOptionPagesForm},
  QueryBrowser in 'QueryBrowser.pas' {QueryBrowserForm},
  MySQLResultSetControls in '..\..\..\common\source\windows\MySQLResultSetControls.pas',
  TableDrag in 'TableDrag.pas' {TableDragForm},
  EditorTable in '..\..\..\common\source\windows\EditorTable.pas',
  EditorTableVTFKEdit in '..\..\..\common\source\windows\EditorTableVTFKEdit.pas',
  EditorTableVTEdit in '..\..\..\common\source\windows\EditorTableVTEdit.pas',
  CommonFuncs in '..\..\..\common\source\windows\CommonFuncs.pas',
  SchemaSelection in '..\..\..\common\source\windows\SchemaSelection.pas' {SchemaSelectionForm},
  About in '..\..\..\common\source\windows\About.pas' {AboutForm},
  AuxApplicationFuncs in '..\..\..\common\source\windows\AuxApplicationFuncs.pas',
  MySQLResultSetFieldViewer in '..\..\..\common\source\windows\MySQLResultSetFieldViewer.pas' {MySQLResultSetFieldViewerForm},
  MySQLCommonFuncs in '..\..\..\common\source\windows\MySQLCommonFuncs.pas',
  SqlCmdPanel in 'SqlCmdPanel.pas',
  RegExTextImporter in 'RegExTextImporter.pas' {RegExTextImporterForm},
  TextSearch in '..\..\..\common\source\windows\TextSearch.pas' {TextSearchForm},
  ScriptPanel in 'ScriptPanel.pas',
  SQLErrorGrid in '..\..\..\common\source\windows\SQL Controls\Source\SQLErrorGrid.pas',
  AppInstanceMgmt in '..\..\..\common\source\windows\AppInstanceMgmt.pas',
  myx_util_public_interface in '..\..\..\common\library\public-interface\myx_util_public_interface.pas',
  myx_sql_parser_public_interface in '..\..\..\common\library\public-interface\myx_sql_parser_public_interface.pas';

{$R *.res}

var
  ConnectionResult: integer;

begin
  //Create Common Options
  MYXCommonOptions := TMYXCommonOptions.Create('query-browser');
  MYXCommonOptionProvider := MYXCommonOptions;

  // Bind to our own text domain instead of default.
  bindtextdomain('query-browser', IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'locale');
  textdomain('query-browser');

  // Add extra domain for runtime library translations
  AddDomainForResourceString('query-browser');

  TP_GlobalIgnoreClass(TWebBrowser);

  // Force program to use user defined instead of the current Windows settings
  if (MYXCommonOptions.Language <> '-') then
    UseLanguage(MYXCommonOptions.Language);

  //Create Application Data Module
  ApplicationDM := TApplicationDM.Create(nil);

  ApplicationDM.LoadOptions;

  //Application Title
  Application.Title := 'MySQL Query Browser';
  Application.HelpFile := '';
  Application.Initialize;

  //Check Common Commandline parameter
  CheckCommonCommandlineParameter;

  //Check Commandline parameter
  ApplicationDM.CheckCommandlineParameter;

  Application.CreateForm(TMainForm, MainForm);
  if ApplicationDM.QBOptions.Embedded then
    ConnectionResult := MainForm.MySQLConn.ConnectToEmbedded
  else
  begin
    myx_mysql_embedded_prevent_start;

    if ApplicationDM.HasScripts then
      ConnectionResult := 1
    else
      ConnectionResult := MainForm.MySQLConn.ConnectToServer(True, True, True);
  end;

  if (ConnectionResult = 1) then
  begin
    RegisterApplication(Application, MainForm.MySQLConn.GetConnectionCaption(False));
    try
      MainForm.PrepareGUI;
      if ApplicationDM.HasScripts then
        ApplicationDM.LoadScripts;

      Application.Run;
    finally
      UnregisterApplication(Application);
    end;
  end;

  Application.ProcessMessages;
end.

