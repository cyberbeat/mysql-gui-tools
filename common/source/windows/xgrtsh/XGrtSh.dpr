program XGrtSh;

uses
  gnugettext in '..\gnugettext.pas',
  TntForms, Forms,
  AuxFuncs in '..\AuxFuncs.pas',
  myx_public_interface in '..\..\..\library\public_interface\windows\myx_public_interface.pas',
  myx_util_public_interface in '..\..\..\library_util\public_interface\windows\myx_util_public_interface.pas',
  myx_grt_public_interface in '..\..\..\library_grt\public_interface\windows\myx_grt_public_interface.pas',
  myx_grt_builtin_module_public_interface in '..\..\..\library_grt_modules\public_interface\windows\myx_grt_builtin_module_public_interface.pas',
  myx_sql_resultset_public_interface in '..\..\..\library_sql_resultset\public_interface\windows\myx_sql_resultset_public_interface.pas',
  XGrtShell in 'XGrtShell.pas' {XGrtShellForm: TTntForm},
  GrtForms in '..\GrtForms.pas',
  GrtObjectTree in '..\GrtObjectTree.pas',
  Grt in '..\Grt.pas',
  MyxOptions in '..\MyxOptions.pas',
  MyxAppOptions in 'MyxAppOptions.pas',
  PNGTools in '..\tools\PNGTools.pas';

{$R *.res}

//Enable WindowsXP look'n'feel
{$R ..\..\..\..\mysql-gui-common\res\windows\WindowsXP.res}

{$R ..\..\..\..\mysql-gui-common\res\windows\mysqlcommon.RES}

{$R ..\..\..\..\mysql-gui-common\res\windows\mysqlgrtsh.RES}

begin
  // Add extra domain for runtime library translations
  AddDomainForResourceString('delphi');

  // Force program to use user defined instead of the current Windows settings
  UseLanguage('en'); //MYXCommonOptions.Language);

  //Application Title
  TntApplication.Title := 'MySQL Grt Shell';
  Application.Title := 'X Grt Shell';
  Application.HelpFile := '';
  Application.Initialize;

  Application.CreateForm(TXGrtShellForm, XGrtShellForm);

  XGrtShellForm.Grt.Console := XGrtShellForm.CommandLineUCE;

  Application.Run;
end.
