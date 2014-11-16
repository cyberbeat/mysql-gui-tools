program MySQLSystemTrayMonitor;

{$R '..\..\..\res\windows\mysqlsystemtraymonitor.res' '..\..\..\res\windows\mysqlsystemtraymonitor.rc'}
{$R '..\..\..\..\common\res\windows\Manifest.res' '..\..\..\..\common\res\windows\Manifest.rc'}

uses
  Instance,
  gnugettext in '..\..\..\..\common\source\windows\gnugettext.pas',
  Forms,
  Main in 'Main.pas' {MainForm},
  Pdh in 'Pdh.pas',
  AuxFuncs in '..\..\..\..\common\source\windows\AuxFuncs.pas',
  AdminService in 'AdminService.pas',
  PNGImage in '..\..\..\..\common\source\windows\png\PNGImage.pas',
  pnglang in '..\..\..\..\common\source\windows\png\pnglang.pas',
  pngzlib in '..\..\..\..\common\source\windows\png\pngzlib.pas',
  MySQLCommonFuncs in '..\..\..\..\common\source\windows\MySQLCommonFuncs.pas',
  PNGTools in '..\..\..\..\common\source\windows\tools\PNGTools.pas';

{$R *.res}

begin
  if not OtherInstanceIsRunning then
  begin
    Application.Initialize;
    Application.ShowMainForm:=False;
    Application.Title:='MySQL System Tray Monitor';

    Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  end;
end.
