program CanvasTest;

uses
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Generic Canvas Testframe';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
