{$APPTYPE CONSOLE}
program dxgettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl                          *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

uses
  Classes,
  SysUtils,
  Windows,
  ExeImage in 'ExeImage.pas',
  rxtypes in 'rxtypes.pas',
  poparser in 'poparser.pas',
  appconsts in 'appconsts.pas',
  xgettext in 'xgettext.pas',
  xgettexttools in 'xgettexttools.pas',
  consoleapp in 'consoleapp.pas',
  consoleoutput in 'consoleoutput.pas',
  gnugettext in '..\..\..\common\source\windows\gnugettext.pas';

{ Main }

var
  conapp:TConsoleApp;
begin
  textdomain('dxgettext');
  AddDomainForResourceString('delphi');
  AddDomainForResourceString('kylix');
  try
    conapp:=TConsoleApp.Create;
    try
      conapp.Execute;
    finally
      FreeAndNil (conapp);
    end;
  except
    on e:Exception do begin
      writeln (e.Message);
      ExitCode:=1;
    end;
  end;
end.

