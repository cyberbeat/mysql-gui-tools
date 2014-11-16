program GeneralDUnitTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestLexicalTools in '..\unit-tests\TestLexicalTools.pas',
  LexicalTools in '..\..\Common\LexicalTools.pas',
  Support in 'Support.pas',
  UCESQLHighlighter in '..\..\UniCodeEditor\Source\UCESQLHighlighter.pas',
  TestUCESQLHighlighter in '..\unit-tests\TestUCESQLHighlighter.pas',
  TestUCE in '..\unit-tests\TestUCE.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

