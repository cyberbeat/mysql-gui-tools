unit TestUCE;

interface

uses
  TestFramework, Windows, Classes, UniCodeEditor, Forms;

type
  // Test methods for class TUniCodeEdit.
  TestTUniCodeEdit = class(TTestCase)
  strict private
    FForm: TForm;
    FEditor: TUniCodeEdit;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestText;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, Support, Controls;
  
//----------------- TestTUniCodeEdit -----------------------------------------------------------------------------------

procedure TestTUniCodeEdit.SetUp;

begin
  FForm := TForm.Create(nil);
  FEditor := TUniCodeEdit.Create(FForm);
  FEditor.Parent := FForm;
  FEditor.Align := alClient;
  FForm.Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTUniCodeEdit.TearDown;

begin
  FreeAndNil(FForm);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTUniCodeEdit.TestText;

begin
  with FEditor do
  begin
    Text := 'abc ' + #13#10 + #13 + #13 + 'def';
    CheckEquals(4, Content.Count, 'Checking correct line break handling (1).');

    Text := #13;
    CheckEquals(1, Content.Count, 'Checking correct line break handling (2).');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUniCodeEdit.Suite);
end.

