unit TestUCESQLHighlighter;

interface

uses
  TestFramework, Windows, Classes, UCESQLHighlighter;

type
  TTokenTestData = record
    TokenType: Integer;
    Position,
    Length: Integer;
    Value: Variant;
  end;

  // Test methods for class TUCESQLHighlighter.
  TestTUCESQLHighlighter = class(TTestCase)
  strict private
    FHighlighter: TUCESQLHighlighter;
    FSource: TStringList;
    FLastSeparator: Char;
    FTestData: array of TTokenTestData;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTokens;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, Support, UCEHighlighter, LexicalTools;
  
//----------------- TestTUCESQLHighlighter -----------------------------------------------------------------------------------

procedure TestTUCESQLHighlighter.SetUp;

var
  Lines: TStringList;
  Line: TStringList;
  I: Integer;

begin
  FHighlighter := TUCESQLHighlighter.Create(nil);
  FLastSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  FSource := TStringList.Create;
  FSource.LoadFromFile(DataPath('ansi-tokens-2.txt'));

  // Load and parse reference data.
  Lines := TStringList.Create;
  Line := TStringList.Create;
  try
    // The result data is basically the same as in ansi-tokens-1.result but the highlighter
    // returns different ids for tokens.
    Lines.LoadFromFile(DataPath('ansi-tokens-2.result'));
    SetLength(FTestData, Lines.Count);
    for I := 0 to Lines.Count - 1 do
    begin
      Line.CommaText := Lines[I];
      FTestData[I].TokenType := StrToInt(Line[0]);
      FTestData[I].Position := StrToInt(Line[1]);
      case FTestData[I].TokenType of
        SYMBOL,
        STRINGCONSTANT,
        EMBEDDED_COMMAND,
        IDENTIFIER,
        KEYWORD,
        QUOTED_ID,
        SYSTEM_VARIABLE,
        USER_VARIABLE,
        MLCOMMENT,
        SLCOMMENT,
        HEXSTRING:
          FTestData[I].Value := Line[2]; // as string
        HEXNUMBER,
        INTEGERNUMBER:
          FTestData[I].Value := StrToInt(Line[2]);
        FLOATNUMBER:
          FTestData[I].Value := StrToFloat(Line[2]);
      end;
    end;
  finally
    Lines.Free;
    Line.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTUCESQLHighlighter.TearDown;

begin
  DecimalSeparator := FLastSeparator;
  FreeAndNil(FHighlighter);
  FreeAndNil(FSource);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTUCESQLHighlighter.TestTokens;

var
  I: Integer;
  Data: TTokenData;
  S: string;
  IntData: Integer;
  FloatData: Double;

begin
  with FHighlighter do
  begin
    SetLine(FSource.Text);
    for I := 0 to High(FTestData) do
    begin
      Next;
      Data := GetTokenInfo;

      CheckEquals(FTestData[I].TokenType, Data.TokenType, 'Test case ' + IntToStr(I) + ' (type)');
      CheckEquals(FTestData[I].Position, Data.Position + Data.Length, 'Test case ' + IntToStr(I) + ' (position)');
      SetString(S, Data.Token, Data.Length);
      case FTestData[I].TokenType of
        IDENTIFIER,
        SYMBOL,
        EMBEDDED_COMMAND,
        KEYWORD,
        QUOTED_ID,
        SYSTEM_VARIABLE,
        USER_VARIABLE,
        HEXSTRING,
        SLCOMMENT:
          begin
            CheckEquals(string(FTestData[I].Value), S, 'Test case ' + IntToStr(I) + ' (string value)');
          end;
        STRINGCONSTANT,
        MLCOMMENT:
          begin
            S := StringReplace(S, #13#10, '#13#10', [rfReplaceAll]);
            CheckEquals(string(FTestData[I].Value), S, 'Test case ' + IntToStr(I) + ' (string value)');
          end;
        HEXNUMBER,       // Note: there are currently no test strings fro HEXNUMBER and HEXSTRING.
                         // There is an issue with the parser generator.
        INTEGERNUMBER:
          begin
            IntData := StrToInt(S);
            CheckEquals(FTestData[I].Value, IntData, 'Test case ' + IntToStr(I) + ' (int value)');
          end;
        FLOATNUMBER:
          begin
            FloatData := StrToFloat(S);
            CheckEquals(FTestData[I].Value, FloatData, 1E-20, 'Test case ' + IntToStr(I) + ' (float value)');
          end;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTUCESQLHighlighter.Suite);
end.

