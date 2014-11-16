unit TestLexicalTools;

interface

uses
  TestFramework, Windows, Classes, LexicalTools, Unicode;
  
type
  // Test methods for class TDelphiLexer
  TestTDelphiLexer = class(TTestCase)
  strict private
    FDelphiLexer: TDelphiLexer;
    FSource: TFileStream;
    FDummy: Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCheckToken;
    procedure TestCheckTokenSymbol;
    procedure TestLexerError;
    procedure TestHexToBinary;
    procedure TestNextToken;
    procedure TestShowToken;
    procedure TestSourcePos;
    procedure TestTokenComponentIdent;
    procedure TestTokenFloat;
    procedure TestTokenInteger;
    procedure TestTokenInt64;
    procedure TestTokenString;
    procedure TestTokenSymbolIs;
  end;

  // Test methods for class TSQLLexer
  TestTSQLLexer = class(TTestCase)
  strict private
    FSQLLexer: TSQLLexer;
    FDummy: Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCheckToken;
    procedure TestCheckTokenSymbol;
    procedure TestNextToken;
    procedure TestScanRawText;
    procedure TestTokenFloat;
    procedure TestTokenInteger;
    procedure TestTokenInt64;
    procedure TestTokenString;
    procedure TestTokenSymbolIs;
    procedure TestLexerError;
  end;

  // Test methods for class TSQLTokenizer
  TTokenEntry = record
    Token: WideChar;
    Position: Integer;
    Value: Variant;
  end;

  TestTSQLTokenizer = class(TTestCase)
  private
    FLastSeparator: Char;
  strict private
    FSQLTokenizer: TSQLTokenizer;
    FSource: TFileStream;
    FTestData: array of TTokenEntry;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTokens;
    procedure TestBug22214;
  end;

  // Test methods for class THashLinkedItem
  TestTHashLinkedItem = class(TTestCase)
  strict private
    FHashLinkedItem: THashLinkedItem;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;
  // Test methods for class THashTreeItem
  
  TestTHashTreeItem = class(TTestCase)
  strict private
    FHashTreeItem: THashTreeItem;
    FDummy: Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClear;
  end;

  // Test methods for class TStringHashTrie
  TestTStringHashTrie = class(TTestCase)
  strict private
    FStringHashTrie: TStringHashTrie;
    FDummy: Integer;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestDelete;
    procedure TestFind;
    procedure TestTraverse;
    procedure TestTraverse1;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, Support;
  
//----------------- TestTDelphiLexer -----------------------------------------------------------------------------------

procedure TestTDelphiLexer.SetUp;

begin
  //FDelphiLexer := TDelphiLexer.Create(FSource, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TearDown;

begin
  FreeAndNil(FDelphiLexer);
  FreeAndNil(FSource);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestCheckToken;

begin
  // TODO: Implement test case.
  //FDelphiLexer.CheckToken('X');
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestCheckTokenSymbol;

begin
  // TODO: Implement test case.
  // FDelphiLexer.CheckTokenSymbol(S);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestLexerError;

begin
  // TODO: Implement test case.
  // FDelphiLexer.LexerError(Message);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestHexToBinary;

begin
  // TODO: Implement test case.
  // FDelphiLexer.HexToBinary(Stream);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestNextToken;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.NextToken;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestShowToken;

begin
  // TODO: Implement test case.
  // FDelphiLexer.ShowToken(Token);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestSourcePos;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.SourcePos;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenComponentIdent;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenComponentIdent;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenFloat;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenFloat;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenInteger;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenInteger;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenInt64;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenInt64;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenString;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenString;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTDelphiLexer.TestTokenSymbolIs;

begin
  // TODO: Implement test case.
  // ReturnValue := FDelphiLexer.TokenSymbolIs(S);
  FDummy := 1;
end;

//----------------- TestTSQLLexer --------------------------------------------------------------------------------------

procedure TestTSQLLexer.SetUp;

begin
  // FSQLLexer := TSQLLexer.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TearDown;

begin
  FreeAndNil(FSQLLexer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestCheckToken;

begin
  // TODO: Implement test case.
  // FSQLLexer.CheckToken(T);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestCheckTokenSymbol;

begin
  // TODO: Implement test case.
  // FSQLLexer.CheckTokenSymbol(S);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestNextToken;

begin
  // TODO: Implement test case.
  // ReturnValue := FSQLLexer.NextToken;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestScanRawText;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.ScanRawText;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestTokenFloat;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.TokenFloat;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestTokenInteger;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.TokenInteger;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestTokenInt64;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.TokenInt64;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestTokenString;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.TokenString;
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestTokenSymbolIs;

begin
  // TODO: Implement test case.
  //ReturnValue := FSQLLexer.TokenSymbolIs(S);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLLexer.TestLexerError;

begin
  // TODO: Implement test case.
  //FSQLLexer.LexerError(Message);
  FDummy := 1;
end;

//----------------- TestTSQLTokenizer ----------------------------------------------------------------------------------

procedure TestTSQLTokenizer.SetUp;

var
  Lines: TStringList;
  Line: TStringList;
  I: Integer;
  C: Integer;

begin
  FLastSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  FSQLTokenizer := TSQLTokenizer.Create;
  FSource := TFileStream.Create(DataPath('ansi-tokens-1.txt'), fmOpenRead);

  // Load and parse reference data.
  Lines := TStringList.Create;
  Line := TStringList.Create;
  try
    Lines.LoadFromFile(DataPath('ansi-tokens-1.result'));
    SetLength(FTestData, Lines.Count);
    for I := 0 to Lines.Count - 1 do
    begin
      Line.CommaText := Lines[I];
      C := StrToInt(Line[0]);
      FTestData[I].Token := WideChar(C);
      FTestData[I].Position := StrToInt(Line[1]);
      case FTestData[I].Token of
        toSymbol,
        toString,
        toEmbeddedCommand,
        toUserVariable,
        toSystemVariable,
        toHexString:
          FTestData[I].Value := Line[2]; // as string
        toInteger,
        toHexNumber:
          FTestData[I].Value := StrToInt(Line[2]);
        toFloat:
          FTestData[I].Value := StrToFloat(Line[2]);
      end;
    end;
  finally
    Lines.Free;
    Line.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLTokenizer.TearDown;

begin
  DecimalSeparator := FLastSeparator;
  FreeAndNil(FSQLTokenizer);
  FreeAndNil(FSource);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLTokenizer.TestTokens;

var
  I: Integer;

begin
  FSQLTokenizer.Initialize(FSource, tsNormal, True);

  for I := 0 to High(FTestData) do
  begin
    CheckEquals(FTestData[I].Token, FSQLTokenizer.NextToken, 'Test case ' + IntToStr(I) + ' a');

    // We are checking here the source position *after* the token has been consumed.
    CheckEquals(FTestData[I].Position, FSQLTokenizer.SourcePosition, 'Test case ' + IntToStr(I) + ' b');
    case FTestData[I].Token of
      toSymbol:
        CheckTrue(FSQLTokenizer.TokenSymbolIs(FTestData[I].Value), 'Test case ' + IntToStr(I) + ' c');
      toString,
      toEmbeddedCommand,
      toUserVariable,
      toSystemVariable,
      toHexString:
        CheckEquals(FTestData[I].Value, FSQLTokenizer.TokenString, 'Test case ' + IntToStr(I) + ' d');
      toInteger,
      toHexNumber:
        CheckEquals(FTestData[I].Value, FSQLTokenizer.TokenInteger, 'Test case ' + IntToStr(I) + ' e');
      toFloat:
        CheckEquals(FTestData[I].Value, FSQLTokenizer.TokenFloat, 1E-20, 'Test case ' + IntToStr(I) + ' f');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTSQLTokenizer.TestBug22214;

var
  Stream: TStringStream;

begin
  Stream := TStringStream.Create('-');
  FSQLTokenizer.Initialize(Stream, tsNormal, True);
  CheckEquals('-', FSQLTokenizer.NextToken, 'Test case 1');
  Stream.Free;
  
  Stream := TStringStream.Create('--');
  FSQLTokenizer.Initialize(Stream, tsNormal, True);
  CheckEquals(toSLComment, FSQLTokenizer.NextToken, 'Test case 2');
  Stream.Free;

  Stream := TStringStream.Create('--a');
  FSQLTokenizer.Initialize(Stream, tsNormal, True);
  CheckEquals('-', FSQLTokenizer.NextToken, 'Test case 3');
  Stream.Free;

  Stream := TStringStream.Create('-- a');
  FSQLTokenizer.Initialize(Stream, tsNormal, True);
  CheckEquals(toSLComment, FSQLTokenizer.NextToken, 'Test case 3');
  Stream.Free;
end;

//----------------- TestTHashLinkedItem --------------------------------------------------------------------------------

procedure TestTHashLinkedItem.SetUp;

begin
  // FHashLinkedItem := THashLinkedItem.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTHashLinkedItem.TearDown;

begin
  FreeAndNil(FHashLinkedItem);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTHashTreeItem.SetUp;

begin
  // FHashTreeItem := THashTreeItem.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTHashTreeItem.TearDown;

begin
  FreeAndNil(FHashTreeItem);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTHashTreeItem.TestClear;

begin
  // FHashTreeItem.Clear;
  // TODO: Validate method results
  FDummy := 1;
end;

//----------------- TestTStringHashTree --------------------------------------------------------------------------------

procedure TestTStringHashTrie.SetUp;

begin
  FStringHashTrie := TStringHashTrie.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TearDown;

begin
  FStringHashTrie.Free;
  FStringHashTrie := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TestAdd;

begin
  // TODO: Implement test case.
  // FStringHashTrie.Add(S, Data);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TestDelete;

begin
  // TODO: Implement test case.
  // FStringHashTrie.Delete(S);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TestFind;

begin
  // TODO: Implement test case.
  // ReturnValue := FStringHashTrie.Find(S, Data);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TestTraverse;

begin
  // TODO: Implement test case.
  // FStringHashTrie.Traverse(UserData, UserProc);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TestTStringHashTrie.TestTraverse1;

begin
  // TODO: Implement test case.
  // FStringHashTrie.Traverse(UserData, UserProc);
  FDummy := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTDelphiLexer.Suite);
  RegisterTest(TestTSQLLexer.Suite);
  RegisterTest(TestTSQLTokenizer.Suite);
  RegisterTest(TestTHashLinkedItem.Suite);
  RegisterTest(TestTHashTreeItem.Suite);
  RegisterTest(TestTStringHashTrie.Suite);
end.

