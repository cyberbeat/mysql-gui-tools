unit LexicalTools;

// Copyright (C) 2004 - 2006 MySQL AB, 2008 Sun Microsystems, Inc.
//
//----------------------------------------------------------------------------------------------------------------------
//
// Description:
//   This unit contains general purpose classes for lexical tasks like tokenizing text.
//
// Initial implementor:
//   2004 Mike Lischke
//
//----------------------------------------------------------------------------------------------------------------------
//
interface

{$I Compilers.inc}

uses
  Windows, Classes, Unicode, SysUtils;

const
  LexerBufferSize = 32768;

  // Tokens returned to caller when processing text.
  toEOF             = WideChar(0);
  toBOF             = WideChar(1);      // set if the lexer has not yet read the first token
  toSymbol          = WideChar(2);
  toString          = WideChar(3);
  toInteger         = WideChar(4);
  toFloat           = WideChar(5);
  toHexNumber       = WideChar(6);
  toWhiteSpace      = WideChar(7);
  toSLComment       = WideChar(8);
  toMLComment       = WideChar(9);
  toUserVariable    = WideChar(10);
  toSystemVariable  = WideChar(11);
  toHexString       = WideChar(12);     // A string containing hex numbers, preceded by a single 'x'.
  toEmbeddedCommand = WideChar(13);
  toKeyword         = WideChar(14);

type
  // TDelphiLexer is a class to quickly split input into tokens while automatically skipping
  // comments or conditional code if necessary.
  TDelphiLexer = class;

  TNewLineEvent = procedure(Sender: TObject; Line: Cardinal) of object;
  TLexerErrorEvent = procedure(Pos: TPoint; const Error: WideString) of object;

  // This class is used to tokenize Delphi source code like text.
  TDelphiLexer = class(TObject)
  private
    FStream: TStream;
    FOrigin: Integer;
    FBuffer: PWideChar;
    FBufPtr: PWideChar;
    FBufEnd: PWideChar;
    FSourcePtr: PWideChar;
    FSourceEnd: PWideChar;
    FTokenPtr: PWideChar;
    FLineStart: PWideChar;
    FSourceLine: Cardinal;
    FSaveChar: WideChar;
    FToken: WideChar;
    FFloatType: WideChar;
    FTokenString: WideString;
    FIsUnicode,
    FNeedSwap: Boolean;
    FDefines: TWideStringList;                // Currently defined compiler symbols.
    FCompilerSwitch: array[0..25] of Boolean; // One entry for each letter from 'a' to 'z'.

    FOnNewLine: TNewLineEvent;
    FOnLexerError: TLexerErrorEvent;
    procedure AdvanceSource(Amount: Cardinal = 1);
    function GetCompilerOption(Option: Char): Boolean;
    function GetToken: WideChar;
    procedure HandleDirective(UseStar: Boolean);
    function NeedChars(Count: Cardinal): Boolean;
    procedure ReadBuffer;
    procedure SkipConditionalPart(WithElse: Boolean);
    function ScanIdentifier: WideString;
    procedure SetCompilerOption(Option: Char; const Value: Boolean);
    procedure SetDefines(const Value: TWideStringList);
    procedure SkipBlanks;
    procedure SkipComments;
    procedure SkipLine;
    procedure SkipString;
    procedure SkipToDirectiveEnd(UseStar: Boolean);
    procedure SkipToEndIf;
    procedure SkipToElseOrEndIf;
    procedure SkipUntil(S: WideString);
  protected
    procedure DoNewLine;
  public
    constructor Create(Stream: TStream; IsUnicode: Boolean);
    destructor Destroy; override;

    procedure CheckToken(T: WideChar);
    procedure CheckTokenSymbol(const S: WideString);
    procedure LexerError(const Message: WideString);
    procedure HexToBinary(Stream: TStream);
    function NextToken: WideChar;
    procedure ShowToken(Token: WideChar);
    function SourcePos: Integer;
    function TokenComponentIdent: WideString;
    function TokenFloat: Extended;
    function TokenInteger: Integer;
    function TokenInt64: Int64;
    function TokenString: WideString;
    function TokenSymbolIs(const S: WideString): Boolean;

    property CompilerOption[Option: Char]: Boolean read GetCompilerOption write SetCompilerOption;
    property Defines: TWideStringList read FDefines write SetDefines;
    property FloatType: WideChar read FFloatType;
    property SourceLine: Cardinal read FSourceLine;
    property Token: WideChar read GetToken;

    property OnNewLine: TNewLineEvent read FOnNewLine write FOnNewLine;
    property OnLexerError: TLexerErrorEvent read FOnLexerError write FOnLexerError;
  end;

  // A lexer to tokenize SQL text. This class is very similar to the Delphi lexer but without any support
  // for conditional parts. It can only handle text encoded as UTF-16 (LE and BE).
  TSQLLexer = class
  private
    FStream: TStream;
    FSavePoint: Int64;     // The original position in the input stream.
    FOrigin: Integer;
    FBuffer: PWideChar;
    FBufPtr: PWideChar;
    FBufEnd: PWideChar;
    FSourcePtr: PWideChar;
    FSourceEnd: PWideChar;
    FTokenPtr: PWideChar;
    FLineStart: PWideChar;
    FSourceLine: Cardinal;
    FSaveChar: WideChar;
    FToken: WideChar;
    FFloatType: WideChar;
    FNeedSwap: Boolean;

    FOnNewLine: TNewLineEvent;
    FOnLexerError: TLexerErrorEvent;
  protected
    procedure AdvanceSource(Amount: Cardinal = 1); virtual;
    procedure DoNewLine; virtual;
    function GetToken: WideChar; virtual;
    function NeedChars(Count: Cardinal): Boolean;
    procedure ReadBuffer; virtual;
    procedure SkipBlanks; virtual;
    procedure SkipComments; virtual;
    procedure SkipLine; virtual;
    procedure SkipUntil(S: WideString); virtual;
  public
    constructor Create(Stream: TStream); virtual;
    destructor Destroy; override;

    procedure CheckToken(T: WideChar);
    procedure CheckTokenSymbol(const S: WideString);
    function NextToken: WideChar;
    procedure Reset; virtual;
    function ScanRawText: WideString;
    function TokenFloat: Extended;
    function TokenInteger: Integer;
    function TokenInt64: Int64;
    function TokenString: WideString;
    function TokenSymbolIs(const S: WideString): Boolean;
    procedure LexerError(const Message: WideString);

    property FloatType: WideChar read FFloatType;
    property SourceLine: Cardinal read FSourceLine;
    property Token: WideChar read GetToken;

    property OnNewLine: TNewLineEvent read FOnNewLine write FOnNewLine;
    property OnLexerError: TLexerErrorEvent read FOnLexerError write FOnLexerError;
  end;

  // The tokenizer state tells where the tokenizer left of the last run. States other than tsNormal are usually
  // used if the input was exhausted but the token was not finished.
  TTokenizerState = (
    tsNormal,
    tsHexString,
    tsSingleQuoteString,
    tsBackQuoteString,
    tsDoubleQuoteString,
    tsComment,
    tsEmbeddedCommand    // A command embedded in a multi line comment.
  );

  // This class is similar to the TSQLLexer but returns any encountered token (e.g. line breaks, spaces, comments
  // etc. are not simply skipped). It does not throw any error (except for wrong data conversions).
  TSQLTokenizer = class
  private
    FStream: TStream;
    FIsAnsi: Boolean;
    FOrigin: Integer;
    FBuffer: array[0..LexerBufferSize] of WideChar;
    FBufPtr: PWideChar;
    FBufEnd: PWideChar;
    FSourcePtr: PWideChar;
    FSourceEnd: PWideChar;
    FTokenPtr: PWideChar;
    FLineStart: PWideChar;
    FSourceLine: Cardinal;
    FToken: WideChar;
    FFloatType: WideChar;
    FNeedSwap: Boolean;
    FState: TTokenizerState;
    FTokenString: WideString; // Accumulates the chars for the current token in case we need to refill the buffer.
  protected
    procedure AccumulateTokenString;
    function AdvanceSource(Amount: Cardinal = 1): Boolean; virtual;
    function GetToken: WideChar; virtual;
    function NeedChars(Count: Cardinal): Boolean;
    procedure ReadBuffer; virtual;
  public
    function NextToken: WideChar;
    procedure Initialize(Stream: TStream; State: TTokenizerState; IsANSI: Boolean);
    function SourcePosition: Integer;
    function TokenFloat: Extended;
    function TokenInteger: Integer;
    function TokenInt64: Int64;
    function TokenPosition: Integer;
    function TokenSymbolIs(const S: WideString): Boolean;

    property FloatType: WideChar read FFloatType;
    property SourceLine: Cardinal read FSourceLine;
    property State: TTokenizerState read FState;
    property Token: WideChar read GetToken;
    property TokenString: WideString read FTokenString;
  end;

// Hash trie implementation.
  
const
  // LeafSize must be 256. No changes allowed.
  LeafSize = 256;
  // BucketSize determines max length of the list. Very big|small values decrease performance, while
  // the optimum value in range 4..16.
  BucketSize = 8;

type
  THashLinkedItem = class(TObject)
  private
    FValue: Cardinal;
    FData: Pointer;
    FNext: THashLinkedItem;
  public
    constructor Create(Value: Cardinal; Data: Pointer; Next: THashLinkedItem);
    destructor Destroy; override;
  end;

  THashTrie = class;

  TTraverseProc = procedure(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean) of object;

  THashTreeItem = class(TObject)
  private
    FOwner: THashTrie;
    FLevel: Integer;
    FFilled: Integer;
    FItems: array of TObject; // This will be at most LeafSize entries.
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: Pointer);
    procedure Delete(Value, Hash: Cardinal);
    function Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;
    function GetFilled: Integer;
    function Modify(Value, Hash: Cardinal; const Data: Pointer): Boolean;
    function ROR(Value: Cardinal): Cardinal;
    function RORN(Value: Cardinal; Level: Integer): Cardinal;
    function Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;
  public
    constructor Create(AOwner: THashTrie);
    destructor Destroy; override;

    procedure Clear;
  end;

  THashTrie = class(TObject)
  private
    FRoot: THashTreeItem;
    function GetCount: Integer;
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: Pointer);
    function CompareValue(Value1, Value2: Cardinal): Boolean; virtual; abstract;
    procedure Delete(Value, Hash: Cardinal);
    procedure DestroyItem(var Value: Cardinal; var Data: Pointer); virtual; abstract;
    function HashValue(Value: Cardinal): Cardinal; virtual; abstract;
    procedure Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    function Find(Value, Hash: Cardinal; var Data: Pointer): Boolean; overload;

    property Count: Integer read GetCount;
  end;

  TStringHashTrie = class;

  TStrHashTraverseProc = procedure(UserData: Pointer; const Value: string; Data: Pointer; var Done: Boolean);
  TStrHashTraverseMeth = procedure(UserData: Pointer; const Value: string; Data: Pointer; var Done: Boolean) of object;

  TSHTFreeItemEvent = procedure(Sender: TStringHashTrie; const S: string; const Data: Pointer) of object;

  TStringHashTrie = class(THashTrie)
  private
    FCaseSensitive: Boolean;
    FOnFreeItem: TSHTFreeItemEvent;
  protected
    function HashValue(Value: Cardinal): Cardinal; override;
    procedure DestroyItem(var Value: Cardinal; var Data: Pointer); override;
    function CompareValue(Value1, Value2: Cardinal): Boolean; override;
    function HashStr(const S: string): Cardinal;
    procedure TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);
    procedure TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);
  public
    procedure Add(const S: string; const Data: Pointer);
    procedure Delete(const S: string);
    function Find(const S: string; var Data: Pointer): Boolean; overload;
    procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc); overload;
    procedure Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth); overload;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;

    property OnFreeItem: TSHTFreeItemEvent read FOnFreeItem write FOnFreeItem;
  end;

  // Support for trie statistics.
  TLengthStatistics = array[1..BucketSize] of Integer;

function CalcStrCRC32(const S: string): Cardinal;
function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;
procedure TrieStatistics(Trie: THashTrie; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);

procedure BinToHex(Buffer: Pointer; Text: PWideChar; BufSize: Integer);
function HexToBin(Text: PWideChar; Buffer: Pointer; BufSize: Integer): Integer;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  {$ifdef Compiler_6_UP}
    RTLConsts,
  {$else}
    Consts,
  {$endif Compiler_6_UP}
  Math, Variants;

resourcestring
  SUnexpectedEOF = 'Unexpected end of file';
  SInvalidDirective = 'Invalid compiler directive.';     

  SXMLErrorUnknown = 'Invalid input found';
  SXMLInvalidTag = 'Invalid tag found';
  SXMLUnmatchedStartTag = 'End tag does not match start tag';
  SXMLInvalidAttribute = 'Invalid attribute format';
  SXMLStringExpected = 'String expected but quote character is missing';
  SXMLUnfinishedString = 'Unexpected end of string found';
  SXMLInvalidNumber = 'Invalid number';
  SXMLInvalidEntity = 'Invalid entity';
  SXMLSpaceMissing = 'Required space character is missing';

// HTML/XML entities, taken from http://www.w3.org/TR/REC-html40/sgml/entities.html.
type
  TEntity = record
    Name: string;
    Value: WideChar;
  end;

const
  // Delphi directives.
  DirectiveStrings: array[0..50] of string = (
    'ifdef', 'ifndef', 'define', 'undef', 'else', 'endif', 'ifopt', 'nodefine',
    'align', 'apptype', 'assertions', 'booleval', 'debuginfo', 'denypackageunit', 'description',
    'designonly', 'objexportall', 'extendedsyntax', 'externalsym', 'hints', 'hppemit', 'implicitbuild',
    'importeddata', 'imagebase', 'include', 'iochecks', 'link', 'localsymbols', 'longstrings', 'minstacksize',
    'maxstacksize', 'minenumsize', 'openstrings', 'optimization', 'overflowchecks', 'savedivide', 'noinclude',
    'rangechecks', 'realcompatibility', 'resource', 'runonly', 'typeinfo', 'referenceinfo', 'definitioninfo',
    'typedaddress', 'varstringchecks', 'warnings', 'weakpackageunit', 'stackframes', 'writableconst', 'warn'
    );

  // Mapping of a directive string to a one letter compiler switch.
  // Note: small letters are used here instead capital letters to indicate the conversion
  // (need to look for on/off keyword)
  DirectiveToSwitch: array[0..50] of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7,  // ifdef ... nodefine
    Ord('a'),
    9,
    Ord('c'),                // assertions
    Ord('b'),                // booleval
    Ord('d'),                // debuginfo
    13, 14, 15, 16,          // denypackageunit .. objexportall
    Ord('x'),                // extendedsyntax
    18, 19, 20, 21,          // externalsym .. implicitbuild
    Ord('g'),                // importeddata
    23, 24,                  // imagebase .. include
    Ord('i'),                // iochecks
    26,                      // link
    Ord('l'),                // localsymbols
    Ord('h'),                // longstrings
    29, 30, 31,              // minstacksize .. minenumsize
    Ord('p'),                // openstrings
    Ord('o'),                // optimization
    Ord('q'),                // overflowchecks
    Ord('u'),                // savedivide',
    36,                      // noinclude
    Ord('r'),                // rangechecks
    38, 39, 40,              // realcompatibility .. runonly
    Ord('m'),                // typeinfo
    Ord('y'),                // referenceinfo
    43,                      // definitioninfo
    Ord('t'),                // typedaddress
    Ord('v'),                // varstringchecks
    46, 47,                  // warnings .. weakpackageunit
    Ord('w'),                // stackframes
    Ord('j'),                // writableconst
    48                       // warn
  );

var
  Directives: TStringList;

//----------------------------------------------------------------------------------------------------------------------

procedure InitializeDirectives;

var
  I: Integer;

begin
  // Prepare an internal list of directive strings to make converting them into IDs quick.
  Directives := TStringList.Create;

  // The object property of each entry is used to hold an ID to be returned from the search code.
  for I := 0 to 50 do
    Directives.AddObject(DirectiveStrings[I], Pointer(DirectiveToSwitch[I]));
  Directives.Sort;
end;

//----------------------------------------------------------------------------------------------------------------------

const
  CRC32_POLYNOMIAL = $EDB88320;

var
  // Dynamic crc32 table.
  CCITT32Table: array of Cardinal;

procedure BuildCRCTable;

var
  i, j: longint;
  value: Cardinal;
begin
  SetLength(CCITT32Table, 256);
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do
      if ((value and 1) <> 0) then
        value := (value shr 1) xor CRC32_POLYNOMIAL
      else
        value := value shr 1;
    Ccitt32Table[i] := value;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function CalcStrCRC32(const S: string): Cardinal;

var
  I: Integer;

begin
  // Create CRC table if not yet done.
  if CCITT32Table = nil then
    BuildCRCTable;

  Result := $FFFFFFFF;
  for I:=1 to Length(S) do
    Result:= (((Result shr 8) and $00FFFFFF) xor (CCITT32Table[(Result xor Byte(S[I])) and $FF]));
end;

//----------------------------------------------------------------------------------------------------------------------

// By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net
//
// If you are hashing n strings (ub1 **)k, do it like this:
//   for (i=0, h=0; i<n; ++i) h = jhash( k[i], len[i], h);

procedure Mix(var A, B, C: Cardinal);

begin
  Dec(A, B); Dec(A, C); A := A xor (C shr 13);
  Dec(B, C); Dec(B, A); B := A xor (A shl 8);
  Dec(C, A); Dec(C, B); C := C xor (B shr 13);
  Dec(A, B); Dec(A, C); A := A xor (C shr 12);
  Dec(B, C); Dec(B, A); B := B xor (A shl 16);
  Dec(C, A); Dec(C, B); C := C xor (B shr 5);
  Dec(A, B); Dec(A, C); A := A xor (C shr 3);
  Dec(B, C); Dec(B, A); B := B xor (A shl 10);
  Dec(C, A); Dec(C, B); C := C xor (B shr 15);
end;

//----------------------------------------------------------------------------------------------------------------------

function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;

// Length: the length of the key.
// InitVal: the previous hash, or an arbitrary value.

var
  A, B, C, Len: Cardinal;
  K: PByteArray;

begin
  // Set up the internal state.
  Len := Length;
  K := Key;
  A := $9E3779B9;  // The golden ratio; an arbitrary value.
  B := $9E3779B9;
  C := InitVal;    // The previous hash value.

  // Handle most of the key.
  while Len >= 12 do
  begin
    Inc(A, K[0] + (Cardinal(K[1]) shl 8) + (Cardinal(K[2]) shl 16) + (Cardinal(K[3]) shl 24));
    Inc(B, K[4] +(Cardinal(K[5]) shl 8) + (Cardinal(K[6]) shl 16) + (Cardinal(K[7]) shl 24));
    Inc(C, K[8] + (Cardinal(K[9]) shl 8) + (Cardinal(K[10]) shl 16) + (Cardinal(K[11]) shl 24));
    Mix(A, B, C);
    Inc(PByte(K), 12);
    Dec(Len, 12);
  end;

   // Handle the last 11 bytes.
  Inc(C, Length);
  if Len >= 11 then
    Inc(C, Cardinal(K[10]) shl 24);
  if Len >= 10 then
    Inc(C, Cardinal(K[9]) shl 16);
  if Len >= 9 then
    Inc(C, Cardinal(K[8]) shl 8);
  if Len >= 8 then
    Inc(B, Cardinal(K[7]) shl 24);
  if Len >= 7 then
    Inc(B, Cardinal(K[6]) shl 16);
  if Len >= 6 then
    Inc(B, Cardinal(K[5]) shl 8);
  if Len >= 5 then
    Inc(B, Cardinal(K[4]));
  if Len >= 4 then
    Inc(A, Cardinal(K[3]) shl 24);
  if Len >= 3 then
    Inc(A, Cardinal(K[2]) shl 16);
  if Len >= 2 then
    Inc(A, Cardinal(K[1]) shl 8);
  if Len >= 1 then
    Inc(A, Cardinal(K[0]));
  // Case 0: nothing left to add.

  Mix(A, B, C);
  Result := C;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure BinToHex(Buffer: Pointer; Text: PWideChar; BufSize: Integer);

// converts a stream of byte values into a hexadecimal text representation
// EAX contains Buffer, EDX contains Text and ECX contains BufSize on enter
// Note: BufSize gives the source size in bytes. Text must be able to keep twice this amount,
//       because each source byte will be converted into two hex digits.

asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EAX
       MOV     EDI, EDX
       XOR     EDX, EDX
       JMP     @@1

@@0:
       DB '0123456789ABCDEF'

@@1:
       LODSB
       MOV     DL, AL
       AND     AL, 0FH
       MOV     AL, @@0.Byte[EAX]
       STOSW
       MOV     AL, DL
       SHR     AL, 4
       MOV     AL, @@0.Byte[EAX]
       STOSW
       DEC     ECX
       JNE     @@1
       POP     EDI
       POP     ESI
end;

//----------------------------------------------------------------------------------------------------------------------

function HexToBin(Text: PWideChar; Buffer: Pointer; BufSize: Integer): Integer; 

// although this procedure takes a wide string as parameter it can still only convert values within the ANSI range
// EAX contains Text, EDX contains Buffer and ECX contains BufSize on enter
// Note: It will twice as much input consumed as given in BufSize which denotes the amount of bytes
//       which can be stored into Buffer.
// Return value is the number of bytes written to Buffer.

asm
       PUSH    ESI
       PUSH    EDI
       PUSH    EBX
       MOV     ESI, EAX
       MOV     EDI, EDX
       MOV     EBX, EDX
       XOR     EDX, EDX
       XOR     EAX, EAX
       JMP     @@1

@@0:
       DB  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1
       DB -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1
       DB -1, -1, -1, -1,- 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1
       DB -1, 10, 11, 12, 13, 14, 15

@@1:
       LODSW
       CMP     AX, '0'                 // check for 0..F range
       JB      @@2
       CMP     AX, 'f'
       JA      @@2
       MOV     DL, @@0.Byte[EAX - '0'] // keep converted value,
       CMP     DL, -1                  // but stop conversion if the value is invalid
       JE      @@2
       SHL     DL, 4

       LODSW                           // load second (we can store two digits into one byte)
       CMP     AX, '0'
       JB      @@2
       CMP     AX, 'f'
       JA      @@2
       MOV     DH, @@0.Byte[EAX - '0']
       CMP     DH, -1
       JE      @@2
       OR      DL, DH
       STOSB
       DEC     ECX
       JNE     @@1

@@2:
       MOV     EAX, EDI
       SUB     EAX, EBX
       POP     EBX
       POP     EDI
       POP     ESI
end;

//----------------------------------------------------------------------------------------------------------------------

function LineStart(Buffer, BufPos: PWideChar): PWideChar; assembler;

// Use LineStart to find the start of the last partial line in the buffer. LineStart starts at BufPos and scans
// backwards for a line separator or line feed. It returns a pointer to the position following that character.
// If no line separator is found then the Buffer pointer is returned. Buffer should point to the begining of a block of
// memory and BufPos to the end of the block you want to scan.
// EAX contains Buffer, EDX BufPos on enter.

asm
       MOV     ECX, EDX
       SUB     ECX, EAX
       SHR     ECX, 1                // calculate difference (in characters) between BufPos and Buffer
       JECXZ   @@Finish

@@2:
       MOV     AX, [EDX]
       CMP     AX, WideLineSeparator
       JZ      @@1                   // found a line separator
       CMP     AX, WideLineFeed
       JZ      @@1                   // found a line feed
       SUB     EDX, 2
       DEC     ECX
       JNZ     @@2
       JMP     @@Finish

@@1:
       MOV     EAX, EDX
@@Finish:
end;

//----------------- TDelphiLexer --------------------------------------------------------------------------------

constructor TDelphiLexer.Create(Stream: TStream; IsUnicode: Boolean);

var
  BOM: WideChar;
  Count: Integer;

begin
  FStream := Stream;
  FIsUnicode := IsUnicode;
  FNeedSwap := False;
  if IsUnicode then
  begin
    // determine byte order in source if possible,
    // if there is no byte order mark at stream start then assume LSB first order (little endian, no swap)
    Count := Stream.Read(BOM, SizeOf(BOM));
    if (BOM = BOM_LSB_FIRST) or (BOM = BOM_MSB_FIRST) then
    begin
      FNeedSwap := BOM = BOM_MSB_FIRST;
    end
    else
      Stream.Seek(-Count, soFromCurrent);
  end;
  GetMem(FBuffer, 2 * LexerBufferSize);
  FBuffer[0] := WideNull;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + LexerBufferSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;

  FLineStart := FSourcePtr;
  FSourceLine := 1;
  FToken := toBOF;

  FDefines := TWideStringList.Create;
  FDefines.Duplicates := dupIgnore;
  FDefines.Sorted := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TDelphiLexer.Destroy;

begin
  FDefines.Free;

  FStream.Seek(Integer(FTokenPtr) - Integer(FBufPtr), 1);
  FreeMem(FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.DoNewLine;

begin
  Inc(FSourceLine);
  FLineStart := FSourcePtr + 1;
  if Assigned(FOnNewLine) then
    FOnNewLine(Self, FSourceLine);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.CheckToken(T: WideChar);

begin
  if Token <> T then
    case T of
      toSymbol:
        LexerError(SIdentifierExpected + ' but "' + TokenString + '" found');
      toString:
        LexerError(SStringExpected + ' but "' + TokenString + '" found');
      toInteger, toFloat:
        LexerError(SNumberExpected + ' but "' + TokenString + '" found');
    else
      LexerError('"' + WideString(T) + '" expected but "' + TokenString + '" found');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.CheckTokenSymbol(const S: WideString);

begin
  if not TokenSymbolIs(S) then
    LexerError('"' + S + '" expected but "' + TokenString + '" found');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.LexerError(const Message: WideString);

// Shows an error message.

var
  S: WideString;

begin
  if Assigned(FOnLexerError) then
    FOnLexerError(Point(FSourcePtr - FLineStart + 1, FSourceLine), Message) // 1 based position
  else
  begin
    S := Message + ' on line ' + IntToStr(FSourceLine);
    MessageBoxW(0, PWideChar(S), 'Lexer Error', MB_OK or MB_ICONERROR or MB_APPLMODAL);
  end;
  Abort;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.HexToBinary(Stream: TStream);

var
  Count: Integer;
  Buffer: array[0..255] of Byte;

begin
  SkipBlanks;
  while FSourcePtr^ <> '}' do
  begin
    Count := HexToBin(FSourcePtr, @Buffer, 256);
    if Count = 0 then
      LexerError(SInvalidBinary);
    Stream.Write(Buffer, Count);
    Inc(FSourcePtr, Count);
    SkipBlanks;
  end;
  NextToken;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.NextToken: WideChar;

// Advances the lexer to the next valid token. This excludes code which is put into a $ifdef $endif part for which the
// test fails, any comment and any white space.

var
  I, J: Integer;
  P, S: PWideChar;

begin
  Result := toEOF;

  // loop until a valid token has been found
  while True do
  begin
    // first remove heading comments (this will automatically skip blanks too)
    SkipComments;

    if (FSourcePtr^ = '{') or NeedChars(2) and (FSourcePtr^ = '(') and ((FSourcePtr + 1)^ = '*') then
    begin
      // comment skipper encountered a compiler directive, handle this first and then start over
      HandleDirective(FSourcePtr^ = '(');
      Continue;
    end;

    // at this point we have the start of a valid token, now extract and return it to the application
    P := FSourcePtr;
    FTokenPtr := P;
    FTokenString := '';
    // Low lines (underlines) are not part of the formal Unicode identifier syntax. So test for them explicitly.
    if (P^ = '_') or UnicodeIsIdentifierStart(Word(P^)) then
    begin
      Inc(P);
      while UnicodeIsIdentifierPart(Word(P^)) do
        Inc(P);
      Result := toSymbol;
    end
    else
    begin
      case P^ of
        '#', '''', '"': // string
          begin
            J := 0;
            S := P;
            while True do
              case P^ of
                '#':
                  begin
                    Inc(P);
                    I := 0;
                    while UnicodeIsNumber(Word(P^)) do
                    begin
                      I := I * 10 + (Ord(P^) - Ord('0'));
                      Inc(P);
                    end;
                    Inc(J);
                  end;
                '"': // Have to allow " " too. This is not valid Delphi syntax, but used in some VCL assembler code.
                  begin
                    Inc(P);
                    while not (P^ in [WideNull, WideLF, WideCR, WideChar('"')]) and (P^ <> WideLineSeparator) do
                    begin
                      Inc(P);
                      Inc(J);
                    end;
                    if P^ <> '"' then
                      LexerError(SInvalidString)
                    else
                      Inc(P);
                  end;
                '''':
                  begin
                    Inc(P);
                    while True do
                    begin
                      case P^ of
                        WideNull, WideLF, WideCR, WideLineSeparator:
                          LexerError(SInvalidString);
                        '''':
                          begin
                            Inc(P);
                            if P^ <> '''' then
                              Break;
                          end;
                      end;
                      Inc(J);
                      Inc(P);
                    end;
                  end;
              else
                Break;
              end;
            P := S;
            SetLength(FTokenString, J);
            J := 1;
            while True do
              case P^ of
                '#':
                  begin
                    Inc(P);
                    I := 0;
                    while UnicodeIsNumber(Word(P^)) do
                    begin
                      I := I * 10 + (Ord(P^) - Ord('0'));
                      Inc(P);
                    end;
                    FTokenString[J] := WideChar(Word(I));
                    Inc(J);
                  end;
                '"': // Have to allow " " too. This is not valid Delphi syntax, but used in some VCL assembler code.
                  begin
                    Inc(P);
                    while not (P^ in [WideNull, WideLF, WideCR, WideCHar('"')]) and (P^ <> WideLineSeparator) do
                    begin
                      FTokenString[J] := P^;
                      Inc(P);
                      Inc(J);
                    end;
                    if P^ <> '"' then
                      LexerError(SInvalidString)
                    else
                      Inc(P);
                  end;
                '''':
                  begin
                    Inc(P);
                    while True do
                    begin
                      case P^ of
                        WideNull, WideLF, WideCR, WideLineSeparator:
                          LexerError(SInvalidString);
                        '''':
                          begin
                            Inc(P);
                            if P^ <> '''' then
                              Break;
                          end;
                      end;
                      FTokenString[J] := P^;
                      Inc(J);
                      Inc(P);
                    end;
                  end;
              else
                Break;
              end;
            Result := toString;
          end;
        '$': // hex number
          begin
            Inc(P);
            while UnicodeIsHexDigit(Word(P^)) do
              Inc(P);
            Result := toInteger;
          end;
        '-', '0'..'9': // Integer, float number or Integer subrange of which the lower bound will be returned
          begin
            Inc(P);
            while UnicodeIsNumber(Word(P^)) do
              Inc(P);
            Result := toInteger;

            // if we have a subrange then we are done, otherwise scan further
            if not ((P^ = '.') and NeedChars(2) and ((P + 1)^ = '.')) then
            begin
              if P^ in [WideChar('.'), WideChar('e'), WideChar('E')] then
              begin
                // it is actually a floating point value
                Result := toFloat;

                // skip irrelevant period character if directly followed by exponent character
                if (P^ = '.') and NeedChars(2) and ((P + 1)^ in [WideChar('e'), WideChar('E')]) then
                  Inc(P);
                // skip exponent letter if there is one followed by a plus or minus sign
                if (P^ in [WideChar('e'), WideChar('E')]) and NeedChars(2) and
                  ((P + 1)^ in [WideChar('+'), WideChar('-')]) then
                  Inc(P);

                // skip whatever left over, the period, the expontent symbol or the sign 
                Inc(P);
                while UnicodeIsNumber(Word(P^)) do
                  Inc(P);

                if (P^ in [WideChar('c'), WideChar('C'), WideChar('d'), WideChar('D'), WideChar('s'), WideChar('S')]) then
                begin
                  Result := toFloat;
                  FFloatType := P^;
                  Inc(P);
                end
                else
                  FFloatType := WideNull;
              end;
            end;
          end;
      else
        // any other symbol not consumed above
        Result := P^;
        if Result <> toEOF then
          Inc(P);
      end;
    end;
    FSourcePtr := P;  
    FToken := Result;
    Break; // if we come here then we have a valid token and can return
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IDFromDirective(const S: WideString): Integer;

// Converts the given directive ID string into a number which can be used to uniquely identify the directive.
// A result of -1 indicates that the directive was not recognized. Otherwise single letter directives
// are returned with their ordinal value and all other directives as a number between 0..49.
// Directives which correspond to a single letter WideString will be handled as if the single letter WideString
// has been given

begin
  Result := -1;

  if (Length(S) > 0) and (S[1] in [WideChar('A')..WideChar('Z'), WideChar('a')..WideChar('z')]) then
  begin
    // single char directives occupy the range 0..25 (the check above ensures that the first character is plain Latin)
    if Length(S) = 1 then
      Result := Ord(UpCase(Char(S[1])))
    else
    begin
      if Directives = nil then
        InitializeDirectives;
      // Return the ID (the position in the original array) instead of the index in the list
      // because this might have changed when sorting the list
      if Directives.Find(S, Result) then
        Result := Integer(Directives.Objects[Result])
      else
        Result := -1;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.GetToken: WideChar;

begin
  if FToken = toBOF then
    NextToken;
  Result := FToken;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.HandleDirective(UseStar: Boolean);

// the current input pointer is placed on the start of a compiler directive ('{$'), now evaluate this

var
  DirectiveID: WideString;
  ID: Integer;
  IsOn, IsOff: Boolean;

begin
  // skip initial curly bracket and dollar sign or parenthesis with star and dollar sign
  Inc(FSourcePtr, 2 + Ord(UseStar));

  // determine what kind of directive we actually have
  if UnicodeIsIdentifierStart(Word(FSourcePtr^)) then
  begin
    // in some cases several directives might be in one {} pair
    repeat
      // extract directive identifier
      DirectiveID := ScanIdentifier;

      ID := IDFromDirective(DirectiveID);
      case ID of
        Ord('A')..Ord('Z'):             // single letter directive, others might follow
          if FSourcePtr^ in [WideChar('+'), WideChar('-')] then
          begin
            FCompilerSwitch[ID - 65] := FSourcePtr^ = '+';
            Inc(FSourcePtr);
            // several switches might be written in the form a+,b+,c- etc. with no spaces between them
            if FSourcePtr^ <> ',' then
              Break;
            Inc(FSourcePtr);
            if not UnicodeIsIdentifierStart(Word(FSourcePtr^)) then
              Break;
          end
          else
            Break; // single letter without + or - is a directive like $E with extension or $I with filename
        Ord('a')..Ord('z'): // single letter directive derived from a multiletter directive, check for on/off
          begin
            SkipBlanks;
            // check 'on' or 'off'
            DirectiveID := ScanIdentifier;
            IsOn := WideUpperCase(DirectiveID) = 'ON';
            IsOff := not IsOn;
            if IsOff then
              IsOff := WideUpperCase(DirectiveID) = 'OFF';

            if IsOn or IsOff then
              FCompilerSwitch[ID - 97] := IsOn
            else
              LexerError(SInvalidDirective);
            Break;
          end;
        0, 1: // ifdef, ifndef
          begin
            // enter conditional source part
            SkipBlanks;
            DirectiveID := ScanIdentifier;
            if (ID = 1) xor FDefines.Find(DirectiveID, ID) then
              Break // conditional part must be parsed normally
            else
            begin
              // conditional part must be skipped
              SkipToDirectiveEnd(UseStar);
              SkipToElseOrEndIf; // look for a matching $endif before returning
            end;
            // at this point all code until the final {$endif} clause has been parsed
            Break;
          end;
        2: // define (a new conditional symbol)
          begin
            SkipBlanks;
            DirectiveID := ScanIdentifier;
            FDefines.Add(DirectiveID);
            Break;
          end;
        3: // undef (a conditional symbol)
          begin
            SkipBlanks;
            DirectiveID := ScanIdentifier;
            if FDefines.Find(DirectiveID, ID) then
              FDefines.Delete(ID);
            Break;
          end;
        4: // else
          begin
            // if we come here regulary then we just parsed an ifdef construct so the 'else'
            // branch can be skipped
            SkipToEndIf;
            Break;
          end;
        5: // endif
          begin
            // if we come here then we just parsed either an ifdef endif or else endif construct
            // which is now finished and needs no further action
            Break;
          end;
        6: // ifopt
          begin
            // enter conditional source part
            SkipBlanks;
            DirectiveID := ScanIdentifier;
            ID := IDFromDirective(DirectiveID);
            if not (ID in [Ord('A')..Ord('Z')]) then
              LexerError(SInvalidDirective);

            if FCompilerSwitch[ID - 65] then
            begin
              // conditional part must be parsed normally
              Break;                    
            end
            else
            begin
              // conditional part must be skipped
              SkipToDirectiveEnd(UseStar);
              SkipToElseOrEndIf; // look for a matching $endif before returning
            end;
            // at this point all code until the final {$endif} clause has been parsed
            Break;
          end;
      else // unknown or unimportant directive
        Break;
      end;
    until False;
  end;

  SkipToDirectiveEnd(UseStar);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.NeedChars(Count: Cardinal): Boolean;

// Ensure that at least Count bytes are available in the current input buffer if possible.
// If less input is available than Count bytes then Result is set to False.

begin
  Result := True;

  // check if we need to load new data
  if Count > Cardinal(FSourceEnd - FSourcePtr) then
  begin
    ReadBuffer;
    Result := Count <= Cardinal(FBufEnd - FSourcePtr);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.ReadBuffer;

var
  Count: Integer;
  ANSIChars: array[0..LexerBufferSize - 1] of Char;

begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd^ := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FSourcePtr^, FBuffer^, 2 * Count);
  FBufPtr := FBuffer + Count;

  // The actual source may not be Unicode (and if Unicode then in two different byte orders) so handle this properly.
  if FIsUnicode then
  begin
    // Count needs the character count, not the byte count as TStream.Read returns.
    Count := FStream.Read(FBufPtr^, 2 * (FBufEnd - FBufPtr)) div 2;
  end
  else
  begin
    // ANSI source, data must be converted to Unicode,
    // the source is expected as being plain text (no UTF8, MBCS etc.)
    Count := FStream.Read(ANSIChars, FBufEnd - FBufPtr);
    ExpandANSIString(ANSIChars, FBufPtr, Count);
  end;
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr + Count;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
      LexerError(SLineTooLong);
  end;
  FSaveChar := FSourceEnd^;
  FSourceEnd^ := WideNull;
  if FNeedSwap then
    StrSwapByteOrder(FBufPtr);
  Inc(FBufPtr, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.AdvanceSource(Amount: Cardinal = 1);

begin
  NeedChars(Amount);
  Inc(FSourcePtr, Amount);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.GetCompilerOption(Option: Char): Boolean;

begin
  Option := Upcase(Option);
  if Option in ['A'..'Z'] then
    Result := FCompilerSwitch[Ord(Option) - 65]
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipConditionalPart(WithElse: Boolean);

// skips all text until an {$endif} part is found, handles recursion too
// If WithElse is True then an $else part is considered as correct end too. Otherwise an exception is raised.

var
  DirectiveID: WideString;
  ID: Integer;
  UseStar: Boolean;

begin
  repeat
    // Skip all characters until either a comment ('{', '//' and '(*') or a string started.
    SkipUntil('{(''/');
    case FSourcePtr^ of
      WideNull:
        LexerError(SUnexpectedEOF);
      '''': // string start
        SkipString;
      '/': // Single line comment.
        if NeedChars(2) and ((FSourcePtr + 1)^ = '/') then
          SkipLine;
    else
      // possible comment start
      if (FSourcePtr^ = '{') or
        NeedChars(2) and (FSourcePtr^ = '(') and ((FSourcePtr + 1)^ = '*') then
      begin
        UseStar := FSourcePtr^ <> '{';
        // perhaps found a directive, check this and then check for end or recursive definitions
        // (no inherent limit to 16 levels here as in Delphi)
        AdvanceSource(1 + Ord(UseStar));
        if FSourcePtr^ = '$' then
        begin
          AdvanceSource;
          // determine what kind of directive we actually have
          if UnicodeIsIdentifierStart(Word(FSourcePtr^)) then
          begin
            // extract directive identifier
            DirectiveID := ScanIdentifier;

            ID := IDFromDirective(DirectiveID);
            case ID of
              0, 1, 6: // ifdef, ifndef, ifopt
                begin
                  // recursive conditional source part, reenter skipping
                  SkipToDirectiveEnd(UseStar);
                  SkipToEndIf;
                  SkipToDirectiveEnd(UseStar);
                end;
              4: // else
                begin
                  // coming across an $else when looking for $endif does only break the
                  // loop if we are looking for the $else part, otherwise continue to $endif
                  if WithElse then
                    Exit;
                end;
              5: // endif
                begin
                  // $endif is what we are looking for
                  Exit;
                end;
            end;
          end;
        end
        else
        begin
          SkipUntil('}'); // Normal comment. Skip until the comment's end.
          AdvanceSource;  // Skip the closing curly bracket too.
        end;
      end
      else
        AdvanceSource;
    end;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.ScanIdentifier: WideString;

var
  P: PWideChar;

begin
  P := FSourcePtr;
  while UnicodeIsIdentifierPart(Word(P^)) do
    Inc(P);
  SetString(Result, FSourcePtr, P - FSourcePtr);
  FSourcePtr := P;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SetCompilerOption(Option: Char; const Value: Boolean);

begin
  Option := Upcase(Option);
  if Option in ['A'..'Z'] then
    FCompilerSwitch[Ord(Option) - 65] := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SetDefines(const Value: TWideStringList);

begin
  FDefines.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipBlanks;

begin
  while True do
  begin
    case FSourcePtr^ of
      WideNull:
        begin
          ReadBuffer;
          if FSourcePtr^ = WideNull then
            Break;
          Continue;
        end;
      WideLineSeparator,
      WideLineFeed:
        DoNewLine;
      WideCarriageReturn:
        // increase line counter only for Macintosh style text (CR only)
        // otherwise just ignore CR characters
        if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
          DoNewLine;
    else
      if not UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) then
      begin
        Break;
      end;
    end;
    AdvanceSource;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipComments;

// skips any Delphi style comment
// Note: The styles //, {} and (* *) are currently hard coded hence you cannot skip C comments like /* */.
//       In order to determine whether the current input is to be considered as being a comment we need some lookahead.

begin
  repeat
    SkipBlanks;
    case FSourcePtr^ of
      '{':                              // multi line comment first type (consider case of compiler directives
        if NeedChars(2) and ((FSourcePtr + 1)^ <> '$') then
        begin
          SkipUntil('}');
          if FSourcePtr^ = WideNull then
            LexerError(SUnexpectedEOF)
          else
            AdvanceSource;
        end
        else
          Break;                        // get out of here if we found a compiler directive
      '/':
        begin
          if NeedChars(2) and ((FSourcePtr + 1)^ = '/') then
            SkipLine
          else
            Break;                      // some other construct but not a comment
        end;
      '(':
        begin
          if NeedChars(3) and ((FSourcePtr + 1)^ = '*') and ((FSourcePtr + 2)^ <> '$') then
          begin
            // multi line comment second type, skip to '*)' combination
            repeat
              SkipUntil('*');
              if NeedChars(2) and (FSourcePtr^ = '*') and ((FSourcePtr + 1)^ = ')') then
                Break
              else
                if FSourcePtr^ <> WideNull then
                  AdvanceSource;
            until FSourcePtr^ = WideNull;
            if FSourcePtr^ = WideNull then
              LexerError(SUnexpectedEOF)
            else
              AdvanceSource(2);
          end
          else
            Break;                      // some other construct but not a comment
        end;
    else
      Break;                            // no more comments, get out of here
    end;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipLine;

// Skips all characters until the end of line.

var
  S: WideString;

begin
  S := WideLineSeparator;
  S := S + WideLineFeed + WideCarriageReturn;
  AdvanceSource;
  SkipUntil(S);
  SkipBlanks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipString;

// skips a Delphi string (used to skip conditional code parts)
// On enter FSourcePtr points to the initial ' character on exit it points to the first character after the terminating
// ' character.

begin
  repeat
    AdvanceSource;
    // find next apostrophe
    SkipUntil('''');
    // advance to position after apostrophe
    if FSourcePtr^ = '''' then
      AdvanceSource;
    // is there another apostrophe then continue looking for string end, otherwise get out of here
    if FSourcePtr^ <> '''' then
      Break;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipToEndIf;

// skips all text until an {$endif} part is found, handles recursion too

begin
  SkipConditionalPart(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipToDirectiveEnd(UseStar: Boolean);

begin
  // skip to closing curly bracket or *)
  if UseStar then
  begin
    repeat
      SkipUntil('*');
      case FSourcePtr^ of
        WideNull:
          LexerError(SInvalidDirective);
        '*':
          begin
            AdvanceSource;
            if FSourcePtr^ = ')' then
            begin
              AdvanceSource;
              Break;
            end;
          end;
      end;
    until False;
  end
  else
  begin
    SkipUntil('}');
    if FSourcePtr^ = '}' then
      AdvanceSource
    else
      LexerError(SInvalidDirective);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipToElseOrEndIf;

// skips all text until either an {$else} or an {$endif} part is found, handles recursion too

begin
  SkipConditionalPart(True);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.SkipUntil(S: WideString);

// skips all characters until one of the characters in S is found or the source is exhausted

var
  L: Cardinal;
  Ch: WideChar;

begin
  L := Length(S);
  if L = 1 then
  begin
    // optimized version for single char
    Ch := S[1];
    while FSourcePtr^ <> Ch do
    begin
      case FSourcePtr^ of
        WideNull:
          begin
            ReadBuffer;
            if FSourcePtr^ = WideNull then
              Exit;
            Continue;
          end;
        WideLineSeparator, WideLineFeed:
          DoNewLine;
        WideCarriageReturn:
          // increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters
          if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end
  else
  begin
    // check for all characters in S
    while StrScanW(PWideChar(S), FSourcePtr^, L) = nil do
    begin
      case FSourcePtr^ of
        WideNull:
          begin
            ReadBuffer;
            if FSourcePtr^ = WideNull then
              Exit;
          end;
        WideLineSeparator, WideLineFeed:
          DoNewLine;
        WideCarriageReturn:
          // increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters
          if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDelphiLexer.ShowToken(Token: WideChar);

// Used for debugging purposes and shows a message box with a description string of the current token along
// with its value.

var
  TokenName: WideString;

begin
  case Token of
    toEOF:
      TokenName := '(end of file)';
    toBOF:
      TokenName := '(begin of file)';
    toSymbol:
      TokenName := '(symbol) = "' + TokenString + '"';
    toString:
      TokenName := '(string) = "' + TokenString + '"';
    toInteger:
      TokenName := '(Integer) = "' + TokenString + '"';
    toFloat:
      TokenName := '(float) = "' + TokenString + '"';
  else
    TokenName := Format('(other) = "%s" (hex: %x)', [WideString(Token), Word(Token)]) ;
  end;
  MessageBoxW(0, PWideChar(TokenName), 'Current token', MB_OK or MB_APPLMODAL);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.SourcePos: Integer;

begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenFloat: Extended;

begin
  if FFloatType <> WideNull then
    Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> WideNull then
    AdvanceSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenInteger: Integer;

begin
  Result := StrToInt(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenInt64: Int64;

begin
  Result := StrToInt64(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenString: WideString;

begin
  if FToken = toString then
    Result := FTokenString
  else
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenSymbolIs(const S: WideString): Boolean;

begin
  Result := (Token = toSymbol) and WideSameText(S, TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TDelphiLexer.TokenComponentIdent: WideString;

var
  P: PWideChar;

begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not UnicodeIsIdentifierStart(Word(P^)) then
      LexerError(SIdentifierExpected);
    repeat
      Inc(P)
    until not UnicodeIsIdentifierPart(Word(P^));
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

//----------------- TSQLLexer ------------------------------------------------------------------------------------------

constructor TSQLLexer.Create(Stream: TStream);

begin
  FStream := Stream;
  FSavePoint := FStream.Position;
  GetMem(FBuffer, 2 * LexerBufferSize);

  Reset;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSQLLexer.Destroy;

begin
  FStream.Seek(Integer(FTokenPtr) - Integer(FBufPtr), 1);
  FreeMem(FBuffer);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.AdvanceSource(Amount: Cardinal = 1);

begin
  NeedChars(Amount);
  Inc(FSourcePtr, Amount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.DoNewLine;

begin
  Inc(FSourceLine);
  FLineStart := FSourcePtr + 1;
  if Assigned(FOnNewLine) then
    FOnNewLine(Self, FSourceLine);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.GetToken: WideChar;

begin
  if FToken = toBOF then
    NextToken;
  Result := FToken;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.NeedChars(Count: Cardinal): Boolean;

// Ensure that at least Count bytes are available in the current input buffer if possible.
// If less input is available than Count bytes then Result is set to False.

begin
  Result := True;

  // check if we need to load new data
  if Count > Cardinal(FSourceEnd - FSourcePtr) then
  begin
    ReadBuffer;
    Result := Count <= Cardinal(FBufEnd - FSourcePtr);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.ReadBuffer;

var
  Count: Integer;

begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd^ := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then
    Move(FSourcePtr^, FBuffer^, 2 * Count);
  FBufPtr := FBuffer + Count;

  Count := FStream.Read(FBufPtr^, 2 * (FBufEnd - FBufPtr)) div 2;

  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr + Count;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
      LexerError(SLineTooLong);
  end;
  FSaveChar := FSourceEnd^;
  FSourceEnd^ := WideNull;

  if FNeedSwap then
    StrSwapByteOrder(FBufPtr);
  Inc(FBufPtr, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.SkipBlanks;

// Skips white spaces like new line, carriage return and any other white space defined by the Unicode standard.

begin
  while True do
  begin
    case FSourcePtr^ of
      WideNull:
        begin
          ReadBuffer;
          if FSourcePtr^ = WideNull then
            Break;
          Continue;
        end;
      WideLineSeparator,
      WideLineFeed:
        DoNewLine;
      WideCarriageReturn:
        // increase line counter only for Macintosh style text (CR only)
        // otherwise just ignore CR characters
        if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
          DoNewLine;
    else
      if not UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) then
      begin
        Break;
      end;
    end;
    AdvanceSource;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.SkipComments;

// Skips any SQL comment.
// Supported styles are /+ ... */ (C like comments), # and -- when followed by a white space. The last two styles
// represent single line comments and everything following them on the same line is considered as part of the comment.

begin
  repeat
    SkipBlanks;
    case FSourcePtr^ of
      '#': // Single line comment, first type.
        SkipLine;
      '-': // Potential single line comment.
        begin
          if NeedChars(3) and ((FSourcePtr + 1)^ = '-') and UnicodeIsWhiteSpace(UCS4Char((FSourcePtr + 2)^)) then
            // Single line comment type 2 found.
            SkipLine;
        end;
      '/': // Potential multi line comment.
        begin
          if NeedChars(2) and ((FSourcePtr + 1)^ = '*') then
          begin
            Inc(FSourcePtr, 2);
            // Multi line comment, skip to '*/' combination.
            repeat
              SkipUntil('*');
              if NeedChars(2) and (FSourcePtr^ = '*') and ((FSourcePtr + 1)^ = '/') then
                Break
              else
                if FSourcePtr^ <> WideNull then
                  AdvanceSource;
            until FSourcePtr^ = WideNull;
            if FSourcePtr^ = WideNull then
              LexerError(SUnexpectedEOF)
            else
              AdvanceSource(2);
          end
          else
            // Some other construct but not a comment.
            Break;
        end;
    else
      // No more comments, get out of here.
      Break;
    end;
  until False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.SkipLine;

// Skips all characters until the end of line.

var
  S: WideString;

begin
  S := WideLineSeparator;
  S := S + WideLineFeed + WideCarriageReturn;
  AdvanceSource;
  SkipUntil(S);
  SkipBlanks;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.SkipUntil(S: WideString);

// Skips all characters until one of the characters in S is found or the source is exhausted.

var
  L: Cardinal;
  Ch: WideChar;

begin
  L := Length(S);
  if L = 1 then
  begin
    // Optimized version for single char.
    Ch := S[1];
    while FSourcePtr^ <> Ch do
    begin
      case FSourcePtr^ of
        WideNull:
          begin
            ReadBuffer;
            if FSourcePtr^ = WideNull then
              Exit;
            Continue;
          end;
        WideLineSeparator, WideLineFeed:
          DoNewLine;
        WideCarriageReturn:
          // Increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters.
          if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end
  else
  begin
    // Check for all characters in S.
    while StrScanW(PWideChar(S), FSourcePtr^, L) = nil do
    begin
      case FSourcePtr^ of
        WideNull:
          begin
            ReadBuffer;
            if FSourcePtr^ = WideNull then
              Exit;
          end;
        WideLineSeparator, WideLineFeed:
          DoNewLine;
        WideCarriageReturn:
          // Increase line counter only for Macintosh style text (CR only)
          // otherwise just ignore CR characters.
          if NeedChars(2) and ((FSourcePtr + 1)^ <> WideLineFeed) then
            DoNewLine;
      end;
      AdvanceSource;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.CheckToken(T: WideChar);

begin
  if Token <> T then
    case T of
      toSymbol:
        LexerError(SIdentifierExpected + ' but ''' + Token + ''' found');
      toString:
        LexerError(SStringExpected + ' but ''' + Token + ''' found');
      toInteger, toFloat:
        LexerError(SNumberExpected + ' but ''' + Token + ''' found');
    else
      LexerError('"' + WideString(T) + '" expected but ' + Token + ' found');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.CheckTokenSymbol(const S: WideString);

begin
  if not TokenSymbolIs(S) then
    LexerError('"' + S + '" expected but "' + TokenString + '" found');
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.NextToken: WideChar;

// Advances the lexer to the next valid token. Comments and white spaces are skipped.

var
  P: PWideChar;
  StringEndChar: WideChar;

begin
  Result := toEOF;

  while True do
  begin
    // First remove heading comments (this will automatically skip blanks too).
    SkipComments;

    // At this point we have the start of a valid token, now extract and return it to the caller.
    P := FSourcePtr;
    FTokenPtr := P;
    // Low lines (underlines) are not part of the formal Unicode identifier syntax. So test for them explicitly.
    if (P^ = '_') or UnicodeIsIdentifierStart(Word(P^)) then
    begin
      Inc(P);
      // XML allows namespace identification. The name space is kept as part of the identifier here.
      // Additionally, there can contain any number of hyphens and periods.  
      while UnicodeIsIdentifierPart(Word(P^)) do
        Inc(P);
      Result := toSymbol;
    end
    else
    begin
      case P^ of
        '''', '"':
          begin
            if P^ = '"' then
              StringEndChar := '"'
            else
              StringEndChar := '''';
            repeat
              Inc(P);
              while not (P^ in [WideNull, WideLF, WideCR, StringEndChar, WideChar('\')]) and (P^ <> WideLineSeparator) do
                Inc(P);
              // If there is an escape character then ignore the next character unless we reached the input end.
              if (P^ = '\') and ((P + 1)^ <> WideNull) then
                Inc(P)
              else
                Break;
            until False;
            if P^ <> StringEndChar then
              LexerError(SInvalidString)
            else
              Inc(P);
            Result := toString;
          end;
        '+', '-', '0'..'9': // Integer or float number
          begin
            if (P^ = '+') or (P^ = '-') then
              Inc(P);
            if NeedChars(2) and (P^ = '0') and (P^ = 'x') then
            begin
              // Found a hex number. Note: only small x is allowed currently.
              Inc(P, 2);
              while UnicodeIsHexDigit(Word(P^)) do
                Inc(P);
              Result := toHexNumber;
            end
            else
            begin
              Inc(P);
              while UnicodeIsNumber(Word(P^)) do
                Inc(P);
              Result := toInteger;

              // Check for a floating point number (possibly with exponent).
              if P^ in [WideChar('.'), WideChar('e'), WideChar('E')] then
              begin
                // It is actually a floating point value.
                Result := toFloat;

                // Skip irrelevant period character if directly followed by exponent character.
                if (P^ = '.') and ((P + 1)^ in [WideChar('e'), WideChar('E')]) then
                  Inc(P);
                // Skip exponent letter if there is one followed by a plus or minus sign
                if (P^ in [WideChar('e'), WideChar('E')]) and ((P + 1)^ in [WideChar('+'), WideChar('-')]) then
                  Inc(P);

                // Skip whatever left over, the period, the expontent symbol or the exponent sign.
                Inc(P);
                while UnicodeIsNumber(Word(P^)) do
                  Inc(P);
              end;
            end;
          end;
      else
        // Any other symbol not consumed above.
        Result := P^;
        if Result <> toEOF then
          Inc(P);
      end;
    end;

    FSourcePtr := P;  
    FToken := Result;
    Break; // if we come here then we have a valid token and can return
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.Reset;

// Reset input buffer and internal buffer to the start of the parsing input (e.g. for restarting the
// parsing process).

var
  BOM: WideChar;
  Count: Integer;

begin
  FStream.Position := FSavePoint;

  // Determine byte order in source if possible.
  // If there is no byte order mark at stream start then assume LSB first order (little endian, no swap).
  Count := FStream.Read(BOM, SizeOf(BOM));
  if (BOM = BOM_LSB_FIRST) or (BOM = BOM_MSB_FIRST) then
    FNeedSwap := BOM = BOM_MSB_FIRST
  else
    FStream.Seek(-Count, soFromCurrent);

  FBuffer[0] := WideNull;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + LexerBufferSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;

  FLineStart := FSourcePtr;
  FSourceLine := 1;
  FToken := toBOF;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.ScanRawText: WideString;

// Skips any leading white spaces and comments and collects then all input until a following white space is found.
// The input is not classified in any way but returned as it appears in the input.

var
  P: PWideChar;
  
begin
  SkipComments;
  P := FSourcePtr;
  while not UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) and (FSourcePtr^ <> WideNull) do
    AdvanceSource;

  SetString(Result, P, FSourcePtr - P);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.TokenFloat: Extended;

begin
  Result := StrToFloat(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.TokenInt64: Int64;

begin
  Result := StrToInt64(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.TokenInteger: Integer;

begin
  Result := StrToInt(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.TokenString: WideString;

begin
  SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLLexer.TokenSymbolIs(const S: WideString): Boolean;

begin
  Result := (FToken = toSymbol) and WideSameText(S, TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLLexer.LexerError(const Message: WideString);

var
  S: WideString;

begin
  if Assigned(FOnLexerError) then
    FOnLexerError(Point(FSourcePtr - FLineStart + 1, FSourceLine), Message) // 1 based position
  else
  begin
    S := Message + ' on line ' + IntToStr(FSourceLine);
    MessageBoxW(0, PWideChar(S), 'Lexer Error', MB_OK or MB_ICONERROR or MB_APPLMODAL);
    Abort;
  end;
end;

//----------------- TSQLTokenizer --------------------------------------------------------------------------------------

procedure TSQLTokenizer.AccumulateTokenString;

// Takes the buffer content in the range from the current token pointer to the current source pointer and
// adds it to the so far accumulated token string.

var
  S: WideString;

begin
  SetString(S, FTokenPtr, FSourcePtr - FTokenPtr);
  FTokenString := FTokenString + S;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.AdvanceSource(Amount: Cardinal = 1): Boolean;

begin
  Result := NeedChars(Amount);
  if Result then
    Inc(FSourcePtr, Amount)
  else
    FSourcePtr := FSourceEnd;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.GetToken: WideChar;

begin
  if FToken = toBOF then
    NextToken;
  Result := FToken;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.NeedChars(Count: Cardinal): Boolean;

// Ensure that at least Count characters are available in the current input buffer if possible.
// If less input is available then False is returned, otherwise True.

begin
  Result := True;

  // Check if we need to load new data.
  if Count > Cardinal(FSourceEnd - FSourcePtr) then
  begin
    ReadBuffer;
    Result := Count <= Cardinal(FSourceEnd - FSourcePtr);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLTokenizer.ReadBuffer;

var
  RemainingChars: Integer;
  NewChars: Integer;
  ANSIBuffer: array[0..LexerBufferSize] of Char;
  WS: Widestring;

begin
  // Copy all not yet consumed input to the start of the buffer.
  if FSourcePtr > @FBuffer then
  begin
    // Keep what we currently have in the token string.
    AccumulateTokenString;

    Inc(FOrigin, FSourcePtr - @FBuffer);
    RemainingChars := FSourceEnd - FSourcePtr;
    if RemainingChars > 0 then
      Move(FSourcePtr^, FBuffer, 2 * RemainingChars);
    FSourceEnd := FBuffer + RemainingChars;
  end;

  if Assigned(FStream) then
  begin
    if FIsAnsi then
    begin
      NewChars := FStream.Read(ANSIBuffer, FBufEnd - FSourceEnd);
      ANSIBuffer[NewChars] := #0;
      WS := ANSIBuffer; // Convert ANSI to Unicode using the current user locale.
      Move(PWideChar(WS)^, FSourceEnd^, 2 * NewChars);
    end
    else
      NewChars := FStream.Read(FSourceEnd^, 2 * (FBufEnd - FSourceEnd)) div 2
  end
  else
    NewChars := 0;
  FSourcePtr := FSourceEnd;
  Inc(FSourceEnd, NewChars);
  FSourceEnd^ := WideNull;
  if FNeedSwap then
    StrSwapByteOrder(FSourcePtr);
  FSourcePtr := @FBuffer;
  FTokenPtr := @FBuffer;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.NextToken: WideChar;

// Advances the lexer to the next valid token. Comments and white spaces are skipped.

  //--------------- local functions --------------------------------------------

  procedure ScanString(EndChar: WideChar);

  begin
    repeat
      while not (FSourcePtr^ in [WideNull, WideLF, WideCR, EndChar, WideChar('\')]) and (FSourcePtr^ <> WideLineSeparator) do
        if not AdvanceSource then
          Break;

      // If there is an escape character then ignore the next character unless we reached the input end.
      if (FSourcePtr^ = '\') then
      begin
        // Skip mask and masked character.
        if not AdvanceSource(2) then
          Break;
      end
      else
        Break;
    until False;
  end;

  //----------------------------------------------------------------------------
  
  procedure ScanLine;

  begin
    while not (FSourcePtr^ in [WideNull, WideLF, WideCR]) and (FSourcePtr^ <> WideLineSeparator) do
      if not AdvanceSource then
        Break;
  end;

  //----------------------------------------------------------------------------

  procedure ScanSymbol;

  begin
    // Low lines (underlines) are not part of the formal Unicode identifier syntax. So test for them explicitly.
    if AdvanceSource then
    begin
      // SQL allows qualifiers. The qualifiers are kept as part of the symbol here.
      while UnicodeIsIdentifierPart(Word(FSourcePtr^)) or (FSourcePtr^ in [WideChar('_'), WideChar('.')]) do
        if not AdvanceSource then
          Break;
      Result := toSymbol;
    end;
  end;

  //----------------------------------------------------------------------------

  procedure ScanPureSymbol;

  begin
    if AdvanceSource then
    begin
      // No qualifiers allowed here.
      while UnicodeIsIdentifierPart(Word(FSourcePtr^)) or (FSourcePtr^ = '_') do
        if not AdvanceSource then
          Break;
      Result := toSymbol;
    end;
  end;

  //--------------- end local functions ----------------------------------------

var
  ScanNumber: Boolean;
  QuoteChar: WideChar;

begin
  if FSourcePtr^ = WideNull then
    ReadBuffer;

  Result := toEOF;

  // Initialize token start pointer for this run.
  FTokenPtr := FSourcePtr;
  FTokenString := '';
  if FSourcePtr^ <> WideNull then
  begin
    // Input scanning depends on current state, which could be a left over from a previous pass.
    case FState of
      tsNormal:
        begin
          case FSourcePtr^ of
            'x', 'X':
              begin
                // Special case: hex number in quotes, but only if 'x' is directly followed by a quote char.
                if NeedChars(2) and ((FSourcePtr + 1)^ in [WideChar('"'), WideChar('''')]) then
                begin
                  // Hex string.
                  FState := tsHexString;
                  AdvanceSource;
                  QuoteChar := FSourcePtr^;
                  AdvanceSource;
                  ScanString(QuoteChar);
                  Result := toHexString;
                  if FSourcePtr^ = QuoteChar then
                    FState := tsNormal;
                  AdvanceSource;
                end
                else
                begin
                  // Normal identifier.
                  ScanSymbol;
                end;
              end;
            '''': // String literal
              begin
                FState := tsSingleQuoteString;
                if AdvanceSource then
                begin
                  ScanString('''');
                  if FSourcePtr^ = '''' then
                    FState := tsNormal;
                  if FSourcePtr^ <> WideNull then
                    AdvanceSource;
                end;
                Result := toString;
              end;
            '"': // String literal
              begin
                FState := tsDoubleQuoteString;
                if AdvanceSource then
                begin
                  ScanString('"');
                  if FSourcePtr^ = '"' then
                    FState := tsNormal;
                  if FSourcePtr^ <> WideNull then
                    AdvanceSource;
                end;
                Result := toString;
              end;
            '`': // String literal
              begin
                FState := tsBackQuoteString;
                if AdvanceSource then
                begin
                  ScanString('`');
                  if FSourcePtr^ = '`' then
                    FState := tsNormal;
                  if FSourcePtr^ <> WideNull then
                    AdvanceSource;
                end;
                Result := toString;
              end;
            '#': // Single line comment, first type.
              begin
                ScanLine;
                Result := toSLComment;
              end;
            '/': // Potential single or multi line comment or an embedded command.
              begin
                if NeedChars(2) then
                begin
                  if (FSourcePtr + 1)^ = '*' then
                  begin
                    AdvanceSource(2);

                    // Check if we have an embedded command. Otherwise act like having a multi line comment.
                    if FSourcePtr^ = '!' then
                    begin
                      FState := tsEmbeddedCommand;
                      Result := toEmbeddedCommand;
                    end
                    else
                    begin
                      FState := tsComment;
                      Result := toMLComment;
                    end;

                    // Skip to '*/' combination or end of line.
                    repeat
                      while not (FSourcePtr^ in [WideNull, WideLF, WideCR, WideChar('*')]) and (FSourcePtr^ <> WideLineSeparator) do
                        if not AdvanceSource then
                          Break;

                      if FSourcePtr^ = '*' then
                      begin
                        if NeedChars(2) and ((FSourcePtr + 1)^ = '/') then
                        begin
                          AdvanceSource(2);
                          FState := tsNormal;
                          Break;
                        end
                        else
                          if not AdvanceSource then
                            Break;
                        // If * is not followed by / then continue scanning.
                      end
                      else
                        Break;
                    until False;
                  end
                  else
                  begin
                    AdvanceSource;
                    Result := '/';
                  end;

                  if Result = toEOF then
                  begin
                    Result := FSourcePtr^;
                    AdvanceSource;
                  end;
                end
                else
                begin
                  // There aren't enough chars left. So it is a simple slash.
                  AdvanceSource;
                  Result := '/';
                end;
              end;
            '+', '-',
            '0'..'9': // Complicated case here. This can either be a single minus/plus, a double minus,
                      // a single line comment introduced with --<space> or a signed number (int, float, hex).
              begin
                case FSourcePtr^ of
                  '-':
                    begin
                      // Sign, operation or comment.
                      if not NeedChars(2) then
                      begin
                        // No more input available, it's a single minus.
                        Result := '-';
                        AdvanceSource;
                      end
                      else
                      begin
                        AdvanceSource;

                        // Check single line comment type 3. Must be dash dash space or dash dash eof.
                        if FSourcePtr^ = '-' then
                        begin
                          if (NeedChars(2) and UnicodeIsWhiteSpace(UCS4Char((FSourcePtr + 1)^))) or
                            ((FSourcePtr + 1)^ = WideNull) then
                          begin
                            ScanLine;
                            Result := toSLComment;
                          end
                          else
                            // There is a double minus but with additional content after it. Return the first minus only.
                            Result := '-';
                        end;
                      end;
                    end;
                  '+':
                    begin
                      // Sign or operation.
                      if not NeedChars(2) then
                      begin
                        // No more input available, it's a single plus.
                        Result := '+';
                        AdvanceSource;
                      end
                      else
                      begin
                        AdvanceSource;

                        // If there is anything else but a digit then it is a + operation not a sign.
                        if not UnicodeIsNumber(Word(FSourcePtr^)) then
                          Result := '+';
                      end;
                    end;
                end;

                // Stop scanning here if we found a single minus, plus or SL comment type 3.
                if Result = toEOF then
                begin
                  // Must be a number. If there was a minus sign then is is skipped already.
                  if FSourcePtr^ = '+' then
                    AdvanceSource;

                  // Number or (for MySQL) identifier starting with one or more digits.
                  if NeedChars(3) and
                    (FSourcePtr^ = '0') and
                    ((FSourcePtr + 1)^ in [WideChar('x'), WideChar('X')]) and
                    UnicodeIsHexDigit(Word((FSourcePtr + 2)^)) then
                  begin
                    // Found a hex number.
                    AdvanceSource(2);
                    while UnicodeIsHexDigit(Word(FSourcePtr^)) do
                      if not AdvanceSource then
                        Break;
                    Result := toHexNumber;
                  end
                  else
                  begin
                    if AdvanceSource then
                    begin
                      while UnicodeIsNumber(Word(FSourcePtr^)) do
                        if not AdvanceSource then
                          Break;
                      Result := toInteger;

                      ScanNumber := True;
                      // Check if we have a MySQL identifier here.
                      if UnicodeIsIdentifierPart(Word(FSourcePtr^)) or (FSourcePtr^ = '_') then
                      begin
                        // Special case letter 'e' must be handled. If followed by a digit or sign it belongs to a number.
                        if NeedChars(2) and not ((FSourcePtr^ in [WideChar('e'), WideChar('E')]) and UnicodeIsNumber(Word(FSourcePtr^))) then
                        begin
                          // It's an identifier.
                          // Low lines (underlines) are not part of the formal Unicode identifier syntax. So test for them explicitly.
                          while UnicodeIsIdentifierPart(Word(FSourcePtr^)) or (FSourcePtr^ = '_') do
                            if not AdvanceSource then
                              Break;
                          Result := toSymbol;
                          ScanNumber := False;
                        end;
                      end;

                      // Check for a floating point number (possibly with exponent).
                      if ScanNumber and (FSourcePtr^ in [WideChar('.'), WideChar('e'), WideChar('E')]) then
                      begin
                        // It is actually a floating point value.
                        Result := toFloat;

                        // Optional fractional part.
                        if FSourcePtr^ = '.' then
                        begin
                          repeat
                            if not AdvanceSource then
                              Break;
                          until not UnicodeIsNumber(Word(FSourcePtr^));
                        end;

                        // Optional exponent part.
                        if FSourcePtr^ in [WideChar('e'), WideChar('E')] then
                        begin
                          AdvanceSource;
                          if FSourcePtr^ in [WideChar('+'), WideChar('-')] then
                            AdvanceSource;
                          
                          while UnicodeIsNumber(Word(FSourcePtr^)) do
                            if not AdvanceSource then
                              Break;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            '@': // User or system variable
              begin
                AdvanceSource;
                if FSourcePtr^ = '@' then
                begin
                  if UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) then
                    Result := '@'
                  else
                  begin
                    Result := toSystemVariable;
                    AdvanceSource;
                  end;
                end
                else
                  if UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) then
                    Result := '@'
                  else
                    Result := toUserVariable;

                if Result <> '@' then
                begin
                  // Skip over all variable name characters.
                  while UnicodeIsIdentifierPart(UCS4Char(FSourcePtr^)) or (FSourcePtr^ = '_') do
                    AdvanceSource;
                end;
              end;
          else
            if (FSourcePtr^ = '_') or UnicodeIsIdentifierStart(Word(FSourcePtr^)) then
              ScanSymbol
            else
            begin
              if UnicodeIsWhiteSpace(UCS4Char(FSourcePtr^)) then
                Result := toWhiteSpace
              else
                // Any other symbol not consumed above.
                Result := FSourcePtr^;

              if Result <> toEOF then
                AdvanceSource;
            end;
          end;
        end;
      tsSingleQuoteString:
        begin
          // A single quoted string is still unfinished.
          Result := toString;
          ScanString('''');
          if FSourcePtr^ = '''' then
            FState := tsNormal;
          if FSourcePtr^ <> WideNull then
            AdvanceSource;
        end;
      tsHexString:
        begin
          // A hex string is still unfinished.
          Result := toString;
          ScanString('''');
          if FSourcePtr^ = '''' then
            FState := tsNormal;
          if FSourcePtr^ <> WideNull then
            AdvanceSource;
        end;
      tsDoubleQuoteString:
        begin
          // A double quoted string is still unfinished.
          Result := toString;
          ScanString('"');
          if FSourcePtr^ = '"' then
            FState := tsNormal;
          if FSourcePtr^ <> WideNull then
            AdvanceSource;
        end;
      tsBackQuoteString:
        begin
          // A back quoted string is still unfinished.
          Result := toString;
          ScanString('`');
          if FSourcePtr^ = '`' then
            FState := tsNormal;
          if FSourcePtr^ <> WideNull then
            AdvanceSource;
        end;
      tsEmbeddedCommand,
      tsComment:
        begin
          // Multi line comment or embedded command pending, skip to '*/' combination or end of line.
          Result := toMLComment;
          repeat
            while not (FSourcePtr^ in [WideNull, WideLF, WideCR, WideChar('*')]) and (FSourcePtr^ <> WideLineSeparator) do
              if not AdvanceSource then
                Break;
            if FSourcePtr^ = '*' then
            begin
              if NeedChars(2) and ((FSourcePtr + 1)^ = '/') then
              begin
                AdvanceSource(2);
                FState := tsNormal;
                Break;
              end
              else
                if not AdvanceSource then
                  Break;
              // If * is not followed by / then continue scanning.
            end
            else
            begin
              AdvanceSource;
              Break;
            end;
          until False;
        end;
    end;
  end;

  // Accumulate the rest of the token into FCurrentToken.
  AccumulateTokenString;

  FToken := Result;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSQLTokenizer.Initialize(Stream: TStream; State: TTokenizerState; IsANSI: Boolean);

// Resets the curent state of the tokenizer and initializes a few variables according to the values given.
// Used to start a new tokenizing run on fresh input.

var
  BOM: WideChar;
  Count: Integer;

begin
  FStream := Stream;
  FState := State;
  FNeedSwap := False;
  FOrigin := 0;
  FIsAnsi := IsANSI; // True if the input is an ANSI stream.

  // Determine byte order in source if possible.
  // If there is no byte order mark at stream start then assume LSB first order (little endian, no swap).
  if Assigned(FStream) then
  begin
    Count := Stream.Read(BOM, SizeOf(BOM));
    if (BOM = BOM_LSB_FIRST) or (BOM = BOM_MSB_FIRST) then
      FNeedSwap := BOM = BOM_MSB_FIRST
    else
      Stream.Seek(-Count, soFromCurrent);
  end;
  
  FBuffer[0] := WideNull;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + LexerBufferSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;

  FLineStart := FSourcePtr;
  FSourceLine := 1;
  FToken := toBOF;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.SourcePosition: Integer;

begin
  Result := FOrigin + (FSourcePtr - FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.TokenFloat: Extended;

begin
  Result := StrToFloat(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.TokenInt64: Int64;

begin
  Result := StrToInt64(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.TokenInteger: Integer;

begin
  Result := StrToInt(TokenString);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.TokenPosition: Integer;

begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSQLTokenizer.TokenSymbolIs(const S: WideString): Boolean;

var
  Len: Integer;
  Token: WideString;
  
begin
  Len := Length(S);
  Result := (FToken = toSymbol) and (Len = (FSourcePtr - FTokenPtr));
  if Result then
  begin
    Token := TokenString;
    Result := WideSameText(Token, S);
  end;
end;

//----------------- TrieStatistics -------------------------------------------------------------------------------------

procedure TrieStatistics(Trie: THashTrie; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);

// Helper procedure to return information about a trie.

  //--------------- local function --------------------------------------------

  procedure TreeStat(Item: THashTreeItem);

  var
    I, J: Integer;
    LinkedItem: THashLinkedItem;
    
  begin
    Inc(PeakCount);
    if Item.FLevel + 1 > MaxLevel then
      MaxLevel := Item.FLevel + 1;

    for J := 0 to High(Item.FItems) do
      if Assigned(Item.FItems[J]) then
      begin
        Inc(FillCount);
        if Item.FItems[J] is THashTreeItem then
          TreeStat(THashTreeItem(Item.FItems[J]))
        else
        begin
          I := 0;
          LinkedItem := THashLinkedItem(Item.FItems[J]);
          while Assigned(LinkedItem) do
          begin
            Inc(I);
            LinkedItem := LinkedItem.FNext;
          end;
          Inc(LengthStatistics[I]);
        end;
      end
      else
        Inc(EmptyCount);
  end;

  //--------------- end local function ----------------------------------------

begin
  MaxLevel := 0;
  PeakCount := 0;
  FillCount := 0;
  EmptyCount := 0;
  
  if Assigned(Trie.FRoot) then
    TreeStat(Trie.FRoot);
end;

//----------------- THashTreeItem --------------------------------------------------------------------------------------

constructor THashTreeItem.Create(AOwner: THashTrie);

begin
  FOwner := AOwner;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashTreeItem.Destroy;

begin
  Clear;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.Clear;

var
  I: Integer;
  LinkedItem: THashLinkedItem;
  
begin
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem then
      THashTreeItem(FItems[I]).Free
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        FOwner.DestroyItem(LinkedItem.FValue, LinkedItem.FData);
        LinkedItem := LinkedItem.FNext;
      end;
      THashLinkedItem(FItems[I]).Free;
    end;
  FItems := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.AddDown(Value, Hash: Cardinal; const Data: Pointer);

var
  I, J: Integer;
  TreeItem: THashTreeItem;
  LinkedItem: THashLinkedItem;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if FItems[I] = nil then
  begin
    FItems[I] := THashLinkedItem.Create(Value, Data, nil);
    Inc(FFilled);
  end
  else
    if FItems[I] is THashTreeItem then
      THashTreeItem(FItems[I]).AddDown(Value, ROR(Hash), Data)
    else
    begin
      J := 0;
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
        Inc(J)
      end;

      if J >= BucketSize then
      begin
        // full
        TreeItem := THashTreeItem.Create(FOwner);
        TreeItem.FLevel := FLevel + 1;
        LinkedItem := THashLinkedItem(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TreeItem.AddDown(LinkedItem.FValue, RORN(FOwner.HashValue(LinkedItem.FValue), FLevel + 1), LinkedItem.FData);
          LinkedItem := LinkedItem.FNext;
        end;
        TreeItem.AddDown(Value, ROR(Hash), Data);
        THashLinkedItem(FItems[I]).Free;
        FItems[I] := TreeItem;
      end
      else
        FItems[I] := THashLinkedItem.Create(Value, Data, THashLinkedItem(FItems[I]));
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTreeItem.Delete(Value, Hash: Cardinal);

var
  I: Integer;
  PrevLinkedItem,
  LinkedItem: THashLinkedItem;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[i] is THashTreeItem then
    begin
      THashTreeItem(FItems[I]).Delete(Value, ROR(Hash));
      if THashTreeItem(FItems[I]).FFilled = 0 then
      begin
        THashTreeItem(FItems[I]).Free;
        FItems[I] := nil;
      end;
    end
    else
    begin
      PrevLinkedItem := nil;
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          if PrevLinkedItem = nil then
          begin
            FItems[I] := LinkedItem.FNext;
            if FItems[I] = nil then
              Dec(FFilled);
          end
          else
            PrevLinkedItem.FNext := LinkedItem.FNext;
          LinkedItem.FNext := nil;
          FOwner.DestroyItem(LinkedItem.FValue, LinkedItem.FData);
          LinkedItem.Free;
          Exit;
        end;
        PrevLinkedItem := LinkedItem;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem then
      Result := THashTreeItem(FItems[I]).Find(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          Data := LinkedItem.FData;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.GetFilled: Integer;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem then
      Inc(Result, THashTreeItem(FItems[I]).GetFilled)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        Inc(Result);
        LinkedItem := LinkedItem.FNext;
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Modify(Value, Hash: Cardinal; const Data: Pointer): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem then
      Result := THashTreeItem(FItems[I]).Modify(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if FOwner.CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.ROR(Value: Cardinal): Cardinal;

begin
  Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.RORN(Value: Cardinal; Level: Integer): Cardinal;

begin
  Result := Value;
  while Level > 0 do
  begin
    Result := ROR(Result);
    Dec(Level);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTreeItem.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc): Boolean;

var
  I: Integer;
  LinkedItem: THashLinkedItem;

begin
  Result := False;
  for I := 0 to High(FItems) do
    if Assigned(FItems[I]) then
    begin
      if FItems[I] is THashTreeItem then
        Result := THashTreeItem(FItems[I]).Traverse(UserData, UserProc, TraverseProc)
      else
      begin
        LinkedItem := THashLinkedItem(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TraverseProc(UserData, UserProc, LinkedItem.FValue, LinkedItem.FData, Result);
          LinkedItem := LinkedItem.FNext;
        end;
      end;
      if Result then
        Break;
    end;
end;

//----------------- THashLinkedItem ------------------------------------------------------------------------------------

constructor THashLinkedItem.Create(Value: Cardinal; Data: Pointer; Next: THashLinkedItem);

begin
  FValue := Value;
  FData := Data;
  FNext := Next;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashLinkedItem.Destroy;

begin
  FNext.Free;
end;

//----------------- THashTrei ------------------------------------------------------------------------------------------

constructor THashTrie.Create;

begin
  inherited;

  FRoot := THashTreeItem.Create(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor THashTrie.Destroy;

begin
  FRoot.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Clear;

begin
  FRoot.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTrie.Find(Value, Hash: Cardinal; var Data: Pointer): Boolean;

begin
  Result := FRoot.Find(Value, Hash, Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function THashTrie.GetCount: Integer;

begin
  Result := FRoot.GetFilled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.AddDown(Value, Hash: Cardinal; const Data: Pointer);

begin
  FRoot.AddDown(Value, Hash, Data);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Delete(Value, Hash: Cardinal);

begin
  FRoot.Delete(Value, Hash);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure THashTrie.Traverse(UserData, UserProc: Pointer; TraverseProc: TTraverseProc);

begin
  FRoot.Traverse(UserData, UserProc, TraverseProc);
end;

//----------------- TStringHashTrie ------------------------------------------------------------------------------------

procedure TStringHashTrie.Add(const S: string; const Data: Pointer);

var
  Value: PChar;

begin
  Value := StrNew(PChar(S));
  AddDown(Cardinal(Value), HashStr(S), Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.CompareValue(Value1, Value2: Cardinal): Boolean;

begin
  if FCaseSensitive then
    Result := StrComp(PChar(Value1), PChar(Value2)) = 0
  else
    Result := StrIComp(PChar(Value1), PChar(Value2)) = 0
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Delete(const S: string);

begin
  inherited Delete(Cardinal(@S), HashStr(S));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.DestroyItem(var Value: Cardinal; var Data: Pointer);

begin
  if Assigned(FOnFreeItem) then
    FOnFreeItem(Self, PChar(Value), Data);

  StrDispose(PChar(Value));

  Value := 0;
  Data := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.Find(const S: string; var Data: Pointer): Boolean;

begin
  Result := Find(Cardinal(PChar(S)), HashStr(S), Data);
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.HashStr(const S: string): Cardinal;
                                                                                                     
begin
  if FCaseSensitive then
    Result := CalcStrCRC32(S)
  else
    Result := CalcStrCRC32(ANSIUpperCase(S));
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringHashTrie.HashValue(Value: Cardinal): Cardinal;

begin
  Result := HashStr(PChar(Value));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Traverse(UserData: Pointer; UserProc: TStrHashTraverseProc);

begin
  inherited Traverse(UserData, @UserProc, TraverseProc);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.TraverseProc(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);

begin
  TStrHashTraverseProc(UserProc)(UserData, PChar(Value), Data, Done);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.Traverse(UserData: Pointer; UserProc: TStrHashTraverseMeth);

begin
  inherited Traverse(UserData, @TMethod(UserProc), TraverseMeth);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringHashTrie.TraverseMeth(UserData, UserProc: Pointer; Value: Cardinal; Data: Pointer; var Done: Boolean);

type
  PTStrHashTraverseMeth = ^TStrHashTraverseMeth;

begin
  PTStrHashTraverseMeth(UserProc)^(UserData, PChar(Value), Data, Done);
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
finalization
  Directives.Free;
end.

