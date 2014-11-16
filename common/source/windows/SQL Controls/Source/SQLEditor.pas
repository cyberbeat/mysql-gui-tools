unit SQLEditor;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls,
  VirtualTrees, TntExtCtrls, TntComCtrls, TntClasses, TntStdCtrls,
  gnugettext, PNGImage,
  MySQLConnection, MySQLResultSet, AuxLists, TntMenus, Forms,
  UCESQLHighlighter, UCEHighlighter, UCEEditorKeyCommands,
  UCEHTMLHighlighter, UCEShared, UniCodeEditor, LexicalTools,
  TextSearch, Options, CommonTypes;

type
  TScriptBreakPoint = class(TObject)
    Line: Integer;
    constructor Create(Line: Integer);
  end;

  TScriptEditor = class(TUniCodeEdit, IOptionChangeListener)
  private
    FFileName: WideString;
    FBreakpoints: TObjectList;
    FBreakPointLineStyle: IUCELineStyle;
    FCurrentPositionLineStyle: IUCELineStyle;
    FBreakPointMarker: IUCELineMarker;
    FDebugInfoMarker: IUCELineMarker;
    FCurrentPositionMarker: IUCELineMarker;
    FEmptyMarker: IUCELineMarker;
    FOptionProvider: IOptionProvider;
  protected
    procedure AddExecutionMarker(Line: Integer);
    procedure AnalyzeLine(CommandEndPending: Boolean; Line: TUCELine);
    procedure DeleteLine(Sender: TObject; Line: TUCELine);
    procedure GutterMouseDown(Sender: TCustomUnicodeEdit; Button: TMouseButton; Shift: TShiftState; X, Y, Line: Integer);
    procedure RemoveExecutionMarker(Line: Integer);
    function Unquote(const S: WideString): WideString;
    procedure ValidateLine(Sender: TObject; Line: TUCELine);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoSearch(Sender: TObject;
      SearchText: WideString; ReplaceText: WideString;
      SearchOptions: TTextSearchOptions): Integer;
    procedure DoEnter; override;
    procedure OptionChanged;
  public
    constructor Create(AOwner: TComponent; OptionProvider: IOptionProvider); reintroduce;
    destructor Destroy; override;

    procedure ChangeLine(Sender: TObject; Line: TUCELine);

    procedure DoDisplaySearch(ShowReplacePage: Boolean = False);

    procedure ToggleBreakpoint(Line: Integer);
    procedure ClearBreakpoints;

    property FileName: WideString read FFileName write FFileName;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math,
  myx_public_interface,
  StringContainers;

const
  DefaultDelimiter = ';';
  
type
  // Data that is stored at line level.
  // TODO: Line data needs rework once a Code DOM is available.
  PStatement = ^TStatement;
  TStatement = record
    Start,                  // -1 if the statement does not start on this line, otherwise its char position (0-based).
    Stop: Integer;          // -1 if the statement does not end on this line, otherwise the position (0-based) of the
                            // last character of this statement.
    FNewSchema: WideString; // If non-empty then a new database is to be used from now on. No need to extract
                            // the commmand again when executing. 
  end;

  TLineData = class
  private
    FAnalyzed: Boolean;                // True if command start and end have been determined.
    FStatements: array of TStatement;
    FEndState: TTokenizerState;        // The state in which the lexer stopped when the line was parsed.
    FHasBreakPoint: Boolean;           // True if there is currently a break point set for this line, otherwise False.
    FCommandPending: Boolean;          // If True then there is an unfinished command on this or a previous line.
    FDelimiter: WideString;            // The statement delimiter, which is active for this line and all following.
    FNextStatementCounter: Integer;    // For use with GetFirstStatement/GetNextStatement.
  protected
    procedure AddStatement(X1, X2: Integer; Schema: WideString = '');
    function GetFirstStatement: PStatement;
    function GetFirstStatementEnd: Integer;
    function GetFirstStatementStart: Integer;
    function GetLastStatementEnd: Integer;
    function GetNextStatement: PStatement;
    function HasStatementStart: Boolean;

    property CommandPending: Boolean read FCommandPending write FCommandPending;
    property EndState: TTokenizerState read FEndState write FEndState;
  public
    constructor Create; virtual;
  end;

  // Style for our break point lines.
  TBreakPointLineStyle = class(TInterfacedObject, IUCELineStyle)
  protected
    function GetBackground: TColor;
    function GetFontStyles: TFontStyles;
    function GetForceFontStyles: Boolean;
    function GetForeground: TColor;
  end;

  // Style for current debug position.
  TCurrentPositionLineStyle = class(TInterfacedObject, IUCELineStyle)
  protected
    function GetBackground: TColor;
    function GetFontStyles: TFontStyles;
    function GetForceFontStyles: Boolean;
    function GetForeground: TColor;
  end;

  // The marker for break point lines.
  TBreakPointMarker = class(TInterfacedObject, IUCELineMarker)
  private
    FImage: TPngObject;
  protected
    procedure Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);
    function GetSize(Index: Integer): TSize;
  end;

  // The marker for lines with debug info.
  TDebugInfoMarker = class(TInterfacedObject, IUCELineMarker)
  private
    FImage: TPngObject;
  protected
    procedure Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);
    function GetSize(Index: Integer): TSize;
  end;

  // The marker for lines with debug info.
  TCurrentPositionMarker = class(TInterfacedObject, IUCELineMarker)
  private
    FImage: TPngObject;
  protected
    procedure Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);
    function GetSize(Index: Integer): TSize;
  end;

  // The marker for lines without debug info.
  TEmptyMarker = class(TInterfacedObject, IUCELineMarker)
  protected
    procedure Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);
    function GetSize(Index: Integer): TSize;
  end;

  TLineParseThread = class(TThread)
  private
    FEdit: TScriptEditor;
    FLine: TUCELine;
  protected
    procedure Execute; override;
  public
    constructor Create(Edit: TScriptEditor; Line: TUCELine);
  end;
  
//----------------- TScriptBreakPoint ----------------------------------------------------------------------------------

constructor TScriptBreakPoint.Create(Line: Integer);

begin
  Self.Line := Line;
end;

//----------------- TScriptEditor --------------------------------------------------------------------------------------

constructor TScriptEditor.Create(AOwner: TComponent; OptionProvider: IOptionProvider);

var
  Temp: Integer;

begin
  inherited Create(AOwner);
  FOptionProvider := OptionProvider;

  OptionProvider.AddListener(Self);
  Font.Name := OptionProvider.OptionAsString['CodeFontName'];
  Font.Height := OptionProvider.OptionAsInteger['CodeFontHeight'];
  Temp := OptionProvider.OptionAsInteger['CodeFontWidth'];
  if Temp > 0 then
    CharWidth := Temp;
  Content.OnValidateLine := ValidateLine;
  Content.OnDeleteLine := DeleteLine;
  Content.OnChangeLine := ChangeLine;
  
  FBreakpoints := TObjectList.Create;
  FFileName := '';

  FBreakPointLineStyle := TBreakPointLineStyle.Create;
  FCurrentPositionLineStyle := TCurrentPositionLineStyle.Create;
  FBreakPointMarker := TBreakPointMarker.Create;
  FDebugInfoMarker := TDebugInfoMarker.Create;
  FCurrentPositionMarker := TCurrentPositionMarker.Create;
  FEmptyMarker := TEmptyMarker.Create;
  TabSize := 2;

  MaxUndo := 32000;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TScriptEditor.Destroy;

begin
  FOptionProvider.RemoveListener(Self);
  FBreakpoints.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.AddExecutionMarker(Line: Integer);

begin
  if (Line > - 1) and (Line < Content.Count) then
  begin
    InvalidateLine(Line);
    Content[Line].PushStyle(FCurrentPositionLineStyle);
    Content[Line].AddMarker(FCurrentPositionMarker);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.AnalyzeLine(CommandEndPending: Boolean; Line: TUCELine);

// Scans the given line for certain information. It tries to locate a command and stores the current state in the line
// data. Additionally, text is examined to learn when the delimiter is changed.

var
  Data: TLineData;
  Lexer: TSQLTokenizer;
  Stream: TWideStringStream;
  Token: WideChar;
  I: Integer;
  StartState: TTokenizerState;
  Start: Integer;
  Stop: Integer;
  LastStop: Integer;
  NewSchema: WideString;

begin
  Stream := TWideStringStream.Create(Line.Text);
  if Line.Index = 0 then
    StartState := tsNormal
  else
  begin
    Data := Content[Line.Index - 1].Data as TLineData;
    StartState := Data.FEndState;
    Data.FDelimiter := Data.FDelimiter;
  end;
  Data := Line.Data as TLineData;
  Lexer := TSQLTokenizer.Create(Stream, StartState);
  Start := -1;
  NewSchema := '';
  try
    repeat
      Token := Lexer.NextToken;
      case Token of
        toEOF:
          Break;
        toWhiteSpace,
        toSLComment,
        toMLComment:
          Continue;
      else
        if Token = Data.FDelimiter[1] then
        begin
          // Potential delimiter found. Scan further to know for sure.
          Stop := Lexer.TokenPosition - 1;
          I := 2;
          while I <= Length(Data.FDelimiter) do
          begin
            Token := Lexer.NextToken;
            if Token = Data.FDelimiter[I] then
              Inc(I)
            else
              Break;
          end;

          if (I - 1) =  Length(Data.FDelimiter) then
          begin
            // A statement ends here. Add it to the list, but without the delimiter.
            // If Start is -1 then there is statement, that might contain special comments.
            if (Start = -1) and not CommandEndPending then
            begin
              LastStop := Data.GetLastStatementEnd;
              if LastStop > -1 then
                Start := LastStop + 1
              else
                Start := 0;
            end;
            Data.AddStatement(Start, Stop, NewSchema);
            Start := -1;
            CommandEndPending := False;
          end;
        end
        else
        begin
          // Any other input is simply noticed as part of a command.
          // if command end is True then we have found already a command on this line in a previous run.
          // In this case we have now a new command start and mark this accordingly while the command end becomes open again.
          if (Start = -1) and not CommandEndPending then
          begin
            // If this is a new command text then check if it is the "delimiter" command.
            // In this case we have to update our delimiter variable.
            if Lexer.TokenSymbolIs('delimiter') then
            begin
              // Skip white space and comments.
              repeat
                Token := Lexer.NextToken;
              until not (Token in [toWhiteSpace, toSLComment, toMLComment]);

              if Token <> toEOF then
              begin
                Data.FDelimiter := Token;
                repeat
                  Token := Lexer.NextToken;
                  if Token in [toEOF, toWhiteSpace, toSLComment, toMLComment] then
                    Break;
                  Data.FDelimiter := Data.FDelimiter + Token;
                until False;

                // The delimiter statement ends here. Don't add it to the list.
              end
              else
                Break;
            end
            else
              if Lexer.TokenSymbolIs('use') then
              begin
                // Database is about to change. Extract the name and store it in the data.
                // This way we don't need to check every line later for a use command.

                // Skip white spaces and comments.
                repeat
                  Token := Lexer.NextToken;
                until not (Token in [toWhiteSpace, toSLComment, toMLComment]);
                if Token <> toEOF then
                begin
                  NewSchema := Lexer.TokenString;
                  if Token = toString then
                    NewSchema := Unquote(NewSchema);

                  // Continue main loop until the terminator appears.
                end;
              end
              else
                Start := Lexer.TokenPosition;
          end;
        end;
      end;
    until False;

    // If there still a command pending then add it to the line too.
    if Start > -1 then
    begin
      Data.AddStatement(Start, -1);
      CommandEndPending := True;
    end;

    Data.CommandPending := CommandEndPending;
    Data.FEndState := Lexer.State;

    Data.FAnalyzed := True;
    if Data.GetFirstStatementStart > -1 then
    begin
      Line.AddMarker(FDebugInfoMarker);
      Line.AddMarker(FEmptyMarker);
    end
    else
    begin
      Line.RemoveMarker(FDebugInfoMarker);
      Line.RemoveMarker(FEmptyMarker);
    end;
  finally
    Lexer.Free;
    Stream.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.DeleteLine(Sender: TObject; Line: TUCELine);

begin
  Line.Data.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.GutterMouseDown(Sender: TCustomUnicodeEdit; Button: TMouseButton; Shift: TShiftState; X, Y, Line: Integer);

begin
  ToggleBreakpoint(Line);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.RemoveExecutionMarker(Line: Integer);

begin
  if (Line > - 1) and (Line < Content.Count) then
  begin
    Content[Line].RemoveStyle(FCurrentPositionLineStyle);
    Content[Line].RemoveMarker(FCurrentPositionMarker);
    InvalidateLine(Line);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ToggleBreakpoint(Line: Integer);

// Switches the break for that line or the line that has the command start for this line on or off.

var
  Data: TLineData;

begin
  repeat
    Data := Content[Line].Data as TLineData;
    if Data.GetFirstStatementStart > -1 then
      Break;
    Dec(Line);
  until Line < 0;

  if Line >= 0 then
  begin
    Data.FHasBreakPoint := not Data.FHasBreakPoint;

    if not Data.FHasBreakPoint then
    begin
      // Empty marker for alignment.
      Content[Line].ReplaceMarker(FBreakPointMarker, FEmptyMarker);
      Content[Line].RemoveStyle(FBreakpointLineStyle);
      InvalidateLine(Line);
    end
    else
    begin
      Content[Line].ReplaceMarker(FEmptyMarker, FBreakPointMarker);
      Content[Line].PushStyle(FBreakpointLineStyle);
      InvalidateLine(Line);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ClearBreakpoints;

// Clear all breakpoints

var
  Data: TLineData;
  Line: Integer;

begin
  Line := Content.Count-1;

  repeat
    Data := Content[Line].Data as TLineData;
    if Data.GetFirstStatementStart > -1 then
    begin
      Data.FHasBreakPoint := False;

      Content[Line].ReplaceMarker(FBreakPointMarker, FEmptyMarker);
      Content[Line].RemoveStyle(FBreakpointLineStyle);
      InvalidateLine(Line);
    end;
    Dec(Line);
  until Line < 0;

end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptEditor.Unquote(const S: WideString): WideString;

begin
  SetString(Result, PWideChar(S) + 1, Length(S) - 2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ValidateLine(Sender: TObject; Line: TUCELine);

var
  LineData: TLineData;
  PrevLineData: TLineData;

begin
  LineData := TLineData.Create;
  Line.Data.Free;
  Line.Data := LineData;
  if Line.Index = 0 then
  begin
    // This is the first line in the editor. Validate this here so we always have a safe anchor where
    // iterations can stop later.
    AnalyzeLine(False, Line);
  end
  else
  begin
    // Check if the previous line has been analyzed already. In this case we can quickly analyze this line too.
    PrevLineData := Content[Line.Index - 1].Data as TLineData;
    if Assigned(PrevLineData) and PrevLineData.FAnalyzed then
      AnalyzeLine(PrevLineData.CommandPending, Line);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ChangeLine(Sender: TObject; Line: TUCELine);

var
  Data: TLineData;
  CommandWasPending: Boolean;
  LastState: TTokenizerState;
  LineIndex: Integer;

begin
  // Line changed, so we have to reanalyze it.
  Data := Line.Data as TLineData;
  if Assigned(Data) then
  begin
    CommandWasPending := Data.CommandPending;
    LastState := Data.EndState;
    ValidateLine(Sender, Line);
    LineIndex := Line.Index;

    Data := Line.Data as TLineData;
    if (Data.CommandPending <> CommandWasPending) or (Data.EndState <> LastState) then
    begin
      // When either a command is now finished that was not before (or vice versa) or
      // a string/comment was closed that was not before (or vice versa) then revalidate all following lines.
      Inc(LineIndex);
      while LineIndex < Content.Count do
      begin
        Data := Content[LineIndex].Data as TLineData;
        // Stop invalidating if this line has never been validated.
        if Data = nil then
          Break;
        Data.FAnalyzed := False;
        Content.RevalidateLine(Line);
        InvalidateLine(LineIndex);
        Inc(LineIndex);                      
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.KeyDown(var Key: Word; Shift: TShiftState);

begin
  if (Key=Ord('F')) and (Shift=[ssCtrl]) then
    DoDisplaySearch
  else
    if (Key=Ord('R')) and (Shift=[ssCtrl]) then
      DoDisplaySearch(True)
    else
      inherited KeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TScriptEditor.DoDisplaySearch(ShowReplacePage: Boolean);

var
  Intf: IAdaptable;
  Component: TComponent;
  Provider: IOptionProvider;

begin
  if TextSearchForm=nil then
  begin
    Component := Self;
    while Component.Owner <> nil do
    begin
      if (Component is TCustomForm) and Supports(Component, IAdaptable, Intf) then
      begin
        Provider := Intf.GetAdapter(IOptionProvider) as IOptionProvider;

        TextSearchForm := TTextSearchForm.Create(nil, Provider);
        break;
      end;

      Component := Component.Owner;
    end;
  end;

  if ShowReplacePage then
    TextSearchForm.PageControl.ActivePage:=
      TextSearchForm.ReplaceTabSheet;


  TextSearchForm.Show;
  TextSearchForm.OnSearch := DoSearch;
end;

//------------------------------------------------------------------------------

function TScriptEditor.DoSearch(Sender: TObject;
  SearchText: WideString; ReplaceText: WideString;
  SearchOptions: TTextSearchOptions): Integer;

var
  SOptions: TSearchOptions;
  //ParentForm: TCustomForm;

begin
  //Translate Options
  SOptions := [];
  if tsoBackwards in SearchOptions then
    include(SOptions, soBackwards);
  if tsoEntireScope in SearchOptions then
    include(SOptions, soEntireScope);
  if tsoIgnoreNonSpacing in SearchOptions then
    include(SOptions, soIgnoreNonSpacing);
  if tsoMatchCase in SearchOptions then
    include(SOptions, soMatchCase);
  if tsoPrompt in SearchOptions then
    include(SOptions, soPrompt);
  if tsoRegularExpression in SearchOptions then
    include(SOptions, soRegularExpression);
  if tsoReplace in SearchOptions then
    include(SOptions, soReplace);
  if tsoReplaceAll in SearchOptions then
    include(SOptions, soReplaceAll);
  if tsoSelectedOnly in SearchOptions then
    include(SOptions, soSelectedOnly);
  if tsoSpaceCompress in SearchOptions then
    include(SOptions, soSpaceCompress);
  if tsoWholeWord in SearchOptions then
    include(SOptions, soWholeWord);

  Result := SearchReplace(SearchText, ReplaceText, SOptions);


  {ParentForm := GetParentForm(self);
  if ParentForm<>nil then
    ParentForm.BringToFront;}
end;

//------------------------------------------------------------------------------

procedure TScriptEditor.DoEnter;

begin
  if TextSearchForm<>nil then
  begin
    TextSearchForm.Show;
    TextSearchForm.OnSearch := DoSearch;
  end;

  inherited DoEnter;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.OptionChanged;

begin
  Font.Name := FOptionProvider.OptionAsString['CodeFontName'];
  Font.Height := FOptionProvider.OptionAsInteger['CodeFontHeight'];
  CharWidth := FOptionProvider.OptionAsInteger['CodeFontWidth'];
end;

//----------------- TLineData ------------------------------------------------------------------------------------------

constructor TLineData.Create;

begin
  FDelimiter := DefaultDelimiter;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLineData.AddStatement(X1, X2: Integer; Schema: WideString = '');

var
  Count: Integer;

begin
  Count := Length(FStatements);
  SetLength(FStatements, Count + 1);
  FStatements[Count].Start := X1;
  FStatements[Count].Stop := X2;
  FStatements[Count].FNewSchema := Schema;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetFirstStatement: PStatement;

begin
  Result := nil;
  FNextStatementCounter := 0;
  if Length(FStatements) > 0 then
  begin
    Result := @FStatements[FNextStatementCounter];
    Inc(FNextStatementCounter);
    if Result.Start = -1 then
    begin
      if FNextStatementCounter < Length(FStatements) then
      begin
        Result := @FStatements[FNextStatementCounter];
        Inc(FNextStatementCounter);
      end
      else
        Result := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetFirstStatementEnd: Integer;

// Returns the position of the end of the first command on this line. The command does not necessarily start on this line.

begin
  Result := -2;
  if Length(FStatements) > 0 then
    Result := FStatements[0].Stop
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetLastStatementEnd: Integer;

// Returns the end position of the last command that ends on this line. The command does not necessarily start on this line
// but cannot have any other complete command after it. An incomplete command could start there, though.

var
  Count: Integer;

begin
  Result := -2;
  Count := High(FStatements);
  if Count > -1 then
  begin
    Result := FStatements[Count].Stop;
    if (Result = -1) and (Count > 0) then
      Result := FStatements[Count - 1].Stop;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetFirstStatementStart: Integer;

// Returns the position of the first command that starts on this line. The command does not necessarily start on this line
// and may also have parts of a previous command that started on another line before it.

begin
  Result := -2;
  if Length(FStatements) = 1 then
    Result := FStatements[0].Start
  else
    if Length(FStatements) > 1 then
      Result := FStatements[1].Start;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetNextStatement: PStatement;

// Returns the next statement in the list of statements. In order to work correctly you *must* call
// GetFirstStatement once before you can iterate further with GetNextStatement.

begin
  Result := nil;
  if FNextStatementCounter < Length(FStatements) then
  begin
    Result := @FStatements[FNextStatementCounter];
    Inc(FNextStatementCounter);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.HasStatementStart: Boolean;

// Determines if this line contains the start of a command.

begin
  // If there is more than one statement then there is for sure one that starts on this line.
  Result := Length(FStatements) > 1;
  if not Result and (Length(FStatements) > 0) then
  begin
    // Only one statement.
    Result := FStatements[0].Start > -1;
  end;
end;

//----------------- TBreakPointLineStyle -------------------------------------------------------------------------------

function TBreakPointLineStyle.GetBackground: TColor;

begin
  Result := $0052FF;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBreakPointLineStyle.GetFontStyles: TFontStyles;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TBreakPointLineStyle.GetForceFontStyles: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBreakPointLineStyle.GetForeground: TColor;

begin
  Result := clWhite;
end;

//----------------- TCurrentpositionLineStyle --------------------------------------------------------------------------

function TCurrentPositionLineStyle.GetBackground: TColor;

begin
  Result := $C69A2F;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentPositionLineStyle.GetFontStyles: TFontStyles;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentPositionLineStyle.GetForceFontStyles: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentPositionLineStyle.GetForeground: TColor;

begin
  Result := clWhite;
end;

//----------------- TBreakPointMarker ----------------------------------------------------------------------------------

procedure TBreakPointMarker.Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_breakpoint');

  FImage.Draw(Canvas, Rect(X, Y, X + FImage.Width, Y + FImage.Height));
end;

//----------------------------------------------------------------------------------------------------------------------

function TBreakPointMarker.GetSize(Index: Integer): TSize;

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_breakpoint');

  Result.cx := FImage.Width;
  Result.cy := FImage.Height;
end;

//----------------- TDebugInfoMarker -----------------------------------------------------------------------------------

procedure TDebugInfoMarker.Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_query_start');

  FImage.Draw(Canvas, Rect(X, Y, X + FImage.Width, Y + FImage.Height));
end;

//----------------------------------------------------------------------------------------------------------------------

function TDebugInfoMarker.GetSize(Index: Integer): TSize;

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_query_start');

  Result.cx := FImage.Width;
  Result.cy := FImage.Height;
end;

//----------------- TCurrentPositionMarker -----------------------------------------------------------------------------

procedure TCurrentPositionMarker.Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_current_pos');

  FImage.Draw(Canvas, Rect(X, Y, X + FImage.Width, Y + FImage.Height));
end;

//----------------------------------------------------------------------------------------------------------------------

function TCurrentPositionMarker.GetSize(Index: Integer): TSize;

begin
  if FImage = nil then
    FImage := LoadPNGImageFromResource('gutter_current_pos');

  Result.cx := FImage.Width;
  Result.cy := FImage.Height;
end;

//----------------- TEmptyMarker ---------------------------------------------------------------------------------------

procedure TEmptyMarker.Draw(Index: Integer; Canvas: TCanvas; X, Y: Integer);

begin
  // Nothing to draw.
end;

//----------------------------------------------------------------------------------------------------------------------

function TEmptyMarker.GetSize(Index: Integer): TSize;

begin
  Result.cx := 12;
  Result.cy := 16;
end;

//----------------- TLineParseThread -----------------------------------------------------------------------------------

constructor TLineParseThread.Create(Edit: TScriptEditor; Line: TUCELine);

begin
  inherited Create(True);

  FEdit := Edit;
  FLine := Line;
  FreeOnTerminate := True;
  Resume;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TLineParseThread.Execute;

var
  Data: TLineData;
  Index: Integer;

begin
  Data := FLine.Data as TLineData;

  // Check again if the data is not already parsed.
  if not Data.FAnalyzed then
  begin
    // If not already analyzed then scan back and find the last parsed line.
    Index := FLine.Index;
    while Index >= 0 do
    begin
      Data := FEdit.Content[Index].Data as TLineData;
      if Assigned(Data) and Data.FAnalyzed then
        Break;
      Dec(Index);
    end;

    // The previous loop safely stops at line 0 if all fails.
    // Now go forward and validate all lines from between the last parsed and the one needed.
    Inc(Index);
    while Index < FLine.Index do
    begin
      Data := FEdit.Content[Index].Data as TLineData;
      FEdit.AnalyzeLine(Data.CommandPending, FEdit.Content[Index]);
      Inc(Index);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

