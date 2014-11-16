unit ScriptPanel;

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
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, SyncObjs,
  VirtualTrees, TntExtCtrls, TntComCtrls, TntClasses, TntStdCtrls,
  gnugettext, auxfuncs, PNGImage,
  MySQLConnection, MySQLResultSet, AuxLists, TntMenus, Forms,
  UCESQLHighlighter, UCEHighlighter, UCEEditorKeyCommands,
  UCEHTMLHighlighter, UCEShared, UniCodeEditor, LexicalTools,
  TextSearch, Options, CommonTypes, SQLErrorGrid;

type
  TExecutionOptions = set of (
    exoSelectionOnly,
    exoStopOnBreakpoints,
    exoStopOnErrors,
    exoStartOver
  );
  
  TScriptPanel = class;
  TScriptEditor = class;
  TScriptErrorGridOld = class;

  TScriptTabSheet = class(TTntPanel)
  private
    FActiveScriptPanel: TScriptPanel;

    FOnBeforeActiveScriptPanelChanged,
    FOnActivateScriptPanel: TNotifyEvent;
    FTabIndex: Integer;
    FScriptPanels: TList;
    function GetPanelCount: Integer;
    function GetScriptPanel(Index: Integer): TScriptPanel;
    procedure SetActiveScriptPanel(ActiveScriptPanel: TScriptPanel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddScriptPanel(MySQLConn: TMySQLConn; OptionProvider: IOptionProvider): TScriptPanel;
    procedure RemoveScriptPanel(Panel: TScriptPanel);

    property ActiveScriptPanel: TScriptPanel read FActiveScriptPanel write SetActiveScriptPanel;
    property OnBeforeActiveScriptPanelChanged: TNotifyEvent read FOnBeforeActiveScriptPanelChanged write
      FOnBeforeActiveScriptPanelChanged;
    property OnActivateScriptPanel: TNotifyEvent read FOnActivateScriptPanel write FOnActivateScriptPanel;
    property ScriptPanel[Index: Integer]: TScriptPanel read GetScriptPanel; default;
    property ScriptPanelCount: Integer read GetPanelCount;
    property TabIndex: Integer read FTabIndex write FTabIndex;
  end;

  TScriptPanel = class(TTntPanel)
  private
    FActive: Boolean;
    FCurrentExecutionLine: Integer;
    FLastExecutionLine: Integer;
    FMySQLConn: TMySQLConn;
    FHighlighter: TUCESQLHighlighter;

    FScriptEditor: TScriptEditor;
    FScriptErrorGrid: TSQLErrorGrid;
    FErrorMessages: TObjectList;
    FProgressMonitor: IProgressMonitor;

    FExecuting,
    FStopping: Boolean;
    FExecutionOptions: TExecutionOptions;
    FExecutionEnd: Integer;
    FSelStartCharIndex: Integer;          // The index of the selection start character.
    FSelEndCharIndex: Integer;            // dito for selection end.

    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
  protected
    procedure DoSelectLine(Sender: TSQLErrorGrid; Line, Position: Integer);
    function GetRunning: Boolean;
    function DetermineNextStatement: Integer;
    procedure DoChildControlEnter(Sender: TObject);
    function ExecuteCurrentStatement: Boolean;
    function HasNextStatement: Boolean;
    procedure InitProgress;
    procedure ProcessUserCommand(Sender: TCustomUnicodeEdit; var Command: TEditorCommand; var AChar: WideChar;
      Data: Pointer);
    procedure ResetExecution;
    function StatementHasBreakPoint: Boolean;
    procedure UpdateProgress;
    procedure UpdateExecutionLineDisplay(NewLine: Integer);
  public
    constructor Create(AOwner: TComponent; OptionProvider: IOptionProvider; Connection: TMySQLConn); reintroduce;
    destructor Destroy; override;

    procedure ExecuteScript(Options: TExecutionOptions);
    procedure StepInto;
    procedure StepOver;
    procedure Stop;
    procedure Pause;

    property Active: Boolean read GetActive write SetActive;
    property MySQLConn: TMySQLConn read FMySQLConn;
    property ProgressMonitor: IProgressMonitor read FProgressMonitor write FProgressMonitor;
    property Running: Boolean read GetRunning;
    property ScriptEditor: TScriptEditor read FScriptEditor;
  end;

  TScriptBreakPoint = class(TObject)
    Line: Integer;
    constructor Create(Line: Integer);
  end;

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
    FHasBreakpoint: Boolean;           // True if there is currently a break point set for this line, otherwise False.
    FCommandPending: Boolean;          // If True then there is an unfinished command on this or a previous line.
    FDelimiter: WideString;            // The statement delimiter, which is active for this line and all following.
    FNextStatementCounter: Integer;
    function GetStatementCount: Integer;
    function GetFirstStatementRaw: PStatement;
    function GetLastStatement: PStatement;
  protected
    procedure AddStatement(X1, X2: Integer; Schema: WideString = '');
    function GetFirstStatement: PStatement;
    function GetFirstStatementEnd: Integer;
    function GetFirstStatementStart: Integer;
    function GetLastStatementEnd: Integer;
    function GetNextStatement: PStatement;
    function HasStatementStart: Boolean;

    property Delimiter: WideString read FDelimiter write FDelimiter;
    property EndState: TTokenizerState read FEndState write FEndState;
  public
    constructor Create; virtual;

    property Analyzed: Boolean read FAnalyzed;
    property CommandPending: Boolean read FCommandPending write FCommandPending;
    property FirstStatementEnd: Integer read GetFirstStatementEnd;      // The end of the very first statement.
    property FirstStatementRaw: PStatement read GetFirstStatementRaw;   // The very first statement, might not start on this line.
    property FirstStatementStart: Integer read GetFirstStatementStart;  // The start point of the first statement that starts on this line.
                                                                        // Might be after the end of the first statement (which then starts on a
                                                                        // previous line.
    property LastStatement: PStatement read GetLastStatement;           // The last statement that begins on this line.
    property LastStatementEnd: Integer read GetLastStatementEnd;        // The position of the end of the last statement that ends on this line.
    property StatementCount: Integer read GetStatementCount;
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
    FTokenizer: TSQLTokenizer;
    FOriginalFormat: TTextFormat;
  protected
    procedure AddExecutionMarker(Line: Integer);
    procedure ChangeLine(Sender: TObject; Line: TUCELine); virtual;
    procedure DeleteLine(Sender: TObject; Line: TUCELine); virtual;
    procedure GutterMouseDown(Sender: TCustomUnicodeEdit; Button: TMouseButton; Shift: TShiftState; X, Y, Line: Integer);
    procedure RemoveExecutionMarker(Line: Integer);
    function Unquote(const S: WideString): WideString;
    procedure ValidateLine(Sender: TObject; Line: TUCELine);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
      SearchOptions: TTextSearchOptions): Integer;
    procedure OptionChanged;
  public
    constructor Create(AOwner: TComponent; OptionProvider: IOptionProvider); reintroduce;
    destructor Destroy; override;

    procedure AnalyzeLine(CommandEndPending: Boolean; Line: TUCELine; HasBreakpoint: Boolean);
    procedure DoDisplaySearch(ShowReplacePage: Boolean = False);

    procedure ToggleBreakpoint(Line: Integer);
    procedure ClearBreakpoints;

    property FileName: WideString read FFileName write FFileName;
    property OriginalTextFormat: TTextFormat read FOriginalFormat write FOriginalFormat;
  end;

  TScriptErrorGridOld = class(TVirtualStringTree, IOptionChangeListener)
    constructor Create(AOwner: TComponent; OptionProvider: IOptionProvider); reintroduce;
    destructor Destroy; override;

    procedure DoMessagesChanged;

    procedure ClearMessages(Sender: TObject);
  private
    FScriptPanel: TScriptPanel;
    RSErrorPNGImg,
    RSWarningPNGImg: TPNGObject;
    ErrorPopupMenu: TTntPopupMenu;
    FOptionProvider: IOptionProvider;
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DblClick; override;
    procedure OptionChanged;
  end;

  PScriptMsg = ^TScriptMsg;
  TScriptMsg = record
    MsgIndex: Integer;
  end;

  TScriptErrorOld = class(TObject)
  public
    Msg: WideString;
    MsgType: TScriptMessageType;
    MsgNr: Integer;
    Line: Integer;
    CharPos: Integer;
    constructor Create(Msg: WideString; MsgType: TScriptMessageType; MsgNr: Integer; Line: Integer; CharPos: Integer);
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Math, PNGTools,
  myx_public_interface,
  StringContainers;

const
  DefaultDelimiter = ';';
  
type
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

//----------------------------------------------------------------------------------------------------------------------

constructor TScriptTabSheet.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  Width := 650;
  Height := 500;

  FScriptPanels := TObjectList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TScriptTabSheet.Destroy;

begin
  FScriptPanels.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptTabSheet.GetPanelCount: Integer;

begin
  Result := FScriptPanels.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptTabSheet.GetScriptPanel(Index: Integer): TScriptPanel;

begin
  Result := TScriptPanel(FScriptPanels[Index]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptTabSheet.AddScriptPanel(MySQLConn: TMySQLConn; OptionProvider: IOptionProvider): TScriptPanel;

var
  ScriptPanel, MasterScriptPanel: TScriptPanel;
  Splitter: TTntSplitter;
  ParentForm: TCustomForm;

begin
  ScriptPanel := TScriptPanel.Create(Self, OptionProvider, MySQLConn);
  ScriptPanel.Parent := self;

  FScriptPanels.Add(ScriptPanel);

  if (FScriptPanels.Count > 1) then
  begin
    MasterScriptPanel := TScriptPanel(FScriptPanels[FScriptPanels.Count - 2]);

    ScriptPanel.Height := MasterScriptPanel.Height div 2;
    MasterScriptPanel.Height := MasterScriptPanel.Height div 2;
    ScriptPanel.Align := alBottom;

    //Copy events & properties
    ScriptPanel.FScriptEditor.PopupMenu := MasterScriptPanel.FScriptEditor.PopupMenu;

    Splitter := TTntSplitter.Create(self);
    Splitter.Parent := self;
    Splitter.Align := alBottom;
    Splitter.Height := 9;
    Splitter.Top := MasterScriptPanel.Top + MasterScriptPanel.Height + 10;
    Splitter.MinSize := 40;
    Splitter.AutoSnap := False;

    ScriptPanel.Top := MasterScriptPanel.Top + MasterScriptPanel.Height + 10;
  end
  else
    ScriptPanel.Align := alClient;

  ActiveScriptPanel := ScriptPanel;

  //Set Focus to new RSPanel
  ParentForm := GetParentForm(ScriptPanel);
  if (ParentForm <> nil) then
    if (ParentForm.Visible and ParentForm.Enabled) then
      ScriptPanel.SetFocus;
  ScriptPanel.Active := True;

  Result := ScriptPanel;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptTabSheet.RemoveScriptPanel(Panel: TScriptPanel);

// Removes the given panel if it is on this sheet and destroys it.

var
  Index: Integer;
  
begin
  // The Remove call will also destroy the panel.
  Index := FScriptPanels.Remove(Panel);
  if Index > -1 then
  begin
    if FActiveScriptPanel = Panel then
      FActiveScriptPanel := nil;
    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptTabSheet.SetActiveScriptPanel(ActiveScriptPanel: TScriptPanel);

var
  i: Integer;

begin
  if (FActiveScriptPanel <> ActiveScriptPanel) then
  begin
    if (Assigned(FOnBeforeActiveScriptPanelChanged)) then
      FOnBeforeActiveScriptPanelChanged(FActiveScriptPanel);

    FActiveScriptPanel := ActiveScriptPanel;

    for i := 0 to FScriptPanels.Count - 1 do
      if (FScriptPanels[i] <> ActiveScriptPanel) then
        TScriptPanel(FScriptPanels[i]).Active := False;

    if (Assigned(FOnActivateScriptPanel)) then
      FOnActivateScriptPanel(FActiveScriptPanel);
  end;
end;

//----------------- TScriptPanel ---------------------------------------------------------------------------------------

constructor TScriptPanel.Create(AOwner: TComponent; OptionProvider: IOptionProvider; Connection: TMySQLConn);

var
  Stroke: TKeyStroke;
  
begin
  inherited Create(AOwner);

  BevelOuter := bvNone;
  Caption := '';

  FMySQLConn := Connection;
  FActive := True;
  FErrorMessages := TObjectList.Create;

  // Create and setup SQL syntax highlighter.
  FHighlighter := TUCESQLHighlighter.Create(Self);
  with FHighlighter do
  begin
    CommentAttributes.Foreground := clGray;
    CommentAttributes.Style := [fsItalic];
    KeyAttributes.Foreground := clBlue;
    StringAttributes.Foreground := $0080FF;
    SpaceAttributes.Foreground := $808080;

    IdentifierAttributes.Foreground := clWindowText;
    QuotedIDAttributes.Foreground := $804000;
    QuotedIDAttributes.Style := [fsBold];

    NumberAttributes.Foreground := clFuchsia;
    SymbolAttributes.Foreground := clWindowText;

    EmbeddedCommandAttributes.Foreground := clNavy;
    EmbeddedCommandAttributes.Background := $F0F0F0;

    SystemVariableAttributes.Foreground := $808000;
    SystemVariableAttributes.Style := [fsBold];
    UserVariableAttributes.Foreground := $C08080;
    UserVariableAttributes.Style := [fsBold];
  end;
  
  // Create and setup the actual SQL script editor.
  FScriptEditor := TScriptEditor.Create(Self, OptionProvider);
  FScriptEditor.Parent := Self;
  with FScriptEditor do
  begin
    // The editor control itself takes care for user controllable properties.
    Align := alClient;
    Highlighter := FHighlighter;
    OnEnter := DoChildControlEnter;
    GutterWidth := 60;
    RightMargin := -1;
    ScrollHintColor.Foreground := clWhite;
    ScrollHintColor.Background := clAppWorkSpace;
    SelectedColor.Foreground := clHighlightText;
    SelectedColor.Background := clHighlight;
    LineNumberFont.Name := 'Terminal';
    LineNumberFont.Size := 6;
    OnProcessUserCommand := ProcessUserCommand;
    OnGutterMouseDown := GutterMouseDown;

    // Add own keystrokes.
    // F5, to toggle break points.
    Stroke := Keystrokes.Add;
    Stroke.Command := ecUserFirst;
    Stroke.Key := VK_F5;
    // Shift+Bk like Bk alone
    Stroke := Keystrokes.Add;
    Stroke.Command := ecDeleteLastChar;
    Stroke.Key := VK_BACK;
    Stroke.Shift := [ssShift];
  end;

  FLastExecutionLine := -1;

  // Create error grid.
  FScriptErrorGrid := TSQLErrorGrid.Create(Self, OptionProvider);
  with FScriptErrorGrid do
  begin
    Parent := Self;
    OnEnter := DoChildControlEnter;
    OnSelectLine := DoSelectLine;
  end;

  ResetExecution;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TScriptPanel.Destroy;

begin
  ResetExecution;
  FScriptErrorGrid.Clear;
  FErrorMessages.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.GetActive: Boolean;

begin
  Result := FActive;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.SetActive(Value: Boolean);

begin
  if Value <> FActive then
  begin
    FActive := Value;

    if Value then
    begin
      FScriptEditor.Color := clWindow;
      if FCurrentExecutionLine > -1 then
        InitProgress;
    end
    else
    begin
      if Assigned(FScriptEditor) then
        FScriptEditor.Color := $00EEEEEE;
      if Assigned(FProgressMonitor) then
        FProgressMonitor.ProgressFinish;
    end;

    // Deactivate other FScriptPanels if this one got active.
    if Value and Parent.InheritsFrom(TScriptTabSheet) then
      TScriptTabSheet(Parent).ActiveScriptPanel := Self;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.DoSelectLine(Sender: TSQLErrorGrid; Line, Position: Integer);

begin
  FScriptEditor.CaretXY := Point(Position, Line);
  FScriptEditor.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.GetRunning: Boolean;

begin
  Result := (FCurrentExecutionLine <> -1);
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.DetermineNextStatement: Integer;

// Iterates through all lines and tries to find one on which a command starts.
// If FCurrentExecutionLine is > 0 then search is started from there.
// Returns the index of the line found or -1 if there is no more.
// Note: Multiple statements on one line are handle as one statement.

var
  I: Integer;
  Data: TLineData;

begin
  Result := -1;
  if FCurrentExecutionLine >= 0 then
    I := FCurrentExecutionLine + 1
  else
    I := 0;
  while I < FScriptEditor.Content.Count do
  begin
    Data := FScriptEditor.Content[I].Data as TLineData;
    if Data.GetFirstStatementStart > -1 then
    begin
      Result := I;
      Break;
    end;
    Inc(I);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.DoChildControlEnter(Sender: TObject);

begin
  Active := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.ExecuteScript(Options: TExecutionOptions);

var
  ReachedEnd: Boolean;
  Marker: Integer;
  P: TPoint;
  SkipFirstLineBreakPoint: Boolean;
  
begin
  try
    if exoStartOver in Options then
      ResetExecution;

    if FCurrentExecutionLine = -1 then
    begin
      SkipFirstLineBreakPoint := False;
      
      // Debugging has not yet started.
      InitProgress;
    end
    else
    begin
      // If we just continue and there is a break point on the current line then ignore that one.
      SkipFirstLineBreakPoint := True;

      // Remove the last execution line marker.
      Marker := FCurrentExecutionLine;
      UpdateExecutionLineDisplay(-1);
      FCurrentExecutionLine := Marker;
    end;

    if (exoSelectionOnly in Options) and FScriptEditor.SelectionAvailable then
    begin
      P := FScriptEditor.BlockBegin;
      if FCurrentExecutionLine < P.Y then
        FCurrentExecutionLine := P.Y;
      FExecutionEnd := FScriptEditor.BlockEnd.Y;

      // Convert selection column indices to char indices.
      FSelStartCharIndex := FScriptEditor.Content[P.Y].ColumnToCharIndex(P.X);
      FSelEndCharIndex := FScriptEditor.Content[FExecutionEnd].ColumnToCharIndex(FScriptEditor.BlockEnd.X)
    end;

    if not FMySQLConn.Connected then
    begin
      if ShowModalDialog(Application.Title + ' - ' + _('No server connection'),
        _('The connection to the server was lost.  Reconnect?'), myx_mtConfirmation, _('Yes') + #13#10 + _('No')) = 1 then
        FMySQLConn.Reconnect
      else
      begin
        ResetExecution;
        Exit;
      end;
    end;
    
    FExecutionOptions := Options;
    ReachedEnd := True;
    FExecuting := True;
    FStopping := False;
    while HasNextStatement do
    begin
      if (not FExecuting) or
        (FStopping) or
        (not SkipFirstLineBreakPoint and StatementHasBreakPoint and (exoStopOnBreakpoints in Options)) or
        (not ExecuteCurrentStatement) or
        (Application.Terminated) then
      begin
        UpdateExecutionLineDisplay(FCurrentExecutionLine);
        ReachedEnd := False;
        UpdateProgress;
        Break;
      end
      else
        FLastExecutionLine := FCurrentExecutionLine;

      SkipFirstLineBreakPoint := False;
      UpdateProgress;
      Application.ProcessMessages;
    end;

    FExecuting := False;

    if ReachedEnd or FStopping then
      ResetExecution;
  except
    ResetExecution;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.HasNextStatement: Boolean;

// Determines if there is a not yet executed statement in the script and returns True if so.

begin
  if FLastExecutionLine = FCurrentExecutionLine then
    FCurrentExecutionLine := DetermineNextStatement;
  Result := (FCurrentExecutionLine > -1) and (FCurrentExecutionLine <= FExecutionEnd);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.InitProgress;

begin
  FErrorMessages.Clear;
  FScriptErrorGrid.Clear;
  if Assigned(FProgressMonitor) then
  begin
    FProgressMonitor.ProgressInit(FScriptEditor.Content.Count);
    FProgressMonitor.ProgressPosition(0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.ProcessUserCommand(Sender: TCustomUnicodeEdit; var Command: TEditorCommand; var AChar: WideChar;
  Data: Pointer);

begin
  with FScriptEditor do
  begin
    case Command of
      ecUserFirst:
        ToggleBreakPoint(CaretY);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.ResetExecution;

// Sets the active statement to none. So on next execution the first statement is used.

begin
  if FMySQLConn.InTransaction then
    FMySQLConn.ExecuteDirect('ROLLBACK');

  UpdateExecutionLineDisplay(-1);
  FLastExecutionLine := -1;
  if Assigned(FProgressMonitor) then
    FProgressMonitor.ProgressFinish;
  FExecutionEnd := MaxInt;
  FExecuting := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.StatementHasBreakPoint: Boolean;

// Determines whether the next to execute statement has a break point set.

var
  Data: TLineData;

begin
  Result := FCurrentExecutionLine > -1;
  if Result then
  begin
    Data := FScriptEditor.Content[FCurrentExecutionLine].Data as TLineData;
    Result := Data.FHasBreakpoint;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.UpdateProgress;

begin
  if Assigned(FProgressMonitor) then
    FProgressMonitor.ProgressPosition(FCurrentExecutionLine);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.UpdateExecutionLineDisplay(NewLine: Integer);

begin
  FScriptEditor.RemoveExecutionMarker(FCurrentExecutionLine);
  FCurrentExecutionLine := NewLine;
  if FCurrentExecutionLine > -1 then
  begin
    FScriptEditor.AddExecutionMarker(FCurrentExecutionLine);
    FScriptEditor.CaretY := FCurrentExecutionLine;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.StepInto;

begin
  StepOver;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.StepOver;

begin
  // The first step is special.
  if (FCurrentExecutionLine = -1) and HasNextStatement then
  begin
    // Set marker to the first line that gets executed next time.
    UpdateExecutionLineDisplay(FCurrentExecutionLine);
    InitProgress;
  end
  else
    if HasNextStatement then
    begin
      // Only advance execution point if there was no error;
      if ExecuteCurrentStatement then
      begin
        FLastExecutionLine := FCurrentExecutionLine;
        UpdateExecutionLineDisplay(DetermineNextStatement);
        if FCurrentExecutionLine > -1 then
          UpdateProgress
        else
          ResetExecution;
      end;
    end
    else
      ResetExecution; // Finished debugging.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.Pause;

begin
  if (FExecuting) then
    FExecuting := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptPanel.Stop;

begin
  if (FExecuting) then
    FStopping := True
  else
    ResetExecution;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptPanel.ExecuteCurrentStatement: Boolean;

// Executes the statement that starts at the current statement position line and ends on the same or a following line.
// Returns False if either the statement could not be executed.

var
  StatementString: WideString;
  Start,
  Stop: TPoint;
  Data: TLineData;
  Statement: PStatement;
  Error: WideString;
  
begin
  Result := FCurrentExecutionLine > -1;
  if Result then
  begin
    Start.Y := FCurrentExecutionLine;
    Data := FScriptEditor.Content[Start.Y].Data as TLineData;
    Statement := Data.GetFirstStatement;

    // If we get an empty statement this might mean we don't have any statement at all OR
    // it could be the first statement on this line begins on an earlier line but
    // we are currently executing a selection. The latter could mean that the selection determines the
    // real command to execute so the command start does not matter. Check this.
    if (Statement = nil) and (exoSelectionOnly in FExecutionOptions) then
      Statement := Data.GetFirstStatementRaw;

    // Anything to do at all?
    if Assigned(Statement) then
    begin
      repeat
        // First check for special case "use db".
        if Length(Statement.FNewSchema) > 0 then
          FMySQLConn.DefaultSchema := Statement.FNewSchema
        else
        begin
          Start.X := Statement.Start;
          Stop.Y := FCurrentExecutionLine;
          Stop.X := Statement.Stop;
          if Stop.X = -1 then
          begin
            // Statement ends on a line somewhere later. Look for it.
            while (Stop.Y < FScriptEditor.Content.Count - 1) and (Stop.Y <= FExecutionEnd) do
            begin
              Inc(Stop.Y);
              Data := FScriptEditor.Content[Stop.Y].Data as TLineData;
              if not Data.FAnalyzed then
                FScriptEditor.AnalyzeLine(True, FScriptEditor.Content[Stop.Y], False);
              Stop.X := Data.GetFirstStatementEnd;
              if Stop.X > -1 then
                Break;
            end;
            Data := nil;
            // If no command end could be found then Stop.Y has the last line as command end line.
            // Use the entire line in this case for the statement.
            if (Stop.Y = FScriptEditor.Content.Count - 1) and (Stop.X < 0) then
              Stop.X := Length(FScriptEditor.Content[Stop.Y].Text);
          end;

          // The statement end points to the first character of the delimiter but the CollectText function
          // does not înclude the end point, so this compensates.
          // Special case to consider here, though, is a potential selection.
          if exoSelectionOnly in FExecutionOptions then
          begin
            if (Start.Y = FScriptEditor.BlockBegin.Y) and (FSelStartCharIndex > Start.X) then
              Start.X := FSelStartCharIndex;
            if (Stop.Y = FScriptEditor.BlockEnd.Y) and (FSelEndCharIndex < Stop.X) then
              Stop.X := FSelEndCharIndex;
          end;

          StatementString := FScriptEditor.Content.CollectTextFromPosition(Start, Stop);

          if not (FMySQLConn.ExecuteDirect(StatementString, 5000, False)) then
          begin
            Error := myx_mysql_error(FMySQLConn.MySQL);
            if Error = '' then
              Error := _('Connection problem. Could not execute command.');
            FScriptErrorGrid.AddError(Error, smtError, myx_mysql_errno(FMySQLConn.MySQL), Start.Y + 1, Start.X + 1);

            if exoStopOnErrors in FExecutionOptions then
              Result := False;
          end;
        end;

        if Data = nil then
          Statement := nil
        else
          Statement := Data.GetNextStatement;
      until (Statement = nil) or not Result;
    end;
  end;
end;

//----------------- TScriptBreakPoint ----------------------------------------------------------------------------------

constructor TScriptBreakPoint.Create(Line: Integer);

begin
  Self.Line := Line;
end;

//----------------- TScriptEditor --------------------------------------------------------------------------------------

constructor TScriptEditor.Create(AOwner: TComponent; OptionProvider: IOptionProvider);

begin
  inherited Create(AOwner);
  FOptionProvider := OptionProvider;

  OptionProvider.AddListener(Self);
  Options := [eoAutoIndent, eoAutoUnindent, eoGroupUndo, eoInserting, eoLineNumbers, eoShowScrollHint, eoTripleClicks,
    eoUndoAfterSave, eoUseSyntaxHighlighting, eoWantTabs, eoUseUndoRedo];

  OptionChanged;
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

  FTokenizer := TSQLTokenizer.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TScriptEditor.Destroy;

begin
  Content.Clear;
  FOptionProvider.RemoveListener(Self);
  FBreakpoints.Free;
  FTokenizer.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.AddExecutionMarker(Line: Integer);

begin
  if (Line > - 1) and (Line < Content.Count) then
  begin
    RefreshLine(Line);
    Content[Line].PushStyle(FCurrentPositionLineStyle);
    Content[Line].AddMarker(FCurrentPositionMarker);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ChangeLine(Sender: TObject; Line: TUCELine);

var
  Data: TLineData;
  CommandWasPending: Boolean;
  LastState: TTokenizerState;
  LineIndex: Integer;
  LastDelimiter: WideString;
  CurrentLine: TUCELine;

begin
  // Line changed, so we have to re-analyze it.
  if Assigned(Line) then
  begin
    Data := Line.Data as TLineData;
    if Assigned(Data) then
    begin
      CommandWasPending := Data.CommandPending;
      LastState := Data.EndState;
      LastDelimiter := Data.Delimiter;

      ValidateLine(Sender, Line);
      LineIndex := Line.Index;

      Data := Line.Data as TLineData;
      if (Data.CommandPending <> CommandWasPending) or (Data.EndState <> LastState) or (Data.Delimiter <> LastDelimiter) then
      begin
        // When either a command is now finished that was not before (or vice versa) or
        // a string/comment was closed that was not before (or vice versa) then revalidate all following lines.
        Inc(LineIndex);
        while LineIndex < Content.Count do
        begin
          CurrentLine := Content.LineNoInit[LineIndex];
          if not (lsValidated in CurrentLine.States) then
            Break;
          Data := CurrentLine.Data as TLineData;
          // Stop invalidating if this line has never been validated.
          if (Data = nil) or not Data.FAnalyzed then
            Break;
          Data.FAnalyzed := False;
          Content[LineIndex].Invalidate;
          RefreshLine(LineIndex);
          Inc(LineIndex);
        end;
      end;
    end;
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
    RefreshLine(Line);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ToggleBreakpoint(Line: Integer);

// Switches the break point for that line or the line that has the command start for this line on or off.
// This command is ignored if there is no such line as given by Line.

var
  UCELine: TUCELine;
  Data: TLineData;

begin
  if Line < Content.Count then
  begin
    Data := nil;

    repeat
      UCELine := Content[Line];

      if (UCELine <> nil) then
      begin
        Data := UCELine.Data as TLineData;
        if Data.GetFirstStatementStart > -1 then
          Break;
        Dec(Line);
      end
      else
        Line := -1;

    until Line < 0;

    if Line >= 0 then
    begin
      Data.FHasBreakpoint := not Data.FHasBreakpoint;

      if not Data.FHasBreakpoint then
      begin
        // Empty marker for alignment.
        Content[Line].ReplaceMarker(FBreakPointMarker, FEmptyMarker);
        Content[Line].RemoveStyle(FBreakpointLineStyle);
        RefreshLine(Line);
      end
      else
      begin
        Content[Line].ReplaceMarker(FEmptyMarker, FBreakPointMarker);
        Content[Line].PushStyle(FBreakpointLineStyle);
        RefreshLine(Line);
      end;
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
  if Content.Count > 0 then
  begin
    Line := Content.Count - 1;

    repeat
      Data := Content[Line].Data as TLineData;
      if Data.GetFirstStatementStart > -1 then
      begin
        Data.FHasBreakpoint := False;

        Content[Line].ReplaceMarker(FBreakPointMarker, FEmptyMarker);
        Content[Line].RemoveStyle(FBreakpointLineStyle);
        RefreshLine(Line);
      end;
      Dec(Line);
    until Line < 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptEditor.Unquote(const S: WideString): WideString;

begin
  if Length(S) >= 2 then
    SetString(Result, PWideChar(S) + 1, Length(S) - 2)
  else
    Result := S;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.ValidateLine(Sender: TObject; Line: TUCELine);

var
  OldLineData: TLineData;
  LineData: TLineData;
  PreviousLine: TUCELine;
  PrevLineData: TLineData;
  CommandWasPending: Boolean;
  HadBreakpoint: Boolean;
  LastState: TTokenizerState;
  LineIndex: Integer;
  LastDelimiter: WideString;
  CurrentLine: TUCELine;

begin
  // Line changed, so we have to re-analyze it.
  OldLineData := Line.Data as TLineData;

  if Assigned(OldLineData) then
  begin
    CommandWasPending := OldLineData.CommandPending;
    LastState := OldLineData.EndState;
    LastDelimiter := OldLineData.Delimiter;

    HadBreakpoint := OldLineData.FHasBreakpoint;
    Line.Data.Free;
    Line.Data := nil;
  end
  else
  begin
    CommandWasPending := False;
    LastState := tsNormal;
    LastDelimiter := DefaultDelimiter;
    HadBreakpoint := False;
  end;

  // Analyze this line now. If its predecessor is not yet analyzed then do this here too.
  if Line.Index = 0 then
  begin
    AnalyzeLine(False, Line, HadBreakpoint);
    LineIndex := 0;
  end
  else
  begin
    // Find first analyzed line before this one.
    PrevLineData := nil;
    LineIndex := Line.Index;
    repeat
      Dec(LineIndex);
      if LineIndex < 0 then
        Break;
        
      PreviousLine := Content.LineNoInit[LineIndex];
      PrevLineData := PreviousLine.Data as TLineData;
    until Assigned(PrevLineData) and PrevLineData.FAnalyzed;

    // Now analyze everything until the current one.
    if LineIndex = -1 then
    begin
      // Not even the first line was already analyzed, so do this explicitely here first.
      AnalyzeLine(False, Content[0], HadBreakpoint);
      LineIndex := 0;
    end;

    while LineIndex < Line.Index do
    begin
      PrevLineData := Content[LineIndex].Data as TLineData;
      Inc(LineIndex);
      AnalyzeLine(PrevLineData.CommandPending, Content.LineNoInit[LineIndex], False);
    end;

    // Finally analyze the actual line if it is not at index 0.
    if LineIndex > 0 then
      AnalyzeLine(PrevLineData.CommandPending, Content.LineNoInit[LineIndex], HadBreakpoint);
  end;

  // Keep last break point status.
  LineData := Line.Data as TLineData;

  if (LineData.CommandPending <> CommandWasPending) or (LineData.EndState <> LastState) or
    (LineData.Delimiter <> LastDelimiter) then
  begin
    // When either a command is now finished that was not before (or vice versa) or
    // a string/comment was closed that was not before (or vice versa) then revalidate all following lines.
    Inc(LineIndex);
    while LineIndex < Content.Count do
    begin
      CurrentLine := Content.LineNoInit[LineIndex];
      if not (lsValidated in CurrentLine.States) then
        Break;
      LineData := CurrentLine.Data as TLineData;
      // Stop invalidating if this line has never been validated.
      if (LineData = nil) or not LineData.FAnalyzed then
        Break;
      LineData.FAnalyzed := False;
      Content[LineIndex].Invalidate;
      RefreshLine(LineIndex);
      Inc(LineIndex);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.AnalyzeLine(CommandEndPending: Boolean; Line: TUCELine; HasBreakpoint: Boolean);

// Scans the given line for certain information. It tries to locate a command and stores the current state in the line
// data. Additionally, text is examined to learn when the delimiter is changed.

var
  Data: TLineData;
  PreviousData: TLineData;
  Stream: TWideStringStream;
  Token: WideChar;
  I: Integer;
  StartState: TTokenizerState;
  Start: Integer;
  Stop: Integer;
  LastStop: Integer;
  NewSchema: WideString;
  CurrentDelimiter: WideString;

begin
  Stream := TWideStringStream.Create(Line.Text);
  try
    if Line.Index = 0 then
    begin
      StartState := tsNormal;
      CurrentDelimiter := DefaultDelimiter;
    end
    else
    begin
      PreviousData := Content[Line.Index - 1].Data as TLineData;
      StartState := PreviousData.FEndState;
      CurrentDelimiter := PreviousData.FDelimiter;
    end;

    Data := TLineData.Create;
    Data.FHasBreakpoint := HasBreakpoint;
    
    Line.Data := Data;
    FTokenizer.Initialize(Stream, StartState, False);
    Start := -1;
    NewSchema := '';
    repeat
      Token := FTokenizer.NextToken;
      case Token of
        toEOF:
          Break;
        toWhiteSpace,            
        toMLComment,
        toSLComment:
          Continue;
      else
        if Token = CurrentDelimiter[1] then
        begin
          // Potential delimiter found. Scan further to know for sure.
          Stop := FTokenizer.TokenPosition;
          I := 2;
          while I <= Length(CurrentDelimiter) do
          begin
            Token := FTokenizer.NextToken;
            if Token = CurrentDelimiter[I] then
              Inc(I)
            else
              Break;
          end;

          if (I - 1) =  Length(CurrentDelimiter) then
          begin
            // A statement ends here. Add it to the list (end point stands on the delimiter).
            // If Start is -1 then there is a statement, that might contain special comments (or is entirely empty).
            if (Start = -1) and not CommandEndPending then
            begin
              LastStop := Data.GetLastStatementEnd;
              if LastStop > -1 then
                Start := LastStop + 1
              else
                Start := 0;
            end;

            // Stop points to the first delimiter character. We have to take this into account
            // when later collecting the command string.
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
            if FTokenizer.TokenSymbolIs('delimiter') then
            begin
              // Skip white space and comments.
              repeat
                Token := FTokenizer.NextToken;
              until not (Token in [toWhiteSpace, toSLComment, toMLComment]);

              if Token <> toEOF then
              begin
                CurrentDelimiter := Token;
                repeat
                  Token := FTokenizer.NextToken;
                  if Token in [toEOF, toWhiteSpace, toSLComment, toMLComment] then
                    Break;
                  CurrentDelimiter := CurrentDelimiter + Token;
                until False;

                // The delimiter statement ends here. Don't add it to the list.
              end
              else
                Break;
            end
            else
            begin
              Start := FTokenizer.TokenPosition;
              if FTokenizer.TokenSymbolIs('use') then
              begin
                // Database is about to change. Extract the name and store it in the data.
                // This way we don't need to check every line later for a use command.

                // Skip white spaces and comments.
                repeat
                  Token := FTokenizer.NextToken;
                until not (Token in [toWhiteSpace, toSLComment, toMLComment]);
                if Token <> toEOF then
                begin
                  NewSchema := FTokenizer.TokenString;
                  if Token = toString then
                    NewSchema := Unquote(NewSchema);

                  // Continue main loop until the delimiter appears.
                end;
              end;
            end;
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
    Data.FEndState := FTokenizer.State;
    Data.FAnalyzed := True;
    Data.Delimiter := CurrentDelimiter;

    Line.ClearMarkers;
    if Data.GetFirstStatementStart > -1 then
    begin
      Line.AddMarker(FDebugInfoMarker);
      if Data.FHasBreakpoint then
        Line.AddMarker(FBreakPointMarker)
      else
        Line.AddMarker(FEmptyMarker);
    end;
  finally
    Stream.Free;
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

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.DoDisplaySearch(ShowReplacePage: Boolean);

var
  Intf: IAdaptable;
  Component: TComponent;
  Provider: IOptionProvider;

begin
  if TextSearchForm = nil then
  begin
    Component := Self;
    while Component.Owner <> nil do
    begin
      if (Component is TCustomForm) and Supports(Component, IAdaptable, Intf) then
      begin
        Provider := Intf.GetAdapter(IOptionProvider) as IOptionProvider;
        TextSearchForm := TTextSearchForm.Create(Self, Provider);
        Break;
      end;
      Component := Component.Owner;
    end;
  end;

  if ShowReplacePage then
    TextSearchForm.PageControl.ActivePage := TextSearchForm.ReplaceTabSheet;


  TextSearchForm.Show;
  TextSearchForm.OnSearch := DoSearch;
end;

//----------------------------------------------------------------------------------------------------------------------

function TScriptEditor.DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
  SearchOptions: TTextSearchOptions): Integer;

var
  SOptions: TSearchOptions;

begin
  // Translate Options
  SOptions := [];
  if tsoBackwards in SearchOptions then
    Include(SOptions, soBackwards);
  if tsoEntireScope in SearchOptions then
    Include(SOptions, soEntireScope);
  if tsoIgnoreNonSpacing in SearchOptions then
    Include(SOptions, soIgnoreNonSpacing);
  if tsoMatchCase in SearchOptions then
    Include(SOptions, soMatchCase);
  if tsoPrompt in SearchOptions then
    Include(SOptions, soPrompt);
  if tsoRegularExpression in SearchOptions then
    Include(SOptions, soRegularExpression);
  if tsoReplace in SearchOptions then
    Include(SOptions, soReplace);
  if tsoReplaceAll in SearchOptions then
    Include(SOptions, soReplaceAll);
  if tsoSelectedOnly in SearchOptions then
    Include(SOptions, soSelectedOnly);
  if tsoSpaceCompress in SearchOptions then
    Include(SOptions, soSpaceCompress);
  if tsoWholeWord in SearchOptions then
    Include(SOptions, soWholeWord);

  Result := SearchReplace(SearchText, ReplaceText, SOptions);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptEditor.OptionChanged;

var
  Temp: Integer;
  
begin
  Font.Name := FOptionProvider.OptionAsString['CodeFontName'];
  Font.Height := FOptionProvider.OptionAsInteger['CodeFontHeight'];
  Temp := FOptionProvider.OptionAsInteger['CodeFontWidth'];
  if Temp > 0 then
    CharWidth := Temp;
  
  Font.Charset := MYXCommonOptions.CodeFontCharset;
  if FOptionProvider.OptionAsBoolean['EditorShowWhitespaces'] then
    Options := Options + [eoShowControlChars]
  else
    Options := Options - [eoShowControlChars];
end;

//----------------- TScriptErrorGrid -----------------------------------------------------------------------------------

constructor TScriptErrorGridOld.Create(AOwner: TComponent; OptionProvider: IOptionProvider);

var
  TVColumn: TVirtualTreeColumn;
  PopupItem: TTntMenuItem;

begin
  inherited Create(AOwner);

  FOptionProvider := OptionProvider;
  OptionProvider.AddListener(Self);
  FScriptPanel := TScriptPanel(AOwner);
  NodeDataSize := SizeOf(TScriptMsg);

  Header.Options := Header.Options + [hoDblClickResize, hoVisible];
  Header.Font.Name := OptionProvider.OptionAsString['DataFontName'];
  Header.Font.Size := -8;
  Font.Name := OptionProvider.OptionAsString['CodeFontName'];
  Font.Height := OptionProvider.OptionAsInteger['CodeFontHeight'];
  TVColumn := Header.Columns.Add;
  TVColumn.Text := '!';
  TVColumn.Width := 16;
  TVColumn := Header.Columns.Add;
  TVColumn.Text := 'Line';
  TVColumn.Width := 50;
  TVColumn := Header.Columns.Add;
  TVColumn.Text := 'Description';
  TVColumn := Header.Columns.Add;
  TVColumn.Text := 'ErrorNr.';
  TVColumn.Width := 70;
  Header.AutoSizeIndex := 2;
  Header.Options := Header.Options + [hoAutoResize] - [hoColumnResize];
  ParentBackground := False;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];
  TreeOptions.StringOptions := TreeOptions.StringOptions - [toSaveCaptions];
  HintMode := hmTooltip;
  ShowHint := True;
  DefaultNodeHeight := 16;
  Align := alBottom;
  Height := 0;

  ErrorPopupMenu := TTntPopupMenu.Create(AOwner);
  PopupMenu := ErrorPopupMenu;

  PopupItem := TTntMenuItem.Create(ErrorPopupMenu);
  PopupItem.Caption := _('Clear Messages');
  PopupItem.OnClick := ClearMessages;
  ErrorPopupMenu.Items.Add(PopupItem);

  //Load PNG Images
  RSErrorPNGImg := LoadPNGImageFromResource('rs_error');
  RSWarningPNGImg := LoadPNGImageFromResource('rs_warning');
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TScriptErrorGridOld.Destroy;

begin
  FOptionProvider.RemoveListener(Self);
  
  RSErrorPNGImg.Free;
  RSWarningPNGImg.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.ClearMessages(Sender: TObject);

begin
  FScriptPanel.FErrorMessages.Clear;
  DoMessagesChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.DoMessagesChanged;

var
  Node: PVirtualNode;
  Nodedata: PScriptMsg;

begin
  if (FScriptPanel.FErrorMessages.Count = 0) then
  begin
    Height := 0;
    Clear;
  end
  else
  begin
    Clear;

    Canvas.Font := Font;
    RootNodeCount := FScriptPanel.FErrorMessages.Count;
    Node := GetFirst;
    while Assigned(Node) do
    begin
      NodeData := GetNodeData(Node);
      NodeData.MsgIndex := Node.Index;
      Multiline[Node] := True;
      NodeHeight[Node] := ComputeNodeHeight(Canvas, Node, 2);

      Node := GetNextSibling(Node);
    end;

    FocusedNode := Node;
                         
    if RootNodeCount = 1 then
     ClientHeight := 1; // Client height bug. The first setting seems not to be honoured.
    ClientHeight := Min(RootNode.TotalHeight - DefaultNodeHeight, 200);

    FScriptPanel.FScriptEditor.EnsureCursorPosVisible;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: WideString);

var
  NodeData: PScriptMsg;

begin
  if (Assigned(OnGetText)) then
    OnGetText(Self, Node, Column, TextType, Text)
  else
  begin
    if (Column = 0) then
    begin
      Text := '';
    end
    else
    begin
      NodeData := GetNodeData(Node);

      if (NodeData <> nil) then
        if (NodeData.MsgIndex < FScriptPanel.FErrorMessages.Count) then
          case Column of
            1:
              Text := IntToStr(TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).Line);
            2:
              Text := TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).Msg;
            3:
              if (TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).MsgNr <> 0) then
                Text := IntToStr(TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).MsgNr)
              else
                Text := '';
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);

var
  NodeData: PScriptMsg;

begin
  if (Column = 0) then
  begin
    NodeData := nil;
    if (Node <> nil) then
      NodeData := GetNodeData(Node);

    if (NodeData <> nil) then
      if (NodeData.MsgIndex < FScriptPanel.FErrorMessages.Count) then
      begin
        case TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).MsgType of
          smtError:
            RSErrorPNGImg.Draw(Canvas, Rect(4, 2,
              RSErrorPNGImg.Width + 1, RSErrorPNGImg.Height + 1));
          smtWarning:
            RSWarningPNGImg.Draw(Canvas, Rect(4, 2,
              RSWarningPNGImg.Width + 1, RSWarningPNGImg.Height + 1));
        end;
      end;
  end;

  if (Assigned(OnAfterCellPaint)) then
    OnAfterCellPaint(Self, Canvas, Node, Column, CellRect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.DblClick;

var
  NodeData: PScriptMsg;

begin
  if (Assigned(OnDblClick)) then
    OnDblClick(Self)
  else
  begin
    NodeData := nil;
    if (FocusedNode <> nil) then
      NodeData := GetNodeData(FocusedNode);

    if (NodeData <> nil) then
    begin
      FScriptPanel.FScriptEditor.CaretY :=
        TScriptErrorOld(FScriptPanel.FErrorMessages[NodeData.MsgIndex]).Line-1;
      FScriptPanel.FScriptEditor.SetFocus;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TScriptErrorGridOld.OptionChanged;

begin
  Font.Name := FOptionProvider.OptionAsString['CodeFontName'];
  Font.Height := FOptionProvider.OptionAsInteger['CodeFontHeight'];
  Header.Font.Name := FOptionProvider.OptionAsString['DataFontName'];
  Header.Font.Size := -8;
end;

//----------------- TScriptError ---------------------------------------------------------------------------------------

constructor TScriptErrorOld.Create(Msg: WideString; MsgType: TScriptMessageType; MsgNr: Integer; Line: Integer;
  CharPos: Integer);

begin
  Self.Msg := Msg;
  Self.MsgType := MsgType;
  Self.MsgNr := MsgNr;
  Self.Line := Line;
  Self.CharPos := CharPos;
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
    Result := FStatements[0].Stop;
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetFirstStatementRaw: PStatement;

begin
  Result := nil;
  if Length(FStatements) > 0 then
    Result := @FStatements[0];
end;

//----------------------------------------------------------------------------------------------------------------------

function TLineData.GetLastStatement: PStatement;

// Returns the last statement that begins on  this line or nil if there is no statement or
// it begins on an earlier line.

begin
  Result := nil;
  if Length(FStatements) > 0 then
  begin
    Result := @FStatements[High(FStatements)];
    if Result.Start < 0 then
      Result := nil;
  end;
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
  if Length(FStatements) > 0 then
  begin
    Result := FStatements[0].Start;
    if (Result = -1) and (Length(FStatements) > 1) then
      Result := FStatements[1].Start;
  end;
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

function TLineData.GetStatementCount: Integer;

begin
  Result := Length(FStatements);
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

//----------------------------------------------------------------------------------------------------------------------

end.

