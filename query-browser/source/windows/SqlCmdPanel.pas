unit SqlCmdPanel;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Classes, Controls,
  Graphics,
  TntExtCtrls, TntComCtrls, TntClasses, TntStdCtrls, Forms,
  UniCodeEditor, UniCodeConsole, VirtualTrees, TableDrag,
  MySQLConnection, ApplicationDataModule,
  myx_public_interface;

type
  TSQLCmdTabSheet = class(TTntPanel)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CommandLineEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CommandLineEdDragDrop(Sender, Source: TObject;
      X, Y: Integer);
    procedure CommandLineEdDragOver(Sender, Source: TObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SQLMemoEnter(Sender: TObject);
    procedure CommandLineEdExecuteCommand(Cmd: WideString;
      Key: Word; Shift: TShiftState);
    function CommandLineEdIsMultilineCommand(Cmd: WideString;
      Key: Word; Shift: TShiftState): Boolean;
    procedure InitializeShell;
  public
    CommandLineEd: TUniCodeConsole;
    MySQLConn: TMySQLConn;
    TableDragForm: TTableDragForm;
    PTxShell: PMYX_TEXT_SHELL;
  end;

function TxShellOutput(text: PChar; user_data: Pointer): integer cdecl;

implementation

uses QueryBrowser;

var
  ShellOutputBuffer: WideString;
  GlobalConsoleBreak: Boolean;

function TxShellOutput(text: PChar; user_data: Pointer): integer cdecl;

var
  PSender: ^TUniCodeEdit;
  Output: WideString;

begin
  PSender := user_data;

  Output := UTF8Decode(text);
  if (Copy(Output, Length(Output), 1) <> #10) then
    ShellOutputBuffer := ShellOutputBuffer + Output
  else
  begin
    if (ShellOutputBuffer <> '') then
    begin
      PSender.Content.Text := ShellOutputBuffer + Output;
      ShellOutputBuffer := '';
    end
    else
      PSender.Content.Text := Output;
  end;

  Result := Ord(GlobalConsoleBreak);
  GlobalConsoleBreak := False;

  Application.ProcessMessages;
end;

constructor TSQLCmdTabSheet.Create(AOwner: TComponent);
begin
  CommandLineEd := TUniCodeConsole.Create(self);
  CommandLineEd.Parent := self;
  CommandLineEd.Align := alClient;
  CommandLineEd.Name := 'CommandLineEd';
  CommandLineEd.OnDragDrop := CommandLineEdDragDrop;
  CommandLineEd.OnDragOver := CommandLineEdDragOver;
  CommandLineEd.OnEnter := SQLMemoEnter;
  CommandLineEd.OnExecuteCommand := CommandLineEdExecuteCommand;
  CommandLineEd.OnIsMultilineCommand := CommandLineEdIsMultilineCommand;
  CommandLineEd.OnKeyDown := CommandLineEdKeyDown;

  CommandLineEd.Color := clBlack;
  CommandLineEd.CharWidth := 6;
  CommandLineEd.ConsoleDelimiter := ';';
  CommandLineEd.ConsolePrompt := '>';
  CommandLineEd.Cursor := crIBeam;
  CommandLineEd.Font.Name := 'Bitstream Vera Sans Mono';
  CommandLineEd.Font.Color := clSilver;
  CommandLineEd.GutterWidth := 0;
  CommandLineEd.InsertCaret := ctHalfBlock;
  CommandLineEd.MaxUndo := 32000;
  CommandLineEd.Options := CommandLineEd.Options - [eoAutoIndent,
    eoAutoUnindent, eoHideSelection, eoScrollPastEOL];
  CommandLineEd.Options := CommandLineEd.Options + [eoKeepTrailingBlanks];

  InitializeShell;

  ShellOutputBuffer := '';

  GlobalConsoleBreak := False;

  TableDragForm := nil;
  PTxShell := nil;
end;

destructor TSQLCmdTabSheet.Destroy;
begin
  if (PTxShell <> nil) then
    myx_finalize_text_shell(PTxShell);
end;

procedure TSQLCmdTabSheet.CommandLineEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Cmd: WideString;
begin
  if (Key = VK_TAB) then
  begin
    cmd := Copy(CommandLineEd.Content[CommandLineEd.CaretY].Text, 8,
      Length(CommandLineEd.Content[CommandLineEd.CaretY].Text));

    if (CompareText(cmd, Copy('select', 1, Length(cmd))) = 0) then
      CommandLineEd.Content[CommandLineEd.CaretY].Text := CommandLineEd.ConsolePrompt + 'select ';

    CommandLineEd.CaretX := Length(CommandLineEd.Content[CommandLineEd.CaretY].Text);

    Key := 0;
  end
  else
    if (Key = Ord('C')) and (ssCtrl in Shift) then
    begin
      GlobalConsoleBreak := True;
    end;
end;

procedure TSQLCmdTabSheet.CommandLineEdDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Node: PVirtualNode;
  BookmarkNodeData: PBookmarkNodeData;
  HistoryNodeData: PHistoryNodeData;
  NewSelStart: Integer;
begin
  if (Source is TUniCodeEdit) then
  begin
    CommandLineEd.ConsoleCommand := TUniCodeEdit(Source).Text;
  end
  else
    if (Source is TVirtualStringTree) then
    begin
      if (TVirtualStringTree(Source).Name = 'BookmarkVT') then
      begin
        Node := TVirtualStringTree(Source).FocusedNode;
        if (Node <> nil) then
        begin
          BookmarkNodeData := TVirtualStringTree(Source).GetNodeData(Node);
          if (BookmarkNodeData.bookmark <> nil) then
            CommandLineEd.ConsoleCommand := BookmarkNodeData.bookmark.sql;
        end;
      end
      else
        if (TVirtualStringTree(Source).Name = 'HistoryVT') then
        begin
          Node := TVirtualStringTree(Source).FocusedNode;
          if (Node <> nil) then
          begin
            HistoryNodeData := TVirtualStringTree(Source).GetNodeData(Node);
            if (HistoryNodeData.NodeType = HISTORY_ENTRY_TYPE) then
              CommandLineEd.ConsoleCommand := Trim(PPMYX_HISTORY_ENTRY(HistoryNodeData.Data)^.sql);
          end;
        end
        else
          if (TVirtualStringTree(Source).Name = 'CatalogVST') then
          begin
            CommandLineEd.ConsoleCommandExcludeDelim :=
              BuildDragSQLCommand(MySQLConn,
              TVirtualStringTree(Source),
              CommandLineEd.ConsoleCommandExcludeDelim, NewSelStart);
            CommandLineEd.ConsoleCommandSelStart := NewSelStart;

            if (CommandLineEd.CanFocus) then
              CommandLineEd.SetFocus;
          end;
    end;

  CommandLineEd.Invalidate;

  CommandLineEd.SetFocus;
end;

procedure TSQLCmdTabSheet.CommandLineEdDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if (Source is TUniCodeEdit) or
    ((Source is TVirtualStringTree) and
    ((TVirtualStringTree(Source).Name = 'HistoryVT') or
    (TVirtualStringTree(Source).Name = 'BookmarkVT') or
    (TVirtualStringTree(Source).Name = 'CatalogVST'))) then
    Accept := True;

  if (GetKeyState(VK_MENU) < 0) or
    (not (ApplicationDM.QBOptions.ShowDragTargetWindowOnAltPressedOnly)) then
  begin
    //ConsoleDropTargetActionBarPaintBox.Visible:=True;

    if (TableDragForm = nil) then
    begin
      TableDragForm := TTableDragForm.Create(nil);
      TableDragForm.PlaceFormBelow(CommandLineEd);
      TableDragForm.Show;
    end;
  end;
end;

procedure TSQLCmdTabSheet.SQLMemoEnter(Sender: TObject);
begin
  {if(LastFocusedControl<>Sender)then
    if(Sender.InheritsFrom(TWinControl))then
      LastFocusedControl:=TWinControl(Sender);}
end;

procedure TSQLCmdTabSheet.CommandLineEdExecuteCommand(Cmd: WideString;
  Key: Word; Shift: TShiftState);
var
  Param: WideString;
  pRetValue: PChar;
begin
  //Command: USE
  Param := myx_parse_sqlmemo_command_use(Cmd);
  if (Param <> '') then
  begin
    CommandLineEd.PrepareNextConsoleCommand;

    MySQLConn.DefaultSchema := Param;
    Exit;
  end;

  //Command: DELIMITER
  Param := myx_parse_sqlmemo_command_delimiter(Cmd);
  if (Param <> '') then
  begin
    CommandLineEd.PrepareNextConsoleCommand;

    CommandLineEd.ConsoleDelimiter := Param;

    Exit;
  end;

  //Command: Exit
  if (myx_parse_sqlmemo_command_exit(Cmd) = 1) then
  begin
    Application.Terminate;
    Exit;
  end;

  //Command: HELP
  pRetValue := _myx_parse_sqlmemo_command_help(PChar(UTF8Encode(Cmd)));
  if (pRetValue <> nil) then
  begin
    myx_ts_display_help(PTxShell, UTF8Decode(Param));

    g_free(pRetValue);

    CommandLineEd.PrepareNextConsoleCommand;

    Exit;
  end
  else
    g_free(pRetValue);

  //Ctrl is held
  if (Shift = [ssCtrl]) then
  begin
    {SQLMemo.Text:=Cmd;

    QueryExecuteClick(Self);

    CommandLineEd.PrepareNextConsoleCommand;}
  end
  //Ctrl+Shift is held
  else
    if (Shift = [ssCtrl, ssShift]) then
    begin
    {SQLMemo.Text:=Cmd;

    QueryExecuteInNewTabClick(Self);

    CommandLineEd.PrepareNextConsoleCommand;}
    end
  //Ctrl+Alt is held
    else
      if (Shift = [ssCtrl, ssShift]) then
      begin
    {SQLMemo.Text:=Cmd;

    QuerySplitAndExecuteClick(Self);

    CommandLineEd.PrepareNextConsoleCommand;}
      end
  //Command: SQL
      else
      begin
    //Call shell command
        MySQLConn.Lock.Acquire;
        try
          Cmd := Trim(Cmd);
          Cmd := Copy(Cmd, 1, Length(Cmd) -
            Length(CommandLineEd.ConsoleDelimiter));

          myx_ts_execute_command(PTxShell, Cmd);
        finally
          MySQLConn.Lock.Release;
        end;

        CommandLineEd.PrepareNextConsoleCommand;
      end;
end;

function TSQLCmdTabSheet.CommandLineEdIsMultilineCommand(Cmd: WideString;
  Key: Word; Shift: TShiftState): Boolean;
var
  Param: WideString;
  pRetValue: PChar;
begin
  Result := True;

  //Command: DELIMITER
  Param := myx_parse_sqlmemo_command_delimiter(Cmd);
  if (Param <> '') then
    Result := False;

  //Command: HELP
  pRetValue := _myx_parse_sqlmemo_command_help(PChar(UTF8Encode(Cmd)));
  if (pRetValue <> nil) then
    Result := False;
  g_free(pRetValue);

  //Command: Exit
  if (myx_parse_sqlmemo_command_exit(Cmd) = 1) then
    Result := False;
end;

procedure TSQLCmdTabSheet.InitializeShell;
begin
  CommandLineEd.Text := 'MySQL GUI Command Line' + #13#10;

  if (MySQLConn.Connected) then
  begin
    CommandLineEd.Content.AddLine(
      Format(_('Your MySQL connection id is %d to server version : %s'),
      [myx_get_thread_id(MySQLConn.MySQL),
      myx_get_mysql_full_version(MySQLConn.MySQL)]));
    CommandLineEd.Content.AddLine('');
    CommandLineEd.Content.AddLine(
      Format(_('Type ''%s'' or ''%s'' for help. Type ''%s'' to clear the screen.'),
      ['help;', '\h', 'cls']));

    PTxShell := myx_init_text_shell(MySQLConn.MySQL);
    if (PTxShell <> nil) then
      myx_ts_set_output_callback(PTxShell, Addr(CommandLineEd), @TxShellOutput);

    CommandLineEd.Content.AddLine('');

    CommandLineEd.ConsolePrompt :=
      myx_get_default_schema(MySQLConn.MySQL) + '> ';

    CommandLineEd.PrepareNextConsoleCommand;
  end
  else
  begin
    CommandLineEd.Content.AddLine('');
    CommandLineEd.Content.AddLine('Not connected.');
  end;
end;

end.

