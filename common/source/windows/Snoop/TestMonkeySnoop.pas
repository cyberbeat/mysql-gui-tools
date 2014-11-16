unit TestMonkeySnoop;

//----------------------------------------------------------------------------------------------------------------------
//
// Copyright (C) 2006 MySQL AB
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
//
//----------------------------------------------------------------------------------------------------------------------
//
// TestMonkeySnoop is a support unit used to connect the MySQL UI test tool "Test Monkey" to a Delphi application.
//
//----------------------------------------------------------------------------------------------------------------------

interface

uses
  Windows, Types, Classes, Controls, SyncObjs, Forms,
  IdTCPServer, IdContext, IdTCPConnection;

const
  NewLine     = WideString(#10);
  Separator   = WideChar(':');
  TM          = WideString('TM');
  ERROR       = WideString('ERROR');
  ReplyStart  = TM + Separator;

type
  // Indicates what reply to send.
  TReplyType = (
    rtNormal,           // Standard answer, just tell everything is fine.
    rtError             // There was an error.
  );

  TCommand = (
    cmUnknown,
    cmTopLevelWindows,
    cmTree,
    cmProperties,
    cmInfo,
    cmBounds,
    cmQuery,
    cmCheck,
    cmWait,
    cmFlash,
    cmChildCount,
    cmListTextpos,
    cmListIndexpos,
    cmListSelected,
    cmListCheckstate,
    cmCheckboxCheckstate,
    cmRadiogroupSelected,
    cmRangeInfo,
    cmDatetimeInfo,
    cmClickable,
    cmClipboard,
    cmMenus,
    cmMenuItems,
    cmQuit
  );

  TParameters = array of WideString;

  TSnoopModule = class
  strict private
    FServer: TIdTCPServer;
    FExcludes: TList;
    FLock: TCriticalSection;
    FCurrentParameters: TParameters;
    FCurrentCommand: TCommand;
    FCurrentCommandString: WideString;
    FReply: WideString;

    function SearchListview(Control: TControl; SearchColumn: Integer; SearchText: WideString): WideString;
  protected
    procedure HandleCommand;
    function IsExcluded(Component: TComponent): Boolean;
    procedure SendReply(Connection: TIdTCPConnection; const Reply: WideString);
    procedure ServerConnect(AThread: TIdContext);
    procedure ServerExecute(AThread: TIdContext);
    function StripCommand(const Parameters: TParameters): TParameters;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddExclude(const Component: TComponent);
    function ComposeBoundsReply(const R: TRect; AddKeyword: Boolean): WideString;
    function ComposeCoordsReply(const X, Y: Integer): WideString;
    function ComposeDateReply(const Date: TDateTime): WideString;
    function ComposeReplyLine(ReplyType: TReplyType; Parameters: array of const): WideString;
    function ComposeTimeReply(const Time: TDateTime): WideString;
    function HandleBounds(const Parameters: TParameters): WideString;
    function HandleCheck(const Parameters: TParameters): WideString;
    function HandleCheckstate(const Parameters: TParameters): WideString;
    function HandleChildcount(const Parameters: TParameters): WideString;
    function HandleClickable(const Parameters: TParameters): WideString;
    function HandleClipboard(const Parameters: TParameters): WideString;
    function HandleDatetimeinfo(const Parameters: TParameters): WideString;
    function HandleFlash(const Parameters: TParameters): WideString;
    function HandleInfo(const Parameters: TParameters): WideString;
    function HandleListCheckstate(const Parameters: TParameters): WideString;
    function HandleListIndexpos(const Parameters: TParameters): WideString;
    function HandleListSelected(const Parameters: TParameters): WideString;
    function HandleListTextpos(const Parameters: TParameters): WideString;
    function HandleMenus(const Parameters: TParameters): WideString;
    function HandleMenuItems(const Parameters: TParameters): WideString;
    function HandleProps(const Parameters: TParameters): WideString;
    function HandleRadiogroupSelected(const Parameters: TParameters): WideString;
    function HandleQuery(const Parameters: TParameters): WideString;
    function HandleQuit: WideString;
    function HandleRangeinfo(const Parameters: TParameters): WideString;
    function HandleTopLevels(const Parameters: TParameters): WideString;
    function HandleTree(const Parameters: TParameters): WideString;
    function HandleWaitidle: WideString;
    function SplitCommand(const Command: WideString; var Parameters: TParameters): WideString;
    function StartupServer: Boolean;
    function ShutdownServer: Boolean;
  end;

function EscapeString(const S: WideString): WideString;

var
  SnoopActive: Boolean;
  SnoopModule: TSnoopModule;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  SysUtils, Unicode, TypInfo, Graphics, Messages, ExtCtrls, StdCtrls, ComCtrls, CheckLst,
  Clipbrd, Menus,
  TntControls, TntForms, TntStdCtrls, TntComCtrls, TntClasses, TntMenus;

const
  Visibility: array[Boolean] of WideString = ('HIDDEN', 'SHOWN');

var
  Commands: TStringList;

//----------------------------------------------------------------------------------------------------------------------

// Specific variables used for top-level window enumeration.
var
  TopLevelWindow: HWnd;

function EnumWindowsProc(Window: HWnd; lParam: LPARAM): BOOL; stdcall;

var
  P: PPoint;
  R: TRect;
  CurrentWindow: HWnd;

begin
  // Consider only visible windows.
  if IsWindowVisible(Window) then
  begin
    P := Pointer(lParam);
    GetWindowRect(Window, R);
    if PtInRect(R, P^) then
    begin
      // If this is the first window we got then just keep it.
      if TopLevelWindow = 0 then
        TopLevelWindow := Window
      else
      begin
        // If we already got a window then check by going backwards in the z-order from the last found window
        // and see if we find the one passed in to the function. If we find it then it is higher in
        // z-order than the previous one and we keep this instead.
        CurrentWindow := TopLevelWindow;
        repeat
          CurrentWindow := GetNextWindow(CurrentWindow, GW_HWNDPREV);
          if CurrentWindow = Window then
          begin
            TopLevelWindow := Window;
            Break;
          end;
        until CurrentWindow = 0;
      end;
    end;
  end;

  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function WindowFromPointEx(pt: TPoint): HWnd;

// Find window under specified point (screen coordinates).

begin
  TopLevelWindow := 0;
  EnumWindows(@EnumWindowsProc, Integer(@pt));
  Result := TopLevelWindow;
end;

//----------------------------------------------------------------------------------------------------------------------

function FindChildControl(Control: TWinControl; P: TPoint): TControl;

var
  Child: TControl;
begin
  Result := Control.ControlAtPos(Control.ScreenToClient(P), True, True, False);
  if Result is TWinControl and Result.Visible and TWinControl(Result).HandleAllocated then
  begin
    Child := FindChildControl(TWinControl(Result), P);
    if Assigned(Child) then
      Result := Child;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function EscapeString(const S: WideString): WideString;

// Scans the given string and masks all characters with a special meaning using a backslash.

var
  I: Integer;

begin
  Result := '';

  // This code is not particularly optimized. We only deal with short strings at a low rate
  // so this simple code is enough for now.
  // For speed increase avoid frequent memory reallocations (caused by assembling the result char by char).
  for I := 1 to Length(S) do
    case S[I] of
      Separator, '\':
        Result := Result + '\' + S[I];
      #13:
        Result := Result + '\r';
      #10:
        Result := Result + '\n';
    else
      Result := Result + S[I];
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function IsDelphiClass(Obj: Pointer): Boolean;

// Returns true if Obj is a Delphi class.

var
  ClassPointer: Pointer;

begin
  if Assigned(Obj) and (Cardinal(Obj) > $FFFF) then
  begin
    ClassPointer := PPointer(Obj)^;
    Result := PPointer(Integer(ClassPointer) + vmtSelfPtr)^ = ClassPointer;
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetComponent(const Reference: WideString): TComponent;

// Interprets the given reference either as pointer value or path to an existing component.
// If the reference is a path then it is parsed and recursively interpreted. nil is returned
// if there was no such entry.

  //----------------------------------------------------------------------------

  function FindComponent(Parent: TComponent; const Path: WideString): TComponent;

  // Recursively scans the path and returns the addressed component.

  var
    SubEntry: WideString;
    DotPos: Integer;
    I: Integer;
    Run: TControl;

  begin
    DotPos := Pos('.', Path);
    if DotPos = 0 then
      SubEntry := Path
    else
      SubEntry := Copy(Path, 1, DotPos - 1);

    Result := nil;

    // Handle special case of anonymous controls first.
    // If a name begins with a capitel letter T and contains an underbar
    // then everything after that bar is consider a hex number representing
    // a pointer in memory (hex numbers are always stored with 8 digits).
    if (Length(SubEntry) > 0) and (SubEntry[1] = 'T') and (Pos('_', SubEntry) > 1) then
    begin
      Result := Pointer(StrToInt('$' + Copy(SubEntry, Pos('_', SubEntry) + 1, 8)));
    end
    else
    begin
      if Parent is TWinControl then
      begin
        for I := 0 to TWinControl(Parent).ControlCount - 1 do
        begin
          Run := TWinControl(Parent).Controls[I];
          if SameText(Run.Name, SubEntry) then
          begin
            Result := Run;
            Break;
          end;
        end;
      end;

      if Result = nil then
      begin
        if Parent = Screen then
        begin
          // For the screen we use the special forms property instead of Components.
          for I := 0 to Screen.FormCount - 1 do
            if Screen.Forms[I].Name = SubEntry then
            begin
              Result := Screen.Forms[I];
              Break;
            end;
        end
        else
          Result := Parent.FindComponent(SubEntry);
      end;
    end;

    if Assigned(Result) and (DotPos > 0) then
      Result := FindComponent(Result, Copy(Path, DotPos + 1, MaxInt));
  end;

  //----------------------------------------------------------------------------

begin
  if Reference[1] in [WideChar('0')..WideChar('9')] then
  begin
    Result := Pointer(StrToInt(Reference));
    if not IsDelphiClass(Result) then
      Result := nil;
  end
  else
    Result := FindComponent(Screen, Reference);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetControl(const Reference: WideString): TControl;

// Same as GetComponent but with the additional check for the component if it is a control.

var
  Component: TComponent;

begin
  Result := nil;
  Component := GetComponent(Reference);

  if Component is TControl then
    Result := Component as TControl;
end;

//----------------------------------------------------------------------------------------------------------------------

function GetComponentName(Component: TComponent): WideString;

// Creates a new and unique anonymous name, depending on an internal counter.

begin
  if Component.Name = '' then
    Result := WideFormat('%s_%.8x', [Component.ClassName, Integer(Component)])
  else
    Result := Component.Name;
end;

//----------------- TSnoopModule ---------------------------------------------------------------------------------------

constructor TSnoopModule.Create;

begin
  FLock := TCriticalSection.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSnoopModule.Destroy;

begin
  ShutdownServer;
  FExcludes.Free;
  FLock.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TTreeviewAccess = class(TCustomTreeview);
  TTntTreeviewAccess = class(TTntCustomTreeview);
  TListviewAccess = class(TCustomListview);
  TTntListviewAccess = class(TTntCustomListview);
  TCheckboxAccess = class(TCustomCheckbox);
  TListboxAcess = class(TCustomListBox);
  TTntListboxAcess = class(TTntCustomListBox);
  TCalendarAccess = class(TCommonCalendar);

function TSnoopModule.SearchListview(Control: TControl; SearchColumn: Integer; SearchText: WideString): WideString;

// Searches the given control (which must be a standard or TNT listview control) for the given text and
// composes a reply string.

var
  ColumnIndex: Integer;
  Index: Integer;
  R: TRect;

begin
  if Control is TTntCustomListview then
  begin
    with TTntListviewAccess(Control) do
    begin
      if SearchColumn = 0 then
      begin
        // Search the main column.
        for Index := 0 to Items.Count - 1 do
          if Items[Index].Caption = SearchText then
          begin
            R := Items[Index].DisplayRect(drBounds);
            R.Right := R.Left + Column[0].Width;
            if (R.Bottom > 0) and (R.Top < ClientHeight) and (R.Right > 0) and (R.Left < ClientWidth) then
              Result := ComposeCoordsReply(R.Left, R.Top);
            Break;
          end;
      end
      else
        if SearchColumn < Columns.Count then
        begin
          Dec(SearchColumn);
          for Index := 0 to Items.Count - 1 do
            if (SearchColumn < Items[Index].SubItems.Count) and (Items[Index].SubItems[SearchColumn] = SearchText) then
            begin
              R := Items[Index].DisplayRect(drBounds);
              if ViewStyle = vsReport then
              begin
                R.Left := 0;
                for ColumnIndex := 0 to SearchColumn do
                  Inc(R.Left, Column[ColumnIndex].Width);
                R.Right := R.Left + Column[SearchColumn + 1].Width;
              end;
              if (R.Bottom > 0) and (R.Top < ClientHeight) and (R.Right > 0) and (R.Left < ClientWidth) then
                Result := ComposeCoordsReply(R.Left, R.Top);
              Break;
            end;
        end
        else
          Result := ComposeReplyLine(rtError, ['invalid column']);
    end;
  end
  else
    if Control is TCustomListview then
    begin
      with TListviewAccess(Control) do
      begin
        if SearchColumn = 0 then
        begin
          // Search the main column.
          for Index := 0 to Items.Count - 1 do
            if Items[Index].Caption = SearchText then
            begin
              R := Items[Index].DisplayRect(drBounds);
              R.Right := R.Left + Column[0].Width;
              if (R.Bottom > 0) and (R.Top < ClientHeight) and (R.Right > 0) and (R.Left < ClientWidth) then
                Result := ComposeCoordsReply(R.Left, R.Top);
              Break;
            end;
        end
        else
          if SearchColumn < Columns.Count then
          begin
            Dec(SearchColumn);
            for Index := 0 to Items.Count - 1 do
              if (SearchColumn < Items[Index].SubItems.Count) and (Items[Index].SubItems[SearchColumn] = SearchText) then
              begin
                R := Items[Index].DisplayRect(drBounds);
                if ViewStyle = vsReport then
                begin
                  R.Left := 0;
                  for ColumnIndex := 0 to SearchColumn do
                    Inc(R.Left, Column[ColumnIndex].Width);
                  R.Right := R.Left + Column[SearchColumn + 1].Width;
                end;
                if (R.Bottom > 0) and (R.Top < ClientHeight) and (R.Right > 0) and (R.Left < ClientWidth) then
                  Result := ComposeCoordsReply(R.Left, R.Top);
                Break;
              end;
          end
          else
            Result := ComposeReplyLine(rtError, ['invalid column']);
      end;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.IsExcluded(Component: TComponent): Boolean;

// Dermines if the given component is in our exclusion list and returns true if so.

begin
  Result := Assigned(FExcludes) and (FExcludes.IndexOf(Component) > -1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSnoopModule.SendReply(Connection: TIdTCPConnection; const Reply: WideString);

// Sends the given string to the given connection and adds a closing TM: line.

begin
  with Connection.IOHandler do
  try
    Write(Utf8Encode(Reply));
    Write(Utf8Encode(TM + Separator + Newline));
  except
    // Ignore connection errors (usually from died clients).
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSnoopModule.ServerConnect(AThread: TIdContext);

begin
  SendReply(AThread.Connection, TM + Separator + 'DelphiSnoop Ready' + NewLine);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSnoopModule.ServerExecute(AThread: TIdContext);

var
  Index: Integer;

begin
  with AThread.Connection.IOHandler do
  begin
    FLock.Enter;
    try
      FCurrentCommandString := Utf8Decode(Readln);

      // Special commands, only used for interactive telnet sessions.
      if FCurrentCommandString = 'disconnect' then
        AThread.Connection.IOHandler.CloseGracefully
      else
        if FCurrentCommandString = '?' then
        begin
          FReply := '';
          for Index := 0 to Commands.Count - 1 do
            FReply := FReply + Commands[Index] + NewLine;
          SendReply(AThread.Connection, FReply);
        end
        else
        begin
          FReply := SplitCommand(FCurrentCommandString, FCurrentParameters);

          // Reply contains only error messages at this point, if something with the CommandString was wrong.
          if FReply = '' then
          begin
            // Schedule the CommandString to the proper handler method.

            FCurrentCommand := cmUnknown;
            if Commands.Find(FCurrentParameters[0], Index) then
              FCurrentCommand := TCommand(Commands.Objects[Index]);

            FLock.Leave;
            TThread.Synchronize(nil, HandleCommand);

            SendReply(AThread.Connection, FReply);
          end
          else
            SendReply(AThread.Connection, FReply);
        end;
    except
      FLock.Leave;
      // Ignore connection problems to avoid that the app crashs.
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSnoopModule.AddExclude(const Component: TComponent);

// Adds the given component to the exclude list so it is not considered when a reply is composed (e.g. used for testing).

begin
  if FExcludes = nil then
    FExcludes := TList.Create;
  FExcludes.Add(Component);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ComposeBoundsReply(const R: TRect; AddKeyword: Boolean): WideString;

// Composes a bounds response line with optional "bounds" keyword (TM:[bounds:]<x>,<y>,<w>,<h>).

var
  S: WideString;

begin
  S := WideFormat('%d,%d,%d,%d', [R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top]);
  if AddKeyWord then
    Result := ComposeReplyLine(rtNormal, ['bounds', S])
  else
    Result := ComposeReplyLine(rtNormal, [S]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ComposeCoordsReply(const X, Y: Integer): WideString;

// Composes a coordinates response line (TM:<x>,<y>).

var
  S: WideString;

begin
  S := WideFormat('%d,%d', [X, Y]);
  Result := ComposeReplyLine(rtNormal, [S]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ComposeDateReply(const Date: TDateTime): WideString;

// Converts the given date to a string using the international date and time format (YYYY-MM-DD).

var
  Year: Word;
  Month: Word;
  Day: Word;

begin
  DecodeDate(Date, Year, Month, Day);
  Result := WideFormat('%.4d-%.2d-%.2d', [Year, Month, Day]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ComposeReplyLine(ReplyType: TReplyType; Parameters: array of const): WideString;

// Composes a TM reply line using the given values and the global separator.

const
  BooleanConstants: array[Boolean] of WideString = ('FALSE', 'TRUE');

var
  I: Integer;
  S: WideString;

begin
  case ReplyType of
    rtNormal:
      Result := TM;
    rtError:
      Result := Error;
  else
    Result := 'UNDEFINED';
  end;

  if Length(Parameters) = 0 then
    Result := Result + Separator
  else
  begin
    for I := Low(Parameters) to High(Parameters) do
    begin
      case Parameters[I].VType of
        vtInteger:
          Result := Result + Separator + IntToStr(Parameters[I].VInteger);
        vtBoolean:
          Result := Result + Separator + BooleanConstants[Parameters[I].VBoolean];
        vtChar:
          Result := Result + Separator + EscapeString(Parameters[I].VChar);
        vtExtended:
          Result := Result + Separator + FloatToStr(Parameters[I].VExtended^);
        vtString:
          Result := Result + Separator + EscapeString(Parameters[I].VString^);
        vtPointer:
          if Parameters[I].VPointer = nil then
            Result := Result + Separator + 'NULL'
          else
            Result := Result + Separator + WideFormat('0x%.8x', [Integer(Parameters[I].VPointer)]);
        vtPChar:
          Result := Result + Separator + EscapeString(Parameters[I].VPChar);
        vtObject:
          if Parameters[I].VObject = nil then
            Result := Result + Separator + 'NULL'
          else
            Result := Result + Separator + WideFormat('0x%.8x', [Integer(Parameters[I].VObject)]);
        vtClass:
          if Parameters[I].VClass = nil then
            Result := Result + Separator + 'NULL'
          else
            Result := Result + Separator + WideFormat('0x%.8x', [Integer(Parameters[I].VClass)]);
        vtWideChar:
          Result := Result + Separator + EscapeString(Parameters[I].VWideChar);
        vtPWideChar:
          Result := Result + Separator + EscapeString(Parameters[I].VPWideChar);
        vtAnsiString:
          Result := Result + Separator + EscapeString(PChar(Parameters[I].VAnsiString));
        vtCurrency:
          Result := Result + Separator + CurrToStr(Parameters[I].VCurrency^);
        vtVariant:
          begin
            S := Parameters[I].VVariant^;
            Result := Result + Separator + EscapeString(S);
          end;
        vtInterface:
          if Parameters[I].VInterface = nil then
            Result := Result + Separator + 'NULL'
          else
            Result := Result + Separator + WideFormat('0x%.8x', [Integer(Parameters[I].VInterface)]);
        vtWideString:
          Result := Result + Separator + EscapeString(PWideChar(Parameters[I].VWideString));
        vtInt64:
          Result := Result + Separator + IntToStr(Parameters[I].VInt64^);
      end;
    end;
  end;
  Result := Result + NewLine;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ComposeTimeReply(const Time: TDateTime): WideString;

// Converts the given time to a string using the 24h time format (hh:mm:ss).

var
  Hour: Word;
  Minute: Word;
  Second: Word;
  MilliSecond: Word;

begin
  DecodeTime(Time, Hour, Minute, Second, MilliSecond);
  Result := WideFormat('%.2d:%.2d:%.2d', [Hour, Minute, Second]);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleBounds(const Parameters: TParameters): WideString;

// Handles the request for the bounds of a particular control.
// Note: The reply for this request does not contain the "bounds" keyword as it is used as an own command.
//       In order to get a bounds line with that keyword use ComposeBoundsReply with proper parameters.

var
  Control: TControl;
  R: TRect;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      try
        if Control.Parent = nil then
          R := Control.BoundsRect
        else
        begin
          R.TopLeft := Control.Parent.ClientToScreen(Control.BoundsRect.TopLeft);
          R.BottomRight := Control.Parent.ClientToScreen(Control.BoundsRect.BottomRight);
        end;
        Result := ComposeBoundsReply(R, False);
      except
        Result := ComposeReplyLine(rtError, ['control not found']);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleCheck(const Parameters: TParameters): WideString;

// Handles the request for the check command, which is used to look for a particular control/widget.

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      Result := ComposeReplyLine(rtNormal, [Assigned(Control)]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleCheckstate(const Parameters: TParameters): WideString;

// Handles the request to determine the check state of a single check box control.

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if Control is TCustomCheckbox then
        Result := ComposeReplyLine(rtNormal, [TCheckboxAccess(Control).Checked])
      else
        Result := ComposeReplyLine(rtError, ['invalid control']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleChildcount(const Parameters: TParameters): WideString;

// Handles the request for the childcount command, which is used to the number of children of a control/widget.

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if Control is TWinControl then
        Result := ComposeReplyLine(rtNormal, [TWinControl(Control).ControlCount])
      else
        Result := ComposeReplyLine(rtNormal, [0]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleClickable(const Parameters: TParameters): WideString;

// Handles the request for the check command, which is used to look for a particular control/widget.

var
  Component: TComponent;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Component := GetComponent(Parameters[0]);
    if Component = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if Component is TWinControl then
        Result := ComposeReplyLine(rtNormal, [TWinControl(Component).CanFocus])
      else
        Result := ComposeReplyLine(rtError, ['invalid control']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleClipboard(const Parameters: TParameters): WideString;

var
  Data: THandle;
  S: PWideChar;

begin
  Clipboard.Open;
  Data := GetClipboardData(CF_UNICODETEXT);
  if Data = 0 then
    Result := ''
  else
  begin
    S := GlobalLock(Data);
    Result := ComposeReplyLine(rtNormal, [S]);
    GlobalUnlock(Data);
  end;
  Clipboard.Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSnoopModule.HandleCommand;

begin
  case FCurrentCommand of
    cmTopLevelWindows:
      FReply := HandleToplevels(FCurrentParameters);
    cmTree:
      FReply := HandleTree(StripCommand(FCurrentParameters));
    cmProperties:
      FReply := HandleProps(StripCommand(FCurrentParameters));
    cmInfo:
      FReply := HandleInfo(StripCommand(FCurrentParameters));
    cmBounds:
      FReply := HandleBounds(StripCommand(FCurrentParameters));
    cmQuery:
      FReply := HandleQuery(StripCommand(FCurrentParameters));
    cmCheck:
      FReply := HandleCheck(StripCommand(FCurrentParameters));
    cmWait:
      FReply := HandleWaitidle;
    cmFlash:
      FReply := HandleFlash(StripCommand(FCurrentParameters));
    cmChildCount:
      FReply := HandleChildcount(StripCommand(FCurrentParameters));
    cmListTextpos:
      FReply := HandleListTextpos(StripCommand(FCurrentParameters));
    cmListIndexpos:
      FReply := HandleListIndexpos(StripCommand(FCurrentParameters));
    cmListSelected:
      FReply := HandleListselected(StripCommand(FCurrentParameters));
    cmListCheckstate:
      FReply := HandleListCheckstate(StripCommand(FCurrentParameters));
    cmCheckboxCheckstate:
      FReply := HandleCheckstate(StripCommand(FCurrentParameters));
    cmRadiogroupSelected:
      FReply := HandleRadiogroupSelected(StripCommand(FCurrentParameters));
    cmRangeInfo:
      FReply := HandleRangeInfo(StripCommand(FCurrentParameters));
    cmDatetimeInfo:
      FReply := HandleDatetimeInfo(StripCommand(FCurrentParameters));
    cmClickable:
      FReply := HandleClickable(StripCommand(FCurrentParameters));
    cmClipboard:
      FReply := HandleClipboard(StripCommand(FCurrentParameters));
    cmMenus:
      FReply := HandleMenus(StripCommand(FCurrentParameters));
    cmMenuItems:
      FReply := HandleMenuItems(StripCommand(FCurrentParameters));
    cmQuit:
      FReply := HandleQuit;
  else
    FReply := ComposeReplyLine(rtError, ['unkown command', FCurrentCommandString]);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleDatetimeinfo(const Parameters: TParameters): WideString;

// Handles the request for the datetime.info command, which returns info about date and time in controls that
// support this kind of info.

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if (Control is TCommonCalendar) then
        if Control is TMonthCalendar then
          Result := ComposeReplyLine(rtNormal, ['date', ComposeDateReply(TMonthCalendar(Control).Date)])
        else
          with TCalendarAccess(Control) do
          begin
            Result := ComposeReplyLine(rtNormal, ['date', ComposeDateReply(DateTime)]);
            Result := Result + ComposeReplyLine(rtNormal, ['time', ComposeTimeReply(DateTime)]);
          end
        else
          Result := ComposeReplyLine(rtError, ['invalid control']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleFlash(const Parameters: TParameters): WideString;

// Handles the request to highlight a control/widget.

var
  Control: TControl;
  Flash: TPanel;

begin
  Result := '';
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
    begin
      Flash := TPanel.Create(nil);
      Flash.Hide;
      Flash.Caption := '';
      Flash.Color := clYellow;
      Flash.Parent := Control.Parent;
      with Control.BoundsRect do
        Flash.SetBounds(Left, Top, Right - Left, Bottom - Top);

      Flash.Show;
      Flash.BringToFront;
      Flash.Update;
      Sleep(100);
      Flash.Hide;
      Control.Update;
      Sleep(100);
      Flash.Show;
      Flash.BringToFront;
      Flash.Update;
      Sleep(100);
      Flash.Hide;
      Control.Update;
      Sleep(100);
      Flash.Show;
      Flash.BringToFront;
      Flash.Update;
      Sleep(100);
      Flash.Free;
      Control.Update;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleInfo(const Parameters: TParameters): WideString;

// Handles the request for specific information of a particular control.

var
  Runner: TControl;
  Control: TControl;
  S: WideString;
  Window: TWinControl;
  R: TRect;
  
begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);

    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      try
        if Control is TWinControl then
          Window := Control as TWinControl
        else
          Window := nil;
        Result := ComposeReplyLine(rtNormal, ['id', Control]);

        Runner := Control.Parent;
        S := GetComponentName(Control);
        while Assigned(Runner) do
        begin
          S := GetComponentName(Runner) + '.' + S;
          Runner := Runner.Parent;
        end;
        Result := Result + ComposeReplyLine(rtNormal, ['path', S]);
        Result := Result + ComposeReplyLine(rtNormal, ['name', GetComponentName(Control)]);
        if Assigned(Window) then
        begin
          S := TntControl_GetText(Window);
          Result := Result + ComposeReplyLine(rtNormal, ['caption', S]);
        end
        else
          Result := Result + ComposeReplyLine(rtNormal, ['caption', nil]);

        Result := Result + ComposeReplyLine(rtNormal, ['class', Control.ClassName]);

        if Assigned(Window) then
          Result := Result + ComposeReplyLine(rtNormal, ['handle', Pointer(Window.Handle)])
        else
          Result := Result + ComposeReplyLine(rtNormal, ['handle', nil]);

        Result := Result + ComposeReplyLine(rtNormal, ['visible', Control.Visible]);

        if Control.Parent = nil then
          R := Control.BoundsRect
        else
        begin
          R.TopLeft := Control.Parent.ClientToScreen(Control.BoundsRect.TopLeft);
          R.BottomRight := Control.Parent.ClientToScreen(Control.BoundsRect.BottomRight);
        end;
        Result := Result + ComposeBoundsReply(R, True);
      except
        Result := ComposeReplyLine(rtError, ['control not found']);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleListCheckstate(const Parameters: TParameters): WideString;

// Handles the request to return a list of checked items in a given list control.

var
  Control: TControl;
  I: Integer;

begin
  Result := '';
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
    begin
      // Only certain controls can be handled here.
      if Control is TCheckListBox then
      begin
        with Control as TCheckListBox do
        begin
          for I := 0 to Items.Count - 1 do
            if Checked[I] then
              Result := Result + ComposeReplyLine(rtNormal, [I]);
        end;
      end
      else
        if Control is TCustomListview then
        begin
          with TListviewAccess(Control) do
          begin
            for I := 0 to Items.Count - 1 do
              if Items[I].Checked then
                Result := Result + ComposeReplyLine(rtNormal, [I]);
          end;
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleListIndexpos(const Parameters: TParameters): WideString;

// Handles the request to find an item given by an index position.

var
  Control: TControl;
  Index: Integer;
  R: TRect;

begin
  Result := '';
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
    begin
      Index := StrToIntDef(Parameters[1], -1);
      if Index < 0 then
        Result := ComposeReplyLine(rtError, ['invalid item index'])
      else
        // Only certain controls can be handled here.
        if Control is TCustomListBox then
        begin
          with Control as TCustomListBox do
          begin
            if Index < Items.Count then
            begin
              R := ItemRect(Index);
              if (R.Bottom > 0) and (R.Top < ClientHeight) then
                Result := ComposeCoordsReply(R.Left, R.Top);
            end
            else
              Result := ComposeReplyLine(rtError, ['invalid item index'])
          end;
        end
        else
          if Control is TCustomTreeview then
          begin
            with TTreeviewAccess(Control) do
            begin
              if Index < Items.Count then
              begin
                if Items[Index].IsVisible then
                begin
                  R := Items[Index].DisplayRect(False);
                  if (R.Bottom > 0) and (R.Top < ClientHeight) then
                    Result := ComposeCoordsReply(R.Left, R.Top);
                end;
              end
              else
                Result := ComposeReplyLine(rtError, ['invalid item index'])
            end;
          end
          else
            if Control is TCustomListview then
            begin
              with TListviewAccess(Control) do
              begin
                if Index < Items.Count then
                begin
                  case ViewStyle of
                    vsIcon:
                      begin
                        R := Items[Index].DisplayRect(drBounds);
                        if (R.Bottom > 0) and (R.Top < ClientHeight) and
                          (R.Right > 0) and (R.Left < ClientWidth) then
                          Result := ComposeCoordsReply(R.Left, R.Top);
                      end;
                    vsSmallIcon:
                      begin
                        R := Items[Index].DisplayRect(drBounds);
                        if (R.Bottom > 0) and (R.Top < ClientHeight) and
                          (R.Right > 0) and (R.Left < ClientWidth) then
                          Result := ComposeCoordsReply(R.Left, R.Top);
                      end;
                    vsList:
                      begin
                        R := Items[Index].DisplayRect(drBounds);
                        if (R.Bottom > 0) and (R.Top < ClientHeight) and
                          (R.Right > 0) and (R.Left < ClientWidth) then
                          Result := ComposeCoordsReply(R.Left, R.Top);
                      end;
                    vsReport:
                      begin
                        R := Items[Index].DisplayRect(drBounds);
                        if (R.Bottom > 0) and (R.Top < ClientHeight) then
                          Result := ComposeCoordsReply(R.Left, R.Top);
                      end;
                  end;
                end
                else
                  Result := ComposeReplyLine(rtError, ['invalid item index'])
              end;
            end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleListSelected(const Parameters: TParameters): WideString;

// Handles the request for a list of selected items in a listing control like list box, treeview and listview.

var
  Control: TControl;
  I: Integer;

begin
  Result := '';
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
    begin
      // Only certain controls can be handled here.
      if Control is TCustomListBox then
      begin
        with Control as TCustomListBox do
        begin
          for I := 0 to Items.Count - 1 do
            if Selected[I] then
              Result := Result + ComposeReplyLine(rtNormal, [I]);
        end;
      end
      else
        if Control is TCustomTreeview then
        begin
          with TTreeviewAccess(Control) do
          begin
            for I := 0 to Items.Count - 1 do
              if Items[I].Selected then
                Result := Result + ComposeReplyLine(rtNormal, [I]);
          end;
        end
        else
          if Control is TCustomListview then
          begin
            with TListviewAccess(Control) do
            begin
              for I := 0 to Items.Count - 1 do
                if Items[I].Selected then
                  Result := Result + ComposeReplyLine(rtNormal, [I]);
            end;
          end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleListTextpos(const Parameters: TParameters): WideString;

// Handles the request for getting the location of an item in a list like control (listbox, listview, treeview).

var
  Control: TControl;
  SearchColumn: Integer;
  SearchText: WideString;
  Index: Integer;
  R: TRect;

begin
  Result := '';
  if Length(Parameters) < 3 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Assigned(Control) then
    begin
      SearchColumn := StrToIntDef(Parameters[1], -1);
      if SearchColumn < 0 then
        Result := ComposeReplyLine(rtError, ['invalid column'])
      else
      begin
        SearchText := Parameters[2];
        if SearchText = '' then
          Result := ComposeReplyLine(rtError, ['invalid search text'])
        else
        begin
          // Only certain controls can be handled here.
          if Control is TTntCustomListBox then
          begin
            with TTntListboxAcess(Control) do
            begin
              if SearchColumn = 0 then
              begin
                Index := Items.IndexOf(SearchText);
                if Index >= 0 then
                begin
                  R := ItemRect(Index);
                  if (R.Bottom > 0) and (R.Top < ClientHeight) then
                    Result := ComposeCoordsReply(R.Left, R.Top);
                end;
              end
              else
                Result := ComposeReplyLine(rtError, ['invalid column'])
            end;
          end
          else
            if Control is TCustomListBox then
            begin
              with TListboxAcess(Control) do
              begin
                if SearchColumn = 0 then
                begin
                  Index := Items.IndexOf(SearchText);
                  if Index >= 0 then
                  begin
                    R := ItemRect(Index);
                    if (R.Bottom > 0) and (R.Top < ClientHeight) then
                      Result := ComposeCoordsReply(R.Left, R.Top);
                  end;
                end
                else
                  Result := ComposeReplyLine(rtError, ['invalid column'])
              end;
            end
            else
              if Control is TTntCustomTreeview then
              begin
                with TTNtTreeviewAccess(Control) do
                begin
                  if SearchColumn = 0 then
                  begin
                    for Index := 0 to Items.Count - 1 do
                      if Items[Index].Text = SearchText then
                      begin
                        R := Items[Index].DisplayRect(False);
                        if (R.Bottom > 0) and (R.Top < ClientHeight) then
                          Result := ComposeCoordsReply(R.Left, R.Top);
                        Break;
                      end;
                  end
                  else
                    Result := ComposeReplyLine(rtError, ['invalid column'])
                end;
              end
              else
                if Control is TCustomTreeview then
                begin
                  with TTreeviewAccess(Control) do
                  begin
                    if SearchColumn = 0 then
                    begin
                      for Index := 0 to Items.Count - 1 do
                        if Items[Index].Text = SearchText then
                        begin
                          R := Items[Index].DisplayRect(False);
                          if (R.Bottom > 0) and (R.Top < ClientHeight) then
                            Result := ComposeCoordsReply(R.Left, R.Top);
                          Break;
                        end;
                    end
                    else
                      Result := ComposeReplyLine(rtError, ['invalid column'])
                  end;
                end
                else
                  if Control is TCustomListview then
                    Result := SearchListview(Control, SearchColumn, SearchText)
                  else
                    Result := ComposeReplyLine(rtError, ['invalid column']);
        end;
      end;
    end
    else
      Result := ComposeReplyLine(rtError, ['control not found']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleMenuItems(const Parameters: TParameters): WideString;

// Handles the request for listing menu items in the given menu or sub menu.

var
  Component: TComponent;
  I: Integer;
  Index: Integer;
  Item: TMenuItem;
  Path: WideString;
  Flags: WideString;
  Parent: TMenuItem;
  ParentName: WideString;
  Menu: TMenu;
  R: TRect;
  Bounds: WideString;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Component := GetComponent(Parameters[0]);
    if Component = nil then
      Result := ComposeReplyLine(rtError, ['menu not found'])
    else
    begin
      Parent := nil;
      if Component is TMenu then
      begin
        Parent := TMenu(Component).Items;
        ParentName := GetComponentName(TMenu(Component));
      end
      else
        if Component is TMenuItem then
        begin
          Parent := TMenuItem(Component);
          ParentName := GetComponentName(Parent);
        end;

      if Assigned(Parent) then
      begin
        Result := '';
        Index := 0;
        for I := 0 to Parent.Count - 1 do
        begin
          Item := Parent[I];
          if not (IsExcluded(Item) or Item.IsLine) then
          begin
            Path := ParentName + '.' + GetComponentName(Item);
            Flags := '';
            if Item.AutoCheck then
              Flags := 'AUTOCHECK,';
            if Item.Checked then
              Flags := Flags + 'CHECKED,';
            if Item.Default then
              Flags := Flags + 'DEFAULT,';
            if Item.Enabled then
              Flags := Flags + 'ENABLED,';
            if Item.RadioItem then
              Flags := Flags + 'RADIOITEM,';
            if Item.Visible then
              Flags := Flags + 'VISIBLE,';

            if Flags = '' then
              Flags := 'NULL'
            else
              SetLength(Flags, Length(Flags) - 1); // Remove the trailing comma, which we don't need.

            // Determine bounds of the menu item.
            Menu := Item.GetParentMenu;
            GetMenuItemRect(Menu.WindowHandle, Parent.Handle, Index, R);
            Bounds := WideFormat('%d,%d,%d,%d', [R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top]);

            // Compose result string for the item.
            if Item is TTntMenuItem then
              Result := Result + ComposeReplyLine(rtNormal, [Item, Path, Item.ClassName, Flags, Pointer(Item.Handle),
               Bounds, TTntMenuItem(Item).Caption])
            else
              Result := Result + ComposeReplyLine(rtNormal, [Item, Path, Item.ClassName, Flags, Pointer(Item.Handle),
                Bounds, Item.Caption]);

            Inc(Index);
          end;
        end;
      end
      else
        Result := ComposeReplyLine(rtError, ['invalid menu']);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleMenus(const Parameters: TParameters): WideString;

// Handles the request for enumerating all top-level windows in the application.

var
  I: Integer;
  J: Integer;
  Form: TForm;
  Menu: TMenu;

begin
  Result := '';
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if not IsExcluded(Form) then
      for J := 0 to Form.ComponentCount - 1 do
      begin
        if Form.Components[J] is TMenu then
        begin
          Menu := Form.Components[J] as TMenu;
          if not IsExcluded(Menu) then
            Result := Result + ComposeReplyLine(rtNormal, [Menu, GetComponentName(Menu), Menu.ClassName]);
        end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleProps(const Parameters: TParameters): WideString;

// Handles the request to return properties for a particular control.

  //----------------------------------------------------------------------------

  procedure CollectProperties(const Obj: TPersistent; const Path: WideString);

  var
    PropertyList: TPropList;
    PropertyCount: Integer;
    S: WideString;
    Ident: string;
    O: TPersistent;
    I: Integer;
    J: Integer;

  begin
    PropertyCount := GetPropList(PTypeInfo(Obj.ClassInfo), [tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      tkSet, tkClass, tkWChar, tkLString, tkWString, tkInt64], @PropertyList);
    for I := 0 to PropertyCount - 1 do
    begin
      // We handle only type kinds here which we also requested for the property list above.
      O := nil;
      case PropertyList[I].PropType^.Kind of
        tkInteger:
          begin
            // Consider special properties Color and Cursor.
            if PropertyList[I].Name = 'Color' then
            begin
              J := GetPropValue(Obj, PropertyList[I]);
              ColorToIdent(J, Ident);
              S := Ident;
            end
            else
              if PropertyList[I].Name = 'Cursor'  then
              begin
                J := GetPropValue(Obj, PropertyList[I]);
                CursorToIdent(J, Ident);
                S := Ident;
              end
              else
                S := GetPropValue(Obj, PropertyList[I]);
          end;
        tkInt64,
        tkFloat,
        tkString,
        tkWChar,
        tkLString,
        tkWString,
        tkChar,
        tkEnumeration:
          S := GetPropValue(Obj, PropertyList[I]);
        tkSet:
          S := '[' + GetPropValue(Obj, PropertyList[I]) + ']';
        tkClass:
          begin
            O := Pointer(Integer(GetPropValue(Obj, PropertyList[I], False)));
            if Assigned(O) then
            begin
              if O is TComponent then
              begin
                S := GetComponentName(TComponent(O));
                O := nil;
              end
              else
                S := '(' + O.ClassName + ')';
            end
            else
              S := '(NULL)';
          end;
      end;
      Result := Result + ComposeReplyLine(rtNormal, [Path + PropertyList[I].Name, S]);
      if Assigned(O) then
      begin
        if Path = '' then
          S := PropertyList[I].Name + '.'
        else
          S := Path + PropertyList[I].Name + '.';
        CollectProperties(O, S);
      end;
    end;
  end;

  //----------------------------------------------------------------------------

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);

    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      try
        Result := '';
        CollectProperties(Control, '');
      except
        Result := ComposeReplyLine(rtError, ['control not found']);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleQuery(const Parameters: TParameters): WideString;

// Handles the request for locating a widget/control at a specific location (given in screen coordinates).

var
  Control: TWinControl;
  ChildControl: TControl;
  P: TPoint;
  Path: WideString;
  Runner: TWinControl;
  Window: HWnd;

begin
  if Length(Parameters) < 2 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    P.X := StrToIntDef(Parameters[0], Maxint);
    P.Y := StrToIntDef(Parameters[1], MaxInt);
    if (P.X = MaxInt) or (P.Y = MaxInt) then
      Result := ComposeReplyLine(rtError, ['invalid coordinates'])
    else
    begin
      Window := WindowFromPointEx(P);
      Control := FindControl(Window);
      if Control = nil then
        Result := ComposeReplyLine(rtNormal, [nil])
      else
      begin
        ChildControl := FindChildControl(Control, P);
        if ChildControl = nil then
          ChildControl := Control;

        Runner := ChildControl.Parent;
        Path := GetComponentName(ChildControl);
        while Assigned(Runner) do
        begin
          Path := GetComponentName(Runner) + '.' + Path;
          Runner := Runner.Parent;
        end;
        Result := ComposeReplyLine(rtNormal, [ChildControl, Path, ChildControl.ClassName]);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleQuit: WideString;

// Handles the request for terminating the application.

begin
  Result := '';
  PostThreadMessage(MainThreadID, WM_QUIT, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TRadioGroupAccess = class(TCustomRadioGroup);

function TSnoopModule.HandleRadiogroupSelected(const Parameters: TParameters): WideString;

// Handles the request for getting the selected index in a radio group or container control.

var
  Control: TControl;
  I: Integer;
  Index: Integer;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if (Control is TCustomRadioGroup) or (Control is TWinControl) then
      begin
        Result := '';
        if Control is TCustomRadioGroup then
        begin
          if TRadioGroupAccess(Control).ItemIndex > -1 then
            Result := ComposeReplyLine(rtNormal, [TRadioGroupAccess(Control).ItemIndex]);
        end
        else
        begin
          Index := -1;
          for I := 0 to TWinControl(Control).ControlCount - 1 do
            if (TWinControl(Control).Controls[I] is TRadioButton) then
            begin
              Inc(Index);
              if TRadioButton(TWinControl(Control).Controls[I]).Checked then
                Break;
            end;
          if Index > -1 then
            Result := ComposeReplyLine(rtNormal, [Index]);
        end;
      end
      else
        Result := ComposeReplyLine(rtError, ['invalid control']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TUpDownAccess = class(TCustomUpDown);

function TSnoopModule.HandleRangeinfo(const Parameters: TParameters): WideString;

// Handles the request to determine the check state of a single check box control.

var
  Control: TControl;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);
    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      if (Control is TTrackBar) or (Control is TProgressBar) or (Control is TCustomUpDown) then
      begin
        if Control is TTrackBar then
          with Control as TTrackBar do
          begin
            Result := ComposeReplyLine(rtNormal, ['min', Min]);
            Result := Result + ComposeReplyLine(rtNormal, ['max', Max]);
            Result := Result + ComposeReplyLine(rtNormal, ['pos', Position]);
            Result := Result + ComposeReplyLine(rtNormal, ['selstart', SelStart]);
            Result := Result + ComposeReplyLine(rtNormal, ['selend', Selend]);
          end
        else
          if Control is TCustomUpDown then
          begin
            with TUpDownAccess(Control) do
            begin
              Result := ComposeReplyLine(rtNormal, ['min', Min]);
              Result := Result + ComposeReplyLine(rtNormal, ['max', Max]);
              Result := Result + ComposeReplyLine(rtNormal, ['pos', Position]);
            end;
          end
          else
            with Control as TProgressBar do
            begin
              Result := ComposeReplyLine(rtNormal, ['min', Min]);
              Result := Result + ComposeReplyLine(rtNormal, ['max', Max]);
              Result := Result + ComposeReplyLine(rtNormal, ['pos', Position]);
            end;
      end
      else
        Result := ComposeReplyLine(rtError, ['invalid control']);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleTopLevels(const Parameters: TParameters): WideString;

// Handles the request for enumerating all top-level windows in the application.

var
  I: Integer;
  Form: TForm;

begin
  Result := '';
  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];

    // Only consider top level windows.
    if Form.Parent = nil then
    begin
      if Form is TTntForm then
        Result := Result + ComposeReplyLine(rtNormal, [Form, GetComponentName(Form), Form.ClassName, Visibility[Form.Visible],
          TTntForm(Form).Caption]) // TNT Forms have a Unicode caption
      else
        Result := Result + ComposeReplyLine(rtNormal, [Form, GetComponentName(Form), Form.ClassName, Visibility[Form.Visible],
          Form.Caption]);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleTree(const Parameters: TParameters): WideString;

// Handles the request for a list (tree) of child controls and their child controls.
// The first parameter either gives the name of the control whose child controls are to be returned or its address
// (indicated by a leading 0x character sequence).

  //----------------------------------------------------------------------------

  procedure CreateSubTree(Parent: TControl; Path: WideString);

  // Creates a response entry line ("TM:<adress>:<name>:<type>:<visibility>") for
  // all the child controls of Parent and their grandchildren,
  // provided it is a TWinControl descendant.

  var
    I: Integer;
    WinControl: TWinControl;
    Child: TControl;
    ChildPath: WideString;

  begin
    if Parent is TWinControl then
    begin
      WinControl := Parent as TWinControl;
      for I := 0 to WinControl.ControlCount - 1 do
      begin
        Child := WinControl.Controls[I];
        ChildPath := Path + '.' + GetComponentName(Child);

        Result := Result + ComposeReplyLine(rtNormal, [Child, ChildPath, Child.ClassName, Visibility[Child.Visible]]);
        CreateSubTree(Child, ChildPath);
      end;
    end;
  end;

  //----------------------------------------------------------------------------

var
  Control: TControl;
  ControlName: WideString;

begin
  if Length(Parameters) < 1 then
    Result := ComposeReplyLine(rtError, ['wrong parameter count'])
  else
  begin
    Control := GetControl(Parameters[0]);

    if Control = nil then
      Result := ComposeReplyLine(rtError, ['control not found'])
    else
      try
        ControlName := GetComponentName(Control);
        Result := ComposeReplyLine(rtNormal, [Control, ControlName, Control.ClassName, Visibility[Control.Visible]]);
        CreateSubTree(Control, ControlName);
      except
        Result := ComposeReplyLine(rtError, ['control not found']);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.HandleWaitidle: WideString;

// Handles the request for the wait command. The method simply processes all messages and then returns.

begin
  Application.ProcessMessages;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.SplitCommand(const Command: WideString; var Parameters: TParameters): WideString;

var
  Head: PWideChar;
  Token: WideString;

  //----------------------------------------------------------------------------

  procedure GetToken;

  // Gets the next token from the command. Leading spaces are skipped.
  // Current position is indicated by Head. If the token is a string
  // then it is unquoted before return.

  var
  Tail: PWideChar;
    S: WideString;
    QuoteChar: WideChar;
    IsNegative: Boolean;
    IsFloat: Boolean;
    IntNumber: Int64;
    FloatNumber: Double;

  begin
    Token := '';
    while UnicodeIsWhiteSpace(Word(Head^)) do
      Inc(Head);

    if Head^ <> WideNull then
    begin
      Tail := Head + 1;
      if UnicodeIsIdentifierStart(Word(Head^))  or (Head^ = '\') then
      begin
        // Ignore leading mask character if any.
        // Set back the tail for error checking below.
        if Head^ = '\' then
          Tail := Head;

        repeat
          while UnicodeIsIdentifierPart(Word(Tail^)) or (Tail^ = '.') do
            Inc(Tail);
          SetString(S, Head, Tail - Head);
          Token := Token + S;

          // Skip mask character if there was one.
          if Tail^ = '\' then
          begin
            Inc(Tail);

            // Check for invalid escape sequence.
            if Tail^ = WideNull then
            begin
              Token := '';
              Result := ComposeReplyLine(rtError, ['invalid escape sequence in command']);
              Break;
            end;
            Head := Tail;
            Inc(Tail);
          end
          else
            Break;
        until False;
      end
      else
      begin
        case Head^ of
          WideNull: // End of input.
            ;
          '"', '''': // Double and single quoted string.
            begin
              QuoteChar := Head^;
              Inc(Head);
              while not (Tail^ in [WideNull, QuoteChar]) do
                Inc(Tail);

              // Check for non-terminated string.
              if Tail^ <> QuoteChar then
              begin
                Token := '';
                Result := ComposeReplyLine(rtError, ['non-terminated string in command']);
              end;
              SetString(Token, Head, Tail - Head);
              Inc(Tail); // Skip closing quote char.
            end;
          '+', '-',
          '0'..'9': // Numbers, might be in hex code (C style), normal integer or float style.
            begin
              IsNegative := False;
              if Head^ in [WideChar('+'), WideChar('-')] then
              begin
                if Head^ = '-' then
                  IsNegative := True;
                Inc(Head);
                Inc(Tail);
              end;

              if (Head^ = '0') and (Tail^ in [WideChar('x'), WideChar('X')]) then
              begin
                // C style hex number. Skip prefix.
                Inc(Tail);
                Head := Tail;
                while Tail^ in [WideChar('0')..WideChar('9'), WideChar('a')..WideChar('f'),
                  WideChar('A')..WideChar('F')] do
                  Inc(Tail);
                SetString(S, Head, Tail - Head);
                IntNumber := StrToIntDef('$' + S, MaxInt);
                if IntNumber = MaxInt then
                begin
                  Token := '';
                  Result := ComposeReplyLine(rtError, ['invalid hex number']);
                end
                else
                begin
                  if IsNegative then
                    IntNumber := -IntNumber;
                  Token := IntToStr(IntNumber);
                end;
              end
              else
              begin
                // Integer or float number.
                IsFloat := False;
                while Tail^ in [WideChar('0')..WideChar('9'), WideChar('.'), WideChar('e'), WideChar('E')] do
                begin
                  if Tail^ in [WideChar('.'), WideChar('e'), WideChar('E')] then
                    IsFloat := True;
                  Inc(Tail);
                end;
                SetString(S, Head, Tail - Head);

                if IsFloat then
                begin
                  FloatNumber := StrToFloat(S);
                  if IsNegative then
                    FloatNumber := -FloatNumber;
                  Token := FloatToStr(FloatNumber);
                end
                else
                begin
                  IntNumber := StrToInt(S);
                  if IsNegative then
                    IntNumber := -IntNumber;
                  Token := IntToStr(IntNumber);
                end;
              end;
            end;
        else
          // Anything else we don't know.
          Result := ComposeReplyLine(rtError, ['invalid command', Command]);
        end;
      end;

      // Keep current parse position.
      Head := Tail; 
    end;
  end;

  //----------------------------------------------------------------------------

var
  Index: Integer;
  
begin
  Result := '';
  Parameters := nil;

  Head := PWideChar(Command);
  if Head^ = #0 then
    Result := ComposeReplyLine(rtError, ['empty command']);

  while Result = '' do
  begin
    GetToken;
    if Token = '' then
      Break;

    Index := Length(Parameters);
    SetLength(Parameters, Index + 1);
    Parameters[Index] := Token;
  end;

  // Delete all entries we found so far if there was an error.
  if Result <> '' then
    Parameters := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.StartupServer: Boolean;

// Initializes the TCP/IP server and opens the snoop port, if the server wasn't running already.
// Result is True if the server is created and active, otherwise False.

var
  S: string;
  Port: Integer;
  I: Integer;

begin
  if FServer = nil then
  begin
    // Set up a TCP/IP server which listens to commands from the Test Monkey.
    // The port to be used comes either from an environment variable or the command line.
    Port := StrToIntDef(GetEnvironmentVariable('MONKEY_SNOOP_PORT'), 0);

    // Search for the command line switch. It overrides the environment variable if given.
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if Pos('-snoop_port=', S) = 1 then
      begin
        Port := StrToIntDef(Copy(S, 13, 4), 0);
        Break;
      end;
    end;

    if Port > 0 then
    begin
      FServer := TIdTCPServer.Create;
      FServer.DefaultPort := Port;
      FServer.MaxConnections := 3;
      FServer.OnConnect := ServerConnect;
      FServer.OnExecute := ServerExecute;
      FServer.Active := True;
    end;
  end;

  Result := Assigned(FServer) and FServer.Active;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.StripCommand(const Parameters: TParameters): TParameters;

// Removes the first entry in the given parameter list, which is the command to execute.

begin
  Result := Copy(Parameters, 1, MaxInt);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSnoopModule.ShutdownServer: Boolean;

// Closes all server connections and destroys the TCP/IP server.
// Result is True if the server was running when this function was called, otherwise False.

begin
  Result := Assigned(FServer);
  FreeAndNil(FServer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Initialize;

// Initializes the module and must be called by the application after all forms are created.

var
  I: Integer;
  S: string;

begin
  // Search for the command line switch which indicates whether we want to start the snoop module or not.
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if Pos('-start_snoop', S) = 1 then
    begin
      SnoopActive := True;
      Break;
    end;
  end;

  if SnoopActive then
  begin
    // Initialize command list.
    Commands := TStringList.Create;
    Commands.AddObject('toplevels', Pointer(cmTopLevelWindows));
    Commands.AddObject('tree', Pointer(cmTree));
    Commands.AddObject('props', Pointer(cmProperties));
    Commands.AddObject('info', Pointer(cmInfo));
    Commands.AddObject('bounds', Pointer(cmBounds));
    Commands.AddObject('query', Pointer(cmQuery));
    Commands.AddObject('check', Pointer(cmCheck));
    Commands.AddObject('waitidle', Pointer(cmWait));
    Commands.AddObject('flash', Pointer(cmFlash));
    Commands.AddObject('childcount', Pointer(cmChildcount));
    Commands.AddObject('list.textpos', Pointer(cmListTextpos));
    Commands.AddObject('list.indexpos', Pointer(cmListIndexpos));
    Commands.AddObject('list.selected', Pointer(cmListSelected));
    Commands.AddObject('list.checkstate', Pointer(cmListCheckstate));
    Commands.AddObject('checkbox.checkstate', Pointer(cmCheckboxCheckstate));
    Commands.AddObject('radiogroup.selected', Pointer(cmRadiogroupSelected));
    Commands.AddObject('range.info', Pointer(cmRangeInfo));
    Commands.AddObject('datetime.info', Pointer(cmDatetimeInfo));
    Commands.AddObject('clickable', Pointer(cmClickable));
    Commands.AddObject('clipboard', Pointer(cmClipboard));
    Commands.AddObject('menus', Pointer(cmMenus));
    Commands.AddObject('menu.items', Pointer(cmMenuItems));
    Commands.AddObject('quit', Pointer(cmQuit));
    Commands.Sorted := True;

    DecimalSeparator := '.';

    SnoopModule := TSnoopModule.Create;
    if not SnoopModule.StartupServer then
    begin
      SnoopActive := False;
      FreeAndNil(SnoopModule);
      FreeAndNil(Commands);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Finalize;

begin
  if Assigned(SnoopModule) then
  begin
    FreeAndNil(SnoopModule);
    Commands.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  Initialize;
finalization
  Finalize;
end.
