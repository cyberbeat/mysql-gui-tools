unit MySQLResultSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Controls,
  TntExtCtrls, TntComCtrls, TntClasses, TntStdCtrls, SyncObjs,
  myx_public_interface, myx_util_public_interface, gnugettext, auxfuncs, MyxError,
  MySQLConnection, VirtualTrees, Graphics, StrUtils,
  TextSearch, UniCodeEditor;

type
  TMySQLRS = class;
  TExecuteQueryThread = class;

  IMySQLRSControlInterface = interface(IUnknown)
    ['{B46142F1-743F-4BB2-9A00-20C7D485D7AA}']
    procedure ClearValues;

    function GetMySQLRS: TMySQLRS;
    procedure SetMySQLRS(MySQLRS: TMySQLRS);
    function GetControl: TObject;

    procedure DoCurrentRowChanged;
    procedure DoEditStateChanged;
    procedure DoStatusCaptionChanged;
    procedure DoMessagesChanged;

    property MySQLRS: TMySQLRS read GetMySQLRS write SetMySQLRS;
    property Control: TObject read GetControl;
  end;

  TConfirmEvent = function(Sender: TObject; Msg: WideString): Boolean of object;
  TResultsetEvent = function(Sender: TObject): TMySQLRS of object;

  PCommandEntry = ^TCommandEntry;
  TCommandEntry = record
    SQL: WideString;
    Caret: TPoint;
    SelectionStart: TPoint;
    SelectionEnd: TPoint;
  end;

  // A list of commands with a scrollable pointer to a current command.
  TCommandList = class(TObject)
  private
    FCommands: TList;
    FCurrent: Integer;

    function GetCurrent: PCommandEntry;
  protected
    procedure AddEntry(const SQL: WideString; const Caret, BB, BE: TPoint);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Back: PCommandEntry;
    procedure Clear;
    function HasBack: Boolean;
    function HasNext: Boolean;
    function Next: PCommandEntry;
    function MakeTOC(const SQL: WideString; const Caret, BB, BE: TPoint): PCommandEntry;

    property Current: PCommandEntry read GetCurrent;
  end;

  TMySQLRS = class(TObject)
    constructor Create;
    destructor Destroy; override;

    procedure ConnectControl(Control: IMySQLRSControlInterface);
    procedure DisconnectControl(Control: IMySQLRSControlInterface);

    procedure ExecuteQuery(Sql: WideString = '');
    procedure Refresh;

    procedure FreeResultSet;

    procedure DoCurrentRowChanged(Sender: IMySQLRSControlInterface; CurrentRow: Integer);
    procedure DoDynamicParamsChanged;
    procedure DoEditStateChanged;
    procedure DoStatusCaptionChanged;

    procedure UpdateDynamicParams;

    procedure DoDisplaySearch(ShowReplacePage: Boolean = False);
    procedure GotoFirstRow;
    procedure GotoLastRow;
    procedure DoEdit;
    procedure DoApplyChanges;
    procedure DoDiscardChanges;

    procedure DoMessagesChanged;
    procedure AddRSMessage(Msg: WideString; MsgType: MYX_QUERY_ERROR_LEVEL;
      MsgNr: Integer);
    procedure ClearRSMessages;

    procedure ConnectRS(RS: TMySQLRS);
    procedure DisconnectRS(RS: TMySQLRS);
  private
    FConnectedControls: TInterfaceList;
    FConnectedResultsets: TList;

    FExecuteQueryThread: TExecuteQueryThread;

    FMySQLConn: TMySQLConn;
    FSQL: Widestring;

    FCommandList: TCommandList;

    FStatusCaption: WideString;

    FEdited: Boolean;
    FActive: Boolean;

    FParentRS: TMySQLRS;
    FCurrentRow: Integer;
    FOnParamChange: TNotifyEvent;

    FOnQueryExecute: TNotifyEvent;
    FOnQueryExecuted: TNotifyEvent;
    FOnQuerySuccess: TNotifyEvent;
    FOnQueryStopped: TNotifyEvent;
    FOnQueryError: TNotifyEvent;

    FOnConfirmDeletion: TConfirmEvent;

    FStopQuery: Boolean;
    FQueryExecuting: Boolean;

    FCreateNextRSForMultipleRSQuery: TResultsetEvent;

    FRemoveRSForMultipleRSQuery: TNotifyEvent;
    FShowRSForMultipleRSQuery: TNotifyEvent;

    FGlobalParams,
    FLocalParams,
    FDynamicParams: TTntStringList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
  protected
    function GetMySQLConn: TMySQLConn;
    procedure SetMySQLConn(MySQLConn: TMySQLConn);
    function GetActive: Boolean;
    procedure SetActive(Active: Boolean);
    procedure Open;
    procedure Close;
    function GetEdited: Boolean;
    procedure SetEdited(Edited: Boolean);
    function GetStatusCaption: WideString;
    procedure SetStatusCaption(StatusCaption: WideString);
    function GetParentRS: TMySQLRS;
    procedure SetParentRS(ParentRS: TMySQLRS);
    function GetRowCount: Integer;
    procedure CheckAttributes;
    function GetStopQuery: Boolean;
    procedure SetStopQuery(StopQuery: Boolean);
    function GetQueryExecuting: Boolean;
    procedure SetQueryExecuting(QueryExecuting: Boolean);
    function GetConnectedDetailRSCount: Integer;
    function GetConnectedDetailRS(I: Integer): TMySQLRS;
    function GetConnectedControlsCount: Integer;
    function GetConnectedControls(I: Integer): IMySQLRSControlInterface;
    function DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
      SearchOptions: TTextSearchOptions): Integer;
  public
    ResultsetLock: TCriticalSection;

    ResultSet: PMYX_RESULTSET;

    ErrorMessages: TObjectList;

    Editable,
    EditingAllowed,
    AtLeastOneRecord,
    FirstRowSelected,
    LastRowSelected: Boolean;
    ErrorOccured: Boolean;

    InsertedRows,
    DeletedRows: Integer;

    function AllowedToDiscard: Boolean;
    procedure ForceStop;

    property CommandList: TCommandList read FCommandList;
    property MySQLConn: TMySQLConn read GetMySQLConn write SetMySQLConn default nil;
    property Active: Boolean read GetActive write SetActive default False;
    property SQL: Widestring read FSQL write FSQL;
    property Edited: Boolean read GetEdited write SetEdited default False;
    property StatusCaption: WideString read GetStatusCaption write SetStatusCaption;
    property ParentRS: TMySQLRS read GetParentRS write SetParentRS;
    property RowCount: Integer read GetRowCount;

    property OnParamChange: TNotifyEvent read FOnParamChange write FOnParamChange;
    property OnQueryExecute: TNotifyEvent read FOnQueryExecute write FOnQueryExecute;
    property OnQueryExecuted: TNotifyEvent read FOnQueryExecuted write FOnQueryExecuted;
    property OnQuerySuccess: TNotifyEvent read FOnQuerySuccess write FOnQuerySuccess;
    property OnQueryStopped: TNotifyEvent read FOnQueryStopped write FOnQueryStopped;
    property OnQueryError: TNotifyEvent read FOnQueryError write FOnQueryError;

    property OnConfirmDeletion: TConfirmEvent read FOnConfirmDeletion write FOnConfirmDeletion;

    property StopQuery: Boolean read GetStopQuery write SetStopQuery default False;
    property QueryExecuting: Boolean read GetQueryExecuting write SetQueryExecuting default False;

    property ConnectedDetailRSCount: Integer read GetConnectedDetailRSCount;
    property ConnectedDetailRS[I: Integer]: TMySQLRS read GetConnectedDetailRS;

    property CreateNextRSForMultipleRSQuery: TResultsetEvent read FCreateNextRSForMultipleRSQuery write FCreateNextRSForMultipleRSQuery;
    property RemoveRSForMultipleRSQuery: TNotifyEvent read FRemoveRSForMultipleRSQuery write FRemoveRSForMultipleRSQuery;
    property ShowRSForMultipleRSQuery: TNotifyEvent read FShowRSForMultipleRSQuery write FShowRSForMultipleRSQuery;

    property ConnectedControlsCount: Integer read GetConnectedControlsCount;
    property ConnectedControls[I: Integer]: IMySQLRSControlInterface read GetConnectedControls;

    property GlobalParams: TTntStringList read FGlobalParams write FGlobalParams;
    property LocalParams: TTntStringList read FLocalParams write FLocalParams;
    property DynamicParams: TTntStringList read FDynamicParams write FDynamicParams;
  end;

  TExecuteQueryThread = class(TThread)
  private
    ThreadPMySQL: Pointer;
    FErrorMessage: WideString;
    FNeedCleanup: Boolean;

    FResultset: TMySQLRS;
    FQuery: WideString;
    FCurrentRowCount: Longint;
    FPreviousRowCount: Longint;
    FRawResultset: PMYX_RESULTSET;
    FAffectedRows: Int64;
  protected
    procedure CancelQuery;
    procedure Execute; override;
    procedure CreateNextRSForMultipleRSQuery;
    procedure RemoveRSForMultipleRSQuery;
    procedure ShowRSForMultipleRSQuery;
  public
    constructor Create(MySQLRS: TMySQLRS);
    destructor Destroy; override;

    procedure BuildGrid;
    procedure QueryExecuted;
    procedure FetchMySQLMessages;
    procedure QuerySuccess;
    procedure QueryStopped;
    procedure QueryError;
    procedure ShowErrorMessage;
  end;

  TRSError = class(TObject)
  private
    FMessage: WideString;
    FMessageType: MYX_QUERY_ERROR_LEVEL;
    FMessageNumber: Integer;
  public
    constructor Create(Msg: WideString; MsgType: MYX_QUERY_ERROR_LEVEL; MsgNr: Integer);

    property Msg: WideString read FMessage;
    property MsgNr: Integer read FMessageNumber;
    property MsgType: MYX_QUERY_ERROR_LEVEL read FMessageType;
  end;

  TRSFieldValue = record
    Value: PAnsiChar;
    Length: Integer;
    BinaryData: Boolean;
    IsNull: Boolean;
  end;

function progress_row_fetch(current_row_count: Longint; previous_row_count: Longint; result_set: PMYX_RESULTSET; user_data: Pointer):Integer; cdecl;
procedure resultset_realloc_before(user_data: Pointer); cdecl;
procedure resultset_realloc_after(user_data: Pointer); cdecl;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  MySQLResultSetControls, ApplicationDataModule,
  Forms, DateUtils;

type
  // A separate thread only to cancel queries.
  TCancelThread = class(TThread)
  private
    FSourceConnection: TMySQLConn;
    FTarget: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(Source: TMySQLConn; Target: Pointer); reintroduce;
  end;

  TMemoryStatusEx = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: Int64;
    ullAvailPhys: Int64;
    ullTotalPageFile: Int64;
    ullAvailPageFile: Int64;
    ullTotalVirtual: Int64;
    ullAvailVirtual: Int64;
    ullAvailExtendedVirtual: Int64;
  end;

//----------------------------------------------------------------------------------------------------------------------

procedure GlobalMemoryStatusEx(var lpBuffer: TMemoryStatusEx); stdcall; external 'Kernel32.dll' name 'GlobalMemoryStatusEx';

function GetMemoryLoad: Integer;

// Returns the current memory load of the system in % (0 .. 100).
var
  Status: TMemoryStatusEx;

begin
  ZeroMemory(@Status, SizeOf(Status));
  Status.dwLength := SizeOf(Status);
  GlobalMemoryStatusEx(Status);

  Result := Status.dwMemoryLoad;
end;

//----------------- TCancelThread --------------------------------------------------------------------------------------

constructor TCancelThread.Create(Source: TMySQLConn; Target: Pointer);

begin
  FSourceConnection := Source;
  FTarget := Target;

  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCancelThread.Execute;

// Used to cancel a running query. We clone the source connection and use this to kill the target query.
// If we cannot create a connection clone or the query cannot be killed then nothing further happens.

var
  NewConnection: Pointer;

begin
  if FSourceConnection.EmbeddedConnection then
    NewConnection := myx_mysql_embedded_init()
  else
    NewConnection := myx_mysql_init();

  if Assigned(NewConnection) and
    (myx_connect_to_instance(FSourceConnection.UserConnection.get_record_pointer, NewConnection) = 0) then
  begin
    myx_kill_query(NewConnection, FTarget);
    myx_mysql_close(NewConnection);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMySQLRS.Create;

begin
  inherited;

  ResultSet := nil;
  FMySQLConn := nil;

  FParentRS := nil;

  FConnectedControls := TInterfaceList.Create;
  FConnectedResultsets := TList.Create;

  FCommandList := TCommandList.Create;

  FCurrentRow := -1;

  InsertedRows := 0;
  DeletedRows := 0;

  FGlobalParams := nil;
  FLocalParams := TTntStringList.Create;
  FDynamicParams := TTntStringList.Create;

  ErrorMessages := TObjectList.Create;

  ErrorOccured := False;

  Editable := False;
  EditingAllowed := False;
  FEdited := False;

  FSynchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  FActive := False;
  StopQuery := False;

  FQueryExecuting := False;

  ResultsetLock := TCriticalSection.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLRS.Destroy;

var
  I: Integer;
  
begin
  // Disconnect from ParentRS
  ParentRS := nil;
  FreeResultSet;

  // Disconnect controls.
  for I := 0 to FConnectedControls.Count - 1 do
    IMySQLRSControlInterface(FConnectedControls[0]).MySQLRS := nil;

  ErrorMessages.Free;
  FCommandList.Free;

  FConnectedControls.Free;
  FConnectedResultsets.Free;

  FLocalParams.Free;
  FDynamicParams.Free;

  FSynchronizer.Free;

  ResultsetLock.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.ConnectControl(Control: IMySQLRSControlInterface);

begin
  if FConnectedControls.IndexOf(Control) = -1 then
    FConnectedControls.Add(Control);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DisconnectControl(Control: IMySQLRSControlInterface);

begin
  FConnectedControls.Remove(Control);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.ForceStop;

// Kills the current SQL execution thread if there is one. This is usually only necessary if the thread does not
// stop by regular means (e.g. hangs on the server).

var
  StartTime: TTime;

begin
  if FQueryExecuting and Assigned(FExecuteQueryThread) then
  begin
    FExecuteQueryThread.Terminate;
    StartTime := Now;
    Screen.Cursor := crHourGlass;
    try
      while FQueryExecuting and (SecondsBetween(StartTime, Now) < 2) do
      begin
        Sleep(100);
        Application.ProcessMessages;
      end;

      // If the query is still running kill the thread.
      if FQueryExecuting then
      begin
        TerminateThread(FExecuteQueryThread.Handle, 0);

        // Make sure visual states and all flags are reset to indicate the query is gone.
        FExecuteQueryThread.QueryError;
        FExecuteQueryThread.QueryExecuted;
        FreeAndNil(FExecuteQueryThread);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.FreeResultSet;

var
  I: Integer;
  
begin
  for I := 0 to FConnectedControls.Count-1 do
    IMySQLRSControlInterface(FConnectedControls[I]).ClearValues;

  if Assigned(ResultSet) then
    myx_query_free_resultset(ResultSet);

  InsertedRows := 0;
  DeletedRows := 0;

  ResultSet := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetMySQLConn: TMySQLConn;

begin
  Result := FMySQLConn;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetMySQLConn(MySQLConn: TMySQLConn);

begin
  FMySQLConn := MySQLConn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.AllowedToDiscard: Boolean;

begin
  Result := not FEdited or (ShowModalDialog(_('Unapplied Changes'),
    _('You have made changes to the resultset that have not been applied yet. Do you want to discard the changes?'),
    myx_mtConfirmation, _('Discard') + #13#10 + _('Abort')) = 1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.ExecuteQuery(Sql: WideString);

begin
  FSQL := Trim(Sql);
  Refresh;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.Refresh;

begin
  if FSQL = '' then
  begin
    ShowModalDialog(_('No SQL Command'),
      _('You tried to execute an empty string. Please type an SQL command into the SQL edit field and execute again.'),
      myx_mtError, _('Ok'));
    Exit;
  end;

  if not AllowedToDiscard  then
    Exit;

  if FMySQLConn.Ping <> 0 then
    FMySQLConn.Reconnect;

  Edited := False;

  FreeResultSet;

  ClearRSMessages;

  EditingAllowed := False;
  StopQuery := False;
  QueryExecuting := True;

  if(Assigned(FOnQueryExecute))then
    FOnQueryExecute(self);

  try
    FExecuteQueryThread := TExecuteQueryThread.Create(self);
    try
      // Thread will be freed after execution.
      FExecuteQueryThread.FreeOnTerminate := True;
      FExecuteQueryThread.Priority := tpIdle;

      FExecuteQueryThread.Resume;
    except
      FreeAndNil(FExecuteQueryThread);
    end;
  except
    on x: Exception do
    begin
      QueryExecuting := False;

      if Assigned(FOnQueryError) then
        FOnQueryError(self);
      if Assigned(FOnQueryExecuted) then
        FOnQueryExecuted(self);
      raise;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetActive(Active: Boolean);

begin
  if(Active)then
    Open
  else
    Close;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetActive: Boolean;

begin
  Result := FActive;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.Open;

begin
  FActive := True;

  ExecuteQuery;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.Close;

begin
  FreeResultSet;

  FActive := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetEdited: Boolean;

begin
  Result := FEdited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetEdited(Edited: Boolean);

begin
  if(FEdited<>Edited)then
  begin
    FEdited := Edited;

    DoEditStateChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.ConnectRS(RS: TMySQLRS);

begin
  if(FConnectedResultsets.IndexOf(RS)=-1)then
    FConnectedResultsets.Add(RS);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DisconnectRS(RS: TMySQLRS);

begin
  if(FConnectedResultsets.IndexOf(RS)<>-1)then
    FConnectedResultsets.Delete(FConnectedResultsets.IndexOf(RS));
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetParentRS: TMySQLRS;

begin
  Result := FParentRS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetParentRS(ParentRS: TMySQLRS);

begin
  if(FParentRS<>nil)and(FParentRS<>ParentRS)then
    FParentRS.DisconnectRS(self);

  FParentRS := ParentRS;

  if(FParentRS<>nil)then
    FParentRS.ConnectRS(self);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetRowCount: Integer;

begin
  if(ResultSet<>nil)then
    Result := ResultSet.rows_num+InsertedRows-DeletedRows
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoCurrentRowChanged(Sender: IMySQLRSControlInterface;
  CurrentRow: Integer);

var
  i: Integer;

begin
  CheckAttributes;

  FirstRowSelected := False;
  if(ResultSet<>nil)then
    if(CurrentRow=0)or((ResultSet.rows_num+InsertedRows=0)and(CurrentRow=1))then
      FirstRowSelected := True;

  LastRowSelected := False;
  if(ResultSet<>nil)then
    if((CurrentRow>=ResultSet.rows_num-1+InsertedRows)or
      (ResultSet.rows_num+InsertedRows=0))then
      LastRowSelected := True;

  FCurrentRow := CurrentRow;

  UpdateDynamicParams;

  for i := 0 to FConnectedControls.Count-1 do
    if(FConnectedControls[i]<>Sender)then
      IMySQLRSControlInterface(FConnectedControls[i]).DoCurrentRowChanged;

  for i := 0 to FConnectedResultsets.Count-1 do
    TMySQLRS(FConnectedResultsets[i]).DoDynamicParamsChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.UpdateDynamicParams;

var
  I: Integer;
  Row: PMYX_RS_ROW;
  col: PMYX_RS_COLUMN;
  Field: MYX_RS_FIELD;
  Column: WideString;
  Value: WideString;

begin
  //Set Dynamic Parameters
  FDynamicParams.Clear;
  if (FCurrentRow>-1) and
     (ResultSet<>nil) and
     (FCurrentRow<Integer(ResultSet.rows_num))then
  begin
    Row := PMYX_RS_ROW(Integer(ResultSet.rows)+sizeof(MYX_RS_ROW)*FCurrentRow);

    for I := 0 to ResultSet.columns_num-1 do
    begin
      col := PMYX_RS_COLUMN(Integer(ResultSet.columns)+(sizeof(MYX_RS_COLUMN)*I));
      Column := Copy(Utf8Decode(col.name), 1, 28);
      if Assigned(Row.fields) then
      begin
        Field := (PMYX_RS_FIELD(Integer(Row.fields) + (sizeof(MYX_RS_FIELD) * I)))^;

        // Only take first 128 chars.
        Value := Utf8Decode(Field.value);
        case col.column_type of
          MYX_RSCT_INTEGER,
          MYX_RSCT_FLOAT,
          MYX_RSCT_ENUM,
          MYX_RSCT_SET,
          MYX_RSCT_DECIMAL,
          MYX_RSCT_BIT,
          MYX_RSCT_TIMESTAMP,
          MYX_RSCT_YEAR,
          MYX_RSCT_NEWDATE,
          MYX_RSCT_NEWDECIMAL:
            FDynamicParams.Add(Column + '=' + Value);
          MYX_RSCT_STRING,
          MYX_RSCT_DATE,
          MYX_RSCT_TIME,
          MYX_RSCT_DATETIME,
          MYX_RSCT_BLOB,
          MYX_RSCT_TEXT:
            FDynamicParams.Add(Column + '=''' + Value + '''');
        end;
      end
      else
        FDynamicParams.Add(Column + '=');
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoDynamicParamsChanged;

begin
  if Assigned(ResultSet) then
  begin
    if Assigned(ResultSet.query) and (ResultSet.query.params_num > 0) and (FSQL <> '') and
      (FParentRS.FDynamicParams.Count > 0) then
    begin
      ExecuteQuery(FSQL);

      if(Assigned(FOnParamChange))then
        FOnParamChange(self);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoEditStateChanged;

var
  I: Integer;

begin
  for I := 0 to FConnectedControls.Count-1 do
    IMySQLRSControlInterface(FConnectedControls[I]).DoEditStateChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoStatusCaptionChanged;

var
  I: Integer;

begin
  for I := 0 to FConnectedControls.Count-1 do
    IMySQLRSControlInterface(FConnectedControls[I]).DoStatusCaptionChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetStatusCaption: WideString;

begin
  Result := FStatusCaption;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetStatusCaption(StatusCaption: WideString);

begin
  FStatusCaption := StatusCaption;

  DoStatusCaptionChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.CheckAttributes;

begin
  Editable := Assigned(ResultSet) and (ResultSet.editable <> 0);
  AtLeastOneRecord := Assigned(ResultSet) and (ResultSet.rows_num + InsertedRows > 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoDisplaySearch(ShowReplacePage: Boolean);

begin
  if TextSearchForm=nil then
    TextSearchForm := TTextSearchForm.Create(nil,
      ApplicationDM.QBOptions);

  if ShowReplacePage then
    TextSearchForm.PageControl.ActivePage := 
      TextSearchForm.ReplaceTabSheet;

  TextSearchForm.Show;
  TextSearchForm.OnSearch := DoSearch;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.DoSearch(Sender: TObject; SearchText: WideString; ReplaceText: WideString;
  SearchOptions: TTextSearchOptions): Integer;

var
  I: Integer;

begin
  Result := 0;

  for I := 0 to FConnectedControls.Count-1 do
    if IMySQLRSControlInterface(FConnectedControls[I]).Control is TMySQLRSGrid then
    begin
      Result := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).Control).DoSearch(Sender,
          SearchText, ReplaceText, SearchOptions);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.GotoFirstRow;

var
  I: Integer;
  RSGrid: TMySQLRSGrid;

begin
  for I := 0 to FConnectedControls.Count-1 do
  begin
    if((TObject(IMySQLRSControlInterface(FConnectedControls[I]).GetControl) is TMySQLRSGrid))then
    begin
      RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).GetControl);

      if(RSGrid.CanFocus)then
        RSGrid.SetFocus;

      RSGrid.FocusedNode := RSGrid.GetFirstNoInit;
      RSGrid.ClearSelection;
      RSGrid.Selected[RSGrid.FocusedNode] := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.GotoLastRow;

var
  I: Integer;
  RSGrid: TMySQLRSGrid;
  LastNode: PVirtualNode;

begin
  for I := 0 to FConnectedControls.Count-1 do
  begin
    if((TObject(IMySQLRSControlInterface(FConnectedControls[I]).GetControl) is TMySQLRSGrid))then
    begin
      RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).GetControl);

      if RSGrid.CanFocus then
        RSGrid.SetFocus;

      LastNode := RSGrid.GetLastNoInit;
      if Assigned(LastNode) then
        // If the result set is editable then an additional (empty) line is added to the RS grid.
        if Assigned(LastNode.PrevSibling) and RSGrid.MySQLRS.Editable then
          RSGrid.FocusedNode := LastNode.PrevSibling
        else
          RSGrid.FocusedNode := LastNode;
          
      RSGrid.ClearSelection;
      RSGrid.Selected[RSGrid.FocusedNode] := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoEdit;

begin
  if Editable then
  begin
    DoEditStateChanged;
    if not EditingAllowed then
    begin
      EditingAllowed := True;
    end
    else
    begin
      DoDiscardChanges;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoApplyChanges;

var
  errors: PMYX_RS_ACTION_ERRORS;
  error: PMYX_RS_ACTION_ERROR;
  I: Integer;
  RSGrid: TMySQLRSGrid;
  RSAction: PMYX_RS_ACTION;

begin
  ClearRSMessages;
  SetStatusCaption('');

  if FMySQLConn.Ping <> 0 then
    FMysQLConn.Reconnect(True);

  ResultsetLock.Acquire;
  try
    // Convert all failed actions to normal actions.
    if Assigned(ResultSet.actions) then
    begin
      for I := 0 to ResultSet.actions.actions_num-1 do
      begin
        RSAction := PMYX_RS_ACTION(Integer(ResultSet.actions.actions) + sizeof(MYX_RS_ACTION) * I);
        if RSAction.status = MYX_RSAS_FAILED then
          RSAction.status := MYX_RSAS_NEW;
      end;

      // Apply actions.
      errors := myx_query_apply_actions(ResultSet);
      if errors <> nil then
      begin
        SetStatusCaption(_('Error while applying actions.'));

        for I := 0 to errors.errors_num-1 do
        begin
          error := PMYX_RS_ACTION_ERROR(Integer(errors.errors) + sizeof(MYX_RS_ACTION_ERROR) * I);
          AddRSMessage(UTF8Decode(error.error_text), error.level, error.error);

          if error.error = 2006 then
            FMySQLConn.Connected := False;
        end;

        g_free(errors);
      end;

      // Delete Rows from Grids.
      for I := 0 to FConnectedControls.Count-1 do
        if((TObject(IMySQLRSControlInterface(FConnectedControls[I]).GetControl) is TMySQLRSGrid))then
        begin
          RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).GetControl);

          RSGrid.ApplyDeleteActionsToGrid;
        end;

      // Update resultset which will clear or re-generate a action list.
      if(myx_query_update_resultset(ResultSet)<>0)then
        SetStatusCaption(_('Error while applying changes to the result set.'));

      // Update rows that had actions.
      for I := 0 to FConnectedControls.Count-1 do
        if((TObject(IMySQLRSControlInterface(FConnectedControls[I]).GetControl) is TMySQLRSGrid))then
        begin
          RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).GetControl);
          RSGrid.SynchronizeActions;
        end;
    end;
  finally
    ResultsetLock.Release;
  end;

  Edited := False;
  DoEditStateChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoDiscardChanges;

var
  DoIt: Boolean;
  I: Integer;
  RSGrid: TMySQLRSGrid;
  
begin
  DoIt := True;

  if Assigned(FOnConfirmDeletion) and Edited then
    DoIt := FOnConfirmDeletion(Self, _('Do you really want to discard all changes?'));

  if DoIt then
  begin
    myx_query_discard_actions(ResultSet);

    for I := 0 to FConnectedControls.Count-1 do
      if (TObject(IMySQLRSControlInterface(FConnectedControls[I]).GetControl) is TMySQLRSGrid) then
      begin
        RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FConnectedControls[I]).GetControl);
        RSGrid.SynchronizeActions;
      end;

    Edited := False;
    EditingAllowed := False;

    DoEditStateChanged;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.DoMessagesChanged;

var
  I: Integer;

begin
  for I := 0 to FConnectedControls.Count-1 do
    IMySQLRSControlInterface(FConnectedControls[I]).DoMessagesChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.AddRSMessage(Msg: WideString; MsgType: MYX_QUERY_ERROR_LEVEL;
  MsgNr: Integer);

begin
  if(MsgType=MYX_QEL_ERROR)then
    ErrorOccured := True;

  ErrorMessages.Add(TRSError.Create(Msg, MsgType, MsgNr));

  DoMessagesChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.ClearRSMessages;

begin
  ErrorMessages.Clear;

  DoMessagesChanged;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetStopQuery: Boolean;

begin
  FSynchronizer.BeginRead;
  try
    Result := FStopQuery;
  finally
    FSynchronizer.EndRead;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetStopQuery(StopQuery: Boolean);

begin
  FSynchronizer.BeginWrite;
  try
    if FStopQuery <> StopQuery then
    begin
      FStopQuery := StopQuery;
      if FStopQuery then
        SetStatusCaption(_('Query is being stopped...'));
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetQueryExecuting: Boolean;

begin
  FSynchronizer.BeginRead;
  try
    Result := FQueryExecuting;
  finally
    FSynchronizer.EndRead;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLRS.SetQueryExecuting(QueryExecuting: Boolean);

begin
  FSynchronizer.BeginWrite;
  try
    if FQueryExecuting <> QueryExecuting then
    begin
      FQueryExecuting := QueryExecuting;
      if FQueryExecuting then
        SetStatusCaption(_('Query is being executed...'));
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetConnectedDetailRSCount: Integer;

begin
  Result := FConnectedResultsets.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetConnectedDetailRS(I: Integer): TMySQLRS;

begin
  if (I<FConnectedResultsets.Count) then
    Result := FConnectedResultsets[I]
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetConnectedControlsCount: Integer;

begin
  Result := FConnectedControls.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLRS.GetConnectedControls(I: Integer): IMySQLRSControlInterface;

begin
  if (I<FConnectedControls.Count) then
    Result := IMySQLRSControlInterface(FConnectedControls[I])
  else
    Result := nil;
end;

//----------------- TExecuteQueryThread --------------------------------------------------------------------------------

constructor TExecuteQueryThread.Create(MySQLRS: TMySQLRS);

begin
  inherited Create(True);

  FResultset := MySQLRS;
  FCurrentRowCount := 0;
  FPreviousRowCount := 0;
  FRawResultset := nil;
  FErrorMessage := '';

  // When the user is in the middle of a transaction or just starts one then don't create a new connection.
  if MySQLRS.MySQLConn.StartsTransaction(FResultset.FSQL) then
    MySQLRS.MySQLConn.InTransaction := True;
  FNeedCleanup := not MySQLRS.MySQLConn.InTransaction;
  if FNeedCleanup then
  begin
    if MySQLRS.MySQLConn.EmbeddedConnection then
      ThreadPMySQL := myx_mysql_embedded_init()
    else
      ThreadPMySQL := myx_mysql_init();

    if ThreadPMySQL = nil then
      raise EMyxError.Create(_('Error while allocating memory for MySQL Struct in Query Execute Thread.'));

    if myx_connect_to_instance(MySQLRS.MySQLConn.UserConnection.get_record_pointer, ThreadPMySQL) <> 0 then
      raise EMyxError.Create(_('Query Execute Thread cannot connect to MySQL'));
  end
  else
    ThreadPMySQL := MySQLRS.MySQLConn.MySQL;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TExecuteQueryThread.Destroy;

begin
  // Avoid any MySQL operation if the thread was stopped externally (which usually indicates a forced shutdown of
  // the thread, e.g. because a query hung on the server).
  if FNeedCleanUp and Assigned(FResultset) and Assigned(FResultset.MySQLConn) and not Terminated then
    myx_mysql_close(ThreadPMySQL);

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.CancelQuery;

var
  Thread: TCancelThread;

begin
  Thread := TCancelThread.Create(FResultset.MySQLConn, ThreadPMySQL);
  try
    Thread.WaitFor;
  finally
    Thread.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.Execute;

var
  ErrorCode: MYX_LIB_ERROR;
  Parameters: TMYX_STRINGLIST;
  Connection: TMySQLConn;
  InTransaction: Boolean;
  DoRemoveLastRS: Boolean;

begin
  DoRemoveLastRS := False;

  Connection := FResultset.MySQLConn;
  try
    try
      InTransaction := Connection.InTransaction;

      // If the user is in a transaction, wait till connection is free.
      if (InTransaction) then
        Connection.Lock.Acquire;
      try
        // Set target schema implicitly.
        myx_use_schema(ThreadPMySQL, Connection.UserConnection.schema);

        FQuery := FResultset.SQL;

        Parameters := TMYX_STRINGLIST.create;
        try
          Parameters.strings.Text := '';
          //Global Params
          if (FResultset.GlobalParams<>nil) then
            Parameters.strings.Text := Parameters.strings.Text+
              FResultset.GlobalParams.Text;
          //Local Params
          if (FResultset.LocalParams<>nil) then
            Parameters.strings.Text := Parameters.strings.Text+
              FResultset.LocalParams.Text;
          //Add dynamic parameters from parent
          if (FResultset.FParentRS<>nil) and
            (FResultset.FParentRS.DynamicParams<>nil) then
            Parameters.strings.Text := Parameters.strings.Text+#13#10+
              FResultset.FParentRS.DynamicParams.Text;

          FRawResultset := myx_query_execute(ThreadPMySQL, FQuery, Ord(ApplicationDM.QBOptions.EnforceEditableQueries),
            Parameters.get_record_pointer, @ErrorCode, Self, progress_row_fetch, resultset_realloc_before,
            resultset_realloc_after, @FAffectedRows);

          // Check if the thread was terminated in the mean time.
          if not Terminated then
          begin
            // Handle multiple resultsets.
            while True do
            begin
              // If this was executed using a cloned MySQL Connection, reassign the original one.
              if (Not(FResultset.MySQLConn.InTransaction)) and
                (FResultset.ResultSet<>nil) then
                FResultset.ResultSet.mysql := FResultset.MySQLConn.MySQL;

              //Display error/summary
              if (ErrorCode=MYX_NO_ERROR) then
                Synchronize(QuerySuccess)
              else
                if ErrorCode=MYX_STOP_EXECUTION then
                  Synchronize(QueryStopped)
                else
                  Synchronize(QueryError);

              //Check if there is a resultset left
              if Not( (FRawResultset<>nil) and (FRawResultset.has_more=1) ) then
                break;

              //Get new MySQLRS
              if Assigned(FResultset.CreateNextRSForMultipleRSQuery) then
              begin
                Synchronize(CreateNextRSForMultipleRSQuery);

                if FResultset<>nil then
                begin
                  //Reset thread values
                  FCurrentRowCount := 0;
                  FPreviousRowCount := 0;
                  FRawResultset := nil;
                  FErrorMessage := '';

                  //Fetch next resultset (use _ function so nil can be passed as query)
                  FRawResultset := _myx_query_execute(ThreadPMySQL, nil, 0, nil, @ErrorCode, Self, progress_row_fetch,
                    resultset_realloc_before, resultset_realloc_after, nil);

                  // Handle last result in a multi resultset query.
                  if (ErrorCode = MYX_SQL_ERROR) and (myx_mysql_errno(ThreadPMySQL) = 0) then
                  begin
                    DoRemoveLastRS := True;
                    Break;
                  end
                  else
                    Synchronize(ShowRSForMultipleRSQuery);
                end
                else
                  Break;
              end
              else
                Break;
            end;
          end;
        finally
          Parameters.Free;
        end;
      finally
        if InTransaction then
          Connection.Lock.Release;
      end;
    except
      Synchronize(QueryError);
      raise;
    end;
  finally
    Synchronize(QueryExecuted);

    if DoRemoveLastRS then
      Synchronize(RemoveRSForMultipleRSQuery);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.CreateNextRSForMultipleRSQuery;

begin
  if Assigned(FResultset.CreateNextRSForMultipleRSQuery) then
    FResultset := FResultset.CreateNextRSForMultipleRSQuery(FResultset)
  else
    FResultset := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.RemoveRSForMultipleRSQuery;

begin
  if Assigned(FResultset.RemoveRSForMultipleRSQuery) then
    FResultset.RemoveRSForMultipleRSQuery(FResultset);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.ShowErrorMessage;

begin
  ShowModalDialog(_('Problem while executing query'), FErrorMessage, myx_mtError);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.ShowRSForMultipleRSQuery;

begin
  if Assigned(FResultset.ShowRSForMultipleRSQuery) then
    FResultset.ShowRSForMultipleRSQuery(FResultset);
end;

//----------------------------------------------------------------------------------------------------------------------

function progress_row_fetch(current_row_count: Longint; previous_row_count: Longint; result_set: PMYX_RESULTSET;
  user_data: Pointer):Integer;

var
  Thread: TExecuteQueryThread;
  S: WideString;

begin
  Thread := user_data;
  if GetMemoryLoad > 95  then
  begin
    Thread.CancelQuery;
    S := _('The memory load of the system is extremly high.');
    if current_row_count >= 100000 then
      S := S + ' ' + _('This is likely because of the current result being very large.') + ' ';
    S := S + #10#10#13#10 + _('In order to keep the system responsive record set retrieval has been stopped.');
    Thread.FErrorMessage := S;
    Thread.Synchronize(Thread.ShowErrorMessage);
    Result := 1;
  end
  else
  begin
    try
      Thread.FCurrentRowCount := current_row_count;
      Thread.FPreviousRowCount := previous_row_count;
      Thread.FRawResultset := result_set;

      Thread.Synchronize(Thread.BuildGrid);

      Result := Ord(Thread.FResultset.StopQuery);
      if Thread.FResultset.StopQuery then
        Thread.CancelQuery;
    except
      on x: Exception do
      begin
        Thread.FErrorMessage := x.Message;
        Thread.Synchronize(Thread.QueryError);
        Result := Ord(True);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure resultset_realloc_before(user_data: Pointer);

var
  Thread: TExecuteQueryThread;

begin
  Thread := user_data;
  try
    Thread.FResultset.ResultsetLock.Acquire;
  except
    on x: Exception do
    begin
      Thread.FResultset.ResultsetLock.Release;

      Thread.FErrorMessage := x.Message;
      Thread.Synchronize(Thread.QueryError);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure resultset_realloc_after(user_data: Pointer);

var
  Tread: TExecuteQueryThread;
  
begin
  Tread := user_data;
  try
    Tread.FResultset.ResultsetLock.Release;
  except
    on x: Exception do
    begin
      Tread.FErrorMessage := x.Message;
      Tread.Synchronize(Tread.QueryError);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.BuildGrid;

var
  J: Integer;
  Control: TObject;
  RSGrid: TMySQLRSGrid;

begin
  if Assigned(FResultset) then
  begin
    if FPreviousRowCount = 0 then
      FResultset.ResultSet := FRawResultset;

    for J := 0 to FResultset.FConnectedControls.Count-1 do
    begin
      Control := IMySQLRSControlInterface(FResultset.FConnectedControls[J]).GetControl;
      if Control.InheritsFrom(TMySQLRSGrid) then
      begin
        RSGrid := Control as TMySQLRSGrid;

        // If this is the first block of rows, add columns.
        if FPreviousRowCount = 0 then
        begin
          RSGrid.MySQLRS.ResultSet := FRawResultset;

          RSGrid.BuildColumns;
          RSGrid.FocusedColumn := 1;
        end;

        // Add rows.
        RSGrid.BuildRows(FPreviousRowCount, FCurrentRowCount - 1, FPreviousRowCount = 0);
        FResultset.CheckAttributes;
        FResultset.StatusCaption := Format(_('%d rows fetched so far.'), [FCurrentRowCount]);
        RSGrid.FinishedBuilding;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.QueryExecuted;

begin
  if(Assigned(FResultset)) then
    if(Assigned(FResultset.OnQueryExecuted))then
      FResultset.OnQueryExecuted(FResultset);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.FetchMySQLMessages;

var
  PMsgs: PMYX_MYSQL_ERROR_MSGS;
  PMsg: PMYX_MYSQL_ERROR_MSG;
  I: Integer;

begin
  PMsgs := myx_mysql_error_msgs_fetch(ThreadPMySQL);

  if (PMsgs <> nil) then
  begin
    for I := 0 to PMsgs.errors_num - 1 do
    begin
      PMsg := PMYX_MYSQL_ERROR_MSG(Integer(PMsgs.errors) +
        sizeof(MYX_MYSQL_ERROR_MSG) * I);

      FResultset.AddRSMessage(UTF8Decode(PMsg.text), PMsg.level,
        PMsg.error);
    end;

    myx_mysql_error_msgs_free(PMsgs);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.QuerySuccess;

var
  j: Integer;
  RSGrid: TMySQLRSGrid;
  S: WideString;

begin
  if(FResultset.ResultSet<>nil)then
    for j := 0 to FResultset.FConnectedControls.Count-1 do
    begin
      if(TObject(IMySQLRSControlInterface(FResultset.FConnectedControls[j]).GetControl).InheritsFrom(TMySQLRSGrid))then
      begin
        RSGrid := TMySQLRSGrid(IMySQLRSControlInterface(FResultset.FConnectedControls[j]).GetControl);

        RSGrid.BuildRowForAppending;
      end;
    end;

  FResultset.CheckAttributes;

  //Check if this command has commited the open Trx (if there is any)
  FResultset.MySQLConn.CheckConnectionStatusChange(FQuery);

  if FCurrentRowCount <> 1 then
    S := 's'
  else
    S := '';

  if (FResultset.ResultSet<>nil)then
    FResultset.SetStatusCaption(
      Format(_('%d row%s fetched in %.4fs (%.4fs)'),
        [FCurrentRowCount, S,
        FResultset.ResultSet.fetch_time,
        FResultset.ResultSet.query_time]))
  else
  begin
    if FAffectedRows > 0 then
    begin
      if FAffectedRows = 1 then
        FResultset.SetStatusCaption(_('1 row affected by the last command, no resultset returned.'))
      else
        FResultset.SetStatusCaption(Format(_('%s rows affected by the last command, no resultset returned.'),
          [IntToStr(FAffectedRows)]))
    end
    else
      FResultset.SetStatusCaption(_('Query returned no resultset.'));
  end;

  FResultset.StopQuery := False;
  FResultset.QueryExecuting := False;

  //Check for errors and warnings
  FetchMySQLMessages;

  if(Assigned(FResultset.OnQuerySuccess))then
    FResultset.OnQuerySuccess(FResultset);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.QueryStopped;

begin
  FResultset.CheckAttributes;

  // Check if this command has commited the open Trx (if there is any).
  FResultset.MySQLConn.CheckConnectionStatusChange(FQuery);

  if FResultset.ResultSet<>nil then
    FResultset.SetStatusCaption(Format(_('Query aborted. %d rows fetched so far in %.4fs (%.4fs)'), [FCurrentRowCount,
      FResultset.ResultSet.fetch_time, FResultset.ResultSet.query_time]))
  else
    FResultset.SetStatusCaption(_('Query aborted.'));

  FResultset.StopQuery := False;
  FResultset.QueryExecuting := False;

  // Check for errors and warnings.
  FetchMySQLMessages;

  if(Assigned(FResultset.OnQueryStopped))then
    FResultset.OnQueryStopped(FResultset);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TExecuteQueryThread.QueryError;

begin
  FResultset.FreeResultSet;

  // Check if there actually was a MySQL Error.
  if myx_mysql_errno(ThreadPMySQL) > 0 then
  begin
    // Check if ExceptionMsg is set.
    if FErrorMessage<>'' then
      FResultset.SetStatusCaption(FErrorMessage)
    else
      FResultset.SetStatusCaption(_('The query could not be executed.'));
  end
  else
    FResultset.SetStatusCaption(_('No resultset returned.'));

  FResultset.StopQuery := False;
  FResultset.QueryExecuting := False;

  // Check for errors and warnings. However if the thread was terminated externally then skip that step
  // as it is likely that our connection does not work anymore.
  if not Terminated then
    FetchMySQLMessages;

  if(Assigned(FResultset.OnQueryError))then
    FResultset.OnQueryError(FResultset);
end;

//----------------- TRSError -------------------------------------------------------------------------------------------

constructor TRSError.Create(Msg: WideString; MsgType: MYX_QUERY_ERROR_LEVEL; MsgNr: Integer);

begin
  FMessage := Msg;
  FMessageType := MsgType;
  FMessageNumber := MsgNr;
end;

//----------------- TCommandList ---------------------------------------------------------------------------------------

constructor TCommandList.Create;

begin
  FCommands := TList.Create;
  FCurrent := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TCommandList.Destroy;

begin
  Clear;
  FCommands.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.GetCurrent: PCommandEntry;

// Returns the current command entry or nil if there is none.

begin
  Result := nil;
  if (FCurrent >= 0) and (FCurrent < FCommands.Count) then
    Result := FCommands[FCurrent];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCommandList.AddEntry(const SQL: WideString; const Caret, BB, BE: TPoint);

var
  Entry: PCommandEntry;
  I: Integer;
  
begin
  New(Entry);
  Entry.SQL := SQL;
  Entry.Caret := Caret;
  Entry.SelectionStart := BB;
  Entry.SelectionEnd := BE;

  // If the current entry is not at the end of the list then delete everything after it
  // making it so the end.
  for I := FCommands.Count - 1 downto FCurrent + 1 do
    Dispose(PCommandEntry(FCommands[I]));

  if FCurrent > -1 then
  begin
    FCommands.Count := FCurrent + 1;
    FCommands.Capacity := FCurrent + 1;
  end;
  FCurrent := FCommands.Add(Entry);
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.Back: PCommandEntry;

// Returns the previous entry in the list relative to the current position or nil
// we reached the start.

begin
  Result := nil;
  if FCurrent > 0 then
  begin
    Dec(FCurrent);
    Result := FCommands[FCurrent];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TCommandList.Clear;

var
  I: Integer;

begin
  for I := 0 to FCommands.Count - 1 do
    Dispose(PCommandEntry(FCommands[I]));
  FCommands.Clear;
  FCurrent := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.HasBack: Boolean;

// Determines if there is still a previous entry in the command list (relative to the current position).

begin
  Result := FCurrent > 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.HasNext: Boolean;

// Determines if there is still a next entry in the command list (relative to the current position).

begin
  Result := FCurrent < FCommands.Count - 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.Next: PCommandEntry;

// Returns the next entry in the list relative to the current position or nil if we reach the end.

begin
  Result := nil;
  if FCurrent < FCommands.Count - 1 then
  begin
    Inc(FCurrent);
    Result := FCommands[FCurrent];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCommandList.MakeTOC(const SQL: WideString; const Caret, BB, BE: TPoint): PCommandEntry;

// Creates a new entry with the given values and makes it the TOC (top of content), i.e., all
// entries after the current one (if there are any) are deleted before the new entry is added.
// There is a special case here: if the current entry does not differ to the given SQL then nothing happens.
// Returns the new (old) current command.

begin
  if (FCurrent = -1) or (Current.SQL <> SQL) then
    AddEntry(SQL, Caret, BB, BE);

  Result := Current;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
