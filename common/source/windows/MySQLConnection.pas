unit MySQLConnection;

// Copyright (C) 2003 - 2004 MySQL AB, 2008 Sun Microsystems, Inc.

interface

uses
  gnugettext, Windows, Messages, SysUtils, Classes, ComCtrls, ImgList,
  Controls, TntComCtrls, TntClasses, Forms, Contnrs,
  ExtCtrls, AuxFuncs, PNGImage, StdCtrls,
  SyncObjs, myx_util_public_interface, myx_public_interface, MyxError,
  Options, myx_sql_parser_public_interface,
  LexicalTools, StringContainers;

const
  WM_CONNECTION_LOST        = WM_USER + 101; // The currently connected server got lost. Reconnection might be possible.
  WM_CONNECTION_ESTABLISHED = WM_USER + 102; // A lost server connection could be established again.
  WM_CONNECTED              = WM_USER + 103; // A new server connection was created (new MySQL structure etc.).
  WM_DISCONNECTED           = WM_USER + 104; // A server connection was completely cut. No reconnection possible.
  WM_DefaultSchemaChanged   = WM_USER + 105;
  WM_SchemaListChanged      = WM_USER + 106;

  CR_SERVER_GONE_ERROR = 2006;
  CR_SERVER_LOST = 2013;
  CR_CONN_HOST_ERROR = 2003;

type
  TFetchDataThread = class;

  // Indicates what kind of data is being fetched currently.
  TFetchingDataKind = 
  (
    dkCatalogSchema,
    dkUserNames,
    dkProcessList,
    dkSchemaTables,
    dkStatusVars,
    djServerVars,
    dkUserData,
    dkSchemaTableStatus,
    dkTableAction,
    dkSchemaIndices,
    dkSchemaViewStatus,
    dkSchemaSPStatus,
    dkSchemaProcBody,
    dkSchemaProcDrop,
    dkSchemaViewDef,
    dkSchemaViewDrop
  );

  TFetchingDataKinds = set of TFetchingDataKind;
  
  TThreadExecMethod = procedure(Sender: TObject) of object;

  TConnectionLock = class(TCriticalSection)
  public
    function TryEnter: Boolean; virtual;
  end;

  IMySQLConnTransactionStatusChangeListener = interface
    procedure TransactionStatusChanged;
    procedure TransactionQueryExecuted(SQLCmd: WideString);
    procedure TransactionStarted;
  end;

  TConnectionState =
  (
    csConnected,
    csUnclear,
    csDisconnected
  );

  TMySQLConn = class(TObject)
  private
    FCurrentDataThread: TFetchDataThread;        // Set by each thread as soon as it starts working. Access is serialized
                                                 // via the critical section.
    FMySQL: Pointer;                             // Main MySQL server connection data.
    FDefaultSchema: WideString;
    FInTransaction: Boolean;
    FTransactionStatusChangeListeners: TList;
    FQuoteChar: WideString;
    FWorkList: TThreadList;
    FStatusBar: TTntStatusBar;
    FEmbeddedConnection: Boolean;
    FFetchDataLock: TConnectionLock;

    FMajorVersion,
    FMinorVersion: Integer;
    FUserConnection: TMYX_USER_CONNECTION;

    FParameters: TTntStringList;
    FDoParameterSubstitution: Boolean;

    FQueryHookMultiReadSync: TMultiReadExclusiveWriteSynchronizer;
    FQueryHookSQL: WideString;

    FDataBeingFetched: TFetchingDataKinds;

    FConnected: Boolean;
    FIsLocalServer: Boolean;

    FAllowConnectionSkip: Boolean;
    FKeepAlive: Boolean;

    FErrorCode: MYX_LIB_ERROR;
    FKeepAliveTimer: TTimer;
    FLastConnectionState: TConnectionState;

    procedure KeepAliveTrigger(Sender: TObject);
    procedure SetConnected(const Value: Boolean);
    procedure SetKeepAlive(const Value: Boolean);
  protected
    function ShowConnectToInstanceForm(var User_connection: TMYX_USER_CONNECTION; var MySQL: Pointer;
      MySQLConnectionsOnly: Boolean; SelectSchemata: Boolean; Create2: Boolean): Integer;
    function ConnectUsingUserConnection(Connection: WideString; var User_connection: TMYX_USER_CONNECTION;
      SelectSchemata: Boolean): Integer;
    function ConnectUsingCommandlineConnectionParams(var User_connection: TMYX_USER_CONNECTION; var FMySQL: Pointer;
      SelectSchemata: Boolean): Integer;
    function Connect(user_conn: TMYX_USER_CONNECTION; SelectSchemata: Boolean): Integer;

    function InternalStartsTransaction(Lexer: TSQLLexer): Boolean;
    function ExecuteDirect(SQLCmd: WideString; WaitTime: Integer; DisplayErrorDialog: Boolean; AllowMultiQueries: Boolean): Boolean; overload;

    procedure SetDefaultSchema(const Value: WideString);
    procedure SetInTransaction(InTransaction: Boolean);
    function GetQuoteChar: WideString;
    function GetFetchingData: Boolean;
  public
    constructor Create(StatusBar: TTntStatusBar);
    destructor Destroy; override;

    procedure ClearWorkList;
    procedure CheckConnectionStatusChange(SQLCmd: WideString);
    procedure TransactionQueryExecuted;
    function ConnectToEmbedded: Integer;
    function ConnectToServer(AutoConnect: Boolean; MySQLConnectionsOnly: Boolean = True; SelectSchemata:
      Boolean = False; Create2: Boolean = False): Integer;
    function Reconnect(RefreshOnly: Boolean = False): Boolean;
    procedure Disconnect(DoNotification: Boolean);
    procedure FetchData(KindOfData: TFetchingDataKind; FetchMethod: TThreadExecMethod; RefreshMethod: TThreadExecMethod;
      Target: TObject; StatusBarText: WideString; AllowParallelExecution: Boolean = False;
      AllowDuplicatedKindOfData: Boolean = False; ShowWaitCursor: Boolean = True; ControlThread: TThread = nil);
    procedure StopCurrentThread(Timeout: Integer);

    function ExecuteDirect(SQLCmd: WideString; WaitTime: Integer = 0; DisplayErrorDialog: Boolean = True): Boolean; overload;
    function ExecuteDirectMulti(SQLCmd: WideString; WaitTime: Integer = 0; DisplayErrorDialog: Boolean = True): Boolean;
    function ExecuteDirectQuery(SQLCmd: WideString; FetchMethod: TThreadExecMethod;
      WaitTime: Integer = 0; DisplayErrorDialog: Boolean = True): Boolean;

    procedure SchemaListChanged;

    procedure AddTransactionStatusListener(Listener: IMySQLConnTransactionStatusChangeListener);
    procedure RemoveTransactionStatusListener(Listener: IMySQLConnTransactionStatusChangeListener);

    function GetConnectionCaption(IncludeSchema: Boolean = True): WideString;
    function Ping: Integer;
    procedure UseStatusBar(StatusBar: TTntStatusBar);
    function StartsTransaction(const SQL: WideString): Boolean;

    property AllowConnectionSkip: Boolean read FAllowConnectionSkip write FAllowConnectionSkip default True;
    property Connected: Boolean read FConnected write SetConnected;
    property DataBeingFetched: TFetchingDataKinds read FDataBeingFetched;
    property DefaultSchema: WideString read FDefaultSchema write SetDefaultSchema;
    property EmbeddedConnection: Boolean read FEmbeddedConnection;
    property FetchingData: Boolean read GetFetchingData;
    property InTransaction: Boolean read FInTransaction write SetInTransaction;
    property IsLocalServer: Boolean read FIsLocalServer write FIsLocalServer;
    property KeepAlive: Boolean read FKeepAlive write SetKeepAlive;
    property Lock: TConnectionLock read FFetchDataLock;
    property MajorVersion: Integer read FMajorVersion;
    property MinorVersion: Integer read FMinorVersion;
    property MySQL: Pointer read FMySQL;
    property QuoteChar: WideString read GetQuoteChar;
    property UserConnection: TMYX_USER_CONNECTION read FUserConnection;
  end;

  TFetchDataThread = class(TThread)
  private
    FConnection: TMySQLConn;
    FErrorMessage: WideString;
    FFetchMethod: TThreadExecMethod;
    FRefreshMethod: TThreadExecMethod;
    FSynchronizedMethod: TThreadExecMethod;
    FKindOfData: TFetchingDataKind;
    FTarget: TObject;
    FStatusBarText: WideString;
    FAllowParallelExecution: Boolean;
    FShowWaitCursor: Boolean;
  protected
    procedure Execute; override;
    procedure SetStatusBar(StatusBarText: WideString);
  public
    constructor Create(AMySQLConn: TMySQLConn; AKindOfData: TFetchingDataKind; AFetchMethod: TThreadExecMethod;
      ARefreshMethod: TThreadExecMethod; ATarget: TObject; AStatusBarText: WideString; AAllowParallelExecution,
      ShowWaitCursor: Boolean);
    procedure UpdateStatusBar;
    procedure ShowError;
    procedure ExecuteSynchronized(SynchronizedMethod: TThreadExecMethod);
    procedure ExecuteSynchronizedMethod;

    property Connection: TMySQLConn read FConnection;
    property Target: TObject read FTarget;
    property StatusBarText: WideString write SetStatusBar;
  end;

function ExecuteMultiCmdSQL(sql: PChar; user_data: Pointer): Integer cdecl;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ConnectToInstance;

var
  DataThreadCount: Integer;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TMySQLConn.Create(StatusBar: TTntStatusBar);

begin
  inherited Create;

  FStatusBar := StatusBar;

  FWorkList := TThreadList.Create;
  FMySQL := nil;
  FUserConnection := nil;
  FTransactionStatusChangeListeners := nil;

  FDefaultSchema := '';

  FConnected := False;
  FIsLocalServer := False;

  FEmbeddedConnection := False;

  //Initialize components for thread handling
  FFetchDataLock := TConnectionLock.Create;
  FDataBeingFetched := [];

  FParameters := TTntStringList.Create;
  FDoParameterSubstitution := False;

  FQueryHookMultiReadSync := TMultiReadExclusiveWriteSynchronizer.Create;

  FAllowConnectionSkip := True;
  FKeepAlive := True;
  FKeepAliveTimer := TTimer.Create(nil);
  with FKeepAliveTimer do
  begin
    OnTimer := KeepAliveTrigger;
    Interval := 5000;
    Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLConn.Destroy;

begin
  FKeepAliveTimer.Free;
  ClearWorkList;
  FWorkList.Free;
  FFetchDataLock.Free;
  FQueryHookMultiReadSync.Free;
  FUserConnection.Free;
  FParameters.Free;

  // Close connection and free memory for MySQL struct
  if Assigned(FMySQL) then
    myx_mysql_close(FMySQL);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.ClearWorkList;

// Goes through the work list and tells all waiting threads to terminate. Before that however the current thread must
// be stopped. It is given a timeout of 2 seconds to finish its work otherwise it is killed.

var
  I: Integer;
  CurrentThread: TFetchDataThread;

begin
  CurrentThread := nil;
  with FWorkList.LockList do
  try
    for I := 0 to Count - 1 do
      TFetchDataThread(Items[I]).Terminate;

    // Now that all threads are marked to terminate they will not start any work but immediately return
    // as soon as the fetch lock is free.
    // Now finish the currently active thread if there is one.
    // Keep the lock on the work list so the current thread does not interfere in our manipulation here.
    if Assigned(FCurrentDataThread) then
    begin
      FCurrentDataThread.FreeOnTerminate := False;
      FCurrentDataThread.Terminate;
      CurrentThread := FCurrentDataThread;
    end;

    // Finally empty work list. The dangling threads will do nothing, just awake and finish their
    // thread proc without calling application code, so we don't need to care any further.
    Clear;
  finally
    FWorkList.UnlockList;
  end;

  if Assigned(CurrentThread) then
  begin
    // Wait 2 seconds and kill the thread if it did not stop working within that time.
    if WaitForSingleObject(CurrentThread.Handle, 2000) = WAIT_TIMEOUT then
    begin
      TerminateThread(CurrentThread.Handle, 999);

      // Restore default cursor and adjust current connection state.
      // The thread had no opportunity to do it.
      if not CurrentThread.FAllowParallelExecution then
        FFetchDataLock.Release;
      InterlockedDecrement(DataThreadCount);
      if DataThreadCount = 0 then
        Screen.Cursor := crDefault;
      FCurrentDataThread := nil;
    end;
    FreeAndNil(CurrentThread);
  end;

  FDataBeingFetched := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ConnectToServer(AutoConnect: Boolean; MySQLConnectionsOnly: Boolean = True;
  SelectSchemata: Boolean = False; Create2: Boolean = False): Integer;

begin
  // If AutoConnect and a connection was specified
  if (AutoConnect) and (MYXCommonOptions.ConnectionToUse <> '') then
    Result := ConnectUsingUserConnection(MYXCommonOptions.ConnectionToUse, FUserConnection, SelectSchemata)
  else
    // If AutoConnect and username was specified
    if (AutoConnect) and (MYXCommonOptions.ConnectionUsername <> '') then
      Result := ConnectUsingCommandlineConnectionParams(FUserConnection, FMySQL, SelectSchemata)
    else
      Result := ShowConnectToInstanceForm(FUserConnection, FMySQL, MySQLConnectionsOnly, SelectSchemata, Create2);

  if Result = 1 then
  begin
    FMajorVersion := myx_get_mysql_major_version(FMySQL);
    FMinorVersion := myx_get_mysql_minor_version(FMySQL);

    FConnected := True;
    FIsLocalServer := myx_is_localhost(FUserConnection.hostname) = 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ShowConnectToInstanceForm(var User_connection: TMYX_USER_CONNECTION; var MySQL: Pointer;
  MySQLConnectionsOnly: Boolean; SelectSchemata: Boolean; Create2: Boolean): Integer;

var
  ConnectToInstanceForm: TConnectToInstanceForm;
  ModalRes: Integer;

begin
  ConnectToInstanceForm := TConnectToInstanceForm.Create(nil, MySQLConnectionsOnly, SelectSchemata);

  try
    ConnectToInstanceForm.AllowConnectionSkip := FAllowConnectionSkip;
    ModalRes := ConnectToInstanceForm.ShowModal;
    if ModalRes = mrOK then
    begin
      // Terminate old connection if still connected.
      Disconnect(False);

      // Initialize Variables.
      User_connection := TMYX_USER_CONNECTION.create(ConnectToInstanceForm.User_Connection.get_record_pointer);
      MySQL := ConnectToInstanceForm.PMySQL;
      DefaultSchema := myx_get_default_schema(FMySQL);

      Result := 1;
    end
    else
      if (ModalRes = mrAbort) then
        Result := -1
      else
        Result := 0;

  finally
    ConnectToInstanceForm.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ConnectUsingUserConnection(Connection: WideString; var User_connection: TMYX_USER_CONNECTION;
  SelectSchemata: Boolean): Integer;

var
  user_conns: PMYX_USER_CONNECTIONS;
  stored_conns: TMYX_USER_CONNECTIONS;
  user_conn: TMYX_USER_CONNECTION;
  error: MYX_LIB_ERROR;
  i: Integer;

begin
  Result := -1;

  //Get connection
  //Fetch connections from library
  user_conns := myx_load_user_connections(
    MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml', @error);
  if (error <> MYX_NO_ERROR) then
    raise EMyxCommonLibraryError.Create(_('Error while loading stored connections.'),
      Ord(error), MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml');

  try
    stored_conns := TMYX_USER_CONNECTIONS.create(user_conns);
    try
      user_conn := nil;
      for i := 0 to stored_conns.user_connections.Count - 1 do
      begin
        if (CompareText(stored_conns.user_connections[i].connection_name,
          MYXCommonOptions.ConnectionToUse) = 0) then
        begin
          user_conn := stored_conns.user_connections[i];
          break;
        end;
      end;

      //If the connection is found, connect
      if (user_conn <> nil) then
      begin
        //Copy connection
        User_connection := TMYX_USER_CONNECTION.Create(user_conn.get_record_pointer);

        Result := Connect(User_connection, SelectSchemata);
      end
      else
        ShowModalDialog(Application.Title + ' - ' + _('Connection not found'),
          Format(_('The connection %s cannot be found in the list of stored connections.'),
          [MYXCommonOptions.ConnectionToUse]), myx_mtError, _('OK'));
    finally
      stored_conns.Free;
    end;
  finally
    myx_free_user_connections(user_conns);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ConnectUsingCommandlineConnectionParams(var User_connection: TMYX_USER_CONNECTION;
  var FMySQL: Pointer; SelectSchemata: Boolean): Integer;
  
begin
  //Create new connection form command line parameters
  User_connection := TMYX_USER_CONNECTION.Create(
    '',
    MYXCommonOptions.ConnectionUsername,
    MYXCommonOptions.ConnectionPassword,
    MYXCommonOptions.ConnectionHost,
    MYXCommonOptions.ConnectionPort,
    MYXCommonOptions.ConnectionSchema,
    '', '', MYX_MYSQL_CONN, MYX_FAVORITE_USER_CONNECTION);

  Result := Connect(User_connection, SelectSchemata);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.Connect(user_conn: TMYX_USER_CONNECTION; SelectSchemata: Boolean): Integer;

var
  ConnectionResult: Integer;

begin
  FMySQL := myx_mysql_init();
  if FMySQL = nil then
  begin
    ShowModalDialog(Application.Title + ' ' + _('Error'), _('Error while allocating memory for MySQL Struct.'),
      myx_mtError, 'OK');

    Result := -1;
    Exit;
  end;

  ConnectionResult := myx_connect_to_instance(user_conn.get_record_pointer, FMySQL);
  if ConnectionResult = 0 then
  begin
    if SelectSchemata and (user_conn.schema <> '') then
    begin
      ConnectionResult := myx_use_schema(FMySQL, user_conn.schema);
      if ConnectionResult <> 0 then
      begin
        ShowModalDialog(Application.Title + ' ' + _('Error'), Format(_('The schema %s cannot be selected.'),
          [user_conn.schema]), myx_mtError, 'OK');
        Result := -1;
        Exit;
      end;

      DefaultSchema := myx_get_default_schema(FMySQL);
    end;

    Result := 1;
  end
  else
  begin
    ShowModalDialog(Application.Title + ' ' + _('Error'),
      _('Could not connect to the specified host.') + ' ' + #13#10 + #13#10 +
      Format(_('MySQL Error Number %d' + #13#10 + '%s'), [myx_mysql_errno(FMySQL), myx_mysql_error(FMySQL)]),
      myx_mtError, 'OK');

    Result := -1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.GetConnectionCaption(IncludeSchema: Boolean): WideString;

begin
  if (FUserConnection <> nil) then
  begin
    if (FUserConnection.username = '') and (FUserConnection.hostname = '') and FConnected then
      Result := 'Embedded'
    else
    begin
      if FUserConnection.storage_type = MYX_FAVORITE_USER_CONNECTION then
        Result := 'Connection: ' + FUserConnection.connection_name
      else
        Result := 'Connection: ' + FUserConnection.username + '@' + FUserConnection.hostname + ':' + IntToStr(FUserConnection.port);
      if (FDefaultSchema <> '') and (IncludeSchema) then
        Result := Result + ' / ' + FDefaultSchema;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

type
  TCastThread = class(TThread);
  
procedure TMySQLConn.FetchData(KindOfData: TFetchingDataKind; FetchMethod: TThreadExecMethod;
  RefreshMethod: TThreadExecMethod; Target: TObject; StatusBarText: WideString; AllowParallelExecution:
  Boolean; AllowDuplicatedKindOfData, ShowWaitCursor: Boolean; ControlThread: TThread);

// This method starts retrieval of data by creating a separate thread and using the provided fetch and refresh methods.
// KindOfData determines what must be retrieved and is used in conjunction with AllowDuplicateKindOfData to allow or
// deny certain data retrieval actions.
// ControlThread has a special meaning as it is used to cancel long lasting operations. If it is assigned this method
// waits for the background to return unless the control thread gets terminated. In this case the data thread
// is killed and this method immediately returns to the caller.

var
  WaitResult: Cardinal;
  Thread: TFetchDataThread;
  
begin
  if not (KindOfData in FDataBeingFetched) or AllowDuplicatedKindOfData then
  begin
    //Add KindOfData to DataBeingFetched Set
    if not (KindOfData in FDataBeingFetched) then
      Include(FDataBeingFetched, KindOfData);
    try
      // Create a new thread.
      Thread := TFetchDataThread.Create(Self, KindOfData, FetchMethod, RefreshMethod, Target, StatusBarText,
        AllowParallelExecution, ShowWaitCursor);
      FWorkList.Add(Thread);
      try
        // Thread will be freed after execution if no control thread is given.
        if ControlThread = nil then
          Thread.FreeOnTerminate := True;

        Thread.Resume;

        if Assigned(ControlThread) then
        begin
          // Wait for the data thread to finish but don't deadlock. Instead poll the thread until it is finished
          // or the control thread is terminated.
          WaitResult := WAIT_OBJECT_0;
          while not TCastThread(ControlThread).Terminated do
          begin
            WaitResult := WaitForSingleObject(Thread.Handle, 100);
            if WaitResult = WAIT_OBJECT_0 then
              Break;
          end;
          // Kill the fetch thread if the control thread has been told to finish but the fetch thread is still working.
          if WaitResult = WAIT_TIMEOUT then
          begin
            TerminateThread(Thread.Handle, 999);

            // Close the connection if we have to kill the last query.
            Reconnect;
           end;
          Thread.Free;
        end;
      except
        FWorkList.Remove(Thread);
        Thread.Free;

        raise;
      end;
    except
      FDataBeingFetched := FDataBeingFetched - [KindOfData];

      raise;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.StartsTransaction(const SQL: WideString): Boolean;

// Checks if the given SQL string contains a START TRANSACTION command.
// Actually this function only sets up the Lexer. The actual check is done in an internal function
// which plays nicely with other parsing stuff here.

var
  Input: TWideStringStream;
  Lexer: TSQLLexer;

begin
  Input := TWideStringStream.Create(SQL);
  Lexer := TSQLLexer.Create(Input);
  try
    Result := InternalStartsTransaction(Lexer);
  finally
    Lexer.Free;
    Input.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.StopCurrentThread(Timeout: Integer);

var
  CurrentThread: TFetchDataThread;
  
begin
  CurrentThread := nil;
  with FWorkList.LockList do
  try
    if Assigned(FCurrentDataThread) then
    begin
      FCurrentDataThread.FreeOnTerminate := False;
      FCurrentDataThread.Terminate;
      CurrentThread := FCurrentDataThread;
    end;
  finally
    FWorkList.UnlockList;
  end;

  if Assigned(CurrentThread) then
  begin
    if WaitForSingleObject(CurrentThread.Handle, Timeout) = WAIT_TIMEOUT then
    begin
      TerminateThread(CurrentThread.Handle, 999);

      // Restore default cursor and adjust current connection state.
      // The thread had no opportunity to do it.
      FDataBeingFetched := FDataBeingFetched - [CurrentThread.FKindOfData];
      if not CurrentThread.FAllowParallelExecution then
        FFetchDataLock.Release;
      InterlockedDecrement(DataThreadCount);
      if DataThreadCount = 0 then
        Screen.Cursor := crDefault;
      FCurrentDataThread := nil;
    end;
    FreeAndNil(CurrentThread);

    if Assigned(FStatusBar) then
    begin
      FStatusBar.Panels[1].Text := '';
      FStatusBar.Invalidate;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.Reconnect(RefreshOnly: Boolean): Boolean;

var
  ConnectionResult: Integer;

begin
  Result := False;

  if Assigned(FUserConnection) then
  begin
    if not RefreshOnly then
    begin
      if Assigned(FMySQL) then
        Disconnect(False);

      if FEmbeddedConnection then
        FMySQL := myx_mysql_embedded_init()
      else
        FMySQL := myx_mysql_init();
      if FMySQL = nil then
        raise EMyxError.Create('Error while allocating memory for MySQL Struct.');
    end;

    ConnectionResult := myx_connect_to_instance(FUserConnection.get_record_pointer, FMySQL);
    if ConnectionResult = 0 then
    begin
      FConnected := True;
      MessageToAllForms(WM_CONNECTED, 0, 0);
      Result := True;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.Disconnect(DoNotification: Boolean);

begin
  if FConnected then
  begin
    FConnected := False;

    // Close connection and free memory for MySQL struct.
    myx_mysql_close(FMySQL);
    FMySQL := nil;

    if DoNotification then
      MessageToAllForms(WM_DISCONNECTED, 0, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.CheckConnectionStatusChange(SQLCmd: WideString);

var
  FoundTransactionCommand: Boolean;
  Lexer: TSQLLexer;
  Input: TWideStringStream;
  Token: WideString;

begin
  // Catch transaction status change.
  Input := TWideStringStream.Create(SQLCmd);
  Lexer := TSQLLexer.Create(Input);
  try
    FoundTransactionCommand := InternalStartsTransaction(Lexer);

    if FoundTransactionCommand then
      InTransaction := True
    else
    begin
      // Start over examining input.
      Lexer.Reset;
      if Lexer.NextToken = toSymbol then
      begin
        if Lexer.TokenSymbolIs('commit') then
        begin
          InTransaction := False;
          FoundTransactionCommand := True;
        end
        else
          if Lexer.TokenSymbolIs('rollback') then
          begin
            // Full rollback or only to a named savepoint?
            // The latter does not stop the current transaction.
            Token := Lexer.NextToken;
            if Token = toSymbol then
            begin
              // Skip optional token.
              if Lexer.TokenSymbolIs('work') then
                Token := Lexer.NextToken;
              if not Lexer.TokenSymbolIs('to') then
              begin
                InTransaction := False;
                FoundTransactionCommand := True;
              end;
            end
            else
            begin
              InTransaction := False;
              FoundTransactionCommand := True;
            end;
          end;
      end;
    end;

    // When in transaction, check if the executed command auto-commits the Trx
    if not FoundTransactionCommand and InTransaction then
      InTransaction := (myx_check_whether_commits_transaction(FMySQL, SQLCmd) = 0);
  finally
    Lexer.Free;
    Input.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ExecuteMultiCmdSQL(sql: PChar; user_data: Pointer): Integer;

var
  Conn: TMySQLConn;
  ErrorCode: MYX_LIB_ERROR;
  AffectedRows: Int64;

begin
  Conn := TMySQLConn(user_data);

  myx_query_execute_direct(Conn.FMySQL, Utf8Decode(sql), @ErrorCode, @AffectedRows);

  if (Conn.FErrorCode = MYX_NO_ERROR) and (ErrorCode <> MYX_NO_ERROR) then
    Conn.FErrorCode := ErrorCode;

  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ExecuteDirect(SQLCmd: WideString; WaitTime: Integer; DisplayErrorDialog: Boolean): Boolean;

begin
  Result := ExecuteDirect(SQLCmd, WaitTime, DisplayErrorDialog, False);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ExecuteDirectMulti(SQLCmd: WideString; WaitTime: Integer; DisplayErrorDialog: Boolean): Boolean;

begin
  Result := ExecuteDirect(SQLCmd, WaitTime, DisplayErrorDialog, True);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ExecuteDirect(SQLCmd: WideString; WaitTime: Integer; DisplayErrorDialog: Boolean; AllowMultiQueries: Boolean): Boolean;

// Directly executes a query. This method might run in parallel to an already running fetch thread.
// So make sure we wait while the other thread is running but don't block totally if the caller thread is the main thread.
// Note: This method is not reentrant! You cannot call it while it is already in progress.

var
  MySQLErrorNr: Integer;
  AffectedRows: Int64;

begin

  Result := False;
  
  if MainThreadID = GetCurrentThreadId then
  begin
    // Called by the main thread so check if there is already a data fetch in progress.
    // Wait for it a limited amount of time.
    repeat
      // Try to get the lock. If that succeeds continue normally.
      if FFetchDataLock.TryEnter then
      begin
        WaitTime := 10; // Give it a value > 0 to avoid the exit call below if the caller did not give a wait time.
        Break;
      end;

      // If we could not get the lock then handle all pending messages (e.g. to finish synchronized calls).
      Application.ProcessMessages;

      // Do this every 100 ms...
      Sleep(100);
      Dec(WaitTime, 100);

      // ...until no more wait time is left.
    until WaitTime <= 0;

    if WaitTime <= 0 then
      Exit;
  end
  else
    FFetchDataLock.Acquire;
    
  try
    Result := False;
    try
      FErrorCode := MYX_NO_ERROR;

      if AllowMultiQueries then
        myx_process_sql_statements(SQLCmd, @ExecuteMultiCmdSQL, self, 0)
      else
        myx_query_execute_direct(FMySQL, SQLCmd, @FErrorCode, @AffectedRows);

      if FErrorCode = MYX_NO_ERROR then
      begin
        Result := True;

        // Check if command causes a transaction state change.
        CheckConnectionStatusChange(SQLCmd);
      end
      else
      begin
        MySQLErrorNr := myx_mysql_errno(FMySQL);

        if DisplayErrorDialog then
          ShowModalDialog(_('Execution Error'), _('Error while executing query.') + #13#10#13#10 + SQLCmd + #13#10#13#10 +
            _('MySQL Error Number ') + IntToStr(MySQLErrorNr) + #13#10 + myx_mysql_error(FMySQL), myx_mtError, 'OK');
      end;
    except
      on x: Exception do
        if DisplayErrorDialog then
          ShowModalDialog(_('Execution Error'),  _('The following error occured while executing the query.') + #13#10#13#10 +
            SQLCmd + #13#10#13#10 + x.Message, myx_mtError, 'OK');
    end;
  finally
    FFetchDataLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ExecuteDirectQuery(SQLCmd: WideString; FetchMethod: TThreadExecMethod;
  WaitTime: Integer; DisplayErrorDialog: Boolean): Boolean;

// Directly executes a query. This method might run in parallel to an already running fetch thread.
// So make sure we wait until the other thread is running but don't block totally if the caller thread is the main thread.
// Note: This method is not reentrant! You cannot call it while it is already in progress.

var
  MySQLErrorNr: Integer;

begin
  Result := False;
  
  if MainThreadID = GetCurrentThreadId then
  begin
    // Called by the main thread so check if there is already a data fetch in progress.
    // Wait for it a limited amount of time.
    repeat
      // Try to get the lock. If that succeeds continue normally.
      if FFetchDataLock.TryEnter then
      begin
        WaitTime := 10; // Give it a value > 0 to avoid the exit call below if the caller did not give a wait time.
        Break;
      end;

      // If we could not get the lock then handle all pending messages (e.g. to finish synchronized calls).
      Application.ProcessMessages;

      // Do this every 100 ms...
      Sleep(100);
      Dec(WaitTime, 100);

      // ...until no more wait time is left.
    until WaitTime <= 0;

    if WaitTime <= 0 then
      Exit;
  end
  else
    FFetchDataLock.Acquire;
    
  try
    Result := False;
    try
      if myx_mysql_query(FMySQL, SQLCmd) = 0 then
      begin
        Result := True;
        FetchMethod(self);

        // Check if command causes a transaction state change.
        CheckConnectionStatusChange(SQLCmd);
      end
      else
      begin
        MySQLErrorNr := myx_mysql_errno(FMySQL);

        if DisplayErrorDialog then
          ShowModalDialog('Execution Error',
            'Error while executing query.' + #13#10#13#10 +
            SQLCmd + #13#10#13#10 +
            'MySQL Error Number ' + IntToStr(MySQLErrorNr) + #13#10 +
            myx_mysql_error(FMySQL),
            myx_mtError, 'OK');
      end;
    except
      on x: Exception do
        if (DisplayErrorDialog) then
          ShowModalDialog('Execution Error',
            'The following error occured while executing the query.' + #13#10#13#10 +
            SQLCmd + #13#10#13#10 +
            x.Message, myx_mtError, 'OK');
    end;
  finally
    FFetchDataLock.Release;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.KeepAliveTrigger(Sender: TObject);

var
  Notify: Boolean;
  
begin
  case Ping of
    0:
      begin
        // Have connection to server. Notify application if this is the first contact after we lost it previously.
        FConnected := True;
        Notify := FLastConnectionState = csDisconnected;
        FLastConnectionState := csConnected;
        if Notify then
          MessageToAllForms(WM_CONNECTION_ESTABLISHED, 0, 0);
      end;
    1:
      begin
        // Don't see the server. Notify application if this happend first time since last check.
        case FLastConnectionState of
          csConnected:
            FLastConnectionState := csUnclear; // One ping went wrong, give it a last chance.
          csUnclear:
            begin
              FConnected := False;
              FLastConnectionState := csDisconnected;
              MessageToAllForms(WM_CONNECTION_LOST, 0, 0);
            end;
        end;
      end;
    2:
      begin
        // Ping could not check because the connection is still in use. Stay in uncertain mode until we know for sure.
        FLastConnectionState := csUnclear;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.SetKeepAlive(const Value: Boolean);

begin
  if FKeepAlive <> Value then
  begin
    FKeepAlive := Value;
    FKeepAliveTimer.Enabled := FKeepAlive;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.SetConnected(const Value: Boolean);

begin
  if FConnected <> Value then
  begin
    if Value then
      Reconnect
    else
      Disconnect(True);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.SetDefaultSchema(const Value: WideString);

var
  error: Integer;

begin
  if FMySQL <> nil then
  begin
    if Value <> '' then
    begin
      Lock.Acquire;
      try
        error := myx_use_schema(FMySQL, Value);
        if (error = 0) then
          FDefaultSchema := myx_get_default_schema(FMySQL)
        else
        begin
          // Server connect might have gone, try automatic reconnection in this case.
          error := myx_mysql_errno(FMySQL);
          if (error = CR_SERVER_GONE_ERROR) or (error = CR_SERVER_LOST) or (error = CR_CONN_HOST_ERROR) then
          begin
            Reconnect(True);

            // Now try again. If this fails again then we show the error.
            error := myx_use_schema(FMySQL, Value);
          end;

          if (error = 0) then
            FDefaultSchema := myx_get_default_schema(FMySQL)
          else
            raise EMyxSQLError.Create(Format(_('The default schema cannot be changed to %s'), [Value]),
              myx_mysql_errno(FMySQL), myx_mysql_error(FMySQL));
        end;
      finally
        Lock.Release;
      end;
    end
    else
      FDefaultSchema := '';

    FUserConnection.schema := Value;

    MessageToAllForms(WM_DefaultSchemaChanged, 0, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.SchemaListChanged;

begin
  MessageToAllForms(WM_SchemaListChanged, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MySQLQueryPostHook(mysql: Pointer; cdata: Pointer; query: PChar; length: Integer); cdecl;

var
  PSender: TMySQLConn;

begin
  PSender := cdata;

  PSender.FQueryHookMultiReadSync.BeginWrite;
  try
    PSender.FQueryHookSQL := UTF8Decode(query);
  finally
    PSender.FQueryHookMultiReadSync.EndWrite;
  end;

  TThread.Synchronize(nil, PSender.TransactionQueryExecuted);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.SetInTransaction(InTransaction: Boolean);

var
  I: Integer;
  TransactionStarted: Boolean;

begin
  if FInTransaction <> InTransaction then
  begin
    TransactionStarted := ((Not (FInTransaction)) and (InTransaction));

    FInTransaction := InTransaction;

    if Assigned(FTransactionStatusChangeListeners) then
      for I := 0 to FTransactionStatusChangeListeners.Count - 1 do
      begin
        if (TransactionStarted) then
          IMySQLConnTransactionStatusChangeListener(FTransactionStatusChangeListeners[I]).TransactionStarted;
          
        IMySQLConnTransactionStatusChangeListener(FTransactionStatusChangeListeners[I]).TransactionStatusChanged;
      end;

    if FInTransaction then
      myx_mysql_set_query_hooks(FMySQL, nil, @MySQLQueryPostHook, Self)
    else
      myx_mysql_set_query_hooks(FMySQL, nil, nil, nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.GetQuoteChar: WideString;

begin
  if (FQuoteChar='') then
    FQuoteChar := Chr(myx_get_mysql_quote_char(FMySQL, 0));

  Result := FQuoteChar;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.InternalStartsTransaction(Lexer: TSQLLexer): Boolean;

// Checks if the current tokens in the Lexer comprise a command that starts a transaction.

begin
  Result := False;
  if Lexer.NextToken = toSymbol then
    if Lexer.TokenSymbolIs('start') then
    begin
      if (Lexer.NextToken = toSymbol) and Lexer.TokenSymbolIs('transaction') then
        Result := True;
    end
    else
      if Lexer.TokenSymbolIs('begin') then
        Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.Ping: Integer;

// Pings the server to see if it is still alive. If the connection has been dropped an attempt is made to
// reconnect.
// Returns 0 if the server is reachable, 1 if not and 2 if we cannot get exclusive access to the connection
// (perhaps because there is a data fetch thread still running).

begin
  if Assigned(FMySQL) then
  begin
    // If we cannot get the lock then the connection is currently working,
    // so there is no need to ping the server.
    if FFetchDataLock.TryEnter then
    try
      Result := myx_ping_server(FMySQL);
    finally
      FFetchDataLock.Release;
    end
    else
      Result := 2;
  end
  else
    Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.GetFetchingData: Boolean;

begin
  Result := FDataBeingFetched <> [];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.AddTransactionStatusListener(Listener: IMySQLConnTransactionStatusChangeListener);

begin
  if FTransactionStatusChangeListeners = nil then
    FTransactionStatusChangeListeners := TList.Create;
  if FTransactionStatusChangeListeners.IndexOf(Pointer(Listener)) = -1 then
    FTransactionStatusChangeListeners.Add(Pointer(Listener));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.RemoveTransactionStatusListener(Listener: IMySQLConnTransactionStatusChangeListener);

begin
  if FTransactionStatusChangeListeners <> nil then
  begin
    FTransactionStatusChangeListeners.Remove(Pointer(Listener));

    if FTransactionStatusChangeListeners.Count = 0 then
    begin
      FTransactionStatusChangeListeners.Free;
      FTransactionStatusChangeListeners := nil;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.TransactionQueryExecuted;

var
  I: Integer;

begin
  FQueryHookMultiReadSync.BeginRead;
  try
    if Assigned(FTransactionStatusChangeListeners) then
      for I := 0 to FTransactionStatusChangeListeners.Count - 1 do
        IMySQLConnTransactionStatusChangeListener(
          FTransactionStatusChangeListeners[I]).TransactionQueryExecuted(FQueryHookSQL);
  finally
    FQueryHookMultiReadSync.EndRead;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLConn.UseStatusBar(StatusBar: TTntStatusBar);

begin
  FStatusBar := StatusBar;
end;

//----------------------------------------------------------------------------------------------------------------------

function TMySQLConn.ConnectToEmbedded: Integer;

var
  ConnectionResult: Integer;

begin
  Result := -1;

  FEmbeddedConnection := True;

  if (myx_mysql_embedded_start() <> 0) then
    Exit;

  FMySQL := myx_mysql_embedded_init();
  if (FMySQL = nil) then
    Exit;

  FUserConnection := TMYX_USER_CONNECTION.Create('', '', '', '', 0, '', '', '', MYX_MYSQL_CONN,
    MYX_FAVORITE_USER_CONNECTION);

  //Connect to instance
  ConnectionResult := myx_connect_to_instance(FUserConnection.get_record_pointer, FMySQL);
  if ConnectionResult = 0 then
  begin
    FConnected := True;
    Result := 1;
  end
  else
  begin
    ShowModalDialog(Application.Title + ' ' + _('Error'),
      _('Could not connect to embedded server.') + ' ' + #13#10 + #13#10 +
      Format(_('MySQL Error Number %d' + #13#10 + '%s'),
      [myx_mysql_errno(FMySQL), myx_mysql_error(FMySQL)]), myx_mtError, 'OK');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TFetchDataThread.Create(AMySQLConn: TMySQLConn; AKindOfData: TFetchingDataKind;
  AFetchMethod: TThreadExecMethod; ARefreshMethod: TThreadExecMethod; ATarget: TObject;
  AStatusBarText: WideString; AAllowParallelExecution, ShowWaitCursor: Boolean);

begin
  inherited Create(True);

  FConnection := AMySQLConn;
  FKindOfData := AKindOfData;
  FFetchMethod := AFetchMethod;
  FRefreshMethod := ARefreshMethod;
  FTarget := ATarget;
  StatusBarText := AStatusBarText;
  FSynchronizedMethod := nil;
  FAllowParallelExecution := AAllowParallelExecution;
  FShowWaitCursor := ShowWaitCursor;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.Execute;

begin
  if not FAllowParallelExecution then
    FConnection.FFetchDataLock.Acquire;

  InterlockedIncrement(DataThreadCount);

  if FShowWaitCursor then
    Screen.Cursor := crAppStart;

  try
    try
      with FConnection.FWorkList.LockList do
      try
        Remove(Self);
        FConnection.FCurrentDataThread := Self;
      finally
        FConnection.FWorkList.UnlockList;
      end;

      if not Terminated and (FStatusBarText <> '') then
        Synchronize(UpdateStatusBar);

      if not Terminated and Assigned(FFetchMethod) then
        FFetchMethod(self);

    except
      on x: EMyxSQLError do
        if not Terminated then
        begin
          FErrorMessage := x.FormattedMessage;
          Synchronize(ShowError);
        end;
      on x: Exception do
        if not Terminated then
        begin
          FErrorMessage := x.Message;
          Synchronize(ShowError);
        end;
    end;
  finally
    try
      with FConnection.FWorkList.LockList do
      try
        FConnection.FCurrentDataThread := nil;
        if Count = 0 then
          FConnection.FDataBeingFetched := []
        else
          FConnection.FDataBeingFetched := FConnection.FDataBeingFetched - [FKindOfData];
      finally
        FConnection.FWorkList.UnlockList;
      end;

      if not FAllowParallelExecution then
        FConnection.FFetchDataLock.Release;

      StatusBarText := '';
      if not Terminated then
        Synchronize(UpdateStatusBar);

      if DataThreadCount > 0 then
        if (InterlockedDecrement(DataThreadCount) = 0) and FShowWaitCursor then
          Screen.Cursor := crDefault;

      if not Terminated and Assigned(FRefreshMethod) then
        ExecuteSynchronized(FRefreshMethod);
    except
      // Capture clean-up exceptions too. Don't let exceptions escape the thread context.
      // However, if there was already an exception then don't overwrite their error message.
      on x: EMyxSQLError do
        if not Terminated and (FErrorMessage = '') then
        begin
          FErrorMessage := x.FormattedMessage;
          Synchronize(ShowError);
        end;
      on x: Exception do
        if not Terminated and (FErrorMessage = '') then
        begin
          FErrorMessage := x.Message;
          Synchronize(ShowError);
        end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.UpdateStatusBar;

begin
  if Assigned(FConnection.FStatusBar) and not Application.Terminated then
  begin
    if FConnection.FStatusBar.Panels.Count > 1 then
      FConnection.FStatusBar.Panels[1].Text := FStatusBarText;
    FConnection.FStatusBar.Repaint;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.ShowError;

begin
  ShowModalDialog(Application.Title + ' ' + _('Error'), FErrorMessage, myx_mtError, _('OK'));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.ExecuteSynchronized(SynchronizedMethod: TThreadExecMethod);

begin
  self.FSynchronizedMethod := SynchronizedMethod;

  Synchronize(ExecuteSynchronizedMethod);
 end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.ExecuteSynchronizedMethod;

begin
  if Assigned(FSynchronizedMethod) then
    FSynchronizedMethod(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFetchDataThread.SetStatusBar(StatusBarText: WideString);

begin
  FStatusBarText := StatusBarText;

  UpdateStatusBar;
end;

//----------------- TConnectionLock ------------------------------------------------------------------------------------

function TConnectionLock.TryEnter: Boolean;

begin
  Result := TryEnterCriticalSection(FSection);
end;

//----------------------------------------------------------------------------------------------------------------------

end.

