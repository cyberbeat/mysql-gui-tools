unit ConnectToInstance;

interface

uses
  gnugettext, Windows, WinSock, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, Menus, Grids,
  PNGImage, myx_util_public_interface, myx_public_interface, AuxFuncs, MySQLConnection,
  Options, OptionsEditor, MyxError, Math,
  TntForms, TntStdCtrls, TntClasses, TntExtCtrls, TntButtons, TntGrids,
  TntComCtrls, AuxApplicationFuncs;

{$include Consts.ini}

type
  TConnectToInstanceForm = class;

  TConnectToInstanceThread = class(TThread)
    constructor Create(CreateSuspended: Boolean; ConnectToInstanceForm: TConnectToInstanceForm); reintroduce;
    destructor Destroy; override;
  protected
    DefaultSchema: WideString;

    procedure Execute; override;
    procedure ShowMySQL3ServerWarning;
    procedure ShowNoDefaultSchemaWarning;
    procedure ShowDefaultSchemaDoesNotExistsWarning;
  public
    ConnectionResult: integer;
    ConnectToInstanceForm: TConnectToInstanceForm;
  end;

  TPingThread = class(TThread)
  private
    output_str: WideString;
  protected
    procedure Execute; override;
    procedure WriteResponseToMemo;
  public
    ip: longint;
    ping_result: PMYX_PING_RESULT;
    ResponseMemo: TTntMemo;
    PingErrorMsgs: TTntStringList;
  end;

  TConnectToInstanceForm = class(TTntForm)
    ConnectToHostPnl: TTntGroupBox;
    ConnectionLbl: TTntLabel;
    ConnectionCBox: TTntComboBox;
    UsernameLbl: TTntLabel;
    PasswordLbl: TTntLabel;
    PasswordEd: TTntEdit;
    PortEd: TTntEdit;
    PortLbl: TTntLabel;
    HostnameLbl: TTntLabel;
    SchemataEdLbl: TTntLabel;
    SchemataEd: TTntEdit;
    BottomPnl: TTntPanel;
    OKBtn: TTntButton;
    CancelBtn: TTntButton;
    UsernameCBox: TTntComboBox;
    HostnameCBox: TTntComboBox;
    ClearFieldsBtn: TTntButton;
    AdvancedOptionsGBox: TTntGroupBox;
    AdvancedOptionsPageControl: TTntPageControl;
    MySQLOptionsTabSheet: TTabSheet;
    AdvancedOptionsGeneralTabSheet: TTabSheet;
    AdvancedOptionsStringGrid: TTntStringGrid;
    AdvancedOptionsBottomPnl: TTntPanel;
    UseCompressionCBox: TTntCheckbox;
    NamedPipeCBox: TTntCheckBox;
    ConnectToInstanceAni: TAnimate;
    AdvancedOptionsBottomRightPnl: TTntPanel;
    AdvancedOptionsDelBtn: TTntSpeedButton;
    AdvancedOptionsAddBtn: TTntSpeedButton;
    AdvancedOptionsHidePageControlHeaderPnl: TTntPanel;
    AdvancedOptionsResetBtn: TTntSpeedButton;
    DetailsBtn: TTntBitBtn ;
    OptionsBtn: TTntBitBtn ;
    UseAnsiQuotesCBox: TTntCheckBox;
    HeaderImg: TTntImage;
    UseSSLCBox: TTntCheckBox;
    PipeNameEdit: TTntEdit;
    PipeNameLabel: TTntLabel;

    constructor Create(AOwner: TComponent;
      MySQLConnectionsOnly: Boolean = True;
      SelectSchemata: Boolean = False); reintroduce; overload;
    //Form std. functions
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    //Stored connection functions
    procedure GetStoredConnections;
    procedure GetValuesFromConnection(a_connection: TMYX_USER_CONNECTION);
    function SetConnectionValues(a_connection: TMYX_USER_CONNECTION): TMYX_USER_CONNECTION;

    //GUI interaction
    //CBoxes
    procedure ConnectionCBoxCloseUp(Sender: TObject);
    procedure ConnectionCBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DetailsBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure ClearFieldsBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);

    //Function to enabled/disable controls while trying to connect
    procedure EnableDisableControls(Enable: Boolean);
    //Function to enabled/disable controls based on user input
    procedure CheckBtnState;
    //Shows/Hides Options
    procedure ShowHideAdvancedOptions;

    //function executed when connection thread terminates
    function IndexOfUserConnectionByValue(
      UserConnections: TMYX_USER_CONNECTIONS;
      AObject: TMYX_USER_CONNECTION): Integer;
    procedure ConnectionResult(Sender: TObject);

    procedure DoPing(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ConnectionCBoxSelect(Sender: TObject);
    procedure TntFormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure NamedPipeCBoxClick(Sender: TObject);
    procedure UsernameCBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HostnameCBoxChange(Sender: TObject);
    procedure PortEdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    PNGImage,
    PNGClearSearchImg: TPNGObject;

    stored_conns: TMYX_USER_CONNECTIONS;
    favorite_conns: TList;

    MySQLConnectionsOnly,
    SelectSchemata: Boolean;

    ConnectToInstanceThread: TConnectToInstanceThread;
    ConnectToInstanceThreadHandle: THandle;

    PingThread: TPingThread;
    PingThreadHandle: THandle;
    ping_result: PMYX_PING_RESULT;
    PingErrorMsgs: TTntStringList;

    //Indicates if threads are active
    Connecting,
    Pinging: Boolean;

    SkipConnecting: Boolean;

    FInitHeight: Integer;

    FAllowConnectionSkip: Boolean;
    FLastHost: WideString;

    procedure CheckForDuplicateConnections;

  protected
    function CreateOptionsForm: TOptionsForm; virtual;
  public
    PMySQL: Pointer;
    User_Connection: TMYX_USER_CONNECTION;

    property AllowConnectionSkip: Boolean read FAllowConnectionSkip write FAllowConnectionSkip;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  PNGTools, Unicode;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

constructor TConnectToInstanceForm.Create(AOwner: TComponent; MySQLConnectionsOnly: Boolean = True;
  SelectSchemata: Boolean = False);

begin
  inherited Create(AOwner);

  FInitHeight := Height;

  InitForm(self);

  Caption := Application.Title+' '+product_version+' '+product_build_level;

  self.MySQLConnectionsOnly := MySQLConnectionsOnly;
  self.SelectSchemata := SelectSchemata;

  //Load resources
  PNGImage := LoadPNGImageFromResource('connection_dialog_header', HeaderImg);
  ConnectToInstanceAni.ResName := 'progress_indicator';

  favorite_conns := TList.Create;

  //Hide AdvancedOptions GroupBox
  Height := FInitHeight -
    ord(AdvancedOptionsGBox.Visible)*(AdvancedOptionsGBox.Height + 16)-
    ord(Not(SelectSchemata))*34;
  AdvancedOptionsGBox.Visible := False;
  AdvancedOptionsHidePageControlHeaderPnl.BringToFront;

  if(SelectSchemata)then
  begin
    SchemataEdLbl.Visible := True;
    SchemataEd.Visible := True;
  end
  else
  begin
    ConnectToHostPnl.Height := ConnectToHostPnl.Height-34;
    AdvancedOptionsGBox.Top := AdvancedOptionsGBox.Top-34;
  end;

  HostnameCBox.Width := PasswordEd.Width;

  ConnectToInstanceThread := nil;
  Connecting := False;
  PingThread := nil;
  ping_result := nil;
  PingErrorMsgs := nil;
  Pinging := False;

  //Fetch Stored Connections
  GetStoredConnections;

  PMySQL := nil;

  SkipConnecting := False;

  AdvancedOptionsPageControl.TabHeight := 1;

  FAllowConnectionSkip := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.FormCreate(Sender: TObject);

begin
  MYXCommonOptions.RestoreWindowPos(self, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.FormDestroy(Sender: TObject);

begin
  MYXCommonOptions.AddWindowPos(self);

  //Terminate PingThread if still running
  if(Pinging)then
    TerminateThread(PingThreadHandle, 0);

  //Free PingThread and stuff
  if(PingThread<>nil)then
    PingThread.Free;
  if(ping_result<>nil)then
    FreeMem(ping_result);
  if(PingErrorMsgs<>nil)then
    PingErrorMsgs.Free;

  favorite_conns.Free;

  PNGClearSearchImg.Free;
  PNGImage.Free;

  stored_conns.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  // If the ConnectToInstanceThread is running, terminate it.
  if Connecting then
    CancelBtnClick(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.GetStoredConnections;

var
  I: integer;
  user_conns: PMYX_USER_CONNECTIONS;
  error: MYX_LIB_ERROR;

begin
  //Fetch connections from library
  user_conns := myx_load_user_connections(
    MYXCommonOptions.UserDataDir+'mysqlx_user_connections.xml', @error);
  if(error<>MYX_NO_ERROR)then
    raise EMyxError.Create('Error while loading stored connections.'+
      'Error Number '+IntToStr(Ord(error)));
  try
    stored_conns := TMYX_USER_CONNECTIONS.create(user_conns);
  finally
    myx_free_user_connections(user_conns);
  end;

  CheckForDuplicateConnections;

  //fill ComboBoxes, only use MYX_MYSQL_CONNs
  ConnectionCBox.Items.Clear;
  UsernameCBox.Items.Clear;
  HostnameCBox.Items.Clear;
  HostnameCBox.Items.Add('localhost');
  favorite_conns.Clear;

  for I := 0 to stored_conns.user_connections.Count-1 do
    if(stored_conns.user_connections[I].connection_type=MYX_MYSQL_CONN)or
      (Not(MySQLConnectionsOnly))then
    begin
      //fill connection lookup
      if(stored_conns.user_connections[I].storage_type=MYX_FAVORITE_USER_CONNECTION)then
      begin
        favorite_conns.Add(stored_conns.user_connections[I]);
        ConnectionCBox.Items.Add(stored_conns.user_connections[I].connection_name);
      end;

      if(UsernameCBox.Items.IndexOf(stored_conns.user_connections[I].username)=-1)then
        UsernameCBox.Items.Add(stored_conns.user_connections[I].username);

      if(HostnameCBox.Items.IndexOf(stored_conns.user_connections[I].hostname)=-1)then
        HostnameCBox.Items.Add(stored_conns.user_connections[I].hostname);
    end;

  //fill edits with last connection
  if(stored_conns.last_connection>-1)and(stored_conns.last_connection<stored_conns.user_connections.Count)then
    if(stored_conns.user_connections[stored_conns.last_connection].connection_type=MYX_MYSQL_CONN)or
      (Not(MySQLConnectionsOnly))then
    begin
      if(favorite_conns.IndexOf(stored_conns.user_connections[stored_conns.last_connection])>-1)then
        ConnectionCBox.ItemIndex := favorite_conns.IndexOf(stored_conns.user_connections[stored_conns.last_connection]);

      GetValuesFromConnection(stored_conns.user_connections[stored_conns.last_connection]);

      User_Connection := stored_conns.user_connections[stored_conns.last_connection];
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.GetValuesFromConnection(a_connection: TMYX_USER_CONNECTION);

var
  I: integer;

begin
  // Fill edits all controls.
  // Start with advanced options as setting them might (wrongly) modify the normal options.
  UseCompressionCBox.Checked := (a_connection.advanced_options.IndexOf('COMPRESS=Yes') > -1);
  UseSSLCBox.Checked := (a_connection.advanced_options.IndexOf('USE_SSL=Yes') > -1);
  UseAnsiQuotesCBox.Checked := (a_connection.advanced_options.IndexOf('ANSI_QUOTES=Yes') > -1);
  NamedPipeCBox.Checked := (a_connection.advanced_options.IndexOf('NAMED_PIPE=Yes') > -1);
  PipeNameEdit.Text := a_connection.advanced_options.values['SOCKET_PATH'];

  with a_connection do
  begin
    UsernameCBox.Text := username;
    PasswordEd.Text := password;
    HostnameCBox.Text := hostname;
    PortEd.Text := IntToStr(port);
    SchemataEd.Text := schema;
  end;

  // Fill Advanced Option StringGrid.
  for I := 0 to AdvancedOptionsStringGrid.ColCount-1 do
    AdvancedOptionsStringGrid.Cols[I].Text := '';

  AdvancedOptionsStringGrid.Cells[0, 0] := 'Option';
  AdvancedOptionsStringGrid.Cells[1, 0] := 'Value';

  // Display connection name.
  if(Comparetext(ConnectionCBox.Text,
    a_connection.connection_name)<>0)and
    (a_connection.storage_type=MYX_FAVORITE_USER_CONNECTION)then
    ConnectionCBox.Text := a_connection.connection_name;

  CheckBtnState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.HostnameCBoxChange(Sender: TObject);
begin
  // Enable OK/Clear buttons in case
  CheckBtnState;
end;

//----------------------------------------------------------------------------------------------------------------------

function TConnectToInstanceForm.SetConnectionValues(a_connection: TMYX_USER_CONNECTION): TMYX_USER_CONNECTION;

var
  Index: Integer;
  
begin
  if(a_connection=nil)then
    a_connection := TMYX_USER_CONNECTION.Create(ConnectionCBox.Text,
      UsernameCBox.Text, PasswordEd.Text, HostnameCBox.Text,
      StrToInt(PortEd.Text), SchemataEd.Text,
      '', '',
      MYX_MYSQL_CONN, MYX_HISTORY_USER_CONNECTION)
  else
  begin
    with a_connection do
    begin
      connection_name := ConnectionCBox.Text;
      username := UsernameCBox.Text;
      password := PasswordEd.Text;
      hostname := HostnameCBox.Text;
      port := StrToInt(PortEd.Text);
      connection_type := MYX_MYSQL_CONN;
      schema := SchemataEd.Text;
      // advanced_options are not processed below, so dont touch them
    end;
  end;

  Index := a_connection.advanced_options.IndexOfName('COMPRESS');
  if UseCompressionCBox.Checked then
    a_connection.advanced_options.Values['COMPRESS'] := 'Yes'
  else
    if Index >-1 then
      a_connection.advanced_options.Delete(Index);

  Index := a_connection.advanced_options.IndexOfName('USE_SSL');
  if UseSSLCBox.Checked then
    a_connection.advanced_options.Values['USE_SSL'] := 'Yes'
  else
    if Index >-1 then
      a_connection.advanced_options.Delete(Index);

  Index := a_connection.advanced_options.IndexOfName('ANSI_QUOTES');
  if UseAnsiQuotesCBox.Checked then
    a_connection.advanced_options.Values['ANSI_QUOTES'] :='Yes'
  else
    if Index >-1 then
      a_connection.advanced_options.Delete(Index);

  Index := a_connection.advanced_options.IndexOfName('NAMED_PIPE');
  if NamedPipeCBox.Checked then
  begin
    a_connection.advanced_options.Values['NAMED_PIPE'] := 'Yes';
    a_connection.advanced_options.Values['SOCKET_PATH'] := PipeNameEdit.Text;
  end
  else
  begin
    if Index >-1 then
      a_connection.advanced_options.Delete(Index);
    Index := a_connection.advanced_options.IndexOfName('SOCKET_PATH');
    if Index >-1 then
      a_connection.advanced_options.Delete(Index);
  end;

  SetConnectionValues := a_connection;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ConnectionCBoxCloseUp(Sender: TObject);

begin
  if PasswordEd.Text = '' then
  begin
    if Visible then
      PasswordEd.SetFocus
    else
      ActiveControl := PasswordEd;
  end
  else
    ActiveControl := UsernameCBox;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ConnectionCBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if(Key=VK_Return)then
  begin
    ConnectionCBoxCloseUp(self);

    if(PasswordEd.Text='')then
      PasswordEd.SetFocus
    else if(OKBtn.Enabled)then
      OKBtn.SetFocus;

    Key := 0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.OKBtnClick(Sender: TObject);

begin
  User_Connection := SetConnectionValues(User_Connection);
  if Assigned(PMySQL) then
  begin
    myx_mysql_close(PMySQL);
    PMySQL := nil;
  end;

  PMySQL := myx_mysql_init();
  if(PMySQL=nil)then
    raise EMyxError.Create(_('Error while allocating memory for MySQL Struct.'));

  ConnectToInstanceThread := TConnectToInstanceThread.Create(True, self);
  try
    ConnectToInstanceThreadHandle := ConnectToInstanceThread.Handle;
    ConnectToInstanceThread.Priority := tpNormal;

    ConnectToInstanceThread.OnTerminate := ConnectionResult;
    ConnectToInstanceThread.FreeOnTerminate := True;

    ConnectToInstanceThread.Resume;
  except
    ConnectToInstanceThread.Free;

    raise;
  end;

  Connecting := True;

  //Show animation
  ConnectToInstanceAni.Visible := True;
  ConnectToInstanceAni.Active := True;

  //Disable controls
  EnableDisableControls(False);

  CancelBtn.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

function TConnectToInstanceForm.IndexOfUserConnectionByValue(UserConnections: TMYX_USER_CONNECTIONS;
  AObject: TMYX_USER_CONNECTION): Integer;

var
  I: integer;

begin
  Result := -1;

  with UserConnections do
  begin
    for I := 0 to user_connections.Count-1 do
    begin
      //Ignore Password when comparing
      if((user_connections[I].connection_name=AObject.connection_name)and
        (user_connections[I].username=AObject.username)and
        (user_connections[I].hostname=AObject.hostname)and
        (user_connections[I].port=AObject.port)and
        (user_connections[I].schema=AObject.schema)and
        (user_connections[I].connection_type=AObject.connection_type)and
        (user_connections[I].storage_type=AObject.storage_type)and
        (user_connections[I].advanced_options.Text=AObject.advanced_options.Text))then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.NamedPipeCBoxClick(Sender: TObject);

begin
  if NamedPipeCBox.Checked then
  begin
    FLastHost := HostnameCBox.Text;
    HostnameCBox.Text := 'localhost';
  end
  else
    HostnameCBox.Text := FLastHost;

  HostnameCBox.Enabled := not NamedPipeCBox.Checked;
  PortEd.Enabled := not NamedPipeCBox.Checked;
  PipeNameLabel.Enabled := NamedPipeCBox.Checked;
  PipeNameEdit.Enabled := NamedPipeCBox.Checked;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.EnableDisableControls(Enable: Boolean);

var
  I, J: integer;
  
begin
  for I := 0 to ConnectToHostPnl.ControlCount-1 do
    ConnectToHostPnl.Controls[I].Enabled := Enable;

  //Disable Controls of AdvancedOptionsPageControl
  for I := 0 to AdvancedOptionsPageControl.PageCount-1 do
    for J := 0 to AdvancedOptionsPageControl.Pages[I].ControlCount-1 do
      AdvancedOptionsPageControl.Pages[I].Controls[J].Enabled := Enable;

  DetailsBtn.Enabled := Enable;
  ClearFieldsBtn.Enabled := Enable;
  OKBtn.Enabled := Enable;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.CancelBtnClick(Sender: TObject);

begin
  if(Connecting)then
  begin
    TerminateThread(ConnectToInstanceThreadHandle, 0);
    ConnectToInstanceThread.Free;
    ConnectToInstanceThread := nil;
  end;

  //Free MySQL Struct even if the state is not connecting
  if(PMySQL<>nil)then
  begin
    myx_mysql_close(PMySQL);
    PMySQL := nil;
  end;

  if(Connecting)then
  begin
    //Stop and hide Animation
    ConnectToInstanceAni.Active := False;
    ConnectToInstanceAni.Visible := False;

    //Enable Controls
    EnableDisableControls(True);

    //Set focus back to password, maybe it was incorrect
    PasswordEd.SetFocus;

    Connecting := False;
  end
  else
  begin
    if(SkipConnecting)then
      ModalResult := mrAbort
    else
      ModalResult := mrCancel;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.CheckBtnState;

begin
  OKBtn.Enabled := (HostnameCBox.Text <> '') and (PortEd.Text <> '');
  ClearFieldsBtn.Enabled := (ConnectionCBox.Text <> '') or (UsernameCBox.Text <> '') or (PasswordEd.Text <> '') or
    (HostnameCBox.Text <> '') or (SchemataEd.Text <> '');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ClearFieldsBtnClick(Sender: TObject);

begin
  User_connection := nil;

  ConnectionCBox.ItemIndex := -1;
  ConnectionCBox.Text := '';
  UsernameCBox.Text := '';
  PasswordEd.Text := '';
  HostnameCBox.Text := '';
  PortEd.Text := '3306';
  SchemataEd.Text := '';

  CheckBtnState;

  UsernameCBox.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ShowHideAdvancedOptions;

begin
  if not AdvancedOptionsGBox.Visible then
  begin
    AdvancedOptionsPageControl.ActivePage := MySQLOptionsTabSheet;
    DetailsBtn.Caption := _('<< &Details');
  end
  else
    DetailsBtn.Caption := _('&Details >>');

  Height := FInitHeight - Ord(AdvancedOptionsGBox.Visible)*(AdvancedOptionsGBox.Height + 16) - Ord(Not(SelectSchemata)) * 34;
  AdvancedOptionsGBox.Visible := not AdvancedOptionsGBox.Visible;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.DetailsBtnClick(Sender: TObject);

begin
  ShowHideAdvancedOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.DoPing(Sender: TObject);

var
  PingMemo: TTntMemo;
  ip: longint;

begin
  TMyxModalDialog(TTntButton(Sender).Owner).ShowEdit;

  PingMemo := TMyxModalDialog(TTntButton(Sender).Owner).ValueMemo;

  if(Not(Pinging))then
  begin
    ip := myx_resolve_network_name(HostnameCBox.Text);
    if(ip=0)then
      PingMemo.Lines.Add(_('Could not resolve hostname') + ' ' + HostnameCBox.Text)
    else
    begin
      //Get memory for Ping result
      if(ping_result<>nil)then
        FreeMem(ping_result);
      ping_result := AllocMem(sizeof(MYX_PING_RESULT));

      if(PingErrorMsgs=nil)then
      begin
        PingErrorMsgs := TTntStringList.Create;

        PingErrorMsgs.Text := 
          'Error Base'+#13#10+
          'Buffer too small.'+#13#10+
          'Destination net unreachable.'+#13#10+
          'Destination host unreachable.'+#13#10+
          'Destination protocol unreachable.'+#13#10+
          'Destination port unreachable.'+#13#10+
          'Out of resources.'+#13#10+
          'Bad option.'+#13#10+
          'Hardware error.'+#13#10+
          'Packet too large.'+#13#10+
          'Request timed out.'+#13#10+
          'Bad request.'+#13#10+
          'Bad route.'+#13#10+
          'TTL expired in transit.'+#13#10+
          'TTL expired REASSEM.'+#13#10+
          'Param problem.'+#13#10+
          'Source quench.'+#13#10+
          'Option too large.'+#13#10+
          'Bad destination.'+#13#10+
          'Address deleted.'+#13#10+
          'Spec MNU change.'+#13#10+
          'MTU change.'+#13#10+
          'Unload';
      end;

      //If there is already a thread running, kill it
      if(Pinging)then
        TerminateThread(PingThreadHandle, 0);

      if(PingThread<>nil)then
        PingThread.Free;

      //Create Thread
      PingThread := TPingThread.Create(True);
      try
        PingThreadHandle := PingThread.Handle;
        PingThread.Priority := tpNormal;
        PingThread.ip := ip;
        PingThread.ping_result := ping_result;
        PingThread.PingErrorMsgs := PingErrorMsgs;
        PingThread.ResponseMemo := PingMemo;

        PingMemo.Text := 'Pinging '+HostnameCBox.Text;

        PingThread.Resume;
        Pinging := True;

        TTntButton(Sender).Caption := 'Stop Pinging';
      except
        PingThread.Free;

        raise;
      end;
    end;
  end
  else
  begin
    if(PingThread<>nil)then
    begin
      PingThread.ResponseMemo := nil;
      PingThread.Terminate;
    end;

    TTntButton(Sender).Caption := 'Ping';

    Pinging := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ConnectionResult(Sender: TObject);

var
  ModelDlg: TMyxModalDialog;
  ping: WideString;

begin
  Connecting := False;

  ConnectToInstanceAni.Active := False;
  ConnectToInstanceAni.Visible := False;

  //accept -20 to ignore warning
  if(TConnectToInstanceThread(Sender).ConnectionResult=0)or
    (TConnectToInstanceThread(Sender).ConnectionResult=-20)then
  begin
    if(ConnectionCBox.ItemIndex>-1)then
    begin
      stored_conns.last_connection := stored_conns.user_connections.IndexOf(User_Connection);
    end
    else
    begin
      //Check if connection is already stored
      stored_conns.last_connection := IndexOfUserConnectionByValue(stored_conns, User_Connection);

      if(stored_conns.last_connection<0)then
      begin
        stored_conns.user_connections.Add(User_Connection);
        stored_conns.last_connection := stored_conns.user_connections.Count-1;
      end
      else
        stored_conns.user_connections[stored_conns.last_connection].Assign(User_Connection);
    end;

    myx_store_user_connections(stored_conns.get_record_pointer,
      MYXCommonOptions.PasswordStorageType,
        MYXCommonOptions.UserDataDir+'mysqlx_user_connections.xml');

    ModalResult := mrOK;
  end
  //ignore ConnectionResult -10 after warning
  else if(TConnectToInstanceThread(Sender).ConnectionResult<>-10)then
  begin
    CancelBtn.Enabled := False;
    try
      ModelDlg := TMyxModalDialog.Create(Application.Title+' Error',
        WideFormat(_('Could not connect to the specified instance. ' + #13#10 + #13#10 +
        'MySQL Error Number %d'#13#10 + '%s' + #13#10#13#10 +
        'If you want to check the network connection, ' + 'please click the Ping button.'),
        [myx_mysql_errno(PMySQL), myx_mysql_error(PMySQL)]), myx_mtError, _('OK') + #13#10 + _('Ping'), True, '', ping,
        5, True);

      TTntButton(ModelDlg.DlgBtnList[1]).ModalResult := 0;
      TTntButton(ModelDlg.DlgBtnList[1]).OnClick := DoPing;

      ModelDlg.HideEdit;

      try
        ModelDlg.ShowModal;

        if Assigned(PingThread) then
        begin
          PingThread.ResponseMemo := nil;
          PingThread.Terminate;

          Pinging := False;
        end;
      finally
        ModelDlg.Free;
      end;

      //Free memory allocated for the MySQL Struct
      myx_mysql_close(PMySQL);
      PMySQL := nil;

    finally
      CancelBtn.Enabled := True;
    end;
  end;

  //Enable Controls
  EnableDisableControls(True);

  if(TConnectToInstanceThread(Sender).ConnectionResult=-10) then
  begin
    if (SchemataEd.Visible) then SchemataEd.SetFocus;
  end
  else
    //Set focus back to password, maybe it was incorrect
    PasswordEd.SetFocus;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceThread.Execute;

begin
  ConnectionResult := 1;

  DefaultSchema := ConnectToInstanceForm.User_Connection.schema;
  ConnectToInstanceForm.User_Connection.schema := '';

  ConnectionResult := myx_connect_to_instance(
    ConnectToInstanceForm.User_Connection.get_record_pointer,
    ConnectToInstanceForm.PMySQL);

  if(ConnectionResult=0)then
  begin
    if(myx_get_mysql_major_version(ConnectToInstanceForm.PMySQL)=3)then
      Synchronize(ShowMySQL3ServerWarning);

    if(ConnectionResult=0)and
      (ConnectToInstanceForm.SelectSchemata)and
      (ConnectToInstanceForm.SchemataEd.Text='')then
    begin
      //Show warning and return -10 (=OK) or -20 (=Ignore
      Synchronize(ShowNoDefaultSchemaWarning);
    end
    else if(ConnectionResult=0)and
      (ConnectToInstanceForm.SelectSchemata)and
      (DefaultSchema<>'')then
    begin
      ConnectionResult := myx_use_schema(ConnectToInstanceForm.PMySQL,
        DefaultSchema);

      if(ConnectionResult<>0)then
        Synchronize(ShowDefaultSchemaDoesNotExistsWarning);
    end;
  end;

  ConnectToInstanceForm.User_Connection.schema := DefaultSchema;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceThread.ShowMySQL3ServerWarning;

begin
  ConnectionResult := ShowOptionalModalDialog(_('Connection Dialog - MySQL 3.x Server Unsupported'),
    _('You are connecting to an MySQL 3.x server. The MySQL GUI tools only support '+
    'MySQL servers 4.0 and higher. Using this tool with a 3.x server might result in '+
    'unexpected behaviour.'),
    myx_mtWarning, _('OK')+#13#10+_('Ignore'),
    True,
    '')*-10;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceThread.ShowNoDefaultSchemaWarning;

begin
  ConnectionResult := ShowOptionalModalDialog(_('Connection Dialog - No Default Schema Specified'),
    _('You have not specified a default schema for this connection. '+
    'Although it is possible to connect without specifying default schema '+
    'you are highly encouraged to do so.'),
    myx_mtWarning, _('OK')+#13#10+_('Ignore'),
    True,
    'connection_dlg_no_schema_specified')*-10;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceThread.ShowDefaultSchemaDoesNotExistsWarning;

var
  AffectedRows: Int64;
  
begin
  //Ask the user if he wants to create the given schema
  if(ShowModalDialog(_('Connection Dialog - Default Schema Does Not Exist'),
    Format(_('The schema `%s` does not exist. '+
    'Do you want to create the schema now?'), [DefaultSchema]),
    myx_mtConfirmation, _('Yes')+#13#10+_('No'))=1)then
  begin
    //Create the schema
    myx_query_execute_direct(ConnectToInstanceForm.PMySQL, 'CREATE DATABASE `'+DefaultSchema+'`', @ConnectionResult,
      @AffectedRows);

    //Make the schema the new default schema
    if(ConnectionResult=0)then
      ConnectionResult := myx_use_schema(ConnectToInstanceForm.PMySQL,
        DefaultSchema);
  end
  else
    ConnectionResult := -10;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TConnectToInstanceThread.Create(CreateSuspended: Boolean; ConnectToInstanceForm: TConnectToInstanceForm);

begin
  inherited Create(CreateSuspended);

  self.ConnectToInstanceForm := ConnectToInstanceForm;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TConnectToInstanceThread.Destroy;

begin
  ConnectToInstanceForm.ConnectToInstanceThread := nil;

  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPingThread.Execute;

var
  i: integer;
  result: integer;

begin
  while True do
  begin
    result := myx_ping_host(ip, 2000, ping_result);

    //Check if the Thread should be terminated
    if(Terminated)then
      Exit;

    //Print result
    if(result=0)then
      output_str := _('Reply from') + ' ' + inet_ntoa(TInAddr(ip)) + ': ' +
        _('Time =') + ' ' + IntToStr(ping_result.round_trip_time) + _('ms') + ' ' +
        'TTL = ' + IntToStr(ping_result.ttl)
    else
      if result = -1 then
      output_str := _('Request timed out.')
      else
      begin
        if(result-1>0)and(result-1<PingErrorMsgs.Count)then
          output_str := PingErrorMsgs[result-1]
        else
          output_str := _('Unknown Error.');
      end;

    Synchronize(WriteResponseToMemo);

    for i := 0 to 9 do
    begin
      //Check if the Thread should be terminated
      if(Terminated)then
        Exit;

      Sleep(100);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPingThread.WriteResponseToMemo;

begin
  if(ResponseMemo<>nil)then
    ResponseMemo.Lines.Add(output_str);
end;

//----------------------------------------------------------------------------------------------------------------------

function TConnectToInstanceForm.CreateOptionsForm: TOptionsForm;

begin
  Result := TOptionsForm.Create(self, nil, PMySQL);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.OptionsBtnClick(Sender: TObject);

var
  OptionsForm: TOptionsForm;
  I: integer;

begin
  OptionsForm := CreateOptionsForm;

  try
    // Select Connections Page
    OptionsForm.ShowOptionPage(ConnectionsPage);

    // Select same connection as selected in the connection dlg
    if Assigned(User_Connection) then
      for I := 0 to OptionsForm.ConnsTreeView.Items.Count - 1 do
        if Assigned(OptionsForm.ConnsTreeView.Items[I].Data) then
          if (TObject(OptionsForm.ConnsTreeView.Items[I].Data) is TMYX_USER_CONNECTION) then
            if ((TMYX_USER_CONNECTION(OptionsForm.ConnsTreeView.Items[I].Data).connection_name =
                User_Connection.connection_name) and
              (TMYX_USER_CONNECTION(OptionsForm.ConnsTreeView.Items[I].Data).storage_path=
                User_Connection.storage_path)) then
              OptionsForm.ConnsTreeView.Selected := OptionsForm.ConnsTreeView.Items[I];

    OptionsForm.ActiveControl := OptionsForm.ConnsTreeView;
    OptionsForm.ShowModal;

    GetStoredConnections;
  finally
    OptionsForm.Free;
  end;
end;

procedure TConnectToInstanceForm.PortEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Enable OK/Clear buttons in case
  CheckBtnState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if (ssCtrl in Shift) and FAllowConnectionSkip then
  begin
    CancelBtn.Caption := _('Skip');
    CancelBtn.Hint := _('Skip the connection dialog and open service configuration.');

    SkipConnecting := True;
  end;

  if Key = VK_F1 then
    ShowHelp;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if not (ssCtrl in Shift) then
  begin
    CancelBtn.Caption := _('Cancel');
    CancelBtn.Hint := _('Click this button to cancel the connection.');

    SkipConnecting := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.ConnectionCBoxSelect(Sender: TObject);

begin
  if ConnectionCBox.ItemIndex > -1 then
  begin
    User_Connection := TMYX_USER_CONNECTION(favorite_conns[ConnectionCBox.ItemIndex]);
    GetValuesFromConnection(User_Connection);
  end;
  CheckBtnState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.TntFormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);

begin
  if (ActiveControl = UsernameCBox) or
    (ActiveControl = PasswordEd) then
  begin
    if (ConnectionCBox.ItemIndex = -1) and
      (ConnectionCBox.Items.Count>0) then
      ConnectionCBox.ItemIndex := 0
    else
      if (WheelDelta > 0) and
        (ConnectionCBox.ItemIndex < ConnectionCBox.Items.Count-1) then
        ConnectionCBox.ItemIndex := ConnectionCBox.ItemIndex + 1
      else
        if (WheelDelta < 0) and
          (ConnectionCBox.ItemIndex > 0) then
          ConnectionCBox.ItemIndex := ConnectionCBox.ItemIndex - 1;

    Handled := True;

    ConnectionCBoxCloseUp(self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.UsernameCBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  // The user name combo box has a special style (csSimple) which breaks the default handling of certain keys.
  // Hence we have to simulate that here.
  case Key of
    VK_ESCAPE:
      CancelBtn.Click;
    VK_RETURN:
      OKBtn.Click;
  end;

  CheckBtnState;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TConnectToInstanceForm.CheckForDuplicateConnections;

var
  I: Integer;
  FoundDuplicate: Boolean;
  List: TWideStringList;
  Name: WideString;

begin
  List := TWideStringList.Create;
  List.Sorted := True;
  FoundDuplicate := False;
  Name := '';
  for I := 0 to stored_conns.user_connections.Count - 1 do
    with stored_conns.user_connections[I] do
    begin
      if (connection_type = MYX_MYSQL_CONN) and (storage_type = MYX_FAVORITE_USER_CONNECTION)  then
      begin
        Name := connection_name;
        if connection_name <> '' then
        begin
          FoundDuplicate := List.IndexOf(connection_name) > -1;
          if FoundDuplicate then
            Break
          else
            List.Add(connection_name);
        end;
      end;
    end;

  if FoundDuplicate then
  begin
    ShowModalDialog(_('Warning'), Format(_('A duplicate connection name was found: "%s".'#13#10 +
      'Please use the connection manager and adjust your connection settings in order to avoid trouble e.g. with ' +
      'backup and restore.'), [Name]), myx_mtWarning);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
