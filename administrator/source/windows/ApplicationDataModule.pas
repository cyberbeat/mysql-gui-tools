unit ApplicationDataModule;

interface

uses
  gnugettext, SysUtils, Classes, TntClasses, ComCtrls, ImgList,
  Controls, Forms, Contnrs, Windows,
  ExtCtrls, AuxFuncs, PNGImage, StdCtrls, StrUtils,
  myx_util_public_interface,
  myx_public_interface, myx_admin_public_interface,
  Options, MyxError, MySQLConnection, AdminService,
  TntForms, TntSysUtils;

type
  TMYXAdminOptions = class;

  TApplicationDM = class(TDataModule)
    ServiceStatusImageList: TImageList;
    AdminTree16ImageList: TImageList;
    AdminTree24ImageList: TImageList;
    Admin48ImageList: TImageList;
    CatalogImageList: TImageList;
    SectionImageList: TImageList;
    ItemSelectImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FOptionProvider: IOptionProvider;
    FCommonOptions: IOptionProvider;
    FFreeService: Boolean;  // True if the DM has created a local service class. This must be freed on DM desctruction.
    FCurrentService: TMySQLService;

    function GetConfigValue(const Key: WideString): WideString;
    function GetDataDir: WideString;
    function GetOptionProvider: IOptionProvider;
    procedure SetCurrentService(const Value: TMySQLService);
  public
    ApplicationIsTerminating: Boolean;

    Options: TMYXAdminOptions;
    CurrentConnection: TMySQLConn;

    procedure CheckCommandlineParameter;
    function CheckFiles: Boolean;
    function GetLastFileDialogPaths(DlgName: WideString): WideString;
    function GetPathFromConfig(const Key: WideString): WideString;
    procedure LoadOptions;
    procedure OnApplicationException(Sender: TObject; E: Exception);
    procedure SetLastFileDialogPaths(DlgName: WideString; Path: WideString);
    procedure MakeBackup;
    function PrepareConnection: Integer;

    property ConfigValue[const S: WideString]: WideString read GetConfigValue;
    property CurrentService: TMySQLService read FCurrentService write SetCurrentService;
    property DataDir: WideString read GetDataDir;
    property OptionProvider: IOptionProvider read GetOptionProvider;
  end;

  TMYXAdminOptions = class(TMyxOptions)
  private
    FCommonOptions: IOptionProvider;
  public
    constructor Create(const ApplicationID: string); override;
    destructor Destroy; override;

    procedure LoadOptions; override;
    procedure StoreOptions; override;
  public
    ShowOnlyServiceSections: Boolean;
    MySQLInstance: WideString;
    MySQLInstallPath: WideString;
    MySQLVersion: WideString;

    ShowUserGlobalPrivileges: Boolean;
    ShowUserTableColumnPrivileges: Boolean;

    StartSection: Integer;
    SectionSidebarWidth: Integer;
    SectionSidebarHidden: Boolean;

    UsePeakLevel: Boolean;
    ResetPeakLevel: Boolean;
    PeakLevelResetTicks: Integer;

    LastFileDialogPaths: TTntStringList;

    BackupProfile: WideString;
    BackupTargetPath: WideString;
    BackupPrefix: WideString;

    AddDateTimeToBackupFiles: Boolean;
    WriteBackupLog: Boolean;
    BackupLogDir: WideString;
    BackupLogEntryAfterRows: Integer;
  end;

  EMyxAdminLibError = class(EMyxLibraryError)
  protected
    function GetFormattedMessage: WideString; override;
  end;

var
  ApplicationDM: TApplicationDM;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  Main, ConnectToInstance, AuxAdminBackupRestore;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.DataModuleCreate(Sender: TObject);

var
  S: WideString;
  
begin
  //Exception Handling
  Application.OnException := OnApplicationException;

  ApplicationIsTerminating := False;

  //DLL Version check
  S := '';
  if (libmysqlx_PUBLIC_INTERFACE_VERSION <> myx_get_public_interface_version) then
    S := Format(_('There is a incompatible version of the ' +
      'library %s installed (Version %s). Please update the library to version %s.'),
      ['libmysqlx.dll', FormatLibraryVersion(myx_get_public_interface_version),
      FormatLibraryVersion(libmysqlx_PUBLIC_INTERFACE_VERSION)]) + #13#10#13#10;

  if (libmysqladmin_PUBLIC_INTERFACE_VERSION <> myx_get_admin_public_interface_version) then
    S := Format(_('There is a incompatible version of the ' +
      'library %s installed (Version %s). Please update the library to version %s.'),
      ['libmysqladmin.dll', FormatLibraryVersion(myx_get_admin_public_interface_version),
      FormatLibraryVersion(libmysqladmin_PUBLIC_INTERFACE_VERSION)]) + #13#10#13#10;

  if (S <> '') then
    if (ShowModalDialog(_('Library version mismatch'),
      Trim(S), myx_mtError, _('Quit') + #13#10 + _('Ignore')) = 1) then
    begin
      ApplicationIsTerminating := True;
      Application.Terminate;
    end;

  // Keep a reference to the global options to make sure it is never freed before the application options are freed.
  FCommonOptions := MYXCommonOptionProvider;

  CurrentConnection := TMySQLConn.Create(nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.DataModuleDestroy(Sender: TObject);

begin
  if FFreeService then
    FCurrentService.Free;
    
  CurrentConnection.Free;

  // We don't need to free the options class. By setting the option provider to nil the ref count is correctly
  // decremented and the class is freed when the ref count reaches zero.
  Options := nil;
  FOptionProvider := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.CheckCommandlineParameter;

var
  I: Integer;
  S: string;

begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    
    //Start in serviceconfig mode
    if (CompareText(S, '-serviceconfig') = 0) then
    begin
      ApplicationDM.Options.ShowOnlyServiceSections := True;

      // Create a dummy service entry just like PrepareConnection does.
      // It is used to do an initial enable/disable call on the  service control page.
      FCurrentService := TMySQLService.Create;
      FFreeService := True;
    end;

    //Set MySQL installation path
    if (CompareText(Copy(S, 1, 13), '-installpath=') = 0) then
      ApplicationDM.Options.MySQLInstallPath :=
        IncludeTrailingPathDelimiter(Copy(ParamStr(i), 14, Length(S)));

    //Select the start section
    if (CompareText(Copy(S, 1, 14), '-startsection=') = 0) then
      ApplicationDM.Options.StartSection := StrToIntDef(Copy(S, 15, Length(ParamStr(i))), 1);

    //User settings Data directory
    if (Copy(S, 1, 3) = '-UD') then
    begin
      MYXCommonOptions.UserDataDir := Copy(ParamStr(i), 4, Length(ParamStr(i)));

      MYXCommonOptions.LoadOptions;
      ApplicationDM.LoadOptions;
    end;

    //MySQLInstance
    if (CompareText(Copy(ParamStr(i), 1, 10), '-instance=') = 0) then
      ApplicationDM.Options.MySQLInstance := Copy(S, 11, Length(ParamStr(i)));

    //Backups
    if (Copy(S, 1, 3) = '-bp') then
      ApplicationDM.Options.BackupProfile := Copy(S, 4, Length(ParamStr(i)));
    if (Copy(S, 1, 3) = '-bt') then
      ApplicationDM.Options.BackupTargetPath := Copy(S, 4, Length(ParamStr(i)));
    if (Copy(S, 1, 3) = '-bx') then
      ApplicationDM.Options.BackupPrefix := Copy(S, 4, Length(ParamStr(i)));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.OnApplicationException(Sender: TObject; E: Exception);

begin
  if (not (Application.Terminated)) then
    ShowError(E);
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.PrepareConnection: Integer;

begin
  if FCurrentService = nil then
  begin
    FCurrentService := TMySQLService.Create;
    FFreeService := True;
  end;

  Result := CurrentConnection.ConnectToServer(True);
  if (Result = 1) and Assigned(CurrentConnection.UserConnection) then
  begin
    Options.MySQLVersion := IntToStr(CurrentConnection.MajorVersion) + '.' + IntToStr(CurrentConnection.MinorVersion);

    if CurrentConnection.IsLocalServer then
    begin
      FCurrentService.ServiceName := myx_get_running_service_name(CurrentConnection.UserConnection.port);
      FCurrentService.ConfigFile := myx_get_running_service_config_file(CurrentConnection.UserConnection.port);
      if (FCurrentService.ServiceName = '') or not FileExists(FCurrentService.ConfigFile) then
      begin
        ShowModalDialog(_('Could not find settings'), _('Either the server service or the configuration file could not ' +
          'be found. Startup variables and service section are therefore disabled.'), myx_mtError, _('OK'));
        CurrentConnection.IsLocalServer := False; // Disables the possibility to edit startup vars or the server service.
      end;
    end;
  end
  else
  begin
    if Result = -1 then
      Options.ShowOnlyServiceSections := True;
    FCurrentService.ServiceName := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.CheckFiles: Boolean;

var
  MissingFiles: WideString;

begin
  CheckFiles := True;

  MissingFiles := '';

  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqlx_dbm_charsets.xml'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqlx_dbm_charsets.xml' + #13#10;
  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqlx_dbm_datatypes.xml'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqlx_dbm_datatypes.xml' + #13#10;

  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.xml' + #13#10;
  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqladmin_status_variables.xml'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqladmin_status_variables.xml' + #13#10;
  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqladmin_system_variables.xml'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqladmin_system_variables.xml' + #13#10;
  if (not (FileExists(MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.dtd'))) then
    MissingFiles := MissingFiles +
      MYXCommonOptions.XMLDir + 'mysqladmin_startup_variables_description.dtd' + #13#10;

  if (MissingFiles <> '') then
  begin
    ShowModalDialog(_('Files missing!'),
      _('Some vital files cannot be found. ') + #13#10 +
      _('Please check the file path and the existence of the following files:') + #13#10#13#10 +
      MissingFiles);

    CheckFiles := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.LoadOptions;

begin
  // Load Options
  Options := TMYXAdminOptions.Create('administrator');
  FOptionProvider := Options; // This increases the reference count to 1. Read more in MySQLAdministrator.dpr
                              // why we need that.
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.GetConfigValue(const Key: WideString): WideString;

// Returns the value of the requested entry in the current config file (if there is one).
// If no config file is set or the key does not exist or consists only of its name without an assignment
// the result value is an empty string.

var
  ErrorNumber: MYX_ADMIN_LIB_ERROR;

begin
  Result := '';

  if Assigned(FCurrentService) and (FCurrentService.ConfigFile <> '') then
    Result := myx_get_cnf_value(FCurrentService.ConfigFile, 'mysqld', Key, @ErrorNumber);
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.GetDataDir: WideString;

begin
  Result := ConfigValue['datadir'];

  if (Result = '') and Assigned(FCurrentService) then
  begin
    // If no data folder could be found then we might have a corrupted or non-existing ini file.
    // Use the binary folder instead (as is the default in the server).
    Result := FCurrentService.PathToBinary + '..\data';
  end;
  Result := WideIncludeTrailingPathDelimiter(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.GetOptionProvider: IOptionProvider;

begin
  // Return the provider interface implemented in the application option.
  Result := Options;
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.GetLastFileDialogPaths(DlgName: WideString): WideString;

begin
  Result := Options.LastFileDialogPaths.Values['DlgName'];
end;

//----------------------------------------------------------------------------------------------------------------------

function TApplicationDM.GetPathFromConfig(const Key: WideString): WideString;

// Reads the option's value with the given name from the config file as a path.
// If the option does not exist or has no value then an empty string is returned.

begin
  Result := ConfigValue[Key];

  // Check if it is an absolute path. If Result is 'checked' then we have a name only
  // option and pass it along unmodified.
  if not ((Length(Result) > 2) and (Result[2] = ':')) and (Result <> 'checked') then
  begin
    // Add the server's data dir to make it an absolute path.
    // This is how the server interprets relative pathes.
    Result := DataDir + Result;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.SetCurrentService(const Value: TMySQLService);

begin
  if FCurrentService <> Value then
  begin
    if FFreeService then
    begin
      FFreeService := False;
      FCurrentService.Free;
    end;
    FCurrentService := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.SetLastFileDialogPaths(DlgName: WideString; Path: WideString);

begin
  Options.LastFileDialogPaths.Values['DlgName'] := Path;
end;

//----------------------------------------------------------------------------------------------------------------------

function BackupProgress(current_table_name: PChar;
  num_tables: Integer; num_tables_processed: Integer;
  num_rows: Integer; num_rows_processed: Integer; user_data: Pointer): Integer; cdecl;
var
  PSender: ^TApplicationDM;
  BackupLogFilename: WideString;
  S: string;
begin
  PSender := user_data;

  with PSender.Options do
  begin
    BackupLogFilename := IncludeTrailingPathDelimiter(
      BackupLogDir) + 'MySQLAdminBackupLog.txt';

    if (BackupLogEntryAfterRows > 0) then
      S := StringAlignLeft(current_table_name, 30) + ' ' +
        StringAlignLeft(
        '(' + IntToStr(num_tables_processed) + '/' + IntToStr(num_tables) + ')', 11) +
        ' | ' +
        IntToStr(num_rows_processed) + '/' + IntToStr(num_rows) + #13#10
    else
      S := StringAlignLeft(current_table_name, 30) +
        ' (' + IntToStr(num_tables_processed) + '/' + IntToStr(num_tables) + ')' + #13#10;

    AddToFile(BackupLogFilename, S);
  end;

  Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TApplicationDM.MakeBackup;

var
  i: Integer;
  user_conns: PMYX_USER_CONNECTIONS;
  stored_conns: TMYX_USER_CONNECTIONS;
  user_conn: TMYX_USER_CONNECTION;
  error: MYX_LIB_ERROR;
  PMySQL: Pointer;
  BackupError: MYX_BACKUP_ERROR;
  error_code: MYX_ADMIN_LIB_ERROR;
  Profile: PMYX_BACKUP_PROFILE;
  fname, ErrorTxt: WideString;
  BackupLogFilename: WideString;

begin
  try
    SendToEventLog(Format(_('MySQL Administrator is preparing backup for profile %s.'),
      [Options.BackupProfile]), EVENTLOG_INFORMATION_TYPE);
  except
    on Exception do begin end;
  end;

  BackupLogFilename := IncludeTrailingPathDelimiter(
    Options.BackupLogDir) + 'MySQLAdminBackupLog.txt';

  if (Options.WriteBackupLog) then
    AddToFile(BackupLogFilename, StringOfChar('-', 40) + #13#10 +
      Format(_('%s - Backup started.' + #13#10 +
      'Preparing backup for for profile %s.'),
      [FormatDateTime('yyyy-mm-dd hh:nn', Now),
      Options.BackupProfile]) + #13#10);

  //Get connection
  //Fetch connections from library
  user_conns := myx_load_user_connections(
    MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml', @error);
  if (error <> MYX_NO_ERROR) then
  begin
    try
      SendToEventLog(Format(_('Error while loading stored connections from %s. Error Number %d.'),
        [MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml', Ord(error)]), EVENTLOG_ERROR_TYPE);
    except
      on Exception do begin end;
    end;

    if (Options.WriteBackupLog) then
      AddToFile(BackupLogFilename, StringOfChar('-', 40) + #13#10 +
        Format(_('Error while loading stored connections from %s. Error Number %d.'),
        [MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml', Ord(error)]) + #13#10);

    Exit;
  end;

  if (Options.WriteBackupLog) then
    AddToFile(BackupLogFilename,
      Format(_('Connections loaded from file %s.'),
      [MYXCommonOptions.UserDataDir + 'mysqlx_user_connections.xml']) + #13#10);

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

      if (Options.WriteBackupLog) then
        AddToFile(BackupLogFilename,
          Format(_('Connection %s selected.'),
          [user_conn.connection_name]) + #13#10);

      if (user_conn = nil) then
      begin
        try
          SendToEventLog(Format(_('Connection %s cannot be found.'),
            [MYXCommonOptions.ConnectionToUse]), EVENTLOG_ERROR_TYPE);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename, StringOfChar('-', 40) + #13#10 +
            Format(_('Connection %s cannot be found.'),
            [MYXCommonOptions.ConnectionToUse]) + #13#10);

        Exit;
      end;

      //Connect to Server
      PMySQL := myx_mysql_init();
      if (PMySQL = nil) then
      begin
        try
          SendToEventLog(_('Error while allocating memory for MySQL Struct.'),
            EVENTLOG_ERROR_TYPE);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename,
            _('Error while allocating memory for MySQL Struct.') + #13#10);

        Exit;
      end;

      if (myx_connect_to_instance(
        user_conn.get_record_pointer, PMySQL) <> 0) then
      begin
        try
          SendToEventLog(Format(_('Cannot connect to MySQL Server. %s (Error Number %d)'),
            [myx_mysql_error(PMySQL), myx_mysql_errno(PMySQL)]),
            EVENTLOG_ERROR_TYPE);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename,
            Format(_('Cannot connect to MySQL Server. %s (Error Number %d)'),
            [myx_mysql_error(PMySQL), myx_mysql_errno(PMySQL)]) + #13#10);

        Exit;
      end;

      if (Options.WriteBackupLog) then
        AddToFile(BackupLogFilename,
          Format(_('Connection to server %s:%d established.'),
          [user_conn.hostname, user_conn.port]) + #13#10);

      if (Pos('.MBP', Uppercase(Options.BackupProfile)) <= 0) then
        Options.BackupProfile := Options.BackupProfile + '.mbp';

      if (Pos(':', Options.BackupProfile) > 0) then
        Profile := myx_load_profile(ExtractFileName(MYXCommonOptions.UserDataDir + Options.BackupProfile),
          ExtractFilePath(Options.BackupProfile), @error_code)
      else
        Profile := myx_load_profile(ExtractFileName(MYXCommonOptions.UserDataDir + Options.BackupProfile),
          ExtractFilePath(MYXCommonOptions.UserDataDir + Options.BackupProfile), @error_code);

      if (error_code <> MYX_ADMIN_NO_ERROR) then
      begin
        try
          SendToEventLog(Format(_('Error while loading profile %s.'),
            [MYXCommonOptions.UserDataDir + Options.BackupProfile]),
            EVENTLOG_ERROR_TYPE);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename,
            Format(_('Error while loading profile %s.'),
            [MYXCommonOptions.UserDataDir + Options.BackupProfile]) + #13#10);

        Exit;
      end;

      if (Options.WriteBackupLog) then
        AddToFile(BackupLogFilename,
          Format(_('Profiled %s loaded.'),
          [MYXCommonOptions.UserDataDir + Options.BackupProfile]) + #13#10);

      if (Options.AddDateTimeToBackupFiles) then
      begin
        if (Copy(Options.BackupPrefix, Length(Options.BackupPrefix), 1) = '_') then
          fname := IncludeTrailingPathDelimiter(Options.BackupTargetPath) +
            Options.BackupPrefix + FormatDateTime('yyyymmdd hhmm', Now) + '.sql'
        else
          fname := IncludeTrailingPathDelimiter(Options.BackupTargetPath) +
            Options.BackupPrefix + ' ' + FormatDateTime('yyyymmdd hhmm', Now) + '.sql';
      end
      else
        fname := IncludeTrailingPathDelimiter(Options.BackupTargetPath) +
          Options.BackupPrefix + '.sql';

      try
        SendToEventLog(Format(_('MySQL Administrator is starting backup for profile %s.'),
          [Options.BackupProfile]), EVENTLOG_INFORMATION_TYPE);
      except
        on Exception do begin end;
      end;

      try
        if (Options.WriteBackupLog) then
        begin
          AddToFile(BackupLogFilename, #13#10 +
            _('Starting backup...') + #13#10#13#10);

          if (Options.BackupLogEntryAfterRows = 0) then
            BackupError := myx_make_backup_with_profile(PMySQL, Profile, fname,
              2000000, BackupProgress, Addr(self))
          else
            BackupError := myx_make_backup_with_profile(PMySQL, Profile, fname,
              Options.BackupLogEntryAfterRows, BackupProgress, Addr(self));
        end
        else
          BackupError := myx_make_backup_with_profile(PMySQL, Profile, fname,
            10000, BackupProgress, Addr(self));

      finally
        myx_free_profile(Profile);
      end;

      if (BackupError <> MYX_BACKUP_NO_ERROR) then
      begin
        ErrorTxt := myx_get_backup_error_string(BackupError);
        case BackupError of
          MYX_BACKUP_SERVER_ERROR:
            ErrorTxt := Format(ErrorTxt, [myx_mysql_errno(PMySQL), myx_mysql_error(PMySQL)]);
          MYX_BACKUP_CANT_OPEN_FILE:
            ErrorTxt := Format(ErrorTxt, [fname]);
          MYX_BACKUP_OUTPUTDEVICE_FULL:
            ErrorTxt := Format(ErrorTxt, [fname]);
        end;

        try
          SendToEventLog(ErrorTxt, EVENTLOG_ERROR_TYPE);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename, #13#10 +
            Format(_('ERROR: %s - The following error occured: ' + #13#10 + '%s'),
            [FormatDateTime('yyyy-mm-dd hh:nn', Now),
            MYXCommonOptions.UserDataDir + Options.BackupProfile]) + #13#10);

        myx_mysql_close(PMySQL);
      end
      else
      begin
        myx_mysql_close(PMySQL);

        try
          SendToEventLog(Format(_('Backup file %s written successfully.'),
            [fname]), EVENTLOG_SUCCESS);
        except
          on Exception do begin end;
        end;

        if (Options.WriteBackupLog) then
          AddToFile(BackupLogFilename, #13#10 +
            Format(_('%s - Backup written successfully.'),
            [FormatDateTime('yyyy-mm-dd hh:nn', Now)]) + #13#10);
      end;
    finally
      stored_conns.Free;
    end;
  finally
    myx_free_user_connections(user_conns);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxAdminLibError.GetFormattedMessage: WideString;

begin
  case MYX_ADMIN_LIB_ERROR(ErrorNr) of
    MYX_ADMIN_ERROR_CANT_OPEN_FILE:
      Result := Format(_('The File %s cannot be opened.'), [ErrorNrParam]);
    MYX_ADMIN_XML_PARSE_ERROR:
      Result := Format(_('An error occured while parsing the XML file %s.'), [ErrorNrParam]);
    MYX_ADMIN_XML_NO_VALID_DOCUMENT:
      Result := Format(_('An error occured while validating the XML file %s.'), [ErrorNrParam]);
    MYX_ADMIN_XML_EMPTY_DOCUMENT:
      Result := Format(_('The XML file %s is empty.'), [ErrorNrParam]);
    MYX_ADMIN_INI_PARSE_ERROR:
      Result := Format(_('An error occured while parsing the INI file %s.'), [ErrorNrParam]);
    MYX_ADMIN_GENERAL_ERROR:
      Result := _('A general error occured.');
    MYX_ADMIN_SQL_ERROR:
      Result := _('An SQL error occured.');
  else
    Result := _('An error occured.');
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TMYXAdminOptions.Create(const ApplicationID: string);

begin
  LastFileDialogPaths := TTntStringList.Create;

  // Keep a reference to the global options to avoid that they are released before the application optiones are gone.
  FCommonOptions := MYXCommonOptionProvider;

  inherited Create(ApplicationID);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMYXAdminOptions.Destroy;

begin
  inherited Destroy;

  LastFileDialogPaths.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMYXAdminOptions.LoadOptions;

var
  POptions: PMYX_APPLICATION_OPTIONS;
  Options: TMYX_APPLICATION_OPTIONS;
  I, J: Integer;
  error: MYX_LIB_ERROR;
  OptionGroupName, OptionName, OptionValue: WideString;
  ExePath: WideString;
  Provider: IOptionProvider;

begin
  ExePath := ExtractFilePath(Application.ExeName);
  Provider := Self;

  StartSection := 1;
  ShowOnlyServiceSections := False;

  MySQLInstallPath := '';

  if (GetDriveType('D:\') = DRIVE_FIXED) and (DirectoryExists('d:\mysql')) then
    MySQLInstallPath := 'd:\mysql\';

  if MySQLInstallPath = '' then
    MySQLInstallPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

  // Set a default value. When a connection is established this value is reset.
  MySQLVersion := '5.0';

  // Update from 1.0.1a to 1.0.2a.
  if FileExists(MYXCommonOptions.UserDataDir + 'mysqlx_options.xml') then
    RenameFile(MYXCommonOptions.UserDataDir + 'mysqlx_options.xml', MYXCommonOptions.UserDataDir +
      'mysqlx_admin_options.xml');

  // Update from 1.0.3 to 1.0.4.
  if FileExists(MYXCommonOptions.UserDataDir + 'mysqlx_admin_options.xml') then
    RenameFile(MYXCommonOptions.UserDataDir + 'mysqlx_admin_options.xml', MYXCommonOptions.UserDataDir +
      'mysqladmin_options.xml');

  //Initialize Values
  SectionSidebarWidth := 185;
  SectionSidebarHidden := False;
  ShowUserGlobalPrivileges := False;
  ShowUserTableColumnPrivileges := False;

  UsePeakLevel := True;
  ResetPeakLevel := True;
  PeakLevelResetTicks := 30;

  LastFileDialogPaths.Text := '';

  AddDateTimeToBackupFiles := True;
  WriteBackupLog := False;
  BackupLogDir := 'C:\';
  BackupLogEntryAfterRows := 0;

  WindowPosList.Clear;

  // Read options file.
  if (FileExists(MYXCommonOptions.UserDataDir + 'mysqladmin_options.xml')) then
  begin
    POptions := myx_get_application_options(
      MYXCommonOptions.UserDataDir + 'mysqladmin_options.xml',
      @error);
    try
      if (error <> MYX_NO_ERROR) then
      begin
        ShowModalDialog(_('XML Error'), _('Error while loading Options file ''') + MYXCommonOptions.UserDataDir +
          'mysqladmin_options.xml' + ''''#13#10 + _('Error Nr.:' )+ IntToStr(Ord(error)), myx_mtError);
      end
      else
      begin
        Options := TMYX_APPLICATION_OPTIONS.Create(POptions);
        try
          for I := 0 to Options.option_groups.Count - 1 do
            for J := 0 to Options.option_groups[I].name_value_pairs.Count - 1 do
            begin
              OptionGroupName := Options.option_groups[I].name;
              OptionName := Options.option_groups[I].name_value_pairs[J].name;
              OptionValue := Options.option_groups[I].name_value_pairs[J].value;

              Provider.OptionAsString[OptionName] := OptionValue;
              if (CompareText(OptionGroupName, 'General') = 0) then
              begin
                if (CompareText(OptionName, 'LastFileDialogPaths') = 0) then
                  LastFileDialogPaths.Text := AnsiReplaceText(OptionValue, '´', #13#10)
                else
                  ;
              end
              else
                if (CompareText(OptionGroupName, 'GUISetup') = 0) then
                begin
                  if (CompareText(OptionName, 'SectionSidebarWidth') = 0) then
                    SectionSidebarWidth := StrToIntDef(OptionValue, 185)
                  else
                    if (CompareText(OptionName, 'SectionSidebarHidden') = 0) then
                      SectionSidebarHidden := (StrToIntDef(OptionValue, 0) = 1);
                end
                else
                  if (CompareText(OptionGroupName, 'AdminUserManagement') = 0) then
                  begin
                    if (CompareText(OptionName, 'ShowUserGlobalPrivileges') = 0) then
                      ShowUserGlobalPrivileges := (StrToIntDef(OptionValue, 0) = 1)
                    else
                      if (CompareText(OptionName, 'ShowUserTableColumnPrivileges') = 0) then
                        ShowUserTableColumnPrivileges := (StrToIntDef(OptionValue, 0) = 1);
                  end
                  else
                    if (CompareText(OptionGroupName, 'AdminHealthGraphs') = 0) then
                    begin
                      if (CompareText(OptionName, 'UsePeakLevel') = 0) then
                        UsePeakLevel := (StrToIntDef(OptionValue, 1) = 1)
                      else
                        if (CompareText(OptionName, 'ResetPeakLevel') = 0) then
                          ResetPeakLevel := (StrToIntDef(OptionValue, 1) = 1)
                        else
                          if (CompareText(OptionName, 'PeakLevelResetTicks') = 0) then
                            PeakLevelResetTicks := StrToIntDef(OptionValue, 30);
                    end
                    else
                      if (CompareText(OptionGroupName, 'AdminBackups') = 0) then
                      begin
                        if (CompareText(OptionName, 'AddDateTimeToBackupFiles') = 0) then
                          AddDateTimeToBackupFiles := (StrToIntDef(OptionValue, 0) = 1)
                        else
                          if (CompareText(OptionName, 'WriteBackupLog') = 0) then
                            WriteBackupLog := (StrToIntDef(OptionValue, 0) = 1)
                          else
                            if (CompareText(OptionName, 'BackupLogDir') = 0) then
                              BackupLogDir := OptionValue
                            else
                              if (CompareText(OptionName, 'BackupLogEntryAfterRows') = 0) then
                                BackupLogEntryAfterRows := StrToIntDef(OptionValue, 0);
                      end
                      else
                        if (CompareText(OptionGroupName, 'WindowPos') = 0) then
                        begin
                          WindowPosList.AddObject(OptionName, TMyxWindowPos.Create(OptionValue));
                        end;
            end;
        finally
          Options.Free;
        end;
      end;
    finally
      myx_free_application_options(POptions);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMYXAdminOptions.StoreOptions;

var
  Options: TMYX_APPLICATION_OPTIONS;
  OptionGroup: TMYX_OPTION_GROUP;
  ExePath: WideString;
  I: Integer;

begin
  if MYXCommonOptions.UserDataDir <> '' then
  begin
    ExePath := ExtractFilePath(Application.ExeName);

    // Create Application Options
    Options := TMYX_APPLICATION_OPTIONS.create;
    try
      StoreListOptions(Options.option_groups);

      OptionGroup := TMYX_OPTION_GROUP.create('General');
      Options.option_groups.Add(OptionGroup);

      AddParam(OptionGroup, 'LastFileDialogPaths', AnsiReplaceText(LastFileDialogPaths.Text, #13#10, '´'));

      OptionGroup := TMYX_OPTION_GROUP.create('GUISetup');
      Options.option_groups.Add(OptionGroup);

      AddParam(OptionGroup, 'SectionSidebarWidth', IntToStr(SectionSidebarWidth));
      AddParam(OptionGroup, 'SectionSidebarHidden', IntToStr(Ord(SectionSidebarHidden)));

      OptionGroup := TMYX_OPTION_GROUP.create('AdminUserManagement');
      Options.option_groups.Add(OptionGroup);

      AddParam(OptionGroup, 'ShowUserGlobalPrivileges', IntToStr(Ord(ShowUserGlobalPrivileges)));
      AddParam(OptionGroup, 'ShowUserTableColumnPrivileges', IntToStr(Ord(ShowUserTableColumnPrivileges)));

      OptionGroup := TMYX_OPTION_GROUP.create('AdminHealthGraphs');
      Options.option_groups.Add(OptionGroup);

      AddParam(OptionGroup, 'UsePeakLevel', IntToStr(Ord(UsePeakLevel)));
      AddParam(OptionGroup, 'ResetPeakLevel', IntToStr(Ord(ResetPeakLevel)));
      AddParam(OptionGroup, 'PeakLevelResetTicks', IntToStr(PeakLevelResetTicks));

      OptionGroup := TMYX_OPTION_GROUP.create('AdminBackups');
      Options.option_groups.Add(OptionGroup);

      AddParam(OptionGroup, 'AddDateTimeToBackupFiles', IntToStr(Ord(AddDateTimeToBackupFiles)));
      AddParam(OptionGroup, 'WriteBackupLog', IntToStr(Ord(WriteBackupLog)));
      AddParam(OptionGroup, 'BackupLogDir', BackupLogDir);
      AddParam(OptionGroup, 'BackupLogEntryAfterRows', IntToStr(BackupLogEntryAfterRows));

      OptionGroup := TMYX_OPTION_GROUP.create('WindowPos');
      Options.option_groups.Add(OptionGroup);

      // Store all window positions.
      for I := 0 to WindowPosList.Count - 1 do
        AddParam(OptionGroup, WindowPosList[I],
          TMyxWindowPos(WindowPosList.Objects[I]).AsWideString);

      // Save options to file.
      myx_store_application_options(Options.get_record_pointer, MYXCommonOptions.UserDataDir + 'mysqladmin_options.xml');
    finally
      Options.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

