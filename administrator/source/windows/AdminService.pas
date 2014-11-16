unit AdminService;

interface

uses Classes, ComCtrls, Registry, Windows, SysUtils, StrUtils, WinSvc,
  myx_admin_public_interface, AuxFuncs, Contnrs, TntRegistry, TntComCtrls,
  TntClasses;

const
  ServicesImageIndex = 25;

type
  TMySQLService = class(TObject)
  public
    ExistingService: Boolean;
    StartType: integer;
    ServiceName: WideString;
    DisplayName: WideString;
    Description: WideString;
    ConfigFile: WideString;
    ConfigFileSection: WideString;
    Binary: WideString;
    PathToBinary: WideString;
    Status: Integer;
    ErrorLog: WideString;
    logfile_size: Longword;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

procedure ScanForServices(TreeView: TTntTreeView; Icon: Integer; ObjectList: TObjectList = nil);
function SelectService(TreeView: TTntTreeView; const ServiceName: WideString): Integer;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

procedure ScanForServices(TreeView: TTntTreeView; Icon: integer; ObjectList: TObjectList);

var
  ServiceReg: TTntRegistry;
  KeyInfo: TRegKeyInfo;
  SubKeyNames: TTntStringList;
  i,j: integer;
  ImagePath, s, s1: WideString;
  tmp: WideString;
  tokenList: TTntStringList;
  MySQLService: TMySQLService;
  errornr: MYX_ADMIN_LIB_ERROR;
  errorlog: WideString;

begin
  ServiceReg:=TTntRegistry.Create;
  SubKeyNames:=TTntStringList.Create;
  try
    ServiceReg.RootKey := HKEY_LOCAL_MACHINE;
    ServiceReg.Access := KEY_ENUMERATE_SUB_KEYS + KEY_QUERY_VALUE;

    ServiceReg.OpenKey('SYSTEM\CurrentControlSet\Services\', False);

    ServiceReg.GetKeyInfo(KeyInfo);
    ServiceReg.GetKeyNames(SubKeyNames);

    ServiceReg.CloseKey;

    for i:=0 to SubKeyNames.Count-1 do
    begin
      ServiceReg.OpenKey('SYSTEM\'+
        'CurrentControlSet\Services\'+SubKeyNames[i], False);

      try
        ImagePath:=ServiceReg.ReadString('ImagePath');
      except
        ImagePath:='';
      end;

      //Check if mysqld is in ImagePath
      if(Pos('mysqld', LowerCase(ImagePath))>0)then
      begin
        MySQLService:=TMySQLService.Create;
        MySQLService.ExistingService:=True;
        //ServiceList.Add(MySQLService);

        MySQLService.ServiceName:=SubKeyNames[i];
        //ReadString returns an exception if  the registry entry contains
        //something other than a string
        try
          MySQLService.DisplayName:=ServiceReg.ReadString('DisplayName');
        except
          MySQLService.DisplayName:= '';
        end;
        try
          MySQLService.Description:=ServiceReg.ReadString('Description');
        except
          MySQLService.Description:= '';
        end;

        try
          MySQLService.StartType:=ServiceReg.ReadInteger('Start');
        except
          MySQLService.StartType:=3;
        end;

        // Typical ImagePath:
        // c:\programme\mysql\bin\mysqld-max-nt --defaults-file="C:\Programme\MySQL\my.cnf" MySQL
        // c:\program files\mysql\bin\mysqld-max-nt --defaults-file="C:\Programme\MySQL\my.cnf" MySQL

        // Parse the image path.
        MySQLService.ConfigFile := '';
        MySQLService.ConfigFileSection := '';

        tokenList  :=  TTntStringList.Create();
        try
          ParseCommandLine(ImagePath, tokenList);

          for j := 0 to tokenList.count-1 do
          begin
            if (j=0) then
            begin
              tmp :=  AnsiDequotedStr(tokenList[j], '"');
              MySQLService.PathToBinary := ExtractFilePath(tmp);
              MySQLService.Binary := Trim(ExtractFileName(tmp));
              if(CompareText(Copy(MySQLService.Binary, Length(MySQLService.Binary)-3, 4),
                '.exe')=0)then
                MySQLService.Binary := Copy(MySQLService.Binary, 1, Length(MySQLService.Binary)-4);
            end
            else if (AnsiStartsStr('--defaults-file=', tokenList[j])) then
            begin
              tmp :=  Copy(tokenList[j],Length('--defaults-file=')+1,
                         Length(tokenList[j])-Length('--defaults-file='));
              MySQLService.ConfigFile :=  AnsiDequotedStr(tmp, '"');
            end
            else
              if not AnsiStartsStr('--', tokenList[j]) then
              with MySQLService do
              begin
                ConfigFileSection :=  AnsiDequotedStr(tokenList[j], '"');

                // Sanity check: make sure we don't read from [client] or [mysql].
                if WideSameText(ConfigFileSection, 'mysql') or WideSameText(ConfigFileSection, 'client') then
                  ConfigFileSection := 'mysqld';
              end;
          end;
        finally
          tokenList.Free();
        end;

        //if the ConfigFile isn't specified in the Registry
        //it can be c:\my.cnf or windows-directory\my.ini
        if(MySQLService.ConfigFile='')then
        begin
          s := GetWindowsDir;

          if(FileExists(s+'my.ini'))then
            MySQLService.ConfigFile := s+'my.ini'
          else if(FileExists('c:\my.cnf'))then
            MySQLService.ConfigFile := 'c:\my.cnf'
          else
          begin
            //If there is no default cnf file yet,
            //use PathToBinaries\my.cnf
            s1 := IncludeTrailingPathDelimiter(
              Copy(MySQLService.PathToBinary, 1, Pos('\bin', MySQLService.PathToBinary)-1));
            if(s<>'')then
              MySQLService.ConfigFile := s1+'my.cnf'
            else
              MySQLService.ConfigFile := s+'my.ini';
          end;
        end;

        //if the ConfigFileSection isn't specified in the Registry
        //it is mysqld
        if(MySQLService.ConfigFileSection='')then
          MySQLService.ConfigFileSection := 'mysqld';


        //-----------------------------------------------------------------
        //Try to find errorfile
        MySQLService.ErrorLog := '';

        //first see if error-log is specified in the configfile
        if ( (MySQLService.ConfigFile<> '') and fileExists(Mysqlservice.ConfigFile) ) then
        begin
          ErrorLog := myx_get_cnf_value(MySQLService.ConfigFile,
                                       mysqlservice.configfilesection,
                                       'log-error', @errornr);
          if (errorlog <> '') then
            MySQLService.ErrorLog :=  errorlog;
        end;

        //secondly make some guesses
        if (MySQLService.ConfigFile = '') then
        begin
          s := GetLocalHostName;


          if(FileExists(GetProgramFilesDir+'MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog := GetProgramFilesDir+'MySQL\data\'+s+'.err'
          else if(FileExists('C:\MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog := 'C:\MySQL\data\'+s+'.err'
          else if(FileExists('D:\MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog := 'D:\MySQL\data\'+s+'.err';
        end;
        MySQLService.logfile_size := 0;

        //-----------------------------------------------------------------
        //Get Service Status
        MySQLService.Status := ServiceStatus('', MySQLService.ServiceName);

        if(TreeView<>nil)then
          AddTreeViewChildNode(TreeView, nil, MySQLService.ServiceName,
            Icon+Ord(MySQLService.Status<>SERVICE_RUNNING), MySQLService);

        if(ObjectList<>nil)then
          ObjectList.Add(MySQLService);
      end;

      ServiceReg.CloseKey;
    end;

  finally
    SubKeyNames.Free;
    ServiceReg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function SelectService(TreeView: TTntTreeView; const ServiceName: WideString): Integer;

// Select the node in the given treeview which corresponds to the given service name.
// Result is the index of the service entry or -1 if not found.
 
var
  I: Integer;

begin
  Result := -1;
  with TreeView do
  begin
    for I := 0 to Items.Count - 1 do
      if Assigned(Items[I].Data) then
        if WideSameText(ServiceName, TMySQLService(Items[I].Data).ServiceName) then
        begin
          Result := I;
          Selected := Items[I];
          Break;
        end;
  end;
end;

//----------------- TMySQLService --------------------------------------------------------------------------------------

constructor TMySQLService.Create;

begin
  // For debugging.
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TMySQLService.Destroy;

begin
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
