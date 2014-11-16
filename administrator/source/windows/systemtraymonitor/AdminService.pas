unit AdminService;

interface

uses Classes, ComCtrls, Registry, Windows, SysUtils, StrUtils, WinSvc,
  AuxFuncs, Contnrs, TntComCtrls, TntRegistry, TntClasses;

type
  TMySQLService = class(TObject)
    ExistingService: Boolean;
    StartType: integer;
    ServiceName: WideString;
    DisplayName: WideString;
    Description: WideString;
    ConfigFile: WideString;
    ConfigFileSection: WideString;
    ServerType: WideString;
    PathToBinary: WideString;
    Status: integer;
    ErrorLog: WideString;
    logfile_size: Longword;
  end;


procedure ScanForServices(TreeView: TTntTreeView; Icon: integer;
  ObjectList: TObjectList = nil; OnlyServicesStartingWithMySQL: Boolean = False);

implementation

procedure ScanForServices(TreeView: TTntTreeView; Icon: integer;
  ObjectList: TObjectList; OnlyServicesStartingWithMySQL: Boolean);
var ServiceReg: TTntRegistry;
  KeyInfo: TRegKeyInfo;
  SubKeyNames: TTntStringList;
  i,j: integer;
  ImagePath, s, s1: WideString;
  tmp: WideString;
  tokenList: TTntStringList;
  MySQLService: TMySQLService;
begin
  ServiceReg:=TTntRegistry.Create(KEY_READ);
  SubKeyNames:=TTntStringList.Create;
  try
    ServiceReg.RootKey:=HKEY_LOCAL_MACHINE;

    ServiceReg.OpenKey('SYSTEM\'+
      'CurrentControlSet\Services', False);

    ServiceReg.GetKeyInfo(KeyInfo);
    ServiceReg.GetKeyNames(SubKeyNames);

    ServiceReg.CloseKey;

    for i:=0 to SubKeyNames.Count-1 do
    begin
      if(OnlyServicesStartingWithMySQL)then
        if(CompareText(Copy(SubKeyNames[i], 1, 5),
          'mysql')<>0)then
          continue;

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
        MySQLService.DisplayName:=ServiceReg.ReadString('DisplayName');
        MySQLService.Description:=ServiceReg.ReadString('Description');

        try
          MySQLService.StartType:=ServiceReg.ReadInteger('Start');
        except
          MySQLService.StartType:=3;
        end;

        //Typical ImagePath:
        //c:\programme\mysql\bin\mysqld-max-nt --defaults-file="C:\Programme\MySQL\my.cnf" MySQL

        //parse the imagePath
        MySQLService.ConfigFile:='';
        MySQLService.ConfigFileSection:='';

        tokenList:= TTntStringList.Create;
        try
          ParseCommandLine(ImagePath, tokenList);

          for j:=0 to tokenList.count-1 do
          begin
            if (j=0) then
            begin
              tmp:= AnsiDequotedStr(tokenList[j], '"');
              MySQLService.PathToBinary:=ExtractFilePath(tmp);
              MySQLService.ServerType:=ExtractFileName(tmp);
            end
            else if (AnsiStartsStr('--defaults-file=', tokenList[j])) then
            begin
              tmp:= Copy(tokenList[j],Length('--defaults-file=')+1,
                         Length(tokenList[j])-Length('--defaults-file='));
              MySQLService.ConfigFile:= AnsiDequotedStr(tmp, '"');
            end
            else if (NOT AnsiStartsStr('--', tokenList[j]) ) then
            begin
              MySQLService.ConfigFileSection:= tokenList[j];
            end;
          end;
        finally
          tokenList.Free;
        end;

        //if the ConfigFile isn't specified in the Registry
        //it can be c:\my.cnf or windows-directory\my.ini
        if(MySQLService.ConfigFile='')then
        begin
          s:=GetWindowsDir;

          if(FileExists(s+'my.ini'))then
            MySQLService.ConfigFile:=s+'my.ini'
          else if(FileExists('c:\my.cnf'))then
            MySQLService.ConfigFile:='c:\my.cnf'
          else
          begin
            //If there is no default cnf file yet,
            //use PathToBinaries\my.cnf
            s1:=IncludeTrailingPathDelimiter(
              Copy(MySQLService.PathToBinary, 1, Pos('\bin', MySQLService.PathToBinary)-1));
            if(s<>'')then
              MySQLService.ConfigFile:=s1+'my.cnf'
            else
              MySQLService.ConfigFile:=s+'my.ini';
          end;
        end;

        //if the ConfigFileSection isn't specified in the Registry
        //it is mysqld
        if(MySQLService.ConfigFileSection='')then
          MySQLService.ConfigFileSection:='mysqld';


        //-----------------------------------------------------------------
        //Try to find errorfile
        MySQLService.ErrorLog:='';

        //first see if error-log is specified in the configfile
        {if ( (MySQLService.ConfigFile<> '') and fileExists(Mysqlservice.ConfigFile) ) then
        begin
          PErrorLog:=myx_get_cnf_value(PChar(UTF8Encode(MySQLService.ConfigFile)),
                                       PChar(UTF8Encode(mysqlservice.configfilesection)),
                                       PChar(UTF8Encode('log-error')), @errornr);
          if (perrorlog <> nil) then
            MySQLService.ErrorLog:= perrorlog;
          myx_free_string(perrorlog);
        end;}

        //secondly make some guesses
        if (MySQLService.ConfigFile = '') then
        begin
          s:=GetLocalHostName;


          if(FileExists(GetProgramFilesDir+'MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog:=GetProgramFilesDir+'MySQL\data\'+s+'.err'
          else if(FileExists('C:\MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog:='C:\MySQL\data\'+s+'.err'
          else if(FileExists('D:\MySQL\data\'+s+'.err'))then
            MySQLService.ErrorLog:='D:\MySQL\data\'+s+'.err';
        end;
        MySQLService.logfile_size:=0;

        //-----------------------------------------------------------------
        //Get Service Status
        MySQLService.Status:=ServiceStatus('', MySQLService.ServiceName);

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


end.
