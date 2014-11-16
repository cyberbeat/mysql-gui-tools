unit JdbcDBConns;

interface

uses gnugettext, Classes, Contnrs, myx_public_interface,
  TntStdCtrls, SysUtils, TntClasses;

type
  TJdbcDriver = class
    DriverClassName: WideString;
    Caption: WideString;
    RevEngModule: WideString;
    MigrationModule: WideString;
    TransformationModule: WideString;
    ObjectPackageName: WideString;
    MigrationTarget: Boolean;

    constructor Create(DriverClassName: WideString;
      Caption: WideString;
      RevEngModule: WideString;
      MigrationModule: WideString;
      TransformationModule: WideString;
      ObjectPackageName: WideString;
      MigrationTarget: Boolean = False);
  end;

  TJdbcDBConn = class
    JdbcDriver: TJdbcDriver;
    JdbcConnString: WideString;
    Settings: TTntStringList;

    constructor Create(JdbcDriver: TJdbcDriver; JdbcConnString: WideString);
    destructor Destroy; override;
  end;

procedure BuildJdbcDrivers(var JdbcDriversList: TObjectList);


implementation

// -----------------------------------------------------------------------------

procedure BuildJdbcDrivers(var JdbcDriversList: TObjectList);

var
  JdbcDriver: TJdbcDriver;

begin
  if (JdbcDriversList=nil) then
    JdbcDriversList := TObjectList.Create;

  //Add Oracle driver
  JdbcDriver := TJdbcDriver.Create(
    'oracle.jdbc.OracleDriver', 'Oracle Database Server',
    'ReverseEngineeringOracle', 'MigrationOracle',  '',
    'db.oracle');
  JdbcDriversList.Add(JdbcDriver);

  //Add Access driver
  JdbcDriver:=TJdbcDriver.Create(
    'sun.jdbc.odbc.JdbcOdbcDriver', 'MS Access',
    'ReverseEngineeringAccess', 'MigrationAccess', '',
    'db');
  JdbcDriversList.Add(JdbcDriver);

  //Add MSSQL driver
  {JdbcDriver:=TJdbcDriver.Create(
    'com.microsoft.jdbc.sqlserver.SQLServerDriver', 'MS SQL Server 2000',
    'ReverseEngineeringMSSQL', 'MigrationMssql',
    'db.mssql');
  JdbcDriversList.Add(JdbcDriver);}

  //Add MySQL driver
  JdbcDriver:=TJdbcDriver.Create(
    'com.mysql.jdbc.Driver', 'MySQL Server',
    'ReverseEngineeringMysqlJdbc', 'MigrationGeneric',
    'TransformationMysql',
    'db.mysql', True);
  JdbcDriversList.Add(JdbcDriver);
end;

// -----------------------------------------------------------------------------

constructor TJdbcDriver.Create(DriverClassName: WideString; Caption: WideString;
  RevEngModule: WideString;  MigrationModule: WideString;
  TransformationModule: WideString;
  ObjectPackageName: WideString; MigrationTarget: Boolean = False);

begin
  inherited Create;

  self.DriverClassName := DriverClassName;
  self.Caption := Caption;
  self.RevEngModule := RevEngModule;
  self.MigrationModule := MigrationModule;
  self.ObjectPackageName := ObjectPackageName;
  self.TransformationModule := TransformationModule;
  self.MigrationTarget := MigrationTarget;
end;

// -----------------------------------------------------------------------------

constructor TJdbcDBConn.Create(JdbcDriver: TJdbcDriver; JdbcConnString: WideString);

begin
  inherited Create;

  self.JdbcDriver := JdbcDriver;
  self.JdbcConnString := JdbcConnString;

  Settings := TTntStringList.Create;
end;

// -----------------------------------------------------------------------------

destructor TJdbcDBConn.Destroy;

begin
  Settings.Free;

  inherited;
end;

// -----------------------------------------------------------------------------

end.
