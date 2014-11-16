unit MySQLConn2;

interface

uses
  MySQLConnection, myx_public_interface;

type
  TMySQLConn2 = class(TMySQLConn)

    function ConnectToServerWithConnection(var Conn: TMYX_USER_CONNECTION): Boolean;
  private
    function CheckConnectionData(Conn: TMYX_USER_CONNECTION): Boolean;
  end;

implementation

function TMySQLConn2.ConnectToServerWithConnection(
  var Conn: TMYX_USER_CONNECTION): Boolean;

var
  ConnRes: Integer;
  EnoughData: Boolean;

begin
  ConnRes := -1;
  Result := False;

  EnoughData := CheckConnectionData(Conn);
  if  EnoughData then // enough data to connect - dont show the dialog
  begin
    ConnRes := Connect(FMySQL, Conn, False);
    if(ConnRes = 1)then
      Result := True;
  end;

  // if autoconnectio failed or not enough data - show dialog
  if not EnoughData or (ConnRes <> 1)then
    ConnRes := ShowConnectToInstanceForm(Conn,
        FMySQL, True, False, True);

  if ConnRes = 1 then
  begin
    MySQLMajorVersion := myx_get_mysql_major_version(FMySQL);
    MySQLMinorVersion := myx_get_mysql_minor_version(FMySQL);

    Connected := True;
    ConnectedToLocalhost := (myx_is_localhost(Conn.hostname) = 1);
  end;
end;

function TMySQLConn2.CheckConnectionData(Conn: TMYX_USER_CONNECTION): Boolean;

begin
  Result :=
    (Length(Conn.username) > 0) and
    (Length(Conn.hostname) > 0) and
    (Conn.port > 0) and (Conn.port < 32768); 
end;

end.
