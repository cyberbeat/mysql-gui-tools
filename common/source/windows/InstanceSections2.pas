unit InstanceSections2;

interface

uses InstanceSections, Classes, Forms, Controls, ExtCtrls, Types,
  Graphics, Messages, ComCtrls, Sections, MySQLConnection, TntComCtrls;


type
  TInstanceSectionForm2 = class(TSectionForm2)  // for MA 2.0
    constructor Create(AOwner: TComponent; Data: Pointer;
      StatusBar: TTntStatusBar = nil); reintroduce;
  private
    StatusBar: TTntStatusBar;

  protected
    ConnectionData: Pointer;
  public
    //MySQLConn: TMySQLConn;

  end;

implementation

//------------------------------------------------------------------------------

constructor TInstanceSectionForm2.Create(AOwner: TComponent;
  Data: Pointer; StatusBar: TTntStatusBar);
begin
  inherited Create(AOwner, StatusBar);

  //self.MySQLConn:=MySQLConn;
  ConnectionData:=Data;
  self.StatusBar:=StatusBar;
end;

end.
