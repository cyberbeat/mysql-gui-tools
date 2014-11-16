unit InstanceSections;

interface

uses
  Classes, Forms, Controls, ExtCtrls, Types, Graphics, Messages, ComCtrls,
  Sections, MySQLConnection, TntComCtrls;


type
  // All individual Forms (which are triggered by the SidebarSections)
  // descend from this class so they can be docked to a TSectionPanel from the
  // MainForm more easily
  TInstanceSectionForm = class(TSectionForm)
  public
    MySQLConn: TMySQLConn;

    constructor Create(AOwner: TComponent; MySQLConn: TMySQLConn; StatusBar: TTntStatusBar = nil); reintroduce;
  end;

  TInstanceSectionForm2 = class(TSectionForm2)  // for MA 2.0
    constructor Create(AOwner: TComponent; Data: Pointer;
      StatusBar: TTntStatusBar = nil); reintroduce;
  private
    StatusBar: TTntStatusBar;

  protected
    ConnectionData: Pointer;
  public
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

constructor TInstanceSectionForm.Create(AOwner: TComponent; MySQLConn: TMySQLConn; StatusBar: TTntStatusBar);

begin
  inherited Create(AOwner, StatusBar);

  self.MySQLConn:=MySQLConn;
  self.StatusBar:=StatusBar;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TInstanceSectionForm2.Create(AOwner: TComponent; Data: Pointer; StatusBar: TTntStatusBar);

begin
  inherited Create(AOwner, StatusBar);

  ConnectionData:=Data;
  self.StatusBar:=StatusBar;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
