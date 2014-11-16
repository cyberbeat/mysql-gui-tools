unit GrtSections;

interface

uses Sections, Grt, Classes, TntComCtrls;

type
  TGrtSectionForm = class(TSectionForm)
  protected
    function GetGrt: TGrt;
  public
    property Grt: TGrt read GetGrt;
  end;

implementation

//----------------------------------------------------------------------------------------------------------------------

function TGrtSectionForm.GetGrt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
 
