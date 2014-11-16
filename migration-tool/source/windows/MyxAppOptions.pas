unit MyxAppOptions;

interface

uses MyxOptions, AuxFuncs, TntForms;

type
  TMyxAppOptions = class(TMyxOptions)
  protected
    procedure SetFixedOptions; override;
    procedure SetDefaultOptions; override;
  end;

  function AppOptions: TMyxOptions;

implementation

var
  OptionsInstance: TMyxOptions;

// -----------------------------------------------------------------------------

function AppOptions: TMyxOptions;

begin
  if (OptionsInstance = nil) then
  begin
    GlobalAppDict;

    OptionsInstance := TMyxAppOptions.Create(
      CommonOptions.OptionString['UserDataDir'] +
        TntApplication.Title + ' Options.xml',
      '/app/options');
  end;

  Result := OptionsInstance;
end;

// -----------------------------------------------------------------------------
// TMyxAppOptions
// -----------------------------------------------------------------------------

procedure TMyxAppOptions.SetFixedOptions;

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMyxAppOptions.SetDefaultOptions;

begin
  //
end;

// -----------------------------------------------------------------------------

end.
