unit WizardInterface;

interface

uses
  StdCtrls, TntStdCtrls;

type
  IWizardInterface = Interface
    function GetNextBtn: TButton;
    function GetBackBtn: TButton;
    function GetDetailsBtn: TButton;

    function GetDetailsActivated: Boolean;
    procedure SetDetailsActivated(Activated: Boolean);

    function GetOperationCanceled: Boolean;
    procedure SetOperationCanceled(Canceled: Boolean);

    property NextBtn: TButton read GetNextBtn;
    property BackBtn: TButton read GetBackBtn;
    property DetailsBtn: TButton read GetDetailsBtn;
    property DetailsActivated: Boolean read GetDetailsActivated write SetDetailsActivated;

    property OperationCanceled: Boolean read GetOperationCanceled write SetOperationCanceled;
  end;

implementation

end.
