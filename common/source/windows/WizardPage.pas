unit WizardPage;

interface

uses
  Classes, TntComCtrls, PngImage,
  WizardInterface, PngTools, Sections, GrtSections, Grt;

type
  TWizardPageForm = class(TGrtSectionForm)
    constructor Create(AOwner: TComponent; StatusBar: TTntStatusBar;
      WizardInterface: IWizardInterface; Grt: TGrt = nil);
    destructor Destroy; override;
  protected
    FWizardInterface: IWizardInterface;

    TaskUncheckedPNGImg,
    TaskCheckedPNGImg,
    TaskErrorPNGImg,
    TaskDisabledPNGImg: TPNGObject;
  end;

implementation

// -----------------------------------------------------------------------------

constructor TWizardPageForm.Create(
  AOwner: TComponent; StatusBar: TTntStatusBar;
  WizardInterface: IWizardInterface; Grt: TGrt);

begin
  inherited Create(AOwner, StatusBar);

  FWizardInterface := WizardInterface;

  TaskUncheckedPNGImg := LoadPNGImageFromResource('task_unchecked');
  TaskCheckedPNGImg := LoadPNGImageFromResource('task_checked');
  TaskErrorPNGImg := LoadPNGImageFromResource('task_error');
  TaskDisabledPNGImg := LoadPNGImageFromResource('task_disabled');
end;

destructor TWizardPageForm.Destroy;

begin
  TaskUncheckedPNGImg.Free;
  TaskCheckedPNGImg.Free;
  TaskErrorPNGImg.Free;
  TaskDisabledPNGImg.Free;

  inherited;
end;

end.
 