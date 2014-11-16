unit AuxApplicationFuncs;

interface

uses Classes, TntClasses, AuxFuncs, Options, Controls;

function ShowOptionalModalDialog(Titel: WideString; Messagetext: WideString;
  DlgType: MYX_MESSAGE_DLG_TYPE = myx_mtInformation;
  buttons: WideString = 'OK';
  ShowDisableMessageCheckbox: Boolean = True;
  PNGImgResName: WideString = ''): Integer;

implementation


function ShowOptionalModalDialog(Titel: WideString; Messagetext: WideString;
  DlgType: MYX_MESSAGE_DLG_TYPE;
  buttons: WideString;
  ShowDisableMessageCheckbox: Boolean;
  PNGImgResName: WideString): Integer;
var ModalDialog: TMyxModalDialog;
  NotUsed: WideString;
begin
  //If warning is already in DoNotShowMessageList, return Ignore
  if(MYXCommonOptions.IgnoreWarningsList.IndexOf(Titel)>-1)then
    Result:=2
  else
  begin
    ModalDialog:=TMyxModalDialog.Create(Titel, Messagetext, DlgType, buttons,
      False, '', NotUsed, 0, False,
      ShowDisableMessageCheckbox, PNGImgResName);
    try
      Result:=ModalDialog.ShowModal;
    if Result = mrCancel then  // VCL result
      Result := 0
    else
      if Result > 100 then
        Dec(Result, 100);      // Our result

      if(ModalDialog.DisableMessageCheckbox<>nil)then
        if(ModalDialog.DisableMessageCheckbox.Checked)then
          if(MYXCommonOptions.IgnoreWarningsList.IndexOf(Titel)=-1)then
            MYXCommonOptions.IgnoreWarningsList.Add(Titel);
    finally
      ModalDialog.Free;
    end;
  end;
end;

end.
 