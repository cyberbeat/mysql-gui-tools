unit CommonFuncs;

interface

uses
  gnugettext, SysUtils, Options, AuxFuncs, Windows, Forms, About,
  TntSystem;

{$include Consts.ini}

procedure CheckCommonCommandlineParameter;
procedure ShowAboutDialog(Caption: WideString;
  Version: WideString; Credits: WideString);
function ShowSplashScreen(Caption: WideString): TAboutForm;

implementation

uses
  Classes, Unicode;

// -----------------------------------------------------------------------------

procedure CheckCommonCommandlineParameter;

var
  i: integer;
  S: WideString;
  HexString: string;
  Utf8String: string;
  
begin
  for i := 1 to WideParamCount do
  begin
    //-------------------------------------------------------------
    //General cmdline parameters

    S := WideParamStr(I);

    //Connection
    if (S = '-c') and (I+1 <= WideParamCount) then
      MYXCommonOptions.ConnectionToUse := WideParamStr(I + 1)
    else
      if (Copy(S, 1, 2) = '-c') then
        MYXCommonOptions.ConnectionToUse := Copy(S, 3, MaxInt)
      else
        //Username
        if ((S = '-u')) and (I+1 <= WideParamCount) then
          MYXCommonOptions.ConnectionUsername := WideParamStr(I + 1)
        else
          if (Copy(S, 1, 2) = '-u') then
            MYXCommonOptions.ConnectionUsername := Copy(S, 3, MaxInt)
          else
            //Password
            if (S = '-p') and (I+1 <= WideParamCount) then
              MYXCommonOptions.ConnectionPassword := WideParamStr(I + 1)
            else
              if (Copy(S, 1, 2) = '-p') then
                MYXCommonOptions.ConnectionPassword := Copy(S, 3, MaxInt)
              else
                //Host
                if (S = '-h') and (I+1 <= WideParamCount) then
                  MYXCommonOptions.ConnectionHost := WideParamStr(I + 1)
                else
                  if (Copy(S, 1, 2) = '-h') then
                    MYXCommonOptions.ConnectionHost := Copy(S, 3, MaxInt)
                  else
                    //Port
                    if (S = '-P') and (I+1 <= WideParamCount) then
                    begin
                      S := WideParamStr(I + 1);
                      MYXCommonOptions.ConnectionPort := StrToIntDef(S, 3306);
                    end
                    else
                      if (Copy(S, 1, 2) = '-P') then
                      begin
                        S := Copy(S, 3, MaxInt);
                        MYXCommonOptions.ConnectionPort := StrToIntDef(S, 3306);
                      end
                      else
                        //Schema
                        if (S = '-D') and (I+1 <= WideParamCount) then
                          MYXCommonOptions.ConnectionSchema := WideParamStr(I + 1)
                        else
                          if (Copy(S, 1, 2) = '-D') then
                            MYXCommonOptions.ConnectionSchema := Copy(S, 3, MaxInt)
                          else
                            // Password hexadecimal
                            if StrLCompW(PWideChar(S), '-x', 2) = 0 then
                            begin
                              if (Length(S) = 2) and (I < WideParamCount) then
                                HexString := WideParamStr(I + 1)
                              else
                                HexString := Copy(S, 3, MaxInt);
                              SetLength(Utf8String, Length(HexString) div 2);
                              HexToBin(PChar(HexString), PChar(Utf8String), Length(Utf8String));
                              MYXCommonOptions.ConnectionPassword := Utf8Decode(Utf8String);
                            end;
  end;

  //If only username was specified, automaticly set
  //other connection values
  if (MYXCommonOptions.ConnectionUsername <> '') then
  begin
    if (MYXCommonOptions.ConnectionHost = '') then
      MYXCommonOptions.ConnectionHost := 'localhost';

    if MYXCommonOptions.ConnectionPort = 0 then
      MYXCommonOptions.ConnectionPort := 3306;
  end;
end;

// -----------------------------------------------------------------------------

procedure ShowAboutDialog(Caption: WideString;
  Version: WideString; Credits: WideString);

var
  AboutForm: TAboutForm;

begin
  AboutForm := TAboutForm.Create(nil);
  try
    AboutForm.Caption := _('About ') + Caption;
    AboutForm.VersionLbl.Caption := _('version') + ' ' + Version;
    AboutForm.TextLbl.Caption := Credits;
    AboutForm.ShowModal;
  finally
    AboutForm.Free;
  end;
end;

// -----------------------------------------------------------------------------

function ShowSplashScreen(Caption: WideString): TAboutForm;

var
  AboutForm: TAboutForm;

begin
  Result := nil;

  AboutForm := TAboutForm.Create(nil);
  try
    AboutForm.Caption := _('About ') + Caption;
    AboutForm.VersionLbl.Caption := _('version') + ' ' +
      product_version+' '+product_build_level;
    AboutForm.TextLbl.Caption := '';
    AboutForm.BorderStyle := bsNone;
    AboutForm.Width := AboutForm.AboutImg.Width;
    AboutForm.Height := AboutForm.AboutImg.Height;
    AboutForm.FormStyle := fsStayOnTop;
    AboutForm.Show;
    AboutForm.Refresh;

    Result := AboutForm;
  except
    AboutForm.Free;
  end;
end;

// -----------------------------------------------------------------------------

end.

