function %func_name%(%func_params_widestrings%): WideString;

var
  pRetValue: PChar;
  sRetValue: string;

begin
  pRetValue := _%func_name%(%func_params_strings_encoded%);
  sRetValue := pRetValue;
  g_free(pRetValue);
  Result := UTF8Decode(sRetValue);
end;

