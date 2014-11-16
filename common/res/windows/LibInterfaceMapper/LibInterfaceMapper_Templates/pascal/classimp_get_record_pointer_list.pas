  if record_pointer.%variable%_num > 0 then
    if record_pointer.%variable% <> nil then
      for I := 0 to record_pointer.%variable%_num - 1 do
      begin
        FreeMem(P%variable_type%(Integer(record_pointer.%variable%) + SizeOf(%variable_type%) * I)^);
      end;

  record_pointer.%variable%_num := %variable%.Count;
  if %variable%.Count = 0 then
  begin
    if record_pointer.%variable% <> nil then
    begin
      for I := 0 to %variable%.Count - 1 do
      begin
        FreeMem(P%variable_type%(Integer(record_pointer.%variable%) + SizeOf(%variable_type%) * I)^);
      end;

      FreeMem(record_pointer.%variable%);
      record_pointer.%variable% := nil;
    end;
  end
  else
  begin
    ReallocMem(record_pointer.%variable%, SizeOf(%variable_type%) * (%variable%.Count + 1));
  end;

  for I := 0 to %variable%.Count - 1 do
  begin
    // Copy the record of the element to the "array".
    GetMem(P%variable_type%(
      Integer(record_pointer.%variable%) + SizeOf(%variable_type%) * I
      )^, Length(UTF8Encode(%variable%[I])) * 2 + 1);

    StrMove(P%variable_type%(
      Integer(record_pointer.%variable%) + SizeOf(%variable_type%) * I
      )^, PChar(UTF8Encode(%variable%[I])),
      Length(UTF8Encode(%variable%[I])) * 2 + 1);
  end;

  // Set terminating element.
  if %variable%.Count > 0 then
    P%variable_type%(Integer(record_pointer.%variable%)+sizeof(%variable_type%) * %variable%.Count)^ := nil;
