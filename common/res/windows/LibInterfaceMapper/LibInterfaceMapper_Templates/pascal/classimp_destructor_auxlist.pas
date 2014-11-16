  // Free memory allocated for List.
  if record_pointer <> nil then
    if record_pointer.%variable% <> nil then
    begin
      for I := 0 to record_pointer.%variable%_num - 1 do
      begin
        FreeMem(PPChar(integer(record_pointer.%variable%) + sizeof(PChar) * I)^);
      end;

      FreeMem(record_pointer.%variable%);
    end;

  //Free List
  %variable%.Free;

