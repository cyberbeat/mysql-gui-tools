  if record_pointer <> nil then
    if record_pointer.%nested_struct% <> nil then
      FreeMem(record_pointer.%nested_struct%);

  %nested_struct%.Free;

