  if %nested_struct% <> nil then
    record_pointer.%nested_struct% := %nested_struct%.get_record_pointer
  else
    record_pointer.%nested_struct% := nil;
