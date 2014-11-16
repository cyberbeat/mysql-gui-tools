  record_pointer.%nested_struct%_num := %nested_struct%.Count;

  // If the number of %nested_struct% is now 0, free memory.
  if %nested_struct%.Count = 0 then
  begin
    if record_pointer.%nested_struct% <> nil then
    begin
      FreeMem(record_pointer.%nested_struct%);
      record_pointer.%nested_struct% := nil;
    end;
  end
  else
  begin
    // Make sure enough memory is allocated for all %nested_struct% in the "array".
    ReallocMem(record_pointer.%nested_struct%, %nested_struct%.Count * SizeOf(%nested_struct_type%));
  end;

  // Get all records in the "array".
  for I := 0 to %nested_struct%.Count - 1 do
  begin
    // Copy the record of the element to the "array".
    P%nested_struct_type%(
      Integer(record_pointer.%nested_struct%) + SizeOf(%nested_struct_type%) * I
      )^ := (T%nested_struct_type%(%nested_struct%[i]).get_record_pointer)^;
  end;

  
