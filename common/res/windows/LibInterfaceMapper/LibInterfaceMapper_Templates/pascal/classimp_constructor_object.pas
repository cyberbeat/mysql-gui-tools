  if %nested_struct% <> nil then
    self.%nested_struct% := T%nested_struct_type%.Create(%nested_struct%)
  else
    self.%nested_struct% := nil;
