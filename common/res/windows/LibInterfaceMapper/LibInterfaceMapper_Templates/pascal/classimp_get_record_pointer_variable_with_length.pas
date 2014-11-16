  // Allocate memory for variable %variable%
  if record_pointer.%variable% = nil then
    GetMem(record_pointer.%variable%, F%variable%_length)
  else
    ReallocMem(record_pointer.%variable%, F%variable%_length);
  StrMove(record_pointer.%variable%, PChar(F%variable%), F%variable%_length);

