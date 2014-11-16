  // Allocate memory for string variable %variable%
  if record_pointer.%variable% = nil then
    GetMem(record_pointer.%variable%, Length(UTF8Encode(F%variable%)) + 1)
  else
    ReallocMem(record_pointer.%variable%, Length(UTF8Encode(F%variable%)) + 1);
  StrMove(record_pointer.%variable%, PChar(UTF8Encode(F%variable%)), Length(UTF8Encode(F%variable%)) + 1);

