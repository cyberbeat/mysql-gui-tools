  for I := 0 to record_pointer.%variable%_num - 1 do
  begin
    %variable%.Add(UTF8Decode(
        PPChar(
          integer(record_pointer.%variable%) + SizeOf(PChar) * I
        )^
      ));
  end;
