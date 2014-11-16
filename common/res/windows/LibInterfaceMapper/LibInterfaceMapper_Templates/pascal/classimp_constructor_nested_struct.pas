  for I := 0 to record_pointer.%nested_struct%_num - 1 do
  begin
    %nested_struct%.Add(T%nested_struct_type%.Create(
      P%nested_struct_type%(
        (Integer(record_pointer.%nested_struct%) + SizeOf(%nested_struct_type%) * I)
        )
      ));
  end;
