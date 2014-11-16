function T%structname%.get_record_pointer: P%structname%;

%classimp_get_record_pointer_var%

begin
  if record_pointer = nil then
  begin
    GetMem(record_pointer, sizeof(%structname%));
%classimp_get_record_pointer_string_variable_init%
%classimp_get_record_pointer_init%
%classimp_get_record_pointer_auxlist_init%
  end;

%classimp_get_record_pointer_variable%
%classimp_get_record_pointer_variable_with_length%
%classimp_get_record_pointer_object%
%classimp_get_record_pointer_nested_struct%
%classimp_get_record_pointer_list%

  Result := record_pointer;
end;
