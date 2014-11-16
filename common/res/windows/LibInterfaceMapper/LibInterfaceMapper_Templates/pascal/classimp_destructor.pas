destructor T%structname%.Destroy;

%classimp_destructor_auxlist_var%

begin
%classimp_destructor_object%
%classimp_destructor_nested_struct%
%classimp_destructor_auxlist%

  if record_pointer <> nil then
  begin
%classimp_destructor_string_variable%  
    FreeMem(record_pointer);
  end;

  inherited Destroy;
end;
