constructor T%structname%.Create(%variable_list%);

begin
  inherited create;

%classimp_constructor_variable_init%
%classimp_constructor_object%
%classimp_constructor_nested_struct_create_objectlist%
%classimp_constructor_auxlist_create_list%
end;

constructor T%structname%.Create(record_pointer: P%structname%);

%classimp_constructor_nested_struct_var%

begin
%classimp_constructor_variable_with_length%

  create(%variable_from_record_list%);

  self.record_pointer := nil;

  if record_pointer <> nil then
  begin
%classimp_constructor_nested_struct%
%classimp_constructor_auxlist%
  end;
end;
