procedure T%structname%.Assign(Source: TPersistent);

begin
  if Source is T%structname% then
  begin
    with T%structname%(Source) do
    begin
%classimp_assign_variable%
%classimp_assign_object%
%classimp_assign_nested_struct%
%classimp_assign_auxlist%
    end;
  end
  else
    inherited Assign(Source);
end;
