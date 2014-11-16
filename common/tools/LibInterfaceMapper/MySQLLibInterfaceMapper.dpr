program MySQLLibInterfaceMapper;

{$APPTYPE CONSOLE}





{%File 'PrepareApplicationFolder.cmd'}

uses
  SysUtils,
  RegExpr,
  Classes,
  IniFiles,
  StrUtils,
  AuxFuncsLibMapper in 'AuxFuncsLibMapper.pas',
  gnugettext in '..\..\source\Windows\gnugettext.pas';

procedure Build_Classes(structname: string;
  ClassDefinition, ClassImplementation,
  ClassVariables, ClassNestedStructLists, ClassLists, ClassNestedStructs: TStringList;
  MakeCfiles: Boolean = False);
var i, j: integer;
  temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8,
  temp9, temp10, temp11, temp12, temp13, temp14: string;
  ListTypes, ListDatatypes: TStringList;
  class_variables,
  class_properties,
  class_nested_structs,
  classimp_constructor_variable_init,
  classimp_constructor_variable_with_length,
  classimp_constructor_nested_struct,
  classimp_constructor_nested_struct_var,
  classimp_constructor_nested_struct_create_objectlist,
  classimp_constructor_auxlist,
  classimp_constructor_auxlist_create_list,
  classimp_destructor_nested_struct,
  classimp_destructor_auxlist,
  classimp_destructor_auxlist_var,
  classimp_destructor_object,
  classimp_destructor_string_variable,
  classimp_get_record_pointer_variable,
  classimp_get_record_pointer_variable_with_length,
  classimp_get_record_pointer_init,
  classimp_get_record_pointer_auxlist_init,
  classimp_get_record_pointer_string_variable_init,
  classimp_get_record_pointer_nested_struct,
  classimp_get_record_pointer_var,
  classimp_get_record_pointer_list,
  classimp_assign_variable,
  classimp_assign_nested_struct,
  classimp_assign_auxlist,
  classimp_assign_object,
  class_auxlists,
  class_object,
  classimp_constructor_object,
  classimp_get_record_pointer_object,
  variable_list,
  variable_from_record_list: string;
  is_nested_num: Boolean;
  TemplatePath: string;
  hFileExt, cFileExt: string;
  string_datatype, binarydata_datatype: string;
  func_param_delim,
  func_call_delim: string;
begin
  if(Not(MakeCfiles))then
  begin
    TemplatePath:=ExtractFilePath(ParamStr(0))+'LibInterfaceMapper_Templates'+PathDelim+'pascal'+PathDelim;
    hFileExt:='pas';
    cFileExt:='pas';
  end
  else
  begin
    TemplatePath:=ExtractFilePath(ParamStr(0))+'LibInterfaceMapper_Templates'+PathDelim+'cpp'+PathDelim;
    hFileExt:='h';
    cFileExt:='cpp';
  end;

  string_datatype:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'STRING_DATATYPE'));
  binarydata_datatype:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'BINARYDATA_DATATYPE'));

  func_param_delim:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'FUNCTION_PARAM_DELIM'));
  //if the func_param_delim is ; it is not
  if(func_param_delim='\;')then
    func_param_delim:=';';
  func_call_delim:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'FUNCTION_CALL_DELIM'));

  temp1:=LoadTextFromFile(TemplatePath+'class_variable.'+hFileExt);
  temp2:=LoadTextFromFile(TemplatePath+'class_property.'+hFileExt);
  temp3:=LoadTextFromFile(TemplatePath+'classimp_constructor_variable_init.'+cFileExt);
  temp4:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_variable.'+cFileExt);
  temp5:=trim(LoadTextFromFile(TemplatePath+'variable_list.'+hFileExt));
  temp6:=trim(LoadTextFromFile(TemplatePath+'variable_from_record_list.'+hFileExt));
  temp7:=LoadTextFromFile(TemplatePath+'classimp_assign_variable.'+cFileExt);
  temp8:=LoadTextFromFile(TemplatePath+'classimp_constructor_variable_with_length.'+cFileExt);
  temp9:=trim(LoadTextFromFile(TemplatePath+'variable_with_length_from_record_list.'+hFileExt));
  temp10:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_string_variable.'+cFileExt);
  temp11:=LoadTextFromFile(TemplatePath+'classimp_destructor_string_variable.'+cFileExt);
  temp12:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_string_variable_init.'+cFileExt);
  temp13:=trim(LoadTextFromFile(TemplatePath+'string_from_record_list.'+cFileExt));
  temp14:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_variable_with_length.'+cFileExt);

  class_variables:='';
  class_properties:='';
  classimp_constructor_variable_init:='';
  classimp_get_record_pointer_variable:='';
  variable_list:='';
  variable_from_record_list:='';
  classimp_assign_variable:='';
  classimp_constructor_variable_with_length:='';
  classimp_destructor_string_variable:='';
  classimp_get_record_pointer_string_variable_init:='';
  classimp_get_record_pointer_variable_with_length:='';
  for i:=0 to ClassVariables.Count-1 do
  begin
    if(CompareText(ClassVariables.Names[i], 'dummy')=0)then
      continue;

    //check if variable is used as nestedstruct_num variable or
    //list_num variable
    is_nested_num:=False;
    if(CompareText(Copy(ClassVariables.Names[i],
      Length(ClassVariables.Names[i])-3, 4), '_num')=0)then
    begin
      for j:=0 to ClassNestedStructLists.Count-1 do
        if(CompareText(ClassNestedStructLists.Names[j]+'_num',
          ClassVariables.Names[i])=0)then
        begin
          is_nested_num:=True;
          break;
        end;

      for j:=0 to ClassLists.Count-1 do
        if(CompareText(ClassLists.Names[j]+'_num',
          ClassVariables.Names[i])=0)then
        begin
          is_nested_num:=True;
          break;
        end;
    end;

    //Don't add nestedstruct_num as variable
    if(Not(is_nested_num))then
    begin
      class_variables:=class_variables+
        AnsiReplaceText(
          AnsiReplaceText(temp1,
          '%variable%', ClassVariables.Names[i]),
        '%datatype%', ClassVariables.ValueFromIndex[i]);

      class_properties:=class_properties+
        AnsiReplaceText(
          AnsiReplaceText(temp2,
          '%variable%', ClassVariables.Names[i]),
        '%datatype%', ClassVariables.ValueFromIndex[i]);

      classimp_constructor_variable_init:=
        classimp_constructor_variable_init+
        AnsiReplaceText(temp3,
        '%variable%', ClassVariables.Names[i]);


      //Special handing for strings
      if(ClassVariables.ValueFromIndex[i]=string_datatype)or
        (ClassVariables.ValueFromIndex[i]=binarydata_datatype)then
      begin
        //If variable_length exists, this is binary data
        if(ClassVariables.IndexOfName(ClassVariables.Names[i]+'_length')=-1)then
          classimp_get_record_pointer_variable:=
            classimp_get_record_pointer_variable+
            AnsiReplaceText(
              AnsiReplaceText(temp10,
              '%variable%', ClassVariables.Names[i]),
            '%datatype%', ClassVariables.ValueFromIndex[i]);

        classimp_destructor_string_variable:=
          classimp_destructor_string_variable+
          AnsiReplaceText(
            AnsiReplaceText(temp11,
            '%variable%', ClassVariables.Names[i]),
          '%datatype%', ClassVariables.ValueFromIndex[i]);

        classimp_get_record_pointer_string_variable_init:=
          classimp_get_record_pointer_string_variable_init+
          AnsiReplaceText(
            AnsiReplaceText(temp12,
            '%variable%', ClassVariables.Names[i]),
          '%datatype%', ClassVariables.ValueFromIndex[i]);
      end;

      variable_list:=variable_list+
        AnsiReplaceText(
          AnsiReplaceText(temp5,
          '%variable%', ClassVariables.Names[i]),
        '%datatype%', ClassVariables.ValueFromIndex[i]);

      classimp_assign_variable:=
        classimp_assign_variable+
        AnsiReplaceText(temp7,
        '%variable%', ClassVariables.Names[i]);

      //If variable_length exists, this is binary data
      if(ClassVariables.IndexOfName(ClassVariables.Names[i]+'_length')<>-1)then
      begin
        classimp_constructor_variable_with_length:=
          classimp_constructor_variable_with_length+
          AnsiReplaceText(temp8,
          '%variable%', ClassVariables.Names[i]);

        variable_from_record_list:=
          variable_from_record_list+
          AnsiReplaceText(temp9,
          '%variable%', ClassVariables.Names[i]);

        classimp_get_record_pointer_variable_with_length:=
          classimp_get_record_pointer_variable_with_length+
          AnsiReplaceText(temp14,
          '%variable%', ClassVariables.Names[i]);

      end
      else
      begin
        //String may need special conversion
        if(ClassVariables.ValueFromIndex[i]=string_datatype)then
          variable_from_record_list:=
            variable_from_record_list+
            AnsiReplaceText(temp13,
            '%variable%', ClassVariables.Names[i])
        else
          variable_from_record_list:=
            variable_from_record_list+
            AnsiReplaceText(temp6,
            '%variable%', ClassVariables.Names[i]);

        if(ClassVariables.ValueFromIndex[i]<>string_datatype)then
          classimp_get_record_pointer_variable:=
            classimp_get_record_pointer_variable+
            AnsiReplaceText(
              AnsiReplaceText(temp4,
              '%variable%', ClassVariables.Names[i]),
            '%datatype%', ClassVariables.ValueFromIndex[i]);
      end;
    end;
  end;

  //Remove last ;
  variable_list:=Copy(variable_list, 1, Length(variable_list)-1);
  //Remove last ,
  variable_from_record_list:=Copy(variable_from_record_list, 1,
    Length(variable_from_record_list)-1);

  temp3:=LoadTextFromFile(TemplatePath+'classimp_constructor_nested_struct.'+cFileExt);
  temp4:=LoadTextFromFile(TemplatePath+'classimp_destructor_nested_struct.'+cFileExt);
  temp5:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_init.'+cFileExt);
  temp6:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_nested_struct.'+cFileExt);
  temp7:=LoadTextFromFile(TemplatePath+'classimp_assign_nested_struct.'+cFileExt);
  temp8:=LoadTextFromFile(TemplatePath+'class_nested_structs.'+hFileExt);
  temp9:=LoadTextFromFile(TemplatePath+'classimp_constructor_nested_struct_create_objectlist.'+cFileExt);

  classimp_constructor_nested_struct:='';
  classimp_destructor_nested_struct:='';
  classimp_get_record_pointer_init:='';
  classimp_get_record_pointer_nested_struct:='';
  class_nested_structs:='';
  classimp_assign_nested_struct:='';
  for i:=0 to ClassNestedStructLists.Count-1 do
  begin
    classimp_constructor_nested_struct:=
      classimp_constructor_nested_struct+
      AnsiReplaceText(
        AnsiReplaceText(temp3,
          '%nested_struct%', ClassNestedStructLists.Names[i]),
        '%nested_struct_type%', ClassNestedStructLists.ValueFromIndex[i]);

    classimp_destructor_nested_struct:=
      classimp_destructor_nested_struct+
      AnsiReplaceText(temp4,
       '%nested_struct%', ClassNestedStructLists.Names[i]);

    classimp_get_record_pointer_init:=
      classimp_get_record_pointer_init+
      AnsiReplaceText(temp5,
       '%nested_struct%', ClassNestedStructLists.Names[i]);

    classimp_get_record_pointer_nested_struct:=
      classimp_get_record_pointer_nested_struct+
      AnsiReplaceText(
        AnsiReplaceText(temp6,
          '%nested_struct%', ClassNestedStructLists.Names[i]),
        '%nested_struct_type%', ClassNestedStructLists.ValueFromIndex[i]);

    classimp_assign_nested_struct:=
      classimp_assign_nested_struct+
      AnsiReplaceText(temp7,
        '%nested_struct%', ClassNestedStructLists.Names[i]);

    class_nested_structs:=
      class_nested_structs+
      AnsiReplaceText(
        AnsiReplaceText(temp8,
          '%nested_struct%', ClassNestedStructLists.Names[i]),
        '%nested_struct_type%', ClassNestedStructLists.ValueFromIndex[i]);

    classimp_constructor_nested_struct_create_objectlist:=
      classimp_constructor_nested_struct_create_objectlist+
      AnsiReplaceText(
        AnsiReplaceText(temp9,
          '%nested_struct%', ClassNestedStructLists.Names[i]),
        '%nested_struct_type%', ClassNestedStructLists.ValueFromIndex[i]);
  end;


  class_auxlists:='';
  classimp_constructor_auxlist:='';
  classimp_destructor_auxlist:='';
  classimp_get_record_pointer_list:='';
  classimp_get_record_pointer_auxlist_init:='';
  classimp_assign_auxlist:='';

  if(ClassLists.Count>0)then
  begin
    ListTypes:=TStringList.Create;
    ListDatatypes:=TStringList.Create;
    try
      ListTypes.Text:=Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'AUX_LISTS');
      ListDatatypes.Text:=Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'AUX_LISTS_DATATYPES');

      temp1:=LoadTextFromFile(TemplatePath+'class_auxlists.'+hFileExt);
      temp2:=LoadTextFromFile(TemplatePath+'classimp_constructor_auxlist_create_list.'+cFileExt);
      temp3:=LoadTextFromFile(TemplatePath+'classimp_constructor_auxlist.'+cFileExt);
      temp4:=LoadTextFromFile(TemplatePath+'classimp_destructor_auxlist.'+cFileExt);
      temp5:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_list.'+cFileExt);
      temp6:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_auxlist_init.'+cFileExt);
      temp7:=LoadTextFromFile(TemplatePath+'classimp_assign_auxlist.'+cFileExt);

      for i:=0 to ClassLists.Count-1 do
      begin
        class_auxlists:=
          class_auxlists+
          AnsiReplaceText(
            AnsiReplaceText(temp1,
              '%variable%', ClassLists.Names[i]),
            '%list_type%', ListTypes.Values[ClassLists.ValueFromIndex[i]]);

        classimp_constructor_auxlist_create_list:=
          classimp_constructor_auxlist_create_list+
          AnsiReplaceText(
            AnsiReplaceText(temp2,
              '%variable%', ClassLists.Names[i]),
            '%list_type%', ListTypes.Values[ClassLists.ValueFromIndex[i]]);

        classimp_constructor_auxlist:=
          classimp_constructor_auxlist+
          AnsiReplaceText(
            AnsiReplaceText(temp3,
              '%variable%', ClassLists.Names[i]),
            '%list_type%', ListTypes.Values[ClassLists.ValueFromIndex[i]]);

        classimp_destructor_auxlist:=
          classimp_destructor_auxlist+
          AnsiReplaceText(
            AnsiReplaceText(temp4,
              '%variable%', ClassLists.Names[i]),
            '%list_type%', ListTypes.Values[ClassLists.ValueFromIndex[i]]);

        classimp_get_record_pointer_list:=
          classimp_get_record_pointer_list+
          AnsiReplaceText(
            AnsiReplaceText(temp5,
              '%variable%', ClassLists.Names[i]),
            '%variable_type%', ListDatatypes.Values[ListTypes.Values[ClassLists.ValueFromIndex[i]]]);

        classimp_get_record_pointer_auxlist_init:=
          classimp_get_record_pointer_auxlist_init+
            AnsiReplaceText(temp6,
              '%variable%', ClassLists.Names[i]);

        classimp_assign_auxlist:=
          classimp_assign_auxlist+
            AnsiReplaceText(temp7,
              '%variable%', ClassLists.Names[i]);
      end;
    finally
      ListTypes.Free;
      ListDatatypes.Free;
    end;
  end;

  if(ClassNestedStructLists.Count>0)or(ClassLists.Count>0)then
  begin
    classimp_constructor_nested_struct_var:=LoadTextFromFile(TemplatePath+'classimp_constructor_nested_struct_var.'+cFileExt);
    classimp_get_record_pointer_var:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_var.'+cFileExt);
  end
  else
  begin
    classimp_constructor_nested_struct_var:='';
    classimp_get_record_pointer_var:='';
  end;

  if(ClassLists.Count>0)then
    classimp_destructor_auxlist_var:=LoadTextFromFile(TemplatePath+'classimp_destructor_auxlist_var.'+cFileExt)
  else
    classimp_destructor_auxlist_var:='';


  class_object:='';
  classimp_constructor_object:='';
  classimp_destructor_object:='';
  classimp_get_record_pointer_object:='';
  classimp_assign_object:='';
  
  if(ClassNestedStructs.Count>0)then
  begin
    temp1:=LoadTextFromFile(TemplatePath+'class_object.'+hFileExt);
    temp2:=LoadTextFromFile(TemplatePath+'classimp_constructor_object.'+cFileExt);
    temp3:=LoadTextFromFile(TemplatePath+'classimp_destructor_object.'+cFileExt);
    temp4:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer_object.'+cFileExt);
    temp5:=trim(LoadTextFromFile(TemplatePath+'object_list.'+hFileExt));
    temp6:=trim(LoadTextFromFile(TemplatePath+'object_from_record_list.'+hFileExt));
    temp7:=LoadTextFromFile(TemplatePath+'classimp_assign_object.'+cFileExt);

    if(variable_list<>'')then
      variable_list:=variable_list+func_param_delim;
    if(variable_from_record_list<>'')then
      variable_from_record_list:=variable_from_record_list+func_call_delim;

    for i:=0 to ClassNestedStructs.Count-1 do
    begin
      class_object:=
        class_object+
        AnsiReplaceText(
          AnsiReplaceText(temp1,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      classimp_constructor_object:=
        classimp_constructor_object+
        AnsiReplaceText(
          AnsiReplaceText(temp2,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      classimp_destructor_object:=
        classimp_destructor_object+
        AnsiReplaceText(
          AnsiReplaceText(temp3,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      classimp_get_record_pointer_object:=
        classimp_get_record_pointer_object+
        AnsiReplaceText(
          AnsiReplaceText(temp4,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      variable_list:=
        variable_list+
        AnsiReplaceText(
          AnsiReplaceText(temp5,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      variable_from_record_list:=
        variable_from_record_list+
        AnsiReplaceText(
          AnsiReplaceText(temp6,
            '%nested_struct%', ClassNestedStructs.Names[i]),
          '%nested_struct_type%', ClassNestedStructs.ValueFromIndex[i]);

      classimp_assign_object:=
        classimp_assign_object+
        AnsiReplaceText(temp7,
        '%nested_struct%', ClassNestedStructs.Names[i]);
    end;
  end;

  //Remove last ;
  if(Copy(variable_list, Length(variable_list), 1)=func_param_delim)then
    variable_list:=Copy(variable_list, 1, Length(variable_list)-1);
  //Remove last ,
  if(Copy(variable_from_record_list, Length(variable_from_record_list), 1)=func_call_delim)then
    variable_from_record_list:=Copy(variable_from_record_list, 1,
      Length(variable_from_record_list)-1);



  //Add line between class_variables and class_nested_structs_get_set_funcs
  if(class_variables<>'')then
    class_variables:=class_variables+#13#10;

  if(class_nested_structs<>'')then
    class_nested_structs:=class_nested_structs+#13#10;

  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'class.'+hFileExt);

  RemoveTrailingLineWrap(temp1, '%class_variables%');
  RemoveTrailingLineWrap(temp1, '%class_properties%');
  RemoveTrailingLineWrap(temp1, '%class_object%');
  RemoveTrailingLineWrap(temp1, '%nested_structs%');
  RemoveTrailingLineWrap(temp1, '%nested_structs_get_set_funcs%');
  RemoveTrailingLineWrap(temp1, '%nested_structs_count_func_and_properties%');

  ClassDefinition.Add(
    AnsiReplaceText(
      AnsiReplaceText(
        AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(temp1,
                  '%structname%', structname),
                '%variable_list%', variable_list),
              '%class_variables%', class_variables),
            '%class_object%', class_object),
          '%class_properties%', class_properties),
        '%class_auxlists%', class_auxlists),
      '%class_nested_structs%', class_nested_structs)
    );

  temp1:=LoadTextFromFile(TemplatePath+'class_forward_decl.'+hFileExt);
  ClassDefinition.Insert(0, AnsiReplaceText(temp1,
    '%structname%', structname));

  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'classimp_constructor.'+cFileExt);

  RemoveTrailingLineWrap(temp1, '%classimp_constructor_nested_struct_var%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_variable_init%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_nested_struct%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_nested_struct_create_objectlist%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_auxlist%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_auxlist_create_list%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_object%');
  RemoveTrailingLineWrap(temp1, '%classimp_constructor_variable_with_length%');
  classimp_constructor_variable_with_length:=classimp_constructor_variable_with_length+#13#10;

  ClassImplementation.Add(
    AnsiReplaceText(
      AnsiReplaceText(
        AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(
                  AnsiReplaceText(
                    AnsiReplaceText(
                      AnsiReplaceText(
                        AnsiReplaceText(temp1,
                          '%structname%', structname),
                        '%variable_list%', variable_list),
                      '%variable_from_record_list%', variable_from_record_list),
                    '%classimp_constructor_variable_init%', classimp_constructor_variable_init),
                  '%classimp_constructor_object%', classimp_constructor_object),
                '%classimp_constructor_nested_struct_create_objectlist%', classimp_constructor_nested_struct_create_objectlist),
              '%classimp_constructor_nested_struct_var%', classimp_constructor_nested_struct_var),
            '%classimp_constructor_nested_struct%', classimp_constructor_nested_struct),
          '%classimp_constructor_auxlist%', classimp_constructor_auxlist),
        '%classimp_constructor_auxlist_create_list%', classimp_constructor_auxlist_create_list),
      '%classimp_constructor_variable_with_length%', classimp_constructor_variable_with_length)
    );

  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'classimp_destructor.'+cFileExt);

  RemoveTrailingLineWrap(temp1, '%classimp_destructor_nested_struct%');
  RemoveTrailingLineWrap(temp1, '%classimp_destructor_auxlist%');
  RemoveTrailingLineWrap(temp1, '%classimp_destructor_auxlist_var%');
  RemoveTrailingLineWrap(temp1, '%classimp_destructor_object%');
  RemoveTrailingLineWrap(temp1, '%classimp_destructor_string_variable%');

  ClassImplementation.Add(
    AnsiReplaceText(
      AnsiReplaceText(
        AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(
              AnsiReplaceText(temp1,
                '%structname%', structname),
              '%classimp_destructor_auxlist_var%', classimp_destructor_auxlist_var),
            '%classimp_destructor_nested_struct%', classimp_destructor_nested_struct),
          '%classimp_destructor_string_variable%', classimp_destructor_string_variable),
        '%classimp_destructor_auxlist%', classimp_destructor_auxlist),
      '%classimp_destructor_object%', classimp_destructor_object)
    );

  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'classimp_get_record_pointer.'+cFileExt);

  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_var%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_init%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_variable%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_nested_struct%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_list%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_auxlist_init%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_object%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_string_variable_init%');
  RemoveTrailingLineWrap(temp1, '%classimp_get_record_pointer_variable_with_length%');

  ClassImplementation.Add(
    AnsiReplaceText(
      AnsiReplaceText(
        AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(
                  AnsiReplaceText(
                    AnsiReplaceText(
                      AnsiReplaceText(temp1,
                        '%structname%', structname),
                      '%classimp_get_record_pointer_variable_with_length%', classimp_get_record_pointer_variable_with_length),
                    '%classimp_get_record_pointer_string_variable_init%', classimp_get_record_pointer_string_variable_init),
                  '%classimp_get_record_pointer_var%', classimp_get_record_pointer_var),
                '%classimp_get_record_pointer_init%', classimp_get_record_pointer_init),
              '%classimp_get_record_pointer_auxlist_init%', classimp_get_record_pointer_auxlist_init),
            '%classimp_get_record_pointer_variable%', classimp_get_record_pointer_variable),
          '%classimp_get_record_pointer_object%', classimp_get_record_pointer_object),
        '%classimp_get_record_pointer_nested_struct%', classimp_get_record_pointer_nested_struct),
      '%classimp_get_record_pointer_list%', classimp_get_record_pointer_list)
    );

  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'classimp_assign.'+cFileExt);

  RemoveTrailingLineWrap(temp1, '%classimp_assign_variable%');
  RemoveTrailingLineWrap(temp1, '%classimp_assign_nested_struct%');
  RemoveTrailingLineWrap(temp1, '%classimp_assign_auxlist%');
  RemoveTrailingLineWrap(temp1, '%classimp_assign_object%');

  ClassImplementation.Add(
    AnsiReplaceText(
      AnsiReplaceText(
        AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(temp1,
              '%structname%', structname),
            '%classimp_assign_variable%', classimp_assign_variable),
          '%classimp_assign_object%', classimp_assign_object),
        '%classimp_assign_nested_struct%', classimp_assign_nested_struct),
      '%classimp_assign_auxlist%', classimp_assign_auxlist)
    );


  //---------------------------------------------------------------------
  temp1:=LoadTextFromFile(TemplatePath+'classimp_nested_struct_funcs.'+cFileExt);

  ClassImplementation.Add(
    AnsiReplaceText(temp1,
      '%structname%', structname)
    );
  ClassImplementation.Add('');

  ClassImplementation.Add('//'+StringOfChar('-', 78));
  ClassImplementation.Add('');
end;

procedure ENUM_Struct_C2Pas(HeaderFileName, UnitFileName, IncFileName: string;
  IgnoreMissingLIB_INTERFACE_VERSION: Boolean = False;
  DoDynamicDllLoading: Boolean = False;
  MakeCfiles: Boolean = False);
var RegExpr, Enums, Structs, Variables, VarNames,
  Funcs, FuncParams: TRegExpr;
  HeaderTxt, FuncDefs,
  DatatypeMapping, NestedStructsPointers,
  ClassVariables, ClassNestedStructLists, ClassLists, ClassNestedStructs,
  ClassDefinition, ClassImplementation,
  ClassNestedStructLists_Num_Vars: TStringList;
  SourceHeader, SourceHeaderNoComments,
  Parameter, Datatype, Datatypes2Map,
  LibInterfaceName, LibInterfaceVersion,
  FuncParameter: string;
  i: integer;
  UnitText: TStringList;
  TemplatePath: string;
  cFileExt, hFileExt: string;
  s, s2, s3, s4: string;
  FunctionDef,
  FunctionPointerDef,
  newFunctionPointerDef,
  FunctionPointerParams,
  FunctionWrapperDef,
  AddToUses: string;
  FunctionWrapper,
  FunctionWrapperPChar,
  FunctionWrapperConstPChar,
  FunctionWrapperVoid: string;
  FuncParameterWidestrings,
  FuncParameterStringsEncoded: string;
  string_datatype, binarydata_datatype: string;
  VarsWithLength: TStringList;
  VarArgsFunc: Boolean;

begin
  if(Not(MakeCfiles))then
  begin
    cFileExt:='pas';
    hFileExt:='pas';
  end
  else
  begin
    cFileExt:='cpp';
    hFileExt:='h';
  end;

  HeaderTxt:=TStringList.Create;
  DatatypeMapping:=TStringList.Create;
  NestedStructsPointers:=TStringList.Create;
  ClassNestedStructLists:=TStringList.Create;
  ClassVariables:=TStringList.Create;
  ClassLists:=TStringList.Create;
  ClassDefinition:=TStringList.Create;
  ClassImplementation:=TStringList.Create;
  UnitText:=TStringList.Create;
  FuncDefs:=TStringList.Create;
  ClassNestedStructLists_Num_Vars:=TStringList.Create;
  ClassNestedStructs:=TStringList.Create;
  VarsWithLength:=TStringList.Create;
  try
    if(Not(MakeCfiles))then
      TemplatePath:=ExtractFilePath(ParamStr(0))+'LibInterfaceMapper_Templates'+PathDelim+'pascal'+PathDelim
    else
      TemplatePath:=ExtractFilePath(ParamStr(0))+'LibInterfaceMapper_Templates'+PathDelim+'cpp'+PathDelim;

    //Check if LibInterfaceMapper_Mappings.ini file is there
    if(Not(FileExists(TemplatePath+'LibInterfaceMapper_Mappings.ini')))then
    begin
      WriteLn('ERROR: File ['+TemplatePath+'LibInterfaceMapper_Mappings.ini] cannot be found.');
      WriteLn('');
      ExitCode:=1;
      Exit;
    end;

    //Initialize Datatype mapping from file
    DatatypeMapping.Text:=Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'DATATYPE_MAPPING');

    string_datatype:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'STRING_DATATYPE'));

    binarydata_datatype:=trim(Get_Ini_Section(TemplatePath+'LibInterfaceMapper_Mappings.ini', 'BINARYDATA_DATATYPE'));

    //Clear list of nested Structs
    ClassNestedStructLists.Clear;

    //Load the C-header file
    SourceHeader:=LoadTextFromFile(HeaderFileName);

    RegExpr:=TRegExpr.Create;
    Enums:=TRegExpr.Create;
    Structs:=TRegExpr.Create;
    Variables:=TRegExpr.Create;
    VarNames:=TRegExpr.Create;
    Funcs:=TRegExpr.Create;
    FuncParams:=TRegExpr.Create;
    try
      //-------------------------------------------------------------
      //Set Expressions
      Enums.Expression:='(?img)(typedef enum|^enum)\s{0,}(\w+){0,}\s{0,}\{'+
        '([a-zA-Z0-9_\s=\,\-]+)\}\s*(\w*)\;';

      Structs.Expression:='(?img)typedef\s{0,}struct\s{0,}\w{0,}\s{0,}\{'+
        '\s{0,}([\w\s\*\;\,/\?\(\)\.\[\]]+)\}\s(\w+){1,}\;\s*';

      //Variables.Expression is build dynamicly

      VarNames.Expression:='(?img)\s*(\w+)\,{0,}';

      //-------------------------------------------------------------
      //Get File Version
      LibInterfaceName:='';

      RegExpr.Expression:='(?img)///\s{0,}\[SCRIPT\:\:LibInterfaceMapper\]\s{1,}\-public_interface\s{1,}\"(\w+)\"';

      if(RegExpr.Exec(SourceHeader))then
      begin
        LibInterfaceName:=RegExpr.Match[1];

        RegExpr.Expression:='(?img)\#define\s{1,}'+LibInterfaceName+'_PUBLIC_INTERFACE_VERSION\s{1,}(\w+)';
        if(RegExpr.Exec(SourceHeader))then
          LibInterfaceVersion:=RegExpr.Match[1];
      end;

      if(LibInterfaceName='')and(IgnoreMissingLIB_INTERFACE_VERSION)then
      begin
        LibInterfaceName:=ChangeFileExt(ExtractFileName(HeaderFileName), '');
        LibInterfaceVersion:='-1';
      end;

      if(LibInterfaceName='')then
      begin
        WriteLn('ERROR: There is no PUBLIC_INTERFACE_VERSION defined in the Header file.');
        WriteLn('');
        WriteLn('The source header file has to contain the following lines:');
        WriteLn('/// [SCRIPT::LibInterfaceMapper] -public_interface "xxx"');
        WriteLn('#define xxx_PUBLIC_INTERFACE_VERSION "x"');
        ExitCode:=1;
        Exit;
      end
      else
      begin
        s:=LoadTextFromFile(TemplatePath+'public_interface_version.'+hFileExt);

        s:=AnsiReplaceText(
            AnsiReplaceText(s,
              '%LibInterfaceName%', LibInterfaceName),
            '%LibInterfaceVersion%', LibInterfaceVersion
          );

        HeaderTxt.Add(s);
      end;

      //-------------------------------------------------------------
      // Find AddToUses
      AddToUses:='';
      RegExpr.Expression:='(?img)///\s{0,}\[SCRIPT\:\:LibInterfaceMapper\]\s{1,}\-add_to_uses\s{1,}\"(.*?)\"';

      if(RegExpr.Exec(SourceHeader))then
      begin
        AddToUses:=RegExpr.Match[1];
        if(AddToUses<>'')then
          AddToUses:=', '+AddToUses;
      end;

      //-------------------------------------------------------------
      // Get Datatypes from GetDatatypesFrom
      RegExpr.Expression:='(?img)///\s{0,}\[SCRIPT\:\:LibInterfaceMapper\]\s{1,}\-add_datatypes_from\s{1,}\"(.*?)\"';

      if(RegExpr.Exec(SourceHeader))then
      begin
        S := RegExpr.Match[1];
        s:=GetDatatypeDefinitionFromFile(
          ExtractFilePath(HeaderFileName)+RegExpr.Match[1],
          NestedStructsPointers);
        DatatypeMapping.Text:=DatatypeMapping.Text+s;
      end;



      //-------------------------------------------------------------
      //Remove comments from file
      RegExpr.Expression:='(?img)(/\*([\w\s=\,\(\)\;/\\\.\-\''\?\:\*\<\>\#\!\+\[\]]+?)\*/)|'+
        '(//([\w\t \=\,\(\)\;/\\\.\-\''\?\:\*\<\>\#\!\+\[\]]+))';

      SourceHeaderNoComments:=RegExpr.Replace(SourceHeader, '', True);


      //No need for enum definitions in C files
      if(Not(MakeCfiles))then
      begin
        //-------------------------------------------------------------
        //Scan through file

        HeaderTxt.Add('  //'+StringOfChar('-', 76));
        HeaderTxt.Add('  // Enum definitions');
        HeaderTxt.Add('');

        //--------------------------------
        //Find Enum-Definitions
        if(Enums.Exec(SourceHeaderNoComments))then
        begin
          repeat
          begin
            //Matches
            // 0..complete match string
            // 1..typedef enum/enum
            // 2..enum name
            // 3..enum values
            // 4..enum type name

            if(CompareText(Enums.Match[1], 'enum')=0)then
            begin
              HeaderTxt.Add('  P'+Enums.Match[2]+' = ^'+Enums.Match[2]+';');
              HeaderTxt.Add('  '+Enums.Match[2]+' =');
            end
            else
            begin
              HeaderTxt.Add('  P'+Enums.Match[4]+' = ^'+Enums.Match[4]+';');
              HeaderTxt.Add('  '+Enums.Match[4]+' =');
            end;

            HeaderTxt.Add('  ('+Enums.Match[3]+'  );');
            HeaderTxt.Add('');

            //Add this enum to datatype mapping
            if(CompareText(Enums.Match[1], 'typedef enum')=0)then
            begin
              DatatypeMapping.Add(Enums.Match[4]+' *=P'+Enums.Match[4]);
              DatatypeMapping.Add(Enums.Match[4]+'='+Enums.Match[4]);
            end;

            if(Enums.Match[2]<>'')then
            begin
              DatatypeMapping.Add(Enums.Match[2]+' *=P'+Enums.Match[2]);
              DatatypeMapping.Add('enum '+Enums.Match[2]+'='+Enums.Match[2]);
            end;
          end
          until not Enums.ExecNext;
        end;

        HeaderTxt.Add('');

        HeaderTxt.Add('  //'+StringOfChar('-', 76));
        HeaderTxt.Add('  // Struct definitions');
        HeaderTxt.Add('');
      end;

      //--------------------------------
      //Find Struct-Definition

      //Scan for Datatype mapping
      if(Structs.Exec(SourceHeaderNoComments))then
      begin
        repeat
        begin
          //Matches
          // 0..complete match string
          // 1..variables
          // 2..struct name

          if(Not(MakeCfiles))then
            HeaderTxt.Add('  P'+Structs.Match[2]+' = ^'+Structs.Match[2]+';');

          //Add pointer to this struct to datatype mapping
          DatatypeMapping.Add(Structs.Match[2]+' *=P'+Structs.Match[2]);
          DatatypeMapping.Add('struct '+Structs.Match[2]+' *=P'+Structs.Match[2]);
        end
        until not Structs.ExecNext;
      end;

      //Create list of datatypes to map: int|xmlChar \*|dbd4_bool|...
      Datatypes2Map:='';
      for i:=0 to DatatypeMapping.Count-1 do
      begin
        Datatypes2Map:=Datatypes2Map+
          AnsiReplaceStr(DatatypeMapping.Names[i], '*', '\*');
        if(i<DatatypeMapping.Count-1)then
          Datatypes2Map:=Datatypes2Map+'|';
      end;

      //Build Variables.Expression dynamicly, because pointers to already
      //parsed structs might have been added
      Variables.Expression:='(?img)\s*('+Datatypes2Map+'|\w+ \*)\s*([\w\,\s]+)\;';

      if(Structs.Exec(SourceHeaderNoComments))then
      begin
        if(Not(MakeCfiles))then
          HeaderTxt.Add('');

        repeat
        begin
          //Matches
          // 0..complete match string
          // 1..variables
          // 2..struct name

          if(Not(MakeCfiles))then
            HeaderTxt.Add('  '+Structs.Match[2]+' = record');

          ClassVariables.Clear;
          ClassNestedStructLists.Clear;
          ClassNestedStructs.Clear;
          ClassLists.Clear;

          ClassNestedStructLists_Num_Vars.Clear;

          //Add pointer to this struct to datatype mapping
          DatatypeMapping.Add(Structs.Match[2]+' *=P'+Structs.Match[2]);

          //Add Datatype to NestedStructsPointers
          NestedStructsPointers.Add('P'+Structs.Match[2]);

          VarsWithLength.Clear;

          if(Variables.Exec(Structs.Match[1]))then
          begin
            repeat
            begin
              //Matches
              // 0..complete match string
              // 1..type
              // 2..variable names
              Datatype:=DatatypeMapping.Values[Variables.Match[1]];
              if(Datatype='')then
                Datatype:='Pointer';

              if(VarNames.Exec(Variables.Match[2]))then
              begin
                repeat
                begin
                  //Matches
                  // 0..complete match string
                  // 1..variable name

                  if(Not(MakeCfiles))then
                    HeaderTxt.Add('    '+VarNames.Match[1]+': '+Datatype+';');

                  if(Datatype='PChar')and
                    (VarsWithLength.IndexOf(VarNames.Match[1])>-1)then
                    Datatype:=binarydata_datatype
                  else
                  //use fix mapping for PChar
                  if(Datatype='PChar')then
                    Datatype:=string_datatype;

                  if(CompareText(Copy(VarNames.Match[1], Length(VarNames.Match[1])-6, 7), '_length')=0)then
                    VarsWithLength.Add(Copy(VarNames.Match[1], 1, Length(VarNames.Match[1])-7));

                  if(NestedStructsPointers.IndexOf(Datatype)<>-1)then
                  begin
                    //Check if there has already been a _num variable
                    if(ClassNestedStructLists_Num_Vars.IndexOf(Uppercase(VarNames.Match[1])+'_NUM')>-1)then
                      //if there is, add to Class' nested struct list
                      ClassNestedStructLists.Add(VarNames.Match[1]+'='+Copy(Datatype, 2, Length(Datatype)))
                    else
                      //if not, add to Class' nested struct
                      //(but only if it has no postfix _pointer)
                      if(CompareText(
                        Copy(VarNames.Match[1], Length(VarNames.Match[1])-7, 8),
                        '_pointer')<>0)then
                        ClassNestedStructs.Add(VarNames.Match[1]+'='+Copy(Datatype, 2, Length(Datatype)))
                      else
                        ClassVariables.Add(VarNames.Match[1]+'='+Datatype);
                  end
                  else
                    if((CompareText(Datatype, 'PPChar')=0)or
                      (CompareText(Datatype, 'PBoolean')=0)or
                      (CompareText(Datatype, 'PInteger')=0))then
                      ClassLists.Add(VarNames.Match[1]+'='+Datatype)
                    else
                    begin
                      //Add to Class' variables list
                      ClassVariables.Add(VarNames.Match[1]+'='+Datatype);

                      ClassNestedStructLists_Num_Vars.Add(Uppercase(VarNames.Match[1]));
                    end;
                end
                until not VarNames.ExecNext;
              end;
            end
            until not Variables.ExecNext;
          end;

          if(Not(MakeCfiles))then
          begin
            HeaderTxt.Add('  end;');
            HeaderTxt.Add('');
          end;

{          //Add pointer to this struct to datatype mapping
          DatatypeMapping.Add(Structs.Match[2]+' *=P'+Structs.Match[2]);

          //Add Datatype to NestedStructsPointers
          NestedStructsPointers.Add('P'+Structs.Match[2]);}


          //Build classes if C Header files contains the string
          //"[SCRIPT::LibInterfaceMapper] -generate_classes XXX"
          if(Pos(UpperCase('[SCRIPT::LibInterfaceMapper] -generate_classes '+Structs.Match[2]),
            UpperCase(SourceHeader))>0)then
            Build_Classes(Structs.Match[2],
              ClassDefinition, ClassImplementation,
              ClassVariables, ClassNestedStructLists, ClassLists, ClassNestedStructs,
              MakeCfiles);
        end
        until not Structs.ExecNext;
      end;

      //Create list of datatypes to map: int|xmlChar \*|dbd4_bool|...
      Datatypes2Map:='';
      for i:=0 to DatatypeMapping.Count-1 do
      begin
        Datatypes2Map:=Datatypes2Map+
          AnsiReplaceStr(DatatypeMapping.Names[i], '*', '\*');
        if(i<DatatypeMapping.Count-1)then
          Datatypes2Map:=Datatypes2Map+'|';
      end;

      if(Not(MakeCfiles))then
      begin
        //--------------------------------
        //Find Functions-Definitions

        FunctionDef:='';
        FunctionPointerDef:='';
        FunctionWrapperDef:='';

        FunctionWrapper:=LoadTextFromFile(TemplatePath+'function_wrapper.'+hFileExt);
        FunctionWrapperPChar:=LoadTextFromFile(TemplatePath+'function_pchar_wrapper.'+hFileExt);
        FunctionWrapperConstPChar:=LoadTextFromFile(TemplatePath+'function_const_pchar_wrapper.'+hFileExt);
        FunctionWrapperVoid:=LoadTextFromFile(TemplatePath+'function_void_wrapper.'+hFileExt);

        Funcs.Expression:='(?img)MYX_PUBLIC_FUNC\s{1,}(['+Datatypes2Map+'|\w*]{1,})\s{1,}(\w{1,})\(([\w\s\*\,\(\)\.]{0,})\)\;';

        if(Funcs.Exec(SourceHeaderNoComments))then
        begin
          //FuncParams.Expression:='(?img)(enum\s){0,1}\s{0,}(['+Datatypes2Map+']{1,})\s{1,}([\w\*]+)\s{0,}';
          //Added function pointers
          FuncParams.Expression:='(?img)(enum\s){0,1}\s{0,}((['+Datatypes2Map+
            ']{1,})\s{1,}([\w\*]+)|((\w+)\s*\(\s*\*\s*(\w+)\s*\)\s*(\((\s*[\w\*]+\s*\,{0,1})*\))))|(\.\.\.)\s{0,}';

          repeat
          begin
            //Matches
            // 0..complete match string
            // 1..function return type
            // 2..function name
            // 3..function parameters

            if(Funcs.Match[2]='myx_grt_call_object_method')then
              FuncParameter:='';

            FuncParameter:='';
            FuncParameterWidestrings:='';
            FuncParameterStringsEncoded:='';

            VarsWithLength.Clear;

            VarArgsFunc:=False;

            if(FuncParams.Exec(Funcs.Match[3]))then
            begin
              repeat
              begin
                //Matches
                // 0..complete match string
                // 1..enum
                // 2..complete match string
                // 3..datatype
                // 4..parameter
                // 5..complete match string
                // 6..function result type
                // 7..function name
                // 8..function parameter datatypes

                //Variable parameter
                if(FuncParams.Match[4]<>'')then
                begin
                  Parameter:=FuncParams.Match[4];
                  Datatypes2Map:=FuncParams.Match[3];

                  if(Pos('**', Parameter)>0)then
                  begin
                    Parameter:=Trim(AnsiReplaceText(Parameter, '*', ''));
                    Datatypes2Map:=Datatypes2Map+' **';
                  end;

                  if(Pos('*', Parameter)>0)then
                  begin
                    Parameter:=Trim(AnsiReplaceText(Parameter, '*', ''));
                    Datatypes2Map:=Datatypes2Map+' *';
                  end;


                  Datatype:=DatatypeMapping.Values[Datatypes2Map];

                  //ignore STRUCT* and STRUCT *
                  if(Datatype='')and(Pos('*', Datatypes2Map)>0)then
                    Datatype:=DatatypeMapping.Values[Copy(Datatypes2Map, 1, Pos('*', Datatypes2Map)-1)+' '+
                      Copy(Datatypes2Map, Pos('*', Datatypes2Map), Length(Datatypes2Map))];
                  if(Datatype='')and(Pos('**', Datatypes2Map)>0)then
                    Datatype:='PPointer'
                  else
                    if(Datatype='')and(Pos('*', Datatypes2Map)>0)then
                      Datatype:='Pointer'//'P'+Trim(AnsiReplaceText(Datatypes2Map, '*', ''))
                      //Accept enum as variable type
                    else
                      if(Datatype='')then
                        Datatype:=Datatypes2Map;

                  FuncParameter:=FuncParameter+
                    Parameter+': '+Datatype+'; ';

                  if(CompareText(Copy(Parameter, Length(Parameter)-6, 7), '_length')=0)then
                    VarsWithLength.Add(Copy(Parameter, 1, Length(Parameter)-7));

                  if(CompareText(Datatype, 'PChar')<>0)then
                  begin
                    FuncParameterWidestrings:=FuncParameterWidestrings+
                      Parameter+': '+Datatype+'; ';

                    FuncParameterStringsEncoded:=FuncParameterStringsEncoded+
                      Parameter+', '
                  end
                  else
                  begin
                    if(VarsWithLength.IndexOf(Parameter)>-1)then
                    begin
                      FuncParameterStringsEncoded:=FuncParameterStringsEncoded+
                        'PChar('+Parameter+'), ';

                      FuncParameterWidestrings:=FuncParameterWidestrings+
                        Parameter+': String; ';
                    end
                    else
                    begin
                      FuncParameterWidestrings:=FuncParameterWidestrings+
                        Parameter+': WideString; ';

                      FuncParameterStringsEncoded:=FuncParameterStringsEncoded+
                        'PChar(UTF8Encode('+Parameter+')), ';
                    end;
                  end;
                end
                else if(FuncParams.Match[10]='...')then
                begin
                  VarArgsFunc:=True;
                  {FuncParameter:=FuncParameter+
                    'args: PChar; ';

                  FuncParameterWidestrings:=FuncParameterWidestrings+
                    'args: PChar; ';

                  FuncParameterStringsEncoded:=FuncParameterStringsEncoded+
                    'args, ';}
                end
                else
                //function pointer
                begin
                  //remove ()
                  s:=Copy(FuncParams.Match[8], 2, Length(FuncParams.Match[8])-2);
                  FunctionPointerParams:='';
                  i:=1;
                  while(s<>'')do
                  begin
                    if(Pos(',', s)>0)then
                    begin
                      s2:=trim(Copy(s, 1, Pos(',', s)-1));
                      s:=Copy(s, Pos(',', s)+1, Length(s));
                    end
                    else
                    begin
                      s2:=trim(s);
                      s:='';
                    end;

                    //filter variable name out
                    s3:='';
                    if(Pos(' ', s2)>0)then
                    begin
                      s3:=s2;
                      while(Pos(' ', s3)>0)do
                        s3:=Copy(s3, Pos(' ', s3)+1, Length(s3));

                      if (Copy(s3, 1, 2)='**') then
                        s3:=Copy(s3, 3, Length(s3))
                      else
                        if (Copy(s3, 1, 1)='*') then
                          s3:=Copy(s3, 2, Length(s3));

                      s2:=Trim(Copy(s2, 1, Length(s2)-Length(s3)));
                    end;

                    if(trim(s3)='')then
                      s3:='Param'+IntToStr(i);

                    s4:=DatatypeMapping.Values[trim(s2)];
                    if ((s4='') and (Pos('**', s2)>0)) then
                      s4:='PPointer'
                    else
                      if(s4='')then
                        s4:='Pointer';

                    FunctionPointerParams:=FunctionPointerParams+
                      s3+': '+s4;
                    if(s<>'')then
                      FunctionPointerParams:=FunctionPointerParams+'; ';

                    inc(i);
                  end;

                  if(CompareText(trim(FuncParams.Match[6]), 'void')<>0)then
                    newFunctionPointerDef:=
                      '  T'+FuncParams.Match[7]+' = function('+
                      FunctionPointerParams+'):'+
                      DatatypeMapping.Values[FuncParams.Match[6]]+
                      ' cdecl;'
                  else
                    newFunctionPointerDef:=
                      '  T'+FuncParams.Match[7]+' = procedure('+
                      FunctionPointerParams+') cdecl;';
                  newFunctionPointerDef:=newFunctionPointerDef+#13#10;

                  if(Pos(newFunctionPointerDef, FunctionPointerDef)=0)then
                    FunctionPointerDef:=FunctionPointerDef+
                      newFunctionPointerDef;

                  FuncParameter:=FuncParameter+
                    FuncParams.Match[7]+': T'+FuncParams.Match[7]+'; ';

                  FuncParameterWidestrings:=FuncParameterWidestrings+
                    FuncParams.Match[7]+': T'+FuncParams.Match[7]+'; ';

                  FuncParameterStringsEncoded:=FuncParameterStringsEncoded+
                    FuncParams.Match[7]+', ';
                end;
              end
              until not FuncParams.ExecNext;
            end;

            //Remove last ,
            FuncParameter:=Copy(FuncParameter, 1, Length(FuncParameter)-2);
            FuncParameterWidestrings:=Copy(FuncParameterWidestrings, 1, Length(FuncParameterWidestrings)-2);
            FuncParameterStringsEncoded:=Copy(FuncParameterStringsEncoded, 1,
              Length(FuncParameterStringsEncoded)-2);

            Datatype:=DatatypeMapping.Values[Funcs.Match[1]];
            if(Datatype='')and(Pos('*', Funcs.Match[1])>0)then
              Datatype:=DatatypeMapping.Values[Copy(Funcs.Match[1], 1, Pos('*', Funcs.Match[1])-1)+' '+
                Copy(Funcs.Match[1], Pos('*', Funcs.Match[1]), Length(Funcs.Match[1]))];
            if(Datatype='')and(Pos('*', Funcs.Match[1])>0)then
              Datatype:='Pointer'
            else if(Datatype='')then
              Datatype:=Funcs.Match[1];

            if(VarArgsFunc)then
              s:='cdecl varargs;'
            else
              s:='cdecl;';
              
            if(Datatype='void')then
            begin
              if(Not(DoDynamicDllLoading))then
              begin
                if(Pos('PChar', FuncParameter)>0)then
                begin
                  FunctionDef:=FunctionDef+
                    'procedure _'+Funcs.Match[2]+'('+FuncParameter+'); '+
                    s+' external '''+LibInterfaceName+'.dll'' name ''_' + Funcs.Match[2]+''';'+#13#10;

                  {FuncParameterWidestrings:=
                    AnsiReplaceText(FuncParameter,
                      'PChar', 'WideString');}

                  FunctionDef:=FunctionDef+
                    'procedure '+Funcs.Match[2]+'('+FuncParameterWidestrings+');'+#13#10;

                  FunctionWrapperDef:=FunctionWrapperDef+
                    AnsiReplaceText(
                      AnsiReplaceText(
                        AnsiReplaceText(FunctionWrapperVoid,
                          '%func_name%', Funcs.Match[2]),
                        '%func_params_strings_encoded%', FuncParameterStringsEncoded),
                      '%func_params_widestrings%', FuncParameterWidestrings);
                end
                else
                begin
                  FunctionDef:=FunctionDef+
                    'procedure '+Funcs.Match[2]+'('+FuncParameter+'); '+
                    s+' external '''+LibInterfaceName+'.dll'' name ''_' + Funcs.Match[2]+ ''';'+#13#10;
                end;
              end
              else
              begin
                FunctionDef:=FunctionDef+
                  'T'+Funcs.Match[2]+' = '+
                  'procedure ('+FuncParameter+'); cdecl;'+#13#10;
              end;
            end
            else
            begin
              if(Not(DoDynamicDllLoading))then
              begin
                if(Pos('PChar', FuncParameter)>0)or(Datatype='PChar')then
                begin
                  FunctionDef:=FunctionDef+
                    'function _'+Funcs.Match[2]+'('+FuncParameter+'): '+
                    Datatype+'; '+s+' external '''+LibInterfaceName+'.dll'' name ''_'+Funcs.Match[2]+''';'+#13#10;

                  {FuncParameterWidestrings:=
                    AnsiReplaceText(FuncParameter,
                      'PChar', 'WideString');}

                  if(Datatype='PChar') and
                    (Not(SameText(Funcs.Match[1], 'const char *'))) then
                  begin
                    FunctionDef:=FunctionDef+
                      'function '+Funcs.Match[2]+'('+FuncParameterWidestrings+'): WideString;'+#13#10;

                    FunctionWrapperDef:=FunctionWrapperDef+
                      AnsiReplaceText(
                        AnsiReplaceText(
                          AnsiReplaceText(FunctionWrapperPChar,
                            '%func_name%', Funcs.Match[2]),
                          '%func_params_strings_encoded%', FuncParameterStringsEncoded),
                        '%func_params_widestrings%', FuncParameterWidestrings);

                  end
                  else if (SameText(Funcs.Match[1], 'const char *')) then
                  begin
                    FunctionDef:=FunctionDef+
                      'function '+Funcs.Match[2]+'('+FuncParameterWidestrings+'): WideString;'+#13#10;

                    FunctionWrapperDef:=FunctionWrapperDef+
                      AnsiReplaceText(
                        AnsiReplaceText(
                          AnsiReplaceText(FunctionWrapperConstPChar,
                            '%func_name%', Funcs.Match[2]),
                          '%func_params_strings_encoded%', FuncParameterStringsEncoded),
                        '%func_params_widestrings%', FuncParameterWidestrings);
                  end
                  else
                  begin
                    FunctionDef:=FunctionDef+
                      'function '+Funcs.Match[2]+'('+FuncParameterWidestrings+'): '+Datatype+';'+#13#10;

                    FunctionWrapperDef:=FunctionWrapperDef+
                      AnsiReplaceText(
                        AnsiReplaceText(
                          AnsiReplaceText(
                            AnsiReplaceText(FunctionWrapper,
                              '%func_name%', Funcs.Match[2]),
                            '%func_returnvalue_type%', Datatype),
                          '%func_params_strings_encoded%', FuncParameterStringsEncoded),
                        '%func_params_widestrings%', FuncParameterWidestrings);
                  end;
                end
                else
                begin
                  FunctionDef:=FunctionDef+
                    'function '+Funcs.Match[2]+'('+FuncParameter+'): '+
                    Datatype+'; '+s+' external '''+LibInterfaceName+'.dll'' name ''_' + Funcs.Match[2] + ''';'+#13#10;
                end;
              end
              else
              begin
                FunctionDef:=FunctionDef+
                  'T'+Funcs.Match[2]+' = '+
                  'function ('+FuncParameter+'): '+Datatype+'; cdecl;'+#13#10;
              end;
            end;

          end
          until not Funcs.ExecNext;
        end;

        if(FunctionPointerDef<>'')then
          HeaderTxt.Add('type'#13#10+
            FunctionPointerDef+#13#10);

        if(FunctionDef<>'')then
          HeaderTxt.Add(FunctionDef);

        if(FunctionWrapperDef<>'')then
          ClassImplementation.Add(FunctionWrapperDef);
      end;

      if(IncFileName<>'')then
      begin
        if(MakeCfiles)then
        begin
          HeaderTxt.Text:=AnsiReplaceText(
            AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(LoadTextFromFile(TemplatePath+'header.'+hFileExt),
                  '%class%', ClassDefinition.Text),
                '%version_definition%', HeaderTxt.Text),
              '%original_headerfile%', HeaderFileName),
            '%filename%', ChangeFileExt(IncFileName, ''));
        end;

        if(ExtractFilePath(IncFileName)<>'')then
          if(Not(DirectoryExists(ExtractFilePath(IncFileName))))then
          begin
            WriteLn('Creating output directory '+GetCurrentDir+PathDelim+ExtractFilePath(IncFileName));
            try
              ForceDirectories(GetCurrentDir+PathDelim+ExtractFilePath(IncFileName));
              except
                WriteLn('Error creating directory '+
                  GetCurrentDir+PathDelim+ExtractFilePath(IncFileName)+'.');
                ExitCode:=1;
                Exit;
              end;
          end;
        HeaderTxt.SaveToFile(IncFileName);

        if(Not(MakeCfiles))then
          WriteLn('Pascal Include file ['+IncFileName+'] created.')
        else
          WriteLn('C++ Header file ['+IncFileName+'] created.')
      end;

      if(UnitFileName<>'')then
      begin
        if((FileExists(TemplatePath+'unit.'+cFileExt))and
            (Not(DoDynamicDllLoading)))or
          ((FileExists(TemplatePath+'unit_dynamic_dll_loading.'+cFileExt))and
            (DoDynamicDllLoading))then
        begin
          if(DoDynamicDllLoading)then
            UnitText.LoadFromFile(TemplatePath+'unit_dynamic_dll_loading.'+cFileExt)
          else
            UnitText.LoadFromFile(TemplatePath+'unit.'+cFileExt);
          UnitText.Text:=
            AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(
                  AnsiReplaceText(
                    AnsiReplaceText(UnitText.Text,
                      '%file_name%', ChangeFileExt(ExtractFileName(UnitFileName), '')),
                    '%header%', HeaderTxt.Text),
                  '%class%', ClassDefinition.Text),
                '%AddToUses%', AddToUses),
              '%classimp%', ClassImplementation.Text);

          if(ExtractFilePath(UnitFileName)<>'')then
            if(Not(DirectoryExists(ExtractFilePath(UnitFileName))))then
            begin
              WriteLn('Creating output directory '+GetCurrentDir+PathDelim+ExtractFilePath(UnitFileName));
              try
                ForceDirectories(GetCurrentDir+PathDelim+
                  ExtractFilePath(UnitFileName));
              except
                WriteLn('Error creating directory '+
                  GetCurrentDir+PathDelim+ExtractFilePath(UnitFileName)+'.');
                ExitCode:=1;
                Exit;
              end;
            end;

          UnitText.SaveToFile(UnitFileName);
          if(not(MakeCfiles))then
            WriteLn('Pascal Unit file ['+UnitFileName+'] created.')
          else
            WriteLn('C++ file ['+UnitFileName+'] created.');
        end
        else
          WriteLn('File ['+TemplatePath+'unit.'+cFileExt+'] not found.');
      end;

    finally
      VarNames.Free;
      Variables.Free;
      Structs.Free;
      Enums.Free;
      RegExpr.Free;
      Funcs.Free;
      FuncParams.Free;
    end;
  finally
    HeaderTxt.Free;
    DatatypeMapping.Free;
    NestedStructsPointers.Free;
    ClassNestedStructLists.Free;
    ClassVariables.Free;
    ClassLists.Free;
    ClassDefinition.Free;
    ClassImplementation.Free;
    UnitText.Free;
    FuncDefs.Free;
    ClassNestedStructLists_Num_Vars.Free;
    ClassNestedStructs.Free;
    VarsWithLength.Free;
  end;
end;

procedure DisplayUsage;
begin
  WriteLn('mysqllibinterfacemapper Ver 1.0.1.0, for Win2k/XP, by Michael Zinner');
  WriteLn('This software comes with ABSOLUTELY NO WARRANTY. This is free software,');
  WriteLn('and you are welcome to modify and redistribute it under the GPL license');
  WriteLn('');
  WriteLn('Converting C Header file to Pascal Include/Unit file and C++ Source/Header file');
  WriteLn('Usage: mysqllibinterface_mapper -h[c header file] -u[pascal unit file] ');
  WriteLn('       -i[pascal include file] [-v] -c[c++ file] -e[c++ header file]');
  WriteLn('');
  WriteLn('       -v can be used to ignore missing interface definition');
end;

procedure ParseParams;
var headerfile,
  unitfile, incfile,
  cppfile, cppheaderfile: string;
  i: integer;
  somethingToDo,
  makeUnitFile,
  makeIncludeFile,
  makeCPPFile,
  makeCPPHeaderFile,
  IgnoreMissingLIB_INTERFACE_VERSION: Boolean;
  DoDynamicDllLoading: Boolean;
begin
  headerfile:='';
  unitfile:='';
  incfile:='';
  cppfile:='';
  cppheaderfile:='';
  somethingToDo:=False;
  makeUnitFile:=False;
  makeIncludeFile:=False;
  makeCPPFile:=False;
  makeCPPHeaderFile:=False;
  IgnoreMissingLIB_INTERFACE_VERSION:=False;
  DoDynamicDllLoading:=False;

  //Check all parameters
  for i:=1 to ParamCount do
  begin
    //-h Source Header file
    if(Copy(ParamStr(i), 1, 2)='-h')then
    begin
      headerfile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);

      if(Not(FileExists(headerfile)))then
      begin
        WriteLn('ERROR: C-header file ['+headerfile+'] cannot be found.');

        headerfile:='';

        ExitCode:=1;

        break;
      end;
    end;

    //-d Dynamic DLL Loading
    if(ParamStr(i)='-d')then
    begin
      DoDynamicDllLoading:=True;
    end;

    //-u Destination Unit file
    if(Copy(ParamStr(i), 1, 2)='-u')then
    begin
      somethingToDo:=True;
      makeUnitFile:=True;

      unitfile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
    end;

    //-i Destination Include file
    if(Copy(ParamStr(i), 1, 2)='-i')then
    begin
      somethingToDo:=True;
      makeIncludeFile:=True;

      incfile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
    end;

    //-c Destination c++ file
    if(Copy(ParamStr(i), 1, 2)='-c')then
    begin
      somethingToDo:=True;
      makeCPPFile:=True;

      cppfile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
    end;

    //-e Destination c++ header file
    if(Copy(ParamStr(i), 1, 2)='-e')then
    begin
      somethingToDo:=True;
      makeCPPHeaderFile:=True;

      cppheaderfile:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);
    end;

    //-v Ignore missing LIB_INTERFACE_VERSION
    if(Copy(ParamStr(i), 1, 2)='-v')then
    begin
      IgnoreMissingLIB_INTERFACE_VERSION:=True;
    end;
  end;

  //Make Pascal include file
  if((makeUnitFile)or(makeIncludeFile))and(headerfile<>'')then
  begin
    if(unitfile='')and(makeUnitFile)then
      unitfile:=ChangeFileExt(headerfile, '.pas');
    if(incfile='')and(makeIncludeFile)then
      incfile:=ChangeFileExt(headerfile, '.inc');

    ENUM_Struct_C2Pas(headerfile, unitfile, incfile,
      IgnoreMissingLIB_INTERFACE_VERSION, DoDynamicDllLoading);
  end;

  //Make c++ file
  if((makeCPPFile)or(makeCPPHeaderFile))and(headerfile<>'')then
  begin
    if(cppfile='')and(makeCPPFile)then
      cppfile:=ChangeFileExt(headerfile, '.cpp');
    if(cppheaderfile='')and(makeCPPHeaderFile)then
      cppheaderfile:=ChangeFileExt(headerfile, '.h');

    ENUM_Struct_C2Pas(headerfile, cppfile, cppheaderfile,
      IgnoreMissingLIB_INTERFACE_VERSION, False,
      True);
  end;

  if(Not(somethingToDo))then
    DisplayUsage;
end;

begin
  //If params are specified
  if(ParamCount>0)and(ParamStr(1)<>'--help')then
    ParseParams
  else
    //else write usage
    DisplayUsage;
end.
