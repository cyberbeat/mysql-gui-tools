unit AuxFuncsLibMapper;

interface

uses
  SysUtils,
  RegExpr,
  Classes,
  IniFiles,
  StrUtils;

function LoadTextFromFile(filename: string): string;
procedure RemoveTrailingLineWrap(var s: string; keyword: string);
function Get_Ini_Section(file_name, section: string): string;
function GetDatatypeDefinitionFromFile(filename: string;
  NestedStructsPointers: TStringList): string;

implementation

function LoadTextFromFile(filename: string): string;
var Stream: TStream;
  Size: Integer;
  S: string;
begin
  if(FileExists(filename))then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size - Stream.Position;
      SetString(S, nil, Size);
      Stream.Read(Pointer(S)^, Size);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    S:='';
    WriteLn('ERROR: ['+filename+'] not found.');
    ExitCode:=1;
  end;

  LoadTextFromFile:=S;
end;

procedure RemoveTrailingLineWrap(var s: string; keyword: string);
begin
  while(Pos(keyword+#13#10, s)>0)do
    s:=Copy(s, 1, Pos(keyword+#13#10, s)+Length(keyword)-1)+
      Copy(s, Pos(keyword+#13#10, s)+Length(keyword)+2, Length(s));
end;

function Get_Ini_Section(file_name, section: string): string;
var DatatypeMappingIni: TMemIniFile;
  theValues: TStringList;
begin
  Result:='';

  DatatypeMappingIni:=TMemIniFile.Create(file_name);
  theValues:=TStringList.Create;
  try
    DatatypeMappingIni.ReadSectionValues(section,
      theValues);

    Result:=theValues.Text;
  finally
    DatatypeMappingIni.Free;
    theValues.Free;
  end;
end;

function GetDatatypeDefinitionFromFile(filename: string;
  NestedStructsPointers: TStringList): string;
var Source, SourceNoComments: string;
  RegExpr, Enums, Structs: TRegExpr;
  DatatypeMapping: TStringList;
begin
  DatatypeMapping:=TStringList.Create;
  RegExpr:=TRegExpr.Create;
  Enums:=TRegExpr.Create;
  Structs:=TRegExpr.Create;
  try
    Source:=LoadTextFromFile(filename);

    //-------------------------------------------------------------
    //Remove comments from file
    RegExpr.Expression:='(?img)(/\*([\w\s=\,\(\)\;/\\\.\-\''\?\:\*\<\>\#\!]+?)\*/)|'+
      '(//([\w\t \=\,\(\)\;/\\\.\-\''\?\:\*\<\>\#\!]+))';

    SourceNoComments:=RegExpr.Replace(Source, '', True);

    //-------------------------------------------------------------
    //Scan through file

    Enums.Expression:='(?img)(typedef enum|^enum)\s{0,}(\w+){0,}\s{0,}\{'+
      '([a-zA-Z0-9_\s=\,]+)\}\s*(\w*)\;';

    //--------------------------------
    //Find Enum-Definitions
    if(Enums.Exec(SourceNoComments))then
    begin
      repeat
      begin
        //Matches
        // 0..complete match string
        // 1..typedef enum/enum
        // 2..enum name
        // 3..enum values
        // 4..enum type name

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

    //--------------------------------
    //Find Struct-Definition

    Structs.Expression:='(?img)typedef\s{0,}struct\s{0,}\w{0,}\s{0,}\{'+
      '\s{0,}([\w\s\*\;\,/\?\(\)\.\[\]]+)\}\s(\w+){1,}\;\s*';

    //Scan for Datatype mapping
    if(Structs.Exec(SourceNoComments))then
    begin
      repeat
      begin
        //Matches
        // 0..complete match string
        // 1..variables
        // 2..struct name

        //Add pointer to this struct to datatype mapping
        DatatypeMapping.Add(Structs.Match[2]+' *=P'+Structs.Match[2]);
        DatatypeMapping.Add('struct '+Structs.Match[2]+' *=P'+Structs.Match[2]);

        //Add Datatype to NestedStructsPointers
        NestedStructsPointers.Add('P'+Structs.Match[2]);
      end
      until not Structs.ExecNext;
    end;

    Result:=DatatypeMapping.Text;
  finally
    DatatypeMapping.Free;
    RegExpr.Free;
    Enums.Free;
    Structs.Free;
  end;
end;

end.
