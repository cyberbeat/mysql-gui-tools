program MySQLXMLIndexGen;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  RegExpr in '..\LibInterfaceMapper\RegExpr.pas',
  StrUtils;

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
  end;

  LoadTextFromFile:=S;
end;

procedure CreateFunctionXMLIndex(filename: string; DestFilename: string;
  IndexFilename: string);
var Funcs, SubFuncs: TRegExpr;
  XmlIndex, FunctionGroups: TStringList;
  SourceText, SubStr, LastID, s: string;
  CurrentFunctionGroup: integer;
  LastPos: integer;
begin
  SourceText:=LoadTextFromFile(filename);

  DeleteFile(DestFilename);

  XmlIndex:=TStringList.Create;
  FunctionGroups:=TStringList.Create;
  Funcs:=TRegExpr.Create;
  SubFuncs:=TRegExpr.Create;
  try
    //Correct <a>s in HTML
    Funcs.Expression:='<DT><code>([^<]*?)</code>\s*<DD>\s*<A NAME="([\w\d]+)"></A>';
    SourceText:=Funcs.Replace(SourceText, '<A NAME="$2"><DT><code>$1</code><DD></A>', True);

    Funcs.Expression:='<A NAME="([\w\d]+)"></A>\s*<DT><code>([^<]*?)</code>\s*<DD>';
    SourceText:=Funcs.Replace(SourceText, '<A NAME="$1"><DT><code>$2</code><DD></A>', True);

    //Find Function Groups (H2 titles)
    Funcs.Expression:='<H2><.*?>[\d\.]*\s+(.*?)</A></H2>';
    if(Funcs.Exec(SourceText))then
    begin
      repeat
      begin
        //Matches
        // 0..complete match string
        // 1..H2 title (without number)

        s:=AnsiReplaceText(
          AnsiReplaceText(Funcs.Match[1], '<code>', ''),
            '</code>', '');

        FunctionGroups.Add(s+'='+
          IntToStr(Funcs.MatchPos[0]));
      end
      until not Funcs.ExecNext;
    end;

    if(FunctionGroups.Count=0)then
      Exit;

      
    //Find functions
    Funcs.Expression:='<A NAME="([\w\d]+)"><DT><code>([^<]*?)</code>\s*<DD></A>';
    SubFuncs.Expression:='<DT><code>([^<]*?)</code>\s*<DD>';

    XmlIndex.Add('<?xml version="1.0" ?>');
    XmlIndex.Add('<functionindex>');

    CurrentFunctionGroup:=0;
    XmlIndex.Add('  <functiongroup name="'+FunctionGroups.Names[CurrentFunctionGroup]+'">');

    LastPos:=0;

    if(Funcs.Exec(SourceText))then
    begin
      repeat
      begin
        //Matches
        // 0..complete match string
        // 1..id
        // 2..function+params
        // 3..function parameter

        //Search for Funcs with same ID
        if(LastPos>0)then
        begin
          SubStr:=Copy(SourceText, LastPos,
            Funcs.MatchPos[0]-LastPos);

          if(SubFuncs.Exec(SubStr))then
          repeat
          begin
            //Matches
            // 0..complete match string
            // 2..function+params
            // 3..function parameter

            //Check if still in FunctionGroups
            if(CurrentFunctionGroup+1<FunctionGroups.Count)then
            begin
              if(LastPos+SubFuncs.MatchPos[0]>StrToInt(FunctionGroups.ValueFromIndex[CurrentFunctionGroup+1]))then
              begin
                if(CurrentFunctionGroup+1<FunctionGroups.Count)then
                  inc(CurrentFunctionGroup);

                if(CurrentFunctionGroup<FunctionGroups.Count)then
                begin
                  XmlIndex.Add('  </functiongroup>');
                  XmlIndex.Add('  <functiongroup name="'+FunctionGroups.Names[CurrentFunctionGroup]+
                    '">');
                end;
              end;
            end;

            s:=AnsiReplaceText(
              AnsiReplaceText(
                AnsiReplaceText(SubFuncs.Match[1], '<code>', ''),
                  '"', '&#34;'),
                '</code>', '');

            XmlIndex.Add('    <function caption="'+s+'" '+
              'id="'+LastID+'" />');
          end
          until not SubFuncs.ExecNext;
        end;

        //Check if still in FunctionGroups
        if(CurrentFunctionGroup+1<FunctionGroups.Count)then
        begin
          if(Funcs.MatchPos[0]>StrToInt(FunctionGroups.ValueFromIndex[CurrentFunctionGroup+1]))then
          begin
            if(CurrentFunctionGroup+1<FunctionGroups.Count)then
              inc(CurrentFunctionGroup);

            if(CurrentFunctionGroup<FunctionGroups.Count)then
            begin
              XmlIndex.Add('  </functiongroup>');
              XmlIndex.Add('  <functiongroup name="'+FunctionGroups.Names[CurrentFunctionGroup]+
                '">');
            end;
          end;
        end;

        s:=AnsiReplaceText(
          AnsiReplaceText(
            AnsiReplaceText(Funcs.Match[2], '<code>', ''),
              '"', '&#34;'),
            '</code>', '');

        XmlIndex.Add('    <function caption="'+s+'" '+
          'id="'+Funcs.Match[1]+'" />');

        LastPos:=Funcs.MatchPos[0]+Funcs.MatchLen[0];
        LastID:=Funcs.Match[1];
      end
      until not Funcs.ExecNext;
    end;

    XmlIndex.Add('  </functiongroup>');
    XmlIndex.Add('</functionindex>');

    XmlIndex.SaveToFile(IndexFilename);

    //Reformat HTML
    Funcs.Expression:='<A NAME="([\w\d]+)"><DT><code>([^<]*?)</code><DD></A>';
    SourceText:=Funcs.Replace(SourceText, '<br><br><br><A NAME="$1">'+
      '<font style="font-size: 4px"><br></font>'+#13#10+
      '<DT>'+#13#10+
      '<table border="0" cellspacing="0" cellpadding="3" width="100%" bgcolor="#99CCFF" bordercolor="#999999" style="border-color:#999999; border-style:solid; border-width:1;">'+#13#10+
      ' <tr>'+#13#10+
      '   <td><i>MySQL Inline-Help Reference - SQL Functions</i></td>'+
      ' </tr>'+#13#10+
      ' <tr>'+#13#10+
      '   <td><b><code>$2</code></b></td>'+#13#10+
      ' </tr>'+#13#10+
      '</table>'+#13#10+
      '<br>'+#13#10+
      '<code><b>$2</b></code><DD></A>', True);

    Funcs.Expression:='<DT><code>([^<]*?)</code>\s*<DD>';
    SourceText:=Funcs.Replace(SourceText, '<DT><code><b>$1</b></code><DD>', True);

    //Remove links
    Funcs.Expression:='(?img)<a href="([^"]*?)">([^<]*?)</a>';
    SourceText:=Funcs.Replace(SourceText, '$2', True);

    XmlIndex.Text:=SourceText;
    XmlIndex.SaveToFile(DestFilename);

  finally
    Funcs.Free;
    SubFuncs.Free;
    FunctionGroups.Free;
    XmlIndex.Free;
  end;
end;

procedure DisplayUsage;
begin
  WriteLn('mysqlxmlindexgen Ver 1.0.0.0, for Win2k/XP, by Michael Zinner');
  WriteLn('This software comes with ABSOLUTELY NO WARRANTY. This is free software,');
  WriteLn('and you are welcome to modify and redistribute it under the GPL license');
  WriteLn('');
  WriteLn('Creating XML index files:');
  WriteLn('Usage: mysqlxmlindexgen -s[source file] -f -d[dest file] -i[Index file]');
  WriteLn('       -f generates a function XML index file, -d specifies the');
  WriteLn('       new formated HTML file and -i the XML index file');
end;

procedure ParseParams;
var somethingToDo,
  DoFunctionXMLIndex: Boolean;
  SourceFilename,
  DestFilename,
  IndexFilename: string;
  i: integer;
begin
  somethingToDo:=False;
  DoFunctionXMLIndex:=False;
  SourceFilename:='';
  DestFilename:='';
  IndexFilename:='';

  //Check all parameters
  for i:=1 to ParamCount do
  begin
    //-s Source file
    if(Copy(ParamStr(i), 1, 2)='-s')then
    begin
      SourceFilename:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);

      if(Not(FileExists(SourceFilename)))then
      begin
        WriteLn('ERROR: Source file ['+SourceFilename+'] cannot be found.');

        SourceFilename:='';

        break;
      end;
    end;

    //-d Destination file
    if(Copy(ParamStr(i), 1, 2)='-d')then
      DestFilename:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);

    //-i Index file
    if(Copy(ParamStr(i), 1, 2)='-i')then
      IndexFilename:=Copy(ParamStr(i), 3, Length(ParamStr(i))-1);

    //-f Do function XML Index
    if(Copy(ParamStr(i), 1, 2)='-f')then
    begin
      DoFunctionXMLIndex:=True;
    end;
  end;

  if(DoFunctionXMLIndex)and(SourceFilename<>'')then
  begin
    if(DestFilename='')then
      DestFilename:=ChangeFileExt(SourceFilename, '_gui.html');

    if(IndexFilename='')then
      IndexFilename:=ChangeFileExt(SourceFilename, '.xml');

    CreateFunctionXMLIndex(SourceFilename, DestFilename, IndexFilename);
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
