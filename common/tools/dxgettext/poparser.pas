unit poparser;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

// This assumes utf-8 only in the .po files

interface

uses
  Classes;

type
  TPoEntry=
    class
    public
      UserCommentList:TStringList;   // Entire lines, utf8!
      AutoCommentList:TStringList;   // Entire lines, utf8!
      MsgId:widestring;              // singular and plural are separated by #0, if plural form is present
      MsgStr:widestring;             // plural forms are separated by #0, if present
      constructor Create;
      destructor Destroy; override;
      procedure Assign (po:TPoEntry);
      procedure Clear;
      procedure WriteToStream (str:TStream);  // Adds an empty line afterwards.
    end;
  TPoEntryList=
    class
    private
      list:TStringList; // Strings are searchkeys, objects are TList of TPoEntries
      function GetSearchKey (MsgId:widestring):utf8string;
    public
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile (filename:string);
      procedure SaveToFile (filename:string);
      procedure Clear;
      function Find (MsgId:widestring):TPoEntry;
      function Delete (MsgId:widestring):boolean;  // True if found and deleted, false if not found
      procedure Add (entry:TPoEntry); // Will fail if MsgId exists. Entry is copied.

      // Iterate through all items. When nil is returned, no more elements are there.
      function FindFirst:TPoEntry;
      function FindNext (po:TPoEntry):TPoEntry;
    end;
  // Easy to use parser for .po files. Just put all the lines into AddLine(),
  // and each time a translation has been found, it turns true once.
  // Always end your parsing by putting an empty line into Addline.
  TPoParser=
    class
    private
      LineNumber:Integer;
      IsMsgId:boolean;
      entry:TPoEntry;  // This is the return value if last AddLine returned True
      entryhasdata:boolean;
    public
      constructor Create;
      destructor Destroy; override;

      // Put all your lines into AddLine(). It will return nil most
      // of the times, but it will return an entry each time the whitespace
      // after an entry has been reached. The entry is only valid until the next
      // call to AddLine().
      function AddLine (line:utf8string):TPoEntry;

      // Read a couple of lines from file and return next TPoEntry. Returns nil if no more entries.
      function ReadNextEntry (var tf:TextFile):TPoEntry;
      property CurrentLineNumber:integer read LineNumber;
    end;


procedure StreamWrite (s:TStream;line:utf8string);
procedure StreamWriteln (s:TStream;line:utf8string='');
procedure StreamWriteMinimumPoHeader (s:TStream;appname:widestring);
procedure StreamWriteDefaultPoTemplateHeader (s:TStream;appname:widestring);


implementation

uses
  Math, SysUtils, gnugettext;

procedure StreamWriteMinimumPoHeader (s:TStream;appname:widestring);
begin
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  StreamWriteln(s, '"POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"Last-Translator: Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"MIME-Version: 1.0\n"');
  StreamWriteln(s, '"Content-Type: text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"Content-Transfer-Encoding: 8bit\n"');
  StreamWriteln(s, '"X-Generator: ' + utf8encode(appname) + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteDefaultPoTemplateHeader (s:TStream;appname:widestring);
begin
  StreamWriteln(s, '# SOME DESCRIPTIVE TITLE.');
  StreamWriteln(s, '# Copyright (C) YEAR THE PACKAGE''S COPYRIGHT HOLDER');
  StreamWriteln(s, '# This file is distributed under the same license as the PACKAGE package.');
  StreamWriteln(s, '# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.');
  StreamWriteln(s, '#');
  StreamWriteln(s, 'msgid ""');
  StreamWriteln(s, 'msgstr ""');
  StreamWriteln(s, '"Project-Id-Version: PACKAGE VERSION\n"');
  StreamWriteln(s, '"POT-Creation-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"PO-Revision-Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', now) + '\n"');
  StreamWriteln(s, '"Last-Translator: Somebody <your.email@address.com>\n"');
  StreamWriteln(s, '"MIME-Version: 1.0\n"');
  StreamWriteln(s, '"Content-Type: text/plain; charset=UTF-8\n"');
  StreamWriteln(s, '"Content-Transfer-Encoding: 8bit\n"');
  StreamWriteln(s, '"X-Generator: ' + utf8encode(appname) + '\n"');
  Streamwriteln(s, '');
end;

procedure StreamWriteln (s:TStream;line:utf8string='');
begin
  StreamWrite (s, line);
  StreamWrite (s, sLineBreak);
end;

procedure StreamWrite (s:TStream;line:utf8string);
var
  len:integer;
begin
  len:=length(line);
  if len>0 then
    if s.Write(line[1],len)<>len then
      raise Exception.Create (_('Error when writing to stream.'));
end;

function String2PO (s:utf8string):utf8string;
// Converts a string to the syntax that is used in .po files
var
  i: integer;
  c: char;
  escnext:boolean;
begin
  Result := '';
  escnext:=False;
  for i := 1 to length(s) do begin
    c := s[i];
    case c of
      #32..#33, #35..pred('\'),succ('\')..#255:
        begin
          if escnext then Result:=Result+'\';
          Result := Result + c;
          escnext:=False;
        end;
      '\':begin
            Result:=Result+'\\';
            escnext:=False;
          end;
      #13:; // Do nothing
      #10:begin
            Result := Result + '\n';
            escnext:=False;
          end;
      #34:begin
            Result := Result + '\"';
            escnext:=False;
          end;
      #0:begin
           Result := Result + '\0';
           escnext:=True;
         end;
      #9:begin
           Result:=Result+'\t';
           escnext:=False;
         end;
    else
      Result := Result + '\x' + IntToHex(ord(c),2);
      escnext:=True;
    end;
  end;
  Result := '"' + Result + '"';
end;

{ TPoParser }

function TPoParser.AddLine(line: utf8string):TPoEntry;
var
  i:integer;
  value:utf8string;
begin
  try
    Inc (LineNumber);
    line:=trim(line);
    if line<>'' then begin
      if not entryhasdata then begin
        entry.Clear;
        entryhasdata:=False;
      end;
      if copy(line,1,2)='# ' then
        entry.UserCommentList.Add(line)
      else
      if copy(line,1,1)='#' then
        entry.AutoCommentList.Add(line)
      else begin
        if uppercase(copy(line,1,12))='MSGID_PLURAL' then begin
          IsMsgId:=True;
          delete (line,1,12);
          line:=trim(line);
          entry.MsgId:=entry.MsgId+#0;
        end;
        if uppercase(copy(line,1,5))='MSGID' then begin
          IsMsgId:=True;
          delete (line,1,5);
          line:=trim(line);
        end;
        if uppercase(copy(line,1,6))='MSGSTR' then begin
          IsMsgId:=False;
          delete (line,1,6);
          if copy(line,1,1)='[' then begin
            if copy(line,2,1)<>'0' then
              entry.MsgStr:=entry.MsgStr+#0;
            delete (line,1,3);
          end;
          line:=trim(line);
        end;
        if (copy(line,1,1)<>'"') or (copy(line,length(line),1)<>'"') then
          raise Exception.Create (Format(_('Illegal line: %s'),[line]));
        value:=copy(line,2,length(line)-2);
        i:=1;
        while i<length(value) do begin
          if value[i]='\' then begin
            delete (value,i,1);
            case value[i] of
              '0':value[i]:=#0;
              'n':value[i]:=#10;
              't':value[i]:=#9;
              'x':begin
                    value[i]:=char(StrToInt('$'+copy(value,i+1,2)));
                    delete (value,i+1,2);
                  end;
            else
              // Do nothing - the character was just escaped.
            end;
          end;
          inc (i);
        end;
        if IsMsgId then entry.MsgId:=entry.MsgId+utf8decode(value)
                   else entry.MsgStr:=entry.MsgStr+utf8decode(value);
      end;
    end;
    if (line='') and entryhasdata then begin
      Result:=entry;
    end else begin
      Result:=nil;
    end;
    entryhasdata:=line<>'';
  except
    on e:Exception do
      raise Exception.Create (format(_('Exception %s in line %d:'+SLineBreak+'%s'),[e.ClassName,LineNumber,e.Message]));
  end;
end;

constructor TPoParser.Create;
begin
  LineNumber:=0;
  entry:=TPoEntry.Create;
end;

destructor TPoParser.Destroy;
begin
  FreeAndNil (entry);
  inherited;
end;

function TPoParser.ReadNextEntry(var tf: TextFile): TPoEntry;
var
  line:string;
begin
  while not eof(tf) do begin
    Readln (tf, line);
    Result:=AddLine(line);
    if Result<>nil then
      exit;
  end;
  Result:=AddLine ('');
end;


{ TPoEntry }

procedure TPoEntry.Assign(po: TPoEntry);
begin
  UserCommentList.Assign(po.UserCommentList);
  AutoCommentList.Assign(po.AutoCommentList);
  MsgId:=po.MsgId;
  MsgStr:=po.MsgStr;
end;

procedure TPoEntry.Clear;
begin
  UserCommentList.Clear;
  AutoCommentList.Clear;
  MsgId:='';
  MsgStr:='';
end;

constructor TPoEntry.Create;
begin
  UserCommentList:=TStringList.Create;
  AutoCommentList:=TStringList.Create;
end;

destructor TPoEntry.Destroy;
begin
  FreeAndNil (UserCommentList);
  FreeAndNil (AutoCommentList);
  inherited;
end;

function FindBestBreak (s:widestring;LineWidth:integer):integer;
// Returns number of characters to include in the line
var
  spacepos:integer;
  i,p:integer;
  MaxLength:integer;
begin
  p:=pos(#10,s);
  spacepos:=0;
  MaxLength:=min(length(s),LineWidth);
  if (p>2) and (p<MaxLength) then begin
    Result:=p;
    exit;
  end;
  i:=MaxLength;
  while i>=1 do begin
    case s[i] of
      #10:begin
            Result:=i;
            exit;
          end;
      ' ':
        if spacepos=0 then
          spacepos:=i;
    end;
    dec (i);
  end;
  if spacepos>LineWidth div 2 then begin
    Result:=spacepos;
  end else
    Result:=MaxLength;
  if (Result>=2) and (ord(s[Result])<32) and (ord(s[Result])<>10) then begin
    for i:=Result-1 downto 1 do begin
      if (ord(s[i])>=32) or (ord(s[i])=10) then begin
        Result:=i;
        exit;
      end;
    end;
  end;
end;

procedure TPoEntry.WriteToStream(str: TStream);
  procedure WritePart (token:utf8string;msg:widestring);
  var
    p:integer;
    part:widestring;
  begin
    StreamWrite (str, token+' ');
    while true do begin
      p:=FindBestBreak (msg,70);
      part:=copy(msg,1,p);
      delete (msg,1,length(part));
      StreamWriteln (str, String2PO(utf8encode(part)));
      if msg='' then
        break;
    end;
  end;
var
  s:utf8string;
  p:integer;
  idx:integer;
  isplural:boolean;
begin
  // Write comments
  s:=trim(UserCommentList.Text);
  if s<>'' then s:=s+sLineBreak;
  StreamWrite (str, s);
  s:=trim(AutoCommentList.Text);
  if s<>'' then s:=s+sLineBreak;
  StreamWrite (str, s);

  // Write msgid and msgstr
  p:=pos(#0,MsgId);
  isplural:=p<>0;
  if not isplural then
    WritePart ('msgid',MsgId)
  else begin
    WritePart ('msgid',copy(MsgId,1,p-1));
    WritePart ('msgid_plural',copy(MsgId,p+1,maxint));
  end;
  p:=pos(#0,MsgStr);
  if (p=0) and (not isplural) then
    WritePart ('msgstr',MsgStr)
  else begin
    idx:=0;
    while true do begin
      if p<>0 then begin
        WritePart ('msgstr['+IntToStr(idx)+']',copy(MsgStr,1,p-1));
        delete (MsgStr,1,p);
      end else begin
        WritePart ('msgstr['+IntToStr(idx)+']',MsgStr);
        break;
      end;
      inc (idx);
      p:=pos(#0,MsgStr);
    end;
  end;

  // Write empty line
  StreamWrite (str, sLineBreak);
end;

{ TPoEntryList }

procedure TPoEntryList.Add(entry: TPoEntry);
var
  p:Integer;
  l:TList;
  idx:integer;
  po:TPoEntry;
  searchkey:utf8string;
begin
  searchkey:=GetSearchKey(entry.MsgId);
  if list.Find(searchkey,idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      po:=TObject(l.Items[p]) as TPoEntry;
      if po.MsgId=entry.MsgId then
        raise Exception.Create (Format(_('This list of translations cannot handle MsgId duplicates. Please remove the duplicate of "%s".'),[po.MsgId]));
    end;
  end else begin
    l:=TList.Create;
    list.AddObject(searchkey,l);
  end;
  po:=TPoEntry.Create;
  po.Assign (entry);
  l.Add(po);
end;

procedure TPoEntryList.Clear;
var
  i,j:integer;
  l:TList;
begin
  for i:=0 to list.count-1 do begin
    l:=list.Objects[i] as TList;
    for j:=0 to l.count-1 do 
      TObject(l.Items[j]).Free;
    l.Free;
  end;
  list.Clear;
end;

constructor TPoEntryList.Create;
begin
  list:=TStringList.Create;
  list.Duplicates:=dupError;
  list.CaseSensitive:=True;
  list.Sorted:=True;
end;

function TPoEntryList.Delete(MsgId: widestring): boolean;
var
  p:Integer;
  l:TList;
  idx:integer;
  po:TPoEntry;
begin
  Result:=False;
  if list.Find(GetSearchKey(MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      po:=TObject(l.Items[p]) as TPoEntry;
      if po.MsgId=MsgId then begin
        po.Free;
        l.Delete (p);
        Result:=True;
        if l.Count=0 then begin
          l.Free;
          list.Delete (idx);
        end;
        exit;
      end;
    end;
  end;
end;

destructor TPoEntryList.Destroy;
begin
  Clear;
  FreeAndNil (list);
  inherited;
end;

function TPoEntryList.Find(MsgId: widestring):TPoEntry;
var
  p:Integer;
  l:TList;
  idx:integer;
begin
  if list.Find(GetSearchKey(MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    for p:=0 to l.count-1 do begin
      Result:=TObject(l.Items[p]) as TPoEntry;
      if Result.MsgId=MsgId then
        exit;
    end;
  end;
  Result:=nil;
end;

function TPoEntryList.FindFirst: TPoEntry;
var
  l:TList;
begin
  if list.Count=0 then begin
    Result:=nil;
    exit;
  end;
  l:=list.Objects[0] as TList;
  if l.Count=0 then
    raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
  Result:=TObject(l.Items[0]) as TPoEntry;
end;

function TPoEntryList.FindNext(po: TPoEntry): TPoEntry;
var
  p:Integer;
  l:TList;
  idx:integer;
begin
  Result:=Nil;
  if list.Find(GetSearchKey(po.MsgId),idx) then begin
    l:=list.Objects[idx] as TList;
    p:=l.IndexOf(po);
    if p=-1 then
      raise Exception.Create (_('Error: Specified TPoEntry was not found in list.'));
    if p=l.Count-1 then begin
      if idx=list.Count-1 then begin
        Result:=nil;
      end else begin
        l:=list.Objects[idx+1] as TList;
        if l.Count=0 then
          raise Exception.Create (_('Internal error in TPoEntryList data structure. Sublist for searchkey was empty.'));
        Result:=TObject(l.Items[0]) as TPoEntry;
      end;
    end else
      Result:=TObject(l.Items[p+1]) as TPoEntry;
  end;
end;

function TPoEntryList.GetSearchKey(MsgId: widestring): utf8string;
var
  p:integer;
begin
  p:=pos(#10,MsgId);
  if p<>0 then begin
    Result:=utf8encode(copy(MsgId,1,p-1));
  end else begin
    Result:=utf8encode(MsgId);
  end;
end;

procedure TPoEntryList.LoadFromFile(filename: string);
var
  tf:TextFile;
  pop:TPoParser;
  pe:TPoEntry;
begin
  AssignFile (tf,filename);
  Reset (tf);
  try
    pop:=TPoParser.Create;
    try
      while true do begin
        pe:=pop.ReadNextEntry(tf);
        if pe=nil then
          break;
        Add (pe);
      end;
    finally
      FreeAndNil (pop);
    end;
  finally
    CloseFile (tf);
  end;
end;

procedure TPoEntryList.SaveToFile(filename: string);
var
  outfile:TFileStream;
  pe:TPoEntry;
begin
  outfile:=TFileStream.Create (filename, fmCreate);
  try
    // Write header
    pe:=Find('');
    if pe<>nil then
      pe.WriteToStream(outfile);

    // Write the rest
    pe:=FindFirst;
    while pe<>nil do begin
      if pe.MsgId<>'' then
        pe.WriteToStream(outfile);
      pe:=FindNext (pe);
    end;
  finally
    FreeAndNil (outfile);
  end;
end;

end.

