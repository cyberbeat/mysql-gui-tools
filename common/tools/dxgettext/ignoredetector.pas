unit ignoredetector;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(****************************************************************)

interface

uses
  PoParser;

// Determines if entry is translatable. If not, it will add some comments to entry.
function IsProbablyTranslatable (entry: TPoEntry):boolean;


implementation

uses
  SysUtils;
  
function IsProbablyTranslatable (entry: TPoEntry):boolean;
var
  i:integer;
  hasletters,hasdigits,haswhitespace:boolean;
  c:char;
  utf8:utf8string;
  found:boolean;
  s:string;
begin
  // Check for msgid ""
  if entry.MsgId='' then begin
    Result:=True;
    exit;
  end;

  utf8:=utf8encode(entry.msgid);
  
  // Check for text types
  hasletters:=False;
  hasdigits:=False;
  haswhitespace:=False;
  for i:=1 to length(utf8) do begin
    c:=utf8[i];
    if (c in ['a'..'z','A'..'Z']) or (c>=#128) then
      hasletters:=True;
    if c in ['0'..'9'] then
      hasdigits:=True;
    if c in [#0..#32] then
      haswhitespace:=True;
  end;
  if (not haswhitespace) and hasletters and hasdigits then begin
    entry.UserCommentList.Add('#  Doesn''t look like text');
    Result:=False;
    exit;
  end;
  if not hasletters then begin
    entry.UserCommentList.Add('#  Doesn''t have any letters');
    Result:=False;
    exit;
  end;

  // Check for font names, component names etc.
  found:=False;
  for i:=0 to entry.AutoCommentList.Count-1 do begin
    s:=trim(entry.AutoCommentList.Strings[i]);
    if copy(s,1,2)='#.' then
    if uppercase(copy(s,length(s)-8,9))='FONT.NAME' then begin
      Found:=True;
    end else begin
      Found:=False;
      break;
    end;
  end;
  if Found then begin
    entry.UserCommentList.Add('#  Seems like a Font.Name extract');
    Result:=False;
    exit;
  end;

  // Check for fieldnames
  found:=False;
  for i:=0 to entry.AutoCommentList.Count-1 do begin
    s:=uppercase(trim(entry.AutoCommentList.Strings[i]));
    if copy(s,1,2)='#.' then
    if (pos('DATAFIELD',s)<>0) or (pos('FIELDNAME',s)<>0) or (pos('LOOKUPFIELD',s)<>0)
    or (pos('INDEXNAME',s)<>0) or (pos('KEYFIELDS',s)<>0) or (pos('LOOKUPKEYFIELDS',s)<>0)
    or (pos('DATABASENAME',s)<>0) or (pos('SESSIONNAME',s)<>0) then begin
      Found:=True;
    end else begin
      Found:=False;
      break;
    end;
  end;
  if Found then begin
    entry.UserCommentList.Add('#  Seems like a Database field name or index field name');
    Result:=False;
    exit;
  end;

  // Check for font names, component names etc.
  found:=False;
  for i:=0 to entry.AutoCommentList.Count-1 do begin
    s:=uppercase(trim(entry.AutoCommentList.Strings[i]));
    if copy(s,1,2)='#.' then
    if copy(s,length(s)-7,8)='....NAME' then begin
      Found:=True;
    end else begin
      Found:=False;
      break;
    end;
  end;
  if Found then begin
    entry.UserCommentList.Add('#  Seems like a component name');
    Result:=False;
    exit;
  end;

  // Check for file masks and file extensions
  s:=utf8;
  if (copy(s,1,1)='*') and (length(s)=5) or (length(s)=4) then begin
    if length(s)=5 then
      delete (s,1,1);
    if copy(s,1,1)='.' then begin
      delete (s,1,1);
      for i:=1 to 3 do begin
        found:=(s[i] in ['a'..'z','A'..'Z']);
        if not found then break;
      end;
      if found then begin
        entry.UserCommentList.Add('#  Looks like a file extension or file mask');
        Result:=False;
        exit;
      end;
    end;
  end;

  Result:=True;
end;

end.

