unit xgettext;
(****************************************************************)
(*                                                              *)
(*  (C) Copyright by Lars B. Dybdahl                            *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241                 *)
(*  You received this file under the Mozilla Public License 1.1 *)
(*                                                              *)
(*  See http://dybdahl.dk/dxgettext/ for more information       *)
(*                                                              *)
(* modifed jgv 2003.05.13                                     *)
(**************************************************************)

interface

uses
  Classes, poparser;

type
  TOnOverwrite = Procedure (sender: TObject; const aFileName: wideString; var Overwrite: boolean) of object;
  TWarningType=
    (wtGenericWarning, wtUnexpectedException, wtConstantReplaced,
     wtSyntaxError, wtParameterError, wtExtendedDirectiveError, wtNonAscii,
     wtNotImplemented,
     wtNGettext); // This means that ngettext() was found, which generates msgid_plural entries in default.po
  TXGTDomain=
    class
    public
      msgid:TStringList;  // Sorted for binary lookups, objects are TItem
      order:TStringList;  // same as msgid, but sorted after occurence. Points to the same objects as msgid, so don't free them!
      constructor Create;
      destructor Destroy; override;
    end;
  TOnProgress=
    procedure (CurrentTask:widestring;CurrentFileName:widestring;LineNumber:Integer) of object;
  TOnWarning=
    procedure (WarningType:TWarningType;Msg,Line:widestring;Filename:widestring;LineNumber:Integer) of object;
  TXGetText =
    class
    private
      ignorelist: TPoEntryList;
      domainlist: TStringList; // Strings are domain name, values are TXGTDomain
      constlist:TStringList;   // List of consts. Strings are names, Objects are TConst
      definedDomain: string;
      procedure doHandleExtendedDirective (line: wideString);
      procedure ClearConstList;
      function GetDomain(domain: widestring): TXGTDomain;
      procedure AddTranslation(domain:widestring; msgid: widestring; Comments, Location: widestring);
      procedure WriteAll(Destinationpath:widestring; domain: widestring);
      function MakePathLinuxRelative(path: widestring): widestring;
    private
      resourcestringmode: Integer;  // 0=None, 1=Const, 2=Resourcestring
      CurrentFilename:widestring;
      LastLineRead:widestring;
      linenr:Integer;
      commentmode:widestring; // Empty means that dxreadln is not inside a comment
      lastcomment:widestring;
      BaseDirectoryList:TStringList; // Always ends in a pathdelimiter
      BaseDirectory:string;
      procedure WritePoFiles (DestinationPath: widestring);
      procedure Warning (WarningType:TWarningType;msg:widestring);
      procedure dxreadln (var src:TextFile;var line:widestring); // same as system.readln, but takes care of comments
      procedure extractstring(var source:widestring;var res: widestring);
      function readstring(var line: widestring; var src: TextFile): widestring; // Reads a pascal ansistring constant
      procedure ExtractFromPascal(sourcefilename: widestring);
      procedure ExtractFromDFM(sourcefilename: widestring);
      procedure ExtractFromRC(sourcefilename: widestring);
      {$ifdef mswindows}
      procedure ExtractFromEXE(sourcefilename: widestring);
      {$endif}
      procedure ExtractFromFile(sourcefilename: widestring);
      procedure ExtractFromFileMasks(mask: widestring);
    public
      // When set, only default domain is written to a file, and this file has it's filename from this variable
      SingleOutputFilename:string;
      
      OnProgress:TOnProgress;
      OnWarning:TOnWarning;
      Recurse:boolean;
      UpdateIgnore:boolean;  // Will create ignore.po if not exists, and put obvious untranslatable items into it
      UseIgnoreFile:boolean; // Will make sure that no item from ignore.po is stored in other files
      AllowNonAscii:boolean;
      OrderbyMsgid:boolean;
      NoWildcards:boolean;
      filemasks:TStringList;
      DestinationPath:string;
      CFiles:TStringList;   // This will contain filenames of C/C++ source files to be scanned elsewhere
      OnOverwrite: TOnOverwrite;

      constructor Create;
      destructor Destroy; override;
      procedure AddBaseDirectory (path:string);
      procedure AddDelphiFilemasks;
      procedure AddKylixFilemasks;
      procedure HandleIgnores;
      procedure Execute;
    end;




implementation

uses
{$ifdef MSWINDOWS}
  Windows, ExeImage, rxtypes,
{$endif}
  SysUtils, Math, appconsts, gnugettext, xgettexttools, Masks,
  ignoredetector;

type
  TConst=
    class
      name:widestring;
      value:widestring;
    end;
  EGetText=
    class (Exception)
    end;

const
  cDefineDirective  = '{gnugettext:'; // Start of an option inside the source code
  cScanOption       = 'scan-all';     // Means that all strings in the source code should be extracted
  cDomainDefinition = 'text-domain';  // Changes default text domain for strings
  cScanResetOption  = 'reset';        // Changes back to default behaviour


procedure TXGetText.extractstring(var source: widestring; var res: widestring);
const whitespace=[#0..#32];
// Extracts the Pascal coded string at the beginning of source.
// Returns the result in res.
// Removes the extracted data from source.
var
  charset: set of char;
  s: widestring;
  constname,uconstname:widestring;
  idx:integer;
begin
  res := '';
  while source <> '' do begin
    case source[1] of
      '#':
        begin
          if copy(source, 2, 1) = '$' then begin
            s := '$';
            delete(source, 1, 2);
            charset := ['0'..'9', 'a'..'f', 'A'..'F'];
          end else begin
            delete(source, 1, 1);
            s := '';
            charset := ['0'..'9'];
          end;
          while (source <> '') and (ord(source[1])<=255) and (char(ord(source[1])) in charset) do begin
            s := s + source[1];
            delete(source, 1, 1);
          end;
          res := res + widechar(StrToInt(s));
          while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
          if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
        end;
      '''':
        begin
          delete(source, 1, 1);
          while true do begin
            if source = '' then begin
              Warning (wtSyntaxError,_('Single quote detected - string starts but does not end'));
              exit;
            end;
            if copy(source, 1, 1) = '''' then begin
              if copy(source, 2, 1) = '''' then begin
                // Double quote detected
                res := res + '''';
                delete(source, 1, 2);
              end else begin
                // End of text part detected
                delete(source, 1, 1);
                break;
              end
            end else begin
              res := res + copy(source, 1, 1);
              delete(source, 1, 1);
            end;
          end;
        end;
      'a'..'z','A'..'Z','_':
        begin
          constname:='';
          while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in ['a'..'z','A'..'Z','_','0'..'9']) do begin
            constname:=constname+source[1];
            delete (source,1,1);
          end;
          uconstname:=uppercase(constname);
          if constlist.Find(uconstname,idx) then begin
            res:=res+(constlist.Objects[idx] as TConst).value;
          end else
          if uconstname='CRLF' then begin
            res:=res+#10;
            if (resourcestringmode<>1) then
              Warning (wtConstantReplaced,Format(_('CRLF substituted with #10 for %s. Consider to use sLineBreak instead.'),[constname]));
          end else
          if uconstname='SLINEBREAK' then begin
            // Don't make a warning on this one because it is so common
            res:=res+#10;
          end else
          if (uconstname='DEPRECATED') or (uconstname='PLATFORM') or (uconstname='LIBRARY') then begin
            // The hinting directive was detected and ignored.
          end else
          begin
            if resourcestringmode=1 then // Don't handle consts that don't work
              break;
            Warning (wtGenericWarning,Format(_('Constant %s is not known.'),[constname]));
          end;
        end;
    else
      break;
    end;
    while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
    if (length(trim(source))>=2) and (copy(source,1,1)='+') then delete (source,1,1);
    while (source<>'') and (ord(source[1])<=255) and (char(ord(source[1])) in whitespace) do delete (source,1,1);
  end;
end;

function TXGetText.readstring(var line: widestring; var src: TextFile): widestring;
var
  s: widestring;
  pluscoming:boolean;
  i:integer;
  ansis:ansistring;
  found:boolean;
begin
  Result := '';
  while true do begin
    extractstring(line, s);
    Result := Result + s;
    line := trim(line);
    pluscoming:=(line='');
    if (line='+') or pluscoming then begin
      // This is a multi-line string
      dxreadln(src, line);
      line := trim(line);
      if pluscoming then begin
        if copy(line,1,1)='+' then begin
          delete (line,1,1);
          line:=trim(line);
        end else begin
          if resourcestringmode<>1 then
            Warning (wtSyntaxError,_('This line is not connected with the previous line using a plus (+).'));
          break;
        end;
      end;
    end else
      break;
  end;
  // Find out if there is just one character above 255
  found:=False;
  for i:=1 to length(Result) do begin
    if ord(Result[i])>=256 then begin
      found:=True;
      break;
    end;
  end;
  if not found then begin
    // Assume the string is not unicode, but the local character set.
    // Move all characters to an ansistring
    SetLength (ansis,length(Result));
    for i:=1 to length(Result) do
      ansis[i]:=char(ord(Result[i]));
    // Convert from local character set to widestring
    Result:=StringToWidestring(ansis);
  end;
end;

function TXGetText.MakePathLinuxRelative (path:widestring):widestring;
var
  baselen:integer;
begin
  baselen:=length(BaseDirectory);
  {$ifdef MSWINDOWS}
  if uppercase(copy(path,1,baselen))=uppercase(BaseDirectory) then begin
    Result:=copy(path,baselen+1,maxint);
  end else begin
    Result:=copy(path,3,maxint);
  end;
  Result:=WindowsPathDelim2LinuxPathDelim(Result);
  {$endif}
  {$ifdef LINUX}
  if copy(path,1,baselen)=BaseDirectory then begin
    Result:=copy(path,baselen+1,maxint);
  end else begin
    Result:=path;
  end;
  {$endif}
end;

procedure TXGetText.ExtractFromPascal(sourcefilename: widestring);
// I didn't have a Pascal parser available when this code was written.
var
  src: TextFile;
  line, uline:widestring;
  s:widestring;
  msgid: widestring;
  p, p2, idx:Integer;
  domain: widestring;
  co:TConst;
  constident:widestring;
  idlength,idoffset:integer;
  idplural:boolean;
begin
  if lowercase(extractfilename(sourcefilename)) = 'gnugettext.pas' then exit;
  if lowercase(extractfilename(sourcefilename)) = 'gnugettextd5.pas' then exit;
  ClearConstList;
  AssignFile(src, sourcefilename);
  Reset(src);
  try
    definedDomain := '';
    lastcomment := '';
    resourcestringmode := 0;
    linenr := 0;
    while not eof(src) do begin
      dxreadln(src, line);
      line := trim(line);

      s := ConvertWhitespaceToSpaces (uppercase(line)) + ' ';

      // This should catch resourcestring start
      if (copy(s, 1, 15) = 'RESOURCESTRING ') then begin
        resourcestringmode := 2;
        delete (line,1,15);
      end;
      if (copy(s, 1, 6) = 'CONST ') then begin
        resourcestringmode := 1;
        delete (line,1,6);
      end;
      // This should catch all ends of resourcestring areas
      if (copy(s, 1, 9) = 'FUNCTION ') or (copy(s, 1, 10) = 'PROCEDURE ') or
        (copy(s, 1, 6) = 'BEGIN ') or (copy(s, 1, 4) = 'VAR ') or
        (copy(s, 1, 5) = 'TYPE ') or
        (copy(s, 1, 12) = 'CONSTRUCTOR ') or (copy(s, 1, 11) = 'DESTRUCTOR ') then
        resourcestringmode := 0;

      if resourcestringmode<>0 then begin
        while true do begin
          line:=trim(line);
          p := pos('''', line);
          if p = 0 then
            break;

          s:=trim(copy(line,1,p-1));
          if copy(s,length(s),1)='=' then begin
            // Identifier probably precedes the string
            s:=trim(copy(s,1,length(s)-1));
            if is_identifier(s) then
              constident:=s
            else
              constident:='';
          end;

          delete(line, 1, p - 1);
          // Extract the string
          msgid := readstring(line, src);
          if resourcestringmode=2 then begin
            if constident<>'' then begin
              if lastcomment<>'' then
                lastcomment:=lastcomment+sLinebreak;
              lastcomment:=lastcomment+'Programmer''s name for it: '+constident;
            end;
            AddTranslation('default', msgid, lastcomment, MakePathLinuxRelative(sourcefilename)+':'+IntToStr(linenr));
            lastcomment := '';
          end;
          if constident<>'' then begin
            if constlist.Find(uppercase(constident),idx) then begin
              co:=constlist.Objects[idx] as TConst;
            end else begin
              co:=TConst.Create;
              co.Name:=constident;
              constlist.AddObject(uppercase(co.name),co);
            end;
            co.Value:=msgid;

            // If source-code comments for gnugettext enable it,
            // extract the constant even though it is not a resourcestring.
            if Length (definedDomain) > 0 then begin
              if lastcomment <> '' then
                lastcomment := lastcomment + sLinebreak;
              lastcomment := lastcomment + 'Programmer''s name for it: ' + constident;
              AddTranslation (definedDomain, msgid, lastcomment, MakePathLinuxRelative(sourcefilename)+':'+IntToStr(linenr));
              lastcomment := '';
            end;
          end;

          // Check what comes next in the line
          if copy(line, 1, 1) <> ';' then begin
            // First parameter is line number, second is the contents of the line
            if resourcestringmode=2 then
              Warning (wtSyntaxError,_('resourcestring does not end in semicolon.'));
            line:='';
            break;
          end else begin
            // If it ended with a semicolon, analyze the rest of the line as if it was a new line
            delete(line, 1, 1);
          end;
        end;
      end else begin
        // Check for occurence of gettext()
        while true do begin
          uline:=uppercase(line);
          p:=poscode('_',uline);
          p2:=poscode('GETTEXT', uline);
          if p=0 then begin
            p:=p2;
          end else
            if p2<>0 then
              p:=min(p,p2);
          if p=0 then
            break;
          if (poscode('FUNCTION',uline)<>0) or
             (poscode('PROCEDURE',uline)<>0) then
            break;

          domain := 'default';
          idoffset:=0;
          if copy(uline,p,1)='_' then idlength:=1
                                 else idlength:=7;
          if uppercase(copy(line, p - 1, 1)) = 'D' then begin
            domain := '';
            idlength:=8;
            idoffset:=-1;
          end;
          if uppercase(copy(line, p - 2, 2)) = 'DC' then begin
            domain := '';
            idlength:=9;
            idoffset:=-2;
          end;
          idplural:=False;
          if uppercase(copy(line, p - 2, 2)) = 'DN' then begin
            domain := '';
            idlength:=9;
            idoffset:=-2;
            idplural:=True;
          end else
          if uppercase(copy(line, p - 1, 1)) = 'N' then begin
            idlength:=8;
            idoffset:=-1;
            idplural:=True;
          end;
          if ((p+idoffset=1) or (not ((ord(uline[p+idoffset-1])<=255) and (char(ord(uline[p+idoffset-1])) in ['a'..'z','A'..'Z','_','0'..'9'])))) and
              (length(line)>=p+idlength+idoffset) and (not ((ord(uline[p+idoffset+idlength])<=255) and (char(ord(uline[p+idoffset+idlength])) in ['a'..'z','A'..'Z','_','0'..'9']))) then begin
            if (idoffset=-1) and (idlength=8) and (uppercase(copy(line,p-1,8))='NGETTEXT') then begin
              // This string may not be wider than 80 characters.
              Warning (wtNGettext,_('ngettext() encountered. This will create msgid_plural entries in the'+sLineBreak+
                                    'default.po file, which may render it uneditable by some GUI po file editors.'+sLineBreak+
                                    'Consider to use dngettext() instead, putting the msgid_plural entries into'+sLineBreak+
                                    'a separate .po file.'));
            end;
            line := trim(copy(line, p + idlength+idoffset, maxint));
            if copy(line, 1, 1) = '(' then begin
              line := trim(copy(line, 2, maxint));
              if domain = '' then begin
                // get first parameter
                extractstring(line, domain);
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end else begin
                  // First parameter is line number, second is line contents
                  Warning (wtSyntaxError,_('Missing comma after first parameter'));
                end;
              end;

              // Get parameter that contains the msgid
              msgid := readstring(line, src);
              if idplural then begin
                line := trim(line);
                if copy(line, 1, 1) = ',' then begin
                  delete(line, 1, 1);
                  line:=trim(line);
                end else begin
                  Warning (wtSyntaxError,_('Missing comma after first parameter'));
                end;
                msgid := msgid+#0+readstring(line, src);
              end;
              AddTranslation(domain, msgid, lastcomment, MakePathLinuxRelative(sourcefilename) + ':' + IntToStr(linenr));
              lastcomment := '';
            end { if a parenthesis is found };
          end else begin
            line := trim(copy(line, p + idlength+idoffset, maxint));
          end { if it looks like a function call identifier };
        end { loop that finds several occurences in the same line };
      end { if resourcestringmode };
    end;
  finally
    CloseFile(src);
  end;

  If length (definedDomain) > 0 then begin
    Warning (wtExtendedDirectiveError, _('$gnugettext: end directive is missing !'));
  end;
end;

constructor TXGetText.Create;
begin
  inherited Create;
  ignorelist:=TPoEntryList.Create;
  CFiles:=TStringList.Create;
  CFiles.Duplicates:=dupError;
  CFiles.CaseSensitive:=True;
  CFiles.Sorted:=True;
  BaseDirectoryList:=TStringList.create;
  filemasks:=TStringList.Create;
  filemasks.Sorted:=True;
  filemasks.Duplicates:=dupIgnore;
  filemasks.CaseSensitive:=True;
  domainlist := TStringList.Create;
  domainlist.Sorted := True;
  domainlist.Duplicates:=dupError;
  domainlist.CaseSensitive:=True;
  constlist:=TStringList.Create;
  constlist.Sorted:=True;
  constlist.Duplicates:=dupError;
  constlist.CaseSensitive:=True;
end;

destructor TXGetText.Destroy;
begin
  ClearConstList;
  FreeAndNil (constlist);
  while domainlist.Count <> 0 do begin
    domainlist.Objects[0].Free;
    domainlist.Delete(0);
  end;
  FreeAndNil(domainlist);
  FreeAndNil (BaseDirectoryList);
  FreeAndNil (filemasks);
  FreeAndNil (CFiles);
  FreeAndNil (ignorelist);
  inherited;
end;

procedure TXGetText.ExtractFromDFM(sourcefilename: widestring);
var
  src: TStream;
  mem: TMemoryStream;
  line, lastline:widestring;
  s: widestring;
  indent: integer;
  comment: widestring;
  p, linenr: integer;
  scope: TStringList;
  propertyname: widestring;
  multilinevalue: boolean;
  mvalue: widestring;
  p1, p2, p3: integer;
  c:char;
begin
  src:=TFileStream.Create(sourcefilename,fmOpenRead);
  try
    // Check for empty file
    if src.Read(c,1)=0 then
      exit;
    // Check for binary dfm file
    src.Seek(0, soFromBeginning);
    if c=#$FF then begin
      // Convert the file into text form in a memory stream
      mem:=TMemoryStream.Create;
      ObjectResourceToText(src,mem);
      FreeAndNil (src);
      src:=mem;
    end;
    src.Seek(0,soFrombeginning);

    scope := TStringList.Create;
    try
      linenr := 0;
      line := '';
      propertyname := '';
      multilinevalue := false;
      while true do begin
        // Get next line and check it out
        lastline := line;
        if not StreamReadln (src, line) then break;
        inc(linenr);
        indent := measureindent(line);
        line := trim(line);

        // Check for changes in scope
        if (indent < scope.Count) and multilinevalue then begin
          multilinevalue := false;
          comment := scope2comment(scope, propertyname);
          AddTranslation('default', mvalue, comment, MakePathLinuxRelative(sourcefilename) + ':' + IntToStr(linenr));
          scope.Delete(scope.count - 1);
        end;
        while indent < scope.Count do begin
          scope.Delete(scope.count - 1);
        end;
        if indent > scope.Count then begin
          p := pos(' ', lastline);
          if p = 0 then s := lastline else s := copy(lastline, p + 1, maxint);
          p := pos(':', s);
          multilinevalue := true;
          mvalue := '';
          if p = 0 then s := '' else s := copy(s, 1, p - 1);
        end;
        while indent > scope.Count do begin
          scope.Add(s);
          s := '';
        end;

        // Analyze the line
        p := pos(' = ', line);
        p1 := pos('''', line);
        p2 := pos('#', line);
        if p1 = 0 then p1 := maxint;
        if p2 = 0 then p2 := maxint;
        p3 := min(p1, p2);

        // Extract property name if the line contains such one
        if (p <> 0) and (p < p3) then begin
          propertyname := trim(copy(line, 1, p - 1));
          multilinevalue := false;
        end;

        // Extract string, if the line contains such one
        if p3 <> maxint then begin
          delete(line, 1, p3 - 1);
          extractstring(line, s);
          if multilinevalue then begin
            mvalue := mvalue + s;
            if trim(line) <> '+' then begin
              comment := scope2comment(scope, propertyname);
              AddTranslation('default', mvalue, comment, MakePathLinuxRelative(sourcefilename) + ':' + IntToStr(linenr));
              mvalue:='';
            end;
          end else begin
            comment := scope2comment(scope, propertyname);
            AddTranslation('default', s, comment, MakePathLinuxRelative(sourcefilename) + ':' + IntToStr(linenr));
          end;
        end;
      end;
    finally
      FreeAndNil(scope);
    end;
  finally
    FreeAndNil (src);
  end;
end;

procedure TXGetText.AddTranslation(domain, msgid: widestring; Comments,
  Location: widestring);
// Adds something to translate to the list
var
  it: TPoEntry;
  i, e: integer;
  sl: TStringList;
  dom:TXGTDomain;
  lookupvalue:ansistring;
begin
  // Check for non-ascii characters
  if not AllowNonAscii then begin
    for i:=1 to length(msgid) do begin
      if ord(msgid[i])>=128 then begin
        Warning (wtNonAscii,format(_('msgid contains non-ascii characters: "%s"'),[msgid]));
        // Don't add an invalid msgid
        exit;
      end;
    end;
  end;

  // Remove any Carriage Returns
  while true do begin
    i:=pos(#13,msgid);
    if i=0 then break;
    delete (msgid,i,1);
  end;

  // Don't add empty strings
  if msgid = '' then exit;

  // Don't add numbers
  val(msgid, i, e);
  if (e = 0) and (msgid = IntToStr(i)) then exit;

  dom:=GetDomain(domain);
  sl:=TStringList.Create;
  try
    sl.Text := utf8encode(msgid);
    if sl.Count=0 then
      lookupvalue:='Weird, but happens if the string contains weird ascii chars'
    else
      lookupvalue:=sl.Strings[0];
  finally
    FreeAndNil(sl);
  end;
  it:=nil;
  if dom.msgid.Find(lookupvalue,i) then begin
    // Scroll back to the first in the list that has the same
    // first line in msgid
    while (i > 0) and (dom.msgid.Strings[i - 1] = lookupvalue) do
      dec(i);
    // Now loop through all those in the list it may be
    while true do begin
      it := dom.msgid.Objects[i] as TPoEntry;
      // Check if we found the correct one
      if it.msgid = msgid then break;
      // Check if we have scrolled past the last one
      if (i = dom.msgid.Count - 1) or (dom.msgid.Strings[i+1] <> lookupvalue) then begin
        it := nil;
        break;
      end;
      inc(i);
    end;
  end;
  if it = nil then begin
    it := TPoEntry.Create;
    dom.msgid.AddObject(lookupvalue, it);
    it.msgid := msgid;
    dom.order.AddObject(lookupvalue, it);
  end;
  if comments<>'' then begin
    sl:=TStringList.Create;
    try
      sl.Text:=utf8encode(comments);
      for i:=0 to sl.Count-1 do begin
        it.AutoCommentList.Add('#. '+sl.Strings[i]);
      end;
    finally
      FreeAndNil (sl);
    end;
  end;

  it.AutoCommentList.Add('#: '+RemoveFilenameSpaces(utf8encode(Location)));
end;

procedure TXGetText.WriteAll(Destinationpath, domain: widestring);
// Outputs a .po file
var
  destination: TFileStream;
  i: integer;
  item: TPoEntry;
  dom:TXGTDomain;
  filename: widestring;
  orderlist:TStrings;
  overwrite: boolean;
begin
  dom:=GetDomain(domain);
  if SingleOutputFilename<>'' then begin
    if domain='default' then begin
      filename:=SingleOutputFilename;
    end else begin
      exit;
    end;
  end else begin
    filename := destinationpath + domain + '.po';
  end;

  // Check for overwriting. Call custom handler if present, and abort if overwriting is not permitted.
  if FileExists (fileName) then begin
    overwrite := True;
    if assigned (OnOverwrite) then OnOverwrite (self, fileName, overwrite);
    if not overwrite then begin
      OnProgress (format (_('Overwrite %s aborted.'), [fileName]), filename, 0);
      Exit;
    end;
  end;

  // %s will be replaced by the filename
  if Assigned(OnProgress) then
    OnProgress (Format(_('Writing %s'),[filename]),filename,0);
  destination:=TFileSTream.Create (filename, fmCreate);
  try
    // Write a dummy header that the user can modify
    StreamWriteDefaultPoTemplateHeader(destination,Format(_('dxgettext %s'),(.version.)));

    // Write out all msgids
    if OrderbyMsgid then orderlist:=dom.msgid
                    else orderlist:=dom.order;
    for i := 0 to orderlist.Count - 1 do begin
      item := orderlist.Objects[i] as TPoEntry;
      item.WriteToStream(destination);
    end;
  finally
    FreeAndNil (destination);
  end;
end;

procedure TXGetText.ExtractFromFile(sourcefilename: widestring);
var
  ext:widestring;
begin
  CurrentFilename:=sourcefilename;
  linenr:=0;
  if ExpandFileName(sourcefilename)<>sourcefilename then
    sourcefilename:=BaseDirectory+SourceFilename;
  try
    ext:=uppercase(ExtractFileExt(sourcefilename));
    if (ext='.C') or (ext='.CPP') then
      CFiles.Add(sourcefilename)
    else begin
      if Assigned(OnProgress) then
        OnProgress (Format(_('Reading %s'),[sourcefilename]),sourcefilename,0);
      if (ext='.DFM') or (ext='.XFM') then
        ExtractFromDFM(sourcefilename)
      else
      if ext='.RC' then
        ExtractFromRC(sourcefilename)
      else
{$ifdef mswindows}
      if (ext='.DLL') or (ext='.EXE') or (ext='.BPL') then
        ExtractFromEXE(sourcefilename)
      else
{$endif}
      if (ext='.PAS') or (ext='.DPR') or (ext='.INC') then
        ExtractFromPascal(sourcefilename)
      else begin
        Warning (wtParameterError,Format(_('WARNING: Unknown file extension %s. Reading file as being pascal source.'),[ext]));
        ExtractFromPascal(sourcefilename)
      end;
    end;
  except
    on e:EControlC do begin
      raise;
    end;
    on e:Exception do begin
      Warning (wtUnexpectedException,'Exception '+e.ClassName+sLineBreak+e.Message);
    end;
  end;
  CurrentFilename:='';
end;

procedure TXGetText.ExtractFromFileMasks(mask:widestring);
var
  sr: TSearchRec;
  more: boolean;
  curdir:widestring;
  dirlist:TStringList;
  sl:TStringList;
  i, idx:integer;
  maskcheck:TMask; // This is only necessary because of a bug in the Windows API FindFirst()
begin
  mask:=ExpandFileName(BaseDirectory+mask);
  dirlist:=TStringList.Create;
  try
    dirlist.Add(ExtractFilePath(mask));
    mask:=ExtractFileName(mask);

    if recurse then begin
      idx:=0;
      while idx<dirlist.count do begin
        curdir:=dirlist.Strings[idx];

        // Find all subdirectories
        more := FindFirst(curdir+'*', faAnyFile, sr) = 0;
        while more do begin
          if (sr.Attr and faDirectory<>0) and (sr.Name<>'.') and (sr.Name<>'..') then
            dirlist.Add(curdir+sr.Name+PathDelim);
          more := FindNext(sr) = 0;
        end;
        SysUtils.FindClose (sr);
        inc (idx);
      end;
    end;

    dirlist.Sort;

    for idx:=0 to dirlist.Count-1 do begin
      curdir:=dirlist.Strings[idx];

      maskcheck:=TMask.Create (mask);
      sl:=TStringList.Create;
      try
        // Extract from all files in current directory
        more := FindFirst(curdir+mask, faAnyFile-faDirectory, sr) = 0;
        while more do begin
          // The following if is only necessary, because several Windows versions
          // have a bug in FindFirst, that makes "test.cpp,v" match on the
          // file mask "*.cpp"
          if maskcheck.Matches(sr.Name) then
            sl.Add (curdir + sr.Name);
          more := FindNext(sr) = 0;
        end;
        SysUtils.FindClose(sr);
        sl.Sort;
        for i:=0 to sl.count-1 do
          ExtractFromFile(sl.Strings[i]);
      finally
        FreeAndNil (sl);
        FreeAndNil (maskcheck);
      end;
    end;
  finally
    FreeAndNil (dirlist);
  end;
end;

function TXGetText.GetDomain(domain: widestring): TXGTDomain;
var
  i: integer;
begin
  if domainlist.Find(domain, i) then begin
    Result := domainlist.Objects[i] as TXGTDomain;
  end else begin
    Result := TXGTDomain.Create;
    domainlist.AddObject(domain, Result);
  end;
end;

procedure TXGetText.dxreadln(var src: TextFile; var line: widestring);
var
  i:integer;
  procedure cutuntil (endtag:widestring);
  var p:integer;
  begin
    p:=i+length(endtag)-1;
    while p<=length(line) do begin
      if copy(line,p,length(endtag))=endtag then begin
        delete (line,i,p+length(endtag)-i);
        exit;
      end;
      inc (p);
    end;
    // At this place, the end tag was not found in the line
    line:=copy(line,1,i-1);
    commentmode:=endtag;
  end;
begin
  line:='';
  while (not eof(src)) and (line='') do begin
    if commentmode<>'' then begin
      while true do begin
        if eof(src) then begin
          line:='';
          exit;
        end;
        readln (src, line);
        line:=trim(line);
        LastLineRead:=line;
        inc (linenr);
        i:=pos(commentmode,line);
        if i<>0 then begin
          delete (line,1,i+length(commentmode)-1);
          commentmode:='';
          break;
        end;
      end;
    end else begin
      readln (src, line);
      line:=trim(line);
      LastLineRead:=line;
      inc (linenr);
      if trim(line)='' then
        lastcomment:='';
    end;
    i:=1;
    while i<=length(line) do begin
      if copy(line,i,1)='''' then begin
        // A string was detected - find the end of it.
        inc (i);
        while true do begin
          if copy(line,i,1)='''' then begin
            inc (i);
            break;
          end;
          // If the string doesn't end until the line is over, finish the procedure
          if i>=length(line) then
            exit;
          inc (i);
        end;
      end else
      if copy(line,i,2)='//' then begin
        // The rest of the line is a comment
        if lastcomment<>'' then
          lastcomment:=lastcomment+sLineBreak;
        lastcomment:=trim(copy(line,i+2,maxint));
        line:=copy(line,1,i-1);
        exit;
      end else
      if copy(line,i,1)='{' then begin
        if pos (cDefineDirective, lowercase(copy(line,1,length(cDefineDirective)))) = 1 then
          doHandleExtendedDirective (line);

        // Bracket comment
        cutuntil ('}');
      end else
      if copy(line,i,2)='(*' then begin
        // Bracket comment, Danish style
        cutuntil ('*)');
      end else
        inc (i);
    end;
    line:=trim(line);
  end;
end;

{ TXGTDomain }

constructor TXGTDomain.Create;
begin
  msgid:=TStringList.Create;
  order:=TStringList.Create;
  msgid.Sorted:=True;
  msgid.Duplicates:=dupAccept;
  msgid.CaseSensitive:=True;
end;

destructor TXGTDomain.Destroy;
begin
  while msgid.count<>0 do begin
    msgid.Objects[0].Free;
    msgid.Delete (0);
  end;
  FreeAndNil (msgid);
  FreeAndNil (order);
  inherited;
end;

procedure TXGetText.WritePoFiles (DestinationPath:widestring);
var
  i:integer;
begin
  for i:=0 to domainlist.Count-1 do begin
    // Write all domain.po files
    WriteAll(DestinationPath,domainlist.Strings[i]);
  end;
end;

procedure TXGetText.ClearConstList;
begin
  while constlist.Count<>0 do begin
    constlist.Objects[0].Free;
    constlist.Delete (0);
  end;
end;

procedure TXGetText.Warning(WarningType:TWarningType;msg: widestring);
begin
  if Assigned(OnWarning) then
    OnWarning (WarningType,msg,LastLineRead,CurrentFilename,linenr);
end;

procedure TXGetText.ExtractFromRC(sourcefilename: widestring);
var
  tf:TextFile;
  line:widestring;
  p, i:integer;
  ident:widestring;
begin
  // Currently, this scanner is based on the RC file that was included
  // with DBISAM version 3. It may not work with other RC files, but
  // if you find an RC file that it does not work with, please send that
  // RC file to Lars@dybdahl.dk
  AssignFile (tf,sourcefilename);
  Reset (tf);
  try
    linenr:=0;
    while not eof(tf) do begin
      // Get next line
      readln (tf,line);
      inc (linenr);
      line:=trim(line);
      LastLineRead:=line;

      if copy(line,1,1)<>'#' then begin
        p:=pos('"',line);
        if p<>0 then begin
          // Find identifier in the beginning of the line
          ident:=trim(copy(line,1,p-1));
          if copy(ident,length(ident),1)=',' then
            delete (ident,length(ident),1);
          if ident<>'' then
            ident:='Programmer''s name: '+ident;

          // Find the msgid
          delete (line,1,p);
          i:=1;
          while i<=length(line) do begin
            if line[i]='\' then begin
              delete (line,i,1);
              inc (i);
            end else
            if line[i]='"' then begin
              delete (line,i,maxint);
            end;
              inc (i);
          end;
          AddTranslation('default',line,ident,MakePathLinuxRelative(sourcefilename) + ':' + IntToStr(linenr));
        end;
      end;
    end;
  finally
    CloseFile (tf);
  end;
end;

procedure TXGetText.AddBaseDirectory(path: string);
begin
  if path<>'' then
    BaseDirectoryList.Add(IncludeTrailingPathDelimiter(path))
  else
    BaseDirectoryList.Add('');
end;

procedure TXGetText.Execute;
var
  i,j:integer;
begin

  // If no base directories, make one
  if BaseDirectoryList.Count=0 then
    AddBaseDirectory(IncludeTrailingPathDelimiter(ExpandFileName('.')));

  // Find destination path
  if DestinationPath='' then
    DestinationPath:=IncludeTrailingPathDelimiter(ExpandFileName('.'));

  // Read current ignore.po file
  if FileExists(DestinationPath+'ignore.po') then
    ignorelist.LoadFromFile(DestinationPath+'ignore.po');

  // Iterate base directories
  for j:=0 to BaseDirectoryList.Count-1 do begin
    BaseDirectory:=BaseDirectoryList.Strings[j];
    for i:=0 to filemasks.count-1 do begin
      if NoWildcards then begin
        ExtractFromFile(filemasks.Strings[i]);
      end else begin
        ExtractFromFileMasks(filemasks.Strings[i]);
      end;
    end;
  end;

  // Handle ignores
  HandleIgnores;

  // Write files
  if UpdateIgnore then
    ignorelist.SaveToFile(DestinationPath+'ignore.po');
  WritePoFiles (DestinationPath);
end;

procedure TXGetText.AddDelphiFilemasks;
begin
  filemasks.add ('*.pas');
  filemasks.add ('*.inc');
  filemasks.Add ('*.rc');
  filemasks.add ('*.dpr');
  filemasks.add ('*.xfm');
  filemasks.add ('*.dfm');
end;

procedure TXGetText.AddKylixFilemasks;
begin
  filemasks.add ('*.pas');
  filemasks.add ('*.inc');
  filemasks.Add ('*.rc');
  filemasks.add ('*.dpr');
  filemasks.add ('*.xfm');
end;


procedure TXGetText.doHandleExtendedDirective(line: wideString);
Const
  cErrOptionUnknown = '{gnugettext: Unknonw option.';
  cErrMissingStart = '{gnugettext: reset found without scan-all.';
  cErrDomainSyntax = '{gnugettext: error in the domain name definition.';
Var
  i : integer;
  tmp : string;
begin
  delete (line, 1, length(cDefineDirective));
  line := trim (line);
  if pos (cScanOption, lowerCase (copy (line, 1, length(cScanOption)))) = 1 then begin
    delete (line, 1, length (cScanOption));
    line := trim (line);
    if pos (cDomainDefinition, lowerCase (copy (line, 1, length(cDomainDefinition)))) = 1 then begin
      delete (line, 1, Length (cDomainDefinition));
      line := trim (line);
      if (length (line) > 0) and (line[1] = '=') then begin
        delete (line, 1, 1);
        line := trim (line);
        if (length (line) > 0) and (line[1] = '''') then begin
          delete (line, 1, 1);
          i := 1;
          tmp := '';
          while (i <= length (line)) and (line[i] <> '}') do begin
            if (line[i] = '''') then begin
              if (i = length (line)) or (line[i+1] <> '''') then begin
                definedDomain := tmp;
                break;
              end
              else inc (i);
            end;
            tmp := tmp + line[i];
            inc (i);
          end;
        end;
      end;

      if length (definedDomain) = 0 then begin
        Warning (wtExtendedDirectiveError, _(cErrDomainSyntax));
      end;
    end
    else definedDomain := 'default';
  end
  else if pos (cScanResetOption, lowerCase (copy (line, 1, length(cScanResetOption)))) = 1 then begin
    if length (definedDomain) = 0 then Warning(wtExtendedDirectiveError, _(cErrMissingStart))
    else definedDomain := ''
  end
  else begin
    Warning (wtExtendedDirectiveError, _(cErrOptionUnknown))
  end;
end;

{$ifdef mswindows}
procedure TXGetText.ExtractFromEXE(sourcefilename: widestring);
  procedure recurse (rl:TResourceList);
  var
    r:TResourceItem;
    i,j:integer;
    ws:widestring;
    itemno:integer;
  begin
    for i:=0 to rl.Count-1 do begin
      r:=rl.Items[i];
      if r.IsList then begin
        recurse (r.List)
      end else begin
        case r.ResType of
          rtString:
            begin
              itemno:=0;
              ws:=PWideChar(r.RawData);
              while ws<>'' do begin
                inc (itemno);
                j:=ord(ws[1]);
                AddTranslation('default',copy(ws,2,j),'Resource '+r.Name+', item no. '+IntToStr(itemno),MakePathLinuxRelative(sourcefilename)+':'+IntToStr(r.Offset));
                delete (ws,1,j+1);
              end;
            end;
        end;
      end;
    end;
  end;
var
  exe:TExeImage;
begin
  exe := TExeImage.CreateImage(nil, sourceFileName);
  try
    recurse (exe.Resources);
  finally
    FreeAndNil (exe);
  end;
end;
{$endif}

procedure TXGetText.HandleIgnores;
var
  j:integer;
  dom:TXGTDomain;
  item:TPoEntry;
  newitem:TPoEntry;
  ignoreitem:TPoEntry;
begin
  // Only default domain is affected by ignore.po
  dom:=GetDomain('default');

  // Add new ignores to new ignore list and update autocomments
  if UpdateIgnore then begin
    for j := 0 to dom.order.Count-1 do begin
      item := dom.order.Objects[j] as TPoEntry;
      ignoreitem:=ignorelist.Find(item.MsgId);
      if ignoreitem=nil then begin
        newitem:=TPoEntry.Create;
        newitem.Assign(item);
        if not IsProbablyTranslatable(newitem) then 
          ignorelist.Add(newitem)
        else
          FreeAndNil (newitem);
      end else begin
        ignoreitem.AutoCommentList.Text:=item.AutoCommentList.Text;
      end;
    end;
  end;

  // Remove ignores from default list
  if UseIgnoreFile then begin
    for j:=dom.order.Count-1 downto 0 do begin
      item:=dom.order.Objects[j] as TPoEntry;
      if ignorelist.Find(item.MsgId)<>nil then
        // Only delete from order list
        dom.order.Delete (j);
    end;
  end;
end;

end.
