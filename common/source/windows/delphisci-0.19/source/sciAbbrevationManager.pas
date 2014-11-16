//CE_Desc_Include(helpdescriptions.txt)
{
	Delphi Scintilla Interface Components
	Copyright (C) 2004,2005 Jan Martin Pettersen (hdalis)

	This library is free software; you can redistribute it and/or
	modify it under the terms of the GNU Lesser General Public
	License as published by the Free Software Foundation; either
	version 2.1 of the License, or (at your option) any later
	version.

	This library is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
	Lesser General Public License for more details.

	You should have received a copy of the GNU Lesser General Public
	License along with this library; if not, write to the Free
	Software Foundatifon, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
  Author : hdalis
	Created: 24/11/2004, 18:21:20
  History: 26/11/2004 Initial Release
           09/01/2005 Added expansion of a template inside another (if AllowTemplateMacros is True)
                      Use <^nameoftemplate> to include one template inside another.. It keeps
                      expanding until no more expansion are possible.. If a templatename isn't found
                      it just removes the templateinclusion i.e <^whatever>, and continues expanding the rest..
           10/01/2005 Added the event OnTemplateMacro to match a templatemacro with something builtin.. All of these
                      starts with <^- instead.. Two is implemented here.. PROMPT prompt|caption  and ROW (Returns linenumber).
			$Id:  $

}
{$Include SciCommonDef.Inc}
unit sciAbbrevationManager;
interface
uses Classes,SciLexerMemo;
type
TSCEvent_templatemacro = procedure(Sender : TObject; const MacroName : String;Params : String;var RetVal : String) of object;
{$Ifdef NEWABBREVS}
  TTemplateItem=class(TCollectionItem)
  private
    FTemplateName : String;
    FTemplate : String;
    procedure SetTemplateName(const Value : String);
    procedure SetTemplate(const Value : String);
  published
    property TemplateName : String read FTemplateName write SetTemplateName;
    property Template : String read FTemplate write SetTemplate;
  end;
  TTemplateList=class(TOwnedCollection)
  protected
    procedure SetItem(Index:Integer; Value: TTemplateItem);
    function GetItem(Index:Integer): TTemplateItem;
    procedure SetString(Index : Integer;Value : String);
    function GetString(Index : Integer) : String;
  public
    constructor Create(AOwner : TComponent;ItemClass : TCollectionItemClass);
    procedure LoadFromFile(const FileName : String);
    procedure SaveToFile(const FileName : String);

    function IndexOf(const TemplateName : String) : Integer;
    property Items[Index : Integer] : TTemplateItem read GetItem write SetItem;
    //property Strings[Index : Integer] : String read GetString write SetString;
    function Add: TTemplateItem;
  end;
{$Endif}

TSciAbbrevManager=class(TComponent)
  private
    fEditor : TScintillaMemo;
    fOnTemplateMacro : TSCEvent_templatemacro;
{$Ifndef NEWABBREVS}
    fAbbrevs : TStrings;
{$Else}
    fAbbrevs : TTemplateList;
{$Endif}
    fIgnoreCase : Boolean;
    fAllowTemplateMacros : Boolean;
{$Ifndef NEWABBREVS}
    procedure SetAbbrevs(Value : TStrings);
{$Else}
    procedure SetAbbrevs(Value : TTemplateList);
{$Endif}
    procedure SetIgnoreCase(const Value : Boolean);
    procedure ExpandMatchingTemplates(var s : String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ProcessMacro(const MacroName : String;Params : String) : String;virtual;
  public
    procedure ExpandAbbrev;
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
  published
{$Ifndef NEWABBREVS}
    property Abbrevations : TStrings read fAbbrevs write SetAbbrevs;
{$Else}
    property Abbrevations : TTemplateList read fAbbrevs write SetAbbrevs;
{$Endif}
    property Editor : TScintillaMemo read fEditor write fEditor;
    property IgnoreCase : Boolean read fIgnoreCase write SetIgnoreCase default False;
    property AllowTemplateMacros : Boolean read FAllowTemplateMacros write FAllowTemplateMacros default True;
    property OnTemplateMacro : TSCEvent_templatemacro read fOnTemplateMacro write fOnTemplateMacro;
end;

implementation
uses SciLexer,SysUtils,sciUtils,SciResLang,Dialogs,Windows;

constructor TSciAbbrevManager.Create(AOwner : TComponent);
begin
  inherited;
{$Ifndef NEWABBREVS}
{$Ifdef COMPILER6_UP}
  fAbbrevs:=TStringList.Create;
  TStringList(fAbbrevs).CaseSensitive:=True;
{$Else}
  fAbbrevs:=TMyStringList.Create;
  TMyStringList(fAbbrevs).CaseSensitive:=True;
{$Endif}
{$Else}
  fAbbrevs:=TTemplateList.Create(Self,TTemplateItem);
{$Endif}
  fAllowTemplateMacros :=True;
  fIgnoreCase:=False;
end;
destructor TSciAbbrevManager.Destroy;
begin
  if fAbbrevs<>nil then fAbbrevs.Free;
  inherited;
end;

procedure TSciAbbrevManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent,Operation);
	if Operation=opRemove then
	begin
		if AComponent=FEditor then
		begin
      fEditor:=nil;
		end;
	end;
end;


{$Ifndef NEWABBREVS}
procedure TSciAbbrevManager.SetAbbrevs(Value : TStrings);
{$Else}
procedure TSciAbbrevManager.SetAbbrevs(Value : TTemplateList);
{$Endif}
begin
  fAbbrevs.Assign(Value);
end;
function TSciAbbrevManager.ProcessMacro(const MacroName : String;Params : String) : String;
var
tmp,maccapt,macprompt,macvar : String;
ps : Integer;
parms : String;
begin
  tmp:=Params;
  Result:='';
  if AnsiCompareStr(MacroName,'PROMPT')=0 then
  begin
    parms:=ProcessArgs(PChar(Params),true);
    ps:=Pos(#13,parms);
    if ps<>0 then
    begin
      maccapt:=Copy(parms,ps+1,Length(parms)-ps);
      Delete(parms,ps,Length(parms)-ps+1);
      macprompt:=parms;
    end else
    begin
      maccapt:='Prompt';
      macprompt:=parms;
    end;
    if InputQuery(maccapt,macprompt,macvar) then
    begin
     Result:=macvar;
    end;
  end else if AnsiCompareStr(MacroName,'ROW')=0 then
  begin
    Result:=ToStr(fEditor.GetCurrentLineNumber+1);
  end else if assigned(fOnTemplateMacro) then
  begin
    fOnTemplateMacro(Self,MacroName,Params,Result);
  end;
end;

procedure TSciAbbrevManager.ExpandMatchingTemplates(var s : String);
var
  macStart,macEnd,ps,psend : Integer;
  tmpmac,macname : String;
  tmp,newitm : String;
  abbrev : String;
  tplidx : Integer;
begin
  tmp:=s;
  macStart:=Pos('<^',tmp);
  if macStart=0 then Exit
  else
  begin
    newItm:=Copy(tmp,1,macStart-1);
    Delete(tmp,1,macStart-1);
    macEnd:=Pos('>',tmp);
    if macEnd<>0 then
    begin
      macname:=Copy(tmp,3,macEnd-3);
      Delete(tmp,1,macEnd);
      if macname<>'' then
      begin
        case macname[1] of
          '$': begin
                 Delete(macname,1,1);
                 ps:=Pos('(',macname);
                 if ps<>0 then
                 begin
                   tmpmac:=Copy(macname,ps+1,Length(macname)-ps);
                   Delete(macname,ps,Length(macname)-ps+1);
                   psend:=Length(tmpmac);
                   while(psend>=1) do
                   begin
                     if tmpmac[psend]=')' then
                      break;
                     Dec(psend);
                   end;
                   if psend>=1 then
                   begin
                     Delete(tmpmac,psend,1);
                   end;
                 end;
                 newItm:=newItm+ProcessMacro(macname,tmpmac);
               end;
          else
          begin
{$Ifdef NEWABBREVS}
            tplidx:=fAbbrevs.IndexOf(macname);
            if tplidx<>-1 then
            begin
              abbrev:=fAbbrevs.Items[tplidx].FTemplate;
{$Else}
            tplidx:=fAbbrevs.IndexOfName(macname);
            if tplidx<>-1 then
            begin
              abbrev:=fAbbrevs.Values[macname];
{$Endif}
              abbrev:=Unslash(abbrev);
              newItm:=newItm+abbrev;
            end;

          end;
        end;
      end;
      s:=newItm+tmp;
      ExpandMatchingTemplates(s);
    end else
      newItm:=newItm+tmp;
  end;
end;


procedure TSciAbbrevManager.ExpandAbbrev;
var
  abbrevpos,currentpos,currentlinenum,indent,
  indentExtra,linestart,abbrlen,i,extrainc,caret_pos,indentsize: Integer;
  abbrev,toabbrev,resultingabbr,linebuf : String;
  c : AnsiChar;
  isIndent,found : Boolean;
  tplidx : Integer;
begin
  if fEditor=nil then
    raise Exception.CreateResFmt(@sEditorPropertyNotAssigned,['TSciAbbrevManager']);
  currentpos:=fEditor.GetCaretInLine;
  if currentpos=0 then Exit; // expand only when at end of word.
  isIndent:=True;
  indentExtra:=0;
  indent:=0;
  currentLineNum:=fEditor.GetCurrentLineNumber;
  linestart:=fEditor.PositionFromLine(currentlinenum);
  if currentpos=0 then Exit;
  indentsize:=fEditor.GetIndent;
  SetLength(linebuf,currentpos);
  fEditor.GetRange(linestart,linestart+currentpos,PChar(linebuf));
  found:=False;
  abbrev:='';
  abbrevpos:=0;

  if KeepIndent in fEditor.Indentation then
  begin
    indent := fEditor.GetLineIndentation(currentLineNum);
  end;
  for i:=currentpos downto 0 do
  begin
    if IsWordChar(Integer(linebuf[i]))=False then
      break;
  end;
  if i<>currentpos then
  begin
    abbrevpos:=i;
    toabbrev:=System.Copy(linebuf,abbrevpos+1,currentpos);
    if abbrevpos=-1 then abbrevpos:=0;
{$Ifdef NEWABBREVS}
    tplidx:=fAbbrevs.IndexOf(toabbrev);
    if tplidx<>-1 then
    begin
      abbrev:=fAbbrevs.Items[tplidx].FTemplate;
{$Else}
    tplidx:=fAbbrevs.IndexOfName(toabbrev);
    if tplidx<>-1 then
    begin
      abbrev:=fAbbrevs.Values[toabbrev];
{$Endif}
      abbrev:=Unslash(abbrev);
      found:=True;
    end;

  end;

  if Found then
  begin
    resultingabbr:='';
    fEditor.SetSel(linestart+abbrevpos,linestart+abbrevpos+Length(toabbrev));
    caret_pos:=-1;
    fEditor.BeginUndoAction;
    if FAllowTemplateMacros=True then
    ExpandMatchingTemplates(abbrev);
    abbrlen:=Length(abbrev);
    extrainc:=0;
    for i:=1 to abbrlen do
    begin
      if i+extrainc>abbrlen then break;
      resultingabbr:='';
      c:=abbrev[i+extrainc];
		  if (isIndent=True) and (c=#9) then
      begin
			  Inc(indentExtra);
			  fEditor.SetLineIndentation(currentLineNum, indent + (indentsize * indentExtra));
        fEditor.SetCurrentPos(fEditor.GetLineIndentPosition(currentLineNum));
        fEditor.SetSel(fEditor.GetCurrentPos,fEditor.GetCurrentPos);
		  end else
      begin
        case c of
        '|': begin
                if (i<abbrlen) and (abbrev[i+1]='|') then
                begin
                  resultingabbr:='|';
                  Inc(extrainc);
                end else
                if caret_pos=-1 then
                begin
                  if i=0 then
                    caret_pos:=linestart+abbrevpos
                  else
                    caret_pos:=fEditor.GetCurrentPos;
                end;
             end;
        #10: begin
               if (fEditor.EOLStyle=eolCRLF) or (fEditor.EOLStyle=eolCR) then
               resultingabbr:=#13;
               if (fEditor.EOLStyle=eolCRLF) or (fEditor.EOLStyle=eolLF) then
               resultingabbr:=Concat(resultingabbr,#10);

             end;
        else
             resultingabbr:=abbrev[i+extrainc];
        end;
        fEditor.ReplaceSel(PChar(resultingabbr));
        if ((resultingabbr<>'') and (c=#10)) then
        begin
          isIndent:=True;
          indentExtra:=0;
          Inc(currentLineNum);
          fEditor.SetLineIndentation(currentLineNum, indent);
          fEditor.SetCurrentPos(fEditor.GetLineIndentPosition(currentLineNum));
          fEditor.SetSel(fEditor.GetCurrentPos,fEditor.GetCurrentPos);
        end else isIndent:=False;
      end;
    end;
    fEditor.EndUndoAction;
    if caret_pos<>-1 then
      fEditor.GotoPos(caret_pos);
  end;
end;


procedure TSciAbbrevManager.SetIgnoreCase(const Value : Boolean);
begin
  fIgnoreCase:=Value;
{$Ifndef NEWABBREVS}
  {$Ifdef COMPILER6_UP}
    TStringList(fAbbrevs).CaseSensitive:=not Value;
  {$Else}
    TMyStringList(fAbbrevs).CaseSensitive:=not Value;
  {$Endif}
{$Endif}
end;

{$Ifdef NEWABBREVS}
procedure TTemplateList.LoadFromFile(const FileName : String);
var
  lst : TStringList;
  cnt,i : Integer;
  itm : TTemplateItem;
  nam,value : String;
begin
  lst:=nil;
  Clear;
  try
    lst:=TStringList.Create;
    lst.LoadFromFile(FileName);
    cnt:=lst.Count;
    for i:=0 to (cnt-1) do
    begin
      nam:=lst.Names[i];
      if nam<>'' then
      begin
        value:=lst.Values[nam];
        if value<>'' then
        begin
          itm:=Add;
          itm.TemplateName:=nam;
          itm.Template:=value;
        end;
      end;
    end;

  finally
    if assigned(lst) then lst.Free;
  end;
end;

procedure TTemplateList.SaveToFile(const FileName : String);
var
  lst : TStringList;
  cnt,i : Integer;
  buf : String;
  itm : TTemplateItem;
begin
  lst:=nil;
  cnt:=Count;
  try
  if cnt>0 then
  begin
    lst:=TStringList.Create;
    for i:=0 to (cnt-1) do
    begin
      itm:=Items[i];
      buf:=itm.TemplateName+'='+itm.Template;
      lst.Add(buf);
    end;
    lst.SaveToFile(FileName);
  end;
  finally
    if assigned(lst) then
      lst.Free;
  end;
end;

procedure TTemplateItem.SetTemplateName(const Value : String);
begin
  if Value<>FTemplateName then
  begin
    FTemplateName:=Value;
    Changed(False);
  end;
end;
procedure TTemplateItem.SetTemplate(const Value : String);
begin
  if Value<>FTemplate then
  begin
    FTemplate:=Value;
    Changed(False);
  end;
end;

constructor TTemplateList.Create(AOwner : TComponent;ItemClass : TCollectionItemClass);
begin
  inherited Create(AOwner,ItemClass);
end;

procedure TTemplateList.SetItem(Index:Integer; Value: TTemplateItem);
begin
  inherited SetItem(Index,Value);
end;

function TTemplateList.GetItem(Index:Integer): TTemplateItem;
begin
  Result:=TTemplateItem(inherited GetItem(Index));
end;

function TTemplateList.Add: TTemplateItem;
begin
  Result:=TTemplateItem(inherited Add);
end;

function TTemplateList.IndexOf(const TemplateName : String) : Integer;
var
  i,cnt : Integer;
  found : Boolean;
  caseignore : Boolean;
begin
  cnt:=Count;
  Result:=-1;
  caseignore :=TSciAbbrevManager(Owner).fIgnoreCase;
  for i:=0 to cnt-1 do
  begin
    if caseignore then
      found:=SameText(Items[i].FTemplateName,TemplateName)
    else
      found:=(Items[i].FTemplateName=TemplateName);
    if found then
    begin
      Result:=i;
      Exit;
    end;
  end;
end;

procedure TTemplateList.SetString(Index : Integer;Value : String);
var
  itm : TTemplateItem;
  ps : Integer;
begin
  itm:=Items[Index];
  if assigned(itm) then
  begin
    ps:=Pos('=',Value);
    if ps<>0 then
    begin
      itm.TemplateName:=Copy(Value,1,ps);
      itm.Template:=Copy(Value,ps,Length(Value)-ps);
    end;
  end;
end;

function TTemplateList.GetString(Index : Integer) : String;
var
  itm : TTemplateItem;
begin
  itm:=Items[Index];
  if assigned(itm) then
  begin
    Result:=itm.TemplateName+'='+itm.Template;
  end else
    Result:='';
end;
{$Endif}
end.
