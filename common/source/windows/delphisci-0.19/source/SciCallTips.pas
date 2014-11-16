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
	Created: 18/11/2004, 19:51:14
  History: 18/11/2004 Initial Release
           12/04/2005 Added the OnBeforeShow event
			$Id: SciCallTips.pas,v 1.1 2004/11/18 19:51:14 hdalis Exp $

     TODO: Find a solution to the setting of the properties of a linked component
           without reassigning the properties in the Loaded procedure.

}
{$Include SciCommonDef.Inc}
unit SciCallTips;
interface
uses Classes, SysUtils,SciLexerMemo,Graphics,SciControllerHandler;
type
  TSciBeforeShowEvent = procedure(Sender : TObject; const Position : LongInt;ListToDisplay : TStrings;var CancelDisplay : Boolean) of object;

TSciCallTips = class(TComponent)
	private
    fCharAddedProc : TCharAddedProc;
    fCallTipClickProc : TCallTipClickProc;
    braceCount,FStartCalltipWord,FCurrentCallTip,FMaxCallTips,FLastPosCallTip : LongInt;
    FApiStrings : TStrings;
    FCurrentCallTipWord,FFunctionDefinition,FCalltipWordCharacters, FCalltipParametersStart, FCalltipParametersEnd,FCalltipParametersSeparators,FCalltipEndDefinition : String;
    FCallTipFore,FCallTipBack,FCallTipHlt : TColor;
    FCalltipDisabled,FIgnoreCase : Boolean;
    FEditor : TScintillaMemo;
    FOnBeforeShow : TSciBeforeShowEvent;
    
    procedure SetApiStrings(Value : TStrings);
    procedure SetCallTipHltColor(Value : TColor);
    procedure SetCallTipForeColor(Value : TColor);
    procedure SetCallTipBackColor(Value : TColor);
    procedure FillFunctionDefinition(xpos : LongInt=-1);
    procedure ContinueCallTip;
    procedure CallMatching(const startwith : String;var deststr : String;fromlist : TStrings;const ignorecase : Boolean;const stopat:Char=#0;const wordindex:LongInt=-1;matchlen:LongInt=-1);
    procedure SetScintilla(Value : TScintillaMemo);
    procedure DetachScintilla;
protected
    procedure Loaded; override;
    procedure EvtCharAdded (Sender : TObject; const ch : LongInt);virtual;
    procedure EvtCallTipClick(Sender : TObject; const position : LongInt);virtual;
	  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
	public
		constructor Create(AOwner : TComponent);override;
		destructor  Destroy; override;
    procedure StartCallTip;

  published
    property ApiStrings : TStrings read FApiStrings write SetApiStrings;
    property BackColor : TColor read FCallTipBack write SetCallTipBackColor default clDefault;
    property Disabled : Boolean read FCalltipDisabled write FCalltipDisabled default False;
    property EndDefinition : String read FCallTipEndDefinition write FCallTipEndDefinition;
    property Editor : TScintillaMemo read FEditor write SetScintilla;
    property ForeColor : TColor read FCallTipFore write SetCallTipForeColor default clDefault;
    property HighlightColor : TColor read FCallTipHlt write SetCallTipHltColor default clDefault;
    property ParametersEnd : String read FCalltipParametersEnd write FCalltipParametersEnd;
    property ParametersSeparators : String read FCallTipParametersSeparators write FCallTipParametersSeparators;
    property ParametersStart : String read FCalltipParametersStart write FCalltipParametersStart;
    property WordCharacters : String read FCalltipWordCharacters write FCalltipWordCharacters;
    property IgnoreCase : Boolean read FIgnoreCase write FIgnoreCase default False;
    property OnBeforeShow : TSciBeforeShowEvent read FOnBeforeShow write FOnBeforeShow;

end;


implementation

uses SciSupport,SciLexer,sciUtils,Windows;

// TSciCallTips
constructor TSciCallTips.Create(AOwner : TComponent);
begin
  FEditor:=nil;
  inherited Create(AOwner);
  braceCount:=0;
  FCalltipParametersStart:='(';
  FCalltipParametersEnd:=')';
  FCalltipParametersSeparators:=',';
  FCalltipEndDefinition:=')';
  FCalltipWordCharacters:='';
  FCalltipDisabled:=False;
  FCallTipFore:=clDefault;
  FCallTipBack:=clDefault;
  FCallTipHlt:=clDefault;
  FIgnoreCase:=False;
	FApiStrings :=TStringList.Create;
	TStringList(FApiStrings).Sorted:=True;
	TStringList(FApiStrings).Duplicates:=dupIgnore;
  FCharAddedProc:=TCharAddedProc.Create(EvtCharAdded);
  FCallTipClickProc:=TCallTipClickProc.Create(EvtCallTipClick);
end;

procedure TSciCallTips.DetachScintilla;
begin
  if FEditor<>nil then
  begin
    FEditor.RemoveHandler(fCharAddedProc,CharAddedHandler);
    FEditor.RemoveHandler(fCallTipClickProc,CallTipClickHandler);
  end;
  FEditor:=nil;
end;
procedure TSciCallTips.Loaded;
begin
  Inherited Loaded;
  if FEditor<>nil then
  begin
  {Update the connected Scintilla control with the settings here, as we don't have any Editor attached before now}
    ForeColor:=FCallTipFore;
    BackColor:=FCallTipBack;
    HighlightColor:=FCallTipHlt;
  end;
end;
procedure TSciCallTips.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent,Operation);
	if Operation=opRemove then
	begin
		if AComponent=FEditor then
		begin
      DetachScintilla;
		end;
	end;
end;

destructor TSciCallTips.Destroy;
begin
	if FApiStrings<>nil then FApiStrings.Free;
  fCharAddedProc.Free;
  fCallTipClickProc.Free;
  Inherited Destroy;
end;

procedure TSciCallTips.SetScintilla(Value : TScintillaMemo);
begin
  if Value<>nil then
  begin
    DetachScintilla;
    FEditor:=Value;
    FEditor.AddHandler(fCharAddedProc,CharAddedHandler);
    FEditor.AddHandler(fCallTipClickProc,CallTipClickHandler);
    if FCallTipWordCharacters='' then
      FCallTipWordCharacters:=FEditor.WordChars;
  end else
  begin
    DetachScintilla;
  end;
end;

procedure TSciCallTips.EvtCharAdded (Sender : TObject; const ch : LongInt);
var
	crange : TCharacterRange;
	selStart,selEnd,style : LongInt;
begin
	crange:=FEditor.GetSelectionRng();
	selStart:=crange.cpMin;
	selEnd:=crange.cpMax;
	if (selEnd=selStart) and (selStart>0) then
	begin
    style:=FEditor.GetStyleAt(selStart-1);
		if style<>1 then
		begin
			if FEditor.CallTipActive then
			begin
        if CharPos(FCalltipParametersEnd,AnsiChar(ch),1)<>0 then
        begin
          Dec(braceCount);
          if braceCount<1 then
            FEditor.CallTipCancel
          else
          begin
            StartCallTip;
          end;
        end else
        if CharPos(FCalltipParametersStart,AnsiChar(ch),1)<>0 then
        begin
          Inc(braceCount);
          StartCallTip;
        end else
        begin
          ContinueCallTip;
        end;
			end else
      if FEditor.AutoCActive then //We're in a autocompletesession, disturb if neccesary.
      begin
        if (CharPos(FCalltipParametersStart,AnsiChar(ch),1)<>0) then
        begin
          Inc(braceCount);
          StartCallTip;
        end else
        if (CharPos(FCalltipParametersEnd,AnsiChar(ch),1)<>0) then
        begin
          Dec(braceCount);
        end;
      end else
      begin
        if (CharPos(FCalltipParametersStart,AnsiChar(ch),1)<>0) then
        begin
          braceCount:=1;
          StartCallTip;
        end;
      end;
		end;
	end;
end;

procedure TSciCallTips.SetCallTipHltColor(Value : TColor);
begin
  FCallTipHlt:=Value;
  if Value<>clDefault then
  begin
    if FEditor=nil then Exit;
    FEditor.CallTipSetForeHlt(Value);
  end;
end;
procedure TSciCallTips.SetCallTipForeColor(Value : TColor);
begin
  FCallTipFore:=Value;
  if Value<>clDefault then
  begin
    if FEditor=nil then Exit;
    FEditor.CallTipSetFore(Value);
  end;
end;
procedure TSciCallTips.SetCallTipBackColor(Value : TColor);
begin
  FCallTipBack:=Value;
  if Value<>clDefault then
  begin
    if FEditor=nil then Exit;
    FEditor.CallTipSetBack(Value);
  end;
end;
procedure TSciCallTips.SetApiStrings(Value : TStrings);
begin
  FApiStrings.Assign(Value);
end;
procedure TSciCallTips.CallMatching(const startwith : String;var deststr : String;fromlist : TStrings;const ignorecase : Boolean;const stopat:Char=#0;const wordindex:LongInt=-1;matchlen:LongInt=-1);
var
  cnt,i,numadded,stopatwhere,amatch : LongInt;
  tmp : String;
begin
	deststr:='';
	numadded:=0;
	cnt:=fromlist.Count;
  if matchlen=-1 then
    matchlen:=Length(startwith);
	for i:=0 to (cnt-1) do
	begin
		tmp:=fromlist.Strings[i];
    if stopat<>#0 then
    begin
      stopatwhere:=Pos(stopat,tmp);
      if stopatwhere>0 then tmp:=Copy(tmp,1,stopatwhere-1);
    end;
    //amatch:=-1;
    if stopat<>#0 then
    begin
      if ignorecase=True then
        amatch:=AnsiStrIComp(PChar(tmp),PChar(startwith))
      else
        amatch:=AnsiStrComp(PChar(tmp),PChar(startwith));
    end else
    begin
      if ignorecase=True then
        amatch:=AnsiStrLIComp(PChar(tmp),PChar(startwith),matchlen)
      else
        amatch:=AnsiStrLComp(PChar(tmp),PChar(startwith),matchlen);
    end;
      if amatch=0 then
      begin
        if (wordindex>=0) and (numadded=(wordindex)) then
        begin
          deststr:=tmp;
          Exit;
        end;
        if numadded>0 then deststr:=deststr+' ';
        deststr:=deststr+tmp;
        numadded:=numadded+1;
      end;
	end;
end;


procedure TSciCallTips.FillFunctionDefinition(xpos : LongInt);
var
	Words : String;
	spacepos : LongInt;
	posEndDef : LongInt;
  calltipenddeflen : LongInt;
  funclen : LongInt;
begin
	if xpos>0 then FLastPosCallTip:=xpos;
	CallMatching(FCurrentCallTipWord,Words,FApiStrings,FIgnoreCase,FCalltipParametersStart[1],-1,Length(FCurrentCallTipWord));
	if Words<>'' then
	begin
		FMaxCallTips:=1;
    spacepos:=1;
    repeat
      spacepos:=CharPos(Words,' ',spacepos);
      if spacepos<>0 then
      begin
        Inc(FMaxCallTips);
        Inc(spacepos);
      end;
    until spacepos=0;
		CallMatching(FCurrentCallTipWord,Words,FApiStrings,FIgnoreCase,#0,FCurrentCallTip,Length(FCurrentCallTipWord));
		if Words<>'' then
		begin
			FFunctionDefinition:=Words;
			if FMaxCallTips>1 then
        Insert(#1,FFunctionDefinition,0);
			if FCalltipEndDefinition<>'' then
			begin
        calltipenddeflen:=Length(FCallTipEndDefinition);
				posEndDef:=Pos(FCalltipEndDefinition,FFunctionDefinition);
        funclen:=Length(FFunctionDefinition);
				if FMaxCallTips>1 then
				begin
					if (posEndDef>0) and ((posEndDef+calltipenddeflen+1)< Length(FFunctionDefinition)) then
					begin
            Insert(#10#02,FFunctionDefinition,posEndDef+calltipenddeflen);
					end else
					begin
						FFunctionDefinition:=FFunctionDefinition+#10#02;
					end;
				end else
				begin
					if (posEndDef>1) and ((posEndDef+calltipenddeflen+1)< funclen) then
					begin
            Insert(#10,FFunctionDefinition,posEndDef+calltipenddeflen);
					end;
				end;
			end;
			FEditor.CallTipShow(FLastPosCallTip-Length(FCurrentCallTipWord),PChar(FFunctionDefinition));
			ContinueCallTip;
		end;
	end;
end;

procedure TSciCallTips.StartCallTip;
var
line : String;
current,xpos,braces : LongInt;
cancelit : Boolean;
begin
  with FEditor do
  begin
  if (FCalltipDisabled=True) then Exit;
    FCurrentCallTip:=0;
    FCurrentCalltipWord:='';
    line:=GetLineS;
    current:=GetCaretInLine;
    xpos:=GetCurrentPos;
    if assigned(FOnBeforeShow) then
    begin
      cancelit:=False;
      FOnBeforeShow(Self,xpos,FApiStrings,cancelit);
      if cancelit then Exit;
    end;

    repeat
      braces:=0;
      while ((current > 0) and ((braces<>0) or (CharPos(FCalltipParametersStart,AnsiChar(line[current]),1)=0))) do
      begin
        if (CharPos(FCalltipParametersStart,AnsiChar(line[current]),1)<>0) then
          Dec(braces)
        else if (CharPos(FCalltipParametersEnd,AnsiChar(line[current]),1)<>0) then
          Inc(braces);
        Dec(current);
        Dec(xpos);
      end;
      if current>0 then
      begin
        Dec(current);
        Dec(xpos);
      end else
        break;
      while (current>0) and (IsSpace(Integer(line[current]))) do
      begin
        Dec(current);
        Dec(xpos);
      end;
    until ((current <= 0) or (CharPos(FCalltipWordCharacters,AnsiChar(line[current]),1)<>0));
    if current<=0 then Exit;
    FStartCalltipWord:=current;
    while((FStartCalltipWord>0) and (CharPos(FCalltipWordCharacters,AnsiChar(line[FStartCalltipWord-1]),1)<>0)) do
    begin
      Dec(FStartCalltipWord);
    end;
    FCurrentCallTipWord:=System.Copy(line,FStartCalltipWord,(current-FStartCalltipWord)+1);
    FFunctionDefinition:='';
    FillFunctionDefinition(xpos);
  end;
end;

procedure TSciCallTips.ContinueCallTip;
var
	line : String;
	current,braces,commas,i : LongInt;
	startHighlight,endHighlight : LongInt;
  funclen : Integer;
begin
  if (FCalltipDisabled=True) then Exit;
  with FEditor do
  begin
    startHighlight:=0;
    commas:=0;
    braces:=0;
    funclen:=Length(FFunctionDefinition);
    FFunctionDefinition:=FFunctionDefinition+' ';
    line:=GetLineS;
    current:=GetCaretInLine;
    for i:=FStartCalltipWord to (current) do
    begin
      if (CharPos(FCalltipParametersStart,AnsiChar(line[i]),1)<>0) then
        Inc(braces)
      else if (CharPos(FCalltipParametersEnd,AnsiChar(line[i]),1)<>0) and (braces>0) then
        Dec(braces)
      else if (braces=1) and (CharPos(FCalltipParametersSeparators,AnsiChar(line[i]),1)<>0) then
        Inc(commas);
    end;
    while (startHighlight<funclen) and
          (CharPos(FCalltipParametersStart,AnsiChar(FFunctionDefinition[startHighlight]),1)=0) do
    begin
      Inc(startHighlight);
    end;
    if (CharPos(FCalltipParametersStart,AnsiChar(FFunctionDefinition[startHighlight]),1)<>0) then
    begin
      Inc(startHighlight);
    end;
    while ((startHighlight<funclen) and (commas > 0)) do
    begin
      if (CharPos(FCalltipParametersSeparators,AnsiChar(FFunctionDefinition[startHighlight]),1)<>0) then
        Dec(commas);
      if (CharPos(FCalltipParametersEnd,AnsiChar(FFunctionDefinition[startHighlight]),1)<>0) then
        commas:=0
      else
        Inc(startHighlight);
    end;
    if (CharPos(FCalltipParametersSeparators,AnsiChar(FFunctionDefinition[startHighlight]),1)<>0) then
      Inc(startHighlight);
    endHighlight:=startHighlight;
    while (endHighlight<funclen) and (CharPos(FCalltipParametersSeparators,AnsiChar(FFunctionDefinition[endHighlight]),1)=0) and (CharPos(FCalltipParametersEnd,AnsiChar(FFunctionDefinition[endHighlight]),1)=0) do
    begin
      Inc(endHighlight);
    end;
    // Adjust the start and end so we do not highlight end or separators.
    if (CharPos(FCalltipParametersEnd,AnsiChar(FFunctionDefinition[endHighlight]),1)<>0) or (CharPos(FCalltipParametersSeparators,AnsiChar(FFunctionDefinition[endHighlight]),1)<>0) then Dec(endHighlight);
    if (CharPos(FCalltipWordCharacters,AnsiChar(FFunctionDefinition[startHighlight]),1)<>0) then Dec(startHighlight);
    CallTipSetHlt(startHighlight,endHighlight);
  end;
end;

procedure TSciCallTips.EvtCallTipClick(Sender : TObject; const position : LongInt);
begin
    case position of
    1:begin
        if (FCurrentCallTip>0) then
        begin
          Dec(FCurrentCallTip);
          FillFunctionDefinition;
        end;
      end;
    0:begin
        if ((FCurrentCallTip+1)<FMaxCalltips)  then
        begin
          Inc(FCurrentCallTip);
          FillFunctionDefinition;
        end;
      end;
    end;
end;


end.
