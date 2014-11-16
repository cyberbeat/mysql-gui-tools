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
           15/04/2005 Added MaxWidth,MaxHeight properties and the OnAutoCSelection event
                      to conform to Scintilla 1.63
			$Id: SciAutoComplete.pas,v 1.1 2004/11/18 19:51:14 hdalis Exp $
     TODO: Find a solution to the setting of the properties of a linked component
           without reassigning the properties in the Loaded procedure.
}
{$Include SciCommonDef.Inc}
unit SciAutoComplete;
interface
uses Classes, SciLexer,SciLexerMemo,SciControllerHandler;

type
  TSciBeforeShowEvent = procedure(Sender : TObject; const Position : LongInt;ListToDisplay : TStrings;var CancelDisplay : Boolean) of object;

TSciAutoComplete = class(TComponent)
	private
		FEditor : TScintillaMemo;
    fCharAddedProc : TCharAddedProc;
    fAutoCSelectionProc : TAutoCSelectionProc;
		FStrings : TStrings;
		FNumStartChars : Integer;
		FStopChars,FAutoStrings,FStartChars : String;
		FAutoCompleteDisabled,FAutoCompleteWord,FAutoCompleteWordOnlyOne,
    FAutoCSetIgnoreCase,FAutoCSetChooseSingle,FAutoCSetAutoHide,
    FAutoCSetDropRestOfWord,FAutoCSetCancelAtStart,autoCCausedByOnlyOne : boolean;
    FAutoCompleteWordCharacters : String;
    FOnAutoCSelection : TSCEvent_autocselection; 
    FOnBeforeShow : TSciBeforeShowEvent;
		procedure setStrings(Value : TStrings);
		procedure setAutoCompleteStopChars(const Value : String);
		procedure setOpt(const Index : Integer;const Value : Boolean);
		function  getOpt(Index : Integer) : Boolean;
		procedure setScintilla(Value : TScintillaMemo);
    procedure detachScintilla;
    procedure setMaxWidth(const Value : Integer);
    procedure setMaxHeight(const Value : Integer);
    function getMaxWidth : Integer;
    function getMaxHeight : Integer;
	protected
		procedure EvtCharAdded (Sender : TObject; const ch : LongInt);virtual;
    procedure EvtAutoCSelection(Sender : TObject;text : PChar);virtual;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		procedure Loaded; override;

	public
		constructor Create(AOwner : TComponent);override;
		destructor  Destroy; override;
		function  StartAutoCompleteWord(const onlyOneWord : boolean) : Boolean;
		procedure StartAutoComplete;
	published
	  property Disabled : Boolean read FAutoCompleteDisabled write FAutoCompleteDisabled default False;
		property NumStartChars : Integer read FNumStartChars write FNumStartChars;
		property StartChars : String read FStartChars write FStartChars;
		property StopChars  : String read FStopChars write setAutoCompleteStopChars;
		property AStrings : TStrings read FStrings write setStrings;
		property IgnoreCase : Boolean index 0 read getOpt Write setOpt;
		property ChooseSingle : Boolean index 1 read getOpt Write setOpt;
		property AutoHide : Boolean index 2 read getOpt Write setOpt;
		property DropRestOfWord : Boolean index 3 read getOpt Write setOpt;
		property CancelAtStart : Boolean index 4 read getOpt Write setOpt;
		property CompleteWord : boolean read FAutoCompleteWord write FAutoCompleteWord;
		property CompleteWordOnlyOne : boolean read FAutoCompleteWordOnlyOne write FAutoCompleteWordOnlyOne;
    property Editor : TScintillaMemo read FEditor write setScintilla;
    property WordCharacters : String read FAutoCompleteWordCharacters write FAutoCompleteWordCharacters;
    property OnBeforeShow : TSciBeforeShowEvent read FOnBeforeShow write FOnBeforeShow;
    property MaxWidth : Integer read getMaxWidth write setMaxWidth;
    property MaxHeight : Integer read getMaxHeight write setMaxHeight;
    property OnAutoCSelection : TSCEvent_autocselection read FOnAutoCSelection write FOnAutoCSelection;
	end;


implementation

uses Math,SciSupport,sciUtils,SysUtils;

// TSciAutoComplete
constructor TSciAutoComplete.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
	FEditor := nil;
	FStartChars :='';
	FStopChars :='';
  FAutoCompleteWordCharacters:='';
	FAutoCompleteWord:=true;
	FAutoCompleteWordOnlyOne:=true;
	FNumStartChars:=1;
	FAutoCompleteDisabled:=False;
	FStrings :=TStringList.Create;
	TStringList(FStrings).Sorted:=true;
	TStringList(FStrings).Duplicates :=dupIgnore;
  FCharAddedProc:=TCharAddedProc.Create(EvtCharAdded);
  FAutoCSelectionProc:=TAutoCSelectionProc.Create(EvtAutoCSelection);
end;

procedure TSciAutoComplete.detachScintilla;
begin
  if FEditor<>nil then
  begin
    FEditor.RemoveHandler(fCharAddedProc,CharAddedHandler);
    FEditor.RemoveHandler(fAutoCSelectionProc,AutoCSelectionHandler);
  end;
  FEditor:=nil;
end;

procedure TSciAutoComplete.Loaded;
begin
  if FEditor<>nil then
  begin
    {Update the connected Scintilla control with the settings here, as we don't have any Editor attached before now}
    IgnoreCase:=FAutoCSetIgnoreCase;
    ChooseSingle:=FAutoCSetChooseSingle;
    AutoHide:=FAutoCSetAutoHide;
    DropRestOfWord:=FAutoCSetDropRestOfWord;
    CancelAtStart:=FAutoCSetCancelAtStart;
    StopChars:=FStopChars;
  end;
end;

procedure TSciAutoComplete.setScintilla(Value : TScintillaMemo);
begin
  if Value<>nil then
  begin
    DetachScintilla;
    FEditor:=Value;
    if FAutoCompleteWordCharacters='' then
      FAutoCompleteWordCharacters:=FEditor.WordChars;
    FEditor.AddHandler(fCharAddedProc,CharAddedHandler);
    FEditor.AddHandler(fAutoCSelectionProc,AutoCSelectionHandler);
  end else
  begin
    DetachScintilla;
  end;
end;

procedure TSciAutoComplete.setMaxWidth(const Value : Integer);
begin
  if FEditor=nil then Exit;
  FEditor.AutoCSetMaxWidth(Value);
end;

procedure TSciAutoComplete.setMaxHeight(const Value : Integer);
begin
  if FEditor=nil then Exit;
  FEditor.AutoCSetMaxHeight(Value);
end;
function TSciAutoComplete.getMaxWidth : Integer;
begin
  if FEditor=nil then
  begin
    Result:=0;
    Exit;
  end;
  Result:=FEditor.AutoCGetMaxWidth;
end;

function TSciAutoComplete.getMaxHeight : Integer;
begin
  if FEditor=nil then
  begin
    Result:=0;
    Exit;
  end;
  Result:=FEditor.AutoCGetMaxHeight;
end;


procedure TSciAutoComplete.setOpt(const Index : Integer;const Value : Boolean);
begin
  case Index of // Set backup variables, so that when Loaded is called we can assign the settings.
	  0:  FAutoCSetIgnoreCase:=Value;
	  1:  FAutoCSetChooseSingle:=Value;
	  2:  FAutoCSetAutoHide:=Value;
	  3:  FAutoCSetDropRestOfWord:=Value;
	  4:  FAutoCSetCancelAtStart:=Value;
  end;
  if FEditor=nil then Exit; // Exit if we don't have any attached control.
	case Index of
    0: FEditor.AutoCSetIgnoreCase(Value);
    1: FEditor.AutoCSetChooseSingle(Value);
    2: FEditor.AutoCSetAutoHide(Value);
    3: FEditor.AutoCSetDropRestOfWord(Value);
    4: FEditor.AutoCSetCancelAtStart(Value);
	end;
end;

function TSciAutoComplete.getOpt(Index : Integer) : Boolean;
begin
  if FEditor=nil then
  begin
    case Index of // Return the settings from backup variables if we don't have any attached control.
      0:  Result:=FAutoCSetIgnoreCase;
      1:  Result:=FAutoCSetChooseSingle;
      2:  Result:=FAutoCSetAutoHide;
      3:  Result:=FAutoCSetDropRestOfWord;
      4:  Result:=FAutoCSetCancelAtStart;
      else Result:=False;
    end;
    Exit; // Exit if we don't have any attached control.
  end;
	case Index of
    0: Result :=FEditor.AutoCGetIgnoreCase;
    1: Result :=FEditor.AutoCGetChooseSingle;
    2: Result :=FEditor.AutoCGetAutoHide;
    3: Result :=FEditor.AutoCGetDropRestOfWord;
    4: Result :=FEditor.AutoCGetCancelAtStart;
    else Result :=False;
	end;
end;

procedure TSciAutoComplete.setStrings(Value : TStrings);
begin
  FStrings.Assign(Value);
  TStringList(FStrings).Sort;
end;

procedure TSciAutoComplete.SetAutoCompleteStopChars(const Value : String);
begin
	FStopChars :=Value;
  if (FEditor=nil) then Exit;
	FEditor.AutoCStops(PChar(Value));
end;

destructor TSciAutoComplete.Destroy;
begin
	FStrings.Free;
  fCharAddedProc.Free;
	inherited Destroy;
end;

procedure TSciAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TSciAutoComplete.StartAutoComplete;
var
  line,startswith : String;
  linenum,current,startword,typedlen : LongInt;
  cancelit : Boolean;
begin

	linenum :=FEditor.GetCurrentLineNumber;
	line :=FEditor.GetLineS(linenum);
	current :=FEditor.GetCaretInLine;
	startword :=current;
  if startword=0 then Exit;
	while ((startword>0) and (CharPos(FEditor.WordChars,AnsiChar(line[startword-1]),1)<>0) or (CharPos(StartChars,AnsiChar(line[startword-1]),1)<>0))
		do startword:=startword-1;
	typedlen :=current-startword+1;

// Remove the commenting of the code below to use the NumStartChars.. Not sure if it's correct yet.
//	if typedlen>NumStartChars then
//  begin
    startswith:=Copy(WideString(line), startword, typedlen);
    if assigned(FOnBeforeShow) then
    begin
      cancelit:=False;
      FOnBeforeShow(Self,FEditor.GetCurrentPos,FStrings,cancelit);
      if cancelit then Exit;
    end;

    FillMatching(startswith,FAutoStrings,AStrings,IgnoreCase);
    FAutoStrings:=Trim(FAutoStrings);
    if FAutoStrings<>'' then
    FEditor.AutoCShow(typedlen,PChar(FAutoStrings));
//  end;
end;

function TSciAutoComplete.StartAutoCompleteWord(const onlyOneWord : boolean) : Boolean;
var
	line,wordsNear,root : String;
	current,startword,doclen,flags,posCurrentWord,minWordLength,nwords,wordMaxSize,wordlen,len,posFind : LongInt;
	allnumber : boolean;
	ft : TTextToFind;
	wordstart : array[0..160] of char;
	wordend : PChar;
begin
	line :=FEditor.GetLineS;
	wordMaxSize:=80;
	minWordLength:=0;
	nwords:=0;

	current:=FEditor.GetCaretInLine;
	startword:=current;
	allnumber :=true;
	doclen:=FEditor.GetLength;
	while ((startword>0) and (CharPos(FEditor.WordChars,AnsiChar(line[startword-1]),1)<>0)) do
	begin
		startword:=startword-1;
		if Boolean(line[startword] < '0') or Boolean(line[startword] > '9') then allNumber := false;
	end;
	if Boolean(startword=current) or Boolean(allnumber=true) then
	begin
		result :=true;
		Exit;
	end;
	root:=Copy(WideString(line), startword, current-startword+1);
	ft.lpstrText:=PChar(root);
	ft.chrg.cpMin :=0;
	ft.chrgText.cpMin :=0;
	ft.chrgText.cpMax :=0;
	flags:=SCFIND_WORDSTART;
	if not IgnoreCase then
		flags :=flags or SCFIND_MATCHCASE;
	posCurrentWord :=FEditor.GetCurrentPos-Length(root);
	wordsNear:='';
	while true do
	begin
		ft.chrg.cpMax := doclen;
		posFind:=FEditor.FindTextX(flags,@ft);
		if (posFind=-1) or (posFind>=doclen) then
		begin
			Break;
		end;
		if posFind=posCurrentWord then
		begin
			ft.chrg.cpMin := posFind + Length(root);
			Continue;
		end;
		FEditor.GetRange(posFind, Min(posFind + wordMaxSize - 3, doclen), wordstart);
		wordend:=Pointer(wordstart + Length(root));
		while (CharPos(FAutoCompleteWordCharacters,AnsiChar(wordend^))>0) do Inc(wordend); //compare to the wordchars property
		wordend^:=#0;
		wordlen :=(wordend-@wordstart);
		if wordlen>Length(root) then
		begin
      if (Pos(wordstart+' ',wordsNear+' ')=0) then
			begin
				if(nwords>0) then
					  wordsNear:=wordsNear+' '+String(wordstart)
				else
				wordsNear:=String(wordstart);

				if minWordLength<wordlen then minWordLength:=wordlen;
				nwords:=nwords+1;
				if (onlyOneWord=true) and Boolean(nwords>1) then
				begin
					result :=true;
					Exit;
				end;
			end;
		end;
		ft.chrg.cpMin := posFind + wordlen;
	end;
	len:=Length(wordsNear);
	if (len>=NumStartChars) and ((not onlyOneWord) or (minWordLength>Length(root))) then
	begin
		FEditor.AutoCShow(Length(root),PChar(wordsNear));
	end else FEditor.AutoCCancel;
	result :=true;
end;

procedure TSciAutoComplete.EvtAutoCSelection(Sender : TObject;text : PChar);
begin
  if assigned(FOnAutoCSelection) then
    FOnAutoCSelection(Sender,text);
end;
procedure TSciAutoComplete.EvtCharAdded (Sender : TObject; const ch : LongInt);
var
	crange : TCharacterRange;
	selStart,selEnd,style : LongInt;
begin
  if FAutoCompleteDisabled=True then Exit;

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
        Exit; // We're in a calltip.. don't disturb..
      end else
				if FEditor.AutoCActive then
				begin
					if (CharPos(FEditor.WordChars,AnsiChar(ch),1)=0) then
					begin
						FEditor.AutoCCancel;
						if (CharPos(StartChars,AnsiChar(ch),1)<>0) or ((CompleteWord=False)) then
						begin
							StartAutoComplete;
						end;
					end else
          if (autoCCausedByOnlyOne) then
            StartAutoCompleteWord(CompleteWordOnlyOne);
				end else
				begin
          autoCCausedByOnlyOne := false;
          if (CharPos(StartChars,AnsiChar(ch),1)<>0) or ((CompleteWord=False) and (CharPos(FEditor.WordChars,AnsiChar(ch),1)<>0)) then
          begin
            StartAutoComplete;
          end else
          if (CompleteWord=true) and ((CharPos(FEditor.WordChars,AnsiChar(ch),1)<>0)) then
          begin
            StartAutoCompleteWord(CompleteWordOnlyOne);
            autoCCausedByOnlyOne := FEditor.AutoCActive;
          end else
          if (CompleteWord=False) and ((CharPos(FEditor.WordChars,AnsiChar(ch),1)<>0)) then
          begin
            StartAutoComplete;
          end;
        end;
		end;
	end;
end;
end.
