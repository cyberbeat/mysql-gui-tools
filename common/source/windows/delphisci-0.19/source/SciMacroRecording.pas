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
			$Id:  $

}
{$Include SciCommonDef.Inc}
unit SciMacroRecording;
interface
uses Classes, SciLexerMemo,SciControllerHandler;
type

TSciMacroEvent=record
  message,wParam,lParam : LongInt;
  TxtLen : LongInt;
  Txt : String;
end;

TSciMacroItem=record
Title : String;
macroevents : array of TSciMacroEvent;
end;

PSciMacroEvent=^TSciMacroEvent;
TSCEvent_macroitemevent = procedure(Sender : TObject; MacroItem : PSciMacroEvent) of object;

TSciMacroRecorder = class(TComponent)
	private
    fMacroRecordProc : TMacroRecordProc;
    FEditor : TScintillaMemo;
    FIsRecording,FMacroLoaded,FMacroRunning : Boolean;
    fMacro  : TList;
    procedure SetIsRecording(value : Boolean);
    procedure SetScintilla(Value : TScintillaMemo);
    procedure DetachScintilla;
protected
    procedure EvtMacroRecord (Sender : TObject; const message : LongInt; const wParam : LongInt; const lParam : LongInt);virtual;
	  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
	public
		constructor Create(AOwner : TComponent);override;
		destructor  Destroy; override;
    procedure   Clear;
    procedure   SaveToStream(stream : TStream);
    procedure   LoadFromStream(stream : TStream);
    procedure   Execute;
    procedure   StartRecord;
    procedure   StopRecord;
    property    IsRecording : Boolean read FIsRecording write SetIsRecording;
    property    MacroLoaded : Boolean read FMacroLoaded;
    property    MacroRunning : Boolean read FMacroRunning;
  published
    property Editor : TScintillaMemo read FEditor write SetScintilla;
end;


implementation
uses SciSupport;

// TSciMacroRecorder
constructor TSciMacroRecorder.Create(AOwner : TComponent);
begin
  FEditor:=nil;
  inherited Create(AOwner);
  FMacro:=TList.Create;
  FIsRecording:=False;
  FMacroLoaded:=False;
  FMacroRunning:=False;
  FMacroRecordProc:=TMacroRecordProc.Create(EvtMacroRecord);
end;

procedure TSciMacroRecorder.DetachScintilla;
begin
  if FEditor<>nil then
  begin
    FEditor.RemoveHandler(fMacroRecordProc,MacroRecordHandler);
  end;
  FEditor:=nil;
end;

procedure TSciMacroRecorder.SetIsRecording(value : Boolean);
begin
  if Value=True then
    StartRecord
  else
    StopRecord;
end;
procedure TSciMacroRecorder.Notification(AComponent: TComponent; Operation: TOperation);
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

destructor TSciMacroRecorder.Destroy;
begin
  fMacroRecordProc.Free;
  FMacro.Free;
  Inherited Destroy;
end;

procedure TSciMacroRecorder.SetScintilla(Value : TScintillaMemo);
begin
  if Value<>nil then
  begin
    DetachScintilla;
    FEditor:=Value;
    FEditor.AddHandler(fMacroRecordProc,MacroRecordHandler);
  end else
  begin
    DetachScintilla;
  end;
end;
procedure TSciMacroRecorder.EvtMacroRecord(Sender : TObject; const message : LongInt; const wParam : LongInt; const lParam : LongInt);
var
m : PSciMacroEvent;
begin
  New(m);
  m^.message:=message;
  m^.wParam:=wParam;
  m^.lParam:=lParam;
  case m^.message of
    SCI_ADDTEXT,
    SCI_INSERTTEXT,
    SCI_REPLACESEL,
    SCI_SETTEXT,
    SCI_APPENDTEXT,
    SCI_REPLACETARGET,
    SCI_SEARCHINTARGET,
    SCI_REPLACETARGETRE
    ,SCI_COPYTEXT,
    SCI_SEARCHNEXT,
    SCI_SEARCHPREV:
     begin
       m^.Txt:=PChar(lParam);
       m^.TxtLen:=Length(m^.Txt);
     end;
  end;
  FMacro.Add(m);
  FMacroLoaded:=True;
end;

procedure TSciMacroRecorder.Clear;
var
i,cnt : Integer;
begin
 cnt :=FMacro.Count;
 for i:=0 to (cnt-1) do
 begin
   Dispose(FMacro.Items[i]);
 end;
 FMacro.Clear;
 FMacro.Capacity:=0;
 FMacroLoaded:=False;
end;

procedure   TSciMacroRecorder.LoadFromStream(stream : TStream);
var
  i,cnt : LongInt;
  m : PSciMacroEvent;
begin
  cnt :=FMacro.Count;
  stream.ReadBuffer(cnt,SizeOf(LongInt));
  for i:=0 to (cnt-1) do
  begin
    New(m);
    stream.ReadBuffer(m^.message,SizeOf(LongInt));
    stream.ReadBuffer(m^.wParam,SizeOf(LongInt));
    stream.ReadBuffer(m^.lParam,SizeOf(LongInt));
    stream.ReadBuffer(m^.TxtLen,SizeOf(LongInt));
    SetLength(m^.Txt,m^.TxtLen);
    stream.ReadBuffer(m^.Txt[1],m^.TxtLen);
    FMacro.Add(m);
  end;
  FMacroLoaded:=(FMacro.Count>0);
end;

procedure   TSciMacroRecorder.SaveToStream(stream : TStream);
var
  i,cnt : LongInt;
  m : PSciMacroEvent;
begin
  cnt :=FMacro.Count;
  if cnt>0 then
  begin
    stream.WriteBuffer(cnt,SizeOf(LongInt));
    for i:=0 to (cnt-1) do
    begin
      m:=FMacro.Items[i];
      stream.WriteBuffer(m^.message,SizeOf(LongInt));
      stream.WriteBuffer(m^.wParam,SizeOf(LongInt));
      stream.WriteBuffer(m^.lParam,SizeOf(LongInt));
      stream.WriteBuffer(m^.TxtLen,SizeOf(LongInt));
      stream.WriteBuffer(m^.Txt[1],m^.TxtLen);
    end;
  end;
end;

procedure   TSciMacroRecorder.Execute;
var
i,cnt : Integer;
m : PSciMacroEvent;
begin
  cnt :=FMacro.Count;
  if cnt>0 then
  begin
    try
      FMacroRunning:=True;
      for i:=0 to (cnt-1) do
      begin
        m:=FMacro.Items[i];
        case m^.message of
          SCI_ADDTEXT,
          SCI_INSERTTEXT,
          SCI_REPLACESEL,
          SCI_SETTEXT,
          SCI_APPENDTEXT,
          SCI_REPLACETARGET,
          SCI_SEARCHINTARGET,
          SCI_REPLACETARGETRE
          ,SCI_COPYTEXT,
          SCI_SEARCHNEXT,
          SCI_SEARCHPREV:
          begin
            m^.lParam:=LongInt(PChar(m^.Txt));
          end;
        end;
        FEditor.Perform(m^.message,m^.wParam,m^.lParam);
      end;
    finally
      FMacroRunning:=False;
    end;
  end;
end;

procedure   TSciMacroRecorder.StartRecord;
begin
  Clear;
  FIsRecording:=True;
  FEditor.StartRecord;
end;
procedure   TSciMacroRecorder.StopRecord;
begin
  FIsRecording:=False;
  FEditor.StopRecord;
end;
end.
