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
      $Id: SciControllerHandler.pas,v 1.5 2004/11/18 04:29:51 hdalis Exp $
  Purpose: This manages, and forwards OnCharAdded, OnMacroRecord, OnCallTipClick
           to the registered components..
           Internally used.

  History: 18/11/2004 Initial Release
           15/04/2005 Changed the connection to the ownerclass to wait until the
                      class calls TSciController.Connect, cause it didn't get the
                      initialized events of the owner when the owner created TSciController
                      in the Create constructor.
}
{$Include SciCommonDef.Inc}
unit SciControllerHandler;
interface
uses Classes,SciLexer;

type
TControllerHandlerType=(
  CharAddedHandler,
  MacroRecordHandler,
  CallTipClickHandler,
  AutoCSelectionHandler);

TControllerControl = class (TScintillaBase)
  public
    property OnCharAdded;
    property OnCallTipClick;
    property OnMacroRecord;
    property OnAutoCSelection;
  end;

TCharAddedProc = class (TObject)
  private
    fCharAddedProc : TSCEvent_charadded;
  public
    constructor Create (aCharAddedProc : TSCEvent_charadded);
    property OnCharAdded : TSCEvent_charadded read fCharAddedProc write fCharAddedProc;
    end;

TMacroRecordProc = class (TObject)
  private
    fMacroRecordProc : TSCEvent_macrorecord;
  public
    constructor Create (aMacroRecordProc : TSCEvent_macrorecord);
    property OnMacroRecord : TSCEvent_macrorecord read fMacroRecordProc write fMacroRecordProc;
    end;

TCallTipClickProc = class (TObject)
  private
    fCallTipClickProc : TSCEvent_calltipclick;
  public
    constructor Create (aCallTipClickProc : TSCEvent_calltipclick);
    property OnCallTipClick : TSCEvent_calltipclick read fCallTipClickProc write fCallTipClickProc;
    end;

TAutoCSelectionProc = class (TObject)
  private
    fAutoCSelectionProc : TSCEvent_autocselection;
  public
    constructor Create (aAutoCSelectionProc : TSCEvent_autocselection);
    property OnAutoCSelection : TSCEvent_autocselection read fAutoCSelectionProc write fAutoCSelectionProc;
    end;

TSciController = class (TObject)
  private
    fControl : TControllerControl;
    fCharAddedChain : TList;
    fInCharAdded : Boolean;
    fOldCharAdded : TSCEvent_charadded;
    fCallTipClickChain : TList;
    fOldCallTipClick : TSCEvent_calltipclick;
    fMacroRecordChain : TList;
    fOldMacroRecord : TSCEvent_macrorecord;
    fAutoCSelectionChain : TList;
    fOldAutoCSelection : TSCEvent_autocselection;
    FConnected : Boolean;
    procedure SetOnCharAdded (const Value: TSCEvent_charadded);
    procedure SetOnCallTipClick (const Value: TSCEvent_calltipclick);
    procedure SetOnMacroRecord (const Value: TSCEvent_macrorecord);
    procedure SetOnAutoCSelection (const Value: TSCEvent_autocselection);
  protected
    procedure EditorCharAdded (Sender : TObject; const ch : LongInt);
    procedure EditorCallTipClick (Sender : TObject; const position : LongInt);
    procedure EditorAutoCSelection (Sender : TObject; text : PChar);
    procedure EditorMacroRecord (Sender : TObject; const message : LongInt; const wParam : LongInt; const lParam : LongInt);
  public
    constructor Create (aControl : TScintillaBase);
    procedure Connect;
    destructor Destroy; override;
    
    procedure ExecuteCharAdded (Sender: TObject; const ch : LongInt);
    procedure AddCharAddedHandler (aHandler : TCharAddedProc);
    procedure RemoveCharAddedHandler (aHandler : TCharAddedProc);

    procedure ExecuteMacroRecord (Sender: TObject;const message : LongInt; const wParam : LongInt; const lParam : LongInt);
    procedure AddMacroRecordHandler (aHandler : TMacroRecordProc);
    procedure RemoveMacroRecordHandler (aHandler : TMacroRecordProc);

    procedure ExecuteCallTipClick (Sender: TObject; const position : LongInt);
    procedure AddCallTipClickHandler (aHandler : TCallTipClickProc);
    procedure RemoveCallTipClickHandler (aHandler : TCallTipClickProc);

    procedure AddAutoCSelectionHandler (aHandler : TAutoCSelectionProc);
    procedure RemoveAutoCSelectionHandler (aHandler : TAutoCSelectionProc);
    procedure ExecuteAutoCSelection (Sender: TObject; text : PChar);

    property  OnCharAdded : TSCEvent_charadded read fOldCharAdded write SetOnCharAdded;
    property  OnCallTipClick : TSCEvent_calltipclick read fOldCallTipClick write SetOnCallTipClick;
    property  OnMacroRecord : TSCEvent_macrorecord read fOldMacroRecord write SetOnMacroRecord;
    property  OnAutoCSelection : TSCEvent_autocselection read fOldAutoCSelection write SetOnAutoCSelection;
end;


implementation
{ TSciController }
uses SciLexerMemo;

procedure TSciController.Connect;
begin
  if FConnected=False then
  begin
    if assigned(fControl.OnCharAdded) then
      fOldCharAdded := fControl.OnCharAdded
    else
      fOldCharAdded := nil;
    fControl.OnCharAdded := EditorCharAdded;
    if assigned(fControl.OnCallTipClick) then
      fOldCallTipClick := fControl.OnCallTipClick
    else
      fOldCallTipClick := nil;
    fControl.OnCallTipClick := EditorCallTipClick;
    if assigned(fControl.OnMacroRecord) then
      fOldMacroRecord := fControl.OnMacroRecord
    else
      fOldMacroRecord:=nil;
    fControl.OnMacroRecord := EditorMacroRecord;
    if assigned(fControl.OnAutoCSelection) then
      fOldAutoCSelection := fControl.OnAutoCSelection
    else
      fOldAutoCSelection:=nil;
    fControl.OnAutoCSelection := EditorAutoCSelection;
    FConnected:=True;
  end;
end;
constructor TSciController.Create (aControl : TScintillaBase);
begin
  FConnected:=False;
  fControl := TControllerControl (aControl);
  fInCharAdded := false;
  fCharAddedChain := TList.Create;
  fCallTipClickChain := TList.Create;
  fMacroRecordChain := TList.Create;
  fAutoCSelectionChain := TList.Create;
end;

destructor TSciController.Destroy;
begin
  fControl.OnCharAdded  := fOldCharAdded;
  fCharAddedChain.Free;
  fControl.OnCallTipClick  := fOldCallTipClick;
  fCallTipClickChain.Free;
  fControl.OnMacroRecord  := fOldMacroRecord;
  fMacroRecordChain.Free;
  fControl.OnAutoCSelection := fOldAutoCSelection;
  fAutoCSelectionChain.Free;
  inherited Destroy;
end;

procedure TSciController.SetOnMacroRecord (const Value: TSCEvent_macrorecord);
begin
  fOldMacroRecord := Value;
  fControl.OnMacroRecord := EditorMacroRecord;
end;

procedure TSciController.EditorMacroRecord (Sender : TObject; const message : LongInt; const wParam : LongInt; const lParam : LongInt);
var
  idx : Integer;
begin
  with fMacroRecordChain do
  begin
    for idx := Count - 1 downto 0 do
    begin
      with TMacroRecordProc (Items[idx]) do
        if Assigned (OnMacroRecord) then 
        begin
          OnMacroRecord (Sender,message,wParam,lParam);
       end;
    end;
  end;
  if Assigned (fOldMacroRecord) then
    fOldMacroRecord (Sender,message,wParam,lParam);
end;
procedure TSciController.EditorAutoCSelection (Sender : TObject; text : PChar);
var
  idx : Integer;
begin
  if not (Sender is TScintillaMemo) then Exit;
  with fAutoCSelectionChain do
  begin
    for idx := Count - 1 downto 0 do
    begin
      with TAutoCSelectionProc (Items[idx]) do
        if Assigned (OnAutoCSelection) then
        begin
          OnAutoCSelection (Sender,text);
       end;
    end;
  end;
  if Assigned (fOldAutoCSelection) then
    fOldAutoCSelection (Sender,text);
end;

procedure TSciController.ExecuteMacroRecord (Sender: TObject;const message : LongInt; const wParam : LongInt; const lParam : LongInt);
begin
  EditorMacroRecord (Sender,message,wParam,lParam);
end;

procedure TSciController.AddMacroRecordHandler(aHandler: TMacroRecordProc);
begin
  fMacroRecordChain.Add(aHandler);
end;

procedure TSciController.RemoveMacroRecordHandler(aHandler: TMacroRecordProc);
begin
  fMacroRecordChain.Remove(aHandler);
end;

procedure TSciController.AddAutoCSelectionHandler(aHandler: TAutoCSelectionProc);
begin
  fAutoCSelectionChain.Add(aHandler);
end;

procedure TSciController.RemoveAutoCSelectionHandler(aHandler: TAutoCSelectionProc);
begin
  fAutoCSelectionChain.Remove(aHandler);
end;

procedure TSciController.ExecuteAutoCSelection (Sender: TObject;text : PChar);
begin
  EditorAutoCSelection(Sender,text);
end;


procedure TSciController.SetOnCharAdded (const Value: TSCEvent_charadded);
begin
  fOldCharAdded := Value;
  fControl.OnCharAdded := EditorCharAdded;
end;

procedure TSciController.SetOnAutoCSelection (const Value: TSCEvent_autocselection);
begin
  fOldAutoCSelection := Value;
  fControl.OnAutoCSelection := EditorAutoCSelection;
end;

procedure TSciController.SetOnCallTipClick(const Value: TSCEvent_calltipclick);
begin
  fOldCallTipClick := Value;
  fControl.OnCallTipClick := EditorCallTipClick;
end;


procedure TSciController.EditorCharAdded (Sender: TObject; const ch : LongInt);
var
  idx : Integer;
begin

  if fInCharAdded then
    exit;
  fInCharAdded := true;
  try
    with fCharAddedChain do begin
      for idx := Count - 1 downto 0 do begin
        with TCharAddedProc (Items[idx]) do
          if Assigned (OnCharAdded) then begin
            OnCharAdded (Sender,ch);
            if (ch = 0) then begin
              fInCharAdded := false;
              exit;
            end;
         end;
      end;
    end;
    if Assigned (fOldCharAdded) then
      fOldCharAdded (Sender,ch);
  finally
    fInCharAdded := false;
  end;
end;

procedure TSciController.EditorCallTipClick (Sender: TObject; const position : LongInt);
var
  idx : Integer;
begin
  with fCallTipClickChain do
  begin
    for idx := Count - 1 downto 0 do begin
      with TCallTipClickProc (Items[idx]) do
        if Assigned (OnCallTipClick) then
        begin
          OnCallTipClick(Sender,position);
        end;
    end;
  end;
  if Assigned (fOldCallTipClick) then
    fOldCallTipClick(Sender,position);
end;


procedure TSciController.ExecuteCharAdded (Sender: TObject; const ch: Integer);
begin
  EditorCharAdded (Sender,ch);
end;

procedure TSciController.ExecuteCallTipClick (Sender: TObject; const position: Integer);
begin
  EditorCallTipClick (Sender,position);
end;

procedure TSciController.AddCharAddedHandler(aHandler: TCharAddedProc);
begin
  fCharAddedChain.Add(aHandler);
end;

procedure TSciController.RemoveCharAddedHandler(aHandler: TCharAddedProc);
begin
  fCharAddedChain.Remove(aHandler);
end;

procedure TSciController.AddCallTipClickHandler(aHandler: TCallTipClickProc);
begin
  fCallTipClickChain.Add(aHandler);
end;

procedure TSciController.RemoveCallTipClickHandler(aHandler: TCallTipClickProc);
begin
  fCallTipClickChain.Remove(aHandler);
end;

constructor TCharAddedProc.Create (aCharAddedProc : TSCEvent_charadded);
begin
  inherited Create;
  fCharAddedProc := aCharAddedProc;
end;

constructor TCallTipClickProc.Create (aCallTipClickProc : TSCEvent_calltipclick);
begin
  inherited Create;
  fCallTipClickProc := aCallTipClickProc;
end;

constructor TMacroRecordProc.Create (aMacroRecordProc : TSCEvent_macrorecord);
begin
  inherited Create;
  fMacroRecordProc := aMacroRecordProc;
end;
constructor TAutoCSelectionProc.Create (aAutoCSelectionProc : TSCEvent_autocselection);
begin
  inherited Create;
  fAutoCSelectionProc := aAutoCSelectionProc;
end;

end.
