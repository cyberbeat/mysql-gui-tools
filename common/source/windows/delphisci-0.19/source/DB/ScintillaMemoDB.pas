//CE_Desc_Include(..\helpdescriptions.txt)
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
	Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
	02111-1307 USA
}
{
  Author : hdalis
	Created: 29/11/2004, 12:41:43
  History: 29/11/2004 Initial Release
			$Id:  $
}
{$Include SciCommonDef.Inc}
unit ScintillaMemoDB;
interface
uses
  SysUtils, Classes, Controls, SciLexer, SciLexerMemo,DB,DBCtrls,Messages;

type
  TScintillaMemoDB = class(TScintillaMemo)
  private
    FDataLink: TFieldDataLink;
    FBeginEdit: boolean;
    FLoadData: TNotifyEvent;
    FFocused : Boolean;

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure UpdateData(Sender: TObject);
    procedure SetFocused(Value: Boolean);
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    function GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure doSciModified(const position : LongInt; const modificationType : LongInt;
														text : PChar; const length : LongInt; const linesAdded : LongInt; const line : LongInt;
														const foldLevelNow : LongInt; const foldLevelPrev : LongInt);override;
  public
    procedure LoadMemo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TField read GetField;

  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property OnLoadData: TNotifyEvent read fLoadData write fLoadData;
  end;

implementation
uses SciSupport;

constructor TScintillaMemoDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TScintillaMemoDB.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TScintillaMemoDB.CMEnter(var Msg: TCMEnter);
begin
  SetFocused(True);
  inherited;
end;

procedure TScintillaMemoDB.CMExit(var Msg: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;
procedure TScintillaMemoDB.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TScintillaMemoDB.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FBeginEdit then
    begin
      FBeginEdit := False;
      Exit;
    end;
    if FDataLink.Field.IsBlob then
      LoadMemo
    else
      Text := FDataLink.Field.Text;
    if Assigned(FLoadData) then
      FLoadData(Self);
  end else
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

procedure TScintillaMemoDB.EditingChange(Sender: TObject);
begin
  if FDataLink.Editing then
  begin
    if Assigned(FDataLink.DataSource)
      and (FDataLink.DataSource.State <> dsInsert)
    then
      FBeginEdit := True;
  end;
end;

function TScintillaMemoDB.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TScintillaMemoDB.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TScintillaMemoDB.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TScintillaMemoDB.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TScintillaMemoDB.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key=27 then
  begin
    Inherited;
    FDataLink.Reset;
  end else
  Inherited;
end;

procedure TScintillaMemoDB.doSciModified(const position : LongInt; const modificationType : LongInt;
														text : PChar; const length : LongInt; const linesAdded : LongInt; const line : LongInt;
														const foldLevelNow : LongInt; const foldLevelPrev : LongInt);
begin
  if Boolean(modificationType or SC_MOD_INSERTTEXT) or
     Boolean(modificationType or SC_MOD_DELETETEXT) or
     Boolean(modificationType or SC_PERFORMED_UNDO) or
     Boolean(modificationType or SC_PERFORMED_REDO) then
    FDataLink.Modified;
end;

procedure TScintillaMemoDB.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TScintillaMemoDB.LoadMemo;
var
  BlobStream: TStream;
begin
  try
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    Lines.BeginUpdate;
    Lines.LoadFromStream(BlobStream);
    Lines.EndUpdate;
    BlobStream.Free;
    Modified := false;
    EmptyUndoBuffer;
  except
    on E: EInvalidOperation do
      Lines.Text := Format('(%s)', [E.Message]);
  end;
  EditingChange(Self);
end;

procedure TScintillaMemoDB.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource)
  then
    DataSource := nil;
end;

procedure TScintillaMemoDB.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TScintillaMemoDB.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TScintillaMemoDB.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TScintillaMemoDB.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TScintillaMemoDB.UpdateData(Sender: TObject);
var
  BlobStream: TStream;
begin
  if FDataLink.Field.IsBlob then
  begin
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    Lines.SaveToStream(BlobStream);
    BlobStream.Free;
  end else
    FDataLink.Field.AsString := Lines.Text;
end;
end.
