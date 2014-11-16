//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.GetItem(Index: Integer): T%structname%;

begin
  Result := T%structname%(inherited GetItem(Index));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure T%structname%_List.SetItem(Index: Integer; AObject: T%structname%);

begin
  inherited SetItem(Index, AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.Add(AObject: T%structname%): Integer;

begin
  Result := inherited Add(AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.Extract(Item: T%structname%): T%structname%;

begin
  Result := T%structname%(inherited Extract(Item));
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.Remove(AObject: T%structname%): Integer;

begin
  Result := inherited Remove(AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.IndexOf(AObject: T%structname%): Integer;

begin
  Result := inherited IndexOf(AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure T%structname%_List.Insert(Index: Integer; AObject: T%structname%);

begin
  inherited Insert(Index, AObject);
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.First: T%structname%;

begin
  Result := T%structname%(inherited First);
end;

//----------------------------------------------------------------------------------------------------------------------

function T%structname%_List.Last: T%structname%;

begin
  Result := T%structname%(inherited Last);
end;

//----------------------------------------------------------------------------------------------------------------------

