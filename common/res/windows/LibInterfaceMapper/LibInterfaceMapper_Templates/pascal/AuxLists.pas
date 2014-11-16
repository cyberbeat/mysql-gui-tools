unit AuxLists;

interface

uses Classes;

type
  TIntegerList = class(TObject)
  private
    list: TList;

    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; value: Integer);
    function GetListCount(): Integer;
  public
    constructor Create();
    destructor Destroy(); override;

    property Count: Integer read getListCount;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
    function Add(Item: Integer): Integer;

    procedure Delete(Index: Integer);
    procedure Clear;
    function First: Integer;
    function Last: Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Exchange(Index1, Index2: Integer);
  end;

  TBooleanList = class(TObject)
  private
    list: TList;

    function GetItem(Index: Integer): Boolean;
    procedure SetItem(Index: Integer; value: Boolean);
    function getListCount(): Integer;
  public
    constructor Create();
    destructor Destroy; override;

    property Count: Integer read getListCount;
    property Items[Index: Integer]: Boolean read GetItem write SetItem; default;
    function Add(Item: Boolean): Integer;

    procedure Delete(Index: Integer);
    procedure Clear;
    function First: Boolean;
    function Last: Boolean;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Exchange(Index1, Index2: Integer);
  end;

implementation

uses
  RTLConsts;

//----------------------------------------------------------------------------------------------------------------------

constructor TIntegerList.Create();

begin
  list:= TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TIntegerList.Destroy();

var
  I: Integer;

begin
  for I := 0 to list.Count - 1 do
    Dispose(list[i]);
  list.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIntegerList.getListCount(): Integer;

begin
  Result := list.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIntegerList.GetItem(Index: Integer) : Integer;

begin
  Result := PInteger(list[Index])^;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.SetItem(Index: Integer; value: Integer);

var
  T: ^Integer;

begin
  if (Index < 0) or (Index >= list.Count) then
    list.Error(@SListIndexError, Index);

  if list[index] <> nil then
    Dispose(list[index]);

  new(T);
  T^ := value;
  list[index] := T;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIntegerList.Add(item: Integer) : Integer;

var
  T: PInteger;

begin
  new(T);
  T^ := item;
  Result := list.add(T);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.Delete(Index: Integer);

var
  T: PInteger;

begin
  if (Index < 0) or (Index >= list.Count) then
    list.Error(@SListIndexError, Index);

  T := list[index];

  list.Delete(index);
  Dispose(T);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.Clear;

var
  I: Integer;

begin
  // free the memory
  for I := 0 to list.Count - 1 do
  begin
    Dispose(list[I]);
  end;

  list.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIntegerList.First: Integer;

begin
  Result := PInteger(list.First)^;
end;

//----------------------------------------------------------------------------------------------------------------------

function TIntegerList.Last: Integer;

begin
  Result := PInteger(list.Last)^;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.Pack;

begin
  list.Pack;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.Sort(Compare: TListSortCompare);

begin
  list.Sort(compare);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIntegerList.Exchange(Index1, Index2: Integer);

begin
  list.Exchange(index1,index2);
end;

//----------------- TBooleanList ---------------------------------------------------------------------------------------

constructor TBooleanList.Create();

begin
  list:= TList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TBooleanList.Destroy;

var
  I: Integer;

begin
  for I := 0 to list.Count - 1 do
    Dispose(list[I]);
  list.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBooleanList.getListCount(): Integer;

begin
  Result := list.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBooleanList.GetItem(Index: Integer) : Boolean;

begin
  Result := PBoolean(list[Index])^;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.SetItem(Index: Integer; value: Boolean);

var
  T: ^Boolean;
  
begin
  if (Index < 0) or (Index >= list.Count) then
    list.Error(@SListIndexError, Index);

  if list[index] <> nil then
    Dispose(list[index]);

  new(T);
  T^ := value;
  list[Index] := T;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBooleanList.Add(Item: Boolean) : Integer;

var
  T: PBoolean;

begin
  new(T);
  T^ := item;
  Result := list.add(T);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.Delete(Index: Integer);

var
  T: PBoolean;

begin
  if (Index < 0) or (Index >= list.Count) then
    list.Error(@SListIndexError, Index);

  T := list[index];

  list.Delete(Index);
  Dispose(T);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.Clear;

var
  I: Integer;

begin
  // free the memory
  for I := 0 to list.Count - 1 do
  begin
    Dispose(list[I]);
  end;

  list.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBooleanList.First: Boolean;

begin
  Result := PBoolean(list.First)^;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBooleanList.Last: Boolean;

begin
  Result := PBoolean(list.Last)^;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.Pack;

begin
  list.Pack;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.Sort(Compare: TListSortCompare);

begin
  list.Sort(Compare);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBooleanList.Exchange(Index1, Index2: Integer);

begin
  list.Exchange(Index1, Index2);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
