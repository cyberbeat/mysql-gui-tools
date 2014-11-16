unit MyxDbUtils;

interface

uses TntStdCtrls, Grt;

procedure UpdateCollations(ComboBox: TTntComboBox);

procedure SetCollations(ComboBox: TTntComboBox;
  Collation: WideString; FailBackCollation: WideString = '');

procedure SetCharsetCollation(Collation: WideString; Obj: Pointer;
   CharsetMember: WideString; CollationMember: WideString);

implementation

// -----------------------------------------------------------------------------

function Grt: TGrt;

begin
  Result := RuntimeEnvironment;
end;

// -----------------------------------------------------------------------------

procedure UpdateCollations(ComboBox: TTntComboBox);

var
  I, J: Integer;
  CharsetList, Charset, CollationList: Pointer;
  ItemText, OldSelVal: WideString;

begin
  if (ComboBox = nil) then
    Exit;

  OldSelVal := ComboBox.Text;
  with ComboBox do
  begin
    Items.Clear;

    // Add favourites
    Items.Add('latin1_swedish_ci');
    Items.Add('latin1_german_ci');
    Items.Add('utf8_general_ci');
    Items.Add('_____________________');
    Items.Add('');    

    CharsetList := Grt.Global['/workbench/catalog/characterSets'];
    for I := 0 to Grt.ListCount(CharsetList) - 1 do
    begin
      Charset := Grt.ListRefItem[CharsetList, I];

      CollationList := Grt.DictItem[Charset, 'collations'];
      for J := 0 to Grt.ListCount(CollationList) - 1 do
      begin
        ItemText := Grt.ListString[CollationList, J];

        Items.Add(ItemText);

        if (ItemText = OldSelVal) and
          (ItemIndex = -1) then
          ItemIndex :=
            Items.Count - 1;
      end;
    end;

    if (ItemIndex = -1) then
    begin
      I := Items.IndexOf('latin1_swedish_ci');

      if (I >= 0) then
        ItemIndex := I
      else
        ItemIndex := 0;
    end;
  end;
end;

// -----------------------------------------------------------------------------

procedure SetCollations(ComboBox: TTntComboBox;
  Collation: WideString; FailBackCollation: WideString);

var
  I: Integer;

begin
  // character set / collation
  with ComboBox do
  begin
    I := Items.IndexOf(Collation);

    if (I < 0) then
    begin
      if (FailBackCollation <> '') then
        I := Items.IndexOf(FailBackCollation);
    end;

    if (I >= 0) then
      ItemIndex := I
    else
      ItemIndex := Items.IndexOf('');
  end;
end;

// -----------------------------------------------------------------------------

procedure SetCharsetCollation(Collation: WideString; Obj: Pointer;
   CharsetMember: WideString; CollationMember: WideString);

var
  P: Integer;

begin
  P := Pos('_', Collation);

  if (P > 0) then
    Grt.DictString[Obj, CharsetMember] := Copy(Collation, 1, P - 1)
  else
    Grt.DictString[Obj, CharsetMember] := Collation;

  Grt.DictString[Obj, CollationMember] := Collation;
end;

// -----------------------------------------------------------------------------

end.
