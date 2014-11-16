unit XGrtForms;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  gnugettext, Windows, Messages, SysUtils, Forms, Controls,
  Classes, IniFiles, TntSysUtils, TntClasses, StdCtrls, TntStdCtrls,
  ExtCtrls, TntExtCtrls, Buttons, TntButtons, ComCtrls, TntComCtrls,
  Menus, TntMenus,
  TypInfo, Variants, AuxFuncs, myx_grt_public_interface;

type
  TXGrtFormApplication = class
    constructor Create(Grt: Pointer; PModule: PMYX_GRT_MODULE);
    destructor Destroy; override;

    procedure BuildPathCacheLists;

    function CreateProxyRoot(argument: Pointer): Pointer;

    // helper functions for ValueGet
    function ValueGetApplication(Path: WideString): Pointer;
    function ValueGetForm(Path: WideString;
      SubPath: WideString; Form: TForm): Pointer;
    function ValueGetAnchors(Path: WideString;
      SubPath: WideString; Form: TForm): Pointer;
    function ValueGetConstraints(Path: WideString;
      SubPath: WideString; Form: TForm): Pointer;
    function ValueGetTypedWidgetLists(
      Path: WideString; SubPath: WideString; Form: TForm): Pointer;

    function ValueGet(argument: Pointer): Pointer;
    function ValueSet(argument: Pointer): Pointer;
    function ListSize(argument: Pointer): Pointer;
    function ListItemGet(argument: Pointer): Pointer;
    function ListItemAdd(argument: Pointer): Pointer;
    function ListItemRemove(argument: Pointer): Pointer;
    function DictItemGet(argument: Pointer): Pointer;
    function DictItemSet(argument: Pointer): Pointer;
    function DictItemCount(argument: Pointer): Pointer;
    function DictItemKey(argument: Pointer): Pointer;

    function GetPathArgument(argument: Pointer): WideString;
    function GetPathValue(Path: string; NewValue: array of const): Pointer;
    function GetObjectGUID(Obj: TObject): string;
    function GetWidgetProp(Obj: TObject; ProperyName: WideString): Variant;
  private
    { Private declarations }
    FGrt: Pointer;
    FPModule: PMYX_GRT_MODULE;
    FProxyRootPath: WideString;
    FProxyRootPathLen: Integer;
    FObjectGUIDList: TStringList;

    FPathValues: THashedStringList;
    FPathApplicationCache,
    FPathFormCache,
    FPathAnchorsCache,
    FPathConstraintsCache: TTntStringList;

    FPathTypedFormControlsList: TTntStringList;
    FPathFormCache_TypedFormControlsListStart: Integer;
  end;

  TXGrtFormWidgetType = class
    constructor Create(GrtClassName: WideString; Properties: WideString;
      NativeClasses: array of const);
    destructor Destroy; override;
  private
    FGrtClassName: WideString;
    FPropertyList: TTntStringList;
    FNativeClassList: TList;
  public
    property GrtClassName: WideString read FGrtClassName;
    property PropertyList: TTntStringList read FPropertyList;
    property NativeClassList: TList read FNativeClassList;
  end;

implementation

// -----------------------------------------------------------------------------

constructor TXGrtFormApplication.Create(Grt: Pointer; PModule: PMYX_GRT_MODULE);

begin
  FGrt := Grt;
  FPModule := PModule;
  FProxyRootPath := '';
  FProxyRootPathLen := 0;

  FObjectGUIDList := TStringList.Create;
  FPathValues := THashedStringList.Create;

  BuildPathCacheLists;
end;

// -----------------------------------------------------------------------------

destructor TXGrtFormApplication.Destroy;

var
  I: Integer;

begin
  FPathValues.Free;

  for I := 0 to FPathTypedFormControlsList.Count - 1 do
    FPathTypedFormControlsList.Objects[I].Free;
  FPathTypedFormControlsList.Free;

  FPathApplicationCache.Free;
  FPathFormCache.Free;
  FPathAnchorsCache.Free;

  FObjectGUIDList.Free;
end;

// -----------------------------------------------------------------------------

procedure TXGrtFormApplication.BuildPathCacheLists;

var
  PathObjectCache,
    PathControlCache: WideString;

begin
  // Build Path caches
  PathObjectCache :=
    '/name'#13#10 +
    '/_id'#13#10 +
    '/parent'#13#10;

  FPathApplicationCache := TTntStringList.Create;
  FPathApplicationCache.Text :=
    PathObjectCache +
    '/active'#13#10 +
    '/mouseX'#13#10 +
    '/mouseY'#13#10 +
    '/forms'#13#10 +
    '/mainForm'#13#10 +
    '/activeForm'#13#10;

  PathControlCache :=
    '/left'#13#10 +
    '/top'#13#10 +
    '/width'#13#10 +
    '/height'#13#10 +
    '/autosize'#13#10 +
    '/caption'#13#10 +
    '/color'#13#10 +
    '/enabled'#13#10 +
    '/visible'#13#10 +
    '/anchors'#13#10 +
    '/constraints'#13#10 +
    '/parent'#13#10 +
    '/controls'#13#10 +
    '/eventOnClick'#13#10 +
    '/eventOnDblClick'#13#10 +
    '/eventOnResize'#13#10 +
    '/eventOnMouseDown'#13#10 +
    '/eventOnMouseMove'#13#10 +
    '/eventOnMouseUp'#13#10 +
    '/eventOnMouseIn'#13#10 +
    '/eventOnMouseOut'#13#10 +
    '/eventOnKeyDown'#13#10 +
    '/eventOnKeyUp'#13#10;

  FPathFormCache := TTntStringList.Create;
  FPathFormCache.Text :=
    PathObjectCache +
    PathControlCache +
    '/active'#13#10 +
    '/activeControl'#13#10 +
    '/alphaBlend'#13#10 +
    '/alphaBlendValue'#13#10 +
    '/windowState'#13#10 +
    '/mainMenu'#13#10 +
    '/eventOnCreate'#13#10 +
    '/eventOnDestroy'#13#10 +
    '/eventOnActivate'#13#10 +
    '/eventOnDeactivate'#13#10 +
    '/eventOnClose'#13#10 +
    '/menuitems'#13#10 +
    '/panels'#13#10 +
    '/buttons'#13#10 +
    '/edits'#13#10 +
    '/checkboxes'#13#10 +
    '/radiobuttons'#13#10 +
    '/listboxes'#13#10 +
    '/dropdowns'#13#10 +
    '/pageControls'#13#10;

  FPathFormCache_TypedFormControlsListStart := 37;

  FPathAnchorsCache := TTntStringList.Create;
  FPathAnchorsCache.Text :=
    PathObjectCache +
    '/left'#13#10 +
    '/top'#13#10 +
    '/right'#13#10 +
    '/bottom'#13#10;

  FPathConstraintsCache := TTntStringList.Create;
  FPathConstraintsCache.Text :=
    PathObjectCache +
    '/minWidth'#13#10 +
    '/maxWidth'#13#10 +
    '/minHeight'#13#10 +
    '/maxHeight'#13#10;

  // Instead of having individual lists for all typed dicts
  // for the form's controls, they are keept in a seperate
  // stringlist
  FPathTypedFormControlsList := TTntStringList.Create;

  // Add menuitems
  FPathTypedFormControlsList.AddObject('/menuitems',
    TXGrtFormWidgetType.Create('forms.Menuitem',
    PathObjectCache +
    PathControlCache +
    '/checked'#13#10 +
    '/icon'#13#10,
    [TTntMenuItem, TMenuItem]));

  // Add panels
  FPathTypedFormControlsList.AddObject('/panels',
    TXGrtFormWidgetType.Create('forms.Panel',
    PathObjectCache +
    PathControlCache +
    '/border'#13#10,
    [TTntPanel, TPanel]));

  // Add buttons
  FPathTypedFormControlsList.AddObject('/buttons',
    TXGrtFormWidgetType.Create(
    'forms.Button',
    PathObjectCache +
    PathControlCache +
    '/isDefault'#13#10 +
    '/icon'#13#10,
    [TButton, TSpeedButton, TTntButton, TTntSpeedButton]));

  // Add edits
  FPathTypedFormControlsList.AddObject('/edits',
    TXGrtFormWidgetType.Create('forms.Edit',
    PathObjectCache +
    PathControlCache +
    '/border'#13#10 +
    '/text'#13#10 +
    '/readOnly'#13#10 +
    '/passwordChar'#13#10,
    [TTntEdit, TEdit]));

  // Add checkboxes
  FPathTypedFormControlsList.AddObject('/checkboxes',
    TXGrtFormWidgetType.Create('forms.Checkbox',
    PathObjectCache +
    PathControlCache +
    '/checked'#13#10,
    [TTntCheckBox, TCheckBox]));

  // Add radiobuttons
  FPathTypedFormControlsList.AddObject('/radiobuttons',
    TXGrtFormWidgetType.Create('forms.Radiobutton',
    PathObjectCache +
    PathControlCache +
    '/group'#13#10 +
    '/checked'#13#10,
    [TTntRadioButton, TRadioButton]));

  // Add listboxes
  FPathTypedFormControlsList.AddObject('/listboxes',
    TXGrtFormWidgetType.Create('forms.Listbox',
    PathObjectCache +
    PathControlCache +
    '/multiselect'#13#10 +
    '/lastSelectedItem'#13#10 +
    '/items'#13#10 +
    '/selectedItems'#13#10,
    [TTntListBox, TListBox]));

  // Add dropdowns
  FPathTypedFormControlsList.AddObject('/dropdowns',
    TXGrtFormWidgetType.Create('forms.Dropdown',
    PathObjectCache +
    PathControlCache +
    '/fixedList'#13#10 +
    '/text'#13#10 +
    '/selectedItem'#13#10 +
    '/items'#13#10,
    [TTntComboBox, TComboBox]));

  // Add pagecontrols
  FPathTypedFormControlsList.AddObject('/pagecontrols',
    TXGrtFormWidgetType.Create('forms.Pagecontrol',
    PathObjectCache +
    PathControlCache +
    '/activePage'#13#10 +
    '/pages'#13#10,
    [TTntPageControl, TPageControl]));
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetPathArgument(argument: Pointer): WideString;

var
  ValueType: MYX_GRT_VALUE_TYPE;
  Value: Pointer;

begin
  Result := '';

  ValueType := myx_grt_value_get_type(argument);

  if (ValueType = MYX_STRING_VALUE) then
    Result := myx_grt_value_as_string(argument)
  else
    if (ValueType = MYX_LIST_VALUE) then
    begin
      Value := myx_grt_list_item_get(argument, 0);
      ValueType := myx_grt_value_get_type(Value);

      if (ValueType = MYX_STRING_VALUE) then
        Result := myx_grt_value_as_string(Value)
    end;

  if (ValueType <> MYX_STRING_VALUE) then
    raise Exception.Create('A wrong argument was passed to the function.');
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetPathValue(Path: string; NewValue: array of const): Pointer;

var
  Index: Integer;
  Value: Pointer;

begin
  // check if the submitted path has already a cached value
  Index := FPathValues.IndexOf(Path);

  if (Index = -1) then
  begin
    // if not, create a new value and cache it
    // then return it
    with NewValue[0] do
      case VType of
        vtInteger:
          Value := myx_grt_value_from_int(VInteger);
        vtExtended:
          Value := myx_grt_value_from_real(VExtended^);
        vtWideString:
          Value := myx_grt_value_from_string(WideString(VWideString));
        vtString:
          Value := myx_grt_value_from_string(VString^);
        vtAnsiString:
          Value := myx_grt_value_from_string(string(VAnsiString));
        vtPChar:
          Value := myx_grt_value_from_string(VPChar);
        vtPointer:
          Value := VPointer;
        vtVariant:
          begin
            case VarType(VVariant^) of
              varInteger, varSmallint, varShortInt, varByte,
                varLongWord, varInt64, varWord:
                Value := myx_grt_value_from_int(VVariant^);
              varSingle, varDouble:
                Value := myx_grt_value_from_real(VVariant^);
              varString:
                Value := myx_grt_value_from_string(VVariant^);
              varNull:
                Value := nil;
            else
              Value := nil;
            end;
          end;
        varNull:
          Value := nil;
      else
        raise EInOutError.Create(_('GetPathValue called with unsupported parameter type.'));
      end;

    FPathValues.AddObject(Path, Value);
  end
  else
  begin
    // if there has already been created a value for the path,
    // modify the value and return it
    Value := FPathValues.Objects[Index];

    with NewValue[0] do
      case VType of
        vtInteger:
          Value := myx_grt_value_change_int(Value, VInteger);
        vtExtended:
          Value := myx_grt_value_change_real(Value, VExtended^);
        vtWideString:
          Value := myx_grt_value_change_string(Value, WideString(VWideString));
        vtString:
          Value := myx_grt_value_change_string(Value, VString^);
        vtAnsiString:
          Value := myx_grt_value_change_string(Value, string(VAnsiString));
        vtPChar:
          Value := myx_grt_value_change_string(Value, VPChar);
        vtPointer:
          begin
            // Release old value
            myx_grt_value_release(Value);

            Value := VPointer;
          end;
        vtVariant:
          begin
            case VarType(VVariant^) of
              varInteger, varSmallint, varShortInt, varByte,
                varLongWord, varInt64, varWord:
                Value := myx_grt_value_change_int(Value, VVariant^);
              varSingle, varDouble:
                Value := myx_grt_value_change_real(Value, VVariant^);
              varString:
                Value := myx_grt_value_change_string(Value, VVariant^);
              varNull:
                Value := nil;
            end;
          end;
      else
        raise EInOutError.Create(_('GetPathValue called with unsupported parameter type.'));
      end;
  end;

  Result := Value;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetObjectGUID(Obj: TObject): string;

var
  Index: Integer;
  Guid: TGUID;

begin
  // check if the given object already has a GUID assigned
  Index := FObjectGUIDList.IndexOfObject(Obj);

  // if not, create a GUI and assign it with the object
  if (Index = -1) then
  begin
    CreateGUID(Guid);
    Index := FObjectGUIDList.AddObject(GUIDToString(Guid), Obj);
  end;

  // return the GUID
  Result := FObjectGUIDList[Index];
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetWidgetProp(Obj: TObject; ProperyName: WideString): Variant;

var
  PropInfo: PPropInfo;

begin
  Result := Null;

  // check if the requested property exists for the given object
  PropInfo := GetPropInfo(Obj.ClassInfo, ProperyName);

  if (Assigned(PropInfo)) then
    Result := GetPropValue(Obj, ProperyName);
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.CreateProxyRoot(argument: Pointer): Pointer;

var
  Path: WideString;

begin
  {if (FProxyRootPath <> '') then
    raise Exception.Create(Format(
      _('A Forms application has already been created at %s.'),
      [FProxyRootPath]));

  Path := GetPathArgument(argument);

  Result := myx_grt_dict_new_proxy(FGrt, FPModule,
    '', 'forms.Application');

  if (myx_grt_dict_item_set_by_path(
    myx_grt_get_root(FGrt), Path, Result) <> 0) then
  begin
    raise Exception.Create(Format(
      _('The object cannot be created at the given path %s.'),
      [Path]));
  end;

  FProxyRootPath := Path;
  FProxyRootPathLen := Length(Path);}
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGetApplication(Path: WideString): Pointer;

var
  CacheIndex: Integer;

begin
  CacheIndex := FPathApplicationCache.IndexOf(Path);

  case CacheIndex of
    0:
      Result := GetPathValue(Path, [Application.Title]);
    1:
      Result := GetPathValue(Path, [GetObjectGUID(self)]);
    2:
      Result := nil;
    3:
      Result := GetPathValue(Path, [Ord(Application.Active)]);
    4:
      Result := GetPathValue(Path, [Mouse.CursorPos.X]);
    5:
      Result := GetPathValue(Path, [Mouse.CursorPos.Y]);
    6:
      begin
        {CacheIndex := FPathValues.IndexOf(Path);
        if (CacheIndex > -1) then
          Result := FPathValues.Objects[CacheIndex]
        else
          Result := GetPathValue(Path, [myx_grt_list_new_proxy(FGrt, FPModule, Path, MYX_DICT_VALUE, 'forms.Form')]);}
      end;
    7:
      Result := GetPathValue(Path, [GetObjectGUID(Application.MainForm)]);
    8:
      Result := GetPathValue(Path, [GetObjectGUID(Screen.ActiveForm)]);
  else
    Result := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGetForm(Path: WideString; SubPath: WideString; Form: TForm): Pointer;

var
  CacheIndex: Integer;
  WidgetType: TXGrtFormWidgetType;

begin
  CacheIndex := FPathFormCache.IndexOf(SubPath);

  // if this is not a widget list (that are at the end of the
  // property list) deal with the member variable manually
  if (CacheIndex < FPathFormCache_TypedFormControlsListStart) then
  begin
    case CacheIndex of
      0: Result := GetPathValue(Path, [Form.Name]);
      1: Result := GetPathValue(Path, [GetObjectGUID(Form)]);
      2: Result := nil;
      3: Result := GetPathValue(Path, [Form.Left]);
      4: Result := GetPathValue(Path, [Form.Top]);
      5: Result := GetPathValue(Path, [Form.Width]);
      6: Result := GetPathValue(Path, [Form.Height]);
      7: Result := GetPathValue(Path, [0]);
      8: Result := GetPathValue(Path, [Form.Caption]);
      9: Result := GetPathValue(Path, [ColorToHtmlColor(Form.Color)]);
      10: Result := GetPathValue(Path, [Ord(Form.Enabled)]);
      11: Result := GetPathValue(Path, [Ord(Form.Visible)]);
      {12: Result := GetPathValue(Path,
          [myx_grt_dict_new_proxy(FGrt, FPModule, Path, 'forms.Anchors')]);
      13: Result := GetPathValue(Path,
          [myx_grt_dict_new_proxy(FGrt, FPModule, Path, 'forms.Constraints')]);}
      14: Result := nil;
      15: Result := nil;
      16: Result := nil;
      17: Result := nil;
      18: Result := nil;
      19: Result := nil;
      20: Result := nil;
      21: Result := nil;
      22: Result := nil;
      23: Result := nil;
      24: Result := nil;
      25: Result := nil;
      26: Result := GetPathValue(Path, [Ord(Form.Active)]);
      27: Result := nil;
      28: Result := GetPathValue(Path, [Ord(Form.AlphaBlend)]);
      29: Result := GetPathValue(Path, [Form.AlphaBlendValue]);
      30: Result := GetPathValue(Path, [Ord(Form.WindowState)]);
      {31:
        begin
          if Assigned(Form.Menu) then
            Result := GetPathValue(Path, [myx_grt_list_new_proxy(FGrt, FPModule, Path, MYX_DICT_VALUE,
              'forms.Menuitem')])
          else
            Result := nil;
        end;}
      32: Result := nil;
      33: Result := nil;
      34: Result := nil;
      35: Result := nil;
      36: Result := nil;
    else
      Result := nil;
    end
  end
  else
  begin
    // If there is a number higher than 36 it is a typed dict holding the components
    {Dec(CacheIndex, FPathFormCache_TypedFormControlsListStart);
    if (CacheIndex >= FPathTypedFormControlsList.Count) then
      raise Exception.Create(Format(_('Components of type %s are not yet handled by the proxy.'), [SubPath]));

    WidgetType := TXGrtFormWidgetType(FPathTypedFormControlsList.Objects[CacheIndex]);
    Result := GetPathValue(Path, [myx_grt_dict_new_typed_proxy(FGrt, FPModule, Path, MYX_DICT_VALUE,
      WidgetType.GrtClassName)]);}
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGetAnchors(Path: WideString; SubPath: WideString; Form: TForm): Pointer;

begin
  case FPathAnchorsCache.IndexOf(SubPath) of
    0:
      Result := GetPathValue(Path, [Form.Name + 'Anchors']);
    1:
      Result := GetPathValue(Path, [GetObjectGUID(Form) + '{Anchors}']);
    2:
      Result := GetPathValue(Path, [GetObjectGUID(Form)]);
    3:
      Result := GetPathValue(Path, [Ord(akLeft in Form.Anchors)]);
    4:
      Result := GetPathValue(Path, [Ord(akTop in Form.Anchors)]);
    5:
      Result := GetPathValue(Path, [Ord(akRight in Form.Anchors)]);
    6:
      Result := GetPathValue(Path, [Ord(akBottom in Form.Anchors)]);
  else
    Result := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGetConstraints(Path: WideString; SubPath: WideString; Form: TForm): Pointer;

begin
  case FPathConstraintsCache.IndexOf(SubPath) of
    0:
      Result := GetPathValue(Path, [Form.Name + 'Constraints']);
    1:
      Result := GetPathValue(Path, [GetObjectGUID(Form) + '{Constraints}']);
    2:
      Result := GetPathValue(Path, [GetObjectGUID(Form)]);
    3:
      Result := GetPathValue(Path, [Form.Constraints.MinWidth]);
    4:
      Result := GetPathValue(Path, [Form.Constraints.MaxWidth]);
    5:
      Result := GetPathValue(Path, [Form.Constraints.MinHeight]);
    6:
      Result := GetPathValue(Path, [Form.Constraints.MaxHeight]);
  else
    Result := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGetTypedWidgetLists(Path: WideString; SubPath: WideString; Form: TForm): Pointer;

var
  ObjName: WideString;
  I,
  J,
  CacheIndex: Integer;
  PathCache: TTntStringList;
  Component: TComponent;
  Prop: Variant;

begin
  Result := nil;

  // Loop over all predefined FPathTypedFormControlsList items that are defined.
  for I := 0 to FPathTypedFormControlsList.Count - 1 do
  begin
    if StrLCompW(PWideChar(SubPath), PWideChar(FPathTypedFormControlsList[I] + '/'),
      Length(FPathTypedFormControlsList[I] + '/')) = 0 then
    begin
      SubPath := Copy(SubPath, Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

      if (GetSubstringCount('/', SubPath) = 1) then
        ObjName := Copy(SubPath, 2, MaxInt)
      else
        ObjName := Copy(SubPath, 2, Pos('/', Copy(SubPath, 2, MaxInt)) - 1);

      // Find Component
      Component := nil;
      for J := 0 to Form.ComponentCount - 1 do
        if (Form.Components[J].Name = ObjName) then
        begin
          Component := Form.Components[J];
          Break;
        end;

      if (GetSubstringCount('/', SubPath) = 1) and Assigned(Component) then
      begin
        {Result := GetPathValue(Path,
          [myx_grt_dict_new_proxy(FGrt, FPModule, Path,
            TXGrtFormWidgetType(
            FPathTypedFormControlsList.Objects[I]
            ).GrtClassName)]);}
      end
      else
        if (GetSubstringCount('/', SubPath) = 2) and Assigned(Component) then
        begin
          SubPath := Copy(SubPath, Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

          PathCache := TXGrtFormWidgetType(FPathTypedFormControlsList.Objects[I]).PropertyList;
          CacheIndex := PathCache.IndexOf(SubPath);

          if CacheIndex < 16 then
          begin
            case CacheIndex of
              0:
                Result := GetPathValue(Path, [Component.Name]);
              1:
                Result := GetPathValue(Path, [GetObjectGUID(Component)]);
              2:
                Result := GetPathValue(Path, [GetObjectGUID(Form)]);
              3:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Left')]);
              4:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Top')]);
              5:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Width')]);
              6:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Height')]);
              7:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Autosize')]);
              8:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Caption')]);
              9:
                begin
                  Prop := GetWidgetProp(Component, 'Color');

                  if not (VarIsNull(Prop)) then
                    Result := GetPathValue(Path, [ColorToHtmlColor(Prop)]);
                end;
              10:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Enabled')]);
              11:
                Result := GetPathValue(Path, [GetWidgetProp(Component, 'Visible')]);
              {12:
                begin
                  // Check if property exists
                  if (Component.InheritsFrom(TControl)) then
                    Result := GetPathValue(Path, [myx_grt_dict_new_proxy(FGrt, FPModule, Path, 'forms.Anchors')]);
                end;}
              {13:
                begin
                  //Check if property exists
                  if (Component.InheritsFrom(TControl)) then
                    Result := GetPathValue(Path, [myx_grt_dict_new_proxy(FGrt, FPModule, Path, 'forms.Constraints')]);
                end;}
            end;
          end;
        end
        else
          if (GetSubstringCount('/', SubPath) = 3) and Assigned(Component) then
          begin
            SubPath := Copy(SubPath, Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

            if (StrLCompW(PWideChar(SubPath), '/anchors/', 9) = 0) and
              (Component.InheritsFrom(TControl)) then
            begin
              SubPath := Copy(SubPath, Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

              CacheIndex := FPathAnchorsCache.IndexOf(SubPath);

              case CacheIndex of
                0:
                  Result := GetPathValue(Path, [Component.Name + 'Anchors']);
                1:
                  Result := GetPathValue(Path, [GetObjectGUID(Component) + '{Anchors}']);
                2:
                  Result := GetPathValue(Path, [GetObjectGUID(Component)]);
                3:
                  Result := GetPathValue(Path, [Ord(akLeft in TControl(Component).Anchors)]);
                4:
                  Result := GetPathValue(Path, [Ord(akTop in TControl(Component).Anchors)]);
                5:
                  Result := GetPathValue(Path, [Ord(akRight in TControl(Component).Anchors)]);
                6:
                  Result := GetPathValue(Path, [Ord(akBottom in TControl(Component).Anchors)]);
              end;
            end
            else
              if (StrLCompW(PWideChar(SubPath), '/constraints/', 13) = 0) and
                (Component.InheritsFrom(TControl)) then
              begin
                SubPath := Copy(SubPath, Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);
                CacheIndex := FPathConstraintsCache.IndexOf(SubPath);
                case CacheIndex of
                  0:
                    Result := GetPathValue(Path, [Component.Name + 'Constraints']);
                  1:
                    Result := GetPathValue(Path, [GetObjectGUID(Component) + '{Constraints}']);
                  2:
                    Result := GetPathValue(Path, [GetObjectGUID(Component)]);
                  3:
                    Result := GetPathValue(Path, [TControl(Component).Constraints.MinWidth]);
                  4:
                    Result := GetPathValue(Path, [TControl(Component).Constraints.MaxWidth]);
                  5:
                    Result := GetPathValue(Path, [TControl(Component).Constraints.MinHeight]);
                  6:
                    Result := GetPathValue(Path, [TControl(Component).Constraints.MaxHeight]);
                end;
              end;
          end;

    end;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueGet(argument: Pointer): Pointer;

var
  Path,
    SubPath: WideString;
  Index: Integer;
  Form: TForm;

begin
  Result := nil;

  Path := GetPathArgument(argument);

  // if there is one / this is a application member var
  if (GetSubstringCount('/', Path) = 1) then
  begin
    // e.g. path=/title
    Result := ValueGetApplication(Path);
  end
  else
    // deal with forms
    if (StrLCompW(PWideChar(Path), '/forms/', 7) = 0) then
    begin
      SubPath := Copy(Path, 8, MaxInt);
      Index := StrToIntDef(Copy(SubPath, 1, Pos('/', SubPath) - 1), -1);

      if (Index > -1) and (Index < Screen.FormCount) then
      begin
        Form := Screen.Forms[Index];

        SubPath := Copy(SubPath, Pos('/', SubPath), MaxInt);

        // Get form properties
        if (GetSubstringCount('/', SubPath) = 1) then
        begin
          // e.g. path=/forms/0/name, subpath= /name
          Result := ValueGetForm(Path, SubPath, Form);
        end
        else
          // Get form.anchors properties
          if (StrLCompW(PWideChar(SubPath), '/anchors/', 9) = 0) then
          begin
            SubPath := Copy(SubPath,
              Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

            // e.g. path=/forms/0/anchors/left, subpath= /left
            Result := ValueGetAnchors(Path, SubPath, Form);
          end
          else
            // Get form.constraints properties
            if (StrLCompW(PWideChar(SubPath), '/constraints/', 13) = 0) then
            begin
              SubPath := Copy(SubPath,
                Pos('/', Copy(SubPath, 2, MaxInt)) + 1, MaxInt);

              // e.g. path=/forms/0/constraints/minWidth, subpath= /minWidth
              Result := ValueGetConstraints(Path, SubPath, Form);
            end
            else
            begin
              // handel typed component lists

              Result := ValueGetTypedWidgetLists(Path, SubPath, Form);
            end;
      end;
    end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueSet(argument: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ListSize(argument: Pointer): Pointer;

var
  Path,
    SubPath: WideString;
  Index: Integer;
  Form: TForm;

begin
  Result := nil;

  Path := GetPathArgument(argument);

  if (Path = '/forms') then
    Result := GetPathValue(Path+'%count%', [Screen.FormCount])
  else
    if (StrLCompW(PWideChar(Path), '/forms/', 7) = 0) then
    begin
      SubPath := Copy(Path, 8, MaxInt);
      Index := StrToIntDef(Copy(SubPath, 1, Pos('/', SubPath) - 1), -1);

      if (Index > -1) and (Index < Screen.FormCount) then
      begin
        Form := Screen.Forms[Index];

        // extract SubPath, e.g. /panels
        SubPath := Copy(SubPath, Pos('/', SubPath), MaxInt);

        if (SubPath = '/mainMenu') and (Assigned(Form.Menu)) then
          Result := GetPathValue(Path+'%count%', [Form.Menu.Items.Count]);
      end;
    end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ListItemGet(argument: Pointer): Pointer;

var
  Path,
    SubPath: WideString;
  Index: Integer;
  Form: TForm;

begin
  Result := nil;

  Path := GetPathArgument(argument);

  {if (Copy(Path, 1, 7) = '/forms/') and
    (GetSubstringCount('/', Path) = 2) then
    Result := GetPathValue(Path, [
      myx_grt_dict_new_proxy(FGrt, FPModule,
        Path, 'forms.Form')])
  else
    if (StrLCompW(PWideChar(Path), '/forms/', 7) = 0) then
    begin
      SubPath := Copy(Path, 8, MaxInt);
      Index := StrToIntDef(Copy(SubPath, 1, Pos('/', SubPath) - 1), -1);

      if (Index > -1) and (Index < Screen.FormCount) then
      begin
        Form := Screen.Forms[Index];

        // extract SubPath, e.g. /panels
        SubPath := Copy(SubPath, Pos('/', SubPath), MaxInt);

        if (StrLCompW(PWideChar(SubPath), '/mainMenu/', 10) = 0) and
          (GetSubstringCount('/', SubPath) = 2) and
          (Assigned(Form.Menu)) then
        begin
          SubPath := Copy(SubPath, Length('/mainMenu/') + 1, MaxInt);

          Index := StrToIntDef(SubPath, -1);

          if (Index > -1) and (Index < Form.Menu.Items.Count) then
            Result := GetPathValue(Path, [GetObjectGUID(
                Form.Menu.Items[Index])])
        end;
      end;
    end;}
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ListItemAdd(argument: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ListItemRemove(argument: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.DictItemGet(argument: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.DictItemSet(argument: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.DictItemCount(argument: Pointer): Pointer;

var
  Path,
  SubPath: WideString;
  I,
  J,
  Index,
  Count: Integer;
  Form: TForm;
  WidgetType: TXGrtFormWidgetType;

begin
  Count := 0;

  Path := GetPathArgument(argument);

  if (StrLCompW(PWideChar(Path), '/forms/', 7) = 0) then
  begin
    SubPath := Copy(Path, 8, MaxInt);
    Index := StrToIntDef(Copy(SubPath, 1, Pos('/', SubPath) - 1), -1);

    if (Index > -1) and (Index < Screen.FormCount) then
    begin
      Form := Screen.Forms[Index];

      // extract SubPath, e.g. /panels
      SubPath := Copy(SubPath, Pos('/', SubPath), MaxInt);

      // check if there is a FPathTypedFormControlsList entry
      Index := FPathTypedFormControlsList.IndexOf(SubPath);

      if (Index > -1) then
      begin
        // get the info about the WidgetType
        WidgetType := TXGrtFormWidgetType(
          FPathTypedFormControlsList.Objects[Index]);

        // search for all widgets of the WidgetType
        for I := 0 to Form.ComponentCount - 1 do
          for J := 0 to WidgetType.NativeClassList.Count - 1 do
            if (Form.Components[I].ClassType = WidgetType.NativeClassList[J]) then
              inc(Count);
      end;
    end;
  end;

  Result := myx_grt_value_from_int(Count);
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.DictItemKey(argument: Pointer): Pointer;

var
  Path,
    SubPath,
    Key: WideString;
  I,
    J,
    Index,
    Count: Integer;
  Form: TForm;
  WidgetType: TXGrtFormWidgetType;

begin
  Count := 0;
  Key := '';

  Path := GetPathArgument(argument);

  if (StrLCompW(PWideChar(Path), '/forms/', 7) = 0) then
  begin
    SubPath := Copy(Path, 8, MaxInt);
    Index := StrToIntDef(Copy(SubPath, 1, Pos('/', SubPath) - 1), -1);

    if (Index > -1) and (Index < Screen.FormCount) then
    begin
      Form := Screen.Forms[Index];

      // extract SubPath, e.g. /panels/MainPanel
      SubPath := Copy(SubPath, Pos('/', SubPath), MaxInt);

      // check if there is a FPathTypedFormControlsList entry
      Index := FPathTypedFormControlsList.IndexOf(
        Copy(SubPath, 1, Pos('/', Copy(SubPath, 2, MaxInt))));

      if (Index > -1) then
      begin
        // get the info about the WidgetType
        WidgetType := TXGrtFormWidgetType(
          FPathTypedFormControlsList.Objects[Index]);

        if (GetSubstringCount('/', SubPath) = 2) then
        begin
          SubPath := Copy(SubPath,
            Length(FPathTypedFormControlsList[Index]) + 2, MaxInt);
          Index := StrToIntDef(SubPath, -1);

          if (Index > -1) then
          begin
            for I := 0 to Form.ComponentCount - 1 do
              for J := 0 to WidgetType.NativeClassList.Count - 1 do
                if (Form.Components[I].ClassType = WidgetType.NativeClassList[J]) then
                begin
                  inc(Count);

                  if (Index = Count - 1) then
                  begin
                    Key := Form.Components[I].Name;
                    break;
                  end;
                end;
          end;
        end;
      end;
    end;
  end;

  Result := myx_grt_value_from_string(Key);
end;

// -----------------------------------------------------------------------------

constructor TXGrtFormWidgetType.Create(GrtClassName: WideString;
  Properties: WideString; NativeClasses: array of const);

var
  I: Integer;

begin
  FGrtClassName := GrtClassName;

  FPropertyList := TTntStringList.Create;
  FPropertyList.Text := Properties;

  FNativeClassList := TList.Create;

  for I := 0 to High(NativeClasses) do
  begin
    with NativeClasses[I] do
      case VType of
        vtClass:
          FNativeClassList.Add(VClass);
      else
        raise EInOutError.Create(_('NativeClasses can only contain classes.'));
      end;
  end;
end;

// -----------------------------------------------------------------------------

destructor TXGrtFormWidgetType.Destroy;

begin
  FNativeClassList.Free;
  FPropertyList.Free;
end;

end.

