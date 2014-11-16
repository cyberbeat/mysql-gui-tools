unit GrtForms;

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
  Menus, TntMenus, TntForms, SyncObjs,
  TypInfo, Variants, AuxFuncs, myx_grt_public_interface,
  Grt;

type
  TXGrtFormApplication = class(TObject, IGrtModuleImplementor)
    constructor Create;
    destructor Destroy; override;

    procedure BuildPathCacheLists;

    function CreateBridgeRoot(Argument: Pointer): Pointer;

    function InitDict(PValue: Pointer): Pointer;
    function UpdateToGrt(PValue: Pointer): Pointer;
    function UpdateFromGrt(PValue: Pointer): Pointer;
    function ValueFromGrt(PValue: Pointer): Pointer;
    function ValueToGrt(PValue: Pointer): Pointer;
    function DelValue(PValue: Pointer): Pointer;

    function UpdateToGrt_ComponentList(
      PValue: Pointer; Form: TTntForm; ComponentClass: TClass; StructName: WideString): Pointer;
    function UpdateToGrt_MenuItemRefList(
      PValue: Pointer; MenuItem: TMenuItem): Pointer;

    function ValueToGrt_Application(PValue: Pointer): Pointer;
    function ValueFromGrt_Application(PValue: Pointer): Pointer;
    function ValueToGrt_Form(PValue: Pointer; Form: TTntForm): Pointer;
    function ValueFromGrt_Form(PValue: Pointer; Form: TTntForm): Pointer;
    function ValueToGrt_Menuitem(PValue: Pointer; Menuitem: TTntMenuItem): Pointer;
    function ValueFromGrt_Menuitem(PValue: Pointer; Menuitem: TTntMenuItem): Pointer;
  protected
    function GetObjectById(Id: String): TObject;
    function GetIdByObject(Obj: TObject): String;

    function GrtExecuteModuleFunction(ModuleName: WideString; FunctionName: WideString; arguments: Pointer): Pointer;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    { Private declarations }
    FNativeGrt: Pointer;

    FFunctionCache: THashedStringList;

    FPathApplicationCache,
    FPathFormCache,
    FPathAnchorsCache,
    FPathConstraintsCache,
    FPathActionCache,
    FPathMenuitemCache: TTntStringList;

    FInitializing: Boolean;

    FIdCache: THashedStringList;
  end;

implementation

// -----------------------------------------------------------------------------

constructor TXGrtFormApplication.Create;

begin
  FNativeGrt := RuntimeEnvironment.NativeGrt;

  FInitializing := False;

  BuildPathCacheLists;

  FIdCache := THashedStringList.Create;
  FFunctionCache := THashedStringList.Create;

  FFunctionCache.Add('initialize');
  FFunctionCache.Add('_initDict');
  FFunctionCache.Add('_updateToGrt');
  FFunctionCache.Add('_updateFromGrt');
  FFunctionCache.Add('_valueToGrt');  
  FFunctionCache.Add('_valueFromGrt');
  FFunctionCache.Add('_delValue');

  // Register the module
  RuntimeEnvironment.AddDelphiModule('Forms',
    FFunctionCache.Text, self);
end;

// -----------------------------------------------------------------------------

destructor TXGrtFormApplication.Destroy;

begin
  //RuntimeEnvironment.RemoveDelphiModule('Forms', self);

  FPathApplicationCache.Free;
  FPathFormCache.Free;
  FPathAnchorsCache.Free;
  FPathConstraintsCache.Free;
  FPathActionCache.Free;
  FPathMenuitemCache.Free;

  FIdCache.Free;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;

begin
  Result := E_NOINTERFACE;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication._AddRef: Integer; stdcall;

begin
  Result := 1;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication._Release: Integer; stdcall;

begin
  Result := 1;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetObjectById(Id: String): TObject;

var
  Index: Integer;

begin
  Index := FIdCache.IndexOf(Id);

  if (Index > -1) then
    Result := FIdCache.Objects[Index]
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GetIdByObject(Obj: TObject): String;

var
  Index: Integer;

begin
  Index := FIdCache.IndexOfObject(Obj);

  if (Index > -1) then
    Result := FIdCache[Index]
  else
  begin
    Result := '{' + GetNewGUID + '}';

    FIdCache.AddObject(Result, Obj);
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.GrtExecuteModuleFunction(
  ModuleName: WideString; FunctionName: WideString; arguments: Pointer): Pointer;

var
  FunctionIndex: Integer;

begin
  FunctionIndex := FFunctionCache.IndexOf(FunctionName);

  case FunctionIndex of
    0: Result := CreateBridgeRoot(arguments);
    1: Result := InitDict(arguments);
    2: Result := UpdateToGrt(arguments);
    3: Result := UpdateFromGrt(arguments);
    4: Result := ValueToGrt(arguments);
    5: Result := ValueFromGrt(arguments);
    6: Result := DelValue(arguments);
  else
    Result := nil;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.CreateBridgeRoot(argument: Pointer): Pointer;

var
  Path: WideString;
  PValue: Pointer;

begin
  Path := GetGrtFunctionArgumentAsString(argument);

  FInitializing := True;
  try
    PValue := myx_grt_dict_new_obj(FNativeGrt, 'forms.Application',
      TntApplication.Title, GetIdByObject(Application), '');
  finally
    FInitializing := False;
  end;

  if (myx_grt_dict_item_set_by_path(
    myx_grt_get_root(FNativeGrt), Path, PValue) <> 0) then
  begin
    raise Exception.Create(Format(
      _('The object cannot be created at the given path %s.'),
      [Path]));
  end;

  Result := PValue;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.InitDict(PValue: Pointer): Pointer;

var
  Data: Pointer;
  StructName: WideString;
  NewObj: TObject;

begin
  StructName := myx_grt_dict_struct_get_name(PValue);

  if (StructName = 'forms.Application') then
  begin
    // Assign the global Application object
    myx_grt_value_bridge_data_object_set(PValue, Application)
  end
  else
    if (StructName = 'forms.Form') then
    begin
      Data := myx_grt_value_bridge_data_object_get(PValue);

      // if there is no bridge_data assigned yet, a new form
      // has to be created
      if (Data = nil) then
      begin
        FInitializing := True;
        try
          NewObj := TTntForm.Create(nil);
          myx_grt_value_bridge_data_object_set(PValue, NewObj);

          myx_grt_dict_item_set_value_from_string(PValue, '_id',
            GetIdByObject(NewObj));
        finally
          FInitializing := False;
        end;
      end;
    end;

  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.UpdateToGrt(PValue: Pointer): Pointer;

var
  Data: Pointer;
  Key: String;
  I,
    J: Integer;
  GrtCount: Integer;
  NativeGrtItem,
  NativeGrtItemData: Pointer;
  Found: Boolean;
  Form: TTntForm;

begin
  Result := nil;

  Data := myx_grt_value_bridge_data_owner_get(PValue);

  if (TObject(Data) is TApplication) then
  begin
    Key := _myx_grt_value_bridge_dict_key_get(PValue);

    // sync forms list
    if (Key = 'forms') and
      (myx_grt_value_get_type(PValue) = MYX_LIST_VALUE) then
    begin
      GrtCount := myx_grt_bridge_list_item_count(PValue);

      // check if all forms are in the GRT list
      for I := 0 to Screen.FormCount - 1 do
      begin
        Found := False;

        for J := 0 to GrtCount - 1 do
        begin
          NativeGrtItem := myx_grt_bridge_list_item_get(PValue, J, 0);
          NativeGrtItemData := myx_grt_value_bridge_data_object_get(NativeGrtItem);

          if (NativeGrtItemData = Screen.Forms[I]) then
          begin
            Found := True;
            break;
          end;
        end;

        // if it is not in the list, add it
        if (Not(Found)) then
        begin
          FInitializing := True;
          try
            // create new dict with the form as bridge_data
            NativeGrtItem := myx_grt_bridge_dict_new(FNativeGrt,
              'forms.Form', Screen.Forms[I]);

            // initialize the form's members
            myx_grt_dict_init_obj(FNativeGrt, NativeGrtItem,
              Screen.Forms[I].Name, GetIdByObject(Screen.Forms[I]), '');

            // add the form to the list
            myx_grt_bridge_list_item_insert(PValue, -1, NativeGrtItem, 0);
          finally
            FInitializing := False;
          end;
        end;
      end;

      // check if the GRT list has a form that is no longer available
      for J := GrtCount - 1 downto 0 do
      begin
        Found := False;

        NativeGrtItem := myx_grt_bridge_list_item_get(PValue, J, 0);
        NativeGrtItemData := myx_grt_value_bridge_data_object_get(NativeGrtItem);

        for I := 0 to Screen.FormCount - 1 do
        begin
          if (NativeGrtItemData = Screen.Forms[I]) then
          begin
            Found := True;
            break;
          end;
        end;

        // if it is no longer available, remove it from the GRT list
        if (Not(Found)) then
          myx_grt_bridge_list_item_del(PValue, J, 0);
      end;
    end;
  end
  else
    // deal with typed dicts
    if (TObject(Data) is TTntForm) then
    begin
      Form := TTntForm(TObject(Data));
      Key := _myx_grt_value_bridge_dict_key_get(PValue);

      if (Key = 'mainMenu') and
        (myx_grt_value_get_type(PValue) = MYX_LIST_VALUE) then
      begin
        if (Form.Menu <> nil) then
        begin
          Result := UpdateToGrt_MenuItemRefList(PValue, Form.Menu.Items);
        end;
      end;

      if (myx_grt_value_get_type(PValue) = MYX_DICT_VALUE) then
      begin
        if (Key = 'menuitems') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntMenuItem, 'forms.Menuitem')
        else
        if (Key = 'panels') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntPanel, 'forms.Panel')
        else
        if (Key = 'buttons') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntButton, 'forms.Button')
        else
        if (Key = 'edits') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntEdit, 'forms.Edit')
        else
        if (Key = 'checkboxes') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntCheckBox, 'forms.Checkbox')
        else
        if (Key = 'radiobuttons') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntRadioButton, 'forms.Radiobutton')
        else
        if (Key = 'listboxes') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntListbox, 'forms.Listbox')
        else
        if (Key = 'dropdowns') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntComboBox, 'forms.Dropdown')
        else
        if (Key = 'pagecontrols') then
          Result := UpdateToGrt_ComponentList(PValue, Form, TTntPageControl, 'forms.Pagecontrol');
      end;
    end
    else
      // deal with menuitms
      if (TObject(Data) is TTntMenuItem) then
      begin
        Key := _myx_grt_value_bridge_dict_key_get(PValue);

        if (Key = 'items') and
          (myx_grt_value_get_type(PValue) = MYX_LIST_VALUE) then
        begin
          Result := UpdateToGrt_MenuItemRefList(PValue, TTntMenuItem(TObject(Data)));
        end;
      end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.UpdateToGrt_ComponentList(
  PValue: Pointer; Form: TTntForm; ComponentClass: TClass; StructName: WideString): Pointer;

var
  Key: String;
  I,
    J: Integer;
  GrtCount: Integer;
  NativeGrtItem,
  NativeGrtItemData: Pointer;
  Found: Boolean;
  CacheList: TList;
  
begin
  Result := nil;
  
  GrtCount := myx_grt_bridge_dict_item_count(PValue, 0);

  CacheList := TList.Create;
  try
    for I := 0 to Form.ComponentCount - 1 do
    begin
      if (Form.Components[I].Name = '') then
        continue;

      if (Form.Components[I] is ComponentClass) then
      begin
        CacheList.Add(Form.Components[I]);
        Found := False;

        for J := 0 to GrtCount - 1 do
        begin
          Key := myx_grt_bridge_dict_item_key_by_index(PValue, J, 0);

          if (Key = Form.Components[I].Name) then
          begin
            Found := True;
            break;
          end;
        end;

        // if it is not in the list, add it
        if (Not(Found)) then
        begin
          FInitializing := True;
          try
            // create new dict with the component as bridge_data
            NativeGrtItem := myx_grt_bridge_dict_new(FNativeGrt,
              StructName, Form.Components[I]);

            // initialize the component's members
            myx_grt_dict_init_obj(FNativeGrt, NativeGrtItem,
              Form.Components[I].Name, GetIdByObject(Form.Components[I]), '');

            // add the component to the list
            myx_grt_bridge_dict_item_set_value(PValue, Form.Components[I].Name, NativeGrtItem, 0);
          finally
            FInitializing := False;
          end;
        end;
      end;
    end;

    // check if the GRT list has a form that is no longer available
    for J := GrtCount - 1 downto 0 do
    begin
      Found := False;

      NativeGrtItem := myx_grt_bridge_dict_item_value_by_index(PValue, J, 0);
      NativeGrtItemData := myx_grt_value_bridge_data_object_get(NativeGrtItem);

      for I := 0 to CacheList.Count - 1 do
      begin
        if (NativeGrtItemData = CacheList[I]) then
        begin
          Found := True;
          break;
        end;
      end;

      // if it is no longer available, remove it from the GRT list
      if (Not(Found)) then
      begin
        Key := _myx_grt_bridge_dict_item_key_by_index(PValue, J, 0);
        _myx_grt_bridge_dict_item_del(PValue, PChar(Key), 0);
      end;
    end;
  finally
    CacheList.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.UpdateToGrt_MenuItemRefList(
  PValue: Pointer; MenuItem: TMenuItem): Pointer;

var
  GrtCount,
  I,
  J: Integer;
  Key: String;
  Found: Boolean;
  NativeGrtItem: Pointer;

begin
  Result := nil;

  GrtCount := myx_grt_bridge_list_item_count(PValue);

  for I := 0 to MenuItem.Count - 1 do
  begin
    Found := False;

    Key := GetIdByObject(MenuItem[I]);

    for J := 0 to GrtCount - 1 do
    begin
      NativeGrtItem := myx_grt_bridge_list_item_get(PValue, J, 0);

      if (_myx_grt_value_as_string(NativeGrtItem) = Key) then
      begin
        Found := True;
        break;
      end;
    end;

    // if it is not in the list, add it
    if (Not(Found)) then
    begin
      FInitializing := True;
      try
        if (Key <> '') then
        begin
          NativeGrtItem := _myx_grt_value_from_string(PChar(Key));

          // add the form to the list
          myx_grt_bridge_list_item_insert(PValue, -1, NativeGrtItem, 0);
        end;
      finally
        FInitializing := False;
      end;
    end;
  end;

  // check if the GRT list has a form that is no longer available
  for J := GrtCount - 1 downto 0 do
  begin
    Found := False;

    NativeGrtItem := myx_grt_bridge_list_item_get(PValue, J, 0);

    Key := _myx_grt_value_as_string(NativeGrtItem);

    for I := 0 to MenuItem.Count - 1 do
    begin
      if (GetIdByObject(MenuItem[I]) = Key) then
      begin
        Found := True;
        break;
      end;
    end;

    // if it is no longer available, remove it from the GRT list
    if (Not(Found)) then
      myx_grt_bridge_list_item_del(PValue, J, 0);
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.UpdateFromGrt(PValue: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueFromGrt(PValue: Pointer): Pointer;

var
  Data: Pointer;

begin
  Result := nil;

  if (FInitializing) then
    Exit;

  Data := myx_grt_value_bridge_data_owner_get(PValue);

  if (TObject(Data) is TApplication) then
    Result := ValueFromGrt_Application(PValue)
  else
    if (TObject(Data) is TTntForm) then
      ValueFromGrt_Form(PValue, TTntForm(Data))
    else
      if (TObject(Data) is TTntMenuItem) then
        ValueFromGrt_Menuitem(PValue, TTntMenuItem(Data));
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueToGrt(PValue: Pointer): Pointer;

var
  Data: Pointer;

begin
  Result := nil;

  Data := myx_grt_value_bridge_data_owner_get(PValue);

  if (TObject(Data) is TApplication) then
    Result := ValueToGrt_Application(PValue)
  else
    if (TObject(Data) is TTntForm) then
      Result := ValueToGrt_Form(PValue, TTntForm(Data))
    else
      if (TObject(Data) is TTntMenuItem) then
        ValueToGrt_Menuitem(PValue, TTntMenuItem(Data));
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueToGrt_Application(PValue: Pointer): Pointer;

var
  Key: String;
  Index: Integer;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathApplicationCache.IndexOf(Key);

  case Index of
    0:
      myx_grt_bridge_value_change_string(PValue, TntApplication.Title);
    3:
      myx_grt_bridge_value_change_int(PValue, Ord(Application.Active));
    4:
      myx_grt_bridge_value_change_int(PValue, Mouse.CursorPos.X);
    5:
      myx_grt_bridge_value_change_int(PValue, Mouse.CursorPos.Y);
    6:
      begin
      end;
    7:
      myx_grt_bridge_value_change_string(PValue,
        GetIdByObject(Application.MainForm));
    8:
      myx_grt_bridge_value_change_string(PValue,
        GetIdByObject(Screen.ActiveForm));
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueFromGrt_Application(PValue: Pointer): Pointer;

var
  Key: String;
  Index: Integer;
  Obj: TObject;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathApplicationCache.IndexOf(Key);

  case Index of
    0:
      TntApplication.Title := myx_grt_bridge_value_as_string(PValue);
    4:
      SetCursorPos(myx_grt_bridge_value_as_int(PValue), Mouse.CursorPos.Y);
    5:
      SetCursorPos(Mouse.CursorPos.X, myx_grt_bridge_value_as_int(PValue));
    8:
      begin
        Obj := GetObjectById(myx_grt_bridge_value_as_string(PValue));

        if (Obj <> nil) then
          TForm(Obj).SetFocus;
      end;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueToGrt_Form(PValue: Pointer; Form: TTntForm): Pointer;

var
  Key,
  StringVal: String;
  Index: Integer;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathFormCache.IndexOf(Key);

  case Index of
    0:
      myx_grt_bridge_value_change_string(PValue, Form.Name);
    3:
      myx_grt_bridge_value_change_int(PValue, Form.Left);
    4:
      myx_grt_bridge_value_change_int(PValue, Form.Top);
    5:
      myx_grt_bridge_value_change_int(PValue, Form.Width);
    6:
      myx_grt_bridge_value_change_int(PValue, Form.Height);
    8:
      myx_grt_bridge_value_change_string(PValue, Form.Caption);
    9:
      myx_grt_bridge_value_change_string(PValue, ColorToHtmlColor(Form.Color));
    10:
      myx_grt_bridge_value_change_int(PValue, Ord(Form.Enabled));
    11:
      myx_grt_bridge_value_change_int(PValue, Ord(Form.Visible));

    28:
      myx_grt_bridge_value_change_int(PValue, Ord(Form.AlphaBlend));
    29:
      myx_grt_bridge_value_change_int(PValue, Form.AlphaBlendValue);
    30:
      begin
        case Form.WindowState of
          wsNormal:
            StringVal := 'Normal';
          wsMinimized:
            StringVal := 'Minimized';
          wsMaximized:
            StringVal := 'Maximized';
        end;

        myx_grt_bridge_value_change_string(PValue, StringVal);
      end;
    38:
      UpdateToGrt_ComponentList(PValue, Form, TTntMenuItem, 'forms.Menuitem');
    39:
      UpdateToGrt_ComponentList(PValue, Form, TTntPanel, 'forms.Panel');
    40:
      UpdateToGrt_ComponentList(PValue, Form, TTntButton, 'forms.Button');
    41:
      UpdateToGrt_ComponentList(PValue, Form, TTntEdit, 'forms.Edit');
    42:
      UpdateToGrt_ComponentList(PValue, Form, TTntCheckBox, 'forms.Checkbox');
    43:
      UpdateToGrt_ComponentList(PValue, Form, TTntRadioButton, 'forms.Radiobutton');
    44:
      UpdateToGrt_ComponentList(PValue, Form, TTntListBox, 'forms.Listbox');
    45:
      UpdateToGrt_ComponentList(PValue, Form, TTntComboBox, 'forms.Dropdown');
    46:
      UpdateToGrt_ComponentList(PValue, Form, TTntPageControl, 'forms.Pagecontrol');
    48:
      UpdateToGrt_MenuItemRefList(PValue, Form.Menu.Items);
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueFromGrt_Form(PValue: Pointer; Form: TTntForm): Pointer;

var
  Key,
  StringVal: String;
  Index,
  IntVal: Integer;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathFormCache.IndexOf(Key);

  case Index of
    0:
      Form.Name := myx_grt_bridge_value_as_string(PValue);
    3:
      Form.Left := myx_grt_bridge_value_as_int(PValue);
    4:
      Form.Top := myx_grt_bridge_value_as_int(PValue);
    5:
      Form.Width := myx_grt_bridge_value_as_int(PValue);
    6:
      Form.Height := myx_grt_bridge_value_as_int(PValue);
    8:
      Form.Caption := myx_grt_bridge_value_as_string(PValue);
    9:
      Form.Color := HtmlColorToColor(myx_grt_bridge_value_as_string(PValue));
    10:
      Form.Enabled := myx_grt_bridge_value_as_int(PValue) = 1;
    11:
      Form.Visible := myx_grt_bridge_value_as_int(PValue) = 1;

    28:
      Form.AlphaBlend := myx_grt_bridge_value_as_int(PValue) = 1;
    29:
      Form.AlphaBlendValue := myx_grt_bridge_value_as_int(PValue);
    30:
      begin
        StringVal := myx_grt_bridge_value_as_string(PValue);

        if (WideSameText(StringVal, 'Minimized')) then
          IntVal := Ord(wsMinimized)
        else
          if (WideSameText(StringVal, 'Maximized')) then
            IntVal := Ord(wsMaximized)
          else
            IntVal := Ord(wsNormal);

        Form.WindowState := TWindowState(IntVal);
      end;
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueToGrt_Menuitem(PValue: Pointer; Menuitem: TTntMenuItem): Pointer;

var
  Key: String;
  Index: Integer;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathMenuitemCache.IndexOf(Key);

  case Index of
    0:
      myx_grt_bridge_value_change_string(PValue, Menuitem.Name);
    4:
      myx_grt_bridge_value_change_string(PValue, Menuitem.Caption);
    5:
      myx_grt_bridge_value_change_int(PValue, Ord(Menuitem.Checked));
    6:
      myx_grt_bridge_value_change_int(PValue, Ord(Menuitem.Enabled));
    7:
      myx_grt_bridge_value_change_int(PValue, Menuitem.ImageIndex);
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.ValueFromGrt_Menuitem(PValue: Pointer; Menuitem: TTntMenuItem): Pointer;

var
  Key: String;
  Index: Integer;

begin
  Result := nil;

  Key := _myx_grt_value_bridge_dict_key_get(PValue);

  Index := FPathMenuitemCache.IndexOf(Key);

  case Index of
    0:
      Menuitem.Name := myx_grt_bridge_value_as_string(PValue);
    4:
      Menuitem.Caption := myx_grt_bridge_value_as_string(PValue);
    5:
      Menuitem.Checked := (myx_grt_bridge_value_as_int(PValue) = 1);
    6:
      Menuitem.Enabled := (myx_grt_bridge_value_as_int(PValue) = 1);
    7:
      Menuitem.ImageIndex := myx_grt_bridge_value_as_int(PValue);
  end;
end;

// -----------------------------------------------------------------------------

function TXGrtFormApplication.DelValue(PValue: Pointer): Pointer;

begin
  Result := nil;
end;

// -----------------------------------------------------------------------------

procedure TXGrtFormApplication.BuildPathCacheLists;

var
  PathObjectCache,
    PathControlCache: WideString;

begin
  // Build Path caches
  PathObjectCache :=
    'name'#13#10 +
    '_id'#13#10 +
    'parent'#13#10;

  FPathApplicationCache := TTntStringList.Create;
  FPathApplicationCache.Text :=
    PathObjectCache +
    'active'#13#10 +
    'mouseX'#13#10 +
    'mouseY'#13#10 +
    'forms'#13#10 +
    'mainForm'#13#10 +
    'activeForm'#13#10;

  PathControlCache :=
    'left'#13#10 +
    'top'#13#10 +
    'width'#13#10 +
    'height'#13#10 +
    'autosize'#13#10 +
    'caption'#13#10 +
    'color'#13#10 +
    'enabled'#13#10 +
    'visible'#13#10 +
    'anchors'#13#10 +
    'constraints'#13#10 +
    'parent'#13#10 +
    'controls'#13#10 +
    'eventOnClick'#13#10 +
    'eventOnDblClick'#13#10 +
    'eventOnResize'#13#10 +
    'eventOnMouseDown'#13#10 +
    'eventOnMouseMove'#13#10 +
    'eventOnMouseUp'#13#10 +
    'eventOnMouseIn'#13#10 +
    'eventOnMouseOut'#13#10 +
    'eventOnKeyDown'#13#10 +
    'eventOnKeyUp'#13#10;

  FPathFormCache := TTntStringList.Create;
  FPathFormCache.Text :=
    PathObjectCache +
    PathControlCache +
    'active'#13#10 +
    'activeControl'#13#10 +
    'alphaBlend'#13#10 +
    'alphaBlendValue'#13#10 +
    'windowState'#13#10 +
    'mainMenu'#13#10 +
    'eventOnCreate'#13#10 +
    'eventOnDestroy'#13#10 +
    'eventOnActivate'#13#10 +
    'eventOnDeactivate'#13#10 +
    'eventOnClose'#13#10 +
    'actions'#13#10 +
    'menuitems'#13#10 +
    'panels'#13#10 +
    'buttons'#13#10 +
    'edits'#13#10 +
    'checkboxes'#13#10 +
    'radiobuttons'#13#10 +
    'listboxes'#13#10 +
    'dropdowns'#13#10 +
    'pageControls'#13#10;

  //FPathFormCache_TypedFormControlsListStart := 37;

  FPathAnchorsCache := TTntStringList.Create;
  FPathAnchorsCache.Text :=
    PathObjectCache +
    'left'#13#10 +
    'top'#13#10 +
    'right'#13#10 +
    'bottom'#13#10;

  FPathConstraintsCache := TTntStringList.Create;
  FPathConstraintsCache.Text :=
    PathObjectCache +
    'minWidth'#13#10 +
    'maxWidth'#13#10 +
    'minHeight'#13#10 +
    'maxHeight'#13#10;

  // Action
  FPathActionCache := TTntStringList.Create;
  FPathActionCache.Text :=
    PathObjectCache +
    'autoCheck'#13#10 +
    'caption'#13#10 +
    'checked'#13#10 +
    'enabled'#13#10 +
    'groupIndex'#13#10 +
    'imageIndex'#13#10 +
    'shortcut'#13#10 +
    'visible'#13#10 +
    'eventOnExecute'#13#10 +
    'eventOnUpdate'#13#10;

  // Menuitem
  FPathMenuItemCache := TTntStringList.Create;
  FPathMenuItemCache.Text :=
    PathObjectCache +
    'action'#13#10 +
    'caption'#13#10 +
    'checked'#13#10 +
    'enabled'#13#10 +
    'imageIndex'#13#10 +
    'items'#13#10 +
    'eventOnClick'#13#10;

  // Instead of having individual lists for all typed dicts
  // for the form's controls, they are keept in a seperate
  // stringlist
  {FPathTypedFormControlsList := TTntStringList.Create;

  // Add panels
  FPathTypedFormControlsList.AddObject('/panels',
    TXGrtFormWidgetType.Create('forms.Panel',
    PathObjectCache +
    PathControlCache +
    'border'#13#10,
    [TTntPanel, TPanel]));

  // Add buttons
  FPathTypedFormControlsList.AddObject('/buttons',
    TXGrtFormWidgetType.Create(
    'forms.Button',
    PathObjectCache +
    PathControlCache +
    'isDefault'#13#10 +
    'icon'#13#10,
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
    [TTntPageControl, TPageControl]));}
end;

end.

