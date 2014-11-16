unit Toolbar;

interface

uses
  gnugettext, Windows, Messages, Classes, Graphics, Forms, ExtCtrls, PNGImage, Controls,
  AuxFuncs, TntStdCtrls, TntExtCtrls, Contnrs, TntSysUtils, SysUtils, Themes,
  ActnList;

type
  TToolbars = class;
  TToolbarContainer = class;
  TToolbarItemGroup = class;
  TToolbarItem = class;
  TPNGButton = class;

{ TToolbarItemActionLink }

  TToolbarItemActionLink = class;

  TToolbars = class(TObject)
    constructor Create; reintroduce;
    destructor Destroy; override;
  private
    FToolbarList: TObjectList;
  protected
    function GetToolbar(const Name: WideString): TToolbarContainer;
    function GetGroup(const Name: WideString): TToolbarItemGroup;
    procedure SetGroupEnabled(const Name: WideString; Enabled: Boolean);
    function GetItem(const Name: WideString): TToolbarItem;
    function GetGroupVisible(const Name: WideString): Boolean;
    procedure SetGroupVisible(const Name: WideString; Visible: Boolean);
    function GetItemEnabled(const Name: WideString): Boolean;
    procedure SetItemEnabled(const Name: WideString; Enabled: Boolean);
    function GetItemDown(const Name: WideString): Boolean;
    procedure SetItemDown(const Name: WideString; Down: Boolean);
  public
    function AddToolbar(Toolbar: TWinControl; Name: WideString;
      CreateToolbarBG: Boolean = True; Vertical: Boolean = False;
      RightOffset: Integer = 0): TToolbarContainer;
    function GetItemNameByObject(Obj: TObject): WideString;

    property Toolbars[const Name: WideString]: TToolbarContainer read GetToolbar;
    property Groups[const Name: WideString]: TToolbarItemGroup read GetGroup;
    property GroupsEnabled[const Name: WideString]: Boolean write SetGroupEnabled;
    property GroupVisible[const Name: WideString]: Boolean read GetGroupVisible write SetGroupVisible;
    property Items[const Name: WideString]: TToolbarItem read GetItem;
    property ItemsEnabled[const Name: WideString]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemsDown[const Name: WideString]: Boolean read GetItemDown write SetItemDown;
  end;

  TToolbarContainer = class(TObject)
    constructor Create(Toolbar: TWinControl;
      Name: WideString; CreateToolbarBG: Boolean = True;
      Vertical: Boolean = False; RightOffset: Integer = 0);
    destructor Destroy; override;
  private
    FToolbar: TWinControl;
    FGroupList: TObjectList;
    FName: WideString;

    FToolbarBG: TTntPaintBox;
    FBGBitmap: TBitmap;

    FBGShape: TTntShape;

    FRightSpace: integer;
    FRightPos: integer;

    FVertical: Boolean;
    FRightOffset: Integer;

    FOnDragOver: TDragOverEvent;

    procedure DoPaintBG(Sender: TObject);
  protected
    procedure SetBGBitmap(BGBitmap: TBitmap);
    function GetWidth: integer;

    function GetGroup(const Name: WideString): TToolbarItemGroup;
    function GetGroupByIndex(Index: Integer): TToolbarItemGroup;
    function GetItem(const Name: WideString): TToolbarItem;
    function GetItemEnabled(const Name: WideString): Boolean;
    procedure SetItemEnabled(const Name: WideString; Enabled: Boolean);
    function GetItemDown(const Name: WideString): Boolean;
    procedure SetItemDown(const Name: WideString; Down: Boolean);

    procedure SetOnDragOver(OnDragOver: TDragOverEvent);
    function GetGroupCount: Integer;
  public
    function AddGroup(Name: WideString;
      DisplaySeparator: Boolean = True; AlignRight: Boolean = False;
      InitialWidth: Integer = 10): TToolbarItemGroup;

    property Groups[const Name: WideString]: TToolbarItemGroup read GetGroup;
    property GroupsByIndex[Name: Integer]: TToolbarItemGroup read GetGroupByIndex;
    property Items[const Name: WideString]: TToolbarItem read GetItem;
    property ItemsEnabled[const Name: WideString]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemsDown[const Name: WideString]: Boolean read GetItemDown write SetItemDown;

    function GetGroupIndex(Item: TToolbarItemGroup): Integer;
  published
    property Name: WideString read FName;
    property Toolbar: TWinControl read FToolbar;
    property BGBitmap: TBitmap read FBGBitmap write SetBGBitmap;
    property Width: integer read GetWidth;
    property RightSpace: integer read FRightSpace write FRightSpace;
    property RightPos: integer read FRightPos write FRightPos;
    property Vertical: Boolean read FVertical write FVertical;
    property OnDragOver: TDragOverEvent read FOnDragOver write SetOnDragOver;
    property GroupCount: Integer read GetGroupCount;
  end;

  TToolbarItemGroup = class(TObject)
    constructor Create(ToolbarContainer: TToolbarContainer;
      Name: WideString; Left: integer;
      DisplaySeparator: Boolean = True;
      AlignRight: Boolean = False;
      InitialWidth: Integer = 10);
    destructor Destroy; override;
  private
    FToolbarContainer: TToolbarContainer;
    FName: WideString;
    FDisplaySeparator: Boolean;
    FAlignRight: Boolean;
    FLeft: Integer;
    FSpace: Integer;

    FSep : TTntBevel;
    FVisible: Boolean;
    FEnabled: Boolean;

    FInitialWidth: Integer;

    FItemList: TObjectList;
  protected
    procedure SetLeft(Left: integer);
    function GetWidth: integer;

    function GetItem(const Name: WideString): TToolbarItem;

    procedure SetVisible(Visible: Boolean);
    procedure SetEnabled(Enabled: Boolean);
    function GetItemCount: Integer;

    function GetItemByIndex(Index: Integer): TToolbarItem;

  public
    function AddItem(Name: WideString;
      ImageResource: WideString;
      Caption: WideString; Hint: WideString;
      Top: Integer;
      ShowLbl: Boolean; LblAtBottom: Boolean;
      AnchorRight: Boolean;
      OnBtnClick: TNotifyEvent;
      OnLblClick: TNotifyEvent = nil;
      CustomControl: TWinControl = nil;
      Enabled: Boolean = True; Down: Boolean = False;
      NoSpace: Boolean = False;
      Action: TBasicAction = nil): TToolbarItem;

    function GetItemIndex(Item: TToolbarItem): Integer;

    property Items[const Name: WideString]: TToolbarItem read GetItem;
    property ItemsByIndex[I: Integer]: TToolbarItem read GetItemByIndex;
  published
    property ToolbarContainer: TToolbarContainer read FToolbarContainer;
    property Left: integer read FLeft write SetLeft;
    property Width: integer read GetWidth;
    property AlignRight: Boolean read FAlignRight;
    property Space: integer read FSpace write FSpace;
    property Visible: Boolean read FVisible write SetVisible;
    property Name: WideString read FName;
    property Enabled: Boolean write SetEnabled;
    property ItemCount: Integer read GetItemCount;
  end;

  TToolbarItem = class(TObject)
    constructor Create(ToolbarItemGroup: TToolbarItemGroup;
      Name: WideString;
      ImageResource: WideString;
      Caption: WideString; Hint: WideString;
      Left: Integer; Top: Integer;
      ShowLbl: Boolean; LblAtBottom: Boolean;
      AnchorRight: Boolean;
      OnBtnClick: TNotifyEvent;
      OnLblClick: TNotifyEvent = nil;
      CustomControl: TWinControl = nil;
      NoSpace: Boolean = False);
    destructor Destroy; override;
  private
    FToolbarItemGroup: TToolbarItemGroup;
    FName: WideString;
    FEnabled: Boolean;
    FDown: Boolean;
    FVisible: Boolean;
    FLeft: integer;
    FPNGBtn: TPNGButton;
    FBtnLbl: TTntLabel;
    FLblDropdownImg: TTntImage;
    FLblDropdownPngImg,
    FLblDropdownDisabledPngImg: TPNGObject;
    FLblAtBottom: Boolean;
    FCustomControl: TWinControl;
    FNoSpace: Boolean;

    FActionLink: TToolbarItemActionLink;
  protected
    function GetEnabled: Boolean;
    procedure SetEnabled(Enabled: Boolean);

    function GetDown: Boolean;
    procedure SetDown(Down: Boolean);
    procedure SetVisible(Visible: Boolean);

    procedure SetLeft(Left: integer);
    function GetWidth: integer;
    function GetHeight: integer;

    procedure SetAction(Value: TBasicAction);
    procedure DoActionChange(Sender: TObject);
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean);
    function GetAction: TBasicAction;
  published
    property ToolbarItemGroup: TToolbarItemGroup read FToolbarItemGroup;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Down: Boolean read GetDown write SetDown;
    property Visible: Boolean read FVisible write SetVisible;
    property Left: integer read FLeft write SetLeft;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property ActionLink: TToolbarItemActionLink read FActionLink write FActionLink;
    property Action: TBasicAction read GetAction write SetAction;
  end;

  TPNGButton = class(TTntImage)
    constructor Create(Owner: TObject; ImgResourceName: WideString); reintroduce;
    destructor Destroy; override;

    procedure DoMouseLeave(Sender: TObject);

    procedure ResetButton;

    procedure Paint; override;
  private
    FButtonImg: TPNGObject;
    FButtonDownImg: TPNGObject;
    FButtonHighlightImg: TPNGObject;

    FEnabled: Boolean;

    FDown: Boolean;
    FClicked: Boolean;
    FMouseOver: Boolean;

    FOnBtnClick: TNotifyEvent;

    FOwner: TObject;

    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure MouseLeave(Sender: TObject);

    function GetEnabled: Boolean; override;
    procedure SetEnabled(Enabled: Boolean); override;

    function GetDown: Boolean;
    procedure SetDown(Down: Boolean);
  public
    ToolbarItem: TToolbarItem;
  published
    property Down: Boolean read GetDown write SetDown;
    property OnBtnClick: TNotifyEvent read FOnBtnClick write FOnBtnClick;
    property Owner: TObject read FOwner;
  end;

  TToolbarItemActionLink = class(TActionLink)
  protected
    FClient: TToolbarItem;
    procedure AssignClient(AClient: TObject); override;
    function IsAutoCheckLinked: Boolean; virtual;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsShortCutLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetAutoCheck(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetShortCut(Value: TShortCut); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

  TToolbarItemActionLinkClass = class of TToolbarItemActionLink;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  PNGTools;
  
//----------------------------------------------------------------------------------------------------------------------

constructor TToolbars.Create;

begin
  inherited;

  FToolbarList:=TObjectList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TToolbars.Destroy;

begin
  FToolbarList.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetToolbar(const Name: WideString): TToolbarContainer;

var
  I: integer;

begin
  Result := nil;

  for I:=0 to FToolbarList.Count-1 do
    if CompareText(TToolbarContainer(FToolbarList[I]).Name, Name)=0 then
    begin
      Result := TToolbarContainer(FToolbarList[I]);
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetGroup(const Name: WideString): TToolbarItemGroup;

var
  I: integer;

begin
  Result := nil;

  for I:=0 to FToolbarList.Count-1 do
  begin
    Result := TToolbarContainer(FToolbarList[I]).Groups[Name];
    if Result<>nil then
      Break;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetItem(const Name: WideString): TToolbarItem;

var
  I: integer;

begin
  Result := nil;
  for I:=0 to FToolbarList.Count-1 do
  begin
    Result := TToolbarContainer(FToolbarList[I]).Items[Name];
    if Result <> nil then
      Break;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetItemEnabled(const Name: WideString): Boolean;

var
  I: integer;
  Item: TToolbarItem;

begin
  Result := False;
  
  for I:=0 to FToolbarList.Count-1 do
  begin
    Item := TToolbarContainer(FToolbarList[I]).Items[Name];
    if Item<>nil then
    begin
      Result := Item.Enabled;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbars.SetItemEnabled(const Name: WideString; Enabled: Boolean);

var
  I: integer;

begin
  for I:=0 to FToolbarList.Count-1 do
    TToolbarContainer(FToolbarList[I]).ItemsEnabled[Name] := Enabled;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetItemDown(const Name: WideString): Boolean;

var
  I: integer;
  Item: TToolbarItem;

begin
  Result := False;

  for I:=0 to FToolbarList.Count-1 do
  begin
    Item := TToolbarContainer(FToolbarList[I]).Items[Name];
    if Item<>nil then
    begin
      Result := Item.Down;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbars.SetItemDown(const Name: WideString; Down: Boolean);

var
  I: integer;

begin
  for I:=0 to FToolbarList.Count-1 do
    TToolbarContainer(FToolbarList[I]).ItemsDown[Name] := Down;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.AddToolbar(Toolbar: TWinControl; Name: WideString;
  CreateToolbarBG: Boolean; Vertical: Boolean; RightOffset: Integer): TToolbarContainer;

begin
  Result:=TToolbarContainer.Create(Toolbar, Name,
    CreateToolbarBG, Vertical, RightOffset);

  FToolbarList.Add(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetGroupVisible(const Name: WideString): Boolean;

var
  ToolbarGroup: TToolbarItemGroup;
  Toolbar, Group: WideString;
  I: Integer;

begin
  Result := False;
  
  if Pos('/', Name)>0 then
  begin
    Toolbar := Copy(Name, 1, Pos('/', Name)-1);
    Group := Copy(Name, Pos('/', Name)+1, Length(Name));
  end
  else
  begin
    Toolbar:='';
    Group:=Name;
  end;

  for I:=0 to FToolbarList.Count-1 do
    if (CompareText(TToolbarContainer(FToolbarList[I]).Name, Toolbar)=0) or
      (Toolbar='') then
    begin
      ToolbarGroup := TToolbarContainer(FToolbarList[I]).Groups[Group];

      if Assigned(ToolbarGroup) then
      begin
        Result:=ToolbarGroup.Visible;
        Break;
      end;
    end;
end;

// ------------------------------------------------------------

procedure TToolbars.SetGroupVisible(const Name: WideString; Visible: Boolean);

var
  I, J: integer;
  Toolbar, Group: WideString;

begin
  if Pos('/', Name)>0 then
  begin
    Toolbar := Copy(Name, 1, Pos('/', Name)-1);
    Group := Copy(Name, Pos('/', Name)+1, Length(Name));
  end
  else
  begin
    Toolbar:='';
    Group:=Name;
  end;

  for I:=0 to FToolbarList.Count-1 do
    if (CompareText(TToolbarContainer(FToolbarList[I]).Name, Toolbar)=0) or
      (Toolbar='') then
      for J:=0 to TToolbarContainer(FToolbarList[I]).FGroupList.Count-1 do
        if CompareText(TToolbarItemGroup(TToolbarContainer(
          FToolbarList[I]).FGroupList[J]).Name, Group)=0 then
        begin
          TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).Visible := Visible;
        end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbars.SetGroupEnabled(const Name: WideString; Enabled: Boolean);

var
  I, J: integer;

begin
  for I:=0 to FToolbarList.Count-1 do
  begin
    for J:=0 to TToolbarContainer(FToolbarList[I]).FGroupList.Count-1 do
      if CompareText(TToolbarItemGroup(TToolbarContainer(
        FToolbarList[I]).FGroupList[J]).Name, Name)=0 then
      begin
        TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).Enabled:=Enabled;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbars.GetItemNameByObject(Obj: TObject): WideString;

var
  I, J, K: integer;

begin
  Result:='';

  for I:=0 to FToolbarList.Count-1 do
    for J:=0 to TToolbarContainer(FToolbarList[I]).FGroupList.Count-1 do
      for K:=0 to TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).FItemList.Count-1 do
        if (TToolbarItem(TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).FItemList[K]).FPNGBtn = Obj) or
          (TToolbarItem(TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).FItemList[K]).FCustomControl = Obj) then
        begin
          Result := TToolbarItem(TToolbarItemGroup(TToolbarContainer(FToolbarList[I]).FGroupList[J]).FItemList[K]).FName;
          Break;
        end;
end;

//----------------- TToolbarContainer ----------------------------------------------------------------------------------------------------

constructor TToolbarContainer.Create(Toolbar: TWinControl;
  Name: WideString; CreateToolbarBG: Boolean;
  Vertical: Boolean; RightOffset: Integer);

begin
  inherited Create;

  FToolbar := Toolbar;
  Fname := Name;
  FGroupList := TObjectList.Create;

  FRightSpace := 0;
  FRightPos := 0;
  FVertical := Vertical;
  FRightOffset := RightOffset;

  FBGBitmap:=nil;

  if CreateToolbarBG {or ThemeServices.ThemesEnabled} then
  begin
    FToolbarBG:=TTntPaintBox.Create(nil);
    FToolbarBG.Parent := Toolbar;
    FToolbarBG.SendToBack;
    FToolbarBG.Align := alClient;
    FToolbarBG.OnPaint := DoPaintBG;

    FBGShape := nil;
  end
  else
  begin
    FBGShape := TTntShape.Create(nil);
    FBGShape.Parent := Toolbar;
    FBGShape.SendToBack;
    FBGShape.Pen.Color := clBtnFace;
    FBGShape.Brush.Color := clBtnFace;
    FBGShape.Align := alClient;

    FToolbarBG:=nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TToolbarContainer.Destroy;

begin
  FreeAndNil(FGroupList);
  FreeAndNil(FBGBitmap);
  FreeAndNil(FToolbarBG);
  FreeAndNil(FBGShape);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.AddGroup(Name: WideString; DisplaySeparator: Boolean;
  AlignRight: Boolean; InitialWidth: Integer): TToolbarItemGroup;

begin
  Result := TToolbarItemGroup.Create(self, Name, Width,
    DisplaySeparator, AlignRight, InitialWidth);

  FGroupList.Add(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetGroupIndex(Item: TToolbarItemGroup): Integer;

begin
  Result := FGroupList.IndexOf(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarContainer.SetBGBitmap(BGBitmap: TBitmap);

begin
  if (BGBitmap=nil) then
  begin
    if (FBGBitmap<>nil) then
    begin
      FBGBitmap.Free;
      FBGBitmap:=nil;
    end;
  end
  else
  begin
    FBGBitmap:=TBitmap.Create;
    FBGBitmap.Width:=BGBitmap.Width;
    FBGBitmap.Height:=BGBitmap.Height;
    FBGBitmap.Canvas.CopyRect(Rect(0, 0, BGBitmap.Width, BGBitmap.Height), BGBitmap.Canvas, Rect(0, 0, BGBitmap.Width,
      BGBitmap.Height));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarContainer.DoPaintBG(Sender: TObject);

var
  Details: TThemedElementDetails;
  
begin
  // If a background image was given then use this. Otherwise paint a themed background if the application is themed.
  if (FToolbarBG <> nil) and (FBGBitmap <> nil) then
    FToolbarBG.Canvas.StretchDraw(Rect(0, 0, FToolbarBG.Width, FToolbarBG.Height), FBGBitmap)
  else
    if ThemeServices.ThemesEnabled then
    begin
      Details := ThemeServices.GetElementDetails(trRebarRoot);
      ThemeServices.DrawElement(FToolbarBG.Canvas.Handle, Details, FToolbarBG.ClientRect);
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetWidth: integer;

var
  I: integer;

begin
  Result := FRightOffset;
  for I:=0 to FGroupList.Count-1 do
    if TToolbarItemGroup(FGroupList[I]).Visible then
      Result := Result + TToolbarItemGroup(FGroupList[I]).Width;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetGroup(const Name: WideString): TToolbarItemGroup;

var
  I: integer;

begin
  Result := nil;

  for I:=0 to FGroupList.Count-1 do
    if CompareText(TToolbarItemGroup(FGroupList[i]).FName, Name)=0 then
    begin
      Result := TToolbarItemGroup(FGroupList[i]);
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetGroupByIndex(Index: Integer): TToolbarItemGroup;

begin
  Result := TToolbarItemGroup(FGroupList[Index])
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetItem(const Name: WideString): TToolbarItem;

var
  I, J: integer;
  Group, Item: WideString;

begin
  Result := nil;
  if Pos('.', Name)>0 then
  begin
    Group := Copy(Name, 1, Pos('.', Name)-1);
    Item := Copy(Name, Pos('.', Name)+1, Length(Name));
  end
  else
  begin
    Group := '';
    Item := Name;
  end;

  for I:=0 to FGroupList.Count-1 do
    if ((CompareText(TToolbarItemGroup(FGroupList[I]).FName, Group)=0) or
      (CompareText(Group, '')=0)) then
    begin
      for J:=0 to TToolbarItemGroup(FGroupList[I]).FItemList.Count-1 do
        if CompareText(TToolbarItem(
          TToolbarItemGroup(FGroupList[I]
            ).FItemList[J]).FName, Item)=0 then
        begin
          Result:=TToolbarItem(
            TToolbarItemGroup(FGroupList[I]
              ).FItemList[J]);

          Break;
        end;

      if Assigned(Result) then
        Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetItemEnabled(const Name: WideString): Boolean;

var
  ToolbarItem: TToolbarItem;

begin
  ToolbarItem := GetItem(Name);

  Result:=ToolbarItem.Enabled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarContainer.SetItemEnabled(const Name: WideString; Enabled: Boolean);

var
  I, J: integer;
  Group, Item: WideString;

begin
  if Pos('.', Name)>0 then
  begin
    Group := Copy(Name, 1, Pos('.', Name)-1);
    Item := Copy(Name, Pos('.', Name)+1, Length(Name));
  end
  else
  begin
    Group := '';
    Item := Name;
  end;

  for I:=0 to FGroupList.Count-1 do
    if ((CompareText(TToolbarItemGroup(FGroupList[I]).FName, Group)=0) or
      (CompareText(Group, '')=0)) then
    begin
      for J:=0 to TToolbarItemGroup(FGroupList[I]).FItemList.Count-1 do
        if CompareText(TToolbarItem(
          TToolbarItemGroup(FGroupList[I]
            ).FItemList[J]).FName, Item)=0 then
        begin
          TToolbarItem(
            TToolbarItemGroup(FGroupList[I]
              ).FItemList[J]).Enabled := Enabled;

          if not(CompareText(Group, '')=0) then
            break;
        end;

      if not(CompareText(Group, '')=0) then
        break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetItemDown(const Name: WideString): Boolean;

var
  ToolbarItem: TToolbarItem;

begin
  ToolbarItem := GetItem(Name);
  Result := Assigned(ToolbarItem) and ToolbarItem.Down;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarContainer.SetItemDown(const Name: WideString; Down: Boolean);

var
  I, J: integer;
  Group, Item: WideString;

begin
  if Pos('.', Name)>0 then
  begin
    Group := Copy(Name, 1, Pos('.', Name)-1);
    Item := Copy(Name, Pos('.', Name)+1, Length(Name));
  end
  else
  begin
    Group := '';
    Item := Name;
  end;

  for I:=0 to FGroupList.Count-1 do
    if ((CompareText(TToolbarItemGroup(FGroupList[I]).FName, Group)=0) or
      (CompareText(Group, '')=0)) then
    begin
      for J:=0 to TToolbarItemGroup(FGroupList[I]).FItemList.Count-1 do
        if CompareText(TToolbarItem(
          TToolbarItemGroup(FGroupList[I]
            ).FItemList[J]).FName, Item)=0 then
        begin
          TToolbarItem(
            TToolbarItemGroup(FGroupList[I]
              ).FItemList[J]).Down := Down;

          if not(CompareText(Group, '')=0) then
            Break;
        end;

      if not(CompareText(Group, '')=0) then
        Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarContainer.GetGroupCount: Integer;

begin
  Result := FGroupList.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarContainer.SetOnDragOver(OnDragOver: TDragOverEvent);

begin
  FOnDragOver := OnDragOver;

  if FToolbarBG<>nil then
    FToolbarBG.OnDragOver := OnDragOver
  else
    if FBGShape<>nil then
      FBGShape.OnDragOver := OnDragOver;
end;

//----------------- TToolbarItemGroup ----------------------------------------------------------------------------------

constructor TToolbarItemGroup.Create(ToolbarContainer: TToolbarContainer;
  Name: WideString; Left: integer;
  DisplaySeparator: Boolean;
  AlignRight: Boolean;
  InitialWidth: Integer);

begin
  inherited Create;

  FToolbarContainer := ToolbarContainer;
  FName := Name;
  FItemList := TObjectList.Create;

  FDisplaySeparator := DisplaySeparator;
  FAlignRight := AlignRight;

  FVisible := True;
  FSpace := 2;

  FInitialWidth := InitialWidth;

  if (AlignRight) then
  begin
    if (ToolbarContainer.RightPos <> 0) then
      ToolbarContainer.RightPos := ToolbarContainer.RightPos + 46;

    FLeft := ToolbarContainer.Toolbar.Width -
      ToolbarContainer.RightSpace - ToolbarContainer.RightPos;
  end
  else
    FLeft := Left;

  if(DisplaySeparator)then
  begin
    FSep := TTntBevel.Create(nil);
    FSep.Parent := ToolbarContainer.Toolbar;

    if (FToolbarContainer.Vertical) then
      FSep.Shape := bsTopLine
    else
      FSep.Shape := bsRightLine;

    if (Not(AlignRight)) then
    begin
      if (FToolbarContainer.Vertical) then
        FSep.Top := Left + 2
      else
        FSep.Left := Left + 2;
    end
    else
    begin
      if (FToolbarContainer.Vertical) then
        FSep.Top := ToolbarContainer.Toolbar.Height -
          ToolbarContainer.RightSpace - ToolbarContainer.RightPos - 4
      else
        FSep.Left := ToolbarContainer.Toolbar.Width -
          ToolbarContainer.RightSpace - ToolbarContainer.RightPos - 4;
      FSep.Anchors := [akRight];
    end;

    if (FToolbarContainer.Vertical) then
    begin
      FSep.Left := 2;
      FSep.Height := 2;
      FSep.Width := ToolbarContainer.Toolbar.Width - 2;
    end
    else
    begin
      FSep.Top := 2;
      FSep.Width := 2;
      FSep.Height := ToolbarContainer.Toolbar.Height - 6;
    end;
  end
  else
    FSep:=nil;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TToolbarItemGroup.Destroy;

begin
  FItemList.Free;

  if(FSep<>nil)then
    FSep.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.AddItem(Name: WideString;
  ImageResource: WideString;
  Caption: WideString; Hint: WideString;
  Top: Integer;
  ShowLbl: Boolean; LblAtBottom: Boolean;
  AnchorRight: Boolean;
  OnBtnClick: TNotifyEvent;
  OnLblClick: TNotifyEvent;
  CustomControl: TWinControl;
  Enabled: Boolean; Down: Boolean;
  NoSpace: Boolean;
  Action: TBasicAction): TToolbarItem;

begin
  if (ToolbarContainer.Vertical) then
    Result := TToolbarItem.Create(self,
      name,
      ImageResource,
      Caption, Hint,
      Top, Left + Width,
      ShowLbl, LblAtBottom,
      AnchorRight,
      OnBtnClick,
      OnLblClick,
      CustomControl,
      NoSpace)
  else
    Result := TToolbarItem.Create(self,
      name,
      ImageResource,
      Caption, Hint,
      Left + Width, Top,
      ShowLbl, LblAtBottom,
      AnchorRight,
      OnBtnClick,
      OnLblClick,
      CustomControl,
      NoSpace);

  FItemList.Add(Result);

  if(AlignRight)then
  begin
    if (ToolbarContainer.Vertical) then
    begin
      Left := Left - Result.Height;

      ToolbarContainer.RightPos :=
        ToolbarContainer.RightPos + Result.Height;
    end
    else
    begin
      Left := Left - Result.Width;

      ToolbarContainer.RightPos :=
        ToolbarContainer.RightPos + Result.Width;
    end;
  end;

  if FVisible=False then
    Result.Visible := False;

  Result.Enabled := Enabled;

  Result.Down := Down;

  Result.Action := Action;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItemGroup.SetLeft(Left: integer);

var
  I: integer;
  Offset: integer;

begin
  Offset := (Left-FLeft);

  for I:=0 to FItemList.Count-1 do
    TToolbarItem(FItemList[I]).Left :=
      TToolbarItem(FItemList[I]).Left + Offset;

  if Assigned(FSep) then
    FSep.Left := FSep.Left + Offset;

  FLeft := Left;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.GetWidth: integer;

var
  I: integer;

begin
  if(FDisplaySeparator)then
    Result := FInitialWidth + 5
  else
    Result := FInitialWidth;

  for I:=0 to FItemList.Count-1 do
    Result := Result + TToolbarItem(FItemList[I]).Width;

  Result := Result + FSpace * FItemList.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.GetItem(const Name: WideString): TToolbarItem;

var
  I: integer;

begin
  Result := nil;

  for I:=0 to FItemList.Count-1 do
    if CompareText(TToolbarItem(FItemList[I]).FName, Name)=0 then
    begin
      Result := TToolbarItem(FItemList[I]);
      Break;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItemGroup.SetVisible(Visible: Boolean);

var
  I: integer;
  PrevSetting: Boolean;

begin
  PrevSetting := FVisible;
  FVisible := Visible;

  if(PrevSetting<>FVisible)then
    for I:=FToolbarContainer.FGroupList.IndexOf(self)+1 to
      FToolbarContainer.FGroupList.Count-1 do
      TToolbarItemGroup(FToolbarContainer.FGroupList[I]).Left:=
        TToolbarItemGroup(FToolbarContainer.FGroupList[I]).Left+
          Width*(Ord(FVisible)*2-1);

  for I:=0 to FItemList.Count-1 do
    TToolbarItem(FItemList[I]).Visible := FVisible;

  if (FSep <> nil) then
    FSep.Visible := Visible;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItemGroup.SetEnabled(Enabled: Boolean);

var
  I: integer;

begin
  FEnabled := Enabled;

  for I:=0 to FItemList.Count-1 do
    TToolbarItem(FItemList[I]).Enabled := FEnabled;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.GetItemCount: Integer;

begin
  Result := FItemList.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.GetItemByIndex(Index: Integer): TToolbarItem;

begin
  if (Index >= 0) and (Index < FItemList.Count) then
    Result := TToolbarItem(FItemList[Index])
  else
    Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemGroup.GetItemIndex(Item: TToolbarItem): Integer;

begin
  Result := FItemList.IndexOf(Item);
end;

//----------------- TToolbarItem ---------------------------------------------------------------------------------------

constructor TToolbarItem.Create(ToolbarItemGroup: TToolbarItemGroup;
  Name: WideString;
  ImageResource: WideString;
  Caption: WideString; Hint: WideString;
  Left: Integer; Top: Integer;
  ShowLbl: Boolean; LblAtBottom: Boolean;
  AnchorRight: Boolean;
  OnBtnClick: TNotifyEvent;
  OnLblClick: TNotifyEvent;
  CustomControl: TWinControl;
  NoSpace: Boolean);
begin
  inherited Create;

  FName := Name;
  FToolbarItemGroup := ToolbarItemGroup;
  FVisible := True;
  FLblAtBottom := LblAtBottom;
  FCustomControl := CustomControl;
  FNoSpace := NoSpace;

  if (ImageResource<>'') then
  begin
    FPNGBtn:=TPNGButton.Create(self, ImageResource);
    FPNGBtn.Parent:=ToolbarItemGroup.ToolbarContainer.Toolbar;
    FPNGBtn.ToolbarItem:=self;
    FPNGBtn.Left:=Left;
    FPNGBtn.Top:=Top;
    if(AnchorRight)then
      FPNGBtn.Anchors:=[akTop, akRight];

    if(Hint<>'')then
    begin
      FPNGBtn.Hint:=Hint;
      FPNGBtn.ShowHint:=True;
    end;

    FPNGBtn.OnBtnClick:=OnBtnClick;
  end
  else
    FPNGBtn:=nil;

  if Assigned(CustomControl) then
  begin
    CustomControl.Parent := FToolbarItemGroup.ToolbarContainer.Toolbar;
    CustomControl.Left := Left;
  end;

  FBtnLbl:=nil;
  FLblDropdownImg:=nil;

  if(ShowLbl)then
  begin
    FBtnLbl:=TTntLabel.Create(nil);
    FBtnLbl.Parent:=ToolbarItemGroup.ToolbarContainer.Toolbar;
    FBtnLbl.Font.Name:='Tahoma';
    FBtnLbl.Font.Height:=11;
    FBtnLbl.Caption:=Caption;
    FBtnLbl.Transparent:=True;

    if(FPNGBtn<>nil)then
    begin
      if(LblAtBottom)then
      begin
        FBtnLbl.Left:=FPNGBtn.Left+(FPNGBtn.Width-FBtnLbl.Width) div 2;
        FBtnLbl.Top:=FPNGBtn.Top+FPNGBtn.Height+1;
      end
      else
      begin
        FBtnLbl.Left:=FPNGBtn.Left+FPNGBtn.Width+6;
        FBtnLbl.Top:=FPNGBtn.Top+(FPNGBtn.Height-FBtnLbl.Height) div 2;
      end;
    end
    else
    begin
      FBtnLbl.Left:=Left-4;
      FBtnLbl.Top:=Top;
    end;

    if(AnchorRight)then
      FBtnLbl.Anchors:=[akTop, akRight];


    if(Hint<>'')then
    begin
      FBtnLbl.Hint:=Hint;
      FBtnLbl.ShowHint:=True;
    end;

    //Add dropdown
    if(Assigned(OnLblClick))then
    begin
      FBtnLbl.OnClick:=OnLblClick;

      FBtnLbl.AutoSize:=False;
      FLblDropdownImg:=TTntImage.Create(nil);
      FLblDropdownImg.Parent:=ToolbarItemGroup.ToolbarContainer.Toolbar;
      FLblDropdownImg.Left:=FBtnLbl.Left+FBtnLbl.Width+2;
      FLblDropdownImg.Top:=FBtnLbl.Top+FBtnLbl.Height-6;
      FLblDropdownPngImg:=LoadPNGImageFromResource('label_extra', FLblDropdownImg);
      FLblDropdownDisabledPngImg:=LoadPNGImageFromResource('label_extra_disabled');

      if(AnchorRight)then
        FLblDropdownImg.Anchors:=[akTop, akRight];

      FBtnLbl.Width:=FBtnLbl.Width+7;
      FBtnLbl.BringToFront;
    end
    else
    begin
      FLblDropdownPngImg:=nil;
      FLblDropdownDisabledPngImg:=nil;
      FBtnLbl.OnClick:=OnBtnClick;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TToolbarItem.Destroy;

begin
  FreeAndNil(FLblDropdownPngImg);
  FreeAndNil(FLblDropdownDisabledPngImg);
  FreeAndNil(FBtnLbl);
  FreeAndNil(FLblDropdownImg);

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItem.GetEnabled: Boolean;

begin
  Result:=FEnabled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.SetEnabled(Enabled: Boolean);

begin
  FEnabled:=Enabled;
  if (FPNGBtn<>nil) then
    FPNGBtn.Enabled := Enabled;

  if (Assigned(FBtnLbl)) then
    FBtnLbl.Enabled := Enabled;

  if (Assigned(FCustomControl)) then
    FCustomControl.Enabled := Enabled;

  if (FLblDropdownPngImg<>nil) then
  begin
    if(FEnabled)then
      FLblDropdownImg.Picture.Assign(FLblDropdownPngImg)
    else
      FLblDropdownImg.Picture.Assign(FLblDropdownDisabledPngImg);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItem.GetDown: Boolean;

begin
  Result := FDown;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.SetDown(Down: Boolean);

begin
  FDown := Down;

  if FPNGBtn<>nil  then
    FPNGBtn.Down := FDown;
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.SetVisible(Visible: Boolean);

begin
  FVisible:=Visible;

  if(FPNGBtn<>nil)then
    FPNGBtn.Visible:=FVisible;

  if(FBtnLbl<>nil)then
    FBtnLbl.Visible:=FVisible;

  if Assigned(FCustomControl) then
    FCustomControl.Visible := FVisible;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.SetLeft(Left: integer);

var
  Offset: integer;

begin
  Offset := (Left - FLeft);

  if (FPNGBtn<>nil) then
    FPNGBtn.Left := FPNGBtn.Left + Offset;

  if (FBtnLbl<>nil) then
    FBtnLbl.Left := FBtnLbl.Left + Offset;

  if (FLblDropdownImg<>nil) then
    FLblDropdownImg.Left := FLblDropdownImg.Left + Offset;

  if Assigned(FCustomControl) then
    FCustomControl.Left := FCustomControl.Left + Offset;

  FLeft:=Left;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItem.GetWidth: integer;

begin
  Result := 0;

  if (FPNGBtn<>nil) and (FNoSpace) then
    Result := FPNGBtn.Width
  else
    if (FPNGBtn<>nil) then
      Result := Result + 4 + FPNGBtn.Width + 4;

  if Assigned(FCustomControl) then
    Result := Result + 4 + FCustomControl.Width + 4;

  if (FBtnLbl<>nil) and not(FLblAtBottom) then
    Result := Result + FBtnLbl.Width + 8 * Ord(FPNGBtn<>nil);
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItem.GetHeight: integer;

begin
  Result := 0;

  if (FPNGBtn<>nil) and (FNoSpace) then
    Result := FPNGBtn.Height
  else
    if (FPNGBtn<>nil) then
      Result := Result + 4 + FPNGBtn.Height + 4;

  if Assigned(FCustomControl) then
    Result := Result + 4 + FCustomControl.Height + 4;

  if (FBtnLbl<>nil) and not(FLblAtBottom) then
    Result := Result + FBtnLbl.Height + 8 * Ord(FPNGBtn<>nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.SetAction(Value: TBasicAction);

begin
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
  end
  else
  begin
    FActionLink := TToolbarItemActionLink.Create(Self);
    FActionLink.Action := Value;
    FActionLink.OnChange := DoActionChange;
    ActionChange(Value, False);
    //Value.FreeNotification(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.DoActionChange(Sender: TObject);

begin
  if Sender = Action then ActionChange(Sender, False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TToolbarItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Enabled = True) then
        Self.Enabled := Enabled;
      if not CheckDefaults or (Self.Visible = True) then
        Self.Visible := Visible;
      if not CheckDefaults or not Assigned(Self.FPNGBtn.OnClick) then
        Self.FPNGBtn.OnClick := OnExecute;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItem.GetAction: TBasicAction;

begin
  if FActionLink <> nil then
    Result := FActionLink.Action else
    Result := nil;
end;

//----------------- TPNGButton -----------------------------------------------------------------------------------------

constructor TPNGButton.Create(Owner: TObject; ImgResourceName: WideString);

begin
  inherited Create(nil);

  FOwner := Owner;
  FEnabled:=True;
  FDown:=False;
  FClicked:=False;
  FMouseOver:=False;

  ControlStyle:=ControlStyle - [csOpaque];

  FButtonImg := LoadPNGImageFromResource(ImgResourceName, TTntImage(self));
  if (FButtonImg = nil) then
    raise Exception.Create(Format(
      _('The image %s cannot be found in the attached resources'),
      [ImgResourceName]));

  FButtonDownImg := LoadPNGImageFromResource(ImgResourceName + '_down', TTntImage(self));
  FButtonHighlightImg := LoadPNGImageFromResource(ImgResourceName + '_highlight', TTntImage(self));

  Width:=FButtonImg.Width;
  Height:=FButtonImg.Height;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TPNGButton.Destroy;

begin
  FButtonImg.Free;

  if (Assigned(FButtonDownImg)) then
    FButtonDownImg.Free;
  if (Assigned(FButtonHighlightImg)) then
    FButtonHighlightImg.Free;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure PerformEraseBackground(Control: TControl; DC: HDC);

var
  LastOrigin: TPoint;

begin
  GetWindowOrgEx(DC, LastOrigin);
  SetWindowOrgEx(DC, LastOrigin.X + Control.Left, LastOrigin.Y + Control.Top, nil);
  Control.Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(DC));
  SetWindowOrgEx(DC, LastOrigin.X, LastOrigin.Y, nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.Paint;

var
  DC: HDC;

begin
  if(Parent<>nil)then
  begin
    DC := GetDC(Parent.Handle);
    try
      // TODO: Need to make this class so that it can directly access the content of its parent window.
      // PerformEraseBackground(Self, DC);

      if(Not(Enabled))then
        FButtonImg.DrawPartialTrans(DC,
          Rect(Left, Top, FButtonImg.Width, FButtonImg.Height),
          200, True)
      else if(FDown)or(FClicked)then
      begin
        if (Assigned(FButtonDownImg)) then
          FButtonDownImg.DrawPartialTrans(DC,
            Rect(Left, Top, FButtonDownImg.Width, FButtonDownImg.Height))
        else
          FButtonImg.DrawPartialTrans(DC,
            Rect(Left, Top, FButtonImg.Width, FButtonImg.Height),
            0, False, 50);
      end
      else if(FMouseOver)then
      begin
        if (Assigned(FButtonHighlightImg)) then
          FButtonHighlightImg.DrawPartialTrans(DC,
            Rect(Left, Top, FButtonHighlightImg.Width, FButtonHighlightImg.Height))
        else
          FButtonImg.DrawPartialTrans(DC,
            Rect(Left, Top, FButtonImg.Width, FButtonImg.Height),
            0, False, 0, 50)
      end
      else
        FButtonImg.DrawPartialTrans(DC,
          Rect(Left, Top, FButtonImg.Width, FButtonImg.Height));
    finally
      ReleaseDC(Parent.Handle, DC);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.CMMouseLeave(var Msg: TMessage);

begin
  inherited;

  MouseLeave(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.MouseLeave(Sender: TObject);

begin
  if(Enabled)then
  begin
    FMouseOver:=False;

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  inherited MouseDown(Button, Shift, X, Y);

  if(Button=mbLeft)and(Enabled)then
  begin
    FClicked:=True;

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.MouseMove(Shift: TShiftState; X, Y: Integer);

begin
  inherited MouseMove(Shift, X, Y);

  if(FMouseOver=False)and(Enabled)then
  begin
    FMouseOver:=True;

    Invalidate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

begin
  inherited MouseUp(Button, Shift, X, Y);

  if FClicked then
  begin
    FClicked:=False;

    if((X<0)or(X>=Width)and
      (Y<0)or(Y>=Height))then
      FMouseOver:=False;

    Invalidate;
  end;

  if(Assigned(FOnBtnClick))then
    FOnBtnClick(self);

  {if (Owner is TToolbarItem) then
    if (Assigned(TToolbarItem(Owner).ActionLink)) then
      TToolbarItem(Owner).ActionLink.Execute(self);}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.ResetButton;

begin
  FDown:=False;
  FClicked:=False;
  FMouseOver:=False;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

function TPNGButton.GetEnabled: Boolean;

begin
  Result:=FEnabled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.SetEnabled(Enabled: Boolean);

begin
  FMouseOver:=False;

  self.FEnabled:=Enabled;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.DoMouseLeave(Sender: TObject);

begin
  MouseLeave(nil);
end;

function TPNGButton.GetDown: Boolean;
begin
  Result:=FDown;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPNGButton.SetDown(Down: Boolean);

begin
  FDown:=Down;

  Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TToolbarItemActionLink }

procedure TToolbarItemActionLink.AssignClient(AClient: TObject);

begin
  FClient := AClient as TToolbarItem;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsAutoCheckLinked: Boolean;

begin
  Result := (Action as TCustomAction).AutoCheck;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsCaptionLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsCheckedLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsEnabledLinked: Boolean;

begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsHelpContextLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsHintLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsGroupIndexLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsImageIndexLinked: Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsShortCutLinked: Boolean;
begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TToolbarItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := True;
end;

function TToolbarItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and
    (@FClient.FPNGBtn.OnBtnClick = @Action.OnExecute);
end;

procedure TToolbarItemActionLink.SetAutoCheck(Value: Boolean);
begin
  //
end;

procedure TToolbarItemActionLink.SetCaption(const Value: string);
begin
  //
end;

procedure TToolbarItemActionLink.SetChecked(Value: Boolean);
begin
  //
end;

procedure TToolbarItemActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TToolbarItemActionLink.SetHelpContext(Value: THelpContext);
begin
  //
end;

procedure TToolbarItemActionLink.SetHint(const Value: string);
begin
  //
end;

procedure TToolbarItemActionLink.SetImageIndex(Value: Integer);
begin
  //
end;

procedure TToolbarItemActionLink.SetShortCut(Value: TShortCut);
begin
  //
end;

procedure TToolbarItemActionLink.SetVisible(Value: Boolean);
begin
  //
end;

procedure TToolbarItemActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  if IsOnExecuteLinked then FClient.FPNGBtn.OnBtnClick := Value;
end;

end.
