unit Sections;

interface

uses Classes, Forms, SysUtils, Controls, ExtCtrls, Types, Graphics, Messages, Menus,
  ComCtrls, AuxFuncs, PNGImage, MyxError, TntForms, TntComCtrls, TntExtCtrls,
  TntMenus, Contnrs, VirtualTrees;

type
  TSectionForm2 = class;

  TCreateSectionForm2Function =
    function (AOwner: TComponent; SidebarSectionType: integer): TSectionForm2 of object;

  //All individual Forms (which are triggered by the SidebarSections)
  //descend from this class so they can be docked to a TSectionPanel from the
  //MainForm more easily
  TSectionForm2 = class(TTntForm)
    constructor Create(AOwner: TComponent; StatusBar: TTntStatusBar = nil); reintroduce;
  private
    pDockedPanel: TTntPanel;
    pSubTreePanel: TTntPanel;

    pStatusBar: TTntStatusBar;

    PInitControls: Boolean;

    FOnProcessStart,
    FOnProcessEnd: TNotifyEvent;

    procedure SetDockedPanel(DockedPanel: TTntPanel);
    function GetDockedPanel: TTntPanel;

    procedure SetSubTreePanel(SubTreePnl: TTntPanel);
    function GetSubTreePanel: TTntPanel;

    procedure SetStatusBar(StatusBar: TTntStatusBar);
    function GetStatusBar: TTntStatusBar;
  protected
    procedure SetSubPageIndex(NewSubPageIndex: Integer); virtual;
    function GetSubPageIndex: Integer; virtual;

    function GetSubPageCount: integer; virtual;

    function GetSectionTitle: WideString; virtual;
    function GetSectionInfo: WideString; virtual;

    function GetSupportAdvancedOptions: Boolean; virtual;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); virtual;
    function GetAdvancedOptionsState: Boolean; virtual;

    function GetSupportApplyChangesBtn: Boolean; virtual;

    procedure DoProcessStart;
    procedure DoProcessEnd;
  public
    property InitControls: Boolean Read PInitControls Write PInitControls;
    property DockedPanel: TTntPanel Read GetDockedPanel Write SetDockedPanel;
    property SubTreePanel: TTntPanel Read GetSubTreePanel Write SetSubTreePanel;
    property StatusBar: TTntStatusBar Read GetStatusBar Write SetStatusBar;

    property SubPageCount: Integer Read GetSubPageCount;
    property SubPageIndex: Integer Read GetSubPageIndex Write SetSubPageIndex;
    property SectionTitle: WideString Read GetSectionTitle;
    property SectionInfo: WideString Read GetSectionInfo;
    property SupportAdvancedOptions: Boolean Read GetSupportAdvancedOptions;
    property AdvancedOptionsVisible: Boolean Write SetAdvancedOptionsVisibleState;
    property AdvancedOptionsState: Boolean Read GetAdvancedOptionsState;
    property SupportApplyChangesBtn: Boolean Read GetSupportApplyChangesBtn;
    property OnProcessStart: TNotifyEvent Read FOnProcessStart Write FOnProcessStart;
    property OnProcessEnd: TNotifyEvent Read FOnProcessEnd Write FOnProcessEnd;

    procedure InitializeControls; virtual;

    procedure SetStatusText(StatusText: WideString);

    function GetFormContentAsText: WideString; virtual;

    procedure BeforeSubPageIndexChange(SectionIndex: Integer); virtual;
    procedure AfterSubPageIndexChange(SectionIndex: Integer); virtual;
  end;

  //The Panel which will holds the docked SectionForms
  TSectionPanel2 = class(TTntPanel)
    constructor Create(AOwner: TComponent; DrawBorder: Boolean = True); reintroduce;
    procedure Paint; override;
    procedure DockWinControl(WinControl: TWinControl);
    //procedure WndProc(var Msg: TMessage); override;
  protected
    function GetBorderColor: TColor;
    procedure SetBorderColor(BorderColor: TColor);
  private
    XPStyleEnabled: Boolean;
    DrawBorder: Boolean;
    FBorderColor: TColor;
  published
    property BorderColor: TColor read GetBorderColor write SetBorderColor;
  end;

  TSidebarSection2 = class(TObject)
    constructor Create(SidebarSectionType: integer;
      IconNr: integer; DisabledIconNr: integer;
      NeedsConnection: Boolean;
      NeedsLocalhostConnection: Boolean;
      MenuItem: TTntMenuItem = nil;
      ShowSubTree: Boolean = False;
      SectionForm: TSectionForm2 = nil;
      SectionObject: TObject = nil);
  public
    SidebarSectionType: integer;
    SectionForm: TSectionForm2;
    ShowSubTree: Boolean;
    SectionObject: TObject;
    MenuItem: TTntMenuItem;

    IconNr: integer;
    DisabledIconNr: integer;
    NeedsConnection: Boolean;
    NeedsLocalhostConnection: Boolean;
  end;

  TSectionControls2 = class(TObject)
    constructor Create(AOwner: TComponent; CreateSectionForm: TCreateSectionForm2Function;
      ViewMenuItem: TTntMenuItem; SectionSidebarHidden: Boolean;
      SectionSidebarWidth: integer; TreeviewImages: TImageList;
      SectionCount: integer; EmbTarget: TSectionControls2; CreateControls: Boolean = True;
      ShowBlueOutline: Boolean = True);

    destructor Destroy; override;

    //function AddSection(SectionTitle: WideString; Section: TSidebarSection2): TSidebarSection2;

    //procedure AdminTreeViewChange(Sender: TObject; Node: TTreeNode);

    procedure HSplitPnlMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HSplitPnlMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSplitPnlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HSplitPnlResize(Sender: TObject);

    function AddViewMenuItem(Name, Caption: WideString; Tag: integer): TTntMenuItem;

    procedure DoViewMenuClick(Sender: TObject);

    procedure ShowSidebarSection(Section: TSidebarSection2);
    procedure HideCurrentSidebarSection;

    function GetCurrentSectionIndex: Integer;

    procedure HideSubTreePnl(Section: TSidebarSection2);
    procedure ShowSubTreePnl(Section: TSidebarSection2);
  private
    FOnCurrentSectionChanged,
    FOnCurrentSectionChanging: TNotifyEvent;

    FCurrentSidebarSection: TSidebarSection2;

    EmbeddingTarget: TSectionControls2;

    function GetCurrentSidebarSection: TSidebarSection2;
  public
    Owner: TComponent;

    SectionList: TObjectList;

    ViewMenuItem: TTntMenuItem;

    LeftPnl,
    SidebarPnl: TTntPanel;
    AdminTreeView: TVirtualStringTree;  // this treeview is managed from Main.pas
    SubTreeSplitter: TTntSplitter;
    SubTreePnl: TTntPanel;

    HSplitPnl: TTntPanel;
    HSplitImg: TTntImage;
    HSplitMX, HSplitSize: integer;
    HSplitLeftPNGImg, HSplitRightPNGImg: TPNGObject;
    SectionSidebarHidden: Boolean;
    SectionSidebarWidth: integer;

    DockPnl: TSectionPanel2;

    CreateSectionForm: TCreateSectionForm2Function;

    SectionCount: integer;

    CreateControls,
    ShowBlueOutline: Boolean;

    property OnCurrentSectionChanging: TNotifyEvent read FOnCurrentSectionChanging write FOnCurrentSectionChanging;
    property OnCurrentSectionChanged: TNotifyEvent read FOnCurrentSectionChanged write FOnCurrentSectionChanged;
    property CurrentSectionIndex: integer read GetCurrentSectionIndex;
    property CurrentSidebarSection: TSidebarSection2 read GetCurrentSidebarSection;
  end;

// MA 1.1 versions

  TSectionForm = class;

  TCreateSectionFormFunction = function (AOwner: TComponent; SidebarSectionType: integer): TSectionForm of object;

  //All individual Forms (which are triggered by the SidebarSections)
  //descend from this class so they can be docked to a TSectionPanel from the
  //MainForm more easily
  TSectionForm = class(TTntForm)
    constructor Create(AOwner: TComponent; StatusBar: TTntStatusBar = nil); reintroduce;
  private
    pDockedPanel: TTntPanel;
    pSubTreePanel: TTntPanel;

    pStatusBar: TTntStatusBar;

    PInitControls: Boolean;

    FOnProcessStart,
    FOnProcessEnd: TNotifyEvent;

    procedure SetDockedPanel(DockedPanel: TTntPanel);
    function GetDockedPanel: TTntPanel;

    procedure SetSubTreePanel(SubTreePnl: TTntPanel);
    function GetSubTreePanel: TTntPanel;

    procedure SetStatusBar(StatusBar: TTntStatusBar);
    function GetStatusBar: TTntStatusBar;
  protected
    procedure ActivateSection; virtual;
    procedure DeactivateSection; virtual;

    procedure SetSubPageIndex(NewSubPageIndex: Integer); virtual;
    function GetSubPageIndex: Integer; virtual;

    function GetSubPageCount: integer; virtual;

    function GetSectionTitle: WideString; virtual;
    function GetSectionInfo: WideString; virtual;

    function GetSupportAdvancedOptions: Boolean; virtual;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); virtual;
    function GetAdvancedOptionsState: Boolean; virtual;

    function GetSupportApplyChangesBtn: Boolean; virtual;

    procedure DoProcessStart;
    procedure DoProcessEnd;
  public
    property InitControls: Boolean Read PInitControls Write PInitControls;
    property DockedPanel: TTntPanel Read GetDockedPanel Write SetDockedPanel;
    property SubTreePanel: TTntPanel Read GetSubTreePanel Write SetSubTreePanel;
    property StatusBar: TTntStatusBar Read GetStatusBar Write SetStatusBar;

    property SubPageCount: Integer Read GetSubPageCount;
    property SubPageIndex: Integer Read GetSubPageIndex Write SetSubPageIndex;
    property SectionTitle: WideString Read GetSectionTitle;
    property SectionInfo: WideString Read GetSectionInfo;
    property SupportAdvancedOptions: Boolean Read GetSupportAdvancedOptions;
    property AdvancedOptionsVisible: Boolean Write SetAdvancedOptionsVisibleState;
    property AdvancedOptionsState: Boolean Read GetAdvancedOptionsState;
    property SupportApplyChangesBtn: Boolean Read GetSupportApplyChangesBtn;
    property OnProcessStart: TNotifyEvent Read FOnProcessStart Write FOnProcessStart;
    property OnProcessEnd: TNotifyEvent Read FOnProcessEnd Write FOnProcessEnd;

    procedure InitializeControls; virtual;

    procedure SetStatusText(StatusText: WideString);

    function GetFormContentAsText: WideString; virtual;

    procedure BeforeSubPageIndexChange(SectionIndex: Integer); virtual;
    procedure AfterSubPageIndexChange(SectionIndex: Integer); virtual;
  end;

  //The Panel which will holds the docked SectionForms
  TSectionPanel = class(TTntPanel)
    constructor Create(AOwner: TComponent; DrawBorder: Boolean = True); reintroduce;
    procedure Paint; override;
    procedure DockWinControl(WinControl: TWinControl);
    //procedure WndProc(var Msg: TMessage); override;
  protected
    function GetBorderColor: TColor;
    procedure SetBorderColor(BorderColor: TColor);
  private
    XPStyleEnabled: Boolean;
    DrawBorder: Boolean;
    FBorderColor: TColor;
  published
    property BorderColor: TColor read GetBorderColor write SetBorderColor;
  end;

  TSidebarSection = class(TObject)
  public
    SidebarSectionType: integer;
    SectionForm: TSectionForm;
    ShowSubTree: Boolean;
    SectionObject: TObject;
    MenuItem: TTntMenuItem;

    IconNr: integer;
    DisabledIconNr: integer;
    NeedsConnection: Boolean;
    NeedsLocalServer: Boolean;

    Disabled: Boolean; // Useful to show there *is* a section, you just can't use it (e.g. for unsupported server versions).

    constructor Create(SidebarSectionType, IconNr, DisabledIconNr: Integer; NeedsConnection, LocalServerOnly: Boolean;
      MenuItem: TTntMenuItem = nil; ShowSubTree: Boolean = False; SectionForm: TSectionForm = nil; SectionObject: TObject = nil;
      Disabled: Boolean = False);
    destructor Destroy; override;
  end;

  TSectionControls = class(TObject)
    constructor Create(AOwner: TComponent; CreateSectionForm: TCreateSectionFormFunction;
      ViewMenuItem: TTntMenuItem; SectionSidebarHidden: Boolean;
      SectionSidebarWidth: integer; TreeviewImages: TImageList;
      SectionCount: integer; CreateControls: Boolean = True;
      ShowBlueOutline: Boolean = True);

    destructor Destroy; override;

    function AddSection(SectionTitle: WideString; Section: TSidebarSection): TSidebarSection;

    procedure AdminTreeViewChange(Sender: TObject; Node: TTreeNode);

    procedure HSplitPnlMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HSplitPnlMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure HSplitPnlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure HSplitPnlResize(Sender: TObject);

    function AddViewMenuItem(Name, Caption: WideString; Tag: integer): TTntMenuItem;

    procedure DoViewMenuClick(Sender: TObject);

    procedure ShowSidebarSection(Section: TSidebarSection);
    function GetCurrentSectionIndex: Integer;

    procedure HideSubTreePnl(Section: TSidebarSection = nil);
    procedure ShowSubTreePnl(Section: TSidebarSection);

    procedure RefreshSidebarIcons(Connected, IsLocalServer, ServicesConfig: Boolean);
  private
    FOnCurrentSectionChanged,
    FOnCurrentSectionChanging: TNotifyEvent;

    FCurrentSidebarSection: TSidebarSection;

    function GetCurrentSidebarSection: TSidebarSection;
  public
    Owner: TComponent;

    SectionList: TObjectList;

    ViewMenuItem: TTntMenuItem;

    LeftPnl,
    SidebarPnl: TTntPanel;
    AdminTreeView: TTntTreeView;
    SubTreeSplitter: TTntSplitter;
    SubTreePnl: TTntPanel;

    HSplitPnl: TTntPanel;
    HSplitImg: TTntImage;
    HSplitMX, HSplitSize: integer;
    HSplitLeftPNGImg, HSplitRightPNGImg: TPNGObject;
    SectionSidebarHidden: Boolean;
    SectionSidebarWidth: integer;

    DockPnl: TSectionPanel;

    CreateSectionForm: TCreateSectionFormFunction;

    SectionCount: integer;

    CreateControls,
    ShowBlueOutline: Boolean;

    property OnCurrentSectionChanging: TNotifyEvent read FOnCurrentSectionChanging write FOnCurrentSectionChanging;
    property OnCurrentSectionChanged: TNotifyEvent read FOnCurrentSectionChanged write FOnCurrentSectionChanged;
    property CurrentSectionIndex: integer read GetCurrentSectionIndex;
    property CurrentSidebarSection: TSidebarSection read GetCurrentSidebarSection;
  end;

implementation

uses
  PNGTools;
  
//------------------------------------------------------------------------------

constructor TSectionForm2.Create(AOwner: TComponent; StatusBar: TTntStatusBar);
begin
  inherited Create(AOwner);

  InitForm(Self);

  DockedPanel := nil;
  SubTreePanel := nil;
  Self.StatusBar := StatusBar;
end;

procedure TSectionForm2.SetDockedPanel(DockedPanel: TTntPanel);
begin
  Self.pDockedPanel := DockedPanel;
end;

function TSectionForm2.GetDockedPanel: TTntPanel;
begin
  GetDockedPanel := pDockedPanel;
end;

procedure TSectionForm2.SetSubTreePanel(SubTreePnl: TTntPanel);
begin
  Self.pSubTreePanel := SubTreePnl;
end;

function TSectionForm2.GetSubTreePanel: TTntPanel;
begin
  GetSubTreePanel := pSubTreePanel;
end;

procedure TSectionForm2.SetStatusBar(StatusBar: TTntStatusBar);
begin
  Self.pStatusBar := StatusBar;
end;

function TSectionForm2.GetStatusBar: TTntStatusBar;
begin
  GetStatusBar := pStatusBar;
end;

procedure TSectionForm2.SetStatusText(StatusText: WideString);
begin
  if(StatusBar<>nil)then
  begin
    if(StatusBar.SimplePanel)then
      StatusBar.SimpleText := StatusText
    else if(StatusBar.Panels.Count>1)then
      StatusBar.Panels[1].Text := StatusText
    else if(StatusBar.Panels.Count>0)then
      StatusBar.Panels[0].Text := StatusText;
  end;
end;

procedure TSectionForm2.InitializeControls;
begin
  //
end;

function TSectionForm2.GetFormContentAsText: WideString;
begin
  Result := '';
end;

procedure TSectionForm2.SetSubPageIndex(NewSubPageIndex: Integer);
begin
  //
end;

function TSectionForm2.GetSubPageIndex: Integer;
begin
  Result := -1;
end;

function TSectionForm2.GetSubPageCount: integer;
begin
  Result := 0;
end;

function TSectionForm2.GetSectionTitle: WideString;
begin
  Result := '';
end;

function TSectionForm2.GetSectionInfo: WideString;
begin
  Result := '';
end;

function TSectionForm2.GetSupportAdvancedOptions: Boolean;
begin
  Result := False;
end;

procedure TSectionForm2.SetAdvancedOptionsVisibleState(State: Boolean);
begin
  //
end;

function TSectionForm2.GetAdvancedOptionsState: Boolean;
begin
  Result := False;
end;

function TSectionForm2.GetSupportApplyChangesBtn: Boolean;
begin
  Result := False;
end;

procedure TSectionForm2.BeforeSubPageIndexChange(SectionIndex: Integer);
begin
  //
end;

procedure TSectionForm2.AfterSubPageIndexChange(SectionIndex: Integer);
begin
  //
end;

procedure TSectionForm2.DoProcessStart;
begin
  if(Assigned(OnProcessStart))then
    OnProcessStart(Self);
end;

procedure TSectionForm2.DoProcessEnd;
begin
  if(Assigned(OnProcessEnd))then
    OnProcessEnd(Self);
end;


//------------------------------------------------------------------------------

constructor TSectionPanel2.Create(AOwner: TComponent; DrawBorder: Boolean);
begin
  inherited Create(AOwner);

  Self.DrawBorder := DrawBorder;

  if(DrawBorder)then
  begin
    XPStyleEnabled := GetXPStyleEnabled;

    if(Not(XPStyleEnabled))then
    begin
      BorderStyle := bsSingle;
      BevelOuter := bvNone;
    end;
  end
  else
  begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
  end;

  FullRepaint := False;

  FBorderColor := clHotLight;
end;

procedure TSectionPanel2.Paint;
var Rect: TRect;
begin
  Rect := GetClientRect;

  if(DrawBorder)then
  begin
    with Canvas do
    begin
      if(XPStyleEnabled)then
      begin
        Pen.Color := FBorderColor;
        Brush.Style := bsClear;
        Rectangle(Rect);

        {ThemeServices.DrawElement(Canvas.Handle,
          ThemeServices.GetElementDetails(ttTabItemHot), Rect);}
      end
      else
        inherited;
    end;
  end
  else
    inherited;
end;

procedure TSectionPanel2.DockWinControl(WinControl: TWinControl);
var SectionForm: TSectionForm2;
begin
  Visible := False;

  //Go back to Parent of Docked WinControl and if it's a TSectionForm2
  //set InitControls
  if(WinControl.Parent.InheritsFrom(TSectionForm2))then
  begin
    SectionForm := TSectionForm2(WinControl.Parent);
    SectionForm.InitControls := True;
  end
  else
    SectionForm := nil;

  try
    WinControl.Parent := Self;
    //Get new Handles for all docked controls
    InitHandles(WinControl);
    //Resize WinControl
    WinControl.Perform(WM_SIZE, 0, 0);
  finally
    if(SectionForm<>nil)then
      SectionForm.InitControls := False;
  end;

  Visible := True;
end;

{procedure TSectionPanel.WndProc(var Msg: TMessage);
begin
  if(Msg.Msg=WM_ERASEBKGND)then
    Msg.Result := 0
  else
    inherited WndProc(Msg);
end;}

function TSectionPanel2.GetBorderColor: TColor;
begin
  Result := FBorderColor;
end;

procedure TSectionPanel2.SetBorderColor(BorderColor: TColor);
begin
  FBorderColor := BorderColor;

  Paint;
end;

//------------------------------------------------------------------------------

constructor TSidebarSection2.Create(SidebarSectionType: integer;
  IconNr: integer; DisabledIconNr: integer;
  NeedsConnection: Boolean; NeedsLocalhostConnection: Boolean;
  MenuItem: TTntMenuItem = nil;
  ShowSubTree: Boolean = False;
  SectionForm: TSectionForm2 = nil;
  SectionObject: TObject = nil);
begin
  inherited Create;

  Self.SidebarSectionType := SidebarSectionType;
  Self.SectionForm := SectionForm;
  Self.ShowSubTree := ShowSubTree;
  Self.SectionObject := SectionObject;
  Self.MenuItem := MenuItem;
  Self.IconNr := IconNr;
  Self.DisabledIconNr := DisabledIconNr;
  Self.NeedsConnection := NeedsConnection;
  Self.NeedsLocalhostConnection := NeedsLocalhostConnection;
end;

//------------------------------------------------------------------------------

constructor TSectionControls2.Create(AOwner: TComponent; CreateSectionForm: TCreateSectionForm2Function;
  ViewMenuItem: TTntMenuItem; SectionSidebarHidden: Boolean;
  SectionSidebarWidth: integer; TreeviewImages: TImageList;
  SectionCount: integer; EmbTarget: TSectionControls2; CreateControls: Boolean;
  ShowBlueOutline: Boolean);

begin
  inherited Create;

  SectionList := TObjectList.Create;

  FCurrentSidebarSection := nil;

  Self.Owner := AOwner;
  Self.CreateSectionForm := CreateSectionForm;
  Self.ViewMenuItem := ViewMenuItem;
  Self.SectionSidebarHidden := SectionSidebarHidden;
  Self.SectionSidebarWidth := SectionSidebarWidth;
  Self.SectionCount := SectionCount;
  Self.CreateControls := CreateControls;

  FOnCurrentSectionChanged := nil;

  //CreateControls := false;

  if(EmbTarget = nil) then
  begin
    EmbeddingTarget := Self;
  end
  else
  begin
    EmbeddingTarget := EmbTarget;
    SubTreePnl := EmbTarget.SubTreePnl;
  end;


  if(CreateControls)then
  begin
    HSplitLeftPNGImg := LoadPNGImageFromResource('sizer_h_left');
    HSplitRightPNGImg := LoadPNGImageFromResource('sizer_h_right');

    LeftPnl := TTntPanel.Create(Owner);
    LeftPnl.Parent := TWinControl(Owner);
    LeftPnl.BevelOuter := bvNone;
    LeftPnl.Name := 'SidebarLeftPnl';
    LeftPnl.Caption := '';
    LeftPnl.Width := 3;

    LeftPnl.EnableAlign;
    LeftPnl.Align := alLeft;

    SidebarPnl := TTntPanel.Create(Owner);
    SidebarPnl.Parent := TWinControl(Owner);
    SidebarPnl.BevelOuter := bvNone;
    SidebarPnl.Name := 'SidebarPnl';
    SidebarPnl.Caption := '';
    SidebarPnl.Align := alLeft;
    SidebarPnl.Width := 185;

    AdminTreeView := TVirtualStringTree.Create(Owner);
    AdminTreeView.Parent := SidebarPnl;
    AdminTreeView.Name := 'AdminTreeView';
    AdminTreeView.Align := alTop;//alClient;
    AdminTreeView.Height := 247;
    AdminTreeView.Images := TreeviewImages;

    SubTreeSplitter := TTntSplitter.Create(Owner);
    SubTreeSplitter.Parent := SidebarPnl;
    SubTreeSplitter.Name := 'SubTreeSplitter';
    SubTreeSplitter.Align := alTop;
    SubTreeSplitter.Height := 3;
    SubTreeSplitter.Visible := True;

    if(EmbeddingTarget = Self)then
    begin
      SubTreePnl := TTntPanel.Create(Owner);
      SubTreePnl.Parent := SidebarPnl;
      SubTreePnl.Name := 'SubTreePnl';
      SubTreePnl.Caption := '';
      SubTreePnl.Align := alClient;
      SubTreePnl.BevelOuter := bvNone;
      SubTreePnl.Visible := True;
    end;

    HSplitPnl := TTntPanel.Create(Owner);
    HSplitPnl.Parent := TWinControl(Owner);
    HSplitPnl.Name := 'HSplitPnl';
    HSplitPnl.Caption := '';
    HSplitPnl.Align := alLeft;
    HSplitPnl.BevelOuter := bvNone;
    HSplitPnl.Cursor := crHSplit;
    HSplitPnl.Width := 7;
    HSplitPnl.OnResize := HSplitPnlResize;
    HSplitPnl.OnMouseDown := HSplitPnlMouseDown;
    HSplitPnl.OnMouseMove := HSplitPnlMouseMove;
    HSplitPnl.OnMouseUp := HSplitPnlMouseUp;

    HSplitImg := TTntImage.Create(Owner);
    HSplitImg.Parent := HSplitPnl;
    HSplitImg.Name := 'HSplitImg';
    HSplitImg.Height := 41;
    HSplitImg.Left := 1;
    HSplitImg.Width := 5;
    HSplitImg.Top := 214;
    HSplitImg.OnMouseDown := HSplitPnlMouseDown;
    HSplitImg.OnMouseMove := HSplitPnlMouseMove;
    HSplitImg.OnMouseUp := HSplitPnlMouseUp;

    SidebarPnl.Left := 0;
    LeftPnl.Left := 0;
  end
  else
  begin
    HSplitLeftPNGImg := nil;
    HSplitRightPNGImg := nil;
  end;

  if(EmbeddingTarget = Self)then
  begin
    DockPnl := TSectionPanel2.Create(Owner, ShowBlueOutline);
    DockPnl.Parent := TWinControl(Owner);
    DockPnl.Name := 'DockPnl';
    DockPnl.Caption := '';
    DockPnl.Align := alClient;

    {im := TTntImage.Create(Owner);
    im.Parent := DockPnl;
    im.Picture.Graphic := LoadPNGImageFromResource('test_r', nil);
    im.Show;}
  end;

  if(CreateControls)then
    if(Not(SectionSidebarHidden))then
    begin
      HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
      SidebarPnl.Width := SectionSidebarWidth;
    end
    else
    begin
      HSplitImg.Picture.Graphic := HSplitRightPNGImg;
      LeftPnl.Width := 0;
      SidebarPnl.Width := 0;
    end;
end;

destructor TSectionControls2.Destroy;
begin
  if(HSplitLeftPNGImg<>nil)then
    HSplitLeftPNGImg.Free;
  if(HSplitRightPNGImg<>nil)then
    HSplitRightPNGImg.Free;

  SectionList.Free;

  inherited Destroy;
end;

{
function TSectionControls2.AddSection(SectionTitle: WideString; Section: TSidebarSection2): TSidebarSection2;
begin
  Result := Section;

  //Add section to sectionlist
  SectionList.Add(Section);

  if(ViewMenuItem<>nil)then
    AddViewMenuItem('Section'+IntToStr(SectionList.Count)+'MI', SectionTitle,
      SectionList.Count-1);

  if(CreateControls)then
  begin
    //Add a node for the section
    AddTreeViewChildNode(AdminTreeView, nil, SectionTitle,
      Section.IconNr,
      Section);
  end;
end;
}

{
procedure TSectionControls2.AdminTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if(Assigned(Node))then
    if(Assigned(Node.Data))then
      ShowSidebarSection(TSidebarSection2(Node.Data));
end;
}

procedure TSectionControls2.HSplitPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HSplitMX := Mouse.CursorPos.X;
  HSplitSize := SidebarPnl.Width;
end;

procedure TSectionControls2.HSplitPnlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var newWidth: integer;
begin
  if(ssLeft in Shift)then
  begin
    newWidth := HSplitSize+(Mouse.CursorPos.X-HSplitMX);

    if(newWidth<250)then
    begin
      if(newWidth>20)then
      begin
        LeftPnl.Width := 3;
        SidebarPnl.Width := newWidth;
        SectionSidebarHidden := False;
        HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
      end
      else
      begin
        LeftPnl.Width := 0;
        SidebarPnl.Width := 0;
        SectionSidebarHidden := True;
        HSplitImg.Picture.Graphic := HSplitRightPNGImg;
      end;
    end;
  end;
end;

procedure TSectionControls2.HSplitPnlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(HSplitMX=Mouse.CursorPos.X)then
  begin
    if(SidebarPnl.Width=0)then
    begin
      LeftPnl.Width := 3;
      SidebarPnl.Width := SectionSidebarWidth;
      SectionSidebarHidden := False;
      HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
    end
    else
    begin
      LeftPnl.Width := 0;
      SidebarPnl.Width := 0;
      SectionSidebarHidden := True;
      HSplitImg.Picture.Graphic := HSplitRightPNGImg;
    end;
  end
  else
  begin
    if(SidebarPnl.Width>0)then
      SectionSidebarWidth := SidebarPnl.Width;
  end;
end;

procedure TSectionControls2.HSplitPnlResize(Sender: TObject);
begin
  HSplitImg.Top := (HSplitPnl.Height-HSplitImg.Height) div 2;
end;

function TSectionControls2.AddViewMenuItem(Name, Caption: WideString; Tag: integer): TTntMenuItem;
var MenuItem: TTntMenuItem;
begin
  MenuItem := TTntMenuItem.Create(Owner);
  MenuItem.Name := Name;
  MenuItem.Caption := Caption;
  MenuItem.Tag := Tag;
  MenuItem.OnClick := DoViewMenuClick;
  MenuItem.OnAdvancedDrawItem := ViewMenuItem.OnAdvancedDrawItem;
  MenuItem.OnMeasureItem := ViewMenuItem.OnMeasureItem;
  ViewMenuItem.Add(MenuItem);

  Result := MenuItem;
end;

procedure TSectionControls2.DoViewMenuClick(Sender: TObject);
begin
  {if(CreateControls)then
  begin
    if(TTntMenuItem(Sender).Tag<AdminTreeView.Items.Count)then
      AdminTreeView.Selected := AdminTreeView.Items[TTntMenuItem(Sender).Tag];
  end
  else
  begin
    if(TTntMenuItem(Sender).Tag<SectionList.Count)then
      ShowSidebarSection(SectionList[TTntMenuItem(Sender).Tag] as TSideBarSection2);
  end;}
end;

procedure TSectionControls2.HideCurrentSidebarSection;

begin
  //hide current
  if(Assigned(FCurrentSidebarSection))then
  begin
    //Call SectionChanging Event
    if(Assigned(FOnCurrentSectionChanging))then
      FOnCurrentSectionChanging(Self);

    FCurrentSidebarSection.SectionForm.DockedPanel.Hide;
    if(FCurrentSidebarSection.SectionForm.SubTreePanel<>nil)then
      HideSubTreePnl(FCurrentSidebarSection);

    if(FCurrentSidebarSection.MenuItem<>nil)then
      FCurrentSidebarSection.MenuItem.Checked := False;
  end;

  FCurrentSidebarSection := nil;
end;

procedure TSectionControls2.ShowSidebarSection(Section: TSidebarSection2);

begin
  //hide current
  if(Assigned(FCurrentSidebarSection))then
  begin
    //Call SectionChanging Event
    if(Assigned(FOnCurrentSectionChanging))then
      FOnCurrentSectionChanging(Self);

    FCurrentSidebarSection.SectionForm.DockedPanel.Hide;
    if(FCurrentSidebarSection.SectionForm.SubTreePanel<>nil)then
      HideSubTreePnl(FCurrentSidebarSection);
  end;

  if(Assigned(Section.SectionForm))then
  begin
    Section.SectionForm.DockedPanel.Show;
    if(Section.ShowSubTree)then
      ShowSubTreePnl(Section);
  end
  else
  begin
    Section.SectionForm := 
      CreateSectionForm(Owner, Section.SidebarSectionType);

    if(Section.SectionForm=nil)then
    begin
      FCurrentSidebarSection := nil;

      if(Assigned(FOnCurrentSectionChanged))then
        FOnCurrentSectionChanged(Self);

      Exit;
    end;

    if(Section.SectionForm.DockedPanel=nil)then
      raise EMyxError.Create('The form that should be docked has no dock panel.');

    EmbeddingTarget.DockPnl.DockWinControl(Section.SectionForm.DockedPanel);

    if(Assigned(Section.SectionForm.SubTreePanel))then
    begin
      Section.SectionForm.InitControls := True;
      try
        Section.SectionForm.SubTreePanel.Parent := EmbeddingTarget.SubTreePnl;
        Section.SectionForm.SubTreePanel.Align := alClient;
      finally
        Section.SectionForm.InitControls := False;
      end;
    end;

    //Dock panels from SectionForm
    if(Section.ShowSubTree)then
      ShowSubTreePnl(Section)
    else
      HideSubTreePnl(FCurrentSidebarSection);

      Section.SectionForm.InitializeControls;
  end;

  //Uncheck last View MenuItem
  if(FCurrentSidebarSection<>nil)then
    if(FCurrentSidebarSection.MenuItem<>nil)then
      FCurrentSidebarSection.MenuItem.Checked := False;
  //Check new current View MenuItem
  if(Section.MenuItem<>nil)then
    Section.MenuItem.Checked := True;

  FCurrentSidebarSection := Section;

  //Call SectionChanged Event
  if(Assigned(FOnCurrentSectionChanged))then
    FOnCurrentSectionChanged(Self);
end;

function TSectionControls2.GetCurrentSectionIndex: Integer;
begin
  Result := SectionList.IndexOf(FCurrentSidebarSection);
end;

procedure TSectionControls2.HideSubTreePnl(Section: TSidebarSection2);
begin
  //if(Not(CreateControls))then
  //  Exit;

  if(Assigned(Section))then
    if(Section.SectionForm.SubTreePanel<>nil)then
      Section.SectionForm.SubTreePanel.Hide;

  EmbeddingTarget.SubTreePnl.Hide;
  EmbeddingTarget.SubTreeSplitter.Hide;
  EmbeddingTarget.AdminTreeView.Align := alClient;
end;

procedure TSectionControls2.ShowSubTreePnl(Section: TSidebarSection2);
begin
  //if(Not(CreateControls))then
  //  Exit;
    
  if(Assigned(Section))then
    if(Section.SectionForm.SubTreePanel<>nil)then
    begin
      EmbeddingTarget.AdminTreeView.Align := alTop;
      EmbeddingTarget.AdminTreeView.Height := (SectionCount+1)*24;//250+24;
      EmbeddingTarget.SubTreePnl.Height := 192;
      //SubTreePnl.Visible := True;
      EmbeddingTarget.SubTreePnl.Show;
      //SubTreeSplitter.Top := 1000;
      //SubTreeSplitter.Show;

      Section.SectionForm.SubTreePanel.Show;
      Section.SectionForm.SubTreePanel.Align := alClient;
    end;
end;

{
procedure TSectionControls2.RefreshSidebarIcons(Connected: Boolean;
  ConnectedToLocalhost: Boolean);
var i: integer;
begin
  if(Assigned(AdminTreeView) and Assigned(AdminTreeView.Items))then
  begin
    for i := 0 to AdminTreeView.Items.Count-1 do
    begin
      if(AdminTreeView.Items[i].Data<>nil)then
      begin
        if(((TSidebarSection2(AdminTreeView.Items[i].Data).NeedsConnection)and
            (Not(Connected)))or
          ((TSidebarSection2(AdminTreeView.Items[i].Data).NeedsLocalhostConnection)and
            (Not(ConnectedToLocalhost))))then
        begin
          AdminTreeView.Items[i].ImageIndex := 
            TSidebarSection2(AdminTreeView.Items[i].Data).DisabledIconNr;
          AdminTreeView.Items[i].SelectedIndex := 
            TSidebarSection2(AdminTreeView.Items[i].Data).DisabledIconNr;
        end
        else
        begin
          AdminTreeView.Items[i].ImageIndex := 
            TSidebarSection2(AdminTreeView.Items[i].Data).IconNr;
          AdminTreeView.Items[i].SelectedIndex := 
            TSidebarSection2(AdminTreeView.Items[i].Data).IconNr;
        end;
      end;
    end;
    AdminTreeView.Refresh;
  end;
end;
}

function TSectionControls2.GetCurrentSidebarSection: TSidebarSection2;

begin
  if (FCurrentSidebarSection = nil) and (SectionList.Count>0) then
    Result := TSidebarSection2(SectionList[0])
  else
    Result := FCurrentSidebarSection;
end;

// MA 1.1 versions implementations

constructor TSectionForm.Create(AOwner: TComponent; StatusBar: TTntStatusBar);
begin
  inherited Create(AOwner);

  InitForm(Self);

  DockedPanel := nil;
  SubTreePanel := nil;
  Self.StatusBar := StatusBar;
end;

procedure TSectionForm.SetDockedPanel(DockedPanel: TTntPanel);
begin
  Self.pDockedPanel := DockedPanel;
end;

function TSectionForm.GetDockedPanel: TTntPanel;
begin
  GetDockedPanel := pDockedPanel;
end;

procedure TSectionForm.SetSubTreePanel(SubTreePnl: TTntPanel);
begin
  Self.pSubTreePanel := SubTreePnl;
end;

function TSectionForm.GetSubTreePanel: TTntPanel;
begin
  GetSubTreePanel := pSubTreePanel;
end;

procedure TSectionForm.SetStatusBar(StatusBar: TTntStatusBar);
begin
  Self.pStatusBar := StatusBar;
end;

function TSectionForm.GetStatusBar: TTntStatusBar;
begin
  GetStatusBar := pStatusBar;
end;

procedure TSectionForm.SetStatusText(StatusText: WideString);
begin
  if(StatusBar<>nil)then
  begin
    if(StatusBar.SimplePanel)then
      StatusBar.SimpleText := StatusText
    else if(StatusBar.Panels.Count>1)then
      StatusBar.Panels[1].Text := StatusText
    else if(StatusBar.Panels.Count>0)then
      StatusBar.Panels[0].Text := StatusText;
  end;
end;

procedure TSectionForm.InitializeControls;
begin
  //
end;

function TSectionForm.GetFormContentAsText: WideString;
begin
  Result := '';
end;

procedure TSectionForm.SetSubPageIndex(NewSubPageIndex: Integer);
begin
  //
end;

function TSectionForm.GetSubPageIndex: Integer;
begin
  Result := -1;
end;

function TSectionForm.GetSubPageCount: integer;
begin
  Result := 0;
end;

function TSectionForm.GetSectionTitle: WideString;
begin
  Result := '';
end;

function TSectionForm.GetSectionInfo: WideString;
begin
  Result := '';
end;

function TSectionForm.GetSupportAdvancedOptions: Boolean;
begin
  Result := False;
end;

procedure TSectionForm.SetAdvancedOptionsVisibleState(State: Boolean);
begin
end;

function TSectionForm.GetAdvancedOptionsState: Boolean;
begin
  Result := False;
end;

function TSectionForm.GetSupportApplyChangesBtn: Boolean;
begin
  Result := False;
end;

procedure TSectionForm.BeforeSubPageIndexChange(SectionIndex: Integer);
begin
end;

procedure TSectionForm.ActivateSection;
begin
end;

procedure TSectionForm.DeactivateSection;
begin
end;

procedure TSectionForm.AfterSubPageIndexChange(SectionIndex: Integer);
begin
end;

procedure TSectionForm.DoProcessStart;
begin
  if(Assigned(OnProcessStart))then
    OnProcessStart(Self);
end;

procedure TSectionForm.DoProcessEnd;
begin
  if(Assigned(OnProcessEnd))then
    OnProcessEnd(Self);
end;


//------------------------------------------------------------------------------

constructor TSectionPanel.Create(AOwner: TComponent; DrawBorder: Boolean);
begin
  inherited Create(AOwner);

  Self.DrawBorder := DrawBorder;

  if(DrawBorder)then
  begin
    XPStyleEnabled := GetXPStyleEnabled;

    if(Not(XPStyleEnabled))then
    begin
      BorderStyle := bsSingle;
      BevelOuter := bvNone;
    end;
  end
  else
  begin
    BorderStyle := bsNone;
    BevelOuter := bvNone;
  end;

  FullRepaint := False;

  FBorderColor := clHotLight;
end;

procedure TSectionPanel.Paint;
var Rect: TRect;
begin
  Rect := GetClientRect;

  if(DrawBorder)then
  begin
    with Canvas do
    begin
      if(XPStyleEnabled)then
      begin
        Pen.Color := FBorderColor;
        Brush.Style := bsClear;
        Rectangle(Rect);

        {ThemeServices.DrawElement(Canvas.Handle,
          ThemeServices.GetElementDetails(ttTabItemHot), Rect);}
      end
      else
        inherited;
    end;
  end
  else
    inherited;
end;

procedure TSectionPanel.DockWinControl(WinControl: TWinControl);
var SectionForm: TSectionForm;
begin
  Visible := False;

  //Go back to Parent of Docked WinControl and if it's a TSectionForm
  //set InitControls
  if(WinControl.Parent.InheritsFrom(TSectionForm))then
  begin
    SectionForm := TSectionForm(WinControl.Parent);
    SectionForm.InitControls := True;
  end
  else
    SectionForm := nil;

  try
    WinControl.Parent := Self;
    //Get new Handles for all docked controls
    InitHandles(WinControl);
    //Resize WinControl
    WinControl.Perform(WM_SIZE, 0, 0);
  finally
    if(SectionForm<>nil)then
      SectionForm.InitControls := False;
  end;

  Visible := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSectionPanel.GetBorderColor: TColor;

begin
  Result := FBorderColor;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSectionPanel.SetBorderColor(BorderColor: TColor);

begin
  FBorderColor := BorderColor;

  Paint;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSidebarSection.Create(SidebarSectionType: integer;
  IconNr: integer; DisabledIconNr: integer;
  NeedsConnection: Boolean; LocalServerOnly: Boolean;
  MenuItem: TTntMenuItem;
  ShowSubTree: Boolean;
  SectionForm: TSectionForm;
  SectionObject: TObject;
  Disabled: Boolean);

begin
  inherited Create;

  Self.SidebarSectionType := SidebarSectionType;
  Self.SectionForm := SectionForm;
  Self.ShowSubTree := ShowSubTree;
  Self.SectionObject := SectionObject;
  Self.MenuItem := MenuItem;
  Self.IconNr := IconNr;
  Self.DisabledIconNr := DisabledIconNr;
  Self.NeedsConnection := NeedsConnection;
  Self.NeedsLocalServer := LocalServerOnly;
  Self.Disabled := Disabled;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSidebarSection.Destroy;

begin
  SectionForm.Free;
  
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TSectionControls.Create(AOwner: TComponent; CreateSectionForm: TCreateSectionFormFunction;
  ViewMenuItem: TTntMenuItem; SectionSidebarHidden: Boolean;
  SectionSidebarWidth: integer; TreeviewImages: TImageList;
  SectionCount: integer; CreateControls: Boolean;
  ShowBlueOutline: Boolean);
begin
  inherited Create;

  SectionList := TObjectList.Create;

  FCurrentSidebarSection := nil;

  Self.Owner := AOwner;
  Self.CreateSectionForm := CreateSectionForm;
  Self.ViewMenuItem := ViewMenuItem;
  Self.SectionSidebarHidden := SectionSidebarHidden;
  Self.SectionSidebarWidth := SectionSidebarWidth;
  Self.SectionCount := SectionCount;
  Self.CreateControls := CreateControls;

  FOnCurrentSectionChanged := nil;

  if(CreateControls)then
  begin
    HSplitLeftPNGImg := LoadPNGImageFromResource('sizer_h_left');
    HSplitRightPNGImg := LoadPNGImageFromResource('sizer_h_right');

    LeftPnl := TTntPanel.Create(Owner);
    LeftPnl.Parent := TWinControl(Owner);
    LeftPnl.BevelOuter := bvNone;
    LeftPnl.Name := 'SidebarLeftPnl';
    LeftPnl.Caption := '';
    LeftPnl.Align := alLeft;
    LeftPnl.Width := 3;

    SidebarPnl := TTntPanel.Create(Owner);
    SidebarPnl.Parent := TWinControl(Owner);
    SidebarPnl.BevelOuter := bvNone;
    SidebarPnl.Name := 'SidebarPnl';
    SidebarPnl.Caption := '';
    SidebarPnl.Align := alLeft;
    SidebarPnl.Width := 185;

    AdminTreeView := TTntTreeView.Create(Owner);
    AdminTreeView.Parent := SidebarPnl;
    AdminTreeView.Name := 'AdminTreeView';
    AdminTreeView.Align := alClient;
    AdminTreeView.Height := 247;
    AdminTreeView.Images := TreeviewImages;
    AdminTreeView.HideSelection := False;
    AdminTreeView.ReadOnly := True;
    AdminTreeView.RightClickSelect := True;
    AdminTreeView.ShowLines := False;
    AdminTreeView.ShowRoot := False;
    AdminTreeView.OnChange := AdminTreeViewChange;

    SubTreeSplitter := TTntSplitter.Create(Owner);
    SubTreeSplitter.Parent := SidebarPnl;
    SubTreeSplitter.Name := 'SubTreeSplitter';
    SubTreeSplitter.Align := alTop;
    SubTreeSplitter.Height := 3;
    SubTreeSplitter.Visible := False;

    SubTreePnl := TTntPanel.Create(Owner);
    SubTreePnl.Parent := SidebarPnl;
    SubTreePnl.Name := 'SubTreePnl';
    SubTreePnl.Caption := '';
    SubTreePnl.Align := alClient;
    SubTreePnl.BevelOuter := bvNone;
    SubTreePnl.Visible := False;

    HSplitPnl := TTntPanel.Create(Owner);
    HSplitPnl.Parent := TWinControl(Owner);
    HSplitPnl.Name := 'HSplitPnl';
    HSplitPnl.Caption := '';
    HSplitPnl.Align := alLeft;
    HSplitPnl.BevelOuter := bvNone;
    HSplitPnl.Cursor := crHSplit;
    HSplitPnl.Width := 7;
    HSplitPnl.OnResize := HSplitPnlResize;
    HSplitPnl.OnMouseDown := HSplitPnlMouseDown;
    HSplitPnl.OnMouseMove := HSplitPnlMouseMove;
    HSplitPnl.OnMouseUp := HSplitPnlMouseUp;

    HSplitImg := TTntImage.Create(Owner);
    HSplitImg.Parent := HSplitPnl;
    HSplitImg.Name := 'HSplitImg';
    HSplitImg.Height := 41;
    HSplitImg.Left := 1;
    HSplitImg.Width := 5;
    HSplitImg.Top := 214;
    HSplitImg.OnMouseDown := HSplitPnlMouseDown;
    HSplitImg.OnMouseMove := HSplitPnlMouseMove;
    HSplitImg.OnMouseUp := HSplitPnlMouseUp;

    SidebarPnl.Left := 0;
    LeftPnl.Left := 0;
  end
  else
  begin
    HSplitLeftPNGImg := nil;
    HSplitRightPNGImg := nil;
  end;

  DockPnl := TSectionPanel.Create(Owner, ShowBlueOutline);
  DockPnl.Parent := TWinControl(Owner);
  DockPnl.Name := 'DockPnl';
  DockPnl.Caption := '';
  DockPnl.Align := alClient;

  if(CreateControls)then
    if(Not(SectionSidebarHidden))then
    begin
      HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
      SidebarPnl.Width := SectionSidebarWidth;
    end
    else
    begin
      HSplitImg.Picture.Graphic := HSplitRightPNGImg;
      LeftPnl.Width := 0;
      SidebarPnl.Width := 0;
    end;
end;

destructor TSectionControls.Destroy;
begin
  if(HSplitLeftPNGImg<>nil)then
    HSplitLeftPNGImg.Free;
  if(HSplitRightPNGImg<>nil)then
    HSplitRightPNGImg.Free;

  SectionList.Free;

  inherited Destroy;
end;

function TSectionControls.AddSection(SectionTitle: WideString; Section: TSidebarSection): TSidebarSection;
begin
  Result := Section;

  //Add section to sectionlist
  SectionList.Add(Section);

  if(ViewMenuItem<>nil)then
    AddViewMenuItem('Section'+IntToStr(SectionList.Count)+'MI', SectionTitle,
      SectionList.Count-1);

  if(CreateControls)then
  begin
    //Add a node for the section
    AddTreeViewChildNode(AdminTreeView, nil, SectionTitle,
      Section.IconNr,
      Section);
  end;
end;

procedure TSectionControls.AdminTreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if(Assigned(Node))then
    if(Assigned(Node.Data))then
      ShowSidebarSection(TSidebarSection(Node.Data));
end;

procedure TSectionControls.HSplitPnlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HSplitMX := Mouse.CursorPos.X;
  HSplitSize := SidebarPnl.Width;
end;

procedure TSectionControls.HSplitPnlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var newWidth: integer;
begin
  if(ssLeft in Shift)then
  begin
    newWidth := HSplitSize+(Mouse.CursorPos.X-HSplitMX);

    if(newWidth<250)then
    begin
      if(newWidth>20)then
      begin
        LeftPnl.Width := 3;
        SidebarPnl.Width := newWidth;
        SectionSidebarHidden := False;
        HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
      end
      else
      begin
        LeftPnl.Width := 0;
        SidebarPnl.Width := 0;
        SectionSidebarHidden := True;
        HSplitImg.Picture.Graphic := HSplitRightPNGImg;
      end;
    end;
  end;
end;

procedure TSectionControls.HSplitPnlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(HSplitMX=Mouse.CursorPos.X)then
  begin
    if(SidebarPnl.Width=0)then
    begin
      LeftPnl.Width := 3;
      SidebarPnl.Width := SectionSidebarWidth;
      SectionSidebarHidden := False;
      HSplitImg.Picture.Graphic := HSplitLeftPNGImg;
    end
    else
    begin
      LeftPnl.Width := 0;
      SidebarPnl.Width := 0;
      SectionSidebarHidden := True;
      HSplitImg.Picture.Graphic := HSplitRightPNGImg;
    end;
  end
  else
  begin
    if(SidebarPnl.Width>0)then
      SectionSidebarWidth := SidebarPnl.Width;
  end;
end;

procedure TSectionControls.HSplitPnlResize(Sender: TObject);
begin
  HSplitImg.Top := (HSplitPnl.Height-HSplitImg.Height) div 2;
end;

function TSectionControls.AddViewMenuItem(Name, Caption: WideString; Tag: integer): TTntMenuItem;
var MenuItem: TTntMenuItem;
begin
  MenuItem := TTntMenuItem.Create(Owner);
  MenuItem.Name := Name;
  MenuItem.Caption := Caption;
  MenuItem.Tag := Tag;
  MenuItem.OnClick := DoViewMenuClick;
  MenuItem.OnAdvancedDrawItem := ViewMenuItem.OnAdvancedDrawItem;
  MenuItem.OnMeasureItem := ViewMenuItem.OnMeasureItem;
  ViewMenuItem.Add(MenuItem);

  Result := MenuItem;
end;

procedure TSectionControls.DoViewMenuClick(Sender: TObject);
begin
  if(CreateControls)then
  begin
    if(TTntMenuItem(Sender).Tag<AdminTreeView.Items.Count)then
      AdminTreeView.Selected := AdminTreeView.Items[TTntMenuItem(Sender).Tag];
  end
  else
  begin
    if(TTntMenuItem(Sender).Tag<SectionList.Count)then
      ShowSidebarSection(SectionList[TTntMenuItem(Sender).Tag] as TSideBarSection);
  end;
end;

procedure TSectionControls.ShowSidebarSection(Section: TSidebarSection);
begin
  //hide current
  if(Assigned(FCurrentSidebarSection))then
  begin
    FCurrentSidebarSection.SectionForm.DeactivateSection;

    //Call SectionChanging Event
    if(Assigned(FOnCurrentSectionChanging))then
      FOnCurrentSectionChanging(Self);

    FCurrentSidebarSection.SectionForm.DockedPanel.Hide;
    if(FCurrentSidebarSection.SectionForm.SubTreePanel<>nil)then
      HideSubTreePnl(FCurrentSidebarSection);
  end;

  if(Assigned(Section.SectionForm))then
  begin
    Section.SectionForm.DockedPanel.Show;
    if(Section.ShowSubTree)then
      ShowSubTreePnl(Section);
  end
  else
  begin
    Section.SectionForm := CreateSectionForm(Owner, Section.SidebarSectionType);

    if(Section.SectionForm=nil)then
    begin
      FCurrentSidebarSection := nil;

      if(Assigned(FOnCurrentSectionChanged))then
        FOnCurrentSectionChanged(Self);

      Exit;
    end;

    if(Section.SectionForm.DockedPanel=nil)then
      raise EMyxError.Create('The form that should be docked has no dock panel.');
      
    DockPnl.DockWinControl(Section.SectionForm.DockedPanel);

    if(Assigned(Section.SectionForm.SubTreePanel))then
    begin
      Section.SectionForm.InitControls := True;
      try
        Section.SectionForm.SubTreePanel.Parent := SubTreePnl;
        Section.SectionForm.SubTreePanel.Align := alClient;
      finally
        Section.SectionForm.InitControls := False;
      end;
    end;

    //Dock panels from SectionForm
    if(Section.ShowSubTree)then
      ShowSubTreePnl(Section)
    else
      HideSubTreePnl;

    Section.SectionForm.InitializeControls;
  end;

  // Uncheck last View MenuItem.
  if Assigned(FCurrentSidebarSection) and Assigned(FCurrentSidebarSection.MenuItem) then
    FCurrentSidebarSection.MenuItem.Checked := False;

  // Check new current View MenuItem.
  if Assigned(Section.MenuItem) then
    Section.MenuItem.Checked := True;

  FCurrentSidebarSection := Section;
  if Assigned(FCurrentSidebarSection) then
    FCurrentSidebarSection.SectionForm.ActivateSection;

  //Call SectionChanged Event
  if Assigned(FOnCurrentSectionChanged) then
    FOnCurrentSectionChanged(Self);
end;

function TSectionControls.GetCurrentSectionIndex: Integer;
begin
  Result := SectionList.IndexOf(FCurrentSidebarSection);
end;

procedure TSectionControls.HideSubTreePnl(Section: TSidebarSection);
begin
  if(Not(CreateControls))then
    Exit;

  if(Assigned(Section))then
    if(Section.SectionForm.SubTreePanel<>nil)then
      Section.SectionForm.SubTreePanel.Hide;

  SubTreePnl.Hide;
  SubTreeSplitter.Hide;
  AdminTreeView.Align := alClient;
end;

procedure TSectionControls.ShowSubTreePnl(Section: TSidebarSection);
begin
  if(Not(CreateControls))then
    Exit;
    
  if(Assigned(Section))then
    if(Section.SectionForm.SubTreePanel<>nil)then
    begin
      AdminTreeView.Align := alTop;
      AdminTreeView.Height := (SectionCount+1)*24;//250+24;
      SubTreePnl.Height := 192;
      SubTreePnl.Visible := True;
      SubTreeSplitter.Top := 1000;
      SubTreeSplitter.Show;

      Section.SectionForm.SubTreePanel.Show;
      Section.SectionForm.SubTreePanel.Align := alClient;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSectionControls.RefreshSidebarIcons(Connected: Boolean; IsLocalServer, ServicesConfig: Boolean);

var
  I: Integer;
  Section: TSidebarSection;

begin
  for I := 0 to AdminTreeView.Items.Count - 1 do
  begin
    if Assigned(AdminTreeView.Items[I].Data) then
    begin
      Section := TSidebarSection(AdminTreeView.Items[I].Data);
      if (Section.NeedsConnection and not Connected) or
        Section.Disabled or
        (Section.NeedsLocalServer and not IsLocalServer and not ServicesConfig) then
      begin
        AdminTreeView.Items[I].ImageIndex := Section.DisabledIconNr;
        AdminTreeView.Items[I].SelectedIndex := Section.DisabledIconNr;
      end
      else
      begin
        AdminTreeView.Items[I].ImageIndex := Section.IconNr;
        AdminTreeView.Items[I].SelectedIndex := Section.IconNr;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSectionControls.GetCurrentSidebarSection: TSidebarSection;

begin
  if (FCurrentSidebarSection = nil) and (SectionList.Count>0) then
    Result := TSidebarSection(SectionList[0])
  else
    Result := FCurrentSidebarSection;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
