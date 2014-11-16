unit AdditionalClasses;

interface

uses Messages, Types, Forms, Menus, Classes, ExtCtrls, Graphics,
  Controls, SysUtils, ComCtrls, Contnrs, Themes, SyncObjs,
  AuxFuncs, myx_public_interface, MySQLConnection, TntClasses,
  TntExtCtrls, TntMenus;

type
  //MySQL User definition
  TMySQLUser = class(TObject)
    constructor Create(username, password: WideString; fullname: WideString = ''; description: WideString = '';
      idicon: integer = -1);
    destructor Destroy; override;
  public
    username,
    password,
    fullname,
    description: WideString;
    idicon: integer;

    hostList: TTntStringList;
  end;

  //TreeNodeData definition for Trees with Columns
  TIndexNode = class(TObject)
    constructor Create(NodeName: WideString; SubItems: WideString = '');
    destructor Distroy;

    function GetNodeName: WideString;
    procedure SetNodeName(NodeName: WideString);

    function GetSubItems: TTntStringList;
    procedure SetSubItems(SubItems: WideString);
  protected
    NodeName: WideString;

    SubItems: TTntStringList;
  end;

  //ProgressGraph used for Health pages
  TProgressGraph = class(TBitmap)
    constructor Create(width, height: integer;
      MinValue, MaxValue: Single); reintroduce; overload;

    function MoveToNextValue(value: Single): integer;
    procedure DrawProgressGraphTo(theCanvas: TCanvas; x, y, w, h: integer);
    procedure SetNewMaxMin(MinValue, MaxValue: Single);
    procedure ClearGraph;
  public
    RefreshCounter: integer;
  private
    CurrentPos: integer;
    Values: Array[0..1280] of Single;
    MinValue, MaxValue: Single;
    CurrentValue: integer;
    ValuesWraped: Boolean;
  end;

  //DeveloperNode definition
  TDevDocType = (DDTModel, DDTReport, DDTWebAppl);

  TDevDocNode = class(TObject)
    constructor Create(IdDevDoc: integer; DocName: WideString; DocType: TDevDocType; SubItems: WideString = '');
    destructor Distroy;

    function GetIdDevDoc: integer;

    function GetDocName: WideString;
    procedure SetDocName(DocName: WideString);

    function GetDocType: TDevDocType;

    function GetSubItems: TTntStringList;
    procedure SetSubItems(SubItems: WideString);
  protected
    IdDevDoc: integer;
    DocName: WideString;
    DocType: TDevDocType;

    SubItems: TTntStringList;
  end;

  //Classes used for Repl
  TReplicationState = (rsUnknown, rsRunning, rsStopped, rsWarning, rsError);

  TReplicationNode = class;
  TReplicationLink = class;

  TReplicationModel = class(TTntPaintBox)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddNode(Caption: WideString; NodeState: TReplicationState = rsUnknown; x: integer = 0; y: integer = 0): TReplicationNode;
    function LinkNodes(Master, Slave: TReplicationNode): TReplicationLink;

    procedure PaintLinks;
    procedure PaintNodes;

    procedure Paint; override;
  public
    NodeList,
    LinkList: TList;
  end;

  TReplicationNode = class(TTntPaintBox)
    constructor Create(AOwner: TComponent; Model: TReplicationModel; Caption: WideString; NodeState: TReplicationState = rsUnknown; x: integer = 0; y: integer = 0); reintroduce;
    destructor Destroy; override;

    procedure Paint; override;

    procedure SetNodeCaption(Caption: WideString);

    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DblClick; override;

    procedure AddSlaveToNode(Sender: TObject);
    procedure ActivateNode(Sender: TObject);
  private
    Model: TReplicationModel;

    NodeCaption: WideString;
    NodeState: TReplicationState;

    NodeActive: Boolean;

    DragStartPoint,
    DragMouseStart: TPoint;

    MasterLinks,
    SlaveLinks: TList;

    PopupMenu: TTntPopupMenu;
  end;

  TReplicationLinkDirection = (RLDTop, RLDRight, RLDBottom, RLDLeft);

  TReplicationLink = class(TObject)
    constructor Create(AOwner: TComponent; Model: TReplicationModel; Master: TReplicationNode; Slave: TReplicationNode); reintroduce;

    procedure AutoAlign(EraseBeforePaint: Boolean = True);
    procedure RefreshLinkDirection;

    procedure Paint;

  private
    Model: TReplicationModel;
    Master,
    Slave: TReplicationNode;

    LinkDirection: TReplicationLinkDirection;
    Left, Top, Width, Height: integer;
    dx, dy: double;
  end;

const
  hv_fac=1.3333333;

implementation

uses ApplicationDataModule;

//------------------------------------------------------------------------------

constructor TMySQLUser.Create(username, password: WideString; fullname: WideString = ''; description: WideString = '';
  idicon: integer = -1);
begin
  inherited Create;

  self.username:=username;
  self.password:=password;
  self.fullname:=fullname;
  self.description:=description;

  hostList:=TTntStringList.Create;
end;

destructor TMySQLUser.Destroy;
begin
  hostList.Free;

  inherited Destroy;
end;

constructor TProgressGraph.Create(width, height: integer;
  MinValue, MaxValue: Single);
begin
  inherited Create;

  self.Width:=width+2;
  self.Height:=height;

  self.MinValue:=MinValue;
  self.MaxValue:=MaxValue;
  if(MaxValue=0)then
    self.MaxValue:=0.1;

  refreshCounter:=0;
  currentPos:=0;

  CurrentValue:=-1;
  ValuesWraped:=False;

  ClearGraph;
end;

function TProgressGraph.MoveToNextValue(value: Single): integer;
var ypos: integer;
begin
  inc(CurrentValue);
  if(CurrentValue>=1280)then
  begin
    CurrentValue:=0;
    ValuesWraped:=True;
  end;

  //dummy value if MinValue-1 was submitted
  if(value=MinValue-1)then
    value:=Round(currentPos+Random*40-20);
  if(value<MinValue)then
    value:=MinValue
  else if(value>MaxValue)then
    value:=MaxValue;

  Values[CurrentValue]:=value;

  Canvas.CopyRect(Rect(0, 0, Width-3, Height-1), Canvas,
    Rect(2, 0, Width-1, Height-1));

  inc(RefreshCounter);
  if(RefreshCounter mod 6=0)then
  begin
    Canvas.Pen.Color:=$00801000;
    Canvas.MoveTo(Width-4, 0);
    Canvas.LineTo(Width-4, Height-1);
  end;
  //don't let RefreshCounter get too big
  if(RefreshCounter>1280+20)then
    RefreshCounter:=RefreshCounter-12;

  ypos:=Round(((value-MinValue)/MaxValue)*(Height-3));

  Canvas.Pen.Color:=$00FFA000;
  Canvas.MoveTo(Width-7, Height-2-currentPos);
  Canvas.LineTo(Width-5, Height-2-ypos);

  currentPos:=ypos;

  MoveToNextValue:=Round(((value-MinValue)/MaxValue)*100);
end;

procedure TProgressGraph.DrawProgressGraphTo(theCanvas: TCanvas; x, y, w, h: integer);
begin
  theCanvas.CopyRect(Rect(0, 0, w-1, h-1),
    Canvas,
    Rect(Width-w-2, Height-h, Width-2-2, Height-1));
end;

procedure TProgressGraph.ClearGraph;
var i: integer;
begin
  Canvas.Brush.Color:=clBlack;
  Canvas.FillRect(Rect(0, 0, self.Width-1, self.Height-1));

  Canvas.Pen.Color:=$00801000;
  for i:=0 to width div 12 do
  begin
    Canvas.MoveTo(width-4-i*12-(RefreshCounter*2) mod 12, 0);
    Canvas.LineTo(width-4-i*12-(RefreshCounter*2) mod 12, self.Height);
  end;
  for i:=1 to height div 12 do
  begin
    Canvas.MoveTo(0, i*12);
    Canvas.LineTo(self.Width, i*12);
  end;
end;

procedure TProgressGraph.SetNewMaxMin(MinValue, MaxValue: Single);
var i, xpos, ypos: integer;
begin
  self.MinValue:=MinValue;
  self.MaxValue:=MaxValue;

  {if(CurrentValue=-1)then
    exit;}

  ClearGraph;

  //Redraw scaled values
  currentPos:=Round(((values[CurrentValue]-MinValue)/MaxValue)*(Height-3));

  Canvas.Pen.Color:=$00FFA000;
  Canvas.MoveTo(Width-5, Height-2-currentPos);

  xpos:=Width-7;
  for i:=CurrentValue-1 downto 0 do
  begin
    ypos:=Round(((values[i]-MinValue)/MaxValue)*(Height-3));

    Canvas.LineTo(xpos, Height-2-ypos);

    dec(xpos, 2);
    if(xpos<=0)then
      break;
  end;

  if(ValuesWraped)and(xpos>0)then
    for i:=1280-1 downto CurrentValue+1 do
    begin
      ypos:=Round(((values[i]-MinValue)/MaxValue)*(Height-3));

      Canvas.LineTo(xpos, Height-2-ypos);

      dec(xpos, 2);
      if(xpos<=0)then
        break;
    end;


end;

//------------------------------------------------------------------------------

constructor TDevDocNode.Create(IdDevDoc: integer; DocName: WideString; DocType: TDevDocType; SubItems: WideString = '');
begin
  inherited Create;

  self.IdDevDoc:=IdDevDoc;
  self.DocName:=DocName;
  self.DocType:=DocType;

  self.SubItems:=TTntStringList.Create;
  self.SubItems.Text:=SubItems;
end;

destructor TDevDocNode.Distroy;
begin
  SubItems.Free;

  inherited;
end;

function TDevDocNode.GetIdDevDoc: integer;
begin
  GetIdDevDoc:=IdDevDoc;
end;

function TDevDocNode.GetDocName: WideString;
begin
  GetDocName:=DocName;
end;

procedure TDevDocNode.SetDocName(DocName: WideString);
begin
  self.DocName:=DocName;
end;

function TDevDocNode.GetDocType: TDevDocType;
begin
  GetDocType:=DocType;
end;

function TDevDocNode.GetSubItems: TTntStringList;
begin
  GetSubItems:=SubItems;
end;

procedure TDevDocNode.SetSubItems(SubItems: WideString);
begin
  self.SubItems.Text:=SubItems;
end;

//------------------------------------------------------------------------------

constructor TIndexNode.Create(NodeName: WideString; SubItems: WideString = '');
begin
  inherited Create;

  self.NodeName:=NodeName;

  self.SubItems:=TTntStringList.Create;
  self.SubItems.Text:=SubItems;
end;

destructor TIndexNode.Distroy;
begin
  SubItems.Free;

  inherited;
end;

function TIndexNode.GetNodeName: WideString;
begin
  GetNodeName:=NodeName;
end;

procedure TIndexNode.SetNodeName(NodeName: WideString);
begin
  self.NodeName:=NodeName;
end;

function TIndexNode.GetSubItems: TTntStringList;
begin
  GetSubItems:=SubItems;
end;

procedure TIndexNode.SetSubItems(SubItems: WideString);

begin
  self.SubItems.Text:=SubItems;
end;

//------------------------------------------------------------------------------

constructor TReplicationModel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  NodeList:=TList.Create;
  LinkList:=TList.Create;
end;

destructor TReplicationModel.Destroy;
begin
  NodeList.Free;
  LinkList.Free;

  inherited Destroy;
end;

function TReplicationModel.AddNode(Caption: WideString; NodeState: TReplicationState = rsUnknown; x: integer = 0; y: integer = 0): TReplicationNode;
var theReplNode: TReplicationNode;
begin
  theReplNode:=TReplicationNode.Create(Owner, self, Caption, NodeState, x, y);

  NodeList.Add(theReplNode);

  AddNode:=theReplNode;
end;

function TReplicationModel.LinkNodes(Master, Slave: TReplicationNode): TReplicationLink;
var theReplLink: TReplicationLink;
begin
  theReplLink:=TReplicationLink.Create(Owner, self, Master, Slave);

  Master.SlaveLinks.Add(theReplLink);
  Slave.MasterLinks.Add(theReplLink);

  LinkList.Add(theReplLink);

  LinkNodes:=theReplLink;
end;

procedure TReplicationModel.PaintNodes;
var i: integer;
begin
  for i:=0 to NodeList.Count-1 do
    TReplicationNode(NodeList[i]).Paint;
end;

procedure TReplicationModel.PaintLinks;
var i: integer;
begin
  for i:=0 to LinkList.Count-1 do
    TReplicationLink(LinkList[i]).AutoAlign;
end;

procedure TReplicationModel.Paint;
begin
  inherited;

  PaintLinks;
  PaintNodes;

  Canvas.Pen.Color:=clHotLight;
  Canvas.MoveTo(0, 0);
  Canvas.LineTo(Width-1, 0);
  Canvas.LineTo(Width-1, Height-1);
  Canvas.LineTo(0, Height-1);
  Canvas.LineTo(0, 0);
end;

//------------------------------------------------------------------------------

constructor TReplicationNode.Create(AOwner: TComponent; Model: TReplicationModel; Caption: WideString; NodeState: TReplicationState = rsUnknown; x: integer = 0; y: integer = 0);
var NewItem: TTntMenuItem;
begin
  inherited Create(AOwner);

  self.Parent:=TWinControl(AOwner);

  self.Model:=Model;

  PopupMenu:=TTntPopupMenu.Create(AOwner);

  NewItem := TTntMenuItem.Create(PopupMenu);
  PopupMenu.Items.Add(NewItem);
  NewItem.Caption:='Add Slave to Server';
  NewItem.Tag:=1;
  NewItem.OnClick:=AddSlaveToNode;
  
  NewItem := TTntMenuItem.Create(PopupMenu);
  PopupMenu.Items.Add(NewItem);
  NewItem.Caption:='Create Installer for Slave';

  NewItem := TTntMenuItem.Create(PopupMenu);
  PopupMenu.Items.Add(NewItem);
  NewItem.Caption:='Activate Node';
  NewItem.OnClick:=ActivateNode;

  NewItem := TTntMenuItem.Create(PopupMenu);
  PopupMenu.Items.Add(NewItem);
  NewItem.Caption:='-';

  NewItem := TTntMenuItem.Create(PopupMenu);
  PopupMenu.Items.Add(NewItem);
  NewItem.Caption:='Remove Slave';

  NodeActive:=True;

  MasterLinks:=TList.Create;
  SlaveLinks:=TList.Create;

  self.NodeState:=NodeState;

  self.Height:=64;
  SetNodeCaption(Caption);

  Left:=x;
  Top:=y;
end;

destructor TReplicationNode.Destroy;
begin
  MasterLinks.Free;
  SlaveLinks.Free;

  inherited;
end;

procedure TReplicationNode.SetNodeCaption(Caption: WideString);
var w: integer;
begin
  NodeCaption:=Caption;

  w:=Canvas.TextWidth(caption);
  if(w<48)then
    w:=48;

  Width:=w;

  Invalidate;
end;

procedure TReplicationNode.Paint;
var w: integer;
begin
  //Draw Img Centered
  if(NodeActive)then
    ApplicationDM.Admin48ImageList.Draw(Canvas, (Width-48) div 2, 0, 1)
  else
    ApplicationDM.Admin48ImageList.Draw(Canvas, (Width-48) div 2, 0, 2);

  w:=Canvas.TextWidth(NodeCaption);
  w:=Width div 2 - w div 2;

  Canvas.TextOut(w, 48, NodeCaption);
end;

procedure TReplicationNode.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  BringToFront;

  if(Button=mbLeft)then
  begin
    DragStartPoint:=Point(Left, Top);
    DragMouseStart:=Point(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end
  else
    PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TReplicationNode.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if(ssLeft	in Shift)then
  begin
    Left:=DragStartPoint.X+(Mouse.CursorPos.X-DragMouseStart.X);
    Top:=DragStartPoint.Y+(Mouse.CursorPos.Y-DragMouseStart.Y);

    Model.Paint;
  end;
end;

procedure TReplicationNode.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
end;

procedure TReplicationNode.DblClick;
begin
  inherited;


end;

procedure TReplicationNode.AddSlaveToNode(Sender: TObject);
var SlaveNode: TReplicationNode;
begin
  SlaveNode:=Model.AddNode('New Server', rsUnknown, Left+92, Top+64);
  SlaveNode.NodeActive:=False;
  Model.LinkNodes(self, SlaveNode);
end;

procedure TReplicationNode.ActivateNode(Sender: TObject);
begin
  NodeActive:=True;
  Invalidate;
end;

//------------------------------------------------------------------------------

constructor TReplicationLink.Create(AOwner: TComponent; Model: TReplicationModel; Master: TReplicationNode; Slave: TReplicationNode);
begin
  inherited Create;

  self.Model:=Model;
  self.Master:=Master;
  self.Slave:=Slave;

  AutoAlign(False);
end;

procedure TReplicationLink.AutoAlign(EraseBeforePaint: Boolean = True);
var MasterMiddle, SlaveMiddle: TPoint;
begin
  if(EraseBeforePaint)then
  begin
    Model.Canvas.Pen.Color:=clWhite;
    Model.Canvas.Brush.Color:=clWhite;
    Paint;
  end;

  RefreshLinkDirection;

  MasterMiddle.X:=Master.Left+Master.Width div 2;
  MasterMiddle.Y:=Master.Top+Master.Height div 2;
  SlaveMiddle.X:=Slave.Left+Slave.Width div 2;
  SlaveMiddle.Y:=Slave.Top+Slave.Height div 2;

  if(LinkDirection=RLDTop)then
  begin
    Left:=SlaveMiddle.X;
    Top:=Slave.Top+Slave.Height;
    Width:=MasterMiddle.X-SlaveMiddle.X;
    Height:=Master.Top-Top;
  end
  else if(LinkDirection=RLDLeft)then
  begin
    Left:=Slave.Left+Slave.Width;
    Top:=SlaveMiddle.Y;
    Width:=Master.Left-Left;
    Height:=MasterMiddle.Y-SlaveMiddle.Y;
  end
  else if(LinkDirection=RLDRight)then
  begin
    Left:=Master.Left+Master.Width;
    Top:=MasterMiddle.Y;
    Width:=Slave.Left-Left;
    Height:=SlaveMiddle.Y-MasterMiddle.Y;
  end
  else
  begin
    Left:=MasterMiddle.X;
    Top:=Master.Top+Master.Height;
    Width:=SlaveMiddle.X-MasterMiddle.X;
    Height:=Slave.Top-Top;
  end;

  dx:=Width/sqrt(sqr(Width)+sqr(Height));
  dy:=Height/sqrt(sqr(Width)+sqr(Height));

  Model.Canvas.Pen.Color:=clNavy;
  Model.Canvas.Brush.Color:=clBlue;
  Paint;
end;

procedure TReplicationLink.RefreshLinkDirection;
var theDir: TReplicationLinkDirection;
  MasterMiddle, SlaveMiddle: TPoint;
  dx, dy: integer;
begin
  theDir:=RLDTop;

  MasterMiddle.X:=Master.Left+Master.Width div 2;
  MasterMiddle.Y:=Master.Top+Master.Height div 2;
  SlaveMiddle.X:=Slave.Left+Slave.Width div 2;
  SlaveMiddle.Y:=Slave.Top+Slave.Height div 2;

  dx:=MasterMiddle.X-SlaveMiddle.X;
  dy:=MasterMiddle.Y-SlaveMiddle.Y;

  //top right corner
  if(dx<=0)and(dy>=0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=RLDRight
    else
      theDir:=RLDTop;
  end
  //bottom right corner
  else if(dx<=0)and(dy<0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=RLDRight
    else
      theDir:=RLDBottom;
  end
  //top left corner
  else if(dx>0)and(dy>=0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=RLDLeft
    else
      theDir:=RLDTop;
  end
  //bottom left corner
  else if(dx>0)and(dy<0)then
  begin
    if(abs(dx*hv_fac)>abs(dy))then
      theDir:=RLDLeft
    else
      theDir:=RLDBottom;
  end;

  LinkDirection:=theDir;
end;

procedure TReplicationLink.Paint;
var MasterMiddle, SlaveMiddle: TPoint;
begin
  MasterMiddle.X:=Master.Left+Master.Width div 2;
  MasterMiddle.Y:=Master.Top+Master.Height div 2;
  SlaveMiddle.X:=Slave.Left+Slave.Width div 2;
  SlaveMiddle.Y:=Slave.Top+Slave.Height div 2;

  with Model.Canvas do
  begin
    if(LinkDirection=RLDTop)then
    begin
      if(MasterMiddle.X>SlaveMiddle.X)then
      begin
        Pen.Width:=2;
        MoveTo(Left+Width, Top+Height-1);
        LineTo(Left, Top+0);

        Pen.Width:=1;
        Polygon([Point(Left+0, Top+0),
          Point(Left+round(dx*15+dy*5), Top+round((dy*15-dx*5))),
          Point(Left+round(dx*15-dy*5), Top+round((dy*15+dx*5)))]);
      end
      else
      begin
        Pen.Width:=2;
        MoveTo(Left+Width, Top+Height-1);
        LineTo(Left, Top+0);

        Pen.Width:=1;
        Polygon([Point(Left+0, Top+0),
          Point(Left+round(dx*15+dy*5), Top+round((dy*15-dx*5))),
          Point(Left+round(dx*15-dy*5), Top+round((dy*15+dx*5)))]);
      end;
    end
    else if(LinkDirection=RLDLeft)then
    begin
      if(MasterMiddle.Y>SlaveMiddle.Y)then
      begin
        Pen.Width:=2;
        MoveTo(Left+Width-1, Top+Height);
        LineTo(Left+0, Top);

        Pen.Width:=1;
        Polygon([Point(Left+0, Top+0),
          Point(Left+round(dx*15+dy*5), Top+round((dy*15-dx*5))),
          Point(Left+round(dx*15-dy*5), Top+round((dy*15+dx*5)))]);
      end
      else
      begin
        Pen.Width:=2;
        MoveTo(Left+Width-1, Top+Height);
        LineTo(Left+0, Top);

        Pen.Width:=1;
        Polygon([Point(Left+0, Top+0),
          Point(Left+round(dx*15+dy*5), Top+round((dy*15-dx*5))),
          Point(Left+round(dx*15-dy*5), Top+round((dy*15+dx*5)))]);
      end;
    end
    else if(LinkDirection=RLDRight)then
    begin
      if(MasterMiddle.X<SlaveMiddle.X)then
      begin
        Pen.Width:=2;
        MoveTo(Left+0, Top);
        LineTo(Left+Width-1, Top+Height-1);

        Pen.Width:=1;
        Polygon([Point(Left+Width-1+0, Top+Height-1+0),
          Point(Left+Width-1-round(dx*15+dy*5), Top+Height-1-round((dy*15-dx*5))),
          Point(Left+Width-1-round(dx*15-dy*5), Top+Height-1-round((dy*15+dx*5)))]);
      end
      else
      begin
        Pen.Width:=2;
        MoveTo(Left+0, Top);
        LineTo(Left+Width-1, Top+Height-1);

        Pen.Width:=1;
        Polygon([Point(Left+Width-1+0, Top+Height+0),
          Point(Left+Width-1-round(dx*15+dy*5), Top+Height-1-round((dy*15-dx*5))),
          Point(Left+Width-1-round(dx*15-dy*5), Top+Height-1-round((dy*15+dx*5)))]);
      end;
    end
    else
    begin
      if(MasterMiddle.X<SlaveMiddle.X)then
      begin
        Pen.Width:=2;
        MoveTo(Left, Top+0);
        LineTo(Left+Width-1, Top+Height-1);

        Pen.Width:=1;
        Polygon([Point(Left+Width-1+0, Top+Height-1+0),
          Point(Left+Width-1-round(dx*15+dy*5), Top+Height-1-round((dy*15-dx*5))),
          Point(Left+Width-1-round(dx*15-dy*5), Top+Height-1-round((dy*15+dx*5)))]);
      end
      else
      begin
        Pen.Width:=2;
        MoveTo(Left, Top+0);
        LineTo(Left+Width-1, Top+Height-1);

        Pen.Width:=1;
        Polygon([Point(Left+Width-1+0, Top+Height-1+0),
          Point(Left+Width-1-round(dx*15+dy*5), Top+Height-1-round((dy*15-dx*5))),
          Point(Left+Width-1-round(dx*15-dy*5), Top+Height-1-round((dy*15+dx*5)))]);
      end;
    end;
  end;
end;



end.
