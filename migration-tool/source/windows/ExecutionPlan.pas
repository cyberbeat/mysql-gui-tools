unit ExecutionPlan;

interface

uses gnugettext, Messages, Windows, SysUtils, Classes, AuxFuncs, Contnrs,
  Controls, TntForms, TntMenus, Graphics, TntStdCtrls, PNGImage, Sections,
  PNGTools;

type
  TPlanTask = class(TObject)
  protected
    FTitle: WideString;
    FDescription: WideString;
    FCompleted: Boolean;
    FSectionNr: Integer;
  public
    constructor Create(Title: WideString; Description: WideString; SectionNr: Integer);
  published
    property Title: WideString read FTitle;
    property Description: WideString read FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
  end;

  TExecutionPlan = class(TGraphicControl)
  private
    FOnPaint: TNotifyEvent;

    BGPNGImg,
    BGSelPNGImg,
    BGDonePNGImg,
    TaskCheckedPNGImg,
    TaskUncheckedPNGImg,
    OverlayPNGImg: TPngObject;

    FCurrentTask: Integer;
  protected
    procedure Paint; override;

    function GetCurrentTask: Integer;
    procedure SetCurrentTask(CurrentTask: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  published
    PlanTasks: TObjectList;

    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;

    property CurrentTask: Integer read GetCurrentTask write SetCurrentTask;
  end;

implementation

constructor TPlanTask.Create(Title: WideString; Description: WideString; SectionNr: Integer);
begin
  FTitle:=Title;
  FDescription:=Description;
  FSectionNr:=SectionNr;
end;

constructor TExecutionPlan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PlanTasks:=TObjectList.Create;

  BGPNGImg:=LoadPNGImageFromResource('migrationplan_bg');
  BGSelPNGImg:=LoadPNGImageFromResource('migrationplan_bg_sel');
  BGDonePNGImg:=LoadPNGImageFromResource('migrationplan_bg_done');

  TaskCheckedPNGImg:=LoadPNGImageFromResource('task_checked');
  TaskUncheckedPNGImg:=LoadPNGImageFromResource('task_unchecked');

  OverlayPNGImg:=LoadPNGImageFromResource('migrationplan_overview');

  FCurrentTask:=0;
end;

destructor TExecutionPlan.Destroy;
begin
  BGPNGImg.Free;
  BGSelPNGImg.Free;
  BGDonePNGImg.Free;

  TaskCheckedPNGImg.Free;
  TaskUncheckedPNGImg.Free;
  OverlayPNGImg.Free;

  PlanTasks.Free;

  inherited;
end;

function TExecutionPlan.GetCurrentTask: Integer;
begin
  Result:=FCurrentTask;
end;

procedure TExecutionPlan.SetCurrentTask(CurrentTask: Integer);
begin
  FCurrentTask:=CurrentTask;

  Invalidate;
end;

procedure TExecutionPlan.Paint;
var i, j: integer;
  S, S1: WideString;
begin
  Canvas.Font:=Font;
  Canvas.Brush.Color:=Color;

  with Canvas do
  begin
    Pen.Style:=psSolid;
    Pen.Color:=$00CB8B64;
    Brush.Style:=bsClear;
    Rectangle(0, 0, Width, Height);

    Pen.Style:=psClear;
    Brush.Style:=bsSolid;
    Brush.Color:=$00E3DFE0;
    Rectangle(1, 1, BGPNGImg.Width, 19);


    Font.Name:='Tahoma';
    Font.Color:=$00868688;
    Font.Height:=-12;
    Font.Style:=[fsBold];

    DrawWideStringText(Canvas.Handle, PWideChar(_('Migration Plan')),
      Length(_('Migration Plan')),
      Rect((BGPNGImg.Width-GetWideStringTextWidth(Canvas, PWideChar(_('Migration Plan')))) div 2, 3,
        1+BGPNGImg.Width, 19));

    Brush.Style:=bsClear;

    for i:=0 to PlanTasks.Count-1 do
    begin
      if(FCurrentTask=i)then
        BGSelPNGImg.Draw(Canvas, Rect(1, 20+BGPNGImg.Height*i,
          1+BGPNGImg.Width, 20+BGPNGImg.Height*(i+1)))
      else
        BGPNGImg.Draw(Canvas, Rect(1, 20+BGPNGImg.Height*i,
          1+BGPNGImg.Width, 20+BGPNGImg.Height*(i+1)));

      //if(Not(TPlanTask(PlanTasks[i]).Completed))then
      if (i > CurrentTask - 1) then
        TaskUncheckedPNGImg.Draw(Canvas, Rect(7, 28+BGPNGImg.Height*i,
          7+TaskUncheckedPNGImg.Width, 28+BGPNGImg.Height*i+TaskUncheckedPNGImg.Height))
      else
        TaskCheckedPNGImg.Draw(Canvas, Rect(7, 28+BGPNGImg.Height*i,
          7+TaskUncheckedPNGImg.Width, 28+BGPNGImg.Height*i+TaskUncheckedPNGImg.Height));

      Font.Color:=$00DD9B6B;
      Font.Height:=-9;
      Font.Style:=[fsBold];

      DrawWideStringText(Canvas.Handle, PWideChar(TPlanTask(PlanTasks[i]).Title),
        Length(TPlanTask(PlanTasks[i]).Title),
        Rect(25, 30+BGPNGImg.Height*i, 1+BGPNGImg.Width, 30+BGPNGImg.Height*i+10));


      Font.Color:=$00000000;
      Font.Height:=-9;
      Font.Style:=[];

      //Multiline output
      S:=TPlanTask(PlanTasks[i]).Description;
      j:=0;
      repeat
        S1:=BreakLine(Canvas, S, 85);

        DrawWideStringText(Canvas.Handle, PWideChar(S1), Length(S1),
          Rect(25, 38+BGPNGImg.Height*i+4+j*10, 110, 38+BGPNGImg.Height*i+4+10+j*10));

        inc(j);
      until (S='');
    end;

    OverlayPNGImg.Draw(Canvas, Rect(120, 30,
      120+OverlayPNGImg.Width, 30+OverlayPNGImg.Height));

    if(Height>PlanTasks.Count*BGPNGImg.Height+20+1)then
    begin
      Brush.Color:=clWhite;
      Pen.Color:=clWhite;
      Rectangle(1, PlanTasks.Count*BGPNGImg.Height+20, Width, Height);

      Pen.Color := $00AAAAAA;
      Pen.Style := psSolid;
      MoveTo(1, 20 + BGPNGImg.Height * (PlanTasks.Count));
      LineTo(BGPNGImg.Width + 1, 20 + BGPNGImg.Height * (PlanTasks.Count));
    end;
  end;


  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TExecutionPlan.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Exit;

  if((Y-20) div BGPNGImg.Height <= PlanTasks.Count-1)then
    CurrentTask:=(Y-20) div BGPNGImg.Height;

  inherited;
end;

end.
