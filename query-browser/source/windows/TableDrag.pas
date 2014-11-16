unit TableDrag;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, PNGImage, AuxFuncs, TntForms,
  TntExtCtrls, myx_public_interface, UniCodeConsole, VirtualTrees,
  MySQLConnection, Options;

type
  TTableDragForm = class(TTnTForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure CaptureBG;
    procedure TntFormPaint(Sender: TObject);
    procedure PlaceFormBelow(WinControl: TWinControl;
      PlaceBelow: Boolean = False);
    procedure TntFormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TntFormDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
    TableDragXPos: Array [0..10] of Integer;

    FDragTarget: TClass;
  public
    { Public declarations }
    property DragTarget: TClass read FDragTarget write FDragTarget;

    procedure ShowNoActivate;
  end;

function BuildDragSQLCommand(MySQLConn: TMySQLConn;
  SourceVT: TVirtualStringTree;
  CurrentCommand: WideString; var NewSelStart: integer;
  TablAddType: MYX_Q_TABLE_ADD_TYPE = MYX_QTAT_SELECT;
  ColumnAddType: MYX_Q_CLAUSE_TYPE = MYX_QCT_SELECT_CLAUSE): WideString;

function GetIdentifyer(MySQLConn: TMySQLConn;
  Schema: TMYX_SCHEMA; Table: TMYX_SCHEMA_TABLE = nil;
  Column: TMYX_SCHEMA_TABLE_COLUMN = nil): WideString;

implementation

uses
  QueryBrowser, PNGTools;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TTableDragForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  AlphaBlend := Not(MyxCommonOptions.DisableTransparencyEffects);

  FDragTarget := TMYX_SCHEMA_TABLE;
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.FormDestroy(Sender: TObject);

begin
  //OverlayPNGImg.Free;
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.FormClose(Sender: TObject;
  var Action: TCloseAction);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.CaptureBG;
//var DC: HDC;
begin
  {DC:=GetDC(0);
  try
    DesktopImg.Picture.Bitmap.Width:=Width;
    DesktopImg.Picture.Bitmap.Height:=Height;

    BitBlt(DesktopImg.Picture.Bitmap.Canvas.Handle,
       0, 0, Width, Height, DC, Left, Top, SRCCOPY);
  finally
    ReleaseDc(0, dc);
  end;}
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.TntFormPaint(Sender: TObject);

var
  xpos: integer;

begin
  Canvas;

  with Canvas do
  begin
    Font.Name:='Tahoma';
    Font.Size:=7;

    Pen.Color:=$009C9B91;
    Brush.Color:=clWhite;
    Rectangle(Rect(0, 0,
      Width,
      Height));

    xpos:=Width;

    if (FDragTarget = TMYX_SCHEMA_TABLE) then
    begin
      TableDragXPos[0]:=xpos;
      TableDragXPos[1]:=PaintEditButton(Canvas, _('DELETE'), nil, xpos);
      TableDragXPos[2]:=PaintEditButton(Canvas,_('INSERT'), nil, xpos);
      TableDragXPos[3]:=PaintEditButton(Canvas, _('UPDATE'), nil, xpos);

      xpos:=xpos-3;
      MoveTo(xpos, 0);
      LineTo(xpos, 20);

      TableDragXPos[4]:=PaintEditButton(Canvas, _('LEFT OUTER JOIN'), nil, xpos);
      TableDragXPos[5]:=PaintEditButton(Canvas, _('JOIN Table(s)'), nil, xpos);
      TableDragXPos[6]:=PaintEditButton(Canvas, _('Add Table(s)'), nil, xpos);

      xpos:=xpos-3;
      MoveTo(xpos, 0);
      LineTo(xpos, 20);

      TableDragXPos[7]:=PaintEditButton(Canvas, _('SELECT'), nil, xpos);

      Font.Color:=clGray;

      DrawWideStringText(Canvas.Handle,
        PWideChar(_('Drop the table(s) on one of the buttons.')),
        Length(_('Drop the table(s) on one of the buttons.')), Rect(4, 4, xpos-3, 4+10));

    end
    else
      if (FDragTarget = TMYX_SCHEMA_TABLE_COLUMN) then
      begin
        TableDragXPos[0]:=xpos;
        TableDragXPos[1]:=PaintEditButton(Canvas, _('ORDER'), nil, xpos);
        TableDragXPos[2]:=PaintEditButton(Canvas, _('HAVING'), nil, xpos);
        TableDragXPos[3]:=PaintEditButton(Canvas, _('GROUP'), nil, xpos);
        TableDragXPos[4]:=PaintEditButton(Canvas, _('WHERE'), nil, xpos);
        TableDragXPos[5]:=PaintEditButton(Canvas, _('FROM'), nil, xpos);
        TableDragXPos[6]:=PaintEditButton(Canvas, _('SELECT'), nil, xpos);

        Font.Color:=clGray;

        DrawWideStringText(Canvas.Handle,
          PWideChar(_('Drop the column(s) on one of the buttons.')),
          Length(_('Drop the column(s) on one of the buttons.')), Rect(4, 4, xpos-3, 4+10));
      end;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.PlaceFormBelow(WinControl: TWinControl;
  PlaceBelow: Boolean);

var
  ControlPos: TPoint;

begin
  ControlPos:=WinControl.ClientToScreen(Point(0, 0));

  if(PlaceBelow)then
  begin
    Left:=ControlPos.X-2;
    Top:=ControlPos.Y+WinControl.Height-3;
    Width:=WinControl.Width;
  end
  else
  begin
    Left:=ControlPos.X;
    Top:=ControlPos.Y+WinControl.Height-20-4;
    Width:=WinControl.Width-4;
  end;

  Height:=20;
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.TntFormDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);

begin
  Accept:=False;

  if (FDragTarget = TMYX_SCHEMA_TABLE) and
    (X>TableDragXPos[7]) and (X<TableDragXPos[0])then
      Accept:=True
  else
    if (FDragTarget = TMYX_SCHEMA_TABLE_COLUMN) and
    (X>TableDragXPos[6]) and (X<TableDragXPos[0])then
      Accept:=True;
end;

// -----------------------------------------------------------------------------

procedure TTableDragForm.TntFormDragDrop(Sender, Source: TObject; X,
  Y: Integer);

var
  TablAddType: MYX_Q_TABLE_ADD_TYPE;
  ColumnAddType: MYX_Q_CLAUSE_TYPE;
  NewSelStart: integer;
  QueryBrowserForm: TQueryBrowserForm;

begin
  TablAddType := MYX_QTAT_UNKNOWN;
  ColumnAddType := MYX_QCT_NO_CLAUSE;

  QueryBrowserForm:=nil;
  if(Source.InheritsFrom(TWinControl))then
    if(TWinControl(Source).Owner is TQueryBrowserForm)then
      QueryBrowserForm:=TQueryBrowserForm(TWinControl(Source).Owner)
    else if(TWinControl(Source).Owner<>nil)then
      if(TWinControl(Source).Owner.Owner<>nil)then
        if(TWinControl(Source).Owner.Owner is TQueryBrowserForm)then
          QueryBrowserForm:=TQueryBrowserForm(TWinControl(Source).Owner.Owner);

  if(QueryBrowserForm<>nil)then
  begin
    if (FDragTarget = TMYX_SCHEMA_TABLE) then
    begin
      if(X>TableDragXPos[7])and(X<TableDragXPos[6])then
        TablAddType:=MYX_QTAT_SELECT
      else if(X>TableDragXPos[6])and(X<TableDragXPos[5])then
        TablAddType:=MYX_QTAT_SELECT_ADD
      else if(X>TableDragXPos[5])and(X<TableDragXPos[4])then
        TablAddType:=MYX_QTAT_SELECT_JOIN
      else if(X>TableDragXPos[4])and(X<TableDragXPos[3])then
        TablAddType:=MYX_QTAT_SELECT_LEFT_OUTER_JOIN
      else if(X>TableDragXPos[3])and(X<TableDragXPos[2])then
        TablAddType:=MYX_QTAT_UPDATE
      else if(X>TableDragXPos[2])and(X<TableDragXPos[1])then
        TablAddType:=MYX_QTAT_INSERT
      else if(X>TableDragXPos[1])and(X<TableDragXPos[0])then
        TablAddType:=MYX_QTAT_DELETE;

      with QueryBrowserForm do
      begin
        if (LastFocusedControl=nil)or(LastFocusedControl = SQLEditor)then
        begin
          NewSelStart := SQLEditor.SelStart;
          CurrentCommand := BuildDragSQLCommand(MySQLConn, SchemataFrame.CatalogVST, CurrentCommand, NewSelStart,
            TablAddType);

          // Setting the current command causes an update message for the query browser.
          // Handle this first before continuing.
          Application.ProcessMessages;
          SQLEditor.CaretOffset(CurrentCommandStart, NewSelStart);
          if SQLEditor.CanFocus then
            SQLEditor.SetFocus;
        end;
      end;
    end
    else
      if (FDragTarget = TMYX_SCHEMA_TABLE_COLUMN) then
      begin
        if(X>TableDragXPos[6])and(X<TableDragXPos[5])then
          ColumnAddType:=MYX_QCT_SELECT_CLAUSE
        else if(X>TableDragXPos[5])and(X<TableDragXPos[4])then
          ColumnAddType:=MYX_QCT_FROM_CLAUSE
        else if(X>TableDragXPos[4])and(X<TableDragXPos[3])then
          ColumnAddType:=MYX_QCT_WHERE_CLAUSE
        else if(X>TableDragXPos[3])and(X<TableDragXPos[2])then
          ColumnAddType:=MYX_QCT_GROUP_CLAUSE
        else if(X>TableDragXPos[2])and(X<TableDragXPos[1])then
          ColumnAddType:=MYX_QCT_HAVING_CLAUSE
        else if(X>TableDragXPos[1])and(X<TableDragXPos[0])then
          ColumnAddType:=MYX_QCT_ORDER_CLAUSE;

        with QueryBrowserForm do
        begin
          if(LastFocusedControl=nil)or(LastFocusedControl=SQLEditor)then
          begin
            SQLEditor.Text := BuildDragSQLCommand(MySQLConn,
              SchemataFrame.CatalogVST,
              SQLEditor.Text,
              NewSelStart,
              MYX_QTAT_SELECT, ColumnAddType);

            SQLEditor.SelStart := NewSelStart;
            SQLEditor.CaretXY := SQLEditor.BlockBegin;

            DoQueryEditorChange;
            if(SQLEditor.CanFocus)then
              SQLEditor.SetFocus;
          end
        end;
      end;
  end;
end;

function BuildDragSQLCommand(MySQLConn: TMySQLConn;
  SourceVT: TVirtualStringTree;
  CurrentCommand: WideString; var NewSelStart: integer;
  TablAddType: MYX_Q_TABLE_ADD_TYPE;
  ColumnAddType: MYX_Q_CLAUSE_TYPE): WideString;

var i: integer;
  SelectedNodes: TNodeArray;
  NodeData,
  ParentNodeData,
  ParentParentNodeData: ^TObject;
  SQLCommand: WideString;
  TablAddError: MYX_Q_TABLE_ADD_ERROR;

begin
  Result:=CurrentCommand;
  NewSelStart:=0;

  SQLCommand:=CurrentCommand;

  if(TablAddType=MYX_QTAT_UNKNOWN)then
  begin
    if(GetKeyState(VK_CONTROL)<0)then
      TablAddType:=MYX_QTAT_SELECT_JOIN
    else if(GetKeyState(VK_SHIFT)<0)then
      TablAddType:=MYX_QTAT_SELECT_LEFT_OUTER_JOIN
    else if(GetKeyState(VK_CONTROL)<0)and(GetKeyState(VK_SHIFT)<0)then
      TablAddType:=MYX_QTAT_SELECT_ADD
    else
      TablAddType:=MYX_QTAT_SELECT;
  end;

  SelectedNodes:=SourceVT.GetSortedSelection(True);

  for i:=0 to SourceVT.SelectedCount-1 do
  begin
    NodeData:=SourceVT.GetNodeData(SelectedNodes[i]);

    if(NodeData<>nil)then
      if(NodeData^<>nil)then
      begin
        if(NodeData^ is TMYX_SCHEMA)then
        begin
          Result:='USE '+GetIdentifyer(MySQLConn,
            TMYX_SCHEMA(NodeData^))+';';
          NewSelStart:=Length(Result);

          break;
        end
        else if(NodeData^ is TMYX_SCHEMA_TABLE)then
        begin
          ParentNodeData:=SourceVT.GetNodeData(
            SelectedNodes[i].Parent);

          if(ParentNodeData<>nil)then
            if(ParentNodeData^<>nil)then
              if(ParentNodeData^ is TMYX_SCHEMA)then
              begin
                SQLCommand:=myx_query_add_table_to_sql(
                  MySQLConn.MySQL,
                  MySQLConn.DefaultSchema,
                  '',
                  TMYX_SCHEMA(ParentNodeData^).schema_name,
                  TMYX_SCHEMA_TABLE(NodeData^).table_name,
                  SQLCommand,
                  TablAddType,
                  @NewSelStart,@TablAddError);

                Result:=SQLCommand + ';';
              end;
        end
        else
          if(NodeData^ is TMYX_SCHEMA_TABLE_COLUMN)then
          begin
            ParentNodeData := SourceVT.GetNodeData(
              SelectedNodes[i].Parent);

            ParentParentNodeData := SourceVT.GetNodeData(
              SelectedNodes[i].Parent.Parent);

            SQLCommand := myx_query_add_column_to_sql(
              MySQLConn.MySQL,
              MySQLConn.DefaultSchema,
              '',
              TMYX_SCHEMA(ParentParentNodeData^).schema_name,
              TMYX_SCHEMA_TABLE(ParentNodeData^).table_name,
              TMYX_SCHEMA_TABLE_COLUMN(NodeData^).column_name,
              SQLCommand,
              ColumnAddType, @NewSelStart);

            Result := SQLCommand + ';';
          end;
      end;
  end;
end;

function GetIdentifyer(MySQLConn: TMySQLConn;
  Schema: TMYX_SCHEMA; Table: TMYX_SCHEMA_TABLE = nil;
  Column: TMYX_SCHEMA_TABLE_COLUMN = nil): WideString;
begin
  if(Schema=nil)then
    Exit;

  //Only schema given
  if(Table=nil)and(Column=nil)then
  begin
    if(myx_identifier_needs_quotes(Schema.schema_name)=1)then
      Result:='`'+Schema.schema_name+'`'
    else
      Result:=Schema.schema_name;
  end
  //Schema and table given
  else if(Column=nil)then
  begin
    if(CompareText(Schema.schema_name, MySQLConn.DefaultSchema)=0)then
    begin
      if(myx_identifier_needs_quotes(Table.table_name)=1)then
        Result:='`'+Table.table_name+'`'
      else
        Result:=Table.table_name;
    end
    else
    begin
      if(myx_identifier_needs_quotes(Schema.schema_name)=1)then
        Result:='`'+Schema.schema_name+'`'
      else
        Result:=Schema.schema_name;

      if(myx_identifier_needs_quotes(Table.table_name)=1)then
        Result:=Result+'.`'+Table.table_name+'`'
      else
        Result:=Result+'.'+Table.table_name;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TTableDragForm.ShowNoActivate;

begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW + SWP_NOMOVE + SWP_NOSIZE + SWP_NOACTIVATE);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
