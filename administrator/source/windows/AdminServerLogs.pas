unit AdminServerLogs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InstanceSections, ComCtrls, StdCtrls, Grids, ImgList, DateUtils,
  ExtCtrls, PNGImage, AuxFuncs, myx_admin_public_interface,
  myx_public_interface, TntExtCtrls, Menus, TntMenus, TntComCtrls,
  TntStdCtrls, TntDialogs, AdminService,
  ApplicationDataModule, MySQLConnection, MyxError,
  StrUtils;

type
  TAdminServerLogsForm = class(TInstanceSectionForm)
    ServerLogsPnl: TTntPanel;
    ServerLogPageControl: TTntPageControl;
    ErrorLogTabSheet: TTabSheet;
    SlowLogTabSheet: TTabSheet;
    QueryTabSheet: TTabSheet;
    HeaderPnl: TTntPanel;
    ErrorBevel: TTntBevel;
    HeaderLbl: TTntLabel;
    HeaderImg: TTntImage;
    BottomPnl: TTntPanel;
    LogImageList: TImageList;
    Label1: TTntLabel;
    MemoPopupMenu: TTntPopupMenu;
    SearchMI: TTntMenuItem;
    SaveMI: TTntMenuItem;
    SearchNextMI: TTntMenuItem;
    N1: TTntMenuItem;
    LogBrowsePnl: TTntPanel;
    PagesPaintBox: TTntPaintBox;
    ErrorLogMemo: TTntRichEdit;
    ErrorLogEventListView: TTntListView;
    LogBlockUpDown: TTntUpDown;
    Label2: TTntLabel;
    EventsLbl: TTntLabel;
    Label4: TTntLabel;
    LocalhostOnlyPnl: TTntPanel;
    Label3: TTntLabel;
    OpenErrorFileBtn: TTntButton;
    SearchBtn: TTntButton;
    Button2: TTntButton;
    RefreshBtn: TTntButton;
    SubTreePnl: TTntPanel;
    ServicesTreeView: TTntTreeView;
    Panel2: TTntPanel;
    Label15: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DisplayLogFileBlock(BlockNr: Integer);
    procedure PagesPaintBoxPaint(Sender: TObject);
    procedure LogBlockUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure PagesPaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ErrorLogEventListViewClick(Sender: TObject);
    procedure SetCaretPos(Edit: TTntRichEdit; const Value: TPoint);

    procedure GetLogFileNames;

    procedure DoFind(Sender: TObject);
    procedure ErrorLogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchMIClick(Sender: TObject);
    procedure SearchNextMIClick(Sender: TObject);
    procedure MemoPopupMenuPopup(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
    procedure SearchDoClose(Sender: TObject);
    procedure SearchDoShow(Sender: TObject);
    procedure ServerLogPageControlChange(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);

    procedure ConnectionLost(var Message: TMessage); message WM_CONNECTION_LOST;
    procedure ConnectionEstablished(var Message: TMessage); message WM_CONNECTION_ESTABLISHED;

    procedure InitializeControls; override;
    procedure OpenErrorFileBtnClick(Sender: TObject);

    procedure MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
    procedure ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FHeaderPNGImg: TPNGObject;
    FLogBarEmptyPNGImg: TPNGObject;
    FLogBarFilledPNGImg: TPNGObject;
    FLogBarFilledTopPNGImg: TPNGObject;

    FErrorLogFileName: WideString;
    FSlowLogFileName: WideString;
    FQueryLogFileName: WideString;

    procedure RefreshServiceList;
    procedure SetCurrentService(MySQLService: TMySQLService);
    procedure ChangeCurrentService(ServiceName: WideString = '');
  protected
    procedure ActivateSection; override;
  public
    FindDialog: TFindDialog;
  end;

type
  PInteger = ^integer;
  TCharRange = record
    cpMin: Longint;
    cpMax: LongInt;
  end;

const
  EM_EXSETSEL = WM_USER + 55;
  EM_SCROLLCARET = $00B7;
  EM_LINESCROLL = $00B6;

  FetchedBlockSizeErrorLog = 20480;
  FetchedBlockSizeSlowLog = 20480;
  FetchedBlockSizeGeneralLog = 40960;

var
  AdminServerLogsForm: TAdminServerLogsForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  PNGTools, gnugettext;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.FormCreate(Sender: TObject);

begin
  InitForm(self);
  
  DockedPanel := ServerLogsPnl;
  SubTreePanel := SubTreePnl;

  FHeaderPNGImg := LoadPNGImageFromResource('server_logs', HeaderImg);

  FLogBarEmptyPNGImg := LoadPNGImageFromResource('log_bar_empty');
  FLogBarFilledPNGImg := LoadPNGImageFromResource('log_bar_filled');
  FLogBarFilledTopPNGImg := LoadPNGImageFromResource('log_bar_filled_top');

  ServerLogPageControl.ActivePageIndex := 0;

  FindDialog := nil;

  LocalhostOnlyPnl.Visible := not (MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections);
  DisableEnablePages(ServerLogPageControl, MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections);

  RefreshServiceList;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.FormDestroy(Sender: TObject);

begin
  FHeaderPNGImg.Free;
  FLogBarEmptyPNGImg.Free;
  FLogBarFilledPNGImg.Free;
  FLogBarFilledTopPNGImg.Free;

  ClearListView(ErrorLogEventListView, myx_ndt_pointer);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.GetLogFileNames;

// Attempts to get the file names of all activated log files. If the particular file does not exist then its name is
// not stored, just as wouldn't it be enabled at all.

var
  Size: Cardinal;
  Buffer: array[0..MAX_PATH] of WideChar;
  ComputerName: WideString;

begin
  if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
  begin
    Size := SizeOf(Buffer);
    GetComputerNameW(Buffer, Size);
    SetString(ComputerName, Buffer, Size);

    // Log file can be check-only (i.e. no file name given, but just enabled).
    // In this case construct the same log file names as the server would do.
    FErrorLogFileName := ApplicationDM.GetPathFromConfig('log-error');
    if FErrorLogFileName = 'checked' then
      FErrorLogFileName := ApplicationDM.DataDir + ComputerName + '.err';
    if not FileExists(FErrorLogFileName) then
      FErrorLogFileName := '';

    FSlowLogFileName := ApplicationDM.GetPathFromConfig('log-slow-queries');
    if FSlowLogFileName = 'checked' then
      FSlowLogFileName := ApplicationDM.DataDir + ComputerName + '-slow.log';
    if not FileExists(FSlowLogFileName) then
      FSlowLogFileName := '';

    FQueryLogFileName := ApplicationDM.GetPathFromConfig('log');
    if FQueryLogFileName = 'checked' then
      FQueryLogFileName := ApplicationDM.DataDir + ComputerName + '.log';
    if not FileExists(FQueryLogFileName) then
      FQueryLogFileName := '';
  end
  else
  begin
    FErrorLogFileName := '';
    FSlowLogFileName := '';
    FQueryLogFileName := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.DisplayLogFileBlock(BlockNr: Integer);

var
  pLogFile: PMYX_LOGFILE;
  LogFile: TMYX_LOGFILE;
  LastBlock: integer;
  i: integer;
  EventCaption: WideString;
  EventDateTime: TDateTime;
  IconNr: integer;
  PInt: PInteger;
  Item: TListItem;
  LogFileName: WideString;
  
begin
  case ServerLogPageControl.ActivePageIndex of
    0:
      LogFileName := FErrorLogFileName;
    1:
      LogFileName := FSlowLogFileName;
    2:
      LogFileName := FQueryLogFileName;
  else
    Exit;
  end;

  if (LogFileName='') or not FileExists(LogFileName) then
  begin
    DisableEnableControls(ServerLogPageControl.ActivePage, False);

    ClearListView(ErrorLogEventListView, myx_ndt_pointer);
    ErrorLogMemo.Text := '';
    EventsLbl.Caption := 'Events:';

    Exit;
  end;

  case ServerLogPageControl.ActivePageIndex of
    0:
      pLogFile := myx_parse_error_log(LogFileName, FetchedBlockSizeErrorLog, BlockNr, @LastBlock);
    1:
      pLogFile := myx_parse_slow_log(LogFileName, FetchedBlockSizeSlowLog, BlockNr, @LastBlock);
    2:
      pLogFile := myx_parse_general_log(LogFileName, FetchedBlockSizeGeneralLog, BlockNr, @LastBlock);
  else
    pLogFile := nil;
  end;

  if pLogFile = nil then
  begin
    DisableEnableControls(ServerLogPageControl.ActivePage, False);

    ClearListView(ErrorLogEventListView, myx_ndt_pointer);
    ErrorLogMemo.Text := '';
    EventsLbl.Caption := 'Events:';

    raise EMyxError.Create('Errorlog could not be parsed.');
  end;

  DisableEnableControls(ServerLogPageControl.ActivePage, True);

  LogFile := TMYX_LOGFILE.Create(pLogFile);
  try
    ErrorLogEventListView.Items.BeginUpdate;
    try
      ClearListView(ErrorLogEventListView, myx_ndt_pointer);

      ErrorLogEventListView.Clear;

      for i := 0 to LogFile.events.Count-1 do
      begin
        IconNr := -1;
        EventCaption := '';

        case LogFile.events[i].event_type of
          MYX_EVENT_START:
          begin
            EventCaption := 'Startup';
            IconNr := 0;
          end;
          MYX_EVENT_END:
          begin
            EventCaption := 'Shutdown';
            IconNr := 1;
          end;
          MYX_EVENT_ERROR:
          begin
            EventCaption := 'Error';
            IconNr := -1;
          end;
          MYX_EVENT_INNODB_START:
          begin
            EventCaption := 'InnoDB Startup';
            IconNr := 0;
          end;
          MYX_EVENT_INNODB_SHUTDOWN:
          begin
            EventCaption := 'InnoDB Shutdown';
            IconNr := 1;
          end;
          MYX_EVENT_FORCED_CLOSE_THREAD:
          begin
            EventCaption := 'Thread closed';
            IconNr := 1;
          end;
          MYX_EVENT_ABORT:
          begin
            EventCaption := 'Abort';
            IconNr := 1;
          end;
          MYX_EVENT_SELECT:
          begin
            EventCaption := 'Select';
            IconNr := 1;
          end;
          MYX_EVENT_INIT:
          begin
            EventCaption := 'Init DB';
            IconNr := 1;
          end;
          MYX_EVENT_CONNECT:
          begin
            EventCaption := 'Connect';
            IconNr := 1;
          end;
          MYX_EVENT_QUIT:
          begin
            EventCaption := 'Quit';
            IconNr := 1;
          end;
          MYX_EVENT_QUERY:
          begin
            EventCaption := 'Query';
            IconNr := 1;
          end;
        end;

        if(LogFile.events[i].date<>nil)then
        begin
          TryEncodeDateTime(LogFile.events[i].date.tm_year+2000,
            LogFile.events[i].date.tm_mon,
            LogFile.events[i].date.tm_mday,
            LogFile.events[i].date.tm_hour,
            LogFile.events[i].date.tm_min,
            LogFile.events[i].date.tm_sec,
            0,
            EventDateTime);

          GetMem(PInt, SizeOf(integer));
          PInt^ := LogFile.events[i].line_no-1;
          Item := AddListViewItem(ErrorLogEventListView, nil,
            FormatDateTime('dd mmm hh:nn', EventDateTime), IconNr, PInt);
          Item.SubItems.Add(EventCaption);
        end
        else
        begin
          GetMem(PInt, SizeOf(integer));
          PInt^ := LogFile.events[i].line_no-1;
          AddListViewItem(ErrorLogEventListView, nil,
            ' '+EventCaption, IconNr, PInt);
        end;
      end;
    finally
      ErrorLogEventListView.Items.EndUpdate;
    end;

    ErrorLogMemo.HandleNeeded;
    ErrorLogMemo.Text := LogFile.lines.text;

    //Scroll to end
    SendMessage(ErrorLogMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    SendMessage(ErrorLogEventListView.Handle, WM_VSCROLL, SB_BOTTOM, 0);

    LogBlockUpDown.Min := 1;
    LogBlockUpDown.Max := LogFile.block_num+1;
    LogBlockUpDown.Position := BlockNr;

    EventsLbl.Caption := 'Events of page '+IntToStr(LogFile.block_num-BlockNr+2)+
      ' / '+IntToStr(LogFile.block_num+1)+':';

    PagesPaintBox.Repaint;
  finally
    myx_free_logfile(pLogFile);
    
    LogFile.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.PagesPaintBoxPaint(Sender: TObject);

var
  i: Integer;
  x: Integer;
  PagesNr: Integer;
  CurrentPage: Integer;

begin
  PagesNr := LogBlockUpDown.Max;
  CurrentPage := LogBlockUpDown.Position;

  //At least one page
  if(PagesNr=0)then
    PagesNr := 1;

  with PagesPaintBox.Canvas do
  begin
    //Pen.Color := $00CB8B64;
    Pen.Color := $00B2ACA5;
    MoveTo(0, 0);
    LineTo(0, PagesPaintBox.Height-1);
    LineTo(PagesPaintBox.Width-1, PagesPaintBox.Height-1);
    LineTo(PagesPaintBox.Width-1, 0);
    LineTo(0, 0);

    FLogBarEmptyPNGImg.Draw(PagesPaintBox.Canvas,
      Rect(1, 1, PagesPaintBox.Width-1, 16));

    Pen.Color := $00BA3F01;
    for i := 0 to PagesNr-1 do
    begin
      x := Round((PagesPaintBox.Width/PagesNr)*i);

      MoveTo(x, 1);
      LineTo(x, PagesPaintBox.Height-1);
    end;

    x := Round((PagesPaintBox.Width/PagesNr)*(PagesNr-CurrentPage));

    FLogBarFilledTopPNGImg.Draw(PagesPaintBox.Canvas,
      Rect(x, 1, x+3, 16));

    FLogBarFilledPNGImg.Draw(PagesPaintBox.Canvas,
      Rect(x+3, 1,
        Round((PagesPaintBox.Width/PagesNr)*(PagesNr-CurrentPage+1)), 16));

    Pen.Color := $00CB8B64;
    Brush.Color := $00D7D0D0;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.LogBlockUpDownClick(Sender: TObject; Button: TUDBtnType);

begin
  if(LogBlockUpDown.Position>0)then
    DisplayLogFileBlock(LogBlockUpDown.Position);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.PagesPaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

begin
  if(Button=mbLeft)then
  begin
    if(LogBlockUpDown.Max>0)then
      LogBlockUpDown.Position := LogBlockUpDown.Max-
        Trunc(X/(PagesPaintBox.Width/LogBlockUpDown.Max));

    DisplayLogFileBlock(LogBlockUpDown.Position);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ErrorLogEventListViewClick(Sender: TObject);

var
  CaretPos: TPoint;

begin
  if(ErrorLogEventListView.Selected<>nil)then
    if(ErrorLogEventListView.Selected.Data<>nil)then
    begin
      CaretPos.Y := PInteger(ErrorLogEventListView.Selected.Data)^;
      CaretPos.X := 0;

      SetCaretPos(ErrorLogMemo, CaretPos);

      if(ServerLogPageControl.ActivePageIndex=1)then
        ErrorLogMemo.SelLength := 6
      else
        ErrorLogMemo.SelLength := 15;
      ErrorLogMemo.SetFocus;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SetCaretPos(Edit: TTntRichEdit; const Value: TPoint);

var
  CharRange: TCharRange;

begin
  CharRange.cpMin := SendMessage(Edit.Handle, EM_LINEINDEX, Value.y, 0) + Value.x;
  CharRange.cpMax := CharRange.cpMin;
  SendMessage(Edit.Handle, EM_EXSETSEL, 0, Longint(@CharRange));

  //SendMessage(Edit.Handle, EM_LINESCROLL, 0, Edit.Lines.Count - 1);
  SendMessage(Edit.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  SendMessage(Edit.Handle, EM_SCROLLCARET, 0, 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.DoFind(Sender: TObject);

var
  FoundAt: LongInt;
  StartPos, ToEnd: Integer;
  SearchType: TSearchTypes;
  
begin
  with ErrorLogMemo do
  begin
    { begin the search after the current selection if there is one }
    { otherwise, begin at the start of the text }
    if SelLength <> 0 then
      StartPos := SelStart + SelLength
    else
      StartPos := 0;

    { ToEnd is the length from StartPos to the end of the text in the rich edit control }
    ToEnd := Length(Text) - StartPos;

    SearchType := [];
    if(frWholeWord in TFindDialog(Sender).Options)then
      SearchType := SearchType+[stWholeWord];
    if(frMatchCase in TFindDialog(Sender).Options)then
      SearchType := SearchType+[stMatchCase];

    FoundAt := FindText(TFindDialog(Sender).FindText, StartPos, ToEnd, SearchType);
    if FoundAt <> -1 then
    begin
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(TFindDialog(Sender).FindText);
    end;

    SendMessage(ErrorLogMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    SendMessage(ErrorLogMemo.Handle, EM_SCROLLCARET, 0, 0);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ErrorLogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  if((Key=Ord('F'))and(ssCtrl in Shift))then
    SearchMIClick(self);

  if(Key=VK_F3)and(FindDialog<>nil)then
    DoFind(FindDialog);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SearchMIClick(Sender: TObject);

begin
  if(FindDialog=nil)then
  begin
    FindDialog := TFindDialog.Create(self);
    FindDialog.Options := [frHideUpDown];
    FindDialog.OnFind := DoFind;
    FindDialog.OnShow := SearchDoShow;
    FindDialog.OnClose := SearchDoClose;

    FindDialog.Execute;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SearchDoClose(Sender: TObject);

begin
  FindDialog := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SearchDoShow(Sender: TObject);

begin
  SetWindowPos(TFindDialog(Sender).Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SearchNextMIClick(Sender: TObject);

begin
  if(FindDialog<>nil)then
    DoFind(FindDialog);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.MemoPopupMenuPopup(Sender: TObject);

begin
  SearchNextMI.Enabled := (FindDialog<>nil);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SaveMIClick(Sender: TObject);

var
  SaveDialog: TTntSaveDialog;

begin
  SaveDialog := TTntSaveDialog.Create(self);
  try
    SaveDialog.Title := 'Save Log File Block';
    SaveDialog.Filter := '*.txt|Text Files';
    SaveDialog.FileName := 'ErrorLog.txt';

    if(SaveDialog.Execute)then
    begin
      ErrorLogMemo.PlainText := True;
      ErrorLogMemo.Lines.SaveToFile(SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ServerLogPageControlChange(Sender: TObject);

var
  ListColumn: TListColumn;

begin
  HeaderLbl.Caption := ServerLogPageControl.ActivePage.Caption;

  HeaderPnl.Parent := ServerLogPageControl.ActivePage;
  LogBrowsePnl.Parent := ServerLogPageControl.ActivePage;
  BottomPnl.Parent := ServerLogPageControl.ActivePage;

  if ServerLogPageControl.ActivePage = SlowLogTabSheet then
  begin
    if ErrorLogEventListView.Columns.Count > 1 then
      ErrorLogEventListView.Columns.Delete(1);
    ErrorLogEventListView.Columns[0].AutoSize := True;
    ErrorLogEventListView.Columns[0].Width := 200;
    ErrorLogEventListView.Columns[0].Caption := _('Log entry');
  end
  else
  begin
    if(ErrorLogEventListView.Columns.Count<2)then
    begin
      ErrorLogEventListView.Columns[0].AutoSize := False;
      ErrorLogEventListView.Columns[0].Width := 90;
      ErrorLogEventListView.Columns[0].Caption := _('Time');
      ListColumn := ErrorLogEventListView.Columns.Add;
      ListColumn.Width := 110;
      ListColumn.Caption := _('Log entry');
    end;
  end;

  if MySQLConn.IsLocalServer or ApplicationDM.Options.ShowOnlyServiceSections then
    DisplayLogFileBlock(1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.RefreshServiceList;

var
  LastService: WideString;
  
begin
  if Assigned(ApplicationDM.CurrentService) then
    LastService := ApplicationDM.CurrentService.ServiceName;
  SetCurrentService(nil);
  ServicesTreeView.Items.Clear;

  // Get all MySQL Services.
  ScanForServices(ServicesTreeView, ServicesImageIndex);
  ChangeCurrentService(LastService);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ActivateSection;

begin
  inherited;

  ChangeCurrentService;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ChangeCurrentService(ServiceName: WideString);

var
  I: Integer;

begin
  // Activate the last selected service or the first one if none was before.
  if ServicesTreeView.Items.Count > 0 then
  begin
    if (ServiceName = '') and Assigned(ApplicationDM.CurrentService) then
      ServiceName := ApplicationDM.CurrentService.ServiceName;
    I := SelectService(ServicesTreeview, ServiceName);
    if I = -1 then
      I := 0;
    SetCurrentService(ServicesTreeView.Items[I].Data);
    ServicesTreeView.Selected := ServicesTreeView.Items[I];
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.SetCurrentService(MySQLService: TMySQLService);

begin
  ApplicationDM.CurrentService := MySQLService;
  GetLogFileNames;
  DisplayLogFileBlock(1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ServicesTreeViewChange(Sender: TObject; Node: TTreeNode);

begin
  if not InitControls then
  begin
    ApplicationDM.CurrentService := nil;

    if (ServicesTreeView.Selected <> nil) then
      if (ServicesTreeView.Selected.Data <> nil) then
        ApplicationDM.CurrentService := TMySQLService(ServicesTreeView.Selected.Data);

    SetCurrentService(ApplicationDM.CurrentService);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ServicesTreeViewDeletion(Sender: TObject; Node: TTreeNode);

begin
  if not (csDestroying in ComponentState) and not (csRecreating in ServicesTreeView.ControlState) then
  begin
    if Node.Data = ApplicationDM.CurrentService then
      SetCurrentService(nil);
    TObject(Node.Data).Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.RefreshBtnClick(Sender: TObject);

begin
  DisplayLogFileBlock(LogBlockUpDown.Position);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ConnectionLost(var Message: TMessage);

begin
  GetLogFileNames;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.ConnectionEstablished(var Message: TMessage);

begin
  GetLogFileNames;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.InitializeControls;

begin
  if MySQLConn.IsLocalServer then
    DisplayLogFileBlock(1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.OpenErrorFileBtnClick(Sender: TObject);

var
  OpenDialog: TTntOpenDialog;
  
begin
  OpenDialog := TTntOpenDialog.Create(self);
  try
    OpenDialog.Title := 'Open Error Log File ...';
    OpenDialog.Filter := 'Error log file|*.*';
    OpenDialog.InitialDir := ApplicationDM.GetLastFileDialogPaths('ErrorLogFile');

    if(OpenDialog.Execute)then
    begin
      ApplicationDM.SetLastFileDialogPaths('ErrorLogFile',
        ExtractFilePath(OpenDialog.FileName));

      case ServerLogPageControl.ActivePageIndex of
        0:
          FErrorLogFileName := OpenDialog.FileName;
        1:
          FSlowLogFileName := OpenDialog.FileName;
        2:
          FQueryLogFileName := OpenDialog.FileName;
      end;

      DisplayLogFileBlock(1);
    end;
  finally
    OpenDialog.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.MenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);

var
  Size: TSize;
  Item: TTntMenuItem;

begin
  if Sender is TTntMenuItem then
  begin
    Item := Sender as TTntMenuItem;
    ACanvas.Font := Font;

    if Item.IsLine then
    begin
      Width := 10; // This will actually have no effect, because other entries are much wider.
      Height := 6;
    end
    else
    begin
      GetTextExtentPoint32W(ACanvas.Handle, PWideChar(Item.Caption), Length(Item.Caption), Size);

      // Border around each entry.
      Width := Size.cx + 4;
      Height := Size.cy + 6;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminServerLogsForm.MenuDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);

var
  Item: TTntMenuItem;

begin
  if Sender is TTntMenuItem then
  begin
    Item := Sender as TTntMenuItem;
    ACanvas.Font := Font;

    if Item.IsLine then
    begin
      // A menu separator.
      ACanvas.Pen.Color := clBtnShadow;
      ACanvas.MoveTo(ARect.Left + 2, (ARect.Bottom + ARect.Top) div 2);
      ACanvas.LineTo(ARect.Right - 2, (ARect.Bottom + ARect.Top) div 2);
    end
    else
    begin
      // Top level items have an invisible parent, so have to check the parent of the parent.
      if (Item.Parent.Parent = nil) and not (Item.GetParentMenu is TPopupMenu) then
      begin
        if [odHotLight, odSelected] * State <> [] then
          ACanvas.Brush.Color := clHighlight
        else
          ACanvas.Brush.Color := clBtnFace;
      end;
      ACanvas.FillRect(ARect);
      Inc(ARect.Left, 8);
      SetBKMode(ACanvas.Handle, TRANSPARENT);
      Windows.DrawTextW(ACanvas.Handle, PWideChar(Item.Caption), Length(Item.Caption), ARect, DT_LEFT + DT_SINGLELINE +
        DT_HIDEPREFIX + DT_VCENTER);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.

