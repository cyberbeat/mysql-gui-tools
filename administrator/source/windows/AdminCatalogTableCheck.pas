unit AdminCatalogTableCheck;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Progress, MySQLConnection,
  myx_public_interface, myx_admin_public_interface, ExtCtrls, ComCtrls,
  ApplicationDataModule, AuxFuncs, TntForms, TntComCtrls, TntStdCtrls,
  TntExtCtrls, TntClasses, gnugettext;

type
  TableAction = (
    taOptimize,
    taCheck,
    taRepair
  );

  TAdminCatalogTableCheckForm = class(TTntForm)
    PageControl: TTntPageControl;
    OptionsTabSheet: TTabSheet;
    GroupBox1: TTntGroupBox;
    QuickRBtn: TTntRadioButton;
    FastRBtn: TTntRadioButton;
    MediumRBtn: TTntRadioButton;
    ExtendedRBtn: TTntRadioButton;
    ChangedRBtn: TTntRadioButton;
    ResultTabSheet: TTabSheet;
    PageControlHidePnl: TTntPanel;
    Label1: TTntLabel;
    Label2: TTntLabel;
    ResultMemo: TTntMemo;
    Label3: TTntLabel;
    HeaderLbl: TTntLabel;
    Label4: TTntLabel;
    OptimizeTabSheet: TTabSheet;
    Label5: TTntLabel;
    Label6: TTntLabel;
    GroupBox2: TTntGroupBox;
    OptLocalCBox: TTntCheckBox;
    Label7: TTntLabel;
    Label8: TTntLabel;
    Label9: TTntLabel;
    Label10: TTntLabel;
    Label11: TTntLabel;
    RepairTabSheet: TTabSheet;
    Label12: TTntLabel;
    Label13: TTntLabel;
    GroupBox3: TTntGroupBox;
    GroupBox4: TTntGroupBox;
    Label14: TTntLabel;
    Label17: TTntLabel;
    RepExtendedCBox: TTntCheckBox;
    RepQuickCBox: TTntCheckBox;
    RepUseFRMCBox: TTntCheckBox;
    Label15: TTntLabel;
    OptLocalLbl: TTntLabel;
    RepLocalCBox: TTntCheckBox;
    Label18: TTntLabel;
    RepairWarning: TTntLabel;
    Panel1: TTntPanel;
    ExecuteTableActionBtn: TTntButton;
    CloseBtn: TTntButton;
    Label19: TTntLabel;
    Label20: TTntLabel;
    SelectTabSheet: TTabSheet;
    Label21: TTntLabel;
    Label22: TTntLabel;
    GroupBox5: TTntGroupBox;
    Label23: TTntLabel;
    OptimizeTablesRBtn: TTntRadioButton;
    CheckTablesRBtn: TTntRadioButton;
    Label24: TTntLabel;
    Label25: TTntLabel;
    RepairCheckTablesRBtn: TTntRadioButton;
    GroupBox6: TTntGroupBox;
    TableListBox: TTntListBox;
    Label26: TTntLabel;

    constructor Create(AOwner: TComponent; Tables: WideString;
      MySQLConn: TMySQLConn;
      Action: TableAction = taOptimize); reintroduce;

    procedure ExecuteTableActionBtnClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ExecuteTableAction(Sender: TObject);
    procedure FinishedTableAction(Sender: TObject);
    procedure UpdateProgressForm(Sender: TObject);
  private
    FTableList: TTntStringList;
    FConnection: TMySQLConn;
    FProgressForm: TProgressForm;
    FAction: TableAction;
    FCheckType: integer;
    FMessages: WideString;
    FCurrentPos: integer;
    FCurrentTable: WideString;
  end;

var
  AdminCatalogTableCheckForm: TAdminCatalogTableCheckForm;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  AdminCatalog;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

constructor TAdminCatalogTableCheckForm.Create(AOwner: TComponent; Tables: WideString; MySQLConn: TMySQLConn;
  Action: TableAction);
  
begin
  inherited Create(AOwner);

  InitForm(self);

  FTableList := TTntStringList.Create;
  FTableList.Text := Tables;
  FConnection := MySQLConn;
  FAction := Action;
  TableListBox.Items.Text := Tables;

  OptLocalCBox.Enabled :=  ((MySQLConn.MajorVersion = 4) and (MySQLConn.MinorVersion >= 1)) or
    (MySQLConn.MajorVersion > 4);
  OptLocalLbl.Enabled := OptLocalCBox.Enabled;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.FormCreate(Sender: TObject);

begin
  PageControl.ActivePageIndex := 0;
  PageControlHidePnl.BringToFront;

  case FAction of
    taOptimize:
      OptimizeTablesRBtn.Checked := True;
    taCheck:
      CheckTablesRBtn.Checked := True;
    taRepair:
      RepairCheckTablesRBtn.Checked := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.FormDestroy(Sender: TObject);

begin
  FTableList.Free;

  TAdminCatalogForm(Owner).AdminCatalogTableCheckForm := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.FormClose(Sender: TObject; var Action: TCloseAction);

begin
  Action := caFree;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.ExecuteTableAction(Sender: TObject);

var
  I, J: integer;
  PMySQL: Pointer;
  PStatuses: PMYX_TABLE_COMMAND_STATUSES;
  Statuses: TMYX_TABLE_COMMAND_STATUSES;
  ErrorString, SuccessString: WideString;
  
begin
  with TAdminCatalogTableCheckForm(TFetchDataThread(Sender).Target) do
  begin
    case FAction of
      taOptimize:
      begin
        ErrorString := _('Error - Table %s could not be optimized.');
        SuccessString := _('Table %s optimized.');
      end;
      taCheck:
      begin
        ErrorString := _('Error - Table %s could not be checked.');
        SuccessString := _('Status checked for table %s.');
      end;
    else
      ErrorString := _('Error - Table %s could not be repaired.');
      SuccessString := _('Table %s repaired.');
    end;

    PMySQL := myx_mysql_init;
    try
      myx_connect_to_instance(FConnection.UserConnection.get_record_pointer, PMySQL);

      for I := 0 to FTableList.Count-1 do
      begin
        FCurrentPos := I;
        FCurrentTable := FTableList[I];
        TFetchDataThread(Sender).ExecuteSynchronized(UpdateProgressForm);

        case FAction of
          taOptimize:
            PStatuses := myx_optimize_table(PMySQL, FTableList[I], FCheckType);
          taCheck:
            PStatuses := myx_check_table(PMySQL, FTableList[I], FCheckType);
        else
          PStatuses := myx_repair_table(PMySQL, FTableList[I], FCheckType);
        end;


        if PStatuses = nil then
        begin
          FMessages := FMessages+StringOfChar('-', 60) + #13#10;
          FMessages := FMessages+
            Format(ErrorString, [FTableList[I]]) + #13#10 + _('MySQL Error Nr.') + IntToStr(myx_mysql_errno(PMySQL)) +
              #13#10 + myx_mysql_error(PMySQL) + #13#10;
        end
        else
        begin
          try
            Statuses := TMYX_TABLE_COMMAND_STATUSES.Create(PStatuses);
            try
              for J := 0 to Statuses.status.Count-1 do
              begin
                FMessages := FMessages + StringOfChar('-', 60) + #13#10;
                FMessages := FMessages + Format(SuccessString, [Statuses.status[J].table]) + #13#10;
                FMessages := FMessages + Statuses.status[J].message + #13#10;
              end;
            finally
              Statuses.Free;
            end;
          finally
            myx_free_command_status(PStatuses);
          end;
        end;

        if FProgressForm.Stopping then
          Break;
      end;

      FProgressForm.Advance(FTableList.Count, False);
      FProgressForm.Refresh;
    finally
      myx_mysql_close(PMySQL);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.FinishedTableAction(Sender: TObject);

begin
  with TAdminCatalogTableCheckForm(TFetchDataThread(Sender).Target) do
  begin
    FProgressForm.Free;

    ResultMemo.Text := FMessages;

    PageControl.ActivePage := ResultTabSheet;
    ExecuteTableActionBtn.Hide;
    Show;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.UpdateProgressForm(Sender: TObject);

begin
  FProgressForm.ActionLbl.Caption := FCurrentTable;
  FProgressForm.Advance(FCurrentPos, False);
end;


//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.ExecuteTableActionBtnClick(Sender: TObject);

begin
  if PageControl.ActivePageIndex = 0 then
  begin
    if OptimizeTablesRBtn.Checked then
      FAction := taOptimize
    else
      if CheckTablesRBtn.Checked then
      FAction := taCheck
    else
      FAction := taRepair;

    case FAction of
      taOptimize:
      begin
        Caption := _('Optimize Tables');
        ExecuteTableActionBtn.Caption := _('Optimize Tables');
      end;
      taCheck:
      begin
        Caption := _('Check Tables');
        ExecuteTableActionBtn.Caption := _('Check Tables');
      end;
      taRepair:
      begin
        Caption := _('Repair Tables');
        ExecuteTableActionBtn.Caption := _('Repair Tables');
      end;
    end;

    PageControl.ActivePageIndex := Ord(FAction) + 1;
  end
  else
  begin
    Hide;

    FCheckType := 0;
    case FAction of
      taOptimize:
        begin
          if OptLocalCBox.Checked then
            FCheckType := Ord(MYX_REPAIR_NO_WRITE_TO_BINLOG);
        end;
      taCheck:
        begin
          if(QuickRBtn.Checked)then
            FCheckType := Ord(MYX_CHECK_QUICK)
          else if(FastRBtn.Checked)then
            FCheckType := Ord(MYX_CHECK_FAST)
          else if(MediumRBtn.Checked)then
            FCheckType := Ord(MYX_CHECK_MEDIUM)
          else if(ExtendedRBtn.Checked)then
            FCheckType := Ord(MYX_CHECK_EXTENDED)
          else if(ChangedRBtn.Checked)then
            FCheckType := Ord(MYX_CHECK_CHANGED)
          else
            FCheckType := Ord(MYX_CHECK_MEDIUM);
        end;
      taRepair:
        begin
          if(RepLocalCBox.Checked)then
            FCheckType := FCheckType+Ord(MYX_REPAIR_NO_WRITE_TO_BINLOG);

          if(RepQuickCBox.Checked)then
            FCheckType := FCheckType+Ord(MYX_CHECK_QUICK);

          if(RepExtendedCBox.Checked)then
            FCheckType := FCheckType+Ord(MYX_CHECK_EXTENDED);

          if(RepUseFRMCBox.Checked)then
            FCheckType := FCheckType+Ord(MYX_REPAIR_USE_FRM);
        end;
    end;

    FMessages := '';
    FCurrentTable := '';
    FCurrentPos := 0;

    FProgressForm := TProgressForm.Create(nil);
    try
      case FAction of
        taOptimize:
        begin
          FProgressForm.Caption := _('Optimize Table Progress ...');
          FProgressForm.ActionCaptionLbl.Caption := _('Optimize Table:');
        end;
        taCheck:
        begin
          FProgressForm.Caption := _('Check Table Progress ...');
          FProgressForm.ActionCaptionLbl.Caption := _('Check Table:');
        end;
      else
        FProgressForm.Caption := _('Repair Table Progress ...');
        FProgressForm.ActionCaptionLbl.Caption := _('Repair Table:');
      end;

      FProgressForm.Reset;
      FProgressForm.Max := FTableList.Count;

      FProgressForm.Show;

      FConnection.FetchData(dkTableAction, ExecuteTableAction, FinishedTableAction, self,_('Checking tables ...'), True);
    except
      FProgressForm.Free;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TAdminCatalogTableCheckForm.CloseBtnClick(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
