unit MigrationReport;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, Sections, StdCtrls, ComCtrls,
  TntComCtrls, TntStdCtrls, AuxFuncs, PNGImage, CheckLst,
  TntCheckLst, JdbcDBConns, AdvancedEdit, ImgList,
  myx_public_interface, myx_grt_public_interface,
  TntForms, TntClasses, Contnrs, VirtualTrees,
  MigrationObjMapDetail, PngTools,
  Grt, WizardPage, TntSysUtils;

type
  TMigrationReportForm = class(TWizardPageForm)
    DockPnl: TTntPanel;
    MainPngControl: TTntPageControl;
    SchemaListViewImgList: TImageList;
    SmallSchemaListViewImgList: TImageList;
    reportTabSheet: TTntTabSheet;
    MigGBox: TTntGroupBox;
    MigTaskHeaderLbl: TTntLabel;
    MigTaskLbl: TTntLabel;
    GRTMessageMemo: TTntMemo;
    SaveReportBtn: TTntButton;
    GenerateMigScriptBtn: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure DoTasks;
    procedure DockPnlResize(Sender: TObject);
    procedure SaveReportBtnClick(Sender: TObject);
    procedure GenerateMigScriptBtnClick(Sender: TObject);
  private
    { Private declarations }
    TaskUncheckedPNGImg,
    TaskCheckedPNGImg,
    TaskErrorPNGImg,
    TaskDisabledPNGImg: TPNGObject;

    FMigLogMainObjList,
      FMigLogObjList: TList;
  public
    { Public declarations }
    procedure BeforeSubPageIndexChange(SectionIndex: Integer); override;
  protected
    procedure SetSubPageIndex(NewSubPageIndex: Integer); override;
    function GetSubPageIndex: Integer; override;

    function GetSubPageCount: integer; override;

    function GetSectionTitle: WideString; override;
    function GetSectionInfo: WideString; override;

    function GetSupportAdvancedOptions: Boolean; override;
    procedure SetAdvancedOptionsVisibleState(State: Boolean); override;
    function GetAdvancedOptionsState: Boolean; override;
  end;

  TLogTreeDataType = (
    LTDT_LogEntry,
    LTDT_ListMember
  );

  PLogTreeData = ^TLogTreeData;
  TLogTreeData = record
    NodeType: TLogTreeDataType;

    PLogEntry: Pointer;
    
    PSourceObject: Pointer;
    SourceCaption: WideString;
    PTargetObject: Pointer;
    TargetCaption: WideString;
    Msg: WideString;

    StructName: WideString;
    MemberCaption: WideString;
  end;

var
  MigrationObjMapForm: TMigrationReportForm;

implementation

uses Main;

{$R *.dfm}

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.FormCreate(Sender: TObject);

begin
  InitForm(self);
  
  DockedPanel := DockPnl;

  MainPngControl.Align := alNone;
  MainPngControl.Left := -4;
  MainPngControl.Top := -27;
  MainPngControl.Width := 765+4+4;
  MainPngControl.Height := 529+274;
  MainPngControl.ActivePageIndex := 0;

  TaskUncheckedPNGImg := LoadPNGImageFromResource('task_unchecked');
  TaskCheckedPNGImg := LoadPNGImageFromResource('task_checked');
  TaskErrorPNGImg := LoadPNGImageFromResource('task_error');
  TaskDisabledPNGImg := LoadPNGImageFromResource('task_disabled');

  FMigLogMainObjList := nil;
  FMigLogObjList := nil;
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.FormDestroy(Sender: TObject);

begin
  TaskUncheckedPNGImg.Free;
  TaskCheckedPNGImg.Free;
  TaskErrorPNGImg.Free;
  TaskDisabledPNGImg.Free;
end;

// -----------------------------------------------------------------------------
// Wizard Interface
// -----------------------------------------------------------------------------

procedure TMigrationReportForm.SetSubPageIndex(NewSubPageIndex: Integer);

begin
  if (NewSubPageIndex=0) then
    DoTasks
  else
    MainPngControl.ActivePageIndex := NewSubPageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetSubPageIndex: Integer;

begin
  Result:=MainPngControl.ActivePageIndex;
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetSubPageCount: integer;

begin
  Result:=MainPngControl.PageCount;
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetSectionTitle: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Summary');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetSectionInfo: WideString;

begin
  case MainPngControl.ActivePageIndex of
    0:
      Result:=_('Please verify the migration report.');
  end;
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetSupportAdvancedOptions: Boolean;

begin
  Result:=False;
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.SetAdvancedOptionsVisibleState(State: Boolean);

begin
  //
end;

// -----------------------------------------------------------------------------

function TMigrationReportForm.GetAdvancedOptionsState: Boolean;

begin
  Result := False;
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.BeforeSubPageIndexChange(SectionIndex: Integer);

begin
  //
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.DockPnlResize(Sender: TObject);

begin
  MainPngControl.Width := DockPnl.Width;
  MainPngControl.Height := DockPnl.Height +
    MainPngControl.TabHeight + 8;
end;

// -----------------------------------------------------------------------------
// ObjMapping page implemetation
// -----------------------------------------------------------------------------

procedure TMigrationReportForm.DoTasks;

var
  PSchemata,
    PSchema,
    PStruct,
    PMember,
    PDatabaseObjects,
    PDatabaseObject,
    PLogs,
    PLog,
    PLogEntries,
    PLogEntry: Pointer;
  I,
    J,
    K,
    L,
    M,
    N,
    SchemaNum: Integer;
  S,
    SchemaName,
    StructName,
    MemberName,
    LogObject: WideString;
  TextAdded: Boolean;
  Report: TTntStringList;

begin
  Report := TTntStringList.Create;
  try
    Report.Clear;
    Report.Add('');
    Report.Add(StringOfChar('-', 80));
    Report.Add(
      StringAlignLeft('-- ' +
        _('MySQL Migration Toolkit Report'), 78) + '--');
    Report.Add(StringAlignLeft('-- ', 78) + '--');
    Report.Add(
      StringAlignLeft('-- ' +
        _('Title:   Summary Of The Migration Process'), 78) + '--');
    Report.Add(
      StringAlignLeft('-- ' +
        _('Date:    ') +
        FormatDateTime('yyyy-mm-dd hh:nn', Now), 78) + '--');
    Report.Add(StringOfChar('-', 80));
    Report.Add(#13#10);

    PSchemata := Grt.Global['/migration/targetCatalog/schemata'];
    SchemaNum := Grt.ListCount(PSchemata);

    S :=  _('1. Schema Migration');
    Report.Add(S + #13#10 +
      StringOfChar('-', Length(S)) + #13#10);

    Report.Add('  ' +
      Format(_('Number of migrated schemata: %d'),
        [SchemaNum]) + #13#10);

    for I := 0 to SchemaNum-1 do
    begin
      PSchema := myx_grt_list_item_get(PSchemata, I);

      SchemaName := Grt.DictString[PSchema, 'name'];
      StructName := UTF8Decode(_myx_grt_dict_struct_get_name(PSchema));

      PStruct := myx_grt_dict_struct_get(Grt.NativeGrt, PSchema);

      Report.Add(
        StringAlignLeft('  '+ _('Schema Name:'), 26) +
        SchemaName);

      for J := 0 to GetListMemberCount(Grt.NativeGrt, StructName, False)-1 do
      begin
        PMember := GetListMember(Grt.NativeGrt, StructName, J);

        MemberName := UTF8Decode(_myx_grt_struct_get_member_name(PMember));

        PDatabaseObjects := myx_grt_dict_item_get_value(PSchema, MemberName);

        Report.Add(
          StringAlignLeft('  - ' +
            myx_grt_struct_get_member_caption(Grt.NativeGrt,
              PStruct, PChar(UTF8Encode(MemberName)), 1)+ ':', 26)+
          IntToStr(myx_grt_list_item_count(PDatabaseObjects)));
      end;

      Report.Add(#13#10 +
        '  ' + _('Details:') + #13#10);

      for J := 0 to GetListMemberCount(Grt.NativeGrt, StructName, False)-1 do
      begin
        PMember := GetListMember(Grt.NativeGrt, StructName, J);

        MemberName := myx_grt_struct_get_member_name(PMember);

        PDatabaseObjects := myx_grt_dict_item_get_value(PSchema, MemberName);

        Report.Add('  - ' +
          myx_grt_struct_get_member_caption(Grt.NativeGrt,
              PStruct, PChar(UTF8Encode(MemberName)), 1));

        for K := 0 to Grt.ListCount(PDatabaseObjects)-1 do
        begin
          PDatabaseObject := myx_grt_list_item_get(PDatabaseObjects, K);

          S := '`' + SchemaName + '`.' +
            '`' + Grt.DictString[PDatabaseObject, 'name'] + '`';

          Report.Add('      ' + S + #13#10 +
            '      ' + StringOfChar('-', Length(S)));

          TextAdded := False;
          for N := 0 to 1 do
          begin
            if (N = 0) then
            begin
              PLogs := Grt.Global['/migration/migrationLog'];
              LogObject := 'refObject';
            end
            else
            begin
              PLogs := Grt.Global['/migration/creationLog'];
              LogObject := 'logObject';
            end;

            for L := 0 to Grt.ListCount(PLogs) - 1 do
            begin
              PLog := Grt.ListItem[PLogs, L];

              if (Grt.DictString[PLog, LogObject] =
                Grt.DictString[PDatabaseObject, '_id']) then
              begin
                PLogEntries := Grt.DictItem[PLog, 'entries'];

                for M := 0 to Grt.ListCount(PLogEntries) - 1 do
                begin
                  PLogEntry := Grt.ListItem[PLogEntries, M];

                  if (Grt.DictInt[PLogEntry, 'entryType'] > 0) then
                  begin
                    Report.Add('      ' +
                      '    ' + Tnt_WideStringReplace(
                        Tnt_WideStringReplace(
                            Grt.DictString[PLogEntry, 'name'],
                            #13#10, #13#10 + '      ' + '      ',
                            [rfReplaceAll]),
                          #10, #13#10 + '      ' + '      ',
                          [rfReplaceAll]
                        ));

                    TextAdded := True;
                  end;
                end;
              end;
            end;
          end;

          if (TextAdded) then
            Report.Add('');
        end;
      end;

      Report.Add(#13#10);

      S :=  _('2. Data Bulk Transfer');
      Report.Add(S + #13#10 +
        StringOfChar('-', Length(S)) + #13#10);

      PDatabaseObjects := myx_grt_dict_item_get_value(PSchema, 'tables');

      for K := 0 to Grt.ListCount(PDatabaseObjects)-1 do
      begin
        PDatabaseObject := myx_grt_list_item_get(PDatabaseObjects, K);

        S := '`' + SchemaName + '`.' +
          '`' + Grt.DictString[PDatabaseObject, 'name'] + '`';

        Report.Add('      ' + S + #13#10 +
          '      ' + StringOfChar('-', Length(S)));

        TextAdded := False;
        PLogs := Grt.Global['/migration/dataTransferLog'];

        for L := 0 to Grt.ListCount(PLogs) - 1 do
        begin
          PLog := Grt.ListItem[PLogs, L];

          if (Grt.DictString[PLog, 'refObject'] =
            Grt.DictString[PDatabaseObject, '_id']) then
          begin
            PLogEntries := Grt.DictItem[PLog, 'entries'];

            for M := 0 to Grt.ListCount(PLogEntries) - 1 do
            begin
              PLogEntry := Grt.ListItem[PLogEntries, M];

              Report.Add('      ' +
                '    ' + Tnt_WideStringReplace(
                  Tnt_WideStringReplace(
                      Grt.DictString[PLogEntry, 'name'],
                      #13#10, #13#10 + '      ' + '      ',
                      [rfReplaceAll]),
                    #10, #13#10 + '      ' + '      ',
                    [rfReplaceAll]
                  ));

              TextAdded := True;
            end;
          end;
        end;

        if (TextAdded) then
          Report.Add('');
      end;
    end;
    
    Report.Add(#13#10 +
      'End of report.');

    Report.Add(StringOfChar('-', 80));

    GRTMessageMemo.Lines := Report;
  finally
    Report.Free;
  end;

  SendMessage(GRTMessageMemo.Handle, WM_VSCROLL, SB_TOP, 0);
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.SaveReportBtnClick(Sender: TObject);

var
  SaveDlg: TSaveDialog;
  FileName: WideString;

begin
  SaveDlg := TSaveDialog.Create(self);
  try
    SaveDlg.Title := 'Store Application Snapshot';
    SaveDlg.DefaultExt := '.txt';

    SaveDlg.Filter := _('MySQL Migration Report')+
      ' (*.txt)|*.txt|'+
      _('All files')+' (*.*)|*.*';

    if SaveDlg.Execute then
    begin
      FileName := SaveDlg.Filename;
      GRTMessageMemo.Lines.SaveToFile(FileName);
    end;
  finally
    SaveDlg.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TMigrationReportForm.GenerateMigScriptBtnClick(Sender: TObject);
begin
  MainForm.GenerateMigrationScriptMIClick(self);
end;

end.
