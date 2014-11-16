unit AdminStartupVariablesOptionFile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, TntControls,
  Forms, TntForms, Dialogs,
  TntDialogs, TntStdCtrls, TntExtCtrls, PNGImage, AuxFuncs,
  myx_admin_public_interface, myx_public_interface, myx_util_public_interface, Controls, StdCtrls,
  ExtCtrls;

type
  TAdminStartupVariablesOptionFileForm = class(TTntForm)
    ServiceSettingsHeaderImg: TTntImage;
    SheetHeaderLbl: TTntLabel;
    Label1: TTntLabel;
    GroupBox1: TTntGroupBox;
    Label2: TTntLabel;
    ParamConfigFileEd: TTntEdit;
    ChooseCnfFile: TTntButton;
    Label3: TTntLabel;
    ParamSectionNameCBox: TTntComboBox;
    Label4: TTntLabel;
    Label5: TTntLabel;
    CancelBtn: TTntButton;
    Button2: TTntButton;
    CreateNewSectionBtn: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetConfigfFile(filename: WideString; section: WideString);
    procedure ChooseCnfFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CreateNewSectionBtnClick(Sender: TObject);
  private
    { Private declarations }
    ServerParameterPNGImg: TPNGObject;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  PNGTools;
  
procedure TAdminStartupVariablesOptionFileForm.FormCreate(Sender: TObject);

begin
  InitForm(self);

  ServerParameterPNGImg:=LoadPNGImageFromResource('server_parameter', ServiceSettingsHeaderImg);
end;

procedure TAdminStartupVariablesOptionFileForm.FormDestroy(
  Sender: TObject);
begin
  //
end;

procedure TAdminStartupVariablesOptionFileForm.FormClose(
  Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TAdminStartupVariablesOptionFileForm.SetConfigfFile(
  filename: WideString; section: WideString);
var stringlist: TMYX_STRINGLIST;
  pstringlist: PMYX_STRINGLIST;
  error: MYX_ADMIN_LIB_ERROR;
begin
  ParamConfigFileEd.Text:=filename;

  ParamSectionNameCBox.Items.Clear;

  //Get config file sections
  if(FileExists(filename))then
  begin
    pstringlist:=myx_get_all_cnf_sections(filename,@error);
    try
      stringlist:=TMYX_STRINGLIST.create(pstringlist);
      try
        ParamSectionNameCBox.Items.Assign(stringlist.strings)
      finally
        stringlist.Free;
      end;
    finally
      myx_free_lib_stringlist(pstringlist);
    end;
  end;

  if(ParamSectionNameCBox.Items.Count>0)then
  begin
    if(section='')then
      section:='mysqld';

    if(ParamSectionNameCBox.Items.IndexOf(section)<>-1)then
      ParamSectionNameCBox.ItemIndex:=ParamSectionNameCBox.Items.IndexOf(section)
    else
      ParamSectionNameCBox.ItemIndex:=0;
  end
  else
  begin
    ParamSectionNameCBox.Items.Add('mysqld');
    ParamSectionNameCBox.ItemIndex:=0;
  end;
end;

procedure TAdminStartupVariablesOptionFileForm.ChooseCnfFileClick(
  Sender: TObject);
var OpenDialog: TTntOpenDialog;
begin
  OpenDialog:=TTntOpenDialog.Create(self);
  try
    OpenDialog.InitialDir:=ExtractFilePath(ParamConfigFileEd.Text);
    OpenDialog.FileName:=ExtractFileName(ParamConfigFileEd.Text);
    OpenDialog.Filter:='All files|*.*|MySQL Configuration File (*.cnf)|*.cnf|'+
      'MySQL Configuration File (*.ini)|*.ini';
    OpenDialog.Options:=[ofPathMustExist, ofNoChangeDir, ofNoReadOnlyReturn,
      ofNoNetworkButton, ofEnableSizing, ofForceShowHidden];

    if(OpenDialog.Execute)then
      SetConfigfFile(OpenDialog.FileName, '');
  finally
    OpenDialog.Free;
  end;
end;

procedure TAdminStartupVariablesOptionFileForm.Button2Click(
  Sender: TObject);
begin
  if(DirectoryExists(ExtractFilePath(ParamConfigFileEd.Text)))then
    ModalResult:=mrOK
  else
    ShowModalDialog('Directory does not exist',
      'The directory where the configuration file should be located '+
      'must exists.',
      myx_mtError,
      'OK');
end;

procedure TAdminStartupVariablesOptionFileForm.CreateNewSectionBtnClick(
  Sender: TObject);
var section: WideString;
begin
  section:='mysqld';

  if(ShowModalEditDialog('Create new Section',
    'Please enter a name for the new section, e.g. mysqld, server...',
    myx_mtEdit, 'OK'#13#10'Cancel',
    True, 'Section Name:', section)=1)then
  begin
    if(ParamSectionNameCBox.Items.IndexOf(section)=-1)then
      ParamSectionNameCBox.Items.Add(section);

    ParamSectionNameCBox.ItemIndex:=ParamSectionNameCBox.Items.IndexOf(section);
  end;
end;

end.
