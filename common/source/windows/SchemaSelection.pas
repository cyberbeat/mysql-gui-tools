unit SchemaSelection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PNGImage, AuxFuncs, ExtCtrls, TntExtCtrls, StdCtrls,
  SchemataTreeView, TntStdCtrls, TntForms, MySQLConnection,
  myx_public_interface, Menus, TntMenus;

type
  TSchemaSelectionForm = class(TTntForm)
    SchemataFrame: TSchemataFrame;
    HeaderPnl: TTntPanel;
    Label1: TTntLabel;
    Label2: TTntLabel;
    BottomPnl: TTntPanel;
    HeaderImg: TTntImage;
    CancelBtn: TTntButton;
    OKBtn: TTntButton;
    TntBevel1: TTntBevel;
    constructor Create(AOwner: TComponent; MySQLConn: TMySQLConn); reintroduce; overload;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
    procedure TntFormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    SchemaPNGImg: TPNGObject;

    MySQLConn: TMySQLConn;
  public
    { Public declarations }
    SelectedSchema: TMYX_SCHEMA;
  end;

implementation

{$R *.dfm}

uses
  PNGTools;

constructor TSchemaSelectionForm.Create(AOwner: TComponent; MySQLConn: TMySQLConn);
begin
  self.MySQLConn:=MySQLConn;

  inherited Create(AOwner);
end;

procedure TSchemaSelectionForm.FormCreate(Sender: TObject);
begin
  InitForm(self);

  SchemaPNGImg:=LoadPNGImageFromResource('schema_32x32', HeaderImg);

  SchemataFrame.MySQLConnection:=MySQLConn;
  SchemataFrame.ShowAssetsOnSchemaExpansion:=False;
  SchemataFrame.ShowSchemaAssets:=False;
  SchemataFrame.ReloadSchemaTree;

  SelectedSchema:=nil;
end;

procedure TSchemaSelectionForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TSchemaSelectionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  //
end;

procedure TSchemaSelectionForm.OKBtnClick(Sender: TObject);
begin
  SelectedSchema:=TMYX_SCHEMA(SchemataFrame.GetCatalogVSTFocusedObject(TMYX_SCHEMA));

  ModalResult:=mrOK;
end;

procedure TSchemaSelectionForm.TntFormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
    ModalResult := mrCancel;
end;

end.
