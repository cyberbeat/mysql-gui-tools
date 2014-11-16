unit MySQLResultSetFieldViewer;

interface

uses
  gnugettext, Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, TntForms, Forms,
  Dialogs, ExtCtrls, TntExtCtrls, ComCtrls, TntComCtrls,
  AuxFuncs, myx_public_interface, StdCtrls, TntStdCtrls,
  PNGImage, Jpeg, UniCodeEditor, TntSysUtils, MySQLResultSet;

type
  TViewerMode = (ViewerModeView, ViewerModeEdit);
  TViewerContentType = (
    ViewerContentTypeNone,
    ViewerContentTypeText,
    ViewerContentTypeImage,
    ViewerContentTypeBinary
  );

  TMySQLResultSetFieldViewerForm = class(TTntForm)
    PageControl: TTntPageControl;
    TextSheet: TTntTabSheet;
    ImageSheet: TTntTabSheet;
    TextMemo: TTntMemo;
    BinarySheet: TTntTabSheet;
    HexUniCodeEd: TUniCodeEdit;
    PlainTextUniCodeEdit: TUniCodeEdit;
    TntSplitter1: TTntSplitter;
    ImageScrollBox: TTntScrollBox;
    FieldImage: TTntImage;
    TextSheetBtnPnl: TTntPanel;
    OKBtn: TTntButton;
    CancelBtn: TTntButton;
    ImageSheetBtnPnl: TTntPanel;
    Ok2Btn: TTntButton;
    Cancel2Btn: TTntButton;
    TntPanel1: TTntPanel;
    OK3Btn: TTntButton;
    Cancel3Btn: TTntButton;
    ImageFormatTitleLbl: TTntLabel;
    TntBevel1: TTntBevel;
    ImageSizeTitleLbl: TTntLabel;
    ImageFormatLbl: TTntLabel;
    ImageSizeLbl: TTntLabel;
    BinarySizeTitelLbl: TTntLabel;
    BinarySizeLbl: TTntLabel;
    procedure PlainTextUniCodeEditScroll(Sender: TCustomUnicodeEdit; DeltaX, DeltaY: Integer);
    procedure HexUniCodeEdScroll(Sender: TCustomUnicodeEdit; DeltaX, DeltaY: Integer);
    constructor Create(AOwner: TComponent; Mode: TViewerMode); reintroduce;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMode: TViewerMode;
    FContentType: TViewerContentType;
    FChanging: Boolean;
  public
    procedure SetContent(RSValue: TRSFieldValue);

    property ContentType: TViewerContentType read FContentType;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  ApplicationDataModule, Options;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

constructor TMySQLResultSetFieldViewerForm.Create(AOwner: TComponent; Mode: TViewerMode);

begin
  inherited Create(AOwner);

  FMode := Mode;

  if Mode = ViewerModeView then
  begin
    CancelBtn.Visible:=False;
    OKBtn.Left:=CancelBtn.Left;
    Cancel2Btn.Visible:=False;
    OK2Btn.Left:=Cancel2Btn.Left;
    Cancel3Btn.Visible:=False;
    OK3Btn.Left:=Cancel3Btn.Left;
    TextMemo.ReadOnly := True;
  end;

  FContentType:=ViewerContentTypeNone;

  FieldImage.Left:=0;
  FieldImage.Top:=0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLResultSetFieldViewerForm.FormCreate(Sender: TObject);

begin
  InitForm(Self);
  ApplicationDM.QBOptions.RestoreWindowPos(Self);
  Font.Name := MYXCommonOptions.DataFontName;
  PageControl.ActivePageIndex := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLResultSetFieldViewerForm.FormDestroy(Sender: TObject);

begin
  ApplicationDM.QBOptions.AddWindowPos(self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLResultSetFieldViewerForm.SetContent(RSValue: TRSFieldValue);

var
  ImgFormat: MYX_IMAGE_FORMAT;
  MemStream: TMemoryStream;
  PNGImage: TPNGObject;
  JPEGImage: TJPEGImage;
  Data: String;
  Buffer: PChar;
  i, j: Integer;
  s, s1: WideString;
  c: Char;
  
begin
  ImageSheet.TabVisible := False;
  TextSheet.TabVisible := False;

  BinarySizeLbl.Caption := FormatFloat('###,###,###', RSValue.Length) + ' ' + _('Bytes');

  if(RSValue.BinaryData)then
  begin
    ImgFormat := myx_guess_image_format(RSValue.Value, RSValue.Length);

    if (ImgFormat <> MYX_IMG_UNKNOWN) and (ImgFormat <> MYX_IMG_GIF) then
    begin
      ImageSheet.TabVisible := True;
      PageControl.ActivePage := ImageSheet;
      FContentType:=ViewerContentTypeImage;
      PageControl.ActivePage:=ImageSheet;
      ImageSizeLbl.Caption := FormatFloat('###,###,###', RSValue.Length)+' '+_('Byte');

      MemStream := TMemoryStream.Create;
      try
        SetString(Data, PChar(RSValue.Value), RSValue.Length);

        if Data <> '' then
        begin
          MemStream.WriteBuffer(PChar(Data)^, RSValue.Length);
          MemStream.Position := 0;
        end;

        case ImgFormat of
          MYX_IMG_PNG:
            begin
              ImageFormatLbl.Caption := _('PNG');
              PNGImage := TPNGObject.Create;
              try
                try
                  PNGImage.LoadFromStream(MemStream);
                  FieldImage.Picture.Assign(PNGImage);
                except
                  ShowModalDialog(_('PNG image corrupted.'), _('The BLOB field contains a corrupted image file.'),
                    myx_mtError, 'Ok');
                  FieldImage.Picture.Bitmap.FreeImage;
                end;
              finally
                PNGImage.Free;
              end;
            end;
          MYX_IMG_JPEG:
            begin
              ImageFormatLbl.Caption := _('JPEG');
              JPEGImage := TJPEGImage.Create;
              try
                try
                  JPEGImage.LoadFromStream(MemStream);
                  FieldImage.Picture.Assign(JPEGImage);
                except
                  ShowModalDialog(_('JPG image corrupted.'), _('The BLOB field contains a corrupted image file.'),
                    myx_mtError, 'Ok');
                  FieldImage.Picture.Bitmap.FreeImage;
                end;
              finally
                JPEGImage.Free;
              end;
            end;
          MYX_IMG_BMP:
            begin
              ImageFormatLbl.Caption := _('BMP');
              try
                FieldImage.Picture.Bitmap.LoadFromStream(MemStream);
              except
                ShowModalDialog(_('JPG image corrupted.'), _('The BLOB field contains a corrupted image file.'),
                  myx_mtError, 'Ok');
                FieldImage.Picture.Bitmap.FreeImage;
              end;
            end;
        end;

        FieldImage.Invalidate;
      finally
        MemStream.Free;
      end;

      FieldImage.Invalidate;
    end;
  end
  else
  begin
    TextSheet.TabVisible := True;
    PageControl.ActivePage := TextSheet;

    FContentType:=ViewerContentTypeText;

    if not RSValue.IsNull then
      TextMemo.Text := TntAdjustLineBreaks(UTF8Decode(RSValue.Value))
    else
      TextMemo.Text := '';

    ImageSheet.TabVisible := False;
  end;

  GetMem(Buffer, RSValue.Length*2+1);
  try
    BinToHex(PChar(RSValue.Value), Buffer, RSValue.Length);

    j:=0;
    i:=0;
    s:='';
    s1:='';
    while(i<RSValue.Length*2)do
    begin
      s:=s+Buffer[i]+Buffer[i+1]+' ';

      c:=RSValue.Value[i div 2];
      if(c=#0)then
        c:='0'
      else if(c=#13)then
        c:='«'
      else if(c=#10)then
        c:='¶'
      else if(c<#32)then
        c:=' ';
      s1:=s1+c;

      inc(j);

      if(j mod 4=0)and(j<16)then
        s:=s+'|';

      if(j=16)then
      begin
        j:=0;
        HexUniCodeEd.Content.AddLine(s);
        PlainTextUniCodeEdit.Content.AddLine(s1);
        s:='';
        s1:='';
      end;

      inc(i, 2);
    end;

    if s<>'' then
    begin
      HexUniCodeEd.Content.AddLine(s);
      PlainTextUniCodeEdit.Content.AddLine(s1);
    end;

  finally
    FreeMem(Buffer);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLResultSetFieldViewerForm.HexUniCodeEdScroll(Sender: TCustomUnicodeEdit; DeltaX, DeltaY: Integer);

begin
  if not FChanging then
  begin
    FChanging := True;
    try
      HexUniCodeEd.Update;
      PlainTextUniCodeEdit.OffsetY := HexUniCodeEd.OffsetY;
      PlainTextUniCodeEdit.Update;
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMySQLResultSetFieldViewerForm.PlainTextUniCodeEditScroll(Sender: TCustomUnicodeEdit; DeltaX, DeltaY: Integer);

begin
  if not FChanging then
  begin
    FChanging := True;
    try
      PlainTextUniCodeEdit.Update;
      HexUniCodeEd.OffsetY := PlainTextUniCodeEdit.OffsetY;
      HexUniCodeEd.Update;
    finally
      FChanging := False;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
