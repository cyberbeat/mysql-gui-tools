object SchemaSelectionForm: TSchemaSelectionForm
  Left = 304
  Top = 249
  Width = 291
  Height = 460
  BorderIcons = [biSystemMenu]
  BorderWidth = 12
  Caption = 'Schema Selection'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = TntFormKeyDown
  PixelsPerInch = 120
  TextHeight = 13
  inline SchemataFrame: TSchemataFrame
    Left = 0
    Top = 51
    Width = 259
    Height = 310
    Align = alClient
    TabOrder = 0
    TabStop = True
    inherited CatalogVST: TVirtualStringTree
      Width = 259
      Height = 265
      OnDblClick = OKBtnClick
      WideDefaultText = 'Fetching Data ...'
    end
    inherited TopPnl: TTntPanel
      Width = 259
      inherited SchemataLbl: TTntLabel
        Width = 48
        Height = 13
      end
    end
    inherited SpacerPnl: TTntPanel
      Width = 259
    end
    inherited AdvancedEdit: TAdvancedEditFrame
      Width = 259
    end
  end
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 259
    Height = 51
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      259
      51)
    object Label1: TTntLabel
      Left = 38
      Top = 2
      Width = 103
      Height = 13
      Caption = 'Schema Selection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TTntLabel
      Left = 38
      Top = 14
      Width = 211
      Height = 13
      Caption = 'Please select a schema from the schema list '
    end
    object HeaderImg: TTntImage
      Left = 0
      Top = 0
      Width = 32
      Height = 32
    end
    object TntBevel1: TTntBevel
      Left = -2
      Top = 36
      Width = 261
      Height = 2
      Anchors = [akLeft, akTop, akRight]
    end
  end
  object BottomPnl: TTntPanel
    Left = 0
    Top = 361
    Width = 259
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object CancelBtn: TTntButton
      Left = 184
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object OKBtn: TTntButton
      Left = 96
      Top = 14
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = OKBtnClick
    end
  end
end
