object NameEditorForm: TNameEditorForm
  Left = 684
  Top = 386
  ActiveControl = NameEd
  Caption = 'Enter a Schema Name'
  ClientHeight = 100
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object TntLabel1: TTntLabel
    Left = 10
    Top = 18
    Width = 100
    Height = 13
    Caption = 'Please enter a name:'
  end
  object TntButton1: TTntButton
    Left = 266
    Top = 18
    Width = 92
    Height = 31
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object NameEd: TTntEdit
    Left = 10
    Top = 44
    Width = 195
    Height = 21
    TabOrder = 1
  end
  object TntButton2: TTntButton
    Left = 266
    Top = 53
    Width = 92
    Height = 31
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
