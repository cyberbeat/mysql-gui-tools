object TextSearchDialog: TTextSearchDialog
  Left = 235
  Top = 171
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Search for Text'
  ClientHeight = 180
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 12
    Width = 52
    Height = 13
    Caption = '&Search for:'
  end
  object cbSearchText: TComboBox
    Left = 73
    Top = 8
    Width = 260
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object gbSearchOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 154
    Height = 129
    Caption = 'Options'
    TabOrder = 1
    object cbSearchCaseSensitive: TCheckBox
      Left = 8
      Top = 17
      Width = 140
      Height = 17
      Caption = 'C&ase sensitivity'
      TabOrder = 0
    end
    object cbSearchWholeWords: TCheckBox
      Left = 8
      Top = 39
      Width = 140
      Height = 17
      Caption = '&Whole words only'
      TabOrder = 1
    end
    object cbSearchFromCursor: TCheckBox
      Left = 8
      Top = 61
      Width = 140
      Height = 17
      Caption = 'Search from &caret'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbSearchSelectedOnly: TCheckBox
      Left = 8
      Top = 83
      Width = 140
      Height = 17
      Caption = '&Selected text only'
      TabOrder = 3
    end
    object cbRegularExpression: TCheckBox
      Left = 8
      Top = 104
      Width = 140
      Height = 17
      Caption = '&Regular expression'
      TabOrder = 4
      OnClick = cbRegularExpressionClick
    end
  end
  object rgSearchDirection: TRadioGroup
    Left = 170
    Top = 40
    Width = 154
    Height = 65
    Caption = 'Direction'
    ItemIndex = 0
    Items.Strings = (
      '&Forward'
      '&Backward')
    TabOrder = 2
  end
  object btnOK: TBitBtn
    Left = 170
    Top = 146
    Width = 75
    Height = 23
    Hint = 'Start search'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 249
    Top = 146
    Width = 75
    Height = 23
    Hint = 'Cancel search'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Kind = bkCancel
  end
end
