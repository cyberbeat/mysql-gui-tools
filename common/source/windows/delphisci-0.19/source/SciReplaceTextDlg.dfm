inherited TextReplaceDialog: TTextReplaceDialog
  Left = 324
  Top = 148
  Caption = 'Replace text'
  ClientHeight = 215
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel [1]
    Left = 8
    Top = 41
    Width = 65
    Height = 13
    Caption = '&Replace with:'
  end
  inherited gbSearchOptions: TGroupBox
    Top = 70
    TabOrder = 2
  end
  inherited rgSearchDirection: TRadioGroup
    Top = 70
    TabOrder = 3
  end
  inherited btnOK: TBitBtn
    Top = 179
    Hint = 'Start search and replace'
    TabOrder = 4
  end
  inherited btnCancel: TBitBtn
    Top = 179
    Hint = 'Cancel search and replace'
    TabOrder = 5
  end
  object cbReplaceText: TComboBox
    Left = 73
    Top = 37
    Width = 260
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
end
