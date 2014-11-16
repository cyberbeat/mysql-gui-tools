object sciAddLanguageForm: TsciAddLanguageForm
  Left = 267
  Top = 166
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add Language'
  ClientHeight = 104
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 13
    Width = 81
    Height = 13
    Caption = 'Language Name:'
  end
  object Label2: TLabel
    Left = 31
    Top = 42
    Width = 64
    Height = 13
    Caption = 'Lexer to use:'
  end
  object languagename: TEdit
    Left = 99
    Top = 10
    Width = 145
    Height = 21
    Hint = 
      'Name you would line to use to select the language via SelectedLa' +
      'nguage'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = languagenameChange
  end
  object LexerToUseCB: TComboBox
    Left = 100
    Top = 40
    Width = 145
    Height = 21
    Hint = 'The lexer this language will use to syntaxcolor this language'
    Style = csDropDownList
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = languagenameChange
  end
  object okButton: TBitBtn
    Left = 18
    Top = 72
    Width = 75
    Height = 25
    Enabled = False
    TabOrder = 2
    Kind = bkOK
  end
  object cancelButton: TBitBtn
    Left = 162
    Top = 72
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
end
