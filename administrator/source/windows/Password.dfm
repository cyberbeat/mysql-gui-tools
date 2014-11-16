object PasswordDialog: TPasswordDialog
  Left = 644
  Top = 432
  BorderStyle = bsDialog
  Caption = 'Account information'
  ClientHeight = 239
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = BALTIC_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object TntLabel1: TTntLabel
    Left = 16
    Top = 16
    Width = 369
    Height = 37
    AutoSize = False
    Caption = 
      'Enter user name and password for the account  under which the ta' +
      'sk will be executed.'
    Font.Charset = BALTIC_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object TntLabel2: TTntLabel
    Left = 16
    Top = 88
    Width = 67
    Height = 16
    Caption = 'User name:'
  end
  object TntLabel3: TTntLabel
    Left = 16
    Top = 144
    Width = 61
    Height = 16
    Caption = 'Password:'
  end
  object UserNameEdit: TTntEdit
    Left = 96
    Top = 84
    Width = 265
    Height = 24
    TabOrder = 0
    Text = 'UserNameEdit'
  end
  object PasswordEdit: TTntEdit
    Left = 96
    Top = 136
    Width = 265
    Height = 24
    AutoSize = False
    TabOrder = 1
    Text = 'PasswordEdit'
  end
  object CancelButton: TTntButton
    Left = 284
    Top = 188
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OKButton: TTntButton
    Left = 192
    Top = 188
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
