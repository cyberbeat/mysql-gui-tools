object KeyEditForm: TKeyEditForm
  Left = 205
  Top = 84
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit Key Assignment'
  ClientHeight = 114
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlAlign: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 114
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 44
      Width = 50
      Height = 13
      Caption = 'Command:'
    end
    object Label2: TLabel
      Left = 15
      Top = 14
      Width = 44
      Height = 13
      Caption = 'ShortCut:'
    end
    object cmbCommand: TComboBox
      Left = 66
      Top = 40
      Width = 208
      Height = 21
      Hint = 'Select the command to be executed when this keystroke is pressed'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      Sorted = True
      TabOrder = 0
    end
    object HotKey: THotKey
      Left = 67
      Top = 11
      Width = 143
      Height = 19
      Hint = 'Press the keystroke you'#39'd like to assign'
      HotKey = 0
      InvalidKeys = []
      Modifiers = [hkExt]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnChange = HotKeyChange
    end
    object btnOK: TBitBtn
      Left = 16
      Top = 74
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 196
      Top = 74
      Width = 75
      Height = 25
      TabOrder = 2
      Kind = bkCancel
    end
  end
end
