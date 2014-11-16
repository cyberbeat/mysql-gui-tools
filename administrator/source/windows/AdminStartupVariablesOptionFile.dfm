object AdminStartupVariablesOptionFileForm: TAdminStartupVariablesOptionFileForm
  Left = 403
  Top = 180
  Caption = 'Choose an Option File'
  ClientHeight = 233
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServiceSettingsHeaderImg: TTntImage
    Left = 15
    Top = 12
    Width = 24
    Height = 24
    AutoSize = True
    Transparent = True
  end
  object SheetHeaderLbl: TTntLabel
    Left = 52
    Top = 16
    Width = 62
    Height = 13
    Caption = 'Option File'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TTntLabel
    Left = 52
    Top = 30
    Width = 164
    Height = 13
    Caption = 'Choose the option file to be edited.'
  end
  object GroupBox1: TTntGroupBox
    Left = 16
    Top = 60
    Width = 733
    Height = 119
    Caption = 'Configuration File'
    TabOrder = 0
    object Label2: TTntLabel
      Left = 14
      Top = 30
      Width = 52
      Height = 13
      Caption = 'Config File:'
    end
    object Label3: TTntLabel
      Left = 16
      Top = 65
      Width = 39
      Height = 13
      Caption = 'Section:'
    end
    object Label4: TTntLabel
      Left = 356
      Top = 30
      Width = 349
      Height = 26
      AutoSize = False
      Caption = 
        'Choose the configuration file to be edited. Note that several fi' +
        'les can be processed when the server starts.'
      WordWrap = True
    end
    object Label5: TTntLabel
      Left = 356
      Top = 66
      Width = 353
      Height = 39
      AutoSize = False
      Caption = 
        'Select the section that should be edited. The server will read t' +
        'he following sections if present. [mysqld], [server], [mysqld-x.' +
        'x]'
      WordWrap = True
    end
    object ParamConfigFileEd: TTntEdit
      Left = 98
      Top = 27
      Width = 197
      Height = 21
      TabOrder = 0
    end
    object ChooseCnfFile: TTntButton
      Left = 298
      Top = 27
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = ChooseCnfFileClick
    end
    object ParamSectionNameCBox: TTntComboBox
      Left = 98
      Top = 61
      Width = 221
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 2
    end
  end
  object CancelBtn: TTntButton
    Left = 644
    Top = 192
    Width = 105
    Height = 25
    Caption = '&Cancel'
    ModalResult = 3
    TabOrder = 1
  end
  object Button2: TTntButton
    Left = 526
    Top = 192
    Width = 105
    Height = 25
    Caption = '&Apply'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CreateNewSectionBtn: TTntButton
    Left = 408
    Top = 192
    Width = 105
    Height = 25
    Caption = '&Create new section'
    Enabled = False
    TabOrder = 3
    OnClick = CreateNewSectionBtnClick
  end
end
