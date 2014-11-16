object MyxConnectionDialogForm: TMyxConnectionDialogForm
  Left = 2418
  Top = 155
  AutoSize = True
  Caption = 'Connect to Database'
  ClientHeight = 546
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ParamsMainPnl: TTntPanel
    Left = 0
    Top = 170
    Width = 418
    Height = 201
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      418
      201)
    object DriverNotInstalledPnl: TTntPanel
      Left = 43
      Top = 32
      Width = 344
      Height = 151
      BevelOuter = bvNone
      TabOrder = 2
      Visible = False
      DesignSize = (
        344
        151)
      object TntLabel2: TTntLabel
        Left = 0
        Top = 8
        Width = 222
        Height = 15
        AutoSize = False
        Caption = 'Driver not attached'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object TntLabel9: TTntLabel
        Left = 0
        Top = 23
        Width = 337
        Height = 18
        AutoSize = False
        Caption = 'The selected driver is not attached to the application yet.'
        WordWrap = True
      end
      object LocateDriverBtn: TTntButton
        Left = 126
        Top = 44
        Width = 209
        Height = 23
        Anchors = [akTop, akRight]
        Caption = 'Locate Driver on Harddisk'
        TabOrder = 0
        OnClick = LocateDriverBtnClick
      end
      object DownloadDriverPnl: TTntPanel
        Left = 0
        Top = 100
        Width = 343
        Height = 53
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        DesignSize = (
          343
          53)
        object TntLabel12: TTntLabel
          Left = 0
          Top = -1
          Width = 337
          Height = 18
          AutoSize = False
          Caption = 'To download the driver from the internet press the button below.'
          WordWrap = True
        end
        object DownloadDriverBtn: TTntButton
          Left = 126
          Top = 20
          Width = 209
          Height = 23
          Anchors = [akTop, akRight]
          Caption = 'Download Driver from the Web'
          TabOrder = 0
          OnClick = DownloadDriverBtnClick
        end
      end
    end
    object ConnectToInstanceAni: TAnimate
      Left = 254
      Top = 11
      Width = 130
      Height = 13
      Anchors = [akLeft, akBottom]
      StopFrame = 14
      Timers = True
      Transparent = False
      Visible = False
    end
    object ConnectGBox: TTntGroupBox
      Left = 26
      Top = 14
      Width = 371
      Height = 187
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Connection Parameters'
      TabOrder = 0
      object ParamsPnl: TTntPanel
        Left = 6
        Top = 51
        Width = 359
        Height = 127
        BevelOuter = bvNone
        TabOrder = 1
      end
      object StoredConnPnl: TTntPanel
        Left = 6
        Top = 20
        Width = 359
        Height = 31
        BevelOuter = bvNone
        TabOrder = 0
        object ConnectionLbl: TTntLabel
          Left = 16
          Top = 4
          Width = 93
          Height = 13
          Alignment = taRightJustify
          Caption = 'Stored Connection:'
        end
        object StoredConnAddBtn: TTntSpeedButton
          Left = 286
          Top = -1
          Width = 23
          Height = 22
          Hint = 'Add Option'
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000360000002800000008000000080000000100
            180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
            FFFF000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000FFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF}
          Margin = 5
          ParentShowHint = False
          ShowHint = True
          OnClick = StoredConnAddBtnClick
        end
        object StoredConnDelBtn: TTntSpeedButton
          Left = 312
          Top = -1
          Width = 23
          Height = 22
          Hint = 'Remove Option'
          Enabled = False
          Glyph.Data = {
            F6000000424DF600000000000000360000002800000008000000080000000100
            180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFF000000000000000000000000000000000000000000000000000000000000
            000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          Margin = 5
          ParentShowHint = False
          ShowHint = True
          OnClick = StoredConnDelBtnClick
        end
        object StoredConnComboBox: TTntComboBox
          Left = 116
          Top = 0
          Width = 165
          Height = 21
          Hint = 'All stored connections'
          DropDownCount = 16
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnCloseUp = StoredConnComboBoxCloseUp
        end
      end
    end
  end
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 418
    Height = 67
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object HeaderImg: TTntImage
      Left = 0
      Top = 0
      Width = 418
      Height = 67
    end
  end
  object AdvParamsMainPnl: TTntPanel
    Left = 0
    Top = 371
    Width = 418
    Height = 118
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    DesignSize = (
      418
      118)
    object TntGroupBox1: TTntGroupBox
      Left = 26
      Top = 14
      Width = 371
      Height = 103
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Advanced Parameters'
      TabOrder = 0
      object AdvParamPnl: TTntPanel
        Left = 6
        Top = 20
        Width = 359
        Height = 76
        BevelOuter = bvNone
        TabOrder = 0
      end
    end
  end
  object BottomPnl: TTntPanel
    Left = 0
    Top = 489
    Width = 418
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object OKBtn: TTntButton
      Left = 150
      Top = 14
      Width = 75
      Height = 25
      Hint = 'Click this button to connect'
      Caption = 'OK'
      Default = True
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TTntButton
      Left = 322
      Top = 14
      Width = 75
      Height = 25
      Hint = 'Click this button to cancel the connection'
      Caption = 'Cancel'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CancelBtnClick
    end
    object ClearFieldsBtn: TTntButton
      Left = 236
      Top = 14
      Width = 75
      Height = 25
      Hint = 'Click this button to clear all fields'
      Caption = 'Clear'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = ClearFieldsBtnClick
    end
    object AdvancedBtn: TTntBitBtn
      Left = 26
      Top = 14
      Width = 99
      Height = 25
      Caption = 'Advanced >>'
      TabOrder = 3
      OnClick = AdvancedBtnClick
    end
  end
  object ConnTypeMainPnl: TTntPanel
    Left = 0
    Top = 67
    Width = 418
    Height = 103
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    DesignSize = (
      418
      103)
    object ConnTypeGBox: TTntGroupBox
      Left = 26
      Top = 14
      Width = 371
      Height = 89
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Connection Type'
      TabOrder = 0
      object ConnTypePnl: TTntPanel
        Left = 6
        Top = 20
        Width = 359
        Height = 63
        BevelOuter = bvNone
        TabOrder = 0
        object DriverLbl: TTntLabel
          Left = 76
          Top = 36
          Width = 33
          Height = 13
          Alignment = taRightJustify
          Caption = 'Driver:'
          Enabled = False
        end
        object RdbmsLbl: TTntLabel
          Left = 21
          Top = 4
          Width = 88
          Height = 13
          Alignment = taRightJustify
          Caption = 'Database System:'
        end
        object DriverComboBox: TTntComboBox
          Left = 116
          Top = 32
          Width = 218
          Height = 21
          Hint = 'All stored connections'
          Style = csDropDownList
          DropDownCount = 16
          Enabled = False
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnCloseUp = DriverComboBoxCloseUp
        end
        object RdbmsComboBox: TTntComboBox
          Left = 116
          Top = 0
          Width = 218
          Height = 21
          Hint = 'All stored connections'
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnCloseUp = RdbmsComboBoxCloseUp
        end
      end
    end
  end
  object LookupMenu: TTntPopupMenu
    Left = 360
    Top = 335
  end
end
