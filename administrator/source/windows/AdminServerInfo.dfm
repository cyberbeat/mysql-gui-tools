object AdminServerInfoForm: TAdminServerInfoForm
  Left = 513
  Top = 217
  Caption = 'ServerInfo'
  ClientHeight = 489
  ClientWidth = 636
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServerInfoPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 636
    Height = 489
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    OnResize = ServerInfoPnlResize
    object RightBGImg: TTntImage
      Left = 320
      Top = 0
      Width = 316
      Height = 489
      Align = alRight
      Stretch = True
    end
    object LogoImg: TTntImage
      Left = 484
      Top = 20
      Width = 126
      Height = 56
      AutoSize = True
    end
    object ServerStatusLbl: TTntLabel
      Left = 110
      Top = 50
      Width = 145
      Height = 13
      Caption = 'MySQL Server is running.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      OnClick = ServerStatusLblClick
      OnMouseEnter = ServerStatusLblMouseEnter
      OnMouseLeave = ServerStatusLblMouseLeave
    end
    object Label5: TTntLabel
      Left = 96
      Top = 32
      Width = 65
      Height = 13
      Caption = 'Server status:'
      Transparent = True
    end
    object ServiceStatusImg: TTntImage
      Left = 38
      Top = 32
      Width = 48
      Height = 48
    end
    object ClientInfoGBox: TTntGroupBox
      Left = 44
      Top = 346
      Width = 500
      Height = 127
      Caption = 'Client Information'
      TabOrder = 1
      object ClientVersionCaptionLbl: TTntLabel
        Left = 18
        Top = 26
        Width = 38
        Height = 13
        Caption = 'Version:'
        Transparent = True
      end
      object ClientHardwareCaptionLbl: TTntLabel
        Left = 18
        Top = 98
        Width = 49
        Height = 13
        Caption = 'Hardware:'
        Transparent = True
      end
      object ClientOSCaptionLbl: TTntLabel
        Left = 18
        Top = 80
        Width = 86
        Height = 13
        Caption = 'Operating System:'
        Transparent = True
      end
      object ClientNetworkNameCaptionLbl: TTntLabel
        Left = 18
        Top = 44
        Width = 74
        Height = 13
        Caption = 'Network Name:'
        Transparent = True
      end
      object ClientNetworkNameLbl: TTntLabel
        Left = 128
        Top = 44
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ClientOSLbl: TTntLabel
        Left = 128
        Top = 80
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ClientHardwareLabel: TTntLabel
        Left = 128
        Top = 98
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ClientVersionLbl: TTntLabel
        Left = 128
        Top = 26
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ClientIPCaptionLabel: TTntLabel
        Left = 18
        Top = 62
        Width = 13
        Height = 13
        Caption = 'IP:'
        Transparent = True
      end
      object ClientIPLbl: TTntLabel
        Left = 128
        Top = 62
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
    end
    object ConnectionGBox: TTntGroupBox
      Left = 44
      Top = 96
      Width = 500
      Height = 105
      Caption = 'Connected to MySQL Server Instance'
      TabOrder = 0
      object UserLbl: TTntLabel
        Left = 128
        Top = 26
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object HostLbl: TTntLabel
        Left = 128
        Top = 42
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object UserCaptionLbl: TTntLabel
        Left = 18
        Top = 26
        Width = 51
        Height = 13
        Caption = 'Username:'
        Transparent = True
      end
      object HostCaptionLbl: TTntLabel
        Left = 18
        Top = 42
        Width = 51
        Height = 13
        Caption = 'Hostname:'
        Transparent = True
      end
      object PortCaptionLbl: TTntLabel
        Left = 18
        Top = 58
        Width = 22
        Height = 13
        Caption = 'Port:'
        Transparent = True
      end
      object PortLbl: TTntLabel
        Left = 128
        Top = 58
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object InstanceCaptionLbl: TTntLabel
        Left = 18
        Top = 74
        Width = 44
        Height = 13
        Caption = 'Instance:'
        Transparent = True
      end
      object InstanceNameLbl: TTntLabel
        Left = 128
        Top = 74
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
    end
    object ServerInfoGBox: TTntGroupBox
      Left = 44
      Top = 210
      Width = 500
      Height = 127
      Caption = 'Server Information'
      TabOrder = 2
      object ServerVersionCaptionLbl: TTntLabel
        Left = 18
        Top = 24
        Width = 76
        Height = 13
        Caption = 'MySQL Version:'
        Transparent = True
      end
      object ServerHardwareCaptionLbl: TTntLabel
        Left = 18
        Top = 96
        Width = 49
        Height = 13
        Caption = 'Hardware:'
        Transparent = True
      end
      object ServerOSCaptionLbl: TTntLabel
        Left = 18
        Top = 78
        Width = 86
        Height = 13
        Caption = 'Operating System:'
        Transparent = True
      end
      object ServerNetworkNameCaptionLbl: TTntLabel
        Left = 18
        Top = 42
        Width = 74
        Height = 13
        Caption = 'Network Name:'
        Transparent = True
      end
      object ServerNetworkNameLbl: TTntLabel
        Left = 128
        Top = 42
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ServerOSLbl: TTntLabel
        Left = 128
        Top = 78
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ServerHardwareLbl: TTntLabel
        Left = 128
        Top = 96
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ServerVersionLbl: TTntLabel
        Left = 128
        Top = 24
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
      object ServerIPCaptionLbl: TTntLabel
        Left = 18
        Top = 60
        Width = 13
        Height = 13
        Caption = 'IP:'
        Transparent = True
      end
      object ServerIPLbl: TTntLabel
        Left = 128
        Top = 60
        Width = 3
        Height = 13
        Caption = '-'
        Transparent = True
      end
    end
  end
end
