object AdminServiceControlForm: TAdminServiceControlForm
  Left = 277
  Top = 102
  Caption = 'Service Control'
  ClientHeight = 634
  ClientWidth = 842
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServiceControlPnl: TTntPanel
    Left = 185
    Top = 0
    Width = 657
    Height = 634
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    DesignSize = (
      657
      634)
    object PageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 637
      Height = 614
      ActivePage = StartStopServiceSheet
      Align = alClient
      TabOrder = 0
      object StartStopServiceSheet: TTabSheet
        Caption = 'Start/Stop Service'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          629
          586)
        object ServiceControlCBox: TTntGroupBox
          Left = 12
          Top = 204
          Width = 605
          Height = 372
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Log Messages'
          TabOrder = 0
          DesignSize = (
            605
            372)
          object LogMessageLbl: TTntLabel
            Left = 16
            Top = 24
            Width = 305
            Height = 13
            Caption = 'This log shows all messages during server startup and shutdown.'
            Transparent = True
          end
          object StartupLogMemo: TTntMemo
            Left = 16
            Top = 43
            Width = 571
            Height = 311
            Anchors = [akLeft, akTop, akRight, akBottom]
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object Panel1: TTntPanel
          Left = 0
          Top = 0
          Width = 629
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          object UserInfoBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 605
            Height = 3
            Shape = bsTopLine
          end
          object ServiceNameHeaderLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 41
            Height = 13
            Caption = 'MySQL'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object PageHeaderImg: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object UserInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 155
            Height = 13
            Caption = 'Start or stop the selected service'
          end
        end
        object ServiceStatusGBox: TTntGroupBox
          Left = 12
          Top = 51
          Width = 605
          Height = 142
          Caption = 'Server Status'
          TabOrder = 2
          object ServiceLabel: TTntLabel
            Left = 16
            Top = 84
            Width = 387
            Height = 50
            AutoSize = False
            Caption = 'Reading service status...'
            Transparent = True
            WordWrap = True
          end
          object ServerStatusLbl: TTntLabel
            Left = 74
            Top = 50
            Width = 154
            Height = 13
            Caption = 'MySQL Service is stopped.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object Label1: TTntLabel
            Left = 74
            Top = 36
            Width = 70
            Height = 13
            Caption = 'Service status:'
            Transparent = True
          end
          object ServiceStatusImg: TTntImage
            Left = 16
            Top = 26
            Width = 48
            Height = 48
          end
          object StopServiceBtn: TTntBitBtn
            Left = 446
            Top = 84
            Width = 139
            Height = 27
            Caption = 'Stop Service'
            TabOrder = 1
            OnClick = StopServiceBtnClick
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000120B0000120B000000000000000000005A00FF5A00FF
              5A00FF5A00FF5A00FF5A00FF8B898A5A595A5151515553545958588886875A00
              FF5A00FF5A00FF5A00FF5A00FF5A00FF00005A00FF5A00FF5A00FF5A00FF8B89
              8A6967689D9C9CCCCBCBCFCFCFC7C7C7AAAAAA6969693131318C8A8B5A00FF5A
              00FF5A00FF5A00FF00005A00FF5A00FF5A00FF6D6B6CA6A5A5FEFEFEFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFCFCFCBCBBBC5554555251515A00FF5A00FF5A00FF
              00005A00FF5A00FF787677C7C6C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE9E9E9706F6F4746475A00FF5A00FF00005A00FF8D8B8C
              C3C2C3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFEFEFEF5F5E5F7A78795A00FF00005A00FFA2A0A1FFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCF
              CECF3130315A00FF00009C9A9BCFCECEFFFFFFFFFFFFFFFFFFF0F0FF0000FF00
              00FF0000FF0000FF0000FF0000FFE1E1FFFFFFFFFFFFFFFDFDFD616061969494
              00007F7D7EF4F4F4FFFFFFFFFFFFFFFFFFF0F0FF0000FF0000FF0000FF0000FF
              0000FF0000FFE1E1FFFFFFFFFFFFFFFFFFFF9696966A686900007B797AFFFFFF
              FFFFFFFFFFFFFFFFFFF0F0FF0000FF0000FF0000FF0000FF0000FF0000FFE1E1
              FFFFFFFFFFFFFFFFFFFFBAB9BA5B595900007B797AFFFFFFFFFFFFFFFFFFFFFF
              FFF0F0FF0000FF0000FF0000FF0000FF0000FF0000FFE1E1FFFFFFFFFFFFFFFF
              FFFFC2C1C25D5B5C00007E7C7DF5F5F5FFFFFFFFFFFFFFFFFFF0F0FF0000FF00
              00FF0000FF0000FF0000FF0000FFE1E1FFFFFFFFFFFFFFFFFFFFAFAFAF737171
              00009A9798D2D1D2FFFFFFFFFFFFFFFFFFF0F0FF0000FF0000FF0000FF0000FF
              0000FF0000FFE1E1FFFFFFFFFFFFFFFFFFFF8282829F9C9D00005A00FFA2A0A1
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFAFAFA514F505A00FF00005A00FF8B8889C7C6C6FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA6
              A5A68D8A8B5A00FF00005A00FF5A00FF7A7879D3D3D3FFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBAB9BA7673745A00FF5A00FF
              00005A00FF5A00FF5A00FF7B797AC5C4C5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFB8B7B88582835A00FF5A00FF5A00FF00005A00FF5A00FF
              5A00FF5A00FF8D8B8C9D9B9CC8C7C8E4E3E3F9F8F9F2F2F2E2E2E2BDBCBC8E8D
              8D9E9B9C5A00FF5A00FF5A00FF5A00FF00005A00FF5A00FF5A00FF5A00FF5A00
              FF5A00FFA29FA08C898A7B797A807E7F8D8B8CABA8A95A00FF5A00FF5A00FF5A
              00FF5A00FF5A00FF0000}
            Margin = 5
            Spacing = -1
          end
          object StartServiceBtn: TTntBitBtn
            Left = 446
            Top = 84
            Width = 139
            Height = 27
            Caption = 'Start Service'
            TabOrder = 0
            Visible = False
            OnClick = StartServiceBtnClick
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000120B0000120B000000000000000000005A00FF5A00FF
              5A00FF5A00FF5A00FF5A00FF8B898A5A595A5151515553545958588886875A00
              FF5A00FF5A00FF5A00FF5A00FF5A00FF00005A00FF5A00FF5A00FF5A00FF8B89
              8A6967689D9C9CCCCBCBCFCFCFC7C7C7AAAAAA6969693131318C8A8B5A00FF5A
              00FF5A00FF5A00FF00005A00FF5A00FF5A00FF6D6B6CA6A5A5FEFEFEFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFCFCFCBCBBBC5554555251515A00FF5A00FF5A00FF
              00005A00FF5A00FF787677C7C6C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFE9E9E9706F6F4746475A00FF5A00FF00005A00FF8D8B8C
              C3C2C3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFEFEFEF5F5E5F7A78795A00FF00005A00FFA2A0A1FFFFFFFFFFFFFFFF
              FFFFFFFFBDEBBFEAF9EBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCF
              CECF3130315A00FF00009C9A9BCFCECEFFFFFFFFFFFFFFFFFFFFFFFF7CD7802A
              BE3199E09CFAFDFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD616061969494
              00007F7D7EF4F4F4FFFFFFFFFFFFFFFFFFFFFFFF7CD78020BB2720BB273FC545
              B8E9BAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9696966A686900007B797AFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF7CD78020BB2720BB2720BB2720BB275CCD61DAF4
              DBFFFFFFFFFFFFFFFFFFBAB9BA5B595900007B797AFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF7CD78020BB2720BB2720BB2720BB2732C139ABE5AEFFFFFFFFFFFFFF
              FFFFC2C1C25D5B5C00007E7C7DF5F5F5FFFFFFFFFFFFFFFFFFFFFFFF7CD78020
              BB2720BB2725BD2C84D988F2FBF2FFFFFFFFFFFFFFFFFFFFFFFFAFAFAF737171
              00009A9798D2D1D2FFFFFFFFFFFFFFFFFFFFFFFF7CD78020BB275FCE64DDF5DE
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8282829F9C9D00005A00FFA2A0A1
              FFFFFFFFFFFFFFFFFFFFFFFF9EE1A1C0ECC2FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFAFAFA514F505A00FF00005A00FF8B8889C7C6C6FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA6
              A5A68D8A8B5A00FF00005A00FF5A00FF7A7879D3D3D3FFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBAB9BA7673745A00FF5A00FF
              00005A00FF5A00FF5A00FF7B797AC5C4C5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFB8B7B88582835A00FF5A00FF5A00FF00005A00FF5A00FF
              5A00FF5A00FF8D8B8C9D9B9CC8C7C8E4E3E3F9F8F9F2F2F2E2E2E2BDBCBC8E8D
              8D9E9B9C5A00FF5A00FF5A00FF5A00FF00005A00FF5A00FF5A00FF5A00FF5A00
              FF5A00FFA29FA08C898A7B797A807E7F8D8B8CABA8A95A00FF5A00FF5A00FF5A
              00FF5A00FF5A00FF0000}
            Margin = 5
            Spacing = -1
          end
          object ConnectToInstanceAni: TAnimate
            Left = 94
            Top = 0
            Width = 130
            Height = 13
            StopFrame = 14
            Timers = True
            Transparent = False
            Visible = False
          end
        end
      end
      object ServiceConfigSheet: TTabSheet
        Caption = 'Configure Service'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object SettingsScrollBox: TTntScrollBox
          Left = 0
          Top = 45
          Width = 629
          Height = 541
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          TabOrder = 0
          object ServiceConfigFileGBox: TTntGroupBox
            Left = 12
            Top = 152
            Width = 605
            Height = 93
            Caption = 'Configuration File'
            TabOrder = 1
            object Label7: TTntLabel
              Left = 14
              Top = 25
              Width = 78
              Height = 13
              Caption = 'Config Filename:'
            end
            object Label8: TTntLabel
              Left = 312
              Top = 24
              Width = 277
              Height = 13
              AutoSize = False
              Caption = 'Configuration file to read startup parameters from.'
              Transparent = True
              WordWrap = True
            end
            object Label9: TTntLabel
              Left = 14
              Top = 57
              Width = 70
              Height = 13
              Caption = 'Section Name:'
            end
            object Label10: TTntLabel
              Left = 312
              Top = 57
              Width = 277
              Height = 32
              AutoSize = False
              Caption = 
                'Name of the configuration file section used for this instance of' +
                ' the server.'
              Transparent = True
              WordWrap = True
            end
            object ConfigFilenameEd: TTntEdit
              Left = 120
              Top = 21
              Width = 179
              Height = 21
              TabOrder = 0
              OnChange = ConfigFilenameEdChange
            end
            object ServiceSectionNameEd: TTntEdit
              Left = 120
              Top = 53
              Width = 179
              Height = 21
              Color = clBtnFace
              ReadOnly = True
              TabOrder = 1
              OnChange = DoPageContentChanged
            end
          end
          object ServiceFeaturesGBox: TTntGroupBox
            Left = 13
            Top = 251
            Width = 605
            Height = 226
            Caption = 'Server Features depending on choosen binary'
            TabOrder = 2
            object Label57: TTntLabel
              Left = 192
              Top = 146
              Width = 401
              Height = 47
              AutoSize = False
              Caption = 
                'Only enable this option when you really need debugging informati' +
                'on. Be aware that this will slow the server down. '
              Transparent = True
              WordWrap = True
            end
            object Label58: TTntLabel
              Left = 192
              Top = 104
              Width = 401
              Height = 36
              AutoSize = False
              Caption = 
                'The use of Named Pipes is only recommended if you make connectio' +
                'ns to localhost. Note that Debug information is not available if' +
                ' this option is set.'
              Transparent = True
              WordWrap = True
            end
            object Label62: TTntLabel
              Left = 192
              Top = 70
              Width = 401
              Height = 13
              AutoSize = False
              Caption = 'Enable this option if you want to use BDB for transactions.'
              Transparent = True
              WordWrap = True
            end
            object Label11: TTntLabel
              Left = 15
              Top = 196
              Width = 68
              Height = 13
              Caption = 'Path to binary:'
              Transparent = True
            end
            object TntLabel1: TTntLabel
              Left = 13
              Top = 20
              Width = 580
              Height = 44
              AutoSize = False
              Caption = 
                'Features listed here can only be switched by using a different b' +
                'inary in opposition to settings like InnoDB support, max. server' +
                ' connections etc. which are controlled by the configuration file' +
                '.'
              Transparent = True
              WordWrap = True
            end
            object PathToBinaryEd: TTntEdit
              Left = 120
              Top = 192
              Width = 437
              Height = 21
              TabOrder = 3
              OnChange = PathToBinaryEdChange
              OnExit = PathToBinaryEdExit
            end
            object DebugCheckbox: TTntCheckBox
              Left = 14
              Top = 145
              Width = 167
              Height = 17
              Caption = 'Debug Information (slow)'
              TabOrder = 2
              OnClick = ServiceFeatureClicked
            end
            object NamedPipeCheckbox: TTntCheckBox
              Left = 14
              Top = 107
              Width = 167
              Height = 17
              Caption = 'Named Pipes'
              TabOrder = 1
              OnClick = ServiceFeatureClicked
            end
            object BDBCheckbox: TTntCheckBox
              Left = 14
              Top = 69
              Width = 167
              Height = 17
              Caption = 'Support for BDB'
              TabOrder = 0
              OnClick = ServiceFeatureClicked
            end
            object PathToBinaryBrowseBtn: TTntBitBtn
              Left = 572
              Top = 191
              Width = 23
              Height = 23
              Caption = '...'
              TabOrder = 4
              OnClick = PathToBinaryBrowseBtnClick
            end
          end
          object ServiceSettingsGBox: TTntGroupBox
            Left = 12
            Top = 6
            Width = 605
            Height = 136
            Caption = 'Service Settings'
            TabOrder = 0
            object Label44: TTntLabel
              Left = 264
              Top = 20
              Width = 327
              Height = 37
              AutoSize = False
              Caption = 
                'When this option is enabled the MySQL Server will launch automat' +
                'ically when the operating system is started.'
              Transparent = True
              WordWrap = True
            end
            object Label3: TTntLabel
              Left = 14
              Top = 67
              Width = 68
              Height = 13
              Caption = 'Display Name:'
            end
            object Label4: TTntLabel
              Left = 312
              Top = 67
              Width = 277
              Height = 13
              AutoSize = False
              Caption = 'Name displayed in the Computer Management Console.'
              Transparent = True
              WordWrap = True
            end
            object Label5: TTntLabel
              Left = 14
              Top = 99
              Width = 95
              Height = 13
              Caption = 'Service Description:'
            end
            object Label6: TTntLabel
              Left = 312
              Top = 99
              Width = 277
              Height = 13
              AutoSize = False
              Caption = 'Description of the service.'
              Transparent = True
              WordWrap = True
            end
            object ServiceAutoStartCBox: TTntCheckBox
              Left = 14
              Top = 19
              Width = 227
              Height = 17
              Caption = 'Launch MySQL server on system start'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = DoPageContentChanged
            end
            object ServiceDisplayNameEd: TTntEdit
              Left = 120
              Top = 63
              Width = 179
              Height = 21
              TabOrder = 1
              OnChange = DoPageContentChanged
            end
            object ServiceDescriptionEd: TTntEdit
              Left = 120
              Top = 95
              Width = 179
              Height = 21
              TabOrder = 2
              OnChange = DoPageContentChanged
            end
          end
          object Panel3: TTntPanel
            Left = 0
            Top = 501
            Width = 629
            Height = 40
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 3
            object InstallServiceBtn: TTntButton
              Left = 158
              Top = 1
              Width = 105
              Height = 25
              Caption = 'Install new Service'
              TabOrder = 0
              OnClick = InstallnewServiceMIClick
            end
            object UninstallServiceBtn: TTntButton
              Left = 276
              Top = 1
              Width = 105
              Height = 25
              Caption = 'Uninstall Service'
              Enabled = False
              TabOrder = 1
              OnClick = UninstallselectedServiceMIClick
            end
            object ApplyChangesBtn: TTntButton
              Left = 394
              Top = 1
              Width = 105
              Height = 25
              Caption = 'Apply Changes'
              Enabled = False
              TabOrder = 2
              OnClick = ApplyChangesBtnClick
            end
            object DiscardChangesBtn: TTntButton
              Left = 512
              Top = 1
              Width = 105
              Height = 25
              Caption = 'Discard Changes'
              Enabled = False
              TabOrder = 3
              OnClick = DiscardChangesBtnClick
            end
          end
        end
        object Panel4: TTntPanel
          Left = 0
          Top = 0
          Width = 629
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          object Bevel1: TTntBevel
            Left = 12
            Top = 38
            Width = 605
            Height = 3
            Shape = bsTopLine
          end
          object ServiceNameHeader2Lbl: TTntLabel
            Left = 46
            Top = 6
            Width = 41
            Height = 13
            Caption = 'MySQL'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
          end
          object PageHeader2Img: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object Label14: TTntLabel
            Left = 46
            Top = 20
            Width = 238
            Height = 13
            Caption = 'Set the Service parameters of the selected service'
            Transparent = True
          end
        end
      end
    end
    object LocalhostOnlyPnl: TTntPanel
      Left = 268
      Top = 6
      Width = 380
      Height = 17
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      DesignSize = (
        380
        17)
      object LocalhostWarningLbl: TTntLabel
        Left = 42
        Top = 2
        Width = 337
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'This section is only available when connected to localhost.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 20
      end
    end
  end
  object SubTreePnl: TTntPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 634
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TTntPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label15: TTntLabel
        Left = 2
        Top = 2
        Width = 86
        Height = 13
        Caption = 'Installed Services:'
      end
    end
    object ServicesTreeView: TTntTreeView
      Left = 0
      Top = 19
      Width = 185
      Height = 615
      Align = alClient
      HideSelection = False
      Images = ApplicationDM.AdminTree16ImageList
      Indent = 19
      PopupMenu = PopupMenu
      ReadOnly = True
      RowSelect = True
      ShowRoot = False
      TabOrder = 1
      OnChange = ServicesTreeViewChange
      OnDeletion = ServicesTreeViewDeletion
    end
  end
  object PopupMenu: TTntPopupMenu
    Left = 66
    Top = 134
    object RefreshServiceStatusMI: TTntMenuItem
      Caption = 'Refresh Service Status'
      OnClick = RefreshServiceStatusMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N1: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object InstallnewServiceMI: TTntMenuItem
      Caption = 'Install new Service'
      OnClick = InstallnewServiceMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object UninstallselectedServiceMI: TTntMenuItem
      Caption = 'Uninstall selected Service'
      OnClick = UninstallselectedServiceMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object ServiceCheckerTmr: TTimer
    Enabled = False
    OnTimer = ServiceCheckerTmrTimer
    Left = 154
    Top = 26
  end
end
