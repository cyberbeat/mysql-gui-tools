object AdminStartupVariablesForm: TAdminStartupVariablesForm
  Left = 274
  Top = 150
  Caption = 'Startup Parameters'
  ClientHeight = 669
  ClientWidth = 902
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
  object StartupParametersPnl: TTntPanel
    Left = 125
    Top = 0
    Width = 777
    Height = 669
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    DesignSize = (
      777
      669)
    object StartupParamsPageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 757
      Height = 598
      Align = alClient
      MultiLine = True
      TabOrder = 0
      OnChange = StartupParamsPageControlChange
    end
    object LocalhostOnlyPnl: TTntPanel
      Left = 388
      Top = 6
      Width = 382
      Height = 17
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 2
      Visible = False
      DesignSize = (
        382
        17)
      object AvailabilityLabel: TTntLabel
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
        Transparent = True
        ExplicitLeft = 18
      end
    end
    object BottomPanel: TTntPanel
      Left = 10
      Top = 608
      Width = 757
      Height = 51
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object ButtonPanel: TPanel
        Left = 350
        Top = 0
        Width = 407
        Height = 51
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        Padding.Left = 5
        TabOrder = 0
        DesignSize = (
          407
          51)
        object ApplyChangesBtn: TTntButton
          Left = 141
          Top = 13
          Width = 130
          Height = 24
          Anchors = [akRight, akBottom]
          Caption = '&Apply changes'
          Enabled = False
          TabOrder = 0
          OnClick = ApplyChangesBtnClick
        end
        object ChooseCnfFileBtn: TTntButton
          Left = 5
          Top = 13
          Width = 130
          Height = 24
          Anchors = [akRight, akBottom]
          Caption = 'Choose Option &File'
          TabOrder = 1
          Visible = False
          OnClick = ChooseCnfFileBtnClick
        end
        object DiscardChangesBtn: TTntButton
          Left = 277
          Top = 13
          Width = 130
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Discard changes'
          Enabled = False
          TabOrder = 2
          OnClick = DiscardChangesBtnClick
        end
      end
      object LabelPanel: TPanel
        Left = 0
        Top = 0
        Width = 350
        Height = 51
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          350
          51)
        object SectionLbl: TTntLabel
          Left = 88
          Top = 26
          Width = 251
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'No section selected'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = True
          Layout = tlCenter
          Visible = False
        end
        object OptionFileCaptionLabel: TTntLabel
          Left = 4
          Top = 2
          Width = 78
          Height = 20
          AutoSize = False
          Caption = 'Option File:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Transparent = True
          Layout = tlCenter
        end
        object OptionFileLabel: TTntLabel
          Left = 88
          Top = 3
          Width = 251
          Height = 20
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'No config file selected'
          Color = clBtnFace
          ParentColor = False
          Transparent = True
          Layout = tlCenter
        end
        object SectionCaptionLbl: TTntLabel
          Left = 4
          Top = 25
          Width = 78
          Height = 20
          AutoSize = False
          Caption = 'Section:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Transparent = True
          Layout = tlCenter
          Visible = False
        end
      end
    end
  end
  object SubTreePnl: TTntPanel
    Left = 0
    Top = 0
    Width = 125
    Height = 669
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object ServicesTreeView: TTntTreeView
      Left = 0
      Top = 19
      Width = 125
      Height = 650
      Align = alClient
      HideSelection = False
      Images = ApplicationDM.AdminTree16ImageList
      Indent = 19
      ReadOnly = True
      RowSelect = True
      ShowRoot = False
      TabOrder = 0
      OnChange = ServicesTreeViewChange
      OnDeletion = ServicesTreeViewDeletion
    end
    object Panel2: TTntPanel
      Left = 0
      Top = 0
      Width = 125
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label15: TTntLabel
        Left = 2
        Top = 2
        Width = 86
        Height = 13
        Caption = 'Installed Services:'
      end
    end
  end
  object SheetHeaderPnl: TTntPanel
    Left = 139
    Top = 19
    Width = 649
    Height = 49
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    object ServiceSettingsHeaderImg: TTntImage
      Left = 11
      Top = 2
      Width = 24
      Height = 24
      AutoSize = True
      Transparent = True
    end
    object SheetHeaderLbl: TTntLabel
      Left = 48
      Top = 6
      Width = 94
      Height = 13
      Caption = 'Service Settings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ServiceSettingsBevel: TTntBevel
      Left = 12
      Top = 38
      Width = 623
      Height = 3
      Shape = bsTopLine
    end
    object Label1: TTntLabel
      Left = 48
      Top = 20
      Width = 453
      Height = 13
      Caption = 
        'Configure the startup variables. Note that changes will have no ' +
        'effect until you restart the server.'
    end
  end
end
