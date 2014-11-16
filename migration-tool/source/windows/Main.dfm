object MainForm: TMainForm
  Left = 179
  Top = 108
  Caption = 'MySQL Migration Toolkit'
  ClientHeight = 680
  ClientWidth = 1016
  Color = clBtnFace
  Constraints.MinHeight = 694
  Constraints.MinWidth = 900
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnCreate = TntFormCreate
  OnDestroy = TntFormDestroy
  OnKeyDown = TntFormKeyDown
  OnResize = TntFormResize
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TTntStatusBar
    Left = 0
    Top = 661
    Width = 1016
    Height = 19
    Panels = <>
  end
  object SplitterPnl: TTntPanel
    Left = 243
    Top = 4
    Width = 10
    Height = 653
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterImg: TTntImage
      Left = 1
      Top = 284
      Width = 7
      Height = 44
      AutoSize = True
      Picture.Data = {
        07544269746D617056040000424D560400000000000036000000280000000700
        00002C000000010018000000000020040000120B0000120B0000000000000000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0E3DFE0E5E2E3F7F7
        F7FBFBFBE3DFE0000000E3DFE0E3DFE0E3DFE0F4F4F4B3B0AFF8F8F8E3DFE000
        0000E3DFE0E0DCDECBD0D59C9999999594F6F5F5E3DFE0000000E3DFE0B9B4BA
        9894958985849D9A99F7F6F6E3DFE0000000E3DFE0E0DCDDC0B9BF928E8E9C98
        97F8F4F7E3DFE0000000E3DFE0E3DFE0E3DFE0BDB8BDA4A0A1F3F2F2E3DFE000
        0000E3DFE0E3DFE0E3DFE0DDD8DAC2BDC2E6E2E3E3DFE0000000E3DFE0E3DFE0
        E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0E3DFE0E3DFE0E3DF
        E0E3DFE0E3DFE0000000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E7E7E7FFFFFFFFFBFFE3DFE0E3DFE0000000E3DFE0E3DFE0B8B2B88E8A89FFFF
        FFE3DFE0E3DFE0000000E3DFE0E3DFE0C5CFD6B4ADB4E7EBEFE3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E7E7E7FFFFFFFFFBFFE3DFE0E3DFE0000000E3DFE0E3DFE0B8B2B88E8A89FFFF
        FFE3DFE0E3DFE0000000E3DFE0E3DFE0C5CFD6B4ADB4E7EBEFE3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E7E7E7FFFFFFFFFBFFE3DFE0E3DFE0000000E3DFE0E3DFE0B8B2B88E8A89FFFF
        FFE3DFE0E3DFE0000000E3DFE0E3DFE0C5CFD6B4ADB4E7EBEFE3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E7E7E7FFFFFFFFFBFFE3DFE0E3DFE0000000E3DFE0E3DFE0B8B2B88E8A89FFFF
        FFE3DFE0E3DFE0000000E3DFE0E3DFE0C5CFD6B4ADB4E7EBEFE3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E7E7E7FFFFFFFFFBFFE3DFE0E3DFE0000000E3DFE0E3DFE0B8B2B88E8A89FFFF
        FFE3DFE0E3DFE0000000E3DFE0E3DFE0C5CFD6B4ADB4E7EBEFE3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0
        E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0000000E3DFE0E3DFE0E3DFE0E3DFE0E3DF
        E0E3DFE0E3DFE0000000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE000
        0000E3DFE0E3DFE0E3DFE0E5E2E3F7F7F7FBFBFBE3DFE0000000E3DFE0E3DFE0
        E3DFE0F4F4F4B3B0AFF8F8F8E3DFE0000000E3DFE0E0DCDECBD0D59C99999995
        94F6F5F5E3DFE0000000E3DFE0B9B4BA9894958985849D9A99F7F6F6E3DFE000
        0000E3DFE0E0DCDDC0B9BF928E8E9C9897F8F4F7E3DFE0000000E3DFE0E3DFE0
        E3DFE0BDB8BDA4A0A1F3F2F2E3DFE0000000E3DFE0E3DFE0E3DFE0DDD8DAC2BD
        C2E6E2E3E3DFE0000000E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE0E3DFE000
        0000}
      OnClick = SplitterImgClick
    end
  end
  object LeftPnl: TTntPanel
    Left = 0
    Top = 4
    Width = 4
    Height = 653
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
  end
  object TntPanel2: TTntPanel
    Left = 1012
    Top = 4
    Width = 4
    Height = 653
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
  end
  object TntPanel3: TTntPanel
    Left = 0
    Top = 0
    Width = 1016
    Height = 4
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
  end
  object TntPanel4: TTntPanel
    Left = 0
    Top = 657
    Width = 1016
    Height = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
  end
  object MigrationPlanPnl: TTntPanel
    Left = 4
    Top = 4
    Width = 239
    Height = 653
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 6
  end
  object MainMenu: TTntMainMenu
    Left = 204
    Top = 8
    object FileMI: TTntMenuItem
      Caption = 'File'
      object LoadApplicationSnapshotMI: TTntMenuItem
        Caption = 'Reload stored application state ...'
        OnClick = LoadApplicationSnapshotMIClick
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object StoreApplicationSnapshotMI: TTntMenuItem
        Caption = 'Store current application state ...'
        OnClick = StoreApplicationSnapshotMIClick
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object GenerateMigrationScriptMI: TTntMenuItem
        Caption = 'Generate Migration Script ...'
        OnClick = GenerateMigrationScriptMIClick
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object ExitMI: TTntMenuItem
        Caption = 'Exit'
        OnClick = ExitMIClick
      end
    end
    object EditMI: TTntMenuItem
      Caption = 'Edit'
      object CopyMI: TTntMenuItem
        Caption = 'Copy'
        Enabled = False
      end
      object CutMI: TTntMenuItem
        Caption = 'Cut'
        Enabled = False
      end
      object PasteMI: TTntMenuItem
        Caption = 'Paste'
        Enabled = False
      end
    end
    object ToolsMI: TTntMenuItem
      Caption = 'Tools'
      object ObjectShellMI: TTntMenuItem
        Caption = 'GRT Environment Shell'
        OnClick = ObjectShellMIClick
      end
    end
    object WindowMI: TTntMenuItem
      Caption = 'Window'
      object N01: TTntMenuItem
        Caption = '0'
      end
    end
    object HelpMI: TTntMenuItem
      Caption = 'Help'
      object OnlineHelpMI: TTntMenuItem
        Caption = 'Help'
        OnClick = OnlineHelpMIClick
      end
      object N4: TTntMenuItem
        Caption = '-'
      end
      object ListReportedBugs: TTntMenuItem
        Caption = 'List Reported Bugs'
        OnClick = ListReportedBugsClick
      end
      object ReportBugMI: TTntMenuItem
        Caption = 'Report a New Bug to MySQL'
        OnClick = ReportBugMIClick
      end
      object N5: TTntMenuItem
        Caption = '-'
      end
      object AboutMI: TTntMenuItem
        Caption = 'About'
        OnClick = AboutMIClick
      end
    end
  end
end
