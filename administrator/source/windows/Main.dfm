object MainForm: TMainForm
  Left = 658
  Top = 300
  Caption = 'MySQL Administrator'
  ClientHeight = 632
  ClientWidth = 772
  Color = clBtnFace
  Constraints.MinHeight = 540
  Constraints.MinWidth = 780
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = TntFormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TTntStatusBar
    Left = 0
    Top = 613
    Width = 772
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Width = 20
      end
      item
        Width = 50
      end>
    OnMouseDown = StatusBarMouseDown
    OnDrawPanel = StatusBarDrawPanel
    ExplicitTop = 593
  end
  object TopPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 772
    Height = 3
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel1: TTntPanel
    Left = 0
    Top = 610
    Width = 772
    Height = 3
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 590
  end
  object Panel2: TTntPanel
    Left = 769
    Top = 3
    Width = 3
    Height = 607
    Align = alRight
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 3
    ExplicitHeight = 587
  end
  object MainMenu: TTntMainMenu
    AutoHotkeys = maManual
    Left = 282
    Top = 66
    object FileMI: TTntMenuItem
      Caption = 'File'
      object ConnecttoServerMI: TTntMenuItem
        Caption = 'New Instance Connection ...'
        OnClick = ConnecttoServerMIClick
      end
      object ReconnectMI: TTntMenuItem
        Caption = 'Reconnect'
        OnClick = ReconnectMIClick
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object CopyActivePageAsTextMI: TTntMenuItem
        Caption = 'Copy active page as text'
        Enabled = False
        OnClick = CopyActivePageAsTextMIClick
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object CloseMI: TTntMenuItem
        Caption = 'Close'
        OnClick = CloseMIClick
      end
    end
    object Edit1: TTntMenuItem
      Caption = 'Edit'
      object CutMI: TTntMenuItem
        Caption = 'Cut'
        Enabled = False
        OnClick = CutMIClick
      end
      object CopyMI: TTntMenuItem
        Caption = 'Copy'
        Enabled = False
        OnClick = CopyMIClick
      end
      object PasteMI: TTntMenuItem
        Caption = 'Paste'
        Enabled = False
        OnClick = PasteMIClick
      end
      object SelectAllMI: TTntMenuItem
        Caption = 'Select All'
        Enabled = False
        OnClick = SelectAllMIClick
      end
    end
    object ViewMI: TTntMenuItem
      Caption = 'View'
    end
    object ToolsMI: TTntMenuItem
      Caption = 'Tools'
      object MySQLQueryBrowserMI: TTntMenuItem
        Caption = 'MySQL Query Browser'
        OnClick = MySQLQueryBrowserMIClick
      end
      object MySQLCommandlineclientMI: TTntMenuItem
        Caption = 'MySQL Command Line Client'
        OnClick = MySQLCommandlineclientMIClick
      end
      object MySQLSystemTrayMonitorMI: TTntMenuItem
        Caption = 'MySQL System Tray Monitor'
        OnClick = MySQLSystemTrayMonitorMIClick
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object WindowsCommandLineMI: TTntMenuItem
        Caption = 'Windows Command Line'
        OnClick = WindowsCommandLineMIClick
      end
      object N9: TTntMenuItem
        Caption = '-'
      end
      object ManageConnectionsMI: TTntMenuItem
        Caption = 'Manage Connections ...'
        OnClick = ManageConnectionsMIClick
      end
      object SaveConnectionMI: TTntMenuItem
        Caption = 'Save current Connection ...'
        OnClick = SaveConnectionMIClick
      end
      object N4: TTntMenuItem
        Caption = '-'
      end
      object OptionsMI: TTntMenuItem
        Caption = 'Options ...'
        OnClick = OptionsMIClick
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
      object N6: TTntMenuItem
        Caption = '-'
      end
      object VisitMySQLcomMI: TTntMenuItem
        Caption = 'Launch MySQL.com website'
        OnClick = VisitMySQLcomMIClick
      end
      object N5: TTntMenuItem
        Caption = '-'
      end
      object ListopenMySQLAdministratorBugsMI: TTntMenuItem
        Caption = 'List reported Bugs'
        OnClick = ListopenMySQLAdministratorBugsMIClick
      end
      object ReportBugMI: TTntMenuItem
        Caption = 'Report a new Bug to MySQL'
        OnClick = ReportBugMIClick
      end
      object N7: TTntMenuItem
        Caption = '-'
      end
      object AboutMI: TTntMenuItem
        Caption = 'About ...'
        OnClick = AboutMIClick
      end
    end
  end
end
