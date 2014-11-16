object MainForm: TMainForm
  Left = 370
  Top = 286
  AlphaBlend = True
  AlphaBlendValue = 200
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 169
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PopupMenu: TTntPopupMenu
    AutoHotkeys = maManual
    OwnerDraw = True
    OnPopup = PopupMenuPopup
    Left = 110
    Top = 8
    object MySQLAdministratorMI: TTntMenuItem
      Caption = 'MySQL Administrator'
      ImageIndex = 0
      OnClick = MySQLAdministratorMIClick
      OnDrawItem = DrawItem
      OnMeasureItem = MeasureItem
    end
    object MySQLQueryBrowserMI: TTntMenuItem
      Caption = 'MySQL Query Browser'
      ImageIndex = 1
      OnClick = MySQLQueryBrowserMIClick
      OnDrawItem = DrawItem
      OnMeasureItem = MeasureItem
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object ActionsMI: TTntMenuItem
      Caption = 'Actions'
      OnDrawItem = DrawItem
      OnMeasureItem = MeasureItem
      object ManageInstancesMI: TTntMenuItem
        Caption = 'Manage MySQL Instances'
        OnClick = ManageInstancesMIClick
      end
    end
    object MontorOptions1: TTntMenuItem
      Caption = 'Monitor Options'
      OnDrawItem = DrawItem
      OnMeasureItem = MeasureItem
      object DisplayCPULoadMI: TTntMenuItem
        Caption = 'Display CPU Load'
        Checked = True
        OnClick = DisplayCPULoadMIClick
        OnDrawItem = DrawSubMenuCheckItem
        OnMeasureItem = MeasureSubMenuCheckItem
      end
      object StartMonitorAutomaticallyMI: TTntMenuItem
        Caption = 'Launch Monitor After Login'
        OnClick = StartMonitorAutomaticallyMIClick
        OnDrawItem = DrawSubMenuCheckItem
        OnMeasureItem = MeasureSubMenuCheckItem
      end
      object OnlyScanServicesStartingWithmysqlMI: TTntMenuItem
        Caption = 'Only Scan Services Starting With "mysql"'
        Checked = True
        OnClick = OnlyScanServicesStartingWithmysqlMIClick
        OnDrawItem = DrawSubMenuCheckItem
        OnMeasureItem = MeasureSubMenuCheckItem
      end
      object ScanIntervallMI: TTntMenuItem
        Caption = 'Scan Interval'
        OnDrawItem = DrawSubMenuItem
        OnMeasureItem = MeasureSubMenuCheckItem
        object N1Second1: TTntMenuItem
          Tag = 1
          Caption = '1 Second'
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
        object N2Seconds1: TTntMenuItem
          Tag = 2
          Caption = '2 Seconds'
          Checked = True
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
        object N5Seconds1: TTntMenuItem
          Tag = 5
          Caption = '5 Seconds'
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
        object N10Sec1: TTntMenuItem
          Tag = 10
          Caption = '10 Seconds'
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
        object N30Sec1: TTntMenuItem
          Tag = 30
          Caption = '30 Seconds'
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
        object N1Minute1: TTntMenuItem
          Tag = 60
          Caption = '1 Minute'
          OnClick = N1Second1Click
          OnDrawItem = DrawSubMenuCheckItem
          OnMeasureItem = MeasureSubMenuCheckItem
        end
      end
    end
    object N1: TTntMenuItem
      Caption = '-'
      OnMeasureItem = MeasureItem
    end
    object CloseMonitorMI: TTntMenuItem
      Caption = 'Close Monitor'
      OnClick = CloseMonitorMIClick
      OnDrawItem = CloseMonitorMIDrawItem
      OnMeasureItem = MeasureItem
    end
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 142
    Top = 8
  end
end
