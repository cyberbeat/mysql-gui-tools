object AdminServerProcessesForm: TAdminServerProcessesForm
  Left = 496
  Top = 233
  Width = 599
  Height = 484
  Caption = 'Server Processes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServerProcessesPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 591
    Height = 450
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object BottomPnl: TTntPanel
      Left = 0
      Top = 405
      Width = 591
      Height = 45
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object BottomBtnPnl: TTntPanel
        Left = 235
        Top = 0
        Width = 356
        Height = 45
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object Button4: TTntButton
          Left = 238
          Top = 8
          Width = 105
          Height = 25
          Caption = 'Refresh'
          TabOrder = 0
        end
      end
    end
    object ListView1: TTntListView
      Left = 0
      Top = 0
      Width = 591
      Height = 405
      Align = alClient
      Columns = <
        item
          Caption = 'PID'
        end
        item
          Caption = 'User'
          Width = 80
        end
        item
          AutoSize = True
          Caption = 'Host'
        end
        item
          Caption = 'DB'
          Width = 80
        end
        item
          Caption = 'Command'
          Width = 80
        end
        item
          Caption = 'Time'
        end
        item
          Caption = 'State'
          Width = 60
        end
        item
          Caption = 'Info'
          Width = 70
        end>
      Items.Data = {
        510000000100000000000000FFFFFFFFFFFFFFFF050000000000000002363504
        726F6F740E6C6F63616C686F73743A31353230056D7973716C0B50726F636573
        736C6973740130FFFFFFFFFFFFFFFFFFFF}
      TabOrder = 1
      ViewStyle = vsReport
    end
  end
end
