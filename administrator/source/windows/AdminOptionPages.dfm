object AdminOptionPagesForm: TAdminOptionPagesForm
  Left = 203
  Top = 231
  Caption = 'AdminOptionPagesForm'
  ClientHeight = 346
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OptionPageControl: TTntPageControl
    Left = 0
    Top = 0
    Width = 559
    Height = 345
    ActivePage = AdminGeneralTabSheet
    Style = tsFlatButtons
    TabHeight = 16
    TabOrder = 0
    object AdminGeneralTabSheet: TTabSheet
      Caption = 'Administrator'
      ImageIndex = 2
      object GroupBox4: TTntGroupBox
        Left = 0
        Top = 6
        Width = 267
        Height = 99
        Caption = 'User Administration'
        TabOrder = 0
        object AdminShowGlobalPrivCBox: TTntCheckBox
          Left = 16
          Top = 20
          Width = 241
          Height = 17
          Caption = 'Show Global Privileges'
          TabOrder = 0
          OnClick = DoChange
        end
        object AdminShowTblColPrivCBox: TTntCheckBox
          Left = 16
          Top = 48
          Width = 241
          Height = 17
          Caption = 'Show Schema Object Privileges'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = DoChange
        end
        object AdminShowHostsCheckbox: TTntCheckBox
          Left = 16
          Top = 76
          Width = 241
          Height = 17
          Caption = 'Show hosts in user list'
          TabOrder = 2
          OnClick = DoChange
        end
      end
      object GroupBox1: TTntGroupBox
        Left = 0
        Top = 111
        Width = 267
        Height = 170
        Caption = 'Health Graphs'
        TabOrder = 1
        object PeakLevelResetTicksLbl: TTntLabel
          Left = 76
          Top = 113
          Width = 181
          Height = 40
          AutoSize = False
          Caption = 'Ticks after the Peak level resets'
          WordWrap = True
        end
        object UsePeakLevelCBox: TTntCheckBox
          Left = 16
          Top = 43
          Width = 241
          Height = 17
          Caption = 'Use Peak Level Indicator'
          TabOrder = 0
          OnClick = DoChange
        end
        object ResetPeakLevelCBox: TTntCheckBox
          Left = 16
          Top = 75
          Width = 241
          Height = 17
          Caption = 'Reset Peak Level after a number of ticks'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = ResetPeakLevelCBoxClick
        end
        object PeakLevelResetTicksEd: TTntEdit
          Left = 16
          Top = 109
          Width = 35
          Height = 21
          TabOrder = 2
          Text = '0'
        end
        object PeakLevelResetTicksUpDown: TTntUpDown
          Left = 51
          Top = 109
          Width = 16
          Height = 21
          Associate = PeakLevelResetTicksEd
          TabOrder = 3
        end
      end
      object GroupBox2: TTntGroupBox
        Left = 282
        Top = 6
        Width = 267
        Height = 275
        Caption = 'Backup'
        TabOrder = 2
        object LogFilePathLbl: TTntLabel
          Left = 16
          Top = 140
          Width = 65
          Height = 13
          Caption = 'Log File Path:'
        end
        object LogEveryLbl: TTntLabel
          Left = 16
          Top = 234
          Width = 140
          Height = 13
          Caption = 'Write a new  log line for every'
        end
        object LogEveryRowLbl: TTntLabel
          Left = 234
          Top = 233
          Width = 22
          Height = 13
          Caption = 'Row'
        end
        object TntLabel1: TTntLabel
          Left = 34
          Top = 67
          Width = 211
          Height = 67
          AutoSize = False
          Caption = 
            'Please note that events will be written to the Windows Event Vie' +
            'wer even if this option is not selected.'
          WordWrap = True
        end
        object AddDateTimeToBackupFilesCBox: TTntCheckBox
          Left = 16
          Top = 20
          Width = 241
          Height = 17
          Caption = 'Add Date/Time to Backup Files'
          TabOrder = 0
          OnClick = DoChange
        end
        object WriteBackupLogfilePathCBox: TTntCheckBox
          Left = 16
          Top = 44
          Width = 241
          Height = 17
          Caption = 'Write Log File for Scheduled Backups'
          TabOrder = 1
          OnClick = DoChange
        end
        object BackupLogFilePathEd: TTntEdit
          Left = 14
          Top = 159
          Width = 204
          Height = 21
          TabOrder = 2
          OnChange = DoChange
        end
        object BackupLogFileBrowseBtn: TTntButton
          Left = 224
          Top = 159
          Width = 25
          Height = 21
          Caption = '...'
          TabOrder = 3
          OnClick = BackupLogFileBrowseBtnClick
        end
        object LogRowProcessingProgressCBox: TTntCheckBox
          Left = 16
          Top = 202
          Width = 241
          Height = 17
          Caption = 'Log Row Processing Progress'
          TabOrder = 4
          OnClick = DoChange
        end
        object LogEveryEd: TTntEdit
          Left = 191
          Top = 230
          Width = 37
          Height = 21
          TabOrder = 5
          OnChange = DoChange
        end
      end
    end
  end
end
