object AdminCatalogTableCheckForm: TAdminCatalogTableCheckForm
  Left = 560
  Top = 216
  BorderStyle = bsDialog
  Caption = 'Table Maintenance'
  ClientHeight = 566
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    484
    566)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlHidePnl: TTntPanel
    Left = 4
    Top = 6
    Width = 471
    Height = 13
    BevelOuter = bvNone
    TabOrder = 1
  end
  object PageControl: TTntPageControl
    Left = 6
    Top = 6
    Width = 475
    Height = 505
    ActivePage = RepairTabSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    Style = tsFlatButtons
    TabHeight = 6
    TabOrder = 0
    ExplicitHeight = 483
    object SelectTabSheet: TTabSheet
      Caption = 'SelectTabSheet'
      ImageIndex = 4
      ExplicitHeight = 467
      object Label21: TTntLabel
        Left = 10
        Top = 0
        Width = 116
        Height = 13
        Caption = 'Tables Maintenance'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label22: TTntLabel
        Left = 10
        Top = 13
        Width = 304
        Height = 13
        Caption = 
          'Please select the task you want to perform on the seleted tables' +
          '.'
      end
      object GroupBox5: TTntGroupBox
        Left = 10
        Top = 36
        Width = 447
        Height = 147
        Caption = 'Tasks'
        TabOrder = 0
        object Label23: TTntLabel
          Left = 146
          Top = 28
          Width = 290
          Height = 26
          AutoSize = False
          Caption = 
            'If appropriate this will repair the table, sort the index pages ' +
            'and update the statistics.'
          WordWrap = True
        end
        object Label24: TTntLabel
          Left = 146
          Top = 70
          Width = 290
          Height = 26
          AutoSize = False
          Caption = 
            'This will check whether the selected tables are corrupted or hav' +
            'e other errors.'
          WordWrap = True
        end
        object Label25: TTntLabel
          Left = 146
          Top = 110
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'This will repair the selected tables.'
          WordWrap = True
        end
        object OptimizeTablesRBtn: TTntRadioButton
          Left = 18
          Top = 26
          Width = 113
          Height = 17
          Caption = 'Optimize Tables'
          TabOrder = 0
        end
        object CheckTablesRBtn: TTntRadioButton
          Left = 18
          Top = 68
          Width = 113
          Height = 17
          Caption = 'Check Tables'
          TabOrder = 1
        end
        object RepairCheckTablesRBtn: TTntRadioButton
          Left = 18
          Top = 108
          Width = 113
          Height = 17
          Caption = 'Repair Tables'
          TabOrder = 2
        end
      end
      object GroupBox6: TTntGroupBox
        Left = 10
        Top = 194
        Width = 447
        Height = 215
        Caption = 'List of Tables'
        TabOrder = 1
        object Label26: TTntLabel
          Left = 250
          Top = 24
          Width = 177
          Height = 173
          AutoSize = False
          Caption = 
            'The selected task will be executed on each table in the list. To' +
            ' change the selection close this dialog and change the selection' +
            '.'
          WordWrap = True
        end
        object TableListBox: TTntListBox
          Left = 20
          Top = 24
          Width = 217
          Height = 173
          ExtendedSelect = False
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object OptimizeTabSheet: TTabSheet
      Caption = 'OptimizeTabSheet'
      ImageIndex = 2
      ExplicitHeight = 415
      object Label5: TTntLabel
        Left = 10
        Top = 0
        Width = 87
        Height = 13
        Caption = 'Optimize tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TTntLabel
        Left = 10
        Top = 17
        Width = 447
        Height = 29
        AutoSize = False
        Caption = 
          'If appropriate this will repair the table, sort the index pages ' +
          'and update the statistics.'
        WordWrap = True
      end
      object GroupBox2: TTntGroupBox
        Left = 10
        Top = 52
        Width = 447
        Height = 61
        Caption = 'Optimize Options'
        TabOrder = 0
        object OptLocalLbl: TTntLabel
          Left = 146
          Top = 28
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'Do not write the command to the binary log file.'
          WordWrap = True
        end
        object OptLocalCBox: TTntCheckBox
          Left = 18
          Top = 26
          Width = 100
          Height = 17
          Caption = 'Local'
          TabOrder = 0
        end
      end
    end
    object OptionsTabSheet: TTabSheet
      Caption = 'OptionsTabSheet'
      ExplicitHeight = 415
      object Label1: TTntLabel
        Left = 10
        Top = 21
        Width = 447
        Height = 33
        AutoSize = False
        Caption = 
          'This will check whether the selected tables are corrupted or hav' +
          'e other errors.'
        WordWrap = True
      end
      object Label4: TTntLabel
        Left = 10
        Top = 0
        Width = 75
        Height = 13
        Caption = 'Check tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object GroupBox1: TTntGroupBox
        Left = 10
        Top = 60
        Width = 447
        Height = 273
        Caption = 'Check method'
        TabOrder = 0
        object Label7: TTntLabel
          Left = 146
          Top = 28
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'Don'#39't scan the rows to check for incorrect links.'
          WordWrap = True
        end
        object Label8: TTntLabel
          Left = 146
          Top = 60
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'Only check tables that haven'#39't been closed properly.'
          WordWrap = True
        end
        object Label9: TTntLabel
          Left = 146
          Top = 92
          Width = 290
          Height = 40
          AutoSize = False
          Caption = 
            'Only check tables that have been changed since the last check or' +
            ' haven'#39't been closed properly.'
          WordWrap = True
        end
        object Label10: TTntLabel
          Left = 146
          Top = 138
          Width = 290
          Height = 60
          AutoSize = False
          Caption = 
            'Scan rows to verify that deleted links are okay. This also calcu' +
            'lates a key checksum for the rows and verifies this with a calcu' +
            'lated checksum for the keys.'
          WordWrap = True
        end
        object Label11: TTntLabel
          Left = 146
          Top = 204
          Width = 290
          Height = 49
          AutoSize = False
          Caption = 
            'Do a full key lookup for all keys for each row. This ensures tha' +
            't the table is 100% consistent, but will take a long time.'
          WordWrap = True
        end
        object ChangedRBtn: TTntRadioButton
          Left = 16
          Top = 98
          Width = 100
          Height = 13
          Caption = 'Changed'
          TabOrder = 4
          WordWrap = True
        end
        object QuickRBtn: TTntRadioButton
          Left = 16
          Top = 28
          Width = 100
          Height = 13
          Caption = 'Quick'
          TabOrder = 0
        end
        object FastRBtn: TTntRadioButton
          Left = 16
          Top = 60
          Width = 100
          Height = 13
          Caption = 'Fast'
          TabOrder = 1
        end
        object MediumRBtn: TTntRadioButton
          Left = 16
          Top = 150
          Width = 100
          Height = 13
          Caption = 'Medium'
          Checked = True
          TabOrder = 2
          TabStop = True
          WordWrap = True
        end
        object ExtendedRBtn: TTntRadioButton
          Left = 16
          Top = 216
          Width = 100
          Height = 13
          Caption = 'Extended'
          TabOrder = 3
          WordWrap = True
        end
      end
    end
    object RepairTabSheet: TTabSheet
      Caption = 'RepairTabSheet'
      ImageIndex = 3
      ExplicitHeight = 415
      object Label12: TTntLabel
        Left = 10
        Top = 17
        Width = 447
        Height = 13
        AutoSize = False
        Caption = 'This will repair the selected tables.'
      end
      object Label13: TTntLabel
        Left = 10
        Top = 0
        Width = 76
        Height = 13
        Caption = 'Repair tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object RepairWarning: TTntLabel
        Left = 10
        Top = 312
        Width = 433
        Height = 94
        AutoSize = False
        Caption = 
          'If the MySQL Server dies during a table repair, it'#39's essential t' +
          'hat you do at once another REPAIR  on the table before executing' +
          ' any other commands on it. If that happens, do NOT use MySQL Adm' +
          'inistrator until the tables are properly repaired. Use the REPAI' +
          'R TABLE command from the console client. (It'#39's always good to st' +
          'art by making a backup). '
        WordWrap = True
      end
      object Label19: TTntLabel
        Left = 10
        Top = 294
        Width = 52
        Height = 13
        Caption = 'Warning:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label20: TTntLabel
        Left = 10
        Top = 412
        Width = 433
        Height = 61
        AutoSize = False
        Caption = 
          'In the worst case you can have a new clean index file without in' +
          'formation about the datafile and when the next command you do ma' +
          'y overwrite the datafile. This is not a likely, but possible sce' +
          'nario.'
        WordWrap = True
      end
      object GroupBox3: TTntGroupBox
        Left = 10
        Top = 36
        Width = 447
        Height = 61
        Caption = 'Repair Option'
        TabOrder = 0
        object Label18: TTntLabel
          Left = 146
          Top = 28
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'Do not write the command to the binary log file.'
          WordWrap = True
        end
        object RepLocalCBox: TTntCheckBox
          Left = 18
          Top = 26
          Width = 100
          Height = 17
          Caption = 'Local'
          TabOrder = 0
        end
      end
      object GroupBox4: TTntGroupBox
        Left = 10
        Top = 106
        Width = 447
        Height = 182
        Caption = 'Repair method'
        TabOrder = 1
        object Label14: TTntLabel
          Left = 146
          Top = 20
          Width = 290
          Height = 13
          AutoSize = False
          Caption = 'Only the index tree is repaired.'
          WordWrap = True
        end
        object Label17: TTntLabel
          Left = 146
          Top = 48
          Width = 290
          Height = 69
          AutoSize = False
          Caption = 
            'The index will be created row by row instead of creating the ind' +
            'ex at a time with sorting. This may be better than sorting on fi' +
            'xed-length keys if you have long CHAR keys that compress very we' +
            'll.'
          WordWrap = True
        end
        object Label15: TTntLabel
          Left = 146
          Top = 108
          Width = 290
          Height = 71
          AutoSize = False
          Caption = 
            'A new index file will be created. This can be used if the first ' +
            '16K block of the index file is destroyed or contains incorrect i' +
            'nformation or if the index file is missing.'
          WordWrap = True
        end
        object RepQuickCBox: TTntCheckBox
          Left = 16
          Top = 19
          Width = 97
          Height = 17
          Caption = 'Quick'
          TabOrder = 1
        end
        object RepExtendedCBox: TTntCheckBox
          Left = 16
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Extended'
          TabOrder = 0
        end
        object RepUseFRMCBox: TTntCheckBox
          Left = 16
          Top = 108
          Width = 97
          Height = 17
          Caption = 'Use FRM'
          TabOrder = 2
        end
      end
    end
    object ResultTabSheet: TTabSheet
      Caption = 'ResultTabSheet'
      ImageIndex = 1
      ExplicitHeight = 415
      object Label2: TTntLabel
        Left = 10
        Top = 17
        Width = 445
        Height = 13
        AutoSize = False
        Caption = 'The selected tables have been checked.'
      end
      object Label3: TTntLabel
        Left = 10
        Top = 38
        Width = 67
        Height = 13
        Caption = 'Check Result:'
      end
      object HeaderLbl: TTntLabel
        Left = 10
        Top = 0
        Width = 75
        Height = 13
        Caption = 'Check tables'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ResultMemo: TTntMemo
        Left = 10
        Top = 52
        Width = 445
        Height = 381
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Panel1: TTntPanel
    Left = 0
    Top = 525
    Width = 484
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = -3
    ExplicitTop = 503
    object ExecuteTableActionBtn: TTntButton
      Left = 244
      Top = 2
      Width = 105
      Height = 25
      Caption = 'Next >>'
      TabOrder = 0
      OnClick = ExecuteTableActionBtnClick
    end
    object CloseBtn: TTntButton
      Left = 362
      Top = 2
      Width = 105
      Height = 25
      Caption = 'Close'
      TabOrder = 1
      OnClick = CloseBtnClick
    end
  end
end
