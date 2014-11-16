object AdminServerHealthGraphSettingsForm: TAdminServerHealthGraphSettingsForm
  Left = 461
  Top = 143
  ActiveControl = CaptionEd
  BorderStyle = bsSizeToolWin
  Caption = 'Graph Settings'
  ClientHeight = 686
  ClientWidth = 549
  Color = clBtnFace
  Constraints.MinHeight = 688
  Constraints.MinWidth = 502
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    549
    686)
  PixelsPerInch = 96
  TextHeight = 13
  object ValueSettingsGBox: TTntGroupBox
    Left = 14
    Top = 190
    Width = 521
    Height = 188
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Value Settings'
    TabOrder = 1
    DesignSize = (
      521
      188)
    object Label3: TTntLabel
      Left = 14
      Top = 125
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Value Unit:'
    end
    object Label6: TTntLabel
      Left = 14
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Value Formula:'
    end
    object Label8: TTntLabel
      Left = 14
      Top = 157
      Width = 69
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Value Caption:'
    end
    object Label15: TTntLabel
      Left = 120
      Top = 73
      Width = 389
      Height = 42
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Formula to calculate the value. Use [variable] to get values fro' +
        'm variables, ^[variable] to get relative values (relative to las' +
        't evaluation).'
      WordWrap = True
    end
    object Label16: TTntLabel
      Left = 298
      Top = 125
      Width = 211
      Height = 13
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 'The unit of the value.'
    end
    object Label17: TTntLabel
      Left = 298
      Top = 157
      Width = 213
      Height = 13
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 'A caption for the value.'
    end
    object ValueUnitCBox: TTntComboBox
      Left = 120
      Top = 121
      Width = 161
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Percentage'
      Items.Strings = (
        'Percentage'
        'Count'
        'Byte'
        'Seconds')
    end
    object ValueFormulaMemo: TTntMemo
      Left = 120
      Top = 18
      Width = 391
      Height = 50
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnDragDrop = ValueFormulaMemoDragDrop
      OnDragOver = ValueFormulaMemoDragOver
      OnKeyDown = ValueFormulaMemoKeyDown
    end
    object ValueCaptionEd: TTntEdit
      Left = 120
      Top = 153
      Width = 161
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 2
    end
  end
  object CancelBtn: TTntButton
    Left = 455
    Top = 639
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelBtnClick
  end
  object ApplyBtn: TTntButton
    Left = 360
    Top = 639
    Width = 75
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Default = True
    TabOrder = 4
    OnClick = ApplyBtnClick
  end
  object GeneralGBox: TTntGroupBox
    Left = 14
    Top = 10
    Width = 521
    Height = 171
    Anchors = [akLeft, akTop, akRight]
    Caption = 'General'
    TabOrder = 0
    DesignSize = (
      521
      171)
    object CaptionLbl: TTntLabel
      Left = 14
      Top = 66
      Width = 39
      Height = 13
      Caption = 'Caption:'
    end
    object Label2: TTntLabel
      Left = 14
      Top = 25
      Width = 59
      Height = 13
      Caption = 'Graph Type:'
    end
    object Label10: TTntLabel
      Left = 14
      Top = 138
      Width = 66
      Height = 13
      Caption = 'Refresh Time:'
      Enabled = False
    end
    object CaptionDescLbl: TTntLabel
      Left = 296
      Top = 66
      Width = 213
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'The graph'#39's caption.'
    end
    object DisplayCaptionDescLbl: TTntLabel
      Left = 296
      Top = 93
      Width = 213
      Height = 32
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Whether the captions should be displayed or not.'
      WordWrap = True
    end
    object GraphTypeDescLbl: TTntLabel
      Left = 296
      Top = 18
      Width = 213
      Height = 39
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'The type of visual representation of the values over time.'
      WordWrap = True
    end
    object Label14: TTntLabel
      Left = 296
      Top = 138
      Width = 213
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Time to next refresh of the value.'
    end
    object DisplayCaptionCBox: TTntCheckBox
      Left = 120
      Top = 98
      Width = 159
      Height = 17
      Caption = 'Display Caption'
      Enabled = False
      TabOrder = 2
    end
    object CaptionEd: TTntEdit
      Left = 120
      Top = 62
      Width = 161
      Height = 21
      TabOrder = 1
    end
    object GraphTypeCBox: TTntComboBox
      Left = 120
      Top = 21
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Line-Graph'
      OnCloseUp = GraphTypeCBoxCloseUp
      Items.Strings = (
        'Line-Graph'
        'Bar-Graph')
    end
    object RefreshTimeCBox: TTntComboBox
      Left = 120
      Top = 134
      Width = 161
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 3
      Text = '1 sec'
      Items.Strings = (
        '0,2 sec'
        '0,5 sec'
        '1 sec'
        '2 sec'
        '5 sec'
        '10 sec'
        '15 sec'
        '30 sec'
        '1 min')
    end
  end
  object MinMaxGBox: TTntGroupBox
    Left = 14
    Top = 387
    Width = 521
    Height = 237
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Min. / Max. Values'
    TabOrder = 2
    DesignSize = (
      521
      237)
    object Label4: TTntLabel
      Left = 14
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Min. Value:'
    end
    object Label5: TTntLabel
      Left = 14
      Top = 66
      Width = 56
      Height = 13
      Caption = 'Max. Value:'
    end
    object Label7: TTntLabel
      Left = 16
      Top = 154
      Width = 66
      Height = 13
      Caption = 'Max. Formula:'
    end
    object MaxCaptionLbl: TTntLabel
      Left = 16
      Top = 200
      Width = 65
      Height = 13
      Caption = 'Max. Caption:'
    end
    object Label18: TTntLabel
      Left = 296
      Top = 19
      Width = 215
      Height = 38
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Lowest possible value. Lower values will be set to this value.'
      WordWrap = True
    end
    object Label19: TTntLabel
      Left = 296
      Top = 61
      Width = 213
      Height = 36
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Highest possible value. Higher values will be set to this value.'
      WordWrap = True
    end
    object Label20: TTntLabel
      Left = 296
      Top = 97
      Width = 213
      Height = 44
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'The max. value will be overwritten by greater values. This will ' +
        'scale the graph dynamically.'
      WordWrap = True
    end
    object Label21: TTntLabel
      Left = 296
      Top = 147
      Width = 213
      Height = 40
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'The max. value is calculated by this formula. See value formula.'
      WordWrap = True
    end
    object MaxCaptionDescLbl: TTntLabel
      Left = 296
      Top = 197
      Width = 213
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'The caption of the max. value.'
      WordWrap = True
    end
    object MinValueEd: TTntEdit
      Left = 120
      Top = 22
      Width = 161
      Height = 21
      TabOrder = 0
    end
    object MaxValueEd: TTntEdit
      Left = 120
      Top = 64
      Width = 161
      Height = 21
      TabOrder = 1
    end
    object AutoextendValueCBox: TTntCheckBox
      Left = 122
      Top = 108
      Width = 161
      Height = 17
      Caption = 'Autoextend Max. Value'
      TabOrder = 2
    end
    object MaxFormularEd: TTntEdit
      Left = 122
      Top = 150
      Width = 161
      Height = 21
      TabOrder = 3
      OnDragDrop = ValueFormulaMemoDragDrop
      OnDragOver = ValueFormulaMemoDragOver
    end
    object MaxCaptionEd: TTntEdit
      Left = 122
      Top = 196
      Width = 161
      Height = 21
      TabOrder = 4
    end
  end
end
