object MigrationObjMapDetailFrame: TMigrationObjMapDetailFrame
  Left = 0
  Top = 0
  Width = 443
  Height = 297
  Align = alTop
  AutoScroll = False
  TabOrder = 0
  DesignSize = (
    443
    297)
  object ObjectSelGBox: TTntGroupBox
    Left = 16
    Top = 7
    Width = 705
    Height = 279
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Mapping of Type Oracle Table'
    TabOrder = 0
    DesignSize = (
      705
      279)
    object StructImage: TTntImage
      Left = 16
      Top = 18
      Width = 32
      Height = 32
    end
    object TntLabel2: TTntLabel
      Left = 61
      Top = 26
      Width = 84
      Height = 13
      Caption = 'Migration method:'
    end
    object DefMethodDescLbl: TTntLabel
      Left = 380
      Top = 26
      Width = 303
      Height = 13
      AutoSize = False
      Caption = 'No migration method available for this source type.'
    end
    object ParameterLbl: TTntLabel
      Left = 62
      Top = 60
      Width = 51
      Height = 13
      Caption = 'Parameter:'
    end
    object MethodComboBox: TTntComboBox
      Left = 164
      Top = 22
      Width = 205
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 13
      ItemIndex = 0
      ParentFont = False
      TabOrder = 1
      Text = 'Standard Migration'
      OnCloseUp = MethodComboBoxCloseUp
      Items.Strings = (
        'Standard Migration')
    end
    object DetailsPnl: TTntPanel
      Left = 163
      Top = 59
      Width = 526
      Height = 119
      Anchors = [akLeft, akTop, akBottom]
      BevelOuter = bvNone
      TabOrder = 2
      object TntLabel5: TTntLabel
        Left = 48
        Top = 76
        Width = 459
        Height = 28
        AutoSize = False
        Caption = 
          'Choose this parameter set for tables that contain lots of data w' +
          'hich does not need transaction safety. This method is ideal for ' +
          'logging information or statistical data.'
        WordWrap = True
      end
      object TntLabel3: TTntLabel
        Left = 48
        Top = 18
        Width = 459
        Height = 28
        AutoSize = False
        Caption = 
          'Standard parameter set. The migrated tables will use the InnoDB ' +
          'storage engine to offer transactional and foreign key support.'
        WordWrap = True
      end
      object TntRadioButton2: TTntRadioButton
        Left = 0
        Top = 58
        Width = 343
        Height = 17
        Caption = 'Statistical data / latin1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object TntRadioButton1: TTntRadioButton
        Left = 0
        Top = 0
        Width = 343
        Height = 17
        Caption = 'Data consistancy / multilanguage'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
      end
    end
    object DetailBtn: TTntButton
      Left = 518
      Top = 240
      Width = 167
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = '<< Hide Detailes'
      TabOrder = 0
      OnClick = DetailBtnClick
    end
    object UserDefinedParamsPnl: TTntPanel
      Left = 162
      Top = 178
      Width = 533
      Height = 53
      Anchors = [akLeft, akBottom]
      BevelOuter = bvNone
      TabOrder = 3
      object ParamsUserDefinedCBox: TTntRadioButton
        Left = 0
        Top = 2
        Width = 215
        Height = 17
        Caption = 'User defined'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = ParamsCBoxClick
      end
      object ParamUserDefinedStrEd: TTntEdit
        Left = 48
        Top = 24
        Width = 473
        Height = 21
        TabOrder = 1
        OnExit = ParamUserDefinedStrEdExit
      end
    end
  end
  object TntPanel1: TTntPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 6
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
end
