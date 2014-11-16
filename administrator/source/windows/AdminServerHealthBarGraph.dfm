object AdminServerHealthBarGraphFrame: TAdminServerHealthBarGraphFrame
  Left = 0
  Top = 0
  Width = 500
  Height = 18
  TabOrder = 0
  TabStop = True
  OnDblClick = TntFrameDblClick
  OnResize = FrameResize
  object MainPnl: TTntPanel
    Left = 94
    Top = 0
    Width = 406
    Height = 18
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
    OnDblClick = TntFrameDblClick
    DesignSize = (
      402
      14)
    object EmptyImg: TTntImage
      Left = 0
      Top = 0
      Width = 402
      Height = 14
      Align = alClient
      Stretch = True
      OnDblClick = TntFrameDblClick
    end
    object ValueImg: TTntImage
      Left = 0
      Top = 0
      Width = 105
      Height = 15
      Stretch = True
      OnDblClick = TntFrameDblClick
    end
    object ValueLbl: TTntLabel
      Left = 2
      Top = 0
      Width = 93
      Height = 13
      Caption = 'Current usage 2.1M'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
      OnDblClick = TntFrameDblClick
    end
    object MaxLbl: TTntLabel
      Left = 328
      Top = 0
      Width = 63
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Total size 6M'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
  end
  object CaptionPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 94
    Height = 18
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    OnDblClick = TntFrameDblClick
    object CaptionLbl: TTntLabel
      Left = 0
      Top = 2
      Width = 50
      Height = 13
      Caption = 'CaptionLbl'
      OnDblClick = TntFrameDblClick
    end
  end
end
