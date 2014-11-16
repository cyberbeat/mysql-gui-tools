object WizardHeaderFrame: TWizardHeaderFrame
  Left = 0
  Top = 0
  Width = 676
  Height = 57
  TabOrder = 0
  TabStop = True
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 676
    Height = 57
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      676
      57)
    object TntShape1: TTntShape
      Left = 0
      Top = 0
      Width = 676
      Height = 55
      Align = alClient
      Pen.Style = psClear
    end
    object HeaderTitleLbl: TTntLabel
      Left = 14
      Top = 13
      Width = 99
      Height = 15
      Caption = 'Source Database'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = True
    end
    object UserInfoBevel: TTntBevel
      Left = 0
      Top = 55
      Width = 676
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object Shape1: TTntShape
      Left = 619
      Top = 0
      Width = 1
      Height = 55
      Anchors = [akTop, akRight]
      Brush.Style = bsClear
      Pen.Color = clBtnShadow
    end
    object HeaderInfoLbl: TTntLabel
      Left = 32
      Top = 28
      Width = 545
      Height = 20
      AutoSize = False
      Caption = 'HeaderInfoLbl'
      Color = clWhite
      ParentColor = False
      Transparent = True
      WordWrap = True
    end
    object AnimPnl: TTntPanel
      Left = 620
      Top = 0
      Width = 56
      Height = 54
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
      object AnimStillImg: TTntImage
        Left = 0
        Top = 0
        Width = 56
        Height = 54
        Align = alClient
      end
      object BusyAnimate: TAnimate
        Left = 0
        Top = 0
        Width = 56
        Height = 54
        Align = alClient
        StopFrame = 74
        Visible = False
      end
    end
  end
end
