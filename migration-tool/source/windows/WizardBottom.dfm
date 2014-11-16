object WizardBottomFrame: TWizardBottomFrame
  Left = 0
  Top = 0
  Width = 749
  Height = 52
  TabOrder = 0
  TabStop = True
  object BottomPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 749
    Height = 52
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      749
      52)
    object TntBevel1: TTntBevel
      Left = 0
      Top = 0
      Width = 749
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object BackBtn: TTntButton
      Left = 484
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '< Back'
      TabOrder = 3
      OnClick = BackBtnClick
    end
    object NextBtn: TTntButton
      Left = 562
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      Default = True
      TabOrder = 0
      OnClick = NextBtnClick
    end
    object CancelBtn: TTntButton
      Left = 656
      Top = 14
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 1
      OnClick = CancelBtnClick
    end
    object DetailsBtn: TTntButton
      Left = 14
      Top = 14
      Width = 85
      Height = 25
      Caption = 'Details >>'
      TabOrder = 2
      Visible = False
      OnClick = DetailsBtnClick
    end
  end
end
