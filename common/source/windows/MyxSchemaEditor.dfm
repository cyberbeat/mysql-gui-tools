object MyxSchemaEditorForm: TMyxSchemaEditorForm
  Left = 1301
  Top = 173
  Width = 486
  Height = 317
  Caption = 'Schema Editor'
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      478
      49)
    object NameLbl: TTntLabel
      Left = 10
      Top = 18
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Schema Name:'
    end
    object TntLabel1: TTntLabel
      Left = 268
      Top = 18
      Width = 45
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Collation:'
    end
    object NameEd: TTntEdit
      Left = 86
      Top = 14
      Width = 163
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = DataChange
    end
    object CollationComboBox: TTntComboBox
      Left = 322
      Top = 14
      Width = 140
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = DataChange
    end
  end
  object LeftPnl: TTntPanel
    Left = 0
    Top = 49
    Width = 13
    Height = 188
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object RightPnl: TTntPanel
    Left = 465
    Top = 49
    Width = 13
    Height = 188
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
  object BottomPnl: TTntPanel
    Left = 0
    Top = 237
    Width = 478
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      478
      51)
    object ApplyChangesBtn: TTntButton
      Left = 247
      Top = 13
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Apply Changes'
      Enabled = False
      TabOrder = 0
      OnClick = ApplyChangesBtnClick
    end
    object CancelBtn: TTntButton
      Left = 363
      Top = 13
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object PageControl: TTntPageControl
    Left = 13
    Top = 49
    Width = 452
    Height = 188
    ActivePage = CommentTabSheet
    Align = alClient
    TabOrder = 4
    object CommentTabSheet: TTntTabSheet
      Caption = 'Comment'
      object CommentMemo: TTntMemo
        Left = 0
        Top = 0
        Width = 444
        Height = 160
        Align = alClient
        TabOrder = 0
        OnChange = DataChange
      end
    end
  end
end
