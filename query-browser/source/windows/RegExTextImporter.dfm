object RegExTextImporterForm: TRegExTextImporterForm
  Left = 274
  Top = 186
  Caption = 'RegEx Text Importer'
  ClientHeight = 664
  ClientWidth = 758
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
  object TntSplitter4: TTntSplitter
    Left = 234
    Top = 39
    Width = 5
    Height = 620
    ExplicitHeight = 627
  end
  object TntShape2: TTntShape
    Left = 0
    Top = 39
    Width = 5
    Height = 620
    Align = alLeft
    Brush.Color = clBtnFace
    Pen.Style = psClear
    ExplicitHeight = 627
  end
  object TntShape3: TTntShape
    Left = 754
    Top = 39
    Width = 4
    Height = 620
    Align = alRight
    Brush.Color = clBtnFace
    Pen.Style = psClear
    ExplicitLeft = 762
    ExplicitHeight = 627
  end
  object TntShape4: TTntShape
    Left = 0
    Top = 659
    Width = 758
    Height = 5
    Align = alBottom
    Brush.Color = clBtnFace
    Pen.Style = psClear
    ExplicitTop = 666
    ExplicitWidth = 766
  end
  object TntShape5: TTntShape
    Left = 0
    Top = 34
    Width = 758
    Height = 5
    Align = alTop
    Brush.Color = clBtnFace
    Pen.Style = psClear
    ExplicitWidth = 766
  end
  object ScrollBox: TTntScrollBox
    Left = 239
    Top = 39
    Width = 515
    Height = 620
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    BevelInner = bvNone
    BorderStyle = bsNone
    TabOrder = 0
    ExplicitWidth = 523
    ExplicitHeight = 627
    object TntSplitter3: TTntSplitter
      Left = 0
      Top = 445
      Width = 515
      Height = 5
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ExplicitTop = 452
      ExplicitWidth = 523
    end
    object ExtractPnl: TTntPanel
      Left = 0
      Top = 0
      Width = 515
      Height = 445
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 523
      ExplicitHeight = 452
      object TntSplitter1: TTntSplitter
        Left = 0
        Top = 109
        Width = 515
        Height = 7
        Cursor = crVSplit
        Align = alTop
        AutoSnap = False
        MinSize = 50
        ExplicitWidth = 523
      end
      object ContentPnl: TTntPanel
        Left = 0
        Top = 116
        Width = 515
        Height = 329
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 523
        ExplicitHeight = 336
        object TntPanel3: TTntPanel
          Left = 0
          Top = 0
          Width = 515
          Height = 329
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitWidth = 523
          ExplicitHeight = 336
          object SourceMemo: TTntMemo
            Left = 0
            Top = 25
            Width = 515
            Height = 304
            Align = alClient
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Bitstream Vera Sans Mono'
            Font.Style = []
            HideSelection = False
            ParentFont = False
            PopupMenu = SourceTextPopupMenu
            ScrollBars = ssBoth
            TabOrder = 0
            WantTabs = True
            WordWrap = False
            OnChange = SourceMemoChange
            ExplicitWidth = 523
            ExplicitHeight = 311
          end
          object TntPanel5: TTntPanel
            Left = 0
            Top = 0
            Width = 515
            Height = 25
            Align = alTop
            Alignment = taLeftJustify
            BevelOuter = bvNone
            Caption = 'Source Text'
            TabOrder = 1
            ExplicitWidth = 523
            DesignSize = (
              515
              25)
            object MatchLU: TTntComboBox
              Left = 66
              Top = 2
              Width = 456
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight, akBottom]
              Enabled = False
              ItemHeight = 13
              TabOrder = 0
              OnCloseUp = MatchLUCloseUp
              Items.Strings = (
                '')
            end
          end
        end
      end
      object RegExPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 515
        Height = 109
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object TntPanel2: TTntPanel
          Left = 0
          Top = 0
          Width = 515
          Height = 17
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          Caption = 'Regular Expression'
          TabOrder = 0
          ExplicitWidth = 523
        end
        object RegExMemo: TTntMemo
          Left = 0
          Top = 17
          Width = 515
          Height = 48
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Bitstream Vera Sans Mono'
          Font.Style = []
          HideSelection = False
          ParentFont = False
          PopupMenu = RegExPopupMenu
          ScrollBars = ssVertical
          TabOrder = 1
          OnChange = RegExMemoChange
          ExplicitWidth = 523
        end
        object TntPanel7: TTntPanel
          Left = 0
          Top = 65
          Width = 515
          Height = 17
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          DesignSize = (
            515
            17)
          object TntShape1: TTntShape
            Left = 0
            Top = -1
            Width = 514
            Height = 18
            Anchors = [akLeft, akTop, akRight, akBottom]
            Pen.Color = clHighlight
            ExplicitWidth = 522
          end
          object RegExErrorLbl: TTntLabel
            Left = 5
            Top = 1
            Width = 51
            Height = 13
            Caption = 'No error.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Transparent = True
            OnDblClick = RegExErrorLblDblClick
          end
        end
        object TntPanel1: TTntPanel
          Left = 0
          Top = 82
          Width = 515
          Height = 27
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            515
            27)
          object ExecuteRegExBtn: TTntButton
            Left = 295
            Top = 6
            Width = 105
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Execute RegEx'
            TabOrder = 0
            OnClick = ExecuteRegExBtnClick
            ExplicitLeft = 303
          end
          object ExecuteNextRegExBtn: TTntButton
            Left = 409
            Top = 6
            Width = 105
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Execute Next'
            TabOrder = 1
            OnClick = ExecuteNextRegExBtnClick
            ExplicitLeft = 417
          end
        end
      end
    end
    object InsertPnl: TTntPanel
      Left = 0
      Top = 450
      Width = 515
      Height = 170
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 457
      ExplicitWidth = 523
      object InsertMemoPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 515
        Height = 170
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 523
        object TntSplitter5: TTntSplitter
          Left = 0
          Top = 65
          Width = 515
          Height = 3
          Cursor = crVSplit
          Align = alTop
          AutoSnap = False
          ExplicitWidth = 523
        end
        object TntPanel6: TTntPanel
          Left = 0
          Top = 0
          Width = 515
          Height = 17
          Align = alTop
          Alignment = taLeftJustify
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitWidth = 523
          object TriggerInsertCBox: TTntCheckBox
            Left = 0
            Top = 0
            Width = 273
            Height = 17
            Caption = 'Trigger Insert Statement'
            TabOrder = 0
            OnClick = TriggerInsertCBoxClick
          end
        end
        object InsertTemplateMemo: TTntMemo
          Left = 0
          Top = 17
          Width = 515
          Height = 48
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Bitstream Vera Sans Mono'
          Font.Style = []
          HideSelection = False
          Lines.Strings = (
            'INSERT INTO foo'
            'VALUES ();')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
          OnChange = InsertTemplateMemoChange
          OnDragDrop = InsertTemplateMemoDragDrop
          OnDragOver = InsertTemplateMemoDragOver
        end
        object InsertsMemo: TTntMemo
          Left = 0
          Top = 68
          Width = 515
          Height = 76
          Align = alClient
          Color = clBtnFace
          HideSelection = False
          TabOrder = 2
          ExplicitWidth = 523
        end
        object TntPanel9: TTntPanel
          Left = 0
          Top = 144
          Width = 515
          Height = 26
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 3
          DesignSize = (
            515
            26)
          object PreviewBtn: TTntButton
            Left = 226
            Top = 5
            Width = 72
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Preview'
            Enabled = False
            TabOrder = 0
            OnClick = PreviewBtnClick
          end
          object StoreSQLBtn: TTntButton
            Left = 308
            Top = 5
            Width = 112
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Store SQL to Script'
            Enabled = False
            TabOrder = 1
            OnClick = StoreSQLBtnClick
          end
          object ExecuteSQLBtn: TTntButton
            Left = 430
            Top = 5
            Width = 92
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Execute SQL'
            Enabled = False
            TabOrder = 2
            OnClick = ExecuteSQLBtnClick
          end
        end
      end
    end
  end
  object StructureTreePnl: TTntPanel
    Left = 5
    Top = 39
    Width = 229
    Height = 620
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object StructureVT: TVirtualStringTree
      Left = 0
      Top = 17
      Width = 229
      Height = 577
      Align = alClient
      DragMode = dmAutomatic
      DragType = dtVCL
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
      PopupMenu = StructureVTPopupMenu
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
      OnDblClick = StructureVTDblClick
      OnEditing = StructureVTEditing
      OnFocusChanged = StructureVTFocusChanged
      OnFocusChanging = StructureVTFocusChanging
      OnFreeNode = StructureVTFreeNode
      OnGetText = StructureVTGetText
      OnPaintText = StructureVTPaintText
      OnInitChildren = StructureVTInitChildren
      OnInitNode = StructureVTInitNode
      OnNewText = StructureVTNewText
      ExplicitHeight = 584
      Columns = <
        item
          Position = 0
          Width = 229
          WideText = 'Parse Structure'
        end>
    end
    object TntPanel4: TTntPanel
      Left = 0
      Top = 0
      Width = 229
      Height = 17
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'Parse Structure'
      TabOrder = 1
    end
    object FoundFuncPnl: TTntPanel
      Left = 0
      Top = 594
      Width = 229
      Height = 26
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        229
        26)
      object CreateNestedExpBtn: TTntButton
        Left = 89
        Top = 5
        Width = 141
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Create Nested Expression'
        Enabled = False
        TabOrder = 0
        OnClick = CreateNestedExpBtnClick
      end
    end
  end
  object TntPanel8: TTntPanel
    Left = 0
    Top = 0
    Width = 758
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 766
    object TntBevel1: TTntBevel
      Left = 0
      Top = 32
      Width = 766
      Height = 2
      Align = alBottom
      Shape = bsTopLine
    end
    object LoadFileBtn: TTntSpeedButton
      Left = 199
      Top = 5
      Width = 110
      Height = 22
      Caption = 'Load Source Text'
      OnClick = LoadFileBtnClick
    end
    object ClearBtn: TTntSpeedButton
      Left = 134
      Top = 5
      Width = 57
      Height = 22
      Caption = 'Clear'
      OnClick = ClearBtnClick
    end
    object SaveBtn: TTntSpeedButton
      Left = 70
      Top = 5
      Width = 57
      Height = 22
      Caption = 'Save'
      OnClick = SaveBtnClick
    end
    object LoadBtn: TTntSpeedButton
      Left = 6
      Top = 5
      Width = 57
      Height = 22
      Caption = 'Load'
      OnClick = LoadBtnClick
    end
    object CaseInsensitiveCBox: TTntCheckBox
      Left = 346
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Case Insensitve'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CaseInsensitiveCBoxClick
    end
    object MultilineStringCBox: TTntCheckBox
      Left = 462
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Multiline String'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CaseInsensitiveCBoxClick
    end
  end
  object SourceTextPopupMenu: TTntPopupMenu
    Left = 649
    Top = 255
    object ExtractRegExMI: TTntMenuItem
      Caption = 'Extract RegEx'
      object ExtractTextandNumbersMI: TTntMenuItem
        Caption = 'Extract Text and Numbers'
        OnClick = ExtractTextandNumbersMIClick
      end
      object ExtractNumbersMI: TTntMenuItem
        Caption = 'Extract Numbers'
        OnClick = ExtractNumbersMIClick
      end
      object ExtractTextMI: TTntMenuItem
        Caption = 'Extract Text'
        OnClick = ExtractTextMIClick
      end
      object ExtractStopwordMI: TTntMenuItem
        Caption = 'Anything But Selected Chars'
        OnClick = ExtractStopwordMIClick
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object AlwaysAssumeMoreThanOneCharMI: TTntMenuItem
        Caption = 'Always Assume More Than One Char'
        Checked = True
        RadioItem = True
        OnClick = AlwaysAssumeMoreThanOneCharMIClick
      end
      object AllowEmptyMatchesMI: TTntMenuItem
        Caption = 'Allow Empty Matches'
        RadioItem = True
        OnClick = AlwaysAssumeMoreThanOneCharMIClick
      end
      object MatchExactCountMI: TTntMenuItem
        Caption = 'Match Exact Count'
        RadioItem = True
        OnClick = AlwaysAssumeMoreThanOneCharMIClick
      end
    end
  end
  object StructureVTPopupMenu: TPopupMenu
    OnPopup = StructureVTPopupMenuPopup
    Left = 109
    Top = 139
    object RemoveRegExMI: TMenuItem
      Caption = 'Remove RegEx'
      OnClick = RemoveRegExMIClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CreateNestedExpressionMI: TMenuItem
      Caption = 'Create Nested Expression'
      OnClick = CreateNestedExpBtnClick
    end
  end
  object RegExPopupMenu: TTntPopupMenu
    Left = 643
    Top = 79
    object CopyforCcodeMI: TTntMenuItem
      Caption = 'Copy for C code'
      OnClick = CopyforCcodeMIClick
    end
    object PasteCcodeMI: TTntMenuItem
      Caption = 'Paste C code'
      OnClick = PasteCcodeMIClick
    end
  end
end
