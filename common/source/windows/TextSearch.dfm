object TextSearchForm: TTextSearchForm
  Left = 538
  Top = 252
  ActiveControl = SearchLU
  AlphaBlendValue = 150
  BorderStyle = bsToolWindow
  Caption = 'Search ...'
  ClientHeight = 206
  ClientWidth = 473
  Color = clBtnFace
  Constraints.MaxWidth = 569
  Constraints.MinWidth = 464
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  DesignSize = (
    473
    206)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TTntPageControl
    Left = 6
    Top = 6
    Width = 461
    Height = 195
    ActivePage = SearchTabSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControlChange
    object SearchTabSheet: TTntTabSheet
      Caption = 'Search'
      ExplicitLeft = 0
      DesignSize = (
        453
        167)
      object SearchLbl: TTntLabel
        Left = 0
        Top = 17
        Width = 52
        Height = 13
        Caption = 'Search for:'
      end
      object SearchLU: TTntComboBox
        Left = 84
        Top = 14
        Width = 355
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
      end
      object SearchBtn: TTntButton
        Left = 260
        Top = 130
        Width = 90
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search'
        TabOrder = 1
        OnClick = SearchBtnClick
      end
      object CloseBtn: TTntButton
        Left = 360
        Top = 130
        Width = 90
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Close'
        TabOrder = 2
        OnClick = CloseBtnClick
      end
      object DetailsBtn: TTntButton
        Left = 0
        Top = 130
        Width = 90
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Details >>'
        TabOrder = 3
        OnClick = DetailsBtnClick
      end
    end
    object ReplaceTabSheet: TTntTabSheet
      Caption = 'Replace'
      DesignSize = (
        453
        167)
      object ReplacesSearchLbl: TTntLabel
        Left = 24
        Top = 18
        Width = 52
        Height = 13
        Alignment = taRightJustify
        Caption = 'Search for:'
      end
      object ReplaceWithLbl: TTntLabel
        Left = 12
        Top = 54
        Width = 65
        Height = 13
        Alignment = taRightJustify
        Caption = 'Replace with:'
      end
      object ReplaceSourceLU: TTntComboBox
        Left = 84
        Top = 14
        Width = 355
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
      end
      object ReplaceWithLU: TTntComboBox
        Left = 84
        Top = 50
        Width = 355
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
      end
      object ReplaceAllBtn: TTntButton
        Left = 276
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Replace All'
        TabOrder = 2
        OnClick = ReplaceAllBtnClick
      end
      object Close2Btn: TTntButton
        Left = 364
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Close'
        TabOrder = 3
        OnClick = CloseBtnClick
      end
      object ReplaceBtn: TTntButton
        Left = 188
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Replace'
        TabOrder = 4
        OnClick = ReplaceBtnClick
      end
      object Details2Btn: TTntButton
        Left = 100
        Top = 130
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Details >>'
        TabOrder = 5
        OnClick = DetailsBtnClick
      end
    end
    object OptionsTabSheet: TTntTabSheet
      Caption = 'Options'
      object OptionPnl: TPanel
        Left = 0
        Top = 0
        Width = 453
        Height = 167
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ScopeGBox: TTntGroupBox
          Left = 12
          Top = 80
          Width = 255
          Height = 71
          Caption = 'Scope'
          TabOrder = 0
          object SearchInSelectedColumnRBtn: TTntRadioButton
            Left = 16
            Top = 42
            Width = 231
            Height = 17
            Caption = 'Search only in selected text/column'
            TabOrder = 1
            OnClick = CaseSensitiveCBoxClick
          end
          object SearchAllColumnsRBtn: TTntRadioButton
            Left = 16
            Top = 20
            Width = 229
            Height = 17
            Caption = 'Search all text/columns'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = CaseSensitiveCBoxClick
          end
        end
        object DirectionGBox: TTntGroupBox
          Left = 282
          Top = 80
          Width = 157
          Height = 71
          Caption = 'Direction'
          TabOrder = 1
          object SearchDownRBtn: TTntRadioButton
            Left = 16
            Top = 42
            Width = 131
            Height = 17
            Caption = 'Search down'
            Checked = True
            TabOrder = 1
            TabStop = True
            OnClick = CaseSensitiveCBoxClick
          end
          object SearchUpRBtn: TTntRadioButton
            Left = 16
            Top = 20
            Width = 133
            Height = 17
            Caption = 'Search up'
            TabOrder = 0
            OnClick = CaseSensitiveCBoxClick
          end
        end
        object OptionsGBox: TTntGroupBox
          Left = 12
          Top = 2
          Width = 255
          Height = 71
          Caption = 'Options'
          TabOrder = 2
          object CaseSensitiveCBox: TTntCheckBox
            Left = 16
            Top = 20
            Width = 119
            Height = 17
            Caption = 'Case sensitive'
            TabOrder = 0
            OnClick = CaseSensitiveCBoxClick
          end
          object WholeWordsOnlyCBox: TTntCheckBox
            Left = 16
            Top = 42
            Width = 141
            Height = 17
            Caption = 'Whole words only'
            TabOrder = 1
            OnClick = CaseSensitiveCBoxClick
          end
          object UseRegExCBox: TTntCheckBox
            Left = 144
            Top = 20
            Width = 99
            Height = 17
            Caption = 'Use RegEx'
            Enabled = False
            TabOrder = 2
            Visible = False
            OnClick = CaseSensitiveCBoxClick
          end
        end
        object TntGroupBox1: TTntGroupBox
          Left = 282
          Top = 2
          Width = 157
          Height = 71
          Caption = 'Origin'
          TabOrder = 3
          object SearchFromCursorRBtn: TTntRadioButton
            Left = 16
            Top = 42
            Width = 133
            Height = 17
            Caption = 'Search from cursor'
            TabOrder = 1
            OnClick = CaseSensitiveCBoxClick
          end
          object SearchFromTopRBtn: TTntRadioButton
            Left = 16
            Top = 20
            Width = 133
            Height = 17
            Caption = 'Search from top'
            Checked = True
            TabOrder = 0
            TabStop = True
            OnClick = CaseSensitiveCBoxClick
          end
        end
      end
    end
  end
  object NoMatchFoundPnl: TPanel
    Left = 267
    Top = 2
    Width = 199
    Height = 21
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Caption = 'No match found.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    Visible = False
  end
end
