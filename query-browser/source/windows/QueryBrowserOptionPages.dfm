object QueryBrowserOptionPagesForm: TQueryBrowserOptionPagesForm
  Left = 556
  Top = 319
  Caption = 'QueryBrowserOptionPagesForm'
  ClientHeight = 416
  ClientWidth = 577
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object OptionPageControl: TTntPageControl
    Left = 0
    Top = 0
    Width = 577
    Height = 416
    ActivePage = BrowserTabSheet
    Align = alClient
    Style = tsFlatButtons
    TabHeight = 16
    TabOrder = 0
    ExplicitHeight = 410
    object BrowserTabSheet: TTntTabSheet
      Caption = 'Browser'
      ImageIndex = 2
      ExplicitHeight = 384
      DesignSize = (
        569
        390)
      object GroupBox4: TTntGroupBox
        Left = 0
        Top = 6
        Width = 565
        Height = 128
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Display Options'
        TabOrder = 0
        object ShowAdvancedToolbarMI: TTntCheckBox
          Left = 16
          Top = 46
          Width = 261
          Height = 17
          Caption = 'Show advanced toolbars'
          TabOrder = 2
          OnClick = DoChange
        end
        object HideTabWhenOneOpenCBox: TTntCheckBox
          Left = 278
          Top = 20
          Width = 279
          Height = 17
          Caption = 'Hide the tab when only one page is open'
          TabOrder = 0
          OnClick = DoChange
        end
        object ToolbarGradientCBox: TTntCheckBox
          Left = 16
          Top = 20
          Width = 261
          Height = 17
          Caption = 'Toolbars use a gradient background'
          TabOrder = 1
          OnClick = DoChange
        end
        object ShowFieldOverlayImagesCBox: TTntCheckBox
          Left = 16
          Top = 72
          Width = 261
          Height = 17
          Caption = 'Show field overlay images for BLOB fields'
          TabOrder = 3
          OnClick = DoChange
        end
        object ShowMouseCursorToolbarGroupCBox: TTntCheckBox
          Left = 278
          Top = 46
          Width = 279
          Height = 17
          Caption = 'Show composer buttons toolbar'
          TabOrder = 4
          OnClick = DoChange
        end
        object AutoEditCheckbox: TTntCheckBox
          Left = 16
          Top = 98
          Width = 325
          Height = 17
          Caption = 'Enable edit mode of result sets when opened'
          TabOrder = 5
          OnClick = DoChange
        end
      end
      object TntGroupBox1: TTntGroupBox
        Left = 1
        Top = 140
        Width = 565
        Height = 181
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Query Options'
        TabOrder = 1
        DesignSize = (
          565
          181)
        object TntLabel1: TTntLabel
          Left = 390
          Top = 81
          Width = 84
          Height = 16
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Substitude LF:'
        end
        object TntLabel2: TTntLabel
          Left = 241
          Top = 81
          Width = 88
          Height = 16
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Substitude CR:'
        end
        object EnforceEditableQueriesCBox: TTntCheckBox
          Left = 15
          Top = 28
          Width = 537
          Height = 17
          Caption = 
            'Enforce queries to be editable by adding primary key columns to ' +
            'the select'
          TabOrder = 0
          OnClick = DoChange
        end
        object OpenExportedResultsetCBox: TTntCheckBox
          Left = 15
          Top = 54
          Width = 537
          Height = 17
          Caption = 'Open resultset in associated application after export'
          TabOrder = 1
          OnClick = DoChange
        end
        object EnableFriendlyLineBreaksCBox: TTntCheckBox
          Left = 15
          Top = 80
          Width = 183
          Height = 17
          Caption = 'Enable friendly line breaks'
          TabOrder = 2
          OnClick = DoChange
        end
        object SubstLFLU: TTntComboBox
          Left = 480
          Top = 77
          Width = 47
          Height = 24
          Anchors = [akTop, akRight]
          ItemHeight = 16
          TabOrder = 3
          OnChange = DoChange
          Items.Strings = (
            #182
            #172
            #166)
        end
        object SubstCRLU: TTntComboBox
          Left = 336
          Top = 77
          Width = 47
          Height = 24
          Anchors = [akTop, akRight]
          ItemHeight = 16
          TabOrder = 4
          OnChange = DoChange
          Items.Strings = (
            #183
            #172
            #187)
        end
        object CreateWindowsStyleLineBreaksCBox: TTntCheckBox
          Left = 15
          Top = 106
          Width = 277
          Height = 17
          Caption = 'Create Windows style line breaks (LF CR)'
          TabOrder = 5
          OnClick = DoChange
        end
        object AlignNumericColsRightCBox: TTntCheckBox
          Left = 15
          Top = 132
          Width = 239
          Height = 17
          Caption = 'Align numeric columns to the right'
          TabOrder = 6
          OnClick = DoChange
        end
        object ForceQueryCheckbox: TCheckBox
          Left = 15
          Top = 155
          Width = 466
          Height = 17
          Caption = 'Force query execution after error'
          TabOrder = 7
          OnClick = DoChange
        end
      end
      object TntGroupBox2: TTntGroupBox
        Left = 0
        Top = 327
        Width = 561
        Height = 51
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Various'
        TabOrder = 2
        object AssociateFileExtensionsCBox: TTntCheckBox
          Left = 16
          Top = 20
          Width = 495
          Height = 17
          Caption = 'Associate sql/query files with Query Browser'
          TabOrder = 0
          OnClick = DoChange
        end
      end
    end
  end
end
