object EdOptionsWindow: TEdOptionsWindow
  Left = 364
  Top = 245
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 410
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object OptionPages: TPageControl
    Left = 0
    Top = 0
    Width = 380
    Height = 372
    ActivePage = OptionsPage
    Align = alTop
    TabOrder = 0
    OnChange = OptionPagesChange
    object OptionsPage: TTabSheet
      Caption = 'Options'
      object optionsPanel: TPanel
        Left = 0
        Top = 0
        Width = 372
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object viewOptionsBox: TGroupBox
          Left = 0
          Top = 0
          Width = 183
          Height = 188
          Caption = 'View Options'
          TabOrder = 0
          object CodeFoldingCB: TCheckBox
            Left = 35
            Top = 75
            Width = 112
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Code Folding'
            TabOrder = 2
          end
          object GutterCB: TCheckBox
            Left = 35
            Top = 34
            Width = 112
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Gutter'
            TabOrder = 0
          end
          object LineNumbersCB: TCheckBox
            Left = 35
            Top = 54
            Width = 112
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Line Numbers'
            TabOrder = 1
          end
          object IndentationGuidesCB: TCheckBox
            Left = 35
            Top = 96
            Width = 112
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Indentation Guides'
            TabOrder = 3
          end
          object BraceHiliteCB: TCheckBox
            Left = 35
            Top = 117
            Width = 112
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Brace Highlight'
            TabOrder = 4
          end
          object WordWrapCB: TComboBox
            Left = 38
            Top = 140
            Width = 120
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 5
            Text = 'No Word wrap'
            Items.Strings = (
              'No Word wrap'
              'Word Wrap'
              'Wrap Characters')
          end
        end
        object gbRightEdge: TGroupBox
          Left = 187
          Top = 190
          Width = 183
          Height = 153
          Caption = 'Right Edge'
          TabOrder = 3
          object Label28: TLabel
            Left = 6
            Top = 71
            Width = 55
            Height = 13
            Caption = 'Edge Color:'
            Transparent = True
          end
          object Label29: TLabel
            Left = 15
            Top = 41
            Width = 46
            Height = 13
            Caption = 'Edge Col:'
            Transparent = True
          end
          object Label30: TLabel
            Left = 6
            Top = 99
            Width = 55
            Height = 13
            Caption = 'Edge Type:'
            Transparent = True
          end
          object EdgeColumnSE: TEdit
            Left = 62
            Top = 38
            Width = 51
            Height = 21
            Hint = 'Edge Column'
            TabOrder = 0
            Text = '0'
            OnKeyPress = TestNumericOnly
          end
          object EdgeColorCB: TColorBox
            Left = 62
            Top = 66
            Width = 110
            Height = 22
            ItemHeight = 16
            TabOrder = 1
          end
          object EdgeTypeCB: TComboBox
            Left = 62
            Top = 94
            Width = 110
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            Items.Strings = (
              'None'
              'Line'
              'Background')
          end
        end
        object indentationBox: TGroupBox
          Left = 188
          Top = 0
          Width = 183
          Height = 188
          Caption = 'Indentation Options'
          TabOrder = 1
          object Label1: TLabel
            Left = 27
            Top = 128
            Width = 64
            Height = 13
            Caption = 'Indent Width:'
          end
          object Label2: TLabel
            Left = 38
            Top = 100
            Width = 53
            Height = 13
            Caption = 'Tab Width:'
          end
          object KeepIndentCB: TCheckBox
            Left = 19
            Top = 34
            Width = 117
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Keep Indent'
            TabOrder = 0
          end
          object TabIndentsCB: TCheckBox
            Left = 19
            Top = 54
            Width = 117
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Tab Indents'
            TabOrder = 1
          end
          object BackSpaceUnIndentsCB: TCheckBox
            Left = 19
            Top = 75
            Width = 117
            Height = 17
            Alignment = taLeftJustify
            Caption = 'B.Space Unindents'
            TabOrder = 2
          end
          object TabWidthSE: TEdit
            Left = 95
            Top = 96
            Width = 41
            Height = 21
            TabOrder = 3
            OnKeyPress = TestNumericOnly
          end
          object IndentWidthSE: TEdit
            Left = 95
            Top = 124
            Width = 41
            Height = 21
            TabOrder = 4
            OnKeyPress = TestNumericOnly
          end
        end
        object OtherGB: TGroupBox
          Left = 0
          Top = 190
          Width = 183
          Height = 153
          Caption = 'Other Options'
          TabOrder = 2
          object Label3: TLabel
            Left = 13
            Top = 121
            Width = 50
            Height = 13
            Caption = 'EOL Style:'
            Transparent = True
          end
          object UseUnicodeCB: TCheckBox
            Left = 34
            Top = 40
            Width = 115
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Use Unicode'
            TabOrder = 0
          end
          object SaveClearsUndoCB: TCheckBox
            Left = 34
            Top = 60
            Width = 115
            Height = 17
            Hint = 'Clear the undobuffer when saving.'
            Alignment = taLeftJustify
            Caption = 'Save Clears Undo'
            TabOrder = 1
          end
          object ReadOnlyCB: TCheckBox
            Left = 34
            Top = 80
            Width = 115
            Height = 17
            Alignment = taLeftJustify
            Caption = 'Read Only'
            TabOrder = 2
          end
          object EOLStyleCB: TComboBox
            Left = 67
            Top = 117
            Width = 82
            Height = 21
            Hint = 
              'Sets the characters that are added into the document when the us' +
              'er presses the Enter key. Default CRLF'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 3
            Items.Strings = (
              'CR+LF'
              'CR'
              'LF')
          end
        end
      end
    end
    object ColorsPage: TTabSheet
      Caption = 'Colors etc'
      ImageIndex = 4
      object colorsPanel: TPanel
        Left = 0
        Top = 0
        Width = 372
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object MarkersGB: TGroupBox
          Left = 0
          Top = 124
          Width = 372
          Height = 78
          Align = alTop
          Caption = 'Markers'
          TabOrder = 2
          object Label20: TLabel
            Left = 188
            Top = 22
            Width = 61
            Height = 13
            Caption = 'Marker Back'
            Transparent = True
          end
          object Label21: TLabel
            Left = 8
            Top = 22
            Width = 57
            Height = 13
            Caption = 'Marker Fore'
            Transparent = True
          end
          object Label22: TLabel
            Left = 187
            Top = 48
            Width = 62
            Height = 13
            Caption = 'B.Mark Back'
            Transparent = True
          end
          object Label23: TLabel
            Left = 7
            Top = 48
            Width = 58
            Height = 13
            Caption = 'B.Mark Fore'
            Transparent = True
          end
          object MarkerForeCB: TColorBox
            Left = 70
            Top = 18
            Width = 110
            Height = 22
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 0
          end
          object MarkerBackCB: TColorBox
            Left = 252
            Top = 18
            Width = 110
            Height = 22
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
          object BMarkForeCB: TColorBox
            Left = 70
            Top = 45
            Width = 110
            Height = 22
            Hint = 'Bookmark foreground color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 2
          end
          object BMarkBackCB: TColorBox
            Left = 252
            Top = 45
            Width = 110
            Height = 22
            Hint = 'Bookmark background color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 3
          end
        end
        object CaretGB: TGroupBox
          Left = 0
          Top = 49
          Width = 372
          Height = 75
          Align = alTop
          Caption = 'Caret'
          TabOrder = 1
          object Label13: TLabel
            Left = 12
            Top = 24
            Width = 24
            Height = 13
            Caption = 'Color'
            Transparent = True
          end
          object Label15: TLabel
            Left = 165
            Top = 24
            Width = 81
            Height = 13
            Caption = 'Line Background'
            Transparent = True
          end
          object Label14: TLabel
            Left = 8
            Top = 48
            Width = 28
            Height = 13
            Caption = 'Width'
            Transparent = True
          end
          object CaretCB: TColorBox
            Left = 40
            Top = 19
            Width = 110
            Height = 22
            Hint = 'Caret Color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 0
          end
          object CaretBackCB: TColorBox
            Left = 250
            Top = 19
            Width = 110
            Height = 22
            Hint = 'Caret line background color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
          object CaretWidthSE: TEdit
            Left = 40
            Top = 45
            Width = 40
            Height = 21
            Hint = 'Width of the caret (0-3)'
            TabOrder = 2
            OnKeyPress = TestNumericOnly
          end
          object CaretLineVisCB: TCheckBox
            Left = 160
            Top = 49
            Width = 103
            Height = 17
            Hint = 
              'Checked when the caretline should be shown with the defined colo' +
              'r'
            Alignment = taLeftJustify
            Caption = 'Caret Line Visible'
            TabOrder = 3
          end
        end
        object SelectionGB: TGroupBox
          Left = 0
          Top = 0
          Width = 372
          Height = 49
          Align = alTop
          Caption = 'Selection'
          TabOrder = 0
          object Label16: TLabel
            Left = 6
            Top = 22
            Width = 54
            Height = 13
            Caption = 'Foreground'
            Transparent = True
          end
          object Label17: TLabel
            Left = 187
            Top = 22
            Width = 58
            Height = 13
            Caption = 'Background'
            Transparent = True
          end
          object SelForeCB: TColorBox
            Left = 63
            Top = 17
            Width = 110
            Height = 22
            Hint = 'Selection foreground color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 0
          end
          object SelBackCB: TColorBox
            Left = 250
            Top = 17
            Width = 110
            Height = 22
            Hint = 'Selection background color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
        end
        object HotspotGB: TGroupBox
          Left = 0
          Top = 276
          Width = 372
          Height = 68
          Align = alClient
          Caption = 'Active Hotspot'
          TabOrder = 4
          object Label24: TLabel
            Left = 11
            Top = 23
            Width = 54
            Height = 13
            Caption = 'Foreground'
            Transparent = True
          end
          object Label25: TLabel
            Left = 192
            Top = 23
            Width = 58
            Height = 13
            Caption = 'Background'
            Transparent = True
          end
          object HotActiveForeCB: TColorBox
            Left = 70
            Top = 19
            Width = 110
            Height = 22
            Hint = 'Hotspot foreground color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 0
          end
          object HotActiveBackCB: TColorBox
            Left = 253
            Top = 19
            Width = 110
            Height = 22
            Hint = 'Hotspot background color'
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
          object HotActiveUnderlineCB: TCheckBox
            Left = 13
            Top = 45
            Width = 70
            Height = 17
            Hint = 'Underline when the mouse hovers over the hotspot'
            Alignment = taLeftJustify
            Caption = 'Underline'
            TabOrder = 2
          end
          object HotActiveSingleLineCB: TCheckBox
            Left = 192
            Top = 45
            Width = 74
            Height = 17
            Hint = 'Single line mode stops a hotspot from wrapping onto next line'
            Alignment = taLeftJustify
            Caption = 'Single Line'
            TabOrder = 3
          end
        end
        object FoldingGB: TGroupBox
          Left = 0
          Top = 202
          Width = 372
          Height = 74
          Align = alTop
          Caption = 'Folding'
          TabOrder = 3
          object Label26: TLabel
            Left = 10
            Top = 22
            Width = 55
            Height = 13
            Caption = 'Fold Margin'
            Transparent = True
          end
          object Label27: TLabel
            Left = 185
            Top = 22
            Width = 64
            Height = 13
            Caption = 'Fold Highlight'
            Transparent = True
          end
          object Label31: TLabel
            Left = 41
            Top = 47
            Width = 24
            Height = 13
            Caption = 'Type'
            Transparent = True
          end
          object FoldLoCB: TColorBox
            Left = 70
            Top = 19
            Width = 110
            Height = 22
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 0
          end
          object FoldHiCB: TColorBox
            Left = 252
            Top = 19
            Width = 110
            Height = 22
            Selected = clDefault
            Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 1
          end
          object markerTypeCB: TComboBox
            Left = 71
            Top = 45
            Width = 109
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            Items.Strings = (
              'Circle'
              'Box'
              'Plus/Minus'
              'Arrows')
          end
        end
      end
    end
    object HighlighterPage: TTabSheet
      Caption = 'LanguageManager'
      ImageIndex = 1
      object highlighterPanel: TPanel
        Left = 0
        Top = 0
        Width = 372
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object languageCBBox: TGroupBox
          Left = 0
          Top = 0
          Width = 372
          Height = 45
          Align = alTop
          Caption = 'Language'
          TabOrder = 0
          object LanguageCB: TComboBox
            Left = 9
            Top = 18
            Width = 145
            Height = 21
            Hint = 'The selected language/lexer.'
            Style = csDropDownList
            ItemHeight = 13
            Sorted = True
            TabOrder = 0
            OnChange = LanguageCBChange
          end
          object addLangButton: TBitBtn
            Left = 163
            Top = 17
            Width = 58
            Height = 22
            Hint = 'Add new language'
            Caption = 'Add'
            TabOrder = 1
            OnClick = addLangButtonClick
            Glyph.Data = {
              DE010000424DDE01000000000000760000002800000024000000120000000100
              0400000000006801000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000666666666666666666666666666666666666000066666666FFF666666666
              666666FFF666666600006666666844F66666666666666888F666666600006666
              666844F66666666666666888F666666600006666666844F66666666666666888
              F666666600006666FFF644FFFFF6666666FFF888FFFFF6660000666844444444
              44F66666688888888888F666000066684444444444F66666688888888888F666
              000066688888446888666666688888888888666600006666666844F666666666
              66666888F666666600006666666844F66666666666666888F666666600006666
              666844F66666666666666888F666666600006666666888666666666666666888
              6666666600006666666666666666666666666666666666660000666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000}
            NumGlyphs = 2
          end
          object remLangButton: TBitBtn
            Left = 305
            Top = 17
            Width = 58
            Height = 22
            Hint = 'Remove selected language'
            Caption = 'Rem'
            TabOrder = 3
            OnClick = remLangButtonClick
            Glyph.Data = {
              DE010000424DDE01000000000000760000002800000024000000120000000100
              0400000000006801000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000666666666666661F66666666666666668F66000066661F66666666666666
              668F6666666666660000666111F6666661F666666888F6666668F66600006661
              11F666661F6666666888F666668F666600006666111F66611F66666666888F66
              688F6666000066666111F611F6666666666888F688F66666000066666611111F
              66666666666688888F66666600006666666111F66666666666666888F6666666
              000066666611111F66666666666688888F666666000066666111F61F66666666
              666888F68F66666600006661111F66611F66666668888F66688F666600006611
              11F6666611F666668888F6666688F66600006611F6666666611F666688F66666
              66688F6600006666666666666666666666666666666666660000666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000}
            NumGlyphs = 2
          end
          object copyLangButton: TBitBtn
            Left = 234
            Top = 17
            Width = 58
            Height = 22
            Hint = 'Copy Language'
            Caption = 'Copy'
            TabOrder = 2
            OnClick = copyLangButtonClick
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              04000000000080000000CE0E0000D80E00001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
              8888888888888888888888888844444444488888884FFFFFFF488888884F0000
              0F480000004FFFFFFF480FFFFF4F00000F480F00004FFFFFFF480FFFFF4F00F4
              44480F00004FFFF4F4880FFFFF4FFFF448880F00F044444488880FFFF0F08888
              88880FFFF0088888888800000088888888888888888888888888}
          end
        end
        object highlighterPageCtrl: TPageControl
          Left = 0
          Top = 45
          Width = 372
          Height = 299
          ActivePage = stylesTabSheet
          Align = alClient
          TabOrder = 1
          object stylesTabSheet: TTabSheet
            Caption = 'Styles'
            object Label19: TLabel
              Left = 0
              Top = 112
              Width = 364
              Height = 18
              Align = alTop
              Alignment = taCenter
              AutoSize = False
              Caption = 'Style Definition'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              Layout = tlCenter
            end
            object topStylePanel: TPanel
              Left = 0
              Top = 0
              Width = 364
              Height = 112
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object GroupBox5: TGroupBox
                Left = 4
                Top = 0
                Width = 162
                Height = 108
                Caption = 'Styles:'
                TabOrder = 0
                object StylesLB: TListBox
                  Left = 9
                  Top = 15
                  Width = 144
                  Height = 85
                  Hint = 'List of styles belonging to the selected language/lexer.'
                  BevelKind = bkSoft
                  BorderStyle = bsNone
                  ImeName = 'The styles this language/lexer contains'
                  ItemHeight = 13
                  TabOrder = 0
                  OnClick = StylesLBClick
                end
              end
              object stylenoBox: TGroupBox
                Left = 171
                Top = 0
                Width = 190
                Height = 108
                Caption = 'Style'
                TabOrder = 1
                object Label4: TLabel
                  Left = 6
                  Top = 23
                  Width = 66
                  Height = 13
                  Caption = 'Style Number:'
                  Transparent = True
                end
                object Label9: TLabel
                  Left = 16
                  Top = 48
                  Width = 56
                  Height = 13
                  Caption = 'Description:'
                  Transparent = True
                end
                object StyleNumberSE: TEdit
                  Left = 73
                  Top = 20
                  Width = 47
                  Height = 21
                  Hint = 'Stylenumber for this style'
                  TabOrder = 0
                  OnExit = StyleElementChanged
                  OnKeyPress = StyleNumberSEKeyPress
                end
                object DescriptionEB: TEdit
                  Left = 72
                  Top = 45
                  Width = 111
                  Height = 21
                  Hint = 'Description of this style'
                  TabOrder = 1
                  OnChange = StyleElementChanged
                end
                object AddStyleB: TBitBtn
                  Left = 37
                  Top = 72
                  Width = 58
                  Height = 22
                  Hint = 'Add Style'
                  Caption = 'Add'
                  TabOrder = 2
                  OnClick = AddStyleBClick
                  Glyph.Data = {
                    DE010000424DDE01000000000000760000002800000024000000120000000100
                    0400000000006801000000000000000000001000000010000000000000000000
                    80000080000000808000800000008000800080800000C0C0C000808080000000
                    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000666666666666666666666666666666666666000066666666FFF666666666
                    666666FFF666666600006666666844F66666666666666888F666666600006666
                    666844F66666666666666888F666666600006666666844F66666666666666888
                    F666666600006666FFF644FFFFF6666666FFF888FFFFF6660000666844444444
                    44F66666688888888888F666000066684444444444F66666688888888888F666
                    000066688888446888666666688888888888666600006666666844F666666666
                    66666888F666666600006666666844F66666666666666888F666666600006666
                    666844F66666666666666888F666666600006666666888666666666666666888
                    6666666600006666666666666666666666666666666666660000666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000}
                  NumGlyphs = 2
                end
                object DeleteStyleB: TBitBtn
                  Left = 99
                  Top = 72
                  Width = 58
                  Height = 22
                  Hint = 'Delete Style'
                  Caption = 'Rem'
                  TabOrder = 3
                  OnClick = DeleteStyleBClick
                  Glyph.Data = {
                    DE010000424DDE01000000000000760000002800000024000000120000000100
                    0400000000006801000000000000000000001000000010000000000000000000
                    80000080000000808000800000008000800080800000C0C0C000808080000000
                    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000666666666666661F66666666666666668F66000066661F66666666666666
                    668F6666666666660000666111F6666661F666666888F6666668F66600006661
                    11F666661F6666666888F666668F666600006666111F66611F66666666888F66
                    688F6666000066666111F611F6666666666888F688F66666000066666611111F
                    66666666666688888F66666600006666666111F66666666666666888F6666666
                    000066666611111F66666666666688888F666666000066666111F61F66666666
                    666888F68F66666600006661111F66611F66666668888F66688F666600006611
                    11F6666611F666668888F6666688F66600006611F6666666611F666688F66666
                    66688F6600006666666666666666666666666666666666660000666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000}
                  NumGlyphs = 2
                end
              end
            end
            object StyleDefGB: TPanel
              Left = 0
              Top = 130
              Width = 364
              Height = 141
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 1
              object Label5: TLabel
                Left = 13
                Top = 8
                Width = 24
                Height = 13
                Caption = 'Font:'
                Transparent = True
              end
              object Label6: TLabel
                Left = 13
                Top = 36
                Width = 112
                Height = 13
                Caption = 'Font Size (0 for default):'
                Transparent = True
              end
              object Label7: TLabel
                Left = 16
                Top = 115
                Width = 57
                Height = 13
                Caption = 'Foreground:'
                Transparent = True
              end
              object Label8: TLabel
                Left = 190
                Top = 115
                Width = 61
                Height = 13
                Caption = 'Background:'
                Transparent = True
              end
              object Label10: TLabel
                Left = 18
                Top = 86
                Width = 55
                Height = 13
                Caption = 'Char. Case:'
                Transparent = True
              end
              object DefaultFontCB: TCheckBox
                Left = 228
                Top = 6
                Width = 109
                Height = 17
                Hint = 
                  'If unchecked, use the fontname, else use the default (Style 32 f' +
                  'ont).'
                Caption = 'Use Default Font'
                TabOrder = 0
                OnClick = StyleElementChanged
              end
              object SizeSE: TEdit
                Left = 127
                Top = 33
                Width = 47
                Height = 21
                Hint = 'Size of the font, 0 for default size.'
                TabOrder = 2
                OnExit = StyleElementChanged
              end
              object ForeCB: TColorBox
                Left = 77
                Top = 111
                Width = 101
                Height = 22
                Hint = 'Foreground color (text color)'
                DefaultColorColor = clDefault
                NoneColorColor = clDefault
                Selected = clDefault
                Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
                ItemHeight = 16
                TabOrder = 11
                OnChange = StyleElementChanged
              end
              object BackCB: TColorBox
                Left = 254
                Top = 111
                Width = 101
                Height = 22
                Hint = 'Background color'
                DefaultColorColor = clDefault
                NoneColorColor = clDefault
                Selected = clDefault
                Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
                ItemHeight = 16
                TabOrder = 12
                OnChange = StyleElementChanged
              end
              object ItalicCB: TCheckBox
                Left = 180
                Top = 34
                Width = 43
                Height = 17
                Caption = 'Italic'
                TabOrder = 3
                OnClick = StyleElementChanged
              end
              object BoldCB: TCheckBox
                Left = 228
                Top = 34
                Width = 46
                Height = 17
                Caption = 'Bold'
                TabOrder = 4
                OnClick = StyleElementChanged
              end
              object UnderlineCB: TCheckBox
                Left = 277
                Top = 34
                Width = 68
                Height = 17
                Caption = 'Underline'
                TabOrder = 5
                OnClick = StyleElementChanged
              end
              object VisibleCB: TCheckBox
                Left = 37
                Top = 61
                Width = 54
                Height = 17
                Caption = 'Visible'
                TabOrder = 6
                OnClick = StyleElementChanged
              end
              object ChangeableCB: TCheckBox
                Left = 180
                Top = 61
                Width = 84
                Height = 17
                Caption = 'Changeable'
                TabOrder = 8
                OnClick = StyleElementChanged
              end
              object EOLFilledCB: TCheckBox
                Left = 103
                Top = 61
                Width = 71
                Height = 17
                Caption = 'EOL Filled'
                TabOrder = 7
                OnClick = StyleElementChanged
              end
              object CaseCB: TComboBox
                Left = 77
                Top = 83
                Width = 101
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 10
                OnChange = StyleElementChanged
                Items.Strings = (
                  'Mixed'
                  'Upper'
                  'Lower')
              end
              object HotspotCB: TCheckBox
                Left = 277
                Top = 61
                Width = 59
                Height = 17
                Caption = 'Hotspot'
                TabOrder = 9
                OnClick = StyleElementChanged
              end
              object FontCB: TtcFontCombobox
                Left = 40
                Top = 3
                Width = 183
                Height = 24
                Hint = 'Font for the current style (only when Use Default is unchecked)'
                OnChange = StyleElementChanged
                OnClick = StyleElementChanged
                DropDownCount = 12
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -13
                Font.Name = 'MS Sans Serif'
                Font.Style = []
                ItemHeight = 18
                ParentFont = False
                TabOrder = 1
                PreviewWidth = 240
                PreviewHeight = 60
                FontTypes = [ftTrueTypeAnsi, ftTrueTypeSymbol]
                ShowPreviewInList = False
              end
            end
          end
          object keywordsTabSheet: TTabSheet
            Caption = 'Keywords'
            ImageIndex = 1
            object Label18: TLabel
              Left = 0
              Top = 112
              Width = 364
              Height = 18
              Align = alTop
              Alignment = taCenter
              AutoSize = False
              Caption = 'Keywords (separated by space)'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = True
              Layout = tlCenter
            end
            object topKeywordsPanel: TPanel
              Left = 0
              Top = 0
              Width = 364
              Height = 112
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object keywordlistBox: TGroupBox
                Left = 4
                Top = 0
                Width = 162
                Height = 108
                Caption = 'Keyword Lists:'
                TabOrder = 0
                object KeyListsLB: TListBox
                  Left = 9
                  Top = 15
                  Width = 144
                  Height = 85
                  Hint = 'Keywordlists for the currently selected language/lexer'
                  BevelKind = bkSoft
                  BorderStyle = bsNone
                  ItemHeight = 13
                  TabOrder = 0
                  OnClick = KeyListsLBClick
                end
              end
              object KeywordListGB: TGroupBox
                Left = 171
                Top = 0
                Width = 190
                Height = 108
                Caption = 'Keyword List'
                TabOrder = 1
                object Label11: TLabel
                  Left = 13
                  Top = 23
                  Width = 59
                  Height = 13
                  Caption = 'List Number:'
                  Transparent = True
                end
                object Label12: TLabel
                  Left = 16
                  Top = 48
                  Width = 56
                  Height = 13
                  Caption = 'Description:'
                  Transparent = True
                end
                object KeyListNumberSE: TEdit
                  Left = 73
                  Top = 20
                  Width = 47
                  Height = 21
                  Hint = 'Keywordlist number for the selected list.'
                  TabOrder = 0
                  OnExit = KeyListElementsChange
                  OnKeyPress = TestNumericOnly
                end
                object KeyListDescriptionEB: TEdit
                  Left = 73
                  Top = 45
                  Width = 111
                  Height = 21
                  Hint = 'Description for the selected list.'
                  TabOrder = 1
                  OnChange = KeyListElementsChange
                end
                object KeyListAdd: TBitBtn
                  Left = 37
                  Top = 72
                  Width = 58
                  Height = 22
                  Hint = 'Add Keyword List'
                  Caption = 'Add'
                  TabOrder = 2
                  OnClick = KeyListAddClick
                  Glyph.Data = {
                    DE010000424DDE01000000000000760000002800000024000000120000000100
                    0400000000006801000000000000000000001000000010000000000000000000
                    80000080000000808000800000008000800080800000C0C0C000808080000000
                    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000666666666666666666666666666666666666000066666666FFF666666666
                    666666FFF666666600006666666844F66666666666666888F666666600006666
                    666844F66666666666666888F666666600006666666844F66666666666666888
                    F666666600006666FFF644FFFFF6666666FFF888FFFFF6660000666844444444
                    44F66666688888888888F666000066684444444444F66666688888888888F666
                    000066688888446888666666688888888888666600006666666844F666666666
                    66666888F666666600006666666844F66666666666666888F666666600006666
                    666844F66666666666666888F666666600006666666888666666666666666888
                    6666666600006666666666666666666666666666666666660000666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000}
                  NumGlyphs = 2
                end
                object KeyListDelete: TBitBtn
                  Left = 99
                  Top = 72
                  Width = 58
                  Height = 22
                  Hint = 'Delete Keyword List'
                  Caption = 'Rem'
                  TabOrder = 3
                  OnClick = KeyListDeleteClick
                  Glyph.Data = {
                    DE010000424DDE01000000000000760000002800000024000000120000000100
                    0400000000006801000000000000000000001000000010000000000000000000
                    80000080000000808000800000008000800080800000C0C0C000808080000000
                    FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000666666666666661F66666666666666668F66000066661F66666666666666
                    668F6666666666660000666111F6666661F666666888F6666668F66600006661
                    11F666661F6666666888F666668F666600006666111F66611F66666666888F66
                    688F6666000066666111F611F6666666666888F688F66666000066666611111F
                    66666666666688888F66666600006666666111F66666666666666888F6666666
                    000066666611111F66666666666688888F666666000066666111F61F66666666
                    666888F68F66666600006661111F66611F66666668888F66688F666600006611
                    11F6666611F666668888F6666688F66600006611F6666666611F666688F66666
                    66688F6600006666666666666666666666666666666666660000666666666666
                    6666666666666666666666660000666666666666666666666666666666666666
                    0000}
                  NumGlyphs = 2
                end
              end
            end
            object KeywordsM: TMemo
              Left = 0
              Top = 130
              Width = 364
              Height = 141
              Hint = 'List of keywords for the selected list.'
              Align = alClient
              BevelKind = bkSoft
              BorderStyle = bsNone
              ScrollBars = ssVertical
              TabOrder = 1
              OnChange = KeyListElementsChange
            end
          end
          object DefaultsPage: TTabSheet
            Caption = 'Defaults'
            ImageIndex = 2
            object Label35: TLabel
              Left = 152
              Top = 78
              Width = 61
              Height = 13
              Caption = 'Background:'
              Transparent = True
            end
            object Label32: TLabel
              Left = 0
              Top = 0
              Width = 364
              Height = 18
              Align = alTop
              Alignment = taCenter
              AutoSize = False
              Caption = 'Default style settings'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
              Transparent = True
              Layout = tlCenter
            end
            object Label33: TLabel
              Left = 45
              Top = 78
              Width = 24
              Height = 13
              Caption = 'Font:'
            end
            object Label34: TLabel
              Left = 0
              Top = 18
              Width = 364
              Height = 13
              Align = alTop
              Alignment = taCenter
              Caption = 'These settings are used when you use a style'
            end
            object Label36: TLabel
              Left = 0
              Top = 31
              Width = 364
              Height = 13
              Align = alTop
              Alignment = taCenter
              Caption = 
                'which uses the clDefault color, or the '#39'Use default font'#39' is che' +
                'cked.'
            end
            object defaultBackgroundCB: TColorBox
              Left = 219
              Top = 74
              Width = 101
              Height = 22
              Hint = 'Background color'
              DefaultColorColor = clDefault
              NoneColorColor = clDefault
              Selected = clDefault
              Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
              ItemHeight = 16
              TabOrder = 0
            end
            object FontButton: TBitBtn
              Left = 74
              Top = 73
              Width = 68
              Height = 25
              Hint = 'Set default font settings (STYLE_DEFAULT 32)'
              Caption = '&Font'
              TabOrder = 1
              OnClick = FontButtonClick
              Glyph.Data = {
                76010000424D7601000000000000760000002800000020000000100000000100
                04000000000000010000130B0000130B00001000000000000000000000000000
                800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
                3333333333333333333333333333333333333333FFF33FFFFF33333300033000
                00333337773377777333333330333300033333337FF33777F333333330733300
                0333333377FFF777F33333333700000073333333777777773333333333033000
                3333333337FF777F333333333307300033333333377F777F3333333333703007
                33333333377F7773333333333330000333333333337777F33333333333300003
                33333333337777F3333333333337007333333333337777333333333333330033
                3333333333377333333333333333033333333333333733333333333333333333
                3333333333333333333333333333333333333333333333333333}
              NumGlyphs = 2
            end
          end
          object otherPage: TTabSheet
            Caption = 'Other Settings'
            ImageIndex = 3
            object langOperators: TGroupBox
              Left = 10
              Top = 65
              Width = 346
              Height = 53
              Caption = 'Operators'
              TabOrder = 0
              object Label37: TLabel
                Left = 14
                Top = 23
                Width = 54
                Height = 13
                Caption = 'Assignment'
              end
              object Label38: TLabel
                Left = 142
                Top = 23
                Width = 84
                Height = 13
                Caption = 'End Of Statement'
              end
              object AssignmentOperatorED: TEdit
                Left = 73
                Top = 20
                Width = 50
                Height = 21
                Hint = 'Assignment operator for this language'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                OnChange = AssignmentOperatorEDChange
              end
              object EndOfStatementED: TEdit
                Left = 231
                Top = 20
                Width = 50
                Height = 21
                Hint = 'End of statement operator for this language'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 1
                OnChange = EndOfStatementEDChange
              end
            end
            object langComments: TGroupBox
              Left = 10
              Top = 124
              Width = 346
              Height = 117
              Caption = 'Commenting'
              TabOrder = 1
              object Label39: TLabel
                Left = 25
                Top = 22
                Width = 43
                Height = 13
                Caption = 'Box Start'
              end
              object Label40: TLabel
                Left = 248
                Top = 22
                Width = 40
                Height = 13
                Caption = 'Box End'
              end
              object Label41: TLabel
                Left = 133
                Top = 22
                Width = 52
                Height = 13
                Caption = 'Box Middle'
              end
              object Label42: TLabel
                Left = 46
                Top = 80
                Width = 27
                Height = 13
                Caption = 'Block'
              end
              object Label49: TLabel
                Left = 9
                Top = 51
                Width = 58
                Height = 13
                Caption = 'Stream Start'
              end
              object Label50: TLabel
                Left = 129
                Top = 51
                Width = 55
                Height = 13
                Caption = 'Stream End'
              end
              object commentBoxStart: TEdit
                Left = 73
                Top = 18
                Width = 50
                Height = 21
                TabOrder = 0
                OnChange = commentBoxStartChange
              end
              object commentBoxEnd: TEdit
                Left = 290
                Top = 18
                Width = 50
                Height = 21
                TabOrder = 2
                OnChange = commentBoxEndChange
              end
              object commentBoxMiddle: TEdit
                Left = 187
                Top = 18
                Width = 50
                Height = 21
                TabOrder = 1
                OnChange = commentBoxMiddleChange
              end
              object commentBlock: TEdit
                Left = 73
                Top = 76
                Width = 50
                Height = 21
                TabOrder = 5
                OnChange = commentBlockChange
              end
              object commentStreamStart: TEdit
                Left = 73
                Top = 47
                Width = 50
                Height = 21
                TabOrder = 3
                OnChange = commentStreamStartChange
              end
              object commentStreamEnd: TEdit
                Left = 187
                Top = 47
                Width = 50
                Height = 21
                TabOrder = 4
                OnChange = commentStreamEndChange
              end
            end
            object langSettingsBox: TGroupBox
              Left = 10
              Top = 5
              Width = 346
              Height = 52
              Caption = 'Language Settings'
              TabOrder = 2
              object Label53: TLabel
                Left = 26
                Top = 24
                Width = 40
                Height = 13
                Caption = 'StyleBits'
              end
              object Label54: TLabel
                Left = 136
                Top = 24
                Width = 26
                Height = 13
                Caption = 'Lexer'
              end
              object NumStyleBitsED: TEdit
                Left = 72
                Top = 21
                Width = 50
                Height = 21
                Hint = 'Number of stylebits used by this highlighter'
                ParentShowHint = False
                ShowHint = True
                TabOrder = 0
                OnExit = NumStyleBitsEDExit
                OnKeyPress = TestNumericOnly
              end
              object lexerforlangCB: TComboBox
                Left = 170
                Top = 21
                Width = 116
                Height = 21
                Hint = 'Highlighter/lexer to use'
                Style = csDropDownList
                ItemHeight = 13
                ParentShowHint = False
                ShowHint = True
                Sorted = True
                TabOrder = 1
                OnExit = lexerforlangCBExit
              end
            end
          end
        end
      end
    end
    object KeyCommandsPage: TTabSheet
      Caption = 'Key commands'
      ImageIndex = 3
      object keycommandsPanel: TPanel
        Left = 0
        Top = 0
        Width = 372
        Height = 344
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object KeyCmdList: TListView
          Left = 0
          Top = 0
          Width = 372
          Height = 308
          Align = alClient
          Columns = <
            item
              Caption = 'Command'
              Width = 160
            end
            item
              Caption = 'Keystroke'
              Width = 110
            end>
          ColumnClick = False
          HideSelection = False
          HotTrack = True
          HotTrackStyles = [htUnderlineCold]
          PopupMenu = KeyMenu
          SortType = stData
          TabOrder = 0
          ViewStyle = vsReport
          OnCompare = KeyCmdListCompare
          OnDblClick = btnEditClick
        end
        object keybuttonsPanel: TPanel
          Left = 0
          Top = 308
          Width = 372
          Height = 36
          Align = alBottom
          BevelOuter = bvNone
          PopupMenu = KeyMenu
          TabOrder = 1
          DesignSize = (
            372
            36)
          object btnAdd: TBitBtn
            Left = 127
            Top = 6
            Width = 66
            Height = 25
            Hint = 'Add a new keyboard command'
            Anchors = [akTop, akRight]
            Caption = '&Add'
            TabOrder = 0
            OnClick = btnAddClick
            Glyph.Data = {
              DE010000424DDE01000000000000760000002800000024000000120000000100
              0400000000006801000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000666666666666666666666666666666666666000066666666FFF666666666
              666666FFF666666600006666666844F66666666666666888F666666600006666
              666844F66666666666666888F666666600006666666844F66666666666666888
              F666666600006666FFF644FFFFF6666666FFF888FFFFF6660000666844444444
              44F66666688888888888F666000066684444444444F66666688888888888F666
              000066688888446888666666688888888888666600006666666844F666666666
              66666888F666666600006666666844F66666666666666888F666666600006666
              666844F66666666666666888F666666600006666666888666666666666666888
              6666666600006666666666666666666666666666666666660000666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000}
            NumGlyphs = 2
          end
          object btnEdit: TBitBtn
            Left = 199
            Top = 6
            Width = 66
            Height = 25
            Hint = 'Edit the selected keyboard command'
            Anchors = [akTop, akRight]
            Caption = '&Edit'
            TabOrder = 1
            OnClick = btnEditClick
            Glyph.Data = {
              36040000424D3604000000000000360000002800000010000000100000000100
              2000000000000004000000000000000000000000000000000000008080000080
              8000008080000080800000808000008080000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000008080000080
              800000808000008080000080800000808000000000FFFFFFFF00FFFFFFFFFFFF
              FF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF0000000000000000000000
              0000008080000000000000000000000000000000000000000000FFFFFFFFFFFF
              FF00000000FFFFFFFF000000000000000000FFFFFF0000000000FFFF00000000
              00000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000FFFFFF
              FF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF0000000000FFFF00000000
              000000FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000FFFFFF
              FF00FFFFFFFFFFFFFF00FFFFFF0000000000FFFFFF0000000000FFFF00000000
              0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
              0000FFFFFF0000000000000000FFFFFFFF00FFFFFF0000000000FFFF00000000
              000000FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000000000
              00000000000000FFFF00000000FFFFFFFF00FFFFFF0000000000FFFF00000000
              0000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
              FF00FFFFFF0000000000FFFFFFFFFFFFFF00FFFFFF0000000000FFFF00000000
              000000FFFFFFFFFFFF0000000000000000000000000000000000000000000000
              0000000000FFFFFFFF00FFFFFFFFFFFFFF00FFFFFF0000000000000000000000
              00000000000000FFFF00FFFFFF0000FFFF00000000000000000000FFFF000000
              0000FFFFFFFFFFFFFF000000000000000000FFFFFF0000000000008080000080
              8000008080000000000000000000000000000000000000FFFF00000000FFFFFF
              FF00FFFFFFFFFFFFFF00FFFFFFFFFFFFFF00FFFFFF0000000000008080000080
              80000080800000808000008080000000000000FFFF0000000000FFFFFFFFFFFF
              FF00FFFFFFFFFFFFFF0000000000000000000000000000000000008080000080
              800000808000008080000000000000FFFF00000000FFFFFFFF00FFFFFF000000
              0000000000FFFFFFFF00000000FFFFFFFF00FFFFFF0000000000008080000080
              8000008080000000000000FFFF0000000000000000FFFFFFFF00FFFFFFFFFFFF
              FF00FFFFFFFFFFFFFF00000000FFFFFFFF000000000000808000008080000080
              8000000000000000FF000000000000808000000000FFFFFFFF00FFFFFFFFFFFF
              FF00FFFFFFFFFFFFFF0000000000000000000080800000808000008080000080
              8000008080000000000000808000008080000000000000000000000000000000
              0000000000000000000000000000008080000080800000808000}
          end
          object btnDelete: TBitBtn
            Left = 272
            Top = 6
            Width = 66
            Height = 25
            Hint = 'Delete the selected keyboard command'
            Anchors = [akTop, akRight]
            Caption = '&Delete'
            TabOrder = 2
            OnClick = btnDeleteClick
            Glyph.Data = {
              DE010000424DDE01000000000000760000002800000024000000120000000100
              0400000000006801000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000666666666666661F66666666666666668F66000066661F66666666666666
              668F6666666666660000666111F6666661F666666888F6666668F66600006661
              11F666661F6666666888F666668F666600006666111F66611F66666666888F66
              688F6666000066666111F611F6666666666888F688F66666000066666611111F
              66666666666688888F66666600006666666111F66666666666666888F6666666
              000066666611111F66666666666688888F666666000066666111F61F66666666
              666888F68F66666600006661111F66611F66666668888F66688F666600006611
              11F6666611F666668888F6666688F66600006611F6666666611F666688F66666
              66688F6600006666666666666666666666666666666666660000666666666666
              6666666666666666666666660000666666666666666666666666666666666666
              0000}
            NumGlyphs = 2
          end
          object btnReset: TBitBtn
            Left = 35
            Top = 6
            Width = 66
            Height = 25
            Hint = 'Reset the command list to default'
            Anchors = [akTop, akRight]
            Caption = '&Reset'
            TabOrder = 3
            OnClick = btnResetClick
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              04000000000080000000CE0E0000D80E00001000000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              7777777777777777777777777777777777777777777777777777777777777777
              7777777777777778477777444447777748777744447777777477774447777777
              7477774474777777747777477748777748777777777844448777777777777777
              7777777777777777777777777777777777777777777777777777}
          end
        end
      end
    end
  end
  object buttonPanel: TPanel
    Left = 0
    Top = 369
    Width = 380
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object CancelBtn: TBitBtn
      Left = 290
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Cancel, and revert to previous settings'
      TabOrder = 0
      OnClick = CancelBtnClick
      Kind = bkCancel
    end
    object ApplyBtn: TBitBtn
      Left = 154
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Apply the settings'
      Caption = 'Apply'
      TabOrder = 1
      OnClick = ApplyBtnClick
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666F6666666666000066666636666666666666666686F666666666
        0000666666306666666666666666886F66666666000066666663066666666666
        66666886F6666666000066666663B066666666666666F8F86F66666600006666
        63000F06666666666668888686F666660000666663FBFBF06666666666686F66
        686F666600006666663FB0333666666666668F688886666600006666663BFB06
        6666666666FF8FF686F66666000066630000BFB06666666668888866686F6666
        00006663FBFBFBFB06666666686F6666668F6666000066663FBFB03336666666
        668F666888866666000066663BFBFB06666666666686F66686F6666600006666
        63BFBFB0666666666668F666686F66660000666663FBFBFB0666666666686F66
        6686F66600006666663FBFBFB066666666668FFFFFF8FF660000666666333333
        3336666666668888888886660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
    end
    object OKBtn: TBitBtn
      Left = 18
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Apply the settings and close dialog'
      TabOrder = 2
      OnClick = OKBtnClick
      Kind = bkOK
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 292
  end
  object KeyMenu: TPopupMenu
    Left = 101
    Top = 112
    object LoadKeyCommands1: TMenuItem
      Caption = 'Load Keycommands..'
      OnClick = LoadKeyCommands1Click
    end
    object SaveKeycommands1: TMenuItem
      Caption = '&Save Keycommands..'
      OnClick = SaveKeycommands1Click
    end
  end
  object odia: TOpenDialog
    Filter = 'Keycommands (*.key)|*.key|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load keycommands from..'
    Left = 201
    Top = 120
  end
  object sdia: TSaveDialog
    Filter = 'Keycommands (*.key)|*.key|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save Keycommands to..'
    Left = 229
    Top = 121
  end
end
