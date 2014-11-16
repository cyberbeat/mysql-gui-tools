object MySQLResultSetFieldViewerForm: TMySQLResultSetFieldViewerForm
  Left = 411
  Top = 274
  ActiveControl = TextMemo
  Caption = 'Field Viewer'
  ClientHeight = 397
  ClientWidth = 587
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    587
    397)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TTntPageControl
    Left = 6
    Top = 6
    Width = 575
    Height = 381
    ActivePage = TextSheet
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TextSheet: TTntTabSheet
      Caption = 'Text'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TextMemo: TTntMemo
        Left = 0
        Top = 0
        Width = 567
        Height = 306
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object TextSheetBtnPnl: TTntPanel
        Left = 0
        Top = 306
        Width = 567
        Height = 47
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          567
          47)
        object OKBtn: TTntButton
          Left = 391
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'OK'
          ModalResult = 1
          TabOrder = 0
        end
        object CancelBtn: TTntButton
          Left = 478
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
      end
    end
    object ImageSheet: TTntTabSheet
      Caption = 'Image'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ImageScrollBox: TTntScrollBox
        Left = 0
        Top = 0
        Width = 567
        Height = 306
        HorzScrollBar.Smooth = True
        HorzScrollBar.Tracking = True
        VertScrollBar.Smooth = True
        VertScrollBar.Tracking = True
        Align = alClient
        BorderStyle = bsNone
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        object FieldImage: TTntImage
          Left = 0
          Top = 0
          Width = 155
          Height = 119
          AutoSize = True
        end
      end
      object ImageSheetBtnPnl: TTntPanel
        Left = 0
        Top = 306
        Width = 567
        Height = 47
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          567
          47)
        object ImageFormatTitleLbl: TTntLabel
          Left = 6
          Top = 8
          Width = 67
          Height = 13
          Caption = 'Image Format:'
        end
        object TntBevel1: TTntBevel
          Left = 0
          Top = 0
          Width = 567
          Height = 4
          Align = alTop
          Shape = bsTopLine
        end
        object ImageSizeTitleLbl: TTntLabel
          Left = 6
          Top = 24
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object ImageFormatLbl: TTntLabel
          Left = 94
          Top = 8
          Width = 3
          Height = 13
          Caption = '-'
        end
        object ImageSizeLbl: TTntLabel
          Left = 94
          Top = 24
          Width = 3
          Height = 13
          Caption = '-'
        end
        object Ok2Btn: TTntButton
          Left = 391
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'OK'
          ModalResult = 1
          TabOrder = 0
        end
        object Cancel2Btn: TTntButton
          Left = 478
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
      end
    end
    object BinarySheet: TTntTabSheet
      Caption = 'Binary'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TntSplitter1: TTntSplitter
        Left = 419
        Top = 0
        Width = 1
        Height = 306
        Align = alRight
      end
      object HexUniCodeEd: TUniCodeEdit
        Left = 0
        Top = 0
        Width = 419
        Height = 306
        Cursor = crIBeam
        Align = alClient
        CharWidth = 7
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Bitstream Vera Sans Mono'
        Font.Pitch = fpFixed
        Font.Style = []
        GutterColor = clBtnFace
        GutterWidth = 30
        Keystrokes = <
          item
            ShortCut = 8230
            Command = ecSelectUp
          end
          item
            ShortCut = 38
            Command = ecUp
          end
          item
            ShortCut = 16422
            Command = ecScrollUp
          end
          item
            ShortCut = 40
            Command = ecDown
          end
          item
            ShortCut = 8232
            Command = ecSelectDown
          end
          item
            ShortCut = 16424
            Command = ecScrollDown
          end
          item
            ShortCut = 37
            Command = ecLeft
          end
          item
            ShortCut = 8229
            Command = ecSelectLeft
          end
          item
            ShortCut = 16421
            Command = ecWordLeft
          end
          item
            ShortCut = 24613
            Command = ecSelectWordLeft
          end
          item
            ShortCut = 39
            Command = ecRight
          end
          item
            ShortCut = 8231
            Command = ecSelectRight
          end
          item
            ShortCut = 16423
            Command = ecWordRight
          end
          item
            ShortCut = 24615
            Command = ecSelectWordRight
          end
          item
            ShortCut = 34
            Command = ecPageDown
          end
          item
            ShortCut = 8226
            Command = ecSelectPageDown
          end
          item
            ShortCut = 16418
            Command = ecPageBottom
          end
          item
            ShortCut = 24610
            Command = ecSelectPageBottom
          end
          item
            ShortCut = 33
            Command = ecPageUp
          end
          item
            ShortCut = 8225
            Command = ecSelectPageUp
          end
          item
            ShortCut = 16417
            Command = ecPageTop
          end
          item
            ShortCut = 24609
            Command = ecSelectPageTop
          end
          item
            ShortCut = 36
            Command = ecLineStart
          end
          item
            ShortCut = 8228
            Command = ecSelectLineStart
          end
          item
            ShortCut = 16420
            Command = ecEditorTop
          end
          item
            ShortCut = 24612
            Command = ecSelectEditorTop
          end
          item
            ShortCut = 35
            Command = ecLineEnd
          end
          item
            ShortCut = 8227
            Command = ecSelectLineEnd
          end
          item
            ShortCut = 16419
            Command = ecEditorBottom
          end
          item
            ShortCut = 24611
            Command = ecSelectEditorBottom
          end
          item
            ShortCut = 45
            Command = ecToggleMode
          end
          item
            ShortCut = 16470
            Command = ecPaste
          end
          item
            ShortCut = 8237
            Command = ecPaste
          end
          item
            ShortCut = 46
            Command = ecDeleteChar
          end
          item
            ShortCut = 8
            Command = ecDeleteLastChar
          end
          item
            ShortCut = 16392
            Command = ecDeleteLastWord
          end
          item
            ShortCut = 16474
            Command = ecUndo
          end
          item
            ShortCut = 32776
            Command = ecUndo
          end
          item
            ShortCut = 24666
            Command = ecRedo
          end
          item
            ShortCut = 40968
            Command = ecRedo
          end
          item
            ShortCut = 13
            Command = ecLineBreak
          end
          item
            ShortCut = 16461
            Command = ecLineBreak
          end
          item
            ShortCut = 16449
            Command = ecSelectAll
          end
          item
            ShortCut = 16451
            Command = ecCopy
          end
          item
            ShortCut = 16429
            Command = ecCopy
          end
          item
            ShortCut = 16472
            Command = ecCut
          end
          item
            ShortCut = 8238
            Command = ecCut
          end
          item
            ShortCut = 24649
            Command = ecBlockIndent
          end
          item
            ShortCut = 16462
            Command = ecInsertLine
          end
          item
            ShortCut = 16468
            Command = ecDeleteWord
          end
          item
            ShortCut = 24661
            Command = ecBlockUnindent
          end
          item
            ShortCut = 16430
            Command = ecDelete
          end
          item
            ShortCut = 16473
            Command = ecDeleteLine
          end
          item
            ShortCut = 24665
            Command = ecDeleteEOL
          end
          item
            ShortCut = 16432
            Command = ecGotoMarker0
          end
          item
            ShortCut = 16433
            Command = ecGotoMarker1
          end
          item
            ShortCut = 16434
            Command = ecGotoMarker2
          end
          item
            ShortCut = 16435
            Command = ecGotoMarker3
          end
          item
            ShortCut = 16436
            Command = ecGotoMarker4
          end
          item
            ShortCut = 16437
            Command = ecGotoMarker5
          end
          item
            ShortCut = 16438
            Command = ecGotoMarker6
          end
          item
            ShortCut = 16439
            Command = ecGotoMarker7
          end
          item
            ShortCut = 16440
            Command = ecGotoMarker8
          end
          item
            ShortCut = 16441
            Command = ecGotoMarker9
          end
          item
            ShortCut = 24624
            Command = ecToggleMarker0
          end
          item
            ShortCut = 24625
            Command = ecToggleMarker1
          end
          item
            ShortCut = 24626
            Command = ecToggleMarker2
          end
          item
            ShortCut = 24627
            Command = ecToggleMarker3
          end
          item
            ShortCut = 24628
            Command = ecToggleMarker4
          end
          item
            ShortCut = 24629
            Command = ecToggleMarker5
          end
          item
            ShortCut = 24630
            Command = ecToggleMarker6
          end
          item
            ShortCut = 24631
            Command = ecToggleMarker7
          end
          item
            ShortCut = 24632
            Command = ecToggleMarker8
          end
          item
            ShortCut = 24633
            Command = ecToggleMarker9
          end>
        LineNumberFont.Charset = DEFAULT_CHARSET
        LineNumberFont.Color = clBlack
        LineNumberFont.Height = -8
        LineNumberFont.Name = 'Terminal'
        LineNumberFont.Style = []
        MaxUndo = 10
        Options = [eoAutoIndent, eoAutoUnindent, eoCursorThroughTabs, eoGroupUndo, eoHideSelection, eoInserting, eoLineNumbers, eoLineNumbersZeroBased, eoReadOnly, eoScrollPastEOL, eoSmartTabs, eoTripleClicks, eoUseSyntaxHighlighting, eoWantTabs]
        ParentColor = False
        ParentFont = False
        ScrollBars = ssVertical
        ScrollHintColor.Background = clAppWorkSpace
        ScrollHintColor.Foreground = clInfoText
        ScrollHintColor.FontStyles = []
        ScrollHintColor.ForceFontStyles = False
        SelectedColor.Background = clBlack
        SelectedColor.Foreground = clBlack
        SelectedColor.FontStyles = []
        SelectedColor.ForceFontStyles = False
        TabOrder = 0
        WorkWidth = 415
        OnScroll = HexUniCodeEdScroll
      end
      object PlainTextUniCodeEdit: TUniCodeEdit
        Left = 420
        Top = 0
        Width = 147
        Height = 306
        Cursor = crIBeam
        Align = alRight
        CharWidth = 7
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Bitstream Vera Sans Mono'
        Font.Pitch = fpFixed
        Font.Style = []
        GutterColor = clBtnFace
        GutterWidth = 0
        Keystrokes = <
          item
            ShortCut = 8230
            Command = ecSelectUp
          end
          item
            ShortCut = 38
            Command = ecUp
          end
          item
            ShortCut = 16422
            Command = ecScrollUp
          end
          item
            ShortCut = 40
            Command = ecDown
          end
          item
            ShortCut = 8232
            Command = ecSelectDown
          end
          item
            ShortCut = 16424
            Command = ecScrollDown
          end
          item
            ShortCut = 37
            Command = ecLeft
          end
          item
            ShortCut = 8229
            Command = ecSelectLeft
          end
          item
            ShortCut = 16421
            Command = ecWordLeft
          end
          item
            ShortCut = 24613
            Command = ecSelectWordLeft
          end
          item
            ShortCut = 39
            Command = ecRight
          end
          item
            ShortCut = 8231
            Command = ecSelectRight
          end
          item
            ShortCut = 16423
            Command = ecWordRight
          end
          item
            ShortCut = 24615
            Command = ecSelectWordRight
          end
          item
            ShortCut = 34
            Command = ecPageDown
          end
          item
            ShortCut = 8226
            Command = ecSelectPageDown
          end
          item
            ShortCut = 16418
            Command = ecPageBottom
          end
          item
            ShortCut = 24610
            Command = ecSelectPageBottom
          end
          item
            ShortCut = 33
            Command = ecPageUp
          end
          item
            ShortCut = 8225
            Command = ecSelectPageUp
          end
          item
            ShortCut = 16417
            Command = ecPageTop
          end
          item
            ShortCut = 24609
            Command = ecSelectPageTop
          end
          item
            ShortCut = 36
            Command = ecLineStart
          end
          item
            ShortCut = 8228
            Command = ecSelectLineStart
          end
          item
            ShortCut = 16420
            Command = ecEditorTop
          end
          item
            ShortCut = 24612
            Command = ecSelectEditorTop
          end
          item
            ShortCut = 35
            Command = ecLineEnd
          end
          item
            ShortCut = 8227
            Command = ecSelectLineEnd
          end
          item
            ShortCut = 16419
            Command = ecEditorBottom
          end
          item
            ShortCut = 24611
            Command = ecSelectEditorBottom
          end
          item
            ShortCut = 45
            Command = ecToggleMode
          end
          item
            ShortCut = 16470
            Command = ecPaste
          end
          item
            ShortCut = 8237
            Command = ecPaste
          end
          item
            ShortCut = 46
            Command = ecDeleteChar
          end
          item
            ShortCut = 8
            Command = ecDeleteLastChar
          end
          item
            ShortCut = 16392
            Command = ecDeleteLastWord
          end
          item
            ShortCut = 16474
            Command = ecUndo
          end
          item
            ShortCut = 32776
            Command = ecUndo
          end
          item
            ShortCut = 24666
            Command = ecRedo
          end
          item
            ShortCut = 40968
            Command = ecRedo
          end
          item
            ShortCut = 13
            Command = ecLineBreak
          end
          item
            ShortCut = 16461
            Command = ecLineBreak
          end
          item
            ShortCut = 16449
            Command = ecSelectAll
          end
          item
            ShortCut = 16451
            Command = ecCopy
          end
          item
            ShortCut = 16429
            Command = ecCopy
          end
          item
            ShortCut = 16472
            Command = ecCut
          end
          item
            ShortCut = 8238
            Command = ecCut
          end
          item
            ShortCut = 24649
            Command = ecBlockIndent
          end
          item
            ShortCut = 16462
            Command = ecInsertLine
          end
          item
            ShortCut = 16468
            Command = ecDeleteWord
          end
          item
            ShortCut = 24661
            Command = ecBlockUnindent
          end
          item
            ShortCut = 16430
            Command = ecDelete
          end
          item
            ShortCut = 16473
            Command = ecDeleteLine
          end
          item
            ShortCut = 24665
            Command = ecDeleteEOL
          end
          item
            ShortCut = 16432
            Command = ecGotoMarker0
          end
          item
            ShortCut = 16433
            Command = ecGotoMarker1
          end
          item
            ShortCut = 16434
            Command = ecGotoMarker2
          end
          item
            ShortCut = 16435
            Command = ecGotoMarker3
          end
          item
            ShortCut = 16436
            Command = ecGotoMarker4
          end
          item
            ShortCut = 16437
            Command = ecGotoMarker5
          end
          item
            ShortCut = 16438
            Command = ecGotoMarker6
          end
          item
            ShortCut = 16439
            Command = ecGotoMarker7
          end
          item
            ShortCut = 16440
            Command = ecGotoMarker8
          end
          item
            ShortCut = 16441
            Command = ecGotoMarker9
          end
          item
            ShortCut = 24624
            Command = ecToggleMarker0
          end
          item
            ShortCut = 24625
            Command = ecToggleMarker1
          end
          item
            ShortCut = 24626
            Command = ecToggleMarker2
          end
          item
            ShortCut = 24627
            Command = ecToggleMarker3
          end
          item
            ShortCut = 24628
            Command = ecToggleMarker4
          end
          item
            ShortCut = 24629
            Command = ecToggleMarker5
          end
          item
            ShortCut = 24630
            Command = ecToggleMarker6
          end
          item
            ShortCut = 24631
            Command = ecToggleMarker7
          end
          item
            ShortCut = 24632
            Command = ecToggleMarker8
          end
          item
            ShortCut = 24633
            Command = ecToggleMarker9
          end>
        LineNumberFont.Charset = DEFAULT_CHARSET
        LineNumberFont.Color = clBlack
        LineNumberFont.Height = -8
        LineNumberFont.Name = 'Terminal'
        LineNumberFont.Style = []
        MaxUndo = 10
        Options = [eoAutoIndent, eoAutoUnindent, eoCursorThroughTabs, eoGroupUndo, eoHideSelection, eoInserting, eoReadOnly, eoScrollPastEOL, eoSmartTabs, eoTripleClicks, eoUseSyntaxHighlighting, eoWantTabs]
        ParentColor = False
        ParentFont = False
        ScrollBars = ssVertical
        ScrollHintColor.Background = clAppWorkSpace
        ScrollHintColor.Foreground = clInfoText
        ScrollHintColor.FontStyles = []
        ScrollHintColor.ForceFontStyles = False
        SelectedColor.Background = clBlack
        SelectedColor.Foreground = clBlack
        SelectedColor.FontStyles = []
        SelectedColor.ForceFontStyles = False
        TabOrder = 1
        WorkWidth = 143
        OnScroll = PlainTextUniCodeEditScroll
      end
      object TntPanel1: TTntPanel
        Left = 0
        Top = 306
        Width = 567
        Height = 47
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          567
          47)
        object BinarySizeTitelLbl: TTntLabel
          Left = 6
          Top = 8
          Width = 23
          Height = 13
          Caption = 'Size:'
        end
        object BinarySizeLbl: TTntLabel
          Left = 52
          Top = 8
          Width = 3
          Height = 13
          Caption = '-'
        end
        object OK3Btn: TTntButton
          Left = 391
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'OK'
          ModalResult = 1
          TabOrder = 0
        end
        object Cancel3Btn: TTntButton
          Left = 478
          Top = 12
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
      end
    end
  end
end
