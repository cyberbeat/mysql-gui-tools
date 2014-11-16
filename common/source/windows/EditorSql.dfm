object EditorSqlForm: TEditorSqlForm
  Left = 375
  Top = 218
  ActiveControl = SqlUCE
  Caption = 'MySQL SQL Editor'
  ClientHeight = 433
  ClientWidth = 596
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPnl: TTntPanel
    Left = 0
    Top = 382
    Width = 596
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      596
      51)
    object SepBevel: TTntBevel
      Left = 0
      Top = 0
      Width = 596
      Height = 3
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 593
    end
    object ExecuteSQLBtn: TTntButton
      Left = 370
      Top = 13
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Execute SQL'
      TabOrder = 0
      OnClick = ExecuteSQLBtnClick
    end
    object CancelBtn: TTntButton
      Left = 482
      Top = 13
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object SqlUCE: TUniCodeEdit
    Left = 0
    Top = 0
    Width = 596
    Height = 382
    Cursor = crIBeam
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    CharWidth = 7
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Bitstream Vera Sans Mono'
    Font.Pitch = fpFixed
    Font.Style = []
    GutterColor = clBtnFace
    GutterWidth = 40
    HighLighter = UCESQLHighlighter
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
        ShortCut = 16430
        Command = ecDeleteWord
      end
      item
        ShortCut = 24661
        Command = ecBlockUnindent
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
    Options = [eoAutoIndent, eoAutoUnindent, eoCursorThroughTabs, eoGroupUndo, eoHideSelection, eoInserting, eoLineNumbers, eoOptimalFill, eoScrollPastEOL, eoShowScrollHint, eoTripleClicks, eoUseUndoRedo, eoUseSyntaxHighlighting, eoWantTabs]
    ParentColor = False
    ParentFont = False
    RightMargin = -1
    ScrollHintColor.Background = clAppWorkSpace
    ScrollHintColor.Foreground = clInfoText
    ScrollHintColor.FontStyles = []
    ScrollHintColor.ForceFontStyles = False
    SelectedColor.Background = clHighlight
    SelectedColor.Foreground = clHighlightText
    SelectedColor.FontStyles = []
    SelectedColor.ForceFontStyles = False
    TabOrder = 1
    WorkWidth = 596
    OnKeyPress = SqlUCEKeyPress
  end
  object UCESQLHighlighter: TUCESQLHighlighter
    DefaultFilter = 'SQL script files (*.sql)|*.sql'
    CommentAttributes.Background = clWindow
    CommentAttributes.Foreground = clGray
    CommentAttributes.Style = [fsItalic]
    EmbeddedCommandAttributes.Background = clWindow
    EmbeddedCommandAttributes.Foreground = clWindowText
    EmbeddedCommandAttributes.Style = []
    IdentifierAttributes.Background = clWindow
    IdentifierAttributes.Foreground = clWindowText
    IdentifierAttributes.Style = []
    KeyAttributes.Background = clWindow
    KeyAttributes.Foreground = clBlue
    KeyAttributes.Style = [fsBold]
    NumberAttributes.Background = clWindow
    NumberAttributes.Foreground = clFuchsia
    NumberAttributes.Style = []
    QuotedIDAttributes.Background = clWindow
    QuotedIDAttributes.Foreground = clWindowText
    QuotedIDAttributes.Style = []
    SpaceAttributes.Background = clWindow
    SpaceAttributes.Foreground = clWindowText
    SpaceAttributes.Style = []
    StringAttributes.Background = clWindow
    StringAttributes.Foreground = clPurple
    StringAttributes.Style = []
    SymbolAttributes.Background = clWindow
    SymbolAttributes.Foreground = clWindowText
    SymbolAttributes.Style = []
    SystemVariableAttributes.Background = clWindow
    SystemVariableAttributes.Foreground = clWindowText
    SystemVariableAttributes.Style = []
    UserVariableAttributes.Background = clWindow
    UserVariableAttributes.Foreground = clWindowText
    UserVariableAttributes.Style = []
    Left = 524
    Top = 16
  end
end
