object MyxTableEditorForm: TMyxTableEditorForm
  Left = 657
  Top = 369
  Caption = 'Table Editor'
  ClientHeight = 416
  ClientWidth = 707
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ScreenSnap = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPnl: TTntPanel
    Left = 0
    Top = 365
    Width = 707
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      707
      51)
    object ApplyChangesBtn: TTntButton
      Left = 476
      Top = 13
      Width = 101
      Height = 25
      Action = ApplyChangesAction
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object CancelBtn: TTntButton
      Left = 592
      Top = 13
      Width = 101
      Height = 25
      Action = CloseAction
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
    object AdvancedBtn: TTntButton
      Left = 13
      Top = 13
      Width = 91
      Height = 25
      Action = ShowAdvancedAction
      Caption = 'Advanced >>'
      TabOrder = 2
    end
  end
  object LeftPnl: TTntPanel
    Left = 0
    Top = 12
    Width = 13
    Height = 353
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
  object RightPnl: TTntPanel
    Left = 694
    Top = 12
    Width = 13
    Height = 353
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 12
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
  end
  object TablePageControl: TTntPageControl
    Left = 13
    Top = 12
    Width = 681
    Height = 353
    ActivePage = ColumnSheet
    Align = alClient
    TabOrder = 4
    OnChange = TablePageControlChange
    object TableSheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'Table'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TableSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          649
          301)
        object TableSheetBgShape: TTntShape
          Left = 0
          Top = 0
          Width = 649
          Height = 301
          Align = alClient
          Brush.Color = clBtnFace
          Pen.Color = clBtnFace
          Visible = False
        end
        object EditorIcon: TTntImage
          Left = 0
          Top = 0
          Width = 48
          Height = 48
          AutoSize = True
        end
        object TableNameLbl: TTntLabel
          Left = 59
          Top = 6
          Width = 60
          Height = 13
          Alignment = taRightJustify
          Caption = 'Table Name:'
          Transparent = True
        end
        object TntLabel3: TTntLabel
          Left = 83
          Top = 42
          Width = 36
          Height = 13
          Alignment = taRightJustify
          Caption = 'Engine:'
          Transparent = True
        end
        object TntLabel1: TTntLabel
          Left = 73
          Top = 78
          Width = 45
          Height = 13
          Alignment = taRightJustify
          Caption = 'Collation:'
          Transparent = True
        end
        object TntLabel2: TTntLabel
          Left = 70
          Top = 120
          Width = 49
          Height = 13
          Alignment = taRightJustify
          Caption = 'Comment:'
          Transparent = True
        end
        object TntLabel6: TTntLabel
          Left = 312
          Top = 6
          Width = 335
          Height = 30
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 
            'The name of the table. It is recommended to use only alpha-numer' +
            'ic characters. Spaces should be avoided and be replaced by _'
          Transparent = True
          WordWrap = True
        end
        object TntLabel9: TTntLabel
          Left = 312
          Top = 42
          Width = 335
          Height = 30
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 
            'The database engine that is used for the table. This option spec' +
            'ifies the performance, data consistency and much more.'
          Transparent = True
          WordWrap = True
        end
        object TntLabel10: TTntLabel
          Left = 312
          Top = 78
          Width = 335
          Height = 30
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 
            'The collation specifies which language specific characters can b' +
            'e stored in the table. Common choices are Latin1 or UTF8.'
          Transparent = True
          WordWrap = True
        end
        object TableNameEd: TTntEdit
          Left = 124
          Top = 2
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = TableNameEdChange
          OnKeyDown = TableNameEdKeyDown
        end
        object TableEngineComboBox: TTntComboBox
          Left = 124
          Top = 38
          Width = 171
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 0
          TabOrder = 1
          OnCloseUp = TableEngineComboBoxCloseUp
        end
        object CollationComboBox: TTntComboBox
          Left = 125
          Top = 74
          Width = 171
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 50
          ItemHeight = 0
          TabOrder = 2
          OnCloseUp = CollationComboBoxCloseUp
        end
        object TntMemo1: TTntMemo
          Left = 126
          Top = 116
          Width = 523
          Height = 181
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 3
        end
      end
    end
    object ColumnSheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'Columns'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ColumnsContentPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ColumnsDetailsPnl: TTntPanel
          Left = 0
          Top = 142
          Width = 649
          Height = 159
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            649
            159)
          object ColumnsDetailsBgShape: TTntShape
            Left = 0
            Top = 0
            Width = 649
            Height = 159
            Align = alClient
            Brush.Color = clBtnFace
            Pen.Color = clBtnFace
            Visible = False
          end
          object ColumnDetailsGroupBox: TTntGroupBox
            Left = 0
            Top = 8
            Width = 649
            Height = 150
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = 'Column Details'
            TabOrder = 0
            DesignSize = (
              649
              150)
            object TntLabel4: TTntLabel
              Left = 42
              Top = 26
              Width = 31
              Height = 13
              Alignment = taRightJustify
              Caption = 'Name:'
            end
            object TntLabel5: TTntLabel
              Left = 268
              Top = 26
              Width = 49
              Height = 13
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              Caption = 'Datatype:'
            end
            object TntLabel37: TTntLabel
              Left = 273
              Top = 59
              Width = 45
              Height = 13
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              Caption = 'Collation:'
            end
            object TntLabel7: TTntLabel
              Left = 34
              Top = 60
              Width = 39
              Height = 13
              Alignment = taRightJustify
              Caption = 'Default:'
            end
            object TntLabel8: TTntLabel
              Left = 24
              Top = 93
              Width = 49
              Height = 13
              Caption = 'Comment:'
            end
            object ColumnNameEd: TTntEdit
              Left = 80
              Top = 22
              Width = 163
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
              OnChange = ColumnDetailChange
            end
            object ColumnDatatypeEd: TTntEdit
              Left = 323
              Top = 22
              Width = 156
              Height = 21
              Anchors = [akTop, akRight]
              TabOrder = 1
              OnChange = ColumnDetailChange
            end
            object ColumnFlagsCheckListBox: TTntCheckListBox
              Left = 494
              Top = 22
              Width = 137
              Height = 110
              OnClickCheck = ColumnDetailChange
              Anchors = [akTop, akRight, akBottom]
              ItemHeight = 13
              TabOrder = 2
            end
            object ColumnCharsetCBox: TTntComboBox
              Left = 323
              Top = 55
              Width = 156
              Height = 21
              Style = csDropDownList
              Anchors = [akTop, akRight]
              ItemHeight = 0
              TabOrder = 3
              OnCloseUp = ColumnDetailChange
            end
            object ColumnCommentMemo: TTntMemo
              Left = 80
              Top = 90
              Width = 399
              Height = 42
              Anchors = [akLeft, akTop, akRight, akBottom]
              TabOrder = 4
              OnChange = ColumnDetailChange
            end
            object SetDefValNullBtn: TTntBitBtn
              Left = 213
              Top = 55
              Width = 30
              Height = 23
              Anchors = [akTop, akRight]
              TabOrder = 5
              OnClick = SetDefValNullBtnClick
              Glyph.Data = {
                BE020000424DBE02000000000000360000002800000017000000090000000100
                18000000000088020000120B0000120B00000000000000000000F600FF7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F00
                0000F600FF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F000000F600FF7F7F7F7F7F7FFFFFFF7F7F7F7F7F7FFFFFFF7F
                7F7F7F7F7FFFFFFFFFFFFF7F7F7F7F7F7FFFFFFFFFFFFFFFFFFF7F7F7FFFFFFF
                FFFFFFFFFFFF7F7F7F7F7F7F7F7F7F000000F600FF7F7F7F7F7F7FFFFFFF7F7F
                7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7F7F
                7F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F000000F600FF7F7F7F
                7F7F7FFFFFFF7F7F7FFFFFFFFFFFFF7F7F7FFFFFFF7F7F7F7F7F7FFFFFFF7F7F
                7FFFFFFF7F7F7F7F7F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F00
                0000F600FF7F7F7F7F7F7FFFFFFFFFFFFF7F7F7FFFFFFF7F7F7FFFFFFF7F7F7F
                7F7F7FFFFFFF7F7F7FFFFFFF7F7F7F7F7F7F7F7F7FFFFFFF7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F000000F600FF7F7F7F7F7F7FFFFFFF7F7F7F7F7F7FFFFFFF7F
                7F7FFFFFFF7F7F7F7F7F7FFFFFFF7F7F7FFFFFFF7F7F7F7F7F7F7F7F7FFFFFFF
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F000000F600FF7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F000000F600FF7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F
                7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F7F00
                0000}
              Margin = 0
            end
            object ColumnDefaultValueEd: TTntEdit
              Left = 80
              Top = 56
              Width = 131
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 6
              OnChange = ColumnDetailChange
            end
          end
        end
        object ColumnVST: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 649
          Height = 142
          Align = alClient
          ButtonStyle = bsTriangle
          ClipboardFormats.Strings = (
            'CSV'
            'HTML Format'
            'Plain text'
            'Rich Text Format'
            'Rich Text Format Without Objects'
            'Unicode text'
            'Virtual Tree Data')
          DefaultNodeHeight = 19
          DefaultPasteMode = amInsertAfter
          DragMode = dmAutomatic
          DragOperations = []
          Header.AutoSizeIndex = -1
          Header.Font.Charset = ANSI_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Images = ColumnHeaderImgList
          Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowImages, hoVisible, hoAutoSpring]
          Images = ColumnImgList
          LineStyle = lsSolid
          PopupMenu = ColumnPopupMenu
          TabOrder = 1
          TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowDropmark, toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnAfterCellPaint = ColumnVSTAfterCellPaint
          OnCreateEditor = ColumnVSTCreateEditor
          OnDblClick = ColumnVSTDblClick
          OnDragOver = ColumnVSTDragOver
          OnDragDrop = ColumnVSTDragDrop
          OnEditing = ColumnVSTEditing
          OnFocusChanged = ColumnVSTFocusChanged
          OnFreeNode = ColumnVSTFreeNode
          OnGetText = ColumnVSTGetText
          OnGetImageIndex = ColumnVSTGetImageIndex
          OnInitNode = ColumnVSTInitNode
          OnLoadNode = ColumnVSTLoadNode
          OnMouseDown = ColumnVSTMouseDown
          OnNewText = ColumnVSTNewText
          OnSaveNode = ColumnVSTSaveNode
          Columns = <
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
              Position = 0
              Width = 127
              WideText = 'Column Name'
            end
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
              Position = 1
              Width = 120
              WideText = 'Datatype'
            end
            item
              ImageIndex = 0
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
              Position = 2
              Width = 32
            end
            item
              ImageIndex = 1
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coVisible]
              Position = 3
              Width = 32
            end
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
              Position = 4
              Width = 180
              WideText = 'Flags'
            end
            item
              Position = 5
              Width = 90
              WideText = 'Default Value'
            end
            item
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
              Position = 6
              Width = 68
              WideText = 'Comment'
            end>
          WideDefaultText = ''
        end
      end
    end
    object IndicesSheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'Indices'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object IndexContentPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object IndexColumnSplitter: TTntSplitter
          Left = 0
          Top = 91
          Width = 649
          Height = 10
          Cursor = crVSplit
          Align = alTop
          Color = clBtnFace
          ParentColor = False
        end
        object IndexDetailsPnl: TTntPanel
          Left = 0
          Top = 202
          Width = 649
          Height = 99
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          Visible = False
          DesignSize = (
            649
            99)
          object IndexDetailsBgShape: TTntShape
            Left = 0
            Top = 0
            Width = 649
            Height = 99
            Align = alClient
            Brush.Color = clBtnFace
            Pen.Color = clBtnFace
            Visible = False
          end
          object TntGroupBox3: TTntGroupBox
            Left = 0
            Top = 12
            Width = 649
            Height = 87
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = 'Index Details'
            TabOrder = 0
          end
        end
        object IndexVST: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 649
          Height = 91
          Align = alTop
          ButtonStyle = bsTriangle
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoVisible]
          PopupMenu = ColumnPopupMenu
          TabOrder = 1
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnCreateEditor = IndexVSTCreateEditor
          OnDblClick = IndexVSTDblClick
          OnFocusChanged = IndexVSTFocusChanged
          OnFreeNode = IndexVSTFreeNode
          OnGetText = IndexVSTGetText
          OnInitNode = IndexVSTInitNode
          OnNewText = IndexVSTNewText
          Columns = <
            item
              Position = 0
              Width = 150
              WideText = 'Index Name'
            end
            item
              Position = 1
              Width = 80
              WideText = 'Type'
            end
            item
              Position = 2
              Width = 200
              WideText = 'Comment'
            end>
          WideDefaultText = ''
        end
        object IndexColumnVST: TVirtualStringTree
          Left = 0
          Top = 101
          Width = 649
          Height = 101
          Align = alClient
          ButtonStyle = bsTriangle
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoVisible]
          PopupMenu = ColumnPopupMenu
          TabOrder = 2
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnCreateEditor = IndexColumnVSTCreateEditor
          OnDblClick = IndexColumnVSTDblClick
          OnFreeNode = IndexColumnVSTFreeNode
          OnGetText = IndexColumnVSTGetText
          OnInitNode = IndexColumnVSTInitNode
          OnNewText = IndexColumnVSTNewText
          Columns = <
            item
              Position = 0
              Width = 150
              WideText = 'Column'
            end
            item
              Position = 1
              Width = 80
              WideText = 'Order'
            end
            item
              Position = 2
              Width = 60
              WideText = 'Length'
            end
            item
              Position = 3
              Width = 150
              WideText = 'Stored Function'
            end
            item
              Position = 4
              Width = 185
              WideText = 'Comment'
            end>
          WideDefaultText = ''
        end
      end
    end
    object ForeignKeySheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'Foreign Keys'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object FkSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object ForeignKeyColumnSplitter: TTntSplitter
          Left = 0
          Top = 111
          Width = 649
          Height = 10
          Cursor = crVSplit
          Align = alTop
          Color = clBtnFace
          ParentColor = False
        end
        object ForeignKeyVST: TVirtualStringTree
          Left = 0
          Top = 0
          Width = 649
          Height = 111
          Align = alTop
          ButtonStyle = bsTriangle
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoVisible]
          PopupMenu = ColumnPopupMenu
          TabOrder = 0
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnCreateEditor = ForeignKeyVSTCreateEditor
          OnDblClick = ForeignKeyVSTDblClick
          OnFocusChanged = ForeignKeyVSTFocusChanged
          OnFreeNode = ForeignKeyVSTFreeNode
          OnGetText = ForeignKeyVSTGetText
          OnInitNode = ForeignKeyVSTInitNode
          OnNewText = ForeignKeyVSTNewText
          Columns = <
            item
              Position = 0
              Width = 150
              WideText = 'Foreign Key Name'
            end
            item
              Position = 1
              Width = 150
              WideText = 'Refered Table'
            end
            item
              Position = 2
              Width = 90
              WideText = 'On Update'
            end
            item
              Position = 3
              Width = 90
              WideText = 'On Delete'
            end
            item
              Position = 4
              Width = 160
              WideText = 'Comment'
            end>
          WideDefaultText = ''
        end
        object ForeignKeyColumnVST: TVirtualStringTree
          Left = 0
          Top = 121
          Width = 649
          Height = 180
          Align = alClient
          ButtonStyle = bsTriangle
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'MS Sans Serif'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoDrag, hoVisible]
          PopupMenu = ColumnPopupMenu
          TabOrder = 1
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toThemeAware, toUseBlendedImages]
          TreeOptions.SelectionOptions = [toExtendedFocus]
          TreeOptions.StringOptions = [toAutoAcceptEditChange]
          OnCreateEditor = ForeignKeyColumnVSTCreateEditor
          OnDblClick = ForeignKeyColumnVSTDblClick
          OnFreeNode = ForeignKeyColumnVSTFreeNode
          OnGetText = ForeignKeyColumnVSTGetText
          OnInitNode = ForeignKeyColumnVSTInitNode
          OnNewText = ForeignKeyColumnVSTNewText
          Columns = <
            item
              Position = 0
              Width = 180
              WideText = 'Column'
            end
            item
              Position = 1
              Width = 180
              WideText = 'Refered Column'
            end>
          WideDefaultText = ''
        end
      end
    end
    object TriggersTab: TTntTabSheet
      BorderWidth = 12
      Caption = 'Triggers'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RoutinesSheetTab: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 301
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object TntShape1: TTntShape
          Left = 648
          Top = 1
          Width = 1
          Height = 299
          Align = alRight
          Brush.Color = 10263441
          Pen.Color = 10263441
          ExplicitHeight = 297
        end
        object TntShape2: TTntShape
          Left = 0
          Top = 1
          Width = 1
          Height = 299
          Align = alLeft
          Brush.Color = 10263441
          Pen.Color = 10263441
          ExplicitHeight = 297
        end
        object TntShape3: TTntShape
          Left = 0
          Top = 0
          Width = 649
          Height = 1
          Align = alTop
          Brush.Color = 10263441
          Pen.Color = 10263441
        end
        object TntShape4: TTntShape
          Left = 0
          Top = 300
          Width = 649
          Height = 1
          Align = alBottom
          Brush.Color = 10263441
          Pen.Color = 10263441
          ExplicitTop = 298
        end
        object RoutinesScrollBox: TTntScrollBox
          Left = 1
          Top = 1
          Width = 647
          Height = 299
          HorzScrollBar.Smooth = True
          HorzScrollBar.Tracking = True
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          Color = clAppWorkSpace
          ParentColor = False
          TabOrder = 0
        end
      end
    end
    object TntTabSheet1: TTntTabSheet
      Caption = 'Table Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object AdvancedOptionsSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 673
        Height = 325
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object AdvancedOptionsScrollBox: TTntScrollBox
          Left = 0
          Top = 0
          Width = 673
          Height = 325
          HorzScrollBar.Range = 641
          HorzScrollBar.Smooth = True
          HorzScrollBar.Tracking = True
          VertScrollBar.Range = 720
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Align = alClient
          AutoScroll = False
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          TabOrder = 0
          object ScrollSpacerAdvOptShape: TTntShape
            Left = 11
            Top = 871
            Width = 65
            Height = 3
            Brush.Style = bsClear
            Pen.Style = psClear
          end
          object VariousGBox: TTntGroupBox
            Left = 10
            Top = 10
            Width = 631
            Height = 185
            Caption = 'Various'
            TabOrder = 0
            object TntLabel19: TTntLabel
              Left = 14
              Top = 26
              Width = 52
              Height = 13
              Caption = 'Pack Keys:'
            end
            object TntLabel20: TTntLabel
              Left = 14
              Top = 115
              Width = 79
              Height = 13
              Caption = 'Auto Increment:'
            end
            object TntLabel21: TTntLabel
              Left = 14
              Top = 75
              Width = 79
              Height = 13
              Caption = 'Table Password:'
            end
            object TntLabel51: TTntLabel
              Left = 230
              Top = 25
              Width = 390
              Height = 44
              AutoSize = False
              Caption = 
                'Use this option to generate smaller indices. This usually makes ' +
                'updates slower and reads faster. Setting it to DEFAULT tells the' +
                ' storage engine to only pack long CHAR/VARCHAR columns.'
              WordWrap = True
            end
            object TntLabel52: TTntLabel
              Left = 230
              Top = 75
              Width = 390
              Height = 31
              AutoSize = False
              Caption = 
                'Password to encrypt the table definition file. This option doesn' +
                #39't do anything in the standard MySQL version.'
              WordWrap = True
            end
            object TntLabel53: TTntLabel
              Left = 230
              Top = 115
              Width = 390
              Height = 18
              AutoSize = False
              Caption = 'The initial AUTO_INCREMENT value for the table, only for MyISAM.'
              WordWrap = True
            end
            object TntLabel54: TTntLabel
              Left = 230
              Top = 144
              Width = 390
              Height = 31
              AutoSize = False
              Caption = 
                'Use this option to delay the key updates until the table is clos' +
                'ed. This works for MyISAM only.'
              WordWrap = True
            end
            object PackKeysComboBox: TTntComboBox
              Left = 110
              Top = 23
              Width = 105
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
              OnCloseUp = PackKeysComboBoxCloseUp
            end
            object AutoIncEd: TTntEdit
              Left = 110
              Top = 111
              Width = 105
              Height = 21
              TabOrder = 2
              OnChange = NextAutoIncChanged
            end
            object TablePasswordEd: TTntEdit
              Left = 110
              Top = 71
              Width = 105
              Height = 21
              TabOrder = 1
              OnChange = TablePasswordChange
            end
            object DelayKeyUpdatesCBox: TTntCheckBox
              Left = 14
              Top = 143
              Width = 201
              Height = 17
              Caption = 'Delay Key Updates'
              TabOrder = 3
              OnClick = OnDelayKeyUpdatesChange
            end
          end
          object RowOptionsGBox: TTntGroupBox
            Left = 10
            Top = 202
            Width = 631
            Height = 237
            Caption = 'Row Options'
            TabOrder = 1
            object TntLabel22: TTntLabel
              Left = 14
              Top = 130
              Width = 67
              Height = 13
              Caption = 'Avg Row Len:'
            end
            object TntLabel24: TTntLabel
              Left = 14
              Top = 170
              Width = 49
              Height = 13
              Caption = 'Min Rows:'
            end
            object TntLabel25: TTntLabel
              Left = 14
              Top = 204
              Width = 53
              Height = 13
              Caption = 'Max Rows:'
            end
            object TntLabel26: TTntLabel
              Left = 14
              Top = 26
              Width = 62
              Height = 13
              Caption = 'Row Format:'
            end
            object TntLabel39: TTntLabel
              Left = 230
              Top = 25
              Width = 390
              Height = 44
              AutoSize = False
              Caption = 
                'Defines how the rows in MyISAM tables should be stored. The opti' +
                'on value can FIXED or DYNAMIC for static or variable-length row ' +
                'format. The utility myisampack can be used to set the type to CO' +
                'MPRESSED.'
              WordWrap = True
            end
            object TntLabel60: TTntLabel
              Left = 230
              Top = 75
              Width = 390
              Height = 44
              AutoSize = False
              Caption = 
                'Activate this option if you want MySQL to maintain a live checks' +
                'um for all rows. This makes the table a little slower to update,' +
                ' but also makes it easier to find corrupted tables.'
              WordWrap = True
            end
            object TntLabel61: TTntLabel
              Left = 230
              Top = 129
              Width = 390
              Height = 31
              AutoSize = False
              Caption = 
                'An approximation of the average row length for your table. You n' +
                'eed to set this only for large tables with variable-size records' +
                '.'
              WordWrap = True
            end
            object TntLabel62: TTntLabel
              Left = 230
              Top = 171
              Width = 390
              Height = 18
              AutoSize = False
              Caption = 'The minimum number of rows you plan to store in the table.'
              WordWrap = True
            end
            object TntLabel63: TTntLabel
              Left = 230
              Top = 203
              Width = 390
              Height = 18
              AutoSize = False
              Caption = 'The maximum number of rows you plan to store in the table.'
              WordWrap = True
            end
            object AvgRowLengthEd: TTntEdit
              Left = 110
              Top = 126
              Width = 105
              Height = 21
              TabOrder = 2
              OnChange = AvgRowLengthChanged
            end
            object MinRowsEd: TTntEdit
              Left = 110
              Top = 166
              Width = 105
              Height = 21
              TabOrder = 3
              OnChange = MinRowChanged
            end
            object MaxRowsEd: TTntEdit
              Left = 110
              Top = 200
              Width = 105
              Height = 21
              TabOrder = 4
              OnChange = MaxRowsChanged
            end
            object UseChecksumCBox: TTntCheckBox
              Left = 14
              Top = 74
              Width = 191
              Height = 17
              Caption = 'Use Checksum'
              TabOrder = 1
              OnClick = UseChecksumCheckBoxClicked
            end
            object RowFormatComboBox: TTntComboBox
              Left = 110
              Top = 22
              Width = 105
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
              OnCloseUp = RowFormatComboBoxCloseUp
            end
          end
          object StorageOptionsGBox: TTntGroupBox
            Left = 10
            Top = 446
            Width = 631
            Height = 137
            Caption = 'Storage Options'
            TabOrder = 2
            object TntLabel27: TTntLabel
              Left = 14
              Top = 25
              Width = 74
              Height = 13
              Caption = 'Data Directory:'
            end
            object TntLabel28: TTntLabel
              Left = 14
              Top = 79
              Width = 79
              Height = 13
              Caption = 'Index Directory:'
            end
            object TntLabel55: TTntLabel
              Left = 368
              Top = 25
              Width = 253
              Height = 44
              AutoSize = False
              Caption = 
                'Directory where to put the tables data file. This works only for' +
                ' MyISAM tables only and not on some operating systems (Windows).'
              WordWrap = True
            end
            object TntLabel56: TTntLabel
              Left = 368
              Top = 79
              Width = 253
              Height = 44
              AutoSize = False
              Caption = 
                'Directory where to put the tables index file. This works only fo' +
                'r MyISAM tables only and not on some operating systems (Windows)' +
                '.'
              WordWrap = True
            end
            object DataDirectoryEd: TTntEdit
              Left = 96
              Top = 21
              Width = 255
              Height = 21
              TabOrder = 0
              OnChange = DataDirChanged
            end
            object IndexDirectoryEd: TTntEdit
              Left = 96
              Top = 75
              Width = 255
              Height = 21
              TabOrder = 1
              OnChange = IndexDirChanged
            end
          end
          object MergeOptionGBox: TTntGroupBox
            Left = 10
            Top = 591
            Width = 631
            Height = 103
            Caption = 'Merge Table Options'
            TabOrder = 3
            object MergeInsertMethodCapionLbl: TTntLabel
              Left = 14
              Top = 68
              Width = 72
              Height = 13
              Caption = 'Insert Method:'
            end
            object UnionTablesCaptionLbl: TTntLabel
              Left = 14
              Top = 24
              Width = 65
              Height = 13
              Caption = 'Union Tables:'
            end
            object UnionTablesDescLbl: TTntLabel
              Left = 364
              Top = 26
              Width = 253
              Height = 31
              AutoSize = False
              Caption = 'List of MyISAM tables that should be used by the MERGE table.'
              WordWrap = True
            end
            object MergeInsertMethodDescLbl: TTntLabel
              Left = 364
              Top = 68
              Width = 253
              Height = 18
              AutoSize = False
              Caption = 'The union table which should be used for inserts.'
              WordWrap = True
            end
            object MergeInsertMethodComboBox: TTntComboBox
              Left = 96
              Top = 64
              Width = 255
              Height = 21
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 1
              OnCloseUp = MergeInsertMethodComboBoxCloseUp
            end
            object UnionTablesEd: TTntEdit
              Left = 96
              Top = 20
              Width = 255
              Height = 21
              TabOrder = 0
              OnChange = UnionTablesChanged
            end
          end
        end
      end
    end
    object TntTabSheet4: TTntTabSheet
      Caption = 'Standard Inserts'
      object StdInsertsSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 673
        Height = 325
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object StandardInsertsUCE: TUniCodeEdit
          Left = 0
          Top = 0
          Width = 673
          Height = 325
          Cursor = crIBeam
          Align = alClient
          CharWidth = 8
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          GutterColor = clBtnFace
          GutterWidth = 30
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
          ParentColor = False
          ParentFont = False
          ScrollHintColor.Background = clAppWorkSpace
          ScrollHintColor.Foreground = clInfoText
          ScrollHintColor.FontStyles = []
          ScrollHintColor.ForceFontStyles = False
          SelectedColor.Background = clBlack
          SelectedColor.Foreground = clBlack
          SelectedColor.FontStyles = []
          SelectedColor.ForceFontStyles = False
          TabOrder = 0
          WorkWidth = 0
        end
      end
    end
  end
  object ColumnImgList: TImageList
    Height = 12
    Width = 12
    Left = 29
    Top = 275
    Bitmap = {
      494C01010100040004000C000C00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000000C00000001002000000000000009
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000300000000C0000000100010000000000600000000000000000000000
      000000000000000000000000FFFFFF00FFF0000000000000FFF0000000000000
      FFF0000000000000FFF0000000000000FFF0000000000000FFF0000000000000
      FFF0000000000000FFF0000000000000FFF0000000000000FFF0000000000000
      FFF0000000000000FFF000000000000000000000000000000000000000000000
      000000000000}
  end
  object ColumnHeaderImgList: TImageList
    Height = 11
    Width = 18
    Left = 65
    Top = 277
    Bitmap = {
      494C010102000400040012000B00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000480000000B0000000100200000000000600C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000480000000B0000000100010000000000840000000000000000000000
      000000000000000000000000FFFFFF006CC456CFF0000000000000006B5DD6B7
      F0000000000000004B5DD4BFF0000000000000002B5DD2B7F000000000000000
      6B5DD6CFF000000000000000FFFFFFFFF0000000000000006CEFDB3B90000000
      000000006B6FC2DB60000000000000004B6FDADB60000000000000002B6FE6DB
      60000000000000006CC7E6D19000000000000000000000000000000000000000
      00000000000000000000}
  end
  object TntActionList: TTntActionList
    Left = 35
    Top = 147
    object NextTabsheetAction: TTntAction
      Caption = 'Next Tabsheet'
      ShortCut = 16418
      OnExecute = NextTabsheetActionExecute
    end
    object PreviousTabsheetAction: TTntAction
      Caption = 'Previous Tabsheet'
      ShortCut = 16417
      OnExecute = PreviousTabsheetActionExecute
    end
    object DeleteSelectedColumnsAction: TTntAction
      Category = 'Edit'
      Caption = 'Delete Selected Columns'
      ShortCut = 16430
      OnExecute = DeleteSelectedColumnsActionExecute
      OnUpdate = DeleteSelectedColumnsActionUpdate
    end
    object ApplyChangeAndCloseAction: TTntAction
      Caption = 'Apply Change And Close'
      ShortCut = 16397
      OnExecute = ApplyChangeAndCloseActionExecute
    end
    object ShowAdvancedAction: TTntAction
      AutoCheck = True
      Caption = 'Show Advanced Controls'
      Checked = True
      OnExecute = ShowAdvancedActionExecute
    end
    object ApplyChangesAction: TTntAction
      Caption = 'Apply Changes'
      Enabled = False
      OnExecute = ApplyChangesActionExecute
      OnUpdate = ApplyChangesActionUpdate
    end
    object DiscardChangesAction: TTntAction
      Caption = 'Discard Changes'
      Enabled = False
      OnExecute = DiscardChangesActionExecute
      OnUpdate = ApplyChangesActionUpdate
    end
    object CloseAction: TTntAction
      Caption = 'Close'
      OnExecute = CloseActionExecute
    end
    object CopyToClipboardAction: TTntAction
      Category = 'Edit'
      Caption = 'Copy selected columns to clipboard'
      ShortCut = 16451
      OnExecute = CopyToClipboardActionExecute
      OnUpdate = DeleteSelectedColumnsActionUpdate
    end
    object PasteFromClipboardAction: TTntAction
      Category = 'Edit'
      Caption = 'Paste column definition from clipboard'
      ShortCut = 16470
      OnExecute = PasteFromClipboardActionExecute
      OnUpdate = PasteFromClipboardActionUpdate
    end
    object SelectAllAction: TTntAction
      Category = 'Edit'
      Caption = 'Select all columns'
      ShortCut = 16449
      OnExecute = SelectAllActionExecute
    end
  end
  object ColumnPopupMenu: TTntPopupMenu
    Left = 35
    Top = 213
    object SelectAllColumnsItem: TTntMenuItem
      Action = SelectAllAction
    end
    object CopyToClipboardItem: TTntMenuItem
      Action = CopyToClipboardAction
    end
    object PastFromClipboardItem: TTntMenuItem
      Action = PasteFromClipboardAction
    end
    object DeleteColumnsItem: TTntMenuItem
      Action = DeleteSelectedColumnsAction
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object ShowAdvancedItem: TTntMenuItem
      Action = ShowAdvancedAction
      AutoCheck = True
    end
  end
  object UCESQLHighlighter: TUCESQLHighlighter
    DefaultFilter = 'SQL script files (*.sql)|*.sql'
    CommentAttributes.Background = clDefault
    CommentAttributes.Foreground = clGray
    CommentAttributes.Style = [fsItalic]
    EmbeddedCommandAttributes.Background = 15790320
    EmbeddedCommandAttributes.Foreground = clNavy
    EmbeddedCommandAttributes.Style = []
    IdentifierAttributes.Background = clDefault
    IdentifierAttributes.Foreground = clWindowText
    IdentifierAttributes.Style = []
    KeyAttributes.Background = clDefault
    KeyAttributes.Foreground = clBlue
    KeyAttributes.Style = [fsBold]
    NumberAttributes.Background = clDefault
    NumberAttributes.Foreground = clFuchsia
    NumberAttributes.Style = []
    QuotedIDAttributes.Background = clWindow
    QuotedIDAttributes.Foreground = clWindowText
    QuotedIDAttributes.Style = []
    SpaceAttributes.Background = clDefault
    SpaceAttributes.Foreground = clWindowText
    SpaceAttributes.Style = []
    StringAttributes.Background = clDefault
    StringAttributes.Foreground = 33023
    StringAttributes.Style = []
    SymbolAttributes.Background = clDefault
    SymbolAttributes.Foreground = clWindowText
    SymbolAttributes.Style = []
    SystemVariableAttributes.Background = clDefault
    SystemVariableAttributes.Foreground = clGray
    SystemVariableAttributes.Style = [fsBold]
    UserVariableAttributes.Background = clDefault
    UserVariableAttributes.Foreground = 12615808
    UserVariableAttributes.Style = [fsBold]
    Left = 34
    Top = 180
  end
end
