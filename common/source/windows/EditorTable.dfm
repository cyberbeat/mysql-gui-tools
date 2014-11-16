object EditorTableForm: TEditorTableForm
  Left = 328
  Top = 177
  ActiveControl = CloseBtn
  Caption = 'MySQL Table Editor'
  ClientHeight = 581
  ClientWidth = 701
  Color = clBtnFace
  Constraints.MinHeight = 448
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TableEditorPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 581
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object HeaderPnl: TTntPanel
      Left = 0
      Top = 60
      Width = 701
      Height = 35
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        701
        35)
      object TableNameLbl: TTntLabel
        Left = 20
        Top = 4
        Width = 61
        Height = 13
        Alignment = taRightJustify
        Caption = 'Table Name:'
      end
      object TntLabel2: TTntLabel
        Left = 261
        Top = 4
        Width = 49
        Height = 13
        Alignment = taRightJustify
        Caption = 'Database:'
      end
      object TntLabel3: TTntLabel
        Left = 469
        Top = 4
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Comment:'
      end
      object TableNameEd: TTntEdit
        Left = 86
        Top = 0
        Width = 151
        Height = 21
        TabOrder = 0
        OnChange = ValueChanged
        OnKeyDown = TableNameEdKeyDown
      end
      object DatabaseCBox: TTntComboBox
        Left = 314
        Top = 0
        Width = 129
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ValueChanged
      end
      object CommentEd: TTntEdit
        Left = 520
        Top = 0
        Width = 165
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = ValueChanged
      end
    end
    object LeftPnl: TTntPanel
      Left = 0
      Top = 95
      Width = 13
      Height = 435
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
    end
    object RightPnl: TTntPanel
      Left = 688
      Top = 95
      Width = 13
      Height = 435
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 3
    end
    object BottomPnl: TTntPanel
      Left = 0
      Top = 530
      Width = 701
      Height = 51
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 6
      DesignSize = (
        701
        51)
      object DiscardChangesBtn: TTntButton
        Left = 442
        Top = 13
        Width = 120
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Discard Changes'
        Enabled = False
        TabOrder = 2
        OnClick = DiscardChangesBtnClick
      end
      object ApplyChangesBtn: TTntButton
        Left = 316
        Top = 13
        Width = 120
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Apply Changes'
        Enabled = False
        TabOrder = 1
        OnClick = ApplyChangesBtnClick
      end
      object CloseBtn: TTntButton
        Left = 568
        Top = 13
        Width = 120
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Close'
        TabOrder = 3
        OnClick = CloseBtnClick
      end
      object DetailsBtn: TTntButton
        Left = 13
        Top = 13
        Width = 120
        Height = 25
        Caption = '&Details >>'
        TabOrder = 0
        Visible = False
        OnClick = DetailsBtnClick
      end
    end
    object GirdOptionPnl: TTntPanel
      Left = 13
      Top = 95
      Width = 675
      Height = 435
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 5
      object MainPageControl: TTntPageControl
        Left = 0
        Top = 0
        Width = 675
        Height = 435
        ActivePage = ColumnTabSheet
        Align = alClient
        TabOrder = 0
        TabStop = False
        OnChange = MainPageControlChange
        OnChanging = MainPageControlChanging
        object ColumnTabSheet: TTabSheet
          BorderWidth = 10
          Caption = 'Columns and Indices'
          object ColumnOptionSplitter: TTntSplitter
            Left = 0
            Top = 193
            Width = 647
            Height = 11
            Cursor = crVSplit
            Align = alBottom
            Color = clBtnFace
            ParentColor = False
            ExplicitWidth = 646
          end
          object ColumnVST: TVirtualStringTree
            Left = 0
            Top = 0
            Width = 647
            Height = 193
            Align = alClient
            ButtonStyle = bsTriangle
            DefaultNodeHeight = 19
            DragMode = dmAutomatic
            DragOperations = []
            DragType = dtVCL
            Header.AutoSizeIndex = 0
            Header.Font.Charset = ANSI_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'MS Sans Serif'
            Header.Font.Style = []
            Header.Images = ColumnHeaderImgList
            Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoVisible, hoAutoSpring]
            Images = ColumnImgList
            LineStyle = lsSolid
            PopupMenu = ColumnPopupMenu
            TabOrder = 0
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
            TreeOptions.PaintOptions = [toShowHorzGridLines, toShowTreeLines, toThemeAware, toUseBlendedSelection]
            TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect]
            TreeOptions.StringOptions = [toAutoAcceptEditChange]
            OnAfterCellPaint = ColumnVSTAfterCellPaint
            OnCreateEditor = ColumnVSTCreateEditor
            OnDblClick = ColumnVSTDblClick
            OnEditing = ColumnVSTEditing
            OnFocusChanged = ColumnVSTFocusChanged
            OnGetText = ColumnVSTGetText
            OnGetImageIndex = ColumnVSTGetImageIndex
            OnKeyDown = ColumnVSTKeyDown
            OnMouseDown = ColumnVSTMouseDown
            OnNewText = ColumnVSTNewText
            Columns = <
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                Position = 0
                Width = 112
                WideText = 'Column Name'
              end
              item
                Position = 1
                Width = 110
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
                Width = 67
                WideText = 'Comment'
              end>
            WideDefaultText = ''
          end
          object ColumnsPageControl: TTntPageControl
            Left = 0
            Top = 204
            Width = 647
            Height = 183
            ActivePage = TntTabSheet1
            Align = alBottom
            TabOrder = 1
            OnChange = ColumnsPageControlChange
            object IndexTabsheet: TTntTabSheet
              Caption = 'Indices'
              DesignSize = (
                639
                155)
              object AddIndexBtn: TTntSpeedButton
                Left = 118
                Top = 130
                Width = 19
                Height = 19
                Flat = True
                Glyph.Data = {
                  F6000000424DF600000000000000360000002800000008000000080000000100
                  180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                  FFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
                  00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                  FFFF000000000000000000000000000000000000000000000000000000000000
                  000000000000000000000000000000000000FFFFFFFFFFFFFFFFFF0000000000
                  00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF}
                OnClick = AddIndexBtnClick
              end
              object DeleteIndexBtn: TTntSpeedButton
                Left = 138
                Top = 130
                Width = 19
                Height = 19
                Flat = True
                Glyph.Data = {
                  F6000000424DF600000000000000360000002800000008000000080000000100
                  180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFF000000000000000000000000000000000000000000000000000000000000
                  000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
                OnClick = DeleteIndexBtnClick
              end
              object IndexVST: TVirtualStringTree
                Left = 10
                Top = 12
                Width = 147
                Height = 117
                ButtonStyle = bsTriangle
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'MS Sans Serif'
                Header.Font.Style = []
                Header.MainColumn = -1
                Header.Options = [hoColumnResize, hoDrag]
                Images = ColumnImgList
                TabOrder = 1
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
                TreeOptions.StringOptions = [toAutoAcceptEditChange]
                OnAfterCellPaint = IndexVSTAfterCellPaint
                OnFocusChanged = IndexVSTFocusChanged
                OnGetText = IndexVSTGetText
                OnGetImageIndex = IndexVSTGetImageIndex
                Columns = <>
                WideDefaultText = ''
              end
              object TntGroupBox1: TTntGroupBox
                Left = 174
                Top = 7
                Width = 453
                Height = 138
                Anchors = [akLeft, akTop, akRight, akBottom]
                Caption = 'Index Settings'
                TabOrder = 0
                DesignSize = (
                  453
                  138)
                object IndexNameLbl: TTntLabel
                  Left = 14
                  Top = 23
                  Width = 60
                  Height = 13
                  Caption = 'Index Name:'
                end
                object IndexKindLbl: TTntLabel
                  Left = 14
                  Top = 56
                  Width = 53
                  Height = 13
                  Caption = 'Index Kind:'
                end
                object TntLabel12: TTntLabel
                  Left = 242
                  Top = 17
                  Width = 69
                  Height = 13
                  Caption = 'Index Columns'
                  Transparent = True
                end
                object IndexTypeLbl: TTntLabel
                  Left = 14
                  Top = 91
                  Width = 56
                  Height = 13
                  Caption = 'Index Type:'
                end
                object AddIndexColumnBtn: TTntSpeedButton
                  Left = 427
                  Top = 51
                  Width = 19
                  Height = 19
                  Anchors = [akTop, akRight]
                  Flat = True
                  Glyph.Data = {
                    F6000000424DF600000000000000360000002800000008000000080000000100
                    180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                    FFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
                    00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                    FFFF000000000000000000000000000000000000000000000000000000000000
                    000000000000000000000000000000000000FFFFFFFFFFFFFFFFFF0000000000
                    00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                    FFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF}
                  OnClick = AddIndexColumnBtnClick
                  ExplicitLeft = 426
                end
                object DeleteIndexColumnBtn: TTntSpeedButton
                  Left = 427
                  Top = 71
                  Width = 19
                  Height = 19
                  Anchors = [akTop, akRight]
                  Flat = True
                  Glyph.Data = {
                    F6000000424DF600000000000000360000002800000008000000080000000100
                    180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    FFFF000000000000000000000000000000000000000000000000000000000000
                    000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
                  OnClick = DeleteIndexColumnBtnClick
                  ExplicitLeft = 426
                end
                object AdvIndexColumnBtn: TTntSpeedButton
                  Left = 427
                  Top = 31
                  Width = 19
                  Height = 19
                  Anchors = [akTop, akRight]
                  Flat = True
                  Glyph.Data = {
                    DE000000424DDE00000000000000360000002800000007000000070000000100
                    180000000000A8000000120B0000120B00000000000000000000FFFFFFFFFFFF
                    000000FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF000000000000FFFF
                    FFFFFFFFFFFFFF000000FFFFFFFFFFFF000000000000000000FFFFFFFFFFFF00
                    0000FFFFFFFFFFFF000000000000000000000000FFFFFF000000FFFFFFFFFFFF
                    000000000000000000FFFFFFFFFFFF000000FFFFFFFFFFFF000000000000FFFF
                    FFFFFFFFFFFFFF000000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF00
                    0000}
                  OnClick = AdvIndexColumnBtnClick
                  ExplicitLeft = 426
                end
                object TntLabel9: TTntLabel
                  Left = 341
                  Top = 17
                  Width = 84
                  Height = 13
                  Alignment = taRightJustify
                  Anchors = [akTop, akRight]
                  Caption = '(Use Drag'#39'n'#39'Drop)'
                  Font.Charset = DEFAULT_CHARSET
                  Font.Color = clGray
                  Font.Height = -11
                  Font.Name = 'MS Sans Serif'
                  Font.Style = []
                  ParentFont = False
                  ExplicitLeft = 340
                end
                object IndexNameEd: TTntEdit
                  Left = 88
                  Top = 19
                  Width = 135
                  Height = 21
                  TabOrder = 0
                  OnChange = IndexNameEdChange
                end
                object IndexKindCBox: TTntComboBox
                  Left = 88
                  Top = 53
                  Width = 135
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 1
                  OnChange = IndexKindCBoxChange
                end
                object IndexColumnsListBox: TTntListBox
                  Left = 242
                  Top = 32
                  Width = 184
                  Height = 91
                  Anchors = [akLeft, akTop, akRight, akBottom]
                  ItemHeight = 13
                  MultiSelect = True
                  TabOrder = 2
                  OnDblClick = AdvIndexColumnBtnClick
                  OnDragDrop = IndexColumnsListBoxDragDrop
                  OnDragOver = IndexColumnsListBoxDragOver
                  OnKeyDown = IndexColumnsListBoxKeyDown
                  OnMouseDown = IndexColumnsListBoxMouseDown
                end
                object IndexTypeCBox: TTntComboBox
                  Left = 88
                  Top = 88
                  Width = 135
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 3
                  OnChange = IndexTypeCBoxChange
                end
              end
            end
            object FKTabsheet: TTntTabSheet
              Caption = 'Foreign Keys'
              DesignSize = (
                639
                155)
              object AddFKBtn: TTntSpeedButton
                Left = 118
                Top = 130
                Width = 19
                Height = 19
                Flat = True
                Glyph.Data = {
                  F6000000424DF600000000000000360000002800000008000000080000000100
                  180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                  FFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
                  00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                  FFFF000000000000000000000000000000000000000000000000000000000000
                  000000000000000000000000000000000000FFFFFFFFFFFFFFFFFF0000000000
                  00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF}
                OnClick = AddFKBtnClick
              end
              object DropFKBtn: TTntSpeedButton
                Left = 138
                Top = 130
                Width = 19
                Height = 19
                Flat = True
                Glyph.Data = {
                  F6000000424DF600000000000000360000002800000008000000080000000100
                  180000000000C0000000120B0000120B00000000000000000000FFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFF000000000000000000000000000000000000000000000000000000000000
                  000000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                  FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
                OnClick = DropFKBtnClick
              end
              object TntGroupBox2: TTntGroupBox
                Left = 174
                Top = 7
                Width = 453
                Height = 138
                Anchors = [akLeft, akTop, akRight, akBottom]
                Caption = 'Foreign Key Settings'
                TabOrder = 0
                DesignSize = (
                  453
                  138)
                object TntLabel14: TTntLabel
                  Left = 14
                  Top = 23
                  Width = 52
                  Height = 13
                  Caption = 'Key Name:'
                end
                object TntLabel15: TTntLabel
                  Left = 14
                  Top = 56
                  Width = 51
                  Height = 13
                  Caption = 'On Delete:'
                end
                object TntLabel16: TTntLabel
                  Left = 14
                  Top = 91
                  Width = 55
                  Height = 13
                  Caption = 'On Update:'
                end
                object TntLabel17: TTntLabel
                  Left = 223
                  Top = 23
                  Width = 53
                  Height = 13
                  Caption = 'Ref. Table:'
                end
                object FKNameEd: TTntEdit
                  Left = 88
                  Top = 19
                  Width = 119
                  Height = 21
                  TabOrder = 0
                  OnChange = FKNameEdChange
                end
                object FKOnDeleteComboBox: TTntComboBox
                  Left = 88
                  Top = 53
                  Width = 119
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 1
                  OnChange = FKOnDeleteComboBoxChange
                end
                object FKColsVST: TVirtualStringTree
                  Left = 224
                  Top = 53
                  Width = 212
                  Height = 71
                  Anchors = [akLeft, akTop, akRight, akBottom]
                  ButtonStyle = bsTriangle
                  DragOperations = []
                  DragType = dtVCL
                  Header.AutoSizeIndex = 0
                  Header.Font.Charset = DEFAULT_CHARSET
                  Header.Font.Color = clWindowText
                  Header.Font.Height = -11
                  Header.Font.Name = 'MS Sans Serif'
                  Header.Font.Style = []
                  Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoVisible, hoAutoSpring]
                  PopupMenu = FKColumnPopupMenu
                  TabOrder = 2
                  TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
                  TreeOptions.PaintOptions = [toShowButtons, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
                  TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect]
                  OnCreateEditor = FKColsVSTCreateEditor
                  OnDblClick = FKColsVSTDblClick
                  OnDragOver = FKColsVSTDragOver
                  OnDragDrop = FKColsVSTDragDrop
                  OnGetText = FKColsVSTGetText
                  OnNewText = FKColsVSTNewText
                  Columns = <
                    item
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                      Position = 0
                      Width = 108
                      WideText = 'Column'
                    end
                    item
                      Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                      Position = 1
                      Width = 100
                      WideText = 'Reference Column'
                    end>
                  WideDefaultText = ''
                end
                object FKOnUpdateComboBox: TTntComboBox
                  Left = 88
                  Top = 88
                  Width = 119
                  Height = 21
                  Style = csDropDownList
                  ItemHeight = 13
                  TabOrder = 3
                  OnChange = FKOnDeleteComboBoxChange
                end
                object FKDestTablesComboBox: TTntComboBox
                  Left = 284
                  Top = 19
                  Width = 152
                  Height = 21
                  Anchors = [akLeft, akTop, akRight]
                  ItemHeight = 13
                  TabOrder = 4
                  OnChange = FKDestTablesComboBoxChange
                  OnCloseUp = FKDestTablesComboBoxCloseUp
                end
              end
              object FKVST: TVirtualStringTree
                Left = 10
                Top = 12
                Width = 147
                Height = 117
                ButtonStyle = bsTriangle
                Header.AutoSizeIndex = 0
                Header.Font.Charset = DEFAULT_CHARSET
                Header.Font.Color = clWindowText
                Header.Font.Height = -11
                Header.Font.Name = 'MS Sans Serif'
                Header.Font.Style = []
                Header.MainColumn = -1
                Header.Options = [hoColumnResize, hoDrag]
                Images = ColumnImgList
                TabOrder = 1
                TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
                TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
                TreeOptions.StringOptions = [toAutoAcceptEditChange]
                OnAfterCellPaint = IndexVSTAfterCellPaint
                OnFocusChanged = IndexVSTFocusChanged
                OnGetText = IndexVSTGetText
                OnGetImageIndex = IndexVSTGetImageIndex
                Columns = <>
                WideDefaultText = ''
              end
            end
            object TntTabSheet1: TTntTabSheet
              Caption = 'Column Details'
              DesignSize = (
                639
                155)
              object TntLabel4: TTntLabel
                Left = 10
                Top = 16
                Width = 31
                Height = 13
                Caption = 'Name:'
              end
              object TntLabel5: TTntLabel
                Left = 222
                Top = 16
                Width = 46
                Height = 13
                Caption = 'Datatype:'
              end
              object TntLabel6: TTntLabel
                Left = 222
                Top = 51
                Width = 28
                Height = 13
                Caption = 'Flags:'
              end
              object TntLabel7: TTntLabel
                Left = 438
                Top = 16
                Width = 67
                Height = 13
                Caption = 'Default Value:'
              end
              object TntLabel8: TTntLabel
                Left = 222
                Top = 115
                Width = 47
                Height = 13
                Caption = 'Comment:'
              end
              object TntLabel36: TTntLabel
                Left = 438
                Top = 51
                Width = 77
                Height = 13
                Caption = 'Column Charset:'
              end
              object TntLabel37: TTntLabel
                Left = 438
                Top = 81
                Width = 73
                Height = 13
                Caption = 'Column Collate:'
              end
              object ColumnNameEd: TTntEdit
                Left = 52
                Top = 12
                Width = 145
                Height = 21
                TabOrder = 0
                OnChange = ColumnNameEdChange
              end
              object ColumnDatatypeEd: TTntEdit
                Left = 282
                Top = 12
                Width = 137
                Height = 21
                TabOrder = 1
                OnChange = ColumnNameEdChange
              end
              object ColumnFlagsCheckListBox: TTntCheckListBox
                Left = 282
                Top = 47
                Width = 137
                Height = 51
                OnClickCheck = ColumnNameEdChange
                ItemHeight = 13
                TabOrder = 2
              end
              object ColumnCommentMemo: TTntMemo
                Left = 282
                Top = 112
                Width = 345
                Height = 33
                Anchors = [akLeft, akTop, akRight, akBottom]
                MaxLength = 255
                TabOrder = 3
                OnChange = ColumnNameEdChange
              end
              object ColumnDefaultValueEd: TTntEdit
                Left = 524
                Top = 12
                Width = 71
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                TabOrder = 4
                OnChange = ColumnNameEdChange
              end
              object ColumnCharsetCBox: TTntComboBox
                Left = 524
                Top = 47
                Width = 103
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                DropDownCount = 16
                ItemHeight = 13
                TabOrder = 5
                OnCloseUp = ColumnCharsetCBoxCloseUp
              end
              object ColumnCollateCBox: TTntComboBox
                Left = 524
                Top = 77
                Width = 103
                Height = 21
                Style = csDropDownList
                Anchors = [akLeft, akTop, akRight]
                ItemHeight = 13
                TabOrder = 6
                OnCloseUp = ColumnNameEdChange
              end
              object TntGroupBox5: TTntGroupBox
                Left = 52
                Top = 46
                Width = 145
                Height = 100
                Caption = 'Column Options'
                TabOrder = 7
                object ColumnPKCBox: TTntCheckBox
                  Left = 16
                  Top = 22
                  Width = 126
                  Height = 17
                  Caption = 'Primary Key'
                  TabOrder = 0
                  OnClick = ColumnNameEdChange
                end
                object ColumnNotNullCBox: TTntCheckBox
                  Left = 16
                  Top = 47
                  Width = 126
                  Height = 17
                  Caption = 'Not Null'
                  TabOrder = 1
                  OnClick = ColumnNameEdChange
                end
                object ColumnAutoIncCBox: TTntCheckBox
                  Left = 16
                  Top = 70
                  Width = 126
                  Height = 17
                  Caption = 'Auto Increment'
                  TabOrder = 2
                  OnClick = ColumnNameEdChange
                end
              end
              object SetDefValNullBtn: TTntBitBtn
                Left = 598
                Top = 11
                Width = 30
                Height = 23
                Anchors = [akTop, akRight]
                TabOrder = 8
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
            end
          end
        end
        object TabSheet2: TTabSheet
          Caption = 'Table Options'
          ImageIndex = 1
          object TableOptionScrollBox: TTntScrollBox
            Left = 0
            Top = 0
            Width = 667
            Height = 407
            HorzScrollBar.Smooth = True
            HorzScrollBar.Tracking = True
            VertScrollBar.Smooth = True
            VertScrollBar.Tracking = True
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            TabOrder = 0
            object ScrollSpacerTableOptionsShape: TTntShape
              Left = 11
              Top = 396
              Width = 65
              Height = 5
              Brush.Style = bsClear
              Pen.Style = psClear
            end
            object TntGroupBox6: TTntGroupBox
              Left = 10
              Top = 154
              Width = 631
              Height = 97
              Caption = 'Character Set'
              TabOrder = 0
              object TableCharsetCaptionLbl: TTntLabel
                Left = 14
                Top = 27
                Width = 39
                Height = 13
                Caption = 'Charset:'
              end
              object TableCollationCaptionLbl: TTntLabel
                Left = 14
                Top = 63
                Width = 43
                Height = 13
                Caption = 'Collation:'
              end
              object TableCharsetDescLbl: TTntLabel
                Left = 230
                Top = 25
                Width = 390
                Height = 31
                AutoSize = False
                Caption = 
                  'The default character set that is used for the table. Starting f' +
                  'rom MySQL 4.1 you can specify an individual character set for ea' +
                  'ch column.'
                WordWrap = True
              end
              object TableCollationDescLbl: TTntLabel
                Left = 230
                Top = 62
                Width = 390
                Height = 18
                AutoSize = False
                Caption = 'Collation method that is used to compare text and sort columns.'
                WordWrap = True
              end
              object TableCharsetComboBox: TTntComboBox
                Left = 96
                Top = 23
                Width = 115
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 0
                OnChange = TableCharsetComboBoxChange
              end
              object TableCollationComboBox: TTntComboBox
                Left = 96
                Top = 59
                Width = 115
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 1
                OnChange = ValueChanged
              end
            end
            object StorageEngineGBox: TTntGroupBox
              Left = 10
              Top = 7
              Width = 631
              Height = 136
              Caption = 'Storage Engine'
              TabOrder = 1
              object TableEngineDescLbl: TTntLabel
                Left = 96
                Top = 65
                Width = 483
                Height = 52
                AutoSize = False
                Caption = 
                  'Very fast, disk based storage engine without support for transac' +
                  'tions. Offers fulltext search, packed keys, and is the default s' +
                  'torage engine.'
                WordWrap = True
              end
              object TntLabel11: TTntLabel
                Left = 14
                Top = 36
                Width = 66
                Height = 13
                Caption = 'Table Engine:'
              end
              object TableEngineLU: TTntComboBox
                Left = 96
                Top = 32
                Width = 247
                Height = 22
                Style = csOwnerDrawFixed
                DropDownCount = 16
                ItemHeight = 16
                TabOrder = 0
                OnChange = TableEngineLUChange
                OnDrawItem = TableEngineLUDrawItem
              end
            end
          end
        end
        object AdvTableOptionsSheet: TTabSheet
          Caption = 'Advanced Options'
          ImageIndex = -1
          object AdvancedOptionsScrollBox: TTntScrollBox
            Left = 0
            Top = 0
            Width = 667
            Height = 407
            HorzScrollBar.Smooth = True
            HorzScrollBar.Tracking = True
            VertScrollBar.Smooth = True
            VertScrollBar.Tracking = True
            Align = alClient
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
            object StorageOptionsGBox: TTntGroupBox
              Left = 10
              Top = 449
              Width = 631
              Height = 137
              Caption = 'Storage Options'
              TabOrder = 2
              object TntLabel27: TTntLabel
                Left = 14
                Top = 25
                Width = 71
                Height = 13
                Caption = 'Data Directory:'
              end
              object TntLabel28: TTntLabel
                Left = 14
                Top = 79
                Width = 74
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
                OnChange = ValueChanged
              end
              object IndexDirectoryEd: TTntEdit
                Left = 96
                Top = 75
                Width = 255
                Height = 21
                TabOrder = 1
                OnChange = ValueChanged
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
                Width = 68
                Height = 13
                Caption = 'Avg Row Len:'
              end
              object TntLabel24: TTntLabel
                Left = 14
                Top = 170
                Width = 50
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
                Width = 60
                Height = 13
                Caption = 'Row Format:'
              end
              object TntLabel39: TTntLabel
                Left = 230
                Top = 16
                Width = 390
                Height = 61
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
                OnChange = ValueChanged
              end
              object MinRowsEd: TTntEdit
                Left = 110
                Top = 166
                Width = 105
                Height = 21
                TabOrder = 3
                OnChange = ValueChanged
              end
              object MaxRowsEd: TTntEdit
                Left = 110
                Top = 200
                Width = 105
                Height = 21
                TabOrder = 4
                OnChange = ValueChanged
              end
              object UseChecksumCBox: TTntCheckBox
                Left = 14
                Top = 74
                Width = 191
                Height = 17
                Caption = 'Use Checksum'
                TabOrder = 1
                OnClick = ValueChanged
              end
              object RowFormatComboBox: TTntComboBox
                Left = 110
                Top = 22
                Width = 105
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 0
                OnChange = ValueChanged
              end
            end
            object RaidOptionsGBox: TTntGroupBox
              Left = 10
              Top = 709
              Width = 631
              Height = 180
              Caption = 'Table RAID Settings'
              TabOrder = 4
              object TntLabel29: TTntLabel
                Left = 14
                Top = 23
                Width = 56
                Height = 13
                Caption = 'RAID Type:'
              end
              object NumberOfChunksCaptionLbl: TTntLabel
                Left = 14
                Top = 91
                Width = 91
                Height = 13
                Caption = 'Number of Chunks:'
              end
              object ChunkSizeCaptionEd: TTntLabel
                Left = 14
                Top = 143
                Width = 52
                Height = 13
                Caption = 'Chunksize:'
              end
              object ChunkSizeUnitLbl: TTntLabel
                Left = 192
                Top = 144
                Width = 13
                Height = 13
                Caption = 'kB'
              end
              object TntLabel57: TTntLabel
                Left = 230
                Top = 12
                Width = 390
                Height = 72
                AutoSize = False
                Caption = 
                  'The RAID functionality can be used to exceed the 2GB/4GB limit f' +
                  'or MyISAM data files on operating systems that do not support bi' +
                  'g files (unrecommended if they do). When the RAID type is set to' +
                  ' STRIPED the table data file will be split into a number for chu' +
                  'nks.'
                WordWrap = True
              end
              object NumberOfChunksDescLbl: TTntLabel
                Left = 230
                Top = 91
                Width = 390
                Height = 44
                AutoSize = False
                Caption = 
                  'Number of chunks to create, the maximum value is 255. For each c' +
                  'hunk there will be a subdirectory created named '#39'00'#39', '#39'01'#39', ... ' +
                  'and a '#39'tbl_name.MYD'#39' file will be created in each directory.'
                WordWrap = True
              end
              object ChunkSizeDescLbl: TTntLabel
                Left = 230
                Top = 145
                Width = 390
                Height = 18
                AutoSize = False
                Caption = 
                  'Size of chunk that will be written to first file, to the next fi' +
                  'le, and so on.'
                WordWrap = True
              end
              object RaidTypeComboBox: TTntComboBox
                Left = 116
                Top = 20
                Width = 99
                Height = 21
                Style = csDropDownList
                ItemHeight = 13
                TabOrder = 0
                OnChange = ValueChanged
              end
              object NumberOfChunksEd: TTntEdit
                Left = 116
                Top = 88
                Width = 55
                Height = 21
                TabOrder = 1
                Text = '2'
                OnChange = ValueChanged
              end
              object ChunkSizeEd: TTntEdit
                Left = 116
                Top = 140
                Width = 55
                Height = 21
                TabOrder = 3
                Text = '64'
                OnChange = ValueChanged
              end
              object ChunkSizeUpDown: TTntUpDown
                Left = 171
                Top = 140
                Width = 16
                Height = 21
                Associate = ChunkSizeEd
                Position = 64
                TabOrder = 4
              end
              object NumberOfChunksUpDown: TTntUpDown
                Left = 171
                Top = 88
                Width = 16
                Height = 21
                Associate = NumberOfChunksEd
                Position = 2
                TabOrder = 2
              end
            end
            object VariousGBox: TTntGroupBox
              Left = 10
              Top = 7
              Width = 631
              Height = 185
              Caption = 'Various'
              TabOrder = 0
              object TntLabel19: TTntLabel
                Left = 14
                Top = 26
                Width = 54
                Height = 13
                Caption = 'Pack Keys:'
              end
              object TntLabel20: TTntLabel
                Left = 14
                Top = 115
                Width = 75
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
                Top = 13
                Width = 390
                Height = 54
                AutoSize = False
                Caption = 
                  'Use this option to generate smaller indices. This usually makes ' +
                  'updates slower and reads faster. Setting it to DEFAULT tells the' +
                  ' storage engine to only pack long CHAR/VARCHAR columns.'
                WordWrap = True
              end
              object TntLabel52: TTntLabel
                Left = 230
                Top = 71
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
                Caption = 
                  'The initial AUTO_INCREMENT value for the table, only for MyISAM ' +
                  'and InnoDB.'
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
                ItemHeight = 13
                TabOrder = 0
                OnChange = ValueChanged
              end
              object AutoIncEd: TTntEdit
                Left = 110
                Top = 111
                Width = 105
                Height = 21
                TabOrder = 2
                OnChange = ValueChanged
              end
              object TablePasswordEd: TTntEdit
                Left = 110
                Top = 71
                Width = 105
                Height = 21
                TabOrder = 1
                OnChange = ValueChanged
              end
              object DelayKeyUpdatesCBox: TTntCheckBox
                Left = 14
                Top = 143
                Width = 201
                Height = 17
                Caption = 'Delay Key Updates'
                TabOrder = 3
                OnClick = ValueChanged
              end
            end
            object MergeOptionGBox: TTntGroupBox
              Left = 10
              Top = 596
              Width = 631
              Height = 103
              Caption = 'Merge Table Options'
              TabOrder = 3
              object MergeInsertMethodCapionLbl: TTntLabel
                Left = 14
                Top = 68
                Width = 68
                Height = 13
                Caption = 'Insert Method:'
              end
              object UnionTablesCaptionLbl: TTntLabel
                Left = 14
                Top = 24
                Width = 66
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
                ItemHeight = 13
                TabOrder = 1
                OnChange = ValueChanged
              end
              object UnionTablesEd: TTntEdit
                Left = 96
                Top = 20
                Width = 255
                Height = 21
                TabOrder = 0
                OnChange = ValueChanged
              end
            end
            object TntGroupBox3: TTntGroupBox
              Left = 10
              Top = 898
              Width = 631
              Height = 61
              Caption = 'Federated'
              TabOrder = 5
              object TntLabel18: TTntLabel
                Left = 14
                Top = 24
                Width = 57
                Height = 13
                Caption = 'Connection:'
              end
              object TntLabel23: TTntLabel
                Left = 364
                Top = 26
                Width = 253
                Height = 31
                AutoSize = False
                Caption = 'The connection string for federated tables.'
                WordWrap = True
              end
              object FederatedConnectionEd: TTntEdit
                Left = 96
                Top = 20
                Width = 255
                Height = 21
                TabOrder = 0
                OnChange = ValueChanged
              end
            end
          end
        end
        object StdInsertsTabSheet: TTabSheet
          Caption = 'Standard Inserts'
          ImageIndex = 2
          DesignSize = (
            667
            407)
          object TntLabel40: TTntLabel
            Left = 10
            Top = 6
            Width = 55
            Height = 13
            Caption = 'SQL Inserts'
          end
          object TntMemo3: TTntMemo
            Left = 10
            Top = 20
            Width = 642
            Height = 376
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
          end
        end
        object DescTabSheet: TTabSheet
          Caption = 'Description'
          ImageIndex = 3
          DesignSize = (
            667
            407)
          object TntLabel41: TTntLabel
            Left = 10
            Top = 6
            Width = 83
            Height = 13
            Caption = 'Table Description'
          end
          object TntMemo2: TTntMemo
            Left = 10
            Top = 20
            Width = 642
            Height = 376
            Anchors = [akLeft, akTop, akRight, akBottom]
            TabOrder = 0
          end
        end
      end
    end
    object TopPnl: TTntPanel
      Left = 0
      Top = 0
      Width = 701
      Height = 13
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 4
    end
    object DockedHeaderPnl: TTntPanel
      Left = 0
      Top = 13
      Width = 701
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      Visible = False
      DesignSize = (
        701
        47)
      object HeaderImg: TTntImage
        Left = 6
        Top = 0
        Width = 24
        Height = 24
      end
      object TntLabel1: TTntLabel
        Left = 34
        Top = 0
        Width = 70
        Height = 13
        Caption = 'Table Editor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object TntLabel10: TTntLabel
        Left = 34
        Top = 14
        Width = 181
        Height = 13
        Caption = 'Use this editor to create or alter tables.'
      end
      object Bevel1: TTntBevel
        Left = 8
        Top = 31
        Width = 682
        Height = 3
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
        ExplicitWidth = 681
      end
    end
  end
  object ColumnImgList: TImageList
    Height = 12
    Width = 12
    Left = 639
    Top = 109
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
    Left = 639
    Top = 143
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
  object IndexColumnPopupMenu: TTntPopupMenu
    Left = 641
    Top = 282
    object SetIndexColumnLengthMI: TTntMenuItem
      Caption = 'Set Index Column Length'
      OnClick = SetIndexColumnLengthMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object ColumnPopupMenu: TTntPopupMenu
    Left = 167
    Top = 130
    object DeleteTableColumnsMI: TTntMenuItem
      Caption = 'Delete Table Columns'
      OnClick = DeleteTableColumnsMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object FKColumnPopupMenu: TTntPopupMenu
    Left = 641
    Top = 313
    object RemoveFKColumnMI: TTntMenuItem
      Caption = 'Remove Column'
      OnClick = RemoveFKColumnMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
end
