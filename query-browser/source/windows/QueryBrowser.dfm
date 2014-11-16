object QueryBrowserForm: TQueryBrowserForm
  Left = 659
  Top = 262
  Caption = 'QueryBrowser'
  ClientHeight = 636
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object DockPnl: TTntPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 846
    Height = 630
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object SidebarSplitter: TTntSplitter
      Left = 619
      Top = 183
      Width = 7
      Height = 447
      Align = alRight
      MinSize = 220
      ExplicitLeft = 621
      ExplicitHeight = 310
    end
    object ToolbarPnl: TTntPanel
      Left = 0
      Top = 0
      Width = 846
      Height = 178
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object QueryToolbarPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 846
        Height = 56
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          846
          56)
        object Bevel1: TTntBevel
          Left = 0
          Top = 54
          Width = 846
          Height = 2
          Align = alBottom
          Shape = bsTopLine
          ExplicitWidth = 852
        end
        object Shape1: TTntShape
          Left = 789
          Top = 0
          Width = 1
          Height = 55
          Anchors = [akTop, akRight]
          Brush.Style = bsClear
          Pen.Color = clBtnShadow
          ExplicitLeft = 795
        end
        object AnimPnl: TTntPanel
          Left = 790
          Top = 0
          Width = 56
          Height = 54
          Anchors = [akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 0
          object AnimStillImg: TTntImage
            Left = 0
            Top = 0
            Width = 56
            Height = 54
            Align = alClient
            OnClick = AnimStillImgClick
          end
          object BusyAnimate: TAnimate
            Left = 0
            Top = 0
            Width = 56
            Height = 54
            Align = alClient
            StopFrame = 74
            Visible = False
            OnStop = BusyAnimateStop
          end
        end
      end
      object AdvancedQueryToolbarPnl: TTntPanel
        Left = 0
        Top = 112
        Width = 846
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object AdvancedQueryToolbarSepBevel: TTntBevel
          Left = 0
          Top = 31
          Width = 846
          Height = 2
          Align = alBottom
          Shape = bsTopLine
          ExplicitWidth = 852
        end
      end
      object DebugToolbarPnl: TTntPanel
        Left = 0
        Top = 145
        Width = 846
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object DebugToolbarSepBevel: TTntBevel
          Left = 0
          Top = 31
          Width = 846
          Height = 2
          Align = alBottom
          Shape = bsTopLine
          ExplicitWidth = 852
        end
      end
      object ScriptToolbarPnl: TTntPanel
        Left = 0
        Top = 56
        Width = 846
        Height = 56
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        Visible = False
        DesignSize = (
          846
          56)
        object TntBevel1: TTntBevel
          Left = 0
          Top = 54
          Width = 846
          Height = 2
          Align = alBottom
          Shape = bsTopLine
          ExplicitWidth = 852
        end
        object TntShape1: TTntShape
          Left = 789
          Top = 0
          Width = 1
          Height = 55
          Anchors = [akTop, akRight]
          Brush.Style = bsClear
          Pen.Color = clBtnShadow
          ExplicitLeft = 795
        end
        object TntPanel2: TTntPanel
          Left = 790
          Top = 0
          Width = 56
          Height = 54
          Anchors = [akTop, akRight]
          BevelOuter = bvNone
          TabOrder = 0
          object ScriptAnimStillImg: TTntImage
            Left = 0
            Top = 0
            Width = 56
            Height = 54
            Align = alClient
            OnClick = AnimStillImgClick
          end
          object ScriptBusyAnimate: TAnimate
            Left = 0
            Top = 0
            Width = 56
            Height = 54
            Align = alClient
            StopFrame = 74
            Visible = False
            OnStop = BusyAnimateStop
          end
        end
      end
    end
    object ToolbarSepPnl: TTntPanel
      Left = 0
      Top = 178
      Width = 846
      Height = 5
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object ToolbarSepShape: TTntShape
        Left = 0
        Top = 0
        Width = 846
        Height = 5
        Align = alClient
        Brush.Color = clBtnFace
        Pen.Color = clBtnFace
        ExplicitWidth = 852
      end
    end
    object MainAreaPnl: TTntPanel
      Left = 0
      Top = 183
      Width = 619
      Height = 447
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object TabsPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 619
        Height = 447
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        inline MainTabHeaderFrame: TTabHeaderFrame
          Left = 0
          Top = 0
          Width = 619
          Height = 24
          Align = alTop
          Color = clWhite
          ParentColor = False
          TabOrder = 0
          TabStop = True
          Visible = False
          OnDragDrop = MainTabHeaderFrameDragDrop
          OnDragOver = MainTabHeaderFrameDragOver
          ExplicitWidth = 619
          ExplicitHeight = 24
        end
        object SQLEditMaximizedPnl: TTntPanel
          Left = 0
          Top = 24
          Width = 619
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          Constraints.MinHeight = 40
          TabOrder = 1
          Visible = False
          object SQLEditMaximizedSplitterPBox: TTntPaintBox
            Left = 0
            Top = 41
            Width = 619
            Height = 4
            Cursor = crVSplit
            Align = alBottom
            OnMouseDown = SQLEditMaximizedSplitterPBoxMouseDown
            OnMouseMove = SQLEditMaximizedSplitterPBoxMouseMove
            OnMouseUp = SQLEditMaximizedSplitterPBoxMouseUp
            OnPaint = SQLEditMaximizedSplitterPBoxPaint
            ExplicitWidth = 621
          end
          object QueryAreaHeaderPBox: TTntPaintBox
            Left = 0
            Top = 0
            Width = 619
            Height = 50
            Align = alTop
            OnPaint = QueryAreaHeaderPBoxPaint
            ExplicitWidth = 621
          end
        end
      end
    end
    object SidebarPnl: TTntPanel
      Left = 626
      Top = 183
      Width = 220
      Height = 447
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 3
      DesignSize = (
        220
        447)
      object SidebarSepShape: TTntShape
        Left = 0
        Top = 234
        Width = 220
        Height = 9
        Align = alBottom
        Brush.Color = clBtnFace
        Pen.Color = clBtnFace
        ExplicitTop = 97
      end
      inline UpperTabHeaderFrame: TTabHeaderFrame
        Left = 0
        Top = 0
        Width = 220
        Height = 234
        Align = alClient
        Color = clWhite
        ParentColor = False
        TabOrder = 0
        TabStop = True
        OnDragOver = MainTabHeaderFrameDragOver
        ExplicitWidth = 220
        ExplicitHeight = 234
        inherited TabHeaderPopupMenu: TTntPopupMenu
          Left = 18
          Top = 65532
        end
      end
      inline LowerTabHeaderFrame: TTabHeaderFrame
        Left = 0
        Top = 243
        Width = 220
        Height = 204
        Align = alBottom
        Color = clWhite
        ParentColor = False
        TabOrder = 1
        TabStop = True
        ExplicitTop = 243
        ExplicitWidth = 220
        ExplicitHeight = 204
      end
      object UpperPageControl: TTntPageControl
        Left = 4
        Top = 4
        Width = 212
        Height = 208
        ActivePage = TntTabSheet1
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabHeight = 18
        TabOrder = 2
        Visible = False
        object TntTabSheet1: TTntTabSheet
          Caption = 'Schemata'
          object SchemataPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 180
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            inline SchemataFrame: TSchemataFrame
              Left = 0
              Top = 0
              Width = 204
              Height = 180
              Align = alClient
              TabOrder = 0
              TabStop = True
              ExplicitWidth = 204
              ExplicitHeight = 180
              inherited CatalogVST: TVirtualStringTree
                Width = 204
                Height = 135
                DragMode = dmAutomatic
                TreeOptions.SelectionOptions = [toLevelSelectConstraint, toMultiSelect, toRightClickSelect]
                OnDblClick = SchemataFrameCatalogVSTDblClick
                OnDragOver = SchemataFrameCatalogVSTDragOver
                OnEndDrag = SchemataFrameCatalogVSTEndDrag
                OnKeyDown = SchemataFrameCatalogVSTKeyDown
                OnMouseDown = SchemataFrameCatalogVSTMouseDown
                OnMouseUp = SchemataFrameCatalogVSTMouseUp
                ExplicitWidth = 204
                ExplicitHeight = 135
                WideDefaultText = 'Fetching Data ...'
              end
              inherited TopPnl: TTntPanel
                Width = 204
                ExplicitWidth = 204
                inherited SchemataLbl: TTntLabel
                  Width = 48
                  ExplicitWidth = 48
                end
              end
              inherited SpacerPnl: TTntPanel
                Width = 204
                ExplicitWidth = 204
              end
              inherited AdvancedEdit: TAdvancedEditFrame
                Width = 204
                ExplicitWidth = 204
              end
              inherited SchemaSearchPopupMenu: TTntPopupMenu
                Left = 66
                Top = 116
              end
              inherited SchemaTreeViewPopupMenu: TTntPopupMenu
                OnPopup = SchemataFrameSchemaTreeViewPopupMenuPopup
                Left = 58
                Top = 146
              end
            end
          end
        end
        object TntTabSheet2: TTntTabSheet
          Caption = 'Bookmarks'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object BookmarksPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 180
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object BookmarkSpacerShape: TTntShape
              Left = 0
              Top = 23
              Width = 204
              Height = 5
              Align = alTop
              Pen.Color = clWhite
            end
            inline BookmarksAdvancedEdit: TAdvancedEditFrame
              Left = 0
              Top = 0
              Width = 204
              Height = 23
              Align = alTop
              Color = clSkyBlue
              ParentColor = False
              TabOrder = 0
              TabStop = True
              ExplicitWidth = 204
              ExplicitHeight = 23
              inherited SearchEd: TTntEdit
                OnChange = BookmarksAdvancedEditSearchEdChange
              end
            end
            object BookmarkVT: TVirtualStringTree
              Left = 0
              Top = 28
              Width = 204
              Height = 152
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 16
              DragType = dtVCL
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.MainColumn = -1
              Header.Options = [hoColumnResize, hoDrag]
              HintMode = hmHintAndDefault
              Images = SideBarImageList
              Indent = 21
              ParentFont = False
              ParentShowHint = False
              PopupMenu = BookmarkPopupMenu
              ShowHint = True
              TabOrder = 1
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toMultiSelect, toRightClickSelect, toSiblingSelectConstraint]
              OnDblClick = BookmarkVTDblClick
              OnDragOver = BookmarkVTDragOver
              OnDragDrop = BookmarkVTDragDrop
              OnGetText = BookmarkVTGetText
              OnGetImageIndex = BookmarkVTGetImageIndex
              OnGetHint = BookmarkVTGetHint
              OnKeyDown = BookmarkVTKeyDown
              OnMouseDown = BookmarkVTMouseDown
              OnNewText = BookmarkVTNewText
              Columns = <>
            end
          end
        end
        object TntTabSheet3: TTntTabSheet
          Caption = 'History'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object HistoryPanel: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 180
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object HistorySpacerShape: TTntShape
              Left = 0
              Top = 23
              Width = 204
              Height = 5
              Align = alTop
              Pen.Color = clWhite
            end
            inline HistoryAdvancedEdit: TAdvancedEditFrame
              Left = 0
              Top = 0
              Width = 204
              Height = 23
              Align = alTop
              Color = clWhite
              ParentColor = False
              TabOrder = 0
              TabStop = True
              ExplicitWidth = 204
              ExplicitHeight = 23
              inherited SearchEd: TTntEdit
                OnChange = HistoryAdvancedEditSearchEdChange
              end
            end
            object HistoryVT: TVirtualStringTree
              Left = 0
              Top = 28
              Width = 204
              Height = 152
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 16
              DragMode = dmAutomatic
              DragType = dtVCL
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoAutoSpring]
              HintMode = hmHintAndDefault
              Images = SideBarImageList
              ParentFont = False
              ParentShowHint = False
              PopupMenu = HistoryPopupMenu
              ScrollBarOptions.ScrollBars = ssVertical
              ShowHint = True
              TabOrder = 1
              TreeOptions.AnimationOptions = [toAnimatedToggle]
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toMultiSelect]
              OnDblClick = HistoryVTDblClick
              OnGetText = HistoryVTGetText
              OnGetImageIndex = HistoryVTGetImageIndex
              OnGetHint = HistoryVTGetHint
              Columns = <
                item
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                  Position = 0
                  Width = 204
                  WideText = 'Col1'
                end>
            end
          end
        end
      end
      object LowerPageControl: TTntPageControl
        Left = 4
        Top = 229
        Width = 212
        Height = 195
        ActivePage = ParamSheet
        Anchors = [akRight, akBottom]
        TabOrder = 3
        Visible = False
        object SynSheet: TTntTabSheet
          Caption = 'Syntax'
          ImageIndex = 2
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object SyntaxPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 167
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object SyntaxVT: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 204
              Height = 167
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 16
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoAutoSpring]
              Images = SideBarImageList
              ParentFont = False
              ScrollBarOptions.ScrollBars = ssVertical
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              OnDblClick = FunctionsVTDblClick
              OnGetText = FunctionsVTGetText
              OnGetImageIndex = FunctionsVTGetImageIndex
              Columns = <
                item
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                  Position = 0
                  Width = 204
                  WideText = 'Col1'
                end>
            end
          end
        end
        object FuncSheet: TTntTabSheet
          Caption = 'Functions'
          ImageIndex = 1
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object FunctionPanel: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 167
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object FunctionsVT: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 204
              Height = 167
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 16
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoAutoSpring]
              Images = SideBarImageList
              ParentFont = False
              ScrollBarOptions.ScrollBars = ssVertical
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              OnDblClick = FunctionsVTDblClick
              OnGetText = FunctionsVTGetText
              OnGetImageIndex = FunctionsVTGetImageIndex
              Columns = <
                item
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                  Position = 0
                  Width = 204
                  WideText = 'Col1'
                end>
            end
          end
        end
        object ParamSheet: TTntTabSheet
          Caption = 'Params'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object ParamPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 167
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object ParamVT: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 204
              Height = 167
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 14
              DragType = dtVCL
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -9
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.Options = [hoColumnResize, hoDrag]
              Images = SideBarImageList
              ParentFont = False
              PopupMenu = ParamPopupMenu
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
              TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
              TreeOptions.StringOptions = [toAutoAcceptEditChange]
              OnDblClick = ParamVTDblClick
              OnEditing = ParamVTEditing
              OnGetText = ParamVTGetText
              OnGetImageIndex = ParamVTGetImageIndex
              OnMouseDown = ParamVTMouseDown
              OnNewText = ParamVTNewText
              Columns = <
                item
                  Position = 0
                  Width = 108
                  WideText = 'Param'
                end
                item
                  Position = 1
                  Width = 92
                  WideText = 'Value'
                end>
            end
          end
        end
        object TrxTabSheet: TTntTabSheet
          Caption = 'Trx'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object TrxPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 204
            Height = 167
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object TransactionDisplay: TVirtualStringTree
              Left = 0
              Top = 0
              Width = 204
              Height = 167
              Align = alClient
              ButtonStyle = bsTriangle
              DefaultNodeHeight = 16
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              Header.AutoSizeIndex = 0
              Header.Font.Charset = DEFAULT_CHARSET
              Header.Font.Color = clWindowText
              Header.Font.Height = -11
              Header.Font.Name = 'MS Sans Serif'
              Header.Font.Style = []
              Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoAutoSpring]
              HintAnimation = hatNone
              HintMode = hmTooltip
              ParentFont = False
              ParentShowHint = False
              PopupMenu = TrxPopupMenu
              ScrollBarOptions.ScrollBars = ssVertical
              ShowHint = True
              TabOrder = 0
              TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
              TreeOptions.MiscOptions = [toInitOnSave, toToggleOnDblClick, toWheelPanning]
              TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages]
              TreeOptions.SelectionOptions = [toFullRowSelect]
              TreeOptions.StringOptions = [toAutoAcceptEditChange]
              OnGetText = TransactionDisplayGetText
              Columns = <
                item
                  Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                  Position = 0
                  Width = 204
                  WideText = 'Col1'
                end>
            end
          end
        end
      end
    end
  end
  object QueryExecutePopupMenu: TTntPopupMenu
    Left = 748
    Top = 16
    object ExecuteFromToolbarMI: TTntMenuItem
      Caption = 'Execute (Ctrl+Enter)'
      OnClick = QueryExecuteClick
    end
    object ExecuteInNewTabFromToolbarMI: TTntMenuItem
      Caption = 'Execute in New Tab (Ctrl+Shift+Enter)'
      OnClick = QueryExecuteInNewTabClick
    end
    object SplitTabAndExecuteFromToolbarMI: TTntMenuItem
      Caption = 'Split Tab and Execute (Ctrl+Alt+Enter)'
      OnClick = QuerySplitAndExecuteClick
    end
  end
  object QueryPopupMenu: TTntPopupMenu
    OnPopup = QueryPopupMenuPopup
    Left = 218
    Top = 12
    object Undo1: TTntMenuItem
      Action = MainForm.EditUndo1
    end
    object Redo1: TTntMenuItem
      Action = MainForm.EditRedo1
    end
    object N25: TTntMenuItem
      Caption = '-'
    end
    object SQLEditCutMI: TTntMenuItem
      Action = MainForm.EditCut1
    end
    object SQLEditCopyMI: TTntMenuItem
      Action = MainForm.EditCopy1
    end
    object SQLEditPasteMI: TTntMenuItem
      Action = MainForm.EditPaste1
    end
    object SelectAll1: TTntMenuItem
      Action = MainForm.EditSelectAll1
    end
    object ClearQueryEditorMI: TTntMenuItem
      Caption = 'Clear'
      OnClick = ClearQueryEditorMIClick
    end
    object N5: TTntMenuItem
      Caption = '-'
    end
    object OpenQueryMI: TTntMenuItem
      Caption = 'Open Query ...'
      OnClick = OpenQueryMIClick
    end
    object SaveQueryAsMI: TTntMenuItem
      Caption = 'Save Query As ...'
      OnClick = SaveQueryAsMIClick
    end
    object N15: TTntMenuItem
      Caption = '-'
    end
    object PasteClipboardContentasPHPcodeMI: TTntMenuItem
      Caption = 'Paste Clipboard Content as PHP Code'
      OnClick = PasteClipboardContentasPHPcodeMIClick
    end
    object PasteClipboardContentasJavaCodeMI: TTntMenuItem
      Caption = 'Paste Clipboard Content as Java Code'
      OnClick = PasteClipboardContentasJavaCodeMIClick
    end
    object N16: TTntMenuItem
      Caption = '-'
    end
    object CopySQLasPHPcodeMI: TTntMenuItem
      Caption = 'Copy Query as PHP code'
      OnClick = CopySQLasPHPcodeMIClick
    end
    object CopySQLasJavaCodeMI: TTntMenuItem
      Caption = 'Copy Query as Java Code'
      OnClick = CopySQLasJavaCodeMIClick
    end
  end
  object SideBarImageList: TImageList
    Height = 14
    Masked = False
    Width = 14
    Left = 669
    Top = 346
    Bitmap = {
      494C01010800090004000E000E00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000380000002A0000000100200000000000C024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEEFD6000000
      0000000000000000000000000000000000000000000000000000FFFFFF00B5B5
      B500BDBDBD00D6D6D600E7E7E700F7F7F7000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00F7F7F700D6D6D6008C8C8C00737373006B6B
      6B007B7B7B00C6C6C600F7F7F700F7F7F700FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00BDBDBD006B7B730039634A0039634A005A6B6300ADAD
      AD00FFFFFF00000000000000000000000000B5DEC600B5DEC600B5DEC600B5DE
      C600B5DEC600B5DEC600FFFFFF000000000000000000A5D6B50042AD63009CD6
      AD00000000000000000000000000FFFFFF00E7E7E700BDBDBD009C9C9C00ADAD
      AD00B5B5B500BDBDBD00B5B5B500ADADAD00ADADAD00ADADAD00ADADAD008C8C
      8C00FFFFFF00FFFFFF00EFEFEF007B7B7B00636363006B6B6B00636363004A4A
      4A00525252002121210042424200E7E7E700F7F7F700FFFFFF0000000000FFFF
      FF00E7E7E7004A735A00398C5A0039A56B0042A56B0042A56B0042A56B003994
      630039524200D6D6D600000000000000000042AD630042AD630042AD630042AD
      630042AD630042AD6300F7FFF70000000000A5D6B50042AD630042AD630042AD
      63009CD6AD000000000094949400ADADAD00ADADAD00BDBDC600ADADAD00ADAD
      A500DECEC600CEBDB500CEBDB500CEBDB500CEBDB500CEBDB500ADADA500A5A5
      A500FFFFFF00EFEFEF00737373007373730031313100212121008C8C8C003939
      6B0018181800212121004242420021212100E7E7E700FFFFFF00FFFFFF00E7EF
      EF00427B5A0042AD6B004AA56B0042AD6B0042B5730042B573004AB573004AB5
      73004AAD7300316B4A00D6D6D6000000000042AD630042AD630042AD630042AD
      630042AD630042AD6300F7FFF700D6EFDE0042AD630042AD630042AD630042AD
      630042AD6300CEEFD600ADADAD00B5B5B500A5A5A500B5B5AD00E7D6D600C6BD
      BD00E7DECE00FFEFDE00FFEFDE00FFE7D600FFE7D600FFE7D600C6BDB500A5A5
      A500FFFFFF008C8C8C008C8C8C0031313100ADADAD004A4A4A00393939003939
      9400737373006B6B6B0008080800424242004A4A4A00FFFFFF00FFFFFF00638C
      730042B5730042AD73008CC6A50073BD940042AD73004ABD7B004ABD7B004ABD
      84004ABD84004AB57B0039635200FFFFFF0042AD630042AD630042AD630042AD
      630042AD630042AD6300F7FFF70000000000ADDEBD0042AD630042AD630042AD
      6300A5D6B50000000000ADADAD00C6C6C600FFFFF700FFF7EF00FFF7EF00C6C6
      BD00E7DED600FFEFDE00FFEFDE00FFEFDE00FFE7D600FFE7D600C6BDB500A5A5
      A500DEDEDE007B7B7B0052525200525252004A4A4A006B6B6B00423942003131
      8C005A5A5A00292129006B6B6B003131310018181800CECECE00D6DEDE0052A5
      7B0052BD84004AB57B005AB58400F7F7F70084C6A5004AB57B004ABD840052C6
      8C0052C68C0052BD8C004AAD7B00B5B5B50000000000F7FFF7004AB56B000000
      00000000000000000000000000000000000000000000A5DEB50042AD6300A5D6
      B5000000000000000000ADADAD00C6C6C600FFFFFF00FFFFF700FFF7F700C6C6
      BD00E7DED600F7E7DE00CEAD9400FFEFE700CEAD9400F7E7D600C6BDB500A5A5
      A500ADADAD0094949400848484009C9C9C006B636B004A4A4A00424242003131
      8C0031313100313131004242420029292900424242007B7B7B00A5BDAD006BCE
      94005AC68C004ABD7B004AAD7B00D6EFDE00F7F7F70094CEB5004ABD840052C6
      8C0052CE940052CE940052C68C006384730000000000F7FFF7004AB56B000000
      00000000000000000000000000000000000000000000F7FFF7004AB56B000000
      00000000000000000000ADADAD00C6C6C60000000000FFFFFF00FFFFF700C6C6
      C600E7DEDE00F7EFE700945A4200DEBDAD00945A4200F7E7D600C6BDB500A5A5
      A5009C9C9C008C8C8C006B6B6B005A525A004A4A4A004A424A00424242003131
      9400313131002929290031293100ADADAD00313131005A5A5A008CB5A50073D6
      A5005AC694004ABD84006BC69400B5DECE00FFFFFF00FFFFFF00A5DEBD0052C6
      8C005ACE94005ACE9C005ACE94005A8C730000000000F7FFF70042AD630042AD
      630042AD630042AD630042AD630042AD630042AD630042AD63004AB56B000000
      00000000000000000000ADADAD00C6C6C6000000000000000000FFFFFF00CEC6
      C600E7E7DE00F7EFE700945A3900945A3900945A3900F7E7DE00C6BDB500A5A5
      A5009C9C9C00ADADAD00BDBDBD006B6B6B00424242004A4A4A00B5B5B5008484
      BD004A4A4A00292929005A5A5A0018181800393939006363630094BDAD0084DE
      B50063CE9C0052BD8400CEEFDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B5E7
      CE005AC694005AD69C005ACE9C0063947B000000000000000000000000000000
      000000000000F7FFF7004AB56B00000000000000000000000000000000000000
      00000000000000000000ADADAD00C6C6C600000000000000000000000000CECE
      CE00EFE7E700FFF7EF00945A3900945A3900945A3900F7E7DE00C6BDBD00A5A5
      A500B5B5B500A5A5A500848484007373730063636300C6BDC6005A5A5A003939
      39008C8C8C008C8C8C0052525200313131004A4A4A008C8C8C00B5CEC6008CE7
      BD0073D6AD005AC6940073CEA500FFFFFF00FFFFFF00F7FFF7008CD6B5007BD6
      AD0063CE9C0063D6A50063D6A50084A594000000000000000000FFFFFF00B5DE
      C600B5DEC600ADDEBD004AB56B00B5DEC600B5DEC600BDE7C600000000000000
      00000000000000000000ADADAD00C6C6C600000000000000000000000000C6C6
      C600E7E7E700FFF7F700945A3900945A3900945A3900F7EFE700C6BDBD00A5A5
      A500E7E7E7008C8C8C00B5B5B500E7E7E700DEDEDE00524A5200393939003939
      3900292929005A5A5A00BDBDBD008484840031313100DEDEDE00E7EFEF008CD6
      B5008CE7BD006BD6A5005AC69400CEEFDE00FFFFFF00FFFFFF00EFF7F70073D6
      AD0063D69C0063DEA50063C69C00C6D6CE000000000000000000F7FFF70042AD
      630042AD630042AD630042AD630042AD630042AD63004AB56B00000000000000
      00000000000000000000ADADAD00C6C6C6000000000000000000F7F7F700C6C6
      C600B5B5B500F7EFEF00945A3900945A3900945A3900F7EFEF00C6BDBD009494
      940000000000949494008C8C8C009C9C9C00848484007B7B7B00636363008484
      8400524A5200ADADAD0031313100393939006363630000000000FFFFFF00A5D6
      BD009CEFCE0084E7BD006BD6A5007BD6AD00DEF7EF00DEF7EF00DEF7EF00D6F7
      E7006BD6A50063DEAD0073AD9400FFFFFF000000000000000000F7FFF70042AD
      630042AD630042AD630042AD630042AD630042AD63004AB56B00000000000000
      00000000000000000000A5A5A500ADADAD00C6C6C600B5B5B500BDBDBD00FFFF
      FF00FFFFFF00BDBDB500945A3900945A3900945A3900ADA5A500ADADAD00E7E7
      E70000000000FFFFFF006363630084848400DEDEDE00C6C6C600ADADAD00E7E7
      E70073737300636363004242420042424200F7F7F7000000000000000000F7FF
      FF0094CEB5009CEFCE008CE7C6007BDEB50073D6AD006BD6AD006BD6A5006BD6
      A50063DEA5006BBD9C00E7EFEF00FFFFFF000000000000000000F7FFF70042AD
      630042AD630042AD630042AD630042AD630042AD63004AB56B00000000000000
      0000000000000000000000000000DEDEDE00E7E7E700F7F7F700000000000000
      000000000000FFF7F700945A3900945A3900945A3900FFF7F700000000000000
      00000000000000000000FFFFFF008C8C8C003939390063636300848484007B7B
      7B005A5A5A00212121008C8C8C00FFFFFF00000000000000000000000000FFFF
      FF00F7FFF700A5D6BD0094DEC60094EFCE008CE7C60084E7BD007BE7BD0073D6
      AD0084C6A500E7EFEF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFF7F700AD7B6300AD7B6300AD7B6300FFF7F700000000000000
      000000000000000000000000000000000000E7E7E700ADADAD00949494008C8C
      8C009C9C9C00E7E7E70000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00E7F7EF00B5DECE00A5D6C6009CD6BD00ADD6C600D6E7
      DE00F7FFF700FFFFFF00FFFFFF00FFFFFF0000000000F7F7F700F7F7F700F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700000000000000000000000000F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7F7F700E7E7E700DEDEDE00D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600DEDEDE00E7E7
      E700F7F7F700FFFFFF00F7F7F700E7E7E700DEDEDE00D6D6D600D6D6D600D6D6
      D600D6D6D600D6D6D600D6D6D600D6D6D600DEDEDE00E7E7E700F7F7F700FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D6C6B500BD9C7B00DECEBD00F7F7
      EF00C6AD9400BDA58C00F7F7EF0000000000A5D6E70042ADD60031A5CE00219C
      CE00219CCE00219CCE00219CCE00219CCE00219CCE00219CCE0031A5CE0039A5
      CE0094CEDE00FFFFFF00A5D6E70031A5CE00219CCE00219CCE00219CCE00219C
      CE00219CCE00219CCE00219CCE00219CCE005AB5D600C6CECE00EFEFEF00FFFF
      FF0029ADE70029ADE70029ADE70029ADE70029ADE70029ADE70029ADE70029AD
      E70029ADE70029ADE70029ADE70029ADE70029ADE70039B5E700000000000000
      0000000000000000000000000000EFE7DE00BD9C7B00CEB5A500000000000000
      0000EFE7DE00BD9C7B00D6BDAD000000000029A5CE0039ADDE00219CCE0042BD
      E70042BDE70042BDE70042BDE70042BDE70042BDE70042BDE70052BDE70073C6
      DE00189CC600F7F7F70029A5CE00189CCE0042BDE70042BDE70042BDE70042BD
      E70042BDE70042BDE70042BDE70042BDE70031ADD6004AADCE00DEDEDE00F7F7
      F70029ADE7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000039B5E700F7EFE700BD9C
      7B00BDA58C000000000000000000D6BDAD00BD9C7B00F7EFE700000000000000
      000000000000D6BDAD00BD9C7B00F7EFE70031A5CE004ABDE700189CC6006BCE
      F7006BCEF7006BCEF7006BCEF7006BCEF7006BCEF7006BCEF70073CEEF008CD6
      EF00189CC600EFEFEF0031A5CE00189CC60063CEF7006BCEF7006BCEF7006BCE
      F7006BCEF7006BCEF7006BCEF7006BCEF7006BCEEF00219CCE00C6CED600EFEF
      EF0029ADE70000000000F7FFFF000000000000000000E7F7FF00000000000000
      0000000000000000000000000000000000000000000039B5E700F7EFE700BD9C
      7B00BDA58C000000000000000000BDA58C00C6AD940000000000000000000000
      000000000000E7DECE00BD9C7B00DED6C60031A5CE0052C6EF00189CC6007BD6
      F7007BD6F7007BD6F7007BD6F7007BD6F7007BD6F7007BD6F7007BD6EF009CDE
      EF00189CC600EFEFEF0031A5CE0021A5CE0063CEEF007BD6F7007BD6F7007BD6
      F7007BD6F7007BD6F7007BD6F7007BD6F7007BD6EF0052B5D60073B5CE00EFEF
      EF0029ADE700E7F7FF0052C6EF00E7F7FF008CD6F7008CD6F700000000000000
      0000000000000000000000000000000000000000000039B5E700F7EFE700BD9C
      7B00BDA58C0000000000F7EFE700BD9C7B00D6C6B50000000000000000000000
      000000000000F7EFE700BD9C7B00D6C6B50031A5CE0063CEEF00189CC6008CDE
      F7008CDEF7008CDEF7008CDEF7008CDEF7008CDEF7008CDEF7008CD6EF00A5DE
      F700189CC600F7F7F70031A5CE0031ADD6005AC6E7008CDEF7008CDEF7008CDE
      F7008CDEF7008CDEF7008CDEF7008CDEF7008CD6EF0094D6EF00219CCE00EFEF
      F70029ADE70000000000ADDEF70052C6EF0052C6EF00000000008CD6F70031B5
      EF0031B5EF0031B5EF0031B5EF00E7F7FF000000000039B5E700F7EFE700BD9C
      7B00BDA58C0000000000F7EFE700BD9C7B00DECEBD0000000000000000000000
      000000000000FFFFF700BD9C7B00D6BDAD0031A5CE006BDEF700189CC600A5EF
      F700A5EFF700A5EFF700A5EFF700A5EFF700A5EFF700A5EFF700A5DEEF00ADDE
      EF00189CC6000000000031A5CE004ABDDE004ABDDE00A5EFF700A5EFF700A5EF
      F700A5EFF700A5EFF700A5EFF700A5EFF700A5DEEF00B5DEEF007BC6DE009CD6
      E70029ADE70000000000F7FFFF0031B5EF008CD6F70000000000F7FFFF00D6EF
      FF00D6EFFF00D6EFFF00D6EFFF00000000000000000039B5E700F7EFE700BD9C
      7B00BDA58C0000000000F7EFE700BD9C7B00D6C6B50000000000000000000000
      000000000000F7EFE700BD9C7B00D6C6B50031A5CE007BE7F70042BDDE00189C
      C600189CC600189CC600189CC600189CC600189CC600189CC600189CC600189C
      C6007BC6DE000000000031A5CE007BE7F70042BDDE00189CC600189CC600189C
      C600189CC600189CC600189CC600189CC600189CC600189CC60073C6DE00FFFF
      FF0029ADE7000000000073CEEF009CDEF70042BDEF00D6EFFF008CD6F70031B5
      EF0031B5EF0031B5EF0031B5EF00E7F7FF000000000039B5E700BDA58C00BD9C
      7B00BD9C7B00BD9C7B00F7EFE700BDA58C00C6AD940000000000000000000000
      000000000000E7DECE00BD9C7B00DED6C60031A5CE008CEFFF008CEFFF008CEF
      FF008CEFFF0094F7FF00E7FFFF00E7FFFF00E7FFFF00E7FFFF0084CEE70094CE
      E700000000000000000031A5CE008CEFFF008CEFFF008CEFFF008CEFFF0094F7
      FF00E7FFFF00E7FFFF00E7FFFF00E7FFFF0084CEE70094CEE700000000000000
      000029ADE700D6EFFF009CDEF70000000000E7F7FF009CDEF700000000000000
      0000000000000000000000000000000000000000000039B5E700F7EFE700BD9C
      7B00C6B59C000000000000000000D6BDAD00BD9C7B00F7EFE700000000000000
      000000000000CEB5A500BD9C7B00F7EFE70031A5CE00B5FFFF0094FFFF0094FF
      FF0094FFFF00C6F7FF004AB5D60042ADD60042ADD60042ADD60029A5CE00EFF7
      FF00000000000000000031A5CE00B5FFFF0094FFFF0094FFFF0094FFFF00C6F7
      FF004AB5D60042ADD60042ADD60042ADD60029A5CE00EFF7FF00000000000000
      000029ADE7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000039B5E700FFFFF700BDA5
      8C00BDA58C000000000000000000EFE7DE00BD9C7B00CEB5A500000000000000
      0000EFDED600BD9C7B00D6BDAD0000000000189CC600BDE7EF00000000000000
      00000000000084CEE70084CEE700D6EFF700D6EFF700D6EFF700EFF7FF000000
      00000000000000000000189CC600BDE7EF0000000000000000000000000084CE
      E70084CEE700D6EFF700D6EFF700D6EFF700EFF7FF0000000000000000000000
      000029ADE7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000039B5E70000000000E7DE
      CE00BD9C7B00BD9C7B00DECEBD0000000000D6C6B500BD9C7B00DECEBD00F7F7
      EF00C6AD9400BDA58C00F7F7EF0000000000BDE7EF005AB5D600189CC600189C
      C600189CC60073C6DE00EFFFFF00000000000000000000000000000000000000
      00000000000000000000BDE7EF005AB5D600189CC600189CC600189CC60073C6
      DE00EFFFFF000000000000000000000000000000000000000000000000000000
      000029ADE70029ADE70029ADE70029ADE70029ADE70029ADE70029ADE70029AD
      E70029ADE70029ADE70029ADE70029ADE70029ADE70039B5E700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000380000002A0000000100010000000000500100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FFDFC0F000200700018E00000020030001040000000001000000000000000000
      01040000000000009F8C0000000000009F9C800000000000801CC00000000000
      F9FCE00000000000C03CE00000000000C03CC00800400000C03C000800600000
      C03E383C00E00000FFFFF83F03F00000800E003FFFFFFF000000000FFFFF0100
      00000000003E310000000007FF86380000000005BF867800000000003F847800
      000000044084780000040004418478000004000400807800000C00313F863800
      000C0037FF863100381CE077FFA1010001FC07F0003FFF00FFFFFFFFFFFFFF00
      00000000000000000000000000000000000000000000}
  end
  object BookmarkPopupMenu: TTntPopupMenu
    OnPopup = BookmarkPopupMenuPopup
    Left = 807
    Top = 316
    object CreateBookmarkFolderMI: TTntMenuItem
      Caption = 'Create Bookmark Folder'
      OnClick = CreateBookmarkFolderMIClick
    end
    object N1: TTntMenuItem
      Caption = '-'
    end
    object DeleteBookmarkMI: TTntMenuItem
      Caption = 'Delete Items'
      OnClick = DeleteBookmarkNodeMIClick
    end
    object N26: TTntMenuItem
      Caption = '-'
    end
    object SaveBookmarksMI: TTntMenuItem
      Caption = 'Save Bookmarks'
      OnClick = SaveBookmarksMIClick
    end
    object LoadBookmarksMI: TTntMenuItem
      Caption = 'Load Bookmarks'
      OnClick = LoadBookmarksMIClick
    end
  end
  object RSGridPopupMenu: TTntPopupMenu
    Images = RSGridPopupImageList
    OnPopup = RSGridPopupMenuPopup
    Left = 428
    Top = 210
    object AddNewRSTabMI: TTntMenuItem
      Caption = 'Add New Resultset Tab (Ctrl+T) '
      ImageIndex = 0
      OnClick = AddNewRSTabMIClick
    end
    object RSAddNewScriptTabMI: TTntMenuItem
      Caption = 'Add New Script Tab (Ctrl+Shift+T)'
      ImageIndex = 1
      OnClick = AddNewScriptTabMIClick
    end
    object N2: TTntMenuItem
      Caption = '-'
    end
    object SplitResultsetTabVerticallyMI: TTntMenuItem
      Caption = 'Split Tab Vertically'
      ImageIndex = 3
      OnClick = SplitResultsetTabVerticallyMIClick
    end
    object SplitTabHorizontallyMI: TTntMenuItem
      Caption = 'Split Tab Horizontally'
      ImageIndex = 2
      OnClick = SplitTabHorizontallyMIClick
    end
    object N3: TTntMenuItem
      Caption = '-'
    end
    object RemoveTabsheetMI: TTntMenuItem
      Caption = 'Remove Tab'
      ImageIndex = 4
      OnClick = RemoveTabsheetMIClick
    end
    object RemoveResultSetMI: TTntMenuItem
      Caption = 'Remove Resultset'
      ImageIndex = 5
      OnClick = RemoveResultSetMIClick
    end
    object N4: TTntMenuItem
      Caption = '-'
    end
    object ExportResultsetMI: TTntMenuItem
      Caption = 'Export Resultset'
    end
    object N7: TTntMenuItem
      Caption = '-'
    end
    object AddRowMI: TTntMenuItem
      Caption = 'Add Row'
      OnClick = AddRowMIClick
    end
    object DeleteRowMI: TTntMenuItem
      Caption = 'Delete Row(s)'
      OnClick = DeleteRowMIClick
    end
    object CopyRowValuesMI: TTntMenuItem
      Caption = 'Copy Row Values'
      OnClick = CopyRowValuesMIClick
    end
    object N13: TTntMenuItem
      Caption = '-'
    end
    object LoadFieldContentMI: TTntMenuItem
      Caption = 'Load Field Content'
      OnClick = LoadFieldContentMIClick
    end
    object SaveFieldContentMI: TTntMenuItem
      Caption = 'Save Field Content'
      OnClick = SaveFieldContentMIClick
    end
    object CopyFieldContentMI: TTntMenuItem
      Caption = 'Copy Field Content'
      OnClick = CopyFieldContentMIClick
    end
    object ClearFieldContentMI: TTntMenuItem
      Caption = 'Clear Field Content'
      OnClick = ClearFieldContentMIClick
    end
    object N17: TTntMenuItem
      Caption = '-'
    end
    object ViewFieldinPopupEditorMI: TTntMenuItem
      Caption = 'View Field in Popup Editor'
      OnClick = ViewFieldinPopupEditorMIClick
    end
    object EditFieldinPopupEditorMI: TTntMenuItem
      Caption = 'Edit Field in Popup Editor'
      OnClick = EditFieldinPopupEditorMIClick
    end
  end
  object MainMenu: TTntMainMenu
    AutoHotkeys = maManual
    Left = 10
    Top = 14
    object ViewMI: TTntMenuItem
      Caption = 'View'
      GroupIndex = 30
      object MaximizeQueryEditMI: TTntMenuItem
        AutoCheck = True
        Caption = 'Maximize Query Edit (F11)'
        OnClick = MaximizeQueryEditMIClick
      end
      object OnlyTabsheetsMI: TTntMenuItem
        AutoCheck = True
        Caption = 'Maximize Tabsheets (F12)'
        OnClick = OnlyTabsheetsMIClick
      end
      object N14: TTntMenuItem
        Caption = '-'
      end
      object ShowSidebarMI: TTntMenuItem
        Caption = 'Sidebar'
        Checked = True
        OnClick = ShowSidebarMIClick
      end
    end
    object QueryMI: TTntMenuItem
      Caption = 'Query'
      SubMenuImages = BookmarkImageList
      GroupIndex = 110
      object AddBookmarkMI: TTntMenuItem
        Caption = 'Add Bookmark (Ctrl+B)'
        OnClick = AddBookmarkMIClick
      end
      object N8: TTntMenuItem
        Caption = '-'
      end
      object QueryExecuteMI: TTntMenuItem
        Caption = 'Execute (Ctrl+Enter)'
        OnClick = QueryExecuteClick
      end
      object QueryExecuteInNewTabMI: TTntMenuItem
        Caption = 'Execute in New Tab  (Ctrl+Shift+Enter)'
        OnClick = QueryExecuteInNewTabClick
      end
      object SplitTabandExecuteMI: TTntMenuItem
        Caption = 'Split Tab and Execute  (Ctrl+Alt+Enter)'
        OnClick = QuerySplitAndExecuteClick
      end
      object QueryRefreshMI: TTntMenuItem
        Caption = 'Refresh'
        OnClick = QueryRefreshClick
      end
      object QueryStopMI: TTntMenuItem
        Caption = 'Stop'
        OnClick = QueryStopClick
      end
      object N6: TTntMenuItem
        Caption = '-'
      end
      object ExplainMI: TTntMenuItem
        Caption = 'Explain'
        OnClick = DoExplain
      end
      object CompareResultsetsMI: TTntMenuItem
        Caption = 'Compare Resultsets'
        OnClick = DoCompare
      end
      object N12: TTntMenuItem
        Caption = '-'
      end
      object CreateViewfromSelectMI: TTntMenuItem
        Caption = 'Create View from Select'
        OnClick = SQLCreateViewClick
      end
      object N24: TTntMenuItem
        Caption = '-'
      end
    end
    object ScriptMI: TTntMenuItem
      Caption = 'Script'
      GroupIndex = 110
      object ScriptExecuteMI: TTntMenuItem
        Tag = 1
        Caption = 'Execute'
        OnClick = ScriptExecuteClick
      end
      object ScriptRunSelectionMI: TTntMenuItem
        Caption = 'Execute Selection'
        Enabled = False
        OnClick = ScriptRunSelectionMIClick
      end
      object N22: TTntMenuItem
        Caption = '-'
      end
      object ScriptContinueMI: TTntMenuItem
        Tag = 1
        Caption = 'Continue'
        OnClick = ScriptContinueClick
      end
      object ScriptStepOverMI: TTntMenuItem
        Tag = 1
        Caption = 'Step Over'
        OnClick = ScriptStepOverClick
      end
      object N9: TTntMenuItem
        Caption = '-'
      end
      object ScriptStopMI: TTntMenuItem
        Tag = 1
        Caption = 'Stop'
        Enabled = False
        OnClick = ScriptStopClick
      end
      object N11: TTntMenuItem
        Caption = '-'
      end
      object ToggleBreakpointMI: TTntMenuItem
        Tag = 1
        Caption = 'Toggle Breakpoint'
        OnClick = ToggleBreakpointMIClick
      end
      object ClearAllBreakpointsMI: TTntMenuItem
        Tag = 1
        Caption = 'Clear All Breakpoints'
        OnClick = ClearAllBreakpointsMIClick
      end
      object N10: TTntMenuItem
        Caption = '-'
      end
      object CreateStoredProcedureFunctionMI: TTntMenuItem
        Caption = 'Create Stored Procedure / Function'
        OnClick = CreateStoredProcedureFunctionMIClick
      end
      object EditAllStoredProceduresFunctionsMI: TTntMenuItem
        Caption = 'Edit All Stored Procedures / Functions'
        OnClick = EditAllStoredProceduresFunctionsMIClick
      end
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
    SpaceAttributes.Background = clWindow
    SpaceAttributes.Foreground = clGray
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
    Left = 186
    Top = 12
  end
  object ScriptMemoPopupMenu: TTntPopupMenu
    Images = RSGridPopupImageList
    OnPopup = ScriptMemoPopupMenuPopup
    Left = 460
    Top = 210
    object ScriptAddNewResultsetTabMI: TTntMenuItem
      Caption = 'Add New Resultset Tab (Ctrl+T) '
      ImageIndex = 0
      OnClick = AddNewRSTabMIClick
    end
    object AddNewScriptTabMI: TTntMenuItem
      Caption = 'Add New Script Tab (Ctrl+Shift+T)'
      ImageIndex = 1
      OnClick = AddNewScriptTabMIClick
    end
    object N19: TTntMenuItem
      Caption = '-'
    end
    object SplitScriptTabHorizontallyMI: TTntMenuItem
      Caption = 'Split Tab Horizontally'
      ImageIndex = 2
      OnClick = SplitScriptTabHorizontallyMIClick
    end
    object TntMenuItem2: TTntMenuItem
      Caption = '-'
    end
    object OpenScriptMI: TTntMenuItem
      Caption = 'Open Script ... (Ctrl+O)'
      OnClick = OpenScriptMIClick
    end
    object SaveScriptMI: TTntMenuItem
      Caption = 'Save Script'
      OnClick = SaveScriptMIClick
    end
    object SaveScriptAsMI: TTntMenuItem
      Caption = 'Save Script As ...'
      OnClick = SaveScriptAsMIClick
    end
    object N18: TTntMenuItem
      Caption = '-'
    end
    object FindMI: TTntMenuItem
      Caption = 'Find ...'
      OnClick = FindMIClick
    end
  end
  object HistoryPopupMenu: TTntPopupMenu
    OnPopup = HistoryPopupMenuPopup
    Left = 807
    Top = 351
    object AddHistoryItemasBookmarkMI: TTntMenuItem
      Caption = 'Add History Item as Bookmark'
      OnClick = AddHistoryItemasBookmarkMIClick
    end
    object N21: TTntMenuItem
      Caption = '-'
    end
    object DeleteSelectedHistoryEntriesMI: TTntMenuItem
      Caption = 'Delete Selected History Entries'
      OnClick = DeleteSelectedHistoryEntriesMIClick
    end
    object N20: TTntMenuItem
      Caption = '-'
    end
    object ClearHistoryMI: TTntMenuItem
      Caption = 'Clear History'
      OnClick = ClearHistoryMIClick
    end
  end
  object RSGridPopupImageList: TImageList
    Left = 428
    Top = 243
    Bitmap = {
      494C010106000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DEDEDE00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DEDEDE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0008080800000000000000
      00000000000008080800FFFFFF00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0008080800000000000000
      00000000000008080800FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DEDEDE00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DEDEDE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00D6D6D600B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000073CEEF0039C6FF0039C6
      FF0039C6FF0039C6FF0039C6FF005AC6EF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007BCEEF007BCE
      EF007BCEEF007BCEEF007BCEEF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B50000000000B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B5000000000000000000B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500F7F7F7000808
      0800FFFFFF00B5B5B500B5B5B5000000000000000000B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500F7F7F7000808
      0800FFFFFF00B5B5B500B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F7000808
      0800FFFFFF00FFFFFF00DEDEDE000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F7000808
      0800FFFFFF00FFFFFF00DEDEDE00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00CECE
      CE00CECECE00FFF7F700CECECE00CECECE00FFFFFF0008080800000000000000
      00000000000008080800FFFFFF000000000000000000B5B5B500FFF7F700FFB5
      7300FFB57300FFF7F700FFFFFF00FFFFFF00FFFFFF0008080800000000000000
      00000000000008080800FFFFFF00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F7000808
      0800FFFFFF00FFFFFF00DEDEDE000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F7000808
      0800FFFFFF00FFFFFF00DEDEDE00000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00CECE
      CE00CECECE00FFF7F700CECECE00CECECE00CECECE00FFF7F700F7F7F7000808
      0800FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00F7FF
      F7005ADE7B005ADE7B005ADE7B005ADE7B005ADE7B00F7FFF700F7F7F7000808
      0800FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00CECE
      CE00CECECE00FFF7F700CECECE00CECECE00CECECE00FFF7F700CECECE00CECE
      CE00CECECE00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00F7FF
      F7005ADE7B005ADE7B005ADE7B005ADE7B005ADE7B005ADE7B00F7FFF700FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00CECE
      CE00CECECE00FFF7F700CECECE00CECECE00CECECE00FFF7F700CECECE00CECE
      CE00CECECE00FFFFFF00B5B5B5000000000000000000B5B5B500FFF7F700FFB5
      7300FFB57300FFB57300FFB57300FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00D6D6D600B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B5000000000000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00D6D6D600B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B500000000000000000073CEEF0039C6FF0039C6
      FF0039C6FF0039C6FF0039C6FF005AC6EF000000000000000000000000000000
      0000000000000000000000000000000000000000000073CEEF0039C6FF0039C6
      FF0039C6FF0039C6FF0039C6FF005AC6EF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD00000000000000000000000000B5B5B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B5B5B50000000000B5B5B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5000000000000000000000000007BCEEF007BCE
      EF007BCEEF007BCEEF007BCEEF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007BCEEF007BCE
      EF007BCEEF007BCEEF007BCEEF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBDBD00B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500BDBDBD00000000000000000000000000B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500B5B5B50000000000B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      8001C001000000008001C001000000008001C001000000008001C00100000000
      8001C001000000008001C001000000008001FFFF000000008001C00100000000
      8001C001000000008001C001000000008001C0010000000080FFC00100000000
      C1FFC00100000000FFFFFFFF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFC081
      80018001C001C08180018001C001C08180018001C001C08180018001C001C081
      80018001C001C08180018001C001C08180018001FFFFC08180018001C001C081
      80018001C001C08180018001C001C08180018001C001C08180FF80FFC001C081
      C1FFC1FFC001C081FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object ParamPopupMenu: TTntPopupMenu
    OnPopup = ParamPopupMenuPopup
    Left = 676
    Top = 454
    object AddParameterMI: TTntMenuItem
      Caption = 'Add Parameter'
      OnClick = AddParameterMIClick
    end
    object DeleteParameterMI: TTntMenuItem
      Caption = 'Delete Parameter'
      OnClick = DeleteParameterMIClick
    end
    object MoveParametertoGlobalParametersMI: TTntMenuItem
      Caption = 'Move Parameter to Global Parameters'
      OnClick = MoveParametertoGlobalParametersMIClick
    end
    object N23: TTntMenuItem
      Caption = '-'
    end
    object RefreshParametersMI: TTntMenuItem
      Caption = 'Refresh Parameters'
      OnClick = RefreshParametersMIClick
    end
  end
  object BookmarkImageList: TImageList
    Left = 46
    Top = 14
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F7F700EFEFEF00EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00000000000000000000000000000000000000000000000000AD7B
      7B00AD7B7B00AD7B7B00AD7B7B00AD7B7B00AD7B7B00AD7B7B00AD7B7B00AD7B
      7B00AD7B7B00AD7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000094CEDE0039A5C600319CBD00319C
      BD00319CBD00319CBD00319CBD00319CBD00319CBD00319CBD00319CBD00319C
      BD005AADCE00E7E7E7000000000000000000000000000000000000000000B584
      8400FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7
      EF00FFF7EF00AD7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C60031A5C60063DEFF00D6F7
      FF00D6F7FF00D6F7FF00D6F7FF00D6F7FF00D6F7FF00D6F7FF00D6F7FF00CEF7
      FF00319CBD00C6CED600F7F7F70000000000000000000000000000000000B58C
      8400FFF7EF00FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEBD00FFDEB500FFDE
      B500FFF7EF00B5847B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C60042ADCE0073D6EF007BE7
      FF006BE7FF006BE7FF006BE7FF006BE7FF006BE7FF006BE7FF0073E7FF00D6F7
      FF005AADCE008CBDCE00E7E7E70000000000000000000000000000000000BD8C
      8400FFF7EF00FFE7CE00FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEBD00FFDE
      B500FFF7EF00B5847B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C6004ABDDE006BCEE70094EF
      FF0084EFFF0084EFFF0084EFFF0084EFFF0084EFFF0084EFFF0084EFFF00D6F7
      FF00A5D6E7004AADC600DEDEDE0000000000000000000000000000000000BD94
      8400FFF7EF00FFEFD600FFE7CE00FFE7CE00FFE7C600FFE7C600FFDEBD00FFDE
      BD00FFF7EF00BD8C7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C6005ACEE7005AC6DE00A5F7
      FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF00ADF7
      FF00C6E7EF005AB5CE00B5CED60000000000000000000000000000000000C694
      8400FFF7EF00FFEFDE00FFEFD600FFEFD600FFE7CE00FFE7C600FFE7C600FFDE
      BD00FFF7EF00BD94840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C6006BDEEF004AB5CE00BDFF
      FF00B5FFFF00B5FFFF00B5FFFF00B5FFFF00B5FFFF00B5FFFF00B5FFFF00B5FF
      FF00C6E7EF00A5D6E7007BBDCE0000000000000000000000000000000000C69C
      8C00FFF7EF00FFEFDE00FFEFDE00FFEFD600FFEFD600FFE7CE00FFE7C600FFE7
      C600FFF7EF00C694840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C6007BEFFF0039A5C600D6FF
      FF00D6FFFF00D6FFFF00D6FFFF00D6FFFF00D6FFFF00D6FFFF00D6FFFF00D6FF
      FF00C6E7EF00E7EFF700319CBD0000000000000000000000000000000000CE9C
      8C00FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEFD600FFEFD600FFE7CE00FFE7
      C600FFF7EF00CE9C840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C60084F7FF0052C6DE00319C
      BD00319CBD00319CBD00319CBD00319CBD00319CBD00319CBD00319CBD00319C
      BD00319CBD0039A5C60094CEDE0000000000000000000000000000000000D6A5
      8C00FFF7EF00FFF7EF00FFF7E700FFEFDE00FFE7D600EFDEC600EFD6BD00F7DE
      C600FFF7EF00CE9C840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C60084F7FF0084F7FF0084F7
      FF0084F7FF0084F7FF00A5FFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FF
      FF004AADCE00000000000000000000000000000000000000000000000000D6A5
      8C00FFF7EF00FFF7EF00FFF7EF00FFF7E700EFDECE00CEBDAD00C6B5A500DEC6
      B500F7EFE700D6A5840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000042A5C600C6FFFF008CFFFF008CFF
      FF008CFFFF008CFFFF00BDE7F70042A5C60042A5C60042A5C60042A5C60052AD
      CE009CD6E700000000000000000000000000000000000000000000000000D6AD
      8C00FFF7EF00FFF7F700FFF7EF00FFF7EF00E7DECE00C6BDAD00000000000000
      0000FFFFFF00D6A58C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000094CEDE00A5D6E700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006BBDD600C6DEEF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEAD
      8C00FFF7EF00FFFFF700FFF7F700FFF7EF00EFE7DE00DED6C60000000000FFFF
      FF00E7BDA500EFD6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094CEDE0042A5C60042A5
      C60042A5C60042A5C60094CEDE00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEB5
      9400FFF7EF00FFF7EF00FFF7EF00FFF7EF00F7F7E700F7EFE700FFFFFF00E7C6
      A500EFD6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DEAD
      8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C00EFD6
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000008007E00300000000
      0003E003000000000001E003000000000001E003000000000001E00300000000
      0001E003000000000001E003000000000001E003000000000001E00300000000
      0007E003000000000007E0330000000000FFE0230000000081FFE00700000000
      FFFFE00F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object TrxPopupMenu: TTntPopupMenu
    Left = 806
    Top = 386
    object TrxCopySQLMI: TTntMenuItem
      Caption = 'Copy SQL to Clipboard'
      OnClick = TrxCopySQLMIClick
    end
  end
end
