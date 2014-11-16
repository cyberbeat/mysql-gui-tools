object MigrationObjManEditForm: TMigrationObjManEditForm
  Left = 271
  Top = 174
  Caption = 'Migration Manual Edit'
  ClientHeight = 529
  ClientWidth = 765
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
  PixelsPerInch = 96
  TextHeight = 13
  object DockPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 765
    Height = 529
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = DockPnlResize
    object MainPngControl: TTntPageControl
      Left = 0
      Top = 0
      Width = 765
      Height = 529
      ActivePage = ObjMapEditTabSheet
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object MigOptsTabSheet: TTntTabSheet
        Caption = 'DoMig'
        DesignSize = (
          757
          498)
        object MigGBox: TTntGroupBox
          Left = 16
          Top = 10
          Width = 725
          Height = 227
          Caption = 'Migration of Meta Data'
          TabOrder = 0
          object GenerateSQLCreateStmtsImg: TTntImage
            Left = 68
            Top = 126
            Width = 13
            Height = 14
            AutoSize = True
            Picture.Data = {
              0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000000D00
              00000E0806000000F47F96D20000000467414D410000AFC837058AE900000019
              74455874536F6674776172650041646F626520496D616765526561647971C965
              3C000001AF4944415478DA63FCFFFF3F03A9801159535E5E1E2790F2006263A8
              D05920DE3169D2A4EF5835013554B0B2B2B66B6A6A32C8CACA82C51E3F7ECC70
              FDFA75861F3F7ED44C9B36ADED3F543158135043A3BCBC7C5D52521283A0A020
              8A53DEBF7FCF306FDE3C861B376E4C9833674E0950FD5FC6DCDC5C2D7E7EFEAB
              E5E5E50C3C3C3C58FDF0EDDB3786F6F676863367CE38ECD8B1E33048D3E49090
              901C3B3B3BBC9E3F74E810C3CC9933372F5BB62C1AA4E95E7D7DBDA2B0B0305E
              4D6FDFBE65A8ACACFC307BF66C6390A60F9D9D9DFC9C9C9C78357DFFFE9DA1B8
              B8F8F7F4E9D3AD409ACE03FD63202D2D8D57D3D3A74F19AAAAAA3E2E5AB4C813
              A429C7D6D676726868285E4DAB56AD62983061C289E3C78F67803431FFF9F367
              5F7E7EBE9DBABA3A560D376FDE64A8AEAEFEB276EDDA5A907E703C6564648401
              FDB4D2C3C383C1C5C58581999919ACF8EFDFBF0C7BF6EC61000633C3C1830737
              9F3F7FBE19287C099E22FCFCFCD40404049A8171160CF41F58D793274FFEDFB9
              73E7FEB973E7D6BE7EFD7A3D50E83250FD1794B4C7C8C8080A42716363637B60
              D211B87AF5EA43506883521410BF04AA05A7410061EDC8EB1FF7EBB500000000
              49454E44AE426082}
          end
          object MigTaskHeaderLbl: TTntLabel
            Left = 28
            Top = 32
            Width = 99
            Height = 13
            Caption = 'Tasks to execute'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object GenSQLLbl: TTntLabel
            Left = 88
            Top = 126
            Width = 158
            Height = 13
            Caption = 'Generate SQL Create Statements'
          end
          object MigTaskLbl: TTntLabel
            Left = 28
            Top = 48
            Width = 341
            Height = 26
            AutoSize = False
            Caption = 
              'The following tasks will now be executed. Please monitor the exe' +
              'cution progress. Press [Advanced >>] to see the log.'
            WordWrap = True
          end
          object MigrateImg: TTntImage
            Left = 68
            Top = 100
            Width = 13
            Height = 14
            AutoSize = True
            Picture.Data = {
              0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000000D00
              00000E0806000000F47F96D20000000467414D410000AFC837058AE900000019
              74455874536F6674776172650041646F626520496D616765526561647971C965
              3C000001AF4944415478DA63FCFFFF3F03A9801159535E5E1E2790F2006263A8
              D05920DE3169D2A4EF5835013554B0B2B2B66B6A6A32C8CACA82C51E3F7ECC70
              FDFA75861F3F7ED44C9B36ADED3F543158135043A3BCBC7C5D52521283A0A020
              8A53DEBF7FCF306FDE3C861B376E4C9833674E0950FD5FC6DCDC5C2D7E7EFEAB
              E5E5E50C3C3C3C58FDF0EDDB3786F6F676863367CE38ECD8B1E33048D3E49090
              901C3B3B3BBC9E3F74E810C3CC9933372F5BB62C1AA4E95E7D7DBDA2B0B0305E
              4D6FDFBE65A8ACACFC307BF66C6390A60F9D9D9DFC9C9C9C78357DFFFE9DA1B8
              B8F8F7F4E9D3AD409ACE03FD63202D2D8D57D3D3A74F19AAAAAA3E2E5AB4C813
              A429C7D6D676726868285E4DAB56AD62983061C289E3C78F67803431FFF9F367
              5F7E7EBE9DBABA3A560D376FDE64A8AEAEFEB276EDDA5A907E703C6564648401
              FDB4D2C3C383C1C5C58581999919ACF8EFDFBF0C7BF6EC61000633C3C1830737
              9F3F7FBE19287C099E22FCFCFCD40404049A8171160CF41F58D793274FFEDFB9
              73E7FEB973E7D6BE7EFD7A3D50E83250FD1794B4C7C8C8080A42716363637B60
              D211B87AF5EA43506883521410BF04AA05A7410061EDC8EB1FF7EBB500000000
              49454E44AE426082}
          end
          object MigExecLbl: TTntLabel
            Left = 88
            Top = 100
            Width = 126
            Height = 13
            Caption = 'Execute Migration Process'
          end
          object GrtMessageLbl: TTntLabel
            Left = 88
            Top = 176
            Width = 619
            Height = 13
            AutoSize = False
            Caption = '-'
            Visible = False
          end
          object GrtProgressLbl: TTntLabel
            Left = 346
            Top = 198
            Width = 361
            Height = 13
            AutoSize = False
            Caption = '-'
            Visible = False
          end
          object ResultLbl: TTntLabel
            Left = 88
            Top = 158
            Width = 196
            Height = 13
            Caption = 'Execution completed successfully.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            Visible = False
          end
          object GrtProgressBar: TTntProgressBar
            Left = 88
            Top = 196
            Width = 251
            Height = 17
            TabOrder = 0
            Visible = False
          end
        end
        object MessageLogGBox: TTntGroupBox
          Left = 16
          Top = 250
          Width = 725
          Height = 248
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Message Log'
          TabOrder = 1
          Visible = False
          DesignSize = (
            725
            248)
          object GRTMessageMemo: TTntMemo
            Left = 18
            Top = 22
            Width = 689
            Height = 208
            Anchors = [akLeft, akTop, akRight, akBottom]
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
      end
      object ObjMapEditTabSheet: TTntTabSheet
        Caption = 'Log'
        object LogMainPnl: TTntPanel
          Left = 0
          Top = 0
          Width = 757
          Height = 244
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            757
            244)
          object LogHeaderLbl: TTntLabel
            Left = 16
            Top = 14
            Width = 80
            Height = 13
            Caption = 'Migrated Objects'
          end
          object LogFilterLbl: TTntLabel
            Left = 515
            Top = 10
            Width = 25
            Height = 13
            Alignment = taRightJustify
            Anchors = [akTop, akRight]
            Caption = 'Filter:'
          end
          object MigLogVT: TVirtualStringTree
            Left = 16
            Top = 30
            Width = 733
            Height = 464
            Anchors = [akLeft, akTop, akRight, akBottom]
            Header.AutoSizeIndex = 0
            Header.Font.Charset = DEFAULT_CHARSET
            Header.Font.Color = clWindowText
            Header.Font.Height = -11
            Header.Font.Name = 'MS Sans Serif'
            Header.Font.Style = []
            Header.Options = [hoColumnResize, hoDrag, hoVisible, hoAutoSpring]
            HintMode = hmHint
            Images = SchemaListViewImgList
            Indent = 16
            ParentShowHint = False
            ScrollBarOptions.ScrollBars = ssVertical
            ShowHint = True
            TabOrder = 0
            TextMargin = 5
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
            TreeOptions.PaintOptions = [toShowRoot, toThemeAware, toUseBlendedImages]
            TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect]
            OnAfterCellPaint = MigLogVTAfterCellPaint
            OnBeforeItemErase = MigLogVTBeforeItemErase
            OnChange = MigLogVTChange
            OnEditing = MigLogVTEditing
            OnFreeNode = MigLogVTFreeNode
            OnGetText = MigLogVTGetText
            OnPaintText = MigLogVTPaintText
            OnGetImageIndex = MigLogVTGetImageIndex
            OnGetHint = MigLogVTGetHint
            OnInitChildren = MigLogVTInitChildren
            OnInitNode = MigLogVTInitNode
            OnMouseDown = MigLogVTMouseDown
            OnNewText = MigLogVTNewText
            Columns = <
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                Position = 0
                Width = 229
                WideText = 'Source Objects'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                Position = 1
                Width = 340
                WideText = 'Migration message'
              end
              item
                Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
                Position = 2
                Width = 160
                WideText = 'Target Objects'
              end>
            WideDefaultText = ''
          end
          object LogFilterComboBox: TTntComboBox
            Left = 554
            Top = 6
            Width = 195
            Height = 21
            Style = csDropDownList
            Anchors = [akTop, akRight]
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 1
            Text = 'Show Mapping Problems'
            OnCloseUp = LogFilterComboBoxCloseUp
            Items.Strings = (
              'Show Mapping Problems'
              'Show All Objects'
              'Show All Objects with Messages')
          end
          object NoMapProblemsPnl: TTntPanel
            Left = 138
            Top = 80
            Width = 423
            Height = 133
            Anchors = [akLeft, akTop, akRight, akBottom]
            BevelOuter = bvNone
            BorderStyle = bsSingle
            Color = clWhite
            TabOrder = 2
            Visible = False
            object TntShape1: TTntShape
              Left = 0
              Top = 0
              Width = 419
              Height = 129
              Align = alClient
              Pen.Color = clWhite
            end
            object TntLabel1: TTntLabel
              Left = 30
              Top = 11
              Width = 162
              Height = 13
              Caption = 'No mapping problems found.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object TntLabel2: TTntLabel
              Left = 30
              Top = 25
              Width = 205
              Height = 13
              Caption = 'Use the Filter to browse all mapped objects.'
            end
            object TntImage1: TTntImage
              Left = 14
              Top = 10
              Width = 13
              Height = 14
              AutoSize = True
              Picture.Data = {
                0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000000D00
                00000E0806000000F47F96D20000000467414D410000AFC837058AE900000019
                74455874536F6674776172650041646F626520496D616765526561647971C965
                3C000002414944415478DA95925F68925118C69FAF4DE52341AD8B9461120EC3
                400883162CE76EC2829440896E82F2262FA65237465462334DA82033A8BC3283
                680B13C26A5DCC260D37627CD4122B28576A23C4546633FF773E096974D58197
                73CECBF37BCF73CE79A96EB78BFF1DD4DF90CD66A3C97490C49E3FA92512CFFD
                7E7F8DD2C7C4643D44A2D087087096C3E178954A25A452692F97CD66914EA7B1
                BA36E87DB03A768CA4B6A3D31AED410470C964B28B66B3192291688395EF851F
                D8777A162B1562A25D77E399D14D59ADD65D028120E57038C0E7F3FFF17FFCFA
                1BDC8FE720A1D720FB765BBF303FF78A856E9A4CA609856A0495F516760E6DEE
                03DEE94F3877EF3D68EE26DC30014FA7EECE44A3D1932CF4D9E974EE4866DAD0
                BB5E4329E5237975142FDF1561BCBC8476A78B6B2786715845C1E3F15442A1D0
                080B957D3E9F60FEE34F1C38BFD83B615CB5158B1FCAA835DA642DC2F41905EA
                F53A5C2E5733180CEE672186DC67B7442281E5D632822F727D7B22FE20E62EA9
                201672502A95E076BB2BE170F8100B4D68341AF65E58AFFD82F10A8319A6D483
                EE9C1AC691BD22F0783CC462310402818564326961A18156AB356BB7DBC7E472
                398AE52AC2F13CF83C8A005BC0E57291CBE5586BD548247281D49AEAFD93C562
                394AD3F4439D4E07AD568B66B38946A3814EA7035219F1781C8944E209C33093
                047ADBEF0883C1A0100A8593E4CF8C62B1788005F2F97C3793C9AC10F1A342A1
                F098C89689BEBAA1F7288A627B6F9B5AAD1E27AF254CA5525FC8BE48E22BDB1C
                445B6375BF010AE7007A2C9631B50000000049454E44AE426082}
            end
          end
        end
        object LogOptionPnl: TTntPanel
          Left = 0
          Top = 244
          Width = 757
          Height = 254
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          Visible = False
          DesignSize = (
            757
            254)
          object LogOptionsGBox: TTntGroupBox
            Left = 16
            Top = 12
            Width = 733
            Height = 241
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = 'SQL Create Script of Selected Object'
            TabOrder = 0
            DesignSize = (
              733
              241)
            object SqlUCE: TUniCodeEdit
              Left = 16
              Top = 20
              Width = 585
              Height = 207
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight, akBottom]
              CharWidth = 7
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Bitstream Vera Sans Mono'
              Font.Pitch = fpFixed
              Font.Style = []
              GutterColor = clBtnFace
              GutterWidth = 0
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
              RightMargin = -1
              ScrollHintColor.Background = clAppWorkSpace
              ScrollHintColor.Foreground = clInfoText
              ScrollHintColor.FontStyles = []
              ScrollHintColor.ForceFontStyles = False
              SelectedColor.Background = clGray
              SelectedColor.Foreground = clBlack
              SelectedColor.FontStyles = []
              SelectedColor.ForceFontStyles = False
              TabOrder = 0
              WorkWidth = 581
            end
            object ApplyChangesBtn: TTntButton
              Left = 618
              Top = 164
              Width = 99
              Height = 25
              Anchors = [akRight, akBottom]
              Caption = 'Apply Changes'
              Enabled = False
              TabOrder = 1
              OnClick = ApplyChangesBtnClick
            end
            object DiscardChangesBtn: TTntButton
              Left = 618
              Top = 202
              Width = 99
              Height = 25
              Anchors = [akRight, akBottom]
              Caption = 'Discard Changes'
              Enabled = False
              TabOrder = 2
              OnClick = DiscardChangesBtnClick
            end
            object CommentOutCBox: TTntCheckBox
              Left = 618
              Top = 18
              Width = 97
              Height = 17
              Anchors = [akTop, akRight]
              Caption = 'Comment out'
              TabOrder = 3
              OnClick = CommentOutCBoxClick
            end
          end
          object DragPnl: TTntPanel
            Left = 0
            Top = 0
            Width = 757
            Height = 11
            Cursor = crSizeNS
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = DragPnlMouseDown
            OnMouseMove = DragPnlMouseMove
          end
        end
      end
    end
  end
  object SchemaListViewImgList: TImageList
    Height = 42
    Width = 42
    Left = 724
    Top = 3
    Bitmap = {
      494C01010100040004002A002A00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000A80000002A0000000100200000000000406E
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00FCFCFC00FCFCFC00FCFCFC00FEFEFE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00FDFDFD00F9F9F900F6F6F600F3F3F300F1F1F100EFEFEF00EFEF
      EF00EDEDED00EAEAEA00E9E9E900EAEAEA00EDEDED00EFEFEF00EFEFEF00F1F1
      F100F3F3F300F6F6F600FAFAFA00FDFDFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFE00FAFA
      FA00F3F3F300ECECEC00E4E4E400DEDEDE00D8D8D800D3D3D300D0D0D000BCCA
      CE008EBECD008CBCCB008CBBCA00BAC7CB00CECECE00CFCFCF00D0D0D000D3D3
      D300D8D8D800DEDEDE00E4E4E400ECECEC00F4F4F400FAFAFA00FEFEFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEFEFE00FBFBFB00F2F2F200E6E6
      E600BAD2D80065B6CE003FA7C6002599BD00158CB1001083A9000F81A7000E7F
      A5000D7EA3000C7CA2000B7AA0000A789E000A769D0009759B000E7CA3001E8A
      AF003899B9005EA9C100B2C3C900D0D0D000D9D9D900E6E6E600F2F2F200FBFB
      FB00FEFEFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEFEFE00FAFAFA00ECECEC0078C0D5002BA1
      C400168DB200158BB100148AAF003C9EBC005EAFC90065B3CB007FC3D80082C6
      DB0079C4D90071C1D90068BFD8005EBCD7004BB0CE0037A1C2002C99BB001885
      AA0007719700066F9600066F96001F89AD0070AEC200CCCCCC00DADADA00ECEC
      EC00FAFAFA00FEFEFE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FBFBFB00C0DEE8002BA4C7001892B600279A
      BD0067BCD600B0E0EE00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA0048C2E10035A1C10014799E00046C93001D88AD00A5BEC600D7D7
      D700EDEDED00FCFCFC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CBE7F000209DC2002099BD0065C5E100AFEE
      FE00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A0E9FB006DB4CD000C7197000D79A000AAC0
      C700DDDDDD00F5F5F50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE0033ACCE00229CC1007AD9F3009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF008DD1EB000B749B00268D
      B000D2D2D200F0F0F00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F2FAFC001C9ABF0060CCEA008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF003EADD700056D
      9500CBCECF00EEEEEE00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5F5FA001E9BBF007EE3FE008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005DCFFA00036B
      9100BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5F5FA00219EC2007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0060D3FE00046B
      9200BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5F5FA00219EC2007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0060D3FE00046C
      9200BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5F5FA00229FC3007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0060D3FE00046C
      9300BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F5FA00229FC4007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0060D3FE00056D
      9400BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F5FA0022A0C4007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0060D3FE00056D
      9400BAC8CD00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F5FA00219EC3007FE4FF008DE7FF009EEBFF00B1EF
      FF00C5F2FE00AEE1EF0093D0E2006CBBD30041A6C3003CA3C0001B8FB3001088
      AD000F86AB000D84A9000C82A7000B80A6001184A9001F91B4001F91B4002DA1
      C3003CB5D5004BC7E60073E1FB00A4ECFD00C6F1FF00ADE9FF005FD2FD00056E
      9500BCCBCF00EFEFEF00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F0FAFD001E9EC2006DD6F3008DE7FF0084DBF30059BE
      D900279FC2001997BB001894B90038A2C20058B2CC0066B9D00066B9D10082CA
      DE007AC7DD0071C5DC0068C2DA005EBFDA003EA9C80037A6C600289ABC001386
      AB0005779E0004769C000C7DA30049A8C500A3DBED00ADE9FF004BBCE6000670
      9600D1D7D900F4F4F40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000030ADD00035ADCF004ABCDB001E9EC20029A3
      C50062BFD900AADFEE00E4F8FD00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA005EDAF70046C1E10032A1C300107EA2000272990049A3C3001985AC001C88
      AD00E2E2E200F8F8F80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A3DCEC0022A3C60025A4C70062C7E300AEED
      FE00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB009EE8FA0068B4CE0008769C000372990085B6
      C500DDDDDD00F6F6F60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE004DBDDB002AA9CC0079DAF4009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF0087CEE80007779E003094
      B500D3D3D300F1F1F10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F7FCFD0025A8CC005FCEEC008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0039ABD4000374
      9A00CBCECF00EEEEEE00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB0026A9CC007EE3FE008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF0059CCF7000171
      9800B9C8CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002AACCE007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005BCDF8000272
      9800B9C9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002BADCF007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005BCDF8000272
      9900B9C9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002BADD0007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005BCDF8000373
      9A00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002CAED1007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005BCDF8000374
      9A00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002CAFD1007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00DCF8FF00E9FAFF00ECFBFF00E8FAFF00E2F9FE00D9F7FE00CEF5
      FE00C1F2FD00B3EFFD00A5ECFC0096E9FC0088E6FB007CE3FB0071E1FA0067DF
      FA0061DEFA005EDDFA0073E1FB00A4ECFD00C6F1FF00ADE9FF005BCEF8000374
      9B00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F6FB002DB0D2007FE4FF008DE7FF009EEBFF00B1EF
      FF00C7F3FF00CFF2FB00ADDFED0080CADE005DBAD30040ABC9001D99BC001493
      B7001291B500118EB3000F8CB0000D8AAE00128BB0002198BA002CA3C40036AF
      D00047C3E20059D7F50073E1FB00A4ECFD00C6F1FF00ADE9FF005BCEF8000475
      9C00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F7FB002DB0D3007FE4FF008DE7FF009AE9FD0078D2
      EA004CB9D6001FA3C60034ABCC005ABEDA0079CDE40093D9EE009ADDF100B8ED
      FD00B8EDFD00B6ECFD00B2EBFD00AEEAFD008FD8F00083D2EC0062BEDB0048AD
      CD001B8FB200057EA4002399BB0065C0D900BFEDFC00ADE9FF005BCEF8000476
      9C00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F7FB002DB1D3007FE4FF0064CFEC002FAFD1004FBC
      DA0083D4EB00A8E5F700B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8ED
      FD00B6ECFD00B2EBFD00AEEAFD00A9E8FD00A3E6FD009DE5FD0097E3FD0091E1
      FD008ADFFE007AD5F70056BCE0002B9CC1001787AA006ABEDA005BCEF8000577
      9D00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F7FB002DB2D40043BFDF0046BBDA009EE1F500B8ED
      FD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B6EC
      FD00B3EBFD00AEEAFD00A9E8FD00A4E7FD009EE5FD0098E3FD0091E1FD008BDF
      FE0085DDFE007EDBFE0079D9FE0073D7FE0067CCF2002595B9002095BD000577
      9E00BAC9CE00EDEDED00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E6F7FB002BB1D40051C1DE00B6ECFC00B8EDFD00B8ED
      FD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B7EDFD00B3EB
      FD00AFEAFD00AAE9FD00A4E7FD009EE5FD0098E3FD0092E1FD008BDFFE0085DD
      FE007FDBFE0079D9FE0074D7FE007FDBFE008FDEFE009CE2FE003C9FC1000378
      9E00BBCACF00EEEEEE00FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5F7FC002AB2D400ABE7F900B8EDFD00B8EDFD00B8ED
      FD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B7EDFD00B3ECFD00AFEA
      FD00AAE9FD00A5E7FD009FE5FD0099E3FD0092E1FD008CDFFE0086DDFE007FDB
      FE007CD9FE0082DBFE008CDEFE009BE3FE00AAE6FE00B7EAFF00B5E7FA00087C
      A100BECFD300F1F1F10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F9FCFE002DB4D500B3EBFC00B8EDFD00B8EDFD00B8ED
      FD00B8EDFD00B8EDFD00B8EDFD00B8EDFD00B7EDFD00B4ECFD00B0EAFD00ABE9
      FD00A5E7FD009FE5FD0099E3FD0093E1FD0090E0FE008BDEFE0089DEFE008ADD
      FE008EDFFE0098E1FE00A4E5FE00B3E9FE00C2EDFF00CDF1FF00CEF0FE000F81
      A600E0E0E000F7F7F70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003CBBDC0070CFE800B8EDFD00B8EDFD00B8ED
      FD00B8EDFD00B8EDFD00B8EDFD00B7EDFD00B4ECFD00B0EAFD00ABE9FD00A6E7
      FD00A3E6FD009FE4FD009BE3FD0098E2FE0095E1FE0092E0FE0093E1FE0095E1
      FE009BE2FE00A5E5FE00B2E9FE00C1EDFF00CFF1FF00D6F3FF007BBFD600208E
      B100F0F0F000FCFCFC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E9F8FC002EB5D7006BCDE700B5ECFC00B8ED
      FD00B8EDFD00B8EDFD00B7EDFD00B4ECFD00B0EBFD00ACE9FD00A6E7FD00A3E7
      FD00A0E6FD009DE5FD009AE3FE0097E2FE0096E2FE0095E1FE0095E1FE0099E2
      FE00A0E4FE00AAE7FE00B6EAFF00C4EEFF00CDF0FE0071BBD3000E85AA00DDE9
      EB00FBFBFB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E7F8FC003DBCDC003AB8D8007BD3
      EB00AEE9FA00B8EDFD00B5ECFD00B1EBFD00ACE9FD00A7E8FD00A1E6FD009DE5
      FD009BE4FD0098E3FE0096E2FE0093E1FE0092E0FE0093E0FE0095E0FE0098E2
      FE009EE4FE00A8E6FE00ABE4FB0078C4DD002592B4002697B900E0EDF000FCFC
      FC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4E1F2003AB9
      DA002AAFD2004DBEDB006CCBE60084D6EF0096E0F600A1E6FD009BE4FD0096E2
      FD0094E1FE0091E0FE008FE0FE008EDFFE008DDFFE008FDFFE0083D7F60071CA
      E90059B9D90039A3C500138AB0002699BC009FD5E500FBFBFB00FEFEFE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E7F7FC0086D6EB004DBFDD002FB1D30024A9CC0022A6C90020A4C7002FAB
      CE0034ACCF0031AACC002FA8CB002CA4C8001695B9001593B7001694B900209A
      BD0041ACCB0081CAE000E6F3F600FDFDFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFE
      FF00B0E3F200AFE2F000B0E2F000FDFDFD00FEFEFE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      28000000A80000002A0000000100010000000000F00300000000000000000000
      000000000000000000000000FFFFFF00FFFFF07FFFC000000000000000000000
      0000000000000000FFF00000FFC0000000000000000000000000000000000000
      FFC000001FC0000000000000000000000000000000000000FF00000007C00000
      00000000000000000000000000000000FE00000003C000000000000000000000
      0000000000000000FE00000003C0000000000000000000000000000000000000
      FE00000003C0000000000000000000000000000000000000FC00000003C00000
      00000000000000000000000000000000FC00000001C000000000000000000000
      0000000000000000FC00000001C0000000000000000000000000000000000000
      FC00000001C0000000000000000000000000000000000000FC00000001C00000
      00000000000000000000000000000000FC00000001C000000000000000000000
      0000000000000000FC00000001C0000000000000000000000000000000000000
      FC00000001C0000000000000000000000000000000000000FC00000001C00000
      00000000000000000000000000000000FC00000003C000000000000000000000
      0000000000000000FE00000003C0000000000000000000000000000000000000
      FE00000003C0000000000000000000000000000000000000FC00000003C00000
      00000000000000000000000000000000FC00000001C000000000000000000000
      0000000000000000FC00000001C0000000000000000000000000000000000000
      FC00000001C0000000000000000000000000000000000000FC00000001C00000
      00000000000000000000000000000000FC00000001C000000000000000000000
      0000000000000000FC00000001C0000000000000000000000000000000000000
      FC00000001C0000000000000000000000000000000000000FC00000001C00000
      00000000000000000000000000000000FC00000001C000000000000000000000
      0000000000000000FC00000001C0000000000000000000000000000000000000
      FC00000001C0000000000000000000000000000000000000FC00000001C00000
      00000000000000000000000000000000FC00000003C000000000000000000000
      0000000000000000FC00000003C0000000000000000000000000000000000000
      FE00000003C0000000000000000000000000000000000000FE00000007C00000
      00000000000000000000000000000000FF0000000FC000000000000000000000
      0000000000000000FFC000001FC0000000000000000000000000000000000000
      FFF00000FFC0000000000000000000000000000000000000FFFFE07FFFC00000
      00000000000000000000000000000000FFFFFFFFFFC000000000000000000000
      0000000000000000FFFFFFFFFFC0000000000000000000000000000000000000
      00000000000000000000000000000000000000000000}
  end
  object SmallSchemaListViewImgList: TImageList
    Left = 688
    Top = 3
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00FEFEFE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEFEFE00F7EDEA00E2D8D400DCD4D000DDD5D100DDD5D100DDD6D200DDD4
      D000D9D3D100E8E8E800FCFCFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9F4F30087C7DF003B7A96002E67830030698500336A8500376882003A73
      8F007A9DAE00BAB7B600E6E6E600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099DEF3000385B60045B7E700A3C8D60087BFD40063B6D30039A8CC000F93
      D0000A6896007C9DAC00CFCCCB00FBFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00003AB6D20041CEF900D1F8FF00FDFFFF00E4F9FD00AEEDFB006DDAF60030BE
      EB00109DDF00317C9E00CAC4C100FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000028A5C4009CEBFD00E6FFFF000000000000000000D1FCFF008EEFFF0041D9
      FF0012AFF000166A8E00D4CDCA00FBFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000037AECA0087E2F9004FBBD4004FAAC3004AA9C4003CA6C4002A9FC0001AA1
      C80042C2F200327C9900E2DCDA00FDFDFD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5DEEC0010A4CC0059D4F000C3E2EB00B6DFEA0094D8EA0068CFE7002CBF
      ED001485B1008399A200D1CFCE00FCFCFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000040BFD90048D5FB00DCF9FF0000000000F3FEFF00C5F4FF0089E8FE003FD6
      FD0017B1F0002B83A500CAC2BF00FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002CA7C500A5ECFD00E6FFFF000000000000000000D3FCFF0093F0FF0043DC
      FF0018B5F400196B8D00CCC5C200FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00002DAAC8004FD9FB004CB9D20053AAC3004FAAC4003FA7C4002AA0C00019A1
      C8001BB6F6001A6F9200CCC4C200FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000025AAC8001FACD000AFE5F0008AD4E9007DD2EA0072D0EA0061CCE80055D1
      ED00138EB70017709200CCC3C100FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004FCEE300AFE8F500EFFCFF00E2FCFF00D8F9FF00CBF6FF00B8F4FF0099F0
      FF0091D0E50039A2C300D9D0CD00FCFCFC000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CAF3FE0042CEE5007BD8E9007ED0E30088D4E30087D4E30089D2E20095DB
      E70042C2DF00AEE1F100F5F1EF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D8F7FE008EDDE90088D2E40087D3E40087D3E40087D2E2008BD9
      E700D2F5FE00FFFBF80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      000000000000000000000000FFFFFF00FC07FFFF00000000F001FFFF00000000
      F001FFFF00000000F000FFFF00000000F000FFFF00000000F180FFFF00000000
      F000FFFF00000000F000FFFF00000000F100FFFF00000000F180FFFF00000000
      F000FFFF00000000F000FFFF00000000F000FFFF00000000F001FFFF00000000
      F803FFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object TreeImageList: TImageList
    Height = 8
    Width = 8
    Left = 702
    Top = 91
    Bitmap = {
      494C010101000400040008000800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000200000000800000001002000000000000004
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      2800000020000000080000000100010000000000200000000000000000000000
      000000000000000000000000FFFFFF00FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      000000000000}
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
    Left = 460
    Top = 312
  end
end
