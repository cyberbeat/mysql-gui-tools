object SchemataFrame: TSchemataFrame
  Left = 0
  Top = 0
  Width = 179
  Height = 314
  TabOrder = 0
  TabStop = True
  object CatalogVST: TVirtualStringTree
    Left = 0
    Top = 45
    Width = 179
    Height = 269
    Align = alClient
    ButtonStyle = bsTriangle
    DragType = dtVCL
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Indent = 14
    PopupMenu = SchemaTreeViewPopupMenu
    TabOrder = 3
    TreeOptions.PaintOptions = [toShowRoot, toThemeAware, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toRightClickSelect]
    TreeOptions.StringOptions = [toAutoAcceptEditChange]
    OnAfterCellPaint = CatalogVSTAfterCellPaint
    OnChange = CatalogVSTChange
    OnCompareNodes = CatalogVSTCompareNodes
    OnExpanding = CatalogVSTExpanding
    OnGetText = CatalogVSTGetText
    OnPaintText = CatalogVSTPaintText
    OnGetImageIndex = CatalogVSTGetImageIndex
    OnMouseDown = CatalogVSTMouseDown
    Columns = <>
    WideDefaultText = 'Fetching Data ...'
  end
  object TopPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 179
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object SchemataLbl: TTntLabel
      Left = 2
      Top = 2
      Width = 47
      Height = 13
      Caption = 'Schemata'
    end
  end
  object SpacerPnl: TTntPanel
    Left = 0
    Top = 40
    Width = 179
    Height = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  inline AdvancedEdit: TAdvancedEditFrame
    Left = 0
    Top = 17
    Width = 179
    Height = 23
    Align = alTop
    Color = clSkyBlue
    ParentColor = False
    TabOrder = 2
    TabStop = True
    ExplicitTop = 17
    ExplicitWidth = 179
    ExplicitHeight = 23
    inherited SearchEd: TTntEdit
      OnChange = AdvancedEditSearchEdChange
    end
  end
  object SchemaSearchPopupMenu: TTntPopupMenu
    Left = 62
    Top = 124
    object SearchAllMI: TTntMenuItem
      AutoCheck = True
      Caption = 'All'
      Checked = True
      Default = True
      GroupIndex = 1
      RadioItem = True
      OnClick = SearchMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object SearchSchemataMI: TTntMenuItem
      Tag = 1
      AutoCheck = True
      Caption = 'Schemata'
      GroupIndex = 1
      RadioItem = True
      OnClick = SearchMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object SearchAssetsMI: TTntMenuItem
      Tag = 2
      AutoCheck = True
      Caption = 'Schema Assets'
      GroupIndex = 1
      RadioItem = True
      OnClick = SearchMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object SearchColumnsMI: TTntMenuItem
      Tag = 4
      AutoCheck = True
      Caption = 'Columns/Indices'
      GroupIndex = 1
      RadioItem = True
      OnClick = SearchMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N3: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CustomMI: TTntMenuItem
      Caption = 'Custom Selection'
      GroupIndex = 1
      RadioItem = True
      OnClick = SearchMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
      object SchemataSubMI: TTntMenuItem
        AutoCheck = True
        Caption = 'Schemata'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object TableSubMI: TTntMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Tables'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object ColumnsSubMI: TTntMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Columns'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object IndicesSubMI: TTntMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'Indices'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object SPSubMI: TTntMenuItem
        Tag = 4
        AutoCheck = True
        Caption = 'Stored Procedures/Functions'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object ViewsSubMI: TTntMenuItem
        Tag = 5
        AutoCheck = True
        Caption = 'Views'
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
      object TriggersSubMI: TTntMenuItem
        Tag = 6
        AutoCheck = True
        Caption = 'Triggers'
        Enabled = False
        OnClick = SearchMIClick
        OnAdvancedDrawItem = MenuDrawItem
        OnMeasureItem = MenuMeasureItem
      end
    end
  end
  object SchemaTreeViewPopupMenu: TTntPopupMenu
    OnPopup = SchemaTreeViewPopupMenuPopup
    Left = 96
    Top = 124
    object EditMI: TTntMenuItem
      Caption = 'Edit'
      ShortCut = 113
      OnClick = EditMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object RenameMI: TTntMenuItem
      Caption = 'Rename'
      ShortCut = 16466
      Visible = False
      OnClick = RenameMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object DropMI: TTntMenuItem
      Caption = 'Drop'
      ShortCut = 16430
      OnClick = DropMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CopyAssetSQLMI: TTntMenuItem
      Caption = 'Copy CREATE statement to Clipboard'
      ShortCut = 16451
      OnClick = CopyAssetSQLMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N1: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CreateNewSchemaMI: TTntMenuItem
      Caption = 'Create New Schema'
      ShortCut = 16462
      OnClick = CreateNewSchemaMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CreateNewTableMI: TTntMenuItem
      Caption = 'Create New Table'
      ShortCut = 16468
      OnClick = CreateNewTableMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CreateNewViewMI: TTntMenuItem
      Caption = 'Create New View'
      ShortCut = 16470
      OnClick = CreateNewViewMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CreateNewStoredProcedureMI: TTntMenuItem
      Caption = 'Create New Procedure / Function'
      ShortCut = 16464
      OnClick = CreateNewStoredProcedureMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N2: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object RefreshCatalogsSchemataListMI: TTntMenuItem
      Caption = 'Refresh'
      ShortCut = 116
      OnClick = RefreshCatalogsSchemataListMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object SchemataAssetsImageList: TImageList
    BkColor = clWhite
    Left = 108
    Top = 86
    Bitmap = {
      494C01010A000E00040010001000FFFFFF00FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00E7E7E700CECECE00CECECE00EFEFEF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CED6
      D600ADADAD00ADADB500ADADB500ADADB500ADADB500ADADB500ADADB500ADAD
      B500ADADB500ADADAD00D6D6D600FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5B5B5007373730073737300C6C6C600FFFFFF00FFFF
      FF00F7F7F700CECECE00DEDEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B594
      8C00A5847B00A5847B00A5847B00A5847B00A5847B00A5847B00A5847B00A584
      7B00A5847B009C84730094949400F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E7E7E700CECECE00DEDEDE0073737300C6C6C600FFFFFF00FFFF
      FF00B5B5B5006B6B6B00ADADAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFCE
      AD00FFE7C600FFDEBD00FFDEBD00FFDEB500FFDEB500FFDEAD00FFD6AD00FFD6
      AD00FFDEAD00CEA58C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E7E7E700BDBDBD00CECECE0084848400BDBDBD00FFFFFF00EFEF
      EF006B9CB50052849400A5A5A500FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFD6
      B500FFE7C600FFDEBD00FFDEBD00FFDEB500FFDEB500FFD6AD00FFD6AD00FFD6
      AD00FFDEAD00CEA58C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00DEDEDE00C6C6C600BDBDBD0073737300C6C6C600FFFFFF00A5B5
      C6007BBDD60084DEF700ADB5B500FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFD6
      BD00FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEBD00FFDEB500FFDEB500FFD6
      AD00FFDEB500CEA58C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7F700BDBDBD00B5B5B500B5B5B50073737300CECECE00DEDED60073A5
      C60073CEFF004AA5CE00D6D6D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFD6
      C600FFEFD600FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEBD00FFDEB500FFDE
      B500FFDEB500CEA58C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00CECE
      CE00A5A5A5009C9C9C00A5A5A500A59C9C0073736B009C9494008494A5007BC6
      EF0039ADEF00739CAD00F7EFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7DE
      CE00FFEFDE00FFEFD600FFE7CE00FFE7CE00FFE7C600FFDEC600FFDEBD00FFDE
      BD00FFE7BD00CEA594008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00E7E7E7008C8C
      8C0052525200B5B5B5009C9C9C007B94A5004A5A6B0063737B005AADD60042AD
      EF0042A5D600ADBDC600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7DE
      CE00FFF7DE00FFEFD600FFEFD600FFE7CE00FFE7CE00FFE7C600FFE7C600FFDE
      BD00FFE7BD00D6A594008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00D6D6D600B5B5
      B500B5B5B500BDB5AD007BADC60052C6FF004AA5D600529CBD005ACEFF00188C
      E700529CBD00CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7E7
      D600FFF7E700FFEFDE00FFEFDE00FFEFD600FFEFD600FFE7CE00FFE7CE00FFE7
      C600FFE7C600D6AD94008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00CECECE00BDBD
      BD00DED6D600DEE7EF006BCEF7004AB5FF0073DEFF007BEFFF00319CE700298C
      D600637B8400BDB5B500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7E7
      DE00FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEFD600FFEFD600FFE7CE00FFE7
      CE00FFEFCE00D6AD9C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00D6D6D600BDBD
      BD00A59C940084C6DE0063D6FF003194FF005AA5C6008CEFFF0042ADFF002984
      CE00525A6300ADADAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7EF
      DE00FFFFF700FFF7EF00FFF7E700FFF7E700FFEFDE00FFEFDE00FFEFD600FFE7
      D600FFEFD600D6AD9C008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00DEDEDE00CEC6
      C60084949C009CE7F7006BC6FF00218CEF00847B84007B949C006BBDEF004ABD
      FF00428CB500C6BDBD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFEF
      E700FFFFF700FFF7EF00FFF7EF00FFF7E700FFF7E700FFEFDE00FFEFDE00FFEF
      DE00FFF7DE00D6B5A5008C8C8C00F7F7F7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00CECECE00EFEF
      EF00C6CED600ADD6F700CEEFFF007BCEFF006BB5DE00A5847B00397BBD0042B5
      FF006B9CB500F7EFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFEF
      E700FFFFFF00FFFFF700FFF7EF00FFF7EF00FFF7EF00FFF7E700FFF7E700F7D6
      C600EFBDAD00C6948C0094949400FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00BDA59400948C
      8C00B5B5B500C6C6C600ADCEE700B5EFFF00A5E7EF00AD948C004A8CCE003194
      E700BDC6C600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      EF00FFFFFF00FFFFF700FFFFF700FFFFF700FFF7EF00FFF7EF00FFF7EF00DEB5
      9C00F79C2900BD845200C6C6CE00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00D6CEBD00A594
      84008C848400847B7B00847B7B007B848C00A59C9C009CB5B5004ABDFF007BAD
      D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFFFF700EFC6
      AD00D6A56B00C6C6BD00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00F7FFFF00DEDE
      D600C6BDB500B5B5B500B5ADAD00BDB5B500DED6CE00DEF7FF00CEEFFF00F7FF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFE7
      D600F7E7DE00F7E7D600EFE7D600EFDED600EFDECE00EFD6CE00E7D6CE00D6BD
      AD00DED6D600F7FFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F7F7F700EFEFEF00F7F7F700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B5C6CE008CA5AD00B5B5
      B500D6D6D600E7DEDE00EFEFEF00FFF7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CECECE00BDC6C600BDC6
      C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6
      C600BDC6C600BDC6C600CECECE00EFEFEF00FFFFFF00FFFFFF00F7F7F700EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00E7E7E700CEC6B500EFEFEF00EFEFEF00EFEF
      EF00F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7F7F700D6D6D600BDBDBD00D6D6D600F7F7F700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B5E7EF00299CC6001084A5003184
      9C0042849C006B8C9C00A5ADAD00B5B5BD00D6D6CE00E7E7DE00EFEFEF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CEC6BD00947B73007B6B63008473
      6B0084736B0084736B0084736B0084736B0084736B0084736B0084736B008473
      6B0084736B0084736B00847B7B00CECECE00FFFFFF00F7F7F7008C7329007B5A
      080073520800735A0800735A10006B52000073843100735A0800735A1000735A
      180084733900C6BD9C00F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00E7E7E700ADADAD008C8C8C00A5A5A500D6D6D600F7F7
      F700FFFFFF00FFFFFF00FFFFFF00FFFFFF0094CEE7009CE7F7006BDEFF004ACE
      F70042B5DE0031A5C600188CB500297B9C00528C9C005A8C9C009CA5A500C6C6
      C600E7E7E700FFFFFF00FFFFFF00FFFFFF00CE9C7B00E7BDA500E7BDA500E7BD
      A500E7BD9C00E7BD9C00E7BD9C00E7B59C00EFBD9C00E7AD8C00DE9C7B00DE9C
      7300DEA58400E7A5840084736B00BDC6C600FFFFFF00EFEFEF00846308009C73
      08009C7B0800A58421008C6B180084944A0073CE73006B5A08008C6B08008463
      0800735A0800634A0800EFEFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DEDEDE00000000007B7B7B008C8C8C00BDBDBD00EFEF
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0039B5E700A5E7F7008CE7FF007BE7
      FF0084E7FF007BE7FF0073E7FF0063D6F70042C6E70039ADCE00319CBD00398C
      A500ADADAD00EFE7E700FFFFFF00FFFFFF00D6AD9400FFF7E700FFEFDE00FFEF
      D600FFE7CE00FFE7CE00FFE7C600FFE7CE00B54A0800BD5A2100B54A0800B54A
      0800EFB58C00EFB5940084736B00BDC6C600FFFFFF00EFEFEF00846B10009C73
      0800AD841800AD8C31006B5A0800BDDEB50084D684008C8442008C6B10009C73
      08008463080073521000EFEFEF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0018181800CECECE00000000008C8C8C00BDBDBD00EFEF
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0029B5E70084D6EF00ADF7FF007BEF
      FF0084EFFF0084EFFF0084EFFF0084EFFF008CEFFF0084F7FF0084EFFF0031BD
      E70042849C00CECEC600FFFFFF00FFFFFF00DEB59400FFEFE700FFEFDE00FFE7
      D600FFE7CE00FFDECE00FFDEC600FFEFD600B54A0800F7E7DE00F7E7DE00B54A
      0800EFAD8400EFB59400847B7300BDC6C600FFFFFF00F7F7F700AD9C63008C6B
      0800AD8418008C7321009C945A00DEF7DE009CDE9C00B5AD84007B6308009C7B
      0800846308008C733100F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0021212100CECECE000084840000000000BDBDBD00EFEF
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0039B5E70063C6EF00BDF7FF008CF7
      FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF008CEFFF007BDEEF0073DE
      F7002994B50094A5A500EFE7E700FFFFFF00DEB59C00FFF7E700FFEFDE00FFE7
      D600FFE7D600FFE7CE00FFDEC600FFEFD600B54A0800F7E7DE00F7E7DE00B552
      1000F7BD9400EFBD94008C7B7300BDC6C600FFFFFF00FFFFFF00F7F7F700A594
      5A008C6B100073631800F7F7EF00ADCEB500B5DEB500BDB58C007B630800946B
      0800846B1800E7DEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00F7F7F70031313100CECECE00080808008C8C8C00BDBDBD00EFEF
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004ABDE70052C6F700ADE7F700A5FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094F7FF008CE7DE0073CECE008CD6
      E7005ABDD600427B9400CEC6C600F7F7F700E7BD9C00FFF7EF00FFEFE700FFEF
      DE00FFEFDE00FFE7D600FFE7CE00FFEFD600B54A0800B54A0800B54A0800B54A
      0800FFD6B500F7C69C008C7B7300BDC6C600FFFFFF00FFFFFF00FFFFFF00F7F7
      F700C6BDA5005A634A0031527300294A630029425A00424A3900846B1800AD9C
      6B00EFEFE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7F700D6D6D60039393900D6D6D6001894940008080800A5A5A500D6D6
      D600F7F7F700FFFFFF00FFFFFF00FFFFFF0042BDE70063CEFF0084CEF700CEFF
      FF00ADFFFF00ADFFFF00A5FFFF009CFFFF0094E7DE008CD6C6008CD6C6008CC6
      CE009CD6DE00217B9C008C9CA500EFE7E700EFC6A500FFF7EF00FFEFE700FFEF
      DE00FFE7D600FFE7D600FFE7CE00FFE7CE00FFEFD6008C8C7B00D6AD8C00FFCE
      AD00FFDEBD00FFCEA5008C7B7300BDC6C600FFFFFF00FFFFFF00FFFFFF00F7F7
      F7009CADAD00638CC6006B9CDE0073A5E7006394CE00426384006B736B00EFEF
      EF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00E7E7E700ADADAD0042424200D6D6D600212121007B7B7B0084848400ADAD
      AD00E7E7E700FFFFFF00FFFFFF00FFFFFF004ABDE70084E7FF005ACEF7008CD6
      F700ADE7F700B5EFF700CEF7FF00B5E7DE009CD6C600BDE7CE00BDEFD600A5CE
      C600C6D6CE00529CB50042738400C6C6C600EFCEA500FFF7F700FFF7EF00F7DE
      CE00EFC6AD00EFC6AD00EFC6AD00F7D6BD00FFE7B5004A529C00CE947300EFB5
      8C00F7CEAD00FFCEA5008C847300BDBDC600FFFFFF00FFFFFF00FFFFFF00DEE7
      DE0084ADDE007BADE70084B5EF0084B5EF0084B5EF00739CCE004A638400D6D6
      D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00DEDEDE005A5A5A0052ADAD00D6D6D600319C9C00212121007B7B7B009C9C
      9C00DEDEDE00FFFFFF00FFFFFF00FFFFFF004AC6E7008CEFFF007BE7FF006BDE
      FF006BDEFF006BD6F70063ADBD00ADC6B500E7EFD600E7F7D600CEEFD600C6E7
      D600E7DECE00A5C6C600186B8C00949C9C00F7CEAD00FFFFFF00AD4A0800B552
      2100B5521800BD632100E7AD8C00FFCE9C00736BAD00F7E7DE004A52B500DE9C
      7300EFBD9C00FFD6AD0094847300BDBDC600FFFFFF00FFFFFF00FFFFFF00B5BD
      B50084B5E7008CC6F7008CC6F7008CC6F7008CC6F7008CC6F7005A84A500A5AD
      9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF006B6B6B00DEDEDE00DEDEDE00DEDEDE0042A5A500319C9C0029292900ADAD
      AD00E7E7E700FFFFFF00FFFFFF00FFFFFF004AC6E7009CFFFF008CFFFF0094FF
      FF0094FFFF007BCECE0073736300C6D6BD00D6F7DE00D6EFCE00CEEFBD00C6DE
      BD00C6DEC600ADCEBD0029738C008CA5AD00F7D6AD00FFFFFF00AD4A0800F7E7
      DE00F7E7DE00A54A10007B7363004A4A8C00F7E7DE00F7E7DE00F7E7DE004A4A
      B500FFD6AD00FFD6AD0094847300BDBDC600FFFFFF00FFFFFF00F7F7F700A5B5
      AD008CBDE70094CEF70094CEF70094CEF70094CEF70094CEF700739CBD009CA5
      9400F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B007BFFFF00DEDEDE006363630052ADAD004AA5A50039393900D6D6
      D600F7F7F700FFFFFF00FFFFFF00FFFFFF0052C6E700ADFFFF0094FFFF0094FF
      FF009CFFFF00A5E7E7007B848400C6C6B500FFFFFF00FFFFEF00F7FFB500D6E7
      7B00A5BD6B0052734A006B848400DEE7E700FFD6AD00FFFFFF00AD4A0800F7E7
      DE00F7E7DE00AD4A0800EFCEB500FFF7D6007384CE00F7E7DE005263C600EFCE
      B500FFDEC600FFDEAD0094847300BDBDC600FFFFFF00FFFFFF00F7F7F700A5B5
      AD007BB5D60094CEF70094CEF70094CEF70094CEF70094CEF70073A5C600ADB5
      AD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008C8C8C0084FFFF0084FFFF00E7E7E700DEDEDE00DEDEDE0052525200F7F7
      F700FFFFFF00FFFFFF00FFFFFF00FFFFFF0084D6EF00A5F7FF00BDFFFF00B5FF
      FF00C6FFFF0084D6EF0073BDCE006B737B00C6C6BD00FFFFFF00FFFFE700FFFF
      94008C8C390094947B00FFFFFF00FFFFFF00FFDEAD00FFFFFF00AD4A0800AD4A
      0800AD4A0800AD4A0800FFF7EF00FFF7EF00FFFFEF007384D600EFDED600FFEF
      D600FFEFD600FFE7B50094847300BDBDC600FFFFFF00FFFFFF00FFFFFF00B5BD
      BD0018739C00217BA5005A9CCE0084BDEF008CC6F70073B5E7005A94BD00DEDE
      D600FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00949494008C8C8C0084848400737373006B6B6B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6EFF7006BC6E70063C6E7005AC6
      E7006BC6E700ADD6E700E7F7FF00CEDEDE007B7B7B00C6C6C600FFFFEF009C9C
      730094948400FFFFFF00FFFFFF00FFFFFF00F7AD8C00F7BDAD00F7C6B500FFCE
      C600FFCEBD00FFC6BD00F7B5A500F7B5A500F7B59C00FFC69C00F7B59C00F7AD
      9400F7AD9400FFAD8C008C736B00BDBDC600FFFFFF00FFFFFF00FFFFFF00EFEF
      EF00297B9C002984B5004A94BD00217BA50018739C00217BA5005A94A500F7F7
      F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7FFFF00F7FFFF00F7FF
      FF00F7FFFF00FFFFFF00FFFFFF00FFFFFF00E7E7E700848484008C8C84009494
      8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DE634A00DE524200DE524200DE52
      4200DE524200DE5A4200DE5A4200DE5A4200DE5A4200DE5A4200DE5A4200DE5A
      4200DE5A4200E75242009C737300CECED600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00ADBDBD0021739C00398CB500529CBD003184AD003984A500DEE7E700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEDEDE009C9C9C00F7F7
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DE4A4200D6423900D6423900D642
      3900D6423900D6423900D6423900D6423900D6423900D6423900D6423900D642
      3900D6423900D6312900D6ADA500EFF7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00F7F7F700CED6D6006B949C0052849C008CA5AD00EFEFEF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7E7EF00B5B5B500ADADB500ADB5
      B500ADB5B500ADADB500B5B5B500E7E7E700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFEFEF00C6C6C600ADADAD00ADAD
      AD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADADAD00ADAD
      AD00B5B5B500D6D6D600F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00F7EFEF00E7DED600DED6D600DED6D600DED6D600DED6D600DED6
      D600DED6D600EFEFEF00FFFFFF00FFFFFF00FFFFFF00EFEFEF00DEDEDE00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00C6A59400AD8C7300A5847300A584
      7300A5847300AD8C7300947B6B00B5B5B500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6948400BD523900BD523900BD52
      3900BD523900BD523900BD523900BD523900BD523900BD523900BD523900BD52
      3900B5634A00ADADAD00E7E7E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFF7F70084C6DE00397B940029638400316B8400316B8400316B84003973
      8C007B9CAD00BDB5B500E7E7E700FFFFFF00FFFFFF00AD9C940084736B008473
      6B0084736B0084736B0084736B0084736B00D6B58C00EFF7A500E7E78C00DEDE
      7B00DEDE7300E7DE7300AD8C7300ADADB500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00F7F7F700DEDEDE00E7E7E700FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BD523900C65A3900FFB57B00FF9C
      4A00FF943900FF943900FF943900FF943900FF943900FF943900FF943900FF94
      4200BD5A390094848400CECECE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF009CDEF7000084B50042B5E700A5CED60084BDD60063B5D60039ADCE000894
      D600086B94007B9CAD00CECECE00FFFFFF00FFFFFF00DEB59C00E7C6AD00DEB5
      9C00DEBD9C00EFC6AD00E7BD9C00DEAD9400E7C69C00DECE8C00CEAD6300C6AD
      5A00BDA55200CEBD6300B5947B00ADADB500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00BDBDBD007B7B7300A5A5AD00E7E7E700FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BD523900CE633900EFA57B00FFAD
      6B00FF9C5200FF9C5200FF9C5200FF9C5200FF9C5200FF9C5200FF9C5200FFA5
      5A00C6634200A5736300B5B5B500EFEFEF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0039B5D60042CEFF00D6FFFF00FFFFFF00E7FFFF00ADEFFF006BDEF70031BD
      EF00109CDE00317B9C00CEC6C600FFFFFF00FFFFFF00DEB59C00EFCEB500E7BD
      A500E7C6AD00FFDECE00EFBD9C00DEB59400EFCEA500EFE7AD00E7D68C00E7CE
      7B00DEC66B00D6CE6B00B59C7B00ADADB500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD006B6B42005A5A290063635A00A5A5AD00E7E7
      E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00BD523900D66B3900E7947300FFBD
      9400FFAD7300FFAD7300FFAD7300FFAD7300FFAD7300FFAD7300FFAD7300FFB5
      7B00D6846300BD634A009C9C9C00DEDEDE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0029A5C6009CEFFF00E7FFFF00FFFFFF00FFFFFF00D6FFFF008CEFFF0042DE
      FF0010ADF700106B8C00D6CECE00FFFFFF00FFFFFF00DEB59C00F7D6BD00EFC6
      AD00E7C6AD00FFE7CE00EFC6A500E7B59400F7D6A500EFE7B500EFD69400E7D6
      8400DEC66B00D6CE7300BDA58400ADADB500FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00BDBDBD00636342008C8C4200B5B55200636331005A5A5A00A5A5
      AD00E7E7E700FFFFFF00FFFFFF00FFFFFF00BD523900DE7B4A00D6846300FFCE
      AD00FFBD8C00FFBD8C00FFBD8C00FFBD8C00FFBD8C00FFBD8C00FFBD8C00FFBD
      8C00DE9C8400CE7B6300B59C9400DEDEDE00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0031ADCE0084E7FF004ABDD6004AADC6004AADC60039A5C600299CC60018A5
      CE0042C6F700317B9C00E7DEDE00FFFFFF00FFFFFF00DEB59C00F7DEC600EFCE
      B500E7CEB500FFE7D600F7CEAD00E7BD9C00F7DEAD00EFE7B500E7D69C00E7D6
      9400DEC67B00DEDE7B00C6AD8400ADADAD00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C6006B6B42009C9C4200D6D66300D6D66300BDBD5A00636331005A5A
      5A00ADADAD00EFEFEF00FFFFFF00FFFFFF00BD523900E7945A00CE735200FFDE
      BD00FFC69C00FFC69C00FFC69C00FFC69C00FFC69C00FFC69C00FFC69C00FFC6
      A500DEA58C00DEADA500CE948400F7F7F700FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00A5DEEF0010A5CE005AD6F700C6E7EF00B5DEEF0094DEEF006BCEE70029BD
      EF001084B500849CA500D6CECE00FFFFFF00FFFFFF00DEB59C00F7DECE00EFCE
      B500E7CEBD00FFE7D600F7CEB500E7BDA500E7846300E78C7300E78C7300E78C
      7300E78C6B00EF8C6300C67B6300BDC6C600FFFFFF00FFFFFF00FFFFFF00E7E7
      E70084847300B5B58400FFFFAD00F7F77B00D6D66300CECE6300BDBD5A006363
      31006B6B6B00E7E7E700FFFFFF00FFFFFF00BD523900EFA57300C6634200FFE7
      CE00FFE7CE00FFE7CE00FFE7CE00FFE7CE00FFE7CE00FFE7CE00FFE7CE00FFE7
      CE00EFC6BD00EFE7DE00BD5A3900FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0042BDDE004AD6FF00DEFFFF00FFFFFF00F7FFFF00C6F7FF008CEFFF0039D6
      FF0010B5F7002984A500CEC6BD00FFFFFF00FFFFFF00DEB59C00EFDECE00EFCE
      B500E7CEBD00FFEFDE00EFCEB500E7BDA500DE6B6300E76B6300E76B6300DE6B
      5A00DE635200C64A4200D68C8C00F7FFFF00FFFFFF00FFFFFF00FFFFFF00CECE
      CE007B7B7300DEDECE00FFFFF700FFFFDE00FFFF9C00DEDE6B00D6D663008C8C
      39006B6B5200DEDEDE00FFFFFF00FFFFFF00BD523900EFB58C00D67B5A00BD52
      3900BD523900BD523900BD523900BD523900BD523900BD523900BD523900BD52
      3900BD523900C6634200DEA59C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0029A5C600A5EFFF00E7FFFF00FFFFFF00FFFFFF00D6FFFF0094F7FF0042DE
      FF0018B5F700186B8C00CEC6C600FFFFFF00FFFFFF00DEB59C00EFDECE00EFCE
      BD00E7CEBD00FFEFDE00EFCEB500E7BDA500F7EFD600F7DEBD00EFC6A500E7BD
      9C00F7D6AD00948C7B00D6E7E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00C6C6C60084847B00E7E7DE00FFFFFF00FFFFDE00FFFF8C00A5A542007373
      5200E7E7E700FFFFFF00FFFFFF00FFFFFF00BD523900EFBD9C00EFBD9C00EFBD
      9C00EFBD9C00EFBD9C00EFCEAD00FFF7E700FFF7E700FFF7E700FFF7E700F7E7
      DE00C6634200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0029ADCE004ADEFF004ABDD60052ADC6004AADC60039A5C60029A5C60018A5
      CE0018B5F700186B9400CEC6C600FFFFFF00FFFFFF00DEB59C00F7DED600EFD6
      C600E7D6C600FFEFE700F7D6BD00E7C6AD00F7DECE00F7D6BD00EFC6A500E7BD
      9C00F7CEAD009C8C7B00D6DEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C6C6C60084848400E7E7E700FFFFF700BDBD8C007B7B5200E7E7
      E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00BD523900F7DECE00EFC6A500EFC6
      A500EFC6A500EFC6A500E7C6B500BD523900BD523900BD523900BD523900C663
      4200DEA59400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0021ADCE0018ADD600ADE7F7008CD6EF007BD6EF0073D6EF0063CEEF0052D6
      EF00108CB50010739400CEC6C600FFFFFF00FFFFFF00DEB59C00F7E7D600EFD6
      C600E7D6C600FFEFE700F7D6C600E7C6B500F7E7D600F7D6BD00EFC6AD00E7BD
      A500FFD6AD009C8C7B00D6DEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00BDBDBD0084848400B5B5A5007B7B7300E7E7DE00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DE9C8C00DEA59400FFF7EF00FFF7
      EF00FFF7EF00FFF7EF00CE735A00EFCEC600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004ACEE700ADEFF700EFFFFF00E7FFFF00DEFFFF00CEF7FF00BDF7FF009CF7
      FF0094D6E70039A5C600DED6CE00FFFFFF00FFFFFF00DEB59C00F7E7DE00EFDE
      CE00EFE7D600FFFFFF00EFDECE00EFD6BD00FFF7E700F7DECE00EFCEB500EFCE
      AD00FFDEBD009C8C7B00D6DEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00BDBDBD007B7B7B00DEDEDE00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7EF00DE9C8C00BD523900BD52
      3900BD523900BD523900DE9C8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CEF7FF0042CEE7007BDEEF007BD6E7008CD6E70084D6E7008CD6E70094DE
      E70042C6DE00ADE7F700F7F7EF00FFFFFF00FFFFFF00EF8C7300EFA59400EFA5
      9400EFA58C00EF9C8C00EFA58C00EF9C8C00EF9C8400EF9C8400EF9C8C00EF9C
      8400F794730094736B00D6DEDE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFEFEF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00DEF7FF008CDEEF008CD6E70084D6E70084D6E70084D6E7008CDE
      E700D6F7FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D6291800D6312900D631
      2900D6312900D6392900D6392900D6392900D6392900D6392900D6392900D639
      2900DE312100B5848400EFF7F700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00F839E00000000000F830E00000000000
      F820E00000000000F820E00000000000F800E00000000000E001E00000000000
      C001E00000000000C001E000000000008003E000000000008001E00000000000
      8001E000000000008003E000000000008003E000000000008007E00000000000
      8007EC0100000000C00FE00300000000FE3F800080008001FC1F000000008001
      FC0F000000008001FC0F000000008001FC0F000000008001FC0F000000008001
      F80F00000000C003F00700000000E007F00700000000C007F00700000000C007
      F00700000000C007F00700000000C007F00F00000000C007F83F00000000C00F
      FFFF00000000E00FFFFF00000000E01F8000FC07FF00FF000000F0018000FFFE
      0000F0018000FE1E0000F00080007C0E0000F000800078060000F18080007002
      0000F000800070020000F000800060020000F100800060030000F18080017007
      0000F0008001780F0000F00080017C1F0000F00080017E3F0000F00180017F7F
      0000F80380017FFF0000FFFFFFFF000300000000000000000000000000000000
      000000000000}
  end
end