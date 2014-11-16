object AdminRestoreForm: TAdminRestoreForm
  Left = 391
  Top = 148
  Caption = 'Backup / Restore'
  ClientHeight = 527
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServerBackupRestorePnl: TTntPanel
    Left = 0
    Top = 0
    Width = 689
    Height = 527
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    object BackupRestorePageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 669
      Height = 466
      ActivePage = GeneralSheet
      Align = alClient
      TabOrder = 0
      object GeneralSheet: TTabSheet
        Caption = 'General'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel6: TTntPanel
          Left = 0
          Top = 0
          Width = 661
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            661
            45)
          object BackupBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 638
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label6: TTntLabel
            Left = 42
            Top = 8
            Width = 92
            Height = 13
            Caption = 'Restore Options'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Restore2Img: TTntImage
            Left = 10
            Top = 10
            Width = 24
            Height = 24
          end
          object Label7: TTntLabel
            Left = 42
            Top = 21
            Width = 236
            Height = 13
            Caption = 'Please select how the backup should be restored.'
          end
        end
        object GroupBox1: TTntGroupBox
          Left = 10
          Top = 56
          Width = 637
          Height = 174
          Caption = 'General'
          TabOrder = 1
          object Label9: TTntLabel
            Left = 18
            Top = 24
            Width = 66
            Height = 13
            Caption = 'File to restore:'
          end
          object Label10: TTntLabel
            Left = 440
            Top = 20
            Width = 175
            Height = 38
            AutoSize = False
            Caption = 'Name of the backup file that should be restored.'
            WordWrap = True
          end
          object Label11: TTntLabel
            Left = 18
            Top = 92
            Width = 76
            Height = 13
            Caption = 'Target Schema:'
          end
          object Label13: TTntLabel
            Left = 18
            Top = 60
            Width = 67
            Height = 13
            Caption = 'Backup Type:'
          end
          object Label14: TTntLabel
            Left = 440
            Top = 60
            Width = 237
            Height = 13
            AutoSize = False
            Caption = 'Type of backup file.'
          end
          object FileToRestoreEd: TTntEdit
            Left = 116
            Top = 20
            Width = 309
            Height = 21
            TabOrder = 0
          end
          object BackupFileTypesLU: TTntComboBox
            Left = 116
            Top = 57
            Width = 165
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 2
            Text = 'SQL Files'
            Items.Strings = (
              'SQL Files')
          end
          object TargetSchemaCBox: TTntComboBox
            Left = 172
            Top = 136
            Width = 253
            Height = 21
            Enabled = False
            ItemHeight = 0
            TabOrder = 1
          end
          object OrgSchemaRadioButton: TRadioButton
            Left = 40
            Top = 115
            Width = 121
            Height = 17
            Caption = 'Original schema'
            Checked = True
            TabOrder = 3
            TabStop = True
            OnClick = SchemaRadioButtonClick
          end
          object OtherSchemaRadioButton: TRadioButton
            Left = 40
            Top = 138
            Width = 121
            Height = 17
            Caption = 'Another schema'
            TabOrder = 4
            OnClick = SchemaRadioButtonClick
          end
        end
        object GroupBox2: TTntGroupBox
          Left = 10
          Top = 332
          Width = 637
          Height = 103
          Caption = 'Character Set'
          TabOrder = 2
          object Label5: TTntLabel
            Left = 18
            Top = 20
            Width = 58
            Height = 13
            Caption = 'File Charset:'
          end
          object Label8: TTntLabel
            Left = 316
            Top = 12
            Width = 305
            Height = 85
            AutoSize = False
            Caption = 
              'If you are importing a SQL file that has not been created with M' +
              'ySQL Administrator, you have to choose the correct character set' +
              ' of the file. If you have created the backup with MySQL Administ' +
              'rator the file was written in UTF-8.'
            WordWrap = True
          end
          object CharsetCBox: TTntComboBox
            Left = 132
            Top = 20
            Width = 165
            Height = 21
            DropDownCount = 16
            ItemHeight = 13
            TabOrder = 0
            Text = 'utf8'
            Items.Strings = (
              'big5'
              'sjis'
              'cp850'
              'cp866'
              'latin1'
              'latin2'
              'latin5'
              'latin7'
              'hebrew'
              'tis620'
              'euckr'
              'gb2312'
              'greek'
              'cp1250'
              'gbk'
              'utf8'
              'macroman'
              'cp1251'
              'cp1256'
              'cp1257'
              'HP-ROMAN8'
              'KOI8-R'
              'US-ASCII'
              'EUC-JP'
              'KOI8-U'
              'ARMSCII-8'
              'UCS-2'
              'MACCENTRALEUROPE')
          end
        end
        object TntGroupBox1: TTntGroupBox
          Left = 10
          Top = 236
          Width = 637
          Height = 90
          Caption = 'Options'
          TabOrder = 3
          object ForceCheckbox: TTntCheckBox
            Left = 20
            Top = 25
            Width = 361
            Height = 17
            Caption = 'Ignore Errors'
            TabOrder = 0
          end
          object CreateDBsCheckbox: TTntCheckBox
            Left = 20
            Top = 58
            Width = 395
            Height = 13
            Caption = 'Create database(s) if they don'#39't exist'
            TabOrder = 1
          end
        end
      end
      object ContentSheet: TTabSheet
        Caption = 'Restore Content'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          661
          438)
        object Panel1: TTntPanel
          Left = 0
          Top = 0
          Width = 661
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            661
            45)
          object RestoreBevel: TTntBevel
            Left = 10
            Top = 38
            Width = 639
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label3: TTntLabel
            Left = 42
            Top = 21
            Width = 85
            Height = 13
            Caption = 'Restore a backup'
          end
          object RestoreImg: TTntImage
            Left = 10
            Top = 10
            Width = 24
            Height = 24
          end
          object Label1: TTntLabel
            Left = 42
            Top = 8
            Width = 45
            Height = 13
            Caption = 'Restore'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        object RestoreTreeView: TVirtualStringTree
          Left = 10
          Top = 61
          Width = 639
          Height = 335
          Anchors = [akLeft, akTop, akRight, akBottom]
          CheckImageKind = ckCustom
          CustomCheckImages = ApplicationDM.ItemSelectImageList
          Header.AutoSizeIndex = 0
          Header.Font.Charset = DEFAULT_CHARSET
          Header.Font.Color = clWindowText
          Header.Font.Height = -11
          Header.Font.Name = 'Tahoma'
          Header.Font.Style = []
          Header.Options = [hoColumnResize, hoVisible]
          HintMode = hmTooltip
          Images = ApplicationDM.AdminTree16ImageList
          Indent = 19
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          TreeOptions.AnimationOptions = [toAnimatedToggle]
          TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus]
          TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag]
          TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
          OnChecked = RestoreTreeViewChecked
          OnFreeNode = RestoreTreeViewFreeNode
          OnGetText = RestoreTreeViewGetText
          OnGetImageIndex = RestoreTreeViewGetImageIndex
          OnInitNode = RestoreTreeViewInitNode
          Columns = <
            item
              MinWidth = 180
              Position = 0
              Width = 250
              WideText = 'Data directory'
            end
            item
              MinWidth = 65
              Position = 1
              Width = 120
              WideText = 'Object type'
            end
            item
              MinWidth = 60
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
              Position = 2
              Width = 60
              WideText = 'Rows'
            end
            item
              MinWidth = 70
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
              Position = 3
              Width = 70
              WideText = 'Data length'
            end
            item
              MinWidth = 80
              Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark]
              Position = 4
              Width = 80
              WideText = 'Update time'
            end>
        end
        object AnalyzeBackupContentBtn: TTntButton
          Left = 470
          Top = 402
          Width = 179
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = '&Analyze Backup File Content'
          TabOrder = 2
          OnClick = AnalyzeBackupContentBtnClick
        end
      end
    end
    object Panel7: TTntPanel
      Left = 10
      Top = 476
      Width = 669
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 475
      object Panel8: TTntPanel
        Left = 434
        Top = 0
        Width = 235
        Height = 41
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object StartRestoreBtn: TTntButton
          Left = 130
          Top = 14
          Width = 105
          Height = 25
          Caption = '&Start Restore'
          Enabled = False
          TabOrder = 0
          OnClick = StartRestoreBtnClick
        end
        object OpenBackupFileBtn: TTntButton
          Left = 12
          Top = 14
          Width = 105
          Height = 25
          Caption = '&Open Backup File'
          TabOrder = 1
          OnClick = OpenBackupFileBtnClick
        end
      end
    end
  end
  object BackupTreeViewPopupMenu: TTntPopupMenu
    Left = 643
    Top = 84
    object RemoveBackupNodeMI: TTntMenuItem
      Caption = 'Remove'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object SubTreeSearchPopupMenu: TTntPopupMenu
    Left = 160
    Top = 40
    object AllcatalogsMI: TTntMenuItem
      Caption = 'All catalogs'
      Checked = True
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object system1: TTntMenuItem
      Caption = 'system'
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object dev1: TTntMenuItem
      Caption = 'dev'
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object world1: TTntMenuItem
      Caption = 'world'
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
end
