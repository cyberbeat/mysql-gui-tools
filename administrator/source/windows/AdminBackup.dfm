object AdminBackupForm: TAdminBackupForm
  Left = 279
  Top = 162
  Caption = 'Backup'
  ClientHeight = 542
  ClientWidth = 872
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ServerBackupRestorePnl: TTntPanel
    Left = 185
    Top = 0
    Width = 687
    Height = 542
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    object BackupRestorePageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 667
      Height = 481
      ActivePage = BackupContentTabSheet
      Align = alClient
      TabOrder = 0
      object BackupContentTabSheet: TTabSheet
        BorderWidth = 10
        Caption = 'Backup Project'
        ImageIndex = 1
        object ContentSplitter: TTntSplitter
          Left = 169
          Top = 109
          Width = 5
          Height = 324
        end
        object Panel1: TTntPanel
          Left = 0
          Top = 0
          Width = 639
          Height = 39
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            639
            39)
          object RestoreBevel: TTntBevel
            Left = 0
            Top = 28
            Width = 636
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label3: TTntLabel
            Left = 32
            Top = 11
            Width = 207
            Height = 13
            Caption = 'Define the name and content of the backup'
          end
          object Backup2Img: TTntImage
            Left = 0
            Top = 0
            Width = 24
            Height = 24
          end
          object Label2: TTntLabel
            Left = 32
            Top = -2
            Width = 88
            Height = 13
            Caption = 'Backup Project'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
        inline SchemataFrame: TSchemataFrame
          Left = 0
          Top = 109
          Width = 169
          Height = 324
          Align = alLeft
          TabOrder = 1
          TabStop = True
          ExplicitTop = 109
          ExplicitWidth = 169
          ExplicitHeight = 324
          inherited CatalogVST: TVirtualStringTree
            Width = 169
            Height = 279
            OnDblClick = SchemataFrameCatalogVSTDblClick
            OnMouseMove = SchemataFrameCatalogVSTMouseMove
            ExplicitWidth = 169
            ExplicitHeight = 279
            WideDefaultText = 'Fetching Data ...'
          end
          inherited TopPnl: TTntPanel
            Width = 169
            ExplicitWidth = 169
            inherited SchemataLbl: TTntLabel
              Width = 48
              ExplicitWidth = 48
            end
          end
          inherited SpacerPnl: TTntPanel
            Width = 169
            ExplicitWidth = 169
          end
          inherited AdvancedEdit: TAdvancedEditFrame
            Width = 169
            ExplicitWidth = 169
          end
          inherited SchemaSearchPopupMenu: TTntPopupMenu
            inherited SearchAllMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited SearchSchemataMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited SearchAssetsMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited SearchColumnsMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited N3: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CustomMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
              inherited SchemataSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited TableSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited ColumnsSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited IndicesSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited SPSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited ViewsSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
              inherited TriggersSubMI: TTntMenuItem
                OnAdvancedDrawItem = MenuDrawItem
                OnMeasureItem = MenuMeasureItem
              end
            end
          end
          inherited SchemaTreeViewPopupMenu: TTntPopupMenu
            inherited EditMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited RenameMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited DropMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CopyAssetSQLMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited N1: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CreateNewSchemaMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CreateNewTableMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CreateNewViewMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited CreateNewStoredProcedureMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited N2: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
            inherited RefreshCatalogsSchemataListMI: TTntMenuItem
              OnAdvancedDrawItem = MenuDrawItem
              OnMeasureItem = MenuMeasureItem
            end
          end
        end
        object Panel2: TTntPanel
          Left = 174
          Top = 109
          Width = 35
          Height = 324
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 2
          object AddSchemaToBackupBtn: TTntButton
            Left = 2
            Top = 46
            Width = 27
            Height = 25
            Caption = '>'
            TabOrder = 0
            OnClick = AddSchemaToBackupBtnClick
          end
          object RemoveSchemaFromBackup: TTntButton
            Left = 2
            Top = 78
            Width = 27
            Height = 25
            Caption = '<'
            TabOrder = 1
            OnClick = RemoveBackupNodeMIClick
          end
        end
        object Panel3: TTntPanel
          Left = 209
          Top = 109
          Width = 428
          Height = 324
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 3
          object Panel4: TTntPanel
            Left = 0
            Top = 0
            Width = 428
            Height = 17
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label1: TTntLabel
              Left = 2
              Top = 2
              Width = 77
              Height = 13
              Caption = 'Backup Content'
            end
          end
          object BackupTreeView: TVirtualStringTree
            Left = 0
            Top = 17
            Width = 428
            Height = 307
            Align = alClient
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
            PopupMenu = BackupTreeViewPopupMenu
            ShowHint = True
            TabOrder = 0
            TreeOptions.AnimationOptions = [toAnimatedToggle]
            TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus]
            TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag]
            TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
            OnChecked = BackupTreeViewChecked
            OnDragOver = BackupTreeViewDragOver
            OnDragDrop = BackupTreeViewDragDrop
            OnFreeNode = BackupTreeViewFreeNode
            OnGetText = BackupTreeViewGetText
            OnGetImageIndex = BackupTreeViewGetImageIndex
            OnInitNode = BackupTreeViewInitNode
            Columns = <
              item
                MinWidth = 160
                Position = 0
                Width = 160
                WideText = 'Data directory'
              end
              item
                MinWidth = 50
                Position = 1
                WideText = 'Objecttype'
              end
              item
                MinWidth = 40
                Position = 2
                WideText = 'Rows'
              end
              item
                MinWidth = 50
                Position = 3
                WideText = 'Data length'
              end
              item
                MinWidth = 90
                Position = 4
                Width = 90
                WideText = 'Last update'
              end>
          end
        end
        object Panel9: TTntPanel
          Left = 0
          Top = 39
          Width = 639
          Height = 70
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          DesignSize = (
            639
            70)
          object GroupBox1: TTntGroupBox
            Left = 0
            Top = 1
            Width = 637
            Height = 57
            Anchors = [akLeft, akTop, akRight]
            Caption = 'General'
            TabOrder = 0
            object Label4: TTntLabel
              Left = 18
              Top = 24
              Width = 67
              Height = 13
              Caption = 'Project Name:'
            end
            object Label5: TTntLabel
              Left = 316
              Top = 24
              Width = 295
              Height = 13
              AutoSize = False
              Caption = 'Name for this backup project.'
            end
            object ProjectNameEd: TTntEdit
              Left = 116
              Top = 20
              Width = 181
              Height = 21
              TabOrder = 0
              Text = 'New Project'
              OnChange = DoChange
              OnExit = ProjectNameEdExit
            end
          end
        end
        object Panel10: TTntPanel
          Left = 637
          Top = 109
          Width = 2
          Height = 324
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 5
        end
      end
      object AdvancedOptionsTabSheet: TTabSheet
        Caption = 'Advanced Options'
        ImageIndex = 2
        object Panel5: TTntPanel
          Left = 0
          Top = 0
          Width = 659
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            659
            45)
          object Bevel1: TTntBevel
            Left = 10
            Top = 38
            Width = 636
            Height = 3
            Anchors = [akLeft, akTop, akBottom]
            Shape = bsTopLine
          end
          object Label15: TTntLabel
            Left = 42
            Top = 8
            Width = 105
            Height = 13
            Caption = 'Advanced Options'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Backup3Img: TTntImage
            Left = 10
            Top = 10
            Width = 24
            Height = 24
          end
          object Label16: TTntLabel
            Left = 42
            Top = 21
            Width = 310
            Height = 13
            Caption = 'Specify detailed settings of how your backup should be performed'
          end
        end
        object AdvOptionsScrollBox: TTntScrollBox
          Left = 0
          Top = 45
          Width = 659
          Height = 408
          Align = alClient
          BevelInner = bvNone
          BorderStyle = bsNone
          ParentBackground = True
          TabOrder = 1
          DesignSize = (
            659
            408)
          object BackExecuteGBox: TTntGroupBox
            Left = 10
            Top = 5
            Width = 637
            Height = 218
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Backup Execution Method'
            TabOrder = 0
            DesignSize = (
              637
              218)
            object Label17: TTntLabel
              Left = 180
              Top = 60
              Width = 440
              Height = 33
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 
                'All tables are locked with FLUSH READ LOCKS. This option is nece' +
                'ssary to create a consistent snapshot of your MyISAM tables.'
              WordWrap = True
            end
            object Label18: TTntLabel
              Left = 180
              Top = 24
              Width = 440
              Height = 29
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 
                'The backup will be executed in a single transaction. This option' +
                ' is necessary to create a consistent snapshot of your InnoDB tab' +
                'les.'
              WordWrap = True
            end
            object Label19: TTntLabel
              Left = 180
              Top = 138
              Width = 440
              Height = 37
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 
                'Each table will be backed up independently. This may lead to inc' +
                'onsistent backups and should be avoided if possible.'
              WordWrap = True
            end
            object Label14: TTntLabel
              Left = 180
              Top = 180
              Width = 443
              Height = 35
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 
                'Backup all tables. Use this option if tables are added to schema' +
                's frequently. Note: your backup content table selection will be ' +
                'ignored.'
              WordWrap = True
            end
            object PointInTimeLbl: TTntLabel
              Left = 180
              Top = 98
              Width = 440
              Height = 33
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 
                'This backup method is the same as InnoDB Online Backup but also ' +
                'logs the current position of the binlog if the option is activat' +
                'ed for the server.'
              WordWrap = True
            end
            object LockAllTablesRButton: TTntRadioButton
              Left = 16
              Top = 60
              Width = 150
              Height = 17
              Caption = 'Lock all tables'
              TabOrder = 1
              OnClick = DoChange
            end
            object SingleTransRButton: TTntRadioButton
              Left = 16
              Top = 24
              Width = 150
              Height = 17
              Caption = 'InnoDB Online Backup'
              Checked = True
              TabOrder = 0
              TabStop = True
              OnClick = DoChange
            end
            object NormalBackupRadioButton: TTntRadioButton
              Left = 16
              Top = 138
              Width = 150
              Height = 17
              Caption = 'Normal backup'
              TabOrder = 3
              OnClick = DoChange
            end
            object CompleteSchematasCBox: TTntCheckBox
              Left = 18
              Top = 180
              Width = 150
              Height = 17
              Caption = 'Complete backup'
              TabOrder = 4
              OnClick = DoChange
            end
            object PointInTimeRBtn: TTntRadioButton
              Left = 16
              Top = 98
              Width = 150
              Height = 17
              Caption = 'Online with binlog pos'
              TabOrder = 2
              OnClick = DoChange
            end
          end
          object OutputFileGBox: TTntGroupBox
            Left = 10
            Top = 234
            Width = 637
            Height = 155
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Output File Options'
            TabOrder = 1
            object Label12: TTntLabel
              Left = 20
              Top = 30
              Width = 67
              Height = 13
              Caption = 'Backup Type:'
            end
            object Label13: TTntLabel
              Left = 318
              Top = 29
              Width = 301
              Height = 19
              AutoSize = False
              Caption = 'Type of generated backup file.'
            end
            object NoCreatesCBox: TTntCheckBox
              Left = 20
              Top = 60
              Width = 230
              Height = 17
              Hint = 
                'If checked no DDL for tables, views, triggers etc. is included i' +
                'n the dump'
              Caption = 'No CREATEs'
              TabOrder = 1
              OnClick = DoChange
            end
            object NoExtendedInsertsCBox: TTntCheckBox
              Left = 20
              Top = 84
              Width = 230
              Height = 17
              Hint = 
                'If checked, each INSERT statement gets only one row instead seve' +
                'ral (which is faster)'
              Caption = 'No EXTENDED INSERTS'
              TabOrder = 2
              OnClick = DoChange
            end
            object AddDropTableCBox: TTntCheckBox
              Left = 20
              Top = 108
              Width = 230
              Height = 17
              Hint = 
                'If checked, a DROP statement is added for each object in the dum' +
                'p, only useful if CREATEs are enabled'
              Caption = 'Add DROP Statements'
              TabOrder = 3
              OnClick = DoChange
            end
            object CompleteInsertsCBox: TTntCheckBox
              Left = 260
              Top = 60
              Width = 171
              Height = 17
              Hint = 'If checked, INSERT statements have the columns list included'
              Caption = 'Complete INSERTs'
              TabOrder = 4
              OnClick = DoChange
            end
            object CommentCBox: TTntCheckBox
              Left = 260
              Top = 85
              Width = 173
              Height = 17
              Hint = 
                'If checked, a short comment is added  to each action in the back' +
                'up'
              Caption = 'Comment'
              TabOrder = 5
              OnClick = DoChange
            end
            object FullPathCBox: TTntCheckBox
              Left = 260
              Top = 108
              Width = 173
              Height = 17
              Hint = 
                'If checked, identifiers will be qualified by the schema name (i.' +
                'e. "schema.table" vs. "table" only)'
              Caption = 'Fully qualified identifiers'
              TabOrder = 6
              OnClick = DoChange
            end
            object AnsiQuotesCBox: TTntCheckBox
              Left = 448
              Top = 84
              Width = 173
              Height = 17
              Hint = 'If checked, allow "" quotes instead only back ticks (`)'
              Caption = 'ANSI Quotes'
              TabOrder = 8
              OnClick = DoChange
            end
            object DisableKeysCBox: TTntCheckBox
              Left = 448
              Top = 108
              Width = 173
              Height = 17
              Hint = 
                'If checked, index creation for MyISAM tables are delayed until a' +
                'll data is loaded'
              Caption = 'Disable keys'
              TabOrder = 9
              OnClick = DoChange
            end
            object BackupFileTypesLU: TTntComboBox
              Left = 118
              Top = 26
              Width = 181
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = 'SQL Files'
              OnChange = DoChange
              Items.Strings = (
                'SQL Files')
            end
            object CompatibilityCheckbox: TCheckBox
              Left = 448
              Top = 60
              Width = 169
              Height = 17
              Hint = 
                'If checked, MYSQL323 is added to the sql_mode variable in the ba' +
                'ckup'
              HelpType = htKeyword
              Caption = 'Compatibility mode'
              TabOrder = 7
              OnClick = DoChange
            end
            object OptimizedCommitCBox: TTntCheckBox
              Left = 20
              Top = 131
              Width = 230
              Height = 17
              Hint = 
                'If checked, auto commit is switched off and manual commits are i' +
                'nserted to speed up loading data'
              Caption = 'Optimized commit'
              TabOrder = 10
              OnClick = DoChange
            end
          end
        end
      end
      object ScheduleTabSheet: TTabSheet
        BorderWidth = 10
        Caption = 'Schedule'
        ImageIndex = 3
        DesignSize = (
          639
          433)
        object ScheduleGBox: TTntGroupBox
          Left = 0
          Top = 40
          Width = 637
          Height = 155
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Schedule'
          TabOrder = 1
          DesignSize = (
            637
            155)
          object ScheduleTargetDirCaptionLbl: TTntLabel
            Left = 18
            Top = 60
            Width = 63
            Height = 13
            Caption = 'Target folder:'
          end
          object ScheduleTargetDirLbl: TTntLabel
            Left = 424
            Top = 55
            Width = 205
            Height = 30
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'Directory where the backup files should be stored.'
            WordWrap = True
          end
          object SchedulePrefixCaptionLbl: TTntLabel
            Left = 18
            Top = 98
            Width = 45
            Height = 13
            Caption = 'Filename:'
          end
          object SchedulePrefixLbl: TTntLabel
            Left = 312
            Top = 94
            Width = 313
            Height = 51
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              'The filename used for the backup. A timestamp will be appended t' +
              'o this filename if the appropriate option is enabled in the appl' +
              'ication options.'
            WordWrap = True
          end
          object ScheduleLbl: TTntLabel
            Left = 312
            Top = 24
            Width = 295
            Height = 19
            AutoSize = False
            Caption = 'To schedule a backup enable this option.'
          end
          object ScheduleTargetDirEd: TTntEdit
            Left = 116
            Top = 55
            Width = 265
            Height = 21
            TabOrder = 1
            Text = 'd:\Backups'
            OnChange = DoChange
          end
          object ScheduleBrowseTargetDirBtn: TTntButton
            Left = 384
            Top = 55
            Width = 25
            Height = 23
            Caption = '...'
            TabOrder = 2
            OnClick = ScheduleBrowseTargetDirBtnClick
          end
          object ScheduleFilenamePrefixEd: TTntEdit
            Left = 116
            Top = 94
            Width = 181
            Height = 21
            TabOrder = 3
            Text = 'New Project'
            OnChange = DoChange
          end
          object ScheduleCBox: TTntCheckBox
            Left = 16
            Top = 24
            Width = 257
            Height = 17
            Caption = 'Schedule this backup project'
            TabOrder = 0
            OnClick = ScheduleCBoxClick
          end
        end
        object Panel6: TTntPanel
          Left = 0
          Top = 0
          Width = 639
          Height = 35
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            639
            35)
          object BackupBevel: TTntBevel
            Left = -2
            Top = 28
            Width = 638
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label6: TTntLabel
            Left = 32
            Top = -2
            Width = 98
            Height = 13
            Caption = 'Schedule Project'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object BackupImg: TTntImage
            Left = 0
            Top = 0
            Width = 24
            Height = 24
          end
          object Label7: TTntLabel
            Left = 32
            Top = 11
            Width = 274
            Height = 13
            Caption = 'Specify if the backup project should be executed regulary.'
          end
        end
        object ExecutionTimeGBox: TTntGroupBox
          Left = 0
          Top = 206
          Width = 637
          Height = 219
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Execution Time'
          TabOrder = 2
          DesignSize = (
            637
            219)
          object SchedulePageControl: TTntPageControl
            Left = 14
            Top = 46
            Width = 356
            Height = 123
            ActivePage = WeeklyTabSheet
            TabHeight = 4
            TabOrder = 1
            object DailyTabSheet: TTabSheet
              Caption = 'DailyTabSheet'
              TabVisible = False
              object Label26: TTntLabel
                Left = 0
                Top = 4
                Width = 261
                Height = 13
                Caption = 'The backup will be executed on each day of the week.'
              end
            end
            object WeeklyTabSheet: TTabSheet
              Caption = 'WeeklyTabSheet'
              ImageIndex = 1
              TabVisible = False
              object Label21: TTntLabel
                Left = 0
                Top = 4
                Width = 307
                Height = 13
                Caption = 'The backup will be executed on each of the following weekdays:'
                WordWrap = True
              end
              object ScheduleMondayCBox: TTntCheckBox
                Left = 2
                Top = 42
                Width = 90
                Height = 17
                Caption = 'Monday'
                TabOrder = 0
                OnClick = DoChange
              end
              object ScheduleTuesdayCBox: TTntCheckBox
                Left = 104
                Top = 42
                Width = 90
                Height = 17
                Caption = 'Tuesday'
                TabOrder = 1
                OnClick = DoChange
              end
              object ScheduleWednesdayCBox: TTntCheckBox
                Left = 206
                Top = 42
                Width = 115
                Height = 17
                Caption = 'Wednesday'
                TabOrder = 2
                OnClick = DoChange
              end
              object ScheduleThursdayCBox: TTntCheckBox
                Left = 2
                Top = 66
                Width = 90
                Height = 17
                Caption = 'Thursday'
                TabOrder = 3
                OnClick = DoChange
              end
              object ScheduleFridayCBox: TTntCheckBox
                Left = 104
                Top = 66
                Width = 90
                Height = 17
                Caption = 'Friday'
                TabOrder = 4
                OnClick = DoChange
              end
              object ScheduleSaturdayCBox: TTntCheckBox
                Left = 206
                Top = 66
                Width = 115
                Height = 17
                Caption = 'Saturday'
                TabOrder = 5
                OnClick = DoChange
              end
              object ScheduleSundayCBox: TTntCheckBox
                Left = 2
                Top = 90
                Width = 90
                Height = 17
                Caption = 'Sunday'
                TabOrder = 6
                OnClick = DoChange
              end
            end
            object MonthlyTabSheet: TTabSheet
              Caption = 'MonthlyTabSheet'
              ImageIndex = 2
              ParentShowHint = False
              ShowHint = False
              TabVisible = False
              object Label22: TTntLabel
                Left = 102
                Top = 33
                Width = 68
                Height = 13
                Caption = 'of each month'
              end
              object Label20: TTntLabel
                Left = 2
                Top = 33
                Width = 32
                Height = 13
                Caption = 'On the'
              end
              object Label25: TTntLabel
                Left = 0
                Top = 4
                Width = 208
                Height = 13
                Caption = 'The backup will be executed once a month.'
              end
              object ScheduleDayOfMonthCBox: TTntComboBox
                Left = 44
                Top = 29
                Width = 49
                Height = 21
                Style = csDropDownList
                DropDownCount = 16
                ItemHeight = 13
                ItemIndex = 0
                TabOrder = 0
                Text = '1 st'
                Items.Strings = (
                  '1 st'
                  '2 nd'
                  '3 rd'
                  '4 th'
                  '5 th'
                  '6 th'
                  '7 th'
                  '8 th'
                  '9 th'
                  '10 th'
                  '11 th'
                  '12 th'
                  '13 th'
                  '14 th'
                  '15 th'
                  '16 th'
                  '17 th'
                  '18 th'
                  '19 th'
                  '20 th'
                  '21 th'
                  '22 th'
                  '23 th'
                  '24 th'
                  '25 th'
                  '26 th'
                  '27 th'
                  '28 th'
                  '29 th'
                  '30 th'
                  '31 th')
              end
            end
          end
          object TimePnl: TTntPanel
            Left = 382
            Top = 46
            Width = 247
            Height = 99
            Anchors = [akLeft, akTop, akRight]
            BevelOuter = bvNone
            TabOrder = 2
            object Label23: TTntLabel
              Left = 4
              Top = 48
              Width = 26
              Height = 13
              Caption = 'Time:'
            end
            object Label24: TTntLabel
              Left = 4
              Top = 2
              Width = 193
              Height = 13
              Caption = 'The backup will be executed at this time:'
              WordWrap = True
            end
            object ScheduleTimeEd: TTntEdit
              Left = 48
              Top = 44
              Width = 69
              Height = 21
              TabOrder = 0
              Text = '23:00'
              OnChange = DoChange
            end
          end
          object ScheduleTypeCbox: TTntComboBox
            Left = 14
            Top = 24
            Width = 269
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnCloseUp = ScheduleTypeCboxCloseUp
            Items.Strings = (
              'Execute backup daily'
              'Execute backup weekly'
              'Execute backup monthly')
          end
        end
      end
    end
    object Panel7: TTntPanel
      Left = 10
      Top = 491
      Width = 667
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        667
        41)
      object NewProjectBtn: TTntButton
        Left = 232
        Top = 15
        Width = 139
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'New Project'
        TabOrder = 0
        OnClick = NewProjectBtnClick
      end
      object ApplyBtn: TTntButton
        Left = 380
        Top = 15
        Width = 139
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Save Project'
        Enabled = False
        TabOrder = 1
        OnClick = ApplyBtnClick
      end
      object StartBackupBtn: TTntButton
        Left = 528
        Top = 15
        Width = 139
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Execute Backup Now'
        Enabled = False
        TabOrder = 2
        OnClick = StartBackupBtnClick
      end
    end
  end
  object SubTreePnl: TTntPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 542
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object TopPnl: TTntPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object SchemataLbl: TTntLabel
        Left = 2
        Top = 2
        Width = 78
        Height = 13
        Caption = 'Backup Projects'
      end
    end
    inline AdvancedEditFrame: TAdvancedEditFrame
      Left = 0
      Top = 17
      Width = 185
      Height = 22
      Align = alTop
      TabOrder = 2
      TabStop = True
      ExplicitTop = 17
      ExplicitWidth = 185
      ExplicitHeight = 22
      inherited SearchEd: TTntEdit
        OnChange = AdvancedEditFrameSearchEdChange
      end
    end
    object SpacerPnl: TTntPanel
      Left = 0
      Top = 39
      Width = 185
      Height = 5
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
    end
    object ProjectListView: TTntListView
      Left = 0
      Top = 44
      Width = 185
      Height = 498
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Project Name'
        end>
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = ProfilesPopupMenu
      ShowColumnHeaders = False
      TabOrder = 1
      ViewStyle = vsReport
      OnSelectItem = ProjectListViewSelectItem
    end
  end
  object BackupTreeViewPopupMenu: TTntPopupMenu
    OnPopup = BackupTreeViewPopupMenuPopup
    Left = 767
    Top = 14
    object RemoveBackupNodeMI: TTntMenuItem
      Caption = 'Remove'
      OnClick = RemoveBackupNodeMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object ClearCompleteContentMI: TTntMenuItem
      Caption = 'Clear Complete Content'
      OnClick = ClearCompleteContentMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object ProfilesPopupMenu: TTntPopupMenu
    OnPopup = ProfilesPopupMenuPopup
    Left = 80
    Top = 98
    object RefreshProfilesMI: TTntMenuItem
      Caption = 'Refresh Backup Projects'
      GroupIndex = 1
      OnClick = RefreshProfilesMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N1: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object DeleteProfileMI: TTntMenuItem
      Caption = 'Delete Backup Project(s)'
      GroupIndex = 1
      OnClick = DeleteProfileMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
end
