object AdminUsersForm: TAdminUsersForm
  Left = 120
  Top = 66
  Caption = 'User Administration'
  ClientHeight = 544
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TTntSplitter
    Left = 185
    Top = 0
    Height = 544
    ExplicitHeight = 546
  end
  object UserPnl: TTntPanel
    Left = 188
    Top = 0
    Width = 593
    Height = 544
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    OnResize = UserPnlResize
    object SectionDisabledLabel: TTntLabel
      Left = 298
      Top = 0
      Width = 295
      Height = 503
      Align = alRight
      Alignment = taRightJustify
      Caption = 'This section is not available  for this server version.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitHeight = 13
    end
    object UserPageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 571
      Height = 487
      ActivePage = UserInfoSheet
      TabOrder = 0
      OnChange = UserPageControlChange
      object UserInfoSheet: TTabSheet
        Caption = 'User Information'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel1: TTntPanel
          Left = 0
          Top = 0
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 0
          object UserInfoBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 535
            Height = 3
            Shape = bsTopLine
          end
          object UserInfoNameLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 104
            Height = 13
            Caption = 'brian, (Brian Aker)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object HeaderImg: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object UserInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 205
            Height = 13
            Caption = 'Login and additional information on the user'
          end
        end
        object UserInfoScrollBox: TTntScrollBox
          Left = 0
          Top = 45
          Width = 563
          Height = 414
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          ParentBackground = True
          TabOrder = 1
          object UserInfoLoginGBox: TTntGroupBox
            Left = 12
            Top = 6
            Width = 534
            Height = 133
            Caption = 'Login Information'
            TabOrder = 0
            object Label27: TTntLabel
              Left = 14
              Top = 26
              Width = 63
              Height = 13
              Caption = 'MySQL User:'
            end
            object Label28: TTntLabel
              Left = 236
              Top = 22
              Width = 285
              Height = 31
              AutoSize = False
              Caption = 
                'The user has to enter this MySQL User name to connect to the MyS' +
                'QL Server'
              WordWrap = True
            end
            object Label3: TTntLabel
              Left = 14
              Top = 68
              Width = 49
              Height = 13
              Caption = 'Password:'
            end
            object Label11: TTntLabel
              Left = 236
              Top = 68
              Width = 285
              Height = 15
              AutoSize = False
              Caption = 'Fill out this field if you want to set the user'#39's password'
              WordWrap = True
            end
            object Label12: TTntLabel
              Left = 14
              Top = 98
              Width = 87
              Height = 13
              Caption = 'Confirm Password:'
            end
            object Label13: TTntLabel
              Left = 236
              Top = 98
              Width = 285
              Height = 15
              AutoSize = False
              Caption = 'Again, enter the user'#39's password to confirm'
              WordWrap = True
            end
            object UserNameEd: TTntEdit
              Left = 120
              Top = 22
              Width = 101
              Height = 21
              TabOrder = 0
              OnChange = UserNameEdChange
            end
            object PasswordEd: TTntEdit
              Left = 120
              Top = 64
              Width = 101
              Height = 21
              PasswordChar = '*'
              TabOrder = 1
              OnChange = UserNameEdChange
            end
            object PasswordConfirmationEd: TTntEdit
              Left = 120
              Top = 94
              Width = 101
              Height = 21
              PasswordChar = '*'
              TabOrder = 2
              OnChange = UserNameEdChange
            end
          end
          object UserInfoAdditionalGBox: TTntGroupBox
            Left = 12
            Top = 150
            Width = 534
            Height = 247
            Caption = 'Additional Information'
            TabOrder = 1
            object Label23: TTntLabel
              Left = 14
              Top = 24
              Width = 50
              Height = 13
              Caption = 'Full Name:'
            end
            object Label25: TTntLabel
              Left = 14
              Top = 54
              Width = 56
              Height = 13
              Caption = 'Description:'
            end
            object Label24: TTntLabel
              Left = 312
              Top = 26
              Width = 207
              Height = 15
              AutoSize = False
              Caption = 'The user'#39's full name'
              WordWrap = True
            end
            object Label26: TTntLabel
              Left = 312
              Top = 56
              Width = 207
              Height = 15
              AutoSize = False
              Caption = 'Additional description of the user'
              WordWrap = True
            end
            object Label29: TTntLabel
              Left = 14
              Top = 184
              Width = 24
              Height = 13
              Caption = 'Icon:'
            end
            object Label30: TTntLabel
              Left = 312
              Top = 186
              Width = 207
              Height = 17
              AutoSize = False
              Caption = 'Icon assigned to the user'
              WordWrap = True
            end
            object Label5: TTntLabel
              Left = 14
              Top = 115
              Width = 95
              Height = 13
              Caption = 'Contact Information:'
            end
            object Label18: TTntLabel
              Left = 312
              Top = 116
              Width = 207
              Height = 15
              AutoSize = False
              Caption = 'Optional contact information '
              WordWrap = True
            end
            object Label19: TTntLabel
              Left = 14
              Top = 86
              Width = 28
              Height = 13
              Caption = 'Email:'
            end
            object Label20: TTntLabel
              Left = 312
              Top = 86
              Width = 207
              Height = 15
              AutoSize = False
              Caption = 'The user'#39's email address'
              WordWrap = True
            end
            object FullNameEd: TTntEdit
              Left = 120
              Top = 22
              Width = 179
              Height = 21
              TabOrder = 0
              OnChange = UserNameEdChange
            end
            object DescriptionEd: TTntEdit
              Left = 120
              Top = 52
              Width = 179
              Height = 21
              TabOrder = 1
              OnChange = UserNameEdChange
            end
            object LoadImgBtn: TTntBitBtn
              Left = 194
              Top = 182
              Width = 103
              Height = 21
              Caption = 'Load from disk'
              TabOrder = 4
              OnClick = LoadImgBtnClick
            end
            object Panel7: TTntPanel
              Left = 120
              Top = 182
              Width = 52
              Height = 52
              BevelOuter = bvNone
              BorderStyle = bsSingle
              Color = clWindow
              TabOrder = 6
              object UserIcon: TTntImage
                Left = 0
                Top = 0
                Width = 48
                Height = 48
              end
            end
            object EmailEd: TTntEdit
              Left = 120
              Top = 82
              Width = 179
              Height = 21
              TabOrder = 2
              OnChange = UserNameEdChange
            end
            object ContactInfoMemo: TTntMemo
              Left = 120
              Top = 112
              Width = 179
              Height = 59
              TabOrder = 3
              OnChange = UserNameEdChange
            end
            object ClearUserImg: TTntBitBtn
              Left = 194
              Top = 214
              Width = 103
              Height = 21
              Caption = 'Clear Image'
              TabOrder = 5
              OnClick = ClearUserImgClick
            end
          end
        end
      end
      object GlobalPrivSheet: TTabSheet
        Caption = 'Global Privileges'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label15: TTntLabel
          Left = 12
          Top = 50
          Width = 91
          Height = 13
          Caption = 'Assigned Privileges'
        end
        object Label16: TTntLabel
          Left = 220
          Top = 50
          Width = 91
          Height = 13
          Caption = 'Available Privileges'
        end
        object Panel9: TTntPanel
          Left = 0
          Top = 0
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 1
          object GlobalBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 535
            Height = 3
            Shape = bsTopLine
          end
          object GlobalPrivNameLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 104
            Height = 13
            Caption = 'brian, (Brian Aker)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Header2Img: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object GlobalPrivInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 178
            Height = 13
            Caption = 'Global Privileges assigned to the User'
          end
        end
        object AssignGlobalPrivBtn: TTntBitBtn
          Left = 184
          Top = 66
          Width = 27
          Height = 25
          Caption = '<'
          TabOrder = 2
          OnClick = AssignGlobalPrivBtnClick
        end
        object RemoveGlobalPrivBtn: TTntBitBtn
          Left = 184
          Top = 100
          Width = 27
          Height = 25
          Caption = '>'
          TabOrder = 3
          OnClick = RemoveGlobalPrivBtnClick
        end
        object GlobalPrivListView: TTntListView
          Left = 220
          Top = 66
          Width = 327
          Height = 359
          Columns = <
            item
              Caption = 'Privilege'
              MaxWidth = 200
              Width = 150
            end
            item
              AutoSize = True
              Caption = 'Description'
              MinWidth = 100
            end>
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 0
          ViewStyle = vsReport
        end
        object GlobalAssignedPrivListView: TTntListView
          Left = 12
          Top = 66
          Width = 163
          Height = 359
          Columns = <
            item
              Caption = 'Rights'
              Width = 139
            end>
          MultiSelect = True
          ReadOnly = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 6
          ViewStyle = vsReport
        end
        object AssignAllGlobalPrivBtn: TTntBitBtn
          Left = 184
          Top = 134
          Width = 27
          Height = 25
          Caption = '<<'
          TabOrder = 4
          OnClick = AssignAllGlobalPrivBtnClick
        end
        object RemoveAllGlobalPrivBtn: TTntBitBtn
          Left = 184
          Top = 168
          Width = 27
          Height = 25
          Caption = '>>'
          TabOrder = 5
          OnClick = RemoveAllGlobalPrivBtnClick
        end
      end
      object SchemataPrivSheet: TTabSheet
        Caption = 'Schema Privileges'
        ImageIndex = 1
        object Label7: TTntLabel
          Left = 360
          Top = 52
          Width = 91
          Height = 13
          Caption = 'Available Privileges'
        end
        object Label6: TTntLabel
          Left = 172
          Top = 50
          Width = 91
          Height = 13
          Caption = 'Assigned Privileges'
        end
        object Panel2: TTntPanel
          Left = 0
          Top = 0
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object DBBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 535
            Height = 3
            Shape = bsTopLine
          end
          object SchemaPrivNameLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 104
            Height = 13
            Caption = 'brian, (Brian Aker)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Header3Img: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object SchemaPrivInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 187
            Height = 13
            Caption = 'Schema Privileges assigned to the User'
          end
        end
        object AssignSchemaPrivBtn: TTntBitBtn
          Left = 324
          Top = 66
          Width = 27
          Height = 25
          Caption = '<'
          TabOrder = 3
          OnClick = AssignSchemaPrivBtnClick
        end
        object RemoveSchemaPrivBtn: TTntBitBtn
          Left = 324
          Top = 98
          Width = 27
          Height = 25
          Caption = '>'
          TabOrder = 4
          OnClick = RemoveSchemaPrivBtnClick
        end
        object SchemaPrivAvailListView: TTntListView
          Left = 360
          Top = 66
          Width = 187
          Height = 359
          Columns = <
            item
              Caption = 'Right'
              MaxWidth = 200
              Width = 120
            end
            item
              AutoSize = True
              Caption = 'Description'
            end>
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 7
          ViewStyle = vsReport
        end
        object SchemaPrivAssignedListView: TTntListView
          Left = 172
          Top = 66
          Width = 143
          Height = 359
          Columns = <
            item
              Caption = 'Rights'
              Width = 98
            end>
          MultiSelect = True
          ReadOnly = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 2
          ViewStyle = vsReport
        end
        inline SchemaPrivSchemataFrame: TSchemataFrame
          Left = 12
          Top = 49
          Width = 146
          Height = 376
          TabOrder = 1
          TabStop = True
          ExplicitLeft = 12
          ExplicitTop = 49
          ExplicitWidth = 146
          ExplicitHeight = 376
          inherited CatalogVST: TVirtualStringTree
            Width = 146
            Height = 331
            PopupMenu = CatalogTreePopup
            OnChange = SchemaPrivSchemataFrameCatalogVSTChange
            ExplicitWidth = 146
            ExplicitHeight = 331
            WideDefaultText = 'Fetching Data ...'
          end
          inherited TopPnl: TTntPanel
            Width = 146
            ExplicitWidth = 146
            inherited SchemataLbl: TTntLabel
              Top = 1
              Width = 48
              ExplicitTop = 1
              ExplicitWidth = 48
            end
          end
          inherited SpacerPnl: TTntPanel
            Width = 146
            ExplicitWidth = 146
          end
          inherited AdvancedEdit: TAdvancedEditFrame
            Width = 146
            ExplicitWidth = 146
            inherited SearchEd: TTntEdit
              Width = 100
              OnChange = AdvancedSchemaEditSearchEdChange
              ExplicitWidth = 100
            end
          end
          inherited SchemaTreeViewPopupMenu: TTntPopupMenu
            inherited RefreshCatalogsSchemataListMI: TTntMenuItem
              OnClick = SchemaPrivSchemataFrameRefreshCatalogsSchemataListMIClick
            end
          end
        end
        object AssignAllSchemaPrivBtn: TTntBitBtn
          Left = 324
          Top = 132
          Width = 27
          Height = 25
          Caption = '<<'
          TabOrder = 5
          OnClick = AssignAllSchemaPrivBtnClick
        end
        object RemoveAllSchemaPrivBtn: TTntBitBtn
          Left = 324
          Top = 166
          Width = 27
          Height = 25
          Caption = '>>'
          TabOrder = 6
          OnClick = RemoveAllSchemaPrivBtnClick
        end
      end
      object TablePrivSheet: TTabSheet
        Caption = 'Schema Object Privileges'
        ImageIndex = 2
        object Label2: TTntLabel
          Left = 404
          Top = 52
          Width = 91
          Height = 13
          Caption = 'Available Privileges'
        end
        object Label4: TTntLabel
          Left = 216
          Top = 50
          Width = 91
          Height = 13
          Caption = 'Assigned Privileges'
        end
        object Panel5: TTntPanel
          Left = 0
          Top = 0
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object TableBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 535
            Height = 3
            Shape = bsTopLine
          end
          object TableColumnPrivNameLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 104
            Height = 13
            Caption = 'brian, (Brian Aker)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Header4Img: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object TableColumnPrivInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 234
            Height = 13
            Caption = 'Table and Column Privileges assigned to the User'
          end
        end
        object AssignTblColPrivBtn: TTntBitBtn
          Left = 368
          Top = 66
          Width = 27
          Height = 25
          Caption = '<'
          TabOrder = 3
          OnClick = AssignTblColPrivBtnClick
        end
        object RemoveTblColPrivBtn: TTntBitBtn
          Left = 368
          Top = 98
          Width = 27
          Height = 25
          Caption = '>'
          TabOrder = 4
          OnClick = RemoveTblColPrivBtnClick
        end
        object TablePrivAvailListView: TTntListView
          Left = 404
          Top = 66
          Width = 143
          Height = 359
          Columns = <
            item
              Caption = 'Right'
              MaxWidth = 200
              Width = 120
            end
            item
              AutoSize = True
              Caption = 'Description'
            end>
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 7
          ViewStyle = vsReport
        end
        object TablePrivAssignedListView: TTntListView
          Left = 216
          Top = 66
          Width = 143
          Height = 359
          Columns = <
            item
              AutoSize = True
              Caption = 'Rights'
            end>
          MultiSelect = True
          ReadOnly = True
          ShowColumnHeaders = False
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 2
          ViewStyle = vsReport
        end
        inline TblColSchemataFrame: TSchemataFrame
          Left = 12
          Top = 49
          Width = 189
          Height = 376
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 12
          ExplicitTop = 49
          ExplicitWidth = 189
          ExplicitHeight = 376
          inherited CatalogVST: TVirtualStringTree
            Width = 189
            Height = 331
            OnChange = TblColSchemataFrameCatalogVSTChange
            ExplicitWidth = 189
            ExplicitHeight = 331
            WideDefaultText = 'Fetching Data ...'
          end
          inherited TopPnl: TTntPanel
            Width = 189
            ExplicitWidth = 189
            inherited SchemataLbl: TTntLabel
              Top = 1
              Width = 48
              ExplicitTop = 1
              ExplicitWidth = 48
            end
          end
          inherited SpacerPnl: TTntPanel
            Width = 189
            ExplicitWidth = 189
          end
          inherited AdvancedEdit: TAdvancedEditFrame
            Width = 189
            ExplicitWidth = 189
            inherited SearchEd: TTntEdit
              Width = 100
              ExplicitWidth = 100
            end
          end
          inherited SchemaTreeViewPopupMenu: TTntPopupMenu
            inherited RefreshCatalogsSchemataListMI: TTntMenuItem
              OnClick = TblColSchemataFrameRefreshCatalogsSchemataListMIClick
            end
          end
        end
        object AssignAllTblColPrivBtn: TTntBitBtn
          Left = 368
          Top = 132
          Width = 27
          Height = 25
          Caption = '<<'
          TabOrder = 5
          OnClick = AssignAllTblColPrivBtnClick
        end
        object RemoveAllTblColPrivBtn: TTntBitBtn
          Left = 368
          Top = 166
          Width = 27
          Height = 25
          Caption = '>>'
          TabOrder = 6
          OnClick = RemoveAllTblColPrivBtnClick
        end
      end
      object ResourceSheet: TTabSheet
        Caption = 'Resources'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel6: TTntPanel
          Left = 0
          Top = 0
          Width = 563
          Height = 45
          Align = alTop
          BevelOuter = bvNone
          ParentColor = True
          TabOrder = 0
          object ResourceBevel: TTntBevel
            Left = 12
            Top = 38
            Width = 535
            Height = 3
            Shape = bsTopLine
          end
          object ResourceNameLbl: TTntLabel
            Left = 46
            Top = 6
            Width = 104
            Height = 13
            Caption = 'brian, (Brian Aker)'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object Header5Img: TTntImage
            Left = 12
            Top = 8
            Width = 24
            Height = 24
          end
          object ResourcesInfoLbl: TTntLabel
            Left = 46
            Top = 20
            Width = 110
            Height = 13
            Caption = 'Resource management'
          end
        end
        object ScrollBox2: TTntScrollBox
          Left = 0
          Top = 45
          Width = 563
          Height = 414
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          ParentBackground = True
          TabOrder = 1
          object UserResourceLimitGBox: TTntGroupBox
            Left = 12
            Top = 6
            Width = 534
            Height = 387
            Caption = 'Limiting user resources'
            TabOrder = 0
          end
        end
      end
    end
    object Panel3: TTntPanel
      Left = 0
      Top = 503
      Width = 593
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object BottomBtnPnl: TTntPanel
        Left = 242
        Top = 0
        Width = 351
        Height = 41
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object DiscardChangesBtn: TTntButton
          Left = 236
          Top = 4
          Width = 105
          Height = 25
          Action = DiscardAction
          TabOrder = 2
        end
        object NewUserBtn: TTntButton
          Left = 4
          Top = 4
          Width = 105
          Height = 25
          Action = AddUserAction
          TabOrder = 0
        end
        object ApplyChangesBtn: TTntButton
          Left = 120
          Top = 4
          Width = 105
          Height = 25
          Action = ApplyAction
          TabOrder = 1
        end
      end
    end
  end
  object SubTreePnl: TTntPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 544
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object UserTreeView: TTntTreeView
      Left = 0
      Top = 45
      Width = 185
      Height = 499
      Align = alClient
      HideSelection = False
      Images = ApplicationDM.AdminTree16ImageList
      Indent = 19
      PopupMenu = UserSubTreePopupMenu
      ReadOnly = True
      ShowLines = False
      ShowRoot = False
      TabOrder = 0
      OnChange = UserTreeViewChange
      OnChanging = UserTreeViewChanging
      OnDeletion = UserTreeViewDeletion
    end
    object SubTreeSearchPnl: TTntPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TTntLabel
        Left = 2
        Top = 2
        Width = 75
        Height = 13
        Caption = 'Users Accounts'
      end
    end
    inline AdvancedEdit: TAdvancedEditFrame
      Left = 0
      Top = 19
      Width = 185
      Height = 22
      Align = alTop
      TabOrder = 2
      TabStop = True
      ExplicitTop = 19
      ExplicitWidth = 185
      ExplicitHeight = 22
      inherited SearchEd: TTntEdit
        OnChange = AdvancedEditSearchEdChange
      end
    end
    object Panel4: TTntPanel
      Left = 0
      Top = 41
      Width = 185
      Height = 4
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
  object UserSubTreePopupMenu: TTntPopupMenu
    OnPopup = UserSubTreePopupMenuPopup
    Left = 60
    Top = 146
    object AddUserMI: TTntMenuItem
      Action = AddUserAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object CloneUserMI: TTntMenuItem
      Action = CloneUserAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object DeleteUserMI: TTntMenuItem
      Action = DeleteUserAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N1: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object AddHostMI: TTntMenuItem
      Action = AddHostAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object RemoveHostMI: TTntMenuItem
      Action = RemoveHostAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object ShowHostsMI: TTntMenuItem
      Action = ShowHostsAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N3: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object RefreshUserListMI: TTntMenuItem
      Action = RefreshAction
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object CatalogTreePopup: TTntPopupMenu
    Left = 212
    Top = 500
    object RefreshCatalogTreeMI: TTntMenuItem
      Caption = 'Refresh Schemata List'
      ShortCut = 116
      OnClick = RefreshCatalogTreeMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object N4: TTntMenuItem
      Caption = '-'
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
    object AddSchemawithWidcardsMI: TTntMenuItem
      Caption = 'Add Schema with Widcards'
      OnClick = AddSchemawithWidcardsMIClick
      OnAdvancedDrawItem = MenuDrawItem
      OnMeasureItem = MenuMeasureItem
    end
  end
  object UserActionManager: TActionManager
    OnUpdate = UserActionManagerUpdate
    Left = 140
    Top = 56
    StyleName = 'XP Style'
    object AddUserAction: TAction
      Category = 'User'
      Caption = 'Add &new user'
      OnExecute = AddUserActionExecute
    end
    object ApplyAction: TAction
      Category = 'User'
      Caption = '&Apply changes'
      OnExecute = ApplyActionExecute
    end
    object DiscardAction: TAction
      Category = 'User'
      Caption = '&Discard changes'
      OnExecute = DiscardActionExecute
    end
    object CloneUserAction: TAction
      Category = 'User'
      Caption = '&Clone user'
      OnExecute = CloneUserActionExecute
    end
    object DeleteUserAction: TAction
      Category = 'User'
      Caption = '&Delete user'
      OnExecute = DeleteUserActionExecute
    end
    object AddHostAction: TAction
      Category = 'Host'
      Caption = 'Add host from which the user can connect'
      OnExecute = AddHostActionExecute
    end
    object RemoveHostAction: TAction
      Category = 'Host'
      Caption = 'Remove host from which the user can connect'
      OnExecute = RemoveHostActionExecute
    end
    object ShowHostsAction: TAction
      Category = 'Host'
      Caption = 'Show hosts in user list'
      OnExecute = ShowHostsActionExecute
    end
    object RefreshAction: TAction
      Category = 'User'
      Caption = 'Refresh user list'
      OnExecute = RefreshActionExecute
    end
  end
end
