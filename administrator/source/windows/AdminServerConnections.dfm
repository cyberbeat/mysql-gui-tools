object AdminServerConnectionsForm: TAdminServerConnectionsForm
  Left = 446
  Top = 243
  Caption = 'Server Connections'
  ClientHeight = 484
  ClientWidth = 591
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
  object ServerProcessesPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 591
    Height = 484
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 0
    object BottomPnl: TTntPanel
      Left = 10
      Top = 434
      Width = 571
      Height = 40
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        571
        40)
      object KillThreadBtn: TTntButton
        Left = 238
        Top = 14
        Width = 105
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Kill thread'
        Enabled = False
        TabOrder = 0
        OnClick = KillThreadBtnClick
      end
      object KillUserBtn: TTntButton
        Left = 352
        Top = 14
        Width = 105
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Kill user'
        Enabled = False
        TabOrder = 1
        OnClick = KillUserBtnClick
      end
      object RefreshBtn: TTntButton
        Left = 466
        Top = 14
        Width = 105
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Refresh'
        TabOrder = 2
        OnClick = RefreshBtnClick
      end
    end
    object ServerProcessesPageControl: TTntPageControl
      Left = 10
      Top = 10
      Width = 571
      Height = 424
      ActivePage = UserConnTabSheet
      Align = alClient
      TabOrder = 1
      OnChange = ServerProcessesPageControlChange
      object ThreadsTabSheet: TTabSheet
        BorderWidth = 12
        Caption = 'Threads'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object ThreadsListView: TTntListView
          Left = 0
          Top = 43
          Width = 539
          Height = 329
          Align = alClient
          Columns = <
            item
              Caption = 'PID'
              Width = 70
            end
            item
              Caption = 'User'
              Width = 80
            end
            item
              Caption = 'Host'
              Width = 117
            end
            item
              Caption = 'DB'
              Width = 80
            end
            item
              Caption = 'Command'
              Width = 80
            end
            item
              Caption = 'Time'
            end
            item
              Caption = 'State'
              Width = 60
            end
            item
              AutoSize = True
              Caption = 'Info'
              WidthType = (
                -2)
            end>
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          PopupMenu = ProcessPopupMenu
          SmallImages = ApplicationDM.AdminTree16ImageList
          TabOrder = 0
          ViewStyle = vsReport
          OnChange = ThreadsListViewChange
        end
        object Panel5: TTntPanel
          Left = 0
          Top = 0
          Width = 539
          Height = 43
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            539
            43)
          object ThreadsBevel: TTntBevel
            Left = 0
            Top = 29
            Width = 537
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label8: TTntLabel
            Left = 34
            Top = 12
            Width = 187
            Height = 13
            Caption = 'A listing of all active connection threads'
          end
          object PageHeaderImg: TTntImage
            Left = 0
            Top = 0
            Width = 24
            Height = 24
            Transparent = True
          end
          object Label2: TTntLabel
            Left = 34
            Top = -2
            Width = 100
            Height = 13
            Caption = 'User connections'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
      object UserConnTabSheet: TTabSheet
        BorderWidth = 12
        Caption = 'User Connections'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object UserConnPnl: TTntPanel
          Left = 0
          Top = 43
          Width = 539
          Height = 329
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Splitter1: TTntSplitter
            Left = 0
            Top = 171
            Width = 539
            Height = 3
            Cursor = crVSplit
            Align = alBottom
            Color = clBtnFace
            ParentColor = False
          end
          object UserConnListView: TTntListView
            Left = 0
            Top = 0
            Width = 539
            Height = 171
            Align = alClient
            Columns = <
              item
                Caption = 'Username'
                Width = 150
              end
              item
                Caption = 'Num.'
              end
              item
                Caption = 'Full Name'
                Width = 150
              end
              item
                Caption = 'Description'
                Width = 135
              end>
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = UserPopupMenu
            SmallImages = ApplicationDM.AdminTree16ImageList
            SortType = stText
            TabOrder = 1
            ViewStyle = vsReport
            OnChange = UserConnListViewChange
          end
          object UserConnThreadsListView: TTntListView
            Left = 0
            Top = 174
            Width = 539
            Height = 155
            Align = alBottom
            Columns = <
              item
                Caption = 'PID'
                Width = 60
              end
              item
                Caption = 'User'
                Width = 80
              end
              item
                Caption = 'Host'
                Width = 117
              end
              item
                Caption = 'DB'
                Width = 80
              end
              item
                Caption = 'Command'
                Width = 80
              end
              item
                Caption = 'Time'
              end
              item
                Caption = 'State'
                Width = 60
              end
              item
                AutoSize = True
                Caption = 'Info'
              end>
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = ProcessPopupMenu
            SmallImages = ApplicationDM.AdminTree16ImageList
            TabOrder = 0
            ViewStyle = vsReport
            OnChange = UserConnThreadsListViewChange
          end
        end
        object Panel1: TTntPanel
          Left = 0
          Top = 0
          Width = 539
          Height = 43
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          DesignSize = (
            539
            43)
          object UserConnBevel: TTntBevel
            Left = 0
            Top = 29
            Width = 537
            Height = 3
            Anchors = [akLeft, akTop, akRight]
            Shape = bsTopLine
          end
          object Label1: TTntLabel
            Left = 34
            Top = 12
            Width = 211
            Height = 13
            Caption = 'All active connection threads sorted by users'
          end
          object PageHeader2Img: TTntImage
            Left = 0
            Top = 0
            Width = 24
            Height = 24
            Transparent = True
          end
          object Label3: TTntLabel
            Left = 34
            Top = -2
            Width = 100
            Height = 13
            Caption = 'User connections'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
        end
      end
    end
  end
  object UserPopupMenu: TTntPopupMenu
    OnPopup = UserPopupMenuPopup
    Left = 180
    Top = 150
    object KillUserMI: TTntMenuItem
      Caption = 'Kill User(s)'
      OnClick = KillUserBtnClick
    end
    object N1: TTntMenuItem
      Caption = '-'
    end
    object ShowUserInfoMI: TTntMenuItem
      Caption = 'Show Details'
      Checked = True
      OnClick = ShowUserInfoMIClick
    end
  end
  object ProcessPopupMenu: TTntPopupMenu
    Left = 148
    Top = 344
    object KillThreadMI: TTntMenuItem
      Caption = 'Kill thread'
      OnClick = KillThreadBtnClick
    end
  end
end
