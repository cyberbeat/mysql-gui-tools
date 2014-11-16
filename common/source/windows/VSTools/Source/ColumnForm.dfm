object FormColumnSettings: TFormColumnSettings
  Left = 364
  Top = 252
  Width = 295
  Height = 370
  BorderIcons = [biSystemMenu]
  Caption = 'Column Settings'
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 295
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  DesignSize = (
    287
    336)
  PixelsPerInch = 120
  TextHeight = 16
  object Bevel1: TBevel
    Left = 20
    Top = 345
    Width = 310
    Height = 17
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Label2: TLabel
    Left = 23
    Top = 303
    Width = 190
    Height = 16
    Alignment = taCenter
    Anchors = [akBottom]
    Caption = 'The selected column should be '
    OnClick = FormCreate
  end
  object Label3: TLabel
    Left = 260
    Top = 303
    Width = 66
    Height = 16
    Anchors = [akBottom]
    Caption = 'pixels wide'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 43
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 279
      Height = 35
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      Caption = 
        'Check the columns you would like to make visible in this Folder.' +
        '  Drag and Drop to reorder the columns. '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object CheckBoxLiveUpdate: TCheckBox
    Left = 22
    Top = 330
    Width = 100
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Live Update'
    TabOrder = 1
    OnClick = CheckBoxLiveUpdateClick
  end
  object ButtonOk: TButton
    Left = 128
    Top = 374
    Width = 92
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 236
    Top = 374
    Width = 93
    Height = 31
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object VSTColumnNames: TVirtualStringTree
    Left = 10
    Top = 58
    Width = 336
    Height = 229
    Anchors = [akLeft, akTop, akRight, akBottom]
    CheckImageKind = ckDarkCheck
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    HintAnimation = hatNone
    TabOrder = 4
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toInitOnSave, toToggleOnDblClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
    OnChecking = VSTColumnNamesChecking
    OnDragAllowed = VSTColumnNamesDragAllowed
    OnDragOver = VSTColumnNamesDragOver
    OnDragDrop = VSTColumnNamesDragDrop
    OnFocusChanging = VSTColumnNamesFocusChanging
    OnFreeNode = VSTColumnNamesFreeNode
    OnGetText = VSTColumnNamesGetText
    OnInitNode = VSTColumnNamesInitNode
    Columns = <>
  end
  object EditPixelWidth: TEdit
    Left = 212
    Top = 300
    Width = 43
    Height = 21
    Anchors = [akBottom]
    TabOrder = 5
    OnExit = EditPixelWidthExit
    OnKeyPress = EditPixelWidthKeyPress
  end
end
