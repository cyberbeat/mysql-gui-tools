object AdminStartupVariablesInnoDBDatafilesForm: TAdminStartupVariablesInnoDBDatafilesForm
  Left = 390
  Top = 354
  BorderStyle = bsSingle
  Caption = 'AdminStartupVariablesInnoDBDatafilesForm'
  ClientHeight = 384
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DriveInfoCBox: TTntGroupBox
    Left = 22
    Top = 160
    Width = 581
    Height = 169
    Caption = 'Drive Info'
    TabOrder = 1
    object Label1: TTntLabel
      Left = 20
      Top = 62
      Width = 28
      Height = 13
      Caption = 'Drive:'
    end
    object Label2: TTntLabel
      Left = 20
      Top = 92
      Width = 69
      Height = 13
      Caption = 'Volume Name:'
    end
    object Label4: TTntLabel
      Left = 20
      Top = 108
      Width = 56
      Height = 13
      Caption = 'File System:'
    end
    object VolumeNameLbl: TTntLabel
      Left = 110
      Top = 92
      Width = 78
      Height = 13
      Caption = 'Volume Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object FileSystemLbl: TTntLabel
      Left = 110
      Top = 108
      Width = 65
      Height = 13
      Caption = 'File System'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TTntLabel
      Left = 20
      Top = 140
      Width = 58
      Height = 13
      Caption = 'Free Space:'
    end
    object FreeSpaceLbl: TTntLabel
      Left = 110
      Top = 140
      Width = 66
      Height = 13
      Caption = 'Free Space'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TTntLabel
      Left = 20
      Top = 124
      Width = 50
      Height = 13
      Caption = 'Total Size:'
    end
    object TotalSizeLbl: TTntLabel
      Left = 110
      Top = 124
      Width = 58
      Height = 13
      Caption = 'Total Size'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label10: TTntLabel
      Left = 20
      Top = 16
      Width = 291
      Height = 40
      AutoSize = False
      Caption = 
        'Select a drive to get infomation about the File System and the a' +
        'vailable free space.'
      WordWrap = True
    end
    object DiskChart: TChart
      Left = 308
      Top = 12
      Width = 263
      Height = 141
      AllowPanning = pmNone
      AllowZoom = False
      BackWall.Brush.Color = clWhite
      BackWall.Brush.Style = bsClear
      BackWall.Pen.Visible = False
      Title.AdjustFrame = False
      Title.Text.Strings = (
        'TChart')
      Title.Visible = False
      AxisVisible = False
      Chart3DPercent = 30
      ClipPoints = False
      Frame.Visible = False
      Legend.ShadowColor = clGray
      Legend.ShadowSize = 1
      Legend.TextStyle = ltsPlain
      View3DOptions.Elevation = 315
      View3DOptions.Orthogonal = False
      View3DOptions.Perspective = 0
      View3DOptions.Rotation = 360
      View3DWalls = False
      BevelOuter = bvNone
      TabOrder = 0
      object Series1: TPieSeries
        Marks.ArrowLength = 8
        Marks.Visible = False
        SeriesColor = clRed
        Title = 'Space'
        Circled = True
        OtherSlice.Text = 'Other'
        PieValues.DateTime = False
        PieValues.Name = 'Pie'
        PieValues.Multiplier = 1.000000000000000000
        PieValues.Order = loAscending
      end
    end
    object DriveCBox: TTntComboBox
      Left = 110
      Top = 58
      Width = 45
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'C:'
      OnChange = DriveCBoxChange
      Items.Strings = (
        'C:'
        'D:'
        'E:'
        'F:')
    end
  end
  object OKBtn: TTntButton
    Left = 440
    Top = 344
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TTntButton
    Left = 528
    Top = 344
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object TableSpaceGBox: TTntGroupBox
    Left = 22
    Top = 12
    Width = 581
    Height = 141
    Caption = 'Data File'
    TabOrder = 0
    object Label7: TTntLabel
      Left = 16
      Top = 102
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label9: TTntLabel
      Left = 16
      Top = 28
      Width = 42
      Height = 13
      Caption = 'Data file:'
    end
    object Label3: TTntLabel
      Left = 16
      Top = 51
      Width = 549
      Height = 46
      AutoSize = False
      Caption = 
        'Data file name with optional path information. Make sure that th' +
        'e name forms a valid filename when combined with the data path v' +
        'alue (see Data Directory option in InnodDB Parameters).'
      WordWrap = True
    end
    object Label5: TTntLabel
      Left = 198
      Top = 98
      Width = 367
      Height = 39
      AutoSize = False
      Caption = 
        'Size of the data file. Remember that you can select to auto - ex' +
        'tend the last specified data file.'
      WordWrap = True
    end
    object SizeEd: TTntEdit
      Left = 70
      Top = 98
      Width = 47
      Height = 21
      TabOrder = 1
      Text = '10'
      OnChange = SizeEdChange
    end
    object SizeUpDown: TTntUpDown
      Left = 117
      Top = 98
      Width = 16
      Height = 21
      Associate = SizeEd
      Max = 32000
      Position = 10
      TabOrder = 2
    end
    object UnitCBox: TTntComboBox
      Left = 140
      Top = 98
      Width = 37
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 3
      Text = 'M'
      Items.Strings = (
        'k'
        'M')
    end
    object TablespaceEd: TTntEdit
      Left = 70
      Top = 24
      Width = 495
      Height = 21
      TabOrder = 0
      OnChange = TablespaceEdChange
    end
  end
end
