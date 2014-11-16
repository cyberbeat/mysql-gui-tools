object AdminBackupProgressForm: TAdminBackupProgressForm
  Left = 684
  Top = 349
  BorderIcons = [biMinimize]
  BorderStyle = bsSingle
  Caption = 'Backup Progress ...'
  ClientHeight = 277
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object StoppingLbl: TTntLabel
    Left = 16
    Top = 246
    Width = 133
    Height = 13
    Caption = 'Stopping backup process ...'
    Visible = False
  end
  object ProgressGBox: TTntGroupBox
    Left = 16
    Top = 12
    Width = 391
    Height = 213
    Caption = 'Backup Progress'
    TabOrder = 0
    object Label1: TTntLabel
      Left = 18
      Top = 44
      Width = 118
      Height = 13
      Caption = 'Total Number of Objects:'
    end
    object TablesLbl: TTntLabel
      Left = 156
      Top = 44
      Width = 8
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TTntLabel
      Left = 18
      Top = 116
      Width = 71
      Height = 13
      Caption = 'Current Object:'
    end
    object Label3: TTntLabel
      Left = 18
      Top = 60
      Width = 67
      Height = 13
      Caption = 'Current Table:'
    end
    object CurrentTableLbl: TTntLabel
      Left = 156
      Top = 60
      Width = 8
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object CurrentTableNameLbl: TTntLabel
      Left = 156
      Top = 116
      Width = 5
      Height = 13
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TTntLabel
      Left = 18
      Top = 132
      Width = 109
      Height = 13
      Caption = 'Total Number of Rows:'
    end
    object TotalRowsLbl: TTntLabel
      Left = 156
      Top = 132
      Width = 8
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TTntLabel
      Left = 18
      Top = 148
      Width = 62
      Height = 13
      Caption = 'Current Row:'
    end
    object CurrentRowLbl: TTntLabel
      Left = 156
      Top = 148
      Width = 8
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ProjectCaptionLbl: TTntLabel
      Left = 18
      Top = 28
      Width = 76
      Height = 13
      Caption = 'Backup Project:'
    end
    object ProjectLbl: TTntLabel
      Left = 156
      Top = 28
      Width = 213
      Height = 13
      AutoSize = False
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object TablesProgressBar: TTntProgressBar
      Left = 18
      Top = 82
      Width = 351
      Height = 17
      TabOrder = 0
    end
    object RowProgressBar: TTntProgressBar
      Left = 18
      Top = 168
      Width = 351
      Height = 17
      TabOrder = 1
    end
  end
  object StopBtn: TTntButton
    Left = 332
    Top = 238
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = StopBtnClick
  end
end
