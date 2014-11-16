object AdminRestoreProgressForm: TAdminRestoreProgressForm
  Left = 874
  Top = 400
  BorderIcons = [biMinimize]
  Caption = 'Restoring file...'
  ClientHeight = 349
  ClientWidth = 492
  Color = clBtnFace
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnKeyDown = TntFormKeyDown
  DesignSize = (
    492
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLbl: TTntLabel
    Left = 16
    Top = 313
    Width = 9
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '...'
    ExplicitTop = 284
  end
  object ProblemLabel: TTntLabel
    Left = 16
    Top = 172
    Width = 76
    Height = 13
    Caption = 'Problems found:'
    Visible = False
  end
  object ProgressGBox: TTntGroupBox
    Left = 16
    Top = 12
    Width = 460
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Restoring file'
    TabOrder = 0
    DesignSize = (
      460
      145)
    object Lbl1: TTntLabel
      Left = 18
      Top = 41
      Width = 108
      Height = 13
      Caption = 'Total Number of Bytes:'
    end
    object TotalBytesLbl: TTntLabel
      Left = 168
      Top = 41
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
    object Lbl2: TTntLabel
      Left = 18
      Top = 57
      Width = 133
      Height = 13
      Caption = 'Number of Bytes processed:'
    end
    object ProcBytesLbl: TTntLabel
      Left = 168
      Top = 57
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
    object BackupFileLbl: TTntLabel
      Left = 168
      Top = 25
      Width = 284
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      ShowAccelChar = False
      ExplicitWidth = 249
    end
    object BackupFileCaptionLbl: TTntLabel
      Left = 18
      Top = 25
      Width = 59
      Height = 13
      Caption = 'Backup File:'
    end
    object TntLabel1: TTntLabel
      Left = 17
      Top = 102
      Width = 63
      Height = 13
      Caption = 'Elapsed time:'
    end
    object TntLabel2: TTntLabel
      Left = 17
      Top = 122
      Width = 75
      Height = 13
      Caption = 'Remaining time:'
    end
    object ElapsedTimeLabel: TTntLabel
      Left = 113
      Top = 102
      Width = 24
      Height = 13
      Caption = 'none'
    end
    object RemainingTimeLabel: TTntLabel
      Left = 113
      Top = 122
      Width = 24
      Height = 13
      Caption = 'none'
    end
    object ProgressBar: TTntProgressBar
      Left = 18
      Top = 79
      Width = 434
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 401
    end
  end
  object StopButton: TTntButton
    Left = 401
    Top = 308
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Stop'
    TabOrder = 1
    OnClick = StopButtonClick
    ExplicitLeft = 368
    ExplicitTop = 279
  end
  object ProblemsMemo: TTntMemo
    Left = 16
    Top = 196
    Width = 462
    Height = 98
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    Visible = False
  end
  object CloseButton: TTntButton
    Left = 401
    Top = 308
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    Visible = False
    OnClick = CloseButtonClick
    ExplicitLeft = 368
    ExplicitTop = 279
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 440
    Top = 20
  end
end
