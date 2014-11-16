object ProgressForm: TProgressForm
  Left = 497
  Top = 388
  BorderIcons = [biMinimize]
  BorderStyle = bsSingle
  Caption = 'Progress ...'
  ClientHeight = 159
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StoppingLbl: TTntLabel
    Left = 16
    Top = 126
    Width = 94
    Height = 13
    Caption = 'Stopping process ...'
    Visible = False
  end
  object ProgressGBox: TTntGroupBox
    Left = 16
    Top = 12
    Width = 391
    Height = 93
    Caption = 'Progress'
    TabOrder = 0
    object ActionCaptionLbl: TTntLabel
      Left = 18
      Top = 28
      Width = 33
      Height = 13
      Caption = 'Action:'
    end
    object ActionLbl: TTntLabel
      Left = 116
      Top = 28
      Width = 253
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
    object InternalProgressBar: TTntProgressBar
      Left = 18
      Top = 52
      Width = 351
      Height = 17
      TabOrder = 0
    end
  end
  object StopBtn: TTntButton
    Left = 332
    Top = 118
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = StopBtnClick
  end
end
