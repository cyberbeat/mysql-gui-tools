object OptionsForm: TOptionsForm
  Left = 197
  Top = 9
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 539
  ClientWidth = 698
  Color = clBtnFace
  Constraints.MaxWidth = 706
  Constraints.MinWidth = 704
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    698
    539)
  PixelsPerInch = 96
  TextHeight = 16
  object HeaderPnl: TTntPanel
    Left = 134
    Top = 14
    Width = 549
    Height = 24
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 0
    DesignSize = (
      549
      24)
    object HeaderBGShape: TTntShape
      Left = 0
      Top = 0
      Width = 550
      Height = 24
      Anchors = [akLeft, akTop, akRight, akBottom]
      Brush.Color = 5066061
      Pen.Style = psClear
    end
    object HeaderImg: TTntImage
      Left = 1
      Top = 1
      Width = 547
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      Picture.Data = {
        07544269746D6170D2180000424DD21800000000000036000000280000006400
        00001500000001001800000000009C180000120B0000120B0000000000000000
        0000FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9
        FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFC
        F9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FC
        FCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9
        FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFC
        F9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FC
        FCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9
        FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFC
        F9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FC
        FCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCF9FCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFC
        FCFCFCFCFCFCFAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9
        F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FA
        F9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9
        FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9
        F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FA
        F9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9
        FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9
        F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FA
        F9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9
        FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9FAF9F9F7F6F7F7F6F7F7F6F7F7F6F7F7F6
        F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7
        F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7
        F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6
        F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7
        F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7
        F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6
        F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7
        F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7
        F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F7F6F7F5F2
        F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5
        F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2
        F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2
        F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5
        F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2
        F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2
        F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5
        F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2
        F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2F2F5F2
        F2F5F2F2F5F2F2F5F2F2F0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0
        EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFED
        F0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EF
        EDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0
        EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFED
        F0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EF
        EDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0
        EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFED
        F0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EF
        EDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EFEDF0EAE9F0EAE9F0EAE9F0
        EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9
        F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EA
        E9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0
        EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9
        F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EA
        E9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0
        EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9
        F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EA
        E9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0EAE9F0
        EAE9EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5
        EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7
        E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EE
        E7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5
        EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7
        E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EE
        E7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5
        EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7
        E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EEE7E5EE
        E7E5EEE7E5EEE7E5EEE7E5EEE7E5EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3
        EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5
        E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EA
        E5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3
        EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5
        E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EA
        E5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3
        EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5
        E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EA
        E5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3EAE5E3E9E0DFE9E0DF
        E9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0
        DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9
        E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DF
        E9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0
        DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9
        E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DF
        E9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0
        DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9
        E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DFE9E0DF
        E9E0DFE9E0DFE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DF
        DDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7
        DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDD
        E7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DF
        DDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7
        DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDD
        E7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DF
        DDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7
        DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDD
        E7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DFDDE7DDDBE7DDDBE7DDDBE7DDDBE7DD
        DBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7
        DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDB
        E7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DD
        DBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7
        DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDB
        E7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DD
        DBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7
        DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDB
        E7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE7DDDBE5D9
        D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5
        D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9
        E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9
        D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5
        D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9
        E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9
        D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5
        D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9
        E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9D9E5D9
        D9E5D9D9E5D9D9E5D9D9E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2
        D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6
        E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6
        D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2
        D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6
        E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6
        D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2
        D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6
        E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6
        D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6E2D6D6EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1
        E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EA
        E1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0EAE1E0
        EAE1E0EAE1E0F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4
        F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9
        F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3
        F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4
        F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9
        F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3
        F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4
        F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9
        F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3
        F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3F9F4F3FDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFB
        FAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFD
        FBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA
        FDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFAFDFBFA}
      Stretch = True
    end
    object TitleLbl: TTntLabel
      Left = 6
      Top = 5
      Width = 45
      Height = 13
      Caption = 'General'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object OptionPageControl: TTntPageControl
    Left = 130
    Top = 38
    Width = 559
    Height = 486
    ActivePage = GeneralTabSheet
    Anchors = [akLeft, akTop, akBottom]
    Style = tsFlatButtons
    TabHeight = 16
    TabOrder = 1
    OnChange = OptionPageControlChange
    ExplicitHeight = 478
    object GeneralTabSheet: TTabSheet
      Caption = 'General'
      ExplicitHeight = 452
      object GroupBox1: TTntGroupBox
        Left = 0
        Top = 6
        Width = 549
        Height = 119
        Caption = 'Applications Options'
        TabOrder = 0
        object Label3: TTntLabel
          Left = 326
          Top = 22
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = 'Language:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ShowTipOfDayCBox: TTntCheckBox
          Left = 16
          Top = 44
          Width = 257
          Height = 17
          Caption = 'Show Tip of Day'
          Enabled = False
          TabOrder = 0
          OnClick = DoPageContentChanged
        end
        object StoreWindowsPosCBox: TTntCheckBox
          Left = 16
          Top = 20
          Width = 257
          Height = 17
          Caption = 'Store Windows Positions'
          TabOrder = 1
          OnClick = DoPageContentChanged
        end
        object LanguageCBox: TTntComboBox
          Left = 384
          Top = 18
          Width = 147
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 2
          Text = 'English'
          OnCloseUp = DoPageContentChanged
          Items.Strings = (
            'English')
        end
        object DisableTransparencyEffectsCBox: TTntCheckBox
          Left = 16
          Top = 68
          Width = 257
          Height = 17
          Caption = 'Disable transparency effects'
          TabOrder = 3
          OnClick = DoPageContentChanged
        end
      end
      object GroupBox2: TTntGroupBox
        Left = 0
        Top = 198
        Width = 549
        Height = 131
        Caption = 'Application Fonts'
        TabOrder = 1
        object Label2: TTntLabel
          Left = 7
          Top = 24
          Width = 74
          Height = 16
          Alignment = taRightJustify
          Caption = 'Default Font:'
        end
        object TntLabel1: TTntLabel
          Left = 20
          Top = 60
          Width = 61
          Height = 16
          Alignment = taRightJustify
          Caption = 'Data Font:'
        end
        object TntLabel2: TTntLabel
          Left = 16
          Top = 96
          Width = 65
          Height = 16
          Alignment = taRightJustify
          Caption = 'Code Font:'
        end
        object TntLabel3: TTntLabel
          Left = 330
          Top = 24
          Width = 26
          Height = 16
          Alignment = taRightJustify
          Caption = 'Size'
        end
        object TntLabel4: TTntLabel
          Left = 330
          Top = 60
          Width = 26
          Height = 16
          Alignment = taRightJustify
          Caption = 'Size'
        end
        object TntLabel5: TTntLabel
          Left = 330
          Top = 96
          Width = 26
          Height = 16
          Alignment = taRightJustify
          Caption = 'Size'
        end
        object TntLabel7: TTntLabel
          Left = 227
          Top = 96
          Width = 37
          Height = 16
          Alignment = taRightJustify
          Caption = 'Width:'
        end
        object TntLabel8: TTntLabel
          Left = 433
          Top = 96
          Width = 11
          Height = 16
          Alignment = taRightJustify
          Caption = 'pt'
        end
        object TntLabel9: TTntLabel
          Left = 433
          Top = 60
          Width = 11
          Height = 16
          Alignment = taRightJustify
          Caption = 'pt'
        end
        object TntLabel10: TTntLabel
          Left = 433
          Top = 24
          Width = 11
          Height = 16
          Alignment = taRightJustify
          Caption = 'pt'
        end
        object ApplicationFontBtn: TTntButton
          Left = 452
          Top = 20
          Width = 81
          Height = 21
          Caption = 'Choose ...'
          TabOrder = 0
          OnClick = ApplicationFontBtnClick
        end
        object DataFontBtn: TTntButton
          Left = 452
          Top = 56
          Width = 81
          Height = 21
          Caption = 'Choose ...'
          TabOrder = 1
          OnClick = DataFontBtnClick
        end
        object CodeFontBtn: TTntButton
          Left = 452
          Top = 92
          Width = 81
          Height = 21
          Caption = 'Choose ...'
          TabOrder = 2
          OnClick = CodeFontBtnClick
        end
        object DefaultFontLU: TTntComboBox
          Left = 88
          Top = 20
          Width = 225
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          Sorted = True
          TabOrder = 3
          OnChange = DoPageContentChanged
        end
        object DefaultFontSizeLU: TTntComboBox
          Left = 368
          Top = 20
          Width = 61
          Height = 24
          ItemHeight = 16
          TabOrder = 4
          Text = '8'
          OnChange = DoPageContentChanged
          Items.Strings = (
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14')
        end
        object DataFontSizeLU: TTntComboBox
          Left = 368
          Top = 56
          Width = 61
          Height = 24
          ItemHeight = 16
          TabOrder = 5
          Text = '8'
          OnChange = DoPageContentChanged
          Items.Strings = (
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14')
        end
        object DataFontLU: TTntComboBox
          Left = 88
          Top = 56
          Width = 225
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          Sorted = True
          TabOrder = 6
          OnChange = DoPageContentChanged
        end
        object CodeFontLU: TTntComboBox
          Left = 88
          Top = 92
          Width = 133
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          Sorted = True
          TabOrder = 7
          OnChange = DoPageContentChanged
        end
        object CodeFontSizeLU: TTntComboBox
          Left = 368
          Top = 92
          Width = 61
          Height = 24
          ItemHeight = 16
          TabOrder = 8
          Text = '8'
          OnChange = DoPageContentChanged
          Items.Strings = (
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14')
        end
        object CodeFontWidthLU: TTntComboBox
          Left = 268
          Top = 92
          Width = 45
          Height = 24
          ItemHeight = 16
          TabOrder = 9
          Text = '8'
          OnChange = DoPageContentChanged
          Items.Strings = (
            '6'
            '7'
            '8'
            '9'
            '10'
            '11'
            '12'
            '13'
            '14')
        end
      end
      object TntGroupBox1: TTntGroupBox
        Left = 0
        Top = 131
        Width = 549
        Height = 61
        Caption = 'Password Storage'
        TabOrder = 2
        object PasswordStorageTypeLbl: TTntLabel
          Left = 215
          Top = 27
          Width = 160
          Height = 16
          Alignment = taRightJustify
          Caption = 'Password storage method:'
        end
        object StorePasswordCBox: TTntCheckBox
          Left = 16
          Top = 26
          Width = 193
          Height = 17
          Caption = 'Store passwords'
          TabOrder = 0
          OnClick = StorePasswordCBoxClick
        end
        object PasswordStorageTypeCBox: TTntComboBox
          Left = 382
          Top = 23
          Width = 147
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          TabOrder = 1
          OnCloseUp = DoPageContentChanged
        end
      end
      object TntGroupBox2: TTntGroupBox
        Left = 0
        Top = 342
        Width = 549
        Height = 85
        Caption = 'Warnings and Messages'
        TabOrder = 3
        object TntLabel6: TTntLabel
          Left = 24
          Top = 24
          Width = 57
          Height = 16
          Alignment = taRightJustify
          Caption = 'Ignorelist:'
        end
        object IgnoreWarningsList: TTntListBox
          Left = 90
          Top = 20
          Width = 339
          Height = 47
          ItemHeight = 16
          Sorted = True
          TabOrder = 0
        end
        object RemoveWarningFromIgnoreListBtn: TTntButton
          Left = 448
          Top = 20
          Width = 81
          Height = 21
          Caption = 'Remove'
          TabOrder = 1
          OnClick = RemoveWarningFromIgnoreListBtnClick
        end
      end
    end
    object ConnectionsTabSheet: TTabSheet
      Caption = 'Connections'
      ImageIndex = 1
      ExplicitHeight = 471
      object ConnsTreeView: TTntTreeView
        Left = 0
        Top = 10
        Width = 211
        Height = 345
        DragMode = dmAutomatic
        HideSelection = False
        Images = OptionTreeImageList
        Indent = 19
        ReadOnly = True
        ShowLines = False
        ShowRoot = False
        TabOrder = 0
        OnChange = ConnsTreeViewChange
        OnChanging = ConnsTreeViewChanging
        OnDragDrop = ConnsTreeViewDragDrop
        OnDragOver = ConnsTreeViewDragOver
      end
      object ConnectionPageControl: TTntPageControl
        Left = 218
        Top = 10
        Width = 331
        Height = 345
        ActivePage = ConnGeneralTabSheet
        TabOrder = 1
        object ConnGeneralTabSheet: TTabSheet
          Caption = 'Connection Parameters'
          object ConnectionLbl: TTntLabel
            Left = 4
            Top = 13
            Width = 70
            Height = 16
            Caption = 'Connection:'
          end
          object UsernameLbl: TTntLabel
            Left = 4
            Top = 45
            Width = 66
            Height = 16
            Caption = 'Username:'
          end
          object PasswordLbl: TTntLabel
            Left = 4
            Top = 77
            Width = 63
            Height = 16
            Caption = 'Password:'
          end
          object PortLbl: TTntLabel
            Left = 4
            Top = 141
            Width = 27
            Height = 16
            Caption = 'Port:'
          end
          object HostnameLbl: TTntLabel
            Left = 4
            Top = 109
            Width = 65
            Height = 16
            Caption = 'Hostname:'
          end
          object TypeLbl: TTntLabel
            Left = 139
            Top = 141
            Width = 35
            Height = 16
            Alignment = taRightJustify
            Caption = 'Type:'
          end
          object SchemataEdLbl: TTntLabel
            Left = 4
            Top = 175
            Width = 53
            Height = 16
            Caption = 'Schema:'
          end
          object Label1: TTntLabel
            Left = 4
            Top = 205
            Width = 39
            Height = 16
            Caption = 'Notes:'
          end
          object PasswordEd: TTntEdit
            Left = 92
            Top = 74
            Width = 217
            Height = 24
            Hint = 'Enter the password of the user account'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
            OnChange = DoPageContentChanged
          end
          object PortEd: TTntEdit
            Left = 92
            Top = 136
            Width = 39
            Height = 24
            Hint = 'Specify the TCP/IP port to connect to'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
            Text = '3306'
            OnChange = DoPageContentChanged
          end
          object TypeCBox: TTntComboBox
            Tag = -1
            Left = 180
            Top = 138
            Width = 129
            Height = 24
            Hint = 'Select the type of database connection'
            Style = csDropDownList
            Enabled = False
            ItemHeight = 16
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
            OnChange = DoPageContentChanged
            Items.Strings = (
              'MySQL'
              'Oracle'
              'ODBC')
          end
          object SchemataEd: TTntEdit
            Left = 92
            Top = 174
            Width = 216
            Height = 24
            Hint = 'Enter the schema to select after the connection is established'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 6
            OnChange = DoPageContentChanged
          end
          object UsernameEd: TTntEdit
            Left = 92
            Top = 42
            Width = 217
            Height = 24
            TabOrder = 1
            OnChange = DoPageContentChanged
          end
          object ConnectionEd: TTntEdit
            Left = 92
            Top = 10
            Width = 217
            Height = 24
            TabOrder = 0
            OnChange = DoPageContentChanged
          end
          object HostnameEd: TTntEdit
            Left = 92
            Top = 106
            Width = 217
            Height = 24
            TabOrder = 3
            OnChange = DoPageContentChanged
          end
          object ConnMemo: TTntMemo
            Left = 92
            Top = 204
            Width = 217
            Height = 101
            TabOrder = 7
          end
        end
        object ConnAdvTabSheet: TTabSheet
          Caption = 'Advanced Parameters'
          ImageIndex = 1
          object Label5: TTntLabel
            Left = 12
            Top = 8
            Width = 135
            Height = 16
            Caption = 'Advanced Parameters'
          end
          object ConnValueListEditor: TValueListEditor
            Left = 12
            Top = 22
            Width = 297
            Height = 283
            DisplayOptions = [doKeyColFixed]
            KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
            ScrollBars = ssVertical
            TabOrder = 0
            TitleCaptions.Strings = (
              'Parameter'
              'Value')
            OnStringsChange = DoPageContentChanged
            ColWidths = (
              146
              145)
          end
        end
      end
      object AddConnectionButton: TTntButton
        Left = 100
        Top = 363
        Width = 140
        Height = 25
        Caption = 'New Connection'
        TabOrder = 2
        OnClick = AddConnectionButtonClick
      end
      object DeleteConnectionButton: TTntButton
        Left = 410
        Top = 363
        Width = 140
        Height = 25
        Caption = 'Delete Connection'
        TabOrder = 3
        OnClick = DeleteConnectionButtonClick
      end
      object CloneConnectionButton: TTntButton
        Left = 255
        Top = 363
        Width = 140
        Height = 25
        Caption = 'Duplicate Connection'
        TabOrder = 4
        OnClick = CloneConnectionButtonClick
      end
    end
    object EditorsTabSheet: TTabSheet
      Caption = 'Editors'
      ImageIndex = 2
      ExplicitHeight = 452
      DesignSize = (
        551
        460)
      object GroupBox4: TTntGroupBox
        Left = 0
        Top = 93
        Width = 549
        Height = 324
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Table Editor'
        TabOrder = 1
        DesignSize = (
          549
          324)
        object Label6: TTntLabel
          Left = 27
          Top = 154
          Width = 70
          Height = 16
          Alignment = taRightJustify
          Caption = 'PK Naming:'
        end
        object Label7: TTntLabel
          Left = 15
          Top = 186
          Width = 82
          Height = 16
          Alignment = taRightJustify
          Caption = 'Index naming:'
        end
        object Label8: TTntLabel
          Left = 31
          Top = 218
          Width = 66
          Height = 16
          Alignment = taRightJustify
          Caption = 'FK naming:'
        end
        object Label9: TTntLabel
          Left = 301
          Top = 154
          Width = 76
          Height = 16
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'PK datatype:'
        end
        object Label10: TTntLabel
          Left = 291
          Top = 186
          Width = 86
          Height = 16
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Def. data type:'
        end
        object TntLabel11: TTntLabel
          Left = 114
          Top = 97
          Width = 135
          Height = 16
          Alignment = taRightJustify
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Default storage engine'
        end
        object EdTblShowSQLBeforeApplyingCBox: TTntCheckBox
          Left = 16
          Top = 21
          Width = 525
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Show SQL command before applying changes'
          TabOrder = 0
          OnClick = DoPageContentChanged
        end
        object EdTblAllColsNotNullCBox: TTntCheckBox
          Left = 16
          Top = 44
          Width = 521
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'All columns Not Null per default'
          TabOrder = 1
          OnClick = DoPageContentChanged
        end
        object EdTblPKAutoNamingEd: TTntComboBox
          Left = 104
          Top = 150
          Width = 147
          Height = 24
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 4
          Text = 'id%tablename%'
          OnChange = DoPageContentChanged
          Items.Strings = (
            'id%tablename%'
            'id_%tablename%'
            '%tablename%id'
            '%tablename%_id')
        end
        object EdTblAllIntUnsignedCBox: TTntCheckBox
          Left = 16
          Top = 68
          Width = 525
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'All integer columns unsigned per default'
          TabOrder = 2
          OnClick = DoPageContentChanged
        end
        object EdTblIndexNamingEd: TTntComboBox
          Left = 104
          Top = 182
          Width = 147
          Height = 24
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 5
          Text = 'index_%nr%'
          OnChange = DoPageContentChanged
          Items.Strings = (
            'index_%nr%'
            'index%nr%'
            'ind_%tablename%_%nr%')
        end
        object EdTblFKNamingEd: TTntComboBox
          Left = 104
          Top = 214
          Width = 147
          Height = 24
          ItemHeight = 16
          ItemIndex = 0
          TabOrder = 6
          Text = 'FK_%tablename%_%nr%'
          OnChange = DoPageContentChanged
          Items.Strings = (
            'FK_%tablename%_%nr%'
            'FK%tablename%%nr%')
        end
        object EdTblPKDatatypeEd: TTntComboBox
          Left = 384
          Top = 150
          Width = 147
          Height = 24
          Anchors = [akTop, akRight]
          ItemHeight = 16
          TabOrder = 7
          Text = 'INTEGER'
          OnChange = DoPageContentChanged
        end
        object DefaultDatatypeComboBox: TTntComboBox
          Left = 384
          Top = 182
          Width = 147
          Height = 24
          Anchors = [akTop, akRight]
          ItemHeight = 16
          TabOrder = 8
          Text = 'VARCHAR(45)'
          OnChange = DoPageContentChanged
        end
        object DefaultStorageEngineLU: TTntComboBox
          Left = 16
          Top = 93
          Width = 87
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          TabOrder = 3
          OnChange = DoPageContentChanged
          Items.Strings = (
            'None'
            'InnoDB'
            'MyISAM'
            'Falcon'
            'NDB'
            'Memory'
            'Merge'
            'BDB'
            'ISAM')
        end
      end
      object GroupBox3: TGroupBox
        Left = 1
        Top = 0
        Width = 547
        Height = 81
        Caption = 'General Settings'
        TabOrder = 0
        object KeepRoutineEditorOnTopCBox: TCheckBox
          Left = 15
          Top = 24
          Width = 274
          Height = 17
          Caption = 'Always keep editor windows on top'
          TabOrder = 0
          OnClick = DoPageContentChanged
        end
        object ShowSpecialCharsCheckBox: TCheckBox
          Left = 15
          Top = 52
          Width = 274
          Height = 17
          Caption = 'Show tab and line breaks in editor'
          TabOrder = 1
          OnClick = DoPageContentChanged
        end
      end
    end
  end
  object CoverPnl: TTntPanel
    Left = 118
    Top = 14
    Width = 16
    Height = 23
    BevelOuter = bvNone
    TabOrder = 5
  end
  object CloseBtn: TTntButton
    Left = 585
    Top = 497
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 4
    OnClick = CloseBtnClick
    ExplicitTop = 489
  end
  object ApplyChangesBtn: TTntButton
    Left = 358
    Top = 497
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Enabled = False
    TabOrder = 2
    OnClick = ApplyChangesBtnClick
    ExplicitTop = 489
  end
  object DiscardChangesBtn: TTntButton
    Left = 472
    Top = 497
    Width = 100
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Discard'
    Enabled = False
    TabOrder = 3
    OnClick = DiscardChangesBtnClick
    ExplicitTop = 489
  end
  object SidebarScrollBox: TTntScrollBox
    Left = 14
    Top = 14
    Width = 103
    Height = 462
    HorzScrollBar.Visible = False
    Anchors = [akLeft, akTop, akBottom]
    Color = clWindow
    ParentColor = False
    TabOrder = 6
    ExplicitHeight = 473
  end
  object Panel1: TTntPanel
    Left = 16
    Top = 16
    Width = 99
    Height = 17
    Caption = 'Category'
    TabOrder = 7
  end
  object OptionTreeImageList: TImageList
    Left = 70
    Top = 208
    Bitmap = {
      494C010107000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000ECE6DE00BAB5AB00B2AFAF00D1D2D500FCFDFF00000000000000
      00000000000000000000000000000000000000000000EDEDED00E6E6E600E5E5
      E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5E500E5E5
      E500E8E8E800F2F2F200FCFCFC00FFFFFF000000000000000000000000000000
      0000000000000000000000000000DECFCE00B59A9C00AD868C009C7D7B009475
      730094757B00AD969400E7DBDE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F3E8D900C1A46800B59A5B00A2906D00A6A09B00F1F3F6000000
      000000000000000000000000000000000000F4F4F400DEDEDE00D1D1D100D0D0
      D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0D000D0D0
      D000CCCCCC00DDDDDD00F5F5F500FFFFFF000000000000000000000000000000
      000000000000FFFBFF00CEBABD00BDAEB500D6CBCE00DED3D600DECFCE00CEAA
      AD00BD969C00AD8284008C656B00D6C7C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F1F2
      F500CBCACC00B9B5B200B7A68900CBAE5D00D4B96C00C1A86F00A79D9000BEBD
      BF00E8EAED0000000000000000000000000076C3DD00189AC600189AC600189A
      C600189AC600189AC600189AC600189AC600189AC600189AC600189AC600189A
      C60040A8CB00CCCCCC00EDEDED00FEFEFE000000000000000000000000000000
      000000000000E7DFDE00D6CBCE00EFEFEF00EFEBEF00E7E3E700D6C7C600C6B2
      B500A58A8C00C69E9C00BD8A8C00A58284000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DAD9DB00ACA0
      8F00B7A37700CCB57800D6C18200D6BF7D00D6BF7B00DAC48100D6C08600B8A4
      7A00998D7900C5C5C8000000000000000000189AC6001B9CC80059C8ED0060CC
      F10060CCF10060CCF10060CCF10060CCF10060CCF10060CCF10060CCF10063CC
      F0001A9BC700B2C0C400E1E1E100000000000000000000000000000000000000
      000000000000E7DBDE00E7E3E70000000000FFFBFF00E7EBEF00D6C3BD00C6AA
      A500AD828400C6969400CE9A9C00BDA6A5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DDDBDC00B4A17C00BFA2
      5100C1A34800CAAE5E00D5BD7900DEC98D00E2CF9900E1CE9600DCC78600D8C1
      7C00D0B87500A28D6300C2C1C20000000000189AC60026A3CE0051C0E5006BD1
      F2006BD1F2006BD1F2006BD1F2006BD1F2006BD1F2006BD1F2006BD1F2006ED2
      F10029A4CD007BB7CB00D5D5D500F3F3F3000000000000000000000000000000
      000000000000F7F7FF00EFE3DE00EFD7D600DECFD600DEDBDE00CEB2B500C68A
      8400B5716B00C68E8C00CEB2AD00F7EFEF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0EFF000B7A37900AC8B2B00AC88
      2000BD9F4400D0B66E00DDC78900E6D4A000EDDDB000EBDBAC00E3D3A300DACC
      A600CCB46E00C1A04300A2895200DCDCDE00189AC60033ACD4004ABADE007AD7
      F3007AD7F3007AD7F3007AD7F3007AD7F3007AD7F3007AD7F3007AD7F3007ED7
      F3004DB8DB0038A6CB00CCCCCC00ECECEC00000000000000000000000000DED3
      CE00BDA29C00B5928C00B5969400C6968C00E7AA6B00DEAE9C00CE929400BD6D
      7300C6615A00B5757300F7F3F700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D5CAB7009C7D25009A760600AE8B
      2600C1A34B00D1B97400DBC99600E5D6AB00F1E4BC00F0E1B500E5D5A700DFD4
      B500CFB87700BA9A3B00A8821700BAAB8F00189AC60043B9DC003FB3D8008CDF
      F5008CDFF5008CDFF5008CDFF5008CDFF5008CDFF5008CDFF5008CDFF5008EDF
      F5006CC6E20047AFD200AFCBD400EDEDED0000000000FFF7EF00D6967300BD5D
      2900B5593100BD552900AD594200CE8E7300FFAA1800FFB23100F7B66300E7A2
      7300C679730094596300B59AA500E7E3E7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B8A47500886702009A740600AD8A
      2400BE9F4400D1BC8000E2D7BD00E5D6AD00ECDCAD00E8D9AC00E1D3AF00DAC5
      8D00CCB16300B8983A00A37D0D00B1955400189AC60059C9E60031AAD100A5E8
      F700A5E8F700A5E8F700A5E8F700A5E8F700A5E8F700A5E8F700A5E8F700A6E6
      F6007EC9E20096D0E40075C0D9000000000000000000D6925A00A53C00009445
      08006B5D1800A5410000AD593100DEA67300FFB23100FFB63900FFB63900FFBE
      4200FFBA5200E7A66300B5715A0094696B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A28A4C00805F000096710300A682
      1700B5943300CCB57500D7C38900DEC88C00E0CC9200E0D1A800DCC99100D0B6
      6B00C3A65000B18F2D009E790800AD8F4200189AC60071DCF00020A0CA00B8F0
      F900B8F0F900B8F0F900B8F0F900B8F0F900B8F0F900B8F0F900B8F0F900B9EF
      F8008BCCE200E4EEF1001D9CC700FAFDFE00EFD3AD00AD410000AD450000AD51
      0000186D0800525D0000B5694A00E7B27B00FFBE4200FFC35200FFC75A00FFC7
      5A00FFC35200FFC34A00FFB64A00B5716B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A68F55007D600500A3862D00AD8C
      2D00B3933300C5AC6700CDB46C00D1B86D00D4BE7F00DAC99B00D2B97100CCB2
      6600C3A75600B799410096700000B79C5A00189AC60084EBF70044BBDA00189A
      C600189AC600189AC600189AC600189AC600189AC600189AC600189AC600189A
      C600189AC60028A1CA0087CAE100FFFFFF00D6925A00BD490000DE650000AD75
      00000086000031710000CE966B00EFBA7B00FFCF6300FFCF7300FFCF6B00FFD3
      7300FFCF6B00FFCF6300EFB25200B5827B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D5C8AC00775A0200AF985600BCA3
      5B00C3AB6500D0BC8700D0BA7C00D3BF8400DDCDA500DAC89400D5C08500CFBA
      7C00CAB37100BAA052008C660000DED0B400189AC6008EF3FA008EF3FA008EF3
      FA008EF3FA008EF3FA00ADF6FB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F1F9
      FC0026A0C900FFFFFF00FFFFFF00FFFFFF00D6863100D66500009C8600006396
      0800639A0000E7860000D6AA8C00F7C77B00FFD37300FFDB8C00FFDF8C00FFDF
      8C00FFD78400FFD77300E7AA6300B59294000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A891580096803900CBBC
      8D00D2C29600D8C9A100DBCDA700E0D3B200DDCDA100D8C89600D7C69400D5C3
      9000D6C594009F812600B99D5D0000000000189AC600CBFDFE0096FAFD0096FA
      FD0096FAFD0096FAFD00B1E3F000189AC600189AC600189AC600189AC60026A0
      C90085CAE100FFFFFF00FFFFFF00FFFFFF00EF963900AD7D000010B2290021B6
      3900C6B63900FFAA3900CEBAAD00FFCF7B00FFE39400FFE79400FFEBA500FFEB
      A500FFE79400FFE38C00DE9E6B00BD9EAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FBF8F500AD976000AB97
      5D00DCD3B700E9E1CC00E7DEC300E4D9B900E4D8B600E5DABA00E3D9B800D9CD
      A500B2985100B99E5F00FDFAF8000000000077C4DD008ACCE200FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0046AED100B6DFED00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7C37B005A96100029CB5A004AD7
      73007BDB7B00EFCF9C00D6C3B500F7CF8400FFEB9C00FFF3B500FFF7C600FFF3
      C600FFEFB500FFEB9400CE967300C6AEB5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFDFC00D3C5
      A500BFAB7C00BDAA7800C8B88C00CFC19900D0C19800CAB98C00C1AC7700C5AE
      7A00D8C9A600FFFEFE000000000000000000ECF7FA0077C4DD00189AC600189A
      C600189AC600189AC60077C4DD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7E70042C352004AD36B00B5EF
      AD00BDF7B500CEDBBD00D6C7D600BDAEA500CEB69C00D6C7A500E7D7BD00F7EB
      C600FFEFBD00FFEBA500C6927300CEBEC6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFEFE00EDE4D500DBCDB100D4C39F00D5C39F00DDCEB100EFE6D6000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000DEEFC6007BDB84009CEB
      A500A5EBA5004ACF7B0073B26B00D6AA7B00CEC3BD00C6B6BD00C6AEAD00C6A6
      A500CEA69C00DEAE9400AD827B00DED3D6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000F7FBE700C6EF
      BD00ADDF8C0094CF6B0094C76300F7D7AD00FFF7F70000000000F7F3F700EFE3
      E700E7D7DE00D6C3C600CEBABD00FFFBFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F7F3
      F300E0DCDC00CFCCCD00D7D9D900E0E2E300E6E7E800EAEBEB00EFF0F000F2F3
      F300F5F6F600FBFBFB00FEFEFE000000000000000000B4B4B400929292009595
      95009595950098949200859EA7004A95BA004087AD007A8B9500999694009595
      950095959500939393009E9E9E00F3F3F3000000000000000000000000000000
      0000F3F5F800CAC9CA00B1ACA800A9A29900A7A09600AAA6A100C2C1C300F0F1
      F400000000000000000000000000000000000000000000000000FFFFFE00D0C7
      BB00E3E4E8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FAF5F400E3CE
      C300C0ADA200AB999100A7948E00A7959100A8989500AB9D9B00ADA3A100B1AA
      A900C0BDBE00D9D9D900F8F8F80000000000EFEFEF009B9B9B009E9E9E009F9F
      9F009F9F9F00A49E9B0078A4B2007AD8F30071C5E30068829600A7A29E009F9F
      9F009F9F9F00A3A3A30079797900D8D8D800000000000000000000000000D6D5
      D800ACA19100B4A07300C2A96A00C8AD6800C9AF6900C2AA6A00AC976900988E
      7D00C7C8CB00000000000000000000000000000000000000000000000000CFB6
      830094836200DADCE10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F0E0DF00EDDA
      C700FDFCE300FAF8DD00F7F5DA00EEE9CF00E3DAC400D8CDB800CCBFAD00C2B2
      A300AA958B00B9B2B100F5F6F60000000000F7F7F700D0D0D000D5D5D500D6D6
      D600D7D7D700DBD8D600A7CBD70095D1E40089C1D600A0B9C800DCD9D800D5D5
      D500D5D5D500D6D6D600C4C4C400F0F0F0000000000000000000D7D3D000B4A0
      7500C1A55600C1A34900C2A44D00C6AA5700C9AD5D00CAAF5F00CDB16000C6AA
      5E00A28D6000C3C1BF000000000000000000000000000000000000000000EDDF
      C500B38E2E009E8E7000D1D2D600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F1E2E100EEDB
      C900FFFFE800FFFFE500FFFFE400FFFFE400FFFFE500FFFFE500FFFFE500FFFF
      EC00D3C8B300B0A4A400F6F7F80000000000FFFFFF00FFFFFF00FFFFFF00FFFC
      FB00F4F0EF00F6F1F000E8E5E400A1AFB40074848900C8CECE00FCFDFD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000E7E5E500BBA47200B797
      3700B7953500C4A65100CFB56900D5BD7900D7C07E00D6BF7C00D1B97100CAAF
      5F00C7A95000A78F5B00D4D4D70000000000000000000000000000000000FEFB
      F900C4A45300CAAE5E00A8987A00CACACD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F1E2E100EEDB
      CA00FFFFE800FEFEE500FEFEE400FEFEE300FDFDE200FDFDE200FDFDE100FFFF
      E700D0C4B000B0A5A500F6F7F80000000000FFFFFF00FFFFFF00F4F7F800A7C9
      D4008DBCCB007EB8C90083A8B50094969600766D6C008F7D7C00A49D9D00D3D3
      D300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FCFDFF00CAB99900B1903200B18E
      2A00C0A24B00D2BC7D00DAC78C00DDC78900E1CE9500E0CD9300DBC68700D3BB
      7500C8AD5C00BB993C00AE9D7D00F8F9FD000000000000000000000000000000
      0000DFC99B00CAAF5E00DFCA8C00B2A38800C8C8CA0000000000000000000000
      0000000000000000000000000000000000000000000000000000F1E2E100EEDB
      CB00FFFFEA00FFFFE700FFFFE600FEFEE500FEFEE400FEFEE300FDFDE200FFFF
      E800D0C4B100B0A5A500F6F7F80000000000FFFFFF00FFFFFF00BAE3EE0094DB
      F000D8F6FD0089E7FB002FC1EA005D758300BB8E8C00C39B9B00B79E9D00A18E
      8D00AAA4A400F0F1F100FFFFFF00FFFFFF00EEEAE500B3954700A8851B00B897
      3900C6A95600E0D2A700FAF8F200ECE2C400E8D9AA00E9D8A500E3D09900D9C3
      8300CFB56B00BFA04500AB8A3800DEDCDA000000000000000000000000000000
      0000F4EAD900D1B66D00DEC98C00EDDBAB00B5A79100C2C0C100000000000000
      0000000000000000000000000000000000000000000000000000F2E4E300EDDA
      CB00FFFFEB00FFFFE800FFFFE700FFFFE600FFFFE500FEFEE400FEFEE300FFFF
      E900D0C4B100B0A5A500F6F7F80000000000FFFFFF00FFFFFF00B8E3EF0082D4
      EA00ABDDEA006ECEE5002AB7E00078889800C5908E00B0828300A67C7C00B78F
      8F00B18C8C00E2D9D800FFFFFF00FFFFFF00DED0B600A27E1900A9851C00BB9C
      4000CAAE5E00DFD0A200FBFAF600FAF8F200F4EFE000EDE1BE00E5D39E00DCC6
      8800D2B97200C2A44E00AD882200D0C4AC000000000000000000000000000000
      000000000000DDC48B00DDC88C00EADAA900F8EBC300BCAE9600B9B7B8000000
      0000000000000000000000000000000000000000000000000000F2E4E300EDDA
      CC00FFFFED00FFFFEA00FFFFE900FFFFE800FFFFE700FFFFE600FFFFE500FFFF
      EA00D1C5B200B0A5A500F6F7F80000000000FFFFFF00FFFFFF00B8E5F1009BE0
      F300DEF7FC0090E8FA0033C5EC0070839100C7969400C39C9B00B88F8F00AB7B
      7B00A6737300E5D4D400FFFFFF00FFFFFF00D0BB90009A750800A9851C00BA9B
      3F00C9AE5D00DDCD9D00F4F0E400F3EEDF00F4F0E200F4EFE200EBE2C600DDCA
      9400D1B87100C1A34D00AC871E00CCB992000000000000000000EFEBE600B5AF
      A700AFACAA00C6B28D00DFCA8B00E6D4A000ECDDAF00F3E4B600BFAE8E00B4B1
      AF00F9FAFC000000000000000000000000000000000000000000F2E4E300EDDA
      CD00FFFFF000FFFFEC00FFFFEB00FFFFE900FFFFE800FFFFE700FFFFE600FFFF
      EB00D1C5B300B0A5A500F6F7F80000000000FFFFFF00FFFFFF00B9E6F20082D9
      EF0098D9EB0067CAE7002CB9E4007993A200D2A7A500C59E9D00C18F8F00C291
      9000B97F7F00E5D1D100FFFFFF00FFFFFF00C9B38100946E0000A6821800B696
      3600C5A85300D9C79200EFE8D500EEE6D000EEE6CF00EEE7D100F0EBD900E6DC
      BF00CDB67000BC9D4200A8821600CEBA8E000000000000000000F2E8D800B696
      4500C3AA6800D0BA7D00D8C17F00DEC98D00E2CF9800E2CE9600E4D19400C5B0
      7F00A8A19B00F1F3F60000000000000000000000000000000000F3E6E500ECD9
      CF00FFFFF100FFFFEE00FFFFED00FFFFEB00FFFFEA00FFFFE900FFFFE800FFFF
      ED00D2C6B400B0A5A500F6F7F80000000000FFFFFF00FFFFFF00CEEDF5007CD2
      E9009BE2F7008BDBF60057B7D9009DA2AC00D5AEAD00C8A0A000C5878600C681
      8100BB867F00E8D7D400FFFFFF00FFFFFF00CFBB8F008C660000A27D1100AF8D
      2800BC9D4000D2BE8100EAE1C600E9DFC200EAE1C600E8DEC200DDCDA000CFB7
      7000C4A75200B6953500A37C0D00D9C8A500000000000000000000000000CBAE
      6A00AE8B2100C2A44E00CDB36700D5BD7700D7C17F00D8C17E00D4BB7000CFB3
      6200B5974D00A89D8800F7F8FA00000000000000000000000000F3E6E500ECD9
      CF00FFFFF300FFFFF000FFFFEF00FFFFED00FFFFEC00FFFFEA00FFFFE900FFFF
      EF00D2C6B500B0A5A500F6F7F80000000000FFFFFF00FFFFFF00FFFFFE00CECF
      D200AABEC700A9BAC400C0B7BB00D8B8B800D1B4B400CBA2A200C88A8900CA87
      8800B68D7F00E6D7D300FFFFFF00FFFFFF00E7DBC6008A650100A4832000B08F
      2E00B6953600CAB37000E6DCBC00E3D7B300DAC99800D1BA7900CCB16400C8AC
      5B00C0A24E00B3923300A37D1300EFE7D900000000000000000000000000F4EA
      DB00AC861F00B4923100BFA14900C7AB5A00CCB16200C5A76200C4AC7B00DBC2
      8800D2B67300CFB37900F8F5F200000000000000000000000000F3E6E500ECD9
      D100FFFFF500FFFFF200FFFFF100FFFFEF00FFFFEE00FFFFEC00FFFFEB00FFFF
      F100D2C6B600B0A5A500F6F7F80000000000FFFFFF00FFFFFF00FEFEFE00E8D2
      D200E5C6C300E4C5C300DFC1C100D8BDBD00D5BABA00CFA6A600CB8B8A00C989
      8900C77E7F00EDD2D200FFFFFF00FFFFFF00FEFDFC00A48537009D7F2200B99E
      4E00BDA05000CFBC8200E1D5B100D1BD8200CDB46E00CFB67100CDB57000C8B0
      6800C4AB6100A9892600BC9E5500000000000000000000000000000000000000
      0000C9AE6E00A27D0E00AE8B2500B4943200B8983800BA9A42009D8C6F00D6D8
      E100000000000000000000000000000000000000000000000000F4E8E600ECD8
      D100FFFFF700FFFFF600FFFFF400FFFFF200FFFFF100FFFFEF00FFFFEE00FFFF
      F300D2C6B700AFA4A400F6F7F80000000000FFFFFF00FFFFFF00FEFEFE00E5D2
      D200DEC6C600DEC7C700DDC4C400DAC0C000D7BEBE00D1A6A600D08E8E00CE8F
      8F00C9848400EDD2D200FFFFFF00FFFFFF0000000000E2D5BD0091701500BDA7
      6500C6AF6D00CBB67A00CDB97C00CFB97B00D1BC8100D0BC8000CEB97B00CDB8
      7B00C2AB6400A37F1D00EFE5D300000000000000000000000000000000000000
      0000F3EBDD00AF8F3700B2953B00B6984000B99C4800BB9F4B00BEA250009E8A
      6500CDCDCF000000000000000000000000000000000000000000F5E9E600EEDA
      D300FFFFFC00FFFFFA00FFFFF800FFFFF600FFFFF500FFFFF200FFFFF100FFFF
      F600D3C7B700B4A9A900F8F9FA0000000000FFFFFF00FFFFFF00FEFDFD00E4D1
      D000DFC8C600DFC8C800DEC7C700DEC6C600DFCACA00E0C1C100DA9E9E00D48D
      8E00CF828200EED2D200FFFFFF00FFFFFF000000000000000000CEBB9300987B
      2700C5B47F00D6C69800D4C49300D5C59400D6C59500D8C79800D9CB9E00C5B1
      7400A27F2100DDCBA80000000000000000000000000000000000000000000000
      000000000000D0BA8C00C1AD7000C9B77E00CBB77E00CAB77D00C9B77D00CCB8
      7E009D8C6D00C8C8C900FEFFFF00000000000000000000000000EEEBEA00D7C9
      C500C3DADD00D9E3E100D6E6E500EAEFE900E5F0EA00F0F7EE00EAF6ED00F3FD
      F400CDC5BA00D0C3C2000000000000000000FFFFFF00FFFFFF00FEFEFE00E3CF
      CE00DCC2C100DEC7C600E2CDCC00E4D1D100E7D6D600EADDDC00E7D4D300E0BE
      BE00D9A09F00F0D6D600FFFFFF00FFFFFF00000000000000000000000000D7C7
      A600A3873F00B9A56C00D3C69F00DCD1B000DBD1AE00D1C39900B69F5D00AB8C
      3D00E3D5B9000000000000000000000000000000000000000000000000000000
      000000000000F2EADE00C2AD7C00CBBC9100CCBB9000CCBB9000CABA8F00CABB
      9200C5B48600A28D6500F3F2F100000000000000000000000000F5FBFC00CFD7
      DA00ABC4CA00B2C5C9009EBDC500B0C3C70099BBC200A6C0C4009BBAC100ACC5
      C800B4BABB00F3E7E5000000000000000000FFFFFF00FFFFFF00FFFFFF00F6F0
      F000EADBDB00DFC9C800D6B8B700DCC4C400E3D0CF00EADADA00F1E8E800F7F2
      F200FCFCFC00FEFDFD00FFFFFF00FFFFFF000000000000000000000000000000
      0000F8F4ED00D3C29F00C0A97900BCA56F00BCA56E00C2AA7800D8C8A600FCF9
      F600000000000000000000000000000000000000000000000000000000000000
      00000000000000000000F3ECE200F1E9DC00F1EADD00F1EADD00F1EADD00F1E9
      DD00F1EADE00EFE7DA00FDFBF90000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00F83F8000FE010000F81F0000F8000000
      E0070000F8000000C0030001F900000080010000F800000000000000E0010000
      0000000080000000000000018000000000000000000000000000000000000000
      000000000000000080010000000000008001000000000000C003000000000000
      F01F000080000000FFFF0000C0400000E0018000F00FC7FFC0010000E007E3FF
      C0010000C003E1FFC00100008001E0FFC00100000000F07FC00100000000F03F
      C00100000000F81FC00100000000C007C00100000000C003C00100000000E001
      C00100000000E001C00100000001F00FC00100008001F007C0010000C003F801
      C0030000E007F801C0030000F00FFC0100000000000000000000000000000000
      000000000000}
  end
end
