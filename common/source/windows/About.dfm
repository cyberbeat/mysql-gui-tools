object AboutForm: TAboutForm
  Left = 402
  Top = 221
  ActiveControl = CloseBtn
  BorderStyle = bsDialog
  Caption = 'About MySQL Administrator'
  ClientHeight = 408
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 106
  TextHeight = 13
  object AboutImg: TTntImage
    Left = 0
    Top = 0
    Width = 444
    Height = 354
  end
  object TextLbl: TTntLabel
    Left = 27
    Top = 261
    Width = 393
    Height = 88
    AutoSize = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object VersionLbl: TTntLabel
    Left = 27
    Top = 235
    Width = 103
    Height = 13
    Caption = 'version 1.0.7 BETA'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object CopyrightLbl: TTntLabel
    Left = 27
    Top = 261
    Width = 401
    Height = 13
    AutoSize = False
    Caption = #169' Copyright 2005, 2006 by MySQL AB. All rights reserved.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LicenceLbl: TTntLabel
    Left = 27
    Top = 286
    Width = 401
    Height = 63
    AutoSize = False
    Caption = 
      'This software is released under the GNU General Public License (' +
      'GPL), which is probably the best known Open Source license. The ' +
      'formal terms of the GPL license can be found at http://www.fsf.o' +
      'rg/licenses/.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object TntBevel1: TTntBevel
    Left = 0
    Top = 344
    Width = 445
    Height = 8
    Shape = bsTopLine
  end
  object CloseBtn: TTntButton
    Left = 353
    Top = 371
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CreditsBtn: TTntButton
    Left = 262
    Top = 371
    Width = 75
    Height = 25
    Caption = 'Credits'
    TabOrder = 1
    OnClick = CreditsBtnClick
  end
  object CloseTimer: TTimer
    Enabled = False
    OnTimer = CloseTimerTimer
    Left = 408
    Top = 8
  end
end
