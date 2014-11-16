object TSciWhatToFillForm: TTSciWhatToFillForm
  Left = 368
  Top = 135
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Select languages to insert..'
  ClientHeight = 389
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object checkingPanel: TPanel
    Left = 0
    Top = 0
    Width = 489
    Height = 351
    Align = alClient
    TabOrder = 0
    object copyrightlabel: TLabel
      Left = 1
      Top = 337
      Width = 487
      Height = 13
      Align = alBottom
      Alignment = taCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object c0: TCheckBox
      Left = 11
      Top = 9
      Width = 107
      Height = 17
      Caption = 'C++/C'
      TabOrder = 0
    end
    object c1: TCheckBox
      Tag = 1
      Left = 11
      Top = 30
      Width = 107
      Height = 17
      Caption = 'Java'
      TabOrder = 1
    end
    object c2: TCheckBox
      Tag = 2
      Left = 11
      Top = 51
      Width = 107
      Height = 17
      Caption = 'JavaScript'
      TabOrder = 2
    end
    object c3: TCheckBox
      Tag = 3
      Left = 11
      Top = 72
      Width = 107
      Height = 17
      Caption = 'Resources (.RC)'
      TabOrder = 3
    end
    object c4: TCheckBox
      Tag = 4
      Left = 11
      Top = 93
      Width = 107
      Height = 17
      Caption = 'IDL/ODL'
      TabOrder = 4
    end
    object c5: TCheckBox
      Tag = 5
      Left = 11
      Top = 114
      Width = 107
      Height = 17
      Caption = 'cppnocase'
      TabOrder = 5
    end
    object c6: TCheckBox
      Tag = 6
      Left = 11
      Top = 135
      Width = 107
      Height = 17
      Caption = 'Delphi Pascal'
      TabOrder = 6
    end
    object c7: TCheckBox
      Tag = 7
      Left = 11
      Top = 156
      Width = 107
      Height = 17
      Caption = 'Visual Basic'
      TabOrder = 7
    end
    object c8: TCheckBox
      Tag = 8
      Left = 11
      Top = 178
      Width = 107
      Height = 17
      Caption = 'VB Script'
      TabOrder = 8
    end
    object c9: TCheckBox
      Tag = 9
      Left = 138
      Top = 9
      Width = 97
      Height = 17
      Caption = 'Python'
      TabOrder = 9
    end
    object c10: TCheckBox
      Tag = 10
      Left = 138
      Top = 30
      Width = 97
      Height = 17
      Caption = 'Ruby'
      TabOrder = 10
    end
    object c11: TCheckBox
      Tag = 11
      Left = 138
      Top = 51
      Width = 97
      Height = 17
      Caption = 'CSS'
      TabOrder = 11
    end
    object c12: TCheckBox
      Tag = 12
      Left = 138
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Perl'
      TabOrder = 12
    end
    object c13: TCheckBox
      Tag = 13
      Left = 138
      Top = 93
      Width = 97
      Height = 17
      Caption = 'HTML/PHP/ASP'
      TabOrder = 13
    end
    object c14: TCheckBox
      Tag = 14
      Left = 138
      Top = 114
      Width = 97
      Height = 17
      Caption = 'XML'
      TabOrder = 14
    end
    object c15: TCheckBox
      Tag = 15
      Left = 138
      Top = 135
      Width = 97
      Height = 17
      Caption = 'MySQL/SQL'
      TabOrder = 15
    end
    object c16: TCheckBox
      Tag = 16
      Left = 138
      Top = 156
      Width = 97
      Height = 17
      Caption = 'PowerBasic'
      TabOrder = 16
    end
    object c17: TCheckBox
      Tag = 17
      Left = 138
      Top = 178
      Width = 97
      Height = 17
      Caption = 'TCL/TK'
      TabOrder = 17
    end
    object c18: TCheckBox
      Tag = 18
      Left = 257
      Top = 9
      Width = 97
      Height = 17
      Caption = 'Batch'
      TabOrder = 18
    end
    object c19: TCheckBox
      Tag = 19
      Left = 257
      Top = 30
      Width = 97
      Height = 17
      Caption = 'Properties/INI'
      TabOrder = 19
    end
    object c20: TCheckBox
      Tag = 20
      Left = 257
      Top = 51
      Width = 97
      Height = 17
      Caption = 'Makefile'
      TabOrder = 20
    end
    object c21: TCheckBox
      Tag = 21
      Left = 257
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Diff'
      TabOrder = 21
    end
    object c22: TCheckBox
      Tag = 22
      Left = 382
      Top = 93
      Width = 91
      Height = 17
      Caption = 'Apache Config'
      TabOrder = 22
    end
    object c23: TCheckBox
      Tag = 23
      Left = 257
      Top = 114
      Width = 97
      Height = 17
      Caption = 'Lua'
      TabOrder = 23
    end
    object c24: TCheckBox
      Tag = 24
      Left = 257
      Top = 135
      Width = 97
      Height = 17
      Caption = 'WML'
      TabOrder = 24
    end
    object c25: TCheckBox
      Tag = 25
      Left = 257
      Top = 156
      Width = 97
      Height = 17
      Caption = 'Ada'
      TabOrder = 25
    end
    object c26: TCheckBox
      Tag = 26
      Left = 257
      Top = 178
      Width = 97
      Height = 17
      Caption = 'nnCronTab'
      TabOrder = 26
    end
    object c27: TCheckBox
      Tag = 27
      Left = 382
      Top = 9
      Width = 72
      Height = 17
      Caption = 'C#'
      TabOrder = 27
    end
    object c28: TCheckBox
      Tag = 28
      Left = 382
      Top = 30
      Width = 72
      Height = 17
      Caption = 'Lisp'
      TabOrder = 28
    end
    object c29: TCheckBox
      Tag = 29
      Left = 382
      Top = 51
      Width = 72
      Height = 17
      Caption = 'Scheme'
      TabOrder = 29
    end
    object c31: TCheckBox
      Tag = 31
      Left = 139
      Top = 200
      Width = 72
      Height = 17
      Caption = 'MMixal'
      TabOrder = 30
    end
    object c32: TCheckBox
      Tag = 32
      Left = 382
      Top = 114
      Width = 72
      Height = 17
      Caption = 'LaTeX'
      TabOrder = 31
    end
    object c33: TCheckBox
      Tag = 33
      Left = 382
      Top = 135
      Width = 72
      Height = 17
      Caption = 'AutoIt 3'
      TabOrder = 32
    end
    object c34: TCheckBox
      Tag = 34
      Left = 382
      Top = 156
      Width = 72
      Height = 17
      Caption = 'VHDL'
      TabOrder = 33
    end
    object c35: TCheckBox
      Tag = 35
      Left = 382
      Top = 178
      Width = 72
      Height = 17
      Caption = 'ASN1'
      TabOrder = 34
    end
    object c30: TCheckBox
      Tag = 30
      Left = 382
      Top = 72
      Width = 72
      Height = 17
      Caption = 'Errorlist'
      TabOrder = 35
    end
    object c36: TCheckBox
      Tag = 36
      Left = 12
      Top = 200
      Width = 72
      Height = 17
      Caption = 'VXML'
      TabOrder = 36
    end
    object c37: TCheckBox
      Tag = 36
      Left = 257
      Top = 93
      Width = 72
      Height = 17
      Caption = 'PHP'
      TabOrder = 37
    end
    object c38: TCheckBox
      Tag = 31
      Left = 257
      Top = 200
      Width = 72
      Height = 17
      Caption = 'TEX'
      TabOrder = 38
    end
    object c39: TCheckBox
      Tag = 31
      Left = 382
      Top = 200
      Width = 72
      Height = 17
      Caption = 'ADA'
      TabOrder = 39
    end
    object c41: TCheckBox
      Tag = 31
      Left = 139
      Top = 221
      Width = 72
      Height = 17
      Caption = 'ASM/MASM'
      TabOrder = 40
    end
    object c40: TCheckBox
      Tag = 36
      Left = 12
      Top = 221
      Width = 72
      Height = 17
      Caption = 'APDL'
      TabOrder = 41
    end
    object c42: TCheckBox
      Tag = 31
      Left = 257
      Top = 221
      Width = 72
      Height = 17
      Caption = 'Clarion'
      TabOrder = 42
    end
    object c43: TCheckBox
      Tag = 31
      Left = 382
      Top = 221
      Width = 91
      Height = 17
      Caption = 'Clarion Nocase'
      TabOrder = 43
    end
    object c45: TCheckBox
      Tag = 31
      Left = 139
      Top = 242
      Width = 72
      Height = 17
      Caption = 'Bullant'
      TabOrder = 44
    end
    object c44: TCheckBox
      Tag = 36
      Left = 12
      Top = 242
      Width = 72
      Height = 17
      Caption = 'Avenue'
      TabOrder = 45
    end
    object c46: TCheckBox
      Tag = 31
      Left = 257
      Top = 242
      Width = 72
      Height = 17
      Caption = 'Baan'
      TabOrder = 46
    end
    object c47: TCheckBox
      Tag = 31
      Left = 382
      Top = 242
      Width = 91
      Height = 17
      Caption = 'Eiffel'
      TabOrder = 47
    end
    object c49: TCheckBox
      Tag = 31
      Left = 140
      Top = 263
      Width = 72
      Height = 17
      Caption = 'EScript'
      TabOrder = 48
    end
    object c48: TCheckBox
      Tag = 36
      Left = 13
      Top = 263
      Width = 72
      Height = 17
      Caption = 'Eiffel Kw'
      TabOrder = 49
    end
    object c50: TCheckBox
      Tag = 31
      Left = 258
      Top = 263
      Width = 72
      Height = 17
      Caption = 'Forth'
      TabOrder = 50
    end
    object c51: TCheckBox
      Tag = 31
      Left = 383
      Top = 263
      Width = 91
      Height = 17
      Caption = 'Fortran'
      TabOrder = 51
    end
    object c53: TCheckBox
      Tag = 31
      Left = 140
      Top = 285
      Width = 72
      Height = 17
      Caption = 'KIX'
      TabOrder = 52
    end
    object c52: TCheckBox
      Tag = 36
      Left = 13
      Top = 285
      Width = 90
      Height = 17
      Caption = 'Fortran 77'
      TabOrder = 53
    end
    object c54: TCheckBox
      Tag = 31
      Left = 258
      Top = 285
      Width = 72
      Height = 17
      Caption = 'MSSQL'
      TabOrder = 54
    end
    object c55: TCheckBox
      Tag = 31
      Left = 383
      Top = 285
      Width = 91
      Height = 17
      Caption = 'Haskell'
      TabOrder = 55
    end
    object c56: TCheckBox
      Tag = 36
      Left = 14
      Top = 306
      Width = 107
      Height = 17
      Caption = 'Objective Caml'
      TabOrder = 56
    end
  end
  object buttonPanel: TPanel
    Left = 0
    Top = 351
    Width = 489
    Height = 38
    Align = alBottom
    TabOrder = 1
    object okButton: TBitBtn
      Left = 28
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object cancelButton: TBitBtn
      Left = 380
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
    object selectAllButton: TBitBtn
      Left = 145
      Top = 6
      Width = 75
      Height = 25
      Caption = '&Select All'
      TabOrder = 2
      OnClick = selectAllButtonClick
      NumGlyphs = 2
    end
    object selectNoneButton: TBitBtn
      Left = 262
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Select &None'
      TabOrder = 3
      OnClick = selectNoneButtonClick
    end
  end
end
