object TableDragForm: TTableDragForm
  Left = 398
  Top = 400
  AlphaBlend = True
  AlphaBlendValue = 180
  BorderStyle = bsNone
  Caption = 'TableDragForm'
  ClientHeight = 20
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDragDrop = TntFormDragDrop
  OnDragOver = TntFormDragOver
  OnPaint = TntFormPaint
  PixelsPerInch = 96
  TextHeight = 13
end
