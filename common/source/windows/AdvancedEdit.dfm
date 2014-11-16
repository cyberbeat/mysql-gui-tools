object AdvancedEditFrame: TAdvancedEditFrame
  Left = 0
  Top = 0
  Width = 174
  Height = 3
  TabOrder = 0
  TabStop = True
  OnMouseDown = FrameMouseDown
  OnResize = FrameResize
  object SearchEd: TTntEdit
    Left = 26
    Top = 4
    Width = 139
    Height = 15
    BorderStyle = bsNone
    TabOrder = 0
    OnChange = SearchEdChange
  end
end
