object MyxTabHeaderFrame: TMyxTabHeaderFrame
  Left = 0
  Top = 0
  Width = 695
  Height = 11
  Color = clWhite
  ParentColor = False
  TabOrder = 0
  TabStop = True
  OnResize = TntFrameResize
  object TabHeaderPopupMenu: TTntPopupMenu
    OnPopup = TabHeaderPopupMenuPopup
    Left = 26
    object RenameTabsheetMI: TTntMenuItem
      Caption = 'Rename Tabsheet'
      OnClick = RenameTabsheetMIClick
    end
    object N1: TTntMenuItem
      Caption = '-'
    end
    object CloseTabsheetMI: TTntMenuItem
      Caption = 'Close Tabsheet'
      OnClick = CloseTabsheetMIClick
    end
  end
end
