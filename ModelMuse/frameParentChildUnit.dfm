object frameParentChild: TframeParentChild
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object tvTree: TTreeView
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    DragMode = dmAutomatic
    Indent = 19
    MultiSelect = True
    MultiSelectStyle = [msControlSelect, msShiftSelect]
    ReadOnly = True
    TabOrder = 0
    OnDragDrop = tvTreeDragDrop
    OnDragOver = tvTreeDragOver
  end
end
