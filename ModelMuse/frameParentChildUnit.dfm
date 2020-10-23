object frameParentChild: TframeParentChild
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object tvTree: TJvTreeView
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
    ExplicitLeft = 80
    ExplicitTop = 48
    ExplicitWidth = 121
    ExplicitHeight = 97
  end
end
