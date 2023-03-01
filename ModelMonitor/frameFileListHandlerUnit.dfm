object frameFileListHandler: TframeFileListHandler
  Left = 0
  Top = 0
  Width = 321
  Height = 240
  TabOrder = 0
  object vstIndexLines: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 321
    Height = 240
    Align = alClient
    Colors.UnfocusedSelectionColor = clHighlight
    Colors.UnfocusedSelectionBorderColor = clHighlight
    DragImageKind = diNoImage
    DragOperations = [doMove]
    DragType = dtVCL
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnGetText = vstIndexLinesGetText
    OnGetNodeDataSize = vstIndexLinesGetNodeDataSize
    OnMouseMove = vstIndexLinesMouseMove
    OnNodeClick = vstIndexLinesNodeClick
    OnNodeDblClick = vstIndexLinesNodeDblClick
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
end
