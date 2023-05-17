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
    Colors.BorderColor = 15987699
    Colors.DisabledColor = clGray
    Colors.DropMarkColor = 15385233
    Colors.DropTargetColor = 15385233
    Colors.DropTargetBorderColor = 15385233
    Colors.FocusedSelectionColor = 15385233
    Colors.FocusedSelectionBorderColor = 15385233
    Colors.GridLineColor = 15987699
    Colors.HeaderHotColor = clBlack
    Colors.HotColor = clBlack
    Colors.SelectionRectangleBlendColor = 15385233
    Colors.SelectionRectangleBorderColor = 15385233
    Colors.SelectionTextColor = clBlack
    Colors.TreeLineColor = 9471874
    Colors.UnfocusedColor = clGray
    Colors.UnfocusedSelectionColor = 15385233
    Colors.UnfocusedSelectionBorderColor = 15385233
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
