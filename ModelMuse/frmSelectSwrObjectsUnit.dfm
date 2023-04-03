inherited frmSelectSwrObjects: TfrmSelectSwrObjects
  HelpType = htKeyword
  HelpKeyword = 'Select_SWR_Objects_Dialog_Box'
  Caption = 'Select SWR Objects'
  ClientWidth = 440
  OnResize = FormResize
  ExplicitWidth = 452
  ExplicitHeight = 272
  PixelsPerInch = 120
  TextHeight = 18
  object btnAddScreenObject: TSpeedButton
    Left = 209
    Top = 64
    Width = 23
    Height = 22
    Hint = 'Add objects to selected objects'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333FF3333333333333003333
      3333333333773FF3333333333309003333333333337F773FF333333333099900
      33333FFFFF7F33773FF30000000999990033777777733333773F099999999999
      99007FFFFFFF33333F7700000009999900337777777F333F7733333333099900
      33333333337F3F77333333333309003333333333337F77333333333333003333
      3333333333773333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnAddScreenObjectClick
  end
  object btnRemoveScreenObject: TSpeedButton
    Left = 209
    Top = 92
    Width = 23
    Height = 22
    Hint = 'Remove objects from selected objects'
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333333FF3333333333333003333333333333F77F33333333333009033
      333333333F7737F333333333009990333333333F773337FFFFFF330099999000
      00003F773333377777770099999999999990773FF33333FFFFF7330099999000
      000033773FF33777777733330099903333333333773FF7F33333333333009033
      33333333337737F3333333333333003333333333333377333333333333333333
      3333333333333333333333333333333333333333333333333333333333333333
      3333333333333333333333333333333333333333333333333333}
    NumGlyphs = 2
    ParentShowHint = False
    ShowHint = True
    OnClick = btnRemoveScreenObjectClick
  end
  object lblAvailable: TLabel
    Left = 8
    Top = 8
    Width = 121
    Height = 18
    Caption = 'Available Objects'
  end
  object lblSelected: TLabel
    Left = 237
    Top = 8
    Width = 119
    Height = 18
    Caption = 'Selected Objects'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 192
    Width = 440
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 191
    ExplicitWidth = 436
    DesignSize = (
      440
      42)
    object btnHelp: TBitBtn
      Left = 165
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 161
    end
    object btnOK: TBitBtn
      Left = 254
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      ExplicitLeft = 250
    end
    object btnCancel: TBitBtn
      Left = 343
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 339
    end
  end
  object vstAvailableObjects: TVirtualStringTree
    AlignWithMargins = True
    Left = 3
    Top = 40
    Width = 200
    Height = 149
    Margins.Top = 40
    Align = alLeft
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
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    TabOrder = 0
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnGetText = vstAvailableObjectsGetText
    OnGetNodeDataSize = vstAvailableObjectsGetNodeDataSize
    OnInitNode = vstAvailableObjectsInitNode
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
  object vstSelectedObjects: TVirtualStringTree
    AlignWithMargins = True
    Left = 237
    Top = 40
    Width = 200
    Height = 149
    Margins.Top = 40
    Align = alRight
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
    Colors.UnfocusedSelectionColor = clWhite
    Colors.UnfocusedSelectionBorderColor = clWhite
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    TabOrder = 1
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnGetText = vstAvailableObjectsGetText
    OnGetNodeDataSize = vstAvailableObjectsGetNodeDataSize
    OnInitNode = vstSelectedObjectsInitNode
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
end
