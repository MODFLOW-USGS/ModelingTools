inherited frmObservationComparisons: TfrmObservationComparisons
  HelpType = htKeyword
  HelpKeyword = 'Observation_Comparisons_Dialog'
  Caption = 'Comparison Observations'
  ClientHeight = 312
  ClientWidth = 784
  ExplicitWidth = 796
  ExplicitHeight = 350
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 277
    Width = 784
    Height = 35
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 276
    ExplicitWidth = 780
    DesignSize = (
      784
      35)
    object btnCancel: TBitBtn
      Left = 688
      Top = 2
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 684
    end
    object btnOK: TBitBtn
      Left = 600
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 596
    end
    object btnHelp: TBitBtn
      Left = 512
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 508
    end
  end
  object treecomboInPlaceEditor: TRbwStringTreeCombo
    Left = 48
    Top = 24
    Width = 121
    Height = 26
    Tree.Left = 0
    Tree.Top = 0
    Tree.Width = 624
    Tree.Height = 441
    Tree.Align = alClient
    Tree.Colors.BorderColor = 15987699
    Tree.Colors.DisabledColor = clGray
    Tree.Colors.DropMarkColor = 15385233
    Tree.Colors.DropTargetColor = 15385233
    Tree.Colors.DropTargetBorderColor = 15385233
    Tree.Colors.FocusedSelectionColor = 15385233
    Tree.Colors.FocusedSelectionBorderColor = 15385233
    Tree.Colors.GridLineColor = 15987699
    Tree.Colors.HeaderHotColor = clBlack
    Tree.Colors.HotColor = clBlack
    Tree.Colors.SelectionRectangleBlendColor = 15385233
    Tree.Colors.SelectionRectangleBorderColor = 15385233
    Tree.Colors.SelectionTextColor = clBlack
    Tree.Colors.TreeLineColor = 9471874
    Tree.Colors.UnfocusedColor = clGray
    Tree.Colors.UnfocusedSelectionColor = 13421772
    Tree.Colors.UnfocusedSelectionBorderColor = 13421772
    Tree.DefaultNodeHeight = 20
    Tree.Header.AutoSizeIndex = 0
    Tree.Header.MainColumn = -1
    Tree.TabOrder = 0
    Tree.OnChange = treecomboInPlaceEditorTreeChange
    Tree.OnFreeNode = treecomboInPlaceEditorTreeFreeNode
    Tree.OnGetText = treecomboInPlaceEditorTreeGetText
    Tree.OnGetNodeDataSize = treecomboInPlaceEditorTreeGetNodeDataSize
    Tree.OnInitNode = treecomboInPlaceEditorTreeInitNode
    Tree.OnMouseDown = treecomboInPlaceEditorTreeMouseDown
    Tree.Touch.InteractiveGestures = [igPan, igPressAndTap]
    Tree.Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Tree.ExplicitWidth = 200
    Tree.ExplicitHeight = 100
    Tree.Columns = <>
    Enabled = True
    Glyph.Data = {
      36020000424D3602000000000000360000002800000010000000080000000100
      2000000000000002000000000000000000000000000000000000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F00000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000C0C0C000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000000000000000000000000000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000F0F0F000F0F0F0000000
      000000000000000000000000000000000000F0F0F000F0F0F000F0F0F000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000000000000000
      00000000000000000000000000000000000000000000F0F0F000C0C0C000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0
      F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000F0F0F000}
    NumGlyphs = 2
    TabOrder = 1
    OnCanClose = treecomboInPlaceEditorCanClose
  end
  inline frameObsComparisons: TframeGrid
    Left = 0
    Top = 0
    Width = 784
    Height = 277
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 780
    ExplicitHeight = 276
    inherited Panel: TPanel
      Top = 236
      Width = 784
      ExplicitTop = 235
      ExplicitWidth = 780
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 408
        ExplicitLeft = 312
      end
      inherited sbInsert: TSpeedButton
        Left = 486
        ExplicitLeft = 370
      end
      inherited sbDelete: TSpeedButton
        Left = 558
        ExplicitLeft = 427
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 784
      Height = 236
      ColCount = 7
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      OnExit = frameObsComparisonsGridExit
      OnSelectCell = frameObsComparisonsGridSelectCell
      OnSetEditText = frameObsComparisonsGridSetEditText
      OnColSize = frameObsComparisonsGridColSize
      Columns = <
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = True
          Format = rcf4String
          LimitToList = True
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4String
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Real
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          WordWrapCaptions = True
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end>
      ExplicitWidth = 780
      ExplicitHeight = 235
    end
  end
end
