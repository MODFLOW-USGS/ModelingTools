inherited frmObservationComparisons: TfrmObservationComparisons
  Caption = 'Observation Comparisons'
  ClientWidth = 760
  ExplicitWidth = 776
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 191
    Width = 760
    Height = 35
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      760
      35)
    object btnCancel: TBitBtn
      Left = 672
      Top = 2
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 584
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 496
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
  end
  object treecomboInPlaceEditor: TRbwStringTreeCombo
    Left = 48
    Top = 24
    Width = 121
    Height = 26
    Tree.Left = 0
    Tree.Top = 0
    Tree.Width = 304
    Tree.Height = 201
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
  end
  inline frameObsComparisons: TframeGrid
    Left = 0
    Top = 0
    Width = 760
    Height = 191
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 760
    ExplicitHeight = 191
    inherited Panel: TPanel
      Top = 150
      Width = 760
      ExplicitTop = 150
      ExplicitWidth = 760
      inherited lbNumber: TLabel
        Width = 55
        Height = 18
        ExplicitWidth = 55
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 399
        ExplicitLeft = 312
      end
      inherited sbInsert: TSpeedButton
        Left = 475
        ExplicitLeft = 370
      end
      inherited sbDelete: TSpeedButton
        Left = 546
        ExplicitLeft = 427
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 760
      Height = 150
      ColCount = 7
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
      OnExit = frameObsComparisonsGridExit
      OnSelectCell = frameObsComparisonsGridSelectCell
      OnColSize = frameObsComparisonsGridColSize
      Columns = <
        item
          AutoAdjustRowHeights = True
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = True
        end
        item
          AutoAdjustRowHeights = True
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
        end>
      ExplicitWidth = 760
      ExplicitHeight = 150
    end
  end
end
