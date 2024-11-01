inherited frmGridValue: TfrmGridValue
  HelpType = htKeyword
  HelpKeyword = 'Grid_Value_Dialog_Box'
  Caption = 'Grid or Mesh Value'
  ClientHeight = 509
  ClientWidth = 663
  KeyPreview = True
  OnClose = FormClose
  ExplicitWidth = 679
  ExplicitHeight = 548
  DesignSize = (
    663
    509)
  TextHeight = 18
  object btnHelp: TBitBtn
    Left = 477
    Top = 469
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
    ExplicitLeft = 448
  end
  object btnClose: TBitBtn
    Left = 574
    Top = 469
    Width = 89
    Height = 33
    Anchors = [akRight, akBottom]
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 2
    ExplicitLeft = 545
  end
  object pnlTabs: TPanel
    Left = 0
    Top = 0
    Width = 663
    Height = 463
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 634
    object splAllDataSets: TSplitter
      Left = 441
      Top = 1
      Height = 461
      ExplicitLeft = 50
      ExplicitHeight = 465
    end
    object splPathline: TSplitter
      Left = 471
      Top = 1
      Height = 461
      ExplicitLeft = 58
      ExplicitHeight = 465
    end
    object splEndPoint: TSplitter
      Left = 501
      Top = 1
      Height = 461
      ExplicitLeft = 88
      ExplicitHeight = 465
    end
    object splSWR: TSplitter
      Left = 531
      Top = 1
      Height = 461
      ExplicitLeft = 118
      ExplicitHeight = 465
    end
    object splGNC: TSplitter
      Left = 561
      Top = 1
      Height = 461
      ExplicitLeft = 148
      ExplicitHeight = 465
    end
    object splXt3d: TSplitter
      Left = 591
      Top = 1
      Height = 461
      ExplicitLeft = 178
      ExplicitHeight = 465
    end
    object splFlowFace: TSplitter
      Left = 621
      Top = 1
      Width = 5
      Height = 461
    end
    object rrlcurrentData: TRbwRollupPanel
      Left = 1
      Top = 1
      Width = 440
      Height = 461
      Align = alLeft
      Caption = 'rrlcurrentData'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 0
      Collapsed = False
      RollupCaption = 'Current Data'
      LabelWidth = 27
      ExpandedWidth = 413
      DesignSize = (
        440
        461)
      object lblModel: TLabel
        Left = 40
        Top = 19
        Width = 43
        Height = 18
        Caption = 'Model'
      end
      object lblDataSet: TLabel
        Left = 40
        Top = 117
        Width = 62
        Height = 18
        Caption = 'Data Set'
      end
      object lblCellValue: TLabel
        Left = 40
        Top = 140
        Width = 39
        Height = 18
        Caption = 'Value'
      end
      object lblExplanation: TLabel
        Left = 40
        Top = 171
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object lblHigher3rdDimensionCoordinate: TStaticText
        Left = 40
        Top = 408
        Width = 231
        Height = 22
        Anchors = [akLeft, akBottom]
        Caption = 'Higher 3rd dimension coordinate'
        TabOrder = 14
      end
      object lblLower3rdDimensionCoordinate: TStaticText
        Left = 40
        Top = 429
        Width = 228
        Height = 22
        Anchors = [akLeft, akBottom]
        Caption = 'Lower 3rd dimension coordinate'
        TabOrder = 15
      end
      object lblSection: TStaticText
        Left = 40
        Top = 361
        Width = 57
        Height = 22
        Anchors = [akLeft, akBottom]
        Caption = 'Section'
        TabOrder = 12
      end
      object lblSelectedObject: TStaticText
        Left = 40
        Top = 316
        Width = 112
        Height = 22
        Anchors = [akLeft, akBottom]
        Caption = 'Selected object'
        TabOrder = 10
      end
      object lblVertex: TStaticText
        Left = 40
        Top = 340
        Width = 104
        Height = 22
        Anchors = [akLeft, akBottom]
        Caption = 'Nearest vertex'
        TabOrder = 11
      end
      object lblColumnWidth: TStaticText
        Left = 169
        Top = 94
        Width = 97
        Height = 22
        Caption = 'Column width'
        TabOrder = 7
      end
      object lblLayer: TStaticText
        Left = 40
        Top = 47
        Width = 43
        Height = 22
        Caption = 'Layer'
        TabOrder = 2
      end
      object lblLayerHeight: TStaticText
        Left = 169
        Top = 48
        Width = 89
        Height = 22
        Caption = 'Layer height'
        TabOrder = 3
      end
      object lblRowWidth: TStaticText
        Left = 169
        Top = 71
        Width = 75
        Height = 22
        Caption = 'Row width'
        TabOrder = 5
      end
      object lblColumn: TStaticText
        Left = 40
        Top = 94
        Width = 57
        Height = 22
        Caption = 'Column'
        TabOrder = 6
      end
      object lblRow: TStaticText
        Left = 40
        Top = 71
        Width = 35
        Height = 22
        Caption = 'Row'
        TabOrder = 4
      end
      object edCellValue: TEdit
        Left = 85
        Top = 139
        Width = 345
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 8
        OnKeyUp = edCellValueKeyUp
      end
      object cbShowThirdDValues: TCheckBox
        Left = 40
        Top = 385
        Width = 390
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Show selected object 3rd dimension coordinates'
        TabOrder = 13
      end
      object comboModel: TComboBox
        Left = 169
        Top = 16
        Width = 246
        Height = 26
        Style = csDropDownList
        TabOrder = 1
        OnChange = comboModelChange
      end
      object memoExplanation: TMemo
        Left = 40
        Top = 192
        Width = 390
        Height = 118
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 9
        OnKeyUp = memoExplanationKeyUp
      end
    end
    object rrlAllDataSets: TRbwRollupPanel
      Left = 444
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlAllDataSets'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 1
      Collapsed = True
      RollupCaption = 'All Data Sets'
      LabelWidth = 27
      ExpandedWidth = 399
      DesignSize = (
        27
        461)
      object lblSelectExplanation: TLabel
        Left = 31
        Top = 63
        Width = 81
        Height = 18
        Caption = 'Explanation'
      end
      object lblSelectValue: TLabel
        Left = 32
        Top = 39
        Width = 39
        Height = 18
        Caption = 'Value'
      end
      object btnUpdate: TButton
        Left = 31
        Top = 425
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Update'
        TabOrder = 4
        OnClick = btnUpdateClick
      end
      object edSelectValue: TEdit
        Left = 77
        Top = 39
        Width = 343
        Height = 26
        ReadOnly = True
        TabOrder = 2
        OnKeyUp = edCellValueKeyUp
      end
      object memoSelectExplanation: TMemo
        Left = 32
        Top = 87
        Width = 0
        Height = 331
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 3
        OnKeyUp = memoExplanationKeyUp
      end
      object virttreecomboDataSets: TRbwStringTreeCombo
        Left = 32
        Top = 7
        Width = 388
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
        Tree.TreeOptions.SelectionOptions = [toFullRowSelect]
        Tree.OnChange = virttreecomboDataSetsTreeChange
        Tree.OnGetText = virttreecomboDataSetsTreeGetText
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Tree.Touch.InteractiveGestures = [igPan, igPressAndTap]
        Tree.Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
        Tree.Columns = <>
        Enabled = True
        Glyph.Data = {
          36020000424D3602000000000000360000002800000010000000080000000100
          2000000000000002000000000000000000000000000000000000D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC0000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00000000000000000000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC000000
          000000000000000000000000000000000000D8E9EC00D8E9EC00D8E9EC00C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00000000000000
          00000000000000000000000000000000000000000000D8E9EC00C0C0C000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00}
        NumGlyphs = 2
        TabOrder = 1
        OnChange = virttreecomboDataSetsChange
      end
    end
    object rrlPathline: TRbwRollupPanel
      Left = 474
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlPathline'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 2
      Collapsed = True
      RollupCaption = 'Pathline'
      LabelWidth = 27
      ExpandedWidth = 399
      object pnlPathline: TPanel
        Left = 28
        Top = 1
        Width = 397
        Height = 459
        Align = alClient
        Caption = 'pnlPathline'
        ShowCaption = False
        TabOrder = 1
        object rdgPathline: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 395
          Height = 421
          Align = alClient
          ColCount = 4
          FixedCols = 1
          RowCount = 14
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          Columns = <
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
        end
        object pnlPathLength: TPanel
          Left = 1
          Top = 422
          Width = 395
          Height = 36
          Align = alBottom
          TabOrder = 1
          object lblLength: TLabel
            Left = 7
            Top = 6
            Width = 107
            Height = 18
            Caption = 'Pathline Length'
          end
          object edLength: TEdit
            Left = 136
            Top = 0
            Width = 249
            Height = 26
            TabOrder = 0
          end
        end
      end
    end
    object rrlEndPoint: TRbwRollupPanel
      Left = 504
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlEndPoint'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 3
      Collapsed = True
      RollupCaption = 'End Point'
      LabelWidth = 27
      ExpandedWidth = 399
      object pnlEndpoint: TPanel
        Left = 28
        Top = 1
        Width = 397
        Height = 459
        Align = alClient
        Caption = 'pnlEndpoint'
        ShowCaption = False
        TabOrder = 1
        object pnlEndPoints: TPanel
          Left = 1
          Top = 352
          Width = 395
          Height = 106
          Align = alBottom
          TabOrder = 1
          object lbledtReleaseTime: TLabeledEdit
            Left = 136
            Top = 6
            Width = 121
            Height = 26
            EditLabel.Width = 92
            EditLabel.Height = 26
            EditLabel.Caption = 'Release time'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 0
            Text = ''
          end
          object lbledtTerminationCode: TLabeledEdit
            Left = 136
            Top = 38
            Width = 121
            Height = 26
            EditLabel.Width = 119
            EditLabel.Height = 26
            EditLabel.Caption = 'Termination code'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 1
            Text = ''
          end
          object lbledtTrackingTime: TLabeledEdit
            Left = 136
            Top = 70
            Width = 121
            Height = 26
            EditLabel.Width = 93
            EditLabel.Height = 26
            EditLabel.Caption = 'Tracking time'
            LabelPosition = lpLeft
            ReadOnly = True
            TabOrder = 2
            Text = ''
          end
        end
        object rdgEndPoints: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 395
          Height = 351
          Align = alClient
          ColCount = 3
          FixedCols = 1
          RowCount = 14
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          Columns = <
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
        end
      end
    end
    object rrlSWR: TRbwRollupPanel
      Left = 534
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlSWR'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 4
      Collapsed = True
      RollupCaption = 'Surface Water Routing'
      LabelWidth = 27
      ExpandedWidth = 383
      object pnlSWR: TPanel
        Left = 28
        Top = 1
        Width = 381
        Height = 459
        Align = alClient
        Caption = 'pnlSWR'
        ShowCaption = False
        TabOrder = 1
        object pnlSwrReaches: TPanel
          Left = 1
          Top = 1
          Width = 379
          Height = 200
          Align = alTop
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          TabOrder = 0
          object lblSwrReaches: TLabel
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 371
            Height = 18
            Align = alTop
            Caption = 'SWR Reaches'
            ExplicitWidth = 103
          end
          object rdgSwrReaches: TRbwDataGrid4
            Left = 1
            Top = 25
            Width = 377
            Height = 174
            Align = alClient
            ColCount = 4
            FixedCols = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
            TabOrder = 0
            ExtendedAutoDistributeText = False
            AutoMultiEdit = False
            AutoDistributeText = False
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = False
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            Columns = <
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end>
            WordWrapRowCaptions = False
          end
        end
        object pnlSwrStructures: TPanel
          Left = 1
          Top = 201
          Width = 379
          Height = 257
          Align = alClient
          TabOrder = 1
          object lblSwrStructures: TLabel
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 371
            Height = 18
            Align = alTop
            Caption = 'SWR Structures'
            ExplicitWidth = 111
          end
          object rdgSwrStructures: TRbwDataGrid4
            Left = 1
            Top = 25
            Width = 377
            Height = 231
            Align = alClient
            ColCount = 3
            FixedCols = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
            TabOrder = 0
            ExtendedAutoDistributeText = False
            AutoMultiEdit = False
            AutoDistributeText = False
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = False
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            Columns = <
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end
              item
                AutoAdjustRowHeights = False
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = False
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              64
              64
              64)
          end
        end
      end
    end
    object rrlGNC: TRbwRollupPanel
      Left = 564
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'GNC'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 5
      Collapsed = True
      RollupCaption = 'Ghost Node Correction'
      LabelWidth = 27
      ExpandedWidth = 412
      DesignSize = (
        27
        461)
      object rdgGhostNode: TRbwDataGrid4
        Left = 40
        Top = 9
        Width = 0
        Height = 445
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 4
        DoubleBuffered = True
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        ParentDoubleBuffered = False
        TabOrder = 1
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
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
        WordWrapRowCaptions = False
      end
    end
    object rrlXt3d: TRbwRollupPanel
      Left = 594
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlXt3d'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 6
      Collapsed = True
      RollupCaption = 'XT3D Data'
      LabelWidth = 27
      ExpandedWidth = 288
      ExplicitLeft = 593
      object lblAngle1: TLabel
        Left = 40
        Top = 76
        Width = 53
        Height = 18
        Caption = 'Angle 1'
      end
      object lblAngle2: TLabel
        Left = 40
        Top = 195
        Width = 53
        Height = 18
        Caption = 'Angle 2'
      end
      object lblAngle3: TLabel
        Left = 40
        Top = 315
        Width = 53
        Height = 18
        Caption = 'Angle 3'
      end
      object lblKz: TLabel
        Left = 40
        Top = 51
        Width = 18
        Height = 18
        Caption = 'Kz'
      end
      object lblKy: TLabel
        Left = 40
        Top = 27
        Width = 18
        Height = 18
        Caption = 'Ky'
      end
      object lblKx: TLabel
        Left = 40
        Top = 3
        Width = 18
        Height = 18
        Caption = 'Kx'
      end
      object imgAngle1: TImage
        Left = 40
        Top = 99
        Width = 89
        Height = 90
      end
      object imgAngle2: TImage
        Left = 40
        Top = 219
        Width = 89
        Height = 90
      end
      object imgAngle3: TImage
        Left = 40
        Top = 337
        Width = 89
        Height = 90
      end
      object btnUpdateXT3D: TButton
        Left = 40
        Top = 433
        Width = 75
        Height = 25
        Caption = 'Update'
        TabOrder = 1
        OnClick = btnUpdateXT3DClick
      end
    end
    object rrlFlowFace: TRbwRollupPanel
      Left = 626
      Top = 1
      Width = 27
      Height = 461
      Align = alLeft
      Caption = 'rrlFlowFace'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Pitch = fpVariable
      Font.Style = []
      ShowCaption = False
      TabOrder = 7
      Collapsed = True
      RollupCaption = 'PRT Flow Faces'
      LabelWidth = 27
      ExpandedWidth = 196
      object Panel1: TPanel
        Left = 28
        Top = 1
        Width = 194
        Height = 459
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 1
        ExplicitLeft = 32
        ExplicitTop = 3
        object pnlFlowFaces: TPanel
          Left = 1
          Top = 1
          Width = 192
          Height = 68
          Align = alTop
          TabOrder = 0
          object lblFlowFaces: TLabel
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 184
            Height = 60
            Align = alClient
            Alignment = taCenter
            Caption = 'Cell Center = 0'#13#10'Top Face = -1'#13#10'Bottom Face = -2'
            Layout = tlCenter
            ExplicitWidth = 121
            ExplicitHeight = 54
          end
        end
        object qzbNodeInformation: TQRbwZoomBox2
          Left = 1
          Top = 69
          Width = 192
          Height = 389
          Align = alClient
          Caption = 'qzbFlowFace'
          TabOrder = 1
          Exaggeration = 1.000000000000000000
          HorizontalDirection = hdRight
          Image32.Left = 0
          Image32.Top = 0
          Image32.Width = 192
          Image32.Height = 389
          Image32.Anchors = [akLeft, akBottom]
          Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
          Image32.BitmapAlign = baTopLeft
          Image32.Color = clWhite
          Image32.ParentColor = False
          Image32.Scale = 1.000000000000000000
          Image32.ScaleMode = smNormal
          Image32.TabOrder = 0
          ImmediateResize = True
          Magnification = 1.000000000000000000
          VerticalDirection = vdUp
          ExplicitTop = 160
          ExplicitWidth = 221
          ExplicitHeight = 300
          DesignSize = (
            192
            389)
        end
      end
    end
  end
end
