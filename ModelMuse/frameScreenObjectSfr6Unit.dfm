inherited frameScreenObjectSfr6: TframeScreenObjectSfr6
  Width = 547
  Height = 435
  OnResize = FrameResize
  ExplicitWidth = 547
  ExplicitHeight = 435
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 547
    Height = 25
    Align = alTop
    TabOrder = 0
    object pnlCaption: TPanel
      Left = 1
      Top = 1
      Width = 545
      Height = 23
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object pgcSfr6: TPageControl
    Left = 0
    Top = 25
    Width = 547
    Height = 410
    ActivePage = tabConfiguration
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 247
    object tabConfiguration: TTabSheet
      Caption = 'Configuration'
      ImageIndex = 3
      object lblSegmentNumber: TLabel
        Left = 134
        Top = 17
        Width = 92
        Height = 15
        Caption = 'Segment number'
      end
      object rdgFormulas: TRbwDataGrid4
        AlignWithMargins = True
        Left = 0
        Top = 50
        Width = 539
        Height = 330
        Margins.Left = 0
        Margins.Top = 50
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        ColCount = 2
        FixedCols = 1
        RowCount = 7
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSetEditText = rdgFormulasSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
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
            Format = rcf4Integer
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 50
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
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
        ExplicitHeight = 167
        ColWidths = (
          64
          186)
      end
      object rdeSegmentNumber: TRbwDataEntry
        Left = 3
        Top = 14
        Width = 125
        Height = 22
        TabOrder = 1
        Text = '1'
        OnChange = rdeSegmentNumberChange
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbSpecifyCrossSection: TCheckBox
        Left = 344
        Top = 17
        Width = 192
        Height = 17
        Caption = 'Specify cross section'
        TabOrder = 2
        OnClick = cbSpecifyCrossSectionClick
      end
    end
    object tabRates: TTabSheet
      Caption = 'Rates'
      object pnlGrid: TPanel
        Left = 0
        Top = 0
        Width = 539
        Height = 334
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 171
        object pnlEditGrid: TPanel
          Left = 1
          Top = 1
          Width = 537
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFormula: TLabel
            Left = 136
            Top = 5
            Width = 44
            Height = 15
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeFormula: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            Text = ''
            OnChange = rdeFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
        object rdgModflowBoundary: TRbwDataGrid4
          Left = 1
          Top = 51
          Width = 537
          Height = 282
          Align = alClient
          ColCount = 3
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
          TabOrder = 1
          OnEnter = rdgModflowBoundaryEnter
          OnMouseUp = rdgModflowBoundaryMouseUp
          OnSelectCell = rdgModflowBoundarySelectCell
          OnSetEditText = rdgModflowBoundarySetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgModflowBoundaryBeforeDrawCell
          OnColSize = rdgModflowBoundaryColSize
          OnStateChange = rdgModflowBoundaryStateChange
          ColorRangeSelection = False
          OnHorizontalScroll = rdgModflowBoundaryHorizontalScroll
          Columns = <
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
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
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = True
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
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
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
          ExplicitHeight = 119
        end
      end
      object pnlBottom: TPanel
        Left = 0
        Top = 334
        Width = 539
        Height = 46
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 171
        DesignSize = (
          539
          46)
        object lblNumTimes: TLabel
          Left = 64
          Top = 15
          Width = 90
          Height = 15
          Caption = 'Number of times'
        end
        object seNumberOfTimes: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 49
          Height = 23
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 2
          OnChange = seNumberOfTimesChange
        end
        object btnDelete: TBitBtn
          Left = 451
          Top = 6
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Delete'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          TabOrder = 1
          OnClick = btnDeleteClick
        end
        object btnInsert: TBitBtn
          Left = 363
          Top = 5
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Insert'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          TabOrder = 0
          OnClick = btnInsertClick
        end
      end
    end
    object tabDownstreamSegments: TTabSheet
      Caption = 'Downstream Segments'
      ImageIndex = 1
      inline frmgrdDownstreamSegments: TframeGrid
        Left = 0
        Top = 0
        Width = 539
        Height = 380
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 539
        ExplicitHeight = 217
        inherited Panel: TPanel
          Top = 339
          Width = 539
          ExplicitTop = 176
          ExplicitWidth = 539
          inherited sbAdd: TSpeedButton
            Left = 280
            ExplicitLeft = 195
          end
          inherited sbInsert: TSpeedButton
            Left = 333
            ExplicitLeft = 232
          end
          inherited sbDelete: TSpeedButton
            Left = 385
            ExplicitLeft = 269
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frmgrdDownstreamSegmentsseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 539
          Height = 339
          OnSetEditText = frmgrdDownstreamSegmentsGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = 'Closest'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 60
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Integer
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 539
          ExplicitHeight = 176
        end
      end
    end
    object tabDiversions: TTabSheet
      Caption = 'Diversions'
      ImageIndex = 2
      inline frmgrdDiversions: TframeGrid
        Left = 0
        Top = 0
        Width = 539
        Height = 380
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 539
        ExplicitHeight = 217
        inherited Panel: TPanel
          Top = 339
          Width = 539
          ExplicitTop = 176
          ExplicitWidth = 539
          inherited sbAdd: TSpeedButton
            Left = 280
            ExplicitLeft = 195
          end
          inherited sbInsert: TSpeedButton
            Left = 333
            ExplicitLeft = 232
          end
          inherited sbDelete: TSpeedButton
            Left = 385
            ExplicitLeft = 269
          end
          inherited seNumber: TJvSpinEdit
            OnChange = frmgrdDiversionsseNumberChange
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 539
          Height = 339
          ColCount = 2
          OnSetEditText = frmgrdDiversionsGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = 'Closest'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 60
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Integer
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
              PickList.Strings = (
                'Fraction'
                'Excess'
                'Threshold'
                'Up to')
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 539
          ExplicitHeight = 176
          ColWidths = (
            64
            79)
        end
      end
    end
    object tabCrossSection: TTabSheet
      Caption = 'Cross Section'
      ImageIndex = 5
      TabVisible = False
      object Splitter1: TSplitter
        Left = 0
        Top = 266
        Width = 539
        Height = 5
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 268
      end
      inline frameCrossSection: TframeGrid
        Left = 0
        Top = 41
        Width = 539
        Height = 225
        Align = alClient
        TabOrder = 0
        ExplicitTop = 47
        ExplicitWidth = 539
        ExplicitHeight = 176
        inherited Panel: TPanel
          Top = 184
          Width = 539
          ExplicitTop = 184
          ExplicitWidth = 539
          inherited lbNumber: TLabel
            Width = 165
            Caption = 'Number or cross section points'
            ExplicitWidth = 165
          end
          inherited sbAdd: TSpeedButton
            Left = 447
            ExplicitLeft = 447
          end
          inherited sbInsert: TSpeedButton
            Left = 476
            ExplicitLeft = 476
          end
          inherited sbDelete: TSpeedButton
            Left = 505
            ExplicitLeft = 505
          end
          inherited seNumber: TJvSpinEdit
            MinValue = 2.000000000000000000
            Value = 2.000000000000000000
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 539
          Height = 184
          ColCount = 3
          RowCount = 3
          OnSelectCell = frameCrossSectionGridSelectCell
          OnSetEditText = frameCrossSectionGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
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
              AutoAdjustCaptionRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
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
          ExplicitLeft = 368
          ExplicitWidth = 171
          ExplicitHeight = 135
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 539
        Height = 41
        Align = alTop
        TabOrder = 1
        ExplicitTop = 182
        object cbSpecifyRoughnessFraction: TCheckBox
          Left = 8
          Top = 8
          Width = 241
          Height = 17
          Caption = ' Specify roughness fraction'
          TabOrder = 0
          OnClick = cbSpecifyRoughnessFractionClick
        end
      end
      object zbChannel: TQRbwZoomBox2
        Left = 0
        Top = 271
        Width = 539
        Height = 109
        Align = alBottom
        Caption = 'zbChannel'
        TabOrder = 2
        Exaggeration = 1.000000000000000000
        HorizontalDirection = hdRight
        Image32.Left = 0
        Image32.Top = 68
        Image32.Width = 539
        Image32.Height = 109
        Image32.Anchors = [akLeft, akBottom]
        Image32.Bitmap.ResamplerClassName = 'TNearestResampler'
        Image32.BitmapAlign = baTopLeft
        Image32.Color = clWhite
        Image32.ParentColor = False
        Image32.Scale = 1.000000000000000000
        Image32.ScaleMode = smNormal
        Image32.TabOrder = 0
        Image32.ExplicitTop = 0
        ImmediateResize = True
        Magnification = 1.000000000000000000
        VerticalDirection = vdUp
        ExplicitTop = 277
        DesignSize = (
          539
          109)
      end
    end
    object tabGWT: TTabSheet
      Caption = 'GWT'
      ImageIndex = 4
      object splSplit: TSplitter
        Left = 121
        Top = 0
        Width = 5
        Height = 380
        ExplicitLeft = 8
        ExplicitHeight = 212
      end
      object tvGwt: TJvPageListTreeView
        Left = 0
        Top = 0
        Width = 121
        Height = 380
        PageDefault = 0
        PageList = jplGwt
        Align = alLeft
        HideSelection = False
        Indent = 19
        TabOrder = 0
        Items.Links = {00000000}
        ExplicitHeight = 217
      end
      object jplGwt: TJvPageList
        Left = 126
        Top = 0
        Width = 413
        Height = 380
        PropagateEnable = False
        Align = alClient
        ExplicitHeight = 217
      end
    end
  end
end
