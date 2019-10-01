inherited frameScreenObjectMt3dSft: TframeScreenObjectMt3dSft
  Width = 482
  Height = 399
  ExplicitWidth = 482
  ExplicitHeight = 399
  inherited pnlBottom: TPanel
    Top = 353
    Width = 482
    ExplicitTop = 353
    ExplicitWidth = 482
    DesignSize = (
      482
      46)
    inherited seNumberOfTimes: TJvSpinEdit
      Top = 12
      ExplicitTop = 12
    end
    inherited btnDelete: TBitBtn
      Left = 394
      ExplicitLeft = 394
    end
    inherited btnInsert: TBitBtn
      Left = 306
      ExplicitLeft = 306
    end
  end
  inherited pnlTop: TPanel
    Width = 482
    ExplicitWidth = 482
    inherited pnlCaption: TPanel
      Width = 480
      ExplicitWidth = 480
    end
  end
  inherited pnlGrid: TPanel
    Width = 482
    Height = 135
    ExplicitWidth = 482
    ExplicitHeight = 135
    inherited pnlEditGrid: TPanel
      Width = 480
      ExplicitWidth = 480
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 480
      Height = 83
      ExplicitLeft = 1
      ExplicitWidth = 480
      ExplicitHeight = 83
    end
  end
  object pgcSft: TPageControl
    Left = 0
    Top = 160
    Width = 482
    Height = 193
    ActivePage = tsRunoff
    Align = alBottom
    TabOrder = 3
    object tsSteady: TTabSheet
      Caption = 'Steady'
      object lblObsLocation: TLabel
        Left = 176
        Top = 6
        Width = 99
        Height = 13
        Caption = 'Observation location'
      end
      object rdgSftInitConcAndDisp: TRbwDataGrid4
        AlignWithMargins = True
        Left = 0
        Top = 35
        Width = 474
        Height = 130
        Margins.Left = 0
        Margins.Top = 35
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        ColCount = 3
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
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
            AutoAdjustColWidths = False
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
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
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
      end
      object comboObsLocation: TComboBox
        Left = 16
        Top = 3
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = 'None'
        Items.Strings = (
          'None'
          'First reach'
          'Last reach'
          'All reaches')
      end
    end
    object tsHeadWaters: TTabSheet
      Caption = 'Head Water Conc.'
      ImageIndex = 1
    end
    object tsPrecipitation: TTabSheet
      Caption = 'Precipitation Conc.'
      ImageIndex = 2
      object pnlBottomPrecip: TPanel
        Left = 0
        Top = 119
        Width = 474
        Height = 46
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          474
          46)
        object lblNumberOfTimesPrecip: TLabel
          Left = 64
          Top = 15
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seNumberOfTimesPrecip: TJvSpinEdit
          Left = 9
          Top = 12
          Width = 49
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 0
          OnChange = seNumberOfTimesPrecipChange
        end
        object btnDeletePrecip: TBitBtn
          Left = 390
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
          TabOrder = 2
          OnClick = btnDeletePrecipClick
        end
        object btnInsertPrecip: TBitBtn
          Left = 306
          Top = 6
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
          TabOrder = 1
          OnClick = btnInsertPrecipClick
        end
      end
      object pnlPrecip: TPanel
        Left = 0
        Top = 0
        Width = 474
        Height = 119
        Align = alClient
        TabOrder = 1
        object pnlPrecipFormula: TPanel
          Left = 1
          Top = 1
          Width = 472
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblPrecipFormula: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdePrecipFormula: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            Text = ''
            OnChange = rdePrecipFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
        object rdgPrecip: TRbwDataGrid4
          Left = 1
          Top = 51
          Width = 472
          Height = 67
          Align = alClient
          ColCount = 3
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = rdgPrecipMouseUp
          OnSelectCell = rdgPrecipSelectCell
          OnSetEditText = rdgPrecipSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgPrecipBeforeDrawCell
          OnColSize = rdgPrecipColSize
          ColorRangeSelection = False
          OnHorizontalScroll = rdgPrecipHorizontalScroll
          Columns = <
            item
              AutoAdjustRowHeights = False
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
        end
      end
    end
    object tsRunoff: TTabSheet
      Caption = 'Runoff Conc.'
      ImageIndex = 3
      object pnlBottomRunoff: TPanel
        Left = 0
        Top = 119
        Width = 474
        Height = 46
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          474
          46)
        object lblNumberOfTimesRunoff: TLabel
          Left = 64
          Top = 15
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seNumberOfTimesRunoff: TJvSpinEdit
          Left = 9
          Top = 12
          Width = 49
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 0
          OnChange = seNumberOfTimesRunoffChange
        end
        object btnDeleteRunoff: TBitBtn
          Left = 390
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
          TabOrder = 2
          OnClick = btnDeleteRunoffClick
        end
        object btnInsertRunoff: TBitBtn
          Left = 306
          Top = 6
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
          TabOrder = 1
          OnClick = btnInsertRunoffClick
        end
      end
      object pnlRunoff: TPanel
        Left = 0
        Top = 0
        Width = 474
        Height = 119
        Align = alClient
        TabOrder = 1
        object pnlFormulaRunoff: TPanel
          Left = 1
          Top = 1
          Width = 472
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFormulaRunoff: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeFormulaRunoff: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            Text = ''
            OnChange = rdeFormulaRunoffChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
        object rdgRunoff: TRbwDataGrid4
          Left = 1
          Top = 51
          Width = 472
          Height = 67
          Align = alClient
          ColCount = 3
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = rdgRunoffMouseUp
          OnSelectCell = rdgRunoffSelectCell
          OnSetEditText = rdgRunoffSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgRunoffBeforeDrawCell
          OnColSize = rdgRunoffColSize
          ColorRangeSelection = False
          OnHorizontalScroll = rdgRunoffHorizontalScroll
          Columns = <
            item
              AutoAdjustRowHeights = False
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
        end
      end
    end
    object tsConstantConcentration: TTabSheet
      Caption = 'Constant Conc.'
      ImageIndex = 4
      object pnlBottomConstConc: TPanel
        Left = 0
        Top = 119
        Width = 474
        Height = 46
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          474
          46)
        object lblNumberOfTimesConstConc: TLabel
          Left = 64
          Top = 16
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seNumberOfTimesConstConc: TJvSpinEdit
          Left = 9
          Top = 12
          Width = 49
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 0
          OnChange = seNumberOfTimesConstConcChange
        end
        object btnDeleteConstConc: TBitBtn
          Left = 390
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
          TabOrder = 2
          OnClick = btnDeleteConstConcClick
        end
        object btnInsertConstConc: TBitBtn
          Left = 306
          Top = 6
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
          TabOrder = 1
          OnClick = btnInsertClick
        end
      end
      object pnlGridConstConc: TPanel
        Left = 0
        Top = 0
        Width = 474
        Height = 119
        Align = alClient
        TabOrder = 1
        object pnlFormulaConstConc: TPanel
          Left = 1
          Top = 1
          Width = 472
          Height = 50
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFormulaConstConc: TLabel
            Left = 136
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object rdeFormulaConstConc: TRbwDataEntry
            Left = 136
            Top = 24
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 0
            Text = ''
            OnChange = rdeFormulaConstConcChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
        object rdgConstConc: TRbwDataGrid4
          Left = 1
          Top = 51
          Width = 472
          Height = 67
          Align = alClient
          ColCount = 3
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = rdgConstConcMouseUp
          OnSelectCell = rdgConstConcSelectCell
          OnSetEditText = rdgConstConcSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgConstConcBeforeDrawCell
          OnColSize = rdgConstConcColSize
          ColorRangeSelection = False
          OnHorizontalScroll = rdgConstConcHorizontalScroll
          Columns = <
            item
              AutoAdjustRowHeights = False
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
        end
      end
    end
  end
end
