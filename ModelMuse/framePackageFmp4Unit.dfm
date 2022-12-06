inherited framePackageFmp4: TframePackageFmp4
  Width = 575
  Height = 500
  ExplicitWidth = 575
  ExplicitHeight = 500
  DesignSize = (
    575
    500)
  inherited lblComments: TLabel
    Enabled = True
  end
  inherited memoComments: TMemo
    Width = 544
    Height = 52
    Enabled = True
    ExplicitWidth = 544
    ExplicitHeight = 52
  end
  object CategoryPanelGroup1: TCategoryPanelGroup [3]
    Left = 0
    Top = 120
    Width = 575
    Height = 380
    VertScrollBar.Tracking = True
    Align = alBottom
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Segoe UI'
    HeaderFont.Style = []
    TabOrder = 1
    object cpnlWaterBalanceRegions: TCategoryPanel
      Top = 661
      Height = 400
      Caption = 'Water Balance Subregion Options (Farm Options)'
      TabOrder = 0
      ExplicitWidth = 550
      object rdgFarms: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 552
        Height = 374
        Align = alClient
        ColCount = 4
        FixedCols = 1
        RowCount = 11
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSelectCell = rdgFarmsSelectCell
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
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Array'
              'List')
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
            CheckMin = False
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'By demand'
              'By average')
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        ExplicitWidth = 553
        ExplicitHeight = 284
      end
    end
    object cpnlOptions: TCategoryPanel
      Top = 537
      Height = 124
      Caption = 'Options'
      TabOrder = 1
      ExplicitWidth = 550
      object cbAllowPrinting: TCheckBox
        Left = 8
        Top = 8
        Width = 308
        Height = 17
        Caption = 'Allow printing to listing file (inverse of NOPRINT)'
        TabOrder = 0
      end
      object cbWellField: TCheckBox
        Left = 8
        Top = 31
        Width = 441
        Height = 29
        Caption = 
          'Allow for aquifer storage and recovery for non-routed deliveries' +
          ' (WELLFIELD)'
        TabOrder = 1
        WordWrap = True
      end
      object cbRecompute: TCheckBox
        Left = 8
        Top = 74
        Width = 449
        Height = 17
        Caption = 'Calls FMP FM routine before budget calculation (RECOMP_Q_BD)'
        TabOrder = 2
      end
    end
    object cpnlMnw2: TCategoryPanel
      Top = 294
      Height = 243
      Caption = 'MNW2 Closure Criteria'
      TabOrder = 2
      ExplicitWidth = 550
      object lblQClose: TLabel
        Left = 74
        Top = 66
        Width = 383
        Height = 45
        Caption = 
          'Criterion for actual MNW pumping rate to converge to FMP pumping' +
          ' requirement (QCLOSE) '
        WordWrap = True
      end
      object lblHPCT: TLabel
        Left = 74
        Top = 111
        Width = 367
        Height = 30
        Caption = 
          'Fraction of reduction of head-change closure criterion if QCLOSE' +
          ' was not met (HPCT)'
        WordWrap = True
      end
      object lblRPCT: TLabel
        Left = 74
        Top = 159
        Width = 351
        Height = 45
        Caption = 
          'Fraction of reduction of residual-change closure criterion if QC' +
          'LOSE was not met (RPCT)'
        WordWrap = True
      end
      object cbMnwClose: TCheckBox
        Left = 3
        Top = -3
        Width = 394
        Height = 75
        Caption = 
          'Adjust solver closure criteria to allow convergence of the FMP p' +
          'umping requirement to pumping simulated by the linked MNW2 packa' +
          'ge (MNWCLOSE option)'
        TabOrder = 0
        WordWrap = True
      end
      object rdeQClose: TRbwDataEntry
        Left = 3
        Top = 63
        Width = 65
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeHPCT: TRbwDataEntry
        Left = 3
        Top = 111
        Width = 65
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRPCT: TRbwDataEntry
        Left = 3
        Top = 157
        Width = 65
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object cpnlOutput: TCategoryPanel
      Top = 0
      Height = 294
      Caption = 'Print'
      TabOrder = 3
      ExplicitWidth = 550
      object clbPrint: TCheckListBox
        Left = 0
        Top = 0
        Width = 552
        Height = 228
        Align = alClient
        ItemHeight = 15
        Items.Strings = (
          'WBS_WATER_USE'
          'FARM_DEMAND_SUPPLY_SUMMARY'
          'FARM_BUDGET'
          'FARM_BUDGET_COMPACT'
          'FARM_NET_RECHARGE_ARRAY'
          'FARM_NET_RECHARGE_LIST'
          'EVAPOTRANSPIRATION_SUMMARY  SUM'
          'EVAPOTRANSPIRATION_SUMMARY SEPARATE'
          'ET_LIST'
          'FARM_WELL_SUMMARY'
          'LANDSCAPE_RUNOFF [COMPACT]'
          'DEEP_PERCOLATION    [COMPACT]')
        TabOrder = 0
        ExplicitWidth = 553
        ExplicitHeight = 234
      end
      object Panel1: TPanel
        Left = 0
        Top = 228
        Width = 552
        Height = 40
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 234
        ExplicitWidth = 553
        object lblPrintRouting: TLabel
          Left = 168
          Top = 8
          Width = 67
          Height = 15
          Caption = 'Print routing'
        end
        object comboPrintRouting: TComboBox
          Left = 13
          Top = 6
          Width = 145
          Height = 23
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Don'#39't use'
          Items.Strings = (
            'Don'#39't use'
            'Static'
            'Transient')
        end
      end
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = cbAllowPrinting
      end
      item
        Control = cbMnwClose
      end
      item
        Control = cbRecompute
      end
      item
        Control = cbWellField
      end
      item
        Control = clbPrint
      end
      item
        Control = comboPrintRouting
      end
      item
        Control = rdeHPCT
      end
      item
        Control = rdeQClose
      end
      item
        Control = rdeRPCT
      end
      item
        Control = rdgFarms
      end>
    Enabled = True
  end
end
