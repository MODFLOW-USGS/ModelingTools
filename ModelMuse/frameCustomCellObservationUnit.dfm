object frameCustomCellObservation: TframeCustomCellObservation
  Left = 0
  Top = 0
  Width = 568
  Height = 350
  TabOrder = 0
  OnResize = FrameResize
  object pcData: TJvPageControl
    Left = 0
    Top = 81
    Width = 568
    Height = 269
    ActivePage = tabLayers
    Align = alClient
    TabOrder = 2
    object tabTimes: TTabSheet
      Caption = 'Observation times'
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 560
        Height = 35
        Align = alTop
        TabOrder = 0
        object rdeMultiValueEdit: TRbwDataEntry
          Left = 8
          Top = 5
          Width = 61
          Height = 22
          TabOrder = 0
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 163
        Width = 560
        Height = 76
        Align = alBottom
        TabOrder = 2
        ExplicitTop = 165
        object lblNumberOfTimes: TLabel
          Left = 63
          Top = 9
          Width = 90
          Height = 15
          Caption = 'Number of times'
        end
        object seTimes: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 49
          Height = 23
          CheckMinValue = True
          ButtonKind = bkClassic
          TabOrder = 0
          OnChange = seTimesChange
        end
        object btnDeleteValue: TButton
          Left = 8
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = btnDeleteValueClick
        end
        object btnInsertValue: TButton
          Left = 71
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Insert'
          TabOrder = 2
          OnClick = btnInsertValueClick
        end
      end
      object rdgObservations: TRbwDataGrid4
        Left = 0
        Top = 35
        Width = 560
        Height = 128
        Align = alClient
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
        TabOrder = 1
        OnExit = rdgObservationsExit
        OnSelectCell = rdgObservationsSelectCell
        OnSetEditText = rdgObservationsSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnColSize = rdgObservationsColSize
        ColorRangeSelection = False
        OnHorizontalScroll = rdgObservationsHorizontalScroll
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
            ComboUsed = False
            Format = rcf4Real
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
            Format = rcf4Real
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Variance (0)'
              'Standard dev. (1)'
              'Coef. of var. (2)'
              'Weight (3)'
              'Sq. rt. of weight (4)')
            WordWrapCaptions = False
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
            WordWrapCaptions = False
            WordWrapCells = True
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
        ColWidths = (
          64
          64
          64
          127
          213)
      end
    end
    object tabLayers: TTabSheet
      Caption = 'Layers'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 163
        Width = 560
        Height = 76
        Align = alBottom
        TabOrder = 2
        ExplicitTop = 165
        object lblNumberOfLayers: TLabel
          Left = 64
          Top = 9
          Width = 91
          Height = 15
          Caption = 'Number of layers'
        end
        object seLayers: TJvSpinEdit
          Left = 8
          Top = 6
          Width = 49
          Height = 23
          CheckMinValue = True
          ButtonKind = bkClassic
          TabOrder = 0
          OnChange = seLayersChange
        end
        object btnDeleteLayer: TButton
          Left = 8
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = btnDeleteLayerClick
        end
        object btnInsertLayer: TButton
          Left = 71
          Top = 41
          Width = 57
          Height = 25
          Caption = 'Insert'
          TabOrder = 2
          OnClick = btnInsertLayerClick
        end
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 560
        Height = 35
        Align = alTop
        TabOrder = 0
        object rdeMultiLayerEdit: TRbwDataEntry
          Left = 3
          Top = 7
          Width = 61
          Height = 22
          TabOrder = 0
          Text = '0'
          OnChange = rdeMultiLayerEditChange
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
      object rdgLayers: TRbwDataGrid4
        Left = 0
        Top = 35
        Width = 560
        Height = 128
        Align = alClient
        ColCount = 2
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
        TabOrder = 1
        OnExit = rdgLayersExit
        OnSelectCell = rdgLayersSelectCell
        OnSetEditText = rdgLayersSetEditText
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
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
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
            Format = rcf4Real
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
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 25
    Align = alTop
    Caption = 'pnlCaption'
    TabOrder = 0
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 568
    Height = 56
    Align = alTop
    TabOrder = 1
    object lblTreatment: TLabel
      Left = 200
      Top = 6
      Width = 90
      Height = 15
      Caption = 'Observation type'
    end
    object edObsName: TLabeledEdit
      Left = 8
      Top = 22
      Width = 137
      Height = 23
      EditLabel.Width = 97
      EditLabel.Height = 15
      EditLabel.Caption = 'Observation name'
      MaxLength = 12
      OEMConvert = True
      TabOrder = 0
      Text = ''
      OnChange = edObsNameChange
      OnExit = edObsNameExit
    end
    object comboTreatment: TComboBox
      Left = 200
      Top = 22
      Width = 145
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Observation'
      OnChange = comboTreatmentChange
      Items.Strings = (
        'Observation'
        'Prediction'
        'Inactive')
    end
  end
end
