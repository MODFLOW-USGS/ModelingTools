inherited frameConcentrationObservation: TframeConcentrationObservation
  inherited pcData: TJvPageControl
    Top = 99
    Height = 251
    ActivePage = tabTimes
    ExplicitTop = 99
    ExplicitHeight = 251
    inherited tabTimes: TTabSheet
      ExplicitHeight = 221
      inherited Panel5: TPanel
        inherited rdeMultiValueEdit: TRbwDataEntry
          Left = 120
          Top = 6
          Height = 23
          TabOrder = 1
          OnChange = rdeMultiValueEditChange
          ExplicitLeft = 120
          ExplicitTop = 6
          ExplicitHeight = 23
        end
        object comboSpeciesNames: TComboBox
          Left = 8
          Top = 6
          Width = 89
          Height = 23
          Style = csDropDownList
          TabOrder = 0
          OnChange = comboSpeciesNamesChange
        end
      end
      inherited Panel2: TPanel
        Top = 145
        ExplicitTop = 145
        inherited lblNumberOfTimes: TLabel
          Left = 95
          ExplicitLeft = 95
        end
        inherited seTimes: TJvSpinEdit
          Width = 81
          ExplicitWidth = 81
        end
      end
      inherited rdgObservations: TRbwDataGrid4
        Height = 110
        ColCount = 6
        DefaultColWidth = 40
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 0
            ParentButtonFont = False
            PickList.Strings = (
              'Observation time'
              'Observation frequency')
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
        ExplicitHeight = 110
        ColWidths = (
          40
          40
          40
          40
          40
          239)
      end
    end
    inherited tabLayers: TTabSheet
      ExplicitHeight = 221
      inherited Panel4: TPanel
        Top = 145
        ExplicitTop = 147
      end
      inherited rdgLayers: TRbwDataGrid4
        Height = 110
        ExplicitHeight = 110
      end
    end
  end
  inherited pnlName: TPanel
    Height = 74
    ExplicitHeight = 74
    inherited lblTreatment: TLabel
      Left = 150
      Top = 11
      ExplicitLeft = 150
      ExplicitTop = 11
    end
    inherited edObsName: TLabeledEdit
      Left = 11
      Top = 27
      EditLabel.ExplicitLeft = 11
      EditLabel.ExplicitTop = 11
      ExplicitLeft = 11
      ExplicitTop = 27
    end
    inherited comboTreatment: TComboBox
      Left = 150
      Top = 27
      ExplicitLeft = 150
      ExplicitTop = 27
    end
  end
end
