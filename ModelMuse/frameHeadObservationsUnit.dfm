inherited frameHeadObservations: TframeHeadObservations
  inherited pcData: TJvPageControl
    Top = 134
    Height = 216
    ActivePage = tabTimes
    ExplicitTop = 134
    ExplicitHeight = 216
    inherited tabTimes: TTabSheet
      ExplicitHeight = 186
      inherited Panel5: TPanel
        inherited rdeMultiValueEdit: TRbwDataEntry
          OnChange = rdeMultiValueEditChange
        end
        object comboMultiStatFlag: TJvImageComboBox
          Left = 99
          Top = 5
          Width = 89
          Height = 25
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          DroppedWidth = 145
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 19
          ItemIndex = -1
          TabOrder = 1
          OnChange = comboMultiStatFlagChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Variance'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Standard dev.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Coef. of var.'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Weight'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Sq. rt. of weight'
            end>
        end
      end
      inherited Panel2: TPanel
        Top = 110
        ExplicitTop = 110
      end
      inherited rdgObservations: TRbwDataGrid4
        Height = 75
        OnBeforeDrawCell = rdgObservationsBeforeDrawCell
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
            CheckMin = True
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
        ExplicitHeight = 75
      end
    end
    inherited tabLayers: TTabSheet
      ExplicitHeight = 186
      inherited Panel4: TPanel
        Top = 110
        ExplicitTop = 110
        inherited seLayers: TJvSpinEdit
          Left = 9
          ExplicitLeft = 9
        end
      end
      inherited rdgLayers: TRbwDataGrid4
        Height = 75
        ExplicitHeight = 75
      end
    end
  end
  inherited pnlName: TPanel
    Height = 109
    ExplicitHeight = 109
    object rgMultiObsMethod: TRadioGroup
      Left = 9
      Top = 49
      Width = 368
      Height = 56
      Caption = 'How will observations be analyzed? (ITT)'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'All heads (1)'
        'Calculate drawdown relative to first head (2)')
      TabOrder = 2
      OnClick = rgMultiObsMethodClick
    end
  end
end
