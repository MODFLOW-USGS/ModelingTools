inherited frameSutraBoundary: TframeSutraBoundary
  Height = 322
  ExplicitHeight = 322
  inherited pnlBottom: TPanel
    Top = 227
    Height = 95
    ExplicitTop = 184
    ExplicitHeight = 95
    inherited lblNumTimes: TLabel
      Left = 65
      Top = 62
      ExplicitLeft = 65
      ExplicitTop = 62
    end
    object lblFluidSourceInLakesPresent: TLabel [1]
      Left = 10
      Top = 5
      Width = 150
      Height = 13
      Caption = 'If lake water present or absent'
    end
    inherited seNumberOfTimes: TJvSpinEdit
      Left = 10
      Top = 56
      Height = 21
      TabOrder = 2
      ExplicitLeft = 10
      ExplicitTop = 56
      ExplicitHeight = 21
    end
    inherited btnDelete: TBitBtn
      Left = 237
      Top = 56
      TabOrder = 1
      ExplicitLeft = 237
      ExplicitTop = 56
    end
    inherited btnInsert: TBitBtn
      Left = 149
      Top = 56
      TabOrder = 0
      ExplicitLeft = 149
      ExplicitTop = 56
    end
    object comboFluidSourceInLakesPresent: TComboBox
      Left = 10
      Top = 29
      Width = 286
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 1
      TabOrder = 3
      Text = 'No change (0)'
      Items.Strings = (
        'Apply if lake water absent (-1)'
        'No change (0)'
        'Apply if lake water present (1)')
    end
  end
  inherited pnlGrid: TPanel
    Top = 81
    Height = 146
    ExplicitTop = 81
    ExplicitHeight = 113
    inherited rdgSutraFeature: TRbwDataGrid4
      Top = 51
      Height = 94
      TabOrder = 1
      OnMouseUp = rdgSutraFeatureMouseUp
      OnSetEditText = rdgSutraFeatureSetEditText
      OnBeforeDrawCell = rdgSutraFeatureBeforeDrawCell
      OnColSize = rdgSutraFeatureColSize
      OnHorizontalScroll = rdgSutraFeatureHorizontalScroll
      Columns = <
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
          ButtonCaption = '...'
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -13
          ButtonFont.Name = 'Tahoma'
          ButtonFont.Style = []
          ButtonUsed = False
          ButtonWidth = 20
          CheckMax = False
          CheckMin = False
          ComboUsed = False
          Format = rcf4Boolean
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
          AutoAdjustColWidths = True
        end>
      OnEndUpdate = rdgSutraFeatureEndUpdate
      ExplicitTop = 51
      ExplicitHeight = 61
    end
    object pnlEditGrid: TPanel
      Left = 1
      Top = 1
      Width = 318
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblFormula: TLabel
        Left = 136
        Top = 5
        Width = 38
        Height = 13
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
  end
  inherited pnlTop: TPanel
    Height = 81
    ExplicitHeight = 81
    DesignSize = (
      320
      81)
    inherited lblSchedule: TLabel
      Left = 1
      Top = 30
      ExplicitLeft = 1
      ExplicitTop = 30
    end
    inherited comboSchedule: TComboBox
      Left = 1
      Top = 49
      ExplicitLeft = 1
      ExplicitTop = 49
    end
  end
end
