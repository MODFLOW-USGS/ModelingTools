inherited frameSutraBoundary: TframeSutraBoundary
  Height = 352
  ExplicitHeight = 352
  inherited pnlBottom: TPanel
    Top = 224
    Height = 128
    ExplicitTop = 224
    ExplicitHeight = 128
    DesignSize = (
      320
      128)
    inherited lblNumTimes: TLabel
      Left = 65
      Top = 94
      ExplicitLeft = 65
      ExplicitTop = 94
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
      Top = 88
      TabOrder = 2
      ExplicitLeft = 10
      ExplicitTop = 88
    end
    inherited btnDelete: TBitBtn
      Left = 237
      Top = 88
      TabOrder = 1
      ExplicitLeft = 237
      ExplicitTop = 88
    end
    inherited btnInsert: TBitBtn
      Left = 149
      Top = 88
      TabOrder = 0
      ExplicitLeft = 149
      ExplicitTop = 88
    end
    object comboFluidSourceInLakesPresent: TComboBox
      Left = 10
      Top = 29
      Width = 286
      Height = 21
      Style = csDropDownList
      ItemIndex = 3
      TabOrder = 3
      Text = 'Use defaults'
      Items.Strings = (
        'Apply if lake water absent (-1)'
        'No change (0)'
        'Apply if lake water present (1)'
        'Use defaults')
    end
    object cbBCTime: TCheckBox
      Left = 10
      Top = 65
      Width = 177
      Height = 17
      AllowGrayed = True
      Caption = 'Use BCTime'
      TabOrder = 4
      OnClick = cbBCTimeClick
    end
  end
  inherited pnlGrid: TPanel
    Top = 81
    Height = 143
    ExplicitTop = 81
    ExplicitHeight = 143
    inherited rdgSutraFeature: TRbwDataGrid4
      Top = 51
      Height = 91
      TabOrder = 1
      OnMouseUp = rdgSutraFeatureMouseUp
      OnSelectCell = rdgSutraFeatureSelectCell
      OnSetEditText = rdgSutraFeatureSetEditText
      OnBeforeDrawCell = rdgSutraFeatureBeforeDrawCell
      OnColSize = rdgSutraFeatureColSize
      OnHorizontalScroll = rdgSutraFeatureHorizontalScroll
      Columns = <
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
          AutoAdjustCaptionRowHeights = False
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
          AutoAdjustCaptionRowHeights = False
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
      ExplicitHeight = 91
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
