inherited frameSutraGeneralizeTransBoundary: TframeSutraGeneralizeTransBoundary
  Width = 586
  Height = 489
  ExplicitWidth = 586
  ExplicitHeight = 489
  inherited pnlBottom: TPanel
    Top = 368
    Width = 586
    Height = 121
    ExplicitLeft = 1
    ExplicitTop = 373
    ExplicitWidth = 586
    ExplicitHeight = 121
    DesignSize = (
      586
      121)
    inherited lblNumTimes: TLabel
      Left = 63
      Top = 87
      ExplicitLeft = 63
      ExplicitTop = 87
    end
    object lblGeneralizedTransportPresent: TLabel [1]
      Left = 8
      Top = 6
      Width = 150
      Height = 13
      Caption = 'If lake water present or absent'
    end
    object lbl1: TLabel [2]
      Left = 317
      Top = 6
      Width = 78
      Height = 13
      Caption = 'Interaction type'
    end
    inherited seNumberOfTimes: TJvSpinEdit
      Top = 79
      ExplicitTop = 79
    end
    inherited btnDelete: TBitBtn
      Left = 497
      Top = 78
      ExplicitLeft = 497
      ExplicitTop = 78
    end
    inherited btnInsert: TBitBtn
      Left = 413
      Top = 78
      ExplicitLeft = 413
      ExplicitTop = 78
    end
    object comboGeneralizedTransportPresent: TComboBox
      Left = 8
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
    object comboLakeGeneralizedTransportType: TComboBox
      Left = 317
      Top = 29
      Width = 262
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 4
      Text = 'Use defaults'
      Items.Strings = (
        'Like solute/energy source/sink (S)'
        'Like spec. conc./temp. (U)'
        'Use defaults')
    end
    object cbBCTime: TCheckBox
      Left = 8
      Top = 56
      Width = 185
      Height = 17
      Caption = 'Use BCTime'
      TabOrder = 5
      OnClick = cbBCTimeClick
    end
  end
  inherited pnlGrid: TPanel
    Top = 131
    Width = 586
    Height = 237
    ExplicitTop = 131
    ExplicitWidth = 586
    ExplicitHeight = 261
    inherited rdgSutraFeature: TRbwDataGrid4
      Width = 584
      Height = 235
      ColCount = 6
      OnMouseUp = rdgSutraFeatureMouseUp
      OnSelectCell = rdgSutraFeatureSelectCell
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
          AutoAdjustRowHeights = False
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
          WordWrapCaptions = False
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
        end
        item
          AutoAdjustRowHeights = False
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
      ExplicitWidth = 584
      ExplicitHeight = 259
    end
  end
  inherited pnlTop: TPanel
    Width = 586
    Height = 81
    ExplicitWidth = 586
    ExplicitHeight = 81
    DesignSize = (
      586
      81)
    inherited lblSchedule: TLabel
      Left = 8
      Top = 30
      ExplicitLeft = 8
      ExplicitTop = 30
    end
    inherited pnlCaption: TPanel
      Width = 584
      ExplicitWidth = 584
    end
    inherited comboSchedule: TComboBox
      Left = 8
      Top = 49
      Width = 572
      ExplicitLeft = 8
      ExplicitTop = 49
      ExplicitWidth = 572
    end
  end
  object pnlEditGrid: TPanel
    Left = 0
    Top = 81
    Width = 586
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
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
