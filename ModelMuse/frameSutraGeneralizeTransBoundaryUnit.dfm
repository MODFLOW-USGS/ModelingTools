inherited frameSutraGeneralizeTransBoundary: TframeSutraGeneralizeTransBoundary
  Width = 586
  Height = 489
  ExplicitWidth = 586
  ExplicitHeight = 489
  inherited pnlBottom: TPanel
    Top = 392
    Width = 586
    Height = 97
    ExplicitTop = 392
    ExplicitWidth = 586
    ExplicitHeight = 97
    inherited lblNumTimes: TLabel
      Top = 71
      ExplicitTop = 71
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
      Left = 9
      Top = 63
      Height = 21
      ExplicitLeft = 9
      ExplicitTop = 63
      ExplicitHeight = 21
    end
    inherited btnDelete: TBitBtn
      Left = 498
      Top = 62
      ExplicitLeft = 498
      ExplicitTop = 62
    end
    inherited btnInsert: TBitBtn
      Left = 414
      Top = 62
      ExplicitLeft = 414
      ExplicitTop = 62
    end
    object comboGeneralizedTransportPresent: TComboBox
      Left = 9
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
  end
  inherited pnlGrid: TPanel
    Top = 131
    Width = 586
    Height = 261
    ExplicitTop = 131
    ExplicitWidth = 586
    ExplicitHeight = 261
    inherited rdgSutraFeature: TRbwDataGrid4
      Width = 584
      Height = 259
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
