inherited frameSutraGeneralizeTransBoundary: TframeSutraGeneralizeTransBoundary
  Height = 264
  ExplicitHeight = 264
  inherited pnlBottom: TPanel
    Top = 218
    ExplicitTop = 218
  end
  inherited pnlGrid: TPanel
    Top = 131
    Height = 87
    ExplicitTop = 131
    ExplicitHeight = 63
    inherited rdgSutraFeature: TRbwDataGrid4
      Height = 85
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
      ExplicitLeft = 2
      ExplicitTop = 2
      ExplicitHeight = 61
    end
  end
  inherited pnlTop: TPanel
    Height = 81
    ExplicitHeight = 81
    DesignSize = (
      320
      81)
    inherited lblSchedule: TLabel
      Left = 8
      Top = 30
      ExplicitLeft = 8
      ExplicitTop = 30
    end
    inherited comboSchedule: TComboBox
      Left = 8
      Top = 49
      ExplicitLeft = 8
      ExplicitTop = 49
    end
  end
  object pnlEditGrid: TPanel
    Left = 0
    Top = 81
    Width = 320
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
    object cbUsed: TCheckBox
      Left = 63
      Top = 27
      Width = 67
      Height = 17
      Caption = 'Used'
      Enabled = False
      TabOrder = 1
      OnClick = cbUsedClick
    end
  end
end
