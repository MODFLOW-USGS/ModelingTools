inherited frameSutraGeneralizedFlowBoundary: TframeSutraGeneralizedFlowBoundary
  Height = 423
  ExplicitHeight = 423
  inherited pnlBottom: TPanel
    Top = 296
    Height = 127
    TabOrder = 3
    ExplicitTop = 296
    ExplicitWidth = 587
    ExplicitHeight = 127
    DesignSize = (
      565
      127)
    inherited lblNumTimes: TLabel
      Left = 61
      Top = 101
      ExplicitLeft = 61
      ExplicitTop = 101
    end
    object lblGeneralizedFlowPresent: TLabel [1]
      Left = 6
      Top = 19
      Width = 157
      Height = 15
      Caption = 'If lake water present or absent'
    end
    object lblLakeGeneralizedFlowType: TLabel [2]
      Left = 320
      Top = 19
      Width = 83
      Height = 15
      Caption = 'Interaction type'
    end
    inherited seNumberOfTimes: TJvSpinEdit
      Left = 6
      Top = 93
      TabOrder = 1
      ExplicitLeft = 6
      ExplicitTop = 93
    end
    inherited btnDelete: TBitBtn
      Left = 478
      Top = 92
      TabOrder = 0
      ExplicitLeft = 500
      ExplicitTop = 92
    end
    inherited btnInsert: TBitBtn
      Left = 390
      Top = 92
      TabOrder = 2
      ExplicitLeft = 412
      ExplicitTop = 92
    end
    object comboGeneralizedFlowPresent: TComboBox
      Left = 6
      Top = 43
      Width = 286
      Height = 23
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'Apply if lake water absent (-1)'
        'No change (0)'
        'Apply if lake water present (1)'
        'Use defaults')
    end
    object comboLakeGeneralizedFlowType: TComboBox
      Left = 320
      Top = 43
      Width = 262
      Height = 23
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 4
      Text = 'Use defaults'
      Items.Strings = (
        'Like fluid source/sink (F)'
        'Like specified pressure (P)'
        'Use defaults')
    end
    object cbBCTime: TCheckBox
      Left = 6
      Top = 70
      Width = 227
      Height = 17
      Caption = 'Use BCTime'
      TabOrder = 5
      OnClick = cbBCTimeClick
    end
  end
  inherited pnlGrid: TPanel
    Top = 136
    Height = 160
    TabOrder = 2
    ExplicitTop = 136
    ExplicitWidth = 587
    ExplicitHeight = 160
    inherited rdgSutraFeature: TRbwDataGrid4
      Height = 158
      ColCount = 11
      OnMouseUp = rdgSutraFeatureMouseUp
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
          AutoAdjustRowHeights = False
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
          WordWrapCaptions = False
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
      ExplicitWidth = 585
      ExplicitHeight = 158
    end
  end
  inherited pnlTop: TPanel
    Height = 86
    ExplicitWidth = 587
    ExplicitHeight = 86
    DesignSize = (
      565
      86)
    inherited lblSchedule: TLabel
      Left = 14
      Top = 24
      ExplicitLeft = 14
      ExplicitTop = 24
    end
    inherited pnlCaption: TPanel
      ExplicitWidth = 585
    end
    inherited comboSchedule: TComboBox
      Left = 8
      Top = 46
      Width = 536
      ExplicitLeft = 8
      ExplicitTop = 46
      ExplicitWidth = 558
    end
  end
  object pnlEditGrid: TPanel
    Left = 0
    Top = 86
    Width = 565
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 587
    object lblFormula: TLabel
      Left = 136
      Top = 5
      Width = 44
      Height = 15
      Alignment = taCenter
      Caption = 'Formula'
    end
    object rdeFormula: TRbwDataEntry
      Left = 136
      Top = 23
      Width = 57
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 2
      Text = ''
      OnChange = rdeFormulaChange
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
    object comboLimit: TJvImageComboBox
      Left = 264
      Top = 18
      Width = 73
      Height = 25
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 19
      ItemIndex = -1
      TabOrder = 0
      OnChange = comboLimitChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'None'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Flow'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Pressure'
        end>
    end
    object comboExit: TJvImageComboBox
      Left = 370
      Top = 18
      Width = 73
      Height = 25
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 73
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 19
      ItemIndex = -1
      TabOrder = 1
      OnChange = comboExitChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Relative'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Direct'
        end>
    end
  end
end
