inherited frameLeach: TframeLeach
  Height = 256
  ExplicitHeight = 256
  inherited Panel: TPanel
    Top = 215
    ExplicitTop = 215
  end
  inherited Grid: TRbwDataGrid4
    Height = 158
    ColCount = 4
    OnSelectCell = GridSelectCell
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
          'Value'
          'Rhoades'
          'None'
          'Custom FMP Formula')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = True
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
        ButtonFont.Name = 'Segoe UI'
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
    ExplicitHeight = 158
    ColWidths = (
      64
      64
      138
      64)
  end
  inherited pnlTop: TPanel
    inherited comboChoice: TComboBox
      Visible = True
      Items.Strings = (
        'Value'
        'Rhoades'
        'None'
        'Custom FMP Formula')
    end
  end
end
