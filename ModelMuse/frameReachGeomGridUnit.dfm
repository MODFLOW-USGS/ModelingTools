inherited frameReachGeomGrid: TframeReachGeomGrid
  Width = 491
  Height = 237
  OnResize = FrameResize
  ExplicitWidth = 491
  ExplicitHeight = 237
  inherited Panel: TPanel
    Top = 196
    Width = 491
    TabOrder = 4
    ExplicitTop = 196
    ExplicitWidth = 491
    DesignSize = (
      491
      41)
    inherited sbAdd: TSpeedButton
      Left = 255
      ExplicitLeft = 255
    end
    inherited sbInsert: TSpeedButton
      Left = 302
      ExplicitLeft = 302
    end
    inherited sbDelete: TSpeedButton
      Left = 350
      ExplicitLeft = 350
    end
  end
  inherited Grid: TRbwDataGrid4
    AlignWithMargins = True
    Left = 3
    Top = 40
    Width = 485
    Height = 153
    Margins.Top = 40
    ColCount = 12
    DefaultColWidth = 10
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 3
    OnMouseUp = GridMouseUp
    OnColSize = GridColSize
    OnHorizontalScroll = GridHorizontalScroll
    Columns = <
      item
        AutoAdjustRowHeights = True
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Rectangular cross section (1)'
          'Trapezoidal cross section (2)'
          'Irregular cross section (3)'
          'Table of values (4)'
          'Entire cell (5)')
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Fixed conductance (0)'
          'Specified leakance (1)'
          'K from flow package (2)'
          'Leakance and K (3)')
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
        CheckMin = True
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
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        CheckMin = True
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
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        CheckMin = True
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
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        CheckMin = True
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
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -13
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
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    ExplicitLeft = 3
    ExplicitTop = 30
    ExplicitWidth = 485
    ExplicitHeight = 163
  end
  object comboType: TJvImageComboBox
    Left = 79
    Top = 3
    Width = 145
    Height = 26
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 20
    ItemIndex = -1
    TabOrder = 0
    OnChange = comboTypeChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Rectangular cross section (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Trapezoidal cross section (2)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Irregular cross section (3)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Table of values (4)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Entire cell (5)'
      end>
  end
  object comboMethod: TJvImageComboBox
    Left = 230
    Top = 3
    Width = 145
    Height = 26
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 20
    ItemIndex = -1
    TabOrder = 1
    OnChange = comboMethodChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Fixed conductance (0) '
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Specified leakance (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'K from flow package (2)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Leakance and K (3)'
      end>
  end
  object rdeValue: TRbwDataEntry
    Left = 381
    Top = 3
    Width = 94
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '0'
    OnChange = rdeValueChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
end
