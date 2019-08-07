inherited frameStructureGrid: TframeStructureGrid
  Width = 451
  Height = 304
  Align = alClient
  OnResize = FrameResize
  ExplicitWidth = 451
  ExplicitHeight = 304
  object lblType: TLabel [0]
    Left = 88
    Top = 3
    Width = 41
    Height = 16
    Caption = 'lblType'
    Enabled = False
  end
  object lblRestrictions: TLabel [1]
    Left = 173
    Top = 3
    Width = 79
    Height = 16
    Caption = 'lblRestrictions'
    Enabled = False
  end
  object lblInitialValueMethod: TLabel [2]
    Left = 245
    Top = 3
    Width = 118
    Height = 16
    Caption = 'lblInitialValueMethod'
    Enabled = False
  end
  object lblInitialValueTabFile: TLabel [3]
    Left = 317
    Top = 3
    Width = 118
    Height = 16
    Caption = 'lblInitialValueTabFile'
    Enabled = False
  end
  inherited Panel: TPanel
    Top = 263
    Width = 451
    TabOrder = 6
    ExplicitTop = 263
    ExplicitWidth = 451
    DesignSize = (
      451
      41)
    inherited sbAdd: TSpeedButton
      Left = 144
      ExplicitLeft = 144
    end
    inherited sbInsert: TSpeedButton
      Left = 173
      ExplicitLeft = 173
    end
    inherited sbDelete: TSpeedButton
      Left = 202
      ExplicitLeft = 202
    end
    inherited seNumber: TJvSpinEdit
      Height = 24
      ExplicitHeight = 24
    end
  end
  inherited Grid: TRbwDataGrid4
    AlignWithMargins = True
    Left = 3
    Top = 60
    Width = 445
    Height = 200
    Margins.Top = 60
    ColCount = 15
    DefaultColWidth = 160
    TabOrder = 5
    OnMouseDown = GridMouseDown
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
        CheckMin = True
        ComboUsed = False
        Format = rcf4Integer
        LimitToList = False
        Max = 1.000000000000000000
        MaxLength = 0
        Min = 1.000000000000000000
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
          'Uncontrolled with zero-depth gradient  (-2)'
          'No structure (0)'
          'Excess volume (1)'
          'Uncontrolled with critical-depth (2)'
          'Pump (3)'
          'Specified stage-discharge relation (4)'
          'Culvert( 5)'
          'Fixed crest weir (6)'
          'Fixed gated spillway (underflow gate) (7)'
          'Movable crest weir (overflow gate) (8)'
          'Gated spillway (underflow gate) (9)'
          'Generalized spillway equation (10)'
          'Inflow structure from SFR2 (11)'
          'Stage-dependent boundary (12)'
          'Overbank flow structure (13)')
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
        CheckMin = True
        ComboUsed = False
        Format = rcf4Integer
        LimitToList = False
        Max = 1.000000000000000000
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Restrict flow from ISTRCONN (<0)'
          'Bi-directional flow (0)'
          'Restrict flow to ISTRCONN (>0)')
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
        CheckMin = False
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Value'
          'Tabfile')
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
        CheckMin = False
        ComboUsed = True
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
        Format = rcf4Integer
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
        Format = rcf4Integer
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
    ExplicitTop = 60
    ExplicitWidth = 445
    ExplicitHeight = 200
    ColWidths = (
      82
      127
      131
      160
      160
      160
      160
      160
      160
      160
      160
      160
      160
      160
      160)
  end
  object comboType: TJvImageComboBox
    Left = 88
    Top = 27
    Width = 79
    Height = 26
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 79
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 20
    ItemIndex = -1
    TabOrder = 0
    OnChange = comboTypeChange
    Items = <>
  end
  object comboRestrictions: TJvImageComboBox
    Left = 173
    Top = 27
    Width = 66
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
    OnChange = comboRestrictionsChange
    Items = <>
  end
  object comboInitialValueMethod: TJvImageComboBox
    Left = 245
    Top = 27
    Width = 66
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
    TabOrder = 2
    OnChange = comboInitialValueMethodChange
    Items = <>
  end
  object comboInitialValueTabFile: TJvImageComboBox
    Left = 317
    Top = 27
    Width = 66
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
    TabOrder = 3
    OnChange = comboInitialValueTabFileChange
    Items = <>
  end
  object rdeValue: TRbwDataEntry
    Left = 389
    Top = 27
    Width = 35
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
    Text = '0'
    OnChange = rdeValueChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
end
