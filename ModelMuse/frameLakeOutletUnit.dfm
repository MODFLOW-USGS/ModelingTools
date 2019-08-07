inherited frameLakeOutlet: TframeLakeOutlet
  Width = 424
  Height = 267
  ExplicitWidth = 424
  ExplicitHeight = 267
  inherited Panel: TPanel
    Top = 182
    Width = 424
    Height = 85
    ExplicitTop = 182
    ExplicitWidth = 424
    ExplicitHeight = 85
    inherited lbNumber: TLabel
      Width = 78
      Caption = 'Number of times'
      ExplicitWidth = 78
    end
    inherited sbAdd: TSpeedButton
      Left = 275
      Anchors = [akTop, akRight]
      ExplicitLeft = 275
    end
    inherited sbInsert: TSpeedButton
      Left = 304
      Anchors = [akTop, akRight]
      ExplicitLeft = 304
    end
    inherited sbDelete: TSpeedButton
      Left = 333
      Anchors = [akTop, akRight]
      ExplicitLeft = 333
    end
    object lblOutletLake: TLabel [4]
      Left = 8
      Top = 37
      Width = 52
      Height = 13
      Caption = 'Outlet lake'
    end
    object lblOutletType: TLabel [5]
      Left = 264
      Top = 36
      Width = 55
      Height = 13
      Caption = 'Outlet type'
    end
    object comboOutletType: TJvImageComboBox
      Left = 264
      Top = 56
      Width = 145
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = 1
      TabOrder = 1
      OnChange = comboOutletTypeChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Specified'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Manning'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'Weir'
        end>
    end
    object comboOutlet: TComboBox
      Left = 8
      Top = 56
      Width = 241
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
  end
  inherited Grid: TRbwDataGrid4
    Width = 424
    Height = 125
    ColCount = 7
    OnSetEditText = GridSetEditText
    OnButtonClick = GridButtonClick
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
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
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
    ExplicitWidth = 424
    ExplicitHeight = 125
  end
  inherited pnlTop: TPanel
    Width = 424
    ExplicitWidth = 424
    inherited edFormula: TLabeledEdit
      EditLabel.ExplicitLeft = 0
      EditLabel.ExplicitTop = -16
      EditLabel.ExplicitWidth = 50
    end
  end
end
