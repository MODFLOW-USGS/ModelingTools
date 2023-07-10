inherited framePackageBuoyancy: TframePackageBuoyancy
  Width = 575
  Height = 377
  ExplicitWidth = 575
  ExplicitHeight = 377
  DesignSize = (
    575
    377)
  object lblRefDensity: TLabel [2]
    Left = 176
    Top = 231
    Width = 121
    Height = 15
    Caption = 'Reference density'
  end
  inherited memoComments: TMemo
    Width = 544
  end
  object cbSpecifyDensity: TCheckBox [4]
    Left = 16
    Top = 160
    Width = 137
    Height = 26
    Caption = 'Specify density'
    Enabled = False
    TabOrder = 1
  end
  object cbRHS: TCheckBox [5]
    Left = 16
    Top = 183
    Width = 544
    Height = 26
    Caption = 'Add off-diagonal terms to right-hand-side (HHFORMULATION_RHS)'
    Enabled = False
    TabOrder = 2
  end
  object cbWriteDensity: TCheckBox [6]
    Left = 16
    Top = 206
    Width = 209
    Height = 26
    Caption = 'Write density'
    Enabled = False
    TabOrder = 3
  end
  object rdgChemDensity: TRbwDataGrid4 [7]
    Left = 0
    Top = 256
    Width = 575
    Height = 121
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    Enabled = False
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 4
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    Columns = <
      item
        AutoAdjustRowHeights = True
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
    WordWrapRowCaptions = False
    ExplicitWidth = 422
  end
  object rdeRefDensity: TRbwDataEntry [8]
    Left = 16
    Top = 228
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = cbSpecifyDensity
      end
      item
        Control = cbRHS
      end
      item
        Control = cbWriteDensity
      end
      item
        Control = rdeRefDensity
      end
      item
        Control = rdgChemDensity
      end>
  end
end
