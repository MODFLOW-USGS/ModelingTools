inherited frmImportShapeFile: TfrmImportShapeFile
  HelpKeyword = 'ImportShapeFileFrame'
  Top = 464
  Width = 563
  Height = 368
  VertScrollBar.Range = 0
  ActiveControl = dgFields
  Caption = 'Import Shapefile'
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  object dgFields: TRbwDataGrid
    Left = 0
    Top = 0
    Width = 563
    Height = 206
    Align = alClient
    ColCount = 4
    FixedColor = 14803425
    FixedCols = 1
    Font.Color = clBlack
    Font.Height = 18
    Font.Name = 'arial'
    Font.Pitch = fpVariable
    Font.Style = []
    Font.Weight = 48
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    ParentFont = False
    TabOrder = 0
    OnDrawCell = dgFieldsDrawCell
    OnSelectCell = dgFieldsSelectCell
    OnSetEditText = dgFieldsSetEditText
    ButtonColor = clButton
    Columns = <
      item
        ButtonCaption = '...'
        ButtonFont.Color = clBlack
        ButtonFont.Height = 18
        ButtonFont.Name = 'arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonFont.Weight = 48
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = False
        Format = rcfString
        LimitToList = False
        MaxLength = 0
        AutoAdjustColWidths = True
        AutoAdjustRowHeights = False
        WordWrapCaptions = False
        WordWrapCells = False
      end
      item
        ButtonCaption = '...'
        ButtonFont.Color = clBlack
        ButtonFont.Height = 18
        ButtonFont.Name = 'arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonFont.Weight = 48
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = False
        Format = rcfBoolean
        LimitToList = False
        MaxLength = 0
        AutoAdjustColWidths = True
        AutoAdjustRowHeights = False
        WordWrapCaptions = False
        WordWrapCells = False
      end
      item
        ButtonCaption = '...'
        ButtonFont.Color = clBlack
        ButtonFont.Height = 18
        ButtonFont.Name = 'arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonFont.Weight = 48
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = False
        Format = rcfCombo
        LimitToList = True
        MaxLength = 0
        AutoAdjustColWidths = True
        AutoAdjustRowHeights = False
        WordWrapCaptions = False
        WordWrapCells = False
      end
      item
        ButtonCaption = '...'
        ButtonFont.Color = clBlack
        ButtonFont.Height = 18
        ButtonFont.Name = 'arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonFont.Weight = 48
        ButtonUsed = False
        ButtonWidth = 25
        CheckMax = False
        CheckMin = False
        Format = rcfCombo
        LimitToList = True
        MaxLength = 0
        AutoAdjustColWidths = True
        AutoAdjustRowHeights = False
        WordWrapCaptions = False
        WordWrapCells = False
      end>
    RowHeights = (
      24
      33
      24
      24
      24)
  end
  object pnlButton: TPanel
    Left = 0
    Top = 206
    Width = 563
    Height = 162
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    object btnCancel: TBitBtn
      Left = 440
      Top = 116
      Width = 115
      Height = 33
      ParentColor = True
      TabOrder = 9
      Kind = bkCancel
    end
    object btnOK: TBitBtn
      Left = 320
      Top = 116
      Width = 115
      Height = 33
      ParentColor = True
      TabOrder = 8
      OnClick = btnOKClick
      Kind = bkOK
    end
    object cbEnclosedCells: TCheckBox
      Left = 6
      Top = 5
      Width = 395
      Height = 31
      Caption = 'Set properties of enclosed elements'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbIntersectedCells: TCheckBox
      Left = 6
      Top = 37
      Width = 395
      Height = 31
      Caption = 'Set properties of intersected elements'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object rgEvaluatedAt: TRadioGroup
      Left = 8
      Top = 104
      Width = 185
      Height = 49
      Items.Strings = (
        'Elements'
        'Nodes')
      Caption = 'Evaluated at'
      ColumnLayout = clAutoSize
      Columns = 2
      ItemIndex = 0
      TabOrder = 3
      OnClick = rgEvaluatedAtClick
    end
    object cbInterpolation: TCheckBox
      Left = 8
      Top = 69
      Width = 465
      Height = 31
      Caption = 'Set properties of elements by interpolation'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object btnNone: TButton
      Left = 440
      Top = 44
      Width = 115
      Height = 33
      Caption = 'Select None'
      ParentColor = True
      TabOrder = 5
      OnClick = btnSelectClick
    end
    object btnAll: TButton
      Left = 440
      Top = 8
      Width = 115
      Height = 33
      Caption = 'Select All'
      ParentColor = True
      TabOrder = 4
      OnClick = btnSelectClick
    end
    object btnToggle: TButton
      Left = 440
      Top = 80
      Width = 115
      Height = 33
      Caption = 'Toggle'
      ParentColor = True
      TabOrder = 6
      OnClick = btnToggleClick
    end
    object btnHelp: TBitBtn
      Left = 200
      Top = 116
      Width = 115
      Height = 33
      ParentColor = True
      TabOrder = 7
      Kind = bkHelp
    end
  end
  object xbShapeDataBase: TXBase
    Active = False
    AutoUpDate = True
    DebugErr = False
    Deleted = False
    Left = 24
    Top = 104
  end
  object OpenDialogShape: TOpenDialog
    Filter = 'Shape Files (*.shp)|*.shp'
    FilterIndex = 0
    Height = 0
    Width = 0
    Left = 64
    Top = 104
  end
end
