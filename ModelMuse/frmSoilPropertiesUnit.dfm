inherited frmSoilProperties: TfrmSoilProperties
  HelpType = htKeyword
  HelpKeyword = 'Farm_Soil_Properties_Dialog_Bo'
  Caption = 'Farm Soil Properties'
  ClientHeight = 305
  ClientWidth = 681
  OnResize = FormResize
  ExplicitWidth = 697
  ExplicitHeight = 344
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 261
    Width = 681
    Height = 44
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    object btnCancel: TBitBtn
      Left = 586
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 489
      Top = 6
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 392
      Top = 6
      Width = 91
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  inline frameSoils: TframeFormulaGrid
    Left = 0
    Top = 0
    Width = 681
    Height = 261
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 681
    ExplicitHeight = 261
    inherited Panel: TPanel
      Top = 220
      Width = 681
      ExplicitTop = 220
      ExplicitWidth = 681
      inherited lbNumber: TLabel
        Width = 108
        Height = 18
        Caption = 'Number of soils'
        ExplicitWidth = 108
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 358
        ExplicitLeft = 316
      end
      inherited sbInsert: TSpeedButton
        Left = 424
        ExplicitLeft = 374
      end
      inherited sbDelete: TSpeedButton
        Left = 489
        ExplicitLeft = 432
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        OnChange = frameSoilsseNumberChange
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 681
      Height = 163
      ColCount = 9
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
      OnMouseUp = frameSoilsGridMouseUp
      OnRowMoved = frameSoilsGridRowMoved
      OnSelectCell = frameSoilsGridSelectCell
      OnSetEditText = frameSoilsGridSetEditText
      OnButtonClick = frameSoilsGridButtonClick
      OnHorizontalScroll = frameSoilsGridHorizontalScroll
      Columns = <
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          LimitToList = False
          MaxLength = 0
          ParentButtonFont = False
          PickList.Strings = (
            'Sandy Loam'
            'Silt'
            'Silty Clay'
            'Other')
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustRowHeights = False
          AutoAdjustCaptionRowHeights = False
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
          WordWrapCaptions = False
          WordWrapCells = False
          CaseSensitivePicklist = False
          CheckStyle = csCheck
          AutoAdjustColWidths = False
        end>
      ExplicitWidth = 681
      ExplicitHeight = 163
    end
    inherited pnlTop: TPanel
      Width = 681
      ExplicitWidth = 681
      inherited edFormula: TLabeledEdit
        Height = 26
        EditLabel.Width = 57
        EditLabel.Height = 18
        EditLabel.ExplicitTop = 9
        EditLabel.ExplicitWidth = 57
        EditLabel.ExplicitHeight = 18
        ExplicitHeight = 26
      end
    end
  end
  object comboSoilType: TComboBox
    Left = 255
    Top = 30
    Width = 145
    Height = 26
    Style = csDropDownList
    Enabled = False
    TabOrder = 1
    OnChange = comboSoilTypeChange
  end
  object rbwprsrGlobal: TRbwParser
    Left = 112
    Top = 8
  end
end
