inherited frmSoilProperties: TfrmSoilProperties
  HelpType = htKeyword
  HelpKeyword = 'Farm_Soil_Properties_Dialog_Bo'
  Caption = 'Farm Soil Properties'
  ClientHeight = 562
  ClientWidth = 788
  OnResize = FormResize
  ExplicitWidth = 804
  ExplicitHeight = 601
  TextHeight = 18
  object splitterSoil: TSplitter
    Left = 0
    Top = 256
    Width = 788
    Height = 5
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 200
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 518
    Width = 788
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
    Width = 788
    Height = 256
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 788
    ExplicitHeight = 256
    inherited Panel: TPanel
      Top = 215
      Width = 788
      ExplicitTop = 215
      ExplicitWidth = 788
      inherited lbNumber: TLabel
        Width = 108
        Height = 18
        Caption = 'Number of soils'
        ExplicitWidth = 108
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 399
        ExplicitLeft = 316
      end
      inherited sbInsert: TSpeedButton
        Left = 471
        ExplicitLeft = 374
      end
      inherited sbDelete: TSpeedButton
        Left = 542
        ExplicitLeft = 432
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        OnChange = frameSoilsseNumberChange
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 788
      Height = 158
      ColCount = 10
      FixedCols = 1
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
      OnMouseUp = frameSoilsGridMouseUp
      OnRowMoved = frameSoilsGridRowMoved
      OnSelectCell = frameSoilsGridSelectCell
      OnSetEditText = frameSoilsGridSetEditText
      OnBeforeDrawCell = frameSoilsGridBeforeDrawCell
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
          AutoAdjustRowHeights = True
          AutoAdjustCaptionRowHeights = False
          ButtonCaption = 'F()'
          ButtonFont.Charset = ANSI_CHARSET
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
            'Sand'
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
      ExplicitWidth = 788
      ExplicitHeight = 158
    end
    inherited pnlTop: TPanel
      Width = 788
      ExplicitWidth = 788
      inherited edFormula: TLabeledEdit
        Height = 26
        EditLabel.Width = 57
        EditLabel.Height = 18
        EditLabel.ExplicitLeft = 128
        EditLabel.ExplicitTop = 9
        EditLabel.ExplicitHeight = 18
        ExplicitHeight = 26
      end
      inherited comboChoice: TComboBox
        Height = 26
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
  inline frameSoilEffectivePrecip: TframeSoilEffectivePrecip
    Left = 0
    Top = 261
    Width = 788
    Height = 257
    Align = alClient
    Enabled = False
    TabOrder = 3
    ExplicitTop = 261
    ExplicitWidth = 788
    ExplicitHeight = 257
    inherited Panel: TPanel
      Top = 216
      Width = 788
      ExplicitTop = 216
      ExplicitWidth = 788
      DesignSize = (
        788
        41)
      inherited lbNumber: TLabel
        Width = 163
        Height = 18
        Caption = 'Number of rows in table'
        ExplicitWidth = 163
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 612
        ExplicitLeft = 630
      end
      inherited sbInsert: TSpeedButton
        Left = 653
        ExplicitLeft = 677
      end
      inherited sbDelete: TSpeedButton
        Left = 707
        ExplicitLeft = 725
      end
      inherited lblMethod: TLabel
        Left = 430
        Top = 9
        Width = 52
        Height = 18
        ExplicitLeft = 430
        ExplicitTop = 9
        ExplicitWidth = 52
        ExplicitHeight = 18
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
      inherited comboInterpolation: TComboBox
        Left = 296
        Height = 26
        ExplicitLeft = 296
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 788
      Height = 135
      OnExit = frameSoilEffectivePrecipGridExit
      OnButtonClick = frameSoilsGridButtonClick
      ExplicitTop = 84
      ExplicitWidth = 788
      ExplicitHeight = 135
    end
    inherited pnlTop: TPanel
      Width = 788
      ExplicitWidth = 788
      inherited lblSoil: TLabel
        Width = 233
        Height = 18
        Caption = 'Effective Precipitation Table - Soil'
        ExplicitWidth = 233
        ExplicitHeight = 18
      end
      inherited edFormula: TLabeledEdit
        Height = 26
        EditLabel.Width = 57
        EditLabel.Height = 18
        EditLabel.ExplicitTop = 31
        EditLabel.ExplicitWidth = 57
        EditLabel.ExplicitHeight = 18
        ExplicitHeight = 26
      end
      inherited comboChoice: TComboBox
        Height = 26
        ExplicitHeight = 26
      end
    end
  end
  object rbwprsrGlobal: TRbwParser
    Left = 112
    Top = 8
  end
end
