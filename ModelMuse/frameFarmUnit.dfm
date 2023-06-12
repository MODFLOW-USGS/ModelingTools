inherited frameFarm: TframeFarm
  Width = 585
  Height = 392
  ExplicitWidth = 585
  ExplicitHeight = 392
  object pcMain: TJvgPageControl
    Left = 0
    Top = 0
    Width = 585
    Height = 392
    ActivePage = tabNonRoutedDelivery
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabHeight = 50
    TabOrder = 0
    TabStop = False
    TabWidth = 80
    TabStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
    TabStyle.BevelInner = bvNone
    TabStyle.BevelOuter = bvNone
    TabStyle.Bold = False
    TabStyle.BackgrColor = clBtnFace
    TabStyle.Font.Charset = DEFAULT_CHARSET
    TabStyle.Font.Color = clWindowText
    TabStyle.Font.Height = -13
    TabStyle.Font.Name = 'Tahoma'
    TabStyle.Font.Style = []
    TabStyle.CaptionHAlign = fhaCenter
    TabStyle.Gradient.Active = False
    TabStyle.Gradient.Orientation = fgdHorizontal
    TabSelectedStyle.Borders = [fsdLeft, fsdTop, fsdRight, fsdBottom]
    TabSelectedStyle.BevelInner = bvNone
    TabSelectedStyle.BevelOuter = bvNone
    TabSelectedStyle.Bold = False
    TabSelectedStyle.BackgrColor = clBtnFace
    TabSelectedStyle.Font.Charset = DEFAULT_CHARSET
    TabSelectedStyle.Font.Color = clWindowText
    TabSelectedStyle.Font.Height = -13
    TabSelectedStyle.Font.Name = 'Tahoma'
    TabSelectedStyle.Font.Style = []
    TabSelectedStyle.CaptionHAlign = fhaCenter
    TabSelectedStyle.Gradient.Active = False
    TabSelectedStyle.Gradient.Orientation = fgdHorizontal
    Options = [ftoAutoFontDirection, ftoExcludeGlyphs, ftoInheriteTabFonts, ftoWordWrap]
    object tabName: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Name'
      Caption = 'Name'
      ImageIndex = 20
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 577
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblFarmId: TLabel
          Left = 130
          Top = 34
          Width = 12
          Height = 16
          Caption = 'ID'
        end
        object seFarmId: TJvSpinEdit
          Left = 3
          Top = 31
          Width = 121
          Height = 24
          MaxValue = 2147483647.000000000000000000
          TabOrder = 1
          OnChange = seFarmIdChange
        end
        object pnlCaption: TPanel
          Left = 0
          Top = 0
          Width = 577
          Height = 25
          Align = alTop
          BevelInner = bvRaised
          BevelOuter = bvNone
          TabOrder = 0
        end
        object edFarmName: TLabeledEdit
          Left = 256
          Top = 31
          Width = 185
          Height = 24
          EditLabel.Width = 63
          EditLabel.Height = 16
          EditLabel.Caption = 'WBS name'
          LabelPosition = lpRight
          TabOrder = 2
          Text = ''
          OnChange = seFarmIdChange
        end
      end
      object PanelOwhm2: TPanel
        Left = 0
        Top = 65
        Width = 577
        Height = 267
        Align = alClient
        TabOrder = 1
        object lblPumpSpread: TLabel
          Left = 261
          Top = 13
          Width = 290
          Height = 32
          Caption = 
            'Specify how pumping demand should be allocated among MNW well no' +
            'des (MNW_PUMP_SPREAD)'
          Constraints.MaxWidth = 310
          WordWrap = True
        end
        object comboPumpSpread: TComboBox
          Left = 3
          Top = 10
          Width = 233
          Height = 24
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'By node conductance (0)'
            'Evenly among nodes (1)'
            'Assign to top node (2)')
        end
      end
    end
    object tabCrops: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Crop_Efficiencies'
      Caption = 'Crop Efficiencies'
      inline frameFormulaGridCrops: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 433
            Width = 24
            OnClick = frameFormulaGridCropssbAddClick
            ExplicitLeft = 434
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Left = 489
            Width = 24
            OnClick = frameFormulaGridCropssbInsertClick
            ExplicitLeft = 490
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 545
            Width = 20
            OnClick = frameFormulaGridCropssbDeleteClick
            ExplicitLeft = 546
            ExplicitWidth = 20
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridCropsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          ColCount = 3
          OnSelectCell = frameFormulaGridCropsGridSelectCell
          OnSetEditText = frameFormulaGridCropsGridSetEditText
          OnButtonClick = frameFormulaGridCropsGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
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
            end>
          ExplicitWidth = 577
          ExplicitHeight = 234
          RowHeights = (
            27
            27)
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridCropsedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabEfficiencyImprovement: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Efficiency-Improvement'
      Caption = 'Efficiency Improvement'
      ImageIndex = 7
      inline frameFormulaGridEfficiencyImprovement: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameFormulaGridEfficiencyImprovementsbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameFormulaGridEfficiencyImprovementsbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameFormulaGridEfficiencyImprovementsbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridEfficiencyImprovementseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          ColCount = 3
          OnSetEditText = frameFormulaGridEfficiencyImprovementGridSetEditText
          OnButtonClick = frameFormulaGridEfficiencyImprovementGridButtonClick
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
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = False
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
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridEfficiencyImprovementedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabDeficiencyScenario: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Deficiency-Scenario'
      Caption = 'Deficiency Scenario'
      ImageIndex = 10
      inline frameDeficiencyScenario: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameDeficiencyScenariosbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameDeficiencyScenariosbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameDeficiencyScenariosbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameDeficiencyScenarioseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameDeficiencyScenarioGridSetEditText
          OnButtonClick = frameDeficiencyScenarioGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameDeficiencyScenarioedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabWaterSource: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Water-Source'
      Caption = 'Water Source'
      ImageIndex = 11
      inline frameWaterSource: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameWaterSourcesbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameWaterSourcesbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameWaterSourcesbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameWaterSourceseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameWaterSourceGridSetEditText
          OnButtonClick = frameWaterSourceGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameWaterSourceedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabBareRunoffFractions: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Bare-Runoff-Fractions'
      Caption = 'Bare Runoff Fractions'
      ImageIndex = 12
      inline frameBareRunoffFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameBareRunoffFractionssbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameBareRunoffFractionssbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameBareRunoffFractionssbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameBareRunoffFractionsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameBareRunoffFractionsGridSetEditText
          OnButtonClick = frameBareRunoffFractionsGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameBareRunoffFractionsedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabAddedDemandRunoffSplit: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Added-Demand-Runoff-Split'
      Caption = 'Added Demand Runoff Split'
      ImageIndex = 8
      inline frameAddedDemandRunoffSplit: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameAddedDemandRunoffSplitsbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameAddedDemandRunoffSplitsbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameAddedDemandRunoffSplitsbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameAddedDemandRunoffSplitseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameAddedDemandRunoffSplitGridSetEditText
          OnButtonClick = frameAddedDemandRunoffSplitGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabAddedCropDemandFlux: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Added-Crop-Demand-Flux'
      Caption = 'Added Crop Demand Flux'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 13
      ParentFont = False
      inline frameAddedCropDemandFlux: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameAddedCropDemandFluxsbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameAddedCropDemandFluxsbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameAddedCropDemandFluxsbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameAddedCropDemandFluxseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameAddedCropDemandFluxGridSetEditText
          OnButtonClick = frameAddedCropDemandFluxGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameAddedCropDemandFluxedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabAddedCropDemandRate: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Added-Crop-Demand-Rate'
      Caption = 'Added Crop Demand Rate'
      ImageIndex = 14
      inline frameAddedCropDemandRate: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameAddedCropDemandRatesbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameAddedCropDemandRatesbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameAddedCropDemandRatesbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameAddedCropDemandRateseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameAddedCropDemandRateGridSetEditText
          OnButtonClick = frameAddedCropDemandRateGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameAddedCropDemandRateedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabCosts: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Costs'
      Caption = 'Costs'
      ImageIndex = 5
      inline frameFormulaGridCosts: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 290
          Width = 577
          Height = 42
          ExplicitTop = 290
          ExplicitWidth = 577
          ExplicitHeight = 42
          DesignSize = (
            577
            42)
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 433
            Width = 24
            OnClick = frameFormulaGridCostssbAddClick
            ExplicitLeft = 434
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Left = 489
            Width = 24
            OnClick = frameFormulaGridCostssbInsertClick
            ExplicitLeft = 490
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 545
            Width = 20
            OnClick = frameFormulaGridCostssbDeleteClick
            ExplicitLeft = 546
            ExplicitWidth = 20
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridCostsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 233
          ColCount = 10
          OnSetEditText = frameFormulaGridCostsGridSetEditText
          OnButtonClick = frameFormulaGridCostsGridButtonClick
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
          ExplicitWidth = 577
          ExplicitHeight = 233
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridCostsedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabNonRoutedDelivery: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Non_Routed_Delivery'
      Caption = 'Non-Routed'#13#10'Delivery'
      ImageIndex = 4
      inline frameDelivery: TframeDeliveryGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 282
          Width = 577
          Height = 50
          ExplicitTop = 282
          ExplicitWidth = 577
          ExplicitHeight = 50
          DesignSize = (
            577
            50)
          inherited lbNumber: TLabel
            Top = 6
            Width = 49
            Height = 32
            Caption = 'Number of times'
            WordWrap = True
            ExplicitTop = 6
            ExplicitWidth = 49
            ExplicitHeight = 32
          end
          inherited sbAdd: TSpeedButton
            Left = 433
            Top = 10
            Width = 24
            ExplicitLeft = 434
            ExplicitTop = 10
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Left = 489
            Top = 10
            Width = 24
            ExplicitLeft = 490
            ExplicitTop = 10
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 545
            Top = 10
            Width = 24
            ExplicitLeft = 546
            ExplicitTop = 10
            ExplicitWidth = 24
          end
          inherited lblNumberOfDeliveryTypes: TLabel
            Width = 49
            Height = 64
            Caption = 'Number of delivery types '
            ExplicitWidth = 49
            ExplicitHeight = 64
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
          inherited seNumberOfDeliveryTypes: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 225
          OnSetEditText = frameDeliveryGridSetEditText
          OnButtonClick = frameDeliveryGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
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
            end>
          ExplicitWidth = 577
          ExplicitHeight = 225
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited lblHowUsed: TLabel
            Width = 56
            Height = 16
            ExplicitWidth = 56
            ExplicitHeight = 16
          end
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboHowUsed: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabDiversionLocation: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Diversion_Location'
      Caption = 'Diversion'#13#10'Location'
      ImageIndex = 2
      inline frameFormulaGridDiversion: TframeFarmDiversion
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 290
          Height = 42
          ExplicitTop = 290
          ExplicitHeight = 42
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 481
            Width = 24
            ExplicitLeft = 482
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Width = 24
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 541
            Width = 20
            ExplicitLeft = 542
            ExplicitWidth = 20
          end
          inherited lblLocationMethod: TLabel
            Left = 208
            Width = 94
            Height = 16
            ExplicitLeft = 208
            ExplicitWidth = 94
            ExplicitHeight = 16
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboMethod: TComboBox
            Left = 336
            Height = 24
            ExplicitLeft = 336
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Height = 233
          OnSetEditText = frameFormulaGridDiversionGridSetEditText
          OnButtonClick = frameFormulaGridDiversionGridButtonClick
          ExplicitHeight = 233
        end
        inherited pnlTop: TPanel
          inherited lblSfrObjects: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblPositionChoice: TLabel
            Width = 84
            Height = 16
            ExplicitWidth = 84
            ExplicitHeight = 16
          end
          inherited lblVertexNumber: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblX: TLabel
            Width = 8
            Height = 16
            ExplicitWidth = 8
            ExplicitHeight = 16
          end
          inherited lblY: TLabel
            Height = 16
            ExplicitHeight = 16
          end
          inherited lblRow: TLabel
            Width = 25
            Height = 16
            ExplicitWidth = 25
            ExplicitHeight = 16
          end
          inherited lblCol: TLabel
            Height = 16
            ExplicitHeight = 16
          end
          inherited comboPositionChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeVertexNumber: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeX: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeY: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeRow: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeCol: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitTop = 8
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabReturnFlowLocation: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Return_Flow_Location'
      Caption = 'Return Flow'#13#10'Location'
      ImageIndex = 3
      inline frameFormulaGridReturnFlow: TframeFarmDiversion
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 290
          Height = 42
          ExplicitTop = 290
          ExplicitHeight = 42
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 481
            Width = 24
            ExplicitLeft = 482
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Width = 24
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 541
            Width = 20
            ExplicitLeft = 542
            ExplicitWidth = 20
          end
          inherited lblLocationMethod: TLabel
            Width = 94
            Height = 16
            ExplicitWidth = 94
            ExplicitHeight = 16
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboMethod: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Height = 233
          OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          OnButtonClick = frameFormulaGridReturnFlowGridButtonClick
          ExplicitHeight = 233
          ColWidths = (
            64
            64
            64
            64
            64)
        end
        inherited pnlTop: TPanel
          inherited lblSfrObjects: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblPositionChoice: TLabel
            Width = 84
            Height = 16
            ExplicitWidth = 84
            ExplicitHeight = 16
          end
          inherited lblVertexNumber: TLabel
            Width = 37
            Height = 16
            ExplicitWidth = 37
            ExplicitHeight = 16
          end
          inherited lblX: TLabel
            Width = 8
            Height = 16
            ExplicitWidth = 8
            ExplicitHeight = 16
          end
          inherited lblY: TLabel
            Height = 16
            ExplicitHeight = 16
          end
          inherited lblRow: TLabel
            Width = 25
            Height = 16
            ExplicitWidth = 25
            ExplicitHeight = 16
          end
          inherited lblCol: TLabel
            Height = 16
            ExplicitHeight = 16
          end
          inherited comboPositionChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeVertexNumber: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeX: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeY: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeRow: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited rdeCol: TRbwDataEntry
            Height = 24
            ExplicitHeight = 24
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitTop = 8
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabDiversionsOwhm2: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Diversion-Locations'
      Caption = 'Diversion Locations'
      ImageIndex = 16
      inline frameDiversionsOwhm2: TframeMultSemiRouted
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Splitter1: TSplitter
          Height = 291
          ExplicitHeight = 291
        end
        inherited pnlBottom: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 480
            ExplicitLeft = 480
          end
          inherited sbDelete: TSpeedButton
            Left = 538
            ExplicitLeft = 538
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
        end
        inherited Panel1: TPanel
          Width = 451
          Height = 291
          ExplicitWidth = 451
          ExplicitHeight = 291
          inherited pnlName: TPanel
            Width = 449
            ExplicitWidth = 449
            inherited lblSemiRouteName: TLabel
              Width = 33
              Height = 16
              ExplicitWidth = 33
              ExplicitHeight = 16
            end
            inherited edSemiRouteName: TEdit
              Height = 24
              ExplicitHeight = 24
            end
          end
          inherited frameFarmDiversions: TframeFarmDiversion
            Width = 449
            Height = 248
            ExplicitWidth = 449
            ExplicitHeight = 248
            inherited Panel: TPanel
              Top = 207
              Width = 449
              ExplicitTop = 207
              ExplicitWidth = 449
              inherited lbNumber: TLabel
                Width = 45
                Height = 16
                ExplicitWidth = 45
                ExplicitHeight = 16
              end
              inherited lblLocationMethod: TLabel
                Width = 94
                Height = 16
                ExplicitWidth = 94
                ExplicitHeight = 16
              end
              inherited seNumber: TJvSpinEdit
                Height = 24
                ExplicitHeight = 24
              end
              inherited comboMethod: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 449
              Height = 150
              OnButtonClick = frameFarmDiversionsGridButtonClick
              ExplicitWidth = 449
              ExplicitHeight = 150
            end
            inherited pnlTop: TPanel
              Width = 449
              ExplicitWidth = 449
              inherited lblSfrObjects: TLabel
                Width = 37
                Height = 16
                ExplicitWidth = 37
                ExplicitHeight = 16
              end
              inherited lblPositionChoice: TLabel
                Width = 84
                Height = 16
                ExplicitWidth = 84
                ExplicitHeight = 16
              end
              inherited lblVertexNumber: TLabel
                Width = 37
                Height = 16
                ExplicitWidth = 37
                ExplicitHeight = 16
              end
              inherited lblX: TLabel
                Width = 8
                Height = 16
                ExplicitWidth = 8
                ExplicitHeight = 16
              end
              inherited lblY: TLabel
                Height = 16
                ExplicitHeight = 16
              end
              inherited lblRow: TLabel
                Width = 25
                Height = 16
                ExplicitWidth = 25
                ExplicitHeight = 16
              end
              inherited lblCol: TLabel
                Height = 16
                ExplicitHeight = 16
              end
              inherited comboPositionChoice: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
              inherited comboSfrObjects: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
              inherited edFormula: TLabeledEdit
                Height = 24
                EditLabel.Width = 47
                EditLabel.Height = 16
                EditLabel.ExplicitLeft = 8
                EditLabel.ExplicitTop = 8
                EditLabel.ExplicitWidth = 47
                EditLabel.ExplicitHeight = 16
                ExplicitHeight = 24
              end
              inherited comboChoice: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
            end
          end
        end
        inherited tvSRCollections: TTreeView
          Height = 291
          ExplicitHeight = 291
        end
        inherited Controller: TRbwController
          ControlList = <
            item
              Control = frameDiversionsOwhm2.seNumber
            end
            item
              Control = frameDiversionsOwhm2.sbAdd
            end>
        end
      end
    end
    object tabNoReturnFlow: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Return-Flow-Choice'
      Caption = 'Return Flow Choice'
      ImageIndex = 15
      inline frameNoReturnFlow: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameNoReturnFlowsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameNoReturnFlowsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 412
            OnClick = frameNoReturnFlowsbDeleteClick
            ExplicitLeft = 412
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameNoReturnFlowseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          ColCount = 3
          OnSetEditText = frameNoReturnFlowGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
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
              ButtonFont.Height = -12
              ButtonFont.Name = 'Segoe UI'
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
                'Return flow allowed'
                'Only deep percolation')
              WordWrapCaptions = False
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            Visible = False
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            Visible = True
            OnChange = frameNoReturnFlowcomboChoiceChange
            Items.Strings = (
              'Return flow allowed'
              'Only deep percolation')
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabReturnFlowOwhm2: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Return-Flow-Locations_'
      Caption = 'Return Flow Locations'
      ImageIndex = 17
      inline frameReturnFlowsOwhm2: TframeMultSemiRouted
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Splitter1: TSplitter
          Height = 291
          ExplicitHeight = 291
        end
        inherited pnlBottom: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 476
            ExplicitLeft = 476
          end
          inherited sbDelete: TSpeedButton
            Left = 538
            ExplicitLeft = 538
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            ExplicitHeight = 24
          end
        end
        inherited Panel1: TPanel
          Width = 451
          Height = 291
          ExplicitWidth = 451
          ExplicitHeight = 291
          inherited pnlName: TPanel
            Width = 449
            ExplicitWidth = 449
            inherited lblSemiRouteName: TLabel
              Width = 33
              Height = 16
              ExplicitWidth = 33
              ExplicitHeight = 16
            end
            inherited edSemiRouteName: TEdit
              Height = 24
              ExplicitHeight = 24
            end
          end
          inherited frameFarmDiversions: TframeFarmDiversion
            Width = 449
            Height = 248
            ExplicitWidth = 449
            ExplicitHeight = 248
            inherited Panel: TPanel
              Top = 207
              Width = 449
              ExplicitTop = 207
              ExplicitWidth = 449
              inherited lbNumber: TLabel
                Width = 45
                Height = 16
                ExplicitWidth = 45
                ExplicitHeight = 16
              end
              inherited sbAdd: TSpeedButton
                Left = 278
                ExplicitLeft = 278
              end
              inherited sbInsert: TSpeedButton
                Left = 294
                ExplicitLeft = 294
              end
              inherited sbDelete: TSpeedButton
                Left = 310
                ExplicitLeft = 310
              end
              inherited lblLocationMethod: TLabel
                Width = 94
                Height = 16
                ExplicitWidth = 94
                ExplicitHeight = 16
              end
              inherited seNumber: TJvSpinEdit
                Height = 24
                ExplicitHeight = 24
              end
              inherited comboMethod: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 449
              Height = 150
              OnButtonClick = frameFarmDiversionsGridButtonClick
              ExplicitWidth = 449
              ExplicitHeight = 150
            end
            inherited pnlTop: TPanel
              Width = 449
              ExplicitWidth = 449
              inherited lblSfrObjects: TLabel
                Width = 37
                Height = 16
                ExplicitWidth = 37
                ExplicitHeight = 16
              end
              inherited lblPositionChoice: TLabel
                Width = 84
                Height = 16
                ExplicitWidth = 84
                ExplicitHeight = 16
              end
              inherited lblVertexNumber: TLabel
                Width = 37
                Height = 16
                ExplicitWidth = 37
                ExplicitHeight = 16
              end
              inherited lblX: TLabel
                Width = 8
                Height = 16
                ExplicitWidth = 8
                ExplicitHeight = 16
              end
              inherited lblY: TLabel
                Height = 16
                ExplicitHeight = 16
              end
              inherited lblRow: TLabel
                Width = 25
                Height = 16
                ExplicitWidth = 25
                ExplicitHeight = 16
              end
              inherited lblCol: TLabel
                Height = 16
                ExplicitHeight = 16
              end
              inherited comboPositionChoice: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
              inherited comboSfrObjects: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
              inherited edFormula: TLabeledEdit
                Height = 24
                EditLabel.Width = 47
                EditLabel.Height = 16
                EditLabel.ExplicitLeft = 8
                EditLabel.ExplicitTop = 8
                EditLabel.ExplicitWidth = 47
                EditLabel.ExplicitHeight = 16
                ExplicitHeight = 24
              end
              inherited comboChoice: TComboBox
                Height = 24
                ExplicitHeight = 24
              end
            end
          end
        end
        inherited tvSRCollections: TTreeView
          Height = 291
          ExplicitHeight = 291
        end
        inherited Controller: TRbwController
          ControlList = <
            item
              Control = frameReturnFlowsOwhm2.seNumber
            end
            item
              Control = frameReturnFlowsOwhm2.sbAdd
            end>
        end
      end
    end
    object tabWaterRights: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Water_Rights'
      Caption = 'Water'#13#10'Rights'
      ImageIndex = 5
      inline frameFormulaGridWaterRights: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 290
          Width = 577
          Height = 42
          ExplicitTop = 290
          ExplicitWidth = 577
          ExplicitHeight = 42
          DesignSize = (
            577
            42)
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 433
            Width = 24
            OnClick = frameFormulaGridWaterRightssbAddClick
            ExplicitLeft = 434
            ExplicitWidth = 24
          end
          inherited sbInsert: TSpeedButton
            Left = 489
            Width = 24
            OnClick = frameFormulaGridWaterRightssbInsertClick
            ExplicitLeft = 490
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 545
            Width = 20
            OnClick = frameFormulaGridWaterRightssbDeleteClick
            ExplicitLeft = 546
            ExplicitWidth = 20
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridWaterRightsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 233
          ColCount = 3
          OnSetEditText = frameFormulaGridWaterRightsGridSetEditText
          OnButtonClick = frameFormulaGridWaterRightsGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
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
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -13
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
          ExplicitWidth = 577
          ExplicitHeight = 233
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            OnChange = frameFormulaGridWaterRightsedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabSwAllotment: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'SW-Allotment'
      Caption = 'SW Allotment'
      ImageIndex = 18
      inline frameSwAllotment: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameSwAllotmentsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameSwAllotmentsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 412
            OnClick = frameSwAllotmentsbDeleteClick
            ExplicitLeft = 412
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameSwAllotmentseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          ColCount = 3
          OnSetEditText = frameSwAllotmentGridSetEditText
          OnButtonClick = frameSwAllotmentGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = True
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
              AutoAdjustRowHeights = True
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
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameSwAllotmentedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabGW_Allocation: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'GW_Allocation_Tab'
      Caption = 'GW Allocation'
      ImageIndex = 6
      inline frameGW_Allocation: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 290
          Width = 577
          Height = 42
          ExplicitTop = 290
          ExplicitWidth = 577
          ExplicitHeight = 42
          DesignSize = (
            577
            42)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameGW_AllocationsbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameGW_AllocationsbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameGW_AllocationsbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameGW_AllocationseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 233
          ColCount = 3
          OnSetEditText = frameGW_AllocationGridSetEditText
          OnButtonClick = frameGW_AllocationGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
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
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 577
          ExplicitHeight = 233
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameGW_AllocationedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabWaterSupplyConcentration: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Water-Supply-Concentration'
      Caption = 'Water Supply Concentration'
      ImageIndex = 19
      inline frameWaterSupplyConcentration: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameWaterSupplyConcentrationsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameWaterSupplyConcentrationsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 412
            OnClick = frameWaterSupplyConcentrationsbDeleteClick
            ExplicitLeft = 412
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameWaterSupplyConcentrationseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          ColCount = 6
          OnSetEditText = frameWaterSupplyConcentrationGridSetEditText
          OnButtonClick = frameWaterSupplyConcentrationGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = False
              AutoAdjustCaptionRowHeights = True
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
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameWaterSupplyConcentrationedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabIrrigationUniformity: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Irrigation-Uniformity'
      Caption = 'Irrigation Uniformity'
      ImageIndex = 9
      inline frameIrrigationUniformity: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 332
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 291
          ExplicitWidth = 577
          DesignSize = (
            577
            41)
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameIrrigationUniformitysbAddClick
            ExplicitLeft = 302
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameIrrigationUniformitysbInsertClick
            ExplicitLeft = 358
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameIrrigationUniformitysbDeleteClick
            ExplicitLeft = 414
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameIrrigationUniformityseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 234
          OnSetEditText = frameIrrigationUniformityGridSetEditText
          OnButtonClick = frameIrrigationUniformityGridButtonClick
          ExplicitWidth = 577
          ExplicitHeight = 234
        end
        inherited pnlTop: TPanel
          Width = 577
          ExplicitWidth = 577
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 47
            EditLabel.Height = 16
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 11
            EditLabel.ExplicitWidth = 47
            EditLabel.ExplicitHeight = 16
            OnChange = frameIrrigationUniformityedFormulaChange
            ExplicitHeight = 24
          end
          inherited comboChoice: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
        end
      end
    end
  end
  object rbwprsrFarmParser: TRbwParser
    Left = 120
    Top = 112
  end
end
