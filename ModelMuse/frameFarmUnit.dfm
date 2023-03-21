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
    ActivePage = tabAddedDemandRunoffSplit
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
    object tabCrops: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Crop_Efficiencies'
      Caption = 'Crop Efficiencies'
      inline frameFormulaGridCrops: TframeFormulaGrid
        Left = 0
        Top = 65
        Width = 577
        Height = 267
        Align = alClient
        TabOrder = 1
        ExplicitTop = 65
        ExplicitWidth = 577
        ExplicitHeight = 267
        inherited Panel: TPanel
          Top = 226
          Width = 577
          ExplicitTop = 226
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 433
            OnClick = frameFormulaGridCropssbAddClick
            ExplicitLeft = 434
          end
          inherited sbInsert: TSpeedButton
            Left = 488
            OnClick = frameFormulaGridCropssbInsertClick
            ExplicitLeft = 489
          end
          inherited sbDelete: TSpeedButton
            Left = 543
            Width = 22
            OnClick = frameFormulaGridCropssbDeleteClick
            ExplicitLeft = 544
            ExplicitWidth = 22
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridCropsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 169
          ColCount = 3
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
          ExplicitHeight = 169
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
        end
      end
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
          Width = 79
          Height = 16
          Caption = 'Farm ID (FID)'
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
          EditLabel.Width = 66
          EditLabel.Height = 16
          EditLabel.Caption = 'Farm name'
          LabelPosition = lpRight
          TabOrder = 2
          Text = ''
          OnChange = seFarmIdChange
        end
      end
    end
    object tabEfficiencyImprovement: TTabSheet
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
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 291
          Width = 577
          ExplicitTop = 301
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameFormulaGridEfficiencyImprovementsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameFormulaGridEfficiencyImprovementsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameFormulaGridEfficiencyImprovementsbDeleteClick
            ExplicitLeft = 413
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
          ExplicitHeight = 244
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
        end
      end
    end
    object tabAddedDemandRunoffSplit: TTabSheet
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
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameAddedDemandRunoffSplitsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameAddedDemandRunoffSplitsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameAddedDemandRunoffSplitsbDeleteClick
            ExplicitLeft = 413
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
          OnSetEditText = frameFormulaGrid1GridSetEditText
          OnButtonClick = frameAddedDemandRunoffSplitGridButtonClick
          ExplicitTop = 60
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
            OnChange = frameFormulaGrid1edFormulaChange
            ExplicitHeight = 24
          end
        end
      end
    end
    object tabIrrigationUniformity: TTabSheet
      Caption = 'Irrigation Uniformity'
      ImageIndex = 9
      inline frameIrrigationUniformity: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 577
        Height = 332
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 56
        ExplicitTop = 32
        inherited Panel: TPanel
          Top = 291
          Width = 577
          inherited lbNumber: TLabel
            Width = 45
            Height = 16
            ExplicitWidth = 45
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 301
            OnClick = frameIrrigationUniformitysbAddClick
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameIrrigationUniformitysbInsertClick
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameIrrigationUniformitysbDeleteClick
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
        end
        inherited pnlTop: TPanel
          Width = 577
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
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 432
            OnClick = frameFormulaGridCostssbAddClick
            ExplicitLeft = 433
          end
          inherited sbInsert: TSpeedButton
            Left = 488
            OnClick = frameFormulaGridCostssbInsertClick
            ExplicitLeft = 489
          end
          inherited sbDelete: TSpeedButton
            Left = 543
            Width = 22
            OnClick = frameFormulaGridCostssbDeleteClick
            ExplicitLeft = 544
            ExplicitWidth = 22
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
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 290
          Height = 42
          ExplicitTop = 300
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
            ExplicitLeft = 482
          end
          inherited sbInsert: TSpeedButton
            Width = 24
            ExplicitLeft = 510
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 539
            Width = 22
            ExplicitLeft = 540
            ExplicitWidth = 22
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
          OnSetEditText = frameFormulaGridDiversionGridSetEditText
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
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
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
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 290
          Height = 42
          ExplicitTop = 300
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
            ExplicitLeft = 482
          end
          inherited sbInsert: TSpeedButton
            Width = 24
            ExplicitLeft = 510
            ExplicitWidth = 24
          end
          inherited sbDelete: TSpeedButton
            Left = 539
            Width = 22
            ExplicitLeft = 540
            ExplicitWidth = 22
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
          inherited edFormula: TLabeledEdit
            Height = 24
            EditLabel.Width = 51
            EditLabel.Height = 16
            EditLabel.Caption = 'Formula '
            EditLabel.ExplicitWidth = 51
            EditLabel.ExplicitHeight = 16
            ExplicitHeight = 24
          end
          inherited comboSfrObjects: TComboBox
            Height = 24
            ExplicitHeight = 24
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
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 282
          Width = 577
          Height = 50
          ExplicitTop = 282
          ExplicitWidth = 577
          ExplicitHeight = 50
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
            Left = 432
            Top = 10
            ExplicitLeft = 433
            ExplicitTop = 10
          end
          inherited sbInsert: TSpeedButton
            Left = 488
            Top = 10
            ExplicitLeft = 489
            ExplicitTop = 10
          end
          inherited sbDelete: TSpeedButton
            Left = 544
            Top = 10
            ExplicitLeft = 545
            ExplicitTop = 10
          end
          inherited lblNumberOfDeliveryTypes: TLabel
            Width = 82
            Height = 32
            Caption = 'Number of delivery types '
            ExplicitWidth = 82
            ExplicitHeight = 32
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
          inherited comboHowUsed: TComboBox
            Height = 24
            ExplicitHeight = 24
          end
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
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 432
            OnClick = frameFormulaGridWaterRightssbAddClick
            ExplicitLeft = 433
          end
          inherited sbInsert: TSpeedButton
            Left = 488
            OnClick = frameFormulaGridWaterRightssbInsertClick
            ExplicitLeft = 489
          end
          inherited sbDelete: TSpeedButton
            Left = 543
            Width = 22
            OnClick = frameFormulaGridWaterRightssbDeleteClick
            ExplicitLeft = 544
            ExplicitWidth = 22
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
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 290
          Width = 577
          Height = 42
          ExplicitTop = 300
          ExplicitWidth = 577
          ExplicitHeight = 42
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
          ExplicitHeight = 243
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
        end
      end
    end
  end
  object rbwprsrFarmParser: TRbwParser
    Left = 120
    Top = 112
  end
end
