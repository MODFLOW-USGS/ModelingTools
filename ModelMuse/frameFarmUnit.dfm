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
    ActivePage = tabCrops
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabHeight = 40
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
        Height = 277
        Align = alClient
        TabOrder = 1
        ExplicitTop = 65
        ExplicitWidth = 577
        ExplicitHeight = 277
        inherited Panel: TPanel
          Top = 236
          Width = 577
          ExplicitTop = 236
          ExplicitWidth = 577
          inherited lbNumber: TLabel
            Width = 95
            Height = 16
            Caption = 'Number of times'
            ExplicitWidth = 95
            ExplicitHeight = 16
          end
          inherited sbAdd: TSpeedButton
            Left = 432
            OnClick = frameFormulaGridCropssbAddClick
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 487
            OnClick = frameFormulaGridCropssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 542
            OnClick = frameFormulaGridCropssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridCropsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 179
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
          ExplicitHeight = 179
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
          EditLabel.Height = 24
          EditLabel.Caption = 'Farm name'
          LabelPosition = lpRight
          TabOrder = 2
          Text = ''
          OnChange = seFarmIdChange
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 301
          Width = 577
          ExplicitTop = 301
          ExplicitWidth = 577
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
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 487
            OnClick = frameFormulaGridCostssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 542
            OnClick = frameFormulaGridCostssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridCostsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 244
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 301
          ExplicitTop = 301
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbDelete: TSpeedButton
            ExplicitLeft = 538
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
          Height = 244
          OnSetEditText = frameFormulaGridDiversionGridSetEditText
          ExplicitHeight = 244
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
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -19
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 301
          ExplicitTop = 301
          inherited lbNumber: TLabel
            Width = 99
            Height = 16
            Caption = 'Number of times '
            ExplicitWidth = 99
            ExplicitHeight = 16
          end
          inherited sbDelete: TSpeedButton
            ExplicitLeft = 538
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
          Height = 244
          OnSetEditText = frameFormulaGridReturnFlowGridSetEditText
          ExplicitHeight = 244
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
            EditLabel.ExplicitLeft = 0
            EditLabel.ExplicitTop = -19
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 293
          Width = 577
          Height = 49
          ExplicitTop = 293
          ExplicitWidth = 577
          ExplicitHeight = 49
          inherited lbNumber: TLabel
            Top = 6
            Width = 60
            Height = 32
            Caption = 'Number of times'
            WordWrap = True
            ExplicitTop = 6
            ExplicitWidth = 60
            ExplicitHeight = 32
          end
          inherited sbAdd: TSpeedButton
            Left = 432
            Top = 10
            ExplicitLeft = 323
            ExplicitTop = 23
          end
          inherited sbInsert: TSpeedButton
            Left = 488
            Top = 10
            ExplicitLeft = 365
            ExplicitTop = 23
          end
          inherited sbDelete: TSpeedButton
            Left = 543
            Top = 10
            ExplicitLeft = 407
            ExplicitTop = 23
          end
          inherited lblNumberOfDeliveryTypes: TLabel
            Width = 112
            Height = 32
            Caption = 'Number of delivery types '
            ExplicitWidth = 112
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
          Height = 236
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
          ExplicitHeight = 236
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 301
          Width = 577
          ExplicitTop = 301
          ExplicitWidth = 577
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
            ExplicitLeft = 323
          end
          inherited sbInsert: TSpeedButton
            Left = 487
            OnClick = frameFormulaGridWaterRightssbInsertClick
            ExplicitLeft = 364
          end
          inherited sbDelete: TSpeedButton
            Left = 542
            OnClick = frameFormulaGridWaterRightssbDeleteClick
            ExplicitLeft = 406
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameFormulaGridWaterRightsseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 244
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
          ExplicitHeight = 244
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
        Height = 342
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 577
        ExplicitHeight = 342
        inherited Panel: TPanel
          Top = 301
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
            OnClick = frameGW_AllocationsbAddClick
            ExplicitLeft = 301
          end
          inherited sbInsert: TSpeedButton
            Left = 357
            OnClick = frameGW_AllocationsbInsertClick
            ExplicitLeft = 357
          end
          inherited sbDelete: TSpeedButton
            Left = 413
            OnClick = frameGW_AllocationsbDeleteClick
            ExplicitLeft = 413
          end
          inherited seNumber: TJvSpinEdit
            Height = 24
            OnChange = frameGW_AllocationseNumberChange
            ExplicitHeight = 24
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 577
          Height = 244
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
