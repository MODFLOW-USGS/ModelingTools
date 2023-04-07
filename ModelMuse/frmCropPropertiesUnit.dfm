inherited frmCropProperties: TfrmCropProperties
  HelpType = htKeyword
  HelpKeyword = 'Farm_Crop_Properties_Dialog_Bo'
  Caption = 'Farm Crop Properties'
  ClientHeight = 521
  ClientWidth = 744
  ExplicitWidth = 756
  ExplicitHeight = 559
  TextHeight = 18
  object splitterMain: TJvNetscapeSplitter
    Left = 225
    Top = 0
    Height = 477
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 280
    ExplicitTop = 136
    ExplicitHeight = 100
  end
  object jvpltvMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 225
    Height = 477
    ShowButtons = True
    PageDefault = 0
    PageList = jplMain
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnChange = jvpltvMainChange
    OnCustomDrawItem = jvpltvMainCustomDrawItem
    Items.Links = {00000000}
    ExplicitHeight = 476
  end
  object jplMain: TJvPageList
    Left = 235
    Top = 0
    Width = 509
    Height = 477
    ActivePage = jvspGwRootInteraction
    PropagateEnable = False
    Align = alClient
    OnChange = jplMainChange
    ExplicitWidth = 505
    ExplicitHeight = 476
    object jvspCropName: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      HelpType = htKeyword
      HelpKeyword = 'Crops_Pane'
      Caption = 'jvspCropName'
      inline frameCropName: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Top = 6
            Width = 115
            Height = 18
            Caption = 'Number of crops'
            ExplicitTop = 6
            ExplicitWidth = 115
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 259
            OnClick = frameCropNamesbAddClick
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            OnClick = frameCropNamesbInsertClick
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 354
            OnClick = frameCropNamesbDeleteClick
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameCropNameseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = frameCropNameGridSetEditText
          OnBeforeDrawCell = frameCropNameGridBeforeDrawCell
          OnButtonClick = GridButtonClick
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspEvapFractions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      HelpType = htKeyword
      HelpKeyword = 'Consumptive_Use_Factors_Pane'
      Caption = 'jvspEvapFractions'
      inline frameEvapFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 259
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 354
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameEvapFractionsGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspLosses: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      HelpType = htKeyword
      HelpKeyword = 'Inefficiency_Losses_to_Surface'
      Caption = 'jvspLosses'
      inline frameLosses: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 259
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 354
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameLossesGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspCropFunction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      HelpType = htKeyword
      HelpKeyword = 'Crop_Price_Function_Pane'
      Caption = 'jvspCropFunction'
      inline frameCropFunction: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 259
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 354
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameCropFunctionGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspCropWaterUse: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      HelpType = htKeyword
      HelpKeyword = 'Consumptive_Use_Flux_or_Crop_C'
      Caption = 'jvspCropWaterUse'
      inline frameCropWaterUse: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 259
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 354
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = GridSetEditText
          OnBeforeDrawCell = frameCropWaterUseGridBeforeDrawCell
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameCropWaterUseGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspIrrigation: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      Caption = 'jvspIrrigation'
      inline frameIrrigation: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 257
            ExplicitLeft = 232
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            ExplicitLeft = 275
          end
          inherited sbDelete: TSpeedButton
            Left = 351
            ExplicitLeft = 318
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameIrrigationGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspLandUseFraction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      Caption = 'jvspLandUseFraction'
      inline frameLandUseFraction: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 261
            ExplicitLeft = 265
          end
          inherited sbInsert: TSpeedButton
            Left = 309
            ExplicitLeft = 314
          end
          inherited sbDelete: TSpeedButton
            Left = 356
            ExplicitLeft = 362
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          ColCount = 3
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameLandUseFractionGridEndUpdate
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
            end>
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspRootPressure: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      Caption = 'jvspRootPressure'
      inline frameRootPressure: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 509
        ExplicitHeight = 477
        inherited Panel: TPanel
          Top = 436
          Width = 509
          ExplicitTop = 436
          ExplicitWidth = 509
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 263
            ExplicitLeft = 265
          end
          inherited sbInsert: TSpeedButton
            Left = 311
            ExplicitLeft = 314
          end
          inherited sbDelete: TSpeedButton
            Left = 359
            ExplicitLeft = 362
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 509
          Height = 379
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameRootPressureGridEndUpdate
          ExplicitWidth = 509
          ExplicitHeight = 379
        end
        inherited pnlTop: TPanel
          Width = 509
          ExplicitWidth = 509
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
    end
    object jvspGwRootInteraction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 509
      Height = 477
      Caption = 'jvspGwRootInteraction'
      OnShow = jvspGwRootInteractionShow
      ExplicitWidth = 505
      ExplicitHeight = 476
      object rdgGwRootInteraction: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 509
        Height = 477
        Align = alClient
        ColCount = 1
        DefaultColWidth = 500
        FixedCols = 0
        RowCount = 4
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnExit = rdgGwRootInteractionExit
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgGwRootInteractionStateChange
        ColorRangeSelection = False
        ColorSelectedRow = False
        Columns = <
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
            Format = rcf4Boolean
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
        ExplicitLeft = -4
        ExplicitTop = 129
        ExplicitHeight = 348
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 477
    Width = 744
    Height = 44
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 476
    ExplicitWidth = 740
    object btnCancel: TBitBtn
      Left = 502
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 405
      Top = 6
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 308
      Top = 6
      Width = 91
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object rbwprsrGlobal: TRbwParser
    Left = 112
    Top = 8
  end
end
