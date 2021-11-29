inherited frmCropProperties: TfrmCropProperties
  HelpType = htKeyword
  HelpKeyword = 'Farm_Crop_Properties_Dialog_Bo'
  Caption = 'Farm Crop Properties'
  ClientHeight = 522
  ClientWidth = 683
  ExplicitWidth = 699
  ExplicitHeight = 561
  PixelsPerInch = 96
  TextHeight = 18
  object splitterMain: TJvNetscapeSplitter
    Left = 225
    Top = 0
    Height = 478
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
    Height = 478
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
  end
  object jplMain: TJvPageList
    Left = 235
    Top = 0
    Width = 448
    Height = 478
    ActivePage = jvspCropName
    PropagateEnable = False
    Align = alClient
    OnChange = jplMainChange
    object jvspCropName: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Crops_Pane'
      Caption = 'jvspCropName'
      inline frameCropName: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
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
            Left = 232
            OnClick = frameCropNamesbAddClick
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            OnClick = frameCropNamesbInsertClick
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
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
          Width = 448
          Height = 380
          OnSetEditText = frameCropNameGridSetEditText
          OnBeforeDrawCell = frameCropNameGridBeforeDrawCell
          OnButtonClick = GridButtonClick
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
    object jvspRootDepth: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Rooting_Depth_Pane'
      Caption = 'jvspRootDepth'
      inline frameRootDepth: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 232
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 448
          Height = 380
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameRootDepthGridEndUpdate
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
    object jvspEvapFractions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Consumptive_Use_Factors_Pane'
      Caption = 'jvspEvapFractions'
      inline frameEvapFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 232
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 448
          Height = 380
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameEvapFractionsGridEndUpdate
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
    object jvspLosses: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Inefficiency_Losses_to_Surface'
      Caption = 'jvspLosses'
      inline frameLosses: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 232
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 448
          Height = 380
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameLossesGridEndUpdate
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
    object jvspCropFunction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Crop_Price_Function_Pane'
      Caption = 'jvspCropFunction'
      inline frameCropFunction: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 232
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 448
          Height = 380
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameCropFunctionGridEndUpdate
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
    object jvspCropWaterUse: TJvStandardPage
      Left = 0
      Top = 0
      Width = 448
      Height = 478
      HelpType = htKeyword
      HelpKeyword = 'Consumptive_Use_Flux_or_Crop_C'
      Caption = 'jvspCropWaterUse'
      inline frameCropWaterUse: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 448
        Height = 478
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 448
        ExplicitHeight = 478
        inherited Panel: TPanel
          Top = 437
          Width = 448
          ExplicitTop = 437
          ExplicitWidth = 448
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 232
            ExplicitLeft = 148
          end
          inherited sbInsert: TSpeedButton
            Left = 275
            ExplicitLeft = 176
          end
          inherited sbDelete: TSpeedButton
            Left = 318
            ExplicitLeft = 204
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 448
          Height = 380
          OnSetEditText = GridSetEditText
          OnBeforeDrawCell = frameCropWaterUseGridBeforeDrawCell
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameCropWaterUseGridEndUpdate
          ExplicitWidth = 448
          ExplicitHeight = 380
        end
        inherited pnlTop: TPanel
          Width = 448
          ExplicitWidth = 448
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
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 478
    Width = 683
    Height = 44
    Align = alBottom
    ParentColor = True
    TabOrder = 2
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
