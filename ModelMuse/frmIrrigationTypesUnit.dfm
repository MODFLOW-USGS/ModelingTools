inherited frmIrrigationTypes: TfrmIrrigationTypes
  HelpType = htKeyword
  HelpKeyword = 'Farm-Irrigation-Types--Dialog-'
  Caption = 'Irrigation Types'
  ClientHeight = 431
  ClientWidth = 581
  ExplicitWidth = 597
  ExplicitHeight = 470
  TextHeight = 18
  object splitterMain: TJvNetscapeSplitter
    Left = 105
    Top = 0
    Height = 382
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 225
    ExplicitTop = -243
    ExplicitHeight = 478
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 382
    Width = 581
    Height = 49
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      581
      49)
    object btnCancel: TBitBtn
      Left = 486
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 397
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 308
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object jvpltvMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 105
    Height = 382
    ShowButtons = True
    PageDefault = 0
    PageList = jvplMain
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 1
    OnChange = jvpltvMainChange
    OnCustomDrawItem = jvpltvMainCustomDrawItem
    Items.Links = {00000000}
  end
  object jvplMain: TJvPageList
    Left = 115
    Top = 0
    Width = 466
    Height = 382
    ActivePage = jvspEvapFraction
    PropagateEnable = False
    Align = alClient
    object jvspIrrigationTypes: TJvStandardPage
      Left = 0
      Top = 0
      Width = 466
      Height = 382
      Caption = 'jvspIrrigationTypes'
      inline frameIrrigationTypes: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 466
        Height = 382
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 466
        ExplicitHeight = 382
        inherited Panel: TPanel
          Top = 341
          Width = 466
          ExplicitTop = 341
          ExplicitWidth = 466
          inherited lbNumber: TLabel
            Width = 179
            Height = 18
            Caption = 'Number or irrigation types'
            ExplicitWidth = 179
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 335
            ExplicitLeft = 332
          end
          inherited sbInsert: TSpeedButton
            Left = 364
            ExplicitLeft = 361
          end
          inherited sbDelete: TSpeedButton
            Left = 393
            ExplicitLeft = 390
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameIrrigationTypesseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 466
          Height = 284
          ColCount = 2
          DefaultColWidth = 65
          FixedCols = 1
          OnEndUpdate = frameIrrigationTypesGridEndUpdate
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
          ExplicitWidth = 466
          ExplicitHeight = 284
        end
        inherited pnlTop: TPanel
          Width = 466
          ExplicitWidth = 466
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
    object jvspEvapFraction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 466
      Height = 382
      Caption = 'jvspEvapFraction'
      inline frameEvaporationFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 466
        Height = 382
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 466
        ExplicitHeight = 382
        inherited Panel: TPanel
          Top = 341
          Width = 466
          ExplicitTop = 341
          ExplicitWidth = 466
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 224
            ExplicitLeft = 197
          end
          inherited sbInsert: TSpeedButton
            Left = 263
            ExplicitLeft = 234
          end
          inherited sbDelete: TSpeedButton
            Left = 307
            ExplicitLeft = 271
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 466
          Height = 284
          OnSelectCell = frameEvaporationFractionsGridSelectCell
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameEvaporationFractionsGridEndUpdate
          ExplicitWidth = 466
          ExplicitHeight = 284
        end
        inherited pnlTop: TPanel
          Width = 466
          ExplicitWidth = 466
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
  end
  object rbwprsrGlobal: TRbwParser
    Left = 112
    Top = 8
  end
end
