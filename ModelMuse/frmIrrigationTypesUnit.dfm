inherited frmIrrigationTypes: TfrmIrrigationTypes
  Caption = 'Irrigation Types'
  ClientHeight = 432
  ClientWidth = 495
  ExplicitWidth = 507
  ExplicitHeight = 470
  TextHeight = 18
  object splitterMain: TJvNetscapeSplitter
    Left = 105
    Top = 0
    Height = 383
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
    Top = 383
    Width = 495
    Height = 49
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    ExplicitTop = 382
    ExplicitWidth = 491
    DesignSize = (
      495
      49)
    object btnCancel: TBitBtn
      Left = 368
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 364
    end
    object btnOK: TBitBtn
      Left = 279
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 275
    end
    object btnHelp: TBitBtn
      Left = 190
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 186
    end
  end
  object jvpltvMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 105
    Height = 383
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
    ExplicitHeight = 382
  end
  object jvplMain: TJvPageList
    Left = 115
    Top = 0
    Width = 380
    Height = 383
    ActivePage = jvspEvapFraction
    PropagateEnable = False
    Align = alClient
    ExplicitWidth = 376
    ExplicitHeight = 382
    object jvspIrrigationTypes: TJvStandardPage
      Left = 0
      Top = 0
      Width = 380
      Height = 383
      Caption = 'jvspIrrigationTypes'
      inline frameIrrigationTypes: TframeGrid
        Left = 0
        Top = 0
        Width = 384
        Height = 383
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 380
        ExplicitHeight = 383
        inherited Panel: TPanel
          Top = 342
          Width = 384
          ExplicitTop = 342
          ExplicitWidth = 380
          inherited lbNumber: TLabel
            Width = 179
            Height = 18
            Caption = 'Number or irrigation types'
            ExplicitWidth = 179
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 284
            ExplicitLeft = 332
          end
          inherited sbInsert: TSpeedButton
            Left = 307
            ExplicitLeft = 361
          end
          inherited sbDelete: TSpeedButton
            Left = 335
            ExplicitLeft = 390
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameIrrigationTypesseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 384
          Height = 342
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
          ExplicitWidth = 380
          ExplicitHeight = 342
        end
      end
    end
    object jvspEvapFraction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 380
      Height = 383
      Caption = 'jvspEvapFraction'
      ExplicitWidth = 376
      ExplicitHeight = 382
      inline frameEvaporationFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 380
        Height = 383
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 376
        ExplicitHeight = 382
        inherited Panel: TPanel
          Top = 342
          Width = 380
          ExplicitTop = 341
          ExplicitWidth = 376
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 189
            ExplicitLeft = 197
          end
          inherited sbInsert: TSpeedButton
            Left = 224
            ExplicitLeft = 234
          end
          inherited sbDelete: TSpeedButton
            Left = 259
            ExplicitLeft = 271
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 380
          Height = 285
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameEvaporationFractionsGridEndUpdate
          ExplicitWidth = 376
          ExplicitHeight = 284
        end
        inherited pnlTop: TPanel
          Width = 380
          ExplicitWidth = 376
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
