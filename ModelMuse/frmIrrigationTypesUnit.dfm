inherited frmIrrigationTypes: TfrmIrrigationTypes
  Caption = 'Irrigation Types'
  ClientHeight = 433
  ClientWidth = 499
  ExplicitWidth = 511
  ExplicitHeight = 471
  TextHeight = 18
  object splitterMain: TJvNetscapeSplitter
    Left = 105
    Top = 0
    Height = 384
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
    Top = 384
    Width = 499
    Height = 49
    Align = alBottom
    ParentColor = True
    TabOrder = 0
    ExplicitTop = 185
    ExplicitWidth = 422
    DesignSize = (
      499
      49)
    object btnCancel: TBitBtn
      Left = 390
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 313
    end
    object btnOK: TBitBtn
      Left = 301
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 224
    end
    object btnHelp: TBitBtn
      Left = 212
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 135
    end
  end
  object jvpltvMain: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 105
    Height = 384
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
    ExplicitLeft = 4
  end
  object jvplMain: TJvPageList
    Left = 115
    Top = 0
    Width = 384
    Height = 384
    ActivePage = jvspIrrigationTypes
    PropagateEnable = False
    Align = alClient
    ExplicitLeft = 120
    ExplicitTop = 96
    ExplicitWidth = 300
    ExplicitHeight = 200
    object jvspIrrigationTypes: TJvStandardPage
      Left = 0
      Top = 0
      Width = 384
      Height = 384
      Caption = 'jvspIrrigationTypes'
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameIrrigationTypes: TframeGrid
        Left = 0
        Top = 0
        Width = 384
        Height = 384
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 126
        ExplicitWidth = 300
        ExplicitHeight = 186
        inherited Panel: TPanel
          Top = 343
          Width = 384
          ExplicitTop = 145
          ExplicitWidth = 300
          inherited lbNumber: TLabel
            Width = 179
            Height = 18
            Caption = 'Number or irrigation types'
            ExplicitWidth = 179
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 290
            ExplicitLeft = 332
          end
          inherited sbInsert: TSpeedButton
            Left = 314
            ExplicitLeft = 361
          end
          inherited sbDelete: TSpeedButton
            Left = 342
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
          Height = 343
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
          ExplicitWidth = 300
          ExplicitHeight = 145
        end
      end
    end
    object jvspEvapFraction: TJvStandardPage
      Left = 0
      Top = 0
      Width = 384
      Height = 384
      Caption = 'jvspEvapFraction'
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameEvaporationFractions: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 384
        Height = 384
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 10
        ExplicitTop = 22
        inherited Panel: TPanel
          Top = 343
          Width = 384
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 197
          end
          inherited sbInsert: TSpeedButton
            Left = 234
          end
          inherited sbDelete: TSpeedButton
            Left = 271
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 384
          Height = 286
          OnSetEditText = GridSetEditText
          OnButtonClick = GridButtonClick
          OnEndUpdate = frameEvaporationFractionsGridEndUpdate
        end
        inherited pnlTop: TPanel
          Width = 384
          inherited edFormula: TLabeledEdit
            Height = 26
            EditLabel.Width = 57
            EditLabel.Height = 18
            EditLabel.ExplicitLeft = 128
            EditLabel.ExplicitTop = 9
            EditLabel.ExplicitHeight = 18
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
