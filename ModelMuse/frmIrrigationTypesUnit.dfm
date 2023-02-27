inherited frmIrrigationTypes: TfrmIrrigationTypes
  Caption = 'Irrigation Types'
  ClientHeight = 235
  TextHeight = 18
  inline frameIrrigationTypes: TframeGrid
    Left = 0
    Top = 0
    Width = 426
    Height = 186
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 426
    ExplicitHeight = 185
    inherited Panel: TPanel
      Top = 144
      Width = 430
      ExplicitTop = 144
      ExplicitWidth = 426
      inherited lbNumber: TLabel
        Width = 179
        Height = 18
        Caption = 'Number or irrigation types'
        ExplicitWidth = 179
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 332
        ExplicitLeft = 332
      end
      inherited sbInsert: TSpeedButton
        Left = 361
        ExplicitLeft = 361
      end
      inherited sbDelete: TSpeedButton
        Left = 390
        ExplicitLeft = 390
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        OnChange = frameIrrigationTypesseNumberChange
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 430
      Height = 144
      ColCount = 2
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
      ExplicitWidth = 426
      ExplicitHeight = 144
      ColWidths = (
        64
        288)
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 186
    Width = 426
    Height = 49
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 185
    DesignSize = (
      426
      49)
    object btnCancel: TBitBtn
      Left = 329
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 333
    end
    object btnOK: TBitBtn
      Left = 240
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 244
    end
    object btnHelp: TBitBtn
      Left = 151
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 155
    end
  end
end
