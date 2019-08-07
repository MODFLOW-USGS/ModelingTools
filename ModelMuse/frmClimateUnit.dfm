inherited frmClimate: TfrmClimate
  HelpType = htKeyword
  HelpKeyword = 'Farm_Climate_Dialog_Box'
  Caption = 'Farm Climate'
  ClientHeight = 275
  ClientWidth = 599
  ExplicitWidth = 615
  ExplicitHeight = 313
  PixelsPerInch = 96
  TextHeight = 18
  inline frameClimate: TframeFormulaGrid
    Left = 0
    Top = 0
    Width = 599
    Height = 231
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 599
    ExplicitHeight = 231
    inherited Panel: TPanel
      Top = 190
      Width = 599
      ExplicitTop = 190
      ExplicitWidth = 599
      inherited lbNumber: TLabel
        Width = 168
        Height = 18
        Caption = 'Number of climate items'
        ExplicitWidth = 168
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 363
        ExplicitLeft = 363
      end
      inherited sbInsert: TSpeedButton
        Left = 392
        ExplicitLeft = 312
      end
      inherited sbDelete: TSpeedButton
        Left = 429
        ExplicitLeft = 432
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 599
      Height = 133
      ColCount = 6
      OnSetEditText = frameClimateGridSetEditText
      OnButtonClick = frameClimateGridButtonClick
      Columns = <
        item
          AutoAdjustRowHeights = True
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
          AutoAdjustRowHeights = True
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
          AutoAdjustRowHeights = True
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
        end>
      ExplicitWidth = 599
      ExplicitHeight = 133
    end
    inherited pnlTop: TPanel
      Width = 599
      ExplicitWidth = 599
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
  object pnlBottom: TPanel
    Left = 0
    Top = 231
    Width = 599
    Height = 44
    Align = alBottom
    ParentColor = True
    TabOrder = 1
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
    Left = 72
    Top = 16
  end
end
