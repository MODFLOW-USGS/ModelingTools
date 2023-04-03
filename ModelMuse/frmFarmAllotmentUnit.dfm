inherited frmFarmAllotment: TfrmFarmAllotment
  HelpType = htKeyword
  HelpKeyword = 'Farm_Allotment_Dialog_Box'
  Caption = 'Farm Allotment'
  ClientHeight = 330
  ClientWidth = 398
  ExplicitWidth = 410
  ExplicitHeight = 368
  TextHeight = 18
  inline frameAllotment: TframeFormulaGrid
    Left = 0
    Top = 0
    Width = 398
    Height = 286
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 394
    ExplicitHeight = 285
    inherited Panel: TPanel
      Top = 245
      Width = 398
      ExplicitTop = 244
      ExplicitWidth = 394
      inherited lbNumber: TLabel
        Width = 180
        Height = 18
        Caption = 'Number of allotment items'
        ExplicitWidth = 180
        ExplicitHeight = 18
      end
      inherited sbAdd: TSpeedButton
        Left = 302
        ExplicitLeft = 305
      end
      inherited sbInsert: TSpeedButton
        Left = 330
        ExplicitLeft = 334
      end
      inherited sbDelete: TSpeedButton
        Left = 360
        ExplicitLeft = 363
      end
      inherited seNumber: TJvSpinEdit
        Height = 26
        ExplicitHeight = 26
      end
    end
    inherited Grid: TRbwDataGrid4
      Width = 398
      Height = 188
      ColCount = 3
      OnSetEditText = frameAllotmentGridSetEditText
      OnButtonClick = frameAllotmentGridButtonClick
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
          AutoAdjustColWidths = False
        end>
      ExplicitWidth = 394
      ExplicitHeight = 187
    end
    inherited pnlTop: TPanel
      Width = 398
      ExplicitWidth = 394
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
  object pnlBottom: TPanel
    Left = 0
    Top = 286
    Width = 398
    Height = 44
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 285
    ExplicitWidth = 394
    object btnCancel: TBitBtn
      Left = 295
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 198
      Top = 6
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 101
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
