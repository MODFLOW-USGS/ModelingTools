inherited frameSoilEffectivePrecip: TframeSoilEffectivePrecip
  Width = 636
  Height = 294
  ExplicitWidth = 636
  ExplicitHeight = 294
  inherited Panel: TPanel
    Top = 253
    Width = 636
    ExplicitTop = 259
    ExplicitWidth = 483
    DesignSize = (
      636
      41)
    inherited sbAdd: TSpeedButton
      Left = 506
      ExplicitLeft = 382
    end
    inherited sbInsert: TSpeedButton
      Left = 545
      ExplicitLeft = 411
    end
    inherited sbDelete: TSpeedButton
      Left = 583
      ExplicitLeft = 440
    end
    object lblMethod: TLabel [4]
      Left = 279
      Top = 8
      Width = 42
      Height = 15
      Caption = 'Method'
    end
    inherited seNumber: TJvSpinEdit
      Height = 23
      ExplicitHeight = 23
    end
    object comboInterpolation: TComboBox
      Left = 145
      Top = 6
      Width = 128
      Height = 23
      Hint = 'Interpolation Method'
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'Constant'
        'Interpolate'
        'Step function'
        'Nearest')
    end
  end
  inherited Grid: TRbwDataGrid4
    Top = 81
    Width = 636
    Height = 172
    ColCount = 2
    Columns = <
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = True
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
      end
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = True
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
        ButtonFont.Name = 'Segoe UI'
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
    ExplicitTop = 59
    ExplicitWidth = 483
    ExplicitHeight = 196
  end
  inherited pnlTop: TPanel
    Width = 636
    Height = 81
    ExplicitWidth = 636
    ExplicitHeight = 81
    object lblSoil: TLabel [0]
      Left = 8
      Top = 8
      Width = 19
      Height = 15
      Caption = 'Soil'
    end
    inherited edFormula: TLabeledEdit
      Top = 52
      Height = 23
      EditLabel.ExplicitLeft = 128
      EditLabel.ExplicitTop = 34
      EditLabel.ExplicitWidth = 44
      ExplicitTop = 52
      ExplicitHeight = 23
    end
    inherited cbMultiCheck: TCheckBox
      Top = 56
      ExplicitTop = 56
    end
  end
end
