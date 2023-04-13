inherited frameDeliveryGrid: TframeDeliveryGrid
  Width = 476
  Height = 210
  ExplicitWidth = 476
  ExplicitHeight = 210
  inherited Panel: TPanel
    Top = 169
    Width = 476
    ExplicitTop = 169
    ExplicitWidth = 476
    DesignSize = (
      476
      41)
    inherited sbAdd: TSpeedButton
      Left = 344
      ExplicitLeft = 282
    end
    inherited sbInsert: TSpeedButton
      Left = 390
      ExplicitLeft = 320
    end
    inherited sbDelete: TSpeedButton
      Left = 436
      ExplicitLeft = 358
    end
    object lblNumberOfDeliveryTypes: TLabel [4]
      Left = 247
      Top = 6
      Width = 72
      Height = 30
      Caption = 'Number of delivery types'
      WordWrap = True
    end
    object seNumberOfDeliveryTypes: TJvSpinEdit
      Left = 176
      Top = 6
      Width = 65
      Height = 23
      CheckMinValue = True
      TabOrder = 1
      OnChange = seNumberOfDeliveryTypesChange
    end
  end
  inherited Grid: TRbwDataGrid4
    Width = 476
    Height = 112
    ColCount = 2
    OnSelectCell = GridSelectCell
    OnSetEditText = GridSetEditText
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end>
    ExplicitWidth = 476
    ExplicitHeight = 112
  end
  inherited pnlTop: TPanel
    Width = 476
    ExplicitWidth = 476
    object lblHowUsed: TLabel [0]
      Left = 255
      Top = 11
      Width = 53
      Height = 15
      Caption = 'How used'
      Enabled = False
    end
    inherited edFormula: TLabeledEdit
      EditLabel.ExplicitTop = -16
      EditLabel.ExplicitWidth = 50
    end
    inherited cbMultiCheck: TCheckBox
      TabOrder = 3
    end
    object comboHowUsed: TComboBox
      Left = 255
      Top = 30
      Width = 105
      Height = 23
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
      OnChange = comboHowUsedChange
      Items.Strings = (
        'Take required amount (0)'
        'Surplus discharged (1)'
        'Surplus stored (2)')
    end
  end
end
