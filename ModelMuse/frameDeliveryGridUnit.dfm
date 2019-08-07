inherited frameDeliveryGrid: TframeDeliveryGrid
  Width = 393
  Height = 210
  ExplicitWidth = 393
  ExplicitHeight = 210
  inherited Panel: TPanel
    Top = 169
    Width = 393
    ExplicitTop = 169
    ExplicitWidth = 393
    DesignSize = (
      393
      41)
    inherited sbAdd: TSpeedButton
      Left = 282
      ExplicitLeft = 282
    end
    inherited sbInsert: TSpeedButton
      Left = 320
      ExplicitLeft = 320
    end
    inherited sbDelete: TSpeedButton
      Left = 358
      ExplicitLeft = 358
    end
    object lblNumberOfDeliveryTypes: TLabel [4]
      Left = 247
      Top = 6
      Width = 41
      Height = 52
      Caption = 'Number of delivery types'
      WordWrap = True
      OnClick = lblNumberOfDeliveryTypesClick
    end
    object seNumberOfDeliveryTypes: TJvSpinEdit
      Left = 176
      Top = 6
      Width = 65
      Height = 21
      CheckMinValue = True
      TabOrder = 1
      OnChange = seNumberOfDeliveryTypesChange
    end
  end
  inherited Grid: TRbwDataGrid4
    Width = 393
    Height = 112
    ColCount = 2
    OnSelectCell = GridSelectCell
    OnSetEditText = GridSetEditText
    Columns = <
      item
        AutoAdjustRowHeights = False
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
    ExplicitWidth = 393
    ExplicitHeight = 112
  end
  inherited pnlTop: TPanel
    Width = 393
    ExplicitWidth = 393
    object lblHowUsed: TLabel [0]
      Left = 255
      Top = 11
      Width = 47
      Height = 13
      Caption = 'How used'
      Enabled = False
    end
    inherited edFormula: TLabeledEdit
      EditLabel.ExplicitLeft = 0
      EditLabel.ExplicitTop = -16
      EditLabel.ExplicitWidth = 50
    end
    inherited cbMultiCheck: TCheckBox
      TabOrder = 2
    end
    object comboHowUsed: TComboBox
      Left = 255
      Top = 30
      Width = 105
      Height = 21
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
