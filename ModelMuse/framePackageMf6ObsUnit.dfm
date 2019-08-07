inherited framePackageMf6Obs: TframePackageMf6Obs
  Height = 355
  ExplicitHeight = 355
  DesignSize = (
    422
    355)
  object lblOutputFormat: TLabel [2]
    Left = 16
    Top = 160
    Width = 69
    Height = 13
    Caption = 'Output format'
  end
  object lblOutputPrecision: TLabel [3]
    Left = 16
    Top = 216
    Width = 110
    Height = 13
    Caption = 'Binary output precision'
  end
  object lblNumberOfDigits: TLabel [4]
    Left = 16
    Top = 272
    Width = 102
    Height = 13
    Caption = 'Text number of digits'
  end
  object comboOutputPrecision: TComboBox [6]
    Left = 16
    Top = 235
    Width = 145
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 1
    Items.Strings = (
      'Single'
      'Double')
  end
  object comboOutputFormat: TComboBox [7]
    Left = 16
    Top = 179
    Width = 145
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = 'Text'
    OnChange = comboOutputFormatChange
    Items.Strings = (
      'Text'
      'Binary')
  end
  object seNumberOfDigits: TJvSpinEdit [8]
    Left = 16
    Top = 291
    Width = 145
    Height = 21
    MaxValue = 30.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 3
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = comboOutputFormat
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
