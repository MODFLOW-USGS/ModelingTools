inherited framePackageMf6Obs: TframePackageMf6Obs
  Height = 355
  ExplicitHeight = 355
  DesignSize = (
    422
    355)
  object lblOutputFormat: TLabel [2]
    Left = 16
    Top = 160
    Width = 77
    Height = 15
    Caption = 'Output format'
  end
  object lblNumberOfDigits: TLabel [3]
    Left = 16
    Top = 216
    Width = 112
    Height = 15
    Caption = 'Text number of digits'
  end
  object comboOutputFormat: TComboBox [5]
    Left = 16
    Top = 179
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 1
    Text = 'Text'
    OnChange = comboOutputFormatChange
    Items.Strings = (
      'Text'
      'Binary')
  end
  object seNumberOfDigits: TJvSpinEdit [6]
    Left = 16
    Top = 235
    Width = 145
    Height = 21
    MaxValue = 30.000000000000000000
    MinValue = 1.000000000000000000
    Value = 10.000000000000000000
    Enabled = False
    TabOrder = 2
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
