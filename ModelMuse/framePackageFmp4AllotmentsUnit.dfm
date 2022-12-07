inherited framePackageFmp4Allotments: TframePackageFmp4Allotments
  Height = 287
  ExplicitHeight = 287
  DesignSize = (
    422
    287)
  object lblSURFACE_WATER: TLabel [2]
    Left = 171
    Top = 160
    Width = 71
    Height = 15
    Caption = 'Surface water'
  end
  object lblGROUNDWATER: TLabel [3]
    Left = 171
    Top = 189
    Width = 69
    Height = 15
    Caption = 'Groundwater'
  end
  object comboSURFACE_WATER: TComboBox [5]
    Left = 16
    Top = 157
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 1
    Text = 'Don'#39't use'
    Items.Strings = (
      'Don'#39't use'
      'Static'
      'Transient')
  end
  object comboGROUNDWATER: TComboBox [6]
    Left = 16
    Top = 186
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = 'Don'#39't use'
    Items.Strings = (
      'Don'#39't use'
      'Static'
      'Transient')
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
        Control = comboSURFACE_WATER
      end
      item
        Control = comboGROUNDWATER
      end>
  end
end
