inherited framePackageFmp4Climate: TframePackageFmp4Climate
  Width = 435
  Height = 320
  ExplicitWidth = 435
  ExplicitHeight = 320
  object lblPrecipitation: TLabel [2]
    Left = 176
    Top = 160
    Width = 67
    Height = 15
    Caption = 'Precipitation'
  end
  object lblReferenceET: TLabel [3]
    Left = 176
    Top = 189
    Width = 246
    Height = 15
    Caption = 'Reference evapotranspiration (REFERENCE_ET )'
  end
  object lblPotET_Bare: TLabel [4]
    Left = 176
    Top = 218
    Width = 182
    Height = 30
    Caption = 'Potetntial ET rate of bare soil (POTENTIAL_EVAPORATION_BARE)'
    WordWrap = True
  end
  object lblDirectRecharge: TLabel [5]
    Left = 176
    Top = 255
    Width = 156
    Height = 15
    Caption = 'Direct recharge beneath roots'
  end
  object lblPrecipitationPotConsup: TLabel [6]
    Left = 176
    Top = 284
    Width = 191
    Height = 15
    Caption = 'Precipitation potential consumption'
  end
  inherited memoComments: TMemo
    Width = 404
    ExplicitWidth = 404
  end
  object comboPrecipitation: TComboBox [8]
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
  object comboReferenceET: TComboBox [9]
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
  object comboPotET_Bare: TComboBox [10]
    Left = 16
    Top = 215
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 3
    Text = 'Don'#39't use'
    Items.Strings = (
      'Don'#39't use'
      'Static'
      'Transient')
  end
  object comboDirectRecharge: TComboBox [11]
    Left = 16
    Top = 252
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 4
    Text = 'Don'#39't use'
    Items.Strings = (
      'Don'#39't use'
      'Static'
      'Transient')
  end
  object comboPrecipitationPotConsup: TComboBox [12]
    Left = 16
    Top = 281
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 5
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
        Control = comboPrecipitation
      end
      item
        Control = comboReferenceET
      end
      item
        Control = comboPotET_Bare
      end
      item
        Control = comboDirectRecharge
      end
      item
        Control = comboPrecipitationPotConsup
      end>
  end
end
