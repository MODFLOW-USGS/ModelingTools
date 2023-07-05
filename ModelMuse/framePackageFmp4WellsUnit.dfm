inherited framePackageFmp4Wells: TframePackageFmp4Wells
  Width = 595
  Height = 514
  ExplicitWidth = 595
  ExplicitHeight = 514
  DesignSize = (
    595
    514)
  object lblPumpSpread: TLabel [2]
    Left = 271
    Top = 290
    Width = 272
    Height = 30
    Caption = 
      'Specify how pumping demand should be allocated among MNW well no' +
      'des (MNW_PUMP_SPREAD)'
    Constraints.MaxWidth = 310
    WordWrap = True
  end
  object lblWellLayer: TLabel [3]
    Left = 223
    Top = 384
    Width = 233
    Height = 15
    Caption = 'Method for specifying well vertical locations'
    Visible = False
    WordWrap = True
  end
  object lblXY: TLabel [4]
    Left = 223
    Top = 355
    Width = 248
    Height = 30
    Caption = 
      'Method for specifying well horizontal locations (INPUT_OPTION XY' +
      ')'
    WordWrap = True
  end
  object lblSmoothing: TLabel [5]
    Left = 223
    Top = 413
    Width = 59
    Height = 15
    Caption = 'Smoothing'
    Visible = False
  end
  object lblProrateDemand: TLabel [6]
    Left = 223
    Top = 442
    Width = 86
    Height = 15
    Caption = 'Prorate Demand'
    Visible = False
  end
  inherited memoComments: TMemo
    Top = 70
    Width = 564
    ExplicitTop = 70
    ExplicitWidth = 564
  end
  object clbPrint: TCheckListBox [8]
    Left = 16
    Top = 165
    Width = 249
    Height = 116
    Enabled = False
    ItemHeight = 15
    Items.Strings = (
      'PRINT BYWELL'
      'PRINT ByWBS'
      'PRINT ByMNW'
      'PRINT LIST'
      'PRINT SMOOTHING'
      'PRINT BYWBS_BYLAYER')
    TabOrder = 1
  end
  object comboPumpSpread: TComboBox [9]
    Left = 16
    Top = 287
    Width = 249
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = 'By node conductance (0)'
    Items.Strings = (
      'By node conductance (0)'
      'Evenly among nodes (1)'
      'Assign to top node (2)'
      'Assign separately in each WBS')
  end
  object comboWellLayer: TComboBox [10]
    Left = 16
    Top = 381
    Width = 201
    Height = 23
    Enabled = False
    ItemIndex = 2
    TabOrder = 3
    Text = 'By depth below surface'
    Visible = False
    Items.Strings = (
      'By layer'
      'By elevation'
      'By depth below surface')
  end
  object comboXY: TComboBox [11]
    Left = 16
    Top = 352
    Width = 201
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 4
    Text = 'By row and column'
    Items.Strings = (
      'By row and column'
      'By X and Y coordinates')
  end
  object comboSmoothing: TComboBox [12]
    Left = 16
    Top = 410
    Width = 201
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 5
    Text = 'none'
    Visible = False
    Items.Strings = (
      'none'
      'By fraction'
      'By thickness')
  end
  object comboProrateDemand: TComboBox [13]
    Left = 16
    Top = 439
    Width = 201
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 6
    Text = 'none'
    Visible = False
    Items.Strings = (
      'none'
      'By capacity'
      'By average')
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
        Control = clbPrint
      end
      item
        Control = comboPumpSpread
      end
      item
        Control = comboXY
      end
      item
        Control = comboWellLayer
      end
      item
        Control = comboSmoothing
      end
      item
        Control = comboProrateDemand
      end>
  end
end
