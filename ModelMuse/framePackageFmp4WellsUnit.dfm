inherited framePackageFmp4Wells: TframePackageFmp4Wells
  Width = 595
  Height = 514
  ExplicitWidth = 595
  ExplicitHeight = 514
  DesignSize = (
    595
    514)
  object lblPumpSpread: TLabel [2]
    Left = 223
    Top = 275
    Width = 357
    Height = 45
    Caption = 
      'Specify how pumping demand should be allocated among MNW well no' +
      'des (MNW_PUMP_SPREAD)'
    WordWrap = True
  end
  object lblWellLayer: TLabel [3]
    Left = 223
    Top = 379
    Width = 351
    Height = 30
    Caption = 'Method for specifying well vertical locations'
    WordWrap = True
  end
  object lblXY: TLabel [4]
    Left = 223
    Top = 329
    Width = 351
    Height = 30
    Caption = 'Method for specifying well horizontal locations'
    WordWrap = True
  end
  object lblWellFormat: TLabel [5]
    Left = 223
    Top = 419
    Width = 96
    Height = 15
    Caption = 'Well input  format'
  end
  inherited memoComments: TMemo
    Top = 70
    Width = 564
    ExplicitTop = 70
  end
  object clbPrint: TCheckListBox [7]
    Left = 16
    Top = 165
    Width = 201
    Height = 100
    Enabled = False
    ItemHeight = 15
    Items.Strings = (
      'PRINT BYWELL'
      'PRINT ByWBS'
      'PRINT ByMNW'
      'PRINT LIST'
      'PRINT SMOOTHING')
    TabOrder = 1
  end
  object comboPumpSpread: TComboBox [8]
    Left = 16
    Top = 272
    Width = 201
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
  object comboWellLayer: TComboBox [9]
    Left = 16
    Top = 376
    Width = 201
    Height = 23
    Enabled = False
    ItemIndex = 2
    TabOrder = 3
    Text = 'By depth below surface'
    Items.Strings = (
      'By layer'
      'By elevation'
      'By depth below surface')
  end
  object comboXY: TComboBox [10]
    Left = 16
    Top = 326
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
  object comboWellFormat: TComboBox [11]
    Left = 16
    Top = 416
    Width = 201
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 5
    Text = 'TIME FRAME'
    Items.Strings = (
      'TIME FRAME'
      'LINEFEED WBS'
      'LINEFEED CAPACITY')
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
        Control = comboWellFormat
      end>
  end
end
