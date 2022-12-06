inherited frameMt3dCtsPkg: TframeMt3dCtsPkg
  Height = 277
  ExplicitHeight = 277
  DesignSize = (
    422
    277)
  object lblForce: TLabel [2]
    Left = 16
    Top = 165
    Width = 341
    Height = 15
    Caption = 'Force injection concentrations to meet treatment levels (IFORCE)'
  end
  object lblWellPackageChoice: TLabel [3]
    Left = 16
    Top = 221
    Width = 163
    Height = 15
    Caption = 'Well package choice (ICTSPKG)'
  end
  object comboForce: TComboBox [5]
    Left = 16
    Top = 184
    Width = 391
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 1
    Text = 
      'Force compliance only if blended concentration exceeds a limit (' +
      '0)'
    Items.Strings = (
      
        'Force compliance only if blended concentration exceeds a limit (' +
        '0)'
      'Always force compliance (1)')
  end
  object comboWellPackageChoice: TComboBox [6]
    Left = 16
    Top = 240
    Width = 391
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = 'MNW2 Multi-Node Well version 2'
    Items.Strings = (
      'MNW2 Multi-Node Well version 2'
      'WEL: Well package')
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
        Control = comboForce
      end
      item
        Control = comboWellPackageChoice
      end>
  end
end
