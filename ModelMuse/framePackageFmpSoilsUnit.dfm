inherited framePackageFmpSoils: TframePackageFmpSoils
  Height = 274
  ExplicitHeight = 274
  DesignSize = (
    422
    274)
  object lblCapillaryFringe: TLabel [2]
    Left = 176
    Top = 160
    Width = 80
    Height = 15
    Caption = 'Capillary fringe'
  end
  object lblSufaceVK: TLabel [3]
    Left = 176
    Top = 189
    Width = 201
    Height = 15
    Caption = 'Surface vertical hydraulic conductivity'
  end
  object comboCapillaryFringe: TComboBox [5]
    Left = 16
    Top = 157
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 1
    Text = 'Array'
    Items.Strings = (
      'Array'
      'List')
  end
  object comboSufaceVK: TComboBox [6]
    Left = 16
    Top = 186
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 2
    Text = 'Array'
    Items.Strings = (
      'Array'
      'List')
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
        Control = comboCapillaryFringe
      end
      item
        Control = comboSufaceVK
      end>
  end
end
