inherited framePackageMST: TframePackageMST
  Width = 498
  Height = 462
  ExplicitWidth = 498
  ExplicitHeight = 462
  inherited memoComments: TMemo
    Width = 467
    ExplicitWidth = 467
  end
  object rgPorosity: TRadioGroup [3]
    Left = 16
    Top = 160
    Width = 467
    Height = 81
    Caption = 'Porosity'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Use shared porosity data set'
      'Use separate porosity data set')
    TabOrder = 1
  end
  object rgSorption: TRadioGroup [4]
    Left = 16
    Top = 334
    Width = 467
    Height = 123
    Caption = 'Sorption'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'not used'
      'LINEAR'
      'FREUNDLICH'
      'LANGMUIR')
    TabOrder = 2
  end
  object rgDecay: TRadioGroup [5]
    Left = 16
    Top = 240
    Width = 467
    Height = 96
    Caption = 'Decay'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'none'
      'ZERO_ORDER_DECAY'
      'FIRST_ORDER_DECAY')
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
        Control = rgPorosity
      end
      item
        Control = rgDecay
      end
      item
        Control = rgSorption
      end>
  end
end
