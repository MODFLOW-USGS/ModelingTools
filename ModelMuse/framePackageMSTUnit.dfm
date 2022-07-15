inherited framePackageMST: TframePackageMST
  Width = 498
  Height = 434
  ExplicitWidth = 498
  ExplicitHeight = 434
  inherited memoComments: TMemo
    Width = 467
    ExplicitWidth = 467
  end
  object rgPorosity: TRadioGroup [3]
    Left = 16
    Top = 160
    Width = 467
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Porosity'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Use shared porosity data set'
      'Use separate porosity data set')
    TabOrder = 1
  end
  object cbFirstOrderDecay: TCheckBox [4]
    Left = 16
    Top = 256
    Width = 449
    Height = 17
    Caption = 'Use first order decay (FIRST_ORDER_DECAY)'
    Enabled = False
    TabOrder = 2
  end
  object cbZeroOrderDecay: TCheckBox [5]
    Left = 16
    Top = 279
    Width = 449
    Height = 17
    Caption = 'Use zero order decay (ZERO_ORDER_DECAY)'
    Enabled = False
    TabOrder = 3
  end
  object rgSorption: TRadioGroup [6]
    Left = 16
    Top = 302
    Width = 467
    Height = 123
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Sorption'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'not used'
      'LINEAR'
      'FREUNDLICH'
      'LANGMUIR')
    TabOrder = 4
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
        Control = cbFirstOrderDecay
      end
      item
        Control = cbZeroOrderDecay
      end
      item
        Control = rgSorption
      end>
  end
end
