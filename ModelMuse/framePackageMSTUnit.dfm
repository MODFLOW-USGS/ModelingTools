inherited framePackageMST: TframePackageMST
  Width = 498
  Height = 486
  ExplicitWidth = 498
  ExplicitHeight = 486
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited memoComments: TMemo
    Width = 467
    StyleElements = [seFont, seClient, seBorder]
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
  object cbSorbate: TCheckBox [6]
    Left = 16
    Top = 464
    Width = 369
    Height = 17
    Caption = 'Save sorbate concentration (SORBATE)'
    Enabled = False
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
        Control = rgDecay
      end
      item
        Control = rgSorption
      end
      item
        Control = cbSorbate
      end>
  end
end
