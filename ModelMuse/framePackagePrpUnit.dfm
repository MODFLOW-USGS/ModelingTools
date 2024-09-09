inherited framePackagePrp: TframePackagePrp
  Width = 577
  Height = 513
  ExplicitWidth = 577
  ExplicitHeight = 513
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object LblSolverTolerance: TLabel [2]
    Left = 104
    Top = 181
    Width = 225
    Height = 15
    Caption = 'Solver Tolerance (EXIT_SOLVE_TOLERANCE)'
  end
  inherited memoComments: TMemo
    Width = 546
    StyleElements = [seFont, seClient, seBorder]
  end
  object RdeSolverTolerance: TRbwDataEntry [4]
    Left = 104
    Top = 200
    Width = 145
    Height = 22
    TabOrder = 1
    Text = 'RdeSolverTolerance'
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object EXTEND_TRACKING: TCheckBox [5]
    Left = 360
    Top = 224
    Width = 97
    Height = 17
    Caption = 'EXTEND_TRACKING'
    TabOrder = 2
  end
  object ComboBox1: TComboBox [6]
    Left = 104
    Top = 240
    Width = 145
    Height = 23
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'None'
    Items.Strings = (
      'None'
      'Binary'
      'CSV (Comma-Separated Values)'
      'Binary and CSV')
  end
end
