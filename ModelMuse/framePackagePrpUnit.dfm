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
    Left = 25
    Top = 161
    Width = 225
    Height = 15
    Caption = 'Solver Tolerance (EXIT_SOLVE_TOLERANCE)'
  end
  object lblPrpTrack: TLabel [3]
    Left = 25
    Top = 210
    Width = 68
    Height = 15
    Caption = 'Track Output'
  end
  inherited memoComments: TMemo
    Width = 546
    StyleElements = [seFont, seClient, seBorder]
  end
  object RdeSolverTolerance: TRbwDataEntry [5]
    Left = 25
    Top = 182
    Width = 145
    Height = 22
    TabOrder = 1
    Text = 'RdeSolverTolerance'
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object EXTEND_TRACKING: TCheckBox [6]
    Left = 360
    Top = 224
    Width = 97
    Height = 17
    Caption = 'EXTEND_TRACKING'
    TabOrder = 2
  end
  object ComboBox1: TComboBox [7]
    Left = 25
    Top = 231
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
  inline frameStopTime: TframeOptionalValue [8]
    Left = 3
    Top = 260
    Width = 176
    Height = 56
    TabOrder = 4
    ExplicitLeft = 3
    ExplicitTop = 260
    inherited LblVariableLabel: TLabel
      Width = 117
      Caption = 'Stop Time (STOPTIME)'
      StyleElements = [seFont, seClient, seBorder]
      ExplicitWidth = 117
    end
    inherited RdeValue: TRbwDataEntry
      StyleElements = [seFont, seClient, seBorder]
    end
  end
  inherited rcSelectionController: TRbwController
    Left = 256
    Top = 32
  end
end
