inherited frameMt3dmsGcgPackage: TframeMt3dmsGcgPackage
  Width = 357
  Height = 393
  ExplicitWidth = 357
  ExplicitHeight = 393
  DesignSize = (
    357
    393)
  object lblMaxOuter: TLabel [2]
    Left = 92
    Top = 160
    Width = 213
    Height = 13
    Caption = 'Maximum number of outer iterations MXITER'
  end
  object lblMaxInner: TLabel [3]
    Left = 92
    Top = 187
    Width = 203
    Height = 13
    Caption = 'Maximum number of inner iterations ITER1'
  end
  object lblPreconditioner: TLabel [4]
    Left = 16
    Top = 214
    Width = 115
    Height = 13
    Caption = 'Preconditioner (ISOLVE)'
  end
  object lblDispersion: TLabel [5]
    Left = 16
    Top = 259
    Width = 172
    Height = 13
    Caption = 'Dispersion tensor treatment (NCRS)'
  end
  object lblRelaxationFactor: TLabel [6]
    Left = 92
    Top = 308
    Width = 120
    Height = 13
    Caption = 'Relaxation factor (ACCL)'
  end
  object lblConvergence: TLabel [7]
    Left = 92
    Top = 336
    Width = 156
    Height = 13
    Caption = 'Convergence criterion (CCLOSE)'
  end
  object lblPrintoutInterval: TLabel [8]
    Left = 92
    Top = 364
    Width = 126
    Height = 13
    Caption = 'Printout interval (IPRGCG)'
  end
  inherited memoComments: TMemo
    Width = 326
    ExplicitWidth = 326
  end
  object spinMaxOuter: TJvSpinEdit [10]
    Left = 16
    Top = 157
    Width = 70
    Height = 21
    CheckMaxValue = False
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 1
  end
  object spinMaxInner: TJvSpinEdit [11]
    Left = 16
    Top = 184
    Width = 70
    Height = 21
    CheckMaxValue = False
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 2
  end
  object comboPreconditioner: TComboBox [12]
    Left = 14
    Top = 233
    Width = 198
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemIndex = 2
    TabOrder = 3
    Text = 'Modified Incomplete Cholesky (3)'
    OnChange = rcSelectionControllerEnabledChange
    Items.Strings = (
      'Jacobi (1)'
      'SSOR (2)'
      'Modified Incomplete Cholesky (3)')
  end
  object comboDispersion: TComboBox [13]
    Left = 16
    Top = 278
    Width = 302
    Height = 21
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 4
    Text = 'Lump all dispersion cross terms to the right-hand-side (0)'
    Items.Strings = (
      'Lump all dispersion cross terms to the right-hand-side (0)'
      'Include full dispersion tensor (1)')
  end
  object rdeRelaxationFactor: TRbwDataEntry [14]
    Left = 16
    Top = 305
    Width = 70
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeConvergence: TRbwDataEntry [15]
    Left = 16
    Top = 333
    Width = 70
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 6
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object spinPrintoutInterval: TJvSpinEdit [16]
    Left = 16
    Top = 361
    Width = 70
    Height = 21
    CheckMinValue = True
    Enabled = False
    TabOrder = 7
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
        Control = spinMaxOuter
      end
      item
        Control = spinMaxInner
      end
      item
        Control = comboPreconditioner
      end
      item
        Control = comboDispersion
      end
      item
        Control = rdeConvergence
      end
      item
        Control = spinPrintoutInterval
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
