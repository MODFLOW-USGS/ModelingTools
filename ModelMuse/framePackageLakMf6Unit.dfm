inherited framePackageLakMf6: TframePackageLakMf6
  Height = 514
  ExplicitHeight = 514
  DesignSize = (
    422
    514)
  object lblSurfaceDepressionDepth: TLabel [2]
    Left = 16
    Top = 369
    Width = 184
    Height = 15
    Caption = 'Surface depression depth (surfdep)'
  end
  object lblMaxIterations: TLabel [3]
    Left = 143
    Top = 419
    Width = 303
    Height = 15
    Caption = 'Maximum number of iterations (MAXIMUM_ITERATIONS)'
  end
  object lblConvergence: TLabel [4]
    Left = 16
    Top = 441
    Width = 334
    Height = 15
    Caption = 'Lake stage convergence criterion (MAXIMUM_STAGE_CHANGE)'
  end
  inherited memoComments: TMemo
    Top = 58
    Height = 207
    ExplicitTop = 58
    ExplicitHeight = 207
  end
  object cbPrintStage: TCheckBox [6]
    Left = 16
    Top = 271
    Width = 145
    Height = 20
    Caption = 'Print stage'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveBudget: TCheckBox [7]
    Left = 16
    Top = 322
    Width = 305
    Height = 20
    Caption = 'Save binary lake budget file (.lk_bud)'
    Enabled = False
    TabOrder = 3
  end
  object cbSaveStage: TCheckBox [8]
    Left = 16
    Top = 297
    Width = 321
    Height = 20
    Caption = 'Save binary Lake stage file (.lk_stg)'
    Enabled = False
    TabOrder = 2
  end
  object rdeSurfaceDepressionDepth: TRbwDataEntry [9]
    Left = 16
    Top = 388
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0.2'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbPackageConvergence: TCheckBox [10]
    Left = 16
    Top = 488
    Width = 455
    Height = 18
    Caption = 'Write package convergence (PACKAGE_CONVERGENCE)'
    Enabled = False
    TabOrder = 6
  end
  object cbSaveBudgetCsv: TCheckBox [11]
    Left = 16
    Top = 348
    Width = 403
    Height = 20
    Caption = 'Save comma-separated lake budget file (.lk_bud.csv)'
    Enabled = False
    TabOrder = 4
  end
  object seMaxIterations: TJvSpinEdit [12]
    Left = 16
    Top = 416
    Width = 121
    Height = 23
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 7
  end
  object rdeConvergence: TRbwDataEntry [13]
    Left = 16
    Top = 460
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 8
    Text = '0.2'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
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
        Control = cbPrintStage
      end
      item
        Control = cbSaveStage
      end
      item
        Control = cbSaveBudget
      end
      item
        Control = rdeSurfaceDepressionDepth
      end
      item
        Control = cbPackageConvergence
      end
      item
        Control = cbSaveBudgetCsv
      end
      item
        Control = seMaxIterations
      end
      item
        Control = rdeConvergence
      end>
  end
end
