inherited framePackageLakMf6: TframePackageLakMf6
  Height = 514
  ExplicitHeight = 514
  DesignSize = (
    422
    514)
  object lblSurfaceDepressionDepth: TLabel [2]
    Left = 16
    Top = 441
    Width = 184
    Height = 15
    Caption = 'Surface depression depth (surfdep)'
  end
  inherited memoComments: TMemo
    Top = 58
    Height = 279
    ExplicitTop = 58
    ExplicitHeight = 279
  end
  object cbPrintStage: TCheckBox [4]
    Left = 16
    Top = 343
    Width = 145
    Height = 20
    Caption = 'Print stage'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveBudget: TCheckBox [5]
    Left = 16
    Top = 394
    Width = 305
    Height = 20
    Caption = 'Save binary lake budget file (.lk_bud)'
    Enabled = False
    TabOrder = 3
  end
  object cbSaveStage: TCheckBox [6]
    Left = 16
    Top = 369
    Width = 321
    Height = 20
    Caption = 'Save binary Lake stage file (.lk_stg)'
    Enabled = False
    TabOrder = 2
  end
  object rdeSurfaceDepressionDepth: TRbwDataEntry [7]
    Left = 16
    Top = 460
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
  object cbPackageConvergence: TCheckBox [8]
    Left = 16
    Top = 488
    Width = 455
    Height = 18
    Caption = 'Write package convergence (PACKAGE_CONVERGENCE)'
    Enabled = False
    TabOrder = 6
  end
  object cbSaveBudgetCsv: TCheckBox [9]
    Left = 16
    Top = 420
    Width = 403
    Height = 20
    Caption = 'Save comma-separated lake budget file (.lk_bud.csv)'
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
      end>
  end
end
