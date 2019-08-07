inherited framePackageLakMf6: TframePackageLakMf6
  Height = 514
  ExplicitHeight = 514
  DesignSize = (
    422
    514)
  object lblSurfaceDepressionDepth: TLabel [2]
    Left = 16
    Top = 439
    Width = 171
    Height = 13
    Caption = 'Surface depression depth (surfdep)'
  end
  inherited memoComments: TMemo
    Top = 58
    Height = 301
    ExplicitTop = 58
    ExplicitHeight = 301
  end
  object cbPrintStage: TCheckBox [4]
    Left = 16
    Top = 365
    Width = 145
    Height = 20
    Caption = 'Print stage'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveBudget: TCheckBox [5]
    Left = 16
    Top = 416
    Width = 305
    Height = 20
    Caption = 'Save binary lake budget file (.lk_bud)'
    Enabled = False
    TabOrder = 2
  end
  object cbSaveStage: TCheckBox [6]
    Left = 16
    Top = 391
    Width = 321
    Height = 20
    Caption = 'Save binary Lake stage file (.lk_stg)'
    Enabled = False
    TabOrder = 3
  end
  object rdeSurfaceDepressionDepth: TRbwDataEntry [7]
    Left = 16
    Top = 458
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
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
      end>
  end
end
