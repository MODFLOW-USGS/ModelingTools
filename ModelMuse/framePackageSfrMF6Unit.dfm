inherited framePackageSfrMF6: TframePackageSfrMF6
  Width = 427
  Height = 377
  ExplicitWidth = 427
  ExplicitHeight = 377
  DesignSize = (
    427
    377)
  object lblMaxIterations: TLabel [2]
    Left = 167
    Top = 277
    Width = 203
    Height = 26
    Caption = 'Maximum number of streamflow iterations (maximum_iteration) '
    WordWrap = True
  end
  object lblMaxDepthChange: TLabel [3]
    Left = 167
    Top = 325
    Width = 150
    Height = 26
    Caption = 'Stream depth  closure criterion (maximum_depth_change)'
    WordWrap = True
  end
  inherited memoComments: TMemo
    Width = 396
    ExplicitWidth = 396
  end
  object cbSaveStage: TCheckBox [5]
    Left = 16
    Top = 220
    Width = 313
    Height = 20
    Caption = 'Save stage (stagefile)'
    Enabled = False
    TabOrder = 2
  end
  object cbSaveBudget: TCheckBox [6]
    Left = 16
    Top = 251
    Width = 337
    Height = 20
    Caption = 'Save budget (budgetfile)'
    Enabled = False
    TabOrder = 3
  end
  object seMaxIterations: TJvSpinEdit [7]
    Left = 16
    Top = 282
    Width = 145
    Height = 21
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 5
  end
  object rdeMaxDepthChange: TRbwDataEntry [8]
    Left = 16
    Top = 325
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbPrintStage: TCheckBox [9]
    Left = 16
    Top = 157
    Width = 249
    Height = 17
    Caption = 'Print stage (PRINT_STAGE)'
    Enabled = False
    TabOrder = 1
  end
  object cbPrintFlows: TCheckBox [10]
    Left = 16
    Top = 187
    Width = 249
    Height = 17
    Caption = 'Print flows (PRINT_FLOWS)'
    Enabled = False
    TabOrder = 6
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
        Control = cbSaveBudget
      end
      item
        Control = cbSaveStage
      end
      item
        Control = rdeMaxDepthChange
      end
      item
        Control = seMaxIterations
      end
      item
        Control = cbPrintStage
      end
      item
        Control = cbPrintFlows
      end>
  end
end
