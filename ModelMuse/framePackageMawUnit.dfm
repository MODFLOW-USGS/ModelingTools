inherited framePackageMaw: TframePackageMaw
  Width = 521
  Height = 513
  ExplicitWidth = 521
  ExplicitHeight = 513
  DesignSize = (
    521
    513)
  object lblShutDownTheta: TLabel [2]
    Left = 176
    Top = 456
    Width = 84
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Shutdown theta'
    ExplicitTop = 255
  end
  object lblShutDownKappa: TLabel [3]
    Left = 176
    Top = 484
    Width = 89
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Shutdown kappa'
    ExplicitTop = 283
  end
  inherited memoComments: TMemo
    Width = 490
    Height = 235
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 490
    ExplicitHeight = 235
  end
  object cbPrintHeads: TCheckBox [5]
    Left = 16
    Top = 303
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Print MAW heads to listing file (PRINT_HEAD)'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveHeads: TCheckBox [6]
    Left = 16
    Top = 323
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW heads file (.maw_head)'
    Enabled = False
    TabOrder = 2
  end
  object cbSaveFlows: TCheckBox [7]
    Left = 16
    Top = 344
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW budget file (.maw_bud)'
    Enabled = False
    TabOrder = 3
  end
  object cbIncludeWellStorage: TCheckBox [8]
    Left = 16
    Top = 428
    Width = 449
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Include well storage (inverse of NO_WELL_STORAGE)'
    Enabled = False
    TabOrder = 7
  end
  object rdeShutDownTheta: TRbwDataEntry [9]
    Left = 16
    Top = 453
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 8
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeShutDownKappa: TRbwDataEntry [10]
    Left = 16
    Top = 481
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 9
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbBudgetCsv: TCheckBox [11]
    Left = 16
    Top = 365
    Width = 490
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save comma-separated values MAW budget file (.maw_bud.csv)'
    Enabled = False
    TabOrder = 4
  end
  object cbFlowCorrection: TCheckBox [12]
    Left = 16
    Top = 407
    Width = 449
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Make flow corrections (FLOW_CORRECTION)'
    Enabled = False
    TabOrder = 6
  end
  object cbFlowReduceCsv: TCheckBox [13]
    Left = 16
    Top = 386
    Width = 490
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Write flow reductions to CSV file (MAW_FLOW_REDUCE_CSV)'
    Enabled = False
    TabOrder = 5
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
        Control = cbPrintHeads
      end
      item
        Control = cbSaveHeads
      end
      item
        Control = cbSaveFlows
      end
      item
        Control = cbIncludeWellStorage
      end
      item
        Control = rdeShutDownTheta
      end
      item
        Control = rdeShutDownKappa
      end
      item
        Control = cbBudgetCsv
      end
      item
        Control = cbFlowCorrection
      end
      item
        Control = cbFlowReduceCsv
      end>
  end
end
