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
    Height = 259
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 490
    ExplicitHeight = 259
  end
  object cbPrintHeads: TCheckBox [5]
    Left = 16
    Top = 325
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Print MAW heads to listing file (PRINT_HEAD)'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveHeads: TCheckBox [6]
    Left = 16
    Top = 348
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW heads file (.maw_head)'
    Enabled = False
    TabOrder = 2
  end
  object cbSaveFlows: TCheckBox [7]
    Left = 16
    Top = 371
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW budget file (.maw_bud)'
    Enabled = False
    TabOrder = 3
  end
  object cbIncludeWellStorage: TCheckBox [8]
    Left = 16
    Top = 430
    Width = 449
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Include well storage (inverse of NO_WELL_STORAGE)'
    Enabled = False
    TabOrder = 4
    ExplicitTop = 229
  end
  object rdeShutDownTheta: TRbwDataEntry [9]
    Left = 16
    Top = 453
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 6
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
    ExplicitTop = 252
  end
  object rdeShutDownKappa: TRbwDataEntry [10]
    Left = 16
    Top = 481
    Width = 145
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
    ExplicitTop = 280
  end
  object cbBudgetCsv: TCheckBox [11]
    Left = 16
    Top = 391
    Width = 490
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save comma-separated values MAW budget file (.maw_bud.csv)'
    Enabled = False
    TabOrder = 7
  end
  object cbFlowCorrection: TCheckBox [12]
    Left = 16
    Top = 412
    Width = 449
    Height = 17
    Caption = 'Make flow corrections (FLOW_CORRECTION)'
    Enabled = False
    TabOrder = 8
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
      end>
  end
end
