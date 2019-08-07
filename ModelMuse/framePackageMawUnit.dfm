inherited framePackageMaw: TframePackageMaw
  Width = 480
  Height = 312
  ExplicitWidth = 480
  ExplicitHeight = 312
  DesignSize = (
    480
    312)
  object lblShutDownTheta: TLabel [2]
    Left = 176
    Top = 255
    Width = 77
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Shutdown theta'
  end
  object lblShutDownKappa: TLabel [3]
    Left = 176
    Top = 283
    Width = 80
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Shutdown kappa'
  end
  inherited memoComments: TMemo
    Width = 449
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 449
  end
  object cbPrintHeads: TCheckBox [5]
    Left = 16
    Top = 160
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Print MAW heads to listing file (PRINT_HEAD)'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveHeads: TCheckBox [6]
    Left = 16
    Top = 183
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW heads file (.maw_head)'
    Enabled = False
    TabOrder = 2
  end
  object cbSaveFlows: TCheckBox [7]
    Left = 16
    Top = 206
    Width = 391
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Save binary MAW budget file (.maw_bud)'
    Enabled = False
    TabOrder = 3
  end
  object cbIncludeWellStorage: TCheckBox [8]
    Left = 16
    Top = 229
    Width = 449
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Include well storage (inverse of NO_WELL_STORAGE)'
    Enabled = False
    TabOrder = 4
  end
  object rdeShutDownTheta: TRbwDataEntry [9]
    Left = 16
    Top = 252
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
  end
  object rdeShutDownKappa: TRbwDataEntry [10]
    Left = 16
    Top = 280
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
      end>
  end
end
