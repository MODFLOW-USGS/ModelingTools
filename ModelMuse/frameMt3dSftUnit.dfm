inherited frameMt3dSftPkg: TframeMt3dSftPkg
  Height = 355
  ExplicitHeight = 355
  DesignSize = (
    422
    355)
  object lblTimeWeightingFactor: TLabel [2]
    Left = 167
    Top = 211
    Width = 209
    Height = 13
    Caption = 'Stream solver time weighting factor (WIMP)'
  end
  object lblSpaceWeightingFactor: TLabel [3]
    Left = 167
    Top = 239
    Width = 218
    Height = 13
    Caption = 'Stream solver space weighting factor (WUPS)'
  end
  object lblClosureCriterion: TLabel [4]
    Left = 167
    Top = 267
    Width = 140
    Height = 13
    Caption = 'Closure criterion (CCLOSESF)'
  end
  object lblMaxIterations: TLabel [5]
    Left = 167
    Top = 295
    Width = 204
    Height = 13
    Caption = 'Maximum number of iterations (MXITERSF)'
  end
  object lblSolverPrintChoice: TLabel [6]
    Left = 167
    Top = 322
    Width = 150
    Height = 13
    Caption = 'Solution information (IPRTXMD)'
  end
  object cbEvaporateMass: TCheckBox [8]
    Left = 16
    Top = 185
    Width = 391
    Height = 17
    Caption = 'Lose solute mass through evaporation (IETSFR)'
    Enabled = False
    TabOrder = 1
  end
  object rdeTimeWeightingFactor: TRbwDataEntry [9]
    Left = 16
    Top = 208
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSpaceWeightingFactor: TRbwDataEntry [10]
    Left = 16
    Top = 236
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeClosureCriterion: TRbwDataEntry [11]
    Left = 16
    Top = 264
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
  object seMaxIterations: TJvSpinEdit [12]
    Left = 16
    Top = 292
    Width = 145
    Height = 21
    CheckOptions = [coCheckOnExit, coCropBeyondLimit]
    CheckMaxValue = False
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 5
  end
  object comboPrintChoice: TJvImageComboBox [13]
    Left = 16
    Top = 319
    Width = 145
    Height = 23
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 145
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 17
    ItemIndex = -1
    TabOrder = 6
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print none'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print summary'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Print details'
      end>
  end
  object cbSimulateStreamTransport: TCheckBox [14]
    Left = 16
    Top = 162
    Width = 391
    Height = 17
    Caption = 'Simulate solute transport though stream'
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
        Control = cbEvaporateMass
      end
      item
        Control = rdeTimeWeightingFactor
      end
      item
        Control = rdeSpaceWeightingFactor
      end
      item
        Control = rdeClosureCriterion
      end
      item
        Control = seMaxIterations
      end
      item
        Control = comboPrintChoice
      end>
  end
end
