inherited framePackageLAK: TframePackageLAK
  Height = 323
  ExplicitHeight = 323
  DesignSize = (
    422
    323)
  object lblTheta: TLabel [1]
    Left = 120
    Top = 146
    Width = 29
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Theta'
    Enabled = False
  end
  object lblIterations: TLabel [2]
    Left = 120
    Top = 174
    Width = 214
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Maximum number of iterations (NSSITR)'
    Enabled = False
  end
  object lblConvergenceCriterion: TLabel [3]
    Left = 120
    Top = 202
    Width = 172
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Convergence criterion (SSCNCR)'
    Enabled = False
  end
  object lblSurfDepth: TLabel [5]
    Left = 120
    Top = 227
    Width = 257
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Height of lake bottom undulations (SURFDEPTH)'
    Enabled = False
  end
  inherited memoComments: TMemo
    Height = 75
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 75
  end
  object rdeTheta: TRbwDataEntry [7]
    Left = 16
    Top = 143
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 1
    Text = '0.5'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeIterations: TRbwDataEntry [8]
    Left = 16
    Top = 171
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '1'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeConvergenceCriterion: TRbwDataEntry [9]
    Left = 16
    Top = 199
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbPrintLake: TCheckBox [10]
    Left = 16
    Top = 252
    Width = 217
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Print lake output (LWRT)'
    Enabled = False
    TabOrder = 4
  end
  object rdeSurfDepth: TRbwDataEntry [11]
    Left = 16
    Top = 224
    Width = 98
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rgBathymetry: TRadioGroup [12]
    Left = 16
    Top = 272
    Width = 281
    Height = 49
    Anchors = [akLeft, akBottom]
    Caption = 'Lake bathymetry (TABLEINPUT)'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Define automatically'
      'Specify bathymetry table for each lake')
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
        Control = cbPrintLake
      end
      item
        Control = rdeIterations
      end
      item
        Control = rdeConvergenceCriterion
      end
      item
        Control = rdeTheta
      end
      item
        Control = lblConvergenceCriterion
      end
      item
        Control = lblIterations
      end
      item
        Control = lblTheta
      end
      item
        Control = rdeSurfDepth
      end
      item
        Control = lblSurfDepth
      end
      item
        Control = rgBathymetry
      end>
  end
end
