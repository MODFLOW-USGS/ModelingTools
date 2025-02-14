inherited framePackageEst: TframePackageEst
  Width = 570
  Height = 367
  ExplicitWidth = 570
  ExplicitHeight = 367
  DesignSize = (
    570
    367)
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object lblDensityWater: TLabel [2]
    Left = 167
    Top = 206
    Width = 231
    Height = 15
    Caption = 'Density of water (DENSITY_WATER)'
  end
  object lblHeatCapacityWater: TLabel [3]
    Left = 167
    Top = 234
    Width = 258
    Height = 15
    Caption = 'Heat capacity of water (HEAT_CAPACITY_WATER)'
  end
  object lblLatentHeat: TLabel [4]
    Left = 167
    Top = 262
    Width = 311
    Height = 15
    Caption = 'Latent heat of vaporization (LATENT_HEAT_VAPORIZATION)'
    WordWrap = True
  end
  inherited memoComments: TMemo
    Width = 539
    StyleElements = [seFont, seClient, seBorder]
  end
  object cbZeroOrderDecayWater: TCheckBox [6]
    Left = 16
    Top = 157
    Width = 539
    Height = 17
    Caption = 'Zero-order decay in aqueous phase (ZERO_ORDER_DECAY_WATER)'
    Enabled = False
    TabOrder = 1
  end
  object cbZeroOrderDecaySolute: TCheckBox [7]
    Left = 16
    Top = 180
    Width = 539
    Height = 17
    Caption = 'Zero-order decay in solid phase (ZERO_ORDER_DECAY_SOLID)'
    Enabled = False
    TabOrder = 2
  end
  object rdeDensityWater: TRbwDataEntry [8]
    Left = 16
    Top = 203
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeHeatCapacityWater: TRbwDataEntry [9]
    Left = 16
    Top = 231
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
  object rdeLatentHeat: TRbwDataEntry [10]
    Left = 16
    Top = 259
    Width = 145
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
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = cbZeroOrderDecayWater
      end
      item
        Control = cbZeroOrderDecaySolute
      end
      item
        Control = rdeDensityWater
      end
      item
        Control = rdeHeatCapacityWater
      end
      item
        Control = rdeLatentHeat
      end>
  end
end
