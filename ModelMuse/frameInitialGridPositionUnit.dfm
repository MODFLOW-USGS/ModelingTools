object frameInitialGridPosition: TframeInitialGridPosition
  Left = 0
  Top = 0
  Width = 335
  Height = 150
  TabOrder = 0
  TabStop = True
  object lblGridAngle: TLabel
    Left = 208
    Top = 44
    Width = 109
    Height = 15
    Caption = 'Grid angle (degrees) '
  end
  object lblVerticalExaggeration: TLabel
    Left = 208
    Top = 84
    Width = 110
    Height = 15
    Caption = 'Vertical exaggeration'
  end
  object lblGridOrigin: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 15
    Caption = 'Grid origin:'
  end
  object lblOriginX: TLabel
    Left = 88
    Top = 44
    Width = 7
    Height = 15
    Caption = 'X'
  end
  object lblOriginY: TLabel
    Left = 88
    Top = 84
    Width = 7
    Height = 15
    Caption = 'Y'
  end
  object lblOriginZ: TLabel
    Left = 88
    Top = 124
    Width = 7
    Height = 15
    Caption = 'Z'
  end
  object rdeAngle: TRbwDataEntry
    Left = 128
    Top = 40
    Width = 73
    Height = 28
    Cursor = crIBeam
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeExaggeration: TRbwDataEntry
    Left = 128
    Top = 80
    Width = 73
    Height = 28
    Cursor = crIBeam
    TabOrder = 4
    Text = '20'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeX: TRbwDataEntry
    Left = 8
    Top = 40
    Width = 73
    Height = 28
    Cursor = crIBeam
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeY: TRbwDataEntry
    Left = 8
    Top = 80
    Width = 73
    Height = 28
    Cursor = crIBeam
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeZ: TRbwDataEntry
    Left = 8
    Top = 120
    Width = 73
    Height = 28
    Cursor = crIBeam
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
end
