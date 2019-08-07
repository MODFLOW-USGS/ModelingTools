object frameRulerOptions: TframeRulerOptions
  Left = 0
  Top = 0
  Width = 320
  Height = 145
  TabOrder = 0
  TabStop = True
  object lblPreview: TLabel
    Left = 208
    Top = 126
    Width = 6
    Height = 13
    Caption = '0'
  end
  object lblPreviewLabel: TLabel
    Left = 3
    Top = 126
    Width = 42
    Height = 13
    Caption = 'Preview:'
  end
  object lblSampleNumber: TLabel
    Left = 3
    Top = 97
    Width = 73
    Height = 13
    Caption = 'Sample number'
  end
  object lblPrecision: TLabel
    Left = 3
    Top = 42
    Width = 42
    Height = 13
    Caption = 'Precision'
  end
  object lblDigits: TLabel
    Left = 3
    Top = 69
    Width = 41
    Height = 13
    Caption = 'Decimals'
  end
  object lblSpacing: TLabel
    Left = 3
    Top = 16
    Width = 132
    Height = 13
    Caption = 'Desired tick spacing (pixels)'
  end
  object rdePreviewNumber: TRbwDataEntry
    Left = 208
    Top = 94
    Width = 101
    Height = 21
    Cursor = crIBeam
    ItemHeight = 13
    TabOrder = 0
    Text = '123456789'
    OnChange = rdePreviewNumberChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object sePrecision: TJvSpinEdit
    Left = 208
    Top = 39
    Width = 101
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 15.000000000000000000
    MinValue = 1.000000000000000000
    Value = 5.000000000000000000
    TabOrder = 1
    OnChange = sePrecisionChange
  end
  object seDigits: TJvSpinEdit
    Left = 208
    Top = 66
    Width = 101
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 4.000000000000000000
    TabOrder = 2
    OnChange = sePrecisionChange
  end
  object seSpacing: TJvSpinEdit
    Left = 208
    Top = 13
    Width = 101
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 100000000.000000000000000000
    MinValue = 1.000000000000000000
    Value = 5.000000000000000000
    TabOrder = 3
    OnChange = sePrecisionChange
  end
end
