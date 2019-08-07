object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  TabOrder = 0
  object SpinEdit1: TSpinEdit
    Left = 24
    Top = 24
    Width = 101
    Height = 23
    Max = 15
    Min = 1
    TabOrder = 0
    Value = 1
  end
  object SpinEdit2: TSpinEdit
    Left = 24
    Top = 56
    Width = 101
    Height = 23
    Max = 20
    Min = 1
    TabOrder = 1
    Value = 1
  end
  object Label1: TLabel
    Left = 136
    Top = 27
    Width = 56
    Height = 16
    Caption = 'Precision'
  end
  object Label2: TLabel
    Left = 136
    Top = 59
    Width = 34
    Height = 16
    Caption = 'Digits'
  end
  object ArgusDataEntry1: TArgusDataEntry
    Left = 24
    Top = 88
    Width = 101
    Height = 24
    Alignment = taRightJustify
    TabOrder = 4
    Text = '0'
    DataType = dtReal
    Max = 1
    ChangeDisabledColor = True
    EnabledColor = clNone
  end
  object Label3: TLabel
    Left = 136
    Top = 92
    Width = 95
    Height = 16
    Caption = 'Sample number'
  end
  object Label4: TLabel
    Left = 136
    Top = 120
    Width = 48
    Height = 16
    Caption = 'Preview'
  end
  object lblPreview: TLabel
    Left = 24
    Top = 120
    Width = 7
    Height = 16
    Caption = '0'
  end
end
