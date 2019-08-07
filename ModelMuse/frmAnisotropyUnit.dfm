inherited frmAnisotropy: TfrmAnisotropy
  Width = 222
  Height = 93
  HorzScrollBar.Range = 0
  VertScrollBar.Range = 0
  Caption = 'Anisotropy'
  PixelsPerInch = 108
  object adeAnisotropy: TArgusDataEntry
    Left = 8
    Top = 8
    Width = 100
    Height = 28
    Alignment = taRightJustify
    TabOrder = 0
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
    EnabledColor = clNone
  end
  object Label1: TLabel
    Left = 120
    Top = 12
    Width = 78
    Height = 20
    Caption = 'Anisotropy'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 48
    Width = 97
    Height = 33
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 120
    Top = 48
    Width = 89
    Height = 33
    TabOrder = 3
    Kind = bkCancel
  end
end
