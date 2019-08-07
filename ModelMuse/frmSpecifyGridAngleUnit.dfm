inherited frmSpecifyGridAngle: TfrmSpecifyGridAngle
  Left = 546
  Top = 416
  Width = 319
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  ActiveControl = cbSpecifyGridAngle
  Caption = 'Grid Creation'
  PixelsPerInch = 96
  object cbSpecifyGridAngle: TCheckBox
    Left = 8
    Top = 8
    Width = 305
    Height = 31
    Caption = 'Calculate Grid Angle Automatically'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = cbSpecifyGridAngleClick
  end
  object adeGridAngle: TArgusDataEntry
    Left = 8
    Top = 40
    Width = 101
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    Color = clNone
    Enabled = False
    TabOrder = 1
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
  end
  object Label1: TLabel
    Left = 120
    Top = 44
    Width = 84
    Height = 21
    Caption = 'Grid Angle'
  end
  object BitBtn1: TBitBtn
    Left = 224
    Top = 232
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 128
    Top = 232
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 4
    OnClick = btnOKClick
    Kind = bkOK
  end
  object cbSmoothGrid: TCheckBox
    Left = 8
    Top = 72
    Width = 153
    Height = 31
    Caption = 'Smooth Grid '
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbSmoothGridClick
  end
  object cbColumns: TCheckBox
    Left = 8
    Top = 104
    Width = 100
    Height = 30
    Caption = 'Columns'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object cbRows: TCheckBox
    Left = 8
    Top = 136
    Width = 100
    Height = 30
    Caption = 'Rows'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object cbLayers: TCheckBox
    Left = 8
    Top = 168
    Width = 100
    Height = 30
    Caption = 'Layers'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object lblCriterion: TLabel
    Left = 0
    Top = 196
    Width = 197
    Height = 21
    Caption = 'Grid Smoothing Criterion'
  end
  object adeCriterion: TArgusDataEntry
    Left = 211
    Top = 192
    Width = 100
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    Color = clWhite
    TabOrder = 10
    Text = '1.2'
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
  end
end
