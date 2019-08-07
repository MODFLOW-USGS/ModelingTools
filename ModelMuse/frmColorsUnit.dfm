inherited frmColors: TfrmColors
  Left = 875
  Top = 432
  HelpType = htKeyword
  HelpKeyword = '3D_Lighting_Controls_Dialog_Box'
  ActiveControl = rdeAmb
  Caption = '3D Lighting Controls'
  ClientHeight = 175
  ClientWidth = 294
  FormStyle = fsStayOnTop
  OnHide = FormHide
  ExplicitWidth = 312
  ExplicitHeight = 220
  PixelsPerInch = 120
  TextHeight = 18
  object lblAmbient: TLabel
    Left = 152
    Top = 40
    Width = 58
    Height = 18
    Caption = 'Ambient'
  end
  object lblDiffuse: TLabel
    Left = 152
    Top = 72
    Width = 49
    Height = 18
    Caption = 'Diffuse'
  end
  object lblSpecular: TLabel
    Left = 152
    Top = 104
    Width = 62
    Height = 18
    Caption = 'Specular'
  end
  object lblLightPosition: TLabel
    Left = 8
    Top = 8
    Width = 95
    Height = 18
    Caption = 'Light Position'
  end
  object lblX: TLabel
    Left = 16
    Top = 32
    Width = 11
    Height = 18
    Caption = 'X'
  end
  object lblY: TLabel
    Left = 16
    Top = 72
    Width = 9
    Height = 18
    Caption = 'Y'
  end
  object lblZ: TLabel
    Left = 16
    Top = 104
    Width = 9
    Height = 18
    Caption = 'Z'
  end
  object lblLightIntensity: TLabel
    Left = 184
    Top = 8
    Width = 93
    Height = 18
    Caption = 'Light Intensity'
  end
  object rdeAmb: TRbwDataEntry
    Left = 224
    Top = 32
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 3
    Text = '0.3'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeDiff: TRbwDataEntry
    Left = 224
    Top = 64
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 4
    Text = '0.002'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeSpec: TRbwDataEntry
    Left = 224
    Top = 96
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 5
    Text = '0.015'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeX: TRbwDataEntry
    Left = 40
    Top = 32
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 0
    Text = '60'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeY: TRbwDataEntry
    Left = 40
    Top = 64
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 1
    Text = '60'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeZ: TRbwDataEntry
    Left = 40
    Top = 96
    Width = 65
    Height = 28
    Cursor = crIBeam
    TabOrder = 2
    Text = '40'
    OnChange = rdeValuesChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnClose: TBitBtn
    Left = 200
    Top = 136
    Width = 89
    Height = 33
    Kind = bkClose
    NumGlyphs = 2
    TabOrder = 8
  end
  object btnApply: TButton
    Left = 104
    Top = 136
    Width = 89
    Height = 33
    Caption = 'Apply'
    TabOrder = 7
    OnClick = btnApplyClick
  end
  object btnHelp: TBitBtn
    Left = 8
    Top = 136
    Width = 89
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnHelpClick
  end
end
