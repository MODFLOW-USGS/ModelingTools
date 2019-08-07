inherited frmFluidProperties: TfrmFluidProperties
  Width = 316
  Height = 226
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  ActiveControl = adeCompressibility
  Caption = 'Fluid Properties'
  PixelsPerInch = 96
  object adeCompressibility: TRbwDataEntry
    Left = 208
    Top = 8
    Width = 100
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    TabOrder = 0
    Text = '4.7e-10'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
  end
  object adeDensity: TRbwDataEntry
    Left = 208
    Top = 40
    Width = 100
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    TabOrder = 1
    Text = '1000'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
  end
  object adeDiffusivity: TRbwDataEntry
    Left = 208
    Top = 72
    Width = 100
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    TabOrder = 2
    Text = '1e-9'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
  end
  object adeViscosity: TRbwDataEntry
    Left = 208
    Top = 104
    Width = 100
    Height = 29
    Cursor = crIBeam
    Alignment = taRightJustify
    TabOrder = 3
    Text = '0.00115'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 175
    Height = 21
    Caption = 'Compressibility (1/Pa)'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 132
    Height = 21
    Caption = 'Density (kg/m^3)'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 139
    Height = 21
    Caption = 'Diffusivity (m^2/s)'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 143
    Height = 21
    Caption = 'Viscosity (Pa-sec)'
  end
  object btnOK: TBitBtn
    Left = 120
    Top = 192
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 8
    OnClick = btnOKClick
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 216
    Top = 192
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 9
    Kind = bkCancel
  end
  object Label5: TLabel
    Left = 8
    Top = 136
    Width = 283
    Height = 42
    Caption = 
      'Note: In Release Candidate 3, fluid properties can no longer be ' +
      'set.'
    WordWrap = True
  end
end
