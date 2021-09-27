object frmEditCO3: TfrmEditCO3
  Left = 292
  Top = 116
  Width = 495
  Height = 224
  Caption = 'Calculate [CO3]'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Times New Roman'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 19
  object Label1: TLabel
    Left = 392
    Top = 18
    Width = 66
    Height = 19
    Caption = 'K HCO3-'
  end
  object Label2: TLabel
    Left = 392
    Top = 50
    Width = 60
    Height = 19
    Caption = '[HCO3-]'
  end
  object Label3: TLabel
    Left = 392
    Top = 82
    Width = 20
    Height = 19
    Caption = 'pH'
  end
  object adeKHCO3: TArgusDataEntry
    Left = 192
    Top = 16
    Width = 193
    Height = 22
    Hint = 'equilibrium constant of bicarbonate'
    ItemHeight = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = '4'
    OnChange = adeKHCO3Change
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object adeHCO3: TArgusDataEntry
    Left = 192
    Top = 48
    Width = 193
    Height = 22
    Hint = 'molar concentration of bicarbonate'
    ItemHeight = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '0'
    OnChange = adeKHCO3Change
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object adePH: TArgusDataEntry
    Left = 192
    Top = 80
    Width = 193
    Height = 22
    Hint = 'acidity of solution'
    ItemHeight = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Text = '5'
    OnChange = adeKHCO3Change
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rgTemperature: TRadioGroup
    Left = 8
    Top = 8
    Width = 177
    Height = 105
    Caption = 'Temperature (Celsius)'
    Columns = 2
    ItemIndex = 4
    Items.Strings = (
      '0'
      '5'
      '10'
      '15'
      '20'
      '25'
      '30'
      '45'
      '60'
      'Other')
    TabOrder = 0
    OnClick = rgTemperatureClick
  end
  object btnHelp: TBitBtn
    Left = 224
    Top = 120
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkHelp
  end
  object btnCancel: TBitBtn
    Left = 304
    Top = 120
    Width = 75
    Height = 25
    TabOrder = 5
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 384
    Top = 120
    Width = 75
    Height = 25
    TabOrder = 6
    OnClick = btnOKClick
    Kind = bkOK
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 160
    Width = 477
    Height = 19
    Panels = <>
    SimplePanel = True
  end
end
