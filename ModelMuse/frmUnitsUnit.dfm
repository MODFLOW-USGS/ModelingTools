inherited frmUnits: TfrmUnits
  Left = 399
  Top = 49
  HelpType = htKeyword
  HelpKeyword = 'Title_and_Units_Dialog_Box'
  ActiveControl = memoTitle
  Caption = 'PHAST Title and Units'
  ClientHeight = 632
  ClientWidth = 723
  ExplicitWidth = 735
  ExplicitHeight = 670
  PixelsPerInch = 120
  TextHeight = 18
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 376
    Height = 18
    Caption = 'Title (Only the first two lines will be printed in the output)'
  end
  object lblTimeUnits: TLabel
    Left = 8
    Top = 153
    Width = 70
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Time units'
  end
  object lblHorizGridUnits: TLabel
    Left = 8
    Top = 184
    Width = 136
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Horizontal grid units'
  end
  object lblVertGridUnits: TLabel
    Left = 8
    Top = 215
    Width = 119
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Vertical grid units'
  end
  object lblHeadUnits: TLabel
    Left = 8
    Top = 246
    Width = 74
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Head units'
  end
  object lblHydraulicCondLengthUnits: TLabel
    Left = 8
    Top = 277
    Width = 184
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Hydraulic conductivity units'
  end
  object lblHydraulicCondTimeUnits: TLabel
    Left = 512
    Top = 277
    Width = 23
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'per'
  end
  object lblSpecificStorageUnits: TLabel
    Left = 8
    Top = 308
    Width = 150
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Specific storage units'
  end
  object lblDispersivityUnits: TLabel
    Left = 8
    Top = 339
    Width = 117
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Dispersivity units'
  end
  object lblFluxLengthUnits: TLabel
    Left = 8
    Top = 370
    Width = 64
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Flux units'
  end
  object lblFluxTimeUnits: TLabel
    Left = 511
    Top = 370
    Width = 23
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'per'
  end
  object lblLeakyHydCondLengthUnits: TLabel
    Left = 8
    Top = 401
    Width = 227
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Leaky hydraulic conductivity units'
  end
  object lblLeakyHydCondTimeUnits: TLabel
    Left = 512
    Top = 401
    Width = 23
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'per'
  end
  object lblLeakyThicknessUnits: TLabel
    Left = 8
    Top = 432
    Width = 147
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Leaky thickness units'
  end
  object lblWellDiameterUnits: TLabel
    Left = 8
    Top = 463
    Width = 132
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Well diameter units'
  end
  object lblWellFlowVolumeUnits: TLabel
    Left = 8
    Top = 494
    Width = 128
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Well flow rate units'
  end
  object lblWellFlowTimeUnits: TLabel
    Left = 511
    Top = 494
    Width = 23
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'per'
  end
  object lblRiverHydCondLengthUnits: TLabel
    Left = 8
    Top = 525
    Width = 252
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'River bed hydraulic conductivity units'
  end
  object lblRiverHydCondTimeUnits: TLabel
    Left = 511
    Top = 525
    Width = 23
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'per'
  end
  object lblRiverThicknessUnits: TLabel
    Left = 8
    Top = 556
    Width = 172
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'River bed thickness units'
  end
  object memoTitle: TMemo
    Left = 8
    Top = 40
    Width = 706
    Height = 104
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    ExplicitWidth = 702
  end
  object btnOK: TBitBtn
    Left = 528
    Top = 590
    Width = 89
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      04000000000068010000120B0000120B00001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
    TabOrder = 21
    OnClick = btnOKClick
    ExplicitTop = 589
  end
  object btnCancel: TBitBtn
    Left = 624
    Top = 590
    Width = 91
    Height = 33
    Anchors = [akLeft, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 22
    ExplicitTop = 589
  end
  object comboTimeUnits: TComboBox
    Left = 328
    Top = 150
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Items.Strings = (
      'seconds'
      'minutes'
      'hours'
      'days'
      'years')
    ExplicitTop = 149
  end
  object comboHorizGridUnits: TComboBox
    Left = 328
    Top = 181
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 180
  end
  object comboVertGridUnits: TComboBox
    Left = 328
    Top = 212
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 211
  end
  object comboHeadUnits: TComboBox
    Left = 328
    Top = 243
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 242
  end
  object comboHydraulicCondLengthUnits: TComboBox
    Left = 328
    Top = 274
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 273
  end
  object comboHydraulicCondTimeUnits: TComboBox
    Left = 541
    Top = 274
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    Items.Strings = (
      'second'
      'minute'
      'hour'
      'day'
      'year')
    ExplicitTop = 273
  end
  object comboSpecificStorageUnits: TComboBox
    Left = 328
    Top = 305
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    Items.Strings = (
      '1/inches'
      '1/feet'
      '1/miles'
      '1/millimeters'
      '1/centimeters'
      '1/meters'
      '1/kilometers')
    ExplicitTop = 304
  end
  object comboDispersivityUnits: TComboBox
    Left = 328
    Top = 336
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 8
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 335
  end
  object comboFluxLengthUnits: TComboBox
    Left = 328
    Top = 367
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 9
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 366
  end
  object comboFluxTimeUnits: TComboBox
    Left = 541
    Top = 367
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 10
    Items.Strings = (
      'second'
      'minute'
      'hour'
      'day'
      'year')
    ExplicitTop = 366
  end
  object comboLeakyHydCondLengthUnits: TComboBox
    Left = 328
    Top = 398
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 11
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 397
  end
  object comboLeakyHydCondTimeUnits: TComboBox
    Left = 541
    Top = 398
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 12
    Items.Strings = (
      'second'
      'minute'
      'hour'
      'day'
      'year')
    ExplicitTop = 397
  end
  object comboLeakyThicknessUnits: TComboBox
    Left = 328
    Top = 429
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 13
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 428
  end
  object comboWellDiameterUnits: TComboBox
    Left = 328
    Top = 460
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 14
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 459
  end
  object comboWellFlowVolumeUnits: TComboBox
    Left = 328
    Top = 491
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 15
    Items.Strings = (
      'gallons'
      'inches^3'
      'feet^3'
      'miles^3'
      'liters'
      'millimeters^3'
      'centimeters^3'
      'meters^3'
      'kilometers^3')
    ExplicitTop = 490
  end
  object comboWellFlowTimeUnits: TComboBox
    Left = 541
    Top = 491
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 16
    Items.Strings = (
      'second'
      'minute'
      'hour'
      'day'
      'year')
    ExplicitTop = 490
  end
  object comboRiverHydCondLengthUnits: TComboBox
    Left = 328
    Top = 522
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 17
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 521
  end
  object comboRiverHydCondTimeUnits: TComboBox
    Left = 541
    Top = 522
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 18
    Items.Strings = (
      'second'
      'minute'
      'hour'
      'day'
      'year')
    ExplicitTop = 521
  end
  object comboRiverThicknessUnits: TComboBox
    Left = 328
    Top = 553
    Width = 177
    Height = 26
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 19
    Items.Strings = (
      'inches'
      'feet'
      'miles'
      'millimeters'
      'centimeters'
      'meters'
      'kilometers')
    ExplicitTop = 552
  end
  object btnHelp: TBitBtn
    Left = 433
    Top = 590
    Width = 89
    Height = 33
    HelpType = htKeyword
    Anchors = [akLeft, akBottom]
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 20
    OnClick = btnHelpClick
    ExplicitTop = 589
  end
end
