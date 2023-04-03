inherited frmVerticalExaggeration: TfrmVerticalExaggeration
  Left = 218
  Top = 249
  HelpType = htKeyword
  HelpKeyword = 'Vertical_Exaggeration_Dialog_Box'
  ActiveControl = rdeVerticalExaggeration
  Caption = 'Vertical Exaggeration'
  ClientHeight = 88
  ExplicitWidth = 438
  ExplicitHeight = 126
  PixelsPerInch = 120
  TextHeight = 18
  object lblVerticalExaggeration: TLabel
    Left = 8
    Top = 11
    Width = 151
    Height = 18
    Caption = 'Vertical exaggeration:'
  end
  object rdeVerticalExaggeration: TRbwDataEntry
    Left = 208
    Top = 8
    Width = 201
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    TabOrder = 1
    Text = '1'
    OnExit = rdeVerticalExaggerationExit
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 319
    Top = 48
    Width = 97
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 215
    Top = 48
    Width = 97
    Height = 33
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
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 111
    Top = 48
    Width = 97
    Height = 33
    HelpType = htKeyword
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnDefault: TButton
    Left = 8
    Top = 48
    Width = 97
    Height = 33
    Caption = 'Default'
    TabOrder = 4
    OnClick = btnDefaultClick
  end
end
