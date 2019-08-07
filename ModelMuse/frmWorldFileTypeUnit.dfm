inherited frmWorldFileType: TfrmWorldFileType
  Caption = 'World File Type'
  ClientHeight = 180
  ClientWidth = 318
  ExplicitWidth = 326
  ExplicitHeight = 214
  PixelsPerInch = 96
  TextHeight = 18
  object lblQuestion: TLabel
    Left = 8
    Top = 8
    Width = 204
    Height = 18
    Caption = 'What type of world file is this?'
  end
  object lnklblRaster: TJvLinkLabel
    Left = 43
    Top = 63
    Width = 262
    Height = 18
    Caption = '<link>Raster world file description<\link>'
    Text.Strings = (
      '<link>Raster world file description<\link>')
    OnLinkClick = lnklblRasterLinkClick
  end
  object lnklblCAD: TJvLinkLabel
    Left = 43
    Top = 109
    Width = 262
    Height = 18
    Caption = '<link>CAD world file description<\link>'
    Text.Strings = (
      '<link>CAD world file description<\link>')
  end
  object rbRaster: TRadioButton
    Left = 24
    Top = 40
    Width = 281
    Height = 17
    Caption = 'Raster world file'
    TabOrder = 0
  end
  object rbCAD: TRadioButton
    Left = 24
    Top = 86
    Width = 281
    Height = 17
    Caption = 'CAD world file'
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 224
    Top = 144
    Width = 81
    Height = 28
    TabOrder = 2
    Kind = bkClose
  end
end
