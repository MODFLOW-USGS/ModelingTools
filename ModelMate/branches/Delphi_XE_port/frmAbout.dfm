object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About ModelMate'
  ClientHeight = 244
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 81
    Top = 17
    Width = 141
    Height = 19
    Caption = 'ModelMate version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -16
    Font.Name = 'Souvenir Medium'
    Font.Style = []
    ParentFont = False
  end
  object lblSSKVer: TLabel
    Left = 244
    Top = 17
    Width = 90
    Height = 18
    Caption = 'lblSSKVer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Lucida Sans Typewriter'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object JvHTLabel1: TJvHTLabel
    Left = 71
    Top = 112
    Width = 106
    Height = 17
    Hint = 'mailto: erbanta@usgs.gov'
    Caption = 
      '<a href="mailto:erbanta@usgs.gov?subject=About%20ModelMate">erba' +
      'nta@usgs.gov</a>'
    ParentShowHint = False
    ShowHint = True
  end
  object JvHTLabel2: TJvHTLabel
    Left = 15
    Top = 172
    Width = 395
    Height = 17
    Caption = 
      '<a href="http://water.usgs.gov/software/lists/groundwater/#gui-a' +
      'nd-postproc">http://water.usgs.gov/software/lists/groundwater/#g' +
      'ui-and-postproc</a>'
  end
  object Label2: TLabel
    Left = 15
    Top = 150
    Width = 292
    Height = 16
    Caption = 'Check for ModelMate updates at the following URL:'
  end
  object btnOK: TButton
    Left = 170
    Top = 213
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object Memo1: TMemo
    Left = 64
    Top = 46
    Width = 279
    Height = 64
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      ' This is a beta-test version of ModelMate.'
      ' Please send suggestions for improvements to:'
      ''
      ' Ned Banta, U.S. Geological Survey'
      ' ')
    ReadOnly = True
    TabOrder = 1
  end
end
