object FormAbout: TFormAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About ModelMate'
  ClientHeight = 362
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBtnText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object Label1: TLabel
    Left = 96
    Top = 53
    Width = 173
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'ModelMate version:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -20
    Font.Name = 'Souvenir Medium'
    Font.Style = []
    ParentFont = False
  end
  object lblSSKVer: TLabel
    Left = 290
    Top = 53
    Width = 108
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'lblSSKVer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -20
    Font.Name = 'Lucida Sans Typewriter'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object JvHTLabel1: TJvHTLabel
    Left = 39
    Top = 154
    Width = 131
    Height = 19
    Hint = 'mailto: erbanta@usgs.gov'
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      '<a href="mailto:erbanta@usgs.gov?subject=About%20ModelMate">erba' +
      'nta@usgs.gov</a>'
    ParentShowHint = False
    ShowHint = True
  end
  object JvHTLabel2: TJvHTLabel
    Left = 39
    Top = 220
    Width = 290
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      '<a href="http://water.usgs.gov/software/ModelMate/">http://water' +
      '.usgs.gov/software/ModelMate/</a>'
  end
  object Label2: TLabel
    Left = 39
    Top = 201
    Width = 354
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Check for ModelMate updates at the following URL:'
  end
  object JvHTLabel3: TJvHTLabel
    Left = 39
    Top = 273
    Width = 210
    Height = 19
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      '<a href="http://pubs.usgs.gov/tm/tm6e4/">http://pubs.usgs.gov/tm' +
      '/tm6e4/</a>'
  end
  object Label3: TLabel
    Left = 39
    Top = 253
    Width = 436
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Documentation for ModelMate is available at the following URL:'
  end
  object btnOK: TButton
    Left = 202
    Top = 316
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnOKClick
  end
  object Memo1: TMemo
    Left = 39
    Top = 88
    Width = 417
    Height = 57
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'Please send suggestions for improvements to:'
      ''
      'Ned Banta, U.S. Geological Survey'
      ' ')
    ReadOnly = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 6
    Top = 6
    Width = 107
    Height = 38
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Panel1'
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object Image1: TImage
      Left = 6
      Top = 6
      Width = 80
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      AutoSize = True
      Picture.Data = {
        0954474946496D61676547494638396150001600A20700289175108566EFF7F5
        7FBEADBFDED6409E85007D5CFFFFFF21F90401000007002C0000000050001600
        82289175108566EFF7F57FBEADBFDED6409E85007D5CFFFFFF03FF58BADC7D70
        AC012161040A198C0945565903E081E2B8159E010C622BCFB457B15E31127246
        983580A032A8B9524563C6C8CCDD5ABA0AAF451074989443D2283C4C9982662D
        903AE00C514BEFEB1A6C3F87702BE0067A287603ECAC17CF0A021B4F4E523D7C
        435A321B3D1A576807325907762104038E633E1D838F855467009E1E172DA19C
        42919C216E1246808B2EAA3B868C71AC0356503BB772845EAC530005C233745E
        40B8B2A2069632642315A9D02398C9D000190204DB1205B8B0D85ECA6A546C77
        88D12D92D38932E1D002A096DB3FE7F106E35E8C6F27EBD2EC71F2E440D2810E
        B712A5325C8203E14C1A7D5422044124CD41140102EF40C811A8CE1B0C7A7170
        00CAC72645BD6243A4D5A8006B06053DC7FE08C3612C85C3591159F27BA49206
        345233AAB4A3E14DC1B722246BA910D80BA696562DD82D8472D2D5C79349A9DC
        1281B1C7BFA1CC7891F3234341C2533847FD6383D017D83748C8CEC02614DD1B
        7425D7D23A3102AED29D7267E21BAB670801470126CDC9C6A7993A225EA11876
        C447AE8D46735D827531370E266198F0851608986CDA899E993C83D884751048
        9665FCACFCC19A17DA68CA54256A21F4392FF4820B1F5E86A52537C58D0343F7
        F338F2E4CE61A04B00003B}
    end
  end
end
