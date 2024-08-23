object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 442
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 32
    Top = 8
    Width = 438
    Height = 15
    Caption = 
      'Convert a ModelMuse .mmZLib file to a .gpt file. (Compile with D' +
      'elphi 12 or above.'
  end
  object lbl2: TLabel
    Left = 24
    Top = 136
    Width = 414
    Height = 15
    Caption = 
      'Convert a ModelMuse .gpb file to a .gpt file. (Compile with Delp' +
      'hi 12 or above.'
  end
  object btn1: TButton
    Left = 32
    Top = 48
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 32
    Top = 176
    Width = 75
    Height = 25
    Caption = 'btn2'
    TabOrder = 1
    OnClick = btn2Click
  end
  object dlgOpen1: TOpenDialog
    Filter = '*.mmZlib|*.mmZlib'
    Left = 160
    Top = 32
  end
  object dlgOpen2: TOpenDialog
    Filter = '*.gpb|*.gpb'
    Left = 160
    Top = 160
  end
end
