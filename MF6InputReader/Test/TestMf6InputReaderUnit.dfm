object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 155
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object fedMf6InputReader: TJvFilenameEdit
    Left = 8
    Top = 72
    Width = 562
    Height = 23
    Filter = '*.exe|*.exe'
    TabOrder = 1
    Text = ''
  end
  object fedInputFiles: TJvFilenameEdit
    Left = 8
    Top = 112
    Width = 562
    Height = 23
    Filter = '*.txt|*.txt'
    TabOrder = 2
    Text = ''
  end
  object JvCreateProcess1: TJvCreateProcess
    WaitForTerminate = False
    Left = 144
    Top = 16
  end
end
