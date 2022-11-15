object frmReplaceHttpWithHttps: TfrmReplaceHttpWithHttps
  Left = 0
  Top = 0
  Caption = 'Replace Http with Https'
  ClientHeight = 64
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 17
  object btnReplaceHttpWithHttps: TButton
    Left = 20
    Top = 10
    Width = 231
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Replace http:// with https://'
    TabOrder = 0
    OnClick = btnReplaceHttpWithHttpsClick
  end
end
