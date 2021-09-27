object MyForm: TMyForm
  Left = 285
  Top = 117
  Width = 870
  Height = 640
  Caption = 'MyForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnHide = FormHide
  PixelsPerInch = 120
  TextHeight = 16
  object RBW_SetWindowState1: TRBW_SetWindowState
    OnWindowStateChange = SetWindowState1WindowStateChange
    Owner = Owner
    Left = 64
    Top = 24
  end
end
