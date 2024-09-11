object MyForm: TMyForm
  Left = 285
  Top = 117
  Caption = 'MyForm'
  ClientHeight = 601
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object RBW_SetWindowState1: TRBW_SetWindowState
    OnWindowStateChange = SetWindowState1WindowStateChange
    Owner = Owner
    Left = 64
    Top = 24
  end
end
