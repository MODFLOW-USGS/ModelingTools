object frmRunModpath: TfrmRunModpath
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmRunModpath'
  ClientHeight = 60
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cbRun: TCheckBox
    Left = 8
    Top = 6
    Width = 102
    Height = 17
    Caption = 'Execute model'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cbForceCBF: TCheckBox
    Left = 116
    Top = 6
    Width = 189
    Height = 17
    Caption = 'Create new composite budget file'
    TabOrder = 1
  end
  object comboModelSelection: TComboBox
    Left = 8
    Top = 29
    Width = 241
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
end
