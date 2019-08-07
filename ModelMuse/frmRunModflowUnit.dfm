object frmRunModflow: TfrmRunModflow
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmRunModflow'
  ClientHeight = 238
  ClientWidth = 459
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    459
    238)
  PixelsPerInch = 120
  TextHeight = 17
  object lblBasicComments: TLabel
    Left = 8
    Top = 103
    Width = 132
    Height = 17
    Caption = 'Description of Project'
  end
  object cbRun: TCheckBox
    Left = 10
    Top = 10
    Width = 134
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Execute model'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object comboModelSelection: TComboBox
    Left = 136
    Top = 8
    Width = 315
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    TabOrder = 1
  end
  object cbModpath: TCheckBox
    Left = 10
    Top = 43
    Width = 211
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Export MODPATH input'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbForceCBF: TCheckBox
    Left = 218
    Top = 43
    Width = 248
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Create new composite budget file'
    TabOrder = 3
  end
  object cbExportZoneBudget: TCheckBox
    Left = 10
    Top = 73
    Width = 211
    Height = 22
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Export ZONEBUDGET input'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object memoComments: TMemo
    Left = 8
    Top = 123
    Width = 443
    Height = 107
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
    ExplicitWidth = 645
  end
end
