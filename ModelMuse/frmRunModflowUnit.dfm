object frmRunModflow: TfrmRunModflow
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmRunModflow'
  ClientHeight = 190
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    367
    190)
  TextHeight = 13
  object lblBasicComments: TLabel
    Left = 6
    Top = 82
    Width = 103
    Height = 13
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Description of Project'
  end
  object cbRun: TCheckBox
    Left = 8
    Top = 8
    Width = 107
    Height = 18
    Caption = 'Execute model'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object comboModelSelection: TComboBox
    Left = 109
    Top = 6
    Width = 252
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
  object cbModpath: TCheckBox
    Left = 8
    Top = 34
    Width = 169
    Height = 18
    Caption = 'Export MODPATH input'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbForceCBF: TCheckBox
    Left = 174
    Top = 34
    Width = 199
    Height = 18
    Caption = 'Create new composite budget file'
    TabOrder = 3
  end
  object cbExportZoneBudget: TCheckBox
    Left = 8
    Top = 58
    Width = 169
    Height = 18
    Caption = 'Export ZONEBUDGET input'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object memoComments: TMemo
    Left = 6
    Top = 98
    Width = 355
    Height = 86
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
end
