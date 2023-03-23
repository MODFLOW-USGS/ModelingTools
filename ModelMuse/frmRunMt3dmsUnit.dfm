object frmRunMt3dms: TfrmRunMt3dms
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmRunMt3dms'
  ClientHeight = 74
  ClientWidth = 228
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object lblMt3dModelSelection: TLabel
    Left = 8
    Top = 29
    Width = 74
    Height = 13
    Caption = 'Model Selection'
  end
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
  object comboMt3dModelSelection: TComboBox
    Left = 8
    Top = 45
    Width = 201
    Height = 21
    Style = csDropDownList
    TabOrder = 1
  end
end
