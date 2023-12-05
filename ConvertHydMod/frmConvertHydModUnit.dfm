object frmConvertHydMod: TfrmConvertHydMod
  Left = 0
  Top = 0
  Caption = 'Convert Hydmod'
  ClientHeight = 171
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    361
    171)
  TextHeight = 15
  object lblHydMod: TLabel
    Left = 8
    Top = 8
    Width = 109
    Height = 15
    Caption = 'Hydmod Output File'
  end
  object lblText: TLabel
    Left = 8
    Top = 64
    Width = 100
    Height = 15
    Caption = 'Converted Text File'
  end
  object edHydMod: TJvFilenameEdit
    Left = 8
    Top = 29
    Width = 345
    Height = 23
    OnAfterDialog = edHydModAfterDialog
    Filter = 'Hydmod output files (*.hyd_out)|*.hyd_out|All files (*.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnExit = edHydModExit
  end
  object edText: TJvFilenameEdit
    Left = 8
    Top = 85
    Width = 345
    Height = 23
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
  end
  object btnConvert: TButton
    Left = 8
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = btnConvertClick
  end
end
