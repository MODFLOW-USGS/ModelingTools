object frmConvertHydMod: TfrmConvertHydMod
  Left = 0
  Top = 0
  Caption = 'Convert Hydmod'
  ClientHeight = 212
  ClientWidth = 736
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    736
    212)
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
    Top = 120
    Width = 138
    Height = 15
    Caption = 'Converted CSV or Text File'
  end
  object edHydMod: TJvFilenameEdit
    Left = 8
    Top = 29
    Width = 720
    Height = 23
    OnAfterDialog = edHydModAfterDialog
    Filter = 'Hydmod output files (*.hyd_out)|*.hyd_out|All files (*.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnExit = edHydModExit
    ExplicitWidth = 341
  end
  object edText: TJvFilenameEdit
    Left = 8
    Top = 141
    Width = 720
    Height = 23
    Filter = 
      'Comma-Separated Values (3.csv)|*.csv|Text files (*.txt)|*.txt|Al' +
      'l files (*.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
    ExplicitWidth = 345
  end
  object btnConvert: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object rgOutputType: TRadioGroup
    Left = 8
    Top = 53
    Width = 720
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output type'
    ItemIndex = 0
    Items.Strings = (
      'Comma-separated values (*.csv)'
      'Text values separated by tab characters (*.txt)')
    TabOrder = 3
    OnClick = rgOutputTypeClick
    ExplicitWidth = 345
  end
end
