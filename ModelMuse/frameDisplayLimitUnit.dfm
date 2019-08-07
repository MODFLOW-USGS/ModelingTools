object frameDisplayLimit: TframeDisplayLimit
  Left = 0
  Top = 0
  Width = 243
  Height = 35
  HorzScrollBar.Range = 0
  VertScrollBar.Range = 0
  TabOrder = 0
  TabStop = True
  object cbCheck: TCheckBox
    Left = 0
    Top = 2
    Width = 89
    Height = 30
    Caption = 'Limit to:'
    Enabled = False
    TabOrder = 2
    OnClick = cbCheckClick
  end
  object rdeLimit: TRbwDataEntry
    Left = 88
    Top = 0
    Width = 152
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 1
    Text = '0'
    OnExit = rdeLimitExit
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object comboBoolLimit: TComboBox
    Left = 88
    Top = 0
    Width = 89
    Height = 24
    Style = csDropDownList
    Enabled = False
    ItemIndex = 0
    TabOrder = 0
    Text = 'False'
    Visible = False
    OnChange = comboBoolLimitChange
    Items.Strings = (
      'False'
      'True')
  end
end
