object framePhastInterpolation: TframePhastInterpolation
  Left = 0
  Top = 0
  Width = 589
  Height = 114
  HorzScrollBar.Range = 0
  VertScrollBar.Range = 0
  TabOrder = 0
  TabStop = True
  object lblDistance1: TLabel
    Left = 8
    Top = 60
    Width = 59
    Height = 16
    Caption = 'Distance 1'
    Enabled = False
  end
  object lblDistance2: TLabel
    Left = 8
    Top = 92
    Width = 59
    Height = 16
    Caption = 'Distance 2'
    Enabled = False
  end
  object lblValue1: TLabel
    Left = 216
    Top = 60
    Width = 43
    Height = 16
    Caption = 'Value 1'
    Enabled = False
  end
  object lblValue2: TLabel
    Left = 216
    Top = 92
    Width = 43
    Height = 16
    Caption = 'Value 2'
    Enabled = False
  end
  object lblMixtureFormula: TLabel
    Left = 392
    Top = 64
    Width = 90
    Height = 16
    Caption = 'Mixture formula'
  end
  object cbPhastInterpolation: TJvCheckBox
    Left = 8
    Top = 16
    Width = 194
    Height = 20
    Caption = 'Use PHAST-style interpolation'
    Enabled = False
    TabOrder = 1
    OnClick = cbPhastInterpolationClick
    LinkedControls = <>
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'Tahoma'
    HotTrackFont.Style = []
  end
  object rdeDistance1: TRbwDataEntry
    Left = 96
    Top = 56
    Width = 101
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeDistance2: TRbwDataEntry
    Left = 96
    Top = 88
    Width = 101
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeValue1: TRbwDataEntry
    Left = 280
    Top = 56
    Width = 101
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeValue2: TRbwDataEntry
    Left = 280
    Top = 88
    Width = 101
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rgInterpolationDirection: TRadioGroup
    Left = 288
    Top = 0
    Width = 289
    Height = 49
    Caption = 'Interpolation direction or mixture'
    Columns = 4
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'X'
      'Y'
      'Z'
      'Mix')
    TabOrder = 0
    OnClick = rgInterpolationDirectionClick
  end
  object edMixFormula: TRbwEdit
    Left = 392
    Top = 88
    Width = 101
    Height = 21
    Cursor = crIBeam
    Color = clBtnFace
    Enabled = False
    TabOrder = 6
  end
  object btnEditMixtureFormula: TButton
    Left = 496
    Top = 88
    Width = 89
    Height = 25
    Caption = 'Edit F()...'
    Enabled = False
    TabOrder = 7
  end
end
