object frameStreamLink: TframeStreamLink
  Left = 0
  Top = 0
  Width = 521
  Height = 450
  TabOrder = 0
  object shpStreamColor: TShape
    Left = 448
    Top = 3
    Width = 65
    Height = 25
  end
  object shpDiversionColor: TShape
    Left = 448
    Top = 34
    Width = 65
    Height = 25
  end
  object lblTimeToPlot: TLabel
    Left = 112
    Top = 250
    Width = 67
    Height = 15
    Caption = ' Time to plot'
  end
  object shpUnconnectedColor: TShape
    Left = 448
    Top = 65
    Width = 65
    Height = 25
  end
  object lblSquareSize: TLabel
    Left = 112
    Top = 282
    Width = 100
    Height = 15
    Caption = 'Square Size (pixels)'
  end
  object shpBadConnection: TShape
    Left = 448
    Top = 96
    Width = 65
    Height = 25
  end
  object btnStreamColor: TButton
    Left = 242
    Top = 3
    Width = 200
    Height = 25
    Caption = 'Change outflow link color'
    TabOrder = 0
    OnClick = btnStreamColorClick
  end
  object btnDiversionColor: TButton
    Left = 242
    Top = 34
    Width = 200
    Height = 25
    Caption = 'Change diversion link color'
    TabOrder = 2
    OnClick = btnDiversionColorClick
  end
  object rgItemsToPlot: TRadioGroup
    Left = 3
    Top = 129
    Width = 185
    Height = 88
    Caption = 'Items to plot'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'All objects'
      'Visible objects'
      'Selected objects')
    TabOrder = 6
  end
  object cbStreams: TCheckBox
    Left = 3
    Top = 7
    Width = 126
    Height = 17
    Caption = 'Plot outflow links'
    TabOrder = 1
    OnClick = cbStreamsClick
  end
  object cbPlotDiversions: TCheckBox
    Left = 3
    Top = 38
    Width = 126
    Height = 17
    Caption = 'Plot diversion links'
    TabOrder = 3
    OnClick = cbPlotDiversionsClick
  end
  object comboTimeToPlot: TJvComboBox
    Left = 3
    Top = 247
    Width = 103
    Height = 23
    TabOrder = 7
    Text = 'comboTimeToPlot'
  end
  object cbPlotUnconnected: TCheckBox
    Left = 3
    Top = 69
    Width = 126
    Height = 17
    Caption = 'Plot unconnected streams'
    TabOrder = 5
    OnClick = cbPlotUnconnectedClick
  end
  object btnUnconnectedColor: TButton
    Left = 242
    Top = 65
    Width = 200
    Height = 25
    Caption = 'Change unconnected color'
    TabOrder = 4
    OnClick = btnUnconnectedColorClick
  end
  object seSquareSize: TJvSpinEdit
    Left = 3
    Top = 279
    Width = 103
    Height = 21
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 6.000000000000000000
    TabOrder = 8
  end
  object cbBadConnection: TCheckBox
    Left = 3
    Top = 100
    Width = 233
    Height = 17
    Caption = 'Plot invalid connections'
    TabOrder = 9
    OnClick = cbBadConnectionClick
  end
  object btnBadConnection: TButton
    Left = 242
    Top = 96
    Width = 200
    Height = 25
    Caption = 'Change invalid color'
    TabOrder = 10
    OnClick = btnBadConnectionClick
  end
  object dlgLinkColor: TColorDialog
    Left = 248
    Top = 72
  end
end
