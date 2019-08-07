object frameDrawingSize: TframeDrawingSize
  Left = 0
  Top = 0
  Width = 238
  Height = 166
  Font.Color = clBlack
  Font.Height = 18
  Font.Name = 'arial'
  Font.Pitch = fpVariable
  Font.Style = []
  Font.Weight = 48
  ParentFont = False
  TabOrder = 0
  object gbDrawingArea: TGroupBox
    Left = 0
    Top = 0
    Width = 238
    Height = 166
    Align = alClient
    Caption = 'gbDrawingArea'
    TabOrder = 0
    object adeMin1: TArgusDataEntry
      Left = 128
      Top = 61
      Width = 101
      Height = 29
      Alignment = taRightJustify
      Color = clWhite
      TabOrder = 1
      Text = '0'
      OnChange = adeMax1Change
      DataType = dtReal
      Max = 1
      ChangeDisabledColor = True
      EnabledColor = clNone
    end
    object lblMin1: TLabel
      Left = 8
      Top = 64
      Width = 92
      Height = 21
      Caption = 'Minimum X:'
      FocusControl = adeMin1
    end
    object adeMax1: TArgusDataEntry
      Left = 128
      Top = 28
      Width = 101
      Height = 29
      Alignment = taRightJustify
      Color = clWhite
      TabOrder = 0
      Text = '0'
      OnChange = adeMax1Change
      DataType = dtReal
      Max = 1
      ChangeDisabledColor = True
      EnabledColor = clNone
    end
    object lblMax1: TLabel
      Left = 8
      Top = 31
      Width = 96
      Height = 21
      Caption = 'Maximum X:'
      FocusControl = adeMax1
    end
    object adeMin2: TArgusDataEntry
      Left = 128
      Top = 127
      Width = 101
      Height = 29
      Alignment = taRightJustify
      Color = clWhite
      TabOrder = 3
      Text = '0'
      OnChange = adeMax2Change
      DataType = dtReal
      Max = 1
      ChangeDisabledColor = True
      EnabledColor = clNone
    end
    object lblMin2: TLabel
      Left = 8
      Top = 130
      Width = 93
      Height = 21
      Caption = 'Minimum Y:'
      FocusControl = adeMin2
    end
    object lblMax2: TLabel
      Left = 8
      Top = 97
      Width = 97
      Height = 21
      Caption = 'Maximum Y:'
      FocusControl = adeMax2
    end
    object adeMax2: TArgusDataEntry
      Left = 128
      Top = 94
      Width = 101
      Height = 29
      Alignment = taRightJustify
      Color = clWhite
      TabOrder = 2
      Text = '0'
      OnChange = adeMax2Change
      DataType = dtReal
      Max = 1
      ChangeDisabledColor = True
      EnabledColor = clNone
    end
  end
end
