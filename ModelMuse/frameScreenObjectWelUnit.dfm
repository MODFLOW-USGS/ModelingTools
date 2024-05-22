inherited frameScreenObjectWel: TframeScreenObjectWel
  Height = 314
  ExplicitHeight = 314
  inherited pnlBottom: TPanel
    Top = 208
    Height = 106
    ExplicitTop = 208
    ExplicitHeight = 106
    DesignSize = (
      320
      106)
    object lblTabfile: TLabel [2]
      Left = 255
      Top = 83
      Width = 34
      Height = 15
      Caption = 'Tabfile'
    end
    inherited comboTimeSeriesInterpolation: TComboBox
      TabOrder = 5
    end
    object fedTabfile: TJvFilenameEdit
      Left = 8
      Top = 80
      Width = 241
      Height = 23
      Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
      TabOrder = 4
      Text = ''
      OnChange = fedTabfileChange
    end
  end
  inherited pnlGrid: TPanel
    Height = 116
    ExplicitHeight = 116
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 64
      ExplicitHeight = 64
    end
  end
end
