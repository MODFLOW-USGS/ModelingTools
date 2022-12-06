inherited frameFormulaGrid: TframeFormulaGrid
  Height = 164
  OnResize = FrameResize
  ExplicitHeight = 164
  inherited Panel: TPanel
    Top = 123
    TabOrder = 2
    ExplicitTop = 123
    inherited sbAdd: TSpeedButton
      Hint = 'Add row|Add a row below the bottom row.'
    end
    inherited sbInsert: TSpeedButton
      Hint = 'Insert row|Insert a row above the selected row.'
    end
  end
  inherited Grid: TRbwDataGrid4
    Top = 57
    Height = 66
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    OnMouseUp = GridMouseUp
    OnColSize = GridColSize
    OnHorizontalScroll = GridHorizontalScroll
    ExplicitTop = 57
    ExplicitHeight = 66
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 301
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edFormula: TLabeledEdit
      Left = 128
      Top = 30
      Width = 121
      Height = 21
      EditLabel.Width = 44
      EditLabel.Height = 15
      EditLabel.Caption = 'Formula'
      Enabled = False
      TabOrder = 0
      Text = ''
      OnChange = edFormulaChange
    end
    object cbMultiCheck: TCheckBox
      Left = 56
      Top = 34
      Width = 49
      Height = 17
      Caption = 'cbMultiCheck'
      TabOrder = 1
      Visible = False
      OnClick = cbMultiCheckClick
    end
  end
end
