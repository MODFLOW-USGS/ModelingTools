inherited frameFormulaGrid: TframeFormulaGrid
  Width = 447
  Height = 164
  OnResize = FrameResize
  ExplicitWidth = 447
  ExplicitHeight = 164
  inherited Panel: TPanel
    Top = 123
    Width = 447
    TabOrder = 2
    ExplicitTop = 123
    ExplicitWidth = 447
    DesignSize = (
      447
      41)
    inherited sbAdd: TSpeedButton
      Left = 231
      Hint = 'Add row|Add a row below the bottom row.'
      ExplicitLeft = 231
    end
    inherited sbInsert: TSpeedButton
      Left = 274
      Hint = 'Insert row|Insert a row above the selected row.'
      ExplicitLeft = 274
    end
    inherited sbDelete: TSpeedButton
      Left = 317
      ExplicitLeft = 317
    end
  end
  inherited Grid: TRbwDataGrid4
    Top = 57
    Width = 447
    Height = 66
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    OnMouseUp = GridMouseUp
    OnSelectCell = GridSelectCell
    OnColSize = GridColSize
    OnHorizontalScroll = GridHorizontalScroll
    ExplicitTop = 57
    ExplicitWidth = 447
    ExplicitHeight = 66
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edFormula: TLabeledEdit
      Left = 128
      Top = 30
      Width = 121
      Height = 23
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
    object comboChoice: TComboBox
      Left = 255
      Top = 28
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 2
      Visible = False
      OnChange = comboChoiceChange
    end
  end
end
