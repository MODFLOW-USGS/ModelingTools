inherited frameScreenObjectFhbFlow: TframeScreenObjectFhbFlow
  inherited pnlBottom: TPanel
    Top = 240
    Height = 76
    ExplicitTop = 240
    ExplicitHeight = 76
    DesignSize = (
      320
      76)
    object lblConductanceInterpretation: TLabel [1]
      Left = 8
      Top = 51
      Width = 114
      Height = 13
      Caption = 'Flow rate interpretation'
    end
    object comboFormulaInterp: TComboBox
      Left = 174
      Top = 48
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = comboFormulaInterpChange
      Items.Strings = (
        'Calculated'
        'Direct'
        'Total per layer')
    end
  end
  inherited pnlGrid: TPanel
    Height = 215
    ExplicitHeight = 215
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 163
      ExplicitHeight = 163
    end
  end
end
