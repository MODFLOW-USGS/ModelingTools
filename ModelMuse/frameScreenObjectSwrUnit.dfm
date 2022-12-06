inherited frameScreenObjectSwr: TframeScreenObjectSwr
  Width = 444
  ExplicitWidth = 444
  inherited pnlBottom: TPanel
    Top = 232
    Width = 444
    Height = 84
    ExplicitTop = 232
    ExplicitWidth = 444
    ExplicitHeight = 84
    DesignSize = (
      444
      84)
    object lblConductanceInterpretation: TLabel [1]
      Left = 8
      Top = 51
      Width = 146
      Height = 15
      Caption = 'Conductance interpretation'
    end
    inherited btnDelete: TBitBtn
      Left = 356
      ExplicitLeft = 356
    end
    inherited btnInsert: TBitBtn
      Left = 272
      ExplicitLeft = 272
    end
    object comboFormulaInterp: TComboBox
      Left = 262
      Top = 48
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'Calculated'
        'Direct'
        'Total per layer')
    end
  end
  inherited pnlTop: TPanel
    Width = 444
    ExplicitWidth = 444
    inherited pnlCaption: TPanel
      Width = 442
      ExplicitWidth = 442
    end
  end
  inherited pnlGrid: TPanel
    Width = 444
    Height = 207
    ExplicitWidth = 444
    ExplicitHeight = 207
    inherited pnlEditGrid: TPanel
      Width = 442
      ExplicitWidth = 442
    end
    inherited rdgModflowBoundary: TRbwDataGrid4
      Width = 442
      Height = 155
      ExplicitWidth = 442
      ExplicitHeight = 155
    end
  end
end
