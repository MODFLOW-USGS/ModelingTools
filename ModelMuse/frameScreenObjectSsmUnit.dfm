inherited frameScreenObjectSsm: TframeScreenObjectSsm
  inherited pnlTop: TPanel
    Height = 76
    ExplicitHeight = 76
    inherited pnlCaption: TPanel
      Align = alTop
    end
    object cbSpecifiedConcentration: TCheckBox
      Left = 8
      Top = 31
      Width = 306
      Height = 17
      Caption = 'Specified concentration (ITYPE = -1)'
      TabOrder = 1
      OnClick = cbSpecifiedConcentrationClick
    end
    object cbMassLoading: TCheckBox
      Left = 8
      Top = 54
      Width = 297
      Height = 17
      Caption = 'Mass-loading (ITYPE = 15)'
      TabOrder = 2
      OnClick = cbMassLoadingClick
    end
  end
  inherited pnlGrid: TPanel
    Top = 76
    Height = 194
    ExplicitTop = 76
    ExplicitHeight = 194
    inherited rdgModflowBoundary: TRbwDataGrid4
      Height = 142
      ExplicitHeight = 142
    end
  end
end
