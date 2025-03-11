inherited frameScreenObjectEsl: TframeScreenObjectEsl
  inherited pnlBottom: TPanel
    StyleElements = [seFont, seClient, seBorder]
    inherited lblNumTimes: TLabel
      StyleElements = [seFont, seClient, seBorder]
    end
    inherited lblChemSpecies: TLabel
      StyleElements = [seFont, seClient, seBorder]
    end
    inherited seNumberOfTimes: TJvSpinEdit
      StyleElements = [seFont, seClient, seBorder]
    end
    inherited comboChemSpecies: TComboBox
      StyleElements = [seFont, seClient, seBorder]
    end
  end
  inherited pnlTop: TPanel
    StyleElements = [seFont, seClient, seBorder]
    inherited pnlCaption: TPanel
      StyleElements = [seFont, seClient, seBorder]
    end
  end
  inherited pnlGrid: TPanel
    StyleElements = [seFont, seClient, seBorder]
    inherited pnlEditGrid: TPanel
      StyleElements = [seFont, seClient, seBorder]
      inherited lblFormula: TLabel
        StyleElements = [seFont, seClient, seBorder]
      end
      inherited rdeFormula: TRbwDataEntry
        StyleElements = [seFont, seClient, seBorder]
      end
    end
  end
end
