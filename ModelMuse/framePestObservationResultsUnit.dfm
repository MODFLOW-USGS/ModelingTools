inherited framePestObservationResults: TframePestObservationResults
  inherited pnlBottom: TPanel
    ExplicitTop = 432
    ExplicitWidth = 555
    inherited comboModels: TComboBox
      OnChange = nil
    end
  end
  inherited pgcObservations: TPageControl
    OnChange = nil
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 555
    inherited tabControls: TTabSheet
      inherited flnmedHeadObsResults: TJvFilenameEdit
        OnChange = nil
      end
      inherited spinSymbolSize: TJvSpinEdit
        OnChange = nil
      end
    end
    inherited tabValues: TTabSheet
      inherited rdgHeadObs: TRbwDataGrid4
        OnMouseUp = nil
      end
      inherited pnlValueControls: TPanel
        inherited btnCopy: TButton
          OnClick = nil
        end
        inherited btnHightlightObjects: TButton
          OnClick = nil
        end
        inherited btnRestore: TButton
          OnClick = nil
        end
      end
    end
    inherited tabGraph: TTabSheet
      inherited pbHeadObs: TPaintBox
        OnMouseDown = nil
        OnMouseMove = nil
        OnMouseUp = nil
        OnPaint = nil
      end
      inherited pnlGraphControls: TPanel
        inherited rgGraphType: TRadioGroup
          OnClick = nil
        end
      end
    end
  end
end
