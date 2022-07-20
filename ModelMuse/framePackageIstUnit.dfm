inherited framePackageIst: TframePackageIst
  Height = 417
  ExplicitHeight = 417
  inline frameRowGrid1: TframeRowGrid [3]
    Left = 0
    Top = 157
    Width = 422
    Height = 260
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 157
    ExplicitWidth = 422
    ExplicitHeight = 260
    inherited Panel: TPanel
      Top = 219
      Width = 422
      inherited lblNumber: TLabel
        Width = 182
        Height = 40
        Caption = 'Number of immobile zones'#13#10
        ExplicitWidth = 182
        ExplicitHeight = 40
      end
      inherited sbAdd: TSpeedButton
        Left = 326
        ExplicitLeft = 326
      end
      inherited sbInsert: TSpeedButton
        Left = 355
        ExplicitLeft = 355
      end
      inherited sbDelete: TSpeedButton
        Left = 384
        ExplicitLeft = 384
      end
      inherited seNumber: TJvSpinEdit
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
      end
    end
    inherited Grid: TRbwRowDataGrid
      Width = 422
      Height = 219
      ColCount = 2
      Columns = <
        item
          AutoAdjustColWidths = False
        end
        item
          AutoAdjustColWidths = False
        end>
    end
  end
end
