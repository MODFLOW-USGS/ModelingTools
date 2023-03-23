inherited frameColorGrid: TframeColorGrid
  Width = 571
  Height = 501
  ExplicitWidth = 571
  ExplicitHeight = 501
  inherited pcChoices: TPageControl
    Width = 571
    Height = 501
    ExplicitWidth = 571
    ExplicitHeight = 501
    inherited tabSelection: TTabSheet
      ExplicitWidth = 563
      ExplicitHeight = 471
      DesignSize = (
        563
        471)
      inherited lblColorScheme: TLabel
        Left = 10
        Top = 291
        ExplicitLeft = 10
        ExplicitTop = 294
      end
      inherited lblCycles: TLabel
        Left = 457
        Top = 342
        ExplicitLeft = 457
        ExplicitTop = 345
      end
      inherited pbColorScheme: TPaintBox
        Left = 5
        Top = 359
        Width = 398
        ExplicitLeft = 5
        ExplicitTop = 362
        ExplicitWidth = 398
      end
      inherited lblColorAdjustment: TLabel
        Left = 10
        Top = 406
        ExplicitLeft = 10
        ExplicitTop = 409
      end
      object lblTime: TLabel [6]
        Left = 420
        Top = 4
        Width = 26
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Time'
      end
      inherited comboColorScheme: TComboBox
        Left = 10
        Top = 315
        Width = 309
        TabOrder = 7
        ExplicitLeft = 10
        ExplicitTop = 315
        ExplicitWidth = 309
      end
      inherited seCycles: TJvSpinEdit
        Left = 457
        Top = 373
        TabOrder = 8
        ExplicitLeft = 457
        ExplicitTop = 373
      end
      inherited jsColorExponent: TJvxSlider
        Top = 430
        TabOrder = 9
      end
      inherited seColorExponent: TJvSpinEdit
        Top = 439
        TabOrder = 10
        ExplicitTop = 439
      end
      inherited cbLogTransform: TCheckBox
        Top = 439
        TabOrder = 11
        ExplicitTop = 439
      end
      inherited udDataSets: TJvUpDown
        Left = 369
        Top = 26
        Width = 16
        Height = 21
        Associate = virttreecomboDataSets
        ExplicitLeft = 369
        ExplicitTop = 26
        ExplicitWidth = 16
        ExplicitHeight = 21
      end
      inherited rgUpdateLimitChoice: TRadioGroup
        Left = 10
        Top = 207
        Width = 309
        TabOrder = 5
        ExplicitLeft = 10
        ExplicitTop = 207
        ExplicitWidth = 309
      end
      inherited virttreecomboDataSets: TRbwStringTreeCombo
        Left = 10
        Top = 26
        Width = 359
        Tree.OnGetNodeDataSize = virttreecomboDataSetsTreeGetNodeDataSize
        Anchors = [akLeft, akTop, akRight]
        Text = '0'
        ExplicitLeft = 10
        ExplicitTop = 26
        ExplicitWidth = 375
      end
      inherited reComment: TRichEdit
        Width = 550
        Height = 118
        TabOrder = 4
        ExplicitWidth = 550
        ExplicitHeight = 118
      end
      inherited btnColorSchemes: TButton
        Left = 333
        Top = 305
        TabOrder = 6
        ExplicitLeft = 333
        ExplicitTop = 305
      end
      object udTime: TJvUpDown
        Left = 511
        Top = 25
        Width = 17
        Height = 21
        Anchors = [akTop, akRight]
        Max = 0
        TabOrder = 3
        OnChangingEx = udTimeChangingEx
      end
      object comboTime3D: TJvComboBox
        Left = 420
        Top = 26
        Width = 85
        Height = 23
        Anchors = [akTop, akRight]
        TabOrder = 2
        Text = '0'
        OnChange = comboTime3DChange
      end
    end
    inherited tabFilters: TTabSheet
      ExplicitWidth = 563
      ExplicitHeight = 471
      DesignSize = (
        563
        471)
      inherited lblNumberOfValuesToIgnore: TLabel
        Top = 443
        ExplicitTop = 443
      end
      inherited rdgValuesToIgnore: TRbwDataGrid4
        Height = 319
        ExplicitHeight = 319
      end
      inherited seNumberOfValuesToIgnore: TJvSpinEdit
        Top = 437
        ExplicitTop = 440
      end
    end
    inherited tabLegend: TTabSheet
      ExplicitWidth = 563
      ExplicitHeight = 471
      inherited imLegend: TImage
        Width = 357
        Height = 471
        ExplicitWidth = 395
        ExplicitHeight = 470
      end
      inherited splColor: TSplitter
        Height = 471
        ExplicitHeight = 470
      end
      inherited pnlLegend: TPanel
        Height = 471
        ExplicitHeight = 473
        DesignSize = (
          201
          471)
        inherited lblColorLegendRows: TLabel
          Top = 376
          ExplicitTop = 376
        end
        inherited seLegendRows: TJvSpinEdit
          Top = 395
          ExplicitTop = 395
        end
        inherited rdgLegend: TRbwDataGrid4
          Height = 311
          ExplicitHeight = 311
        end
      end
    end
  end
end
