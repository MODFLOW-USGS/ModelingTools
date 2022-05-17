object frameSutraRegionalProperty: TframeSutraRegionalProperty
  Left = 0
  Top = 0
  Width = 679
  Height = 480
  TabOrder = 0
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 679
    Height = 480
    ActivePage = tsIceProperties
    Align = alClient
    OwnerDraw = True
    TabHeight = 75
    TabOrder = 0
    OnDrawTab = pgcMainDrawTab
    object tsAdsorption: TTabSheet
      Caption = 'Parameters'
      object grpAdsorption: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 193
        Align = alTop
        Caption = 'Adsorption parameters/Thermal conductivity model'
        TabOrder = 0
        object lblFirstDistributionCoefficient: TLabel
          Left = 106
          Top = 132
          Width = 227
          Height = 20
          Caption = 'First distribution coefficient (CHI1)'
        end
        object lblSecondDistributionCoefficient: TLabel
          Left = 106
          Top = 159
          Width = 249
          Height = 20
          Caption = 'Second distribution coefficient (CHI2)'
        end
        object rgSorptionModel: TRadioGroup
          Left = 6
          Top = 21
          Width = 417
          Height = 105
          Caption = 'Sorption model (ADSMOD)'
          Items.Strings = (
            'None'
            'Linear'
            'Freundlich'
            'Langmuir')
          TabOrder = 0
        end
        object rdeFirstDistributionCoefficient: TRbwDataEntry
          Left = 6
          Top = 129
          Width = 94
          Height = 22
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeSecondDistributionCoefficient: TRbwDataEntry
          Left = 6
          Top = 155
          Width = 94
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
      object rgTransportModel: TRadioGroup
        Left = 6
        Top = 199
        Width = 417
        Height = 105
        Caption = 'Thermal conductivity model (TCMOD)'
        Items.Strings = (
          'Arithmetic-mean'
          'Geometric-mean'
          'Harmonic-mean')
        TabOrder = 1
      end
    end
    object tsTotalSaturation: TTabSheet
      Caption = 'Saturation'
      ImageIndex = 1
      object grpWaterSaturation: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 395
        Align = alClient
        Caption = 'Total water saturation parameters'
        TabOrder = 0
        object rgWatSatFunct: TRadioGroup
          Left = 3
          Top = 22
          Width = 417
          Height = 124
          Caption = 'Specified total water saturation function (SWMOD)'
          Items.Strings = (
            'None'
            'van Genuchten'
            'Brooks-Corey'
            'Piecewise-linear'
            'User-defined')
          TabOrder = 0
          OnClick = rgWatSatFunctClick
        end
        object jplTotalSaturation: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 152
          Width = 667
          Height = 241
          Margins.Left = 0
          Margins.Top = 130
          Margins.Right = 0
          Margins.Bottom = 0
          ActivePage = jvspTotSatControls
          PropagateEnable = False
          Align = alClient
          object jvspTotSatControls: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspTotSatControls'
            object lblResidWatSat: TLabel
              Left = 164
              Top = 16
              Width = 263
              Height = 20
              Caption = 'Residual total water saturation (SWRES)'
            end
            object lblVgenAlpha: TLabel
              Left = 164
              Top = 42
              Width = 300
              Height = 20
              Caption = 'van Genuchten function parameter '#945'_VG (AA)'
            end
            object lblVgenEta: TLabel
              Left = 164
              Top = 68
              Width = 300
              Height = 20
              Caption = 'van Genuchten function parameter '#951'_VG (VN)'
            end
            object lblAirEntryPressure: TLabel
              Left = 164
              Top = 94
              Width = 166
              Height = 20
              Caption = 'Air-entry pressure (PENT)'
            end
            object lblPoreDistIndex: TLabel
              Left = 164
              Top = 120
              Width = 240
              Height = 20
              Caption = 'Pore size distribution index (RLAMB)'
            end
            object lblPoreDistInd: TLabel
              Left = 164
              Top = 146
              Width = 482
              Height = 20
              Caption = 
                'Pressure at which the saturation reaches the residual saturation' +
                ' (PSWRES)'
            end
            object rdeResidWatSat: TRbwDataEntry
              Left = 13
              Top = 10
              Width = 145
              Height = 22
              TabOrder = 0
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              CheckMin = True
              ChangeDisabledColor = True
            end
            object rdeVgenAlpha: TRbwDataEntry
              Left = 13
              Top = 38
              Width = 145
              Height = 22
              TabOrder = 1
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeVgenEta: TRbwDataEntry
              Left = 13
              Top = 66
              Width = 145
              Height = 22
              TabOrder = 2
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdePoreDistIndex: TRbwDataEntry
              Left = 13
              Top = 122
              Width = 145
              Height = 22
              TabOrder = 4
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdePresAtResid: TRbwDataEntry
              Left = 13
              Top = 150
              Width = 145
              Height = 22
              TabOrder = 5
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeAirEntryPressure: TRbwDataEntry
              Left = 13
              Top = 94
              Width = 145
              Height = 22
              TabOrder = 3
              Text = '0'
              DataType = dtReal
              CheckMax = True
              ChangeDisabledColor = True
            end
          end
          object jvspTotSatUserDefined: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspTotSatUserDefined'
            inline frameTotSatUserDefined: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 241
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 241
              inherited Panel: TPanel
                Top = 200
                Width = 667
                ExplicitTop = 200
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 149
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 149
                end
                inherited sbAdd: TSpeedButton
                  Left = 350
                  OnClick = frameTotSatUserDefinedsbAddClick
                  ExplicitLeft = 350
                end
                inherited sbInsert: TSpeedButton
                  Left = 414
                  OnClick = frameTotSatUserDefinedsbInsertClick
                  ExplicitLeft = 414
                end
                inherited sbDelete: TSpeedButton
                  Left = 479
                  ExplicitLeft = 479
                end
                inherited seNumber: TJvSpinEdit
                  Height = 28
                  MaxValue = 10.000000000000000000
                  ExplicitHeight = 28
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 200
                ExplicitWidth = 667
                ExplicitHeight = 200
              end
            end
          end
        end
      end
    end
    object tsPermeability: TTabSheet
      Caption = 'Permeability'
      ImageIndex = 2
      object grpRelativePerm: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 395
        Align = alClient
        Caption = 'Relative Permeability Parameters'
        TabOrder = 0
        object rgRelativePermChoice: TRadioGroup
          Left = 3
          Top = 22
          Width = 417
          Height = 124
          Caption = 'Specified relative permeability function (RKMOD)'
          Items.Strings = (
            'None'
            'van Genuchten'
            'Brooks-Corey'
            'Piecewise-linear'
            'User-defined')
          TabOrder = 0
          OnClick = rgRelativePermChoiceClick
        end
        object jplRelativePerm: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 152
          Width = 667
          Height = 241
          Margins.Left = 0
          Margins.Top = 130
          Margins.Right = 0
          Margins.Bottom = 0
          ActivePage = jvspRelativePermControls
          PropagateEnable = False
          Align = alClient
          object jvspRelativePermControls: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspRelativePermControls'
            object lblMinRelPerm: TLabel
              Left = 164
              Top = 16
              Width = 264
              Height = 20
              Caption = 'Minimum relative permeability (RKMIN)'
            end
            object lblRelPermEta: TLabel
              Left = 164
              Top = 44
              Width = 300
              Height = 20
              Caption = 'van Genuchten function parameter '#951'_VG (VN)'
            end
            object lblRelPermPoreDistIndex: TLabel
              Left = 164
              Top = 68
              Width = 240
              Height = 20
              Caption = 'Pore size distribution index (RLAMB)'
            end
            object lblSatAtMinPerm: TLabel
              Left = 164
              Top = 94
              Width = 470
              Height = 40
              Caption = 
                'Liquid-water saturation at which the permeability reaches its mi' +
                'nimum (SLRKMIN)'
              WordWrap = True
            end
            object rdeMinRelPerm: TRbwDataEntry
              Left = 13
              Top = 10
              Width = 145
              Height = 22
              TabOrder = 0
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              CheckMin = True
              ChangeDisabledColor = True
            end
            object rdeRelPermEta: TRbwDataEntry
              Left = 13
              Top = 42
              Width = 145
              Height = 22
              TabOrder = 1
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeRelPermPoreDistIndex: TRbwDataEntry
              Left = 13
              Top = 70
              Width = 145
              Height = 22
              TabOrder = 2
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeSatAtMinPerm: TRbwDataEntry
              Left = 13
              Top = 98
              Width = 145
              Height = 22
              TabOrder = 3
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              CheckMin = True
              ChangeDisabledColor = True
            end
          end
          object jvspRelativePermUserDefined: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspTotSatUserDefined'
            inline frameRelPermParam: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 241
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 241
              inherited Panel: TPanel
                Top = 200
                Width = 667
                ExplicitTop = 200
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 149
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 149
                end
                inherited sbAdd: TSpeedButton
                  Left = 350
                  ExplicitLeft = 350
                end
                inherited sbInsert: TSpeedButton
                  Left = 414
                  ExplicitLeft = 414
                end
                inherited sbDelete: TSpeedButton
                  Left = 479
                  ExplicitLeft = 479
                end
                inherited seNumber: TJvSpinEdit
                  Height = 28
                  MaxValue = 10.000000000000000000
                  ExplicitHeight = 28
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 200
                ExplicitWidth = 667
                ExplicitHeight = 200
              end
            end
          end
        end
      end
    end
    object tsLiquidWater: TTabSheet
      Caption = 'Saturation'
      ImageIndex = 3
      object grpLiqWatSat: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 395
        Align = alClient
        Caption = 'Liquid water saturation parameters'
        TabOrder = 0
        object rgLiqWatSatChoice: TRadioGroup
          Left = 3
          Top = 22
          Width = 417
          Height = 124
          Caption = 'Specified liquid water saturation function (SLMOD)'
          Items.Strings = (
            'None'
            'Exponential'
            'Modified power law'
            'Piecewise-linear'
            'User-defined')
          TabOrder = 0
          OnClick = rgLiqWatSatChoiceClick
        end
        object jplLiqWatSat: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 152
          Width = 667
          Height = 241
          Margins.Left = 0
          Margins.Top = 130
          Margins.Right = 0
          Margins.Bottom = 0
          ActivePage = jvspLiqWatSatControls
          PropagateEnable = False
          Align = alClient
          object jvspLiqWatSatControls: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspRelativePermControls'
            object lblResidLiqWatSat: TLabel
              Left = 164
              Top = 16
              Width = 292
              Height = 20
              Caption = 'Residual liquid water saturation  (SLSATRES)'
            end
            object lblExpParamW: TLabel
              Left = 164
              Top = 42
              Width = 225
              Height = 20
              Caption = 'Exponential parameter w_EXP (W)'
            end
            object lblPowerAlpha: TLabel
              Left = 164
              Top = 68
              Width = 369
              Height = 20
              Caption = 'Modified power law model parameter, '#945'_POW (ALPHA)'
            end
            object lblPowerBeta: TLabel
              Left = 164
              Top = 94
              Width = 352
              Height = 20
              Caption = 'Modified power law model parameter, '#946'_POW(BETA)'
            end
            object lblLiqWatRelTemSatMin: TLabel
              Left = 164
              Top = 118
              Width = 418
              Height = 40
              Caption = 
                'Relative temperature at which the liquid saturation reaches the ' +
                'residual liquid saturation (TLRES)'
              WordWrap = True
            end
            object rdeResidLiqWatSat: TRbwDataEntry
              Left = 13
              Top = 10
              Width = 145
              Height = 22
              TabOrder = 0
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              CheckMin = True
              ChangeDisabledColor = True
            end
            object rdeExpParamW: TRbwDataEntry
              Left = 13
              Top = 38
              Width = 145
              Height = 22
              TabOrder = 1
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdePowerAlpha: TRbwDataEntry
              Left = 13
              Top = 66
              Width = 145
              Height = 22
              TabOrder = 2
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdeLiqWatRelTemSatMin: TRbwDataEntry
              Left = 13
              Top = 122
              Width = 145
              Height = 22
              TabOrder = 4
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object rdePowerBeta: TRbwDataEntry
              Left = 13
              Top = 94
              Width = 145
              Height = 22
              TabOrder = 3
              Text = '0'
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
          end
          object jvspLiqWatSatParameters: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 241
            Caption = 'jvspTotSatUserDefined'
            inline frameGrid1: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 241
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 241
              inherited Panel: TPanel
                Top = 200
                Width = 667
                ExplicitTop = 200
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 149
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 149
                end
                inherited sbAdd: TSpeedButton
                  Left = 350
                  ExplicitLeft = 350
                end
                inherited sbInsert: TSpeedButton
                  Left = 414
                  ExplicitLeft = 414
                end
                inherited sbDelete: TSpeedButton
                  Left = 479
                  ExplicitLeft = 479
                end
                inherited seNumber: TJvSpinEdit
                  Height = 28
                  MaxValue = 10.000000000000000000
                  ExplicitHeight = 28
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 200
                ExplicitWidth = 667
                ExplicitHeight = 200
              end
            end
          end
        end
      end
    end
    object tsIceProperties: TTabSheet
      Caption = 'and Latent Heat'
      ImageIndex = 4
      object grpFreezeHeat: TGroupBox
        Left = 0
        Top = 0
        Width = 671
        Height = 395
        Align = alClient
        Caption = 'Freezing Temperature and Latent Heat'
        TabOrder = 0
        object lblMaxFreezeTemp: TLabel
          Left = 164
          Top = 32
          Width = 367
          Height = 20
          Caption = 'Maximum freezing temperature of pore water (TFREEZ)'
        end
        object lblLatentHeat: TLabel
          Left = 164
          Top = 58
          Width = 193
          Height = 20
          Caption = 'Latent heat of fusion (HTLAT)'
        end
        object rdeMaxFreezeTemp: TRbwDataEntry
          Left = 13
          Top = 26
          Width = 145
          Height = 22
          TabOrder = 0
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMax = True
          ChangeDisabledColor = True
        end
        object rdeLatentHeat: TRbwDataEntry
          Left = 13
          Top = 54
          Width = 145
          Height = 22
          TabOrder = 1
          Text = '3.34E5'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
  end
end
