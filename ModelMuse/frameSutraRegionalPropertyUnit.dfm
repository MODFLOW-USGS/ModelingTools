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
    ActivePage = tsPermeability
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
        Height = 209
        Align = alTop
        Caption = 'Adsorption parameters/Thermal conductivity model'
        TabOrder = 0
        object lblFirstDistributionCoefficient: TLabel
          Left = 133
          Top = 135
          Width = 182
          Height = 15
          Caption = 'First distribution coefficient (CHI1)'
        end
        object lblSecondDistributionCoefficient: TLabel
          Left = 133
          Top = 168
          Width = 199
          Height = 15
          Caption = 'Second distribution coefficient (CHI2)'
        end
        object rgSorptionModel: TRadioGroup
          Left = 3
          Top = 21
          Width = 417
          Height = 105
          Caption = 'Sorption model (ADSMOD)'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Linear'
            'Freundlich'
            'Langmuir')
          TabOrder = 0
          OnClick = rgSorptionModelClick
        end
        object btnedFirstDistributionCoefficient: TssButtonEdit
          Left = 6
          Top = 132
          Width = 121
          Height = 23
          Enabled = True
          NumGlyphs = 1
          TabOrder = 1
          Text = '0'
          OnButtonClick = DoFormulaButtonClick
          OnExit = btnedExit
        end
        object btnedSecondDistributionCoefficient: TssButtonEdit
          Left = 6
          Top = 165
          Width = 121
          Height = 23
          Enabled = True
          NumGlyphs = 1
          TabOrder = 2
          Text = '0'
          OnButtonClick = DoFormulaButtonClick
          OnExit = btnedExit
        end
      end
      object rgTransportModel: TRadioGroup
        Left = 3
        Top = 215
        Width = 417
        Height = 105
        Caption = 'Thermal conductivity model (TCMOD)'
        ItemIndex = 0
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
          ItemIndex = 0
          Items.Strings = (
            'None (NONE)'
            'van Genuchten (VGEN)'
            'Brooks-Corey (BCOR)'
            'Piecewise-linear (PLIN)'
            'User-defined (UDEF)')
          TabOrder = 0
          OnClick = rgWatSatFunctClick
        end
        object jplTotalSaturation: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 147
          Width = 667
          Height = 246
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
            Height = 246
            Caption = 'jvspTotSatControls'
            object lblResidWatSat: TLabel
              Left = 164
              Top = 10
              Width = 206
              Height = 15
              Caption = 'Residual total water saturation (SWRES)'
            end
            object lblVgenAlpha: TLabel
              Left = 164
              Top = 41
              Width = 242
              Height = 15
              Caption = 'van Genuchten function parameter '#945'_VG (AA)'
            end
            object lblVgenEta: TLabel
              Left = 164
              Top = 70
              Width = 242
              Height = 15
              Caption = 'van Genuchten function parameter n_VG (VN)'
            end
            object lblAirEntryPressure: TLabel
              Left = 164
              Top = 99
              Width = 133
              Height = 15
              Caption = 'Air-entry pressure (PENT)'
            end
            object lblPoreDistIndex: TLabel
              Left = 164
              Top = 128
              Width = 192
              Height = 15
              Caption = 'Pore size distribution index (RLAMB)'
            end
            object lblPresAtResid: TLabel
              Left = 164
              Top = 157
              Width = 385
              Height = 15
              Caption = 
                'Pressure at which the saturation reaches the residual saturation' +
                ' (PSWRES)'
              WordWrap = True
            end
            object btnedVgenAlpha: TssButtonEdit
              Left = 13
              Top = 36
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 0
              Text = '0.1'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedVgenEta: TssButtonEdit
              Left = 13
              Top = 67
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 1
              Text = '-0.5'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedResidWatSat: TssButtonEdit
              Left = 13
              Top = 7
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 2
              Text = '0.05'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedAirEntryPressure: TssButtonEdit
              Left = 13
              Top = 96
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 3
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedPoreDistIndex: TssButtonEdit
              Left = 13
              Top = 125
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 4
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedPresAtResid: TssButtonEdit
              Left = 13
              Top = 154
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 5
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
          end
          object jvspTotSatUserDefined: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 246
            Caption = 'jvspTotSatUserDefined'
            inline frameTotSatUserDefined: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 246
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 246
              inherited Panel: TPanel
                Top = 205
                Width = 667
                ExplicitTop = 205
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 120
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 120
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
                  MaxValue = 10.000000000000000000
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 205
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = '...'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 20
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                ExplicitWidth = 667
                ExplicitHeight = 205
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
          ItemIndex = 0
          Items.Strings = (
            'None (NONE)'
            'van Genuchten (VGEN)'
            'Brooks-Corey (BCOR)'
            'Piecewise-linear (PLIN)'
            'User-defined (UDEF)')
          TabOrder = 0
          OnClick = rgRelativePermChoiceClick
        end
        object jplRelativePerm: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 147
          Width = 667
          Height = 246
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
            Height = 246
            Caption = 'jvspRelativePermControls'
            object lblMinRelPerm: TLabel
              Left = 164
              Top = 11
              Width = 211
              Height = 15
              Caption = 'Minimum relative permeability (RKMIN)'
            end
            object lblRelPermEta: TLabel
              Left = 164
              Top = 45
              Width = 242
              Height = 15
              Caption = 'van Genuchten function parameter n_VG (VN)'
            end
            object lblRelPermPoreDistIndex: TLabel
              Left = 164
              Top = 79
              Width = 192
              Height = 15
              Caption = 'Pore size distribution index (RLAMB)'
            end
            object lblSatAtMinPerm: TLabel
              Left = 164
              Top = 113
              Width = 418
              Height = 30
              Caption = 
                'Liquid-water saturation at which the relative permeability reach' +
                'es its minimum (SLRKMIN)'
              WordWrap = True
            end
            object btnedMinRelPerm: TssButtonEdit
              Left = 13
              Top = 8
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 0
              Text = '0.001'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedRelPermEta: TssButtonEdit
              Left = 13
              Top = 42
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 1
              Text = '2'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedRelPermPoreDistIndex: TssButtonEdit
              Left = 13
              Top = 76
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 2
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedSatAtMinPerm: TssButtonEdit
              Left = 13
              Top = 110
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 3
              Text = '0.01'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
          end
          object jvspRelativePermUserDefined: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 246
            Caption = 'jvspTotSatUserDefined'
            inline frameRelPermParam: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 246
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 246
              inherited Panel: TPanel
                Top = 205
                Width = 667
                ExplicitTop = 205
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 120
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 120
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
                  MaxValue = 10.000000000000000000
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 205
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = '...'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 20
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                ExplicitWidth = 667
                ExplicitHeight = 205
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
          ItemIndex = 0
          Items.Strings = (
            'None (NONE)'
            'Exponential (EXPO)'
            'Modified power law (POWR)'
            'Piecewise-linear (PLIN)'
            'User-defined (UDEF)')
          TabOrder = 0
          OnClick = rgLiqWatSatChoiceClick
        end
        object jplLiqWatSat: TJvPageList
          AlignWithMargins = True
          Left = 2
          Top = 147
          Width = 667
          Height = 246
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
            Height = 246
            Caption = 'jvspRelativePermControls'
            object lblResidLiqWatSat: TLabel
              Left = 164
              Top = 11
              Width = 229
              Height = 15
              Caption = 'Residual liquid water saturation  (SLSATRES)'
            end
            object lblExpParamW: TLabel
              Left = 164
              Top = 40
              Width = 178
              Height = 15
              Caption = 'Exponential parameter w_EXP (W)'
            end
            object lblPowerAlpha: TLabel
              Left = 164
              Top = 69
              Width = 293
              Height = 15
              Caption = 'Modified power law model parameter, '#945'_POW (ALPHA)'
            end
            object lblPowerBeta: TLabel
              Left = 164
              Top = 98
              Width = 278
              Height = 15
              Caption = 'Modified power law model parameter, '#946'_POW(BETA)'
            end
            object lblLiqWatRelTemSatMin: TLabel
              Left = 164
              Top = 127
              Width = 409
              Height = 30
              Caption = 
                'Relative temperature at which the liquid saturation reaches the ' +
                'residual liquid saturation (TLRES)'
              WordWrap = True
            end
            object btnedResidLiqWatSat: TssButtonEdit
              Left = 13
              Top = 6
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 0
              Text = '0.01'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedExpParamW: TssButtonEdit
              Left = 13
              Top = 37
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 1
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedPowerAlpha: TssButtonEdit
              Left = 13
              Top = 66
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 2
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedPowerBeta: TssButtonEdit
              Left = 13
              Top = 95
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 3
              Text = '0'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
            object btnedLiqWatRelTemSatMin: TssButtonEdit
              Left = 13
              Top = 124
              Width = 145
              Height = 23
              Enabled = True
              NumGlyphs = 1
              TabOrder = 4
              Text = '-2'
              OnButtonClick = DoFormulaButtonClick
              OnExit = btnedExit
            end
          end
          object jvspLiqWatSatParameters: TJvStandardPage
            Left = 0
            Top = 0
            Width = 667
            Height = 246
            Caption = 'jvspTotSatUserDefined'
            inline frameLiquidWaterSatUserDefined: TframeGrid
              Left = 0
              Top = 0
              Width = 667
              Height = 246
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 667
              ExplicitHeight = 246
              inherited Panel: TPanel
                Top = 205
                Width = 667
                ExplicitTop = 205
                ExplicitWidth = 667
                inherited lbNumber: TLabel
                  Width = 120
                  Caption = 'Number of Parameters'
                  ExplicitWidth = 120
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
                  MaxValue = 10.000000000000000000
                end
              end
              inherited Grid: TRbwDataGrid4
                Width = 667
                Height = 205
                Columns = <
                  item
                    AutoAdjustRowHeights = False
                    AutoAdjustCaptionRowHeights = False
                    ButtonCaption = '...'
                    ButtonFont.Charset = DEFAULT_CHARSET
                    ButtonFont.Color = clWindowText
                    ButtonFont.Height = -11
                    ButtonFont.Name = 'Tahoma'
                    ButtonFont.Style = []
                    ButtonUsed = False
                    ButtonWidth = 20
                    CheckMax = False
                    CheckMin = False
                    ComboUsed = False
                    Format = rcf4Real
                    LimitToList = False
                    MaxLength = 0
                    ParentButtonFont = False
                    WordWrapCaptions = True
                    WordWrapCells = False
                    CaseSensitivePicklist = False
                    CheckStyle = csCheck
                    AutoAdjustColWidths = True
                  end>
                ExplicitWidth = 667
                ExplicitHeight = 205
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
          Top = 27
          Width = 290
          Height = 15
          Caption = 'Maximum freezing temperature of pore water (TFREEZ)'
        end
        object lblLatentHeat: TLabel
          Left = 164
          Top = 61
          Width = 154
          Height = 15
          Caption = 'Latent heat of fusion (HTLAT)'
        end
        object btnedMaxFreezeTemp: TssButtonEdit
          Left = 13
          Top = 24
          Width = 145
          Height = 23
          Enabled = True
          NumGlyphs = 1
          TabOrder = 0
          Text = '0'
          OnButtonClick = DoFormulaButtonClick
          OnExit = btnedExit
        end
        object btnedLatentHeat: TssButtonEdit
          Left = 13
          Top = 58
          Width = 145
          Height = 23
          Enabled = True
          NumGlyphs = 1
          TabOrder = 1
          Text = '334000'
          OnButtonClick = DoFormulaButtonClick
          OnExit = btnedExit
        end
      end
    end
  end
end
