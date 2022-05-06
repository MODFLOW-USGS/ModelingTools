object frameSutraRegionalProperty: TframeSutraRegionalProperty
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object jplMain: TJvPageList
    Left = 0
    Top = 0
    Width = 640
    Height = 480
    ActivePage = jvspWaterSaturation
    PropagateEnable = False
    Align = alClient
    ExplicitLeft = 176
    ExplicitTop = 136
    ExplicitWidth = 300
    ExplicitHeight = 200
    object jvspAdsorbtion: TJvStandardPage
      Left = 0
      Top = 0
      Width = 640
      Height = 480
      Caption = 'jvspAdsorbtion'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpAdsorption: TGroupBox
        Left = 0
        Top = 0
        Width = 640
        Height = 193
        Align = alTop
        Caption = 'Adsorption parameters'
        TabOrder = 0
        ExplicitTop = 8
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
    object jvspWaterSaturation: TJvStandardPage
      Left = 0
      Top = 0
      Width = 640
      Height = 480
      Caption = 'jvspWaterSaturation'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpWaterSaturation: TGroupBox
        Left = 0
        Top = 0
        Width = 640
        Height = 480
        Align = alClient
        Caption = 'Total Water Saturation Parameters'
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 40
        object lblResidWatSat: TLabel
          Left = 167
          Top = 146
          Width = 263
          Height = 20
          Caption = 'Residual total water saturation (SWRES)'
        end
        object lblVgenAlpha: TLabel
          Left = 167
          Top = 172
          Width = 328
          Height = 20
          Caption = 'van Genuchten function parameter alpha_VG (AA)'
        end
        object lblVgenEta: TLabel
          Left = 167
          Top = 200
          Width = 312
          Height = 20
          Caption = 'van Genuchten function parameter eta_VG (VN)'
        end
        object rgWatSatFunct: TRadioGroup
          Left = 14
          Top = 29
          Width = 417
          Height = 105
          Caption = 'Specified total water saturation function (SWMOD)'
          Items.Strings = (
            'None'
            'Linear'
            'Freundlich'
            'Langmuir')
          TabOrder = 0
        end
        object rdeResidWatSat: TRbwDataEntry
          Left = 16
          Top = 144
          Width = 145
          Height = 22
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object rdeVgenAlpha: TRbwDataEntry
          Left = 14
          Top = 172
          Width = 145
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object rdeVgenEta: TRbwDataEntry
          Left = 14
          Top = 200
          Width = 145
          Height = 22
          TabOrder = 3
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
    end
  end
end
