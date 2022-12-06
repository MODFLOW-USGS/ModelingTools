inherited framePackageUZF: TframePackageUZF
  Width = 548
  Height = 514
  ExplicitWidth = 548
  ExplicitHeight = 514
  DesignSize = (
    548
    514)
  inherited memoComments: TMemo
    Width = 517
    Height = 67
    ExplicitWidth = 517
    ExplicitHeight = 67
  end
  inherited pnLayerOption: TPanel
    Top = 144
    Width = 548
    Height = 370
    ExplicitTop = 144
    ExplicitWidth = 548
    ExplicitHeight = 370
    inherited lblLayerOption: TLabel
      Top = 5
      Width = 266
      Caption = 'Recharge and discharge location option (NUZTOP)'
      ExplicitTop = 5
      ExplicitWidth = 266
    end
    object lblVerticalKSource: TLabel [1]
      Left = 16
      Top = 45
      Width = 254
      Height = 15
      Caption = 'Vertical hydraulic conductivity source (IUZFOPT)'
      Enabled = False
    end
    object lblNumberOfTrailingWaves: TLabel [2]
      Left = 16
      Top = 96
      Width = 189
      Height = 15
      Caption = 'Number of trailing waves (NTRAIL2)'
      Enabled = False
    end
    object lblNumberOfWaveSets: TLabel [3]
      Left = 16
      Top = 143
      Width = 161
      Height = 15
      Caption = 'Number of wave sets (NSETS2)'
      Enabled = False
    end
    object lblSURFDEP: TLabel [4]
      Left = 16
      Top = 286
      Width = 216
      Height = 30
      Caption = 
        'The average height of undulations in the land surface altitude (' +
        'SURFDEP) '
      Enabled = False
      WordWrap = True
    end
    object lblET_SmoothingFactor: TLabel [5]
      Left = 288
      Top = 318
      Width = 187
      Height = 15
      Caption = 'ET smoothing interval (smoothfact)'
    end
    inherited comboLayerOption: TComboBox
      Left = 16
      Top = 24
      ItemIndex = -1
      Text = ''
      Items.Strings = (
        'Top layer (1)'
        'Specified layer (2)'
        'Top active cell (3)')
      ExplicitLeft = 16
      ExplicitTop = 24
      ExplicitHeight = 23
    end
    object comboVerticalKSource: TComboBox
      Left = 16
      Top = 69
      Width = 255
      Height = 23
      Style = csDropDownList
      Enabled = False
      TabOrder = 1
      Items.Strings = (
        'Specify vertical hydraulic conductivity (1)'
        'Use vertical hydraulic conductivity from flow package (2)')
    end
    object rdeNumberOfTrailingWaves: TRbwDataEntry
      Left = 16
      Top = 115
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 3
      Text = '15'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeNumberOfWaveSets: TRbwDataEntry
      Left = 16
      Top = 162
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 4
      Text = '20'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object rdeSURFDEP: TRbwDataEntry
      Left = 16
      Top = 343
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 6
      Text = '1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object chklstOptions: TCheckListBox
      Left = 16
      Top = 190
      Width = 517
      Height = 67
      Enabled = False
      ItemHeight = 15
      Items.Strings = (
        ' Route discharge to streams, lakes, or SWR reaches (IRUNFLG)'
        'Simulate evapotranspiration (IETFLG)'
        'Print summary of UZF budget terms (IFTUNIT)'
        'Specify residual water content (SPECIFYTHTR)'
        'Specify initial unsaturated water content (SPECIFYTHTI)'
        'Calculate surface leakage (inverse of NOSURFLEAK)'
        
          'Calculate rejected infiltration using surficial hydraulic conduc' +
          'tivity (REJECTSURFK)'
        
          'Calculate surface leakage using surficial hydraulic conductivity' +
          ' (SEEPSURFK)'
        'Calculate ET using smoothing interval (ETSQUARE)'
        'Write discharge and recharge (NETFLUX)')
      TabOrder = 5
      OnClickCheck = chklstOptionsClickCheck
    end
    object rgAssignmentMethod: TRadioGroup
      Left = 240
      Top = 104
      Width = 293
      Height = 80
      Caption = 'Infiltration assignment method'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'Objects overwrite values of previous objects'
        'Sum values of all objects')
      TabOrder = 2
      WordWrap = True
    end
    object rdeET_SmoothingFactor: TRbwDataEntry
      Left = 288
      Top = 343
      Width = 145
      Height = 22
      TabOrder = 7
      Text = '0.1'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMax = True
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = chklstOptions
      end
      item
        Control = comboLayerOption
      end
      item
        Control = lblLayerOption
      end
      item
        Control = rdeSURFDEP
      end
      item
        Control = lblSURFDEP
      end
      item
        Control = rdeNumberOfTrailingWaves
      end
      item
        Control = lblNumberOfTrailingWaves
      end
      item
        Control = rdeNumberOfWaveSets
      end
      item
        Control = lblNumberOfWaveSets
      end
      item
        Control = comboVerticalKSource
      end
      item
        Control = lblVerticalKSource
      end
      item
        Control = rgAssignmentMethod
      end>
  end
end
