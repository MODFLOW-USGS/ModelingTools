object frmUcodeSettings: TfrmUcodeSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Settings'
  ClientHeight = 570
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 19
  object btnUcodeResetDefaults: TButton
    Left = 6
    Top = 542
    Width = 237
    Height = 25
    Caption = 'Reset All Values to Defaults'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = btnUcodeResetDefaultsClick
  end
  object btnCancel: TBitBtn
    Left = 415
    Top = 542
    Width = 90
    Height = 25
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 5
  end
  object btnOK: TBitBtn
    Left = 320
    Top = 542
    Width = 89
    Height = 25
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 4
    OnClick = btnOKClick
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 2
    Width = 342
    Height = 135
    Caption = 'Model Name and Measurement Units'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lblModelName: TLabel
      Left = 15
      Top = 25
      Width = 77
      Height = 16
      Caption = 'Model Name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lblModelLenUnit: TLabel
      Left = 15
      Top = 52
      Width = 109
      Height = 16
      Caption = 'Model Length Unit:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lblModelTimeUnit: TLabel
      Left = 16
      Top = 79
      Width = 98
      Height = 16
      Caption = 'Model Time Unit:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lblModelMassUnit: TLabel
      Left = 16
      Top = 105
      Width = 102
      Height = 16
      Caption = 'Model Mass Unit:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object edtModelName: TEdit
      Left = 144
      Top = 22
      Width = 190
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = edtModelNameChange
    end
    object cmbModelLenUnit: TComboBox
      Left = 144
      Top = 49
      Width = 93
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = cmbModelLenUnitChange
      Items.Strings = (
        'ft'
        'cm'
        'm'
        'km'
        'mi')
    end
    object cmbModelTimeUnit: TComboBox
      Left = 144
      Top = 76
      Width = 93
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = cmbModelTimeUnitChange
      Items.Strings = (
        's'
        'min'
        'd'
        'mo'
        'yr')
    end
    object cmbModelMassUnit: TComboBox
      Left = 144
      Top = 103
      Width = 93
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnChange = cmbModelMassUnitChange
      Items.Strings = (
        'mg'
        'g'
        'kg'
        'lb'
        'slug')
    end
  end
  object GroupBox2: TGroupBox
    Left = 6
    Top = 143
    Width = 342
    Height = 392
    Caption = 'UCODE Functionality'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lblStdErrOne: TLabel
      Left = 8
      Top = 20
      Width = 315
      Height = 23
      Caption = 'Calculation of Statistics:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object cmbStdErrOne: TComboBox
      Left = 8
      Top = 38
      Width = 307
      Height = 24
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = cmbStdErrOneChange
      Items.Strings = (
        'Standard error calculated from observation data'
        'Use 1.0 instead of calculated standard error')
    end
    object gpbxCalcSens: TGroupBox
      Left = 6
      Top = 68
      Width = 329
      Height = 82
      Caption = 'Calculation of Sensitivities'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object ckbxModelDeriv: TCheckBox
        Left = 9
        Top = 20
        Width = 149
        Height = 59
        Caption = 'Use model-calculated derivatives'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = True
        OnClick = ckbxModelDerivClick
      end
      object btnDerivInterface: TButton
        Left = 164
        Top = 20
        Width = 159
        Height = 55
        Caption = 'Set Up Model-Calculated Derivatives...'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        WordWrap = True
      end
    end
    object gpbxInvObjFn: TGroupBox
      Left = 6
      Top = 156
      Width = 329
      Height = 115
      Caption = 'Investigate Objective Function Mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object lblSosFile: TLabel
        Left = 7
        Top = 64
        Width = 52
        Height = 16
        Caption = 'SOS file:'
      end
      object lblSosMethod: TLabel
        Left = 7
        Top = 18
        Width = 161
        Height = 16
        Caption = 'Source of parameter values:'
        WordWrap = True
      end
      object btnBrowseSOS: TSpeedButton
        Left = 300
        Top = 82
        Width = 23
        Height = 22
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FF078DBE
          078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078DBE078D
          BE078DBEFF00FFFF00FF078DBE25A1D172C7E785D7FA66CDF965CDF965CDF965
          CDF965CDF865CDF965CDF866CEF939ADD8078DBEFF00FFFF00FF078DBE4CBCE7
          39A8D1A0E2FB6FD4FA6FD4F96ED4FA6FD4F96FD4FA6FD4FA6FD4FA6ED4F93EB1
          D984D7EB078DBEFF00FF078DBE72D6FA078DBEAEEAFC79DCFB79DCFB79DCFB79
          DCFB79DCFB7ADCFB79DCFA79DCFA44B5D9AEF1F9078DBEFF00FF078DBE79DDFB
          1899C79ADFF392E7FB84E4FB83E4FC83E4FC84E4FC83E4FC83E4FB84E5FC48B9
          DAB3F4F9078DBEFF00FF078DBE82E3FC43B7DC65C3E0ACF0FD8DEBFC8DEBFC8D
          EBFD8DEBFD8DEBFC8DEBFD0C85184CBBDAB6F7F96DCAE0078DBE078DBE8AEAFC
          77DCF3229CC6FDFFFFC8F7FEC9F7FEC9F7FEC9F7FEC8F7FE0C85183CBC5D0C85
          18DEF9FBD6F6F9078DBE078DBE93F0FE93F0FD1697C5078DBE078DBE078DBE07
          8DBE078DBE0C851852D97F62ED9741C4650C8518078DBE078DBE078DBE9BF5FE
          9AF6FE9AF6FE9BF5FD9BF6FE9AF6FE9BF5FE0C851846CE6C59E48858E18861EB
          9440C1650C8518FF00FF078DBEFEFEFEA0FBFFA0FBFEA0FBFEA1FAFEA1FBFE0C
          85180C85180C85180C851856E18447CD6E0C85180C85180C8518FF00FF078DBE
          FEFEFEA5FEFFA5FEFFA5FEFF078CB643B7DC43B7DC43B7DC0C85184EDD7936BA
          540C8518FF00FFFF00FFFF00FFFF00FF078DBE078DBE078DBE078DBEFF00FFFF
          00FFFF00FFFF00FF0C851840D0650C8518FF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85182AB7432DBA490C85
          18FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FF0C851821B5380C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C85180C8518FF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C85180C85180C
          85180C8518FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      end
      object edtSosFile: TEdit
        Left = 6
        Top = 81
        Width = 294
        Height = 24
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = edtSosFileChange
      end
      object cmbSosMethod: TComboBox
        Left = 7
        Top = 37
        Width = 317
        Height = 24
        Style = csDropDownList
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'Parameter Constraints and SOSIncrement Values'
        OnChange = cmbSosMethodChange
        Items.Strings = (
          'Parameter Constraints and SOSIncrement Values'
          'SOS File')
      end
    end
    object gpbxLinAdv: TGroupBox
      Left = 6
      Top = 273
      Width = 329
      Height = 64
      Caption = 'Advanced Test Model Linearity Mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object lblConfPred: TLabel
        Left = 17
        Top = 19
        Width = 196
        Height = 16
        Caption = 'Confidence or Prediction Intervals:'
      end
      object ComboBox1: TComboBox
        Left = 17
        Top = 35
        Width = 277
        Height = 24
        Style = csDropDownList
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'Confidence intervals'
        Items.Strings = (
          'Confidence intervals'
          'Prediction intervals')
      end
    end
    object gpbxPred: TGroupBox
      Left = 6
      Top = 338
      Width = 329
      Height = 40
      Caption = 'Prediction Mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      object ckbxPredSen: TCheckBox
        Left = 17
        Top = 18
        Width = 235
        Height = 17
        Caption = 'Calculate Prediction Sensitivities'
        TabOrder = 0
        OnClick = ckbxPredSenClick
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 354
    Top = 2
    Width = 433
    Height = 533
    Caption = 'Output Options'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object lblOutputVerbosity: TLabel
      Left = 8
      Top = 17
      Width = 98
      Height = 16
      Caption = 'Output verbosity:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object cmbVerbose: TComboBox
      Left = 8
      Top = 36
      Width = 409
      Height = 24
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 3
      ParentFont = False
      TabOrder = 0
      Text = '3 - Warnings, notes, selected input'
      OnChange = cmbVerboseChange
      Items.Strings = (
        '0 - No extraneous output'
        '1 - Warnings'
        '2 - Warnings, notes'
        '3 - Warnings, notes, selected input'
        '4 - Warnings, notes, all input'
        '5 - All available output')
    end
    object gpbxObs: TGroupBox
      Left = 8
      Top = 68
      Width = 416
      Height = 77
      Caption = 'Printing of Observations, Simulated Values, Residuals'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object ckbxStartRes: TCheckBox
        Left = 6
        Top = 19
        Width = 231
        Height = 17
        Caption = 'Print for starting parameter values'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ckbxStartResClick
      end
      object ckbxIntRes: TCheckBox
        Left = 6
        Top = 37
        Width = 305
        Height = 17
        Caption = 'Print after each parameter-estimation iteration'
        TabOrder = 1
        OnClick = ckbxIntResClick
      end
      object ckbxFinalRes: TCheckBox
        Left = 6
        Top = 55
        Width = 275
        Height = 17
        Caption = 'Print for final parameter values'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = ckbxFinalResClick
      end
    end
    object gpbxSen: TGroupBox
      Left = 8
      Top = 150
      Width = 416
      Height = 168
      Caption = 'Printing of Sensitivity Tables'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object lblStartSens: TLabel
        Left = 6
        Top = 18
        Width = 198
        Height = 16
        Caption = 'Print for starting parameter values:'
      end
      object lblIntSens: TLabel
        Left = 6
        Top = 67
        Width = 268
        Height = 16
        Caption = 'Print after each parameter-estimation iteration:'
      end
      object lblFinalSens: TLabel
        Left = 6
        Top = 118
        Width = 178
        Height = 16
        Caption = 'Print for final parameter values:'
      end
      object cmbStartSens: TComboBox
        Left = 6
        Top = 36
        Width = 403
        Height = 24
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = cmbStartSensChange
        Items.Strings = (
          'Composite scaled sensitivities'
          'Dimensionless and composite scaled sensitivities'
          
            'One-percent scaled sensitivities and composite scaled sensitivit' +
            'ies'
          'Composite, dimensionless, and one-percent scaled sensitivities'
          'Unscaled sensitivities and composite scaled sensitivities'
          'All sensitivity tables'
          'No sensitivity tables')
      end
      object cmbIntSens: TComboBox
        Left = 6
        Top = 86
        Width = 403
        Height = 24
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = cmbIntSensChange
        Items.Strings = (
          'Composite scaled sensitivities'
          'Dimensionless and composite scaled sensitivities'
          
            'One-percent scaled sensitivities and composite scaled sensitivit' +
            'ies'
          'Composite, dimensionless, and one-percent scaled sensitivities'
          'Unscaled sensitivities and composite scaled sensitivities'
          'All sensitivity tables'
          'No sensitivity tables')
      end
      object cmbFinalSens: TComboBox
        Left = 6
        Top = 137
        Width = 403
        Height = 24
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnChange = cmbFinalSensChange
        Items.Strings = (
          'Composite scaled sensitivities'
          'Dimensionless and composite scaled sensitivities'
          
            'One-percent scaled sensitivities and composite scaled sensitivit' +
            'ies'
          'Composite, dimensionless, and one-percent scaled sensitivities'
          'Unscaled sensitivities and composite scaled sensitivities'
          'All sensitivity tables'
          'No sensitivity tables')
      end
    end
    object gpbxDataEx: TGroupBox
      Left = 8
      Top = 328
      Width = 416
      Height = 136
      Caption = 'Data-Exchange Files'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object ckbxDataEx: TCheckBox
        Left = 6
        Top = 15
        Width = 407
        Height = 41
        Caption = 
          'Generate data-exchange files for graphical and numerical analysi' +
          's'
        Checked = True
        State = cbChecked
        TabOrder = 0
        WordWrap = True
        OnClick = ckbxDataExClick
      end
      object ckbxCreateInit: TCheckBox
        Left = 6
        Top = 57
        Width = 404
        Height = 77
        Caption = 
          'Generate "init" files (but no other data-exchange files) when ru' +
          'nning Sensitivity-Analysis Mode in preparation for analysis of n' +
          'onlinear confidence intervals or advanced evaluation of residual' +
          's and nonlinearity.'
        TabOrder = 1
        WordWrap = True
        OnClick = ckbxCreateInitClick
      end
    end
    object ckbxEigenValues: TCheckBox
      Left = 8
      Top = 511
      Width = 246
      Height = 17
      Caption = 'Calculate and Print Eigenvalues'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 6
      OnClick = ckbxEigenValuesClick
    end
    object cbxWriteDerivedParams: TCheckBox
      Left = 8
      Top = 475
      Width = 225
      Height = 17
      Caption = 'Write Derived Parameters'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 4
      OnClick = cbxWriteDerivedParamsClick
    end
    object cbxWritePriorInfo: TCheckBox
      Left = 8
      Top = 493
      Width = 209
      Height = 17
      Caption = 'Write Prior Information'
      Checked = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      State = cbChecked
      TabOrder = 5
      OnClick = cbxWritePriorInfoClick
    end
  end
end
