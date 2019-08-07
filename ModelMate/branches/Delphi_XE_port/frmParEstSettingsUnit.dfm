object frmParEstSettings: TfrmParEstSettings
  Left = 50
  Top = 50
  BorderStyle = bsDialog
  Caption = 'UCODE Parameter-Estimation Settings'
  ClientHeight = 568
  ClientWidth = 714
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblTolpar: TLabel
    Left = 110
    Top = 37
    Width = 298
    Height = 16
    Caption = 'TolPar: Default parameter-change closure tolerance'
  end
  object lblTolSOSC: TLabel
    Left = 110
    Top = 64
    Width = 254
    Height = 16
    Caption = 'TolSOSC: Sum-of-squares closure tolerance'
  end
  object Label2: TLabel
    Left = 110
    Top = 9
    Width = 355
    Height = 16
    Caption = 'MaxIter: Maximum number of parameter-estimation iterations'
  end
  object Label3: TLabel
    Left = 110
    Top = 91
    Width = 376
    Height = 16
    Caption = 'MaxChange: Default maximum fractional parameter-value change'
  end
  object Label4: TLabel
    Left = 110
    Top = 118
    Width = 249
    Height = 16
    Caption = 'MaxChangeRealm: Maximum-change realm'
  end
  object Label5: TLabel
    Left = 110
    Top = 193
    Width = 559
    Height = 22
    AutoSize = False
    Caption = 
      'MinimumSensRatio: Parameters with smaller CSS ratio are removed ' +
      'from estimation'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 110
    Top = 222
    Width = 555
    Height = 22
    AutoSize = False
    Caption = 
      'ReincludeSensRatio: Omitted parameters with larger CSS ratio are' +
      ' returned to estimation'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 110
    Top = 256
    Width = 555
    Height = 51
    AutoSize = False
    Caption = 
      'TolParWtOS: Applies to observations with StatFlag = CV and WtOSC' +
      'onstant > 0. If the parameter change in a parameter estimation i' +
      'teration is less than TolParWtOS '#215' TolPar, simulated values are ' +
      'used to calculate the weights. Otherwise, observed values are us' +
      'ed.'
    WordWrap = True
  end
  object lblOmitVals: TLabel
    Left = 189
    Top = 444
    Width = 340
    Height = 48
    Caption = 
      'Model-generated numeric values that should be ignored as simulat' +
      'ed equivalents to observations in the regression.  Use Down and ' +
      'Up arrow keys to view and extend list'
    WordWrap = True
  end
  object edtTolPar: TEdit
    Left = 6
    Top = 34
    Width = 100
    Height = 24
    TabOrder = 0
    Text = '0.01'
    OnChange = edtTolParChange
    OnClick = edtTolParClick
  end
  object edtTolSOSC: TEdit
    Left = 6
    Top = 61
    Width = 100
    Height = 24
    TabOrder = 1
    Text = '0.0'
    OnChange = edtTolSOSCChange
    OnClick = edtTolSOSCClick
  end
  object edtMaxChange: TEdit
    Left = 6
    Top = 88
    Width = 100
    Height = 24
    TabOrder = 2
    Text = '2.0'
    OnChange = edtMaxChangeChange
    OnClick = edtMaxChangeClick
  end
  object cmbMaxChangeRealm: TComboBox
    Left = 6
    Top = 115
    Width = 100
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    ItemIndex = 0
    TabOrder = 3
    Text = 'Native'
    OnChange = cmbMaxChangeRealmChange
    OnClick = cmbMaxChangeRealmClick
    Items.Strings = (
      'Native'
      'Regression')
  end
  object btnUcodeParEstResetDefaults: TButton
    Left = 8
    Top = 501
    Width = 165
    Height = 24
    Caption = 'Reset All Values to Defaults'
    TabOrder = 11
    OnClick = btnUcodeParEstResetDefaultsClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 546
    Width = 714
    Height = 22
    Panels = <
      item
        Alignment = taCenter
        Text = 'Status'
        Width = 50
      end
      item
        Width = 50
      end>
    ParentFont = True
    UseSystemFont = False
    OnClick = StatusBar1Click
  end
  object gbTrustRegion: TGroupBox
    Left = 8
    Top = 313
    Width = 698
    Height = 120
    Caption = 'Trust Region'
    TabOrder = 9
    object lblMaxStep: TLabel
      Left = 310
      Top = 63
      Width = 230
      Height = 16
      Caption = 'MaxStep: Maximum Allowable Step Size'
    end
    object lblConsecmax: TLabel
      Left = 310
      Top = 87
      Width = 374
      Height = 16
      Caption = 'ConsecMax: Max. number of times MaxStep is used consecutively'
    end
    object lblDoglegOptions: TLabel
      Left = 28
      Top = 43
      Width = 283
      Height = 16
      Caption = 'The following settings apply to the Dogleg option:'
    end
    object cbTrustRegion: TCheckBox
      Left = 11
      Top = 17
      Width = 362
      Height = 17
      Caption = 'Use Trust Region modification of Gauss-Newton regression:'
      TabOrder = 0
      OnClick = cbTrustRegionClick
    end
    object rbDogleg: TRadioButton
      Left = 376
      Top = 17
      Width = 67
      Height = 17
      Caption = 'Dogleg'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = rbDoglegClick
    end
    object rbHookStep: TRadioButton
      Left = 445
      Top = 17
      Width = 86
      Height = 17
      Caption = 'Hookstep'
      TabOrder = 5
      OnClick = rbHookStepClick
    end
    object edtMaxStep: TEdit
      Left = 250
      Top = 60
      Width = 55
      Height = 24
      TabOrder = 2
    end
    object seConsecmax: TSpinEdit
      Left = 250
      Top = 85
      Width = 55
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 5
    end
    object cbUseMaxStep: TCheckBox
      Left = 28
      Top = 62
      Width = 213
      Height = 17
      Caption = 'Limit dogleg step size to MaxStep'
      TabOrder = 1
      OnClick = cbUseMaxStepClick
    end
  end
  object cbStatsOnNonConverge: TCheckBox
    Left = 93
    Top = 144
    Width = 574
    Height = 17
    Caption = 
      'Stats_On_Nonconverge: On nonconvergence, calculate final sensiti' +
      'vites and statistics'
    TabOrder = 4
  end
  object cbOmitInsensitive: TCheckBox
    Left = 93
    Top = 167
    Width = 580
    Height = 17
    Caption = 
      'OmitInsensitive: Use composite scaled sensitivities to omit and ' +
      'reinclude parameters in regression'
    TabOrder = 5
    OnClick = cbOmitInsensitiveClick
  end
  object edtMinSensRatio: TEdit
    Left = 25
    Top = 190
    Width = 81
    Height = 24
    TabOrder = 6
  end
  object edtReincludeSensRatio: TEdit
    Left = 25
    Top = 219
    Width = 81
    Height = 24
    TabOrder = 7
  end
  object edtTolParWtOS: TEdit
    Left = 6
    Top = 256
    Width = 100
    Height = 24
    TabOrder = 8
  end
  object btnAdvanced: TButton
    Left = 468
    Top = 501
    Width = 164
    Height = 24
    Caption = 'Advanced Settings...'
    TabOrder = 12
    OnClick = btnAdvancedClick
  end
  object dgOmitVals: TDataGrid
    Left = 8
    Top = 439
    Width = 175
    Height = 56
    ColCount = 1
    DefaultColWidth = 150
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goEditing]
    ScrollBars = ssVertical
    TabOrder = 10
    OnExit = dgOmitValsExit
    Columns = <
      item
        Title.WordWrap = False
      end>
    DataGridOptions = [dgoAppendRow, dgoInsertRow, dgoDeleteRow]
    RowCountMin = 1
    SelectedIndex = 0
    Version = '2.0'
  end
  object btnCancel: TBitBtn
    Left = 336
    Top = 518
    Width = 75
    Height = 25
    TabOrder = 14
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 255
    Top = 518
    Width = 75
    Height = 25
    TabOrder = 13
    OnClick = btnOKClick
    Kind = bkOK
  end
  object spedMaxIter: TSpinEdit
    Left = 6
    Top = 6
    Width = 100
    Height = 26
    MaxValue = 0
    MinValue = 0
    TabOrder = 16
    Value = 0
    OnChange = spedMaxIterChange
    OnClick = spedMaxIterClick
  end
end
