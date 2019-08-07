object frmParEstSettings: TfrmParEstSettings
  Left = 50
  Top = 50
  BorderStyle = bsDialog
  Caption = 'UCODE Parameter-Estimation Settings'
  ClientHeight = 675
  ClientWidth = 872
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object lblTolpar: TLabel
    Left = 131
    Top = 44
    Width = 355
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'TolPar: Default parameter-change closure tolerance'
  end
  object lblTolSOSC: TLabel
    Left = 131
    Top = 76
    Width = 306
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'TolSOSC: Sum-of-squares closure tolerance'
  end
  object Label2: TLabel
    Left = 131
    Top = 11
    Width = 419
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MaxIter: Maximum number of parameter-estimation iterations'
  end
  object Label3: TLabel
    Left = 131
    Top = 108
    Width = 451
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MaxChange: Default maximum fractional parameter-value change'
  end
  object Label4: TLabel
    Left = 131
    Top = 140
    Width = 303
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'MaxChangeRealm: Maximum-change realm'
  end
  object lblMSR: TLabel
    Left = 131
    Top = 242
    Width = 732
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
    Caption = 
      'MinimumSensRatio: Parameters with smaller CSS ratio are removed ' +
      'from estimation'
    WordWrap = True
  end
  object lblRSR: TLabel
    Left = 131
    Top = 277
    Width = 732
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    AutoSize = False
    Caption = 
      'ReincludeSensRatio: Omitted parameters with larger CSS ratio are' +
      ' returned to estimation'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 131
    Top = 310
    Width = 732
    Height = 61
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 224
    Top = 530
    Width = 639
    Height = 60
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Model-generated numeric values that should be ignored as simulat' +
      'ed equivalents to observations in the regression.  Use Down and ' +
      'Up arrow keys to view and extend list'
    WordWrap = True
  end
  object edtTolPar: TEdit
    Left = 7
    Top = 40
    Width = 119
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
    Text = '0.01'
    OnChange = edtTolParChange
    OnClick = edtTolParClick
  end
  object edtTolSOSC: TEdit
    Left = 7
    Top = 72
    Width = 119
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 1
    Text = '0.0'
    OnChange = edtTolSOSCChange
    OnClick = edtTolSOSCClick
  end
  object edtMaxChange: TEdit
    Left = 7
    Top = 105
    Width = 119
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 2
    Text = '2.0'
    OnChange = edtMaxChangeChange
    OnClick = edtMaxChangeClick
  end
  object cmbMaxChangeRealm: TComboBox
    Left = 7
    Top = 137
    Width = 119
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
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
    Left = 10
    Top = 599
    Width = 195
    Height = 28
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Reset All Values to Defaults'
    TabOrder = 11
    OnClick = btnUcodeParEstResetDefaultsClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 648
    Width = 872
    Height = 27
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 9
    Top = 373
    Width = 853
    Height = 149
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Trust Region'
    TabOrder = 9
    object lblMaxStep: TLabel
      Left = 368
      Top = 78
      Width = 279
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MaxStep: Maximum Allowable Step Size'
    end
    object lblConsecmax: TLabel
      Left = 368
      Top = 105
      Width = 455
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'ConsecMax: Max. number of times MaxStep is used consecutively'
      WordWrap = True
    end
    object lblDoglegOptions: TLabel
      Left = 33
      Top = 51
      Width = 337
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'The following settings apply to the Dogleg option:'
    end
    object cbTrustRegion: TCheckBox
      Left = 4
      Top = 23
      Width = 475
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Use Trust Region modification of Gauss-Newton regression:'
      TabOrder = 0
      OnClick = cbTrustRegionClick
    end
    object rbDogleg: TRadioButton
      Left = 485
      Top = 23
      Width = 79
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Dogleg'
      Checked = True
      TabOrder = 4
      TabStop = True
      OnClick = rbDoglegClick
    end
    object rbHookStep: TRadioButton
      Left = 566
      Top = 23
      Width = 103
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Hookstep'
      TabOrder = 5
      OnClick = rbHookStepClick
    end
    object edtMaxStep: TEdit
      Left = 297
      Top = 74
      Width = 65
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
    end
    object seConsecmax: TSpinEdit
      Left = 297
      Top = 103
      Width = 65
      Height = 28
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 5
    end
    object cbUseMaxStep: TCheckBox
      Left = 33
      Top = 74
      Width = 253
      Height = 47
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Limit dogleg step size to MaxStep'
      TabOrder = 1
      WordWrap = True
      OnClick = cbUseMaxStepClick
    end
  end
  object cbStatsOnNonConverge: TCheckBox
    Left = 110
    Top = 171
    Width = 753
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Stats_On_Nonconverge: On nonconvergence, calculate final sensiti' +
      'vites and statistics'
    TabOrder = 4
  end
  object cbOmitInsensitive: TCheckBox
    Left = 110
    Top = 196
    Width = 753
    Height = 40
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'OmitInsensitive: Use composite scaled sensitivities (CSS) to omi' +
      't and reinclude parameters in regression'
    TabOrder = 5
    WordWrap = True
    OnClick = cbOmitInsensitiveClick
  end
  object edtMinSensRatio: TEdit
    Left = 30
    Top = 239
    Width = 96
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 6
  end
  object edtReincludeSensRatio: TEdit
    Left = 30
    Top = 273
    Width = 96
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 7
  end
  object edtTolParWtOS: TEdit
    Left = 7
    Top = 310
    Width = 119
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 8
  end
  object btnAdvanced: TButton
    Left = 556
    Top = 599
    Width = 195
    Height = 28
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Advanced Settings...'
    TabOrder = 12
    OnClick = btnAdvancedClick
  end
  object dgOmitVals: TEcDataGrid
    Left = 10
    Top = 526
    Width = 207
    Height = 67
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 399
    Top = 615
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 14
  end
  object btnOK: TBitBtn
    Left = 303
    Top = 615
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 13
    OnClick = btnOKClick
  end
  object spedMaxIter: TSpinEdit
    Left = 7
    Top = 7
    Width = 119
    Height = 28
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxValue = 0
    MinValue = 0
    TabOrder = 16
    Value = 0
    OnChange = spedMaxIterChange
    OnClick = spedMaxIterClick
  end
end
