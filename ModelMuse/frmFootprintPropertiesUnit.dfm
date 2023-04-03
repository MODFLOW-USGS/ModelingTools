inherited frmFootprintProperties: TfrmFootprintProperties
  HelpType = htKeyword
  HelpKeyword = 'Footprint_Properties_Dialog_Bo'
  Caption = 'WellFootprint Properties'
  ClientHeight = 262
  ClientWidth = 468
  ExplicitWidth = 480
  ExplicitHeight = 300
  TextHeight = 18
  object pnl1: TPanel
    Left = 0
    Top = 221
    Width = 468
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 220
    ExplicitWidth = 464
    DesignSize = (
      468
      41)
    object btnHelp: TBitBtn
      Left = 123
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 119
    end
    object btnOK: TBitBtn
      Left = 237
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 233
    end
    object btnCancel: TBitBtn
      Left = 351
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 347
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 221
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 464
    ExplicitHeight = 220
    object lblClosureCriterion: TLabel
      Left = 16
      Top = 82
      Width = 114
      Height = 18
      Caption = 'Closure criterion'
    end
    object lblMaxIteration: TLabel
      Left = 16
      Top = 134
      Width = 208
      Height = 18
      Caption = 'Maximum number of iterations'
    end
    object lblRedistribution: TLabel
      Left = 16
      Top = 184
      Width = 156
      Height = 18
      Caption = 'Redistribution criterion'
    end
    object lblMinDepthRateIndex: TLabel
      Left = 16
      Top = 110
      Width = 179
      Height = 18
      Caption = 'Minimum depth-rate index'
    end
    object rdeClosureCriterion: TRbwDataEntry
      Left = 266
      Top = 79
      Width = 145
      Height = 22
      TabOrder = 3
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMax = True
      CheckMin = True
      ChangeDisabledColor = True
    end
    object seMaxIteration: TJvSpinEdit
      Left = 266
      Top = 135
      Width = 145
      Height = 26
      Increment = 1000.000000000000000000
      MaxValue = 2147483647.000000000000000000
      Value = 10000.000000000000000000
      TabOrder = 5
    end
    object cbIntitialDistribution: TCheckBox
      Left = 16
      Top = 158
      Width = 441
      Height = 17
      Caption = 'Initially distribute withdrawals halfway to nearest neighbor'
      TabOrder = 6
    end
    object seRedistribution: TJvSpinEdit
      Left = 266
      Top = 181
      Width = 145
      Height = 26
      MaxValue = 2147483647.000000000000000000
      Value = 10000.000000000000000000
      TabOrder = 7
    end
    object cbSaveBinary: TCheckBox
      Left = 16
      Top = 17
      Width = 473
      Height = 17
      Caption = 'Save results to binary file (Binary_Results_File)'
      TabOrder = 0
    end
    object cbSaveText: TCheckBox
      Left = 16
      Top = 40
      Width = 457
      Height = 17
      Caption = 'Save results to text file (Text_Results_File)'
      TabOrder = 1
    end
    object cbOpenListFile: TCheckBox
      Left = 16
      Top = 63
      Width = 395
      Height = 17
      Caption = 'Open listing file in text editor'
      TabOrder = 2
    end
    object rdeMinDepthRateIndex: TRbwDataEntry
      Left = 266
      Top = 107
      Width = 145
      Height = 22
      TabOrder = 4
      Text = '1e-6'
      OnChange = rdeMinDepthRateIndexChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end
