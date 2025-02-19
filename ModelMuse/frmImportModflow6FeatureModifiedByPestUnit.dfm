inherited frmImportModflow6FeatureModifiedByPest: TfrmImportModflow6FeatureModifiedByPest
  HelpType = htKeyword
  HelpKeyword = 'Import_MODFLOW_6_Feature_Modif'
  Caption = 'Import MODFLOW 6 Feature Modified by PEST'
  ClientHeight = 174
  ClientWidth = 403
  ExplicitWidth = 419
  ExplicitHeight = 213
  TextHeight = 18
  object lblModelFeatureFile: TLabel
    Left = 8
    Top = 8
    Width = 119
    Height = 18
    Caption = 'Model feature file'
  end
  object lblStressPeriod: TLabel
    Left = 8
    Top = 72
    Width = 94
    Height = 18
    Caption = 'Stress period'
  end
  object fedModelFeatureFile: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 387
    Height = 26
    Filter = 
      'MODFLOW 6 Feature files|*.chd; *.wel;*.drn;*.riv;*.ghb;*.rch;*.e' +
      'vt;*.csub;*.maw6;*.sfr;*.lak6;*.uzf'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    ExplicitWidth = 601
  end
  object seStressPeriod: TJvSpinEdit
    Left = 8
    Top = 96
    Width = 121
    Height = 26
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    TabOrder = 1
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 133
    Width = 403
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitWidth = 764
    DesignSize = (
      403
      41)
    object btnCancel: TBitBtn
      Left = 303
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 664
    end
    object btnOK: TBitBtn
      Left = 206
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 567
    end
    object btnHelp: TBitBtn
      Left = 109
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 470
    end
  end
end
